{-# LANGUAGE RecordWildCards #-}

module Effect.ChatCompletion.OpenAI where

import Control.Lens
import Data.Aeson.Text (encodeToLazyText)
import Data.Generics.Labels ()
import Data.Generics.Product
import Data.Text.Lazy qualified as TL
import Data.Text.Lens (unpacked)
import Data.Time
import Data.Vector (Vector)
import Data.Vector qualified as V
import Effect.ChatCompletion
import Effect.ChatCompletion.Tool
import Effect.ChatCompletion.Types
import Effect.ChatCompletionStorage
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import OpenAI.V1
    ( Methods (..)
    , getClientEnv
    , makeMethods
    )
import OpenAI.V1.Chat.Completions
    ( ChatCompletionObject (..)
    , Content (..)
    , CreateChatCompletion (..)
    , Message (..)
    , _CreateChatCompletion
    )
import OpenAI.V1.Models (Model (..))
import OpenAI.V1.Tool qualified as OpenAiTool
import OpenAI.V1.ToolCall qualified as OpenAiTC
import Relude
import UnliftIO

newtype OpenAiApiKey = OpenAiApiKey Text
    deriving stock (Show, Eq, Ord, Generic)

data OpenAiSettings = OpenAiSettings
    { apiKey :: OpenAiApiKey
    , model :: Text
    , baseUrl :: Text
    , responseLogger :: ConversationId -> ChatCompletionObject -> IO ()
    }
    deriving stock (Generic)

defaultOpenAiSettings :: OpenAiApiKey -> OpenAiSettings
defaultOpenAiSettings apiKey =
    OpenAiSettings
        { apiKey = apiKey
        , model = "gpt-4o"
        , baseUrl = "https://api.openai.com"
        , responseLogger = \_ _ -> pure ()
        }

mapContent :: (a -> b) -> Message a -> Message b
mapContent f = \case
    System{..} -> System{content = f content, ..}
    User{..} -> User{content = f content, ..}
    Assistant{..} -> Assistant{assistant_content = fmap f assistant_content, ..}
    Tool{..} -> Tool{content = f content, ..}

runChatCompletionOpenAi
    :: forall es a
     . ( IOE :> es
       , ChatCompletionStorage :> es
       , Error ChatCompletionError :> es
       )
    => OpenAiSettings
    -> [ToolDef es]
    -> Eff (ChatCompletion ': es) a
    -> Eff es a
runChatCompletionOpenAi settings tools es = do
    clientEnv <- liftIO . getClientEnv $ settings ^. #baseUrl
    let Methods{createChatCompletion} =
            makeMethods
                clientEnv
                (settings ^. #apiKey . typed @Text)

    runChatCompletion createChatCompletion es
  where
    runChatCompletion
        :: (CreateChatCompletion -> IO ChatCompletionObject)
        -> Eff (ChatCompletion ': es) a
        -> Eff es a
    runChatCompletion createChatCompletion = interpret \_ -> \case
        RespondToConversation convId -> makeOpenAIRequests createChatCompletion convId

    adapt :: IO x -> Eff es x
    adapt m = liftIO m `catchAny` \e -> throwError . ChatCompletionError $ displayException e

    makeOpenAIRequests
        :: (CreateChatCompletion -> IO ChatCompletionObject)
        -> ConversationId
        -> Eff es [ChatMsg]
    makeOpenAIRequests createChatCompletion convId = do
        fullConv <- getConversation convId
        response <-
            adapt
                . tryAny
                . createChatCompletion
                $ _CreateChatCompletion
                    { messages = V.fromList $ toOpenAIMessage <$> fullConv
                    , model = settings ^. #model . to Model
                    , tools = Just . V.fromList $ mkTool <$> tools
                    }
        case response of
            Left err ->
                throwError
                    . ChatCompletionError
                    $ "OpenAI API error: "
                    <> displayException err
            Right chatCompletionObject -> do
                liftIO $ (settings ^. #responseLogger) convId chatCompletionObject
                now <- liftIO getCurrentTime

                let chatMsg :: Either String ChatMsg
                    chatMsg = do
                        openAiMsg <-
                            maybe (Left "No message in OpenAI response") Right
                                $ chatCompletionObject
                                ^? #choices
                                    . taking 1 folded -- no support for multiple choices
                                    . #message
                        fromOpenAIMessage now openAiMsg
                case chatMsg of
                    Right msg@AssistantMsg{} -> appendMessages convId [msg]
                    Right msg@ToolCallMsg{toolCalls} -> do
                        _ <- appendMessages convId [msg]
                        forM_ toolCalls \tc -> do
                            result <- case tools ^? folded . filteredBy (#name . only (tc ^. #toolName)) of
                                Nothing ->
                                    throwError
                                        . ChatCompletionError
                                        $ "Tool not found: "
                                        <> tc
                                        ^. #toolName . unpacked
                                Just tool -> runTool tool (tc ^. #toolArgs)
                            case result of
                                Left err -> throwError $ ChatCompletionError err
                                Right toolResponse -> do
                                    void
                                        $ appendMessages
                                            convId
                                            [ ToolCallResponseMsg
                                                { toolCallId = tc ^. #toolCallId
                                                , toolResponse = toolResponse
                                                , createdAt = now
                                                }
                                            ]
                        makeOpenAIRequests createChatCompletion convId
                    Right msg ->
                        throwError
                            . ChatCompletionError
                            $ "Unexpected message type from OpenAI: "
                            <> show msg
                    Left err -> throwError $ ChatCompletionError err

toOpenAIMessage :: ChatMsg -> Message (Vector Content)
toOpenAIMessage msg = case msg of
    SystemMsg{content} -> System{name = Nothing, content = toContentVector content}
    UserMsg{content} -> User{content = toContentVector content, name = Nothing}
    AssistantMsg{content} ->
        Assistant
            { assistant_content = Just $ toContentVector content
            , name = Nothing
            , refusal = Nothing
            , assistant_audio = Nothing
            , tool_calls = Nothing
            }
    ToolCallMsg{toolCalls} ->
        Assistant
            { assistant_content = Nothing
            , name = Nothing
            , refusal = Nothing
            , assistant_audio = Nothing
            , tool_calls = Just . V.fromList $ fmap translateToolCalls toolCalls
            }
    ToolCallResponseMsg{toolCallId, toolResponse} ->
        Tool
            { content = toContentVector . TL.toStrict $ encodeToLazyText toolResponse
            , tool_call_id = getTyped toolCallId
            }
  where
    toContentVector :: Text -> Vector Content
    toContentVector = V.singleton . Text

    translateToolCalls :: ToolCall -> OpenAiTC.ToolCall
    translateToolCalls tc =
        OpenAiTC.ToolCall_Function
            { id = tc ^. #toolCallId . typed @Text
            , function =
                OpenAiTC.Function
                    { name = tc ^. #toolName
                    , arguments = tc ^. #toolArgs . typed
                    }
            }

mkTool :: ToolDef es -> OpenAiTool.Tool
mkTool t =
    OpenAiTool.Tool_Function
        $ OpenAiTool.Function
            { name = t ^. #name
            , description = t ^. #description . to Just
            , parameters = t ^. #parameterSchema
            , strict = True <$ t ^. #parameterSchema
            }

-- | Convert the reponse from OpenAI to the internal ChatMsg format.
-- Only assistant message are supported.
fromOpenAIMessage :: UTCTime -> Message Text -> Either String ChatMsg
fromOpenAIMessage now = \case
    Assistant{tool_calls = Just tcs} ->
        Right
            ToolCallMsg
                { toolCalls = do
                    tc <- V.toList tcs
                    pure
                        $ ToolCall
                            { toolCallId = tc ^. #id . to ToolCallId
                            , toolName = tc ^. #function . #name
                            , toolArgs = tc ^. #function . #arguments . to ToolArgs
                            }
                , createdAt = now
                }
    Assistant{assistant_content} ->
        Right
            AssistantMsg
                { content = fromMaybe "" assistant_content
                , createdAt = now
                }
    System{} -> Left "misuse of fromOpenAIMessage: System messages are not supported"
    User{} -> Left "misuse of fromOpenAIMessage: User messages are not supported"
    Tool{} -> Left "misuse of fromOpenAIMessage: Tool messages are not supported"
