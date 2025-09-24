{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ChatCompletion.Providers.OpenAI.ChatCompletion where

import ChatCompletion.Effect
import ChatCompletion.Storage.Effect
import ChatCompletion.Types
import Control.Lens
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Text (encodeToLazyText)
import Data.Generics.Labels ()
import Data.Generics.Product
import Data.Map qualified as Map
import Data.Text.Lazy qualified as TL
import Data.Vector (Vector)
import Data.Vector qualified as V
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.OpenAI
import Effectful.Time
import OpenAI.V1.Chat.Completions
    ( Content (..)
    , CreateChatCompletion (..)
    , Message (..)
    , _CreateChatCompletion
    )
import OpenAI.V1.Models (Model (..))
import OpenAI.V1.ResponseFormat qualified as RF
import OpenAI.V1.Tool qualified as OpenAiTool
import OpenAI.V1.ToolCall qualified as OpenAiTC
import Relude
import Prelude qualified

data ChatCompletionSettings es = ChatCompletionSettings
    { model :: Model
    , overrides :: CreateChatCompletion -> CreateChatCompletion
    , requestLogger :: ConversationId -> NativeMsgFormat -> Eff es ()
    }
    deriving stock (Generic)

defaultChatCompletionSettings :: ChatCompletionSettings es
defaultChatCompletionSettings =
    ChatCompletionSettings
        { model = "gpt-5-nano"
        , overrides = Prelude.id
        , requestLogger = \_ _ -> pure ()
        }

runLlmChat
    :: ( Time :> es
       , Error LlmChatError :> es
       , ChatCompletionStorage :> es
       , OpenAI :> es
       )
    => ChatCompletionSettings es
    -> Eff (LlmChat ': es) a
    -> Eff es a
runLlmChat ChatCompletionSettings{..} = interpret \_ -> \case
    GetLlmResponse tools' responseFormat convId -> do
        messages <- getConversation convId
        let tools = fmap mkToolFromDeclaration tools'

            req :: CreateChatCompletion
            req =
                overrides _CreateChatCompletion
                    & #messages .~ V.fromList (toOpenAIMessage <$> messages)
                    & #model .~ model
                    & ( case tools of
                            [] -> Relude.id
                            _ -> #tools ?~ V.fromList tools
                      )
                    & #response_format .~ case responseFormat of
                        Unstructured -> Nothing
                        JsonValue -> Just RF.JSON_Object
                        JsonSchema schema ->
                            Just
                                $ RF.JSON_Schema
                                    RF.JSONSchema
                                        { description = Nothing
                                        , name = "response_format"
                                        , schema = Just schema
                                        , strict = Nothing
                                        }
        requestLogger convId . NativeMsgOut $ toJSON req
        chatCompletionObject <-
            chatCompletion req
                `catchError` \_ err -> case err of
                    LlmClientError e -> do
                        requestLogger convId (NativeRequestFailure $ displayException e)
                        throwError $ LlmClientError e
                    other -> throwError other

        requestLogger convId . NativeMsgIn $ toJSON chatCompletionObject
        now <- currentTime
        openAiMsg <-
            maybe (throwError $ LlmExpectationError "No message in OpenAI response") pure
                $ chatCompletionObject
                ^? #choices . taking 1 folded . #message
        either throwError pure $ toChatMsgIn now openAiMsg

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
                    , arguments =
                        TL.toStrict
                            $ encodeToLazyText
                            $ Object
                            $ KM.fromMap
                            $ Map.mapKeys Key.fromText (tc ^. #toolArgs)
                    }
            }

mkToolFromDeclaration :: ToolDeclaration -> OpenAiTool.Tool
mkToolFromDeclaration t =
    OpenAiTool.Tool_Function
        $ OpenAiTool.Function
            { name = t ^. #name
            , description = t ^. #description . to Just
            , parameters = t ^. #parameterSchema
            , strict = Nothing -- True <$ t ^. #parameterSchema -- they seem to not support maybe values?
            }

instance IsChatMsg (Message Text) (Message (Vector Content)) where
    toChatMsgIn createdAt = \case
        Assistant{tool_calls = Just tcs} ->
            Right
                ToolCallMsg
                    { toolCalls = do
                        tc <- V.toList tcs
                        pure
                            $ ToolCall
                                { toolCallId = tc ^. #id . to ToolCallId
                                , toolName = tc ^. #function . #name
                                , toolArgs = case eitherDecodeStrictText (tc ^. #function . #arguments) of
                                    Right (Object km) -> Map.mapKeys Key.toText (KM.toMap km)
                                    _ -> mempty
                                }
                    , createdAt
                    }
        Assistant{assistant_content} ->
            Right
                AssistantMsg
                    { content = fromMaybe "" assistant_content
                    , createdAt
                    }
        System{} -> Left $ LlmExpectationError "System messages are not supported"
        User{} -> Left $ LlmExpectationError "User messages are not supported"
        Tool{} -> Left $ LlmExpectationError "Tool messages are not supported"

    fromChatMsg = toOpenAIMessage

---- | Convert the reponse from OpenAI to the internal ChatMsg format.
---- Only assistant message are supported.
-- fromOpenAIMessage :: UTCTime -> Message Text -> Either String ChatMsg
-- fromOpenAIMessage now = \case
--    Assistant{tool_calls = Just tcs} ->
--        Right
--            ToolCallMsg
--                { toolCalls = do
--                    tc <- V.toList tcs
--                    pure
--                        $ ToolCall
--                            { toolCallId = tc ^. #id . to ToolCallId
--                            , toolName = tc ^. #function . #name
--                            , toolArgs = case eitherDecodeStrictText (tc ^. #function . #arguments) of
--                                Right (Object km) -> Map.mapKeys Key.toText (KM.toMap km)
--                                _ -> mempty
--                            }
--                , createdAt = now
--                }
--    Assistant{assistant_content} ->
--        Right
--            AssistantMsg
--                { content = fromMaybe "" assistant_content
--                , createdAt = now
--                }
--    System{} -> Left "misuse of fromOpenAIMessage: System messages are not supported"
--    User{} -> Left "misuse of fromOpenAIMessage: User messages are not supported"
--    Tool{} -> Left "misuse of fromOpenAIMessage: Tool messages are not supported"
