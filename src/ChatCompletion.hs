module ChatCompletion
    ( module X
    , appendUserMessage
    , respondWithTools
    , executeToolCalls
    , respondWithToolsStructured
    , respondWithToolsJson
    ) where

import ChatCompletion.Effect as X
import ChatCompletion.Storage.Effect as X
import ChatCompletion.Tool as X
import ChatCompletion.Types as X
import Control.Lens ((^.))
import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap qualified as KM
import Data.List qualified as L
import Data.Map qualified as Map
import Data.OpenApi (ToSchema, toSchema)
import Data.Text qualified as Text
import Effectful
import Effectful.Error.Static
import Relude

-- | Send a user message and get response (no tool handling)
appendUserMessage
    :: ChatCompletionStorage :> es
    => ConversationId
    -> Text
    -> Eff es ()
appendUserMessage convId content = do
    let userMsgIn = UserMsgIn{content}
    appendMessage convId userMsgIn

-- | Send a user message and handle any tool calls automatically
respondWithTools
    :: ChatCompletionStorage :> es
    => ChatCompletion :> es
    => Error ChatCompletionError :> es
    => (ChatMsg -> Eff es ()) -- Callback for each message as it's produced
    -> [ToolDef es] -- Tools available for this conversation
    -> ConversationId
    -> Text
    -> Eff es [ChatMsg] -- Returns all new messages (assistant responses and tool calls)
respondWithTools callback tools convId content =
    respondWithTools' callback Unstructured tools convId content

-- | Send a user message and handle any tool calls automatically
respondWithToolsStructured
    :: forall a es
     . ToSchema a
    => FromJSON a
    => ChatCompletionStorage :> es
    => ChatCompletion :> es
    => Error ChatCompletionError :> es
    => (ChatMsg -> Eff es ()) -- Callback for each message as it's produced
    -> [ToolDef es] -- Tools available for this conversation
    -> ConversationId
    -> Text
    -> Eff es ([ChatMsg], Either String a)
respondWithToolsStructured callback tools convId msg = do
    msgs <-
        respondWithTools' callback (JsonSchema . toJSON . toSchema $ Proxy @a) tools convId msg
    let assistantContents :: [Text]
        assistantContents = [content | AssistantMsg{content} <- msgs]

        parsedContents :: Either String a
        parsedContents = case L.reverse assistantContents of
            [] -> Left "No assistant response found"
            (lastContent : _) -> eitherDecodeStrictText lastContent
    pure (msgs, parsedContents)

respondWithToolsJson
    :: forall es
     . ChatCompletionStorage :> es
    => ChatCompletion :> es
    => Error ChatCompletionError :> es
    => (ChatMsg -> Eff es ()) -- Callback for each message as it's produced
    -> [ToolDef es] -- Tools available for this conversation
    -> ConversationId
    -> Text
    -> Eff es ([ChatMsg], Either String Value)
respondWithToolsJson callback tools convId msg = do
    msgs <-
        respondWithTools' callback JsonValue tools convId msg
    let assistantContents :: [Text]
        assistantContents = [content | AssistantMsg{content} <- msgs]

        parsedContents :: Either String Value
        parsedContents = case L.reverse assistantContents of
            [] -> Left "No assistant response found"
            (lastContent : _) -> eitherDecodeStrictText lastContent
    pure (msgs, parsedContents)

-- | Send a user message and handle any tool calls automatically
respondWithTools'
    :: ChatCompletionStorage :> es
    => ChatCompletion :> es
    => Error ChatCompletionError :> es
    => (ChatMsg -> Eff es ()) -- Callback for each message as it's produced
    -> ResponseFormat
    -> [ToolDef es] -- Tools available for this conversation
    -> ConversationId
    -> Text
    -> Eff es [ChatMsg] -- Returns all new messages (assistant responses and tool calls)
respondWithTools' callback responseFormat tools convId content = do
    -- Add the user message
    appendUserMessage convId content

    -- Get response and handle any tool calls
    handleToolLoop callback responseFormat tools convId []

-- | Execute tool calls and return the responses
executeToolCalls
    :: [ToolDef es]
    -> [ToolCall]
    -> Eff es [ChatMsgIn]
executeToolCalls tools toolCalls = do
    forM toolCalls $ \tc -> do
        response <- case find (\t -> t ^. #name == tc ^. #toolName) tools of
            Nothing ->
                pure
                    $ ToolResponse
                        { modelResponse = "Tool not found: " <> tc ^. #toolName
                        , localResponse = []
                        }
            Just tool -> do
                let args = Object (KM.fromMap (Map.mapKeys fromText (tc ^. #toolArgs)))
                result <- tool ^. #executeFunction $ args
                case result of
                    Right resp -> pure resp
                    Left err ->
                        pure
                            $ ToolResponse
                                { modelResponse = "Tool error: " <> Text.pack err
                                , localResponse = []
                                }
        pure
            $ ToolCallResponseMsgIn
                { toolCallId = tc ^. #toolCallId
                , toolResponse = response
                }

-- | Internal helper to handle the tool execution loop
handleToolLoop
    :: ChatCompletionStorage :> es
    => ChatCompletion :> es
    => Error ChatCompletionError :> es
    => (ChatMsg -> Eff es ()) -- Callback for each message as it's produced
    -> ResponseFormat
    -> [ToolDef es]
    -> ConversationId
    -> [ChatMsg] -- Accumulated responses
    -> Eff es [ChatMsg]
handleToolLoop callback responseFormat tools convId accumulated = do
    -- Get conversation and send to LLM
    conv <- getConversation convId

    response <- sendMessages convId responseFormat (toToolDeclaration <$> tools) conv
    appendMessage convId (chatMsgToIn response)

    -- Stream the response message
    callback response

    case response of
        -- Assistant message - we're done
        AssistantMsg{} -> pure (accumulated <> [response])
        -- Tool calls - execute them and continue
        ToolCallMsg{toolCalls} -> do
            toolResponsesIn <- executeToolCalls tools toolCalls
            -- Convert tool responses to ChatMsg by fetching the conversation again
            forM_ toolResponsesIn $ \msgIn -> appendMessage convId msgIn
            -- Get the updated conversation to get messages with timestamps
            updatedConv <- getConversation convId
            let newMessages = drop (length conv + 1) updatedConv  -- +1 to skip the ToolCallMsg we already streamed
            -- Stream the tool response messages
            forM_ newMessages callback
            handleToolLoop callback responseFormat tools convId (accumulated <> [response] <> newMessages)

        -- Unexpected response
        _ -> throwError $ ProviderError $ "Unexpected response type: " <> show response
  where
    toToolDeclaration tool =
        ToolDeclaration
            { name = tool ^. #name
            , description = tool ^. #description
            , parameterSchema = tool ^. #parameterSchema
            }

    chatMsgToIn :: ChatMsg -> ChatMsgIn
    chatMsgToIn = \case
        SystemMsg content _ -> SystemMsgIn content
        UserMsg content _ -> UserMsgIn content
        AssistantMsg content _ -> AssistantMsgIn content
        ToolCallMsg toolCalls _ -> ToolCallMsgIn toolCalls
        ToolCallResponseMsg toolCallId toolResponse _ -> ToolCallResponseMsgIn toolCallId toolResponse
