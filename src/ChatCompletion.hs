{-# LANGUAGE RecordWildCards #-}

module ChatCompletion
    ( module X
    , module ChatCompletion
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
import Data.OpenApi (ToSchema, toInlinedSchema)
import Data.Text qualified as Text
import Effectful
import Effectful.Error.Static
import Relude

-- | Send a user message and get response (no tool handling)

-- | Send a user message and handle any tool calls automatically
respondWithTools
    :: ChatCompletionStorage :> es
    => ChatCompletion :> es
    => Error ChatCompletionError :> es
    => (ChatMsg -> Eff es ()) -- Callback for each message as it's produced
    -> [ToolDef es] -- Tools available for this conversation
    -> ConversationId
    -> Eff es [ChatMsg] -- Returns all new messages (assistant responses and tool calls)
respondWithTools callback tools convId =
    respondWithTools' callback Unstructured tools convId

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
    -> Eff es ([ChatMsg], Either String a)
respondWithToolsStructured callback tools convId = do
    msgs <-
        respondWithTools'
            callback
            (JsonSchema . toJSON . toInlinedSchema $ Proxy @a)
            tools
            convId
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
    -> Eff es ([ChatMsg], Either String Value)
respondWithToolsJson callback tools convId = do
    msgs <- respondWithTools' callback JsonValue tools convId
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
    -> [ToolDef es] -- ^ Tools available for these calls
    -> ConversationId
    -> Eff es [ChatMsg] -- ^ Returns all new messages (assistant responses and tool calls)
respondWithTools' callback responseFormat tools convId = do
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
                pure $
                    ToolResponse
                        { modelResponse = "Tool not found: " <> tc ^. #toolName
                        , localResponse = []
                        }
            Just tool -> do
                let args = Object (KM.fromMap (Map.mapKeys fromText (tc ^. #toolArgs)))
                result <- tool ^. #executeFunction $ args
                case result of
                    Right resp -> pure resp
                    Left err ->
                        pure $
                            ToolResponse
                                { modelResponse = "Tool error: " <> Text.pack err
                                , localResponse = []
                                }
        pure $
            ToolCallResponseMsgIn
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

    response <- sendMessages responseFormat (toToolDeclaration <$> tools) conv
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
            let newMessages = drop (length conv + 1) updatedConv -- +1 to skip the ToolCallMsg we already streamed
            -- Stream the tool response messages
            forM_ newMessages callback
            handleToolLoop
                callback
                responseFormat
                tools
                convId
                (accumulated <> [response] <> newMessages)

        -- Unexpected response
        _ -> throwError $ ChatExpectationError $ "Unexpected response type: " <> show response
  where
    toToolDeclaration tool =
        ToolDeclaration
            { name = tool ^. #name
            , description = tool ^. #description
            , parameterSchema = tool ^. #parameterSchema
            }

chatMsgToIn :: ChatMsg -> ChatMsgIn
chatMsgToIn = \case
    SystemMsg{..} -> SystemMsgIn{..}
    UserMsg{..} -> UserMsgIn{..}
    AssistantMsg{..} -> AssistantMsgIn{..}
    ToolCallMsg{..} -> ToolCallMsgIn{..}
    ToolCallResponseMsg{..} -> ToolCallResponseMsgIn{..}
