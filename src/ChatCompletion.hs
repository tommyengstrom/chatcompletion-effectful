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
import Effectful.Time
import Relude

-- | Send a user message and handle any tool calls automatically
respondWithTools
    :: ( HasCallStack,  ChatCompletionStorage :> es
       , Time :> es
       , Error LlmRequestError :> es
       , Error ChatExpectationError :> es
       )
    => LlmRequestHandler es
    -> [ToolDef es] -- Tools available for this conversation
    -> ConversationId
    -> Eff es [ChatMsg] -- Returns all new messages (assistant responses and tool calls)
respondWithTools handler tools convId =
    fst <$> respondWithTools' handler Unstructured tools convId

-- | Send a user message and handle any tool calls automatically
respondWithToolsStructured
    :: forall a es
     . ( HasCallStack
       , ToSchema a
       , Error LlmRequestError :> es
       , Time :> es
       , FromJSON a
       , ChatCompletionStorage :> es
       , Error ChatExpectationError :> es
       )
    => LlmRequestHandler es
    -> [ToolDef es] -- Tools available for this conversation
    -> ConversationId
    -> Eff es ([ChatMsg], a)
respondWithToolsStructured handler tools convId = do
    (msgs, lastMsgContent) <-
        respondWithTools'
            handler
            (JsonSchema . toJSON . toInlinedSchema $ Proxy @a)
            tools
            convId
    a <- either (throwError . ChatExpectationError) pure $
                    eitherDecodeStrictText lastMsgContent
    pure (msgs, a)

respondWithToolsJson
    :: forall es
     . ( HasCallStack
       ,  Time :> es
       , Error LlmRequestError :> es
       , Error ChatExpectationError :> es
       , ChatCompletionStorage :> es
       )
    => LlmRequestHandler es
    -> [ToolDef es] -- Tools available for this conversation
    -> ConversationId
    -> Eff es ([ChatMsg], Value)
respondWithToolsJson handler tools convId = do
    (msgs, lastMsgContent) <- respondWithTools' handler JsonValue tools convId
    a <- either (throwError . ChatExpectationError) pure $
                    eitherDecodeStrictText lastMsgContent
    pure (msgs, a)

-- | Send a user message and handle any tool calls automatically
respondWithTools'
    :: ( HasCallStack
       , ChatCompletionStorage :> es
       , Time :> es
       , Error LlmRequestError :> es
       , Error ChatExpectationError :> es
       )
    => LlmRequestHandler es
    -> ResponseFormat
    -> [ToolDef es]
    -- ^ Tools available for these calls
    -> ConversationId
    -> Eff es ([ChatMsg], Text)
    -- ^ Returns all new messages (assistant responses and tool calls)
respondWithTools' handler responseFormat tools convId = do
    msgs <- handleToolLoop handler responseFormat tools convId []

    case L.reverse msgs of
        AssistantMsg{content} : _ -> pure (msgs, content)
        msg : _ ->
            throwError
                . ChatExpectationError
                $ "Expected the last message to be an assitant message but got: "
                    <> show msg
        [] -> throwError $ ChatExpectationError "Assistant returned no messages"

-- | Execute tool calls and return the responses
executeToolCalls
    :: Time :> es
    => [ToolDef es]
    -> [ToolCall]
    -> Eff es [ChatMsg]
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
        createdAt <- currentTime
        pure $
            ToolCallResponseMsg
                { toolCallId = tc ^. #toolCallId
                , toolResponse = response
                , createdAt
                }

-- | Internal helper to handle the tool execution loop
handleToolLoop
    :: ( ChatCompletionStorage :> es
       , Time :> es
       , Error LlmRequestError :> es
       , Error ChatExpectationError :> es
       )
    => LlmRequestHandler es
    -> ResponseFormat
    -> [ToolDef es]
    -> ConversationId
    -> [ChatMsg] -- Accumulated responses
    -> Eff es [ChatMsg]
handleToolLoop requestHandler responseFormat tools convId accumulated = do
    conv <- getConversation convId

    response <- requestHandler (toToolDeclaration <$> tools) responseFormat conv
    appendMessage convId response

    case response of
        -- Assistant message - we're done
        AssistantMsg{} -> pure (accumulated <> [response])
        -- Tool calls - execute them and continue
        ToolCallMsg{toolCalls} -> do
            traverse_ (appendMessage convId) =<< executeToolCalls tools toolCalls
            -- Get the updated conversation to get messages with timestamps
            updatedConv <- getConversation convId
            let newMessages = drop (length conv + 1) updatedConv -- +1 to skip the ToolCallMsg we already streamed
            -- Stream the tool response messages
            handleToolLoop
                requestHandler
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
