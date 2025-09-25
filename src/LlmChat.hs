{-# LANGUAGE RecordWildCards #-}

module LlmChat
    ( module X
    , module LlmChat
    ) where

import LlmChat.Effect as X
import LlmChat.Storage.Effect as X
import LlmChat.Tool as X
import LlmChat.Types as X
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
    :: ( HasCallStack
       , LlmChatStorage :> es
       , Time :> es
       , Error LlmChatError :> es
       , LlmChat :> es
       )
    => [ToolDef es] -- Tools available for this conversation
    -> ConversationId
    -> Eff es [ChatMsg] -- Returns all new messages (assistant responses and tool calls)
respondWithTools tools convId =
    fst <$> respondWithTools' Unstructured tools convId

-- | Send a user message and handle any tool calls automatically
respondWithToolsStructured
    :: forall a es
     . ( HasCallStack
       , ToSchema a
       , Time :> es
       , FromJSON a
       , LlmChatStorage :> es
       , Error LlmChatError :> es
       , LlmChat :> es
       )
    => [ToolDef es] -- Tools available for this conversation
    -> ConversationId
    -> Eff es ([ChatMsg], a)
respondWithToolsStructured tools convId = do
    (msgs, lastMsgContent) <-
        respondWithTools'
            (JsonSchema . toJSON . toInlinedSchema $ Proxy @a)
            tools
            convId
    a <-
        either (throwError . LlmExpectationError) pure $
            eitherDecodeStrictText lastMsgContent
    pure (msgs, a)

respondWithToolsJson
    :: forall es
     . ( HasCallStack
       , Time :> es
       , Error LlmChatError :> es
       , LlmChatStorage :> es
       , LlmChat :> es
       )
    => [ToolDef es] -- Tools available for this conversation
    -> ConversationId
    -> Eff es ([ChatMsg], Value)
respondWithToolsJson tools convId = do
    (msgs, lastMsgContent) <- respondWithTools' JsonValue tools convId
    a <-
        either (throwError . LlmExpectationError) pure $
            eitherDecodeStrictText lastMsgContent
    pure (msgs, a)

-- | Send a user message and handle any tool calls automatically
respondWithTools'
    :: ( HasCallStack
       , LlmChatStorage :> es
       , Time :> es
       , Error LlmChatError :> es
       , LlmChat :> es
       )
    => ResponseFormat
    -> [ToolDef es]
    -- ^ Tools available for these calls
    -> ConversationId
    -> Eff es ([ChatMsg], Text)
    -- ^ Returns all new messages (assistant responses and tool calls)
respondWithTools' responseFormat tools convId = do
    msgs <- handleToolLoop responseFormat tools convId []

    case L.reverse msgs of
        AssistantMsg{content} : _ -> pure (msgs, content)
        msg : _ ->
            throwError
                . LlmExpectationError
                $ "Expected the last message to be an assitant message but got: "
                    <> show msg
        [] -> throwError $ LlmExpectationError "Assistant returned no messages"

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
    :: ( LlmChatStorage :> es
       , Time :> es
       , Error LlmChatError :> es
       , LlmChat :> es
       )
    => ResponseFormat
    -> [ToolDef es]
    -> ConversationId
    -> [ChatMsg] -- Accumulated responses
    -> Eff es [ChatMsg]
handleToolLoop responseFormat tools convId accumulated = do
    response <- getLlmResponse (toToolDeclaration <$> tools) responseFormat convId
    appendMessage convId response

    case response of
        -- Assistant message - we're done
        AssistantMsg{} -> pure (accumulated <> [response])
        -- Tool calls - execute them and continue
        ToolCallMsg{toolCalls} -> do
            toolCallResponseMsgs <-  executeToolCalls tools toolCalls
            traverse_ (appendMessage convId) toolCallResponseMsgs
            handleToolLoop
                responseFormat
                tools
                convId
                (accumulated <> [response] <> toolCallResponseMsgs)

        _ -> throwError $ LlmExpectationError $ "Unexpected response type: " <> show response
  where
    toToolDeclaration tool =
        ToolDeclaration
            { name = tool ^. #name
            , description = tool ^. #description
            , parameterSchema = tool ^. #parameterSchema
            }
