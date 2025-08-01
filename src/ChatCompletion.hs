module ChatCompletion (module X, appendUserMessage, respondWithTools, executeToolCalls) where

import ChatCompletion.Effect as X
import ChatCompletion.Storage.Effect as X
import ChatCompletion.Tool as X
import ChatCompletion.Types as X
import Control.Lens ((^.))
import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap qualified as KM
import Data.Map qualified as Map
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
appendUserMessage conversationId content = do
    let userMsgIn = UserMsgIn{content}
    appendMessage conversationId userMsgIn

-- | Send a user message and handle any tool calls automatically
respondWithTools
    :: ChatCompletionStorage :> es
    => ChatCompletion :> es
    => Error ChatCompletionError :> es
    => [ToolDef es] -- Tools available for this conversation
    -> ConversationId
    -> Text
    -> Eff es [ChatMsg] -- Returns all new messages (assistant responses and tool calls)
respondWithTools tools conversationId content = do
    -- Add the user message
    appendUserMessage conversationId content

    -- Get response and handle any tool calls
    handleToolLoop tools conversationId []

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
    => [ToolDef es]
    -> ConversationId
    -> [ChatMsg] -- Accumulated responses
    -> Eff es [ChatMsg]
handleToolLoop tools conversationId accumulated = do
    -- Get conversation and send to LLM
    conv <- getConversation conversationId

    response <- sendMessages (toToolDeclaration <$> tools) conv
    appendMessage conversationId (chatMsgToIn response)

    case response of
        -- Assistant message - we're done
        AssistantMsg{} -> pure (accumulated ++ [response])
        -- Tool calls - execute them and continue
        ToolCallMsg{toolCalls} -> do
            toolResponsesIn <- executeToolCalls tools toolCalls
            -- Convert tool responses to ChatMsg by fetching the conversation again
            forM_ toolResponsesIn $ \msgIn -> appendMessage conversationId msgIn
            -- Get the updated conversation to get messages with timestamps
            updatedConv <- getConversation conversationId
            let newMessages = drop (length conv) updatedConv
            handleToolLoop tools conversationId (accumulated ++ newMessages)

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
