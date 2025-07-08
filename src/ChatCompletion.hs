module ChatCompletion (module X, appendUserMessage, respondWithTools, executeToolCalls) where

import ChatCompletion.Effect as X
import ChatCompletion.Storage.Effect as X
import ChatCompletion.Tool as X
import ChatCompletion.Types as X
import Control.Lens ((^.), to)
import Data.Aeson
import Data.Generics.Product (typed)
import Data.Text qualified as Text
import Data.Time
import Effectful
import Effectful.Error.Static
import Relude

-- | Send a user message and get response (no tool handling)
appendUserMessage
    :: IOE :> es
    => ChatCompletionStorage :> es
    => ConversationId
    -> Text
    -> Eff es ()
appendUserMessage conversationId content = do
    currentTime <- liftIO getCurrentTime
    let userMsg = UserMsg{content, createdAt = currentTime}
    appendMessages conversationId [userMsg]

-- | Send a user message and handle any tool calls automatically
respondWithTools
    :: IOE :> es
    => ChatCompletionStorage :> es
    => ChatCompletion :> es
    => Error ChatCompletionError :> es
    => [ToolDef es]  -- Tools available for this conversation
    -> ConversationId
    -> Text
    -> Eff es [ChatMsg]  -- Returns all new messages (assistant responses and tool calls)
respondWithTools tools conversationId content = do
    -- Add the user message
    appendUserMessage conversationId content

    -- Get response and handle any tool calls
    handleToolLoop tools conversationId []

-- | Execute tool calls and return the responses
executeToolCalls
    :: IOE :> es
    => [ToolDef es]
    -> [ToolCall]
    -> Eff es [ChatMsg]
executeToolCalls tools toolCalls = do
    now <- liftIO getCurrentTime
    forM toolCalls $ \tc -> do
        response <- case find (\t -> t ^. #name == tc ^. #toolName) tools of
            Nothing -> pure $ ToolResponse
                { modelResponse = "Tool not found: " <> tc ^. #toolName
                , localResponse = []
                }
            Just tool -> do
                let args = case tc ^. #toolArgs . typed @Text . to eitherDecodeStrictText of
                        Right v -> v
                        Left _ -> Null
                result <- tool ^. #executeFunction $ args
                case result of
                    Right resp -> pure resp
                    Left err -> pure $ ToolResponse
                        { modelResponse = "Tool error: " <> Text.pack err
                        , localResponse = []
                        }
        pure $ ToolCallResponseMsg
            { toolCallId = tc ^. #toolCallId
            , toolResponse = response
            , createdAt = now
            }

-- | Internal helper to handle the tool execution loop
handleToolLoop
    :: IOE :> es
    => ChatCompletionStorage :> es
    => ChatCompletion :> es
    => Error ChatCompletionError :> es
    => [ToolDef es]
    -> ConversationId
    -> [ChatMsg]  -- Accumulated responses
    -> Eff es [ChatMsg]
handleToolLoop tools conversationId accumulated = do
    -- Get conversation and send to LLM
    conv <- getConversation conversationId

    response <- sendMessages (toToolDeclaration <$> tools) conv
    appendMessages conversationId [response]

    case response of
        -- Assistant message - we're done
        AssistantMsg{} -> pure (accumulated ++ [response])

        -- Tool calls - execute them and continue
        ToolCallMsg{toolCalls} -> do
            toolResponses <- executeToolCalls tools toolCalls
            appendMessages conversationId toolResponses
            let newMessages = response : toolResponses
            handleToolLoop tools conversationId (accumulated ++ newMessages)

        -- Unexpected response
        _ -> throwError $ ChatCompletionError $ "Unexpected response type: " <> show response
  where
    toToolDeclaration tool = ToolDeclaration
        { name = tool ^. #name
        , description = tool ^. #description
        , parameterSchema = tool ^. #parameterSchema
        }
