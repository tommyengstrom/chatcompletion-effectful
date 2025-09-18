{-# LANGUAGE RecordWildCards #-}

module ChatCompletion.Storage.InMemory where

import ChatCompletion.Storage.Effect
import ChatCompletion.Types
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Time
import Data.UUID.V4 (nextRandom)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import UnliftIO
import Prelude


runChatCompletionStorageInMemory
    :: forall es a
     . ( IOE :> es
       , Error ChatStorageError :> es
       )
    => TVar (Map ConversationId [ChatMsg])
    -> Eff (ChatCompletionStorage ': es) a
    -> Eff es a
runChatCompletionStorageInMemory tvar = interpret \_ -> \case
    CreateConversation systemPrompt -> do
        conversationId <- ConversationId <$> liftIO nextRandom
        timestamp <- liftIO getCurrentTime
        atomically $
            modifyTVar' tvar (Map.insert conversationId [SystemMsg systemPrompt timestamp])
        pure conversationId
    DeleteConversation conversationId -> do
        atomically $ modifyTVar' tvar (Map.delete conversationId)
    GetConversation conversationId -> do
        conversations <- readTVarIO tvar
        let conv = Map.lookup conversationId conversations
        case conv of
            Nothing -> throwError $  NoSuchConversation conversationId
            Just c -> pure c
    AppendMessage conversationId msgIn -> do
        msg <- toChatMsg msgIn
        atomically $ do
            modifyTVar' tvar $ Map.adjust (<> [msg]) conversationId
    ListConversations -> do
        conversations <- readTVarIO tvar
        pure $ Map.keys conversations

toChatMsg :: IOE :> es => ChatMsgIn -> Eff es ChatMsg
toChatMsg msgIn = do
    createdAt <- liftIO getCurrentTime
    pure case msgIn of
        UserMsgIn{..} -> UserMsg{..}
        SystemMsgIn{..} -> SystemMsg{..}
        AssistantMsgIn{..} -> AssistantMsg{..}
        ToolCallMsgIn{..} -> ToolCallMsg{..}
        ToolCallResponseMsgIn{..} -> ToolCallResponseMsg{..}
