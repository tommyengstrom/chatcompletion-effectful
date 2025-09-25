{-# LANGUAGE RecordWildCards #-}

module LlmChat.Storage.InMemory where

import LlmChat.Storage.Effect
import LlmChat.Types
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.UUID.V4 (nextRandom)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Prelude
import Effectful.Time
import GHC.IO (unsafePerformIO)
import Effectful.Concurrent
import Effectful.Concurrent.STM


runLlmChatStorageInMemory
    :: forall es a
     . ( Time :> es
       , Error ChatStorageError :> es
       , Concurrent :> es
       )
    => TVar (Map ConversationId [ChatMsg])
    -> Eff (LlmChatStorage ': es) a
    -> Eff es a
runLlmChatStorageInMemory tvar = interpret \_ -> \case
    CreateConversation systemPrompt -> do
        conversationId <- ConversationId <$> pure (unsafePerformIO nextRandom)
        timestamp <- currentTime
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
    AppendMessage conversationId msg -> do
        atomically $ modifyTVar' tvar $ Map.adjust (<> [msg]) conversationId
    ListConversations -> do
        conversations <- readTVarIO tvar
        pure $ Map.keys conversations

