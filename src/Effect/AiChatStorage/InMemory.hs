module Effect.AiChatStorage.InMemory where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Time
import Data.UUID.V4 (nextRandom)
import Effect.AiChat.Types
import Effect.AiChatStorage
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import UnliftIO
import Prelude

runAiChatStorageInMemory
    :: forall es a
     . ( IOE :> es
       , Error AiChatStorageError :> es
       )
    => Eff (AiChatStorage ': es) a
    -> Eff es a
runAiChatStorageInMemory es = do
    tvar <- newTVarIO (mempty :: Map ConversationId [ChatMsg])
    runInMemoryAiChatStorage tvar es
  where
    runInMemoryAiChatStorage
        :: TVar (Map ConversationId [ChatMsg]) -> Eff (AiChatStorage ': es) a -> Eff es a
    runInMemoryAiChatStorage tvar = interpret \_ -> \case
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
                Nothing -> throwError $ NoSuchConversation conversationId
                Just c -> pure c
        AppendMessages conversationId chatMsgs -> do
            atomically $ do
                modifyTVar' tvar $ Map.adjust (<> chatMsgs) conversationId
                fromMaybe [] . Map.lookup conversationId <$> readTVar tvar
