module Effect.AiChatStorage where

import Effectful
import Relude
import Effectful.TH
import Effect.AiChat.Types

newtype AiChatStorageError
    = NoSuchConversation ConversationId
    deriving stock Show

data AiChatStorage :: Effect where
    CreateConversation :: SystemPrompt -> AiChatStorage m ConversationId
    DeleteConversation :: ConversationId -> AiChatStorage m ()
    GetConversation :: ConversationId -> AiChatStorage m [ChatMsg]
    AppendMessages :: ConversationId -> [ChatMsg] -> AiChatStorage m [ChatMsg]

type instance DispatchOf AiChatStorage = 'Dynamic

makeEffect ''AiChatStorage

