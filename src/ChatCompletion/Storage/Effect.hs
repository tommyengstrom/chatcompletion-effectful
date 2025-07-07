module ChatCompletion.Storage.Effect where

import ChatCompletion.Types
import Effectful
import Effectful.TH
import Relude

newtype ChatCompletionStorageError
    = NoSuchConversation ConversationId
    deriving stock (Show, Eq)

data ChatCompletionStorage :: Effect where
    CreateConversation :: SystemPrompt -> ChatCompletionStorage m ConversationId
    DeleteConversation :: ConversationId -> ChatCompletionStorage m ()
    GetConversation :: ConversationId -> ChatCompletionStorage m [ChatMsg]
    AppendMessages :: ConversationId -> [ChatMsg] -> ChatCompletionStorage m [ChatMsg]
    ListConversations :: ChatCompletionStorage m [ConversationId]

type instance DispatchOf ChatCompletionStorage = 'Dynamic

makeEffect ''ChatCompletionStorage

