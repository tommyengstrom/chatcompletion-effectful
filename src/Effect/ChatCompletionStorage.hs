module Effect.ChatCompletionStorage where

import Effectful
import Relude
import Effectful.TH
import Effect.ChatCompletion.Types

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

