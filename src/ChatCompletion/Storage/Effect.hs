module ChatCompletion.Storage.Effect where

import ChatCompletion.Types
import Effectful
import Effectful.TH
import Prelude

data ChatCompletionStorage :: Effect where
    CreateConversation :: SystemPrompt -> ChatCompletionStorage m ConversationId
    DeleteConversation :: ConversationId -> ChatCompletionStorage m ()
    GetConversation :: ConversationId -> ChatCompletionStorage m [ChatMsg]
    AppendMessage :: ConversationId -> ChatMsgIn -> ChatCompletionStorage m ()
    ListConversations :: ChatCompletionStorage m [ConversationId]

type instance DispatchOf ChatCompletionStorage = 'Dynamic

makeEffect ''ChatCompletionStorage




data ChatStorageError
    = NoSuchConversation ConversationId
    deriving stock (Show, Eq)
