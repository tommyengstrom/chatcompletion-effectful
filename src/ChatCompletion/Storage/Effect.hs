module ChatCompletion.Storage.Effect where

import ChatCompletion.Types
import Data.Text (Text)
import Effectful
import Effectful.TH
import Effectful.Time
import Prelude

data ChatCompletionStorage :: Effect where
    CreateConversation :: SystemPrompt -> ChatCompletionStorage m ConversationId
    DeleteConversation :: ConversationId -> ChatCompletionStorage m ()
    GetConversation :: ConversationId -> ChatCompletionStorage m [ChatMsg]
    AppendMessage :: ConversationId -> ChatMsg -> ChatCompletionStorage m ()
    ListConversations :: ChatCompletionStorage m [ConversationId]

type instance DispatchOf ChatCompletionStorage = 'Dynamic

makeEffect ''ChatCompletionStorage

appendUserMessage
    :: ( Time :> es
       , ChatCompletionStorage :> es
       )
    => ConversationId
    -> Text
    -> Eff es ()
appendUserMessage convId content = do
    timestamp <- currentTime
    let userMsgIn =
            UserMsg
                { content
                , hidden = False
                , createdAt = timestamp
                }
    appendMessage convId userMsgIn

data ChatStorageError
    = NoSuchConversation ConversationId
    deriving stock (Show, Eq)
