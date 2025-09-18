module ChatCompletion.Storage.Effect where

import ChatCompletion.Types
import Effectful
import Effectful.TH
import Prelude
import Data.Text (Text)

data ChatCompletionStorage :: Effect where
    CreateConversation :: SystemPrompt -> ChatCompletionStorage m ConversationId
    DeleteConversation :: ConversationId -> ChatCompletionStorage m ()
    GetConversation :: ConversationId -> ChatCompletionStorage m [ChatMsg]
    AppendMessage :: ConversationId -> ChatMsgIn -> ChatCompletionStorage m ()
    ListConversations :: ChatCompletionStorage m [ConversationId]

type instance DispatchOf ChatCompletionStorage = 'Dynamic

makeEffect ''ChatCompletionStorage


appendUserMessage
    :: ChatCompletionStorage :> es
    => ConversationId
    -> Text
    -> Eff es ()
appendUserMessage convId content = do
    let userMsgIn = UserMsgIn{content, hidden=False}
    appendMessage convId userMsgIn


data ChatStorageError
    = NoSuchConversation ConversationId
    deriving stock (Show, Eq)



