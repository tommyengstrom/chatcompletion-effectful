module LlmChat.Storage.Effect where

import LlmChat.Types
import Data.Text (Text)
import Effectful
import Effectful.TH
import Effectful.Time
import Prelude

data LlmChatStorage :: Effect where
    CreateConversation :: SystemPrompt -> LlmChatStorage m ConversationId
    DeleteConversation :: ConversationId -> LlmChatStorage m ()
    GetConversation :: ConversationId -> LlmChatStorage m [ChatMsg]
    AppendMessage :: ConversationId -> ChatMsg -> LlmChatStorage m ()
    ListConversations :: LlmChatStorage m [ConversationId]

type instance DispatchOf LlmChatStorage = 'Dynamic

makeEffect ''LlmChatStorage

appendUserMessage
    :: ( Time :> es
       , LlmChatStorage :> es
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
