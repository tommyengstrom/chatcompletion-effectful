module ChatCompletion (module X, appendUserMessage) where

import ChatCompletion.Effect as X
import ChatCompletion.Storage.Effect as X
import ChatCompletion.Tool as X
import ChatCompletion.Types as X
import Data.Text (Text)
import Data.Time
import Effectful

appendUserMessage
    :: IOE :> es
    => ChatCompletionStorage :> es
    => ConversationId
    -> Text
    -> Eff es [ChatMsg]
appendUserMessage conversationId content = do
    currentTime <- liftIO getCurrentTime
    let userMsg = UserMsg{content, createdAt = currentTime}
    appendMessages conversationId [userMsg]
