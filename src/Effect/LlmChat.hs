module Effect.LlmChat where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.OpenApi (ToSchema)

newtype MessageId = MessageId { unMessageId :: Text }
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (FromJSON, ToJSON, ToSchema)

data ChatMsg = ChatMsg
    { messageId :: MessageId
    , role :: ChatRole
    , content :: [ChatContent]
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

data ChatRole = RoleUser | RoleAssistant
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

data ChatContent
    = ContentText { text :: Text }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
