module Effect.AiChat.Types where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Proxy
import Data.UUID(UUID)
import Effectful  (Eff)
import Data.Time
import Prelude
import Data.OpenApi (ToSchema(..))

type SystemPrompt = Text

newtype ConversationId = ConversationId UUID
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (FromJSON, ToJSON, ToSchema)

newtype ToolCallId = ToolCallId Text
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (FromJSON, ToJSON, ToSchema)

newtype ToolArgs = ToolArgs Text
    deriving stock (Show, Eq, Generic)
    deriving newtype (FromJSON, ToJSON)
instance ToSchema ToolArgs where
    declareNamedSchema _ = declareNamedSchema $ Proxy @Text

newtype ToolResponse = ToolResponse Value
    deriving stock (Show, Eq, Generic)
    deriving newtype (FromJSON, ToJSON)
instance ToSchema ToolResponse where
    declareNamedSchema _ = declareNamedSchema $ Proxy @Text

newtype UIComponent = UIComponent Value
    deriving stock (Show, Eq, Generic)
    deriving newtype (FromJSON, ToJSON)
instance ToSchema UIComponent where
    declareNamedSchema _ = declareNamedSchema $ Proxy @Text

data ChatMsg
    = SystemMsg
        { content :: Text
        , createdAt :: UTCTime
        }
    | UserMsg
        { content :: Text
        , createdAt :: UTCTime
        }
    | AssistantMsg
        { content :: Text
        , createdAt :: UTCTime
        }
    | ToolCallMsg
        { toolCalls :: [ToolCall]
        , createdAt :: UTCTime
        }
    | ToolCallResponseMsg
        { toolCallId :: ToolCallId
        , toolResponse :: ToolResponse
        , localUiComponent :: [UIComponent]
        , createdAt :: UTCTime
        }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

data ToolDef es = ToolDef
    { name :: Text
    , description :: Text
    , parmeterSchema :: Value -- JSON schema for the tool parameters
    , executeFunction :: Value -> Eff es (Either Text ToolResponse)
    }
    deriving stock (Generic)

data ToolCall
    = ToolCall
        { toolCallId :: ToolCallId
        , toolName :: Text
        , toolArgs :: ToolArgs
        }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

data ToolCallResponse
    = ToolCallResponse
        { id :: ToolCallId
        , toolResponse :: ToolResponse
        , localUiComponent :: [UIComponent]
        }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

