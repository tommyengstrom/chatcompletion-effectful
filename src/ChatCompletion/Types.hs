module ChatCompletion.Types where

import Data.Aeson
import Data.OpenApi
import Data.Proxy
import Data.Text (Text)
import Data.Time
import Data.UUID (UUID)
import Effectful (Eff)
import GHC.Generics (Generic)
import Web.HttpApiData
import Prelude

type SystemPrompt = Text

newtype ConversationId = ConversationId UUID
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype
        ( FromJSON
        , ToJSON
        , ToSchema
        , ToParamSchema
        , FromHttpApiData
        , ToHttpApiData
        )

newtype ToolCallId = ToolCallId Text
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (FromJSON, ToJSON, ToSchema)

newtype ToolArgs = ToolArgs Text
    deriving stock (Show, Eq, Generic)
    deriving newtype (FromJSON, ToJSON)
instance ToSchema ToolArgs where
    declareNamedSchema _ = declareNamedSchema $ Proxy @Text

newtype UIComponent = UIComponent Value
    deriving stock (Show, Eq, Generic)
    deriving newtype (FromJSON, ToJSON)
instance ToSchema UIComponent where
    declareNamedSchema _ = declareNamedSchema $ Proxy @Text

data ChatMsgIn
    = SystemMsgIn
        { content :: Text
        }
    | UserMsgIn
        { content :: Text
        }
    | AssistantMsgIn
        { content :: Text
        }
    | ToolCallMsgIn
        { toolCalls :: [ToolCall]
        }
    | ToolCallResponseMsgIn
        { toolCallId :: ToolCallId
        , toolResponse :: ToolResponse
        }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

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
        , createdAt :: UTCTime
        }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

data ToolDef es = ToolDef
    { name :: ToolName
    , description :: ToolDescription
    , parameterSchema :: Maybe Value -- JSON schema for the tool parameters
    , executeFunction :: Value -> Eff es (Either String ToolResponse)
    }
    deriving stock (Generic)

type ToolName = Text
type ToolDescription = Text

data ToolDeclaration = ToolDeclaration
    { name :: ToolName
    , description :: ToolDescription
    , parameterSchema :: Maybe Value -- JSON schema for the tool parameters
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data ToolCall
    = ToolCall
    { toolCallId :: ToolCallId
    , toolName :: ToolName
    , toolArgs :: ToolArgs
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

data ToolResponse
    = ToolResponse
    { modelResponse :: Text
    -- ^ The value returned to the LLM
    , localResponse :: [UIComponent]
    -- ^ Components to render in the chat
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

data ToolCallResponse
    = ToolCallResponse
    { id :: ToolCallId
    , response :: Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
