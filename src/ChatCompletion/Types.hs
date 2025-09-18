module ChatCompletion.Types where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Map (Map)
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

data ResponseFormat
    = Unstructured
    | JsonValue
    | JsonSchema Value
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

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
        , hidden :: Bool
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
        , hidden :: Bool
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

-- | Represents a tool/function call made by the LLM
data ToolCall
    = ToolCall
    { toolCallId :: ToolCallId
    -- ^ Unique identifier for this tool call
    , toolName :: ToolName
    -- ^ Name of the tool to invoke
    , toolArgs :: Map Text Value
    -- ^ Arguments as key-value pairs
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance ToSchema ToolCall where
    declareNamedSchema _ = do
        textSchema <- declareSchemaRef (Proxy @Text)
        return $
            NamedSchema (Just "ToolCall") $
                mempty
                    & type_ ?~ OpenApiObject
                    & properties
                        .~ [ ("toolCallId", textSchema)
                           , ("toolName", textSchema)
                           ,
                               ( "toolArgs"
                               , Inline $
                                    mempty
                                        & type_ ?~ OpenApiObject
                                        & additionalProperties ?~ AdditionalPropertiesAllowed True
                               )
                           ]
                    & required .~ ["toolCallId", "toolName", "toolArgs"]

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
