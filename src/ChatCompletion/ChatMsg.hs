module ChatCompletion.ChatMsg where

import Control.Lens
import Data.Aeson
import Data.OpenApi
    ( AdditionalProperties (..)
    , NamedSchema (..)
    , OpenApiType (..)
    , Referenced (..)
    , ToSchema
    , declareSchemaRef
    )
import Data.OpenApi.Lens
import Data.OpenApi.Schema (ToSchema (..))
import Data.Time
import Relude

-- | Conversion between the message type used by the provider and our representation
-- `toChatMsgIn` is subject to change if it turns out that one-to-one conversion isn't
-- general enough.
class
    (ToJSON msgIn, ToJSON msgOut) =>
    IsChatMsg msgIn msgOut
        | msgIn -> msgOut
        , msgOut -> msgIn
    where
    toChatMsgIn :: msgIn -> Either String ChatMsgIn

    fromChatMsg :: ChatMsg -> msgOut

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

type ToolName = Text
type ToolDescription = Text

newtype ToolCallId = ToolCallId Text
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (FromJSON, ToJSON, ToSchema)

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

newtype UIComponent = UIComponent Value
    deriving stock (Show, Eq, Generic)
    deriving newtype (FromJSON, ToJSON)

instance ToSchema UIComponent where
    declareNamedSchema _ = declareNamedSchema $ Proxy @Text
