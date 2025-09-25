module LlmChat.Types
    ( module LlmChat.Types
    , module LlmChat.ChatMsg
    ) where

import LlmChat.ChatMsg
import Data.Aeson
import Data.OpenApi
import Data.Text (Text)
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
data ToolDef es = ToolDef
    { name :: ToolName
    , description :: ToolDescription
    , parameterSchema :: Maybe Value -- JSON schema for the tool parameters
    , executeFunction :: Value -> Eff es (Either String ToolResponse)
    }
    deriving stock (Generic)

data ToolDeclaration = ToolDeclaration
    { name :: ToolName
    , description :: ToolDescription
    , parameterSchema :: Maybe Value -- JSON schema for the tool parameters
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data ToolCallResponse
    = ToolCallResponse
    { id :: ToolCallId
    , response :: Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
