module Effect.ChatCompletion.Types where

import Data.Aeson
import Data.OpenApi
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Proxy
import Control.Lens ((&), (?~), (.~))
import Data.UUID(UUID)
import Effectful  (Eff)
import Data.Time
import Prelude

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

data NoParameters = NoParameters
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance ToSchema NoParameters where
    declareNamedSchema _ = pure $ NamedSchema (Just "NoParameters") s
        where
            s = mempty
                & additionalProperties ?~ AdditionalPropertiesAllowed False
                & properties .~ mempty

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
        , createdAt :: UTCTime
        }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

data ToolDef es = ToolDef
    { name :: ToolName
    , description :: ToolDescription
    , parmeterSchema :: Maybe Value -- JSON schema for the tool parameters
    , executeFunction :: Value -> Eff es (Either String ToolResponse)
    }
    deriving stock (Generic)

type ToolName = Text
type ToolDescription = Text

defineToolNoArgument :: forall es.
     ToolName
    -> ToolDescription
    -> (Eff es (Either String ToolResponse)) -- ^ Function to execute the tool
    -> ToolDef es
defineToolNoArgument name' description' executeFunction = ToolDef
    { name = name'
    , description = description'
    , parmeterSchema = Nothing
    , executeFunction = \_ -> executeFunction
    }
defineTool :: forall a es. (FromJSON a, ToSchema a)
    => ToolName
    -> ToolDescription
    -> (a -> Eff es (Either String ToolResponse)) -- ^ Function to execute the tool
    -> ToolDef es
defineTool name' description' executeFunction = ToolDef
    { name = name'
    , description = description'
    , parmeterSchema = Just .toJSON $ toSchema (Proxy @a)
    , executeFunction = \args -> do
        case fromJSON args of
            Error err -> pure $ Left $ "Failed to parse tool arguments: " <> err
            Success val -> executeFunction val
    }

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
        { modelResponse :: Text -- ^ The value returned to the LLM
        , localResponse :: [UIComponent] -- ^ Components to render in the chat
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

