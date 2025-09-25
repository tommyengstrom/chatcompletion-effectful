module LlmChat.Tool where

import LlmChat.Types
import Control.Lens
import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap qualified as KM
import Data.Generics.Labels ()
import Data.Map qualified as Map
import Data.OpenApi
import Effectful
import Relude

runTool
    :: ToolDef es
    -> Map Text Value
    -> Eff es (Either String ToolResponse)
runTool tool args = do
    -- Convert Map to JSON object
    let argsValue = Object (KM.fromMap (Map.mapKeys fromText args))
    tool ^. #executeFunction $ argsValue

defineToolNoArgument
    :: forall es
     . ToolName
    -> ToolDescription
    -> (Eff es (Either String ToolResponse))
    -- ^ Function to execute the tool
    -> ToolDef es
defineToolNoArgument name' description' executeFunction =
    ToolDef
        { name = name'
        , description = description'
        , parameterSchema = Nothing
        , executeFunction = \_ -> executeFunction
        }

defineToolWithArgument
    :: forall a es
     . (FromJSON a, ToSchema a)
    => ToolName
    -> ToolDescription
    -> (a -> Eff es (Either String ToolResponse))
    -- ^ Function to execute the tool
    -> ToolDef es
defineToolWithArgument name' description' executeFunction =
    ToolDef
        { name = name'
        , description = description'
        , parameterSchema =
            Just . toJSON $ toInlinedSchema (Proxy @a)
                & additionalProperties ?~ AdditionalPropertiesAllowed False
        , executeFunction = \args -> do
            case fromJSON args of
                Error err -> pure $ Left $ "Failed to parse tool arguments: " <> err
                Success val -> executeFunction val
        }
