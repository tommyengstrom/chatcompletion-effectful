
module Effect.ChatCompletion.Tool where

import Control.Lens
import Data.Aeson
import Data.OpenApi
import Relude
import Data.Generics.Labels ()
import Data.Generics.Product
import Effect.ChatCompletion
import Effect.ChatCompletion.Types
import Effectful
import Effectful.Error.Static


runTool
    :: Error ChatCompletionError :> es
    => ToolDef es
    -> ToolArgs
    -> Eff es (Either String ToolResponse)
runTool tool args = do
    parsedArgs <- case args ^. typed @Text . to eitherDecodeStrictText of
        Left err -> throwError $ ChatCompletionError $ "Failed to parse tool arguments: " <> err
        Right val -> pure val
    tool ^. #executeFunction $ parsedArgs



defineToolNoArgument :: forall es.
     ToolName
    -> ToolDescription
    -> (Eff es (Either String ToolResponse)) -- ^ Function to execute the tool
    -> ToolDef es
defineToolNoArgument name' description' executeFunction = ToolDef
    { name = name'
    , description = description'
    , parameterSchema = Nothing
    , executeFunction = \_ -> executeFunction
    }

defineToolWithArgument :: forall a es. (FromJSON a, ToSchema a)
    => ToolName
    -> ToolDescription
    -> (a -> Eff es (Either String ToolResponse)) -- ^ Function to execute the tool
    -> ToolDef es
defineToolWithArgument name' description' executeFunction = ToolDef
    { name = name'
    , description = description'
    , parameterSchema = Just .toJSON $ toSchema (Proxy @a)
        & additionalProperties ?~ AdditionalPropertiesAllowed False
    , executeFunction = \args -> do
        case fromJSON args of
            Error err -> pure $ Left $ "Failed to parse tool arguments: " <> err
            Success val -> executeFunction val
    }
