module Effectful.OpenAI where

import Control.Lens
import Data.Generics.Labels ()
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Exception
import Effectful.TH
import OpenAI.V1
import OpenAI.V1.Chat.Completions
import Relude
import Servant.Client (ClientError)
import ChatCompletion.Error

data OpenAI :: Effect where
    -- Send messages to the LLM and get a single response
    -- The response could be an assistant message or tool calls
    ChatCompletion :: CreateChatCompletion -> OpenAI m ChatCompletionObject

makeEffect ''OpenAI

data OpenAIConfig = OpenAIConfig
    { apiKey :: Text
    , baseUrl :: Text
    , organizationId :: Maybe Text
    , projectId :: Maybe Text
    }
    deriving stock (Generic)

defaultOpenAIConfig :: Text -> OpenAIConfig
defaultOpenAIConfig apiKey = OpenAIConfig
    { apiKey
    , baseUrl = "https://api.openai.com"
    , organizationId = Nothing
    , projectId = Nothing
    }

runOpenAI
    :: forall es a
     . ( IOE :> es
       , Error LlmRequestError :> es
       )
    => OpenAIConfig
    -> Eff (OpenAI ': es) a
    -> Eff es a
runOpenAI cfg eff = do
    clientEnv <- liftIO . getClientEnv $ cfg ^. #baseUrl
    let Methods{createChatCompletion} =
            makeMethods
                clientEnv
                (cfg ^. #apiKey)
                (cfg ^. #organizationId)
                (cfg ^. #projectId)

    interpretWith eff \_ -> \case
        ChatCompletion ccc ->
            try @ClientError (liftIO $ createChatCompletion ccc)
                >>= either (throwError . LlmRequestError) pure


