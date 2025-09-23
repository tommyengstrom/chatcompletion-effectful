{-# LANGUAGE RecordWildCards #-}
module Effectful.OpenAI where

import Data.Generics.Labels ()
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Exception
import Effectful.TH
import OpenAI.V1
import OpenAI.V1.Chat.Completions
import Relude hiding (Reader, runReader, ask)
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
 --   , requestLogger :: forall es. Maybe ConversationId -> Value -> Eff es ()
    }

defaultOpenAIConfig :: Text -> OpenAIConfig
defaultOpenAIConfig apiKey = OpenAIConfig
    { apiKey
    , baseUrl = "https://api.openai.com"
    , organizationId = Nothing
    , projectId = Nothing
  --  , requestLogger = \_ _ -> pure ()
    }

runOpenAI
    :: forall es a
     . ( IOE :> es
       , Error LlmRequestError :> es
       )
    => OpenAIConfig
    -> Eff (OpenAI ': es) a
    -> Eff es a
runOpenAI OpenAIConfig {..} eff = do
    clientEnv <- liftIO . getClientEnv $ baseUrl
    let Methods{createChatCompletion} =
            makeMethods
                clientEnv
                apiKey
                organizationId
                projectId

    interpretWith eff \_ -> \case
        ChatCompletion ccc -> do
            response <- try @ClientError (liftIO $ createChatCompletion ccc)
            case response of
                Right r -> do
                    pure r
                Left err -> do
                    throwError $ LlmRequestError err


