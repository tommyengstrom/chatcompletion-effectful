{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

-- | Google Gemini provider for the ChatCompletion effect.
--
-- This module provides integration with Google's Gemini API for chat completions.
-- It supports:
--
-- * Text-based conversations
-- * System instructions
-- * Function calling (tools)
-- * Multiple tool calls in a single response
--
-- Note: Gemini requires at least one user message in the conversation.
-- System-only messages are not supported.
--
-- Example usage:
--
-- @
-- import ChatCompletion.Providers.Google
--
-- main = do
--     apiKey <- GoogleApiKey <$> getEnv "GEMINI_API_KEY"
--     let settings = defaultGoogleSettings apiKey
--     runEff
--         $ runError @ChatCompletionError
--         $ runChatCompletionGoogle settings
--         $ do
--             response <- sendMessages [] [UserMsg "Hello!" now]
--             liftIO $ print response
-- @
module ChatCompletion.Providers.Google
    ( -- * Types
      module ChatCompletion.Providers.Google.Types

      -- * API
    , module ChatCompletion.Providers.Google.API

      -- * Conversions
    , module ChatCompletion.Providers.Google.Convert

      -- * Settings
    , defaultGoogleSettings

      -- * Effect Handler
    , runChatCompletionGoogle
    ) where

import ChatCompletion.Effect
import ChatCompletion.Providers.Google.API
import ChatCompletion.Providers.Google.Convert
import ChatCompletion.Providers.Google.Types
import ChatCompletion.Types
import Control.Lens hiding ((.=))
import Data.Generics.Labels ()
import Data.Generics.Product
import Data.Text qualified as T
import Data.Time
import Data.UUID (nil)
import Data.Vector qualified as V
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Network.HTTP.Client.TLS (newTlsManager)
import Relude
import Servant.Client

-- | Default settings for Google provider
defaultGoogleSettings :: GoogleApiKey -> GoogleSettings
defaultGoogleSettings apiKey =
    GoogleSettings
        { apiKey = apiKey
        , model = "gemini-2.5-flash"
        , baseUrl = "https://generativelanguage.googleapis.com"
        , responseLogger = \_ _ -> pure ()
        }

-- | Run the ChatCompletion effect using Google's Gemini API
runChatCompletionGoogle
    :: forall es a
     . ( IOE :> es
       , Error ChatCompletionError :> es
       )
    => GoogleSettings
    -> Eff (ChatCompletion ': es) a
    -> Eff es a
runChatCompletionGoogle settings es = do
    manager <- liftIO newTlsManager
    let baseUrl' = parseBaseUrl $ T.unpack (settings ^. #baseUrl)
    case baseUrl' of
        Left err -> throwError $ ProviderError $ "Invalid base URL: " <> show err
        Right url -> do
            let clientEnv = mkClientEnv manager url
            let generateContent = client geminiAPI
            runChatCompletion generateContent clientEnv es
  where
    runChatCompletion
        :: ([Text] -> Text -> GeminiChatRequest -> ClientM GeminiChatResponse)
        -> ClientEnv
        -> Eff (ChatCompletion ': es) a
        -> Eff es a
    runChatCompletion generateContent clientEnv = interpret \_ -> \case
        SendMessages tools messages ->
            sendMessagesToGemini generateContent clientEnv tools messages

    adapt :: ClientEnv -> ClientM x -> Eff es x
    adapt env m = do
        result <- liftIO $ runClientM m env
        case result of
            Left err -> throwError $ fromServantError err
            Right x -> pure x

    sendMessagesToGemini
        :: ([Text] -> Text -> GeminiChatRequest -> ClientM GeminiChatResponse)
        -> ClientEnv
        -> [ToolDeclaration]
        -> [ChatMsg]
        -> Eff es ChatMsg
    sendMessagesToGemini generateContent clientEnv tools' messages = do
        let tools =
                if null tools'
                    then Nothing
                    else Just $ V.singleton $ GeminiTool $ V.fromList $ fmap mkToolFromDeclaration tools'
        let (systemInstruction, otherMessages) = extractSystemMessage messages
        let contents = V.fromList $ mapMaybe toGeminiContent otherMessages
        let modelName = settings ^. #model
        let modelPath = [modelName <> ":generateContent"]
        response <-
            adapt clientEnv
                $ generateContent
                    modelPath
                    (settings ^. #apiKey . typed @Text)
                    GeminiChatRequest
                        { contents = contents
                        , tools = tools
                        , systemInstruction = systemInstruction
                        }

        liftIO $ (settings ^. #responseLogger) (ConversationId nil) response

        case response ^? #candidates . taking 1 folded . #content of
            Nothing -> throwError $ ProviderError "No content in Gemini response"
            Just geminiContent -> do
                now <- liftIO getCurrentTime
                fromGeminiContent now geminiContent
