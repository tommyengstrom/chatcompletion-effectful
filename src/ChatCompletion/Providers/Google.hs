{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module ChatCompletion.Providers.Google where

import ChatCompletion.Effect
import ChatCompletion.Providers.Google.API
import ChatCompletion.Providers.Google.Convert
import ChatCompletion.Providers.Google.Types
import ChatCompletion.Types
import Control.Lens hiding ((.=))
import Data.Aeson.Text (encodeToLazyText)
import Data.Generics.Labels ()
import Data.Generics.Product
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time
import Data.Aeson (toJSON)
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
        , requestLogger = \_ -> pure ()
        }

-- | Run the ChatCompletion effect using Google's Gemini API
runChatCompletionGoogle
    :: forall es a
     . ( IOE :> es
       , Error ChatExpectationError :> es
       )
    => GoogleSettings
    -> Eff (ChatCompletion ': es) a
    -> Eff es a
runChatCompletionGoogle settings es = do
    manager <- liftIO newTlsManager
    let baseUrl' = parseBaseUrl $ T.unpack (settings ^. #baseUrl)
    case baseUrl' of
        Left err -> throwError $ ChatExpectationError $ "Invalid base URL: " <> show err
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
        SendMessages responseFormat tools messages ->
            sendMessagesToGemini generateContent clientEnv responseFormat tools messages

    adapt :: ClientEnv -> ClientM x -> Eff es x
    adapt env m = do
        result <- liftIO $ runClientM m env
        case result of
            Left err -> throwError . ChatRequestError $  err
            Right x -> pure x

    sendMessagesToGemini
        :: ([Text] -> Text -> GeminiChatRequest -> ClientM GeminiChatResponse)
        -> ClientEnv
        -> ResponseFormat
        -> [ToolDeclaration]
        -> [ChatMsg]
        -> Eff es ChatMsg
    sendMessagesToGemini generateContent clientEnv responseFormat tools' messages = do
        let hasTools = not (null tools')
        let tools =
                if hasTools
                    then Just $ V.singleton $ GeminiTool $ V.fromList $ fmap mkToolFromDeclaration tools'
                    else Nothing

        let (originalSystemInstruction, otherMessages) = extractSystemMessage messages

        -- When we have both tools and structured output, use prompt instructions
        -- instead of generationConfig (which Google doesn't support with tools)
        let systemInstruction = case (hasTools, responseFormat, originalSystemInstruction) of
                (True, JsonValue, Just sysContent) ->
                    Just
                        $ sysContent
                            { parts =
                                (sysContent ^. #parts)
                                    <> V.singleton
                                        ( GeminiTextPart
                                            "\n\nCRITICAL REQUIREMENT: You MUST respond with valid JSON only. Do not include any text outside the JSON object. Do not wrap in markdown code blocks."
                                        )
                            }
                (True, JsonValue, Nothing) ->
                    Just
                        $ GeminiContent
                            { role = "system"
                            , parts =
                                V.singleton
                                    ( GeminiTextPart
                                        "CRITICAL REQUIREMENT: You MUST respond with valid JSON only. Do not include any text outside the JSON object. Do not wrap in markdown code blocks."
                                    )
                            }
                (True, JsonSchema schema, Just sysContent) ->
                    Just
                        $ sysContent
                            { parts =
                                (sysContent ^. #parts)
                                    <> V.singleton
                                        ( GeminiTextPart
                                            $ "\n\nCRITICAL REQUIREMENT: You MUST respond with valid JSON matching this schema. Do not include any text outside the JSON. Do not wrap in markdown code blocks.\nSchema:\n"
                                            <> TL.toStrict (encodeToLazyText schema)
                                        )
                            }
                (True, JsonSchema schema, Nothing) ->
                    Just
                        $ GeminiContent
                            { role = "system"
                            , parts =
                                V.singleton
                                    ( GeminiTextPart
                                        $ "CRITICAL REQUIREMENT: You MUST respond with valid JSON matching this schema. Do not include any text outside the JSON. Do not wrap in markdown code blocks.\nSchema:\n"
                                        <> TL.toStrict (encodeToLazyText schema)
                                    )
                            }
                _ -> originalSystemInstruction

        let contents = V.fromList $ mapMaybe toGeminiContent otherMessages
        let modelName = settings ^. #model
        let modelPath = [modelName <> ":generateContent"]

        -- Only use generationConfig when we don't have tools
        -- (Google doesn't support both simultaneously)
        let generationConfig =
                if hasTools
                    then Nothing -- Can't use with tools, rely on prompt instructions instead
                    else case responseFormat of
                        Unstructured -> Nothing
                        JsonValue ->
                            Just
                                $ GeminiGenerationConfig
                                    { responseMimeType = Just "application/json"
                                    , responseSchema = Nothing
                                    }
                        JsonSchema schema ->
                            Just
                                $ GeminiGenerationConfig
                                    { responseMimeType = Just "application/json"
                                    , responseSchema = Just schema
                                    }
            req :: GeminiChatRequest
            req = GeminiChatRequest
                        { contents = contents
                        , tools = tools
                        , systemInstruction = systemInstruction
                        , generationConfig = generationConfig
                        }
        response <-
            adapt clientEnv
                $ generateContent
                    modelPath
                    (settings ^. #apiKey . typed @Text)
                    req

        liftIO $ (settings ^. #requestLogger) (toJSON response)

        case response ^? #candidates . taking 1 folded . #content of
            Nothing -> throwError $ ChatExpectationError "No content in Gemini response"
            Just geminiContent -> do
                now <- liftIO getCurrentTime
                fromGeminiContent now geminiContent
