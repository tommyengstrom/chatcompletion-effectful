{-# LANGUAGE DeriveGeneric #-}

module ChatCompletion.Providers.Google.Types where

import ChatCompletion.Types (ConversationId)
import Data.Aeson
import Data.Vector (Vector)
import Relude

-- | Google API Key
newtype GoogleApiKey = GoogleApiKey Text
    deriving stock (Show, Eq, Ord, Generic)

-- | Google provider settings
data GoogleSettings = GoogleSettings
    { apiKey :: GoogleApiKey
    , model :: Text
    , baseUrl :: Text
    , responseLogger :: ConversationId -> GeminiChatResponse -> IO ()
    }
    deriving stock (Generic)

-- | Gemini API types
data GeminiContent = GeminiContent
    { parts :: Vector GeminiPart
    , role :: Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data GeminiPart
    = GeminiTextPart {text :: Text}
    | GeminiFunctionCallPart {functionCall :: GeminiFunctionCall}
    | GeminiFunctionResponsePart {functionResponse :: GeminiFunctionResponse}
    deriving stock (Show, Eq, Generic)

instance ToJSON GeminiPart where
    toJSON (GeminiTextPart t) = object ["text" .= t]
    toJSON (GeminiFunctionCallPart fc) = object ["functionCall" .= fc]
    toJSON (GeminiFunctionResponsePart fr) = object ["functionResponse" .= fr]

instance FromJSON GeminiPart where
    parseJSON = withObject "GeminiPart" $ \o -> do
        maybeText <- o .:? "text"
        maybeFunctionCall <- o .:? "functionCall"
        maybeFunctionResponse <- o .:? "functionResponse"
        case (maybeText, maybeFunctionCall, maybeFunctionResponse) of
            (Just t, Nothing, Nothing) -> pure $ GeminiTextPart t
            (Nothing, Just fc, Nothing) -> pure $ GeminiFunctionCallPart fc
            (Nothing, Nothing, Just fr) -> pure $ GeminiFunctionResponsePart fr
            _ ->
                fail
                    "Invalid GeminiPart: must have exactly one of text, functionCall, or functionResponse"

data GeminiFunctionCall = GeminiFunctionCall
    { name :: Text
    , args :: Value
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data GeminiFunctionResponse = GeminiFunctionResponse
    { name :: Text
    , response :: Value
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data GeminiTool = GeminiTool
    { functionDeclarations :: Vector GeminiFunctionDeclaration
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data GeminiFunctionDeclaration = GeminiFunctionDeclaration
    { name :: Text
    , description :: Text
    , parameters :: Maybe Value
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data GeminiChatRequest = GeminiChatRequest
    { contents :: Vector GeminiContent
    , tools :: Maybe (Vector GeminiTool)
    , systemInstruction :: Maybe GeminiContent
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data GeminiChatResponse = GeminiChatResponse
    { candidates :: Vector GeminiCandidate
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data GeminiCandidate = GeminiCandidate
    { content :: GeminiContent
    , finishReason :: Maybe Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)