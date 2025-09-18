{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ChatCompletion.Error where

import Control.Concurrent (threadDelay)
import Control.Lens hiding ((.=))
import Data.Aeson (FromJSON, ToJSON)
import Data.Generics.Labels ()
import Data.List (lookup)
import Data.Time (NominalDiffTime)
import Network.HTTP.Types.Status (statusCode)
import Relude
import Servant.Client
    ( ClientError (..)
    , Response
    , ResponseF (..)
    )

data ChatCompletionError
    = HttpError HttpErrorDetails
    | RateLimitError RateLimitDetails
    | ParseError ParseErrorDetails
    | NetworkError NetworkErrorDetails
    | ProviderError Text
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data HttpErrorDetails = HttpErrorDetails
    { statusCode :: Int
    , message :: Text
    , responseBody :: Maybe Text
    , isRetryable :: Bool
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data RateLimitDetails = RateLimitDetails
    { retryAfter :: Maybe NominalDiffTime
    , message :: Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data ParseErrorDetails = ParseErrorDetails
    { expected :: Text
    , actual :: Text
    , parseError :: Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data NetworkErrorDetails = NetworkErrorDetails
    { operation :: Text
    , cause :: Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data RetryConfig = RetryConfig
    { maxAttempts :: Int
    , initialDelay :: NominalDiffTime
    , maxDelay :: NominalDiffTime
    , backoffMultiplier :: Double
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

defaultRetryConfig :: RetryConfig
defaultRetryConfig =
    RetryConfig
        { maxAttempts = 3
        , initialDelay = 1
        , maxDelay = 60
        , backoffMultiplier = 2
        }

fromServantError :: ClientError -> ChatCompletionError
fromServantError = \case
    FailureResponse _ response ->
        let status = statusCode $ responseStatusCode response
            body = decodeUtf8 $ responseBody response
         in case status of
                429 ->
                    RateLimitError
                        $ RateLimitDetails
                            { retryAfter = parseRetryAfter response
                            , message = body
                            }
                s
                    | s >= 400 && s < 500 ->
                        HttpError
                            $ HttpErrorDetails
                                { statusCode = s
                                , message = body
                                , responseBody = Just body
                                , isRetryable = False
                                }
                s
                    | s >= 500 ->
                        HttpError
                            $ HttpErrorDetails
                                { statusCode = s
                                , message = body
                                , responseBody = Just body
                                , isRetryable = True
                                }
                _ -> ProviderError $ "Unexpected status: " <> show status
    DecodeFailure msg _ ->
        ParseError
            $ ParseErrorDetails
                { expected = "Valid JSON response"
                , actual = msg
                , parseError = msg
                }
    ConnectionError exc ->
        NetworkError
            $ NetworkErrorDetails
                { operation = "HTTP request"
                , cause = toText $ displayException exc
                }
    err -> ProviderError $ show err

parseRetryAfter :: Response -> Maybe NominalDiffTime
parseRetryAfter response =
    lookup "Retry-After" (toList $ responseHeaders response)
        >>= \value -> readMaybe (decodeUtf8 value) <&> fromInteger

isRetryable :: ChatCompletionError -> Bool
isRetryable = \case
    HttpError details -> details ^. #isRetryable
    RateLimitError _ -> True
    NetworkError _ -> True
    _ -> False

getRetryDelay :: ChatCompletionError -> RetryConfig -> Int -> NominalDiffTime
getRetryDelay err config attempt = case err of
    RateLimitError details -> fromMaybe defaultDelay (details ^. #retryAfter)
    _ -> defaultDelay
  where
    defaultDelay =
        min (config ^. #maxDelay)
            $ (config ^. #initialDelay)
            * fromRational (toRational ((config ^. #backoffMultiplier) ^ attempt))

withRetry
    :: RetryConfig -> IO (Either ChatCompletionError a) -> IO (Either ChatCompletionError a)
withRetry config action = go 0
  where
    go attempt = do
        result <- action
        case result of
            Left err | isRetryable err && attempt < config ^. #maxAttempts -> do
                let delay = getRetryDelay err config attempt
                threadDelay (round $ delay * 1_000_000)
                go (attempt + 1)
            other -> pure other
