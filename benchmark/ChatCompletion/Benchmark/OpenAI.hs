module ChatCompletion.Benchmark.OpenAI where

import ChatCompletion
import ChatCompletion.Benchmark.Common
import ChatCompletion.Providers.OpenAI
import ChatCompletion.Storage.InMemory
import Control.Exception (catch, try)
import Data.Char (isDigit)
import Data.List (isInfixOf)
import Data.Text qualified as T
import Data.Time.Clock (NominalDiffTime, getCurrentTime)
import Effectful
import Effectful.Error.Static
import Relude
import Text.Printf

-- Models to benchmark
openAIModels :: [Text]
openAIModels = 
    [ "gpt-4o"
    , "gpt-4o-mini"
    , "gpt-5"
    , "gpt-5-mini"
    , "gpt-5-nano"
    ]

-- Format time for display
formatTime :: NominalDiffTime -> Text
formatTime t = T.pack $ printf "%.1fs" (realToFrac t :: Double)

-- Extract error information from exception string
extractErrorInfo :: String -> String
extractErrorInfo err
    | "statusCode = 503" `isInfixOf` err = "503 Service Unavailable"
    | "statusCode = 502" `isInfixOf` err = "502 Bad Gateway"
    | "statusCode = 500" `isInfixOf` err = "500 Internal Server Error"
    | "statusCode = 429" `isInfixOf` err = "429 Too Many Requests"
    | "statusCode = 401" `isInfixOf` err = "401 Unauthorized"
    | "statusCode = 403" `isInfixOf` err = "403 Forbidden"
    | "statusCode = 404" `isInfixOf` err = "404 Not Found"
    | "statusCode = 400" `isInfixOf` err = "400 Bad Request"
    | "NetworkError" `isInfixOf` err = 
        let statusCode = extractStatusCode err
        in if statusCode /= ""
           then statusCode <> " Network Error"
           else "Network Error"
    | otherwise = take 80 err  -- Fallback to first 80 chars of error
  where
    extractStatusCode :: String -> String
    extractStatusCode s = 
        case T.breakOn "statusCode = " (T.pack s) of
            (_, rest) | not (T.null rest) -> 
                let code = T.take 3 $ T.drop 13 rest  -- Skip "statusCode = "
                in if T.all isDigit code
                   then T.unpack code
                   else ""
            _ -> ""

-- Run benchmarks for all OpenAI models
runOpenAIBenchmarks :: Text -> IO ()
runOpenAIBenchmarks apiKey = do
    putStrLn "\n--- OpenAI Benchmarks ---"
    forM_ openAIModels $ \model -> do
        putStrLn $ "\nBenchmarking " <> T.unpack model <> "..."
        benchmarkOpenAIModel apiKey model
            `catch` \(e :: SomeException) -> do
                putStrLn $ "  Unexpected error: " <> take 100 (show e)
                pure BenchmarkResult
                    { modelName = model
                    , simpleMessageTime = 0
                    , toolCallTime = 0
                    , sequentialToolsTime = 0
                    }

-- Benchmark a specific OpenAI model
benchmarkOpenAIModel :: Text -> Text -> IO BenchmarkResult
benchmarkOpenAIModel apiKey model = do
    let settings = (defaultOpenAiSettings (OpenAiApiKey apiKey))
            { model = model }
    
    -- Get current time to make prompts unique and prevent caching
    currentTime <- getCurrentTime
    let timeStamp = T.pack $ show currentTime
    
    -- Simple message benchmark
    putStr "  - Simple message: "
    simpleResult <- try $ timeAction $ runOpenAIBenchmark settings $ do
        convId <- createConversation $ "Act exactly as a simple calculator. No extra text, just the answer. [Session: " <> timeStamp <> "]"
        appendUserMessage convId "2 + 2"
        messages <- getConversation convId
        resp <- sendMessages Unstructured [] messages
        appendMessage convId (chatMsgToIn resp)
        pure ()
    simpleTime <- case simpleResult of
        Right (time, _) -> do
            putStrLn $ T.unpack $ formatTime time
            pure time
        Left (e :: SomeException) -> do
            putStrLn $ "FAILURE - " <> extractErrorInfo (show e)
            pure 0
    
    -- Tool call benchmark
    putStr "  - Tool call: "
    toolResult <- try $ timeAction $ runOpenAIBenchmark settings $ do
        convId <- createConversation $ 
            "You are the users assistant. When asked about contacts or phone numbers, use the available tools to find the information. [Session: " <> timeStamp <> "]"
        _ <- respondWithTools (\_ -> pure ()) [listContacts] convId "What is my friend John's last name?"
        pure ()
    toolTime <- case toolResult of
        Right (time, _) -> do
            putStrLn $ T.unpack $ formatTime time
            pure time
        Left (e :: SomeException) -> do
            putStrLn $ "FAILURE - " <> extractErrorInfo (show e)
            pure 0
    
    -- Sequential tool calls benchmark
    putStr "  - Sequential tools: "
    seqResult <- try $ timeAction $ runOpenAIBenchmark settings $ do
        convId <- createConversation $ 
            "You are the users assistant, always trying to help them without first clarifying what they want. When asked about contacts or phone numbers, use the available tools to find the information. [Session: " <> timeStamp <> "]"
        _ <- respondWithTools (\_ -> pure ()) [listContacts, showPhoneNumber] convId "What is John's phone number?"
        pure ()
    seqTime <- case seqResult of
        Right (time, _) -> do
            putStrLn $ T.unpack $ formatTime time
            pure time
        Left (e :: SomeException) -> do
            putStrLn $ "FAILURE - " <> extractErrorInfo (show e)
            pure 0
    
    pure BenchmarkResult
        { modelName = model
        , simpleMessageTime = simpleTime
        , toolCallTime = toolTime
        , sequentialToolsTime = seqTime
        }

-- Run a single benchmark with OpenAI settings
runOpenAIBenchmark
    :: OpenAiSettings
    -> Eff '[ChatCompletion, ChatCompletionStorage, Error ChatCompletionError, IOE] a
    -> IO a
runOpenAIBenchmark settings action = do
    tvar <- newTVarIO mempty
    runEff
        $ runErrorNoCallStackWith (error . show)
        $ runChatCompletionStorageInMemory tvar
        $ runChatCompletionOpenAi settings
        $ action