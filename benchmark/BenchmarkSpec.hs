module Main where

import ChatCompletion.Benchmark.Google
import ChatCompletion.Benchmark.OpenAI
import Control.Exception (catch)
import Data.Text qualified as T
import Relude

main :: IO ()
main = do
    putStrLn "ChatCompletion Benchmark Suite"
    putStrLn "==============================\n"
    
    -- Check for API keys
    openAiKey <- lookupEnv "OPENAI_API_KEY"
    googleKey <- lookupEnv "GEMINI_API_KEY"
    
    -- Run OpenAI benchmarks if key is available
    case openAiKey of
        Just key -> do
            putStrLn "Running OpenAI benchmarks..."
            runOpenAIBenchmarks (T.pack key)
                `catch` \(e :: SomeException) -> 
                    putStrLn $ "OpenAI benchmarks failed: " <> show e
        Nothing -> 
            putStrLn "Skipping OpenAI benchmarks (OPENAI_API_KEY not set)"
    
    putStrLn ""
    
    -- Run Google benchmarks if key is available  
    case googleKey of
        Just key -> do
            putStrLn "Running Google benchmarks..."
            runGoogleBenchmarks (T.pack key)
                `catch` \(e :: SomeException) -> 
                    putStrLn $ "Google benchmarks failed: " <> show e
        Nothing -> 
            putStrLn "Skipping Google benchmarks (GEMINI_API_KEY not set)"
    
    putStrLn "\nBenchmark suite completed."