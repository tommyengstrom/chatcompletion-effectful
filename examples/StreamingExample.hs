{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module StreamingExample where

import ChatCompletion
import ChatCompletion.Providers.OpenAI
import ChatCompletion.Storage.InMemory
import Data.Time (getCurrentTime)
import Effectful
import Effectful.Error.Static
import Relude
import Data.Text qualified as Text
import System.Environment (getEnv)

main :: IO ()
main = do
    apiKeyText <- Text.pack <$> getEnv "OPENAI_API_KEY"
    let apiKey = OpenAiApiKey apiKeyText
        settings = defaultOpenAiSettings apiKey
    
    storageVar <- newTVarIO mempty
    
    result <- runEff
        . runError @ChatCompletionError
        . runChatCompletionStorageInMemory storageVar
        . runChatCompletionOpenAi settings
        $ do
            -- Create a conversation
            conversationId <- createConversation "You are a helpful assistant."
            
            -- Define a simple tool
            let tools = 
                    [ defineToolNoArgument
                        "get_current_time"
                        "Get the current time"
                        (do
                            time <- liftIO getCurrentTime
                            pure $ Right $ ToolResponse
                                { modelResponse = "Current time: " <> show time
                                , localResponse = []
                                }
                        )
                    ]
            
            -- Test with streaming - messages will be printed as they're produced
            liftIO $ putStrLn "=== Testing With Streaming ==="
            msgs1 <- respondWithTools 
                (\msg -> liftIO $ putStrLn $ "STREAM: " <> show msg)
                tools 
                conversationId 
                "What time is it?"
            liftIO $ putStrLn $ "Total messages: " <> show (length msgs1)
            
            -- Test without streaming callback (silent collection)
            liftIO $ putStrLn "\n=== Testing Without Streaming Callback ==="
            msgs2 <- respondWithTools
                (const $ pure ())  -- No-op callback
                tools 
                conversationId 
                "Tell me the time again"
            liftIO $ forM_ msgs2 $ \msg -> 
                putStrLn $ "COLLECTED: " <> show msg
            
            pure ()
    
    case result of
        Left err -> putStrLn $ "Error: " <> show err
        Right _ -> putStrLn "Success!"