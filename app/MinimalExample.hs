{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import Effectful.OpenAI
import LlmChat
import LlmChat.Providers.OpenAI.ChatCompletion
import Relude hiding (lookupEnv, newTVarIO)
import System.Environment (getEnv)

main :: IO ()
main = do
    apiKey <- toText <$> getEnv "OPENAI_API_KEY"
    runEffectStack (defaultOpenAIConfig apiKey) do
        reply <-
            respondWithTools
                []
                [ SystemMsg "You are a helpful assistant."
                , UserMsg "Hello! What's 2 + 2?"
                ]
        putTextLn "Assistant reply:"
        print reply

runEffectStack
    :: OpenAIConfig
    -> Eff '[ LlmChat , OpenAI , Error LlmChatError , Concurrent , IOE ] a
    -> IO a
runEffectStack config =
    runEff
        . runConcurrent
        . runErrorNoCallStackWith @LlmChatError (error . show)
        . runOpenAI config
        . runLlmChat defaultChatCompletionSettings
