module ChatCompletion.Provider.OpenAISpec where

import ChatCompletion
import ChatCompletion.TestHelpers
import ChatCompletion.Providers.OpenAI
import ChatCompletion.Storage.InMemory
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import Relude
import Test.Hspec

runOpenAI
    :: TVar (Map ConversationId [ChatMsg])
    -> Eff
        '[ ChatCompletion
         , ChatCompletionStorage
         , Error ChatStorageError
         , Error ChatCompletionError
         , IOE
         ]
        a
    -> IO a
runOpenAI tvar action = do
    apiKey <-
        maybe
            (error "OPENAI_API_KEY not set in environment")
            (pure . OpenAiApiKey . T.pack)
            =<< lookupEnv "OPENAI_API_KEY"
    let settings = defaultOpenAiSettings apiKey
    -- The handlers expect this effect order
    runEff
        . runErrorNoCallStackWith (error . show)
        . runErrorNoCallStackWith (error . show)
        . runChatCompletionStorageInMemory tvar
        $ runChatCompletionOpenAi settings action

spec :: Spec
spec = describe "ChatCompletion Provider - OpenAI" $ do
    -- Run common tests
    specWithProvider runOpenAI

    -- OpenAI-specific tests can be added here
    describe "OpenAI-specific features" $ do
        it "placeholder for OpenAI-specific tests" $ do
            True `shouldBe` True
