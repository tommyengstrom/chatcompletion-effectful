module ChatCompletion.Provider.OpenAISpec where

import ChatCompletion
import ChatCompletion.Common
import ChatCompletion.Providers.OpenAI
import ChatCompletion.Storage.InMemory
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import Relude
import Test.Hspec

runOpenAI
    :: TVar (Map ConversationId [ChatMsg])
    -> Eff '[ChatCompletion, Error ChatCompletionError, ChatCompletionStorage, Error ChatCompletionStorageError, IOE] a
    -> IO a
runOpenAI tvar action = do
    apiKey <- maybe
        (error "OPENAI_API_KEY not set in environment")
        (pure . OpenAiApiKey . T.pack)
        =<< lookupEnv "OPENAI_API_KEY"
    let settings = defaultOpenAiSettings apiKey
    runEff
        $ runErrorNoCallStackWith (error . show)
        $ runChatCompletionStorageInMemory tvar
        $ runErrorNoCallStackWith (error . show)
        $ runChatCompletionOpenAi settings
        $ action

spec :: Spec
spec = describe "ChatCompletion Provider - OpenAI" $ do
    -- Run common tests
    specWithProvider runOpenAI
    
    -- OpenAI-specific tests can be added here
    describe "OpenAI-specific features" $ do
        it "placeholder for OpenAI-specific tests" $ do
            True `shouldBe` True