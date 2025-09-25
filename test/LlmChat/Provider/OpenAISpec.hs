module LlmChat.Provider.OpenAISpec where

import LlmChat
import Effectful.OpenAI
import LlmChat.Storage.InMemory
import ProviderAgnosticTests
import Data.Generics.Labels ()
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import Relude
import Test.Hspec
import LlmChat.Providers.OpenAI.ChatCompletion
import System.Environment (getEnv)
import Effectful.Time
import Effectful.Concurrent

runEffectStack
    :: TVar (Map ConversationId [ChatMsg])
    -> Eff
        '[ LlmChat
         , LlmChatStorage
         , OpenAI
         , Error ChatStorageError
         , Error LlmChatError
         , Time
         , Concurrent
         , IOE
         ]
        a
    -> IO a
runEffectStack tvar action = do
    cfg <- defaultOpenAIConfig . T.pack <$> getEnv "OPENAI_API_KEY"

    -- The handlers expect this effect order
    runEff
        . runConcurrent
        . runTime
        . runErrorNoCallStackWith (error . show)
        . runErrorNoCallStackWith (error . show)
        . runOpenAI cfg
        . runLlmChatStorageInMemory tvar
        $ runLlmChat defaultChatCompletionSettings action
        -- - $ runChatCompletionOpenAi settings action

spec :: Spec
spec = describe "LlmChat Provider - OpenAI" $ do
    -- Run common tests
    tvar <- runIO $ newTVarIO mempty
    specWithProvider (runEffectStack tvar)

    -- OpenAI-specific tests can be added here
    describe "OpenAI-specific features" $ do
        it "placeholder for OpenAI-specific tests" $ do
            True `shouldBe` True
