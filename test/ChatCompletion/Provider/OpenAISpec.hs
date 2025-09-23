module ChatCompletion.Provider.OpenAISpec where

import ChatCompletion
import Effectful.OpenAI
import ChatCompletion.Storage.InMemory
import ProviderAgnosticTests
import Data.Generics.Labels ()
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import Relude
import Test.Hspec
import ChatCompletion.Providers.OpenAI.ChatCompletion
import System.Environment (getEnv)
import Effectful.Time

runEffectStack
    :: TVar (Map ConversationId [ChatMsg])
    -> Eff
        '[ LlmChat
         , ChatCompletionStorage
         , OpenAI
         , Error ChatStorageError
         , Error ChatExpectationError
         , Error LlmRequestError
         , Time
         , IOE
         ]
        a
    -> IO a
runEffectStack tvar action = do
    cfg <- defaultOpenAIConfig . T.pack <$> getEnv "OPENAI_API_KEY"

    -- The handlers expect this effect order
    runEff
        . runTime
        . runErrorNoCallStackWith (error . show)
        . runErrorNoCallStackWith (error . show)
        . runErrorNoCallStackWith (error . show)
        . runOpenAI cfg
        . runChatCompletionStorageInMemory tvar
        $ runLlmChat defaultChatCompletionSettings action
        -- - $ runChatCompletionOpenAi settings action

spec :: Spec
spec = describe "ChatCompletion Provider - OpenAI" $ do
    -- Run common tests
    tvar <- runIO $ newTVarIO mempty
    specWithProvider (runEffectStack tvar)

    -- OpenAI-specific tests can be added here
    describe "OpenAI-specific features" $ do
        it "placeholder for OpenAI-specific tests" $ do
            True `shouldBe` True
