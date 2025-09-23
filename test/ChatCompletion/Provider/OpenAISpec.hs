module ChatCompletion.Provider.OpenAISpec where

import ChatCompletion
import Effectful.OpenAI
import ChatCompletion.Storage.InMemory
import ChatCompletion.TestHelpers
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
        '[ OpenAI
         , Time
         , Error LlmRequestError
         , ChatCompletionStorage
         , Error ChatStorageError
         , Error ChatExpectationError
         , IOE
         ]
        a
    -> IO a
runEffectStack tvar action = do
    cfg <- defaultOpenAIConfig . T.pack <$> getEnv "OPENAI_API_KEY"

    -- The handlers expect this effect order
    runEff
        . runErrorNoCallStackWith (error . show)
        . runErrorNoCallStackWith (error . show)
        . runChatCompletionStorageInMemory tvar
        . runErrorNoCallStackWith (error . show)
        . runTime
        $ runOpenAI cfg action
        -- - $ runChatCompletionOpenAi settings action

spec :: Spec
spec = describe "ChatCompletion Provider - OpenAI" $ do
    -- Run common tests
    tvar <- runIO $ newTVarIO mempty
    specWithProvider (runEffectStack tvar)
        (mkChatCompletionRequest defaultChatCompletionSettings)

    -- OpenAI-specific tests can be added here
    describe "OpenAI-specific features" $ do
        it "placeholder for OpenAI-specific tests" $ do
            True `shouldBe` True
