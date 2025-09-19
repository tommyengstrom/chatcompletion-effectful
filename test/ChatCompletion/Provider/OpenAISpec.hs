module ChatCompletion.Provider.OpenAISpec where

import ChatCompletion
import ChatCompletion.Providers.OpenAI
import ChatCompletion.Storage.InMemory
import ChatCompletion.TestHelpers
import Control.Lens
import Data.Generics.Labels ()
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import Effectful.OpenAI
import OpenAI.V1.Chat.Completions (ReasoningEffort (..))
import Relude
import Test.Hspec

runEffectStack
    :: TVar (Map ConversationId [ChatMsg])
    -> Eff
        '[ ChatCompletion
         , OpenAI
         , Error OpenAIError
         , ChatCompletionStorage
         , Error ChatStorageError
         , Error ChatCompletionError
         , IOE
         ]
        a
    -> IO a
runEffectStack tvar action = do
    cfg <-
        maybe
            (error "OPENAI_API_KEY not set in environment")
            (pure . defaultOpenAIConfig . T.pack)
            =<< lookupEnv "OPENAI_API_KEY"

    let settings :: OpenAiSettings es
        settings =
            defaultOpenAiSettings
                & #overrides .~ (#reasoning_effort ?~ ReasoningEffort_Minimal)
    -- The handlers expect this effect order
    runEff
        . runErrorNoCallStackWith (error . show)
        . runErrorNoCallStackWith (error . show)
        . runChatCompletionStorageInMemory tvar
        . runErrorNoCallStackWith (error . show)
        . runOpenAI cfg
        $ runChatCompletionOpenAi settings action

spec :: Spec
spec = describe "ChatCompletion Provider - OpenAI" $ do
    -- Run common tests
    specWithProvider runEffectStack

    -- OpenAI-specific tests can be added here
    describe "OpenAI-specific features" $ do
        it "placeholder for OpenAI-specific tests" $ do
            True `shouldBe` True
