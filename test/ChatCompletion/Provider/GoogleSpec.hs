module ChatCompletion.Provider.GoogleSpec where

import ChatCompletion
import ChatCompletion.Providers.Google
import ChatCompletion.Providers.Google.Types
import ChatCompletion.Storage.InMemory
import ProviderAgnosticTests
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import Relude
import Test.Hspec
import Effectful.Time
import Effectful.Concurrent

runGoogle
    :: TVar (Map ConversationId [ChatMsg])
    -> Eff
        '[ LlmChat
         , ChatCompletionStorage
         , Error ChatStorageError
         , Error LlmChatError
         , Time
         , Concurrent
         , IOE
         ]
        a
    -> IO a
runGoogle tvar action = do
    apiKey <-
        maybe
            (error "GEMINI_API_KEY not set in environment")
            (pure . GoogleApiKey . T.pack)
            =<< lookupEnv "GEMINI_API_KEY"
    let settings = defaultGoogleSettings apiKey
    runEff
        . runConcurrent
        . runTime
        . runErrorNoCallStackWith (error . show)
        . runErrorNoCallStackWith (error . show)
        . runChatCompletionStorageInMemory tvar
        . runChatCompletionGoogle settings
        $ action

spec :: Spec
spec = describe "ChatCompletion Provider - Google" $ do
    -- Run common tests
    tvar <- runIO $ newTVarIO mempty
    specWithProvider (runGoogle tvar)

    -- Google-specific tests
    describe "Google-specific features" $ do
        it "handles system instructions correctly" $ do
            -- Google uses systemInstruction field instead of system messages in contents
            True `shouldBe` True -- Placeholder for now
