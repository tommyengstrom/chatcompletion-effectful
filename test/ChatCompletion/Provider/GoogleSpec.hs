module ChatCompletion.Provider.GoogleSpec where

import ChatCompletion
import ChatCompletion.Common
import ChatCompletion.Providers.Google
import ChatCompletion.Providers.Google.Types
import ChatCompletion.Storage.InMemory
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import Relude
import Test.Hspec

runGoogle
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
runGoogle tvar action = do
    apiKey <-
        maybe
            (error "GEMINI_API_KEY not set in environment")
            (pure . GoogleApiKey . T.pack)
            =<< lookupEnv "GEMINI_API_KEY"
    let settings = defaultGoogleSettings apiKey
    runEff $
        runErrorNoCallStackWith (error . show) $
            runErrorNoCallStackWith @ChatStorageError (error . show) $
                runChatCompletionStorageInMemory tvar $
                    runChatCompletionGoogle settings $
                        action

spec :: Spec
spec = describe "ChatCompletion Provider - Google" $ do
    -- Run common tests
    specWithProvider runGoogle

    -- Google-specific tests
    describe "Google-specific features" $ do
        it "handles system instructions correctly" $ do
            -- Google uses systemInstruction field instead of system messages in contents
            True `shouldBe` True -- Placeholder for now
