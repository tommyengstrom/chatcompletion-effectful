module Effect.ChatCompletionStorage.InMemorySpec where

--import Effectful
import Relude
-- import Effect.ChatCompletion.Types
-- import Effect.ChatCompletionStorage
-- import Effect.ChatCompletionStorage.InMemory
import Test.Hspec
-- import Test.QuickCheck


spec ::Spec
spec = do
    describe "ChatCompletionStorage InMemory" $ do
        it "should create, get, append and delete conversations" $ do
            False `shouldBe` True
