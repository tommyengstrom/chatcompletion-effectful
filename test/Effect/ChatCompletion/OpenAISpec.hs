module Effect.ChatCompletion.OpenAISpec where

import Test.Hspec
import Relude

spec :: Spec
spec = describe "ChatCompletion OpenAI" $ do
    it "should create, get, append and delete conversations" $ do
        False `shouldBe` True
