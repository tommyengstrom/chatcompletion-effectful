module Effect.ChatCompletion.OpenAISpec where

import Data.Text qualified as T
import Effect.ChatCompletion
import Effect.ChatCompletion.OpenAI
import Effect.ChatCompletion.Types
import Effect.ChatCompletionStorage
import Data.Time
import Effect.ChatCompletionStorage.InMemory
import Effectful
import Effectful.Error.Static
import Relude
import Test.Hspec

runEffectStack
    :: OpenAiSettings
    -> TVar (Map ConversationId [ChatMsg])
    -> Eff
        '[ ChatCompletion
         , Error ChatCompletionError
         , ChatCompletionStorage
         , Error ChatCompletionStorageError
         , IOE
         ]
        a
    -> IO a
runEffectStack settings tvar =
    runEff
        . runErrorNoCallStackWith (error . show)
        . runChatCompletionStorageInMemory tvar
        . runErrorNoCallStackWith (error . show)
        . runChatCompletionOpenAi settings []

spec :: Spec
spec = describe "ChatCompletion OpenAI" $ do
    settings <- runIO do
        apiKey <-
            maybe
                (error "OPENAI_API_KEY not set in environment")
                (pure . OpenAiApiKey . T.pack)
                =<< lookupEnv "OPENAI_API_KEY"
        pure $ defaultOpenAiSettings apiKey
    tvar <- runIO $ newTVarIO (mempty :: Map ConversationId [ChatMsg])

    it "Respons to only SystemMsg" $ do
        conv <- runEffectStack settings tvar do
            convId <- createConversation "You are a hungry cowboy."
            respondToConversation convId
        conv `shouldSatisfy` \case
            [SystemMsg "You are a hungry cowboy." _, AssistantMsg t _] -> T.length t > 0
            _ -> False

    it "Reponds to inital UserMsg" $ do
        conv <- runEffectStack settings tvar do
            convId <- createConversation "Act exactly as a simple calculator. No extra text, just the answer."
            now <- liftIO getCurrentTime
            _ <- appendMessages convId [UserMsg "2 + 2" now]
            respondToConversation convId
        conv `shouldSatisfy` \case
            [SystemMsg {}, UserMsg {}, AssistantMsg t _] -> t == "4"
            _ -> False

    it "Tool call is correctly triggered" $ do
        False `shouldBe` True

    it "Resolves tool call" $ do
        False `shouldBe` True



