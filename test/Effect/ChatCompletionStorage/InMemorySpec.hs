{-# OPTIONS_GHC -Wno-orphans #-}

module Effect.ChatCompletionStorage.InMemorySpec where

import Control.Lens (folded, reversed, (^..), (^?))
import Data.Generics.Product
import Data.Generics.Sum
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time
import Data.UUID
import Effect.ChatCompletion.Types
import Effect.ChatCompletionStorage
import Effect.ChatCompletionStorage.InMemory
import Effectful
import Effectful.Error.Static
import Relude
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

instance Arbitrary ConversationId where
    arbitrary =
        fmap ConversationId $
            fromWords
                <$> arbitraryBoundedIntegral
                <*> arbitraryBoundedIntegral
                <*> arbitraryBoundedIntegral
                <*> arbitraryBoundedIntegral

runEffStack
    :: Eff '[Error ChatCompletionStorageError, IOE] a
    -> IO (Either ChatCompletionStorageError a)
runEffStack = runEff . runErrorNoCallStack

data SomeText = SomeText Text
    deriving stock (Show, Generic)
instance Arbitrary SomeText where
    arbitrary = SomeText . T.pack <$> listOf (choose ('a', 'z'))
spec :: Spec
spec =
    describe "runChatCompletionStorageInMemory" $ do
        tvar <- runIO $ newTVarIO (mempty :: Map ConversationId [ChatMsg])
        specGeneralized (runChatCompletionStorageInMemory tvar)

specGeneralized
    :: (forall a. Eff (ChatCompletionStorage ': '[Error ChatCompletionStorageError, IOE] ) a
    -> Eff '[Error ChatCompletionStorageError, IOE] a)
    -> Spec
specGeneralized runStorage = do
    describe "ChatCompletionStorage" $ do
        it "Fails to fetch conversation that does not exist" $ do
            property $ \(convId :: ConversationId) -> monadicIO $ do
                result <-
                    run
                        . runEffStack
                        . runStorage
                        $ getConversation convId
                liftIO $ result `shouldBe` Left (NoSuchConversation convId)
        it "Can retreive conversation after creating it" $ do
            property $ \(SomeText systemPrompt) -> monadicIO $ do
                Right conv <- run . runEffStack $ runStorage do
                    convId <- createConversation systemPrompt
                    getConversation convId
                liftIO do
                    length conv `shouldBe` 1
                    conv ^.. folded . _Ctor @"SystemMsg" . typed @Text
                        `shouldBe` [systemPrompt]
        it "Lists more conversation after creating one" $ do
            property $ \(SomeText systemPrompt) -> monadicIO $ do
                Right (listBefore, convId, listAfter) <- run
                    . runEffStack
                    $ runStorage do
                        listBefore <- listConversations
                        convId <- createConversation systemPrompt
                        (listBefore,convId,) <$> listConversations
                liftIO $
                    Set.difference (Set.fromList listAfter) (Set.fromList listBefore)
                        `shouldBe` [convId]
        it "AppendMessage return the full conversation" $ property \(SomeText userPrompt) -> monadicIO do
            Right (beforeAppend, afterAppend) <- run $ runEffStack $ runStorage $ do
                convIds <- listConversations
                convId <- liftIO . generate $ elements convIds
                conv <- getConversation convId
                now <- liftIO getCurrentTime
                (conv,) <$> appendMessages convId [UserMsg userPrompt now]
            liftIO $ length beforeAppend + 1 `shouldBe` length afterAppend
            liftIO $
                afterAppend ^? reversed . folded . _Ctor @"UserMsg" . typed @Text
                    `shouldBe` Just userPrompt
        it "GetConversation errors if conversation does not exist" $ do
            property $ \(convId :: ConversationId) -> monadicIO $ do
                result <-
                    run
                        . runEffStack
                        . runStorage
                        $ getConversation convId
                liftIO $ result `shouldBe` Left (NoSuchConversation convId)
