{-# OPTIONS_GHC -Wno-orphans #-}

module ChatCompletion.Storage.InMemorySpec where

import ChatCompletion.Storage.Effect
import ChatCompletion.Storage.InMemory
import ChatCompletion.Types
import Control.Lens (folded, reversed, taking, (^..))
import Data.Generics.Product
import Data.Generics.Sum
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.UUID
import Effectful
import Effectful.Error.Static
import Effectful.Time
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
    :: Eff
        '[ Error ChatStorageError
         , Time
         , IOE
         ]
        a
    -> IO (Either ChatStorageError a)
runEffStack =
    runEff
        . runTime
        . runErrorNoCallStack

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
    :: ( forall a es
          . ( Error ChatStorageError :> es
            , Time :> es
            , IOE :> es
            )
         => Eff (ChatCompletionStorage ': es) a
         -> Eff es a
       )
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
        it "AppendMessage adds messages to the end of the conversation" $
            property \(SomeText userPrompt1) (SomeText userPrompt2) -> monadicIO do
                Right (beforeAppend, afterAppend) <- run $ runEffStack $ runStorage $ do
                    convIds <- listConversations
                    convId <- liftIO . generate $ elements convIds
                    conv <- getConversation convId
                    appendUserMessage convId userPrompt1
                    appendUserMessage convId userPrompt2
                    (conv,) <$> getConversation convId
                liftIO $ length beforeAppend + 2 `shouldBe` length afterAppend
                liftIO $
                    afterAppend
                        ^.. reversed . taking 2 folded . _Ctor @"UserMsg" . typed @Text
                        `shouldBe` [userPrompt2, userPrompt1]
        it "GetConversation errors if conversation does not exist" $ do
            property $ \(convId :: ConversationId) -> monadicIO $ do
                result <-
                    run
                        . runEffStack
                        . runStorage
                        $ getConversation convId
                liftIO $ result `shouldBe` Left (NoSuchConversation convId)
