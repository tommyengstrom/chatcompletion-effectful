module ChatCompletion.OpenAISpec where

import ChatCompletion
import ChatCompletion.OpenAI
import ChatCompletion.Storage.InMemory
import Control.Lens (folded, (^..))
import Data.Aeson
import Data.Generics.Sum
import Data.OpenApi (ToSchema)
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import Relude
import Test.Hspec

runEffectStack
    :: OpenAiSettings
    -> TVar (Map ConversationId [ChatMsg])
    -> Eff '[ChatCompletion, Error ChatCompletionError, ChatCompletionStorage, Error ChatCompletionStorageError, IOE] a
    -> IO a
runEffectStack settings tvar =
    runEff
        . runErrorNoCallStackWith (error . show)
        . runChatCompletionStorageInMemory tvar
        . runErrorNoCallStackWith (error . show)
        . runChatCompletionOpenAi settings

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

    it "Responds to only SystemMsg" $ do
        (response, conv) <- runEffectStack settings tvar do
            convId <- createConversation "You are a hungry cowboy."
            messages <- getConversation convId
            resp <- sendMessages [] messages
            appendMessages convId [resp]
            conv <- getConversation convId
            pure (resp, conv)
        response `shouldSatisfy` \case
            AssistantMsg t _ -> T.length t > 0
            _ -> False
        conv `shouldSatisfy` (== 2) . length
    it "Reponds to inital UserMsg" $ do
        (response, conv) <- runEffectStack settings tvar do
            convId <-
                createConversation "Act exactly as a simple calculator. No extra text, just the answer."
            appendUserMessage convId "2 + 2"
            messages <- getConversation convId
            resp <- sendMessages [] messages
            appendMessages convId [resp]
            conv <- getConversation convId
            pure (resp, conv)
        response `shouldSatisfy` \case
            AssistantMsg t _ -> t == "4"
            _ -> False
        conv `shouldSatisfy` (== 3) . length

    it "Tool call is correctly triggered" $ do
        response <- runEffectStack settings tvar do
            convId <- createConversation "You are the users assistant."
            msgs <- respondWithTools [listContacts] convId "What is my friend John's last name?"
            pure msgs
        response
            `shouldSatisfy` any
                ( \case
                    ToolCallMsg{toolCalls} -> any (\ToolCall{toolName} -> toolName == "list_contact") toolCalls
                    _ -> False
                )

    it "Resolves multiple tool calls" $ do
        response <- runEffectStack settings tvar do
            convId <- createConversation "You are the users assistant."
            msgs <- respondWithTools [listContacts, showPhoneNumber] convId "What is John's phone number?"
            pure msgs
        response
            `shouldSatisfy` any
                ( \case
                    ToolCallMsg{toolCalls} -> any (\ToolCall{toolName} -> toolName == "list_contact") toolCalls
                    _ -> False
                )
        response
            `shouldSatisfy` any
                ( \case
                    ToolCallMsg{toolCalls} -> any (\ToolCall{toolName} -> toolName == "show_phone_number") toolCalls
                    _ -> False
                )
        response
            `shouldSatisfy` any
                ( \case
                    AssistantMsg{content} -> T.isInfixOf "123-456-7890" content
                    _ -> False
                )
        response `shouldSatisfy` (== 5) . length
        (response ^.. folded . _Ctor @"ToolCallMsg") `shouldSatisfy` (== 2) . length
        (response ^.. folded . _Ctor @"ToolCallResponseMsg") `shouldSatisfy` (== 2) . length
        (response ^.. folded . _Ctor @"AssistantMsg") `shouldSatisfy` (== 1) . length

listContacts :: ToolDef es
listContacts =
    defineToolNoArgument
        "list_contact"
        "List all the contacts of the user."
        ( pure
            $ Right
            $ ToolResponse
                { modelResponse = "Contacts:\n" <> T.intercalate "\n- " contacts
                , localResponse = [UIComponent $ toJSON contacts]
                }
        )
  where
    contacts :: [Text]
    contacts = ["John Snow", "Arya Stark", "Tyrion Lannister"]

data FullName = FullName
    { fullName :: Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

showPhoneNumber :: ToolDef es
showPhoneNumber =
    defineToolWithArgument
        "show_phone_number"
        "Show the phone number of a contact. Must use full name for lookup."
        ( \case
            FullName "John Snow" ->
                pure
                    $ Right
                    $ ToolResponse
                        { modelResponse = "Phone number: 123-456-7890"
                        , localResponse = [UIComponent $ toJSON ("123-456-7890" :: Text)]
                        }
            FullName n -> pure $ Left $ "No phone number for contact: " <> T.unpack n
        )
