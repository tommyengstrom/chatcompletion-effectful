module ChatCompletion.OpenAISpec where

import Data.Aeson
import Data.OpenApi (ToSchema)
import Data.Text qualified as T
import ChatCompletion
import ChatCompletion.OpenAI
import ChatCompletion.Storage.InMemory
import Effectful
import Effectful.Error.Static
import Relude
import Test.Hspec

runEffectStack
    :: es
        ~ '[ Error ChatCompletionError
           , ChatCompletionStorage
           , Error ChatCompletionStorageError
           , IOE
           ]
    => OpenAiSettings
    -> TVar (Map ConversationId [ChatMsg])
    -> [ToolDef es]
    -> Eff (ChatCompletion ': es) a
    -> IO a
runEffectStack settings tvar tools =
    runEff
        . runErrorNoCallStackWith (error . show)
        . runChatCompletionStorageInMemory tvar
        . runErrorNoCallStackWith (error . show)
        . runChatCompletionOpenAi settings tools

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
        conv <- runEffectStack settings tvar [] do
            convId <- createConversation "You are a hungry cowboy."
            respondToConversation convId
        conv `shouldSatisfy` \case
            [SystemMsg "You are a hungry cowboy." _, AssistantMsg t _] -> T.length t > 0
            _ -> False

    it "Reponds to inital UserMsg" $ do
        conv <- runEffectStack settings tvar [] do
            convId <-
                createConversation "Act exactly as a simple calculator. No extra text, just the answer."
            _ <- appendUserMessage convId "2 + 2"
            respondToConversation convId
        conv `shouldSatisfy` \case
            [SystemMsg{}, UserMsg{}, AssistantMsg t _] -> t == "4"
            _ -> False

    it "Tool call is correctly triggered" $ do
        conv <- runEffectStack settings tvar [listContacts] do
            convId <- createConversation "You are the users assistant."
            _ <- appendUserMessage convId "What is my friend John's last name?"
            respondToConversation convId
        conv
            `shouldSatisfy` any
                ( \case
                    ToolCallMsg{toolCalls} -> any (\ToolCall{toolName} -> toolName == "list_contact") toolCalls
                    _ -> False
                )

    it "Resolves multiple tool calls" $ do
        conv <- runEffectStack settings tvar [listContacts, showPhoneNumber] do
            convId <- createConversation "You are the users assistant."
            _ <- appendUserMessage convId "What is John's phone number?"
            respondToConversation convId
        conv
            `shouldSatisfy` any
                ( \case
                    ToolCallMsg{toolCalls} -> any (\ToolCall{toolName} -> toolName == "list_contact") toolCalls
                    _ -> False
                )
        conv
            `shouldSatisfy` any
                ( \case
                    ToolCallMsg{toolCalls} -> any (\ToolCall{toolName} -> toolName == "show_phone_number") toolCalls
                    _ -> False
                )
        conv
            `shouldSatisfy` any
                ( \case
                    AssistantMsg{content} -> T.isInfixOf "123-456-7890" content
                    _ -> False
                )

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
