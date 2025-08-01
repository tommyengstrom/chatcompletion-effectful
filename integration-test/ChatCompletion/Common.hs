module ChatCompletion.Common where

import ChatCompletion
import Control.Lens (folded, (^..))
import Data.Aeson
import Data.Generics.Sum
import Data.OpenApi (ToSchema)
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import Relude
import Test.Hspec

-- | Common spec that tests basic ChatCompletion functionality
-- The runner function should handle setting up the specific provider
specWithProvider
    :: ( forall a
          . TVar (Map ConversationId [ChatMsg])
          -> Eff '[ChatCompletion, Error ChatCompletionError, ChatCompletionStorage, Error ChatCompletionStorageError, IOE] a
          -> IO a
       )
    -> Spec
specWithProvider runProvider = do
    tvar <- runIO $ newTVarIO (mempty :: Map ConversationId [ChatMsg])

    -- Removed system-only message test as Google requires at least one user message

    it "Responds to initial UserMsg" $ do
        (response, conv) <- runProvider tvar $ do
            convId <-
                createConversation "Act exactly as a simple calculator. No extra text, just the answer."
            appendUserMessage convId "2 + 2"
            messages <- getConversation convId
            resp <- sendMessages [] messages
            appendMessage convId (chatMsgToIn resp)
            conv <- getConversation convId
            pure (resp, conv)
        response `shouldSatisfy` \case
            AssistantMsg t _ -> T.elem '4' t  -- More lenient check for different providers
            _ -> False
        conv `shouldSatisfy` (== 3) . length

    it "Tool call is correctly triggered" $ do
        response <- runProvider tvar $ do
            convId <- createConversation "You are the users assistant. When asked about contacts or phone numbers, use the available tools to find the information."
            msgs <- respondWithTools [listContacts] convId "What is my friend John's last name?"
            pure msgs
        response
            `shouldSatisfy` any
                ( \case
                    ToolCallMsg{toolCalls} -> any (\ToolCall{toolName} -> toolName == "list_contact") toolCalls
                    _ -> False
                )

    it "Resolves multiple tool calls" $ do
        response <- runProvider tvar $ do
            convId <- createConversation "You are the users assistant. When asked about contacts or phone numbers, use the available tools to find the information."
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
        response `shouldSatisfy` (>= 3) . length  -- At least 3 messages
        (response ^.. folded . _Ctor @"ToolCallMsg") `shouldSatisfy` (>= 1) . length
        (response ^.. folded . _Ctor @"ToolCallResponseMsg") `shouldSatisfy` (>= 1) . length
        (response ^.. folded . _Ctor @"AssistantMsg") `shouldSatisfy` (>= 1) . length

-- Helper tools for testing
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
        "Show the phone number of a contact. Must use full name for lookup, as given by `list_contact`."
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

-- Helper function to convert ChatMsg to ChatMsgIn
chatMsgToIn :: ChatMsg -> ChatMsgIn
chatMsgToIn = \case
    SystemMsg content _ -> SystemMsgIn content
    UserMsg content _ -> UserMsgIn content
    AssistantMsg content _ -> AssistantMsgIn content
    ToolCallMsg toolCalls _ -> ToolCallMsgIn toolCalls
    ToolCallResponseMsg toolCallId toolResponse _ -> ToolCallResponseMsgIn toolCallId toolResponse