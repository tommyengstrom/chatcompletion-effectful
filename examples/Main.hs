module Main where

import ChatCompletion
import ChatCompletion.OpenAI
import ChatCompletion.Storage.InMemory
import Data.Aeson
import Data.OpenApi (ToSchema)
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import Relude

-- | Run the chat completion effect stack with dynamic tools
runChatApp :: IO ()
runChatApp = do
    -- Get OpenAI API key from environment
    apiKey <-
        maybe
            (error "OPENAI_API_KEY not set in environment")
            (pure . OpenAiApiKey . T.pack)
            =<< lookupEnv "OPENAI_API_KEY"

    let settings = defaultOpenAiSettings apiKey
    tvar <- newTVarIO mempty

    runEff
        $ runErrorNoCallStackWith @ChatCompletionStorageError (error . show)
        $ runChatCompletionStorageInMemory tvar
        $ runErrorNoCallStackWith @ChatCompletionError (error . show)
        $ runChatCompletionOpenAi settings
        $ do
            putStrLn "=== Dynamic Tool Example ==="
            
            -- Create a conversation
            convId <- createConversation "You are a helpful assistant with access to the user's contacts."
            
            -- Send a message with tools
            msgs <- respondWithTools myTools convId "What is John's phone number?"
            
            -- Display the conversation
            for_ msgs $ \msg -> case msg of
                UserMsg content _ -> 
                    putStrLn $ "User: " <> T.unpack content
                AssistantMsg content _ -> 
                    putStrLn $ "Assistant: " <> T.unpack content
                ToolCallMsg calls _ -> 
                    for_ calls $ \(ToolCall _ name _) ->
                        putStrLn $ "Tool Call: " <> T.unpack name
                ToolCallResponseMsg _ (ToolResponse modelResp _) _ ->
                    putStrLn $ "Tool Response: " <> T.unpack modelResp
                _ -> pure ()

-- | Define available tools
myTools :: [ToolDef es]
myTools = [listContactsTool, showPhoneNumberTool]

-- | Tool definition: List contacts
listContactsTool :: ToolDef es
listContactsTool =
    defineToolNoArgument
        "list_contact"
        "List all the contacts of the user."
        ( pure $ Right $ ToolResponse
            { modelResponse = "Contacts:\n" <> T.intercalate "\n- " contacts
            , localResponse = [UIComponent $ toJSON contacts]
            }
        )
  where
    contacts = ["John Snow", "Arya Stark", "Tyrion Lannister"]

-- | Tool definition: Show phone number
showPhoneNumberTool :: ToolDef es
showPhoneNumberTool =
    defineToolWithArgument @FullName
        "show_phone_number"
        "Show the phone number of a contact. Must use full name for lookup."
        ( \case
            FullName "John Snow" -> pure $ Right $ ToolResponse
                { modelResponse = "Phone number: 123-456-7890"
                , localResponse = [UIComponent $ toJSON ("123-456-7890" :: Text)]
                }
            FullName n -> pure $ Right $ ToolResponse
                { modelResponse = "No phone number for contact: " <> n
                , localResponse = []
                }
        )

-- | Data type for contact name parameter
data FullName = FullName
    { fullName :: Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Main function
main :: IO ()
main = do
    putStrLn "ChatCompletion Dynamic Tools Example"
    putStrLn "==================================="
    putStrLn "Make sure to set OPENAI_API_KEY environment variable"
    putStrLn ""

    runChatApp

    putStrLn "\n=== Example Complete ==="
