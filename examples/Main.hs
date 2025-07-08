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

-- | Run the chat completion effect stack
runChatApp
    :: es
        ~ '[ Error ChatCompletionError
           , ChatCompletionStorage
           , Error ChatCompletionStorageError
           , IOE
           ]
    => Eff (ChatCompletion ': es) a
    -> IO a
runChatApp action = do
    -- Get OpenAI API key from environment
    apiKey <-
        maybe
            (error "OPENAI_API_KEY not set in environment")
            (pure . OpenAiApiKey . T.pack)
            =<< lookupEnv "OPENAI_API_KEY"

    let settings = defaultOpenAiSettings apiKey
    tvar <- newTVarIO mempty

    runEff
        $ runErrorNoCallStackWith (error . show)
        $ runChatCompletionStorageInMemory tvar
        $ runErrorNoCallStackWith (error . show)
        $ runChatCompletionOpenAi settings action

-- | Example 1: Simple conversation
simpleConversation :: IO ()
simpleConversation = runChatApp $ do
    putStrLn "=== Simple Conversation ==="

    -- Create a conversation with a system prompt
    convId <- createConversation "You are a helpful assistant that provides concise answers."

    -- Add a user message
    appendUserMessage convId "What's the weather like today?"

    -- Get response
    response <- respondToConversation [] convId

    case response of
        [AssistantMsg content _] -> do
            putStrLn $ "User: What's the weather like today?"
            putStrLn $ "Assistant: " <> T.unpack content
        _ -> putStrLn "Unexpected response format"

-- | Example 2: Tool calling with contacts
toolCallingExample :: IO ()
toolCallingExample = runChatApp $ do
    putStrLn "\n=== Tool Calling Example ==="

    -- Create a conversation with tools
    convId <-
        createConversation "You are a helpful assistant with access to the user's contacts."

    -- Ask for contact information
    appendUserMessage convId "What is John's phone number?"
    response <- respondToConversation [listContacts, showPhoneNumber] convId

    putStrLn $ "User: What is John's phone number?"
    putStrLn $ "Response has " <> show (length response) <> " messages:"

    for_ response $ \msg -> case msg of
        AssistantMsg content _ -> putStrLn $ "  Assistant: " <> T.unpack content
        ToolCallMsg calls _ -> for_ calls $ \(ToolCall _ name _) ->
            putStrLn $ "  Tool Call: " <> T.unpack name
        ToolCallResponseMsg _ (ToolResponse modelResp _) _ ->
            putStrLn $ "  Tool Response: " <> T.unpack modelResp
        _ -> putStrLn $ "  Other: " <> show msg

-- | Tool definition: List contacts
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
    contacts = ["John Snow", "Arya Stark", "Tyrion Lannister"]

-- | Data type for contact name parameter
data FullName = FullName
    { fullName :: Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Tool definition: Show phone number
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

-- | Main function to run all examples
main :: IO ()
main = do
    putStrLn "ChatCompletion Examples"
    putStrLn "====================="
    putStrLn "Make sure to set OPENAI_API_KEY environment variable"
    putStrLn ""

    simpleConversation
    toolCallingExample

    putStrLn "\n=== Examples Complete ==="
