{-# LANGUAGE RecordWildCards #-}

module ChatCompletion.Benchmark.Common where

import ChatCompletion
import Data.Aeson
import Data.OpenApi (ToSchema)
import Data.Text qualified as T
import Data.Time.Clock
import Relude
import Text.Printf

-- Benchmark result type
data BenchmarkResult = BenchmarkResult
    { modelName :: Text
    , simpleMessageTime :: NominalDiffTime
    , toolCallTime :: NominalDiffTime
    , sequentialToolsTime :: NominalDiffTime
    }
    deriving stock (Show)

-- Format benchmark results for display
formatBenchmarkResult :: BenchmarkResult -> Text
formatBenchmarkResult BenchmarkResult{..} = 
    if simpleMessageTime == 0 && toolCallTime == 0 && sequentialToolsTime == 0
    then ""  -- Don't print summary for failed benchmarks
    else T.unlines
        [ modelName <> ":"
        , " - Responds to simple message: " <> formatTime simpleMessageTime
        , " - Message with tool call: " <> formatTime toolCallTime
        , " - Sequential tool calls: " <> formatTime sequentialToolsTime
        ]
  where
    formatTime :: NominalDiffTime -> Text
    formatTime t = T.pack $ printf "%.1fs" (realToFrac t :: Double)

-- Time a computation
timeAction :: IO a -> IO (NominalDiffTime, a)
timeAction action = do
    start <- getCurrentTime
    result <- action
    end <- getCurrentTime
    pure (diffUTCTime end start, result)

-- Benchmark scenarios (this function is not used, actual benchmarks are in provider modules)

-- Helper to convert ChatMsg to ChatMsgIn
chatMsgToIn :: ChatMsg -> ChatMsgIn
chatMsgToIn = \case
    SystemMsg content _ -> SystemMsgIn content
    UserMsg content _ -> UserMsgIn content
    AssistantMsg content _ -> AssistantMsgIn content
    ToolCallMsg toolCalls _ -> ToolCallMsgIn toolCalls
    ToolCallResponseMsg toolCallId toolResponse _ -> ToolCallResponseMsgIn toolCallId toolResponse

-- Test tools (copied from integration tests)
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