module ChatCompletion.PostgresLoggerSpec where

import ChatCompletion.PostgresLogger
    ( JsonField (..)
    , createTableQuery
    , getAllLogs
    , postgresResponseLogger
    )
import ChatCompletion.Types (ConversationId (..))
import Control.Lens
import Data.Aeson (Result (..), Value, fromJSON, object, toJSON)
import Data.Aeson qualified as Aeson
import Data.Generics.Labels ()
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple
import OpenAI.V1.Chat.Completions (ChatCompletionObject)
import Relude
import Test.Hspec

-- Create a simple test value that can be converted to ChatCompletionObject
testChatCompletion :: ChatCompletionObject
testChatCompletion =
    case fromJSON testJson of
        Success obj -> obj
        Error err -> error $ toText $ "Failed to create test ChatCompletionObject: " <> err
  where
    testJson =
        object
            [ "id" Aeson..= ("test-id" :: Text)
            , "object" Aeson..= ("chat.completion" :: Text)
            , "created" Aeson..= (1234567890 :: Int)
            , "model" Aeson..= ("gpt-4" :: Text)
            , "choices" Aeson..= ([] :: [Value])
            , "usage"
                Aeson..= object
                    [ "prompt_tokens" Aeson..= (10 :: Int)
                    , "completion_tokens" Aeson..= (20 :: Int)
                    , "total_tokens" Aeson..= (30 :: Int)
                    ]
            ]

spec :: Spec
spec = describe "PostgresLogger" $ do
    let connectionString = "host=localhost port=5432 user=postgres password=postgres dbname=chatcompletion-test"
    let getConnection = connectPostgreSQL connectionString

    describe "logs ChatCompletionObject to PostgreSQL as JSONB" $ do
        -- Create a unique table name for this test
        tableName <- runIO $ do
            now <- getCurrentTime
            let unixTime :: String = show $ (floor $ utcTimeToPOSIXSeconds now :: Integer)
            pure $ "openai_logs_" <> toText unixTime <> "_test1"

        -- Setup and cleanup functions
        let setupTable = do
                conn <- getConnection
                _ <- execute_ conn $ createTableQuery tableName
                close conn

        let cleanup = do
                conn <- getConnection
                _ <- execute_ conn $ fromString $ toString $ "DROP TABLE IF EXISTS " <> tableName
                close conn

        -- Clean up before tests, setup table, then run tests with cleanup after
        runIO do
            cleanup
            setupTable
        afterAll_ cleanup $ do
            it "works correctly" $ do
                -- Create a test conversation ID
                testConvId <- ConversationId <$> nextRandom
                -- Log the response
                postgresResponseLogger tableName getConnection testConvId testChatCompletion

                -- Verify it was logged
                logs <- getAllLogs @ChatCompletionObject tableName getConnection
                length logs `shouldBe` 1

                case viaNonEmpty head logs of
                    Nothing -> expectationFailure "Expected at least one log entry"
                    Just logEntry -> do
                        -- Verify the JSON was stored correctly
                        case logEntry ^. #response of
                            JsonField actualResponse -> toJSON actualResponse `shouldBe` toJSON testChatCompletion
                        -- Verify the conversation ID was stored correctly
                        (logEntry ^. #conversationId) `shouldBe` testConvId
                        -- Verify timestamp is recent
                        now <- getCurrentTime
                        let logTime = logEntry ^. #loggedAt
                        diffUTCTime now logTime `shouldSatisfy` (< 10) -- Within 10 seconds
    describe "can log multiple entries" $ do
        -- Create a unique table name for this test
        tableName <- runIO $ do
            now <- getCurrentTime
            let unixTime :: String = show $ (floor $ utcTimeToPOSIXSeconds now :: Integer)
            pure $ "openai_logs_" <> toText unixTime <> "_test2"

        -- Setup and cleanup functions
        let setupTable = do
                conn <- getConnection
                _ <- execute_ conn $ createTableQuery tableName
                close conn

        let cleanup = do
                conn <- getConnection
                _ <- execute_ conn $ fromString $ toString $ "DROP TABLE IF EXISTS " <> tableName
                close conn

        -- Clean up before tests, setup table, then run tests with cleanup after
        runIO do
            cleanup
            setupTable
        afterAll_ cleanup $ do
            it "works correctly" $ do
                -- Create test conversation IDs
                testConvId1 <- ConversationId <$> nextRandom
                testConvId2 <- ConversationId <$> nextRandom
                -- Log twice
                postgresResponseLogger tableName getConnection testConvId1 testChatCompletion
                postgresResponseLogger tableName getConnection testConvId2 testChatCompletion

                logs <- getAllLogs @ChatCompletionObject tableName getConnection
                length logs `shouldBe` 2
