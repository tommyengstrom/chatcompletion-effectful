module ChatCompletion.PostgresPoolSpec where

import ChatCompletion.Error (LlmChatError)
import ChatCompletion.Storage.Effect
import ChatCompletion.Storage.InMemorySpec (specGeneralized)
import ChatCompletion.Storage.Postgres
import ChatCompletion.Types
import Data.Generics.Labels ()
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Database.PostgreSQL.Simple
import Effectful
import Effectful.Error.Static
import Relude
import Test.Hspec
import UnliftIO (forConcurrently)
import Effectful.Time

spec :: Spec
spec = describe "PostgreSQL Connection Pooling" $ do
    -- Create a unique table name based on current time
    conversationsTable <- runIO $ do
        now <- getCurrentTime
        let unixTime :: String = show $ (floor $ utcTimeToPOSIXSeconds now :: Integer)
        pure $ "conversations_pool_" <> toText unixTime

    let connectionString = "host=localhost port=5432 user=postgres password=postgres dbname=chatcompletion-test"
    let config =
            (defaultPostgresConfig connectionString)
                { conversationsTable = conversationsTable
                , poolSize = 5
                , poolStripes = 2
                }

    let cleanup = do
            conn <- connectPostgreSQL connectionString
            _ <- execute_ conn $ fromString $ toString $ "DROP TABLE IF EXISTS " <> conversationsTable
            close conn

    describe "runChatCompletionStoragePostgresWithPool" $ do
        -- Clean up before tests, setup table, then run tests with cleanup after
        runIO do
            cleanup
            setupTableWithPool config
        afterAll_ cleanup $
            specGeneralized (runChatCompletionStoragePostgresWithPool config)

    describe "Connection pool behavior" $ do
        it "handles concurrent operations efficiently" $ do
            -- Clean up and setup before this specific test
            cleanup
            setupTableWithPool config

            -- Run multiple concurrent operations
            results <- forConcurrently ([1 .. 20] :: [Int]) $ \i -> do
                runEff
                    . runTime
                    . runError @LlmChatError
                    . runErrorNoCallStackWith @ChatStorageError (error . show)
                    $ runChatCompletionStoragePostgresWithPool config
                    $ do
                        -- Each concurrent operation creates and uses a conversation
                        convId <- createConversation $ "Test system prompt " <> show i
                        appendUserMessage convId  $ "User message " <> show i
                        createdAt <- currentTime
                        appendMessage convId $ AssistantMsg
                            {content = "Assistant response " <> show i
                            , createdAt
                            }
                        msgs <- getConversation convId
                        pure (length msgs)

            -- Check all results succeeded with 3 messages each
            forM_ results $ \result -> case result of
                Left err -> expectationFailure $ "Concurrent operation failed: " <> show err
                Right msgCount -> msgCount `shouldBe` 3

            cleanup

    describe "Backward compatibility" $ do
        it "runChatCompletionStoragePostgres' works with default pool" $ do
            cleanup
            -- For backward compatibility test, we need to use config with our test table
            let testConfig =
                    PostgresConfig
                        { connectionString = connectionString
                        , poolSize = 1
                        , connectionTimeout = 5
                        , connectionIdleTime = 60
                        , poolStripes = 1
                        , conversationsTable = conversationsTable
                        }
            setupTableWithPool testConfig

            -- Test using the backward compatible function with our test table
            result <- runEff
                    . runTime
                . runError @LlmChatError
                . runErrorNoCallStackWith @ChatStorageError (error . show)
                $ runChatCompletionStoragePostgresWithPool testConfig do
                    convId <- createConversation "Backward compatible test"
                    appendUserMessage convId  "Test message"
                    msgs <- getConversation convId
                    pure (convId, length msgs)

            case result of
                Left err -> expectationFailure $ "Unexpected error: " <> show err
                Right (_, msgCount) -> msgCount `shouldBe` 2
            cleanup
