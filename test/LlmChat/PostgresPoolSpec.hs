module LlmChat.PostgresPoolSpec where

import LlmChat.Error (LlmChatError)
import LlmChat.PostgresSpec (postgresStorageBehaviourSpec)
import LlmChat.Storage.Effect
import LlmChat.Storage.Postgres
import LlmChat.Types
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Database.PostgreSQL.Simple
import Effectful
import Effectful.Error.Static
import Effectful.PostgreSQL (WithConnection)
import Effectful.PostgreSQL.Connection.Pool (runWithConnectionPool)
import Effectful.Time
import Relude
import Test.Hspec
import UnliftIO (forConcurrently)
import UnliftIO.Pool (destroyAllResources, mkDefaultPoolConfig, newPool, setNumStripes)

spec :: Spec
spec = describe "PostgreSQL Connection Pooling" $ do
    conversationsTable <- runIO newConversationsTable
    let connectionString = "host=localhost port=5432 user=postgres password=postgres dbname=chatcompletion-test"

    let cleanup = do
            conn <- connectPostgreSQL connectionString
            dropTableIfExists conn conversationsTable
            close conn

    describe "runLlmChatStoragePostgres with a pool" $ do
        pool <- runIO $ makePool connectionString 2 5

        runIO do
            cleanup
            runEff
                . runWithConnectionPool pool
                $ setupTable conversationsTable

        afterAll_ (cleanup *> destroyAllResources pool) do
            postgresStorageBehaviourSpec pool conversationsTable

    describe "Connection pool behavior" $ do
        it "handles concurrent operations efficiently" $ do
            pool <- makePool connectionString 2 5
            cleanup
            runEff
                . runWithConnectionPool pool
                $ setupTable conversationsTable

            let runStack :: Eff '[LlmChatStorage, WithConnection, Error ChatStorageError, Error LlmChatError, Time, IOE] Int -> IO (Either LlmChatError (Either ChatStorageError Int))
                runStack =
                    runEff
                        . runTime
                        . runErrorNoCallStack @LlmChatError
                        . runErrorNoCallStack @ChatStorageError
                        . runWithConnectionPool pool
                        . runLlmChatStoragePostgres conversationsTable

            results <- forConcurrently ([1 .. 20] :: [Int]) $ \i ->
                runStack do
                    convId <- createConversation $ "Test system prompt " <> show i
                    appendUserMessage convId $ "User message " <> show i
                    appendMessage convId $ AssistantMsg
                        { content = "Assistant response " <> show i
                        , toolCalls = []
                        }
                    msgs <- getConversation convId
                    pure (length msgs)

            forM_ results $ \case
                Left err -> expectationFailure $ "Concurrent operation failed: " <> show err
                Right (Left err) -> expectationFailure $ "Concurrent operation failed: " <> show err
                Right (Right msgCount) -> msgCount `shouldBe` 3

            destroyAllResources pool
            cleanup

  where
    newConversationsTable = do
        now <- getCurrentTime
        let unixTime :: String = show $ (floor $ utcTimeToPOSIXSeconds now :: Integer)
        pure $ "conversations_pool_" <> toText unixTime
    makePool connStr stripes maxOpen = do
        config <- mkDefaultPoolConfig (connectPostgreSQL connStr) close 60 maxOpen
        newPool $ setNumStripes (Just stripes) config
    dropTableIfExists conn tableName = do
        [Only exists] <-
            query
                conn
                "SELECT EXISTS (SELECT 1 FROM information_schema.tables WHERE table_schema = current_schema() AND table_name = ?)"
                (Only tableName)
        when exists do
            void $ execute_ conn $ fromString $ toString $ "DROP TABLE " <> tableName
