module ChatCompletion.PostgresSpec where

import ChatCompletion.Storage.InMemorySpec (specGeneralized)
import ChatCompletion.Storage.Postgres
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Database.PostgreSQL.Simple
import Relude
import Test.Hspec

spec :: Spec
spec = describe "runChatCompletionStoragePostgres (with pooling)" $ do
    -- Create a unique table name based on current time
    conversationsTable <- runIO $ do
        now <- getCurrentTime
        let unixTime :: String = show $ (floor $ utcTimeToPOSIXSeconds now :: Integer)
        pure $ "conversations_" <> toText unixTime

    let connectionString = "host=localhost port=5432 user=postgres password=postgres dbname=chatcompletion-test"
    let config = PostgresConfig
            { connectionString = connectionString
            , poolSize = 1
            , connectionTimeout = 5
            , connectionIdleTime = 60
            , poolStripes = 1
            , conversationsTable = conversationsTable
            }

    let cleanup = do
            conn <- connectPostgreSQL connectionString
            _ <- execute_ conn $ fromString $ toString $ "DROP TABLE IF EXISTS " <> conversationsTable
            close conn

    -- Clean up before tests, setup table, then run tests with cleanup after
    runIO do
        cleanup
        setupTableWithPool config
    afterAll_ cleanup
        $ specGeneralized (runChatCompletionStoragePostgresWithPool config)
