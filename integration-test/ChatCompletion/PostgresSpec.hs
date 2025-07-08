module ChatCompletion.PostgresSpec where

import ChatCompletion.Storage.InMemorySpec (specGeneralized)
import ChatCompletion.Storage.Postgres
import Control.Lens
import Data.Generics.Labels ()
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Database.PostgreSQL.Simple
import Relude
import Test.Hspec

spec :: Spec
spec = describe "runChatCompletionStoragePostgres" $ do
    -- Create a unique table name based on current time
    tableName <- runIO $ do
        now <- getCurrentTime
        let unixTime :: String = show $ (floor $ utcTimeToPOSIXSeconds now :: Integer)
        pure $ "conversations_" <> toText unixTime

    let connectionString = "host=localhost port=5432 user=postgres password=postgres dbname=chatcompletion-test"
    let settings =
            PostgresSettings
                { getConnection = connectPostgreSQL connectionString
                , tableName = tableName
                }

    -- Setup and cleanup functions
    let setupTable = do
            conn <- settings ^. #getConnection
            _ <- execute_ conn $ createTableQuery settings
            _ <- execute_ conn $ createIndexQuery settings
            close conn

    let cleanup = do
            conn <- settings ^. #getConnection
            _ <- execute_ conn $ fromString $ toString $ "DROP TABLE IF EXISTS " <> tableName
            close conn

    -- Clean up before tests, setup table, then run tests with cleanup after
    runIO do
        cleanup
        setupTable
    afterAll_ cleanup
        $ specGeneralized (runChatCompletionStoragePostgres settings)
