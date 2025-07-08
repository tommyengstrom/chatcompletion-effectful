{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ChatCompletion.PostgresLogger where

import ChatCompletion.Types (ConversationId (..))
import Data.Aeson (FromJSON, Result (..), ToJSON, Value, fromJSON, toJSON)
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Database.PostgreSQL.Simple.FromRow qualified as PG
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Relude

type TableName = Text

-- Newtype wrapper to avoid overlapping instances
newtype JsonField a = JsonField a
    deriving stock (Show, Generic)

instance ToJSON a => ToField (JsonField a) where
    toField (JsonField obj) = toField (toJSON obj)

instance (FromJSON a, Typeable a) => FromField (JsonField a) where
    fromField f mdata = do
        (value :: Value) <- fromField f mdata
        case fromJSON value of
            Success obj -> pure (JsonField obj)
            Error err -> returnError ConversionFailed f ("Could not decode from JSON: " <> err)

data LogEntry a = LogEntry
    { logId :: Int64
    , conversationId :: ConversationId
    , response :: JsonField a
    , loggedAt :: UTCTime
    }
    deriving stock (Show, Generic)

instance ToField ConversationId where
    toField (ConversationId uuid) = toField uuid

instance FromField ConversationId where
    fromField f mdata = ConversationId <$> fromField f mdata

instance (FromJSON a, Typeable a) => FromRow (LogEntry a) where
    fromRow = LogEntry <$> PG.field <*> PG.field <*> PG.field <*> PG.field

instance ToJSON a => ToRow (LogEntry a) where
    toRow (LogEntry logId' conversationId' response' loggedAt') = [toField logId', toField conversationId', toField response', toField loggedAt']

postgresResponseLogger
    :: ToJSON a => TableName -> IO Connection -> ConversationId -> a -> IO ()
postgresResponseLogger tableName getConnection convId response = do
    conn <- getConnection
    now <- getCurrentTime
    withTransaction conn $ do
        _ <- execute conn (insertQuery tableName) (convId, JsonField response, now)
        pure ()
    close conn

-- Helper function that creates a PostgreSQL logger function
makePostgresLogger
    :: ToJSON a => TableName -> IO Connection -> ConversationId -> a -> IO ()
makePostgresLogger = postgresResponseLogger

createTableQuery :: TableName -> Query
createTableQuery tableName =
    fromString
        $ toString
        $ "CREATE TABLE IF NOT EXISTS "
        <> tableName
        <> " ("
        <> "id BIGSERIAL PRIMARY KEY, "
        <> "conversation_id UUID NOT NULL, "
        <> "response JSONB NOT NULL, "
        <> "logged_at TIMESTAMP WITH TIME ZONE NOT NULL"
        <> ")"

insertQuery :: TableName -> Query
insertQuery tableName =
    fromString
        $ toString
        $ "INSERT INTO "
        <> tableName
        <> " (conversation_id, response, logged_at) VALUES (?, ?, ?)"

selectAllQuery :: TableName -> Query
selectAllQuery tableName =
    fromString
        $ toString
        $ "SELECT id, conversation_id, response, logged_at FROM "
        <> tableName
        <> " ORDER BY logged_at DESC"

selectByTimeRangeQuery :: TableName -> Query
selectByTimeRangeQuery tableName =
    fromString
        $ toString
        $ "SELECT id, conversation_id, response, logged_at FROM "
        <> tableName
        <> " WHERE logged_at >= ? AND logged_at <= ? ORDER BY logged_at DESC"

-- Helper functions for querying logs
getAllLogs :: (FromJSON a, Typeable a) => TableName -> IO Connection -> IO [LogEntry a]
getAllLogs tableName getConnection = do
    conn <- getConnection
    result <- query_ conn (selectAllQuery tableName)
    close conn
    pure result

getLogsByTimeRange
    :: (FromJSON a, Typeable a)
    => TableName -> IO Connection -> UTCTime -> UTCTime -> IO [LogEntry a]
getLogsByTimeRange tableName getConnection startTime endTime = do
    conn <- getConnection
    result <- query conn (selectByTimeRangeQuery tableName) (startTime, endTime)
    close conn
    pure result
