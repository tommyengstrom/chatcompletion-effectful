{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module LlmChat.Storage.Postgres where

import LlmChat.Storage.Effect
import LlmChat.Types
import Control.Lens
import Data.Aeson (Value, decode, encode, toJSON)
import Data.Generics.Labels ()
import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Data.Time
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Database.PostgreSQL.Simple.FromRow qualified as PG
import Database.PostgreSQL.Simple.ToField
import Effectful
import Effectful.Time
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Relude
import UnliftIO (finally)


-- | Configuration for PostgreSQL connection pooling
data PostgresConfig = PostgresConfig
    { connectionString :: ByteString
    , poolSize :: Int
    -- ^ Number of connections in pool
    , connectionTimeout :: NominalDiffTime
    -- ^ Timeout for acquiring connection
    , connectionIdleTime :: NominalDiffTime
    -- ^ Max idle time before closing
    , poolStripes :: Int
    -- ^ Number of sub-pools
    , conversationsTable :: Text
    -- ^ Table name for conversations
    }
    deriving stock (Generic)

-- | Default configuration with reasonable defaults
defaultPostgresConfig :: ByteString -> PostgresConfig
defaultPostgresConfig connStr =
    PostgresConfig
        { connectionString = connStr
        , poolSize = 1
        , connectionTimeout = 5
        , connectionIdleTime = 60
        , poolStripes = 1
        , conversationsTable = "conversations"
        }

-- | Connection pool wrapper
data PostgresPool = PostgresPool
    { pool :: Pool Connection
    , config :: PostgresConfig
    }
    deriving stock (Generic)

-- | Legacy settings for backward compatibility
data PostgresSettings = PostgresSettings
    { getConnection :: IO Connection
    , conversationsTable :: Text
    }
    deriving stock (Generic)

instance ToField ConversationId where
    toField (ConversationId uuid) = toField uuid

instance FromField ConversationId where
    fromField f mdata = ConversationId <$> fromField f mdata

instance ToField ChatMsg where
    toField msg = toField (toJSON msg)

instance FromField ChatMsg where
    fromField f mdata = do
        (value :: Value) <- fromField f mdata
        case decode (encode value) of
            Just msg -> pure msg
            Nothing -> returnError ConversionFailed f "Could not decode ChatMsg from JSON"

data MessageRow = MessageRow
    { messageId :: Int
    , conversationId :: ConversationId
    , message :: ChatMsg
    , createdAt :: UTCTime
    }
    deriving stock (Show, Eq, Generic)

instance FromRow MessageRow where
    fromRow = MessageRow <$> PG.field <*> PG.field <*> PG.field <*> PG.field



-- | Create a new connection pool
createPostgresPool :: PostgresConfig -> IO (Pool Connection)
createPostgresPool config =
    Pool.newPool
        $ Pool.setNumStripes (Just $ config ^. #poolStripes)
        $ Pool.defaultPoolConfig
            (connectPostgreSQL (config ^. #connectionString))
            close
            (realToFrac $ config ^. #connectionTimeout)
            (config ^. #poolSize)

-- | Run an action with a connection from the pool
withPooledConnection :: Pool Connection -> (Connection -> IO a) -> IO a
withPooledConnection = Pool.withResource

runLlmChatStoragePostgres
    :: forall es a
     . ( IOE :> es
       , Error ChatStorageError :> es
       , Time :> es
       )
    => PostgresSettings
    -> Eff (LlmChatStorage ': es) a
    -> Eff es a
runLlmChatStoragePostgres settings eff = do
    interpret handleStorage eff
  where
    handleStorage :: EffectHandler LlmChatStorage es
    handleStorage _ = \case
        CreateConversation systemPrompt -> do
            conversationId <- ConversationId <$> liftIO nextRandom
            timestamp <- currentTime
            let initialMsg = SystemMsg systemPrompt timestamp
            conn <- liftIO $ settings ^. #getConnection
            liftIO $ withTransaction conn $ do
                void $ execute conn (insertMessageQuery settings) (conversationId, initialMsg)
            liftIO $ close conn
            pure conversationId
        DeleteConversation conversationId -> do
            conn <- liftIO $ settings ^. #getConnection
            liftIO $ withTransaction conn $ do
                _ <- execute conn (deleteConversationQuery settings) (Only conversationId)
                pure ()
            liftIO $ close conn
        GetConversation conversationId -> do
            conn <- liftIO $ settings ^. #getConnection
            result <- liftIO $ withTransaction conn $ do
                query conn (selectMessagesQuery settings) (Only conversationId)
            liftIO $ close conn
            case result :: [MessageRow] of
                [] -> throwError $ NoSuchConversation conversationId
                rows -> pure $ map (^. #message) rows
        AppendMessage conversationId msgIn -> liftIO do
            conn <- settings ^. #getConnection
            withTransaction conn $ do
                -- Check if conversation exists
                exists <- query conn (checkConversationExistsQuery settings) (Only conversationId)
                case exists of
                    [] -> pure () -- Conversation doesn't exist, skip
                    [Only (_ :: Int)] -> do
                        -- Insert the message
                        _ <- execute conn (insertMessageQuery settings) (conversationId, msgIn)
                        pure ()
                    _ -> pure ()
            close conn
        ListConversations -> liftIO do
            conn <- settings ^. #getConnection
            result <- withTransaction conn $ do
                query_ conn (listConversationsQuery settings)
            close conn
            pure $ map (\(Only cid) -> cid) result

-- | Run storage with connection pool
runLlmChatStoragePostgresWithPool
    :: forall es a
     . ( IOE :> es
       , Time :> es
       , Error ChatStorageError :> es
       )
    => PostgresConfig
    -> Eff (LlmChatStorage ': es) a
    -> Eff es a
runLlmChatStoragePostgresWithPool config eff = do
    pool' <- liftIO $ createPostgresPool config
    finally
        (interpret (handleStoragePooled pool') eff)
        (liftIO $ Pool.destroyAllResources pool')
  where
    handleStoragePooled :: Pool Connection -> EffectHandler LlmChatStorage es
    handleStoragePooled pool' _ = \case
        CreateConversation systemPrompt -> do
            conversationId <- ConversationId <$> liftIO nextRandom
            timestamp <- currentTime
            let initialMsg = SystemMsg systemPrompt timestamp
            liftIO $ withPooledConnection pool' $ \conn -> do
                withTransaction conn $ do
                    void
                        $ execute
                            conn
                            (insertMessageQuery (PostgresSettings (pure conn) (config ^. #conversationsTable)))
                            (conversationId, initialMsg)
            pure conversationId
        DeleteConversation conversationId -> do
            liftIO $ withPooledConnection pool' $ \conn -> do
                withTransaction conn $ do
                    _ <-
                        execute
                            conn
                            (deleteConversationQuery (PostgresSettings (pure conn) (config ^. #conversationsTable)))
                            (Only conversationId)
                    pure ()
        GetConversation conversationId -> do
            result <- liftIO $ withPooledConnection pool' $ \conn -> do
                withTransaction conn $ do
                    query
                        conn
                        (selectMessagesQuery (PostgresSettings (pure conn) (config ^. #conversationsTable)))
                        (Only conversationId)
            case result :: [MessageRow] of
                [] -> throwError $ NoSuchConversation conversationId
                rows -> pure $ rows ^.. folded . #message
        AppendMessage conversationId msgIn -> liftIO $ withPooledConnection pool' $ \conn -> do
            withTransaction conn $ do
                -- Check if conversation exists
                exists <-
                    query
                        conn
                        ( checkConversationExistsQuery
                            (PostgresSettings (pure conn) (config ^. #conversationsTable))
                        )
                        (Only conversationId)
                case exists of
                    [] -> pure () -- Conversation doesn't exist, skip
                    [Only (_ :: Int)] -> do
                        -- Insert the message
                        _ <-
                            execute
                                conn
                                (insertMessageQuery (PostgresSettings (pure conn) (config ^. #conversationsTable)))
                                (conversationId, msgIn)
                        pure ()
                    _ -> pure ()
        ListConversations -> liftIO $ withPooledConnection pool' $ \conn -> do
            result <- withTransaction conn $ do
                query_
                    conn
                    (listConversationsQuery (PostgresSettings (pure conn) (config ^. #conversationsTable)))
            pure $ map (\(Only cid) -> cid) result

-- | Backward compatible function that creates a pool internally
runLlmChatStoragePostgres'
    :: forall es a
     . ( IOE :> es
       , Error ChatStorageError :> es
       , Time :> es
       )
    => ByteString
    -> Eff (LlmChatStorage ': es) a
    -> Eff es a
runLlmChatStoragePostgres' connStr = runLlmChatStoragePostgresWithPool (defaultPostgresConfig connStr)

setupTable :: PostgresSettings -> IO ()
setupTable settings = do
    conn <- settings ^. #getConnection
    withTransaction conn $ do
        void $ execute_ conn (createTableQuery settings)
        void $ execute_ conn (createIndexQuery settings)
    close conn

-- | Setup table using connection pool
setupTableWithPool :: PostgresConfig -> IO ()
setupTableWithPool config = do
    pool' <- createPostgresPool config
    withPooledConnection pool' $ \conn -> do
        withTransaction conn $ do
            let settings = PostgresSettings (pure conn) (config ^. #conversationsTable)
            void $ execute_ conn (createTableQuery settings)
            void $ execute_ conn (createIndexQuery settings)
    Pool.destroyAllResources pool'

createTableQuery :: PostgresSettings -> Query
createTableQuery settings =
    fromString
        $ toString
        $ "CREATE TABLE IF NOT EXISTS "
        <> settings
        ^. #conversationsTable
            <> " ("
            <> "id SERIAL PRIMARY KEY, "
            <> "conversation_id UUID NOT NULL, "
            <> "message JSONB NOT NULL, "
            <> "created_at timestamp with time zone NOT NULL DEFAULT now()"
            <> ")"

insertMessageQuery :: PostgresSettings -> Query
insertMessageQuery settings =
    fromString
        $ toString
        $ "INSERT INTO "
        <> settings
        ^. #conversationsTable
            <> " (conversation_id, message) VALUES (?, ?)"

deleteConversationQuery :: PostgresSettings -> Query
deleteConversationQuery settings =
    fromString
        $ toString
        $ "DELETE FROM "
        <> settings
        ^. #conversationsTable
            <> " WHERE conversation_id = ?"

selectMessagesQuery :: PostgresSettings -> Query
selectMessagesQuery settings =
    fromString
        $ toString
        $ "SELECT id, conversation_id, message, created_at FROM "
        <> settings
        ^. #conversationsTable
            <> " WHERE conversation_id = ? ORDER BY created_at ASC, id ASC"

selectMessagesQuery' :: PostgresSettings -> Query
selectMessagesQuery' _settings =
    [sql| SELECT id, conversation_id, message, created_at FROM ?
             WHERE conversation_id = ? ORDER BY created_at ASC, id ASC
        |] -- (Only "conversations")
        -- (toString convTable, Only conversationId)
   -- where
   --     convTable = settings ^. #conversationsTable

checkConversationExistsQuery :: PostgresSettings -> Query
checkConversationExistsQuery settings =
    fromString
        $ toString
        $ "SELECT 1 FROM "
        <> settings
        ^. #conversationsTable
            <> " WHERE conversation_id = ? LIMIT 1"

listConversationsQuery :: PostgresSettings -> Query
listConversationsQuery settings =
    fromString
        $ toString
        $ "SELECT DISTINCT conversation_id FROM "
        <> settings
        ^. #conversationsTable

createIndexQuery :: PostgresSettings -> Query
createIndexQuery settings =
    fromString
        $ toString
        $ "CREATE INDEX IF NOT EXISTS idx_"
        <> settings
        ^. #conversationsTable
            <> "_conversation_id ON "
            <> settings
        ^. #conversationsTable
            <> " (conversation_id)"
