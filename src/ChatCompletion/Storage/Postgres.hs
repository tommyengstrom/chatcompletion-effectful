{-# OPTIONS_GHC -Wno-orphans #-}

module ChatCompletion.Storage.Postgres where

import ChatCompletion.Error (ChatCompletionError (..), StorageErrorDetails (..))
import ChatCompletion.Storage.Effect
import ChatCompletion.Types
import Control.Lens
import Data.Aeson (Value, decode, encode, toJSON)
import Data.Generics.Labels ()
import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Data.Time
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Database.PostgreSQL.Simple.FromRow qualified as PG
import Database.PostgreSQL.Simple.ToField
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Relude
import UnliftIO (finally, try)

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

instance ToField ChatMsgIn where
    toField msg = toField (toJSON msg)

instance FromField ChatMsgIn where
    fromField f mdata = do
        (value :: Value) <- fromField f mdata
        case decode (encode value) of
            Just msg -> pure msg
            Nothing -> returnError ConversionFailed f "Could not decode ChatMsgIn from JSON"

data MessageRow = MessageRow
    { messageId :: Int
    , conversationId :: ConversationId
    , message :: ChatMsgIn
    , createdAt :: UTCTime
    }
    deriving stock (Show, Eq, Generic)

instance FromRow MessageRow where
    fromRow = MessageRow <$> PG.field <*> PG.field <*> PG.field <*> PG.field

-- Helper function to convert ChatMsg to ChatMsgIn
chatMsgToIn :: ChatMsg -> ChatMsgIn
chatMsgToIn = \case
    SystemMsg content _ -> SystemMsgIn content
    UserMsg content _ -> UserMsgIn content
    AssistantMsg content _ -> AssistantMsgIn content
    ToolCallMsg toolCalls _ -> ToolCallMsgIn toolCalls
    ToolCallResponseMsg toolCallId toolResponse _ -> ToolCallResponseMsgIn toolCallId toolResponse

-- Helper function to convert ChatMsgIn to ChatMsg with timestamp
chatMsgFromIn :: ChatMsgIn -> UTCTime -> ChatMsg
chatMsgFromIn msgIn timestamp = case msgIn of
    SystemMsgIn content -> SystemMsg content timestamp
    UserMsgIn content -> UserMsg content timestamp
    AssistantMsgIn content -> AssistantMsg content timestamp
    ToolCallMsgIn toolCalls -> ToolCallMsg toolCalls timestamp
    ToolCallResponseMsgIn toolCallId toolResponse -> ToolCallResponseMsg toolCallId toolResponse timestamp

-- | Check if a connection is healthy
isHealthyConnection :: Connection -> IO Bool
isHealthyConnection conn = do
    result <- try $ query_ conn "SELECT 1" :: IO (Either SomeException [Only Int])
    pure $ case result of
        Right [Only 1] -> True
        _ -> False

-- | Create a new connection pool
createPostgresPool :: PostgresConfig -> IO (Pool Connection)
createPostgresPool config =
    Pool.newPool
        $ Pool.setNumStripes (Just $ config ^. #poolStripes)
        $ Pool.defaultPoolConfig
            createConnection
            closeConnection
            (realToFrac $ config ^. #connectionTimeout)
            (config ^. #poolSize)
  where
    createConnection = do
        conn <- connectPostgreSQL (config ^. #connectionString)
        healthy <- isHealthyConnection conn
        if healthy
            then pure conn
            else do
                close conn
                error "Failed to create healthy connection"
    closeConnection = close

-- | Run an action with a connection from the pool
withPooledConnection :: Pool Connection -> (Connection -> IO a) -> IO a
withPooledConnection = Pool.withResource

-- | Pool metrics for monitoring
data PoolMetrics = PoolMetrics
    { activeConnections :: Int
    , idleConnections :: Int
    , pendingRequests :: Int
    , totalCreated :: Int
    , totalDestroyed :: Int
    }
    deriving stock (Show, Eq, Generic)

-- | Get current pool metrics (simplified version as resource-pool doesn't expose detailed metrics)
getPoolMetrics :: PostgresPool -> IO PoolMetrics
getPoolMetrics _pgPool =
    -- The resource-pool library doesn't expose detailed metrics
    -- This is a placeholder that could be enhanced with custom tracking
    pure
        $ PoolMetrics
            { activeConnections = 0 -- Would need custom tracking
            , idleConnections = 0 -- Would need custom tracking
            , pendingRequests = 0 -- Would need custom tracking
            , totalCreated = 0 -- Would need custom tracking
            , totalDestroyed = 0 -- Would need custom tracking
            }

-- | Create a PostgresPool from config
withPostgresPool :: PostgresConfig -> (PostgresPool -> IO a) -> IO a
withPostgresPool config action = do
    pool' <- createPostgresPool config
    let pgPool = PostgresPool pool' config
    finally (action pgPool) (Pool.destroyAllResources pool')

runChatCompletionStoragePostgres
    :: forall es a
     . ( IOE :> es
       , Error ChatCompletionError :> es
       )
    => PostgresSettings
    -> Eff (ChatCompletionStorage ': es) a
    -> Eff es a
runChatCompletionStoragePostgres settings eff = do
    interpret handleStorage eff
  where
    handleStorage :: EffectHandler ChatCompletionStorage es
    handleStorage _ = \case
        CreateConversation systemPrompt -> do
            conversationId <- ConversationId <$> liftIO nextRandom
            let initialMsg = SystemMsgIn systemPrompt
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
            case result of
                [] -> throwError $ StorageError $ NoSuchConversation conversationId
                rows -> pure $ map (\(MessageRow _ _ msgIn createdAt) -> chatMsgFromIn msgIn createdAt) rows
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
runChatCompletionStoragePostgresWithPool
    :: forall es a
     . ( IOE :> es
       , Error ChatCompletionError :> es
       )
    => PostgresConfig
    -> Eff (ChatCompletionStorage ': es) a
    -> Eff es a
runChatCompletionStoragePostgresWithPool config eff = do
    pool' <- liftIO $ createPostgresPool config
    finally
        (interpret (handleStoragePooled pool') eff)
        (liftIO $ Pool.destroyAllResources pool')
  where
    handleStoragePooled :: Pool Connection -> EffectHandler ChatCompletionStorage es
    handleStoragePooled pool' _ = \case
        CreateConversation systemPrompt -> do
            conversationId <- ConversationId <$> liftIO nextRandom
            let initialMsg = SystemMsgIn systemPrompt
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
            case result of
                [] -> throwError $ StorageError $ NoSuchConversation conversationId
                rows -> pure $ map (\(MessageRow _ _ msgIn createdAt) -> chatMsgFromIn msgIn createdAt) rows
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
runChatCompletionStoragePostgres'
    :: forall es a
     . ( IOE :> es
       , Error ChatCompletionError :> es
       )
    => ByteString
    -> Eff (ChatCompletionStorage ': es) a
    -> Eff es a
runChatCompletionStoragePostgres' connStr = runChatCompletionStoragePostgresWithPool (defaultPostgresConfig connStr)

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
