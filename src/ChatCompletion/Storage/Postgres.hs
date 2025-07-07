{-# OPTIONS_GHC -Wno-orphans #-}

module ChatCompletion.Storage.Postgres where

import Control.Lens
import Data.Aeson (Value, decode, encode, toJSON)
import Data.Generics.Labels ()
import Data.Time
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Database.PostgreSQL.Simple.FromRow qualified as PG
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import ChatCompletion.Types
import ChatCompletion.Storage.Effect
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Relude

data PostgresSettings = PostgresSettings
    { getConnection :: IO Connection
    , tableName :: Text
    }
    deriving stock (Generic)

instance ToField ConversationId where
    toField (ConversationId uuid) = toField uuid

instance FromField ConversationId where
    fromField f mdata = ConversationId <$> fromField f mdata

instance ToField [ChatMsg] where
    toField msgs = toField (toJSON msgs)

instance FromField [ChatMsg] where
    fromField f mdata = do
        (value :: Value) <- fromField f mdata
        case decode (encode value) of
            Just msgs -> pure msgs
            Nothing -> returnError ConversionFailed f "Could not decode ChatMsg list from JSON"

data ConversationRow = ConversationRow
    { conversationId :: ConversationId
    , messages :: [ChatMsg]
    }
    deriving stock (Show, Eq, Generic)

instance FromRow ConversationRow where
    fromRow = ConversationRow <$> PG.field <*> PG.field

instance ToRow ConversationRow where
    toRow (ConversationRow cid msgs) = [toField cid, toField msgs]

runChatCompletionStoragePostgres
    :: forall es a
     . ( IOE :> es
       , Error ChatCompletionStorageError :> es
       )
    => PostgresSettings
    -> Eff (ChatCompletionStorage ': es) a
    -> Eff es a
runChatCompletionStoragePostgres settings = interpret \_ -> \case
    CreateConversation systemPrompt -> do
        conversationId <- ConversationId <$> liftIO nextRandom
        timestamp <- liftIO getCurrentTime
        let initialMsg = SystemMsg systemPrompt timestamp
        conn <- liftIO $ settings ^. #getConnection
        liftIO $ withTransaction conn $ do
            _ <- execute conn (insertQuery settings) (ConversationRow conversationId [initialMsg])
            pure ()
        liftIO $ close conn
        pure conversationId
    DeleteConversation conversationId -> do
        conn <- liftIO $ settings ^. #getConnection
        liftIO $ withTransaction conn $ do
            _ <- execute conn (deleteQuery settings) (Only conversationId)
            pure ()
        liftIO $ close conn
    GetConversation conversationId -> do
        conn <- liftIO $ settings ^. #getConnection
        result <- liftIO $ withTransaction conn $ do
            query conn (selectQuery settings) (Only conversationId)
        liftIO $ close conn
        case result of
            [] -> throwError $ NoSuchConversation conversationId
            [ConversationRow _ msgs] -> pure msgs
            _ -> throwError $ NoSuchConversation conversationId
    AppendMessages conversationId chatMsgs -> do
        conn <- liftIO $ settings ^. #getConnection
        result <- liftIO $ withTransaction conn $ do
            existing <- query conn (selectQuery settings) (Only conversationId)
            case existing of
                [] -> pure [] -- Return empty list if conversation doesn't exist (matches InMemory behavior)
                [ConversationRow _ msgs] -> do
                    let newMsgs = msgs <> chatMsgs
                    _ <- execute conn (updateQuery settings) (newMsgs, conversationId)
                    pure newMsgs
                _ -> pure []
        liftIO $ close conn
        pure result
    ListConversations -> do
        conn <- liftIO $ settings ^. #getConnection
        result <- liftIO $ withTransaction conn $ do
            query_ conn (listQuery settings)
        liftIO $ close conn
        pure $ map (\(ConversationRow cid _) -> cid) result

createTableQuery :: PostgresSettings -> Query
createTableQuery settings =
    fromString
        $ toString
        $ "CREATE TABLE IF NOT EXISTS "
        <> settings
        ^. #tableName
            <> " ("
            <> "conversation_id UUID PRIMARY KEY, "
            <> "messages JSONB NOT NULL"
            <> ")"

insertQuery :: PostgresSettings -> Query
insertQuery settings =
    fromString
        $ toString
        $ "INSERT INTO "
        <> settings
        ^. #tableName <> " (conversation_id, messages) VALUES (?, ?)"

deleteQuery :: PostgresSettings -> Query
deleteQuery settings =
    fromString
        $ toString
        $ "DELETE FROM "
        <> settings
        ^. #tableName <> " WHERE conversation_id = ?"

selectQuery :: PostgresSettings -> Query
selectQuery settings =
    fromString
        $ toString
        $ "SELECT conversation_id, messages FROM "
        <> settings
        ^. #tableName <> " WHERE conversation_id = ?"

updateQuery :: PostgresSettings -> Query
updateQuery settings =
    fromString
        $ toString
        $ "UPDATE "
        <> settings
        ^. #tableName <> " SET messages = ? WHERE conversation_id = ?"

listQuery :: PostgresSettings -> Query
listQuery settings =
    fromString
        $ toString
        $ "SELECT conversation_id, messages FROM "
        <> settings
        ^. #tableName
