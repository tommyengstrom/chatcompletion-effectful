{-# OPTIONS_GHC -Wno-orphans #-}

module ChatCompletion.Storage.Postgres where

import ChatCompletion.Storage.Effect
import ChatCompletion.Types
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

runChatCompletionStoragePostgres
    :: forall es a
     . ( IOE :> es
       , Error ChatCompletionStorageError :> es
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
                [] -> throwError $ NoSuchConversation conversationId
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

createTableQuery :: PostgresSettings -> Query
createTableQuery settings =
    fromString $
        toString $
            "CREATE TABLE IF NOT EXISTS "
                <> settings
                    ^. #tableName
                <> " ("
                <> "id SERIAL PRIMARY KEY, "
                <> "conversation_id UUID NOT NULL, "
                <> "message JSONB NOT NULL, "
                <> "created_at timestamp with time zone NOT NULL DEFAULT now()"
                <> ")"

insertMessageQuery :: PostgresSettings -> Query
insertMessageQuery settings =
    fromString $
        toString $
            "INSERT INTO "
                <> settings
                    ^. #tableName
                <> " (conversation_id, message) VALUES (?, ?)"

deleteConversationQuery :: PostgresSettings -> Query
deleteConversationQuery settings =
    fromString $
        toString $
            "DELETE FROM "
                <> settings
                    ^. #tableName
                <> " WHERE conversation_id = ?"

selectMessagesQuery :: PostgresSettings -> Query
selectMessagesQuery settings =
    fromString $
        toString $
            "SELECT id, conversation_id, message, created_at FROM "
                <> settings
                    ^. #tableName
                <> " WHERE conversation_id = ? ORDER BY created_at ASC, id ASC"

checkConversationExistsQuery :: PostgresSettings -> Query
checkConversationExistsQuery settings =
    fromString $
        toString $
            "SELECT 1 FROM "
                <> settings
                    ^. #tableName
                <> " WHERE conversation_id = ? LIMIT 1"

listConversationsQuery :: PostgresSettings -> Query
listConversationsQuery settings =
    fromString $
        toString $
            "SELECT DISTINCT conversation_id FROM "
                <> settings
                    ^. #tableName

createIndexQuery :: PostgresSettings -> Query
createIndexQuery settings =
    fromString $
        toString $
            "CREATE INDEX IF NOT EXISTS idx_"
                <> settings
                    ^. #tableName
                <> "_conversation_id ON "
                <> settings
                    ^. #tableName
                <> " (conversation_id)"
