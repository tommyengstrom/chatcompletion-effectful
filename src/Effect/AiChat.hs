module Effect.AiChat where

import Effectful
import Effectful.TH
import Effect.AiChat.Types
import Data.Aeson (FromJSON, ToJSON)
import Relude

data AiChatError = AiChatError String
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data AiChat :: Effect where
    RespondToConversation :: ConversationId -> AiChat m [ChatMsg]

type instance DispatchOf AiChat = 'Dynamic
makeEffect ''AiChat

