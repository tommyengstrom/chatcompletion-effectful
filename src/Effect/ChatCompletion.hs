module Effect.ChatCompletion where

import Data.Aeson (FromJSON, ToJSON)
import Effect.ChatCompletion.Types
import Effectful
import Effectful.TH
import Relude

data ChatCompletionError = ChatCompletionError String
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data ChatCompletion :: Effect where
    RespondToConversation :: ConversationId -> ChatCompletion m [ChatMsg]

type instance DispatchOf ChatCompletion = 'Dynamic
makeEffect ''ChatCompletion
