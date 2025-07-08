module ChatCompletion.Effect where

import ChatCompletion.Types
import Data.Aeson (FromJSON, ToJSON)
import Effectful
import Effectful.TH
import Relude

data ChatCompletionError = ChatCompletionError String
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data ChatCompletion :: Effect where
    RespondToConversation
        :: (forall es. [ToolDef es])
        -> ConversationId
        -> ChatCompletion m [ChatMsg]

type instance DispatchOf ChatCompletion = 'Dynamic
makeEffect ''ChatCompletion
