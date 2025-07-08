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
    -- Send messages to the LLM and get a single response
    -- The response could be an assistant message or tool calls
    SendMessages
        :: [ToolDeclaration]            -- Available tools
        -> [ChatMsg]                    -- Messages to send
        -> ChatCompletion m ChatMsg     -- Single response message

type instance DispatchOf ChatCompletion = 'Dynamic
makeEffect ''ChatCompletion

