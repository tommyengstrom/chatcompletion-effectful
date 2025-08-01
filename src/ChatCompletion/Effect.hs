module ChatCompletion.Effect 
    ( module ChatCompletion.Effect
    , module ChatCompletion.Error
    ) where

import ChatCompletion.Error
import ChatCompletion.Types
import Effectful
import Effectful.TH

data ChatCompletion :: Effect where
    -- Send messages to the LLM and get a single response
    -- The response could be an assistant message or tool calls
    SendMessages
        :: [ToolDeclaration]            -- Available tools
        -> [ChatMsg]                    -- Messages to send
        -> ChatCompletion m ChatMsg     -- Single response message

type instance DispatchOf ChatCompletion = 'Dynamic
makeEffect ''ChatCompletion

