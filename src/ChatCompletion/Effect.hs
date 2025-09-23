module ChatCompletion.Effect
    ( module ChatCompletion.Effect
    , module ChatCompletion.Error
    ) where

import ChatCompletion.Error
import ChatCompletion.Types
import Effectful
import Effectful.TH

data LlmChat :: Effect where
    -- Send messages to the LLM and get a single response
    -- The response could be an assistant message or tool calls
    GetLlmResponse
        :: [ToolDeclaration] -- ^ Available tools for this request
        -> ResponseFormat
        -> [ChatMsg] -- ^ Messages to send
        -> LlmChat m ChatMsg -- ^ Single response message, can be tool call that requires resolving.

makeEffect ''LlmChat



