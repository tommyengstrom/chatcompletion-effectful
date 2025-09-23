module ChatCompletion.Effect
    ( module ChatCompletion.Effect
    , module ChatCompletion.Error
    ) where

import ChatCompletion.Error
import ChatCompletion.Types
import Effectful
import Effectful.TH
import Data.Aeson (Value)
import GHC.Generics (Generic)

data LlmChat :: Effect where
    -- Send messages to the LLM and get a single response
    -- The response could be an assistant message or tool calls
    GetLlmResponse
        :: [ToolDeclaration]
        -- ^ Available tools for this request
        -> ResponseFormat
        -> ConversationId
        -> LlmChat m ChatMsg
        -- ^ Single response message, can be tool call that requires resolving.

makeEffect ''LlmChat

data NativeMsgFormat
    = NativeMsgOut Value
    | NativeMsgIn Value
    deriving stock (Generic)
