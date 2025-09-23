{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ChatCompletion.Error where

import Data.Generics.Labels ()
import Servant.Client (ClientError)
import Relude

data ChatExpectationError
    = ChatExpectationError String
    deriving stock (Show, Eq, Generic)


newtype LlmRequestError = LlmRequestError ClientError
    deriving stock (Show, Generic)
    deriving newtype (Eq)
