{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ChatCompletion.Error where

import Data.Generics.Labels ()
import Relude
import Servant.Client ( ClientError (..))

data ChatCompletionError
    = ChatRequestError ClientError
    | ChatDecodeError String
    | ChatExpectationError String
    deriving stock (Show, Eq, Generic)
