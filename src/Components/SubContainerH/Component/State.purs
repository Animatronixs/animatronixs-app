module SubContainerH.Component.State where

import Data.Maybe (Maybe)

type State =
  { b :: Maybe Int
  , c :: Maybe String
  , loading :: Boolean
  , username :: String
  , result :: Maybe String
  }