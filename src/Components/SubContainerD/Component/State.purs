module SubContainerD.Component.State where

import Data.Maybe (Maybe)

type State =
  { b :: Maybe Int
  , c :: Maybe String
  }