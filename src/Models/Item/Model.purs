module Item.Model where

import Prelude

type ItemId = Int

type Item =
  {
    title :: String
  }

initialItem :: Item
initialItem =
  {
    title: ""
  }
