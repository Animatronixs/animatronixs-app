module ItemList.Model where

import Item.Model (ItemId)
  
type ItemList = 
  {
    items :: Array ItemId
  , nextId :: ItemId   
  }

initialItemList :: ItemList
initialItemList = 
  {
    items: []
  , nextId: 1  
  }