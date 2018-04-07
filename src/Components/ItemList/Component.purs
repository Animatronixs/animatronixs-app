module ItemList.Component where

import Prelude

import Item.Component (ItemQuery(..), ItemMessage(..), item)
import Data.Array (snoc, filter, length)
import Data.Array.ST.Iterator (next)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.HTML (a)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Item.Model (ItemId, initialItem)
import ItemList.Model (ItemList, initialItemList)

type State = Boolean

-- | The component query algebra.
data Query a
  = NewItem a
  | HandleItemMessage ItemId ItemMessage a

-- | The slot value that is filled by items during the install process.
newtype ItemSlot = ItemSlot ItemId 
derive instance eqItemSlot :: Eq ItemSlot 
derive instance ordItemSlot :: Ord ItemSlot

-- | The component definition.
component :: forall m. H.Component HH.HTML Query Unit Void m 
component =
  H.parentComponent 
    {
      initialState: const initialItemList 
    , render 
    , eval 
    , receiver: const Nothing 
    }
  where 

  render :: ItemList -> H.ParentHTML Query ItemQuery ItemSlot m 
  render st = 
    HH.div_
      [ HH.h1_ [ HH.text "Item list" ]
      , HH.p_ 
          [
            HH.button 
              [ HE.onClick (HE.input_ NewItem) ]
              [ HH.text "New Item"]
          ]
      , HH.ul_ (map renderItem st.items)    
      ]

  renderItem :: ItemId -> H.ParentHTML Query ItemQuery ItemSlot m 
  renderItem itemId = 
    HH.slot 
      (ItemSlot itemId)
      (item initialItem)
      unit
      (HE.input (HandleItemMessage itemId))

  eval :: Query ~> H.ParentDSL ItemList Query ItemQuery ItemSlot Void m 
  eval (NewItem next) = do
    H.modify addItem
    pure next
  eval (HandleItemMessage p msg next) = do
    case msg of 
      NotifyRemove -> do 
        H.modify (removeItem p)
      NotifyToggle -> do
        H.modify (toggleItem p)
    pure next

-- | Adds an item to the current state.
addItem :: ItemList -> ItemList 
addItem st = st { nextId = st.nextId + 1, items = st.items `snoc` st.nextId }

-- | Removes an item from the current state.
removeItem :: ItemId -> ItemList -> ItemList 
removeItem itemId st = st { items = filter (_ /= itemId) st.items }  

-- | Toggles an item, TO DO: currently only removes the item
toggleItem :: ItemId -> ItemList -> ItemList
toggleItem itemId st = st { items = filter (_ /= itemId) st.items }