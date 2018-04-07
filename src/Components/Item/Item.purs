module Item.Component where

import Item.Model(Item)
-- import Order.Model
import Prelude

-- import Color.Scheme.X11 (turquoise)
-- import Button.Component (Query(..))
import Item.Component (ItemMessage(..))
-- import Control.Monad.Aff (runAff)
-- import Control.Monad.Aff.Endpoint (execEndpoint)
import Control.Monad.Eff (Eff, kind Effect)
-- import Control.Monad.Eff.Class (liftEff)
-- import Control.Monad.Eff.Exception (message)
import Control.Monad.State as CMS
-- import DOM.HTML.Indexed.InputType as HP
-- import Data.Array.ST.Iterator (next)
-- import Data.Bifunctor (bimap)
import Data.Maybe (Maybe(..))
import Halogen as H
-- import Halogen.HTML (a)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
-- import Network.HTTP.Affjax (AJAX)

foreign import data DOM :: Effect
foreign import appendToBody :: forall eff. String -> Eff (dom :: DOM | eff) Unit

-- | The item component query algebra.
data ItemQuery a
  = UpdateTitle String a
  | Toggle a
  | Remove a

data ItemMessage
  = NotifyRemove
  | NotifyToggle

-- | The item component definition.
item :: forall m. Item -> H.Component HH.HTML ItemQuery Unit ItemMessage m
item initialState =
  H.component
    {
      initialState: const initialState
    , render
    , eval
    , receiver: const Nothing     
    }
  where

  render :: Item -> H.ComponentHTML ItemQuery
  render i =
    HH.li_
      [ HH.input
        [ HP.type_ HP.InputText
        , HP.placeholder "Untitled"
        , HP.autofocus true
        , HP.value i.title
        , HE.onValueChange (HE.input UpdateTitle)    
        ]
      , HH.button 
        [
          HP.title "Toggle item"
        , HE.onClick (HE.input_ Toggle)
        ] 
        [ HH.text "Toggle" ]
      , HH.button 
        [
          HP.title "Remove item"
        , HE.onClick (HE.input_ Remove)
        ] 
        [ HH.text "X" ]  
      ]

  eval :: ItemQuery ~> H.ComponentDSL Item ItemQuery ItemMessage m 
  eval (UpdateTitle title next) = do
    CMS.modify (_ { title = title })
    pure next 
  eval (Remove next) = do 
    H.raise NotifyRemove 
    pure next
  eval (Toggle next) = do 
    H.raise NotifyToggle 
    pure next