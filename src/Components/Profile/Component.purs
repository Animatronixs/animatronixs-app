module Profile.Component where

import Prelude

import CSS (color, red)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
--import Profile.Component.Query (Query(..))
import Profile.Component.State (State())

--import Prelude (class Eq, class Ord, type (~>), Unit, Void, const, pure, unit)

--type State = Unit

data Slot = Slot

derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

data Query a
  = Noop a

ui :: forall m. H.Component HH.HTML Query Unit Void m
ui = H.component
  { initialState: const unit
  , render
  , eval
  , receiver: const Nothing
  }
  where
    render _ =
      HH.div_
        [ HH.h1_ [ HH.text "Your Profile"]
        , HH.p_ [ HH.text "what a nice profile!" ]
        ]

    eval :: Query ~> H.ComponentDSL State Query Void m
    eval (Noop n) = pure n
