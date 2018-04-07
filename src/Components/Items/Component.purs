module Items.Component where

import Data.Maybe (Maybe(..))
import Halogen as H
import CSS (color, red)
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Prelude (class Eq, class Ord, type (~>), Unit, Void, const, pure, unit)

data Input a
  = Noop a

type State = Unit

data Slot = Slot

derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

ui :: forall m. H.Component HH.HTML Input Unit Void m
ui = H.component
  { initialState: const unit
  , render
  , eval
  , receiver: const Nothing
  }
  where
    render _ =
      HH.div_
        [ HH.h1 [CSS.style do color red] [ HH.text "Your Items" ]
        , HH.p_ [ HH.text "Snowy Owl" ]
        ]
    eval :: Input ~> H.ComponentDSL State Input Void m
    eval (Noop n) = pure n