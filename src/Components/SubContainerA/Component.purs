module SubContainerA.Component where

import Prelude

import CSS as CB
import ComponentA.Component as ComponentA
import ComponentB.Component as ComponentB
import ComponentC.Component as ComponentC

import SubContainerA.Component.State (State)
import Control.Monad.Eff.Exception (stack)
import Control.Monad.State (state)
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (Maybe(..), maybe)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Query a = ReadStates a

type ChildQuery = Coproduct3 ComponentA.Query ComponentB.Query ComponentC.Query
type ChildSlot = Either3 Unit Unit Unit

-- Values of the type Slot are used as the IDs for child components
-- in the rendered HTML.
data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

ui :: forall m. H.Component HH.HTML Query Unit Void m
ui = 
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState =
    { b: Nothing
    , c: Nothing
    }     

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m 
  render state = 
    HH.section
    [ HP.class_ (H.ClassName "section-features js--section-features")
    , HP.id_ ("features")
    ]
    [ HH.div 
      [ HP.class_ (H.ClassName "row")]
      [ HH.h2_
        [ HH.text "Make it real — make it life"]
      , HH.p
        [ HP.class_ (H.ClassName "long-copy")]
        [ HH.text "Hello, we're Animatronixs, your real life creation service. We know you're imaginative. No time for waiting. So let us turn your ideas into reality, we'really good at it, we promise!"]  
      ]
    , HH.div
      [ HP.class_ (H.ClassName "row js-waypoint-1 animated fadeIn")]
      [ HH.div 
        [ HP.class_ (H.ClassName "col span-1-of-4 box")]
        [ HH.i 
          [ HP.class_ (H.ClassName "ion-ios-infinite-outline icon-big")]
          []
        , HH.h3_
          [ HH.text "Control at your fingertips"]
        , HH.p_
          [ HH.text "Never cook again! We really mean that. Our subscription plans include up to 365 days/year coverage. You can also choose to order more flexibly if that's your style."] 
        ]
      , HH.div
        [ HP.class_ (H.ClassName "col span-1-of-4 box")]
        [ HH.i 
          [ HP.class_ (H.ClassName "ion-ios-stopwatch-outline icon-big")]
          []
        , HH.h3_
          [ HH.text "Real-time actions"]
        , HH.p_
          [ HH.text "You're only twenty minutes away from your delicious and super healthy meals delivered right to your home. We work with the best chefs in each town to ensure that you're 100% happy."]
        ]
      , HH.div
        [ HP.class_ (H.ClassName "col span-1-of-4 box")]
        [ HH.i 
          [ HP.class_ (H.ClassName "ion-ios-nutrition-outline icon-big")]
          []
        , HH.h3_
          [ HH.text "100% creator-friendly"]
        , HH.p_
          [ HH.text "All our vegetables are fresh, organic and local. Animals are raised without added hormones or antibiotics. Good for your health, the environment, and it also tastes better!"]
        ]
      , HH.div
        [ HP.class_ (H.ClassName "col span-1-of-4 box")]
        [ HH.i 
          [ HP.class_ (H.ClassName "ion-ios-cart-outline icon-big")]
          []
        , HH.h3_
          [ HH.text "Start collecting"]
        , HH.p_
          [ HH.text "We don't limit your creativity, which means you can order whatever you feel like. You can also choose from our menu containing over 100 delicious meals. It's up to you!"]
        ]
      ]
    ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
  eval = case _ of
    ReadStates next -> do
      pure next