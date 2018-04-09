module SubContainerD.Component where

import Prelude

import CSS as CB
import ComponentA.Component as ComponentA
import ComponentB.Component as ComponentB
import ComponentC.Component as ComponentC
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
import SubContainerD.Component.State (State)

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
    [ HP.class_ (H.ClassName "section-cities")
    , HP.id_ ("cities")
    ]
    [ HH.div
        [ HP.class_ (H.ClassName "row")]
        [ HH.h2_
          [ HH.text "We're currently in these cities"]
        ]
    , HH.div
        [ HP.class_ (H.ClassName "row js--waypoint-3 animated fadeIn")]
        [ HH.div 
            [ HP.class_ (H.ClassName "col span-1-of-4 box")]
            [ HH.img 
              [ HP.src "lisbon.jpg"
              , HP.alt "Lisbon"
              ]
            , HH.h3_
                [ HH.text "Lisbon"]
            , HH.div
                [ HP.class_ (H.ClassName "city-feature")]
                [ HH.i 
                    [ HP.class_ (H.ClassName "ion-ios-person icon-small")]
                    []
                , HH.text "1600+ happy eaters"
                ]
            , HH.div
                [ HP.class_ (H.ClassName "city-feature")]
                [ HH.i 
                    [ HP.class_ (H.ClassName "ion-ios-star icon-small")]
                    []
                , HH.text "60+ top chefs"
                ]
            , HH.div
                [ HP.class_ (H.ClassName "city-feature")]
                [ HH.i 
                    [ HP.class_ (H.ClassName "ion-social-twitter icon-small")]
                    []
                , HH.a
                    [ HP.href "#"]
                    [ HH.text "@animatronixs_lx"]
                ]
            ]
        , HH.div
            [ HP.class_ (H.ClassName "col span-1-of-4 box")]
            [ HH.img 
              [ HP.src "san-francisco.jpg"
              , HP.alt "San Francisco"
              ]
            , HH.h3_
                [ HH.text "San Francisco"]
            , HH.div
                [ HP.class_ (H.ClassName "city-feature")]
                [ HH.i 
                    [ HP.class_ (H.ClassName "ion-ios-person icon-small")]
                    []
                , HH.text "3700+ happy eaters"
                ]
            , HH.div
                [ HP.class_ (H.ClassName "city-feature")]
                [ HH.i 
                    [ HP.class_ (H.ClassName "ion-ios-star icon-small")]
                    []
                , HH.text "160+ top chefs"
                ]
            , HH.div
                [ HP.class_ (H.ClassName "city-feature")]
                [ HH.i 
                    [ HP.class_ (H.ClassName "ion-social-twitter icon-small")]
                    []
                , HH.a
                    [ HP.href "#"]
                    [ HH.text "@animatronixs_sf"]
                ]
            ]
        , HH.div 
            [ HP.class_ (H.ClassName "col span-1-of-4 box")]
            [ HH.img 
              [ HP.src "berlin.jpg"
              , HP.alt "Berlin"
              ]
            , HH.h3_
                [ HH.text "Berlin"]
            , HH.div
                [ HP.class_ (H.ClassName "city-feature")]
                [ HH.i 
                    [ HP.class_ (H.ClassName "ion-ios-person icon-small")]
                    []
                , HH.text "2300+ happy eaters"
                ]
            , HH.div
                [ HP.class_ (H.ClassName "city-feature")]
                [ HH.i 
                    [ HP.class_ (H.ClassName "ion-ios-star icon-small")]
                    []
                , HH.text "110+ top chefs"
                ]
            , HH.div
                [ HP.class_ (H.ClassName "city-feature")]
                [ HH.i 
                    [ HP.class_ (H.ClassName "ion-social-twitter icon-small")]
                    []
                , HH.a
                    [ HP.href "#"]
                    [ HH.text "@animatronixs_berlin"]
                ]
            ]
        , HH.div
            [ HP.class_ (H.ClassName "col span-1-of-4 box")]
            [ HH.img 
              [ HP.src "london.jpg"
              , HP.alt "London"
              ]
            , HH.h3_
                [ HH.text "London"]
            , HH.div
                [ HP.class_ (H.ClassName "city-feature")]
                [ HH.i 
                    [ HP.class_ (H.ClassName "ion-ios-person icon-small")]
                    []
                , HH.text "1200+ happy eaters"
                ]
            , HH.div
                [ HP.class_ (H.ClassName "city-feature")]
                [ HH.i 
                    [ HP.class_ (H.ClassName "ion-ios-star icon-small")]
                    []
                , HH.text "50+ top chefs"
                ]
            , HH.div
                [ HP.class_ (H.ClassName "city-feature")]
                [ HH.i 
                    [ HP.class_ (H.ClassName "ion-social-twitter icon-small")]
                    []
                , HH.a
                    [ HP.href "#"]
                    [ HH.text "@animatronixs_london"]
                ]
            ]            
        ]
    ] 

--    HH.div_
--    [ HH.div
--        [ HP.class_ (H.ClassName "box")]
--        [ HH.h2_ [ HH.text "Component A" ]
--        , HH.slot' CP.cp1 unit ComponentA.component unit absurd
--        ]
--    , HH.div
--        [ HP.class_ (H.ClassName "box")]
--        [ HH.h2_ [ HH.text "Component B" ]
--        , HH.slot' CP.cp2 unit ComponentB.component unit absurd
--        ]
--    , HH.div
--        [ HP.class_ (H.ClassName "box")]
--        [ HH.h2_ [ HH.text "Component C" ]
--        , HH.slot' CP.cp3 unit ComponentC.component unit absurd
--        ]
--    , HH.p_
--        [ HH.text "Last observed states:"]
--    , HH.ul_
--        [ HH.li_ [ HH.text ("Component A: ") ]
--        , HH.li_ [ HH.text ("Component B: ") ]
--        , HH.li_ [ HH.text ("Component C: ") ]
--        ]
--    , HH.button
--        [ HE.onClick (HE.input_ ReadStates) ]
--        [ HH.text "Check states now" ]
--    ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
  eval = case _ of
    ReadStates next -> do
      --b <- H.query' CP.cp2 unit (H.request ItemListB.GetCount)
      --c <- H.query' CP.cp3 unit (H.request ItemListC.GetValue)
      --H.put { b, c }
      pure next