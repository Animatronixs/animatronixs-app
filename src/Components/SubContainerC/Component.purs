module SubContainerC.Component where

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
import Halogen.HTML as HH
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SubContainerC.Component.State (State)

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
    [ HP.class_ (H.ClassName "section-steps")
    , HP.id_ ("works")
    ]
    [
      HH.div
      [ HP.class_ (H.ClassName "row")]
      [ HH.h2_ 
        [ HH.text "How it works — Simple as 1, 2, 3"]
      ]
    , HH.div
      [ HP.class_ (H.ClassName "row")]
      [ HH.div
        [ HP.class_ (H.ClassName "col span-1-of-2 steps-box")]
        [ HH.img
          [ HP.src "app-iPhone.png"
          , HP.alt "Animatronixs app on iPhone"
          , HP.class_ (H.ClassName "app-screen js--wp-2 animated fadeInUp")
          ]
        ]
      , HH.div
        [ HP.class_ (H.ClassName "col span-1-of-2 stepsbox")]
        [ HH.div
          [ HP.class_ (H.ClassName "works-step clearfix")]
          [ HH.div_
            [ HH.text "1"]
          , HH.p_
            [ HH.text "Choose the subscription plan that best fits your needs and sign up today."]
          ]
        , HH.div
          [ HP.class_ (H.ClassName "works-step clearfix")]
          [ HH.div_
            [ HH.text "2"]
          , HH.p_
            [ HH.text "Order your delicious meal using our mobile app or website. Or you can even call us!"]
          ]
        , HH.div
          [ HP.class_ (H.ClassName "works-step clearfix")]
          [ HH.div_
            [ HH.text "3"]
          , HH.p_
            [ HH.text "Enjoy your meal after less than 20 minutes. See you the next time!"]
          ]
        , HH.a
          [ HP.href ("#")
          , HP.class_ (H.ClassName "btn-app")
          ]
          [ HH.img
            [ HP.src "download-app.svg"
            , HP.alt "App Store Button"  
            ]
          ]
        , HH.a
          [ HP.href ("#")
          , HP.class_ (H.ClassName "btn-app") 
          ]
          [ HH.img
            [ HP.src "download-app-android.png"
            , HP.alt "Play Store Button" 
            ]
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
