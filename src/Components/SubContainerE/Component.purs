module SubContainerE.Component where

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
import SubContainerE.Component.State (State)

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
    [ HP.class_ (H.ClassName "section-testimonials")
    , HP.id_ ("testimonials")
    ]
    [ HH.div
        [ HP.class_ (H.ClassName "row")]
        [ HH.h2_ 
            [ HH.text "Our customers can't live without us"]
        ]
    , HH.div
        [ HP.class_ (H.ClassName "row")]
        [ HH.div
            [ HP.class_ (H.ClassName "col span-1-of-3")]
            [ HH.blockquote_
                [ HH.text "Omnifood is just awesome! I just launched a startup which leaves me with no time for cooking, so Omnifood is a life-saver. Now that I got used to it, I couldn't live without my daily meals!"]
                , HH.cite_
                    [ HH.img 
                      [ HP.src "customer-1.jpg", HP.alt "Customer 1 photo"]
                    , HH.text "Alberto Duncan"
                    ]
            ]
        , HH.div
            [ HP.class_ (H.ClassName "col span-1-of-3")]
            [ HH.blockquote_
                [ HH.text "Inexpensive, healthy and great-tasting meals, delivered right to my home. We have lots of food delivery here in Lisbon, but no one comes even close to Omifood. Me and my family are so in love!"]
                , HH.cite_
                    [ HH.img 
                      [ HP.src "customer-2.jpg", HP.alt "Customer 2 photo"]
                    , HH.text "Joana Silva"
                    ]
            ]            
        , HH.div
            [ HP.class_ (H.ClassName "col span-1-of-3")]
            [ HH.blockquote_
                [ HH.text "I was looking for a quick and easy food delivery service in San Franciso. I tried a lot of them and ended up with Omnifood. Best food delivery service in the Bay Area. Keep up the great work!"]
                , HH.cite_
                    [ HH.img 
                      [ HP.src "customer-3.jpg", HP.alt "Customer 3 photo"]
                    , HH.text "Milton Chapman"
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