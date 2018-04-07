module Container.Component where

import Prelude

import CSS as CB
import Container.Component.State (State)
import Control.Monad.Eff.Exception (stack)
import Control.Monad.State (state)
import Data.Either.Nested (Either4)
import Data.Functor.Coproduct.Nested (Coproduct4)
import Data.Maybe (Maybe(..), maybe)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SubContainerA.Component as SubContainerA
import SubContainerB.Component as SubContainerB
import SubContainerC.Component as SubContainerC
import SubContainerD.Component as SubContainerD
--import ItemList.Component as ComponentA

data Query a = ReadStates a

type ChildQuery = Coproduct4 SubContainerA.Query SubContainerB.Query SubContainerC.Query SubContainerD.Query
type ChildSlot = Either4 Unit Unit Unit Unit

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
    { --a: Nothing
    --, b: Nothing
    --, c: Nothing  
    --, d: Nothing      
    }   

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m 
  render state = 
    HH.div_
    [ HH.div
        [ HP.class_ (H.ClassName "box")]
        [ HH.h2_ [ HH.text "SubContainer A!" ]
        , HH.slot' CP.cp1 unit SubContainerA.ui unit absurd
        ]
    , HH.div
        [ HP.class_ (H.ClassName "box")]
        [ HH.h2_ [ HH.text "SubContainer B" ]
        , HH.slot' CP.cp2 unit SubContainerB.ui unit absurd
        ]
    , HH.div
        [ HP.class_ (H.ClassName "box")]
        [ HH.h2_ [ HH.text "SubContainer C" ]
        , HH.slot' CP.cp3 unit SubContainerC.ui unit absurd
        ]
    , HH.div
        [ HP.class_ (H.ClassName "box")]
        [ HH.h2_ [ HH.text "SubContainer D" ]
        , HH.slot' CP.cp4 unit SubContainerD.ui unit absurd
        ]       
    --, HH.p_
    --    [ HH.text "Last observed states:"]
    --, HH.ul_
    --    [ HH.li_ [ HH.text ("SubContainer A: " <> show state.a) ]
    --    , HH.li_ [ HH.text ("SubContainer B: " <> show state.b) ]
    --    , HH.li_ [ HH.text ("SubContainer C: " <> show state.c) ]
    --    ]
    --, HH.button
    --    [ HE.onClick (HE.input_ ReadStates) ]
    --    [ HH.text "Check states now" ]
    ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
  eval = case _ of
    ReadStates next -> do
      --a <- H.query' CP.cp1 unit (H.request ComponentA.GetCount)
      --b <- H.query' CP.cp2 unit (H.request ComponentB.GetValue)
      --c <- H.query' CP.cp3 unit (H.request ComponentC.GetCount)
      --d <- H.query' CP.cp4 unit (H.request ComponentD.GetCount)
      H.put {}
      pure next