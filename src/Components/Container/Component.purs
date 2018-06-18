module Container.Component where

import Prelude

import Control.Monad.Aff (Aff)

import CSS as CB
import Container.Component.State (State)
import Control.Monad.Eff.Exception (stack)
import Control.Monad.State (state)
import Data.Either.Nested (Either8)
import Data.Functor.Coproduct.Nested (Coproduct8)
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
import SubContainerE.Component as SubContainerE
import SubContainerF.Component as SubContainerF
import SubContainerG.Component as SubContainerG
import SubContainerH.Component as SubContainerH
import Network.HTTP.Affjax as AX

data Query a = ReadStates a

type ChildQuery = Coproduct8 SubContainerA.Query SubContainerB.Query SubContainerC.Query SubContainerD.Query SubContainerE.Query SubContainerF.Query SubContainerG.Query SubContainerH.Query
type ChildSlot = Either8 Unit Unit Unit Unit Unit Unit Unit Unit

-- Values of the type Slot are used as the IDs for child components
-- in the rendered HTML.
data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (ajax :: AX.AJAX | eff))
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
    { }   

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (ajax :: AX.AJAX | eff))
  render state = 
    HH.div_
    [ HH.slot' CP.cp1 unit SubContainerA.ui unit absurd
    , HH.slot' CP.cp2 unit SubContainerB.ui unit absurd
    , HH.slot' CP.cp3 unit SubContainerC.ui unit absurd
    , HH.slot' CP.cp4 unit SubContainerD.ui unit absurd
    , HH.slot' CP.cp5 unit SubContainerE.ui unit absurd
    , HH.slot' CP.cp6 unit SubContainerF.ui unit absurd
    , HH.slot' CP.cp7 unit SubContainerG.ui unit absurd
    , HH.slot' CP.cp8 unit SubContainerH.ui unit absurd
    ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (Aff (ajax :: AX.AJAX | eff))
  eval = case _ of
    ReadStates next -> do                   
      H.put {}
      pure next