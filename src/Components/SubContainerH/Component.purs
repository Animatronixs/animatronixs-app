module SubContainerH.Component where

import Prelude

import CSS (offset)
import CSS as CB
import ComponentA.Component as ComponentA
import ComponentB.Component as ComponentB
import ComponentE.Component as ComponentE
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (stack)
import Control.Monad.State (state)
import Data.Array.NonEmpty (findLastIndex)
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.HTTP.Method (Method(..))
import Data.HTTP.Method as Data.HTTP.Method
import Data.Maybe (Maybe(..), maybe)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Util (effPure)
import Network.HTTP.Affjax as AX
import SubContainerH.Component.State (State)
import Type.Row.Effect.Equality (effFrom)

-- This component is based partially on
-- https://github.com/slamdata/purescript-halogen/blob/master/examples/effects-aff-ajax/src/Component.purs

data Query a 
  -- = ReadStates a
  -- | 
  = SetUserName String a
  | MakeRequest a

type ChildQuery = Coproduct3 ComponentA.Query ComponentB.Query ComponentE.Query
type ChildSlot = Either3 Unit Unit Unit

-- Values of the type Slot are used as the IDs for child components
-- in the rendered HTML.
data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

-- ui :: forall m. H.Component HH.HTML Query Unit Void m
ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (ajax :: AX.AJAX | eff))
ui = 
  H.parentComponent
  -- H.component
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
    , loading: false
    , username: ""
    , result: Nothing
    }     

  -- render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render :: State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (ajax :: AX.AJAX | eff))
  render state =
    HH.section
      [ HP.class_ (H.ClassName "section-form")
      , HP.id_ ("form2")
      ]
      [ HH.div
          [ HP.class_ (H.ClassName "row")]
          [ HH. h2_ 
              [ HH.text "Example of Aff Ajax"]
          ]   
      , HH.div
          [ HP.class_ (H.ClassName "row")]
          [ HH.slot' CP.cp3 unit ComponentE.component unit absurd]
      ] 

  -- eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (Aff (ajax :: AX.AJAX | eff))
  eval = case _ of
   -- ReadStates next -> do
      --b <- H.query' CP.cp2 unit (H.request ItemListB.GetCount)
      --c <- H.query' CP.cp3 unit (H.request ItemListC.GetValue)
      --H.put { b, c }
    --  pure next
    SetUserName username next -> do
      H.modify(_ { username = username, result = Nothing :: Maybe String })
      pure next
    MakeRequest next -> do
      username <- H.gets _.username
      H.modify (_ { loading = true })
      response <- H.liftAff $ AX.get ("https://api.github.com/users/" <> username)
      H.modify (_ { loading = false, result = Just response.response})
      -- H.modify (_ { loading = false, result = Just "dummy response"})
      pure next