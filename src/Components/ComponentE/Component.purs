module ComponentE.Component where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Array.ST.Iterator (next)
import Data.Maybe (Maybe(..))
import Halogen as H
-- import Halogen.HTML (a)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX

-- type State = Boolean
type State =
  { loading :: Boolean
  , username :: String
  , result :: Maybe String
  }

data Query a
  = ToggleState a
--  | GetState (Boolean -> a)
  | SetUserName String a
  | MakeRequest a

-- component :: forall m. H.Component HH.HTML Query Unit Void m
component :: forall eff. H.Component HH.HTML Query Unit Void (Aff (ajax :: AX.AJAX | eff))
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  -- initialState = false
  initialState = { loading: false, username: "", result: Nothing }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.p_ [ HH.text "Toggle me!" ]
      , HH.button
          [ HE.onClick (HE.input_ ToggleState) ]
          [ HH.text "button" ] -- [ HH.text (if state then "On" else "Off") ]
      ]

  -- eval :: Query ~> H.ComponentDSL State Query Void m
  eval :: Query ~> H.ComponentDSL State Query Void (Aff (ajax :: AX.AJAX | eff))
  eval = case _ of
  -- eval (ToggleState next) = do
  --  H.modify not
  --  pure next
    ToggleState next -> do
      pure next
--  eval (GetState reply) = do
--    reply <$> H.get
    SetUserName username next -> do
      pure next
    MakeRequest next -> do
      pure next  

  