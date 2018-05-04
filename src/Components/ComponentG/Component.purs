module ComponentG.Component where

import Prelude

import CSS (offset)
import Control.Monad.Aff (Aff)
import Data.Array.ST.Iterator (next)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX

-- type State = Boolean
type State =
  { loading :: Boolean
  , servonumber :: String
  , result :: Maybe String
  }

data Query a
  = ToggleState a
--  | GetState (Boolean -> a)
  | SetServoNumber String a
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
  initialState = { loading: false, servonumber: "", result: Nothing }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.form
      [ HP.method (HP.POST)
      , HP.action "#"
      , HP.class_ (H.ClassName "contact-form")  
      ]
      [ HH.div
          [ HP.class_ (H.ClassName "row")]
          [ HH.div
              [ HP.class_ (H.ClassName "col span-1-of-3")]
              [ HH.label_
                  [ HH.text "Servo number"]
              ]
          , HH.div
              [ HP.class_ (H.ClassName "col span-2-of-3")]
              [ HH.input
                  [ HP.type_ HP.InputText
                  , HP.name "servonumber"
                  , HP.id_ "servonumber"
                  , HP.placeholder "Servo number (choose 1 or 2)"
                  , HP.value state.servonumber  
                  , HE.onValueInput (HE.input SetServoNumber)
                  ]
              ]                      
          ]
      , HH.div
          [ HP.class_ (H.ClassName "row")]
          [ HH.div
              [ HP.class_ (H.ClassName "col span-1-of-3")]
              [ HH.label_
                  [ HH.text " "]
              ]
          , HH.div
              [ HP.class_ (H.ClassName "col span-2-of-3")]
              [ HH.input
                [ HP.type_ HP.InputSubmit
                , HP.disabled state.loading
                , HE.onClick (HE.input_ MakeRequest)
                , HP.value "Rotate Servo"
                ]
              ]
              , HH.p_
                  [ HH.text (if state.loading then "Working..." else "")]
              , HH.div_
                  case state.result of
                    Nothing -> []
                    Just result ->
                      [ HH.h2_
                          [ HH.text "Response:"]
                      , HH.pre_
                          [ HH.code_
                              [ HH.text result]
                          ]
                      ]
          ]
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
    SetServoNumber servonumber next -> do
      H.modify (_ { servonumber = servonumber, result = Nothing :: Maybe String })
      pure next
    MakeRequest next -> do
      servonumber <- H.gets _.servonumber
      H.modify (_ { loading = true })
      -- response <- H.liftAff $ AX.get ("http://localhost:8080/rotateservos?params=" <> servonumber)
      -- response <- H.liftAff $ AX.get ("http://192.168.1.101:8080/rotateservos?params=" <> servonumber)
      response <- H.liftAff $ AX.get ("/rotateservos?params=" <> servonumber)
      H.modify (_ { loading = false, result = Just response.response })
      pure next  
