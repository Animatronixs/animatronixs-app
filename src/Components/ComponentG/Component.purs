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
  , servoposition :: String
  , result :: Maybe String
  }

data Query a
  = ToggleState a
--  | GetState (Boolean -> a)
  | SetServoPosition String a
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
  initialState = { loading: false, servoposition: "", result: Nothing }

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
                  [ HH.text "Servo position (0%=left, 50%=center, 100%=right)"]
              ]
          , HH.div
              [ HP.class_ (H.ClassName "col span-2-of-3")]
              --[ HH.input
              --    [ HP.type_ HP.InputText
              --    , HP.name "servoposition"
              --    , HP.id_ "servoposition"
              --    , HP.placeholder "Servo position (choose 1 or 2)"
              --    , HP.value state.servoposition  
              --    , HE.onValueInput (HE.input SetServoPosition)
              --    ]
              --] 
              [ HH.input
                [ HP.type_ HP.InputRange
                -- , HP.list "tickmarks" 
                , HP.name "servoposition"
                , HP.id_ "servoposition"
                , HP.value state.servoposition
                , HE.onValueChange (HE.input SetServoPosition)
                ]
              , HH.datalist
                [ HP.id_ "tickmarks" ]
                [ HH.option
                  [ HP.value "0"
                  -- , HP.label "0%" 
                  ]
                  []
                , HH.option
                  [ HP.value "10" ]
                  []
                , HH.option
                  [ HP.value "20" ]
                  []
                , HH.option
                  [ HP.value "30" ]
                  []
                , HH.option
                  [ HP.value "40" ]
                  []
                , HH.option
                  [ HP.value "50" 
                  -- , HP.label "50%"    
                  ]
                  []
                , HH.option
                  [ HP.value "60" ]
                  []
                , HH.option
                  [ HP.value "70" ]
                  []
                , HH.option
                  [ HP.value "80" ]
                  []
                , HH.option
                  [ HP.value "90" ]
                  []
                , HH.option
                  [ HP.value "100" 
                  -- , HP.label "100%"    
                  ]
                  []                                                                                                                                                              
                ]
              ]

-- <input type="range" list="tickmarks">

-- <datalist id="tickmarks">
--   <option value="0" label="0%">
--   <option value="10">
--   <option value="20">
--   <option value="30">
--   <option value="40">
--   <option value="50" label="50%">
--   <option value="60">
--   <option value="70">
--   <option value="80">
--   <option value="90">
--   <option value="100" label="100%">
-- </datalist>


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
    SetServoPosition servoposition next -> do
      H.modify (_ { servoposition = servoposition, result = Nothing :: Maybe String })
      pure next
    MakeRequest next -> do
      servoposition <- H.gets _.servoposition
      H.modify (_ { loading = true })
      -- response <- H.liftAff $ AX.get ("http://localhost:8080/rotateservos?params=" <> servoposition)
      -- response <- H.liftAff $ AX.get ("http://192.168.1.101:8080/rotateservos?params=" <> servoposition)
      response <- H.liftAff $ AX.get ("/rotateservos?params=" <> servoposition)
      H.modify (_ { loading = false, result = Just response.response })
      pure next  
