module SubContainerH.Component where

import Prelude

import CSS as CB
import ComponentA.Component as ComponentA
import ComponentB.Component as ComponentB
import ComponentC.Component as ComponentC
import Control.Monad.Eff.Exception (stack)
import Control.Monad.State (state)
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
import Halogen.HTML.Properties (FormMethod(..))
import Halogen.HTML.Properties as H.HTML
import Halogen.HTML.Properties as HP
import SubContainerH.Component.State (State)

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
      [ HP.class_ (H.ClassName "section-form")
      , HP.id_ ("form2")
      ]
      [ HH.div
          [ HP.class_ (H.ClassName "row")]
          [ HH. h2_ 
              [ HH.text "Example of Aff Ajax"]
          ]
      -- START OF NEW CODE    
      , HH.div
          [ HP.class_ (H.ClassName "row")]
          [
              HH.form_ $
                [ HH.h1_ 
                    [ HH.text "Lookup GitHub user"]
                , HH.label_
                    [ HH.div_
                        [ HH.text "Enter username:"]
                    --, continue...
                    ]                  
                ]
          ]
      -- END OF NEW CODE
      , HH.div
          [ HP.class_ (H.ClassName "row")]
          [ HH.form
              [ HP.method (H.HTML.POST)
              , HP.action "#"
              , HP.class_ (H.ClassName "contact-form")  
              ] 
              [ HH.div
                  [ HP.class_ (H.ClassName "row")]
                  [ HH.div
                      [ HP.class_ (H.ClassName "col span-1-of-3")]
                      [ HH.label
                          [ HP.for "name"]
                          [ HH.text "Name"]
                      ]
                  , HH.div
                      [ HP.class_ (H.ClassName "col span-2-of-3")]
                      [ HH.input
                          [ HP.type_ HP.InputText
                          , HP.name "name"
                          , HP.id_ "name"
                          , HP.placeholder "Your name"
                          , HP.required true
                          ]
                      ]
                  ]
              , HH.div
                  [ HP.class_ (H.ClassName "row")]
                  [ HH.div
                      [ HP.class_ (H.ClassName "col span-1-of-3")]
                      [ HH.label
                          [ HP.for "email"]
                          [ HH.text "Email"]
                      ]
                  , HH.div
                      [ HP.class_ (H.ClassName "col span-2-of-3")]
                      [ HH.input
                          [ HP.type_ HP.InputEmail
                          , HP.name "email"
                          , HP.id_ "email"
                          , HP.placeholder "Your email"
                          , HP.required true
                          ]
                      ]
                  ]
              , HH.div
                  [ HP.class_ (H.ClassName "row")]
                  [ HH.div
                      [ HP.class_ (H.ClassName "col span-1-of-3")]
                      [ HH.label
                          [ HP.for "find-us"]
                          [ HH.text "How did you find us?"]
                      ]
                  , HH.div
                      [ HP.class_ (H.ClassName "col span-2-of-3")]
                      [ HH.select
                          [ HP.name "find-us"
                          , HP.id_ "find-us"
                          ]
                          [ HH.option
                              [ HP.value "friends"
                              , HP.selected true 
                              ]
                              [ HH.text "Friends"]
                          , HH.option
                              [ HP.value "search"]
                              [ HH.text "Search engine"]
                          , HH.option
                              [ HP.value "ad"]
                              [ HH.text "Advertisement"]
                          , HH.option
                              [ HP.value "other"]
                              [ HH.text "Other"]
                          ]
                      ]
                  ]
              , HH.div
                  [ HP.class_ (H.ClassName "row")]
                  [ HH.div
                      [ HP.class_ (H.ClassName "col span-1-of-3")]
                      [ HH.label_
                          [ HH.text "Newsletter?"]
                      ]
                  , HH.div
                      [ HP.class_ (H.ClassName "col span-2-of-3")]
                      [ HH.input
                          [ HP.type_ HP.InputCheckbox
                          , HP.name "news"
                          , HP.id_ "news"
                          , HP.checked true
                          ]
                      , HH.text "Yes please"
                      ]                      
                  ]
              , HH.div
                  [ HP.class_ (H.ClassName "row")]
                  [ HH.div
                      [ HP.class_ (H.ClassName "col span-1-of-3")]
                      [ HH.label_
                          [ HH.text "Drop us a line"]
                      ]
                  , HH.div
                      [ HP.class_ (H.ClassName "col span-2-of-3")]
                      [ HH.textarea
                          [ HP.name "message"
                          , HP.placeholder "Your message"
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
                          , HP.value "Send it!"
                          ]
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