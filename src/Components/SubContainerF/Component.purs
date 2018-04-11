module SubContainerF.Component where

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
import SubContainerF.Component.State (State)

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
    [ HP.class_ (H.ClassName "section-plans js--section-plans")
    , HP.id_ ("plans")
    ]
    [ HH.div
        [ HP.class_ (H.ClassName "row")]
        [ HH.h2_
            [ HH.text "Start eating healthy today"]
        ]
    , HH.div
        [ HP.class_ (H.ClassName "row")]
        [ HH.div
            [ HP.class_ (H.ClassName "col span-1-of-3")]
            [ HH.div
                [ HP.class_ (H.ClassName "plan-box js--waypoint-4")]
                [ HH.div_
                    [ HH.h3_
                        [ HH.text "Premium"]
                    , HH.p
                        [ HP.class_ (H.ClassName "plan-price")]
                        [ HH.text "$399"
                        , HH.span_
                            [ HH.text "/ month"]
                        ]
                    , HH.p
                        [ HP.class_ (H.ClassName "plan-price-meal")]
                        [ HH.text "That's only 13.30$ per meal"]
                    ]
                , HH.div_
                    [ HH.ul_
                        [ HH.li_
                            [ HH.i
                                [ HP.class_ (H.ClassName "ion-ios-checkmark-empty icon-small")]
                                []
                            , HH.text "1 meal every day"
                            ]
                        , HH.li_
                            [ HH.i
                                [ HP.class_ (H.ClassName "ion-ios-checkmark-empty icon-small")]
                                []
                            , HH.text "Order 24/7"
                            ]
                        , HH.li_
                            [ HH.i
                                [ HP.class_ (H.ClassName "ion-ios-checkmark-empty icon-small")]
                                []
                            , HH.text "Access to newest creations"
                            ]
                        , HH.li_
                            [ HH.i
                                [ HP.class_ (H.ClassName "ion-ios-checkmark-empty icon-small")]
                                []
                            , HH.text "Free delivery"
                            ]    
                        ]
                    ]
                , HH.div_
                    [ HH.a
                        [ HP.href "#", HP.class_ (H.ClassName "btn btn-full")]
                        [ HH.text "Sign up now"]
                    ]
                ]
            ]


        ,  HH.div
            [ HP.class_ (H.ClassName "col span-1-of-3")]
            [ HH.div
                [ HP.class_ (H.ClassName "plan-box js--waypoint-4")]
                [ HH.div_
                    [ HH.h3_
                        [ HH.text "Pro"]
                    , HH.p
                        [ HP.class_ (H.ClassName "plan-price")]
                        [ HH.text "$149"
                        , HH.span_
                            [ HH.text "/ month"]
                        ]
                    , HH.p
                        [ HP.class_ (H.ClassName "plan-price-meal")]
                        [ HH.text "That's only 14.90$ per meal"]
                    ]
                , HH.div_
                    [ HH.ul_
                        [ HH.li_
                            [ HH.i
                                [ HP.class_ (H.ClassName "ion-ios-checkmark-empty icon-small")]
                                []
                            , HH.text "1 meal 10 days/month"
                            ]
                        , HH.li_
                            [ HH.i
                                [ HP.class_ (H.ClassName "ion-ios-checkmark-empty icon-small")]
                                []
                            , HH.text "Order 24/7"
                            ]
                        , HH.li_
                            [ HH.i
                                [ HP.class_ (H.ClassName "ion-ios-checkmark-empty icon-small")]
                                []
                            , HH.text "Access to newest creations"
                            ]
                        , HH.li_
                            [ HH.i
                                [ HP.class_ (H.ClassName "ion-ios-checkmark-empty icon-small")]
                                []
                            , HH.text "Free delivery"
                            ]    
                        ]
                    ]
                , HH.div_
                    [ HH.a
                        [ HP.href "#", HP.class_ (H.ClassName "btn btn-ghost")]
                        [ HH.text "Sign up now"]
                    ]
                ]
            ]
        ,  HH.div
            [ HP.class_ (H.ClassName "col span-1-of-3")]
            [ HH.div
                [ HP.class_ (H.ClassName "plan-box js--waypoint-4")]
                [ HH.div_
                    [ HH.h3_
                        [ HH.text "Starter"]
                    , HH.p
                        [ HP.class_ (H.ClassName "plan-price")]
                        [ HH.text "$19"
                        , HH.span_
                            [ HH.text "/ meal"]
                        ]
                    , HH.p
                        [ HP.class_ (H.ClassName "plan-price-meal")]
                        [ HH.text "."]
                    ]
                , HH.div_
                    [ HH.ul_
                        [ HH.li_
                            [ HH.i
                                [ HP.class_ (H.ClassName "ion-ios-checkmark-empty icon-small")]
                                []
                            , HH.text "1 meal"
                            ]
                        , HH.li_
                            [ HH.i
                                [ HP.class_ (H.ClassName "ion-ios-checkmark-empty icon-small")]
                                []
                            , HH.text "Order from 8 am to 12 pm"
                            ]
                        , HH.li_
                            [ HH.i
                                [ HP.class_ (H.ClassName "ion-ios-close-empty icon-small")]
                                []
                            , HH.text ""
                            ]
                        , HH.li_
                            [ HH.i
                                [ HP.class_ (H.ClassName "ion-ios-checkmark-empty icon-small")]
                                []
                            , HH.text "Free delivery"
                            ]    
                        ]
                    ]
                , HH.div_
                    [ HH.a
                        [ HP.href "#", HP.class_ (H.ClassName "btn btn-ghost")]
                        [ HH.text "Sign up now"]
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