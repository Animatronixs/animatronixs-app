module Router where

import BigPrelude

import Container.Component as Container
import Control.Monad.Aff (Aff)
import Control.Monad.State.Class (modify)
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct (Coproduct)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.String (toLower)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Profile.Component as Profile
import Routing (matchesAff)
import Routing.Match (Match)
import Routing.Match.Class (lit, num)

data Input a
  = Goto Routes a

data CRUD
  = Index
  | Show Number

data Routes
  = Profile
--  | ItemList CRUD
  | Container
  | Home

init :: State
init = { currentPage: "Home" }

routing :: Match Routes
routing = profile
--      <|> itemlist
      <|> container
      <|> home
  where
    profile = Profile <$ route "profile"
    home = Home <$ lit ""
--    itemlist = ItemList <$> (route "itemlist" *> parseCRUD)
    container = Container <$ route "container"
    route str = lit "" *> lit str
--    parseCRUD = Show <$> num <|> pure Index

type State =
  { currentPage :: String
  }

--type ChildState = Either3 ProfileState ItemListState ContainerState

--type ChildQuery = Coproduct Profile.Input ItemList.Input
type ChildQuery = Coproduct Profile.Query Container.Query
--type ChildSlot = Either Profile.Slot ItemList.Slot
type ChildSlot = Either Profile.Slot Container.Slot

--pathToProfile :: ChildPath Profile.Input ChildQuery Profile.Slot ChildSlot
--pathToProfile = cpL
pathToProfile :: ChildPath Profile.Query ChildQuery Profile.Slot ChildSlot
pathToProfile = cpL

--pathToItemList :: ChildPath ItemList.Input ChildQuery ItemList.Slot ChildSlot
--pathToItemList = cpR
--pathToItemList :: ChildPath ItemList.Input ChildQuery ItemList.Slot ChildSlot
--pathToItemList = cpR :> cpL

pathToContainer :: ChildPath Container.Query ChildQuery Container.Slot ChildSlot
pathToContainer = cpR

type QueryP
  = Coproduct Input ChildQuery

ui :: forall m. H.Component HH.HTML Input Unit Void m
ui = H.parentComponent
  { initialState: const init
  , render
  , eval
  , receiver: const Nothing
  }
  where
    render :: State -> H.ParentHTML Input ChildQuery ChildSlot m
    render st =
      HH.div_
        [ HH.header_
          [ HH.nav_
            [ HH.div
              [ HP.class_ (H.ClassName "row")]
              [ HH.h1_ [ HH.text (st.currentPage) ]
              , HH.ul
                [ HP.class_ (H.ClassName "main-nav js--main-nav")]
                (map link ["Home", "Profile", "Container"])
              ]
              , HH.a
                [ HP.class_ (H.ClassName "mobile-nav-icon js--nav-icon")]
                [ HH.i
                  [ HP.class_ (H.ClassName "ion-navicon-round")]
                  []
                ]
            ]
            , HH.div
              [ HP.class_ (H.ClassName "hero-text-box")]
              [ HH.h1_
                [ HH.text "Goodbye junk food." 
                  , HH.br_
                  , HH.text "Hello super healthy meals."
                ]
                , HH.a 
                  [ HP.class_ (H.ClassName "btn btn-full js--scroll-to-plans")
                    , HP.href ("#")
                  ]
                  [ HH.text "I'm hungry" ]
                , HH.a 
                  [ HP.class_ (H.ClassName "btn btn-ghost js--scroll-to-start")
                    , HP.href ("#")
                  ]
                  [ HH.text "Show me more" ]
              ]
          ]
          , viewPage st.currentPage
        ]

    link s = HH.li_ [ HH.a [ HP.href ("#/" <> toLower s) ] [ HH.text s ] ]

    viewPage :: String -> H.ParentHTML Input ChildQuery ChildSlot m
    viewPage "Container" =
      HH.slot' pathToContainer Container.Slot Container.ui unit absurd
    viewPage "Profile" =
      HH.slot' pathToProfile Profile.Slot Profile.ui unit absurd
    viewPage _ =
      HH.div_ []

    eval :: Input ~> H.ParentDSL State Input ChildQuery ChildSlot Void m
    eval (Goto Profile next) = do
      modify (_ { currentPage = "Profile" })
      pure next
--    eval (Goto (ItemList view) next) = do
--      modify case view of
--                  Index -> (_ { currentPage = "ItemList" })
--                  Show n -> (_ { currentPage = "ItemList " <> show n })
--      pure next
    eval (Goto Container next) = do
      modify (_ { currentPage = "Container" })
      pure next
    eval (Goto Home next) = do
      modify (_ { currentPage = "Home" })
      pure next

routeSignal :: forall eff. H.HalogenIO Input Void (Aff (HA.HalogenEffects eff))
            -> Aff (HA.HalogenEffects eff) Unit
routeSignal driver = do
  Tuple old new <- matchesAff routing
  redirects driver old new

redirects :: forall eff. H.HalogenIO Input Void (Aff (HA.HalogenEffects eff))
          -> Maybe Routes
          -> Routes
          -> Aff (HA.HalogenEffects eff) Unit
redirects driver _ =
  driver.query <<< H.action <<< Goto
-- redirects driver _ Home =
--   driver (left (action (Goto Home))))
-- redirects driver _ Profile =
--   driver (left (action (Goto Profile))))
-- redirects driver _ (Items view) =
--   driver (left (action (Goto (Items view)))))