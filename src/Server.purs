module Server where

import Led.Model (Led(..), getLedsEndpoint)
import Order.Model (Order(..), getOrdersEndpoint)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM.HTML.HTMLTrackElement.ReadyState (ReadyState(..))
import Data.Array (filter)
import Network.HTTP.Affjax (AJAX)
import Node.Express.Endpoint (EXPRESS, listen, hostStatic, hostEndpoint, makeApp)
import Prelude (Unit, (==), bind, pure, ($), discard)

myLeds :: Array Led
myLeds = [Led {ledId: 1, color: "green"}, Led {ledId: 2, color: "blue"}, Led {ledId: 3, color: "white"}]

myOrders :: Array Order
myOrders = [Order {productId: 1, quantity: 50}, Order {productId: 2, quantity: 6}]

main :: forall eff. Eff ( console :: CONSOLE, express :: EXPRESS, ajax :: AJAX, exception :: EXCEPTION | eff ) Unit
main = do
  app <- makeApp []
  hostEndpoint app getOrdersEndpoint (\productId _ -> pure $ filterOrders productId)
  hostEndpoint app getLedsEndpoint (\ledId _ -> pure $ filterLeds ledId)
  --hostStatic app "static"
  hostStatic app "dist"  
  listen app 8080
  log "Listening on port 8080"

filterLeds :: Int -> Array Led
filterLeds id = filter (\(Led {ledId}) -> ledId == id) myLeds

filterOrders :: Int -> Array Order
filterOrders id = filter (\(Order {productId}) -> productId == id) myOrders