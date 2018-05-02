module Server where

-- Uses external node modules
-- Read https://kofno.github.io/2015/10/19/purescript-ffi.html
-- Read https://github.com/purescript/documentation/blob/master/guides/FFI.md

-- var tessel = require('tessel');
-- var http = require('http');
-- var fs = require('fs');
-- var url = require('url');

import Led.Model (Led(..), toggleLedsEndpoint)
import Order.Model (Order(..), getOrdersEndpoint)
import Index.Model (Index(..), getIndicesEndpoint)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM.HTML.HTMLTrackElement.ReadyState (ReadyState(..))
import Data.Array (filter)
import Network.HTTP.Affjax (AJAX)
import Node.Express.Endpoint (EXPRESS, listen, hostStatic, hostEndpoint, makeApp)
import Node.Tessel
import Prelude (Unit, (==), bind, pure, ($), discard)

myLeds :: Array Led
myLeds = [Led {ledId: 1, color: "green"}, Led {ledId: 2, color: "blue"}, Led {ledId: 3, color: "white"}]

myOrders :: Array Order
myOrders = [Order {productId: 1, quantity: 50}, Order {productId: 2, quantity: 6}]

myIndices :: Array Index
myIndices = [Index {indexId: 1, title: "index"}]

main :: forall eff. Eff ( console :: CONSOLE, express :: EXPRESS, ajax :: AJAX, exception :: EXCEPTION | eff ) Unit
main = do
  -- let tessel = Tessel -- reference Tessel library
  app <- makeApp []
  hostEndpoint app getOrdersEndpoint (\productId _ -> pure $ filterOrders productId)
  hostEndpoint app toggleLedsEndpoint (\ledId _ -> pure $ filterLeds ledId)
  hostEndpoint app getIndicesEndpoint (\indexId _ -> pure $ filterIndices indexId)
  -- hostStatic app "static"
  hostStatic app "dist"  
  listen app 8080
  log "Listening on port 8080"

filterLeds :: Int -> Array Led
filterLeds id = filter (\(Led {ledId}) -> ledId == id) myLeds

filterOrders :: Int -> Array Order
filterOrders id = filter (\(Order {productId}) -> productId == id) myOrders

filterIndices :: Int -> Array Index
filterIndices id = filter (\(Index {indexId}) -> indexId == id) myIndices