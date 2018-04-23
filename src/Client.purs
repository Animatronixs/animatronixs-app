 module Client where

import BigPrelude

import Color.Scheme.X11 (darkolivegreen)
import Control.Monad.Aff (forkAff)
import DOM.HTML.Document (body)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Network.HTTP.Affjax as AX -- new

import Router as R

-- main :: forall eff. Eff (HA.HalogenEffects eff) Unit
main :: forall eff. Eff (ajax :: AX.AJAX | HA.HalogenEffects eff) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  driver <- runUI R.ui unit body
  forkAff $ R.routeSignal driver
