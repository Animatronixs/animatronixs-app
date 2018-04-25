module Led.Model where

import Prelude (class Show, Unit)

import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode.Generic (gEncodeJson)
import Data.Argonaut.Decode.Generic (gDecodeJson)
import Data.Generic (class Generic, gShow)
import Data.Endpoint (Endpoint(Endpoint))
import Data.HTTP.Method (Method(GET))

data Led = Led { ledId :: Int
                   , color :: String }

derive instance genericLed :: Generic Led

instance encodeJsonLed :: EncodeJson Led where
  encodeJson = gEncodeJson
instance decodeJsonLed :: DecodeJson Led where
  decodeJson = gDecodeJson
instance showLed :: Show Led where
  show = gShow

getLedsEndpoint :: Endpoint Int Unit (Array Led)
getLedsEndpoint = Endpoint {url: "/getleds", method: GET}