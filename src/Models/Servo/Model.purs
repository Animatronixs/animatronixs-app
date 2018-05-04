module Servo.Model where

import Prelude (class Show, Unit)

import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode.Generic (gEncodeJson)
import Data.Argonaut.Decode.Generic (gDecodeJson)
import Data.Generic (class Generic, gShow)
import Data.Endpoint (Endpoint(Endpoint))
import Data.HTTP.Method (Method(GET))

data Servo = Servo { servoId :: Int
                   , color :: String }

derive instance genericServo :: Generic Servo

instance encodeJsonLed :: EncodeJson Servo where
  encodeJson = gEncodeJson
instance decodeJsonLed :: DecodeJson Servo where
  decodeJson = gDecodeJson
instance showServo :: Show Servo where
  show = gShow

-- getServosEndpoint :: Endpoint Int Unit (Array Servo)
-- getServosEndpoint = Endpoint {url: "/getservos", method: GET}

rotateServosEndpoint :: Endpoint Int Unit (Array Servo)
rotateServosEndpoint = Endpoint {url: "/rotateservos", method: GET}