module Index.Model where

import Prelude (class Show, Unit)

import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode.Generic (gEncodeJson)
import Data.Argonaut.Decode.Generic (gDecodeJson)
import Data.Generic (class Generic, gShow)
import Data.Endpoint (Endpoint(Endpoint))
import Data.HTTP.Method (Method(GET))

data Index = Index { indexId :: Int
                   , title :: String }

derive instance genericIndex :: Generic Index

instance encodeJsonIndex :: EncodeJson Index where
  encodeJson = gEncodeJson
instance decodeJsonIndex :: DecodeJson Index where
  decodeJson = gDecodeJson
instance showIndex :: Show Index where
  show = gShow

getIndicesEndpoint :: Endpoint Int Unit (Array Index)
getIndicesEndpoint = Endpoint {url: "/getindices", method: GET}