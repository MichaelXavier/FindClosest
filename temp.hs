import Network.HTTP
import Network.URI (URI, parseURIReference)

import Text.JSON

import Control.Applicative
import Data.URLEncoded (importList, addToURI, URLEncoded)
import Data.List (intersperse)

--{-# LANGUAGE OverlappingInstances #-}

data DirectionsRequest = DirectionsRequest {
                           origin::String,
                           destination::String
                         } deriving (Show)

buildParams :: DirectionsRequest -> URLEncoded
buildParams (DirectionsRequest o d) = importList $ [
                                                     ("origin",      o),
                                                     ("destination", d),
                                                     ("mode",        "driving"),
                                                     ("sensor",      "false")
                                                   ]

requestURI :: DirectionsRequest -> Maybe URI
requestURI r = addToURI (buildParams r) `fmap` (parseURIReference "http://maps.google.com/maps/api/directions/json")
