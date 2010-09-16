module Google.DirectionsRequest (
                                  DirectionsRequest,
                                )

-----------------------------------------
-- Includes
-----------------------------------------
import Network.URI (URI, parseURIReference)
import Data.Maybe (fromJust)
import Data.URLEncoded (importList, addToURI, URLEncoded)
import Data.Maybe (fromJust)

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

requestURI :: DirectionsRequest -> URI
requestURI r = fromJust $ addToURI (buildParams r) `fmap` (parseURIReference "http://maps.google.com/maps/api/directions/json")

toRequest :: DirectionsRequest -> Request_String
toRequest dr =  Request {
                          rqURI     = requestURI dr,
                          rqMethod  = GET,
                          rqHeaders = [],
                          rqBody    = ""
                        }
