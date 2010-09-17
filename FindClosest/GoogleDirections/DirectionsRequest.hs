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

import Google.DirectionsResponse (DirectionSummary, summarizeResponse, ParsedResponse, readResponse)

{-# LANGUAGE OverlappingInstances #-}

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

get :: DirectionsRequest -> IO String
get dr = do
    eresp <- simpleHTTP (toRequest dr)
    resp <- handleE (err . show) eresp
    case rspCode resp of
                      (2,0,0) -> return (rspBody resp)
                      _ -> err (httpError resp)
    where
    showRspCode (a,b,c) = map intToDigit [a,b,c]
    httpError resp = showRspCode (rspCode resp) ++ " " ++ rspReason resp

handleE :: Monad m => (ConnError -> m a) -> Either ConnError a -> m a
handleE h (Left e) = h e
handleE _ (Right v) = return v

err :: String -> IO a
err msg = do 
	  hPutStrLn stderr msg
	  exitFailure
