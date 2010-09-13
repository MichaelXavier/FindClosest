import Network.HTTP
import Network.URI (URI, parseURIReference)
import Network.Stream (ConnError)

import Text.JSON
import Text.Printf

import Data.URLEncoded (importList, addToURI, URLEncoded)
import Data.Maybe (fromJust)
import Data.Char (intToDigit)
import Data.List (find)

import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)


{-# LANGUAGE OverlappingInstances, GeneralizedNewtypeDeriving #-}

---------------------------------
--- Request stuff
---------------------------------
data DirectionsRequest = DirectionsRequest {
                           origin :: String,
                           destination :: String
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

--FIXME: figure out type from ghci
toRequest :: DirectionsRequest -> Request_String
toRequest dr =  Request {
                          rqURI     = requestURI dr,
                          rqMethod  = GET,
                          rqHeaders = [],
                          rqBody    = ""
                        }

parseJSON :: String -> [(String, String)]
parseJSON s = case obj of
                (Ok v) -> v
                (Error e) -> error e
              where obj = fmap fromJSObject . decode $ s :: Result [(String, String)]

-- lots of copypasta going on here
err :: String -> IO a
err msg = do 
	  hPutStrLn stderr msg
	  exitFailure

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

---------------------------------
--- Response stuff
---------------------------------
newtype DirectionsResponse = DirectionsResponse { locations :: [Location] }

newtype Interval = Interval { seconds :: Int } deriving (Num)

instance Show Interval where
  show (Interval s) = printf "%dh%m" (fst hoursMins) (snd hoursMins)
                      where hoursMins = divMod 60 s

newtype Miles = Miles { feet :: Int } deriving (Num)

instance Show Miles where
  show (Miles f) = printf "%.1f" (f / 5280)

data Location = Location {
                           distance :: Miles,
                           duration :: Interval,
                           name :: String
                         } deriving (Show)

--FIXME: the whole point was to take a list, damnit
doIt :: String -> String -> IO [(String, String)]
doIt o d = fmap parseJSON (get (DirectionsRequest o d))

toResponse :: [(String, String)] -> DirectionsResponse
toResponse = DirectionsResponse { locations = parseLocations}
             where parseLocations = fromJust $ lookup 
