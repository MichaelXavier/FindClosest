-----------------------------------
--- Includes
-----------------------------------
import Text.JSON

import Data.Maybe (fromJust)

import Text.Printf

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-----------------------------------
--- Relevant types
-----------------------------------
data ParsedResponse = ParsedResponse {
                      routes::[Route]
                    } deriving (Eq, Show)

instance JSON ParsedResponse where
  showJSON r = makeObj
    [ ("routes", showJSON $ map showJSON $ routes r) ]

  readJSON (JSObject obj) = let
      hash = fromJSObject obj
    in do
      r_routes <- mLookup "routes" hash >>= readJSON
      return $ ParsedResponse { routes = r_routes }

data Route = Route {
                     legs::[Leg]
                   } deriving (Eq, Show)

instance JSON Route where
  showJSON r = makeObj
    [ ("legs", showJSON $ map showJSON $ legs r) ]

  readJSON (JSObject obj) = let
      hash = fromJSObject obj
    in do
      r_legs <- mLookup "legs" hash >>= readJSON
      return $ Route { legs = r_legs }


data Leg = Leg {
                 duration::Int,
                 distance::Int,
                 endAddress::String
               } deriving (Eq, Show)

--TODO: this can do applicable?
--TODO is showJSON even necessary
instance JSON Leg where
  showJSON l = makeObj 
    [ ("duration", showJSON $ duration l),
      ("distance", showJSON $ distance l),
      ("end_address", showJSON $ endAddress l) ]

      --TODO: can't get the types to work out here
  readJSON (JSObject obj) = let
      hash = fromJSObject obj
      value = mLookup "value" hash >>= readJSON
    in do
      r_duration    <- mLookup "duration" value >>= readJSON
      r_distance    <- mLookup "distance" value >>= readJSON
      r_end_address <- mLookup "end_address" value >>= readJSON
      return $ Leg 
        { duration   = r_duration,
          distance   = r_distance,
          endAddress = r_end_address }

  readJSON _ = fail ""

-----------------------------------
--- Helpers
-----------------------------------
mLookup :: (Monad m) => String -> [(String, a)] -> m a
mLookup a as = maybe (fail $ "No such element: " ++ a) return (lookup a as)

