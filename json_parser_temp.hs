-----------------------------------
--- Imports
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
                 duration::TextValue,
                 distance::TextValue,
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
    in do
      r_duration    <- mLookup "duration" hash >>= readJSON
      r_distance    <- mLookup "distance" hash >>= readJSON
      r_end_address <- mLookup "end_address" hash >>= readJSON
      return $ Leg 
        { duration   = r_duration,
          distance   = r_distance,
          endAddress = r_end_address }

  readJSON _ = fail ""

data TextValue = TextValue {
                             text::String,
                             value::Int
                           } deriving (Eq, Show)

instance JSON TextValue where
  showJSON t = makeObj
    [ ("text", showJSON $ text t),
      ("value", showJSON $ value t) ]

  readJSON (JSObject obj) = let
      hash = fromJSObject obj
    in do
      r_text <- mLookup "text" hash >>= readJSON
      r_value <- mLookup "value" hash >>= readJSON
      return $ TextValue { text = r_text, value = r_value }

-----------------------------------
--- Helpers
-----------------------------------
mLookup :: (Monad m) => String -> [(String, a)] -> m a
mLookup a as = maybe (fail $ "No such element: " ++ a) return (lookup a as)

