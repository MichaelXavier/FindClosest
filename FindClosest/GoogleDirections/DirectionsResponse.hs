module Google.DirectionsResponse (
                                   DirectionSummary,
                                   summarizeResponse,
                                   ParsedResponse
                                 )

-----------------------------------
--- Imports
-----------------------------------
import Data.Maybe (fromJust)
import Text.Printf

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-----------------------------------
--- Public API
-----------------------------------
data DirectionsSummary = DirectionsSummary {
                                             address::String,
                                             duration::Seconds,
                                             distance::Meters,
                                           }
instance Ord DirectionsSummary where
  compare = compare . duration

instance Show DirectionsSummary where
  show = intersperse "\n" $ zipWith ((++) . show) ["Location: ", "Duration: ", "Distance"]

-- figure out how to decode and lift the response out of the Result
readResponse :: String -> ParsedResponse
readResponse = fmap . decode --FIXME: yeah right like its going to let me do that

summarizeResponse :: ParsedResponse -> DirectionsSummary
summarizeResponse r = DirectionsSummary {
                                          address = l_endAddress . last . legs route,
                                          duration = totalDuration route,
                                          distance = totalDistance route
                                        }
    where route = min $ routes r

-- Unit Conversions
newtype Meters = Meters { meters::Integer } deriving (Num)

instance Show Meters where
  show = (printf "%.2fmi") . toMiles

toMiles :: Meters -> Double
toMiles = (*0.000621371192) . fromIntegral . meters

newtype Seconds = Seconds { seconds::Integer } deriving (Num)

toHrsMins :: Seconds -> (Integer, Integer)
toHrsMins = ((flip divMod) 60) . mins . seconds
    where mins = ((flip div) 60)

instance Show Seconds where
  show s = printf "%dh%dm" (fst hm) (snd hm)
    where hm = toHrsMins s


-----------------------------------
--- JSON Types
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

totalDuration :: Route -> Integer
totalDuration  = (foldl1 ((+) . l_duration)) . legs

totalDistance :: Route -> Integer
totalDistance  = (foldl1 ((+) . l_distance)) . legs

instance JSON Route where
  showJSON r = makeObj
    [ ("legs", showJSON $ map showJSON $ legs r) ]

  readJSON (JSObject obj) = let
      hash = fromJSObject obj
    in do
      r_legs <- mLookup "legs" hash >>= readJSON
      return $ Route { legs = r_legs }

instance Ord Route where
  compare = compare . totalDuration

data Leg = Leg {
                 l_duration::TextValue,
                 l_distance::TextValue,
                 l_endAddress::String
               } deriving (Eq, Show)

instance JSON Leg where
  showJSON l = makeObj 
    [ ("duration", showJSON $ l_duration l),
      ("distance", showJSON $ l_distance l),
      ("end_address", showJSON $ l_endAddress l) ]

  readJSON (JSObject obj) = let
      hash = fromJSObject obj
    in do
      r_duration    <- mLookup "duration" hash >>= readJSON
      r_distance    <- mLookup "distance" hash >>= readJSON
      r_end_address <- mLookup "end_address" hash >>= readJSON
      return $ Leg 
        { l_duration   = r_duration,
          l_distance   = r_distance,
          l_endAddress = r_end_address }

  readJSON _ = fail ""

data TextValue = TextValue {
                             text::String,
                             value::Integer
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
-- Courtesy of the extremely helpful article at:
-- http://therning.org/magnus/archives/719
mLookup :: (Monad m) => String -> [(String, a)] -> m a
mLookup a as = maybe (fail $ "No such element: " ++ a) return (lookup a as)
