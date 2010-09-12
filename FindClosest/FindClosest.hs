--TODO: figure out how to split up the utils
module FindClosest 
  (Finder)

data Finder { origin  :: Origin,
              options :: [LocationOption],
            } deriving (Show)

data Origin { address :: String,
              city    :: String,
              state   :: String,
              postal  :: String,
              country :: String,
            } deriving (Show)

newtype LocationOption { getLocationOption :: String }
