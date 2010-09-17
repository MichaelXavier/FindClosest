import Google.DirectionsRequest (get)
import Google.DirectionsResponse (summarizeResponse, readResponse)

main :: IO ()
main =  do
-- ... do stuff here
  map printStrLn $ rankClosest requests

getOrigin :: IO String 
getOrigin = do -- ...

getDestination :: IO String
--...

rankClosest :: [DirectionsRequest] -> [DirectionsSummary]
rankClosest :: (sort . fmap) (summarizeResponse readResponse . get)
