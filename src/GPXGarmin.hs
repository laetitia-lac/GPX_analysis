module GPXGarmin where
import Text.XML.Light
import Data.Maybe
import Data.Time
import Data.Fixed

-- supports GPX obtained from a Garmin device via connect.garmin.com

data GPX = GPX  { name :: String
                , ttype :: String
                , track :: [Trkpt]
                } deriving Show

data Trkpt = Trkpt  { lat  :: Double   -- degrees N
                    , lon  :: Double   -- degrees E
                    , ele  :: Double   -- m
                    , time :: Int      -- ms since "time zero"
                    } deriving Show

{- 
   We define a raw gps format were trackpoint information is
   still in string form. This is for internal parsing purposes only.
-}
type Gpx 
 = ( String      -- name  
   , String      -- type
   , [ ( String  -- lat
       , String  -- lon
       , String  -- ele
       , String  -- time
       )
    ]
   )

parseGarmin :: String -> GPX
parseGarmin str = gpx2GPX $ xml2gpx $ parseXML str

gpx2GPX :: Gpx -> GPX
gpx2GPX (nm,ty,pts) = GPX nm ty $ map quad2trkpt pts

quad2trkpt :: (String,String,String,String) -> Trkpt
quad2trkpt (la,lo,el,ti) = Trkpt (read la) (read lo) (read el) (str2ms ti)

-- convert time-string to milliseconds since 17th Nov 1858 !
str2ms :: String -> Int
str2ms tstr 
  = let
      ltime :: LocalTime
      ltime = parseTimeOrError True defaultTimeLocale tformat tstr
      day :: Int
      day = fromInteger $ toModifiedJulianDay $ localDay ltime
      tod = localTimeOfDay ltime
      hr = todHour tod
      min = todMin tod
      (MkFixed ps) = todSec tod
      ms :: Int
      ms = fromInteger (ps `div` 1000000000)
    in 1000*( 24*3600*day + 60*(60*hr+min) )+ms
  where tformat = iso8601DateFormat (Just "%H:%M:%S.000Z")


xml2gpx :: [Content] -> Gpx
xml2gpx contents
  = let
      trk = catMaybes $ map (filterChild $ isX "trk") $ onlyElems contents
      nm = strContent $ head $ catMaybes $ map (filterChild $ isX "name") trk
      typ = strContent $ head $ catMaybes $ map (filterChild $ isX "type") trk
      segs = elChildren $ head $ catMaybes $ map (filterChild $ isX "trkseg") trk
      pts = map elm2quad segs
   in (nm,typ,pts)

isX nm e = qName (elName e) == nm

isQ nm q = qName q == nm

elm2quad :: Element -> (String,String,String,String)
elm2quad pt
 = ( fromMaybe "" $ findAttrBy (isQ "lat") pt
   , fromMaybe "" $ findAttrBy (isQ "lon") pt
   , childString "ele" pt
   , childString "time" pt
   )

childString :: String-> Element -> String
childString nm pt
 = case filterChild (isX nm) pt of
    (Just child)  ->  strContent child 
    Nothing       ->  ""
