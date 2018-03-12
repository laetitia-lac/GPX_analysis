module Haversine where


{-

 the Haversine formula
 adapted from Data.Geo.Geodetic.Haversine.haversine

-}

-- first, the 'hav' function:
hav :: Double -> Double
hav theta = sqr (sin (theta / 2))

sqr n = n * n

-- we start assuming a radius of one, and radian coordinates
haversine ::  (Double, Double)  -- 1st coord (radians)
          ->  (Double, Double)  -- 2nd coord (radians)
          ->  Double            -- great circle distance (radians)
haversine (phi1,lambda1) (phi2,lambda2)
  = 2 * asin (sqrt hbody)
  where hbody = hav (phi2-phi1) + cos phi1 * cos phi2 * hav (lambda2 - lambda1)

-- now one for the earth, using degrees
eRadius :: Double
eRadius = 6371e3 -- metres (m)

deg2rad :: Double -> Double
deg2rad d = pi * d / 180

cdeg2rad (lat,lon) = (deg2rad lat, deg2rad lon)

eHaversine :: (Double, Double)  -- 1st coord - latitude, longitude (degrees)
           -> (Double, Double)  -- 2nd coord - latitude, longitude (degrees)
           -> Double            -- great circle distance (m)
eHaversine pt1 pt2
 = eRadius * haversine (cdeg2rad pt1) (cdeg2rad pt2)
