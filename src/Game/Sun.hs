module Game.Sun where

import Game.Settings



data Sun = Sun { x :: Int
               , y :: Int
               , z :: Int
               , l :: Int
               } deriving (Show, Eq)

makeSun :: Int -> Int -> Int -> Int -> Sun
makeSun x1 y1 z1 l1 = Sun { x = x1
                          , y = y1
                          , z = z1
                          , l = l1 }

sunSpots :: Sun -> Int -> Int -> Float
sunSpots sun x1 y1 = max r0 rb
  where r0 = sunSpot sun x1 y1
        r  = max ra rb
        ra = max rn rs
        rb = max re rw
        rn = sunSpot sun x1 (y1 + gridh)
        rs = sunSpot sun x1 (y1 - gridh)
        re = sunSpot sun (x1 + gridw) y1
        rw = sunSpot sun (x1 - gridw) y1

sunSpot :: Sun -> Int -> Int -> Float
sunSpot sun x1 y1
  | dist < fromIntegral(radius)  = dist + ml
  | otherwise                    = dist + ml
  where dist = max 0 $ (1.0/(fromIntegral(gridh))) - soid
        soid = (cos (s2*t1) + sin (s2*t2)) / 2
        sx   = x sun - x1
        sy   = y sun - y1
        t1   = fromIntegral (sx - sy)
        t2   = fromIntegral (sx + sy)
        ml   = 0.0
        s2   = pi / (2*fromIntegral gridh)

sunSeason :: Integer -> Int
sunSeason t = round $ 10*cos((2*pi)/(3600*(fromInteger(t))))
