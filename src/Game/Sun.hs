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
  where dist = max 0 $ 2.0 - soid
        soid = ((sin (s1*t1)) + (s2*t2))
        sx   = x sun - x1
        sy   = y sun - y1
        t1   = fromIntegral (sx)
        t2   = fromIntegral (sy)
        ml   = 0.2
        s1   = (2*pi) / ((fromIntegral gridw))
        s2   = 2 / ((fromIntegral gridh))

sunSeason :: Integer -> Int
sunSeason t = round $ 5*cos((2*pi)/(3600*(fromInteger(t))))
