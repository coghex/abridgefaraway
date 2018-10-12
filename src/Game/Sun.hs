module Game.Sun where

import GHC.Float (double2Float)

import Game.Settings
import Game.Map



data Sun = Sun { x :: Float
               , y :: Float
               , z :: Int
               , l :: Int
               } deriving (Show, Eq)

makeSun :: Float -> Float -> Int -> Int -> Sun
makeSun x1 y1 z1 l1 = Sun { x = x1
                          , y = y1
                          , z = z1
                          , l = l1 }

moveSun :: Sun -> Integer -> Sun
moveSun s0 time = Sun { x = (x s0) + (((fromInteger(time))/(36000.0/(fromIntegral(gridw)))))
                      , y = (y s0)-- + fromIntegral(sunSeason time)
                      , z = (z s0)
                      , l = (l s0)
                      }

sunSpots :: [Float] -> Int -> Int -> Float
sunSpots sun x y = sun !! (x+(y*gridw))

theBigSpotter :: Sun -> [Float]
theBigSpotter sun = do
  let r0 = expandGrid (take (gridh*gridw) (repeat (double2Float 0.0)))
      r1 = map (sunRow sun) r0
      r2 = stripGrid r1
      r3 = flattenGrid r2
  r3

sunRow :: Sun -> ([(Float, Int)], Int) -> ([(Float, Int)], Int)
sunRow sun (s, y) = (map (sunAllSpots sun y) s, y)

sunAllSpots :: Sun -> Int -> (Float, Int) -> (Float, Int)
sunAllSpots sun y (s, x) = (spotSun sun x y, x)

spotSun :: Sun -> Int -> Int -> Float
spotSun sun x1 y1 = max r0 rb
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
        soid = ((sin (s1*t1)) + ((0.1)*(sin(3.0*s1*t1))) + (s2*t2))
        sx   = x sun - (fromIntegral x1)
        sy   = y sun - (fromIntegral y1)
        t1   = sx
        t2   = sy
        ml   = 0.2
        s1   = (2*pi) / ((fromIntegral gridw))
        s2   = 2 / ((fromIntegral gridh))

sunSeason :: Integer -> Int
sunSeason t = 1--round $ 5*(sin((2*pi)/(36000*(fromInteger(t)))))
