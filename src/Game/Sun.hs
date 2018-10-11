module Game.Sun where

import GHC.Float (double2Float)

import Game.Settings
import Game.Map



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
        soid = ((sin (s1*t1)) + (s2*t2))
        sx   = x sun - x1
        sy   = y sun - y1
        t1   = fromIntegral (sx)
        t2   = fromIntegral (sy)
        ml   = 0.2
        s1   = (2*pi) / ((fromIntegral gridw))
        s2   = 2 / ((fromIntegral gridh))

sunSeason :: Integer -> Int
sunSeason t = round $ 5*sin((2*pi)/(36000*(fromInteger(t))))
