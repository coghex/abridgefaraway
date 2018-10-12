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
moveSun s0 time = Sun { x = (((fromInteger(time))/(36000.0/(fromIntegral(gridw)))))
                      , y = (y s0)--sunSeason time
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
sunSpot sun x1 y1 = dist + ml
  where dist  = max 0 $ (soid2)
        soid1 = (td)*(((r1)*(sin (s1*t1)))+((r2)*(sin (s2*t2))))
        soid2 = (1-td)*(((2)*(sin (s1*sx))+(s2*sy)))
        sx    = x sun - ((fromIntegral ((x1))))
        sy    = y sun - ((fromIntegral ((y1))))
        t1    = sx+sy 
        t2    = sx-sy
        td    = ((sin((t1)*((fromIntegral(gridw))/36000.0)))+1)/2
        ml    = 0.5
        s1    = (pi) / ((fromIntegral gridw))
        s2    = (pi) / ((fromIntegral gridw))
        r1    = 1
        r2    = 1

sunSeason :: Integer -> Float
sunSeason 0 = ((fromIntegral(gridh))+((fromIntegral(gridh))/3.0))
sunSeason t = ((fromIntegral(gridh))+((fromIntegral(gridh))/3.0)) + (((fromIntegral(gridh))/(8))*(sin ((2.0*pi*fromInteger(t))/(36.0))))
