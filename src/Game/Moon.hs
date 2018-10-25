module Game.Moon where

import GHC.Float (double2Float)

import Game.Settings
import Game.Map



data Moon = Moon { x :: Float
                 , y :: Float
                 , z :: Int
                 , l :: Int
                 } deriving (Show, Eq)

makeMoon :: Float -> Float -> Int -> Int -> Moon
makeMoon x1 y1 z1 l1 = Moon { x = x1
                            , y = y1
                            , z = z1
                            , l = l1 }

moveMoon :: Moon -> Integer -> Moon
moveMoon m0 time = Moon { x = (((fromInteger(time))/(1387.3/(fromIntegral(gridw)))))+(sin(2.0*pi*(fromInteger time)/((360.0*1440.0)*(fromIntegral gridw))))
                        , y = ((fromIntegral(gridh))/2)+(fromIntegral(gridh)/30.0)*(sin(2.0*pi*(fromInteger time)/((360.0*1440.0)*(fromIntegral gridw))))

                        , z = (z m0)
                        , l = (l m0)
                        }

moonSpots :: [Float] -> Int -> Int -> Float
moonSpots moon x y = moon !! (x+(y*gridw))

moonSpotter :: Moon -> [Float]
moonSpotter moon = do
  let r0 = expandGrid (take (gridh*gridw) (repeat (double2Float 0.0)))
      r1 = map (moonRow moon) r0
      r2 = stripGrid r1
      r3 = flattenGrid r2
  r3

moonRow :: Moon -> ([(Float, Int)], Int) -> ([(Float, Int)], Int)
moonRow moon (s, y) = (map (moonAllSpots moon y) s, y)

moonAllSpots :: Moon -> Int -> (Float, Int) -> (Float, Int)
moonAllSpots moon y (s, x) = (spotMoon moon x y, x)

spotMoon :: Moon -> Int -> Int -> Float
spotMoon moon x1 y1 = max r0 rb
  where r0 = moonSpot moon x1 y1
        r  = max ra rb
        ra = max rn rs
        rb = max re rw
        rn = moonSpot moon x1 (y1 + 2*gridh)
        rs = moonSpot moon x1 (y1 - 2*gridh)
        re = moonSpot moon (x1 + 2*gridw) y1
        rw = moonSpot moon (x1 - 2*gridw) y1

moonSpot :: Moon -> Int -> Int -> Float
moonSpot moon x1 y1 = min (dist + 0.4) 0.7
  where dist  = max 0 $ (0.25-((td1*soid1)+(td2*soid2)))
        soid1 = (((r1)*(sin (s1*t1)))+((r2)*(sin (s2*t2))))
        soid2 = ((0.8)*(cos (2*s1*sx))+((s2*sy)))
        sx    = x moon - ((fromIntegral ((x1))))
        sy    = y moon - ((fromIntegral ((y1))))
        t1    = 2*sx+sy
        t2    = 2*sx-sy
        td1   = abs $ cos ((pi/4)+(s1*(x moon)/(354.4)))
        td2   = sin ((pi/4)+(s1*(x moon)/(354.4)))
        s1    = (pi) / (fromIntegral gridw)
        s2    = (pi) / (fromIntegral gridw)
        r1    = 0.1
        r2    = 0.1





