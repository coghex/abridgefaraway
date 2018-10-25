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
                        , y = (y m0)
                        , z = (z m0)
                        , l = (l m0)
                        }

moonSpots :: [Float] -> Int -> Int -> Float
moonSpots moon x y = moon !! (x+(y*gridw))
