module Game.Volc where

import Numeric.Noise.Perlin
import Data.List (zip4, zipWith5)
import Game.State
import Game.Settings
import Game.Data
import Game.Map
import Game.Noise

volcanate :: State -> Env -> [Int] -> [Int] -> [Float]
volcanate state env g e = zipWith5 (volcSpot perlin) ijlist gcards ecards g e
  where (gn, gs, ge, gw) = cardinals g
        gcards           = zip4 gn gs ge gw
        (en, es, ee, ew) = cardinals e
        ecards           = zip4 en es ee ew
        ijlist           = zip (makeXList gridh [0..(gridh*gridw)]) (makeYList gridh [0..(gridh*gridw)])
        perl             = head $ envSeeds env
        perlin           = makePerlin perl 4 0.15 0.5
  

volcSpot :: Perlin -> (Int, Int) -> (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Int -> Int -> Float
volcSpot perlin (i, j) (gn, gs, ge, gw) (en, es, ee, ew) g e = volcScore perlin i j g gn gs ge gw e en es ee ew

volcScore :: Perlin -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Float
volcScore perlin i j g gn gs ge gw e en es ee ew = p * (calcGVolc g gn gs ge gw) * (calcEVolc e en es ee ew)
  where p = getNoise i j perlin

calcGVolc :: Int -> Int -> Int -> Int -> Int -> Float
calcGVolc g gn gs ge gw
  | ((g/=gn)&&(g/=gs)&&(g/=ge)&&(g/=gw)) = 1.0
  | ((g/=gn)&&(g/=gs)&&(g/=ge))          = 0.8
  | ((g/=gn)&&(g/=gs)         &&(g/=gw)) = 0.8
  | ((g/=gn)         &&(g/=ge)&&(g/=gw)) = 0.8
  | (         (g/=gs)&&(g/=ge)&&(g/=gw)) = 0.8
  | ((g/=gn)&&(g/=gs))                   = 0.5
  | (         (g/=gs)&&(g/=ge))          = 0.5
  | (                  (g/=ge)&&(g/=gw)) = 0.5
  | (         (g/=gs)         &&(g/=gw)) = 0.5
  | ((g/=gn)         &&(g/=ge))          = 0.5
  | ((g/=gn)                  &&(g/=gw)) = 0.5
  | ((g/=gn))                            = 0.2
  | (         (g/=gs))                   = 0.2
  | (                  (g/=ge))          = 0.2
  | (                           (g/=gw)) = 0.2
  | otherwise                            = 0.1

calcEVolc :: Int -> Int -> Int -> Int -> Int -> Float
calcEVolc e en es ee ew
  | ((end > 100) && (esd > 100) && (eed > 100) && (ewd > 100)) = 1.0
  | ((end > 100) && (esd > 100) && (eed > 100))                = 0.8
  | ((end > 100) && (esd > 100)                && (ewd > 100)) = 0.8
  | ((end > 100)                && (eed > 100) && (ewd > 100)) = 0.8
  | (               (esd > 100) && (eed > 100) && (ewd > 100)) = 0.8
  | ((end > 100) && (esd > 100))                               = 0.5
  | (               (esd > 100) && (eed > 100))                = 0.5
  | (                              (eed > 100) && (ewd > 100)) = 0.5
  | (               (esd > 100)                && (ewd > 100)) = 0.5
  | ((end > 100)                && (eed > 100))                = 0.5
  | ((end > 100)                               && (ewd > 100)) = 0.5
  | ((end > 100))                                              = 0.2
  | (               (esd > 100))                               = 0.2
  | (                              (eed > 100))                = 0.2
  | (                                             (ewd > 100)) = 0.2
  | otherwise                                                  = 0.1
  where end = abs $ en - e
        esd = abs $ es - e
        eed = abs $ ee - e
        ewd = abs $ ew - e
