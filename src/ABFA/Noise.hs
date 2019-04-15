module ABFA.Noise where

import Numeric.Noise.Perlin

makePerlin :: Int -> Int -> Double -> Double -> Perlin
makePerlin seed octaves scale persistance = perlin seed octaves scale persistance

getNoise :: Int -> Int -> Perlin -> Float
getNoise x y p = realToFrac $ noiseValue p (xf, yf, 0.0)
  where xf = fromIntegral x
        yf = fromIntegral y
