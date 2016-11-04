module Rand where

import System.Random

randomList :: (Random a) => (a,a) -> Int -> StdGen -> [a]
randomList bnds n = do
  take n . randomRs bnds

randomN :: Int -> Int -> IO Int
randomN min max = do
  getStdRandom $ randomR (min, max)

buildList2 :: ([a], [a]) -> [(a, a)]
buildList2 ([], [])     = []
buildList2 (a:as, b:bs) = [(a, b)] ++ buildList2 (as, bs)
