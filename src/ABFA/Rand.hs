module ABFA.Rand where

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

newSeeds :: ([StdGen])
newSeeds = [s1, s2, s3, s4, s5, s6]
  where s1 = mkStdGen 1
        s2 = mkStdGen 2
        s3 = mkStdGen 3
        s4 = mkStdGen 4
        s5 = mkStdGen 5
        s6 = mkStdGen 6

-- evaluates a given function with a stdgen from a set list
-- if the stdgen is the last arg, which is the convention with the library
withStdGen :: [StdGen] -> Int -> (StdGen -> a) -> a
withStdGen sgs n f = f s0
  where s0 = sgs !! (n-1)

-- gives a new set of random gens to help with recursive calls using the stdgens
splitSGs :: [StdGen] -> [StdGen]
splitSGs sgs = map splitSG sgs

splitSG :: StdGen -> StdGen
splitSG sg = snd $ split sg



