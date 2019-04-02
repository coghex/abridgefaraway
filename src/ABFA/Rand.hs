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

makeSeeds :: IO ([StdGen])
makeSeeds = do 
  s1 <- newStdGen
  s2 <- newStdGen
  s3 <- newStdGen
  s4 <- newStdGen
  s5 <- newStdGen
  s6 <- newStdGen
  return ([s1, s2, s3, s4, s5, s6])

-- evaluates a given function with a stdgen from a set list
-- if the stdgen is the last arg, which is the convention with the library
withStdGen :: [StdGen] -> Int -> (StdGen -> a) -> a
withStdGen sgs n f = f s0
  where s0 = sgs !! n

-- gives a new set of random gens to help with recursive calls using the stdgens
splitSGs :: [StdGen] -> [StdGen]
splitSGs sgs = map splitSG sgs

splitSG :: StdGen -> StdGen
splitSG sg = snd $ split sg



