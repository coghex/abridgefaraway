module Game.Rand where

import System.Random
import Game.Settings

randomList :: (Random a) => (a,a) -> Int -> StdGen -> [a]
randomList bnds n = do
  take n . randomRs bnds

randomN :: Int -> Int -> IO Int
randomN min max = do
  getStdRandom $ randomR (min, max)

buildList2 :: ([a], [a]) -> [(a, a)]
buildList2 ([], [])     = []
buildList2 (a:as, b:bs) = [(a, b)] ++ buildList2 (as, bs)

makeSeeds :: [(Int, Int)] -> Int -> StdGen -> StdGen -> [Int] -> [[(Int, Int)]]
makeSeeds []         _ _  _  _               = []
makeSeeds ((x,y):xs) r s1 s2 (nspot:nspots) = (buildList2 ((randomList ((x-fudge), (x+fudge)) (nspot) s1), (randomList ((y-fudge),(y+fudge)) (nspot) s2))):(makeSeeds xs (r+1) (mkStdGen (r+1)) (mkStdGen (r-1)) nspots)

newRandomList :: [Int] -> IO ([[Int]])
newRandomList []      = return []
newRandomList (i:is)  = do
  sg <- newStdGen
  rl <- newRandomList is
  return $ (randomList (0, npaths) pathlen sg) : (rl)
