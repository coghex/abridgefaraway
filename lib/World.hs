module World where
import System.Random
import Language.Haskell.Interpreter
import Data.List.Split
import Control.Monad

mapsize = 10

addn      :: Int -> Int -> Int
addn n x  =  x + n

distance :: Int -> Int -> Int -> Int -> Int -> Int -> Int
distance a b x1 y1 x2 y2 = do
  let p1 = ((x2-x1)*(x2-x1))+((y2-y1)*(y2-y1))
  let p2 = ((x2-a)*(x2-a))+((y2-b)*(y2-b))
  p1*p2

seedTile :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int) ->  (Int, Int)
seedTile a b s tile x1 y1 x2 t
  | ((fst t) >= 5) && (distance a b x1 y1 x2 (snd t) <= (s*s*s*s)) = (tile, (snd t)) 
  | ((fst t) <  5) && (distance a b x1 y1 x2 (snd t) <= (s*s*s*s)) = (tile, (snd t))
  | otherwise                                              = ((fst t), (snd t))

seedRow :: Int -> Int -> Int -> Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
seedRow a b s tile x y r = do
  (map (seedTile a b s tile x y (snd r)) (fst r), (snd r))

seedWorld :: Int -> Int -> Int -> Int -> Int -> Int -> [([(Int, Int)], Int)] -> [([(Int, Int)], Int)]
seedWorld a b s tile x y l = do
  map (seedRow a b s tile x y) l

workRows :: [Int] -> [(Int,Int)]
workRows l = do
  zip l [0..90]

stripRow :: [(Int, Int)] -> [Int]
stripRow ((a,b):ys) = a : stripRow ys
stripRow _          = []

stripMap :: [([(Int, Int)], Int)] -> [[Int]]
stripMap ((a,b):ys) = (stripRow a) : stripMap ys
stripMap _          = [[]]

flattenMap :: [[Int]] -> [Int]
flattenMap xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

randomList :: (Random a) => (a,a) -> Int -> StdGen -> [a]
randomList bnds n = take n . randomRs bnds

buildList :: ([a], [a], [a], [a], [a], [a]) -> [(a, a, a, a, a, a)]
buildList ([], [], [], [], [], []) = []
buildList (a:as, b:bs, c:cs, d:ds, e:es, f:fs) = [(a, b, c, d, e, f)] ++ buildList (as, bs, cs, ds, es, fs)

buildMap :: [([(Int, Int)], Int)] -> [(Int, Int, Int, Int, Int, Int)] -> [([(Int, Int)], Int)]
buildMap m []           = m
buildMap m ((a, b, c, d, e, f):xs) = do
  buildMap (seedWorld a b f e c d m) xs

buildWorld :: [Int] -> IO ([Int])
buildWorld m = do
  
  seed <- newStdGen
  let x = buildList $ (randomList (2, 90::Int) mapsize seed, randomList (2, 60::Int) mapsize seed, randomList (1, 90::Int) mapsize seed, randomList (1, 60::Int) mapsize seed, randomList (1, 4::Int) mapsize seed, randomList (10, 20::Int) mapsize seed)

  let mapexp = chunksOf 90 m
  let mapnew = zip (map workRows mapexp) [0..60]
  let map2 = buildMap mapnew x
  let map3 = stripMap (map2)
  let map4 = flattenMap map3
  return map4


randomN :: Int -> Int -> IO Int
randomN min max = do
  getStdRandom $ randomR(min, max)

