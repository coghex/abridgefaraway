module World where
import System.Random
import Language.Haskell.Interpreter
import Data.List.Split
import Control.Monad

addn      :: Int -> Int -> Int
addn n x  =  x + n

distance :: Int -> Int -> Int -> Int -> Int
distance x1 y1 x2 y2 = do
  ((x2-x1)*(x2-x1))+((y2-y1)*(y2-y1))

seedTile :: Int -> Int -> Int -> Int -> Int -> (Int, Int) ->  (Int, Int)
seedTile s tile x1 y1 x2 t
  | ((fst t) >= 5) && (distance x1 y1 x2 (snd t) <= (s*s)) = (tile, (snd t)) 
  | ((fst t) <  5) && (distance x1 y1 x2 (snd t) <= ((s*s)-25)) = (tile, (snd t))
  | otherwise                                              = ((fst t), (snd t))

seedRow :: Int -> Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
seedRow s tile x y r = do
  (map (seedTile s tile x y (snd r)) (fst r), (snd r))

seedWorld :: Int -> Int -> Int -> Int -> [([(Int, Int)], Int)] -> [([(Int, Int)], Int)]
seedWorld s tile x y l = do
  map (seedRow s tile x y) l

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

buildList :: ([a], [a], [a], [a]) -> [(a, a, a, a)]
buildList ([],[],[],[]) = []
buildList (a:as, b:bs, c:cs, d:ds) = [(a,b,c,d)] ++ buildList (as, bs, cs, ds)

buildMap :: [([(Int, Int)], Int)] -> [(Int, Int, Int, Int)] -> [([(Int, Int)], Int)]
buildMap m []           = m
buildMap m ((a,b,c,d):xs) = do
  buildMap (seedWorld d c a b m) xs

buildWorld :: [Int] -> IO ([Int])
buildWorld m = do
  
  seed <- newStdGen
  let x = buildList $ (randomList (1, 90::Int) 10 seed, randomList (1, 60::Int) 10 seed, randomList (1, 5::Int) 10 seed, randomList (2, 10::Int) 10 seed)

  let mapexp = chunksOf 90 m
  let mapnew = zip (map workRows mapexp) [0..60]
  let map2 = buildMap mapnew x
  let map3 = stripMap (map2)
  let map4 = flattenMap map3
  return map4


randomN :: Int -> Int -> IO Int
randomN min max = do
  getStdRandom $ randomR(min, max)

