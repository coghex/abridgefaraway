module World where
import System.Random
import Data.Array.Repa hiding((++), repr, map)
import Data.Array.Repa.Eval
import Language.Haskell.Interpreter
import Data.List.Split

addn      :: Int -> Int -> Int
addn n x  =  x + n

distance :: Int -> Int -> Int -> Int -> Int
distance x1 y1 x2 y2 = do
  ((x2-x1)*(x2-x1))+((y2-y1)*(y2-y1))

seedTile :: Int -> Int -> Int -> Int -> (Int, Int) ->  (Int, Int)
seedTile tile x1 y1 x2 t
  | distance x1 y1 x2 (snd t) <= 500 = (tile, (snd t)) 
  | otherwise                        = ((fst t), (snd t))

seedRow :: Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
seedRow tile x y r = do
  (map (seedTile tile x y (snd r)) (fst r), (snd r))

seedWorld :: Int -> Int -> Int -> [([(Int, Int)], Int)] -> [([(Int, Int)], Int)]
seedWorld tile x y l = do
  map (seedRow tile x y) l

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

buildWorld :: Array U DIM2 Int -> IO (Array U DIM2 Int)
buildWorld m = do
  x <- randomN 90
  y <- randomN 60
  t <-  randomN 6

  let mapl = toList m
  let mapexp = chunksOf 90 mapl
  let mapnew = zip (map workRows mapexp) [0..60]
  let map2 = seedWorld t x y mapnew
  let map3 = stripMap map2
  let map4 = flattenMap map3
  let mapr = fromListUnboxed (extent m) (map (addn 0) map4)
  return mapr


randomN :: Int -> IO Int
randomN max = do
  getStdRandom $ randomR(1, max)

