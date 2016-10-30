module World where
import System.Random
import Language.Haskell.Interpreter
import Data.List.Split
import Control.Monad

addn      :: Int -> Int -> Int
addn n x  =  x + n

distance :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
distance tile a b c d x1 y1 x2 y2 = do
  let t = tile+1
  let p1 = ((x2-x1)*(x2-x1))+((y2-y1)*(y2-y1))
  let p2 = ((x2-a)*(x2-a))+((y2-b)*(y2-b))
  let p3 = ((x2-c)*(x2-c))+((y2-d)*(y2-d))
  p1*p2*t*t

seedTile :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int) ->  (Int, Int)
seedTile a b c d s tile x1 y1 x2 t
  | (distance tile a b c d x1 y1 x2 (snd t) <= (50*s*s)) = (tile, (snd t)) 
  | otherwise                                              = ((fst t), (snd t))

seedRow :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
seedRow a b c d s tile x y r = do
  (map (seedTile a b c d s tile x y (snd r)) (fst r), (snd r))

seedWorld :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [([(Int, Int)], Int)] -> [([(Int, Int)], Int)]
-- seedWorld a b c d s 0    x y l = l
seedWorld a b c d s 4    x y l = l
-- seedWorld a b c d s 5    x y l = l
seedWorld a b c d s tile x y l = do
  map (seedRow a b c d s tile x y) l

workRows :: [Int] -> [(Int,Int)]
workRows l = do
  zip l [0..120]

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

buildList :: ([a], [a], [a], [a], [a], [a], [a], [a]) -> [(a, a, a, a, a, a, a, a)]
buildList ([], [], [], [], [], [], [], []) = []
buildList (a:as, b:bs, c:cs, d:ds, e:es, f:fs, g:gs, h:hs) = [(a, b, c, d, e, f, g, h)] ++ buildList (as, bs, cs, ds, es, fs, gs, hs)

buildMap :: [([(Int, Int)], Int)] -> [(Int, Int, Int, Int, Int, Int, Int, Int)] -> [([(Int, Int)], Int)]
buildMap m []           = m
buildMap m ((a, b, c, d, e, f, g, h):xs) = do
  buildMap (seedWorld a b c d h g e f m) xs

buildWorld :: [Int] -> Int -> Int -> IO ([Int])
buildWorld m y1 y2 = do
  mapsize <- randomN 10 50 
  seed <- newStdGen
  let x = buildList $ (randomList ((y1-10), (y1+10)) mapsize seed, randomList ((y2-10), (y2+10)) mapsize seed, randomList ((y1-11), (y1+11)) mapsize seed, randomList ((y2-12), (y2+12)) mapsize seed, randomList ((y1-13), (y1+13)) mapsize seed, randomList ((y2-14), (y2+14)) mapsize seed, randomList (0, 5::Int) mapsize seed, randomList (5, 25::Int) mapsize seed)

  let mapexp = chunksOf 120 m
  let mapnew = zip (map workRows mapexp) [0..90]
  let map2 = buildMap mapnew x

  let map3 = stripMap (map2)
  let map4 = flattenMap map3
  return map4


randomN :: Int -> Int -> IO Int
randomN min max = do
  getStdRandom $ randomR(min, max)
