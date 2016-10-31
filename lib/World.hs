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
-- for rows: seedTile _ _ _ _ _ _    _  _  _  (a, 0) = (7, 0)
seedTile _ _ _ _ _ _    _  _  0  (a, i) = (7, i)
seedTile _ _ _ _ _ _    _  _  1  (a, i) = (7, i)
seedTile _ _ _ _ _ _    _  _  2  (a, i) = (7, i)
seedTile _ _ _ _ _ _    _  _  88 (a, i) = (7, i)
seedTile _ _ _ _ _ _    _  _  89 (a, i) = (7, i)
seedTile a b c d s tile x1 y1 x2 t
  | (distance tile a b c d x1 y1 x2 (snd t) <= (50*s*s)) = (tile, (snd t)) 
  | otherwise                                              = ((fst t), (snd t))

seedRow :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
seedRow a b c d s tile x y r = do
  (map (seedTile a b c d s tile x y (snd r)) (fst r), (snd r))

seedWorld :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [([(Int, Int)], Int)] -> [([(Int, Int)], Int)]
-- seedWorld a b c d s 0    x y l = l
seedWorld a b c d s 4    x y l = l
seedWorld a b c d s 6    x y l = l
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

makeCoords :: [Int] -> [(Int, Int)] -> [(Int, Int)] -> IO ([Int])
makeCoords m []     _      = return m
makeCoords m (l:ls) (k:ks) = do
  m2 <- buildWorld m (fst k) (snd k) (fst l) (snd l)
  makeCoords m2 ls ks

makeIce :: [Int] -> IO [Int]
makeIce m = do
  let mapexp = chunksOf 120 m
  let mapnew = zip (map workRows mapexp) [0..90]
  mapsize <- randomN 250 500
  seed <- newStdGen
  let x = buildList $ (randomList (2, 10::Int) mapsize seed, randomList (0, 120::Int) mapsize seed, randomList (1, 9::Int) mapsize seed, randomList (1, 119::Int) mapsize seed, randomList (0, 5::Int) mapsize seed, randomList (2, 118::Int) mapsize seed, randomList (7, 7::Int) mapsize seed, randomList (5, 10::Int) mapsize seed)

  let map0 = buildMap mapnew x
  let y = buildList $ (randomList (82, 90::Int) mapsize seed, randomList (0, 120::Int) mapsize seed, randomList (81, 89::Int) mapsize seed, randomList (1, 119::Int) mapsize seed, randomList (85, 88::Int) mapsize seed, randomList (2, 118::Int) mapsize seed, randomList (7, 7::Int) mapsize seed, randomList (5, 10::Int) mapsize seed)
  let map0a = buildMap map0 y
  let map1 = stripMap map0a
  let map2 = flattenMap map1
  return map2

buildWorld :: [Int] -> Int -> Int -> Int -> Int -> IO ([Int])
buildWorld m s1 s2 y1 y2 = do
  mapsize <- randomN 10 50 
  seed <- newStdGen
  let x = buildList $ (randomList ((y1-s1), (y1+s1)) mapsize seed, randomList ((y2-s2), (y2+s2)) mapsize seed, randomList ((y1-s1-1), (y1+s1+1)) mapsize seed, randomList ((y2-s2-2), (y2+s2+2)) mapsize seed, randomList ((y1-s1-3), (y1+s1+3)) mapsize seed, randomList ((y2-s2-4), (y2+s2+4)) mapsize seed, randomList (0, 5::Int) mapsize seed, randomList (5, 25::Int) mapsize seed)

  let mapexp = chunksOf 120 m
  let mapnew = zip (map workRows mapexp) [0..90]
  let map2 = buildMap mapnew x

  let map3 = stripMap (map2)
  let map4 = flattenMap map3
  return map4


randomN :: Int -> Int -> IO Int
randomN min max = do
  getStdRandom $ randomR(min, max)
