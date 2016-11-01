module World where
import System.Random
import Language.Haskell.Interpreter
import Data.List.Split
import Control.Monad
import Settings
import State

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
seedTile _ _ _ _ _ _    _  _  0        (a, i) = (7, i)
seedTile _ _ _ _ _ _    _  _  1        (a, i) = (7, i)
seedTile _ _ _ _ _ _    _  _  2        (a, i) = (7, i)
seedTile a b c d s tile x1 y1 x2 t
  | ((x2==(maph-1))||(x2==(maph-2)))                                   = (7, (snd t))
  | (distance tile a b c d x1 y1 x2 (snd t) <= (500)) = (tile, (snd t)) 
  | otherwise                                            = ((fst t), (snd t))

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
  zip l [0..mapw]

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

makeState :: WorldArgs -> IO WorldArgs
makeState (WorldArgs pos ran siz m win texs) = do
  m2 <- makeCoords m pos siz
  let x = WorldArgs pos ran siz m2 win texs
  return x

makeCoords :: [Int] -> [(Int, Int)] -> [(Int, Int)] -> IO ([Int])
makeCoords m []     _      = return m
makeCoords m (l:ls) (k:ks) = do
  m2 <- buildWorld m (fst k) (snd k) (fst l) (snd l)
  makeCoords m2 ls ks

isCoast :: [([(Int, Int)], Int)] -> Int -> Int -> Int -> Bool
isCoast _ 0 _ _    = False
isCoast _ 1 _ _    = False
isCoast _ 2 _ _    = False
isCoast _ 3 _ _    = False
isCoast _ 4 _ _    = False
isCoast l 5 i j    | (j==mapw) = False
                   | otherwise = (if (fst ((fst (l !! (i+1)) !! (j)))) == 7 then True else False)
isCoast _ 6 _ _    = False
isCoast _ 7 _ _    = False
isCoast l a i j    = False

coastRow :: [([(Int, Int)], Int)] -> Int -> (Int, Int) -> (Int, Int)
coastRow m i (a, j) | (isCoast m a i j) = (6, j)
                    | otherwise         = (a, j)

buildCoast :: [([(Int, Int)], Int)] -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
buildCoast mapa (m, a) = (map (coastRow mapa a) m, a)

findRiver :: [([(Int, Int)], Int)] -> (Int, Int) -> (Int, Int, Bool)
findRiver _ (0, y) = (0, y, False)
findRiver _ (x, 0) = (x, 0, False)
findRiver m (x, y) = if (fst ((fst (m !! (x-1))) !! (y-1)) == 5) then (x, y, False) else (x, y, True)

makeRiver :: [Int] -> Int -> IO [Int]
makeRiver l s = do
  seed <- newStdGen
  let rx = randomList (0, maph) s seed
  let ry = randomList (0, mapw) s seed
  let r = zip rx ry
  let mapexp = chunksOf mapw l
  let mapnew = zip (map workRows mapexp) [0..maph]
  let riverb = map (findRiver mapnew) r
  let map1 = stripMap mapnew
  let map2 = flattenMap map1

  return map2

makeCoast :: WorldArgs -> IO WorldArgs
makeCoast (WorldArgs pos ran siz m win texs) = do
  let mapexp = chunksOf mapw m
  let mapnew = zip (map workRows mapexp) [0..maph]
  let map0 = map (buildCoast mapnew) mapnew
  let map1 = stripMap map0
  let map2 = flattenMap map1
  return (WorldArgs pos ran siz map2 win texs)

makeIce :: WorldArgs -> IO WorldArgs
makeIce (WorldArgs pos ran siz m win texs)  = do
  let mapexp = chunksOf mapw m
  let mapnew = zip (map workRows mapexp) [0..maph]
  mapsize <- randomN 250 500
  seed <- newStdGen
  let x = buildList $ (randomList (2, 10::Int) mapsize seed, randomList (0, mapw) mapsize seed, randomList (1, 9::Int) mapsize seed, randomList (1, (mapw-1)) mapsize seed, randomList (0, 5::Int) mapsize seed, randomList (2, (mapw-2)) mapsize seed, randomList (ntiles, ntiles) mapsize seed, randomList (5, 10::Int) mapsize seed)

  let map0 = buildMap mapnew x
  let y = buildList $ (randomList ((maph-8), maph) mapsize seed, randomList (0, mapw) mapsize seed, randomList ((maph-9), (maph-1)) mapsize seed, randomList (1, (mapw-1)) mapsize seed, randomList ((maph-5), (maph-2)) mapsize seed, randomList (2, (mapw-2)) mapsize seed, randomList (ntiles, ntiles) mapsize seed, randomList (5, 10::Int) mapsize seed)
  let map0a = buildMap map0 y
  let map1 = stripMap map0a
  let map2 = flattenMap map1
  return (WorldArgs pos ran siz map2 win texs)

buildWorld :: [Int] -> Int -> Int -> Int -> Int -> IO ([Int])
buildWorld m s1 s2 y1 y2 = do
  mapsize <- randomN 10 50 
  seed <- newStdGen
  let x = buildList $ (randomList ((y1-s1), (y1+s1)) mapsize seed, randomList ((y2-s2), (y2+s2)) mapsize seed, randomList ((y1-s1-1), (y1+s1+1)) mapsize seed, randomList ((y2-s2-2), (y2+s2+2)) mapsize seed, randomList ((y1-s1-3), (y1+s1+3)) mapsize seed, randomList ((y2-s2-4), (y2+s2+4)) mapsize seed, randomList (0, 5::Int) mapsize seed, randomList (5, 25::Int) mapsize seed)

  let mapexp = chunksOf mapw m
  let mapnew = zip (map workRows mapexp) [0..maph]
  let map2 = buildMap mapnew x

  let map3 = stripMap (map2)
  let map4 = flattenMap map3
  
  makeRiver map4 mapsize

changeSpot :: Int -> Int -> Int -> Int -> (Int, Int) -> (Int, Int)
changeSpot x y j t (a, i) | ((x==i) && (y==j)) = (t, i)
                          | otherwise          = (a, i)

changeRow :: Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
changeRow x y t m = ((map (changeSpot x y (snd m) t) (fst m)), (snd m))

changeTile :: [Int] -> (Int, Int) -> Int -> IO ([Int])
changeTile m (x, y) t = do
  let mapexp = chunksOf mapw m
  let mapnew = zip (map workRows mapexp) [0..maph]
  let map0 = map (changeRow x y t) mapnew
  let map1 = stripMap map0
  let map2 = flattenMap map1
  return (map2)

randomN :: Int -> Int -> IO Int
randomN min max = do
  getStdRandom $ randomR(min, max)
