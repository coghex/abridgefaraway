module Game.Trees where

import Data.List.Split (chunksOf)
import Game.State
import Game.Settings

initTrees :: [(Int, Int)] -> [Int] -> [Int]
initTrees strs z = do
  let trees = (take (zonew*zoneh) (repeat 0))
  let t2 = makeTrees strs trees
  removeTrees t2 z

removeTrees :: [Int] -> [Int] -> [Int]
removeTrees trees z = zipWith removeTreeFold trees z

removeTreeFold :: Int -> Int -> Int
removeTreeFold tree tile
  | (tile <= 10) = 0
  | otherwise    = tree

makeTrees :: [(Int, Int)] -> [Int] -> [Int]
makeTrees m trees = do
  let z0 = expandTrees trees
  let z1 = foldl (seedTrees) z0 m
  let z2 = stripTrees z1
  flattenTrees z2

stripTrees :: [([(Int, Int)], Int)] -> [[Int]]
stripTrees ((a, b):ys) = (stripRow a) : stripTrees ys
stripTrees _           = [[]]

stripRow :: [(Int, Int)] -> [Int]
stripRow ((a, b):ys) = a : stripRow ys
stripRow _           = []

flattenTrees :: [[Int]] -> [Int]
flattenTrees xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

expandTrees :: [Int] -> [([(Int, Int)], Int)]
expandTrees m = zip (map workRows (chunksOf zonew m)) [0..zoneh]

workRows :: [Int] -> [(Int, Int)]
workRows l = do
  zip l [0..zonew]

seedTrees :: [([(Int, Int)], Int)] -> (Int, Int) -> [([(Int, Int)], Int)]
seedTrees z (x, y) = map (seedTreeRow x y) z

seedTreeRow :: Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
seedTreeRow x y (z, j) = (map (seedTreeSpot x y j) z, j)

seedTreeSpot :: Int -> Int -> Int -> (Int, Int) -> (Int, Int)
seedTreeSpot x y j (2, i) = (0, i)
seedTreeSpot x y j (t, i)
  | (x==i)&&(y==j) = (1, i)
  | otherwise      = (t, i)
