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

workrow :: [Int] -> Int -> Int -> Int -> Int -> [Int]
workrow l i x y t | (i <= 10) = [s | s <- l, j <- [0..90]]
                  | (i >  10) = [s | s <- l, j <- [0..90]]

buildWorld :: Array U DIM2 Int -> IO (Array U DIM2 Int)
buildWorld m = do
  x <- randomN 90
  y <- randomN 60
  t <- randomN 6

  let mapl = toList m
  let mapexp = chunksOf 90 mapl
  let mapnew = [l | l<-mapexp, i<-[0..60] ]
  print (length mapnew)
  print (length mapexp)
  let mapr = fromListUnboxed (extent m) (map ((addn 2)) mapl)
  return mapr


randomN :: Int -> IO Int
randomN max = do
  getStdRandom $ randomR(1, max)

