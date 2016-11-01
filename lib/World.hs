module World where

import System.Random
import Language.Haskell.Interpreter
import Data.List.Split
import Control.Monad

randomList :: (Random a) => (a,a) -> Int -> StdGen -> [a]
randomList bnds n = take n . randomRs bnds
