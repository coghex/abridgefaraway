module Save where
import Data.List.Split
import Text.Read
import Util

saveMap :: [Int] -> IO ()
saveMap s = do
  let fileName = "data/save.txt"
  let writeData = (foldl1 ((++)) (map addSpace s))
  writeFile fileName writeData

loadMap :: IO ([Int])
loadMap = do
  contents <- readFile "data/save.txt"
  return $ init $ map read (splitOn " " contents)


addSpace :: Int -> String
addSpace s = ((show s)++" ")
