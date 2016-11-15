module Game.Save where
import Data.List.Split
import Text.Read
import Game.Util

saveMap :: [Int] -> IO ()
saveMap s = do
  let fileName = "data/save.txt"
  f <- loadMap
  if (f == s) then do
    return ()
  else do
    let writeData = (foldl1 ((++)) (map addSpace s))
    writeFile fileName writeData

loadMap :: IO ([Int])
loadMap = do
  contents <- readFile "data/save.txt"
  return $ init $ map read (splitOn " " contents)


addSpace :: Int -> String
addSpace s = ((show s)++" ")
