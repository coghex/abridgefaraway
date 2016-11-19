module Game.Save where
import Data.List.Split
import Control.Exception
import System.IO.Error
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

loadZone :: Int -> Int -> IO ([Int])
loadZone x y = do
  contents <- tryJust handleReadFile (readFile $ "data/zones/zone" ++ (show x) ++ "_" ++ (show y) ++ ".txt")
  case contents of
    Left except -> return []
    Right cont -> return $ init $ map read (splitOn " " cont)
  where
    handleReadFile :: IOError -> Maybe String
    handleReadFile er
      | isDoesNotExistError er = Just "readFile: does not exist"
      | isPermissionError   er = Just "readFile: permission denied"
      | otherwise              = Nothing

loadPath :: Int -> Int -> IO ([Int])
loadPath x y = do
  contents <- tryJust handleReadFile (readFile $ "data/zones/path" ++ (show x) ++ "_" ++ (show y) ++ ".txt")
  case contents of
    Left except -> return []
    Right cont -> return $ init $ map read (splitOn " " cont)
  where
    handleReadFile :: IOError -> Maybe String
    handleReadFile er
      | isDoesNotExistError er = Just "readFile: does not exist"
      | isPermissionError   er = Just "readFile: permission denied"
      | otherwise              = Nothing

saveZone :: Int -> Int -> [Int] -> [Int] -> [Int] -> [(Int, Int)] -> [(Int, Int)] -> [Int] -> IO ()
saveZone x y curz paths pathrands bushes brands bsizes = do
  let fileName1 = "data/zones/zone" ++ (show x) ++ "_" ++ (show y) ++ ".txt"
  let writeData1 = (foldl1 ((++)) (map addSpace curz))
  oldz <- loadZone x y
  if (oldz == curz) then do
    return ()
  else do
    writeFile fileName1 writeData1
    let fileName2 = "data/zones/path" ++ (show x) ++ "_" ++ (show y) ++ ".txt"
    let writeData2 = (foldl1 ((++)) (map addSpace paths))
    writeFile fileName2 writeData2

addSpace :: Int -> String
addSpace s = ((show s)++" ")
