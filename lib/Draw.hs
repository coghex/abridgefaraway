module Draw where

import Data.Bits ((.|.))
import Data.List.Split (chunksOf)
import Graphics.GL
import System.Random (StdGen, mkStdGen)
import qualified Graphics.UI.GLFW as GLFW

import State
import Rand
import Settings

resequence_ :: [IO ()] -> IO ()
resequence_ = foldr (>>) (return ())

expandMap :: [Int] -> [([(Int, Int)], Int)]
expandMap m = zip (map workRows (chunksOf gridw m)) [0..gridh]

workRows :: [Int] -> [(Int, Int)]
workRows l = do
  zip l [0..gridw]

flattenMap :: [[Int]] -> [Int]
flattenMap xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

stripMap :: [([(Int, Int)], Int)] -> [[Int]]
stripMap ((a,b):ys) = (stripRow a) : stripMap ys
stripMap _          = [[]]

stripRow :: [(Int, Int)] -> [Int]
stripRow ((a,b):ys) = a : stripRow ys
stripRow _          = []

sceneSetup :: IO ()
sceneSetup = do
  glClear $ fromIntegral  $  GL_COLOR_BUFFER_BIT
                         .|. GL_DEPTH_BUFFER_BIT

drawSquare :: IO ()
drawSquare = do
  glBegin GL_QUADS
  glTexCoord2f   0    0
  glVertex3f   (-1) (-1)   1
  glTexCoord2f   1    0
  glVertex3f     1  (-1)   1
  glTexCoord2f   1    1
  glVertex3f     1    1    1
  glTexCoord2f   0    1
  glVertex3f   (-1)   1    1
  glEnd



drawCursor :: Int -> Int -> IO ()
drawCursor x y = do
  glLoadIdentity
  glTranslatef (2*((fromIntegral x) - ((fromIntegral gridw)/2))) (2*((fromIntegral y) - ((fromIntegral gridh)/2))) (-400)
  glEnable GL_BLEND
  glColor3f 1 1 1
  glBegin GL_QUADS
  glTexCoord2f   0    0
  glVertex3f   (-1) (-1)   1
  glTexCoord2f   1    0
  glVertex3f     1  (-1)   1
  glTexCoord2f   1    1
  glVertex3f     1    1    1
  glTexCoord2f   0    1
  glVertex3f   (-1)   1    1
  glEnd

drawTile :: [GLuint] -> Int -> Int -> Int -> IO ()
drawTile texs x y t = do
  glLoadIdentity
  glTranslatef (2*((fromIntegral x) - ((fromIntegral gridw)/2))) (2*((fromIntegral y) - ((fromIntegral gridh)/2))) (-400)

  glBindTexture GL_TEXTURE_2D (texs !! t)
  glColor3f 1 1 1
  glBegin GL_QUADS
  glTexCoord2f   0    0
  glVertex3f   (-1) (-1)   1
  glTexCoord2f   1    0
  glVertex3f     1  (-1)   1
  glTexCoord2f   1    1
  glVertex3f     1    1    1
  glTexCoord2f   0    1
  glVertex3f   (-1)   1    1
  glEnd

drawScene :: State -> GLFW.Window -> IO ()
drawScene state win = do
  let mapnew = expandMap (stateGrid state)
  resequence_ (drawWorld (stateTexs state) mapnew)
  glFlush

drawWorld :: [GLuint] -> [([(Int, Int)], Int)] -> [IO ()]
drawWorld texs m = map (drawRow texs) m

drawRow :: [GLuint] -> ([(Int, Int)], Int) -> IO ()
drawRow texs (a, b) = resequence_ (map (drawSpot texs b) a)

drawSpot :: [GLuint] -> Int -> (Int, Int) -> IO ()
drawSpot texs y (t,x) = (drawTile texs x y t)

makeSeeds :: [(Int, Int)] -> Int -> StdGen -> StdGen -> [Int] -> [[(Int, Int)]]
makeSeeds []         _ _  _  _               = []
makeSeeds ((x,y):xs) r s1 s2 (nspot:nspots) = (buildList2 ((randomList ((x-fudge), (x+fudge)) (nspot) s1), (randomList ((y-fudge),(y+fudge)) (nspot) s2))):(makeSeeds xs (r+1) (mkStdGen (r+1)) (mkStdGen (r-1)) nspots)
