import qualified Graphics.UI.GLFW as GLFW
import qualified Data.ByteString.Internal as BSI
import Graphics.GL
import System.Exit ( exitWith, ExitCode(..) )
import Control.Monad ( forever )
import Control.Monad.State
import System.Random
import Paths
import Util
import GLinit
import GLloop
import World
import Settings
import State

main :: IO ()
main = do
  True <- GLFW.init
  GLFW.defaultWindowHints
  Just win <- GLFW.createWindow pixelsw pixelsh "A Bridge Far Away..." Nothing Nothing
  GLFW.makeContextCurrent (Just win)
  texs <- initGL win

  seed <- newStdGen
  len <- randomN 1 15
  let xargs = randomList (0, maph) len seed
  let yargs = randomList (0, mapw) len seed
  let xsize = randomList (5, 15::Int) len seed
  let ysize = randomList (6, 16::Int) len seed

  let emptymap = take (maph*mapw) (repeat 5);
  let w = packArgs xargs yargs xsize ysize emptymap win texs
  w1 <- makeState w
  w2 <- makeIce w1
  w3 <- makeCoast w2

  let w0 = themap w3

  GLFW.setWindowRefreshCallback win (Just (drawScene w3))
  GLFW.setFramebufferSizeCallback win (Just resizeScene)
  GLFW.setKeyCallback win (Just keyPressed)
  GLFW.setWindowCloseCallback win (Just shutdown)
  forever $ do
    GLFW.pollEvents
    drawScene w3 win
    GLFW.swapBuffers win
