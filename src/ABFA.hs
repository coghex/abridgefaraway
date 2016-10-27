import qualified Graphics.UI.GLFW as GLFW
import qualified Data.ByteString.Internal as BSI
import Data.Array.Repa hiding((++))
import Data.Array.Repa.Algorithms.Randomish ( randomishIntArray )
import System.Exit ( exitWith, ExitCode(..) )
import Control.Monad ( forever )
import Control.Monad.State
import System.Random
import Paths
import Util
import GLinit
import GLloop
import World

main :: IO ()
main = do
  True <- GLFW.init
  GLFW.defaultWindowHints
  Just win <- GLFW.createWindow 800 600 "A Bridge Far Away..." Nothing Nothing
  GLFW.makeContextCurrent (Just win)
  texs <- initGL win
  let emptymap :: Array U DIM2 Int; emptymap = randomishIntArray ( Z :. (60::Int) :. (90::Int)) 0 0 1
  w1 <- buildWorld emptymap
  print w1
  GLFW.setWindowRefreshCallback win (Just (drawScene texs emptymap))
  GLFW.setFramebufferSizeCallback win (Just resizeScene)
  GLFW.setKeyCallback win (Just keyPressed)
  GLFW.setWindowCloseCallback win (Just shutdown)
  forever $ do
    GLFW.pollEvents
    drawScene texs emptymap win
    GLFW.swapBuffers win
