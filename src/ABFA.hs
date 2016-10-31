import qualified Graphics.UI.GLFW as GLFW
import qualified Data.ByteString.Internal as BSI
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
  Just win <- GLFW.createWindow 1600 1200 "A Bridge Far Away..." Nothing Nothing
  GLFW.makeContextCurrent (Just win)
  texs <- initGL win
  seed <- newStdGen
  len <- randomN 1 15
  let xargs = randomList (0, 90::Int) len seed
  let yargs = randomList (0, 120::Int) len seed
  let xsize = randomList (5, 15::Int) len seed
  let ysize = randomList (6, 16::Int) len seed

  let emptymap = take (90*120) (repeat 5);
  w0 <- makeCoords emptymap (zip xargs yargs) (zip xsize ysize)
  w1 <- makeIce w0
  w2 <- makeCoast w1
  w3 <- changeTile w2 (1, 1) 7

  GLFW.setWindowRefreshCallback win (Just (drawScene texs emptymap))
  GLFW.setFramebufferSizeCallback win (Just resizeScene)
  GLFW.setKeyCallback win (Just keyPressed)
  GLFW.setWindowCloseCallback win (Just shutdown)
  forever $ do
    GLFW.pollEvents
    drawScene texs w3 win
    GLFW.swapBuffers win
