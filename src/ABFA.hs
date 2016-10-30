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

  let emptymap = take (90*120) (repeat 5);
  w1 <- buildWorld emptymap 50 50
  w2 <- buildWorld w1 50 100
  GLFW.setWindowRefreshCallback win (Just (drawScene texs emptymap))
  GLFW.setFramebufferSizeCallback win (Just resizeScene)
  GLFW.setKeyCallback win (Just keyPressed)
  GLFW.setWindowCloseCallback win (Just shutdown)
  forever $ do
    GLFW.pollEvents
    drawScene texs w2 win
    GLFW.swapBuffers win
