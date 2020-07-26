module Paracletus.Oblatum.Callback where
-- callbacks for GLFW are defined
import Artos.Queue
import Artos.Var
import qualified Paracletus.Oblatum.GLFW as GLFW

errorCallback ∷ Queue Event → GLFW.Error → String → IO ()
errorCallback tc e s = atomically $ writeQueue tc $ EventError e s
keyCallback ∷ Queue Event → GLFW.Window → GLFW.Key → Int → GLFW.KeyState → GLFW.ModifierKeys → IO ()
keyCallback tc win k sc ka mk = atomically $ writeQueue tc $ EventKey win k sc ka mk
