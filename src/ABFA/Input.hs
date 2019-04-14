module ABFA.Input where
-- the inputs from mouse and keyboard are processed

import Control.Monad.RWS.Strict (liftIO, asks, ask, gets, get, modify)
import Control.Monad (when)
import qualified GLUtil.ABFA as GLFW
import ABFA.Game
import ABFA.State
import ABFA.Data

-- case function for all of the keys
evalKey :: GLFW.Window -> GLFW.Key -> Game ()
evalKey window k = do
  state <- get
  env   <- ask
  let keylayout = settingKeyLayout (stateSettings state)
  let gs        = stateGame state
  when ((gs == SMenu) && (keyCheck keylayout k "ESC")) $ do
    liftIO $ GLFW.closeGLFW window

-- checks key with settings
keyCheck :: KeyLayout -> GLFW.Key -> String -> Bool
keyCheck keylayout k str = (k == (GLFW.getGLFWKey nk))
  where nk = getKey keylayout str

-- retrieves user key setting
getKey :: KeyLayout -> String -> String
getKey keylayout "ESC" = keyESC keylayout
getKey keylayout "C"   = keyC   keylayout
getKey keylayout "R"   = keyR   keylayout
getKey keylayout _     = "NULL"

