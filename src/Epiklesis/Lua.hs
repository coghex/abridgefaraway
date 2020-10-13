module Epiklesis.Lua where
-- the interface to the lua state
-- is instantiated.
import Prelude()
import UPrelude
import Control.Concurrent (threadDelay)
import qualified Foreign.Lua as Lua
import Anamnesis.Data
import Artos.Var
import Artos.Queue
import Epiklesis.Data
import qualified Paracletus.Oblatum.GLFW as GLFW

initLua ∷ IO (LuaState)
initLua = do
  ls ← Lua.newstate
  return $ LuaState { luaState = ls }

importKeyLayout ∷ Lua.State → String → IO (GLFW.KeyLayout)
importKeyLayout ls fn = Lua.runWith ls $ do
  Lua.openlibs
  Lua.dofile $ fn ⧺ "config.lua"
  esckey ← Lua.getglobal "esckey" *> Lua.peek (-1)
  return $ makeKeyLayout esckey

makeKeyLayout ∷ String → GLFW.KeyLayout
makeKeyLayout esckey = GLFW.KeyLayout { klEsc = esckey }

importSettings ∷ LuaState → String → IO (Settings)
importSettings ls' fn = do
  let ls = luaState ls'
  layout ← importKeyLayout ls fn
  Lua.runWith ls $ do
    Lua.openlibs
    Lua.dofile $ fn ⧺ "base.lua"
    (sw, sh) ← Lua.callFunc "getScreenSize"
    fontPath ← Lua.callFunc "fontAtlas"
    tbPath   ← Lua.callFunc "textboxTexture"
    return $ makeSettings (sw∷Int) (sh∷Int) (fontPath∷String) (tbPath∷String) layout

makeSettings ∷ Int → Int → String → String → GLFW.KeyLayout → Settings
makeSettings sw sh fp tbp kl =
  Settings { settingScreenW   = sw
           , settingScreenH   = sh
           , settingFontPath  = fp
           , settingTBPath    = tbp
           , settingKeyLayout = kl }

loadState ∷ Env → State → IO ()
loadState env state = do
  threadDelay 1000000
  let eventQ = envEventsChan env
  atomically $ writeQueue eventQ $ EventLoaded 1
  return ()
