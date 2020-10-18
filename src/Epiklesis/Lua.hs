module Epiklesis.Lua where
-- the interface to the lua state
-- is instantiated.
import Prelude()
import UPrelude
import Control.Concurrent (threadDelay)
import qualified Foreign.Lua as Lua
import Anamnesis.Data
import Anamnesis.Util
import Artos.Var
import Artos.Queue
import Epiklesis.Data
import qualified Paracletus.Oblatum.GLFW as GLFW

initLua ∷ IO (LuaState)
initLua = do
  ls ← Lua.newstate
  return $ LuaState { luaState = ls
                    , luaWindows = [] }

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
    txPath   ← Lua.callFunc "textureDirectory"
    return $ makeSettings (sw∷Int) (sh∷Int) (fontPath∷String) (tbPath∷String) (txPath∷String) layout

makeSettings ∷ Int → Int → String → String → String → GLFW.KeyLayout → Settings
makeSettings sw sh fp tbp txs kl =
  Settings { settingScreenW   = sw
           , settingScreenH   = sh
           , settingFontPath  = fp
           , settingTBPath    = tbp
           , settingTexPath   = txs
           , settingKeyLayout = kl }

loadState ∷ Env → State → IO ()
loadState env st = do
  let ls = luaState $ luaSt st
  re ← Lua.runWith ls $ do
    Lua.registerHaskellFunction "newWindow" (hsNewWindow env)
    Lua.registerHaskellFunction "newText" (hsNewText env)
    Lua.openlibs
    Lua.dofile $ "mod/base/base.lua"
    ret ← Lua.callFunc "initLua"
    return (ret∷Int)
  threadDelay 1000000
  let eventQ = envEventsChan env
  atomically $ writeQueue eventQ $ EventLoaded 1
  return ()

hsNewText ∷ Env → String → Float → Float → String → Lua.Lua ()
hsNewText env win x y str = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdnewText win newText) str
  where newText = WinText (x,y) str

hsNewWindow ∷ Env → String → String → Lua.Lua ()
hsNewWindow env name background = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdnewWindow win) name
  where win = Window name background []

-- converts windows to corresponding texture list
windowTextures ∷ [Window] → [String]
windowTextures []     = []
windowTextures (w:ws) = (singleWindowTextures w) ⧺ (windowTextures ws)

singleWindowTextures ∷ Window → [String]
singleWindowTextures w = [winBackground w]
