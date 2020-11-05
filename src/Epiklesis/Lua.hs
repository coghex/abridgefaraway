module Epiklesis.Lua where
-- the interface to the lua state
-- is instantiated.
import Prelude()
import UPrelude
import qualified Foreign.Lua as Lua
import Anamnesis.Data
import Anamnesis.Util
import Anamnesis.World
import Artos.Var
import Artos.Queue
import Epiklesis.Data
import Epiklesis.World
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
    mtbPath  ← Lua.callFunc "mouseboxTexture"
    txPath   ← Lua.callFunc "textureDirectory"
    return $ makeSettings (sw∷Int) (sh∷Int) (fontPath∷String) (tbPath∷String) (mtbPath∷String) (txPath∷String) layout

makeSettings ∷ Int → Int → String → String → String → String → GLFW.KeyLayout → Settings
makeSettings sw sh fp tbp mtbp txs kl =
  Settings { settingScreenW   = sw
           , settingScreenH   = sh
           , settingFontPath  = fp
           , settingTBPath    = tbp
           , settingMTBPath   = mtbp
           , settingTexPath   = txs
           , settingKeyLayout = kl }

loadState ∷ Env → State → IO ()
loadState env st = do
  let ls = luaState $ luaSt st
  re ← Lua.runWith ls $ do
    Lua.registerHaskellFunction "newWindow" (hsNewWindow env)
    Lua.registerHaskellFunction "newLuaWindow" (hsNewLuaWindow env)
    Lua.registerHaskellFunction "newText" (hsNewText env)
    Lua.registerHaskellFunction "newButton" (hsNewButton env)
    Lua.registerHaskellFunction "newButtonAction" (hsNewButtonAction env)
    Lua.registerHaskellFunction "switchWindow" (hsSwitchWindow env)
    Lua.registerHaskellFunction "newTile" (hsNewTile env)
    Lua.registerHaskellFunction "newMenu" (hsNewMenu env)
    Lua.registerHaskellFunction "newMenuElement" (hsNewMenuElement env)
    Lua.registerHaskellFunction "newWorld" (hsNewWorld env)
    Lua.openlibs
    Lua.dofile $ "mod/base/base.lua"
    ret ← Lua.callFunc "initLua"
    return (ret∷Int)
  let eventQ = envEventsChan env
  atomically $ writeQueue eventQ $ EventLoaded 1
  return ()

hsNewText ∷ Env → String → Double → Double → String → Lua.Lua ()
hsNewText env win x y str = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdnewText win newText) str
  where newText = WinText (x,y) False str

hsNewWindow ∷ Env → String → String → String → Lua.Lua ()
hsNewWindow env name "menu" background = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdnewWindow win) name
  where win = Window name WinTypeMenu background [] [] [] [] WorldNULL
hsNewWindow env name "game" background = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdnewWindow win) name
  where win = Window name (WinTypeGame) background [] [] [] [] WorldNULL
hsNewWindow env name wintype background = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaError errorstr) wintype
  where errorstr = "window type " ⧺ wintype ⧺ " not known"

hsNewLuaWindow ∷ Env → LuaWindow → Lua.Lua ()
hsNewLuaWindow env lw = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdnewLuaWindow lw) "luawindow"

hsNewButton ∷ Env → String → Double → Double → String → Lua.Lua ()
hsNewButton env win x y str = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdnewButton win newText "") str
  where newText = WinText (x,y) True str

hsNewButtonAction ∷ Env → String → Double → Double → String → String → String → Lua.Lua ()
hsNewButtonAction env win x y str "action" args = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdnewLink win newLink) str
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdnewButton win newText args) str
  where newText = WinText (x,y) True str
        newLink = WinLink (x,y) (strwidth,0.5) "action" args
        strwidth = (fromIntegral (length str)) / (4.0 ∷ Double)
hsNewButtonAction env win x y str "link" args = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdnewLink win newLink) str
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdnewButton win newText args) str
  where newText = WinText (x,y) True str
        newLink = WinLink (x,y) (strwidth,0.5) "link" args
        strwidth = (fromIntegral (length str)) / (4.0 ∷ Double)
hsNewButtonAction env win x y str action args = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaError errorstr) str
  where errorstr = "action " ⧺ action ⧺ " not known"

hsNewTile ∷ Env → String → Double → Double → String → Lua.Lua ()
hsNewTile env win x y str = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdnewTile win (WinTile (x,y) str)) str

hsNewMenu ∷ Env → String → String → Double → Double → Lua.Lua ()
hsNewMenu env win menu x y = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdnewMenu win (WinMenu menu (x,y) [])) win

hsNewMenuElement ∷ Env → String → String → String → Lua.Lua ()
hsNewMenuElement env menu "text" args = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdnewMenuElement menu (WinElemText args)) menu
hsNewMenuElement env menu "slider" args = do
  let eventQ = envEventsChan env
  -- these links are not working right now
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdnewLink menu (WinLink (-4.0,2.0) (100.0,100.0) "sliderLeft" "sliderLeft") ) args
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdnewMenuElement menu (WinElemSlider (read min) (read max) (read dflt) text)) menu
  where (text,dflt,min,max) = tupify $ words args
        tupify ∷ [String] → (String,String,String,String)
        tupify t = ((t !! 0), (t !! 1), (t !! 2), (t !! 3))
hsNewMenuElement env menu elemtype args = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaError errorstr) menu
  where errorstr = "newMenuElement " ⧺ elemtype ⧺ " not known"

hsNewWorld ∷ Env → String → Int → Int → String → Lua.Lua()
hsNewWorld env menu w h texs = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdnewWorld menu (createWorld w h 32 32 texs)) menu

hsSwitchWindow ∷ Env → String → Lua.Lua ()
hsSwitchWindow env name = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdswitchWindow name) name

-- converts windows to corresponding texture list
windowTextures ∷ [Window] → [String]
windowTextures []     = []
windowTextures (w:ws) = (singleWindowTextures w) ⧺ (windowTextures ws)

singleWindowTextures ∷ Window → [String]
singleWindowTextures w = [winBackground w]
