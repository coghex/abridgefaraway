module Epiklesis.Lua where
-- the interface to the lua state
-- is instantiated.
import Prelude()
import UPrelude
import qualified Foreign.Lua as Lua
import Anamnesis.Data
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
                    , luaCurrWin = 0
                    , luaWindows = [] }

makeKeyLayout ∷ String → String → String → String → String → GLFW.KeyLayout
makeKeyLayout esckey retkey delkey spckey shkey =
  GLFW.KeyLayout { klEsc = esckey
                 , klRet = retkey
                 , klDel = delkey
                 , klSpc = spckey
                 , klSh  = shkey }

loadState ∷ Env → State → IO ()
loadState env st = do
  let ls = luaState $ luaSt st
  _ ← Lua.runWith ls $ do
    Lua.registerHaskellFunction "newWindow" (hsNewWindow env)
    Lua.registerHaskellFunction "newText" (hsNewText env)
    Lua.registerHaskellFunction "switchWindow" (hsSwitchWindow env)
    Lua.registerHaskellFunction "setBackground" (hsSetBackground env)
    Lua.openlibs
    _ ← Lua.dofile $ "mod/base/base.lua"
    ret ← Lua.callFunc "initLua"
    return (ret∷Int)
  let eventQ = envEventsChan env
  atomically $ writeQueue eventQ $ EventLoaded
  return ()

hsNewWindow ∷ Env → String → String → Lua.Lua ()
hsNewWindow env name "menu" = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdnewWindow win)
  where win = Window name WinTypeMenu (0,0,(-1)) []
hsNewWindow env _    wintype = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaError errorstr)
  where errorstr = "window type " ⧺ wintype ⧺ " not known"

hsNewText ∷ Env → String → Double → Double → String → String → Lua.Lua ()
hsNewText env win x y text "text" = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdnewElem win (WinElemText (x,y) False text))
hsNewText env win x y text "textbox" = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdnewElem win (WinElemText (x,y) True text))
hsNewText env _   _ _ _    textType = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaError errorstr)
  where errorstr = "window type " ⧺ textType ⧺ " not known"

hsSetBackground ∷ Env → String → String → Lua.Lua ()
hsSetBackground env win fp = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdnewElem win (WinElemBack fp))

hsSwitchWindow ∷ Env → String → Lua.Lua ()
hsSwitchWindow env name = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdswitchWindow name)

-- returns list of textures a window may require
findReqTextures ∷ Window → [String]
findReqTextures win = findTexturesFromElems $ winElems win
findTexturesFromElems ∷ [WinElem] → [String]
findTexturesFromElems ((WinElemBack fp):wes) = elemTexs ⧺ findTexturesFromElems wes
  where elemTexs = [fp]
findTexturesFromElems ((WinElemText _ _ _):wes) = []
findTexturesFromElems ((WinElemNULL):wes)       = []

--findLuaWinCam ∷ LuaState → (Float,Float,Float)
--findLuaWinCam (LuaState _ currWin wins) = winCursor win
--  where win = wins !! currWin

-- some simple data manipulators
addWinToLuaState ∷ LuaState → Window → LuaState
addWinToLuaState ls win = LuaState (luaState ls) (luaCurrWin ls) $ (luaWindows ls) ⧺ [win]

addElemToLuaState ∷ String → WinElem → LuaState → LuaState
addElemToLuaState thisWin e ls = ls { luaWindows = newWins }
  where newWins = addElemToWindows thisWin e (luaWindows ls)

addElemToWindows ∷ String → WinElem → [Window] → [Window]
addElemToWindows _       _    []         = []
addElemToWindows thisWin e (win:wins)
  | (thisWin == (winTitle win)) = [(addElemToWindow e win)] ⧺ addElemToWindows thisWin e wins
  | otherwise                   = [win] ⧺ addElemToWindows thisWin e wins
addElemToWindow ∷ WinElem → Window → Window
addElemToWindow e win = win { winElems = (winElems win) ⧺ [e] }

changeCurrWin ∷ Int → LuaState → LuaState
changeCurrWin n ls = ls { luaCurrWin = n }

