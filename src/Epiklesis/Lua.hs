module Epiklesis.Lua where
-- the interface to the lua state
-- is instantiated.
import Prelude()
import UPrelude
import Data.List (sort)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import qualified Foreign.Lua as Lua
import System.Directory (getDirectoryContents)
import System.FilePath (combine)
import Anamnesis.Data
import Artos.Var
import Artos.Queue
import Epiklesis.Data
import Epiklesis.Elems
import Epiklesis.Shell
import Epiklesis.World
import Paracletus.Data
import Paracletus.Draw
import Paracletus.Oblatum.Data
import qualified Paracletus.Oblatum.GLFW as GLFW

initLua ∷ IO (LuaState)
initLua = do
  ls ← Lua.newstate
  initLC ← initLuaConfig ls "mod/base/config.lua"
  return $ LuaState { luaState   = ls
                    , luaFPS     = Nothing
                    , luaConfig  = initLC
                    , luaCurrWin = 0
                    , luaLastWin = 0
                    , luaNDefTex = 0
                    , luaShell   = initShell
                    , luaCmds    = ["newWindow", "newText", "newMenu", "newMenuBit", "newLink", "newWorld", "switchWindow", "setBackground", "luaModule", "newDynObj", "toggleFPS"]
                    , luaModules = []
                    , luaWindows = [] }

initLuaConfig ∷ Lua.State → String → IO (LuaConfig)
initLuaConfig ls fn = Lua.runWith ls $ do
  Lua.openlibs
  Lua.dofile $ fn
  esckey   ← Lua.getglobal "esckey"   *> Lua.peek (-1)
  retkey   ← Lua.getglobal "retkey"   *> Lua.peek (-1)
  delkey   ← Lua.getglobal "delkey"   *> Lua.peek (-1)
  spckey   ← Lua.getglobal "spckey"   *> Lua.peek (-1)
  tabkey   ← Lua.getglobal "tabkey"   *> Lua.peek (-1)
  upkey    ← Lua.getglobal "upkey"    *> Lua.peek (-1)
  leftkey  ← Lua.getglobal "leftkey"  *> Lua.peek (-1)
  downkey  ← Lua.getglobal "downkey"  *> Lua.peek (-1)
  rightkey ← Lua.getglobal "rightkey" *> Lua.peek (-1)
  shkey    ← Lua.getglobal "shkey"    *> Lua.peek (-1)
  return $ LuaConfig $ KeyLayout esckey retkey delkey spckey tabkey upkey leftkey downkey rightkey shkey

loadState ∷ Env → State → IO ()
loadState env st = do
  let ls = luaState $ luaSt st
  _ ← Lua.runWith ls $ do
    Lua.registerHaskellFunction "newWindow" (hsNewWindow env)
    Lua.registerHaskellFunction "newText" (hsNewText env)
    Lua.registerHaskellFunction "newMenu" (hsNewMenu env)
    Lua.registerHaskellFunction "newMenuBit" (hsNewMenuBit env)
    Lua.registerHaskellFunction "newLink" (hsNewLink env)
    Lua.registerHaskellFunction "newWorld" (hsNewWorld env)
    Lua.registerHaskellFunction "switchWindow" (hsSwitchWindow env)
    Lua.registerHaskellFunction "setBackground" (hsSetBackground env)
    Lua.registerHaskellFunction "loadModule" (hsLoadModule env)
    Lua.registerHaskellFunction "newDynObj" (hsNewDynObj env)
    Lua.registerHaskellFunction "toggleFPS" (hsToggleFPS env)
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
  where win = Window name WinTypeMenu (0,0,(-1)) [] []
hsNewWindow env name "game" = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdnewWindow win)
  where win = Window name WinTypeGame (0,0,(-1)) [] []
hsNewWindow env _    wintype = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaError errorstr)
  where errorstr = "window type " ⧺ wintype ⧺ " not known"

hsNewText ∷ Env → String → Double → Double → String → String → Lua.Lua ()
hsNewText env win x y text "text" = do
  let eventQ = envEventsChan env
      initCache = WECached $ addText False x (x,y) text
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdnewElem win (WinElemText (x,y) False text) initCache)
hsNewText env win x y text "textbox" = do
  let eventQ = envEventsChan env
      initCache = WECached $ (addTextBox posOffset size) ⧺ addText False x (x,y) text
      size = calcTextBoxSize text
      posOffset = (x - 1.0,y + 0.5)
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdnewElem win (WinElemText (x,y) True text) initCache)
hsNewText env _   _ _ _    textType = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaError errorstr)
  where errorstr = "window type " ⧺ textType ⧺ " not known"

hsNewMenu ∷ Env → String → String → Double → Double → Lua.Lua ()
hsNewMenu env win name x y = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdnewElem win (WinElemMenu name (x,y) []) WEUncached)

hsNewMenuBit ∷ Env → String → String → String → String → Lua.Lua ()
hsNewMenuBit env win menu "text" args = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdnewMenuBit win menu (MenuText args))
hsNewMenuBit env win menu "slider" args = do
  let eventQ = envEventsChan env
      sargs = splitOn ":" args
      rargs = splitOn "-" $ last sargs
  r1 ← case (readMaybe (head rargs)) of
    Nothing → do
      let errorstr = "cannot read slider range"
      Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaError errorstr)
      return $ -1
    Just n → return n
  r2 ← case (readMaybe (last rargs)) of
    Nothing → do
      let errorstr = "cannot read slider range"
      Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaError errorstr)
      return $ -1
    Just n  → return n
  let text  = head sargs
      range = (r1,r2)
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdnewMenuBit win menu (MenuSlider text range))
hsNewMenuBit env _   _    mbtype _    = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaError errorstr)
  where errorstr = "menubit type " ⧺ mbtype ⧺ " not known"

hsNewLink ∷ Env → String → Double → Double → String → String → String → Lua.Lua ()
hsNewLink env win x y text "action" "exit" = do
  let eventQ = envEventsChan env
      size'  = (length (text),length (splitOn ['\n'] text))
      size   = (fromIntegral (fst size'), fromIntegral (snd size'))
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdnewElem win (WinElemLink (x,y) size LinkExit) WEUncached)
hsNewLink env win x y text "action" "back" = do
  let eventQ = envEventsChan env
      size'  = (length (text),length (splitOn ['\n'] text))
      size   = (fromIntegral (fst size'), fromIntegral (snd size'))
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdnewElem win (WinElemLink (x,y) size LinkBack) WEUncached)
hsNewLink env _   _ _ _    "action" args   = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaError errorstr)
  where errorstr = "link args " ⧺ args ⧺ " unknown"
hsNewLink env win x y text "link" args     = do
  let eventQ = envEventsChan env
      size'  = (length (text),length (splitOn ['\n'] text))
      size   = (fromIntegral (fst size'), fromIntegral (snd size'))
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdnewElem win (WinElemLink (x,y) size (LinkLink args)) WEUncached)
hsNewLink env _   _ _ _    action _        = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaError errorstr)
  where errorstr = "link action " ⧺ action ⧺ " unknown"

hsNewWorld ∷ Env → String → Int → Int → Int → Int → String → Lua.Lua ()
hsNewWorld env win zx zy sx sy dp = do
  let eventQ = envEventsChan env
  rawdp ← Lua.liftIO $ getDirectoryContents dp
  let dps = map (combine dp) $ sort $ filter filterOutPathJunk rawdp
      filterOutPathJunk ∷ FilePath → Bool
      filterOutPathJunk "."  = False
      filterOutPathJunk ".." = False
      filterOutPathJunk _    = True
      --wp = WorldParams (zx,zy) (sx,sy) ncont conts seeds rands sizes types $ length dps
      wp = WorldParams (zx,zy) (sx,sy) $ length dps
      -- TODO: represent aspect ratio
      wd = WorldData (1.0,1.0) (16,8) [Zone (0,0) (initSegs)]
      initSegs = take zy (repeat (take zx (repeat (initSeg))))
      initSeg  = SegmentNULL--Segment $ take sy (repeat (take sx (repeat (Tile 1 1))))
      --ncont = head (withStdGen sgs 1 (randomRs (minnc maxnc)))
      --conts = tail $ buildList2 ((withStdGen sgs 1 (randomList (f, (gw - f)) ncont)), (withStdGen sgs 2 (randomList (1, (gh - 1)) ncont)))
      --seeds = tail $ makeSeeds seedsseed nspots sgs f
      --seedsseed = buildList2 ((withStdGen sgs 3 (randomList (f, (gw - f)) ncont)), (withStdGen sgs 4 (randomList (1, (gh - 1)) ncont)))
      --rands = tail $ makeSeeds randsseed nspots (splitSGs sgs) f
      --randsseed = buildList2 ((withStdGen sgs 5 (randomList (f, (gw - f)) ncont)), (withStdGen sgs 6 (randomList (1, (gh - 1)) ncont)))
      --sizes = withStdGen sgs 1 $ randomList (mins, maxs) ncont
      --types = withStdGen sgs 2 $ randomBiomeList ncont
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdnewElem win (WinElemWorld wp wd dps) (WECached (calcTiles wp wd)))

hsSetBackground ∷ Env → String → String → Lua.Lua ()
hsSetBackground env win fp = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdnewElem win (WinElemBack fp) WEUncached)

hsSwitchWindow ∷ Env → String → Lua.Lua ()
hsSwitchWindow env name = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdswitchWindow name)

hsLoadModule ∷ Env → String → Lua.Lua ()
hsLoadModule env fp = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdloadModule fp)

hsToggleFPS ∷ Env → Lua.Lua ()
hsToggleFPS env = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdtoggleFPS)

hsNewDynObj ∷ Env → String → String → Lua.Lua ()
hsNewDynObj env win "fps" = do
  let eventQ = envEventsChan env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLua (LuaCmdnewElem win (WinElemDyn DynFPS [DynData (0,0) (0,0), DynData (0,0) (0,0), DynData (0,0) (0,0), DynData (0,0) (0,0)]) WEUncached)

-- returns list of textures a window may require
findReqTextures ∷ Window → [String]
findReqTextures win = findTexturesFromElems $ reverse $ sort $ winElems win
findTexturesFromElems ∷ [WinElem] → [String]
findTexturesFromElems []                        = []
findTexturesFromElems ((WinElemBack fp):wes)    = elemTexs ⧺ findTexturesFromElems wes
  where elemTexs = [fp]
findTexturesFromElems ((WinElemWorld _ _ dps):wes)  = dps ⧺ findTexturesFromElems wes
findTexturesFromElems ((WinElemLink _ _ _):wes) = findTexturesFromElems wes
findTexturesFromElems ((WinElemText _ _ _):wes) = findTexturesFromElems wes
findTexturesFromElems ((WinElemMenu _ _ _):wes) = findTexturesFromElems wes
findTexturesFromElems ((WinElemDyn _ _):wes)    = findTexturesFromElems wes
findTexturesFromElems ((WinElemNULL):wes)       = findTexturesFromElems wes

-- some simple data manipulators
addWinToLuaState ∷ LuaState → Window → LuaState
addWinToLuaState ls win = ls { luaWindows = (luaWindows ls) ⧺ [win] }

addElemToLuaState ∷ String → WinElem → WinElemCache → LuaState → LuaState
addElemToLuaState thisWin e ec ls = ls { luaWindows = newWins }
  where newWins = addElemToWindows thisWin e ec (luaWindows ls)

addElemToWindows ∷ String → WinElem → WinElemCache → [Window] → [Window]
addElemToWindows _       _ _  []         = []
addElemToWindows thisWin e ec (win:wins)
  | (thisWin == (winTitle win)) = [(addElemToWindow e ec win)] ⧺ addElemToWindows thisWin e ec wins
  | otherwise                   = [win] ⧺ addElemToWindows thisWin e ec wins
addElemToWindow ∷ WinElem → WinElemCache → Window → Window
addElemToWindow e ec win = win { winCache = (winCache win) ⧺ [ec]
                               , winElems = (winElems win) ⧺ [e] }

addMenuBitToLuaState ∷ String → String → MenuBit → LuaState → LuaState
addMenuBitToLuaState win menu menubit ls = case (findWin win (luaWindows ls)) of
  Nothing → ls
  Just w  → ls { luaWindows = replaceWin w' (luaWindows ls) }
    where w' = addMenuBitToWindow menu menubit w
addMenuBitToWindow ∷ String → MenuBit → Window → Window
addMenuBitToWindow menu mb win = win { winElems = addBitToMenu menu mb (winElems win) }

findLastMenuBitPos ∷ String → String → LuaState → (Double,Double)
findLastMenuBitPos win menu ls = case (findWin win (luaWindows ls)) of
    Nothing → (0,0)
    Just w  → case (findMenuPos menu (winElems w)) of
                Nothing → (0,0)
                Just p  → p

changeCurrWin ∷ Int → LuaState → LuaState
changeCurrWin n ls = ls { luaCurrWin = n
                        , luaLastWin = n' }
  where n' = luaCurrWin ls

-- sets the fps of any fps elements
-- in the current window
setFPS ∷ LuaState → Int → LuaState
setFPS ls fps = ls { luaWindows = replaceWin win (luaWindows ls) }
  where win = setWinFPS fps $ currentWindow ls
setWinFPS ∷ Int → Window → Window
setWinFPS fps win = win { winElems = setElemFPS fps $ winElems win }
setElemFPS ∷ Int → [WinElem] → [WinElem]
setElemFPS _   []       = []
setElemFPS fps ((WinElemDyn DynFPS dd):wes) = [WinElemDyn DynFPS (calcFPSDyn fps)] ⧺ setElemFPS fps wes
setElemFPS fps (we:wes) = [we] ⧺ setElemFPS fps wes

-- finds the texture offsets needed
-- for any 4 didget number
calcFPSDyn ∷ Int → [DynData]
calcFPSDyn fps
  | fps < 0 = [nulldd,nulldd,nulldd,nulldd]
  | fps < 10 = [dd1,nulldd,nulldd,nulldd]
  | fps < 100 = [dd1,dd2,nulldd,nulldd]
  | fps < 1000 = [dd1,dd2,dd3,nulldd]
  | otherwise = [dd1,dd2,dd3,dd4]
  where nulldd = DynData (0,0) (0,0)
        dd1 = calcNumDyn $ fps `mod` 10
        dd2 = calcNumDyn $ (fps `div` 10) `mod` 10
        dd3 = calcNumDyn $ (fps `div` 100) `mod` 10
        dd4 = calcNumDyn $ (fps `div` 1000) `mod` 10
-- finds the offset for a single didget int
calcNumDyn ∷ Int → DynData
calcNumDyn 0 = DynData (0,0) (-3,0)
calcNumDyn 1 = DynData (0,0) (-12,0)
calcNumDyn 2 = DynData (0,0) (-11,0)
calcNumDyn 3 = DynData (0,0) (-10,0)
calcNumDyn 4 = DynData (0,0) (-9,0)
calcNumDyn 5 = DynData (0,0) (-8,0)
calcNumDyn 6 = DynData (0,0) (-7,0)
calcNumDyn 7 = DynData (0,0) (-6,0)
calcNumDyn 8 = DynData (0,0) (-5,0)
calcNumDyn 9 = DynData (0,0) (-4,0)
calcNumDyn _ = DynData (0,0) (0,0)

-- replaces specific window in windows
replaceWin ∷ Window → [Window] → [Window]
replaceWin _ [] = []
replaceWin win (w:ws)
  | (winTitle w) ≡ (winTitle win) = [win] ⧺ (replaceWin win ws)
  | otherwise = [w] ⧺ (replaceWin win ws)

-- adds to specific menu in winElems
addBitToMenu ∷ String → MenuBit → [WinElem] → [WinElem]
addBitToMenu _    _  [] = []
addBitToMenu menu mb ((WinElemMenu name pos mbs):wes)
  | (name ≡ menu) = [WinElemMenu name pos (mbs ⧺ [mb])] ⧺ addBitToMenu menu mb wes
  | otherwise     = [WinElemMenu name pos mbs] ⧺ addBitToMenu menu mb wes
addBitToMenu menu mb (we:wes) = [we] ⧺ addBitToMenu menu mb wes

-- returns window with name
findWin ∷ String → [Window] → Maybe Window
findWin _  []     = Nothing
findWin wn (w:ws)
  | winTitle w ≡ wn = Just w
  | otherwise       = findWin wn ws

-- returns menu position with name
findMenuPos ∷ String → [WinElem] → Maybe (Double,Double)
findMenuPos _  [] = Nothing
findMenuPos mn ((WinElemMenu name (x,y) bits):wes)
  | name ≡ mn = Just (x + 4.0, (y + 0.5 - ((fromIntegral (length bits))/2.0)))
  | otherwise = findMenuPos mn wes
findMenuPos mn (_:wes) = findMenuPos mn wes

-- its ok to have !! here since
-- currwin and wins are created
-- together, the outcome is known
currentWindow ∷ LuaState → Window
currentWindow ls = (luaWindows ls) !! (luaCurrWin ls)
