{-# LANGUAGE Strict #-}
module Paracletus.Oblatum.Input where
-- key input is handled
import Prelude()
import UPrelude
import Control.Monad (when)
import Control.Monad.State.Class (modify',gets)
import Anamnesis
import Anamnesis.Data
import Anamnesis.Map
import Anamnesis.Util
import Anamnesis.World
import Artos.Data
import Artos.Queue
import Artos.Var
import Epiklesis.Data
import Epiklesis.Lua
import Epiklesis.Shell
import Paracletus.Data
import Paracletus.Oblatum.Data
import qualified Paracletus.Oblatum.GLFW as GLFW
      
-- TODO: un-hardcode the pixels here
convertPixels ∷ ∀ a. (Fractional a, Num a) ⇒ (a,a) → (a,a)
convertPixels (x,y) = (x',y')
  where x' = ((x - (1280.0 / 2.0)) / 64.0)
        y' = - ((y - ( 720.0 / 2.0)) / 64.0)

posClose ∷ (Double,Double) → (Double,Double) → (Double,Double) → Bool
posClose (buttWidth,buttHeight) (x1,y1) (x2,y2)
  | ((abs(x1 - x2)) < buttWidth) && ((abs(y1 - y2)) < buttHeight) = True
  | otherwise = False

moveSlider ∷ Double → Int → DrawState → DrawState
moveSlider x n ds = ds { dsDyns = dyns }
  where dyns = moveSliderDyn x n (dsDyns ds)
moveSliderDyn ∷ Double → Int → [DynData] → [DynData]
moveSliderDyn _ _ []       = []
moveSliderDyn x n (dd:dds)
  | ddRef dd == DDSlider n = [dd'] ⧺ (moveSliderDyn x n dds)
  | otherwise     = [dd]  ⧺ (moveSliderDyn x n dds)
  where dd' = dd { ddPosition = (x', snd (ddPosition dd)) }
        x' = min 5.4 $ max 0.0 $ 2.0*(realToFrac x) - 0.4

moveLSSlider ∷ Double → Int → LuaState → LuaState
moveLSSlider x n ls = ls { luaWindows = replaceWin w' (luaWindows ls) }
  where w' = moveLSSliderWin x n $ currentWindow ls
moveLSSliderWin ∷ Double → Int → Window → Window
moveLSSliderWin x n win = win { winElems = moveLSSliderWinElems x n (winElems win) }
moveLSSliderWinElems ∷ Double → Int → [WinElem] → [WinElem]
moveLSSliderWinElems _ _ []       = []
moveLSSliderWinElems x n ((WinElemMenu name pos bits):wes) = [WinElemMenu name pos bits'] ⧺ moveLSSliderWinElems x n wes
  where bits' = moveLSSliderMenuBits (x + (fst pos)) n bits
moveLSSliderWinElems x n ((WinElemDyn (DynSlider pos) dd):wes) = [WinElemDyn (DynSlider pos) (moveLSSliderDD x n dd)] ⧺ moveLSSliderWinElems x n wes
moveLSSliderWinElems x n (we:wes) = [we] ⧺ moveLSSliderWinElems x n wes
moveLSSliderDD ∷ Double → Int → [DynData] → [DynData]
moveLSSliderDD _ _ []       = []
moveLSSliderDD x n (dd:dds)
  | ddRef dd == DDSlider n = [dd'] ⧺ moveLSSliderDD x n dds
  | otherwise     = [dd]  ⧺ moveLSSliderDD x n dds
  where dd' = dd { ddPosition = (x', snd (ddPosition dd)) }
        x' = min 5.4 $ max 0.0 $ 2.0*(realToFrac x) - 0.4
moveLSSliderMenuBits ∷ Double → Int → [MenuBit] → [MenuBit]
moveLSSliderMenuBits _ _ []       = []
moveLSSliderMenuBits x n ((MenuSlider i text range val sel curs ci):mbs)
  | n == i = [mb'] ⧺ moveLSSliderMenuBits x n mbs
  | otherwise = [mb] ⧺ moveLSSliderMenuBits x n mbs
  where mb'   = MenuSlider i text range val'' sel curs ci
        val'' = Just $ min (snd range) $ max (fst range) val'
        val'  = round $ (mx - mn)*(0.4*(x+4.0))
        mn    = fromIntegral $ fst range
        mx    = fromIntegral $ snd range
        mb    = MenuSlider i text range val sel curs ci
moveLSSliderMenuBits x n (mb:mbs) = [mb] ⧺ moveLSSliderMenuBits x n mbs

isElemLink ∷ WinElem → Bool
isElemLink (WinElemLink _ _ _)  = True
isElemLink (WinElemWorld _ _ _) = False
isElemLink (WinElemText _ _ _)  = False
isElemLink (WinElemTTF _ _ _ _)   = False
isElemLink (WinElemBack _ )     = False
isElemLink (WinElemDyn _ _)     = False
isElemLink (WinElemMenu _ _ _)  = False
isElemLink (WinElemNULL)        = False

moveSliderWithMouse ∷ Int → Anamnesis ε σ ()
moveSliderWithMouse n = do
  st ← get
  case (windowSt st) of
    Just win → do
      pos' ← liftIO $ GLFW.getCursorPos win
      let ls = luaSt st
          thisWin = currentWindow ls
          (x,_) = convertPixels pos'
          ds = drawSt st
      modify' $ \s → s { drawSt     = moveSlider x n ds
                       , luaSt      = moveLSSlider x n ls}
      return ()
    Nothing → return ()

moveCamWithKeys ∷ Anamnesis ε σ ()
moveCamWithKeys = do
  env ← ask
  st ←  get
  let ls      = luaSt st
      currWin = currentWindow ls
  if ((winType currWin) ≡ WinTypeGame) then do
    case (findWorldData currWin) of
      Just (_,wd) → do
        let oldIS    = inputState st
            dir      = case (findDir oldIS) of
                         Just d  → d
                         Nothing → CardNULL
            oldcam   = winCursor currWin
            newIS    = oldIS { keyAccel = newaccel }
            newaccel = decell $ accelIS dir (keyAccel oldIS)
            newcam   = keyMoveCam newaccel oldcam
            newSC    = moveScreenCursor newcam
            newWD    = wd { wdCam = newSC }
            newWin'  = replaceWorldData currWin newWD
            newWin   = newWin' { winCursor = newcam }
            newWins  = findAndReplaceWindow newWin (luaWindows ls)
            newLS   = ls { luaWindows = newWins }
        if ((newaccel) ≡ (0.0,0.0)) then liftIO (atomically (writeQueue (envLCmdChan env) (LoadCmdWorld newLS))) else return ()
        modify' $ \s → s { luaSt = newLS
                         , inputState = newIS }
      Nothing     → return ()
  else return ()

-- many keys can be held at once,
-- we define bahavior of all
-- combinations here
findDir ∷ InputState → Maybe Cardinal
findDir is = if      (keyUp    is) ∧ (keyLeft  is) ∧ (keyRight is) ∧ (keyDown  is) then Nothing
             else if (keyUp    is) ∧ (keyLeft  is) ∧ (keyRight is) then Just North
             else if (keyUp    is) ∧ (keyLeft  is) ∧ (keyDown  is) then Just West
             else if (keyUp    is) ∧ (keyRight is) ∧ (keyDown  is) then Just East
             else if (keyUp    is) ∧ (keyLeft  is) then Just NorthWest
             else if (keyUp    is) ∧ (keyRight is) then Just NorthEast
             else if (keyUp    is) ∧ (keyDown  is) then Nothing
             else if (keyUp    is) then Just North
             else if (keyDown  is) ∧ (keyLeft  is) ∧ (keyRight is) then Just South
             else if (keyDown  is) ∧ (keyLeft  is) then Just SouthWest
             else if (keyDown  is) ∧ (keyRight is) then Just SouthEast
             else if (keyDown  is) then Just South
             else if (keyLeft  is) ∧ (keyRight is) then Nothing
             else if (keyLeft  is) then Just West
             else if (keyRight is) then Just East
             else Nothing

-- accelerate the inputstate
accelIS ∷ Cardinal → (Float,Float) → (Float,Float)
accelIS North (x,y) = (x, 1.1*(y - 0.1))
accelIS West  (x,y) = (1.1*(x + 0.1), y)
accelIS South (x,y) = (x, 1.1*(y + 0.1))
accelIS East  (x,y) = (1.1*(x - 0.1), y)
accelIS NorthWest (x,y) = (1.1*(x + 0.1), 1.1*(y - 0.1))
accelIS NorthEast (x,y) = (1.1*(x - 0.1), 1.1*(y - 0.1))
accelIS SouthWest (x,y) = (1.1*(x + 0.1), 1.1*(y + 0.1))
accelIS SouthEast (x,y) = (1.1*(x - 0.1), 1.1*(y + 0.1))
accelIS CardNULL (x,y) = (x,y)

decell ∷ (Float,Float) → (Float,Float)
decell (x,y)
  | ((abs x) < 0.01) ∧ ((abs y) < 0.01) = (0.0,0.0)
  | ((abs x) < 0.01) = (0.0,(y / 1.1))
  | ((abs y) < 0.01) = ((x / 1.1),0.0)
  | otherwise = ((x / 1.1),(y / 1.1))

keyMoveCam ∷ (Float,Float) → (Float,Float,Float) → (Float,Float,Float)
keyMoveCam (i,j) (x,y,z) = (x+i,y+j,z)

moveCamWithMouse ∷ Anamnesis ε σ ()
moveCamWithMouse = do
  st ← get
  let win' = windowSt st
  case win' of
    Just win → do
      let currWin = currentWindow ls
          ls = luaSt st
      if ((winType currWin) ≡ WinTypeGame) then do
        case (findWorldData currWin) of
          Just (_,wd) → do
            pos' ← liftIO $ GLFW.getCursorPos win
            let pos = ((realToFrac (fst pos')),(realToFrac (snd pos')))
                oldpos = mouse3Cache (inputState st)
                diff = (((fst pos)-(fst oldpos)),((snd pos)-(snd oldpos)))
                oldcam = winCursor currWin
                newcam = moveCam oldcam diff
                moveCam ∷ (Float,Float,Float) → (Float,Float) → (Float,Float,Float)
                moveCam (x1,y1,z1) (x2,y2) = (x1+x2',y1-y2',z1) where (x2',y2') = (x2/3.6,y2/3.6)
                oldIS = inputState st
                newIS = oldIS { mouse3 = True
                              , mouse3Cache = ((fst pos),(snd pos)) }
                newSC = moveScreenCursor newcam
                newWD = wd { wdCam = newSC }
                newWin' = replaceWorldData currWin newWD
                newWin  = newWin' { winCursor = newcam }
                newWins = findAndReplaceWindow newWin (luaWindows ls)
                newLS = ls { luaWindows = newWins }
            modify' $ \s → s { luaSt = newLS
                             , inputState = newIS }
          Nothing → return ()
      else return ()
    Nothing → return ()

moveScreenCursor ∷ (Float,Float,Float) → (Float,Float)
moveScreenCursor (x,y,_) = (-0.05*x,-0.05*y)

-- input state elems persist and must be deleted
addISElem ∷ InputElem → InputState → InputState
addISElem (IESlider b n) is = is { isElems = (isElems is) ⧺ [IESlider b n] }
addISElem (IESelect b n) is = is { isElems = (isElems is) ⧺ [IESelect b n] }
addISElem IENULL       is = is

toggleSliderIS ∷ Int → Bool → InputState → InputState
toggleSliderIS n s is = is { isElems = (testSliderIS n s (isElems is))
                           , inpCap  = False }
testSliderIS ∷ Int → Bool → [InputElem] → [InputElem]
testSliderIS _ _ []       = []
testSliderIS n s ((IESlider b i):ies)
  | n ≡ i = [IESlider s i] ⧺ testSliderIS n s ies
  | otherwise = [IESlider b i] ⧺ testSliderIS n s ies
testSliderIS n s (ie:ies) = [ie] ⧺ testSliderIS n s ies

turnOffISElems ∷ [InputElem] → [InputElem]
turnOffISElems []       = []
turnOffISElems (ie:ies) = [ie'] ⧺ turnOffISElems ies
  where ie' = case ie of
                IESlider b n → IESlider False n
                IESelect b n → IESelect b n
                IENULL       → IENULL

sliderPressed ∷ InputState → Int
sliderPressed is = sliderPressedF $ isElems is
sliderPressedF ∷ [InputElem] → Int
sliderPressedF [] = -1
sliderPressedF ((IESlider b n):ies) = if b then n else sliderPressedF ies
sliderPressedF (ie:ies) = sliderPressedF ies

toggleSelectIS ∷ Int → InputState → InputState
toggleSelectIS n is = is { isElems = toggleSelectISF n (isElems is)
                         , inpCap  = True }
toggleSelectISF ∷ Int → [InputElem] → [InputElem]
toggleSelectISF _ []       = []
toggleSelectISF n ((IESelect b i):ies)
  | (n ≡ i) = [ie'] ⧺ toggleSelectISF n ies
  -- all non selected boxes go false
  | otherwise = [IESelect False i] ⧺ toggleSelectISF n ies
  where ie' = IESelect (not b) i
toggleSelectISF n (ie:ies) = [ie] ⧺ toggleSelectISF n ies

selectedBox ∷ InputState → Int
selectedBox is = selectedBoxF (isElems is)
selectedBoxF ∷ [InputElem] → Int
selectedBoxF []       = -1
selectedBoxF ((IESelect b n):ies) = if b then n else selectedBoxF ies
selectedBoxF (ie:ies) = selectedBoxF ies

-- provides input to the selected element
inpSelectedElems ∷ InpElemData → [WinElem] → [WinElem]
inpSelectedElems _   []       = []
inpSelectedElems ied ((WinElemMenu name pos bits):wes) = [WinElemMenu name pos bits'] ⧺ inpSelectedElems ied wes
  where bits' = inpSelectedBits ied bits
inpSelectedElems ied (we:wes) = [we] ⧺ inpSelectedElems ied wes
inpSelectedBits ∷ InpElemData → [MenuBit] → [MenuBit]
inpSelectedBits _   []       = []
inpSelectedBits ied ((MenuSlider ind text range val True curs ci):mbs) = [MenuSlider ind text range val' True curs ci'] ⧺ inpSelectedBits ied mbs
  where ci' = case ied of
                  IEDAdd _ → case val of
                               Nothing → ci
                               Just v0 → if (v0 > 999) then ci else max 0 $ min (cursMax v0) (ci - 1)
                  IEDLeft  → case val of
                               Nothing → 0
                               Just v0 → max 0 $ min (cursMax v0) (ci + 1)
                  IEDRight → case val of
                               Nothing → 0
                               Just v0 → max 0 $ min (cursMax v0) (ci - 1)
                  _        → ci
        val' = case ied of
                  IEDAdd n → case val of
                               Nothing → Just n
                               Just v0 → addDecVal n ((cursMax v0) - ci) val
                  IEDDel   → case val of
                               Nothing → Nothing
                               Just v0 → delDecVal ((cursMax v0) - ci) val
                  _        → val
        cursMax ∷ Int → Int
        cursMax v
          | (v > 999) = 4
          | (v > 99)  = 3
          | (v > 9)   = 2
          | otherwise = 1
        addDecVal ∷ Int → Int → Maybe Int → Maybe Int
        addDecVal _  _ Nothing  = Nothing
        addDecVal n0 i (Just n) = Just $ read $ addVal n0 i $ show n
        delDecVal ∷ Int → Maybe Int → Maybe Int
        delDecVal _ Nothing  = Nothing
        delDecVal i (Just n) = case (delVal i (show n)) of
                                 ""  → Nothing
                                 str → Just $ read str
        addVal ∷ Int → Int → String → String
        addVal n0 i s
          | (length s > 3) = s
          | otherwise      = retl ⧺ retn ⧺ retr
          where retl = take i s
                retn = show n0
                retr = drop i s
        delVal ∷ Int → String → String
        delVal i s = retl ⧺ retr
          where retl = init $ take i s
                retr = drop i s
inpSelectedBits ied ((MenuSlider ind text range val False curs ci):mbs) = [MenuSlider ind text range val False curs ci] ⧺ inpSelectedBits ied mbs
inpSelectedBits ied (mb:mbs) = [mb] ⧺ inpSelectedBits ied mbs

