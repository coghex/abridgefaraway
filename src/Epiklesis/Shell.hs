{-# LANGUAGE Strict #-}
module Epiklesis.Shell where
-- a basic shell for executing
-- lua commands is defined
import Prelude()
import UPrelude
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import Anamnesis.Data
import Epiklesis.Data
import Epiklesis.Elems
import Epiklesis.Module
import Paracletus.Data
import Paracletus.Oblatum.Data
import qualified Data.ByteString.Char8 as BL
import qualified Foreign.Lua as Lua

-- empty shell
initShell ∷ Shell
initShell = Shell "$> " False Nothing 1 False "" "" "" (-1) []

-- executes lua command in state
evalShell ∷ Env → LuaState → IO LuaState
evalShell env ls = do
  let oldSh = luaShell ls
  (ret,outbuff) ← execShell env (luaState ls) (shInpStr oldSh)
  let retstring = (shOutStr oldSh) ⧺ (shPrompt oldSh) ⧺ (shInpStr oldSh) ⧺ "\n" ⧺ (show ret) ⧺ " > " ⧺ outbuff ⧺ "\n"
      newSh = oldSh { shInpStr = ""
                    , shOutStr = retstring
                    , shTabbed = Nothing 
                    , shHistI  = -1
                    , shHist   = ([shInpStr oldSh] ⧺ shHist oldSh)
                    , shCursor = 0 }
  return ls { luaShell = newSh }

execShell ∷ Env → Lua.State → String → IO (Lua.Status,String)
execShell env ls str = do
  Lua.runWith ls $ do
    Lua.openlibs
    loadModuleFunctions env
  luaerror ← Lua.runWith ls $ Lua.loadstring $ BL.pack str
  _   ← Lua.runWith ls $ Lua.pcall 0 1 Nothing
  ret ← Lua.runWith ls $ Lua.tostring' $ Lua.nthFromBottom (-1)
  Lua.runWith ls $ Lua.pop $ Lua.nthFromBottom (-1)
  return $ (luaerror,(BL.unpack ret))

-- produce graphics tiles
genShell ∷ Shell → [GTile]
genShell sh = case (shOpen sh) of
                True  → (addTextBox posOffset size) ⧺ (addText False (fst pos) pos' str) ⧺ (addText False (fst pos) cpos cursor)
                False → []
  where size      = (32,18)
        posOffset = ((fst pos) - 1.0, (snd pos) + 0.5)
        pos       = (-7.0,4)
        pos'      = (-7.0,4)
        cpos      = (-7.0 + cursPos,4)
        str       = genShellStr sh--[shPrompt sh]
        cursor    = case (shCBlink sh) of
                        True  → "|"
                        False → ""
        cursPos   = findCursPos (shInpStr sh) (shCursor sh)
genShellStr ∷ Shell → String
genShellStr sh
  | (height > 8) = shortret
  | otherwise    = retstring
  where prompt    = shPrompt sh
        strsin    = shInpStr sh
        strsout   = shOutStr sh
        height    = length $ filter (≡ '\n') retstring
        retstring = strsout ⧺ prompt ⧺ strsin
        shortret  = flattenWith '\n' $ drop (height - 8) (splitOn "\n" retstring)
        flattenWith ∷ Char → [String] → String
        flattenWith _  []         = ""
        flattenWith ch (str:strs) = str ⧺ [ch] ⧺ flattenWith ch strs

findCursPos ∷ String → Int → Double
findCursPos str curs = foldl (+) 0.9 $ map fontOffset str'
  where str' = take curs str

removeShellString ∷ Shell → Shell
removeShellString sh = sh { shTabbed = Nothing
                          , shInpStr = newStr
                          , shCursor = max 0 ((shCursor sh) - 1) }
  where newStr = myInit (take (shCursor sh) (shInpStr sh)) ⧺ (drop (shCursor sh) (shInpStr sh))
        myInit ""  = ""
        myInit str = init str

addShellString ∷ Shell → String → Shell
addShellString sh str = sh { shTabbed = Nothing
                           , shInpStr = (shInpStr sh) ⧺ str
                           , shCursor = (shCursor sh) + 1
                           , shCBlink = False }

-- reveals on screen
openSh ∷ Shell → Bool → Shell
openSh sh on = sh { shOpen = on }

-- cycles through commands with tab
tabShell ∷ Shell → [String] → Shell
tabShell sh cmds
  | shTabbed sh ≡ Nothing =
      sh { shCache  = shInpStr sh
         , shInpStr = tabCommand 0 (shInpStr sh) cmds
         , shTabbed = Just 0 }
  | otherwise             =
      sh { shTabbed = Just incSh
         , shInpStr = tabCommand incSh (shCache sh) cmds }
    where incSh = incShTabbed $ shTabbed sh

incShTabbed ∷ Maybe Int → Int
incShTabbed Nothing  = 0
incShTabbed (Just n) = (n+1)

tabCommand ∷ Int → String → [String] → String
tabCommand n inpStr cmds
  | matchedStrings ≡ [] = inpStr
  | otherwise           = matchedStrings !! (n `mod` (length matchedStrings))
  where matchedStrings = filter (isPrefixOf inpStr) cmds

-- adds lua output of commands to the shell
outputToShell ∷ Shell → String → Shell
outputToShell sh str = sh { shOutStr = (init (shOutStr sh)) ⧺ " " ⧺ str ⧺ "\n" }

-- cycles through the shell history
upShell ∷ Shell → Shell
upShell sh
  | shHist sh ≡ [] = sh
  | otherwise      = sh { shInpStr = (shHist sh) !! (incShHist `mod` (length (shHist sh)))
                        , shHistI  = incShHist }
  where incShHist = (shHistI sh) + 1

downShell ∷ Shell → Shell
downShell sh
  | shHist sh ≡ [] = sh
  | shHistI sh ≥ 0 = sh { shInpStr = (shHist sh) !! ((shHistI sh) `mod` (length (shHist sh)))
                        , shHistI  = max (-1) ((shHistI sh) - 1) }
  | otherwise      = sh { shInpStr = "" }

-- moves the cursor in the shell
leftShell ∷ Shell → Shell
leftShell sh = sh { shCursor = max 0 ((shCursor sh) - 1)
                  , shCBlink = True }

rightShell ∷ Shell → Shell
rightShell sh = sh { shCursor = min (length (shInpStr sh)) ((shCursor sh) + 1)
                   , shCBlink = True }

-- resets the shell, used for ctrl-c
resetShell ∷ Shell → Shell
resetShell sh = sh { shInpStr = ""
                   , shOutStr = retstring
                   , shTabbed = Nothing 
                   , shHistI  = -1 }
  where retstring = (shOutStr sh) ⧺ (shPrompt sh) ⧺ shInpStr sh ⧺ "\n"

