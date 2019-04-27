module ABFA.Shell where
-- the lua shell is defined

import qualified Foreign.Lua as Lua
import qualified Data.ByteString.Char8 as BL
import GLUtil.Font
import GLUtil.Util
import GLUtil.UI
import ABFA.State
import ABFA.Settings
import ABFA.Game
import ABFA.Data

-- executes a string in the lua state
execShell :: Lua.State -> String -> IO (String)
execShell ls str = do
  Lua.runWith ls $ Lua.openlibs
  error <- Lua.runWith ls $ Lua.loadstring $ BL.pack str
  res <- Lua.runWith ls $ Lua.pcall 0 1 Nothing
  ret <- Lua.runWith ls $ Lua.tostring' $ Lua.nthFromBottom (-1)
  Lua.runWith ls $ Lua.pop $ Lua.nthFromBottom (-1)
  return $ (show error) ++ ": " ++ (BL.unpack ret)

-- draws a shell 
drawShell :: State -> Env -> IO ()
drawShell state env = do
  drawTextBox boxtex screenw screenh (-1) (28) fsize fsize sizex sizey
  drawShellRow fonts fsize 1 shellinp $ stateShellBuff state
  where fonts    = envFonts env
        fsize    = settingFontSize settings
        settings = stateSettings state
        shellinp = stateShellInput state
        boxtex   = (envUTex env) !! 1
        screenw  = settingScreenW settings
        screenh  = settingScreenH settings
        sizex    = round $ 32.0*((fromIntegral screenw) / (fromIntegral screenh))
        sizey    = round $ 62.0*((fromIntegral screenh) / (fromIntegral screenw))

-- draws a single row of the shell
drawShellRow :: [Font] -> Int -> Int -> String -> [String] -> IO ()
drawShellRow fonts fsize _ shellinp []         = drawFont dfont fsize FNULL (-6, (fromIntegral (-5))) shellinp
  where dfont    = fonts !! 1
drawShellRow fonts fsize n shellinp (str:strs) = do
  drawFont dfont fsize FNULL (-8, (fromIntegral (n-6))) str
  drawShellRow fonts fsize (n+1) shellinp strs
  where dfont    = fonts !! 1

-- deletes the input when user types
inputDelete :: String -> String
inputDelete "" = ""
inputDelete s  = init s
