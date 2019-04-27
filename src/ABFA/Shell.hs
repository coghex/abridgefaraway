module ABFA.Shell where
-- the ui elements and how they are drawn are defined

import GLUtil.Font
import GLUtil.Util
import ABFA.State
import ABFA.Settings
import ABFA.Game
import ABFA.Data

-- draws a shell 
drawShell :: State -> Env -> IO ()
drawShell state env = drawShellRow fonts fsize 1 shellinp $ stateShellBuff state
  where fonts    = envFonts env
        fsize    = settingFontSize settings
        settings = stateSettings state
        shellinp = stateShellInput state

-- draws a single row of the shell
drawShellRow :: [Font] -> Int -> Int -> String -> [String] -> IO ()
drawShellRow fonts fsize _ shellinp []         = drawFont dfont fsize FNULL    (-9, (fromIntegral (-6))) shellinp
  where dfont    = fonts !! 1
drawShellRow fonts fsize n shellinp (str:strs) = do
  drawFont dfont fsize FNULL    (-10, (fromIntegral (n-6))) str
  drawShellRow fonts fsize (n-1) shellinp strs
  where dfont    = fonts !! 1
