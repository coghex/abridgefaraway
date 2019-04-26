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
drawShell state env = resequence_ $ map (drawShellRow fonts fsize) $ stateShellBuff state
  where fonts    = envFonts env
        fsize    = settingFontSize settings
        settings = stateSettings state

-- draws a single row of the shell
drawShellRow :: [Font] -> Int -> String -> IO ()
drawShellRow fonts fsize str = do
  drawFont dfont fsize FNULL    (-1, 1) str
  where dfont    = fonts !! 2
