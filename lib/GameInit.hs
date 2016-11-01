module GameInit where

import State

gameInit :: Env -> State -> IO ()
gameInit env state = do
  print "hi"
