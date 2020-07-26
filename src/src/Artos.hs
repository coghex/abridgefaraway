{-# LANGUAGE Strict #-}
module Artos where
import System.Exit
import Artos.Except
-- fails IO, thus everything,
-- when an error is returned
-- by any of the submodules.
checkStatus ∷ Either AExcept () → IO ()
checkStatus (Right ()) = pure ()
checkStatus (Left err) = do
  putStrLn $ displayException err
  exitFailure

