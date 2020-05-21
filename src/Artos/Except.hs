{-# LANGUAGE ExistentialQuantification #-}
module Artos.Except
  ( AExcept(..), displayException
  ) where
-- exceptions are printed
import Control.Exception (Exception, displayException)
import Prelude()
import UPrelude
-- exceptions contain possibly a
-- code, and two strings. the
-- msg contains a possible error
-- message. exTy is the name of the
-- module throwing the exception
data AExcept = ∀ r. (Show r, Eq r) ⇒ AExcept
       { code ∷ Maybe r
       , msg  ∷ String
       , exTy ∷ String }
instance Show AExcept where
  show (AExcept (Just code) msg exTy) = show code ⧺ " " ⧺ msg ⧺ " " ⧺ exTy
  show (AExcept Nothing msg exTy) = show msg ⧺ exTy
instance Exception AExcept where
  displayException (AExcept Nothing msg exTy) = unlines
    [ ""
    , exTy ⧺ " exception:"
    , "*** " ⧺ msg ]
  displayException (AExcept (Just c) msg exTy) = unlines
    [ ""
    , exTy ⧺ " error: " ⧺ show c
    , "*** " ⧺ msg ]
