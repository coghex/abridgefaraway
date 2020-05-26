{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
module Artos.Except
  ( AExcept(..), Exceptable
  , displayException, testEx
  ) where
-- exceptions are printed
import Prelude()
import UPrelude
import Control.Exception (Exception, displayException)
import Data.Typeable (Typeable)
import Type.Reflection
-- exceptions contain possibly a
-- code, and two strings. the
-- msg contains a possible error
-- message. exTy is the name of the
-- module throwing the exception
type Exceptable r = (Typeable r, Eq r, Show r)
data AExcept = ∀ r. (Exceptable r) ⇒ AExcept
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

-- provides equality for exceptions
-- regardless of type
testEx ∷ ∀ r. Exceptable r ⇒ AExcept → r → Bool
testEx (AExcept (Just res) _ _) r = case eqTypeRep (typeRep @r) (typeOf res) of
  Just HRefl → res == r
  Nothing → False
testEx (AExcept Nothing _ _) _ = False
