{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Artos.Except
  ( AExcept(..), ExType(..), Exceptable
  , displayException, testEx
  ) where
-- exceptions are printed
import Prelude()
import UPrelude
import Control.Exception (Exception, displayException)
import Type.Reflection
-- exceptions contain possibly a
-- code, and two strings. the
-- msg contains a possible error
-- message. exTy is the name of the
-- module throwing the exception
type Exceptable ς = (Typeable ς, Eq ς, Show ς)
data ExType = ExNull | ExAnamnesis | ExParacletus deriving (Show, Eq)
data AExcept = ∀ ς. (Exceptable ς) ⇒ AExcept
       { code ∷ Maybe ς
       , exTy ∷ ExType
       , msg  ∷ String }
instance Show AExcept where
  show (AExcept (Just code) exTy msg) = (show code) ⧺ " " ⧺ (show msg) ⧺ " " ⧺ (show exTy)
  show (AExcept Nothing exTy msg) = (show msg) ⧺ (show exTy)
  
instance Exception AExcept where
  displayException (AExcept Nothing exTy msg) = unlines
    [ ""
    , (show exTy) ⧺ " exception:"
    , "*** " ⧺ msg ]
  displayException (AExcept (Just c) exTy msg) = unlines
    [ ""
    , (show exTy) ⧺ " error: " ⧺ show c
    , "*** " ⧺ msg ]

-- provides equality for exceptions
-- regardless of type
testEx ∷ ∀ ς. Exceptable ς ⇒ AExcept → ς → Bool
testEx (AExcept (Just r) _ _) ex = case eqTypeRep (typeRep @ς) (typeOf r) of
  Just HRefl → r == ex
  Nothing → False
testEx (AExcept Nothing _ _) _ = False
