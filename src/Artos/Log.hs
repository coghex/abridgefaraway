{-# LANGUAGE CPP #-}
module Artos.Log where
-- logging functions are defined
import Prelude()
import UPrelude
import qualified Control.Monad.Logger.CallStack as LoggerCS
import Data.String (fromString)
import GHC.Stack
import Anamnesis
-- debugging flags
isDev ∷ Bool
#ifdef DEVELOPMENT
isDev = True
#else
isDev = False
#endif
{-# INLINE isDev #-}
-- forces strict monadic application
inDev ∷ Applicative m ⇒ m () → m ()
#ifdef DEVELOPMENT
inDev = id
#else
inDev = const (pure ())
#endif
{-# INLINE inDev #-}
-- *** functions
logDebug ∷ HasCallStack ⇒ String → Anamnesis r e s ()
#ifdef DEVELOPMENT
logDebug = LoggerCS.logDebug ∘ fromString
#else
logDebug = const (pure ())
#endif
{-# INLINE logDebug #-}
logInfo ∷ HasCallStack ⇒ String → Anamnesis r e s ()
logInfo = LoggerCS.logInfo ∘ fromString
{-# INLINE logInfo #-}
logWarn ∷ HasCallStack ⇒ String → Anamnesis r e s ()
logWarn = LoggerCS.logWarn ∘ fromString
{-# INLINE logWarn #-}
logError ∷ HasCallStack ⇒ String → Anamnesis r e s ()
logError = LoggerCS.logError ∘ fromString
{-# INLINE logError #-}
