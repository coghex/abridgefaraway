{-# LANGUAGE CPP #-}
module Artos.Log where
-- logging functions are defined
import qualified Control.Monad.Logger.CallStack as LoggerCS
import Data.String (fromString)
import GHC.Stack
import Anamnesis
logDebug ∷ HasCallStack ⇒ String → Anamnesis r e s ()
#ifdef DEVELOPMENT
logDebug = LoggerCS.logDebug . fromString
#else
logDebug = const (pure ())
#endif
{-# INLINE logDebug #-}
logInfo ∷ HasCallStack ⇒ String → Anamnesis r e s ()
logInfo = LoggerCS.logInfo . fromString
{-# INLINE logInfo #-}
logWarn ∷ HasCallStack ⇒ String → Anamnesis r e s ()
logWarn = LoggerCS.logWarn . fromString
{-# INLINE logWarn #-}
logError ∷ HasCallStack ⇒ String → Anamnesis r e s ()
logError = LoggerCS.logError . fromString
{-# INLINE logError #-}
