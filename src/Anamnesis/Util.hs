{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
module Anamnesis.Util where
-- utility functions for the
-- anamnesis monad are defined
-- debug and logging functions,
-- and some threading functions
import Prelude()
import UPrelude
import Control.Concurrent
import qualified Control.Monad.Logger.CallStack as LoggerCS
import Data.String (fromString)
import Data.Time.Clock.System
import Graphics.Vulkan
import GHC.Stack
import System.Exit
import Anamnesis
import Anamnesis.Data
import Artos
import Artos.Except
import Artos.Var

-- for c functions that have to run in the main
-- thread for as long as the program runs
occupyThreadAndFork ∷ Anamnesis ε σ () → Anamnesis' ε () → Anamnesis ε σ ()
occupyThreadAndFork mainProg deputyProg = Anamnesis $ \e s c → do
  mainThreadId ← myThreadId
  -- make new state, use same env
  threadState ← atomically $ newTVar ⌫ readTVar s
  _ ← Control.Concurrent.forkFinally (unAnamnate deputyProg e threadState pure ⌦ checkStatus) $ \case
    Left  ex → throwTo mainThreadId ex
    Right () → throwTo mainThreadId ExitSuccess
  unAnamnate mainProg e s c

-- allocates something before returning, if
-- exception occurs, freeing does not happen
allocResource ∷ (α → Anamnesis' ε ()) → Anamnesis ε σ α → Anamnesis ε σ α
allocResource free alloc = Anamnesis $ \e s c → unAnamnate alloc e s $ \case
  Left ex → c (Left ex)
  Right a → c (Right a) ⌦ \r → r ⚟ unAnamnate (free a) e s pure
{-# INLINE allocResource #-}
-- common case where we dont prepend
-- the release acton for finer control
allocResource' ∷ (α → Anamnesis' ε ()) → Anamnesis ε σ α → Anamnesis ε σ (α, Anamnesis ε σ ())
allocResource' free alloc = Anamnesis $ \e s c → unAnamnate alloc e s $ \case
  Left ex → c (Left ex)
  Right a → c (Right (a, Anamnesis $ \e' s' c' → c' (Right ()) ⌦ \r → r ⚟ unAnamnate (free a) e' s' pure))
{-# INLINE allocResource' #-}
-- run nested continuations locally frees
-- all resources, only for side effects 
locally ∷ Anamnesis' ε α → Anamnesis ε σ α
locally p = Anamnesis $ \e s c → unAnamnate p e s pure ⌦ c
{-# INLINE locally #-}
bracket ∷ Anamnesis ε σ α → (α → Anamnesis ε σ β) → (α → Anamnesis ε σ μ) → Anamnesis ε σ μ
bracket before after thing = do
  a  ← before
  er ← try $ thing a
  _  ← after a
  Anamnesis $ \_ _ → ($ er)
{-# INLINE bracket #-}
finally ∷ Anamnesis ε σ α → Anamnesis ε σ β → Anamnesis ε σ α
finally a sequal = do
  er ← try a
  _  ← sequal
  Anamnesis $ \_ _ → ($ er)
{-# INLINE finally #-}
try ∷ Anamnesis ε σ α → Anamnesis ε σ (Either AExcept α)
try a = Anamnesis $ \e s c → unAnamnate a e s $ c . Right
{-# INLINE try #-}
loop ∷ Anamnesis' ε LoopControl → Anamnesis ε σ ()
loop action = do
  status ← locally action
  if status ≡ ContinueLoop then loop action else return ()

getTime :: Anamnesis ε σ Double
getTime = do
  now <- liftIO getSystemTime
  start <- sStartTime <$> get
  let deltaSeconds      = systemSeconds now - systemSeconds start
      deltaNS :: Int64  = fromIntegral (systemNanoseconds now)  - fromIntegral (systemNanoseconds start)
      seconds :: Double = fromIntegral deltaSeconds + fromIntegral deltaNS / 1e9
  return seconds

-- debugging flags
isDev ∷ Bool
#ifdef DEVELOPMENT
isDev = True
#else
isDev = False
#endif
{-# INLINE isDev #-}
-- forces strictness
inDev ∷ Applicative m ⇒ m () → m ()
#ifdef DEVELOPMENT
inDev = id
#else
inDev = const (pure ())
#endif
{-# INLINE inDev #-}
-- logging functions
logDebug ∷ HasCallStack ⇒ String → Anamnesis ε σ ()
#ifdef DEVELOPMENT
logDebug = LoggerCS.logDebug ∘ fromString
#else
logDebug = const $ pure ()
#endif
{-# INLINE logDebug #-}
logInfo ∷ HasCallStack ⇒ String → Anamnesis ε σ ()
logInfo = LoggerCS.logInfo ∘ fromString
{-# INLINE logInfo #-}
logWarn ∷ HasCallStack ⇒ String → Anamnesis ε σ ()
logWarn = LoggerCS.logWarn ∘ fromString
{-# INLINE logWarn #-}
logError ∷ HasCallStack ⇒ String → Anamnesis ε σ ()
logError = LoggerCS.logError ∘ fromString
{-# INLINE logError #-}
logExcept ∷ (Exceptable ς, HasCallStack) ⇒ ς → ExType → String → Anamnesis ε σ α
logExcept ret exType msg = throwError $ AExcept (Just ret) exType (msg ⧺ "\n" ⧺ prettyCallStack callStack ⧺ "\n")
