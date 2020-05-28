{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
module Anamnesis
  ( Anamnesis(..), Anamnesis'
  , MonadIO(..), MonadError(..)
  , MonadReader(..), MonadState(..)
  ) where
-- the application monad is defined
import UPrelude
import Control.Monad.IO.Class
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import qualified Control.Monad.Logger as Logger
import Data.Tuple (swap)
import Artos.Except
import Artos.Var
import Anamnesis.Data
-- monadic typeclass instances
-- inlined for exporting
-- ε = env, σ = state
-- α = action, ς = result
newtype Anamnesis ε σ α = Anamnesis {unAnamnate ∷ TVar Env → TVar State → (Either AExcept α → IO σ) → IO σ}
-- common case where σ is either
-- an action or and exception
type Anamnesis' ε α = Anamnesis ε (Either AExcept α) α
instance Functor (Anamnesis ε σ) where
  fmap f p = Anamnesis $ \e s c → unAnamnate p e s (c ∘ fmap f)
  {-# INLINE fmap #-}
instance Applicative (Anamnesis ε σ) where
  pure x = Anamnesis $ \_ _ → ($ Right x)
  {-# INLINE pure #-}
  pf <*> px = Anamnesis $ \e s c → unAnamnate pf e s $ \g → unAnamnate px e s (c ∘ (g <*>))
  {-# INLINE (<*>) #-}
instance Monad (Anamnesis ε σ) where
  return = pure
  {-# INLINE return #-}
  px >>= k = Anamnesis $ \e s c → unAnamnate px e s $ \case
    Right x → unAnamnate (k x) e s c
    Left ex → c (Left ex)
  {-# INLINE (>>=) #-}
instance MonadIO (Anamnesis ε σ) where
  liftIO m = Anamnesis $ \_ _ → (Right ⊚ m ⌦)
  {-# INLINE liftIO #-}
instance MonadError AExcept (Anamnesis ε σ) where
  throwError e = Anamnesis $ \_ _ → ($ Left e)
  {-# INLINE throwError #-}
  catchError px catcher = Anamnesis $ \e s c → unAnamnate px e s $ \case
    Left ex → unAnamnate (catcher ex) e s c
    Right r → c (Right r)
  {-# INLINE catchError #-}
instance MonadReader Env (Anamnesis ε σ) where
  ask = Anamnesis $ \e _ → (Right ⊚ atomically (readTVar e) ⌦)
  {-# INLINE ask #-}
  local _ a = a -- this is a placeholder
  {-# INLINE local #-}
instance MonadState State (Anamnesis ε σ) where
  get = Anamnesis $ \_ st → (Right ⊚ atomically (readTVar st) ⌦)
  {-# INLINE get #-}
  put s = Anamnesis $ \_ st → (Right ⊚ atomically (writeTVar st s) ⌦)
  {-# INLINE put #-}
  state f = Anamnesis $ \_ st → (Right ⊚ atomically (modifyTVar st (swap ∘ f)) ⌦)
  {-# INLINE state #-}
instance Logger.MonadLogger (Anamnesis ε σ) where
  monadLoggerLog loc ls ll msg = do
    lf ← gets logFunc
    liftIO $ lf loc ls ll (Logger.toLogStr msg)
  {-# INLINE monadLoggerLog #-}
