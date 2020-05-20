{-# LANGUAGE MultiParamTypeClasses #-}
module Anamnesis
  ( Anamnesis(..), Anamnesis'
  , MonadIO(..), MonadError(..)
  , MonadReader(..), MonadState(..)
  ) where
-- the application monad is defined
import Control.Monad.IO.Class
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import qualified Control.Monad.Logger as Logger
import Data.IORef (IORef)
import Data.Tuple (swap)
import Artos.Except
import Artos.Var
import Anamnesis.Data
-- monadic typeclass instances
-- inlined for exporting
newtype Anamnesis ret env state a = Anamnesis { unAnamnate ∷ IORef (AExcept) → TVar Env → TVar State → (Either (AExcept) a → IO ret) → IO ret }
-- common case where the input
-- is an exception tuple
type Anamnesis' e s a = Anamnesis (Either AExcept a) a
instance Functor (Anamnesis ret env state) where
  fmap f p = Anamnesis $ \r e s c → unAnamnate p r e s (c . fmap f)
  {-# INLINE fmap #-}
instance Applicative (Anamnesis ret env state) where
  pure x = Anamnesis $ const $ const $ const ($ Right x)
  {-# INLINE pure #-}
  pf <*> px = Anamnesis $ \r e s c → unAnamnate pf r e s $ \g → unAnamnate px r e s (c . (g <*>))
  {-# INLINE (<*>) #-}
instance Monad (Anamnesis ret env state) where
  return = pure
  {-# INLINE return #-}
  px >>= k = Anamnesis $ \r e s c → unAnamnate px r e s $ \case
    Right x → unAnamnate (k x) r e s c
    Left ex → c (Left ex)
  {-# INLINE (>>=) #-}
-- other class instances
-- since we always run in IO, we
-- can add this to provide liftIO
instance MonadIO (Anamnesis ret env state) where
  liftIO m = Anamnesis $ const $ const $ const (Right <$> m >>=)
  {-# INLINE liftIO #-}
-- throws errors into the result
instance MonadError AExcept (Anamnesis ret env state) where
  throwError e = Anamnesis $ const $ const $ const ($ Left e)
  {-# INLINE throwError #-}
  catchError px catcher = Anamnesis $ \r e s c → unAnamnate px r e s $ \case
    Left  ex  → unAnamnate (catcher ex) r e s c
    Right res → c (Right res)
  {-# INLINE catchError #-}
-- here we define the monadic
-- references to the internal data
instance MonadReader Env (Anamnesis ret env state) where
  ask = Anamnesis $ \_ e _ → (Right <$> atomically (readTVar e) >>=)
  {-# INLINE ask #-}
  -- this is a placeholder
  local _ a = a
  {-# INLINE local #-}
instance MonadState State (Anamnesis ret env state) where
  get = Anamnesis $ \_ _ st → (Right <$> atomically (readTVar st) >>=)
  {-# INLINE get #-}
  put s = Anamnesis $ \_ _ st → (Right <$> atomically (writeTVar st s) >>=)
  {-# INLINE put #-}
  state f = Anamnesis $ \_ _ st → (Right <$> atomically (modifyTVar st (swap . f)) >>=)
  {-# INLINE state #-}
instance Logger.MonadLogger (Anamnesis ret env state) where
  monadLoggerLog loc ls ll msg = do
    lf <- gets logFunc
    liftIO $ lf loc ls ll (Logger.toLogStr msg)
  {-# INLINE monadLoggerLog #-}
