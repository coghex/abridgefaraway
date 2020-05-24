{-# LANGUAGE BangPatterns #-}
module Artos.Var where
-- thread safe memory is defined
import Prelude ()
import UPrelude
import qualified Control.Concurrent.STM as STM
import Data.Function ((&))
-- type synonyms for ease of use
type MVar = STM.TMVar
type TVar = STM.TVar
-- function synonyms
newTVar ∷ a → STM.STM (TVar a)
newTVar = STM.newTVar
readTVar ∷ TVar a → STM.STM a
readTVar = STM.readTVar
writeTVar ∷ TVar a → a → STM.STM ()
writeTVar = STM.writeTVar
-- taken from a random hackage
-- module i saw. strict,
-- requires bangpatterns
modifyTVar ∷ TVar a → (a → (a, b)) → STM.STM b
modifyTVar ref f = STM.readTVar ref ⌦ \a → f a & \(!a', !b) → STM.writeTVar ref a' ⚞ b
atomically ∷ STM.STM a → IO a
atomically = STM.atomically
