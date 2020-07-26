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
newTVar ∷ α → STM.STM (TVar α)
newTVar = STM.newTVar
readTVar ∷ TVar α → STM.STM α
readTVar = STM.readTVar
writeTVar ∷ TVar α → α → STM.STM ()
writeTVar = STM.writeTVar
-- taken from a random hackage
-- module i saw. strict,
-- requires bangpatterns
modifyTVar ∷ TVar α → (α → (α, β)) → STM.STM β
modifyTVar ref f = STM.readTVar ref ⌦ \a → f a & \(!a', !b) → STM.writeTVar ref a' ⚞ b
atomically ∷ STM.STM α → IO α
atomically = STM.atomically
