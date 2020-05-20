{-# LANGUAGE ExplicitForAll #-}
module UPrelude (module Prelude, module Prelude.Unicode, module UPrelude) where
-- the prelude is modified for
-- custom symbols and typesynonyms,
-- no logic goes in here
import Prelude.Unicode
import qualified Prelude as P
import Prelude hiding ((>>=), (=<<))

-- shortens monadic sequencing
(⌦) ∷ Monad m ⇒ ∀ a b. m a → (a → m b) → m b
(⌦) = (P.>>=)
-- basic class inheritance for
-- all data types
