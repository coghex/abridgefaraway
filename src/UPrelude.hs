{-# LANGUAGE ExplicitForAll #-}
module UPrelude (module Prelude, module Prelude.Unicode, module UPrelude, module Control.Monad.Unicode) where
-- the prelude is modified for
-- custom symbols and typesynonyms,
-- no logic goes in here
import Prelude.Unicode
import Control.Monad.Unicode
import qualified Prelude as P
import Prelude hiding ((>>=), (=<<))

-- shortens monadic sequencing
(⌦) ∷ Monad m ⇒ ∀ a b. m a → (a → m b) → m b
(⌦) = (P.>>=)
