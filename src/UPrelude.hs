{-# LANGUAGE ExplicitForAll #-}
module UPrelude
  ( module Prelude
  , module Prelude.Unicode
  , module UPrelude
  , module Control.Applicative.Unicode
  , module Control.Monad.Unicode ) where
-- the prelude is modified for
-- custom symbols and typesynonyms,
-- no logic goes in here
import Prelude.Unicode
import qualified Prelude as P
import Prelude hiding ((>>=), (=<<))
import qualified Data.Bits as B
import qualified Data.Functor as F
import qualified System.FilePath as FP
import Control.Monad.Unicode
import Control.Applicative.Unicode

-- this function should be in prelude
flatten ∷ [[α]] → [α]
flatten xs = (\z n → foldr (\x y → foldr z y x) n xs) (:) []

-- fixity
infixl 7 ⌃
infixl 5 ⌄
infixl 1 ⌦
infixl 1 ⌫
infixl 4 ⚟
infixl 4 ⚞
infixl 4 ⊚
infixr 5 ⊘
infixr 7 ⊙

-- bitwise operators
(⌃) ∷ B.Bits a ⇒ a → a → a
(⌃) = (B..&.)
{-# INLINE (⌃) #-}
(⌄) ∷ B.Bits a ⇒ a → a → a
(⌄) = (B..|.)
{-# INLINE (⌄) #-}
-- functor sequencing
(⚟) ∷ Functor f ⇒ a → f b → f a
(⚟) = (F.<$)
{-# INLINE (⚟) #-}
(⚞) ∷ Functor f ⇒ f a → b → f b
(⚞) = (F.$>)
{-# INLINE (⚞) #-}
(⊚) ∷ Functor f ⇒ (a → b) → f a → f b
(⊚) = (F.<$>)
{-# INLINE (⊚) #-}
-- shortens monadic sequencing
(⌦) ∷ Monad m ⇒ ∀ a b. m a → (a → m b) → m b
(⌦) = (P.>>=)
{-# INLINE (⌦) #-}
(⌫) ∷ Monad m ⇒ (a → m b) → m a → m b
(⌫) = (P.=<<)
{-# INLINE (⌫) #-}
-- filepath operators
(⊘) ∷ FilePath → FilePath → FilePath
(⊘) = (FP.</>)
{-# INLINE (⊘) #-}
(⊙) ∷ FilePath → String → FilePath
(⊙) = (FP.<.>)
{-# INLINE (⊙) #-}
