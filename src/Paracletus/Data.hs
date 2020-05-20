module Paracletus.Data
  ( ParacExcept(..), ParacResult(..)
  ) where
data ParacExcept = ParacExcept
       { code ∷ Maybe ParacResult
       , msg  ∷ String
       } deriving (Show)
data ParacResult = ParacSuccess | ParacError deriving (Show)
