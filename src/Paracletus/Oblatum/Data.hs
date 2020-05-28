module Paracletus.Oblatum.Data where
-- the data for the input engine is defined
data KeyLayout = KeyLayout
  { keyC   ∷ String
  , keyR   ∷ String
  , keySPC ∷ String
  , keyESC ∷ String
  , keyRET ∷ String
  , keyDEL ∷ String
  , keySH  ∷ String
  , keyLFT ∷ String
  , keyRGT ∷ String
  , keyUPP ∷ String
  , keyDWN ∷ String
  } deriving (Eq, Show)
