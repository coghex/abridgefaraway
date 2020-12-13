module Paracletus.Oblatum.Font where
-- functions for loading fonts
--import Prelude()
--import UPrelude
import FreeType
import Control.Monad
import Data.Char
import Data.Word
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Storable
import Paracletus.Oblatum.Data

data FontTex = FontTex Int Int [Word8]

loadFTChar ∷ FilePath → Char → Int → IO FontTex
loadFTChar fp char px = do
  ft_With_FreeType $ \lib →
    ft_With_Face lib fp 0 $ \face → do
      isScalable ← FT_IS_SCALABLE face
      when isScalable $ ft_Set_Char_Size face 0 ((fromIntegral px) * 64) 0 0
      ft_Load_Char face (fromIntegral $ ord char) FT_LOAD_RENDER
      slot ← peek . frGlyph =<< peek face
      withBitmap lib (gsrBitmap slot) $ \bmap → do
        let bufferSize = fromIntegral $ (bRows bmap) * fromIntegral (bPitch bmap)
        buffr ← peekArray bufferSize $ bBuffer bmap
        --drawBitmap (fromIntegral $ bPitch bmap) buffr
        return $ FontTex (fromIntegral (bPitch bmap)) (fromIntegral (bRows bmap)) buffr

withBitmap ∷ FT_Library → FT_Bitmap → (FT_Bitmap → IO a) → IO a
withBitmap lib source f =
  if any (== bPixel_mode source)
       [ FT_PIXEL_MODE_MONO, FT_PIXEL_MODE_GRAY2
       , FT_PIXEL_MODE_GRAY4, FT_PIXEL_MODE_BGRA ]
    then ft_Bitmap_With lib $ \targetPtr → do
           with source $ \sourcePtr → do
             ft_Bitmap_Convert lib sourcePtr targetPtr . fromIntegral $ bPixel_mode source
             f =<< peek targetPtr
    else f source

drawBitmap ∷ Int → [Word8] → IO ()
drawBitmap _ [] = return ()
drawBitmap n list = do
  putStrLn $ color <$> take n list
  drawBitmap n $ drop n list
  where
    color :: Word8 -> Char
    color a =
      case () of
        () | a == 0    -> ' '
           | a < 85    -> '░'
           | a < 170   -> '▒'
           | a < 255   -> '▓'
           | otherwise -> '█'

-- specific font data encoded here
chXUnit ∷ Double
chXUnit = 1.0 / 9.0
chYUnit ∷ Double
chYUnit = 1.0 / 7.0
indexTTF ∷ Char → TTFData
indexTTF 'a' = TTFData 85  (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (0.0*chYUnit)
indexTTF 'b' = TTFData 86  (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (0.0*chYUnit)
indexTTF 'c' = TTFData 87  (3.0*chXUnit) (5.0*chYUnit) (4.0*chXUnit)  (-0.5*chYUnit)
indexTTF 'd' = TTFData 88  (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (0.0*chYUnit)
indexTTF 'e' = TTFData 89  (3.0*chXUnit) (5.0*chYUnit) (4.0*chXUnit)  (-0.5*chYUnit)
indexTTF 'f' = TTFData 90  (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (0.0*chYUnit)
indexTTF 'g' = TTFData 91  (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (-2.0*chYUnit)
indexTTF 'h' = TTFData 92  (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (0.0*chYUnit)
indexTTF 'i' = TTFData 93  (1.0*chXUnit) (6.0*chYUnit) (2.0*chXUnit)  (0.0*chYUnit)
indexTTF 'j' = TTFData 94  (2.0*chXUnit) (7.0*chYUnit) (3.0*chXUnit)  (-2.0*chYUnit)
indexTTF 'k' = TTFData 95  (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (0.0*chYUnit)
indexTTF 'l' = TTFData 96  (1.0*chXUnit) (6.0*chYUnit) (2.0*chXUnit)  (0.0*chYUnit)
indexTTF 'm' = TTFData 97  (9.0*chXUnit) (5.0*chYUnit) (10.0*chXUnit) (-0.5*chYUnit)
indexTTF 'n' = TTFData 98  (5.0*chXUnit) (5.0*chYUnit) (6.0*chXUnit)  (-0.5*chYUnit)
indexTTF 'o' = TTFData 99  (4.0*chXUnit) (5.0*chYUnit) (5.0*chXUnit)  (-0.5*chYUnit)
indexTTF 'p' = TTFData 100 (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (-2.0*chYUnit)
indexTTF 'q' = TTFData 101 (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (-2.0*chYUnit)
indexTTF 'r' = TTFData 102 (3.0*chXUnit) (5.0*chYUnit) (4.0*chXUnit)  (-0.5*chYUnit)
indexTTF 's' = TTFData 103 (4.0*chXUnit) (5.0*chYUnit) (5.0*chXUnit)  (-0.5*chYUnit)
indexTTF 't' = TTFData 104 (3.0*chXUnit) (6.0*chYUnit) (4.0*chXUnit)  (0.0*chYUnit)
indexTTF 'u' = TTFData 105 (4.0*chXUnit) (5.0*chYUnit) (5.0*chXUnit)  (-0.5*chYUnit)
indexTTF 'v' = TTFData 106 (5.0*chXUnit) (5.0*chYUnit) (6.0*chXUnit)  (-0.5*chYUnit)
indexTTF 'w' = TTFData 107 (9.0*chXUnit) (5.0*chYUnit) (10.0*chXUnit) (-0.5*chYUnit)
indexTTF 'x' = TTFData 108 (4.0*chXUnit) (5.0*chYUnit) (5.0*chXUnit)  (-0.5*chYUnit)
indexTTF 'y' = TTFData 109 (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (-2.0*chYUnit)
indexTTF 'z' = TTFData 110 (4.0*chXUnit) (5.0*chYUnit) (5.0*chXUnit)  (-0.5*chYUnit)
indexTTF _   = TTFData 0   (1.0*chXUnit) (1.0*chYUnit) (2.0*chXUnit)  (0.0*chYUnit)
