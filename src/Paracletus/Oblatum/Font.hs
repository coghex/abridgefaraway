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
indexTTF ∷ TextSize → Char → TTFData
indexTTF TextSize30px '!' = TTFData 21  (1.0*chXUnit) (7.0*chYUnit) (2.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px '"' = TTFData 22  (3.0*chXUnit) (3.0*chYUnit) (4.0*chXUnit)  (2.0*chYUnit)
indexTTF TextSize30px '#' = TTFData 23  (5.0*chXUnit) (5.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px '$' = TTFData 24  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px '%' = TTFData 25  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px '&' = TTFData 26  (7.0*chXUnit) (7.0*chYUnit) (8.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px '\'' = TTFData 27  (1.0*chXUnit) (2.0*chYUnit) (2.0*chXUnit)  (2.0*chYUnit)
indexTTF TextSize30px '(' = TTFData 28  (2.0*chXUnit) (7.0*chYUnit) (3.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px ')' = TTFData 29  (2.0*chXUnit) (7.0*chYUnit) (3.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px '*' = TTFData 30  (5.0*chXUnit) (5.0*chYUnit) (6.0*chXUnit)  (1.0*chYUnit)
indexTTF TextSize30px '+' = TTFData 31  (5.0*chXUnit) (5.0*chYUnit) (6.0*chXUnit)  (1.0*chYUnit)
indexTTF TextSize30px ',' = TTFData 32  (2.0*chXUnit) (3.0*chYUnit) (3.0*chXUnit)  (-2.0*chYUnit)
indexTTF TextSize30px '-' = TTFData 33  (5.0*chXUnit) (1.0*chYUnit) (6.0*chXUnit)  (2.0*chYUnit)
indexTTF TextSize30px '.' = TTFData 34  (1.0*chXUnit) (1.0*chYUnit) (2.0*chXUnit)  (-2.0*chYUnit)
indexTTF TextSize30px '/' = TTFData 35  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px '0' = TTFData 36  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px '1' = TTFData 37  (3.0*chXUnit) (7.0*chYUnit) (4.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px '2' = TTFData 38  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px '3' = TTFData 39  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px '4' = TTFData 40  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px '5' = TTFData 41  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px '6' = TTFData 42  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px '7' = TTFData 43  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px '8' = TTFData 44  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px '9' = TTFData 45  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px ':' = TTFData 46  (1.0*chXUnit) (4.0*chYUnit) (2.0*chXUnit)  (0.5*chYUnit)
indexTTF TextSize30px ';' = TTFData 47  (1.0*chXUnit) (5.0*chYUnit) (2.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px '<' = TTFData 48  (3.0*chXUnit) (5.0*chYUnit) (4.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px '=' = TTFData 49  (5.0*chXUnit) (3.0*chYUnit) (6.0*chXUnit)  (0.5*chYUnit)
indexTTF TextSize30px '>' = TTFData 50  (3.0*chXUnit) (5.0*chYUnit) (4.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px '?' = TTFData 51  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px '@' = TTFData 52  (7.0*chXUnit) (7.0*chYUnit) (8.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px 'A' = TTFData 53  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px 'B' = TTFData 54  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px 'C' = TTFData 55  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px 'D' = TTFData 56  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px 'E' = TTFData 57  (4.0*chXUnit) (7.0*chYUnit) (5.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px 'F' = TTFData 58  (3.0*chXUnit) (7.0*chYUnit) (4.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px 'G' = TTFData 59  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px 'H' = TTFData 60  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px 'I' = TTFData 61  (1.0*chXUnit) (7.0*chYUnit) (2.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px 'J' = TTFData 62  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px 'K' = TTFData 63  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px 'L' = TTFData 64  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px 'M' = TTFData 65  (9.0*chXUnit) (7.0*chYUnit) (10.0*chXUnit) (0.0*chYUnit)
indexTTF TextSize30px 'N' = TTFData 66  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px 'O' = TTFData 67  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px 'P' = TTFData 68  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px 'Q' = TTFData 69  (6.0*chXUnit) (7.0*chYUnit) (7.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px 'R' = TTFData 70  (6.0*chXUnit) (7.0*chYUnit) (7.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px 'S' = TTFData 71  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px 'T' = TTFData 72  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px 'U' = TTFData 73  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px 'V' = TTFData 74  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px 'W' = TTFData 75  (9.0*chXUnit) (7.0*chYUnit) (10.0*chXUnit) (0.0*chYUnit)
indexTTF TextSize30px 'X' = TTFData 76  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px 'Y' = TTFData 77  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px 'Z' = TTFData 78  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px '[' = TTFData 79  (2.0*chXUnit) (7.0*chYUnit) (3.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px '\\' = TTFData 80  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px ']' = TTFData 81  (2.0*chXUnit) (7.0*chYUnit) (3.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px '^' = TTFData 82  (5.0*chXUnit) (4.0*chYUnit) (6.0*chXUnit)  (2.0*chYUnit)
indexTTF TextSize30px '_' = TTFData 83  (7.0*chXUnit) (1.0*chYUnit) (8.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px '`' = TTFData 84  (2.0*chXUnit) (2.0*chYUnit) (3.0*chXUnit)  (2.0*chYUnit)
indexTTF TextSize30px 'a' = TTFData 85  (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px 'b' = TTFData 86  (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px 'c' = TTFData 87  (3.0*chXUnit) (5.0*chYUnit) (4.0*chXUnit)  (-0.5*chYUnit)
indexTTF TextSize30px 'd' = TTFData 88  (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px 'e' = TTFData 89  (3.0*chXUnit) (5.0*chYUnit) (4.0*chXUnit)  (-0.5*chYUnit)
indexTTF TextSize30px 'f' = TTFData 90  (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px 'g' = TTFData 91  (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (-2.0*chYUnit)
indexTTF TextSize30px 'h' = TTFData 92  (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px 'i' = TTFData 93  (1.0*chXUnit) (6.0*chYUnit) (2.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px 'j' = TTFData 94  (2.0*chXUnit) (7.0*chYUnit) (3.0*chXUnit)  (-2.0*chYUnit)
indexTTF TextSize30px 'k' = TTFData 95  (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px 'l' = TTFData 96  (1.0*chXUnit) (6.0*chYUnit) (2.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px 'm' = TTFData 97  (9.0*chXUnit) (5.0*chYUnit) (10.0*chXUnit) (-0.5*chYUnit)
indexTTF TextSize30px 'n' = TTFData 98  (5.0*chXUnit) (5.0*chYUnit) (6.0*chXUnit)  (-0.5*chYUnit)
indexTTF TextSize30px 'o' = TTFData 99  (4.0*chXUnit) (5.0*chYUnit) (5.0*chXUnit)  (-0.5*chYUnit)
indexTTF TextSize30px 'p' = TTFData 100 (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (-2.0*chYUnit)
indexTTF TextSize30px 'q' = TTFData 101 (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (-2.0*chYUnit)
indexTTF TextSize30px 'r' = TTFData 102 (3.0*chXUnit) (5.0*chYUnit) (4.0*chXUnit)  (-0.5*chYUnit)
indexTTF TextSize30px 's' = TTFData 103 (4.0*chXUnit) (5.0*chYUnit) (5.0*chXUnit)  (-0.5*chYUnit)
indexTTF TextSize30px 't' = TTFData 104 (3.0*chXUnit) (6.0*chYUnit) (4.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px 'u' = TTFData 105 (4.0*chXUnit) (5.0*chYUnit) (5.0*chXUnit)  (-0.5*chYUnit)
indexTTF TextSize30px 'v' = TTFData 106 (5.0*chXUnit) (5.0*chYUnit) (6.0*chXUnit)  (-0.5*chYUnit)
indexTTF TextSize30px 'w' = TTFData 107 (9.0*chXUnit) (5.0*chYUnit) (10.0*chXUnit) (-0.5*chYUnit)
indexTTF TextSize30px 'x' = TTFData 108 (4.0*chXUnit) (5.0*chYUnit) (5.0*chXUnit)  (-0.5*chYUnit)
indexTTF TextSize30px 'y' = TTFData 109 (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (-2.0*chYUnit)
indexTTF TextSize30px 'z' = TTFData 110 (4.0*chXUnit) (5.0*chYUnit) (5.0*chXUnit)  (-0.5*chYUnit)
indexTTF TextSize30px '{' = TTFData 111 (3.0*chXUnit) (7.0*chYUnit) (4.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px '|' = TTFData 112 (1.0*chXUnit) (7.0*chYUnit) (2.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px '}' = TTFData 113 (3.0*chXUnit) (7.0*chYUnit) (4.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize30px '~' = TTFData 114 (5.0*chXUnit) (3.0*chYUnit) (6.0*chXUnit)  (1.0*chYUnit)
indexTTF TextSize16px '!' = TTFData 115 (1.0*chXUnit) (7.0*chYUnit) (2.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px '"' = TTFData 116 (3.0*chXUnit) (3.0*chYUnit) (4.0*chXUnit)  (2.0*chYUnit)
indexTTF TextSize16px '#' = TTFData 117 (5.0*chXUnit) (5.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px '$' = TTFData 118 (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px '%' = TTFData 119 (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px '&' = TTFData 120 (7.0*chXUnit) (7.0*chYUnit) (8.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px '\'' = TTFData 121 (1.0*chXUnit) (2.0*chYUnit) (2.0*chXUnit)  (2.0*chYUnit)
indexTTF TextSize16px '(' = TTFData 122 (2.0*chXUnit) (7.0*chYUnit) (3.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px ')' = TTFData 123 (2.0*chXUnit) (7.0*chYUnit) (3.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px '*' = TTFData 124 (5.0*chXUnit) (5.0*chYUnit) (6.0*chXUnit)  (1.0*chYUnit)
indexTTF TextSize16px '+' = TTFData 125 (5.0*chXUnit) (5.0*chYUnit) (6.0*chXUnit)  (1.0*chYUnit)
indexTTF TextSize16px ',' = TTFData 126 (2.0*chXUnit) (3.0*chYUnit) (3.0*chXUnit)  (-2.0*chYUnit)
indexTTF TextSize16px '-' = TTFData 127 (5.0*chXUnit) (1.0*chYUnit) (6.0*chXUnit)  (2.0*chYUnit)
indexTTF TextSize16px '.' = TTFData 128 (1.0*chXUnit) (1.0*chYUnit) (2.0*chXUnit)  (-2.0*chYUnit)
indexTTF TextSize16px '/' = TTFData 129 (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px '0' = TTFData 130 (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px '1' = TTFData 131 (3.0*chXUnit) (7.0*chYUnit) (4.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px '2' = TTFData 132 (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px '3' = TTFData 133 (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px '4' = TTFData 134 (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px '5' = TTFData 135 (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px '6' = TTFData 136 (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px '7' = TTFData 137 (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px '8' = TTFData 138 (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px '9' = TTFData 139 (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px ':' = TTFData 140 (1.0*chXUnit) (4.0*chYUnit) (2.0*chXUnit)  (0.5*chYUnit)
indexTTF TextSize16px ';' = TTFData 141 (1.0*chXUnit) (5.0*chYUnit) (2.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px '<' = TTFData 142 (3.0*chXUnit) (5.0*chYUnit) (4.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px '=' = TTFData 143 (5.0*chXUnit) (3.0*chYUnit) (6.0*chXUnit)  (0.5*chYUnit)
indexTTF TextSize16px '>' = TTFData 144 (3.0*chXUnit) (5.0*chYUnit) (4.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px '?' = TTFData 145 (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px '@' = TTFData 146 (7.0*chXUnit) (7.0*chYUnit) (8.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px 'A' = TTFData 147 (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px 'B' = TTFData 148 (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px 'C' = TTFData 149 (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px 'D' = TTFData 150 (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px 'E' = TTFData 151 (4.0*chXUnit) (7.0*chYUnit) (5.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px 'F' = TTFData 152 (3.0*chXUnit) (7.0*chYUnit) (4.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px 'G' = TTFData 153 (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px 'H' = TTFData 154 (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px 'I' = TTFData 155 (1.0*chXUnit) (7.0*chYUnit) (2.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px 'J' = TTFData 156 (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px 'K' = TTFData 157 (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px 'L' = TTFData 158 (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px 'M' = TTFData 159 (9.0*chXUnit) (7.0*chYUnit) (10.0*chXUnit) (0.0*chYUnit)
indexTTF TextSize16px 'N' = TTFData 160 (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px 'O' = TTFData 161 (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px 'P' = TTFData 162 (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px 'Q' = TTFData 163 (6.0*chXUnit) (7.0*chYUnit) (7.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px 'R' = TTFData 164 (6.0*chXUnit) (7.0*chYUnit) (7.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px 'S' = TTFData 165 (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px 'T' = TTFData 166 (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px 'U' = TTFData 167 (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px 'V' = TTFData 168 (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px 'W' = TTFData 169 (9.0*chXUnit) (7.0*chYUnit) (10.0*chXUnit) (0.0*chYUnit)
indexTTF TextSize16px 'X' = TTFData 170 (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px 'Y' = TTFData 171 (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px 'Z' = TTFData 172 (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px '[' = TTFData 173 (2.0*chXUnit) (7.0*chYUnit) (3.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px '\\' = TTFData 174 (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px ']' = TTFData 175 (2.0*chXUnit) (7.0*chYUnit) (3.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px '^' = TTFData 176 (5.0*chXUnit) (4.0*chYUnit) (6.0*chXUnit)  (2.0*chYUnit)
indexTTF TextSize16px '_' = TTFData 177 (7.0*chXUnit) (1.0*chYUnit) (8.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px '`' = TTFData 178 (2.0*chXUnit) (2.0*chYUnit) (3.0*chXUnit)  (2.0*chYUnit)
indexTTF TextSize16px 'a' = TTFData 179 (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px 'b' = TTFData 180 (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px 'c' = TTFData 181 (3.0*chXUnit) (5.0*chYUnit) (4.0*chXUnit)  (-0.5*chYUnit)
indexTTF TextSize16px 'd' = TTFData 182 (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px 'e' = TTFData 183 (3.0*chXUnit) (5.0*chYUnit) (4.0*chXUnit)  (-0.5*chYUnit)
indexTTF TextSize16px 'f' = TTFData 184 (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px 'g' = TTFData 185 (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (-2.0*chYUnit)
indexTTF TextSize16px 'h' = TTFData 186 (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px 'i' = TTFData 187 (1.0*chXUnit) (6.0*chYUnit) (2.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px 'j' = TTFData 188 (2.0*chXUnit) (7.0*chYUnit) (3.0*chXUnit)  (-2.0*chYUnit)
indexTTF TextSize16px 'k' = TTFData 189 (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px 'l' = TTFData 190 (1.0*chXUnit) (6.0*chYUnit) (2.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px 'm' = TTFData 191 (9.0*chXUnit) (5.0*chYUnit) (10.0*chXUnit) (-0.5*chYUnit)
indexTTF TextSize16px 'n' = TTFData 192 (5.0*chXUnit) (5.0*chYUnit) (6.0*chXUnit)  (-0.5*chYUnit)
indexTTF TextSize16px 'o' = TTFData 193 (4.0*chXUnit) (5.0*chYUnit) (5.0*chXUnit)  (-0.5*chYUnit)
indexTTF TextSize16px 'p' = TTFData 194 (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (-2.0*chYUnit)
indexTTF TextSize16px 'q' = TTFData 195 (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (-2.0*chYUnit)
indexTTF TextSize16px 'r' = TTFData 196 (3.0*chXUnit) (5.0*chYUnit) (4.0*chXUnit)  (-0.5*chYUnit)
indexTTF TextSize16px 's' = TTFData 197 (4.0*chXUnit) (5.0*chYUnit) (5.0*chXUnit)  (-0.5*chYUnit)
indexTTF TextSize16px 't' = TTFData 198 (3.0*chXUnit) (6.0*chYUnit) (4.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px 'u' = TTFData 199 (4.0*chXUnit) (5.0*chYUnit) (5.0*chXUnit)  (-0.5*chYUnit)
indexTTF TextSize16px 'v' = TTFData 200 (5.0*chXUnit) (5.0*chYUnit) (6.0*chXUnit)  (-0.5*chYUnit)
indexTTF TextSize16px 'w' = TTFData 201 (9.0*chXUnit) (5.0*chYUnit) (10.0*chXUnit) (-0.5*chYUnit)
indexTTF TextSize16px 'x' = TTFData 202 (4.0*chXUnit) (5.0*chYUnit) (5.0*chXUnit)  (-0.5*chYUnit)
indexTTF TextSize16px 'y' = TTFData 203 (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (-2.0*chYUnit)
indexTTF TextSize16px 'z' = TTFData 204 (4.0*chXUnit) (5.0*chYUnit) (5.0*chXUnit)  (-0.5*chYUnit)
indexTTF TextSize16px '{' = TTFData 205 (3.0*chXUnit) (7.0*chYUnit) (4.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px '|' = TTFData 206 (1.0*chXUnit) (7.0*chYUnit) (2.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px '}' = TTFData 207 (3.0*chXUnit) (7.0*chYUnit) (4.0*chXUnit)  (0.0*chYUnit)
indexTTF TextSize16px '~' = TTFData 208 (5.0*chXUnit) (3.0*chYUnit) (6.0*chXUnit)  (1.0*chYUnit)
indexTTF _            _   = TTFData 0   (1.0*chXUnit) (1.0*chYUnit) (2.0*chXUnit)  (0.0*chYUnit)
