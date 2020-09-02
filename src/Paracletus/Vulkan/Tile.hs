{-# LANGUAGE Strict #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Paracletus.Vulkan.Tile where
-- vulkan specific translations from
-- the tiles array to a bunch of vertices
import Prelude()
import UPrelude
import Graphics.Vulkan.Core_1_0
import Numeric.DataFrame
import Anamnesis.Draw
import Paracletus.Data
import Paracletus.Vulkan.Atlas
import Paracletus.Vulkan.Vertex

calcVertices ∷ DrawState → (DataFrame Vertex '[XN 0], DataFrame Word32 '[XN 3])
calcVertices ds' = (verts, inds)
  where verts = vertices      ts
        inds  = indices       ts
        ts    = dsTiles       ds
        ds    = calcFontTiles ds'

calcFontTiles ∷ DrawState → DrawState
calcFontTiles ds = ds { dsTiles = newTiles }
  where newTiles = (dsTiles ds) ⧺ addTextBoxs (dsTextB ds)
        addTextBoxs ∷ [TextBox] → [GTile]
        --addTextBoxs [] = []
        addTextBoxs [] = calcTextbox (-2) (-2) 4 4
        addTextBoxs (tb:tbs) = (addTextBox tb) ⧺ (addTextBoxs tbs)
        addTextBox ∷ TextBox → [GTile]
        addTextBox (TextBox _   "")     = []
        addTextBox (TextBox pos (c:st)) = (addTextBox newTB) ⧺ (tbTile pos c)
          where newTB = TextBox { tbPos = (newPos pos c)
                                , tbString = st }
        tbTile ∷ (Float, Float) → Char → [GTile]
        tbTile (x,y) c = [newTile c x y]
        newTile ∷ Char → Float → Float → GTile
        newTile c x y = GTile { tPos = (x,y)
                              , tScale = (1,1)
                              , tInd = fontIndex c
                              , tSize = (16,6)
                              , tT = 1 }
        newPos ∷ (Float, Float) → Char → (Float, Float)
        newPos (x,y) c = ((x+(fontOffset c)),y)

-- creates textbox of arbitrary size
calcTextbox ∷ Float → Float → Int → Int → [GTile]
calcTextbox x y sx sy = [topLeftTile] ⧺ (topRow sx) ⧺ [topRightTile]
  where sx' = fromIntegral sx
        topLeftTile = defaultGTile
                        { tPos   = (x,y)
                        , tScale = (0.5,0.5)
                        , tT     = 6 }
        topRightTile = defaultGTile
                         { tPos   = (x+(0.5*sx')+0.5,y)
                         , tScale = (0.5,0.5)
                         , tT     = 7 }
        topRow ∷ Int → [GTile]
        topRow 0  = []
        topRow width = [thisTile] ⧺ (topRow (width - 1))
          where thisTile = defaultGTile
                  { tPos   = (x+(0.5*width'),y)
                  , tScale = (0.5,0.5)
                  , tT     = 8 }
                width' = fromIntegral width

-- combines all GTiles into a dataframe
-- of vertices, preformed every frame
vertices ∷ [GTile] → DataFrame Vertex '[XN 0]
vertices ts = fromList $ combineVertices ts
  where vertsqs = [ S $ Vertex (vec3 (-1) (-1) 0) (vec4 1 0 0 1) (vec3 0 1 0.1)
                  , S $ Vertex (vec3   1  (-1) 0) (vec4 0 1 0 1) (vec3 1 1 0.1)
                  , S $ Vertex (vec3   1    1  0) (vec4 0 0 1 1) (vec3 1 0 0.1)
                  , S $ Vertex (vec3 (-1)   1  0) (vec4 1 1 1 1) (vec3 0 0 0.1) ]
        combineVertices [] = []
        combineVertices (tile:tts) = withTC (indexAtlas ax ay sx sy) (withTC (+ vec3 0 0 t) (withPos (+ vec4 x y 0 0) (withScale (* vec3 xscale yscale 1) vertsqs))) ⧺ combineVertices tts
          where (x',y') = tPos tile
                ( x, y) = (2*x', 2*y')
                (ax', ay') = tInd tile
                ( ax,  ay) = (fromIntegral ax', fromIntegral ay')
                (xscale,yscale) = tScale tile
                (sx, sy) = tSize tile
                t = fromIntegral $ tT tile
        withPos f = map (\(S v) → S v { pos = fromHom ∘ f ∘ toHomPoint $ pos v })
        withTC f = map (\(S v) → S v { texCoord = f $ texCoord v })
        withScale f = map (\(S v) → S v { pos = f $ pos v})

indices ∷ [GTile] → DataFrame Word32 '[XN 3]
indices tiles = atLeastThree $ fromList $ (combineIndices tiles)
combineIndices ∷ ∀ a. (Num a) ⇒ [GTile] → [a]
combineIndices []           = []
combineIndices (_:tiles) = oneRectIndices ⧺ (map (+4) (combineIndices tiles))
  where oneRectIndices = [0,3,2,2,1,0]

-- these pattern matches translate to
-- a giant case statement where we find
-- the index and horizontal offset of
-- each character that is in the
-- standart tileset
fontIndex ∷ Char → (Int, Int)
fontIndex 'a'  = ( 0,0)
fontIndex 'b'  = ( 1,0)
fontIndex 'c'  = ( 2,0)
fontIndex 'd'  = ( 3,0)
fontIndex 'e'  = ( 4,0)
fontIndex 'f'  = ( 5,0)
fontIndex 'g'  = ( 6,0)
fontIndex 'h'  = ( 7,0)
fontIndex 'i'  = ( 8,0)
fontIndex 'j'  = ( 9,0)
fontIndex 'k'  = (10,0)
fontIndex 'l'  = (11,0)
fontIndex 'm'  = (12,0)
fontIndex 'n'  = (13,0)
fontIndex 'o'  = (14,0)
fontIndex 'p'  = (15,0)
fontIndex 'q'  = ( 0,1)
fontIndex 'r'  = ( 1,1)
fontIndex 's'  = ( 2,1)
fontIndex 't'  = ( 3,1)
fontIndex 'u'  = ( 4,1)
fontIndex 'v'  = ( 5,1)
fontIndex 'w'  = ( 6,1)
fontIndex 'x'  = ( 7,1)
fontIndex 'y'  = ( 8,1)
fontIndex 'z'  = ( 9,1)
fontIndex '?'  = (10,1)
fontIndex '!'  = (11,1)
fontIndex '('  = (12,1)
fontIndex ')'  = (13,1)
fontIndex '\'' = (14,1)
fontIndex '"'  = (15,1)
fontIndex 'A'  = ( 0,2)
fontIndex 'B'  = ( 1,2)
fontIndex 'C'  = ( 2,2)
fontIndex 'D'  = ( 3,2)
fontIndex 'E'  = ( 4,2)
fontIndex 'F'  = ( 5,2)
fontIndex 'G'  = ( 6,2)
fontIndex 'H'  = ( 7,2)
fontIndex 'I'  = ( 8,2)
fontIndex 'J'  = ( 9,2)
fontIndex 'K'  = (10,2)
fontIndex 'L'  = (11,2)
fontIndex 'M'  = (12,2)
fontIndex 'N'  = (13,2)
fontIndex 'O'  = (14,2)
fontIndex 'P'  = (15,2)
fontIndex 'Q'  = ( 0,3)
fontIndex 'R'  = ( 1,3)
fontIndex 'S'  = ( 2,3)
fontIndex 'T'  = ( 3,3)
fontIndex 'U'  = ( 4,3)
fontIndex 'V'  = ( 5,3)
fontIndex 'W'  = ( 6,3)
fontIndex 'X'  = ( 7,3)
fontIndex 'Y'  = ( 8,3)
fontIndex 'Z'  = ( 9,3)
fontIndex '.'  = (10,3)
fontIndex ':'  = (11,3)
fontIndex ','  = (12,3)
fontIndex ';'  = (13,3)
fontIndex '+'  = (14,3)
fontIndex '-'  = (15,3)
fontIndex '*'  = ( 0,4)
fontIndex '/'  = ( 1,4)
fontIndex '='  = ( 2,4)
fontIndex '1'  = ( 3,4)
fontIndex '2'  = ( 4,4)
fontIndex '3'  = ( 5,4)
fontIndex '4'  = ( 6,4)
fontIndex '5'  = ( 7,4)
fontIndex '6'  = ( 8,4)
fontIndex '7'  = ( 9,4)
fontIndex '8'  = (10,4)
fontIndex '9'  = (11,4)
fontIndex '0'  = (12,4)
fontIndex '%'  = (13,4)
fontIndex '&'  = (14,4)
--fontIndex ''   = (15,4)
fontIndex '`'  = ( 0,5)
fontIndex '~'  = ( 1,5)
fontIndex '@'  = ( 2,5)
fontIndex '#'  = ( 3,5)
fontIndex '$'  = ( 4,5)
fontIndex '^'  = ( 5,5)
fontIndex '_'  = ( 6,5)
fontIndex '\\' = ( 7,5)
fontIndex '|'  = ( 8,5)
fontIndex '<'  = ( 9,5)
fontIndex '>'  = (10,5)
fontIndex _    = (15,4)
fontOffset ∷ Char → Float
fontOffset 'a'  = 0.4
fontOffset 'b'  = 0.4
fontOffset 'c'  = 0.4
fontOffset 'd'  = 0.4
fontOffset 'e'  = 0.4
fontOffset 'f'  = 0.4
fontOffset 'g'  = 0.4
fontOffset 'h'  = 0.4
fontOffset 'i'  = 0.3
fontOffset 'j'  = 0.3
fontOffset 'k'  = 0.4
fontOffset 'l'  = 0.3
fontOffset 'm'  = 0.4
fontOffset 'n'  = 0.4
fontOffset 'o'  = 0.4
fontOffset 'p'  = 0.4
fontOffset 'q'  = 0.4
fontOffset 'r'  = 0.4
fontOffset 's'  = 0.4
fontOffset 't'  = 0.4
fontOffset 'u'  = 0.4
fontOffset 'v'  = 0.4
fontOffset 'w'  = 0.4
fontOffset 'x'  = 0.4
fontOffset 'y'  = 0.4
fontOffset 'z'  = 0.4
fontOffset '?'  = 0.4
fontOffset '!'  = 0.3
fontOffset '('  = 0.3
fontOffset ')'  = 0.3
fontOffset '\'' = 0.3
fontOffset '"'  = 0.3
fontOffset 'A'  = 0.5
fontOffset 'B'  = 0.5
fontOffset 'C'  = 0.5
fontOffset 'D'  = 0.5
fontOffset 'E'  = 0.5
fontOffset 'F'  = 0.5
fontOffset 'G'  = 0.5
fontOffset 'H'  = 0.5
fontOffset 'I'  = 0.4
fontOffset 'J'  = 0.5
fontOffset 'K'  = 0.5
fontOffset 'L'  = 0.5
fontOffset 'M'  = 0.5
fontOffset 'N'  = 0.5
fontOffset 'O'  = 0.5
fontOffset 'P'  = 0.5
fontOffset 'Q'  = 0.5
fontOffset 'R'  = 0.5
fontOffset 'S'  = 0.5
fontOffset 'T'  = 0.5
fontOffset 'U'  = 0.5
fontOffset 'V'  = 0.5
fontOffset 'W'  = 0.5
fontOffset 'X'  = 0.5
fontOffset 'Y'  = 0.5
fontOffset 'Z'  = 0.5
fontOffset '.'  = 0.3
fontOffset ':'  = 0.3
fontOffset ','  = 0.3
fontOffset ';'  = 0.3
fontOffset '+'  = 0.4
fontOffset '-'  = 0.4
fontOffset '*'  = 0.4
fontOffset '/'  = 0.3
fontOffset '='  = 0.4
fontOffset '1'  = 0.4
fontOffset '2'  = 0.4
fontOffset '3'  = 0.4
fontOffset '4'  = 0.4
fontOffset '5'  = 0.4
fontOffset '6'  = 0.4
fontOffset '7'  = 0.4
fontOffset '8'  = 0.4
fontOffset '9'  = 0.4
fontOffset '0'  = 0.4
fontOffset '%'  = 0.4
fontOffset '&'  = 0.4
--fontOffset ''   = 0.0
fontOffset '`'  = 0.3
fontOffset '~'  = 0.4
fontOffset '@'  = 0.4
fontOffset '#'  = 0.4
fontOffset '$'  = 0.4
fontOffset '^'  = 0.3
fontOffset '_'  = 0.4
fontOffset '\\' = 0.3
fontOffset '|'  = 0.3
fontOffset '<'  = 0.4
fontOffset '>'  = 0.4
fontOffset _    = 1.0
