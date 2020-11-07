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
import Anamnesis.Data
import Epiklesis.Data
import Paracletus.Data
import Paracletus.Vulkan.Atlas
import Paracletus.Vulkan.Vertex

calcVertices ∷ (Float,Float,Float) → DrawState → (DataFrame Vertex '[XN 0], DataFrame Word32 '[XN 3])
calcVertices cam ds' = (verts, inds)
  where verts = vertices cam  ts
        inds  = indices       ts
        ts    = dsTiles       ds
        ds    = calcFontTiles $ calcShell $ calcMouseBox ds'

calcShell ∷ DrawState → DrawState
calcShell (DrawState oldt oldtb oldmb ShellNULL) = DrawState oldt oldtb oldmb ShellNULL
calcShell ds = ds { dsTextB = newTextBs }
  where newTextBs = [newTextB] ⧺ (dsTextB ds)
        newTextB  = TextBox pos size True str
        str       = shString $ dsShell ds
        pos       = shPos $ dsShell ds
        size      = shSize $ dsShell ds

calcFontTiles ∷ DrawState → DrawState
calcFontTiles ds = ds { dsTiles = newTiles }
  where newTiles = (dsTiles ds) ⧺ (addTextBoxs (dsTextB ds))
        addTextBoxs ∷ [TextBox] → [GTile]
        addTextBoxs [] = []
        addTextBoxs (tb:tbs) = (addTextBox tb (tbPos tb)) ⧺ (addTextBoxs tbs)
        addTextBox ∷ TextBox → (Double,Double) → [GTile]
        addTextBox (TextBox _   size box "") (oX,oY)
          | box = calcTextbox (oX - 1) (oY + 0.5) (fst size) (snd size)
          | otherwise = []
        addTextBox (TextBox pos size box (c:st)) (oX, oY) = (addTextBox newTB (oX,oY)) ⧺ (tbTile pos c)
          where newTB = TextBox { tbPos = (newPos pos c oX)
                                , tbSize = size
                                , tbBox = box
                                , tbString = st }
        tbTile ∷ (Double,Double) → Char → [GTile]
        tbTile _     '\n' = []
        tbTile (x,y) c    = [newTile c x y]
        newTile ∷ Char → Double → Double → GTile
        newTile c x y = GTile { tPos = (x,y)
                              , tScale = (1,1)
                              , tInd = fontIndex c
                              , tSize = (16,6)
                              , tT = 1
                              , tMoves = False }
        newPos ∷ (Double,Double) → Char → Double → (Double,Double)
        newPos (_,y) '\n' oX = (oX,(y - 0.5))
        newPos (x,y) c    _  = ((x+(fontOffset c)),y)

calcMouseBox ∷ DrawState → DrawState
calcMouseBox (DrawState a b MBNULL sh) = DrawState a b MBNULL sh
calcMouseBox ds = ds { dsTiles = (dsTiles ds)⧺newTiles }
  where newTiles   = [topTile, bottomTile, leftTile, rightTile, trTile, tlTile, brTile, blTile]
        topTile    = GTile ttpos ((absfloor pos2diff) - bSize,bSize) (0,0) (1,1) 12 False
        bottomTile = GTile btpos ((absfloor pos2diff) - bSize,bSize) (0,0) (1,1) 15 False
        leftTile   = GTile ltpos (bSize,(absfloor pos1diff) - bSize) (0,0) (1,1) 11 False
        rightTile  = GTile rtpos (bSize,(absfloor pos1diff) - bSize) (0,0) (1,1) 18 False
        (pos1,pos2) = rtF (mbPos1 (dsMBox ds)) (mbPos2 (dsMBox ds))
        --pos1       = rtF $ mbPos1 $ dsMBox ds
        --pos2       = rtF $ mbPos2 $ dsMBox ds
        --pos1'      = mbPos1 $ dsMBox ds
        --pos2'      = mbPos2 $ dsMBox ds
        --pos1       = ((realToFrac (fst pos1')), (realToFrac (snd pos1')))
        --pos2       = ((realToFrac (fst pos2')), (realToFrac (snd pos2')))
        ttpos      = (((fst pos1) + (pos2diff / 2.0)),(snd pos1))
        btpos      = (((fst pos1) + (pos2diff / 2.0)),(snd pos2))
        ltpos      = ((fst pos1),((snd pos1)+(pos1diff / 2.0)))
        rtpos      = ((fst pos2),((snd pos1)+(pos1diff / 2.0)))
        pos1diff   = ((snd pos2) - (snd pos1))
        pos2diff   = ((fst pos2) - (fst pos1))
        bSize      = 0.2
        absfloor ∷ Double → Double
        absfloor x = case (x' > bSize) of
          True → x'
          False → bSize
          where x' = abs x
        rtF ∷ (Float,Float) → (Float,Float) → ((Double,Double),(Double,Double))
        rtF (x1,y1) (x2,y2) = ((x1',y1'),(x2out,y2out))
          where x1' = realToFrac x1
                y1' = realToFrac y1
                x2' = realToFrac x2
                y2' = realToFrac y2
                x2out = case (abs(x2'-x1') < (2.0*bSize)) of
                        True  → if      (x1' > x2') then x1' - (2.0*bSize)
                                else if (x1' < x2') then x1' + (2.0*bSize)
                                else x1'
                        False → x2'
                y2out = case (abs(y2'-y1') < (2.0*bSize)) of
                        True  → if      (y1' > y2') then y1' - (2.0*bSize)
                                else if (y1' < y2') then y1' + (2.0*bSize)
                                else y1'
                        False → y2'
        (tlTile,trTile,brTile,blTile) = checkMouseBoxDirection pos1 pos2
        checkMouseBoxDirection ∷ (Double,Double) → (Double,Double) → (GTile,GTile,GTile,GTile)
        checkMouseBoxDirection (px1,py1) (px2,py2)
          | ((px1 > px2) ∧ (py1 > py2)) = (tlTilese, trTilese, brTilese, blTilese)
          | ((px1 < px2) ∧ (py1 > py2)) = (tlTilesw, trTilesw, brTilesw, blTilesw)
          | ((px1 > px2) ∧ (py1 < py2)) = (tlTilene, trTilene, brTilene, blTilene)
          | ((px1 < px2) ∧ (py1 < py2)) = (tlTilenw, trTilenw, brTilenw, blTilenw)
          | otherwise                   = (tlTilefun, trTilefun, brTilefun, blTilefun)
              where tlTilefun  = GTile (10.0,10.0) (bSize,bSize) (0,0) (1,1) 14 False
                    trTilefun  = GTile (10.0,10.0) (bSize,bSize) (0,0) (1,1) 13 False
                    brTilefun  = GTile (10.0,10.0) (bSize,bSize) (0,0) (1,1) 16 False
                    blTilefun  = GTile (10.0,10.0) (bSize,bSize) (0,0) (1,1) 17 False
                    tlTilese   = GTile ((fst pos1),(snd pos1)) (bSize,bSize) (0,0) (1,1) 13 False
                    trTilese   = GTile ((fst pos2),(snd pos1)) (bSize,bSize) (0,0) (1,1) 14 False
                    brTilese   = GTile ((fst pos2),(snd pos2)) (bSize,bSize) (0,0) (1,1) 17 False
                    blTilese   = GTile ((fst pos1),(snd pos2)) (bSize,bSize) (0,0) (1,1) 16 False
                    tlTilesw   = GTile ((fst pos1) + 0.125,(snd pos1)) (bSize,bSize) (0,0) (1,1) 14 False
                    trTilesw   = GTile ((fst pos2) - 0.125,(snd pos1)) (bSize,bSize) (0,0) (1,1) 13 False
                    brTilesw   = GTile ((fst pos2) - 0.125,(snd pos2)) (bSize,bSize) (0,0) (1,1) 16 False
                    blTilesw   = GTile ((fst pos1) + 0.125,(snd pos2)) (bSize,bSize) (0,0) (1,1) 17 False
                    tlTilenw   = GTile ((fst pos1) + 0.125,(snd pos1) + 0.125) (bSize,bSize) (0,0) (1,1) 17 False
                    trTilenw   = GTile ((fst pos2) - 0.125,(snd pos1) + 0.125) (bSize,bSize) (0,0) (1,1) 16 False
                    brTilenw   = GTile ((fst pos2) - 0.125,(snd pos2) - 0.125) (bSize,bSize) (0,0) (1,1) 13 False
                    blTilenw   = GTile ((fst pos1) + 0.125,(snd pos2) - 0.125) (bSize,bSize) (0,0) (1,1) 14 False
                    tlTilene   = GTile ((fst pos1),(snd pos1) + 0.125) (bSize,bSize) (0,0) (1,1) 16 False
                    trTilene   = GTile ((fst pos2),(snd pos1) + 0.125) (bSize,bSize) (0,0) (1,1) 17 False
                    brTilene   = GTile ((fst pos2),(snd pos2) - 0.125) (bSize,bSize) (0,0) (1,1) 14 False
                    blTilene   = GTile ((fst pos1),(snd pos2) - 0.125) (bSize,bSize) (0,0) (1,1) 13 False

-- creates textbox of arbitrary size
calcTextbox ∷ Double → Double → Int → Int → [GTile]
calcTextbox x y sx sy = [topLeftTile] ⧺ (topRow sx) ⧺ [topRightTile] ⧺ (middleBit sx sy) ⧺ (bottomRow sx sx sy)
  where sx' = fromIntegral sx
        topLeftTile = defaultGTile
                        { tPos   = (x,y)
                        , tScale = (0.5,0.5)
                        , tT     = 6 }
        topRightTile = defaultGTile
                         { tPos   = (x+(0.5*sx')+0.5,y)
                         , tScale = (0.5,0.5)
                         , tT     = 5 }
        topRow ∷ Int → [GTile]
        topRow 0  = []
        topRow width = thisTile : (topRow (width - 1))
          where thisTile = defaultGTile
                  { tPos   = (x+(0.5*width'),y)
                  , tScale = (0.5,0.5)
                  , tT     = 4 }
                width' = fromIntegral width
        middleBit ∷ Int → Int → [GTile]
        middleBit _     0      = []
        middleBit width height = (middleTiles width height) ⧺ (middleBit width (height - 1))
          where middleTiles ∷ Int → Int → [GTile]
                middleTiles 0 h = [leftTile, rightTile]
                  where leftTile = defaultGTile
                          { tPos = (x, y - (0.5*height'))
                          , tScale = (0.5,0.5)
                          , tT = 10 }
                        rightTile = defaultGTile
                          { tPos = (x + 0.5 + (0.5*width'), y - (0.5*height'))
                          , tScale = (0.5,0.5)
                          , tT = 3 }
                        width' = fromIntegral width
                        height' = fromIntegral h
                middleTiles w h = middleTile : (middleTiles (w - 1) h)
                  where middleTile = defaultGTile
                          { tPos   = (x + (0.5*width'), y - (0.5*height'))
                          , tScale = (0.5,0.5)
                          , tT     = 2 }
                        width' = fromIntegral w
                        height' = fromIntegral h
        bottomRow ∷ Int → Int → Int → [GTile]
        bottomRow 0 width     height      = [bottomLeftTile, bottomRightTile]

          where bottomLeftTile = defaultGTile
                  { tPos   = (x, y - 0.5 - (0.5*height'))
                  , tScale = (0.5,0.5)
                  , tT     = 9 }
                bottomRightTile = defaultGTile
                  { tPos   = (x + 0.5 + (0.5*width'), y - 0.5 - (0.5*height'))
                  , tScale = (0.5,0.5)
                  , tT     = 8 }
                width'  = fromIntegral width
                height' = fromIntegral height
        bottomRow n width height = thisTile : (bottomRow (n - 1) width height)
          where thisTile = defaultGTile
                  { tPos   = (x + (0.5*width'),y - 0.5 - (0.5*height'))
                  , tScale = (0.5,0.5)
                  , tT     = 7 }
                width'  = fromIntegral n
                height' = fromIntegral height

-- combines all GTiles into a dataframe
-- of vertices, preformed every frame
vertices ∷ (Float,Float,Float) → [GTile] → DataFrame Vertex '[XN 0]
vertices (cx,cy,_) ts = fromList $ combineVertices ts
  where vertsqs = [ S $ Vertex (vec3 (-1) (-1) 0) (vec4 1 0 0 1) (vec3 0 1 0.1)
                  , S $ Vertex (vec3   1  (-1) 0) (vec4 0 1 0 1) (vec3 1 1 0.1)
                  , S $ Vertex (vec3   1    1  0) (vec4 0 0 1 1) (vec3 1 0 0.1)
                  , S $ Vertex (vec3 (-1)   1  0) (vec4 1 1 1 1) (vec3 0 0 0.1) ]
        combineVertices [] = []
        combineVertices (tile:tts) = withTC (indexAtlas ax ay sx sy) (withTC (+ vec3 0 0 t) (withPos (+ vec4 x y 0 0) (withScale (* vec3 xscale yscale 1) vertsqs))) ⧺ combineVertices tts
          where (x',y') = tPos tile
                (x0,y0) = (realToFrac(2*x'), realToFrac(2*y'))
                (x, y)  = case (tMoves tile) of
                  True  → ((x0+(0.1*cx)),(y0+(0.1*cy)))
                  False → ((x0,y0))
                (ax', ay') = tInd tile
                ( ax,  ay) = (fromIntegral ax', fromIntegral ay')
                (xscale',yscale') = tScale tile
                (xscale, yscale)  = (realToFrac xscale', realToFrac yscale')
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
fontOffset ∷ Char → Double
fontOffset 'a'  = 0.3
fontOffset 'b'  = 0.3
fontOffset 'c'  = 0.3
fontOffset 'd'  = 0.3
fontOffset 'e'  = 0.3
fontOffset 'f'  = 0.3
fontOffset 'g'  = 0.3
fontOffset 'h'  = 0.3
fontOffset 'i'  = 0.2
fontOffset 'j'  = 0.2
fontOffset 'k'  = 0.3
fontOffset 'l'  = 0.2
fontOffset 'm'  = 0.3
fontOffset 'n'  = 0.3
fontOffset 'o'  = 0.3
fontOffset 'p'  = 0.3
fontOffset 'q'  = 0.3
fontOffset 'r'  = 0.3
fontOffset 's'  = 0.3
fontOffset 't'  = 0.3
fontOffset 'u'  = 0.3
fontOffset 'v'  = 0.3
fontOffset 'w'  = 0.3
fontOffset 'x'  = 0.3
fontOffset 'y'  = 0.3
fontOffset 'z'  = 0.3
fontOffset '?'  = 0.3
fontOffset '!'  = 0.2
fontOffset '('  = 0.2
fontOffset ')'  = 0.2
fontOffset '\'' = 0.2
fontOffset '"'  = 0.2
fontOffset 'A'  = 0.4
fontOffset 'B'  = 0.4
fontOffset 'C'  = 0.4
fontOffset 'D'  = 0.4
fontOffset 'E'  = 0.4
fontOffset 'F'  = 0.4
fontOffset 'G'  = 0.4
fontOffset 'H'  = 0.4
fontOffset 'I'  = 0.3
fontOffset 'J'  = 0.4
fontOffset 'K'  = 0.4
fontOffset 'L'  = 0.4
fontOffset 'M'  = 0.4
fontOffset 'N'  = 0.4
fontOffset 'O'  = 0.4
fontOffset 'P'  = 0.4
fontOffset 'Q'  = 0.4
fontOffset 'R'  = 0.4
fontOffset 'S'  = 0.4
fontOffset 'T'  = 0.4
fontOffset 'U'  = 0.4
fontOffset 'V'  = 0.4
fontOffset 'W'  = 0.4
fontOffset 'X'  = 0.4
fontOffset 'Y'  = 0.4
fontOffset 'Z'  = 0.4
fontOffset '.'  = 0.15
fontOffset ':'  = 0.2
fontOffset ','  = 0.15
fontOffset ';'  = 0.2
fontOffset '+'  = 0.3
fontOffset '-'  = 0.3
fontOffset '*'  = 0.3
fontOffset '/'  = 0.2
fontOffset '='  = 0.3
fontOffset '1'  = 0.3
fontOffset '2'  = 0.3
fontOffset '3'  = 0.3
fontOffset '4'  = 0.3
fontOffset '5'  = 0.3
fontOffset '6'  = 0.3
fontOffset '7'  = 0.3
fontOffset '8'  = 0.3
fontOffset '9'  = 0.3
fontOffset '0'  = 0.3
fontOffset '%'  = 0.3
fontOffset '&'  = 0.3
--fontOffset ''   = 0.0
fontOffset '`'  = 0.2
fontOffset '~'  = 0.3
fontOffset '@'  = 0.3
fontOffset '#'  = 0.3
fontOffset '$'  = 0.3
fontOffset '^'  = 0.2
fontOffset '_'  = 0.3
fontOffset '\\' = 0.2
fontOffset '|'  = 0.2
fontOffset '<'  = 0.3
fontOffset '>'  = 0.3
fontOffset _    = 0.5
