module Game.Zone where

import Control.Parallel.Strategies (parMap, rpar)
import Data.List (zip4, zipWith4, zipWith6)
import System.Random
import Graphics.GL
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import GLUtil.Textures
import Game.Data
import Game.State
import Game.Settings
import Game.Map
import Game.Draw
import Game.Sun
import Game.Noise
import Game.Rand

drawZoneElev :: State -> [[GL.TextureObject]] -> IO ()
drawZoneElev state texs = do
  let gnew               = expandZone currentZone
      currentZone        = elev (head (stateZones state))
      (camx, camy, camz) = getZoneCam (head (stateZones state))
      nulltex            = texs !! 10
      emin0              = emin (head (stateZones state))
      emax0              = emax (head (stateZones state))
  resequence_ (map (drawZoneElevRow nulltex camx camy camz emin0 emax0) gnew)
  glFlush

drawZoneElevRow :: [GL.TextureObject] -> Float -> Float -> Int -> Float -> Float -> ([(Float, Int)], Int) -> IO ()
drawZoneElevRow texs camx camy camz emin emax (t1, t2) = resequence_ (map (drawZoneElevSpot texs camx camy camz emin emax t2) t1)

drawZoneElevSpot :: [GL.TextureObject] -> Float -> Float -> Int -> Float -> Float -> Int -> (Float, Int) -> IO ()
drawZoneElevSpot texs camx camy camz emax emin y (t, x) = withTextures2D texs $ drawZoneElevTile texs camx camy camz emax emin x y t

drawZoneElevTile :: [GL.TextureObject] -> Float -> Float -> Int -> Float -> Float -> Int -> Int -> Float -> IO ()
drawZoneElevTile texs camx camy camz emax emin x y t = do
  glLoadIdentity
  glTranslatef (2*((nx) - ((fromIntegral zonew)/2))) (2*((ny) - ((fromIntegral zoneh)/2))) (-zoom/4)
  glColor3f elev elev $ elevZoneOcean t elev
  drawSquare
  where nx = fromIntegral(x)+camx
        ny = fromIntegral(y)+camy
        elev = 1.0-((t-emin)/(emax-emin))

elevZoneOcean :: Float -> Float -> Float
elevZoneOcean t x
  | t <= (sealevel) = 8
  | otherwise = x

drawZone :: State -> [[GL.TextureObject]] -> IO ()
drawZone state texs = do
  let zgs                = stateZones state
      mapxr              = mapx currzone
      mapyr              = mapy currzone
      sun                = tapGrid sunspots mapxr mapyr
      sunspots           = stateSunSpots state
      (camx, camy, camz) = getZoneCam currzone
      currzone           = head zgs
      zcnew              = expandZone $ (cont currzone)
      zgnew              = expandZone $ (grid currzone)
      zgzip              = zip zgnew zcnew
  resequence_ (map (drawZoneRow texs camx camy camz sun) zgzip)
  glFlush

drawZoneRow :: [[GL.TextureObject]] -> Float -> Float -> Int -> Float -> (([(Int, Int)], Int), ([(Int, Int)], Int)) -> IO ()
drawZoneRow texs camx camy camz sun ((a, y), (b, _)) = resequence_ (map (drawZoneSpot texs camx camy camz sun y) newzgzip)
  where newzgzip = zip a b

drawZoneSpot :: [[GL.TextureObject]] -> Float -> Float -> Int -> Float -> Int -> ((Int, Int), (Int, Int)) -> IO ()
drawZoneSpot texs camx camy camz sun y ((g, x), (c, _)) = withTextures2D tex $ drawZoneTile tex camx camy camz sun x y
  where tex = [((texs !! c) !! g)]

drawZoneCursor :: State -> [[GL.TextureObject]] -> IO ()
drawZoneCursor state texs = do
  let (cx, cy)           = getZoneCursor currzone
      zgs                = stateZones state
      mapxr              = mapx currzone
      mapyr              = mapy currzone
      sun                = tapGrid sunspots mapxr mapyr
      sunspots           = stateSunSpots state
      (camx, camy, camz) = getZoneCam currzone
      currzone           = head zgs
  withTextures2D (head texs) $ drawZoneTile (head texs) camx camy camz (sun) cx cy

drawZoneTile :: [GL.TextureObject] -> Float -> Float -> Int -> Float -> Int -> Int -> IO ()
drawZoneTile texs camx camy 0 sun x y = do
  glLoadIdentity
  glTranslatef (2*((nx) - ((fromIntegral zonew)/2))) (2*((ny) - ((fromIntegral zoneh)/2))) (-zoom/4)
  glColor3f t1 t2 t3
  drawSquare
  where
    t1 = 0.8*sun*sun + 0.2*sun
    t2 = 0.9*sun
    t3 = (log (sun+1))
    nx = fromIntegral(x)+camx
    ny = fromIntegral(y)+camy

getZoneCursor :: Zone -> (Int, Int)
getZoneCursor zg = (cx, cy)
  where cx = curx zg
        cy = cury zg

getZoneCam :: Zone -> (Float, Float, Int)
getZoneCam zg = (camxr, camyr, camzr)
  where camxr = camx zg
        camyr = camy zg
        camzr = camz zg

moveZoneCam :: Float -> Zone -> Card -> Zone
moveZoneCam n zone South
  | (y < (fromIntegral zoneh))   = moveCamZone zone (x, y+n)
  | otherwise     = moveCamZone zone (x, y)
  where x = camx zone
        y = camy zone
moveZoneCam n zone North
  | (y > -(fromIntegral zoneh))       = moveCamZone zone (x, y-n)
  | otherwise     = moveCamZone zone (x, y)
  where x = camx zone
        y = camy zone
moveZoneCam n zone East
  | (x > -(fromIntegral zonew))       = moveCamZone zone (x-n, y)
  | otherwise     = moveCamZone zone (x, y)
  where x = camx zone
        y = camy zone
moveZoneCam n zone West
  | (x < (fromIntegral zonew))   = moveCamZone zone (x+n, y)
  | otherwise     = moveCamZone zone (x, y)
  where x = camx zone
        y = camy zone

moveCamZone :: Zone -> (Float, Float) -> Zone
moveCamZone zone (x, y) = Zone { grid = (grid zone)
                               , cont = (cont zone)
                               , elev = (elev zone)
                               , emax = (emax zone)
                               , emin = (emin zone)
                               , nois = (nois zone)
                               , mapx = (mapx zone)
                               , mapy = (mapy zone)
                               , camx = x
                               , camy = y
                               , camz = (camz zone)
                               , curx = (curx zone)
                               , cury = (cury zone) }

moveZoneCursor :: Zone -> Card -> Zone
moveZoneCursor zone South = moveCursorZone zone (x, y-1)
  where x = curx zone
        y = cury zone
moveZoneCursor zone North = moveCursorZone zone (x, y+1)
  where x = curx zone
        y = cury zone
moveZoneCursor zone East = moveCursorZone zone (x+1, y)
  where x = curx zone
        y = cury zone
moveZoneCursor zone West = moveCursorZone zone (x-1, y)
  where x = curx zone
        y = cury zone

moveCursorZone :: Zone -> (Int, Int) -> Zone
moveCursorZone zone (x, y) = Zone { grid = (grid zone)
                                  , cont = (cont zone)
                                  , elev = (elev zone)
                                  , emax = (emax zone)
                                  , emin = (emin zone)
                                  , nois = (nois zone)
                                  , mapx = (mapx zone)
                                  , mapy = (mapy zone)
                                  , camx = (camx zone)
                                  , camy = (camy zone)
                                  , camz = (camz zone)
                                  , curx = x
                                  , cury = y }


generateZone :: State -> Zone
generateZone state = genZone state x y zc conts seeds rands nconts
  where (x, y) = stateCursor state
        zc     = take (zoneh*zonew) (repeat 1)
        conts  = stateConts  state
        seeds  = stateSeeds  state
        rands  = stateRands  state
        nconts = stateNConts state

genZone :: State -> Int -> Int -> [Int] -> [(Int, Int)] -> [[(Int, Int)]] -> [[(Int, Int)]] -> Int -> Zone
genZone state x y zc conts seeds rands nconts = Zone { grid = g0
                                                     , cont = zoneconts
                                                     , elev = newelev
                                                     , emax = maximum newelev
                                                     , emin = minimum newelev
                                                     , nois = perl
                                                     , mapx = x
                                                     , mapy = y
                                                     , camx = 0.0
                                                     , camy = 0.0
                                                     , camz = 0
                                                     , curx = round $ fromIntegral(zonew)/2.0
                                                     , cury = round $ fromIntegral(zoneh)/2.0
                                                     }
  where zoneconts        = zc1
        zoneelev         = take (zoneh*zonew) (repeat 1.0)
        zc1              = initZoneCont state newelev x y zc conts seeds rands nconts
        (en, es, ee, ew) = cardinalsXY x y (stateElev state)
        enn              = quot (en+e) 2
        esn              = quot (es+e) 2
        een              = quot (ee+e) 2
        ewn              = quot (ew+e) 2
        perl             = x+(y*gridh)
        e                = tapGrid (stateElev state) x y
        g0               = initZoneGrid state r newelev zoneconts
        newelev          = initZoneBlurElev x y state perl zoneconts zoneelev e conts seeds rands nconts (enn, esn, een, ewn)
        r                = x+(y*gridw)

blurZone :: State -> [Int] -> [Float] -> Int -> [Float]
blurZone state conts elev n = elev

initZoneBlurElev :: Int -> Int -> State -> Int -> [Int] -> [Float] -> Int -> [(Int, Int)] -> [[(Int, Int)]] -> [[(Int, Int)]] -> Int -> (Int, Int, Int, Int) -> [Float]
initZoneBlurElev x0 y0 state perl zc ze e l k j i cards = do
  let e1 = elevZone x0 y0 state perl zc ze l k j i e cards
  blurZone state zc e1 erosion

elevZone :: Int -> Int -> State -> Int -> [Int] -> [Float] -> [(Int, Int)] -> [[(Int, Int)]] -> [[(Int, Int)]] -> Int -> Int -> (Int, Int, Int, Int) -> [Float]
elevZone x0 y0 state perl zc elev []     []     []     _ e0 cards = elev
elevZone x0 y0 state perl zc elev _      _      _      0 e0 cards = elev
elevZone x0 y0 state perl zc elev (l:ls) (k:ks) (j:js) i e0 cards = do
  let x = findZoneElev x0 y0 state perl i (fst l) (snd l) zc elev k j e0 cards
  elevZone x0 y0 state perl zc x ls ks js (i-1) e0 cards

findZoneElev :: Int -> Int -> State -> Int -> Int -> Int -> Int -> [Int] -> [Float] -> [(Int, Int)] -> [(Int, Int)] -> Int -> (Int, Int, Int, Int) -> [Float]
findZoneElev _  _  _     _    _ _ _ _  e []     []     ei cards = e
findZoneElev x0 y0 state perl c x y zc e (k:ks) (j:js) ei cards = do
  let newzc = expandZone zc
      newe  = expandZone e
      e0    = parMap rpar (elevZoneRow x0 y0 state perl newzc c (fst k) (snd k) (fst j) (snd j) ei cards) newe
      e1    = stripGrid e0
      e2    = flattenGrid e1
  findZoneElev x0 y0 state perl c x y zc e2 ks js ei cards

elevZoneRow :: Int -> Int -> State -> Int -> [([(Int, Int)], Int)] -> Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int, Int, Int) -> ([(Float, Int)], Int) -> ([(Float, Int)], Int)
elevZoneRow x0 y0 state perl zc c w x y z e0 cards (t1, t2) = (map (elevZoneTile x0 y0 state zc c t2 perl w x y z e0 cards) t1, t2)

elevZoneTile :: Int -> Int -> State -> [([(Int, Int)], Int)] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int, Int, Int) -> (Float, Int) -> (Float, Int)
elevZoneTile x0 y0 state c e j perl w x y z e0 cards (t, i) = ((elevOfZone state x0 y0 i j perl e0 cards), i)

-- east and west are swapped here, i must have made a mistake in my elevdist formula, this fixes it...
elevOfZone :: State -> Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int, Int, Int) -> Float
elevOfZone state x0 y0 i j perl e (en, es, ee, ew) = elevDist e en es ew ee i j perl
  --where e0 = fromIntegral ((stateElev state) !! (x0 + (y0*gridw)))

elevDist :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Float
elevDist e en es ee ew i j perl = b0 + (b1*ifloat) + (b2*jfloat) + (b3*ifloat2) + (b4*jfloat2) + 100.0*noise
  where enf = fromIntegral en
        esf = fromIntegral es
        eef = fromIntegral ee
        ewf = fromIntegral ew
        ef  = fromIntegral e
        ifloat = fromIntegral i
        jfloat = fromIntegral j
        ifloat2 = ifloat*ifloat
        jfloat2 = jfloat*jfloat
        wfloat = fromIntegral zonew
        hfloat = fromIntegral zoneh
        wfloat2 = wfloat*wfloat
        hfloat2 = hfloat*hfloat
        b0 = eef + esf - ef
        b1 = (4.0 * ef/wfloat) - (ewf/wfloat) - (3.0 * eef/wfloat)
        b2 = (4.0 * ef/hfloat) - (enf/hfloat) - (3.0 * esf/hfloat)
        b3 = (2.0 * eef/wfloat2) + (2.0 * ewf/wfloat2) - (4.0 * ef/wfloat2)
        b4 = (2.0 * esf/hfloat2) + (2.0 * enf/hfloat2) - (4.0 * ef/hfloat2)
        perlin = makePerlin perl 4 0.15 0.5
        noise = getNoise i j perlin

initZoneGrid :: State -> Int -> [Float] -> [Int] -> [Int]
initZoneGrid state r e zoneconts = g4
  where g0 = map (blankZoneGrid state) zoneconts
        g1 = zipWith3 (seedZoneGrid state) ccards gcards zoneconts
        g2 = zipWith4 (seedZoneElevGrid state) gcards ecards e g1
        g3 = zipWith4 (seedZoneGridGaps state) gcards2 zoneconts g2 rlist2
        g4 = zipWith6 (solveGridPuzzle) zoneconts ccards gcards3 g3 rlist3 kochance3
        (nc, sc, ec, wc) = zoneCardinalsC zoneconts 
        (ng, sg, eg, wg) = zoneCardinalsG g0 56
        (ng2, sg2, eg2, wg2) = zoneCardinalsG g2 56
        (ng3, sg3, eg3, wg3) = zoneCardinalsG g3 56
        (ne, se, ee, we) = zoneCardinalsE e
        ecards = zip4 ne se ee we
        ccards = zip4 nc sc ec wc
        gcards = zip4 ng sg eg wg
        gcards2 = zip4 ng2 sg2 eg2 wg2
        gcards3 = zip4 ng3 sg3 eg3 wg3
        kochance3 = randomList (0,1) (zoneh*zonew) stdgen0
        rlist2 = randomList (0, 110) (zoneh*zonew) stdgen1
        rlist3 = randomList (0, 110) (zoneh*zonew) stdgen2
        stdgen0 = mkStdGen r
        (_, stdgen1) = next stdgen0
        (_, stdgen2) = next stdgen1

seedZoneGridGaps :: State -> (Int, Int, Int, Int) -> Int -> Int -> Int -> Int
seedZoneGridGaps state (ng, sg, eg, wg) 3 56 r = seedPlainsGrid ng sg eg wg r
seedZoneGridGaps state (ng, sg, eg, wg) c g  r = g

seedPlainsGrid :: Int -> Int -> Int -> Int -> Int -> Int
seedPlainsGrid _  _  _  _  r
  | ((r > 35) && (r < 66)) || 
    ((r > 71) && (r  < 87)) || 
    (r == 93) || (r == 95) || (r == 96) || 
    ((r > 97) && (r < 111))                = (r `mod` 6)
  | otherwise                              = r

solveGridPuzzle :: Int -> (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Int -> Int -> Int -> Int
solveGridPuzzle c (nc, sc, ec, wc) (ng, sg, eg, wg) g r ko
  | (g > 35) && (g < 66)                             = g
  | (ko == 1)                                        = g
  | otherwise                                        = bestMove ng sg eg wg g r


bestMove :: Int -> Int -> Int -> Int -> Int -> Int -> Int
bestMove ng sg eg wg g r = randbs
  where nscores = map (nFitScore ng) [0..ntiles]
        sscores = map (sFitScore sg) [0..ntiles]
        escores = map (eFitScore eg) [0..ntiles]
        wscores = map (wFitScore wg) [0..ntiles]
        cscores = zipWith4 addScores nscores sscores escores wscores
        bestscs = findBestScores cscores 0
        randbs  = randScore bestscs r g

randScore :: [Int] -> Int -> Int -> Int
randScore scores i g
  | (length scores) > i = scores !! i
  | otherwise           = g
  where ir = i `mod` (length scores)

findBestScores :: [Int] -> Int -> [Int]
findBestScores []    _ = []
findBestScores score n
  | (head score) == 4 = n : findBestScores (tail score) (n+1)
  | otherwise         = findBestScores (tail score) (n+1)

nFitScore :: Int -> Int -> Int
nFitScore ng g   = if (g `elem` (sfitlist !! ng)) then 1 else 0
sFitScore :: Int -> Int -> Int
sFitScore sg g   = if (g `elem` (nfitlist !! sg)) then 1 else 0
eFitScore :: Int -> Int -> Int
eFitScore wg g   = if (g `elem` (wfitlist !! wg)) then 1 else 0
wFitScore :: Int -> Int -> Int
wFitScore eg g   = if (g `elem` (efitlist !! eg)) then 1 else 0

addScores :: Int -> Int -> Int -> Int -> Int
addScores ns ss es ws = ns + ss + es + ws

blankZoneGrid :: State -> Int -> Int
blankZoneGrid state 3 = 0
blankZoneGrid state 4 = 0
blankZoneGrid state 5 = 0
blankZoneGrid state 6 = 0
blankZoneGrid state n = 0

seedZoneGrid :: State -> (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Int -> Int
seedZoneGrid state (nc, sc, ec, wc) (ng, sg, eg, wg) 3 = seedZoneEdges state 3 nc sc ec wc ng sg eg wg
seedZoneGrid state (nc, sc, ec, wc) (ng, sg, eg, wg) 4 = 0--seedZoneEdges state 4 nc sc ec wc ng sg eg wg
seedZoneGrid state (nc, sc, ec, wc) (ng, sg, eg, wg) 5 = seedZoneEdges state 5 nc sc ec wc ng sg eg wg
seedZoneGrid state (nc, sc, ec, wc) (ng, sg, eg, wg) 6 = 0--seedZoneEdges state 6 nc sc ec wc ng sg eg wg
seedZoneGrid state (nc, sc, ec, wc) (ng, sg, eg, wg) n = 0

seedZoneEdges :: State -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
seedZoneEdges state n nc sc ec wc ng sg eg wg
  | ((nc /= n) && (sc /= n) && (ec/= n) && (wc /= n))  = 40
  | ((nc /= n) && (sc /= n) && (ec /= n))              = 50
  | ((nc /= n) && (sc /= n) &&              (wc /= n)) = 48
  | ((nc /= n) &&              (ec /= n) && (wc /= n)) = 46
  |              ((sc /= n) && (ec /= n) && (wc /= n)) = 52
  | ((nc /= n) && (sc /= n))                           = 51
  | ((nc /= n) &&              (ec /= n))              = 38
  | ((nc /= n) &&                           (wc /= n)) = 36
  |              ((sc /= n) &&              (wc /= n)) = 42
  |              ((sc /= n) && (ec /= n))              = 44
  |                           ((ec /= n) && (wc /= n)) = 45
  | ((nc /= n))                                        = 37
  |              ((sc /= n))                           = 43
  |                           ((ec /= n))              = 41
  |                                        ((wc /= n)) = 39
  | otherwise                                          = 56

seedZoneElevGrid :: State -> (Int, Int, Int, Int) -> (Float, Float, Float, Float) -> Float -> Int -> Int
seedZoneElevGrid state (ng, sg, eg, wg) (ne, se, ee, we) e 56
  | ((e-ne) > telev) && ((e-se) > telev) && ((e-ee) > telev) && ((e-we) > telev) = 40
  | ((e-ne) > telev) && ((e-se) > telev) && ((e-ee) > telev)                     = 50
  | ((e-ne) > telev) && ((e-se) > telev) &&                     ((e-we) > telev) = 48
  | ((e-ne) > telev) &&                     ((e-ee) > telev) && ((e-we) > telev) = 46
  |                     ((e-se) > telev) && ((e-ee) > telev) && ((e-we) > telev) = 52
  | ((e-ne) > telev) && ((e-se) > telev) && ((e-ee) > telev) && ((e-we) > telev) = 51
  | ((e-ne) > telev) && ((e-se) > telev)                                         = 38
  | ((e-ne) > telev) &&                     ((e-ee) > telev)                     = 36
  | ((e-ne) > telev) &&                                         ((e-we) > telev) = 42
  |                     ((e-se) > telev) &&                     ((e-we) > telev) = 44
  |                                         ((e-ee) > telev) && ((e-we) > telev) = 45
  | ((e-ne) > telev)                                                             = 37
  |                     ((e-se) > telev)                                         = 43
  |                                         ((e-ee) > telev)                     = 41
  |                                                             ((e-we) > telev) = 39
  | otherwise                                                                    = 56
  where telev = 80
seedZoneElevGrid state (ng, sg, eg, wg) (ne, se, ee, we) e n  = n

--seedZoneGridGaps :: State -> (Int, Int, Int, Int) -> Int -> Int -> Int
--seedZoneGridGaps state (ng, sg, eg, wg) 3 55 = fitGap ng sg eg wg (ntiles-2)
--seedZoneGridGaps state (ng, sg, eg, wg) 5 55 = fitGap ng sg eg wg (ntiles-1)
--seedZoneGridGaps _     _                c n  = n

fitGap :: Int -> Int -> Int -> Int -> Int -> Int
fitGap ng sg eg wg 0 = 55
fitGap ng sg eg wg n = if (tileAllFits (ng+1) (sg+1) (eg+1) (wg+1) (n+1)) then n else (fitGap ng sg eg wg (n-1))

tileAllFits :: Int -> Int -> Int -> Int -> Int -> Bool
tileAllFits 56 56 56 56 _ = False
tileAllFits ng sg eg wg n = (n `elem` tileNFits sg) && (n `elem` tileSFits ng) && (n `elem` tileEFits wg) && (n `elem` tileWFits eg)

tileNFits :: Int -> [Int]
tileNFits 56 = [1..ntiles]
tileNFits n  = if (0 `elem` nfitlist !! n) then [] else (nfitlist !! n)

tileSFits :: Int -> [Int]
tileSFits 56 = [1..ntiles]
tileSFits n  = if (0 `elem` sfitlist !! n) then [] else (sfitlist !! n)


tileEFits :: Int -> [Int]
tileEFits 56 = [1..ntiles]
tileEFits n  = if (0 `elem` efitlist !! n) then [] else (efitlist !! n)


tileWFits :: Int -> [Int]
tileWFits 56 = [1..ntiles]
tileWFits n  = if (0 `elem` wfitlist !! n) then [] else (wfitlist !! n)

initZoneCont :: State -> [Float] -> Int -> Int -> [Int] -> [(Int, Int)] -> [[(Int, Int)]] -> [[(Int, Int)]] -> Int -> [Int]
initZoneCont state elev _ _ zg0 []     []     []     _ = zg0
initZoneCont state elev _ _ zg0 _      _      _      0 = zg0
initZoneCont state elev x y zg0 (l:ls) (k:ks) (j:js) i = do
  let zg = seedZoneCont state elev x y 0 i (fst l) (snd l) zg0 k j
  initZoneCont state elev x y zg ls ks js (i-1)

seedZoneCont :: State -> [Float] -> Int -> Int -> Int -> Int -> Int -> Int -> [Int] -> [(Int, Int)] -> [(Int, Int)] -> [Int]
seedZoneCont state elev _ _ _ _ _  _  zg []     []     = zg
seedZoneCont state elev x y i c x0 y0 zg (k:ks) (j:js) = do
  let nzg = expandZone zg
      zg0 = parMap rpar (seedZoneContRow state elev x y c i (fst k) (snd k) (fst j) (snd j)) nzg
      zg1 = stripGrid zg0
      zg2 = flattenGrid zg1
  seedZoneCont state elev x y (i+1) c x0 y0 zg2 ks js

seedZoneContRow :: State -> [Float] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
seedZoneContRow state elev x y i c w x0 y0 z0 (t1, t2) = (map (seedZoneContTile state elev x y i c t2 w x0 y0 z0) t1, t2)

seedZoneContTile :: State -> [Float] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int) -> (Int, Int)
seedZoneContTile state elev x y it c j w x0 y0 z0 (t, i)
  | e <= sealevel                                                                                 = (7, i)
  | e >= sealevel && ((t == 1) || (randstate == 1))                                               = (8, i)
  | (randstate == 1) && (zoneDistance x y i j w x0         y0         z0 t' <= maxdist)           = (randstate, i)
  | (randstate == 1) && (zoneDistance x y i j w (x0+gridw) y0         z0 t' <= maxdist)           = (randstate, i)
  | (randstate == 1) && (zoneDistance x y i j w x0         (y0+gridh) z0 t' <= maxdist)           = (randstate, i)
  | (randstate == 1) && (zoneDistance x y i j w (x0-gridw) y0         z0 t' <= maxdist)           = (randstate, i)
  | (randstate == 1) && (zoneDistance x y i j w x0         (y0-gridh) z0 t' <= maxdist)           = (randstate, i)
  | (randstate > 6) || (randstate < 2)                                                            = (t, i)
  | (randstate == 2) && (zoneDistance x y i j w x0         y0         z0 t' < 8*(maxdist-cfudge)) = (t, i)
  | (randstate == 2) && (zoneDistance x y i j w (x0+gridw) y0         z0 t' < 8*(maxdist-cfudge)) = (t, i)
  | (randstate == 2) && (zoneDistance x y i j w x0         (y0+gridw) z0 t' < 8*(maxdist-cfudge)) = (t, i)
  | (randstate == 2) && (zoneDistance x y i j w (x0-gridw) y0         z0 t' < 8*(maxdist-cfudge)) = (t, i)
  | (randstate == 2) && (zoneDistance x y i j w x0         (y0-gridw) z0 t' < 8*(maxdist-cfudge)) = (t, i)
  | (randstate == 2) && (zoneDistance x y i j w x0         y0         z0 t' <= (4*maxdist))       = (randstate, i)
  | (randstate == 2) && (zoneDistance x y i j w (x0+gridw) y0         z0 t' <= (4*maxdist))       = (randstate, i)
  | (randstate == 2) && (zoneDistance x y i j w x0         (y0+gridh) z0 t' <= (4*maxdist))       = (randstate, i)
  | (randstate == 2) && (zoneDistance x y i j w (x0-gridw) y0         z0 t' <= (4*maxdist))       = (randstate, i)
  | (randstate == 2) && (zoneDistance x y i j w x0         (y0-gridh) z0 t' <= (4*maxdist))       = (randstate, i)
  | (randstate == 5) && (zoneDistance x y i j w x0         y0         z0 t' < maxdist-cfudge)     = (t, i)
  | (randstate == 5) && (zoneDistance x y i j w (x0+gridw) y0         z0 t' < maxdist-cfudge)     = (t, i)
  | (randstate == 5) && (zoneDistance x y i j w x0         (y0+gridh) z0 t' < maxdist-cfudge)     = (t, i)
  | (randstate == 5) && (zoneDistance x y i j w (x0-gridw) y0         z0 t' < maxdist-cfudge)     = (t, i)
  | (randstate == 5) && (zoneDistance x y i j w x0         (y0-gridh) z0 t' < maxdist-cfudge)     = (t, i)
  | zoneDistance x y i j w x0         y0         z0 t' <= maxdist                                 = (randstate, i)
  | zoneDistance x y i j w (x0+gridw) y0         z0 t' <= maxdist                                 = (randstate, i)
  | zoneDistance x y i j w x0         (y0+gridh) z0 t' <= maxdist                                 = (randstate, i)
  | zoneDistance x y i j w (x0-gridw) y0         z0 t' <= maxdist                                 = (randstate, i)
  | zoneDistance x y i j w x0         (y0-gridh) z0 t' <= maxdist                                 = (randstate, i)
  | otherwise                                                                                    = (t, i)
  where
    randstate = (stateTypes state) !! c
    maxdist   = 1000.0 * fromIntegral(((stateSizes state) !! c))
    cfudge    = 2.0 * fromIntegral(((stateRangeRands state) !! (c)) - minnconts)
    t'        = fromIntegral(t)
    e         = tapZoneGridM i j elev

formatZoneElev :: [Float] -> (Int, Int) -> String
formatZoneElev e (x, y) = "Elev: " ++ (showFloatFoReal $ roundTo precision (getZoneElev e x y))

getZoneElev :: [Float] -> Int -> Int -> Float
getZoneElev e0 x y = do
  let elev = e0 !! (x+(zonew*y))
  elev - sealevel
