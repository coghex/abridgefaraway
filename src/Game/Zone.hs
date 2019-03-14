module Game.Zone where

import Control.Parallel.Strategies (parMap, rpar)
import Data.List (zip4, zipWith4, zipWith5, zipWith6)
import System.Random
import Graphics.GL
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import GLUtil.Textures
import Game.Data
import Game.State
import Game.Settings
import Game.Fits
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
      zoom               = stateZoom state
  resequence_ (map (drawZoneElevRow nulltex camx camy camz emin0 emax0 zoom) gnew)
  glFlush

drawZoneElevRow :: [GL.TextureObject] -> Float -> Float -> Int -> Float -> Float -> Float -> ([(Float, Int)], Int) -> IO ()
drawZoneElevRow texs camx camy camz emin emax zoom (t1, t2) = resequence_ (map (drawZoneElevSpot texs camx camy camz emin emax t2 zoom) t1)

drawZoneElevSpot :: [GL.TextureObject] -> Float -> Float -> Int -> Float -> Float -> Int -> Float -> (Float, Int) -> IO ()
drawZoneElevSpot texs camx camy camz emax emin y zoom (t, x) = withTextures2D texs $ drawZoneElevTile texs camx camy camz emax emin x y t zoom

drawZoneElevTile :: [GL.TextureObject] -> Float -> Float -> Int -> Float -> Float -> Int -> Int -> Float -> Float -> IO ()
drawZoneElevTile texs camx camy camz emax emin x y t zoom = do
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
      zoom               = stateZoom state
  resequence_ (map (drawZoneRow texs camx camy camz sun zoom) zgzip)
  glFlush

drawZoneRow :: [[GL.TextureObject]] -> Float -> Float -> Int -> Float -> Float -> (([(Int, Int)], Int), ([(Int, Int)], Int)) -> IO ()
drawZoneRow texs camx camy camz sun zoom ((a, y), (b, _)) = resequence_ (map (drawZoneSpot texs camx camy camz sun zoom y) newzgzip)
  where newzgzip = zip a b

drawZoneSpot :: [[GL.TextureObject]] -> Float -> Float -> Int -> Float -> Float -> Int -> ((Int, Int), (Int, Int)) -> IO ()
drawZoneSpot texs camx camy camz sun zoom y ((g, x), (c, _)) = withTextures2D tex $ drawZoneTile tex camx camy camz sun zoom x y
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
      zoom               = stateZoom state
  withTextures2D (head texs) $ drawZoneTile (head texs) camx camy camz (sun) zoom cx cy

drawZoneTile :: [GL.TextureObject] -> Float -> Float -> Int -> Float -> Float -> Int -> Int -> IO ()
drawZoneTile texs camx camy 0 sun zoom x y = do
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
                               , zazz = (zazz zone)
                               , zgrd = (zgrd zone)
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
                                  , zazz = (zazz zone)
                                  , zgrd = (zgrd zone)
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

makeZazzGrid :: [Zazz] -> [Int] -> [Int]
makeZazzGrid []     g = g
makeZazzGrid (z:zs) g = makeZazzGrid zs ng
  where ng = makeZazzGridFromZazz z g

makeZazzGridFromZazz :: Zazz -> [Int] -> [Int]
makeZazzGridFromZazz (Zazz x y s t) g = g3
  where zg = expandZone g
        g1 = map (makeZazzGridRow x y s t) zg
        g2 = stripGrid g1
        g3 = flattenGrid g2

makeZazzGridRow :: Int -> Int -> (Int, Int) -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
makeZazzGridRow x y (w, h) t (g, j) = ((map (makeZazzGridSpot x y w h t j) g), j)

makeZazzGridSpot :: Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int) -> (Int, Int)
makeZazzGridSpot x y w h t j (g, i)
  | (g /= 0)                                                   = (g, i)
  | ((i-x) < w) && ((j-y) < h) && ((i-x) >= 0) && ((j-y) >= 0) = (((((i-x)) `mod` w) + ((j-y) * h)), i)
  | otherwise                                                  = (0, i)

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
                                                     , zazz = newzazz
                                                     , zgrd = newzazzgrid
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
        g0               = initZoneGrid state s0 newelev zoneconts
        --g1               = initZoneGridZazz state s0 g0 zoneconts
        newelev          = initZoneBlurElev x y state perl zoneconts zoneelev e conts seeds rands nconts (enn, esn, een, ewn)
        newzazz          = initZazz (length zazzcounts) s0 g0 zoneconts zazzcounts []
        newzazzgrid      = makeZazzGrid newzazz (take (gridw*gridh) (repeat 0))
        s0               = mkStdGen r
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

initZoneGrid :: State -> StdGen -> [Float] -> [Int] -> [Int]
initZoneGrid state s0 e zoneconts = g4
  where g0 = map (blankZoneGrid state) zoneconts
        g1 = zipWith3 (seedZoneGrid state) ccards gcards zoneconts
        g2 = zipWith4 (seedZoneElevGrid state) gcards ecards e g1
        g3 = zipWith4 (seedZoneGridGaps) gcards2 zoneconts g2 rlist2
        --g4 = zipWith6 (solveGridPuzzle) zoneconts ccards gcards3 g3 rlist3 kochance3
        g4 = zipWith3 (seedZoneSpaces) gcards3 zoneconts g3
        --g5 = findOpenSpaceChunks nzazz 8 5 zoneconts g4 stdgen0
        (nc, sc, ec, wc) = zoneCardinalsC zoneconts 
        (ng, sg, eg, wg) = zoneCardinalsG g0 nulltile
        (ng2, sg2, eg2, wg2) = zoneCardinals g2
        (ng3, sg3, eg3, wg3) = zoneCardinals g3
        (ne, se, ee, we) = zoneCardinalsE e
        ecards = zip4 ne se ee we
        ccards = zip4 nc sc ec wc
        gcards = zip4 ng sg eg wg
        gcards2 = zip4 ng2 sg2 eg2 wg2
        gcards3 = zip4 ng3 sg3 eg3 wg3
        rlist2 = randomList (0, (ntiles-1)) (zoneh*zonew) stdgen1
        rlist3 = randomList (0, (ntiles-1)) (zoneh*zonew) stdgen2
        stdgen0 = s0
        (_, stdgen1) = split stdgen0
        (_, stdgen2) = split stdgen1


findZazzSize :: Int -> (Int, Int)
findZazzSize n = zazzsizes !! (n-1)

initZazz :: Int -> StdGen -> [Int] -> [Int] -> [Int] -> [Zazz] -> [Zazz]
initZazz 0 s0 g0 zc _        z = z
initZazz n s0 g0 zc []       z = z
initZazz n s0 g0 zc ( 0:zss) z = initZazz (n-1) s0 g0 zc zss z
initZazz n s0 g0 zc (zs:zss) z = initZazz (n-1) s0 gnew zc zss (z ++ zr)
  where zr     = findVolcChunks zs n x y zc g0 s0 []
        gnew   = findOpenSpaceChunks zs x y zc g0 s0
        (x, y) = findZazzSize n
--initZazz 8 s0 g0 zc (zs:zss) z = initZazz (n-1) s0 gnew zc zss (z ++ zr)
--  where zr = findZazzChunks zs 16  8 zc g0 s0
--        gnew = findOpenSpaceChunks zs 16  8 zc g0 s0
--initZazz 7 s0 g0 zc (zs:zss) z = initZazz (n-1) s0 gnew zc zss (z ++ zr)
--  where zr = findZazzChunks zs  8  8 zc g0 s0
--        gnew = findOpenSpaceChunks zs  8  8 zc g0 s0
--initZazz 6 s0 g0 zc (zs:zss) z = initZazz (n-1) s0 gnew zc zss (z ++ zr)
--  where zr = findZazzChunks zs  8  4 zc g0 s0
--        gnew = findOpenSpaceChunks zs  8  4 zc g0 s0
--initZazz 5 s0 g0 zc (zs:zss) z = initZazz (n-1) s0 gnew zc zss (z ++ zr)
--  where zr = findZazzChunks zs  4  4 zc g0 s0
--        gnew = findOpenSpaceChunks zs  4  4 zc g0 s0
--initZazz 4 s0 g0 zc (zs:zss) z = initZazz (n-1) s0 gnew zc zss (z ++ zr)
--  where zr = findZazzChunks zs  4  3 zc g0 s0
--        gnew = findOpenSpaceChunks zs  4  3 zc g0 s0
--initZazz 3 s0 g0 zc (zs:zss) z = initZazz (n-1) s0 gnew zc zss (z ++ zr)
--  where zr = findZazzChunks zs  3  3 zc g0 s0
--        gnew = findOpenSpaceChunks zs  3  3 zc g0 s0
--initZazz 2 s0 g0 zc (zs:zss) z = initZazz (n-1) s0 gnew zc zss (z ++ zr)
--  where zr = findZazzChunks zs  3  2 zc g0 s0
--        gnew = findOpenSpaceChunks zs  3  2 zc g0 s0
--initZazz 1 s0 g0 zc (zs:zss) z = initZazz (n-1) s0 gnew zc zss (z ++ zr)
--  where zr = findZazzChunks zs  1  1 zc g0 s0
--        gnew = findOpenSpaceChunks zs  1  1 zc g0 s0


findVolcChunks :: Int -> Int -> Int -> Int -> [Int] -> [Int] -> StdGen -> [Zazz] -> [Zazz]
findVolcChunks 0 nn _ _ _ g _ z = z
findVolcChunks n nn w h c g r z = (findVolcChunks (n-1) nn w h c gnew r znew)
  where gnew = findOpenSpaceChunk w h c g r
        znew = z ++ (findVolcChunk nn w h c g r)

findVolcChunk :: Int -> Int -> Int -> [Int] -> [Int] -> StdGen -> [Zazz]
findVolcChunk n w h c g r
  | (length ilis == 0) = []
  | otherwise          = [z4]
  where zg    = expandZone g
        cz    = expandZone c
        g1    = zipWith (findOpenSpaceChunkRows w h g) cz zg
        ilis  = findSpaces g1
        i0    = head $ randomList (0, ((length ilis)-1)) 1 r
        i1    = ilis !! i0
        randv = head $ randomList (0, (volccounts !! (n-1))) 2 r
        z4    = Zazz { zazzx = (fst i1)
                     , zazzy = (snd i1)
                     , zazzs = (findZazzSize n)
                     , zazzt = randv
                     }

findOpenSpaceChunks :: Int -> Int -> Int -> [Int] -> [Int] -> StdGen -> [Int]
findOpenSpaceChunks 0 _ _ _ g _ = g
findOpenSpaceChunks n w h c g r = findOpenSpaceChunks (n-1) w h c gnew r
  where gnew = findOpenSpaceChunk wn hn c g r
        wn   = w--if (h >= w) then (w) else (w-1)
        hn   = h--if (w >= h) then (h) else (h-1)

findOpenSpaceChunk :: Int -> Int -> [Int] -> [Int] -> StdGen -> [Int]
findOpenSpaceChunk w h c g r
  | (length ilis) == 0 = g
  | otherwise          = g4
  where zg   = expandZone g
        cz   = expandZone c
        g1   = zipWith (findOpenSpaceChunkRows w h g) cz zg
        ilis = findSpaces g1
        i0   = head $ randomList (0, ((length ilis)-1)) 1 r
        rs   = expandZone $ randomList (0, 8) (zoneh*zonew) r
        g2   = zipWith3 (placeBlankSpace w h ilis i0) cz zg rs
        g3   = stripGrid g2
        g4   = flattenGrid g3

placeBlankSpace :: Int -> Int -> [(Int, Int)] -> Int -> ([(Int, Int)], Int) -> ([(Int, Int)], Int) -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
placeBlankSpace w h ilis i0 (c, _) (g, j) (r, _)
  | ((length ilis) <= 0) = (g, j)
  | otherwise            = ((zipWith3 (placeBlankSpaceRows (w) (h) x y j) g r c), j)
  where (x, y) = ilis !! i0
--placeBlankSpace w h ilis i0 (_, _) (g, j) (r, _) = (g, j)

placeBlankSpaceRows :: Int -> Int -> Int -> Int -> Int -> (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int)
placeBlankSpaceRows w h x y j (g, i) (r, _) (3, _) = ((placeBlankSpaceSpot w h x y i j g r), i)
placeBlankSpaceRows w h x y j (g, i) (r, _) (c, _) = (g, i)

placeBlankSpaceSpot :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
placeBlankSpaceSpot w h x y i j g r
  | (((x-i)) <= w) && ((x-i) > 0) && (((y-j)) <= h) && ((y-j) > 0) = calcZazzTileBackground r
  | otherwise                              = g

calcZazzTileBackground :: Int -> Int
calcZazzTileBackground 0 = 95
calcZazzTileBackground 1 = 96
calcZazzTileBackground 2 = 97
calcZazzTileBackground 3 = 104
calcZazzTileBackground 4 = 105
calcZazzTileBackground 5 = 208
calcZazzTileBackground 6 = 209
calcZazzTileBackground 7 = 216
calcZazzTileBackground 8 = 217
calcZazzTileBackground _ = specialtile

findSpaces :: [([(Int, Int)], Int)] -> [(Int, Int)]
findSpaces [] = []
findSpaces x  = ss
  where ss  = ret ++ findSpaces (tail x)
        ret = findSpacesFromRow (head x)

findSpacesFromRow :: ([(Int, Int)], Int) -> [(Int, Int)]
findSpacesFromRow ([], _) = []
findSpacesFromRow (x,  j) = ss
  where ss  = ret ++ findSpacesFromRow ((tail x), j)
        ret = findSpacesAtSpot j (head x)

findSpacesAtSpot :: Int -> (Int, Int) -> [(Int, Int)]
findSpacesAtSpot j (1, i) = [(i, j)]
findSpacesAtSpot _ (_, _) = []

findOpenSpaceChunkRows :: Int -> Int -> [Int] -> ([(Int, Int)], Int) -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
--findOpenSpaceChunkRows w h zg (3, _) (g, j) = ((map (findOpenSpaceChunkSpot w h j zg) g), j)
findOpenSpaceChunkRows w h zg (c, _) (g, j) = ((zipWith (findOpenSpaceChunkSpot w h j zg) g c), j)--(g, j)

findOpenSpaceChunkSpot :: Int -> Int -> Int -> [Int] -> (Int, Int) -> (Int, Int) -> (Int, Int)
findOpenSpaceChunkSpot w h j zg (g, i) (3, _)
  | (g == blanktile) = (isSpaceEmpty i j w h g zg, i)
  | otherwise        = (0, i)
findOpenSpaceChunkSpot w h j zg (g, i) (c, _) = (g, i)

isSpaceEmpty :: Int -> Int -> Int -> Int -> Int -> [Int] -> Int
isSpaceEmpty i j 0 h g zg = 1
isSpaceEmpty i j w h g zg
  | ((i+1) >= zonew)                       = 0
  | isSpaceRowEmpty (i+1) j h newg zg == 0 = 0
  | (newg == blanktile)                    = isSpaceEmpty (i+1) j (w-1) h newg zg
  | otherwise                              = 0
  where newg = tapZoneGrid zg (i+1) j

isSpaceRowEmpty :: Int -> Int -> Int -> Int -> [Int] -> Int
isSpaceRowEmpty _ _ 0 _ _  = 1
isSpaceRowEmpty i j h g zg
  | ((j+1) >= zoneh)    = 0
  | (newg == blanktile) = isSpaceRowEmpty i (j+1) (h-1) newg zg
  | otherwise           = 0
  where newg = tapZoneGrid zg i (j+1)

seedZoneSpaces :: (Int, Int, Int, Int) -> Int -> Int -> Int
seedZoneSpaces (ng, sg, eg, wg) 3 g
  | (g == nulltile) = findOpenSpace ng sg eg wg g
  | otherwise       = g
seedZoneSpaces (_ , _ , _ , _ ) _ g = g

findOpenSpace :: Int -> Int -> Int -> Int -> Int -> Int
findOpenSpace ng sg eg wg g
  | ((ng /= nulltile) || (sg /= nulltile) || (eg /= nulltile) || (wg /= nulltile)) = nulltile
  | otherwise                                                                      = blanktile

seedZoneGridGaps :: (Int, Int, Int, Int) -> Int -> Int -> Int -> Int
--seedZoneGridGaps _     (_ , _ , _ , _ ) 3 g r 1  = g
seedZoneGridGaps (ng, sg, eg, wg) 3 g r
  | (g == nulltile) = seedGaps ng sg eg wg r
  | otherwise       = g
seedZoneGridGaps (_ , _ , _ , _ ) c g r = g

seedGaps :: Int -> Int -> Int -> Int -> Int -> Int
seedGaps ng sg eg wg r
  | perfectFit ng sg eg wg 244 = 244
  | perfectFit ng sg eg wg 245 = 245
  | perfectFit ng sg eg wg 246 = 246
  | perfectFit ng sg eg wg 252 = 252
  | perfectFit ng sg eg wg 260 = 260
  | otherwise                  = nulltile

perfectFit :: Int -> Int -> Int -> Int -> Int -> Bool
perfectFit ng sg eg wg n
  | (n `elem` nfitn) && (n `elem` sfitn) && (n `elem` efitn) && (n `elem` wfitn) = True
  | otherwise                                                                  = False
  where nfitn = sfitlist !! ng
        sfitn = nfitlist !! sg
        efitn = wfitlist !! eg
        wfitn = efitlist !! wg

elemOrNull :: Int -> [Int] -> Bool
elemOrNull g fitn
  | (g == nulltile) = True
  | (g `elem` fitn) = True
  | otherwise       = False

iterateGridSolution :: Int -> StdGen -> [Int] -> [(Int, Int, Int, Int)] -> [(Int, Int, Int, Int)] -> [Int] -> [Int] -> [Int] -> [Int]
iterateGridSolution 0 _      _  _      _      g _ _  = g
iterateGridSolution n stdgen zc ccards gcards g r ko = iterateGridSolution (n-1) stdgennew zc ccards gcardsnew gnew rnew konew
  where gnew = zipWith6 solveGridPuzzle zc ccards gcards g r ko
        gcardsnew = zip4 ng sg eg wg
        (ng, sg, eg, wg) = zoneCardinalsG gnew 56
        rnew = randomList (0, (ntiles-1)) (zoneh*zonew) stdgennew
        (_, stdgennew) = split stdtemp
        (_, stdtemp) = split stdgen
        konew = randomList (0,2) (zoneh*zonew) stdtemp

solveGridPuzzle :: Int -> (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Int -> Int -> Int -> Int
solveGridPuzzle 3 ( 3,  3,  3,  3) (ng, sg, eg, wg) g r ko
  | (g > 35) && (g < 66)                             = g
  | (ko == 1)                                        = g
  | otherwise                                        = bestMove ng sg eg wg g r
solveGridPuzzle c (nc, sc, ec, wc) (ng, sg, eg, wg) g r ko = g


bestMove :: Int -> Int -> Int -> Int -> Int -> Int -> Int
bestMove ng sg eg wg g r = randbs
  where nscores = map (nFitScore ng) [0..ntiles]
        sscores = map (sFitScore sg) [0..ntiles]
        escores = map (eFitScore eg) [0..ntiles]
        wscores = map (wFitScore wg) [0..ntiles]
        cscores = zipWith4 addScores nscores sscores escores wscores
        bestscs = findAllBestScores cscores
        randbs  = randScores bestscs r g

randScores :: ([Int], [Int], [Int], [Int]) -> Int -> Int -> Int
randScores (s1, s2, s3, s4) i g
  | length s1 > 0 = randScore s1 i g
  | length s2 > 0 = randScore s2 i g
  | length s3 > 0 = randScore s3 i g
  | length s4 > 0 = randScore s4 i g
  | otherwise = 56

randScore :: [Int] -> Int -> Int -> Int
randScore scores i g
  | (length scores) > i = scores !! i
  | otherwise           = g
  where ir = i `mod` (length scores)

findAllBestScores :: [Int] -> ([Int], [Int], [Int], [Int])
findAllBestScores score = (findBestScores score 0 4, findBestScores score 0 3, findBestScores score 0 2, findBestScores score 0 1)

findBestScores :: [Int] -> Int -> Int -> [Int]
findBestScores []    _ _ = []
findBestScores score n i
  | (head score) == i = n : findBestScores (tail score) (n+1) i
  | otherwise         = findBestScores (tail score) (n+1) i

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
blankZoneGrid state 3 = nulltile
blankZoneGrid state 4 = nulltile
blankZoneGrid state 5 = nulltile
blankZoneGrid state 6 = nulltile
blankZoneGrid state n = nulltile

seedZoneGrid :: State -> (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Int -> Int
seedZoneGrid state (nc, sc, ec, wc) (ng, sg, eg, wg) 3 = seedZoneEdges state 3 nc sc ec wc ng sg eg wg
seedZoneGrid state (nc, sc, ec, wc) (ng, sg, eg, wg) 4 = 0--seedZoneEdges state 4 nc sc ec wc ng sg eg wg
seedZoneGrid state (nc, sc, ec, wc) (ng, sg, eg, wg) 5 = 0--seedZoneEdges state 5 nc sc ec wc ng sg eg wg
seedZoneGrid state (nc, sc, ec, wc) (ng, sg, eg, wg) 6 = 0--seedZoneEdges state 6 nc sc ec wc ng sg eg wg
seedZoneGrid state (nc, sc, ec, wc) (ng, sg, eg, wg) n = 0

seedZoneEdges :: State -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
seedZoneEdges state n nc sc ec wc ng sg eg wg
  | ((nc /= n) && (sc /= n) && (ec/= n) && (wc /= n))  = 225
  | ((nc /= n) && (sc /= n) && (ec /= n))              = 235
  | ((nc /= n) && (sc /= n) &&              (wc /= n)) = 233
  | ((nc /= n) &&              (ec /= n) && (wc /= n)) = 232
  |              ((sc /= n) && (ec /= n) && (wc /= n)) = 234
  | ((nc /= n) && (sc /= n))                           = 227
  | ((nc /= n) &&              (ec /= n))              = 239
  | ((nc /= n) &&                           (wc /= n)) = 236
  |              ((sc /= n) &&              (wc /= n)) = 237
  |              ((sc /= n) && (ec /= n))              = 238
  |                           ((ec /= n) && (wc /= n)) = 226
  | ((nc /= n))                                        = 265
  |              ((sc /= n))                           = 267
  |                           ((ec /= n))              = 264
  |                                        ((wc /= n)) = 266
  | otherwise                                          = nulltile

seedZoneElevGrid :: State -> (Int, Int, Int, Int) -> (Float, Float, Float, Float) -> Float -> Int -> Int
seedZoneElevGrid state (ng, sg, eg, wg) (ne, se, ee, we) e 56
  | ((e-ne) > telev) && ((e-se) > telev) && ((e-ee) > telev) && ((e-we) > telev) = 225
  | ((e-ne) > telev) && ((e-se) > telev) && ((e-ee) > telev)                     = 235
  | ((e-ne) > telev) && ((e-se) > telev) &&                     ((e-we) > telev) = 233
  | ((e-ne) > telev) &&                     ((e-ee) > telev) && ((e-we) > telev) = 232
  |                     ((e-se) > telev) && ((e-ee) > telev) && ((e-we) > telev) = 234
  | ((e-ne) > telev) && ((e-se) > telev) && ((e-ee) > telev) && ((e-we) > telev) = 227
  | ((e-ne) > telev) && ((e-se) > telev)                                         = 239
  | ((e-ne) > telev) &&                     ((e-ee) > telev)                     = 236
  | ((e-ne) > telev) &&                                         ((e-we) > telev) = 237
  |                     ((e-se) > telev) &&                     ((e-we) > telev) = 238
  |                                         ((e-ee) > telev) && ((e-we) > telev) = 226
  | ((e-ne) > telev)                                                             = 265
  |                     ((e-se) > telev)                                         = 267
  |                                         ((e-ee) > telev)                     = 264
  |                                                             ((e-we) > telev) = 266
  | otherwise                                                                    = nulltile
  where telev = 40
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
  -- | e >= sealevel && ((t == 1) || (randstate == 1))                                               = (8, i)
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
