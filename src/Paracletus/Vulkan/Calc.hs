module Paracletus.Vulkan.Calc ( calcVertices ) where
-- translations from lua state to draw
-- state are defined, should be run
-- outside of parent thread
import Prelude()
import UPrelude
import Control.Parallel.Strategies (rpar, rseq, runEval)
import Graphics.Vulkan.Core_1_0
import Numeric.DataFrame
import Anamnesis.Data
import Paracletus.Data
import Paracletus.Draw
import Paracletus.Vulkan.Atlas
import Paracletus.Vulkan.Vertex

-- determines dataframes from drawstate,
-- preformance of dataframe creation seems
-- to be negligent, regardless, keep other
-- calcuations to a minimum.
calcVertices ∷ (Float,Float,Float) → [GTile] →  (DataFrame Vertex '[XN 0], DataFrame Word32 '[XN 3])
calcVertices cam ts = runEval $ do
  verts ← rpar $ vertices cam ts
  inds ← rseq $ indices ts
  rseq verts
  return (verts,inds)
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

