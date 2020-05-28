{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Paracletus.Vulkan.Vertex where
import Prelude()
import UPrelude
import qualified Control.Monad.ST as ST
import Data.Maybe
import GHC.Generics (Generic)
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.Vulkan.Marshal.Create.DataFrame()
import Numeric.DataFrame
import qualified Numeric.DataFrame.ST as ST
import Numeric.Dimensions

data Vertex = Vertex { pos      ∷ Vec3f
                     , color    ∷ Vec3f
                     , texCoord ∷ Vec2f
                     } deriving (Eq, Ord, Show, Generic)
instance PrimBytes Vertex

atLeastThree ∷ (All KnownDimType ns, BoundedDims ns) ⇒ DataFrame t (n ': ns) → DataFrame t (XN 3 ': ns)
atLeastThree = fromMaybe (error "not enough vertex points") ∘ constrainDF

dfLen ∷ DataFrame t (xns ∷ [XNat]) → Word32
dfLen (XFrame (_ ∷ DataFrame t ns)) = case dims @ns of
  n :* _ → fromIntegral $ dimVal n
  U      → 1

vertIBD ∷ VkVertexInputBindingDescription
vertIBD = createVk
  $  set @"binding"   0
  &* set @"stride"    (bSizeOf @Vertex undefined)
  &* set @"inputRate" VK_VERTEX_INPUT_RATE_VERTEX
vertIADs ∷ Vector VkVertexInputAttributeDescription 3
vertIADs = ST.runST $ do
  mv ← ST.newPinnedDataFrame
  ST.writeDataFrame mv (0 :* Empty) ∘ scalar $ createVk
    $  set @"location" 0
    &* set @"binding"  0
    &* set @"format"   VK_FORMAT_R32G32B32_SFLOAT
    &* set @"offset"   (bFieldOffsetOf @"pos" @Vertex undefined)
  ST.writeDataFrame mv (1 :* Empty) ∘ scalar $ createVk
    $  set @"location" 1
    &* set @"binding"  0
    &* set @"format"   VK_FORMAT_R32G32B32_SFLOAT
    &* set @"offset"   (bFieldOffsetOf @"color" @Vertex undefined)
  ST.writeDataFrame mv (2 :* Empty) ∘ scalar $ createVk
    $  set @"location" 2
    &* set @"binding"  0
    &* set @"format"   VK_FORMAT_R32G32_SFLOAT
    &* set @"offset"   (bFieldOffsetOf @"texCoord" @Vertex undefined)
  ST.unsafeFreezeDataFrame mv
