{-# OPTIONS_GHC -funbox-strict-fields            #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations            #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TypeFamilies         #-}
module Yage.Formats.Ygm
  ( YGM(..)
  -- * Format
  , YGMVertex(..)
  , ygmFormat
  -- * Im/Export
  , ygmToFile
  , ygmFromFile
  -- * Predicates
  , sameModels
  ) where

import           Yage.Lens
import           Yage.Prelude
import           Data.Binary
-- import           Data.Map                    (keys)
import           Data.Data
import           Data.Text.Binary            as Bin ()
import           Foreign.Storable
import           Foreign.Ptr

import           Codec.Compression.GZip
import           Control.Parallel.Strategies
import qualified Data.ByteString.Lazy        as B

import           Linear
import           Quine.GL.Types
import           Quine.GL.Attribute
import           Yage.Geometry
import           Yage.Vertex
import           Yage.Rendering.GL

data YGMVertex = YGMVertex
  { _ygmPosition :: !Vec3
  , _ygmTexture  :: !Vec2
  , _ygmTangentX :: !Vec3
  , _ygmTangentZ :: !Vec4
  } deriving (Eq,Ord,Show,Data,Typeable,Generic)

makeLenses ''YGMVertex

data YGM = YGM
  { ygmName   :: Text
  , ygmModels :: Map Text (TriGeo YGMVertex)
  } deriving ( Eq, Typeable, Generic )


ygmToFile :: FilePath -> YGM -> IO ()
ygmToFile name = B.writeFile (fpToString name) . compress . encode

ygmFromFile :: FilePath -> IO YGM
ygmFromFile path = do
  !ygm <- ({-# SCC "decode" #-} decode) .
          ({-# SCC "decompress" #-} decompress) <$>
           {-# SCC "readFile" #-} (B.readFile (fpToString path))
  return ygm { ygmModels = {-# SCC "ygmModels" #-} ygmModels ygm `using` parTraversable ({-# SCC "deepsec" #-} rdeepseq) }

ygmFormat :: (Real a2, Real a1, Real a, Fractional a2) => Pos a -> Tex a1 -> TBN a2 -> YGMVertex
ygmFormat pos tex tangentBasis@(V3 t _b n) = YGMVertex
  { _ygmPosition  = realToFrac <$> pos
  , _ygmTexture   = realToFrac <$> tex
  , _ygmTangentX  = realToFrac <$> t
  , _ygmTangentZ  = realToFrac <$> (vector n & _w .~ basisSign)
  }
 where
  basisSign = if det33 tangentBasis < 0 then -1.0 else 1.0


sameModels :: YGM -> YGM -> Bool
sameModels a b = (ygmModels a) == (ygmModels b)


instance NFData YGMVertex
instance Binary YGMVertex

instance Show YGM where
  show YGM{ygmName, ygmModels} = unpack $ format "YGM {name = {}, groups={}}" (Shown ygmName, Shown $ keys ygmModels)

instance Binary YGM

instance NFData YGM where rnf = genericRnf

-- * GL Layout

instance HasPosition YGMVertex Vec3 where
  position = ygmPosition
  positionlayout = to $ const $ Layout 3 GL_FLOAT False (sizeOf (undefined::YGMVertex)) nullPtr

instance HasTexture YGMVertex Vec2 where
  texture = ygmTexture
  texturelayout = to $ const $ Layout 2 GL_FLOAT False (sizeOf (undefined::YGMVertex)) (nullPtr `plusPtr` (sizeOf (undefined::Vec3)))

instance HasTangentX YGMVertex Vec3 where
  tangentX = ygmTangentX
  tangentXlayout = to $ const $ Layout 3 GL_FLOAT False (sizeOf (undefined::YGMVertex)) (nullPtr `plusPtr` (sizeOf (undefined::Vec3) + sizeOf (undefined::Vec2)))

instance HasTangentZ YGMVertex Vec4 where
  tangentZ = ygmTangentZ
  tangentZlayout = to $ const $ Layout 4 GL_FLOAT False (sizeOf (undefined::YGMVertex)) (nullPtr `plusPtr` (sizeOf (undefined::Vec3) + sizeOf (undefined::Vec2) + sizeOf (undefined::Vec3)))

-- * Memory Representation

instance Storable YGMVertex where
  peek ptr = YGMVertex
    <$> peek (castPtr ptr)
    <*> peek (castPtr $ ptr `plusPtr` (sizeOf (undefined::Vec3)))
    <*> peek (castPtr $ ptr `plusPtr` (sizeOf (undefined::Vec3) + sizeOf (undefined::Vec2)))
    <*> peek (castPtr $ ptr `plusPtr` (sizeOf (undefined::Vec3) + sizeOf (undefined::Vec2) + sizeOf (undefined::Vec3)))
  poke ptr YGMVertex{..} = do
    poke (castPtr ptr) _ygmPosition
    poke (castPtr $ ptr `plusPtr` sizeOf (undefined::Vec3)) _ygmTexture
    poke (castPtr $ ptr `plusPtr` (sizeOf (undefined::Vec3) + sizeOf (undefined::Vec2))) _ygmTangentX
    poke (castPtr $ ptr `plusPtr` (sizeOf (undefined::Vec3) + sizeOf (undefined::Vec2) + sizeOf (undefined::Vec3))) _ygmTangentZ
  sizeOf _ = sizeOf (undefined::Vec3) + sizeOf (undefined::Vec2) + sizeOf (undefined::Vec3) + sizeOf (undefined::Vec4)
  alignment _ = alignment (undefined::Vec3)
