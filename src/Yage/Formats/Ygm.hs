{-# OPTIONS_GHC -funbox-strict-fields            #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Yage.Formats.Ygm
  ( YGM(..)
  -- * Format
  , YGMVertex(..)
  , ygmPosition
  , ygmTexture
  , ygmTangentX
  , ygmTangentZ
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
import           Data.Map                    (keys)
import           Data.Data
import           Data.Text.Binary            as Bin ()

import           Codec.Compression.GZip
import           Control.Parallel.Strategies
import qualified Data.ByteString.Lazy        as B

import           Linear
import           Quine.GL.Types
import           Yage.Geometry

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
  , _ygmTangentZ  = realToFrac <$> normal
  }
  where
  normal    = vector n & _w .~ basisSign
  basisSign = if det33 tangentBasis < 0 then -1.0 else 1.0


sameModels :: YGM -> YGM -> Bool
sameModels a b = (ygmModels a) == (ygmModels b)


instance NFData YGMVertex
instance Binary YGMVertex

instance Show YGM where
  show YGM{ygmName, ygmModels} = unpack $ format "YGM {name = {}, groups={}}" (Shown ygmName, Shown $ keys ygmModels)

instance Binary YGM

instance NFData YGM where rnf = genericRnf
