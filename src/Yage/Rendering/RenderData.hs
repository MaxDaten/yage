{-# LANGUAGE TemplateHaskell    #-}
module Yage.Rendering.RenderData
  ( RenderData(..)
  , HasRenderData(..)
  , fromMesh
  ) where

import Yage.Prelude
import Yage.Lens
import Yage.Resource.YageResource
import Yage.Rendering.Mesh
import qualified Yage.Rendering.Resources.GL.Buffer as B
import qualified Data.Vector.Storable as SV
import Quine.GL.Buffer

data RenderData i v = RenderData
  { _vertexBuffer :: Buffer v
  , _indexBuffer  :: Buffer i
  , _elementCount :: Int
  }

makeClassy ''RenderData

fromMesh :: Storable v => Mesh v -> YageResource (RenderData (SVector Word32) (SVector v))
fromMesh mesh = RenderData
  <$> B.vertexBuffer StaticDraw (mesh^.meshVertices)
  <*> B.elementBuffer StaticDraw (SV.map fromIntegral indices)
  <*> pure (length indices)
 where
  indices = mesh^.concatedMeshIndices
