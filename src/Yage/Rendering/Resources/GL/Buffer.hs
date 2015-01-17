{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
module Yage.Rendering.Resources.GL.Buffer
  ( RenderData(..)
  , HasRenderData(..)
  , createBuffer
  , createVertexBuffer
  , createElementBuffer
  , fromMesh
  ) where

import           Yage.Prelude
import           Yage.Lens

import           Yage.Rendering.GL
import           Yage.Rendering.Resources.GL.Base
import           Yage.Rendering.Mesh
import           Yage.Resource.YageResource

import qualified Data.Vector.Storable       as VS

import           Quine.GL.Buffer            as Img
import           Quine.StateVar

data RenderData i v = RenderData
  { _renderDataVertexBuffer :: !(Buffer (SVector v))
  , _renderDataIndexBuffer  :: !(Buffer (SVector i))
  , _renderDataElementCount :: !Int
  , _renderDataElementMode  :: !GLenum
  , _renderDataElementType  :: !GLenum
  }

makeClassyFor "HasRenderData" "renderData"
  [ ("_renderDataVertexBuffer", "vertexBuffer")
  , ("_renderDataIndexBuffer", "indexBuffer")
  , ("_renderDataElementCount", "elementCount")
  , ("_renderDataElementMode", "elementMode")
  , ("_renderDataElementType", "elementType")
  ] ''RenderData

fromMesh :: Storable v => Mesh v -> YageResource (RenderData Word32 v)
fromMesh mesh = RenderData
  <$> createVertexBuffer StaticDraw (mesh^.meshVertices)
  <*> createElementBuffer StaticDraw (VS.map fromIntegral idxs)
  <*> pure (length idxs)
  <*> pure GL_TRIANGLES
  <*> pure GL_UNSIGNED_INT
 where
  idxs = mesh^.concatedMeshIndices


createBuffer :: BufferData a => BufferTarget -> BufferUsage -> a -> Acquire (Buffer a)
createBuffer target usage xs = do
  buff <- glResource
  boundBufferAt target $= buff
  bufferData target $= (usage, xs)
  return buff

createVertexBuffer :: Storable a => BufferUsage -> SVector a -> Acquire (Buffer (SVector a))
createVertexBuffer = createBuffer ArrayBuffer

createElementBuffer :: (Storable a, Integral a) => BufferUsage -> SVector a -> Acquire (Buffer (SVector a))
createElementBuffer = createBuffer ElementArrayBuffer
