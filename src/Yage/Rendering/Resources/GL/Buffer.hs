{-# LANGUAGE ScopedTypeVariables #-}
module Yage.Rendering.Resources.GL.Buffer
  ( createBuffer
  , vertexBuffer
  , elementBuffer
  ) where

import           Yage.Prelude

import           Yage.Rendering.Resources.GL.Base

import           Quine.GL.Buffer            as Img
import           Quine.StateVar


createBuffer :: BufferData a => BufferTarget -> BufferUsage -> a -> Acquire (Buffer a)
createBuffer target usage xs = do
  buff <- glResource
  boundBufferAt target $= buff
  bufferData target $= (usage, xs)
  return buff

vertexBuffer :: Storable a => BufferUsage -> SVector a -> Acquire (Buffer (SVector a))
vertexBuffer = createBuffer ArrayBuffer

elementBuffer :: (Storable a, Integral a) => BufferUsage -> SVector a -> Acquire (Buffer (SVector a))
elementBuffer = createBuffer ElementArrayBuffer
