{-# LANGUAGE FlexibleContexts #-}
module Yage.Texture
  ( module Yage.Image
  , materialRes
  , texture2DRes
  ) where

import Yage.Prelude
import Yage.Lens

import Yage.Image
import Yage.Material
import Yage.Resources
import Yage.Rendering.Resources.GL.Texture
import Yage.Rendering.GL
import Yage.Geometry.D2.Rectangle


materialRes :: (GetRectangle i Int, Image2D i) => Material col i -> YageResource (Material col (Texture px))
materialRes imgMat = (set materialTexture) <$> (createTexture2DImage GL_TEXTURE_2D (imgMat^.materialTexture)) <*> pure imgMat

texture2DRes :: (GetRectangle i Int, Image2D i) => i -> YageResource (Texture px)
texture2DRes = createTexture2DImage GL_TEXTURE_2D

