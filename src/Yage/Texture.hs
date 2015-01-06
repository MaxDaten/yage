{-# LANGUAGE FlexibleContexts #-}
module Yage.Texture
  ( module Yage.Image
  , materialRes
  ) where

import Yage.Prelude
import Yage.Lens

import Yage.Image
import Yage.Material
import Yage.Resources
import Yage.Rendering.Resources.GL.Texture
import Yage.Rendering.GL

materialRes :: Image2D (Image px) => Material col (Image px) -> YageResource (Material col (Texture px))
materialRes imgMat = (set materialTexture) <$> (createTexture2DImage GL_TEXTURE_2D (imgMat^.materialTexture)) <*> pure imgMat

