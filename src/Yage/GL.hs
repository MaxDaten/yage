module Yage.GL
  ( module GL
  , module Bits
  , module Types
  ) where

import Data.Bits          as Bits
import Graphics.GL.Core41 as GL
import Quine.GL.Types     as Types

-- supported extensions
import Graphics.GL.Ext.EXT.TextureFilterAnisotropic as GL
import Graphics.GL.Ext.ARB.SeamlessCubemapPerTexture as GL
