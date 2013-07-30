module Yage.Rendering.Utils (
    makeVAO
    ) where

import             Yage.Import

import             Control.Monad.IO.Class

import qualified   Graphics.Rendering.OpenGL       as GL
import             Graphics.Rendering.OpenGL.GL    (($=))

{-
copied and modified 
http://hackage.haskell.org/packages/archive/GLUtil/0.6.4/doc/html/src/Graphics-GLUtil-VertexArrayObjects.html#makeVAO
-}

makeVAO :: MonadIO m => m () -> m GL.VertexArrayObject
makeVAO setup = do [vao] <- io $ GL.genObjectNames 1
                   io $ GL.bindVertexArrayObject $= Just vao
                   setup
                   io $ GL.bindVertexArrayObject $= Nothing
                   return vao