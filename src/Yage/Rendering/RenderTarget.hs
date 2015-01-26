{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Yage.Rendering.RenderTarget
  ( RenderTarget
  , IsRenderTarget(getAttachments)
  , renderTarget
  , framebufferObj
  , targetRectangle
  , mkRenderTarget
  ) where

import Yage.Prelude
import Yage.Lens
import Yage.Math
import Data.Data
import Yage.Geometry.D2.Rectangle
import Yage.Rendering.Resources.GL.Framebuffer
import Yage.Resources
import Yage.Rendering.Resources.GL.Texture

data RenderTarget cs = RenderTarget
  { _renderTarget       :: cs
  , _framebufferObj     :: Framebuffer
  , _targetRectangle    :: Rectangle Int
  } deriving (Show,Typeable,Data,Generic)

makeLenses ''RenderTarget

instance GetRectangle (RenderTarget t) Int where
  asRectangle = targetRectangle

class IsRenderTarget cs where
  getAttachments :: cs -> ([Attachment], Maybe Attachment, Maybe Attachment)

mkRenderTarget :: (IsRenderTarget cs, GetRectangle cs Int) => cs -> YageResource (RenderTarget cs)
mkRenderTarget b =  RenderTarget b <$> createFramebuffer cs d s <*> pure (b^.asRectangle)
  where (cs,d,s) = getAttachments b

instance (IsRenderTarget t, Resizeable2D t) => Resizeable2D (RenderTarget t) where
  resize2D t w h = do
    new <- resize2D (t^.renderTarget) w h
    let (cs,d,s) = getAttachments (t^.renderTarget)
    fbo <- attachFramebuffer (t^.framebufferObj) cs d s
    return $ t & framebufferObj .~ fbo & targetRectangle.extend .~ V2 w h & renderTarget .~ new
