{-# OPTIONS_GHC -fno-warn-name-shadowing  #-}
{-# OPTIONS_GHC -fno-warn-orphans         #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

module Yage.Rendering.RenderTarget
  ( RenderTarget
  , IsRenderTarget(getAttachments)
  , renderTarget
  , framebufferObj
  , targetRectangle
  -- * Constructor
  , mkRenderTarget
  -- * Controlled Targets
  , autoResized
  , onChange
  -- * Target Size Definition
  , module Rectangle
  ) where

import Yage.Prelude
import Yage.Lens
import Yage.Math
import Data.Data
import Yage.Geometry.D2.Rectangle as Rectangle
import Yage.Rendering.Resources.GL.Framebuffer
import Yage.Rendering.Resources.GL.TextureFormat
import Yage.Resources
import Yage.Rendering.Resources.GL.Texture
import Yage.Rendering.RenderSystem

data RenderTarget cs = RenderTarget
  { _renderTarget       :: cs
  , _framebufferObj     :: Framebuffer
  , _targetRectangle    :: Rectangle Int
  } deriving (Show,Typeable,Data,Generic)

makeLenses ''RenderTarget

class IsRenderTarget cs where
  getAttachments :: cs -> ([Attachment], Maybe Attachment, Maybe Attachment)

mkRenderTarget :: (IsRenderTarget cs, GetRectangle cs Int) => cs -> YageResource (RenderTarget cs)
mkRenderTarget b =  RenderTarget b <$> createFramebuffer cs d s <*> pure (b^.asRectangle)
  where (cs,d,s) = getAttachments b

instance GetRectangle (RenderTarget t) Int where
  asRectangle = targetRectangle

instance (IsRenderTarget t, Resizeable2D t) => Resizeable2D (RenderTarget t) where
  resize2D t w h
    | t^.targetRectangle.extend == V2 w h = return t
    | otherwise = do
      new <- resize2D (t^.renderTarget) w h
      let (cs,d,s) = getAttachments new
      fbo <- attachFramebuffer (t^.framebufferObj) cs d s
      return $ t & framebufferObj         .~ fbo
                 & targetRectangle.extend .~ V2 w h
                 & renderTarget           .~ new

-- | Shortcut for 'Texture's to make a single texture to a single color 'RenderTarget'
instance IsRenderTarget (Texture2D px) where
  getAttachments tx = ([mkAttachment tx], Nothing, Nothing)

instance IsRenderTarget (Texture2D px, Texture2D (DepthComponent32F f)) where
  getAttachments (t,d) = ([mkAttachment t], Just $ mkAttachment d, Nothing)

instance IsRenderTarget (Texture2D px, Texture2D (DepthComponent24 f)) where
  getAttachments (t,d) = ([mkAttachment t], Just $ mkAttachment d, Nothing)

instance GetRectangle t Int => GetRectangle (t, b) Int where
  asRectangle = _1.asRectangle

-- * Controlled Targets

-- | Creates and returns constantly a 'RenderTarget' (never freed till termination of the application)
--  that is always resized when the incomming Viewport size changes
autoResized
  :: (MonadResource m, IsRenderTarget t, Resizeable2D t, GetRectangle t Int)
  => (Rectangle Int -> YageResource t)
  -- ^ inital constructor
  -> RenderSystem m (Rectangle Int) (RenderTarget t)
autoResized initRes = initTarget where
  initTarget = mkDynamicRenderPass $ \inRect ->
    (\(_, target) -> (target, doResize target)) <$> allocateAcquire (mkRenderTarget =<< (initRes inRect))
  doResize s = flip mkStatefulRenderPass s $ \target newRect -> do
    let V2 w h = newRect^.extend
    if (V2 w h == target^.targetRectangle.extend)
    then return (target,target)
    else do
      resizedTarget <- resize2D target w h <&> targetRectangle.extend .~ V2 w h
      return (resizedTarget, resizedTarget)

-- | Creates and returns constantly a 'RenderTarget' (never freed till termination of the application)
--  Reattaches targets on change.
onChange :: (MonadResource m, IsRenderTarget t, Eq t, GetRectangle t Int) => RenderSystem m t (RenderTarget t)
onChange = off where
  off = mkDynamicRenderPass $ \inTarget -> fmap (\(_,res) -> (res, on res)) $ allocateAcquire (mkRenderTarget inTarget)
  on res =  flip mkStatefulRenderPass res $ \lastTarget inTarget ->
    if lastTarget^.renderTarget == inTarget
    then return (lastTarget,lastTarget)
    else do
      let (cs, d, s) = getAttachments inTarget
      fbo <- attachFramebuffer (lastTarget^.framebufferObj) cs d s
      let new = lastTarget
            & framebufferObj   .~ fbo
            & targetRectangle  .~ inTarget^.asRectangle
            & renderTarget     .~ inTarget
      return (new,new)

