{-# LANGUAGE TemplateHaskell #-}
module Yage.Rendering.RenderContext
  ( RenderContext(..)
  , ctxViewport
  ) where


import Yage.Prelude
import Yage.Lens
import Yage.Viewport


data RenderContext = RenderContext
  { _ctxViewport :: Viewport Int
  -- ^ the full viewport of the render context in px
  }

-- makeFieldsWith abbreviatedFields ''RenderContext
makeClassy ''RenderContext

instance HasViewport RenderContext Int where
  viewport = ctxViewport
