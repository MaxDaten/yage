module Yage.Pipeline.Types where

--import Yage.Rendering.Backend.RenderPass
import Yage.Prelude
import Yage.Rendering
import Yage.Viewport


type YageRenderSystem scene = Viewport Int -> scene -> RenderSystem ()