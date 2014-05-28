module Yage.Pipeline.Types where

--import Yage.Rendering.Backend.RenderPass
import Yage.Rendering
--import Yage.Scene


type YageRenderSystem scene = ViewportI -> scene -> RenderSystem ()