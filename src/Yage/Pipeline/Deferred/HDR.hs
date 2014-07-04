module Yage.Pipeline.Deferred.HDR where

import Yage.Prelude
import Yage.Lens

import Yage.Rendering
import Yage.Viewport
import Yage.HDR
import Yage.Scene
import Yage.Pipeline.Types
import qualified Yage.Pipeline.Deferred.LightPass       as L
import qualified Yage.Pipeline.Deferred.GeometryPass    as G
import qualified Yage.Pipeline.Deferred.DownsamplePass  as D
import qualified Yage.Core.OpenGL as GL


type HDRScene ent sky = Scene HDRCamera ent (Environment L.LitEntityDraw sky)
hdrLightingPass :: G.GeometryPass -> YageRenderSystem (HDRScene ent sky) Texture
hdrLightingPass geometryPass viewport scene = 
    let cam         = scene^.sceneCamera.hdrCamera
        lightPass   = L.lightPass geometryPass viewport cam (scene^.sceneEnvironment)
        lightTex    = L.lBufferChannel . renderTargets $ lightPass
        lights      = scene^.sceneEnvironment.envLights
    in do
        lightPass  `runRenderPass` (L.toLitEntity <$> lights)
        bloomedTex <- brightPass 4 lightTex
        return $ bloomedTex
        -- filter
        -- downsample-bloom
        -- compose



brightPass :: Int -> Texture -> RenderSystem Texture
brightPass samples tex = do
    t1 <- D.downsample 2 tex
    t2 <- D.downsample 4 tex
    t3 <- D.downsample 8 tex
    t4 <- D.downsample 16 tex
    return t4