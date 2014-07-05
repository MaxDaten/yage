{-# LANGUAGE DataKinds, TypeOperators #-}
module Yage.Pipeline.Deferred.HDR where

import Yage.Prelude
import Yage.Lens

import Yage.Rendering
import Yage.Viewport
import Yage.HDR
import Yage.Scene
import Yage.Pipeline.Types
import Yage.Uniforms as U


import Yage.Pipeline.Deferred.Common
import Yage.Pipeline.Deferred.Sampler
import qualified Yage.Pipeline.Deferred.LightPass       as L
import qualified Yage.Pipeline.Deferred.GeometryPass    as G
import qualified Yage.Pipeline.Deferred.DownsamplePass  as D


type HDRScene ent sky = Scene HDRCamera ent (Environment L.LitEntityDraw sky)
hdrLightingPass :: G.GeometryPass -> YageRenderSystem (HDRScene ent sky) Texture
hdrLightingPass geometryPass viewport scene = 
    let cam         = scene^.sceneCamera.hdrCamera
        lightPass   = L.lightPass geometryPass viewport cam (scene^.sceneEnvironment)
        lightTex    = L.lBufferChannel . renderTargets $ lightPass
        lights      = scene^.sceneEnvironment.envLights
    in do
        lightPass  `runRenderPass` (L.toLitEntity <$> lights)
        bloomedTex <- bloomPass 4 =<< brightFilter lightTex (scene^.sceneCamera.hdrWhitePoint)
        (1.0, lightTex) `additiveCompose` (1.0, bloomedTex)
        -- filter
        -- downsample-bloom
        -- compose


type BrightPass = YageDeferredPass 
                            SingleRenderTarget
                            (ShaderData ( SamplerUniforms ++ '[ YWhitePoint ] ) '[ TextureUniform "FilterTexture" ])
                            TargetData
                            TargetVertex
brightFilter :: Texture -> Float -> RenderSystem Texture
brightFilter tex whitePoint =
    let target      = mkSingleTextureTarget $ tex & textureId <>~ "-brightPass"
        bright      :: BrightPass
        bright      = samplerPass tex target (target^.asRectangle) "res/glsl/pass/brightFilter.frag"
                        & passPerFrameData.shaderUniforms <<+>~ U.whitePoint =: realToFrac whitePoint

    in do
        bright `runRenderPass` [ targetEntity tex ]
        return $ target^.targetTexture


bloomPass :: Int -> Texture -> RenderSystem Texture
bloomPass samples tex = do
    t1 <- D.downsample 2 tex
    t2 <- D.downsample 4 tex
    t3 <- D.downsample 8 tex
    t4 <- D.downsample 16 tex
    a <- (1.0, t1) `additiveCompose` (1.0, t2)
    b <- (1.0, t3) `additiveCompose` (1.0, t4)
    (1.0, a) `additiveCompose` (1.0, b)


type AddUniforms = SamplerUniforms ++ [ "BaseWeight" ::: GLfloat
                                      , "AddWeight" ::: GLfloat 
                                      ]
type AddTextures = [ TextureUniform "BaseTexture", TextureUniform "AddTexture" ]
type AddFrameData = ShaderData AddUniforms AddTextures
type AdditiveComposePass = YageDeferredPass 
                            SingleRenderTarget
                            AddFrameData
                            TargetData
                            TargetVertex
-- | inplace additive
-- adds second argument texture to first argument 
additiveCompose :: (Float, Texture) -> (Float, Texture) -> RenderSystem Texture
additiveCompose (baseWeight, baseTex) (addWeight, toAdd) =
    let target          = mkSingleTextureTarget $ baseTex & textureId <>~ ("+" ++ toAdd^.textureId)
        
        additivePass    :: AdditiveComposePass
        additivePass    = samplerPass baseTex target (target^.asRectangle) "res/glsl/pass/addCompose.frag"
                            & passPerFrameData.shaderTextures <<+>~ Field =: toAdd
                            & passPerFrameData.shaderUniforms <<+>~ uniforms
    in do
        additivePass `runRenderPass` [ targetEntity baseTex ]
        return $ target^.targetTexture

    where

    uniforms = Field =: realToFrac baseWeight <+> Field =: realToFrac addWeight
