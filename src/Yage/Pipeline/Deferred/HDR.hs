{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Yage.Pipeline.Deferred.HDR where

import Yage.Prelude hiding (toList)
import Yage.Lens

import Data.Foldable (toList)

import Yage.Rendering
import Yage.Viewport
import Yage.HDR
import Yage.Scene
import Yage.Pipeline.Types
import Yage.Uniforms as U
import Yage.TH.Shader

import Yage.Pipeline.Deferred.Common
import Yage.Pipeline.Deferred.Sampler
import qualified Yage.Pipeline.Deferred.LightPass       as L
import qualified Yage.Pipeline.Deferred.GeometryPass    as G
import qualified Yage.Pipeline.Deferred.DownsamplePass  as D
import qualified Yage.Pipeline.Deferred.SkyPass         as S
import Yage.Pipeline.Deferred.GaussFilter


type HDRScene ent = Scene HDRCamera ent (Environment L.LitEntityDraw S.SkyEntityDraw)
hdrLightingPass :: G.GeometryPass -> YageRenderSystem (HDRScene ent) Texture
hdrLightingPass geometryPass viewport scene = 
    let cam             = scene^.sceneCamera.hdrCamera
        bloomSettings   = scene^.sceneCamera.hdrBloomSettings
        
        lightPass       = L.lightPass geometryPass viewport (scene^.sceneEnvironment)
        lightData       = L.litPerFrameData geometryPass viewport cam

        lightTex        = L.lBufferChannel . renderTargets $ lightPass
        lights          = scene^.sceneEnvironment.envLights

        atmosphere      = S.skyPass lightPass viewport
        atmosphereData  = S.skyFrameData viewport cam
    in do
        runRenderPass lightPass lightData       ( L.toLitEntity <$> lights )
        runRenderPass atmosphere atmosphereData ( S.toSkyEntity <$> scene^.sceneEnvironment.envSky.to toList )

        bloomedTex <- brightFilter lightTex ( scene^.sceneCamera.hdrWhitePoint )
                      >>= D.downsampleBoxed5x5 ( bloomSettings^.bloomPreDownsampling ) 
                      >>= bloomPass ( bloomSettings^.bloomGaussPasses ) 
        (1.0, lightTex) `additiveCompose` (bloomSettings^.bloomFactor, bloomedTex)


type BrightUniforms = [ YProjectionMatrix, YTextureSize "TextureSize", YWhitePoint, YModelMatrix ]
type BrightTextures = '[ TextureUniform "FilterTexture" ]

type BrightPass     = YageTextureSampler SingleRenderTarget BrightUniforms BrightTextures

brightFilter :: Texture -> Float -> RenderSystem Texture
brightFilter tex whitePoint =
    let target      = mkSingleTextureTarget $ tex & textureId <>~ "-brightPass"
        bright      :: BrightPass
        bright      = samplerPass "brightFilter" target (target^.asRectangle) $(fragmentFile "res/glsl/pass/brightFilter.frag")

        frameData   :: ShaderData [ YProjectionMatrix, YTextureSize "TextureSize", YWhitePoint ] BrightTextures
        frameData   = singleTextureSampler (target^.asRectangle) tex
                        & shaderUniforms <<+>~ U.whitePoint =: realToFrac whitePoint

    in do
        runRenderPass bright frameData [ targetEntity tex ]
        return $ target^.targetTexture


bloomPass :: Int -> Texture -> RenderSystem Texture
bloomPass samples tex = foldM (flip.const $ gaussFilter) tex [0..samples-1]


type AddUniforms = [ YProjectionMatrix
                   , YTextureSize "BaseTextureSize"
                   , YTextureSize "AddTextureSize"
                   , "BaseWeight" ::: GLfloat
                   , "AddWeight" ::: GLfloat
                   ]
type AddTextures = [ TextureUniform "BaseTexture", TextureUniform "AddTexture" ]
type AddFrameData = ShaderData AddUniforms AddTextures

type AdditiveShader = Shader (AddUniforms ++ '[YModelMatrix]) AddTextures TargetVertex
type AdditiveComposePass = YageDeferredPass SingleRenderTarget AdditiveShader
                            
-- | inplace additive
-- adds second argument texture to first argument 
additiveCompose :: (Float, Texture) -> (Float, Texture) -> RenderSystem Texture
additiveCompose (baseWeight, baseTex) (addWeight, toAdd) =
    let target          = mkSingleTextureTarget $ baseTex & textureId <>~ ("+" ++ toAdd^.textureId)
        
        additivePass    :: AdditiveComposePass
        additivePass    = samplerPass "Yage.AdditiveCompose" target (target^.asRectangle) $(fragmentFile "res/glsl/pass/addCompose.frag")

        frameData       :: AddFrameData
        frameData       = (targetRectangleData (target^.asRectangle) `append`
                          sampleData baseTex `append`
                          sampleData toAdd)
                                & shaderUniforms <<+>~ weights

    in do
        runRenderPass additivePass frameData [ targetEntity baseTex ]
        return $ target^.targetTexture

    where

    weights = SField =: realToFrac baseWeight <+> SField =: realToFrac addWeight


instance Implicit (FieldNames '[ TextureUniform "FilterTexture" ]) where
    implicitly = SField =: "FilterTexture"

instance Implicit ( FieldNames AddTextures ) where
    implicitly = 
        SField =: "BaseTexture" <+>
        SField =: "AddTexture"
