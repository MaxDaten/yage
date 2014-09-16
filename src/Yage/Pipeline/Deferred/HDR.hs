{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Yage.Pipeline.Deferred.HDR where

import Yage.Prelude hiding (toList)
import Yage.Lens

import Data.Foldable (toList)

import Yage.Rendering
import Yage.HDR
import Yage.Scene
import Yage.Material
import Yage.Pipeline.Types

import qualified Yage.Pipeline.Deferred.LightPass       as L
import qualified Yage.Pipeline.Deferred.GeometryPass    as G
import qualified Yage.Pipeline.Deferred.DownsamplePass  as D
import qualified Yage.Pipeline.Deferred.SkyPass         as S
import qualified Yage.Pipeline.Deferred.AdditiveCompose as A
import qualified Yage.Pipeline.Deferred.BrightFilter    as B
import qualified Yage.Pipeline.Deferred.ToneMapPass     as T
import Yage.Pipeline.Deferred.GaussFilter


type HDRScene ent = Scene HDRCamera ent (Environment L.LitEntityDraw S.SkyEntityDraw)

hdrLightingPass :: G.GeometryPass -> YageRenderSystem (HDRScene ent dat) Texture
hdrLightingPass geometryPass viewport scene =
    let cam             = scene^.sceneCamera.hdrCamera
        bloomSettings   = scene^.sceneCamera.hdrBloomSettings

        lightDescr      = L.lightPass geometryPass viewport (scene^.sceneEnvironment)
        lightData       = L.litPerFrameData geometryPass viewport cam (scene^.environmentMap)

        lightTex        = L.lBufferChannel . renderTargets $ lightDescr
        lights          = scene^.sceneEnvironment.envLights
        skyData         = S.skyFrameData viewport cam

        skyPass         = runRenderPass $ S.skyPass lightDescr viewport
        lightPass       = runRenderPass lightDescr

        toneMap tex     = T.runToneMapPass tex viewport (scene^.sceneCamera)
    in do
        lightData `lightPass` ( L.toLitEntity <$> lights )
        skyData   `skyPass`   ( S.toSkyEntity <$> scene^.sceneEnvironment.envSky.to toList )

        bloomedTex      <- B.brightFilter lightTex ( scene^.sceneCamera.hdrWhitePoint )
                           >>= D.downsampleBoxed5x5 ( bloomSettings^.bloomPreDownsampling )
                           >>= bloomPass ( bloomSettings^.bloomGaussPasses )

        toneMap =<< (1.0, lightTex) `A.additiveCompose` (bloomSettings^.bloomFactor, bloomedTex)



bloomPass :: Int -> Texture -> RenderSystem Texture
bloomPass samples tex = foldM (flip.const $ gaussFilter) tex [0..samples-1]


environmentMap :: Getter (HDRScene ent dat) RenderMaterial
environmentMap = sceneEnvironment.to getter where
    -- TODO : BLACK DUMMY after resource overhaul
    getter (Environment _ Nothing _)        = error "Yage.Pipeline.Deferred.HDR.environmentMap: missing env map"
    getter (Environment _ (Just sky) _)     = sky^.materials

