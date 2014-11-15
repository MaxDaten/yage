{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Yage.Pipeline.Deferred.HDR where

import Yage.Prelude                                         hiding ( toList, last, head )
import Yage.Lens                                            hiding ( cons )

import Data.List                                            ( last, tail )

import Yage.Rendering
import Yage.HDR
import Yage.Scene
import Yage.Material
import Yage.Pipeline.Types

import qualified Yage.Pipeline.Deferred.LightPass             as L
import qualified Yage.Pipeline.Deferred.GeometryPass          as G
import qualified Yage.Pipeline.Deferred.SkyPass               as S
-- import qualified Yage.Pipeline.Deferred.AdditiveCompose       as A
import qualified Yage.Pipeline.Deferred.GlareDetectionPass    as Glare
import qualified Yage.Pipeline.Deferred.ToneMapPass           as T
import Yage.Pipeline.Deferred.GaussFilter


type HDRScene ent = Scene HDRCamera ent (Environment Light S.SkyEntity)

hdrLightingPass :: G.GeometryPass -> YageRenderSystem (HDRScene ent dat) Texture
hdrLightingPass geometryPass viewport scene =
    let cam             = scene^.sceneCamera.hdrCameraHandle
        bloomSettings   = scene^.sceneCamera.hdrBloomSettings

        lightDescr      = L.lightPass geometryPass viewport (scene^.sceneEnvironment)
        lightData       = L.litPerFrameData geometryPass viewport cam (scene^.radianceMap.matTexture)

        lightTex        = lightDescr^.renderTargets.to L.lBufferChannel
        lights          = scene^.sceneEnvironment.envLights
        skyData         = S.skyFrameData viewport cam

        skyChannels     = RenderTarget "fbo-sky" S.SkyInChannels
                            { S.sBufferChannel = lightDescr^.renderTargets.to L.lBufferChannel
                            , S.sDepthChannel  = lightDescr^.renderTargets.to L.lDepthChannel
                            }
        skyPass         = runRenderPass $ S.skyPass skyChannels viewport
        lightPass       = runRenderPass lightDescr

        composeAndToneMap base ts = T.runToneMapPass base ts viewport (scene^.sceneCamera)

        bloomPasses     = bloomSettings^.bloomGaussPasses
        bFactor         = bloomSettings^.bloomFactor
        bloomWeights    :: [ Double ]
        bloomWeights    = reverse $ map (\x -> bFactor * fromIntegral (2^x :: Int) / 127.0) [ (0::Int) .. bloomPasses ]
    in do
        lightData `lightPass` ( L.toLitEntity viewport cam <$> lights )
        skyData   `skyPass`   ( S.toSkyEntity <$> scene^.sceneEnvironment.envSky.to repack )

        bloomedTextureSet <- Glare.glareDetection
                                ( bloomSettings^.bloomPreDownsampling )
                                ( scene^.sceneCamera.hdrExposure )
                                ( bloomSettings^.bloomThreshold ) lightTex
                                >>= bloomPass (bloomSettings^.bloomWidth) ( bloomSettings^.bloomGaussPasses )

        lightTex `composeAndToneMap` zip bloomWeights bloomedTextureSet



bloomPass :: Double -> Int -> Texture -> RenderSystem [ Texture ]
bloomPass _ 0          _           = return []
bloomPass bloomWidth numSamples baseTexture =
    let targets = map (\idx -> mkTargets $ 2^idx) $ [1..numSamples]
    in tail <$> foldM ( \txs target -> fmap ((++) txs . singleton) $ gaussFilter bloomWidth (last txs) target ) [baseTexture] targets

    where

    baseId   = baseTexture^.textureId
    baseSpec = baseTexture^.textureSpec

    -- | returns both targets for horizontal and vertical blur
    mkTargets :: Int -> (RenderTarget SingleRenderTarget, RenderTarget SingleRenderTarget)
    mkTargets downFactor = ( mkTarget "gaussX" downFactor
                           , mkTarget "gaussY" downFactor
                           )

    mkTarget :: String -> Int -> RenderTarget SingleRenderTarget
    mkTarget directionId downFactor =
        mkSingleTargetFromSpec
            ( mkPassId directionId downFactor )
            ( baseSpec & texSpecDimension %~ \(V2 w h) -> V2 (max (w `div` downFactor) 1) (max(h `div` downFactor) 1) )

    mkPassId :: String -> Int -> ByteString
    mkPassId directionId factor = toStrict . encodeUtf8 $ format "-{}-{}-{}" ( Shown baseId, Shown factor, Shown directionId )


environmentMap :: Getter (HDRScene ent dat) (Material MaterialColorAlpha)
environmentMap = sceneEnvironment.to getter where
    -- TODO : BLACK DUMMY after resource overhaul
    getter (Environment _ Nothing _)        = error "Yage.Pipeline.Deferred.HDR.environmentMap: missing env map"
    getter (Environment _ (Just sky) _)     = sky^.materials.S.skyEnvironmentMap

radianceMap :: Getter (HDRScene ent dat) (Material MaterialColorAlpha)
radianceMap = sceneEnvironment.to getter where
    -- TODO : BLACK DUMMY after resource overhaul
    getter (Environment _ Nothing _)        = error "Yage.Pipeline.Deferred.HDR.environmentMap: missing env map"
    getter (Environment _ (Just sky) _)     = sky^.materials.S.skyRadianceMap

