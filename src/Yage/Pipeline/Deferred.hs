{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
module Yage.Pipeline.Deferred
    ( module Yage.Pipeline.Deferred
    , module ResourceLoader
    , module Pass
    ) where

import Yage.Prelude
import Yage.Lens

import Yage.Rendering
import Yage.Rendering.Textures (texSpecDimension)
import Yage.Scene
import Yage.Viewport
import Yage.HDR

import Yage.Pipeline.Types

import Yage.Pipeline.Deferred.ResourceLoader  as ResourceLoader
import Yage.Pipeline.Deferred.GeometryPass    as Pass
import Yage.Pipeline.Deferred.LightPass       as Pass
import Yage.Pipeline.Deferred.SkyPass         as Pass
import Yage.Pipeline.Deferred.ScreenPass      as Pass
import Yage.Pipeline.Deferred.DownsamplePass  as Pass
import Yage.Pipeline.Deferred.HDR             as Pass

type DeferredEnvironment = Environment Pass.LitEntityDraw Pass.SkyEntityDraw
type DeferredScene       = Scene HDRCamera GeoEntityDraw DeferredEnvironment 

yDeferredLighting :: YageRenderSystem DeferredScene ()
yDeferredLighting viewport scene = 
    let cam                           = scene^.sceneCamera.hdrCamera
        base                          = Pass.geoPass viewport cam
        lighting                      = Pass.lightPass base viewport cam (scene^.sceneEnvironment)
        atmosphere                    = Pass.skyPass lighting viewport cam

        colorTex                      = Pass.lBufferChannel . renderTargets $ lighting

        final addTex                  = Pass.screenPass colorTex addTex viewport (scene^.sceneCamera)
        --final      = Pass.screenPass (Pass.gNormalChannel . renderTargets $ base) viewport
    in do
    base        `runRenderPass`  ( toGeoEntity cam   <$> scene^.sceneEntities )
    
    lighting    `runRenderPass`  ( toLitEntity       <$> scene^.sceneEnvironment.envLights )
    
    atmosphere  `runRenderPass`  ( toSkyEntity       <$> scene^.sceneEnvironment.envSky.to toList )
    
    bloomTex <- downsample 4 colorTex "downsamplepass1"

    final bloomTex `runRenderPass`  [ toScrEntity        $  Pass.Screen viewport ]

downsample :: Int -> Texture -> String -> RenderSystem Texture
downsample downfactor toDownsample targetName = 
    let inSize   = texSpecDimension $ toDownsample^.textureSpec
        outSize  = floor <$> ((fromIntegral <$> inSize) ^/ fromIntegral downfactor)
        toTarget@(RenderTarget _ (SingleRenderTarget tex)) = Pass.mkSingleTargetHDR targetName outSize
    in do
        Pass.downsamplePass toDownsample toTarget `runRenderPass` [ quadTarget 0 outSize ]
        return tex

{--
--}

