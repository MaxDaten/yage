{-# LANGUAGE OverloadedStrings          #-}
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
import Yage.Scene
import Yage.HDR

import Yage.Pipeline.Types

import Yage.Pipeline.Deferred.ResourceLoader  as ResourceLoader
import Yage.Pipeline.Deferred.GeometryPass    as Pass
import Yage.Pipeline.Deferred.LightPass       as Pass
import Yage.Pipeline.Deferred.SkyPass         as Pass
import Yage.Pipeline.Deferred.ScreenPass      as Pass

type DeferredEnvironment = Environment Pass.LitEntityDraw Pass.SkyEntityDraw
type DeferredScene       = Scene HDRCamera GeoEntityDraw DeferredEnvironment 

yDeferredLighting :: YageRenderSystem DeferredScene
yDeferredLighting viewport scene = 
    let cam        = scene^.sceneCamera.hdrCamera
        base       = Pass.geoPass viewport cam
        lighting   = Pass.lightPass base viewport cam (scene^.sceneEnvironment)
        atmosphere = Pass.skyPass lighting viewport cam
        final      = Pass.screenPass (Pass.lBufferChannel . renderTargets $ lighting) viewport (scene^.sceneCamera)
        --final      = Pass.screenPass (Pass.gNormalChannel . renderTargets $ base) viewport
    in do
    base        `runRenderPass`  ( toGeoEntity cam   <$> scene^.sceneEntities )
    
    lighting    `runRenderPass`  ( toLitEntity       <$> scene^.sceneEnvironment.envLights )
    
    atmosphere  `runRenderPass`  ( toSkyEntity       <$> scene^.sceneEnvironment.envSky.to toList )
    
    final       `runRenderPass`  [ toScrEntity        $  Pass.Screen viewport ]
