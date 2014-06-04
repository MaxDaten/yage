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

import Yage.Scene
import Yage.Rendering

import Yage.Pipeline.Types

import Yage.Pipeline.Deferred.ResourceLoader  as ResourceLoader
import Yage.Pipeline.Deferred.GeometryPass    as Pass
import Yage.Pipeline.Deferred.LightPass       as Pass
import Yage.Pipeline.Deferred.SkyPass         as Pass
import Yage.Pipeline.Deferred.ScreenPass      as Pass

type DeferredEnvironment = Environment Pass.LitEntityDraw Pass.SkyEntityDraw
type DeferredScene       = Scene GeoEntityDraw DeferredEnvironment 

yDeferredLighting :: YageRenderSystem DeferredScene
yDeferredLighting viewport scene = 
    let base       = Pass.geoPass viewport scene
        lighting   = Pass.lightPass base viewport scene
        atmosphere = Pass.skyPass lighting viewport scene
        final      = Pass.screenPass (Pass.lBufferChannel . renderTargets $ lighting) viewport
        --final      = Pass.screenPass (Pass.gNormalChannel . renderTargets $ base) viewport
    in do
    base        `runRenderPass`  ( toGeoEntity scene <$> scene^.sceneEntities )
    
    lighting    `runRenderPass`  ( toLitEntity       <$> scene^.sceneEnvironment.envLights )
    
    atmosphere  `runRenderPass`  ( toSkyEntity       <$> scene^.sceneEnvironment.envSky.to toList )
    
    final       `runRenderPass`  [ toScrEntity        $  Pass.Screen viewport ]
