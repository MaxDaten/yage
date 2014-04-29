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

import Yage.Pipeline.Deferred.ResourceLoader  as ResourceLoader
import Yage.Pipeline.Deferred.GeometryPass    as Pass
import Yage.Pipeline.Deferred.LightPass       as Pass
import Yage.Pipeline.Deferred.SkyPass         as Pass
import Yage.Pipeline.Deferred.ScreenPass      as Pass



yDeferredLighting :: ViewportI -> SScene Pass.GeoVertex Pass.GeoMaterial Pass.LitVertex -> RenderSystem ()
yDeferredLighting viewport scene = 
    let base       = Pass.geoPass viewport scene
        lighting   = Pass.lightPass base viewport scene
        atmosphere = Pass.skyPass lighting viewport scene
        final      = Pass.screenPass (Pass.lBufferChannel . renderTargets $ lighting) viewport
    in do
    base        `runRenderPass`  (scene^.sceneEntities)
    lighting    `runRenderPass`  (scene^.sceneEnvironment.envLights)
    atmosphere  `runRenderPass`  (toList $ scene^.sceneEnvironment.envSky)
    final       `runRenderPass`  [(Pass.Screen viewport)]
