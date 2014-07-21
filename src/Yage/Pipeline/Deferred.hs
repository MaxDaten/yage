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
import Yage.Pipeline.Deferred.Common          as Pass

type DeferredEnvironment = Environment Pass.LitEntityDraw Pass.SkyEntityDraw
type DeferredScene       = Scene HDRCamera GeoEntityDraw DeferredEnvironment 

yDeferredLighting :: YageRenderSystem DeferredScene ()
yDeferredLighting viewport scene = 
    let -- renderRes                     = viewport & rectangle %~ fmap (/2.0)
        cam                           = scene^.sceneCamera.hdrCamera
        base                          = Pass.geoPass viewport
        baseData                      = geoFrameData viewport cam
        
        final                         = Pass.screenPass viewport
        screenData toScreen           = Pass.screenFrameData toScreen viewport (scene^.sceneCamera)
    in do
    runRenderPass base baseData ( toGeoEntity cam   <$> scene^.sceneEntities )
    
    hdrTex <- hdrLightingPass base viewport scene

    runRenderPass final (screenData hdrTex)  [ targetEntity $ viewport^.rectangle ]


{--
--}

