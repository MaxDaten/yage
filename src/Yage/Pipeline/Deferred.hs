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
import Yage.HDR
import Yage.UI.GUI

import Yage.Pipeline.Types

import Yage.Pipeline.Deferred.ResourceLoader  as ResourceLoader
import Yage.Pipeline.Deferred.GeometryPass    as Pass
import Yage.Pipeline.Deferred.LightPass       as Pass
import Yage.Pipeline.Deferred.SkyPass         as Pass
import Yage.Pipeline.Deferred.ScreenPass      as Pass
import Yage.Pipeline.Deferred.DownsamplePass  as Pass
import Yage.Pipeline.Deferred.HDR             as Pass
import Yage.Pipeline.Deferred.Common          as Pass
import Yage.Pipeline.Deferred.GuiPass         as Pass

type DeferredEnvironment = Environment Light Pass.SkyEntityDraw
type DeferredScene       = Scene HDRCamera GeoEntityDraw DeferredEnvironment GUI

yDeferredLighting :: YageRenderSystem DeferredScene ()
yDeferredLighting viewport scene =
    let -- renderRes                     = viewport & rectangle %~ fmap (/2.0)
        cam                     = scene^.sceneCamera.hdrCamera
        baseDescr               = Pass.geoPass viewport
        runBasePass             = runRenderPass baseDescr
        baseData                = geoFrameData viewport cam
    in do
    -- render out our geometric attributes (color, normal, ...)
    baseData `runBasePass` ( toList $ toGeoEntity cam <$> scene^.sceneEntities )

    -- calculate lighting based on attributes + bloom & apply tone mapping
    hdrTex <- Pass.hdrLightingPass baseDescr viewport scene

    -- rendered gui elements (TODO: should be gamma correct)
    guiTex <- Pass.runGuiPass hdrTex viewport ( scene^.sceneGui )
    --guiTex <- return $ mkTexture "BLACKDUMMY" $ Texture2D $ blackDummy TexRGB8

    -- bring it to the default render target - the screen
    Pass.screenPass viewport [ hdrTex, guiTex ]
