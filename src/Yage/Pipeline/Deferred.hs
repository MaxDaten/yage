{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
module Yage.Pipeline.Deferred
    ( module Yage.Pipeline.Deferred
    , module Pass
    , module Yage.Viewport
    , module RenderSystem
    ) where

import           Yage.Lens
import           Yage.Prelude

import           Yage.HDR
import           Yage.Rendering
import           Yage.Scene
import           Yage.UI.GUI
import           Yage.Viewport
import           Yage.Rendering.Backend.RenderSystem   as RenderSystem

import           Yage.Pipeline.Types

import           Yage.Pipeline.Deferred.Common         as Pass
import           Yage.Pipeline.Deferred.DownsamplePass as Pass
import           Yage.Pipeline.Deferred.GeometryPass   as Pass
import           Yage.Pipeline.Deferred.GuiPass        as Pass
import           Yage.Pipeline.Deferred.HDR            as Pass
import           Yage.Pipeline.Deferred.LightPass      as Pass
import           Yage.Pipeline.Deferred.ScreenPass     as Pass
import           Yage.Pipeline.Deferred.SkyPass        as Pass

type DeferredEnvironment = Environment Light Pass.SkyEntity
type DeferredScene       = Scene HDRCamera GeoEntity DeferredEnvironment GUI

yDeferredLighting :: YageRenderSystem DeferredScene ()
yDeferredLighting viewport scene =
    let -- renderRes                     = viewport & rectangle %~ fmap (/2.0)
        cam                     = scene^.sceneCamera.hdrCameraHandle
        baseDescr               = Pass.geoPass viewport
        runBasePass             = runRenderPass baseDescr
        baseData                = geoFrameData viewport cam
    in do
    -- render out our geometric attributes (color, normal, ...)
    baseData `runBasePass` ( toGeoEntity cam <$> scene^.sceneEntities )

    -- calculate lighting based on attributes + bloom & apply tone mapping
    hdrTex <- Pass.hdrLightingPass baseDescr viewport scene

    -- rendered gui elements (TODO: should be gamma correct)
    guiTex <- Pass.runGuiPass hdrTex viewport ( scene^.sceneGui )

    -- bring it to the default render target - the screen
    Pass.screenPass viewport [ hdrTex, guiTex ]
