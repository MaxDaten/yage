{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Yage.Pipeline.Deferred.Light where

import Yage.Prelude
import Yage.Lens


import Yage.Scene
import Yage.Light
import Yage.Primitives
import Yage.Uniforms

import Yage.Rendering.Mesh
import Yage.Rendering.Transformation

import Yage.Pipeline.Deferred.Spec

mkLight :: Light -> SceneLight LitVertex
mkLight light = 
    let (vol, trans) = lightData
    in SceneLight vol trans light (GLDrawSettings Triangles (Just Front))
    where
        lightData = case lightType light of
            Pointlight{..}    -> (makeSimpleTriMesh "plight" . vertices . triangles $ geoSphere 2 1
                                 , idTransformation & transPosition .~ (realToFrac <$> pLightPosition)
                                                    & transScale    .~ (realToFrac <$> pLightRadius)
                                 )
            Spotlight{..}     -> error "Yage.Pipeline.Deferred.Light.mkLight: Spotlight not supported"
            OmniDirectional _ -> error "Yage.Pipeline.Deferred.Light.mkLight: OmniDirectional not supported"




lightAttributes :: Light -> Uniforms YLightAttributes
lightAttributes Light{lightType,lightAttribs} = case lightType of
    Pointlight{..}    -> lightPosition =: (realToFrac <$> pLightPosition)                    <+>
                         lightRadius   =: (realToFrac <$> pLightRadius)                      <+>
                         lightSpecular =: (realToFrac <$> lightSpecularColor lightAttribs)   <+>
                         lightDiffuse  =: (realToFrac <$> lightDiffuseColor lightAttribs)    <+> 
                         lightAmbient  =: (realToFrac <$> lightAmbientColor lightAttribs)    <+>
                         lightAtten    =: (realToFrac <$> lightAttenuation lightAttribs)     <+>
                         lightSpecExp  =: (realToFrac $ lightSpecularExp lightAttribs)
    Spotlight{..}     -> error "Yage.Pipeline.Deferred.Light.lightAttributes: Spotlight not supported"
    OmniDirectional _ -> error "Yage.Pipeline.Deferred.Light.lightAttributes: OmniDirectional not supported"
