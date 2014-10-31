{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-orphans -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Yage.Pipeline.Deferred.LightPass where

import Yage.Prelude
import Yage.Lens
import Yage.Math

import Yage.Geometry as Geometry
import Yage.Geometry3D
import Yage.Uniforms as U
import Yage.Camera
import Yage.Viewport
import Yage.Scene
import Yage.Transformation
import Yage.Material
import Yage.TH.Shader

import Yage.Rendering

import Yage.Pipeline.Deferred.Common
import Yage.Pipeline.Deferred.GeometryPass

import qualified Graphics.Rendering.OpenGL as GL



type YLightPosition              = "Light.LightPosition"             ::: V4 GLfloat
type YConeAnglesAndRadius        = "Light.LightConeAnglesAndRadius"  ::: V3 GLfloat
type YLightColor                 = "Light.LightColor"                ::: V3 GLfloat
type YLightDirection             = "Light.LightDirection"            ::: V3 GLfloat
-- type YLightAttenuation   = "Light.Attenuation"     ::: V3 GLfloat
-- type YSpecularExp        = "Light.SpecularExp"     ::: GLfloat

type YLightAttributes    = [ YLightPosition
                           , YConeAnglesAndRadius
                           , YLightDirection
                           , YLightColor
                           -- , YLightAttenuation
                           -- , YSpecularExp
                           ]


uLightPosition :: SField YLightPosition
uLightPosition = SField

uConeAnglesAndRadius :: SField YConeAnglesAndRadius
uConeAnglesAndRadius = SField

uLightColor :: SField YLightColor
uLightColor = SField

uLightDirection :: SField YLightDirection
uLightDirection = SField

-- uLightAttenuation :: SField YLightAttenuation
-- uLightAttenuation = SField

-- uLightSpecularExp :: SField YSpecularExp
-- uLightSpecularExp = SField

type LitPerFrameUni     = PerspectiveUniforms ++ [ YViewToScreen, YViewportDim, YZProjRatio, YGamma ]
type LitPerFrameTex     = [ YAlbedoTex, YNormalTex, YDepthTex, YEnvironmentCubeMap ]

type LitPerEntityUni    = '[ YModelMatrix ] ++ YLightAttributes

type LitVertex          = Vertex (Y'P3 GLfloat)

data LitPassChannels = LitPassChannels
    { lBufferChannel :: Texture
    , lDepthChannel  :: Texture
    }

type LitUni = LitPerFrameUni ++ LitPerEntityUni
type LitTex = LitPerFrameTex
type LitShader = Shader LitUni LitTex LitVertex

type LightPass = YageDeferredPass LitPassChannels LitShader

type LitPassScene ent sky = Scene ent (Environment Light sky)

lightPass :: GeometryPass -> Viewport Int -> (Environment Light sky) -> LightPass
lightPass base viewport environment =
    passPreset target (viewport^.rectangle) (ShaderUnit shaderProg)
       & passPreRendering .~ preRendering

    where

    target =
        let GeoPassChannelsF{..} = base^.renderTargets
        in RenderTarget "light-fbo" $ LitPassChannels
                                { lBufferChannel   = lightTex
                                , lDepthChannel    = gDepthChannel
                                }

    lightSpec       = mkTextureSpec (viewport^.rectangle.extend) GL.Float GL.RGBA GL.RGBA16F
    lightTex        = mkTargetTexture "lbuffer" lightSpec

    shaderProg = ShaderProgramUnit
                    { _shaderName    = "LightPass.hs"
                    , _shaderSources = [ $(vertexFile "res/glsl/pass/lightPass.vert")^.shaderSource
                                       , $(fragmentFile "res/glsl/pass/lightPass.frag")^.shaderSource
                                       ]
                    }
    preRendering   = io $ do
        GL.viewport     GL.$= viewport^.glViewport
        let AmbientLight ambientColor = environment^.envAmbient
            V3 r g b                  = realToFrac <$> ambientColor
        GL.clearColor   GL.$= GL.Color4 r g b 0

        GL.depthFunc    GL.$= Nothing           -- disable func add
        GL.depthMask    GL.$= GL.Disabled       -- writing to depth is disabled

        GL.blend        GL.$= GL.Enabled        -- could reject background frags!
        GL.blendEquation GL.$= GL.FuncAdd
        GL.blendFunc    GL.$= (GL.One, GL.One)

        -- we reject front faces because of culling if cam is in volume
        GL.cullFace     GL.$= Just GL.Front
        GL.frontFace    GL.$= GL.CCW
        GL.polygonMode  GL.$= (GL.Fill, GL.Fill)

        GL.clear        [ GL.ColorBuffer ]



litPerFrameData :: GeometryPass -> Viewport Int -> Camera -> Material MaterialColorAlpha  -> ShaderData LitPerFrameUni LitPerFrameTex
litPerFrameData base viewport camera envMat = ShaderData lightUniforms attributeTextures
    where

    lightUniforms   :: Uniforms LitPerFrameUni
    lightUniforms   =
        let (V2 near far)           = realToFrac <$> V2 (camera^.cameraPlanes.camZNear) (camera^.cameraPlanes.camZFar)
            zProj                   = V2 ( ( far + near ) / ( far - near ) )
                                         ( ( 2.0 * near * far ) / ( far - near ) )
            dim                     = fromIntegral <$> viewport^.rectangle.extend
            theGamma                = realToFrac $ viewport^.viewportGamma
            Rectangle xy0 xy1       = fromIntegral <$> viewport^.rectangle
            viewToScreenM           = orthographicMatrix (xy0^._x) (xy1^._x) (xy1^._y) (xy0^._y) 0.0 1.0
        in
        perspectiveUniforms viewport camera     <+>
        viewToScreenMatrix  =: viewToScreenM    <+>
        viewportDim         =: dim              <+>
        zProjRatio          =: zProj            <+>
        gamma               =: theGamma

    attributeTextures :: Textures LitPerFrameTex
    attributeTextures =
        textureSampler =: ( base^.renderTargets.to gAlbedoChannel )   <+>
        textureSampler =: ( base^.renderTargets.to gNormalChannel )   <+>
        textureSampler =: ( base^.renderTargets.to gDepthChannel  )   <+>
        textureSampler =: (envMat^.matTexture)


instance FramebufferSpec LitPassChannels RenderTargets where
    fboColors LitPassChannels{lBufferChannel} =
        [ Attachment (ColorAttachment 0) $ TextureTarget GL.Texture2D lBufferChannel 0
        ]

    fboDepth LitPassChannels{lDepthChannel} =
        Just $ Attachment DepthAttachment $ TextureTarget GL.Texture2D lDepthChannel 0



toLitEntity :: Viewport Int -> Camera -> Light -> RenderEntity LitVertex (ShaderData LitPerEntityUni '[])
toLitEntity viewport cam Light{..} = case _lightType of

    Pointlight{..}      ->
        let transform  = idTransformation & transPosition .~ _pLightPosition
                                          & transScale    .~ pure _pLightRadius

            uniforms   =    modelMatrix                 =: ( fmap realToFrac <$> transform^.transformationMatrix )
                        <+> uLightPosition              =: ( realToFrac      <$> _pLightPosition^.to viewSpacePos._xyz.to point )
                        <+> uConeAnglesAndRadius        =: ( realToFrac      <$> ( 0 & _z .~ _pLightRadius ) )
                        <+> uLightDirection             =: ( realToFrac      <$> 0.0 )
                        <+> uLightColor                 =: ( realToFrac      <$> lightEnergy )
            renderData = mkFromVerticesF "plight" . map (position3 =:) . vertices . triangles $ geoSphere 2 1
        in RenderEntity renderData (ShaderData uniforms mempty) lightVolumeSettings

    Spotlight{..}       ->
        let half                = _sLightOuterAngle / 2.0
            basisRadius         = _sLightRadius * sin half / sin (pi / 2.0 - half)
            normalizedLightDir  = normalize $ _sLightDirection^.to viewSpaceDirection._xyz
            transform           = idTransformation
                                    & transPosition    .~ ( realToFrac <$> _sLightPosition )
                                    & transScale       .~ V3 basisRadius _sLightRadius basisRadius
                                    & transOrientation .~ lookAt worldSpace ( normalize _sLightDirection )

            uniforms   =    modelMatrix                 =: ( fmap realToFrac <$> transform^.transformationMatrix )
                        <+> uLightPosition              =: ( realToFrac      <$> _sLightPosition^.to viewSpacePos._xyz.to point )
                        <+> uConeAnglesAndRadius        =: ( realToFrac      <$> V3 (cos $ _sLightInnerAngle / 2) (cos $ _sLightOuterAngle / 2) _sLightRadius )
                        <+> uLightDirection             =: ( realToFrac      <$> normalizedLightDir )
                        <+> uLightColor                 =: ( realToFrac      <$> lightEnergy )
            renderData = mkFromVerticesF "slight" . map (position3 =:) . vertices . triangles $ cone 1 1 24
        in RenderEntity renderData (ShaderData uniforms mempty) lightVolumeSettings

    -- | Directional lighting is rendered with a fullscreen quad
    DirectionalLight{..}  ->
        let dim          = realToFrac <$> viewport^.rectangle.extend
            trans        = idTransformation & transPosition._xy .~ 0.5 * dim
                                            & transScale        .~ V3 ( dim^._x ) (- (dim^._y) ) (1)
            scaleM       = kronecker . point $ trans^.transScale
            transM       = mkTransformation (trans^.transOrientation) (trans^.transPosition)
            modelM       = transM !*! scaleM

            uniforms   =    modelMatrix                 =: ( fmap realToFrac <$> modelM )
                        <+> uLightPosition              =: ( realToFrac      <$> 0.0 )
                        <+> uConeAnglesAndRadius        =: ( realToFrac      <$> 0.0 )
                        <+> uLightDirection             =: ( realToFrac      <$> _dLightDirection^.to viewSpaceDirection._xyz )
                        <+> uLightColor                 =: ( realToFrac      <$> lightEnergy )
            renderData = mkFromVerticesF "dlight" . vertices . triangles $ targetFace

        in RenderEntity renderData (ShaderData uniforms mempty) (GLDrawSettings GL.Triangles (Just GL.Back))

    where
    worldSpace           = V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)
    lightEnergy          = _lightColor ^* _lightIntensity
    viewSpacePos p       = cam^.cameraMatrix !* point p
    viewSpaceDirection d = normalize $ cam^.cameraMatrix !* vector d
    lightVolumeSettings  = GLDrawSettings GL.Triangles (Just GL.Front)

    targetFace           :: Face LitVertex
    targetFace           = Face
        (position3 =: V3 (-0.5) ( 0.5) 0.0)
        (position3 =: V3 (-0.5) (-0.5) 0.0)
        (position3 =: V3 ( 0.5) (-0.5) 0.0)
        (position3 =: V3 ( 0.5) ( 0.5) 0.0)
