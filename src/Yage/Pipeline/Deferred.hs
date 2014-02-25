{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
module Yage.Pipeline.Deferred where

import Yage.Prelude
import Yage.Math
import Yage.Geometry
import Yage.Uniforms

import Yage.Rendering hiding (P3N3)
import Yage.Rendering.Viewport
import Yage.Rendering.Transformation

import Yage.Scene
import Yage.Camera
import qualified Graphics.Rendering.OpenGL as GL



type GeoGlobalUniforms = [YProjectionMatrix, YViewMatrix, YVPMatrix]
type GeoLocalUniforms  = [YModelMatrix, YNormalMatrix]
type GeoVertex         = P3N3


type LitGlobalUniforms = '[]
type LitLocalUniforms  = '[]
type LitVertex         = P3N3T2

type ScrGlobalUniforms = [YProjectionMatrix, YScreenTex]
type ScrLocalUniforms  = '[YModelMatrix]
type ScrVertex         = P3T2



yDeferredLightingDescr :: ViewportI 
                       -> Scene GeoVertex 
                       -> DeferredLightingDescr GeoGlobalUniforms GeoLocalUniforms GeoVertex
                                                LitGlobalUniforms LitLocalUniforms LitVertex
                                                ScrGlobalUniforms ScrLocalUniforms ScrVertex
yDeferredLightingDescr viewport scene =
    let size            = floor . (*) (viewport^.vpFactor) . fromIntegral <$> viewport^.vpSize
        albedoTex       = TextureBuffer "gbuffer-albedo" Texture2D $ GLBufferSpec RGB8 size
        normalTex       = TextureBuffer "gbuffer-normal" Texture2D $ GLBufferSpec RGB8 size
        specularTex     = TextureBuffer "gbuffer-specul" Texture2D $ GLBufferSpec RGB8 size
        glossyTex       = TextureBuffer "gbuffer-glossy" Texture2D $ GLBufferSpec RGB8 size
        depthBuff       = RenderbufferResource "gbuffer-depth" $ GLBufferSpec DepthComponent32 size

        geoPass         = PassDescr
                            { passFBSpec         = CustomFramebuffer ( "geo-fbo"
                                                 , colorAttachment (TextureTarget Texture2D albedoTex    0)
                                                 <> colorAttachment (TextureTarget Texture2D normalTex   0)
                                                 <> colorAttachment (TextureTarget Texture2D specularTex 0)
                                                 <> colorAttachment (TextureTarget Texture2D glossyTex   0)
                                                 <> depthAttachment (RenderbufferTarget depthBuff)
                                                 -- <> depthStencilAttachment (TextureTarget Texture2D depthStencilTex 0)
                                                 )
                            , passShader         = ShaderResource "res/glsl/pass/geoPass.vert" "res/glsl/pass/geoPass.frag"
                            , passGlobalUniforms = geoUniforms (fromIntegral <$> viewport) (scene^.sceneCamera)
                            , passEntityUniforms = entityUniforms
                            , passGlobalTextures = []
                            , passPreRendering   = io $ do
                                GL.viewport     GL.$= toGLViewport viewport
                                GL.clearColor   GL.$= GL.Color4 0 0 1 0
                                GL.depthFunc    GL.$= Just GL.Less
                                GL.depthMask    GL.$= GL.Enabled
                                GL.cullFace     GL.$= Just GL.Back
                                GL.frontFace    GL.$= GL.CCW
                                GL.polygonMode  GL.$= (GL.Fill, GL.Fill)

                                --GL.polygonMode  GL.$= (GL.Line, GL.Line)
                                GL.clear        [ GL.ColorBuffer, GL.DepthBuffer ]
                            , passPostRendering  = return ()
                            }


        --lightTex        = TextureBuffer "lbuffer-light" $ GLTextureSpec Texture2D RGBA' (800, 600)
        
        --lightPass       = PassDescr
        --                    { passFBSpec         = CustomFramebuffer ( "light-fbo"
        --                                         , colorAttachment        (TextureTarget Texture2D lightTex   0)
        --                                         <> depthStencilAttachment (TextureTarget Texture2D depthTex 0)
        --                                         )
        --                    --, passShader         = ShaderResource "res/glsl/pass/lightPass.vert" "res/glsl/pass/lightPass.frag"
        --                    , passShader         = ShaderResource "res/glsl/pass/screenPass.vert" "res/glsl/pass/screenPass.frag"
        --                    , passGlobalUniforms = screenUniforms viewport
        --                    , passEntityUniforms = viewportUniforms -- missing light attributes .....
        --                    }
        screenPass      = PassDescr
                            { passFBSpec         = DefaultFramebuffer
                            , passShader         = ShaderResource "res/glsl/pass/screenPass.vert" "res/glsl/pass/screenPass.frag"
                            , passGlobalUniforms = screenUniforms (fromIntegral <$> viewport) 
                            , passEntityUniforms = viewportUniforms
                            , passGlobalTextures = [TextureDefinition (0, "albedo") normalTex]
                            , passPreRendering   = io $ do
                                -- our 0/0 is top left (y-Axis is flipped)
                                GL.viewport     GL.$= toGLViewport viewport
                                GL.clearColor   GL.$= GL.Color4 1 1 1 0
                                GL.depthFunc    GL.$= Nothing    -- TODO to init
                                GL.cullFace     GL.$= Just GL.Back
                                GL.frontFace    GL.$= GL.CCW
                                GL.polygonMode  GL.$= (GL.Fill, GL.Fill)
                                GL.clear        [ GL.ColorBuffer, GL.DepthBuffer ]

                            , passPostRendering  = return ()
                            }
    in
    DeferredLightingDescr
    { dfGeoPassDescr        = geoPass
    , dfLightingPassDescr   = error "lightpass not defined" -- lightPass
    , dfFinalScreen         = screenPass
    }


geoUniforms :: Viewport GLfloat -> Camera -> Uniforms GeoGlobalUniforms
geoUniforms vp cam = 
    let projM = cameraProjectionMatrix cam vp
        viewM = (fmap . fmap) realToFrac (cam^.cameraHandle.to camMatrix)
        vpM   = projM !*! viewM
    in projectionMatrix =: projM <+> 
       viewMatrix       =: viewM <+>
       vpMatrix         =: vpM

entityUniforms :: RenderEntity a -> Uniforms GeoLocalUniforms
entityUniforms ent =
    let trans        = ent^.entityTransformation
        scaleM       = kronecker . point $ trans^.transScale
        transM       = mkTransformation (trans^.transOrientation) (trans^.transPosition)
        modelM       = transM !*! scaleM
        -- TODO rethink the normal matrix here
        normalM      = (adjoint <$> (inv33 . fromTransformation $ modelM) <|> Just eye3) ^?!_Just
    in modelMatrix  =: modelM <+>
       normalMatrix =: normalM


--{--
--   to screen
----}


screenUniforms :: Viewport Float -> Uniforms ScrGlobalUniforms
screenUniforms vp =
    let projM = (fmap . fmap) realToFrac $ cameraProjectionMatrix (Camera2D fpsCamera $ CameraPlanes 0 10) vp :: M44 GLfloat
    in projectionMatrix =: projM <+>
       screenTex        =: 0

viewportUniforms :: RenderEntity a -> Uniforms ScrLocalUniforms
viewportUniforms screenEnt =
    -- our screen has it's origin (0/0) at the top left corner (y-Axis is flipped)
    -- we need to flip our screen object upside down with the object origin point at bottom left to keep the u/v coords reasonable
    let trans        = screenEnt^.entityTransformation 
                            & transPosition._xy    +~ ((0.5) * screenEnt^.entityTransformation.transScale._xy)
                            & transScale._y        *~ (-1)
        scaleM       = kronecker . point $ trans^.transScale
        transM       = mkTransformation (trans^.transOrientation) (trans^.transPosition)
        modelM       = transM !*! scaleM
    in modelMatrix =: modelM

