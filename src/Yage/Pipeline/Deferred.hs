{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
module Yage.Pipeline.Deferred
    ( module Yage.Pipeline.Deferred
    , module Spec
    , module ResourceLoader
    ) where

import Yage.Prelude
import Yage.Lens
import Yage.Math
import Yage.Geometry
import Yage.Primitives


import Yage.Rendering hiding (P3, P3N3, P3T2)
import Yage.Rendering.Viewport
import Yage.Rendering.Transformation

import Yage.Scene
import Yage.Camera
import qualified Graphics.Rendering.OpenGL as GL

import Yage.Pipeline.Deferred.Spec as Spec
import Yage.Pipeline.Deferred.ResourceLoader as ResourceLoader



yDeferredLighting :: ViewportI -> SScene GeoVertex -> RenderSystem ()
yDeferredLighting viewport scene =
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
    in do
        runRenderPass geoPass (scene^.sceneEntities)
        runRenderPass screenPass [(Screen viewport)]


geoUniforms :: Viewport GLfloat -> Camera -> Uniforms GeoGlobalUniforms
geoUniforms vp cam = 
    let projM = cameraProjectionMatrix cam vp
        viewM = (fmap . fmap) realToFrac (cam^.cameraHandle.to camMatrix)
        vpM   = projM !*! viewM
    in projectionMatrix =: projM <+> 
       viewMatrix       =: viewM <+>
       vpMatrix         =: vpM

entityUniforms :: SceneEntity a -> Uniforms GeoLocalUniforms
entityUniforms ent =
    let trans        = ent^.transformation
        scaleM       = kronecker . point $ trans^.transScale
        transM       = mkTransformation (trans^.transOrientation) (trans^.transPosition)
        modelM       = transM !*! scaleM
        -- TODO rethink the normal matrix here
        normalM      = (adjoint <$> (inv33 . fromTransformation $ modelM) <|> Just eye3) ^?!_Just
    in modelMatrix  =: ((fmap . fmap) realToFrac modelM) <+>
       normalMatrix =: ((fmap . fmap) realToFrac normalM)


--{--
--   to screen
----}


newtype Screen = Screen ViewportI

instance Renderable Screen ScrVertex where
    renderDefinition _ = 
        let q    = (vertices . triangles $ addQuadTex $ quad 1) :: [Vertex ScrVertex]
            mesh = makeMesh "SCREEN" q
        in RenderEntity mesh Triangles []


screenUniforms :: Viewport Float -> Uniforms ScrGlobalUniforms
screenUniforms vp =
    let projM = (fmap . fmap) realToFrac $ cameraProjectionMatrix (Camera2D fpsCamera $ CameraPlanes 0 10) vp :: M44 GLfloat
    in projectionMatrix =: projM <+>
       screenTex        =: 0

viewportUniforms :: Screen -> Uniforms ScrLocalUniforms
viewportUniforms (Screen vp) =
    -- our screen has it's origin (0/0) at the top left corner (y-Axis is flipped)
    -- we need to flip our screen object upside down with the object origin point at bottom left to keep the u/v coords reasonable
    let dim           = realToFrac <$> vp^.vpSize
        trans         = idTransformation & transPosition._xy .~ 0.5 * dim
                                         & transScale        .~ V3 ( dim^._x ) (- (dim^._y) ) (1)
        --trans        = screenEnt^.transformation 
        --                    & transPosition._xy    +~ ((0.5) * screenEnt^.transformation.transScale._xy)
        --                    & transScale._y        *~ (-1)
        scaleM       = kronecker . point $ trans^.transScale
        transM       = mkTransformation (trans^.transOrientation) (trans^.transPosition)
        modelM       = transM !*! scaleM
    in modelMatrix =: modelM


addQuadTex :: Primitive (Vertex P3) -> Primitive (Vertex P3T2)
addQuadTex (Quad (Face a b c d)) = Quad $ Face  (a <+> texture2 =: (V2 0 1))
                                                (b <+> texture2 =: (V2 0 0))
                                                (c <+> texture2 =: (V2 1 0))
                                                (d <+> texture2 =: (V2 1 1))
addQuadTex _ = error "not a quad"

