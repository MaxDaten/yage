{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
module Yage.Pipeline.Deferred
    ( module Yage.Pipeline.Deferred
    , module Spec
    , module ResourceLoader
    , module Light
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

import Yage.Pipeline.Deferred.Spec              as Spec
import Yage.Pipeline.Deferred.ResourceLoader    as ResourceLoader
import Yage.Pipeline.Deferred.Light             as Light



yDeferredLighting :: ViewportI -> SScene GeoVertex LitVertex -> RenderSystem ()
yDeferredLighting viewport scene =
    let size            = floor . (*) (viewport^.vpFactor) . fromIntegral <$> viewport^.vpSize
        albedoT         = TextureBuffer "gbuffer-albedo" Texture2D $ GLBufferSpec RGBA8 size
        normalT         = TextureBuffer "gbuffer-normal" Texture2D $ GLBufferSpec RGBA8 size
        --specularTex     = TextureBuffer "gbuffer-specul" Texture2D $ GLBufferSpec RGB8 size
        --glossyTex       = TextureBuffer "gbuffer-glossy" Texture2D $ GLBufferSpec RGB8 size
        depthBuff       = TextureBuffer "gbuffer-depth" Texture2D  $ GLBufferSpec DepthComponent32 size

        geoPass         = PassDescr
                            { passFBSpec         = CustomFramebuffer ( "geo-fbo"
                                                 , colorAttachment (TextureTarget Texture2D albedoT      0)
                                                 <> colorAttachment (TextureTarget Texture2D normalT     0)
                                                 -- <> colorAttachment (TextureTarget Texture2D specularTex 0)
                                                 -- <> colorAttachment (TextureTarget Texture2D glossyTex   0)
                                                 <> depthAttachment (TextureTarget Texture2D depthBuff 0)
                                                 -- <> depthStencilAttachment (TextureTarget Texture2D depthStencilTex 0)
                                                 )
                            , passShader         = ShaderResource "res/glsl/pass/geoPass.vert" "res/glsl/pass/geoPass.frag"
                            , passGlobalUniforms = geoUniforms (fromIntegral <$> viewport) (scene^.sceneCamera)
                            , passEntityUniforms = entityUniforms
                            , passGlobalTextures = []
                            , passPreRendering   = io $ do
                                GL.viewport     GL.$= toGLViewport viewport
                                GL.clearColor   GL.$= GL.Color4 0 0 0 0
                                
                                GL.depthFunc    GL.$= Just GL.Less
                                GL.depthMask    GL.$= GL.Enabled
                                
                                GL.blend        GL.$= GL.Disabled

                                GL.cullFace     GL.$= Just GL.Back
                                GL.frontFace    GL.$= GL.CCW
                                GL.polygonMode  GL.$= (GL.Fill, GL.Fill)

                                --GL.polygonMode  GL.$= (GL.Line, GL.Line)
                                GL.clear        [ GL.ColorBuffer, GL.DepthBuffer ]
                            , passPostRendering  = return ()
                            }


        lightTex        = TextureBuffer "lbuffer" Texture2D $ GLBufferSpec RGB8 size
        
        lightPass       = PassDescr
                            { passFBSpec         = CustomFramebuffer ( "light-fbo"
                                                 , colorAttachment (TextureTarget Texture2D lightTex   0)
                                                 -- <> depthAttachment (TextureTarget Texture2D depthBuff 0) -- depth buffer is read only (see passPreRendering)
                                                 )
                            , passShader         = ShaderResource "res/glsl/pass/lightPass.vert" "res/glsl/pass/lightPass.frag"
                            , passGlobalUniforms = lightPassUniforms (fromIntegral <$> viewport) (scene^.sceneCamera)
                            , passEntityUniforms = lightUniforms (fromIntegral <$> viewport) (scene^.sceneCamera)
                            , passGlobalTextures = [ TextureDefinition (0, "albedo") albedoT
                                                   , TextureDefinition (1, "normal") normalT
                                                   , TextureDefinition (2, "depth") depthBuff
                                                   ]
                            , passPreRendering   = io $ do
                                GL.viewport     GL.$= toGLViewport viewport
                                GL.clearColor   GL.$= GL.Color4 0 0 0 0 --- ambient?
                                
                                GL.depthFunc    GL.$= Just GL.Never      --- disable func add
                                GL.depthMask    GL.$= GL.Disabled       -- writing to depth is disabled
                                
                                GL.blend        GL.$= GL.Enabled            --- could reject background frags!
                                GL.blendEquation GL.$= GL.FuncAdd
                                GL.blendFunc    GL.$= (GL.One, GL.One) -- addition each time

                                GL.cullFace     GL.$= Just GL.Back
                                GL.frontFace    GL.$= GL.CCW
                                GL.polygonMode  GL.$= (GL.Fill, GL.Fill)
                                
                                GL.clear        [ GL.ColorBuffer ]
                                -- print "setup correct lighting params"
                            , passPostRendering  = return ()
                            }
        screenPass      = PassDescr
                            { passFBSpec         = DefaultFramebuffer
                            , passShader         = ShaderResource "res/glsl/pass/screenPass.vert" "res/glsl/pass/screenPass.frag"
                            , passGlobalUniforms = screenUniforms (fromIntegral <$> viewport) 
                            , passEntityUniforms = viewportUniforms
                            , passGlobalTextures = [TextureDefinition (0, "albedo") lightTex]
                            , passPreRendering   = io $ do
                                -- our 0/0 is top left (y-Axis is flipped)
                                GL.viewport     GL.$= toGLViewport viewport
                                GL.clearColor   GL.$= GL.Color4 1 1 1 0
                                
                                GL.depthFunc    GL.$= Nothing    -- TODO to init
                                GL.depthMask    GL.$= GL.Disabled
                                
                                GL.blend        GL.$= GL.Disabled

                                GL.cullFace     GL.$= Just GL.Back
                                GL.frontFace    GL.$= GL.CCW
                                GL.polygonMode  GL.$= (GL.Fill, GL.Fill)
                                
                                GL.clear        [ GL.ColorBuffer, GL.DepthBuffer ]
                            , passPostRendering  = return ()
                            }
    in do
        runRenderPass geoPass    (scene^.sceneEntities)
        runRenderPass lightPass  (scene^.sceneLights)
        runRenderPass screenPass [(Screen viewport)]


geoUniforms :: Viewport GLfloat -> Camera -> Uniforms GeoGlobalUniforms
geoUniforms vp cam =
    let projM = cameraProjectionMatrix cam vp
        viewM = (fmap . fmap) realToFrac (cam^.cameraHandle.to camMatrix)
        vpM   = projM !*! viewM
        zfar  = (realToFrac $ - cam^.cameraPlanes.camZFar)
    in viewMatrix       =: viewM <+>
       vpMatrix         =: vpM   <+>
       zFarPlane        =: zfar  <+>
       albedoTex        =: 0     <+>
       normalTex        =: 1

entityUniforms :: SceneEntity a -> Uniforms GeoLocalUniforms
entityUniforms ent =
    let trans        = ent^.transformation
        scaleM       = kronecker . point $ trans^.transScale
        transM       = mkTransformation (trans^.transOrientation) (trans^.transPosition)
        modelM       = transM !*! scaleM
        -- TODO rethink the normal matrix here
        normalM      = (adjoint <$> (inv33 . fromTransformation $ modelM) {--<|> Just eye3--}) ^?!_Just
    in modelMatrix  =: ((fmap . fmap) realToFrac modelM) <+>
       normalMatrix =: ((fmap . fmap) realToFrac normalM)

--{--
--   to screen
----}


newtype Screen = Screen ViewportI

instance Renderable Screen ScrVertex where
    renderDefinition _ = 
        let q    = (vertices . triangles $ addQuadTex $ quad 1) :: [Vertex ScrVertex]
            mesh = makeSimpleTriMesh "YAGE:SCREEN" q
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


addQuadTex :: Primitive (Vertex P3) -> Primitive (Vertex P3TX2)
addQuadTex (Quad (Face a b c d)) = Quad $ Face  (a <+> texture2 =: (V2 0 1))
                                                (b <+> texture2 =: (V2 0 0))
                                                (c <+> texture2 =: (V2 1 0))
                                                (d <+> texture2 =: (V2 1 1))
addQuadTex _ = error "not a quad"


--{--
--   light pass
----}

lightPassUniforms :: Viewport Int -> Camera -> Uniforms LitGlobalUniforms
lightPassUniforms vp cam =
    let projM        = cameraProjectionMatrix cam (fromIntegral <$> vp)
        viewM        = cam^.cameraHandle.to camMatrix
        vpM          = projM !*! viewM
        zNearFar     = realToFrac <$> V2 (-cam^.cameraPlanes^.camZNear) (-cam^.cameraPlanes^.camZFar)
    in
    viewMatrix       =: ((fmap . fmap) realToFrac viewM) <+>
    vpMatrix         =: ((fmap . fmap) realToFrac vpM) <+>
    viewportDim      =: (fromIntegral <$> vp^.vpSize) <+>
    zNearFarPlane    =: zNearFar <+>
    albedoTex        =: 0        <+>
    normalTex        =: 1        <+>
    depthTex         =: 2

lightUniforms :: Viewport Float -> Camera -> SceneLight a -> Uniforms LitLocalUniforms
lightUniforms _vp _cam light = 
    let trans        = light^.lightTransformation
        scaleM       = kronecker . point $ trans^.transScale
        transM       = mkTransformation (trans^.transOrientation) (trans^.transPosition)
        modelM       = transM !*! scaleM
        lightProps   = light^.lightProperties
    in 
    modelMatrix =: ((fmap.fmap) realToFrac modelM)  <+>
    lightAttributes ( lightProps )


