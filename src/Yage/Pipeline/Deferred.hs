{-# LANGUAGE OverloadedStrings          #-}
module Yage.Pipeline.Deferred where

import Yage.Prelude
import Yage.Math

import Yage.Rendering

import qualified Graphics.Rendering.OpenGL as GL

yDeferredLightingDescr :: ViewportI -> DeferredLightingDescr
yDeferredLightingDescr viewport =
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
                            , passGlobalUniforms = sceneUniforms (fromIntegral <$> viewport)
                            , passEntityUniforms = entityUniforms
                            , passGlobalTextures = []
                            , passPreRendering   = io $ do
                                GL.viewport     GL.$= toGLViewport viewport
                                GL.clearColor   GL.$= GL.Color4 0 0 1 0
                                GL.depthFunc    GL.$= Just GL.Less
                                GL.depthMask    GL.$= GL.Enabled
                                GL.cullFace     GL.$= Just GL.Back
                                GL.frontFace    GL.$= GL.CCW
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
                            , passGlobalTextures = [TextureDefinition (0, "albedo") albedoTex]
                            , passPreRendering   = io $ do
                                -- our 0/0 is top left (y-Axis is flipped)
                                GL.viewport     GL.$= toGLViewport viewport
                                GL.clearColor   GL.$= GL.Color4 1 1 1 0
                                GL.depthFunc    GL.$= Nothing    -- TODO to init
                                GL.cullFace     GL.$= Just GL.Back
                                GL.frontFace    GL.$= GL.CCW
                                GL.clear        [ GL.ColorBuffer, GL.DepthBuffer ]
                                --GL.polygonMode  GL.$= (GL.Line, GL.Line)

                            , passPostRendering  = return ()
                            }
    in
    DeferredLightingDescr
    { dfGeoPassDescr        = geoPass
    , dfLightingPassDescr   = undefined -- lightPass
    , dfFinalScreen         = screenPass
    }


sceneUniforms :: Viewport Float -> RenderScene -> ShaderDefinition ()
sceneUniforms vp scene =
    let projM = cameraProjectionMatrix (scene^.sceneCamera) vp :: M44 Float
        viewM = scene^.sceneCamera.cameraHandle.to camMatrix   :: M44 Float
        vpM   = projM !*! viewM
    in do
        "projection_matrix" != projM
        "view_matrix"       != viewM
        "vp_matrix"         != vpM

entityUniforms :: RenderEntity -> ShaderDefinition ()
entityUniforms ent = 
    let trans        = ent^.entityTransformation
        scaleM       = kronecker . point $ trans^.transScale
        transM       = mkTransformation (trans^.transOrientation) (trans^.transPosition)
        modelM       = transM !*! scaleM
        -- TODO rethink the normal matrix here
        normalM      = (adjoint <$> (inv33 . fromTransformation $ modelM) <|> Just eye3) ^?!_Just
    in do
        "model_matrix"      != modelM
        "normal_matrix"     != normalM


{--
   to screen
--}


screenUniforms :: Viewport Float -> RenderScene -> ShaderDefinition ()
screenUniforms vp _ =
    let projM = cameraProjectionMatrix (Camera2D fpsCamera $ CameraPlanes 0 10) vp :: M44 Float
    in do
        "projection_matrix" != projM
        "texture"           != (0 :: GLint)

viewportUniforms :: RenderEntity -> ShaderDefinition ()
viewportUniforms screen = 
    -- our screen has it's origin (0/0) at the top left corner (y-Axis is flipped)
    -- we need to flip our screen object upside down with the object origin point at bottom left to keep the u/v coords reasonable
    let trans        = screen^.entityTransformation & transPosition._xy    +~ ((0.5) * screen^.entityTransformation.transScale._xy)
                                                    & transScale._y        *~ (-1)
        scaleM       = kronecker . point $ trans^.transScale
        transM       = mkTransformation (trans^.transOrientation) (trans^.transPosition)
        modelM       = transM !*! scaleM
        -- TODO rethink the normal matrix here
    in do
        "model_matrix"      != modelM

{--

        getProjection :: Camera -> RenderTarget -> M44 Float
        getProjection (Camera3D _ fov) target =
            


    createUniformDef ent = 
        



--}

{--

mkSceneRenderer :: RenderScene -> Maybe (String, FramebufferSpec TextureResource RenderbufferResource) -> RenderSystem (Renderer ())
mkSceneRenderer scene mfbo = do
    (renderView, viewEntities) <- prepareResources
    Just framebuffer           <- case mfbo of
                                    Just (fboId, _) -> use $ compiledFBOs.at fboId
                                    Nothing         -> return $ Just defaultFramebuffer
    return $ renderFrame renderView viewEntities framebuffer

    where

        prepareResources :: RenderSystem (RenderView, [ViewEntity])
        prepareResources = do 
            renderSettings  <- ask
            let renderTarget        = renderSettings^.reRenderTarget

                viewMatrix          = scene^.sceneCamera.cameraHandle.to camMatrix
                projMatrix          = getProjection (scene^.sceneCamera) renderTarget
                renderView          = RenderView viewMatrix projMatrix
                entities            = map toRenderEntity $ scene^.sceneEntities
                resourceables       = Resourceables entities mfbo

--------- SUCKAGE!!!!!!!!!!!!!!!!!------------------
            loadedRes <- runResourceManager resourceables =<< get
            put loadedRes
            return (renderView, map (toViewEntity renderView loadedRes) entities)

        getProjection :: Camera -> RenderTarget -> M44 Float
        getProjection (Camera3D _ fov) target =
            let V2 w h      = fromIntegral <$> target^.targetSize
                (n, f)      = double2Float <$$> (target^.targetZNear, target^.targetZFar)
                aspect      = (w/h)
            in projectionMatrix fov aspect n f -- TODO : move zfar/znear

        getProjection (Camera2D _) target =
            let V2 w h      = fromIntegral <$> target^.targetSize
                V2 x y      = fromIntegral <$> target^.targetXY
                (n, f)      = double2Float <$$> (target^.targetZNear, target^.targetZFar)
            in orthographicMatrix x w y h n f

---------------------------------------------------------------------------------------------------



toViewEntity :: RenderView -> RenderResources -> RenderEntity -> ViewEntity
toViewEntity rview@RenderView{..} RenderResources{..} ent =
    let scaleM       = kronecker . point $ ent^.entityTransformation.transScale
        transM       = mkTransformation (ent^.entityTransformation.transOrientation) (ent^.entityTransformation.transPosition)
        modelM       = transM !*! scaleM
        -- TODO rethink the normal matrix here
        normalM      = (adjoint <$> (inv33 . fromTransformation $ modelM) <|> Just eye3) ^?!_Just
    in ViewEntity
        { _vdMVPMatrix         = _rvProjectionMatrix !*! _rvViewMatrix !*! modelM
        , _vdModelViewMatrix   = _rvViewMatrix !*! modelM
        , _vdModelMatrix       = modelM
        , _vdNormalMatrix      = normalM
        , _vdRenderData        = getRenderData $ ent^.entityRenderDef
        , _vdUniformDef        = (ent^.entityRenderDef.rdefProgram._2, uniformEnv)
        }
    
    where
        
        getRenderData renderDef =
            let rData = renderDef^.rdefData
                rProg = renderDef^.rdefProgram^._1
                rTexs  = renderDef^.rdefTextures
            in RenderData
                { _vao           = _loadedVertexArrays^.at (rData, rProg) ^?!_Just
                , _shaderProgram = _loadedShaders^.at rProg ^?!_Just
                , _texObjs       = map makeTexObj rTexs
                , _elementCount  = meshTriangleCount rData
                , _drawMode      = renderDef^.rdefMode
                }
        
        makeTexObj tex =
            let obj = _loadedTextures^.at (tex^.texResource) ^?!_Just
                ch  = tex^.texChannel & _1 %~ fromIntegral
            in (obj, ch)
        
        uniformEnv = ShaderEnv
            { _seProgram           = _loadedShaders^.at (ent^.entityRenderDef^.rdefProgram._1) ^?!_Just
            , _seViewDef           = undefined -- TODO init is currently in renderer (awkward!!)
            , _seView              = rview
            }

--}
