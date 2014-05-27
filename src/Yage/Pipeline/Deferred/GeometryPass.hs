{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE NamedFieldPuns     #-}
module Yage.Pipeline.Deferred.GeometryPass where

import Yage.Prelude
import Yage.Lens
import Yage.Math

import Yage.Geometry as Geometry
import Yage.Uniforms as Uniforms
import Yage.Camera
import Yage.Scene
import Yage.Material

import Yage.Rendering

import qualified Graphics.Rendering.OpenGL as GL


--type GeoGlobalUniforms = PerspectiveUniforms ++ [YZFarPlane, YAlbedoTex, YNormalTex]
--type AlbedoMaterialU   = YMaterial "AlbedoColor" "AlbedoTexture"
--type NormalMaterialU   = YMaterial "NormalColor" "NormalTexture"

type GeoPerFrameUni    = PerspectiveUniforms ++ '[ YZFarPlane ]
type GeoPerFrameTex    = '[]
type GeoPerFrame       = ShaderData GeoPerFrameUni GeoPerFrameTex


type GeoPerEntityUni   = [ YModelMatrix, YNormalMatrix, YAlbedoColor, YNormalColor ]
type GeoPerEntityTex   = [ YAlbedoTex, YNormalTex ]
type GeoPerEntity      = ShaderData GeoPerEntityUni GeoPerEntityTex

type GeoVertex         = P3TX2NT3

data GeoPassChannels = GeoPassChannels
    { gAlbedoChannel :: Texture
    , gNormalChannel :: Texture
    , gDepthChannel  :: Texture
    }

data GeoMaterial mat = GeoMaterial
    { _albedoMaterial :: mat
    , _normalMaterial :: mat
    } deriving ( Functor, Foldable, Traversable )

makeLenses ''GeoMaterial


-- | Base type for a GeoEntity
type GeoEntityT mesh mat = Entity (mesh GeoVertex) (GeoMaterial mat)
-- | Drawable entity with loaded resources
type GeoEntityDraw = GeoEntityT Mesh RenderMaterial
-- | Resource descripte entity
type GeoEntityRes  = GeoEntityT MeshResource ResourceMaterial

type GeometryPass = PassDescr
                        GeoPassChannels 
                        GeoPerFrame 
                        GeoPerEntity
                        GeoVertex


type GeoPassScene env = Scene GeoEntityDraw env

geoPass :: ViewportI -> GeoPassScene env -> GeometryPass
geoPass viewport scene = PassDescr
    { passTarget         = geoTarget
    , passShader         = ShaderResource "res/glsl/pass/geoPass.vert" "res/glsl/pass/geoPass.frag"
    , passPerFrameData   = geoShaderData
    , passPreRendering   = io $ do
        GL.viewport     GL.$= toGLViewport viewport
        GL.clearColor   GL.$= GL.Color4 0 0 0 0
        
        GL.depthFunc    GL.$= Just GL.Less
        GL.depthMask    GL.$= GL.Enabled
        
        GL.blend        GL.$= GL.Disabled

        GL.cullFace     GL.$= Just GL.Back
        GL.frontFace    GL.$= GL.CCW
        GL.polygonMode  GL.$= (GL.Fill, GL.Fill)

        -- GL.polygonMode  GL.$= (GL.Line, GL.Line)
        GL.clear        [ GL.ColorBuffer, GL.DepthBuffer ]
    , passPostRendering  = return ()
    }

    where
    sceneCam        = scene^.sceneCamera
    glvp            = fromIntegral <$> viewport
    baseSpec        = mkTextureSpec' (glvp^.vpSize) GL.RGBA
    depthSpec       = mkTextureSpec' (glvp^.vpSize) GL.DepthComponent
    
    geoTarget       = RenderTarget "geo-fbo" $
                        GeoPassChannels 
                           { gAlbedoChannel = Texture "gbuffer-albedo" $ TextureBuffer GL.Texture2D baseSpec
                           , gNormalChannel = Texture "gbuffer-normal" $ TextureBuffer GL.Texture2D baseSpec
                           , gDepthChannel  = Texture "gbuffer-depth"  $ TextureBuffer GL.Texture2D depthSpec
                           }
    
    geoShaderData   :: GeoPerFrame 
    geoShaderData   =
        let zfar    = - (realToFrac $ sceneCam^.cameraPlanes.camZFar)
            uniform = perspectiveUniforms glvp sceneCam  <+>
                      zFarPlane        =: zfar
        in ShaderData uniform mempty


instance FramebufferSpec GeoPassChannels RenderTargets where
    fboColors GeoPassChannels{gAlbedoChannel, gNormalChannel} = 
        [ Attachment (ColorAttachment 0) $ TextureTarget GL.Texture2D gAlbedoChannel 0
        , Attachment (ColorAttachment 1) $ TextureTarget GL.Texture2D gNormalChannel 0
        ] 
    
    fboDepth GeoPassChannels{gDepthChannel} = 
        Just $ Attachment DepthAttachment $ TextureTarget GL.Texture2D gDepthChannel 0


toGeoEntity :: GeoEntityDraw -> RenderEntity GeoVertex GeoPerEntity
toGeoEntity ent = toRenderEntity shaderData ent
    where
    shaderData = ShaderData uniforms RNil `append`
                 materialUniforms (ent^.materials.albedoMaterial) `append`
                 materialUniforms (ent^.materials.normalMaterial)
    uniforms =
        let modelM       = calcModelMatrix $ ent^.transformation
            normalM      = (adjoint <$> (inv33 . fromTransformation $ modelM) {--<|> Just eye3--}) ^?!_Just
        in modelMatrix       =: ((fmap . fmap) realToFrac modelM)           <+>
           normalMatrix      =: ((fmap . fmap) realToFrac normalM)


defaultGeoMaterial :: GeoMaterial ResourceMaterial
defaultGeoMaterial = 
    let albedoMat = defaultMaterialSRGB
        normalMat = defaultMaterialSRGB & singleMaterial .~ (TexturePure $ Texture "NORMALDUMMY" $ Texture2D $ zeroNormalDummy TexSRGB8)
    in GeoMaterial albedoMat normalMat


instance Default (GeoMaterial ResourceMaterial) where
    def = defaultGeoMaterial


instance Applicative GeoMaterial where
    pure mat = GeoMaterial mat mat
    GeoMaterial f g <*> GeoMaterial m n = GeoMaterial (f m) (g n)

instance HasResources vert (GeoMaterial ResourceMaterial) (GeoMaterial RenderMaterial) where
    requestResources = mapM requestResources

