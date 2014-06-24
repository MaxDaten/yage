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
import Yage.Viewport
import Yage.Scene
import Yage.Material

import Yage.Rendering

import qualified Graphics.Rendering.OpenGL as GL


type GeoPerFrameUni    = PerspectiveUniforms ++ '[ YZFarPlane ]
type GeoPerFrameTex    = '[]
type GeoPerFrame       = ShaderData GeoPerFrameUni GeoPerFrameTex


type GeoPerEntityUni   = [ YModelMatrix
                         , YNormalMatrix
                         ] 
                         ++ YAlbedoMaterial 
                         ++ YNormalMaterial

type GeoPerEntityTex   = [ YAlbedoTex, YNormalTex ]
type GeoPerEntity      = ShaderData GeoPerEntityUni GeoPerEntityTex

type GeoVertex         = Vertex (Y'P3TX2TN GLfloat)

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


type GeoPassScene env = Scene Camera GeoEntityDraw env

{--
Pass Description
--}

geoPass :: Viewport Int -> Camera -> GeometryPass
geoPass viewport camera = PassDescr
    { passTarget         = geoTarget
    , passShader         = ShaderResource "res/glsl/pass/geoPass.vert" "res/glsl/pass/geoPass.frag"
    , passPerFrameData   = geoShaderData
    , passPreRendering   = io $ do
        GL.viewport     GL.$= (viewport^.glViewport)
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
    glvp            = fromIntegral <$> viewport
    baseSpec        = mkTextureSpec' (viewport^.rectangle.extend) GL.RGBA
    depthSpec       = mkTextureSpec' (viewport^.rectangle.extend) GL.DepthComponent
    
    geoTarget       = RenderTarget "geo-fbo" $
                        GeoPassChannels 
                           { gAlbedoChannel = mkTexture "gbuffer-albedo" $ TextureBuffer GL.Texture2D baseSpec
                           , gNormalChannel = mkTexture "gbuffer-normal" $ TextureBuffer GL.Texture2D baseSpec
                           , gDepthChannel  = mkTexture "gbuffer-depth"  $ TextureBuffer GL.Texture2D depthSpec
                           }
    
    geoShaderData   :: GeoPerFrame 
    geoShaderData   =
        let zfar    = - (realToFrac $ camera^.cameraPlanes.camZFar)
            uniform = perspectiveUniforms glvp camera  <+>
                      zFarPlane        =: zfar
        in ShaderData uniform mempty

{--
Geo Pass Utils
--}

toGeoEntity :: Camera -> GeoEntityDraw -> RenderEntity GeoVertex GeoPerEntity
toGeoEntity camera ent = toRenderEntity shaderData ent
    where
    shaderData = ShaderData uniforms RNil `append`
                 materialUniforms (ent^.materials.albedoMaterial) `append`
                 materialUniforms (ent^.materials.normalMaterial)
    uniforms =
        modelMatrix       =: ( ent^.entityTransformation.transformationMatrix & traverse.traverse %~ realToFrac )           <+>
        normalMatrix      =: ( theNormalMatrix & traverse.traverse %~ realToFrac )

    theNormalMatrix :: M33 Float
    theNormalMatrix = 
        let invCam        = camera & cameraTransformation %~ inverseTransformation
            invViewM      = invCam^.cameraMatrix
            invModelM     = ent^.entityTransformation.to inverseTransformation.transformationMatrix
        in adjoint $ invModelM^.to m44_to_m33 !*! invViewM^.to m44_to_m33


defaultGeoMaterial :: GeoMaterial ResourceMaterial
defaultGeoMaterial =
    let albedoMat = defaultMaterialSRGB
        normalMat = defaultMaterialSRGB & singleMaterial .~ (TexturePure $ mkTexture "NORMALDUMMY" $ Texture2D $ zeroNormalDummy TexSRGB8)
    in GeoMaterial albedoMat normalMat


instance Default (GeoMaterial ResourceMaterial) where
    def = defaultGeoMaterial


instance Applicative GeoMaterial where
    pure mat = GeoMaterial mat mat
    GeoMaterial f g <*> GeoMaterial m n = GeoMaterial (f m) (g n)


instance HasResources vert (GeoMaterial ResourceMaterial) (GeoMaterial RenderMaterial) where
    requestResources = mapM requestResources


instance FramebufferSpec GeoPassChannels RenderTargets where
    fboColors GeoPassChannels{gAlbedoChannel, gNormalChannel} = 
        [ Attachment (ColorAttachment 0) $ TextureTarget GL.Texture2D gAlbedoChannel 0
        , Attachment (ColorAttachment 1) $ TextureTarget GL.Texture2D gNormalChannel 0
        ] 
    
    fboDepth GeoPassChannels{gDepthChannel} = 
        Just $ Attachment DepthAttachment $ TextureTarget GL.Texture2D gDepthChannel 0
