{-# OPTIONS_GHC -fno-warn-orphans    #-}
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
import Yage.TH.Shader

import Yage.Rendering

import qualified Graphics.Rendering.OpenGL as GL
import Yage.Pipeline.Deferred.Common


type GeoPerFrameUni    = PerspectiveUniforms

type GeoPerEntityUni   = [ YModelMatrix
                         , YNormalMatrix
                         ]
                         ++ YAlbedoMaterial
                         ++ YNormalMaterial
                         ++ YRoughnessMaterial

type GeoUniforms       = GeoPerFrameUni ++ GeoPerEntityUni
type GeoTextures       = [ YAlbedoTex, YNormalTex, YRoughnessTex ]
type GeoVertex         = Vertex (Y'P3TX2TN GLfloat)
type GeoShader         = Shader GeoUniforms GeoTextures GeoVertex

data GeoPassChannelsF t = GeoPassChannelsF
    { gAlbedoChannel :: t
    , gNormalChannel :: t
    , gDepthChannel  :: t
    } deriving ( Functor, Foldable, Traversable )
type GeoPassChannels = GeoPassChannelsF Texture

data GeoMaterial mat = GeoMaterial
    { _albedoMaterial    :: IMaterial MaterialColorAlpha mat
    , _normalMaterial    :: IMaterial MaterialColorAlpha mat
    , _roughnessMaterial :: IMaterial Double mat
    } deriving ( Functor, Foldable, Traversable )

makeLenses ''GeoMaterial


-- | Base type for a GeoEntity
type GeoEntityT mesh mat = Entity (mesh GeoVertex) (GeoMaterial mat)
-- | Drawable entity with loaded resources
type GeoEntityDraw = GeoEntityT Mesh Texture
-- | Resource descripte entity
type GeoEntityRes  = GeoEntityT MeshResource TextureResource

type GeometryPass = YageDeferredPass GeoPassChannels GeoShader



type GeoPassScene env = Scene Camera GeoEntityDraw env

{--
Pass Description
--}

geoPass :: Viewport Int -> GeometryPass
geoPass viewport =
    let thePass     = passPreset geoTarget (viewport^.rectangle) (ShaderUnit shaderProg)
        clearBuffer = (thePass^.passPreRendering) >> io (GL.clear [ GL.DepthBuffer ])
    in thePass & passPreRendering .~ clearBuffer

    where

    shaderProg = ShaderProgramUnit
                 { _shaderName       = "GeometryPass.hs"
                 , _shaderSources    = [ $(vertexFile "res/glsl/pass/geoPass.vert")^.shaderSource
                                       , $(fragmentFile "res/glsl/pass/geoPass.frag")^.shaderSource
                                       ]
                 }

    geoTarget  = RenderTarget "geo-fbo" $
                    GeoPassChannelsF
                        { gAlbedoChannel = mkTargetTexture "gbuffer-albedo" baseSpec
                        , gNormalChannel = mkTargetTexture "gbuffer-normal" normSpec
                        , gDepthChannel  = mkTargetTexture "gbuffer-depth"  depthSpec
                        }

    baseSpec        = mkTextureSpec' (viewport^.rectangle.extend) GL.RGBA
    normSpec        = mkTextureSpec (viewport^.rectangle.extend) GL.UnsignedByte GL.RG GL.RG16
    depthSpec       = mkTextureSpec' (viewport^.rectangle.extend) GL.DepthComponent

{--
Geo Pass Utils
--}

geoFrameData :: Viewport Int -> Camera -> ShaderData GeoPerFrameUni '[]
geoFrameData viewport camera =
    let uniform = perspectiveUniforms (fromIntegral <$> viewport) camera
    in ShaderData uniform mempty


toGeoEntity :: Camera -> GeoEntityDraw -> RenderEntity GeoVertex (ShaderData GeoPerEntityUni GeoTextures)
toGeoEntity camera ent = toRenderEntity shaderData ent
    where
    shaderData = ShaderData uniforms RNil `append`
                 materialUniformsColor (ent^.materials.albedoMaterial) `append`
                 materialUniformsColor (ent^.materials.normalMaterial) `append`
                 materialUniformsIntensity (ent^.materials.roughnessMaterial)
    uniforms =
        modelMatrix       =: ( ent^.entityTransformation.transformationMatrix & traverse.traverse %~ realToFrac )           <+>
        normalMatrix      =: ( theNormalMatrix & traverse.traverse %~ realToFrac )

    theNormalMatrix :: M33 Float
    theNormalMatrix =
        let invCam        = camera & cameraTransformation %~ inverseTransformation
            invViewM      = invCam^.cameraMatrix
            invModelM     = ent^.entityTransformation.to inverseTransformation.transformationMatrix
        in adjoint $ invModelM^.to m44_to_m33 !*! invViewM^.to m44_to_m33


defaultGeoMaterial :: GeoMaterial TextureResource
defaultGeoMaterial =
    let albedoMat    = defaultMaterialSRGB
        normalMat    = defaultMaterialSRGB & singleMaterial .~ (TexturePure $ mkTexture "NORMALDUMMY" $ Texture2D $ zeroNormalDummy TexSRGB8)
        roughnessMat = mkMaterialF 1.0 $ pure $ TexturePure $ mkTexture "ROUGHDUMMY" $ Texture2D $ zeroNormalDummy TexY8
    in GeoMaterial albedoMat normalMat roughnessMat


instance Default (GeoMaterial TextureResource) where
    def = defaultGeoMaterial



instance HasResources vert (GeoMaterial TextureResource) (GeoMaterial Texture) where
    requestResources = mapM requestResources


instance FramebufferSpec GeoPassChannels RenderTargets where
    fboColors GeoPassChannelsF{gAlbedoChannel, gNormalChannel} =
        [ Attachment (ColorAttachment 0) $ TextureTarget GL.Texture2D gAlbedoChannel 0
        , Attachment (ColorAttachment 1) $ TextureTarget GL.Texture2D gNormalChannel 0
        ]

    fboDepth GeoPassChannelsF{gDepthChannel} =
        Just $ Attachment DepthAttachment $ TextureTarget GL.Texture2D gDepthChannel 0
