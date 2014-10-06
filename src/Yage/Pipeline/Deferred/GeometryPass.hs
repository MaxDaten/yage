{-# OPTIONS_GHC -fno-warn-orphans    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE QuasiQuotes        #-}
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
import Yage.TH.Shader as GLSL

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


-------------------------------------------------------------------------------
-- | Fragment code
geoFragmentProgramm :: GLSL.ShaderSource FragmentShader
geoFragmentProgramm = [GLSL.yFragment|
#version 410 core

#include "pass/Common.glsl"
#include "pass/GBuffer.glsl"

uniform sampler2D AlbedoTexture;
uniform sampler2D NormalTexture;
uniform sampler2D RoughnessTexture;

uniform vec4 AlbedoColor;
uniform vec4 NormalColor;
uniform float RoughnessIntensity;

in vec3 VertexPos_View;
in vec2 AlbedoST;
in vec2 NormalST;
in vec2 RoughnessST;
smooth in mat3 TangentToView;

// Red Green Blue Depth
layout (location = 0) out vec4 OutAlbedo;
layout (location = 1) out vec4 OutNormal;
// layout (location = 2) out vec3 specularOut;
// layout (location = 3) out vec3 glossyOut;


vec4 GetAlbedoColor()
{
    return AlbedoColor * texture( AlbedoTexture, AlbedoST );
}

float GetRoughness()
{
    return RoughnessIntensity * texture( RoughnessTexture, RoughnessST ).r;
}


void main()
{
    OutAlbedo.rgb   = GetAlbedoColor().rgb;
    OutAlbedo.a     = GetRoughness();

    vec3 texNormal = NormalColor.rgb * DecodeNormal( texture( NormalTexture, NormalST ).rg );
    texNormal      = normalize( texNormal );
    OutNormal.rg   = EncodeNormal( TangentToView * texNormal ).rg;
}
|]
-- ============================================================================


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
                                       , geoFragmentProgramm^.shaderSource
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
    depthSpec       = mkTextureSpec (viewport^.rectangle.extend) GL.UnsignedByte GL.DepthComponent GL.DepthComponent24

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

        -- let viewRotM   = fromQuaternion $ camera^.cameraOrientation
        --     invViewM   = viewRotM

        --     modelRotM  = fromQuaternion $ ent^.entityOrientation
        --     invModelM  = modelRotM !*! (kronecker $ negate $ ent^.entityScale)
        -- in adjoint $ invModelM !*! invViewM

        -- let -- invCam        = camera & cameraTransformation %~ inverseTransformation
        --     invViewM      = camera^.cameraMatrix
        --     entTransform  = ent^.entityTransformation
        --     invModelM     = (entTransform & transScale %~ negate)^.transformationMatrix
        -- in adjoint $ invViewM^.to m44_to_m33 !*! invModelM^.to m44_to_m33


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
