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
import qualified        Yage.Formats.Ygm                 as YGM
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

data GeoMaterial = GeoMaterial
    { _albedoMaterial    :: Material MaterialColorAlpha
    , _normalMaterial    :: Material MaterialColorAlpha
    , _roughnessMaterial :: Material Double
    }

makeLenses ''GeoMaterial


-- | Base type for a GeoEntity
type GeoEntity = Entity (Mesh GeoVertex) GeoMaterial
type GeometryPass = YageDeferredPass GeoPassChannels GeoShader



type GeoPassScene env = Scene Camera GeoEntity env


-------------------------------------------------------------------------------
-- | Fragment code
geoFragmentProgram :: GLSL.ShaderSource FragmentShader
geoFragmentProgram = [GLSL.yFragment|
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
layout (location = 1) out vec2 OutNormal;
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

    vec3 texNormal = texture( NormalTexture, NormalST ).rgb;
    vec3 Normal    = NormalColor.rgb * DecodeTextureNormal( texNormal );
    OutNormal.rg   = EncodeNormalXY ( normalize ( TangentToView * Normal ) );
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
                                       , geoFragmentProgram^.shaderSource
                                       ]
                 }

    geoTarget  = RenderTarget "geo-fbo" $
                    GeoPassChannelsF
                        { gAlbedoChannel = mkTargetTexture "gbuffer-albedo" baseSpec
                        , gNormalChannel = mkTargetTexture "gbuffer-normal" normSpec
                        , gDepthChannel  = mkTargetTexture "gbuffer-depth"  depthSpec
                        }

    baseSpec        = mkTextureSpec' (viewport^.rectangle.extend) GL.RGBA
    normSpec        = mkTextureSpec (viewport^.rectangle.extend) GL.HalfFloat GL.RG GL.RG16F
    depthSpec       = mkTextureSpec (viewport^.rectangle.extend) GL.UnsignedByte GL.DepthComponent GL.DepthComponent24

{--
Geo Pass Utils
--}

geoFrameData :: Viewport Int -> Camera -> ShaderData GeoPerFrameUni '[]
geoFrameData viewport camera =
    let uniform = perspectiveUniforms (fromIntegral <$> viewport) camera
    in ShaderData uniform mempty


toGeoEntity :: Camera -> GeoEntity -> RenderEntity GeoVertex (ShaderData GeoPerEntityUni GeoTextures)
toGeoEntity camera ent = toRenderEntity shaderData ent
    where
    shaderData = ShaderData uniforms RNil `append`
                 materialUniformsColor (ent^.materials.albedoMaterial) `append`
                 materialUniformsColor (ent^.materials.normalMaterial) `append`
                 materialUniformsIntensity (ent^.materials.roughnessMaterial)
    uniforms =
        modelMatrix       =: ( ent^.entityTransformation.transformationMatrix & traverse.traverse %~ realToFrac )           <+>
        normalMatrix      =: ( theNormalMatrix & traverse.traverse %~ realToFrac )

    theNormalMatrix :: M33 Double
    theNormalMatrix =
        let invCam        = camera & cameraTransformation %~ inverseTransformation
            invViewM      = fmap realToFrac <$> invCam^.cameraMatrix
            invModelM     = ent^.entityTransformation.to inverseTransformation.transformationMatrix
        in adjoint $ invModelM^.to m44_to_m33 !*! invViewM^.to m44_to_m33


defaultGeoMaterial :: GeoMaterial
defaultGeoMaterial =
    let albedoMat    = defaultMaterialSRGB
        normalMat    = defaultMaterialSRGB & matTexture .~ (mkTexture2D "NORMALDUMMY" $ zNormalDummy TexSRGB8)
        roughnessMat = mkMaterial 1.0 $ mkTexture2D "ROUGHDUMMY" $ zeroNormalDummy TexY8
    in GeoMaterial albedoMat normalMat roughnessMat


instance Default GeoMaterial where
    def = defaultGeoMaterial


instance FramebufferSpec GeoPassChannels RenderTargets where
    fboColors GeoPassChannelsF{gAlbedoChannel, gNormalChannel} =
        [ Attachment (ColorAttachment 0) $ TextureTarget GL.Texture2D gAlbedoChannel 0
        , Attachment (ColorAttachment 1) $ TextureTarget GL.Texture2D gNormalChannel 0
        ]

    fboDepth GeoPassChannelsF{gDepthChannel} =
        Just $ Attachment DepthAttachment $ TextureTarget GL.Texture2D gDepthChannel 0

geoVertex :: Vertex YGM.YGMFormat -> GeoVertex
geoVertex internal =
    position3 =: ( realToFrac <$> ( rGet position3 internal :: V3 Float ) ) <+>
    texture2  =: ( realToFrac <$> ( rGet texture2 internal  :: V2 Float ) ) <+>
    tangentX  =: ( realToFrac <$> ( rGet tangentX internal  :: V3 Float ) ) <+>
    tangentZ  =: ( realToFrac <$> ( rGet tangentZ internal  :: V4 Float ) )
