#version 410 core
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_include : require
/*
*/

#include <common.h>
#include "pass/gbuffer.h"
#include <brdf.h>

uniform samplerCube RadianceEnvironment;
uniform sampler3D SceneOpacityVoxel;
uniform vec3 SceneBoundsLow;
uniform vec3 SceneBoundsHigh;
uniform int MaxMipmapLevel;
uniform int DiffuseMipmapOffset;
uniform vec3 CameraPosition;

in vec4 ScreenPos;

layout (location = 0) out vec4 pixelColor;

vec3 ApproximateSpecularIBL( vec3 SpecularColor, float Roughness, float NoV, vec3 R)
{
    float MipMapLevel = Roughness * MaxMipmapLevel;
    vec3 SpecularIBL  = textureLod( RadianceEnvironment, R, MipMapLevel ).rgb;

    vec2 envBRDF = EnvironmentBRDF( Roughness, NoV );
    return SpecularIBL * (SpecularColor * envBRDF.x + envBRDF.y);
}


vec3 SurfaceAmbientShading ( Surface surface )
{
    // vector from origin (view space) to lit point
    vec3 P  = surface.Position;
    vec3 N  = surface.Normal;

    // direction from lit point to the view
    vec3 V  = normalize( CameraPosition - P );
    float NoV = saturate( dot(N, V) );
    vec3 R   = reflect( -V, N );

    vec3 DiffuseAmbient = surface.Albedo.rgb * textureLod( RadianceEnvironment, N, MaxMipmapLevel + DiffuseMipmapOffset ).rgb;
    vec3 SpecularAmbient = ApproximateSpecularIBL( surface.Specular, surface.Roughness, NoV, R );
    float OcclusionMaskAmbient = 1.0;

    vec3 OutColor = vec3(0.0);
    OutColor += DiffuseAmbient;
    OutColor += SpecularAmbient;
    OutColor *= OcclusionMaskAmbient;

    return OutColor;
}

void main()
{
    vec2 uv = gl_FragCoord.xy / textureSize(inChannelA, 0);

    Surface surface = DecodeGBuffer( uv );
    pixelColor.rgb  = SurfaceAmbientShading ( surface );

    // pixelColor.rgb = vec3(surface.Roughness);
    // pixelColor.rgb  += vec3(0.01, 0, 0);
    // if (IsPositionalLight(Light))
    // {
    // }
    // pixelColor.rgb  = EncodeTextureNormal(surface.Position / 10);
    // pixelColor.rgb = 0.5 * EncodeTextureNormal( surface.Normal );

}

