#version 410 core
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_include : require
/*
*/

#include <common.h>
#include "pass/gbuffer.h"
#include <brdf.h>

struct Box {
  vec3 lo, hi;
};

uniform samplerCube RadianceEnvironment;
uniform sampler3D SceneOpacityVoxel;
// maps world coords in bounds to the -0.5 .. +0.5 range
uniform mat4 WorldToVoxelSpace;
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

mat3 fakeTangentSpace(in vec3 N)
{
  vec3 a = cross(N, vec3(1,0,0));
  vec3 b = cross(N, vec3(0,1,0));
  vec3 c = cross(N, vec3(0,0,1));
  float da = length(a);
  float db = length(b);
  float dc = length(c);
  vec3 T;
  if (da > db && da > dc)
  {
    T = normalize(a);
  } else if (db > da && db > dc)
  {
    T = normalize(b);
  } else
  {
    T = normalize(c);
  }

  vec3 B = normalize(cross(N,T));
  return mat3(T, B, N);
}

vec4 VoxelConeTrace(in vec3 Origin, vec3 Direction, float ConeAngleRatio, float MaxDist)
{
  vec4 accum = vec4(0);

  float gridDim = float(textureSize(SceneOpacityVoxel, 0).x);
  float minDiameter = 4.0 / gridDim;

  float startDist = minDiameter;
  float dist = startDist;
  while (dist <= MaxDist && accum.w < 1.0)
  {
    float sampleDiameter = max(minDiameter, ConeAngleRatio * dist);
    float sampleLOD   = log2(sampleDiameter * gridDim);
    vec3 samplePos    = Origin + Direction * dist;
    vec4 sampleValue  = textureLod(SceneOpacityVoxel, samplePos, sampleLOD);
    accum += sampleValue * (1.0 - accum.w);
    dist += sampleDiameter;
  }

  return accum;
}

float AmbientOcclusion(in Surface surface, in sampler3D SceneVoxelRep, in mat4 WorldToVoxelSpace)
{
  vec4 origin = WorldToVoxelSpace * vec4(surface.Position, 1.0);
  origin.xyz *= 1.0 / origin.w;
  // mapping from -0.5..0.5 to 0..1
  origin.xyz = (origin.xyz + 1) / 2.0;
  mat3 TBN = fakeTangentSpace(surface.Normal);

  const float coneRatio = 2;
  const float maxDist = 0.5;
  vec4 accum = vec4(0);
  accum += VoxelConeTrace(origin.xyz, TBN[2], coneRatio, maxDist);
  accum += 0.707 * VoxelConeTrace(origin.xyz, normalize(TBN[2] + TBN[0]), coneRatio, maxDist);
  accum += 0.707 * VoxelConeTrace(origin.xyz, normalize(TBN[2] - TBN[0]), coneRatio, maxDist);
  accum += 0.707 * VoxelConeTrace(origin.xyz, normalize(TBN[2] + TBN[1]), coneRatio, maxDist);
  accum += 0.707 * VoxelConeTrace(origin.xyz, normalize(TBN[2] - TBN[1]), coneRatio, maxDist);

  return 1.0 - accum.a;
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
    float OcclusionMaskAmbient = AmbientOcclusion( surface, SceneOpacityVoxel, WorldToVoxelSpace );

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
    // pixelColor.rgb = ((WorldToVoxelSpace * vec4(surface.Position, 1.0)).xyz + 1) / 2.0;
    // pixelColor.rgb = ((surface.Position / 10) + 1) / 2;


    // pixelColor.rgb = vec3(surface.Roughness);
    // pixelColor.rgb  += vec3(0.01, 0, 0);
    // if (IsPositionalLight(Light))
    // {
    // }
    // pixelColor.rgb  = EncodeTextureNormal(surface.Position / 10);
    // pixelColor.rgb = 0.5 * EncodeTextureNormal( surface.Normal );

}

