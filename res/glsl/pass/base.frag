#version 410 core
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_include : require

#include <common.h>
#include <definitions.h>
#include <textureUnits.h>
#include "pass/gbuffer.h"

uniform sampler2D AlbedoTexture;
uniform sampler2D NormalTexture;
uniform sampler2D RoughnessTexture;
uniform sampler2D MetallicTexture;

uniform vec4 AlbedoColor;
uniform vec4 NormalColor;
uniform float RoughnessIntensity;
uniform float MetallicIntensity;

in vec2 AlbedoST;
in vec2 NormalST;
in vec2 RoughnessST;
in vec2 MetallicST;
in mat3 TangentInverse;
in vec3 PositionWorld;

layout (location = G_CHANNEL_A) out vec4 outChannelA;
layout (location = G_CHANNEL_B) out vec4 outChannelB;
layout (location = G_CHANNEL_C) out vec4 outChannelC;
layout (location = G_CHANNEL_D) out vec4 outChannelD;


Surface GetSurface(void)
{
  Surface surface;
  surface.Albedo      = texture( AlbedoTexture, AlbedoST ) * AlbedoColor;
  surface.Roughness   = texture( RoughnessTexture, RoughnessST ).r * RoughnessIntensity;
  surface.Metallic    = texture( MetallicTexture, MetallicST ).r * MetallicIntensity;

  vec3 N = NormalColor.rgb * DecodeTextureNormal( texture( NormalTexture, NormalST ).rgb );
  N.z = NormalColor.a;
  surface.Normal      = normalize( TangentInverse * N  );

  surface.Position    = PositionWorld;
  return surface;
}

void main()
{
    Surface surface = GetSurface();
    EncodeGBuffer( surface, outChannelA, outChannelB, outChannelC, outChannelD );
}
