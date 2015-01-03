#version 410 core
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_include : require

#include <common.h>
#include "pass/gbuffer.h"

uniform sampler2D AlbedoTexture;
uniform sampler2D NormalTexture;
uniform sampler2D RoughnessTexture;
uniform sampler2D MetallicTexture;

uniform vec4 AlbedoColor;
uniform vec4 NormalColor;
uniform float RoughnessIntensity;
uniform float MetallicIntensity;

// uniform mat4 ModelMatrix = mat4(1.0);

in vec2 AlbedoST;
in vec2 NormalST;
in vec2 RoughnessST;
in vec2 MetallicST;
in mat3 TangentInverse;


Surface GetSurface(void)
{
  Surface surface;
  surface.Albedo      = texture( AlbedoTexture, AlbedoST ) * AlbedoColor;
  surface.Roughness   = texture( RoughnessTexture, RoughnessST ).r * RoughnessIntensity;
  surface.Metallic    = texture( MetallicTexture, MetallicST ).r * MetallicIntensity;

  surface.Normal      = normalize( TangentInverse * DecodeTextureNormal( texture( NormalTexture, NormalST ).rgb ) );
  UNUSED(NormalColor);
  return surface;
}


void main()
{
    Surface surface = GetSurface();
    EncodeGBuffer( surface );
}
