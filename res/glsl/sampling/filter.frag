#version 410
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_include : require

#include <locations.h>
#include "sampling/sampling.h"

layout(location = FRAG_COLOR) out vec4 fragColor;

vec4 combine(in vec4 dest, in vec4 src, in vec4 weight)
{
  return dest + weight * src;
}

void main()
{
  vec4 dest = vec4(0);
  for (int i = 0; i < iUsedTextures; i++)
  {
    vec2 uv = gl_FragCoord.xy / textureSize(iTextures[i], 0);
    dest = combine(dest, texture(iTextures[i], uv), iWeights[i]);
  }

  fragColor = dest;
}
