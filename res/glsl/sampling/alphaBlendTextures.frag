#version 410
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_include : require

#include <locations.h>
#include "sampling/sampling.h"

layout(location = FRAG_COLOR) out vec4 fragColor;

void main()
{
  fragColor = vec4(0);
  for (int i = 0; i < iUsedTextures; i++)
  {
    vec2 uv = gl_FragCoord.xy / textureSize(iTextures[i], 0);
    vec4 texColor = iColors[i] * texture(iTextures[i], uv);
    // fragColor = mix(fragColor, texColor, texColor.a);
    fragColor = texColor;
  }
}
