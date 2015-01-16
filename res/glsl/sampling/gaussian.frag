#version 410 core
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_include : require
// http://rastergrid.com/blog/2010/09/efficient-gaussian-blur-with-linear-sampling/

#include <locations.h>
#include "sampling/sampling.h"

uniform vec2 iDirection;
uniform float iWidth = 1.0;

#define GAUSS_SAMPLES 13

#if     GAUSS_SAMPLES == 13
float offsets[13] = float[]( -1.7688, -1.1984, -0.8694, -0.6151, -0.3957, -0.1940, 0, 0.1940, 0.3957, 0.6151, 0.8694, 1.1984, 1.7688 );
const float n = 13.0;
#elif   GAUSS_SAMPLES == 7
float offsets[7] = float[]( -1.4652, -0.7916, -0.3661, 0, 0.3661, 0.7916, 1.4652 );
const float n = 7.0;
#endif

layout(location = FRAG_COLOR) out vec4 fragColor;

void main()
{
  vec4 texColor = vec4(0);
  vec2 uv = gl_FragCoord.xy * iTargetSize.zw;
  vec2 step = iWidth * iDirection / textureSize(iTextures[0], 0);
  for (int i = 0; i < int(n); i++) {
    texColor += texture( iTextures[0], uv + offsets[i] * step);
  }
  texColor += texture(iTextures[1], uv);
  texColor /= n + iUsedTextures - 1;

  fragColor = texColor;
}
