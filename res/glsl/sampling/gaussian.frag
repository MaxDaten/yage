#version 410 core
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_include : require
// http://rastergrid.com/blog/2010/09/efficient-gaussian-blur-with-linear-sampling/
// https://software.intel.com/en-us/blogs/2014/07/15/an-investigation-of-fast-real-time-gpu-based-image-blur-algorithms

#include <locations.h>
#include "sampling/sampling.h"

uniform vec2 iDirection;
uniform float iWidth = 1.0;

layout(location = FRAG_COLOR) out vec4 fragColor;

vec3 GaussianBlur( sampler2D tex0, vec2 centreUV, vec2 pixelOffset )
{
  vec3 colOut = vec3( 0, 0, 0 );


  const int stepCount = 3;
  const float gWeights[stepCount] = float[] ( 0.2270270270, 0.3162162162, 0.0702702703 );
  const float gOffsets[stepCount] = float[] ( 0.0, 1.3846153846, 3.2307692308 );

  // const int stepCount = 2;
  // const float gWeights[stepCount] = float[] (0.44908, 0.05092);
  // const float gOffsets[stepCount] = float[] (0.53805, 2.06278);

  for (int i = 0; i < stepCount; i++){
    vec2 texCoordOffset = gOffsets[i] * pixelOffset;
    vec3 col = texture(tex0, centreUV + texCoordOffset).rgb +  texture(tex0, centreUV - texCoordOffset).rgb;
    colOut += gWeights[i] * col;
  }

  return colOut;
}

void main()
{
  vec2 uv = gl_FragCoord.xy * iTargetSize.zw;
  vec2 step = iWidth * iDirection / textureSize(iTextures[0], 0);

  fragColor.rgb = GaussianBlur(iTextures[0], uv, step);
  fragColor.rgb += texture(iTextures[1], uv).rgb;
}
