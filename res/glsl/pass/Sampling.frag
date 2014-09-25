#ifndef __SAMPLING__
#define __SAMPLING__

const int N_SAMPLES = 6;

uniform sampler2D TextureSamplers [ N_SAMPLES ];
uniform vec4      TextureSize     [ N_SAMPLES ];

in      vec2      SamplingUV      [ N_SAMPLES ];

#endif // __SAMPLING__