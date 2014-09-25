#ifndef __SAMPLING__
#define __SAMPLING__

#include "GlobalDefs.glsl"

// const int N_SAMPLES = 12;

uniform sampler2D TextureSamplers [ MAX_TEXTURES ];
uniform vec4      TextureSize     [ MAX_TEXTURES ];
uniform int       N_SAMPLES = 1;

in      vec2      SamplingUV      [ MAX_TEXTURES ];


#endif // __SAMPLING__