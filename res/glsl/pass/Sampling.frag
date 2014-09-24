#ifndef __SAMPLING__
#define __SAMPLING__

uniform sampler2D   TextureSampler0;
uniform vec4        TextureSize0    = vec4(0);
in      vec2        SamplingUV0;

uniform sampler2D   TextureSampler1;
uniform vec4        TextureSize1    = vec4(0);
in      vec2        SamplingUV1;

uniform sampler2D   TextureSampler2;
uniform vec4        TextureSize2    = vec4(0);
in      vec2        SamplingUV2;


#endif // __SAMPLING__