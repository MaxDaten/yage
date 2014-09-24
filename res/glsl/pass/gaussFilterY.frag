#version 410 core
// http://rastergrid.com/blog/2010/09/efficient-gaussian-blur-with-linear-sampling/

#include "Sampling.frag"

uniform float offsets[3] = float[]( 0.0, 1.3846153846, 3.2307692308 );
uniform float weights[3] = float[]( 0.2270270270, 0.3162162162, 0.0702702703 );

layout (location = 0) out vec3 pixelColor;

void main()
{
    vec4 texColor = texture( TextureSampler0, SamplingUV0 ) * weights[0];
    for (int i=1; i<3; i++) {
        texColor += texture( TextureSampler1, SamplingUV0 + vec2(0.0, offsets[i] * TextureSize0.w) ) * weights[i];
        texColor += texture( TextureSampler2, SamplingUV0 - vec2(0.0, offsets[i] * TextureSize0.w) ) * weights[i];
    }
    pixelColor = texColor.rgb;
}

