#version 410 core

#include "Sampling.frag"

in vec2 SamplingUV;
layout (location = 0) out vec3 pixelColor;


void main(void)
{
    vec3 baseColor = texture( TextureSampler0, SamplingUV0 ).rgb;
    vec4 addColor  = texture( TextureSampler1, SamplingUV1 );
    vec3 outColor  = mix( baseColor, addColor.rgb, addColor.a );
    
    pixelColor = clamp(outColor, 0, 1);
}

