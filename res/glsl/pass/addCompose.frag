#version 410 core

#include "Sampling.frag"

uniform float BaseWeight = 1.0;
uniform float AddWeight = 1.0;

layout (location = 0) out vec3 pixelColor;

void main()
{  
    vec3 texColor = BaseWeight * texture( TextureSampler0, SamplingUV0 ).rgb;
    texColor     += AddWeight  * texture( TextureSampler1, SamplingUV1 ).rgb;
    
    pixelColor = texColor;
}

