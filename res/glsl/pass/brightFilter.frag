#version 410 core

#include "Sampling.frag"

uniform float WhitePoint;

layout (location = 0) out vec3 pixelColor;

void main()
{
    vec3 texColor = texture( TextureSampler0, SamplingUV0 ).rgb;
    vec3 color = vec3(0.0);
    
    if (length(texColor) >= WhitePoint)
    {
        color = texColor;
    }

    pixelColor = color;
}

