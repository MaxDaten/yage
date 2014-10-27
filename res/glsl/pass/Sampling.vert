#version 410 core

#include "GlobalDefs.glsl"

in vec2 vPosition;

uniform int N_SAMPLES       = 1;

out vec2 SamplingUV[ MAX_TEXTURES ];
out vec4 VertexPos;

void main(void)
{
    
    VertexPos   = vec4(vPosition, 0.0, 1.0);
    
    for (int i = 0; i < N_SAMPLES; i++)
    {
        SamplingUV[i] = vPosition * 0.5 + 0.5;
    }

    gl_Position = VertexPos;
}
