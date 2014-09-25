#version 410 core

#include "GlobalDefs.glsl"

in vec3 vPosition;
in vec2 vTexture;

uniform mat4 ProjMatrix     = mat4(0.0);
uniform mat4 ModelMatrix    = mat4(0.0);

uniform int N_SAMPLES       = 1;

out vec2 SamplingUV[ MAX_TEXTURES ];
out vec4 VertexPos;

void main(void)
{
    
    VertexPos   = ProjMatrix * ModelMatrix * vec4(vPosition, 1.0);
    
    for (int i = 0; i < N_SAMPLES; i++)
    {
        SamplingUV[i] = vTexture;
    }

    gl_Position = VertexPos;
}
