#version 410 core

in vec3 vPosition;
in vec2 vTexture;

uniform mat4 ProjMatrix     = mat4(0.0);
uniform mat4 ModelMatrix    = mat4(0.0);

const int N_SAMPLES = 6; // TODO : TO UNIFORM

out vec2 SamplingUV[ N_SAMPLES ];
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
