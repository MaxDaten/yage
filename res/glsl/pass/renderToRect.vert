#version 410 core


in vec3 vPosition;
in vec2 vTexture;

uniform mat4 ProjMatrix     = mat4(0.0);
uniform mat4 ModelMatrix    = mat4(0.0);

out vec2 VertexUV;
out vec4 VertexPos;

void main(void)
{
    
    VertexPos   = ProjMatrix * ModelMatrix * vec4(vPosition, 1.0);
    
    VertexUV    = vTexture;

    gl_Position = VertexPos;
}
