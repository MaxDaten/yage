#version 410 core

uniform mat4 ViewMatrix;
uniform mat4 VPMatrix;
uniform mat4 ModelMatrix;

in vec3 vPosition;

out vec3 VertexPosVS;

mat4 MVPMatrix = VPMatrix * ModelMatrix;
void main()
{
    VertexPosVS     = (ViewMatrix * ModelMatrix * vec4(vPosition, 1.0)).xyz;
    gl_Position     = MVPMatrix * vec4( vPosition, 1.0 );
}
