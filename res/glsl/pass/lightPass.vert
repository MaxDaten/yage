#version 410 core

uniform mat4 ViewMatrix        = mat4(0.0);
uniform mat4 VPMatrix          = mat4(0.0);
uniform mat4 ModelMatrix       = mat4(0.0);

in vec3 vPosition;

out mat4 ViewSpace;
out vec3 VertexPosVS;

mat4 MVPMatrix = VPMatrix * ModelMatrix;
void main()
{
    VertexPosVS     = vec3(ViewMatrix * ModelMatrix * vec4(vPosition, 1.0));
    ViewSpace       = ViewMatrix;
    gl_Position     = MVPMatrix * vec4( vPosition, 1.0 );
}
