#version 410 core

uniform mat4 ViewMatrix        = mat4(1.0);
uniform mat4 VPMatrix          = mat4(1.0);
uniform mat4 ModelMatrix       = mat4(1.0);
uniform mat4 SkyTextureMatrix  = mat4(1.0);

in vec3 vPosition;

out vec3 VertexSTP;

mat4 MVPMatrix = VPMatrix * ModelMatrix;
void main()
{
    mat4 MVPMatrix  = VPMatrix * ModelMatrix;
    VertexSTP       = (SkyTextureMatrix * vec4(vPosition, 1.0)).stp;
    gl_Position     = MVPMatrix * vec4( vPosition, 1.0 );
}
