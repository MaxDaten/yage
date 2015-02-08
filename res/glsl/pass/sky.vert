#version 410 core
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_include : require

#include <common.h>
#include <definitions.h>
#include <attributes.h>

out gl_PerVertex {
vec4 gl_Position;
};

uniform mat4 SkyTextureMatrix  = mat4(1.0);

layout(location = VPOSITION) in vec3 vPosition;

out vec3 VertexSTP;

mat4 MVPMatrix = VPMatrix * ModelMatrix;
void main()
{
    mat4 MVPMatrix  = VPMatrix * ModelMatrix;
    VertexSTP       = (SkyTextureMatrix * vec4(vPosition, 1.0)).stp;
    gl_Position     = MVPMatrix * vec4( vPosition, 1.0 );
}
