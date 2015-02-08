#version 410 core
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_include : require

#include <common.h>
#include <attributes.h>
#include <brdf.h>

out gl_PerVertex {
vec4 gl_Position;
};

uniform LightData Light;

layout(location = VPOSITION) in vec3 vPosition;

out vec3 VertexPosVS;
out vec4 ScreenPos;
// out mat4 ViewToWorld;
// out Light iLight;

mat4 MVPMatrix = VPMatrix * ModelMatrix;

void main()
{
    vec4 OutPosition;
    if (IsPositionalLight( Light ))
    {
        VertexPosVS     = (ViewMatrix * ModelMatrix * vec4(vPosition, 1.0)).xyz;
        OutPosition     = MVPMatrix * vec4( vPosition, 1.0 );
    }
    // directional light
    else
    {
        // we render a full screen quad for directional lights
        VertexPosVS     = (ViewToScreenMatrix * ModelMatrix * vec4(vPosition.xy, 1.0, 0.0)).xyz;
        OutPosition     = vec4 (float(gl_VertexID / 2) * 4.0 - 1.0, float(gl_VertexID % 2) * 4.0 - 1.0, 0.0, 1.0);
    }
    // in NDC
    // iLight       = LightInViewspace( Light );
    ScreenPos    = OutPosition;
    gl_Position  = OutPosition;

}

// Light LightInViewspace(Light light)
// {
//   light
// }
