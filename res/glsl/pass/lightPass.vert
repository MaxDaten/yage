#version 410 core

#include "Common.glsl"
#include "BRDF.glsl"

uniform mat4 ViewMatrix;
uniform mat4 VPMatrix;
uniform mat4 ModelMatrix;
uniform mat4 ViewToScreenMatrix;
uniform LightData Light;

uniform ivec2 ViewportDim;

in vec3 vPosition;

out vec3 VertexPosVS;
out vec4 ScreenPos;
out mat4 ViewToWorld;

mat4 MVPMatrix = VPMatrix * ModelMatrix;
void main()
{
    vec4 OutPosition;
    ViewToWorld = inverse( ViewMatrix ); // TODO : to uniform
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
        OutPosition     = ViewToScreenMatrix * ModelMatrix * vec4( vPosition, 1.0 );
    }
    // in NDC
    ScreenPos    = OutPosition;
    gl_Position  = OutPosition;

    UNUSED(ViewportDim);
}
