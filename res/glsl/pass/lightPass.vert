#version 410 core

#include "Common.glsl"
#include "BRDF.glsl"

uniform mat4 ViewMatrix;
uniform mat4 VPMatrix;
uniform mat4 ModelMatrix;
uniform mat4 ViewToScreenMatrix;
uniform LightData Light;

in vec3 vPosition;

out vec3 VertexPosVS;

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
        VertexPosVS     = (ViewToScreenMatrix * ModelMatrix * vec4(vPosition.xy, 1.0, 0.0)).xyz;
        OutPosition     = ViewToScreenMatrix * ModelMatrix * vec4( vPosition, 1.0 );
    }
    
    
    gl_Position  = OutPosition;
}
