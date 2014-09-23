#version 410 core

#include "Common.glsl"
#include "GBuffer.glsl"

uniform sampler2D AlbedoTexture;
uniform sampler2D NormalTexture;
uniform sampler2D RoughnessTexture;

uniform vec4 AlbedoColor;
uniform vec4 NormalColor;
uniform float RoughnessIntensity;

in vec3 VertexPos_View;
in vec2 AlbedoST;
in vec2 NormalST;
in vec2 RoughnessST;
smooth in mat3 TangentToView;

// Red Green Blue Depth
layout (location = 0) out vec4 OutAlbedo;
layout (location = 1) out vec4 OutNormal;
// layout (location = 2) out vec3 specularOut;
// layout (location = 3) out vec3 glossyOut;


vec4 GetAlbedoColor()
{
    return AlbedoColor * texture( AlbedoTexture, AlbedoST );
}

float GetRoughness()
{
    return RoughnessIntensity * texture( RoughnessTexture, RoughnessST ).r;
}


void main()
{
    OutAlbedo.rgb   = GetAlbedoColor().rgb;

    OutAlbedo.a     = GetRoughness();

    vec3 texNormal = NormalColor.rgb * DecodeNormal( texture( NormalTexture, NormalST ).rg );
    texNormal      = normalize( texNormal );
    OutNormal.rg   = EncodeNormal( TangentToView * texNormal ).rg;
}
