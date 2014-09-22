#version 410 core


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


float saturate(float value)
{
    return clamp(value, 0.0, 1.0);
}

vec4 GetAlbedoColor()
{
    return AlbedoColor * texture( AlbedoTexture, AlbedoST );
}

float GetRoughness()
{
    return RoughnessIntensity * texture( RoughnessTexture, RoughnessST ).r;
}


vec3 DecodeNormal( vec3 normalC )
{
    return 2.0 * normalC - 1.0;
}


vec3 GetBumpedNormal()
{
    vec2 NormalXY = DecodeNormal( texture( NormalTexture, NormalST ).rgb ).rg;
    float NormalZ = sqrt( saturate( 1.0 - dot( NormalXY, NormalXY ) ) );
    return NormalColor.rgb * normalize(vec3( NormalXY, NormalZ ));
    // NormalColor;
    // return normalize(DecodeNormal( texture( NormalTexture, NormalST ).rgb ));
}


vec3 EncodeNormal( vec3 Normal3d )
{
    return Normal3d.xyz * 0.5 + 0.5;
}


void main()
{
    OutAlbedo.rgb   = GetAlbedoColor().rgb;

    OutAlbedo.a     = GetRoughness();


    OutNormal.rg   = EncodeNormal( TangentToView * GetBumpedNormal() ).rg;
}
