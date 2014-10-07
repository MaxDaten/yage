#ifndef __GBUFFER__
#define __GBUFFER__

struct Surface 
{
    vec3 Position;
    vec3 Albedo;
    vec3 Specular;
    vec3 Normal;
    float Roughness;
};

vec3 DecodeTextureNormal( vec3 TexNormal )
{
    return TexNormal * 2.0 - 1.0;
    
}

vec3 EncodeTextureNormal( vec3 Normal3d )
{
    return Normal3d.xyz * 0.5 + 0.5;
}

vec3 DecodeNormalXY( vec2 Normal )
{
    float NormalZ = sqrt( saturate ( 1.0 - dot( Normal, Normal ) ) );
    return vec3( Normal, NormalZ );
}

#endif // __GBUFFER__