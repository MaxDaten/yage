#ifndef __GBUFFER__
#define __GBUFFER__

#include "Common.glsl"


vec3 DecodeNormal( vec2 TexNormal )
{
    vec2 NormalXY = TexNormal * 2.0 - 1.0;
    float NormalZ = sqrt( saturate ( 1.0 - dot( NormalXY, NormalXY ) ) );
    return vec3( NormalXY, NormalZ );
}

vec3 EncodeNormal( vec3 Normal3d )
{
    return Normal3d.xyz * 0.5 + 0.5;
}

#endif // __GBUFFER__