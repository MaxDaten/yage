/* # Buffer Encoding

   ## Normal Encoding
    - http://aras-p.info/texts/CompactNormalStorage.html
*/
#ifndef __GBUFFER__
#define __GBUFFER__

/*
    0 : simple z reconstruction
    1 : spherical map transformation
*/
#define NORMAL_ENCODING_TYPE 1

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


vec3 DecodeNormalZ( vec2 Normal )
{
    float NormalZ = sqrt( saturate ( 1.0 - dot( Normal, Normal ) ) );
    return vec3( Normal, NormalZ );
}


vec2 EncodeNormalSpheremap( vec3 Normal )
{
    vec2 n_e = normalize( Normal.xy ) * sqrt( Normal.z * 0.5 + 0.5 );
    return n_e;
}


vec3 DecodeNormalSpheremap( vec4 NormalEncoded )
{
    vec3 NN = NormalEncoded.xyz;

    NN.z      = dot( NN.xy, NN.xy ) * 2.0 - 1.0;
    NN.xy     = normalize(NN.xy) * sqrt( 1 - NN.z * NN.z );
    return NN;
}


vec2 EncodeNormalXY( vec3 Normal )
{
#if NORMAL_ENCODING_TYPE == 0
    return Normal.rg;
#elif NORMAL_ENCODING_TYPE == 1
    return EncodeNormalSpheremap( Normal );
#endif
}


vec3 DecodeNormalXY( vec2 Normal )
{
#if NORMAL_ENCODING_TYPE == 0
    return DecodeNormalZ( Normal );
#elif NORMAL_ENCODING_TYPE == 1
    return DecodeNormalSpheremap( vec4(Normal, 0, 0) );
#endif
}

#endif // __GBUFFER__