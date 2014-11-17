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
// Red Green Blue Depth
layout (location = 0) out vec4 channelA;
layout (location = 1) out vec4 channelB;
smooth in mat3 TangentToView;


uniform sampler2D inChannelA;
uniform sampler2D inChannelB;
uniform sampler2D DepthTexture;
uniform vec2 ZProjRatio;
in vec3 VertexPosVS;


struct Surface 
{
    vec3 Position;
    vec4 Albedo;
    vec3 Specular;
    vec3 Normal;
    float Metallic;
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
    return normalize( Normal.xy ) * sqrt( Normal.z * 0.5 + 0.5 );
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


void EncodeGBuffer( Surface surface )
{
    channelA.rgb   = surface.Albedo.rgb;
    channelA.a     = surface.Roughness;

    channelB.rg    = EncodeNormalXY ( normalize ( TangentToView * surface.Normal ) );
    
    channelB.b     = surface.Metallic;
    channelB.a     = 0.0; // currently unused
}

Surface DecodeGBuffer( vec2 uv )
{
      // the channel for albedo rgb + distance from View
    vec4 chA       = texture( inChannelA, uv ).rgba;
    vec4 chB       = texture( inChannelB, uv ).rgba;
    float bufferDepth  = texture( DepthTexture, uv ).r;
    
    Surface surface;

    // extrapolate the view space position of the pixel to the zFar plane
    surface.Position  = PositionVSFromDepth( bufferDepth, ZProjRatio, VertexPosVS );
    surface.Albedo    = vec4(chA.rgb, 1.0);
    surface.Roughness = max(chA.a, 0.02);
    surface.Specular  = vec3(0.5);
    surface.Normal    = normalize( DecodeNormalXY( chB.rg ) );
    surface.Metallic  = chB.b;

    surface.Specular = mix( 0.08 * surface.Specular, surface.Albedo.rgb, vec3(surface.Metallic));
    surface.Albedo   -= surface.Albedo * surface.Metallic;

    return surface;
}


#endif // __GBUFFER__