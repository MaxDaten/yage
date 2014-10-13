/*
    a lighting model for physically based rendering
    calculation is done in view-space. mainly inspired by unreal engine.

    Context:
        - calculations, positions and vectors are in view space
        - calculations and directions a lit point centric 
          (e.g. `L` is the direction from lit point (`P`) to the light center)

    # irradiance area lights
    - http://www.dgp.toronto.edu/~ghali/publications/thesis/html/node4.html

*/
#version 410 core

#include "Common.glsl"
#include "GBuffer.glsl"
#include "BRDF.glsl"

uniform vec2 ZProjRatio;

uniform sampler2D AlbedoTexture;
uniform sampler2D NormalTexture;
uniform sampler2D DepthTexture;
uniform samplerCube EnvironmentCubeMap;
uniform float Gamma = 2.2;


uniform ivec2 ViewportDim;

in vec3 VertexPosVS;

// Light in view space (Position, Direction, etc)
uniform LightT Light;


layout (location = 0) out vec4 pixelColor;


Surface GetSurfaceAttributes( vec4 channelA, vec4 channelB, float bufferDepth )
{
    Surface attribs;
    
    // extrapolate the view space position of the pixel to the zFar plane
    attribs.Position  = PositionVSFromDepth( bufferDepth, ZProjRatio, VertexPosVS );
    attribs.Albedo    = channelA.rgb;
    attribs.Roughness = channelA.a;
    attribs.Specular  = vec3(0.5);
    attribs.Normal    = normalize( DecodeNormalXY( channelB.rg ) );
    return attribs;
}

float DistanceAttenuationInverseSquare( float distance2 )
{
    return 16 / ( distance2 + 0.0001 );
}

// [Karis 2013, "Real Shading in Unreal Engine 4"]
float MaskingRadius( float distance2, float radius )
{
    float L0 = pow( sqrt(distance2) / radius, 4 );
    return square( saturate( 1 - L0) ); 
}

vec3 SpecularTerm ( Surface surface, float NoL, vec3 L, vec3 V )
{
    float Roughness = surface.Roughness;
    float a  = square( Roughness );
    float a2 = a * a;
    float Energy = 1;
    vec3 N   = surface.Normal;
    // vec3 R   = reflect(-V, N);
    
    vec3 H    = normalize( V + L );
    // float NoL = saturate( dot(N, L) );
    float NoV = saturate( dot(N, V) );
    float NoH = saturate( dot(N, H) );
    float VoH = saturate( dot(V, H) );

    float D   = SpecularNDF( a2, NoH );
    // float D   = 1;
    float G   = Geometric( Roughness, NoV, NoL );
    // float G   = 1;
    vec3 F    = Fresnel( surface.Specular, VoH );

    return (Energy * D * G) * F;
}

vec3 DiffuseTerm ( Surface surface )
{
    return Diffuse( surface.Albedo );
}

float SpotAttenuation( vec3 L, LightT light )
{
    vec3 direction = -light.LightDirection;
    float inner = light.LightConeAnglesAndRadius.x;
    float outer = light.LightConeAnglesAndRadius.y;
    return square(saturate( dot(L, direction) - outer ) / (outer -  inner) );
}

vec3 SurfaceShading ( Surface surface, LightT light )
{
    // vector from origin (view space) to lit point
    vec3 P  = surface.Position;
    // direction from lit point to the view
    vec3 V  = normalize(-P);

    // vector from the current lit point to the light source
    vec3 PtoL       = light.LightPosition - P;
    float distance2 = dot( PtoL, PtoL );


    float DistanceAttenuation = DistanceAttenuationInverseSquare( distance2 );
    float RadiusMask          = MaskingRadius( distance2, light.LightConeAnglesAndRadius.z );
    float Attenuation         = DistanceAttenuation * RadiusMask;

    // direction to the light
    vec3 L          = normalize( PtoL );
    float NoL       = saturate(dot( surface.Normal, L));
    if ( IsSpotlight( light ) )
    {
        Attenuation *= SpotAttenuation( L, light);
    }

    vec3 DiffuseShading  = DiffuseTerm( surface );
    vec3 SpecularShading = SpecularTerm( surface, NoL, L, V );

    return light.LightColor.rgb * NoL * Attenuation * (DiffuseShading + SpecularShading);
}

void main()
{
    vec2 screenUV = gl_FragCoord.xy / ViewportDim.xy;
    
    // the channel for albedo rgb + distance from View
    vec4 albedoCh       = texture( AlbedoTexture, screenUV ).rgba;
    vec4 normalCh       = vec4(texture( NormalTexture, screenUV ).rg, 0, 0);
    float zBufferDepth  = texture( DepthTexture, screenUV ).r;

    
    Surface surface = GetSurfaceAttributes( albedoCh, normalCh, zBufferDepth );
    pixelColor.rgb  = SurfaceShading ( surface, Light );
    
    // pixelColor.rgb  += vec3(0.05, 0, 0);
    // pixelColor.rgb  = EncodeTextureNormal(surface.Position / 10);
    // pixelColor.rgb += 0.5 * EncodeTextureNormal( surface.Normal );

    EnvironmentCubeMap; // unused
}

