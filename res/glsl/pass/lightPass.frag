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
uniform samplerCube RadianceEnvironment;
uniform float Gamma = 2.2;


in vec3 VertexPosVS;
in vec4 ScreenPos;
in mat4 ViewToWorld;


// Light in view space (Position, Direction, etc)
uniform LightData Light;


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
    attribs.Metallic  = 1.0;

    attribs.Specular = mix( 0.08 * attribs.Specular, attribs.Albedo, vec3(attribs.Metallic));
    attribs.Albedo   -= attribs.Albedo * attribs.Metallic;
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


vec3 ApproximateSpecularIBL( vec3 SpecularColor, float Roughness, float NoV, vec3 R)
{
    // float MaxMipLevel = 8;
    float MaxMipLevel = 5;
    float MipMapLevel = Roughness * MaxMipLevel;
    vec3 SpecularIBL  = textureLod( RadianceEnvironment, R, MipMapLevel ).rgb;

    vec2 envBRDF = EnvironmentBRDF( Roughness, NoV );
    return SpecularIBL * (SpecularColor * envBRDF.x + envBRDF.y);
}

vec3 ReflectanceTerm ( Surface surface, float NoL, vec3 L, vec3 V )
{
    float Roughness = surface.Roughness;
    float a  = square( Roughness );
    float a2 = square( a );
    // float Energy = 1;
    vec3 N   = surface.Normal;
    vec3 R   = vec3(ViewToWorld * vec4(reflect( -V, N ), 0.0));
    
    vec3 H    = normalize( V + L );
    // float NoL = saturate( dot(N, L) );
    float NoV = saturate( dot(N, V) );
    float NoH = saturate( dot(N, H) );
    float VoH = saturate( dot(V, H) );

    float D   = SpecularNDF( a2, NoH );
    // float D   = 1;
    float G   = Geometric( a, NoV, NoL );
    // float G   = 1;
    vec3 F    = Fresnel( surface.Specular, VoH );

    vec3 SpecularColor = D * G * F;
    // SpecularColor = specularIBL; // <<<<

    return SpecularColor;
}

vec3 DiffuseTerm ( Surface surface )
{
    return Diffuse( surface.Albedo - surface.Albedo * surface.Metallic );
}

float SpotAttenuation( vec3 L, LightData light )
{
    vec3 direction = -light.LightDirection;
    float inner = light.LightConeAnglesAndRadius.x;
    float outer = light.LightConeAnglesAndRadius.y;
    return square(saturate( dot(L, direction) - outer ) / (outer -  inner) );
}

vec3 SurfaceShading ( Surface surface, LightData light )
{
    // vector from origin (view space) to lit point
    vec3 P  = surface.Position;
    vec3 L  = -light.LightDirection;
    vec3 N   = surface.Normal;
    // direction from lit point to the view
    vec3 V  = normalize(-P);

    float Attenuation = 1.0;

    if ( IsPositionalLight( light ) )
    {
        // vector from the current lit point to the light source
        vec3 PtoL       = light.LightPosition.xyz - P;
        float distance2 = dot( PtoL, PtoL );

        float DistanceAttenuation = DistanceAttenuationInverseSquare( distance2 );
        float RadiusMask          = MaskingRadius( distance2, light.LightConeAnglesAndRadius.z );
        Attenuation               = DistanceAttenuation * RadiusMask;

        // direction to the light
        L = normalize( PtoL );
        
        if ( IsSpotlight( light ) )
        {
            Attenuation *= SpotAttenuation( L, light);
        }
    }

    vec3 OutColor = vec3(0.0);
    vec3 DiffuseShading  = vec3(0.0);
    vec3 SpecularShading = vec3(0.0);
    vec3 DiffAndSpecIBL  = vec3(0.0);
    float NoV = saturate( dot(N, V) );

    if ( Attenuation > 0 )
    {
        float NoL = saturate( dot( surface.Normal, L ) );
        DiffuseShading  = DiffuseTerm( surface );
        SpecularShading = ReflectanceTerm( surface, NoL, L, V );

        OutColor = light.LightColor.rgb * NoL * Attenuation * (DiffuseShading + SpecularShading);
        // OutColor = SpecularShading; // <<<<
    }
    vec3 R   = vec3(ViewToWorld * vec4(reflect( -V, N ), 0.0));
    vec3 WorldNormal = vec3(ViewToWorld * vec4(N, 0.0));
    DiffAndSpecIBL += DiffuseShading * textureLod( RadianceEnvironment, WorldNormal, 5 ).rgb;
    DiffAndSpecIBL += ApproximateSpecularIBL( SpecularShading, surface.Roughness, NoV, R );

    return DiffAndSpecIBL;
}

void main()
{
    vec2 gBufferUV = 0.5 + 0.5 * ScreenPos.xy / ScreenPos.w;
    
    // the channel for albedo rgb + distance from View
    vec4 albedoCh       = texture( AlbedoTexture, gBufferUV ).rgba;
    vec4 normalCh       = vec4(texture( NormalTexture, gBufferUV ).rg, 0, 0);
    float zBufferDepth  = texture( DepthTexture, gBufferUV ).r;
    
    Surface surface = GetSurfaceAttributes( albedoCh, normalCh, zBufferDepth );
    pixelColor.rgb  = SurfaceShading ( surface, Light );

    
    // pixelColor.rgb = vec3(surface.Roughness);
    // pixelColor.rgb  += vec3(0.05, 0, 0);
    // pixelColor.rgb  = EncodeTextureNormal(surface.Position / 10);
    // pixelColor.rgb += 0.5 * EncodeTextureNormal( surface.Normal );

}

