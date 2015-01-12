#version 410 core
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_include : require
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

#include <common.h>
#include <brdf.h>
#include "pass/gbuffer.h"


uniform samplerCube RadianceEnvironment;
uniform vec3 CameraPosition;

in vec4 ScreenPos;
// in Light iLight;


// Light in view space (Position, Direction, etc)
uniform LightData Light;


layout (location = 0) out vec4 pixelColor;



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

vec3 ReflectanceTerm ( Surface surface, vec3 L, vec3 V )
{
    float Roughness = surface.Roughness;
    float a  = square( Roughness );
    float a2 = square( a );
    // float Energy = 1;
    vec3 N   = surface.Normal;

    vec3 H    = normalize( V + L );
    float NoL = saturate( dot(N, L) );
    float NoV = abs(dot(N, V)) + 1e-5f; // avoid artifact [Lagarde & Rousiers 2014, Moving Frostbite to Physically Based Rendering, S.12]
    float NoH = saturate( dot(N, H) );
    float VoH = saturate( dot(V, H) );
    float LoH = saturate( dot(L, H) );

    float D   = SpecularNDF( a2, NoH );
    float G   = Geometric( a, NoV, NoL );
    vec3 F    = Fresnel( surface.Specular, 1, LoH ) ; // or VoH (is the same ;)

    return D * G * F;
}

vec3 DiffuseTerm ( Surface surface )
{
    return Diffuse( surface.Albedo.rgb );
}

float SpotAttenuation( vec3 L, LightData light )
{
    vec3 direction = light.LightDirection;
    float inner = light.LightConeAnglesAndRadius.x;
    float outer = light.LightConeAnglesAndRadius.y;
    return square(saturate( dot(L, direction) - outer ) / (outer -  inner) );
}

vec3 SurfaceShading ( Surface surface, LightData light )
{
    // vector from origin (view space) to lit point
    vec3 P  = surface.Position;
    vec3 L  = -light.LightDirection;
    vec3 N  = surface.Normal;

    // direction from lit point to the view
    vec3 V  = normalize( CameraPosition - P );

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
        L = PtoL / sqrt( distance2 );

        if ( IsSpotlight( light ) )
        {
            Attenuation *= SpotAttenuation( L, light);
        }
    }

    vec3 OutColor = vec3(0.0);
    vec3 DiffuseShading  = vec3(0.0);
    vec3 SpecularShading = vec3(0.0);
    float NoL = saturate( dot( N, L ) );
    float NoV = saturate( dot(N, V) );

    if ( Attenuation > 0 )
    {
        DiffuseShading  = DiffuseTerm( surface );
        SpecularShading = ReflectanceTerm( surface, L, V );
        OutColor = light.LightColor.rgb * NoL * Attenuation * (DiffuseShading + SpecularShading); // <<<<
    }
    vec3 R   = reflect( -V, N );

    //...
    // TODO : MaxMipLevel & ViewToWorld & Metalness to uniform
    vec3 DiffuseAmbient = surface.Albedo.rgb * textureLod( RadianceEnvironment, N, 5 ).rgb;
    vec3 SpecularAmbient = ApproximateSpecularIBL( surface.Specular, surface.Roughness, NoV, R );;

    OutColor += DiffuseAmbient;
    OutColor += SpecularAmbient;

    return OutColor;
}

void main()
{
    vec2 gBufferUV = 0.5 + 0.5 * ScreenPos.xy / ScreenPos.w;

    Surface surface = DecodeGBuffer( gBufferUV );
    pixelColor.rgb  = SurfaceShading ( surface, Light );

    // pixelColor.rgb = vec3(surface.Roughness);
    // pixelColor.rgb  += vec3(0.05, 0, 0);
    // pixelColor.rgb  = EncodeTextureNormal(surface.Position / 10);
    // pixelColor.rgb += 0.5 * EncodeTextureNormal( surface.Normal );

}

