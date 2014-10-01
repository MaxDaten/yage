/*
    a lighting model for physically based rendering
    calculation is done in view-space 
*/
#version 410 core

#include "Common.glsl"
#include "GBuffer.glsl"
#include "BRDF.glsl"

uniform vec2 ZProjRatio;
uniform mat3 ViewToWorldMatrix;

uniform sampler2D AlbedoTexture;
uniform sampler2D NormalTexture;
uniform sampler2D DepthTexture;
uniform samplerCube EnvironmentCubeMap;
uniform float Gamma = 2.2;

// lightPosition is in WorldSpace
uniform vec3 lightPosition;    
uniform vec3 lightRadius;    
uniform vec4 lightColor;
// x = constant, y = inv linear, z = inv square
uniform vec3 lightAttenuation;
uniform float lightSpecularExp;

uniform ivec2 ViewportDim;

in mat4 ViewSpace;
in vec3 VertexPosVS;


layout (location = 0) out vec4 pixelColor;

// the position of the current fragment in view space
vec3 GetPosition( vec2 uv )
{
    return PositionVSFromDepth( texture( DepthTexture, uv ).r
                              , ZProjRatio
                              , VertexPosVS
                              );
}

vec3 GetNormal( vec2 uv )
{
    return DecodeNormal( texture( NormalTexture, uv ).rg );
}

float GetRoughness( vec4 channel )
{
    return channel.a;
}

vec3 GetSpecular( vec4 channel )
{
    return vec3(0.5); // TODO : remove default 
}

vec3 PointLightSpecular ( vec4 channel, vec3 VtoL, vec3 V, vec3 N )
{
    float Roughness = GetRoughness( channel );
    float a  = Roughness * Roughness;
    float a2 = a * a;
    float Energy = 0.2;
    vec3 R   = reflect(-V, N);
    
    vec3 L    = normalize( VtoL );
    vec3 H    = normalize( V + L );
    float NoL = saturate( dot(N, L) );
    float NoV = saturate( dot(N, V) );
    float NoH = saturate( dot(N, H) );
    float VoH = saturate( dot(V, H) );

    float D   = SpecularNDF( Roughness, NoH );
    float G   = Geometric( Roughness, NoV, NoL );

     // TODO: replace roughness with real spec texture
    vec3 F    = Fresnel( GetSpecular( channel ), VoH );

    return (Energy * D * G) * F;
}

vec3 PointLightDiffuse ( vec4 channel )
{
    return Diffuse( channel.rgb );
}

void main()
{
    vec2 screenUV = gl_FragCoord.xy / ViewportDim.xy;
    
    // the channel for albedo rgb + distance from View
    vec4 albedoCh     = texture( AlbedoTexture, screenUV ).rgba;
    
    // retrieve the normal of the pixel to lit
    vec3 N  = GetNormal( screenUV );

    // extrapolate the View space position of the pixel to the zFar plane
    vec3 P  = GetPosition( screenUV );
    vec3 V  = -P;

    // world light position to View space
    vec3 L = vec3( ViewSpace * vec4( lightPosition, 1.0 ) );

    // direction from the lit pixel to the light source
    vec3 VtoL = L - V;
    float dist      = length(VtoL);

    vec3 DiffuseLight = PointLightDiffuse( albedoCh );
    vec3 SpecLight    = PointLightSpecular( albedoCh, VtoL, V, N );

    pixelColor.rgb = lightColor.rgb * DiffuseLight;
    pixelColor.rgb += lightColor.rgb * SpecLight;

    ViewToWorldMatrix;
    lightRadius;
    lightAttenuation;
    lightSpecularExp;
    EnvironmentCubeMap;
/*
    vec3 toLightDir = pixToLight / dist;
    // direction from pixel to View
    vec3 toViewDir    = normalize(-pixelPosVS);

    float lambertian = saturate( dot(normalVS, toLightDir) );
    float specular = 0.0;
    if (lambertian > 0.0) {
        vec3 halfDir    = normalize( toLightDir + toViewDir );
        float specAngle = saturate( dot( halfDir, normalVS ) );

        lightSpecularExp;
        // specular = D_GGX(pow(max(abs(specAngle),0.000001f), lightSpecularExp));
        specular = 0.25 * D_GGX(Roughness, specAngle);
        // specular = pow(specAngle, lightSpecularExp);
    }

    float curve = min(pow(dist / lightRadius.x, 6.0), 1.0);
    float invSquare = 1.0 / ( 1.0 + lightAttenuation.x +                // constant
                                    lightAttenuation.y * dist +         // inv linear
                                    lightAttenuation.z * dist * dist    // inv quadric
                            );
    float attenuation = mix( invSquare, 0.0, curve );
    
    pixelColor.a    = 1.0;
    pixelColor.rgb  = Diffuse( Albedo ) * attenuation * ( lambertian * lightColor.rgb + specular * lightColor.rgb );
    
    vec3 reflectedDirection = normalize( ViewToWorldMatrix * reflect( -toViewDir, normalVS ) );

    pixelColor.rgb += 0.25 * ( 1.0 - Roughness ) * texture( EnvironmentCubeMap, reflectedDirection ).rgb;
*/    
}

