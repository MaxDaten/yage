/*
    a lighting model for physically based rendering
    calculation is done in view-space 



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

// [Karis 2013, "Real Shading in Unreal Engine 4"]
float RadialFalloff( float distance2, float radius )
{
    float L0 = pow( sqrt(distance2) / radius, 4 );
    L0 = pow( saturate( 1 - L0), 2);
    return L0 / (distance2 + 1); 
}

vec3 PointLightSpecular ( Surface surface, vec3 VtoL, vec3 V )
{
    float Roughness = surface.Roughness;
    float a  = Roughness * Roughness;
    float a2 = a * a;
    float Energy = 1;
    vec3 N   = surface.Normal;
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
    vec3 F    = Fresnel( surface.Specular, VoH );

    return (Energy * D * G) * F;
}

vec3 PointLightDiffuse ( Surface surface )
{
    return Diffuse( surface.Albedo );
}

vec3 CalculateLighting ( Surface surface, LightT light )
{
    vec3 P  = surface.Position;
    // vector from lit point to the view
    vec3 V  = normalize(-P);

    // direction from the current lit pixel to the light source
    vec3 PtoL   = light.Position - P;
    float dist2 = dot( PtoL, PtoL );

    vec3 DiffuseTerm = PointLightDiffuse( surface );
    vec3 SpecTerm    = PointLightSpecular( surface, PtoL, V );

    vec3 OutColor;
    OutColor.rgb = light.Color.rgb * DiffuseTerm;
    OutColor.rgb += light.Color.rgb * SpecTerm;
    OutColor.rgb *= RadialFalloff( dist2, light.Radius );
    return OutColor;
}

void main()
{
    vec2 screenUV = gl_FragCoord.xy / ViewportDim.xy;
    
    // the channel for albedo rgb + distance from View
    vec4 albedoCh       = texture( AlbedoTexture, screenUV ).rgba;
    vec4 normalCh       = vec4(texture( NormalTexture, screenUV ).rg, 0, 0);
    float zBufferDepth  = texture( DepthTexture, screenUV ).r;

    
    Surface surface = GetSurfaceAttributes( albedoCh, normalCh, zBufferDepth );
    pixelColor.rgb  = CalculateLighting ( surface, Light );
    
    // pixelColor.rgb  = vec3(0.5, 0, 0);
    // pixelColor.rgb = EncodeTextureNormal( surface.Normal );

    EnvironmentCubeMap;    
}

