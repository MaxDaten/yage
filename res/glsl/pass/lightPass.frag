#version 410 core

#include "Common.glsl"
#include "GBuffer.glsl"

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



const float f0 = pow((1.0-(1.0/1.31)), 2)/pow((1.0+(1.0/1.31)), 2); 
float Fresnel( vec3 halfDir, vec3 viewDir, float f0 );


// GGX / Trowbridge-Reitz
// [Walter et al. 2007, "Microfacet models for refraction through rough surfaces"]
float D_GGX( float Roughness, float NoH )
{
    float m = Roughness * Roughness;
    float m2 = m * m;
    float d = ( NoH * m2 - NoH ) * NoH + 1; // 2 mad
    return m2 / ( PI*d*d );                 // 3 mul, 1 rcp
}



void main()
{
    vec2 screenUV = gl_FragCoord.xy / ViewportDim.xy;
    
    // the channel for albedo rgb + distance from View
    vec4 albedoCh = texture( AlbedoTexture, screenUV ).rgba;
    
    // the lit pixel albedo color 
    vec3 Albedo       = gamma( albedoCh.rgb, Gamma ).rgb;
    float Roughness   = albedoCh.a;

    // retrieve the normal of the lit pixel
    vec3 normalVS  = DecodeNormal( texture( NormalTexture, screenUV ).rg );

    // extrapolate the View space position of the pixel to the zFar plane
    vec3 pixelPosVS = PositionVSFromDepth( texture( DepthTexture, screenUV ).r, ZProjRatio, VertexPosVS );

    // world light position to View space
    vec3 lightPosVS = vec3( ViewSpace * vec4( lightPosition, 1.0 ) );

    // direction from the lit pixel to the light source
    vec3 pixToLight = lightPosVS - pixelPosVS;
    float dist      = length(pixToLight);
    
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
    pixelColor.rgb  = Albedo * attenuation * ( lambertian * lightColor.rgb + specular * lightColor.rgb );
    
    vec3 reflectedDirection = normalize( ViewToWorldMatrix * reflect( -toViewDir, normalVS ) );

    pixelColor.rgb += 0.25 * ( 1.0 - Roughness ) * texture( EnvironmentCubeMap, reflectedDirection ).rgb;
}

/*
float Fresnel( vec3 halfDir, vec3 viewDir, float f0 )
{
    float base = 1 - dot(viewDir, halfDir);
    float exponential = pow(base, 5.0);
    return exponential + f0 * (1.0 - exponential);
}
*/
