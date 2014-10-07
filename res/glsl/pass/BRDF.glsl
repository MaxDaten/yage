/*
    The BRDF for the physically based pipeline

    References:
    - http://seblagarde.wordpress.com/2011/08/17/feeding-a-physical-based-lighting-mode/
    - https://de45xmedrsdbp.cloudfront.net/Resources/files/2013SiggraphPresentationsNotes-26915738.pdf  
*/
#ifndef __BRDF__
#define __BRDF__


struct LightT
{
    vec3    Position;
    float   Radius;
    vec3    Color;
};


vec3 DiffuseLambert( vec3 diffuseColor )
{
    return diffuseColor / PI;
}


vec3 Diffuse( vec3 diffuseColor )
{
    return DiffuseLambert( diffuseColor );
}

// GGX / Trowbridge-Reitz
// [Walter et al. 2007, "Microfacet models for refraction through rough surfaces"]
float D_GGX( float Roughness, float NoH )
{
    float m = Roughness * Roughness;
    float m2 = m * m;
    float d = ( NoH * m2 - NoH ) * NoH + 1;
    return m2 / ( PI * d * d );
}


float SpecularNDF( float roughness, float NoH )
{
    return D_GGX( roughness, NoH );
}


// [Schlick 1994, "An Inexpensive BRDF Model for Physically-Based Rendering"]
// [Lagarde 2012, "Spherical Gaussian approximation for Blinn-Phong, Phong and Fresnel"]
vec3 FresnelSchlick( vec3 F0, float VoH )
{
    return F0 + ( 1 - F0 ) * exp2( (-5.55473 * VoH - 6.98316) * VoH );
}


vec3 Fresnel( vec3 specularColor, float VoH )
{
    return FresnelSchlick( specularColor, VoH);
}


// Tuned to match behavior of Vis_Smith
// [Karis 2013, "Real Shading in Unreal Engine 4"]
float GeometricSchlick( float Roughness, float NoV, float NoL )
{
    float k = Roughness * Roughness * 0.5;
    float GV = NoV * (1 - k) + k;
    float GL = NoL * (1 - k) + k;
    return 0.25 / ( GV * GL );
}

float Geometric ( float roughness, float NoV, float NoL)
{
    return GeometricSchlick( roughness, NoV, NoL );
}

#endif // BRDF
