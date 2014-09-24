#version 410 core
// http://frictionalgames.blogspot.de/2012/09/tech-feature-hdr-lightning.html
// http://http.download.nvidia.com/developer/presentations/2004/6800_Leagues/6800_Leagues_HDR.pdf
// http://filmicgames.com/archives/75

#include "Sampling.frag"

// 0: LinearToneMapping
// 1: ReinhardToneMapping
// 2: Uncharted2ToneMapping
#define TONE_MAPPING_TYPE 2

uniform float InverseGamma  = 1.0 / 2.2;
uniform float Exposure      = 1.0;
uniform float ExposureBias  = 1.0;
uniform float WhitePoint    = 11.2;

const float A = 0.15;
const float B = 0.50;
const float C = 0.10;
const float D = 0.20;
const float E = 0.02;
const float F = 0.30;

layout (location = 0) out vec3 pixelColor;

//------------------------------------

vec4 ToneColor( void ) 
{
    return texture( TextureSampler0, SamplingUV0 );
}

vec3 inverseGamma(vec3 x)
{
    return pow(x, vec3(InverseGamma));
}

vec3 LinearToneMapping(vec3 color)
{
    return color;
}

vec3 ReinhardToneMapping(vec3 color)
{
    color = color / (1+color);
    return color;
}

vec3 Uncharted2ToneMapping(vec3 color)
{
    return ((color*(A*color+C*B)+D*E)/(color*(A*color+B)+D*F))-E/F;
}

vec3 ToneMapping(vec3 color)
{
#if     TONE_MAPPING_TYPE == 0
    return LinearToneMapping(color);
#elif   TONE_MAPPING_TYPE == 1
    return ReinhardToneMapping(color);
#elif   TONE_MAPPING_TYPE == 2
    return Uncharted2ToneMapping(color);
#endif
}

void main()
{
    vec3 texColor = ToneColor().rgb;

    texColor     *= Exposure;

    vec3 color = ToneMapping( ExposureBias + texColor );
    vec3 whiteScale = 1.0f / ToneMapping(vec3(WhitePoint));


    color *= whiteScale;
    color = inverseGamma( color );
    pixelColor = clamp( color, 0, 1 );
}

