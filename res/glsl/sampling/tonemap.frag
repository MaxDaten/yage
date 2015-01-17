#version 410 core
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_include : require

// http://frictionalgames.blogspot.de/2012/09/tech-feature-hdr-lightning.html
// http://http.download.nvidia.com/developer/presentations/2004/6800_Leagues/6800_Leagues_HDR.pdf
// http://filmicgames.com/archives/75

#include "sampling/sampling.h"

// 0: LinearToneMapping
// 1: ReinhardToneMapping
// 2: Uncharted2ToneMapping
#define TONE_MAPPING_TYPE 2

uniform float InverseGamma  = 1.0 / 2.2;
uniform float Exposure      = 1.0;
uniform float ExposureBias  = 0.0;
uniform float WhitePoint    = 11.2;

layout (location = 0) out vec3 pixelColor;

//------------------------------------

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
    float A = 0.15;
    float B = 0.50;
    float C = 0.10;
    float D = 0.20;
    float E = 0.02;
    float F = 0.30;
    return ((color*(A*color+C*B)+D*E) / (color*(A*color+B)+D*F))- E / F;
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
  vec2 uv = gl_FragCoord.xy / textureSize(iTextures[0], 0);
  vec3 sceneColor = texture(iTextures[0], uv).rgb;

  sceneColor     *= Exposure;

  vec3 linearColor = sceneColor;

  // add bloom
  linearColor += texture(iTextures[1], uv).rgb;

  vec3 outColor = 2.0 * ToneMapping( ExposureBias + linearColor );
  vec3 whiteScale = 1.0 / ToneMapping(vec3(WhitePoint));


  outColor *= whiteScale;
  // outColor = inverseGamma(outColor);

  pixelColor = clamp( outColor, 0, 1 );
}
