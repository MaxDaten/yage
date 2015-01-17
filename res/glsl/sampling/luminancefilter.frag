#version 410 core
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_include : require

#include <locations.h>
#include <color.h>
#include "sampling/sampling.h"

layout(location = FRAG_COLOR) out vec4 fragColor;
uniform float iLuminanceCutoff;

void main()
{
    vec2 uv = gl_FragCoord.xy / textureSize(iTextures[0], 0);
    vec4 color = texture( iTextures[0], uv );
    float bloom = Luminance(color) - iLuminanceCutoff;
    bloom = saturate(bloom / 2.0);

    fragColor = vec4(bloom * color, color.a);
}
