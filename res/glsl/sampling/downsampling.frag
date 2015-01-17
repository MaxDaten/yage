#version 410 core
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_include : require

#include <locations.h>
#include "sampling/sampling.h"

layout(location = FRAG_COLOR) out vec4 fragColor;

void main()
{
    vec2 size = textureSize(iTextures[0], 0);
    vec2 offset = vec2(1.0) / size;
    vec2 samplingUV = gl_FragCoord.xy * iTargetSize.zw;
    vec2 uv[4];

    uv[0] = samplingUV + offset * vec2(-1, -1);
    uv[1] = samplingUV + offset * vec2( 1, -1);
    uv[2] = samplingUV + offset * vec2(-1,  1);
    uv[3] = samplingUV + offset * vec2( 1,  1);

    vec3 sampleColor[4];
    for (uint i = 0; i < 4; ++i)
    {
        sampleColor[i] = texture( iTextures[0], uv[i] ).rgb;
    }

    fragColor.rgb = (sampleColor[0] + sampleColor[1] + sampleColor[2] + sampleColor[3]) * 0.25;
    fragColor.a = 1.0;
}
