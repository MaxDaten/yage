#version 410 core
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_include : require

#include <locations.h>
uniform sampler3D iTexture;

layout(location = FRAG_COLOR) out vec4 fragColor;
in int f_Layer;
void main()
{
    const vec2 size = textureSize(iTexture, 0);
    const vec2 pixel = 1.0 / size;
    const vec2 samplingUV = gl_FragCoord.xy * pixel;

    fragColor.rgb = texture( iTexture, samplingUV ).rgb;
    fragColor.a = 1.0;
}
