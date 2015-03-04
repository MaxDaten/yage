#version 430 core
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_include : require

#include <common.h>
#include <locations.h>
uniform usampler3D iTexture;

layout(location = FRAG_COLOR) out vec4 fragColor;
void main()
{
    fragColor = convRGBA8ToVec4(texelFetch( iTexture, ivec3(gl_FragCoord.xy,gl_Layer), 0 ));
    fragColor.rgb /= 255.0;
}
