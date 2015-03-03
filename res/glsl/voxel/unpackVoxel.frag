#version 430 core
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_include : require

#include <common.h>
#include <locations.h>
uniform usampler3D iTexture;

layout(location = FRAG_COLOR) out vec4 fragColor;
void main()
{
    const vec3 size = textureSize(iTexture, 0);
    const vec3 pixel = 1.0 / size;
    const vec3 samplingUV = vec3(gl_FragCoord.xy,gl_Layer) * pixel;

    fragColor = convRGBA8ToVec4(texture( iTexture, samplingUV )) / 255.0;
}
