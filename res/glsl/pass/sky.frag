#version 410 core
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_include : require

#include <common.h>
#include <definitions.h>

uniform samplerCube SkyTexture;
uniform vec4 SkyColor;
in vec3 VertexSTP;

layout (location = 0) out vec4 pixelColor;

void main()
{
  pixelColor = textureLod(SkyTexture, VertexSTP, 0);
}
