#version 410 core

uniform sampler2D ElementTexture;

in vec2 TexCoord;
in vec4 Color;

layout (location = 0) out vec4 OutColor;

void main(void)
{
    OutColor = Color * texture( ElementTexture, TexCoord );
}
