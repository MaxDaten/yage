#version 410 core

uniform sampler2D ElementTexture;

in vec2 TextureCoords;
in vec4 Color;

layout (location = 0) out vec4 fragColor;

vec4 ElementColor(void)
{
    float fontColor = texture( ElementTexture, TextureCoords ).r;
    return Color * vec4(fontColor);
}

void main(void)
{
    fragColor = ElementColor();
    // fragColor = vec4(1.0, 0.0, 0.0, 1.0);
}

