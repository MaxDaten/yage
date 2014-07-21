#version 410 core

uniform sampler2D FilterTexture;
uniform vec4 TextureSize; // w, h, 1/w, 1/h
uniform float WhitePoint;

in vec2 VertexUV;
layout (location = 0) out vec3 pixelColor;

void main()
{
    TextureSize;
    vec3 texColor = texture( FilterTexture, VertexUV ).rgb;
    vec3 color = vec3(0.0);
    
    if (length(texColor) >= WhitePoint)
    {
        color = texColor;
    }

    pixelColor = color;
}

