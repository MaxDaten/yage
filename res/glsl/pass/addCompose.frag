#version 410 core

uniform sampler2D BaseTexture;
uniform float BaseWeight = 1.0;

uniform sampler2D AddTexture;
uniform float AddWeight = 1.0;
uniform vec4 TextureSize; // w, h, 1/w, 1/h

in vec2 VertexUV;
layout (location = 0) out vec3 pixelColor;

void main()
{
    TextureSize;
    vec3 texColor = BaseWeight * texture( BaseTexture, VertexUV ).rgb;
    texColor     += AddWeight  * texture( AddTexture, VertexUV).rgb;
    
    pixelColor = texColor;
}

