#version 410 core

uniform sampler2D DownsampleTexture;
uniform vec4 TextureSize; // w, h, 1/w, 1/h

in vec2 VertexUV;
layout (location = 0) out vec3 pixelColor;

void main()
{
    vec2 offset = TextureSize.zw;
    vec2 uv[4];

    uv[0] = VertexUV + offset * vec2(-1, -1);
    uv[1] = VertexUV + offset * vec2( 1, -1);
    uv[2] = VertexUV + offset * vec2(-1,  1);
    uv[3] = VertexUV + offset * vec2( 1,  1);
    
    vec3 sampleColor = vec3(0);
    for (uint i = 0; i < 4; ++i)
    {
        sampleColor += texture( DownsampleTexture, uv[i] ).rgb;
    }

    pixelColor = sampleColor * 0.25;
}

