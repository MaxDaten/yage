#version 410 core

uniform sampler2D ScreenTexture0;
uniform vec4 TextureSize0;

uniform sampler2D ScreenTexture1;
uniform vec4 TextureSize1;

in vec2 VertexUV;
layout (location = 0) out vec3 pixelColor;


void main(void)
{
    TextureSize0;
    TextureSize1;
    vec3 baseColor = texture( ScreenTexture0, VertexUV ).rgb;
    vec4 addColor  = texture( ScreenTexture1, VertexUV );
    vec3 outColor  = mix( baseColor, addColor.rgb, addColor.a );
    
    pixelColor = clamp(outColor, 0, 1);
}

