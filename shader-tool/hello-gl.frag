#version 410

uniform float fade_factor;

// interface blocks not supported
// http://www.opengl.org/wiki/Interface_Block_(GLSL)
/*
uniform ABlock
{
    float some_var;
    vec3 another_var[4];
} blocks[4];
*/

struct Tex
{
    vec2 uv[4];
    vec2 size;
};

uniform Tex singleData;
uniform Tex textureData[2];
uniform sampler2D textures[2];
in vec2 texcoord;

layout (location = 0) out vec4 pixelColor;

void main()
{
    textureData[0].uv;
    textureData[1].uv;
    singleData;
    // blocks[0].some_var; // interface blocks not supported
    pixelColor = mix(
        texture(textures[0], texcoord),
        texture(textures[1], texcoord),
        fade_factor);
}
