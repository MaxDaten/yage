#version 330 core

out vec4 fragColor;
in vec4 interpolated_color;
in vec2 tex_coord;

uniform sampler2D textures;

void main()
{
    vec4 color = texture(textures, tex_coord) * interpolated_color;
    fragColor = color;
}