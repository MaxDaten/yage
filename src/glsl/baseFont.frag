#version 330 core

out vec4 fragColor;
// in vec4 interpolated_color;
in vec2 tex_coord;
uniform vec3 glyph_color = vec3(1);

uniform sampler2D textures;

// http://www.opengl.org/wiki/Texture#Swizzle_mask
void main()
{
    vec4 color = texture(textures, tex_coord);
    fragColor = color.rrrr;
}