#version 330 core

out vec4 fragColor;

in vec2 vertex_uv;

uniform vec3 glyph_color = vec3(1, 0, 0);
uniform sampler2D textures;

// http://www.opengl.org/wiki/Texture#Swizzle_mask
void main()
{
    vec4 color = texture(textures, vertex_uv);
    fragColor = vec4(glyph_color, color.r);
}