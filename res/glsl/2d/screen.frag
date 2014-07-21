#version 150


in vec2 vertex_uv;

uniform sampler2D screenTex;
out vec4 fragColor;

void main()
{
    fragColor = texture(screenTex, vertex_uv);
}