#version 150


in vec4 in_vert_position;
in vec2 in_vert_texture;

out vec2 vertex_uv;

void main()
{   
    gl_Position = in_vert_position;
    vertex_uv = in_vert_texture;
}