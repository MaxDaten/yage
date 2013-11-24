#version 330 core


in vec2 in_vert_position;
in vec2 in_vert_texture;
in vec4 in_vert_color;

uniform mat4 mvp_matrix = mat4(0.0);
uniform mat4 modelview_matrix = mat4(0.0);
uniform mat3 normal_matrix = mat3(0.0);

smooth out vec4 interpolated_color;
out vec2 vertex_uv;

void main()
{   
    gl_Position         = mvp_matrix * vec4(in_vert_position, 0.0, 1.0);    
    interpolated_color  = in_vert_color;
    vertex_uv           = in_vert_texture;
}