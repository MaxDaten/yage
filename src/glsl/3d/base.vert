#version 330 core


in vec4 in_vert_position;
in vec3 in_vert_normal;
in vec4 in_vert_color;

uniform mat4 mvp_matrix;
uniform mat4 modelview_matrix;
uniform mat3 normal_matrix;


smooth out vec4 interpolated_color;
out vec3 vertex_normal;
out vec3 vertex_position;

void main()
{
    vertex_position    = vec3( modelview_matrix * in_vert_position );
    vertex_normal      = normalize( normal_matrix * in_vert_normal );
    interpolated_color = in_vert_color;
    gl_Position        = mvp_matrix * in_vert_position;
}