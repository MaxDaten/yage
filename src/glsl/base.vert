#version 330


in vec4 vert_position;
in vec3 vert_normal;
// in vec4 vert_color;

uniform mat4 projection_matrix;
uniform mat4 view_matrix;
uniform mat4 model_matrix;
uniform mat3 normal_matrix;

// uniform     vec3    pos_offset      = vec3(0.0, 0.0, 0.0);
uniform     float   global_time     = 0.0;


void main()
{
    mat3 normM = normal_matrix;
    vec3 normV = vert_normal;
    gl_Position = projection_matrix * (view_matrix * (model_matrix * vert_position));
}