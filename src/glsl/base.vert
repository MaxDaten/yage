#version 330

in          vec3    vert_position;

uniform mat4 projection_matrix;
uniform mat4 view_matrix;
uniform mat4 model_matrix;

// uniform     vec3    pos_offset      = vec3(0.0, 0.0, 0.0);
uniform     float   global_time     = 0.0;


void main()
{
    vec4 location = vec4(vert_position, 1.0);
    gl_Position = projection_matrix * view_matrix * model_matrix * vec4(vert_position, 1.0);
}