#version 330 core


in vec4 in_vert_position;
in vec3 in_vert_normal;
in vec4 in_vert_color;
in vec2 in_vert_texture;

uniform mat4 projection_matrix;
uniform mat4 view_matrix;
uniform mat4 model_matrix;
uniform mat3 normal_matrix;

// smooth out vec4 interpolated_color;
out vec2 tex_coord;

void main()
{   
    vec4 view_position = view_matrix * model_matrix * in_vert_position;
    vec4 proj_position = projection_matrix * view_position;
    gl_Position = proj_position;
    
    vec3 normV = normalize( normal_matrix * in_vert_normal );
    vec3 norm = in_vert_normal;
    vec4 interpolated_color = in_vert_color;
    tex_coord = in_vert_texture;
}