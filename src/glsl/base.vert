#version 330


in vec4 in_vert_position;
in vec3 in_vert_normal;
in vec4 in_vert_color;

uniform mat4 projection_matrix;
uniform mat4 view_matrix;
uniform mat4 model_matrix;
uniform mat3 normal_matrix;

uniform vec4 global_light_direction = normalize(vec4(0.0, -1.0, -1.0, 0.0));
uniform vec4 global_light_intensity = vec4(1.0, 1.0, 1.0, 1.0);

// uniform     vec3    pos_offset      = vec3(0.0, 0.0, 0.0);
uniform     float   global_time     = 0.0;

smooth out vec4 interpolated_color;

void main()
{   
    float tT = global_time*10;
    vec4 add_pos = vec4(20 * sin(tT), 20 * sin(tT+10), 20 * cos(tT+20), 0.0);
    vec4 view_position = view_matrix * model_matrix * (in_vert_position + add_pos);
    vec4 proj_position = projection_matrix * view_position;
    gl_Position = proj_position;
    
    vec3 normV = normalize( normal_matrix * in_vert_normal );

    vec3 dirToLight = (global_light_direction).xyz;
    float cosLight = clamp(dot((-1.0) * normV, dirToLight), 0, 1);

    interpolated_color = global_light_intensity * cosLight * in_vert_color;
}