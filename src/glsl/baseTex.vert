#version 330 core


in vec4 in_vert_position;
in vec3 in_vert_normal;
in vec4 in_vert_color;
in vec2 in_vert_texture;

uniform mat4 projection_matrix;
uniform mat4 view_matrix;
uniform mat4 model_matrix;
uniform mat3 normal_matrix;

uniform vec3 global_light_direction = normalize(vec3(0.0, -1.0, -1.0));
uniform vec3 global_light_intensity = vec3(1.0, 1.0, 1.0);

uniform float global_time     = 0.0;

smooth out vec4 interpolated_color;
out vec2 tex_coord;

void main()
{   
    vec4 view_position = view_matrix * model_matrix * in_vert_position;
    vec4 proj_position = projection_matrix * view_position;
    gl_Position = proj_position;
    
    vec3 normV = normalize( normal_matrix * in_vert_normal );

    float cosLight = clamp(dot((-1.0) * normV, global_light_direction), 0, 1);

    vec4 baseColor = in_vert_color;
    interpolated_color = vec4(global_light_intensity * cosLight * baseColor.rgb, baseColor.a);

    tex_coord = in_vert_texture;
}