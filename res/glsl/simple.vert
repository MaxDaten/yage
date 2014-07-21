#version 150


in vec4 in_vert_position;
in vec3 in_vert_normal;
in vec4 in_vert_color;

smooth out vec4 interpolated_color;

void main()
{   
    gl_Position = in_vert_position;
    

    interpolated_color = vec4(1.0, 0.0, 0.0, 0.0);
}