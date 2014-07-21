#version 330

out vec4 fragColor;
in vec4 interpolated_color;
in vec3 normal;

void main()
{
    fragColor = interpolated_color;
}