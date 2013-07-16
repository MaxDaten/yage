#version 330

in vec3 position;
uniform vec3 offset = vec3(0.0, 0.0, 0.0);

void main()
{
    gl_Position = vec4(position + offset, 1.0);
}