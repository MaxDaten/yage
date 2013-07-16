#version 330

in vec3 position;
// uniform vec3 initialUniform = vec3(1.0, 0.0, 0.0);

void main()
{
    gl_Position = vec4(position, 1.0);
}