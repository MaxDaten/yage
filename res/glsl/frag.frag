#version 330

out vec4 fragColor;

void main()
{
    fragColor = vec4(gl_PointCoord.x, gl_PointCoord.y, gl_PointCoord.x * gl_PointCoord.y, 1.0f);
}