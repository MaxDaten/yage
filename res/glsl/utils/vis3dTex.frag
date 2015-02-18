#version 420 core
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_include : require

layout(location = 0) out vec4 gl_FragColor;
in flat int bOccupied;
void main()
{

  gl_FragColor = bOccupied == 1 ? vec4(1,0,0,1) : vec4(1, 1, 1, 0.005);
}
