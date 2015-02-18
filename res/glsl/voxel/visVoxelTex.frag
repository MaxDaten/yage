#version 420 core
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_include : require

layout(location = 0) out vec4 gl_FragColor;
in vec4 f_VoxelColor;
void main()
{
  gl_FragColor = f_VoxelColor;
}
