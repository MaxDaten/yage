#version 410 core
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_include : require

#include <locations.h>

layout ( triangles ) in;
layout ( triangle_strip, max_vertices = 3 ) out;

in gl_PerVertex { vec4 gl_Position; } gl_in [];
out gl_PerVertex { vec4 gl_Position; int gl_Layer; };

flat in int vInstance[];

void main()
{
  gl_Layer = vInstance[0];
  gl_Position = gl_in[0].gl_Position;
  EmitVertex();
  gl_Position = gl_in[1].gl_Position;
  EmitVertex();
  gl_Position = gl_in[2].gl_Position;
  EmitVertex();
  // EndPrimitive();
}
