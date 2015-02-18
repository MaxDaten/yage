#version 420 core
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_include : require
#extension GL_ARB_geometry_shader4 : enable


in gl_PerVertex { vec4 gl_Position; float gl_PointSize;} gl_in [];
out gl_PerVertex { vec4 gl_Position; float gl_PointSize;};

layout ( points ) in;
layout ( points, max_vertices = 1 ) out;

in uvec3 v_VoxelCoord[];
out flat int bOccupied[];

uniform sampler3D toVis3D;
uniform vec2 gridDim;

bool isVoxelPresent(in uvec3 coord)
{
  vec4 val = texture(toVis3D, coord / gridDim.x).rgba;
  return length(val) > 0;
}

void main()
{
	bOccupied[0] = isVoxelPresent(v_VoxelCoord[0]) ? 1 : 0;

	gl_PointSize = gl_in[0].gl_PointSize * 2;
	gl_Position = gl_in[0].gl_Position;
	EmitVertex();
	EndPrimitive();
}