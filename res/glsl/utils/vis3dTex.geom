#version 420 core
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_include : require
#extension GL_ARB_geometry_shader4 : enable


in gl_PerVertex { vec4 gl_Position; } gl_in [];
out gl_PerVertex { vec4 gl_Position; };

layout ( points ) in;
layout ( points, max_vertices = 1 ) out;

in uvec3 v_VoxelCoord[];
out vec4 f_VoxelColor;

uniform sampler3D toVis3D;
uniform vec2 gridDim;

bool isVoxelPresent(in vec4 voxel)
{
  return dot(voxel,voxel) > 0;
}

void main()
{
	vec4 voxel = texture(toVis3D, v_VoxelCoord[0] / gridDim.x).rgba;
	if (isVoxelPresent(voxel))
	{
		f_VoxelColor = voxel;
	} 
	else
	{
		f_VoxelColor = vec4(1, 1, 1, 0.005);
	}

	// gl_PointSize = gl_in[0].gl_PointSize * 2; // no working for a reason
	gl_Position = gl_in[0].gl_Position;
	EmitVertex();
	EndPrimitive();
}