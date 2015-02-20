#version 430 core
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_include : require
#extension GL_ARB_shader_image_load_store : require
#extension GL_ARB_geometry_shader4 : enable

#include <common.h>
#include "voxel/voxel.h"

in gl_PerVertex { vec4 gl_Position; } gl_in [];
out gl_PerVertex { vec4 gl_Position; };

layout ( points ) in;
layout ( points, max_vertices = 1 ) out;

in ivec3 v_VoxelCoord[];
out vec4 f_VoxelColor;

uniform readonly layout(binding = 0, r32ui /*rgba32ui*/ ) uimage3D toVis3D;
uniform vec2 gridDim;

void main()
{
	vec4 voxel = convRGBA8ToVec4(imageLoad(toVis3D, v_VoxelCoord[0]));
	voxel.rgb /= 255.0;

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
