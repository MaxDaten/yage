#version 430 core
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_include : require
#extension GL_ARB_shader_image_load_store : require
#extension GL_ARB_geometry_shader4 : enable
#extension GL_ARB_shader_image_size : require

#include <common.h>
#include <definitions.h>
#include "voxel/voxelize.h"

in gl_PerVertex { vec4 gl_Position; } gl_in [];
out gl_PerVertex { vec4 gl_Position; };

layout ( points ) in;
// layout ( points, max_vertices = 1 ) out;
layout ( triangle_strip, max_vertices = 26 ) out;

in ivec3 v_VoxelCoord[];
out vec4 f_VoxelColor;

uniform readonly layout(binding = 0, r32ui) uimage3D VoxelBuffer;
uniform readonly layout(binding = 1, r8ui) uimage3D VoxelPageMask;

uniform int RenderEmpty;


bool isVoxelPresent(in vec4 voxel)
{
  return dot(voxel,voxel) > 0;
}


void main()
{
  vec4 voxel;
  vec3 halfVox;
  if (VoxelizeMode == VOXELIZESCENE)
  {
    voxel = convRGBA8ToVec4(imageLoad(VoxelBuffer, v_VoxelCoord[0]));
	  voxel.rgb /= 255.0;
    halfVox = 0.9/imageSize(VoxelBuffer);
  }
  else
  {
    bool pageInMarker = imageLoad(VoxelPageMask, v_VoxelCoord[0]).r == USE_PAGE_MARKER;
    voxel = pageInMarker ? vec4(1.0,0.5,0.0,0.3) : vec4(0.0,0.0,0.0,0.0);
    halfVox = 0.8/imageSize(VoxelPageMask);
  }

	if (isVoxelPresent(voxel))
	{
		f_VoxelColor = voxel;
	}
	else if (RenderEmpty == 1)
	{
		f_VoxelColor = vec4(1, 1, 1, 0.005);
	}
  else
  {
    return;
  }

  const vec4 center = gl_in[0].gl_Position;
  vec4 pos = center;

  pos.xyz = center.xyz + vec3(-1,-1,1) * halfVox;
  gl_Position =  VPMatrix * ModelMatrix * pos;
  EmitVertex();

  pos.xyz = center.xyz + vec3(1,-1,1) * halfVox;
  gl_Position =  VPMatrix * ModelMatrix * pos;
  EmitVertex();

  pos.xyz = center.xyz + vec3(-1,1,1) * halfVox;
  gl_Position =  VPMatrix * ModelMatrix * pos;
  EmitVertex();

  pos.xyz = center.xyz + vec3(1,1,1) * halfVox;
  gl_Position =  VPMatrix * ModelMatrix * pos;
  EmitVertex();

  //+X
  //for degenerate purpose
  EmitVertex();
  //f_color = vec4( 0.2, 0.2, 0.2, 1 );
  pos.xyz = center.xyz + vec3(1,-1,1) * halfVox;
  gl_Position =  VPMatrix * ModelMatrix * pos;
  EmitVertex();

  pos.xyz = center.xyz + vec3(1,1,-1) * halfVox;
  gl_Position =  VPMatrix * ModelMatrix * pos;
  EmitVertex();

  pos.xyz = center.xyz + vec3(1,-1,-1) * halfVox;
  gl_Position =  VPMatrix * ModelMatrix * pos;
  EmitVertex();


  //-Z
  EmitVertex(); //for degenerate purpose

  pos.xyz = center.xyz + vec3(-1,-1,-1) * halfVox;
  gl_Position =  VPMatrix * ModelMatrix * pos;
  EmitVertex();

  pos.xyz = center.xyz + vec3(1,1,-1) * halfVox;
  gl_Position =  VPMatrix * ModelMatrix * pos;
  EmitVertex();

  pos.xyz = center.xyz + vec3(-1,1,-1) * halfVox;
  gl_Position =  VPMatrix * ModelMatrix * pos;
  EmitVertex();

  //-X
  EmitVertex(); //for degenerate purpose
  //f_color = vec4( 0.5, 0.5, 0.5, 1 );
  pos.xyz = center.xyz + vec3(-1,-1,-1) * halfVox;
  gl_Position =  VPMatrix * ModelMatrix * pos;
  EmitVertex();

  pos.xyz = center.xyz + vec3(-1,1,1) * halfVox;
  gl_Position =  VPMatrix * ModelMatrix * pos;
  EmitVertex();

  pos.xyz = center.xyz + vec3(-1,-1,1) * halfVox;
  gl_Position =  VPMatrix * ModelMatrix * pos;
  EmitVertex();

  //-Y
  EmitVertex();

  pos.xyz = center.xyz + vec3(-1,-1,-1) * halfVox;
  gl_Position =  VPMatrix * ModelMatrix * pos;
  EmitVertex();

  pos.xyz = center.xyz + vec3(1,-1,1) * halfVox;
  gl_Position =  VPMatrix * ModelMatrix * pos;
  EmitVertex();

  pos.xyz = center.xyz + vec3(1,-1,-1) * halfVox;
  gl_Position =  VPMatrix * ModelMatrix * pos;
  EmitVertex();

  //+Y
  EmitVertex();
  //f_color = color;
  pos.xyz = center.xyz + vec3(1,1,-1) * halfVox;
  gl_Position =  VPMatrix * ModelMatrix * pos;
  EmitVertex();
  EmitVertex();

  pos.xyz = center.xyz + vec3(-1,1,-1) * halfVox;
  gl_Position =  VPMatrix * ModelMatrix * pos;
  EmitVertex();

  pos.xyz = center.xyz + vec3(1,1,1) * halfVox;
  gl_Position =  VPMatrix * ModelMatrix * pos;
  EmitVertex();

  pos.xyz = center.xyz + vec3(-1,1,1) * halfVox;
  gl_Position =  VPMatrix * ModelMatrix * pos;
  EmitVertex();

  EmitVertex();

  EndPrimitive();
}

/*
  const vec3 faces[6] =
    ( vec3(1,0,0), vec3(-1,0,0)    // x faces
    , vec3(0,1,0), vec3(0,-1,0)    // y faces
    , vec3(0,0,1), vec3(0,0,-1));  // z faces

  for (int i = 0; i < 6; i++)
  {
    vec3 face = faces[i];
    pos = center.xyz + face * halfVox;
    gl_Position = VPMatrix * ModelMatrix * pos;
    EmitVertex();

    gl_Position = VPMatrix * ModelMatrix * pos;
    EmitVertex();

    gl_Position = VPMatrix * ModelMatrix * pos;
    EmitVertex();

    gl_Position = VPMatrix * ModelMatrix * pos;
    EmitVertex();
    EmitVertex();
  }
  EndPrimitive();
*/
