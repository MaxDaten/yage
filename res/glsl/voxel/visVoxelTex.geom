#version 430 core
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_include : require
#extension GL_ARB_geometry_shader4 : enable

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

uniform sampler3D VoxelRGB;
uniform usampler3D VoxelPageMask;
uniform usampler3D VoxelBuffer;

uniform int RenderEmpty;
uniform int SampleLevel;

bool isVoxelPresent(in vec4 voxel)
{
  return voxel.a > 0;
}


void main()
{
  vec4 voxel = vec4(0,0,0,0);
  vec3 halfVox;
  ivec3 maskSize = textureSize(VoxelPageMask, 0);
  ivec3 size = textureSize(VoxelRGB, SampleLevel);
  bool pageInMarker = texture(VoxelPageMask, vec3(v_VoxelCoord[0])/ size).r == USE_PAGE_MARKER;

  if (VoxelizeMode == VOXELIZESCENE)
  {
    VoxelBuffer;
    // voxel = pageInMarker ? convRGBA8ToVec4(textureLod(VoxelBuffer, vec3(v_VoxelCoord[0]) / size, SampleLevel)) : voxel;
    halfVox = 1.0/vec3(size);
    voxel = textureLod(VoxelRGB, vec3(v_VoxelCoord[0]) / vec3(size), SampleLevel);
    // voxel = convRGBA8ToVec4(textureLod(VoxelBuffer, vec3(v_VoxelCoord[0]) / vec3(size), SampleLevel));
    // voxel.rgb /= 255.0;
  }
  else
  {
    voxel = texture(VoxelPageMask, vec3(v_VoxelCoord[0])/ maskSize).r == USE_PAGE_MARKER
              ? vec4(0.0,1.0,0.5,0.25)
              : vec4(0.0,0.0,0.0,0.0);
    halfVox = 0.95/vec3(maskSize);
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
