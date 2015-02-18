// [Crassin and Green, Octree-based Sparse Voxelization, Ch. 22, OpenGL Insights]
#version 430 core
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_include : require
#extension GL_ARB_shader_image_load_store : require
#extension GL_ARB_shader_image_size : require
// #extension GL_NV_shader_atomic_fp16_vector : require
// #extension GL_NV_shader_atomic_float : require

#include <common.h>
#include <definitions.h>
#include "voxel/voxelize.h"
#include "pass/gbuffer.h"

uniform sampler2D AlbedoTexture;
uniform vec4 AlbedoColor;

uniform coherent volatile layout(binding = 0, r32ui /*rgba32ui*/ ) uimage3D VoxelAlbedo;
// uniform coherent volatile layout(binding = 1, r32ui /*rgba32ui*/ ) uimage3D VoxelAlbedoG;
// uniform coherent volatile layout(binding = 2, r32ui /*rgba32ui*/ ) uimage3D VoxelAlbedoB;
// uniform coherent volatile layout(binding = 1, rg16ui ) image3D VoxelAlbedoBA;

in vec3 Position;
in vec2 TextureCoord;
// in mat3 TangentInverse;

flat in int Axis;
flat in vec4 AABB;

void imageAtomicRGBA8Avg( vec4 val, ivec3 coord, layout(r32ui) coherent volatile uimage3D img );
/*
Surface GetSurface(void)
{
  Surface surface;
  surface.Albedo      = texture( AlbedoTexture, TextureCoord );
  surface.Roughness   = 0;
  surface.Metallic    = 0;

  surface.Normal      = vec3(0, 0, 0);

  surface.Position    = Position;
  return surface;
}
*/
void main()
{
  // discard fragments outside the triangle bound
  if (Position.x < AABB.x || Position.y < AABB.y || Position.x > AABB.z || Position.y > AABB.w)
    discard;

  ivec3 gridDim = imageSize(VoxelAlbedo);
  ivec3 tempCoord = ivec3(gl_FragCoord.x, gl_FragCoord.y, gl_FragCoord.z * gridDim.x);
  ivec3 gridCoord;

  // we need to swizzle the coords according to the view direction
  if (Axis == X_AXIS)
  {
    gridCoord.x = gridDim.x - tempCoord.z;
    gridCoord.y = tempCoord.y;
    gridCoord.z = tempCoord.x;
    // discard;
  }
  else if (Axis == Y_AXIS)
  {
    gridCoord.x = tempCoord.x;
    gridCoord.y = gridDim.x - tempCoord.z;
    gridCoord.z = tempCoord.y;
    // discard;
  }
  else // Z_AXIS
  {
    gridCoord = tempCoord;
    // discard;
  }
  // Surface surface = GetSurface();
  vec4 albedo = texture(AlbedoTexture, TextureCoord) * AlbedoColor;
  /*
  imageAtomicAdd(VoxelAlbedoRG, gridCoord, albedo.rg); // Todo atomic
  imageAtomicAdd(VoxelAlbedoBA, gridCoord, vec2(albedo.b, 1));
  */

  imageAtomicRGBA8Avg(albedo, gridCoord, VoxelAlbedo);
}

void imageAtomicRGBA8Avg( vec4 val, ivec3 coord, layout(r32ui) coherent volatile uimage3D img )
{
  val.rgb *= 255.0;
  val.a = 1;

  uint newVal = convVec4ToRGBA8( val );
  uint prev = 0;
  uint cur;

  while( (cur = imageAtomicCompSwap( img, coord, prev, newVal ) ) != prev )
   {
     prev = cur;
     vec4 rval = convRGBA8ToVec4( cur );
     rval.xyz = rval.xyz*rval.w;
     vec4 curVal = rval + val;
     curVal.xyz /= curVal.w;
     newVal = convVec4ToRGBA8( curVal );
   }
}
