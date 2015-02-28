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

uniform coherent volatile layout(binding = 0, r32ui) uimage3D VoxelBuffer;
uniform coherent volatile layout(binding = 1, r8ui) uimage3D PageMask;

in vec4 Position;
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
const bool DebugAxis = true;
void main()
{
  // discard fragments outside the triangle bound
  if (Position.x < AABB.x || Position.y < AABB.y || Position.x > AABB.z || Position.y > AABB.w)
    discard;

  vec3 AxisColor = vec3(0);
  const ivec3 gridDim = (VoxelizeMode == VOXELIZESCENE ? imageSize(VoxelBuffer) : imageSize(PageMask)) -1;

  ivec3 tempCoord = ivec3(gl_FragCoord.x, gl_FragCoord.y, gl_FragCoord.z);
  ivec3 gridCoord;

  // we need to swizzle the coords according to the view direction
  if (Axis == X_AXIS)
  {
    tempCoord.z = int(round(gl_FragCoord.z * gridDim.x));
    // gridCoord.xyz = tempCoord.zyx;
    gridCoord.xyz = ivec3(gridDim.x - tempCoord.z, tempCoord.y, gridDim.z - tempCoord.x);
    // map depth because our grid has no uniform dimensions
    // discard;
    AxisColor.r = 1.0;
  }
  else if (Axis == Y_AXIS)
  {
    tempCoord.z = int(round(gl_FragCoord.z * gridDim.y));
    gridCoord.xyz = ivec3(tempCoord.x, gridDim.y - tempCoord.z, gridDim.z - tempCoord.y);
    AxisColor.g = 1.0;
    // discard;
  }
  else // Z_AXIS
  {
    tempCoord.z = gridDim.z - int(round(gl_FragCoord.z * gridDim.z));
    gridCoord = tempCoord;
    // discard;
    AxisColor.b = 1.0;
  }

  if (VoxelizeMode == VOXELIZESCENE)
  {
    // Surface surface = GetSurface();
    vec4 albedo = texture(AlbedoTexture, TextureCoord) * AlbedoColor;
    albedo.rgb = DebugAxis ? AxisColor : albedo.rgb;
    imageAtomicRGBA8Avg(albedo, gridCoord, VoxelBuffer);
  }
  else
  {
    imageStore(PageMask, gridCoord, uvec4(USE_PAGE_MARKER,0,0,0));
  }

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
