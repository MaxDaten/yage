// [Crassin and Green, Octree-based Sparse Voxelization, Ch. 22, OpenGL Insights]
#version 430 core
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_include : require
#extension GL_ARB_shader_image_load_store : require
#extension GL_ARB_shader_image_size : enable

#include <common.h>
#include <definitions.h>
#include "pass/voxelize.h"
#include "pass/gbuffer.h"

// uniform sampler2D AlbedoTexture;
// uniform vec4 AlbedoColor;

uniform layout(binding = 0, rgba8 ) image3D VoxelAlbedo;

in vec3 Position;
// in vec2 TextureCoord;
// in mat3 TangentInverse;

// flat in vec4 AABB;
flat in int Axis;

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
/*   if (Position.x < AABB.x || Position.y < AABB.y || Position.x > AABB.z || Position.y > AABB.w)
    discard;
 */
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
  imageStore(VoxelAlbedo, gridCoord, vec4(1, 1, 1, 1)); // Todo atomic
}
