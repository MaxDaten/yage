#version 420 core
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_include : require

#include <common.h>

out gl_PerVertex {
vec4 gl_Position;
};

out ivec3 v_VoxelCoord;
uniform vec3 gridDim;

void main()
{
  // texel coord [0..dim)
  v_VoxelCoord.x = gl_VertexID % int(gridDim.x);
  v_VoxelCoord.y = gl_VertexID / int(gridDim.x * gridDim.z);
  v_VoxelCoord.z = (gl_VertexID / int(gridDim.x)) % int(gridDim.z);

  vec3 pos = vec3(v_VoxelCoord.x, v_VoxelCoord.y, v_VoxelCoord.z) / gridDim * 2.0 - 1.0;
  pos.z += 1.0 / gridDim.z;
  pos.x -= 1.0 / gridDim.x;
  gl_Position = vec4(pos, 1.0);
}
