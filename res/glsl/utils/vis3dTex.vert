#version 420 core
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_include : require

#include <common.h>

out gl_PerVertex {
vec4 gl_Position;
};

out uvec3 v_VoxelCoord;
uniform vec2 gridDim;

void main()
{
  // texel coord [0..dim)
  v_VoxelCoord.x = gl_VertexID % int(gridDim.x);
  v_VoxelCoord.y = gl_VertexID / int(gridDim.x * gridDim.x);
  v_VoxelCoord.z = (gl_VertexID / int(gridDim.x)) % int(gridDim.x);

  vec3 pos = vec3(v_VoxelCoord.x, v_VoxelCoord.y, v_VoxelCoord.z) / gridDim.x * 2.0 - 1.0;
  pos.z += gridDim.y;
  pos.x -= gridDim.y;
  gl_Position = VPMatrix * ModelMatrix * vec4(pos, 1.0);
}
