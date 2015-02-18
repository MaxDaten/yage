// [Crassin and Green, Octree-based Sparse Voxelization, Ch. 22, OpenGL Insights]
#version 430 core
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_include : require

#include <common.h>
#include <attributes.h>

out gl_PerVertex { vec4 gl_Position; };

// naturally in model-space
layout(location = VPOSITION) in vec3 vPosition;
// layout(location = VTEXTURE)  in vec2 vTexture;
// layout(location = VTANGENTX) in vec3 vTangentX;
// layout(location = VTANGENTZ) in vec4 vTangentZ;

// out vec3 v_position;
// out vec2 v_textureCoord;
// out vec3 v_tangentX;
// out vec4 v_tangentZ;


void main()
{
  // v_tangentX        = a_TangentX;
  // v_tangentZ        = a_TangentZ;
  // v_position        = vec3(ModelMatrix * vec4(a_Position, 1.0));
  // v_textureCoord    = a_Texture;
  gl_Position       = ModelMatrix * vec4(vPosition, 1.0);
}
