// [Crassin and Green, Octree-based Sparse Voxelization, Ch. 22, OpenGL Insights]
#version 430 core
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_include : require

#include <common.h>
#include <definitions.h>
#include "voxel/voxelize.h"

layout ( triangles ) in;
layout ( triangle_strip, max_vertices = 3 ) out;

in gl_PerVertex { vec4 gl_Position; } gl_in [];
out gl_PerVertex { vec4 gl_Position; int gl_ViewportIndex; };

out vec3 Position;
out vec2 TextureCoord;
flat out int Axis;
flat out vec4 AABB;
// out mat3 TangentInverse;

in vec2 g_textureCoord[];
// in vec3 v_tangentX[];
// in vec4 v_tangentZ[];

uniform mat4 X_Projection;
uniform mat4 Y_Projection;
uniform mat4 Z_Projection;


uniform readonly layout(binding = 0, r32ui) uimage3D VoxelBuffer;
uniform readonly layout(binding = 1, r8ui) uimage3D PageMask;


void main()
{
  ivec3 gridDim = VoxelizeMode == VOXELIZESCENE ? imageSize(VoxelBuffer) : imageSize(PageMask);

  vec3 faceNormal = normalize(cross(gl_in[1].gl_Position.xyz - gl_in[0].gl_Position.xyz, gl_in[2].gl_Position.xyz - gl_in[0].gl_Position.xyz));
  float absX = abs(faceNormal.x);
  float absY = abs(faceNormal.y);
  float absZ = abs(faceNormal.z);

  // find dominant axis (axis with maximal areo of projected triangle)
  // X dominant?
  mat4 projectionMatrix;
  if (absX > absY && absX > absZ)
  {
    projectionMatrix = X_Projection;
    Axis = X_AXIS;
    gl_ViewportIndex = X_AXIS;
    gridDim.xyz = gridDim.zyx;
  }
  // Y Dominant?
  else if (absY > absX && absY > absZ)
  {
    projectionMatrix = Y_Projection;
    Axis = Y_AXIS;
    gl_ViewportIndex = Y_AXIS;
    gridDim.xyz = gridDim.xzy;
  }
  // Z Dominant!
  else
  {
    projectionMatrix = Z_Projection;
    Axis = Z_AXIS;
    gl_ViewportIndex = Z_AXIS;
    gridDim.xyz = gridDim.xyz;
  }
  vec4 clip_position[3];

  clip_position[0] = projectionMatrix * gl_in[0].gl_Position;
  clip_position[1] = projectionMatrix * gl_in[1].gl_Position;
  clip_position[2] = projectionMatrix * gl_in[2].gl_Position;

  // Axis Aligned Bounding Box of the triangle in clip space
  AABB = vec4(clip_position[0].xy, clip_position[0].xy);
  AABB.xy = min( clip_position[1].xy, AABB.xy);
  AABB.zw = max( clip_position[1].xy, AABB.zw);

  AABB.xy = min( clip_position[2].xy, AABB.xy);
  AABB.zw = max( clip_position[2].xy, AABB.zw);

  // enlarge it for conservative rasterization in fragment
  AABB.xy -= 1.0 / gridDim.xy;
  AABB.zw += 1.0 / gridDim.xy;

  // vertex enlargement for conservative rasterization
  // (Conservative Rasterisation GPU Gems 2, Ch 42)[http://http.developer.nvidia.com/GPUGems2/gpugems2_chapter42.html]
  // "Overestimated conservative rasterization can be seen as the image-processing operation dilation of the polygon by the pixel cell."

  // calculate the planes on the 3 edges (in normal form) for dilation
  vec3 e0 = vec3(clip_position[1].xy - clip_position[0].xy, 0);
  vec3 e1 = vec3(clip_position[2].xy - clip_position[1].xy, 0);
  vec3 e2 = vec3(clip_position[0].xy - clip_position[2].xy, 0);
  vec3 n0 = cross(e0, vec3(0,0,1));
  vec3 n1 = cross(e1, vec3(0,0,1));
  vec3 n2 = cross(e2, vec3(0,0,1));

  // now dilate (grow along the normal of the corresponding edge)
  vec2 pl = 1.4142135637309 / gridDim.xy;
  clip_position[0].xy += pl * ((e2.xy / dot(e2.xy,n0.xy)) + (e0.xy / dot(e0.xy,n2.xy)));
  clip_position[1].xy += pl * ((e0.xy / dot(e0.xy,n1.xy)) + (e1.xy / dot(e1.xy,n0.xy)));
  clip_position[2].xy += pl * ((e1.xy / dot(e1.xy,n2.xy)) + (e2.xy / dot(e2.xy,n1.xy)));


  Position      = clip_position[0].xyz;
  TextureCoord  = g_textureCoord[0];
  gl_Position   = clip_position[0];
  EmitVertex();

  Position      = clip_position[1].xyz;
  TextureCoord  = g_textureCoord[1];
  gl_Position   = clip_position[1];
  EmitVertex();

  Position      = clip_position[2].xyz;
  TextureCoord  = g_textureCoord[2];
  gl_Position   = clip_position[2];
  EmitVertex();

  EndPrimitive();
}

/*

  // we are done here, emit the new triangle
  // gl_Position = projectionMatrix * gl_in[0].gl_Position;
  // gl_Position = vec4(-1, 1, 0, 0);
  // gl_Position = clip_position[0];
  Position    = clip_position[0].xyz;
  vec3 tangentZ   = normalize(NormalMatrix * v_tangentZ[0].xyz);
  vec3 tangentX   = normalize(NormalMatrix * v_tangentX[0].xyz);
  vec3 tangentY   = normalize(cross( tangentZ, tangentX ) * v_tangentZ[0].w);
  TangentInverse  = mat3( tangentX, tangentY, tangentZ );

  EmitVertex();

  // gl_Position = vec4(0, 1, 0, 0);
  // gl_Position = clip_position[1];
  Position    = clip_position[1].xyz;
  tangentZ   = normalize(NormalMatrix * v_tangentZ[1].xyz);
  tangentX   = normalize(NormalMatrix * v_tangentX[1].xyz);
  tangentY   = normalize(cross( tangentZ, tangentX ) * v_tangentZ[1].w);
  TangentInverse  = mat3( tangentX, tangentY, tangentZ );
  TextureCoord = v_textureCoord[1];
  EmitVertex();

  // gl_Position = projectionMatrix * gl_in[2].gl_Position;
  // gl_Position = vec4(1, 1, 0, 0);
  // gl_Position = clip_position[2];
  Position    = clip_position[2].xyz;
  tangentZ   = normalize(NormalMatrix * v_tangentZ[2].xyz);
  tangentX   = normalize(NormalMatrix * v_tangentX[2].xyz);
  tangentY   = normalize(cross( tangentZ, tangentX ) * v_tangentZ[2].w);
  TangentInverse  = mat3( tangentX, tangentY, tangentZ );
  TextureCoord = v_textureCoord[2];
  EmitVertex();

  EndPrimitive();
  */
