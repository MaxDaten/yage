#version 410 core
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_include : require

#include <common.h>
#include <attributes.h>

out gl_PerVertex {
vec4 gl_Position;
};

uniform mat4 AlbedoTextureMatrix    = mat4(1.0);
uniform mat4 NormalTextureMatrix    = mat4(1.0);
uniform mat4 RoughnessTextureMatrix = mat4(1.0);
uniform mat4 MetallicTextureMatrix  = mat4(1.0);

// naturally in model-space
layout(location = VPOSITION) in vec3 vPosition;
layout(location = VTEXTURE)  in vec2 vTexture;
layout(location = VTANGENTX) in vec3 vTangentX;
layout(location = VTANGENTZ) in vec4 vTangentZ;

out vec2 AlbedoST;
out vec2 NormalST;
out vec2 RoughnessST;
out vec2 MetallicST;
out mat3 TangentInverse;
out vec3 PositionWorld;

void main()
{
  mat4 ModelToView     = ViewMatrix * ModelMatrix;
  mat4 ModelToProj     = VPMatrix * ModelMatrix;

  // flip vertical (t or v) because opengl's first row in an image is bottom left (instead of top left)
  // tangents are respected in the frag-shaders for NormalY calculation (cross arguments are flipped)
  AlbedoST             = (AlbedoTextureMatrix * vec4(vTexture, 0.0, 1.0)).st;
  NormalST             = (NormalTextureMatrix * vec4(vTexture, 0.0, 1.0)).st;
  RoughnessST          = (RoughnessTextureMatrix * vec4(vTexture, 0.0, 1.0)).st;
  MetallicST           = (MetallicTextureMatrix  * vec4(vTexture, 0.0, 1.0)).st;

  // normalize is neccessary to unstretch the tangets again
  vec3 tangentZ        = normalize(NormalMatrix * vTangentZ.xyz);
  vec3 tangentX        = normalize(NormalMatrix * vTangentX.xyz);
  vec3 tangentY        = normalize(cross( tangentZ, tangentX ) * vTangentZ.w);
  TangentInverse       = mat3( tangentX, tangentY, tangentZ );

  PositionWorld        = vec3(ModelMatrix * vec4(vPosition, 1.0));
  gl_Position     = ModelToProj * vec4(vPosition, 1.0);
}
