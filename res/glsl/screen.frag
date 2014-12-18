#version 410
#extension GL_ARB_separate_shader_objects : enable

// (c) 2014 Jan-Philip Loos
//
// project a texture onto the screen
layout(location = 0) out vec4 fragColor;
uniform sampler2D iTexture;
uniform vec4 iColor;
// xy : px, zw : 1/xy
uniform vec4 iScreenSize;

void main()
{
  iScreenSize.zw;
  vec2 uv = gl_FragCoord.xy / textureSize(iTexture, 0);
  fragColor = iColor * texture(iTexture, uv);
}
