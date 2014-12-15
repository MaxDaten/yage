#version 410
#extension GL_ARB_separate_shader_objects : enable

// (c) 2014 Jan-Philip Loos
//
// Passes vertex position & color

layout(location = 0) in vec3 aPosition;
layout(location = 3) in vec3 aColor;

out vec3 color;
out gl_PerVertex {
  vec4 gl_Position;
};

void main() {
  color = aColor;
  gl_Position = vec4(aPosition, 1.0);
}
