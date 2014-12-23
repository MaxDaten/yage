#version 410
#extension GL_ARB_separate_shader_objects : enable

out gl_PerVertex {
vec4 gl_Position;
};

void main() {
  gl_Position = vec4 (float(gl_VertexID / 2) * 4.0 - 1.0, float(gl_VertexID % 2) * 4.0 - 1.0, 0.0, 1.0);
}
