#version 410
#extension GL_ARB_separate_shader_objects : enable

out gl_PerVertex {
vec4 gl_Position;
};

out int g_Layer;

void main() {
  g_Layer = gl_InstanceID;
  gl_Position = vec4 (float(gl_VertexID / 2) * 4.0 - 1.0, float(gl_VertexID % 2) * 4.0 - 1.0, 0.0, 1.0);
}
