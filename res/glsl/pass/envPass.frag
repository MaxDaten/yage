#version 410 core


uniform samplerCube SkyTexture;
uniform vec4 SkyColor;
in vec3 VertexSTP;

// Red Green Blue Depth
layout (location = 0) out vec4 pixelColor;

void main()
{
    pixelColor       = SkyColor * texture(SkyTexture, VertexSTP);
}

