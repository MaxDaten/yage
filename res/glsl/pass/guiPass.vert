#version 410 core

uniform mat4 ModelMatrix       = mat4(0.0);
uniform mat4 VPMatrix          = mat4(0.0);

in vec2 vPosition;
in vec2 vTexture;
in vec4 vColor;

out vec2 TextureCoords;
out vec4 Color;

void main(void)
{
    TextureCoords   = vTexture;
    Color           = vColor;
    gl_Position     = VPMatrix * ModelMatrix * vec4( vPosition, 0.0, 1.0 );
}

