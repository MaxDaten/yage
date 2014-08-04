#version 410 core

uniform mat4 ModelMatrix       = mat4(0.0);
uniform mat4 VPMatrix          = mat4(0.0);

in vec2 vPosition;
in vec2 vTexture;
in vec4 vColor;

out vec2 TextureCoords;
out vec4 BaseColor;

void main(void)
{
    // our gui is lower left orientated, positive y-axis upwards
    TextureCoords   = vec2( vTexture.s, 1.0 - vTexture.t );
    BaseColor       = vColor;
    gl_Position     = VPMatrix * (ModelMatrix * vec4( vPosition, 0.0, 1.0 ));
}

