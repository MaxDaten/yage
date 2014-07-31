#version 410 core

uniform mat4 MVPMatrix           = mat4(1.0);

// naturally in model-space
in vec3 vPosition;
in vec2 vTexture;

out vec2 TextureCoords;

void main()
{
    TextureCoords   = vTexture;
    
    gl_Position     = MVPMatrix * vec4( vPosition, 1.0 );
}

