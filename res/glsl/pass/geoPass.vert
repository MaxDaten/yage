#version 410 core

uniform mat4 AlbedoTextureMatrix    = mat4(1.0);
uniform mat4 NormalTextureMatrix    = mat4(1.0);
uniform mat4 RoughnessTextureMatrix = mat4(1.0);
uniform mat4 MetallicTextureMatrix  = mat4(1.0);
uniform mat4 ViewMatrix          = mat4(1.0);
uniform mat4 VPMatrix            = mat4(1.0);
uniform mat4 ModelMatrix         = mat4(1.0);
uniform mat3 NormalMatrix        = mat3(1.0);

// naturally in model-space
in vec3 vPosition;
in vec2 vTexture;
in vec3 vTangentX;
in vec4 vTangentZ;

out vec2 AlbedoST;
out vec2 NormalST;
out vec2 RoughnessST;
out vec2 MetallicST;
out mat3 TangentInverse;

// Gram-Schmidt
mat3 orthogonalize ( mat3 basis )
{
    vec3 t = basis[0] - dot(basis[2], basis[0]) * basis[2];
    vec3 b = basis[1] - dot(basis[2], basis[1]) * basis[2] - dot(t, basis[1]) * t;
    return mat3(normalize(t), normalize(b), normalize(basis[2]));
}

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
    
    vec3 tangentZ        = NormalMatrix * vTangentZ.xyz;
    vec3 tangentX        = NormalMatrix * vTangentX.xyz;
    vec3 tangentY        = normalize(cross( tangentZ, tangentX ) * vTangentZ.w);
    TangentInverse       = mat3( tangentX, tangentY, tangentZ );
    
    gl_Position     = ModelToProj * vec4( vPosition, 1.0 );
}
