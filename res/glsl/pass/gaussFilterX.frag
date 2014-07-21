#version 410 core
// http://rastergrid.com/blog/2010/09/efficient-gaussian-blur-with-linear-sampling/

uniform sampler2D SamplingTexture;
uniform vec4 TextureSize; // w, h, 1/w, 1/h
uniform float offsets[3] = float[]( 0.0, 1.3846153846, 3.2307692308 );
uniform float weights[3] = float[]( 0.2270270270, 0.3162162162, 0.0702702703 );

in vec2 VertexUV;
layout (location = 0) out vec3 pixelColor;

void main()
{
    vec4 texColor = texture( SamplingTexture, VertexUV ) * weights[0];
    for (int i=1; i<3; i++) {
        texColor += texture( SamplingTexture, VertexUV + vec2(offsets[i] * TextureSize.z, 0.0) ) * weights[i];
        texColor += texture( SamplingTexture, VertexUV - vec2(offsets[i] * TextureSize.z, 0.0) ) * weights[i];
    }
    pixelColor = texColor.rgb;
}

