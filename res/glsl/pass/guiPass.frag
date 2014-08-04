#version 410 core

#define UVDEBUG 1

#define GUI_TXT 0
#define GUI_SDF 1
#define GUI_IMG 2

uniform sampler2D ElementTexture;
uniform int GUIType;

in vec2 TextureCoords;
in vec4 BaseColor;

layout (location = 0) out vec4 fragColor;

vec4 FontColor(void)
{
    float fontColor = texture( ElementTexture, TextureCoords ).r;
    vec4 outColor   = BaseColor; 
    outColor.a     *= fontColor > 0.5 ? 1.0 : 0.0;
    return outColor;
}

vec4 UVColor(void)
{
    return vec4( TextureCoords.s, TextureCoords.t, 0.0, 1.0 );
}

vec4 ElementColor(void)
{
    vec4 texColor = texture( ElementTexture, TextureCoords );
    float alpha   = texColor.a * BaseColor.a;
    return vec4(mix(BaseColor.rgb, texColor.rgb, alpha ), alpha );
}

void main(void)
{
    // if (uvdebug == UVDEBUG)
    // fragColor = mix( UVColor(), FontColor(), 0.1);
    vec4 outColor = vec4( 1, 0, 0, 1); 
    
    if ( GUIType == GUI_TXT || GUIType == GUI_SDF )
    {
        outColor = FontColor();
    }
    
    else if ( GUIType == GUI_IMG )
    {
        outColor = ElementColor();
    }

    fragColor = outColor;
}

