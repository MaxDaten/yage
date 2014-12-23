#ifndef __SAMPLING_H__
#define __SAMPLING_H__

#define MAX_TEXTURES 12

uniform sampler2D iTextures [ MAX_TEXTURES ];
uniform vec4        iColors [ MAX_TEXTURES ];

// number of textures in iTexture array
uniform int       iUsedTextures = 1;

#endif // __SAMPLING_H__
