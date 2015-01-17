#ifndef __SAMPLING_H__
#define __SAMPLING_H__

#include <definitions.h>

uniform sampler2D  iTextures [ MAX_TEXTURES ];
uniform vec4        iWeights [ MAX_TEXTURES ];

uniform int       iUsedTextures = 1;
 /* x = width, y = height, z = 1/x, w = 1/y */
uniform vec4      iTargetSize;

#endif /* __SAMPLING_H__ */
