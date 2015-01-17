#ifndef __COLOR_H__
#define __COLOR_H__

// http://stackoverflow.com/a/596241/605745
// Luminance Digital CCIR601
float Luminance(vec3 color)
{
  return dot(color, vec3(0.299, 0.587, 0.114));
  // return dot( color, vec3( 0.3, 0.59, 0.11 ) );
}

vec4 gamma(vec3 x, float y)
{
    return vec4(pow(x.r, y), pow(x.g, y), pow(x.b, y), 1.0);
}


vec4 gamma(vec4 x, float y)
{
    return vec4(pow(x.r, y), pow(x.g, y), pow(x.b, y), x.a);
}


/* __COLOR_H__ */
#endif
