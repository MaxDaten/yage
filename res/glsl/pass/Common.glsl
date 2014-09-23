#ifndef __COMMON__
#define __COMMON__

const float PI = 3.1415926535897932f;

#define saturate(X) (clamp(X, 0.0, 1.0))

float LinearDepth (float z, vec2 zRatio)
{
    return (zRatio.y / (z - zRatio.x));
}


// reconstruct position
vec3 PositionVSFromDepth ( float zDeviceDepth, vec2 zRatio, vec3 ViewPosition )
{
    vec3 viewRay = vec3(ViewPosition.xy / ViewPosition.z, 1.0);
    return viewRay * LinearDepth( zDeviceDepth, zRatio );
}


vec4 gamma(vec3 x, float y)
{
    return vec4(pow(x.r, y), pow(x.g, y), pow(x.b, y), 1.0);
}

vec4 gamma(vec4 x, float y)
{
    return vec4(pow(x.r, y), pow(x.g, y), pow(x.b, y), x.a);
}


#endif // __COMMON__