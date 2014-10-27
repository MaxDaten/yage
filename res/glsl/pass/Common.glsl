#ifndef __COMMON__
#define __COMMON__

#include "GlobalDefs.glsl"

const float PI = 3.1415926535897932f;

#define saturate(X) (clamp(X, 0.0, 1.0))

float square(float X){ return X*X; }
vec2 square(vec2 X)  { return X*X; }
vec3 square(vec3 X)  { return X*X; }
vec4 square(vec4 X)  { return X*X; }

/*
    [http://www.geeks3d.com/20091216/geexlab-how-to-visualize-the-depth-buffer-in-glsl/]
    this is double checked and really 100% correct for OpenGL

    Use this formular for zRatio:
    zRatio.x = ( far + near ) / ( far - near )
    zRatio.y = ( 2.0 * near * far ) / ( far - near )
    with near > 0 and far > 0 and far > near

    @return depth-value [near..far] in view space
*/
float LinearDepth (float zBufferDepth, vec2 zRatio)
{
    float zDeviceDepth = 2.0 * zBufferDepth - 1.0;      // [-1..1]
    return ( zRatio.y / ( zRatio.x - zDeviceDepth ) );
}


// reconstruct position
// zBufferDepth in OpenGL in the range [0,1]
// [http://mynameismjp.wordpress.com/2010/09/05/position-from-depth-3/]
vec3 PositionVSFromDepth ( float zBufferDepth, vec2 zRatio, vec3 ViewPosition )
{
    // negate because we look at the negative z axis
    // vec3 viewRay = normalize(ViewPosition);
    vec3 viewRay = vec3(ViewPosition.xy / abs(ViewPosition.z), -1.0);
    // vec3 viewRay = ViewPosition.xyz * (100.0 / - ViewPosition.z);
    return viewRay * LinearDepth( zBufferDepth, zRatio );
}

float Luminance( vec3 color )
{
    return dot( color, vec3( 0.3, 0.59, 0.11 ) );
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