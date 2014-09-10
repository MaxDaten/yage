#version 410 core

// in vec4 interpolated_color;
#define ZBUFFER_DEPTH 1
uniform vec2 ZNearFar;
uniform vec2 ZProjRatio;

uniform sampler2D AlbedoTexture;
uniform sampler2D NormalTexture;
uniform sampler2D DepthTexture;
uniform float Gamma = 2.2;

// lightPosition is in WorldSpace
uniform vec3 lightPosition;    
uniform vec3 lightRadius;    
uniform vec4 lightColor;
// x = constant, y = inv linear, z = inv square
uniform vec3 lightAttenuation;
uniform float lightSpecularExp;

uniform ivec2 ViewportDim;

in mat4 ViewSpace;
in vec3 VertexPosVS;


float zNear = ZNearFar.x;
float zFar = ZNearFar.y;


layout (location = 0) out vec4 pixelColor;

float LinearDepth (float z)
{
    return (ZProjRatio.y / (z - ZProjRatio.x));
}

const float f0 = pow((1.0-(1.0/1.31)), 2)/pow((1.0+(1.0/1.31)), 2); 
float Fresnel( vec3 halfDir, vec3 viewDir, float f0 );

float saturate(float value)
{
    return clamp(value, 0.0, 1.0);
}

vec4 gamma(vec3 x, float y)
{
    return vec4(pow(x.r, y), pow(x.g, y), pow(x.b, y), 1.0);
}

vec4 gamma(vec4 x, float y)
{
    return vec4(pow(x.r, y), pow(x.g, y), pow(x.b, y), x.a);
}


void main()
{
    vec2 st;
    st = gl_FragCoord.xy / ViewportDim.xy;
    
    // the channel for albedo rgb + distance from View
    vec4 albedoCh           = texture( AlbedoTexture, st ).rgba;
    
    // the lit pixel albedo color 
    vec3 pixel_albedo       = gamma(albedoCh.rgb, Gamma).rgb;

    // distance from view position (View)
    // float zzz = ;

    #ifdef ZBUFFER_DEPTH
    float depth             = texture(DepthTexture, st).x;
    float linearDepth       = LinearDepth(depth);
    vec3 viewRay            = vec3(VertexPosVS.xy / VertexPosVS.z, 1.0);
    #else
    DepthTexture;
    float linearDepth       = albedoCh.a;
    vec3 viewRay            = vec3(VertexPosVS.xy * (zFar / VertexPosVS.z), zFar);
    #endif

    // retrieve the normal of the lit pixel
    vec3 normalVS  = texture( NormalTexture, st ).rgb * 2.0 - 1.0;

    // world light position to View space
    vec3 lightPosVS = vec3(ViewSpace * vec4(lightPosition, 1.0));

    // extrapolate the View space position of the pixel to the zFar plane
    vec3 pixelPosVS     = viewRay * linearDepth;
    
    // direction from the lit pixel to the light source
    vec3 pixToLight = lightPosVS - pixelPosVS;
    float dist      = length(pixToLight);
    vec3 toLightDir = pixToLight / dist;

    float lambertian = saturate(dot(normalVS, toLightDir));
    float specular = 0.0;
    if (lambertian > 0.0) {
        // direction from pixel to View
        vec3 viewDir    = normalize(-pixelPosVS);
        vec3 halfDir    = normalize(toLightDir + viewDir);
        float specAngle = saturate(dot(halfDir, normalVS));

        specular = pow(specAngle, lightSpecularExp);
    }

    // float atten_factor = 1.0/(lightAttenuation.x + lightAttenuation.y * dist_2d + lightAttenuation.z * dist_2d * dist_2d);
    float curve = min(pow(dist / lightRadius.x, 6.0), 1.0);
    float atten_factor = mix(
        lightAttenuation.x +                // constant
        lightAttenuation.y * dist +         // inv linear
        lightAttenuation.z * dist * dist    // inv quadric
        , 0.0
        , curve
        );
    pixelColor =  vec4( pixel_albedo * (
               atten_factor * ( lambertian * lightColor.rgb + specular * lightColor.rgb )
               ), 1.0);
}

/*
float Fresnel( vec3 halfDir, vec3 viewDir, float f0 )
{
    float base = 1 - dot(viewDir, halfDir);
    float exponential = pow(base, 5.0);
    return exponential + f0 * (1.0 - exponential);
}
*/
