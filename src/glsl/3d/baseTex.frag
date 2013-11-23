#version 330 core

out vec4 fragColor;
in vec4 interpolated_color;
in vec2 vertex_uv;
in vec3 vertex_normal;

uniform sampler2D textures;
uniform vec3 global_light_direction = normalize(vec3(0.0, 0.0, 1.0)); // should be the other direction
uniform vec3 global_light_intensity = vec3(1.0, 1.0, 1.0);

void main()
{

    float cosLight    = clamp( dot( vertex_normal, global_light_direction ), 0, 1 );
    vec4 lightedColor = vec4( global_light_intensity * cosLight * interpolated_color.rgb
                            , interpolated_color.a );
    vec4 color        = texture( textures, vertex_uv ) * lightedColor;
    fragColor         = color;
}

