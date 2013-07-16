#version 330

out vec4 fragColor;

void main()
{
    // float lerpValue = gl_FragCoord.y / 500.0f;
    
    fragColor = sin(gl_FragCoord.y / 10.0f) >= 0 || sin(gl_FragCoord.x / 10.0f) >= 0
            ? vec4(1.0f, 1.0f, 1.0f, 1.0f) 
            : vec4(0.2f, 0.2f, 0.2f, 1.0f);
}