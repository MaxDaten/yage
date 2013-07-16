#version 330

out vec4 fragColor;

void main()
{
    float size = 20.0f;
    vec4 black = vec4(0.0f, 0.0f, 0.0f, 1.0f);
    vec4 white = vec4(1.0f, 1.0f, 1.0f, 1.0f);
    float chessX = step(0, sin(gl_FragCoord.y / size));
    float chessY = step(0, sin(gl_FragCoord.x / size));
    
    fragColor = (chessX == chessY)
            ? white 
            : black;
}