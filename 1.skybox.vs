#version 330 core
layout (location = 0) in vec3 aPos;

out vec3 TexDir;

uniform mat4 view;
uniform mat4 projection;

void main()
{
    TexDir = aPos;
    mat4 viewNoTrans = mat4(mat3(view));      // ตัด translation ออก
    vec4 pos = projection * viewNoTrans * vec4(aPos, 1.0);
    gl_Position = pos.xyww;                   // depth = 1.0
}