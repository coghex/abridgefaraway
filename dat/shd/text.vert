#version 450
#extension GL_ARB_separate_shader_objects : enable

// see https://www.khronos.org/opengl/wiki/Interface_Block_(GLSL) for
// difference between block name and instance name
layout(binding = 0) uniform TransformationMatrix {
  mat4 model;
  mat4 view;
  mat4 proj;
} trans;

//layout(binding = 1) uniform ShdTextBuffer {
//  uint text;
//} textBuff;

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec4 inColor;
layout(location = 2) in vec3 inTexCoord;
layout(location = 3) in vec3 inMove;

layout(location = 0) out vec4 fragColor;
layout(location = 1) out vec2 fragTexCoord;
layout(location = 2) out int fragTexIndex;

out gl_PerVertex {
    vec4 gl_Position;
};

void main() {
    gl_Position = trans.proj * trans.view * trans.model * vec4(inPosition, 1.0);
    fragColor = inColor;
    fragTexCoord = inTexCoord.xy;
    fragTexIndex = int(inTexCoord.z);
}
