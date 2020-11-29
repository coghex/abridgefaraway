#version 450
#extension GL_ARB_separate_shader_objects : enable

// see https://www.khronos.org/opengl/wiki/Interface_Block_(GLSL) for
// difference between block name and instance name
layout(binding = 0) uniform TransformationObject {
  mat4 model;
  mat4 view;
  mat4 proj;
  ivec3 texindex;
} trans;

layout(binding = 2) uniform DynTransObject {
  mat4 move;
} dyn;

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
    vec4 col0 = vec4(1.0,0.0,0.0,0.0);
    vec4 col1 = vec4(0.0,1.0,0.0,0.0);
    vec4 col2 = vec4(0.0,0.0,1.0,0.0);
    vec4 col3 = vec4(0.0,0.0,-1.0,1.0);
    mat4 basicI = mat4(col0,col1,col2,col3);
    mat4 view = (inMove.z > 0.0) ? trans.view : basicI;
    mat4 dynV = (inMove.y > 0.0) ? (trans.model * dyn.move) : trans.model;
    gl_Position = trans.proj * view * dynV * vec4(inPosition, 1.0);
    fragColor = inColor;
    fragTexCoord = inTexCoord.xy;
    fragTexIndex = int(inTexCoord.z);
}
