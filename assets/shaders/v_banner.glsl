#version 410 core

layout(location = 0) in vec2 vPosition;
layout(location = 1) in vec2 vTexCoord;

out gl_PerVertex {
  vec4 gl_Position;
};

out v_PerVertex {
  vec2 texCoord;
};

uniform mat4 proj;
uniform vec2 pos;

void main () {
  gl_Position = proj * vec4(vPosition + pos, 1.0, 1.0);;
  texCoord = vTexCoord;
}
