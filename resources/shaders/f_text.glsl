#version 410 core

out vec4 fColor;

in v_PerVertex {
  vec2 texCoord;
};

uniform sampler2D atlas;

void main() {
  fColor = vec4(0.0, 0.0, 0.0, texture(atlas, texCoord).r);
}
