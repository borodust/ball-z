#version 410 core

out vec4 fColor;

in v_PerVertex {
  vec2 texCoord;
};

uniform sampler2D banner;

void main() {
  fColor = vec4(texture(banner, texCoord));
}
