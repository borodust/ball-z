#version 410 core

out vec4 fColor;

in v_PerVertex {
  vec3 color;
};

void main() {
  fColor = vec4(color, 1.0);
}
