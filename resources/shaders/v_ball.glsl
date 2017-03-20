#version 410 core

#include <lighting>

layout(location = 0) in vec3 vPosition;
layout(location = 1) in vec3 vNormal;


out gl_PerVertex {
  vec4 gl_Position;
};

out v_PerVertex {
  vec4 color;
};

uniform mat4 modelViewProjection;
uniform mat3 normalTransform;
uniform vec4 baseColor;
uniform DirectionalLight dLight;

void main() {
  color = computeLight(baseColor, normalTransform * vNormal, dLight);
  gl_Position = modelViewProjection * vec4(vPosition, 1.0);
}
