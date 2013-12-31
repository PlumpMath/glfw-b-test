#version 430

layout(location = 0) in vec4 attribPosition;

void main () {
  gl_Position = attribPosition;
}
