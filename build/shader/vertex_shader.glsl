#version 450
in vec2 vp;

layout( location = 2 ) uniform vec4 color = vec4(224.0/255.0, 212.0/255.0, 40.0/255.0, 1);
layout( location = 3 ) uniform mat4 view = mat4(1);

void main() {
   gl_Position = vec4(vp.x, vp.y, 0, 1)*view;
}

