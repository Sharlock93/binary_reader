#version 450
out vec4 frag_color;
layout( location = 2 ) uniform vec4 color = vec4(224.0/255.0, 212.0/255.0, 40.0/255.0, 1);

void main() {
	frag_color = color;
}
