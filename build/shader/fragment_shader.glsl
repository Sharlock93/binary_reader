#version 450
layout( location = 2 ) uniform vec4 color = vec4(224.0/255.0, 212.0/255.0, 40.0/255.0, 1);
layout( location = 5) uniform bool has_texture = false;


in vec2 frag_uv;
uniform sampler2D tex;
out vec4 frag_color;

void main() {
	frag_color = vec4(1, 1, 1, 1);

	if(has_texture) {
		frag_color.w = texture(tex, frag_uv).r;
	}
}
