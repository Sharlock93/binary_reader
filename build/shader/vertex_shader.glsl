#version 450
layout(location = 0) in vec2 vp;
layout(location = 1) in vec2 uv;

layout( location = 2 ) uniform vec4 color = vec4(224.0/255.0, 212.0/255.0, 40.0/255.0, 1);
layout( location = 3 ) uniform mat4 view = mat4(1);
layout( location = 5 ) uniform bool has_texture = false;
layout( location = 6 ) uniform bool invert_y_uv = false;

out vec2 frag_uv;

void main() {

   gl_Position = vec4(vp.x, vp.y, 0, 1)*view;
   if(invert_y_uv) {
	   frag_uv = vec2(uv.x, uv.y);
   } else {
	   frag_uv = uv;
   }
}

