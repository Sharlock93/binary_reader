#ifndef SH_SIMPLE_MATH
#define SH_SIMPLE_MATH

typedef union {
	struct {
		f32 x;
		f32 y;
		f32 z;
		f32 w;
	};

	f32 _d[4];
} pos4;



typedef union {
	struct {
		pos4 a;
		pos4 b;
		pos4 c;
		pos4 d;
	};

	f32 m[16];
}  mat4;



typedef struct sh_vec2 {
	f32 x;
	f32 y;
} sh_vec2, pos2;


void sh_vec2_add(sh_vec2 *r, sh_vec2 *a, sh_vec2 *b);
void sh_vec2_sub(sh_vec2 *r, sh_vec2 *a, sh_vec2 *b);
void sh_vec2_mul(sh_vec2 *r, sh_vec2 *a, float val);
float sh_vec2_dot(sh_vec2 *a, sh_vec2 *b);
void sh_vec2_normal(sh_vec2 *r, sh_vec2 *a);
float sh_vec2_lensq(sh_vec2 *a);
float sh_vec2_len(sh_vec2 *a);
void sh_vec2_normalize(sh_vec2 *r, sh_vec2 *a);

// void sh_vec2_add(sh_vec2 *r, sh_vec2 *a, sh_vec2 *b) {
// 	r->x = a->x + b->x;
// 	r->y = a->y + b->y;
// }

// void sh_vec2_sub(sh_vec2 *r, sh_vec2 *a, sh_vec2 *b) {
// 	r->x = a->x - b->x;
// 	r->y = a->y - b->y;
// }

// void sh_vec2_mul(sh_vec2 *r, sh_vec2 *a, float val) {
// 	r->x = a->x*val;
// 	r->y = a->y*val;
// }


// void sh_vec2_normal(sh_vec2 *r, sh_vec2 *a) {
// 	r->x = -a->y;
// 	r->y =  a->x;
// }

// void sh_vec2_normalize(sh_vec2 *r, sh_vec2 *a) {
// 	float length = sh_vec2_len(a);
// 	r->x = a->x/length;
// 	r->y = a->y/length;
// }

// float sh_vec2_lensq(sh_vec2 *a) {
// 	return a->x*a->x + a->y*a->y;
// }

// float sh_vec2_len(sh_vec2 *a) {
// 	return sqrt(sh_vec2_lensq(a));
// }

// float sh_vec2_dot(sh_vec2 *a, sh_vec2 *b) {
// 	return a->x*b->x + a->y*b->y;
// }


//debug funcs

void print_sh_vec2(sh_vec2 *p) {
	printf("(x: %f, y: %f)", p->x, p->y);
}


mat4 ortho(float left, float right, float bottom, float top, float pnear, float pfar) {
	float width = (right-left);
	float height = (top - bottom);
	mat4 m = {0};


	m.a.x = 2.0f/(width);
	m.b.y = 2.0f/(height);
	m.c.z = -2.0f/(pfar - pnear);

	m.a.w = - (right + left) / (right - left);
	m.b.w = - (top + bottom) / (top - bottom);
	m.c.w = - (pfar + pnear) / (pfar - pnear);
	m.d.w = 1;
	return m;
}

#endif
