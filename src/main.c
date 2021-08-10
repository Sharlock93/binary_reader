#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <Windows.h>
#include <intrin.h>
#include <math.h>
#include <stdarg.h>
#include "sh_tools.c"

#include "language.c"
#include "editor.c"

#include <GL/gl.h>

#include "wglext.h"
#include "glcorearb.h"
#include "gl_load_funcs.h"
#include "sh_simple_vec_math.c"
#include "table_read_utils.c"
#include <ft2build.h>


#pragma comment(lib, "lib/glfw3dll.lib")

#include <GLFW/glfw3.h>

#include FT_FREETYPE_H

#define STB_TRUETYPE_IMPLEMENTATION
#include "stb_truetype.h"

#define USE_FREE_TYPE 1
// #define USE_GLFW 


typedef struct character_info {
	pos4 uv;
	i32 x_offset;
	i32 y_offset;
} character_info;

typedef struct sh_window_context {
	i16 height;
	i16 width;
	i16 x;
	i16 y;
	i8 should_close;
	char *window_name;
	HDC hdc;
	mat4 screen_scale;
	i32 vbo;
	i32 font_size;
	f32 quad_height;
	f32 quad_width;
	char input[256];
	i32 input_size;
	u8 *font_atlas;
	u8 *ft_atlas;
	i32 font_atlas_texture;
	i32 ft_atlas_texture;
	i32 atlas_height;
	i32 atlas_width;
	i32 ft_atlas_height;
	i32 ft_atlas_width;

	character_info char_cache[94];
	character_info ft_char_cache[94];
	char *text;
	i32 line_numbers;
	GLFWwindow *window;
	i32 left_add;
	i32 top_add;
} sh_window_context;


// u - v, float - 4 byte each - so 8 bytes
// index <- 

sh_window_context gl_ctx;
font_directory *font;

#include "opengl_window.c"

typedef struct line {
	float x1;
	float y1;
	float x2;
	float y2;
} line;


int comp_sh_point(const void *a, const void *b) {
	line *edge_a = (line*)a;
	line *edge_b = (line*)b;

	float a_y = MIN(edge_a->y1, edge_a->y2);
	float b_y = MIN(edge_b->y1, edge_b->y2);

	if(a_y > b_y) {
		return 1;
	} else if(a_y < b_y) {
		return -1;
	}

	return 0;
}

typedef struct intersection {
	struct intersection *next;
	line *edge;
	float intersect;
	int slope_direction;
	float slope;
} intersection; 


void bubble_sort_intersection(intersection **head) {
	intersection *actual_head = *head;
	intersection **to = head;
	int sorted = 1;

	while(true) {
		to = head;
		sorted = 1;
		while(*to != NULL && (*to)->next != NULL) {
			if((*to)->intersect > (*to)->next->intersect) {
				intersection *temp = (*to)->next;
				(*to)->next = temp->next;
				temp->next = *to;
				*to = temp;
				sorted = 0;
			}
			to = &((*to)->next);
		}
		if(sorted) break;
	}
}

int counter_add_to_link = 0;
void add_to_linked_list(intersection **head, intersection *new_intersection) {
	intersection *new_node = (intersection*) calloc(1, sizeof(intersection));
	*new_node = *new_intersection;
	if(*head == NULL) {
		*head = new_node;
	} else {
		intersection *m = *head;
		while(m->next) { m = m->next; }
		m->next = new_node;
	}
}


line* generate_edges(pos2 *pts, int contour_counts, int *contour_end_index, int *edges_num) {

	if(pts == NULL) return NULL;
	line *edges = NULL;
	int size_aprox = contour_end_index[contour_counts-1];
	edges = (line*) calloc(sizeof(line)*size_aprox, 1);
	int j = 0;
	int counter = 0;
	for(int i = 0; i < contour_counts; i++) {
		for(; j < contour_end_index[i]-1; j++) {
			if(pts[j].y == pts[j+1].y) continue;
			edges[counter].x1 = pts[j].x;
			edges[counter].x2 = pts[j+1].x;
			edges[counter].y1 = pts[j].y;
			edges[counter].y2 = pts[j+1].y;
			counter++;
		}
		j++;
	}
	*edges_num = counter;

	return edges;
}

void tesselate_bezier_quadratic(pos2 **pts, pos2 p0, pos2 p1, pos2 p2, int steps) {

	float step_val = 1.0f/steps;
	for(int i = 1; i <= steps; ++i) {
		float t = step_val*i;

		float x = p0.x*(1.0f-t)*(1.0f-t) + p1.x*2.0f*(1.0f-t)*t + p2.x*t*t;
		float y = p0.y*(1.0f-t)*(1.0f-t) + p1.y*2.0f*(1.0f-t)*t + p2.y*t*t;
		pos2 new_point = {x, y};//sh_vec_add(&p0_mod, &p1_mod);
		buf_push(*pts, new_point);
	}
}

pos2* generate_points(sh_glyph_outline *glyph, float scale, int tesselate_amount, int *pts_len, int *num_contour, int **contour_end_index) {

	pos2 *pts = NULL;//malloc(sizeof(pos2)*256);
	int contour_start = 1;
	int first_point_off = 0;

	int x_offset = glyph->p1.x;
	int y_offset = glyph->p1.y;

	*num_contour = glyph->contour_count;

	int i =0;
	pos2 pflt;
	f32 m = 0;
	for(int j = 0; j < *num_contour; ++j) {
		int i = j != 0 ? glyph->contour_last_index[j-1]+1 : 0;
		int x = i;
		contour_start = 1;

		for(; i < glyph->contour_last_index[j]; ++i) {
			sh_font_point *p = glyph->points + i;;
			pflt.x = (p->x - x_offset)*scale;
			m = (p->x - x_offset)*scale;
			pflt.y = (p->y - y_offset)*scale;
			if(p->flag.on_curve) {
				buf_push(pts, pflt);
			} else {
				pos2 mid_p = {0};
				if(!p[1].flag.on_curve) {
					mid_p.x = p->x + (f32)( (p[1].x - p->x)/2 );
					mid_p.y = p->y + (f32)( (p[1].y - p->y)/2 );
				} else {
					mid_p.x = p[1].x;
					mid_p.y = p[1].y;
				}

				mid_p.x -= x_offset;
				mid_p.x *= scale;

				mid_p.y -= y_offset;
				mid_p.y *= scale;

				if(contour_start) {
					first_point_off = 1;
					//we are starting on an off curve
					if(p[1].flag.on_curve) continue; //next point on curve then just move to the next
					buf_push(pts, mid_p); continue; //point after is off curve too so just make a mid out of the two
				}

				int len = buf_len(pts)-1;
				tesselate_bezier_quadratic(&pts, pts[len], pflt, mid_p, tesselate_amount);
			}

			contour_start = 0;
		}

		sh_font_point *p = glyph->points + i;
		pflt = (pos2){(p->x-x_offset)*scale, (p->y - y_offset)*scale};
		if(first_point_off) {
			if(p->flag.on_curve)  {
				buf_push(pts, pflt);
			} else {
				pos2 mid_p = (pos2) {
					(p->x + ((glyph->points[x].x - p->x)/2) - x_offset)*scale,
					(p->y + ((glyph->points[x].y - p->y)/2) - y_offset)*scale
				};
				tesselate_bezier_quadratic(&pts, pts[buf_len(pts)-1], pflt , mid_p, tesselate_amount);
			}
		} else {
			buf_push(pts, pflt);
		}

		int last_point_contour = j ? (*contour_end_index)[j-1] : 0;
		buf_push(pts, pts[last_point_contour]); //add the first point again so we have an actual loop
		buf_push(*contour_end_index, buf_len(pts)); //mark the end of the contour
		first_point_off = 0;

	}

	return pts;
}

//@mem(temp_texture.bitmap.mem, UINT8, 1, temp_texture.bitmap.x, temp_texture.bitmap.y, temp_texture.bitmap.x)
//@mem(mem, UINT8, 1, w, h, w)

void rasterize_glyph(line *edges, int num_edges, unsigned char *mem, int m_size, int w, int h) {

	if(!edges) return;
	qsort(edges, num_edges, sizeof(line), comp_sh_point);
	// memset(mem, 0, m_size);
	intersection *edge_intersection = NULL;

	int div_scanline = 15;
	float alpha = 255.0f/div_scanline;
	float step_per_line = 1.0f/div_scanline;
	int from_edge = 0;

	for(int i = 0; i < h; ++i) {
		int scanline = i;

		for(int n = 0; n < div_scanline; ++n) {

			float scanline_offset = scanline + n*step_per_line;
			intersection **update_head = &edge_intersection;

			while(update_head && *update_head) {
				line *edge = (*update_head)->edge;
				float bigger_y = MAX(edge->y1, edge->y2);

				if(bigger_y < scanline_offset) {
					intersection *free_node = *update_head;
					(*update_head) = (*update_head)->next;
					free(free_node);
					continue;
				}

				(*update_head)->intersect += (*update_head)->slope;
				update_head = &(*update_head)->next;
			}

			for(int j = from_edge; j < num_edges; ++j) {
				line *edge = edges + j;

				float bigger_y = MAX(edge->y1, edge->y2);
				float smaller_y = MIN(edge->y1, edge->y2);

				if(smaller_y >= scanline_offset) {
					from_edge = j;
					break;
				}

				if(bigger_y < scanline_offset) continue;

				float dy = edge->y2 - edge->y1;
				float dx = edge->x2 - edge->x1;
				float slope = dx/dy;
				float x_intersect = -1;
				if(dx == 0) {
					x_intersect = edge->x1;
				} else {
					x_intersect = ((scanline_offset - edge->y1)*slope + edge->x1);
					if(x_intersect < 0) {
						x_intersect = dx > 0 ? edge->x1 : edge->x2;
					} else if(x_intersect > w) {
						x_intersect = dx > 0 ? edge->x2 : edge->x1;
					}
				}

				intersection intrsct = {
					.slope = slope*step_per_line,
					.edge = edge,
					.slope_direction = dy > 0 ? 1 : -1,
					.intersect = x_intersect,
				};

				add_to_linked_list(&edge_intersection, &intrsct);
				from_edge++;
			}

			bubble_sort_intersection(&edge_intersection);
			unsigned char *current_scanline = mem + scanline*w;

			if(edge_intersection) {
				int first_pixel_index = (int)edge_intersection->intersect;
				float f_pixel = edge_intersection->intersect;
				float l_pixel = 0;

				int last_pixel_index = -1;
				int count_edge_direction = edge_intersection->slope_direction;
				intersection *edge_check = edge_intersection->next;

				while(edge_check) {
					if(count_edge_direction == 0) {
						first_pixel_index = (int)edge_check->intersect;
						f_pixel = edge_check->intersect;
						count_edge_direction += edge_check->slope_direction;
						edge_check = edge_check->next;
						continue;
					}

					count_edge_direction += edge_check->slope_direction;

					if(count_edge_direction == 0) {
						last_pixel_index = (int)edge_check->intersect;
						l_pixel = edge_check->intersect;

						if(first_pixel_index == last_pixel_index) {
							float cover = l_pixel - f_pixel;
							if(cover == 0) continue;
							current_scanline[first_pixel_index] += (u8)(alpha*cover );
						} else {
							float cover = 1 + (first_pixel_index) - f_pixel;
							current_scanline[first_pixel_index] += (u8)( alpha*cover );
							cover =  l_pixel - last_pixel_index ;
							current_scanline[last_pixel_index] += (u8)( alpha*cover );

							for(int j = first_pixel_index + 1; j < last_pixel_index; ++j) {
								current_scanline[j] += (u8)alpha;
							}
						}
					}

					edge_check = edge_check->next;
				}

			}

		}
	}

}


void render_letter(unsigned char *mem, int m_size, int m_w, int m_h, font_directory *font, char letter, int font_size_pixel) {

	sh_glyph_outline g = get_glyph_outline(font, letter);

	// int h = g.p2.y - g.p1.y;
	int as_ds = font->hhea.ascent - font->hhea.descent;
	float ft_scale = (float) font_size_pixel/(as_ds);

	int num_contours = 0;
	int *contour_ends_index = NULL;
	int num_edges = 0;
	int pts_len = 0;

	pos2 *pts = generate_points(&g, ft_scale, 5, &pts_len, &num_contours, &contour_ends_index); //the amount of tessilation affects the speed a ton
	line *edges = generate_edges(pts, num_contours, contour_ends_index, &num_edges);
	rasterize_glyph(edges, num_edges, mem, m_size, m_w, m_h);

	buf_free(contour_ends_index);
	free(edges);
	buf_free(pts);
	sh_free_glyph_outline(&g);

}

void setup(void) {

	// abcdefg
	gl_ctx = (sh_window_context){ 500, 500, 0, 0, 0, "editor" };
	gl_ctx.font_size = 24;
	gl_ctx.quad_height = 64;
	gl_ctx.quad_width = 64;
	buf_fit(gl_ctx.text, 256);
	memset(gl_ctx.text, 0, 256);
	buf_push(gl_ctx.text, 'A');
	sh_window_setup();
	glClearColor(61.0f/255.0f, 41.0f/255.0f, 94.0f/255.0f, 1);

	size_t mem_size = 0;
	char* ttf_file = read_file("yes.ttf", &mem_size);
	char* file_loc = ttf_file;
	font = sh_init_font(ttf_file);


}


typedef struct split_lines {
	char **lines;
	i32 *line_length;
	i32 line_nums;
} split_lines;

split_lines split_text(char *text, i32 str_length) {


	split_lines lines = {0};


	if(str_length <= 0) return lines;

	buf_push(lines.lines, text);
	buf_push(lines.line_length, 0);
	lines.line_nums++;


	for(i32 i = 0; i < str_length; i++, text++) {

		if(text[0] == '\n' || text[0] == '\r') {
			char *prev_line = lines.lines[lines.line_nums-1];
			lines.line_length[lines.line_nums-1] = (i32)( text - prev_line );;

			buf_push(lines.lines, text + 1);
			buf_push(lines.line_length, 0);

			lines.line_nums++;
		}
	}

	char *prev_line = lines.lines[lines.line_nums-1];
	lines.line_length[lines.line_nums-1] = (i32)( text - prev_line );

	return lines;
}

typedef struct render_context {
	f32 next_x;
	f32 next_y;
	f32 row_height; // pixels
	f32 left_margin;
	f32 top_margin;
} render_context;

typedef struct cursor {
	i32 row_number;
	i32 column_number;
} cursor;

typedef struct text_rect {
	f32 x;
	f32 y;
	f32 width;
	f32 height;
	f32 u_s;
	f32 v_s;
	f32 u_e;
	f32 v_e;
} text_rect;

render_context ren_ctx = {0, 0, 64, 5, 3};
cursor curs = {0, 0};
text_rect *line_quads = NULL;

i32 quad_vbo = 0;
i32 index_vbo = 0;
i32 *indices = NULL;
i32 index_count = 0;
pos2 *quad_vertices = NULL;
i32 vert_count = 0;

void setup_line_quads(split_lines lines) {
	i32 index_start = 0;

	for(i32 i = 0; i < buf_len(line_quads); i++) {

		text_rect *r = line_quads + i;

		buf_fit(quad_vertices, 8);

		buf_push(quad_vertices, (pos2){r->x, r->y - r->height});
		buf_push(quad_vertices, (pos2){r->x, r->y});
		buf_push(quad_vertices, (pos2){r->x + r->width, r->y});
		buf_push(quad_vertices, (pos2){r->x + r->width, r->y - r->height});

		buf_push(quad_vertices, (pos2){r->u_s, r->v_s});
		buf_push(quad_vertices, (pos2){r->u_s, r->v_e});
		buf_push(quad_vertices, (pos2){r->u_e, r->v_e});
		buf_push(quad_vertices, (pos2){r->u_e, r->v_s});


		buf_push(indices, index_start + 0);
		buf_push(indices, index_start + 1);
		buf_push(indices, index_start + 2);

		buf_push(indices, index_start + 0);
		buf_push(indices, index_start + 2);
		buf_push(indices, index_start + 3);

		vert_count += 8;
		index_start += 8;
		index_count += 6;

	}
}

void setup_quad_vbo(void) {
	// this is dumb

	if(quad_vbo == 0) {
		glGenBuffers(1, &quad_vbo);
	}

	glBindBuffer(GL_ARRAY_BUFFER, quad_vbo);
	glBufferData(GL_ARRAY_BUFFER, sizeof(pos2)*vert_count, quad_vertices, GL_DYNAMIC_DRAW);

	if(index_vbo == 0) {
		glGenBuffers(1, &index_vbo);
	}

	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, index_vbo);
	// printf("%d\n", (vert_count/8)*6 );
	// fflush(stdout);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(i32)*(vert_count/8)*6, indices, GL_DYNAMIC_DRAW);
}


i8 once = 1;
void render_text(char *text) {

	if(text == NULL) return;


	i32 y = (i32)strlen(text);
	split_lines l = split_text(text, y);
	if(l.line_nums == 0)
		gl_ctx.line_numbers = 1;
	else
		gl_ctx.line_numbers = l.line_nums;

	int as_ds = font->hhea.ascent - font->hhea.descent;
	float ft_scale = (float) gl_ctx.font_size/(as_ds);

#if USE_FREE_TYPE
	f32 w = (f32)gl_ctx.ft_atlas_width;
	f32 h = (f32)gl_ctx.ft_atlas_height;
#else 
	f32 w = (f32)gl_ctx.atlas_width;
	f32 h = (f32)gl_ctx.atlas_height;
#endif

	f32 top = ren_ctx.next_y - ren_ctx.top_margin;

#if USE_FREE_TYPE
	f32 baseline_y = top - font->hhea.ascent*ft_scale - 10;
#else
	f32 baseline_y = top - font->hhea.ascent*ft_scale;
#endif

	for(i32 i = 0; i < l.line_nums; i++) {


		for(i32 j = 0; j < l.line_length[i]; j++) {

#if USE_FREE_TYPE

			character_info *c = gl_ctx.ft_char_cache + (l.lines[i][j] - ' ');
#else
			character_info *c = gl_ctx.char_cache + (l.lines[i][j] - ' ');
#endif

			pos4 char_uv = c->uv;
			pos2 size = {char_uv.z, char_uv.w};
			pos2 size_uv = {char_uv.z, char_uv.w};


#if USE_FREE_TYPE
			buf_push(line_quads,
					(text_rect){
					(f32)ren_ctx.next_x + ren_ctx.left_margin + gl_ctx.left_add,
					(f32)baseline_y + size.y + c->y_offset + gl_ctx.top_add,
					size.x,
					size.y,
					char_uv.x/w,
					( char_uv.y + size_uv.y )/h,
					( char_uv.x + size_uv.x )/w,
					char_uv.y/h,
					});

			if(once) {
				printf("(%f %f) (%f %f)/(%f %f) (%f %f)/(%f %f) (%f %f)\n",
						size.x, size.y,
						char_uv.x/w, char_uv.y/h,
						char_uv.x, char_uv.y,
						( char_uv.x + size_uv.x )/w, ( char_uv.y + size_uv.y )/h,
						( char_uv.x + size_uv.x ), ( char_uv.y + size_uv.y ),
						w, h
					  );
				once = 0;
			}
#else 
			buf_push(line_quads,
					(text_rect){
					(f32)ren_ctx.next_x + ren_ctx.left_margin + gl_ctx.left_add,
					(f32)baseline_y + size.y + c->y_offset + gl_ctx.top_add,
					size.x,
					size.y,
					char_uv.x/w,
					char_uv.y/h,
					( char_uv.x + size_uv.x )/w,
					( char_uv.y + size_uv.y )/h
					});


#endif

			ren_ctx.next_x += size.x;
		}



		ren_ctx.next_x = 0;
		baseline_y -= gl_ctx.font_size;
	}

	setup_line_quads(l);
}


void draw_rect(f32 x, f32 y, f32 width, f32 height) {

	i32 backup_poly_mode = 0;
	glGetIntegerv(GL_POLYGON_MODE, &backup_poly_mode);

	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	u32 buf = 0;

	pos2 quad[6] = {
		{x, y},
		{x, y+height},
		{x+width, y+height},

		{x, y},
		{x+width, y+height},
		{x+width, y},

	};

	glGenBuffers(1, &buf);
	glBindBuffer(GL_ARRAY_BUFFER, buf);
	glBufferData(GL_ARRAY_BUFFER, sizeof(pos2)*6, quad, GL_STATIC_DRAW);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, 0);
	glDrawArrays(GL_TRIANGLES, 0, 6);

	glDeleteBuffers(1, &buf);
	glPolygonMode(GL_FRONT_AND_BACK, backup_poly_mode);
}


void draw_rect_outline(f32 x, f32 y, f32 width, f32 height) {

	i32 backup_poly_mode = 0;
	glGetIntegerv(GL_POLYGON_MODE, &backup_poly_mode);
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	u32 buf = 0;

	pos2 quad[4] = {
		{x, y},
		{x, y+height},
		{x+width, y+height},
		{x+width, y},

	};

	glGenBuffers(1, &buf);
	glBindBuffer(GL_ARRAY_BUFFER, buf);
	glBufferData(GL_ARRAY_BUFFER, sizeof(pos2)*4, quad, GL_STATIC_DRAW);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, 0);
	glDrawArrays(GL_LINE_LOOP, 0, 4);

	glDeleteBuffers(1, &buf);
	glPolygonMode(GL_FRONT_AND_BACK, backup_poly_mode);
}


void draw_baseline(i32 lines) {

	u32 bufs[2] = {0};
	glGenBuffers(1, bufs);

	pos4 *line = NULL;
	buf_fit(line, lines);

	for(i32 i = 0; i < lines; i++) {
		buf_push(line, (pos4){
				ren_ctx.left_margin, gl_ctx.height - ren_ctx.top_margin - gl_ctx.font_size*i,
				ren_ctx.left_margin + gl_ctx.width, gl_ctx.height - ren_ctx.top_margin - gl_ctx.font_size*i,
				});
	}

	glBindBuffer(GL_ARRAY_BUFFER, bufs[0]);
	glBufferData(GL_ARRAY_BUFFER, sizeof(pos4)*lines, line, GL_STATIC_DRAW);
	glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, 0);
	pos4 red = {1, 0, 0, 1};
	glUniform4fv(2, 1, &red.x);
	glUniform1i(5, 0);

	glDrawArrays(GL_LINES, 0, lines*2);
	glDeleteBuffers(1, bufs);

	int as_ds = font->hhea.ascent - font->hhea.descent;
	float ft_scale = (float) gl_ctx.font_size/(as_ds);
	i32 h = font->hhea.ascent - font->hhea.descent;
	draw_rect_outline(ren_ctx.left_margin, gl_ctx.height - ren_ctx.top_margin, 20, -h*ft_scale);

}

void draw_text_atlas(i32 x, i32 y) {
	u32 atlas_vbo = 0;
	glGenBuffers(1, &atlas_vbo);
	pos2 pos = {(f32)x, (f32)y};

	pos2 quad[] = {
		{pos.x, pos.y},
		{pos.x, pos.y + (f32)gl_ctx.atlas_height},
		{pos.x + (f32)gl_ctx.atlas_width, pos.y + (f32)gl_ctx.atlas_height},

		{pos.x, pos.y},
		{pos.x + (f32)gl_ctx.atlas_width, pos.y + (f32)gl_ctx.atlas_height},
		{pos.x + (f32)gl_ctx.atlas_width, pos.y},

		{0, 0},
		{0, 1},
		{1, 1},

		{0, 0},
		{1, 1},
		{1, 0},
	};
	

	glBindBuffer(GL_ARRAY_BUFFER, atlas_vbo);
	glBufferData(GL_ARRAY_BUFFER, sizeof(pos2)*12, quad, GL_STATIC_DRAW);

	glEnableVertexAttribArray(1);
	glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, 0);
	glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 0, (void*)(sizeof(pos2)*6));
	pos4 black = {1, 1, 1, 1};
	glUniform4fv(2, 1, &black.x);

	glBindTexture(GL_TEXTURE_2D, gl_ctx.font_atlas_texture);

	glDrawArrays(GL_TRIANGLES, 0, 6);
	glBindBuffer(GL_ARRAY_BUFFER, 0);
	glDeleteBuffers(1, &atlas_vbo);

}

void draw_ft_text_atlas(i32 x, i32 y) {
	u32 atlas_vbo = 0;
	glGenBuffers(1, &atlas_vbo);
	pos2 pos = {(f32)x, (f32)y};

	pos2 quad[] = {
		{pos.x, pos.y},
		{pos.x, pos.y + (f32)gl_ctx.ft_atlas_height},
		{pos.x + (f32)gl_ctx.ft_atlas_width, pos.y + (f32)gl_ctx.ft_atlas_height},

		{pos.x, pos.y},
		{pos.x + (f32)gl_ctx.ft_atlas_width, pos.y + (f32)gl_ctx.ft_atlas_height},
		{pos.x + (f32)gl_ctx.ft_atlas_width, pos.y},

		{0, 0},
		{0, 1},
		{1, 1},

		{0, 0},
		{1, 1},
		{1, 0},
	};
	

	glBindBuffer(GL_ARRAY_BUFFER, atlas_vbo);
	glBufferData(GL_ARRAY_BUFFER, sizeof(pos2)*12, quad, GL_STATIC_DRAW);

	glEnableVertexAttribArray(1);
	glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, 0);
	glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 0, (void*)(sizeof(pos2)*6));
	pos4 black = {1, 1, 1, 1};
	glUniform4fv(2, 1, &black.x);

	glBindTexture(GL_TEXTURE_2D, gl_ctx.ft_atlas_texture);

	glDrawArrays(GL_TRIANGLES, 0, 6);
	glBindBuffer(GL_ARRAY_BUFFER, 0);
	glDeleteBuffers(1, &atlas_vbo);

}

void draw_letter(char c) {
	pos2 position = {10, 10};
	pos4 char_uv = gl_ctx.char_cache[(c - ' ')].uv;
	pos2 size = {char_uv.z, char_uv.w};
	pos2 size_uv = {char_uv.z, char_uv.w};

	f32 w = (f32)gl_ctx.atlas_width;
	f32 h = (f32)gl_ctx.atlas_height;

	u32 letter_vbo = 0;;

	pos2 letter_data[] = {
		{position.x, position.y},
		{position.x, position.y + size.y},
		{position.x + size.x, position.y + size.y},

		{position.x, position.y},
		{position.x + size.x, position.y + size.y},
		{position.x + size.x, position.y},


		{char_uv.x/w, char_uv.y/h},
		{( char_uv.x )/w, ( char_uv.y + size_uv.y )/h},
		{( char_uv.x + size_uv.x )/w, ( char_uv.y + size_uv.y )/h},

		{char_uv.x/w, char_uv.y/h},
		{( char_uv.x + size_uv.x )/w, ( char_uv.y + size_uv.y )/h},
		{( char_uv.x + size_uv.x )/w, ( char_uv.y )/h},
	};


	glGenBuffers(1, &letter_vbo);
	glBindBuffer(GL_ARRAY_BUFFER, letter_vbo);
	glBufferData(GL_ARRAY_BUFFER, sizeof(pos2)*12, letter_data, GL_STATIC_DRAW);

	glEnableVertexAttribArray(1);
	glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, 0);
	glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 0, (void*)(sizeof(pos2)*6));
	glUniform1i(5, 1);
	pos4 black = {1, 1, 1, 1};
	glUniform4fv(2, 1, &black.x);

	glBindTexture(GL_TEXTURE_2D, gl_ctx.font_atlas_texture);

	glDrawArrays(GL_TRIANGLES, 0, 6);
	glBindBuffer(GL_ARRAY_BUFFER, 0);
	glDeleteBuffers(1, &letter_vbo);


}

char letter = 'A';

void render(void) {


	render_text(gl_ctx.text);
	setup_quad_vbo();

	// glUniform1i(5, 1);
	// glUniform1i(6, 0);
	// draw_text_atlas(0, 0);

	// glUniform1i(5, 1);
	// glUniform1i(6, 1);
	// draw_ft_text_atlas(gl_ctx.atlas_width + 10, 0);

	// glUniform1i(6, 1);

	glBindBuffer(GL_ARRAY_BUFFER, quad_vbo);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, 0);
	glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 0, (void*)(sizeof(pos2)*4));
	glUniform1i(5, 1);

#if USE_FREE_TYPE
	glBindTexture(GL_TEXTURE_2D, gl_ctx.ft_atlas_texture);
	glUniform1i(6, 1);
#else
	glBindTexture(GL_TEXTURE_2D, gl_ctx.font_atlas_texture);
	glUniform1i(6, 0);
#endif

	pos4 yellow = {1, 1, 1, 1};
	glUniform4fv(2, 1, &yellow.x);

	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, index_vbo);
	glPolygonMode( GL_FRONT_AND_BACK, GL_FILL );
	glDrawElements(GL_TRIANGLES, index_count, GL_UNSIGNED_INT , 0);

	pos4 black = {1, 1, 1, 1};
	glUniform4fv(2, 1, &black.x);
	
	buf_clear(indices);
	buf_clear(quad_vertices);
	buf_clear(line_quads);
	vert_count = 0;
	ren_ctx.next_y = 500;
	ren_ctx.next_x = 0;
	index_count = 0;

}

void reset_global_context() {
	gl_ctx.input[0] = '\0';
	gl_ctx.input_size = 0;
}

void delete_character(void) {
	//handle cursor location
	
	//delete from the end
	buf_pop(gl_ctx.text);
}


void add_character(char c) {
	buf_push(gl_ctx.text, c);
}


void mem_copy_square(u8 *dest, u8 *source, i32 w, i32 h, i32 stride, i32 dest_stride) {
	for(i32 i = 0; i < h; i++) {
		memcpy(dest + i*dest_stride, source + i*stride, stride);
	}
}


void setup_font_atlas() {

	int as_ds = font->hhea.ascent - font->hhea.descent;
	float ft_scale = (float) gl_ctx.font_size/(as_ds);

	i32 max_char_width  =  (i32)( ( font->head.xMax - font->head.xMin ) * ft_scale );
	i32 max_char_height =  (i32)( (as_ds) * ft_scale );


	i32 char_count = 94;
	char start_char = ' ';

	i32 pixel_gap = 2;
	i32 char_per_row = ((i32)sqrt(char_count)) + 1;
	i32 atlas_width  = ( max_char_width + pixel_gap )*char_per_row;

	i32 rows = char_count/char_per_row;

	i32 atlas_height = 	( max_char_height + pixel_gap )*rows;

	gl_ctx.font_atlas = (u8*)calloc((atlas_height)*(atlas_width), sizeof(u8));

	gl_ctx.atlas_height = atlas_height;
	gl_ctx.atlas_width = atlas_width;

	char *mem_loc = gl_ctx.font_atlas;

	i32 x = 0;
	i32 y = 0;

	i32 max_row_height = 0;


	for(i32 i = start_char; i < start_char + char_count; i++ ) {

		// render_letter(int m_size, int m_w, int m_h, font_directory *font, char letter, int font_size_pixel) {
		render_letter(
				mem_loc + x + y*atlas_width,
				atlas_width*atlas_height,
				atlas_width,
				gl_ctx.font_size,
				font,
				(char)i,
				gl_ctx.font_size
				);

		sh_glyph_outline g = get_glyph_outline(font, i);
		i32 w = (i32)( ( g.p2.x - g.p1.x )*ft_scale );
		i32 h = (i32)( ( g.p2.y - g.p1.y )*ft_scale );

		
		gl_ctx.char_cache[i - start_char].uv.x = (f32)x;
		gl_ctx.char_cache[i - start_char].uv.y = (f32)y;
		gl_ctx.char_cache[i - start_char].uv.z = (f32)( w+pixel_gap );
		gl_ctx.char_cache[i - start_char].uv.w = (f32)( h+pixel_gap );

		gl_ctx.char_cache[i - start_char].x_offset = (i32)( g.p1.x*ft_scale );
		gl_ctx.char_cache[i - start_char].y_offset = (i32)( g.p1.y*ft_scale );

		if(h > max_row_height) max_row_height = h;

		x += (i32)(w) + pixel_gap;
		if((x +(i32)(w)+pixel_gap) > atlas_width ) {
			y += max_row_height + pixel_gap;
			max_row_height = 0;
			x = 0;
		}

		
	}



	glGenTextures(1, &gl_ctx.font_atlas_texture);
	glBindTexture(GL_TEXTURE_2D, gl_ctx.font_atlas_texture);

	// glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	// glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);


	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

	glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

	glTexImage2D(
			GL_TEXTURE_2D, 0, GL_RED, atlas_width, atlas_height, 0,
			GL_RED, GL_UNSIGNED_BYTE, gl_ctx.font_atlas);

	glPixelStorei(GL_UNPACK_ALIGNMENT, 4);


}


void setup_font_atlas_freetype(void) {

	FT_Library lib;
	FT_Face face;

	if(FT_Init_FreeType(&lib) != 0) {
		puts("failed to init freetype");
		exit(1);
	}

	if(FT_New_Face(lib, "./envy.ttf", 0, &face) != 0) {
		puts("new face failed");
	}


	FT_Set_Pixel_Sizes(face, 0, gl_ctx.font_size);

	i32 char_count = 94;
	char start_char = ' ';




	i32 max_char_width = face->size->metrics.x_ppem;
	i32 max_char_height = face->size->metrics.y_ppem;

	i32 pixel_gap = 2;
	i32 char_per_row = ((i32)sqrt(char_count)) + 1;
	i32 rows = char_count/char_per_row;

	i32 atlas_width  = ( max_char_width + pixel_gap )*char_per_row;
	i32 atlas_height = ( max_char_height + pixel_gap )*rows;
	gl_ctx.ft_atlas_height = atlas_height;
	gl_ctx.ft_atlas_width = atlas_width;

	gl_ctx.ft_atlas = (u8*)calloc((atlas_height)*(atlas_width), sizeof(u8));



	FT_GlyphSlot glyph_slot = face->glyph;

	i32 x = 0;
	i32 y = 0;

	for(i32 i = start_char; i < start_char + char_count; i++) {
		u32 glyph_index = FT_Get_Char_Index(face, i);
		FT_Load_Glyph(face, glyph_index, FT_LOAD_DEFAULT);
		FT_Render_Glyph(glyph_slot, FT_RENDER_MODE_NORMAL);

		u8 *bitmap_buf = glyph_slot->bitmap.buffer;
		i32 width = glyph_slot->bitmap.width;
		i32 height = glyph_slot->bitmap.rows;
		i32 pitch = glyph_slot->bitmap.pitch;

		mem_copy_square(gl_ctx.ft_atlas + x + y*atlas_width, bitmap_buf, width, height, pitch, atlas_width);

		
		gl_ctx.ft_char_cache[i - start_char].uv.x = (f32)x;
		gl_ctx.ft_char_cache[i - start_char].uv.y = (f32)y;
		gl_ctx.ft_char_cache[i - start_char].uv.z = (f32)( width);
		gl_ctx.ft_char_cache[i - start_char].uv.w = (f32)( height);

		x += width + pixel_gap;
		if((x + max_char_width) > atlas_width) {
			y += max_char_height + pixel_gap;
			x = 0;
		}


	}
	
	fflush(stdout);

	glGenTextures(1, &gl_ctx.ft_atlas_texture);
	glBindTexture(GL_TEXTURE_2D, gl_ctx.ft_atlas_texture);

	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

	// glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	// glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);


	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

	glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

	glTexImage2D(
			GL_TEXTURE_2D, 0, GL_RED, atlas_width, atlas_height, 0,
			GL_RED, GL_UNSIGNED_BYTE, gl_ctx.ft_atlas);

	glPixelStorei(GL_UNPACK_ALIGNMENT, 4);

}

int main(void) {

	setup();

	// glPolygonMode( GL_FRONT_AND_BACK, GL_LINE );

	setup_font_atlas();
	setup_font_atlas_freetype();

	while(!gl_ctx.should_close) {
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

#ifdef USE_GLFW
		int scancode = glfwGetKey(gl_ctx.window, GLFW_KEY_ESCAPE);
		if(scancode == GLFW_PRESS) {
			gl_ctx.should_close = 1;
		}

		glfwPollEvents();
#else

		handle_events();
#endif

		if(gl_ctx.input_size > 0) {

			if(buf_len(gl_ctx.text) > 0) {
				buf__hdr(gl_ctx.text)->size--;
			}

			for(i32  i = 0; i < gl_ctx.input_size; i++) {
				if(gl_ctx.input[i] == '\b') {
					delete_character();
				} else {
					add_character(gl_ctx.input[i]);
				}
			}

			buf_push(gl_ctx.text, '\0');
		}

		render();

#ifdef USE_GLFW
		glfwSwapBuffers(gl_ctx.window);
#else
		SwapBuffers(gl_ctx.hdc);
#endif

		reset_global_context();
	}


	return 0;
}
