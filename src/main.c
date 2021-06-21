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
#include FT_FREETYPE_H

#define STB_TRUETYPE_IMPLEMENTATION
#include "stb_truetype.h"

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
	i32 font_atlas_texture;
} sh_window_context;

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

	int div_scanline = 5;
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

	int h = g.p2.y - g.p1.y;
	int as_ds = font->hhea.ascent - font->hhea.descent;
	float ft_scale = (float) font_size_pixel/(as_ds);

	int num_contours = 0;
	int *contour_ends_index = NULL;
	int num_edges = 0;
	int pts_len = 0;

	pos2 *pts = generate_points(&g, ft_scale, 3, &pts_len, &num_contours, &contour_ends_index); //the amount of tessilation affects the speed a ton
	line *edges = generate_edges(pts, num_contours, contour_ends_index, &num_edges);
	rasterize_glyph(edges, num_edges, mem, m_size, m_w, m_h);

	buf_free(contour_ends_index);
	free(edges);
	buf_free(pts);
	sh_free_glyph_outline(&g);

}







void setup(void) {

	gl_ctx = (sh_window_context){ 500, 500, 0, 0, 0, "editor" };
	gl_ctx.font_size = 64;
	gl_ctx.quad_height = 64;
	gl_ctx.quad_width = 64;
	sh_window_setup();
	glClearColor(61.0f/255.0f, 41.0f/255.0f, 94.0f/255.0f, 1);

	size_t mem_size = 0;
	char* ttf_file = read_file("envy.ttf", &mem_size);
	char* file_loc = ttf_file;
	font = sh_init_font(ttf_file);


}



char *text = NULL;

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
} text_rect;

render_context ren_ctx = {0, 500, 12, 5, 3};
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

	for(i32 i = 0; i < lines.line_nums; i++) {

		text_rect *r = line_quads + i;

		buf_fit(quad_vertices, 4);

		buf_push(quad_vertices, (pos2){r->x, r->y - r->height});
		buf_push(quad_vertices, (pos2){r->x, r->y});
		buf_push(quad_vertices, (pos2){r->x + r->width, r->y});
		buf_push(quad_vertices, (pos2){r->x + r->width, r->y - r->height});

		buf_push(indices, index_start + 0);
		buf_push(indices, index_start + 1);
		buf_push(indices, index_start + 2);

		buf_push(indices, index_start + 0);
		buf_push(indices, index_start + 2);
		buf_push(indices, index_start + 3);

		vert_count += 4;
		index_start += 4;
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
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(i32)*(vert_count/4)*6, indices, GL_DYNAMIC_DRAW);
}

void render_text(char *text) {

	if(text == NULL) return;

	i32 y = (i32)strlen(text);
	split_lines l = split_text(text, y);

	for(i32 i = 0; i < l.line_nums; i++) {
		buf_push(line_quads,
				(text_rect){
					(f32)ren_ctx.next_x + ren_ctx.left_margin,
					(f32)ren_ctx.next_y - ren_ctx.top_margin,
					(f32)(l.line_length[i]*gl_ctx.font_size),
					(f32)gl_ctx.font_size
				});
		ren_ctx.next_y -= gl_ctx.font_size;
	}

	setup_line_quads(l);
}





void render(void) {

	// render_text(text);
	// setup_quad_vbo();


	// glBindBuffer(GL_ARRAY_BUFFER, quad_vbo);
	// glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, 0);

	// glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, index_vbo);

	// glDrawElements(GL_TRIANGLES, index_count, GL_UNSIGNED_INT , 0);

	// buf_clear(indices);
	// buf_clear(quad_vertices);
	// buf_clear(line_quads);
	// vert_count = 0;
	// ren_ctx = (render_context){0, 500, 12, 5, 3};

	glBindTexture(GL_TEXTURE_2D, gl_ctx.font_atlas_texture);

	// render_line_quads();
}

void reset_global_context() {
	gl_ctx.input[0] = '\0';
	gl_ctx.input_size = 0;
}

void delete_character(void) {
	//handle cursor location
	
	//delete from the end
	buf_pop(text);
}


void add_character(char c) {
	buf_push(text, c);
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
	printf("rows %d\n", rows);

	i32 atlas_height = 	( max_char_height + pixel_gap )*rows;

	gl_ctx.font_atlas = (u8*)calloc((atlas_height)*(atlas_width), sizeof(u8));
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

	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

	glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

	glTexImage2D(
			GL_TEXTURE_2D, 0, GL_RED, atlas_width, atlas_height, 0,
			GL_RED, GL_UNSIGNED_BYTE, gl_ctx.font_atlas);

	glPixelStorei(GL_UNPACK_ALIGNMENT, 4);


}


int main(void) {

	setup();

	setup_font_atlas();

	while(!gl_ctx.should_close) {
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

		handle_events();

		if(gl_ctx.input_size > 0) {

			if(buf_len(text) > 0) {
				buf__hdr(text)->size--;
			}

			for(i32  i = 0; i < gl_ctx.input_size; i++) {
				if(gl_ctx.input[i] == '\b') {
					delete_character();
				} else {
					add_character(gl_ctx.input[i]);
				}
			}

			buf_push(text, '\0');
		}

		render();
		SwapBuffers(gl_ctx.hdc);
		reset_global_context();
	}


	return 0;
}
