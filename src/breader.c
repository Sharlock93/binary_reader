typedef struct bit_stream {
	u8 *stream;
	u64 bits;
	i32 available;
	i32 bit_header; // should be between 0 and 64
	i8 next_to_byte;
	u64 current_bit;
	u64 current_byte_stream;
} bit_stream;

bit_stream reader_head = {0};

u8 *data_stream = NULL;


void print_reader_head() {
	printf("bits: 0x%llx, av: %d, bh: %d\n", reader_head.bits, reader_head.available, reader_head.bit_header);
	printf("stream_head: %x %d\n", reader_head.stream[0], reader_head.next_to_byte);

	for(i32 i = 0; i < 64; i++) {
		if(i != 0  && i % 8 == 0) putchar(' ');
		i64 mask = (1LLU << (63 - i));
		if(reader_head.bits & mask)
			putchar('1');
		else 
			putchar('0');

	}

	putchar('\n');

	for(i32 i = 64; i > 0; i--) {
		if(i != 64 && i % 8 == 0) putchar(' ');
		if(i == reader_head.bit_header) break;
		else
			putchar(' ');
	}

	if(reader_head.bit_header != 0) 
		putchar('^');
	else 
		putchar('-');

	putchar('\n');
}

void clamp_bit_header() {
	if(reader_head.bit_header > 64) reader_head.bit_header = 64;
	if(reader_head.bit_header < 0) reader_head.bit_header = 0;
}
// float, i32, string

void read_data(sh_decl_val *t);
void setup_data(sh_decl_val *t);
void read_or_setup_data(sh_decl_val *t, i8 read_or_setup);
i32 read_stmt(sh_statement *st);
i32 read_decl_binary(sh_decl_val *val_decl, i8 read_or_setup);
sh_decl_val eval_expr(sh_expression *expr, i8);
sh_decl_val eval_expr_parent(sh_expression *e, i8 read_return_data, sh_decl_val *parent);
i64 do_op(sh_expr_operator op, i64 left_op, i64 right_op);
sh_decl_val do_op_decl_val(sh_expr_operator op, sh_decl_val left_op, sh_decl_val right_op);



i8 is_delayed_read_var(sh_decl *d) {
	if(d->var.init_expr != NULL && d->var.init_expr->type == SH_QUESTION_OPERATOR_EXPR) return 1;
	return 0;
}

i8 sh_has_decl_tag(sh_decl_val *v, sh_tag_type type) {
	if(v->decl && v->decl->tags != NULL) {
		for(i32 i = 0; i < buf_len(v->decl->tags); i++) {
			if(v->decl->tags[i]->type == type) { return 1; }
		}
	}
	return 0;
}


i8 is_tag_assert(sh_decl_tag *d) { return d->type == SH_ASSERT_TAG; }
i8 is_tag_cond(sh_decl_tag *d) { return d->type == SH_COND_TAG; }

i8 has_assert_tag(sh_decl_val *v) {
	return sh_has_decl_tag(v, SH_ASSERT_TAG);
}



i8 has_cond_tag(sh_decl_val *v) {
	return sh_has_decl_tag(v, SH_COND_TAG);
}


i8 cond_tag_failed(sh_decl_val *v) {

	if(v != NULL && v->tag_evals != NULL) {
		for(i32 i = 0; i < buf_len(v->tag_evals); i++) {
			if(v->tag_evals[i].type == SH_COND_TAG) {
				return !v->tag_evals[i].eval;
			}
		}
	}
	return 1;

}

i8 assert_tag_failed(sh_decl_val *v) {
	if(v == NULL  || v->tag_evals == NULL) return 0;
	for(i32 i = 0; i < buf_len(v->tag_evals); i++) {
		if(v->tag_evals[i].type == SH_ASSERT_TAG) {
			return !v->tag_evals[i].eval;
		}
	}

	return 0;
}


void read_bits_into_reader(i32 bits) {
	if(reader_head.next_to_byte > 0) {
		u8 byte = reader_head.stream[0];
		i32 smaller = MIN(bits, reader_head.next_to_byte);
		u8 mask = ( 1 << smaller ) - 1;

		i8 remaning_bits = reader_head.next_to_byte - smaller;
		mask <<= remaning_bits;
		byte &= mask;
		byte >>= remaning_bits;
		reader_head.bits <<= smaller;
		reader_head.bits |= byte;


		if(reader_head.next_to_byte - bits <= 0) {
			bits -= reader_head.next_to_byte;
			reader_head.next_to_byte = 0;
		} else {
			reader_head.next_to_byte -= smaller;
			bits -= smaller;
		}

		reader_head.bit_header += smaller;

		if(bits < 0) bits = 0;
		if(reader_head.next_to_byte == 0)
			reader_head.stream++;
	}


	if(reader_head.next_to_byte == 0) {

		i32 div = bits/8;
		i32 reminder = bits%8;

		if(reminder > 0 && reader_head.available + bits <= 64 ) {
			bits = (div + 1) * 8;
		}

		while(bits >= 8) {
			u8 byte = reader_head.stream[0];
			reader_head.stream++;

			reader_head.bits <<= 8;
			reader_head.bits |= byte;
			reader_head.available += 8;
			reader_head.bit_header += 8;
			bits -= 8;
		}

		if(bits > 0) {
			i8 remaining_to_byte = 8 - bits;
			reader_head.bits <<= bits;
			u8 byte = reader_head.stream[0];
			byte >>= remaining_to_byte;
			reader_head.bits |= byte;
			reader_head.next_to_byte = remaining_to_byte;
			reader_head.bit_header += bits;
		}

		if(reader_head.bit_header > 64) reader_head.bit_header = 64;
		if(reader_head.available > 64) reader_head.available = 64;

	}
}

void make_reader_fit_bits(i64 bits) {

	if(reader_head.bit_header < bits) {
		read_bits_into_reader((i32)( bits - reader_head.bit_header ));
	}
}

u64 reverse_bits(u64 bits, i32 bitcount) {
	u64 reverse = 0;

	while(bitcount > 0) {
		reverse <<= 1;
		reverse |= bits & 1;
		bits >>= 1;
		bitcount--;
	}

	return reverse;
}

u64 read_bits_from_reader(i64 bits_to_read) {
	u64 bits = 0;
	u64 mask = 1;

	for(int i = 0; i < bits_to_read; i++) {
		mask <<= 1;
	}

	mask = mask - 1;
	mask = mask << ( reader_head.bit_header -  bits_to_read);

	bits = reader_head.bits & mask;
	bits >>= (reader_head.bit_header -  bits_to_read);
	reader_head.bit_header -= (i32)bits_to_read;

	reader_head.current_bit += bits_to_read;
	reader_head.current_byte_stream += bits_to_read/8;

	return bits;
}


void rewind_reader_head_by_bits(i64 bits) {

	i64 bits_save = bits;

	if(bits + reader_head.bit_header <= 64) {

		if(reader_head.bit_header != reader_head.available) {
			reader_head.bit_header += (i32)bits;
		}

	} else {

		i32 rewind_byte = (i32)( bits/8 );
		i32 rewind_reminder = (i32)( bits%8 );
		i8 inc = 0;

		u8 *stream_before = reader_head.stream - 8;

		if(reader_head.next_to_byte > 0) {

			u64 byte = stream_before[0];
			i8 smaller = MIN(8 - reader_head.next_to_byte, (i8)bits);
			byte >>= reader_head.next_to_byte;
			i8 mask = mask = ( 1 << smaller ) - 1;
			byte &= mask;
			reader_head.bits >>= smaller;

			byte <<= (reader_head.bit_header - bits);
			reader_head.bits = reader_head.bits | byte;

			reader_head.next_to_byte += (i32)bits;
			bits -= smaller;

			if(reader_head.next_to_byte >= 8) {
				reader_head.next_to_byte = 0;
			}

		}

		if(bits < 0) bits = 0;

		if(bits > 0)  {

			stream_before--;
			inc = 0;
			if(rewind_reminder)
				reader_head.stream--;


			while(bits > 0) {

				u64 byte = stream_before[0];
				i32 smaller = MIN(8, (i32)bits);

				reader_head.bits >>= smaller;

				i8 mask = (1 << smaller) -1;
				byte = byte & mask;

				byte <<= reader_head.bit_header - smaller;
				reader_head.bits = reader_head.bits | byte;

				reader_head.next_to_byte += smaller;

				if(reader_head.next_to_byte >= 8) {
					stream_before--;
					reader_head.next_to_byte -= 8;
				}

				if(reader_head.next_to_byte == 0) {
					reader_head.stream--; 
				}

				bits -= smaller;
			}

			// reader_head.bit_header += rewind_byte*8;
			// reader_head.available += rewind_byte*8;


			if(reader_head.bit_header > 64) reader_head.bit_header = 64;
			if(reader_head.available > 64) reader_head.available = 64;


		}
	}

	reader_head.current_bit -= bits_save;
	reader_head.current_byte_stream -= bits_save/8;

}

void sh_seek_reader_head(sh_offset_tag offset_tag, i64 offset_amount_bit, sh_decl_val *v) {

	switch(offset_tag.start) {
		case SH_OFFSET_FILESTART: {
			i64 remaining_bits = offset_amount_bit - reader_head.current_bit;
			read_bits_from_reader(remaining_bits);
		} break;
		case SH_OFFSET_CURRENT: {

		} break;

		case SH_OFFSET_START: {
			i32 parent_count = offset_tag.parent_count;
			sh_decl_val *p = v->parent;
			sh_pos parent_start_pos = p->pos;

			while(parent_count > 0 && p != NULL) {
				p = p->parent;
				parent_start_pos = p->pos;
				parent_count--;
			}



			i64 to_parent = parent_start_pos.bit - reader_head.current_bit;
			i64 seek_amount = to_parent + offset_amount_bit;
			// printf("to_p: %lld seek %lld cur: %lld am %lld\n",
			// 		to_parent, seek_amount, reader_head.current_bit, offset_amount_bit);

			read_bits_from_reader(seek_amount);
		} break;
		default: { assert_exit(false, "Offset type not valid"); }
	}
}

void sh_seek_reader_pos(sh_pos pos) {

	i64 remaining_bits = reader_head.current_bit - pos.bit;
	if(remaining_bits < 0) {
		read_bits_from_reader(-1*remaining_bits);
	} else {
		rewind_reader_head_by_bits(remaining_bits);
	}
}




#if 1
u8 read_byte_from_stream(void) {

	u8 byte = 0;

	make_reader_fit_bits(8);

	byte = read_bits_from_reader(8) & 0xFF;

	return byte;
}


i64 read_bit(sh_typespec *int_type) {
	i64 temp_val = read_bits_from_reader(1);
	return temp_val;
}


char* read_str_null_term(i32 *size) {
	char* str = NULL;
	// i32 str_len = (i32)strlen((char*)reader_head.stream); // checks until null term
	// char *str = calloc(str_len + 1, sizeof(char));
	// memcpy(str, reader_head, str_len);

	// *size = str_len;

	// reader_head += str_len + 1; // we actually need to read the 0 byte

	return str;
}

i64 read_int_val(sh_typespec *int_type) {
	// should also read be? 
	i64 temp_val = 0;
	i32 size = int_type->size_byte;
	char *dst = (char*)&temp_val;

	if(is_int_be(int_type->base_type)) {
		for(i32 i = size - 1; i >= 0; i--) {
			u8 byte = read_byte_from_stream();
			dst[i] = byte;
		}
	} else {
		for(i32 i = 0; i < size; i++) {
			u8 byte = read_byte_from_stream();
			dst[i] = byte;
		}
	}

	
	return temp_val;
}



f64 read_float_val(sh_typespec *f_type) {
	// should also read be? 
	i32 size = f_type->size_byte;


	if(size == 4) {
		f32 temp_val = 0;
		char *dst = (char*)&temp_val;
		for(i32 i = 0; i < size; i++) {
			u8 byte = read_byte_from_stream();
			dst[i] = byte;
		}

		return temp_val;

	} else {
		f64 temp_val = 0;
		char *dst = (char*)&temp_val;
		for(i32 i = 0; i < size; i++) {
			u8 byte = read_byte_from_stream();
			dst[i] = byte;
		}

		return temp_val;
	}
	
}


void setup_array(sh_decl_val *v) {
	if(v->is_setup) return;

	if(v->type->array_size_expr->type == SH_QUESTION_OPERATOR_EXPR) {
		v->is_setup = 1;

		sh_typespec *t = v->type->base;
		sh_decl_val *vals = (sh_decl_val*)calloc(1, sizeof(sh_decl_val));
		vals->type = t;
		vals->parent = v;
		buf_push(v->array, vals);
		buf_clear(v->array);

	} else {
		sh_decl_val val = eval_expr_parent(v->type->array_size_expr, 0, v);
		if(val.read_at_least_once) {
			v->array_size = (i32)val.ival;
			sh_typespec *t = v->type->base;
			sh_decl_val *vals = (sh_decl_val*)calloc(val.ival, sizeof(sh_decl_val));
			for(i32 i = 0; i < val.ival; i++) {
				vals->type = t;
				vals->parent = v;
				buf_push(v->array, vals);
				vals++;
			}
			v->is_setup = 1;
		}

	}

}


void read_array(sh_decl_val *v) {

	setup_array(v);

	if(v->type->array_size_expr->type == SH_QUESTION_OPERATOR_EXPR) {
		if(v->array == NULL) {
			sh_decl_val *vnew = (sh_decl_val *)calloc(1, sizeof(sh_decl_val));
			vnew->type = v->type->base;
			read_data(vnew);
			v->checked_child_add_on = 1;
			v->child_has_add_on = vnew->child_has_add_on;
			buf_push(v->array, vnew);
		}
	} else {

		i64 val = v->array_size;
		for(i32 i = 0; i < val; i++) {
			read_data(v->array[i]);

			v->checked_child_add_on = 1;
			v->child_has_add_on = v->array[i]->child_has_add_on;

			v->read_size_byte += v->array[i]->read_size_byte;
			v->read_size_bit += v->array[i]->read_size_bit;
		}
	}
}


void setup_when_type(sh_decl_val *v) {
	if(v->type->type == SH_TYPE_WHEN) {
		sh_decl_val when_expr = eval_expr_parent(v->type->when->when_expr, 0, v);
		if(when_expr.read_at_least_once) {
			// i32 value = (i32)when_expr.ival;
			sh_typespec *t = sh_get_when_typespec(v->type->when, when_expr);
			if(t) {
				v->type = t;
				setup_data(v);
			} else {
				v->type = v->type->when->else_type;
				if(v->type != NULL) {
					setup_data(v);
				} else {
					v->is_setup = 1;
				}
			}
		}
	}
}

void setup_struct(sh_decl_val *v) {

	if(v->is_setup) return;

	i32 full_construct = 1;

	i8 child_has_add_on = v->child_has_add_on;
	if(v->is_partial_setup) {

		sh_type *type = v->type->base_type;
		sh_decl_val **struct_fields = v->fields;

		for(sh_decl_val **fields = struct_fields; fields != buf_end(struct_fields); fields++) {
			if(fields[0]->is_setup) continue;
			setup_data(fields[0]);
			full_construct = full_construct && fields[0]->is_setup;
		}

		v->data = struct_fields;


	} else {

		sh_type *type = v->type->base_type;

		for(sh_decl **fields = type->struct_type.fields; fields != buf_end( type->struct_type.fields ); fields++) {
			sh_decl_val *struct_field_val = (sh_decl_val*) calloc(1, sizeof(sh_decl_val));
			struct_field_val->decl = fields[0];

			struct_field_val->parent = v;

			
			struct_field_val->type = fields[0]->struct_field.type;
			setup_data(struct_field_val);
			if(!child_has_add_on && sh_has_decl_tag(struct_field_val, SH_ADD_ON_TAG)) {
				child_has_add_on = 1;
			}

			full_construct = full_construct && struct_field_val->is_setup;
			buf_push(v->fields, struct_field_val);
		}
	}

	v->is_setup = full_construct ? 1 : 0;
	v->is_partial_setup = !v->is_setup;
	v->checked_child_add_on = 1;
	v->child_has_add_on = child_has_add_on;
}

void read_struct(sh_decl_val *v) {

	sh_decl_val **sf = v->data;

	for(sh_decl_val **fields = sf; fields != buf_end(sf); fields++) {

		if(fields[0]->is_setup == 0)
			setup_data(fields[0]);


		read_data(fields[0]);
		v->read_size_byte += fields[0]->read_size_byte;
		v->read_size_bit += fields[0]->read_size_bit;
	}

}


sh_decl_val sh_copy_decl_type(sh_decl_val *v) {
	sh_decl_val x = {0};

	x.type = v->type;
	x.parent = NULL;
	x.type->array_count = 0;


	return x;
}


i8 do_struct_add_on(sh_decl_val *v, sh_decl_val *x) {

	i8 add_on_passed = 0;
	sh_decl_val **f = v->fields;
	//@TODO: can better
	for(i32 i = 0; i < buf_len(f); i++) {
		sh_decl_val *field1 = f[i];
		sh_decl_val *field2 = x->fields[i];
		if(!sh_has_decl_tag(field1, SH_ADD_ON_TAG)) continue;

		sh_decl_tag *add_on_tag = sh_get_decl_tag(field1, SH_ADD_ON_TAG);

		sh_decl_val f1 = eval_expr_parent(add_on_tag->add_on.add_expr, 0, v);
		sh_decl_val f2 = eval_expr_parent(add_on_tag->add_on.add_expr, 0, x);

		if(do_op_decl_val(SH_EQ, f1, f2).ival) {
			add_on_passed = 1;
			if(is_typespec_array(field1->type)) {
				i32 start_index = buf_len(field1->array);
				for(i32 i = 0; i < buf_len(field2->array); i++) {
					buf_push(field1->array, field2->array[i]);
					field1->array[start_index + i]->parent = field1;
				}

				field1->array_size += field2->array_size;

			} else if(is_typespec_int_type(field1->type)) {
				field1->ival += field2->ival;
			}
		}

	}
	// sh_print_decl_val(v);
	// sh_print_decl_val(x);
	return add_on_passed;
}

i8 do_add_on(sh_decl_val *v, sh_decl_val *w) {

	assert_exit(v->type == w->type, "cannot merge two different values");

	if(is_typespec_struct(v->type)) {
		return do_struct_add_on(v, w);
	}

	return 0;
}


void read_and_setup_struct(sh_decl_val *v) {
	setup_struct(v);

	if(v->read_at_least_once && ( !v->checked_child_add_on && sh_has_decl_tag(v, SH_ADD_ON_TAG) || v->child_has_add_on )) {
		sh_decl_val x = sh_copy_decl_type(v);
		setup_struct(&x);
		read_struct(&x);

		// add from x to v, if field has add_on_tag => check the expr
		do_struct_add_on(v, &x);

	} else {
		read_struct(v);
	}
}

sh_decl_val* sh_get_decl_val(sh_decl *d) {
	for(i32 i = 0; i < buf_len(val_decls); i++) {
		if(val_decls[i]->decl == d) return val_decls[i];
	}

	return NULL;
}


void setup_data(sh_decl_val *v) {
	if(!v->is_setup) {
		read_or_setup_data(v, 0);
		// v->is_setup = 1;
	}
}


sh_decl_val sh_decl_val_equal(sh_decl_val l, sh_decl_val r) {
	sh_decl_val res = {.type = sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_i64)};
	res.ival = l.ival == r.ival;
	return res;
}

sh_decl_val sh_eval_assert_tag(sh_expression *eval_func, sh_decl_val *value, i8 get_val) {

	switch(eval_func->type) {
		case SH_INT_LITERAL: {
			sh_decl_val val = {0};
			val.type = sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_i64);
			val.ival = get_val ? eval_func->vi64 : value->ival == eval_func->vi64;
			return val;
		} break;

		case SH_FLOAT_LITERAL: {
			sh_decl_val val = {0};
			if(get_val) {
				val.type = sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_f64);
				val.fval = eval_func->vf64;
			} else {
				val.type = sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_i64);
				val.ival = value->fval == eval_func->vf64;
			}

			return val;
		} break;


		case SH_STRING_LITERAL: {
			sh_decl_val val = {0};

			if(get_val) {
				val.type = sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_string);
				val.str = eval_func->str_val;
			} else {
				val.type = sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_i64);
				val.ival = eval_func->str_size == value->str_len && strncmp(eval_func->str_val, value->str, value->str_len) == 0;
			}

			return val;
		} break;

		case SH_OPERATOR_EXPR: {

			sh_decl_val left_eval = {0};
			sh_decl_val right_eval = {0};


			if(is_expr_arithmatic(eval_func) || is_expr_and_or(eval_func)) {
				get_val = 1;
				left_eval = sh_eval_assert_tag(eval_func->left_op, value, get_val);
				right_eval = sh_eval_assert_tag(eval_func->right_op, value, get_val);
				sh_decl_val val = do_op_decl_val(eval_func->op, left_eval, right_eval);

				if(is_expr_arithmatic(eval_func))
					return do_op_decl_val(SH_EQ, val, *value);
				return val;

			} else  {
				get_val = 1;
				right_eval = sh_eval_assert_tag(eval_func->right_op, value, get_val);
				return do_op_decl_val(eval_func->op, *value, right_eval);
			}

		} break;

		case SH_FIELD_ACCESS_EXPR: {

			sh_decl_val *d = value;
			if(eval_func->pointer_expr != NULL) {
				d = sh_get_expr_decl_val(eval_func->pointer_expr);
			}

			sh_expression *sf = eval_func->field_access;

			sh_decl_val **fields = NULL;
			sh_decl_val *nearest_parent_struct = d;

			while(nearest_parent_struct->parent != NULL && !is_typespec_struct(d->type)) {
				nearest_parent_struct = nearest_parent_struct->parent;
			}

			assert_exit(nearest_parent_struct != NULL, "couldn't find a parent that has a struct type");

			fields = nearest_parent_struct->fields;

			i32 index = sh_find_struct_field_index(nearest_parent_struct->type->base_type, sf->name, sf->name_len);

			assert_exit(index != -1, "field name %s not found on type %s", sf->name, nearest_parent_struct->type->base_type->name);

			sh_decl_val *field = fields[index];

			if(get_val) {
				return *field;
			} else {
				return sh_decl_val_equal(*field, *value);
			}

		} break;

		case SH_ARRAY_LITERAL: {

			sh_decl_val v = {0};
			v.type = sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_i64);
			v.ival = 1;

			for(i16 i = 0; i < buf_len(eval_func->values); i++) {
				v.ival = v.ival && sh_eval_assert_tag(eval_func->values[i], value->array[i], 0).ival;
				if(v.ival == 0) break;
			}

			return v;

		} break;

		case SH_STRUCT_LITERAL: {

			i32 field_index = 0;

			sh_type *t = value->type->base_type;

			sh_decl_val result = {0};
			result.ival = 1;
			result.type = sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_i64);
			for(i32 i = 0; i < buf_len(eval_func->fields); i++) {

				sh_expression *e = eval_func->fields[i];
				sh_expression *value_expr = e;

				if(e->type == SH_FIELD_ASSIGNMENT_EXPR) {
					field_index = sh_find_struct_field_index(t, e->left->name, e->left->name_len);
					value_expr = e->right;
				} 

				sh_decl_val *field_value = value->fields[field_index];
				sh_decl_val v = sh_eval_assert_tag(value_expr, field_value, 0);
				result.ival = result.ival && v.ival;

				field_index++;
			}

			return result;
		} break;

		default:
			assert_exit(false, "unhandled eval expr for assert ");
	}

	return (sh_decl_val){0};
}

void sh_check_assert_tag(sh_decl_val *v) {
	sh_decl *d = v->decl;

	if(d->tags != NULL) {
		for(i8 i = 0; i < buf_len(d->tags); i++) {
			if(is_tag_assert(d->tags[i])) {
				d->tags[i]->asrt.eval = (i8)sh_eval_assert_tag(d->tags[i]->asrt.expr, v, 0).ival;
				buf_push(v->tag_evals, (sh_tag_eval) { SH_ASSERT_TAG, d->tags[i]->asrt.eval });
			}
		}
	}
}

i8 sh_check_cond_tag(sh_decl_val *v) {
	sh_decl *d = v->decl;

	if(d->tags != NULL) {
		for(i8 i = 0; i < buf_len(d->tags); i++) {
			if(is_tag_cond(d->tags[i])) {
				d->tags[i]->asrt.eval = (i8)eval_expr_parent(d->tags[i]->asrt.expr, 0, v).ival;
				buf_push(v->tag_evals, (sh_tag_eval) { SH_COND_TAG, d->tags[i]->asrt.eval });
				return d->tags[i]->asrt.eval;
			}
		}
	}

	return 0;
}

void read_data(sh_decl_val *v) {

	if(has_cond_tag(v) && !sh_check_cond_tag(v)) {
		return;
	}

	sh_pos current_pos = {reader_head.current_byte_stream, reader_head.current_bit};

	if(sh_has_decl_tag(v, SH_OFFSET_TAG)) {
		sh_decl_tag *d = sh_get_decl_tag(v, SH_OFFSET_TAG);
		sh_decl_val x = eval_expr_parent(d->offset.expr, 0, v->parent);

		sh_seek_reader_head(d->offset, x.ival*8, v);
	}

	v->pos.byte = reader_head.current_byte_stream;
	v->pos.bit = reader_head.current_bit;

	read_or_setup_data(v, 1);
	v->read_at_least_once = 1;

	if(v->decl)
		sh_check_assert_tag(v);

	if(sh_has_decl_tag(v, SH_OFFSET_TAG)) {
		sh_seek_reader_pos(current_pos);
	}
}


void read_or_setup_data(sh_decl_val *v, i8 read_or_setup) {

	i32 size_read = 0; 

	if(v->type) size_read = v->type->size_byte;

	if(is_typespec_str(v->type)) {
		if(read_or_setup) {
			v->data = read_str_null_term(&v->str_len);
		} else {
			v->is_setup = 1;
		}
	} else if(is_typespec_int_type(v->type) || is_typespec_char(v->type)) {
		if(read_or_setup) {
			i64 val = read_int_val(v->type);
			v->ival = val;
			v->read_size_byte += v->type->size_byte;
			v->read_size_bit += v->type->size_bit;
		} else {
			v->is_setup = 1;
		}
	} else if(is_typespec_struct(v->type)) {
		if(read_or_setup) {
			read_and_setup_struct(v);
		} else {
			setup_struct(v);
		}
	} else if(is_typespec_array(v->type)) {


		if(read_or_setup) {
			if(v->is_setup == 0) setup_array(v);

			// handling 64+ bits is gonna need splitting the bits into more than
			if(is_typespec_bit_or_tib(v->type->base)) { 
				make_reader_fit_bits(v->array_size);
				i64 val = read_bits_from_reader(v->array_size);

				if(is_typespec_bit(v->type->base)) {
					val = reverse_bits(val, v->array_size);
				}

				v->read_size_byte += v->array_size/8;
				v->read_size_bit += v->array_size;
				v->ival = val;
			} else {

				read_array(v);
			}

		} else {
			setup_array(v);
		}
	} else if(is_typespec_float_type(v->type)) {
		if(read_or_setup) {
			f64 val = read_float_val(v->type);
			v->fval = val;
		} else {
			v->is_setup = 1;
		}
	} else if(is_typespec_bit_or_tib(v->type)) {
		if(read_or_setup) {
			make_reader_fit_bits(1);
			i64 val = read_bit(v->type);
			v->ival = val;
			v->read_size_bit += 1;
		} else {
			v->is_setup = 1;
		}

	} else if(is_typespec_enum(v->type)) {

		//enums are numbers -> read base_type
		if(read_or_setup) {
			 // this is shit
			sh_decl_val x = {0};
			x.type = v->type->base_type->enum_type.base_type;
			setup_data(&x);
			read_data(&x);
			v->ival = x.ival;
			v->read_size_byte += x.read_size_byte;
			v->read_size_bit += x.read_size_bit;
		} else {

			v->is_setup = 1;

		}

	} else if(is_typespec_when(v->type)) {
		setup_when_type(v);
    } else if(is_typespec_bit_field(v->type)) {

        if(read_or_setup) {
            i64 val = read_int_val(v->type->base->base_type);

            // u64 mask = v->0;
            // v->ival = val;
            // v->read_size_byte += v->type->size_byte;
            // v->read_size_bit += v->type->size_bit;
        } else {
            v->is_setup = 1;
        }

	} else {
		assert_exit(false, "typespec not handled for reading");
	}

}

i32 read_decl_binary(sh_decl_val *val_decl, i8 read_or_setup) {
	i32 size_read = 0;

	switch(val_decl->decl->type) {

		case SH_VAR_DECL: {
			if(read_or_setup) {
				read_data(val_decl);
			} else {
				setup_data(val_decl);
			}
		} break;

		default: {
			assert_exit(false, "we don't handle this decl %s", sh_print_decl(val_decl->decl));
		}
	}


	return size_read;
}


i8 indent = 0;

void print_indent() {
	for(i8 i = 0; i < indent*2; i++ ) {
		putchar(' ');
	}
}


sh_print_tag sh_get_print_tag(sh_decl_val *d) {

	sh_print_tag p = {0};

	i8 found = 0;
	if(d != NULL) {
		if(d->decl != NULL  && d->decl->tags != NULL) {
			for(i32 i = 0; i < buf_len(d->decl->tags); i++) {
				if(d->decl->tags[i]->type == SH_PRINT_TAG) {
					p = d->decl->tags[i]->print;
					found = 1;
					break;
				}
			}
		}
	} else {
		found = 1;
	}

	if(!found) {
		p = sh_get_print_tag(d->parent);
	}



	return p;
}

sh_array_print_tag sh_get_array_print_tag(sh_decl_val *d) {


	sh_array_print_tag p = {
		.count = -1,
		.start = -1,
		.row = -1,
		.print_index = 1,
		.newline = 1,
		.seperator = ','
	};

	if(d != NULL && d->decl != NULL  && d->decl->tags != NULL) {
		for(i32 i = 0; i < buf_len(d->decl->tags); i++) {
			if(d->decl->tags[i]->type == SH_ARRAY_PRINT_TAG) {
				p = d->decl->tags[i]->array_print;
				break;
			}
		}
	}

	return p;
}

sh_struct_print_tag sh_get_struct_print_tag(sh_decl_val *d) {


	sh_struct_print_tag p = { .field_name = -1, .newline = -1, .seperator = ',' };

	i8 found = 0;
	if(d != NULL) {
		if(d->decl != NULL  && d->decl->tags != NULL) {
			for(i32 i = 0; i < buf_len(d->decl->tags); i++) {
				if(d->decl->tags[i]->type == SH_STRUCT_PRINT_TAG) {
					p = d->decl->tags[i]->struct_print;
					found = 1;
					break;
				}
			}
		}
	} else {
		found = 1;
	}

	if(!found) {
		p = sh_get_struct_print_tag(d->parent);
	}



	return p;
}

sh_decl_tag* sh_get_decl_tag(sh_decl_val *d, sh_tag_type type) {

	if(d->decl != NULL  && d->decl->tags != NULL) {
		for(i32 i = 0; i < buf_len(d->decl->tags); i++) {
			if(d->decl->tags[i]->type == type) {
				return d->decl->tags[i];
			}
		}
	}

	return NULL;
}

void sh_print_based_on_format(sh_print_type t, sh_decl_val *v) {
	char *format = "%lld";
	switch(t) {
		case SH_HEX_PRINT:  format = "h %llx"; break;
		case SH_DEC_PRINT:  format = "%lld"; break;
		case SH_BIN_PRINT:  format = ""; break;
		case SH_OCT_PRINT:  format = "o %llo"; break;
		default: format = "%lld";
	}

	if(t == SH_BIN_PRINT) {
		i32 bit_size = (i32)log2((double)v->ival) + 1;

		if(is_typespec_bit_or_tib(v->type->base)) {
			bit_size = v->array_size;
		}

		putchar('b');
		putchar(' ');
		for(i32 i = (i32)(bit_size-1); i >= 0; i--) {
			if((v->ival & (1LLU << i)) > 0)
				putchar('1');
			else putchar('0');
		}

	} else {
		printf(format, v->ival);
	}


}


void sh_print_struct_decl_val(sh_decl_val *t) {

	sh_struct_print_tag p_tag = sh_get_struct_print_tag(t);

	sh_decl_val **v = t->fields;

	putchar('{');

	if(p_tag.newline) { putchar('\n'); }

	indent++;

	i32 len = buf_len(v);
	for(i32 i = 0; i < len; i++) {
		if(has_cond_tag(v[i]) && cond_tag_failed(v[i])) continue;

		if(p_tag.newline) {
			print_indent();
		} else {
			putchar(' ');
		}

		// if(is_typespec_array(v[i]->type) && !p_tag.newline) {
		// 	putchar('\n');
		// 	print_indent();
		// }


		if(v[i]->decl && p_tag.field_name) {
			printf("%s = ", v[i]->decl->name);
		}

		sh_print_decl_val(v[i]);

		// if(!p_tag.newline && i != (len - 1))
		// 	putchar(' ');

		if(i < len - 1 ) {
			putchar(' ');
			putchar(p_tag.seperator);
		}

		if(p_tag.newline)
			putchar('\n');
	}


	indent--;
	if(p_tag.newline) {
		print_indent();
	} else {
		putchar(' ');
	}

	putchar('}');

}

void sh_print_array_decl_val(sh_decl_val *t) {
	i32 v = t->array_size;

	if(is_typespec_char(t->type->base)) {

		for(i32 i = 0; i < v; i++) { putchar(t->array[i]->ch); }

	} else if(is_typespec_bit_or_tib(t->type->base)) {

		sh_print_tag print = sh_get_print_tag(t);
		sh_print_based_on_format(print.type, t);

	} else {

		sh_array_print_tag arr_p = sh_get_array_print_tag(t);


		i32 start = arr_p.start != -1 ? arr_p.start : 0;
		i32 count = arr_p.count != -1 ? arr_p.count + start : v;
		i32 row = arr_p.row != -1 ? arr_p.row : 1;

		i8 pr_index = arr_p.print_index;
		i8 override_newline = arr_p.row != -1;
		i8 newline = arr_p.newline || override_newline;
		char seperator = arr_p.seperator;

		count = MIN(count, v);

		putchar('[');
		
		indent++;
		if(newline) {
			putchar('\n');
		} 

		i32 row_count = 1;

		for(i32 i = start; i < count; i++) {

			if(newline) {
				if(( row_count-1)%row == 0)
					print_indent();
			} else putchar(' ');

			if(pr_index) {
				printf("%d = ", i);
			} 

			sh_print_decl_val(t->array[i]);

			if(i != (count-1) ) {
				putchar(' ');
				putchar(seperator);
			}

			if(newline && row_count%row == 0 ) {
				putchar('\n');
				// print_indent();
			} 

			row_count++;
		}

		if(newline && (row_count-1)%row != 0 ) putchar('\n');

		indent--;

		if(newline) {
			print_indent();
		} else {
			putchar(' ');
		}

		putchar(']');
	}

}

void sh_print_decl_val(sh_decl_val *t) {
	if(has_cond_tag(t) && cond_tag_failed(t)) { return; }

	if(t->read_at_least_once == 0 && !is_typespec_struct(t->type)) {
		printf("not read");
		return;
	}

	sh_print_tag print = sh_get_print_tag(t);


	if(is_typespec_str(t->type)) {
		printf("%s", t->str);
	} else if(is_typespec_int_type(t->type)) {
		
		sh_print_based_on_format(print.type, t);

	} else if(is_typespec_struct(t->type)) {

		sh_print_struct_decl_val(t);
	} else if(is_typespec_array(t->type)) {

		sh_print_array_decl_val(t);

	} else if(is_typespec_float_type(t->type)) {
		printf("%f", t->fval);
	} else if(is_typespec_char(t->type)) {
		putchar(t->ch);
	} else if(is_typespec_bit_or_tib(t->type)) {
		putchar( t->ival & 1 ? '1' : '0' );
	} else if(is_typespec_enum(t->type)) {

		sh_decl *enum_field_decl = sh_get_enum_field(t->ival, t->type->base_type);
		if(enum_field_decl != NULL) {
			printf("%s %lld", enum_field_decl->name, enum_field_decl->vu64);
		} else {
			printf("value (%lld) is outside enum range", t->ival);
		}

	} else if(is_typespec_when(t->type)) {
		printf("value %lld doesn't map to any of the provided types", t->ival);
	} else {
		assert_exit(false, "unhandled decl_val");
	}

	if(has_assert_tag(t) && assert_tag_failed(t) && t->read_at_least_once) {

		sh_expression *expr = NULL;
		for(i32 i = 0; i < buf_len(t->decl->tags); i++ ) {
			if(t->decl->tags[i]->type == SH_ASSERT_TAG) {
				expr = t->decl->tags[i]->asrt.expr;
			}

		}

		if(expr) {
			printf(" (assert %s failed)", sh_print_expr(expr));
		}
	}

}


i64 get_id_val(sh_decl *d) {
	sh_decl_val *v = sh_get_decl_val(d);

	if(is_typespec_int_type(v->type)) {
		return v->ival;
	}

	return 0;
}


sh_decl_val do_op_decl_val(sh_expr_operator op, sh_decl_val left_op, sh_decl_val right_op) {
	i64 result = 0;

	sh_decl_val val = {0};

	switch(op) {
		case SH_PLUS: {

			if(is_typespec_float_type(left_op.type) || is_typespec_float_type(right_op.type)) {
				f64 left = is_typespec_float_type(left_op.type) ? left_op.fval : (f64) left_op.ival;
				f64 right = is_typespec_float_type(right_op.type) ? right_op.fval : (f64) right_op.ival;
				val.type = sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_f64);
				val.fval = left + right;
			} else {
				val.type = sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_i64);
				val.ival = left_op.ival + right_op.ival;
			}

		} break;

		case SH_MINUS: {
			if(is_typespec_float_type(left_op.type) || is_typespec_float_type(right_op.type)) {
				f64 left = is_typespec_float_type(left_op.type) ? left_op.fval : (f64) left_op.ival;
				f64 right = is_typespec_float_type(right_op.type) ? right_op.fval : (f64) right_op.ival;
				val.type = sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_f64);
				val.fval = left - right;
			} else {
				val.type = sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_i64);
				val.ival = left_op.ival - right_op.ival;
			}


		} break;

		case SH_DIV: {
			if(is_typespec_float_type(left_op.type) || is_typespec_float_type(right_op.type)) {
				f64 left = is_typespec_float_type(left_op.type) ? left_op.fval : (f64) left_op.ival;
				f64 right = is_typespec_float_type(right_op.type) ? right_op.fval : (f64) right_op.ival;
				val.type = sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_f64);
				val.fval = left / right;
			} else {
				val.type = sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_i64);
				val.ival = left_op.ival / right_op.ival;
			}


		} break;
		case SH_ASTERISK: {
			if(is_typespec_float_type(left_op.type) || is_typespec_float_type(right_op.type)) {
				f64 left = is_typespec_float_type(left_op.type) ? left_op.fval : (f64) left_op.ival;
				f64 right = is_typespec_float_type(right_op.type) ? right_op.fval : (f64) right_op.ival;
				val.type = sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_f64);
				val.fval = left * right;
			} else {
				val.type = sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_i64);
				val.ival = left_op.ival * right_op.ival;
			}


		} break;

		case SH_BOR: {

			assert_exit(!is_typespec_float_type(left_op.type) && !is_typespec_float_type(right_op.type), "can't do binary OR between types");
			val.type = sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_i64);
			val.ival = left_op.ival + right_op.ival;

		} break;
		case SH_BXOR: {

			assert_exit(!is_typespec_float_type(left_op.type) && !is_typespec_float_type(right_op.type), "can't do binary OR between types");
			val.type = sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_i64);
			val.ival = left_op.ival ^ right_op.ival;

		} break;
		case SH_AND_OPERATOR: {
			assert_exit(!is_typespec_float_type(left_op.type) && !is_typespec_float_type(right_op.type), "can't do binary OR between types");
			val.type = sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_i64);
			val.ival = left_op.ival & right_op.ival;
		} break;

		case SH_GR: {
			val.type = sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_i64);
			if(is_typespec_float_type(left_op.type) || is_typespec_float_type(right_op.type)) {
				f64 left = is_typespec_float_type(left_op.type) ? left_op.fval : (f64) left_op.ival;
				f64 right = is_typespec_float_type(right_op.type) ? right_op.fval : (f64) right_op.ival;
				val.ival = left > right;
			} else {
				val.ival = left_op.ival > right_op.ival;
			}
		} break;
		case SH_LT: {
			val.type = sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_i64);
			if(is_typespec_float_type(left_op.type) || is_typespec_float_type(right_op.type)) {
				f64 left = is_typespec_float_type(left_op.type) ? left_op.fval : (f64) left_op.ival;
				f64 right = is_typespec_float_type(right_op.type) ? right_op.fval : (f64) right_op.ival;
				val.ival = left < right;
			} else {
				val.ival = left_op.ival < right_op.ival;
			}
		} break;
		case SH_GREQ: {
			val.type = sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_i64);
			if(is_typespec_float_type(left_op.type) || is_typespec_float_type(right_op.type)) {
				f64 left = is_typespec_float_type(left_op.type) ? left_op.fval : (f64) left_op.ival;
				f64 right = is_typespec_float_type(right_op.type) ? right_op.fval : (f64) right_op.ival;
				val.ival = left >= right;
			} else {
				val.ival = left_op.ival >= right_op.ival;
			}
	
		} break;
		case SH_LTEQ: {
			val.type = sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_i64);
			if(is_typespec_float_type(left_op.type) || is_typespec_float_type(right_op.type)) {
				f64 left = is_typespec_float_type(left_op.type) ? left_op.fval : (f64) left_op.ival;
				f64 right = is_typespec_float_type(right_op.type) ? right_op.fval : (f64) right_op.ival;
				val.ival = left <= right;
			} else {
				val.ival = left_op.ival <= right_op.ival;
			}
		} break;
		case SH_EQ: {
			val.type = sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_i64);

			if(is_typespec_float_type(left_op.type) || is_typespec_float_type(right_op.type)) {
				f64 left = is_typespec_float_type(left_op.type) ? left_op.fval : (f64) left_op.ival;
				f64 right = is_typespec_float_type(right_op.type) ? right_op.fval : (f64) right_op.ival;
				val.ival = left == right;
			} else if(is_typespec_str_or_char_arr(left_op.type) && is_typespec_str_or_char_arr(right_op.type)) {
				char *l = NULL;
				char *r = NULL;

				i32 bigger_size = 0;

				if(is_typespec_array(left_op.type)) {
					l = calloc(left_op.array_size + 1, sizeof(char));
					for(i32 i = 0; i < left_op.array_size; i++) {
						l[i] = left_op.array[i]->ch;
					}
					bigger_size = left_op.array_size;
				} else {
					l = left_op.str;
					bigger_size = left_op.str_len;
				}

				if(is_typespec_array(right_op.type)) {
					r = calloc(right_op.array_size + 1, sizeof(char));
					for(i32 i = 0; i < right_op.array_size; i++) {
						r[i] = right_op.array[i]->ch;
					}
					if(right_op.array_size > bigger_size) bigger_size = right_op.array_size;
				} else {
					r = right_op.str;
					if(right_op.str_len > bigger_size) bigger_size = right_op.str_len;
				}

				val.ival = strncmp(l, r, bigger_size) == 0;

			} else {
				val.ival = left_op.ival == right_op.ival;
			}
	
		} break;
		case SH_NEQ: {
			val.type = sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_i64);
			if(is_typespec_float_type(left_op.type) || is_typespec_float_type(right_op.type)) {
				f64 left = is_typespec_float_type(left_op.type) ? left_op.fval : (f64) left_op.ival;
				f64 right = is_typespec_float_type(right_op.type) ? right_op.fval : (f64) right_op.ival;
				val.ival = left != right;
			} else if(is_typespec_str_or_char_arr(left_op.type) && is_typespec_str_or_char_arr(right_op.type)) {
				char *l = NULL;
				char *r = NULL;

				i32 bigger_size = 0;

				if(is_typespec_array(left_op.type)) {
					l = calloc(left_op.array_size + 1, sizeof(char));
					for(i32 i = 0; i < left_op.array_size; i++) {
						l[i] = left_op.array[i]->ch;
					}
					bigger_size = left_op.array_size;
				} else {
					l = left_op.str;
					bigger_size = left_op.str_len;
				}

				if(is_typespec_array(right_op.type)) {
					l = calloc(right_op.array_size + 1, sizeof(char));
					for(i32 i = 0; i < right_op.array_size; i++) {
						l[i] = right_op.array[i]->ch;
					}
					if(right_op.array_size > bigger_size) bigger_size = right_op.array_size;
				} else {
					r = right_op.str;
					if(right_op.str_len > bigger_size) bigger_size = right_op.str_len;
				}

				val.ival = strncmp(l, r, bigger_size) != 0;

			} else {
				val.ival = left_op.ival != right_op.ival;
			}
		} break;

		case SH_AND: {
			val.type = sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_i64);

			if(is_typespec_float_type(left_op.type) || is_typespec_float_type(right_op.type)) {
				f64 left = is_typespec_float_type(left_op.type) ? left_op.fval : (f64) left_op.ival;
				f64 right = is_typespec_float_type(right_op.type) ? right_op.fval : (f64) right_op.ival;
				val.ival = left && right;
			} else {
				val.ival = left_op.ival && right_op.ival;
			}

		} break;
		case SH_OR: {
			val.type = sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_i64);
			if(is_typespec_float_type(left_op.type) || is_typespec_float_type(right_op.type)) {
				f64 left = is_typespec_float_type(left_op.type) ? left_op.fval : (f64) left_op.ival;
				f64 right = is_typespec_float_type(right_op.type) ? right_op.fval : (f64) right_op.ival;
				val.ival = left || right;
			} else {
				val.type = sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_i64);
				val.ival = left_op.ival || right_op.ival;
			}
		} break;

		default: assert_exit(false, "bad op");
	}

	return val;
}

//Todo: floats? 
i64 do_op(sh_expr_operator op, i64 left_op, i64 right_op) {
	i64 result = 0;

	switch(op) {
		case SH_PLUS: return left_op + right_op;
		case SH_MINUS: return left_op - right_op;
		case SH_DIV: return left_op/right_op;
		case SH_ASTERISK: return left_op * right_op;

		case SH_BOR: return left_op | right_op;
		case SH_BXOR: return left_op ^ right_op;
		case SH_AND_OPERATOR: return left_op & right_op;

		case SH_GR: return left_op > right_op;
		case SH_LT: return left_op < right_op;
		case SH_GREQ: return left_op >= right_op;
		case SH_LTEQ: return left_op <= right_op;
		case SH_EQ: return left_op == right_op;
		case SH_NEQ: return left_op != right_op;

		case SH_AND: return left_op && right_op;
		case SH_OR: return left_op || right_op;

		default: assert_exit(false, "bad op");
	}


	return result;
}

sh_decl_val* sh_get_expr_decl_val(sh_expression *e) {

	switch(e->type) {

		case SH_ID_EXPR: {
			return sh_get_decl_val(e->var_decl);
		} break;

		case SH_FIELD_ACCESS_EXPR: {
			puts("we got here");
			sh_decl_val *d = sh_get_expr_decl_val(e->pointer_expr);
			sh_expression *sf = e->field_access;
			sh_decl_val **fields = d->data;
			i32 index = sh_find_struct_field_index(d->type->base_type, sf->name, sf->name_len);
			sh_decl_val *field = fields[index];
			return field;

		} break;

		case SH_POST_INC_EXPR: {
			return sh_get_expr_decl_val(e->operand);
		} break;

		case SH_PEEK_EXPR: {
			sh_decl_val *decl_val = (sh_decl_val*)calloc(1, sizeof(sh_decl_val));
			decl_val->type = sh_type_check_expr(e->peek_type)->base_type;

			read_data(decl_val);

			rewind_reader_head_by_bits(decl_val->read_size_bit);

			return decl_val;
		} break;

		default : { assert_exit(false, "unhandled expr for decl_val type %s", sh_print_expr(e)); return NULL; }
	} 

}


// wrong, not all vals are i64 ?  (maybe ptrs as i64 and then convert back?)
// this returns bits
i64 sizeof_expr_eval(sh_expression *e) {
	i64 val = 0;

	switch(e->type) {

		case SH_ID_EXPR: {
			sh_semantic_type *t = sh_type_check_expr(e);
			return t->base_type->size_bit;
		} break;

		case SH_INT_LITERAL: {
			// int literal should be sized properly
			return 8;
		} break;

		case SH_OPERATOR_EXPR: {
			puts("operator size isn't proprly implemented yet");
			return 8;
		} break;

		case SH_FIELD_ACCESS_EXPR: {
			sh_type *t = sh_type_check_expr(e->pointer_expr)->base_type->base_type;
			sh_expression *sf = e->field_access;
			i32 field_index = sh_find_struct_field_index(t, sf->name, sf->name_len);

			return t->struct_type.fields[field_index]->struct_field.type->size_bit;
			
		} break;

		case SH_ARRAY_EXPR: {
			i64 base_size = sizeof_expr_eval(e->array_expr);
			sh_decl_val v = eval_expr(e->array_index_expr, 0);
			base_size *= v.ival;
			return base_size;
		} break;

		case SH_SIZEOF_BIT_EXPR: {
			return sizeof_expr_eval(e->operand);
		}

		case SH_SIZEOF_BYTE_EXPR: {
			return sizeof_expr_eval(e->operand);
		}


		default : { assert_exit(false, "unhandled expr type %s", sh_print_expr(e)); }
	} 

	return val;
}




sh_decl_val* eval_stmt(sh_statement *stmt) {

	switch(stmt->type) {
		case SH_INC_DEC_STATEMENT: {
			sh_decl_val *v = sh_get_expr_decl_val(stmt->expr);
			if(is_typespec_int_type(v->type)) {
				v->ival++;
			}

			return v;
		} break;

		case SH_VAR_DECL_STATEMENT: {
			sh_decl *d = stmt->var_decl;
			sh_decl_val *left = sh_get_decl_val(d);
			if(d->var.init_expr) {
				setup_data(left);
				left->data = eval_expr(d->var.init_expr, 0).data;
			}
			return left;

		} break;

		case SH_ASSIGNMENT_STATEMENT: {
			sh_decl_val *left = sh_get_expr_decl_val(stmt->left_side_expr);
			left->data = eval_expr(stmt->right_side_expr, 0).data;

			return left;
		} break;

		default: {
			assert_exit(false, "unhandled stmt type eval %s", sh_print_statement(stmt));
			return NULL;
		}
	}


}


sh_decl_val eval_expr(sh_expression *e, i8 read_return_data) {
	return eval_expr_parent(e, read_return_data, NULL);
}
// wrong, not all vals are i64 ?  (maybe ptrs as i64 and then convert back?)
sh_decl_val eval_expr_parent(sh_expression *e, i8 read_return_data, sh_decl_val *parent) {

	switch(e->type) {

		case SH_STRING_LITERAL: {
			sh_decl_val v = {.type = sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_string)};
			v.str = e->str_val;
			v.read_at_least_once = 1;
			return v;
		} break;


		case SH_ID_EXPR: {
			sh_decl_val v = {0};
			sh_decl_val *d = sh_get_decl_val(e->var_decl);

			if(read_return_data) {
				read_data(d);
			} else {
				v = *d;
			}

			return v;
		} break;

		case SH_PARENT_EXPR: {
			sh_decl_val v = {0};

			if(!read_return_data) {
				if(parent) {
					v = *parent;
				}
			}

			return v;
		} break;


		case SH_INT_LITERAL: {

			sh_decl_val v = {.type = sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_i64)};
			v.ival = e->vi64;
			v.read_at_least_once = 1;

			return v;
		} break;

		case SH_OPERATOR_EXPR: {

			sh_decl_val left = eval_expr_parent(e->left_op, read_return_data, parent);
			sh_decl_val right = eval_expr_parent(e->right_op, read_return_data, parent);

			sh_decl_val result = do_op_decl_val(e->op, left, right);
			result.read_at_least_once = left.read_at_least_once && right.read_at_least_once;
			result.is_setup = 1;

			return result;

		} break;

		case SH_FIELD_ACCESS_EXPR: {

			sh_decl_val v = {0};

			if(read_return_data) {
				// assume its id expr for now

				sh_decl_val *d = sh_get_decl_val(e->pointer_expr->var_decl);
				sh_expression *sf = e->field_access;

				sh_decl_val **fields = d->data;
				sh_decl_val *field_val = NULL;

				i32 len = buf_len(fields);
				assert_exit(len != 0, "fields len must not be zero");

				for(i32 i = 0; i < len; i++) {
					sh_decl *vd = fields[i]->decl;
					if(sf->name_len == vd->name_len && strncmp(sf->name, vd->name, vd->name_len) == 0) {
						field_val = fields[i];
						break;
					}
				}

				assert_exit(field_val != NULL, "field_val cannot be null");
				read_data(field_val);
			} else {
				sh_decl_val *d = NULL;

				if(e->pointer_expr != NULL) {
					 v = eval_expr_parent(e->pointer_expr, read_return_data, parent);
					 d = &v;

				} else {
					// it has the format .value, thus the decl_val is this item

					d = parent;
					sh_decl_val *nearest_parent_struct = d;

					while(nearest_parent_struct->parent != NULL && !is_typespec_struct(nearest_parent_struct->type)) {
						nearest_parent_struct = nearest_parent_struct->parent;
					}

					d = nearest_parent_struct;
				}

				sh_expression *sf = e->field_access;

				if(sf->type == SH_ID_EXPR && sf->name[0] == '@') {
					v = *(d->parent);
				} else {
					i32 index = sh_find_struct_field_index(d->type->base_type, sf->name, sf->name_len);

					if(index < buf_len(d->fields) && index != -1) {
						sh_decl_val *field = d->fields[index];
						v = *field;
					}
				}
			}

			return v;
		} break;

		case SH_SIZEOF_BIT_EXPR: {
			sh_decl_val v = {.type = sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_i64)};
			sh_semantic_type *t = sh_type_check_expr(e->operand);
			v.ival = sizeof_expr_eval(e->operand);
			return v;
		}

		case SH_SIZEOF_BYTE_EXPR: {
			sh_decl_val v = {.type = sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_i64)};
			sh_semantic_type *t = sh_type_check_expr(e->operand);
			v.ival = sizeof_expr_eval(e->operand)/8;
			return v;
		}


		case SH_ARRAY_EXPR: {
			sh_decl_val v = {0};
			sh_decl_val val = eval_expr_parent(e->array_expr, 0, parent);
			sh_decl_val index = eval_expr_parent(e->array_index_expr, 0, parent);

			if(read_return_data) {

				if(val.type->array_size_expr->type == SH_QUESTION_OPERATOR_EXPR) {
					//@TODO: test this better

					sh_decl_val *act_val = &val;

					if(act_val->parent != NULL) {
						sh_typespec *parent_type = act_val->parent->type; 
						sh_decl_val *p = act_val->parent;

						if(is_typespec_struct(parent_type)) {
							for(sh_decl_val **fields = p->fields; fields != buf_end(act_val->parent->fields); fields++) {
								if(act_val->array == fields[0]->array) {
									act_val = fields[0];
									break;
								}
							}
						} else if(is_typespec_array(parent_type)) {
							for(i32 i = 0; i < p->array_size; i++) {
								if(act_val->array == p->array[0]->array) {
									act_val = p->array[0];
									break;
								}
							}
						} 
					} else {
						for(sh_decl_val **d_vals = val_decls; d_vals != buf_end(val_decls); d_vals++ ) {
							if(act_val->array == d_vals[0]->array ) {
								act_val = d_vals[0];
								break;
							}
						}
					}

					bit_stream store = reader_head;
					char *store_str = reader_head.stream;



					i8 passed_addon = 0;
					if(act_val->read_at_least_once && act_val->checked_child_add_on && act_val->child_has_add_on) {

						sh_decl_val value = sh_copy_decl_type(act_val->array[0]);
						setup_data(&value);
						read_data(&value);

						for(i32 i = 0; i < act_val->array_size; i++) {
							passed_addon = do_add_on(act_val->array[i], &value);
							if(passed_addon) break;
						}

					} 

					if(!passed_addon) {

						while(buf_len(act_val->array) <= index.ival) {
							sh_decl_val *vnew = (sh_decl_val*)calloc(1, sizeof(sh_decl_val));
							vnew->type = val.type->base;
							buf_push(act_val->array, vnew);
						}


						// rewind_reader_head_by_bits(v.read_size_bit);
						reader_head = store;
						reader_head.stream = store_str;

						read_data(act_val->array[act_val->array_size]);

						act_val->child_has_add_on = act_val->array[index.ival]->child_has_add_on;
						act_val->checked_child_add_on = act_val->array[index.ival]->checked_child_add_on;

						act_val->type->array_count++;
						act_val->array_size++;
						act_val->read_at_least_once = 1;

					}
					
				} else {
					read_data(val.array[index.ival]);
				}
			} else {
				if(val.read_at_least_once) {
					v = *val.array[index.ival];
				}
			}

			return v;
		} break;


		case SH_PEEK_EXPR: {
			// wrong for array
			sh_decl_val v = {0};
			v.type = sh_type_check_expr(e->peek_type)->base_type;
			// bit_stream store = reader_head;
			// char *store_str = reader_head.stream;
			read_data(&v);
			rewind_reader_head_by_bits(v.read_size_bit);
			// reader_head = store;
			// reader_head.stream = store_str;
			return v;
		} break;

		default : {
			assert_exit(false, "unhandled expr type %s for reading", sh_print_expr(e));
			sh_decl_val v = {0};
			return v;
		}
	} 

}


i32 read_for_stmt(sh_statement *st) {
	i32 size_read = 0;



	sh_decl_val *control_var = NULL;

	if(st->init_statement) {
		control_var = eval_stmt(st->init_statement);
	}

	sh_decl_val condition_value = eval_expr(st->condition_expr, 0);

	while(condition_value.ival) {
		read_stmt(st->comp_statement);
		eval_stmt(st->post_loop_expr);

		condition_value = eval_expr(st->condition_expr, 0);
	}


	return size_read;
}

i32 read_if_stmt(sh_statement *st) {
	i32 size_read = 0;

	sh_decl_val condition_value = eval_expr(st->condition_expr, 0);
	if(condition_value.ival) {
		return read_stmt(st->comp_statement);
	} else {
		i32 elif_passed = 0;
		if(st->elseif_stmts) {
			sh_statement **elifs = st->elseif_stmts;

			for(; elifs != buf_end(st->elseif_stmts); elifs++) {
				sh_decl_val cond_val = eval_expr(elifs[0]->condition_expr, 0);
				if(cond_val.ival) {
					read_stmt(elifs[0]->comp_statement);
					elif_passed = 1;
					break;
				}
			}

		} 

		if(!elif_passed && st->else_stmt != NULL) {
			read_stmt(st->else_stmt);
		}
	}


	return size_read;
}

i32 read_stmt(sh_statement *st) {
	i32 size_read = 0;

	switch(st->type) {

		case SH_COMPOUND_STATEMENT: {

			for(sh_statement **s = st->statements; s != buf_end(st->statements); s++) {
				size_read += read_stmt(s[0]);
			}

		} break;
		case SH_VAR_DECL_STATEMENT: {

			sh_decl_val *v = sh_get_decl_val(st->var_decl);
			size_read = read_decl_binary(v, !is_delayed_read_var(v->decl));

		} break;

		case SH_IF_STATEMENT: {

			size_read = read_if_stmt(st);

		} break;

		case SH_FOR_STATEMENT: {

			read_for_stmt(st);

		} break;

		case SH_EXPR_STATEMENT: {

			size_read = (i32)eval_expr(st->expr, 1).ival;

		} break;

		case SH_SKIP_BIT_STATEMENT: {
			i32 skip_bits = (i32)eval_expr(st->skip, 0).ival;// cannot have 

			make_reader_fit_bits(skip_bits);
			reader_head.bit_header -= skip_bits;

			clamp_bit_header();

		} break;

		case SH_SKIP_BYTE_STATEMENT: {
			i32 skip_bits = (i32)eval_expr(st->skip, 0).ival;// cannot have 
			skip_bits *= 8;

			make_reader_fit_bits(skip_bits);
			reader_head.bit_header -= skip_bits;

			clamp_bit_header();

		} break;


		case SH_REWIND_STATEMENT: {
			i32 rewind_bits = (i32)eval_expr(st->rewind, 0).ival;
			rewind_reader_head_by_bits(rewind_bits);
		} break;

		default: {
			assert_exit(false, "unhandled statement read %s", sh_print_statement(st));
		}
	}

	return size_read;
}



void test_binary_file(char *template_filename, char *binary_filename) {

	puts("----------------------------------------------");
	printf("test: %s | %s\n", template_filename, binary_filename);
	puts("----------------------------------------------\n");
	buf_clear(tokens);
	buf_clear(decls);
	buf_clear(val_decls);
	buf_clear(stmts);

	while(buf_len(type_table) > __COUNTER__) {
		buf_pop(type_table);
	}

	data_stream = (u8*)read_file(binary_filename, NULL);

	compile(template_filename);

	reader_head = (bit_stream){0};
	reader_head.stream = data_stream;
	reader_head.available = 0;
	reader_head.bits = 0;

	for(sh_statement **st = stmts; st != buf_end(stmts); st++) {
		read_stmt(st[0]);
	}

	for(sh_decl_val **d = val_decls; d != buf_end(val_decls); d++) {
		printf("%s = ", sh_print_decl(d[0]->decl));
		sh_print_decl_val(d[0]);
		putchar('\n');
	}

	free(data_stream);
	buf_free(tokens);
	buf_free(decls);
	buf_free(val_decls);
	buf_free(stmts);

	// printf("==================%s========================\n\n", filename);
}

void test_file(char *filename) {

	char buffer[256] = "tests/";
	char *bin = _strdup(strncat(buffer, filename, 256));
	char *tst = _strdup(strncat(buffer, ".sh_it", 256));

	test_binary_file(tst, bin);

	free(bin);
	free(tst);
}

#endif
