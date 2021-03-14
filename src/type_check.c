typedef struct sh_semantic_type sh_semantic_type;

sh_semantic_type* sh_type_check_var_decl(sh_decl *decl);
sh_semantic_type* sh_type_check_expr(sh_expression* expr);
sh_semantic_type* sh_type_check_decl(sh_decl *decl);
sh_semantic_type* sh_type_check_expr_with_type(sh_expression* expr, sh_semantic_type* type_check);
sh_semantic_type* sh_type_check_expr(sh_expression *expr);
sh_semantic_type* sh_type_check_stmt(sh_statement *stmt, sh_semantic_type *check_type_against);


typedef struct sh_semantic_type {
	sh_typespec *base_type;
	i32 size_byte;
	i32 is_ptr;
	i32 is_rvalue;
	i32 array_size;
	i32 is_nil;

	union {
		i64 int_literal;
		f64 float_literal;
		i64 const_val;
		char *str_value;
	};

} sh_semantic_type;


sh_semantic_type* sh_new_semantic_type(sh_typespec *base_type) {
	sh_semantic_type *t = (sh_semantic_type*)calloc(1, sizeof(sh_semantic_type));
	t->base_type = base_type;//sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, base_type);
	t->size_byte = base_type->size_byte;
	t->is_rvalue = 1;
	return t;
}


sh_semantic_type* sh_new_semantic_type_typespec(sh_typespec* type) {
	sh_semantic_type *t = sh_new_semantic_type(type);
	t->is_ptr = type->type == SH_TYPE_PTR;
	t->size_byte = type->size_byte;

	return t;
}

sh_semantic_type* sh_new_nil_semantic_type(void) {
	sh_semantic_type *t = sh_new_semantic_type(sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_nil));
	t->is_nil = 1;
	return t;
}

sh_semantic_type* sh_new_const(sh_type* base_type, i32 const_value) {
	sh_semantic_type* t = sh_new_semantic_type(sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, base_type));
	t->const_val = const_value;
	t->size_byte = base_type->size_byte;
	return t;
}

sh_semantic_type* sh_new_int_literal(i64 int_val) {
	sh_semantic_type* t = sh_new_semantic_type(
			sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_i32));
	t->int_literal = int_val;
	t->size_byte = sh_type_i64.size_byte;
	return t;
}

sh_semantic_type* sh_new_array_literal(sh_typespec* base_type, sh_expression *array_size_expr) {
	sh_semantic_type* t = sh_new_semantic_type( base_type);
	t->base_type->array_size_expr = array_size_expr;
	return t;
}


sh_semantic_type* sh_new_float_literal(f64 float_val) {
	sh_semantic_type* t = sh_new_semantic_type( sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_f32));
	t->float_literal = float_val;
	t->size_byte = sh_type_f64.size_byte;
	return t;
}

sh_semantic_type* sh_new_string_literal(char* str_value, i32 str_len) {
	sh_semantic_type* t = sh_new_semantic_type(
			sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_string)
	);
	t->str_value = str_value;
	t->size_byte = str_len; // do a strlen?

	return t;
}

sh_semantic_type* sh_type_check_struct_field(sh_type* type, char* name, i32 name_len) {

	assert_exit(type->is_struct == 1, "Type is not struct");

	sh_decl **fields = type->struct_type.fields;

	for(i32 i = 0; i < buf_len(fields); i++ ) {
		sh_decl *field = fields[i];
		if(field->name_len == name_len && strncmp(field->name, name, name_len) == 0 ) {
			return sh_type_check_decl(field);
		}
	}

	return NULL;
}

i32 sh_semantic_type_equal(sh_semantic_type *a, sh_semantic_type *b) {
	i32 is_equal = true;


	sh_typespec *a_ts = a->base_type;
	sh_typespec *b_ts = b->base_type;

	if(b->is_nil) {
		if(a->is_ptr)
			return 1;
		else {
			return 0;
		}
	}

	while(true) {

		if(a_ts->type != b_ts->type) {
			is_equal = false;
			break;
		}

		if(a_ts->type != SH_TYPE_DEFINED_TYPE) {
			a_ts = a_ts->base;
			b_ts = b_ts->base;
		} else {
			is_equal = a_ts->base_type == b_ts->base_type;
			break;
		}

	}


	return is_equal;
}





sh_semantic_type* sh_type_check_array_literal_expr(sh_expression *expr, sh_semantic_type* type_check) {
	sh_semantic_type *first_value = sh_type_check_expr(expr->values[0]);

	for(sh_expression **exprs = expr->values+1; exprs != buf_end(expr->values); exprs++) {
		sh_expression *e = exprs[0];
		sh_semantic_type *sm_type = sh_type_check_expr(e);

		if(!sh_semantic_type_equal(first_value, sm_type)) {
			printf("Array has mixed types all elems must be %s\n",
					sh_print_typespec(first_value->base_type));
			exit(0);
		}

		free(sm_type);
	}

	sh_typespec* array_typespec = sh_new_typespec(SH_TYPE_ARRAY, first_value->base_type);

	free(first_value);

	sh_semantic_type *st = sh_new_array_literal(
			array_typespec,
			sh_new_int_literal_expr(buf_len(expr->values))
			);

	return st;
}

sh_semantic_type* sh_type_check_struct_literal_expr(sh_expression *expr, sh_semantic_type *type_check) {

	assert_exit(type_check != NULL, "Cannot deduce the type of struct literal.");

	if(type_check->base_type->type != SH_TYPE_DEFINED_TYPE && type_check->base_type->base_type->is_struct != 1) {
		assert_exit(false, "Cannot assign struct literal to type %s\n", sh_print_typespec(type_check->base_type));
	}


	sh_expression **fields = expr->fields;
	i32 struct_literal_len = buf_len(fields);

	sh_type *struct_type = type_check->base_type->base_type;
	i32 struct_type_field_len = buf_len(struct_type->struct_type.fields);
	if(struct_literal_len > struct_type_field_len) {
		printf("size mismatch struct literal has length %d, struct type %s has %d members\n", struct_literal_len, struct_type->name, struct_type_field_len);
	}


	i32 field_index = 0;
	for(i32 i = 0; i <  buf_len(fields); i++) {

		sh_expression *field = fields[i];

		if(field->type == SH_FIELD_ASSIGNMENT_EXPR) {
			assert_exit(field->left->type == SH_ID_EXPR, "not a field name.");
			sh_semantic_type* field_type = sh_type_check_struct_field(struct_type, field->left->name, field->left->name_len);

			assert_exit(field_type != NULL, "could not find the field ( %s ) for type ( struct %s ) ", field->left->name, struct_type->name);

			sh_semantic_type* assign_type = sh_type_check_expr_with_type(field->right, field_type);

			if(!sh_semantic_type_equal(field_type, assign_type)) {
				char *left = sh_print_typespec(field_type->base_type);
				char *right = sh_print_typespec(assign_type->base_type);
				printf("cannot assign type %s to %s for struct field %s", right, left, field->left->name);
				exit(0);
			}

			field_index = sh_find_struct_field_index(struct_type, field->left->name, field->left->name_len);

		} else {

			if(field_index >= struct_type_field_len) {
				/* printf("reached end of struct fields but struct literal had more values."); */
				printf("extra value in struct literal after named last field");
				exit(1);
			}

			sh_decl *struct_field = struct_type->struct_type.fields[field_index];

			sh_semantic_type* struct_field_type = sh_new_semantic_type_typespec(struct_field->struct_field.type);
			sh_semantic_type* literal_field_type = sh_type_check_expr_with_type(field, struct_field_type);

			if(!sh_semantic_type_equal(struct_field_type, literal_field_type)) {
				char *right =  sh_print_typespec(literal_field_type->base_type);
				char *left = sh_print_typespec(struct_field_type->base_type);
				printf("cannot assign type %s to %s for struct field %s", right, left, struct_field->name);
				exit(0);
			}

		}

		field_index++;
	}


	return type_check;

}

sh_semantic_type* sh_type_check_id_expr(sh_expression *expr, sh_semantic_type *type) {
	// highly questionable, 
	sh_decl *decl = sh_get_decl(expr->name, expr->name_len); 
	if(decl == NULL) {
		printf("unknown identifier %s\n", expr->name);
		exit(1);
	}
	sh_semantic_type *t = sh_type_check_decl(decl);
	t->is_rvalue = 0;
	return t;

}

sh_semantic_type* sh_type_check_operator_expr(sh_expression *expr, sh_semantic_type *type) {

	sh_semantic_type *left_op = sh_type_check_expr(expr->left_op);
	sh_semantic_type *right_op = sh_type_check_expr(expr->right_op);

	if(!sh_semantic_type_equal(left_op, right_op)) {
		char *left_type = sh_print_typespec(left_op->base_type);
		char *right_type = sh_print_typespec(right_op->base_type);

		printf("operator %s cannot be used between type %s and %s ",
				base_type_names[expr->op], left_type, right_type);

		exit(0);
	}

	free(right_op);

	left_op->is_rvalue = 1;

	return left_op;

}

sh_semantic_type* sh_type_check_array_expr(sh_expression* expr, sh_semantic_type* type_check) {
	sh_semantic_type *expr_type = sh_type_check_expr(expr->array_expr);
	assert_exit(expr->array_index_expr != NULL, "array index cannot be empty");

	sh_semantic_type *array_index = sh_type_check_expr(expr->array_index_expr);

	if(expr_type->base_type->type != SH_TYPE_ARRAY) {
		printf("Cannot index a non-array type, tried to index type: %s",
				sh_print_typespec(expr_type->base_type));
		exit(1);
	}

	if(!sh_semantic_type_equal(array_index, sh_new_int_literal(0))) {

		char *arr_expr = sh_print_expr(expr->array_index_expr);
		char *index_exp = sh_print_typespec(array_index->base_type);
		printf("Array index must be an integer, expr (%s) has type %s",
				arr_expr, index_exp);
		exit(1);
	}
	sh_semantic_type *t = sh_new_semantic_type(expr_type->base_type->base);
	t->is_rvalue = 0;
	return t;

}

sh_semantic_type* sh_type_check_field_access_expr(sh_expression* expr, sh_semantic_type* type_check) {

	sh_semantic_type *before_op = sh_type_check_expr(expr->pointer_expr);

	if(before_op->base_type->base_type->is_struct != 1) {
		printf("Only structs can be accessed, %s has type %s", sh_print_expr(expr->pointer_expr), sh_print_typespec(before_op->base_type));
		exit(0);
	}

	i32 index = sh_find_struct_field_index(
			before_op->base_type->base_type,
			expr->field_access->name,
			expr->field_access->name_len
			);

	if(index == -1) {
		assert_exit(false, "Field name %s does not exist on type %s\n", expr->field_access->name,
				sh_print_typespec(before_op->base_type)
				);
	}

	sh_semantic_type *t = sh_type_check_struct_field(before_op->base_type->base_type, expr->field_access->name, expr->field_access->name_len);
	t->is_rvalue = 0;
	return t;

}

sh_semantic_type* sh_type_check_expr(sh_expression *expr) {
	return sh_type_check_expr_with_type(expr, NULL);
}

sh_semantic_type* sh_type_check_expr_with_type(sh_expression* expr, sh_semantic_type* type_check) {
	
	switch(expr->type) {
		case SH_UNKNOWN_EXPR: {
			printf(" unknown expr \n");
			exit(0);
		} break;
		case SH_INT_LITERAL: {
			return sh_new_int_literal(expr->vi64);
		} break;

		case SH_STRING_LITERAL: {
			return sh_new_string_literal(expr->str_val, expr->str_size);
		} break;

		case SH_FLOAT_LITERAL: {
			return sh_new_float_literal(expr->vf64);
		} break;

		case SH_ARRAY_LITERAL: {
			return sh_type_check_array_literal_expr(expr, type_check);
		} break;

		case SH_STRUCT_LITERAL: {
			return sh_type_check_struct_literal_expr(expr, type_check);
		} break;

		case SH_NIL_LITERAL: {
			return sh_new_nil_semantic_type();
		} break;

		case SH_ID_EXPR: {
			return sh_type_check_id_expr(expr, type_check);
		} break;

		case SH_PTR_DEREF_EXPR: {
			sh_semantic_type *t = sh_type_check_expr(expr->operand);

			assert_exit(t->is_ptr == 1, "Cannot dereference non-pointer type.");

			sh_semantic_type *t_no_ptr = sh_new_semantic_type(t->base_type->base);
			t->is_rvalue = 0;
			return t_no_ptr;
		} break;
		case SH_OPERATOR_EXPR: {
			return sh_type_check_operator_expr(expr, type_check);
		} break;

		case SH_DEC_EXPR:
		case SH_INC_EXPR: {
			return sh_type_check_expr(expr->operand);
		} break;

		case SH_POST_INC_EXPR:
		case SH_POST_DEC_EXPR: {
			return sh_type_check_expr(expr->operand);
		} break;

		case SH_ARRAY_EXPR: {
			return sh_type_check_array_expr(expr, type_check);
			} break;

		case SH_FIELD_ACCESS_EXPR: {
			return sh_type_check_field_access_expr(expr, type_check);
		} break;

		case SH_FUNC_EXPR: {
			sh_semantic_type *func_type = sh_type_check_expr(expr->func_expr);
			return func_type;
		} break;

		case SH_ADDRESS_OF_EXPR: {
			sh_semantic_type *t = sh_type_check_expr(expr->operand);
			assert_exit(!t->is_rvalue, "address of operator expects an l-value.");
			return sh_new_semantic_type(sh_new_typespec(SH_TYPE_PTR, t->base_type));
		} break;

		default: {
			assert_exit(false, "we don't handle this expr type: %s", sh_print_expr(expr));
		} break;

	}

	return NULL;
}

sh_semantic_type* sh_type_check_return_stmt(sh_statement *stmt, sh_semantic_type *check_type_against) {

	sh_semantic_type *ret_stmt_type = NULL;

	if(stmt->ret_expr) {
		ret_stmt_type = sh_type_check_expr(stmt->ret_expr);
	} else {
		ret_stmt_type = sh_new_semantic_type(
				sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_void)
				);
	}

	if(!sh_semantic_type_equal(ret_stmt_type, check_type_against)) {

		char *ret_stmt_type_str = sh_print_typespec(ret_stmt_type->base_type);
		char *func_type = sh_print_typespec(check_type_against->base_type);
		printf("return statement has type %s function has return type of %s\n",
				ret_stmt_type_str, func_type);
		exit(1);
	}

	return ret_stmt_type;

}

//@note: should return something from compound stmt?? 
sh_semantic_type* sh_type_check_compound_stmt(sh_statement *stmt, sh_semantic_type *check_type_against) { 
	sh_statement **comp_stmt = stmt->statements;

	for(sh_statement **s = comp_stmt; s != buf_end(comp_stmt); s++) {
		sh_statement *st = s[0];
		sh_type_check_stmt(st, check_type_against);
	}

	return NULL;//
}

//@Note: should we check against left and right? 
sh_semantic_type* sh_type_check_assignment_stmt(sh_statement *stmt, sh_semantic_type *check_type_against) {
	sh_semantic_type *left = sh_type_check_expr(stmt->left_side_expr);

	sh_semantic_type *right = sh_type_check_expr_with_type(stmt->right_side_expr, left);

	if(!sh_semantic_type_equal(left, right)) {
		char *right_type = sh_print_typespec(right->base_type);
		char *left_type = sh_print_typespec(left->base_type);
		char *stmt_str = sh_print_statement(stmt);
		printf("cannot assign type %s to type %s\n %s", right_type, left_type, stmt_str);
		exit(0);
	}


	return left;
}


sh_semantic_type* sh_type_check_elif_stmt(sh_statement* stmt, sh_semantic_type *type_check) {

	if(stmt->condition_expr) {
		sh_type_check_expr(stmt->condition_expr);
	}

	if(stmt->comp_statement) {
		return sh_type_check_stmt(stmt->comp_statement, type_check);
	}

	return NULL;
}


sh_semantic_type* sh_type_check_if_stmt(sh_statement* stmt, sh_semantic_type *type_check) {
	if(stmt->init_statement) {
		sh_type_check_stmt(stmt->init_statement, type_check);
	}

	if(stmt->condition_expr) {
		sh_type_check_expr(stmt->condition_expr);
	}

	if(stmt->comp_statement) {
		sh_type_check_stmt(stmt->comp_statement, type_check);
	} // probably should return a value 

	sh_statement **elseifs = stmt->elseif_stmts;
	while(elseifs != buf_end(stmt->elseif_stmts)) {
		sh_type_check_elif_stmt(elseifs[0], type_check);
		elseifs++;
	}

	if(stmt->else_stmt) {
		sh_type_check_stmt(stmt->else_stmt, type_check);
	}

	return NULL;
}

sh_semantic_type* sh_type_check_stmt(sh_statement *stmt, sh_semantic_type *check_type_against) {

	switch(stmt->type) {
		case SH_RETURN_STATEMENT: {
			return sh_type_check_return_stmt(stmt, check_type_against);
		} break;

		case SH_COMPOUND_STATEMENT: {
			
			return sh_type_check_compound_stmt(stmt, check_type_against);
			return NULL;
		} break;

		case SH_VAR_DECL_STATEMENT: {
			return sh_type_check_decl(stmt->var_decl);
		} break;

		case SH_ASSIGNMENT_STATEMENT: {
			return sh_type_check_assignment_stmt(stmt, check_type_against);
		} break;

		case SH_FUNC_CALL_STATEMENT: {
			return sh_type_check_expr(stmt->unary_expr);;
		} break;

		case SH_INC_DEC_STATEMENT: {
			return sh_type_check_expr(stmt->unary_expr);;
	 	} break;

		case SH_IF_STATEMENT: {
			return sh_type_check_if_stmt(stmt, check_type_against);
			
		} break;
		case SH_WHILE_STATEMENT:
		case SH_FOR_STATEMENT: {
			if(stmt->init_statement) { return sh_type_check_stmt(stmt->init_statement, check_type_against); }
			if(stmt->condition_expr) { return sh_type_check_expr(stmt->condition_expr); }
			if(stmt->post_loop_expr) { return sh_type_check_expr(stmt->post_loop_expr); }
			if(stmt->comp_statement) { return sh_type_check_stmt(stmt->comp_statement, check_type_against); } // probably should return a value 
			return NULL;
		} break;

		case SH_BREAK_STATEMENT: break; //pass in parrent
		case SH_CONTINUE_STATEMENT: break;
	}

	return NULL;
}

sh_semantic_type* sh_type_check_var_decl(sh_decl *decl) {

	sh_semantic_type *var_type = sh_new_semantic_type_typespec(decl->var.type);

	if(!decl->type_checked && decl->var.init_expr) {
		sh_semantic_type *t = sh_type_check_expr_with_type(decl->var.init_expr, var_type);

		if(!sh_semantic_type_equal(var_type, t)) {
			char *expected_type = sh_print_typespec(var_type->base_type);
			char *got_type = sh_print_typespec(t->base_type);
			printf("type mismatched expected %s got %s", expected_type, got_type);
			exit(0);
		}


		decl->type_checked = 1;
	}

	decl->total_size = var_type->size_byte;

	return var_type;
}


sh_semantic_type* sh_type_check_func_decl(sh_decl *decl) {
	sh_semantic_type *ret_type = sh_new_semantic_type(decl->func.return_type);

	if(decl->type_checked) return ret_type;

	sh_statement **comp_stmt = decl->func.compound_statement->statements;
	i32 has_return_stmt = 0;

	for(sh_statement **stmt = comp_stmt; stmt != buf_end(comp_stmt); stmt++) {
		sh_statement *st = stmt[0];

		if(st->type == SH_RETURN_STATEMENT) has_return_stmt = 1;

		sh_type_check_stmt(st, ret_type);
	}

	if(!has_return_stmt) { 
		if(ret_type->base_type->base_type != &sh_type_void) {
			printf("error: \"%s\" ", decl->name);
			char *must_return = sh_print_typespec(ret_type->base_type);
			printf("no return statement found, function must return type %s\n", must_return);
			free(must_return);
		}
	} 

	decl->type_checked = 1;
	return ret_type;
}

sh_semantic_type* sh_type_check_struct_decl(sh_decl *decl) {
	sh_decl **fields = decl->struct_decl.fields;

	while(fields != buf_end(decl->struct_decl.fields)) {
		sh_type_check_decl(fields[0]);
		fields++;
	}

	decl->type_checked = 1;

	return sh_new_semantic_type_typespec(sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, sh_get_type_name(decl->name, decl->name_len)));
}

sh_semantic_type* sh_type_check_decl(sh_decl *decl) {

	switch(decl->type) {
		case SH_VAR_DECL: {
			return sh_type_check_var_decl(decl);
		} break;
		case SH_STRUCT_FIELD_DECL: {
			return sh_new_semantic_type_typespec(decl->struct_field.type);
		} break;
		case SH_FUNC_DECL: {
			return sh_type_check_func_decl(decl);
		} break;

		case SH_STRUCT_DECL: {
			return sh_type_check_struct_decl(decl);
		} break;

		default: {
			assert_exit(false, "Uknown decl type %s", sh_print_decl(decl));
		} break;
	}


	return NULL;
}
