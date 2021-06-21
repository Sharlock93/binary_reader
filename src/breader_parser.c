sh_expression* sh_parse_assert_assignment_expr(void) {

	assert_exit(is_token(SH_IDENTIFIER),"Left side of assignment must be a field name got %s",current_token->name);

	sh_expression *assignment_expr = sh_new_expr(SH_FIELD_ASSIGNMENT_EXPR);
	assignment_expr->left = sh_parse_field_name();

	assert_exit(expect_token(SH_ASSIGNMENT), "Expected '=' got %s\n", current_token->name);

	assignment_expr->right = sh_parse_assert_tag_expr();

	return assignment_expr;
}

sh_expression* sh_parse_assert_struct_literal(void) {

	assert_exit(expect_token(SH_OPEN_BRACE), "Expected '{' got %s.", current_token->name);
	assert_exit(is_token(SH_CLOSE_BRACE) == 0, "Cannot have empty struct literal" );
	sh_expression *struct_literal = sh_new_expr(SH_STRUCT_LITERAL);

	do {

		if(current_token[1].type.base == '=') {  // assignment statement/field assignment expr
			buf_push(struct_literal->fields, sh_parse_assert_assignment_expr());
		} else {
			buf_push(struct_literal->fields, sh_parse_assert_tag_expr());
			sh_expression* blen = struct_literal->fields[buf_len(struct_literal->fields)-1];

			if(is_token(SH_ASSIGNMENT)) {
				assert_exit(false, "Cannot use expression as field name %s\n", sh_print_expr(blen));
			}

		}

	} while(expect_token(SH_COMMA));


	assert_exit(expect_token(SH_CLOSE_BRACE), "Expected '}' got %s\n", current_token->name);

	return struct_literal;

}


sh_expression* sh_parse_assert_array_literal(void) {
	assert_exit(expect_token(SH_OPEN_BRACKET), "Expected '[' got %s.", current_token->name);
	assert_exit(is_token(SH_CLOSE_BRACKET) == 0, "Cannot have empty array literal" );
	sh_expression *array_literal = sh_new_expr(SH_ARRAY_LITERAL);

	do {
		buf_push(array_literal->values, sh_parse_assert_tag_expr());
	} while(expect_token(SH_COMMA));


	assert_exit(expect_token(SH_CLOSE_BRACKET), "Expected '}' got %s\n", current_token->name);

	return array_literal;
}


sh_expression* sh_parse_base_tag_expr(void) {

	sh_expression* new_expr = NULL;

	switch(current_token->type.base) {
		case SH_INT: {
			new_expr = sh_expr_int_literal();
			sh_next_token();
		} break;
		case SH_STRING: {
			new_expr = sh_expr_string_literal();
			sh_next_token();
		} break;

		case SH_IDENTIFIER: {
			new_expr = sh_parse_id_expr();
		} break;

		case SH_OPEN_PARAN: {
			sh_next_token();
			new_expr = sh_parse_assert_tag_expr();
			assert(expect_token(')'));
		} break;

		case SH_OPEN_BRACKET:  {
			new_expr = sh_parse_assert_array_literal();
		} break;

		case SH_OPEN_BRACE:  {
			new_expr = sh_parse_assert_struct_literal();
		} break;

		case SH_FLOAT: {
			new_expr = sh_expr_float_literal();
			sh_next_token();
		} break;

		case SH_DOT_OPERATOR: {

			sh_token_base_type type = current_token->type.base;
			sh_next_token();
			new_expr = sh_new_field_access_expr(new_expr, sh_parse_field_name());
			new_expr->access_type = type;

		} break;

		default:  {
			assert_exit(false, "expected a literal or a variable got %s\n", current_token->name);
		} break;
	}

	while(true) {
		switch(current_token->type.base) {
			case '[': {
				new_expr = sh_new_array_index_expr(new_expr, sh_array_index_expr());
				expect_token(']');
			} break;

			case SH_DOT_OPERATOR:
			case SH_POINTER_ACCESS: {
				sh_token_base_type type = current_token->type.base;
				sh_next_token();
				new_expr = sh_new_field_access_expr(new_expr, sh_parse_field_name());
				new_expr->access_type = type;
			} break;

			default: goto break_out; break;
		}
	}

break_out:

	return new_expr;
}

sh_expression* sh_parse_unary_tag_expr() {
	return sh_parse_base_tag_expr();
}

sh_expression* sh_parse_bitwise_tag_expr() {
	sh_expression* expr = sh_parse_unary_tag_expr();
	return expr;
}




sh_expression* sh_parse_multi_tag_expr() {
	sh_expression* expr = sh_parse_bitwise_tag_expr();

	while(is_token(SH_ASTERISK) || is_token(SH_DIV)) {
		sh_expr_operator op = current_token->type.base;
		sh_next_token();
		expr = sh_new_op_expr(expr, sh_parse_logical_tag_expr(), op);
	}

	return expr;
}


sh_expression* sh_parse_addition_tag_expr() {
	sh_expression* expr = sh_parse_multi_tag_expr();

	while( is_token( SH_PLUS ) ||  is_token(SH_MINUS)) {
		sh_expr_operator op = current_token->type.base;
		sh_next_token();
		expr = sh_new_op_expr(expr, sh_parse_multi_tag_expr(), op);
	}

	return expr;
}


sh_expression* sh_parse_logical_tag_expr() {
	sh_expression *expr = NULL;
	i8 is_and_or = current_token->type.base == SH_AND || current_token->type.base == SH_OR;

	if(current_token->type.base >= SH_LOGICAL_OP_START && current_token->type.base <= SH_LOGICAL_OP_END && !is_and_or) {
		while(current_token->type.base >= SH_LOGICAL_OP_START && current_token->type.base <= SH_LOGICAL_OP_END && !is_and_or) {
			sh_expr_operator op = current_token->type.base;
			sh_next_token();
			expr = sh_new_op_expr(expr, sh_parse_addition_tag_expr(), op);

			is_and_or = current_token->type.base == SH_AND || current_token->type.base == SH_OR;
		}
	} else {
		expr = sh_parse_addition_tag_expr();
	}

	return expr;
}

sh_expression* sh_parse_logical_and_or_tag_expr() {
	sh_expression* expr = sh_parse_logical_tag_expr();

	while(current_token->type.base == SH_AND || current_token->type.base == SH_OR) {
		sh_expr_operator op = current_token->type.base;
		sh_next_token();
		expr = sh_new_op_expr(expr, sh_parse_logical_tag_expr(), op);
	}


	return expr;
}


sh_expression* sh_parse_assert_tag_expr() {
	sh_expression *expr = sh_parse_logical_and_or_tag_expr();
	return expr;
}

sh_decl_tag* sh_parse_assert_tag() {
	sh_decl_tag *tag = sh_new_decl_tag(SH_ASSERT_TAG);
	assert_exit(expect_token(SH_OPEN_PARAN), "Expected ( got %s\n", current_token->name);
	tag->asrt.expr = sh_parse_assert_tag_expr();
	assert_exit(expect_token(SH_CLOSE_PARAN), "Expected ) got %s\n", current_token->name );
	return tag;
}

sh_decl_tag* sh_parse_cond_tag() {
	sh_decl_tag *tag = sh_new_decl_tag(SH_COND_TAG);
	assert_exit(expect_token(SH_OPEN_PARAN), "Expected ( got %s\n", current_token->name);
	tag->asrt.expr = sh_parse_expression();
	assert_exit(expect_token(SH_CLOSE_PARAN), "Expected ) got %s\n", current_token->name );
	return tag;
}

sh_decl_tag* sh_parse_add_on_tag(void) {
	sh_decl_tag *tag = sh_new_decl_tag(SH_ADD_ON_TAG);
	assert_exit(expect_token(SH_OPEN_PARAN), "Expected ( got %s\n", current_token->name);
	tag->asrt.expr = sh_parse_expression();
	assert_exit(expect_token(SH_CLOSE_PARAN), "Expected ) got %s\n", current_token->name );
	return tag;
}



#define STR_CHECK(str) is_str_id_eq(str, sizeof(str) - 1)
sh_decl_tag* sh_parse_offset_tag(void) {
	sh_decl_tag *tag = sh_new_decl_tag(SH_OFFSET_TAG);

	assert_exit(expect_token(SH_OPEN_PARAN), "Expected ( got %s\n", current_token->name);

	assert_exit(is_token(SH_IDENTIFIER) || is_token(SH_BXOR), "Expected one of (file_start, start, current, -) got %s\n", current_token->name);

	sh_offset_type type = SH_OFFSET_CURRENT;

	i32 parent_count = 0;
	while(expect_token(SH_BXOR)) {
		parent_count++;
	}

	if(STR_CHECK("file_start")) { type = SH_OFFSET_FILESTART; }
	else if(STR_CHECK("start")) { type = SH_OFFSET_START; }
	else if(STR_CHECK("current")) {  }
	else if(STR_CHECK("-")) {}
	else { assert_exit(false, "Expected one of (file_start, start, current, -) got %s\n", current_token->name); }
	sh_next_token();

	tag->offset.start = type;//

	assert_exit(expect_token(SH_COMMA), "Expected , got %s\n", current_token->name);

	tag->offset.expr = sh_parse_expression();
	if(type == SH_OFFSET_START) tag->offset.parent_count = parent_count;

	assert_exit(expect_token(SH_CLOSE_PARAN), "Expected ) got %s\n", current_token->name );
	return tag;
}
#undef STR_CHECK


sh_print_tag sh_parse_print_options(void) {
	sh_print_tag p = {0};
	assert_exit(current_token->name_len == 1, "Expected any of (h, d, b, o) got %s\n", current_token->name);

	char f = current_token->name[0];
	switch(f) {
		case 'h': case 'H': { p.type = SH_HEX_PRINT; } break;
		case 'o': case 'O': { p.type = SH_OCT_PRINT; } break;
		case 'd': case 'D': { p.type = SH_DEC_PRINT; } break;
		case 'b': case 'B': { p.type = SH_BIN_PRINT; } break;
		default: { assert_exit(false, "Expected any of (h, d, b, o) got %s\n", current_token->name);}
	}

	sh_next_token();

	return p;
}

sh_decl_tag* sh_parse_print_tag() {
	sh_decl_tag *tag = sh_new_decl_tag(SH_PRINT_TAG);
	assert_exit(expect_token(SH_OPEN_PARAN), "Expected ( got %s\n", current_token->name);
	tag->print = sh_parse_print_options();
	assert_exit(expect_token(SH_CLOSE_PARAN), "Expected ) got %s\n", current_token->name );
	return tag;
}


sh_array_print_tag sh_parse_array_print_options(void) {

	sh_array_print_tag p = {-1, -1, -1, 1, 1, ','};

	if(!expect_token(SH_MINUS)) {
		assert_exit(is_token(SH_INT), "Expected a number got %s", current_token->name);
		i32 val = parse_int_token(current_token);
		p.count = val;
		sh_next_token();
	}


	if(expect_token(SH_COMMA)) {

		if(!expect_token(SH_MINUS)) {
			assert_exit(is_token(SH_INT), "Expected a number got %s", current_token->name);
			i32 val = parse_int_token(current_token);
			p.start = val;
			sh_next_token();
		}
	}

	if(expect_token(SH_COMMA)) {

		if(!expect_token(SH_MINUS)) {
			assert_exit(is_token(SH_INT), "Expected a number got %s", current_token->name);
			i32 val = parse_int_token(current_token);
			p.row = val;
			sh_next_token();
		}
	}

	if(expect_token(SH_COMMA)) {
		if(!expect_token(SH_MINUS)) {
			assert_exit(is_token(SH_INT), "Expected a number got %s", current_token->name);
			i32 val = parse_int_token(current_token);
			p.print_index = val;
			sh_next_token();
		}
	}

	if(expect_token(SH_COMMA)) {
		if(!expect_token(SH_MINUS)) {
			assert_exit(is_token(SH_INT), "Expected a number got %s", current_token->name);
			i32 val = parse_int_token(current_token);
			p.newline = val;
			sh_next_token();
		}
	}

	if(expect_token(SH_COMMA)) {
		if(!expect_token(SH_MINUS)) {
			assert_exit(
					is_token(SH_IDENTIFIER) && current_token->name_len == 1,
					"Expected a character got %s", current_token->name
					);
			p.seperator = current_token->name[0];
			sh_next_token();
		}
	}



	return p;
}


sh_decl_tag* sh_parse_array_print_tag() {
	sh_decl_tag *tag = sh_new_decl_tag(SH_ARRAY_PRINT_TAG);
	assert_exit(expect_token(SH_OPEN_PARAN), "Expected ( got %s\n", current_token->name);
	tag->array_print = sh_parse_array_print_options();
	assert_exit(expect_token(SH_CLOSE_PARAN), "Expected ) got %s\n", current_token->name );
	return tag;
}


sh_struct_print_tag sh_parse_struct_print_options(void) {

	sh_struct_print_tag p = {-1, -1, ','};

	if(!expect_token(SH_MINUS)) {
		assert_exit(is_token(SH_INT), "Expected a number got %s", current_token->name);
		i32 val = parse_int_token(current_token);
		p.field_name = val;
		sh_next_token();
	}


	if(expect_token(SH_COMMA)) {

		if(!expect_token(SH_MINUS)) {
			assert_exit(is_token(SH_INT), "Expected a number got %s", current_token->name);
			i32 val = parse_int_token(current_token);
			p.newline = val;
			sh_next_token();
		}
	}

	// if(expect_token(SH_COMMA)) {
	// 	if(!expect_token(SH_MINUS)) {
	// 		assert_exit(
	// 				is_token(SH_IDENTIFIER) && current_token->name_len == 1,
	// 				"Expected a character got %s", current_token->name
	// 				);
	// 		p.seperator = current_token->name[0];
	// 		sh_next_token();
	// 	}
	// }



	return p;
}

sh_decl_tag* sh_parse_struct_print_tag(void) {
	sh_decl_tag *tag = sh_new_decl_tag(SH_STRUCT_PRINT_TAG);
	assert_exit(expect_token(SH_OPEN_PARAN), "Expected ( got %s\n", current_token->name);
	tag->struct_print = sh_parse_struct_print_options();
	assert_exit(expect_token(SH_CLOSE_PARAN), "Expected ) got %s\n", current_token->name );
	return tag;
}

