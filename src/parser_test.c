// foward declarations
// copy src to dest and free src, return len of src without null, uses strlen

i32 has_func_call = 0;


typedef enum sh_storage_type {
	SH_NO_STORAGE,
	SH_GLOBAL_STORAGE,
	SH_LOCAL_STORAGE
} sh_storage_type;


typedef struct sh_typespec {
	sh_type_kind type;
	i32 array_count;
	i32 size_byte;
	i32 size_bit;

	union {
		sh_type *base_type;
		sh_typespec *base;
		sh_statement *when;
	};
	sh_expression *array_size_expr;
} sh_typespec;

typedef enum sh_decl_type {
	SH_UNKNOWN_DECL,
	SH_VAR_DECL,
	SH_FUNC_DECL,
	SH_TYPEDEF_DECL,
	SH_ARRAY_DECL,
	SH_STRUCT_DECL,
	SH_STRUCT_FIELD_DECL,
	SH_ENUM_DECL,
	SH_ENUM_FIELD_DECL,
} sh_decl_type;// possibly more

char *decl_type_name[] = {
	[ SH_VAR_DECL ] = "variable",
	[ SH_FUNC_DECL ] = "function",
	[ SH_ARRAY_DECL ] = "array",
	[ SH_STRUCT_DECL ] = "struct",
	[ SH_STRUCT_FIELD_DECL ] = "field",
};

typedef struct sh_func_decl {
	sh_decl **args;
	sh_typespec *return_type;
	sh_statement *compound_statement;
} sh_func_decl;

typedef struct sh_var_decl {
	sh_typespec *type; //type
	sh_expression *init_expr; // initializer
} sh_var_decl;

typedef struct sh_typedef_decl {
	sh_typespec *base;
} sh_typedef_decl;


typedef struct sh_struct_field_decl {
	sh_typespec *type;
	i32 offset; // bytes
} sh_struct_field_decl;


typedef struct sh_struct_decl {
	sh_decl **fields;
} sh_struct_decl;

typedef enum sh_tag_type {
	SH_UNKNOWN_TAG,
	SH_DLL_IMPORT_TAG,
	SH_VAR_IMPORT_TAG,
	SH_ASSERT_TAG,
	SH_COND_TAG,
	SH_ADD_ON_TAG,
	SH_OFFSET_TAG,
	SH_PRINT_TAG,
	SH_ARRAY_PRINT_TAG,
	SH_STRUCT_PRINT_TAG
} sh_tag_type;

typedef struct sh_dll_import_tag {
	char *dll_file_path;
	char *func_name;
} sh_dll_import_tag;

typedef struct sh_var_import_tag {
	char *var_name;
} sh_var_import_tag;

typedef struct sh_enum_decl {
	sh_decl **enum_fields;
} sh_enum_decl;


typedef struct sh_assert_tag {
	sh_expression *expr;
	i8 eval;
} sh_assert_tag;


typedef enum sh_print_type {
	SH_DEC_PRINT,
	SH_HEX_PRINT,
	SH_OCT_PRINT,
	SH_BIN_PRINT
} sh_print_type;

typedef struct sh_print_tag {
	sh_print_type type;
} sh_print_tag;

typedef struct sh_array_print_tag {
	i32 count;
	i32 start;
	i32 row;
	i8 print_index;
	i8 newline;
	char seperator;
} sh_array_print_tag;

typedef struct sh_struct_print_tag {
	i8 field_name;
	i8 newline;
	char seperator;
} sh_struct_print_tag;


typedef struct sh_add_on_tag {
	sh_expression *add_expr;
} sh_add_on_tag;


typedef enum sh_offset_type {
	SH_OFFSET_CURRENT,
	SH_OFFSET_FILESTART,
	SH_OFFSET_START
} sh_offset_type;

typedef struct sh_offset_tag {
	sh_offset_type start;
	sh_expression *expr;
	i32 parent_count;
} sh_offset_tag;


typedef struct sh_decl_tag {
	sh_tag_type type;

	union {
		sh_dll_import_tag dll;
		sh_var_import_tag var;
		sh_assert_tag asrt;
		sh_print_tag print;
		sh_array_print_tag array_print;
		sh_struct_print_tag struct_print;
		sh_add_on_tag add_on;
		sh_offset_tag offset;
	};

} sh_decl_tag;


typedef struct sh_decl {
	sh_decl_type type;
	i32 total_size;
	i32 type_checked;

	sh_storage_type storage;
	sh_memory_info *mem_info;

	sh_decl_tag **tags; // ?
	const char *name;
	i32 name_len;

	union {

		struct { //normal ints? 
			union {
				i8  vi8;
				i16 vi16;
				i32 vi32;
				i64 vi64;
				
				u8 	vu8;
				u16 vu16;
				u32 vu32;
				u64 vu64;

				f32 vf32;
				f64 vf64;
			};
		};

		struct { // arrays ?? 
			union {

				i8  *i8_array;
				i16 *i16_array;
				i32 *i32_array;
				i64 *i64_array;

				u8  *u8_array;
				u16 *u16_array;
				u32 *u32_array;
				u64 *u64_array;

				f32 *f32_array;
				f64 *f64_array;

				u8   *string_array;
				void *array;
			};
		};

	};

	union {
		sh_func_decl func;
		sh_struct_decl struct_decl;
		sh_struct_field_decl struct_field;
		sh_var_decl var;
		sh_enum_decl enum_decl;
		sh_typedef_decl typedef_decl;
	};

	//for arrays
	
} sh_decl;


typedef struct sh_tag_eval {
	sh_tag_type type;
	i8 eval;
} sh_tag_eval;

typedef struct sh_pos {
	i64 byte;
	i64 bit;
} sh_pos;

typedef struct sh_decl_val {
	sh_decl *decl;
	sh_typespec *type;
	i8 is_setup;
	i8 is_partial_setup;
	i8 read_at_least_once;
	i32 array_size;
	i32 str_len;
	sh_decl_val *parent;
	sh_tag_eval *tag_evals;
	i32 read_size_byte;
	i32 read_size_bit;

	i8 checked_child_add_on;
	i8 child_has_add_on;

	sh_pos pos;

	union {
		i64 ival;
		u64 uval;
		f64 fval;
		char  ch;
		char* str;
		void *data;
		sh_decl_val **fields;
		sh_decl_val *field;
		sh_decl_val **array;
	};

} sh_decl_val;



typedef struct when_stmt {
	sh_token type_id;
	sh_typespec *type;
} when_stmt;

typedef enum sh_statement_type {
	
	SH_INITIALIZED_STATEMENT,
	SH_COMPOUND_STATEMENT,
	SH_VAR_DECL_STATEMENT,
	SH_ASSIGNMENT_STATEMENT,
	SH_INC_DEC_STATEMENT,
	/* SH_DEC_STATEMENT, */
	SH_FOR_STATEMENT,
	SH_WHILE_STATEMENT,
	SH_IF_STATEMENT,
	SH_ELIF_STATEMENT,
	SH_ELSE_STATEMENT,
	SH_RETURN_STATEMENT,
	SH_BREAK_STATEMENT,
	SH_CONTINUE_STATEMENT,
	SH_FUNC_CALL_STATEMENT,
	SH_EXPR_STATEMENT,

	SH_SKIP_BIT_STATEMENT,
	SH_SKIP_BYTE_STATEMENT,
	SH_REWIND_STATEMENT,

	SH_WHEN_STATEMENT,

} sh_statement_type;


typedef struct sh_statement {
	sh_statement_type type;
	i32 stmt_size;

	union {

		sh_decl *var_decl;

		sh_expression *unary_expr;
		sh_expression *expr;
		sh_expression *skip;
		sh_expression *rewind;
		sh_expression *peek;

		struct {
			sh_expression *left_side_expr;
			sh_expression *right_side_expr;
		};//assignment operator;

		struct {
			sh_statement *init_statement;
			sh_expression *condition_expr;
			sh_statement *post_loop_expr;
			sh_statement  *comp_statement;

			sh_statement **elseif_stmts;
			sh_statement *else_stmt;
		}; // 

		sh_expression *ret_expr;
		sh_statement **statements;

		struct {
			sh_expression *when_expr;
			when_stmt *when_types;
			sh_typespec *else_type;
		};

	};

} sh_statement;


typedef enum sh_expression_type {
	SH_UNKNOWN_EXPR,
	SH_INT_LITERAL,
	SH_FLOAT_LITERAL,
	SH_STRING_LITERAL,
	SH_ARRAY_LITERAL,
	SH_STRUCT_LITERAL,
	SH_NIL_LITERAL,
	SH_FIELD_ASSIGNMENT_EXPR,
	SH_ID_EXPR, // nothing must remain as ID? 
	SH_VAR_EXPR,

	SH_PTR_DEREF_EXPR,

	SH_OPERATOR_EXPR,
	SH_QUESTION_OPERATOR_EXPR,
	SH_PARENT_EXPR,

	SH_INC_EXPR,
	SH_POST_INC_EXPR,
	SH_DEC_EXPR,
	SH_POST_DEC_EXPR,
	SH_FIELD_ACCESS_EXPR,
	SH_ADDRESS_OF_EXPR,
	SH_ARRAY_EXPR,
	SH_FUNC_EXPR,

	SH_SIZEOF_BYTE_EXPR,
	SH_SIZEOF_BIT_EXPR,

	SH_PEEK_EXPR,
	SH_END_EXPR_TYPE,


} sh_expression_type ;


char* expr_type_names[SH_END_EXPR_TYPE] = {
	[ SH_UNKNOWN_EXPR ] = "unknown",
	[ SH_INT_LITERAL ] = "integer",
	[ SH_FLOAT_LITERAL ] = "float",
	[ SH_STRING_LITERAL ] = "string",
	[ SH_ARRAY_LITERAL ] = "array",
	[ SH_STRUCT_LITERAL ] = "struct",
	[ SH_NIL_LITERAL ] = "nil",
	[ SH_FIELD_ASSIGNMENT_EXPR ] = "field assignment",
	[ SH_ID_EXPR ] = "ID expr", // nothing must remain as ID? 
	[ SH_VAR_EXPR ] = "var expr",

	[ SH_PTR_DEREF_EXPR ] = "ptr deref",
	[ SH_OPERATOR_EXPR ] = "operator expr",

	[ SH_INC_EXPR ] = "prefix inc",
	[ SH_POST_INC_EXPR ] = "suffix inc",
	[ SH_DEC_EXPR ] = "prefix dec",
	[ SH_POST_DEC_EXPR ] = "postfix dec",
	[ SH_FIELD_ACCESS_EXPR ] = "field access",
	[ SH_ADDRESS_OF_EXPR ] = "address of",
	[ SH_ARRAY_EXPR ] = "array expr",
	[ SH_FUNC_EXPR ] = "func expr",
	[ SH_SIZEOF_BYTE_EXPR ] = "sizeof_byte",
	[ SH_SIZEOF_BIT_EXPR ] = "sizeof_bit",
} ;





typedef struct sh_expression {
	sh_expression_type type;
	char* name; // varname
	i32 name_len;

	union {

		struct { //int literal
			union {
				i8  vi8;
				i16 vi16;
				i32 vi32;
				i64 vi64;

				u8 	vu8;
				u16 vu16;
				u32 vu32;
				u64 vu64;

				f32 vf32;
				f64 vf64;
			};
		};

		sh_decl *var_decl;

		// struct { 
		// };



		struct { //string literal
			char *str_val;
			i32 str_size;
		};

		struct {
			sh_expression *operand;
			sh_expression *peek_type;
		};

		struct { 
			sh_expr_operator op;
			sh_expression *left_op;
			sh_expression *right_op;
		};

		struct { // function
			sh_expression *func_expr;
			sh_expression **args;
		};


		//array
		struct {
			sh_expression **values;
			sh_expression **fields;
		};

		struct { 
			sh_expression *array_expr;
			sh_expression *array_index_expr;
		};

		struct { // field assignment expr
			sh_expression* left;
			sh_expression* right;
		};

		struct { 
			sh_token_base_type access_type; // either -> or .
			sh_expression *pointer_expr;
			sh_expression *field_access;
		};

	};

} sh_expression ;

i8 is_expr_arithmatic(sh_expression *e) {
	switch(e->op) {
		case SH_PLUS:
		case SH_MINUS:
		case SH_DIV:
		case SH_ASTERISK:
			return 1;
		default: return 0;
	}
}

i8 is_expr_and_or(sh_expression *e) {
	switch(e->op) {
		case SH_AND:
		case SH_OR:
			return 1;
		default: return 0;
	}
}

char* get_expr_type_string(sh_expression *expr) {
	return expr_type_names[expr->type];
}

bool is_type(sh_token *token) {
	for(sh_type **types = type_table;  types != buf_end(type_table); types++ ) {
		sh_type *i = *types;
		if(token->name_len == i->name_len && strncmp(token->name, i->name, i->name_len) == 0) {
			return 1;
		}
	}
	return 0;
}

bool is_keyword(sh_keyword *keyword) {
	if(current_token->name_len == keyword->name_len &&
	   strncmp(current_token->name, keyword->name, keyword->name_len) == 0)
	{
		return 1;
	}

	return 0;
}


bool expect_keyword(sh_keyword *keyword) {
	if(is_keyword(keyword)) { 
		sh_next_token();
		return 1;
	}

	return 0;
}


i32 sh_find_struct_field_index(sh_type *type, char *name, i32 name_len) {
	assert_exit(type->is_struct, "Type is not struct");
	sh_decl **fields = type->struct_type.fields;
	for(i32 i = 0; i < buf_len(fields); i++ ) {
		sh_decl *field = fields[i];
		if(field->name_len == name_len && strncmp(field->name, name, name_len) == 0 ) {
			return i;
		}
	}

	return -1;
}

sh_typespec* sh_get_when_typespec(sh_statement *w_stmt, sh_decl_val value) {
	assert_exit(w_stmt->type == SH_WHEN_STATEMENT, "Type is not when statement");

	when_stmt *w = w_stmt->when_types;

	for(i32 i = 0; i < buf_len(w); i++ ) {

		if(w[i].type_id.type.base == SH_INT ) {

			i32 ival = parse_int_token(&w[i].type_id);
			if(ival == value.ival) { return w[i].type; }

		} else if(w[i].type_id.type.base == SH_STRING) {
			char *str_val = w[i].type_id.name;

			if(is_typespec_array(value.type)) {
				i8 same_len = w[i].type_id.name_len == value.array_size;

				if(same_len) {
					char *temp_buf = (char*)calloc(value.array_size + 1, sizeof(char));
					for(i32 i = 0; i < value.array_size; i++) {
						temp_buf[i] = value.array[i]->ch;
					}

					if(strncmp(str_val, temp_buf, value.array_size)  == 0) {
						return w[i].type;
						break;
					} 
				}


			} else {
				i8 same_len = w[i].type_id.name_len == value.str_len;
				if(same_len && strncmp(str_val, value.str, value.str_len) == 0) {
					return w[i].type;
				}
				break;
			}

		}

	}

	return NULL;
}

sh_decl* sh_get_enum_field(i64 value, sh_type *enum_type) {
	for(i32 i = 0; i < buf_len(enum_type->enum_type.fields); i++) {
		sh_decl *enum_field = enum_type->enum_type.fields[i];
		if(value == enum_field->vi64)  return enum_field;
	}
	return NULL;
}


i32 sh_get_struct_field_offset(sh_type *type, char *name, i32 name_len) {
	assert_exit(type->is_struct, "Type is not struct");
	sh_decl **fields = type->struct_type.fields;
	for(i32 i = 0; i < buf_len(fields); i++ ) {
		sh_decl *field = fields[i];
		if(field->name_len == name_len && strncmp(field->name, name, name_len) == 0 ) {
			return field->struct_field.offset;
		}
	}

	return -1;
}

sh_decl* sh_get_struct_field_name(sh_type *type, char *name, i32 name_len) {
	assert_exit(type->is_struct, "Type is not struct");
	sh_decl **fields = type->struct_type.fields;
	for(i32 i = 0; i < buf_len(fields); i++ ) {
		sh_decl *field = fields[i];
		if(field->name_len == name_len && strncmp(field->name, name, name_len) == 0 ) {
			return field;
		}
	}

	return NULL;
}

f64 parse_float_token(sh_token *tok) {
	f64 val = 0;

	i32 sep_index = 0;
	for(i32 i = 0; i < tok->name_len; i++) {
		if(tok->name[i] == '.') {
			sep_index = i;
			break;
		}
	}

	i32 power_up = 1;
	for(i32 i = sep_index+1; i < tok->name_len; i++) {
		val += (tok->name[i] - '0')*pow(10, -power_up++ );
	}

	power_up = 0;
	for(i32 i = sep_index - 1; i >= 0; i--) {
		val += (tok->name[i] - '0')*pow(10, power_up++ );
	}

	return val;
}


i32 parse_int_token(sh_token *tok) {

	i32 val = 0;
	for(i32 i = tok->name_len - 1; i >= 0; i--) {
		val += (i32 )((tok->name[i] - '0')*pow(10, tok->name_len - i - 1 ));
	}

	return val;
}

sh_expression* sh_new_int_literal_expr(i64 val) { 
	sh_expression *new_expr = (sh_expression *)calloc(1, sizeof(sh_expression));
	new_expr->type = SH_INT_LITERAL;
	new_expr->vi64 = val;
	return new_expr;
}

sh_expression* sh_expr_int_literal() {
	sh_expression *new_expr = (sh_expression *)calloc(1, sizeof(sh_expression));
	new_expr->type = SH_INT_LITERAL;
	new_expr->vi64 = parse_int_token(current_token);
	return new_expr;
}

sh_expression* sh_expr_float_literal() {
	sh_expression *new_expr = (sh_expression *)calloc(1, sizeof(sh_expression));
	new_expr->type = SH_FLOAT_LITERAL;
	new_expr->vf64 = parse_float_token(current_token);
	return new_expr;
}


sh_expression* sh_expr_string_literal() {
	sh_expression *new_expr = (sh_expression *)calloc(1, sizeof(sh_expression));
	new_expr->type = SH_STRING_LITERAL;
	new_expr->str_val = current_token->name;
	new_expr->str_size = current_token->name_len;
	return new_expr;
}


sh_expression* sh_new_id_expr(sh_token *tok) {
	sh_expression *new_expr = (sh_expression *)calloc(1, sizeof(sh_expression));
	new_expr->type = SH_ID_EXPR; // maybe we need the sh_decl too
	new_expr->name = tok->name;
	new_expr->name_len = tok->name_len;
	return new_expr;
}


sh_expression* sh_new_nil_literal_expr(void) {
	sh_expression *new_expr = (sh_expression *)calloc(1, sizeof(sh_expression));
	new_expr->type = SH_NIL_LITERAL; // maybe we need the sh_decl too
	new_expr->name = nil_keyword.name;
	new_expr->name_len = nil_keyword.name_len;
	return new_expr;
}


sh_expression* sh_new_expr(sh_expression_type type) {
	sh_expression *new_expr = (sh_expression *)calloc(1, sizeof(sh_expression));
	new_expr->type = type;
	return new_expr;
}

sh_expression* sh_new_unary_expr(sh_expression_type type, sh_expression *unary_expr) {
	sh_expression *new_expr = (sh_expression *)calloc(1, sizeof(sh_expression));
	new_expr->type = type; // maybe we need the sh_decl too
	new_expr->operand = unary_expr;
	return new_expr;
}




sh_expression* sh_new_array_index_expr(sh_expression *array_expression, sh_expression *array_index) {
	sh_expression *new_expr = (sh_expression *)calloc(1, sizeof(sh_expression));
	new_expr->type = SH_ARRAY_EXPR; // maybe we need the sh_decl too
	new_expr->array_expr = array_expression;
	new_expr->array_index_expr = array_index;
	return new_expr;
}

sh_expression* sh_new_field_access_expr(sh_expression *expr, sh_expression *var_expr) {

	sh_expression *new_expr = (sh_expression *)calloc(1, sizeof(sh_expression));
	new_expr->type = SH_FIELD_ACCESS_EXPR; // maybe we need the sh_decl too
	new_expr->access_type = current_token->type.base;
	new_expr->pointer_expr = expr;
	new_expr->field_access = var_expr;
	return new_expr;
}

sh_expression* sh_new_op_expr(sh_expression *left, sh_expression* right, sh_expr_operator op) {
	sh_expression *new_add = (sh_expression *) calloc(1, sizeof(sh_expression));
	new_add->type = SH_OPERATOR_EXPR;
	new_add->op = op;
	new_add->left_op = left;
	new_add->right_op = right;
	return new_add;
}

sh_expression* sh_new_question_operator_expr() {
	sh_expression *new_expr = (sh_expression *)calloc(1, sizeof(sh_expression));
	new_expr->type = SH_QUESTION_OPERATOR_EXPR;
	return new_expr;
}

sh_expression* sh_new_parent_expr() {
	sh_expression *new_expr = (sh_expression *)calloc(1, sizeof(sh_expression));
	new_expr->type = SH_PARENT_EXPR;
	return new_expr;
}


sh_decl* sh_get_decl(char *name, i32 name_len) {
	for(sh_decl **decl = decls; decl != buf_end(decls); decl++) {
		sh_decl *t = decl[0];
		if( t->name_len == name_len && strncmp(t->name, name, name_len) == 0)  {
			return t;
		}
	}

	return NULL;
}

sh_decl* sh_new_decl(sh_decl_type type) {
	sh_decl *decl = (sh_decl*) calloc(1, sizeof(sh_decl));
	decl->type = type;
	return decl;
}

sh_decl_tag* sh_new_decl_tag(sh_tag_type tag_type) {
	sh_decl_tag *tag = (sh_decl_tag*) calloc(1, sizeof(sh_decl_tag));
	tag->type = tag_type;
	return tag;
}

sh_typespec* sh_new_base_typespec(sh_type_kind type, sh_type *base_type) {
	sh_typespec *new_typespec = (sh_typespec*) calloc(1, sizeof(sh_typespec));
	new_typespec->type = type;
	new_typespec->base_type = base_type;
	new_typespec->size_byte = base_type->size_byte;
	new_typespec->size_bit = base_type->size_bit;
	return new_typespec;
}

sh_typespec* sh_new_typespec(sh_type_kind type, sh_typespec *base) {
	sh_typespec *new_typespec = (sh_typespec*) calloc(1, sizeof(sh_typespec));
	new_typespec->type = type;
	new_typespec->base = base;
	return new_typespec;

}

sh_expression* sh_new_function_call_expr(sh_expression *func_expr, sh_expression** args) {
	sh_expression *new_expr = (sh_expression *)calloc(1, sizeof(sh_expression));

	new_expr->type = SH_FUNC_EXPR;
	new_expr->func_expr = func_expr;
	new_expr->args = args;

	return new_expr;
}


sh_expression** sh_parse_function_params(void) {

	assert(expect_token('('));
	if(is_token(')')) {
		return NULL;
	}

	sh_expression **args = NULL;

	sh_expression *first_arg = sh_parse_expression();

	buf_push(args, first_arg);

	while(expect_token(',')) {
		buf_push(args, (sh_parse_expression()));
	}

	return args;
}

sh_expression* sh_array_index_expr(void) {

	assert(expect_token('['));
	if(is_token(']')) {
		return NULL;
	}

	sh_expression *expr = sh_parse_expression();

	return expr;
}


sh_expression* sh_parse_ptr_deref(void) {
	assert(expect_token(SH_ASTERISK));
	sh_expression *new_expr = sh_new_unary_expr(SH_PTR_DEREF_EXPR, sh_parse_unary_expr());
	return new_expr;
}

sh_expression* sh_parse_address_of_expr(void) {
	assert(expect_token(SH_AND_OPERATOR));
	sh_expression *new_expr = sh_new_unary_expr(SH_ADDRESS_OF_EXPR, sh_parse_base_expr());
	return new_expr;
}


sh_expression* sh_parse_field_name(void) {
	assert_exit(is_token(SH_IDENTIFIER) || is_token(SH_AT), "Expected field name got %s", current_token->name);
	sh_expression *new_expr = NULL;
	new_expr = sh_new_id_expr(current_token);
	sh_next_token();
	return new_expr;
}

sh_expression* sh_parse_id_expr(void) {
	assert(is_token(SH_IDENTIFIER));

	sh_expression *new_expr = NULL;


	if(is_keyword(&nil_keyword)) {
		new_expr =  sh_new_nil_literal_expr();
	} else {
		new_expr = sh_new_id_expr(current_token);
		sh_decl *decl = sh_get_decl(current_token->name, current_token->name_len);
		// assert_exit(decl != NULL, "Cannot find identifier %s\n", current_token->name);
		new_expr->var_decl = decl;
	}

	
	sh_next_token();

	return new_expr;
}


sh_expression* sh_parse_skip_expr(void) {
	assert_exit(expect_token(SH_IDENTIFIER), "expected skip keyword got %s\n", current_token->name);

	assert_exit(expect_token('('), "expected '(' got %s\n",  current_token->name);
	sh_expression *skip_param = sh_parse_expression();
	assert_exit(expect_token(')'), "expected ')' got %s\n", current_token->name);

	assert_exit(expect_token(';'), "expected ';' got %s\n", current_token->name);

	return skip_param;
}

sh_expression* sh_parse_rewind_expr(void) {
	assert_exit(expect_token(SH_IDENTIFIER), "expected skip keyword got %s\n", current_token->name);

	assert_exit(expect_token('('), "expected '(' got %s\n",  current_token->name);
	sh_expression *skip_param = sh_parse_expression();
	assert_exit(expect_token(')'), "expected ')' got %s\n", current_token->name);
	assert_exit(expect_token(';'), "expected ';' got %s\n", current_token->name);

	return skip_param;
}




sh_expression* sh_new_sizeof_expr(sh_expression *sizeof_expr, sh_expression_type sizeof_type) {
	sh_expression *size_of = sh_new_expr(sizeof_type);
	size_of->operand = sizeof_expr;
	return size_of;
}

sh_expression* sh_new_peek_expr(sh_expression *peek_type) {
	sh_expression *peek = sh_new_expr(SH_PEEK_EXPR);
	peek->peek_type = peek_type;
	return peek;
}

sh_expression* sh_parse_peek_expr(void) {
	assert_exit(expect_keyword(&peek_keyword), "expected peek keyword got %s\n", current_token->name);

	assert_exit(expect_token('('), "expected '(' got %s\n",  current_token->name);
	sh_expression *peek_expr = sh_parse_expression();
	assert_exit(expect_token(')'), "expected ')' got %s\n", current_token->name);
	assert_exit(expect_token(';'), "expected ';' got %s\n", current_token->name);

	return peek_expr;
}


sh_expression* sh_parse_assignment_expr(void) {
	assert_exit(
			is_token(SH_IDENTIFIER),
			"Left side of assignment must be a field name got %s",
			current_token->name
	);

	sh_expression *assignment_expr = sh_new_expr(SH_FIELD_ASSIGNMENT_EXPR);
	assignment_expr->left = sh_parse_field_name();

	assert_exit(expect_token(SH_ASSIGNMENT), "Expected '=' got %s\n", current_token->name);

	assignment_expr->right = sh_parse_expression();

	return assignment_expr;
}


sh_expression* sh_parse_array_literal(void) {
	assert_exit(expect_token(SH_OPEN_BRACKET), "Expected '[' got %s.", current_token->name);

	assert_exit(is_token(SH_CLOSE_BRACKET) == 0, "Cannot have empty array literal" );
	sh_expression *array_literal = sh_new_expr(SH_ARRAY_LITERAL);

	do {
		buf_push(array_literal->values, sh_parse_expression());
	} while(expect_token(SH_COMMA));


	assert_exit(expect_token(SH_CLOSE_BRACKET), "Expected '}' got %s\n", current_token->name);

	return array_literal;
}

sh_expression* sh_parse_struct_literal(void) {
	assert_exit(expect_token(SH_OPEN_BRACE), "Expected '{' got %s.", current_token->name);

	assert_exit(is_token(SH_CLOSE_BRACE) == 0, "Cannot have empty struct literal" );
	sh_expression *struct_literal = sh_new_expr(SH_STRUCT_LITERAL);

	do {
		if(current_token[1].type.base == '=') {  // assignment statement/field assignment expr
			buf_push(struct_literal->fields, sh_parse_assignment_expr());
		} else {
			buf_push(struct_literal->fields, sh_parse_expression());
			sh_expression* blen = struct_literal->fields[buf_len(struct_literal->fields)-1];

			if(is_token(SH_ASSIGNMENT)) {
				assert_exit(false, "Cannot use expression as field name %s\n", sh_print_expr(blen));
			}

		}

	} while(expect_token(SH_COMMA));


	assert_exit(expect_token(SH_CLOSE_BRACE), "Expected '}' got %s\n", current_token->name);

	return struct_literal;
}

sh_expression* sh_parse_increment() {
	assert(expect_token(SH_INCREMENT));
	sh_expression *expr = sh_new_unary_expr(SH_INC_EXPR, sh_parse_base_expr());
	return expr;
}

sh_expression* sh_parse_decrement() {
	assert(expect_token(SH_DECREMENT));
	sh_expression *expr = sh_new_unary_expr(SH_DEC_EXPR, sh_parse_base_expr());
	return expr;
}

sh_typespec* parse_type() {

	assert(is_type(current_token));

	sh_typespec *type = sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, sh_get_type(current_token));

	sh_next_token();

	while(is_token(SH_OPEN_BRACKET) || is_token(SH_ASTERISK)) {

		switch(current_token->type.base) {
			case SH_OPEN_BRACKET: {
				sh_next_token();
				type = sh_new_typespec(SH_TYPE_ARRAY, type);
				if(!is_token(SH_CLOSE_BRACKET)) {
					//@Todo: this size should be constant
					type->array_size_expr = sh_parse_expression();
					type->array_count = type->array_size_expr->type == SH_INT_LITERAL ? type->array_size_expr->vi32 : 0;
					type->size_byte = type->base->size_byte*type->array_count;
					type->size_bit =  8*type->size_byte;
					// sh_next_token();
				} else {
					type->array_size_expr = NULL;
				}

				assert_exit(expect_token(SH_CLOSE_BRACKET), "Expected ']' but got %s\n", current_token->name);
			} break;

			case SH_ASTERISK: {
				type = sh_new_typespec(SH_TYPE_PTR, type);
				type->size_byte = 8;
				type->size_bit = 8*8;
				sh_next_token();
			} break;

			default: {
				assert_exit(false, "Wrong type construct");
			} break;
		}
	}

	return type;
}


sh_decl** sh_parse_func_arg_list(void) {

	if(is_token(')')) {
		return NULL;
	}

	if(!is_type(current_token)) {
		printf("expected a type, got non-type token '%s'\n", current_token->name);
		exit(1);
	}

	sh_decl **args = NULL;

	do {
		sh_decl *v = sh_parse_var_decl(SH_LOCAL_STORAGE);
		buf_push(args, v);
		buf_push(decls, v);
	} while(expect_token(SH_COMMA));

	return args;
}


sh_decl* sh_parse_var_decl(sh_storage_type storage) {
	assert(is_token(SH_IDENTIFIER));

	sh_decl *var_decl = sh_new_decl(SH_VAR_DECL);
	var_decl->var.type = parse_type();
	var_decl->storage = storage;

	var_decl->total_size = var_decl->var.type->size_byte;

	assert(is_token(SH_IDENTIFIER));

	var_decl->name = current_token->name;
	var_decl->name_len = current_token->name_len;
	sh_next_token();

	//@Note/Todo: parser can probably infer the size of the array from the init_expr if 
	//array size expr is not given 
	if(expect_token('=')) {
		var_decl->var.init_expr = sh_parse_expression();
	}

	if(expect_token('@')) {
		sh_decl_tag *tag = sh_parse_decl_tag();
		buf_push(var_decl->tags, tag);
	}

	return var_decl;
}


sh_decl* sh_parse_func_decl() {
	sh_decl *decl = sh_new_decl(SH_FUNC_DECL);

	decl->func.return_type = parse_type();
	decl->name = current_token->name;
	decl->name_len = current_token->name_len;

	
	sh_next_token();

	assert(expect_token('('));

	decl->func.args = sh_parse_func_arg_list();

	assert(expect_token(')'));

	i32 has_body = is_token('{');

	
	if(has_body) {
		has_func_call = 0;
		decl->func.compound_statement = sh_parse_compound_statement();// is there a func call here? 
		decl->total_size = decl->func.compound_statement->stmt_size;
		if(has_func_call == 1) {
			decl->total_size += 32 + 8 ; //maybe? 
		}
	} else {
		assert_exit(expect_token(SH_SEMI_COLON), "expected ; got %s\n", current_token->name);
	}


	return decl;
}


sh_decl* sh_parse_typedef_decl() {
	assert(expect_keyword(&typedef_keyword));

	sh_decl *decl = sh_new_decl(SH_TYPEDEF_DECL);

	decl->typedef_decl.base = parse_type();

	decl->name = current_token->name;

	{ 
		sh_type *new_type = (sh_type *)calloc(1, sizeof(sh_type));
		new_type->name = current_token->name;
		new_type->name_len = current_token->name_len;
		new_type->alias.aliased_type = decl->typedef_decl.base;
		new_type->is_alias = 1;
		new_type->is_ptr = decl->typedef_decl.base->type == SH_TYPE_PTR;
		buf_push(type_table, new_type);
	}

	sh_next_token();
	assert(expect_token(';'));

	return decl;
}

sh_typespec* sh_parse_when_statement_typespec(void) {

	sh_statement *w_stmt = (sh_statement *)calloc(1, sizeof(sh_statement));
	w_stmt->when_expr = sh_parse_expression();
	w_stmt->type = SH_WHEN_STATEMENT;


	assert_exit(expect_token(SH_OPEN_BRACE), "Expected { got %s", current_token->name);

	while(!is_token(SH_CLOSE_BRACE)) {
		//@TODO: this need to be more inclusive and cover all the types
		// assert_exit(false, "Expected number or str got %s\n", current_token->name); // 

		if(expect_keyword(&else_keyword)) {
			assert_exit(expect_token(SH_POINTER_ACCESS), "Expected -> got %s", current_token->name);
			sh_typespec *s = parse_type();
			w_stmt->else_type = s;
			assert_exit(expect_token(SH_COMMA) || is_token(SH_CLOSE_BRACE), "Expected ,  got %s", current_token->name);
			continue;
		} 

		sh_token val = *current_token;
		sh_next_token();
		assert_exit(expect_token(SH_POINTER_ACCESS), "Expected -> got %s", current_token->name);
		sh_typespec *s = parse_type();
		assert_exit(expect_token(SH_COMMA) || is_token(SH_CLOSE_BRACE), "Expected ,  got %s", current_token->name);
		buf_push(w_stmt->when_types, (when_stmt){ val, s });
	}

	assert_exit(expect_token(SH_CLOSE_BRACE), "Expected } got %s", current_token->name);


	sh_typespec *ty = sh_new_typespec(SH_TYPE_WHEN, NULL);

	ty->when = w_stmt;

	return ty;
}

sh_decl* sh_parse_struct_field_decl() {
	sh_decl *new_decl = sh_new_decl(SH_STRUCT_FIELD_DECL);
	new_decl->storage = SH_LOCAL_STORAGE;

	if(expect_keyword(&when_keyword)) {
		new_decl->struct_field.type = sh_parse_when_statement_typespec();
	} else {
		assert_exit(is_type(current_token), "Excected a type got %s\n", current_token->name);
		new_decl->struct_field.type = parse_type();
	}

	new_decl->name = current_token->name;
	new_decl->name_len = current_token->name_len;

	sh_next_token();

    if(expect_token(SH_COLON)) {
        i32 bit_field_size = parse_int_token(current_token);
        sh_typespec *bit_field = sh_new_typespec(SH_TYPE_BIT_FIELD, new_decl->struct_field.type);
        bit_field->size_bit = bit_field_size;
        new_decl->struct_field.type = bit_field;
        sh_next_token();
    }

	while(expect_token('@')) {
		sh_decl_tag *tag = sh_parse_decl_tag();
		buf_push(new_decl->tags, tag);
	}

	return new_decl;
}

sh_decl* sh_parse_struct_decl() {
	assert(expect_keyword(&struct_keyword));

	sh_decl* decl = sh_new_decl(SH_STRUCT_DECL);

	decl->name = current_token->name;
	decl->name_len = current_token->name_len;

	sh_type *new_type = (sh_type*) calloc(1, sizeof(sh_type));
	new_type->is_struct = 1;
	new_type->name = current_token->name;
	new_type->name_len = current_token->name_len;

	sh_next_token();
	assert(expect_token('{'));
	i32 field_offset = 0;
	while(!is_token('}')) {
		sh_decl *field = sh_parse_struct_field_decl();

        if(field->type.type)

		field->struct_field.offset = field_offset;
		field_offset += field->struct_field.type->size_byte; // offset and size are the same? 

		buf_push(decl->struct_decl.fields, field);
		assert_exit(expect_token(';'), "expected ; got %s\n", current_token->name);
	}
	
	new_type->struct_type.fields = decl->struct_decl.fields;
	decl->total_size = field_offset;
	new_type->size_byte = field_offset;
	new_type->size_bit = 8*field_offset;
	buf_push(type_table, new_type);
	assert(expect_token('}'));
	/* assert(expect_token(';')); */

	return decl;
}

i64 field_counter = 0;
i64 biggest_number = 0;
sh_decl *sh_parse_enum_field() {
	sh_decl *field = sh_new_decl(SH_ENUM_FIELD_DECL);

	assert_exit(is_token(SH_IDENTIFIER), "expected identifier got %s", current_token->name);

	field->name = current_token->name;
	field->name_len = current_token->name_len;
	sh_next_token();

	if(expect_token(SH_ASSIGNMENT)) {
		if(is_token(SH_IDENTIFIER)) {
			
			sh_next_token();
		} else {
			assert_exit(is_token(SH_INT), "expected a interger literal got %s\n", current_token->name);
			field_counter = parse_int_token(current_token);
			sh_next_token();
		}
	}

	field->vu64 = field_counter;

	return field;
}

sh_decl* sh_parse_enum_decl() {
	assert(expect_keyword(&enum_keyword));
	sh_decl* decl = sh_new_decl(SH_ENUM_DECL);

	sh_typespec *base_type = NULL;
	if(is_keyword(&pack_keyword)) {
		sh_next_token();
	} else if(is_type(current_token)) {
		 base_type = parse_type();
	} else {
		base_type = sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_i32);
	}


	assert_exit(is_token(SH_IDENTIFIER), "Expected identifier got %s\n", current_token->name);

	decl->name = current_token->name;
	decl->name_len = current_token->name_len;

	sh_next_token();

	assert_exit(expect_token(SH_OPEN_BRACE), "Expected '{' got %s\n", current_token->name);


	field_counter = 0;
	biggest_number = 0;
	while(!is_token(SH_CLOSE_PARAN)) {
		sh_decl *enum_field = sh_parse_enum_field();
		buf_push(decl->enum_decl.enum_fields, enum_field);

		if(field_counter > biggest_number) biggest_number = field_counter;

		if(is_token(SH_CLOSE_BRACE)) {
			break;
		}  else {
			assert_exit(expect_token(SH_COMMA), "Expected , got %s\n", current_token->name);
		}


		field_counter++;
	}

	assert_exit(expect_token(SH_CLOSE_BRACE), "Expected '}' got %s\n", current_token->name);


	if(base_type == NULL) {
		i32 bit_size = (i32)log2((double)biggest_number) + 1;
		sh_expression *array_size_expr = sh_new_int_literal_expr(bit_size);
		base_type = sh_new_typespec(SH_TYPE_ARRAY, sh_new_base_typespec(SH_TYPE_DEFINED_TYPE, &sh_type_tib));
		base_type->array_size_expr = array_size_expr;
		base_type->array_count = bit_size;
	}


	{ 
		sh_type *new_type = (sh_type *)calloc(1, sizeof(sh_type));
		new_type->name = decl->name;
		new_type->name_len = decl->name_len;
		new_type->enum_type.base_type = base_type;
		new_type->enum_type.fields = decl->enum_decl.enum_fields;
		new_type->is_enum = 1;
		buf_push(type_table, new_type);
	}



	return decl;
}


sh_decl_tag *sh_parse_dll_import() {

	sh_decl_tag *tag = sh_new_decl_tag(SH_DLL_IMPORT_TAG);

	assert_exit(expect_token(SH_OPEN_PARAN), "Expected ( got %s\n", current_token->name);

	sh_token **dll_name = NULL;

	i32 len = current_token->name_len;
	while(!expect_token(SH_COMMA)) {
		len += current_token->name_len;
		buf_push(dll_name, current_token);
		sh_next_token();
	}

	char *name = (char*)calloc(len+1, sizeof(char));

	for(int i = 0; i < buf_len(dll_name); i++) {
		strncat(name, dll_name[i]->name, dll_name[i]->name_len);
	}

	buf_free(dll_name);


	char *func_name = current_token->name;

	//@Todo: compute file path? maybe? current dir if nothing is attached etc
	tag->dll.dll_file_path = name;
	tag->dll.func_name = func_name;

	sh_next_token();
	assert_exit(expect_token(SH_CLOSE_PARAN), "Expected ) got %s\n", current_token->name );

	return tag;
}

sh_decl_tag *sh_parse_var_import() {

	sh_decl_tag *tag = sh_new_decl_tag(SH_VAR_IMPORT_TAG);

	assert_exit(expect_token(SH_OPEN_PARAN), "Expected ( got %s\n", current_token->name);

	tag->var.var_name = current_token->name;
	assert_exit(expect_token(SH_IDENTIFIER), "expected identifier got %s\n", current_token->name);
	
	assert_exit(expect_token(SH_CLOSE_PARAN), "Expected ) got %s\n", current_token->name );

	return tag;
}


#define EK(key) expect_keyword(&key##_keyword)

sh_decl_tag* sh_parse_decl_tag() {
	sh_decl_tag *tag = NULL;

	if(EK( dll_import ))        { tag = sh_parse_dll_import(); }
	else if(EK( var_import ))   { tag = sh_parse_var_import(); }
	else if(EK( assert ))       { tag = sh_parse_assert_tag(); }
	else if(EK( cond ))         { tag = sh_parse_cond_tag()  ; }
	else if(EK( print ))        { tag = sh_parse_print_tag() ; }
	else if(EK( array_print ))  { tag = sh_parse_array_print_tag() ; }
	else if(EK( struct_print )) { tag = sh_parse_struct_print_tag() ; }
	else if(EK( add_on ))       { tag = sh_parse_add_on_tag() ; }
	else if(EK( offset ))       { tag = sh_parse_offset_tag() ; }
	else {
		assert_exit(false, "tag (@%s) not handled", current_token->name);
	}

	return tag;
}

#undef EK

void parse_file() {

	while(current_token->type.base != SH_END_FILE) {

		sh_statement *stmt = sh_parse_statement();


		if(stmt != NULL)
			buf_push(stmts, stmt);
#if 0

		sh_decl* decl = NULL;
		sh_decl_tag *tag = NULL;

		sh_token *token_start = current_token;

		if(expect_token(SH_AT)) {
			tag = sh_parse_decl_tag();
			token_start = current_token;
		}

		
		if(is_type(current_token)) {

			decl = sh_parse_var_decl(SH_GLOBAL_STORAGE);

			if(expect_token(SH_OPEN_PARAN)) {
				current_token = token_start;
				decl = sh_parse_func_decl();
				decl->storage = SH_GLOBAL_STORAGE;
			} else {
				assert(expect_token(';'));
			}

		} else if(is_keyword(&typedef_keyword)) {
			decl = sh_parse_typedef_decl();
		} else if(is_keyword(&struct_keyword)) {
			decl = sh_parse_struct_decl();
		} else if(expect_token(SH_COMMENT)) {
			continue;
		} else {
			assert_exit(false, "unexpected token %s\n", current_token->name);
		}

		if(tag) {
			buf_push(decl->tags, tag);
		}

		buf_push(decls, decl);
#endif

	}
}


char* sh_print_typespec(sh_typespec *type) {

	i32 at  = 0;
	char *buffer = (char *)calloc(1, sizeof(char)*1024);

	switch(type->type) {

		case SH_TYPE_ARRAY: {
			at += copy_and_free(buffer + at, sh_print_typespec(type->base));
			at += sprintf(buffer + at, "[");
			if(type->array_size_expr) {
				at += copy_and_free(buffer + at, sh_print_expr(type->array_size_expr));
			}
			at += sprintf(buffer + at, "]");
		} break;

		case SH_TYPE_PTR: {
			at += copy_and_free(buffer + at, sh_print_typespec(type->base));
			at += sprintf(buffer + at, "*");
		} break;

		case SH_TYPE_DEFINED_TYPE: { 
			at += sprintf(buffer + at, "%s", type->base_type->name);

			if(type->base_type->is_alias) {
				at += sprintf(buffer + at, " => ");
				at += copy_and_free(
						buffer + at,
						sh_print_typespec(type->base_type->alias.aliased_type)
				);
			}
		} break;
	}


	return buffer;
}


char* sh_print_var_decl(sh_decl *decl) {
	char *buffer = (char*) calloc(1, sizeof(char)*1024);
	i32 at  = 0;


	at += sprintf(buffer+at, "%s: ", decl->name);
	at += copy_and_free(buffer+at, sh_print_typespec(decl->var.type));

	if(decl->var.init_expr) {
		at += sprintf(buffer+at, " = ");
		at += copy_and_free(buffer + at, sh_print_expr(decl->var.init_expr));
	}

	// at += sprintf(buffer+at, );

	return buffer;
}

char* sh_print_func_decl(sh_decl *decl) {
	char *buffer = (char*) calloc(1, sizeof(char)*1024);
	i32 at  = 0;


	at += sprintf(buffer+at, "func: %s, ret: ", decl->name);
	at += copy_and_free(buffer+at, sh_print_typespec(decl->func.return_type));
	at += sprintf(buffer+at, "\n");
	if(decl->func.args) {
		at += sprintf(buffer+at, "\t");

		for(sh_decl **i = decl->func.args; i < buf_end(decl->func.args); i++) {
			at += copy_and_free(buffer + at, sh_print_var_decl(i[0]));
			at += sprintf(buffer+at, "\t");
		}
	}

	if(decl->func.compound_statement) {
		sh_print_statement(decl->func.compound_statement);
	}

	return buffer;

}

char* sh_print_typedef_decl(sh_decl *decl) {
	char *buffer = (char*) calloc(1, sizeof(char)*1024);
	i32 at  = 0;

	at += sprintf(buffer+at, "aliasing ");
	at += copy_and_free(buffer + at, sh_print_typespec(decl->typedef_decl.base));
	at += sprintf(buffer+at, " => %s\n", decl->name);

	return buffer;
}

char* sh_print_struct_decl(sh_decl *decl) {

	char *buffer = (char*) calloc(1, sizeof(char)*1024);
	i32 at  = 0;

	at += sprintf(buffer + at, "struct: %s\n", decl->name);
	at += sprintf(buffer + at, "\tfields: \n");
	for(sh_decl **i = decl->struct_decl.fields; i != buf_end(decl->struct_decl.fields); i++) {
		at += sprintf(buffer + at ,"\t\t");
		at += copy_and_free(buffer + at, sh_print_decl(i[0]));
	}

	return buffer;
}


char* sh_print_decl(sh_decl *decl) {
	switch(decl->type) {
		case SH_VAR_DECL: { return sh_print_var_decl(decl); } break;
		case SH_FUNC_DECL: { return sh_print_func_decl(decl); } break;
		case SH_TYPEDEF_DECL: { return sh_print_typedef_decl(decl); } break;
		case SH_STRUCT_DECL: { return sh_print_struct_decl(decl); } break;
		default: return "unknown decl";
	}

	return NULL;
}

sh_expression* sh_parse_base() {

	sh_expression* new_expr = NULL;

	if(expect_keyword(&sizeof_bit_keyword)) {
		assert_exit(expect_token('('), "expected '(' got %s", current_token->name);
		new_expr = sh_new_sizeof_expr(sh_parse_expression(), SH_SIZEOF_BIT_EXPR);
		assert_exit(expect_token(')'), "expected ')' got %s", current_token->name);

	} else if(expect_keyword(&sizeof_byte_keyword)) {

		assert_exit(expect_token('('), "expected '(' got %s", current_token->name);
		new_expr = sh_new_sizeof_expr(sh_parse_expression(), SH_SIZEOF_BYTE_EXPR);
		assert_exit(expect_token(')'), "expected ')' got %s", current_token->name);

	} else if(expect_keyword(&peek_keyword)) {
		assert_exit(expect_token('('), "expected '(' got %s", current_token->name);
		new_expr = sh_new_peek_expr(sh_parse_expression());
		assert_exit(expect_token(')'), "expected ')' got %s", current_token->name);
	} else  {

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
				new_expr = sh_parse_expression();
				assert(expect_token(')'));
			} break;

			case SH_OPEN_BRACKET:  {
				new_expr = sh_parse_array_literal();
			} break;

			case SH_OPEN_BRACE:  {
				new_expr = sh_parse_struct_literal();
			} break;

			case SH_FLOAT: {
				new_expr = sh_expr_float_literal();
				sh_next_token();
			} break;

			case SH_COMMENT: {
				printf("what are you doing here? ");
				exit(1);
			} break;

			case SH_QUESTION_OPERATOR: {
				new_expr = sh_new_question_operator_expr();
				sh_next_token();
			} break;

			case SH_DOT_OPERATOR: {
				// skip for next
			} break;
			
			case '@': {
				new_expr = sh_new_parent_expr();
				sh_next_token();
			} break;

			default:  {
				assert_exit(false, "expected a literal or a variable got %s\n", current_token->name);
			} break;
		}

	}

	while(true) {
		switch(current_token->type.base) {
			case '[': {
				new_expr = sh_new_array_index_expr(new_expr, sh_array_index_expr());
				expect_token(']');
			} break;
			case '(': {
				new_expr = sh_new_function_call_expr(new_expr, sh_parse_function_params());
				has_func_call = 1;
				expect_token(')');
			} break;

			case SH_DOT_OPERATOR:
			case SH_POINTER_ACCESS: {
				sh_token_base_type type = current_token->type.base;
				sh_next_token();
				new_expr = sh_new_field_access_expr(new_expr, sh_parse_field_name());
				new_expr->access_type = type;
			} break;
			case SH_INCREMENT: {
				assert(expect_token(SH_INCREMENT));
				new_expr = sh_new_unary_expr(SH_POST_INC_EXPR, new_expr);
			} break;
			case SH_DECREMENT: {
				assert(expect_token(SH_DECREMENT));
				new_expr = sh_new_unary_expr(SH_POST_DEC_EXPR, new_expr);
			} break;

			default: goto break_out; break;
		}
	}

break_out:

	return new_expr;
}

sh_expression* sh_parse_base_expr() {

	
	return sh_parse_base();
}


sh_expression* sh_parse_unary_expr() {

	/* while(current_token->type.base == SH_) */
	switch(current_token->type.base) {
		case SH_ASTERISK: {
			return sh_parse_ptr_deref();
		} break;
		case SH_AND_OPERATOR: {
			return sh_parse_address_of_expr();
		} break;
		case SH_INCREMENT: {
			return sh_parse_increment();
		} break;
		case SH_DECREMENT: {
			return sh_parse_decrement();
		} break;

		default: {
			// we move on
		} break;
	}

	// parsed_base expr

	return sh_parse_base_expr();
}

sh_expression* sh_parse_bitwise_expr() {
	sh_expression* expr = sh_parse_unary_expr();

	return expr;
}

sh_expression* sh_parse_logical_expr() {
	sh_expression* expr = sh_parse_bitwise_expr();

	while(current_token->type.base >= SH_LOGICAL_OP_START && current_token->type.base <= SH_LOGICAL_OP_END) {
		sh_expr_operator op = current_token->type.base;
		sh_next_token();
		expr = sh_new_op_expr(expr, sh_parse_bitwise_expr(), op);
	}

	return expr;
}

sh_expression* sh_parse_multi() {
	sh_expression* expr = sh_parse_logical_expr();

	while(is_token(SH_ASTERISK) || is_token(SH_DIV)) {
		sh_expr_operator op = current_token->type.base;
		sh_next_token();

		expr = sh_new_op_expr(expr, sh_parse_logical_expr(), op);
	}

	return expr;
}



sh_expression* sh_parse_addition() {
	sh_expression* expr = sh_parse_multi();

	while( is_token( SH_PLUS ) ||  is_token(SH_MINUS)) {
		sh_expr_operator op = current_token->type.base;
		sh_next_token();
		expr = sh_new_op_expr(expr, sh_parse_multi(), op);
	}

	return expr;
}


//copy dest to src and free src
i32 copy_and_free(char *dest, char *src) {
	i32 size = (i32)strlen(src);
	strncpy(dest, src, size);
	free(src);
	return size;
}

char* sh_print_expr(sh_expression *expr) {

	char* expr_buffer = (char*)calloc(1, sizeof(char)*1024);
	i32 at = 0;

	assert_exit(expr != NULL, "expr shouldn't be null");

	switch(expr->type) {
		case SH_INT_LITERAL: {
			at += sprintf(expr_buffer + at, "%lld", expr->vi64);
		} break;

		case SH_FLOAT_LITERAL: {
			at += sprintf(expr_buffer + at, "%f", expr->vf64);
		} break;

		case SH_STRING_LITERAL: {
			sprintf(expr_buffer + at, "\"%s\"", expr->str_val);
		} break;

		case SH_ARRAY_LITERAL: {
			at += sprintf(expr_buffer, "[");
			for(i32 i = 0; i < buf_len(expr->values); i++) {
				at += copy_and_free(expr_buffer + at, sh_print_expr(expr->values[i]));
				if(i != buf_len(expr->values) - 1) {
					at += sprintf(expr_buffer+at, ",");
				}
			}

			at += sprintf(expr_buffer+at, "]");
		} break;

		case SH_STRUCT_LITERAL: {

			at += sprintf(expr_buffer, "{");
			for(i32 i = 0; i < buf_len(expr->fields); i++) {

				if(expr->fields[i]->type == SH_FIELD_ASSIGNMENT_EXPR) {
					at += copy_and_free(expr_buffer + at, sh_print_expr(expr->fields[i]->left));
					at += sprintf(expr_buffer+at, " = ");
					at += copy_and_free(expr_buffer + at, sh_print_expr(expr->fields[i]->right));
				} else {
					at += copy_and_free(expr_buffer + at, sh_print_expr(expr->fields[i]));
				}




				if(i != buf_len(expr->fields) - 1) {
					at += sprintf(expr_buffer+at, ", ");
				}
			}

			at += sprintf(expr_buffer+at, "}");

		} break;

		case SH_OPERATOR_EXPR: {
			if(expr->left_op != NULL)
				at += copy_and_free(expr_buffer + at, sh_print_expr(expr->left_op));
			at += sprintf(expr_buffer + at, "%s", base_type_names[expr->op]);
			at += copy_and_free(expr_buffer + at, sh_print_expr(expr->right_op));
		} break;

		case SH_PTR_DEREF_EXPR: {
			at += sprintf(expr_buffer+at, "de_ref(");
			at += copy_and_free(expr_buffer + at, sh_print_expr(expr->operand));
			at += sprintf(expr_buffer+at, ")");
		} break;

		case SH_FIELD_ACCESS_EXPR: {
			if(expr->pointer_expr != NULL)
				at += copy_and_free(expr_buffer + at, sh_print_expr(expr->pointer_expr));

			at += sprintf(expr_buffer+at, base_type_names[expr->access_type]);
			at += copy_and_free(expr_buffer + at, sh_print_expr(expr->field_access));
		} break;

		case SH_DEC_EXPR:
		case SH_INC_EXPR: {
			at += sprintf(expr_buffer+at,expr->type == SH_INC_EXPR ? "++" : "--");
			at += copy_and_free(expr_buffer + at, sh_print_expr(expr->operand));
		} break;

		case SH_POST_DEC_EXPR:
		case SH_POST_INC_EXPR: {
			at += copy_and_free(expr_buffer + at, sh_print_expr(expr->operand));
			at += sprintf(expr_buffer + at,expr->type == SH_POST_INC_EXPR ? "++" : "--");
		} break;

		case SH_FUNC_EXPR: {
			at += copy_and_free(expr_buffer + at, sh_print_expr(expr->func_expr));
			at += sprintf(expr_buffer + at,"(");
			for(sh_expression **args = expr->args; args != buf_end(expr->args); args++) {
				at += copy_and_free(expr_buffer + at, sh_print_expr(args[0]));
				if((args+1) != buf_end(expr->args)) {
					at += sprintf(expr_buffer + at,", ");
				}
			}
			at += sprintf(expr_buffer + at,")");
		} break;

		case SH_ARRAY_EXPR: {

			at += copy_and_free(expr_buffer + at, sh_print_expr(expr->array_expr));
			at += sprintf(expr_buffer + at,"[");
			at += copy_and_free(expr_buffer + at, sh_print_expr(expr->array_index_expr));
			at += sprintf(expr_buffer + at,"]");
		} break;

		case SH_ID_EXPR: {
			at += sprintf(expr_buffer + at,"%s", expr->name);
		} break;

		case SH_ADDRESS_OF_EXPR: {
			at += sprintf(expr_buffer + at,"&");
			at += copy_and_free(expr_buffer + at, sh_print_expr(expr->operand));
		} break;

		case SH_QUESTION_OPERATOR_EXPR: {
			at += sprintf(expr_buffer+at, "?");
		} break;

		case SH_PEEK_EXPR: {
			at += sprintf(expr_buffer+at, "peek(");
			at += copy_and_free(expr_buffer + at, sh_print_expr(expr->peek_type));
			at += sprintf(expr_buffer+at, ")");
		} break;

		case SH_SIZEOF_BYTE_EXPR: {
			at += sprintf(expr_buffer+at, "sizeof_byte(");
			at += copy_and_free(expr_buffer + at, sh_print_expr(expr->operand));
			at += sprintf(expr_buffer+at, ")");
		} break;

		case SH_SIZEOF_BIT_EXPR: {
			at += sprintf(expr_buffer+at, "sizeof_bit(");
			at += copy_and_free(expr_buffer + at, sh_print_expr(expr->operand));
			at += sprintf(expr_buffer+at, ")");
		} break;

		case SH_PARENT_EXPR: {
			at += sprintf(expr_buffer+at, "@");
			// at += copy_and_free(expr_buffer + at, sh_print_expr(expr->operand));
			// at += sprintf(expr_buffer+at, ")");
		} break;

		default: {
			assert_exit(false, "unhandled expr type");
		}
	}

	return expr_buffer;
}

sh_expression* sh_parse_expression() {
	sh_expression* expr = sh_parse_addition();
	return expr;
}


sh_statement* sh_parse_for_statement() {

	assert(expect_keyword(&for_keyword));
	assert(expect_token('('));

	sh_statement *for_statement = (sh_statement *) calloc(1, sizeof(sh_statement));
	for_statement->type = SH_FOR_STATEMENT;

	if(!is_token(';')) {
		// init-statement
		for_statement->init_statement = sh_parse_statement();
		for_statement->stmt_size += for_statement->init_statement->stmt_size;
	} else {
		expect_token(';');
	}

	if(!is_token(';')) {
		for_statement->condition_expr = sh_parse_expression();
	}

	expect_token(';');

	if(!is_token(')')) {
		for_statement->post_loop_expr = sh_parse_statement();
		for_statement->stmt_size += for_statement->post_loop_expr->stmt_size;
	}

	assert(expect_token(')'));

	if(is_token('{')) {
		// loop body
		for_statement->comp_statement = sh_parse_compound_statement();
		for_statement->stmt_size += for_statement->comp_statement->stmt_size;
	}


	return for_statement;
}

sh_statement* sh_parse_while_statement() {
	assert(expect_keyword(&while_keyword));
	assert(expect_token('('));

	sh_statement *while_statement = (sh_statement *) calloc(1, sizeof(sh_statement));
	while_statement->type = SH_WHILE_STATEMENT;
	while_statement->condition_expr = sh_parse_expression();
	assert(expect_token(')'));
	while_statement->comp_statement = sh_parse_compound_statement();

	return while_statement;
}

sh_statement* sh_parse_elif_statement() {
	assert(expect_keyword(&elif_keyword));
	assert(expect_token('('));

	sh_statement *elif_statement = (sh_statement *) calloc(1, sizeof(sh_statement));
	elif_statement->type = SH_ELIF_STATEMENT;
	elif_statement->condition_expr = sh_parse_expression();
	assert(expect_token(')'));
	elif_statement->comp_statement = sh_parse_compound_statement();

	return elif_statement;
}


sh_statement* sh_parse_if_statement() {
	assert(expect_keyword(&if_keyword));
	assert(expect_token('('));

	sh_statement *if_statement = (sh_statement *) calloc(1, sizeof(sh_statement));
	if_statement->type = SH_IF_STATEMENT;
	if_statement->condition_expr = sh_parse_expression();
	assert_exit(expect_token(')'), "expected ')' got %s", current_token->name);
	if_statement->comp_statement = sh_parse_compound_statement();

	if_statement->stmt_size = if_statement->comp_statement->stmt_size;

	while(is_keyword(&elif_keyword)) {
		sh_statement *elif_stmt = sh_parse_elif_statement();
		if_statement->stmt_size += elif_stmt->stmt_size;
		buf_push(if_statement->elseif_stmts, elif_stmt);
	}

	if(expect_keyword(&else_keyword)) {
		if_statement->else_stmt = sh_parse_compound_statement();
		if_statement->stmt_size += if_statement->else_stmt->stmt_size;
	}
	

	return if_statement;
}


sh_statement* sh_parse_return_statement() {
	assert(expect_keyword(&return_keyword));

	sh_statement *return_statement = (sh_statement *) calloc(1, sizeof(sh_statement));
	return_statement->type = SH_RETURN_STATEMENT;

	if(!is_token(';')) {
		return_statement->ret_expr = sh_parse_expression();
	} 

	return return_statement;
}

sh_statement* sh_parse_continue_statement() {
	assert(expect_keyword(&continue_keyword));

	sh_statement *continue_statement = (sh_statement *) calloc(1, sizeof(sh_statement));
	continue_statement->type = SH_CONTINUE_STATEMENT;

	return continue_statement;
}

//@Todo: maybe break out of multiple places:
sh_statement* sh_parse_break_statement() {
	assert(expect_keyword(&break_keyword));

	sh_statement *break_statement = (sh_statement *) calloc(1, sizeof(sh_statement));
	break_statement->type = SH_BREAK_STATEMENT;
	/* break_statement->condition_expr = sh_parse_expression(); */

	return break_statement;
}


sh_decl_val* sh_new_val_decl(sh_decl *d, sh_typespec *type) {
	sh_decl_val *v = (sh_decl_val*) calloc(1, sizeof(sh_decl_val));
	v->decl = d;
	v->type = type;
	return v;
}





sh_statement* sh_parse_statement() {

start:
	while(expect_token(SH_COMMENT));
	
	if(is_token(SH_END_FILE)) return NULL;

	sh_statement *new_statement = (sh_statement*) calloc(1, sizeof(sh_statement)); // all sizes are zero

	if(is_type(current_token)) {

		new_statement->type = SH_VAR_DECL_STATEMENT;
		new_statement->var_decl = sh_parse_var_decl(SH_LOCAL_STORAGE);
		new_statement->stmt_size = new_statement->var_decl->total_size;

		sh_decl_val *val_decl = sh_new_val_decl(new_statement->var_decl, new_statement->var_decl->var.type);

		buf_push(decls, new_statement->var_decl);
		buf_push(val_decls, val_decl);

		assert_exit(expect_token(';'), "expected ; got %s", current_token->name);
	} else if(is_keyword(&struct_keyword)) {

		sh_parse_struct_decl();
		goto start;

	} else if(is_keyword(&skip_bit_keyword)) {
		sh_expression *skip = sh_parse_skip_expr();
		new_statement->type = SH_SKIP_BIT_STATEMENT;
		new_statement->skip = skip;

	} else if(is_keyword(&skip_byte_keyword)) {
		sh_expression *skip = sh_parse_skip_expr();
		new_statement->type = SH_SKIP_BYTE_STATEMENT;
		new_statement->skip = skip;

	} else if(is_keyword(&rewind_keyword)) {

		sh_expression *rewind = sh_parse_rewind_expr();
		new_statement->type = SH_REWIND_STATEMENT;
		new_statement->rewind = rewind;

	} else if(is_keyword(&for_keyword)) {
		new_statement = sh_parse_for_statement();
	} else if(is_keyword(&while_keyword)) {
		new_statement = sh_parse_while_statement();
	} else if(is_keyword(&if_keyword)) {
		new_statement = sh_parse_if_statement();
	} else if(is_keyword(&return_keyword)) {

		new_statement = sh_parse_return_statement();
		assert(expect_token(';'));

	} else if(is_keyword(&break_keyword)) {

		new_statement = sh_parse_break_statement();
		assert(expect_token(';'));

	} else if(is_keyword(&continue_keyword)) {

		new_statement = sh_parse_continue_statement();
		assert(expect_token(';'));
	} else if(is_keyword(&enum_keyword)) {

		sh_decl *d = sh_parse_enum_decl();
		free(d);
		goto start;

	} else {
		sh_expression *parsed_expr = sh_parse_expression();

		new_statement->type = SH_EXPR_STATEMENT;
		new_statement->expr = parsed_expr;

		if(expect_token('=')) {
			sh_expression *right_hand = sh_parse_expression();
			new_statement->type = SH_ASSIGNMENT_STATEMENT;
			new_statement->left_side_expr = parsed_expr;
			new_statement->right_side_expr = right_hand;
		} else if(parsed_expr->type >= SH_INC_EXPR && parsed_expr->type <= SH_POST_DEC_EXPR) {
			new_statement->type = SH_INC_DEC_STATEMENT;
			new_statement->unary_expr = parsed_expr;
		} else if(parsed_expr->type == SH_FUNC_EXPR) {
			new_statement->type = SH_FUNC_CALL_STATEMENT;
			new_statement->unary_expr = parsed_expr;
		}


		// @Todo: this is bad juju
		expect_token(';');
	}

	

	return new_statement;
}

sh_statement* sh_parse_compound_statement() {

	assert(expect_token('{'));
	sh_statement* new_statement = (sh_statement *)calloc(1, sizeof(sh_statement));

	new_statement->type = SH_COMPOUND_STATEMENT;

	while(expect_token(SH_COMMENT));

	i32 stmt_size = 0;
	while(!is_token('}')) {
		sh_statement *stmt = sh_parse_statement();
		stmt_size += stmt->stmt_size;
		buf_push(new_statement->statements, stmt);
	}

	assert(expect_token('}'));
	new_statement->stmt_size = stmt_size;

	return new_statement;
}


i32 indent_level = 0;

i32 sh_print_indent(char *buffer) {
	for(i32 i = 0; i < indent_level; i++) {
		buffer[i] = '\t';
	}

	return indent_level;
}


#define BUFFER_AT (buffer + at)
#define ADD_TO_BUFFER(str) (at += sprintf(buffer+at,(str)))
#define COPY_TO_BUFFER(expr) ( at += copy_and_free(buffer+at, (expr)))

char* sh_print_statement(sh_statement *stmt) {

	char *buffer = (char*)calloc(1, sizeof(char)*2048);

	i32 at = 0;

	switch(stmt->type) {

		case SH_VAR_DECL_STATEMENT: {
			at += sh_print_indent(BUFFER_AT);
			sh_print_decl(stmt->var_decl);
		} break;

		case SH_COMPOUND_STATEMENT: {
			sh_statement **stmts = stmt->statements;

			at += sh_print_indent(BUFFER_AT);
			ADD_TO_BUFFER("\ncomp {\n");

			indent_level++;

			for(sh_statement **s = stmts; s != buf_end(stmts); s++) {
				COPY_TO_BUFFER(sh_print_statement(*s));
			}

			indent_level--;
			indent_level--; //??
			at += sh_print_indent(BUFFER_AT);
			at += sprintf(BUFFER_AT, "}\n");
			indent_level--;
		} break;

		case SH_ASSIGNMENT_STATEMENT: {
			at += sh_print_indent(BUFFER_AT);
			COPY_TO_BUFFER(sh_print_expr(stmt->left_side_expr));
			ADD_TO_BUFFER(" = ");
			COPY_TO_BUFFER(sh_print_expr(stmt->right_side_expr));
			ADD_TO_BUFFER("\n");
		} break;

		case SH_INC_DEC_STATEMENT: {
			at += sh_print_indent(BUFFER_AT);
			COPY_TO_BUFFER(sh_print_expr(stmt->unary_expr));
		} break;

		case SH_FUNC_CALL_STATEMENT: {
			at += sh_print_indent(BUFFER_AT);
			COPY_TO_BUFFER(sh_print_expr(stmt->unary_expr));
			ADD_TO_BUFFER("\n");
		} break;

		case SH_IF_STATEMENT:
		case SH_WHILE_STATEMENT:
		case SH_FOR_STATEMENT: {
			at += sh_print_indent(BUFFER_AT);

			switch(stmt->type) {
				case SH_WHILE_STATEMENT: ADD_TO_BUFFER("while\n"); break;
				case SH_IF_STATEMENT: ADD_TO_BUFFER("if\n"); break;
				case SH_FOR_STATEMENT: ADD_TO_BUFFER("for\n"); break;
			}

			if(stmt->init_statement) {
				indent_level++;
				at += sh_print_indent(BUFFER_AT);
				ADD_TO_BUFFER("init: \t");
				indent_level++;

				i32 store_indent_level = indent_level;
				indent_level = 0;

				COPY_TO_BUFFER(sh_print_statement(stmt->init_statement));

				indent_level = store_indent_level;
				indent_level--;
				indent_level--;
			}

			if(stmt->condition_expr) {
				indent_level++;
				at += sh_print_indent(BUFFER_AT);

				ADD_TO_BUFFER("condition: ");
				COPY_TO_BUFFER(sh_print_expr(stmt->condition_expr));
				ADD_TO_BUFFER("\n");

				indent_level--;
			}
			
			if(stmt->post_loop_expr) {
				indent_level++;
				at += sh_print_indent(BUFFER_AT);
				ADD_TO_BUFFER("post loop: ");
				COPY_TO_BUFFER(sh_print_statement(stmt->post_loop_expr));
				ADD_TO_BUFFER("\n");
				indent_level--;
			}

			if(stmt->comp_statement) {
				indent_level++;
				at += sh_print_indent(BUFFER_AT);
				ADD_TO_BUFFER("body: \n");
				indent_level++;
				COPY_TO_BUFFER(sh_print_statement(stmt->comp_statement));
				indent_level--;
			}
		} break;
	}

	return buffer;
}


#undef COPY_TO_BUFFER
#undef ADD_TO_BUFFER 
#undef BUFFER_AT
