typedef struct sh_decl sh_decl;
typedef struct sh_type sh_type;
typedef struct sh_typespec sh_typespec;

typedef enum sh_type_kind {
	SH_TYPE_UNKNOWN,
	SH_TYPE_DEFINED_TYPE,
	SH_TYPE_PTR,
	SH_TYPE_VAR,
	SH_TYPE_ARRAY,
	SH_TYPE_STRUCT,
} sh_type_kind;

typedef struct sh_struct_type {
	sh_decl **fields;
} sh_struct_type;

typedef struct sh_alias_type {
	sh_typespec *aliased_type;
} sh_alias_type;

typedef struct sh_type {
	i32 name_len;
	i32 size_byte;

	i32 is_alias;
	i32 is_struct;
	i32 is_ptr;

	char *name;

	union {
		sh_struct_type struct_type;
		sh_alias_type alias;
	};

} sh_type;


typedef struct sh_keyword {
	i32 name_len;
	char* name;	
} sh_keyword;

sh_type **type_table = NULL;
sh_keyword *keyword_table = NULL;


sh_type sh_type_i8 = { .size_byte = 1, .name_len = 2, .name = "i8"};
sh_type sh_type_u8 = { .size_byte = 1, .name_len = 2, .name = "u8"};


sh_type sh_type_i16 = { .size_byte = 2, .name_len = 3, .name = "i16"};
sh_type sh_type_u16 = { .size_byte = 2, .name_len = 3, .name = "u16"};

sh_type sh_type_i32 = { .size_byte = 4, .name_len = 3, .name = "i32"};
sh_type sh_type_u32 = { .size_byte = 4, .name_len = 3, .name = "u32"};

sh_type sh_type_i64 = { .size_byte = 8, .name_len = 3, .name = "i64"};
sh_type sh_type_u64 = { .size_byte = 8, .name_len = 3, .name = "u64"};

sh_type sh_type_i8be = { .size_byte = 1, .name_len = 5, .name = "i8be"};
sh_type sh_type_u8be = { .size_byte = 1, .name_len = 5, .name = "u8be"};

sh_type sh_type_i16be = { .size_byte = 2, .name_len = 5, .name = "i16be"};
sh_type sh_type_u16be = { .size_byte = 2, .name_len = 5, .name = "u16be"};

sh_type sh_type_i32be = { .size_byte = 4, .name_len = 5, .name = "i32be"};
sh_type sh_type_u32be = { .size_byte = 4, .name_len = 5, .name = "u32be"};

sh_type sh_type_i64be = { .size_byte = 8, .name_len = 5, .name = "i64be"};
sh_type sh_type_u64be = { .size_byte = 8, .name_len = 5, .name = "u64be"};

sh_type sh_type_char = { .size_byte = 0, .name_len = 4, .name = "char"};
sh_type sh_type_string = { .size_byte = 0, .name_len = 6, .name = "string"};

sh_type sh_type_void = { .size_byte = 0, .name_len = 4, .name = "void"};

sh_type sh_type_f32 = { .size_byte = 0, .name_len = 3, .name = "f32"};
sh_type sh_type_f64 = { .size_byte = 0, .name_len = 3, .name = "f64"};

sh_type sh_type_nil = { .size_byte = 0, .name_len = 3, .name = "nil"};

void setup_internal_types() {



	buf_push(type_table, &sh_type_i8);
	buf_push(type_table, &sh_type_u8);
	buf_push(type_table, &sh_type_i16);
	buf_push(type_table, &sh_type_u16);
	buf_push(type_table, &sh_type_i32);
	buf_push(type_table, &sh_type_u32);
	buf_push(type_table, &sh_type_i64);
	buf_push(type_table, &sh_type_u64);

	buf_push(type_table, &sh_type_i8be);
	buf_push(type_table, &sh_type_u8be);
	buf_push(type_table, &sh_type_i16be);
	buf_push(type_table, &sh_type_u16be);
	buf_push(type_table, &sh_type_i32be);
	buf_push(type_table, &sh_type_u32be);
	buf_push(type_table, &sh_type_i64be);
	buf_push(type_table, &sh_type_u64be);

	buf_push(type_table, &sh_type_string);
	buf_push(type_table, &sh_type_char);
	buf_push(type_table, &sh_type_void);


	buf_push(type_table, &sh_type_f32);
	buf_push(type_table, &sh_type_f64);

	buf_push(type_table, &sh_type_nil);
}


sh_keyword typedef_keyword = { .name = "typedef", .name_len = 7 };
sh_keyword struct_keyword = { .name = "struct", .name_len = 6 };
sh_keyword for_keyword = { .name = "for", .name_len = 3 };
sh_keyword while_keyword = { .name = "while", .name_len = 5 };
sh_keyword if_keyword = { .name = "if", .name_len = 2 };
sh_keyword elif_keyword = { .name = "elif", .name_len = 4 };
sh_keyword else_keyword = { .name = "else", .name_len = 4 };
sh_keyword break_keyword = { .name = "break", .name_len = 5 };
sh_keyword return_keyword = { .name = "return", .name_len = 6 };
sh_keyword continue_keyword = { .name = "continue", .name_len = 8 };
sh_keyword nil_keyword = { .name = "nil", .name_len = 3 };

void setup_keywords() {

	buf_push(keyword_table, typedef_keyword);
	buf_push(keyword_table, struct_keyword);
	buf_push(keyword_table, for_keyword);
	buf_push(keyword_table, while_keyword);
	buf_push(keyword_table, if_keyword);
	buf_push(keyword_table, else_keyword);
	buf_push(keyword_table, elif_keyword);

	buf_push(keyword_table, continue_keyword);
	buf_push(keyword_table, break_keyword);
	buf_push(keyword_table, return_keyword);
	buf_push(keyword_table, nil_keyword);
}


sh_type* sh_get_type(sh_token *token) {
	for(sh_type **types = type_table;  types != buf_end(type_table); types++ ) {
		sh_type *i = *types;
		if(token->name_len == i->name_len && strncmp(token->name, i->name, i->name_len) == 0) {
			return i;
		}
	}
	return NULL;
}

sh_type* sh_get_type_name(const char *name, i32 name_len) {
	for(sh_type **types = type_table;  types != buf_end(type_table); types++ ) {
		sh_type *i = *types;
		if(name_len == i->name_len && strncmp(name, i->name, i->name_len) == 0) {
			return i;
		}
	}
	return NULL;

}
