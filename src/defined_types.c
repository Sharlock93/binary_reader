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
	SH_TYPE_ENUM,
	SH_TYPE_WHEN,
    SH_TYPE_BIT_FIELD
} sh_type_kind;

typedef struct sh_struct_type {
	sh_decl **fields;
} sh_struct_type;

typedef struct sh_alias_type {
	sh_typespec *aliased_type;
} sh_alias_type;

typedef struct sh_enum_type {
	sh_typespec *base_type;
	sh_decl **fields;
} sh_enum_type;


typedef struct sh_type {
	i32 name_len;
	i32 size_byte;
	i32 size_bit;

	i32 is_alias;
	i32 is_struct;
	i32 is_ptr;
	i32 is_enum;

	const char *name;

	union {
		sh_struct_type struct_type;
		sh_alias_type alias;
		sh_enum_type enum_type;
	};

} sh_type;


typedef struct sh_keyword {
	char* name;	
	i32 name_len;
} sh_keyword;

sh_type **type_table = NULL;
sh_keyword *keyword_table = NULL;


#define DEFINE_TYPE(n, size_bit) sh_type sh_type_##n = { sizeof(#n)-1, size_bit/8, size_bit, .name = #n }
DEFINE_TYPE(i8,     8);
DEFINE_TYPE(u8,     8);
DEFINE_TYPE(i16,    16);
DEFINE_TYPE(u16,    16);
DEFINE_TYPE(i32,    32);
DEFINE_TYPE(u32,    32);
DEFINE_TYPE(i64,    64);
DEFINE_TYPE(u64,    64);

DEFINE_TYPE(i8be,   8);
DEFINE_TYPE(u8be,   8);
DEFINE_TYPE(i16be,  16);
DEFINE_TYPE(u16be,  16);
DEFINE_TYPE(i32be,  32);
DEFINE_TYPE(u32be,  32);
DEFINE_TYPE(i64be,  64);
DEFINE_TYPE(u64be,  64);

DEFINE_TYPE(char,   8);
DEFINE_TYPE(string, 0);
DEFINE_TYPE(void,   0);

DEFINE_TYPE(f32,    32);
DEFINE_TYPE(f64,    64);
DEFINE_TYPE(nil,    0);
DEFINE_TYPE(bit,    1);
DEFINE_TYPE(tib,    1);
#undef DEFINE_TYPE


#define DEFINE_TYPE(n, x) buf_push(type_table, &sh_type_##n); __COUNTER__
void setup_internal_types() {
	DEFINE_TYPE(i8,     8);
	DEFINE_TYPE(u8,     8);
	DEFINE_TYPE(i16,    16);
	DEFINE_TYPE(u16,    16);
	DEFINE_TYPE(i32,    32);
	DEFINE_TYPE(u32,    32);
	DEFINE_TYPE(i64,    64);
	DEFINE_TYPE(u64,    64);

	DEFINE_TYPE(i8be,   8);
	DEFINE_TYPE(u8be,   8);
	DEFINE_TYPE(i16be,  16);
	DEFINE_TYPE(u16be,  16);
	DEFINE_TYPE(i32be,  32);
	DEFINE_TYPE(u32be,  32);
	DEFINE_TYPE(i64be,  64);
	DEFINE_TYPE(u64be,  64);

	DEFINE_TYPE(char,   8);
	DEFINE_TYPE(string, 0);
	DEFINE_TYPE(void,   0);

	DEFINE_TYPE(f32,    32);
	DEFINE_TYPE(f64,    64);
	DEFINE_TYPE(nil,    0);
	DEFINE_TYPE(bit,    1);
	DEFINE_TYPE(tib,    1);

}
#undef DEFINE_TYPE

#define KEYWORD_DEFINE(key) sh_keyword key##_keyword = { #key, sizeof(#key) - 1 }
	KEYWORD_DEFINE(typedef);
	KEYWORD_DEFINE(struct);
	KEYWORD_DEFINE(enum);
	KEYWORD_DEFINE(union);
	KEYWORD_DEFINE(pack);
	KEYWORD_DEFINE(for);
	KEYWORD_DEFINE(while);
	KEYWORD_DEFINE(if);
	KEYWORD_DEFINE(elif);
	KEYWORD_DEFINE(else);
	KEYWORD_DEFINE(break);
	KEYWORD_DEFINE(return);
	KEYWORD_DEFINE(continue);
	KEYWORD_DEFINE(nil);
	KEYWORD_DEFINE(dll_import);
	KEYWORD_DEFINE(var_import);
	KEYWORD_DEFINE(skip_bit);
	KEYWORD_DEFINE(skip_byte);
	KEYWORD_DEFINE(rewind);
	KEYWORD_DEFINE(peek);
	KEYWORD_DEFINE(sizeof_byte);
	KEYWORD_DEFINE(sizeof_bit);
	KEYWORD_DEFINE(assert);
	KEYWORD_DEFINE(cond);
	KEYWORD_DEFINE(add_on);
	KEYWORD_DEFINE(offset);

	KEYWORD_DEFINE(print);
	KEYWORD_DEFINE(array_print);
	KEYWORD_DEFINE(struct_print);
	KEYWORD_DEFINE(when);
#undef KEYWORD_DEFINE



void setup_keywords() {

#define KEYWORD_DEFINE(key) buf_push(keyword_table, key##_keyword)
	KEYWORD_DEFINE(typedef);
	KEYWORD_DEFINE(struct);
	KEYWORD_DEFINE(enum);
	KEYWORD_DEFINE(union);
	KEYWORD_DEFINE(pack);
	KEYWORD_DEFINE(for);
	KEYWORD_DEFINE(while);
	KEYWORD_DEFINE(if);
	KEYWORD_DEFINE(elif);
	KEYWORD_DEFINE(else);
	KEYWORD_DEFINE(break);
	KEYWORD_DEFINE(return);
	KEYWORD_DEFINE(continue);
	KEYWORD_DEFINE(nil);
	KEYWORD_DEFINE(dll_import);
	KEYWORD_DEFINE(var_import);
	KEYWORD_DEFINE(skip_bit);
	KEYWORD_DEFINE(skip_byte);
	KEYWORD_DEFINE(rewind);
	KEYWORD_DEFINE(peek);
	KEYWORD_DEFINE(sizeof_byte);
	KEYWORD_DEFINE(sizeof_bit);
	KEYWORD_DEFINE(assert);
	KEYWORD_DEFINE(cond);
	KEYWORD_DEFINE(add_on);
	KEYWORD_DEFINE(offset);

	KEYWORD_DEFINE(print);
	KEYWORD_DEFINE(array_print);
	KEYWORD_DEFINE(struct_print);
	KEYWORD_DEFINE(when);
#undef KEYWORD_DEFINE
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
