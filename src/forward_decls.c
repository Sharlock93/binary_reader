//lex.c

typedef enum sh_token_base_type sh_token_base_type;
typedef enum sh_tag_type sh_tag_type;
typedef struct sh_token_type sh_token_type;
typedef struct sh_token sh_token;

// funcs
void skip_whitespace(void);
void sh_tokenize(void);
void free_token(sh_token *tok);
i32 is_token(sh_token_base_type type);
sh_token* sh_next_token();
i32 expect_token(sh_token_base_type type);
void sh_print_token(sh_token *tok);


//defined_types.c
typedef struct sh_type sh_type;
typedef enum sh_type_kind sh_type_kind;
typedef struct sh_struct_type sh_struct_type;
typedef struct sh_alias_type sh_alias_type;
typedef struct sh_type sh_type;
typedef struct sh_keyword sh_keyword;


//funcs
void setup_internal_types(void);
void setup_keywords(void);
sh_type* sh_get_type(sh_token *token);
sh_type* sh_get_type_name(const char *name, i32 name_len);



//intel_xed.c
void xed_init(void);
i32 print_gen_code(char *ptr);


//parser_test.c

typedef struct sh_decl sh_decl;
typedef enum sh_storage_type sh_storage_type;
typedef struct sh_expression sh_expression;
typedef struct sh_memory_info sh_memory_info;
typedef struct sh_decl_tag sh_decl_tag;
typedef sh_token_base_type sh_expr_operator;
typedef struct sh_decl sh_decl;
typedef struct sh_statement sh_statement;
typedef struct sh_typespec sh_typespec;
typedef struct sh_func_decl sh_func_decl;
i32 parse_int_token(sh_token *tok);
i8 is_typespec_array(sh_typespec *);

sh_expression* sh_parse_expression();
sh_expression* sh_parse_base_expr();
sh_expression* sh_expr_int_literal();
sh_expression* sh_parse_unary_expr();
sh_statement*  sh_parse_statement();

sh_decl_tag* sh_parse_decl_tag();
sh_expression* sh_new_op_expr(sh_expression *left, sh_expression* right, sh_expr_operator op);
sh_statement* sh_parse_compound_statement();


sh_decl* sh_parse_var_decl(sh_storage_type storage);
char* sh_print_decl(sh_decl *decl);
char* sh_print_expr(sh_expression *expr);
char* sh_print_statement(sh_statement *stmt);
char* sh_print_typespec(sh_typespec *type);

i32 copy_and_free(char *dest, char *src);


//breader_parser.c

typedef struct stream_pos stream_pos;
typedef struct sh_decl_val sh_decl_val;

void sh_print_decl_val(sh_decl_val *t);
sh_expression* sh_parse_base_tag_expr(void);
sh_expression* sh_parse_unary_tag_expr(void);
sh_expression* sh_parse_bitwise_tag_expr(void);
sh_expression* sh_parse_logical_tag_expr(void);
sh_expression* sh_parse_multi_tag_expr(void);
sh_expression* sh_parse_addition_tag_expr(void);
sh_expression* sh_parse_logical_and_or_tag_expr(void);
sh_expression* sh_parse_assert_tag_expr(void);

sh_decl_tag* sh_parse_assert_tag(void);
sh_decl_tag* sh_parse_cond_tag(void);
sh_decl_tag* sh_parse_print_tag(void);
sh_decl_tag* sh_parse_array_print_tag(void);
sh_decl_tag* sh_parse_struct_print_tag(void);
sh_decl_tag* sh_get_decl_tag(sh_decl_val *d, sh_tag_type type);
sh_decl_tag* sh_parse_add_on_tag();
sh_decl_tag* sh_parse_offset_tag();
sh_decl_val* sh_get_expr_decl_val(sh_expression *e);


// Opengl
typedef struct sh_window_context sh_window_context;
