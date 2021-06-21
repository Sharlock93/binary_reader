#if INCLUDE_LANGUAGE

#include "forward_decls.c"

#include "base_types.c"
#include "lex.c"
#include "defined_types.c"

typedef struct sh_statement sh_statement;
typedef struct sh_decl_val sh_decl_val;

sh_token *tokens;
sh_token *current_token;
sh_decl **decls = NULL;
sh_decl_val **val_decls = NULL;
sh_statement **stmts = NULL;
void compile(char *filepath);

#include "intel_xed.c"
#include "parser_test.c"
#include "breader_parser.c"
#include "type_check.c"
#include "generate_ir.c"
#include "generator.c"
#include "breader.c"




void setup() {
	setup_internal_types();
	setup_keywords();
}

void tokenize() {
	do {
		sh_tokenize();
		buf_push(tokens, main_token);
	} while(main_token.type.base != SH_END_FILE);

	current_token = tokens;
}

void compile(char *filepath) {
	main_source = read_file(filepath, NULL);
	char *store = main_source;
	tokenize();
	parse_file();
	free(store);
	// gen_main();
}

void test() {
#if 1
	test_file("assert");
	test_file("peek");
	test_file("bit_field");
	test_file("array_size_depend");
	test_file("read_cond");
	test_file("enums");
	test_file("bmp_test");
	test_file("bmp_test_read_cond");
	test_file("when");
	test_file("printing");
	test_file("add_on");
	test_file("offset");
#endif
}


void test_bmp() {
	// test_binary_file("tests/bmp.sh_it", "tests/bmp");
	// test_binary_file("tests/bmp.sh_it", "tests/bmp2.bmp");
	test_binary_file("tests/bmp.sh_it", "tests/bmp3.bmp");
	// test_binary_file("tests/bmp.sh_it", "tests/bmp4.bmp");
}

void test_png() {
	test_binary_file("tests/png/png.sh_it", "tests/png/basn0g01.png");
	test_binary_file("tests/png/png.sh_it", "tests/png/basn6a16.png");
	test_binary_file("tests/png/png.sh_it", "tests/png/oi4n2c16.png");
}

void test_ttf() {
	test_binary_file("tests/ttf/ttf.sh_it", "tests/ttf/envy.ttf");
}

void language_main(void) {
	// setup();
	// test();
	// test_bmp();
	// test_png();
	// test_ttf();

}

#endif 
