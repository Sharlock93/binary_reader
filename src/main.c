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
#include "base_types.c"

#include "lex.c"

#include "defined_types.c"

sh_token *tokens;
sh_token *current_token;
sh_decl **decls = NULL;


#include "intel_xed.c"

#include "parser_test.c"
#include "type_check.c"
#include "generate_ir.c"
#include "generator.c"


void compile() {
	main_source = read_file("test.sh_it", NULL);

	setup_internal_types();
	setup_keywords();

	do {
		sh_tokenize();
		buf_push(tokens, main_token);
	} while(main_token.type.base != SH_END_FILE);

	current_token = tokens;
	parse_file();
	// gen_main();

}


void read_var_decl(sh_decl *d, u8 *data) {



}

void read_decl_binary(sh_decl *decl, u8 *data) {


	switch(decl->type) {

		case SH_VAR_DECL: {
			read_var_decl(decl, data);
		} break;

		default: {
			assert_exit(false, "we don't handle this decl %s", sh_print_decl(decl));
		}
	}


}

int main(void) {
	compile();
	

	u8 *binary_file = (u8*)read_file("meow", NULL);


	read_decl_binary(decls[0], binary_file);

	return 0;
}
