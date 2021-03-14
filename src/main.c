#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <Windows.h>
#include <math.h>

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


int main(void) {

#if 1 
	main_source = read_file("test.txt", NULL);


	setup_internal_types();
	setup_keywords();
	
	xed_init();

	do {
		sh_tokenize();
		buf_push(tokens, main_token);
	} while(main_token.type.base != SH_END_FILE);
	
	current_token = tokens;

	parse_file();

	for(sh_decl **decl = decls; decl != buf_end(decls); decl++) {
		sh_type_check_decl(decl[0]);
	}

	gen_main();



	/* for(sh_decl **decl = decls; decl != buf_end(decls); decl++) { */
	/* 	sh_gen_ir_decl(decl[0]); */
	/* } */
    /*  */
	/* for(sh_operation **op = operations; op != buf_end(operations); op++) { */
	/* 	sh_print_op(op[0]); */
	/* 	printf("\n"); */
	/* } */


#endif
	return 0;
}
