typedef struct sh_token_type {
	sh_token_base_type base;
	i32 size_byte;
} sh_token_type;

typedef struct sh_token {
	sh_token_type type;
	i32 name_len;
	char *name;
} sh_token;


sh_token main_token = {0};
sh_token *current_token = NULL;
char *main_source = NULL;


void skip_whitespace() {
	while(isspace(main_source[0])) main_source++;
}

void sh_tokenize() {

	skip_whitespace();

	switch(main_source[0]) {
		case '\0': {
			main_token.type.base = SH_END_FILE;
			main_token.name = "";
			main_token.name_len = 0;
		} break;

		case '"': {
			// Note: handle escapes? maybe? or not needed
			main_source++;
			char *str_start = main_source;
			while(main_source[0] != '"') main_source++;
			char *str_end = main_source;

			i32 str_len = (i32)( str_end - str_start );
			main_token.type.base = SH_STRING;
			main_token.name = (char *) calloc(str_len + 1, sizeof(char));
			memcpy(main_token.name, str_start, str_len);
			main_token.name[str_len] = 0;
			main_token.name_len = str_len;
			main_source++;
		} break;


// logical operators: 
// << >> <= >= == -- ++ && 
		case '-': {
			if(main_source[1] == '-') {
				main_token.type.base = SH_DECREMENT;
				main_token.name_len = 2;
				main_source += 2;
			} else if (main_source[1] == '>') {
				main_token.type.base = SH_POINTER_ACCESS;
				main_token.name_len = 2;
				main_source += 2;
			} else {
				main_token.type.base = SH_MINUS;
				main_token.name_len = 1;
				main_source++;
			}

			main_token.type.size_byte = 0;
			main_token.name = base_type_names[main_token.type.base];
		} break;
		case '+': {
			if(main_source[1] == '+') {
				main_token.type.base = SH_INCREMENT;
				main_token.name_len = 2;
				main_source += 2;
			} else {
				main_token.type.base = SH_PLUS;
				main_token.name_len = 1;
				main_source++;
			}

			main_token.type.size_byte = 0;
			main_token.name = base_type_names[main_token.type.base];

		} break;

		case '!': {

			if(main_source[1] == '=') {
				main_token.type.base = SH_NEQ;
				main_token.name_len = 2;
				main_source += 2;
			} else {
				main_token.type.base = main_source[0];
				main_token.name_len = 1;
				main_source++;
			}

			main_token.type.size_byte = 0;
			main_token.name = base_type_names[main_token.type.base];


		} break;

		case '=': {
			if(main_source[1] == '=') {
				main_token.type.base = SH_EQ;
				main_token.name_len = 2;
				main_source += 2;
			} else {
				main_token.type.base = main_source[0];
				main_token.name_len = 1;
				main_source++;
			}

			main_token.type.size_byte = 0;
			main_token.name = base_type_names[main_token.type.base];
		} break;

		case '<':
			if(main_source[1] == '=') {
				main_token.type.base = SH_LTEQ;
				main_token.name_len = 2;
				main_source++;
			} else {
				main_token.type.base = SH_LT;
				main_token.name_len = 1;
			}

			main_source++;
			main_token.type.size_byte = 0;
			main_token.name = base_type_names[main_token.type.base];
			break;

		case '>':
			if(main_source[1] == '=') {
				main_token.type.base = SH_GREQ;
				main_token.name_len = 2;
				main_source++;
			} else {
				main_token.type.base = SH_GR;
				main_token.name_len = 1;
			}

			main_source++;
			main_token.type.size_byte = 0;
			main_token.name = base_type_names[main_token.type.base];
			break;

		case '&': { 

			if(main_source[1] == '&') {
				main_token.type.base = SH_AND;
				main_token.name_len = 2;
				main_source++;
			} else {
				main_token.type.base = SH_AND_OPERATOR;
				main_token.name_len = 1;
			}

			main_source++;
			main_token.type.size_byte = 0;
			main_token.name = base_type_names[main_token.type.base];

		} break;

		case '|': { 

			if(main_source[1] == '|') {
				main_token.type.base = SH_OR;
				main_token.name_len = 2;
				main_source++;
			} else {
				main_token.type.base = main_source[0];
				main_token.name_len = 1;
			}

			main_source++;
			main_token.type.size_byte = 0;
			main_token.name = base_type_names[main_token.type.base];

		} break;

		case '/': {

			if(main_source[1] == '/') {
				main_token.type.base = SH_COMMENT;

				char *token_start = main_source;

				while(main_source[0] != '\n') {
					main_source++;
				}

				char *token_end = main_source;
				i32 token_len = (i32)( token_end - token_start );

				main_token.type.size_byte = token_len;
				main_token.name_len = token_len;
				main_token.name = (char *) calloc(token_len + 1, sizeof(char));
				memcpy(main_token.name, token_start, token_len);
				main_token.name[token_len] = 0;

			} else {
				main_token.type.base = SH_DIV;
				main_token.name_len = 1;
				main_token.type.size_byte = 0;
				main_token.name = base_type_names[main_token.type.base];
				main_source++;
			}

		} break;

		case '*': {
			main_token.type.base = SH_ASTERISK;
			main_token.name_len = 1;
			main_token.type.size_byte = 0;
			main_token.name = base_type_names[main_token.type.base];
			main_source++;
		} break;

		case '^': {
			main_token.type.base = SH_BXOR;
			main_token.name_len = 1;
			main_token.type.size_byte = 0;
			main_token.name = base_type_names[main_token.type.base];
			main_source++;
		} break;



		case '@':
		case ',':
		case '?':
		case '(':
		case ')':
		case '{':
		case '}':
		case '[':
		case ';':
        case ':':
		case '.':
		case ']': {
			main_token.type.base = main_source[0];
			main_token.name_len = 1;
			main_token.type.size_byte = 0;
			main_token.name = base_type_names[main_token.type.base];
			main_source++;
		} break;
 
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9': {
			char *token_start = main_source;
			sh_token_base_type type = SH_INT;

			while(isdigit(main_source[0]) || main_source[0] == '.' ) {
				if(main_source[0] == '.') {
					type = SH_FLOAT;
				}

				main_source++;
			}

			char *token_end = main_source;

			i32 token_len = (i32)( token_end - token_start );

			main_token.type.base = type;
			main_token.type.size_byte = base_type_size[type];

			main_token.name_len = token_len;
			main_token.name = (char *) calloc(token_len + 1, sizeof(char));
			memcpy(main_token.name, token_start, token_len);
			main_token.name[token_len] = 0;

		} break;

		default: {
			char *token_start = main_source;

			while(isalnum(main_source[0]) || main_source[0] == '_') {
				main_source++;
			}

			char *token_end = main_source;
			i32 token_len = (i32)( token_end - token_start );

			main_token.type.base = SH_IDENTIFIER;
			main_token.type.size_byte = token_len;
			main_token.name_len = token_len;
			main_token.name = (char *) calloc(token_len + 1, sizeof(char));
			memcpy(main_token.name, token_start, token_len);
			main_token.name[token_len] = 0;
		} break;

	}

}

void free_token(sh_token *tok) {

	i32 freeable = tok->type.base > SH_FREE_START && tok->type.base < SH_FREE_END;

	if(tok->name && freeable) {
		free(tok->name);
	}
}


i32 is_token(sh_token_base_type type) {
	if(current_token->type.base == type) {
		return true;
	}

	return false;
}


sh_token* sh_next_token() {

	// Todo: Fix this comment skip

	if(is_token(SH_COMMENT)) {
		while(is_token(SH_COMMENT)) current_token++;
	} else {
		current_token++;
	}

	while(is_token(SH_COMMENT))
		current_token++;

	return current_token;
}


i32 expect_token(sh_token_base_type type) {
	if(current_token->type.base == type) {
		sh_next_token();
		return 1;
	}

	return 0;
}




void sh_print_token(sh_token *tok) {
	printf("token_type:  %d %s, n: %s, s: %d\n",
		tok->type.base,
		base_type_names[tok->type.base],
		tok->name,
		tok->type.size_byte
	);
}

i8 is_str_id_eq(char *str, i32 str_len) {
	return str_len == current_token->name_len && strncmp(str, current_token->name, str_len) == 0;
}
