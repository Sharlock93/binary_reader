typedef enum sh_token_base_type {
	SH_FREE_START, // indicate its freeable 

	SH_INT,
	SH_STRING,
	SH_FLOAT,
	SH_IDENTIFIER,

	SH_FREE_END,

	SH_END_FILE,

	SH_ASSIGNMENT = '=',

	SH_OPEN_PARAN = '(',
	SH_CLOSE_PARAN = ')',

	SH_OPEN_BRACE = '{',
	SH_CLOSE_BRACE = '}',

	SH_OPEN_BRACKET = '[',
	SH_CLOSE_BRACKET = ']',
	SH_SEMI_COLON = ';',

	SH_COLON = ':',

	SH_QUESTION_OPERATOR = '?', //wut? 
	SH_COMMA = ',',
	SH_DOT_OPERATOR = '.',

	SH_PLUS,
	SH_MINUS,

	SH_DIV,
	SH_ASTERISK,

	SH_BOR = '|',
	SH_BXOR = '^',

	SH_GR ,
	SH_LOGICAL_OP_START = SH_GR,
	SH_AND_OPERATOR,
	SH_LT,
	SH_GREQ,
	SH_LTEQ,
	SH_EQ,
	SH_NEQ,
	SH_AND,
	SH_OR,
	SH_LOGICAL_OP_END = SH_OR,

	SH_INCREMENT,
	SH_DECREMENT,
	SH_POINTER_ACCESS,

	SH_COMMENT,
	SH_AT = '@'

} sh_token_base_type;

char *base_type_names[] = {
	[SH_INT] = "int",
	[SH_FLOAT] = "float",
	[SH_IDENTIFIER] = "identifier",
	[SH_STRING] = "str",

	[SH_ASSIGNMENT] = "=",
	[SH_OPEN_PARAN] = "(",
	[SH_CLOSE_PARAN] = ")",

	[SH_OPEN_BRACE] = "{",
	[SH_CLOSE_BRACE] = "}",

	[SH_OPEN_BRACKET] = "[",
	[SH_CLOSE_BRACKET] = "]",

	[SH_SEMI_COLON] = ";",
	[SH_COLON] = ":",

	[SH_ASTERISK] = "*",
	[SH_PLUS] = "+",
	[SH_MINUS] = "-",
	[SH_DIV] = "/",
	[SH_QUESTION_OPERATOR] = "?",
	[SH_COMMA] = ",",

	[SH_AND_OPERATOR] = "&",
	[SH_BOR] = "|",
	[SH_BXOR] = "^",

	[SH_GR] = ">",
	[SH_GREQ] = ">=",
	[SH_LT] = "<",
	[SH_LTEQ] = "<=",


	[SH_EQ] = "==",
	[SH_NEQ] = "!=",

	[SH_INCREMENT] = "++",
	[SH_DECREMENT] = "--",
	[SH_POINTER_ACCESS] = "->",

	[SH_AND] = "&&",
	[SH_OR] = "||",
	[SH_COMMENT] = "//",
	[SH_DOT_OPERATOR] = ".",

	[SH_AT] = "@",

	[SH_END_FILE] = "eof"
};

i32 base_type_size[] = {
	[SH_INT] = 4,
	[SH_FLOAT] = 4
};

