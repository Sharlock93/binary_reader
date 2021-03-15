typedef struct sh_operation sh_operation;
typedef enum sh_register sh_register;

typedef enum sh_register {
	RAX,
	RCX,
	RDX,
	RBX,
	RSP,
	RBP,
	RSI,
	RDI,
	R8,
	R9,
	R10,
	R11,
	R12,
	R13,
	R14,
	R15,


} sh_register;


char *register_names[] = {
	[RAX] = "RAX",
	[RCX] = "RCX",
	[RDX] = "RDX",
	[RBX] = "RBX",
	[RSP] = "RSP",
	[RBP] = "RBP",
	[RSI] = "RSI",
	[RDI] = "RDI",
	[R8 ] = "R8",
	[R9 ] = "R9",
	[R10] = "R10",
	[R11] = "R11",
	[R12] = "R12",
	[R13] = "R13",
	[R14] = "R14",
	[R15] = "R15"
};


sh_operation **operations = NULL;

typedef enum sh_op_type {
	SH_UNKNOWN_OP,
	SH_STORE_OP,
	SH_LOAD_OP,
	SH_ADD_OP,
	SH_SUB_OP,
	SH_MUL_OP,
	SH_DIV_OP
} sh_op_type;

char *op_type_name[] = {
	[SH_UNKNOWN_OP] = "unknown",
	[SH_STORE_OP] = "store",
	[SH_LOAD_OP] = "load",
	[SH_ADD_OP] = "+",
	[SH_SUB_OP] = "-",
	[SH_MUL_OP] = "*",
	[SH_DIV_OP] = "/"
};

typedef enum sh_op_source_type {
	SH_SRC_UNKOWN,
	SH_SRC_REGISTER,
	SH_SRC_MEMORY,
	SH_SRC_STACK,
	SH_SRC_IMMEDIATE
} sh_op_source_type;


char *op_source_names[] = {
	[SH_SRC_UNKOWN] = "unkown",
	[SH_SRC_REGISTER] = "register",
	[SH_SRC_MEMORY] = "memory",
	[SH_SRC_STACK] = "stack",
	[SH_SRC_IMMEDIATE] = "imm"
};

typedef struct sh_op_source {
	sh_op_source_type type;

	sh_register reg;
	i64 mem_address;
	i64 stack_relative;
	i64 imm_val;

} sh_op_dst, sh_op_src, sh_op_operand;


typedef struct sh_operation {
	sh_op_type  op;
	union {
		sh_op_operand  src;
		sh_op_operand  first_op;
	};

	union {
		sh_op_operand  dst;
		sh_op_operand  second_op;
	};

	union {
		sh_op_operand  store_res;
	};

} sh_operation;


sh_operation* sh_new_op(sh_op_type type) {
	sh_operation *op = (sh_operation*) calloc(1, sizeof(sh_operation));
	op->op = type;
	return op;
}

void sh_gen_store_op(sh_op_src src, sh_op_dst dst) {
	sh_operation *op = sh_new_op(SH_STORE_OP);
	op->src = src;
	op->dst = dst;
	buf_push(operations, op);
}


sh_op_type sh_convert_expr_op(sh_expr_operator op) {
	switch(op) {
		case SH_PLUS: return SH_ADD_OP; break;
		case SH_MINUS: return SH_SUB_OP; break;
		case SH_ASTERISK: return SH_MUL_OP; break;
		case SH_DIV: return SH_DIV_OP; break;
		default: return SH_UNKNOWN_OP; break;
	}
}


void sh_gen_operation(sh_expr_operator operator, sh_op_src dst, sh_op_src first_op, sh_op_dst second_op) {

	sh_operation *op = sh_new_op(sh_convert_expr_op(operator));
	op->first_op = first_op;
	op->second_op = second_op;
	op->store_res = dst;
	buf_push(operations, op);
}

//first move our memorty into the provided reg
sh_op_operand sh_new_mem_location_reg(u64 mem_address, sh_register reg) {
	sh_op_operand operand = {
		.type = SH_SRC_MEMORY,
		.mem_address = mem_address,
		.reg = reg
	};

	return operand;
}

sh_op_operand sh_new_mem_location(u64 mem_address) {
	sh_op_operand operand = {
		.type = SH_SRC_MEMORY,
		.mem_address = mem_address
	};

	return operand;
}

sh_op_operand sh_new_reg_location(sh_register reg) {
	sh_op_operand operand = {
		.type = SH_SRC_REGISTER,
		.reg = reg
	};

	return operand;
}

sh_op_operand sh_new_stack_location(u64 stack_offset) {
	sh_op_operand operand = {
		.type = SH_SRC_STACK,
		.stack_relative = stack_offset
	};

	return operand;
}


sh_op_operand sh_new_ir_imm_operand(i64 imm_val, sh_register store_reg) {
	return (sh_op_operand){ .type = SH_SRC_IMMEDIATE, .imm_val = imm_val, .reg = store_reg };
}


sh_op_operand sh_gen_ir_expr(sh_expression *expr) {
	sh_op_operand operand = {0};


	switch(expr->type) {
		case SH_INT_LITERAL: {
			return sh_new_ir_imm_operand(expr->vi64, RAX);
		} break;

		case SH_OPERATOR_EXPR: {

			sh_op_operand left_op = sh_gen_ir_expr(expr->left_op);
			if(left_op.type == SH_SRC_IMMEDIATE) {
				sh_op_operand new_store =  sh_new_reg_location(RAX);
				sh_gen_store_op(left_op, new_store );
				left_op = new_store;
			}

			sh_op_operand right_op = sh_gen_ir_expr(expr->right_op);

			if(right_op.type == SH_SRC_IMMEDIATE) {
				sh_op_operand new_store =  sh_new_reg_location(RBX);
				sh_gen_store_op(right_op, new_store);
				right_op = new_store;
			}

			sh_gen_operation(expr->op, left_op, left_op, right_op);

			return left_op;
		} break;
	}

	return operand;
}

void sh_gen_ir_var_decl(sh_decl *decl) {
	assert_exit(decl->type == SH_VAR_DECL, "Need a decl");

	sh_op_src src = {0};
	sh_op_dst dst = sh_new_mem_location(0);

	if(decl->var.init_expr) {
		src = sh_gen_ir_expr(decl->var.init_expr);
	}

	sh_gen_store_op(src, dst);
}


void sh_gen_ir_decl(sh_decl *decl) {

	switch(decl->type) {
		case SH_VAR_DECL: {
			sh_gen_ir_var_decl(decl);
		} break;
	}
}


void sh_print_operand(sh_op_operand *operand) {
	switch(operand->type) {
		case SH_SRC_UNKOWN: {
			printf("No");
		} break;
		case SH_SRC_IMMEDIATE: {
			printf("%lld", operand->imm_val);
		} break;

		case SH_SRC_MEMORY: {
			printf("[@%lld]", operand->mem_address);
		} break;

		case SH_SRC_REGISTER: {
			printf("%s", register_names[operand->reg]);
		} break;

		case SH_SRC_STACK: {
			printf("[stack]");
		} break;
	}
}


void sh_print_store_op(sh_operation *op) {

	assert_exit(op->dst.type != SH_SRC_IMMEDIATE, "cannot store into immediate?");
	sh_print_operand(&op->dst);
	printf(" <== ");
	sh_print_operand(&op->src);
}

void sh_print_op(sh_operation *op) {

	switch(op->op) {
		case SH_STORE_OP: {
			sh_print_store_op(op);
	 	} break;
		
		case SH_ADD_OP: {
			sh_print_operand(&op->store_res);
			printf("<==");
			sh_print_operand(&op->first_op);
			printf("+");
			sh_print_operand(&op->second_op);
		} break;
	}

}
