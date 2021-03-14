typedef void (*rand_func)();

typedef sh_op_source_type sh_memory_type;

struct sh_memory_info {
	sh_memory_type mem_type;
	union {
		u64 stack_rel;
		u64 mem_address;
		sh_register reg;
	};
};

typedef enum sh_opcode_prefix {
	REX_W = 0b01001000
} sh_opcode_prefix;

char* main_mem = NULL;
char* end_of_mem = NULL;

char* location_of_printf = NULL;


char* writer_head = NULL; // where are we writing code gen
char* stack_base = NULL; // where the stack starts
char* stack_top = NULL; // where it is now
char* data_section = NULL;
char* data_section_header = NULL;

i32 stack_size = 1024;
i32 data_section_size = 1024;
i32 code_section_size = 1024;

#define WRITE_HEAD(val) (writer_head[0] = (val), writer_head++)
#define WRITE_HEAD_REL(val, rel) (writer_head[rel] = (val))


char* write_string_literal(const char *str, size_t bytes); // copy to data sectio
void make_stack_size(i32 bytes);
void reduce_stack_size(i32 bytes);
void write_to_register(sh_register reg, char* data_to_write, i32 data_size);
void write_push_stack_register(sh_register reg);
void write_pop_stack_register(sh_register reg);
void write_add_register_rm(sh_register reg, sh_register rm, char *number, i32 data_size);
void write_mov(sh_op_operand dst, sh_op_operand src);
char* get_reg_name_op(sh_op_operand reg);


// returns a register that is different from reg1 
sh_register choose_register_keep_left(sh_register reg1, sh_register reg2) {
	for(sh_register i = reg1; i < R15; i++) {
		if(reg1 != i)  {
			reg2 = i;
			break;
		}

	}

	return reg2;
}

sh_register choose_register(sh_register reg1) {
	sh_register re = RAX + 1;
	for(sh_register i = re; i < R15; i++) {
		if(reg1 != i)  {
			re = i;
			break;
		}

	}

	return re;
}

sh_register choose_register_keep_both(sh_register reg1, sh_register reg2) {
	for(sh_register i = RAX+1; i < R15; i++) {
		if(i != reg1 && i != reg2) {
			return i;
		}
	}

	return R15;
}

void write_mov(sh_op_operand dst, sh_op_operand src) {

	/* printf("%s[%s] <= %s[%s]\n", op_source_names[dst.type], register_names[dst.reg], register_names[src.reg], op_source_names[src.type]); */
	switch(src.type) {
		case SH_SRC_IMMEDIATE: {

			switch(dst.type) {
				case SH_SRC_MEMORY: {
					write_to_register(RAX, (char*)&dst.mem_address, 8);
					WRITE_HEAD(REX_W);
					WRITE_HEAD(0xC7);
					WRITE_HEAD(0b00000000 | RAX);
					char *num = (char*)&src.imm_val;
					for(int i = 0; i < 4; i++) {
						WRITE_HEAD(num[i]);
					}

				} break;

				case SH_SRC_REGISTER: {
					char *dat = (char*)&src.imm_val;
					write_to_register(dst.reg, dat, 8);
				} break;
			}


		} break;

		case SH_SRC_REGISTER: {

			switch(dst.type) {
				// mov [mem], reg
				case SH_SRC_MEMORY: {
					write_to_register(dst.reg, (char*)&dst.mem_address, 8);
					WRITE_HEAD(REX_W);
					WRITE_HEAD(0x89);
					WRITE_HEAD(0b00000000 | src.reg << 3 | dst.reg );
				} break;

				case SH_SRC_REGISTER: {

					if(dst.reg != src.reg) {
						WRITE_HEAD(REX_W);
						WRITE_HEAD(0x89);
						WRITE_HEAD(0b11000000 | src.reg << 3 | dst.reg);
					}

				} break;
			}



		} break;

		case SH_SRC_MEMORY: {

			switch(dst.type) {
				case SH_SRC_MEMORY: {

					sh_register src_reg = src.reg;
					sh_register dst_reg = dst.reg;

					src.reg = src_reg;
					char *dat = (char*)&src.mem_address;
					write_to_register(src_reg, dat, 8);


					WRITE_HEAD(REX_W);
					WRITE_HEAD(0x8B);
					WRITE_HEAD(0b00000000 | dst_reg << 3 | src_reg);


					dat = (char*)&dst.mem_address;
					write_to_register(src_reg, dat, 8);

					WRITE_HEAD(REX_W);
					WRITE_HEAD(0x89);
					WRITE_HEAD(0b00000000 | dst_reg << 3 | src_reg);

				} break;

				case SH_SRC_REGISTER: {

					sh_register src_reg = src.reg;
					sh_register dst_reg = dst.reg;

					write_to_register(src_reg, (char*)&src.mem_address, 8);
					WRITE_HEAD(REX_W);
					WRITE_HEAD(0x8B);
					WRITE_HEAD(0b00000000 | dst_reg << 3 | src_reg );
				} break;

			}



		} break;

	}

}


void write_sub_register_rm(sh_register reg, sh_register rm, char *number, i32 data_size) {
	if(data_size > 4) {
		WRITE_HEAD(REX_W); // 
	}
	
	write_to_register(rm, number, data_size);

	WRITE_HEAD(0x2B);
	WRITE_HEAD(0xC0 | (reg << 3) | rm);

}


void write_add_register_rm(sh_register reg, sh_register rm, char *number, i32 data_size) {
	if(data_size > 4) {
		WRITE_HEAD(REX_W); // 
	}
	
	write_to_register(rm, number, data_size);

	WRITE_HEAD(0x80);
	WRITE_HEAD(0xC0 | (reg << 3) | rm);

}

void write_push_stack_register(sh_register reg) {
	u8 encode_reg = 0;

	if(reg >= R8) {
		WRITE_HEAD(0x40 | 0b0001);
		reg &= (0x7);
	}

	WRITE_HEAD(0x50 | reg);
}

void write_pop_stack_register(sh_register reg) {
	u8 encode_reg = 0;

	if(reg >= R8) {
		WRITE_HEAD(0x40 | 0b0001);
		reg &= (0x7);
	}

	WRITE_HEAD(0x58 | reg);
}

char* write_data_section(char *data_to_write, i32 bytes) {
	char* data_ptr = data_section_header;

	for(int i = 0; i < bytes; i++) {
		data_section_header[0] = data_to_write[i];
		data_section_header++;
	}

	return data_ptr;
}

void setup_system() {
	// set stack registers
	// ips ptr goes to main_mem
}

void make_stack_size(i32 bytes) {
	WRITE_HEAD(REX_W);
	WRITE_HEAD(0x83);
	WRITE_HEAD(0b11101000|RSP);

	char *where = (char*)&bytes;
	WRITE_HEAD(where[0]);
}

void reduce_stack_size(i32 bytes) {
	WRITE_HEAD(REX_W);
	WRITE_HEAD(0x83);
	WRITE_HEAD(0b11000000|RSP);

	char *where = (char*)&bytes;
	WRITE_HEAD(where[0]);
}


// does mov? 
void write_to_register(sh_register reg, char* data_to_write, i32 data_size) {

	if(data_size > 4) {
		WRITE_HEAD(REX_W); // 
	}
	WRITE_HEAD(0xb8 + reg);
	for(int i = 0; i < data_size; i++) {
		WRITE_HEAD(data_to_write[i]);
	}

}

sh_op_operand write_add_op(sh_op_dst result_dest, sh_op_operand left_op, sh_op_operand right_op) {

	i8 encode_mod_rm = 0; // 
	i8 op_code = 0;

	// should fix imm + reg => reg + imm
	/* if(left_op.type == SH_SRC_IMMEDIATE && right_op.type != SH_SRC_IMMEDIATE) { */
	/* 	sh_op_operand t = left_op; */
	/* 	left_op = right_op; */
	/* 	right_op = t; */
	/* } */

	// we should mov to the result at the end? 
	switch(left_op.type) {
		case SH_SRC_REGISTER: {

			// adding to reg from ==
			encode_mod_rm = 0b11000000;
			encode_mod_rm |=  result_dest.reg << 3 | right_op.reg;

			switch(right_op.type) {
				case SH_SRC_IMMEDIATE: {

					WRITE_HEAD(REX_W);
					WRITE_HEAD(0x81);

					encode_mod_rm = 0b11000000;
					encode_mod_rm |=  left_op.reg;
					WRITE_HEAD(encode_mod_rm);

					char *ptr = (char*)&right_op.imm_val;
					for(int i = 0; i < 4; i++) {
						WRITE_HEAD(ptr[i]); //one byte rex one byte op 
					}

					write_mov(result_dest, left_op);

				} break;

				case SH_SRC_REGISTER: {

					WRITE_HEAD(REX_W);
					WRITE_HEAD(0x01);
					encode_mod_rm = 0b11000000 | left_op.reg << 3 | right_op.reg;
					WRITE_HEAD(encode_mod_rm);

					/* write_mov(result_dest, left_op); */

				} break;


				case SH_SRC_MEMORY: {

					printf("%s %s %s\n", get_reg_name_op(result_dest), get_reg_name_op(left_op), get_reg_name_op(right_op));
					write_to_register(right_op.reg, (char*)&right_op.mem_address, 8);

					WRITE_HEAD(REX_W);
					WRITE_HEAD(0x03);
					WRITE_HEAD(0b0000000 | left_op.reg << 3 | right_op.reg);

					/* write_mov(result_dest, left_op); */

				} break;

			}

		} break;
		case SH_SRC_MEMORY: {

			switch(right_op.type) {
				case SH_SRC_IMMEDIATE: {

					sh_op_operand temp_left = sh_new_reg_location(left_op.reg);
					write_mov(temp_left, left_op);

					WRITE_HEAD(REX_W);
					WRITE_HEAD(0x81);
					encode_mod_rm = 0b11000000;
					encode_mod_rm |=  left_op.reg;
					WRITE_HEAD(encode_mod_rm);

					char *ptr = (char*)&right_op.imm_val;
					for(int i = 0; i < 4; i++) {
						WRITE_HEAD(ptr[i]); //one byte rex one byte op 
					}

					/* return temp_left; */
					write_mov(result_dest, temp_left);

				} break;

				case SH_SRC_REGISTER: {

					WRITE_HEAD(REX_W);
					WRITE_HEAD(0x01);
					encode_mod_rm = 0b11000000 | right_op.reg << 3 | left_op.reg;
					WRITE_HEAD(encode_mod_rm);

					write_mov(result_dest, left_op);

				} break;


				case SH_SRC_MEMORY: {
					write_mov(sh_new_reg_location(left_op.reg), right_op);
					write_to_register(right_op.reg, (char*)&right_op.mem_address, 8);

					WRITE_HEAD(REX_W);
					WRITE_HEAD(0x03);
					WRITE_HEAD(0b00000000 | left_op.reg << 3 | right_op.reg);

					write_mov(result_dest, sh_new_reg_location(left_op.reg));

				} break;
			}



		} break;
		case SH_SRC_IMMEDIATE: {

			switch(right_op.type) {
				case SH_SRC_IMMEDIATE: {

					write_to_register(left_op.reg, (char*)&left_op.imm_val, 8);

					op_code = 0x81;

					encode_mod_rm = 0b11000000;
					encode_mod_rm |=  left_op.reg;

					WRITE_HEAD(REX_W);
					WRITE_HEAD(op_code);
					WRITE_HEAD(encode_mod_rm);

					char *ptr = (char*)&right_op.imm_val;
					for(int i = 0; i < 4; i++) {
						WRITE_HEAD(ptr[i]); //one byte rex one byte op 
					}

					printf("%s %s %s\n", get_reg_name_op(result_dest), get_reg_name_op(left_op), get_reg_name_op(right_op));
					write_mov(result_dest, sh_new_reg_location(left_op.reg));

				} break;

				case SH_SRC_REGISTER: {

				} break;
				
				/* case SH_SRC_REGISTER: { */
                /*  */
				/* 	put */
				/* 	op_code = 0x81; */
                /*  */
				/* 	encode_mod_rm = 0b11000000; */
				/* 	encode_mod_rm |=  left_op.reg; */
                /*  */
				/* 	char *ptr = (char*)&right_op.imm_val; */
				/* 	for(int i = 0; i < 4; i++) { */
				/* 		WRITE_HEAD_REL(ptr[i], 3 + i); //one byte rex one byte op  */
				/* 	} */
                /*  */
				/* 	write_mov(result_dest, left_op); */
                /*  */
				/* } break; */

				default: {
					puts("case not handled");
				} break;

			}

		} break;

	}

	


	return result_dest;
}


sh_op_operand write_sub_op(sh_op_dst result_dest, sh_op_operand right_op) {

	i8 encode_mod_rm = 0; // 
	i8 op_code = 0;

	switch(result_dest.type) {

		case SH_SRC_REGISTER: { // R (M|I)
			op_code = 0x2B;

			switch(right_op.type) {
				case SH_SRC_REGISTER: {
					encode_mod_rm = 0b11000000;
					encode_mod_rm |=  result_dest.reg << 3 | right_op.reg;
				} break;
				case SH_SRC_MEMORY: {
					encode_mod_rm = 0b00000000;
					encode_mod_rm |=  result_dest.reg << 3 | right_op.reg;
				} break;
				case SH_SRC_IMMEDIATE: {
					op_code = 0x81;

					encode_mod_rm = 0b11101000;
					encode_mod_rm |=  result_dest.reg;

					char *ptr = (char*)&right_op.imm_val;
					for(int i = 0; i < 4; i++) {
						WRITE_HEAD_REL(ptr[i], 3 + i); //one byte rex one byte op 
					}

				} break;

			}

		} break;

		// we are moving adding to [mem], mem address in reg, mod bits = 00, reg addr in rm field
		case SH_SRC_MEMORY: { // M (R|I)

			op_code = 0x29;

			switch(right_op.type) {
				case SH_SRC_REGISTER: {
					encode_mod_rm = 0b00000000;
					encode_mod_rm |=  right_op.reg << 3 | result_dest.reg;
				} break;
				case SH_SRC_MEMORY: {
					assert_exit(result_dest.type != SH_SRC_MEMORY, "subtraction is not supported from mem to mem.");
				} break;
				case SH_SRC_IMMEDIATE: {
					op_code = 0x81;
					encode_mod_rm = 0b00101000;
					encode_mod_rm |=  result_dest.reg;

					char *ptr = (char*)&right_op.imm_val;
					for(int i = 0; i < 4; i++) {
						WRITE_HEAD_REL(ptr[i], 3 + i); //one byte rex one byte op 
					}
				} break;

			}

		} break;

	}


	WRITE_HEAD(REX_W);
	WRITE_HEAD(op_code);
	WRITE_HEAD(encode_mod_rm);
	if(right_op.type == SH_SRC_IMMEDIATE || result_dest.type == SH_SRC_IMMEDIATE) {
		writer_head += 4;
	}

	return result_dest;
}


char* get_reg_name(sh_register reg) {
	return register_names[reg];
}


char* get_reg_name_op(sh_op_operand reg) {
	return register_names[reg.reg];
}


sh_op_operand write_mul_op(sh_op_dst result_dest, sh_op_operand left_op, sh_op_operand right_op) {

	i8 encode_mod_rm = 0; // 
	i8 op_code = 0;

	printf("%s %s %s\n", op_source_names[result_dest.type], op_source_names[left_op.type], op_source_names[right_op.type]);

	switch(left_op.type) {
		case SH_SRC_IMMEDIATE: { //everything is 3 OP
			switch(right_op.type) {

				case SH_SRC_MEMORY: {

					write_to_register(right_op.reg, (char*)&left_op.mem_address, 8);

					encode_mod_rm = 0b00000000;
					encode_mod_rm |=  result_dest.reg << 3 | right_op.reg;
					
					WRITE_HEAD(REX_W);
					WRITE_HEAD(op_code);
					WRITE_HEAD(encode_mod_rm);

					char *ptr = (char*)&right_op.imm_val;
					for(int i = 0; i < 4; i++) {
						WRITE_HEAD(ptr[i]); //one byte rex one byte op 
					}

					//TODO double check if this is actually needed or not? 
					/* write_mov(result_dest, sh_new_reg_location(right_op.reg)); */
				} break;

				case SH_SRC_IMMEDIATE: {

					write_to_register(left_op.reg, (char*)&left_op.imm_val, 8);

					encode_mod_rm = 0b11000000;
					encode_mod_rm |=  result_dest.reg << 3 | left_op.reg;

					WRITE_HEAD(REX_W);
					/* WRITE_HEAD(0x0F); */
					WRITE_HEAD(0x69);
					WRITE_HEAD(encode_mod_rm);

					char *ptr = (char*)&right_op.imm_val;
					for(int i = 0; i < 4; i++) {
						WRITE_HEAD(ptr[i]); //one byte rex one byte op 
					}


					/* encode_mod_rm = 0b11000000; */
					/* encode_mod_rm |=  result_dest.reg << 3 | result_dest.reg; */


					/* write_mov(sh_new_reg_location(result_dest.reg), sh_new_reg_location(left_op.reg)); */

				} break;


				case SH_SRC_REGISTER: {

					write_to_register(left_op.reg, (char*)&left_op.imm_val, 8);

					printf("what %s %s %s\n", get_reg_name_op(result_dest), get_reg_name_op(left_op), get_reg_name_op(right_op));
					encode_mod_rm = 0b11000000;
					encode_mod_rm |=  left_op.reg << 3 | right_op.reg;

					WRITE_HEAD(REX_W);
					WRITE_HEAD(0x0f);
					WRITE_HEAD(0xAf);
					WRITE_HEAD(encode_mod_rm);

					/* char *ptr = (char*)&left_op.imm_val; */
					/* for(int i = 0; i < 4; i++) { */
					/* 	WRITE_HEAD(ptr[i]); //one byte rex one byte op  */
					/* } */


					write_mov(result_dest, sh_new_reg_location(left_op.reg));

				} break;

			}

			
		
			
		} break;


		case SH_SRC_MEMORY: {
			
			switch(right_op.type) {

				case SH_SRC_REGISTER: {

					write_mov(sh_new_reg_location(left_op.reg), left_op);

					encode_mod_rm = 0b11000000;
					encode_mod_rm |=  left_op.reg << 3 | right_op.reg;

					WRITE_HEAD(REX_W);
					WRITE_HEAD(0x0F);
					WRITE_HEAD(0xAF);
					WRITE_HEAD(encode_mod_rm);

					write_mov(sh_new_reg_location(result_dest.reg), sh_new_reg_location(left_op.reg));


				} break;

				case SH_SRC_MEMORY: { // left and righ mem are memory

					write_mov(sh_new_reg_location(left_op.reg), left_op);
					write_to_register(right_op.reg, (char*)&right_op.mem_address, 8);

					encode_mod_rm = 0b00000000;
					encode_mod_rm |=  left_op.reg << 3 | right_op.reg;

					WRITE_HEAD(REX_W);
					WRITE_HEAD(0x0F);
					WRITE_HEAD(0xAF);
					WRITE_HEAD(encode_mod_rm);

					write_mov(sh_new_reg_location(result_dest.reg), sh_new_reg_location(left_op.reg));

				} break;

				case SH_SRC_IMMEDIATE: {

					write_to_register(left_op.reg, (char*)&left_op.mem_address, 8);

					/* printf("what %s %s %s\n", get_reg_name_op(result_dest), get_reg_name_op(left_op), get_reg_name_op(right_op)); */
					encode_mod_rm = 0b00000000;
					encode_mod_rm |=  left_op.reg << 3 | left_op.reg;

					WRITE_HEAD(REX_W);
					WRITE_HEAD(0x69);
					WRITE_HEAD(encode_mod_rm);

					char *ptr = (char*)&right_op.imm_val;
					for(int i = 0; i < 4; i++) {
						WRITE_HEAD(ptr[i]); //one byte rex one byte op 
					}


					write_mov(result_dest, sh_new_reg_location(left_op.reg));

				} break;
			}

		} break;


		case SH_SRC_REGISTER: {

			switch(right_op.type) {

				case SH_SRC_REGISTER: {

					WRITE_HEAD(REX_W);
					WRITE_HEAD(0x0f);
					WRITE_HEAD(0xAF);
					/* if(result_dest.reg == RAX) { */
					/* 	WRITE_HEAD(0b11000000 | result_dest.reg << 3 | right_op.reg); */
					/* } else { */
						WRITE_HEAD(0b11000000 | right_op.reg << 3 | left_op.reg);
					/* } */

				} break;

				case SH_SRC_MEMORY: {

					write_to_register(right_op.reg, (char*)&right_op.mem_address, 8);

					encode_mod_rm = 0b00000000;
					encode_mod_rm |=  result_dest.reg << 3 | right_op.reg;

					WRITE_HEAD(REX_W);
					WRITE_HEAD(0x0f);
					WRITE_HEAD(0xAF);

					WRITE_HEAD(encode_mod_rm);

					//TODO double check if this is actually needed or not? 
					/* write_mov(result_dest, sh_new_reg_location(left_op.reg)); */
					write_mov(sh_new_reg_location(result_dest.reg), sh_new_reg_location(left_op.reg));
				} break;

				case SH_SRC_IMMEDIATE: {
					// maybe this is needed? 

					write_to_register(left_op.reg, (char*)&right_op.imm_val, 8);

					encode_mod_rm = 0b11000000;
					encode_mod_rm |=  result_dest.reg << 3 | left_op.reg;

					WRITE_HEAD(REX_W);
					WRITE_HEAD(0x0f);
					WRITE_HEAD(0xAF);
					WRITE_HEAD(encode_mod_rm);

					/* char *ptr = (char*)&right_op.imm_val; */
					/* for(int i = 0; i < 4; i++) { */
					/* 	WRITE_HEAD(ptr[i]); //one byte rex one byte op  */
					/* } */


					/* write_mov(sh_new_reg_location(result_dest.reg), sh_new_reg_location(left_op.reg)); */

				} break;
			}





		} break;

		default: {}
	}


	return result_dest;
}




sh_op_operand write_operation(sh_op_type op, sh_op_dst result_dest, sh_op_operand left_op, sh_op_operand right_op) {


	printf("%s[%s] = %s[%s] %s %s[%s]\n",
			op_source_names[result_dest.type],
			get_reg_name(result_dest.reg),
			op_source_names[left_op.type],
			get_reg_name(left_op.reg),
			op_type_name[op],
			op_source_names[right_op.type],
			get_reg_name(right_op.reg)
	);

	/* if(result_dest.type == SH_SRC_REGISTER) { */
	/* 	if(left_op.type == SH_SRC_MEMORY && result_dest.reg == left_op.reg) { */
	/* 		left_op.reg = choose_register_keep_both(result_dest.reg, right_op.reg); */
	/* 	} */
    /*  */
	/* 	if(right_op.type == SH_SRC_MEMORY && result_dest.reg == right_op.reg) { */
	/* 		right_op.reg = choose_register_keep_both(result_dest.reg, left_op.reg); */
	/* 	} */
	/* } */

	// @Note: needs more consideration 
	// if we move left op to result register, we might break what is inside the register
	// 	we should only move if we know righ op is not a register == result reg
	
	switch(op) {
		case SH_ADD_OP: {
			/* if(result_dest.type == SH_SRC_REGISTER && right_op.type != SH_SRC_REGISTER) { */
			/* 	write_mov(result_dest, left_op); */
			/* } */

			return write_add_op(result_dest, left_op, right_op);
		} break;

		case SH_SUB_OP: {
			return write_sub_op(result_dest, right_op);
		} break;

		case SH_MUL_OP: {
			return write_mul_op(result_dest, left_op, right_op);
		} break;
	}

}


void write_call(const char* call_address) {
	write_to_register(RAX, (char*)&data_section, 8);
	WRITE_HEAD(0xFF);
	WRITE_HEAD(0b00010000);
}


void write_printf(const char* str) {
	make_stack_size(64);
	write_to_register(RCX, (char*)&str, 8);
	write_call(location_of_printf);
	reduce_stack_size(64);
}


u64 sh_allocate_mem_stack(i32 bytes) {

	return (u64)0;
}


u64 sh_allocate_memory_main(i32 bytes) {
	char *addr = data_section_header;
	data_section_header += bytes;
	return (u64)addr;
}

u64 sh_allocate_memory_stack(i32 bytes) {
	// something something move stack ptr
	char *addr = data_section_header;
	data_section_header += bytes;
	return (u64)addr;
}




void memory_setup() {
	SYSTEM_INFO sys_inf;
	GetSystemInfo(&sys_inf);

	i32 page_count = 1;
	i32 page_in_bytes = page_count*sys_inf.dwPageSize;

	main_mem = (char*) VirtualAlloc(NULL, page_in_bytes, MEM_COMMIT | MEM_RESERVE, PAGE_EXECUTE_READWRITE);

	FillMemory((void*)main_mem, page_in_bytes, 0xC3);

	writer_head = main_mem;
	end_of_mem = main_mem + page_in_bytes;
	stack_base = end_of_mem; //  stack starts at the end of mem, goes "down" in address 
	stack_top = end_of_mem; //  stack starts at the end of mem, goes "down" in address 
	location_of_printf = (char*)printf;
	data_section = stack_base - stack_size - data_section_size;
	data_section_header = data_section;
	location_of_printf = (char*)printf;

	write_data_section((char*)&location_of_printf, 8);

}



//
void sh_gen_decl(sh_decl *decl);
void sh_gen_var_decl(sh_decl *decl);
sh_op_operand sh_gen_expr(sh_expression *expr, sh_register store_result);



void sh_gen_var_decl(sh_decl *decl) {

	i32 local_variable = 0;
	if(decl->mem_info == NULL) {
		decl->mem_info = (sh_memory_info *)calloc(1, sizeof(sh_memory_info));
	}

	if(local_variable) {
		decl->mem_info->mem_address = sh_allocate_memory_stack(decl->total_size); // is it a local var? gen stack
	} else {
		decl->mem_info->mem_address = sh_allocate_memory_main(decl->total_size); // is it a local var? gen stack
	}

	sh_op_operand init = {0};
	if(decl->var.init_expr) {
		init = sh_gen_expr(decl->var.init_expr, RAX);
	}

	sh_register move_via = choose_register(RAX);
	write_mov(sh_new_mem_location_reg((u64)decl->mem_info->mem_address, move_via), init);

}

sh_op_operand sh_gen_id_expr(sh_expression *id_expr, sh_register store_result) {

	if(id_expr->var_decl->mem_info->mem_type == SH_SRC_STACK) {
		return sh_new_stack_location(id_expr->var_decl->mem_info->stack_rel);

	}

	return sh_new_mem_location_reg(id_expr->var_decl->mem_info->mem_address, store_result);
}

sh_op_operand sh_gen_expr(sh_expression *expr, sh_register store_result) {
	switch(expr->type) {
		case SH_INT_LITERAL:
			write_to_register(store_result, (char*)&expr->vi64, 8);
			return sh_new_reg_location(store_result);
			/* return sh_new_ir_imm_operand(expr->vi64, store_result); */
			break;

		case SH_ID_EXPR: {
			/* sh_op_operand reg = sh_new_reg_location(store_result); */
			/* write_mov(reg, ); */
			return sh_gen_id_expr(expr, store_result);
		} break;

		case SH_OPERATOR_EXPR: {


			sh_register right_op_store = choose_register(store_result);

			sh_op_operand right_op = sh_gen_expr(expr->right_op, right_op_store);

			sh_register left_op_store = choose_register(right_op_store);

			sh_op_operand left_op = sh_gen_expr(expr->left_op, left_op_store);

			sh_op_dst dst = sh_new_reg_location(store_result);

			// if they are the same register, they must be stored inside another register
			/* if(left_op.type == right_op.type && left_op.reg == right_op.reg) { */
			/* 	dst.reg = choose_register_keep_both(left_op.reg, right_op.reg); */
			/* } */

			write_operation(sh_convert_expr_op(expr->op), dst, left_op, right_op);
			return dst;
		} break;

	}

}

void sh_gen_statement(sh_statement *stmt) {

	switch(stmt->type) {
		case SH_COMPOUND_STATEMENT: {
			sh_statement **c = stmt->statements;
			for(i32 i = 0; i < buf_len(c); i++) {
				sh_gen_statement(c[0]);
			}
		} break;

		case SH_VAR_DECL_STATEMENT: {

		} break;

	}
}

void sh_gen_func_decl(sh_decl *decl) {
	assert_exit(decl->type == SH_FUNC_DECL, "not a func decl");

	// sh_make_space for local vars
	sh_gen_statement(decl->func.compound_statement);

}


void sh_gen_decl(sh_decl *decl) {
	switch(decl->type) {
		case SH_VAR_DECL:
			sh_gen_var_decl(decl);
			break;
		case SH_FUNC_DECL:
			sh_gen_func_decl(decl);
			break;

	}
}


void testing_op() {


	//  multiplay memory location mem1 and mem2
	// mov value of [mem2] to some reg1

	// mov reg1  mem1_addr
	// mov reg1, [reg1] //reg 1 has value of [mem1]
	// mov reg2, mem2
	// mov , reg1
	/* write_mov(sh_new_mem_location_reg(0x0, RAX), sh_new_mem_location_reg(0x2, RAX)); */

	write_operation(SH_MUL_OP,
			sh_new_mem_location_reg(0x0, RAX),
			sh_new_mem_location_reg(0x0, RAX),
			sh_new_mem_location_reg(0x1, RBX)
	);
}

void gen_main() {

	memory_setup();

#if 0
	testing_op();

#else
	for(sh_decl **d = decls;d != buf_end(decls); d++) {
		sh_gen_decl(*d);
	}
#endif

	rand_func f = (rand_func) main_mem;

	i32 main_mem_len = (i32)( writer_head - main_mem );

	i32 len_acc = 0;
	puts("================gen code");
	while(main_mem_len > 0) {
		i32 length = print_gen_code(main_mem + len_acc);
		main_mem_len -= length;
		len_acc += length;
	}
	puts("=================end gen code");

	__try {
		/* f(); */
	}
	__except(EXCEPTION_EXECUTE_HANDLER) {
		printf("yeet");
	}


}
