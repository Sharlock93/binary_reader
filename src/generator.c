typedef f64 (*rand_func)();

typedef sh_op_source_type sh_memory_type;

struct sh_memory_info {
	sh_memory_type mem_type;
	i32 mem_size;
	sh_register reg;
	union {
		u64 mem_address;
		i32 stack_rel;
	};
};

typedef enum sh_opcode_prefix {
	REX_W = 0b01001000,
	REX_B = 0b01000001,
	REX_R = 0b01000100,
} sh_opcode_prefix;

u8* main_mem = NULL;
u8* end_of_mem = NULL;

char* location_of_printf = NULL;


u8* writer_head = NULL; // where are we writing code gen
char* stack_base = NULL; // where the stack starts
char* stack_top = NULL; // where it is now
char* data_section = NULL;
char* data_section_header = NULL;
char* main_func_address = NULL;

i32 stack_size = 1024;
i32 data_section_size = 1024;
i32 code_section_size = 1024;
i32 stack_offset = 0;




#define WRITE_HEAD(val) (writer_head[0] = (val), writer_head++)
#define WRITE_HEAD_REL(val, rel) (writer_head[rel] = (val))
#define CLIP_REG(reg) if(reg >= R8 && reg < NO_REG) reg &= 0x7;


void write_to_register(sh_register reg, char* data_to_write, i32 data_size);
void sh_gen_statement(sh_decl *decl, sh_statement *stmt, i32 *stack_relative);
void generate_func_end(sh_decl* decl, u64 mem_address, i32 func_size);
void write_mov(sh_op_operand dst, sh_op_operand src);
void write_mov_xmm(sh_op_operand dst, sh_op_operand src);
void write_jmp();
char* get_reg_name_op(sh_op_operand reg) { return register_names[reg.reg]; }
char* get_reg_name(sh_register reg) { return register_names[reg]; }
sh_op_operand sh_gen_expr_decl(sh_expression *expr, sh_register store_result, sh_decl *decl);
sh_op_operand sh_gen_expr(sh_expression *expr, sh_register store_result);

i8 is_expr_id(sh_expression *e) { return e->type == SH_ID_EXPR; }
i8 is_expr_ptr(sh_expression *e) { return is_expr_id(e) && is_typespec_ptr(e->var_decl->var.type); }
i8 is_expr_int_type(sh_expression *e) { return is_expr_id(e) && is_typespec_int_type(e->var_decl->var.type) || e->type == SH_INT_LITERAL; }
i8 is_expr_float_type(sh_expression *e) { return is_expr_id(e) && is_typespec_float_type(e->var_decl->var.type) || e->type == SH_FLOAT_LITERAL; }
i8 is_expr_add_or_sub(sh_expression *e) { return e->op == SH_PLUS || e->op == SH_MINUS; }

i32 get_expr_type_size(sh_expression *e) {
	i32 size = 1;

	if(is_expr_id(e)) {
		sh_typespec *t = e->var_decl->var.type;

		if(is_typespec_ptr(t) || is_typespec_array(t)) {
			size = t->base->size_byte;
		} else if(is_typespec_defined_type(t)) {
			size = t->size_byte;
		}
	}

	return size;
}

sh_decl_tag* sh_get_dll_import_tag(sh_decl *d) {
	sh_decl_tag **t = d->tags;
	while(t != buf_end(d->tags)) {
		if(t[0]->type == SH_DLL_IMPORT_TAG) return t[0];
		t++;
	}

	return NULL;
}

void import_dll_func(sh_decl *d) {

	//we should cache loading the modules
	sh_decl_tag *dll = sh_get_dll_import_tag(d);

	char *dll_path = dll->dll.dll_file_path;
	char *func_name = dll->dll.func_name;

	HMODULE dll_module = LoadLibrary(dll_path);

	printf("%s\n", dll_path);
	assert_exit(dll_module != NULL, "DLL module %s doesn't exist, %p", dll_path);

	void *func_address = GetProcAddress(dll_module, func_name);

	assert_exit(
			func_address != NULL,
			"func name %s doesn't exist in module %s.",
			func_name, dll_path
	);
	
	d->mem_info->mem_address = (i64)func_address;
	/* FreeLibrary(dll_module); */

}

void write_push_stack_register(sh_register reg) {
	u8 encode_reg = 0;

	if(reg >= R8) {
		WRITE_HEAD(0x40 | 0b0001);
		reg &= (0x7);
	}

	WRITE_HEAD(0x50 + reg);
}

void write_pop_stack_register(sh_register reg) {
	u8 encode_reg = 0;

	if(reg >= R8) {
		WRITE_HEAD(0x40 | 0b0001);
		reg &= (0x7);
	}

	WRITE_HEAD(0x58 + reg);
}

char* write_data_section(char *data_to_write, i32 bytes) {
	char* data_ptr = data_section_header;

	for(int i = 0; i < bytes; i++) {
		data_section_header[0] = data_to_write[i];
		data_section_header++;
	}

	return data_ptr;
}


// returns a register that is different from reg1 
sh_register choose_register(sh_register reg1) {
	//@Todo: this is just a ++ and a few checks.
	sh_register re = reg1;

	if(re == RBX) re += 3;
	else re++;
	
	return re;
}


u8 gen_prefix(sh_op_operand left, sh_op_operand right) {
	u8 prefix = 0;

	i32 mem_size = left.mem_size;

	if(mem_size > 4) {
		prefix = REX_W;
	}


	if(left.reg >= R8 && left.reg != NO_REG) {
		prefix |= REX_B;
	}

	if(right.reg >= R8 && right.reg != NO_REG) {
		prefix |= REX_R;
	}
	
	return prefix;
}

u8 gen_sib(sh_op_operand op) { // based on only one operand? 
	u8 sib = 0;

	sib = sib | op.scale << 6 | op.base;
	if(op.index == NO_REG) {
		sib |= 0b00100000;
	} else {
		sib |= op.index << 3;
	}

	return sib;
}

void make_stack_size(i32 bytes) {
	WRITE_HEAD(REX_W);
	WRITE_HEAD(0x83);
	char *where = (char*)&bytes;
	WRITE_HEAD(where[0]);
}

void reduce_stack_size(i32 bytes) {
	WRITE_HEAD(REX_W);
	WRITE_HEAD(0x83);
	char *where = (char*)&bytes;
	WRITE_HEAD(where[0]);
}


u64 sh_allocate_memory_main(i32 bytes) {
	char *addr = data_section_header;
	data_section_header += bytes;
	return (u64)addr;
}

i8 is_mem(sh_op_operand op) { return op.type == SH_SRC_MEMORY; }
i8 is_reg(sh_op_operand op) { return op.type == SH_SRC_REGISTER; }
i8 is_imm(sh_op_operand op) { return op.type == SH_SRC_IMMEDIATE || op.type == SH_SRC_FLOAT_IMMEDIATE; }
i8 is_mem_sib( sh_op_operand op) { return op.type == SH_SRC_MEM_SIB; }
i8 is_mem_or_sib( sh_op_operand op) { return op.type == SH_SRC_MEM_SIB || op.type == SH_SRC_MEMORY; }

i8 is_xmm(sh_op_operand op) { return op.reg_type == SH_XMM_REGISTER; }


i32 is_operand_equal(sh_op_operand a, sh_op_operand b) {

	// reg reg are always equal, you can't move across different sizes
	if(is_reg(a) && is_reg(b) && a.reg == b.reg && a.reg_type == b.reg_type) return 1;

	return a.type == b.type &&
		a.mem_address == b.mem_address &&
		a.mem_size == b.mem_size &&
		a.reg_type == b.reg_type &&
		a.reg == b.reg &&
		a.index == b.index &&
		a.base == b.base  && 
		a.scale == b.scale &&
		a.imm_val == b.imm_val;
}

sh_op_operand sh_new_operand_mem_info(sh_memory_info *mem_info) {
	sh_op_operand op = {0};

	switch(mem_info->mem_type) {
		case SH_SRC_MEM_SIB: { op = sh_new_stack_location(mem_info->stack_rel, mem_info->mem_size, mem_info->reg); } break;
		case SH_SRC_MEMORY: { op =  sh_new_mem_location_reg(mem_info->mem_address, mem_info->reg, mem_info->mem_size); } break;
		default: { assert_exit(false, "Unknown memory info type %s\n", op_source_names[mem_info->mem_type]); } break;
	}

	return op;
}

sh_op_operand load_addr_to_reg(sh_op_operand op) {

	sh_op_operand mov_to_reg = sh_new_reg_location(op.reg, op.mem_size);
	sh_op_operand src_addr_imm = sh_new_ir_imm_operand(op.mem_address, op.reg, op.mem_size);

	if(is_xmm(op)) {
		write_mov_xmm(mov_to_reg, src_addr_imm);
		mov_to_reg.reg_type = SH_XMM_REGISTER;
	} else {
		write_mov(mov_to_reg, src_addr_imm);
	}


	return op;
}

sh_op_operand load_imm_to_reg(sh_op_operand op) {
	sh_op_operand mov_to_reg = {0};

	if(op.type == SH_SRC_FLOAT_IMMEDIATE) {
		i64 address = (i64)write_data_section((char*)&op.imm_valf, 8);
		mov_to_reg = sh_new_reg_location(op.reg, op.mem_size);
		sh_op_operand src_addr_imm = sh_new_mem_location_reg(address, op.reg, op.mem_size);
		write_mov_xmm(mov_to_reg, src_addr_imm);
		mov_to_reg.reg_type = SH_XMM_REGISTER;
	} else {
		mov_to_reg = sh_new_reg_location(op.reg, op.mem_size);
		sh_op_operand src_addr_imm = sh_new_ir_imm_operand(op.imm_val, op.reg, op.mem_size);
		write_mov(mov_to_reg, src_addr_imm);
	}

	return mov_to_reg;
}


void convert_double_to_single_float(sh_op_operand op) {
	WRITE_HEAD(0xF2);
	WRITE_HEAD(0x0F);
	WRITE_HEAD(0x5A);
	WRITE_HEAD(0b11000000 | op.reg << 3 | op.reg);

}

void convert_single_to_double_float(sh_op_operand op) {
	WRITE_HEAD(0xF2);
	WRITE_HEAD(0x0F);
	WRITE_HEAD(0x5A);
	WRITE_HEAD(0b11000000 | op.reg << 3 | op.reg);

}



typedef enum MODRM {
	MODRM_REG_MEM = 0b00000000,
	MODRM_REG_REG = 0b11000000,
	MODRM_REG_IMM = 0b00000000,
	MODRM_REG_SIB = 0b00000100,
	MODRM_REG_SIB_DISP8 = 0b01000100,
	MODRM_REG_SIB_DIPS32 = 0b10000100,
} MODRM;


void write_mov_xmm(sh_op_operand dst, sh_op_operand src) {
	// all regs are xmm(0 - 15) ?

	if(is_operand_equal(dst, src)) {
		puts("regs equal");
		return;
	}
		// both operands are mem so load one to a register

	if(is_mem_or_sib(dst) && is_mem_or_sib(src) ) {
		write_mov_xmm(sh_new_reg_location(src.reg, src.mem_size), src);
	}

	if(is_imm(src)) {
		src = load_imm_to_reg(src);

		if(dst.mem_size < 8) {
			convert_double_to_single_float(src);
		}

	}

	u8 mov_prefix = 0xF3;
	u8 mov_op = 0;
	u8 modrm = 0;
	u8 sib = 0;
	char *v = NULL;
	i32 write_size = 0;
	sh_register l_reg = dst.reg;
	sh_register r_reg = src.reg;

	CLIP_REG(l_reg);
	CLIP_REG(r_reg);

	if(is_reg(dst) && !is_imm(src)) {

		mov_op = 0x10;

		if(src.mem_size > 4) {
			mov_prefix = 0xF2;
		}

		modrm = MODRM_REG_REG;


		if(is_mem_or_sib(src)) {
			// @Note: @ addr 0 would be wrong (also very illegal to access most likely)
			modrm = MODRM_REG_MEM;

			if(src.mem_address) {
				load_addr_to_reg(src);
			}


			if(is_mem_sib(src)) {

				sib = gen_sib(src);

				if(src.displacement > 0) {
					modrm = MODRM_REG_SIB_DIPS32;
					v = (char*)&src.displacement;
					write_size = 4;
				} else {
					modrm = MODRM_REG_SIB;
				}

				r_reg = src.base;
			}
		}

		modrm |= l_reg << 3 | r_reg;

	} else {

		mov_op = 0x11;

		if(dst.mem_size > 4) {
			mov_prefix = 0xF2;
		}

		if(dst.mem_address) {
			load_addr_to_reg(dst);
		}

		modrm = MODRM_REG_MEM;

		if(is_mem_sib(dst)) {
			sib = gen_sib(dst);

			if(dst.displacement > 0) {
				modrm = MODRM_REG_SIB_DIPS32;
				v = (char*)&dst.displacement;
				write_size = 4; // displacement cannot be over 4 bytes
			} else  { modrm = MODRM_REG_SIB; }

			l_reg = dst.base;
		} 

		modrm |= l_reg | r_reg << 3;
	}



	




	WRITE_HEAD(mov_prefix);
	
	WRITE_HEAD(0x0F);
	WRITE_HEAD(mov_op);

	//@Todo: immedate and stack needs a lot of bytes
	if(!is_imm(src)) WRITE_HEAD(modrm);
	if(is_mem_sib(src) || is_mem_sib(dst)) WRITE_HEAD(sib);
	
	for(i32 i = 0; v != NULL && i < write_size; i++) {
		WRITE_HEAD(v[i]);
	}


}

void write_mov(sh_op_operand dst, sh_op_operand src) {

	if(is_operand_equal(dst, src)) return;
		// both operands are mem so load one to a register
	if(is_mem_or_sib(dst) && is_mem_or_sib(src) ) {
		write_mov(sh_new_reg_location(src.reg, src.mem_size), src);
	}

	if(!is_reg(dst) && is_imm(src) && src.mem_size > 4) {
		src = load_imm_to_reg(src);
	}

	u8 prefix = gen_prefix(dst, src);
	u8 mov_op = 0;
	u8 modrm = 0;
	u8 sib = 0;
	char *v = NULL;
	i32 write_size = 0;
	sh_register l_reg = dst.reg;
	sh_register r_reg = src.reg;

	CLIP_REG(l_reg);
	CLIP_REG(r_reg);

	if(is_reg(dst) && !is_imm(src)) {

		mov_op = 0x8B;
		modrm = MODRM_REG_REG;

		if(is_mem_or_sib(src)) {
			// @Note: @ addr 0 would be wrong (also very illegal to access most likely)
			modrm = MODRM_REG_MEM;

			if(src.mem_address) {
				load_addr_to_reg(src);
			}


			if(is_mem_sib(src)) {

				sib = gen_sib(src);

				if(src.displacement > 0) {
					modrm = MODRM_REG_SIB_DIPS32;
					v = (char*)&src.displacement;
					write_size = 4;
				} else {
					modrm = MODRM_REG_SIB;
				}

				r_reg = src.base;
			}
		}

		modrm |= l_reg << 3 | r_reg;

	} else {

		mov_op = 0x89;

		if(dst.mem_address) {
			load_addr_to_reg(dst);
		}

		modrm = MODRM_REG_MEM;

		if(is_mem_sib(dst)) {
			sib = gen_sib(dst);

			if(dst.displacement > 0) {
				modrm = MODRM_REG_SIB_DIPS32;
				v = (char*)&dst.displacement;
				write_size = 4; // displacement cannot be over 4 bytes
			} else  { modrm = MODRM_REG_SIB; }

			l_reg = dst.base;
		} 

		modrm |= l_reg | r_reg << 3;
	}



	if(is_imm(src)) {
		mov_op = 0xB8 + l_reg;
		v = (char*)&src.imm_val;
		write_size = src.mem_size;
	}


	if(prefix)  {
		if(is_reg(dst) && src.mem_size < 8) {
			prefix &= ~(1 << 3);
		}

		WRITE_HEAD(prefix);
	}



	WRITE_HEAD(mov_op);

	//@Todo: immedate and stack needs a lot of bytes
	if(!is_imm(src)) WRITE_HEAD(modrm);
	if(is_mem_sib(src) || is_mem_sib(dst)) WRITE_HEAD(sib);
	
	for(i32 i = 0; v != NULL && i < write_size; i++) {
		WRITE_HEAD(v[i]);
	}

#if 0
	switch(src.type) {

		default: {
			assert_exit(false, "Unhandled Src operand for mov type %s.", op_source_names[src.type]);
		} break;

		
		case SH_SRC_IMMEDIATE: {

			switch(dst.type) {
				default: {
					assert_exit(false, "Unhandled DST operand type %s.", op_source_names[src.type]);
				} break;

				case SH_SRC_MEMORY: {
					write_to_register(RAX, (char*)&dst.mem_address, 8);

					if(dst.mem_size > 4) {
						WRITE_HEAD(REX_W);
					}

					WRITE_HEAD(0xC7);
					WRITE_HEAD(0b00000000 | RAX);
					char *num = (char*)&src.imm_val;
					for(int i = 0; i < 4; i++) {
						WRITE_HEAD(num[i]);
					}

				} break;

				case SH_SRC_REGISTER: {
					char *dat = (char*)&src.imm_val;
					write_to_register(dst.reg, dat, src.mem_size);
				} break;
			}


		} break;

		case SH_SRC_REGISTER: {

			switch(dst.type) {
				default: {
					assert_exit(false, "Unhandled DST operand type %s.", op_source_names[src.type]);
				} break;


				// mov [mem], reg
				case SH_SRC_MEMORY: {
					write_to_register(dst.reg, (char*)&dst.mem_address, 8);
					if(dst.mem_size > 4) {
						WRITE_HEAD(REX_W);
					}

					WRITE_HEAD(0x89);
					WRITE_HEAD(0b00000000 | src.reg << 3 | dst.reg );
				} break;

				case SH_SRC_REGISTER: {

					/* printf("%s => %s\n", get_reg_name_op(src), get_reg_name_op(dst)); */
					if(dst.reg != src.reg) {
						i8 prefix = 0;

						if(dst.mem_size > 4) {
							prefix = REX_W;
						}

						if(dst.reg >= R8) {
							dst.reg &= 7;
							prefix |= 0b01000001;
						}

						if(src.reg >= R8) {
							/* src.reg &= 7; */
							prefix |= 0b01000100;
						}

						WRITE_HEAD(prefix);
						WRITE_HEAD(0x89);
						WRITE_HEAD(0b11000000 | src.reg << 3 | dst.reg);
					}

				} break;


				case SH_SRC_MEM_SIB: {

					u8 prefix = 0;
					if(dst.mem_size > 4) {
						prefix = REX_W;
					}

					if(src.reg >= R8) {
						prefix |= 0b01000100;
						src.reg &= 0x07;
					}


					if(prefix > 0) {
						WRITE_HEAD(prefix);
					}

					WRITE_HEAD(0x89);
					WRITE_HEAD(0b10000100 | src.reg << 3);
					WRITE_HEAD(0b00100000 | RSP);

					char *imm_val = (char*)&dst.stack_relative;
					for(int i = 0; i < 4; i++) {
						WRITE_HEAD(imm_val[i]);
					}

				} break;
			}



		} break;

		case SH_SRC_MEMORY: {

			switch(dst.type) {
				default: {
					assert_exit(false, "Unhandled DST operand type %s.", op_source_names[src.type]);
				} break;

				case SH_SRC_MEMORY: {

					sh_register src_reg = src.reg;
					sh_register dst_reg = dst.reg;

					src.reg = src_reg;
					char *dat = (char*)&src.mem_address;
					write_to_register(src_reg, dat, 8);


					if(dst.mem_size > 4) {
						WRITE_HEAD(REX_W);
					}

					WRITE_HEAD(0x8B);
					WRITE_HEAD(0b00000000 | dst_reg << 3 | src_reg);


					dat = (char*)&dst.mem_address;
					write_to_register(src_reg, dat, 8);

					if(dst.mem_size > 4) {
						WRITE_HEAD(REX_W);
					}

					WRITE_HEAD(0x89);
					WRITE_HEAD(0b00000000 | dst_reg << 3 | src_reg);

				} break;

				case SH_SRC_REGISTER: {

					sh_register src_reg = src.reg;
					sh_register dst_reg = dst.reg;

					write_to_register(src_reg, (char*)&src.mem_address, 8);
					if(dst.mem_size > 4) {
						WRITE_HEAD(REX_W);
					}

					WRITE_HEAD(0x8B);
					WRITE_HEAD(0b00000000 | dst_reg << 3 | src_reg );
				} break;

			}
		} break;


		case SH_SRC_MEM_SIB: {

			switch(dst.type) {

				default: {
					assert_exit(false, "Unhandled DST operand type %s.", op_source_names[src.type]);
				} break;


				// mov [mem], reg
				case SH_SRC_MEMORY: {

					write_to_register(dst.reg, (char*)&dst.mem_address, 8);
					if(dst.mem_size > 4) {
						WRITE_HEAD(REX_W);
					}

					WRITE_HEAD(0x89);
					WRITE_HEAD(0b00000000 | src.reg << 3 | dst.reg );

				} break;

				case SH_SRC_REGISTER: {

					u8 prefix = 0;
					if(dst.mem_size > 4) {
						prefix = REX_W;
					}

					if(dst.reg >= R8) {
						prefix |= 0b01000100;
						dst.reg &= 0x07;
					}


					if(prefix > 0) {
						WRITE_HEAD(prefix);
					}

					WRITE_HEAD(0x8B);
					WRITE_HEAD(0b10000100 | dst.reg << 3);
					WRITE_HEAD(0b00100000 | RSP);

					char *imm_val = (char*)&src.stack_relative;
					for(int i = 0; i < 4; i++) {
						WRITE_HEAD(imm_val[i]);
					}

				} break;


				case SH_SRC_MEM_SIB: {
					u8 prefix = 0;
					if(dst.mem_size > 4) {
						prefix = REX_W;
					}

					if(src.reg >= R8) {
						prefix |= 0b01000100;
						src.reg &= 0x07;
					}


					if(prefix > 0) {
						WRITE_HEAD(prefix);
					}


					write_mov(sh_new_reg_location(src.reg, src.mem_size), src);

					WRITE_HEAD(0x89);
					WRITE_HEAD(0b10000100 | src.reg << 3);
					WRITE_HEAD(0b00100000 | RSP);

					char *imm_val = (char*)&dst.stack_relative;
					for(int i = 0; i < 4; i++) {
						WRITE_HEAD(imm_val[i]);
					}

				} break;
			}


		} break;

	}

#endif

}

void write_lea(sh_op_operand dst, sh_op_operand src) {

	u8 prefix = gen_prefix(dst, src);
	u8 lea_op = 0;
	u8 modrm = 0;
	u8 sib = 0;
	char *v = NULL;
	i32 write_size = 0;
	sh_register l_reg = dst.reg;
	sh_register r_reg = src.reg;

	/* if(is_reg(dst)) { */

	lea_op = 0x8D;

	// @Note: @ addr 0 would be wrong (also very illegal to access most likely)
	modrm = MODRM_REG_MEM;

	if(src.mem_address) {
		load_addr_to_reg(src);
	}

	if(is_mem_sib(src)) {
		sib = gen_sib(src);
		if(src.displacement > 0) {
			modrm = MODRM_REG_SIB_DIPS32;
			v = (char*)&src.displacement;
			write_size = 4;
		} else { modrm = MODRM_REG_SIB; }

		r_reg = src.base;
	}

	modrm |= l_reg << 3 | r_reg;


	if(prefix)  { WRITE_HEAD(prefix); }

	WRITE_HEAD(lea_op);
	WRITE_HEAD(modrm);

	//@Todo: immedate and stack needs a lot of bytes
	if(is_mem_sib(src)) WRITE_HEAD(sib);
	
	for(i32 i = 0; v != NULL && i < write_size; i++) {
		WRITE_HEAD(v[i]);
	}

}



// does mov? 
void write_to_register(sh_register reg, char* data_to_write, i32 data_size) {

	i8 prefix = 0;

	if(data_size > 4) {
		prefix = 0b01001000;
	}

	if(reg >= R8) {
		prefix |= 0b01000001;
		reg &= 0x07;
	}


	if(prefix > 0) {
		WRITE_HEAD(prefix);
	}

	WRITE_HEAD(0xb8 + reg);
	for(int i = 0; i < data_size; i++) {
		WRITE_HEAD(data_to_write[i]);
	}

}

u8 cmp_opcodes[SH_LOGICAL_OP_END - SH_LOGICAL_OP_START + 1] =  {
	[SH_GR 			 - SH_GR] = 0x7F,
	[SH_AND_OPERATOR - SH_GR] =  -1, 
	[SH_LT 			 - SH_GR] = 0x7C,
	[SH_GREQ 		 - SH_GR] = 0x7D,
	[SH_LTEQ 		 - SH_GR] = 0x7E,
	[SH_EQ 			 - SH_GR] = 0x74,
	[SH_NEQ 		 - SH_GR] = 0x75,
	[SH_AND 		 - SH_GR] = -1,
	[SH_OR 		     - SH_GR] = -1  // these should be handled differently? 
};

sh_op_operand write_cmp_op(sh_token_base_type operator, sh_op_dst d, sh_op_operand l, sh_op_operand r) {


	if( operator < SH_LOGICAL_OP_START  || operator > SH_LOGICAL_OP_END) {
		assert_exit(false, "Op is not a logical operator %s\n", base_type_names[operator]);
	}

	i8 jmpopcode = 0;
	jmpopcode = cmp_opcodes[operator - SH_GR];
	assert_exit(jmpopcode != -1, "logical operator not implemented");


	if(is_imm(l)) { // if left is imm need to load left to reg
		l = load_imm_to_reg(l);
	} else if(is_mem_or_sib(l) && is_mem_or_sib(r)) {
		sh_op_operand left_reg = sh_new_reg_location(l.reg, l.mem_size);
		write_mov(left_reg, l);
		l = left_reg;
	}

	if(is_imm(r) && r.mem_size > 4) {
		r = load_imm_to_reg(r);
	}

	u8 prefix = gen_prefix(l, r);
	u8 op = 0;
	u8 modrm = 0;
	u8 sib = 0;
	char *v = NULL;
	char *imm = NULL;
	i32 imm_size = 0;
	i32 write_size = 0;

	sh_register l_reg = l.reg;
	sh_register r_reg = r.reg;

	if(is_imm(r)) {
		
		op = 0x81;
		modrm = MODRM_REG_REG | 7 << 3;


		imm = (char*)&r.imm_val;
		imm_size = r.mem_size;

		if(is_mem_or_sib(l)) {
			modrm = MODRM_REG_MEM;
			if(l.mem_address) {
				load_addr_to_reg(l);
			}

			if(is_mem_sib(l)) {
				sib = gen_sib(l);
				if(l.displacement > 0) {
					modrm = MODRM_REG_SIB_DIPS32;
					v = (char*)&l.displacement;
					write_size = 4;
					l_reg = l.base;
				} else { modrm = MODRM_REG_SIB; }
			}
		}
		modrm |= 7 << 3 | l_reg;
	} else {

		op = 0x3B;
		modrm = MODRM_REG_REG;

		// if l is not reg then its definitely a mem reg cmp
		if(is_mem_or_sib(l)) {

			op = 0x39;

			modrm = MODRM_REG_MEM;
			if(l.mem_address) {
				load_addr_to_reg(l);
			}

			if(is_mem_sib(l)) {
				sib = gen_sib(l);
				if(l.displacement > 0) {
					modrm = MODRM_REG_SIB_DIPS32;
					v = (char*)&l.displacement;
					write_size = 4;
					l_reg = l.base; // or RSP tbh
				} else { modrm = MODRM_REG_SIB; }
			}

			modrm |= l_reg | r_reg << 3;

		} else {
			// reg, r/m

			modrm = MODRM_REG_REG;

			if(is_mem_or_sib(r)) {
				modrm = MODRM_REG_MEM;
				if(r.mem_address) {
					load_addr_to_reg(r);
				}

				if(is_mem_sib(r)) {
					sib = gen_sib(r);
					if(r.displacement > 0) {

						modrm = MODRM_REG_SIB_DIPS32;
						v = (char*)&r.displacement;
						write_size = 4;
					} else { modrm = MODRM_REG_SIB; }

					r_reg = r.base;
				}
			}

			modrm |= l_reg << 3 | r_reg;
		}

	}

	if(prefix)  { WRITE_HEAD(prefix); }

	WRITE_HEAD(op);

	WRITE_HEAD(modrm);
	if(is_mem_sib(l) || is_mem_sib(r)) WRITE_HEAD(sib);
	
	for(i32 i = 0; v != NULL && i < 4; i++) {
		WRITE_HEAD(v[i]);
	}


	for(i32 i = 0; imm != NULL && i < 4; i++) {
		WRITE_HEAD(imm[i]);
	}

	write_mov(d, sh_new_ir_imm_operand(1, RAX, 8));
	WRITE_HEAD(jmpopcode);
	WRITE_HEAD(10); 
	write_mov(d, sh_new_ir_imm_operand(0, RAX, 8)); 



#if 0
	if( (left_op.type == SH_SRC_MEM_SIB || left_op.type == SH_SRC_MEMORY )) {
		if(right_op.type == SH_SRC_MEM_SIB || right_op.type == SH_SRC_MEMORY ) {
			sh_op_operand temp = sh_new_reg_location(left_op.reg, left_op.mem_size);
			write_mov(temp, left_op);
			left_op = temp;
		} else {
			sh_op_operand t = left_op;
			left_op = right_op;
			right_op = t;
		}
	}

	switch(left_op.type) {
		case SH_SRC_REGISTER: {
			switch(right_op.type) {
				case SH_SRC_REGISTER: {

					u8 op = 0x3B; //r64 - rm64
					
					sh_register l = left_op.reg;
					sh_register r = right_op.reg;
					u8 prefix = 0;

					if(right_op.mem_size > 4 || left_op.mem_size > 4) {
						prefix = REX_W;
					}

					if(l >= R8) {
						prefix |= REX_B;
						l &= 7;
					}

					if(r >= R8) {
						prefix |= REX_R;
						r &= 7;
					}

					if(prefix > 0) {
						WRITE_HEAD(prefix);
					}
					WRITE_HEAD(op);
					WRITE_HEAD(0b11000000 | l << 3 | r);

					write_mov(result_dest, sh_new_ir_imm_operand(1, RAX, 4));
					WRITE_HEAD(jmpopcode);
					WRITE_HEAD(5);
					write_mov(result_dest, sh_new_ir_imm_operand(0, RAX, 4));
					// copy RAX 4 bytes

				} break;

				case SH_SRC_MEM_SIB: {

					u8 op = 0x3B; //r64 - rm64
					
					sh_register l = left_op.reg;
					sh_register r = right_op.reg;
					u8 prefix = 0;

					if(right_op.mem_size > 4 || left_op.mem_size > 4) {
						prefix = REX_W;
					}

					if(l >= R8) {
						prefix |= REX_B;
						l &= 7;
					}

					if(r >= R8) {
						prefix |= REX_R;
						r &= 7;
					}

					if(prefix > 0) {
						WRITE_HEAD(prefix);
					}

					WRITE_HEAD(op);
					WRITE_HEAD(0b10000100 | l << 3);
					WRITE_HEAD(0b10100000 | RSP);

					char *rel_stack = (char*)&right_op.stack_relative ;
					for(i32 i = 0; i < 4; i++) {
						WRITE_HEAD(rel_stack[i]);
					}

					write_mov(result_dest, sh_new_ir_imm_operand(1, RAX, 4));
					WRITE_HEAD(jmpopcode);
					WRITE_HEAD(5);
					write_mov(result_dest, sh_new_ir_imm_operand(0, RAX, 4));
					// copy RAX 4 bytes

				} break;


				default: {
					assert_exit(false, "right op type %s not implemented for CMP operation.\n",
							op_source_names[right_op.type]);
				} break;

			}
		} break;

		case SH_SRC_MEM_SIB: {

			switch(right_op.type) {
				default: {
					assert_exit(false, "right op type %s not implemented for CMP operation.\n",
							op_source_names[right_op.type]);
				} break;

				case SH_SRC_REGISTER: {

					sh_register r = right_op.reg;
					u8 prefix = gen_prefix(left_op, right_op);

					if(left_op.mem_size <= 4) prefix ^= REX_W;

					if(prefix & REX_R) r &= 7;

					if(prefix) {
						WRITE_HEAD(prefix);
					}

					WRITE_HEAD(0x39); 
					WRITE_HEAD(0b10000100 | r << 3);
					WRITE_HEAD(0b10100000 | RSP);
					char *val = (char*)&left_op.stack_relative;

					for(i32 i = 0; i < 4; i++) {
						WRITE_HEAD(val[i]);
					}

					write_mov(result_dest, sh_new_ir_imm_operand(1, RAX, 4));
					WRITE_HEAD(jmpopcode);
					WRITE_HEAD(5);
					write_mov(result_dest, sh_new_ir_imm_operand(0, RAX, 4));


				} break;

				case SH_SRC_IMMEDIATE: {

					if(left_op.mem_size > 0) {
						WRITE_HEAD(REX_W);
					}

					WRITE_HEAD(0x81); 
					WRITE_HEAD(0b10111100);
					WRITE_HEAD(0b10100000 | RSP);
					char *val = (char*)&left_op.stack_relative;

					for(i32 i = 0; i < 4; i++) {
						WRITE_HEAD(val[i]);
					}

					val = (char*)&right_op.imm_val;
					for(i32 i = 0; i < 4; i++) {
						WRITE_HEAD(val[i]);
					}


				} break;

			}

		} break;

		default: {
			assert_exit(false, "left op type %s not implemented for CMP operation.\n", op_source_names[left_op.type]);
		} break;
	}


#endif

	return d;
}

sh_op_operand write_add_op(sh_op_dst d, sh_op_operand l, sh_op_operand r) {

	if(is_imm(l)) { // if left is imm need to load left to reg
		l = load_imm_to_reg(l);
	} else if(is_mem_or_sib(l)) {

		sh_op_operand left_reg = sh_new_reg_location(l.reg, l.mem_size);
		if(is_xmm(l)) {
			write_mov_xmm(left_reg, l);
			left_reg.reg_type = SH_XMM_REGISTER;
		} else {
			write_mov(left_reg, l);
		}
		l = left_reg;
	}

	if(is_imm(r) && r.mem_size > 4) {
		r = load_imm_to_reg(r);
	}


	u8 prefix = gen_prefix(l, r);
	u8 xmm_prefix = 0xf2;
	u8 op = 0;
	u8 modrm = 0;
	u8 sib = 0;
	u8 xmm = is_xmm(r) || is_xmm(l);
	char *v = NULL;
	char *imm = NULL;
	i32 imm_size = 0;
	i32 write_size = 0;

	sh_register l_reg = l.reg;
	sh_register r_reg = r.reg;

	if(is_imm(r)) {
		
		op = 0x81;
		
		modrm = MODRM_REG_REG | 0 << 3;


		imm = (char*)&r.imm_val;
		imm_size = r.mem_size;

		if(is_mem_or_sib(l)) {
			modrm = MODRM_REG_MEM;
			if(l.mem_address) {
				load_addr_to_reg(l);
			}

			if(is_mem_sib(l)) {
				sib = gen_sib(l);
				if(l.displacement > 0) {
					modrm = MODRM_REG_SIB_DIPS32;
					v = (char*)&l.displacement;
					write_size = 4;
				} else { modrm = MODRM_REG_SIB; }
			}
			l_reg = l.base;
		}

		modrm |= 0 << 3 | l_reg;
		
	} else {

		op = 0x03;
		modrm = MODRM_REG_REG;

		// if l is not reg then its definitely a mem reg cmp
		if(is_mem_or_sib(l)) {

			op = 0x01;

			modrm = MODRM_REG_MEM;
			if(l.mem_address) {
				load_addr_to_reg(l);
			}

			if(is_mem_sib(l)) {
				sib = gen_sib(l);
				if(l.displacement > 0) {
					modrm = MODRM_REG_SIB_DIPS32;
					v = (char*)&l.displacement;
					write_size = 4;
				} else { modrm = MODRM_REG_SIB; }

				l_reg = l.base;
			}

			modrm |= l_reg | r_reg << 3;

		} else {
			// reg, r/m

			modrm = MODRM_REG_REG;

			if(is_mem_or_sib(r)) {
				modrm = MODRM_REG_MEM;
				if(r.mem_address) {
					load_addr_to_reg(r);
				}

				if(is_mem_sib(r)) {
					sib = gen_sib(r);
					if(r.displacement > 0) {

						modrm = MODRM_REG_SIB_DIPS32;
						v = (char*)&r.displacement;
						write_size = 4;
					} else { modrm = MODRM_REG_SIB; }

					r_reg = r.base;
				}

			}

			modrm |= l_reg << 3 | r_reg;
		}

	}

	if(xmm) {
		op = 0x58;
		if(l.mem_size < 8)
			xmm_prefix = 0xf3;

	}


	if(xmm) {
		WRITE_HEAD(xmm_prefix);
		WRITE_HEAD(0x0F);
	} else {
		if(prefix)  { WRITE_HEAD(prefix); }
	}

	WRITE_HEAD(op);
	WRITE_HEAD(modrm);

	if(is_mem_sib(l) || is_mem_sib(r)) WRITE_HEAD(sib);
	
	for(i32 i = 0; v != NULL && i < 4; i++) {
		WRITE_HEAD(v[i]);
	}


	for(i32 i = 0; imm != NULL && i < 4; i++) {
		WRITE_HEAD(imm[i]);
	}


	if(xmm) {
		d.reg_type = SH_XMM_REGISTER;
		write_mov_xmm(d, l);
	} else {
		write_mov(d, l);
	}

	return d;

}


//@Note: should not return an operand (maybe?)
void write_inc_op(sh_op_operand op) {


	u8 prefix = 0;

	if(op.mem_size > 4) { prefix = REX_W; }
	if(op.reg >= R8) { prefix |= REX_R; }
	if(prefix) WRITE_HEAD(prefix);

	i32 modrm = 0;

	switch(op.type) {
		case SH_SRC_REGISTER: modrm = 0b11000000 | op.reg; break;
		case SH_SRC_MEM_SIB: modrm = 0b10000100; break;
		case SH_SRC_MEMORY: modrm = 0b01000000 | op.reg; break;
		default: assert_exit(false, "wrong mem type for inc");
	}


	WRITE_HEAD(0xFF);

	WRITE_HEAD(modrm);

	if(op.type == SH_SRC_MEM_SIB) {
		WRITE_HEAD(0b10100000 | RSP);
		char *r = (char*)&op.stack_relative;
		for(i32 i = 0; i < 4; i++) {
			WRITE_HEAD(r[i]);
		}
	}
}


sh_op_operand write_sub_op(sh_op_dst result_dest, sh_op_operand left_op, sh_op_operand right_op) {

	i8 encode_mod_rm = 0; // 
	i8 op_code = 0;

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

					encode_mod_rm = 0b11101000;
					encode_mod_rm |=  left_op.reg;
					WRITE_HEAD(encode_mod_rm);

					char *ptr = (char*)&right_op.imm_val;
					for(int i = 0; i < 4; i++) {
						WRITE_HEAD(ptr[i]); //one byte rex one byte op 
					}

				} break;

				case SH_SRC_REGISTER: {

					WRITE_HEAD(REX_W);
					WRITE_HEAD(0x01);
					encode_mod_rm = 0b11000000 | right_op.reg << 3 | left_op.reg;
					WRITE_HEAD(encode_mod_rm);

				} break;


				case SH_SRC_MEMORY: {

					write_to_register(right_op.reg, (char*)&right_op.mem_address, 8);

					WRITE_HEAD(REX_W);
					WRITE_HEAD(0x03);
					WRITE_HEAD(0b0000000 | left_op.reg << 3 | right_op.reg);

				} break;

			}

		} break;
		case SH_SRC_MEMORY: {

			switch(right_op.type) {
				case SH_SRC_IMMEDIATE: {

					sh_op_operand temp_left = sh_new_reg_location(left_op.reg, left_op.mem_size);
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

				} break;

				case SH_SRC_REGISTER: {

					write_mov(sh_new_reg_location(left_op.reg, left_op.mem_size), left_op);

					WRITE_HEAD(REX_W);
					WRITE_HEAD(0x01);
					encode_mod_rm = 0b11000000 | right_op.reg << 3 | left_op.reg;
					WRITE_HEAD(encode_mod_rm);

				} break;


				case SH_SRC_MEMORY: {
					write_mov(sh_new_reg_location(left_op.reg, left_op.mem_size), left_op);
					write_to_register(right_op.reg, (char*)&right_op.mem_address, 8);

					WRITE_HEAD(REX_W);
					WRITE_HEAD(0x03);
					WRITE_HEAD(0b00000000 | left_op.reg << 3 | right_op.reg);

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

				} break;

				case SH_SRC_REGISTER: {

				} break;
				
				default: {
					puts("case not handled");
				} break;

			}

		} break;

	}

	


	return result_dest;
}

sh_op_operand write_mul_op(sh_op_dst result_dest, sh_op_operand left_op, sh_op_operand right_op) {

	i8 encode_mod_rm = 0; // 
	i8 op_code = 0;

	//@Todo: we don't consider imuls 3 operand mul when right side is immedate

	//@Todo: this could be wrong
	if( (left_op.type == SH_SRC_MEM_SIB || left_op.type == SH_SRC_MEMORY )) {
		if(right_op.type == SH_SRC_MEM_SIB || right_op.type == SH_SRC_MEMORY ) {
			sh_op_operand temp = sh_new_reg_location(left_op.reg, left_op.mem_size);
			write_mov(temp, left_op);
			left_op = temp;
		} else {
			sh_op_operand t = left_op;
			left_op = right_op;
			right_op = t;
		}
	}



	switch(left_op.type) {
		case SH_SRC_IMMEDIATE: { //everything is 3 OP
			switch(right_op.type) {

				default: {
					assert_exit(false, "Not implemented %s %s", op_source_names[left_op.type],
							op_source_names[right_op.type]);
				}

				case SH_SRC_MEM_SIB: {

					left_op = load_imm_to_reg(left_op);

					sh_register left_reg = left_op.reg;
					sh_register right_reg = right_op.reg;

					u8 prefix = 0; //REX_W;

					if(left_op.mem_size > 4) {
						prefix = REX_W;
					}

					if(left_reg >= R8) {
						prefix |= 0b01000001;
						left_reg &= 7; //only the 3 lower bits are needed
					}

					if(prefix > 0) {
						WRITE_HEAD(prefix);
					}

					WRITE_HEAD(0x0f);
					WRITE_HEAD(0xAF);

					WRITE_HEAD(0b10000100 | left_reg << 3 );
					WRITE_HEAD(0b10100000 | RSP );

					char *relative = (char*)&right_op.stack_relative;
					for(i32 i = 0; i < 4; i++) {
						WRITE_HEAD(relative[i]);
					}

				} break;

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

				} break;

				case SH_SRC_IMMEDIATE: {

					write_to_register(left_op.reg, (char*)&left_op.imm_val, 8);

					encode_mod_rm = 0b11000000;
					encode_mod_rm |=  result_dest.reg << 3 | left_op.reg;

					WRITE_HEAD(REX_W);
					WRITE_HEAD(0x69);
					WRITE_HEAD(encode_mod_rm);

					char *ptr = (char*)&right_op.imm_val;
					for(int i = 0; i < 4; i++) {
						WRITE_HEAD(ptr[i]); //one byte rex one byte op 
					}

				} break;


				case SH_SRC_REGISTER: {

					write_to_register(left_op.reg, (char*)&left_op.imm_val, 8);

					encode_mod_rm = 0b11000000;
					encode_mod_rm |=  left_op.reg << 3 | right_op.reg;

					WRITE_HEAD(REX_W);
					WRITE_HEAD(0x0f);
					WRITE_HEAD(0xAf);
					WRITE_HEAD(encode_mod_rm);

				} break;

			}

		} break;


		case SH_SRC_MEMORY: {
			
			switch(right_op.type) {
				default: {
					assert_exit(false, "Not implemented");
				}


				case SH_SRC_REGISTER: {

					write_mov(sh_new_reg_location(left_op.reg, left_op.mem_size), left_op);

					encode_mod_rm = 0b11000000;
					encode_mod_rm |=  left_op.reg << 3 | right_op.reg;

					WRITE_HEAD(REX_W);
					WRITE_HEAD(0x0F);
					WRITE_HEAD(0xAF);
					WRITE_HEAD(encode_mod_rm);

				} break;

				case SH_SRC_MEMORY: { // left and righ mem are memory

					write_mov(sh_new_reg_location(left_op.reg, left_op.mem_size), left_op);
					write_to_register(right_op.reg, (char*)&right_op.mem_address, 8);

					encode_mod_rm = 0b00000000;
					encode_mod_rm |=  left_op.reg << 3 | right_op.reg;

					WRITE_HEAD(REX_W);
					WRITE_HEAD(0x0F);
					WRITE_HEAD(0xAF);
					WRITE_HEAD(encode_mod_rm);

				} break;

				case SH_SRC_IMMEDIATE: {

					write_to_register(left_op.reg, (char*)&left_op.mem_address, 8);

					encode_mod_rm = 0b00000000;
					encode_mod_rm |=  left_op.reg << 3 | left_op.reg;

					WRITE_HEAD(REX_W);
					WRITE_HEAD(0x69);
					WRITE_HEAD(encode_mod_rm);

					char *ptr = (char*)&right_op.imm_val;
					for(int i = 0; i < 4; i++) {
						WRITE_HEAD(ptr[i]); //one byte rex one byte op 
					}


				} break;
			}

		} break;


		case SH_SRC_REGISTER: {

			switch(right_op.type) {
				default: {
					assert_exit(false, " implemented");
				}

				case SH_SRC_REGISTER: {

					sh_register left_reg = left_op.reg;
					sh_register right_reg = right_op.reg;
					i8 prefix = REX_W;

					if(left_op.reg >= R8) {
						prefix |= 0b01000001;
						left_reg &= 7; //only the 3 lower bits are needed
					}

					if(right_op.reg >= R8) {
						prefix |= 0b01000100;
						right_reg &= 7; //only the 3 lower bits are needed
					}

					WRITE_HEAD(prefix);

					WRITE_HEAD(0x0f);
					WRITE_HEAD(0xAF);
					WRITE_HEAD(0b11000000 | left_reg << 3 | right_reg);

				} break;

				case SH_SRC_MEMORY: {

					write_to_register(right_op.reg, (char*)&right_op.mem_address, 8);

					encode_mod_rm = 0b00000000;
					encode_mod_rm |=  result_dest.reg << 3 | right_op.reg;

					WRITE_HEAD(REX_W);
					WRITE_HEAD(0x0f);
					WRITE_HEAD(0xAF);

					WRITE_HEAD(encode_mod_rm);

				} break;

				case SH_SRC_MEM_SIB: {

					sh_register left_reg = left_op.reg;
					sh_register right_reg = right_op.reg;

					u8 prefix = 0; //REX_W;

					if(left_op.mem_size > 4) {
						prefix = REX_W;
					}

					if(left_reg >= R8) {
						prefix |= 0b01000001;
						left_reg &= 7; //only the 3 lower bits are needed
					}

					if(prefix > 0) {
						WRITE_HEAD(prefix);
					}

					WRITE_HEAD(0x0f);
					WRITE_HEAD(0xAF);

					WRITE_HEAD(0b10000100 | left_reg << 3 );
					WRITE_HEAD(0b10100000 | RSP );

					char *relative = (char*)&right_op.stack_relative;
					for(i32 i = 0; i < 4; i++) {
						WRITE_HEAD(relative[i]);
					}
 

				} break;

				case SH_SRC_IMMEDIATE: {
					// maybe this is needed? 

					load_imm_to_reg(right_op);
					//write_to_register(right_op.reg, (char*)&right_op.imm_val, 8);

					encode_mod_rm = 0b11000000;
					encode_mod_rm |=  left_op.reg << 3 | right_op.reg;

					WRITE_HEAD(REX_W);
					WRITE_HEAD(0x0f);
					WRITE_HEAD(0xAF);
					WRITE_HEAD(encode_mod_rm);

				} break;
			}

		} break;

		case SH_SRC_MEM_SIB: {

			assert_exit(false, "illegal left operand for imul.");

			/* switch(right_op.type) { */
			/* 	case SH_SRC_REGISTER: { */
			/* 		sh_register l = left_op.reg; */
			/* 		sh_register r = right_op.reg; */
			/* 		u8 prefix = gen_prefix(left_op, right_op); */
            /*  */
			/* 		if(prefix & REX_B) l &= 7; */
			/* 		if(prefix & REX_R) r &= 7; */
            /*  */
			/* 		if(prefix) WRITE_HEAD(prefix); */
            /*  */
			/* 		WRITE_HEAD(0x0F); */
			/* 		WRITE_HEAD(0xAF); */
			/* 		WRITE_HEAD(0b00000000 | ); */
            /*  */
            /*  */
            /*  */
			/* 	} break; */
			/* 	case SH_SRC_IMMEDIATE: { */
            /*  */
			/* 	} break; */
			/* 	default: { */
			/* 		assert_exit(false, "illegal operand combination, can't do addr/stack and addr/stack.\n"); */
			/* 	} */
			/* } */

		} break;

		default: {
			assert_exit(false, "mul src op not implemented %s\n", op_source_names[left_op.type]);
		}
	}

	write_mov(result_dest, left_op);

	return result_dest;
}




sh_op_operand write_operation( sh_token_base_type op, sh_op_dst result_dest, sh_op_operand left_op,
		sh_op_operand right_op) {


#if 0
	printf("%s[%s] = %s[%s] %s %s[%s]\n",
			op_source_names[result_dest.type],
			get_reg_name(result_dest.reg),
			op_source_names[left_op.type],
			get_reg_name(left_op.reg),
			op_type_name[op],
			op_source_names[right_op.type],
			get_reg_name(right_op.reg)
	);
#endif 

	switch(op) {
		case SH_PLUS: {
			return write_add_op(result_dest, left_op, right_op);
		} break;
		case SH_MINUS: {
			return write_sub_op(result_dest, left_op, right_op);
		} break;
		case SH_ASTERISK: {
			return write_mul_op(result_dest, left_op, right_op);
		} break;

		//@Todo: write out all the other logical ops
		case SH_EQ:
		case SH_LT:
		case SH_GR:  {
			return write_cmp_op(op, result_dest, left_op, right_op );
		} break;

		default: {
			assert_exit(false, "Op not implemented");
			return (sh_op_operand){0};
		} break;
	}

}

void write_cmp_to_1(void) {
	WRITE_HEAD(0x3C);
	WRITE_HEAD(0x01);
}


void write_jneq(void)  {
	WRITE_HEAD(0x0F); // jump if neq? 
	WRITE_HEAD(0x85);
}

void write_jmp() {
	WRITE_HEAD(0xe9);
}

void generate_func_start(sh_decl* decl, u64 mem_address, i32 func_size) {
	// store the registers , x64 -> RCX, RDX, R8, R9 -> first 9 registers => stack
	//TODO(sh): this shouldn't be straight 8 bytes per func arg, they can be 4 bytes too, wasted space
	i32 func_args = buf_len(decl->func.args);
	i32 stack_alloc = MAX(4, func_args); // +1 for alignment of stack? 
	i32 stack_alloc_bytes = stack_alloc*8; // caller must have allocated home space

	if(func_args > 0) {
		write_mov(sh_new_stack_location(8, 8, RAX), sh_new_reg_location(RCX, 8));
		write_mov(sh_new_stack_location(16, 8, RAX), sh_new_reg_location(RDX, 8));
		write_mov(sh_new_stack_location(24, 8, RAX), sh_new_reg_location(R8, 8));
		write_mov(sh_new_stack_location(32, 8, RAX), sh_new_reg_location(R9, 8));
	}

	if(func_size > 0) {
		write_sub_op(
				sh_new_reg_location(RSP, 8),
				sh_new_reg_location(RSP, 8),
				sh_new_ir_imm_operand(func_size, RAX, 8)
				);
	}
}


void generate_func_end(sh_decl* decl, u64 mem_address, i32 func_size) {
	// store the registers , x64 -> RCX, RDX, R8, R9 -> first 9 registers => stack
	//TODO(sh): this shouldn't be straight 8 bytes per func arg, they can be 4 bytes too, wasted space
	i32 func_args = buf_len(decl->func.args);
	i32 stack_alloc = MAX(4, func_args); // +1 for alignment of stack? 
	i32 stack_alloc_bytes = stack_alloc*8; // caller must have allocated home space

	if(func_args > 0) {
		write_mov(sh_new_reg_location(RCX, 8), sh_new_stack_location(8, 8, RAX));
		write_mov(sh_new_reg_location(RDX, 8), sh_new_stack_location(16, 8, RAX));
		write_mov(sh_new_reg_location(R8, 8), sh_new_stack_location(24, 8, RAX));
		write_mov(sh_new_reg_location(R9, 8), sh_new_stack_location(32, 8, RAX));
	}

	if(func_size > 0) {

		write_add_op(
				sh_new_reg_location(RSP, 8),
				sh_new_reg_location(RSP, 8),
				sh_new_ir_imm_operand(func_size, RCX, 4)
				);
	}
}

void sh_gen_decl(sh_decl *decl);
void sh_gen_var_decl(sh_decl *decl, i32 *stack_relative);
sh_op_operand sh_gen_expr(sh_expression *expr, sh_register store_result);


sh_op_operand write_func_call(sh_expression *func_expr, sh_register reg) {

	sh_expression **args = func_expr->args;
	sh_expression *fexpr = func_expr->func_expr;

	sh_register arg_reg[4] =  {RCX, RDX, R8, R9};

	i32 arg_len = buf_len(args);
	i32 arg_len_count = arg_len;

	if(arg_len > 0) {
		write_sub_op(sh_new_reg_location(RSP, 8),
				sh_new_reg_location(RSP, 8),
				sh_new_ir_imm_operand(32, RAX, 4) //wut D: 
		);

		stack_offset = 32;
	}

	//for args 1~4 use regs, then right to left push stack
	for(int i = 0; i < buf_len(args) && i < 4; i++) {
		sh_expression *arg = args[i];
		sh_op_operand res = sh_gen_expr(arg, arg_reg[i]);
		write_mov(sh_new_reg_location(arg_reg[i], 8), res);

		arg_len_count--;
	}

	if(arg_len_count > 0) {
		for(i32 i = arg_len_count - 1; i >= 0; i-- ) {

			sh_expression *e = args[4 + i];
			sh_op_operand res = sh_gen_expr(e, RAX);
			write_mov(sh_new_reg_location(RAX, 8), res);
			write_push_stack_register(RAX);
		}
	}

	write_mov(sh_new_reg_location(RAX, 8), sh_new_ir_imm_operand(fexpr->var_decl->mem_info->mem_address, reg, 8));
	/* write_to_register(RAX, (char*)&fexpr->var_decl->mem_info->mem_address, 8); */

	WRITE_HEAD(REX_W);
	WRITE_HEAD(0xff);
	WRITE_HEAD(0b11010000 | RAX);

	write_mov(sh_new_reg_location(reg, 8), sh_new_reg_location(RAX, 8));

	if(arg_len_count > 0) {
		/* write_add_op( */
		/* 		sh_new_reg_location(RSP, 8), */
		/* 		sh_new_reg_location(RSP, 8), */
		/* 		sh_new_ir_imm_operand(arg_len_count*8, RAX, 4) //wut D:  */
		/* 		); */
	}

	if(arg_len > 0) {
		write_add_op(
				sh_new_reg_location(RSP, 8),
				sh_new_reg_location(RSP, 8),
				sh_new_ir_imm_operand(32 + arg_len_count*8, RAX, 4) //wut D: 
				);
	}

	return sh_new_reg_location(reg, 8);
}


sh_memory_info* allocate_memory_info(sh_decl *decl, i32 stack_relative) {
	sh_memory_info *new_mem_info = (sh_memory_info*)calloc(1, sizeof(sh_memory_info));

	if(decl->storage == SH_LOCAL_STORAGE) {
		new_mem_info->mem_type = SH_SRC_MEM_SIB;
		new_mem_info->stack_rel = stack_relative;
	} else {
		new_mem_info->mem_address = sh_allocate_memory_main(decl->total_size); // is it a local var? gen stack
		new_mem_info->mem_type = SH_SRC_MEMORY;
	}

	new_mem_info->mem_size = decl->var.type->size_byte;

	return new_mem_info;
}




// both array and structs are multi-mem moves

void sh_gen_var_struct_init(sh_op_operand mem_location, sh_expression *struct_literal, sh_typespec *struct_type) {

	sh_op_operand init = {0};
	i32 base_displacment = (i32)mem_location.displacement;

	i32 field_index = 0;
	sh_type *t = struct_type->base_type;
	sh_expression **f = struct_literal->fields;

	for(i32 i = 0; i < buf_len(f); i++) {
		sh_expression *literal_field = f[i];
		sh_expression *value_expr = f[i];

		// assignment using the fields name
		if(literal_field->type == SH_FIELD_ASSIGNMENT_EXPR) {
			value_expr = literal_field->right;

			field_index = sh_find_struct_field_index(t,
					literal_field->left->name,
					literal_field->left->name_len
					);
		} 


		//if the field is an array or a struct, recusrsive 
		sh_decl *field = t->struct_type.fields[field_index];

		if(is_typespec_struct(field->struct_field.type)) {
			mem_location.displacement = base_displacment + field->struct_field.offset;
			sh_gen_var_struct_init(mem_location, value_expr, field->struct_field.type);
		} else if(is_typespec_array(field->struct_field.type)) {

		} else {
			init = sh_gen_expr(value_expr, RAX);
			sh_register move_via = choose_register(RAX);
			mem_location.reg = move_via;
			if(mem_location.type == SH_SRC_MEM_SIB) {
				mem_location.stack_relative = base_displacment + field->struct_field.offset;
			} else {
				assert_exit(false, "we don't handle global structs just yet");
			}

			
			mem_location.mem_size = field->struct_field.type->size_byte;

			write_mov(mem_location, init);

			

		}

		field_index++;

	}

}

void sh_gen_var_array_init(sh_op_operand mem_location, sh_expression *array_literal, sh_typespec *array_type) {

	sh_op_operand init = {0};

	i32 field_index = 0;

	sh_expression **values = array_literal->values;

	// should be either stack or mem
	mem_location.mem_size = array_type->base->size_byte;

	i64 disp = mem_location.displacement;

	for(i32 i = 0; i < buf_len(values); i++) {
		// array base is a struct? 
		if(is_typespec_struct(array_type->base)) {
			
			mem_location.displacement = disp;
			sh_gen_var_struct_init(mem_location, values[i], array_type->base);

		} else if(is_typespec_array(array_type->base)) {

			mem_location.displacement = disp;
			sh_gen_var_array_init(mem_location, values[i], array_type->base);

		} else {
			sh_op_operand init = sh_gen_expr(values[i], RAX);
			sh_register move_via = choose_register(RAX);
			mem_location.displacement = disp;
			write_mov(mem_location, init);
		}

		disp += array_type->base->size_byte;
	}

}


void sh_gen_var_scalar_type_init(sh_decl *decl) {
	sh_op_operand init = sh_gen_expr_decl(decl->var.init_expr, RAX, decl);
	sh_register move_via = choose_register(RAX);
	decl->mem_info->reg = move_via;
	sh_op_operand op = sh_new_operand_mem_info(decl->mem_info);

	sh_semantic_type *t = sh_type_check_expr(decl->var.init_expr);

	if(is_typespec_float_type(t->base_type)) {
		write_mov_xmm(op, init);
	} else {
		write_mov(op, init);
	}

}

//need to pass stack info? 
void sh_gen_var_decl(sh_decl *decl, i32 *stack_relative) {

	i32 local_variable = decl->storage == SH_LOCAL_STORAGE;

	if(decl->mem_info == NULL) {

		if(local_variable) {
			decl->mem_info = allocate_memory_info(decl, *stack_relative);
		} else {
			decl->mem_info = allocate_memory_info(decl, 0);
		}

	}
	
	if(decl->var.init_expr) {

		if(is_typespec_struct(decl->var.type)) {
			sh_op_operand mem_location = sh_new_operand_mem_info(decl->mem_info);
			sh_gen_var_struct_init(mem_location, decl->var.init_expr, decl->var.type);
		} else if(is_typespec_array(decl->var.type)) {
			sh_op_operand mem_location = sh_new_operand_mem_info(decl->mem_info);
			sh_gen_var_array_init(mem_location, decl->var.init_expr, decl->var.type);
		} else {
			sh_gen_var_scalar_type_init(decl);
		}
	}

}

sh_op_operand sh_gen_string_literal(sh_expression *str_expr, sh_register store_result) {
	sh_op_operand op = sh_new_ir_imm_operand(0x0, store_result, 8);

	op.imm_val = (i64)write_data_section(str_expr->str_val, str_expr->str_size);

	/* char *m = "\0"; */
	/* write_data_section(m, 1); */

	return op;
}


sh_op_operand sh_gen_id_expr(sh_expression *id, sh_register store_result) {
	
	
	sh_op_operand op = sh_new_operand_mem_info(id->var_decl->mem_info);
	op.reg = store_result;

	if(is_expr_float_type(id)) {
		op.reg_type = SH_XMM_REGISTER;
	}

	return op;
}


sh_op_operand sh_gen_field_access_expr(sh_expression *f_access, sh_register store_result) {

	sh_op_operand struct_memory = sh_gen_expr(f_access->pointer_expr, store_result);

	sh_expression *field_name = f_access->field_access;


	//assume its a defined type
	sh_semantic_type *ty = sh_type_check_expr(f_access->pointer_expr);

	sh_type* t = ty->base_type->base_type;

	// @Todo: this is dumb, 
	sh_decl *field = sh_get_struct_field_name(t, field_name->name, field_name->name_len);
	i32 mem_size = field->struct_field.type->size_byte;
	i32 field_offset = field->struct_field.offset;

	if(is_mem_sib(struct_memory)) {
		struct_memory.displacement += field_offset;
		return struct_memory;
	}

	return sh_new_mem_location_reg(struct_memory.mem_address + field_offset, store_result, mem_size);

}


i32 depth = 0;
sh_op_operand sh_gen_operator_expr(sh_expression *expr, sh_register store_result, sh_decl *d) {
	// @Todo(sh): maybe move all the func calls and push the registers there? instead of at every 
	// register spilling might be needed for deeply weird instructions
	// slethi-ulman algorithm for optimal register allocation

	sh_op_operand res = {0};
	sh_op_operand left = sh_gen_expr(expr->left_op, store_result);
	sh_register right_store = choose_register(store_result);
	sh_op_operand right = sh_gen_expr(expr->right_op, right_store);

	if(is_expr_add_or_sub(expr)) {
		sh_register temp_store = choose_register(right_store);

		if(is_expr_ptr(expr->left_op) && is_expr_int_type(expr->right_op)) {

			sh_op_operand mul = sh_new_ir_imm_operand(get_expr_type_size(expr->left_op), temp_store, 4);
			write_mul_op(right, right, mul);

		} else if(is_expr_ptr(expr->right_op) && is_expr_int_type(expr->left_op)) {

			sh_op_operand mul = sh_new_ir_imm_operand(get_expr_type_size(expr->right_op), temp_store, 4);
			write_mul_op(left, left, mul);

		}
	}



	// this is wrong we should also handle other types of movs
	sh_op_operand dst_location = sh_new_reg_location(store_result, 8);
	res = write_operation(expr->op, dst_location, left, right);

	return res;
}


sh_op_operand sh_gen_array_expr(sh_expression *expr, sh_register store_result) {

	sh_op_operand array_location = sh_gen_expr(expr->array_expr, store_result);

	sh_semantic_type *type = sh_type_check_expr(expr->array_expr);

	sh_register temp_reg = choose_register(store_result);
	sh_register temp_reg2 = choose_register(temp_reg);

	sh_op_operand index = sh_gen_expr(expr->array_index_expr, temp_reg);
	sh_op_operand index_mul = sh_new_ir_imm_operand( type->base_type->base->size_byte, temp_reg2, 4 );

	write_mul_op(index, index, index_mul);

	array_location.index = index.reg;

	write_lea(sh_new_reg_location(store_result, 8), array_location);

	array_location.base = store_result;
	array_location.index = NO_REG; // we load the effective address into store_result, i.e: the base
	array_location.mem_size = type->size_byte;
	array_location.reg = NO_REG;

	return array_location;//sh_new_mem_sib_location(SH_MEM_SIB_SCALE1, NO_REG, store_result, NO_REG, type->size_byte, 0);
}

// all real numbers are double at first?

sh_op_operand sh_gen_float_literal(sh_expression *expr, sh_register store_result) {
	return sh_new_ir_imm_operandf(expr->vf64, store_result, 8);
}


// @Todo: this feels wrong
sh_op_operand sh_gen_expr(sh_expression *expr, sh_register store_result) {
	return sh_gen_expr_decl(expr, store_result, NULL);
}

sh_op_operand sh_gen_expr_decl(sh_expression *expr, sh_register store_result, sh_decl *decl) {

	sh_op_operand res = {0};


	switch(expr->type) {
		case SH_INT_LITERAL:
			//not all int literals are suppose to be 8 bytes
			write_to_register(store_result, (char*)&expr->vi64, 8);
			res = sh_new_reg_location(store_result, 8);
			break;
		case SH_ID_EXPR: {
			res = sh_gen_id_expr(expr, store_result);
		} break;

		case SH_OPERATOR_EXPR: {

			return sh_gen_operator_expr(expr, store_result, decl);
			
		} break;


		case SH_FUNC_EXPR:  {

			if(store_result != RAX) {
				write_push_stack_register(RAX);
				stack_offset += 8;
			}

			res = write_func_call(expr, store_result);// ???

			if(store_result != RAX) {
				write_pop_stack_register(RAX);
				stack_offset -= 8;
			}

		} break;

		case SH_PTR_DEREF_EXPR: {
			sh_op_operand operand_mem = sh_gen_expr(expr->operand, store_result);
			sh_op_operand reg = sh_new_reg_location(store_result, 8);
			write_mov(reg, operand_mem);
			sh_op_operand mem_loc = sh_new_mem_location_reg(0, store_result, 8);
			write_mov(reg, mem_loc);
			return reg;
		} break;

		case SH_ADDRESS_OF_EXPR: {
			sh_op_operand operand_mem = sh_gen_expr(expr->operand, store_result);
			assert_exit(is_mem_or_sib(operand_mem), "Cannot take address of non-memory operands");

			sh_op_operand reg = sh_new_reg_location(store_result, 8);
			write_lea(reg, operand_mem);

			return reg;
		} break;

		case SH_STRUCT_LITERAL: {
			// pass in starting memory? 
			//@Todo: this is bad juju 2.0
			assert_exit(false, "we don't handle struct literal here");
		} break;

		case SH_NIL_LITERAL: {
			return sh_new_ir_imm_operand(0 , store_result , 8 );
		} break;

		case SH_STRING_LITERAL: {
			return sh_gen_string_literal(expr, store_result);
		} break;

		case SH_FIELD_ACCESS_EXPR: {
			return sh_gen_field_access_expr(expr, store_result);
		} break;
		case SH_ARRAY_EXPR: {
			return sh_gen_array_expr(expr, store_result);
		} break;
		case SH_FLOAT_LITERAL: {
			return sh_gen_float_literal(expr, store_result);
		} break;

		default: {
			assert_exit(false, "Unhandled gen expr ( %s ) of type (%s)\n", sh_print_expr(expr), get_expr_type_string(expr));
		} break;
	}


	return res;
}




void sh_gen_if_statement(sh_decl *parent_decl, sh_statement *if_stmt, i32 *stack_relative) {

	sh_gen_expr(if_stmt->condition_expr, RAX);

	write_cmp_to_1();
	write_jneq();

	i32 **end_labels = NULL;

	i32 *if_stmt_start = (i32*)writer_head;
	i32 *next_label = (i32*)writer_head;

	writer_head += 4; // if 

	sh_gen_statement(parent_decl, if_stmt->comp_statement, stack_relative);
	i32 elif_size = buf_len(if_stmt->elseif_stmts);

	// Todo: shit code
	if(elif_size > 0) {

		write_jmp();
		buf_push(end_labels, (i32*)writer_head);
		writer_head += 4; // move 4 bytes to make space for label
		*next_label = (i32) ( writer_head - (u8*)next_label) - 4;

		for(int i = 0; i < elif_size; i++) {

			sh_statement *elif = if_stmt->elseif_stmts[i];
			sh_gen_expr(elif->condition_expr, RAX);

			write_cmp_to_1();
			write_jneq();

			next_label = (i32*)writer_head;
			writer_head += 4; // if 

			sh_gen_statement(parent_decl, elif->comp_statement, stack_relative);

			//jumps to end
			// write jumps if this isn't the final one
			// also write jump if else is not null
			if(i + 1 != elif_size || if_stmt->else_stmt != NULL)  { 
				write_jmp();
				buf_push(end_labels, (i32*)writer_head);
				writer_head += 4; // move 4 bytes to make space for label
			} 

			*next_label = (i32) ( writer_head - (u8*)next_label) - 4;
		}

	}

	if(if_stmt->else_stmt) {

		write_jmp();
		buf_push(end_labels, (i32*)writer_head);
		writer_head += 4; // move 4 bytes to make space for label
		*next_label = (i32) ( writer_head - (u8*)next_label) - 4;

		sh_gen_statement(parent_decl, if_stmt->else_stmt, stack_relative);
	} else {
		*next_label = (i32) ( writer_head - (u8*)next_label) - 4;
	}

	//patch in the end jmps, this is dirty
	for(i32 **end = end_labels; end != buf_end(end_labels); end++) {
		end[0][0] = (i32) ( writer_head - (u8*)end[0]) - 4;
	}

}



typedef struct sh_end_label {
	sh_statement_type type;
	i32 *label;
} sh_end_label;

sh_end_label* scope_end_labels = NULL;

void sh_gen_for_statement(sh_decl *decl, sh_statement *stmt, i32 *stack_relative) {

	if(stmt->init_statement) {
		sh_gen_statement(decl, stmt->init_statement, stack_relative);
	}

	// break statements create jmps to end labels
	
	i32 *end_jmp_label = NULL;
	i32 *start_label = (i32 *)writer_head; // we jump back to this address

	if(stmt->condition_expr) {
		sh_gen_expr(stmt->condition_expr, RAX);
		write_cmp_to_1();
		write_jneq();
		end_jmp_label = (i32 *)writer_head;
		writer_head += 4;
	}

	// push sentinel value to indicate scope start 
	buf_push(scope_end_labels, (sh_end_label){ SH_FOR_STATEMENT, 0});

	if(stmt->comp_statement) {
		sh_gen_statement(decl, stmt->comp_statement, stack_relative);
	}

	u8 *post_loop_stmt_label = writer_head;
	
	if(stmt->post_loop_expr) {
		sh_gen_statement(decl, stmt->post_loop_expr, stack_relative);
	}

	write_jmp(); // 
	i32 start_label_rel_addr = (i32)(writer_head - (u8*)start_label);
	*((i32*)writer_head) = -start_label_rel_addr - 4;
	writer_head += 4;

	if(end_jmp_label != NULL) {
		*end_jmp_label = (i32) (writer_head - (u8*)end_jmp_label) - 4;
	}


	// if the loop contains other loops thus we start at the top of the labels
	// break statements jump to end of the loop statement statement
	for(i32 i = buf_len(scope_end_labels)-1; i >= 0; i--) {
		sh_end_label *label = buf_pop(scope_end_labels);

		i32 start_label_rel_addr = 0;
		if(label->type == SH_CONTINUE_STATEMENT) {
			start_label_rel_addr = (i32)(post_loop_stmt_label - (u8*)label->label);
		} else if(label->type == SH_BREAK_STATEMENT) {
			start_label_rel_addr = (i32)(writer_head - (u8*)label->label);
		} else break;

		*label->label = start_label_rel_addr - 4;
	}

	/* // remove the sentinel val */
	/* buf_pop(scope_end_labels); */

}


void sh_gen_statement(sh_decl *decl, sh_statement *stmt, i32 *stack_relative) {

	switch(stmt->type) {
		case SH_COMPOUND_STATEMENT: {
			sh_statement **c = stmt->statements;
			for(i32 i = 0; i < buf_len(c); i++) {
				sh_gen_statement(decl, c[i], stack_relative);
			}
		} break;

		case SH_VAR_DECL_STATEMENT: {
			sh_gen_var_decl(stmt->var_decl, stack_relative);
			*stack_relative += stmt->stmt_size;
		} break;

		case SH_RETURN_STATEMENT: {
			sh_op_operand ret = sh_gen_expr(stmt->ret_expr, RAX);

			if(is_expr_float_type(stmt->ret_expr)) {
				write_mov_xmm(sh_new_reg_location(RAX, ret.mem_size), ret);
			} else {
				write_mov(sh_new_reg_location(RAX, ret.mem_size), ret);
			}

			generate_func_end(decl, decl->mem_info->mem_address, decl->total_size);
			WRITE_HEAD(0xC3);
		} break;

		case SH_IF_STATEMENT: {
			sh_gen_if_statement(decl, stmt, stack_relative);
		} break;

		case SH_WHILE_STATEMENT:
		case SH_FOR_STATEMENT: {
			sh_gen_for_statement(decl, stmt, stack_relative);
		} break;

		case SH_CONTINUE_STATEMENT: 
		case SH_BREAK_STATEMENT: {
			write_jmp();
			buf_push(scope_end_labels, (sh_end_label){stmt->type, (i32*)writer_head} );
			writer_head += 4;
		} break;

		case SH_ASSIGNMENT_STATEMENT: {
			//@Todo: doesn't work as it is for pointers
			// structs are different 
			sh_op_operand res = sh_gen_expr(stmt->right_side_expr, RAX);
			sh_op_operand left = sh_gen_expr(stmt->left_side_expr, RCX);
			write_mov(left, res);
		} break;

		case SH_INC_DEC_STATEMENT: {
			sh_op_operand r = sh_gen_expr(stmt->unary_expr->operand, RAX);
			write_inc_op(r);
		} break;

		case SH_FUNC_CALL_STATEMENT: {
			sh_gen_expr(stmt->unary_expr, RAX);
		} break;

		default: {
			assert_exit(false, "Statement type not implemented\n");
		} break;
	}
}






//@Todo: maybe branches should be checked and see if there is a return in each branch or some shit
// maybe the parser or typechecker can mark all the exit points? no clue
// 
void sh_gen_func_decl(sh_decl *decl) {

	if(is_dll_import(decl)) {

		import_dll_func(decl);

	} else {

		// size of the function is needed, specifically, how many local vars exist?  
		generate_func_start(decl, decl->mem_info->mem_address, decl->total_size);

		sh_decl **args = decl->func.args;

		i32 stack_relative = 0;
		i32 assign_args = 0;
		// assign stack size for them? 

		if(buf_len(args) > 0 ) {
			stack_relative = 32 + 8;
			assign_args = 8 + decl->total_size;
		}

		sh_register pass_args[4] = { RCX, RDX, R8, R9 };
		for(int i = 0; i < buf_len(args) && i < 4; i++) {
			sh_decl *arg = args[i];
			sh_register r = pass_args[i];


			if(arg->mem_info == NULL) {
				arg->mem_info = (sh_memory_info* ) malloc(sizeof(sh_memory_info));
			}


			if(i < 4) {
				arg->mem_info->mem_type = SH_SRC_MEM_SIB;
				arg->mem_info->stack_rel = assign_args;
				arg->mem_info->reg = r;

				assign_args += 8;

				continue;
				/* arg->mem_info->mem_type = SH_SRC_REGISTER; */
				/* arg->mem_info->reg = r; */

			} else {
				arg->mem_info->mem_type = SH_SRC_MEM_SIB;
				arg->mem_info->stack_rel = assign_args;
			}

			assign_args += arg->total_size;

		}

		sh_gen_statement(decl, decl->func.compound_statement, &stack_relative); // after 32 bytes? 

		/* if(decl->func.return_type == NULL) { */
		generate_func_end(decl, decl->mem_info->mem_address, decl->total_size);
		WRITE_HEAD(0xC3);
		/* } */
	}
}


void sh_gen_decl(sh_decl *decl) {

	switch(decl->type) {
		case SH_VAR_DECL:
			sh_gen_var_decl(decl, 0);
			break;
		case SH_FUNC_DECL:

			if(decl->mem_info == NULL) {
				decl->mem_info = (sh_memory_info *)calloc(1, sizeof(sh_memory_info));
				//sh_allocate_memory_main(decl->total_size); // is it a local var? gen stack
				decl->mem_info->mem_address = (u64)writer_head;
				decl->mem_info->mem_type = SH_SRC_MEMORY;
			}


			if(decl->name_len == 4 && strncmp(decl->name, "main", 4) == 0) {
				main_func_address = (char*)writer_head;
			}

			sh_gen_func_decl(decl);

			decl->mem_info->mem_size = (i32)((u64)writer_head - decl->mem_info->mem_address );

			break;

		default: {
			puts("unhandled decl type");
		}
	}
}

void print_code_mem(char *mem, i32 size) {
	i32 main_mem_len = (i32)( writer_head - main_mem );
	i32 len_acc = 0;
	while(len_acc < size) {
		i32 length = print_gen_code(mem + len_acc);
		len_acc += length;
	}
}



#define PRRINT_GEN_CODE(text) \
	puts(text);\
	print_code_mem((char*)mem_start, (i32)(writer_head - mem_start ));\
	mem_start = writer_head;\
	puts("---------------")

void write_mov_test() {
	u8 *mem_start = writer_head;

	puts("8 bytes");
	sh_op_operand dst = sh_new_reg_location(RAX, 8);
	sh_op_operand src = sh_new_reg_location(RCX, 8);

	write_mov(dst, src);
	PRRINT_GEN_CODE("reg <== reg");

	src = sh_new_ir_imm_operand(4, RCX, 8);
	write_mov(dst, src);
	PRRINT_GEN_CODE("reg <== imm");

	src = sh_new_mem_location_reg(0x1234, RCX, 8);
	write_mov(dst, src);
	PRRINT_GEN_CODE("reg <== mem");


	dst = sh_new_mem_location_reg(0x1234, RAX, 8);
	src = sh_new_reg_location(RCX, 8);
	write_mov(dst, src);
	PRRINT_GEN_CODE("mem <== reg");

	src = sh_new_mem_location_reg(0x1234, RCX, 8);
	write_mov(dst, src);
	PRRINT_GEN_CODE("mem <== mem");


	//stack
	dst = sh_new_reg_location(RAX, 8);
	src = sh_new_mem_sib_location(SH_MEM_SIB_SCALE1, NO_REG, RSP, NO_REG, 4, 4);
	write_mov(dst, src);
	PRRINT_GEN_CODE("reg <== stack");



	dst = sh_new_mem_sib_location(SH_MEM_SIB_SCALE1, NO_REG, RSP, NO_REG, 4, 10);
	src = sh_new_reg_location(RCX, 8);
	write_mov(dst, src);
	PRRINT_GEN_CODE("stack <== reg");

	dst = sh_new_reg_location(RCX, 8);
	src = sh_new_mem_sib_location(SH_MEM_SIB_SCALE1, NO_REG, RAX, NO_REG, 4, 10);
	write_mov(dst, src);
	PRRINT_GEN_CODE("stack <== reg");



	dst = sh_new_reg_location(R8, 8);
	src = sh_new_ir_imm_operand(4, RCX, 8);
	write_mov(dst, src);
	PRRINT_GEN_CODE("reg[R*] <== imm");

}


void write_cmp_test() {
	u8 *mem_start = writer_head;

	sh_token_base_type op = SH_LT;

	puts("8 bytes");

	sh_op_operand d = sh_new_reg_location(RAX, 8);
	sh_op_operand l = sh_new_reg_location(RAX, 8);
	sh_op_operand r = sh_new_reg_location(RCX, 8);

	write_cmp_op(op, d, l, r);
	PRRINT_GEN_CODE("reg <== reg");

	r = sh_new_ir_imm_operand(4, RCX, 8);
	write_cmp_op(op, d, l, r);
	PRRINT_GEN_CODE("reg <== imm");

	l = sh_new_ir_imm_operand(4, RAX, 8);
	r = sh_new_ir_imm_operand(4, RCX, 8);
	write_cmp_op(op, d, l, r);
	PRRINT_GEN_CODE("imm <== imm");

	l = sh_new_ir_imm_operand(4, RAX, 8);
	r = sh_new_mem_location_reg(0x1234, RCX, 8);
	write_cmp_op(op, d, l, r);
	PRRINT_GEN_CODE("imm <== sib");


	l = sh_new_reg_location(RAX, 8);
	r = sh_new_mem_location_reg(0x1234, RCX, 8);
	write_cmp_op(op, d, l, r);
	PRRINT_GEN_CODE("reg <== mem");


	l = sh_new_mem_location_reg(0x1234, RAX, 8);
	r = sh_new_reg_location(RBX, 8);
	write_cmp_op(op, d, l, r);
	PRRINT_GEN_CODE("mem <== reg");

	r = sh_new_mem_location_reg(0x1234, RCX, 8);
	write_cmp_op(op, d, l, r);
	PRRINT_GEN_CODE("mem <== mem");


	//stack
	l = sh_new_reg_location(RAX, 8);
	r = sh_new_mem_sib_location(SH_MEM_SIB_SCALE1, RSP, RSP, RAX, 8, 4);
	write_cmp_op(op, d, l, r);
	PRRINT_GEN_CODE("reg <== stack");

	l = sh_new_mem_sib_location(SH_MEM_SIB_SCALE2, RSP, RSP, RAX, 8, 2);
	r = sh_new_reg_location(RCX, 8);
	write_cmp_op(op, d, l, r);
	PRRINT_GEN_CODE("stack <== reg");


	l = sh_new_mem_sib_location(SH_MEM_SIB_SCALE2, RCX, RAX, RAX, 8, 10);
	r = sh_new_ir_imm_operand(8, RCX, 8);
	write_cmp_op(op, d, l, r);
	PRRINT_GEN_CODE("stack cmp imm");

    
}


void write_add_test() {
	u8 *mem_start = writer_head;

	sh_token_base_type op = SH_LT;

	puts("8 bytes");

	sh_op_operand d = sh_new_reg_location(RAX, 8);
	sh_op_operand l = sh_new_reg_location(RAX, 8);
	sh_op_operand r = sh_new_reg_location(RCX, 8);

	write_add_op(d, l, r);
	PRRINT_GEN_CODE("reg + reg");

	r = sh_new_ir_imm_operand(4, RCX, 8);
	write_add_op(d, l, r);
	PRRINT_GEN_CODE("reg + imm");

	l = sh_new_ir_imm_operand(4, RAX, 8);
	r = sh_new_ir_imm_operand(4, RCX, 8);
	write_add_op(d, l, r);
	PRRINT_GEN_CODE("imm + imm");

	l = sh_new_ir_imm_operand(4, RAX, 8);
	r = sh_new_mem_location_reg(0x1234, RCX, 8);
	write_add_op(d, l, r);
	PRRINT_GEN_CODE("imm + mem");


	l = sh_new_reg_location(RAX, 8);
	r = sh_new_mem_location_reg(0x1234, RCX, 8);
	write_add_op(d, l, r);
	PRRINT_GEN_CODE("reg + mem");


	l = sh_new_mem_location_reg(0x1234, RAX, 8);
	r = sh_new_reg_location(RBX, 8);
	write_add_op(d, l, r);
	PRRINT_GEN_CODE("mem + reg");

	r = sh_new_mem_location_reg(0x1234, RCX, 8);
	write_add_op(d, l, r);
	PRRINT_GEN_CODE("mem + mem");


	//stack
	l = sh_new_reg_location(RAX, 8);
	r = sh_new_mem_sib_location(SH_MEM_SIB_SCALE1, NO_REG, RSP, RAX, 8, 4);
	write_add_op(d, l, r);
	PRRINT_GEN_CODE("reg + stack");

	l = sh_new_mem_sib_location(SH_MEM_SIB_SCALE2, NO_REG, RSP, RAX, 8, 2);
	r = sh_new_reg_location(RCX, 8);
	write_add_op(d, l, r);
	PRRINT_GEN_CODE("stack + reg");


	l = sh_new_mem_sib_location(SH_MEM_SIB_SCALE2, NO_REG, RSP, RAX, 8, 10);
	r = sh_new_ir_imm_operand(8, RCX, 8);
	write_add_op(d, l, r);
	PRRINT_GEN_CODE("stack + imm");


	l = sh_new_mem_sib_location(SH_MEM_SIB_SCALE1, NO_REG, RSP, RAX, 8, 10);
	r = sh_new_mem_sib_location(SH_MEM_SIB_SCALE1, NO_REG, RSP, RAX, 8, 10);
	write_add_op(d, l, r);
	PRRINT_GEN_CODE("stack + stack");


	d = sh_new_xmm_reg_location(RAX, 8);
	l = sh_new_xmm_reg_location(RAX, 8);
	r = sh_new_xmm_reg_location(RCX, 8);

	write_add_op(d, l, r);

	// WRITE_HEAD(0xF2);
	// WRITE_HEAD(0x0F);
	// WRITE_HEAD(0x58);
	// WRITE_HEAD(0b11000000 | l.reg << 3 | r.reg);

	PRRINT_GEN_CODE("xmm + xmm");

    
}

void write_lea_test() {
	u8 *mem_start = writer_head;

	puts("8 bytes");

	sh_op_operand l = sh_new_reg_location(RAX, 8);
	sh_op_operand r = sh_new_mem_location_reg(0x1234, RCX, 8);
	write_lea(l, r);
	PRRINT_GEN_CODE("reg + mem");
	r = sh_new_mem_sib_location(SH_MEM_SIB_SCALE1, NO_REG, RAX, NO_REG, 4, 4);
	write_lea(l, r);
	PRRINT_GEN_CODE("reg + stack");

}

void write_float_test() {
	u8 *mem_start = writer_head;

	sh_op_operand dst = sh_new_reg_location(XMM0, 8);
	sh_op_operand src = sh_new_reg_location(XMM2, 8);

	write_mov_xmm(dst, src);
	PRRINT_GEN_CODE("reg <== reg");

	src = sh_new_ir_imm_operandf(4.6, RCX, 8);
	write_mov_xmm(dst, src);

	WRITE_HEAD(0xF2);
	WRITE_HEAD(REX_W);
	WRITE_HEAD(0x0F);
	WRITE_HEAD(0x2D);
	WRITE_HEAD(0b11000000);

	PRRINT_GEN_CODE("reg <== imm");

	// src = sh_new_mem_location_reg(0x1234, RCX, 8);
	// write_mov_xmm(dst, src);
	// PRRINT_GEN_CODE("reg <== mem");


//	dst = sh_new_mem_location_reg(0x1234, RAX, 8);
//	src = sh_new_reg_location(RCX, 8);
//	write_mov(dst, src);
//	PRRINT_GEN_CODE("mem <== reg");

//	src = sh_new_mem_location_reg(0x1234, RCX, 8);
//	write_mov(dst, src);
//	PRRINT_GEN_CODE("mem <== mem");


//	//stack
//	dst = sh_new_reg_location(RAX, 8);
//	src = sh_new_mem_sib_location(SH_MEM_SIB_SCALE1, NO_REG, RSP, NO_REG, 4, 4);
//	write_mov(dst, src);
//	PRRINT_GEN_CODE("reg <== stack");



//	dst = sh_new_mem_sib_location(SH_MEM_SIB_SCALE1, NO_REG, RSP, NO_REG, 4, 10);
//	src = sh_new_reg_location(RCX, 8);
//	write_mov(dst, src);
//	PRRINT_GEN_CODE("stack <== reg");

//	dst = sh_new_reg_location(RCX, 8);
//	src = sh_new_mem_sib_location(SH_MEM_SIB_SCALE1, NO_REG, RAX, NO_REG, 4, 10);
//	write_mov(dst, src);
//	PRRINT_GEN_CODE("stack <== reg");



//	dst = sh_new_reg_location(R8, 8);
//	src = sh_new_ir_imm_operand(4, RCX, 8);
//	write_mov(dst, src);
//	PRRINT_GEN_CODE("reg[R*] <== imm");

}
#undef PRRINT_GEN_CODE

void testing_op() {

	// write_mov_test();
	// write_cmp_test();
	write_add_test();
	// write_lea_test();

	// write_float_test();

}





void gen_setup_stack() {
	write_mov(sh_new_reg_location(RAX, 8), sh_new_reg_location(RSP, 8));
	write_mov(sh_new_reg_location(RCX, 8), sh_new_reg_location(RBP, 8));


	write_to_register(RSP, (char*)&stack_top, 8); // what D: 
	write_to_register(RBP, (char*)&stack_base, 8); // what D:  increase down wars i.e: from mem location 1 to 0 on push
	write_push_stack_register(RAX);
	write_push_stack_register(RCX);


}

void print_code() {

	i32 main_mem_len = (i32)( writer_head - main_mem );
	i32 len_acc = 0;

	puts("======================================");
	puts("Code Gen:\n");
	xed_init();

	for(int i = 0; i < buf_len(decls); i++) {
		sh_decl *d = decls[i];

		if(d->storage == SH_GLOBAL_STORAGE && !is_dll_import(d)) {
			printf("-----------%s-------------\n", d->name);

			i32 mem_len = d->mem_info->mem_size;
			char *mem_addr = (char*)d->mem_info->mem_address;
			i32 len_acc = 0;
			while(mem_len > 0) {
				printf("%0.5d: ", len_acc);
				i32 length = print_gen_code(mem_addr + len_acc);
				mem_len -= length;
				len_acc += length;
			}

			puts("----------------------------");
		}
		
	}

	puts("=================");
}


void memory_setup() {
	SYSTEM_INFO sys_inf;
	GetSystemInfo(&sys_inf);

	i32 page_count = 1;
	i32 page_in_bytes = page_count*sys_inf.dwPageSize;

	main_mem = (u8*) VirtualAlloc(NULL, page_in_bytes, MEM_COMMIT | MEM_RESERVE, PAGE_EXECUTE_READWRITE);

	FillMemory((void*)main_mem, page_in_bytes, 0xC3);

	writer_head = main_mem;
	end_of_mem = main_mem + page_in_bytes;
	stack_base = (char*)end_of_mem; //  stack starts at the end of mem, goes "down" in address 
	stack_top = (char*)end_of_mem; //  stack starts at the end of mem, goes "down" in address 
	location_of_printf = (char*)printf;
	data_section = stack_base - stack_size - data_section_size;
	data_section_header = data_section;
	location_of_printf = (char*)printf;

	write_data_section((char*)&location_of_printf, 8);


}


void gen_main() {

	xed_init();
	memory_setup();
	rand_func f = NULL;
#if 0
	testing_op();
	f = (rand_func)main_mem;
#else

	/* gen_setup_stack(); */

	for(sh_decl **d = decls;d != buf_end(decls); d++) {
		if(d[0]->storage == SH_GLOBAL_STORAGE) {
			sh_gen_decl(*d);
		}
	}

	print_code();
	f = (rand_func) main_func_address;
#endif

	__try {
		fflush(stdout);
		f64 x = f();
		printf("%f\n", x);
	}
	__except(EXCEPTION_EXECUTE_HANDLER) {
		printf("yeet");
	}


}
