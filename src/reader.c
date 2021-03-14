typedef struct sh_binary_data {
	size_t len;
	i32 consumed;
	u8 *mem;
	u8 *current_ptr;
} sh_binary_data;


void read_mem_be(u8 *src, u8 *dest, i32 size) {
	for(i32 i = 0; i < size; i++ ) {
		dest[size - i - 1] = src[i];
	}
}

void read_mem(u8 *src, u8 *dest, i32 size) {
	for(i32 i = 0; i < size; i++ ) {
		dest[i] = src[i];
	}
}

typedef void (*read_mem_type)(u8*,u8*,i32);



void map_mem_to_var(sh_decl *var_decl, sh_binary_data *binary_data) {

	i32 remaining_bytes = binary_data->len - binary_data->consumed;
	if(remaining_bytes <= var_decl->total_size) {
		printf("bytes remaining: %d smaller than request bytes: %d\n",
				remaining_bytes,
				var_decl->total_size
		);

		assert(remaining_bytes >= var_decl->total_size);
	}

	switch(var_decl->type) {
		case SH_VAR_DECL: {

			u8 *dest = &var_decl->vu8;

			if(var_decl->base_type >= SH_DECL_I8BE) { // is it big end
				read_mem_be(binary_data->current_ptr, dest, var_decl->total_size);
			} else {
				read_mem(binary_data->current_ptr, dest, var_decl->total_size);
			}

		} break;

		case SH_ARRAY_DECL: {

			var_decl->array = (void *) malloc(var_decl->total_size*sizeof(u8));

			read_mem_type func = read_mem;
			if(var_decl->base_type >= SH_DECL_I8BE) { // is it big end
				func = read_mem_be;
			}

			for(i32 i = 0; i < var_decl->array_size; i++) {
				u8 *dest = (u8 *)var_decl->array + i*var_decl->base_type_size;
				func(binary_data->current_ptr + i*var_decl->base_type_size, dest, var_decl->base_type_size);
			}

		} break;
	}

}

void parse_binary_file(sh_decl *decls, sh_binary_data *bin) {
	for(i32 i = 0; i < buf_len(decls); i++) {
		map_mem_to_var(decls + i, bin);
		bin->consumed += decls[i].total_size;
		bin->current_ptr += decls[i].total_size;
	}
}


void read_mem_test() {
	i32 yeet[] = {1, 2, 3, 4, 5};
	i32 yeet2[2] = {0};

	read_mem((u8 *)yeet, (u8 *)yeet2, 8);

	for(int i = 0; i < 2; i++) {
		printf("%d\n", yeet2[i]);
	}
}


void print_decl_value(sh_decl *decl) {

	printf("decl_name: %s, len: %d, type: %s",
		decl->name, decl->total_size,
		get_base_type_name(decl));

	switch(decl->type) {
		case SH_ARRAY_DECL: {
			printf("[%d]: ", decl->array_size);

			if(decl->base_type == SH_DECL_STRING) {
				printf("%.*s\n", decl->string_size, decl->string_array);
			} else {
				printf("\n");
				for(i32 i = 0; i < decl->array_size; i++) {
					printf("\t%d\n", decl->i32_array[i]);
				} 
			}

		} break;
		default: {
			printf(": %lld\n", decl->vi64);
		} break;
	}


}


