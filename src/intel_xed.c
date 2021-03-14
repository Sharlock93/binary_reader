#include "../include/xed/xed-interface.h"
#include "../include/xed/xed-print-info.h"

enum { BUF_SIZE = 1024 };

char buf[BUF_SIZE];

xed_machine_mode_enum_t mmode = XED_MACHINE_MODE_LONG_64;
xed_address_width_enum_t stack_addr_width =  XED_ADDRESS_WIDTH_64b;

xed_decoded_inst_t inst = {0};
xed_print_info_t print_info = {0};

void xed_init() {
	
	;
	xed_bool_t long_mode = 0;
	xed_tables_init();


	xed_decoded_inst_t inst;
	xed_decoded_inst_zero(&inst);
	xed_decoded_inst_set_mode(&inst, mmode, stack_addr_width);

	xed_init_print_info(&print_info);

	print_info.blen = 1024;
	print_info.buf = buf;
	print_info.p = &inst;

}


i32 print_gen_code(char *ptr) {

	xed_decoded_inst_zero_keep_mode(&inst);
	xed_decoded_inst_set_mode(&inst, mmode, stack_addr_width);

	xed_decode(&inst, (const xed_uint8_t*)ptr, 15);
	i32 inst_decoded_length = xed_decoded_inst_get_length(&inst);


	print_info.p = &inst;
	xed_format_generic(&print_info);


	printf("%s\n", buf);
	return inst_decoded_length;
}
