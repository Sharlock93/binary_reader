struct offset_table {
	u8[4] sfnf_ver @print(h);
	u16be num_tables;
	u16be searchRange;
	u16be entrySelector;
	u16be range_shift;
}

struct format4 {
	u16be seg2;
	u16be searchRange;
	u16be entrySel;
	u16be range_shift;
	u16be[.seg2/2] endCode  @array_print(10, -, -, 0, 0);
	u16be reserve;
	u16be[.seg2/2] startCode  @array_print(10, -, -, 0, 0);
	i16be[.seg2/2] idDelta @array_print(10, -, -, 0, 0);
	u16be[.seg2/2] idRangeOffset @array_print(10, -, -, 0, 0);
	u16be[?] glypharr @array_print(10, -, -, 0, 0);
}

struct encoding_general {
	u16be format;
	u16be length;
	u16be lang;
	when .format {
		4 -> format4,
		else -> u8[.length]
	} format_data @array_print(10, -, -, 0, 0);
}

struct encoding_table {
	u16be plat_id;
	u16be encode_id;
	u32be offset_sub;
	encoding_general dat @offset(^^start, .offset_sub);
}


struct cmap_type {
	u16be version;
	u16be num_tables;
	encoding_table[.num_tables] encoding_tables;
}

struct name_record {
	u16be plat_id;
	u16be encode_id;
	u16be lang_id;
	u16be name_id;
	u16be length;
	u16be offset;
	when .plat_id {
		1 -> char[.length],
		else -> u8[.length]
	} str_data @offset(^^start, .offset + @.@.@.offset) @array_print(0, -, -, 0, 0);
}

struct name_table {
	u16be format;
	u16be count;
	u16be offset;
	name_record[.count] name_records @struct_print(0, 0); 
	
}

struct table_entry {
	char[4] tag;
	u32be check_sum;
	u32be offset;
	u32be length;
	when .tag {
		"cmap" -> cmap_type,
		"name" -> name_table,
		else -> u8[.length]
	} table_data @offset(file_start, .offset) @array_print(10, -, -, 0, 0) @struct_print(1, 1);
}


struct font {
	offset_table off_table;
	table_entry[.off_table.num_tables] table_directory @struct_print(1, 0);
}

font f;
