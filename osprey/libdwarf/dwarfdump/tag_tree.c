/* 
  Copyright (C) 2000,2004 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement
  or the like.  Any license provided herein, whether implied or
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with
  other software, or any other product whatsoever.

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1500 Crittenden Lane,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan



$Header: /proj/osprey/CVS/open64/osprey1.0/libdwarf/dwarfdump/tag_tree.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
#include <dwarf.h>
#include <stdio.h>

static unsigned int tag_tree_combination_table[0x36][2];
static char *tag_name[ ] = {
	"0x00", 
	"0x01 DW_TAG_array_type", 
	"0x02 DW_TAG_class_type", 
	"0x03 DW_TAG_entry_point", 
	"0x04 DW_TAG_enumeration_type", 
	"0x05 DW_TAG_formal_parameter", 
	"0x06", 
	"0x07", 
	"0x08 DW_TAG_imported_declaration", 
	"0x09", 
	"0x0a DW_TAG_label", 
	"0x0b DW_TAG_lexical_block", 
	"0x0c", 
	"0x0d DW_TAG_member", 
	"0x0e", 
	"0x0f DW_TAG_pointer_type", 
	"0x10 DW_TAG_reference_type", 
	"0x11 DW_TAG_compile_unit", 
	"0x12 DW_TAG_string_type", 
	"0x13 DW_TAG_structure_type", 
	"0x14", 
	"0x15 DW_TAG_subroutine_type", 
	"0x16 DW_TAG_typedef", 
	"0x17 DW_TAG_union_type", 
	"0x18 DW_TAG_unspecified_parameters", 
	"0x19 DW_TAG_variant", 
	"0x1a DW_TAG_common_block", 
	"0x1b DW_TAG_common_inclusion", 
	"0x1c DW_TAG_inheritance", 
	"0x1d DW_TAG_inlined_subroutine", 
	"0x1e DW_TAG_module", 
	"0x1f DW_TAG_ptr_to_member_type", 
	"0x20 DW_TAG_set_type", 
	"0x21 DW_TAG_subrange_type", 
	"0x22 DW_TAG_with_stmt", 
	"0x23 DW_TAG_access_declaration", 
	"0x24 DW_TAG_base_type", 
	"0x25 DW_TAG_catch_block", 
	"0x26 DW_TAG_const_type", 
	"0x27 DW_TAG_constant", 
	"0x28 DW_TAG_enumerator", 
	"0x29 DW_TAG_file_type", 
	"0x2a DW_TAG_friend", 
	"0x2b DW_TAG_namelist", 
	"0x2c DW_TAG_namelist_item", 
	"0x2d DW_TAG_packed_type", 
	"0x2e DW_TAG_subprogram", 
	"0x2f DW_TAG_template_type_parameter", 
	"0x30 DW_TAG_template_value_parameter", 
	"0x31 DW_TAG_thrown_type", 
	"0x32 DW_TAG_try_block", 
	"0x33 DW_TAG_variant_part", 
	"0x34 DW_TAG_variable", 
	"0x35 DW_TAG_volatile_type", 
};

int
main ()
{
    int i;
    int num;
    scanf("%x\n", &num); /* 0xffffffff */
    while (! feof(stdin)) {
	int tag;
	scanf("%x\n", &tag);
	scanf("%x\n", &num);
	while (num != 0xffffffff) {
	    int idx = num / 0x20;
	    int bit = num % 0x20;
	    tag_tree_combination_table[tag][idx] |= (1 << bit);
	    scanf("%x\n", &num);
	}
    }
    printf("static unsigned int tag_tree_combination_table [ ][2] = {\n");
    for (i = 0; i < 0x36; i ++) {
	printf("/* %-37s*/\n", tag_name[i]);
	printf("    { %#.8x, %#.8x},\n", 
	       tag_tree_combination_table[i][0], 
	       tag_tree_combination_table[i][1]);
    }
    printf("};\n");
    return(0);
}
