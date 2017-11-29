/*

  Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  You should have received a copy of the GNU Lesser General Public
  License along with this program; if not, write the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307,
  USA.

*/

#include "config.h"
#include "dwarf_stuff.h"
#include "libdwarfdefs.h"
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include "pro_incl.h"
#include "pro_expr.h"

/* adds an attribute to a die */
extern void _dwarf_pro_add_at_to_die(Dwarf_P_Die die,
				     Dwarf_P_Attribute attr);

/* Bug 1188
   There is probably a bug in somewhere dealing with the data width
   optimization (data format depends on the best fit for the incoming value).
   We will use the format based on the incoming data type (intype) rather
   than the best fit. This atleast causes gdb and TotalView to read in the
   .debug_info sections correctly.
*/
Dwarf_P_Attribute
dwf_add_AT_unsigned_const_ext(Dwarf_P_Debug dbg,
			      Dwarf_P_Die ownerdie,
			      Dwarf_Half attr,
			      Dwarf_Unsigned value,
			      Dwarf_Error *error,
			      Dwarf_Unsigned intype)
{
    Dwarf_P_Attribute 	new_attr;
    Dwarf_Half		attr_form;
    Dwarf_Small		size;

    if (dbg == NULL) {
        _dwarf_p_error(NULL, error, DW_DLE_DBG_NULL);
        return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    if (ownerdie == NULL) {
        _dwarf_p_error(dbg, error, DW_DLE_DIE_NULL);
        return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    if (attr != DW_AT_const_value ||
	(intype != 1 && intype != 2 && intype != 4 && intype != 8)) {
      _dwarf_p_error(dbg, error, DW_DLE_INPUT_ATTR_BAD);
      return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    /*
       Compute the number of bytes
       needed to hold constant.
    */
    switch (intype) {
    case 1: attr_form = DW_FORM_data1; size = 1; break;
    case 2: attr_form = DW_FORM_data2; size = 2; break;
    case 4: attr_form = DW_FORM_data4; size = 4; break;
    case 8: attr_form = DW_FORM_data8; size = 8; break;
    }

    new_attr = (Dwarf_P_Attribute)
        _dwarf_p_get_alloc(dbg, sizeof(struct Dwarf_P_Attribute_s));
    if (new_attr == NULL) {
        _dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL);
        return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    new_attr->ar_attribute = attr;
    new_attr->ar_attribute_form = attr_form;
#if defined(BUILD_OS_DARWIN) /* Should be related to target */
#define NO_RELOCATION (~0)
    new_attr->ar_rel_type = NO_RELOCATION;
#else
    new_attr->ar_rel_type = R_MIPS_NONE;
#endif /* ! defined(BUILD_OS_DARWIN) */
    new_attr->ar_reloc_len = 0 ; /* irrelevant: unused with R_MIPS_NONE */
    new_attr->ar_nbytes = size;
    new_attr->ar_next  = 0;

    new_attr->ar_data = (char *)
        _dwarf_p_get_alloc(dbg, size);
    if (new_attr->ar_data == NULL) {
        _dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL);
        return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }
    WRITE_UNALIGNED(dbg,new_attr->ar_data,
	(const void *)&value,
	sizeof(value),
		size);

    /* add attribute to the die */
    _dwarf_pro_add_at_to_die(ownerdie, new_attr);
    return new_attr;
}

/* Bug 1785
   Complex constant (Fortran parameters) should be encoded as 8 byte or 16 byte
   blocks. This subroutine is just to keep things around simple.
*/
Dwarf_P_Attribute
dwf_add_AT_complex_const(Dwarf_P_Debug	dbg,
			 Dwarf_P_Die ownerdie,
			 Dwarf_Half attr,
			 Dwarf_Unsigned	value1,
			 Dwarf_Unsigned	value2,
			 Dwarf_Error *error,
			 Dwarf_Unsigned intype)
{
    char encode_buffer[ENCODE_SPACE_NEEDED];
    Dwarf_P_Attribute 	new_attr;
    Dwarf_Small		size;
    char value[17];
    int index, shift;
    char *block_dest_ptr;

    if (dbg == NULL) {
        _dwarf_p_error(NULL, error, DW_DLE_DBG_NULL);
        return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    if (ownerdie == NULL) {
        _dwarf_p_error(dbg, error, DW_DLE_DIE_NULL);
        return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    if (attr != DW_AT_const_value ||
	(intype != 4 && intype != 8)) {
      _dwarf_p_error(dbg, error, DW_DLE_INPUT_ATTR_BAD);
      return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    /*
       Compute the number of bytes
       needed to hold constant.
    */
    switch (intype) {
    case 4: size = 8; break;
    case 8: size = 16; break;
    }

    value[0] = size;
    for (index = 0; index < size/2; index ++)
      value[index+1] = (value1 >> (index * 8)) & 0xff;
    for (index = 0; index < size/2; index ++)
      value[index+size/2+1] = (value2 >> (index * 8)) & 0xff;

    new_attr = (Dwarf_P_Attribute)
        _dwarf_p_get_alloc(dbg, sizeof(struct Dwarf_P_Attribute_s));
    if (new_attr == NULL) {
        _dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL);
        return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    new_attr->ar_attribute = attr;
    new_attr->ar_attribute_form = DW_FORM_block;
#if defined(BUILD_OS_DARWIN) /* Should be related to target */
    new_attr->ar_rel_type = NO_RELOCATION;
#else /* defined(BUILD_OS_DARWIN) */
    new_attr->ar_rel_type = R_MIPS_NONE;
#endif /* defined(BUILD_OS_DARWIN) */
    new_attr->ar_reloc_len = 0 ; /* irrelevant: unused with R_MIPS_NONE */
    new_attr->ar_nbytes = size+1;
    new_attr->ar_next  = 0;

    new_attr->ar_data = block_dest_ptr = (char *)
        _dwarf_p_get_alloc(dbg, size+1);
    if (new_attr->ar_data == NULL) {
        _dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL);
        return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    memcpy(block_dest_ptr, &value[0], size+1);

    /* add attribute to the die */
    _dwarf_pro_add_at_to_die(ownerdie, new_attr);
    return new_attr;
}
