/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

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

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


/* Inlines for looking up attribute values */

/*REFERENCED*/
inline mUINT32
OPERATOR_is_scf (OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return (mUINT32) OPERATOR_info [op]._flags & OPERATOR_PROPERTY_scf;
}

/*REFERENCED*/
inline mUINT32
OPERATOR_is_stmt (OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return (mUINT32) OPERATOR_info [op]._flags & OPERATOR_PROPERTY_stmt;
}

/*REFERENCED*/
inline mUINT32
OPERATOR_is_expression(OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return (mUINT32) OPERATOR_info [op]._flags & OPERATOR_PROPERTY_expression;
}

/*REFERENCED*/
inline mUINT32
OPERATOR_is_leaf (OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return (mUINT32) OPERATOR_info [op]._flags & OPERATOR_PROPERTY_leaf;
}

/*REFERENCED*/
inline mUINT32
OPERATOR_is_store (OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return (mUINT32) OPERATOR_info [op]._flags & OPERATOR_PROPERTY_store;
}

/*REFERENCED*/
inline mUINT32
OPERATOR_is_load (OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return (mUINT32) OPERATOR_info [op]._flags & OPERATOR_PROPERTY_load;
}

/*REFERENCED*/
inline mUINT32
OPERATOR_is_call(OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return (mUINT32) OPERATOR_info [op]._flags & OPERATOR_PROPERTY_call;
}

/*REFERENCED*/
inline mUINT32
OPERATOR_is_compare (OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return (mUINT32) OPERATOR_info [op]._flags & OPERATOR_PROPERTY_compare;
}

/*REFERENCED*/
inline mUINT32
OPERATOR_is_non_scf (OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return (mUINT32) OPERATOR_info [op]._flags & OPERATOR_PROPERTY_non_scf;
}

/*REFERENCED*/
inline mUINT32
OPERATOR_is_boolean (OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return (mUINT32) OPERATOR_info [op]._flags & OPERATOR_PROPERTY_boolean;
}

/*REFERENCED*/
inline mUINT32
OPERATOR_is_endsbb (OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return (mUINT32) OPERATOR_info [op]._flags & OPERATOR_PROPERTY_endsbb;
}

/*REFERENCED*/
inline mUINT32
OPERATOR_is_comp_unit_if (OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return (mUINT32) OPERATOR_info [op]._flags & OPERATOR_PROPERTY_comp_unit_if;
}

/*REFERENCED*/
inline mUINT32
OPERATOR_is_not_executable (OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return (mUINT32) OPERATOR_info [op]._flags & OPERATOR_PROPERTY_not_executable;
}

/*REFERENCED*/
inline mUINT32
OPERATOR_is_prefetch (OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return (mUINT32) OPERATOR_info [op]._flags & OPERATOR_PROPERTY_prefetch;
}

/*REFERENCED*/
inline mUINT32
OPERATOR_has_next_prev (OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return (mUINT32) OPERATOR_info [op]._flags & OPERATOR_PROPERTY_next_prev;
}

/*REFERENCED*/
inline mUINT32
OPERATOR_has_sym (OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return (mUINT32) OPERATOR_info [op]._flags & OPERATOR_PROPERTY_sym;
}

/*REFERENCED*/
inline mUINT32
OPERATOR_has_label (OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return (mUINT32) OPERATOR_info [op]._flags & OPERATOR_PROPERTY_label;
}

/*REFERENCED*/
inline mUINT32
OPERATOR_has_num_entries (OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return (mUINT32) OPERATOR_info [op]._flags & OPERATOR_PROPERTY_num_entries;
}

/*REFERENCED*/
inline mUINT32
OPERATOR_has_offset (OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return (mUINT32) OPERATOR_info [op]._flags & OPERATOR_PROPERTY_offset;
}

/*REFERENCED*/
inline mUINT32
OPERATOR_has_2offsets (OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return (mUINT32) OPERATOR_info [op]._flags & OPERATOR_PROPERTY_2offsets;
}

/*REFERENCED*/
inline mUINT32
OPERATOR_has_bits (OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return (mUINT32) OPERATOR_info [op]._flags & OPERATOR_PROPERTY_bits;
}

/*REFERENCED*/
inline mUINT32
OPERATOR_has_ndim (OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return (mUINT32) OPERATOR_info [op]._flags & OPERATOR_PROPERTY_ndim;
}

/*REFERENCED*/
inline mUINT32
OPERATOR_has_esize (OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return (mUINT32) OPERATOR_info [op]._flags & OPERATOR_PROPERTY_esize;
}

/*REFERENCED*/
inline mUINT32
OPERATOR_has_value (OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return (mUINT32) OPERATOR_info [op]._flags & OPERATOR_PROPERTY_value;
}

/*REFERENCED*/
inline mUINT32
OPERATOR_has_flags (OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return (mUINT32) OPERATOR_info [op]._flags & OPERATOR_PROPERTY_flags;
}

/*REFERENCED*/
inline mUINT32
OPERATOR_has_inumber (OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return (mUINT32) OPERATOR_info [op]._flags & OPERATOR_PROPERTY_inumber;
}

/*REFERENCED*/
inline mUINT32
OPERATOR_has_1ty (OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return (mUINT32) OPERATOR_info [op]._flags & OPERATOR_PROPERTY_1ty;
}

/*REFERENCED*/
inline mUINT32
OPERATOR_has_2ty (OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return (mUINT32) OPERATOR_info [op]._flags & OPERATOR_PROPERTY_2ty;
}

/*REFERENCED*/
inline mUINT32
OPERATOR_has_ereg_supp (OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return (mUINT32) OPERATOR_info [op]._flags & OPERATOR_PROPERTY_ereg_supp;
}

/*REFERENCED*/
inline mUINT32
OPERATOR_has_barrier (OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return (mUINT32) OPERATOR_info [op]._flags & OPERATOR_PROPERTY_barrier;
}

/*REFERENCED*/
inline mUINT32
OPERATOR_has_last_label (OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return (mUINT32) OPERATOR_info [op]._flags & OPERATOR_PROPERTY_last_label;
}

/*REFERENCED*/
inline mUINT32
OPERATOR_has_field_id (OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return (mUINT32) OPERATOR_info [op]._flags & OPERATOR_PROPERTY_field_id;
}

/*REFERENCED*/
inline mINT8
OPERATOR_nkids (OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return OPERATOR_info [op].nkids;
}

/*REFERENCED*/
inline OPERATOR_MAPCAT
OPERATOR_mapcat (OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return OPERATOR_info[op].mapcat;
}

/*REFERENCED*/
inline BOOL
OPERATOR_is_black_box (OPERATOR op)
{
  Is_True(op >= OPERATOR_FIRST && op <= OPERATOR_LAST, ("Bad operator %d", op));
  return (op == OPR_IO);
}

/* Compatibility functions */

#define Is_Valid_Opcode Is_Valid_Opcode_FUNC

#ifdef __cplusplus
extern "C" BOOL Is_Valid_Opcode_FUNC (OPCODE op);
BOOL Is_Valid_Opcode_Parts(OPERATOR opr, TYPE_ID rtype, TYPE_ID desc);
#else
extern BOOL Is_Valid_Opcode_FUNC (OPCODE op);
#endif /* __cplusplus */

/*REFERENCED*/
inline OPERATOR
OPCODE_operator (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR) (op & 0xFF);
}

/*REFERENCED*/
inline TYPE_ID
OPCODE_rtype (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (TYPE_ID) ((op >> 8) & 0x3F);
}

/*REFERENCED*/
inline TYPE_ID
OPCODE_desc (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (TYPE_ID) ((op >> 14) & 0x3F);
}

extern char* OPCODE_name (OPCODE op);

/* Inlines for looking up attribute values */

/*REFERENCED*/
inline mUINT32
OPCODE_is_scf (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_is_scf (OPCODE_operator (op)));
}

/*REFERENCED*/
inline mUINT32
OPCODE_is_stmt (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_is_stmt (OPCODE_operator (op)));
}

/*REFERENCED*/
inline mUINT32
OPCODE_is_expression (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_is_expression (OPCODE_operator (op)));
}

/*REFERENCED*/
inline mUINT32
OPCODE_is_leaf (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_is_leaf (OPCODE_operator (op)));
}

/*REFERENCED*/
inline mUINT32
OPCODE_is_store (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_is_store (OPCODE_operator (op)));
}

/*REFERENCED*/
inline mUINT32
OPCODE_is_load (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_is_load (OPCODE_operator (op)));
}

/*REFERENCED*/
inline mUINT32
OPCODE_is_call (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_is_call (OPCODE_operator (op)));
}

/*REFERENCED*/
inline mUINT32
OPCODE_is_compare (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_is_compare (OPCODE_operator (op)));
}

/*REFERENCED*/
inline mUINT32
OPCODE_is_non_scf (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_is_non_scf (OPCODE_operator (op)));
}

/*REFERENCED*/
inline mUINT32
OPCODE_is_boolean (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_is_boolean (OPCODE_operator (op)));
}

/*REFERENCED*/
inline mUINT32
OPCODE_is_endsbb (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_is_endsbb (OPCODE_operator (op)));
}

/*REFERENCED*/
inline mUINT32
OPCODE_is_comp_unit_if (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_is_comp_unit_if (OPCODE_operator (op)));
}

/*REFERENCED*/
inline mUINT32
OPCODE_is_not_executable (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_is_not_executable (OPCODE_operator (op)));
}

/*REFERENCED*/
inline mUINT32
OPCODE_is_prefetch (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_is_prefetch (OPCODE_operator (op)));
}

/*REFERENCED*/
inline mUINT32
OPCODE_has_next_prev (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_has_next_prev (OPCODE_operator (op)));
}

/*REFERENCED*/
inline mUINT32
OPCODE_has_sym (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_has_sym (OPCODE_operator (op)));
}

/*REFERENCED*/
inline mUINT32
OPCODE_has_label (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_has_label (OPCODE_operator (op)));
}

/*REFERENCED*/
inline mUINT32
OPCODE_has_num_entries (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_has_num_entries (OPCODE_operator (op)));
}

/*REFERENCED*/
inline mUINT32
OPCODE_has_offset (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_has_offset (OPCODE_operator (op)));
}

/*REFERENCED*/
inline mUINT32
OPCODE_has_2offsets (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_has_2offsets (OPCODE_operator (op)));
}

/*REFERENCED*/
inline mUINT32
OPCODE_has_bits (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_has_bits (OPCODE_operator (op)));
}

/*REFERENCED*/
inline mUINT32
OPCODE_has_ndim (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_has_ndim (OPCODE_operator (op)));
}

/*REFERENCED*/
inline mUINT32
OPCODE_has_esize (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_has_esize (OPCODE_operator (op)));
}

/*REFERENCED*/
inline mUINT32
OPCODE_has_value (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_has_value (OPCODE_operator (op)));
}

/*REFERENCED*/
inline mUINT32
OPCODE_has_flags (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_has_flags (OPCODE_operator (op)));
}

/*REFERENCED*/
inline mUINT32
OPCODE_has_inumber (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_has_inumber (OPCODE_operator (op)));
}

/*REFERENCED*/
inline mUINT32
OPCODE_has_1ty (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_has_1ty (OPCODE_operator (op)));
}

/*REFERENCED*/
inline mUINT32
OPCODE_has_2ty (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_has_2ty (OPCODE_operator (op)));
}

/*REFERENCED*/
inline mUINT32
OPCODE_has_ereg_supp (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_has_ereg_supp (OPCODE_operator (op)));
}

/*REFERENCED*/
inline mUINT32
OPCODE_has_barrier (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_has_barrier (OPCODE_operator (op)));
}

/*REFERENCED*/
inline mUINT32
OPCODE_has_last_label (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_has_last_label (OPCODE_operator (op)));
}

/*REFERENCED*/
inline mUINT32
OPCODE_has_field_id (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_has_field_id (OPCODE_operator (op)));
}

/*REFERENCED*/
inline mINT8
OPCODE_nkids(OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_nkids (OPCODE_operator (op)));
}

/*REFERENCED*/
inline OPERATOR_MAPCAT
OPCODE_mapcat (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_mapcat (OPCODE_operator (op)));
}

/*REFERENCED*/
inline BOOL
OPCODE_is_black_box (OPCODE op)
{
  Is_True(Is_Valid_Opcode (op), ("Bad opcode %d", op));
  return (OPERATOR_is_black_box (OPCODE_operator (op)));
}
