/*
 * Copyright 2005-2008 NVIDIA Corporation.  All rights reserved.
 */

/*

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

*/


/* CGEXP routines for loads and stores */
#include <map>
#include "elf_stuff.h"
#include "defs.h"
#include "em_elf.h"
#include "erglob.h"
#include "erbe.h"
#include "tracing.h"
#include "config.h"
#include "config_debug.h"
#include "xstats.h"
#include "topcode.h"
#include "tn.h"
#include "cg_flags.h"
#include "targ_isa_enums.h"
#include "targ_isa_lits.h"
#include "op.h"
#include "stblock.h"
#include "data_layout.h"
#include "strtab.h"
#include "symtab.h"
#include "cg.h"
#include "cgexp.h"
#include "cgexp_internals.h"
#include "cgemit.h"	// for CG_emit_non_gas_syntax
#include "be_symtab.h"
#include "whirl2ops.h"

std::map<pair<UINT,INT64>, TN*> st_to_tn_map;

void Exp_Ldst_Init (void)
{
	st_to_tn_map.clear();
}

static const TOP load_top[11] = {
  TOP_ld_qualifier_space_s8, TOP_ld_qualifier_space_s16, 
  TOP_ld_qualifier_space_s32, TOP_ld_qualifier_space_s64,
  TOP_ld_qualifier_space_u8, TOP_ld_qualifier_space_u16, 
  TOP_ld_qualifier_space_u32, TOP_ld_qualifier_space_u64,
  TOP_ld_qualifier_space_f32, TOP_ld_qualifier_space_f64
};
static const TOP loado_top[11] = {
  TOP_ld_qualifier_space_s8_o, TOP_ld_qualifier_space_s16_o, 
  TOP_ld_qualifier_space_s32_o, TOP_ld_qualifier_space_s64_o,
  TOP_ld_qualifier_space_u8_o, TOP_ld_qualifier_space_u16_o, 
  TOP_ld_qualifier_space_u32_o, TOP_ld_qualifier_space_u64_o,
  TOP_ld_qualifier_space_f32_o, TOP_ld_qualifier_space_f64_o
};
static const TOP loadr_top[11] = {
  TOP_ld_qualifier_space_s8_r, TOP_ld_qualifier_space_s16_r, 
  TOP_ld_qualifier_space_s32_r, TOP_ld_qualifier_space_s64_r,
  TOP_ld_qualifier_space_u8_r, TOP_ld_qualifier_space_u16_r, 
  TOP_ld_qualifier_space_u32_r, TOP_ld_qualifier_space_u64_r,
  TOP_ld_qualifier_space_f32_r, TOP_ld_qualifier_space_f64_r
};
static const TOP load32_top[11] = {
  TOP_ld_qualifier_space_s8_b32, TOP_ld_qualifier_space_s16_b32, 
  TOP_UNDEFINED, TOP_UNDEFINED,
  TOP_ld_qualifier_space_u8_b32, TOP_ld_qualifier_space_u16_b32, 
  TOP_UNDEFINED, TOP_UNDEFINED,
  TOP_UNDEFINED, TOP_UNDEFINED
};
static const TOP load32o_top[11] = {
  TOP_ld_qualifier_space_s8_b32_o, TOP_ld_qualifier_space_s16_b32_o, 
  TOP_UNDEFINED, TOP_UNDEFINED,
  TOP_ld_qualifier_space_u8_b32_o, TOP_ld_qualifier_space_u16_b32_o, 
  TOP_UNDEFINED, TOP_UNDEFINED,
  TOP_UNDEFINED, TOP_UNDEFINED
};
static const TOP load32r_top[11] = {
  TOP_ld_qualifier_space_s8_b32_r, TOP_ld_qualifier_space_s16_b32_r, 
  TOP_UNDEFINED, TOP_UNDEFINED,
  TOP_ld_qualifier_space_u8_b32_r, TOP_ld_qualifier_space_u16_b32_r, 
  TOP_UNDEFINED, TOP_UNDEFINED,
  TOP_UNDEFINED, TOP_UNDEFINED
};
static const TOP loade_top[11] = {
  TOP_ld_qualifier_space_s8_a64, TOP_ld_qualifier_space_s16_a64, 
  TOP_ld_qualifier_space_s32_a64, TOP_ld_qualifier_space_s64_a64,
  TOP_ld_qualifier_space_u8_a64, TOP_ld_qualifier_space_u16_a64, 
  TOP_ld_qualifier_space_u32_a64, TOP_ld_qualifier_space_u64_a64,
  TOP_ld_qualifier_space_f32_a64, TOP_ld_qualifier_space_f64_a64
};
static const TOP loadeo_top[11] = {
  TOP_ld_qualifier_space_s8_a64_o, TOP_ld_qualifier_space_s16_a64_o, 
  TOP_ld_qualifier_space_s32_a64_o, TOP_ld_qualifier_space_s64_a64_o,
  TOP_ld_qualifier_space_u8_a64_o, TOP_ld_qualifier_space_u16_a64_o, 
  TOP_ld_qualifier_space_u32_a64_o, TOP_ld_qualifier_space_u64_a64_o,
  TOP_ld_qualifier_space_f32_a64_o, TOP_ld_qualifier_space_f64_a64_o
};
static const TOP loader_top[11] = {
  TOP_ld_qualifier_space_s8_a64_r, TOP_ld_qualifier_space_s16_a64_r, 
  TOP_ld_qualifier_space_s32_a64_r, TOP_ld_qualifier_space_s64_a64_r,
  TOP_ld_qualifier_space_u8_a64_r, TOP_ld_qualifier_space_u16_a64_r, 
  TOP_ld_qualifier_space_u32_a64_r, TOP_ld_qualifier_space_u64_a64_r,
  TOP_ld_qualifier_space_f32_a64_r, TOP_ld_qualifier_space_f64_a64_r
};
static const TOP loade32_top[11] = {
  TOP_ld_qualifier_space_s8_b32_a64, TOP_ld_qualifier_space_s16_b32_a64, 
  TOP_UNDEFINED, TOP_UNDEFINED,
  TOP_ld_qualifier_space_u8_b32_a64, TOP_ld_qualifier_space_u16_b32_a64, 
  TOP_UNDEFINED, TOP_UNDEFINED,
  TOP_UNDEFINED, TOP_UNDEFINED
};
static const TOP loade32o_top[11] = {
  TOP_ld_qualifier_space_s8_b32_a64_o, TOP_ld_qualifier_space_s16_b32_a64_o, 
  TOP_UNDEFINED, TOP_UNDEFINED,
  TOP_ld_qualifier_space_u8_b32_a64_o, TOP_ld_qualifier_space_u16_b32_a64_o, 
  TOP_UNDEFINED, TOP_UNDEFINED,
  TOP_UNDEFINED, TOP_UNDEFINED
};
static const TOP loade32r_top[11] = {
  TOP_ld_qualifier_space_s8_b32_a64_r, TOP_ld_qualifier_space_s16_b32_a64_r, 
  TOP_UNDEFINED, TOP_UNDEFINED,
  TOP_ld_qualifier_space_u8_b32_a64_r, TOP_ld_qualifier_space_u16_b32_a64_r, 
  TOP_UNDEFINED, TOP_UNDEFINED,
  TOP_UNDEFINED, TOP_UNDEFINED
};

static const TOP store_top[11] = {
  TOP_st_qualifier_space_s8, TOP_st_qualifier_space_s16, 
  TOP_st_qualifier_space_s32, TOP_st_qualifier_space_s64,
  TOP_st_qualifier_space_u8, TOP_st_qualifier_space_u16, 
  TOP_st_qualifier_space_u32, TOP_st_qualifier_space_u64,
  TOP_st_qualifier_space_f32, TOP_st_qualifier_space_f64
};
static const TOP storeo_top[11] = {
  TOP_st_qualifier_space_s8_o, TOP_st_qualifier_space_s16_o, 
  TOP_st_qualifier_space_s32_o, TOP_st_qualifier_space_s64_o,
  TOP_st_qualifier_space_u8_o, TOP_st_qualifier_space_u16_o, 
  TOP_st_qualifier_space_u32_o, TOP_st_qualifier_space_u64_o,
  TOP_st_qualifier_space_f32_o, TOP_st_qualifier_space_f64_o
};
static const TOP storer_top[11] = {
  TOP_st_qualifier_space_s8_r, TOP_st_qualifier_space_s16_r, 
  TOP_st_qualifier_space_s32_r, TOP_st_qualifier_space_s64_r,
  TOP_st_qualifier_space_u8_r, TOP_st_qualifier_space_u16_r, 
  TOP_st_qualifier_space_u32_r, TOP_st_qualifier_space_u64_r,
  TOP_st_qualifier_space_f32_r, TOP_st_qualifier_space_f64_r
};
static const TOP store32_top[11] = {
  TOP_st_qualifier_space_s8_b32, TOP_st_qualifier_space_s16_b32, 
  TOP_UNDEFINED, TOP_UNDEFINED,
  TOP_st_qualifier_space_u8_b32, TOP_st_qualifier_space_u16_b32, 
  TOP_UNDEFINED, TOP_UNDEFINED,
  TOP_UNDEFINED, TOP_UNDEFINED
};
static const TOP store32o_top[11] = {
  TOP_st_qualifier_space_s8_b32_o, TOP_st_qualifier_space_s16_b32_o, 
  TOP_UNDEFINED, TOP_UNDEFINED,
  TOP_st_qualifier_space_u8_b32_o, TOP_st_qualifier_space_u16_b32_o, 
  TOP_UNDEFINED, TOP_UNDEFINED,
  TOP_UNDEFINED, TOP_UNDEFINED
};
static const TOP store32r_top[11] = {
  TOP_st_qualifier_space_s8_b32_r, TOP_st_qualifier_space_s16_b32_r, 
  TOP_UNDEFINED, TOP_UNDEFINED,
  TOP_st_qualifier_space_u8_b32_r, TOP_st_qualifier_space_u16_b32_r, 
  TOP_UNDEFINED, TOP_UNDEFINED,
  TOP_UNDEFINED, TOP_UNDEFINED
};
static const TOP storee_top[11] = {
  TOP_st_qualifier_space_s8_a64, TOP_st_qualifier_space_s16_a64, 
  TOP_st_qualifier_space_s32_a64, TOP_st_qualifier_space_s64_a64,
  TOP_st_qualifier_space_u8_a64, TOP_st_qualifier_space_u16_a64, 
  TOP_st_qualifier_space_u32_a64, TOP_st_qualifier_space_u64_a64,
  TOP_st_qualifier_space_f32_a64, TOP_st_qualifier_space_f64_a64
};
static const TOP storeeo_top[11] = {
  TOP_st_qualifier_space_s8_a64_o, TOP_st_qualifier_space_s16_a64_o, 
  TOP_st_qualifier_space_s32_a64_o, TOP_st_qualifier_space_s64_a64_o,
  TOP_st_qualifier_space_u8_a64_o, TOP_st_qualifier_space_u16_a64_o, 
  TOP_st_qualifier_space_u32_a64_o, TOP_st_qualifier_space_u64_a64_o,
  TOP_st_qualifier_space_f32_a64_o, TOP_st_qualifier_space_f64_a64_o
};
static const TOP storeer_top[11] = {
  TOP_st_qualifier_space_s8_a64_r, TOP_st_qualifier_space_s16_a64_r, 
  TOP_st_qualifier_space_s32_a64_r, TOP_st_qualifier_space_s64_a64_r,
  TOP_st_qualifier_space_u8_a64_r, TOP_st_qualifier_space_u16_a64_r, 
  TOP_st_qualifier_space_u32_a64_r, TOP_st_qualifier_space_u64_a64_r,
  TOP_st_qualifier_space_f32_a64_r, TOP_st_qualifier_space_f64_a64_r
};
static const TOP storee32_top[11] = {
  TOP_st_qualifier_space_s8_b32_a64, TOP_st_qualifier_space_s16_b32_a64, 
  TOP_UNDEFINED, TOP_UNDEFINED,
  TOP_st_qualifier_space_u8_b32_a64, TOP_st_qualifier_space_u16_b32_a64, 
  TOP_UNDEFINED, TOP_UNDEFINED,
  TOP_UNDEFINED, TOP_UNDEFINED
};
static const TOP storee32o_top[11] = {
  TOP_st_qualifier_space_s8_b32_a64_o, TOP_st_qualifier_space_s16_b32_a64_o, 
  TOP_UNDEFINED, TOP_UNDEFINED,
  TOP_st_qualifier_space_u8_b32_a64_o, TOP_st_qualifier_space_u16_b32_a64_o, 
  TOP_UNDEFINED, TOP_UNDEFINED,
  TOP_UNDEFINED, TOP_UNDEFINED
};
static const TOP storee32r_top[11] = {
  TOP_st_qualifier_space_s8_b32_a64_r, TOP_st_qualifier_space_s16_b32_a64_r, 
  TOP_UNDEFINED, TOP_UNDEFINED,
  TOP_st_qualifier_space_u8_b32_a64_r, TOP_st_qualifier_space_u16_b32_a64_r, 
  TOP_UNDEFINED, TOP_UNDEFINED,
  TOP_UNDEFINED, TOP_UNDEFINED
};

static ISA_ENUM_CLASS_VALUE
Space_Enum (VARIANT v, TN *tn)
{
#ifdef Is_True_On
  // check if multiple memory spaces were set
  UINT t = V_memory_space(v);
  UINT n = 0;
  while (t != 0) { // count # bits set
    if ((t & 1) == 1) 
      ++n;
    t = t >> 1;
  }
  if (n > 1) 
    DevWarn("multiple memory spaces set");
#endif
  if (V_global_mem(v)) return ECV_space_global;
  else if (V_shared_mem(v)) return ECV_space_shared;
  else if (V_local_mem(v)) return ECV_space_local;
  else if (V_const_mem(v)) return ECV_space_const;
  else if (V_param_mem(v)) return ECV_space_param;
  else if (TN_has_memory_space(tn)) {
	if (TN_in_global_mem(tn)) return ECV_space_global;
  	else if (TN_in_shared_mem(tn)) return ECV_space_shared;
  	else if (TN_in_local_mem(tn)) return ECV_space_local;
  	else if (TN_in_const_mem(tn)) return ECV_space_const;
  	else if (TN_in_param_mem(tn)) return ECV_space_param;
	else FmtAssert(FALSE, ("unknown memory space"));
  }
  else return ECV_UNDEFINED;
}

static inline TOP
Pick_Load_Instruction (TYPE_ID mtype, BOOL val_is32, BOOL addr_is64, 
	BOOL use_reg, BOOL has_offset)
{
  if (addr_is64) {
    if (val_is32) {
      if (use_reg)
  	return loade32r_top[Mtype_Index(mtype)];
      else if (has_offset)
  	return loade32o_top[Mtype_Index(mtype)];
      else
  	return loade32_top[Mtype_Index(mtype)];
    }
    else {
      if (use_reg)
  	return loader_top[Mtype_Index(mtype)];
      else if (has_offset)
  	return loadeo_top[Mtype_Index(mtype)];
      else
  	return loade_top[Mtype_Index(mtype)];
    }
  }
  else {
    if (val_is32) {
      if (use_reg)
  	return load32r_top[Mtype_Index(mtype)];
      else if (has_offset)
  	return load32o_top[Mtype_Index(mtype)];
      else
  	return load32_top[Mtype_Index(mtype)];
    }
    else {
      if (use_reg)
  	return loadr_top[Mtype_Index(mtype)];
      else if (has_offset)
  	return loado_top[Mtype_Index(mtype)];
      else
  	return load_top[Mtype_Index(mtype)];
    }
  }
}

static inline TOP
Pick_Store_Instruction (TYPE_ID mtype, BOOL val_is32, BOOL addr_is64, 
	BOOL use_reg, BOOL has_offset)
{
  if (addr_is64) {
    if (val_is32) {
      if (use_reg)
  	return storee32r_top[Mtype_Index(mtype)];
      else if (has_offset)
  	return storee32o_top[Mtype_Index(mtype)];
      else
  	return storee32_top[Mtype_Index(mtype)];
    }
    else {
      if (use_reg)
  	return storeer_top[Mtype_Index(mtype)];
      else if (has_offset)
  	return storeeo_top[Mtype_Index(mtype)];
      else
  	return storee_top[Mtype_Index(mtype)];
    }
  }
  else {
    if (val_is32) {
      if (use_reg)
  	return store32r_top[Mtype_Index(mtype)];
      else if (has_offset)
  	return store32o_top[Mtype_Index(mtype)];
      else
  	return store32_top[Mtype_Index(mtype)];
    }
    else {
      if (use_reg)
  	return storer_top[Mtype_Index(mtype)];
      else if (has_offset)
  	return storeo_top[Mtype_Index(mtype)];
      else
  	return store_top[Mtype_Index(mtype)];
    }
  }
}

static void
Set_TN_Memory_Space_From_Variant (TN *tn, VARIANT v, TN *base)
{
  // need to track which memory space an address register is associated with,
  // so know what to issue when indirecting.
  if (V_global_mem(v)) {
	Set_TN_in_global_mem(tn);
  }
  else if (V_shared_mem(v)) {
	Set_TN_in_shared_mem(tn);
  }
  else if (V_param_mem(v)) {
	Set_TN_in_param_mem(tn);
  }
  else if (V_local_mem(v)) {
	Set_TN_in_local_mem(tn);
  }
  else if (V_const_mem(v)) {
	Set_TN_in_const_mem(tn);
  }
  else if (TN_has_memory_space(base)) {
	Set_TN_memory_space(tn, TN_memory_space(base));
  }
  else FmtAssert(FALSE, ("NYI: tn memory type"));
}

void
Expand_Load (OPCODE opcode, TN *result, TN *base, TN *ofst, VARIANT v, OPS *ops)
{
  TOP top;
  const TYPE_ID dtype = OPCODE_desc(opcode);
  const TYPE_ID rtype = OPCODE_rtype(opcode);
  TN *dresult =  result;
  BOOL need_convert = FALSE;
  BOOL val_is32 = FALSE;
  BOOL addr_is64;
  BOOL use_reg;
  BOOL has_offset;
  TN *space_tn = Gen_Enum_TN(Space_Enum(v,base)); 
  TN *qualifier_tn;

  switch (Space_Enum(v,base)) {
  case ECV_space_global:
  case ECV_space_shared:
    break;
  default:
    // ignore volatile on these memories
    Reset_V_volatile(v);
  }
  if (V_volatile(v))
    qualifier_tn = Gen_Enum_TN(ECV_qualifier_volatile);
  else
    qualifier_tn = Gen_Enum_TN(ECV_qualifier_none);

  Is_True(TN_size(result) >= MTYPE_byte_size(rtype), ("load won't fit in result"));
  // check if need convert after load
  if (dtype != rtype && MTYPE_is_integral(rtype)) {
	if ((rtype == MTYPE_U4 || rtype == MTYPE_I4)
	  && MTYPE_byte_size(dtype) < MTYPE_byte_size(rtype))
	{
		// special-case ability to load small value into 32-bit reg
		// if given something like U4U2LDID.
		val_is32 = TRUE;
	} else {
		// will need convert after load, so size tmp on dtype.
		dresult =  Build_TN_Of_Mtype (dtype);
		need_convert = TRUE;
	}
  }
  Is_True (TN_is_constant(ofst), ("Expand_Load: Illegal offset TN"));
  if (TN_is_symbol(base)) {
        use_reg = FALSE;
	addr_is64 = (MTYPE_RegisterSize(ST_mtype(TN_var(base))) == 8);
	if (TN_has_value(ofst) && TN_value(ofst) == 0 
	  && TN_is_symbol(base) && Is_Simple_Type(ST_type(TN_var(base)))) 
	{
		// ld name
		has_offset = FALSE;
	} else {
		// ld name[ofst]
		has_offset = TRUE;
	}
  } 
  else {
	// ld [reg+offset]
        use_reg = TRUE;
	has_offset = TRUE;
	addr_is64 = (TN_register_class(base) == ISA_REGISTER_CLASS_integer64);
  }
  top = Pick_Load_Instruction (dtype, val_is32, addr_is64, use_reg, has_offset);
  FmtAssert(top != TOP_UNDEFINED, ("no topcode"));
  if (has_offset)
  	Build_OP (top, dresult, qualifier_tn, space_tn, base, ofst, ops);
  else
  	Build_OP (top, dresult, qualifier_tn, space_tn, base, ops);

  if (TN_enum(space_tn) == ECV_space_shared) {
	// mark if came from ld.shared,
	// so can later replace this and use grf directly
	Set_TN_from_shared_load(dresult);
  }
  if (need_convert) {
	Expand_Convert (result, rtype, dresult, dtype, ops);
  }
  // set tn space if looks like an address load
  if (rtype == Pointer_Mtype) {
	Set_TN_Memory_Space_From_Variant (result, v, base);
  }
}


void
Expand_Store (TYPE_ID mtype, TN *src, TN *base, TN *ofst, VARIANT v, OPS *ops)
{
  TOP top;
  BOOL val_is32 = FALSE;
  BOOL addr_is64;
  BOOL use_reg;
  BOOL has_offset;
  TN *space_tn = Gen_Enum_TN(Space_Enum(v,base)); 
  TN *qualifier_tn;

  switch (Space_Enum(v,base)) {
  case ECV_space_global:
  case ECV_space_shared:
    break;
  default:
    // ignore volatile on these memories
    Reset_V_volatile(v);
  }
  if (V_volatile(v))
    qualifier_tn = Gen_Enum_TN(ECV_qualifier_volatile);
  else
    qualifier_tn = Gen_Enum_TN(ECV_qualifier_none);


  if (TN_size(src) != MTYPE_byte_size(mtype)) {
    const TYPE_ID regtype = Mtype_Of_TN(src);
    if ((regtype == MTYPE_U4 || regtype == MTYPE_I4)
      && MTYPE_byte_size(mtype) < MTYPE_byte_size(regtype))
    {
	// special-case ability to load small value into 32-bit reg
	// if given something like U4U2LDID.
	val_is32 = TRUE;
    }
    else {
	// insert size convert
	TN *tmp =  Build_TN_Of_Mtype (mtype);
	Expand_Convert (tmp, mtype, src, Mtype_Of_TN(src), ops);
	src = tmp;
    }
  }
  Is_True (TN_is_constant(ofst), ("Expand_Store: Illegal offset TN"));
  if (TN_is_symbol(base)) {
	use_reg = FALSE;
	addr_is64 = (MTYPE_RegisterSize(ST_mtype(TN_var(base))) == 8);
	if (TN_has_value(ofst) && TN_value(ofst) == 0 
	  && TN_is_symbol(base) && Is_Simple_Type(ST_type(TN_var(base)))) 
	{
		// st name
		has_offset = FALSE;
	} else {
		// st name[ofst]
		has_offset = TRUE;
	}
  } 
  else {
	// st *(reg+ofst)
	use_reg = TRUE;
        has_offset = TRUE;
	addr_is64 = (TN_register_class(base) == ISA_REGISTER_CLASS_integer64);
  }
  top = Pick_Store_Instruction (mtype, val_is32, addr_is64, use_reg, has_offset);
  FmtAssert(top != TOP_UNDEFINED, ("no topcode"));
  if (has_offset)
  	Build_OP (top, qualifier_tn, space_tn, base, ofst, src, ops);
  else
  	Build_OP (top, qualifier_tn, space_tn, base, src, ops);
}

void
Expand_Lda (TN *dest, TN *src, OPS *ops)
{
  FmtAssert(FALSE, ("NYI"));
}

static void
Expand_Lda (TYPE_ID mtype, TN *dest, TN *base, TN *ofst, VARIANT v, OPS *ops)
{
  // lda doesn't really need to know memory type,
  // as it is a link-time constant.
  // So instead of doing ld r, &sym
  // just do mov r, &sym.
  FmtAssert(mtype == Pointer_Mtype, ("unexpected mtype"));
  Is_True (TN_is_constant(ofst), ("Expand_Load: Illegal offset TN"));

  ST *sym = TN_var(base);
  if (ST_sclass(sym) == SCLASS_PSTATIC) {
    // mangle local names so unique (in case inline multiple local shared vars)
    // but only if name has not already been mangled.
    char buf[80];
    char *p = ST_name(sym);
    sprintf(buf, "%d", (INT) ST_ofst(sym));
    p += strlen(p) - strlen(buf);
    if (strncmp(ST_name(sym), "__cuda_", 7) == 0
      && strcmp(p, buf) == 0) // ofst matches
      ; // reuse existing name
    else
      Set_ST_name(sym, Save_Str2i ("__cuda_", ST_name(sym), (INT) ST_ofst(sym)));
  }
  if (TN_has_value(ofst) && TN_value(ofst) == 0)
  	Build_OP (Is_Target_64bit() ? TOP_mov_u64_a : TOP_mov_u32_a,
		dest, base, ops);
  else 
  	Build_OP (Is_Target_64bit() ? TOP_mov_u64_ao : TOP_mov_u32_ao,
  		dest, base, ofst, ops);

  Set_TN_Memory_Space_From_Variant (dest, v, base);
}

static OPCODE 
OPCODE_make_signed_op(OPERATOR op, TYPE_ID rtype, TYPE_ID desc, BOOL is_signed)
{
  if (MTYPE_is_signed(rtype) != is_signed)
	rtype = MTYPE_complement(rtype);
  if (MTYPE_is_signed(desc) != is_signed)
	desc =	MTYPE_complement(desc);

  return OPCODE_make_op(op, rtype, desc);
}

static void
Expand_Composed_Load ( OPCODE op, TN *result, TN *base, TN *disp, VARIANT variant, OPS *ops)
{
  return Expand_Load( op, result, base, disp, variant, ops );
}

void
Expand_Misaligned_Load ( OPCODE op, TN *result, TN *base, TN *disp, VARIANT variant, OPS *ops)
{
  ErrMsgSrcpos(EC_Unaligned_Memory, current_srcpos);
}


static void
Expand_Composed_Store (TYPE_ID mtype, TN *obj, TN *base, TN *disp, VARIANT variant, OPS *ops)
{
  return Expand_Store( mtype, obj, base, disp, variant, ops );
}

void
Expand_Misaligned_Store (TYPE_ID mtype, TN *obj_tn, TN *base_tn, TN *disp_tn, VARIANT variant, OPS *ops)
{
  ErrMsgSrcpos(EC_Unaligned_Memory, current_srcpos);
}

// recognize some special symbols and create specific tns for them
static TN*
Get_TN_For_Predefined_Symbol (ST *sym, INT64 ofst)
{
  INT index = 0;
  if (ofst == 0) 
	index += 0;
  else if (ofst == 4) 
	index += 1;
  else if (ofst == 8) 
	index += 2;
  else 
	FmtAssert(FALSE, ("unexpected offset %lld with symbol %s", 
			ofst, ST_name(sym) ));

  if (strcmp(ST_name(sym), "threadIdx") == 0) {	
	return Tid_TN(index);	
  }
  else if (strcmp(ST_name(sym), "blockDim") == 0) {	
	return Ntid_TN(index);	
  }
  else if (strcmp(ST_name(sym), "blockIdx") == 0) {	
	return Ctaid_TN(index);	
  }
  else if (strcmp(ST_name(sym), "gridDim") == 0) {	
	return Nctaid_TN(index);	
  }
  else if (strcmp(ST_name(sym), "warpSize") == 0) {	
	// this is evil hack:  change name of st to ptx name
	Set_ST_name(sym, Save_Str("WARP_SZ"));
	return Gen_Symbol_TN(sym, 0, 0);	
  }
  else if (strcmp(ST_name(sym), "WARP_SZ") == 0) {	
	return Gen_Symbol_TN(sym, 0, 0);	
  }
  else {
	return NULL; // not a recognized symbol
  }
}

static void
Exp_Ldst (
  OPCODE opcode,
  TN *tn,
  ST *sym,
  INT64 ofst,
  BOOL indirect_call,
  BOOL is_store,
  BOOL is_load,
  OPS *ops,
  VARIANT variant)
{
  ST* base_sym = NULL;
  INT64 base_ofst = 0;
  TN* base_tn = NULL;
  TN* ofst_tn = NULL;
  TN* tmp_tn = NULL;
  const BOOL is_lda = (!is_load && !is_store);
  OPS newops = OPS_EMPTY;
  OP* op = NULL;

  if (Trace_Exp2) {
    fprintf(TFile, "exp_ldst %s: ", OPCODE_name(opcode));
    if (tn) Print_TN(tn,FALSE);
    if (is_store) fprintf(TFile, " -> ");
    else fprintf(TFile, " <- ");
    if (ST_class(sym) != CLASS_CONST)
      fprintf(TFile, "%lld (%s)\n", ofst, ST_name(sym));
    else
      fprintf(TFile, "%lld ()\n", ofst);
  }
  
  if (TY_is_volatile(ST_type(sym)) && ! V_volatile(variant)) 
    DevWarn("not marked volatile?");

  Allocate_Object(sym);         /* make sure sym is allocated */
  Set_BE_ST_referenced(sym);
  
  Base_Symbol_And_Offset_For_Addressing (sym, ofst, &base_sym, &base_ofst);

  if (base_sym == SP_Sym || base_sym == FP_Sym) {

    base_tn = (base_sym == SP_Sym) ? SP_TN : FP_TN;
    if (sym == base_sym) {
      // can have direct reference to SP or FP,
      // e.g. if actual stored to stack.
	FmtAssert( false, ("stack pointer nyi: probably an unexpected call or intrinsic") );

    } else {
      /* Because we'd like to see symbol name in .s file, 
       * still reference the symbol rather than the sp/fp base.  
       * Do put in the offset from the symbol.  
       * We put the symbol in the TN and then
       * let cgemit replace symbol with the final offset.
       * We generate a SW reg, <sym>, <SP> rather than SW reg,<sym>
       * because cgemit and others expect a separate tn for the
       * offset and base. 
       */
	// Most locals will become pregs, but sometimes WOPT leaves some,
	// and hw doesn't have real stack.
	// So either put in .local area or in preg.
	if (CG_opt_level > 0 // for -O0 use locals
	  && !ST_addr_saved(sym) 
          // Casts to a larger size will have addr_saved set in adjust_addr.
	  // && !ST_addr_passed(sym)
	  // if we allow calls may need to check for addr_passed,
	  // but right now the adjust_addr routine doesn't recompute 
	  // addr_passed, only addr_saved.
	  && !TY_has_union(ST_type(sym)) ) // can't pick register type if union
	{
		// safe to put in register.
		// have to create mapping from st_index,offset to tn.
		std::map<pair<UINT,INT64>,TN*>::iterator it;
		// use sym mtype; may convert to smaller use mtype.
		TYPE_ID sym_mtype = Mtype_For_Type_Offset(ST_type(sym),ofst);
		// Mtype_Of_TN always returns unsigned,
		// but want to preserve sign-ness of operation.
		TYPE_ID tn_mtype = Mtype_TransferSign(sym_mtype, Mtype_Of_TN(tn));
		TN *symtn;
		FmtAssert(!is_lda, ("lda but not addr used?"));
		it = st_to_tn_map.find(
			pair<UINT,INT64>(ST_index(sym), ofst) );
		if (it == st_to_tn_map.end()) {
			// create new tn
			symtn = Build_TN_Of_Mtype(sym_mtype);
			st_to_tn_map.insert( 
			  pair<pair<UINT,INT64>,TN*>(
			    pair<UINT,INT64>(ST_index(sym), ofst), 
			    symtn) );
			DevWarn("map local %d,%d to tn%d", ST_index(sym), (INT)ofst, TN_number(symtn));
		}
		else {
			symtn = it->second;
		}
		// Do copy of tn<->symtn, but check for size mismatches, 
		// e.g. U4U2LDID of 32bit tn, or U1STID, or U4U4LDID of 64bit tn
		if (is_store) {
		    // copy tn -> symtn
                    // check for case of needing convert for different sizes,
                    // because Exp_COPY lacks proper mtype info.
                    if (TN_size(symtn) != TN_size(tn)) {
                      // sizes don't match, so do convert rather than simple copy
                      Expand_Convert (symtn, sym_mtype, tn, tn_mtype, &newops);
                    } else {
                      Exp_COPY (symtn, tn, &newops);
                    }
		    is_store = FALSE;	// so don't do it again
		}
		else if (is_load) {
		    // copy symtn -> tn
                    // check for case of needing convert for different sizes,
                    // because Exp_COPY lacks proper mtype info.
                    if (TN_size(symtn) != TN_size(tn)) {
                      // sizes don't match, so do convert rather than simple copy
                      Expand_Convert (tn, tn_mtype, symtn, sym_mtype, &newops);
                    } else {
                      Exp_COPY (tn, symtn, &newops);
                    }
		    is_load = FALSE;	// so don't do it again
		}
	} 
	else if (CGEXP_auto_as_static) {
		// put in .local area (like a static).
		// Note that this doesn't work for recursion,
		// but we don't support that yet.
		// Inlining could cause multiple stack variables of
		// same name, with different stack offsets, but we need
		// unique names for pstatic since accessed by name.
		// So suffix names with offset.
		DevWarn("convert stack variable %s to static local", ST_name(sym));
		Set_ST_sclass(sym, SCLASS_PSTATIC);
		if (ST_is_value_parm(sym)) Clear_ST_is_value_parm(sym);
		Set_ST_in_local_mem(sym);
		Set_V_local_mem(variant);
		Set_ST_name(sym, Save_Str2i ("__cuda_", ST_name(sym), (INT) ST_ofst(sym)));
		Set_ST_base(sym, sym);
		Set_ST_ofst(sym, 0);
		// reallocate object so know to emit it
		// (note that this leaves stack allocated,
		// but that's okay since we ignore stack).
  		Allocate_Object(sym); 

		base_tn = Gen_Symbol_TN (sym, 0, 0);
		ofst_tn = Gen_Literal_TN (ofst, 4);
	} else {
		FmtAssert(FALSE, ("stack variables not supported"));
	}
    }

  } else if (ST_sclass(sym) == SCLASS_COMMON 
	  || ST_sclass(sym) == SCLASS_FSTATIC
	  || ST_sclass(sym) == SCLASS_PSTATIC
	  || ST_sclass(sym) == SCLASS_UGLOBAL
	  || ST_sclass(sym) == SCLASS_DGLOBAL) 
  {
	ofst_tn = Gen_Literal_TN (ofst, 4);
	base_tn = Gen_Symbol_TN (sym, 0, 0);
	if (V_memory_space(variant)) {
		// use existing variant info (global ptr)
	} else if (ST_in_global_mem(sym)) {
		Set_V_global_mem(variant);
	} else if (ST_in_shared_mem(sym)) {
		Set_V_shared_mem(variant);
	} else if (ST_in_local_mem(sym)) {
		Set_V_local_mem(variant);
	} else if (ST_in_constant_mem(sym)) {
		Set_V_const_mem(variant);
	} else if (ST_in_texture_mem(sym)) {
		if (tn != NULL) {
		    if (Trace_Exp2) fprintf(TFile,"replace tn home with texture ldid\n");
		    // don't emit the load, just set the home of the result
		    // which will later be used by the texture asm.
		    Set_TN_home (tn, 
			WN_CreateLdid (opcode,ofst,sym,ST_type(sym)));
		    Set_TN_in_texture_mem(tn);
		    return;
		}
		else 
		    FmtAssert(FALSE, ("texture variable NYI"));
	}
	else if (ST_is_initialized(sym)
	  &&  (ST_sclass(sym) == SCLASS_PSTATIC) 
	   || (ST_sclass(sym) == SCLASS_FSTATIC))
	{
		// assume is initialized local;
		// put initialization into const memory
		Set_ST_in_constant_mem(sym);
		Set_V_const_mem(variant);
	}
  	else FmtAssert(FALSE, ("variable not in memory space"));

  } else if (ST_sclass(sym) == SCLASS_FORMAL) {
	// formals in global entry must be put in param space
	ofst_tn = Gen_Literal_TN (ofst, 4);
	base_tn = Gen_Symbol_TN (sym, 0, 0);
	Set_V_param_mem(variant);
  } else if (ST_sclass(sym) == SCLASS_EXTERN) {
	base_tn = Get_TN_For_Predefined_Symbol(sym, ofst);
	FmtAssert (base_tn != NULL, ("unrecognized extern symbol: %s", ST_name(sym)));
	if (is_load) {
	    if (TN_is_symbol(base_tn)) {
		// must be constant symbol like WARP_SZ
		Expand_Mtype_Immediate (tn, base_tn, OPCODE_rtype(opcode), &newops);
	    } else {
		// TID is really a register, 
		// so turn load of tid into a move/convert.
		// CUDA says these are 32bits, but NVISA says they are 16 bits,
		// so generate a convert
		Expand_Convert (tn, OPCODE_rtype(opcode), 
				base_tn, MTYPE_U2, &newops);
	    }
	    is_load = FALSE;	// so nothing else done
	}
	else
		FmtAssert(FALSE, ("NYI"));

  } else {
	FmtAssert(FALSE, ("NYI"));
  }

  if (is_store) {
        if (V_align_all(variant) == 0)
                Expand_Store (OPCODE_desc(opcode), tn, base_tn, ofst_tn,
                        variant, &newops);
        else
                Expand_Misaligned_Store (OPCODE_desc(opcode), tn,
                        base_tn, ofst_tn, variant, &newops);
  }
  else if (is_load) {
        if (V_align_all(variant) == 0)
                Expand_Load (opcode, tn, base_tn, ofst_tn, variant, &newops);
        else
                Expand_Misaligned_Load (opcode, tn,
                        base_tn, ofst_tn, variant, &newops);
  }
  else if (is_lda) {
        Expand_Lda (OPCODE_rtype(opcode), tn, base_tn, ofst_tn, variant, &newops);
        // Expand_Add (tn, ofst_tn, base_tn, OPCODE_rtype(opcode), &newops);
  }

  FOR_ALL_OPS_OPs (&newops, op) {
    if (is_load && ST_is_constant(sym) && OP_load(op)) {
      // If we expanded a load of a constant, 
      // nothing else can alias with the loads 
      // we have generated.
      Set_OP_no_alias(op);
    }
    if (Trace_Exp2) {
      fprintf(TFile, "exp_ldst into "); Print_OP (op);
    }
  }
  /* Add the new OPs to the end of the list passed in */
  OPS_Append_Ops(ops, &newops);
}

void Exp_Lda ( 
  TYPE_ID mtype, 
  TN *tgt_tn, 
  ST *sym, 
  INT64 ofst, 
  OPERATOR call_opr,
  OPS *ops)
{
  OPCODE opcode = OPCODE_make_op(OPR_LDA, mtype, MTYPE_V);
  Exp_Ldst (opcode, tgt_tn, sym, ofst, 
	(call_opr == OPR_ICALL),
	FALSE, FALSE, ops, V_NONE);
}

void
Exp_Load (
  TYPE_ID rtype, 
  TYPE_ID desc, 
  TN *tgt_tn, 
  ST *sym, 
  INT64 ofst, 
  OPS *ops, 
  VARIANT variant)
{
  OPCODE opcode = OPCODE_make_op (OPR_LDID, rtype, desc);
  Exp_Ldst (opcode, tgt_tn, sym, ofst, FALSE, FALSE, TRUE, ops, variant);
}

void
Exp_Store (
  TYPE_ID mtype, 
  TN *src_tn, 
  ST *sym, 
  INT64 ofst, 
  OPS *ops, 
  VARIANT variant)
{
  OPCODE opcode = OPCODE_make_op(OPR_STID, MTYPE_V, mtype);
  Exp_Ldst (opcode, src_tn, sym, ofst, FALSE, TRUE, FALSE, ops, variant);
}

static ISA_ENUM_CLASS_VALUE
Pick_Prefetch_Hint (VARIANT variant)
{
  UINT32 pf_flags = V_pf_flags(variant);
  FmtAssert(FALSE, ("NYI"));
}

void Exp_Prefetch (TOP opc, TN* src1, TN* src2, VARIANT variant, OPS* ops)
{
  FmtAssert(opc == TOP_UNDEFINED,
            ("Prefetch opcode should be selected in Exp_Prefetch"));
  const UINT32 pf_flags = V_pf_flags(variant);
  const ISA_ENUM_CLASS_VALUE pfhint = Pick_Prefetch_Hint(variant);
  TOP top;

  FmtAssert(FALSE, ("NYI"));
}

/* ======================================================================
 * Exp_Extract_Bits
 * ======================================================================*/
void Exp_Extract_Bits (TYPE_ID rtype, TYPE_ID desc, UINT bit_offset, UINT bit_size,
		       TN *tgt_tn, TN *src_tn, OPS *ops)
{
    INT64 src_val;
    BOOL is_double = MTYPE_is_size_double (rtype);

    /* The constant supports only matching host and target endianness and 32bit ints 
     * 
     * Ideally the whirl simplifier should catch these constant cases,
     * but it doesn't, so special-case it here.
     */  
    if (TN_Can_Use_Constant_Value (src_tn, desc, &src_val)
     && (!is_double))
    {
        if (MTYPE_is_signed (rtype))
        {
            /* ISO/ANSI C doesn't mandate sign extension must happen with a shift right.
             * All compilers do this but be warned it's not mandatory.
             */
            INT32 val = ((INT32) src_val) << (MTYPE_bit_size (rtype) - bit_offset - bit_size);
            val = val >> (MTYPE_bit_size (rtype) - bit_size);
            
            Expand_Mtype_Immediate (tgt_tn, Gen_Literal_TN(val,4), rtype, ops);
        }
        else
        {
            UINT32 bit_mask = ((-1U) >> (MTYPE_bit_size(desc) - bit_size)) << bit_offset;
            UINT32 val = src_val & bit_mask;
            
            val = val >> bit_offset;
            Expand_Mtype_Immediate (tgt_tn, Gen_Literal_TN(val,4), rtype, ops);
        }
    }
    else
    {
        /* Extract via Shift Left + (signed) ? Arithmetic Shift Right : Shift Right */
        TN *tmp1_tn = Build_TN_Like (tgt_tn);
        
        INT32 left_shift_amt = MTYPE_bit_size (rtype) - bit_offset - bit_size;
        Expand_Shift ( tmp1_tn, src_tn, Gen_Literal_TN(left_shift_amt, 4),
                      rtype, shift_left, ops );

        INT32 right_shift_amt = MTYPE_bit_size(rtype) - bit_size;
        Expand_Shift ( tgt_tn, tmp1_tn, Gen_Literal_TN(right_shift_amt, 4),
                      rtype, MTYPE_is_signed(rtype) ? shift_aright : shift_lright, ops );
    }
}

/* ======================================================================
 * Exp_Deposit_Bits - deposit src2_tn into a field of src1_tn returning
 * the result in tgt_tn.
 * ======================================================================*/
void Exp_Deposit_Bits (TYPE_ID rtype, TYPE_ID desc, UINT bit_offset, UINT bit_size,
		       TN *tgt_tn, TN *src1_tn, TN *src2_tn, OPS *ops)
{
  // since nvisa can handle large masks, do and-mask rather than shifts
  FmtAssert( bit_size != 0, ("size of bit field cannot be 0"));
  FmtAssert( MTYPE_bit_size(desc) == 32, ("NYI: deposit_bits for non-word size"));

  TN *tmp1_tn = Build_TN_Like(tgt_tn);
  TN *tmp2_tn = Build_TN_Like(tgt_tn);
  UINT src2_mask = (1 << bit_size) - 1;
  UINT src1_mask = ~(src2_mask << bit_offset);
  INT64 src1_val;
  INT64 src2_val;
  UINT val;
  if (TN_Can_Use_Constant_Value (src1_tn, desc, &src1_val)) {
	val = src1_val;
	val = val & src1_mask;
	tmp1_tn = Gen_Literal_TN (val, 4);
  }
  else {
	Expand_Binary_And( tmp1_tn, src1_tn, Gen_Literal_TN(src1_mask,4), 
		rtype, ops);
  }
  if (TN_Can_Use_Constant_Value (src2_tn, desc, &src2_val)) {
	val = src2_val;
	val = val & src2_mask;
	val = val << bit_offset;
	tmp2_tn = Gen_Literal_TN (val, 4);
  }
  else {
  	Expand_Binary_And( tmp2_tn, src2_tn, Gen_Literal_TN(src2_mask,4), 
		rtype, ops);
  	Expand_Shift( tmp2_tn, tmp2_tn, Gen_Literal_TN(bit_offset, 4),
               rtype, shift_left, ops);
  }
  if (TN_is_constant(tmp1_tn) && TN_has_value(tmp1_tn)
   && TN_is_constant(tmp2_tn) && TN_has_value(tmp2_tn))
  {
	// both operands are constant
	src1_val = TN_value(tmp1_tn);
	src2_val = TN_value(tmp2_tn);
	src1_val = src1_val | src2_val;
	val = src1_val;
	Expand_Mtype_Immediate (tgt_tn, Gen_Literal_TN(val,4), rtype, ops);
  }
  else {
	Expand_Binary_Or( tgt_tn, tmp1_tn, tmp2_tn, rtype, ops);
  }
}

void 
Expand_Lda_Label (TN *dest, TN *lab, OPS *ops)
{
  Expand_Mtype_Immediate(dest, lab, Mtype_Of_TN(dest), ops);
}
