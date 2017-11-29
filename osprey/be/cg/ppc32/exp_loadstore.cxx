/*
 * Copyright (C) 2006-2009 Tsinghua University.  All Rights Reserved.
 */

/*
 * Copyright 2002, 2003, 2004 PathScale, Inc.  All Rights Reserved.
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


/* CGEXP routines for loads and stores */
#include "elf_stuff.h"
#include <vector>
#include "defs.h"
#include "em_elf.h"
#include "erglob.h"
#include "ercg.h"
#include "tracing.h"
#include "config.h"
#include "config_targ_opt.h"
#include "config_debug.h"
#include "xstats.h"
#include "topcode.h"
#include "tn.h"
#include "cg_flags.h"
#include "targ_isa_lits.h"
#include "op.h"
#include "stblock.h"
#include "data_layout.h"
#include "strtab.h"
#include "symtab.h"
#include "cg.h"
#include "cgexp.h"
#include "cgexp_internals.h"
#include "cgemit.h" // for CG_emit_non_gas_syntax

#define OFFSET_HI(offset) ((((offset) + 0x8000) >> 16) << 16)
#define OFFSET_LO(offset) ((((offset) + 0x8000) & 0xffff) - 0x8000)

void
Expand_Lda (TN *dest, TN *src, OPS *ops)
{
  FmtAssert(FALSE, ("NYI: Expand_Lda"));
}

#define MTYPE_is_longlong(t) (MTYPE_is_integral(t) && MTYPE_is_size_double(t))

static TOP
Pick_Load_Instruction (TYPE_ID rtype, TYPE_ID desc, BOOL regx)
{
  switch (desc) {
  case MTYPE_I1: 
  case MTYPE_U1: return regx ? TOP_lbzx : TOP_lbz;
  case MTYPE_I2: return regx ? TOP_lhax : TOP_lha;
  case MTYPE_U2: return regx ? TOP_lhzx : TOP_lhz;
  case MTYPE_I4: 
  case MTYPE_U4: return regx ? TOP_lwzx : TOP_lwz;
  case MTYPE_I8: 
  case MTYPE_U8: return regx ? TOP_lwzx : TOP_lwz;
  case MTYPE_F4: return regx ? TOP_lfsx : TOP_lfs;
  case MTYPE_F8: return regx ? TOP_lfdx : TOP_lfd;
    FmtAssert(FALSE, ("NYI: Pick_Load_Instruction mtype"));
  case MTYPE_V:
    if (rtype != MTYPE_V) // use rtype to pick load (e.g. if lda)
      return Pick_Load_Instruction(rtype,rtype, regx);
    // else fallthru
  default:  
    FmtAssert(FALSE, ("NYI: Pick_Load_Instruction mtype"));
    return TOP_UNDEFINED;
  }
}

static void
Handle_Misc_MType(TYPE_ID rtype, TYPE_ID mtype, TN * result, OPS * ops)
{
  if ((rtype == MTYPE_I4) && (mtype == MTYPE_I1)) {
    Build_OP(TOP_extsb, result, result, ops);
  } else if ((rtype == MTYPE_I4) && (mtype == MTYPE_I2)) {
    Build_OP(TOP_extsh, result, result, ops);
  }
  
}

static BOOL
Need_Extends_Signbit(TYPE_ID rtype, TYPE_ID desc)
{
	return rtype == MTYPE_I4 && desc == MTYPE_I1;
}

static void
Extends_SignBits(TYPE_ID rtype, TYPE_ID desc, TN* result, TN* src, OPS* ops)
{
	if ((rtype == MTYPE_I4) && (desc == MTYPE_I1)) {
		Build_OP(TOP_extsb, result, src, ops);
	}
}

static void 
Expand_64Bit_Load(OPCODE opcode,
    TN* result, TN* base, TN* ofst, OPS* ops)
{
  const TYPE_ID desc      = OPCODE_desc(opcode);
  const TYPE_ID rtype     = OPCODE_rtype(opcode);
  const TYPE_ID new_mtype = (rtype == MTYPE_I8 ? MTYPE_I4 : MTYPE_U4);
  const TYPE_ID new_desc  = MTYPE_is_size_double(desc) ? new_mtype : desc;
  const TOP new_top       = Pick_Load_Instruction(new_mtype, new_desc, TN_is_register(ofst));

  FmtAssert(MTYPE_is_size_double(rtype), ("NYI"));
  
  TN * result_h = Get_TN_Pair(result);
  FmtAssert(result_h, ("Expand_64Bit_Load : result tn pair not setup"));  
  
  if (MTYPE_is_size_double(desc)){
    TN* ofst_h = NULL;
    if (TN_is_register(ofst)) {
      ofst_h = Build_TN_Of_Mtype(Pointer_Mtype);
      Expand_Add(ofst_h, ofst, Gen_Literal_TN(4, Pointer_Size), MTYPE_I4, ops);
    }
    else {
      ofst_h = TN_is_symbol(ofst)
        ? Gen_Symbol_TN(TN_var(ofst), TN_offset(ofst) + 4, TN_relocs(ofst))
        : Gen_Literal_TN(TN_value(ofst) + 4, TN_size(ofst));
    }
    Build_OP(new_top, result,   base, ofst_h, ops);    
    Build_OP(new_top, result_h, base, ofst,   ops);        
  } else {  // !MTYPE_is_size_double(desc)
    if (Need_Extends_Signbit(new_mtype, new_desc)) {
      TN* tn = Build_TN_Like(result);
      Build_OP(new_top, tn, base, ofst, ops);
      if (rtype == MTYPE_I8) {
	  	if (TN_is_dedicated(result)) {
	  		TN* tn1 = Build_TN_Like(result);
			Extends_SignBits(new_mtype, new_desc, tn1, tn, ops);
			Expand_Copy(result, tn1, MTYPE_I4, ops);
			Build_OP(TOP_srawi, result_h, tn1, Gen_Literal_TN(31, 4), ops);
	  	} else {
	  		Extends_SignBits(new_mtype, new_desc, result, tn, ops);
			Build_OP(TOP_srawi, result_h, result, Gen_Literal_TN(31, 4), ops);
	  	}
      } else {
		Extends_SignBits(new_mtype, new_desc, result, tn, ops);
		Build_OP(TOP_li, result_h, Gen_Literal_TN(0, 4), ops);
      }
    } else {
      if (rtype == MTYPE_I8) {
	  	if (TN_is_dedicated(result)) {
			TN* tn = Build_TN_Like(result);
			Build_OP(new_top, tn, base, ofst, ops);
			Expand_Copy(result, tn, MTYPE_I4, ops);
			Build_OP(TOP_srawi, result_h, tn, Gen_Literal_TN(31, 4), ops);
	  	} else {
	  		Build_OP(new_top, result, base, ofst, ops);
			Build_OP(TOP_srawi, result_h, result, Gen_Literal_TN(31, 4), ops);
	  	}
      } else {
		Build_OP(new_top, result, base, ofst, ops);
		Build_OP(TOP_li, result_h, Gen_Literal_TN(0, 4), ops);
      }
      
    }
  }
}


void
Expand_Load (OPCODE opcode, TN *result, TN *base, TN *ofst, OPS *ops)
{
  if (OP_NEED_PAIR(OPCODE_rtype(opcode))){
    Expand_64Bit_Load(opcode, result, base, ofst, ops);
    return;
  }

  TYPE_ID  mtype = OPCODE_desc(opcode);
  TYPE_ID  rtype = OPCODE_rtype(opcode);
  BOOL notFixImm = (TN_has_value(ofst) && !ISA_LC_Value_In_Class(TN_value(ofst), LC_simm16));
  BOOL    bNeedX = TN_is_register(ofst) || notFixImm;
  TOP        top = Pick_Load_Instruction (rtype, mtype, bNeedX);

  // Handle very large offsets (that consume greater than 16 bits).
  // Try and see if the base and offset can be merged.
  // An example is gcc.c-torture/compile/990617-1.c
  // what does the above comment mean ? 
  // I doubt that having nothing connecting to the blow code.
  if (TN_is_rematerializable(base)) {   // like a = *((char *)8000)
    WN *home = TN_home(base);
    if (WN_operator(home) == OPR_INTCONST) {
      INT64 val = WN_const_val(home);
      TN *tmp_base = Gen_Literal_TN (val + TN_value(ofst), TN_size(base));
      tmp_base = Expand_Immediate_Into_Register(NULL, tmp_base, MTYPE_U4, ops);
      Build_OP(top, result, tmp_base, Gen_Literal_TN (0, Pointer_Size), ops);
      return;
    }
  }

  TN* ofst_tn;
  if (notFixImm) {
    ofst_tn = Expand_Immediate_Into_Register(NULL, ofst, MTYPE_U4, ops);
  }
  else {
    ofst_tn = ofst;
  }
  if (Need_Extends_Signbit(rtype, mtype)) {
    TN* tn = Build_TN_Like(result);
    Build_OP(top, tn, base, ofst_tn, ops);
    Extends_SignBits(rtype, mtype, result, tn, ops);
  } else {
    Build_OP(top, result, base, ofst_tn, ops);
  }
}

static TOP
Pick_Store_Instruction (TYPE_ID mtype, BOOL regx)
{
  switch (mtype) {
      case MTYPE_I1:    case MTYPE_U1:  return regx ? TOP_stbx : TOP_stb;
      case MTYPE_I2:    case MTYPE_U2:  return regx ? TOP_sthx : TOP_sth;
      case MTYPE_I4:    case MTYPE_U4:  return regx ? TOP_stwx : TOP_stw;
      case MTYPE_I8:    case MTYPE_U8:  return regx ? TOP_stwx : TOP_stw;
      case MTYPE_F4:    return regx ? TOP_stfsx : TOP_stfs;
      case MTYPE_F8:    return regx ? TOP_stfdx : TOP_stfd;
  default:  FmtAssert(FALSE, ("NYI: Pick_Store_Instruction mtype"));
    return TOP_UNDEFINED;
  }
}

static void Expand_64Bit_Store(TYPE_ID mtype,
                TN *src, TN *base, TN *ofst, OPS *ops)
{
  TYPE_ID new_mtype = (mtype == MTYPE_I8 ? MTYPE_I4 : MTYPE_U4);
  TOP new_top = Pick_Store_Instruction(new_mtype, TN_is_register(ofst));
  TN* src_h   = Get_TN_Pair(src);

  if (src_h == NULL){        
    src_h   = Build_TN_Of_Mtype(MTYPE_I4);
    Build_OP(TOP_srawi, src_h, src, Gen_Literal_TN(31, 4), ops);
  }
  
  TN * ofst_h  = NULL;

  if (TN_is_register(ofst)) {
    ofst_h = Build_TN_Of_Mtype(Pointer_Mtype);
    Expand_Add(ofst_h, ofst, Gen_Literal_TN(4, Pointer_Size), MTYPE_I4, ops);
  }
  else {
    ofst_h = TN_is_symbol(ofst) ? 
      Gen_Symbol_TN(TN_var(ofst),      TN_offset(ofst)+4, TN_relocs(ofst)) : 
      Gen_Literal_TN(TN_value(ofst)+4, TN_size(ofst));
  }
  // for BIG_ENDIAN, store high bits at low address
  Build_OP(new_top, src, base, ofst_h, ops);
  Build_OP(new_top, src_h,   base, ofst,   ops);
}

void
Expand_Store (TYPE_ID mtype, TN *src, TN *base, TN *ofst, OPS *ops)
{
  if (OP_NEED_PAIR(mtype)) {
    Expand_64Bit_Store(mtype, src, base, ofst, ops);
    return;
  }

  BOOL notFixImm = (TN_has_value(ofst) && !ISA_LC_Value_In_Class(TN_value(ofst), LC_simm16));
  BOOL    bNeedX = TN_is_register(ofst) || notFixImm;
  TOP        top = Pick_Store_Instruction(mtype, bNeedX);

  FmtAssert(src, ("NYI"));
  if (notFixImm) {
    TN * tmp_tn = Expand_Immediate_Into_Register(NULL, ofst, MTYPE_U4, ops);
    Build_OP(top, src, base, tmp_tn, ops);
  }
  else {
    Build_OP(top, src, base, ofst, ops);
  }
}

static OPCODE 
OPCODE_make_signed_op(OPERATOR op, TYPE_ID rtype, TYPE_ID desc, BOOL is_signed)
{
  if (MTYPE_is_signed(rtype) != is_signed)
    rtype = MTYPE_complement(rtype);
  if (MTYPE_is_signed(desc) != is_signed)
    desc =  MTYPE_complement(desc);

  return OPCODE_make_op(op, rtype, desc);
}

/* ====================================================================
 *
 * Adjust_Addr_TNs
 *
 * We have a memory reference operation, with a base and displacement,
 * where the displacement is literal.  We want to create another memop
 * with the displacement modified by a small amount.
 *
 * WARNING:  If an add operation is required, it will be expanded here.
 *
 * ====================================================================
 */

static void
Adjust_Addr_TNs (
  TOP   opcode,     /* The new memory operation */
  TN    **base_tn,  /* The base address -- may be modified */
  TN    **disp_tn,  /* The displacement -- may be modified */
  INT16 disp,       /* A displacement to add */
  OPS *ops)
{

  if (Potential_Immediate_TN_Expr (opcode, *disp_tn, disp))
  {
    if (TN_has_value(*disp_tn)) {
      *disp_tn = Gen_Literal_TN (TN_value(*disp_tn) + disp, 4);
    } else {
      *disp_tn = Gen_Symbol_TN (TN_var(*disp_tn),
                 TN_offset(*disp_tn) + disp, 0);
    }
  } else {
    TN *tmp = Build_TN_Of_Mtype (Pointer_Mtype);
    // because disp may be symbolic reloc on base,
    // want to still add it with base and create new base and disp.
    Expand_Add (tmp, *disp_tn, *base_tn, Pointer_Mtype, ops);
    *base_tn = tmp;
    *disp_tn = Gen_Literal_TN (disp, 4);
  }
}

static void
Expand_Composed_Load (OPCODE op, TN *result, TN *base, TN *disp, VARIANT variant, OPS *ops)
{
  Expand_Load(op, result, base, disp, ops);
  return;
  
  DevWarn("Expand_Composed_Load Not verified");  
  TYPE_ID rtype= OPCODE_rtype(op);
  TYPE_ID desc = OPCODE_desc(op);
  
  if (MTYPE_is_float(rtype)) {      
    DevWarn("Expand_Composed_Load: encount float");
    Expand_Load(op, result, base, disp, ops);
  }
  else {
    TN * load1 = Build_TN_Of_Mtype(MTYPE_I4);
    TN * load2 = Build_TN_Of_Mtype(MTYPE_I4);
    TN * load3 = Build_TN_Of_Mtype(MTYPE_I4); 
    TN * newbase = Build_TN_Of_Mtype(MTYPE_I4); 
    TN * res_h = NULL;
    switch (desc) {
      case MTYPE_I1:
      case MTYPE_U1:
        if (TN_is_register(disp)) 
          Build_OP(TOP_lbzx, result, base, disp, ops); 
        else 
          Build_OP(TOP_lbz, result, base, disp, ops);    
        break;
      
      case MTYPE_I8:
      case MTYPE_U8:
        if (V_align_offset_known(variant) && V_align_offset(variant) >= 8) {
          DevWarn("WHY here! Expand_Composed_Load(I2/U2) V_align_offset > 1");
          Expand_Load(op, result, base, disp, ops);
          return;
        }
        
        if (TN_is_register(disp)) {            
          Expand_Add(newbase, base, disp, MTYPE_I4, ops);
          base = newbase;
          disp = Gen_Literal_TN(0, 4);
        }

           
        Build_OP(TOP_lbz, result, base, disp, ops);
        Build_OP(TOP_lbz, load1,  base, Gen_Adjusted_TN(disp, 1), ops);
        Build_OP(TOP_lbz, load2,  base, Gen_Adjusted_TN(disp, 2), ops);
        Build_OP(TOP_lbz, load3, base, Gen_Adjusted_TN(disp, 3), ops);
                  
        // result = result & 0xff
        Build_OP(TOP_rlwinm, result, result, Gen_Literal_TN(24, 4), 
            Gen_Literal_TN(0, 4), Gen_Literal_TN(7, 4), ops);

        // insert load0[7..0] into result[16..23], etc
        Build_OP(TOP_rlwimi, result, load1, Gen_Literal_TN(16, 0), 
          Gen_Literal_TN(8, 4), Gen_Literal_TN(15, 4), result, ops);
        Build_OP(TOP_rlwimi, result, load2, Gen_Literal_TN(8, 0), 
          Gen_Literal_TN(16, 4),  Gen_Literal_TN(23, 4), result, ops);
        Build_OP(TOP_rlwimi, result, load3, Gen_Literal_TN(0, 0), 
          Gen_Literal_TN(24, 4),  Gen_Literal_TN(31, 4), result,  ops);

        res_h = Get_TN_Pair(result);
        FmtAssert(res_h, ("Expand_Composed_Load : TN pair not setup"));
           
        Build_OP(TOP_lbz, result, base, Gen_Adjusted_TN(disp, 4), ops);
        Build_OP(TOP_lbz, load1,  base, Gen_Adjusted_TN(disp, 5), ops);
        Build_OP(TOP_lbz, load2,  base, Gen_Adjusted_TN(disp, 6), ops);
        Build_OP(TOP_lbz, load3,  base, Gen_Adjusted_TN(disp, 7), ops);
                  
        // result = result & 0xff
        Build_OP(TOP_rlwinm, result, result, Gen_Literal_TN(24, 4), 
            Gen_Literal_TN(0, 4), Gen_Literal_TN(7, 4), ops);

        // insert load0[7..0] into result[16..23], etc
        Build_OP(TOP_rlwimi, result, load1, Gen_Literal_TN(16, 0), 
          Gen_Literal_TN(8, 4), Gen_Literal_TN(15, 4), result, ops);
        Build_OP(TOP_rlwimi, result, load2, Gen_Literal_TN(8, 0), 
          Gen_Literal_TN(16, 4),  Gen_Literal_TN(23, 4), result, ops);
        Build_OP(TOP_rlwimi, result, load3, Gen_Literal_TN(0, 0), 
          Gen_Literal_TN(24, 4),  Gen_Literal_TN(31, 4), result, ops);
        
        return;
      case MTYPE_I4:
      case MTYPE_U4:
        if (V_align_offset_known(variant) && V_align_offset(variant) >= 4) {
          DevWarn("WHY here! Expand_Composed_Load(I2/U2) V_align_offset > 1");
          Expand_Load(op, result, base, disp, ops);
          return;
        }
        if (TN_is_register(disp)) {            
          Expand_Add(newbase, base, disp, MTYPE_I4, ops);
          base = newbase;
          disp = Gen_Literal_TN(0, 4);
        }
        
        Build_OP(TOP_lbz, result, base, disp, ops);
        Build_OP(TOP_lbz, load1,  base, Gen_Adjusted_TN(disp, 1), ops);
        Build_OP(TOP_lbz, load2,  base, Gen_Adjusted_TN(disp, 2), ops);
        Build_OP(TOP_lbz, load3, base, Gen_Adjusted_TN(disp, 3), ops);
                  
        // result = result & 0xff
        Build_OP(TOP_rlwinm, result, result, Gen_Literal_TN(24, 4), 
            Gen_Literal_TN(0, 4), Gen_Literal_TN(7, 4), ops);

        // insert load0[7..0] into result[16..23], etc
        Build_OP(TOP_rlwimi, result, load1, Gen_Literal_TN(16, 0), 
          Gen_Literal_TN(8, 4), Gen_Literal_TN(15, 4), result, ops);
        Build_OP(TOP_rlwimi, result, load2, Gen_Literal_TN(8, 0), 
          Gen_Literal_TN(16, 4),  Gen_Literal_TN(23, 4), result, ops);
        Build_OP(TOP_rlwimi, result, load3, Gen_Literal_TN(0, 0), 
          Gen_Literal_TN(24, 4),  Gen_Literal_TN(31, 4), result, ops);
        
        Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
        return;
      case MTYPE_I2:
      case MTYPE_U2:
        if (V_align_offset_known(variant) && V_align_offset(variant) >= 2) {
          DevWarn("WHY here! Expand_Composed_Load(I2/U2) V_align_offset > 1");
          Expand_Load(op, result, base, disp, ops);
          return;
        }

        if (TN_is_register(disp)) {            
          Expand_Add(newbase, base, disp, MTYPE_I4, ops);
          base = newbase;
          disp = Gen_Literal_TN(0, 4);
        }
        
        Build_OP(TOP_lbz, result, base, disp, ops);
        Build_OP(TOP_lbz, load1,  base, Gen_Adjusted_TN(disp, 1), ops);
                  
        // result = result & 0xff
        Build_OP(TOP_rlwinm, result, result, Gen_Literal_TN(8, 4), 
            Gen_Literal_TN(16, 4), Gen_Literal_TN(23, 4), ops);
        Build_OP(TOP_rlwimi, result, load1, Gen_Literal_TN(0, 0), 
          Gen_Literal_TN(24, 4), Gen_Literal_TN(31, 4), result, ops);

        Handle_Misc_MType(rtype, desc, result, ops);
        return;
      default:
        FmtAssert (FALSE, ("Expand_Composed_Load: unexpected operand size\n"));
    }
  }
}

void
Expand_Misaligned_Load (OPCODE op, TN *result, TN *base, TN *disp, VARIANT variant, OPS *ops)
{
  Expand_Composed_Load (op, result, base, disp, variant, ops);
}

void
Expand_Composed_Store (TYPE_ID mtype, TN *obj, TN *base, TN *disp, VARIANT variant, OPS *ops)
{
  Expand_Store(mtype, obj, base, disp, ops);
  return;
}

void
Expand_Misaligned_Store (TYPE_ID mtype, TN *obj_tn, TN *base_tn, TN *disp_tn, VARIANT variant, OPS *ops)
{
  Expand_Composed_Store (mtype, obj_tn, base_tn, disp_tn, variant, ops);
}


BOOL Is_Stack_Used(void)
{
  return FALSE;
}

#ifdef TARG_PPC32
/* this function is used to get vbuf variable relocation type.
 * first get vbuf type through st and then relocation type can
 * be decided through intrinsic_id and vbuf type. why we need 
 * intrinsic id is because relocation type in different intrinsic
 * function call has different relocation type such as lwc2 and vbl
 */
TN_RELOCS
Get_Internal_Buf_Reloc_Type(ST* st, INTRINSIC id) {
FmtAssert(FALSE, ("Not IMP"));  
}
#endif 


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
  VARIANT variant,
  INTRINSIC intrn_id = INTRINSIC_INVALID)
{
  ST *base_sym;
  INT64 base_ofst;
  TN *base_tn;
  TN *ofst_tn;
  TN *tmp_tn;
  BOOL is_lda = (!is_load && !is_store);
  OPS newops;
  OP *op;
  OPS_Init(&newops);

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
  
  Allocate_Object(sym);         /* make sure sym is allocated */
  
  Base_Symbol_And_Offset_For_Addressing (sym, ofst, &base_sym, &base_ofst);

  if (base_sym == SP_Sym || base_sym == FP_Sym) {
    base_tn = (base_sym == SP_Sym) ? SP_TN : FP_TN;
    if (sym == base_sym) {
      // can have direct reference to SP or FP,
      // e.g. if actual stored to stack.
      if (ISA_LC_Value_In_Class(base_ofst, LC_simm16))
          ofst_tn = Gen_Literal_TN (base_ofst, 4);
      else {
        if (is_lda) {
          tmp_tn = Build_TN_Of_Mtype(Pointer_Mtype);
          Expand_Add(tmp_tn, base_tn, Gen_Literal_TN(base_ofst, Pointer_Size), MTYPE_U4, &newops);
          base_tn = tmp_tn;
          ofst_tn = Gen_Literal_TN(0, Pointer_Size);
        }
        else {
          ofst_tn = Expand_Immediate_Into_Register(NULL, Gen_Literal_TN(base_ofst, Pointer_Size), MTYPE_U4, &newops);
        }
      }
    }
    else {
      /* Because we'd like to see symbol name in .s file, 
       * still reference the symbol rather than the sp/fp base.  
       * Do put in the offset from the symbol.  
       * We put the symbol in the TN and then
       * let cgemit replace symbol with the final offset.
       * We generate a SW reg, <sym>, <SP> rather than SW reg,<sym>
       * because cgemit and others expect a separate tn for the
       * offset and base. 
       */
      if (ISA_LC_Value_In_Class(base_ofst, LC_simm16))
        ofst_tn = Gen_Symbol_TN (sym, ofst, 0);
      else {
        if (is_lda) {
          tmp_tn = Build_TN_Of_Mtype(Pointer_Mtype);
          Expand_Add(tmp_tn, base_tn, Gen_Literal_TN(base_ofst, Pointer_Size), MTYPE_U4, &newops);
          base_tn = tmp_tn;
          ofst_tn = Gen_Literal_TN(0, Pointer_Size);
        }
        else {
          ofst_tn = Expand_Immediate_Into_Register(NULL, Gen_Literal_TN(base_ofst, Pointer_Size), MTYPE_U4, &newops);
        }
      }
    }
  }
  else if ((ST_class(base_sym) == CLASS_BLOCK || ST_class(base_sym)==CLASS_VAR)
       && ST_gprel(base_sym)) 
    {
      // gp-relative reference
      PU_References_GP = TRUE;       
      if (ISA_LC_Value_In_Class(base_ofst, LC_simm16)) {
        base_tn = Build_TN_Of_Mtype(Pointer_Mtype);
        Build_OP(TOP_lis, base_tn, Gen_Symbol_TN(base_sym, base_ofst, TN_RELOC_HIGH16), &newops);
        ofst_tn = Gen_Symbol_TN(base_sym, base_ofst, TN_RELOC_LOW16);
      } 
      else {
          FmtAssert(FALSE, ("gp-relative offset doesn't fit in 16 bits"));
      }
    }
  else if (! Gen_PIC_Shared) {
    base_tn = Build_TN_Of_Mtype(Pointer_Mtype);
    Build_OP(TOP_lis, base_tn, Gen_Symbol_TN(base_sym, base_ofst, TN_RELOC_HIGH16), &newops);
    ofst_tn = Gen_Symbol_TN(base_sym, base_ofst, TN_RELOC_LOW16);
    // Build_OP(TOP_la, base_tn, ofst_tn, base_tn, &newops);
    // ofst_tn = Gen_Literal_TN(0, Pointer_Size);
  }
  else if (Guaranteed_Small_GOT) {
    TN *tmp2;
    if (! ST_is_export_local(base_sym) &&
        ! ISA_LC_Value_In_Class(base_ofst, LC_simm16)) { // use %got_page and %got_offset
      tmp2 = Build_TN_Of_Mtype (Pointer_Mtype); //to store loaded sym addr 
      Expand_Load (
        // load is of address, not of result type
        OPCODE_make_signed_op(OPR_LDID, Pointer_Mtype, Pointer_Mtype, FALSE),
        tmp2, GP_TN, Gen_Symbol_TN(base_sym, base_ofst, TN_RELOC_GOT_PAGE), 
        &newops);
      // got address should not alias
      Set_OP_no_alias(OPS_last(&newops));
      base_tn = tmp2;
      // add offset to address
      ofst_tn = Gen_Symbol_TN(base_sym, base_ofst, TN_RELOC_GOT_OFST);
    }
    else {
      if (is_lda && OFFSET_LO(base_ofst) == 0) {
        // want to stop at address
        tmp2 = tn;
        is_lda = FALSE; // to save the additional add
      }
      else tmp2 = Build_TN_Of_Mtype (Pointer_Mtype); //to store loaded sym addr 

      Expand_Load (
        // load is of address, not of result type
        OPCODE_make_signed_op(OPR_LDID, Pointer_Mtype, Pointer_Mtype, FALSE),
        tmp2, GP_TN, 
        Gen_Symbol_TN(base_sym, OFFSET_HI(base_ofst), TN_RELOC_GOT_DISP), 
        &newops);

      // got address should not alias
      Set_OP_no_alias(OPS_last(&newops));
      base_tn = tmp2;
      // add offset to address
      ofst_tn = Gen_Literal_TN(OFFSET_LO(base_ofst), 4);
    }
  }
  else {
    FmtAssert(FALSE, ("NYI: Exp_Ldst"));
  }
  
  if (is_store) {
    if (variant == 0)
      Expand_Store (OPCODE_desc(opcode), tn, base_tn, ofst_tn, &newops);
    else 
      Expand_Misaligned_Store (OPCODE_desc(opcode), tn, 
                   base_tn, ofst_tn, variant, &newops);
  }
  else if (is_load) {
    if (variant == 0)
      Expand_Load (opcode, tn, base_tn, ofst_tn, &newops);
    else 
      Expand_Misaligned_Load (opcode, tn, 
                  base_tn, ofst_tn, variant, &newops);
  }
  else if (is_lda) {
    if ( ST_sclass(sym) == SCLASS_FORMAL && MTYPE_is_m(TY_mtype(ST_type(sym))) ) {
      base_tn = SP_TN;
      Build_OP(TOP_lwz, tn, base_tn, Gen_Symbol_TN(sym, 0, 0), &newops);
      if (ofst > 0)
        Expand_Add(tn, tn, Gen_Literal_TN(ofst, 4), MTYPE_U4, &newops);
    }
    else {
      Build_OP(TOP_la, tn, ofst_tn, base_tn, &newops);
    }
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
  OPS *ops,
  INTRINSIC intrn_id)
{
  OPCODE opcode = OPCODE_make_op(OPR_LDA, mtype, MTYPE_V);
  Exp_Ldst (opcode, tgt_tn, sym, ofst, 
    (call_opr == OPR_ICALL),
    FALSE, FALSE, ops, 0, intrn_id);
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
  Exp_Ldst(opcode, tgt_tn, sym, ofst, FALSE, FALSE, TRUE, ops, variant);
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
  Exp_Ldst(opcode, src_tn, sym, ofst, FALSE, TRUE, FALSE, ops, variant);
}

static ISA_ENUM_CLASS_VALUE
Pick_Prefetch_Hint (VARIANT variant)
{
  UINT32 pf_flags = V_pf_flags(variant);
  if (PF_GET_READ(pf_flags)) {
    if (PF_GET_STRIDE_1L(pf_flags))
      return ECV_pfhint_L1_load;
    else return ECV_pfhint_L2_load;
  }
  else {
    if (PF_GET_STRIDE_1L(pf_flags))
      return ECV_pfhint_L1_store;
    else return ECV_pfhint_L2_store;
  }
}

void Exp_Prefetch (TOP opc, TN* src1, TN* src2, VARIANT variant, OPS* ops)
{
  FmtAssert(FALSE, ("Exp_Prefetch : NYI"));  
}

/* ======================================================================
 * Exp_Extract_Bits
 * ======================================================================*/
void
Exp_Extract_Bits (TYPE_ID rtype, TYPE_ID desc, UINT bit_offset, UINT bit_size,
          TN *tgt_tn, TN *src_tn, OPS *ops)
{
  //FIXME:change to single def, and tgt_tn should not be used as src_opnd in ops
  FmtAssert(bit_size != 0, ("size of bit field cannot be 0"));
  UINT rsize = MTYPE_bit_size(rtype) <= 32 ? MTYPE_bit_size(rtype) : 32;
  UINT dsize = MTYPE_bit_size(desc) <= 32 ? MTYPE_bit_size(desc) : 32;
  FmtAssert((bit_offset + bit_size) <= MTYPE_bit_size(desc), ("Exp_Extract_Bits : I8 <- I8 bits out of limit"));
  if (!OP_NEED_PAIR(rtype)) {
    if (!OP_NEED_PAIR(desc)) { //I4 <-I4
      FmtAssert((bit_offset + bit_size) <= MTYPE_bit_size(desc), ("Exp_Extract_Bits : I4 <- I4 bits out of limit"));
      UINT rotate = (32 - dsize + bit_offset+bit_size) % 32;
      UINT mb = 32 - bit_size;
      UINT me = 31;
      Build_OP(TOP_rlwinm, tgt_tn, src_tn, Gen_Literal_TN(rotate, 4), 
          Gen_Literal_TN(mb, 4), Gen_Literal_TN(me, 4), ops);
    } else { // I4 <- I8
      TN * src_high = Get_TN_Pair(src_tn);
      FmtAssert(src_high, ("Exp_Extract_Bits : I4 <- I8 src tn pair not setup"));
      FmtAssert(bit_size <= 32, ("Exp_Extract_Bits : I4 <- I8  result bit size out of limit"));
      if (bit_offset < 32) {
        if (bit_offset + bit_size <= 32) {
          UINT rotate = (bit_offset+bit_size) % 32;
          UINT mb = 32 - bit_size;
          UINT me = 31;
          Build_OP(TOP_rlwinm, tgt_tn, src_high, Gen_Literal_TN(rotate, 4), 
               Gen_Literal_TN(mb, 4), Gen_Literal_TN(me, 4), ops);
        } else {
          UINT high_rotate = bit_offset + bit_size - 32;
          UINT high_mb = 32 - bit_size;
          UINT high_me = 63 - bit_offset - bit_size;
          Build_OP(TOP_rlwinm, tgt_tn, src_high, Gen_Literal_TN(high_rotate, 4), 
            Gen_Literal_TN(high_mb, 4), Gen_Literal_TN(high_me, 4), ops);
          UINT low_rotate = bit_offset + bit_size - 32;
          UINT low_mb = 64 - bit_offset - bit_size;
          UINT low_me = 31;
          Build_OP(TOP_rlwimi, tgt_tn, src_tn, Gen_Literal_TN(low_rotate, 4), 
            Gen_Literal_TN(low_mb, 4), Gen_Literal_TN(low_me, 4), tgt_tn, ops);
        }
      } else {// bit_offset >= 32
        UINT rotate = (bit_offset + bit_size - 32) % 32;
        UINT mb = 32 - bit_size;
        UINT me = 31;
        Build_OP(TOP_rlwinm, tgt_tn, src_high, Gen_Literal_TN(rotate, 4), 
            Gen_Literal_TN(mb, 4), Gen_Literal_TN(me, 4), ops);
      }
    }
    if (MTYPE_is_signed(rtype)) {
      if (bit_size == 8) {
        Build_OP(TOP_extsb, tgt_tn, tgt_tn, ops);
      } else if (bit_size == 16) {
        Build_OP(TOP_extsh, tgt_tn, tgt_tn, ops);
      } else if (bit_size != 32){
        Build_OP(TOP_slwi,  tgt_tn, tgt_tn, Gen_Literal_TN(32 - bit_size, 4), ops);
        Build_OP(TOP_srawi, tgt_tn, tgt_tn, Gen_Literal_TN(32 - bit_size, 4), ops);
      }
    }
  } 
  else {
    TN * tgt_high = Get_TN_Pair(tgt_tn);
    FmtAssert(tgt_high, ("Exp_Extract_Bits : I8 <- I8 result tn pair not setup"));
    if (!OP_NEED_PAIR(desc)) {// I8 <- I4
      FmtAssert(bit_offset < dsize, ("Exp_Extract_Bits : I8 <- I4 bit offset out of limit"));
      FmtAssert((bit_offset + bit_size) <= dsize, ("Exp_Extract_Bits : I8 <- I4 bits out of limit"));

      UINT rotate = (32 - dsize + bit_offset + bit_size) % 32;
      UINT mb = 32 - bit_size;
      UINT me = 31;
      Build_OP(TOP_rlwinm, tgt_tn, src_tn, Gen_Literal_TN(rotate, 4), 
        Gen_Literal_TN(mb, 4), Gen_Literal_TN(me, 4), ops);
    } else {// I8 <- I8
      TN * src_high = Get_TN_Pair(src_tn);
      FmtAssert(src_high, ("Exp_Extract_Bits : I8 <- I8 source tn pair not setup"));

      if (bit_offset < 32){
        if (bit_offset + bit_size <= 32) {
          UINT rotate = (bit_offset+bit_size) % 32;
          UINT mb = 32 - bit_size;
          UINT me = 31;
          Build_OP(TOP_rlwinm, tgt_tn, src_high, Gen_Literal_TN(rotate, 4), 
               Gen_Literal_TN(mb, 4), Gen_Literal_TN(me, 4), ops);
        } else if (bit_size <= 32){
          UINT high_rotate = bit_offset + bit_size - 32;
          UINT high_mb = 32 - bit_size;
          UINT high_me = 63 - bit_offset - bit_size;
          Build_OP(TOP_rlwinm, tgt_tn, src_high, Gen_Literal_TN(high_rotate, 4), 
            Gen_Literal_TN(high_mb, 4), Gen_Literal_TN(high_me, 4), ops);
          UINT low_rotate = bit_offset + bit_size - 32;
          UINT low_mb = 64 - bit_offset - bit_size;
          UINT low_me = 31;
          Build_OP(TOP_rlwimi, tgt_tn, src_tn, Gen_Literal_TN(low_rotate, 4), 
            Gen_Literal_TN(low_mb, 4), Gen_Literal_TN(low_me, 4), tgt_tn, ops);
        } else {
          UINT hh_rotate = (bit_offset + bit_size - 32) % 32;
          UINT hh_mb = 64 - bit_size;
          UINT hh_me = 31;
          Build_OP(TOP_rlwinm, tgt_high, src_high, Gen_Literal_TN(hh_rotate, 4), 
            Gen_Literal_TN(hh_mb, 4), Gen_Literal_TN(hh_me, 4), ops);
          if (bit_size + bit_offset < 64) {
            UINT lh_rotate = (bit_offset + bit_size - 32) % 32;
            UINT lh_mb = 0;
            UINT lh_me = 63 - bit_offset - bit_size;
            Build_OP(TOP_rlwinm, tgt_tn, src_high, Gen_Literal_TN(lh_rotate, 4), 
              Gen_Literal_TN(lh_mb, 4), Gen_Literal_TN(lh_me, 4), ops);

            UINT ll_rotate = (bit_offset + bit_size - 32) % 32;
            UINT ll_mb = 64 - bit_offset - bit_size;
            UINT ll_me = 31;
            Build_OP(TOP_rlwimi, tgt_tn, src_tn, Gen_Literal_TN(ll_rotate, 4), 
              Gen_Literal_TN(ll_mb, 4), Gen_Literal_TN(ll_me, 4), tgt_tn, ops);
          } else {
            Expand_Copy(tgt_tn,src_tn, MTYPE_I4, ops);
          }
        }
      } else {  // bit_offset >= 32
        UINT rotate = (bit_offset + bit_size - 32) % 32;
        UINT mb = 32 - bit_size;
        UINT me = 31;
        Build_OP(TOP_rlwinm, tgt_tn, src_tn, Gen_Literal_TN(rotate, 4), 
          Gen_Literal_TN(mb, 4), Gen_Literal_TN(me, 4), ops);       
      }
    }
    if (bit_size < 32) {
      if (MTYPE_is_signed(rtype)) {
          if (bit_size == 8) {
            Build_OP(TOP_extsb, tgt_tn, tgt_tn, ops);
          } else if (bit_size == 16) {
            Build_OP(TOP_extsh, tgt_tn, tgt_tn, ops);
          } else {
            Build_OP(TOP_slwi,  tgt_tn, tgt_tn, Gen_Literal_TN(32 - bit_size, 4), ops);
            Build_OP(TOP_srawi, tgt_tn, tgt_tn, Gen_Literal_TN(32 - bit_size, 4), ops);
          }
          Build_OP(TOP_srawi, tgt_high, tgt_tn, Gen_Literal_TN(31, 4), ops);
      } else {
          Build_OP(TOP_li, tgt_high, Gen_Literal_TN(0, 4), ops);
      }
    } else if (bit_size == 32) {
      if (MTYPE_is_signed(rtype)) {
        Build_OP(TOP_srawi, tgt_high, tgt_tn, Gen_Literal_TN(31, 4), ops);
      } else {
        Build_OP(TOP_li, tgt_high, Gen_Literal_TN(0, 4), ops);
      }
    } else if (bit_size != 64){
      if (MTYPE_is_signed(rtype)) {
          if (bit_size == 40) {
            Build_OP(TOP_extsb, tgt_high, tgt_high, ops);
          } else if (bit_size == 48) {
            Build_OP(TOP_extsh, tgt_high, tgt_high, ops);
          } else {
            Build_OP(TOP_slwi,  tgt_high, tgt_high, Gen_Literal_TN(64 - bit_size, 4), ops);
            Build_OP(TOP_srawi, tgt_high, tgt_high, Gen_Literal_TN(64 - bit_size, 4), ops);
          }
      }
    }
  }
}

/* ======================================================================
 * Exp_Deposit_Bits - deposit src2_tn into a field of src1_tn returning
 * the result in tgt_tn.
 * ======================================================================*/
void
Exp_Deposit_Bits (TYPE_ID rtype, TYPE_ID desc, UINT bit_offset, UINT bit_size,
  TN *tgt_tn, TN *src1_tn, TN *src2_tn, OPS *ops)
{
  UINT rsize = MTYPE_bit_size(rtype) <= 32 ? MTYPE_bit_size(rtype) : 32;
  UINT dsize = MTYPE_bit_size(desc) <= 32 ? MTYPE_bit_size(desc) : 32;
  FmtAssert(bit_offset + bit_size <= MTYPE_bit_size(rtype), ("Exp_Deposit_Bits : bit out of range"));
  
  if (tgt_tn != src1_tn) {
    Expand_Copy(tgt_tn, src1_tn, rtype, ops);
  }
  
  if (rsize < 32) { // for stb and sth in stbits
	UINT rotate = rsize - bit_offset - bit_size;
	UINT mb = 32 - rsize + bit_offset;
	UINT me = 32 - rsize + bit_offset + bit_size - 1;
	Build_OP(TOP_rlwimi, tgt_tn, src2_tn, Gen_Literal_TN(rotate, 4),
		Gen_Literal_TN(mb, 4), Gen_Literal_TN(me, 4), tgt_tn, ops);
  } else {
	  if (!OP_NEED_PAIR(rtype) && !OP_NEED_PAIR(desc)) { // I4 <- I4
	    UINT rotate = 32 - bit_size - bit_offset;
	    UINT mb = bit_offset;
	    UINT me = bit_offset + bit_size - 1;
	    Build_OP(TOP_rlwimi, tgt_tn, src2_tn, Gen_Literal_TN(rotate, 4),
	      Gen_Literal_TN(mb, 4), Gen_Literal_TN(me, 4), tgt_tn, ops);
	  }
	  else if (!OP_NEED_PAIR(rtype) && OP_NEED_PAIR(desc)) { // I4 <- I8
	    UINT rotate = 32 - bit_size - bit_offset;
	    UINT mb = bit_offset;
	    UINT me = bit_offset + bit_size - 1;
	    Build_OP(TOP_rlwimi, tgt_tn, src2_tn, Gen_Literal_TN(rotate, 4),
	      Gen_Literal_TN(mb, 4), Gen_Literal_TN(me, 4), tgt_tn, ops);  
	  }
	  else if (OP_NEED_PAIR(rtype) && !OP_NEED_PAIR(desc)) { // I8 <- I4
	    TN * tgt_high = Get_TN_Pair(tgt_tn);
	    FmtAssert(tgt_high, ("Exp_Deposit_Bits : I8 <- I4 source1 tn pair not setup"));
	    
	    if (bit_offset < 32) {
	      if (bit_offset + bit_size <= 32) {
	        UINT rotate = 32 - bit_offset - bit_size;
	        UINT mb = bit_offset;
	        UINT me = bit_offset + bit_size - 1;
	        Build_OP(TOP_rlwimi, tgt_high, src2_tn, Gen_Literal_TN(rotate, 4),
	          Gen_Literal_TN(mb, 4), Gen_Literal_TN(me, 4), tgt_high, ops);    
	      } else {// bit_offset < 32, bit_offset + bit_size > 32
	        UINT high_rotate = 64 - bit_offset - bit_size;
	        UINT high_mb = bit_offset;
	        UINT high_me = 31;
	        Build_OP(TOP_rlwimi, tgt_high, src2_tn, Gen_Literal_TN(high_rotate, 4),
	          Gen_Literal_TN(high_mb, 4), Gen_Literal_TN(high_me, 4), tgt_high, ops);
	        UINT low_rotate = 64 - bit_offset - bit_size;
	        UINT low_mb = 0;
	        UINT low_me = bit_offset + bit_size - 33;
	        Build_OP(TOP_rlwimi, tgt_tn, src2_tn, Gen_Literal_TN(low_rotate, 4),
	          Gen_Literal_TN(low_mb, 4), Gen_Literal_TN(low_me, 4), tgt_tn, ops);
	      }      
	    }
	    else {  // bit_offset > 32      
	      UINT rotate = 64 - bit_offset - bit_size;
	      UINT mb = bit_offset - 32;
	      UINT me = bit_size + bit_offset - 33;
	      Build_OP(TOP_rlwimi, tgt_tn, src2_tn, Gen_Literal_TN(rotate, 4),
	        Gen_Literal_TN(mb, 4), Gen_Literal_TN(me, 4), tgt_tn, ops);  
	    }
	  }
	  else {  // I8 <- I8
	    TN * tgt_high = Get_TN_Pair(tgt_tn);
	    TN * src2_high = Get_TN_Pair(src2_tn);
	    FmtAssert(tgt_high, ("Exp_Deposit_Bits : I8 <- I8 source1 tn pair not setup"));
	    FmtAssert(src2_high, ("Exp_Deposit_Bits : I8 <- I8 source2 tn pair not setup"));
	    
	    if (bit_offset < 32) {
	      if (bit_offset + bit_size <= 32) {
	        UINT rotate = 32 - bit_offset - bit_size;
	        UINT mb = bit_offset;
	        UINT me = bit_offset + bit_size - 1;
	        Build_OP(TOP_rlwimi, tgt_high, src2_tn, Gen_Literal_TN(rotate, 4),
	          Gen_Literal_TN(mb, 4), Gen_Literal_TN(me, 4), tgt_high, ops);    
	      } else if (bit_size <= 32){
	        UINT high_rotate = 64 - bit_offset - bit_size;
	        UINT high_mb = bit_offset;
	        UINT high_me = 31;
	        Build_OP(TOP_rlwimi, tgt_high, src2_tn, Gen_Literal_TN(high_rotate, 4),
	          Gen_Literal_TN(high_mb, 4), Gen_Literal_TN(high_me, 4), tgt_high, ops);
	        UINT low_rotate = 64 - bit_offset - bit_size;
	        UINT low_mb = 0;
	        UINT low_me = bit_offset + bit_size - 33;
	        Build_OP(TOP_rlwimi, tgt_tn, src2_tn, Gen_Literal_TN(low_rotate, 4),
	          Gen_Literal_TN(low_mb, 4), Gen_Literal_TN(low_me, 4), tgt_tn, ops);
	      } else {
	        UINT hh_rotate = 64 - bit_offset - bit_size;
	        UINT hh_mb = bit_offset;
	        UINT hh_me = bit_offset + bit_size - 33;
	        Build_OP(TOP_rlwimi, tgt_high, src2_high, Gen_Literal_TN(hh_rotate, 4),
	          Gen_Literal_TN(hh_mb, 4), Gen_Literal_TN(hh_me, 4), tgt_high, ops);
	        if (bit_offset + bit_size < 64) {
	          UINT hl_rotate = 64 - bit_offset - bit_size;
	          UINT hl_mb = bit_offset + bit_size - 32;
	          UINT hl_me = 31;
	          Build_OP(TOP_rlwimi, tgt_high, src2_tn, Gen_Literal_TN(hl_rotate, 4),
	          Gen_Literal_TN(hl_mb, 4), Gen_Literal_TN(hl_me, 4), tgt_high, ops);
	          UINT ll_rotate = 64 - bit_offset - bit_size;
	          UINT ll_mb = 0;
	          UINT ll_me = bit_offset + bit_size - 33;
	          Build_OP(TOP_rlwimi, tgt_tn, src2_tn, Gen_Literal_TN(ll_rotate, 4),
	          Gen_Literal_TN(ll_mb, 4), Gen_Literal_TN(ll_me, 4), tgt_tn, ops);
	        } else {
	          Build_OP(TOP_mr, tgt_tn, src2_tn, ops);
	        } 
	      }
	    }
	    else {  // bit_offset >= 32      
	      UINT rotate = 64 - bit_offset - bit_size;
	      UINT mb = bit_offset - 32;
	      UINT me = bit_size + bit_offset - 33;
	      Build_OP(TOP_rlwimi, tgt_tn, src2_tn, Gen_Literal_TN(rotate, 4),
	        Gen_Literal_TN(mb, 4), Gen_Literal_TN(me, 4), tgt_tn, ops);  
	    }
	  }
  }
}

void 
Expand_Lda_Label (TN *dest, TN *lab, OPS *ops)
{
  Set_TN_is_reloc_got_disp(lab);
  if (Use_32_Bit_Pointers)
  {
    TN * base_tn = Build_TN_Of_Mtype(Pointer_Mtype);
    Build_OP(TOP_lis, base_tn, Gen_Symbol_TN(TN_var(lab), 0, TN_RELOC_HIGH16), ops);    
    TN * ofst_tn = Gen_Symbol_TN(TN_var(lab), 0, TN_RELOC_LOW16);
    Build_OP(TOP_la, dest, ofst_tn, base_tn, ops);
  }
  else
    FmtAssert(FALSE, ("Not IMP"));
//    Build_OP(TOP_ld, dest, GP_TN, lab, ops);
}
