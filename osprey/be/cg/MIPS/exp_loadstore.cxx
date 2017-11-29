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
#include "cgemit.h"	// for CG_emit_non_gas_syntax

#define OFFSET_HI(offset) ((((offset) + 0x8000) >> 16) << 16)
#define OFFSET_LO(offset) ((((offset) + 0x8000) & 0xffff) - 0x8000)

void
Expand_Lda (TN *dest, TN *src, OPS *ops)
{
  FmtAssert(FALSE, ("NYI: Expand_Lda"));
}

static TOP
Pick_Load_Instruction (TYPE_ID rtype, TYPE_ID desc)
{
  switch (desc) {
  case MTYPE_I1: return TOP_lb;
  case MTYPE_U1: return TOP_lbu;
  case MTYPE_I2: return TOP_lh;
  case MTYPE_U2: return TOP_lhu;
  case MTYPE_I4: return TOP_lw;
  case MTYPE_U4: return MTYPE_is_size_double(rtype) ? TOP_lwu : TOP_lw;
  case MTYPE_I8: case MTYPE_U8:	return TOP_ld;
  case MTYPE_F4: 			return TOP_lwc1;
  case MTYPE_F8: 			return TOP_ldc1;
    
  case MTYPE_V:
    if (rtype != MTYPE_V)
      // use rtype to pick load (e.g. if lda)
      return Pick_Load_Instruction(rtype,rtype);
    // else fallthru
  default:  
    FmtAssert(FALSE, ("NYI: Pick_Load_Instruction mtype"));
    return TOP_UNDEFINED;
  }
}

#if defined(TARG_SL)

#define max_ofst 31 

BOOL 
Expand_V1buf_Ldst(TOP top, TN* value, TN* base,  TN* ofst, BOOL is_load, OPS* ops)
{
  // following code is used to do following conversion 
  //   lb  ==> c2.ldi.v2g.b
  //   lbu ==> c2.ldi.v2g.b.u
  if(is_load) { 
    if(TN_is_symbol(ofst)) {
      INT64 ofst_val; 
      ST* st = TN_var(ofst);
      ofst_val = TN_value(ofst);
      if(ST_in_v1buf(st)) { 
        ofst = Gen_Symbol_TN(st, ofst_val, 
	         TN_RELOC_GPREL_SL2_V15); 
	if(top == TOP_lbu) 
          top = TOP_c2_ldi_v2g_b_u; 
        else if(top == TOP_lb)
          top = TOP_c2_ldi_v2g_b; 
        else 
          Fail_FmtAssertion("no lbu/lb found"); 
        Build_OP(top, value, ofst, ops); 
        return TRUE; 
      }
    }
    else if(TN_is_v1buf_addr(base)) { // handling iload 
      if(top == TOP_lbu) 
        top = TOP_c2_ld_v2g_b_u; 
      else if(top == TOP_lb)
        top = TOP_c2_ld_v2g_b; 
      else 
        Fail_FmtAssertion("no lbu/lb found"); 

      if(TN_value(ofst) > max_ofst) { 
        Build_OP(TOP_addiu, base, base, ofst, ops); 
        ofst = Gen_Literal_TN(0, 4); 
      }
      Build_OP(top, value, base, ofst, ops); 
      return TRUE; 
    }
  }
  else { //is_store 
    // following code is used to do following conversion 
    //   sb  ==> c2.sti.v2g.b
    if(TN_is_symbol(ofst)) {
      INT64 ofst_val; 
      ST* st = TN_var(ofst);
      ofst_val = TN_value(ofst);
      if(ST_in_v1buf(st)) { 
        ofst = Gen_Symbol_TN(st, ofst_val, 
               TN_RELOC_GPREL_SL2_V15); 
        Build_OP(TOP_c2_sti_g2v_b, value, ofst, ops); 
        return TRUE; 
      }
    }
    else if (TN_is_v1buf_addr(base)) { //handling istore 
      if(TN_value(ofst) > max_ofst) { 
        Build_OP(TOP_addiu, base, base, ofst, ops); 
        ofst = Gen_Literal_TN(0, 4); 
      }
      Build_OP(TOP_c2_st_g2v_b, value, base, ofst, ops); 
      return TRUE; 
    }
  }
  return FALSE; // no expansion happen  
}
#endif 

void
Expand_Load (OPCODE opcode, TN *result, TN *base, TN *ofst, OPS *ops)
{
  TYPE_ID mtype = OPCODE_desc(opcode);
  TOP top = Pick_Load_Instruction (OPCODE_rtype(opcode), mtype);
  Is_True (TN_is_constant(ofst), ("Expand_Load: Illegal offset TN"));
  // Handle very large offsets (that consume greater than 16 bits).
  // Try and see if the base and offset can be merged.
  // An example is gcc.c-torture/compile/990617-1.c
  if (TN_is_rematerializable(base) && 
     (!ISA_LC_Value_In_Class(TN_value(ofst), LC_simm16))) {
    WN *home = TN_home(base);
    if (WN_operator(home) == OPR_INTCONST) {
      INT64 val = WN_const_val(home);
      BOOL is_double = TN_size(base) == 8;
      TN *tmp_base = Gen_Literal_TN (val + TN_value(ofst), TN_size(base));
      tmp_base = Expand_Immediate_Into_Register(tmp_base, is_double, ops);
      Build_OP (top, result, tmp_base, Gen_Literal_TN (0, 2), ops);
      return;
    }
  }
#if defined(TARG_SL)
  if(CG_sl2 && CG_SL2_enable_v1buf_expansion && 
    Expand_V1buf_Ldst(top, result, base, ofst, TRUE, ops)) return; 
#endif 
  Build_OP (top, result, base, ofst, ops);
}

static TOP
Pick_Store_Instruction (TYPE_ID mtype)
{
  switch (mtype) {
  case MTYPE_I1:	case MTYPE_U1:	return TOP_sb;
  case MTYPE_I2:	case MTYPE_U2:	return TOP_sh;
  case MTYPE_I4:	case MTYPE_U4:	return TOP_sw;
  case MTYPE_I8: 	case MTYPE_U8:	return TOP_sd;
  case MTYPE_F4: 			return TOP_swc1;
  case MTYPE_F8: 			return TOP_sdc1;
  default:  FmtAssert(FALSE, ("NYI: Pick_Store_Instruction mtype"));
    return TOP_UNDEFINED;
  }
}

void
Expand_Store (TYPE_ID mtype, TN *src, TN *base, TN *ofst, OPS *ops)
{
  TOP top = Pick_Store_Instruction (mtype);
  Is_True (TN_is_constant(ofst), ("Expand_Store: Illegal offset TN"));

#if defined(TARG_SL)
  if(CG_sl2 && CG_SL2_enable_v1buf_expansion && 
    Expand_V1buf_Ldst(top, src, base, ofst, FALSE, ops)) return; 
#endif 
  if (!TN_has_value(ofst) || ISA_LC_Value_In_Class(TN_value(ofst), LC_simm16))
    Build_OP (top, src, base, ofst, ops);
  else {
    TN *tmp_tn = Build_TN_Of_Mtype(Pointer_Mtype);
    INT32 tmp = TN_value(ofst) >> 16;
    if (tmp < 0) {
      // immediate has been sign extended, chop off the top 4 bytes
      if (Use_32_Bit_Pointers)
	tmp &= 0xffff;
    }
    Build_OP(TOP_lui, tmp_tn, Gen_Literal_TN(tmp, 4), ops);
    Build_OP(Use_32_Bit_Pointers ? TOP_addu : TOP_daddu, tmp_tn, 
	     tmp_tn, base, ops);
    Build_OP(top, src, tmp_tn, Gen_Literal_TN(TN_value(ofst)&0xffff, 4), ops);
  }
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
  TOP	opcode,		/* The new memory operation */
  TN	**base_tn,	/* The base address -- may be modified */
  TN	**disp_tn,	/* The displacement -- may be modified */
  INT16	disp,		/* A displacement to add */
  OPS *ops)
{

  if ( Potential_Immediate_TN_Expr (opcode, *disp_tn, disp) )
  {
    if ( TN_has_value(*disp_tn) ) {
      *disp_tn = Gen_Literal_TN ( TN_value(*disp_tn) + disp, 4 );
    } else {
      *disp_tn = Gen_Symbol_TN ( TN_var(*disp_tn),
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

#ifdef TARG_SL
static void
Expand_Composed_Load ( OPCODE op, TN *result, TN *base, TN *disp, VARIANT variant, OPS *ops)
{
  TYPE_ID rtype= OPCODE_rtype(op);
  TYPE_ID desc = OPCODE_desc(op);
  DevWarn ("Expand_Composed_Load: Unaligned load found");
  if (MTYPE_is_float(rtype)) {
    TN     *load;
    DevWarn ("Float type in Expand_Composed_Load");
    if (rtype == MTYPE_F4) {
      load = Build_TN_Of_Mtype(MTYPE_I4);
      Expand_Composed_Load ( OPC_I4I4LDID, load, base, disp, variant, ops);
      // mtc1 moves bits into fp reg.
      Build_OP ( TOP_mtc1, result, load, ops );
    } else {
      load = Build_TN_Of_Mtype(MTYPE_I8);
      Expand_Composed_Load ( OPC_I8I8LDID, load, base, disp, variant, ops);
      Build_OP ( TOP_dmtc1, result, load, ops );
    }
    Reset_TN_is_fpu_int(result);
    return;
  }
	else {
	  TN *load1 = Build_TN_Of_Mtype(MTYPE_I4);
	  TN *load2 = Build_TN_Of_Mtype(MTYPE_I4);
	  TN *load3 = Build_TN_Of_Mtype(MTYPE_I4);
	  TN *newbase = Build_TN_Of_Mtype(MTYPE_I4); 
	  switch (desc) {
	    case MTYPE_I8:
	    case MTYPE_U8:
				FmtAssert (FALSE, ("Expand_Composed_Load: 64bits data\n"));
	      return;
	    case MTYPE_I4:
	    case MTYPE_U4:
				Build_OP (TOP_lbu, result, base, disp, ops);		 

	      Expand_Add (newbase, base, disp, MTYPE_I4, ops);	
	      Build_OP (TOP_lbu, load1, newbase, Gen_Literal_TN(1, 4), ops);	
	      Build_OP(TOP_depb, result, result, load1,  Gen_Literal_TN(15, 4), Gen_Literal_TN(8, 4), ops);
	      
	      Build_OP (TOP_lbu, load2, newbase, Gen_Literal_TN(2, 4), ops);		 
	      Build_OP(TOP_depb, result, result, load2,  Gen_Literal_TN(23, 4), Gen_Literal_TN(8, 4), ops);     

	      Build_OP (TOP_lbu, load3, newbase, Gen_Literal_TN(3, 4), ops);		 
	      Build_OP(TOP_depb, result, result, load3,  Gen_Literal_TN(31, 4), Gen_Literal_TN(8, 4), ops);
	      
	      //Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
	      return;
	    case MTYPE_I2:
	    case MTYPE_U2:
				Build_OP (TOP_lbu, result, base, disp, ops);		 

	      Expand_Add (newbase, base, disp, MTYPE_I4, ops);	
	      Build_OP (TOP_lbu, load1, newbase, Gen_Literal_TN(1, 4), ops);	
	      Build_OP(TOP_depb, result, result, load1,  Gen_Literal_TN(15, 4), Gen_Literal_TN(8, 4), ops);	    
	      return;
	    default:
	      FmtAssert (FALSE, ("Expand_Composed_Load: unexpected operand size\n"));
	  }
	}
}
#else
static void
Expand_Composed_Load ( OPCODE op, TN *result, TN *base, TN *disp, VARIANT variant, OPS *ops)
{
  TYPE_ID rtype= OPCODE_rtype(op);
  TYPE_ID desc = OPCODE_desc(op);
  TN *tmp;

  if (MTYPE_is_float(rtype)) {
    TN     *load;

    if (rtype == MTYPE_F4) {
      load = Build_TN_Of_Mtype(MTYPE_I4);
      Expand_Composed_Load ( OPC_I4I4LDID, load, base, disp, variant, ops);
      // mtc1 moves bits into fp reg.
      Build_OP ( TOP_mtc1, result, load, ops );
    } else {
      load = Build_TN_Of_Mtype(MTYPE_I8);
      Expand_Composed_Load ( OPC_I8I8LDID, load, base, disp, variant, ops);
      Build_OP ( TOP_dmtc1, result, load, ops );
    }
    Reset_TN_is_fpu_int(result);
    return;
  }

  switch (desc) {
    case MTYPE_I8:
    case MTYPE_U8:
      Build_OP (Target_Byte_Sex == BIG_ENDIAN ? TOP_ldl : TOP_ldr,
  		result, base, disp, ops);
      // The highest byte is at effective address + 7.
      Adjust_Addr_TNs (Target_Byte_Sex == BIG_ENDIAN ? TOP_ldr : TOP_ldl,
  		      &base, &disp, 7, ops);
      Build_OP (Target_Byte_Sex == BIG_ENDIAN ? TOP_ldr : TOP_ldl,
  		result, base, disp, ops);
      Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
      return;
    case MTYPE_I4:
    case MTYPE_U4:
      Build_OP (Target_Byte_Sex == BIG_ENDIAN ? TOP_lwl : TOP_lwr,
  		result, base, disp, ops);
      // The highest byte is at effective address + 3.
      Adjust_Addr_TNs (Target_Byte_Sex == BIG_ENDIAN ? TOP_lwr : TOP_lwl,
  		      &base, &disp, 3, ops);
      Build_OP (Target_Byte_Sex == BIG_ENDIAN ? TOP_lwr : TOP_lwl,
  		result, base, disp, ops);
      Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
      // Clear the top 32 bits for U8U4 load.
      if ((rtype == MTYPE_U8) && (desc == MTYPE_U4)) {
	Build_OP (TOP_dsll32, result, result, Gen_Literal_TN(0, 4), ops);
	Build_OP (TOP_dsrl32, result, result, Gen_Literal_TN(0, 4), ops);
      }
      return;
    case MTYPE_I2:
    case MTYPE_U2:
      if (Target_Byte_Sex == BIG_ENDIAN) {
        Build_OP (desc == MTYPE_I2 ? TOP_lb : TOP_lbu,
		  result, base, disp, ops);
	Build_OP (TOP_sll, result, result, Gen_Literal_TN(8, 4), ops);
        // The LSB is at effective address + 1.
        Adjust_Addr_TNs (TOP_lb, &base, &disp, 1, ops);
        tmp = Build_TN_Of_Mtype(rtype);
        Build_OP (TOP_lbu, tmp, base, disp, ops);
        Build_OP (TOP_or, result, result, tmp, ops);
      } else {
        tmp = Build_TN_Of_Mtype(rtype);
        Build_OP (TOP_lbu, tmp, base, disp, ops);
        // The MSB is at effective address + 1.
        Adjust_Addr_TNs (TOP_lb, &base, &disp, 1, ops);
        Build_OP (desc == MTYPE_I2 ? TOP_lb : TOP_lbu,
		  result, base, disp, ops);
	Build_OP (TOP_sll, result, result, Gen_Literal_TN(8, 4), ops);
        Build_OP (TOP_or, result, result, tmp, ops);
      }
      return;
    default:
      FmtAssert (FALSE, ("Expand_Composed_Load: unexpected operand size\n"));
  }
}
#endif

void
Expand_Misaligned_Load ( OPCODE op, TN *result, TN *base, TN *disp, VARIANT variant, OPS *ops)
{
  Expand_Composed_Load ( op, result, base, disp, variant, ops);
}

#ifdef TARG_SL
void
Expand_Composed_Store (TYPE_ID mtype, TN *obj, TN *base, TN *disp, VARIANT variant, OPS *ops)
{
	DevWarn ("Expand_Composed_Store: Unaligned store found");
  if (MTYPE_is_float(mtype))
  {
	TN     *tmp;
	DevWarn ("Float type in Expand_Composed_Load");
	if (mtype == MTYPE_F4)
	{
		tmp = Build_TN_Of_Mtype(MTYPE_I4);
		Build_OP ( TOP_mfc1, tmp, obj, ops );
		Expand_Composed_Store (MTYPE_I4, tmp, base, disp, variant, ops);
	}
	else
	{
		tmp = Build_TN_Of_Mtype(MTYPE_I8);
		Build_OP ( TOP_dmfc1, tmp, obj, ops );
		Expand_Composed_Store (MTYPE_I8, tmp, base, disp, variant, ops);
	}
	return;
  }

  else {
	  TN *store1 = Build_TN_Of_Mtype(MTYPE_I4);
	  TN *store2 = Build_TN_Of_Mtype(MTYPE_I4);
	  TN *store3 = Build_TN_Of_Mtype(MTYPE_I4);
	  TN *newbase = Build_TN_Of_Mtype(MTYPE_I4); 
	  switch (mtype) {
	    case MTYPE_I8:
	    case MTYPE_U8:
				FmtAssert (FALSE, ("Expand_Composed_Store: 64bits data\n"));
	      return;
	    case MTYPE_I4:
	    case MTYPE_U4:
					Build_OP (TOP_sb, obj, base, disp, ops);		 
	
		      Expand_Add (newbase, base, disp, MTYPE_I4, ops);	
		      Build_OP(TOP_extrbu, store1, obj,  Gen_Literal_TN(15, 4), Gen_Literal_TN(8, 4), ops);
		      Build_OP (TOP_sb, store1, newbase, Gen_Literal_TN(1, 4), ops);	
		      
		      Build_OP(TOP_extrbu, store2, obj,  Gen_Literal_TN(23, 4), Gen_Literal_TN(8, 4), ops);
		      Build_OP (TOP_sb, store2, newbase, Gen_Literal_TN(2, 4), ops);	
		      
		      Build_OP(TOP_extrbu, store3, obj,  Gen_Literal_TN(31, 4), Gen_Literal_TN(8, 4), ops);
		      Build_OP (TOP_sb, store3, newbase, Gen_Literal_TN(3, 4), ops);			      
		      
		      Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
	      return;
	    case MTYPE_I2:
	    case MTYPE_U2:
					Build_OP (TOP_sb, obj, base, disp, ops);		 
	
		      Expand_Add (newbase, base, disp, MTYPE_I4, ops);	
		      Build_OP(TOP_extrbu, store1, obj,  Gen_Literal_TN(15, 4), Gen_Literal_TN(8, 4), ops);
		      Build_OP (TOP_sb, store1, newbase, Gen_Literal_TN(1, 4), ops);		    
	      return;
	    default:
	      FmtAssert (FALSE, ("Expand_Composed_Load: unexpected operand size\n"));
	  }	
  }
}
#else
static void
Expand_Composed_Store (TYPE_ID mtype, TN *obj, TN *base, TN *disp, VARIANT variant, OPS *ops)
{
  if (MTYPE_is_float(mtype))
  {
	TN     *tmp;

	if (mtype == MTYPE_F4)
	{
		tmp = Build_TN_Of_Mtype(MTYPE_I4);
		Build_OP ( TOP_mfc1, tmp, obj, ops );
		Expand_Composed_Store (MTYPE_I4, tmp, base, disp, variant, ops);
	}
	else
	{
		tmp = Build_TN_Of_Mtype(MTYPE_I8);
		Build_OP ( TOP_dmfc1, tmp, obj, ops );
		Expand_Composed_Store (MTYPE_I8, tmp, base, disp, variant, ops);
	}
	return;
  }
//	FmtAssert (FALSE, ("Expand_Composed_Store: unexpected instruction (sdr)\n"));
  TN *tmp;
  switch (mtype) {
    case MTYPE_I8:
    case MTYPE_U8:
    	Build_OP (Target_Byte_Sex == BIG_ENDIAN ? TOP_sdl : TOP_sdr,
		obj, base, disp, ops);
      // The highest byte is at effective address + 7.
      Adjust_Addr_TNs (Target_Byte_Sex == BIG_ENDIAN ? TOP_sdr : TOP_sdl,
		      &base, &disp, 7, ops);
      Build_OP (Target_Byte_Sex == BIG_ENDIAN ? TOP_sdr : TOP_sdl,
		obj, base, disp, ops);
      return;
    case MTYPE_I4:
    case MTYPE_U4:
      Build_OP (Target_Byte_Sex == BIG_ENDIAN ? TOP_swl : TOP_swr,
		obj, base, disp, ops);
      // The highest byte is at effective address + 3.
      Adjust_Addr_TNs (Target_Byte_Sex == BIG_ENDIAN ? TOP_swl : TOP_swr,
		      &base, &disp, 3, ops);
      Build_OP (Target_Byte_Sex == BIG_ENDIAN ? TOP_swr : TOP_swl,
		obj, base, disp, ops);
      return;
    case MTYPE_I2:
    case MTYPE_U2:
      if (Target_Byte_Sex == BIG_ENDIAN) {
        tmp = Build_TN_Of_Mtype(mtype);
	Build_OP (TOP_srl, tmp, obj, Gen_Literal_TN(8, 4), ops);
        Build_OP (TOP_sb, tmp, base, disp, ops);
        // The LSB is at effective address + 1.
        Adjust_Addr_TNs (TOP_lb, &base, &disp, 1, ops);
        Build_OP (TOP_sb, obj, base, disp, ops);
      } else {
        Build_OP (TOP_sb, obj, base, disp, ops);
        // The MSB is at effective address + 1.
        Adjust_Addr_TNs (TOP_lb, &base, &disp, 1, ops);
        tmp = Build_TN_Of_Mtype(mtype);
	Build_OP (TOP_srl, tmp, obj, Gen_Literal_TN(8, 4), ops);
        Build_OP (TOP_sb, tmp, base, disp, ops);
      }
      return;
    default:
      FmtAssert (FALSE, ("Expand_Composed_Load: unexpected operand size\n"));
  }
}
#endif

void
Expand_Misaligned_Store (TYPE_ID mtype, TN *obj_tn, TN *base_tn, TN *disp_tn, VARIANT variant, OPS *ops)
{
  Expand_Composed_Store (mtype, obj_tn, base_tn, disp_tn, variant, ops);
}


BOOL Is_Stack_Used(void)
{
  return FALSE;
}

#ifdef TARG_SL
/* this function is used to get vbuf variable relocation type.
 * first get vbuf type through st and then relocation type can
 * be decided through intrinsic_id and vbuf type. why we need 
 * intrinsic id is because relocation type in different intrinsic
 * function call has different relocation type such as lwc2 and vbl
 */
TN_RELOCS
Get_Internal_Buf_Reloc_Type( ST* st, INTRINSIC id) {
  TN_RELOCS reloc = TN_RELOC_NONE;
  Is_True((ST_in_vbuf(st) || ST_in_sbuf(st)), (" Passing a non-vbuf variable to Get_Vbuf_Reloc_Type"));
  if( id == INTRN_VBUF_OFFSET || id==INTRN_SBUF_OFFSET ) {
    if(ST_in_v1buf(st)) return TN_RELOC_GPREL_V1_15;
    else if(ST_in_v2buf(st)) return TN_RELOC_GPREL_V2_15;
    else if(ST_in_v4buf(st)) return TN_RELOC_GPREL_V4_15;
    else return TN_RELOC_GPREL_S;
  }
}
#endif 


#ifdef TARG_SL 
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
  BOOL is_internal_mem_ofst = FALSE)
#else 
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
#endif   
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
	tmp_tn = Build_TN_Of_Mtype(Pointer_Mtype);
	Build_OP(TOP_lui, tmp_tn, 
		 Gen_Literal_TN(base_ofst >> 16, 4), &newops);
	Build_OP(Use_32_Bit_Pointers ? TOP_addu : TOP_daddu, tmp_tn, 
		 tmp_tn, base_tn, &newops);
	base_tn = tmp_tn;
	ofst_tn = Gen_Literal_TN (base_ofst & 0xffff, 4);
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
	tmp_tn = Build_TN_Of_Mtype(Pointer_Mtype);
	Build_OP(TOP_lui, tmp_tn, 
		 Gen_Symbol_TN(sym, ofst, TN_RELOC_HIGH16), &newops);
	Build_OP(Use_32_Bit_Pointers ? TOP_addu : TOP_daddu, tmp_tn, 
		 tmp_tn, base_tn, &newops);
	base_tn = tmp_tn;
	ofst_tn = Gen_Symbol_TN(sym, ofst, TN_RELOC_LOW16);
      }
    }
  }
  else if ((ST_class(base_sym) == CLASS_BLOCK || ST_class(base_sym)==CLASS_VAR)
	   && ST_gprel(base_sym)) 
    {
      // gp-relative reference
      PU_References_GP = TRUE;

      if (ISA_LC_Value_In_Class(base_ofst, LC_simm16)) {
	base_tn = GP_TN;
	
#ifdef TARG_SL
      if (is_internal_mem_ofst && ST_in_vbuf(sym)) {
        base_tn = Zero_TN;
        ofst_tn = Gen_Symbol_TN(sym, ofst, TN_RELOC_GPREL_SL2_V15);
        Set_TN_is_v1buf_addr(tn);
      }
      else if ( is_internal_mem_ofst && ST_in_sbuf(sym) ) {
	  base_tn =  Zero_TN;
         ofst_tn = Gen_Symbol_TN(sym, ofst, TN_RELOC_GPREL_S);
     }
     else 
#endif       
	ofst_tn = Gen_Symbol_TN (sym, ofst, TN_RELOC_GPREL16);
      } 
      else {
	FmtAssert(FALSE, ("gp-relative offset doesn't fit in 16 bits"));
      }
    }
  else if (! Gen_PIC_Shared) {
    base_tn = Build_TN_Of_Mtype(Pointer_Mtype);
    Build_OP(TOP_lui, base_tn, 
	     Gen_Symbol_TN(base_sym, base_ofst, TN_RELOC_HIGH16), &newops);
    ofst_tn = Gen_Symbol_TN(base_sym, base_ofst, TN_RELOC_LOW16);
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
        is_lda = FALSE;	// to save the additional add
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
    if (TN_is_symbol(ofst_tn)) 
      Build_OP(Use_32_Bit_Pointers?TOP_addiu:TOP_daddiu, 
		      tn, base_tn, ofst_tn, &newops);
    else Expand_Add (tn, ofst_tn, base_tn, OPCODE_rtype(opcode), &newops);
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

#ifdef TARG_SL
void Exp_Lda ( 
  TYPE_ID mtype, 
  TN *tgt_tn, 
  ST *sym, 
  INT64 ofst, 
  OPERATOR call_opr,
  OPS *ops,
  BOOL is_internal_mem_ofst)
#else 
void Exp_Lda ( 
  TYPE_ID mtype, 
  TN *tgt_tn, 
  ST *sym, 
  INT64 ofst, 
  OPERATOR call_opr,
  OPS *ops)
#endif   
{
  OPCODE opcode = OPCODE_make_op(OPR_LDA, mtype, MTYPE_V);
#ifdef TARG_SL
  Exp_Ldst (opcode, tgt_tn, sym, ofst, 
	(call_opr == OPR_ICALL),
	FALSE, FALSE, ops, 0, is_internal_mem_ofst);
#else 
  Exp_Ldst (opcode, tgt_tn, sym, ofst, 
	(call_opr == OPR_ICALL),
	FALSE, FALSE, ops, 0);
#endif 	
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
  ISA_ENUM_CLASS_VALUE pfhint;
  FmtAssert(opc == TOP_UNDEFINED,
            ("Prefetch opcode should be selected in Exp_Prefetch"));
  pfhint = Pick_Prefetch_Hint(variant);
  Build_OP(TOP_pref, Gen_Enum_TN(pfhint), src1, src2, ops);
}

/* ======================================================================
 * Exp_Extract_Bits
 * ======================================================================*/
#if defined(TARG_SL)
void
Exp_Extract_Bits (TYPE_ID rtype, TYPE_ID desc, UINT bit_offset, UINT bit_size,
		  TN *tgt_tn, TN *src_tn, OPS *ops)
{
	UINT adjust_offset = (bit_offset + bit_size) - 1;
	if(MTYPE_signed(rtype)) {
    Build_OP(TOP_extrbs, tgt_tn, src_tn,  Gen_Literal_TN(adjust_offset, 4), Gen_Literal_TN(bit_size, 4), ops);		
	}
	else {
		Build_OP(TOP_extrbu, tgt_tn, src_tn,  Gen_Literal_TN(adjust_offset, 4), Gen_Literal_TN(bit_size, 4), ops);	
	}
}
#else
void
Exp_Extract_Bits (TYPE_ID rtype, TYPE_ID desc, UINT bit_offset, UINT bit_size,
		  TN *tgt_tn, TN *src_tn, OPS *ops)
{
  TN *tmp1_tn = Build_TN_Like (tgt_tn);
  UINT pos = (Target_Byte_Sex == BIG_ENDIAN || CG_emit_non_gas_syntax)
	     ? MTYPE_bit_size(desc)-bit_offset-bit_size : bit_offset;
  if (pos == 0 && bit_size <= 16 && ! MTYPE_signed(rtype)) {
    Build_OP(TOP_andi, tgt_tn, src_tn, 
	     Gen_Literal_TN((1 << bit_size)-1, 4), ops);
    return;
  }

  TOP left_shift_op = TOP_sll;
  INT left_shift_amt = MTYPE_bit_size(rtype) - pos - bit_size;
  if (MTYPE_is_size_double(rtype)) {
    left_shift_op = TOP_dsll;
    if (left_shift_amt > 31) {
      left_shift_op = TOP_dsll32;
      left_shift_amt -= 32;
    }
  }
  Build_OP(left_shift_op, tmp1_tn, src_tn, Gen_Literal_TN(left_shift_amt, 4),
	   ops);
  TOP right_shift_op = TOP_sra;
  INT right_shift_amt = MTYPE_bit_size(rtype) - bit_size;
  if (MTYPE_is_size_double(rtype)) {
    right_shift_op = TOP_dsra;
    if (right_shift_amt > 31) {
      right_shift_op = TOP_dsra32;
      right_shift_amt -= 32;
    }
  }
  if (! MTYPE_signed(rtype)) {
    if (right_shift_op == TOP_sra) 
      right_shift_op = TOP_srl;
    else if (right_shift_op == TOP_dsra)
      right_shift_op = TOP_dsrl;
    else right_shift_op = TOP_dsrl32;
  }
  Build_OP(right_shift_op, tgt_tn, tmp1_tn, Gen_Literal_TN(right_shift_amt, 4), 
	   ops);
}
#endif

/* ======================================================================
 * Exp_Deposit_Bits - deposit src2_tn into a field of src1_tn returning
 * the result in tgt_tn.
 * ======================================================================*/
#if defined(TARG_SL)
void
Exp_Deposit_Bits (TYPE_ID rtype, TYPE_ID desc, UINT bit_offset, UINT bit_size,
		  TN *tgt_tn, TN *src1_tn, TN *src2_tn, OPS *ops)
{
  FmtAssert(bit_size != 0, ("size of bit field cannot be 0"));
  UINT adjust_offset = (bit_offset + bit_size) - 1;
  if(tgt_tn!=src1_tn) {
		//Expand_Copy(tgt_tn, src1_tn, MTYPE_I4, ops);
		Expand_Add (tgt_tn, src1_tn, Zero_TN, MTYPE_I4, ops);
  }
	Build_OP(TOP_depb, tgt_tn, tgt_tn, src2_tn,  Gen_Literal_TN(adjust_offset, 4), Gen_Literal_TN(bit_size, 4), ops);

}
#else
void
Exp_Deposit_Bits (TYPE_ID rtype, TYPE_ID desc, UINT bit_offset, UINT bit_size,
		  TN *tgt_tn, TN *src1_tn, TN *src2_tn, OPS *ops)
{
  FmtAssert(bit_size != 0, ("size of bit field cannot be 0"));

  UINT targ_bit_offset = bit_offset;
  if (Target_Byte_Sex == BIG_ENDIAN) {
    targ_bit_offset = MTYPE_bit_size(desc) - bit_offset - bit_size;
  }
  TN *tmp1_tn = Build_TN_Like (src1_tn);
  TOP shift_op = TOP_srl;
  INT shift_amt = targ_bit_offset;
  if (MTYPE_is_size_double(rtype)) {
    shift_op = TOP_dsrl;
    if (shift_amt > 31) {
      shift_op = TOP_dsrl32;
      shift_amt -= 32;
    }
  }
  Build_OP(shift_op, tmp1_tn, src1_tn, Gen_Literal_TN(shift_amt,4), ops);
  Build_OP(TOP_xor, tmp1_tn, tmp1_tn, src2_tn, ops);
  shift_op = TOP_sll;
  shift_amt = MTYPE_bit_size(rtype) - bit_size;
  if (MTYPE_is_size_double(rtype)) {
    shift_op = TOP_dsll;
    if (shift_amt > 31) {
      shift_op = TOP_dsll32;
      shift_amt -= 32;
    }
  }
  Build_OP(shift_op, tmp1_tn, tmp1_tn, Gen_Literal_TN(shift_amt, 4), ops);
  shift_op = TOP_srl;
  shift_amt = MTYPE_bit_size(rtype) - bit_size - targ_bit_offset;
  if (MTYPE_is_size_double(rtype)) {
    shift_op = TOP_dsrl;
    if (shift_amt > 31) {
      shift_op = TOP_dsrl32;
      shift_amt -= 32;
    }
  }
  Build_OP(shift_op, tmp1_tn, tmp1_tn, Gen_Literal_TN(shift_amt, 4), ops);
  Build_OP(TOP_xor, tgt_tn, src1_tn, tmp1_tn, ops);
}
#endif

void 
Expand_Lda_Label (TN *dest, TN *lab, OPS *ops)
{
  Set_TN_is_reloc_got_disp(lab);
  if (Use_32_Bit_Pointers)
    Build_OP(TOP_lw, dest, GP_TN, lab, ops);
  else
    Build_OP(TOP_ld, dest, GP_TN, lab, ops);
}
