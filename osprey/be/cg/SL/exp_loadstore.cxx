/********************************************************************\
|*                                                                  *|   
|*  Copyright (c) 2006 by SimpLight Nanoelectronics.                *|
|*  All rights reserved                                             *|
|*                                                                  *|
|*  This program is free software; you can redistribute it and/or   *|
|*  modify it under the terms of the GNU General Public License as  *|
|*  published by the Free Software Foundation; either version 2,    *|
|*  or (at your option) any later version.                          *|
|*                                                                  *|
\********************************************************************/

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

extern TN *Get_64Bit_High_TN(TN *low, TYPE_ID type, OPS * ops);
extern TN *Get_TN_Pair(TN *key);
extern void Add_TN_Pair(TN *key, TN *pair);

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
  case MTYPE_I4: 
  case MTYPE_U4: 
  case MTYPE_I8: 
  case MTYPE_U8:    
  case MTYPE_F4: 
  case MTYPE_F8: return TOP_lw; 
    
  case MTYPE_V:
    if (rtype != MTYPE_V)
      // use rtype to pick load (e.g. if lda)
      return Pick_Load_Instruction(rtype,rtype);
    // else fallthru
  default:  
    FmtAssert(FALSE, ("Pick_Load_Instruction: Unexpected type %d", desc));
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

static TN *
Get_64Bit_High_Addr(TN *base, TN ** ofst, TN ** ofst_h, OPS * ops)
{
  if (TN_is_constant(*ofst) && ISA_LC_Value_In_Class(TN_value(*ofst) + 4, LC_simm16)) {
    *ofst_h = TN_is_symbol(*ofst) ? 
      Gen_Symbol_TN(TN_var(*ofst), TN_offset(*ofst)+4, TN_relocs(*ofst)) : 
      Gen_Literal_TN(TN_value(*ofst)+4, TN_size(*ofst));

    return base;
  } else {  // offset > LC_simm16   
    TN *new_base = Build_TN_Of_Mtype(MTYPE_U4);
    Expand_Add(new_base, base, *ofst, MTYPE_U4, ops);
    *ofst   = Gen_Literal_TN(0, 4);
    *ofst_h = Gen_Literal_TN(4, 4);

    return new_base;	
  }
}

static void 
Expand_64Bit_Load(OPCODE opcode, TN *result, TN *base, TN *ofst, OPS* ops)
{
  TYPE_ID desc      = OPCODE_desc(opcode);
  TYPE_ID rtype     = OPCODE_rtype(opcode);

  FmtAssert(MTYPE_byte_size(rtype) == 8, ("Expand_64Bit_Load: NYI"));

  TN *result_h = Get_TN_Pair(result);

  if (MTYPE_is_longlong(rtype)) {
    TYPE_ID new_mtype = (rtype == MTYPE_I8 ? MTYPE_I4 : MTYPE_U4);
    TYPE_ID new_desc  = MTYPE_is_longlong(desc) ? new_mtype : desc;
    TOP new_top       = Pick_Load_Instruction(new_mtype, new_desc);

    if (MTYPE_is_longlong(desc)){ //I8<--I8
      TN *ofst_h = NULL;
      TN *new_base = Get_64Bit_High_Addr(base, &ofst, &ofst_h, ops);
      Build_OP(new_top, result, new_base, ofst,   ops);    

      // GRA spill/spilt may duplicate TN from OPC_I8I8LDID, but in this case 
      // only 1 TN is duplicated from Dup_TN, the paired TN is duplicated from
      // another site. Thus we treate OPC_I8I8LDID like OPC_I4I4LDID
      if (result_h != NULL) {
        Build_OP(new_top, result_h, new_base, ofst_h, ops);        
      }
    } else {  // I8 <--I4   
      Build_OP(new_top, result, base, ofst, ops);

      if (result_h != NULL) {
        if (MTYPE_signed(desc)) {
          Build_OP(TOP_sra, result_h, result, Gen_Literal_TN(31, 4), ops);
        } else {		
          Build_OP(TOP_or, result_h, Zero_TN, Zero_TN, ops);
        }    
      }
    }
  }else if (MTYPE_is_double(desc)) {
    TN *ofst_h = NULL;
    TN *new_base = Get_64Bit_High_Addr(base, &ofst, &ofst_h, ops);

    Build_OP(TOP_lw, result,   new_base, ofst,   ops);

    // GRA spill/spilt may duplicate TN from OPC_F8F8LDID, but in this case 
    // only 1 TN is duplicated from Dup_TN, the paired TN is duplicated from
    // another site. Thus we treate OPC_F8F8LDID like OPC_I4I4LDID
    if (result_h != NULL) {  	
      Build_OP(TOP_lw, result_h, new_base, ofst_h, ops);   
    }
  }
}

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
      Build_OP (top, result, tmp_base, Gen_Literal_TN (0, 4), ops);
      if (MTYPE_is_longlong(OPCODE_rtype(opcode)))
        Build_OP (top, Get_TN_Pair(result), tmp_base, Gen_Literal_TN (4, 4), ops);
      return;
    }
  }
#if defined(TARG_SL)
  if(CG_sl2 && CG_SL2_enable_v1buf_expansion && 
    Expand_V1buf_Ldst(top, result, base, ofst, TRUE, ops)) return; 
#endif 
  if (MTYPE_byte_size(OPCODE_rtype(opcode)) == 8) {
    Expand_64Bit_Load(opcode, result, base, ofst, ops);
  } else {
    Build_OP (top, result, base, ofst, ops);
  }
}

static TOP
Pick_Store_Instruction (TYPE_ID mtype)
{
  switch (mtype) {
    case MTYPE_I1:  case MTYPE_U1:  return TOP_sb;
    case MTYPE_I2:  case MTYPE_U2:  return TOP_sh;
    case MTYPE_I4:  case MTYPE_U4:  return TOP_sw;
    case MTYPE_I8:  case MTYPE_U8:  return TOP_sw;
    case MTYPE_F4:  return TOP_sw;
    case MTYPE_F8:  return TOP_sw;
    default:  
              FmtAssert(FALSE, ("NYI: Pick_Store_Instruction mtype"));
              return TOP_UNDEFINED;
  }
}

static void 
Expand_64Bit_Store(TYPE_ID mtype, TN *src, TN *base, TN *ofst, OPS *ops)
{ 
  TN *src_h   = Get_TN_Pair(src);
  TN *ofst_h  = NULL;  
  TN *new_base = Get_64Bit_High_Addr(base, &ofst, &ofst_h, ops);

  if (MTYPE_is_longlong(mtype)) {
    TYPE_ID new_mtype = (mtype == MTYPE_I8 ? MTYPE_I4 : MTYPE_U4);
    TOP new_top = Pick_Store_Instruction(new_mtype);

    if (src_h == NULL){        
      src_h = Get_64Bit_High_TN(src, mtype, ops);
    }

    Build_OP(new_top, src,   new_base, ofst,   ops);
    Build_OP(new_top, src_h, new_base, ofst_h, ops);
  } else if (MTYPE_is_double(mtype)) {

    Build_OP(TOP_sw, src,   new_base, ofst,   ops);

    // GRA spill/spilt may duplicate TN from OPC_F8STID, but in this case 
    // only 1 TN is duplicated from Dup_TN, the paired TN is duplicated from
    // another site. Thus we treate OPC_F8STID like OPC_I4STID
    if (src_h != NULL) {
      Build_OP(TOP_sw, src_h, new_base, ofst_h, ops);
    }
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

  if(MTYPE_byte_size(mtype) == 8) {
    Expand_64Bit_Store(mtype, src, base, ofst, ops);
    return;
  }
  if (!TN_has_value(ofst) || ISA_LC_Value_In_Class(TN_value(ofst), LC_simm16))
    Build_OP (top, src, base, ofst, ops);
  else {
    TN *tn_ofst, *tn_addr;
    tn_addr = tn_ofst = Build_TN_Of_Mtype(Pointer_Mtype);
    TN *tn_ofst_low = Build_TN_Of_Mtype(Pointer_Mtype);
    INT32 ofst_up = TN_value(ofst) >> 16;
    INT32 ofst_low = TN_value(ofst) & 0xffff;
    if (ofst_up < 0) {
       if (Use_32_Bit_Pointers) 
         ofst_up &= 0xffff;
    }

    Build_OP(TOP_lui, tn_ofst, Gen_Literal_TN(ofst_up, 4), ops);
    Build_OP(TOP_ori, tn_ofst_low, Zero_TN, Gen_Literal_TN(ofst_low, 4), ops);
    Build_OP(Use_32_Bit_Pointers ? TOP_add : TOP_dadd, tn_ofst, tn_ofst, tn_ofst_low, ops);
    Build_OP(Use_32_Bit_Pointers ? TOP_addu : TOP_daddu, tn_addr, tn_ofst, base, ops); 
    Build_OP(top, src, tn_addr, Gen_Literal_TN(0,4), ops);
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
    DevWarn ("Float type in Expand_Composed_Load");
    if (rtype == MTYPE_F4) {
      Expand_Composed_Load ( OPC_I4I4LDID, result, base, disp, variant, ops);
    } else {
      Expand_Composed_Load ( OPC_I8I8LDID, result, base, disp, variant, ops);
    }
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
        Expand_Add(newbase, base, disp, MTYPE_I4, ops);	

        Build_OP(TOP_lbu,  result, base, disp, ops);		 

        Build_OP(TOP_lbu,  load1, newbase, Gen_Literal_TN(1, 4), ops);	
        Build_OP(TOP_depb, result, load1, Gen_Literal_TN(15, 4), Gen_Literal_TN(8, 4), result, ops);

        Build_OP(TOP_lbu,  load2, newbase, Gen_Literal_TN(2, 4), ops);		 
        Build_OP(TOP_depb, result, load2, Gen_Literal_TN(23, 4), Gen_Literal_TN(8, 4), result, ops);     

        Build_OP(TOP_lbu,  load3, newbase, Gen_Literal_TN(3, 4), ops);		 
        Build_OP(TOP_depb, result, load3, Gen_Literal_TN(31, 4), Gen_Literal_TN(8, 4), result, ops);

        result = Get_TN_Pair(result);
        FmtAssert(result != NULL, ("result tn in Expand_Composed_Load not setup\n"));

        Build_OP(TOP_lbu,  result, newbase, Gen_Literal_TN(4, 4), ops);		 

        Build_OP(TOP_lbu,  load1, newbase, Gen_Literal_TN(5, 4), ops);	
        Build_OP(TOP_depb, result, load1, Gen_Literal_TN(15, 4), Gen_Literal_TN(8, 4), result, ops);

        Build_OP(TOP_lbu,  load2, newbase, Gen_Literal_TN(6, 4), ops);		 
        Build_OP(TOP_depb, result, load2, Gen_Literal_TN(23, 4), Gen_Literal_TN(8, 4), result, ops);     

        Build_OP(TOP_lbu,  load3, newbase, Gen_Literal_TN(7, 4), ops);		 
        Build_OP(TOP_depb, result, load3, Gen_Literal_TN(31, 4), Gen_Literal_TN(8, 4), result, ops);
        return;
      case MTYPE_I4:
      case MTYPE_U4:
        Build_OP (TOP_lbu, result, base, disp, ops);		 

        Expand_Add (newbase, base, disp, MTYPE_I4, ops);	
        Build_OP (TOP_lbu, load1, newbase, Gen_Literal_TN(1, 4), ops);	
        Build_OP(TOP_depb, result, load1,  Gen_Literal_TN(15, 4), Gen_Literal_TN(8, 4), result, ops);

        Build_OP (TOP_lbu, load2, newbase, Gen_Literal_TN(2, 4), ops);		 
        Build_OP(TOP_depb, result, load2,  Gen_Literal_TN(23, 4), Gen_Literal_TN(8, 4), result, ops);     

        Build_OP (TOP_lbu, load3, newbase, Gen_Literal_TN(3, 4), ops);		 
        Build_OP(TOP_depb, result, load3,  Gen_Literal_TN(31, 4), Gen_Literal_TN(8, 4), result, ops);

        //Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
        return;
      case MTYPE_I2:
      case MTYPE_U2:
        Build_OP (TOP_lbu, result, base, disp, ops);		 

        Expand_Add (newbase, base, disp, MTYPE_I4, ops);	
        Build_OP (TOP_lbu, load1, newbase, Gen_Literal_TN(1, 4), ops);	
        Build_OP(TOP_depb, result, load1,  Gen_Literal_TN(15, 4), Gen_Literal_TN(8, 4), result, ops);	    
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
    TN *tmp;
    DevWarn ("Float type in Expand_Composed_Load");
    if (mtype == MTYPE_F4)
    {
      Expand_Composed_Store (MTYPE_I4, obj, base, disp, variant, ops);
    }
    else
    {
      Expand_Composed_Store (MTYPE_I8, obj, base, disp, variant, ops);
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

        Expand_Add (newbase, base, disp, MTYPE_I4, ops);	

        Build_OP(TOP_sb, obj, base, disp, ops);		 	

        Build_OP(TOP_extrbu, store1, obj, Gen_Literal_TN(15, 4), Gen_Literal_TN(8, 4), ops);
        Build_OP(TOP_sb, store1, newbase, Gen_Literal_TN(1, 4), ops);	

        Build_OP(TOP_extrbu, store2, obj, Gen_Literal_TN(23, 4), Gen_Literal_TN(8, 4), ops);
        Build_OP(TOP_sb, store2, newbase, Gen_Literal_TN(2, 4), ops);	

        Build_OP(TOP_extrbu, store3, obj, Gen_Literal_TN(31, 4), Gen_Literal_TN(8, 4), ops);
        Build_OP(TOP_sb, store3, newbase, Gen_Literal_TN(3, 4), ops);			      

        obj = Get_TN_Pair(obj);
        FmtAssert(obj != NULL, ("source tn pair in Expand_Composed_Store not setup\n"));

        Build_OP(TOP_sb, obj, newbase, Gen_Literal_TN(4, 4), ops);		 	

        Build_OP(TOP_extrbu, store1, obj, Gen_Literal_TN(15, 4), Gen_Literal_TN(8, 4), ops);
        Build_OP(TOP_sb, store1, newbase, Gen_Literal_TN(5, 4), ops);	

        Build_OP(TOP_extrbu, store2, obj, Gen_Literal_TN(23, 4), Gen_Literal_TN(8, 4), ops);
        Build_OP(TOP_sb, store2, newbase, Gen_Literal_TN(6, 4), ops);	

        Build_OP(TOP_extrbu, store3, obj, Gen_Literal_TN(31, 4), Gen_Literal_TN(8, 4), ops);
        Build_OP(TOP_sb, store3, newbase, Gen_Literal_TN(7, 4), ops);			      

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

        // Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
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
	FmtAssert (FALSE, ("Expand_Composed_Store: unexpected instruction (sdr)\n"));
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
      Adjust_Addr_TNs (Target_Byte_Sex == BIG_ENDIAN ? TOP_swr : TOP_swl,
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

void Exp_Prefetch (TOP opc, TN *src1, TN *src2, VARIANT variant, OPS* ops)
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
void Build_Extrb_OP(TYPE_ID desc, TYPE_ID rtype, TN *tgt_tn, TN *src_tn, 
    UINT pos, UINT width, OPS *ops)
{
  if (width == 32) {
    FmtAssert(pos == 31, ("Build_Extrb_OP: pos is out of range\n"));
    Expand_Copy(tgt_tn, src_tn, MTYPE_U4, ops);
  } else {    
    if (MTYPE_signed(desc)) {
      Build_OP(TOP_extrbs, tgt_tn, src_tn, Gen_Literal_TN(pos, 4), 
          Gen_Literal_TN(width, 4), ops);     
    } else {
      Build_OP(TOP_extrbu, tgt_tn, src_tn, Gen_Literal_TN(pos, 4), 
          Gen_Literal_TN(width, 4), ops); 
    }
  }
}

void Build_Extrbu_OP(TYPE_ID desc, TYPE_ID rtype, TN *tgt_tn, TN *src_tn, 
    UINT pos, UINT width, OPS *ops)
{
  if (width == 32) {
    FmtAssert(pos == 31, ("Build_Extrbu_OP: pos is  out of range\n"));
    Expand_Copy(tgt_tn, src_tn, MTYPE_U4, ops);
  } else {    
    Build_OP(TOP_extrbu, tgt_tn, src_tn, Gen_Literal_TN(pos, 4), 
        Gen_Literal_TN(width, 4), ops); 
  } 
}
void Build_Depb_OP(TYPE_ID desc, TYPE_ID rtype, TN *tgt_tn, TN *src_tn, 
    UINT pos, UINT width, OPS *ops)
{
  if (width == 32) {
    FmtAssert(pos == 31, ("Build_Extrb_OP: pos is out of range\n"));
    Expand_Copy(tgt_tn, src_tn, MTYPE_U4, ops);
  } else {
    Build_OP(TOP_depb, tgt_tn, src_tn, Gen_Literal_TN(pos, 4), 
        Gen_Literal_TN(width, 4), tgt_tn, ops);
  }
}

extern TN *
Get_64Bit_High_TN(TN *low, TYPE_ID type, OPS * ops);

void
Exp_Extract_Bits (TYPE_ID rtype, TYPE_ID desc, UINT bit_offset, UINT bit_size,
		  TN *tgt_tn, TN *src_tn, OPS *ops)
{
  FmtAssert(bit_size != 0, ("Exp_Extract_Bits: size of bit field cannot be 0"));
  FmtAssert((bit_offset + bit_size) <= MTYPE_bit_size(desc), 
      ("Exp_Extract_Bits : Not valid bit_offset %d or bit_size %d", bit_offset, bit_size));

  if (!MTYPE_is_longlong(rtype)) {
    if (!MTYPE_is_longlong(desc)) { //I4 <-I4bit
      UINT pos = (bit_offset + bit_size) - 1;
      UINT width = bit_size;      
      Build_Extrb_OP(desc, rtype, tgt_tn, src_tn, pos, width, ops);
    } else { // I4  <- I8 bit
      TN *src_high = Get_TN_Pair(src_tn);
      FmtAssert(src_high, ("Exp_Extract_Bits : I4 <- I8 bit source tn pair not setup"));

      if (bit_offset < 32) {
        if (bit_offset + bit_size <= 32) { // 20-30 (20, 11) 
          UINT pos = bit_offset + bit_size - 1;
          UINT width = bit_size;					
          Build_Extrb_OP(desc, rtype, tgt_tn, src_tn, pos, width, ops);     
        } else { // 20-35 (20, 16)<---(20-31)(0-11), (32-35)(0-3)(12, 15)
          UINT low_pos = 31;
          UINT low_width = 32 - bit_offset;            
          Build_Extrb_OP(desc, rtype, tgt_tn, src_tn, low_pos, low_width, ops);

          UINT high_pos = bit_size-1;
          UINT high_width = bit_size + bit_offset - 32;
          Build_Depb_OP(desc, rtype, tgt_tn, src_high, high_pos, high_width, ops);      
        }
      } else { // bit_offset >= 32
        UINT pos   = bit_offset + bit_size - 33;
        UINT width = bit_size;
        Build_Extrb_OP(desc, rtype, tgt_tn, src_high, pos, width, ops);
      }
    }      
  } else { 
    TN *tgt_high = Get_TN_Pair(tgt_tn);
    FmtAssert(tgt_high != NULL, ("Exp_Extract_Bits : to I8 result tn pair not setup"));

    if (!MTYPE_is_longlong(desc)) { // I8 <- I4 bit
      UINT pos   = bit_offset + bit_size - 1;
      UINT width = bit_size;

      Build_Extrb_OP(desc, rtype, tgt_tn, src_tn, pos, width, ops);
      tgt_high = Get_64Bit_High_TN(tgt_tn, desc, ops);  
    } else { // I8 <- I8 bit   

      TN *src_high = Get_TN_Pair(src_tn);
      FmtAssert(src_high != NULL, ("Exp_Extract_Bits : I8 <- I8 source tn pair not setup"));

      if (bit_offset < 32){
        if (bit_offset + bit_size <= 32) { // example: 10~20
          UINT pos   = bit_offset+bit_size - 1;
          UINT width = bit_size;

          Build_Extrb_OP(desc, rtype, tgt_tn, src_tn, pos, width, ops);
          tgt_high = Get_64Bit_High_TN(tgt_tn, desc, ops);              
        } else {
          if (bit_size <= 32) { // example: 10~ 38(10, 29) , 10-31(0-21), 32-38(22-28)
            UINT low_pos   = 31;
            UINT low_width = 32 - bit_offset;
            Build_Extrb_OP(desc, rtype, tgt_tn, src_tn, low_pos, low_width, ops);

            UINT high_pos   = bit_size-1;
            UINT high_width = bit_size + bit_offset - 32;
            Build_Depb_OP(desc, rtype, tgt_tn, src_high, high_pos, high_width, ops);

            tgt_high = Get_64Bit_High_TN(tgt_tn, desc, ops);        
          } else { //  example: 20~54(20, 35) 
            // 3 part, 20-31(0-11), 32-51(0-19)(12-31), 52-54(20-22)(0-2)            
            UINT pos_1   = 31;
            UINT width_1 = 32 - bit_offset;
            Build_Extrbu_OP(desc, rtype, tgt_tn, src_tn, pos_1, width_1, ops);

            if (bit_offset != 0) {
              UINT pos_2   = 31;
              UINT width_2 = bit_offset;
              Build_Depb_OP(desc, rtype, tgt_tn, src_high, pos_2, width_2, ops);              
            }

            UINT pos_3   = bit_size + bit_offset - 32 - 1;
            UINT width_3 = bit_size - 32;
            Build_Extrb_OP(desc, rtype, tgt_high, src_high, pos_3, width_3, ops);
          }
        }
      } else {  // bit_offset >= 32
        // example: 35-55(35, 21)(3,23) 
        UINT pos   = bit_offset + bit_size -33;
        UINT width = bit_size;

        Build_Extrb_OP(desc, rtype, tgt_tn, src_high, pos, width, ops);
        tgt_high = Get_64Bit_High_TN(tgt_tn, desc, ops);
      }
    }    
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
  FmtAssert(bit_size != 0, ("Exp_Deposit_Bits: size of bit field cannot be 0"));
  FmtAssert(bit_offset + bit_size <= MTYPE_bit_size(rtype), ("Exp_Deposit_Bits : bit out of range"));

  if (tgt_tn!=src1_tn) {
    Expand_Add(tgt_tn, src1_tn, Zero_TN, rtype, ops);
  }

  if (!MTYPE_is_longlong(rtype)) {  // I4 bit <- I4  || I4 bit <- I8 
    // example : [4, 20] 
    UINT pos   = bit_offset + bit_size - 1;
    UINT width = bit_size;
    Build_Depb_OP(desc, rtype, tgt_tn, src2_tn, pos, width, ops);
  } else {
    TN *tgt_high  = Get_TN_Pair(tgt_tn);
    TN *src1_high = Get_TN_Pair(src1_tn);

    FmtAssert(tgt_high != NULL, ("Exp_Deposit_Bits : I8 <- I4 source1 tn pair not setup"));

    if (src1_high != NULL)
      Expand_Add(tgt_high, src1_high, Zero_TN, MTYPE_U4, ops);

    if (!MTYPE_is_longlong(desc)) { // I8 bit <- I4          
      if (bit_offset < 32) {        
        if (bit_offset + bit_size <= 32) { // example: [10-30](10,21)
          UINT pos   = bit_offset + bit_size - 1;
          UINT width = bit_size;

          Build_Depb_OP(desc, rtype, tgt_tn, src2_tn, pos, width, ops);

        } else { // bit_offset < 32, bit_offset + bit_size > 32
          // example: [10,38](10, 29) <---[10,31](0,21), [32-38](22-28) 
          UINT low_pos    = 31;
          UINT low_width  = 32 - bit_offset;
          Build_Depb_OP(desc, rtype, tgt_tn, src2_tn, low_pos, low_width, ops);

          UINT high_pos   = bit_offset + bit_size - 33;
          UINT high_width = bit_offset + bit_size - 32;
          TN *temp_tn    = Build_TN_Like(src2_tn);          
          Build_OP(TOP_srl, temp_tn, src2_tn, Gen_Literal_TN(low_width, 4), ops);

          Build_Depb_OP(desc, rtype, tgt_high, temp_tn, high_pos, high_width, ops);
        }      
      } else {  // bit_offset > 32      
        // example: [33, 35(1,3)](33,3)        
        UINT pos   = bit_offset + bit_size - 33;
        UINT width = bit_size;
        Build_Depb_OP(desc, rtype, tgt_high, src2_tn, pos, width, ops);
      }
    } else {  // I8bit <- I8
      TN *src2_high = Get_TN_Pair(src2_tn);
      if (bit_offset < 32) {
        if (bit_offset + bit_size <= 32) {
          UINT pos   = bit_offset + bit_size - 1;
          UINT width = bit_size;
          Build_Depb_OP(desc, rtype, tgt_tn, src2_tn, pos, width, ops);
        } else { 
          if (bit_size <= 32) { // example: [10,38](10, 29) <---[10,31](0,21), [32-38](22-28)
            UINT low_pos   = 31;
            UINT low_width = 32 - bit_offset;

            Build_Depb_OP(desc, rtype, tgt_tn, src2_tn, low_pos, low_width, ops);

            UINT high_pos   = bit_offset + bit_size - 33;
            UINT high_width = bit_offset + bit_size - 32;
            TN *temp_tn    = Build_TN_Like(src2_tn);

            Build_OP(TOP_srl, temp_tn, src2_tn, Gen_Literal_TN(low_width, 4), ops);
            Build_Depb_OP(desc, rtype, tgt_high, temp_tn, high_pos, high_width, ops);

          } else { //bit_offset < 32, bit_size >32
            // example: [20-55](20,36) <--[0-35] 
            //  l[20-31]<--l[0-11]; h[32-51](0-19)<--l[12-31]; h[52-55](20-23)<--h[0-3]
            FmtAssert(src2_high != NULL, ("Exp_Deposit_Bits : I8 <- I8 source2 tn pair not setup"));

            UINT pos_1   = 31;
            UINT width_1 = 32-bit_offset;

            Build_Depb_OP(desc, rtype, tgt_tn, src2_tn, pos_1, width_1, ops);

            if (bit_offset != 0) {
              UINT pos_2   = bit_offset-1;
              UINT width_2 = bit_offset;
              TN *temp_tn = Build_TN_Like(src2_tn);

              Build_OP(TOP_srl, temp_tn, src2_tn, Gen_Literal_TN(width_1, 4), ops);// right shift                 
              Build_Depb_OP(desc, rtype, tgt_high, temp_tn, pos_2, width_2, ops);
            }

            UINT pos_3 = bit_offset + bit_size -33;
            UINT width_3 = bit_size - 32;

            Build_Depb_OP(desc, rtype, tgt_high, src2_high, pos_3, width_3, ops);
          }
        }
      } else {  // bit_offset >= 32      
        // example [35, 39](35,5)[0,7] <-- 
        UINT pos   = bit_offset + bit_size - 33;
        UINT width = bit_size;

        Build_Depb_OP(desc, rtype, tgt_high, src2_tn, pos, width, ops);
      }
    }
  }
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
