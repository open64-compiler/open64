/*
 * Copyright (C) 2008-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2007, 2008 PathScale, LLC.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2006, 2007.  QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


/* =======================================================================
 * =======================================================================
 *
 *  Module: ebo_special.cxx
 *  $Revision: 1.245 $
 *  $Date: 05/11/30 16:23:38-08:00 $
 *  $Author: tkong@hyalite.keyresearch $
 *  $Source: be/cg/x8664/SCCS/s.ebo_special.cxx $
 *
 *  Revision comments:
 *
 *  17-June-1998 - Initial version
 *
 *  Description:
 *  ============
 *
 *  EBO special case optimizations.
 *
 * =======================================================================
 * =======================================================================
 */

#ifdef USE_PCH
#include "cg_pch.h"
#endif // USE_PCH
#pragma hdrstop

#ifdef _KEEP_RCS_ID
static const char source_file[] = __FILE__;
#endif /* _KEEP_RCS_ID */

#include <stdarg.h>
#include "defs.h"
#include "config_targ_opt.h"
#include "errors.h"
#include "mempool.h"
#include "tracing.h"
#include "timing.h"
#include "cgir.h"
#include "tn_map.h"
#include "cg_loop.h"
#include "cg.h"
#include "cgexp.h"
#include "register.h"
#include "cg_region.h"
#include "wn.h"
#include "region_util.h"
#include "op_list.h"
#include "cgprep.h"
#include "gtn_universe.h"
#include "gtn_set.h"
#include "cg_db_op.h"
#include "whirl2ops.h"
#include "cgtarget.h"
#include "gra_live.h"
#include "reg_live.h"
#include "cflow.h"
#include "cg_spill.h"
#include "cgexp_internals.h"
#include "data_layout.h"
#include "stblock.h"
#include "cxx_hash.h"
#include "op.h"
#ifdef TARG_X8664
#include "opt_alias_interface.h"
#include "opt_alias_mgr.h"
#include "lra.h"
#endif

#include "ebo.h"
#include "ebo_info.h"
#include "ebo_special.h"
#include "ebo_util.h"
#include "cgtarget.h"
#include "dominate.h"

#include "config_lno.h"

#include <iostream>
#include <string>
#include <sstream>
#include <set>
#include <vector>
#include <list>
#include <deque>
#include <map>

extern BOOL TN_live_out_of( TN*, BB* );
extern void Set_flags_strcmp_expand();

/* Define a macro to strip off any bits outside of the left most 4 bytes. */
#define TRUNC_32(val) (val & 0x00000000ffffffffll)

/* Define a macro to sign-extend the least signficant 32 bits */
#define SEXT_32(val) (((INT64)(val) << 32) >> 32)

/* ===================================================================== */

typedef HASH_TABLE<ST_IDX, INITV_IDX> ST_TO_INITV_MAP;
static ST_TO_INITV_MAP *st_initv_map = NULL;
static BOOL st_initv_map_inited = FALSE;
static GTN_SET *work_gtn_set = NULL;
static BS *work_defined_set = NULL;
static MEM_POOL *work_pool = NULL;
static INT32 fixed_branch_cost, taken_branch_cost;

static BOOL Convert_Imm_And( OP* op, TN *tnr, TN *tn, INT64 imm_val, EBO_TN_INFO *tninfo );
static BOOL Convert_Imm_Mul( OP* op, TN *tnr, TN *tn, INT64 imm_val, EBO_TN_INFO *tninfo );
static BOOL Convert_Imm_Or( OP* op, TN *tnr, TN *tn, INT64 imm_val, EBO_TN_INFO *tninfo );
static BOOL Convert_Imm_Add( OP* op, TN *tnr, TN *tn, INT64 imm_val, EBO_TN_INFO *tninfo, BOOL simplify_iadd );
static BOOL Convert_Imm_Xor( OP* op, TN *tnr, TN *tn, INT64 imm_val, EBO_TN_INFO *tninfo );
static BOOL Convert_Imm_Cmp( OP* op, TN *tnr, TN *tn, INT64 imm_val, EBO_TN_INFO *tninfo );

static BB_MAP dfo_map;

enum ADDR_MODE { REG_MODE = 0,     /* reg                      */
		 BASE_MODE,	   /* offset(base)             */
		 BASE_INDEX_MODE,  /* offset(base,index,scale) */
		 INDEX_MODE,       /* offset(,index,scale)     */
		 N32_MODE,         /* offset                   */
		 UNDEFINED_MODE	   /* end marker               */
	       };

static void Init_Addr_Modes();
static TOP Get_Top_For_Addr_Mode (TOP, ADDR_MODE);

static OP *Compose_Mem_Op_And_Copy_Info (OP *op, TN *index, TN *offset,
					 TN *scale, TN *base,
					 EBO_TN_INFO **load_actual_tninfo);
void expand_strcmp_bb(BB * call_bb);
/* Initialize and finalize ebo special routines. */
void
EBO_Special_Start (MEM_POOL *pool)
{

  Init_Addr_Modes();
}

void
EBO_Special_Finish (void)
{
  st_initv_map = NULL;
  st_initv_map_inited = FALSE;
  work_gtn_set = NULL;
  work_defined_set = NULL;
  work_pool = NULL;
}

// Return TRUE if a predessor OP's operand is available at OP.  The predecessor
// defines the TN identified by TNINFO.  PRED_OPND_IDX tells which predecessor
// operand.
static BOOL
Pred_Opnd_Avail (OP *op, EBO_TN_INFO *tninfo, int pred_opnd_idx,
		 EBO_TN_INFO **pred_tninfo_p = NULL)
{
  EBO_OP_INFO *pred_opinfo;
  EBO_TN_INFO *pred_tninfo;

  pred_opinfo = locate_opinfo_entry(tninfo);
  if (pred_opinfo == NULL)
    return FALSE;
  pred_tninfo = pred_opinfo->actual_opnd[pred_opnd_idx];
  if (pred_tninfo_p != NULL)
    *pred_tninfo_p = pred_tninfo;
  return EBO_tn_available(OP_bb(op), pred_tninfo);
}


/*
 * Identify OP's that contain a constant and operate in a way that
 * will allow the constant to be added into an offset field of
 * a load or store instruction.
 */
BOOL EBO_Can_Merge_Into_Offset (OP *op)
{
#if Is_True_On
  if (!(EBO_Opt_Mask & EBO_CAN_MERGE_INTO_OFFSET)) return FALSE;
#endif
  TOP top = OP_code(op);
  
  if( !OP_iadd( op ) && !OP_isub( op ) )
    return FALSE;

  if ((op == BB_entry_sp_adj_op(OP_bb(op))) ||
      (op == BB_exit_sp_adj_op(OP_bb(op))))
    return FALSE;

  /* opnd0 will serve as a base/index register. Check its size first,
     because a negative 32-value value could be a big positive value
     in a 64-bit reg; but it is OK if it is a positive 32-bit value.
   */
  if( MTYPE_is_size_double( Pointer_Mtype ) &&
      TN_size( OP_opnd(op,0) ) == 4 )
    return FALSE;

  TN *tn = OP_opnd(op,1);
  if (TN_Is_Constant(tn))
    return TRUE;

  return FALSE;
}


static
void
EBO_Set_OP_omega (OP *op, ...)
{
  INT opnds = OP_opnds(op);
  INT i;
  va_list tninfos;

  va_start(tninfos, op);
  CG_LOOP_Init_Op(op);
  for (i = 0; i < opnds; i++) {
    EBO_TN_INFO *tninfo = va_arg(tninfos, EBO_TN_INFO *);
    Set_OP_omega (op, i, ((tninfo != NULL) ? tninfo->omega : 0));
  }

  va_end(tninfos);
  return;
}



static
void
EBO_Copy_OP_omega (OP *new_op, OP *old_op)
{
  INT opnds = OP_opnds(new_op);
  INT i;

  CG_LOOP_Init_Op(new_op);
  for (i = 0; i < opnds; i++) {
    Set_OP_omega (new_op, i, OP_omega(old_op,i));
  }

  return;
}


static
void
EBO_OPS_omega (OPS *ops)
{
  OP *next_op = OPS_first(ops);
  while (next_op != NULL) {
    INT opnds = OP_opnds(next_op);
    INT i;

    CG_LOOP_Init_Op(next_op);
    for (i = 0; i < opnds; i++) {
      Set_OP_omega (next_op, i, 0);
    }

    next_op = OP_next(next_op);
  }

  return;
}


BOOL Combine_L1_L2_Prefetches( OP* op, TN** opnd_tn, EBO_TN_INFO** opnd_tninfo )
{
#if Is_True_On
  if (!(EBO_Opt_Mask & EBO_COMBINE_L1_L2_PREFETCH)) return FALSE;
#endif
  if( !OP_prefetch( op ) )
    return FALSE;

  /* Determine the proper hash value. */
  const int hash_value = EBO_hash_op( op, opnd_tninfo );

  if( EBO_Trace_Hash_Search ){
    #pragma mips_frequency_hint NEVER
    fprintf( TFile,"%sLook for redundant prefetch ops in hash chain %d for\n\t",
	     EBO_trace_pfx,hash_value );
    Print_OP_No_SrcLine(op);
  }

  BOOL replaced = FALSE;
  EBO_OP_INFO* opinfo = EBO_opinfo_table[hash_value];

  while( opinfo != NULL ){

    OP* pred_op = opinfo->in_op;
    if( pred_op == NULL       ||
	!OP_prefetch(pred_op) ||
	OP_opnds(pred_op) != OP_opnds(op) ){
      opinfo = opinfo->same;
      continue;
    }

    // Compare the addresses
    int i = 1;
    for( ; i < OP_opnds(op); i++ ){
      if( OP_opnd( op, i ) != OP_opnd( pred_op, i ) )
	break;
    }
    
    if( i < OP_opnds(op) ){
      opinfo = opinfo->same;
      continue;
    }

    // Compare the hint field
    const ISA_ENUM_CLASS_VALUE pfhint0 = TN_enum( OP_opnd(op,0) );
    const ISA_ENUM_CLASS_VALUE pfhint1 = TN_enum( OP_opnd(pred_op,0) );

    // Remove an identical prefetch op.
    if( pfhint0 == pfhint1 ){
      replaced = TRUE;
      break;
    }

    // Remove a dominated prefetch.
    if( pfhint0 == ECV_pfhint_L1_L2_load &&
	( pfhint1 == ECV_pfhint_L1_load ||
	  pfhint1 == ECV_pfhint_L2_load ) ){
      // pred_op is dominated by the current op.
      remove_op( opinfo );
      OP_Change_To_Noop( opinfo->in_op );
      opinfo->in_op = NULL;
      opinfo->in_bb = NULL;

      break;
    }

    if( pfhint1 == ECV_pfhint_L1_L2_load &&
	( pfhint0 == ECV_pfhint_L1_load ||
	  pfhint0 == ECV_pfhint_L2_load ) ){
      replaced = TRUE;
      break;
    }    

    // Combine L1 & L2 prefetches into an L1_L2 prefetch.
    if( ( pfhint0 == ECV_pfhint_L1_load &&
	  pfhint1 == ECV_pfhint_L2_load ) ||
	( pfhint0 == ECV_pfhint_L2_load &&
	  pfhint1 == ECV_pfhint_L1_load ) ){
      ;
    } else {
      opinfo = opinfo->same;
      continue;
    }

    OP* new_op = Dup_OP( op );
    Set_OP_opnd( new_op, 0 , Gen_Enum_TN( ECV_pfhint_L1_L2_load ) );

    //Bug 13609 : keep prefetchnta suggested by LNO
    BOOL nt = (OP_code(op)==TOP_prefetchnta);
    TOP  new_top = nt?TOP_prefetchnta:TOP_prefetcht0;
   
    if( OP_find_opnd_use( op, OU_base ) < 0 )
      new_top = nt?TOP_prefetchntaxx:TOP_prefetcht0xx;
    else if( OP_find_opnd_use( op, OU_index ) >= 0 )
      new_top = nt?TOP_prefetchntax:TOP_prefetcht0x;

    OP_Change_Opcode( new_op, new_top );

    if( EBO_Trace_Data_Flow ){
      fprintf( TFile, "%sReplace L1 and L2 prefetch OPs ",EBO_trace_pfx );
      Print_OP_No_SrcLine(pred_op);
      Print_OP_No_SrcLine(op);
      fprintf( TFile,"%swith   ",EBO_trace_pfx );
      Print_OP_No_SrcLine(new_op);
    }

    Copy_WN_For_Memory_OP( new_op, op);
    if ( OP_volatile( op ) )
      Set_OP_volatile( new_op );
    OP_srcpos( new_op ) = OP_srcpos( op );

    if( EBO_in_loop ){
      EBO_Copy_OP_omega( new_op, op );
    }

    BB_Insert_Op_After( OP_bb(op), op, new_op );
    replaced = TRUE;

    remove_op( opinfo );
    OP_Change_To_Noop(opinfo->in_op);
    opinfo->in_op = NULL;
    opinfo->in_bb = NULL;

    break;
  }

  return replaced;
}


BOOL
combine_adjacent_loads(OP *op,
                       EBO_TN_INFO **opnd_tninfo,
		       EBO_OP_INFO *opinfo,
                       INT64 offset_pred,
                       INT64 offset_succ)
{
  // TODO
  return FALSE;
}

static void
Expand_Extract_Bits (TYPE_ID rtype, TYPE_ID desc, UINT bit_offset, 
		     UINT bit_size,
		     TN *tgt_tn, TN *src_tn, OPS *ops)
{
  BOOL is_double = MTYPE_is_size_double(rtype);
  FmtAssert(MTYPE_bit_size(rtype) == MTYPE_bit_size(desc), 
	    ("Expand_Extract_Bits: Handle this case (1)")); 
  UINT pos = (Target_Byte_Sex != Host_Byte_Sex)
             ? MTYPE_bit_size(desc)-bit_offset-bit_size : bit_offset;
  if (pos == 0 && bit_size <= 16 && ! MTYPE_signed(rtype)) {
    Build_OP(is_double?TOP_andi64:TOP_andi32, tgt_tn, src_tn, 
	     Gen_Literal_TN((1 << bit_size)-1, is_double?8:4), ops);
    return;
  }

  TN* tmp1_tn = EBO_in_peep
    ? Build_Dedicated_TN (TN_register_class(tgt_tn), 
			  TN_register(tgt_tn), TN_size(tgt_tn))
    : Build_TN_Of_Mtype(rtype);

  if (EBO_in_peep) {
    // after LRA, EBO needs to take care of x86 fixup.
    Exp_COPY(tmp1_tn, src_tn, ops);
    src_tn = tmp1_tn;
  }
  ISA_REGISTER_CLASS rclass = ISA_REGISTER_CLASS_integer;
  INT reg_size = is_double ? 64 : 32;
    //ISA_REGISTER_CLASS_INFO_Bit_Size(ISA_REGISTER_CLASS_Info(rclass));
  TOP left_shift_op = is_double?TOP_shli64:TOP_shli32;
  INT left_shift_amt = reg_size - pos - bit_size;

  if( left_shift_amt == 0 ){
    Build_OP( MTYPE_bit_size(desc) == 64 ? TOP_mov64 : TOP_mov32,
	      tmp1_tn, src_tn, ops );

  } else if (left_shift_amt > 0){
    Build_OP(left_shift_op, tmp1_tn, src_tn, Gen_Literal_TN(left_shift_amt, 4),
	     ops);
  }

  TOP right_shift_op = is_double?TOP_sari64:TOP_sari32;
  INT right_shift_amt = reg_size - bit_size;
  if (! MTYPE_signed(rtype))
    right_shift_op = is_double ? TOP_shri64: TOP_shri32;

  if( right_shift_amt == 0 ){
    Build_OP( MTYPE_bit_size(desc) == 64 ? TOP_mov64 : TOP_mov32,
	      tgt_tn, tmp1_tn, ops );

  } else if (right_shift_amt > 0){
    Build_OP(right_shift_op, tgt_tn, tmp1_tn, 
	     Gen_Literal_TN(right_shift_amt, 4),
	     ops);
  }

  else if (left_shift_amt < 0 && right_shift_amt < 0) {
    if (left_shift_amt < right_shift_amt)
      Build_OP(right_shift_op, tgt_tn, src_tn, 
	       Gen_Literal_TN(right_shift_amt-left_shift_amt, 4),
	       ops);
    else
      Build_OP(left_shift_op, tgt_tn, src_tn, 
	       Gen_Literal_TN(left_shift_amt-right_shift_amt, 4),
	       ops);      
  } else
    FmtAssert( FALSE, ("Expand_Extract_Bits: Handle this case (2)")); 
}

struct SIZE_EXT_INFO {
  mUINT8 src_size;  // measured in bytes
  mUINT8 dest_size; // measured in bytes
  bool sign_ext;
};

#define SET_SIZE_EXT_INFO(o,s,d,sign) \
do {                                  \
  (o)->src_size = s;                  \
  (o)->dest_size = d;                 \
  (o)->sign_ext = sign;               \
} while(0)


static void Get_Size_Ext_Info( TOP top, SIZE_EXT_INFO* info )
{
  switch( top ){
  case TOP_movsbl:  SET_SIZE_EXT_INFO( info, 1, 4, true );   break;
  case TOP_movzbl:  SET_SIZE_EXT_INFO( info, 1, 4, false );  break;
  case TOP_movswl:  SET_SIZE_EXT_INFO( info, 2, 4, true );   break;
  case TOP_movzwl:  SET_SIZE_EXT_INFO( info, 2, 4, false );  break;
  case TOP_movsbq:  SET_SIZE_EXT_INFO( info, 1, 8, true );   break;
  case TOP_movzbq:  SET_SIZE_EXT_INFO( info, 1, 8, false );  break;
  case TOP_movswq:  SET_SIZE_EXT_INFO( info, 2, 8, true );   break;
  case TOP_movzwq:  SET_SIZE_EXT_INFO( info, 2, 8, false );  break;
  case TOP_movslq:  SET_SIZE_EXT_INFO( info, 4, 8, true );   break;
  case TOP_movzlq:  SET_SIZE_EXT_INFO( info, 4, 8, false );  break;

  case TOP_ld32_n32:
  case TOP_ld32:
  case TOP_ldx32:
  case TOP_ldxx32:
  case TOP_ld32_abs:
    SET_SIZE_EXT_INFO( info, 4, 4, false );
    break;
  case TOP_ld64:
  case TOP_ldx64:
  case TOP_ldxx64:
  case TOP_ld64_abs:
  case TOP_ld64_off:
    SET_SIZE_EXT_INFO( info, 8, 8, false );
    break;
  case TOP_ld8_32_n32:
  case TOP_ld8_32:
  case TOP_ldx8_32:
  case TOP_ldxx8_32:
    SET_SIZE_EXT_INFO( info, 1, 4, true );
    break;
  case TOP_ldu8_32_n32:
  case TOP_ldu8_32:
  case TOP_ldxu8_32:
  case TOP_ldxxu8_32:
    SET_SIZE_EXT_INFO( info, 1, 4, false );
    break;
  case TOP_ld8_abs:
    SET_SIZE_EXT_INFO( info, 1, 1, false );
    break;
  case TOP_ld16_32_n32:
  case TOP_ld16_32:
  case TOP_ldx16_32:
  case TOP_ldxx16_32:
    SET_SIZE_EXT_INFO( info, 2, 4, true );
    break;
  case TOP_ldu16_32_n32:
  case TOP_ldu16_32:
  case TOP_ldxu16_32:
  case TOP_ldxxu16_32:
    SET_SIZE_EXT_INFO( info, 2, 4, false );
    break;
  case TOP_ld16_abs:
    SET_SIZE_EXT_INFO( info, 2, 2, false );
    break;
  case TOP_ld8_64:
  case TOP_ldx8_64:
  case TOP_ldxx8_64:
  case TOP_ld8_64_off:
    SET_SIZE_EXT_INFO( info, 1, 8, true );
    break;
  case TOP_ldu8_64:
  case TOP_ldxu8_64:
  case TOP_ldxxu8_64:
  case TOP_ldu8_64_off:
    SET_SIZE_EXT_INFO( info, 1, 8, false );
    break;
  case TOP_ld16_64:
  case TOP_ldx16_64:
  case TOP_ldxx16_64:
  case TOP_ld16_64_off:
    SET_SIZE_EXT_INFO( info, 2, 8, true );
    break;
  case TOP_ldu16_64:
  case TOP_ldxu16_64:
  case TOP_ldxxu16_64:
  case TOP_ldu16_64_off:
    SET_SIZE_EXT_INFO( info, 2, 8, false );
    break;
  case TOP_ld32_64:
  case TOP_ldx32_64:
  case TOP_ldxx32_64:
  case TOP_ld32_64_off:
    SET_SIZE_EXT_INFO( info, 4, 8, true );
    break;

  default:
    FmtAssert( FALSE, ("Get_Size_Ext_Info: NYI") );
  }

  return;
}


BOOL
delete_subset_mem_op(OP *op,
                     EBO_TN_INFO **opnd_tninfo,
		     EBO_OP_INFO *opinfo,
                     INT64 offset_pred,
                     INT64 offset_succ)
{
#if Is_True_On
  if (!(EBO_Opt_Mask & EBO_DELETE_SUBSET_MEM_OP)) return FALSE;
#endif
  OP *pred_op = opinfo->in_op;
  BB *bb = OP_bb(op);
  INT opcount = OP_opnds(op);
  TN *pred_result = OP_store(pred_op) 
    ? OP_opnd(pred_op, OP_find_opnd_use(pred_op, OU_storeval))
    : OP_result(pred_op,0);
  TN *succ_result = OP_store(op)
    ? OP_opnd(op, OP_find_opnd_use(op,OU_storeval))
    : OP_result(op,0);
  BOOL succ_is_subset = FALSE;
  INT byte_offset;
  OPS ops = OPS_EMPTY;

  if (EBO_Trace_Data_Flow) {
    fprintf(TFile,"%ssubset    OP in BB:%d    ",EBO_trace_pfx,BB_id(bb));
    Print_OP_No_SrcLine(op);
    fprintf(TFile,"      Matches   OP in BB:%d    ",BB_id(opinfo->in_bb));
    Print_OP_No_SrcLine(pred_op);
  }
  
  if ((Opt_Level < 2) && (bb != opinfo->in_bb)) {
   /* Global TN's aren't supported at low levels of optimization. */
    return FALSE;
  }

  if (!EBO_in_peep &&
      (bb != opinfo->in_bb) &&
      !TN_Is_Constant(pred_result) &&
      has_assigned_reg(pred_result)) {
    if (EBO_Trace_Data_Flow) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile,"%sShouldn't move dedicated register references across blocks.\n",
              EBO_trace_pfx);
    }
    return FALSE;
  }

  const INT size_pred = CGTARG_Mem_Ref_Bytes(pred_op);
  const INT size_succ = CGTARG_Mem_Ref_Bytes(op);
  if (size_pred < size_succ) return FALSE;

  if ((offset_pred <= offset_succ) &&
      ((offset_pred + size_pred) >= (offset_succ + size_succ))) {
    succ_is_subset = TRUE;
  }

  if (!succ_is_subset) {
    return FALSE;
  }

  byte_offset = offset_succ - offset_pred;

  if (!OP_store(pred_op) || !OP_load(op)) {
   /* Can only optimize Store - Load pattern. */
    return FALSE;
  }

  if (byte_offset > 8) return FALSE;

  if ((TN_register_class(pred_result) != ISA_REGISTER_CLASS_integer) ||
      (TN_register_class(succ_result) != ISA_REGISTER_CLASS_integer)) {
   /* Can only play games with integers. */
    return FALSE;
  }

  if (size_succ == 1 &&
      EBO_in_peep){
    // All integer registers are byteable under m64.
    if (Is_Target_32bit()) {
      const REGISTER reg = TN_register(pred_result);
      const REGISTER_SET regs =
	REGISTER_SUBCLASS_members(ISA_REGISTER_SUBCLASS_m32_8bit_regs);
      if (!REGISTER_SET_MemberP(regs, reg))
	return FALSE;
    }
  }

  if (EBO_Trace_Optimization) {
#pragma mips_frequency_hint NEVER
    fprintf(TFile,"%sReplace subset load with left/right shifts\n",EBO_trace_pfx);
  }   

  if (offset_succ == offset_pred &&
      size_pred == size_succ) {
    TOP top = OP_code(op);
    if (TOP_is_load_ext(top) &&
	top != OP_code(pred_op))
      // The load needs sign/zero extension
      Exp_COPY_Ext(top, OP_result(op, 0), OP_opnd(pred_op,0), &ops );
    else
      Exp_COPY( OP_result(op,0), OP_opnd(pred_op,0), &ops );
    OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
    BB_Insert_Ops_After(OP_bb(op), op, &ops);
    return TRUE;
  }    

  /* Since a load will be converted to shifting ops later, and the new shifting
     op will affect the rflags. Thus, make a check first to avoid ruinning the
     rflags.
  */
  for( OP* next_op = OP_next(op); next_op != NULL; next_op = OP_next( next_op ) ){
    if( OP_reads_rflags( next_op ) )
      return FALSE;

    if( TOP_is_change_rflags( OP_code(next_op) ) )
      break;
  }

  INT bit_size = size_succ*8;
  INT succ_offset = (offset_succ - offset_pred);
  INT bit_offset = succ_offset*8;
  TYPE_ID rtype = MTYPE_UNKNOWN, desc = MTYPE_UNKNOWN;

  if( size_pred == 1 )
    desc = MTYPE_I1;
  else if( size_pred == 2 )
    desc = MTYPE_I2;    
  else if( size_pred == 4 )
    desc = MTYPE_I4;
  else if( size_pred == 8 )
    desc = MTYPE_I8;
  else
    FmtAssert( false, ("delete_subset_mem_op: NYI (1)"));

  struct SIZE_EXT_INFO op_size_ext_info;
  Get_Size_Ext_Info( OP_code(op), &op_size_ext_info );
  const BOOL unsigned_op = !op_size_ext_info.sign_ext;

  if (desc == MTYPE_I1) {
    if (unsigned_op)
      rtype = MTYPE_U1;
    else
      rtype = desc;
  } else if (desc == MTYPE_I2) {
    if (unsigned_op)
      rtype = MTYPE_U2;
    else
      rtype = desc;
  } else if (desc == MTYPE_I4) {
    if (unsigned_op)
      rtype = MTYPE_U4;
    else
      rtype = desc;
  } else {
    if (unsigned_op)
      rtype = MTYPE_U8;
    else
      rtype = desc;
  }

  Expand_Extract_Bits (rtype, desc, 
		       bit_offset, bit_size,
		       OP_result(op, 0), OP_opnd(pred_op, 0), &ops);

  OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
  BB_Insert_Ops_After(OP_bb(op), op, &ops);
  return TRUE;
}

/* 
 * delete_reload_across_dependency
 *
 * For a given load or store and one it matches,
 * attempt to replace one of them.
 * Return TRUE if this op is no longer needed.
 */
BOOL
delete_reload_across_dependency (OP *op,
                                 EBO_TN_INFO **opnd_tninfo,
		                 EBO_OP_INFO *opinfo,
		                 EBO_OP_INFO *intervening_opinfo)
{
  return FALSE;

  BB *bb = OP_bb(op);
  OPS ops = OPS_EMPTY;
  OP *pred_op = opinfo->in_op;
  OP *intervening_op = intervening_opinfo->in_op;
  TOP pred_opcode = OP_code(pred_op);
  TOP intervening_opcode = OP_code(intervening_op);
  INT pred_base_idx = OP_find_opnd_use(pred_op, OU_base);
  INT intervening_base_idx =  
    OP_find_opnd_use(intervening_op, OU_base);
  INT size_pred;
  INT size_succ;
  INT size_intervening;

  TN *pred_result;
  TN *intervening_result;
  TN *pred_index;
  TN *intervening_index;
  TN *predicate1;
  TN *predicate2;

 /* We can't assign registers, so don't optimize if it's already been done. */
  if (EBO_in_loop) return FALSE;
  if (EBO_in_peep) return FALSE;

  if (EBO_Trace_Execution) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile,"%sEnter delete_reload_across_dependency.\n",EBO_trace_pfx);
    Print_OP_No_SrcLine(pred_op);
    Print_OP_No_SrcLine(intervening_op);
    Print_OP_No_SrcLine(op);
  }

 /* Be sure we have a "Store .. Store .. Load" pattern. */
  if ((pred_op == NULL) ||
      (intervening_op == NULL) ||
      !OP_load(op) ||
      !(OP_load(pred_op) || OP_store(pred_op)) ||
      !OP_store(intervening_op)) return FALSE;

  pred_op = opinfo->in_op;
  intervening_op = intervening_opinfo->in_op;

  if (OP_prefetch(op) ||
      OP_prefetch(pred_op) ||
      OP_prefetch(intervening_op)) return FALSE;

  if (OP_unalign_mem(op) ||
      OP_unalign_mem(pred_op) ||
      OP_unalign_mem(intervening_op)) return FALSE;

  size_succ = CGTARG_Mem_Ref_Bytes(op);
  size_pred = CGTARG_Mem_Ref_Bytes(pred_op);
  size_intervening = CGTARG_Mem_Ref_Bytes(intervening_op);

  if ((size_succ != size_pred) ||
      (size_succ != size_intervening)) return FALSE;

 /* Capture the values in the preceeding memory OPs. */
  pred_result = OP_store(pred_op) ? 
    OP_opnd(pred_op, OP_find_opnd_use(pred_op, OU_storeval))
    : OP_result(pred_op,0);
  intervening_result = OP_opnd(intervening_op,
                               OP_find_opnd_use(intervening_op,OU_storeval));

  if ((TN_register_class(intervening_result) != 
       TN_register_class(pred_result)) ||
      (TN_register_class(intervening_result) != 
       TN_register_class(OP_result(op,0)))) {
    if (EBO_Trace_Data_Flow) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile,"%sInter-register copies are not supported\n",
              EBO_trace_pfx);
    }
    return FALSE;
  }

  if (TNs_Are_Equivalent( pred_result, intervening_result)) {
    /* It doesn't matter if the addresses are the same or different
       because the value will always be the same! Just Copy the value. */
    Is_True( false, ("delete_reload_across_dependency: NYI (1)"));
    return TRUE;
  }

 /* We need to compare the addresses BEFORE they were incremented.
    If both OPs are incremented by the same value, we can compare
    the addresses AFTER the increment.
 */
  pred_index = OP_opnd(pred_op, pred_base_idx);
  intervening_index = OP_opnd(intervening_op, intervening_base_idx);

 /* Are the index's available from each store? */
  if (!EBO_tn_available (bb, opinfo->actual_opnd[pred_base_idx]) ||
      !EBO_tn_available (bb, intervening_opinfo->actual_opnd[intervening_base_idx])) {
    if (EBO_Trace_Data_Flow) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile,"%sBase address not available for compare\n",
              EBO_trace_pfx);
    }
    return FALSE;
  }

  /* Compare the reload address with the intervening store address.
     Select the stored value if the address are the same,
     and resuse the predecesor value if they are not the same.
  */

  return TRUE;
}

// Optimize store-load sequence.  STORE_OPINFO specifies the store.  LOAD_OP is
// the load, with LOAD_ACTUAL_TNINFO and LOAD_OPND_TNINFO describing the actual
// and optimal operands, respectively.  Both the store and the load access the
// same memory location.
static BOOL
Special_Store_Load_Sequence (OP *load_op,
			     EBO_TN_INFO **load_actual_tninfo,
			     EBO_TN_INFO **load_opnd_tninfo,
			     EBO_OP_INFO *store_opinfo)
{
  Is_True(OP_store(store_opinfo->in_op) && OP_load(load_op),
	  ("Special_Store_Load_Sequence: not store-load sequence"));

  INT storeval_idx = OP_find_opnd_use(store_opinfo->in_op, OU_storeval);
  Is_True(storeval_idx >= 0,
	  ("Special_Store_Load_Sequence: invalid storeval index"));
  TN *storeval_tn = OP_opnd(store_opinfo->in_op, storeval_idx);

  // Replace:			; bug 7602
  //   xmm     = load mem1	; first_load_op
  //   mem2    = store xmm	; store_op
  //   int_reg = load mem2	; load_op
  // with:
  //   xmm     = load mem1
  //   mem2    = store xmm
  //   int_reg = load mem1	; change to load from mem1
  if (TN_register_class(OP_result(load_op,0)) == ISA_REGISTER_CLASS_integer &&
      TN_register_class(storeval_tn) == ISA_REGISTER_CLASS_float &&
      TN_size(OP_result(load_op, 0)) == TN_size(storeval_tn)) {	// bug 11321
    OP *store_op = store_opinfo->in_op;
    EBO_TN_INFO *storeval_tninfo = store_opinfo->actual_opnd[storeval_idx];
    if (storeval_tninfo != NULL &&
    	storeval_tninfo->in_op != NULL &&
	OP_load(storeval_tninfo->in_op)) {
      INT i;
      OP *first_load_op = storeval_tninfo->in_op;
      EBO_OP_INFO *first_load_opinfo = storeval_tninfo->in_opinfo;
      Is_True(first_load_opinfo != NULL,
	      ("Special_Store_Load_Sequence: opinfo NULL"));

      // Get first_load_op's base/index/offset/scale.  See if its base/index
      // TNs are available at load_op.
      TN *base_tn = NULL;
      TN *index_tn = NULL;
      TN *offset_tn = NULL;
      TN *scale_tn = NULL;

      INT base_idx = TOP_Find_Operand_Use(OP_code(first_load_op), OU_base);
      INT index_idx = TOP_Find_Operand_Use(OP_code(first_load_op), OU_index);
      INT offset_idx = TOP_Find_Operand_Use(OP_code(first_load_op),OU_offset);
      INT scale_idx = TOP_Find_Operand_Use(OP_code(first_load_op), OU_scale);

      EBO_TN_INFO *base_tninfo = NULL, *index_tninfo = NULL;

      if (base_idx >= 0) {	// base
	base_tn = OP_opnd(first_load_op, base_idx);
	base_tninfo = first_load_opinfo->actual_opnd[base_idx];
	if (base_tninfo == NULL ||
	    !EBO_tn_available(OP_bb(load_op), base_tninfo))
	  return FALSE;
      }
      if (index_idx >= 0) {	// index
	index_tn = OP_opnd(first_load_op, index_idx);
	index_tninfo = first_load_opinfo->actual_opnd[index_idx];
	if (index_tninfo == NULL ||
	    !EBO_tn_available(OP_bb(load_op), index_tninfo))
	  return FALSE;
      }
      offset_tn = offset_idx >= 0 ? OP_opnd(first_load_op, offset_idx) : NULL;
      scale_tn = scale_idx >= 0 ? OP_opnd(first_load_op, scale_idx) : NULL;

      // Check for aliased stores to mem1 occuring after first_load_op.
      EBO_OP_INFO *opinfo = EBO_opinfo_table[first_load_opinfo->hash_index];
      Is_True(opinfo != NULL,
	      ("Special_Store_Load_Sequence: OP hash table empty"));
      for ( ; opinfo != first_load_opinfo; opinfo = opinfo->same) {
	if (opinfo->in_op &&	// opinfo->in_op is NULL if OP is deleted
	    OP_store(opinfo->in_op) &&
	    opinfo->in_op != store_op) {
	  return FALSE;		// Potential alias store.
	}
      }

      // If load_op already loads from mem1, then don't replace load_op with
      // another copy of itself.
      {
	INT load_op_base_idx = TOP_Find_Operand_Use(OP_code(first_load_op),
						    OU_base);
	INT load_op_index_idx = TOP_Find_Operand_Use(OP_code(first_load_op),
						     OU_index);
	INT load_op_offset_idx = TOP_Find_Operand_Use(OP_code(first_load_op),
						      OU_offset);
	INT load_op_scale_idx = TOP_Find_Operand_Use(OP_code(first_load_op),
						     OU_scale);
	TN *load_op_base_tn = load_op_base_idx >= 0 ?
				OP_opnd(load_op, base_idx) : NULL;
	TN *load_op_index_tn = load_op_index_idx >= 0 ?
				 OP_opnd(load_op, index_idx) : NULL;
	TN *load_op_offset_tn = load_op_offset_idx >= 0 ?
				  OP_opnd(load_op, offset_idx) : NULL;
	TN *load_op_scale_tn = load_op_scale_idx >= 0 ?
				 OP_opnd(load_op, scale_idx) : NULL;

	if (load_op_base_tn == base_tn &&
	    load_op_index_tn == index_tn &&
	    (load_op_offset_tn == offset_tn ||
	     (TN_has_value(load_op_offset_tn) && TN_has_value(offset_tn) &&
	      TN_value(load_op_offset_tn) == TN_value(offset_tn))) &&
	    (load_op_scale_tn == scale_tn ||
	     (TN_has_value(load_op_scale_tn) && TN_has_value(scale_tn) &&
	      TN_value(load_op_scale_tn) == TN_value(scale_tn)))) {
	  return FALSE;
	}
      }

      // OK to change from load mem1 to load mem2.
      OP *new_op = Compose_Mem_Op_And_Copy_Info(load_op, index_tn, offset_tn,
						scale_tn, base_tn,
						load_actual_tninfo);

      BB_Insert_Op_After(OP_bb(load_op), load_op, new_op);

      // Increment the reference counts for first_load_op's base/index TNs.
      if (base_tninfo != NULL)
	inc_ref_count(base_tninfo);
      if (index_tninfo != NULL)
	inc_ref_count(index_tninfo);

      return TRUE;
    }
  }
  return FALSE;
}


/* 
 * delete_memory_op
 *
 * For a given load or store and one it matches,
 * attempt to replace one of them.
 * Return TRUE if this op is no longer needed.
 */
static
BOOL
delete_memory_op (OP *op,
                  EBO_TN_INFO **actual_tninfo,
                  EBO_TN_INFO **opnd_tninfo,
		  EBO_OP_INFO *opinfo)
{
#if Is_True_On
  if (!(EBO_Opt_Mask & EBO_DELETE_MEMORY_OP)) return FALSE;
#endif
  OPS ops = OPS_EMPTY;
  INT size_pred;
  INT size_succ;

  /* In each case below, before attempting to remove a load, store, or
     prefetch, we must make sure the instruction does not side-effect
     any state, etc. If it does, we can't remove it. This check is
     needed in addition to our general mechanism of making all state
     appear live on exit to the function. */

  ISA_REGISTER_CLASS state_rc = ISA_REGISTER_CLASS_UNDEFINED;
  if (state_rc != ISA_REGISTER_CLASS_UNDEFINED)
  {
    INT results = OP_results(op);
    for (UINT i = 0; i < results; i++)
    {
      TN *tn = OP_result(op, i);
      if (TN_is_register(tn) && (TN_register_class(tn) == state_rc))
	return FALSE;
    }
  }
  
  /* Remove the second OP for:
     Prefetch - Prefetch,
     Load - Prefetch,
     Store - Prefetch
  */
  if (OP_prefetch(op))
  {
    if (EBO_Trace_Optimization) {
      fprintf(TFile,"%sRemove following Prefetch combination\n",EBO_trace_pfx);
    }

    return TRUE;
  }

  /* Don't optimize:
     Prefetch - Load,
     Prefetch - Store,
  */
  if (OP_prefetch(opinfo->in_op))
  {
    return FALSE;
  }

  /* Don't try to optimize unaligned or unknown accesses. */
  if (OP_unalign_mem(op) || OP_unalign_mem(opinfo->in_op))
    return FALSE;

  size_pred = CGTARG_Mem_Ref_Bytes(opinfo->in_op);
  size_succ = CGTARG_Mem_Ref_Bytes(op);

  /* Replace the result tn of the second OP for:
     Load - Load,
  */
  if (OP_load(op) && OP_load(opinfo->in_op))
  {
    /* Make sure the result TNs' regclasses and ops match. */

    if (TN_register_class(OP_result(op,0)) !=
	TN_register_class(OP_result(opinfo->in_op, 0)))
    {
      if (EBO_Trace_Optimization)
	fprintf(TFile,"%sRegclass mismatch for Load - Load combination\n",
		EBO_trace_pfx);

      return FALSE;
    }

    /* This check should be moved after the subset check below, since
       here currently prevents us reaching the subsetting... (but too
       close to release to do it now...). */
    if (OP_code(op) != OP_code(opinfo->in_op))
    {
      if (EBO_Trace_Optimization)
	fprintf(TFile,"%sMtype mismatch for Load - Load combination\n",
		EBO_trace_pfx);

      return FALSE;
    }

    /* If the size of the data item loaded by the two loads is
       different, but the starting memory address is the same.
       There is a chance that the predecessor load is a wider load
       and that the new load's data can be extracted.

       Notice that while performing 32-bit operations with a GPR result in
       64-bit mode, opteron will zero-extend the 32-bit result.
    */
    
    if ((size_pred != size_succ) ||
	(OP_results(op) != OP_results(opinfo->in_op)) ||
	( TN_size(OP_result(opinfo->in_op, 0)) != TN_size(OP_result(op, 0)) &&
	  ( size_pred < 4 || size_succ < 4 ) ) )
    {
      if (EBO_Trace_Optimization)
      {
	fprintf(TFile,"%sSize mismatch for Load - Load combination: %d:%d %d:%d \n",
		EBO_trace_pfx,size_pred,size_succ,
		TN_size(OP_result(opinfo->in_op, 0)),TN_size(OP_result(op, 0)));
      }

      return delete_subset_mem_op (op, opnd_tninfo, opinfo, 0, 0);
    }
    
    if (!EBO_in_peep &&
	(OP_bb(op) != OP_bb(opinfo->in_op)) &&
	!TN_Is_Constant(OP_result(opinfo->in_op, 0)) &&
	has_assigned_reg(OP_result(opinfo->in_op, 0)))
    {
      if (EBO_Trace_Data_Flow)
      {
	fprintf(TFile,"%sShouldn't move dedicated register references across blocks.\n",
		EBO_trace_pfx);
      }
	
      return FALSE;
    }

    /* Remove the second load, and replace it with a copy of the first */
    
    if (EBO_Trace_Optimization)
    {
      fprintf(TFile,"%sRemove Load - Load combination\n",EBO_trace_pfx);
    }

    TOP top = OP_code(op);
    if (TOP_is_load_ext(top) &&
	top != OP_code(opinfo->in_op)) {
      // The load needs sign/zero extension
      for (UINT i = 0; i < OP_results(op); i++)
	Exp_COPY_Ext(top, OP_result(op, i), 
			 OP_result(opinfo->in_op, i), &ops);
    } else {
      for (UINT i = 0; i < OP_results(op); i++)
	EBO_Exp_COPY(NULL, OP_result(op, i), 
		     OP_result(opinfo->in_op, i), &ops);
    }
    
    if (EBO_in_loop)
      EBO_OPS_omega (&ops);
    
    BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
    return TRUE;
  }
  /* Replace the result tn of the second OP for:
     Store - Load
  */
  else if (OP_load(op) && OP_store(opinfo->in_op))
  {
    if( OP_side_effects( opinfo->in_op ) ){
      if( EBO_Trace_Optimization )
	fprintf(TFile,
		"%sStore has side effects for Load - Store combination\n",
		EBO_trace_pfx);

      return FALSE;
    }

    INT storeval_idx = OP_find_opnd_use(opinfo->in_op,OU_storeval);
    if (storeval_idx < 0)
    {
      if (EBO_Trace_Optimization)
	fprintf(TFile,
		"%sStore value TN unknown for Load - Store combination\n",
		EBO_trace_pfx);
	  
      return FALSE;
    }

    TN *storeval_tn = OP_opnd(opinfo->in_op, storeval_idx);
    const int size_storeval = CGTARG_Mem_Ref_Bytes(opinfo->in_op);

    /* Make sure the storeval/result TNs' regclasses and mtypes
       match. It isn't sufficient to just check regclasses since
       user-defined operations for two ctypes in the same regfile can
       have different semantics. Make an exception for 32-bit
       loads/stores to the integer register file, since we know that
       they have the same semantics for both signed and unsigned. */

    if (TN_register_class(OP_result(op,0)) !=
	TN_register_class(storeval_tn))
    {
      if (Special_Store_Load_Sequence(op, actual_tninfo, opnd_tninfo, opinfo))
        return TRUE;

      if (EBO_Trace_Data_Flow)
	fprintf(TFile,"%sRegclass mismatch for Store - Load combination\n",
		EBO_trace_pfx);

      return FALSE;
    }

    if (!EBO_in_peep &&
	(OP_bb(op) != OP_bb(opinfo->in_op)) &&
	!TN_Is_Constant(storeval_tn) &&
	has_assigned_reg(storeval_tn))
    {
      if (EBO_Trace_Data_Flow)
	fprintf(TFile,"%sShouldn't move dedicated register references across blocks.\n",
		EBO_trace_pfx);
      
      return FALSE;
    }

    if (TN_is_dedicated(storeval_tn) && 
	(TN_register(storeval_tn) == RCX || TN_register(storeval_tn) == RDX)) 
    { // bug 3842
      if (EBO_Trace_Data_Flow)
	fprintf(TFile,"%sShould not move special dedicated registers RCX and RDX.\n",
		EBO_trace_pfx);
      return FALSE;
    }

    /* If the size of the data moved to and from memory is the same,
       but the size of the stored value is larger than the size of
       the value we want to load, then mask off the upper portion of
       the stored value and use that instead of the loaded value. */
    if (size_pred == size_succ)
    {
      if (size_storeval > size_succ)
      {
	if (EBO_Trace_Data_Flow)
	  fprintf(TFile,"%sSize mismatch for Store - Load combination: %d %d %d\n",
		  EBO_trace_pfx,size_pred,size_storeval,size_succ);

	return delete_subset_mem_op (op, opnd_tninfo, opinfo, 0, 0);
      }

      if (EBO_Trace_Optimization)
	fprintf(TFile,"%sRemove Store - Load combination\n",EBO_trace_pfx);

      TOP top = OP_code(op);
      if (TOP_is_load_ext(top) &&
	  top != OP_code(opinfo->in_op))
	// The load needs sign/zero extension
	Exp_COPY_Ext(top, OP_result(op, 0), storeval_tn, &ops);
      else
	EBO_Exp_COPY(NULL, OP_result(op, 0), storeval_tn, &ops);

      if (EBO_in_loop) {
	CG_LOOP_Init_Op(OPS_first(&ops));
	Set_OP_omega (OPS_first(&ops), 0, opinfo->actual_opnd[storeval_idx]->omega);
      }

      BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
      return TRUE;
    }
    /* The size of the memory accesses are different, but the starting
       memory address is the same.  There is a chance that the
       predecessor store is wider than the load. */
    else
    {
      return delete_subset_mem_op (op, opnd_tninfo, opinfo, 0, 0);
    }
  }
  /* Remove the first OP for:
     Store - Store
  */
  else if (OP_store(op) && OP_store(opinfo->in_op) &&
	   (OP_bb(op) == OP_bb(opinfo->in_op)))
  {
    if (size_pred != size_succ)
      return FALSE;

    if (opinfo->op_must_not_be_removed)
      return FALSE;

    if (EBO_Trace_Optimization)
      fprintf(TFile,"%sRemove Store - Store combination\n",EBO_trace_pfx);

    remove_op (opinfo);
    OP_Change_To_Noop(opinfo->in_op);
    opinfo->in_op = NULL;
    opinfo->in_bb = NULL;
    return FALSE;
  }
  /* Don't optimize:
     Load - Store
  */
  else {
    if( EBO_Trace_Optimization ){
      fprintf( TFile, "Load - Store combination is not optimized\n" );
    }
    return FALSE;
  }

  return FALSE;
}


/* 
 * delete_duplicate_op
 *
 * For a given op and one it matches, attempt to replace 
 * one of them.
 * Return TRUE if this op is no longer needed.
 */
BOOL
delete_duplicate_op (OP *op,
		     EBO_TN_INFO **opnd_tninfo,
		     EBO_OP_INFO *opinfo,
		     EBO_TN_INFO **actual_tninfo)
{
#if Is_True_On
  if (!(EBO_Opt_Mask & EBO_DELETE_DUPLICATE_OP)) return FALSE;
#endif
  INT resnum;
  OPS ops = OPS_EMPTY;

  // integer compare ops can not be deleted because they
  // need to set the rflags once again.
  if (OP_icmp(op))
    return FALSE;

  if (EBO_Trace_Data_Flow) {
    fprintf(TFile,"%sDuplicate OP in BB:%d    ",EBO_trace_pfx,BB_id(OP_bb(op)));
    Print_OP_No_SrcLine(op);
    fprintf(TFile,"      Matches   OP in BB:%d    ",BB_id(opinfo->in_bb));
    Print_OP_No_SrcLine(opinfo->in_op);
  }

  /* Global TN's aren't supported at low levels of optimization. */

  if ((Opt_Level < 2) && (OP_bb(op) != opinfo->in_bb))
    return FALSE;

  /* Separate load/store processing, but logically it's just a special case. */

  if (OP_memory(op))
  {
    return delete_memory_op (op, actual_tninfo, opnd_tninfo, opinfo);
  }
  else
  {
    /* Take good care of the rflags cases.
     */

    if( TOP_is_change_rflags( OP_code(op) ) ){
      for( OP* next_op = OP_next(op); next_op != NULL; next_op = OP_next( next_op ) ){
	if( OP_reads_rflags( next_op ) )
	  return FALSE;

	if( TOP_is_change_rflags( OP_code(next_op) ) )
	  break;
      }
    }

    /* There is no easy way to copy FCC registers, so skip this optimization
     *  if the result is of register class FCC. */

    TOP top = OP_code(op);
    /* Create copies of the result TN's. */

    for (resnum = 0; resnum < OP_results(op); resnum++) {
      if (TOP_is_load_ext(top) &&
	  top != OP_code(opinfo->in_op) )
	// The load needs sign/zero extension
	Exp_COPY_Ext(top, OP_result(op, resnum), 
		     OP_result(opinfo->in_op, resnum), &ops);
      else
	EBO_Exp_COPY(NULL, OP_result(op, resnum), 
		     OP_result(opinfo->in_op, resnum), &ops);
    }

    if (EBO_in_loop)
      EBO_OPS_omega (&ops);

    BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
    return TRUE;
  }
  
  return FALSE;
}


/* Return the opcode for <op> that can take <imm_val>
   as its <opnd>th operand.
*/
static TOP TOP_with_Imm_Opnd( OP* op, int opnd, INT64 imm_val )
{
  const TOP top = OP_code(op);
  const ISA_OPERAND_INFO* oinfo = ISA_OPERAND_Info(top);
  const ISA_OPERAND_VALTYP* vtype = ISA_OPERAND_INFO_Operand(oinfo, 1);
    
  if( ISA_OPERAND_VALTYP_Is_Literal(vtype) )
    return TOP_UNDEFINED;

  //const ISA_LIT_CLASS lc = ISA_OPERAND_VALTYP_Literal_Class(vtype);
  if( !ISA_LC_Value_In_Class( imm_val, LC_simm32 ) )
    return TOP_UNDEFINED;

  return CGTARG_Immed_To_Reg( top );
}


/* Attempt to convert an add of 'tn' + 'imm_val' into an addi. Return
   TRUE if we succeed, FALSE otherwise. */
static BOOL
Convert_Imm_Add (OP *op, TN *tnr, TN *tn, INT64 imm_val, 
                 EBO_TN_INFO *tninfo, BOOL simplify_iadd)
{
#if Is_True_On
  if (!(EBO_Opt_Mask & EBO_CONVERT_IMM_ADD)) return FALSE;
#endif
  OP *pred_op = (tninfo) ? tninfo->in_op : NULL;
  OP *new_op = NULL;
  TOP new_opcode;
  BOOL is_64bit = (TN_size(tnr) == 8);
  if (imm_val == 0) {
    if( Is_Target_32bit() ){
      for( OP* next_op = OP_next(op); next_op != NULL; next_op = OP_next( next_op ) ){
	if( OP_reads_rflags( next_op ) ){
	  return FALSE;
	}

	if( TOP_is_change_rflags( OP_code(next_op) ) )
	  break;
      }
    }

    new_opcode = is_64bit ? TOP_mov64 : TOP_mov32;
    new_op = Mk_OP(new_opcode, tnr, tn);

  } else if (ISA_LC_Value_In_Class ( imm_val, LC_simm32)) {
    // Use simplify_iadd to guard against inc/dec forms which
    // come from addi-addi combinations.
    if ( simplify_iadd ) {
      if ( OP_code(op) == TOP_lea32 || OP_code(op) == TOP_lea64 ) {
        return FALSE;
      } else if ( OP_code(op) == TOP_addi32 || OP_code(op) == TOP_addi64 ) {
        if ( ( imm_val != 1 ) && ( imm_val != -1 ) )
          return FALSE;
        else if ( Is_Target_32bit() ) 
          return FALSE;
      }
    } else if ( OP_code(op) == TOP_addi32 || OP_code(op) == TOP_addi64 ||
                OP_code(op) == TOP_lea32  || OP_code(op) == TOP_lea64 ) {
      return FALSE;
    }
    new_opcode = is_64bit ? TOP_addi64 : TOP_addi32;
    BOOL rflags_read = FALSE;
    if ( simplify_iadd )
      new_opcode = OP_code(op);

    // If there is an instruction that is awaiting a rflags update then, 
    // do not convert the current op.
    for( OP* next_op = OP_next(op); next_op != NULL;
	 next_op = OP_next( next_op ) ){
      if( OP_reads_rflags( next_op ) )
	rflags_read = TRUE;
      if( TOP_is_change_rflags( OP_code(next_op) ) )
	break;
    }
    if( !rflags_read && EBO_in_peep &&
	!TNs_Are_Equivalent(tnr,tn) )
      new_opcode = is_64bit ? TOP_lea64 : TOP_lea32;      
    else if ( imm_val == 1 && CG_use_incdec &&
	      TN_is_register(tnr) && TN_is_register(tn) && 
	      TNs_Are_Equivalent(tnr,tn) )
      new_opcode = is_64bit ? TOP_inc64 : TOP_inc32;      
    else if ( imm_val == -1 && CG_use_incdec && 
	      TN_is_register(tnr) && TN_is_register(tn) && 
	      TNs_Are_Equivalent(tnr,tn) )
      new_opcode = is_64bit ? TOP_dec64 : TOP_dec32;      

    if (new_opcode == OP_code(op) || 
	(rflags_read && 
	 ((TOP_is_change_rflags( new_opcode ) && 
	   !TOP_is_change_rflags( OP_code(op) )) ||
	  (!TOP_is_change_rflags( new_opcode ) && 
	   TOP_is_change_rflags( OP_code(op) )))))
      return FALSE;

    if ( simplify_iadd ) {
      bool valid_inc_dec = true;
      if ( is_64bit && (OP_code(op) != TOP_addi64))
        valid_inc_dec = false;
      else if (!is_64bit && (OP_code(op) != TOP_addi32))
        valid_inc_dec = false;

      if (valid_inc_dec == false)
        return FALSE;
    }

    if ( simplify_iadd ) {
      // valid inc/dec conversion with like type input are fold opportunities
      if ( pred_op && 
           ( OP_code(op) == OP_code(pred_op) ) && 
           ( tninfo->reference_count == 1 ) )
        return FALSE;
    }

    if (new_opcode != TOP_inc32 && new_opcode != TOP_inc64 &&
	new_opcode != TOP_dec32 && new_opcode != TOP_dec64)
      new_op = Mk_OP(new_opcode, tnr, tn, Gen_Literal_TN(imm_val, 4));
    else
      new_op = Mk_OP(new_opcode, tnr, tn);
  } else {
    return FALSE;
    TN *src1 = Gen_Literal_TN(imm_val, TN_size(tnr));
    TN *tmp = Build_TN_Like(tnr);
    new_op = Mk_OP(TOP_ldc64, tmp, src1);
    BB_Insert_Op_After(OP_bb(op), op, new_op);
    op = new_op;
    new_opcode = is_64bit ? TOP_add64 : TOP_add32;
    if( EBO_in_peep ){
      FmtAssert( TNs_Are_Equivalent(tnr,tn), ("Convert_Imm_Add: NYI"));
    }
    new_op = Mk_OP(new_opcode, tnr, tn, tmp);
  }
  BB_Insert_Op_After(OP_bb(op), op, new_op);
  return TRUE;
}


/*
 * Look at an exression that has a constant first operand and attempt to
 * simplify the computations.
 */
BOOL
Constant_Operand0 (OP *op,
                   TN **opnd_tn,
                   EBO_TN_INFO **opnd_tninfo)
{
#if Is_True_On
  if (!(EBO_Opt_Mask & EBO_CONSTANT_OPERAND0)) return FALSE;
#endif
  TOP opcode = OP_code(op);
  INT o0_idx = 0;
  INT o1_idx = (OP_opnds(op) > 1) ? 1 : -1;  

  /* Nothing to optimize if no operands... */
  if (OP_opnds(op) < 1)
    return FALSE;

  if (EBO_Trace_Execution)
  {
    fprintf(TFile, "%sin BB:%d constant0 OP :- %s",
            EBO_trace_pfx, BB_id(OP_bb(op)),TOP_Name(OP_code(op)));
    for (UINT i = 0; i < OP_opnds(op); i++)
    {
      fprintf(TFile," ");
      Print_TN(opnd_tn[i],FALSE);
    }
    fprintf(TFile,"\n");
  }

  TN *tn0 = opnd_tn[o0_idx];
  TN *tn1 = (o1_idx >= 0) ? opnd_tn[o1_idx] : NULL;
  TN *tnr = OP_has_result(op) ? OP_result(op,0) : NULL;

  /* Don't mess with symbols. */
  if (TN_is_symbol(tn0))
    return FALSE;

  /* Conditional moves have two of the three operands marked as opnd1
     and opnd2, so we can reach here (operand representing the use of
     the result register is not marked). However we can't do anything
     special if 'tn0' is constant (we must have this check because
     'tn1' can also be constant when we reach here, we don't go to
     Fold_Constant_Expression because the operand representing the use
     of the result register is not constant). */
  if (TOP_is_cond_move(opcode))
    return FALSE;
  
  /* We should only be called if tn0 is constant and tn1 is not. */
  FmtAssert(TN_Is_Constant(tn0) && 
	    ((OP_opnds(op) > 2) || !tn1 || !TN_Is_Constant(tn1)),
	    ("Constant_Operand0: Unexpected constant/non-constant operands"));

  if (opcode == TOP_add32 ||
      opcode == TOP_add64 ||
      opcode == TOP_lea32 ||
      opcode == TOP_lea64)
    return Convert_Imm_Add(op, tnr, tn1, TN_value(tn0), 
                           opnd_tninfo[o1_idx], false);

  return FALSE;
}


/* Attempt to convert an int and of 'tn' & '0xffff' into a move ext. Return
   TRUE if we succeed, FALSE otherwise.
*/
static BOOL Convert_Imm_And( OP* op, TN *tnr, TN *tn, INT64 imm_val, EBO_TN_INFO *tninfo )
{
#if Is_True_On
  if (!(EBO_Opt_Mask & EBO_CONVERT_IMM_AND)) return FALSE;
#endif
  TOP new_top = TOP_UNDEFINED;
  OPS ops = OPS_EMPTY;

  /* First, handle special cases. */

  if( imm_val == 0xff ){
    // Under m32, not all general purpose registers are byte-addressable.
    if (Is_Target_32bit() &&
	EBO_in_peep) {
      const REGISTER reg = TN_register(tn);
      const REGISTER_SET regs =
	      REGISTER_SUBCLASS_members(ISA_REGISTER_SUBCLASS_m32_8bit_regs);
      Is_True(TN_register_class(tn) == ISA_REGISTER_CLASS_integer,
	      ("Convert_Imm_And: TN not integer register class"));
      if (!REGISTER_SET_MemberP(regs, reg))
        return FALSE;
    }

    new_top = TN_size(tnr) == 8 ? TOP_movzbq : TOP_movzbl;

  } else if( imm_val == 0xffff ){
    new_top = TN_size(tnr) == 8 ? TOP_movzwq : TOP_movzwl;
    
  } else if( imm_val == 0xffffffff && TN_size(tnr) != 8){
    new_top = TOP_mov32;
  }

  if( new_top != TOP_UNDEFINED ){
    Build_OP( new_top, tnr, tn, &ops );
    BB_Insert_Ops_After( OP_bb(op), op, &ops );

    return TRUE;
  }

  /* Second, convert the opcode to carry the <imm_val>. */

  new_top = TOP_with_Imm_Opnd( op, 1, imm_val );

  if( new_top == TOP_UNDEFINED )
    return FALSE;

  Build_OP( new_top, tnr, tn,
	    Gen_Literal_TN(imm_val,4),
	    &ops );
  BB_Insert_Ops_After( OP_bb(op), op, &ops );

  return TRUE;
}

  
static BOOL Convert_Imm_Or( OP* op, TN *tnr, TN *tn, INT64 imm_val, EBO_TN_INFO *tninfo )
{
#if Is_True_On
  if (!(EBO_Opt_Mask & EBO_CONVERT_IMM_OR)) return FALSE;
#endif
  OPS ops = OPS_EMPTY;

  if( imm_val == 0x0 ){
    Exp_COPY( tnr, tn, &ops );
    BB_Insert_Ops_After( OP_bb(op), op, &ops );
    return TRUE;
  }

  const TOP new_top = TOP_with_Imm_Opnd( op, 1, imm_val );
  if( new_top == TOP_UNDEFINED )
    return FALSE;

  Build_OP( new_top, tnr, tn,
	    Gen_Literal_TN(imm_val,4),
	    &ops );
  BB_Insert_Ops_After( OP_bb(op), op, &ops );

  return TRUE;
}

  
static BOOL Convert_Imm_Xor( OP* op, TN *tnr, TN *tn, INT64 imm_val, EBO_TN_INFO *tninfo )
{
#if Is_True_On
  if (!(EBO_Opt_Mask & EBO_CONVERT_IMM_XOR)) return FALSE;
#endif
  OPS ops = OPS_EMPTY;

  if( imm_val == 0x0 ){
    Exp_COPY( tnr, tn, &ops );
    BB_Insert_Ops_After( OP_bb(op), op, &ops );
    return TRUE;
  }

  if( imm_val == -1 ){
    Build_OP( TN_size(tnr) == 4 ? TOP_not32 : TOP_not64, tnr, tn, &ops );
    BB_Insert_Ops_After( OP_bb(op), op, &ops );
    return TRUE;
  }

  const TOP new_top = TOP_with_Imm_Opnd( op, 1, imm_val );

  if( new_top == TOP_UNDEFINED )
    return FALSE;

  Build_OP( new_top, tnr, tn,
	    Gen_Literal_TN(imm_val,4),
	    &ops );

  BB_Insert_Ops_After( OP_bb(op), op, &ops );

  return TRUE;
}


static BOOL Convert_Imm_Cmp( OP* op, TN *tnr, TN *tn, INT64 imm_val,
			     EBO_TN_INFO *tninfo )
{
#if Is_True_On
  if (!(EBO_Opt_Mask & EBO_CONVERT_IMM_CMP)) return FALSE;
#endif
  const TOP new_top = TOP_with_Imm_Opnd( op, 1, imm_val );

  if( new_top == TOP_UNDEFINED )
    return FALSE;

  OPS ops = OPS_EMPTY;
  Build_OP( new_top, tnr, tn, Gen_Literal_TN(imm_val,4), &ops );

  BB_Insert_Ops_After( OP_bb(op), op, &ops );

  return TRUE;
}


/* Attempt to convert an int mul of 'tn' * 'imm_val' into a shift. Return
   TRUE if we succeed, FALSE otherwise.
*/
static BOOL Convert_Imm_Mul( OP *op, TN *tnr, TN *tn, INT64 imm_val, EBO_TN_INFO *tninfo )
{
#if Is_True_On
  if (!(EBO_Opt_Mask & EBO_CONVERT_IMM_MUL)) return FALSE;
#endif
  TN* tnr1 = ( OP_results(op) == 2 ) ? OP_result( op, 1 ) : NULL;

  OP *new_op = NULL;
  const BOOL is_64bit = (TN_size(tnr) == 8);
  const TYPE_ID mtype = is_64bit ? MTYPE_I8 : MTYPE_I4;
  UINT64 val = imm_val < 0 ? -imm_val : imm_val;
  OPS ops = OPS_EMPTY;

  if( imm_val == 0 ){
    Exp_Immediate( tnr, Gen_Literal_TN(0,4), false, &ops );
    if( tnr1 != NULL ){
      Exp_Immediate( tnr1, Gen_Literal_TN(0,4), false, &ops );
    }
    BB_Insert_Ops_After( OP_bb(op), op, &ops );
    return TRUE;
  }

  if( imm_val == 1 ){
    Exp_COPY( tnr, tn, &ops );
    if( tnr1 != NULL ){
      Exp_Immediate( tnr1, Gen_Literal_TN(0,4), false, &ops );
    }
    BB_Insert_Ops_After( OP_bb(op), op, &ops );
    return TRUE;
  }

  if( tnr1 != NULL )
    return FALSE;

  bool need_an_add = false;

  /* Check for an unsigned power of two + 1. */
  
  if( val >= 2 &&
      ( (val-1) & (val-2) ) == 0 ){
    val--;
    need_an_add = true;
  }

  /* Check whether it can carry an imm opnd. */
  /* Check for unsigned power of two. */
  if( ( val & ( val - 1 ) ) != 0 ){
    const TOP new_top = TOP_with_Imm_Opnd( op, 1, imm_val );

    if( new_top == TOP_UNDEFINED )
      return FALSE;

    Build_OP( new_top, tnr, tn,
	      Gen_Literal_TN(imm_val,4),
	      &ops );
    BB_Insert_Ops_After( OP_bb(op), op, &ops );

    return TRUE;
  }

  if( TNs_Are_Equivalent(tnr, tn ) && need_an_add ){
    if( TN_register(tn) != REGISTER_UNDEFINED )
      return FALSE;

    TN* tmp = Build_TN_Like( tn );
    Exp_COPY( tmp, tn, &ops );
    tn = tmp;
  }

  /* determine the power of val. */
  
  int power = 0;
  while( val != 1 ){
    power++;
    val >>= 1;
  }

  Expand_Shift( tnr, tn, Gen_Literal_TN( power, 4 ), mtype, shift_left, &ops );

  if( need_an_add ){
    Expand_Add( tnr, tnr, tn, mtype, &ops );
  }

  if( imm_val < 0 && imm_val != INT64_MIN )
    Expand_Neg( tnr, tnr, mtype, &ops );

  BB_Insert_Ops_After( OP_bb(op), op, &ops );
  return TRUE;
}

BOOL OP_iadd_inc(OP* op)
{
  if (OP_iadd(op)) return TRUE;
  TOP top = OP_code(op);
  if (top == TOP_inc32 || top == TOP_inc64 ||
      top == TOP_dec32 || top == TOP_dec64)
	 return TRUE;
  return FALSE; 

}


/*
 * Look at an exression that has a constant second operand and attempt to
 * simplify the computations.
 */
BOOL
Constant_Operand1 (OP *op,
                   TN **opnd_tn,
                   EBO_TN_INFO **opnd_tninfo)
{
#if Is_True_On
  if (!(EBO_Opt_Mask & EBO_CONSTANT_OPERAND1)) return FALSE;
#endif
  BB *bb = OP_bb(op);
  TOP opcode = OP_code(op);
  INT o0_idx = 0;
  INT o1_idx = (OP_opnds(op) > 1) ? 1 : -1;  

  /* Nothing to optimize if no operands... */
  if (OP_opnds(op) < 1)
    return FALSE;

  if (EBO_Trace_Execution)
  {
    fprintf(TFile, "%sin BB:%d constant1 OP :- %s",
            EBO_trace_pfx, BB_id(OP_bb(op)), TOP_Name(OP_code(op)));
    for (UINT i = 0; i < OP_opnds(op); i++)
    {
      fprintf(TFile," ");
      Print_TN(opnd_tn[i],FALSE);
    }
    fprintf(TFile,"\n");
  }

  TN *tn0 = opnd_tn[o0_idx];
  TN *tn1 = opnd_tn[o1_idx];
  if (OP_code(op) == TOP_inc32 || OP_code(op) == TOP_inc64)
    tn1 = Gen_Literal_TN(1, 4);
  else if (OP_code(op) == TOP_dec32 || OP_code(op) == TOP_dec64)
    tn1 = Gen_Literal_TN(-1, 4);

  TN *tnr = OP_has_result(op) ? OP_result(op,0) : NULL;

  /* Don't mess with symbols. */
  if (TN_is_symbol(tn1))
    return FALSE;

  /* Don't treat a memory opnd as a regular opnd. */
  if( o1_idx > 0 &&
      ( OP_find_opnd_use( op, OU_base ) == o1_idx ||
	OP_find_opnd_use( op, OU_index ) == o1_idx ) ){
    return FALSE;
  }

  /* We should only be called if tn1 is constant and tn0 is not. */
  FmtAssert(TN_Is_Constant(tn1) && ((OP_opnds(op) > 2) || !TN_Is_Constant(tn0)),
	    ("Constant_Operand1: Unexpected constant/non-constant operands"));

  /* For all the negative value whose TN_size
     is 4, the higher 32-bit is 0s due to the restriction of opteron.
  */
  const INT64 imm_val = TN_value(tn1);

  if( OP_iand( op ) )
    return Convert_Imm_And(op, tnr, tn0, imm_val, opnd_tninfo[o0_idx]);

  if( OP_ior( op ) )
    return Convert_Imm_Or( op, tnr, tn0, imm_val, opnd_tninfo[o0_idx] );

  if( OP_ixor( op ) )
    return Convert_Imm_Xor(op, tnr, tn0, imm_val, opnd_tninfo[o0_idx]);

  if (opcode == TOP_add32 ||
      opcode == TOP_add64 ||
      opcode == TOP_lea32 || 
      opcode == TOP_lea64 )
    return Convert_Imm_Add( op, tnr, tn0, imm_val, 
                            opnd_tninfo[o0_idx], false );


  if( OP_imul( op ) )
    return Convert_Imm_Mul( op, tnr, tn0, imm_val, opnd_tninfo[o0_idx] );

  if( OP_icmp( op ) )
    return Convert_Imm_Cmp( op, tnr, tn0, imm_val, opnd_tninfo[o0_idx] );

  /*****************************************************************/
  /* Now, look for sequences ending in 'op' that can be optimized. */

  /* No opnd info if operand is constant. */
  if (opnd_tninfo[o0_idx] == NULL)
    return FALSE;

  OP *pred_op = opnd_tninfo[o0_idx]->in_op;
  if (pred_op == NULL)
    return FALSE;

  TOP pred_opcode = OP_code(pred_op);

  /* Look for a sequence of two addi that can be combined. */
  if (OP_iadd_inc(op) && OP_iadd_inc(pred_op))
  {
    INT ptn0_idx = 0;
    INT ptn1_idx = 1;
    TN *ptn0 = OP_opnd(pred_op, ptn0_idx);
    TN *ptn1 = OP_opnd(pred_op, ptn1_idx);
    if (OP_code(pred_op) == TOP_inc32 || OP_code(pred_op) == TOP_inc64)
      ptn1 = Gen_Literal_TN(1, 4);
    else if (OP_code(pred_op) == TOP_dec32 || OP_code(pred_op) == TOP_dec64)
      ptn1 = Gen_Literal_TN(-1, 4);

    if (TN_is_constant(ptn1) && !TN_is_symbol(ptn1))
    {
      EBO_OP_INFO *pred_opinfo = locate_opinfo_entry(opnd_tninfo[o0_idx]);
      EBO_TN_INFO *ptn0_tninfo = pred_opinfo->actual_opnd[ptn0_idx];

      if (EBO_tn_available(bb, ptn0_tninfo))
      {
	const INT64 new_val = imm_val + TN_value(ptn1);
	if (Convert_Imm_Add(op, tnr, ptn0, new_val, ptn0_tninfo, false))
	{
	  if (EBO_Trace_Optimization)
	    fprintf(TFile,"\tcombine immediate adds\n");

	  return TRUE;
	}
      }
    }
  }

  if ( opcode == TOP_addi32 ||
       opcode == TOP_addi64 ) {
    if ( ( imm_val == 1 ) || ( imm_val == -1 ) ) {
      return Convert_Imm_Add( op, tnr, tn0, imm_val, 
                              opnd_tninfo[o0_idx], true );
    }
  }

  return FALSE;
}


  
/*
 * Look at a branch exression that has all constant operands and attempt to
 * evaluate the expression.
 *
 */
BOOL
Resolve_Conditional_Branch (OP *op, TN **opnd_tn)
{
#if Is_True_On
  if (!(EBO_Opt_Mask & EBO_RESOLVE_CONDITIONAL_BRANCH)) return FALSE;
#endif
  if( TOP_is_ijump( OP_code(op) ) )
    return FALSE;

  Is_True( false, ("Resolve_Conditional_Branch: NYI") );
  return FALSE;
}


  
/*
 * Look at an exression that has all constant operands and attempt to
 * evaluate the expression.
 *
 * Supported operations are:
 *   add, sub, mult, and, or, xor, nor, sll, srl, slt
 */
BOOL
Fold_Constant_Expression (OP *op,
                          TN **opnd_tn,
                          EBO_TN_INFO **opnd_tninfo)
{
#if Is_True_On
  if (!(EBO_Opt_Mask & EBO_FOLD_CONSTANT_EXPRESSION)) return FALSE;
#endif
  TOP opcode = OP_code(op);
  TN *tnr = OP_result(op,0);

  if (OP_opnds(op) == 0)
    return FALSE;

  /* Only attempt to do compile-time arithmetic on integers. */

  if (TN_register_class(tnr) != ISA_REGISTER_CLASS_integer)
    return FALSE;

  /* Don't remove an op that will change the rflag, and this rflag is necessary
     for the following op.
  */
  if( TOP_is_change_rflags( OP_code(op) ) ){
    for( OP* next_op = OP_next(op); next_op != NULL; next_op = OP_next( next_op ) ){
      if( OP_reads_rflags( next_op ) )
	return FALSE;

      if( TOP_is_change_rflags( OP_code(next_op) ) )
	break;
    }
  }

  if (EBO_Trace_Execution)
  {
    fprintf(TFile, "%sin BB:%d Constant OP :- %s ",
	    EBO_trace_pfx, BB_id(OP_bb(op)),TOP_Name(opcode));

    for (UINT i = 0; i < OP_opnds(op); i++)
    {
      fprintf(TFile," ");
      Print_TN(opnd_tn[i],TRUE);
    }
      
    fprintf(TFile,"\n");
  }

  TN *tn0 = opnd_tn[0];
  TN *tn1 = opnd_tn[1];

  INT64 result_val;
  INT64 tn0_val, tn1_val;
  UINT64 tn0_uval, tn1_uval;

  ST *result_sym = NULL;
  INT32 result_relocs;

  /* Determine the constant value of every operand. */
  
  if (TN_is_symbol(tn0))
  {
    /* Can we handle case where both tn's are symbols? How? */
    if ((OP_opnds(op) == 2) && TN_is_symbol(tn1))
      return FALSE;
      
    tn0_uval = TN_offset(tn0);
    tn0_val = TN_offset(tn0);
    result_sym = TN_var(tn0);
    result_relocs = TN_relocs(tn0);
  }
  else
  {
    tn0_val = tn0_uval = TN_value(tn0);
  }

  if (OP_opnds(op) == 1)
  {
    tn1_val = 0;
    tn1_uval = 0;
  }
  else if (TN_is_symbol(tn1))
  {
    tn1_uval = TN_offset(tn1);
    tn1_val = TN_offset(tn1);
    result_sym = TN_var(tn1);
    result_relocs = TN_relocs(tn1);
  }
  else
  {
    tn1_val = tn1_uval = TN_value(tn1);
  }

  /* All the rest of the operations have at most two operands. */
  
  if (OP_opnds(op) > 2)
    return FALSE;

  /* Addition... */

  if (TOP_is_iadd(opcode))
  {
    result_val = tn0_val + tn1_val;
    if( TN_size(tnr) == 4 && Get_TN_Pair(tnr) == NULL )
      result_val = (INT32)result_val;
    goto Constant_Created;
  }

  if (OP_opnds(op) == 2 && !TN_is_symbol(tn1))
  {
    /* Subtraction... */

    if (opcode == TOP_sub32 || opcode == TOP_sub64 ||
	opcode == TOP_subi32 || opcode == TOP_subi64)
    {
      result_val = tn0_val - tn1_val;
      goto Constant_Created;
    }

    /* Multiplication... */

    if( opcode == TOP_imuli32 ||
	opcode == TOP_imuli64 ){
      result_val = tn0_val * tn1_val;
      if( TN_size(tnr) == 4 && Get_TN_Pair(tnr) == NULL )
	result_val = (INT32)result_val;
      goto Constant_Created;
    }
  }

  /* Logical... */
  
  if (opcode == TOP_and32 || opcode == TOP_and64 ||
      opcode == TOP_andi32 || opcode == TOP_andi64)
  {
    result_val = tn0_uval & tn1_uval;
    goto Constant_Created;
  }
  else if (opcode == TOP_or32 || opcode == TOP_or64 ||
	   opcode == TOP_ori32 || opcode == TOP_ori64)
  {
    result_val = tn0_uval | tn1_uval;
    goto Constant_Created;
  }
  else if (opcode == TOP_xor32 || opcode == TOP_xor64 ||
	   opcode == TOP_xori32 || opcode == TOP_xori64)
  {
    result_val = tn0_uval ^ tn1_uval;
    goto Constant_Created;
  }
    
  /* Shift... */

  if (opcode == TOP_shl32 || opcode == TOP_shli32 )
  {
    result_val = TRUNC_32(tn0_uval << tn1_uval);
    goto Constant_Created;
  }

  if ( opcode == TOP_shl64 || opcode == TOP_shli64)
  {
    result_val = tn0_uval << tn1_uval;
    goto Constant_Created;
  }

  else if (opcode == TOP_sar32 || opcode == TOP_sar64 ||
	   opcode == TOP_sari32 || opcode == TOP_sari64)
  {
    result_val = tn0_val >> tn1_uval;

    // Set the most significant bits according to the sign bit.  Bug 9150.
    if ((opcode == TOP_sar32 ||
	 opcode == TOP_sari32) &&
	(tn0_val & 0x80000000)) {
      result_val |= (~0 << (31 - tn1_uval)) & 0xffffffff;
    }
    goto Constant_Created;
  }
  else if (opcode == TOP_shr32 || opcode == TOP_shri32)
  {
    result_val = TRUNC_32(tn0_val) >> tn1_uval;
    goto Constant_Created;
  }
  else if (opcode == TOP_shr64 || opcode == TOP_shri64)
  {
    result_val = (UINT64)tn0_val >> tn1_uval;
    goto Constant_Created;
  }

  return FALSE;

  /* We've evaluated the expression, so replace it with the result. */

 Constant_Created:

  OPS ops = OPS_EMPTY;
  TN *tnc;

  if (result_sym != NULL)
  {
    /* Don't consider using an offset that does not fit in the LC_simm32 class.
     */
    if( !ISA_LC_Value_In_Class(result_val, LC_simm32) )
      return FALSE;

    tnc = Gen_Symbol_TN(result_sym, result_val, result_relocs);
  }
  else
  {
    const int size = Get_TN_Pair(tnr) == NULL ? TN_size(tnr) : 8;
    tnc = Gen_Literal_TN(result_val, size);
  }
  
  Expand_Immediate (tnr, tnc, OP_result_is_signed(op,0), &ops);

  /* If generating the literal requires more than one instruction,
     then just keep the original instruction. It's not clear that this
     is always the right thing, since by eliminating the instruction
     we could create dead code. */
  
  if (OP_next(OPS_first(&ops)) != NULL)
    return FALSE;

  if (EBO_in_loop)
    EBO_OPS_omega (&ops);

  BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);

  if (EBO_Trace_Optimization)
  {
    fprintf(TFile, "%sin BB:%d Redefine ",
	    EBO_trace_pfx, BB_id(OP_bb(op)));
    Print_TN(tnr,TRUE);
    fprintf(TFile," with load of ");
    Print_TN(tnc,FALSE);
    fprintf(TFile, "\n");
  }

  return TRUE;
}


static TN *_tn_swap_tmp;
static EBO_TN_INFO *_tninfo_swap_tmp;

/* If 'sll_opinfo' is an immediate logical left shift, return its
   EBO_OP_INFO and the immediate shift value in 'shift' (if 'shift' is
   non-NULL). Return NULL otherwise. */
static EBO_OP_INFO *
imm_sll (EBO_OP_INFO *sll_opinfo, INT32 *shift =NULL)
{
  if (!sll_opinfo)
    return NULL;
  
  OP *sll = sll_opinfo->in_op;
  TOP top = OP_code(sll);
  if (top != TOP_shl32 &&
      top != TOP_shl64 )
    return NULL;

  TN *imm = OP_opnd(sll, 1);
  if (!TN_has_value(imm))
    return NULL;

  if (shift)
    *shift = TN_value(imm);
  
  return sll_opinfo;
}

/* If 'sra_opinfo' is an immediate arithmetic right shift, return its
   EBO_OP_INFO and the immediate shift value in 'shift' (if 'shift' is
   non-NULL). Return NULL otherwise. */
static EBO_OP_INFO *
imm_sra (EBO_OP_INFO *sar_opinfo, INT32 *shift =NULL)
{
  if (!sar_opinfo)
    return NULL;
  
  OP *sar = sar_opinfo->in_op;
  TOP top = OP_code(sar);
  if( top != TOP_sar32 &&
      top != TOP_sar64 )
    return NULL;

  TN *imm = OP_opnd(sar, 1);
  if (!TN_has_value(imm))
    return NULL;

  if (shift)
    *shift = TN_value(imm);
  
  return sar_opinfo;
}

/* If 'srl_opinfo' is an immediate logical right shift, return its
   EBO_OP_INFO and the immediate shift value in 'shift' (if 'shift' is
   non-NULL). Return NULL otherwise. */
static EBO_OP_INFO *
imm_srl (EBO_OP_INFO *srl_opinfo, INT32 *shift =NULL)
{
  if (!srl_opinfo)
    return NULL;
  
  OP *srl = srl_opinfo->in_op;
  TOP top = OP_code(srl);
  if (top != TOP_shl32 &&
      top != TOP_shl64 )
    return NULL;

  TN *imm = OP_opnd(srl, 1);
  if (!TN_has_value(imm))
    return NULL;

  if (shift)
    *shift = TN_value(imm);
  
  return srl_opinfo;
}

/* If 'add_opinfo' is an immediate add, return its EBO_OP_INFO and the
   immediate value in 'imm' (if 'imm' is non-NULL). Return NULL
   otherwise. */
static EBO_OP_INFO *
index_add (EBO_OP_INFO *add_opinfo, TN *base, TN **index =NULL)
{
  if (!add_opinfo)
    return NULL;
  
  OP *add = add_opinfo->in_op;
  TOP top = OP_code(add);
  if (! TOP_is_iadd(top))
    return NULL;

  TN *imm_tn = OP_opnd(add, 1);
  TN *other_tn = OP_opnd(add, 0);

  if (imm_tn == base) {
    if (index)
      *index = other_tn;
  } else
    if (index)
      *index = imm_tn;
  
  return add_opinfo;
}


/* If 'and_opinfo' is an and, return its EBO_OP_INFO and the
   operand tns. Return NULL otherwise. */
static EBO_OP_INFO *
decode_and (EBO_OP_INFO *and_opinfo, TN **left, TN **right)
{
  if (!and_opinfo)
    return NULL;
  
  OP *andd = and_opinfo->in_op;
  TOP top = OP_code(andd);
  if (top != TOP_and32 &&
      top != TOP_and64)
    return NULL;

  TN *left_tn = OP_opnd(andd, 0);
  TN *right_tn = OP_opnd(andd, 1);
  if (!TN_is_register(left_tn) || !TN_is_register(right_tn))
    return NULL;

  if (left)
    *left = left_tn;
  if (right)
    *right = right_tn;
  
  return and_opinfo;
}

/*
 * CGTARG_Copy_Operand already catches most of the case we care about,
 * but there are some extra cases involving a 0 first operand and
 * we want to treat int->float and float->int register moves as copies.
 */
INT EBO_Copy_Operand (OP *op)
{
  INT opnd;

  if (OP_copy(op)) {
    TOP topcode = OP_code(op);
    if (topcode == TOP_add32 || topcode == TOP_add64 ||
        topcode == TOP_or32  || topcode == TOP_or64  ||
        topcode == TOP_mov32 || topcode== TOP_mov64 )
      return 0;    
  }

  opnd = CGTARG_Copy_Operand(op);
  if (opnd >= 0) {
    return opnd;
  }

  return -1;
}

/* Make sure 'bb' contains one or more OPs (with perhaps an
   unconditional jump) that produce a single result and that can be
   speculated. If it does, return the last OP in the sequence. Return
   the number of OPs in 'bb' (not counting any jump) in 'len'. If
   'no_define' is non-NULL, then return NULL if that TN is defined by
   any OP. If 'in_defined_tns' is non-NULL, then return NULL if any of
   those TNs are used by any OP. If 'out_defined_tns' is non-NULL,
   return the TNs defined in any OP that should not be allowed to be
   used by cmovable ops in another BB. */
static OP *
cmovable_op (BB *bb, UINT *len, TN *no_define,
	     BS *in_defined_tns, BS **out_defined_tns)
{
  *len = BB_length(bb);
  if (*len == 0)
    return NULL;
  
  OP *jump = BB_xfer_op(bb);
  if (jump && ((*len == 1) || (OP_code(jump) != TOP_jmp)))
    return NULL;

  /* Examine each OP, collecting the set of defined TNs. If any OP
     can't be speculated, return NULL. */

  work_gtn_set = GTN_SET_ClearD(work_gtn_set);

  OP *last = NULL;
  for (OP *op = BB_first_op(bb); op; op = OP_next(op))
  {
    if (OP_xfer(op))
      break;

    last = op;

    if (!CGTARG_Can_Be_Speculative(op))
      return NULL;

    for (UINT i = 0; i < OP_results(op); i++)
    {
      TN *res = OP_result(op, i);
      if (TN_is_register(res) && !TN_is_const_reg(res))
      {
	if (TN_is_global_reg(res))
	  work_gtn_set = GTN_SET_Union1D(work_gtn_set, res, work_pool);

	if (out_defined_tns)
	  *out_defined_tns = BS_Union1D(*out_defined_tns, TN_number(res), work_pool);
      }
    }

    if (in_defined_tns)
    {
      for (UINT i = 0; i < OP_opnds(op); i++)
      {
	TN *opnd = OP_opnd(op, i);
	if (TN_is_register(opnd) && BS_MemberP(in_defined_tns, TN_number(opnd)))
	  return NULL;
      }
    }
  }

  /* If 'last' defines not more than 1 TN, or if any of the TN's
     defined in non-'last' OPs are live out of the block, or any of
     the defined TN's are 'no_define'; then return NULL. */

  if (!last ||
      (OP_results(last) != 1) ||
      ((no_define != NULL) && GTN_SET_MemberP(work_gtn_set, no_define)))
    return NULL;
  
  work_gtn_set = GTN_SET_Difference1D(work_gtn_set, OP_result(last, 0));
  if (GTN_SET_IntersectsP(work_gtn_set, BB_live_out(bb)))
    return NULL;

  if (out_defined_tns)
    *out_defined_tns = BS_Difference1D(*out_defined_tns,
					   TN_number(OP_result(last, 0)));
    
  if (OP_same_res(last))
    return NULL;

  return last;
}

static BOOL is_live_tn(OP *current_op, TN *current_tn)
{
  OP *op;
  Is_True(GRA_LIVE_Phase_Invoked, ("Bad call to is_live_tn"));
  BOOL is_live = tn_has_live_def_into_BB(current_tn, OP_bb(current_op));
  BOOL past_current_op = FALSE;

  FOR_ALL_BB_OPs(OP_bb(current_op), op) {
    INT num_opnds = OP_opnds(op);
    INT num_results = OP_results(op);
    if (op == current_op) {
      past_current_op = TRUE;
      if (!is_live)
	return FALSE;
    }
    if (past_current_op) {
      for (int opndnum = 0; opndnum < num_opnds; opndnum++) {
	if (tn_registers_identical(current_tn, OP_opnd(op,opndnum)))
	  return TRUE;
      }
    }
    for (int resnum = 0; resnum < num_results; resnum++) {
      if (tn_registers_identical(current_tn, OP_result(op,resnum))) {
	if (past_current_op && !OP_cond_def(op))
	  return FALSE;
	else
	  is_live = TRUE;
      }
    }
  }
  Is_True(past_current_op && is_live, ("Bad call to is_live_tn"));
  return GTN_SET_MemberP(BB_live_out(OP_bb(current_op)), current_tn);
}


static BOOL test_is_replaced( OP* alu_op, OP* test_op, const EBO_TN_INFO* tninfo );


static BOOL move_ext_is_replaced( OP* op, const EBO_TN_INFO* tninfo )
{
  struct SIZE_EXT_INFO op_size_ext_info;
  struct SIZE_EXT_INFO pred_size_ext_info;

#if Is_True_On
  if (!(EBO_Opt_Mask & EBO_MOVE_EXT_IS_REPLACED)) return FALSE;
#endif
  OP* pred = tninfo == NULL ? NULL : tninfo->in_op;

  if( pred == NULL )
    return FALSE;

  const TOP pred_top = OP_code(pred);

  if( !TOP_is_move_ext( pred_top ) &&
      !TOP_is_load( pred_top ) ){
    return FALSE;
  }

  // Delete OP if it zero-extends a value that is already zero-extended.  For
  // example:
  //    x = movzbl ...	# zero-extend 8->32
  //  ... = movzbq x	# zero-extend 8->64
  // movzbq can be deleted because movzbl implicitly zero-extends to 64-bit.
  if (TOP_is_move_ext(OP_code(op)) &&
      (TOP_is_move_ext(pred_top) || TOP_is_load_ext(pred_top))) {
    Get_Size_Ext_Info(pred_top, &pred_size_ext_info );
    Get_Size_Ext_Info(OP_code(op), &op_size_ext_info );
    if (op_size_ext_info.dest_size >= pred_size_ext_info.dest_size &&
	op_size_ext_info.src_size >= pred_size_ext_info.src_size &&
	op_size_ext_info.sign_ext == FALSE &&
	pred_size_ext_info.sign_ext == FALSE) {
      // OK to delete OP.  Do this by inserting a copy after OP to copy OP's
      // src to OP's result.  This will make OP dead and be removed.
      OPS ops = OPS_EMPTY;
      Exp_COPY(OP_result(op, 0), OP_opnd(op, 0), &ops);
      BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
    }
  }

  if( TOP_is_load( pred_top ) &&
      OP_find_opnd_use( pred, OU_index ) >= 0 )
    return FALSE;

  Get_Size_Ext_Info( pred_top, &pred_size_ext_info );
  Get_Size_Ext_Info( OP_code(op), &op_size_ext_info );

  if( pred_size_ext_info.sign_ext &&
      !op_size_ext_info.sign_ext ){
    return FALSE;
  }

  //if( pred_size_ext_info.dest_size != op_size_ext_info.src_size )    return FALSE;

  OP* new_op = NULL;
  OPS ops = OPS_EMPTY;

  if( TOP_is_move_ext( pred_top ) ){
    EBO_TN_INFO* opnd_info = get_tn_info( OP_opnd(pred,0) );
    if( opnd_info != NULL && opnd_info->sequence_num > tninfo->sequence_num )
      return FALSE;

    const TYPE_ID mtype = op_size_ext_info.dest_size == 4 ? MTYPE_I4 : MTYPE_I8;
    
    /* bug#1711
       Now we only got one situation that <pred> performs zero ext, and
       <op> performs sign ext.
    */
    const bool sign_ext =
      ( op_size_ext_info.src_size <= pred_size_ext_info.src_size ) ?
      op_size_ext_info.sign_ext : pred_size_ext_info.sign_ext;
    // bug 6871 - choose the min size to sign-extend or zero-extend.
    const INT size = 
      ( op_size_ext_info.src_size <= pred_size_ext_info.src_size ) ?
      op_size_ext_info.src_size : pred_size_ext_info.src_size;

    Expand_Convert_Length( OP_result( op, 0 ), OP_opnd( pred, 0 ),
			   Gen_Literal_TN( 8*size, 4 ),
			   mtype,
			   sign_ext,
			   &ops );

    new_op = OPS_first( &ops );

    if( ( OP_code(new_op) == OP_code(op) ) &&
	TNs_Are_Equivalent( OP_result(pred,0), OP_opnd(pred,0) ) )
      return FALSE;


  } else if( TOP_is_load( pred_top ) ){
    return FALSE;

    if( !TNs_Are_Equivalent( OP_result(op,0), OP_opnd(op,0) ) )
      return FALSE;

    if( OP_find_opnd_use( pred, OU_index ) >= 0 )
      return FALSE;

    /* Make sure the result of <pred> is used only once. */
    EBO_TN_INFO* opnd_info = get_tn_info( OP_result(pred,0) );
    if( opnd_info->reference_count > 1 )
      return FALSE;    

    /* Make sure the base ptr is not over-written. */
    opnd_info = get_tn_info( OP_opnd( pred, 0 ) );
    if( opnd_info != NULL && opnd_info->sequence_num > tninfo->sequence_num )
      return FALSE;

    EBO_OP_INFO* pred_opinfo = locate_opinfo_entry((EBO_TN_INFO*)tninfo);
    FmtAssert( pred_opinfo->in_op == pred, ("move_ext_is_replaced: NYI (1)") );

    TOP new_top = TOP_UNDEFINED;

    if( pred_size_ext_info.src_size == 1 ){
      if( op_size_ext_info.dest_size == 4 )
	new_top = pred_size_ext_info.sign_ext ? TOP_ld8_32 : TOP_ldu8_32;
      else if( op_size_ext_info.dest_size == 8 )
	new_top = pred_size_ext_info.sign_ext ? TOP_ld8_64 : TOP_ldu8_64;

    } else if( pred_size_ext_info.src_size == 2 ){
      if( op_size_ext_info.dest_size == 4 )
	new_top = pred_size_ext_info.sign_ext ? TOP_ld16_32 : TOP_ldu16_32;
      else if( op_size_ext_info.dest_size == 8 )
	new_top = pred_size_ext_info.sign_ext ? TOP_ld16_64 : TOP_ldu16_64;

    } else if( pred_size_ext_info.src_size == 4 ){
      if( op_size_ext_info.dest_size == 8 ){
	FmtAssert( op_size_ext_info.sign_ext,("move_ext_is_replaced: NYI (2)"));
	new_top = TOP_ld32_64;
      }
    }

    Is_True( new_top != TOP_UNDEFINED, ("move_ext_is_replaced: NYI (3)") );

    new_op = Mk_OP( new_top,
		    OP_result( op, 0 ),
		    OP_opnd( pred, 0 ),
		    OP_opnd( pred, 1 ) );

    remove_op( pred_opinfo );
    OP_Change_To_Noop( pred );
  }

  if( new_op == NULL )
    return FALSE;

  // open64.net bug949. When we do OP changes, we don't
  // miss the check for movext ops on a byte register.
  // If the byte register is not addressable under -m32,
  // We cancel this change.

  if ( Is_Target_32bit() &&
       EBO_in_peep && 
       TOP_is_move_ext( OP_code(new_op ))) {

    const TOP check_top = OP_code(new_op);
    if (check_top == TOP_movsbl ||
        check_top == TOP_movzbl ||
        check_top == TOP_movsbq ||
        check_top == TOP_movzbq ) {
      const REGISTER reg = TN_register(OP_opnd(new_op, 0));
      const REGISTER_SET regs =
        REGISTER_SUBCLASS_members(ISA_REGISTER_SUBCLASS_m32_8bit_regs);
    
    if( !REGISTER_SET_MemberP( regs, reg ) )
      return FALSE;
    }
  }


  Set_OP_unrolling( new_op, OP_unrolling(op) );
  Set_OP_orig_idx( new_op, OP_map_idx(op) );
  Set_OP_unroll_bb( new_op, OP_unroll_bb(op) );
	
  Copy_WN_For_Memory_OP( new_op, pred );
  if ( OP_volatile( pred ) )
    Set_OP_volatile( new_op );
  OP_srcpos( new_op ) = OP_srcpos( op );
  BB_Insert_Op_After( OP_bb(op), op, new_op );

  if( EBO_Trace_Data_Flow ){
#pragma mips_frequency_hint NEVER
    fprintf( TFile, "Special_Sequence merges " );
    Print_OP_No_SrcLine( pred );
    fprintf( TFile, "                   with   " );
    Print_OP_No_SrcLine( op );
    
    fprintf( TFile, "                   new op " );
    Print_OP_No_SrcLine( new_op );
  }

  return TRUE;
}
static inline TN* OP_opnd_use( OP* op, ISA_OPERAND_USE use );

BOOL Delete_Unwanted_Prefetches ( OP* op )
{
#if Is_True_On
  if (!(EBO_Opt_Mask & EBO_DELETE_UNWANTED_PREFETCHES)) return FALSE;
#endif
  WN *mem_wn = Get_WN_From_Memory_OP(op);
  TN* base;

  OP *incr = NULL;
  OP *as_opnd = NULL;
  OP *as_result = NULL;
  OP *leaxx = NULL;
  OP *load_store = NULL;
  BOOL sib = FALSE;
  BB* bb = OP_bb( op );
  OP *next = BB_first_op( bb );

  INT prefetch_offset = WN_offset(mem_wn);

#ifdef KEY
  // bug 10953: LNO tells me this prefetch must be kept
  if(PF_GET_KEEP_ANYWAY(WN_prefetch_flag(mem_wn)))
   return FALSE;
#endif  
  if (OP_find_opnd_use( op, OU_base ) >= 0 && 
      // the prefetch instruction has passed a call of this function, so pass it.
      Get_Top_For_Addr_Mode(OP_code(op), BASE_MODE) == OP_code(op))
    base = OP_opnd( op, OP_find_opnd_use( op, OU_base ));
  else
    return FALSE; // Can not analyze further; make safe assumption.
   
  while (next && !incr) {
    
    if ((OP_code(next) == TOP_addi32 || OP_code(next) == TOP_addi64)) {
      if (OP_results(next) != 0 && OP_result(next, 0) == base && 
	  OP_opnd(next, 0) == base)
	incr = next;
      else if (OP_results(next) != 0 && OP_result(next, 0) == base)
	as_result = next;
      else if (OP_opnd(next, 0) == base)
	as_opnd = next;
    } else if ((OP_code(next) == TOP_leax32 || OP_code(next) == TOP_leax64) 
		    && OP_result(next, 0) == base)
	    leaxx = next;
    
    next = OP_next(next);
  }
  
  INT delta_base;
  if (!incr) {
    if (!as_result && !as_opnd && !leaxx)
      return TRUE;
    else if (leaxx)
    {  // further analyze the two terms for RPR
      TN* term;
      term = OP_opnd_use(leaxx, OU_index);

      OP *w_incr;
        for (w_incr = BB_first_op(bb); w_incr != NULL; w_incr = OP_next(w_incr))
	{
          if (((OP_code(w_incr) == TOP_addi32 || OP_code(w_incr) == TOP_addi64)) &&
            (OP_results(w_incr) != 0 && OP_result(w_incr, 0) == term && 
             OP_opnd(w_incr, 0) == term))
	  break;
	}
      if (w_incr != NULL){
	sib = TRUE;
        delta_base = TN_value(OP_opnd(w_incr,1)) * (TN_value(OP_opnd_use(leaxx,OU_scale)));
      } else
        return TRUE;
    } else if (as_result)
	    incr = as_result;
    else 
      incr = as_opnd;
  }
  
  if (!sib) 
    delta_base = TN_value(OP_opnd(incr, 1));

  next = BB_first_op( bb );
  while (next && !load_store) {
    if ((OP_memory(next) || OP_load_exe(next)) &&
	OP_find_opnd_use( next, OU_base ) >= 0 &&
	base == OP_opnd( next, OP_find_opnd_use( next, OU_base ))) {
      INT load_store_offset = 
	TN_value(OP_opnd( next, OP_find_opnd_use( next, OU_offset )));
      INT prefetch_ahead = LNO_Prefetch_Ahead;
      INT Cache_Line_Size = 64 /* bytes */;
      INT leeway = 3; // some ops may be moved around by EBO and scheduler.
      if ((delta_base - prefetch_offset +load_store_offset) < 
	  (Cache_Line_Size*(prefetch_ahead+ leeway)))
	load_store = next;
    }
    
    next = OP_next(next);
  }
  
  if (!load_store)
    return TRUE;

  return FALSE;
}


// define X8664 status flag register marco.
// the expression SF==OF is treated as a sigle status flag register.
// because SF==OF is used in jl/jge
// some transformation may affect SF or OF, but not change SF==OF
#define OF_MASK   0x00000001
#define SF_MASK   0x00000002
#define ZF_MASK   0x00000004
#define AF_MASK   0x00000008
#define CF_MASK   0x00000010
#define PF_MASK   0x00000020
#define SF_EQ_OF_MASK   0x00000040
#define ALL_FLAG_MASK   0x0000005f

/*
 * Test_Compare_Size
 *    return test/compare instruction's compare operand size.
 */
static 
INT32 Test_Compare_Size( const TOP top )
{
  switch ( top ) {
    case TOP_test8:
    case TOP_testx8:
    case TOP_testxx8:
    case TOP_testxxx8:
    case TOP_testi8:
    case TOP_cmp8:
    case TOP_cmpx8:
    case TOP_cmpxx8:
    case TOP_cmpxxx8:
    case TOP_cmpi8:
    case TOP_cmpxi8:
    case TOP_cmpxxi8:
    case TOP_cmpxxxi8:
      return 1;
    case TOP_test16:
    case TOP_testx16:
    case TOP_testxx16:
    case TOP_testxxx16:
    case TOP_testi16:
    case TOP_cmp16:
    case TOP_cmpx16:
    case TOP_cmpxx16:
    case TOP_cmpxxx16:
    case TOP_cmpi16:
    case TOP_cmpxi16:
    case TOP_cmpxxi16:
    case TOP_cmpxxxi16:
      return 2;
    case TOP_test32:
    case TOP_testx32:
    case TOP_testxx32:
    case TOP_testxxx32:
    case TOP_testi32:
    case TOP_cmp32:
    case TOP_cmpx32:
    case TOP_cmpxx32:
    case TOP_cmpxxx32:
    case TOP_cmpi32:
    case TOP_cmpxi32:
    case TOP_cmpxxi32:
    case TOP_cmpxxxi32:
      return 4;
    case TOP_test64:
    case TOP_testx64:
    case TOP_testxx64:
    case TOP_testxxx64:
    case TOP_testi64:
    case TOP_cmp64:
    case TOP_cmpx64:
    case TOP_cmpxx64:
    case TOP_cmpxxx64:
    case TOP_cmpi64:
    case TOP_cmpxi64:
    case TOP_cmpxxi64:
    case TOP_cmpxxxi64:
      return 8;
  }
  Is_True (FALSE, ("unexpected op\n"));
  return 0;
}

/*
 * Read_Flags_Mask
 *    return which status regsiter flag is read by top instruction.
 * For unkonwed instruction read rflags,just assume read all rflags
 */
static 
INT32 Read_Flags_Mask ( const TOP top )
{
  Is_True ( TOP_is_read_rflags ( top ), (" unxpected op \n") ) ;
  switch ( top ) {
    case TOP_jb:
    case TOP_jae:
    case TOP_cmovb:
    case TOP_cmovae:
    case TOP_fcmovb:
    case TOP_fcmovnb:
      return CF_MASK;
    case TOP_jp:
    case TOP_jnp:
    case TOP_cmovp:
    case TOP_cmovnp:
    case TOP_fcmovu:
    case TOP_fcmovnu:
      return PF_MASK;
    case TOP_je:
    case TOP_jne:
    case TOP_cmove:
    case TOP_cmovne:
    case TOP_fcmove:
    case TOP_fcmovne:
      return ZF_MASK;
    case TOP_jbe:
    case TOP_ja:
    case TOP_cmovbe:
    case TOP_cmova:
    case TOP_fcmovbe:
    case TOP_fcmovnbe:
      return CF_MASK | ZF_MASK;
    case TOP_jl:
    case TOP_jge:
    case TOP_cmovl:
    case TOP_cmovge:
      return SF_EQ_OF_MASK;
    case TOP_jle:
    case TOP_jg:
    case TOP_cmovle:
    case TOP_cmovg:
      return SF_EQ_OF_MASK | ZF_MASK;
    case TOP_js:
    case TOP_jns:
    case TOP_cmovs:
    case TOP_cmovns:
      return SF_MASK;
  }
  return ALL_FLAG_MASK;
}

/*
 * table for indicate which status flag register will be affected by 
 * mov-ext/load-ext opt.
 */

#define TEST_IDX(same_src_size, sign0, sign1)   \
          ( ( ( ( same_src_size ) & 1 ) << 2 ) | \
            ( ( ( sign0 ) & 1 ) << 1)  | \
            ( ( sign1 ) & 1 ) )

static INT32 Test_Updated_Flags[8] = 
     { // list 8 combinations for BOOL vlaue same_src_size, sign0, sign1
       // the first case is test's two srouce are same size movzbl
       SF_MASK,
       SF_MASK,
       SF_MASK,
       0,
       SF_MASK,
       SF_MASK | ZF_MASK, // exception is movzbl + movswl change to testb, only affect SF
       SF_MASK | ZF_MASK, // however movzwl+movsbl change to testb, affect SF and ZF
       SF_MASK | ZF_MASK,
     };

#define TESTI_IDX(sign, tmsb,  smsb, in)   \
          (( ( ( sign ) & 1 ) << 3 ) | \
            ( ( ( tmsb ) & 1 ) << 2 ) | \
            ( ( ( smsb ) & 1 ) << 1)  | \
            ( ( in ) & 1 ) )
// ZF,SF,PF
static INT32 TestI_Updated_Flags[16] = 
     { // sign, constant  test msb, src_size msb, in range
       // in range means [0,255], [0,65535]
       0,             // 0, 0, 0, 0
       0,             // 0, 0, 0, 1
       SF_MASK, // 0, 0, 1, 0
       SF_MASK, // 0, 0, 1, 1
       0,             // 0, 1, 0, 0
       0,             // 0, 1, 0, 1
       SF_MASK, // 0, 1, 1, 0
       SF_MASK, // 0, 1, 1, 1
       
       ZF_MASK, // 1, 0, 0, 0
       0,             // 1, 0, 0, 1
       SF_MASK|ZF_MASK, // 1, 0, 1, 0
       SF_MASK, // 1, 0, 1, 1
       
       SF_MASK|ZF_MASK,             // 1, 1, 0, 0
       SF_MASK,             // 1, 1, 0, 1
       ZF_MASK, // 1, 1, 1, 0
       0, // 1, 1, 1, 1
     };


static INT32 Compare_Updated_Flags[8] = 
     { // list 8 combinations for BOOL vlaue same_src_size, sign0, sign1
       SF_MASK | OF_MASK | SF_EQ_OF_MASK,
       ZF_MASK | SF_MASK | CF_MASK | OF_MASK | SF_EQ_OF_MASK,
       ZF_MASK | SF_MASK | CF_MASK | OF_MASK | SF_EQ_OF_MASK,
       OF_MASK | SF_MASK,
       ZF_MASK | SF_MASK | CF_MASK | OF_MASK | SF_EQ_OF_MASK,
       ZF_MASK | SF_MASK | CF_MASK | OF_MASK | SF_EQ_OF_MASK,
       ZF_MASK | SF_MASK | CF_MASK | OF_MASK | SF_EQ_OF_MASK,
       ZF_MASK | SF_MASK | CF_MASK | OF_MASK | SF_EQ_OF_MASK,
     };


/*
 * if src's in op is really a mov-ext, get extend info.
 * if not, assume, there is a movll or movqq instruction for src.
 *   this can unfiy two src.
 */
static 
TN* Test_Get_Size_Ext_Info ( const OP* mov_op, SIZE_EXT_INFO* info,
                             INT32 test_size, TN* src)
{
  if ( mov_op && TOP_is_move_ext ( OP_code( mov_op ) ) ) {
    Get_Size_Ext_Info ( OP_code ( mov_op ), info );
    return OP_opnd ( mov_op, 0 );
  }
  else {
    info->src_size = test_size;
    info->dest_size = test_size;
    info->sign_ext = FALSE;
    return src;
  }
}

/*
 * suppose a constant is used in cmpi/testi, treat it as a mov extend to 
 * unfiy the processing.
 * For example:
 * movzbl %ax, %eax
 * cmpl $1, %eax
 *
 * treat $1 is movzbl $1
 */
static
void Get_Constant_Size_Ext_Info ( INT64 val, SIZE_EXT_INFO* info,
                                  INT32 test_size )
{
  info->dest_size = test_size;
  if ( val >= 0 ) {
    info->sign_ext = FALSE;
    if ( val <= 0xff ) {
      info->src_size = 1;
    }
    else if ( val <= 0xffff ) {
      info->src_size = 2;
    }
    else if ( val <= 0xffffffffLL ) {
      info->src_size = 4;
    }
    else {
      info->src_size = 8;
    }
  }
  else {
    info->sign_ext = TRUE;
    if ( val >= -128 ) {
      info->src_size = 1;
    }
    else if( val >= -65536 ) {
      info->src_size = 2;
    }
    else if ( val >= 0xffffffff80000000LL ) {
      info->src_size = 4;
    }
    else {
      info->src_size = 8;
    }
  }
}

/*
 * check if a TN (after register allocation)'s def at def_op
 * can live to current op.
 * This is used to check if def_op's source is stiall valid
 * form def_op to current_op
 */
static
BOOL is_movext_source_tn_valid(OP *current_op, OP* mov_ext_op, TN *tn)
{
  // Now the condition is strict.
  // mov_op and current_op is same BB.
  INT num_results;
  
  Is_True ( TOP_is_move_ext ( OP_code( mov_ext_op ) ), 
            ("expect mov_ext op\n") );

  if( OP_bb ( current_op ) != OP_bb ( mov_ext_op ) )  {
    return FALSE;
  }

  OP *op = OP_next ( mov_ext_op );
  while ( op ) {
    if ( op == current_op ) {
      return TRUE;
    }
    num_results = OP_results ( op );
      
    for ( int resnum = 0; resnum < num_results; resnum++ ) {
      if ( tn_registers_identical ( tn, OP_result ( op, resnum ) ) ) {
        return FALSE;
      }
    }
      
    op = OP_next ( op );
  }
  
  return FALSE;
}

/*
 * ICMP_Is_Replaced
 *  Replace mov-ext/load-ext + test/compare with more precious instruction
 *  Expect remove mov-ext and load-ext instruction.
 *  When mov-ext is not removed, it seems no harm.
 *  When load-ext is not revmoed, maybe introduce more memory load.
 *    Maybe need provide a function to load_exectue to tell if load-ext combine
 *    is legal or not.
 *    Or borrow some logical from load_exectue opt.
 */
extern INT32 Current_PU_Count();
static 
BOOL ICMP_Is_Replaced ( OP *op, TN **opnd_tn, EBO_TN_INFO **opnd_tninfo )
{ 
  const TOP top = OP_code( op );
  Is_True( TOP_is_icmp ( top ), ( "unexpected opcode" ) );
  TN* src0 = opnd_tn[0];
  TN* src1 = opnd_tn[1];
  EBO_TN_INFO *tninfo0 = opnd_tninfo[0];
  EBO_TN_INFO *tninfo1 = opnd_tninfo[1];

  // 1. op must be test or comapre
  //    category test/compare into test/testi/compare/comparei
  BOOL is_test = FALSE;
  BOOL is_testi = FALSE;
  BOOL is_cmp = FALSE;
  BOOL is_cmpi = FALSE;
  switch ( top ) {
    case TOP_test8:
    case TOP_test16:
    case TOP_test32:
    case TOP_test64:
      if ( TN_has_value(src1) )
        is_testi = TRUE;
      else if ( TN_has_value(src0) ) {
        is_testi = TRUE;
        // interchange src0, src1
        src0 = opnd_tn[1];
        src1 = opnd_tn[0];
        tninfo0 = opnd_tninfo[1];
        tninfo1 = opnd_tninfo[0];
      }
      else 
        is_test = TRUE;
      break;
    case TOP_testi8:
    case TOP_testi16:
    case TOP_testi32:
    case TOP_testi64:
      is_testi = TRUE;
      break;
    case TOP_cmp8:
    case TOP_cmp16:
    case TOP_cmp32:
    case TOP_cmp64:
      if ( TN_has_value(src1) )
        is_cmpi = TRUE;
      else if ( TN_has_value(src0) ) 
        return FALSE;
      else 
        is_cmp = TRUE;
      break;
    case TOP_cmpi8:
    case TOP_cmpi16:
    case TOP_cmpi32:
    case TOP_cmpi64:
      is_cmpi = TRUE;
      break;
    default:
      return FALSE;
  }

  // 2. test's src register's in_op is mov-ext or load-ext
  //    compare's src register are mov-ext or load-ext
  // 
  //    here genreate change_mask indicate which status flag register
  //    will be changed if test/compare is changed.
  INT32 change_mask = 0;
  OP* mov_ext0 = NULL;
  OP* mov_ext1 = NULL;
  INT32 test_size = Test_Compare_Size( top );
  struct SIZE_EXT_INFO ext_info0;
  struct SIZE_EXT_INFO ext_info1;
  TN* new_src0;     // source in new op
  TN* new_src1;
  
  if ( is_test ) {
    mov_ext0 = tninfo0 == NULL ? NULL : tninfo0->in_op;
    if ( TNs_Are_Equivalent ( src0, src1 ) ) {
      mov_ext1 = mov_ext0;
    }
    else {
      mov_ext1 = tninfo1 == NULL ? NULL : tninfo1->in_op;
    }

    if (mov_ext0 == NULL && mov_ext1 == NULL)
      return FALSE;

    new_src0 = Test_Get_Size_Ext_Info ( mov_ext0, &ext_info0, test_size, src0 );
    new_src1 = Test_Get_Size_Ext_Info ( mov_ext1, &ext_info1, test_size, src1 );

    if ( ext_info0.src_size >= test_size && ext_info1.src_size >= test_size )
        return FALSE;

    if ( ext_info0.dest_size != test_size || ext_info1.dest_size != test_size )
       return FALSE;

    // special case for movzbl, movswl as input
    // unsgined mov-ext has smaller src_size
    if ( ext_info0.sign_ext != ext_info1.sign_ext &&
        ( ( !ext_info0.sign_ext && ext_info0.src_size < ext_info1.src_size ) ||
          ( !ext_info1.sign_ext && ext_info1.src_size < ext_info0.src_size ) ) ) {
       change_mask = SF_MASK;
    }
    // movsbl, movswl, can chagne to testw, not change movsbl's dest TN
    else if ( ext_info0.sign_ext &&
              ext_info1.sign_ext &&
              ext_info0.src_size != ext_info1.src_size &&
              ext_info0.src_size < test_size &&
              ext_info1.src_size < test_size ) {
       if (ext_info0.src_size < ext_info1.src_size) {
         new_src0 = Test_Get_Size_Ext_Info ( NULL, &ext_info0, test_size, src0 );
       }
       else {
         new_src1 = Test_Get_Size_Ext_Info ( NULL, &ext_info1, test_size, src1 );
       }
       change_mask = SF_MASK;
    }
    else {
      INT32 idx = TEST_IDX ( ext_info0.src_size != ext_info1.src_size,
        ext_info0.sign_ext, ext_info1.sign_ext );
     
      change_mask = Test_Updated_Flags[idx];
    }
    
  }
  else if ( is_testi ) {
    Is_True ( TN_has_value ( src1 ), ("testi src1 expect to be constant\n") );
    INT64 val = TN_value ( src1 );
    mov_ext0 = tninfo0 == NULL ? NULL : tninfo0->in_op;
    if(mov_ext0 == NULL)
      return FALSE;
    new_src0 = Test_Get_Size_Ext_Info ( mov_ext0, &ext_info0, test_size, src0 );
    if ( ext_info0.src_size >= test_size || ext_info0.dest_size != test_size )
      return FALSE;

     // sign, constant  test msb, src_size msb, in range
     INT test_msb =  ( val >> ( test_size * 8 -1 ) ) & 0x1;
     INT src_msb =   ( val >> ( ext_info0.src_size * 8 - 1 ) ) & 0x1;
     BOOL inrange = val & ( ~ ( ( 1 << ext_info0.src_size ) - 1 ) ) == 0;
     INT32 idx = TESTI_IDX ( ext_info0.sign_ext, test_msb, src_msb, inrange );
     change_mask = TestI_Updated_Flags[idx];
     new_src1 = src1;
  }
  else if ( is_cmp ) {
    mov_ext0 = tninfo0 == NULL ? NULL : tninfo0->in_op;
    mov_ext1 = tninfo1 == NULL ? NULL : tninfo1->in_op;
    if ( mov_ext0 == NULL && mov_ext1 == NULL )
      return FALSE;
    new_src0 = Test_Get_Size_Ext_Info ( mov_ext0, &ext_info0, test_size, src0 );
    new_src1 = Test_Get_Size_Ext_Info ( mov_ext1, &ext_info1, test_size, src1 );

    if ( ext_info0.src_size >= test_size && ext_info1.src_size >= test_size )
        return FALSE;

    if ( ext_info0.dest_size != test_size || ext_info1.dest_size != test_size )
       return FALSE;

    INT32 idx = TEST_IDX ( ext_info0.src_size != ext_info1.src_size,
        ext_info0.sign_ext, ext_info1.sign_ext );
     
    change_mask = Compare_Updated_Flags[idx];
  }
  else if ( is_cmpi ) {
    Is_True ( TN_has_value(src1), ("cmpi src1 expect to be constant\n") );
    INT64 val = TN_value ( src1 );
    mov_ext0 = tninfo0 == NULL ? NULL : tninfo0->in_op;
    if ( mov_ext0 == NULL )
      return FALSE;
    new_src0 = Test_Get_Size_Ext_Info ( mov_ext0, &ext_info0, test_size, src0 );
    if ( ext_info0.src_size >= test_size || ext_info0.dest_size != test_size )
      return FALSE;

    // consider constant as movzbl or movsbl and use Compare_Updated_Flags
    new_src1 = src1;
    Get_Constant_Size_Ext_Info ( val, &ext_info1, test_size );
    if ( ext_info1.src_size >= test_size )
      return FALSE;
    if ( ext_info1.src_size < ext_info0.src_size )
      ext_info1.src_size = ext_info0.src_size;
    INT32 idx = TEST_IDX ( ext_info0.src_size != ext_info1.src_size,
        ext_info0.sign_ext, ext_info1.sign_ext );
     
    change_mask = Compare_Updated_Flags[idx];
  }
  else {
    return FALSE;
  }

  // this means all flags is changed.
  if ( change_mask == ALL_FLAG_MASK )
    return FALSE;
  
  // 3. gather which status flag used by jump or cmov
  //    search ops after test/comapre, if find op update rflags, abort
  //    Determin if its valid to perform transform
  if ( change_mask != 0 ) {
    OP* next_op = OP_next ( op );
    while ( next_op ) {
      TOP next_top = OP_code ( next_op );

      if ( TOP_is_read_rflags ( next_top ) ) {
        INT32 read_flags = Read_Flags_Mask ( next_top );
        if ( read_flags & change_mask )
          return FALSE;
      }

      if ( TOP_is_change_rflags ( next_top ) ) {
        break;
      }
      next_op = OP_next(next_op);
    }
  }
  // check if mov-ext's result TN will live out of BB.
  // like
  // char a
  // if ( a > 10 )
  //    use a;
  BOOL find_opt = FALSE;
  if ( mov_ext0 && new_src0 != src0 && 
       !TN_live_out_of( OP_result(mov_ext0 , 0 ), OP_bb ( op ) ) ) {
    find_opt = TRUE;
  }
  if ( mov_ext1 && new_src1 != src1 && 
       !TN_live_out_of( OP_result(mov_ext1 , 0 ), OP_bb ( op ) ) ) {
    find_opt = TRUE;
  }
  if ( find_opt == FALSE )
    return FALSE;


  // 4. check TN register is not modified through definition to usage.
  //    like movzbl %al, %esi
  //         movzbl %bx, %eax
  //         testl %esi, %eax
  //    it can't change to 
  //         testb %al, %bx
  //    check if %al is modified in later code sequence.
  if ( mov_ext0 && new_src0 != src0 &&
       !is_movext_source_tn_valid ( op, mov_ext0, new_src0 ) ) {
    return FALSE;
  }
  if ( mov_ext1 && new_src1 != src1 &&
       !is_movext_source_tn_valid ( op, mov_ext1, new_src1 ) ) {
    return FALSE;
  } 
 
  // 4. transform here
  TOP new_top;
  if ( is_test ) {
    INT32 new_size = MIN ( ext_info0.src_size, ext_info1.src_size );
    if(new_size == 1)
        new_top = TOP_test8;
    else if(new_size == 2)
        new_top = TOP_test16;
    else if(new_size == 4)
        new_top = TOP_test32;
    else
        return FALSE;
  }
  else if ( is_testi ) {
    INT32 new_size = ext_info0.src_size;
    INT64 new_val = TN_Value ( new_src1 );
    // truncate value to avoid warning in assembler
    if(new_size == 1) {
        new_val &= 0xff;
        new_top = TOP_testi8;
    }
    else if(new_size == 2) {
        new_val &= 0xffff;
        new_top = TOP_testi16;
    }
    else if(new_size == 4) {
        new_val &= 0xffffffff;
        new_top = TOP_testi32;
    }
    else
        return FALSE;
    new_src1 = Dup_TN ( new_src1 );
    Set_TN_size ( new_src1, new_size );
    Set_TN_value ( new_src1, new_val );
  }
  else if ( is_cmp ) {
    INT32 new_size = MIN ( ext_info0.src_size, ext_info1.src_size );
    if ( new_size == 1 )
        new_top = TOP_cmp8;
    else if(new_size == 2)
        new_top = TOP_cmp16;
    else if(new_size == 4)
        new_top = TOP_cmp32;
    else
        return FALSE;
  }
  else if ( is_cmpi ) {
    INT32 new_size = ext_info0.src_size;
    INT64 new_val = TN_Value ( new_src1 );
    // if value is unsigned, truncate
    // if value is signed, truncate and sign ext.
    if ( new_size == 1 ) {
      if ( ext_info0.sign_ext )
        new_val =  ( new_val << ( sizeof(INT64) * 8 - 8 ) ) >> ( sizeof(INT64) * 8 - 8 );
      else
        new_val &= 0xff;
      new_top = TOP_cmpi8;
    }
    else if ( new_size == 2 ) {
      if(ext_info0.sign_ext)
        new_val =  ( new_val << ( sizeof(INT64) * 8 - 16 ) ) >> ( sizeof(INT64) * 8 - 16 );
      else
        new_val &= 0xffff;
      new_top = TOP_cmpi16;
    }
    else if ( new_size == 4 ) {
      if(ext_info0.sign_ext)
        new_val =  ( new_val << ( sizeof(INT64) * 8 - 32 ) ) >> ( sizeof(INT64) * 8 - 32 );
      else
        new_val &= 0xffffffff;
      new_top = TOP_cmpi32;
    }
    else
        return FALSE;
    new_src1 = Dup_TN ( new_src1 );
    Set_TN_size ( new_src1, new_size );
    Set_TN_value ( new_src1, new_val );
  }
  else {
    return FALSE;
  }

  // open64.net bug941. under m32, at ebo post process phase
  // if the new_top is set to 8bit operator and the GPR is not 8byte addressable 
  // we disable the change of op.
  if ( Is_Target_32bit() &&
       EBO_in_peep &&
       ( new_top == TOP_cmpi8 ||
         new_top == TOP_test8 || 
         new_top == TOP_testi8 ||
         new_top == TOP_cmp8)) {
    const REGISTER_SET regs = REGISTER_SUBCLASS_members(ISA_REGISTER_SUBCLASS_m32_8bit_regs);
    if ( TN_is_register(new_src0) && !REGISTER_SET_MemberP(regs, TN_register(new_src0) )) 
        return FALSE;
    if ( TN_is_register(new_src1) && !REGISTER_SET_MemberP(regs, TN_register(new_src1) ))
        return FALSE;
  }         

  OP* new_op = Mk_OP( new_top, Rflags_TN(), new_src0, new_src1 );
  Set_OP_unrolling( new_op, OP_unrolling ( op ) );
  Set_OP_orig_idx( new_op, OP_map_idx ( op ) );
  Set_OP_unroll_bb( new_op, OP_unroll_bb ( op ) );
  
  Copy_WN_For_Memory_OP( new_op, op );
  if ( OP_volatile( op ) )
     Set_OP_volatile( op );
  OP_srcpos( new_op ) = OP_srcpos( op );
  BB_Insert_Op_After( OP_bb(op), op, new_op );
  return TRUE;
}


extern void dump_bb (BB *bb);
extern void dump_tn (TN *tn);

/*
 * Look at an expression and it's inputs to identify special sequences
 * that can be simplified.
 */
BOOL Special_Sequence( OP *op, TN **opnd_tn, EBO_TN_INFO **opnd_tninfo )
{  
#if Is_True_On
  if (!(EBO_Opt_Mask & EBO_SPECIAL_SEQUENCE)) return FALSE;
#endif
  const TOP top = OP_code( op );

  if( top == TOP_lea64  ||
      top == TOP_leax64 ||
      top == TOP_leaxx64 ){
    if( EBO_Merge_Memory_Addr( op, opnd_tn, opnd_tninfo, opnd_tninfo ) ){
      return TRUE;
    }
  }

  if( OP_idiv(op) ){
    /* Convert
       rax rdx :- idiv rax rdx rax
       to
       rax :- 1
       rdx :- 0
    */
    const TN* divisor = OP_opnd( op, 2 );
    const TN* dividend = OP_opnd( op, 0 );

    if( divisor == dividend ){
      TN* quotient = OP_result( op, 0 );
      TN* remainder = OP_result( op , 1 );

      /* x / x = 1 */
      OPS ops = OPS_EMPTY;
      Expand_Immediate( quotient,  Gen_Literal_TN( 1, 4 ), false, &ops );
      Expand_Immediate( remainder, Gen_Literal_TN( 0, 4 ), false, &ops );
      BB_Insert_Ops_After( OP_bb(op), op, &ops );

      OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
      OP_srcpos(OPS_last(&ops)) = OP_srcpos(op);

      if( EBO_Trace_Data_Flow ){
	fprintf( TFile, "Special_Sequence replaces " );
	Print_OP_No_SrcLine(op);
	fprintf( TFile, "with " );
	Print_OPS_No_SrcLines( (const OPS*)&ops);
      }

      return TRUE;
    }
  }

  /* Folding: TOP_movsbq = TOP_movsbl + TOP_movslq
   */
  if( TOP_is_move_ext( top ) ){
    if( move_ext_is_replaced( op, opnd_tninfo[0] ) )
      return TRUE;
  }

  if ( CG_Movext_ICMP && TOP_is_icmp ( top ) ) {
    if ( ICMP_Is_Replaced ( op, opnd_tn, opnd_tninfo ) )
      return TRUE;
  }

  if( ( top == TOP_test8 || top == TOP_test16 ||
        top == TOP_test32 || top == TOP_test64 ) &&
      TNs_Are_Equivalent( OP_opnd(op,0), OP_opnd(op,1) ) ){

    const EBO_TN_INFO* tninfo = opnd_tninfo[0];
    OP* alu_op = tninfo == NULL ? NULL : tninfo->in_op;

    if( test_is_replaced( alu_op, op, tninfo ) )
      return TRUE;

  } else if ( top == TOP_movslq ) {
    const EBO_TN_INFO* tninfo = opnd_tninfo[0];
    OP* alu_op = tninfo == NULL ? NULL : tninfo->in_op;

    if (alu_op == NULL || alu_op->bb != op->bb)
      return FALSE;

    if (op->next != NULL &&
	is_live_tn(op->next, OP_result(alu_op, 0))) 
      return FALSE;
    else if (op->next == NULL && 
	     GTN_SET_MemberP(BB_live_out(OP_bb(op)), OP_result(alu_op, 0)))
      return FALSE;

    if (!TN_is_register(OP_opnd(alu_op, 0)))
      return FALSE;
	
    /* Make sure the opnd0 of <alu_op> is not re-defined again.
     */
    const EBO_TN_INFO* alu_opnd0_info = get_tn_info( OP_opnd( alu_op, 0 ) );
    if( alu_opnd0_info &&
	alu_opnd0_info->sequence_num >= tninfo->sequence_num )
      return FALSE;
    
    if (OP_code(alu_op) == TOP_andi32 &&
	(unsigned int)TN_value( OP_opnd( alu_op, 1)) <= 0x7fffffff &&
	(!EBO_in_peep ||
	 TN_is_register( OP_result( op, 0)) &&
	 TN_is_register( OP_opnd( alu_op, 0)) &&
	 TNs_Are_Equivalent( OP_result( op, 0), OP_opnd( alu_op, 0)))) {
      OP* new_op = Mk_OP( OP_code(alu_op), OP_result(op, 0),
			  OP_opnd(alu_op, 0), OP_opnd(alu_op, 1));
      OP_srcpos( new_op ) = OP_srcpos( op );
      BB_Insert_Op_After( OP_bb(op), op, new_op );

      if( EBO_Trace_Data_Flow ){
	fprintf( TFile, "Special_Sequence merges " );
	Print_OP_No_SrcLine(op);
	fprintf( TFile, "and " );
	Print_OP_No_SrcLine(alu_op);
	fprintf( TFile, "with ");
	Print_OP_No_SrcLine(new_op);
      }
      return TRUE;
    }
  } else if ( top == TOP_ldc32 || top == TOP_ldc64 ) {
    TN* tn0 = OP_opnd(op, 0);
    BOOL rflags_read = FALSE;
      
    /* Don't mess with symbols. */
    if (TN_is_symbol(tn0))
      return FALSE;

    /* We should not convert ldc to xor if constant is non-zero */
    if( TN_value(tn0) != 0)
      return FALSE;
      
    // If there is an instruction that is awaiting a rflags update then, 
    // do not convert the current op.
    for( OP* next_op = OP_next(op); next_op != NULL;
	 next_op = OP_next( next_op ) ){
      if( OP_reads_rflags( next_op ) )
	rflags_read = TRUE;
      if( TOP_is_change_rflags( OP_code(next_op) ) )
	break;
    }
    if (!rflags_read && CG_use_xortozero) {
      OP* new_op = Mk_OP(top == TOP_ldc32? TOP_zero32: TOP_zero64, 
		     OP_result(op, 0));
      BB_Insert_Op_After(OP_bb(op), op, new_op);
      return TRUE;
    }
  } 

  else if ( top == TOP_movdq ) {
    const EBO_TN_INFO* tninfo = opnd_tninfo[0];
    OP* alu_op = tninfo == NULL ? NULL : tninfo->in_op;
    
    if (!alu_op)
      return FALSE;

    if ( TOP_is_vector_packed_single ( OP_code (alu_op ) ) )
      op->opr = TOP_movaps;
    else if ( TOP_is_vector_packed_double ( OP_code (alu_op ) ) )
      op->opr = TOP_movaps;
  }

  else if( top == TOP_jne || top == TOP_je ) {
    /* For
     *    sete
     *    test32/test64
     *    jne/je
     * Transform to :
     *    je/jne (inverted branch).
     */
    const EBO_TN_INFO* tninfo = opnd_tninfo[0];
    OP* test_op = tninfo == NULL ? NULL : tninfo->in_op;
    if ( test_op && 
	 ( OP_code( test_op ) == TOP_test32 ||
	   OP_code( test_op ) == TOP_test64 ||
       OP_code( test_op ) == TOP_test8  ||
       OP_code( test_op ) == TOP_test16 ) &&
	 TNs_Are_Equivalent( OP_opnd(test_op, 0), OP_opnd(test_op, 1) ) ) {
      const EBO_TN_INFO* test_tninfo = get_tn_info( OP_opnd(test_op, 0 ));
      OP* set_op = test_tninfo == NULL ? NULL : test_tninfo->in_op;
      if ( set_op && OP_code( set_op ) == TOP_sete &&
	   !TN_live_out_of( OP_result(set_op,0), OP_bb(test_op) ) ){
	// Skip transform if rflag is redefined between set_op and test_op.
	BOOL skip = FALSE;
	if (OP_bb(set_op) != OP_bb(test_op))
	  skip = TRUE;		// Assume redefined if cross BB.
	else {
	  OP* next;
	  for (next = OP_next(set_op); next != test_op; next = OP_next(next)) {
	    if (!next ||	// test_op doesn't follow set_op
		TOP_is_change_rflags(OP_code(next))) {
	      skip = TRUE;
	      break;
	    }
	  }
	}
	// Delete test_op and change the branch.
	if (!skip) {
	  OP_Change_To_Noop( test_op );
	  op->opr = ( top == TOP_jne ) ? TOP_je : TOP_jne;
	}
      }
    }

    OP* alu_op = tninfo == NULL ? NULL : tninfo->in_op;

    if( test_is_replaced( alu_op, op, tninfo ) )
      return TRUE;
  }

  else if (!EBO_in_peep &&
	   top == TOP_shri64) {
    // Replace shift-left/shift-right with shift-right/move.  Look for
    // bit-field extraction of form:
    //   t = x << a
    //   y = t >> b
    // The field size is 64-b.  If the field size is 8, 16, or 32, then replace
    // the sequence with:
    //   t = x >> (b-a)
    //   y = movz t	; movzbq, movzwq, movzlq depending on field size
    // movz is perferred over the second shift because sometimes we have to mov
    // into a special register anyway, such as rax for returning y (bug 8594).
    //
    // There's no advantage in doing this optimization after register
    // allocation.
    const EBO_TN_INFO* tninfo = opnd_tninfo[0];
    OP* alu_op = tninfo == NULL ? NULL : tninfo->in_op;

    if (alu_op &&
	OP_code(alu_op) == TOP_shli64) {
      TN *left = OP_opnd(alu_op, 1);	// left shift amount
      TN *right = OP_opnd(op, 1);	// right shift amount
      Is_True(TN_Is_Constant(left) && TN_has_value(left) &&
	      TN_Is_Constant(right) && TN_has_value(right),
	      ("Special_Sequence: unexpected shift opnds"));
      int new_right_shift_val = TN_value(right) - TN_value(left);
      if (new_right_shift_val < 0)
	return FALSE;
      if (!Pred_Opnd_Avail(op, opnd_tninfo[0], 0))
	return FALSE;

      TOP mov_opcode;
      int field_size = 64 - TN_value(right);
      if (field_size == 8) {
	mov_opcode = TOP_movzbq;
      } else if (field_size == 16) {
	mov_opcode = TOP_movzwq;
      } else if (field_size == 32) {
	mov_opcode = TOP_movzlq;
      } else {
        return FALSE;
      }
      OP *new_shift, *new_move;
      TN *tmp_tn = Dup_TN(OP_result(alu_op, 0));
      new_shift = Mk_OP(TOP_shri64, tmp_tn, OP_opnd(alu_op, 0),
			Gen_Literal_TN(new_right_shift_val, TN_size(right)));
      BB_Insert_Op_After(OP_bb(op), op, new_shift);
      new_move = Mk_OP(mov_opcode, OP_result(op, 0), tmp_tn);
      BB_Insert_Op_After(OP_bb(op), new_shift, new_move);
      return TRUE;
    }
  }

  else if (top == TOP_shli32 ||
	   top == TOP_shli64) {

    if (EBO_in_peep)	// Can't break the x86-style property.  Bug 14424.
      return FALSE;

    // Replace:
    //   movzbl			; zero-extend
    //   shift-left 31		; shift left 31 bits
    // with:
    //   shift-left 31
    // The shift-left makes the zero-extend obsolete by shifting over the
    // zero-extended field.
    const EBO_TN_INFO* tninfo = opnd_tninfo[0];
    TN *left = OP_opnd(op, 1);	// shift amount
    Is_True(TN_Is_Constant(left) && TN_has_value(left),
	    ("Special_Sequence: shift amount not integer constant"));
    int left_shift_amount = TN_value(left);
    OP* alu_op = tninfo == NULL ? NULL : tninfo->in_op;
    EBO_TN_INFO *alu_opnd0_tninfo = NULL;

    if (alu_op &&
	((OP_code(alu_op) == TOP_movzbl && left_shift_amount >= 31) ||
	 (OP_code(alu_op) == TOP_movzwl && left_shift_amount >= 31) ||
	 (OP_code(alu_op) == TOP_movzbq && left_shift_amount >= 63) ||
	 (OP_code(alu_op) == TOP_movzwq && left_shift_amount >= 63) ||
	 (OP_code(alu_op) == TOP_movzlq && left_shift_amount >= 63)) &&
	Pred_Opnd_Avail(op, opnd_tninfo[0], 0, &alu_opnd0_tninfo)) {
      Set_OP_opnd(op, 0, OP_opnd(alu_op, 0));
      dec_ref_count(opnd_tninfo[0]);
      inc_ref_count(alu_opnd0_tninfo);
      opnd_tninfo[0] = alu_opnd0_tninfo;
      return FALSE;
    }
  }

  return FALSE;
}

/* This is used to eliminate unnecessry shift operations like:
 *     TN78($2) :-  dsrl32 TN1($0) <const> ;
 * Replace uses of the result with Zero_TN, and eliminate the shift op.
 * The def should not be live out of the current basic block.
 * 
 * Also, as a final step eliminate all copy ops (could be by-product of 
 * the removal of shifts above).
 */
void Redundancy_Elimination()
{
#if Is_True_On
  if (!(EBO_Opt_Mask & EBO_REDUNDANCY_ELIMINATION)) return;
#endif
  for( BB* bb = REGION_First_BB; bb != NULL; bb = BB_next( bb ) ){
    if( BB_rid(bb) && (RID_level(BB_rid(bb)) >= RL_CGSCHED) ){
      // don't change bb's which have already been through CG
      continue;
    }
    for( OP* op = BB_first_op( bb ); op != NULL; op = OP_next( op ) ){
      BOOL done = FALSE;
      INT copy_operand = CGTARG_Copy_Operand(op);
      if (copy_operand >= 0) {
	// Other ops could be rendered useless by the shift removal.
	// Example:
	//   TN969 :- srl TN1($0) (0x3) ;
	//   TN969 :- xor TN969<defopnd> TN916 ; 
	// Here, the xor operation is uselss if TN969 is not live 
	// out of this block and can be replaced by TN916.

	/* The whole point is to get rid of result and replace 
	   with copy_operand */

	TN* src = OP_opnd( op, copy_operand );
	TN* result = OP_result( op, 0 );
	
	FmtAssert( TN_is_register( src ) && TN_is_register( result ), 
		   ("Redundancy_Elimination: src/result not registers in EBO_in_peep (1)"));
	
	// do not delete assignment to result if result is live out
	if (EBO_no_liveness_info_available ||
	    REG_LIVE_Outof_BB (TN_register_class(result), 
			       TN_register(result), bb)) {
	  // see if the register is re-defined before block end
	  // we can still delete the op if there is a re-definition
	  BOOL redefined = FALSE;
	  for( OP* dw_op = OP_next( op ); dw_op != NULL; dw_op = OP_next( dw_op ) ){
	    if ((OP_code(dw_op) != TOP_asm) && 
		OP_results(dw_op) &&
		TN_is_register(OP_result(dw_op, 0)) &&
		(TN_register_class(OP_result(dw_op, 0)) == 
		 TN_register_class(result)) &&
		(TN_register(OP_result(dw_op, 0)) == TN_register(result))) {
	      redefined = TRUE;
	      break;
	    }
	  }
	  if (redefined == FALSE)
	    continue;
	}

	/* In the following scenario, the copy_op can not be deleted 
	 *    a = b   << copy_op >>
	 *      = a
	 *    b =     << redef >>
	 *      = a
	 * NOTE: If redef is a copy op and copy operand is 'a'
	 *       then copy_op can be removed
	 */
	BOOL cannot_be_removed = FALSE;
	for( OP* dw_op = OP_next( op ); dw_op != NULL; dw_op = OP_next( dw_op ) ){
	  if ((OP_code(dw_op) != TOP_asm) && 
	      OP_results(dw_op) &&
	      TN_is_register( OP_result( dw_op, 0)) &&
	      TNs_Are_Equivalent( OP_result( dw_op, 0), src)) {
	    
	    // see NOTE above
	    INT dw_op_copy_operand = CGTARG_Copy_Operand(dw_op);
	    TN *dw_op_copy_operand_tn = (dw_op_copy_operand>=0)?OP_opnd(dw_op, dw_op_copy_operand):NULL;
	    if ((dw_op_copy_operand >= 0) && 
		TN_is_register( dw_op_copy_operand_tn) &&
		TNs_Are_Equivalent(dw_op_copy_operand_tn, result)) 
	      break;
	    
	    // search if 'result' is being used after dw_op
	    for( OP* dw1_op = OP_next( dw_op ); dw1_op != NULL; dw1_op = OP_next( dw1_op ) ){
	      for( int i = 0; i < OP_opnds( dw1_op ); i++ ){
		if ( TN_is_register( OP_opnd( dw1_op, i)) &&
		     TNs_Are_Equivalent( OP_opnd( dw1_op, i), result)) {
		  cannot_be_removed = TRUE;
		  break;
		}
	      }
	    }
	    if (cannot_be_removed)
	      break;
	  }
	}
	if (cannot_be_removed)
	  continue;	

	/* Traverse downwards; replace result with src */

	for( OP* dw_op = OP_next( op ); dw_op != NULL; dw_op = OP_next( dw_op ) ){

	  for( int i = 0; i < OP_opnds( dw_op ); i++ ){
	    if ( TN_is_register( OP_opnd( dw_op, i)) && 
		 TNs_Are_Equivalent( OP_opnd( dw_op, i), result)) {
	      Set_OP_opnd( dw_op, i, src );
	      done = TRUE;
	    }
	  }

	  if( OP_results( dw_op ) == 1 ){
	    TN* tnr = OP_result( dw_op, 0 );

	    if( TN_is_register( tnr) && 
		TNs_Are_Equivalent( tnr, result ) || 
		TNs_Are_Equivalent( tnr, src ) )
	      break;
	  }
	}      

	if( done ){
	  OP* dead = op;
	  op = OP_prev( op );
	  
	  if( EBO_Trace_Optimization ){
	    fprintf( TFile, "Redundancy_Elimination removes simplified op - " );
	    Print_OP_No_SrcLine( dead );
	  }
	  
	  BB_Remove_Op( bb, dead );
	  
	  if( op == NULL )
	    op = BB_first_op( bb );
	}       

	continue; // end for this op
      }

      if (OP_code( op ) != TOP_sar32 &&
	  OP_code( op ) != TOP_sar64 &&
	  OP_code( op ) != TOP_shl32 && 
	  OP_code( op ) != TOP_shl64 && 
	  OP_code( op ) != TOP_shr32 && 
	  OP_code( op ) != TOP_shr64 )
	continue;

      /* The whole point is to get rid of result. */

      TN* src = OP_opnd( op, 0 );
      TN* result = OP_result( op, 0 );

      FmtAssert( TN_is_register( src ) && TN_is_register( result ), 
		 ("Redundancy_Elimination: src/result not registers in EBO_in_peep (2)"));
      
      FmtAssert( false, ("Redundancy_Elimination: UNIMPLEMENTED (1)") );

      // do not delete assignment to result if result is live out
      if (EBO_no_liveness_info_available ||
	  REG_LIVE_Outof_BB (TN_register_class(result), 
			     TN_register(result), bb)) {
	// see if the register is re-defined before block end
	// we can still delete the op if there is a re-definition
	BOOL redefined = FALSE;
	for( OP* dw_op = OP_next( op ); dw_op != NULL; dw_op = OP_next( dw_op ) ){
	  if (OP_has_result(dw_op) &&
	      TN_is_register(OP_result(dw_op, 0)) &&
	      (TN_register_class(OP_result(dw_op, 0)) == 
	       TN_register_class(result)) &&
	      (TN_register(OP_result(dw_op, 0)) == TN_register(result))) {
	    redefined = TRUE;
	    break;
	  }
	}
	if (redefined == FALSE)
	  continue;
      }

      /* Traverse downwards; replace result with Zero_TN */

      for( OP* dw_op = OP_next( op ); dw_op != NULL; dw_op = OP_next( dw_op ) ){
	for( int i = 0; i < OP_opnds( dw_op ); i++ ){
	  if( TN_is_register(OP_opnd( dw_op, i)) && 
	      (TN_register_class(OP_opnd(dw_op, i)) == 
	       TN_register_class(result)) &&
	      (TN_register(OP_opnd( dw_op, i )) == TN_register(result))) {
	    FmtAssert( false, ("Redundancy_Elimination: UNIMPLEMENTED (2)") );
	    done = TRUE;
	  }
	}

	if( OP_results( dw_op ) == 1 ){
	  TN* tnr = OP_result( dw_op, 0 );

	  if( TN_is_register( tnr) && 
	      TNs_Are_Equivalent( tnr, result ) )
	    break;
	}
      }      

      if( done ){
	OP* dead = op;
	op = OP_prev( op );

	if( EBO_Trace_Optimization ){
	  fprintf( TFile, "Redundancy_Elimination removes simplified op - " );
	  Print_OP_No_SrcLine( dead );
	}

	BB_Remove_Op( bb, dead );

	if( op == NULL )
	  op = BB_first_op( bb );
      }
    }
  }   
}


static inline TN* OP_opnd_use( OP* op, ISA_OPERAND_USE use )
{
  const int indx = OP_find_opnd_use( op, use );
  return ( indx >= 0 ) ? OP_opnd( op, indx ) : NULL;
}


/* return <ofst> = <ofst1> + <ofst2> * <scale>
 */
static TN* Compose_Addr_offset( TN* ofst1, TN* ofst2, TN* scale )
{
  if( ofst1 == NULL )
    ofst1 = Gen_Literal_TN( 0, 4 );

  if( ofst2 == NULL )
    ofst2 = Gen_Literal_TN( 0, 4 );

  if( scale == NULL )
    scale = Gen_Literal_TN( 1, 4 );

  // Cannot handle two symbols.
  if( TN_is_symbol(ofst1) &&
      TN_is_symbol(ofst2) ){
    return NULL;
  }

  if( TN_is_symbol(ofst1) ){
    ST* sym = TN_var(ofst1);
    const INT64 ofst = TN_value(ofst2) * TN_value(scale) + TN_offset(ofst1);

    // keep the relocs for TLS object
    Is_True ( TN_relocs(ofst2) == TN_RELOC_NONE, ("bad TN relocs for ofst2") );
    return Gen_Symbol_TN( sym, ofst,
                          ST_is_thread_local(sym) ? TN_relocs(ofst1) : TN_RELOC_NONE );
  }

  if( TN_is_symbol(ofst2) ){
    if( TN_value(scale) != 1 )
      return NULL;

    ST* sym = TN_var(ofst2);
    const INT64 ofst = TN_value(ofst1) + TN_offset(ofst2);

    // keep the relocs for TLS object
    Is_True ( TN_relocs(ofst1) == TN_RELOC_NONE, ("bad TN relocs for ofst1") );
    return Gen_Symbol_TN( sym, ofst, 
                          ST_is_thread_local(sym) ? TN_relocs(ofst2) : TN_RELOC_NONE );
  }

  const INT64 value = TN_value(ofst1) + TN_value(ofst2) * TN_value(scale);

  if( !ISA_LC_Value_In_Class( value, LC_simm32 ) )
    return NULL;

  return Gen_Literal_TN( value, 4 );
}

#define IS_VALID_SCALE(s)  ( (s)==1 || (s)==2 || (s)==4 || (s)==8 )

static BOOL Compose_Addr( OP* mem_op, EBO_TN_INFO* pt_tninfo,
			  ISA_OPERAND_USE replace_opnd,
			  TN** opnd_tn,
			  TN** index, TN** offset, TN** scale, TN** base )
{
#if Is_True_On
  if (!(EBO_Opt_Mask & EBO_COMPOSE_ADDR)) return FALSE;
#endif
  /* address = ofst + base + index * scale */
  struct ADDRESS_COMPONENT {
    TN* index;
    TN* base;
    TN* offset;
    TN* scale;
  } a, b;

  bzero( &a, sizeof(a) );
  a.scale = Gen_Literal_TN( 1, 4 );

  /* First, extract the address components from a previous op that
     compute the address. The components are stored into <a>.
  */

  OP* addr_op = pt_tninfo->in_op;
  const TOP top = OP_code( addr_op );

  switch( top ){
  case TOP_lea32:
    if( Is_Target_64bit() )
      return FALSE;
    // fall thru
  case TOP_lea64:
    a.base   = OP_opnd_use( addr_op, OU_base );
    a.offset = OP_opnd_use( addr_op, OU_offset );
    break;

  case TOP_leaxx32:
    if( Is_Target_64bit() )
      return FALSE;
    // fall thru
  case TOP_leaxx64:
    a.index  = OP_opnd_use( addr_op, OU_index );
    a.offset = OP_opnd_use( addr_op, OU_offset );
    a.scale  = OP_opnd_use( addr_op, OU_scale );

    if( TN_value(a.scale) == 1 ){
      a.base = a.index;
      a.index = a.scale = NULL;
    }

    break;

  case TOP_leax32:
    if( Is_Target_64bit() )
      return FALSE;
    // fall thru
  case TOP_leax64:
    a.index  = OP_opnd_use( addr_op, OU_index );
    a.offset = OP_opnd_use( addr_op, OU_offset );
    a.scale  = OP_opnd_use( addr_op, OU_scale );
    a.base   = OP_opnd_use( addr_op, OU_base );
    break;

  case TOP_shli32:
    if( Is_Target_64bit() )
      return FALSE;
    // fall thru
  case TOP_shli64:
    {
      TN* tn = OP_opnd( addr_op, 1 );
      const INT64 val = TN_value(tn);
      if( val < 0 || val > 3 )
	return FALSE;

      a.scale = Gen_Literal_TN( 1 << val, 4 );
      a.offset = Gen_Literal_TN( 0, 4 );
      a.index = OP_opnd( addr_op, 0 );
    }
    break;

  case TOP_add32:
    if( Is_Target_64bit() )
      return FALSE;
    // fall thru
  case TOP_add64:
    a.base = OP_opnd( addr_op, 0 );
    a.index = OP_opnd( addr_op, 1 );
    a.offset = Gen_Literal_TN( 0, 4 );
    a.scale = Gen_Literal_TN( 1, 4 );
    break;

  case TOP_addi32:
  case TOP_inc32:
  case TOP_dec32:
    if( Is_Target_64bit() )
      return FALSE;
    // fall thru
  case TOP_addi64:
  case TOP_inc64:
  case TOP_dec64:
    a.base = OP_opnd( addr_op, 0 );
    if (top == TOP_inc32 || top == TOP_inc64)
      a.offset = Gen_Literal_TN(1, 4);
    else if (top == TOP_dec32 || top == TOP_dec64)
      a.offset = Gen_Literal_TN(-1, 4);
    else
      a.offset = OP_opnd( addr_op, 1 );
    break;

  case TOP_mov32:
    if( Is_Target_64bit() )
      return FALSE;
    // fall thru
  case TOP_mov64:
    {
      TN* opnd = OP_opnd( addr_op, 0 );
      a.offset = Gen_Literal_TN( 0, 4 );
      a.base = opnd;
    }
    break;

  case TOP_ldc32:
  case TOP_ldc64:
    a.offset = OP_opnd( addr_op, 0 );
    break;

  default:
    return FALSE;
  }

  const int op_offset_idx = OP_find_opnd_use( mem_op, OU_offset );
  TN* op_offset_tn = opnd_tn[op_offset_idx];

  // Replace a.index with a symbol from a previous ldc64 op.
  if( a.index != NULL &&
      TN_value( a.scale ) == 1 ){
    const EBO_TN_INFO* tninfo = get_tn_info( a.index );
    if( tninfo != NULL        &&
	tninfo->in_op != NULL &&
	tninfo->sequence_num < pt_tninfo->sequence_num &&
	( OP_code(tninfo->in_op) == TOP_ldc64 ||
	  OP_code(tninfo->in_op) == TOP_ldc32 ) ){
      TN* offset = Compose_Addr_offset( a.offset,
					OP_opnd( tninfo->in_op, 0 ),
					NULL );
      if( offset != NULL ){
	a.offset = offset;
	a.index  = NULL;
      }
    }
  }

  // Replace a.base with a symbol from a previous ldc64 op.
  if( a.base != NULL ){
    const EBO_TN_INFO* tninfo = get_tn_info( a.base );
    if( tninfo != NULL        &&
	tninfo->in_op != NULL &&
	tninfo->sequence_num < pt_tninfo->sequence_num &&
	( OP_code(tninfo->in_op) == TOP_ldc64 ||
	  OP_code(tninfo->in_op) == TOP_ldc32 ) ){
      TN* offset = Compose_Addr_offset( a.offset,
					OP_opnd( tninfo->in_op, 0 ),
					NULL );
      if( offset != NULL ){
	a.offset = offset;
	a.base = NULL;
      }
    }
  }

  if( !EBO_in_peep ){
    if( OP_bb( addr_op ) != OP_bb( mem_op ) ){
      for( int i = 0; i < OP_opnds(addr_op); i++ ){
	if( has_assigned_reg( OP_opnd(addr_op,i) ) )
	  return FALSE;
      }
    }

    if( a.index != NULL ){
      const REGISTER reg = TN_register( a.index );
      if( reg == RAX || reg == RCX || reg == RDX )
	return FALSE;
    }

    if( a.base != NULL ){
      const REGISTER reg = TN_register( a.base );
      if( reg == RAX || reg == RCX || reg == RDX )
	return FALSE;
    }
  }

  /* Check <index> and <base> will not be re-defined between
     <addr_op> and <mem_op>, inclusive.
  */

  if( a.base != NULL ){
    EBO_TN_INFO* tninfo = get_tn_info( a.base );
    if( tninfo != NULL && tninfo->sequence_num >= pt_tninfo->sequence_num ){
      return FALSE;
    }
  }
  
  if( a.index != NULL ){
    EBO_TN_INFO* tninfo = get_tn_info( a.index );
    if( tninfo != NULL && tninfo->sequence_num >= pt_tninfo->sequence_num ){
      return FALSE;
    }
  }

  /* Second, extract the address components from <mem_op>, and deposit them
     to <b>.
  */

  b.scale  = OP_opnd_use( mem_op, OU_scale );
  b.base   = OP_opnd_use( mem_op, OU_base );
  b.index  = OP_opnd_use( mem_op, OU_index );
  b.offset = OP_opnd_use( mem_op, OU_offset );

  if( b.scale == NULL )
    b.scale = Gen_Literal_TN( 1, 4 );

  /* Now, make up the final address components from <a> and <b>.
   */

  *index = *offset = *base = *scale = NULL;

  if( replace_opnd == OU_base ){
    // offset = b.offset + a.offset
    *offset = Compose_Addr_offset( b.offset, a.offset, NULL );

    // base = a.base
    *base = a.base;

    // index * scale = a.index * a.scale + b.index * b.scale
    if( a.index == NULL &&
	b.index == NULL ){
      *index = *scale = NULL;

    } else if( a.index == NULL ){
      *index = b.index;
      *scale = b.scale;

    } else if( b.index == NULL ){
      *index = a.index;
      *scale = a.scale;
      
    } else {
      if( TNs_Are_Equivalent( a.index, b.index ) ){
	*index = a.index;
	*scale = Gen_Literal_TN( TN_value(a.scale) + TN_value(b.scale), 4 );

      } else {
	if( *base != NULL )
	  return FALSE;
	if( TN_value(b.scale) == 1 ){
	  *base = b.index;
	  *index = a.index;
	  *scale = a.scale;

	} else if( TN_value(a.scale) == 1 ){
	  *base = a.index;
	  *index = b.index;
	  *scale = b.scale;

	} else
	  return FALSE;
      }
    }

  } else if( replace_opnd == OU_index ){
    // offset = b.offset + a.offset * b.scale
    *offset = Compose_Addr_offset( b.offset, a.offset, b.scale );

    // index * scale = a.index * a.scale * b.scale
    *index = a.index;
    *scale = Gen_Literal_TN( TN_value(a.scale) * TN_value(b.scale), 4 );

    // base = b.base + a.base * b.scale
    if( b.base == NULL &&
	a.base == NULL ){
      *base = NULL;

    } else if( b.base == NULL ){
      *base = a.base;

      if( TN_value(b.scale) != 1 ){
	if( *index == NULL ){
	  *scale = b.scale;
	  *index = *base;
	  *base  = NULL;

	} else {
	  if( !TNs_Are_Equivalent( *index, a.base ) )
	    return FALSE;
	  *base = NULL;
	  *scale = Gen_Literal_TN( TN_value(*scale) + TN_value(b.scale), 4 );
	}
      }

    } else if( a.base == NULL ){
      *base = b.base;

    } else {
      if( *index == NULL ){
	*index = a.base;
	*scale = b.scale;
	*base  = b.base;

      } else {
	if( !TNs_Are_Equivalent( b.base, a.base ) ) {
	  // Bug 3724 - If a.base and a.index are identical, 
	  // then we could still fold a.base into b.index
	  // and adjust the scale.
	  if ( !TNs_Are_Equivalent ( a.base, a.index ) )
	    return FALSE;
	  else {
	    *index = b.base;
	  }
	}

	if( TN_value(*scale) != 1 )
	  return FALSE;

	*scale = Gen_Literal_TN( TN_value(b.scale) + 1, 4 );
	*base = *index;
	*index = a.base;
      }
    }

  } else {
    return FALSE;
  }

  /* Filter out any invalid combination. */

  if( *offset == NULL ||
      !ISA_LC_Value_In_Class( TN_value(*offset), LC_simm32 ) )
    return FALSE;

  /* Bug 14488: under -mcmodel=kernel, if the offset is with
     respect to a section name, check for a smaller offset size,
     so that the final relocation produced by the linker using
     possibly custom linker scripts can fit R_X86_64_32S. */
  if (mcmodel == KERNEL &&
      TN_is_symbol(*offset) &&
      ST_sym_class(TN_var(*offset)) == CLASS_BLOCK &&
      !ISA_LC_Value_In_Class(TN_offset(*offset), LC_simm16))
    return FALSE;

  if( *scale != NULL && !IS_VALID_SCALE( TN_value(*scale) ) ){
    if( *base != NULL ||
	TN_value(*scale) > 8 )
      return FALSE;

    *base = *index;
    *scale = Gen_Literal_TN( TN_value(*scale) - 1, 4 );

    if( !IS_VALID_SCALE( TN_value(*scale) ) )
      return FALSE;
  }

  /* Make sure the index is not %rsp. */

  if( *index != NULL &&
      TN_register(*index) == RSP ){
    if( TN_value( *scale ) != 1 )
      return FALSE;

    TN* tmp_tn = *index;
    *index = *base;
    *base = tmp_tn;
  }

  if( *index == NULL ){
    if( *scale != NULL && TN_value(*scale) > 1 )
      return FALSE;
      
    *scale = NULL;
  }

  if( *base == NULL  &&
      *index != NULL &&
      TN_value(*scale) == 1 ){
    *base = *index;
    *index = *scale = NULL;
  }

  return TRUE;
}


// Group together opcodes that perform the same function but with different
// address modes.
typedef struct {
  TOP reg_mode;
  TOP base_mode;
  TOP base_index_mode;
  TOP index_mode;
  TOP n32_mode;
} Addr_Mode_Group;

// Map an opcode to its address-modes group.
static Addr_Mode_Group *Top_To_Addr_Mode_Group[TOP_count+1];

// List all address mode groups.  Each group gives the opcodes for OPs that
// perform the same function but with different address modes.  Entries can be
// listed in any order.  No duplicates allowed.  The table doesn't have to be
// complete; it may list only those OPs that EBO can do something about.
static Addr_Mode_Group Addr_Mode_Group_Table[] = {
  // REG_MODE	BASE_MODE	BASE_INDEX_MODE	INDEX_MODE     N32_MODE (offset)

  // Load and stores.
  {TOP_UNDEFINED, TOP_store8,	TOP_storex8,	TOP_storexx8,	TOP_store8_n32},
  {TOP_UNDEFINED, TOP_store16,	TOP_storex16,	TOP_storexx16, TOP_store16_n32},
  {TOP_UNDEFINED, TOP_store32,	TOP_storex32,	TOP_storexx32, TOP_store32_n32},
  {TOP_UNDEFINED, TOP_store64,	TOP_storex64,	TOP_storexx64, TOP_store64_off},
  {TOP_UNDEFINED, TOP_stss,	TOP_stssx,	TOP_stssxx,	TOP_stss_n32},
  {TOP_UNDEFINED, TOP_stsd,	TOP_stsdx,	TOP_stsdxx,	TOP_stsd_n32},
  {TOP_UNDEFINED, TOP_stntss,   TOP_stntssx,    TOP_stntssxx,   TOP_UNDEFINED},
  {TOP_UNDEFINED, TOP_stntsd,   TOP_stntsdx,    TOP_stntsdxx,   TOP_UNDEFINED},
  {TOP_UNDEFINED, TOP_stdqa,	TOP_stdqax,	TOP_stdqaxx,	TOP_stdqa_n32},
  {TOP_UNDEFINED, TOP_stntpd,	TOP_stntpdx,	TOP_stntpdxx,	TOP_UNDEFINED},
  {TOP_UNDEFINED, TOP_stdqu,	TOP_stdqux,	TOP_stdquxx,	TOP_UNDEFINED},
  {TOP_UNDEFINED, TOP_fmovsldupx, TOP_fmovsldupxx, TOP_fmovsldupxxx, TOP_UNDEFINED},
  {TOP_UNDEFINED, TOP_fmovshdupx, TOP_fmovshdupxx, TOP_fmovshdupxxx, TOP_UNDEFINED},
  {TOP_UNDEFINED, TOP_fmovddupx,  TOP_fmovddupxx,  TOP_fmovddupxxx, TOP_UNDEFINED},
  {TOP_UNDEFINED, TOP_ldss,	TOP_ldssx,	TOP_ldssxx,	TOP_ldss_n32},
  {TOP_UNDEFINED, TOP_ldsd,	TOP_ldsdx,	TOP_ldsdxx,	TOP_ldsd_n32},
  {TOP_UNDEFINED, TOP_lddqa,	TOP_lddqax,	TOP_lddqaxx,	TOP_lddqa_n32},
  {TOP_UNDEFINED, TOP_ldupd,	TOP_ldupdx,	TOP_ldupdxx,	TOP_UNDEFINED},
  {TOP_UNDEFINED, TOP_ldups,	TOP_ldupsx,	TOP_ldupsxx,	TOP_ldups_n32},
  {TOP_UNDEFINED, TOP_lddqu,	TOP_lddqux,	TOP_lddquxx,	TOP_UNDEFINED},
  {TOP_UNDEFINED, TOP_ldlps,	TOP_ldlpsx,	TOP_ldlpsxx,	TOP_ldlps_n32},
  {TOP_UNDEFINED, TOP_ldlpd,	TOP_ldlpdx,	TOP_ldlpdxx,	TOP_UNDEFINED},
  {TOP_UNDEFINED, TOP_ldaps,	TOP_ldapsx,	TOP_ldapsxx,	TOP_ldaps_n32},
  {TOP_UNDEFINED, TOP_ldapd,	TOP_ldapdx,	TOP_ldapdxx,	TOP_ldapd_n32},
  {TOP_UNDEFINED, TOP_stlps,	TOP_stlpsx,	TOP_stlpsxx,	TOP_stlps_n32},
  {TOP_UNDEFINED, TOP_stlpd,	TOP_stlpdx,	TOP_stlpdxx,	TOP_stlpd_n32},
  {TOP_UNDEFINED, TOP_staps,	TOP_stapsx,	TOP_stapsxx,	TOP_staps_n32},
  {TOP_UNDEFINED, TOP_stapd,	TOP_stapdx,	TOP_stapdxx,	TOP_stapd_n32},
  {TOP_UNDEFINED, TOP_stups,	TOP_stupsx,	TOP_stupsxx,	TOP_stups_n32},
  {TOP_UNDEFINED, TOP_stupd,	TOP_stupdx,	TOP_stupdxx,	TOP_stupd_n32},
  {TOP_UNDEFINED, TOP_ldhps,	TOP_ldhpsx,	TOP_ldhpsxx,	TOP_UNDEFINED},
  {TOP_UNDEFINED, TOP_ldhpd,	TOP_ldhpdx,	TOP_ldhpdxx,	TOP_ldhpd_n32},
  {TOP_UNDEFINED, TOP_sthps,	TOP_sthpsx,	TOP_sthpsxx,	TOP_UNDEFINED},
  {TOP_UNDEFINED, TOP_vstss,	TOP_vstssx,	TOP_vstssxx,	TOP_vstss_n32},
  {TOP_UNDEFINED, TOP_vstsd,	TOP_vstsdx,	TOP_vstsdxx,	TOP_vstsd_n32},
  {TOP_UNDEFINED, TOP_vstntss,  TOP_vstntssx,   TOP_vstntssxx,  TOP_UNDEFINED},
  {TOP_UNDEFINED, TOP_vstntsd,  TOP_vstntsdx,   TOP_vstntsdxx,  TOP_UNDEFINED},
  {TOP_UNDEFINED, TOP_vstdqa,	TOP_vstdqax,	TOP_vstdqaxx,	TOP_UNDEFINED},
  {TOP_UNDEFINED, TOP_vstntpd,	TOP_vstntpdx,	TOP_vstntpdxx,	TOP_UNDEFINED},
  {TOP_UNDEFINED, TOP_vstdqu,	TOP_vstdqux,	TOP_vstdquxx,	TOP_vstdqu_n32},
  //{TOP_vmovsldup, TOP_vmovsldupx, TOP_vmovsldupxx, TOP_vmovsldupxxx, TOP_UNDEFINED},
  //{TOP_vfmovshdup, TOP_vmovshdupx, TOP_vmovshdupxx, TOP_vmovshdupxxx, TOP_UNDEFINED},
  {TOP_vmovddup,  TOP_vmovddupx,    TOP_vmovddupxx,   TOP_vmovddupxxx,   TOP_UNDEFINED},
  {TOP_UNDEFINED, TOP_vldss,	TOP_vldssx,	TOP_vldssxx,	TOP_vldss_n32},
  {TOP_UNDEFINED, TOP_vldsd,	TOP_vldsdx,	TOP_vldsdxx,	TOP_vldsd_n32},
  {TOP_UNDEFINED, TOP_vlddqa,	TOP_vlddqax,	TOP_vlddqaxx,	TOP_UNDEFINED},
  {TOP_UNDEFINED, TOP_vldupd,	TOP_vldupdx,	TOP_vldupdxx,	TOP_vldupd_n32},
  {TOP_UNDEFINED, TOP_vldups,	TOP_vldupsx,	TOP_vldupsxx,	TOP_vldups_n32},
  {TOP_UNDEFINED, TOP_vlddqu,	TOP_vlddqux,	TOP_vlddquxx,	TOP_vlddqu_n32},
  {TOP_UNDEFINED, TOP_vldlps,	TOP_vldlpsx,	TOP_vldlpsxx,	TOP_vldlps_n32},
  {TOP_UNDEFINED, TOP_vldlpd,	TOP_vldlpdx,	TOP_vldlpdxx,	TOP_vldlpd_n32},
  {TOP_UNDEFINED, TOP_vldaps,	TOP_vldapsx,	TOP_vldapsxx,	TOP_vldaps_n32},
  {TOP_UNDEFINED, TOP_vldapd,	TOP_vldapdx,	TOP_vldapdxx,	TOP_vldapd_n32},
  {TOP_UNDEFINED, TOP_vstlps,	TOP_vstlpsx,	TOP_vstlpsxx,	TOP_vstlps_n32},
  {TOP_UNDEFINED, TOP_vstlpd,	TOP_vstlpdx,	TOP_vstlpdxx,	TOP_vstlpd_n32},
  {TOP_UNDEFINED, TOP_vstaps,	TOP_vstapsx,	TOP_vstapsxx,	TOP_vstaps_n32},
  {TOP_UNDEFINED, TOP_vstapd,	TOP_vstapdx,	TOP_vstapdxx,	TOP_vstapd_n32},
  {TOP_UNDEFINED, TOP_vldhps,	TOP_vldhpsx,	TOP_vldhpsxx,	TOP_vldhps_n32},
  {TOP_UNDEFINED, TOP_vldhpd,	TOP_vldhpdx,	TOP_vldhpdxx,	TOP_vldhpd_n32},
  {TOP_UNDEFINED, TOP_vsthps,	TOP_vsthpsx,	TOP_vsthpsxx,	TOP_vsthps_n32},
  {TOP_UNDEFINED, TOP_vsthpd,	TOP_vsthpdx,	TOP_vsthpdxx,	TOP_vsthpd_n32},
  {TOP_UNDEFINED, TOP_ld8_64,	TOP_ldx8_64,	TOP_ldxx8_64,	TOP_ld8_64_off},
  {TOP_UNDEFINED, TOP_ldu8_64,	TOP_ldxu8_64,	TOP_ldxxu8_64, TOP_ldu8_64_off},
  {TOP_UNDEFINED, TOP_ld16_64,	TOP_ldx16_64,	TOP_ldxx16_64, TOP_ld16_64_off},
  {TOP_UNDEFINED, TOP_ldu16_64,	TOP_ldxu16_64,	TOP_ldxxu16_64,	TOP_ldu16_64_off},
  {TOP_UNDEFINED, TOP_ld8_32,	TOP_ldx8_32,	TOP_ldxx8_32,  TOP_ld8_32_n32},
  {TOP_UNDEFINED, TOP_ldu8_32,	TOP_ldxu8_32,	TOP_ldxxu8_32, TOP_ldu8_32_n32},
  {TOP_UNDEFINED, TOP_ld16_32,	TOP_ldx16_32,	TOP_ldxx16_32, TOP_ld16_32_n32},
  {TOP_UNDEFINED, TOP_ldu16_32,	TOP_ldxu16_32,	TOP_ldxxu16_32, TOP_ldu16_32_n32},
  {TOP_UNDEFINED, TOP_ld32,	TOP_ldx32,	TOP_ldxx32,	TOP_ld32_n32},
  {TOP_UNDEFINED, TOP_ld32_64,	TOP_ldx32_64,	TOP_ldxx32_64, TOP_ld32_64_off},
  {TOP_UNDEFINED, TOP_ld64,	TOP_ldx64,	TOP_ldxx64,	TOP_ld64_off},
  {TOP_UNDEFINED, TOP_prefetch,	TOP_prefetchx,	TOP_prefetchxx,	TOP_UNDEFINED},
  {TOP_UNDEFINED, TOP_prefetchw,  TOP_prefetchwx,  TOP_prefetchwxx, TOP_UNDEFINED},
  {TOP_UNDEFINED, TOP_prefetcht0, TOP_prefetcht0x, TOP_prefetcht0xx, TOP_UNDEFINED},
  {TOP_UNDEFINED, TOP_prefetcht1, TOP_prefetcht1x, TOP_prefetcht1xx, TOP_UNDEFINED},

  {TOP_UNDEFINED, TOP_prefetchnta, TOP_prefetchntax, TOP_prefetchntaxx, TOP_UNDEFINED},
  {TOP_UNDEFINED, TOP_sthpd,	TOP_sthpdx,	TOP_sthpdxx,	TOP_sthpd_n32},
  // LEA
  {TOP_UNDEFINED, TOP_lea32,	TOP_leax32,	TOP_leaxx32,	TOP_UNDEFINED},
  {TOP_UNDEFINED, TOP_lea64,	TOP_leax64,	TOP_leaxx64,	TOP_UNDEFINED},

  // Load-execute.

  {TOP_add32,	TOP_addx32,	TOP_addxx32,	TOP_addxxx32,	TOP_UNDEFINED},
  {TOP_add64,	TOP_addx64,	TOP_addxx64,	TOP_addxxx64,	TOP_UNDEFINED},
  {TOP_addss,	TOP_addxss,	TOP_addxxss,	TOP_addxxxss,	TOP_UNDEFINED},
  {TOP_addsd,	TOP_addxsd,	TOP_addxxsd,	TOP_addxxxsd,	TOP_UNDEFINED},	
  {TOP_addsd,	TOP_addxsd,	TOP_addxxsd,	TOP_addxxxsd,	TOP_UNDEFINED},	
  {TOP_vfaddsd, TOP_vfaddxsd,   TOP_vfaddxxsd,  TOP_vfaddxxxsd, TOP_UNDEFINED},
  {TOP_add128v16, TOP_addx128v16, TOP_addxx128v16, TOP_addxxx128v16, TOP_UNDEFINED},
  {TOP_add128v32, TOP_addx128v32, TOP_addxx128v32, TOP_addxxx128v32, TOP_UNDEFINED},
  {TOP_add128v64, TOP_addx128v64, TOP_addxx128v64, TOP_addxxx128v64, TOP_UNDEFINED},
  {TOP_vadd128v8,  TOP_vaddx128v8,  TOP_vaddxx128v8,  TOP_vaddxxx128v8, TOP_UNDEFINED},
  {TOP_vadd128v16, TOP_vaddx128v16, TOP_vaddxx128v16, TOP_vaddxxx128v16, TOP_UNDEFINED},
  {TOP_vadd128v32, TOP_vaddx128v32, TOP_vaddxx128v32, TOP_vaddxxx128v32, TOP_UNDEFINED},
  {TOP_vadd128v64, TOP_vaddx128v64, TOP_vaddxx128v64, TOP_vaddxxx128v64, TOP_UNDEFINED},
  {TOP_fadd128v32,	TOP_faddx128v32,	TOP_faddxx128v32,	TOP_faddxxx128v32,	TOP_UNDEFINED},
  {TOP_fadd128v64,	TOP_faddx128v64,	TOP_faddxx128v64,	TOP_faddxxx128v64,	TOP_UNDEFINED},
  {TOP_fhadd128v32,	TOP_fhaddx128v32,	TOP_fhaddxx128v32,	TOP_fhaddxxx128v32,	TOP_UNDEFINED},
  {TOP_fhadd128v64,	TOP_fhaddx128v64,	TOP_fhaddxx128v64,	TOP_fhaddxxx128v64,	TOP_UNDEFINED},
  {TOP_faddsub128v32,	TOP_faddsubx128v32,	TOP_faddsubxx128v32,	TOP_faddsubxxx128v32,	TOP_UNDEFINED},
  {TOP_faddsub128v64,	TOP_faddsubx128v64,	TOP_faddsubxx128v64,	TOP_faddsubxxx128v64,	TOP_UNDEFINED},
  {TOP_fhsub128v32,	TOP_fhsubx128v32,	TOP_fhsubxx128v32,	TOP_fhsubxxx128v32,	TOP_UNDEFINED},
  {TOP_fhsub128v64,	TOP_fhsubx128v64,	TOP_fhsubxx128v64,	TOP_fhsubxxx128v64,	TOP_UNDEFINED},
  {TOP_vfadd128v32,	TOP_vfaddx128v32,	TOP_vfaddxx128v32,	TOP_vfaddxxx128v32,	TOP_UNDEFINED},
  {TOP_vfadd128v64,	TOP_vfaddx128v64,	TOP_vfaddxx128v64,	TOP_vfaddxxx128v64,	TOP_UNDEFINED},
  {TOP_vfhadd128v32,	TOP_vfhaddx128v32,	TOP_vfhaddxx128v32,	TOP_vfhaddxxx128v32,	TOP_UNDEFINED},
  {TOP_vfhadd128v64,	TOP_vfhaddx128v64,	TOP_vfhaddxx128v64,	TOP_vfhaddxxx128v64,	TOP_UNDEFINED},
  {TOP_vfaddsub128v32,	TOP_vfaddsubx128v32,	TOP_vfaddsubxx128v32,	TOP_vfaddsubxxx128v32,	TOP_UNDEFINED},
  {TOP_vfaddsub128v64,	TOP_vfaddsubx128v64,	TOP_vfaddsubxx128v64,	TOP_vfaddsubxxx128v64,	TOP_UNDEFINED},
  {TOP_vfhsub128v32,	TOP_vfhsubx128v32,	TOP_vfhsubxx128v32,	TOP_vfhsubxxx128v32,	TOP_UNDEFINED},
  {TOP_vfhsub128v64,	TOP_vfhsubx128v64,	TOP_vfhsubxx128v64,	TOP_vfhsubxxx128v64,	TOP_UNDEFINED},
  {TOP_sub32,	TOP_subx32,	TOP_subxx32,	TOP_subxxx32,	TOP_UNDEFINED},
  {TOP_sub64,	TOP_subx64,	TOP_subxx64,	TOP_subxxx64,	TOP_UNDEFINED},
  {TOP_subss,	TOP_subxss,	TOP_subxxss,	TOP_subxxxss,	TOP_UNDEFINED},
  {TOP_subsd,	TOP_subxsd,	TOP_subxxsd,	TOP_subxxxsd,	TOP_UNDEFINED},
  {TOP_vsubss,	TOP_vsubxss,	TOP_vsubxxss,	TOP_vsubxxxss,	TOP_UNDEFINED},
  {TOP_vsubsd,	TOP_vsubxsd,	TOP_vsubxxsd,	TOP_vsubxxxsd,	TOP_UNDEFINED},
  {TOP_sub128v8,	TOP_subx128v8,	TOP_subxx128v8,	TOP_subxxx128v8,	TOP_UNDEFINED},
  {TOP_sub128v16,	TOP_subx128v16,	TOP_subxx128v16,	TOP_subxxx128v16,	TOP_UNDEFINED},
  {TOP_sub128v32,	TOP_subx128v32,	TOP_subxx128v32,	TOP_subxxx128v32,	TOP_UNDEFINED},
  {TOP_sub128v64,	TOP_subx128v64,	TOP_subxx128v64,	TOP_subxxx128v64,	TOP_UNDEFINED},
  {TOP_fsub128v32,	TOP_fsubx128v32, TOP_fsubxx128v32,	TOP_fsubxxx128v32,	TOP_UNDEFINED},
  {TOP_fsub128v64,	TOP_fsubx128v64, TOP_fsubxx128v64,	TOP_fsubxxx128v64,	TOP_UNDEFINED},
  {TOP_vsub128v8,	TOP_vsubx128v8,		TOP_vsubxx128v8,	TOP_vsubxxx128v8,	TOP_UNDEFINED},
  {TOP_vsub128v16,	TOP_vsubx128v16,	TOP_vsubxx128v16,	TOP_vsubxxx128v16,	TOP_UNDEFINED},
  {TOP_vsub128v32,	TOP_vsubx128v32,	TOP_vsubxx128v32,	TOP_vsubxxx128v32,	TOP_UNDEFINED},
  {TOP_vsub128v64,	TOP_vsubx128v64,	TOP_vsubxx128v64,	TOP_vsubxxx128v64,	TOP_UNDEFINED},
  {TOP_vfsub128v32,	TOP_vfsubx128v32, 	TOP_vfsubxx128v32,	TOP_vfsubxxx128v32,	TOP_UNDEFINED},
  {TOP_vfsub128v64,	TOP_vfsubx128v64, 	TOP_vfsubxx128v64,	TOP_vfsubxxx128v64,	TOP_UNDEFINED},
  {TOP_mulss,	TOP_mulxss,	TOP_mulxxss,	TOP_mulxxxss,	TOP_UNDEFINED},
  {TOP_mulsd,	TOP_mulxsd,	TOP_mulxxsd,	TOP_mulxxxsd,	TOP_UNDEFINED},
  {TOP_vmulss,	TOP_vmulxss,	TOP_vmulxxss,	TOP_vmulxxxss,	TOP_UNDEFINED},
  {TOP_vmulsd,	TOP_vmulxsd,	TOP_vmulxxsd,	TOP_vmulxxxsd,	TOP_UNDEFINED},
  {TOP_vfaddss,	TOP_vfaddxss,	TOP_vfaddxxss,	TOP_vfaddxxxss,	TOP_UNDEFINED},
  {TOP_vfaddsd,	TOP_vfaddxsd,	TOP_vfaddxxsd,	TOP_vfaddxxxsd,	TOP_UNDEFINED},
  {TOP_vfsqrtss, TOP_vfsqrtxss,	TOP_vfsqrtxxss,	TOP_vfsqrtxxxss, TOP_UNDEFINED},
  {TOP_vfsqrtsd, TOP_vfsqrtxsd,	TOP_vfsqrtxxsd,	TOP_vfsqrtxxxsd, TOP_UNDEFINED},
  {TOP_vfrsqrtss, TOP_vfrsqrtxss, TOP_vfrsqrtxxss, TOP_vfrsqrtxxxss, TOP_UNDEFINED},
  {TOP_vfrsqrt128v32, TOP_vfrsqrtx128v32, TOP_vfrsqrtxx128v32, TOP_vfrsqrtxxx128v32,	TOP_UNDEFINED},
  {TOP_vfsqrt128v64, TOP_vfsqrtx128v64, TOP_vfsqrtxx128v64, TOP_vfsqrtxxx128v64,	TOP_UNDEFINED},
  {TOP_vfsqrt128v32, TOP_vfsqrtx128v32, TOP_vfsqrtxx128v32, TOP_vfsqrtxxx128v32,	TOP_UNDEFINED},
  {TOP_vfrcp128v32, TOP_vfrcpx128v32, TOP_vfrcpxx128v32, TOP_vfrcpxxx128v32,	TOP_UNDEFINED},
  {TOP_fmul128v32, TOP_fmulx128v32, TOP_fmulxx128v32, TOP_fmulxxx128v32,	TOP_UNDEFINED},
  {TOP_fmul128v64, TOP_fmulx128v64, TOP_fmulxx128v64, TOP_fmulxxx128v64,	TOP_UNDEFINED},
  {TOP_cmpgt128v8, TOP_cmpgtx128v8, TOP_cmpgtxx128v8, TOP_cmpgtxxx128v8,	TOP_UNDEFINED},
  {TOP_vfmul128v32, TOP_vfmulx128v32, TOP_vfmulxx128v32, TOP_vfmulxxx128v32,	TOP_UNDEFINED},
  {TOP_vfmul128v64, TOP_vfmulx128v64, TOP_vfmulxx128v64, TOP_vfmulxxx128v64,	TOP_UNDEFINED},
  {TOP_vcmpgt128v8, TOP_vcmpgtx128v8, TOP_vcmpgtxx128v8, TOP_vcmpgtxxx128v8,	TOP_UNDEFINED},
  {TOP_cmpgt128v16,	TOP_cmpgtx128v16,	TOP_cmpgtxx128v16,	TOP_cmpgtxxx128v16,	TOP_UNDEFINED},
  {TOP_cmpgt128v32,	TOP_cmpgtx128v32,	TOP_cmpgtxx128v32,	TOP_cmpgtxxx128v32,	TOP_UNDEFINED},
  {TOP_cmpeq128v8,	TOP_cmpeqx128v8,	TOP_cmpeqxx128v8,	TOP_cmpeqxxx128v8,	TOP_UNDEFINED},
  {TOP_cmpeq128v16,	TOP_cmpeqx128v16,	TOP_cmpeqxx128v16,	TOP_cmpeqxxx128v16,	TOP_UNDEFINED},
  {TOP_cmpeq128v32,	TOP_cmpeqx128v32,	TOP_cmpeqxx128v32,	TOP_cmpeqxxx128v32,	TOP_UNDEFINED},
  {TOP_vcmpgt128v16,	TOP_vcmpgtx128v16,	TOP_vcmpgtxx128v16,	TOP_vcmpgtxxx128v16,	TOP_UNDEFINED},
  {TOP_vcmpgt128v32,	TOP_vcmpgtx128v32,	TOP_vcmpgtxx128v32,	TOP_vcmpgtxxx128v32,	TOP_UNDEFINED},
  {TOP_vcmpeq128v8,	TOP_vcmpeqx128v8,	TOP_vcmpeqxx128v8,	TOP_vcmpeqxxx128v8,	TOP_UNDEFINED},
  {TOP_vcmpeq128v16,	TOP_vcmpeqx128v16,	TOP_vcmpeqxx128v16,	TOP_vcmpeqxxx128v16,	TOP_UNDEFINED},
  {TOP_vcmpeq128v32,	TOP_vcmpeqx128v32,	TOP_vcmpeqxx128v32,	TOP_vcmpeqxxx128v32,	TOP_UNDEFINED},
  {TOP_vmaxs128v8,	TOP_vmaxsx128v8,	TOP_vmaxsxx128v8,	TOP_vmaxsxxx128v8,	TOP_UNDEFINED},
  {TOP_vmaxs128v16,	TOP_vmaxsx128v16,	TOP_vmaxsxx128v16,	TOP_vmaxsxxx128v16,	TOP_UNDEFINED},
  {TOP_vmaxs128v32,	TOP_vmaxsx128v32,	TOP_vmaxsxx128v32,	TOP_vmaxsxxx128v32,	TOP_UNDEFINED},
  {TOP_vmins128v8,	TOP_vminsx128v8,	TOP_vminsxx128v8,	TOP_vminsxxx128v8,	TOP_UNDEFINED},
  {TOP_vmins128v16,	TOP_vminsx128v16,	TOP_vminsxx128v16,	TOP_vminsxxx128v16,	TOP_UNDEFINED},
  {TOP_vmins128v32,	TOP_vminsx128v32,	TOP_vminsxx128v32,	TOP_vminsxxx128v32,	TOP_UNDEFINED},
  {TOP_vmaxu128v8,	TOP_vmaxux128v8,	TOP_vmaxuxx128v8,	TOP_vmaxuxxx128v8,	TOP_UNDEFINED},
  {TOP_vmaxu128v16,	TOP_vmaxux128v16,	TOP_vmaxuxx128v16,	TOP_vmaxuxxx128v16,	TOP_UNDEFINED},
  {TOP_vmaxu128v32,	TOP_vmaxux128v32,	TOP_vmaxuxx128v32,	TOP_vmaxuxxx128v32,	TOP_UNDEFINED},
  {TOP_vminu128v8,	TOP_vminux128v8,	TOP_vminuxx128v8,	TOP_vminuxxx128v8,	TOP_UNDEFINED},
  {TOP_vminu128v16,	TOP_vminux128v16,	TOP_vminuxx128v16,	TOP_vminuxxx128v16,	TOP_UNDEFINED},
  {TOP_divss,	TOP_divxss,	TOP_divxxss,	TOP_divxxxss,	TOP_UNDEFINED},
  {TOP_divsd,	TOP_divxsd,	TOP_divxxsd,	TOP_divxxxsd,	TOP_UNDEFINED},
  {TOP_vdivss,	TOP_vdivxss,	TOP_vdivxxss,	TOP_vdivxxxss,	TOP_UNDEFINED},
  {TOP_vdivsd,	TOP_vdivxsd,	TOP_vdivxxsd,	TOP_vdivxxxsd,	TOP_UNDEFINED},
  {TOP_fdiv128v32,	TOP_fdivx128v32,	TOP_fdivxx128v32,	TOP_fdivxxx128v32,	TOP_UNDEFINED},
  {TOP_fdiv128v64,	TOP_fdivx128v64,	TOP_fdivxx128v64,	TOP_fdivxxx128v64,	TOP_UNDEFINED},
  {TOP_vfdiv128v32,	TOP_vfdivx128v32,	TOP_vfdivxx128v32,	TOP_vfdivxxx128v32,	TOP_UNDEFINED},
  {TOP_vfdiv128v64,	TOP_vfdivx128v64,	TOP_vfdivxx128v64,	TOP_vfdivxxx128v64,	TOP_UNDEFINED},
  {TOP_and8,	TOP_andx8,	TOP_andxx8,	TOP_andxxx8,	TOP_UNDEFINED},
  {TOP_and16,	TOP_andx16,	TOP_andxx16,	TOP_andxxx16,	TOP_UNDEFINED},
  {TOP_and32,	TOP_andx32,	TOP_andxx32,	TOP_andxxx32,	TOP_UNDEFINED},
  {TOP_and64,	TOP_andx64,	TOP_andxx64,	TOP_andxxx64,	TOP_UNDEFINED},
  {TOP_and128v8,	TOP_andx128v8,	        TOP_andxx128v8,	        TOP_andxxx128v8,	TOP_UNDEFINED},
  {TOP_and128v16,	TOP_andx128v16,	        TOP_andxx128v16,	TOP_andxxx128v16,	TOP_UNDEFINED},
  {TOP_and128v32,	TOP_andx128v32,	        TOP_andxx128v32,	TOP_andxxx128v32,	TOP_UNDEFINED },
  {TOP_and128v64,	TOP_andx128v64,	        TOP_andxx128v64,	TOP_andxxx128v64,	TOP_UNDEFINED},
  {TOP_vand128v8,	TOP_vandx128v8,		TOP_vandxx128v8,        TOP_vandxxx128v8,        TOP_UNDEFINED},
  {TOP_vand128v16,	TOP_vandx128v16,	TOP_vandxx128v16,       TOP_vandxxx128v16,        TOP_UNDEFINED},
  {TOP_vand128v32,	TOP_vandx128v32,	TOP_vandxx128v32,       TOP_vandxxx128v32,        TOP_UNDEFINED},
  {TOP_vand128v64,	TOP_vandx128v64,	TOP_vandxx128v64,       TOP_vandxxx128v64,        TOP_UNDEFINED},
  {TOP_fand128v32,	TOP_fandx128v32,	TOP_fandxx128v32,	TOP_fandxxx128v32,	TOP_UNDEFINED},
  {TOP_fand128v64,	TOP_fandx128v64,	TOP_fandxx128v64,	TOP_fandxxx128v64,	TOP_UNDEFINED},
  {TOP_vfand128v32,	TOP_vfandx128v32,	TOP_vfandxx128v32,	TOP_vfandxxx128v32,	TOP_UNDEFINED},
  {TOP_vfand128v64,	TOP_vfandx128v64,	TOP_vfandxx128v64,	TOP_vfandxxx128v64,	TOP_UNDEFINED},
  // andps/andpd share the same load-execute OPs as fand128v32/fand128v64.
  // Must put andps/andpd after fand128v32/fand128v64 so that the load-execute
  // OPs will have fand128v32/fand128v64 as the base mode.
  {TOP_andps,	TOP_fandx128v32,	TOP_fandxx128v32,	TOP_fandxxx128v32,	TOP_UNDEFINED},
  {TOP_andpd,	TOP_fandx128v64,	TOP_fandxx128v64,	TOP_fandxxx128v64,	TOP_UNDEFINED},
  {TOP_vfand128v32,	TOP_vfandx128v32,	TOP_vfandxx128v32,	TOP_vfandxxx128v32,	TOP_UNDEFINED},
  {TOP_vfand128v64,	TOP_vfandx128v64,	TOP_vfandxx128v64,	TOP_vfandxxx128v64,	TOP_UNDEFINED},
  {TOP_or8,	TOP_orx8,	TOP_orxx8,	TOP_orxxx8,	TOP_UNDEFINED},
  {TOP_or16,	TOP_orx16,	TOP_orxx16,	TOP_orxxx16,	TOP_UNDEFINED},
  {TOP_or32,	TOP_orx32,	TOP_orxx32,	TOP_orxxx32,	TOP_UNDEFINED},
  {TOP_or64,	TOP_orx64,	TOP_orxx64,	TOP_orxxx64,	TOP_UNDEFINED},
  {TOP_or128v8,	TOP_orx128v8,	TOP_orxx128v8,	TOP_orxxx128v8,	TOP_UNDEFINED},
  {TOP_or128v16,	TOP_orx128v16,	TOP_orxx128v16,	TOP_orxxx128v16,	TOP_UNDEFINED},
  {TOP_or128v32,	TOP_orx128v32,	TOP_orxx128v32,	TOP_orxxx128v32,	TOP_UNDEFINED},
  {TOP_or128v64,	TOP_orx128v64,	TOP_orxx128v64,	TOP_orxxx128v64,	TOP_UNDEFINED},
  {TOP_for128v32,	TOP_forx128v32,	TOP_forxx128v32,	TOP_forxxx128v32,	TOP_UNDEFINED},
  {TOP_for128v64,	TOP_forx128v64,	TOP_forxx128v64,	TOP_forxxx128v64,	TOP_UNDEFINED},
  {TOP_vor128v8,	TOP_vorx128v8,		TOP_vorxx128v8,		TOP_vorxxx128v8,	TOP_UNDEFINED},
  {TOP_vor128v16,	TOP_vorx128v16,		TOP_vorxx128v16,	TOP_vorxxx128v16,	TOP_UNDEFINED},
  {TOP_vor128v32,	TOP_vorx128v32,		TOP_vorxx128v32,	TOP_vorxxx128v32,	TOP_UNDEFINED},
  {TOP_vor128v64,	TOP_vorx128v64,		TOP_vorxx128v64,	TOP_vorxxx128v64,	TOP_UNDEFINED},
  {TOP_vfor128v32,	TOP_vforx128v32,	TOP_vforxx128v32,	TOP_vforxxx128v32,	TOP_UNDEFINED},
  {TOP_vfor128v64,	TOP_vforx128v64,	TOP_vforxx128v64,	TOP_vforxxx128v64,	TOP_UNDEFINED},
  // orps/orpd share the same load-execute OPs as for128v32/for128v64.  Must
  // put orps/orpd after for128v32/for128v64 so that the load-execute OPs will
  // have for128v32/for128v64 as the base mode.
  {TOP_orps,	        TOP_forx128v32,	        TOP_forxx128v32,	TOP_forxxx128v32,	TOP_UNDEFINED},
  {TOP_orpd,	        TOP_forx128v64,	        TOP_forxx128v64,	TOP_forxxx128v64,	TOP_UNDEFINED},
  {TOP_vfor128v32,	TOP_vforx128v32,	TOP_vforxx128v32,	TOP_vforxxx128v32,	TOP_UNDEFINED},
  {TOP_vfor128v64,	TOP_vforx128v64,	TOP_vforxx128v64,	TOP_vforxxx128v64,	TOP_UNDEFINED},
  {TOP_xor8,	TOP_xorx8,	TOP_xorxx8,	TOP_xorxxx8,	TOP_UNDEFINED},
  {TOP_xor16,	TOP_xorx16,	TOP_xorxx16,	TOP_xorxxx16,	TOP_UNDEFINED},
  {TOP_xor32,	TOP_xorx32,	TOP_xorxx32,	TOP_xorxxx32,	TOP_UNDEFINED},
  {TOP_xor64,	TOP_xorx64,	TOP_xorxx64,	TOP_xorxxx64,	TOP_UNDEFINED},
  {TOP_xor128v8,	TOP_xorx128v8,	TOP_xorxx128v8,	        TOP_xorxxx128v8,	TOP_UNDEFINED},
  {TOP_xor128v16,	TOP_xorx128v16,	TOP_xorxx128v16,	TOP_xorxxx128v16,	TOP_UNDEFINED},
  {TOP_xor128v32,	TOP_xorx128v32,	TOP_xorxx128v32,	TOP_xorxxx128v32,	TOP_UNDEFINED},
  {TOP_xor128v64,	TOP_xorx128v64,	TOP_xorxx128v64,	TOP_xorxxx128v64,	TOP_UNDEFINED},
  {TOP_fxor128v32,	TOP_fxorx128v32,	TOP_fxorxx128v32,	TOP_fxorxxx128v32,	TOP_UNDEFINED},
  {TOP_fxor128v64,	TOP_fxorx128v64,	TOP_fxorxx128v64,	TOP_fxorxxx128v64,	TOP_UNDEFINED},
  {TOP_vxor128v8,	TOP_vxorx128v8,	        TOP_vxorxx128v8,	TOP_vxorxxx128v8,	TOP_UNDEFINED},
  {TOP_vxor128v16,	TOP_vxorx128v16,	TOP_vxorxx128v16,	TOP_vxorxxx128v16,	TOP_UNDEFINED},
  {TOP_vxor128v32,	TOP_vxorx128v32,	TOP_vxorxx128v32,	TOP_vxorxxx128v32,	TOP_UNDEFINED},
  {TOP_vxor128v64,	TOP_vxorx128v64,	TOP_vxorxx128v64,	TOP_vxorxxx128v64,	TOP_UNDEFINED},
  {TOP_vfxor128v32,	TOP_vfxorx128v32,	TOP_vfxorxx128v32,	TOP_vfxorxxx128v32,	TOP_UNDEFINED},
  {TOP_vfxor128v64,	TOP_vfxorx128v64,	TOP_vfxorxx128v64,	TOP_vfxorxxx128v64,	TOP_UNDEFINED},
  // xorps/xorpd share the same load-execute OPs as fxor128v32/fxor128v64.
  // Must put xorps/xorpd after fxor128v32/fxor128v64 so that the load-execute
  // OPs will have fxor128v32/fxor128v64 as the base mode.
  {TOP_xorps,	TOP_fxorx128v32,	        TOP_fxorxx128v32,	TOP_fxorxxx128v32,	TOP_UNDEFINED},
  {TOP_xorpd,	TOP_fxorx128v64,	        TOP_fxorxx128v64,	TOP_fxorxxx128v64,	TOP_UNDEFINED},
  {TOP_vfxor128v32,	TOP_vfxorx128v32,	TOP_vfxorxx128v32,	TOP_vfxorxxx128v32,	TOP_UNDEFINED},
  {TOP_vfxor128v64,	TOP_vfxorx128v64,	TOP_vfxorxx128v64,	TOP_vfxorxxx128v64,	TOP_UNDEFINED},

  {TOP_fmax128v32,	TOP_fmaxx128v32,	TOP_fmaxxx128v32,	TOP_fmaxxxx128v32,	TOP_UNDEFINED},
  {TOP_fmax128v64,	TOP_fmaxx128v64,	TOP_fmaxxx128v64,	TOP_fmaxxxx128v64,	TOP_UNDEFINED},
  {TOP_fmin128v32,	TOP_fminx128v32,	TOP_fminxx128v32,	TOP_fminxxx128v32,	TOP_UNDEFINED},
  {TOP_fmin128v64,	TOP_fminx128v64,	TOP_fminxx128v64,	TOP_fminxxx128v64,	TOP_UNDEFINED},
  {TOP_vfmax128v32,	TOP_vfmaxx128v32,	TOP_vfmaxxx128v32,	TOP_vfmaxxxx128v32,	TOP_UNDEFINED},
  {TOP_vfmax128v64,	TOP_vfmaxx128v64,	TOP_vfmaxxx128v64,	TOP_vfmaxxxx128v64,	TOP_UNDEFINED},
  {TOP_vfmin128v32,	TOP_vfminx128v32,	TOP_vfminxx128v32,	TOP_vfminxxx128v32,	TOP_UNDEFINED},
  {TOP_vfmin128v64,	TOP_vfminx128v64,	TOP_vfminxx128v64,	TOP_vfminxxx128v64,	TOP_UNDEFINED},
  {TOP_cmp8,	TOP_cmpx8,	TOP_cmpxx8,	TOP_cmpxxx8,	TOP_UNDEFINED},
  {TOP_cmp16,	TOP_cmpx16,	TOP_cmpxx16,	TOP_cmpxxx16,	TOP_UNDEFINED},
  {TOP_cmp32,	TOP_cmpx32,	TOP_cmpxx32,	TOP_cmpxxx32,	TOP_UNDEFINED},
  {TOP_cmp64,	TOP_cmpx64,	TOP_cmpxx64,	TOP_cmpxxx64,	TOP_UNDEFINED},

  {TOP_cmpi8,	TOP_cmpxi8,	TOP_cmpxxi8,	TOP_cmpxxxi8,	TOP_UNDEFINED},
  {TOP_cmpi16,	TOP_cmpxi16,	TOP_cmpxxi16,	TOP_cmpxxxi16,	TOP_UNDEFINED},
  {TOP_cmpi32,	TOP_cmpxi32,	TOP_cmpxxi32,	TOP_cmpxxxi32,	TOP_UNDEFINED},
  {TOP_cmpi64,	TOP_cmpxi64,	TOP_cmpxxi64,	TOP_cmpxxxi64,	TOP_UNDEFINED},

  {TOP_test8,	TOP_testx8,	    TOP_testxx8,	TOP_testxxx8,	TOP_UNDEFINED},
  {TOP_test16,	TOP_testx16,	TOP_testxx16,	TOP_testxxx16,	TOP_UNDEFINED},
  {TOP_test32,	TOP_testx32,	TOP_testxx32,	TOP_testxxx32,	TOP_UNDEFINED},
  {TOP_test64,	TOP_testx64,	TOP_testxx64,	TOP_testxxx64,	TOP_UNDEFINED},
  {TOP_comiss,	TOP_comixss,	TOP_comixxss,	TOP_comixxxss,	TOP_UNDEFINED},
  {TOP_comisd,	TOP_comixsd,	TOP_comixxsd,	TOP_comixxxsd, 	TOP_UNDEFINED},
  {TOP_vcomiss,	TOP_vcomixss,	TOP_vcomixxss,	TOP_vcomixxxss,	TOP_UNDEFINED},
  {TOP_vcomisd,	TOP_vcomixsd,	TOP_vcomixxsd,	TOP_vcomixxxsd,	TOP_UNDEFINED},
  // FMA4
  {TOP_vfmaddss,       TOP_vfmaddxss,	    TOP_vfmaddxxss,	  TOP_vfmaddxxxss,          TOP_UNDEFINED},
  {TOP_vfmaddsd,       TOP_vfmaddxsd,	    TOP_vfmaddxxsd,	  TOP_vfmaddxxxsd,          TOP_UNDEFINED},
  {TOP_vfmaddps,       TOP_vfmaddxps,	    TOP_vfmaddxxps,	  TOP_vfmaddxxxps,          TOP_UNDEFINED},
  {TOP_vfmaddpd,       TOP_vfmaddxpd,  	    TOP_vfmaddxxpd,	  TOP_vfmaddxxxpd,          TOP_UNDEFINED},
  {TOP_UNDEFINED,      TOP_vfmaddxrss,      TOP_vfmaddxxrss,      TOP_vfmaddxxxrss,         TOP_UNDEFINED},
  {TOP_UNDEFINED,      TOP_vfmaddxrsd,      TOP_vfmaddxxrsd,      TOP_vfmaddxxxrsd,         TOP_UNDEFINED},
  {TOP_UNDEFINED,      TOP_vfmaddxrps,      TOP_vfmaddxxrps,      TOP_vfmaddxxxrps,         TOP_UNDEFINED},
  {TOP_UNDEFINED,      TOP_vfmaddxrpd,      TOP_vfmaddxxrpd,      TOP_vfmaddxxxrpd,         TOP_UNDEFINED},
  {TOP_vfmaddsubps,    TOP_vfmaddsubxps,    TOP_vfmaddsubxxps,	  TOP_vfmaddsubxxxps,       TOP_UNDEFINED},
  {TOP_vfmaddsubpd,    TOP_vfmaddsubxpd,    TOP_vfmaddsubxxpd,	  TOP_vfmaddsubxxxpd,       TOP_UNDEFINED},
  {TOP_UNDEFINED,      TOP_vfmaddsubxrps,   TOP_vfmaddsubxxrps,   TOP_vfmaddsubxxxrps,      TOP_UNDEFINED},
  {TOP_UNDEFINED,      TOP_vfmaddsubxrpd,   TOP_vfmaddsubxxrpd,   TOP_vfmaddsubxxxrpd,      TOP_UNDEFINED},
  {TOP_vfmsubss,       TOP_vfmsubxss,	    TOP_vfmsubxxss,	  TOP_vfmsubxxxss,          TOP_UNDEFINED},
  {TOP_vfmsubsd,       TOP_vfmsubxsd,	    TOP_vfmsubxxsd,	  TOP_vfmsubxxxsd,          TOP_UNDEFINED},
  {TOP_vfmsubps,       TOP_vfmsubxps,	    TOP_vfmsubxxps,	  TOP_vfmsubxxxps,          TOP_UNDEFINED},
  {TOP_vfmsubpd,       TOP_vfmsubxpd,  	    TOP_vfmsubxxpd, 	  TOP_vfmsubxxxpd,          TOP_UNDEFINED},
  {TOP_UNDEFINED,      TOP_vfmsubxrss,      TOP_vfmsubxxrss,      TOP_vfmsubxxxrss,         TOP_UNDEFINED},
  {TOP_UNDEFINED,      TOP_vfmsubxrsd,      TOP_vfmsubxxrsd,      TOP_vfmsubxxxrsd,         TOP_UNDEFINED},
  {TOP_UNDEFINED,      TOP_vfmsubxrps,      TOP_vfmsubxxrps,      TOP_vfmsubxxxrps,         TOP_UNDEFINED},
  {TOP_UNDEFINED,      TOP_vfmsubxrpd,      TOP_vfmsubxxrpd,      TOP_vfmsubxxxrpd,         TOP_UNDEFINED},
  {TOP_vfmsubaddps,    TOP_vfmsubaddxps,    TOP_vfmsubaddxxps,	  TOP_vfmsubaddxxxps,       TOP_UNDEFINED},
  {TOP_vfmsubaddpd,    TOP_vfmsubaddxpd,    TOP_vfmsubaddxxpd, 	  TOP_vfmsubaddxxxpd,       TOP_UNDEFINED},
  {TOP_UNDEFINED,      TOP_vfmsubaddxrps,   TOP_vfmsubaddxxrps,   TOP_vfmsubaddxxxrps,      TOP_UNDEFINED},
  {TOP_UNDEFINED,      TOP_vfmsubaddxrpd,   TOP_vfmsubaddxxrpd,   TOP_vfmsubaddxxxrpd,      TOP_UNDEFINED},
  {TOP_vfnmaddss,      TOP_vfnmaddxss,	    TOP_vfnmaddxxss,	  TOP_vfnmaddxxxss,         TOP_UNDEFINED},
  {TOP_vfnmaddsd,      TOP_vfnmaddxsd,	    TOP_vfnmaddxxsd,	  TOP_vfnmaddxxxsd,         TOP_UNDEFINED},
  {TOP_vfnmaddps,      TOP_vfnmaddxps,	    TOP_vfnmaddxxps,	  TOP_vfnmaddxxxps,         TOP_UNDEFINED},
  {TOP_vfnmaddpd,      TOP_vfnmaddxpd,      TOP_vfnmaddxxpd,	  TOP_vfnmaddxxxpd,         TOP_UNDEFINED},
  {TOP_UNDEFINED,      TOP_vfnmaddxrss,     TOP_vfnmaddxxrss,     TOP_vfnmaddxxxrss,        TOP_UNDEFINED},
  {TOP_UNDEFINED,      TOP_vfnmaddxrsd,     TOP_vfnmaddxxrsd,     TOP_vfnmaddxxxrsd,        TOP_UNDEFINED},
  {TOP_UNDEFINED,      TOP_vfnmaddxrps,     TOP_vfnmaddxxrps,     TOP_vfnmaddxxxrps,        TOP_UNDEFINED},
  {TOP_UNDEFINED,      TOP_vfnmaddxrpd,     TOP_vfnmaddxxrpd,     TOP_vfnmaddxxxrpd,        TOP_UNDEFINED},
  {TOP_vfnmsubss,      TOP_vfnmsubxss,	    TOP_vfnmsubxxss,	  TOP_vfnmsubxxxss,         TOP_UNDEFINED},
  {TOP_vfnmsubsd,      TOP_vfnmsubxsd,	    TOP_vfnmsubxxsd,	  TOP_vfnmsubxxxsd,         TOP_UNDEFINED},
  {TOP_vfnmsubps,      TOP_vfnmsubxps,	    TOP_vfnmsubxxps,	  TOP_vfnmsubxxxps,         TOP_UNDEFINED},
  {TOP_vfnmsubpd,      TOP_vfnmsubxpd,      TOP_vfnmsubxxpd, 	  TOP_vfnmsubxxxpd,         TOP_UNDEFINED},
  {TOP_UNDEFINED,      TOP_vfnmsubxrss,     TOP_vfnmsubxxrss,     TOP_vfnmsubxxxrss,        TOP_UNDEFINED},
  {TOP_UNDEFINED,      TOP_vfnmsubxrsd,     TOP_vfnmsubxxrsd,     TOP_vfnmsubxxxrsd,        TOP_UNDEFINED},
  {TOP_UNDEFINED,      TOP_vfnmsubxrps,     TOP_vfnmsubxxrps,     TOP_vfnmsubxxxrps,        TOP_UNDEFINED},
  {TOP_UNDEFINED,      TOP_vfnmsubxrpd,     TOP_vfnmsubxxrpd,     TOP_vfnmsubxxxrpd,        TOP_UNDEFINED},
  // FMA3: form1
  {TOP_xfmadd132ss,       TOP_xfmadd132xss,	    TOP_xfmadd132xxss,	  TOP_xfmadd132xxxss,          TOP_UNDEFINED},
  {TOP_xfmadd132sd,       TOP_xfmadd132xsd,	    TOP_xfmadd132xxsd,	  TOP_xfmadd132xxxsd,          TOP_UNDEFINED},
  {TOP_xfmadd132ps,       TOP_xfmadd132xps,	    TOP_xfmadd132xxps,	  TOP_xfmadd132xxxps,          TOP_UNDEFINED},
  {TOP_xfmadd132pd,       TOP_xfmadd132xpd,  	    TOP_xfmadd132xxpd,	  TOP_xfmadd132xxxpd,          TOP_UNDEFINED},
  {TOP_xfmaddsub132ps,    TOP_xfmaddsub132xps,      TOP_xfmaddsub132xxps, TOP_xfmaddsub132xxxps,       TOP_UNDEFINED},
  {TOP_xfmaddsub132pd,    TOP_xfmaddsub132xpd,      TOP_xfmaddsub132xxpd, TOP_xfmaddsub132xxxpd,       TOP_UNDEFINED},
  {TOP_xfmsub132ss,       TOP_xfmsub132xss,	    TOP_xfmsub132xxss,	  TOP_xfmsub132xxxss,          TOP_UNDEFINED},
  {TOP_xfmsub132sd,       TOP_xfmsub132xsd,	    TOP_xfmsub132xxsd,	  TOP_xfmsub132xxxsd,          TOP_UNDEFINED},
  {TOP_xfmsub132ps,       TOP_xfmsub132xps,	    TOP_xfmsub132xxps,	  TOP_xfmsub132xxxps,          TOP_UNDEFINED},
  {TOP_xfmsub132pd,       TOP_xfmsub132xpd,  	    TOP_xfmsub132xxpd, 	  TOP_xfmsub132xxxpd,          TOP_UNDEFINED},
  {TOP_xfmsubadd132ps,    TOP_xfmsubadd132xps,      TOP_xfmsubadd132xxps, TOP_xfmsubadd132xxxps,       TOP_UNDEFINED},
  {TOP_xfmsubadd132pd,    TOP_xfmsubadd132xpd,      TOP_xfmsubadd132xxpd, TOP_xfmsubadd132xxxpd,       TOP_UNDEFINED},
  {TOP_xfnmadd132ss,      TOP_xfnmadd132xss,	    TOP_xfnmadd132xxss,	  TOP_xfnmadd132xxxss,         TOP_UNDEFINED},
  {TOP_xfnmadd132sd,      TOP_xfnmadd132xsd,	    TOP_xfnmadd132xxsd,	  TOP_xfnmadd132xxxsd,         TOP_UNDEFINED},
  {TOP_xfnmadd132ps,      TOP_xfnmadd132xps,	    TOP_xfnmadd132xxps,	  TOP_xfnmadd132xxxps,         TOP_UNDEFINED},
  {TOP_xfnmadd132pd,      TOP_xfnmadd132xpd,        TOP_xfnmadd132xxpd,	  TOP_xfnmadd132xxxpd,         TOP_UNDEFINED},
  {TOP_xfnmsub132ss,      TOP_xfnmsub132xss,	    TOP_xfnmsub132xxss,	  TOP_xfnmsub132xxxss,         TOP_UNDEFINED},
  {TOP_xfnmsub132sd,      TOP_xfnmsub132xsd,	    TOP_xfnmsub132xxsd,	  TOP_xfnmsub132xxxsd,         TOP_UNDEFINED},
  {TOP_xfnmsub132ps,      TOP_xfnmsub132xps,	    TOP_xfnmsub132xxps,	  TOP_xfnmsub132xxxps,         TOP_UNDEFINED},
  {TOP_xfnmsub132pd,      TOP_xfnmsub132xpd,        TOP_xfnmsub132xxpd,   TOP_xfnmsub132xxxpd,         TOP_UNDEFINED},
  // FMA3: form2
  {TOP_xfmadd213ss,       TOP_xfmadd213xss,	    TOP_xfmadd213xxss,	  TOP_xfmadd213xxxss,          TOP_UNDEFINED},
  {TOP_xfmadd213sd,       TOP_xfmadd213xsd,	    TOP_xfmadd213xxsd,	  TOP_xfmadd213xxxsd,          TOP_UNDEFINED},
  {TOP_xfmadd213ps,       TOP_xfmadd213xps,	    TOP_xfmadd213xxps,	  TOP_xfmadd213xxxps,          TOP_UNDEFINED},
  {TOP_xfmadd213pd,       TOP_xfmadd213xpd,  	    TOP_xfmadd213xxpd,	  TOP_xfmadd213xxxpd,          TOP_UNDEFINED},
  {TOP_xfmaddsub213ps,    TOP_xfmaddsub213xps,      TOP_xfmaddsub213xxps, TOP_xfmaddsub213xxxps,       TOP_UNDEFINED},
  {TOP_xfmaddsub213pd,    TOP_xfmaddsub213xpd,      TOP_xfmaddsub213xxpd, TOP_xfmaddsub213xxxpd,       TOP_UNDEFINED},
  {TOP_xfmsub213ss,       TOP_xfmsub213xss,	    TOP_xfmsub213xxss,	  TOP_xfmsub213xxxss,          TOP_UNDEFINED},
  {TOP_xfmsub213sd,       TOP_xfmsub213xsd,	    TOP_xfmsub213xxsd,	  TOP_xfmsub213xxxsd,          TOP_UNDEFINED},
  {TOP_xfmsub213ps,       TOP_xfmsub213xps,	    TOP_xfmsub213xxps,	  TOP_xfmsub213xxxps,          TOP_UNDEFINED},
  {TOP_xfmsub213pd,       TOP_xfmsub213xpd,  	    TOP_xfmsub213xxpd, 	  TOP_xfmsub213xxxpd,          TOP_UNDEFINED},
  {TOP_xfmsubadd213ps,    TOP_xfmsubadd213xps,      TOP_xfmsubadd213xxps, TOP_xfmsubadd213xxxps,       TOP_UNDEFINED},
  {TOP_xfmsubadd213pd,    TOP_xfmsubadd213xpd,      TOP_xfmsubadd213xxpd, TOP_xfmsubadd213xxxpd,       TOP_UNDEFINED},
  {TOP_xfnmadd213ss,      TOP_xfnmadd213xss,	    TOP_xfnmadd213xxss,	  TOP_xfnmadd213xxxss,         TOP_UNDEFINED},
  {TOP_xfnmadd213sd,      TOP_xfnmadd213xsd,	    TOP_xfnmadd213xxsd,	  TOP_xfnmadd213xxxsd,         TOP_UNDEFINED},
  {TOP_xfnmadd213ps,      TOP_xfnmadd213xps,	    TOP_xfnmadd213xxps,	  TOP_xfnmadd213xxxps,         TOP_UNDEFINED},
  {TOP_xfnmadd213pd,      TOP_xfnmadd213xpd,        TOP_xfnmadd213xxpd,	  TOP_xfnmadd213xxxpd,         TOP_UNDEFINED},
  {TOP_xfnmsub213ss,      TOP_xfnmsub213xss,	    TOP_xfnmsub213xxss,	  TOP_xfnmsub213xxxss,         TOP_UNDEFINED},
  {TOP_xfnmsub213sd,      TOP_xfnmsub213xsd,	    TOP_xfnmsub213xxsd,	  TOP_xfnmsub213xxxsd,         TOP_UNDEFINED},
  {TOP_xfnmsub213ps,      TOP_xfnmsub213xps,	    TOP_xfnmsub213xxps,	  TOP_xfnmsub213xxxps,         TOP_UNDEFINED},
  {TOP_xfnmsub213pd,      TOP_xfnmsub213xpd,        TOP_xfnmsub213xxpd,   TOP_xfnmsub213xxxpd,         TOP_UNDEFINED},
  // FMA3: form3
  {TOP_xfmadd231ss,       TOP_xfmadd231xss,	    TOP_xfmadd231xxss,	  TOP_xfmadd231xxxss,          TOP_UNDEFINED},
  {TOP_xfmadd231sd,       TOP_xfmadd231xsd,	    TOP_xfmadd231xxsd,	  TOP_xfmadd231xxxsd,          TOP_UNDEFINED},
  {TOP_xfmadd231ps,       TOP_xfmadd231xps,	    TOP_xfmadd231xxps,	  TOP_xfmadd231xxxps,          TOP_UNDEFINED},
  {TOP_xfmadd231pd,       TOP_xfmadd231xpd,  	    TOP_xfmadd231xxpd,	  TOP_xfmadd231xxxpd,          TOP_UNDEFINED},
  {TOP_xfmaddsub231ps,    TOP_xfmaddsub231xps,      TOP_xfmaddsub231xxps, TOP_xfmaddsub231xxxps,       TOP_UNDEFINED},
  {TOP_xfmaddsub231pd,    TOP_xfmaddsub231xpd,      TOP_xfmaddsub231xxpd, TOP_xfmaddsub231xxxpd,       TOP_UNDEFINED},
  {TOP_xfmsub231ss,       TOP_xfmsub231xss,	    TOP_xfmsub231xxss,	  TOP_xfmsub231xxxss,          TOP_UNDEFINED},
  {TOP_xfmsub231sd,       TOP_xfmsub231xsd,	    TOP_xfmsub231xxsd,	  TOP_xfmsub231xxxsd,          TOP_UNDEFINED},
  {TOP_xfmsub231ps,       TOP_xfmsub231xps,	    TOP_xfmsub231xxps,	  TOP_xfmsub231xxxps,          TOP_UNDEFINED},
  {TOP_xfmsub231pd,       TOP_xfmsub231xpd,  	    TOP_xfmsub231xxpd, 	  TOP_xfmsub231xxxpd,          TOP_UNDEFINED},
  {TOP_xfmsubadd231ps,    TOP_xfmsubadd231xps,      TOP_xfmsubadd231xxps, TOP_xfmsubadd231xxxps,       TOP_UNDEFINED},
  {TOP_xfmsubadd231pd,    TOP_xfmsubadd231xpd,      TOP_xfmsubadd231xxpd, TOP_xfmsubadd231xxxpd,       TOP_UNDEFINED},
  {TOP_xfnmadd231ss,      TOP_xfnmadd231xss,	    TOP_xfnmadd231xxss,	  TOP_xfnmadd231xxxss,         TOP_UNDEFINED},
  {TOP_xfnmadd231sd,      TOP_xfnmadd231xsd,	    TOP_xfnmadd231xxsd,	  TOP_xfnmadd231xxxsd,         TOP_UNDEFINED},
  {TOP_xfnmadd231ps,      TOP_xfnmadd231xps,	    TOP_xfnmadd231xxps,	  TOP_xfnmadd231xxxps,         TOP_UNDEFINED},
  {TOP_xfnmadd231pd,      TOP_xfnmadd231xpd,        TOP_xfnmadd231xxpd,	  TOP_xfnmadd231xxxpd,         TOP_UNDEFINED},
  {TOP_xfnmsub231ss,      TOP_xfnmsub231xss,	    TOP_xfnmsub231xxss,	  TOP_xfnmsub231xxxss,         TOP_UNDEFINED},
  {TOP_xfnmsub231sd,      TOP_xfnmsub231xsd,	    TOP_xfnmsub231xxsd,	  TOP_xfnmsub231xxxsd,         TOP_UNDEFINED},
  {TOP_xfnmsub231ps,      TOP_xfnmsub231xps,	    TOP_xfnmsub231xxps,	  TOP_xfnmsub231xxxps,         TOP_UNDEFINED},
  {TOP_xfnmsub231pd,      TOP_xfnmsub231xpd,        TOP_xfnmsub231xxpd,   TOP_xfnmsub231xxxpd,         TOP_UNDEFINED},
  // end FMA3
  {TOP_icall,	       TOP_icallx,	    TOP_icallxx,	  TOP_icallxxx,	            TOP_UNDEFINED},
  {TOP_ijmp,	       TOP_ijmpx,	    TOP_ijmpxx,	          TOP_ijmpxxx,	            TOP_UNDEFINED},
  {TOP_cvtsd2ss,       TOP_cvtsd2ss_x,	    TOP_cvtsd2ss_xx,	  TOP_cvtsd2ss_xxx,	    TOP_UNDEFINED},
  {TOP_cvtsi2ss,       TOP_cvtsi2ss_x,	    TOP_cvtsi2ss_xx,	  TOP_cvtsi2ss_xxx,	    TOP_UNDEFINED},
  {TOP_cvtsi2ssq,      TOP_cvtsi2ssq_x,     TOP_cvtsi2ssq_xx,	  TOP_cvtsi2ssq_xxx,	    TOP_UNDEFINED},
  {TOP_cvtsi2sd,       TOP_cvtsi2sd_x,	    TOP_cvtsi2sd_xx,	  TOP_cvtsi2sd_xxx,	    TOP_UNDEFINED},
  {TOP_cvtsi2sdq,      TOP_cvtsi2sdq_x,     TOP_cvtsi2sdq_xx,	  TOP_cvtsi2sdq_xxx,	    TOP_UNDEFINED},
  {TOP_cvtdq2pd,       TOP_cvtdq2pd_x,	    TOP_cvtdq2pd_xx,	  TOP_cvtdq2pd_xxx,	    TOP_UNDEFINED},
  {TOP_cvtdq2ps,       TOP_cvtdq2ps_x,	    TOP_cvtdq2ps_xx,	  TOP_cvtdq2ps_xxx,	    TOP_UNDEFINED},
 {TOP_cvtps2pd,	       TOP_cvtps2pd_x,	    TOP_cvtps2pd_xx,	  TOP_cvtps2pd_xxx,	    TOP_UNDEFINED},
  {TOP_cvtpd2ps,       TOP_cvtpd2ps_x,	    TOP_cvtpd2ps_xx,	  TOP_cvtpd2ps_xxx,	    TOP_UNDEFINED},
  {TOP_cvttps2dq,      TOP_cvttps2dq_x,     TOP_cvttps2dq_xx,	  TOP_cvttps2dq_xxx,	    TOP_UNDEFINED},
  {TOP_cvttpd2dq,      TOP_cvttpd2dq_x,     TOP_cvttpd2dq_xx,	  TOP_cvttpd2dq_xxx,	    TOP_UNDEFINED},
  {TOP_vcvtsd2ss,      TOP_vcvtsd2ssx,	    TOP_vcvtsd2ssxx,	  TOP_vcvtsd2ssxxx,	    TOP_UNDEFINED},
  {TOP_vcvtsi2ss,      TOP_vcvtsi2ssx,	    TOP_vcvtsi2ssxx,	  TOP_vcvtsi2ssxxx,	    TOP_UNDEFINED},
  {TOP_vcvtsi2ssq,     TOP_vcvtsi2ssqx,     TOP_vcvtsi2ssqxx,	  TOP_vcvtsi2ssqxxx,	    TOP_UNDEFINED},
  {TOP_vcvtsi2sd,      TOP_vcvtsi2sdx,      TOP_vcvtsi2sdxx,	  TOP_vcvtsi2sdxxx,	    TOP_UNDEFINED},
  {TOP_vcvtsi2sdq,     TOP_vcvtsi2sdqx,     TOP_vcvtsi2sdqxx,	  TOP_vcvtsi2sdqxxx,	    TOP_UNDEFINED},
  {TOP_vcvtdq2pd,      TOP_vcvtdq2pdx,	    TOP_vcvtdq2pdxx,	  TOP_vcvtdq2pdxxx,	    TOP_UNDEFINED},
  {TOP_vcvtdq2ps,      TOP_vcvtdq2psx,	    TOP_vcvtdq2psxx,	  TOP_vcvtdq2psxxx,	    TOP_UNDEFINED},
 {TOP_vcvtps2pd,       TOP_vcvtps2pdx,	    TOP_vcvtps2pdxx,	  TOP_vcvtps2pdxxx,	    TOP_UNDEFINED},
  {TOP_vcvtpd2dq,      TOP_vcvtpd2dqx,	    TOP_vcvtpd2dqxx,	  TOP_vcvtpd2dqxxx,	    TOP_UNDEFINED},
  {TOP_vcvtpd2dqy,     TOP_vcvtpd2dqyx,	    TOP_vcvtpd2dqyxx,	  TOP_vcvtpd2dqyxxx,	    TOP_UNDEFINED},
  {TOP_vcvtpd2ps,      TOP_vcvtpd2psx,	    TOP_vcvtpd2psxx,	  TOP_vcvtpd2psxxx,	    TOP_UNDEFINED},
  {TOP_vcvtpd2psy,     TOP_vcvtpd2psyx,	    TOP_vcvtpd2psyxx,	  TOP_vcvtpd2psyxxx,	    TOP_UNDEFINED},
  {TOP_vcvttps2dq,     TOP_vcvttps2dqx,     TOP_vcvttps2dqxx,	  TOP_vcvttps2dqxxx,	    TOP_UNDEFINED},
  {TOP_vcvttpd2dq,     TOP_vcvttpd2dqx,     TOP_vcvttpd2dqxx,	  TOP_vcvttpd2dqxxx,	    TOP_UNDEFINED},
  {TOP_vcvttpd2dqy,    TOP_vcvttpd2dqyx,    TOP_vcvttpd2dqyxx,	  TOP_vcvttpd2dqyxxx,	    TOP_UNDEFINED},
  // SSE 4.1:
  {TOP_mpsadbw,        TOP_mpsadbwx,        TOP_mpsadbwxx,        TOP_mpsadbwxxx,           TOP_UNDEFINED},
  {TOP_muldq,          TOP_muldqx,          TOP_muldqxx,          TOP_muldqxxx,             TOP_UNDEFINED},
  {TOP_mul128v32,      TOP_mulx128v32,      TOP_mulxx128v32,      TOP_mulxxx128v32,         TOP_UNDEFINED},
  {TOP_fdp128v32,      TOP_fdpx128v32,      TOP_fdpxx128v32,      TOP_fdpxxx128v32,         TOP_UNDEFINED},
  {TOP_fdp128v64,      TOP_fdpx128v64,      TOP_fdpxx128v64,      TOP_fdpxxx128v64,      TOP_UNDEFINED},
  {TOP_fblend128v32,   TOP_fblendx128v32,   TOP_fblendxx128v32,   TOP_fblendxxx128v32,      TOP_UNDEFINED},
  {TOP_fblend128v64,   TOP_fblendx128v64,   TOP_fblendxx128v64,   TOP_fblendxxx128v64,      TOP_UNDEFINED},
  {TOP_fblendv128v32,  TOP_fblendvx128v32,  TOP_fblendvxx128v32,  TOP_fblendvxxx128v32,     TOP_UNDEFINED},
  {TOP_fblendv128v64,  TOP_fblendvx128v64,  TOP_fblendvxx128v64,  TOP_fblendvxxx128v64,     TOP_UNDEFINED},
  {TOP_blendv128v8,    TOP_blendvx128v8,    TOP_blendvxx128v8,    TOP_blendvxxx128v8,       TOP_UNDEFINED},
  {TOP_blend128v16,    TOP_blendx128v16,    TOP_blendxx128v16,    TOP_blendxxx128v16,       TOP_UNDEFINED},
  {TOP_minu128v8,      TOP_minux128v8,      TOP_minuxx128v8,      TOP_minuxxx128v8,         TOP_UNDEFINED},
  {TOP_mins128v8,      TOP_minsx128v8,      TOP_minsxx128v8,      TOP_minsxxx128v8,         TOP_UNDEFINED},
  {TOP_maxu128v8,      TOP_maxux128v8,      TOP_maxuxx128v8,      TOP_maxuxxx128v8,         TOP_UNDEFINED},
  {TOP_maxs128v8,      TOP_maxsx128v8,      TOP_maxsxx128v8,      TOP_maxsxxx128v8,         TOP_UNDEFINED},
  {TOP_minu128v16,     TOP_minux128v16,     TOP_minuxx128v16,     TOP_minuxxx128v16,        TOP_UNDEFINED},
  {TOP_maxu128v16,     TOP_maxux128v16,     TOP_maxuxx128v16,     TOP_maxuxxx128v16,        TOP_UNDEFINED},
  {TOP_mins128v16,     TOP_minsx128v16,     TOP_minsxx128v16,     TOP_minsxxx128v16,        TOP_UNDEFINED},
  {TOP_maxs128v16,     TOP_maxsx128v16,     TOP_maxsxx128v16,     TOP_maxsxxx128v16,        TOP_UNDEFINED},
  {TOP_minu128v32,     TOP_minux128v32,     TOP_minuxx128v32,     TOP_minuxxx128v32,        TOP_UNDEFINED},
  {TOP_maxu128v32,     TOP_maxux128v32,     TOP_maxuxx128v32,     TOP_maxuxxx128v32,        TOP_UNDEFINED},
  {TOP_mins128v32,     TOP_minsx128v32,     TOP_minsxx128v32,     TOP_minsxxx128v32,        TOP_UNDEFINED},
  {TOP_maxs128v32,     TOP_maxsx128v32,     TOP_maxsxx128v32,     TOP_maxsxxx128v32,        TOP_UNDEFINED},
  {TOP_round128v32,    TOP_roundx128v32,    TOP_roundxx128v32,    TOP_roundxxx128v32,       TOP_UNDEFINED},
  {TOP_roundss,        TOP_roundxss,        TOP_roundxxss,        TOP_roundxxxss,           TOP_UNDEFINED},
  {TOP_round128v64,    TOP_roundx128v64,    TOP_roundxx128v64,    TOP_roundxxx128v64,       TOP_UNDEFINED},
  {TOP_roundsd,        TOP_roundxsd,        TOP_roundxxsd,        TOP_roundxxxsd,           TOP_UNDEFINED},
  {TOP_finsr128v32,    TOP_finsrx128v32,    TOP_finsrxx128v32,    TOP_finsrxxx128v32,       TOP_UNDEFINED},
  {TOP_insr128v8,      TOP_insrx128v8,      TOP_insrxx128v8,      TOP_insrxxx128v8,         TOP_UNDEFINED},
  {TOP_insr128v16,     TOP_insrx128v16,     TOP_insrxx128v16,     TOP_insrxxx128v16,        TOP_UNDEFINED},
  {TOP_insr128v32,     TOP_insrx128v32,     TOP_insrxx128v32,     TOP_insrxxx128v32,        TOP_UNDEFINED},
  {TOP_insr128v64,     TOP_insrx128v64,     TOP_insrxx128v64,     TOP_insrxxx128v64,        TOP_UNDEFINED},
  {TOP_fextr128v32,    TOP_fextrx128v32,    TOP_fextrxx128v32,    TOP_fextrxxx128v32,       TOP_UNDEFINED},
  {TOP_extr128v8,      TOP_extrx128v8,      TOP_extrxx128v8,      TOP_extrxxx128v8,         TOP_UNDEFINED},
  {TOP_extr128v16,     TOP_extrx128v16,     TOP_extrxx128v16,     TOP_extrxxx128v16,        TOP_UNDEFINED},
  {TOP_extr128v32,     TOP_extrx128v32,     TOP_extrxx128v32,     TOP_extrxxx128v32,        TOP_UNDEFINED},
  {TOP_extr128v64,     TOP_extrx128v64,     TOP_extrxx128v64,     TOP_extrxxx128v64,        TOP_UNDEFINED},
  {TOP_pmovsxbw,       TOP_pmovsxbwx,       TOP_pmovsxbwxx,       TOP_pmovsxbwxxx,          TOP_UNDEFINED},
  {TOP_pmovzxbw,       TOP_pmovzxbwx,       TOP_pmovzxbwxx,       TOP_pmovzxbwxxx,          TOP_UNDEFINED},
  {TOP_pmovsxbd,       TOP_pmovsxbdx,       TOP_pmovsxbdxx,       TOP_pmovsxbdxxx,          TOP_UNDEFINED},
  {TOP_pmovzxbd,       TOP_pmovzxbdx,       TOP_pmovzxbdxx,       TOP_pmovzxbdxxx,          TOP_UNDEFINED},
  {TOP_pmovsxbq,       TOP_pmovsxbqx,       TOP_pmovsxbqxx,       TOP_pmovsxbqxxx,          TOP_UNDEFINED},
  {TOP_pmovzxbq,       TOP_pmovzxbqx,       TOP_pmovzxbqxx,       TOP_pmovzxbqxxx,          TOP_UNDEFINED},
  {TOP_pmovsxwd,       TOP_pmovsxwdx,       TOP_pmovsxwdxx,       TOP_pmovsxwdxxx,          TOP_UNDEFINED},
  {TOP_pmovzxwd,       TOP_pmovzxwdx,       TOP_pmovzxwdxx,       TOP_pmovzxwdxxx,          TOP_UNDEFINED},
  {TOP_pmovsxwq,       TOP_pmovsxwqx,       TOP_pmovsxwqxx,       TOP_pmovsxwqxxx,          TOP_UNDEFINED},
  {TOP_pmovzxwq,       TOP_pmovzxwqx,       TOP_pmovzxwqxx,       TOP_pmovzxwqxxx,          TOP_UNDEFINED},
  {TOP_pmovsxdq,       TOP_pmovsxdqx,       TOP_pmovsxdqxx,       TOP_pmovsxdqxxx,          TOP_UNDEFINED},
  {TOP_pmovzxdq,       TOP_pmovzxdqx,       TOP_pmovzxdqxx,       TOP_pmovzxdqxxx,          TOP_UNDEFINED},
  {TOP_ptest128,       TOP_ptestx128,       TOP_ptestxx128,       TOP_ptestxxx128,          TOP_UNDEFINED},
  {TOP_cmpeq128v64,    TOP_cmpeqx128v64,    TOP_cmpeqxx128v64,    TOP_cmpeqxxx128v64,       TOP_UNDEFINED},
  {TOP_packusdw,       TOP_packusdwx,       TOP_packusdwxx,       TOP_packusdwxxx,          TOP_UNDEFINED},
  // SSE4.2
  {TOP_crc32b,         TOP_crc32bx,         TOP_crc32bxx,         TOP_crc32bxxx,            TOP_UNDEFINED},
  {TOP_crc32w,         TOP_crc32wx,         TOP_crc32wxx,         TOP_crc32wxxx,            TOP_UNDEFINED},
  {TOP_crc32d,         TOP_crc32dx,         TOP_crc32dxx,         TOP_crc32dxxx,            TOP_UNDEFINED},
  {TOP_crc32q,         TOP_crc32qx,         TOP_crc32qxx,         TOP_crc32qxxx,            TOP_UNDEFINED},
  {TOP_cmpestri,       TOP_cmpestrix,       TOP_cmpestrixx,       TOP_cmpestrixxx,          TOP_UNDEFINED},
  {TOP_cmpestrm,       TOP_cmpestrmx,       TOP_cmpestrmxx,       TOP_cmpestrmxxx,          TOP_UNDEFINED},
  {TOP_cmpistri,       TOP_cmpistrix,       TOP_cmpistrixx,       TOP_cmpistrixxx,          TOP_UNDEFINED},
  {TOP_cmpistrm,       TOP_cmpistrmx,       TOP_cmpistrmxx,       TOP_cmpistrmxxx,          TOP_UNDEFINED},
  {TOP_cmpgt128v64,    TOP_cmpgtx128v64,    TOP_cmpgtxx128v64,    TOP_cmpgtxxx128v64,       TOP_UNDEFINED},
  {TOP_popcnt16,       TOP_popcntx16,       TOP_popcntxx16,       TOP_popcntxxx16,          TOP_UNDEFINED},
  {TOP_popcnt32,       TOP_popcntx32,       TOP_popcntxx32,       TOP_popcntxxx32,          TOP_UNDEFINED},
  {TOP_popcnt64,       TOP_popcntx64,       TOP_popcntxx64,       TOP_popcntxxx64,          TOP_UNDEFINED},
  // SSSE3
  {TOP_psign128v8,     TOP_psignx128v8,     TOP_psignxx128v8,     TOP_psignxxx128v8,        TOP_UNDEFINED},
  {TOP_psign128v16,    TOP_psignx128v16,    TOP_psignxx128v16,    TOP_psignxxx128v16,       TOP_UNDEFINED},
  {TOP_psign128v32,    TOP_psignx128v32,    TOP_psignxx128v32,    TOP_psignxxx128v32,       TOP_UNDEFINED},
  {TOP_pabs128v8,      TOP_pabsx128v8,      TOP_pabsxx128v8,      TOP_pabsxxx128v8,         TOP_UNDEFINED},
  {TOP_pabs128v16,     TOP_pabsx128v16,     TOP_pabsxx128v16,     TOP_pabsxxx128v16,        TOP_UNDEFINED},
  {TOP_pabs128v32,     TOP_pabsx128v32,     TOP_pabsxx128v32,     TOP_pabsxxx128v32,        TOP_UNDEFINED},
  {TOP_palignr128,     TOP_palignrx128,     TOP_palignrxx128,     TOP_palignrxxx128,        TOP_UNDEFINED},
  {TOP_pshuf128v8,     TOP_pshufx128v8,     TOP_pshufxx128v8,     TOP_pshufxxx128v8,        TOP_UNDEFINED},
  {TOP_pmulhrsw128,    TOP_pmulhrswx128,    TOP_pmulhrswxx128,    TOP_pmulhrswxxx128,        TOP_UNDEFINED},
  {TOP_pmaddubsw128,   TOP_pmaddubswx128,   TOP_pmaddubswxx128,   TOP_pmaddubswxxx128,       TOP_UNDEFINED},
  {TOP_phsub128v16,    TOP_phsubx128v16,    TOP_phsubxx128v16,    TOP_phsubxxx128v16,       TOP_UNDEFINED},
  {TOP_phsub128v32,    TOP_phsubx128v32,    TOP_phsubxx128v32,    TOP_phsubxxx128v32,       TOP_UNDEFINED},
  {TOP_phsubs128v16,   TOP_phsubsx128v16,   TOP_phsubsxx128v16,   TOP_phsubsxxx128v16,      TOP_UNDEFINED},
  {TOP_phadd128v16,    TOP_phaddx128v16,    TOP_phaddxx128v16,    TOP_phaddxxx128v16,       TOP_UNDEFINED},
  {TOP_phadd128v32,    TOP_phaddx128v32,    TOP_phaddxx128v32,    TOP_phaddxxx128v32,       TOP_UNDEFINED},
  {TOP_phadds128v16,   TOP_phaddsx128v16,   TOP_phaddsxx128v16,   TOP_phaddsxxx128v16,      TOP_UNDEFINED},
  // VEX SSE 4.1:
  {TOP_vmpsadbw,       TOP_vmpsadbwx,       TOP_vmpsadbwxx,       TOP_vmpsadbwxxx,          TOP_UNDEFINED},
  {TOP_vmuldq,         TOP_vmuldqx,         TOP_vmuldqxx,         TOP_vmuldqxxx,            TOP_UNDEFINED},
  {TOP_vmulld,         TOP_vmulldx,         TOP_vmulldxx,         TOP_vmulldxxx,            TOP_UNDEFINED},
  {TOP_vmul128v16,     TOP_vmulx128v16,     TOP_vmulxx128v16,     TOP_vmulxxx128v16,            TOP_UNDEFINED},
  {TOP_vfdp128v32,     TOP_vfdpx128v32,     TOP_vfdpxx128v32,     TOP_vfdpxxx128v32,        TOP_UNDEFINED},
  {TOP_vfdp128v64,     TOP_vfdpx128v64,     TOP_vfdpxx128v64,     TOP_vfdpxxx128v64,         TOP_UNDEFINED},
  {TOP_vfblend128v32,  TOP_vfblendx128v32,  TOP_vfblendxx128v32,  TOP_vfblendxxx128v32,     TOP_UNDEFINED},
  {TOP_vfblend128v64,  TOP_vfblendx128v64,  TOP_vfblendxx128v64,  TOP_vfblendxxx128v64,     TOP_UNDEFINED},
  {TOP_vfblendv128v32, TOP_vfblendvx128v32, TOP_vfblendvxx128v32, TOP_vfblendvxxx128v32,    TOP_UNDEFINED},
  {TOP_vfblendv128v64, TOP_vfblendvx128v64, TOP_vfblendvxx128v64, TOP_vfblendvxxx128v64,    TOP_UNDEFINED},
  {TOP_vblendv128v8,   TOP_vblendvx128v8,   TOP_vblendvxx128v8,   TOP_vblendvxxx128v8,      TOP_UNDEFINED},
  {TOP_vblend128v16,   TOP_vblendx128v16,   TOP_vblendxx128v16,   TOP_vblendxxx128v16,      TOP_UNDEFINED},
  {TOP_vmins128v8,     TOP_vminsx128v8,     TOP_vminsxx128v8,     TOP_vminsxxx128v8,        TOP_UNDEFINED},
  {TOP_vmaxs128v8,     TOP_vmaxsx128v8,     TOP_vmaxsxx128v8,     TOP_vmaxsxxx128v8,        TOP_UNDEFINED},
  {TOP_vminu128v16,    TOP_vminux128v16,    TOP_vminuxx128v16,    TOP_vminuxxx128v16,       TOP_UNDEFINED},
  {TOP_vmaxu128v16,    TOP_vmaxux128v16,    TOP_vmaxuxx128v16,    TOP_vmaxuxxx128v16,       TOP_UNDEFINED},
  {TOP_vminu128v32,    TOP_vminux128v32,    TOP_vminuxx128v32,    TOP_vminuxxx128v32,       TOP_UNDEFINED},
  {TOP_vmaxu128v32,    TOP_vmaxux128v32,    TOP_vmaxuxx128v32,    TOP_vmaxuxxx128v32,       TOP_UNDEFINED},
  {TOP_vmins128v32,    TOP_vminsx128v32,    TOP_vminsxx128v32,    TOP_vminsxxx128v32,       TOP_UNDEFINED},
  {TOP_vmaxs128v32,    TOP_vmaxsx128v32,    TOP_vmaxsxx128v32,    TOP_vmaxsxxx128v32,       TOP_UNDEFINED},
  {TOP_vround128v32,   TOP_vroundx128v32,   TOP_vroundxx128v32,   TOP_vroundxxx128v32,      TOP_UNDEFINED},
  {TOP_vroundss,       TOP_vroundxss,       TOP_vroundxxss,       TOP_vroundxxxss,          TOP_UNDEFINED},
  {TOP_vround128v64,   TOP_vroundx128v64,   TOP_vroundxx128v64,   TOP_vroundxxx128v64,      TOP_UNDEFINED},
  {TOP_vroundsd,       TOP_vroundxsd,       TOP_vroundxxsd,       TOP_vroundxxxsd,          TOP_UNDEFINED},
  {TOP_vfinsr128v32,   TOP_vfinsrx128v32,   TOP_vfinsrxx128v32,   TOP_vfinsrxxx128v32,      TOP_UNDEFINED},
  {TOP_vinsr128v8,     TOP_vinsrx128v8,     TOP_vinsrxx128v8,     TOP_vinsrxxx128v8,        TOP_UNDEFINED},
  {TOP_vinsr128v16,    TOP_vinsrx128v16,    TOP_vinsrxx128v16,    TOP_vinsrxxx128v16,       TOP_UNDEFINED},
  {TOP_vinsr128v32,    TOP_vinsrx128v32,    TOP_vinsrxx128v32,    TOP_vinsrxxx128v32,       TOP_UNDEFINED},
  {TOP_vinsr128v64,    TOP_vinsrx128v64,    TOP_vinsrxx128v64,    TOP_vinsrxxx128v64,       TOP_UNDEFINED},
  {TOP_vfextr128v32,   TOP_vfextrx128v32,   TOP_vfextrxx128v32,   TOP_vfextrxxx128v32,      TOP_UNDEFINED},
  {TOP_vextr128v8,     TOP_vextrx128v8,     TOP_vextrxx128v8,     TOP_vextrxxx128v8,        TOP_UNDEFINED},
  {TOP_vextr128v16,    TOP_vextrx128v16,    TOP_vextrxx128v16,    TOP_vextrxxx128v16,       TOP_UNDEFINED},
  {TOP_vextr128v32,    TOP_vextrx128v32,    TOP_vextrxx128v32,    TOP_vextrxxx128v32,       TOP_UNDEFINED},
  {TOP_vextr128v64,    TOP_vextrx128v64,    TOP_vextrxx128v64,    TOP_vextrxxx128v64,       TOP_UNDEFINED},
  {TOP_vpmovsxbw,      TOP_vpmovsxbwx,      TOP_vpmovsxbwxx,      TOP_vpmovsxbwxxx,         TOP_UNDEFINED},
  {TOP_vpmovzxbw,      TOP_vpmovzxbwx,      TOP_vpmovzxbwxx,      TOP_vpmovzxbwxxx,         TOP_UNDEFINED},
  {TOP_vpmovsxbd,      TOP_vpmovsxbdx,      TOP_vpmovsxbdxx,      TOP_vpmovsxbdxxx,         TOP_UNDEFINED},
  {TOP_vpmovzxbd,      TOP_vpmovzxbdx,      TOP_vpmovzxbdxx,      TOP_vpmovzxbdxxx,         TOP_UNDEFINED},
  {TOP_vpmovsxbq,      TOP_vpmovsxbqx,      TOP_vpmovsxbqxx,      TOP_vpmovsxbqxxx,         TOP_UNDEFINED},
  {TOP_vpmovzxbq,      TOP_vpmovzxbqx,      TOP_vpmovzxbqxx,      TOP_vpmovzxbqxxx,         TOP_UNDEFINED},
  {TOP_vpmovsxwd,      TOP_vpmovsxwdx,      TOP_vpmovsxwdxx,      TOP_vpmovsxwdxxx,         TOP_UNDEFINED},
  {TOP_vpmovzxwd,      TOP_vpmovzxwdx,      TOP_vpmovzxwdxx,      TOP_vpmovzxwdxxx,         TOP_UNDEFINED},
  {TOP_vpmovsxwq,      TOP_vpmovsxwqx,      TOP_vpmovsxwqxx,      TOP_vpmovsxwqxxx,         TOP_UNDEFINED},
  {TOP_vpmovzxwq,      TOP_vpmovzxwqx,      TOP_vpmovzxwqxx,      TOP_vpmovzxwqxxx,         TOP_UNDEFINED},
  {TOP_vpmovsxdq,      TOP_vpmovsxdqx,      TOP_vpmovsxdqxx,      TOP_vpmovsxdqxxx,         TOP_UNDEFINED},
  {TOP_vpmovzxdq,      TOP_vpmovzxdqx,      TOP_vpmovzxdqxx,      TOP_vpmovzxdqxxx,         TOP_UNDEFINED},
  {TOP_vptest128,      TOP_vptestx128,      TOP_vptestxx128,      TOP_vptestxxx128,         TOP_UNDEFINED},
  {TOP_vcmpeq128v64,   TOP_vcmpeqx128v64,   TOP_vcmpeqxx128v64,   TOP_vcmpeqxxx128v64,      TOP_UNDEFINED},
  {TOP_vpackusdw,      TOP_vpackusdwx,      TOP_vpackusdwxx,      TOP_vpackusdwxxx,         TOP_UNDEFINED},
  // VEX SSE4.2
  {TOP_vcmpestri,      TOP_vcmpestrix,      TOP_vcmpestrixx,      TOP_vcmpestrixxx,         TOP_UNDEFINED},
  {TOP_vcmpestrm,      TOP_vcmpestrmx,      TOP_vcmpestrmxx,      TOP_vcmpestrmxxx,         TOP_UNDEFINED},
  {TOP_vcmpistri,      TOP_vcmpistrix,      TOP_vcmpistrixx,      TOP_vcmpistrixxx,         TOP_UNDEFINED},
  {TOP_vcmpistrm,      TOP_vcmpistrmx,      TOP_vcmpistrmxx,      TOP_vcmpistrmxxx,         TOP_UNDEFINED},
  {TOP_vcmpgt128v64,   TOP_vcmpgtx128v64,   TOP_vcmpgtxx128v64,   TOP_vcmpgtxxx128v64,      TOP_UNDEFINED},
  // VEX SSSE3
  {TOP_vpsign128v8,    TOP_vpsignx128v8,    TOP_vpsignxx128v8,    TOP_vpsignxxx128v8,       TOP_UNDEFINED},
  {TOP_vpsign128v16,   TOP_vpsignx128v16,   TOP_vpsignxx128v16,   TOP_vpsignxxx128v16,      TOP_UNDEFINED},
  {TOP_vpsign128v32,   TOP_vpsignx128v32,   TOP_vpsignxx128v32,   TOP_vpsignxxx128v32,      TOP_UNDEFINED},
  {TOP_vabs128v8,      TOP_vabsx128v8,      TOP_vabsxx128v8,      TOP_vabsxxx128v8,        TOP_UNDEFINED},
  {TOP_vabs128v16,     TOP_vabsx128v16,     TOP_vabsxx128v16,     TOP_vabsxxx128v16,       TOP_UNDEFINED},
  {TOP_vabs128v32,     TOP_vabsx128v32,     TOP_vabsxx128v32,     TOP_vabsxxx128v32,       TOP_UNDEFINED},
  {TOP_vpalignr128,    TOP_vpalignrx128,    TOP_vpalignrxx128,    TOP_vpalignrxxx128,       TOP_UNDEFINED},
  {TOP_vpshuf128v8,    TOP_vpshufx128v8,    TOP_vpshufxx128v8,    TOP_vpshufxxx128v8,       TOP_UNDEFINED},
  {TOP_vmulhrsw,       TOP_vmulhrswx,       TOP_vmulhrswxx,       TOP_vmulhrswxxx,          TOP_UNDEFINED},
  {TOP_vpmaddubsw128,  TOP_vpmaddubswx128,  TOP_vpmaddubswxx128,  TOP_vpmaddubswxxx128,     TOP_UNDEFINED},
  {TOP_vphsub128v16,   TOP_vphsubx128v16,   TOP_vphsubxx128v16,   TOP_vphsubxxx128v16,      TOP_UNDEFINED},
  {TOP_vphsub128v32,   TOP_vphsubx128v32,   TOP_vphsubxx128v32,   TOP_vphsubxxx128v32,      TOP_UNDEFINED},
  {TOP_vphsubs128v16,  TOP_vphsubsx128v16,  TOP_vphsubsxx128v16,  TOP_vphsubsxxx128v16,     TOP_UNDEFINED},
  {TOP_vphadd128v16,   TOP_vphaddx128v16,   TOP_vphaddxx128v16,   TOP_vphaddxxx128v16,      TOP_UNDEFINED},
  {TOP_vphadd128v32,   TOP_vphaddx128v32,   TOP_vphaddxx128v32,   TOP_vphaddxxx128v32,      TOP_UNDEFINED},
  {TOP_vphadds128v16,  TOP_vphaddsx128v16,  TOP_vphaddsxx128v16,  TOP_vphaddsxxx128v16,     TOP_UNDEFINED},
  // XOP
  {TOP_vfrczpd,        TOP_vfrczpdx,        TOP_vfrczpdxx,        TOP_vfrczpdxxx,           TOP_UNDEFINED},
  {TOP_vfrczps,        TOP_vfrczpsx,        TOP_vfrczpsxx,        TOP_vfrczpsxxx,           TOP_UNDEFINED},
  {TOP_vfrczsd,        TOP_vfrczsdx,        TOP_vfrczsdxx,        TOP_vfrczsdxxx,           TOP_UNDEFINED},
  {TOP_vfrczss,        TOP_vfrczssx,        TOP_vfrczssxx,        TOP_vfrczssxxx,           TOP_UNDEFINED},
  {TOP_vpcmov,        TOP_vpcmovx,          TOP_vpcmovxx,         TOP_vpcmovxxx,            TOP_UNDEFINED},
  {TOP_UNDEFINED,     TOP_vpcmovxr,         TOP_vpcmovxxr,        TOP_vpcmovxxxr,           TOP_UNDEFINED},
  {TOP_vpcomb,        TOP_vpcombx,          TOP_vpcombxx,         TOP_vpcombxxx,            TOP_UNDEFINED},
  {TOP_vpcomd,        TOP_vpcomdx,          TOP_vpcomdxx,         TOP_vpcomdxxx,            TOP_UNDEFINED},
  {TOP_vpcomq,        TOP_vpcomqx,          TOP_vpcomqxx,         TOP_vpcomqxxx,            TOP_UNDEFINED},
  {TOP_vpcomub,       TOP_vpcomubx,         TOP_vpcomubxx,        TOP_vpcomubxxx,           TOP_UNDEFINED},
  {TOP_vpcomud,       TOP_vpcomudx,         TOP_vpcomudxx,        TOP_vpcomudxxx,           TOP_UNDEFINED},
  {TOP_vpcomuq,       TOP_vpcomuqx,         TOP_vpcomuqxx,        TOP_vpcomuqxxx,           TOP_UNDEFINED},
  {TOP_vpcomuw,       TOP_vpcomuwx,         TOP_vpcomuwxx,        TOP_vpcomuwxxx,           TOP_UNDEFINED},
  {TOP_vpcomw,        TOP_vpcomwx,          TOP_vpcomwxx,         TOP_vpcomwxxx,            TOP_UNDEFINED},
  {TOP_vphaddbd,      TOP_vphaddbdx,        TOP_vphaddbdxx,       TOP_vphaddbdxxx,          TOP_UNDEFINED},
  {TOP_vphaddbq,      TOP_vphaddbqx,        TOP_vphaddbqxx,       TOP_vphaddbqxxx,          TOP_UNDEFINED},
  {TOP_vphaddbw,      TOP_vphaddbwx,        TOP_vphaddbwxx,       TOP_vphaddbwxxx,          TOP_UNDEFINED},
  {TOP_vphadddq,      TOP_vphadddqx,        TOP_vphadddqxx,       TOP_vphadddqxxx,          TOP_UNDEFINED},
  {TOP_vphaddubd,     TOP_vphaddubdx,       TOP_vphaddubdxx,      TOP_vphaddubdxxx,         TOP_UNDEFINED},
  {TOP_vphaddubq,     TOP_vphaddubqx,       TOP_vphaddubqxx,      TOP_vphaddubqxxx,         TOP_UNDEFINED},
  {TOP_vphaddubw,     TOP_vphaddubwx,       TOP_vphaddubwxx,      TOP_vphaddubwxxx,         TOP_UNDEFINED},
  {TOP_vphaddudq,     TOP_vphaddudqx,       TOP_vphaddudqxx,      TOP_vphaddudqxxx,         TOP_UNDEFINED},
  {TOP_vphadduwd,     TOP_vphadduwdx,       TOP_vphadduwdxx,      TOP_vphadduwdxxx,         TOP_UNDEFINED},
  {TOP_vphadduwq,     TOP_vphadduwqx,       TOP_vphadduwqxx,      TOP_vphadduwqxxx,         TOP_UNDEFINED},
  {TOP_vphaddwd,      TOP_vphaddwdx,        TOP_vphaddwdxx,       TOP_vphaddwdxxx,          TOP_UNDEFINED},
  {TOP_vphaddwq,      TOP_vphaddwqx,        TOP_vphaddwqxx,       TOP_vphaddwqxxx,          TOP_UNDEFINED},
  {TOP_vphsubbw,      TOP_vphsubbwx,        TOP_vphsubbwxx,       TOP_vphsubbwxxx,          TOP_UNDEFINED},
  {TOP_vphsubdq,      TOP_vphsubdqx,        TOP_vphsubdqxx,       TOP_vphsubdqxxx,          TOP_UNDEFINED},
  {TOP_vphsubwd,      TOP_vphsubwdx,        TOP_vphsubwdxx,       TOP_vphsubwdxxx,          TOP_UNDEFINED},
  {TOP_vpmacsdd,      TOP_vpmacsddx,        TOP_vpmacsddxx,       TOP_vpmacsddxxx,          TOP_UNDEFINED},
  {TOP_vpmacsdqh,     TOP_vpmacsdqhx,       TOP_vpmacsdqhxx,      TOP_vpmacsdqhxxx,         TOP_UNDEFINED},
  {TOP_vpmacsdql,     TOP_vpmacsdqlx,       TOP_vpmacsdqlxx,      TOP_vpmacsdqlxxx,         TOP_UNDEFINED},
  {TOP_vpmacsdd,      TOP_vpmacsddx,        TOP_vpmacsddxx,       TOP_vpmacsddxxx,          TOP_UNDEFINED},
  {TOP_vpmacssdqh,    TOP_vpmacssdqhx,      TOP_vpmacssdqhxx,     TOP_vpmacssdqhxxx,        TOP_UNDEFINED},
  {TOP_vpmacssdql,    TOP_vpmacssdqlx,      TOP_vpmacssdqlxx,     TOP_vpmacssdqlxxx,        TOP_UNDEFINED},
  {TOP_vpmacsswd,     TOP_vpmacsswdx,       TOP_vpmacsswdxx,      TOP_vpmacsswdxxx,         TOP_UNDEFINED},
  {TOP_vpmacssww,     TOP_vpmacsswwx,       TOP_vpmacsswwxx,      TOP_vpmacsswwxxx,         TOP_UNDEFINED},
  {TOP_vpmacswd,      TOP_vpmacswdx,        TOP_vpmacswdxx,       TOP_vpmacswdxxx,          TOP_UNDEFINED},
  {TOP_vpmacsww,      TOP_vpmacswwx,        TOP_vpmacswwxx,       TOP_vpmacswwxxx,          TOP_UNDEFINED},
  {TOP_vpmadcsswd,    TOP_vpmadcsswdx,      TOP_vpmadcsswdxx,     TOP_vpmadcsswdxxx,        TOP_UNDEFINED},
  {TOP_vpmadcswd,     TOP_vpmadcswdx,       TOP_vpmadcswdxx,      TOP_vpmadcswdxxx,         TOP_UNDEFINED},
  {TOP_vpperm,        TOP_vppermx,          TOP_vppermxx,         TOP_vppermxxx,            TOP_UNDEFINED},
  {TOP_UNDEFINED,     TOP_vppermxr,         TOP_vppermxxr,        TOP_vppermxxxr,           TOP_UNDEFINED},
  {TOP_vprotbi,       TOP_vprotbix,         TOP_vprotbixx,        TOP_vprotbixxx,           TOP_UNDEFINED},
  {TOP_vprotb,        TOP_vprotbx,          TOP_vprotbxx,         TOP_vprotbxxx,            TOP_UNDEFINED},
  {TOP_UNDEFINED,     TOP_vprotbxr,         TOP_vprotbxxr,        TOP_vprotbxxxr,           TOP_UNDEFINED},
  {TOP_vprotdi,       TOP_vprotdix,         TOP_vprotdixx,        TOP_vprotdixxx,           TOP_UNDEFINED},
  {TOP_vprotd,        TOP_vprotdx,          TOP_vprotdxx,         TOP_vprotdxxx,            TOP_UNDEFINED},
  {TOP_UNDEFINED,     TOP_vprotdxr,         TOP_vprotdxxr,        TOP_vprotdxxxr,           TOP_UNDEFINED},
  {TOP_vprotqi,       TOP_vprotqix,         TOP_vprotqixx,        TOP_vprotqixxx,           TOP_UNDEFINED},
  {TOP_vprotq,        TOP_vprotqx,          TOP_vprotqxx,         TOP_vprotqxxx,            TOP_UNDEFINED},
  {TOP_UNDEFINED,     TOP_vprotqxr,         TOP_vprotqxxr,        TOP_vprotqxxxr,           TOP_UNDEFINED},
  {TOP_vprotwi,       TOP_vprotwix,         TOP_vprotwixx,        TOP_vprotwixxx,           TOP_UNDEFINED},
  {TOP_vprotw,        TOP_vprotwx,          TOP_vprotwxx,         TOP_vprotwxxx,            TOP_UNDEFINED},
  {TOP_UNDEFINED,     TOP_vprotwxr,         TOP_vprotwxxr,        TOP_vprotwxxxr,           TOP_UNDEFINED},
  {TOP_vpshab,        TOP_vpshabx,          TOP_vpshabxx,         TOP_vpshabxxx,            TOP_UNDEFINED},
  {TOP_UNDEFINED,     TOP_vpshabxr,         TOP_vpshabxxr,        TOP_vpshabxxxr,           TOP_UNDEFINED},
  {TOP_vpshad,        TOP_vpshadx,          TOP_vpshadxx,         TOP_vpshadxxx,            TOP_UNDEFINED},
  {TOP_UNDEFINED,     TOP_vpshadxr,         TOP_vpshadxxr,        TOP_vpshadxxxr,           TOP_UNDEFINED},
  {TOP_vpshaq,        TOP_vpshaqx,          TOP_vpshaqxx,         TOP_vpshaqxxx,            TOP_UNDEFINED},
  {TOP_UNDEFINED,     TOP_vpshaqxr,         TOP_vpshaqxxr,        TOP_vpshaqxxxr,           TOP_UNDEFINED},
  {TOP_vpshaw,        TOP_vpshawx,          TOP_vpshawxx,         TOP_vpshawxxx,            TOP_UNDEFINED},
  {TOP_UNDEFINED,     TOP_vpshawxr,         TOP_vpshawxxr,        TOP_vpshawxxxr,           TOP_UNDEFINED},
  {TOP_vpshlb,        TOP_vpshlbx,          TOP_vpshlbxx,         TOP_vpshlbxxx,            TOP_UNDEFINED},
  {TOP_UNDEFINED,     TOP_vpshlbxr,         TOP_vpshlbxxr,        TOP_vpshlbxxxr,           TOP_UNDEFINED},
  {TOP_vpshld,        TOP_vpshldx,          TOP_vpshldxx,         TOP_vpshldxxx,            TOP_UNDEFINED},
  {TOP_UNDEFINED,     TOP_vpshldxr,         TOP_vpshldxxr,        TOP_vpshldxxxr,           TOP_UNDEFINED},
  {TOP_vpshlq,        TOP_vpshlqx,          TOP_vpshlqxx,         TOP_vpshlqxxx,            TOP_UNDEFINED},
  {TOP_UNDEFINED,     TOP_vpshlqxr,         TOP_vpshlqxxr,        TOP_vpshlqxxxr,           TOP_UNDEFINED},
  {TOP_vpshlw,        TOP_vpshlwx,          TOP_vpshlwxx,         TOP_vpshlwxxx,            TOP_UNDEFINED},
  {TOP_UNDEFINED,     TOP_vpshlwxr,         TOP_vpshlwxxr,        TOP_vpshlwxxxr,           TOP_UNDEFINED},
};

// Associate an address mode group to an opcode.
static void
Add_Addr_Mode_Group (TOP top, Addr_Mode_Group *address_mode_group)
{
  if (top == TOP_UNDEFINED)
    return;

  // Don't redefine the address group for TOP if it is already defined.  This
  // is so that if TOP appears in multiple groups, we use only the first group,
  // which is assumed to be the most authoritative for TOP.
  if (Top_To_Addr_Mode_Group[top] == NULL)
    Top_To_Addr_Mode_Group[top] = address_mode_group;
}

// Build the Top_To_Addr_Modes table.
static void
Init_Addr_Modes()
{
  int i;
  ADDR_MODE mode;
  static bool table_is_initialized = false;

  if( table_is_initialized )
    return;

  table_is_initialized = true;

  for (i = 0; i < TOP_UNDEFINED; i++) {
    Top_To_Addr_Mode_Group[i] = NULL;
  }

  // Make sure the code below is in sync with the number of address modes.
  // Currently the last mode is N32_Mode.
  ADDR_MODE last_mode = N32_MODE;
  ADDR_MODE undefined_mode = UNDEFINED_MODE;
  FmtAssert(1 + (int)last_mode == (int) undefined_mode,
	    ("Init_Addr_Modes: some address modes not handled"));
  
  UINT table_size = sizeof(Addr_Mode_Group_Table) / sizeof(Addr_Mode_Group);
  for (i = 0; i < table_size; i++) {
    Addr_Mode_Group *group = &Addr_Mode_Group_Table[i];

    Add_Addr_Mode_Group(group->reg_mode, group);
    Add_Addr_Mode_Group(group->base_mode, group);
    Add_Addr_Mode_Group(group->base_index_mode, group);
    Add_Addr_Mode_Group(group->index_mode, group);
    Add_Addr_Mode_Group(group->n32_mode, group);
  }
}

static TOP
Get_Top_For_Addr_Mode (TOP top, ADDR_MODE mode)
{
  Addr_Mode_Group *group = Top_To_Addr_Mode_Group[top];
  if (group != NULL) {
    switch (mode) {
      case BASE_MODE:		return group->base_mode;
      case BASE_INDEX_MODE:	return group->base_index_mode;
      case INDEX_MODE:		return group->index_mode;
      case N32_MODE:		return group->n32_mode;
    }
    FmtAssert(FALSE, ("Get_Top_For_Addr_Mode: address mode not handled"));
  }
  return TOP_UNDEFINED;
}

bool 
Test_if_base_mode (OP* op) {
  //function that banks on the number of operands to figure
  // out if the BASE MODE is being used
  ADDR_MODE mode = BASE_MODE;
  const TOP new_top = Get_Top_For_Addr_Mode(OP_code(op), mode);
  if (new_top == OP_code(op)) {
      if (OP_store(op) || OP_load(op) || OP_prefetch(op))
          return true;

      if (OP_load_exe(op)) {
          if (OP_icmp(op)) { 
              // Some integer compares are not easy to convert to SIB
              return false;
          } 
          if (OP_opnds(op) > 3) {
              return false;
          } else if (OP_opnds(op) == 2 || OP_opnds(op) == 3) {
              if (OP_results(op) == 1) {
                  return true;
              } else {
                  return false;
              }
          } else {
              return false;
          }
      } else {
          return false;
      }
  } else {
      return false;
  }
}

OP *
Compose_Mem_Base_Index_Mode ( OP* op, TN* index, TN* offset, TN* scale, TN* base )
{
    // Based on existing function
  Is_True( offset != NULL, ("Compose_Mem_Base_Index_Mode: offset is NULL") );
  Is_True( index != NULL, ("Compose_Mem_Base_Index_Mode: index is NULL") );
  Is_True( scale != NULL, ("Compose_Mem_Base_Index_Mode: scale is NULL") );
  Is_True( base != NULL, ("Compose_Mem_Base_Index_Mode: base is NULL") );
  OP* new_op = NULL;
  ADDR_MODE mode = BASE_INDEX_MODE;
  const TOP new_top = Get_Top_For_Addr_Mode(OP_code(op), mode);
  const TOP old_top = OP_code(op);
  FmtAssert( new_top != TOP_UNDEFINED, ("Compose_Mem_Op: unknown top") );
  if( TOP_is_prefetch( new_top ) ){
      new_op = Mk_OP( new_top, OP_opnd( op, 0 ), base, offset, index, scale );
  } else {
    TN* storeval = NULL;

    if( TOP_is_store(new_top) ){
      storeval = OP_opnd( op, OP_find_opnd_use( op, OU_storeval ) );
    } else {
      storeval = OP_result( op, 0 );
    }
    if (OP_load(op) || OP_store(op) || OP_prefetch(op)) {
        if (Is_Target_Orochi() && Is_Target_AVX() && OP_load(op) &&
            (OP_vec_lo_ldst(op) || OP_vec_hi_ldst(op))) {
            new_op = Mk_OP( new_top, storeval, OP_opnd( op, 0 ), 
                            base, index, scale, offset );
        } else {
            new_op = Mk_OP( new_top, storeval, base, offset, index, scale );
        }
    } else if (OP_load_exe(op)) {
        if (OP_opnds(op) == 2) {
            FmtAssert ((storeval != NULL), 
                    ("Unsupported storeval with operands == 2 || == 1"));
            new_op = Mk_OP( new_top, storeval, base, index, scale, offset );
            return new_op;
        } else if (OP_opnds(op) == 3) {
            FmtAssert ((storeval != NULL), 
                    ("Unsupported storeval with operands >2"));
            new_op = Mk_OP( new_top, storeval, 
                        OP_opnd(op,0), base, index, scale, offset );
            return new_op;
        }
        FmtAssert(0,("Unsupported for Compose_Mem_Base_Index_Mode\n"));
    } else {
        FmtAssert(0,("Unsupported for Compose_Mem_Base_Index_Mode\n"));
    }
  }
  Copy_WN_For_Memory_OP(new_op, op);
  if (OP_volatile(op)) // Bug 4245 - copy "volatile" flag
    Set_OP_volatile(new_op);
  OP_srcpos(new_op) = OP_srcpos(op);

  Set_OP_unrolling(new_op, OP_unrolling(op));
  Set_OP_orig_idx(new_op, OP_map_idx(op));
  Set_OP_unroll_bb(new_op, OP_unroll_bb(op));

  return new_op;
}

//
// interior pointer translation
//
// This implementation utilizes discovery of similar address expressions
// on a collection of variables to answer the question of can these 
// expressions share index resources in SIB form, reducing the number
// of unique address components needed to provide array accesses.
//

// Do we have more base registers in flight than we have physregs or
// do we have local register pressure on gpr regs.
static bool have_basereg_pressure(std::set<TN*>& counted_base_regs,
                                  BB *lhead) {
  bool have_gpr_reg_pressure = false;
  INT num_br = counted_base_regs.size();
  INT num_pr = REGISTER_CLASS_register_count(ISA_REGISTER_CLASS_integer) - 3;

  // Two contraints, if we actually found register pressure during
  // scheduling or if we have more base regs than we have physicial gpr regs
  // to grant.  We are removing 1 reg for the RSP.
  if (BB_regpressure(lhead,ISA_REGISTER_CLASS_integer) ||
      (num_pr < num_br)) {
    have_gpr_reg_pressure = true;
  }

  if ((CG_PU_Has_Feedback) && (have_gpr_reg_pressure)) {
    // Find the loop epilog, the loop has 2 successors, the
    // loop head and the loop epilog.
    BB *epilog = NULL;
    BBLIST *lst;
    for ( lst = BB_succs(lhead); lst != NULL;
        lst = BBLIST_next(lst) ) {
      BB *bb = BBLIST_item(lst);
      if (bb != lhead) {
        epilog = bb;
        break;
      }
    }

    // If we are fdo optimized, and this loop is not executed much more
    // frequently than the loop epilog, do nothing
    if (BB_freq_fb_based(lhead) && BB_freq_fb_based(epilog)) {
      if (BB_freq(lhead) < (BB_freq(epilog) * 20.0))
        have_gpr_reg_pressure = false;
    }
  }

  return have_gpr_reg_pressure;
}

// find last def of tn in a given block
static OP* find_def_in_bb(BB *bb, TN *tn) {
  OP *def_op = NULL;
  OP *op;
  FOR_ALL_BB_OPs_REV(bb, op){
    for (INT i = 0; i < OP_results(op); ++i) {
      TN *res_tn = OP_result (op,i);
      if (TN_is_register(res_tn)) {
        if (TNs_Are_Equivalent(res_tn, tn)) {
          REGISTER reg1 = TN_register(res_tn);
          REGISTER reg2 = TN_register(tn);
          if (reg1 == reg2) {
            def_op = op;
            break;
          }
        }
      }
    }
    if (def_op != NULL) break;
  }

  return def_op;
}

static bool tn_has_no_def(TN *tn, TN_MAP def_map) {
  OP *def_op = (OP*)TN_MAP_Get(def_map, tn);
  return (def_op == NULL);
}

// find a given op in the visited queue
static bool op_find(std::deque<OP*>& ops_visited, OP *op)
{
  bool found = false;
  std::deque<OP*>::iterator visited_ops_it;
  for (visited_ops_it = ops_visited.begin();
       visited_ops_it != ops_visited.end();
       ++visited_ops_it) {
    OP* cur_op = *visited_ops_it;
    if (cur_op == op) {
      found = true;
      break;
    }
  }
  return found;
}

static bool tn_find(std::deque<TN*>& tn_queue, TN *tn)
{
  bool found = false;
  std::deque<TN*>::iterator tn_queue_it;
  for (tn_queue_it = tn_queue.begin();
       tn_queue_it != tn_queue.end();
       ++tn_queue_it) {
    TN* cur_tn = *tn_queue_it;
    if (cur_tn == tn) {
      found = true;
      break;
    }
  }
  return found;
}

// compare two def trees and mark attributes concerning input tn's tree
static void compare_def_tree(TN *tn,
                             INT *num,
                             bool count_marked,
                             INT  *total_marked,
                             std::deque<OP*>& diff_ops,
                             std::map<BB*,std::deque<OP*> >& bb_ops_visited_map,
                             std::map<BB*,std::deque<OP*> >& bb_ops_compare_map,
                             TN_MAP def_map) {
  INT i;
  bool skip_cover = false;

  if (tn_has_no_def(tn, def_map) == false) {
    std::deque<OP*> ops_visited;
    OP *def_op = (OP*)TN_MAP_Get(def_map, tn);
    *num = *num + 1;

    // mark the current def_op if we have not done so
    ops_visited = bb_ops_visited_map[def_op->bb];
    if (op_find(ops_visited, def_op) == false) {
      if (count_marked) {
        // ignore covering register transfers
        if ((*num == 1) &&
            ((OP_code(def_op) == TOP_mov32) ||
             (OP_code(def_op) == TOP_mov64))) {
          skip_cover = true;
        } else {
          *total_marked = *total_marked + 1;
          diff_ops.push_front(def_op);
        }
      }
    }

    if (EBO_Trace_Optimization && skip_cover) {
      fprintf( TFile, "covering register mov skipped for this compare tree\n");
    }

    // mark the current def_op if we have not done so
    std::deque<OP*> ops_compared;
    ops_compared = bb_ops_compare_map[def_op->bb];
    if (op_find(ops_compared, def_op) == false) {
      ops_compared.push_front(def_op);
      bb_ops_compare_map[def_op->bb] = ops_compared;

      for (i = 0; i < OP_opnds(def_op); i++) {
        TN *opnd_tn = OP_opnd(def_op,i);
        if (TN_is_register(opnd_tn) && !TN_is_dedicated(opnd_tn)) {
          compare_def_tree(opnd_tn, num, count_marked, total_marked,
                           diff_ops, bb_ops_visited_map, 
                           bb_ops_compare_map, def_map);
        }
      }
    }
  }
}

// mark the visited components of tn's tree to minimize compares
static void mark_def_tree(TN *tn,
                          INT *num,
                          bool count_marked,
                          INT  *total_marked,
                          std::map<BB*,std::deque<OP*> >& bb_ops_visited_map,
                          TN_MAP def_map) {
  INT i;

  if (tn_has_no_def(tn, def_map) == false) {
    std::deque<OP*> ops_visited;
    OP *def_op = (OP*)TN_MAP_Get(def_map, tn);
    *num = *num + 1;

    // mark the current def_op if we have not done so
    ops_visited = bb_ops_visited_map[def_op->bb];
    if (op_find(ops_visited, def_op) == false) {
      if (count_marked)
        *total_marked = *total_marked + 1;
      ops_visited.push_front(def_op);
      bb_ops_visited_map[def_op->bb] = ops_visited;

      for (i = 0; i < OP_opnds(def_op); i++) {
        TN *opnd_tn = OP_opnd(def_op,i);
        if (TN_is_register(opnd_tn) && !TN_is_dedicated(opnd_tn)) {
          mark_def_tree(opnd_tn, num, count_marked, total_marked,
                        bb_ops_visited_map, def_map);
        }
      }
    }
  }
}

// find the given load_op if it exists in the current queue
static bool is_load_in_queue(OP* load_op,
                             std::deque<OP*>& load_ops) {
  bool load_in_queue = false;
  std::deque<OP*>::iterator load_ops_it;
  for (load_ops_it = load_ops.begin();
       load_ops_it != load_ops.end();
       ++load_ops_it) {
    OP* op = *load_ops_it;
    if (load_op == op) {
      load_in_queue = true;
      break;
    }
  }
  return load_in_queue;
}

// determine if this load expression contains only our target load
static bool single_load_expression(std::deque<OP*>& diff_ops,
                                   std::deque<OP*>& load_ops) {
  // a walk from a load through our load based expression will
  // be a sequence in this def pattern
  OP *last_op = NULL;
  INT i;
  bool expr_ok = true;
  bool no_more_load_expr = false;
  std::deque<OP*>::iterator diff_ops_it;
  for (diff_ops_it = diff_ops.begin();
       diff_ops_it != diff_ops.end();
       ++diff_ops_it) {
    OP* diff_op = *diff_ops_it;

    // Check to see if we have a 2nd load, it will not be in load_ops
    if (OP_load(diff_op) && !is_load_in_queue(diff_op, load_ops)) {
      expr_ok = false;
      break;
    }

    if (no_more_load_expr) {
      if (OP_load_exe(diff_op)) {
        expr_ok = false;
        break;
      }
    }

    // TODO: are there other patterns?
    if ((last_op != NULL) &&
        (TOP_is_move_ext(OP_code(diff_op)) || TOP_is_move(OP_code(diff_op)))) {
      // any move must include that last diff_op
      bool found_reg = false;
      for (i = 0; i < OP_opnds(diff_op); i++) {
        TN *opnd_tn = OP_opnd(diff_op,i);
        if (TN_is_register(opnd_tn)) {
          TN *res_tn = OP_result (last_op,0);
          REGISTER reg1 = TN_register(res_tn);
          REGISTER reg2 = TN_register(opnd_tn);
          if (reg1 == reg2) {
            found_reg = true;
            last_op = diff_op;
            break;
          }
        }
      }
      expr_ok = found_reg;
    } else if (last_op) {
      // this is the tail insn, it must include the def of last_op
      bool found_reg = false;
      for (i = 0; i < OP_opnds(diff_op); i++) {
        TN *opnd_tn = OP_opnd(diff_op,i);
        if (TN_is_register(opnd_tn)) {
          TN *res_tn = OP_result (last_op,0);
          REGISTER reg1 = TN_register(res_tn);
          REGISTER reg2 = TN_register(opnd_tn);
          if (reg1 == reg2) {
            found_reg = true;
            break;
          }
        }
      }
      expr_ok = found_reg;
      no_more_load_expr = true;
    }

    if (OP_load(diff_op))
      last_op = diff_op;
  }

  return expr_ok;
}

// Compare two marked trees to see if they are initially similar.
static bool bidirection_diff(TN *tn1,
                             TN *tn2,
                             std::deque<OP*>& diff_ops_tn1_mark,
                             std::deque<OP*>& diff_ops_tn2_mark,
                             TN_MAP def_map) {
  INT total_marked[2];
  INT defs_seen[2];
  INT similar_threshold;
  INT i;
  bool is_consistant = true;

  for (i = 0; i < 2; i++) {
    total_marked[i] = 0;
    defs_seen[i] = 0;
  }

  // first mark the tn1 tree, then compare tn2 to tn1
  {
    std::map<BB*, std::deque<OP*> > bb_ops_visited_map;
    std::map<BB*, std::deque<OP*> > bb_ops_compare_map;
    mark_def_tree(tn1, &defs_seen[0], true, &total_marked[0],
                  bb_ops_visited_map, def_map);
    similar_threshold = (total_marked[0] / 4) + 2;
    compare_def_tree(tn2, &defs_seen[1], true, &total_marked[1],
                     diff_ops_tn1_mark, bb_ops_visited_map, 
                     bb_ops_compare_map, def_map);
  }
  // rule of thumb, slightly less than 75% is similar
  if (similar_threshold < total_marked[1])
    is_consistant = false;

  // Hueristic: limit the def chain size
  if ((defs_seen[0] > 100) || (defs_seen[1] > 100))
    is_consistant = false;

  for (i = 0; i < 2; i++) {
    total_marked[i] = 0;
    defs_seen[i] = 0;
  }

  // first mark the tn2 tree, then compare tn1 to tn2
  {
    std::map<BB*, std::deque<OP*> > bb_ops_visited_map;
    std::map<BB*, std::deque<OP*> > bb_ops_compare_map;
    mark_def_tree(tn2, &defs_seen[0], true, &total_marked[0],
                  bb_ops_visited_map, def_map);
    similar_threshold = (total_marked[0] / 4) + 2;
    compare_def_tree(tn1, &defs_seen[1], true, &total_marked[1],
                     diff_ops_tn2_mark, bb_ops_visited_map, 
                     bb_ops_compare_map, def_map);
  }
  // rule of thumb, slightly less than 75% is similar
  if (similar_threshold < total_marked[1])
    is_consistant = false;

  // Hueristic: limit the def chain size
  if ((defs_seen[0] > 100) || (defs_seen[1] > 100))
    is_consistant = false;

  return is_consistant;
}

// find the sub expression load and add it to load_ops
static bool diff_ops_find_load(std::deque<OP*>& diff_ops,
                               std::deque<OP*>& load_ops) {
  OP *diff_op = NULL;
  bool has_load = false;
  std::deque<OP*>::iterator diff_ops_it;
  for (diff_ops_it = diff_ops.begin();
       diff_ops_it != diff_ops.end();
       ++diff_ops_it) {
    diff_op = *diff_ops_it;
    if (OP_load(diff_op)) {
      load_ops.push_front(diff_op);
      has_load = true;
      break;
    }
  }
  return has_load;
}

// do our sub expressions match
static bool diff_ops_same(std::deque<OP*>& diff1_ops,
                          std::deque<OP*>& diff2_ops) {
  OP *op1 = NULL;
  OP *op2 = NULL;
  bool ops_same = true;
  std::deque<OP*>::iterator diff_ops1_it;
  std::deque<OP*>::iterator diff_ops2_it;
  diff_ops1_it = diff1_ops.begin();
  diff_ops2_it = diff2_ops.begin();
  while (diff_ops1_it != diff1_ops.end() &&
         diff_ops2_it != diff2_ops.end()) {
    op1 = *diff_ops1_it;
    op2 = *diff_ops2_it;
    if (OP_code(op1) != OP_code(op2)) {
      ops_same = false;
      break;
    }
    ++diff_ops1_it;
    ++diff_ops2_it;
  }
  return ops_same;
}

// Process the correlated ST queues and compare the address expressions.
// Place the matched queues in the symbol map for our address expressions
static void process_queues(std::set<ST*>& counted_addr_sts,
                           std::map<ST*,std::deque<TN*> >& symbol_addr_map,
                           TN_MAP def_map) {
  std::map<BB*, std::deque<OP*> > bb_ops_visited_map;
  std::set<ST*>::const_iterator counted_addr_sts_it;
  INT total_marked[2];
  INT defs_seen[2];
  INT similar_threshold;
  TN *tn1;
  TN *tn2;

  // Walk ST based unique tn queues.
  //
  // The resultant output are changes in symbol_addr_map on a
  // symbol by symbol basis, where only paired tn lists persist.
  //
  // We have built all the def_maps we can see for this loop.
  // Next we look at the candidates where number of TNs mapped to a ST
  // meets the following criteria:
  //
  // 2: automatic pair, determine address ordering
  // > 4: pair if possible closely matching accesses, else prune

  for (counted_addr_sts_it = counted_addr_sts.begin();
       counted_addr_sts_it != counted_addr_sts.end();
       ++counted_addr_sts_it) {
    INT tn_count;
    std::deque<TN*> st_tns;
    std::deque<TN*> matched_pairs;

    st_tns = symbol_addr_map[*counted_addr_sts_it];
    tn_count = st_tns.size();

    if (tn_count == 0) continue;

    if (tn_count == 2) {
      INT i;
      std::deque<OP*> diff_ops_tn1_mark;
      std::deque<OP*> diff_ops_tn2_mark;
      std::deque<TN*>::iterator st_tns_iter;
      bool is_consistant = true;

      st_tns_iter = st_tns.begin();
      for (i = 0; i < 2; i++, ++st_tns_iter) {
        switch (i) {
        case 0: tn1 = *st_tns_iter; break;
        case 1: tn2 = *st_tns_iter; break;
        }
      }

      // configure initial consistancy and diff queues
      is_consistant = bidirection_diff(tn1, tn2, diff_ops_tn1_mark,
                                       diff_ops_tn2_mark, def_map);

      // We are looking at things like J and J+1 for array access patterns,
      // if the diff_ops lists are of different size.  We then take
      // note of how they differ, as they should be highly similar to compare.
      OP *diff_op1 = NULL;
      OP *diff_op2 = NULL;
      bool is_tn1_major = false;
      bool is_tn2_major = false;
      std::deque<OP*>::reverse_iterator diff_ops_it1;
      std::deque<OP*>::reverse_iterator diff_ops_it2;
      INT size_of_diff_ops1 = diff_ops_tn1_mark.size();
      INT size_of_diff_ops2 = diff_ops_tn2_mark.size();

      // Only allow a single difference in size of the diff ops
      if (is_consistant) {
        if (size_of_diff_ops1 > size_of_diff_ops2) {
          if ((size_of_diff_ops1 - size_of_diff_ops2) != 1)
            is_consistant = false;
        } else if (size_of_diff_ops2 > size_of_diff_ops1) {
          if ((size_of_diff_ops2 - size_of_diff_ops1) != 1)
            is_consistant = false;
        } else {
          is_consistant = false;
        }
      }

      diff_ops_it1 = diff_ops_tn1_mark.rbegin();
      diff_ops_it2 = diff_ops_tn2_mark.rbegin();
      while(1 && is_consistant) {
        if (diff_ops_it1 == diff_ops_tn1_mark.rend())
          break;
        if (diff_ops_it2 == diff_ops_tn2_mark.rend())
          break;

        diff_op1 = *diff_ops_it1;
        diff_op2 = *diff_ops_it2;
        if (OP_code(diff_op1) != OP_code(diff_op2)) {
          is_consistant = false;
          break;
        }

        diff_ops_it1++;
        diff_ops_it2++;
      }

      if (is_consistant) {
        ST *st = NULL;
        // the interior most of the longer diff chain is the determining op
        if (diff_ops_it1 != diff_ops_tn1_mark.rend()) {
          diff_op1 = *diff_ops_it1;
          if (OP_iadd(diff_op1) || OP_isub(diff_op1) || OP_imul(diff_op1)) {
            if (OP_isub(diff_op1))
              is_tn2_major = true;
            else
              is_tn1_major = true;
          }
        } else if (diff_ops_it2 != diff_ops_tn2_mark.rend()) {
          diff_op2 = *diff_ops_it2;
          if (OP_iadd(diff_op2) || OP_isub(diff_op2) || OP_imul(diff_op2)) {
            if (OP_isub(diff_op2))
              is_tn1_major = true;
            else
              is_tn2_major = true;
          }
        }

        st = *counted_addr_sts_it;
        if (is_tn1_major) {
          matched_pairs.push_back(tn1);
          matched_pairs.push_back(tn2);
        } else if (is_tn2_major) {
          matched_pairs.push_back(tn2);
          matched_pairs.push_back(tn1);
        }
      }

      diff_ops_tn1_mark.clear();
      diff_ops_tn2_mark.clear();
      symbol_addr_map[*counted_addr_sts_it].clear();
      if (!matched_pairs.empty()) {
        // Now copy the ordered pairs of tns back
        symbol_addr_map[*counted_addr_sts_it] = matched_pairs;
      }
    } else if (tn_count == 3) {
      // Ordering the sort of the arity index values is non-trivial.
      symbol_addr_map[*counted_addr_sts_it].clear();
    } else {
      bool have_pair;
      std::deque<TN*>::iterator st_tns_iter1;
      std::deque<TN*>::iterator st_tns_iter2;
      std::deque<OP*> diff_ops_tn1_mark;
      std::deque<OP*> diff_ops_tn2_mark;
      std::deque<OP*> load_ops;
      bool first_time = true;
      bool load_scenario = false;
      bool lea_scenario = false;
      bool changed = true;
      OP *first_ld = NULL;
      OP *second_ld = NULL;

      while (changed) {
        changed = false;
        // In this case we are looking for base regs which
        // have addr calcs that differ by a single load based expression while
        // attempting to match pairs like v(i,j,k,m) and v(i,j,k,n)
        for (st_tns_iter1 = st_tns.begin();
             st_tns_iter1 != st_tns.end();
             ++st_tns_iter1) {

          tn1 = *st_tns_iter1;
          have_pair = false;

          // For this collection, the differing load
          // must be the same, this is why load_ops is used.
          for (st_tns_iter2 = st_tns.begin();
               st_tns_iter2 != st_tns.end();
               ++st_tns_iter2) {
            bool is_consistant;
            tn2 = *st_tns_iter2;
            if (tn1 == tn2) continue;

            // configure initial consistancy and diff queues
            is_consistant = bidirection_diff(tn1, tn2, diff_ops_tn1_mark,
                                             diff_ops_tn2_mark, def_map);

            // Check if we have but 1 load different in each diff and
            // that the loads are mutually exclusive, and that only those
            // 2 loads are shared as pairs thoughout the group.
            if (is_consistant &&
                (lea_scenario == false) &&
                diff_ops_find_load(diff_ops_tn1_mark, load_ops)) {
              if (single_load_expression(diff_ops_tn1_mark, load_ops) &&
                  !single_load_expression(diff_ops_tn2_mark, load_ops)) {
                if (first_time) {
                  first_ld = *load_ops.begin();
                }
                load_ops.clear();
                if (diff_ops_find_load(diff_ops_tn2_mark, load_ops)) {
                  if (single_load_expression(diff_ops_tn2_mark, load_ops) &&
                      !single_load_expression(diff_ops_tn1_mark, load_ops)) {
                    if (first_time) {
                      second_ld = *load_ops.begin();
                      first_time = false;
                      load_scenario = true;
                      have_pair = true;
                    } else if (is_load_in_queue(first_ld, diff_ops_tn1_mark) &&
                               is_load_in_queue(second_ld, diff_ops_tn2_mark)) {                      have_pair = true;
                    }
                    if (have_pair) {
                      // tn2 contains the target load, so its in front
                      matched_pairs.push_back(tn2);
                      matched_pairs.push_back(tn1);
                      st_tns_iter2 = st_tns.erase(st_tns_iter2);
                    }
                  }
                }
              }
              load_ops.clear();
            }

            // If we did not match the load_scenerio try the lea
            // scenario for close offset calcs
            if (load_scenario == false) {
              if (diff_ops_tn1_mark.size() == diff_ops_tn2_mark.size()) {
                if (diff_ops_same(diff_ops_tn1_mark, diff_ops_tn2_mark)) {
                  OP *diff_op1 = *diff_ops_tn1_mark.begin();
                  OP *diff_op2 = *diff_ops_tn2_mark.begin();
                  // only allowing delta of 1 in addr calc for
                  // lea based offset, the lea is the inner most part of the
                  // def queue(front)
                  if (((OP_code(diff_op1) == TOP_lea32) &&
                       (OP_code(diff_op2) == TOP_lea32)) ||
                      ((OP_code(diff_op1) == TOP_lea64) &&
                       (OP_code(diff_op2) == TOP_lea64))) {
                    int diff_off_loc1 = OP_find_opnd_use( diff_op1, OU_offset );                    int diff_off_loc2 = OP_find_opnd_use( diff_op2, OU_offset );                    if ((diff_off_loc1 != -1) && (diff_off_loc2 != -1)) {
                      TN *offset_tn2 = OP_opnd( diff_op1, diff_off_loc1 );
                      TN *offset_tn1 = OP_opnd( diff_op2, diff_off_loc2 );
                      // Remember the diff ops contains the compared pair
                      // tn addr expression, i.e. diff_op1 contains part of
                      // tn2's address expression.
                      INT64 val2 = TN_value(offset_tn2);
                      INT64 val1 = TN_value(offset_tn1);
                      // order the pair by larger value
                      if (val2 > val1) {
                        if ((val2 - val1) == 1)
                          have_pair = true;
                      }
                      if (have_pair) {
                        matched_pairs.push_back(tn2);
                        matched_pairs.push_back(tn1);
                        st_tns_iter2 = st_tns.erase(st_tns_iter2);
                        lea_scenario = true;
                      }
                    }
                  }
                }
              }
            }
            diff_ops_tn1_mark.clear();
            diff_ops_tn2_mark.clear();
            if (have_pair)
              break;
          }
          if (have_pair) {
            st_tns_iter1 = st_tns.erase(st_tns_iter1);
            changed = true;
            break;
          }
        }
      }
      // Clear the current work list, its has changed context
      symbol_addr_map[*counted_addr_sts_it].clear();
      if (!matched_pairs.empty() &&
          (tn_count == matched_pairs.size())) {
        symbol_addr_map[*counted_addr_sts_it] = matched_pairs;
      }
    }
    // Do some cleanup
    matched_pairs.clear();
  }
}

// dump a given def tree
static void dump_def_tree(TN *tn,
                          TN_MAP def_map,
                          int indent_idx)
{
  INT i;
  if (tn_has_no_def(tn, def_map) == false) {
    std::deque<OP*> def_ops;
    OP *def_op = (OP*)TN_MAP_Get(def_map, tn);
    for (i = 0; i < indent_idx; i++)
      printf(" ");

    Print_OP_No_SrcLine(def_op);
    for (i = 0; i < OP_opnds(def_op); i++) {
      TN *opnd_tn = OP_opnd(def_op,i);
      if (TN_is_register(opnd_tn) && !TN_is_dedicated(tn))
        dump_def_tree(opnd_tn, def_map, indent_idx+2);
    }
  }
}

// build a def tree for each base register
static bool build_base_def_trees (TN *tn,
                                  BB *bb_defreach,
                                  TN_MAP def_map,
                                  BS *def_set) {
  INT i;
  bool ret_val = true;
  BB *def_bb = NULL;
  OP *def_op = NULL;
  std::map<INT, BB*> block_map;
  INT32 bb_dfo_id, last_def_bb_dfo_id, bb_defreach_dfo_id;

  bb_defreach_dfo_id = BB_MAP32_Get(dfo_map, bb_defreach);

  // First find all the defs and put them into the bb def_set,
  // then find the nearest def, then add the def op to the def_map.
  if (TN_is_register(tn) && TN_is_global_reg(tn)) {
    BB *bb1, *bb2;
    for (bb1 = REGION_First_BB; bb1 != NULL; bb1 = BB_next(bb1)) {
      block_map[bb1->id] = bb1;
      if (GTN_SET_MemberP(BB_live_def(bb1), tn))
        BS_Union1D(def_set, bb1->id, NULL);
    }
    if (!BS_EmptyP(def_set)) {
      BS_ELT id1, id2, last_id;
      last_def_bb_dfo_id = 0;
      last_id = BS_Choose(def_set);
      for (id1 = BS_Choose(def_set); id1 != BS_CHOOSE_FAILURE;
           id1 = BS_Choose_Next(def_set,id1)) {
        for (id2 = BS_Choose(def_set); id2 != BS_CHOOSE_FAILURE;
             id2 = BS_Choose_Next(def_set,id2)) {
          if (id1 == id2) continue;
          bb1 = block_map[id1];
          bb_dfo_id = BB_MAP32_Get(dfo_map, bb1);
          if ((bb_dfo_id > last_def_bb_dfo_id) &&
              (bb_dfo_id < bb_defreach_dfo_id)) {
            // remove exterior def blocks
            BS_Difference1D(def_set, id1);
            last_def_bb_dfo_id = bb_dfo_id;
            last_id = id1;
          }
        }
      }

      def_bb = block_map[last_id];
      def_op = find_def_in_bb(def_bb, tn);
    }
  } else if (TN_is_register(tn)) {
    def_bb = bb_defreach;
    def_op = find_def_in_bb(def_bb, tn);
  }

  if (!BS_EmptyP(def_set))
    BS_ClearD(def_set);

  // if we have a def, fill in the def tree leaf node
  if (def_bb && def_op) {
    if (OP_call(def_op) || OP_load(def_op) || TN_is_dedicated(tn)) {
      if ((OP*)TN_MAP_Get(def_map, tn) == NULL)
        TN_MAP_Set(def_map, tn, def_op);
      ret_val = true;
    } else {
      if ((OP*)TN_MAP_Get(def_map, tn) == NULL)
        TN_MAP_Set(def_map, tn, def_op);
      for (i = 0; i < OP_opnds(def_op); i++) {
        TN *opnd_tn = OP_opnd(def_op,i);
        if (TN_is_register(opnd_tn))
          ret_val = build_base_def_trees(opnd_tn, def_bb, def_map, def_set);
      }
    }
  } else {
    if (TN_is_dedicated(tn)) {
      // rip regs have no def
      if (TN_register_class(tn) != ISA_REGISTER_CLASS_rip)
        ret_val = false;
    } else {
      ret_val = false;
    }
  }

  return ret_val;
}

static ST *get_addr_symbol(OP *op)
{
  ST *st = NULL;
  WN *mem_wn = (WN*) OP_MAP_Get(OP_to_WN_map, op);
  if (mem_wn) {
    POINTS_TO *pt = Points_to(Alias_Manager, mem_wn);
    if (pt->Expr_kind() == EXPR_IS_ADDR && pt->Base_is_fixed()) {
      st = pt->Base();
    } else if (pt->F_param() && pt->Based_sym() != NULL) {
      st = pt->Based_sym();
    } else if (pt->Unique_pt() && pt->Based_sym() != NULL) {
      st = pt->Based_sym();
    }
  }
  return st;
}

// Build the ST queues and compare them, then process for matches
static void build_addr_queues (
            std::set<ST*>& counted_addr_sts,
            std::map<ST*,std::deque<TN*> >& symbol_addr_map,
            std::map<TN*,std::deque<OP*> >& use_map,
            std::set<TN*>& counted_base_regs,
            BS* def_set) {
  TN_MAP def_map = TN_MAP_Create();
  std::deque<TN*> st_tns;
  INT total_ops_count = 0;

  // Hueristic: limit search size
  if (PU_BB_Count > 2000) return;

  // first build the symbol queues
  std::set<TN*>::const_iterator counted_base_regs_it;
  for (counted_base_regs_it = counted_base_regs.begin();
       counted_base_regs_it != counted_base_regs.end();
       ++counted_base_regs_it) {
    std::deque<OP*> used_ops;
    used_ops = use_map[(*counted_base_regs_it)];
    std::deque<OP*>::iterator used_ops_it;
        for (used_ops_it = used_ops.begin();
             used_ops_it != used_ops.end();
             ++used_ops_it) {
      OP* cur_op = *used_ops_it;

      if (OP_prefetch(cur_op)) continue;
      if (OP_unrolling(cur_op)) continue;

      // look at the orig code, excluding unroll copies and prefetch code
      ST *st = get_addr_symbol(cur_op);
      TN *tn = *counted_base_regs_it;

      if (st == NULL)
        continue;

      if (symbol_addr_map[st].empty()) {
        symbol_addr_map[st].push_front(tn);
        counted_addr_sts.insert(st);
      } else {
        bool found_tn = false;
        st_tns = symbol_addr_map[st];
        std::deque<TN*>::iterator st_tns_iter;
        for (st_tns_iter = st_tns.begin();
             st_tns_iter != st_tns.end();
             ++st_tns_iter) {
          if (*st_tns_iter == tn) {
            found_tn = true;
            break;
          }
        }
        if (found_tn == false) {
          symbol_addr_map[st].push_front(tn);
        }
      }
    }
  }

  // prune out the cases we cannot handle
  std::set<ST*>::const_iterator counted_addr_sts_it;
  for (counted_addr_sts_it = counted_addr_sts.begin();
       counted_addr_sts_it != counted_addr_sts.end();
       ++counted_addr_sts_it) {
    st_tns = symbol_addr_map[*counted_addr_sts_it];
    INT tn_count = st_tns.size();

    // currently only fielding 2 to 3 divergent base reg accesses
    switch (tn_count) {
    case 2:
    case 3:
      break;
    default:
      if ((tn_count & 0x1) == 0x1)
        symbol_addr_map[*counted_addr_sts_it].clear();
      else if (tn_count > 8)
        symbol_addr_map[*counted_addr_sts_it].clear();
    }
  }

  // now process them
  for (counted_addr_sts_it = counted_addr_sts.begin();
       counted_addr_sts_it != counted_addr_sts.end();
       ++counted_addr_sts_it) {
    st_tns = symbol_addr_map[*counted_addr_sts_it];
    std::deque<TN*>::iterator st_tns_iter;
    for (st_tns_iter = st_tns.begin();
         st_tns_iter != st_tns.end();
         ++st_tns_iter) {
      std::deque<OP*> used_ops;
      used_ops = use_map[(*st_tns_iter)];
      std::deque<OP*>::iterator used_ops_it;
      for (used_ops_it = used_ops.begin();
           used_ops_it != used_ops.end();
           ++used_ops_it) {
        OP* cur_op = *used_ops_it;

        if (OP_prefetch(cur_op)) continue;
        if (OP_unrolling(cur_op)) continue;

        TN *tn = *st_tns_iter;
        if (build_base_def_trees(tn, cur_op->bb, def_map, def_set)) {
          if (EBO_Trace_Optimization)
            dump_def_tree(tn, def_map, 0);
        }

        // stop on first successful map - we only need one
        break;
      }
    }
  }

  process_queues(counted_addr_sts, symbol_addr_map, def_map);
  TN_MAP_Delete(def_map);
}

static void prune_adds_from_live_range_analysis(
            std::deque<TN*>& add_tns,
            std::set<TN*>& counted_base_regs,
            std::map<TN*,OP*>& add_map)
{
  std::set<TN*>::const_iterator counted_base_regs_it;
  for (counted_base_regs_it = counted_base_regs.begin();
       counted_base_regs_it != counted_base_regs.end();
       ++counted_base_regs_it) {
    TN *tn = *counted_base_regs_it;
    if (tn_find(add_tns, tn) == false) {
      OP* add_op = add_map[tn];
      Truncate_LRs_For_OP(add_op);
    }
  }
}

// After building interior pointers candidates, remove
// all the effected counted_base_regs from SIB processing so
// that we do not translate them in SIB translation.
static bool remove_cands_from_sib_gen_and_correlate(
            std::deque<INT>& size_queue,
            std::set<ST*>& counted_addr_sts,
            std::map<INT,std::deque<ST*> >& correlated_addr_map,
            std::map<ST*,std::deque<TN*> >& symbol_addr_map,
            std::set<TN*>& counted_base_regs,
            std::map<TN*,OP*>& add_map,
            BB *bb,
            bool loop_vectorized,
            MEM_POOL *pool) {
  INT num_cands = 0;
  INT num_regs = 0;
  INT min_reclaimable = 3;
  INT num_ranges_mitigated = 0;
  INT num_pr = REGISTER_CLASS_register_count(ISA_REGISTER_CLASS_integer);
  bool clear_all = false;

  // Iterate on the st bases tn lists to determine if we
  // need to remove the interior pointer cands from the
  // counted_base_regs set which is used to drive SIB translation.
  // Also figure out how many registers we reduced the new code by.
  std::set<ST*>::const_iterator counted_addr_sts_it;
  for (counted_addr_sts_it = counted_addr_sts.begin();
       counted_addr_sts_it != counted_addr_sts.end();
       ++counted_addr_sts_it) {
    std::deque<TN*> st_tns;
    st_tns = symbol_addr_map[*counted_addr_sts_it];

    // Skip the non interior pointer cands
    if (st_tns.empty()) continue;

    // we will use this to iterate over the correlated_addr_map with
    if (correlated_addr_map[st_tns.size()].empty())
      size_queue.push_front(st_tns.size());

    correlated_addr_map[st_tns.size()].push_front(*counted_addr_sts_it);
  }

  // pick largest pattern
  INT max_pattern = 0;
  std::deque<INT>::const_iterator size_queue_iter;
  for (size_queue_iter = size_queue.begin();
       size_queue_iter != size_queue.end();
       ++size_queue_iter) {
    INT cor_addr_index = *size_queue_iter;
    INT cur_size = 0;
    INT new_size = 0;
    if (!correlated_addr_map[max_pattern].empty()) {
      std::deque<ST*> st_queue = correlated_addr_map[max_pattern];
      cur_size = st_queue.size() * max_pattern;
    }
    if (!correlated_addr_map[cor_addr_index].empty()) {
      std::deque<ST*> st_queue = correlated_addr_map[cor_addr_index];
      new_size = st_queue.size() * cor_addr_index;
    }
    if (new_size > cur_size)
      max_pattern = cor_addr_index;
  }

  if (max_pattern) {
    TN_MAP orig_conflict_map;
    TN_MAP new_conflict_map;
    mINT8 fatpoint[ISA_REGISTER_CLASS_MAX+1];
    const INT len = BB_length(bb);
    INT* regs_in_use = (INT *)alloca(sizeof(INT) * (len+1));
    std::deque<TN*> add_tns;

    MEM_POOL_Push(pool);

    // Calculate the current live ranges
    LRA_Estimate_Fat_Points(bb, fatpoint, regs_in_use, pool);
    // build a list of tns interior pointers will operate on
    for (counted_addr_sts_it = counted_addr_sts.begin();
         counted_addr_sts_it != counted_addr_sts.end();
       ++counted_addr_sts_it) {
      std::deque<TN*> st_tns;
      st_tns = symbol_addr_map[*counted_addr_sts_it];

      // Skip the non interior pointer cands
      if (st_tns.empty()) continue;

      INT cor_addr_index = st_tns.size();
      if (cor_addr_index != max_pattern) continue;

      if (!correlated_addr_map[cor_addr_index].empty()) {
        std::deque<TN*>::iterator st_tns_iter;
        for (st_tns_iter = st_tns.begin();
             st_tns_iter != st_tns.end();
             ++st_tns_iter) {
          TN *tn1 = *st_tns_iter;
          ++st_tns_iter;
          TN *tn2 = *st_tns_iter;
          add_tns.push_front(tn2);
        }
      }
    }

    // remove all the sib adds from our live range maps so that we get
    // an accurate picture for analysis.
    prune_adds_from_live_range_analysis(add_tns, counted_base_regs, add_map);
    add_tns.clear();

    // Do the initial live range analysis
    orig_conflict_map = Calculate_All_Conflicts(bb, regs_in_use, 
                                                ISA_REGISTER_CLASS_integer);

    // Merge all pairs live ranges on the minor basereg
    bool first_time = true;
    for (counted_addr_sts_it = counted_addr_sts.begin();
         counted_addr_sts_it != counted_addr_sts.end();
       ++counted_addr_sts_it) {
      std::deque<TN*> st_tns;
      st_tns = symbol_addr_map[*counted_addr_sts_it];

      // Skip the non interior pointer cands
      if (st_tns.empty()) continue;

      INT cor_addr_index = st_tns.size();
      if (cor_addr_index != max_pattern) continue;

      if (!correlated_addr_map[cor_addr_index].empty()) {
        switch (cor_addr_index) {
          case 2:
            num_regs++;
            num_cands++;
            break;
          case 3:
            break;
          default:
            num_regs += (cor_addr_index / 2);
            num_cands++;
            break;
        }

        std::deque<TN*>::iterator st_tns_iter;
        for (st_tns_iter = st_tns.begin();
             st_tns_iter != st_tns.end();
             ++st_tns_iter) {
          TN *tn1 = *st_tns_iter;
          ++st_tns_iter;
          TN *tn2 = *st_tns_iter;
          Merge_Live_Ranges(tn1, tn2, first_time);
          first_time = false;
        }
      }
    }

    // If Query_Conflicts_Improved returns with a state that indicates
    // that introduction of interior pointers does not benefit live
    // range pressure, we do not proceed with the allowing the translation.
    new_conflict_map = Calculate_All_Conflicts(bb, regs_in_use, 
                                               ISA_REGISTER_CLASS_integer);
    INT N_i = 0;
    INT D_i = 0;
    INT avg_conflicts = 0;
    INT k_conflicts = 0;
    if (Query_Conflicts_Improved(orig_conflict_map, 
                                 new_conflict_map,
                                 3,
                                 &num_ranges_mitigated,
                                 ISA_REGISTER_CLASS_integer) == false) {
      clear_all = true;
    } else if (Find_Max_Conflicts(orig_conflict_map,
                                  &avg_conflicts,
                                  &k_conflicts,
                                  &N_i,
                                  &D_i,
                                  ISA_REGISTER_CLASS_integer) == num_pr) {
      // For vectorized loops we want more than k-conflicts as max in the
      // original conflict map context.
      if (loop_vectorized)
        min_reclaimable = 4; 
    }
    TN_MAP_Delete(orig_conflict_map);
    TN_MAP_Delete(new_conflict_map);

    MEM_POOL_Pop(pool);
  }

  // If we found we can save some regs, remember we need one for the
  // distance calc/interior pointer index reg based on the ST which 
  // are included in max_pattern
  if (num_regs > 0)
    num_regs--;

  // Hueristic: We need a higher constraint for singleton pairs
  if (max_pattern == 2)
    if (num_regs < (2 * max_pattern))
      clear_all = true;

  // Hueristic: If we have vectorized code we account
  // for reduced loop trip by increasing the number of required
  // reclaimable regs for translation. Else we calculate
  // live range pressure and apply some more huerstics.
  if (num_regs < min_reclaimable) {
    clear_all = true;
  } else if (clear_all == false) {
    if (EBO_Trace_Optimization) {
      printf("reclaim regs=%d: num base regs = %d, idx = %d, improved = %d\n",
             num_regs, counted_base_regs.size(), 
             max_pattern, num_ranges_mitigated);
    }

    // Hueristic: Calculate the amount of work interior pointners will remove 
    // and the amount we leave by bypassing sib generation for 
    // our primary candidates.  If that work is zero or less we do nothing.
    INT reload_time = CGTARG_Latency(TOP_ldx64);
    INT work_added = (num_cands * max_pattern) - (num_regs + 1);
    INT work_removed = (reload_time * num_ranges_mitigated) + num_regs;
    INT net_work = (work_removed - work_added);
    if (net_work <= 0)
      clear_all = true;

    // Hueristic: Filter out less benefitial cases
    if ((num_regs == 3) && 
        (loop_vectorized) &&
        (counted_base_regs.size() < num_pr)) {
      if (num_ranges_mitigated < num_regs)
        clear_all = true;
    }

    if (EBO_Trace_Optimization) {
      if (clear_all == false)
        printf("net work(cands=%d) = %d\n", num_cands, net_work);
    }
  }

  // Now prune out all the validated candidates from SIB translation,
  // so that we can apply interior pointer translation to that select
  // set of base regsiters.
  num_cands = 0;
  for (counted_addr_sts_it = counted_addr_sts.begin();
       counted_addr_sts_it != counted_addr_sts.end();
       ++counted_addr_sts_it) {
    std::deque<TN*> st_tns;
    st_tns = symbol_addr_map[*counted_addr_sts_it];

    // Skip the non interior pointer cands
    if (st_tns.empty()) continue;

    INT cor_addr_index = st_tns.size();

    // If we failed the above hueristics we will clean up here.
    if (clear_all) {
      correlated_addr_map[cor_addr_index].clear();
      continue;
    }

    // Count only the validated pattern, clear the rest.
    if (cor_addr_index == max_pattern) {
      num_cands++;
    } else {
      correlated_addr_map[cor_addr_index].clear();
      continue;
    }

    // Only remove sib candidates which are interior pointer actionable
    if (!correlated_addr_map[cor_addr_index].empty() &&
        (cor_addr_index == max_pattern)) {
      std::deque<TN*>::iterator st_tns_iter;
      for (st_tns_iter = st_tns.begin();
           st_tns_iter != st_tns.end();
           ++st_tns_iter) {
        TN *tn = *st_tns_iter;
        counted_base_regs.erase(tn);
      }
    }
  }
  return (num_cands > 0);
}

// Translate base address expressions and adjust addresses as needed
static void interior_pointer_translation(
            BB *target_bb,
            TN *distance_tn,
            std::deque<TN*>& st_unified_tns,
            std::map<TN*,OP*>& add_map,
            std::map<TN*,std::deque<OP*> >& use_map) {
  INT i, j;
  for (i = 0; i < st_unified_tns.size(); i+=2) {
    TN *tn1 = st_unified_tns[i];
    TN *tn2 = st_unified_tns[i+1];

    // Now key off of tn2 to replace all entries in the use_map for tn1
    // utilizing the distance_tn in SIB form.
    std::deque<OP*> used_ops;
    used_ops = use_map[tn1];
    std::deque<OP*>::iterator used_ops_it;
    for (used_ops_it = used_ops.begin();
         used_ops_it != used_ops.end();
         ++used_ops_it) {
      OP* whatnow;
      // Note: If we ever add the 3 tuple version this will need to have
      //       two forms of scale factor, 1 and 2.
      TN* scale = ( Is_Target_32bit() ) ?
                    Gen_Literal_TN(1,4) :
                    Gen_Literal_TN(1,8);

      int offset_loc = OP_find_opnd_use( *used_ops_it, OU_offset );
      TN *offset_tn;
      if (offset_loc >= 0) {
        offset_tn = OP_opnd( *used_ops_it, offset_loc );
      } else {
        offset_tn =  ( Is_Target_32bit() ) ?
                       Gen_Literal_TN(0,4) :
                       Gen_Literal_TN(0,8);
      }

      // We adjust the offset depending on if tn2's add is before or
      // after this use and tn1's add was not.
      bool tn1_add_before = OP_Precedes(add_map[tn1], *used_ops_it);
      bool tn2_add_before = OP_Precedes(add_map[tn2], *used_ops_it);
      if (tn1_add_before && !tn2_add_before) {
        OP *add_op = add_map[tn2];
        if (OP_iadd(add_op)) {
          TN *imm_tn = OP_opnd(add_op, 1);
          if (TN_is_constant(imm_tn) && !TN_is_symbol(imm_tn)) {
            INT64 val = TN_value(offset_tn) + TN_value(imm_tn);
            offset_tn = ( Is_Target_32bit() ) ?
                          Gen_Literal_TN(val,4) :
                          Gen_Literal_TN(val,8);
          }
        }
      } else if (!tn1_add_before && tn2_add_before) {
        OP *add_op = add_map[tn2];
        if (OP_iadd(add_op)) {
          TN *imm_tn = OP_opnd(add_op, 1);
          if (TN_is_constant(imm_tn) && !TN_is_symbol(imm_tn)) {
            INT64 val = TN_value(offset_tn) - TN_value(imm_tn);
            offset_tn = ( Is_Target_32bit() ) ?
                          Gen_Literal_TN(val,4) :
                          Gen_Literal_TN(val,8);
          }
        }
      }

      whatnow = Compose_Mem_Base_Index_Mode(*used_ops_it, distance_tn,
                                            offset_tn, scale, tn2);
      OP_scycle(whatnow) = OP_scycle(*used_ops_it);

      Set_OP_unrolling(whatnow, OP_unrolling(*used_ops_it));
      Set_OP_orig_idx(whatnow, OP_map_idx(*used_ops_it));
      Set_OP_unroll_bb(whatnow, OP_unroll_bb(*used_ops_it));
      BB_Insert_Op_Before(target_bb,*used_ops_it,whatnow);
      OPS_Remove_Op(&target_bb->ops, *used_ops_it);
    }
    BB_Remove_Op(target_bb,add_map[tn1]);
  }
  GRA_LIVE_Compute_Liveness_For_BB(target_bb);
}

// Add the distance tests to the orig loop prolog and translate the interior 
// pointer copy of the multiverioned loops.
static void add_interior_ptr_tests_and_trans(
            std::map<TN*,OP*>& add_map,
            std::map<TN*,std::deque<OP*> >& use_map,
            std::map<INT,std::deque<ST*> >& correlated_addr_map,
            std::map<ST*,std::deque<TN*> >& symbol_addr_map,
            std::deque<INT>& size_queue,
            LOOP_DESCR* loop) {
  BB *loop_head = LOOP_DESCR_loophead(loop);
  BBLIST *lst;
  BB *orig_loop_prolog;
  BB *default_prolog;
  BB *orig_loop_epilog;
  TN *distance_tn = NULL;
  TN *br_targ_bb_tn = NULL;
  INT i, j;
  INT num_maps = size_queue.size();
  bool int_ptr_translated = false;

  // First find the imm pred of the loop, this is the orig_loop_prolog
  for ( lst = BB_preds(loop_head); lst != NULL;
        lst = BBLIST_next(lst) ) {
    BB *bb = BBLIST_item(lst);
    if (bb != loop_head) {
      orig_loop_prolog = bb;
      break;
    }
  }

  // Now find the default prolog, orig prolog has 2 successors, the loop head
  // and the default prolog.
  for ( lst = BB_succs(orig_loop_prolog); lst != NULL;
        lst = BBLIST_next(lst) ) {
    BB *bb = BBLIST_item(lst);
    if (bb != loop_head) {
      default_prolog = bb;
      break;
    }
  }

  // Now find the orig loop epilog, the orig loop has 2 successors, the
  // loop head and the orig loop epilog.
  for ( lst = BB_succs(loop_head); lst != NULL;
        lst = BBLIST_next(lst) ) {
    BB *bb = BBLIST_item(lst);
    if (bb != loop_head) {
      orig_loop_epilog = bb;
      break;
    }
  }

  // pick largest pattern
  INT max_pattern = 0;
  std::deque<INT>::const_iterator size_queue_iter;
  for (size_queue_iter = size_queue.begin();
       size_queue_iter != size_queue.end();
       ++size_queue_iter) {
    INT cor_addr_index = *size_queue_iter;
    INT cur_size = 0;
    INT new_size = 0;
    if (!correlated_addr_map[max_pattern].empty()) {
      std::deque<ST*> st_queue = correlated_addr_map[max_pattern];
      cur_size = st_queue.size() * max_pattern;
    }
    if (!correlated_addr_map[cor_addr_index].empty()) {
      std::deque<ST*> st_queue = correlated_addr_map[cor_addr_index];
      new_size = st_queue.size() * cor_addr_index;
    }
    if (new_size > cur_size)
      max_pattern = cor_addr_index;
  }

  // The overhead intailed in appending smaller distance calc ST groups
  // offers only a marginal benefit in the loop while adding signficant
  // overhead to the loop, so choose the largest ST group pattern.
  if (max_pattern != 0) {
    INT cor_addr_index = max_pattern;
    if (!correlated_addr_map[cor_addr_index].empty()) {
      std::deque<ST*> st_queue = correlated_addr_map[cor_addr_index];
      std::deque<TN*> st_unified_tns;
      bool is_internal_paired = (st_queue.size() == 1);
      INT num_syms_mapped = st_queue.size();

      std::deque<ST*>::iterator st_iter;
      for (st_iter = st_queue.begin();
           st_iter != st_queue.end();
           ++st_iter) {
        if (!symbol_addr_map[*st_iter].empty()) {
          std::deque<TN*> st_tns;
          st_tns = symbol_addr_map[*st_iter];
          for (i = 0; i < cor_addr_index; i+=2) {
            st_unified_tns.push_back(st_tns[i]);
            st_unified_tns.push_back(st_tns[i+1]);
          }
        }
      }

      // Now we have a unified set of basereg pairs which look
      // the same regardless of how they were processed.

      // Build the test expression and link it to flow
      TN *tnr = NULL;
      TN *expr_tn = NULL;
      TYPE_ID mtype;
      OPS ops = OPS_EMPTY;
      INT last_iter = st_unified_tns.size() - 2;
      for (i = 0; i < st_unified_tns.size(); i+=2) {
        TN *tn1 = st_unified_tns[i];
        TN *tn2 = st_unified_tns[i+1];

        if (i == 0) {
          int_ptr_translated = true;
          expr_tn = Build_TN_Like(tn1);
          tnr = Build_TN_Like(tn2);
          Reset_TN_is_global_reg(tn2);

          Build_OP( Is_Target_64bit() ? TOP_mov64 : TOP_mov32,
                    expr_tn, tn1, &ops);
          Set_OP_res_norename(OPS_last(&ops));
          Build_OP( Is_Target_64bit() ? TOP_mov64 : TOP_mov32,
                    tnr, tn2, &ops);
          Set_OP_res_norename(OPS_last(&ops));
          Build_OP( Is_Target_64bit() ? TOP_sub64 : TOP_sub32,
                    expr_tn, expr_tn, tnr, &ops);
          Set_OP_res_norename(OPS_last(&ops));
        } else {
          // all other cases
          Build_OP( Is_Target_64bit() ? TOP_mov64 : TOP_mov32,
                    tnr, tn1, &ops);
          Set_OP_res_norename(OPS_last(&ops));
          Build_OP( Is_Target_64bit() ? TOP_sub64 : TOP_sub32,
                    tnr, tnr, tn2, &ops);
          Set_OP_res_norename(OPS_last(&ops));
          if (i == last_iter) {
            Build_OP( Is_Target_64bit() ? TOP_xor64 : TOP_xor32,
                      tnr, tnr, expr_tn, &ops);
            Set_OP_res_norename(OPS_last(&ops));
          } else {
            Build_OP( Is_Target_64bit() ? TOP_or64 : TOP_or32,
                      expr_tn, expr_tn, tnr, &ops);
            Set_OP_res_norename(OPS_last(&ops));
          }
        }
      }
      Build_OP( Is_Target_64bit() ? TOP_test64 : TOP_test32, Rflags_TN(),
                tnr, tnr, &ops);
      BB_Insert_Ops_Before(orig_loop_prolog,
                           BB_first_op(orig_loop_prolog), &ops);
      GRA_LIVE_Compute_Liveness_For_BB(orig_loop_prolog);
      distance_tn = expr_tn;

      // Add the fixup code for the orig loop epilog to restore
      // live out regs of the loop.
      OPS fixup_ops = OPS_EMPTY;
      tnr = Build_TN_Like(distance_tn);
      Build_OP( Is_Target_64bit() ? TOP_mov64 : TOP_mov32,
                tnr, distance_tn, &fixup_ops);
      for (i = 0; i < st_unified_tns.size(); i+=2) {
        TN* tn1 = st_unified_tns[i];
        TN* tn2 = st_unified_tns[i+1];
        TN* scale = ( Is_Target_32bit() ) ?
                      Gen_Literal_TN(1,4) :
                      Gen_Literal_TN(1,8);
        TN* offset = ( Is_Target_32bit() ) ?
                       Gen_Literal_TN(0,4) :
                       Gen_Literal_TN(0,8);
        Build_OP ( Is_Target_64bit() ? TOP_leax64 : TOP_leax32,
                   tn1, tn2, tnr,
                   scale, offset, &fixup_ops);
        Set_OP_res_norename(OPS_last(&fixup_ops));
      }

      // This block is currently empty.
      BB_Prepend_Ops(orig_loop_epilog, &fixup_ops);
      GRA_LIVE_Compute_Liveness_For_BB(orig_loop_epilog);

      // Now remove the initial tn1 which we made of copy of in the
      // test from the loop so we get the physreg back
      interior_pointer_translation(loop_head, distance_tn,
                                   st_unified_tns, add_map, use_map);
      st_unified_tns.clear();
    }
  }
  if (int_ptr_translated && EBO_Trace_Optimization) {
    ST *pu_name = Get_Current_PU_ST();
    if (pu_name) {
      printf("interior pointer trans(%s) at bb(%d)\n",
             ST_name(pu_name), loop_head->id);
    }
  }
}

//
// end interior pointer implemenation
//

static bool histogram_of_index_counters (
    std::set<TN*> counted_base_regs,
    std::map<int,std::list<OP*> >& val_lis_map,
    std::map<TN*,OP*> add_map) {
    if (counted_base_regs.size() < 3)
        return false;
    std::set<TN*>::const_iterator counted_base_regs_it;
    for (counted_base_regs_it = counted_base_regs.begin();
         counted_base_regs_it != counted_base_regs.end();
         ++counted_base_regs_it) {
        OP* add_op_c = add_map[*counted_base_regs_it];
        TN* constant_tn = OP_opnd(add_op_c,1);
        val_lis_map[TN_value(constant_tn)].push_front(add_op_c);
    }

    if (val_lis_map.size() != 1) {
        return false;
    } else if (val_lis_map.begin()->first < 4) {
        return false;
    }
    return true;
}

static bool test_change_affects_flags (
    std::set<TN*> counted_base_regs,
    std::map<TN*,OP*> add_map) {
    std::set<TN*>::const_iterator counted_base_regs_it;
    for (counted_base_regs_it = counted_base_regs.begin();
         counted_base_regs_it != counted_base_regs.end();
         ++counted_base_regs_it) {
        OP* whc = add_map[*counted_base_regs_it];
        bool anychange = false;
        for (OP *tesop = OP_next(whc); 
                tesop != NULL; 
                tesop = OP_next(tesop)) {
	    TOP top = OP_code(tesop);
	    if (OP_reads_rflags(tesop)) {
	        return true;
	    }
	    if (TOP_is_change_rflags(top)) {
                anychange = true;
	        break;
            }
        }
        if (anychange == false)
            return false;
    }
    return false;
}

static bool collect_counters (bool pdom_header, 
    bool compare_undo, BB *fromwhere, 
    GTN_SET* otherliveins, std::map<TN*, bool>& avoid_table, 
    std::set<TN*>& seen_defs, std::set<TN*>& used_at_all, 
    std::map<TN*, OP*>& add_map, BB** add_bb,
    std::map<TN*,std::deque<OP*> >& use_map,
    std::map<TN*, OP*>& compare_map,
    std::set<TN*>& compare_opnds) {
    OP* op;
    op = BB_first_op(fromwhere);
    int insnum = 0;
    while(op != NULL) {
        // avoid_table holds operands for which 
        // SIB is not to be done
        // known defs tracks all defs along the
        // traversed control flow path
        for (INT i = 0; i < OP_opnds(op); ++i) {
            if (avoid_table.find(OP_opnd(op,i)) == avoid_table.end()) {
                avoid_table[OP_opnd(op,i)] = false;
            }
        }
        for (INT i = 0; i < OP_results(op); ++i) {
            if (avoid_table.find(OP_result(op,i)) == avoid_table.end()) {
                avoid_table[OP_result(op,i)] = false;
            }
        }
        std::ostringstream reason;
        {
            bool doesnotapply = false;
            if (OP_load_exe(op) || OP_load(op) 
                    || OP_store(op) || OP_prefetch(op)) {
                if (Test_if_base_mode(op) == false) {
                    doesnotapply = true;
                } else if (!(TN_has_value(OP_opnd(op, 
                                    OP_find_opnd_use(op,OU_offset))))) {
                    doesnotapply = true;
                }
            } else {
                if (pdom_header) {
                    if (compare_undo) {
                        if (!(OP_iadd(op) && 
                          (OP_result(op,0) == OP_opnd(op,0) &&
                          (TN_is_constant((OP_opnd(op,1)))))) && 
                          !((OP_code(op) == TOP_cmp64) && 
                            (TN_is_register(OP_opnd(op,1)) == TRUE) &&
                          (TN_is_register(OP_opnd(op,0)) == TRUE)) &&
                          !((OP_code(op) == TOP_cmp32) 
                            && (TN_is_register(OP_opnd(op,1)) == TRUE) &&
                            (TN_is_register(OP_opnd(op,0)) == TRUE))) {
                            doesnotapply = true;
                        }
                    } else {
                        if (!(OP_iadd(op) && 
                          (OP_result(op,0) == OP_opnd(op,0) &&
                          (TN_is_constant((OP_opnd(op,1))))))) {
                            doesnotapply = true;
                        }
                    }
                } else {
                    doesnotapply = true;
                }
            }
            if (doesnotapply) {
                // 
                // cases for which SIB is not applicable
                // we need to ensure that the registers
                // used in these cases are not used in  
                // a SIB situation (simplifying assumption)
                // add operands to the avoid_table
                // 
                for (INT i = 0; i < OP_opnds(op); ++i) {
                    avoid_table[OP_opnd(op,i)] = true;
                }
                for (INT i = 0; i < OP_results(op); ++i) {
                    avoid_table[OP_result(op,i)] = true;
                }
            } else {
                if (OP_load_exe(op) || OP_load(op) 
                        || OP_store(op) || OP_prefetch(op)) {
                    for (INT i = 0; i < OP_results(op); ++i) {
                        avoid_table[OP_result(op,i)] = true;
                    }
                    TN* which_base = OP_opnd(op,
                            TOP_Find_Operand_Use(OP_code(op), OU_base));

                    for (INT i = 0; i < OP_opnds(op); ++i) {
                        if (OP_opnd(op,i) != which_base) {
                            avoid_table[OP_opnd(op,i)] = true;
                        }
                    }
                    if (avoid_table[which_base] == false) {
                        used_at_all.insert(which_base);
                        // This will not replace an exiting mapping
                        use_map[which_base].push_front(op); 
                    }
                } else if (pdom_header && OP_iadd(op) 
                            && (OP_result(op,0) == OP_opnd(op,0) 
                            && (TN_is_constant((OP_opnd(op,1)))))) {
                    FmtAssert((OP_results(op) == 1), 
                                ("Add operation with more than one result"));
                    if (avoid_table[OP_result(op,0)] == false) {
                        if (otherliveins != NULL) {
                            if (GTN_SET_MemberP(otherliveins, 
                                        OP_result(op,0)) == 0) {
                                if (*add_bb == NULL)
                                    *add_bb = op->bb;
                                if (*add_bb != op->bb) 
                                    return false;
                                add_map[OP_result(op,0)] = op;
                                if (seen_defs.find (OP_result(op,0)) 
                                        == seen_defs.end())
                                    seen_defs.insert(OP_result(op,0));
                                else 
                                    avoid_table[OP_result(op,0)] = true;
                            } else {
                                // Some SIB like counters are 
                                // used outside the loop. we need 
                                // a special case fix for these.
                                // as of now we don't enable 
                                // SIB for these.
                                reason << " reason:LiveOut variable";
                                avoid_table[OP_result(op,0)] = true;
                            }
                        } else {
                            if (*add_bb == NULL)
                                *add_bb = op->bb;
                            if (*add_bb != op->bb) {
                                return false;
                            }

                            add_map[OP_result(op,0)] = op;
                            if (seen_defs.find (OP_result(op,0)) 
                                    == seen_defs.end())
                                seen_defs.insert(OP_result(op,0));
                            else 
                                avoid_table[OP_result(op,0)] = true;
                        }
                    }
                } else if (compare_undo && (OP_code(op) == TOP_cmp64
                                || OP_code(op) == TOP_cmp32)) {
                    for (INT i = 0; i < OP_opnds(op); ++i) {
                        if (TN_is_register(OP_opnd(op,i))) {
                            if (avoid_table[OP_opnd(op,i)] == false) {
                                if (otherliveins != NULL) {
                                    if (GTN_SET_MemberP(otherliveins, 
                                            OP_result(op,0)) == 0) {
                                        if (compare_map.find(OP_opnd(op,i)) == compare_map.end()) {
                                            compare_map[OP_opnd(op,i)] = op;
                                            compare_opnds.insert(OP_opnd(op,i));
                                        } else {
                                            avoid_table[OP_opnd(op,i)] = true;
                                        }
                                    } else {
                                        avoid_table[OP_opnd(op,i)] = true;
                                    }
                                } else {
                                    if (compare_map.find(OP_opnd(op,i)) == compare_map.end()) {
                                        compare_map[OP_opnd(op,i)] = op;
                                        compare_opnds.insert(OP_opnd(op,i));
                                    } else {
                                        avoid_table[OP_opnd(op,i)] = true;
                                    }
                                }
                            } 
                        }
                    }
                } else {
                    if (pdom_header || compare_undo) {
                        return false;
                    } else {
                        for (INT i = 0; i < OP_opnds(op); ++i) {
                            avoid_table[OP_opnd(op,i)] = true;
                        }
                        for (INT i = 0; i < OP_results(op); ++i) {
                            avoid_table[OP_result(op,i)] = true;
                        }
                    }
                }
            }
        }
        op = OP_next(op);
        insnum++;
    }
    return true;
}

static void fix_base_instructions (char* Cur_PU_Name,
        LOOP_DESCR* loop, GTN_SET* otherliveins, MEM_POOL *pool) {
    std::map<TN*, bool> avoid_table;
    std::set<TN*> seen_defs;
    std::set<TN*> used_at_all;
    std::map<TN*, OP*> add_map; 
    std::map<TN*,std::deque<OP*> > use_map;
    std::map<ST*,std::deque<TN*> > symbol_addr_map;
    std::map<INT,std::deque<ST*> > correlated_addr_map;
    std::set<ST*> counted_addr_sts;
    std::deque<INT> size_queue;
    std::set<OP*> dep_map;
    std::set<TN*> counted_base_regs;
    std::map<TN*, OP*> compare_map;
    std::set<TN*> compare_opnds;
    LOOPINFO *info = LOOP_DESCR_loopinfo(loop);
    BB *lhead;
    lhead = LOOP_DESCR_loophead(loop);
    BB *add_bb;
    BB *abbinloop;
    OP *opi;
    bool outofhere = false;
    bool can_mv = false;
    bool loop_vectorized = false;
    add_bb = NULL;

    if (info && LOOPINFO_vectorized(info))
      loop_vectorized = true;

    FOR_ALL_BB_SET_members(LOOP_DESCR_bbset(loop), abbinloop) {
        bool pdom_header = false;
        bool compare_undo = false;
        if (BS_MemberP(BB_pdom_set(lhead), BB_id(abbinloop))) {
            pdom_header = true;
        } else {
            pdom_header = false;
        }

        if (collect_counters (pdom_header, 
                    compare_undo && pdom_header, abbinloop, 
                    otherliveins, avoid_table, 
                    seen_defs, used_at_all, 
                    add_map, &add_bb, use_map, 
                    compare_map, compare_opnds) == false) {
            outofhere == true;
            break;
        }
    }

    if (outofhere) {
        return;
    } else if (add_bb == NULL) {
        return;
    }

    for (std::set<TN*>::iterator sdit = 
        seen_defs.begin(); sdit != seen_defs.end(); ++sdit) {
        if (avoid_table[*sdit] == false) {
            if (used_at_all.find(*sdit) != used_at_all.end()) {
                counted_base_regs.insert(*sdit);
            }
        }
    }

    //
    // find out the implications of reordering the add instruction
    // We do a walk on the counted_base_regs, get the use_map 
    // for each such reg, and check if the def is either
    // dominating or post-dominating it. If it is dominating it,
    // we dont need compensation code for it. If it is post-dominating 
    // and the new inc's location is not post-dominating it, 
    // then we need compensation code for the use.
    //

    std::set<TN*>::iterator cbreg_it;
    std::set<TN*> skip_set;
    for (cbreg_it = counted_base_regs.begin(); 
         cbreg_it != counted_base_regs.end();
         ++cbreg_it) {
        std::map<TN*, std::deque<OP*> >::iterator use_map_it;
        if (add_map.find(*cbreg_it) == add_map.end()) {
            skip_set.insert (*cbreg_it);
        }
    }

    for (std::set<TN*>::iterator skip_it = skip_set.begin();
         skip_it != skip_set.end(); ++skip_it) {
        counted_base_regs.erase(*skip_it);
    }

    
    for (cbreg_it = counted_base_regs.begin(); 
         cbreg_it != counted_base_regs.end();
         ++cbreg_it) {
        OP* defing_ins = add_map[*cbreg_it];
        BB* defing_bb = defing_ins->bb;

        std::deque<OP*>::iterator use_deq_it;
        for (use_deq_it = use_map[*cbreg_it].begin();
                 use_deq_it != use_map[*cbreg_it].end();
                 ++use_deq_it) {
            BB* where_used = (*use_deq_it)->bb;
            if (defing_bb == where_used) {
                bool usefirst = true;
                for (OP* whichop = *use_deq_it;
                        whichop != NULL;
                        whichop = OP_prev(whichop)) {
                    if (whichop == defing_ins) {
                        usefirst = false;
                        break;
                    }
                }
                if (usefirst == true) 
                    dep_map.insert (*use_deq_it);
            }
        }
    }

    std::map<int,std::list<OP*> > val_lis_map;
    if (histogram_of_index_counters (counted_base_regs, 
              val_lis_map, add_map) == false) {
        return;
    }

    if (test_change_affects_flags (counted_base_regs, 
                      add_map) == true) {
          return;
    }

    // Interior pointers keys off of loops with basereg(gpr) reg pressure
    if (CG_interior_ptrs_x86 &&
        (BB_SET_Size(LOOP_DESCR_bbset(loop)) == 1) &&
        have_basereg_pressure(counted_base_regs, lhead)) {
      // create a set to aid interior pointer analysis
      BS *def_set = BS_Create_Empty(PU_BB_Count + 1, pool);

      // Build and process interior pointers on base regs(mv case)
      build_addr_queues (counted_addr_sts,
              symbol_addr_map, use_map,
              counted_base_regs, def_set);
      // After building interior pointers candidates, remove
      // all the effected counted_base_regs from SIB processing so
      // that we do not translate them.
      can_mv = remove_cands_from_sib_gen_and_correlate(size_queue,
                                                       counted_addr_sts,
                                                       correlated_addr_map,
                                                       symbol_addr_map,
                                                       counted_base_regs,
                                                       add_map,
                                                       lhead, 
                                                       loop_vectorized,
                                                       pool);
    }

    OP* add_template = val_lis_map.begin()->second.front();
    if (counted_base_regs.size() == 0) {
          return;
    } else {
          TN* newinc; 
          newinc = OP_opnd(add_template,1);
          if (TN_value(newinc) <= 0 ) {
              return;
          }
    }
    
    TN* newinc; 
    if (Is_Target_32bit()) {
        newinc = Gen_Literal_TN (TN_value(OP_opnd(add_template,1)), 4);
    } else {
        newinc = Gen_Literal_TN (TN_value(OP_opnd(add_template,1)), 8);
    }
    OP* newadd;
    TN* newres = Build_TN_Like(OP_result(add_template,0));
    Set_TN_is_global_reg(newres);
    TOP alea;
    if (Is_Target_32bit()) {
        alea = TOP_lea32;
    } else {
        alea = TOP_lea64;
    }
    newadd = Mk_OP(alea, newres, newres, newinc);
    newadd->bb = add_bb;
    BB_Prepend_Op(add_bb,newadd);
    bool aft_prnt = false;
    std::set<TN*>::const_iterator counted_base_regs_it;
    std::set<OP*> changed_ops;
    for (counted_base_regs_it = counted_base_regs.begin();
         counted_base_regs_it != counted_base_regs.end();
         ++counted_base_regs_it) {
        std::deque<OP*> used_ops;
        used_ops = use_map[(*counted_base_regs_it)];
        std::deque<OP*>::iterator used_ops_it;
        for (used_ops_it = used_ops.begin(); 
             used_ops_it != used_ops.end();
             ++used_ops_it) {
            OP* whatnow;
            OP* newadd; 
            OP* newmovop;
            TN *movtn; 
            TN* scale;
            if( Is_Target_32bit() ){
                scale = Gen_Literal_TN(1,4);
            } else {
                scale = Gen_Literal_TN(1,8);
            }
            
            TN* offset_d;
            if (dep_map.find(*used_ops_it) == dep_map.end()) {
                if (Is_Target_32bit()) {
                    offset_d = Gen_Literal_TN (TN_value(OP_opnd(*used_ops_it,
                          TOP_Find_Operand_Use(OP_code(*used_ops_it), 
                      OU_offset))), 4); 
                } else {
                    offset_d = Gen_Literal_TN (TN_value(OP_opnd(*used_ops_it,
                          TOP_Find_Operand_Use(OP_code(*used_ops_it), 
                      OU_offset))), 4); 
                }
            } else {
                if( Is_Target_32bit() ){
                    offset_d = Gen_Literal_TN (
                                TN_value(OP_opnd(*used_ops_it,
                                  TOP_Find_Operand_Use(
                                    OP_code(*used_ops_it), OU_offset))) 
                                - TN_value(newinc), 4);
                } else {
                    offset_d = Gen_Literal_TN (
                                TN_value(OP_opnd(*used_ops_it,
                                    TOP_Find_Operand_Use(
                                        OP_code(*used_ops_it), OU_offset)))
                                - TN_value(newinc), 8);
                }
            }

            whatnow = Compose_Mem_Base_Index_Mode(*used_ops_it,
                  newres, offset_d, scale, 
                  OP_opnd(*used_ops_it,
                      TOP_Find_Operand_Use(OP_code(*used_ops_it), 
                  OU_base)));
            OP_scycle(whatnow) = OP_scycle(*used_ops_it);

            BB_Insert_Op_Before((*used_ops_it)->bb,*used_ops_it,whatnow);
            whatnow->bb = (*used_ops_it)->bb;
            changed_ops.insert(whatnow);
            OPS_Remove_Op(&((*used_ops_it)->bb->ops), *used_ops_it);
        }
    }

    for (counted_base_regs_it = counted_base_regs.begin();
         counted_base_regs_it != counted_base_regs.end();
         ++counted_base_regs_it) {
        OP* anaddop = add_map[*counted_base_regs_it];
        BB_Remove_Op(anaddop->bb,anaddop);
    }

    std::set<OP*>::iterator changed_ops_it;
    bool enable_large_disp_opt = false;
    std::set<OP*> large_disp_ops;
    if (enable_large_disp_opt) {
        for (changed_ops_it = changed_ops.begin();
             changed_ops_it != changed_ops.end();
             ++changed_ops_it) {
            TN* theofst = OP_opnd(*changed_ops_it, 
                    TOP_Find_Operand_Use(OP_code(*changed_ops_it),
                        OU_offset));
            FmtAssert ((TN_is_constant(theofst)), 
                    ("Not a constant offset\n"));
            INT64 theval = TN_value(theofst);
            // make a new lea op Mk_Op
            if (theval < 127) 
                if (theval > -128)
                    continue;
            TOP alea;
            TN* zeroval;
            if (Is_Target_32bit()) {
                zeroval = Gen_Literal_TN (0,4);
                alea = TOP_lea32;
            } else {
                zeroval = Gen_Literal_TN (0,8);
                alea = TOP_lea64;
            }
            TN* thebase = OP_opnd(*changed_ops_it,
                        TOP_Find_Operand_Use(OP_code(*changed_ops_it),
                            OU_base));
            TN* newgprtn = Build_TN_Like(thebase);
            OP* thelea = Mk_OP(alea, newgprtn, thebase, theofst);
            INT offset_loc, base_loc;
            offset_loc = TOP_Find_Operand_Use(OP_code(*changed_ops_it),
                        OU_offset);
            base_loc = TOP_Find_Operand_Use(OP_code(*changed_ops_it),
                        OU_base);
            Set_OP_opnd(*changed_ops_it, offset_loc, zeroval);
            Set_OP_opnd(*changed_ops_it, base_loc, newgprtn);
            large_disp_ops.insert(thelea);
        }
    }
    
    BBLIST* predlis = BB_preds(LOOP_DESCR_loophead(loop));
    bool inserted_some = false;
    while (predlis) {
        BB *pred = BBLIST_item(predlis);
        predlis = BBLIST_next(predlis);
        if (!BB_SET_MemberP(LOOP_DESCR_bbset(loop), pred)) {
            inserted_some = true;
            OP* init_co;
            TN* zeroval;
            TOP whichldc;
            if( Is_Target_32bit()) {
                zeroval = Gen_Literal_TN (0,4);
                whichldc = TOP_ldc32;
            } else {
                whichldc = TOP_ldc64;
                zeroval = Gen_Literal_TN (0,8);
            }
            init_co = Mk_OP (whichldc, newres, zeroval , newres);
            if (BB_branch_op(pred)) {
                BB_Insert_Op_Before(pred,BB_branch_op(pred),init_co);
                init_co->bb = pred;
                std::set<OP*>::iterator init_ops_it;
                if (enable_large_disp_opt) {
                    for (init_ops_it = large_disp_ops.begin();
                       init_ops_it != large_disp_ops.end();
                       ++init_ops_it) {
                        OP* toinse;
                        toinse = Mk_OP (OP_code(*init_ops_it), 
                            OP_result (*init_ops_it,0),
                            OP_opnd (*init_ops_it,0),
                            OP_opnd (*init_ops_it,1));
                        BB_Insert_Op_Before(pred,BB_branch_op(pred),toinse);
                    }
                }
            } else {
                BB_Append_Op(pred,init_co);
                init_co->bb = pred;
                std::set<OP*>::iterator init_ops_it;
                if (enable_large_disp_opt) {
                    for (init_ops_it = large_disp_ops.begin();
                         init_ops_it != large_disp_ops.end();
                         ++init_ops_it) {
                        OP* toinse;
                        toinse = Mk_OP (OP_code(*init_ops_it), 
                                 OP_result (*init_ops_it,0),
                                 OP_opnd (*init_ops_it,0),
                                 OP_opnd (*init_ops_it,1));
                        BB_Append_Op(pred,toinse);
                    }
                }
            }
        }
    }

    if (CG_interior_ptrs_x86 && can_mv) {
      CG_LOOP_Multiversion(loop, 1, pool);
      // Add distance and unification tests to orig loop prolog and translate
      add_interior_ptr_tests_and_trans(add_map, use_map, correlated_addr_map,
                                       symbol_addr_map, size_queue, loop);
    }

    FmtAssert(inserted_some == true, 
            ("No init instruction inserted!"));
    GRA_LIVE_Recalc_Liveness (NULL);
}

static void fix_compare_binding (LOOP_DESCR* loop, 
        GTN_SET* otherliveins) {
      std::map<TN*, bool> avoid_table;
      std::set<TN*> seen_defs;
      std::set<TN*> used_at_all;
      std::map<TN*, OP*> add_map; 
      std::map<TN*,std::deque<OP*> > use_map;
      std::set<OP*> dep_map;
      std::set<TN*> counted_base_regs;
      std::map<TN*, OP*> compare_map;
      std::set<TN*> compare_opnds;
      BB *lhead;
      lhead = LOOP_DESCR_loophead(loop);
      BB *add_bb;
      add_bb = NULL;
      BB *abbinloop;
      OP *opi;
      bool outofhere = false;
      FOR_ALL_BB_SET_members(LOOP_DESCR_bbset(loop), abbinloop) {
          bool pdom_header = false;
          bool compare_undo = true;
          if (BS_MemberP(BB_pdom_set(lhead), BB_id(abbinloop))) {
              pdom_header = true;
          } else {
              pdom_header = false;
          }

          if (collect_counters (pdom_header, 
                      compare_undo && pdom_header, abbinloop, 
                      otherliveins, avoid_table, 
                      seen_defs, used_at_all, 
                      add_map, &add_bb, use_map, 
                      compare_map, compare_opnds) == false) {
              outofhere == true;
              break;
          }
      }

      if (outofhere) {
          return;
      } else if (add_bb == NULL) {
          return;
      }

      // additional checks
      for (std::set<TN*>::iterator sdit = 
          seen_defs.begin(); sdit != seen_defs.end(); ++sdit) {
          if (avoid_table[*sdit] == false) {
              if (used_at_all.find(*sdit) != used_at_all.end()) {
                  counted_base_regs.insert(*sdit);
              }
          }
      }

      //
      // find out the implications of reordering the add instruction
      // We do a walk on the counted_base_regs, get the use_map 
      // for each such reg, and check if the def is either
      // dominating or post-dominating it. If it is dominating it,
      // we dont need compensation code for it. If it is post-dominating 
      // and the new inc's location is not post-dominating it, 
      // then we need compensation code for the use.
      //

      std::set<TN*>::iterator cbreg_it;
      std::set<TN*> skip_set;
      // additional checks
      for (cbreg_it = counted_base_regs.begin(); 
           cbreg_it != counted_base_regs.end();
           ++cbreg_it) {
          std::map<TN*, std::deque<OP*> >::iterator use_map_it;
          if (add_map.find(*cbreg_it) == add_map.end()) {
              skip_set.insert (*cbreg_it);
          }
      }

      for (std::set<TN*>::iterator skip_it = skip_set.begin();
           skip_it != skip_set.end(); ++skip_it) {
          counted_base_regs.erase(*skip_it);
      }

      std::map<int,std::list<OP*> > val_lis_map;
      if (histogram_of_index_counters (counted_base_regs, 
              val_lis_map, add_map) == false) {
          return;
      }

      if (test_change_affects_flags (counted_base_regs, 
                      add_map) == true) {
          return;
      }

      int cardinale = 0;
      std::set<TN*> interset;
      for (std::set<TN*>::iterator comit = 
           compare_opnds.begin(); comit != compare_opnds.end(); ++comit) {
          if (counted_base_regs.find(*comit) != counted_base_regs.end()) {
              cardinale++;
              interset.insert(*comit);
          }
      }

      if (cardinale != 1) {
          for (std::set<TN*>::iterator comit = 
             compare_opnds.begin(); comit != compare_opnds.end(); ++comit) {
              counted_base_regs.erase (*comit);
          }
      } else {
          TN* bound_name = *(interset.begin());
          counted_base_regs.erase (bound_name);
          TOP whichmov;
          TN* homing_gr;
          if (Is_Target_32bit()) {
              whichmov = TOP_mov32;
          } else {
              whichmov = TOP_mov64;
          }
          homing_gr = Build_TN_Like(bound_name);
          OP* new_add_op;
          TOP add_opcod;
          if (Is_Target_32bit()) {
              add_opcod = TOP_addi32;
          } else {
              add_opcod = TOP_addi64;
          }

          new_add_op = Mk_OP(add_opcod, homing_gr, homing_gr, 
                OP_opnd(add_map[bound_name],1)); 
          BB_Insert_Op_Before (add_bb, add_map[bound_name], new_add_op);
          OP_scycle(new_add_op) = OP_scycle(add_map[bound_name]);
          new_add_op->bb = add_bb;

          FOR_ALL_BB_SET_members(LOOP_DESCR_bbset(loop), abbinloop) {
              FOR_ALL_BB_OPs (abbinloop,opi) {
                  if (opi != compare_map [bound_name] 
                          && opi != add_map[bound_name]) {
                      for (int i = 0; i < OP_opnds(opi); i++) {
                          if (OP_opnd(opi,i) == bound_name)
                              Set_OP_opnd (opi, i, homing_gr);
                      }
                      for (int i = 0; i < OP_results(opi); i++) {
                          if (OP_result(opi,i) == bound_name)
                              Set_OP_result (opi, i, homing_gr);
                      }
                  }
              }
          }

          use_map[homing_gr] = use_map[bound_name];
          use_map.erase(bound_name);
          add_map[homing_gr] = new_add_op;
          add_map.erase(bound_name);
          counted_base_regs.insert (homing_gr);
          counted_base_regs.erase (bound_name);

          BBLIST* predlis = BB_preds(LOOP_DESCR_loophead(loop));
          while (predlis) {
              BB *pred = BBLIST_item(predlis);
              predlis = BBLIST_next(predlis);
              if (!BB_SET_MemberP(LOOP_DESCR_bbset(loop), pred)) {
                  if (BB_branch_op(pred)) {
                      OP* homing_mov;
                      homing_mov = Mk_OP(whichmov, homing_gr, bound_name);
                      BB_Insert_Op_Before (pred, BB_branch_op(pred), homing_mov);
                      homing_mov->bb = pred;
                  } else {
                      OP* homing_mov;
                      homing_mov = Mk_OP(whichmov, homing_gr, bound_name);
                      BB_Append_Op(pred,homing_mov);
                      homing_mov->bb = pred;
                  }
              }
          }
          GRA_LIVE_Recalc_Liveness (NULL);
      }
}

void Counter_Merge (char *Cur_PU_Name) {
  GRA_LIVE_Recalc_Liveness (NULL);
  MEM_POOL loop_descr_pool;

  memset(&loop_descr_pool, 0, sizeof loop_descr_pool);
  MEM_POOL_Initialize(&loop_descr_pool, "loop_descriptors", TRUE);
  MEM_POOL_Push (&loop_descr_pool);
  Calculate_Dominators();

  LOOP_DESCR *theloops = LOOP_DESCR_Detect_Loops(&loop_descr_pool);
  dfo_map = BB_Depth_First_Map(NULL, NULL);
  for (LOOP_DESCR *loop = theloops; loop; loop = LOOP_DESCR_next(loop)) {
      if (!BB_innermost(LOOP_DESCR_loophead(loop)))
          continue;
      BB *abbinloop;
      OP *opi;
      bool dontdoit = false;
      FOR_ALL_BB_SET_members(LOOP_DESCR_bbset(loop), abbinloop) {
        FOR_ALL_BB_OPs (abbinloop,opi) {
            // escape hatch for loops that hold volatile
            // registers or have calls. calls may modify
            // contents of SIB registers
            if (OP_volatile(opi))
                dontdoit = true;
            if (OP_call(opi))
                dontdoit = true;
        }
      }
      if (dontdoit) continue;
    
      //
      // collect the LiveIn sets of pdoms of loop header
      // which are not part of the loop
      // 
      GTN_SET* otherliveins;
      otherliveins = NULL;
      static MEM_POOL merge_counter_pool;
      static BOOL merge_counter_pool_init;

      if (! merge_counter_pool_init) {
        merge_counter_pool_init = TRUE;
        MEM_POOL_Initialize(&merge_counter_pool, 
                            "loop_descriptors", TRUE);
      }
      MEM_POOL_Push (&merge_counter_pool);

      // We collect the live-ins of successors of the loop BBs
      // which are not part of the loop.
      // We will avoid doing SIB for the variables which are 
      // part of that live-in set
      FOR_ALL_BB_SET_members(LOOP_DESCR_bbset(loop), abbinloop) {
          BBLIST* succlis = BB_succs(abbinloop);
          while (succlis) {
              BB *succ = BBLIST_item(succlis);
              succlis = BBLIST_next(succlis);
              if (!BB_SET_MemberP(LOOP_DESCR_bbset(loop), succ)) {
                  if (otherliveins == NULL) {
                      otherliveins = BB_live_in(succ);
                  } else {
                      otherliveins = GTN_SET_Union (otherliveins,
                          BB_live_in (succ), &merge_counter_pool);
                  }
              }
          }
      }

      fix_compare_binding (loop, otherliveins);

      // collect otherliveins again as fix_compare_binding may have
      // changed liveness
      otherliveins = NULL;
      FOR_ALL_BB_SET_members(LOOP_DESCR_bbset(loop), abbinloop) {
          BBLIST* succlis = BB_succs(abbinloop);
          while (succlis) {
              BB *succ = BBLIST_item(succlis);
              succlis = BBLIST_next(succlis);
              if (!BB_SET_MemberP(LOOP_DESCR_bbset(loop), succ)) {
                  if (otherliveins == NULL) {
                      otherliveins = BB_live_in(succ);
                  } else {
                      otherliveins = GTN_SET_Union (otherliveins,
                          BB_live_in (succ), &merge_counter_pool);
                  }
              }
          }
      }

      fix_base_instructions (Cur_PU_Name, loop, otherliveins, 
                             &merge_counter_pool);

      MEM_POOL_Pop(&merge_counter_pool);
  }

  MEM_POOL_Pop (&loop_descr_pool);
  MEM_POOL_Delete(&loop_descr_pool);
  Free_Dominators_Memory ();
}

static OP *
Compose_Mem_Op( OP* op, TN* index, TN* offset, TN* scale, TN* base )
{
  Is_True( offset != NULL, ("Compose_Mem_Op: offset is NULL") );

  OP* new_op = NULL;
  ADDR_MODE mode = N32_MODE;

  if (index != NULL)
    mode = base == NULL ? INDEX_MODE : BASE_INDEX_MODE;
  else if (base != NULL)
    mode = BASE_MODE;

  const TOP new_top = Get_Top_For_Addr_Mode(OP_code(op), mode);

  FmtAssert( new_top != TOP_UNDEFINED, ("Compose_Mem_Op: unknown top") );

  if( TOP_is_prefetch( new_top ) ){
    if( mode == INDEX_MODE )
      new_op = Mk_OP( new_top, OP_opnd( op, 0 ), index, scale, offset );
    else
      new_op = Mk_OP( new_top, OP_opnd( op, 0 ), base, offset, index, scale );

  } else {
    TN* storeval = NULL;

    if( TOP_is_store(new_top) ){
      storeval = OP_opnd( op, OP_find_opnd_use( op, OU_storeval ) );
    } else {
      storeval = OP_result( op, 0 );
    }

    if( new_top == TOP_leax64 ){
      Is_True(mode != N32_MODE, ("Compose_Mem_Op: unexpected address mode"));
      if( mode == INDEX_MODE )
	new_op = Mk_OP( new_top, storeval, index, scale, offset );
      else
	new_op = Mk_OP( new_top, storeval, base, index, scale, offset );
    } else {
      if (Is_Target_Orochi() && Is_Target_AVX() && OP_load(op) &&
          (OP_vec_lo_ldst(op) || OP_vec_hi_ldst(op))) {
        if (mode == N32_MODE)
  	  new_op = Mk_OP( new_top, storeval, OP_opnd( op, 0 ), offset );
        else if (mode == INDEX_MODE)
  	  new_op = Mk_OP( new_top, storeval, OP_opnd( op, 0 ), 
                          index, scale, offset );
        else if (mode == BASE_INDEX_MODE)
	  new_op = Mk_OP( new_top, storeval, OP_opnd( op, 0 ),
                          base, index, scale, offset );    
        else
	  new_op = Mk_OP( new_top, storeval, OP_opnd( op, 0 ),
                          base, offset );    
      } else {
        if (mode == N32_MODE)
  	  new_op = Mk_OP( new_top, storeval, offset );
        else if (mode == INDEX_MODE)
  	  new_op = Mk_OP( new_top, storeval, index, scale, offset );
        else
	  new_op = Mk_OP( new_top, storeval, base, offset, index, scale );    
      }
    }
  }

  return new_op;
}


// Compose a memory OP that looks like OP but with the address mode specified
// by OFFSET/BASE/INDEX/SCALE.  Copy OP's info to the new OP.
static OP *
Compose_Mem_Op_And_Copy_Info(OP* op, TN* index_tn, TN* offset_tn, TN* scale_tn,
			     TN* base_tn, EBO_TN_INFO **actual_tninfo)
{
  OP *new_op = Compose_Mem_Op(op, index_tn, offset_tn, scale_tn, base_tn);

  Copy_WN_For_Memory_OP(new_op, op);
  if (OP_volatile(op)) // Bug 4245 - copy "volatile" flag
    Set_OP_volatile(new_op);
  OP_srcpos(new_op) = OP_srcpos(op);

  Set_OP_unrolling(new_op, OP_unrolling(op));
  Set_OP_orig_idx(new_op, OP_map_idx(op));
  Set_OP_unroll_bb(new_op, OP_unroll_bb(op));

  if (EBO_in_loop) {
    CG_LOOP_Init_Op(new_op);
    const INT op_base_idx = OP_find_opnd_use(op, OU_base);
    EBO_TN_INFO *base_tninfo = op_base_idx >= 0 ?
				 actual_tninfo[op_base_idx] : NULL;
    if (base_tninfo != NULL && base_tninfo->omega != 0) {
      Set_OP_omega(new_op, OP_find_opnd_use(new_op, OU_base),
		   base_tninfo->omega);
    }

    if (index_tn != NULL) {
      EBO_TN_INFO *tninfo = get_tn_info(index_tn);
      if (tninfo != NULL && tninfo->omega != 0) {
	Set_OP_omega(new_op, OP_find_opnd_use(new_op, OU_index), tninfo->omega);
      }
    }
  }

  return new_op;
}

static
BOOL examine_backward_bb(BS * visit_set, OP* last_op, OP *def_op, TN* base_tn) 
{

    Is_True(!BS_MemberP(visit_set, BB_id(OP_bb(last_op))), ("bb should not visited"));
    Is_True(TN_is_gra_homeable(base_tn), ("only homeable tn is passed"));
    
    visit_set = BS_Union1D(visit_set, BB_id(OP_bb(last_op)), &MEM_local_pool);
    OP* op = NULL;
    for (op = last_op; op != NULL; op = OP_prev(op)) {

        if (op == def_op)  break;

        if (OP_store(op)) {
            WN *wn = Get_WN_From_Memory_OP(op);
            if (wn != NULL) {

                // if there is no alias manager, be conservative
                if (Alias_Manager == NULL) return TRUE;
                
                ALIAS_RESULT result = Aliased(Alias_Manager, TN_home(base_tn), wn);
                if (result == POSSIBLY_ALIASED || result == SAME_LOCATION )
                    return TRUE;
            }
        }
   }

   return FALSE;

}

static
BOOL search_preds_for_alias(BS* visit_set, OP * last_op, OP *def_op, TN* base_tn)
{
    if (!BS_MemberP(visit_set, BB_id(OP_bb(last_op)) && 
        examine_backward_bb(visit_set, last_op, def_op, base_tn)))
        return TRUE;
    
    BBLIST *lst;
    for (lst = BB_preds(OP_bb(last_op)); lst != NULL; lst = BBLIST_next(lst)) {
        BB * cur_bb = BBLIST_item(lst);
        if (BB_last_op(cur_bb) &&
            search_preds_for_alias(visit_set, BB_last_op(cur_bb), def_op, base_tn)) 
            return TRUE;
    }

    return FALSE;
}

// check whether there is a store that is alias with base_tn's home
// in the path from def_op to use_op
static
BOOL alias_store_in_path(OP* use_op, OP* def_op, TN* base_tn)
{
    BB_SET* visit_set = BB_SET_Universe(PU_BB_Count+2, &MEM_local_pool);
    BS_ClearD(visit_set);
    
    if(search_preds_for_alias(visit_set, use_op, def_op, base_tn))
        return TRUE;

    return FALSE;
    
}

BOOL EBO_Merge_Memory_Addr( OP* op,
			    TN** opnd_tn,
			    EBO_TN_INFO** opnd_tninfo,
			    EBO_TN_INFO** actual_tninfo )
{
#if Is_True_On
  if (!(EBO_Opt_Mask & EBO_MERGE_MEMORY_ADDR)) return FALSE;
#endif
  if( EBO_in_peep ){
    return FALSE;
  }

  if (Get_Top_For_Addr_Mode(OP_code(op), BASE_MODE) == TOP_UNDEFINED) {
    return FALSE;
  }

  if (Is_Target_Orochi() && Is_Target_AVX() && 
      (CG_load_execute == 0) &&
      (OP_vec_lo_ldst(op) || OP_vec_hi_ldst(op))) {
    return FALSE;
  }

  const INT op_base_idx = OP_find_opnd_use( op, OU_base );
  EBO_TN_INFO* base_tninfo = op_base_idx >= 0 ? actual_tninfo[op_base_idx] : NULL;
  OP* addr_op = (base_tninfo != NULL) ? base_tninfo->in_op : NULL;
  bool pass = false;
  TN* index_tn  = NULL;
  TN* offset_tn = NULL;
  TN* scale_tn  = NULL;
  TN* base_tn   = NULL;

  // First, try to subsume the base.
  if( addr_op != NULL && !OP_load(addr_op) && !OP_simulated(addr_op) ){
    pass = Compose_Addr( op, base_tninfo, OU_base, opnd_tn,
			 &index_tn, &offset_tn, &scale_tn, &base_tn );
  }

  // Otherwise, try to subsume the index.
  if( !pass ){
    const INT op_index_idx = OP_find_opnd_use( op, OU_index );
    if( op_index_idx < 0 )
      return FALSE;

    EBO_TN_INFO* index_tninfo = actual_tninfo[op_index_idx];
    if( index_tninfo == NULL || index_tninfo->in_op == NULL )
      return FALSE;

    addr_op = index_tninfo->in_op;

    if( addr_op == NULL || OP_load(addr_op) || OP_simulated(addr_op) )
      return FALSE;

    if( !Compose_Addr( op, index_tninfo, OU_index, opnd_tn,
		       &index_tn, &offset_tn, &scale_tn, &base_tn ) )
      return FALSE;
  }

  // TODO: support N32_MODE for -m32
  if( base_tn == NULL && index_tn == NULL )
    return FALSE;

  // disable merging memory addr on TLS initial-exec
  if( TN_relocs(offset_tn) == TN_RELOC_X8664_GOTTPOFF )
    return FALSE;

  TN* rip = Rip_TN();
  if ( index_tn == rip || base_tn == rip )
    return FALSE;

  // when base_tn is gra_homeable and there is an alias
  // def in between, we need to make tn to be none homeable
  // For example,
  // s1: t = ld x
  // s2: t1 = t
  // s3: st x
  // s4: st t1
  // after ebo merging address, s4 changes to
  // s4: st t
  // If t is homeable and is chosen to be spilled, it will be restored
  // from its home location at s4. Thus, it will have wrong value. 
  if(base_tn != NULL && TN_is_gra_homeable(base_tn) &&
     alias_store_in_path(op, addr_op, base_tn)){
     Reset_TN_is_gra_homeable(base_tn);
     Set_TN_home(base_tn, NULL);
  }

  OP* new_op = Compose_Mem_Op_And_Copy_Info(op, index_tn, offset_tn, scale_tn,
					    base_tn, actual_tninfo);

  BB_Insert_Op_After( OP_bb(op), op, new_op);

  if( EBO_Trace_Optimization ){
    #pragma mips_frequency_hint NEVER
    fprintf( TFile,
	     "%sin BB:%d merge memory addr expression (from BB:%d) with offset (in BB:%d)\n",
	     EBO_trace_pfx, BB_id(OP_bb(op)),BB_id(OP_bb(addr_op)),BB_id(OP_bb(op)) );
    Print_OP_No_SrcLine(addr_op);
    Print_OP_No_SrcLine(op);

    fprintf(TFile, "  to insert the new op:\n");
    Print_OP_No_SrcLine(new_op);
    fprintf( TFile, "\n" );
  }

  return TRUE;
}


/* return TRUE if <ex_op> is safe to perform an 8-bit or 16-bit operation;
   the original <ex_op> performs a 32-bit or 64-bit operation.
 */
static BOOL Check_loadbw_execute( int ld_bytes, OP* ex_op )
{
  if( !CG_loadbw_execute ||
      ld_bytes > 2       ||
      OP_opnds( ex_op ) != 2 )
    return FALSE;

  if( TN_size(OP_result(ex_op,0)) == ld_bytes )
    return TRUE;

  /* Check all the opnds of <ex_op> to make sure it is safe for
     <ex_op> to perform 8-bit or 16-bit operation.  (bug#131)
  */

  OP* ld_op[] = { NULL, NULL };

  for( int i = 0; i < OP_opnds( ex_op ); i++ ){
    TN* opnd = OP_opnd( ex_op, i );
    if( TN_is_register( opnd ) ){
      const EBO_TN_INFO* opnd_info = get_tn_info( opnd );
      if( opnd_info == NULL ||
	  opnd_info->in_op == NULL )
	return FALSE;

      OP* pred_op = opnd_info->in_op;

      if( OP_icmp(ex_op) &&
	  ( TOP_is_load_ext(OP_code(pred_op)) ||
	    TOP_is_move_ext(OP_code(pred_op)) ) ){
	/* If the opnd is coming from a zero-extension operation,
	   then don't consider using a shorter format for <ex_op>, because
	   we don't know the run-time msb value of this opnd, and we do not
	   have unsigned cmp. (bug#2197)
	 */
	struct SIZE_EXT_INFO pred_size_ext_info;
	Get_Size_Ext_Info( OP_code(pred_op), &pred_size_ext_info );
	if (!pred_size_ext_info.sign_ext) {
	  // If the consumer of rsp cares only about equality/inequality, then
	  // it is ok to use 8-bit cmp.
	  OP *op;
	  for (op = OP_next(ex_op); op != NULL; op = OP_next(op)) {
	    TOP top = OP_code(op);
	    if (OP_reads_rflags(op) &&
		top != TOP_je &&
		top != TOP_jne &&
		top != TOP_sete &&
		top != TOP_setne &&
		top != TOP_cmove &&
		top != TOP_cmovne) {
	      return FALSE;
	    }
	    if (TOP_is_change_rflags(top))
	      break;
	  }
	}
      }

      if( OP_load( pred_op ) ){
	if( CGTARG_Mem_Ref_Bytes( pred_op ) != ld_bytes )
	  return FALSE;
	ld_op[i] = pred_op;	
      }

    } else if( TN_has_value( opnd ) ){
      const INT64 value = TN_value( opnd );
      if( ( ld_bytes == 1 &&
	    !ISA_LC_Value_In_Class(value, LC_simm8) ) ||
	  ( ld_bytes == 2 &&
	    !ISA_LC_Value_In_Class(value, LC_simm16) ) )
	return FALSE;
    }
  }

  /* If both opnds are coming from load operations, make sure they have the same
     signness.
   */
  if( ld_op[0] != NULL &&
      ld_op[1] != NULL ){
    struct SIZE_EXT_INFO info0;
    struct SIZE_EXT_INFO info1;

    Get_Size_Ext_Info( OP_code(ld_op[0]), &info0 );
    Get_Size_Ext_Info( OP_code(ld_op[1]), &info1 );

    if( info0.sign_ext != info1.sign_ext )
      return FALSE;
  }

  return TRUE;
}


/*
 * If the cmp's load operand is not in the form that the 
 * load exec table can allow, morph it here, as it can only
 * support 1 type of mapping when this case there are 2.
 */
static TOP Fit_Cmp_By_Load_Usage( TOP cur_top, BOOL use_right )
{
  TOP new_top = cur_top;
 
  if( use_right ){
    switch(cur_top){
    case TOP_cmpx8:
      new_top = TOP_cmpxr8;
      break;
    case TOP_cmpxx8:
      new_top = TOP_cmpxxr8;
      break;
    case TOP_cmpxxx8:
      new_top = TOP_cmpxxxr8;
      break;
    case TOP_cmpx16:
      new_top = TOP_cmpxr16;
      break;
    case TOP_cmpxx16:
      new_top = TOP_cmpxxr16;
      break;
    case TOP_cmpxxx16:
      new_top = TOP_cmpxxxr16;
      break;
    case TOP_cmpx32:
      new_top = TOP_cmpxr32;
      break;
    case TOP_cmpxx32:
      new_top = TOP_cmpxxr32;
      break;
    case TOP_cmpxxx32:
      new_top = TOP_cmpxxxr32;
      break;
    case TOP_cmpx64:
      new_top = TOP_cmpxr64;
      break;
    case TOP_cmpxx64:
      new_top = TOP_cmpxxr64;
      break;
    case TOP_cmpxxx64:
      new_top = TOP_cmpxxxr64;
      break;
    default:
      break;
    }
  }

  return new_top;
}


// What is the load-execute instruction corresponding to 
// load "op" and execute "ex_op"
static TOP
Load_Execute_Format (OP *ld_op, OP *ex_op, ADDR_MODE mode)
{
  TOP new_top = OP_code( ex_op );
  const int ld_bytes = CGTARG_Mem_Ref_Bytes( ld_op );

  if( Check_loadbw_execute( ld_bytes, ex_op ) ){

    switch( OP_code(ex_op) ){
    case TOP_cmp32: 
      if( ld_bytes == 1 )
	new_top = TOP_cmp8;
      else if( ld_bytes == 2 )
	new_top = TOP_cmp16;
      break;

    case TOP_cmpi32:
      if( ld_bytes == 1 )
	new_top = TOP_cmpi8;
      else if( ld_bytes == 2 )
	new_top = TOP_cmpi16;
      break;
      
    case TOP_xor32:
      if( ld_bytes == 1 )
	new_top = TOP_xor8;
      else if( ld_bytes == 2 )
	new_top = TOP_xor16;
      break;

    case TOP_and32: 
      if( ld_bytes == 1 )
	new_top = TOP_and8;
      else if( ld_bytes == 2 )
	new_top = TOP_and16;
      break;
	
    case TOP_or32: 
      if( ld_bytes == 1 )
	new_top = TOP_or8;
      else if( ld_bytes == 2 )
	new_top = TOP_or16;
      break;
    }
  }

  new_top = Get_Top_For_Addr_Mode(new_top, mode);
  Is_True( new_top != TOP_UNDEFINED, ("Load_Execute_Format: NYI (1)") );
  OP* fake_new_op = Mk_OP( new_top, NULL, NULL, NULL, NULL, NULL, NULL );
  
  if( CGTARG_Mem_Ref_Bytes( fake_new_op ) > ld_bytes )
    new_top = TOP_UNDEFINED;

  /* Not all the registers are 8-bit addressable under i386 target.
   */
  if (Is_Target_32bit() &&
      EBO_in_peep       &&
      OP_opnd_size(fake_new_op, 0) == 8) {
    TN *opnd = OP_opnd(ex_op, 0);
    const REGISTER reg = TN_register(opnd);
    const REGISTER_SET regs =
      REGISTER_SUBCLASS_members(ISA_REGISTER_SUBCLASS_m32_8bit_regs);
    Is_True(TN_register_class(opnd) == ISA_REGISTER_CLASS_integer,
	    ("Load_Execute_Format: opnd not integer register class"));

    if (!REGISTER_SET_MemberP(regs, reg))
      new_top = TOP_UNDEFINED;
  }

  return new_top;
}


static BOOL test_is_replaced( OP* alu_op, OP* test_op, const EBO_TN_INFO* tninfo )
{
#if Is_True_On
  if (!(EBO_Opt_Mask & EBO_TEST_IS_REPLACED)) return FALSE;
#endif
  if( alu_op == NULL ||
      OP_bb( alu_op ) != OP_bb( test_op ) )
    return FALSE;

  OP* new_op = NULL;
  TOP top = OP_code( alu_op );

  if( !TOP_is_iadd( top ) &&
      !TOP_is_isub( top ) &&
      !TOP_is_ior( top )  &&
      !TOP_is_ixor( top ) &&
      !TOP_is_iand( top ) ){

    // The conversion from test -> cmpi was intended to facilitate address 
    // folding and bug 5517 has proved that it is detrimental to performance.
    if (CG_use_test)
      return FALSE;

    /* Change a cmp to test if the opnds are from a load, so that more folding
       could happen later.
    */
    if( OP_load( alu_op ) &&
	!TN_live_out_of( OP_result(alu_op,0), OP_bb(test_op) ) ){
      TOP cmpi_op;
      if( OP_code ( test_op ) == TOP_test64 )
        cmpi_op = TOP_cmpi64;
      else if( OP_code ( test_op ) == TOP_test32 )
        cmpi_op = TOP_cmpi32;
      else if( OP_code ( test_op ) == TOP_test16 )
        cmpi_op = TOP_cmpi16;
      else if( OP_code ( test_op ) == TOP_test8 )
        cmpi_op = TOP_cmpi8;
      new_op = Mk_OP( cmpi_op, Rflags_TN(), OP_opnd(test_op,0), Gen_Literal_TN( 0, 4) );

      Set_OP_unrolling( new_op, OP_unrolling(test_op) );
      Set_OP_orig_idx( new_op, OP_map_idx(test_op) );
      Set_OP_unroll_bb( new_op, OP_unroll_bb(test_op) );
      
      Copy_WN_For_Memory_OP( new_op, test_op );
      if ( OP_volatile( test_op ) )
        Set_OP_volatile( new_op );
      OP_srcpos( new_op ) = OP_srcpos( test_op );
      BB_Insert_Op_After( OP_bb(test_op), test_op, new_op );

      return TRUE;
    } 

    return FALSE;
  }

  /* Fix bug#963 and bug#1054
     A test is redundant only if the source is coming from an AND operation.
     TODO:
     Add TOP_js and TOP_jns so that more test or cmp operations can be removed.
   */
  if( !TOP_is_iand( top ) )
    return FALSE;

  if( !TOP_is_change_rflags(top) )
    return FALSE;

  /* TODO:
     Handle <alu_op> with memory operand.
   */
  if( OP_opnds( alu_op ) > 2 )
    return FALSE;

  const TN* src1 = OP_opnd( alu_op, 0 );
  const TN* src2 = OP_opnd( alu_op, 1 );

  bool test_is_redundant = false;

  /* Make sure the operands are not re-defined after <alu_op>.
   */
  EBO_TN_INFO* src_info = get_tn_info( src1 );
  if( src_info != NULL ){
    if( src_info->sequence_num > tninfo->sequence_num )
      return FALSE;

    if( EBO_in_loop )
      return FALSE;

    OP* next = OP_next( alu_op );
    while( next != test_op ){
      const TOP top = OP_code(next);
      if( TOP_is_change_rflags( top ) )
	break;
      next = OP_next( next );
    }
 
    test_is_redundant = next == test_op;

    /* Don't pass case like
         a = a - 1
	 test a, a
       to later phase.
    */
    if( src_info->sequence_num == tninfo->sequence_num )
      return test_is_redundant;
  }

  if( TN_is_register( src2 ) ){
    src_info = get_tn_info( src2 );
    if( src_info != NULL && src_info->sequence_num >= tninfo->sequence_num ){
      return test_is_redundant;
    }
  }

  /* Change <test_op> according to different <alu_op>. */
  top = OP_code(test_op);

  if( OP_iand( alu_op ) ){
    if( TN_has_value(src2) ){
      new_op = Mk_OP( top == TOP_test64 ? TOP_testi64 : TOP_testi32,
		      Rflags_TN(), src1, src2 );
    } else
      new_op = Mk_OP( top, Rflags_TN(), src1, src2 );

  } else if( OP_iadd( alu_op ) ){
    if( TN_has_value(src2) &&
	TN_value(src2) < 0 ){
      new_op = Mk_OP( top == TOP_test64 ? TOP_cmpi64 : TOP_cmpi32,
		      Rflags_TN(), src1, Gen_Literal_TN( -TN_value(src2), 4) );
    }

  } else if( OP_isub( alu_op ) ){
    Is_True( TN_is_register(src2), ("test_is_replaced: NYI (1)") );
    new_op = Mk_OP( top == TOP_test64 ? TOP_cmp64 : TOP_cmp32,
		    Rflags_TN(), src1, src2 );
  }

  /* Generate new operation to replace <test_op>. */

  if( new_op != NULL ){
    Set_OP_unrolling( new_op, OP_unrolling(test_op) );
    Set_OP_orig_idx( new_op, OP_map_idx(test_op) );
    Set_OP_unroll_bb( new_op, OP_unroll_bb(test_op) );
      
    Copy_WN_For_Memory_OP( new_op, test_op );
    if ( OP_volatile( test_op ) )
      Set_OP_volatile( new_op );
    OP_srcpos( new_op ) = OP_srcpos( test_op );
    BB_Insert_Op_After( OP_bb(test_op), test_op, new_op );
      
    if( EBO_Trace_Data_Flow ){
#pragma mips_frequency_hint NEVER
      fprintf( TFile, "Special_Sequence replaces " );
      Print_OP_No_SrcLine( test_op );
      fprintf( TFile, "                   with   " );
      Print_OP_No_SrcLine( new_op );
    }

    return TRUE;
  }

  return test_is_redundant;
}


/* A folded load op and an alu op could across <store_op> that will alias
   with the load op. Thus, we need to update the op_must_not_be_moved field
   for all the load ops preceeding <store_op>.
*/
void Update_op_must_not_be_moved( OP* store_op, EBO_TN_INFO** opnd_tninfo )
{
  if( Alias_Manager == NULL )
    return;

  if( OP_has_implicit_interactions(store_op) || OP_unalign_mem(store_op) )
    return;

  Is_True( OP_store(store_op),
	   ("Update_op_must_not_be_moved: must be a store operation") );

  const INT hash_value = EBO_hash_op( store_op, opnd_tninfo );
  EBO_OP_INFO* opinfo = EBO_opinfo_table[hash_value];

  while( opinfo != NULL ){
    OP* load_op = opinfo->in_op;
    if( !opinfo->op_must_not_be_moved &&
	load_op != NULL &&
	OP_load( load_op ) ){
      WN* pred_wn = OP_hoisted(load_op)  ? NULL : Get_WN_From_Memory_OP(load_op);
      WN* succ_wn = OP_hoisted(store_op) ? NULL : Get_WN_From_Memory_OP(store_op);

      if( (pred_wn != NULL) && (succ_wn != NULL) ){
	const ALIAS_RESULT result = Aliased( Alias_Manager, pred_wn, succ_wn );
	if( result == POSSIBLY_ALIASED || result == SAME_LOCATION ){
	  opinfo->op_must_not_be_moved = TRUE;	  
	}
      }
    }

    opinfo = opinfo->same;    
  }
}

static hTN_MAP32 _load_exec_map = NULL;


/* Limitations of load_exec module:
   (1) Cannot handle loads which across bbs, given the way that ebo works;
   (2) Cannot keep track of TNs which are defined more than once, since loads
       can be removed and then created by other optimizations.
*/
void Init_Load_Exec_Map( BB* bb, MEM_POOL* pool )
{
  _load_exec_map = hTN_MAP32_Create( pool );
  // Map each register to the TN that was last defined into the register.
  TN *last_TN[ISA_REGISTER_CLASS_MAX+1][REGISTER_MAX+1];

  if (EBO_in_peep) {
    memset(last_TN, 0,
	   sizeof(TN*) * (ISA_REGISTER_CLASS_MAX + 1) * (REGISTER_MAX + 1));
  }

  OP* op = NULL;
  FOR_ALL_BB_OPs_FWD( bb, op ){
    if( OP_load( op ) ){
      TN* tn = OP_result( op, 0 );

      // If a load op writes to <tn> which is live out of <bb>, then there is
      // no need to perform load_exe on this load op.  (Before register
      // allocation case.)
      if (!EBO_in_peep &&
	  TN_live_out_of(tn, bb)) {
	hTN_MAP32_Set( _load_exec_map, tn, CG_load_execute + 2 );

      } else {
	const INT32 uses = hTN_MAP32_Get( _load_exec_map, tn ) - 1;
	// If TN is loaded more than once in BB, and if an earlier load has
	// more than CG_load_execute number of uses, then don't reset the
	// number of uses.  This will disable load-execution for all loads of
	// the TN.  (A limitation of the current code is that it cannot track
	// separate loads of the same TN separately.)
	if( uses  <= CG_load_execute ){
	  hTN_MAP32_Set( _load_exec_map, tn, 1 );
	}
      }
    }

    for( int i = 0; i < OP_opnds(op); i++ ){
      TN* opnd = OP_opnd( op, i );
      if( TN_is_register( opnd ) ){
	const INT32 uses = hTN_MAP32_Get( _load_exec_map, opnd );
	if( uses > 0 ){
	  hTN_MAP32_Set( _load_exec_map, opnd, uses+1 );
	}
      }
    }

    // Record which registers are used by the OP's result TNs.
    if (EBO_in_peep) {
      for (int i = 0; i < OP_results(op); i++) {
	TN *tn = OP_result(op, i);
	last_TN[TN_register_class(tn)][TN_register(tn)] = tn;
      }
    }
  }

  // Disable load-execution for live-out TNs.  (After register allocation
  // case.)
  if (EBO_in_peep) {
    ISA_REGISTER_CLASS cl;
    FOR_ALL_ISA_REGISTER_CLASS(cl) {
      for (REGISTER reg = 0; reg <= REGISTER_MAX; reg++) {
	if (last_TN[cl][reg] &&
	    (EBO_no_liveness_info_available ||
	     REG_LIVE_Outof_BB(cl, reg, bb))) {
	  hTN_MAP32_Set(_load_exec_map, last_TN[cl][reg], CG_load_execute + 2);
	}
      }
    }
  }
}


BOOL EBO_Not_Load_Exec_Opnd( OP* ld_op )
{
  const INT32 load_uses = hTN_MAP32_Get( _load_exec_map, 
                                         OP_result(ld_op,0) ) - 1;
  return ( load_uses > CG_load_execute );
}


// Convert movss and movsd to moco-buffer forms that allow the mov src
// operand to have a micro-op optimization form in the pipeine.  Also
// attempt to schedule the compute-to operation closer to the canonical
// mov form.
void EBO_SIMD_Compute_To( OP* simd_op )
{
  BB *bb = OP_bb(simd_op);
  OP *anti_op = NULL;
  ARC_LIST *true_arcs, *arcs;
  ARC_LIST *anti_arcs;
  BOOL hoist_only_cand = FALSE;
  BOOL convert_cand = FALSE;
  BOOL found_anti_dep = FALSE;
  BOOL cand_ok = FALSE;
  BOOL found_dep = FALSE;
  TN *tnr = OP_results(simd_op) ? OP_result(simd_op, 0) : NULL;
  int i;
  int num_ops;

  // check for xmm/mmx register class
  if (tnr && TN_is_register(tnr)) {
    if (TN_register_class(tnr) != ISA_REGISTER_CLASS_float)
      return;

    if (OP_load(simd_op))
      return;

    if (!TOP_is_move(OP_code(simd_op)))
      return;

    if ((TN_size(tnr) != 8) && (TN_size(tnr) != 16))
      return;

    // Map all the mov candidates, and also allow
    // translations on movss/movsd to movaps/movapd.
    switch (OP_code(simd_op)) {
    case TOP_movaps:
    case TOP_movapd:
    case TOP_movdq:
    case TOP_vmovaps:
    case TOP_vmovapd:
    case TOP_vmovdqa:
      hoist_only_cand = TRUE;
      break;
    case TOP_movss:
    case TOP_movsd:
    case TOP_vmovss:
    case TOP_vmovsd:
      convert_cand = TRUE;
      break;
    default:
      return;
    }
  } else {
    return;
  }

  // First check to see if the register allocator produced
  // an identity copy which will be elimated later.
  for (i = 0; i < OP_opnds(simd_op); i++) {
    TN *src_tn = OP_opnd(simd_op, i);
    if (TN_is_register(src_tn)) {
      if (TN_register(tnr) == TN_register(src_tn))
        return;
    }
  }

  // check for anti-deps on the simd_op's src operands
  for (anti_arcs = OP_succs(simd_op); 
       anti_arcs != NULL; anti_arcs = ARC_LIST_rest(anti_arcs)) {
    ARC *anti_arc = ARC_LIST_first(anti_arcs);
    OP *succ_op = ARC_succ(anti_arc);
    if (ARC_kind(anti_arc) != CG_DEP_REGANTI) continue;

    // Iff the src only operand of the simd_op has an anti arc
    // note it, we will check to see if the operands def occurs
    // between the flow dependent edges we will attempt to 
    // convert for, as the moco buffer will be invalidated.
    TN *src_tn = OP_opnd(simd_op, ARC_opnd(anti_arc));
    if (src_tn &&
        (TN_register_class(src_tn) == ISA_REGISTER_CLASS_float)  &&
        (src_tn != tnr)) {
      anti_op = succ_op;
      break;
    }
  }

  // See if there are uses of this simd_op's src which occur 
  // between use_op and the simd_op, since we know its a move. 
  for (true_arcs = OP_succs(simd_op); 
       true_arcs != NULL; true_arcs = ARC_LIST_rest(true_arcs)) {
    ARC *true_arc = ARC_LIST_first(true_arcs);
    OP *succ_op = ARC_succ(true_arc);
    if (ARC_kind(true_arc) != CG_DEP_REGIN) continue;

    // no move - move opt
    if (TOP_is_move( OP_code(succ_op)) ) continue;

    if (OP_flop(succ_op)) {
      OP *cur_op, *first_op;
      BOOL first_time = TRUE;

      first_op = succ_op;
      num_ops = 0;

      // If we are on in a loop, check the position of the 
      // succ_op relative to the simd_op
      if (BB_loop_head_bb(bb)) {
        // Skip loop carried successor arcs
        if (OP_Precedes(succ_op, simd_op))
          continue;
      }

      // Find the closest suitable op to succ_op if there are flow 
      // dependencies.
      cur_op = NULL;
      if (OP_next(simd_op) != succ_op) {
        for (arcs = OP_preds(succ_op); 
             arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
          ARC *arc = ARC_LIST_first(arcs);
          if (ARC_pred(arc) == simd_op) continue;
          if (OP_Precedes(simd_op, ARC_pred(arc))) {
            cur_op = ARC_pred(arc);
            if ((OP_Precedes(first_op, cur_op) || first_time) &&
                OP_Precedes(cur_op, succ_op)) {
              if (OP_next(cur_op) != succ_op) {
                first_op = cur_op;
                first_time = FALSE;
              } else {
                // dep arc prevents motion
                first_op = succ_op;
                break;
              }
            }
          }
        }
      }

      // Now evaluate the results
      if (first_op != succ_op) { 
        if (OP_load(first_op)) {
          hoist_only_cand = convert_cand = FALSE; 
        } else if (anti_op && (OP_Precedes(anti_op, first_op))) {
          hoist_only_cand = convert_cand = FALSE; 
        } else {
          found_dep = TRUE;
          first_op = OP_next(first_op);
        }
      } else if (cur_op == NULL) {
        // If there are no dependencies but there are instructions in
        // between, allow motioning up next to the simd_op.
        if (OP_next(simd_op) != succ_op) {
          first_op = OP_next(simd_op);
        }
      }

      // See if the new hoist position is inside the latency 
      // shadow of a conversion candidate.
      if ((convert_cand || hoist_only_cand) && 
          (first_op != succ_op)) {
        for (cur_op = OP_next(simd_op); 
             cur_op != succ_op; cur_op = OP_next(cur_op)) {
          num_ops++;
          if (cur_op == first_op)
            break;
        }
      }

      // We want to gate a conversion and hoisting based on if we can 
      // move the op into the latency shadow.
      if ((convert_cand || hoist_only_cand) && 
          (num_ops > 0) &&
          (ARC_latency(true_arc) < num_ops)) {
        hoist_only_cand = convert_cand = FALSE;
      }

      // Apply hoist-ability here
      if (first_op != succ_op) {
        // Now do the hoist if we can
        if (convert_cand || hoist_only_cand) {
          cand_ok = TRUE;
          if (EBO_Trace_Data_Flow) {
            if (found_dep) {
              fprintf( TFile, "dep(conv=%s): hoist to succ compute-to\n",
                       convert_cand ? "true" : "false");
            } else {
              fprintf( TFile, "clean(conv=%s): hoist to succ compute-to\n",
                       convert_cand ? "true" : "false");
            }
          }

          BB_Move_Op_Before(bb, first_op, bb, succ_op);
          break;
        }
      } else if (convert_cand) {
        // Allow conversion if they are right next to each other
        if (EBO_Trace_Data_Flow) {
          fprintf( TFile, "convert(no hoist) for succ compute-to\n");
        }
        cand_ok = TRUE;
      }
    }
  }

  // Convert the candidate if we passed all tests and the op
  // is not yet in the cannonical form.
  if (convert_cand && cand_ok) {
    // we have a single dependence arc on this def, we can convert
    if (OP_code(simd_op) == TOP_movsd) {
      OP_Change_Opcode( simd_op, TOP_movaps );
    } else if (OP_code(simd_op) == TOP_movss) {
      OP_Change_Opcode( simd_op, TOP_movaps );
    }
    if (OP_code(simd_op) == TOP_vmovsd) {
      OP_Change_Opcode( simd_op, TOP_vmovapd );
    } else if (OP_code(simd_op) == TOP_vmovss) {
      OP_Change_Opcode( simd_op, TOP_vmovaps );
    }
  }
}


// Do basic block level compute-to opts
void EBO_Compute_To ( BB* bb )
{
  OP *op;

  // We need to do this step only once for the bb, as the code
  // is static, we will only change opcodes and move ops, no ops are
  // deleted or inserted, ergo, the DDG is static.
  CG_DEP_Compute_Graph ( bb,
                         INCLUDE_ASSIGNED_REG_DEPS,
                         NON_CYCLIC,
                         NO_MEMREAD_ARCS,
                         INCLUDE_MEMIN_ARCS,
                         NO_CONTROL_ARCS,
                         NULL);

  FOR_ALL_BB_OPs (bb, op) {
    if (OP_flop(op))
      EBO_SIMD_Compute_To(op);
  }

  CG_DEP_Delete_Graph (bb);
}


// Attempt to fold a constant onto the index calcuation of an array
BOOL EBO_Fold_Lea_Const_Component( OP* mem_op )
{
  OP *const_op, *pred_op;
  BB *bb = OP_bb(mem_op);
  ARC_LIST *arcs;
  TN *lea_tnr;
  int i;

  // Identify the lea tns.
  int base_loc = OP_find_opnd_use( mem_op, OU_base );
  TN *base_tn = base_loc >= 0 ?  OP_opnd( mem_op, base_loc ) : NULL;
  int index_loc = OP_find_opnd_use( mem_op, OU_index );
  TN *index_tn = index_loc >= 0 ?  OP_opnd( mem_op, index_loc ) : NULL;
  int scale_loc = OP_find_opnd_use( mem_op, OU_scale );
  TN *scale_tn = scale_loc >= 0 ?  OP_opnd( mem_op, scale_loc ) : NULL;
  int offset_loc = OP_find_opnd_use( mem_op, OU_offset );
  TN *offset_tn = offset_loc >= 0 ?  OP_opnd( mem_op, offset_loc ) : NULL;

  if ((base_tn == NULL) ||
      (index_tn == NULL) ||
      (scale_tn == NULL) ||
      (offset_tn == NULL))
    return FALSE;

  if (!TN_is_constant(offset_tn)) return FALSE;
  if (TN_value(offset_tn) != 0) return FALSE;
  if (TN_size(index_tn) != 4) return FALSE;

  // for this bb, obtain the dependence graph so that we can
  // walk this mem_op's expression tree.
  CG_DEP_Compute_Graph ( bb,
                         INCLUDE_ASSIGNED_REG_DEPS,
                         NON_CYCLIC,
                         NO_MEMREAD_ARCS,
                         NO_MEMIN_ARCS,
                         NO_CONTROL_ARCS,
                         NULL);

  // locate the ldc32(const_op) and evaluate it
  const_op = NULL;
  for (arcs = OP_preds(mem_op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
    ARC *arc = ARC_LIST_first(arcs);
    OP *pred_op = ARC_pred(arc);
    if (OP_code(pred_op) == TOP_ldc32) {
      TN *tnr = OP_result(pred_op, 0);
      if (TN_is_register(tnr) && !TN_is_global_reg(tnr)) {
        if (TN_register(tnr) == TN_register(base_tn)) {
          const_op = pred_op;
          break;
        }
      }
    }
  }

  // If we fail to match any part of the expression, we are done
  if (const_op == NULL) return FALSE;

  // A non global ldc32 can have but 1 use to transform this expression.
  int num_ldc_uses = 0;
  for (arcs = OP_succs(const_op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
    ARC *arc = ARC_LIST_first(arcs);
    if (ARC_kind(arc) != CG_DEP_REGIN) continue;
    OP *succ_op = ARC_succ(arc);
    for (i = 0; i < OP_opnds(succ_op); i++) {
      if (OP_opnd(succ_op, i) == base_tn)
        num_ldc_uses++;
    }
  }

  // Nothing to do, the constant cannot be folded.
  if (num_ldc_uses > 1) return FALSE;

  // Now fold the constant from the ldc32 onto the lea as an offset field.
  TN *const_tn = OP_opnd(const_op, 0);
  lea_tnr = OP_result(mem_op, 0);
  OP *new_op = Mk_OP(TOP_leaxx32, 
                     lea_tnr, 
                     index_tn,
                     scale_tn,
                     const_tn);
  Set_OP_unrolling( new_op, OP_unrolling(mem_op) );
  Set_OP_orig_idx( new_op, OP_map_idx(mem_op) );
  Set_OP_unroll_bb( new_op, OP_unroll_bb(mem_op) );
  OP_srcpos( new_op ) = OP_srcpos( mem_op );
  BB_Insert_Op_After( bb, mem_op, new_op );
  OP_Change_To_Noop( const_op );

  CG_DEP_Delete_Graph (bb);

  return TRUE;
}


// Attempt scalar replacement of const init array elements with ldc32 to
// to register, this only within the interation.
BOOL EBO_Opt_Const_Array( OP* mem_op,
                          LOOP_DESCR* loop,
                          INT loop_iter_size )
{
  LOOPINFO *info = loop->loopinfo;
  WN *loop_info = LOOPINFO_wn(info);
  WN *loop_indvar =  WN_loop_induction(loop_info);
  WN *load_wn = Get_WN_From_Memory_OP(mem_op);
  BB *bb = OP_bb(mem_op);
  POINTS_TO *load_data;
  BOOL ret_val = FALSE;
  int value;
  int unroll_iter = OP_unrolling(mem_op);

  // For now only do up counting loops
  if (WN_Loop_Up_Trip(loop_info) == FALSE)
    return ret_val;

  if ((loop_indvar == NULL) || (load_wn == NULL))
    return ret_val;

  if (Alias_Manager && Valid_alias(Alias_Manager, load_wn)) {
    load_data = Points_to(Alias_Manager, load_wn);
  } else {
    return ret_val;
  }

  if (load_data && load_data->Base_is_fixed()) {
    ST *load_sym = load_data->Base();

    // we can only proceed when processing vars
    if (ST_class(load_sym) != CLASS_VAR)
      return ret_val;

    // only handle some types of storage
    switch (ST_sclass(load_sym)) {
    case SCLASS_AUTO:
    case SCLASS_PSTATIC:
    case SCLASS_FSTATIC:
    case SCLASS_EXTERN:
      break;
    default:
      return ret_val;
    }

    TY_IDX sym_type = ST_type(load_sym);
    if (TY_kind(sym_type) == KIND_ARRAY) {
      TY_IDX ty_ele = TY_etype(sym_type);
      if(TY_kind(ty_ele) == KIND_SCALAR) {
        OPCODE opcode = WN_opcode(load_wn);
        OPERATOR oper = OPCODE_operator(opcode); 

        if ((oper == OPR_ILOAD) &&
            (WN_operator(WN_kid0(load_wn)) == OPR_LDID)) {
          WN *index_var = WN_kid0(load_wn);
          char *index_name = ST_name(WN_st(index_var));
          char *indvar_name = ST_name(WN_st(loop_indvar));
          if (index_name == indvar_name) {
            int element_size = TY_mtype (ty_ele);
            // See if we have a const array or not, if so
            // and since our index is the loop induction var, we can 
            // try to obtain the initialized data to attempt
            // an unrolled replacement of the data items as immediates
            // via ldc32 moves instead of via memory.
            if ((TY_is_const(ty_ele)) && 
                (TY_AR_ndims(sym_type) == 1) &&
                (element_size == 4)) {
              int array_size = TY_size(sym_type);
              // Because of this constraint, we do not need to examine
              // the initial value of the induction variable, but 
              // we can only do this on up counted loops.
              if ((array_size / element_size) == loop_iter_size) {
                TCON *init_array = new TCON[loop_iter_size];
                INITO_IDX inito = Find_INITO_For_Symbol(load_sym);
                INITO ino = Inito_Table[inito];
                INITV_IDX idx;
                INT32 i,j;
                BOOL no_val = FALSE;

                // fetch the contents of the initalized const array.
                if (INITO_val(ino) != (INITO_IDX) NULL) {
                  FOREACH_INITV (INITO_val(ino), idx) {
                    INITV_IDX ninv;
                    INITV inv = Initv_Table[idx];
                    j = 0;
                    if (INITV_kind(inv) != INITVKIND_BLOCK) continue;
                    for (i = 0; i < INITV_repeat1(inv); i++) {
                      for (ninv = INITV_blk(inv); ninv; 
                           ninv = INITV_next(ninv)) {
                        INITV inv_ele = Initv_Table[ninv];
                        TCON tcon;
                        switch ( INITV_kind(inv_ele) ) {
                        case INITVKIND_ZERO:
                          tcon = Host_To_Targ (INITV_mtype (inv_ele), 0);
                          break;
                        case INITVKIND_ONE:
                          tcon = Host_To_Targ (INITV_mtype (inv_ele), 1);
                          break;
                        case INITVKIND_VAL:
                          tcon = INITV_tc_val(inv_ele);
                          break;
                        default:
                          no_val = TRUE;
                        }
                        init_array[j++] = tcon;
                      }
                    }
                  }
                }

                // If we ever trip over an element we cannot dereference, we 
                // fail.
                if (no_val)
                  return ret_val;

                // Now begin processing the scalar replacement candidate
                value = TCON_ival(init_array[unroll_iter]);
                if (ISA_LC_Value_In_Class( value, LC_simm32 )) {
                  TN *tnr = NULL;
                  OP *new_op;
                  // There will only be the 1 result on this load anyways.
                  for (i = 0; i < OP_results(mem_op); i++) {
                    tnr = OP_result(mem_op, i);
                  }
                  // This step happens before register allocation
                  if (tnr) {
                    new_op = Mk_OP(TOP_ldc32, tnr, Gen_Literal_TN(value, 4));
                    Set_OP_unrolling( new_op, OP_unrolling(mem_op) );
                    Set_OP_orig_idx( new_op, OP_map_idx(mem_op) );
                    Set_OP_unroll_bb( new_op, OP_unroll_bb(mem_op) );
                    OP_srcpos( new_op ) = OP_srcpos( mem_op );
                    BB_Insert_Op_After( bb, mem_op, new_op );
                    ret_val = TRUE;
                  }
                } 
                delete init_array;
              }
            }
          }
        }
      }
    }
  }

  return ret_val;
}


BOOL EBO_Process_SSE5_Load_Exectute_FMA_p1(TOP new_top, 
                                           ADDR_MODE mode, 
                                           TN* base, 
                                           TN* scale, 
                                           TN* index, 
                                           TN* offset, 
                                           TN* result,
                                           OP* ld_op,
                                           OP* alu_op,
                                           EBO_TN_INFO** actual_tninfo)
{
  OP* new_op = NULL;
  EBO_TN_INFO* tninfo;
  TN *opnd0;
  TN *opnd1;
  BB* bb = OP_bb( alu_op );

  // add load form: both mul opnds persist
  tninfo = actual_tninfo[0];
  opnd0 = tninfo->local_tn;
  tninfo = actual_tninfo[1];
  opnd1 = tninfo->local_tn;

  if( mode == BASE_MODE ){
    // base + offset
    new_op = Mk_OP( new_top, result, opnd0, opnd1, base, offset );
  } else if( mode == BASE_INDEX_MODE ){
    // offset + base + index * scale
    new_op = Mk_OP( new_top, result, opnd0, opnd1, base, index, scale, offset );
  } else {
    // offset + index * scale
    new_op = Mk_OP( new_top, result, opnd0, opnd1, index, scale, offset );
  }

  Is_True( !EBO_in_loop, ("EBO_Process_SSE5_Load_Exectute_FMA_p1: NYI (1)") );

  Set_OP_unrolling( new_op, OP_unrolling(alu_op) );
  Set_OP_orig_idx( new_op, OP_map_idx(alu_op) );
  Set_OP_unroll_bb( new_op, OP_unroll_bb(alu_op) );

  Copy_WN_For_Memory_OP( new_op, ld_op );
  if (OP_volatile(ld_op)) {
    Reset_OP_volatile(ld_op);	// allow OP to be deleted
    Set_OP_volatile(new_op);
  }
  OP_srcpos( new_op ) = OP_srcpos( alu_op );
  BB_Insert_Op_After( bb, alu_op, new_op );

  // If folding a restore of a spilled value, mark the spill store as needed
  // even if all the restores are deleted.
  ST *spill_loc = CGSPILL_OP_Spill_Location(ld_op);
  if (spill_loc != (ST *)0) {		// It's a spill OP.
    SPILL_SYM_INFO &info = CGSPILL_Get_Spill_Sym_Info(spill_loc);
    info.Set_Used_By_Load_Exe();
  }

  if( EBO_Trace_Data_Flow ){
    #pragma mips_frequency_hint NEVER
    fprintf( TFile, "EBO_Process_SSE5_Load_Exectute_FMA_p1 merges " );
    Print_OP_No_SrcLine( ld_op );
    fprintf( TFile, "                   with   " );
    Print_OP_No_SrcLine( alu_op );

    fprintf( TFile, "                   new op " );
    Print_OP_No_SrcLine( new_op );
  }

  return TRUE;
}


BOOL EBO_Process_SSE5_Load_Exectute_FMA_p2(TOP new_top, 
                                           ADDR_MODE mode, 
                                           TN* base, 
                                           TN* scale, 
                                           TN* index, 
                                           TN* offset, 
                                           TN* result,
                                           OP* ld_op,
                                           OP* alu_op,
                                           EBO_TN_INFO** actual_tninfo)
{
  OP* new_op = NULL;
  EBO_TN_INFO* tninfo;
  TN *opnd0;
  TN *opnd2;
  BB* bb = OP_bb( alu_op );

  // mul load form: the opnd0(the other mul opnd) and opnd2(add opnd) persist
  tninfo = actual_tninfo[0];
  opnd0 = tninfo->local_tn;
  tninfo = actual_tninfo[2];
  opnd2 = tninfo->local_tn;

  if( mode == BASE_MODE ){
    // base + offset
    new_op = Mk_OP( new_top, result, opnd0, 
                    base, offset, opnd2 );
  } else if( mode == BASE_INDEX_MODE ){
    // offset + base + index * scale
    new_op = Mk_OP( new_top, result, opnd0, base, 
                    index, scale, offset, opnd2 );
  } else {
    // offset + index * scale
    new_op = Mk_OP( new_top, result, opnd0, 
                    index, scale, offset, opnd2 );
  }

  Is_True( !EBO_in_loop, ("EBO_Process_SSE5_Load_Exectute_FMA_p1: NYI (1)") );

  Set_OP_unrolling( new_op, OP_unrolling(alu_op) );
  Set_OP_orig_idx( new_op, OP_map_idx(alu_op) );
  Set_OP_unroll_bb( new_op, OP_unroll_bb(alu_op) );

  Copy_WN_For_Memory_OP( new_op, ld_op );
  if (OP_volatile(ld_op)) {
    Reset_OP_volatile(ld_op);	// allow OP to be deleted
    Set_OP_volatile(new_op);
  }
  OP_srcpos( new_op ) = OP_srcpos( alu_op );
  BB_Insert_Op_After( bb, alu_op, new_op );

  // If folding a restore of a spilled value, mark the spill store as needed
  // even if all the restores are deleted.
  ST *spill_loc = CGSPILL_OP_Spill_Location(ld_op);
  if (spill_loc != (ST *)0) {		// It's a spill OP.
    SPILL_SYM_INFO &info = CGSPILL_Get_Spill_Sym_Info(spill_loc);
    info.Set_Used_By_Load_Exe();
  }

  if( EBO_Trace_Data_Flow ){
    #pragma mips_frequency_hint NEVER
    fprintf( TFile, "EBO_Process_SSE5_Load_Exectute_FMA_p1 merges " );
    Print_OP_No_SrcLine( ld_op );
    fprintf( TFile, "                   with   " );
    Print_OP_No_SrcLine( alu_op );

    fprintf( TFile, "                   new op " );
    Print_OP_No_SrcLine( new_op );
  }

  return TRUE;
}


TOP EBO_Reset_Top(TOP top, ADDR_MODE mode)
{
  Addr_Mode_Group *group = Top_To_Addr_Mode_Group[top+4];
  if (group != NULL) {
    switch (mode) {
      case BASE_MODE:           return group->base_mode;
      case BASE_INDEX_MODE:     return group->base_index_mode;
      case INDEX_MODE:          return group->index_mode;
      case N32_MODE:            return group->n32_mode;
    }
    FmtAssert(FALSE, ("EBO_Reset_Top: address mode not handled"));
  }
  return TOP_UNDEFINED;
}


BOOL EBO_Process_SSE5_Load_Execute(TOP new_top, 
                                   ADDR_MODE mode, 
                                   int idx,
                                   TN* base, 
                                   TN* scale, 
                                   TN* index, 
                                   TN* offset, 
                                   TN* result,
                                   OP* ld_op,
                                   OP* alu_op,
                                   EBO_TN_INFO** actual_tninfo)
{
  // use the mul memopnd form if the add form is not present
  if ((idx != 2) && EBO_Is_FMA4(OP_code(alu_op))) {
    Addr_Mode_Group *group = Top_To_Addr_Mode_Group[new_top];
    new_top = EBO_Reset_Top(group->base_mode, mode);
  }

  switch (new_top) {

  // pattern 1 - add src is memop
  case TOP_vfmaddxss:
  case TOP_vfmaddxxss:
  case TOP_vfmaddxxxss:
  case TOP_vfmaddxsd:
  case TOP_vfmaddxxsd:
  case TOP_vfmaddxxxsd:
  case TOP_vfnmaddxss:
  case TOP_vfnmaddxxss:
  case TOP_vfnmaddxxxss:
  case TOP_vfnmaddxsd:
  case TOP_vfnmaddxxsd:
  case TOP_vfnmaddxxxsd:
  case TOP_vfmaddxps:
  case TOP_vfmaddxxps:
  case TOP_vfmaddxxxps:
  case TOP_vfmaddxpd:
  case TOP_vfmaddxxpd:
  case TOP_vfmaddxxxpd:
  case TOP_vfmaddsubxps:
  case TOP_vfmaddsubxxps:
  case TOP_vfmaddsubxxxps:
  case TOP_vfmaddsubxpd:
  case TOP_vfmaddsubxxpd:
  case TOP_vfmaddsubxxxpd:
  case TOP_vfnmaddxps:
  case TOP_vfnmaddxxps:
  case TOP_vfnmaddxxxps:
  case TOP_vfnmaddxpd:
  case TOP_vfnmaddxxpd:
  case TOP_vfnmaddxxxpd:
  case TOP_vfmsubxss:
  case TOP_vfmsubxxss:
  case TOP_vfmsubxxxss:
  case TOP_vfmsubxsd:
  case TOP_vfmsubxxsd:
  case TOP_vfmsubxxxsd:
  case TOP_vfnmsubxss:
  case TOP_vfnmsubxxss:
  case TOP_vfnmsubxxxss:
  case TOP_vfnmsubxsd:
  case TOP_vfnmsubxxsd:
  case TOP_vfnmsubxxxsd:
  case TOP_vfmsubxps:
  case TOP_vfmsubxxps:
  case TOP_vfmsubxxxps:
  case TOP_vfmsubxpd:
  case TOP_vfmsubxxpd:
  case TOP_vfmsubxxxpd:
  case TOP_vfmsubaddxps:
  case TOP_vfmsubaddxxps:
  case TOP_vfmsubaddxxxps:
  case TOP_vfmsubaddxpd:
  case TOP_vfmsubaddxxpd:
  case TOP_vfmsubaddxxxpd:
  case TOP_vfnmsubxps:
  case TOP_vfnmsubxxps:
  case TOP_vfnmsubxxxps:
  case TOP_vfnmsubxpd:
  case TOP_vfnmsubxxpd:
  case TOP_vfnmsubxxxpd:
  // FMA3: form1
  case TOP_xfmadd132xss:
  case TOP_xfmadd132xxss:
  case TOP_xfmadd132xxxss:
  case TOP_xfmadd132xsd:
  case TOP_xfmadd132xxsd:
  case TOP_xfmadd132xxxsd:
  case TOP_xfnmadd132xss:
  case TOP_xfnmadd132xxss:
  case TOP_xfnmadd132xxxss:
  case TOP_xfnmadd132xsd:
  case TOP_xfnmadd132xxsd:
  case TOP_xfnmadd132xxxsd:
  case TOP_xfmadd132xps:
  case TOP_xfmadd132xxps:
  case TOP_xfmadd132xxxps:
  case TOP_xfmadd132xpd:
  case TOP_xfmadd132xxpd:
  case TOP_xfmadd132xxxpd:
  case TOP_xfmaddsub132xps:
  case TOP_xfmaddsub132xxps:
  case TOP_xfmaddsub132xxxps:
  case TOP_xfmaddsub132xpd:
  case TOP_xfmaddsub132xxpd:
  case TOP_xfmaddsub132xxxpd:
  case TOP_xfnmadd132xps:
  case TOP_xfnmadd132xxps:
  case TOP_xfnmadd132xxxps:
  case TOP_xfnmadd132xpd:
  case TOP_xfnmadd132xxpd:
  case TOP_xfnmadd132xxxpd:
  case TOP_xfmsub132xss:
  case TOP_xfmsub132xxss:
  case TOP_xfmsub132xxxss:
  case TOP_xfmsub132xsd:
  case TOP_xfmsub132xxsd:
  case TOP_xfmsub132xxxsd:
  case TOP_xfnmsub132xss:
  case TOP_xfnmsub132xxss:
  case TOP_xfnmsub132xxxss:
  case TOP_xfnmsub132xsd:
  case TOP_xfnmsub132xxsd:
  case TOP_xfnmsub132xxxsd:
  case TOP_xfmsub132xps:
  case TOP_xfmsub132xxps:
  case TOP_xfmsub132xxxps:
  case TOP_xfmsub132xpd:
  case TOP_xfmsub132xxpd:
  case TOP_xfmsub132xxxpd:
  case TOP_xfmsubadd132xps:
  case TOP_xfmsubadd132xxps:
  case TOP_xfmsubadd132xxxps:
  case TOP_xfmsubadd132xpd:
  case TOP_xfmsubadd132xxpd:
  case TOP_xfmsubadd132xxxpd:
  case TOP_xfnmsub132xps:
  case TOP_xfnmsub132xxps:
  case TOP_xfnmsub132xxxps:
  case TOP_xfnmsub132xpd:
  case TOP_xfnmsub132xxpd:
  // FMA3: form2
  case TOP_xfmadd213xss:
  case TOP_xfmadd213xxss:
  case TOP_xfmadd213xxxss:
  case TOP_xfmadd213xsd:
  case TOP_xfmadd213xxsd:
  case TOP_xfmadd213xxxsd:
  case TOP_xfnmadd213xss:
  case TOP_xfnmadd213xxss:
  case TOP_xfnmadd213xxxss:
  case TOP_xfnmadd213xsd:
  case TOP_xfnmadd213xxsd:
  case TOP_xfnmadd213xxxsd:
  case TOP_xfmadd213xps:
  case TOP_xfmadd213xxps:
  case TOP_xfmadd213xxxps:
  case TOP_xfmadd213xpd:
  case TOP_xfmadd213xxpd:
  case TOP_xfmadd213xxxpd:
  case TOP_xfmaddsub213xps:
  case TOP_xfmaddsub213xxps:
  case TOP_xfmaddsub213xxxps:
  case TOP_xfmaddsub213xpd:
  case TOP_xfmaddsub213xxpd:
  case TOP_xfmaddsub213xxxpd:
  case TOP_xfnmadd213xps:
  case TOP_xfnmadd213xxps:
  case TOP_xfnmadd213xxxps:
  case TOP_xfnmadd213xpd:
  case TOP_xfnmadd213xxpd:
  case TOP_xfnmadd213xxxpd:
  case TOP_xfmsub213xss:
  case TOP_xfmsub213xxss:
  case TOP_xfmsub213xxxss:
  case TOP_xfmsub213xsd:
  case TOP_xfmsub213xxsd:
  case TOP_xfmsub213xxxsd:
  case TOP_xfnmsub213xss:
  case TOP_xfnmsub213xxss:
  case TOP_xfnmsub213xxxss:
  case TOP_xfnmsub213xsd:
  case TOP_xfnmsub213xxsd:
  case TOP_xfnmsub213xxxsd:
  case TOP_xfmsub213xps:
  case TOP_xfmsub213xxps:
  case TOP_xfmsub213xxxps:
  case TOP_xfmsub213xpd:
  case TOP_xfmsub213xxpd:
  case TOP_xfmsub213xxxpd:
  case TOP_xfmsubadd213xps:
  case TOP_xfmsubadd213xxps:
  case TOP_xfmsubadd213xxxps:
  case TOP_xfmsubadd213xpd:
  case TOP_xfmsubadd213xxpd:
  case TOP_xfmsubadd213xxxpd:
  case TOP_xfnmsub213xps:
  case TOP_xfnmsub213xxps:
  case TOP_xfnmsub213xxxps:
  case TOP_xfnmsub213xpd:
  case TOP_xfnmsub213xxpd:
  // FMA3: form3
  case TOP_xfmadd231xss:
  case TOP_xfmadd231xxss:
  case TOP_xfmadd231xxxss:
  case TOP_xfmadd231xsd:
  case TOP_xfmadd231xxsd:
  case TOP_xfmadd231xxxsd:
  case TOP_xfnmadd231xss:
  case TOP_xfnmadd231xxss:
  case TOP_xfnmadd231xxxss:
  case TOP_xfnmadd231xsd:
  case TOP_xfnmadd231xxsd:
  case TOP_xfnmadd231xxxsd:
  case TOP_xfmadd231xps:
  case TOP_xfmadd231xxps:
  case TOP_xfmadd231xxxps:
  case TOP_xfmadd231xpd:
  case TOP_xfmadd231xxpd:
  case TOP_xfmadd231xxxpd:
  case TOP_xfmaddsub231xps:
  case TOP_xfmaddsub231xxps:
  case TOP_xfmaddsub231xxxps:
  case TOP_xfmaddsub231xpd:
  case TOP_xfmaddsub231xxpd:
  case TOP_xfmaddsub231xxxpd:
  case TOP_xfnmadd231xps:
  case TOP_xfnmadd231xxps:
  case TOP_xfnmadd231xxxps:
  case TOP_xfnmadd231xpd:
  case TOP_xfnmadd231xxpd:
  case TOP_xfnmadd231xxxpd:
  case TOP_xfmsub231xss:
  case TOP_xfmsub231xxss:
  case TOP_xfmsub231xxxss:
  case TOP_xfmsub231xsd:
  case TOP_xfmsub231xxsd:
  case TOP_xfmsub231xxxsd:
  case TOP_xfnmsub231xss:
  case TOP_xfnmsub231xxss:
  case TOP_xfnmsub231xxxss:
  case TOP_xfnmsub231xsd:
  case TOP_xfnmsub231xxsd:
  case TOP_xfnmsub231xxxsd:
  case TOP_xfmsub231xps:
  case TOP_xfmsub231xxps:
  case TOP_xfmsub231xxxps:
  case TOP_xfmsub231xpd:
  case TOP_xfmsub231xxpd:
  case TOP_xfmsub231xxxpd:
  case TOP_xfmsubadd231xps:
  case TOP_xfmsubadd231xxps:
  case TOP_xfmsubadd231xxxps:
  case TOP_xfmsubadd231xpd:
  case TOP_xfmsubadd231xxpd:
  case TOP_xfmsubadd231xxxpd:
  case TOP_xfnmsub231xps:
  case TOP_xfnmsub231xxps:
  case TOP_xfnmsub231xxxps:
  case TOP_xfnmsub231xpd:
  case TOP_xfnmsub231xxpd:
    return EBO_Process_SSE5_Load_Exectute_FMA_p1(new_top, mode, base, 
                                                 scale, index, offset, 
                                                 result, ld_op,
                                                 alu_op, actual_tninfo);

  // pattern 2 - 2nd mul src is memop
  case TOP_vfmaddxrss:
  case TOP_vfmaddxxrss:
  case TOP_vfmaddxxxrss:
  case TOP_vfmaddxrsd:
  case TOP_vfmaddxxrsd:
  case TOP_vfmaddxxxrsd:
  case TOP_vfnmaddxrss:
  case TOP_vfnmaddxxrss:
  case TOP_vfnmaddxxxrss:
  case TOP_vfnmaddxrsd:
  case TOP_vfnmaddxxrsd:
  case TOP_vfnmaddxxxrsd:
  case TOP_vfmaddxrps:
  case TOP_vfmaddxxrps:
  case TOP_vfmaddxxxrps:
  case TOP_vfmaddxrpd:
  case TOP_vfmaddxxrpd:
  case TOP_vfmaddxxxrpd:
  case TOP_vfmaddsubxrps:
  case TOP_vfmaddsubxxrps:
  case TOP_vfmaddsubxxxrps:
  case TOP_vfmaddsubxrpd:
  case TOP_vfmaddsubxxrpd:
  case TOP_vfmaddsubxxxrpd:
  case TOP_vfnmaddxrps:
  case TOP_vfnmaddxxrps:
  case TOP_vfnmaddxxxrps:
  case TOP_vfnmaddxrpd:
  case TOP_vfnmaddxxrpd:
  case TOP_vfnmaddxxxrpd:
  case TOP_vfmsubxrss:
  case TOP_vfmsubxxrss:
  case TOP_vfmsubxxxrss:
  case TOP_vfmsubxrsd:
  case TOP_vfmsubxxrsd:
  case TOP_vfmsubxxxrsd:
  case TOP_vfnmsubxrss:
  case TOP_vfnmsubxxrss:
  case TOP_vfnmsubxxxrss:
  case TOP_vfnmsubxrsd:
  case TOP_vfnmsubxxrsd:
  case TOP_vfnmsubxxxrsd:
  case TOP_vfmsubxrps:
  case TOP_vfmsubxxrps:
  case TOP_vfmsubxxxrps:
  case TOP_vfmsubxrpd:
  case TOP_vfmsubxxrpd:
  case TOP_vfmsubxxxrpd:
  case TOP_vfmsubaddxrps:
  case TOP_vfmsubaddxxrps:
  case TOP_vfmsubaddxxxrps:
  case TOP_vfmsubaddxrpd:
  case TOP_vfmsubaddxxrpd:
  case TOP_vfmsubaddxxxrpd:
  case TOP_vfnmsubxrps:
  case TOP_vfnmsubxxrps:
  case TOP_vfnmsubxxxrps:
  case TOP_vfnmsubxrpd:
  case TOP_vfnmsubxxrpd:
  case TOP_vfnmsubxxxrpd:
    return EBO_Process_SSE5_Load_Exectute_FMA_p2(new_top, mode, base, 
                                                 scale, index, offset, 
                                                 result, ld_op,
                                                 alu_op, actual_tninfo);

  // not yet implemented
  default:
    FmtAssert( FALSE, ("EBO_Process_SSE5_Load_Execute: NYI") );
    break;
  }

  return FALSE;
}


BOOL EBO_Is_FMA3( TOP top )
{
  BOOL ret_val;

  switch (top) {
  // form1
  case TOP_xfmadd132ss:
  case TOP_xfmadd132sd:
  case TOP_xfmadd132ps:
  case TOP_xfmadd132pd:
  case TOP_xfmaddsub132ps:
  case TOP_xfmaddsub132pd:
  case TOP_xfmsub132ss:
  case TOP_xfmsub132sd:
  case TOP_xfmsub132ps:
  case TOP_xfmsub132pd:
  case TOP_xfmsubadd132ps:
  case TOP_xfmsubadd132pd:
  // form2
  case TOP_xfmadd213ss:
  case TOP_xfmadd213sd:
  case TOP_xfmadd213ps:
  case TOP_xfmadd213pd:
  case TOP_xfmaddsub213ps:
  case TOP_xfmaddsub213pd:
  case TOP_xfmsub213ss:
  case TOP_xfmsub213sd:
  case TOP_xfmsub213ps:
  case TOP_xfmsub213pd:
  case TOP_xfmsubadd213ps:
  case TOP_xfmsubadd213pd:
  // form3
  case TOP_xfmadd231ss:
  case TOP_xfmadd231sd:
  case TOP_xfmadd231ps:
  case TOP_xfmadd231pd:
  case TOP_xfmaddsub231ps:
  case TOP_xfmaddsub231pd:
  case TOP_xfmsub231ss:
  case TOP_xfmsub231sd:
  case TOP_xfmsub231ps:
  case TOP_xfmsub231pd:
  case TOP_xfmsubadd231ps:
  case TOP_xfmsubadd231pd:
    ret_val = TRUE;
    break;
  // form1
  case TOP_xfnmadd132ss:
  case TOP_xfnmadd132sd:
  case TOP_xfnmadd132ps:
  case TOP_xfnmadd132pd:
  case TOP_xfnmsub132ss:
  case TOP_xfnmsub132sd:
  case TOP_xfnmsub132ps:
  case TOP_xfnmsub132pd:
  // form2
  case TOP_xfnmadd213ss:
  case TOP_xfnmadd213sd:
  case TOP_xfnmadd213ps:
  case TOP_xfnmadd213pd:
  case TOP_xfnmsub213ss:
  case TOP_xfnmsub213sd:
  case TOP_xfnmsub213ps:
  case TOP_xfnmsub213pd:
  // form3
  case TOP_xfnmadd231ss:
  case TOP_xfnmadd231sd:
  case TOP_xfnmadd231ps:
  case TOP_xfnmadd231pd:
  case TOP_xfnmsub231ss:
  case TOP_xfnmsub231sd:
  case TOP_xfnmsub231ps:
  case TOP_xfnmsub231pd:
    ret_val = TRUE;
    break;

  default:
    ret_val = FALSE;
    break;
  }

  return ret_val;
}


TOP EBO_Translate_FMA_4_to_3( OP* alu_op )
{
  const TOP top = OP_code(alu_op);
  TOP new_top;

  switch (top) {
  case TOP_vfmaddss:
    new_top = TOP_xfmadd213ss;
    break;
  case TOP_vfmaddsd:
    new_top = TOP_xfmadd213sd;
    break;
  case TOP_vfmaddps:
    new_top = TOP_xfmadd213ps;
    break;
  case TOP_vfmaddpd:
    new_top = TOP_xfmadd213pd;
    break;
  case TOP_vfmaddsubps:
    new_top = TOP_xfmaddsub213ps;
    break;
  case TOP_vfmaddsubpd:
    new_top = TOP_xfmaddsub213pd;
    break;
  case TOP_vfmsubss:
    new_top = TOP_xfmsub213ss;
    break;
  case TOP_vfmsubsd:
    new_top = TOP_xfmsub213sd;
    break;
  case TOP_vfmsubps:
    new_top = TOP_xfmsub213ps;
    break;
  case TOP_vfmsubpd:
    new_top = TOP_xfmsub213pd;
    break;
  case TOP_vfmsubaddps:
    new_top = TOP_xfmsubadd213ps;
    break;
  case TOP_vfmsubaddpd:
    new_top = TOP_xfmsubadd213pd;
    break;
  case TOP_vfnmaddss:
    new_top = TOP_xfnmadd213ss;
    break;
  case TOP_vfnmaddsd:
    new_top = TOP_xfnmadd213sd;
    break;
  case TOP_vfnmaddps:
    new_top = TOP_xfnmadd213ps;
    break;
  case TOP_vfnmaddpd:
    new_top = TOP_xfnmadd213pd;
    break;
  case TOP_vfnmsubss:
    new_top = TOP_xfnmsub213ss;
    break;
  case TOP_vfnmsubsd:
    new_top = TOP_xfnmsub213sd;
    break;
  case TOP_vfnmsubps:
    new_top = TOP_xfnmsub213ps;
    break;
  case TOP_vfnmsubpd:
    new_top = TOP_xfnmsub213pd;
    break;

  default:
    new_top = TOP_UNDEFINED;
    break;
  }

  return new_top;
}


BOOL EBO_Is_FMA4( TOP top )
{
  BOOL ret_val;

  switch (top) {
  case TOP_vfmaddss:
  case TOP_vfmaddsd:
  case TOP_vfmaddps:
  case TOP_vfmaddpd:
  case TOP_vfmaddsubps:
  case TOP_vfmaddsubpd:
  case TOP_vfmsubss:
  case TOP_vfmsubsd:
  case TOP_vfmsubps:
  case TOP_vfmsubpd:
  case TOP_vfmsubaddps:
  case TOP_vfmsubaddpd:
    ret_val = TRUE;
    break;
  case TOP_vfnmaddss:
  case TOP_vfnmaddsd:
  case TOP_vfnmaddps:
  case TOP_vfnmaddpd:
  case TOP_vfnmsubss:
  case TOP_vfnmsubsd:
  case TOP_vfnmsubps:
  case TOP_vfnmsubpd:
    ret_val = TRUE;
    break;

  default:
    ret_val = FALSE;
    break;
  }

  return ret_val;
}

BOOL EBO_Is_FMA3_NEG( TOP top )
{
  BOOL ret_val;

  switch (top) {
  // form1
  case TOP_xfnmadd132ss:
  case TOP_xfnmadd132sd:
  case TOP_xfnmadd132ps:
  case TOP_xfnmadd132pd:
  case TOP_xfnmsub132ss:
  case TOP_xfnmsub132sd:
  case TOP_xfnmsub132ps:
  case TOP_xfnmsub132pd:
  // form2
  case TOP_xfnmadd213ss:
  case TOP_xfnmadd213sd:
  case TOP_xfnmadd213ps:
  case TOP_xfnmadd213pd:
  case TOP_xfnmsub213ss:
  case TOP_xfnmsub213sd:
  case TOP_xfnmsub213ps:
  case TOP_xfnmsub213pd:
  // form3
  case TOP_xfnmadd231ss:
  case TOP_xfnmadd231sd:
  case TOP_xfnmadd231ps:
  case TOP_xfnmadd231pd:
  case TOP_xfnmsub231ss:
  case TOP_xfnmsub231sd:
  case TOP_xfnmsub231ps:
  case TOP_xfnmsub231pd:
    ret_val = TRUE;
    break;

  default:
    ret_val = FALSE;
    break;
  }

  return ret_val;
}

BOOL EBO_Is_FMA4_NEG( TOP top )
{
  BOOL ret_val;

  switch (top) {
  case TOP_vfnmaddss:
  case TOP_vfnmaddsd:
  case TOP_vfnmaddps:
  case TOP_vfnmaddpd:
  case TOP_vfnmsubss:
  case TOP_vfnmsubsd:
  case TOP_vfnmsubps:
  case TOP_vfnmsubpd:
    ret_val = TRUE;
    break;

  default:
    ret_val = FALSE;
    break;
  }

  return ret_val;
}

static BOOL EBO_Allowable_Unaligned_Vector( OP *alu_op )
{
  const TOP top = OP_code(alu_op);
  BOOL ret_val;

  // no alignment constraint on orochi targets for vector ops
  if (EBO_Is_FMA3(top) || OP_sse5(alu_op))
    return TRUE;

  switch (top) {
  case TOP_vcvtdq2pd:
  case TOP_vcvtps2pd:
  case TOP_cvtdq2pd:
  case TOP_cvtps2pd:
    ret_val = TRUE;
    break;

  default:
    ret_val = FALSE;
    break;
  }
  return ret_val;
}

static BOOL Process_Side_Effects(TN** opnd_tn, 
                                 EBO_TN_INFO** actual_tninfo,
                                 BOOL rval,
                                 BOOL opnds_swapped)
{
  if ( !rval && opnds_swapped ) {
    // return operands to original state
    TN *tmp = opnd_tn[0];
    EBO_TN_INFO* tninfo = actual_tninfo[0];
    actual_tninfo[0] = actual_tninfo[1];
    opnd_tn[0] = opnd_tn[1];
    actual_tninfo[1] = tninfo;
    opnd_tn[1] = tmp;
  }
  return rval;
}

static void Get_Disassociated_FMA_TOP_Codes( OP *alu_op, 
                                             TOP *mul_top, 
                                             TOP *arith_top )
{
  const TOP top = OP_code(alu_op);
  TOP new_mul_top;
  TOP new_arith_top;

  switch (top) {
  // fused multiply-adds
  case TOP_xfmadd213ss:
  case TOP_vfmaddss:
    new_mul_top = TOP_vmulss; 
    new_arith_top = TOP_vfaddss; 
    break;
  case TOP_xfmadd213sd:
  case TOP_vfmaddsd:
    new_mul_top = TOP_vmulsd; 
    new_arith_top = TOP_vfaddsd; 
    break;
  case TOP_xfmadd213ps:
  case TOP_vfmaddps:
    new_mul_top = TOP_vfmul128v32; 
    new_arith_top = TOP_vfadd128v32; 
    break;
  case TOP_xfmadd213pd:
  case TOP_vfmaddpd:
    new_mul_top = TOP_vfmul128v64; 
    new_arith_top = TOP_vfadd128v64; 
    break;

  // fused multiply-addsubs
  case TOP_xfmaddsub213ps:
  case TOP_vfmaddsubps:
    new_mul_top = TOP_vfmul128v32; 
    new_arith_top = TOP_vfaddsub128v32; 
    break;
  case TOP_xfmaddsub213pd:
  case TOP_vfmaddsubpd:
    new_mul_top = TOP_vfmul128v64; 
    new_arith_top = TOP_vfaddsub128v64; 
    break;

  // fused multiply-subs
  case TOP_xfnmadd213ss:
  case TOP_vfnmaddss:
  case TOP_xfmsub213ss:
  case TOP_vfmsubss:
    new_mul_top = TOP_vmulss; 
    new_arith_top = TOP_vsubss; 
    break;
  case TOP_xfnmadd213sd:
  case TOP_vfnmaddsd:
  case TOP_xfmsub213sd:
  case TOP_vfmsubsd:
    new_mul_top = TOP_vmulsd; 
    new_arith_top = TOP_vsubsd; 
    break;
  case TOP_xfnmadd213ps:
  case TOP_vfnmaddps:
  case TOP_xfmsub213ps:
  case TOP_vfmsubps:
    new_mul_top = TOP_vfmul128v32; 
    new_arith_top = TOP_vfsub128v32; 
    break;
  case TOP_xfnmadd213pd:
  case TOP_vfnmaddpd:
  case TOP_xfmsub213pd:
  case TOP_vfmsubpd:
    new_mul_top = TOP_vfmul128v64; 
    new_arith_top = TOP_vfsub128v64; 
    break;

  // everything else
  default:
    new_mul_top = TOP_UNDEFINED; 
    new_arith_top = TOP_UNDEFINED; 
    break;
  }

  *mul_top = new_mul_top;
  *arith_top = new_arith_top;
}

static BOOL Is_Benefitial_To_Load_Exec_Float_OP( OP *ld_op, OP *alu_op )
{
  BOOL ret_val = TRUE;
  TN *result = NULL;

  // find an appropriate result tn
  for (INT i = 0; i < OP_results(ld_op); i++){
    result = OP_result(ld_op, i);
    if( TN_is_register(result) ){
      if( TN_register_class(result) == ISA_REGISTER_CLASS_float )
        break;
    }
  }

  // Bypass this process if register class is not float
  if( ( result != NULL ) &&
      ( TN_register_class(result) == ISA_REGISTER_CLASS_float ) ){
    BB *bb = OP_bb(ld_op);

    const INT len = BB_length(bb);
    INT *regs_in_use = (INT*)alloca(sizeof(INT) * (len+1));
    mINT8 fatpoint[ISA_REGISTER_CLASS_MAX+1];
    TN_MAP conflict_map;
    static MEM_POOL load_exe_pool;
    static BOOL load_exe_pool_init;

    if (! load_exe_pool_init) {
      load_exe_pool_init = TRUE;
      MEM_POOL_Initialize(&load_exe_pool, "live_range_info", TRUE);
    }

    MEM_POOL_Push(&load_exe_pool);
    LRA_Estimate_Fat_Points(bb, fatpoint, regs_in_use, &load_exe_pool);

    conflict_map = Calculate_All_Conflicts(bb, regs_in_use,
                                           TN_register_class(result)); 

    INT P_x = REGISTER_CLASS_register_count(TN_register_class(result));
    INT local_conflicts = Find_Degree_For_TN(result, regs_in_use);

    TN_MAP_Delete(conflict_map);
    MEM_POOL_Pop(&load_exe_pool);

    // In the case where register pressure is manageable, defer this choice 
    // until later(after register allocation)
    if( local_conflicts < P_x )
      ret_val = FALSE;

    if( EBO_Trace_Data_Flow && ret_val ) {
      fprintf(TFile, "load-exec(%d) on %s and %s\n",
              EBO_in_peep, 
              TOP_Name(OP_code(ld_op)), TOP_Name(OP_code(alu_op)));
    }
  }

  return ret_val;
}

BOOL EBO_Associate_FMA( OP* op, EBO_TN_INFO** actual_tninfo )
{
  BOOL ret_val = FALSE;
  TOP top = OP_code(op);

  if ((EBO_in_peep == FALSE) || 
      (CG_load_execute == 0) ||
      (OP_flop(op) == FALSE) ||
      EBO_Is_FMA3(top) || 
      EBO_Is_FMA4(top))
    return ret_val;

  // After register allocation, we have opportunities
  // to claim back some fma operations if lra did a good
  // job. We made this easy when disassociation ran, as 
  // we marked the candidates with the fma top code in the auxcode
  // field.
  if (TOP_is_load_exe(top) == FALSE) {
    TOP aux_top = (TOP)OP_auxcode(op);
    if (EBO_Is_FMA3(aux_top) || EBO_Is_FMA4(aux_top)) {
      mUINT8 op_idx = OP_auxidx(op);
      OP* mul_op = actual_tninfo[op_idx]->in_op;
      BOOL is_fma3 = EBO_Is_FMA3(aux_top);
      if(mul_op && OP_fmul(mul_op) &&
         (actual_tninfo[op_idx]->reference_count == 1) &&
         (TOP_is_load_exe(OP_code(mul_op)) == FALSE)) {
        BB *bb = OP_bb(op);
        TN *mul_opnd1 = OP_opnd(mul_op, 0);
        TN *mul_opnd2 = OP_opnd(mul_op, 1);
        TN *arith_opnd;
        TN *result = OP_result(op, 0);
        OP *fma_op;
        BOOL reg_defined = FALSE;
         
	// Now check for defs on the mul opnds
        for (INT i = 0; i< 2; i++) {
          TN *mul_tn = OP_opnd(mul_op, i);
          ISA_REGISTER_CLASS reg_cl = TN_register_class(mul_tn);
          REGISTER reg = TN_register(mul_tn);
          for( OP *next_op = OP_next(mul_op); next_op != NULL && next_op != op; 
	       next_op = OP_next( next_op ) ){
            if (OP_Defs_Reg(next_op, reg_cl, reg)) {
              reg_defined = TRUE;
              break;
            }
          }
        }

        // If we found a def on the mul opnds, we cannot proceed
        if (reg_defined)
          return ret_val;

        // The mul result and the arith opnd are in different opnds
        switch(op_idx) {
        case 0: arith_opnd = OP_opnd(op, 1); break;
        case 1: arith_opnd = OP_opnd(op, 0); break;
        }

        // Add a register transfer if mul_opnd1
        // does not have the same register as the fma result.
        // We need to preserve destructive dest for fma3.
        if (is_fma3) {
          TN *mulresult_tn = OP_result(mul_op,0);
          REGISTER opnd0_reg = TN_register(mul_opnd1);
          REGISTER result_reg = TN_register(result);
          if (opnd0_reg != result_reg) {
            BOOL swap_needed = FALSE;
            REGISTER arith_reg = TN_register(arith_opnd);
            REGISTER opnd1_reg = TN_register(mul_opnd2);
	    OP *mov_op;
            // If arith_opnd and result are the same the mul result 
            // will be different.
            if (arith_reg == result_reg) {
              // If mulresult_tn and mul_opnds are the same, we cannot reunify,
              // register allocation left us nothing to use.
              if ((TN_register(mulresult_tn) == opnd0_reg) ||
                  (TN_register(mulresult_tn) == opnd1_reg))
                return ret_val;

              mov_op = Mk_OP(TOP_movaps, mulresult_tn, arith_opnd);
              Set_OP_unrolling(mov_op, OP_unrolling(op));
              Set_OP_orig_idx(mov_op, OP_map_idx(op));
              Set_OP_unroll_bb(mov_op, OP_unroll_bb(op));

              OP_srcpos( mov_op ) = OP_srcpos( op );
              BB_Insert_Op_Before( bb, op, mov_op );
              arith_opnd = mulresult_tn;
            } else if (opnd1_reg == result_reg) {
              TN *temp = mul_opnd1;
              mul_opnd1 = mul_opnd2;
              mul_opnd2 = temp;
              swap_needed = TRUE;
            }

            if (swap_needed == FALSE) {
              mov_op = Mk_OP(TOP_movaps, result, mul_opnd1);
              Set_OP_unrolling(mov_op, OP_unrolling(op));
              Set_OP_orig_idx(mov_op, OP_map_idx(op));
              Set_OP_unroll_bb(mov_op, OP_unroll_bb(op));

              OP_srcpos( mov_op ) = OP_srcpos( op );
              BB_Insert_Op_Before( bb, op, mov_op );
              mul_opnd1 = result;
            }
          }
        }

        fma_op = Mk_OP(aux_top, result, mul_opnd1, mul_opnd2, arith_opnd);
        Set_OP_unrolling(fma_op, OP_unrolling(op));
        Set_OP_orig_idx(fma_op, OP_map_idx(op));
        Set_OP_unroll_bb(fma_op, OP_unroll_bb(op));

        OP_srcpos( fma_op ) = OP_srcpos( op );
        BB_Insert_Op_After( bb, op, fma_op );
        OP_Change_Aux_Opcode( op, (mUINT16)OP_auxcode(mul_op), op_idx );

        // Now mark the arith op for deletion and delete the mul op
        BB_Remove_Op(bb, mul_op);
        ret_val = TRUE;
      }
    }
  }
  return ret_val;
}

BOOL EBO_Disassociate_FMA( OP* alu_op )
{
  BOOL ret_val = FALSE;

  if( CG_load_execute == 0 )
    return ret_val;

  // Look for situations where fma insns exist under register pressure.
  // We lose opportunities in this scenario for eliminating
  // live ranges as our default fma behavior is reg-reg only. It is
  // also worth indicating here that load-executing the fma instructions
  // themselves reduces througput, making this action desirable for both cases.
  // This operation preceeds load_execution, so we can handle both scenarios
  // here.  This functionalty is under control of CG_load_execute. We resolve 
  // the register pressure dilemma by disassocation of the fma components 
  // so that we can load execute two components or remove 2 live ranges via 
  // the memory forms of the fma components.
  if( TOP_is_load_exe(OP_code(alu_op)) == FALSE ) {
    BB *bb = OP_bb(alu_op);
    TN *mul_opnd1 = OP_opnd( alu_op, 0 );
    TN *mul_opnd2 = OP_opnd( alu_op, 1 );
    TN *arith_opnd = OP_opnd( alu_op, 2 );
    TN *result = OP_result(alu_op, 0);
    BOOL fma_chained = FALSE;
    const INT len = BB_length(bb);
    INT *regs_in_use = (INT*)alloca(sizeof(INT) * (len+1));
    mINT8 fatpoint[ISA_REGISTER_CLASS_MAX+1];
    TN_MAP conflict_map;
    TOP mul_top;
    TOP arith_top;
    static MEM_POOL fma_exe_pool;
    static BOOL fma_exe_pool_init;

    if (! fma_exe_pool_init) {
      fma_exe_pool_init = TRUE;
      MEM_POOL_Initialize(&fma_exe_pool, "live_range_info", TRUE);
    }

    MEM_POOL_Push(&fma_exe_pool);
    LRA_Estimate_Fat_Points(bb, fatpoint, regs_in_use, &fma_exe_pool);

    conflict_map = Calculate_All_Conflicts(bb, regs_in_use,
                                           ISA_REGISTER_CLASS_float);

    INT P_x = REGISTER_CLASS_register_count(ISA_REGISTER_CLASS_float);
    INT local_conflicts = Find_Degree_For_TN(result, regs_in_use);

    Get_Disassociated_FMA_TOP_Codes( alu_op, &mul_top, &arith_top );

    // Chained single use fma instructions produce simple live ranges
    // which are better left in this form.
    BOOL is_fma3 = EBO_Is_FMA3( OP_code(alu_op) );
    if( Is_TN_Sdsu( result ) ){
      OP *use_op = Find_UseOp_For_TN( result );
      if( use_op ) {
        TOP use_top = OP_code(use_op);
        if( EBO_Is_FMA4( use_top ) || EBO_Is_FMA3( use_top ) )
          fma_chained = TRUE;
      }
    }

    // Now if we successfully mapped a translation, add the new code
    // for scenarios where we have at least 2(5 for fma3) live ranges greater
    // than the number of fp registers, as we will be giving potentially
    // two back from the load exec forms for the new insns.
    INT pressure_seed = is_fma3 ? 5 : 2; 
    if( ( local_conflicts > ( P_x + pressure_seed ) ) && 
        ( fma_chained == FALSE ) &&
        ( mul_top != TOP_UNDEFINED ) &&
        ( arith_top != TOP_UNDEFINED ) ){
      TOP alu_top = OP_code(alu_op);
      if ( EBO_Is_FMA4( alu_top ) &&
           Is_Target_FMA() &&
           ( local_conflicts <= ( P_x + 6) ) ){
        TOP new_top = EBO_Translate_FMA_4_to_3( alu_op );
        OP_Change_Opcode( alu_op, new_top );
        ret_val = FALSE; // we are only changing this op, do not delete it
      } else {
        TN *mul_result = Build_TN_Like(result);
        OP *mul_op = Mk_OP( mul_top, mul_result, mul_opnd1, mul_opnd2 );
        OP *arith_op;
        mUINT8 mul_res_idx;
        if( EBO_Is_FMA4_NEG( alu_top ) || EBO_Is_FMA3_NEG( alu_top) ) {
          arith_op = Mk_OP( arith_top, result, arith_opnd, mul_result );
          mul_res_idx = 1;
        } else {
          arith_op = Mk_OP( arith_top, result, mul_result, arith_opnd );
          mul_res_idx = 0;
        }

        // Add the mul component of the fma
        Set_OP_unrolling( mul_op, OP_unrolling(alu_op) );
        Set_OP_orig_idx( mul_op, OP_map_idx(alu_op) );
        Set_OP_unroll_bb( mul_op, OP_unroll_bb(alu_op) );

        OP_srcpos( mul_op ) = OP_srcpos( alu_op );
        BB_Insert_Op_After( bb, alu_op, mul_op );

        // Now add the arithmetic (add, sub, addsub or subadd part)
        Set_OP_unrolling( arith_op, OP_unrolling(alu_op) );
        Set_OP_orig_idx( arith_op, OP_map_idx(alu_op) );
        Set_OP_unroll_bb( arith_op, OP_unroll_bb(alu_op) );
        OP_Change_Aux_Opcode( arith_op, (mUINT16)OP_code(alu_op), mul_res_idx );

        OP_srcpos( arith_op ) = OP_srcpos( alu_op );
        BB_Insert_Op_After( bb, mul_op, arith_op );
        ret_val = TRUE;
      }
    }

    TN_MAP_Delete(conflict_map);
    MEM_POOL_Pop(&fma_exe_pool);
  }

  return ret_val;
}

BOOL EBO_Load_Execution( OP* alu_op, 
                         TN** opnd_tn,     
                         EBO_TN_INFO** actual_tninfo,
                         int alu_cmp_idx )
{
#if Is_True_On
  if (!(EBO_Opt_Mask & EBO_LOAD_EXECUTION)) return FALSE;
#endif
  const TOP top = OP_code(alu_op);
  BOOL opnds_swapped = FALSE;
  BOOL rval = FALSE;
  BOOL pressure_check_bypass = FALSE;

  if( top == TOP_xor64 ||
      top == TOP_or64  ||
      top == TOP_and64 ||
      top == TOP_test64 )
    return FALSE;

  if ((top == TOP_cmp64) && (EBO_flow_safe == FALSE))
    return FALSE;

  if (TOP_is_load_exe(top) ||		// Skip if TOP is already load-execute.
      Get_Top_For_Addr_Mode(top, BASE_MODE) == TOP_UNDEFINED ||
      !TOP_is_load_exe(Get_Top_For_Addr_Mode(top, BASE_MODE)))
    return FALSE;

  if( OP_opnds(alu_op) == 2               &&
      TN_is_register( OP_opnd(alu_op,1) ) &&
      TNs_Are_Equivalent( OP_opnd(alu_op,0), OP_opnd(alu_op,1) ) )
    return FALSE;

  EBO_TN_INFO* tninfo = NULL;
  int opnd0_indx = 0;  // indicate which opnd will be kept for the new op.

  if( EBO_flow_safe ){
    int i = alu_cmp_idx;
    if( TN_is_register( OP_opnd( alu_op, i ) ) ){
      tninfo = actual_tninfo[i];
      opnd0_indx = OP_opnds(alu_op) - 1 - i;
      Is_True( opnd0_indx >= 0, ("NYI") );
    }
  } else if( EBO_Is_FMA4( OP_code(alu_op) ) ){
    int i;
    OP *mul_in_op = actual_tninfo[1]->in_op;
    OP *add_sub_in_op2 = actual_tninfo[2]->in_op;

    if (CG_fma4_load_exec == FALSE)
      return FALSE;

    i = (mul_in_op && OP_load(mul_in_op)) ? 1 : -1;
    if (i == -1) {
      i = (add_sub_in_op2 && OP_load(add_sub_in_op2)) ? 2 : -1;
    }
    
    // FMA4 ops can only have loads on opnd1 and opnd2.
    if (i == -1) {
      // However we can swap opnd1 and opnd0 because the mul part of the fma
      // is communitive.
      if (actual_tninfo[0]->in_op && OP_load(actual_tninfo[0]->in_op)) {
        TN *tmp = opnd_tn[0];
        tninfo = actual_tninfo[0];
        actual_tninfo[0] = actual_tninfo[1];
        opnd_tn[0] = opnd_tn[1];
        actual_tninfo[1] = tninfo;
        opnd_tn[1] = tmp;
        i = 1;
        opnds_swapped = TRUE;
      } else
        return FALSE;
    } 

    // none of the load operands for fma4 can be move contrained
    if (actual_tninfo[i]->in_opinfo->op_must_not_be_moved)
      return Process_Side_Effects(opnd_tn, actual_tninfo, rval, opnds_swapped);

    alu_cmp_idx = i;
    if( TN_is_register( OP_opnd( alu_op, i ) ) ){
      tninfo = actual_tninfo[i];
      opnd0_indx = OP_opnds(alu_op) - 1 - i;
      Is_True( opnd0_indx >= 0, ("NYI") );
    }
  } else {
    // FMA3 operand selection follows general rules
    if ( EBO_Is_FMA3(OP_code(alu_op)) ){
      if (CG_fma3_load_exec == FALSE)
        return FALSE;
    }
    for( int i = OP_opnds(alu_op) - 1; i >= 0; i-- ){
      if( TN_is_register( OP_opnd( alu_op, i ) ) ){
        tninfo = actual_tninfo[i];
        opnd0_indx = OP_opnds(alu_op) - 1 - i;
        Is_True( opnd0_indx >= 0, ("NYI") );
        break;
      }
    }
  }

  OP* ld_op = tninfo == NULL ? NULL : tninfo->in_op;
  EBO_OP_INFO* ld_opinfo = tninfo == NULL ? NULL : tninfo->in_opinfo;

  // If we cannot use the op we were directed to use, there is no other
  // action to do.
  if (( EBO_flow_safe ) && 
      ( ld_op != NULL) && 
      ( ld_opinfo->op_must_not_be_moved ))
    return Process_Side_Effects(opnd_tn, actual_tninfo, rval, opnds_swapped);

  if( ld_op == NULL || !OP_load( ld_op ) ||
      ld_opinfo->op_must_not_be_moved ){
      
    // Now, try opnd0

    if( !EBO_flow_safe ) {
      if( !TOP_is_commutative( OP_code(alu_op) ) )
        return Process_Side_Effects(opnd_tn, actual_tninfo, rval, opnds_swapped);
    } 

    tninfo = actual_tninfo[0];
    ld_op = tninfo == NULL ? NULL : tninfo->in_op;
    ld_opinfo = tninfo == NULL ? NULL : tninfo->in_opinfo;

    if( ld_op == NULL || !OP_load( ld_op ) ||
	ld_opinfo->op_must_not_be_moved )
      return Process_Side_Effects(opnd_tn, actual_tninfo, rval, opnds_swapped);

    // Check whether we can swap opnd0 and opnd1 of <alu_op>
    TN* result = OP_result( alu_op, 0 );
    TN* opnd0 = OP_opnd( alu_op, 0 );

    if( EBO_in_peep && TNs_Are_Equivalent( result, opnd0 ) )
      return Process_Side_Effects(opnd_tn, actual_tninfo, rval, opnds_swapped);

    opnd0_indx = 1;
  }

  BB* bb = OP_bb( alu_op );

  if( (OP_bb( ld_op ) != bb) && !EBO_flow_safe )
    return Process_Side_Effects(opnd_tn, actual_tninfo, rval, opnds_swapped);

#ifdef TARG_X8664
  if ((OP_bb( ld_op ) != bb) && EBO_flow_safe ) {
    BB *ld_bb = OP_bb( ld_op );
    if (!BS_MemberP(BB_dom_set(bb), BB_id(ld_bb)))
      return Process_Side_Effects(opnd_tn, actual_tninfo, rval, opnds_swapped);
  }
#endif

  /* bug#1480
     The memory opnd of an alu op must be aligned.
     According to Section 4.4.4. (Data Alignment) of Volumn 1,
     "128-bit media instructions that access a 128-bit operand in memory
     incur a general-protection exception if the operand is not aligned to
     a 16-byte boundary ..."
   */
  if( OP_unalign_mem( ld_op ) &&
      !EBO_Allowable_Unaligned_Vector( alu_op ) &&
      TOP_is_vector_op( OP_code(ld_op) ) ){
    return Process_Side_Effects(opnd_tn, actual_tninfo, rval, opnds_swapped);
  }

  if ((OP_code(ld_op) == TOP_ldhps ||
	OP_code(ld_op) == TOP_ldhpsx ||
	OP_code(ld_op) == TOP_ldhpsxx) &&
      OP_code(alu_op) == TOP_cvtps2pd)
    return FALSE;

  // For AVX code, it is too attractive not to eliminate 3 ops for 1
  // TODO: possibly confine to vectorized loops?  For now its a hueristic
  if( Is_Target_Orochi() && Is_Target_AVX() && OP_vec_lo_ldst(ld_op) ) {
    if( ( PU_src_lang(Get_Current_PU()) == PU_F77_LANG ) ||
        ( PU_src_lang(Get_Current_PU()) == PU_F90_LANG ) ) {
      if( ( OP_code(alu_op) == TOP_vcvtdq2pd ) ||
          ( OP_code(alu_op) == TOP_vcvtps2pd ) ){
        pressure_check_bypass = TRUE; 
      }
    } else {
      pressure_check_bypass = TRUE; 
    }
  }

  // Gate load execute before register allocation based on 
  // localized register pressure
  if( !EBO_in_peep ){
    if( !Is_Benefitial_To_Load_Exec_Float_OP( ld_op, alu_op ) && 
        !pressure_check_bypass ) {
      return Process_Side_Effects(opnd_tn, actual_tninfo, rval, opnds_swapped);
    }
  }

  /* Check <index> and <base> will not be re-defined between
     <ld_op> and <alu_op>, inclusive.
  */

  const int index_reg = OP_find_opnd_use( ld_op, OU_index );
  const int base_reg  = OP_find_opnd_use( ld_op, OU_base );

  ADDR_MODE mode = BASE_MODE;

  if( index_reg < 0 && base_reg < 0 )
    mode = N32_MODE;

  else if( index_reg >= 0 )
    mode = base_reg < 0 ? INDEX_MODE : BASE_INDEX_MODE;

  if( mode == N32_MODE ){
    // We need to add one more addressing mode for m32.
    //DevWarn( "Support me!!!" );
    return Process_Side_Effects(opnd_tn, actual_tninfo, rval, opnds_swapped);
  }

  if( index_reg >= 0 ){
    const TN* opnd = OP_opnd( ld_op, index_reg );
    const EBO_TN_INFO* ptinfo = get_tn_info( opnd );
    if( ptinfo != NULL && ptinfo->sequence_num >= tninfo->sequence_num ){
      return Process_Side_Effects(opnd_tn, actual_tninfo, rval, opnds_swapped);
    }
  }

  if( base_reg >= 0 ){
    const TN* opnd = OP_opnd( ld_op, base_reg );
    const EBO_TN_INFO* ptinfo = get_tn_info( opnd );
    if( ptinfo != NULL && ptinfo->sequence_num >= tninfo->sequence_num ){
      return Process_Side_Effects(opnd_tn, actual_tninfo, rval, opnds_swapped);
    }
  }

  /* Make sure the value from <ld_op> will be not over-written by
     any store located between <ld_op> and <alu_op>. (bug#2680)
  */

  {
    const INT hash_value = EBO_hash_op( ld_op, NULL );
    EBO_OP_INFO* opinfo = EBO_opinfo_table[hash_value];

    while( opinfo != NULL ){
      OP* pred_op = opinfo->in_op;

      if( pred_op == ld_op )
	break;

      /* It is quite expensive to check the aliasing info here. */
      if(
#ifdef KEY
         pred_op &&	// Bug 7596
#endif
	 OP_store( pred_op ) )
        return Process_Side_Effects(opnd_tn, actual_tninfo, rval, opnds_swapped);

      opinfo = opinfo->same;
    }
  }

  TOP new_top = Load_Execute_Format( ld_op, alu_op, mode );

  if( new_top == TOP_UNDEFINED )
    return Process_Side_Effects(opnd_tn, actual_tninfo, rval, opnds_swapped);

  if( EBO_flow_safe && (opnd0_indx == 1) ) {
    new_top = Fit_Cmp_By_Load_Usage( new_top, TRUE );
  }

  const INT32 load_uses = hTN_MAP32_Get( _load_exec_map, OP_result(ld_op,0) ) - 1;

  /* It is always profitable to perform load execution if the new latency is shorter.
   */
  if( ( load_uses > CG_load_execute ) &&
      ( CGTARG_Latency(top) < CGTARG_Latency(new_top) ) ){
    return Process_Side_Effects(opnd_tn, actual_tninfo, rval, opnds_swapped);
  }

  // we only want one reaching use in this block for a cmp peep
  if( EBO_flow_safe && ( load_uses > 1 ) ) {
    return Process_Side_Effects(opnd_tn, actual_tninfo, rval, opnds_swapped);
  }

  // If load is volatile, replace with exactly one load-exe OP, in order to
  // maintain the same number of memory accesses.
  if (OP_volatile(ld_op) &&
      load_uses != 1) {
    return Process_Side_Effects(opnd_tn, actual_tninfo, rval, opnds_swapped);
  }

  TN* offset = OP_opnd( ld_op, OP_find_opnd_use( ld_op, OU_offset ) );
  TN* base   = base_reg >= 0 ? OP_opnd( ld_op, base_reg ) : NULL;
  TN* index  = index_reg >= 0 ? OP_opnd( ld_op, index_reg ) : NULL;
  TN* result = OP_has_result( alu_op ) ? OP_result( alu_op, 0 ) : NULL;
  TN* scale  = index_reg >= 0 ?
    OP_opnd( ld_op, OP_find_opnd_use( ld_op, OU_scale ) ) : NULL;

  TN* opnd1 = NULL;
  TN* opnd0 = OP_opnd( alu_op, opnd0_indx );


  // For TOP_cmpi cases
  if( opnd0_indx == 1 && (TN_has_value(opnd0) || EBO_flow_safe ) ){
    opnd1 = opnd0;
    opnd0 = NULL;
  }

  OP* new_op = NULL;

  TOP alu_top = OP_code(alu_op);
  if ((OP_sse5(alu_op) && EBO_Is_FMA4(alu_top)) || EBO_Is_FMA3(alu_top)) {
    // succeed or fail based on layout match
    rval = EBO_Process_SSE5_Load_Execute(new_top, mode, alu_cmp_idx, base,
                                           scale, index, offset,
                                           result, ld_op, alu_op, 
                                           actual_tninfo);
    return Process_Side_Effects(opnd_tn, actual_tninfo, rval, opnds_swapped);
  }

  if( OP_opnds(alu_op) > 2 )
    return Process_Side_Effects(opnd_tn, actual_tninfo, rval, opnds_swapped);

  // Standard Load Execute processing
  if( mode == BASE_MODE ){
    // base + offset
    if( OP_opnds(alu_op) == 1 ){
      if( result == NULL )
	new_op = Mk_OP( new_top, base, offset );
      else
	new_op = Mk_OP( new_top, result, base, offset );
    } else if( opnd1 != NULL )
      new_op = Mk_OP( new_top, result, base, offset, opnd1 );
    else
      new_op = Mk_OP( new_top, result, opnd0, base, offset );

  } else if( mode == BASE_INDEX_MODE ){
    // offset + base + index * scale
    if( OP_opnds(alu_op) == 1 ){
      if( result == NULL )
	new_op = Mk_OP( new_top, base, index, scale, offset );
      else
	new_op = Mk_OP( new_top, result, base, index, scale, offset );
      
    } else if( opnd1 != NULL )
      new_op = Mk_OP( new_top, result, base, index, scale, offset, opnd1 );
    else
      new_op = Mk_OP( new_top, result, opnd0, base, index, scale, offset );

  } else {
    // offset + index * scale
    if( OP_opnds(alu_op) == 1 ){
      if( result == NULL )
	new_op = Mk_OP( new_top, index, scale, offset );
      else
	new_op = Mk_OP( new_top, result, index, scale, offset );
    } else if( opnd1 != NULL )
      new_op = Mk_OP( new_top, result, index, scale, offset, opnd1 );
    else
      new_op = Mk_OP( new_top, result, opnd0, index, scale, offset );
  }

  Is_True( !EBO_in_loop, ("EBO_Load_Execution: NYI (1)") );

  Set_OP_unrolling( new_op, OP_unrolling(alu_op) );
  Set_OP_orig_idx( new_op, OP_map_idx(alu_op) );
  Set_OP_unroll_bb( new_op, OP_unroll_bb(alu_op) );

  Copy_WN_For_Memory_OP( new_op, ld_op );
  if (OP_volatile(ld_op)) {
    Reset_OP_volatile(ld_op);	// allow OP to be deleted
    Set_OP_volatile(new_op);
  }
  OP_srcpos( new_op ) = OP_srcpos( alu_op );
  BB_Insert_Op_After( bb, alu_op, new_op );

  // If folding a restore of a spilled value, mark the spill store as needed
  // even if all the restores are deleted.
  ST *spill_loc = CGSPILL_OP_Spill_Location(ld_op);
  if (spill_loc != (ST *)0) {		// It's a spill OP.
    SPILL_SYM_INFO &info = CGSPILL_Get_Spill_Sym_Info(spill_loc);
    info.Set_Used_By_Load_Exe();
  }

  if( EBO_Trace_Data_Flow ){
    #pragma mips_frequency_hint NEVER
    fprintf( TFile, "EBO_Load_Execution merges " );
    Print_OP_No_SrcLine( ld_op );
    fprintf( TFile, "                   with   " );
    Print_OP_No_SrcLine( alu_op );

    fprintf( TFile, "                   new op " );
    Print_OP_No_SrcLine( new_op );
  }

  return TRUE;
}

static INT
Get_Power_Of_2 (INT val)
{
  INT i, pow2mask;

  pow2mask = 1;
  for ( i = 0; i < 5; ++i ) {
    if (val == pow2mask) return i;
    pow2mask <<= 1;
  }

  FmtAssert(FALSE, ("Get_Power_Of_2 unexpected value (%d)", val));
  /* NOTREACHED */
}

BOOL
Check_No_Use_Between (OP* from, OP* to, TN* result)
{
  if (!TN_is_register(result))
    return FALSE;

  for (OP* op = from->next; op && op != to; op = op->next) {
    for (INT i = 0; i < OP_opnds(op); i ++) {
      TN* opnd = OP_opnd(op, i);
      if (TN_is_register(opnd)) {
	if (TNs_Are_Equivalent(result, opnd))
	  return FALSE;

	// Account for fp uses when testing for sp uses.  We may need to adjust
	// the sp before using the fp in order to make the fp access legal.
	// Bug 11209.
	if (result == SP_TN &&
	    TNs_Are_Equivalent(FP_TN, opnd))
	  return FALSE;
      }
    }
  }

  return TRUE;
}

static BOOL
alu_op_defines_rflags_used (OP* alu_op, OP* op)
{
  // Bug 2040 - if alu_op changes rflags and there is an operaton between
  // op and alu_op that reads the rflags, then we can not delete alu_op.
  if (TOP_is_change_rflags( OP_code(alu_op) )) {
    BOOL rflags_read = FALSE;
    for( OP* next_op = OP_next(alu_op); next_op != NULL && next_op != op; 
	 next_op = OP_next( next_op ) ){
      if( OP_reads_rflags( next_op ) )
	rflags_read = TRUE;
      if( TOP_is_change_rflags( OP_code(next_op) ) )
	break;
    }
    if (rflags_read)
      return TRUE; 
  }

  return FALSE;
}

BOOL
EBO_Lea_Insertion( OP* op, TN** opnd_tn, EBO_TN_INFO** actual_tninfo )
{
#if Is_True_On
  if (!(EBO_Opt_Mask & EBO_LEA_INSERTION)) return FALSE;
#endif
  TOP code = OP_code (op);
  OP* new_op = NULL;
  INT64 offset, offset_tmp;
  INT shift; 
  ST *base, *base_tmp;
  BOOL rflags_read = FALSE;

  // If there is an instruction that is awaiting a rflags update then,
  // do not convert the current op. 
  for( OP* next_op = OP_next(op); next_op != NULL; 
	next_op = OP_next( next_op ) ){
    if( OP_reads_rflags( next_op ) )
      rflags_read = TRUE;
    if( TOP_is_change_rflags( OP_code(next_op) ) )
      break;
  }

  switch (code) {
  case TOP_imul32:
  case TOP_imul64:
    {
      if (!CG_fold_constimul)
        break;
      // Try to fold the second operand	
      OP* alu_op = actual_tninfo[1]->in_op;
      if (alu_op && alu_op->bb == op->bb &&
	  (OP_code(alu_op) == TOP_ldc32 || 
	   OP_code(alu_op) == TOP_ldc64) &&
	  !TN_is_symbol(OP_opnd(alu_op, 0)) &&
	  !TN_live_out_of(OP_result(alu_op, 0), OP_bb(alu_op)) &&
	  ((op->next != NULL &&
	    !is_live_tn(op->next, OP_result(alu_op, 0))) ||
	   (op->next == NULL && 
	    !GTN_SET_MemberP(BB_live_out(OP_bb(op)), 
			     OP_result(alu_op, 0)))) &&
	  Check_No_Use_Between(alu_op, op, OP_result(alu_op, 0))) {
	INT32 val = (INT32)TN_value(OP_opnd(alu_op, 0));
	INT64 val64 = TN_value(OP_opnd(alu_op, 0));
	BOOL is_double = (code == TOP_imul64);
	
	val64 *= val64; // see the use
	if (TNs_Are_Equivalent(OP_opnd(op, 0), OP_opnd(op, 1)))
	  new_op = Mk_OP (is_double?TOP_ldc64:TOP_ldc32, 
	  	          OP_result(op, 0),
			  Gen_Literal_TN(is_double?(INT64)val64:(INT32)val*val,
					 is_double?8:4));
	else if (OP_code(alu_op) == TOP_ldc64) {
	  if (!ISA_LC_Value_In_Class (TN_value(OP_opnd(alu_op, 0)), LC_simm32)) 
	    break;
	}
	if (!new_op)
	  new_op = Mk_OP ((code == TOP_imul32)?TOP_imuli32:TOP_imuli64, 
			  OP_result(op, 0), OP_opnd(op, 0),
			  Gen_Literal_TN(val, 4));
	
	if (alu_op_defines_rflags_used(alu_op, op))
	  return FALSE;

	OP_srcpos( new_op ) = OP_srcpos( op );
	if( EBO_Trace_Data_Flow ){
	  fprintf( TFile, "Lea_Insertion merges " );
	  Print_OP_No_SrcLine(op);
	  fprintf( TFile, "and " );
	  Print_OP_No_SrcLine(alu_op);
	  fprintf( TFile, "with " );
	  Print_OP_No_SrcLine(new_op);
	}
	dec_ref_count(actual_tninfo[1]);
	break;
      }
      if (TN_is_register(OP_opnd(op, 0)) &&
	  !TNs_Are_Equivalent(OP_opnd(op, 0),OP_result(op, 0)) ) {
	// Check if we can interchange the two operands and then see if we 
	// can constant fold.
	OP* alu_op = actual_tninfo[0]->in_op;
	if (alu_op && alu_op->bb == op->bb &&
	    (OP_code(alu_op) == TOP_ldc32 ||
	     OP_code(alu_op) == TOP_ldc64) &&
	    !TN_is_symbol(OP_opnd(alu_op, 0)) &&
	    !TN_live_out_of(OP_result(alu_op, 0), OP_bb(alu_op)) &&
	    ((op->next != NULL &&
	      !is_live_tn(op->next, OP_result(alu_op, 0))) ||
	     (op->next == NULL && 
	      !GTN_SET_MemberP(BB_live_out(OP_bb(op)), 
			       OP_result(alu_op, 0)))) &&
	    Check_No_Use_Between(alu_op, op, OP_result(alu_op, 0))) {
	  INT32 val = (INT32)TN_value(OP_opnd(alu_op, 0));
	  if (OP_code(alu_op) == TOP_ldc64) {
	    if (!ISA_LC_Value_In_Class (TN_value(OP_opnd(alu_op, 0)), LC_simm32)) 
	      break;
	  }
	  new_op = Mk_OP ((code == TOP_imul32)?TOP_imuli32:TOP_imuli64, 
			  OP_result(op, 0), OP_opnd(op, 1), 
			  Gen_Literal_TN(val, 4));

	  if (alu_op_defines_rflags_used(alu_op, op))
	    return FALSE;

	  OP_srcpos( new_op ) = OP_srcpos( op );
	  if( EBO_Trace_Data_Flow ){
	    fprintf( TFile, "Lea_Insertion merges " );
	    Print_OP_No_SrcLine(op);
	    fprintf( TFile, "and " );
	    Print_OP_No_SrcLine(alu_op);
	    fprintf( TFile, "with " );
	    Print_OP_No_SrcLine(new_op);
	  }
	  dec_ref_count(actual_tninfo[0]);
	}
      }
      break;
    }
  case TOP_add32:
  case TOP_add64:
    {
      OP* alu_op = actual_tninfo[0]->in_op;
      if( TN_is_register(OP_opnd(op, 0)) &&
	  TNs_Are_Equivalent(OP_opnd(op, 0),OP_result(op, 0)) ) {
	if (alu_op && alu_op->bb == op->bb && 
	    ((OP_code(alu_op) == TOP_leax32 && code == TOP_add32) ||
	     (OP_code(alu_op) == TOP_leax64 && code == TOP_add64)) &&
	    TN_value(OP_opnd(alu_op, 2)) == 1 &&
	    !TN_is_symbol(OP_opnd(alu_op, 3)) &&
	    TN_value(OP_opnd(alu_op, 3)) == 0 &&
	    TN_is_register(OP_opnd(alu_op, 0)) &&
	    TN_is_register(OP_opnd(alu_op, 1)) &&
	    TN_is_register(OP_opnd(op, 1)) &&
	    TNs_Are_Equivalent(OP_opnd(alu_op, 0),OP_opnd(op, 1)) &&
	    TNs_Are_Equivalent(OP_opnd(alu_op, 0),OP_opnd(alu_op, 1)) &&
	    Check_No_Use_Between(alu_op, op, OP_result(alu_op, 0)) &&
	    Pred_Opnd_Avail(op, actual_tninfo[0], 0)) {
	  new_op = Mk_OP ((code == TOP_add32)?TOP_leax32:TOP_leax64, 
			  OP_result(op, 0), OP_opnd(alu_op, 0), 
			  OP_opnd(alu_op, 1), 
			  Gen_Literal_TN(2, 4), Gen_Literal_TN(0, 4));	  

	  if (rflags_read && 
	      ((TOP_is_change_rflags( OP_code(new_op) ) &&
		!TOP_is_change_rflags( OP_code(op) )) ||
	       (!TOP_is_change_rflags( OP_code(new_op) ) &&
		TOP_is_change_rflags( OP_code(op) ))))
	    return FALSE;

	  if (alu_op_defines_rflags_used(alu_op, op))
	    return FALSE;

	  OP_srcpos( new_op ) = OP_srcpos( op );
	  if( EBO_Trace_Data_Flow ){
	    fprintf( TFile, "Lea_Insertion merges " );
	    Print_OP_No_SrcLine(op);
	    fprintf( TFile, "and " );
	    Print_OP_No_SrcLine(alu_op);
	    fprintf( TFile, "with " );
	    Print_OP_No_SrcLine(new_op);
	  }
	  BB_Remove_Op(OP_bb(alu_op), alu_op);
	} else {
	  // merge cases like:
	  //    leal 0(,%rax,4), %edi
	  //    addl %edi,%ebx
	  alu_op = actual_tninfo[1]->in_op;
	  if (alu_op && alu_op->bb == op->bb && 
	      ((OP_code(alu_op) == TOP_leaxx32 && code == TOP_add32) ||
	       (OP_code(alu_op) == TOP_leaxx64 && code == TOP_add64)) &&
	      !TN_live_out_of(OP_result(alu_op, 0), OP_bb(alu_op)) &&
	      ((op->next != NULL &&
		!is_live_tn(op->next, OP_result(alu_op, 0))) ||
	       (op->next == NULL && 
		!GTN_SET_MemberP(BB_live_out(OP_bb(op)), 
				 OP_result(alu_op, 0)))) &&
	      Check_No_Use_Between(alu_op, op, OP_result(alu_op, 0))) {
	    // result of alu_op should not be redefined between alu_op and op
	    EBO_TN_INFO *src0_info, *src1_info;
	    src0_info = get_tn_info( OP_opnd(op, 1));
	    src1_info = get_tn_info( OP_result(alu_op, 0));
	    if (src0_info->sequence_num > src1_info->sequence_num)
	      break;
	    
	    // There should be no redefinitions of alu_op's first opnd.
	    if (!Pred_Opnd_Avail(op, actual_tninfo[1], 0))
	      break;

	    new_op = Mk_OP ((code == TOP_add32)?TOP_leax32:TOP_leax64, 
			    OP_result(op, 0), OP_opnd(op, 0), 
			    OP_opnd(alu_op, 0), OP_opnd(alu_op, 1), 
			    OP_opnd(alu_op, 2));

	    if (rflags_read && 
		((TOP_is_change_rflags( OP_code(new_op) ) &&
		  !TOP_is_change_rflags( OP_code(op) )) ||
		 (!TOP_is_change_rflags( OP_code(new_op) ) &&
		  TOP_is_change_rflags( OP_code(op) ))))
	      return FALSE;

	    if (alu_op_defines_rflags_used(alu_op, op))
	      return FALSE;

	    OP_srcpos( new_op ) = OP_srcpos( op );
	    if( EBO_Trace_Data_Flow ){
	      fprintf( TFile, "Lea_Insertion merges " );
	      Print_OP_No_SrcLine(op);
	      fprintf( TFile, "and " );
	      Print_OP_No_SrcLine(alu_op);
	      fprintf( TFile, "with " );
	      Print_OP_No_SrcLine(new_op);
	    }
	    BB_Remove_Op(OP_bb(alu_op), alu_op);	    
	  }
	}

	break;
      }

      //opnd0 and result are not equivalent
      if (alu_op && alu_op->bb == op->bb && 
	  ((OP_code(alu_op) == TOP_leax32 && code == TOP_add32) ||
	   (OP_code(alu_op) == TOP_leax64 && code == TOP_add64)) &&
	  TN_value(OP_opnd(alu_op, 2)) == 1 &&
	  !TN_is_symbol(OP_opnd(alu_op, 3)) &&
	  TN_value(OP_opnd(alu_op, 3)) == 0 &&
	  TN_is_register(OP_opnd(alu_op, 0)) &&
	  TN_is_register(OP_opnd(alu_op, 1)) &&
	  TN_is_register(OP_opnd(op, 1)) &&
	  TNs_Are_Equivalent(OP_opnd(alu_op, 0),OP_opnd(op, 1)) &&
	  TNs_Are_Equivalent(OP_opnd(alu_op, 0),OP_opnd(alu_op, 1)) &&
	  !TN_live_out_of(OP_result(alu_op, 0), OP_bb(alu_op)) &&
	  ((op->next != NULL &&
	    !is_live_tn(op->next, OP_result(alu_op, 0))) ||
	   (op->next == NULL && 
	    !GTN_SET_MemberP(BB_live_out(OP_bb(op)), OP_result(alu_op, 0)))) &&
	  Check_No_Use_Between(alu_op, op, OP_result(alu_op, 0))) {
	EBO_TN_INFO *src0_info, *src1_info;
	src0_info = get_tn_info( OP_opnd(op, 1));
	src1_info = get_tn_info( OP_opnd(alu_op, 0));
	// operand redefined?	
	if (src0_info && src1_info &&
	    src0_info->sequence_num > src1_info->sequence_num) {
	  new_op = Mk_OP ((code == TOP_add32)?TOP_leax32:TOP_leax64, 
			  OP_result(op, 0), OP_opnd(alu_op, 0), 
			  OP_opnd(alu_op, 1), 
			  Gen_Literal_TN(2, 4), Gen_Literal_TN(0, 4));	  

	  if (rflags_read && 
	      ((TOP_is_change_rflags( OP_code(new_op) ) &&
		!TOP_is_change_rflags( OP_code(op) )) ||
	       (!TOP_is_change_rflags( OP_code(new_op) ) &&
		TOP_is_change_rflags( OP_code(op) ))))
	    return FALSE;

	  if (alu_op_defines_rflags_used(alu_op, op))
	    return FALSE;

	  OP_srcpos( new_op ) = OP_srcpos( op );
	  if( EBO_Trace_Data_Flow ){
	    fprintf( TFile, "Lea_Insertion merges " );
	    Print_OP_No_SrcLine(op);
	    fprintf( TFile, "and " );
	    Print_OP_No_SrcLine(alu_op);
	    fprintf( TFile, "with " );
	    Print_OP_No_SrcLine(new_op);
	  }
	  BB_Remove_Op(OP_bb(alu_op), alu_op);
	  break;
	}
      }
     
      // Bug 1563 - dont let this module place SP or FP as the index register.
      if (OP_opnd(op, 1) == SP_TN || OP_opnd(op, 1) == FP_TN) {
	// Can not push things around after Adjust_X86_Style_Op
	if (EBO_in_peep) break; 
	// something crazy?
        if (OP_opnd(op, 0) == SP_TN || OP_opnd(op, 0) == FP_TN) break;
        new_op = Mk_OP ((code == TOP_add32)?TOP_leax32:TOP_leax64, 
	  	      OP_result(op, 0), OP_opnd(op, 1), OP_opnd(op, 0), 
		      Gen_Literal_TN(1, 4), Gen_Literal_TN(0, 4));
      } else 
        new_op = Mk_OP ((code == TOP_add32)?TOP_leax32:TOP_leax64, 
	  	      OP_result(op, 0), OP_opnd(op, 0), OP_opnd(op, 1), 
		      Gen_Literal_TN(1, 4), Gen_Literal_TN(0, 4));

      if (rflags_read && 
	  ((TOP_is_change_rflags( OP_code(new_op) ) &&
	    !TOP_is_change_rflags( OP_code(op) )) ||
	   (!TOP_is_change_rflags( OP_code(new_op) ) &&
	    TOP_is_change_rflags( OP_code(op) ))))
	return FALSE;

      OP_srcpos( new_op ) = OP_srcpos( op );
      if( EBO_Trace_Data_Flow ){
	fprintf( TFile, "Lea_Insertion removes " );
	Print_OP_No_SrcLine(op);
	fprintf( TFile, "Lea_Insertion inserts " );
	Print_OP_No_SrcLine(new_op);
      }
      break;
    }
  case TOP_addi32:
  case TOP_addi64:
    {
      base = base_tmp = NULL;
      offset = offset_tmp = 0;
      if (TN_is_symbol (OP_opnd(op, 1))) {
	if (!EBO_in_peep)
	  // symbol offsets are not known until data layout time
	  break;
	TN *t = OP_opnd(op, 1);
	Base_Symbol_And_Offset (TN_var(t), &base, &offset);
	if (base == SP_Sym || base == FP_Sym) {
	  offset += TN_offset(t);
	  if ( TN_is_reloc_neg(t) )
	    offset = -offset;
	  if ( TN_is_reloc_low16(t) )
	    offset = offset & 0xffff;
	  else if ( TN_is_reloc_high16(t) )
	    offset = ( ( offset - (short)offset ) >> 16) & 0xffff;
	  else if ( TN_is_reloc_higher(t) )
	    offset = ( ( offset + 0x80008000LL ) >> 32 ) & 0xffff;
	  else if ( TN_is_reloc_highest(t) )
	    offset = ( ( offset + 0x800080008000LL ) >> 48 ) & 0xffff;
	} 
	
      } else 
	offset = TN_value(OP_opnd(op, 1));
      
      if( TN_is_register(OP_opnd(op, 0)) &&
	  TN_is_register(OP_result(op, 0)) &&
	  TNs_Are_Equivalent(OP_opnd(op, 0),OP_result(op, 0)) ) {
	OP* alu_op = actual_tninfo[0]->in_op;
	if (alu_op && alu_op->bb && alu_op->bb == op->bb && 
	    OP_code(alu_op) == code &&
	    Check_No_Use_Between(alu_op, op, OP_result(alu_op, 0))) {
	  if (TN_is_symbol (OP_opnd(alu_op, 1))) {
	    if (!EBO_in_peep)
	      // symbol offsets are not known until data layout time
	      break;
	    TN *tmp = OP_opnd(alu_op, 1);
	    Base_Symbol_And_Offset (TN_var(tmp), &base_tmp, &offset_tmp);
	    if (base != base_tmp)
	      break;
	    if (base_tmp == SP_Sym || base_tmp == FP_Sym) {
	      offset_tmp += TN_offset(tmp);
	      if ( TN_is_reloc_neg(tmp) )
		offset_tmp = -offset_tmp;
	      if ( TN_is_reloc_low16(tmp) )
		offset_tmp = offset_tmp & 0xffff;
	      else if ( TN_is_reloc_high16(tmp) )
		offset_tmp = 
		( ( offset_tmp - (short)offset_tmp ) >> 16) & 0xffff;
	      else if ( TN_is_reloc_higher(tmp) )
		offset_tmp = ( ( offset_tmp + 0x80008000LL ) >> 32 ) & 0xffff;
	      else if ( TN_is_reloc_highest(tmp) )
		offset_tmp = 
		( ( offset_tmp + 0x800080008000LL ) >> 48 ) & 0xffff;
	    } 
	    
	  } else 
	    offset_tmp = TN_value(OP_opnd(alu_op, 1));
      
	  // see if the value can fit in a simm32 offset field of lea
	  if (ISA_LC_Value_In_Class (offset+offset_tmp, LC_simm32)) {
	    TN* offset_tn = NULL;
	    if (TN_is_symbol(OP_opnd(op, 1)) && 
		base && base != SP_Sym && base != FP_Sym) 
	      offset_tn = Gen_Symbol_TN( TN_var(OP_opnd(op, 0)), 
					TN_offset(OP_opnd(op, 0))+
					offset+offset_tmp,0);
	    new_op = Mk_OP ((code == TOP_addi32)?TOP_addi32:TOP_addi64,
			    OP_result(op, 0), OP_opnd(op, 0), 
			    offset_tn? offset_tn: 
			    Gen_Literal_TN(offset+offset_tmp, 4));

	    if (rflags_read && 
		((TOP_is_change_rflags( OP_code(new_op) ) &&
		  !TOP_is_change_rflags( OP_code(op) )) ||
		 (!TOP_is_change_rflags( OP_code(new_op) ) &&
		  TOP_is_change_rflags( OP_code(op) ))))
	      return FALSE;

	    if (alu_op_defines_rflags_used(alu_op, op))
	      return FALSE;

	    OP_srcpos( new_op ) = OP_srcpos( op );
	    if( EBO_Trace_Data_Flow ){
	      fprintf( TFile, "Lea_Insertion merges " );
	      Print_OP_No_SrcLine(op);
	      fprintf( TFile, "and " );
	      Print_OP_No_SrcLine(alu_op);
	      fprintf( TFile, "with " );
	      Print_OP_No_SrcLine(new_op);
	    }

	    BB_Remove_Op(OP_bb(alu_op), alu_op);
	  }
	}
	break;
      }
      
      // see if the value can fit in a simm32 offset field of lea
      if (ISA_LC_Value_In_Class (offset, LC_simm32))
	new_op = Mk_OP ((code == TOP_addi32)?TOP_lea32:TOP_lea64,
			OP_result(op, 0), OP_opnd(op, 0), OP_opnd(op, 1));

      if (rflags_read && new_op &&
	  ((TOP_is_change_rflags( OP_code(new_op) ) &&
	    !TOP_is_change_rflags( OP_code(op) )) ||
	   (!TOP_is_change_rflags( OP_code(new_op) ) &&
	    TOP_is_change_rflags( OP_code(op) ))))
	return FALSE;
      
      OP_srcpos( new_op ) = OP_srcpos( op );
      if (new_op) {
	if( EBO_Trace_Data_Flow ){
	  fprintf( TFile, "Lea_Insertion removes " );
	  Print_OP_No_SrcLine(op);
	  fprintf( TFile, "Lea_Insertion inserts " );
	  Print_OP_No_SrcLine(new_op);
	}
      }
      break;
    }
  case TOP_shli32:
  case TOP_shli64:
    { 
      // Transform the shifts into leaxx if the result and the first operand 
      // are non-identical.
      if( (TN_is_register(OP_opnd(op, 0)) &&
	   !TNs_Are_Equivalent(OP_opnd(op, 0),OP_result(op, 0)))) {
	// Check if the shift amount is 0/1/2/3
	shift = TN_value(OP_opnd(op,1));
	if (shift != 0 && shift != 1 && shift != 2 && shift != 3)
	  break;
	
	shift = 1 << shift;
	// convert 'shliq $const,opnd0,res' to
	// 'leaxxq 0(,opnd0,1<<$const),res'
	new_op = Mk_OP ((code == TOP_shli32)?TOP_leaxx32:TOP_leaxx64,
			OP_result(op, 0), OP_opnd(op, 0), 
			Gen_Literal_TN(shift, 4), 
			Gen_Literal_TN(0, 4));
      }

      if (rflags_read && new_op &&
	  ((TOP_is_change_rflags( OP_code(new_op) ) &&
	    !TOP_is_change_rflags( OP_code(op) )) ||
	   (!TOP_is_change_rflags( OP_code(new_op) ) &&
	    TOP_is_change_rflags( OP_code(op) ))))
	return FALSE;

      if (new_op) {
	if( EBO_Trace_Data_Flow ){
	  fprintf( TFile, "Lea_Insertion removes " );
	  Print_OP_No_SrcLine(op);
	  fprintf( TFile, "Lea_Insertion inserts " );
	  Print_OP_No_SrcLine(new_op);
	}
      }
      break;
    }
  case TOP_lea32:
  case TOP_lea64: 
    {
      if (TN_is_symbol (OP_opnd(op, 1))) {
	if (!EBO_in_peep)
	  // symbol offsets are not known until data layout time
	  break;
	TN *t = OP_opnd(op, 1);
	Base_Symbol_And_Offset (TN_var(t), &base, &offset);
	if (base == SP_Sym || base == FP_Sym) {
	  offset += TN_offset(t);
	  if ( TN_is_reloc_neg(t) )
	    offset = -offset;
	  if ( TN_is_reloc_low16(t) )
	    offset = offset & 0xffff;
	  else if ( TN_is_reloc_high16(t) )
	    offset = ( ( offset - (short)offset ) >> 16) & 0xffff;
	  else if ( TN_is_reloc_higher(t) )
	    offset = ( ( offset + 0x80008000LL ) >> 32 ) & 0xffff;
	  else if ( TN_is_reloc_highest(t) )
	    offset = ( ( offset + 0x800080008000LL ) >> 48 ) & 0xffff;
	} 
	
      } else 
	offset = TN_value(OP_opnd(op, 1));
      
      // Fold a previous addi or lea into this lea
      OP* alu_op = actual_tninfo[0]->in_op;
      if (alu_op && alu_op->bb == op->bb &&
	  ((OP_code(op) == TOP_lea32 &&
	    (OP_code(alu_op) == TOP_addi32 ||
	     OP_code(alu_op) == TOP_lea32)) ||
	   (OP_code(op) == TOP_lea64 &&
	    (OP_code(alu_op) == TOP_addi64 ||
	     OP_code(alu_op) == TOP_lea64))) &&
	  // we should be able to combine offsets
	  (!TN_is_symbol (OP_opnd(alu_op, 1)) || EBO_in_peep) &&
	  // Can not add up two symbols right now
	  (!TN_is_symbol (OP_opnd(op, 1)) || 
	   !TN_is_symbol (OP_opnd(alu_op, 1))) &&
	  !TN_live_out_of(OP_result(alu_op, 0), OP_bb(alu_op)) &&
	  ((op->next != NULL &&
	    !is_live_tn(op->next, OP_result(alu_op, 0))) ||
	   (op->next == NULL && 
	    !GTN_SET_MemberP(BB_live_out(OP_bb(op)), OP_result(alu_op, 0)))) &&
	  // There should be no other uses of result of alu_op
	  Check_No_Use_Between(alu_op, op, OP_result(alu_op, 0)) &&
	  // There should be no redefinitions of alu_op's first opnd.
	  Pred_Opnd_Avail(op, actual_tninfo[0], 0) &&
	  // The following scenario occurs when LRA removes a copy op between
	  // a local TN and a GTN, after register allocation, and later EBO
	  // deletes the assignment to the local TN. 
	  // See -O3 176.gcc/local_alloc.c BB:26
	  !TN_live_out_of(OP_opnd(alu_op, 0), OP_bb(alu_op)) &&
	  TN_is_register(OP_result(alu_op, 0)) &&
	  TN_is_register(OP_opnd(alu_op, 0)) &&
	  (!TNs_Are_Equivalent(OP_result(alu_op, 0), OP_opnd(alu_op, 0)) ||
	   !is_live_tn(alu_op->next, OP_opnd(alu_op, 0)))) {
	INT64 tmp_offset, new_offset;
	TN* ofst_tn = NULL;
	if (TN_is_symbol (OP_opnd(alu_op, 1))) {
	  TN *t = OP_opnd(alu_op, 1);
	  Base_Symbol_And_Offset (TN_var(t), &base_tmp, &tmp_offset);
	  if (base_tmp == SP_Sym || base_tmp == FP_Sym) {
	    tmp_offset += TN_offset(t);
	    if ( TN_is_reloc_neg(t) )
	      tmp_offset = -tmp_offset;
	    if ( TN_is_reloc_low16(t) )
	      tmp_offset = tmp_offset & 0xffff;
	    else if ( TN_is_reloc_high16(t) )
	      tmp_offset = 
		( ( tmp_offset - (short)tmp_offset ) >> 16) & 0xffff;
	    else if ( TN_is_reloc_higher(t) )
	      tmp_offset = ( ( tmp_offset + 0x80008000LL ) >> 32 ) & 0xffff;
	    else if ( TN_is_reloc_highest(t) )
	      tmp_offset = 
		( ( tmp_offset + 0x800080008000LL ) >> 48 ) & 0xffff;
	  } 
	  
	} else 
	  tmp_offset = TN_value(OP_opnd(alu_op, 1));
      
	new_offset = offset + tmp_offset;

	if (base_tmp || base) {
	  TN* t = OP_opnd(op, 1);
	  if (TN_is_symbol(t) && base && base != SP_Sym && base != FP_Sym)
	    ofst_tn = Gen_Symbol_TN( TN_var(t), TN_offset(t)+new_offset,0);
	  t = OP_opnd(alu_op, 1);
	  if (TN_is_symbol(t) && 
	      base_tmp && base_tmp != SP_Sym && base_tmp != FP_Sym)
	    ofst_tn = 
	      Gen_Symbol_TN( TN_var(t), TN_offset(t)+new_offset,0);	  
	}

	// check if new_offset can fit in a simm32 field of a new lea
	if (ISA_LC_Value_In_Class (new_offset, LC_simm32)) {
	  new_op = Mk_OP ((code == TOP_lea32)?TOP_lea32:TOP_lea64,
			  OP_result(op, 0), OP_opnd(alu_op, 0), 
			  ofst_tn ? ofst_tn : Gen_Literal_TN(new_offset, 4));
	  
	  if (alu_op_defines_rflags_used(alu_op, op))
	    return FALSE;

	  OP_srcpos( new_op ) = OP_srcpos( op );
	  if( EBO_Trace_Data_Flow ){
	    fprintf( TFile, "Lea_Insertion merges " );
	    Print_OP_No_SrcLine(op);
	    fprintf( TFile, "and " );
	    Print_OP_No_SrcLine(alu_op);
	    fprintf( TFile, "with " );
	    Print_OP_No_SrcLine(new_op);
	  }
	  BB_Remove_Op(OP_bb(alu_op), alu_op);
	  break;
	}
      }

      if( TN_is_register(OP_opnd(op, 0)) &&
	  TNs_Are_Equivalent(OP_opnd(op, 0),OP_result(op, 0))  &&
	  // Do not convert leaq $symbol(%rsi), %rsi to 
	  // addq $symbol, %rsi (no space for 64-bit immediate)
	  !TN_is_symbol(OP_opnd(op, 1))) {
	new_op = Mk_OP ((code == TOP_lea32)?TOP_addi32:TOP_addi64,
			OP_result(op, 0), OP_opnd(op, 0), 
			OP_opnd(op, 1));
      }
      // convert 'leaq 0(%rsp), %rax' to 'movq %rsp, %rax'
      else if (!TN_is_symbol (OP_opnd(op, 1)) && 
	       TN_value(OP_opnd(op, 1)) == 0) {
	new_op = Mk_OP ((code == TOP_lea32)?TOP_mov32:TOP_mov64,
			OP_result(op, 0), OP_opnd(op, 0));
      }
      // convert 'leaq 0(%rsp), %rax' to 'movq %rsp, %rax'
      else if (TN_is_symbol (OP_opnd(op, 1)) && 
	       (base == SP_Sym || base == FP_Sym) &&
	       EBO_in_peep &&
	       offset == 0) {
	new_op = Mk_OP ((code == TOP_lea32)?TOP_mov32:TOP_mov64,
			OP_result(op, 0), OP_opnd(op, 0));
      }

      if (rflags_read && new_op &&
	  ((TOP_is_change_rflags( OP_code(new_op) ) &&
	    !TOP_is_change_rflags( OP_code(op) )) ||
	   (!TOP_is_change_rflags( OP_code(new_op) ) &&
	    TOP_is_change_rflags( OP_code(op) ))))
	return FALSE;

      if (new_op) {
	if( EBO_Trace_Data_Flow ){
	  fprintf( TFile, "Lea_Insertion removes " );
	  Print_OP_No_SrcLine(op);
	  fprintf( TFile, "Lea_Insertion inserts " );
	  Print_OP_No_SrcLine(new_op);
	}
      }
      break;
    }
  case TOP_leax32:
  case TOP_leax64:
    {
      OP* alu_op = actual_tninfo[0]->in_op;
      if( TN_is_register(OP_opnd(op, 0)) &&
	  TN_is_register(OP_opnd(op, 1)) &&
	  TN_is_register(OP_result(op, 0)) &&
	  !TNs_Are_Equivalent(OP_opnd(op, 0),OP_result(op, 0)) &&
	  !TNs_Are_Equivalent(OP_opnd(op, 1),OP_result(op, 0))) {
	//opnd0 and result are not equivalent
	if (alu_op && alu_op->bb == op->bb && 
	    OP_code(alu_op) == code && 
	    TN_value(OP_opnd(alu_op, 2)) == 1 &&
	    !TN_is_symbol(OP_opnd(alu_op, 3)) &&
	    TN_value(OP_opnd(alu_op, 3)) == 0 &&
	    TN_value(OP_opnd(op, 2)) == 1 &&
	    !TN_is_symbol(OP_opnd(op, 3)) &&
	    TN_value(OP_opnd(op, 3)) == 0 &&
	    TN_is_register(OP_opnd(alu_op, 0)) &&
	    TN_is_register(OP_opnd(alu_op, 1)) &&
	    TN_is_register(OP_opnd(op, 1)) &&
	    TNs_Are_Equivalent(OP_opnd(alu_op, 0),OP_opnd(op, 1)) &&
	    TNs_Are_Equivalent(OP_opnd(alu_op, 0),OP_opnd(alu_op, 1)) &&
	    !TN_live_out_of(OP_result(alu_op, 0), OP_bb(alu_op)) &&
	    ((op->next != NULL &&
	      !is_live_tn(op->next, OP_result(alu_op, 0))) ||
	     (op->next == NULL && 
	      !GTN_SET_MemberP(BB_live_out(OP_bb(op)), OP_result(alu_op, 0)))) &&
	    Check_No_Use_Between(alu_op, op, OP_result(alu_op, 0)) &&
	    Pred_Opnd_Avail(op, actual_tninfo[0], 0)) {
	  EBO_TN_INFO *src0_info, *src1_info;
	  src0_info = get_tn_info( OP_opnd(op, 1));
	  src1_info = get_tn_info( OP_opnd(alu_op, 0));

	  if (src0_info && src1_info &&
	      src0_info->sequence_num <= src1_info->sequence_num) {
	    new_op = Mk_OP ((code == TOP_leax32)?TOP_leax32:TOP_leax64, 
			    OP_result(op, 0), OP_opnd(alu_op, 0), 
			    OP_opnd(alu_op, 1), 
			    Gen_Literal_TN(2, 4), Gen_Literal_TN(0, 4));	  

	    if (rflags_read && 
		((TOP_is_change_rflags( OP_code(new_op) ) &&
		  !TOP_is_change_rflags( OP_code(op) )) ||
		 (!TOP_is_change_rflags( OP_code(new_op) ) &&
		  TOP_is_change_rflags( OP_code(op) ))))
	      return FALSE;

	    if (alu_op_defines_rflags_used(alu_op, op))
	      return FALSE;

	    OP_srcpos( new_op ) = OP_srcpos( op );
	    if( EBO_Trace_Data_Flow ){
	      fprintf( TFile, "Lea_Insertion merges " );
	      Print_OP_No_SrcLine(op);
	      fprintf( TFile, "and " );
	      Print_OP_No_SrcLine(alu_op);
	      fprintf( TFile, "with " );
	      Print_OP_No_SrcLine(new_op);
	    }
	    BB_Remove_Op(OP_bb(alu_op), alu_op);
	  }
	} else if (CG_fold_shiftadd && alu_op && alu_op->bb == op->bb && 
		   ((OP_code(alu_op) == TOP_leaxx64 && 
		     code == TOP_lea64) ||
		    (OP_code(alu_op) == TOP_leaxx32 && 
		     code == TOP_lea32)) &&
		   TN_value(OP_opnd(op, 2)) == 1 &&
		   !TN_is_symbol(OP_opnd(alu_op, 2)) &&
		   TN_value(OP_opnd(alu_op, 2)) == 0 &&
		   !TN_is_symbol(OP_opnd(op, 3)) &&
		   TN_value(OP_opnd(op, 3)) == 0 &&
		   !TN_live_out_of(OP_result(alu_op, 0), OP_bb(alu_op)) &&
		   ((op->next != NULL &&
		     !is_live_tn(op->next, OP_result(alu_op, 0))) ||
		    (op->next == NULL && 
		     !GTN_SET_MemberP(BB_live_out(OP_bb(op)), 
				      OP_result(alu_op, 0)))) &&
		   Check_No_Use_Between(alu_op, op, OP_result(alu_op, 0))) {
	  // Fold leaxx into a following leax.
	  // Enabling this slows down crafty by 1 second but we would just 
	  // have the code around to see if it may help in other cases.
	  new_op = Mk_OP ((code == TOP_leax32)?TOP_leax32:TOP_leax64, 
			  OP_result(op, 0), OP_opnd(op, 1), 
			  OP_opnd(alu_op, 0), OP_opnd(alu_op, 1),
			  Gen_Literal_TN(0, 4));

	  if (alu_op_defines_rflags_used(alu_op, op))
	    return FALSE;

	  OP_srcpos( new_op ) = OP_srcpos( op );
	  if( EBO_Trace_Data_Flow ){
	    fprintf( TFile, "Lea_Insertion merges " );
	    Print_OP_No_SrcLine(op);
	    fprintf( TFile, "and " );
	    Print_OP_No_SrcLine(alu_op);
	    fprintf( TFile, "with " );
	    Print_OP_No_SrcLine(new_op);
	  }
	  BB_Remove_Op(OP_bb(alu_op), alu_op);
	} else { 

	  // check for second operand
	  alu_op = actual_tninfo[1]->in_op;
	  if (alu_op && alu_op->bb == op->bb && 
	      OP_code(alu_op) == code && 
	      TN_value(OP_opnd(alu_op, 2)) == 1 &&
	      !TN_is_symbol(OP_opnd(alu_op, 3)) &&
	      TN_value(OP_opnd(alu_op, 3)) == 0 &&
	      TN_value(OP_opnd(op, 2)) == 1 &&
	      !TN_is_symbol(OP_opnd(op, 3)) &&
	      TN_value(OP_opnd(op, 3)) == 0 &&
	      TN_is_register(OP_opnd(alu_op, 0)) &&
	      TN_is_register(OP_opnd(alu_op, 1)) &&
	      TN_is_register(OP_opnd(op, 0)) &&
	      TNs_Are_Equivalent(OP_opnd(alu_op, 0),OP_opnd(op, 0)) &&
	      TNs_Are_Equivalent(OP_opnd(alu_op, 0),OP_opnd(alu_op, 1)) &&
	      !TN_live_out_of(OP_result(alu_op, 0), OP_bb(alu_op)) &&
	      ((op->next != NULL &&
		!is_live_tn(op->next, OP_result(alu_op, 0))) ||
	       (op->next == NULL && 
		!GTN_SET_MemberP(BB_live_out(OP_bb(op)), OP_result(alu_op, 0)))) &&
	      Check_No_Use_Between(alu_op, op, OP_result(alu_op, 0)) &&
	      Pred_Opnd_Avail(op, actual_tninfo[1], 0)) {
	    new_op = Mk_OP ((code == TOP_leax32)?TOP_leax32:TOP_leax64, 
			    OP_result(op, 0), OP_opnd(op, 0), 
			    OP_opnd(alu_op, 1), 
			    Gen_Literal_TN(2, 4), Gen_Literal_TN(0, 4));	  
	    if (rflags_read && 
		((TOP_is_change_rflags( OP_code(new_op) ) &&
		  !TOP_is_change_rflags( OP_code(op) )) ||
		 (!TOP_is_change_rflags( OP_code(new_op) ) &&
		  TOP_is_change_rflags( OP_code(op) ))))
	      return FALSE;

	    if (alu_op_defines_rflags_used(alu_op, op))
	      return FALSE;

	    OP_srcpos( new_op ) = OP_srcpos( op );
	    if( EBO_Trace_Data_Flow ){
	      fprintf( TFile, "Lea_Insertion merges " );
	      Print_OP_No_SrcLine(op);
	      fprintf( TFile, "and " );
	      Print_OP_No_SrcLine(alu_op);
	      fprintf( TFile, "with " );
	      Print_OP_No_SrcLine(new_op);
	    }
	    BB_Remove_Op(OP_bb(alu_op), alu_op);
	  } else if (CG_fold_shiftadd && alu_op && alu_op->bb == op->bb && 
		     ((OP_code(alu_op) == TOP_leaxx64 && 
		       code == TOP_leax64) ||
		      (OP_code(alu_op) == TOP_leaxx32 && 
		       code == TOP_leax32)) &&
		     !TN_is_symbol(OP_opnd(alu_op, 2)) &&
		     TN_value(OP_opnd(alu_op, 2)) == 0 &&
		     TN_value(OP_opnd(op, 2)) == 1 &&
		     !TN_is_symbol(OP_opnd(op, 3)) &&
		     TN_value(OP_opnd(op, 3)) == 0 &&
		     !TN_live_out_of(OP_result(alu_op, 0), OP_bb(alu_op)) &&
		     ((op->next != NULL &&
		       !is_live_tn(op->next, OP_result(alu_op, 0))) ||
		      (op->next == NULL && 
		       !GTN_SET_MemberP(BB_live_out(OP_bb(op)), 
					OP_result(alu_op, 0)))) &&
		     Check_No_Use_Between(alu_op, op, OP_result(alu_op, 0))) {
	    // Fold leaxx into a following leax.
	    // Enabling this slows down crafty by 1 second but we would just 
	    // have the code around to see if it may help in other cases.
	    new_op = Mk_OP ((code == TOP_leax32)?TOP_leax32:TOP_leax64, 
			    OP_result(op, 0), OP_opnd(op, 0), 
			    OP_opnd(alu_op, 0), OP_opnd(alu_op, 1), 
			    Gen_Literal_TN(0, 4));	  	    
	    
	    if (alu_op_defines_rflags_used(alu_op, op))
	      return FALSE;

	    OP_srcpos( new_op ) = OP_srcpos( op );
	    if( EBO_Trace_Data_Flow ){
	      fprintf( TFile, "Lea_Insertion merges " );
	      Print_OP_No_SrcLine(op);
	      fprintf( TFile, "and " );
	      Print_OP_No_SrcLine(alu_op);
	      fprintf( TFile, "with " );
	      Print_OP_No_SrcLine(new_op);
	    }
	    BB_Remove_Op(OP_bb(alu_op), alu_op);
	  }
	}
	break;
      }	    

      if( ((TN_is_register(OP_opnd(op, 0)) &&
	    TNs_Are_Equivalent(OP_opnd(op, 0),OP_result(op, 0))) ||
	   (TN_is_register(OP_opnd(op, 1)) &&
	    TNs_Are_Equivalent(OP_opnd(op, 1),OP_result(op, 0)))) &&
	  TN_value(OP_opnd(op, 2)) == 1 &&
	  !TN_is_symbol(OP_opnd(op, 3)) &&
	  TN_value(OP_opnd(op, 3)) == 0) {
	TN *opnd1, *opnd2;
	if (TN_is_register(OP_opnd(op, 0)) &&
	    TNs_Are_Equivalent(OP_opnd(op, 0),OP_result(op, 0))) {
	  opnd1 = OP_opnd(op, 0);
	  opnd2 = OP_opnd(op, 1);
	} else {
	  opnd2 = OP_opnd(op, 0);
	  opnd1 = OP_opnd(op, 1);
	}
	new_op = Mk_OP ((code == TOP_leax32)?TOP_add32:TOP_add64,
			OP_result(op, 0), opnd1, opnd2);
      }

      if (rflags_read && new_op && 
	  ((TOP_is_change_rflags( OP_code(new_op) ) &&
	    !TOP_is_change_rflags( OP_code(op) )) ||
	   (!TOP_is_change_rflags( OP_code(new_op) ) &&
	    TOP_is_change_rflags( OP_code(op) ))))
	return FALSE;

      if (new_op) {
	if( EBO_Trace_Data_Flow ){
	  fprintf( TFile, "Lea_Insertion removes " );
	  Print_OP_No_SrcLine(op);
	  fprintf( TFile, "Lea_Insertion inserts " );
	  Print_OP_No_SrcLine(new_op);
	}
      }
      break;
    }
  case TOP_leaxx32:
  case TOP_leaxx64:
    {
      if( ((TN_is_register(OP_opnd(op, 0)) &&
	    TNs_Are_Equivalent(OP_opnd(op, 0),OP_result(op, 0))))) {
	if (!TN_is_symbol(OP_opnd(op, 2)) &&
	    TN_value(OP_opnd(op, 2)) == 0) {
	  shift = Get_Power_Of_2(TN_value(OP_opnd(op, 1)));
	  new_op = Mk_OP ((code == TOP_leaxx32)?TOP_shli32:TOP_shli64,
			  OP_result(op, 0), 
			  OP_result(op, 0), Gen_Literal_TN(shift, 4));
	} 
	else if (TN_is_symbol(OP_opnd(op, 2)) &&
		 TN_value(OP_opnd(op, 1)) == 1) {
	  new_op = Mk_OP ((code == TOP_leaxx32)?TOP_addi32:TOP_addi64,
			  OP_result(op, 0), 
			  OP_result(op, 0), OP_opnd(op, 2));
	}
      }

      if (rflags_read && new_op && 
	  ((TOP_is_change_rflags( OP_code(new_op) ) &&
	    !TOP_is_change_rflags( OP_code(op) )) ||
	   (!TOP_is_change_rflags( OP_code(new_op) ) &&
	    TOP_is_change_rflags( OP_code(op) ))))
	return FALSE;

      if (new_op) {
	if( EBO_Trace_Data_Flow ){
	  fprintf( TFile, "Lea_Insertion removes " );
	  Print_OP_No_SrcLine(op);
	  fprintf( TFile, "Lea_Insertion inserts " );
	  Print_OP_No_SrcLine(new_op);
	}
      }
      break;
    }
    
  default:
    break;
  }
  if (new_op) {
    OP_srcpos(new_op) = OP_srcpos(op);
    BB_Insert_Op_After(OP_bb(op), op, new_op);
    return TRUE;
  }
  
  return FALSE;
}

BOOL
EBO_Fold_Load_Duplicate( OP* op, TN** opnd_tn, EBO_TN_INFO** actual_tninfo )
{
#if Is_True_On
  if (!(EBO_Opt_Mask & EBO_FOLD_LOAD_DUPLICATE)) return FALSE;
#endif

  BOOL do_unpckhpd = FALSE;
  if (OP_code(op) != TOP_fmovddup && OP_code(op) != TOP_vmovddup)
    return FALSE;

  OP* shuf_op = actual_tninfo[0]->in_op;
  if (!shuf_op || shuf_op->bb != op->bb || 
      (OP_code(shuf_op) != TOP_shufpd && 
       OP_code(shuf_op) != TOP_ldhpd &&
       OP_code(shuf_op) != TOP_ldapd &&
       OP_code(shuf_op) != TOP_ldupd &&
       OP_code(shuf_op) != TOP_ldsd))
    return FALSE;

  if (!TOP_is_load(OP_code(shuf_op)) &&
      (TN_live_out_of(OP_result(shuf_op, 0), OP_bb(shuf_op)) ||
       ((op->next != NULL &&
	 is_live_tn(op->next, OP_result(shuf_op, 0))) ||
	(op->next == NULL && 
	 GTN_SET_MemberP(BB_live_out(OP_bb(op)), 
			 OP_result(shuf_op, 0)))) ||
       !Check_No_Use_Between(shuf_op, op, OP_result(shuf_op, 0))))
    return FALSE;

  // result of shuf_op should not be redefined between shuf_op and op
  EBO_TN_INFO *src0_info, *src1_info;
  src0_info = get_tn_info( OP_opnd(op, 0));
  src1_info = get_tn_info( OP_result(shuf_op, 0));
  if (src0_info->sequence_num > src1_info->sequence_num)
    return FALSE;
    
  if (!TOP_is_load(OP_code(shuf_op)) &&
      (!TN_is_register(OP_opnd(shuf_op, 0)) ||
       !TN_is_register(OP_opnd(shuf_op, 1)) ||
       !TNs_Are_Equivalent(OP_opnd(shuf_op, 0), OP_opnd(shuf_op, 1)) ||
       TN_is_symbol(OP_opnd(shuf_op, 2)) ||
       TN_value(OP_opnd(shuf_op, 2)) != 1))
    return FALSE;

  if (TOP_is_load(OP_code(shuf_op))) /* op uses result of a load */
    shuf_op = op;

  EBO_TN_INFO *loaded_tn_info = get_tn_info( OP_opnd(shuf_op, 0) );
  OP* load = loaded_tn_info->in_op;
  OP* new_op = NULL;
  if (!load || load->bb != shuf_op->bb || !TOP_is_load(OP_code(load)))
  {
    if(OP_code(shuf_op) == TOP_shufpd && 
       TNs_Are_Equivalent(OP_opnd(shuf_op, 1), OP_opnd(shuf_op, 0)))
    {
      do_unpckhpd = TRUE; 
      new_op = Mk_OP (TOP_unpckhpd, 
		    OP_result(op, 0), 
		    OP_opnd(shuf_op, 0), 
		    OP_opnd(shuf_op, 0)); 
    } else
    return FALSE;
  }

  if (!do_unpckhpd)
  {
    EBO_TN_INFO *src_info = get_tn_info( OP_result(load, 0) );
    if (loaded_tn_info->sequence_num > src_info->sequence_num)
      return FALSE;

    INT base_loc = OP_find_opnd_use( load, OU_base );
    INT offset_loc = OP_find_opnd_use( load, OU_offset );
    INT index_loc = OP_find_opnd_use( load, OU_index );
    INT scale_loc = OP_find_opnd_use( load, OU_scale );

    TN *base   = NULL;
    TN *offset   = NULL;
    TN *index   = NULL;
    TN *scale   = NULL;
    if (base_loc >= 0) 
      base = OP_opnd( load, OP_find_opnd_use( load, OU_base ) );
    if (offset_loc >= 0) 
      offset = OP_opnd( load, OP_find_opnd_use( load, OU_offset ) );
    if (index_loc >= 0) 
      index = OP_opnd( load, OP_find_opnd_use( load, OU_index ) );
    if (scale_loc >= 0) 
      scale = OP_opnd( load, OP_find_opnd_use( load, OU_scale ) );

    if (!offset || TN_is_symbol(offset))
      return FALSE;

    // base and index, if defined, should not be re-defined between
    // load and op.
    if (base_loc >= 0 && !Pred_Opnd_Avail(op, loaded_tn_info, base_loc))
      return FALSE;
    if (index_loc >= 0 && !Pred_Opnd_Avail(op, loaded_tn_info, index_loc))
      return FALSE;
    // for this bb, obtain the dependence graph so that we can
    // walk this op's expression tree to check dependencies.
    // Make sure that load operand is not overwritten
    // by a store operation before op using this dependency graph.

    CG_DEP_Compute_Graph ( op->bb,
                         NO_ASSIGNED_REG_DEPS,
                         NON_CYCLIC,
                         NO_MEMREAD_ARCS,
                         INCLUDE_MEMIN_ARCS,
                         NO_CONTROL_ARCS,
                         NULL);


    const INT hash_value = EBO_hash_op( load, NULL );
    EBO_OP_INFO* opinfo = EBO_opinfo_table[hash_value];

    while( opinfo != NULL ) {
      OP* next_op = opinfo->in_op;
      if( next_op == load ) break;
      if( next_op && OP_store( next_op ) ) {
        ARC_LIST  *arcs;
        for (arcs = OP_succs(load); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
           ARC *arc = ARC_LIST_first(arcs);
           if (ARC_kind(arc) != CG_DEP_MEMANTI) continue;
           OP *succ_op = ARC_succ(arc);
           if ((succ_op == next_op)  && OP_Precedes(next_op, op))  {
              CG_DEP_Delete_Graph (op->bb);
              return FALSE;
            }
         }
      }
      opinfo = opinfo->same;
    }
    CG_DEP_Delete_Graph (op->bb);

    TOP topcode;
    if (base && offset && index && scale) {
      new_op = Mk_OP (TOP_fmovddupxx, 
	  OP_result(op, 0), 
	  OP_opnd(load, 0), 
		    OP_opnd(load, 1), 
		    OP_opnd(load, 2), 
		    OP_opnd(load, 3));
    } else if (base && offset) {
      new_op = Mk_OP (TOP_fmovddupx, 
	  OP_result(op, 0), 
	  OP_opnd(load, 0), 
	  OP_opnd(load, 1));
    } else if (index && scale && offset) {
      new_op = Mk_OP (TOP_fmovddupxxx, 
	  OP_result(op, 0), 
	  OP_opnd(load, 0), 
	  OP_opnd(load, 1), 
	  OP_opnd(load, 2));
    }

    if ( op == shuf_op /* op uses result of a load */ &&
	TOP_is_vector_high_loadstore( OP_code( load ) ) ) {
      INT offset_loc = OP_find_opnd_use( new_op, OU_offset );
      INT offset_value = TN_value( OP_opnd( new_op, offset_loc ) );
      Set_OP_opnd( new_op, offset_loc, 
	  Gen_Literal_TN( offset_value - 8, 
	    TN_size( OP_opnd( new_op, offset_loc ) ) ) );    
    }
    else if ( op != shuf_op && !TOP_is_vector_high_loadstore( OP_code( load ) ) ) {
      INT offset_loc = OP_find_opnd_use( new_op, OU_offset );
      INT offset_value = TN_value( OP_opnd( new_op, offset_loc ) );
      Set_OP_opnd( new_op, offset_loc, 
		 Gen_Literal_TN( offset_value + 8, 
				 TN_size( OP_opnd( new_op, offset_loc ) ) ) );
    }
  }
  if (new_op) {
    if (shuf_op != op) {
      if( EBO_Trace_Data_Flow ){
	fprintf( TFile, "Fold_Load_Duplicate merges " );
	Print_OP_No_SrcLine(op);
	fprintf( TFile, "and " );
	Print_OP_No_SrcLine(shuf_op);
	fprintf( TFile, "Fold_Load_Duplicate inserts " );
	Print_OP_No_SrcLine(new_op);
      }
      OP_srcpos( new_op ) = OP_srcpos( op );
      BB_Remove_Op(OP_bb(shuf_op), shuf_op);
      BB_Insert_Op_After(OP_bb(op), op, new_op);
    } else {
      if( EBO_Trace_Data_Flow ){
	fprintf( TFile, "Fold_Load_Duplicate removes " );
	Print_OP_No_SrcLine(op);
	fprintf( TFile, "Fold_Load_Duplicate inserts " );
	Print_OP_No_SrcLine(new_op);
      }
      OP_srcpos( new_op ) = OP_srcpos( op );
      BB_Insert_Op_After(OP_bb(op), op, new_op);
    }    
    return TRUE;
  }

  return FALSE;
}

// Return TRUE if OP has no operands but can be eliminated if it is a
// duplicate.
BOOL
EBO_Can_Eliminate_Zero_Opnd_OP (OP *op)
{
  switch (OP_code(op)) {
    case TOP_zero32:
    case TOP_zero64:
    case TOP_xzero32:
    case TOP_xzero64:
    case TOP_xzero128v32:
    case TOP_xzero128v64:
      return TRUE;
  }
  return FALSE;
}
void expand_strcmp_bb(BB * call_bb) {
  int i;
  BB *bb;
  TOP loadc;
  int dont_expand = 0;
  BB *beg_bb, *diff_bb, *same_bb, *cmpz_bb, *cmpbyte2_bb, *incidx_bb, *old_succ;
  TN *beglb_tn, *difflb_tn, *oldsucclb_tn, *samelb_tn;
  TN *arg1,*stack_arg1,*stack_arg2,*char1of1, *cmp_res, *arg2, *cmpz_res, *char2of1, *char1of2,*result, *ret_reg,*char1of3;
  LABEL_IDX beglb, difflb, oldsucclb, samelb;
  OP *ld_arg1, *ld_byte1, *ld_byte3, *cmp_bytes, *ld_arg2, *jne, *cmpz_byte, *je, *ld_byte2,*ld1,*ld2;
  OP *inc_arg1, *inc_arg2, *reset_result, *jmp, *set_result, *op_iter;
  static int first_store = 1;

  Set_flags_strcmp_expand();
  is_str_expand = TRUE;
  /* assume that call to strcmp is the last op. This assumption is correct,
     but should we have an IsTrue and verify to make sure?
     verify that last op of BB is call to strcmp.
     can also be jmp */
  OP* last_op = BB_last_op(call_bb);
  Is_True(OP_code(last_op) == TOP_call, ("Last op of BB is not a call"));

  if (OP_code(last_op) == TOP_call) {
    BBLIST* old_ftsucc_list = BBlist_Fall_Thru_Succ(call_bb);
    if (old_ftsucc_list)
      old_succ = old_ftsucc_list->item;
  }

  TOP last_op_code = OP_code(last_op);
  if(last_op_code != TOP_call) {
    fprintf(stderr, "unexpected opcode\n");
  }

  BB* last_bb;
  for(last_bb = REGION_First_BB; BB_next(last_bb); last_bb = BB_next(last_bb))
    ;
  REGISTER rx,reg;
  ISA_REGISTER_CLASS rc;
  arg1 = 0;
  arg2 = 0;

  for (ld_arg1 = OP_prev(last_op); ld_arg1; ld_arg1 = OP_prev(ld_arg1)) {
      result = OP_result(ld_arg1, 0);
      if((OP_code(ld_arg1) == TOP_store32) && (first_store == 1))
      {
        TN *opnd = OP_opnd(ld_arg1, 0);
        arg2 = opnd;
        stack_arg2 = OP_opnd(ld_arg1, 1);
        first_store++;
      }
      else
      {
        if((OP_code(ld_arg1) == TOP_store32) && (first_store == 2))
        {
          TN *opnd = OP_opnd(ld_arg1, 0);
          arg1 = opnd;
          stack_arg1 = OP_opnd(ld_arg1, 1);
          first_store = 1;
          break;
        }
      }
    }
    /* Create the 6 blocks needed to expand strcmp.
     * Add them between the call block and its successor.
     * Chain them together by setting their prev/next fields.
     * Set up their pred/succ and fallthrough attributes
     */
  BB* mylist = Gen_BB_N (6);

  beg_bb = &mylist[0];
  beglb = Gen_Label_For_BB(beg_bb);
  beglb_tn = Gen_Label_TN(beglb, 0);

  diff_bb = &mylist[5];
  difflb = Gen_Label_For_BB(diff_bb);
  difflb_tn = Gen_Label_TN(difflb, 0);
  Unlink_Pred_Succ(call_bb, old_succ);
  Target_Simple_Fall_Through_BB(call_bb, &mylist[0]);

  for(i = 1; i < 6; i++){
    Insert_BB(&mylist[i], &mylist[i-1]);
    if (i != 5)
      Target_Simple_Fall_Through_BB(&mylist[i-1], &mylist[i]);
  }

  Target_Simple_Fall_Through_BB(&mylist[5], old_succ);
  Set_TN_is_global_reg(arg1); //This will make TN6 map to %rsi stay
  Set_TN_is_global_reg(arg2);
  Reset_TN_is_gra_homeable(arg1);
  Reset_TN_is_gra_homeable(arg2);
  Reset_TN_is_rematerializable(arg1);
  Reset_TN_is_rematerializable(arg2);
  Set_TN_home(arg1,NULL);
  Set_TN_home(arg2,NULL);
  Set_TN_is_global_reg(stack_arg1); //This will make TN6 map to %rsi stay
  Set_TN_is_global_reg(stack_arg2);
  char1of1 = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 4);
  Set_TN_is_global_reg(char1of1);
  char1of2 = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 4);
  Set_TN_is_global_reg(char1of2);
  char1of3 = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 1);
  Set_TN_is_global_reg(char1of3);
  ld_byte1 = Mk_OP(TOP_ld32,char1of1,arg1,Gen_Literal_TN(0, 4));

  BB_Append_Op(beg_bb,ld_byte1);


  cmp_res = Gen_Register_TN(ISA_REGISTER_CLASS_rflags, 8);
  Set_TN_is_global_reg(cmp_res);
  cmp_bytes = Mk_OP(TOP_cmpxxx8, cmp_res, char1of1, arg2,
                    Gen_Literal_TN(1, 8) /* scaling factor */,
                    Gen_Literal_TN(0,8));
  BB_Append_Op(beg_bb, cmp_bytes);
  jne = Mk_OP(TOP_jne, cmp_res, difflb_tn);
  BB_Append_Op(beg_bb, jne);
  oldsucclb = Gen_Label_For_BB(old_succ);
  oldsucclb_tn = Gen_Label_TN(oldsucclb, 0);
  Link_Pred_Succ(beg_bb, diff_bb);
  BB_Remove_Op(call_bb, last_op);
  /* remove the tag that says this is a call block */

  ANNOTATION *ant = ANNOT_Get(BB_annotations(call_bb), ANNOT_CALLINFO);
  BB_annotations(call_bb) = ANNOT_Unlink(BB_annotations(call_bb), ant);
  Reset_BB_call(call_bb);

  same_bb = &mylist[4];

  samelb = Gen_Label_For_BB(same_bb);
  samelb_tn = Gen_Label_TN(samelb, 0);

  cmpz_bb = &mylist[1];
  cmpz_res = cmp_res;
  cmpz_byte = Mk_OP(TOP_test8, cmpz_res, char1of1, char1of1);
  BB_Append_Op(cmpz_bb, cmpz_byte);
  je = Mk_OP(TOP_je, cmpz_res, samelb_tn);
  BB_Append_Op(cmpz_bb, je);
  Link_Pred_Succ(cmpz_bb, same_bb);
  cmpbyte2_bb = &mylist[2];
  char2of1 = char1of1;
  ld_byte2 = Mk_OP(TOP_ld8_32, char2of1, arg1, Gen_Literal_TN(1, 8));
  BB_Append_Op(cmpbyte2_bb, ld_byte2);
 cmp_bytes = Mk_OP(TOP_cmpxxx8, cmp_res, char2of1, arg2, Gen_Literal_TN(1, 8), Gen_Literal_TN(1,8));
  BB_Append_Op(cmpbyte2_bb, cmp_bytes);
  jne = Mk_OP(TOP_jne, cmp_res, difflb_tn);
  BB_Append_Op(cmpbyte2_bb, jne);
  Link_Pred_Succ(cmpbyte2_bb, diff_bb);


  incidx_bb = &mylist[3];
  TOP ptr_add = Is_Target_32bit() ? TOP_add32 : TOP_add64;
  inc_arg1 = Mk_OP(ptr_add, arg1, arg1, Gen_Literal_TN(2, 8));
  BB_Append_Op(incidx_bb, inc_arg1);
  inc_arg2 = Mk_OP(ptr_add, arg2, arg2, Gen_Literal_TN(2, 8));
  BB_Append_Op(incidx_bb, inc_arg2);

  cmpz_byte = Mk_OP(TOP_test8, cmpz_res, char1of1, char1of1);
  BB_Append_Op(incidx_bb, cmpz_byte);
  jne = Mk_OP(TOP_jne, cmpz_res, beglb_tn);
  BB_Append_Op(incidx_bb, jne);
  Link_Pred_Succ(incidx_bb, beg_bb);
  if(Is_Target_32bit()){
    loadc = TOP_zero32;
  }
  else
   loadc = TOP_zero64;
  result = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 8);
  Set_TN_is_global_reg(result);
  reset_result = Mk_OP(loadc, result, Gen_Literal_TN(0, 8));
  BB_Append_Op(same_bb, reset_result);
  jmp = Mk_OP(TOP_jmp, oldsucclb_tn);
  Link_Pred_Succ(same_bb, old_succ);

  set_result = Mk_OP(TOP_sbb32, result, result, result);
  BB_Append_Op(diff_bb, set_result);
  set_result = Mk_OP(TOP_ori32, result, result, Gen_Literal_TN(1, 8));
  BB_Append_Op(diff_bb, set_result);
  ld_byte1 = Mk_OP(TOP_ld32,arg1,stack_arg1/*arg1*/,Gen_Literal_TN(0, 4));
  ld_byte2 = Mk_OP(TOP_ld32,arg2,stack_arg2/*arg1*/,Gen_Literal_TN(4, 8));
  BB_Prepend_Op(old_succ,ld_byte1);
  BB_Prepend_Op(old_succ,ld_byte2);
  BB_Append_Op(same_bb, jmp);

  if (BB_preds_len(old_succ) == 2) {
    /* %eax in this block refers to result of strcmp,
       replace its use with the result register in all
       ops of old_succ till it is defined
     */
    int result_reg_def = 0;
    for (op_iter = BB_first_op(old_succ); (!result_reg_def && op_iter);
         op_iter = OP_next(op_iter)) {
      for (i = 0; i < OP_opnds(op_iter); i++) {
        TN *otn = OP_opnd(op_iter, i);
        if (TN_is_dedicated(otn) // the dedicated tag should be removed
            && (TN_register_and_class(otn) == CLASS_AND_REG_v0)) { //<-- int ret value

          Set_OP_opnd(op_iter, i, result);
        }
      }
      for (i = 0; i < OP_results(op_iter); i++) {
        TN *rtn = OP_result(op_iter, i);
        if (TN_is_dedicated(rtn)
            && (TN_register_and_class(rtn) == CLASS_AND_REG_v0)) {
          result_reg_def = 1;
          continue;
        }
      }
    }
  }
  else {
    /* %eax in this block could refer to result of call to another
       instruction in one of the other predecessor. In this case,
       put result of strcmp in %eax in bb #9 and #10.
     */
    /* this case is unlikely to happen. check with a test case. if so, place an assertion here.*/
    ret_reg = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 8);
    Set_TN_register_and_class(ret_reg, CLASS_AND_REG_v0);//missing a call in this or earlier block-maybe
    Set_TN_is_dedicated(ret_reg); // might not need this
    Set_TN_is_global_reg(ret_reg);
    TOP mov = Is_Target_32bit() ? TOP_mov32 : TOP_mov64;
    OP *copy_result = Mk_OP(mov, ret_reg, result);
   BB_Insert_Op(same_bb, jmp, copy_result, true);

    ret_reg = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 8);
    Set_TN_register_and_class(ret_reg, CLASS_AND_REG_v0);//missing a call in this or earlier block-maybe
    Set_TN_is_dedicated(ret_reg); // might not need this
    Set_TN_is_global_reg(ret_reg);
    mov = Is_Target_32bit() ? TOP_mov32 : TOP_mov64;
    copy_result = Mk_OP(mov, ret_reg, result);
    BB_Append_Op(diff_bb, copy_result);
  }
  GRA_LIVE_Recalc_Liveness(NULL);
}
