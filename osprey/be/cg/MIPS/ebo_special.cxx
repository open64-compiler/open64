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


/* =======================================================================
 * =======================================================================
 *
 *  Module: ebo_special.cxx
 *  $Revision: 1.1 $
 *  $Date: 2005/07/27 02:13:46 $
 *  $Author: kevinlo $
 *  $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/cg/MIPS/ebo_special.cxx,v $
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

#include "ebo.h"
#include "ebo_info.h"
#include "ebo_special.h"
#include "ebo_util.h"

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

/* Initialize and finalize ebo special routines. */
void
EBO_Special_Start (MEM_POOL *pool)
{
  st_initv_map = CXX_NEW(ST_TO_INITV_MAP(31, pool), pool);
  st_initv_map_inited = FALSE;
  work_gtn_set = GTN_SET_Create_Empty(Last_TN + 1, pool);
  work_defined_set = BS_Create_Empty(Last_TN + 1, pool);
  work_pool = pool;

  INT32 idummy;
  double ddummy;
  CGTARG_Compute_Branch_Parameters(&idummy, &fixed_branch_cost,
				   &taken_branch_cost, &ddummy);
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


/*
 * Identify OP's that contain a constant and operate in a way that
 * will allow the constant to be added into an offset field of
 * a load or store instruction.
 */
BOOL EBO_Can_Merge_Into_Offset (OP *op)
{
  TOP top = OP_code(op);
  
  if ((top != TOP_add) && (top != TOP_dadd) && 
      (top != TOP_addu) && (top != TOP_daddu) &&
      (top != TOP_addi) && (top != TOP_daddi) && 
      (top != TOP_addiu) && (top != TOP_daddiu) &&
      (top != TOP_sub) && (top != TOP_subu) &&
      (top != TOP_dsub) && (top != TOP_dsubu))
    return FALSE;

  if ((op == BB_entry_sp_adj_op(OP_bb(op))) ||
      (op == BB_exit_sp_adj_op(OP_bb(op))))
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
Expand_Extract_Bits (TYPE_ID rtype, TYPE_ID desc, UINT bit_offset, UINT bit_size,
                  TN *tgt_tn, TN *src_tn, OPS *ops)
{
  TN *tmp1_tn = tgt_tn;
  UINT pos = (Target_Byte_Sex != Host_Byte_Sex)
             ? MTYPE_bit_size(desc)-bit_offset-bit_size : bit_offset;
  if (pos == 0 && bit_size <= 16 && ! MTYPE_signed(rtype)) {
    Build_OP(TOP_andi, tgt_tn, src_tn,
             Gen_Literal_TN((1 << bit_size)-1, 4), ops);
    return;
  }
  ISA_REGISTER_CLASS rclass = ISA_REGISTER_CLASS_integer;
  INT reg_size = 
	  ISA_REGISTER_CLASS_INFO_Bit_Size(ISA_REGISTER_CLASS_Info(rclass));
#if defined(TARG_SL)
  TOP left_shift_op = TOP_sll;
#else
  TOP left_shift_op = TOP_dsll;
#endif
  INT left_shift_amt = reg_size - pos - bit_size;
  if (left_shift_amt > 31) {
#if defined(TARG_SL)
    Is_True(0,("shift > 31 bits"));
#endif
    left_shift_op = TOP_dsll32;
    left_shift_amt -= 32;
  }
  if (left_shift_amt >= 0)
    Build_OP(left_shift_op, tmp1_tn, src_tn, Gen_Literal_TN(left_shift_amt, 4),
	     ops);
#if defined(TARG_SL)
  TOP right_shift_op = TOP_sra;
#else
  TOP right_shift_op = TOP_dsra;
#endif
  INT right_shift_amt = reg_size - bit_size;
  if (right_shift_amt > 31) {
#if defined(TARG_SL)
    Is_True(0,("shift > 31 bits"));
#endif
    right_shift_op = TOP_dsra32;
    right_shift_amt -= 32;
  }
  if (! MTYPE_signed(rtype)) {
#if defined(TARG_SL)
    if (right_shift_op == TOP_sra)
      right_shift_op = TOP_srl;
    else
      right_shift_op = TOP_srl; // ???? SC
#else
    if (right_shift_op == TOP_dsra)
      right_shift_op = TOP_dsrl;
    else right_shift_op = TOP_dsrl32;
#endif
  }
  if (right_shift_amt >= 0)
    Build_OP(right_shift_op, tgt_tn, tmp1_tn, 
	     Gen_Literal_TN(right_shift_amt, 4),
	     ops);
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
    FmtAssert( FALSE, ("Handle this case"));
}

BOOL
delete_subset_mem_op(OP *op,
                     EBO_TN_INFO **opnd_tninfo,
		     EBO_OP_INFO *opinfo,
                     INT64 offset_pred,
                     INT64 offset_succ)
{
  OP *pred_op = opinfo->in_op;
  BB *bb = OP_bb(op);
  INT opcount = OP_opnds(op);
  TN *pred_result = OP_store(pred_op) 
                      ? OP_opnd(pred_op, TOP_Find_Operand_Use(OP_code(pred_op),OU_storeval))
                      : OP_result(pred_op,0);
  TN *succ_result = OP_store(op)
                      ? OP_opnd(op, TOP_Find_Operand_Use(OP_code(op),OU_storeval))
                      : OP_result(op,0);
  INT size_pred;
  INT size_succ;
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

  size_pred = CGTARG_Mem_Ref_Bytes(pred_op);
  size_succ = CGTARG_Mem_Ref_Bytes(op);
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

  if (EBO_Trace_Optimization) {
#pragma mips_frequency_hint NEVER
    fprintf(TFile,"%sReplace subset load with left/right shifts\n",EBO_trace_pfx);
  }   

  if (byte_offset > 8) return FALSE;

  if ((TN_register_class(pred_result) != ISA_REGISTER_CLASS_integer) ||
      (TN_register_class(succ_result) != ISA_REGISTER_CLASS_integer)) {
   /* Can only play games with integers. */
    return FALSE;
  }

  if (offset_succ == offset_pred &&
      size_pred == size_succ) {
    Build_OP(TOP_or, OP_result(op, 0), OP_opnd(pred_op, 0), Zero_TN, &ops);
    OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
    BB_Insert_Ops_After(OP_bb(op), op, &ops);
    return TRUE;
  }    

  INT bit_size = size_succ*8;
  INT succ_offset = (offset_succ - offset_pred);
  INT bit_offset = succ_offset*8;
  TYPE_ID rtype, desc;
  if (OP_code(pred_op) == TOP_sb)
    desc = MTYPE_I1;
  else if (OP_code(pred_op) == TOP_sh)
    desc = MTYPE_I2;
  else if (OP_code(pred_op) == TOP_sw)
    desc = MTYPE_I4;
  else 
    desc = MTYPE_I8;
  BOOL unsigned_op = (OP_code(op) == TOP_lbu ||
		      OP_code(op) == TOP_lhu ||
		      OP_code(op) == TOP_lwu);
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
                  EBO_TN_INFO **opnd_tninfo,
		  EBO_OP_INFO *opinfo)
{
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
       and that the new load's data can be extracted. */
    
    if ((size_pred != size_succ) ||
	(OP_results(op) != OP_results(opinfo->in_op)) ||
	(TN_size(OP_result(opinfo->in_op, 0)) != TN_size(OP_result(op, 0))))
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

    for (UINT i = 0; i < OP_results(op); i++)
      EBO_Exp_COPY(NULL, OP_result(op, i), OP_result(opinfo->in_op, i), &ops);
    
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
    INT storeval_idx = TOP_Find_Operand_Use(OP_code(opinfo->in_op),OU_storeval);
    /* The increment must be preserved. */
    if (storeval_idx < 0)
    {
      if (EBO_Trace_Optimization)
	fprintf(TFile,
		"%sStore value TN unknown for Load - Store combination\n",
		EBO_trace_pfx);
	  
      return FALSE;
    }

    TN *storeval_tn = OP_opnd(opinfo->in_op, storeval_idx);

    /* Make sure the storeval/result TNs' regclasses and mtypes
       match. It isn't sufficient to just check regclasses since
       user-defined operations for two ctypes in the same regfile can
       have different semantics. Make an exception for 32-bit
       loads/stores to the integer register file, since we know that
       they have the same semantics for both signed and unsigned. */

    if (TN_register_class(OP_result(op,0)) !=
	TN_register_class(storeval_tn))
    {
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

    /* If the size of the data moved to and from memory is the same,
       but the size of the stored value is larger than the size of
       the value we want to load, then mask off the upper portion of
       the stored value and use that instead of the loaded value. */
    if (size_pred == size_succ)
    {
      if (TN_size(storeval_tn) > size_succ)
      {
	if (EBO_Trace_Data_Flow)
	  fprintf(TFile,"%sSize mismatch for Store - Load combination: %d %d %d\n",
		  EBO_trace_pfx,size_pred,TN_size(storeval_tn),size_succ);

	return delete_subset_mem_op (op, opnd_tninfo, opinfo, 0, 0);
      }

      if (EBO_Trace_Optimization)
	fprintf(TFile,"%sRemove Store - Load combination\n",EBO_trace_pfx);

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
		     EBO_OP_INFO *opinfo)
{
  INT resnum;
  OPS ops = OPS_EMPTY;

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
    return delete_memory_op (op, opnd_tninfo, opinfo);
  }
  else
  {
    /* There is no easy way to copy FCC registers, so skip this optimization
     *  if the result is of register class FCC. */
    
    if (OP_results(op) == 1)
      if (TN_register_class(OP_result(op, 0)) == ISA_REGISTER_CLASS_fcc)
	return FALSE;

    /* Create copies of the result TN's. */

    for (resnum = 0; resnum < OP_results(op); resnum++) {
      EBO_Exp_COPY(NULL, OP_result(op, resnum), OP_result(opinfo->in_op, resnum), &ops);
    }

    if (EBO_in_loop)
      EBO_OPS_omega (&ops);

    BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
    return TRUE;
  }
  
  return FALSE;
}



/* Attempt to convert an add of 'tn' + 'imm_val' into an addi. Return
   TRUE if we succeed, FALSE otherwise. */
static BOOL
Convert_Imm_Add (OP *op, TN *tnr, TN *tn, INT64 imm_val, EBO_TN_INFO *tninfo)
{
  TOP top;
  OP *new_op = NULL;

  Is_True(TOP_is_iadd(OP_code(op)),
	  ("Convert_Imm_Add: unexpected opcode"));
  
  /* If 'imm_val' is 0, try a mov, otherwise try addi and addmi. */

  if (ISA_LC_Value_In_Class(imm_val, LC_simm16))
    top = TOP_is_iop64(OP_code(op)) ? TOP_daddiu : TOP_addiu;
  else
    return FALSE;

  if (EBO_Trace_Optimization)
    fprintf(TFile,"\treplace %s with %s\n", TOP_Name(OP_code(op)), TOP_Name(top));

  if (!new_op)
    new_op = Mk_OP(top, tnr, tn, Gen_Literal_TN(imm_val, TN_size(tnr)));

  OP_srcpos(new_op) = OP_srcpos(op);
  if (EBO_in_loop)
    EBO_Set_OP_omega(new_op, tninfo, NULL);

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
  TN *tnr = (OP_results(op) == 0) ? NULL : OP_result(op,0);

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
	    ("Unexpected constant/non-constant operands"));

  const INT64 imm_val = TN_Value(tn0);

  if (opcode == TOP_add)
    return Convert_Imm_Add(op, tnr, tn1, imm_val, opnd_tninfo[o1_idx]);

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
  TN *tnr = (OP_results(op) == 0) ? NULL : OP_result(op,0);

  /* Don't mess with symbols. */
  if (TN_is_symbol(tn1))
    return FALSE;

  /* We should only be called if tn1 is constant and tn0 is not. */
  FmtAssert(TN_Is_Constant(tn1) && ((OP_opnds(op) > 2) || !TN_Is_Constant(tn0)),
	    ("Unexpected constant/non-constant operands"));

  const INT64 imm_val = TN_Value(tn1);

  if (opcode == TOP_add) 
    return Convert_Imm_Add(op, tnr, tn0, imm_val, opnd_tninfo[o0_idx]);

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
  if (OP_iadd(op) && OP_iadd(pred_op))
  {
    INT ptn0_idx = TOP_Find_Operand_Use(pred_opcode,OU_opnd1);
    INT ptn1_idx = TOP_Find_Operand_Use(pred_opcode,OU_opnd2);
    TN *ptn0 = OP_opnd(pred_op, ptn0_idx);
    TN *ptn1 = OP_opnd(pred_op, ptn1_idx);

    if (TN_is_constant(ptn1) && !TN_is_symbol(ptn1))
    {
      EBO_OP_INFO *pred_opinfo = locate_opinfo_entry(opnd_tninfo[o0_idx]);
      EBO_TN_INFO *ptn0_tninfo = pred_opinfo->actual_opnd[ptn0_idx];

      if (EBO_tn_available(bb, ptn0_tninfo))
      {
	const INT64 new_val = imm_val + TN_Value(ptn1);
	if (Convert_Imm_Add(op, tnr, ptn0, new_val, ptn0_tninfo))
	{
	  if (EBO_Trace_Optimization)
	    fprintf(TFile,"\tcombine immediate adds\n");

	  return TRUE;
	}
      }
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
  BB *bb = OP_bb(op);

  /* We only concern ourselves with conditional branches, which we
     expect to have 2 successors, and a target. */
  if ((BBlist_Len(BB_succs(bb)) != 2)
      || (TOP_Find_Operand_Use(OP_code(op),OU_target) < 0))
    return FALSE;

  /* Evaluate the branch condition to determine if the branch is taken
     or not. */

  TN *tn0 = opnd_tn[0];
  INT64 tn0_val = TN_Value (tn0);
  UINT64 tn0_uval = TN_Value (tn0);

  TN *tn1;
  INT64 tn1_val;
  UINT64 tn1_uval;

  if (OP_opnds(op) > 1)
  {
    tn1 = opnd_tn[1];
    tn1_val = TN_Value (tn1);
    tn1_uval = TN_Value (tn1);
  }

  
  BOOL taken;
  TOP top = OP_code(op);

  if (top == TOP_beq) 
    taken = (tn0_uval == tn1_uval);
  else if (top == TOP_bgez)
    taken = (tn0_val >= 0);
  else if (top == TOP_bltz)
    taken = (tn0_val < 0);
  else if (top == TOP_bne) 
    taken = (tn0_uval != tn1_uval);
  else
    return FALSE;

  if (EBO_Trace_Optimization)
  {
    INT opndnum = OP_opnds(op);
    fprintf(TFile, "%sin BB:%d Resolve conditional BR :- %s ",
              EBO_trace_pfx, BB_id(bb),TOP_Name(OP_code(op)));
    for (UINT i = 0; i < opndnum; i++)
    {
      fprintf(TFile," ");
      Print_TN(opnd_tn[i],FALSE);
    }
    fprintf(TFile,"\n");
  }

  /* Find the fall-through and target blocks. If the branch is taken,
     replace the conditional branch with an unconditional jump to the
     target. If the branch is not-taken, delete it and
     fall-through. */
  
  BB *fall_bb;
  BB *branch_bb;

  fall_bb = BB_next(bb);
  branch_bb = BBLIST_item(BB_succs(bb));
  if (branch_bb == fall_bb)
    branch_bb = BBLIST_item(BBLIST_next(BB_succs(bb)));

  if (taken)
  {
    OPS ops = OPS_EMPTY;

    Build_OP (TOP_j, OP_opnd(op,TOP_Find_Operand_Use(OP_code(op),OU_target)), &ops);
    BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
    Unlink_Pred_Succ (bb, fall_bb);
    Change_Succ_Prob (bb, branch_bb, 1.0);
  }
  else
  {
    Unlink_Pred_Succ (bb, branch_bb);
    Change_Succ_Prob (bb, fall_bb, 1.0);
  }
  
  return TRUE;
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
  TOP opcode = OP_code(op);
  TN *tnr = OP_result(op,0);

  if (OP_opnds(op) == 0)
    return FALSE;

  /* Only attempt to do compile-time arithmetic on integers. */

  if (TN_register_class(tnr) != ISA_REGISTER_CLASS_integer)
    return FALSE;

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

  if (tn0 == Zero_TN || tn1 == Zero_TN)
    return FALSE; // instruction is for loading an immediate to register

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
    tn0_uval = TN_Value (tn0);
    tn0_val = TN_Value (tn0);
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
    tn1_uval = TN_Value (tn1);
    tn1_val = TN_Value (tn1);
  }

  /* All the rest of the operations have at most two operands. */
  
  if (OP_opnds(op) > 2)
    return FALSE;

  /* Addition... */

  if (TOP_is_iadd(opcode))
  {
    result_val = tn0_val + tn1_val;
    goto Constant_Created;
  }

  /* Subtraction... */

  if (OP_opnds(op) > 1 && !TN_is_symbol(tn1))
  {
    if (opcode == TOP_sub)
    {
      result_val = tn0_val - tn1_val;
      goto Constant_Created;
    }
  }

  /* Logical... */
  
  if (opcode == TOP_and)
  {
    result_val = tn0_uval & tn1_uval;
    goto Constant_Created;
  }
  else if (opcode == TOP_or)
  {
    result_val = tn0_uval | tn1_uval;
    goto Constant_Created;
  }
  else if (opcode == TOP_xor)
  {
    result_val = tn0_uval ^ tn1_uval;
    goto Constant_Created;
  }
    
  /* Shift... */

  if (opcode == TOP_sll)
  {
    result_val = TRUNC_32(tn0_uval << tn1_uval);
    goto Constant_Created;
  }
  else if (opcode == TOP_sra)
  {
    result_val = tn0_val >> tn1_uval;
    goto Constant_Created;
  }
  else if (opcode == TOP_srl)
  {
    result_val = TRUNC_32(tn0_val) >> tn1_uval;
    goto Constant_Created;
  }

  return FALSE;

  /* We've evaluated the expression, so replace it with the result. */

  Constant_Created:

  OPS ops = OPS_EMPTY;
  TN *tnc;

  if (result_sym != NULL)
  {
    tnc = Gen_Symbol_TN(result_sym, result_val, result_relocs);
  }
  else
  {
    tnc = Gen_Literal_TN(result_val, TN_size(tnr));
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


#define SWAP_TN(a, b) (_tn_swap_tmp = a, a = b, b = _tn_swap_tmp)
#define SWAP_TNINFO(a, b) (_tninfo_swap_tmp = a, a = b, b = _tninfo_swap_tmp)
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
  if (top != TOP_sll)
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
imm_sra (EBO_OP_INFO *sra_opinfo, INT32 *shift =NULL)
{
  if (!sra_opinfo)
    return NULL;
  
  OP *sra = sra_opinfo->in_op;
  TOP top = OP_code(sra);
  if (top != TOP_sra)
    return NULL;

  TN *imm = OP_opnd(sra, 1);
  if (!TN_has_value(imm))
    return NULL;

  if (shift)
    *shift = TN_value(imm);
  
  return sra_opinfo;
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
  if (top != TOP_srl)
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
  if (top != TOP_and)
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
    if (topcode == TOP_add || topcode == TOP_dadd ||
        topcode == TOP_addu || topcode == TOP_daddu ||
        topcode == TOP_or || 
        topcode == TOP_mov_s || topcode== TOP_mov_d)
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
  if (jump && ((*len == 1) || (OP_code(jump) != TOP_j)))
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
	if (past_current_op)
	  return FALSE;
	else
	  is_live = TRUE;
      }
    }
  }
  Is_True(past_current_op && is_live, ("Bad call to is_live_tn"));
  return GTN_SET_MemberP(BB_live_out(OP_bb(current_op)), current_tn);
}



/*
 * Look at an expression and it's inputs to identify special sequences
 * that can be simplified.
 */
BOOL Special_Sequence (OP *op,
                       TN **opnd_tn,
                       EBO_TN_INFO **opnd_tninfo)
{  
  if (!EBO_in_peep)
    return FALSE;
  if (OP_code(op) == TOP_beq ||
      OP_code(op) == TOP_bne)  {
    INT64 val = 0;
    TN *opnd0 = OP_opnd(op, 0);
    TN *opnd1 = OP_opnd(op, 1);
    EBO_TN_INFO *opnd1_tninfo = get_tn_info(opnd1);
    if (opnd1_tninfo &&
	EBO_tn_available(OP_bb(op), opnd1_tninfo) &&
	opnd1_tninfo->in_op &&
	(OP_code(opnd1_tninfo->in_op) == TOP_addiu)) {
      OP *op_in = opnd1_tninfo->in_op;
      if (TN_has_value(OP_opnd(op_in, 1)) && 
	  (TN_value(OP_opnd(op_in, 1)) != -1))
	return FALSE;
      else if (TN_has_value(OP_opnd(op_in, 1)))
	val = -1;
      else
	return FALSE;
    } else if (opnd1 != Zero_TN) {      
      return FALSE;
    }

    // Try folding shift+beq/bne to bgez/bltz
    EBO_TN_INFO *opnd0_tninfo = get_tn_info(opnd0);
    if (opnd0_tninfo &&
	EBO_tn_available(OP_bb(op), opnd0_tninfo) &&
	opnd0_tninfo->in_op &&
	(OP_code(opnd0_tninfo->in_op) == TOP_sra || 
	 OP_code(opnd0_tninfo->in_op) == TOP_dsra || 
	 OP_code(opnd0_tninfo->in_op) == TOP_dsra32)) {
      EBO_TN_INFO *tninfo;
      OP *shift_op = opnd0_tninfo->in_op;

      // Quit if result of the shift is live out of this BB
      TN *shift_res = OP_result(shift_op, 0);
      if (EBO_in_peep) {
	if (REG_LIVE_Outof_BB (TN_register_class(shift_res), 
			       TN_register(shift_res), 
			       OP_bb(shift_op)))
	  return FALSE;
      } else {
	if (CG_localize_tns && (TN_is_dedicated(shift_res) || 
				TN_is_global_reg(shift_res)))
	  return FALSE;
	else if (GRA_LIVE_TN_Live_Outof_BB (shift_res, OP_bb(shift_op)))
	  return FALSE;
      }

      // Find the control operand for the "shift".
      TN *shift_ctrl_opnd = OP_opnd(shift_op, 1);
      FmtAssert(TN_has_value(shift_ctrl_opnd), 
		 ("shift ctrl opnd is not a constant"));
      switch(shift_op->opr) {
      case TOP_sra:
	if (TN_value(shift_ctrl_opnd) != 31)
	  return FALSE;
	break;
      case TOP_dsra32:
	if ((TN_value(shift_ctrl_opnd) % 32) != 31)
	  return FALSE;
	break;
      case TOP_dsra:
	if (TN_value(shift_ctrl_opnd) == 63)
	  FmtAssert(FALSE, ("shift of 63 with dsra encountered"));
	break;	
      }

      OP *new_op;
      OPS ops = OPS_EMPTY;
      if (OP_code(op) == TOP_bne)
	val = (val == 0)?-1:0;
      new_op = Mk_OP((val==0)?TOP_bgez:TOP_bltz, 
		     OP_opnd(shift_op, 0), OP_opnd(op, 2));
      OP_srcpos(new_op) = OP_srcpos(op);
      OPS_Append_Op(&ops, new_op);
      BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
      OP_Change_To_Noop(op);
      OP_Change_To_Noop(shift_op);
      if (EBO_Trace_Optimization) {
	fprintf(TFile, "shift and beq combined to produce bgez.\n");
      }
      return TRUE;
    }    
  }

  TOP top = OP_code(op);
  
#ifdef Is_True_On
  if (EBO_Trace_Optimization)
  {
    fprintf(TFile, "OP: ");
    Print_OP_No_SrcLine(op);
    for (UINT i = 0; i < OP_opnds(op); i++)
    {
      fprintf(TFile, "  local tn: ");
      Print_TN(OP_opnd(op, i), TRUE);
      fprintf(TFile, "\n  ebo tn: ");
      Print_TN(opnd_tn[i], TRUE);
      fprintf(TFile, "\n   local in_op: ");
      fprintf(TFile, "<null>\n");
      fprintf(TFile, "   ebo in_op: ");
      if (opnd_tninfo[i] && opnd_tninfo[i]->in_op)
	Print_OP_No_SrcLine(opnd_tninfo[i]->in_op);
      else
	fprintf(TFile, "<null>\n");

      if (opnd_tninfo[i] && opnd_tninfo[i]->replacement_tn)
      {
	fprintf(TFile, "  replace tn: ");
	Print_TN(opnd_tninfo[i]->replacement_tn, TRUE);
	fprintf(TFile, "\n   replace in_op: ");
	if (opnd_tninfo[i]->replacement_tninfo &&
	    opnd_tninfo[i]->replacement_tninfo->in_op)
	  Print_OP_No_SrcLine(opnd_tninfo[i]->replacement_tninfo->in_op);
	else
	  fprintf(TFile, "<null>\n");
      }
    }
  }
#endif

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
  for( BB* bb = REGION_First_BB; bb != NULL; bb = BB_next( bb ) ){
    if( BB_rid(bb) && (RID_level(BB_rid(bb)) >= RL_CGSCHED) ){
      // don't change bb's which have already been through CG
      continue;
    }
    if (CG_skip_local_ebo &&
        ((BB_id(bb) < CG_local_skip_before) ||
         (BB_id(bb) > CG_local_skip_after)  ||
         (BB_id(bb) == CG_local_skip_equal)))
      continue;
    for( OP* op = BB_first_op( bb ); op != NULL; op = OP_next( op ) ){
      BOOL done = FALSE;
      INT copy_operand = CGTARG_Copy_Operand(op);
      if (copy_operand >= 0) {
	// Other ops could be rendered uselesss by the shift removal.
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
		   ("src/result not registers in EBO_in_peep"));
	
	// do not delete assignment to result if result is live out
	if (REG_LIVE_Outof_BB (TN_register_class(result), 
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

	/* SPECIAL_CASE:
	 * In the following scenario, the copy_op can not be deleted 
	 *    a = b   << copy_op >>
	 *      = a
	 *    b =     << redef >>
	 *      = a
	 * NOTE: If redef is a copy op and copy operand is 'a'
	 *       then copy_op can be removed
	 *       In this special_case, we have to look beyond redef,
	 *       and, delete all uses of 'a' until 'a' is re-defined.
	 */
	BOOL cannot_be_removed = FALSE;
	BOOL special_case = FALSE;
	for( OP* dw_op = OP_next( op ); dw_op != NULL; 
	     dw_op = OP_next( dw_op ) ){
	  if ((OP_code(dw_op) != TOP_asm) && 
	      OP_results(dw_op) &&
	      TN_is_register( OP_result( dw_op, 0)) &&
	      TNs_Are_Equivalent( OP_result( dw_op, 0), src)) {
	    
	    // see NOTE above
	    INT dw_op_copy_operand = CGTARG_Copy_Operand(dw_op);
	    TN *dw_op_copy_operand_tn = 
	      (dw_op_copy_operand>=0)?OP_opnd(dw_op, dw_op_copy_operand):NULL;
	    if ((dw_op_copy_operand >= 0) && 
		TN_is_register( dw_op_copy_operand_tn) &&
		TNs_Are_Equivalent(dw_op_copy_operand_tn, result)) {
	      special_case = TRUE;
	      break;
	    }
	    
	    // search if 'result' is being used after dw_op
	    for( OP* dw1_op = OP_next( dw_op ); dw1_op != NULL; 
		 dw1_op = OP_next( dw1_op ) ){
#ifdef TARG_SL2 
     /*   
       *   [1] TN20($5) = TN16($6) TN1($0) ; copy
       *   [2]               = TN20($5)  GTN30($sp) (sym:satd_acc_5+0) ;
       *   [3] TN17($6) = 
       *   [4] TN22($5) =
       *   [5] TN19($6) = OP TN18($6) TN22($5)
       */      
             BOOL redef = FALSE;
             for(int i = 0; i < OP_results(dw1_op); i++) {
               if(TN_is_register(OP_result(dw1_op, i)) && 
		      TNs_Are_Equivalent(OP_result(dw1_op, i), result)) {
                        redef = TRUE;
			   cannot_be_removed=TRUE;
			   break; //redefined = true;
               }			   
	      }
	      if(redef)
		  break;
#endif 
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
#ifdef TARG_SL2	  
         if(OP_same_res(dw_op) || (OP_code(dw_op) == TOP_c2_satd || 
	      OP_code(dw_op) == TOP_c2_sad) || OP_code(dw_op) == TOP_c2_muls) {
	     for (INT i = 0; i < OP_results(dw_op); i++) 
              if(TNs_Are_Equivalent(result,  OP_result(dw_op, i)))  {
	           cannot_be_removed = TRUE;
		    break;
              }			
         }
#endif 
	}
	if (cannot_be_removed)
	  continue;	

	/* Traverse downwards; replace result with src */

	for( OP* dw_op = OP_next( op ); dw_op != NULL; 
	     dw_op = OP_next( dw_op ) ){

	  for( int i = 0; i < OP_opnds( dw_op ); i++ ){
	    if ( TN_is_register( OP_opnd( dw_op, i)) && 
		 TNs_Are_Equivalent( OP_opnd( dw_op, i), result)) {
	      Set_OP_opnd( dw_op, i, src );
	      done = TRUE;
	    }
	  }
/* need check instruction with two results */ 
#ifdef TARG_SL2 
	  if( OP_results( dw_op ) > 0 ){
#else 
	  if( OP_results( dw_op ) == 1 ){
#endif 	  	

#ifdef TARG_SL2 
	    TN* tnr = OP_result( dw_op, 0 );
           if(OP_code(dw_op) == TOP_c2_sad || OP_code(dw_op) == TOP_c2_satd ||
            OP_code(dw_op) == TOP_c2_muls)
             tnr = OP_result(dw_op, 1);
#else 
	    TN* tnr = OP_result( dw_op, 0 );
#endif 

	    if( TN_is_register( tnr) && 
		( TNs_Are_Equivalent( tnr, result ) || 
		  TNs_Are_Equivalent( tnr, src ) )) {
	      if (special_case) {
		if (TNs_Are_Equivalent ( tnr, result ))
		  break;
	      } else {
		break;
	      }
	    }
	  }
	}      

	if( done ){
	  OP* dead = op;
	  op = OP_prev( op );
	  
	  if( EBO_Trace_Optimization ){
	    fprintf( TFile, 
		     "Redundancy_Elimination removes simplified op - " );
	    Print_OP_No_SrcLine( dead );
	  }
	  
	  BB_Remove_Op( bb, dead );
	  
	  if( op == NULL )
	    op = BB_first_op( bb );
	}       

	continue; // end for this op
      }

      if (OP_code( op ) != TOP_srl && 
	  OP_code( op ) != TOP_sra && 
	  OP_code( op ) != TOP_sll && 
	  OP_code( op ) != TOP_dsrl && 
	  OP_code( op ) != TOP_dsra && 
	  OP_code( op ) != TOP_dsll && 
	  OP_code( op ) != TOP_dsll32 && 
	  OP_code( op ) != TOP_dsra32 && 
	  OP_code( op ) != TOP_dsrl32)
	continue;

      /* The whole point is to get rid of result. */

      TN* src = OP_opnd( op, 0 );
      TN* result = OP_result( op, 0 );

      FmtAssert( TN_is_register( src ) && TN_is_register( result ), 
		 ("src/result not registers in EBO_in_peep"));
      
      if (OP_opnd( op, 0) != Zero_TN)
	continue;

      // do not delete assignment to result if result is live out
      if (REG_LIVE_Outof_BB (TN_register_class(result), 
			     TN_register(result), bb)) {
	// see if the register is re-defined before block end
	// we can still delete the op if there is a re-definition
	BOOL redefined = FALSE;
	for( OP* dw_op = OP_next( op ); dw_op != NULL; 
	     dw_op = OP_next( dw_op ) ){
	  if (OP_results(dw_op) &&
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

      for( OP* dw_op = OP_next( op ); dw_op != NULL; 
	   dw_op = OP_next( dw_op ) ){
	for( int i = 0; i < OP_opnds( dw_op ); i++ ){
	  if( TN_is_register(OP_opnd( dw_op, i)) && 
	      (TN_register_class(OP_opnd(dw_op, i)) == 
	       TN_register_class(result)) &&
	      (TN_register(OP_opnd( dw_op, i )) == TN_register(result))) {
	    Set_OP_opnd( dw_op, i, Zero_TN );
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

