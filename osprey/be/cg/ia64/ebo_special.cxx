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
 *  $Revision: 1.1.1.1 $
 *  $Date: 2005/10/21 19:00:00 $
 *  $Author: marcel $
 *  $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/ia64/ebo_special.cxx,v $
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
#include "cxx_hash.h"

#include "ebo.h"
#include "ebo_info.h"
#include "ebo_special.h"
#include "ebo_util.h"
#include "region_bb_util.h"
#include "ipfec_options.h"
/* Define a macro to strip off any bits outside of the left most 4 bytes. */
#define TRUNC_32(val) (val & 0x00000000ffffffffll)

/* Define a macro to sign-extend the least signficant 32 bits */
#define SEXT_32(val) (((INT64)(val) << 32) >> 32)

typedef HASH_TABLE<ST_IDX, INITV_IDX> ST_TO_INITV_MAP;
static ST_TO_INITV_MAP *st_initv_map = NULL;
static BOOL st_initv_map_inited = FALSE;
static GTN_SET *work_gtn_set = NULL;
static BS *work_defined_set = NULL;
static MEM_POOL *work_pool = NULL;

/* ===================================================================== */

/* Initialize and finalize ebo special routines. */


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
  TN *tn;

  if (!OP_iadd(op) && !OP_isub(op) && !OP_ior(op)) {
    return FALSE;
  }
  if ((op == BB_entry_sp_adj_op(OP_bb(op))) ||
      (op == BB_exit_sp_adj_op(OP_bb(op)))) {
     return FALSE;
  }
  tn = OP_opnd(op,1);
  if ((OP_iadd(op) || OP_ior(op)) && TN_Is_Constant(tn)) {
    return TRUE;
  }
  tn = OP_opnd(op,2);
  if ((OP_iadd(op) || OP_ior(op)) && TN_Is_Constant(tn)) {
    return TRUE;
  }
  return FALSE;
}

 


/*
 * Function: EBO_bit_length
 *
 * Determine the length of significant bits.
 *
 */
static
INT
EBO_bit_length (OP *op)
{
  switch (OP_code(op))
  {
    case TOP_dep:
     { TN *length_tn = OP_opnd(op,4);
       if (TN_has_value(length_tn)) return TN_value(length_tn);
       return -1;
     }
    case TOP_dep_z:
    case TOP_extr:
    case TOP_extr_u:
     { TN *length_tn = OP_opnd(op,3);
       if (TN_has_value(length_tn)) return TN_value(length_tn);
       return -1;
     }
    case TOP_shr_i:
    case TOP_shr_i_u:
     { TN *length_tn = OP_opnd(op,2);
       if (TN_has_value(length_tn)) return (64-TN_value(length_tn));
       return -1;
     }
    case TOP_ld1:
    case TOP_ld1_i:
    case TOP_st1:
    case TOP_st1_i:
    case TOP_sxt1:
    case TOP_zxt1:
      return 8;
    case TOP_ld2:
    case TOP_ld2_i:
    case TOP_st2:
    case TOP_st2_i:
    case TOP_sxt2:
    case TOP_zxt2:
      return 16;
    case TOP_cmp4_eq:
    case TOP_cmp4_eq_unc:
    case TOP_cmp4_eq_and:
    case TOP_cmp4_eq_or:
    case TOP_cmp4_eq_or_andcm:
    case TOP_cmp4_ne_and:
    case TOP_cmp4_ne_or:
    case TOP_cmp4_ne_or_andcm:
    case TOP_cmp4_lt:
    case TOP_cmp4_lt_unc:
    case TOP_cmp4_ltu:
    case TOP_cmp4_ltu_unc:
    case TOP_cmp4_z1_lt_and:
    case TOP_cmp4_z1_lt_or:
    case TOP_cmp4_z1_lt_or_andcm:
    case TOP_cmp4_z1_le_and:
    case TOP_cmp4_z1_le_or:
    case TOP_cmp4_z1_le_or_andcm:
    case TOP_cmp4_z1_gt_and:
    case TOP_cmp4_z1_gt_or:
    case TOP_cmp4_z1_gt_or_andcm:
    case TOP_cmp4_z1_ge_and:
    case TOP_cmp4_z1_ge_or:
    case TOP_cmp4_z1_ge_or_andcm:
    case TOP_cmp4_i_eq:
    case TOP_cmp4_i_eq_unc:
    case TOP_cmp4_i_eq_and:
    case TOP_cmp4_i_eq_or:
    case TOP_cmp4_i_eq_or_andcm:
    case TOP_cmp4_i_ne_and:
    case TOP_cmp4_i_ne_or:
    case TOP_cmp4_i_ne_or_andcm:
    case TOP_cmp4_i_lt:
    case TOP_cmp4_i_lt_unc:
    case TOP_cmp4_i_ltu:
    case TOP_cmp4_i_ltu_unc:
    case TOP_cmp4_eq_orcm:
    case TOP_cmp4_eq_andcm:
    case TOP_cmp4_eq_and_orcm:
    case TOP_cmp4_ne:
    case TOP_cmp4_ne_unc:
    case TOP_cmp4_ne_orcm:
    case TOP_cmp4_ne_andcm:
    case TOP_cmp4_ne_and_orcm:
    case TOP_cmp4_le:
    case TOP_cmp4_le_unc:
    case TOP_cmp4_gt:
    case TOP_cmp4_gt_unc:
    case TOP_cmp4_ge:
    case TOP_cmp4_ge_unc:
    case TOP_cmp4_leu:
    case TOP_cmp4_leu_unc:
    case TOP_cmp4_gtu:
    case TOP_cmp4_gtu_unc:
    case TOP_cmp4_geu:
    case TOP_cmp4_geu_unc:
    case TOP_cmp4_z1_lt_orcm:
    case TOP_cmp4_z1_lt_andcm:
    case TOP_cmp4_z1_lt_and_orcm:
    case TOP_cmp4_z1_le_orcm:
    case TOP_cmp4_z1_le_andcm:
    case TOP_cmp4_z1_le_and_orcm:
    case TOP_cmp4_z1_gt_orcm:
    case TOP_cmp4_z1_gt_andcm:
    case TOP_cmp4_z1_gt_and_orcm:
    case TOP_cmp4_z1_ge_orcm:
    case TOP_cmp4_z1_ge_andcm:
    case TOP_cmp4_z1_ge_and_orcm:
    case TOP_cmp4_z2_lt_orcm:
    case TOP_cmp4_z2_lt_andcm:
    case TOP_cmp4_z2_lt_and_orcm:
    case TOP_cmp4_z2_le_orcm:
    case TOP_cmp4_z2_le_andcm:
    case TOP_cmp4_z2_le_and_orcm:
    case TOP_cmp4_z2_gt_orcm:
    case TOP_cmp4_z2_gt_andcm:
    case TOP_cmp4_z2_gt_and_orcm:
    case TOP_cmp4_z2_ge_orcm:
    case TOP_cmp4_z2_ge_andcm:
    case TOP_cmp4_z2_ge_and_orcm:
    case TOP_cmp4_z2_lt_and:
    case TOP_cmp4_z2_lt_or:
    case TOP_cmp4_z2_lt_or_andcm:
    case TOP_cmp4_z2_le_and:
    case TOP_cmp4_z2_le_or:
    case TOP_cmp4_z2_le_or_andcm:
    case TOP_cmp4_z2_gt_and:
    case TOP_cmp4_z2_gt_or:
    case TOP_cmp4_z2_gt_or_andcm:
    case TOP_cmp4_z2_ge_and:
    case TOP_cmp4_z2_ge_or:
    case TOP_cmp4_z2_ge_or_andcm:
    case TOP_cmp4_i_eq_orcm:
    case TOP_cmp4_i_eq_andcm:
    case TOP_cmp4_i_eq_and_orcm:
    case TOP_cmp4_i_ne:
    case TOP_cmp4_i_ne_unc:
    case TOP_cmp4_i_ne_orcm:
    case TOP_cmp4_i_ne_andcm:
    case TOP_cmp4_i_ne_and_orcm:
    case TOP_cmp4_i_le:
    case TOP_cmp4_i_le_unc:
    case TOP_cmp4_i_gt:
    case TOP_cmp4_i_gt_unc:
    case TOP_cmp4_i_ge:
    case TOP_cmp4_i_ge_unc:
    case TOP_cmp4_i_leu:
    case TOP_cmp4_i_leu_unc:
    case TOP_cmp4_i_gtu:
    case TOP_cmp4_i_gtu_unc:
    case TOP_cmp4_i_geu:
    case TOP_cmp4_i_geu_unc:
    case TOP_cmp4_lt_and:
    case TOP_cmp4_lt_or:
    case TOP_cmp4_lt_or_andcm:
    case TOP_cmp4_le_and:
    case TOP_cmp4_le_or:
    case TOP_cmp4_le_or_andcm:
    case TOP_cmp4_gt_and:
    case TOP_cmp4_gt_or:
    case TOP_cmp4_gt_or_andcm:
    case TOP_cmp4_ge_and:
    case TOP_cmp4_ge_or:
    case TOP_cmp4_ge_or_andcm:
    case TOP_cmp4_lt_orcm:
    case TOP_cmp4_lt_andcm:
    case TOP_cmp4_lt_and_orcm:
    case TOP_cmp4_le_orcm:
    case TOP_cmp4_le_andcm:
    case TOP_cmp4_le_and_orcm:
    case TOP_cmp4_gt_orcm:
    case TOP_cmp4_gt_andcm:
    case TOP_cmp4_gt_and_orcm:
    case TOP_cmp4_ge_orcm:
    case TOP_cmp4_ge_andcm:
    case TOP_cmp4_ge_and_orcm:
    case TOP_ld4:
    case TOP_ld4_i:
    case TOP_st4:
    case TOP_st4_i:
    case TOP_sxt4:
    case TOP_zxt4:
      return 32;
    case TOP_and_i: {

      if (TN_has_value(OP_opnd(op,1))) {
       /* Only handle right justified masks, for now. */

        switch (TN_value(OP_opnd(op,1))) {
        case 1: return 1;
        case 3: return 2;
        case 7: return 3;
        case 15: return 4;
        case 31: return 5;
        case 63: return 6;
        case 127: return 7;
        case 255: return 64; /*it's sign extended */
        default: return -1;
        }

      }

    }
  }

  return -1;
}
 



static
INT
Get_Right_Mask_Length (UINT64 mask)
/*
 *  Determine the number of consecutive right
 *  justified bits that are in the mask.
 *  Return -1, if the constant is not a
 *  properly constructed mask.
 */
{
  INT64 i;

  if (mask == 0) return -1;
  if ((mask & 1) == 0) return -1;

  for ( i = 1; i < 64; i++)
  {
    if (((mask >> i) & 1) == 0) return ((mask >> i) == 0) ? i : -1;
  }

  return -1;
}



static
INT
Get_Mask_Shift_Count (UINT64 mask)
/*
 *  Determine the number of trailing zeros after a
 *  mask of all "1" bits.  Return -1, if the constant
 *  is not a properly constructed mask.
 */
{
  INT64 i;

  if (mask == 0) return -1;

  for ( i = 0; i < 63; i++)
  {
    if ((mask & (1 << i)) != 0) return i;
  }

  return -1;
}




static
BOOL
TOP_Is_Unconditional_Compare (TOP opcode)
{
  return ((opcode == TOP_tnat_nz_unc)    ||
          (opcode == TOP_tnat_z_unc)     ||
          (opcode == TOP_cmp_eq_unc)     ||
          (opcode == TOP_cmp_ge_unc)     ||
          (opcode == TOP_cmp_geu_unc)    ||
          (opcode == TOP_cmp_gt_unc)     ||
          (opcode == TOP_cmp_gtu_unc)    ||
          (opcode == TOP_cmp_le_unc)     ||
          (opcode == TOP_cmp_leu_unc)    ||
          (opcode == TOP_cmp_lt_unc)     ||
          (opcode == TOP_cmp_ltu_unc)    ||
          (opcode == TOP_cmp_ne_unc)     ||
          (opcode == TOP_cmp4_eq_unc)    ||
          (opcode == TOP_cmp4_ge_unc)    ||
          (opcode == TOP_cmp4_geu_unc)   ||
          (opcode == TOP_cmp4_gt_unc)    ||
          (opcode == TOP_cmp4_gtu_unc)   ||
          (opcode == TOP_cmp4_le_unc)    ||
          (opcode == TOP_cmp4_leu_unc)   ||
          (opcode == TOP_cmp4_lt_unc)    ||
          (opcode == TOP_cmp4_ltu_unc)   ||
          (opcode == TOP_cmp4_ne_unc)    ||
          (opcode == TOP_cmp4_i_eq_unc)  ||
          (opcode == TOP_cmp4_i_ge_unc)  ||
          (opcode == TOP_cmp4_i_geu_unc) ||
          (opcode == TOP_cmp4_i_gt_unc)  ||
          (opcode == TOP_cmp4_i_gtu_unc) ||
          (opcode == TOP_cmp4_i_le_unc)  ||
          (opcode == TOP_cmp4_i_leu_unc) ||
          (opcode == TOP_cmp4_i_lt_unc)  ||
          (opcode == TOP_cmp4_i_ltu_unc) ||
          (opcode == TOP_cmp4_i_ne_unc)  ||
          (opcode == TOP_cmp_i_eq_unc)   ||
          (opcode == TOP_cmp_i_ge_unc)   ||
          (opcode == TOP_cmp_i_geu_unc)  ||
          (opcode == TOP_cmp_i_gt_unc)   ||
          (opcode == TOP_cmp_i_gtu_unc)  ||
          (opcode == TOP_cmp_i_le_unc)   ||
          (opcode == TOP_cmp_i_leu_unc)  ||
          (opcode == TOP_cmp_i_lt_unc)   ||
          (opcode == TOP_cmp_i_ltu_unc)  ||
          (opcode == TOP_cmp_i_ne_unc)   ||
          (opcode == TOP_fclass_m_unc)   ||
          (opcode == TOP_fclass_nm_unc)  ||
          (opcode == TOP_tbit_nz_unc)    ||
          (opcode == TOP_tbit_z_unc)     ||
          (opcode == TOP_fcmp_eq_unc)    ||
          (opcode == TOP_fcmp_ge_unc)    ||
          (opcode == TOP_fcmp_gt_unc)    ||
          (opcode == TOP_fcmp_le_unc)    ||
          (opcode == TOP_fcmp_lt_unc)    ||
          (opcode == TOP_fcmp_neq_unc)   ||
          (opcode == TOP_fcmp_nge_unc)   ||
          (opcode == TOP_fcmp_ngt_unc)   ||
          (opcode == TOP_fcmp_nle_unc)   ||
          (opcode == TOP_fcmp_nlt_unc)   ||
          (opcode == TOP_fcmp_ord_unc)   ||
          (opcode == TOP_fcmp_unord_unc));
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
EBO_Set_Predicate_omega (OP *new_op, EBO_TN_INFO *pred_tninfo)
{
  if (pred_tninfo != NULL) {
    Set_OP_omega (new_op, OP_PREDICATE_OPND, pred_tninfo->omega);
  }
  return;
}


static
void
EBO_OPS_omega (OPS *ops, EBO_TN_INFO *pred_tninfo)
{
  OP *next_op = OPS_first(ops);
  while (next_op != NULL) {
    INT opnds = OP_opnds(next_op);
    INT i;

    CG_LOOP_Init_Op(next_op);
    for (i = 0; i < opnds; i++) {
      Set_OP_omega (next_op, i, 0);
    }

    if (OP_has_predicate(next_op) && (pred_tninfo != NULL)) {
      Set_OP_omega(next_op, OP_PREDICATE_OPND, pred_tninfo->omega);
    }
    next_op = OP_next(next_op);
  }

  return;
}




static
void
EBO_Copy_Predicate(TN *predicate_tn, TN *source_tn, OPS *ops)
/*
 * EBO_Copy_Predicate
 *
 * Predicates are set by creating a test-equal of 0 against 0
 * with the "always true" predicate (p0) for the OP, and placing
 * the predicate_tn in the first result position if the source_tn
 * is the True_TN; in the second result position if source_tn is
 * constant and not True_TN; and in the first position with the
 * source_tn as the predicate_operand otherwise.
 *
 */
{
  TN *result1 = True_TN;
  TN *result2 = True_TN;
  TN *predicate = True_TN;

  if (source_tn == True_TN) {
    result1 = predicate_tn;
  } else if (TN_Is_Constant(source_tn)) {
    result2 = predicate_tn;
  } else {
    result1 = predicate_tn;
    predicate = source_tn;
  }

  Build_OP (TOP_cmp_eq_unc, result1, result2, predicate, Zero_TN, Zero_TN, ops);
}



static
void
EBO_Complement_Predicate(TN *predicate_tn, TN *source_tn, OPS *ops)
/*
 * EBO_Complement_Predicate
 *
 * Predicates are complemented by creating a test-equal of 0 against 0
 * with the "always true" predicate (p0) for the OP, and placing
 * the predicate_tn in the second result position if the source_tn
 * is the True_TN; in the first result position if source_tn is
 * constant and not True_TN; and in the second position with the
 * source_tn as the predicate_operand otherwise.
 *
 */
{
  TN *result1 = True_TN;
  TN *result2 = True_TN;
  TN *predicate = True_TN;

  if (source_tn == True_TN) {
    result1 = predicate_tn;
  } else if (TN_is_constant(source_tn)) {
    result2 = predicate_tn;
  } else {
    result1 = predicate_tn;
    predicate = source_tn;
  }

  Build_OP (TOP_cmp_eq_unc, result2, result1, predicate, Zero_TN, Zero_TN, ops);
}



static
BOOL
condition_redundant(OP *elim_op,
                    EBO_TN_INFO **opnd_tninfo,
                    EBO_OP_INFO *prev_opinfo)
{
  OP *prev_op = prev_opinfo->in_op;
  OPS ops = OPS_EMPTY;
  TOP p_opcode = OP_code(prev_op);
  TOP e_opcode = OP_code(elim_op);

  if ((p_opcode == e_opcode) ||
      ((p_opcode == TOP_cmp_eq) && (e_opcode == TOP_cmp_ne)) ||
      ((p_opcode == TOP_cmp_i_eq) && (e_opcode == TOP_cmp_i_ne)) ||
      ((p_opcode == TOP_cmp_eq_unc) && (e_opcode == TOP_cmp_ne_unc)) ||
      ((p_opcode == TOP_cmp_i_eq_unc) && (e_opcode == TOP_cmp_i_ne_unc)) ||
      ((p_opcode == TOP_cmp4_eq) && (e_opcode == TOP_cmp4_ne)) ||
      ((p_opcode == TOP_cmp4_i_eq) && (e_opcode == TOP_cmp4_i_ne)) ||
      ((p_opcode == TOP_cmp4_eq_unc) && (e_opcode == TOP_cmp4_ne_unc)) ||
      ((p_opcode == TOP_cmp4_i_eq_unc) && (e_opcode == TOP_cmp4_i_ne_unc)) ||
      ((p_opcode == TOP_cmp_ne) && (e_opcode == TOP_cmp_eq)) ||
      ((p_opcode == TOP_cmp_i_ne) && (e_opcode == TOP_cmp_i_eq)) ||
      ((p_opcode == TOP_cmp_ne_unc) && (e_opcode == TOP_cmp_eq_unc)) ||
      ((p_opcode == TOP_cmp_i_ne_unc) && (e_opcode == TOP_cmp_i_eq_unc)) ||
      ((p_opcode == TOP_cmp4_ne) && (e_opcode == TOP_cmp4_eq)) ||
      ((p_opcode == TOP_cmp4_i_ne) && (e_opcode == TOP_cmp4_i_eq)) ||
      ((p_opcode == TOP_cmp4_ne_unc) && (e_opcode == TOP_cmp4_eq_unc)) ||
      ((p_opcode == TOP_cmp4_i_ne_unc) && (e_opcode == TOP_cmp4_i_eq_unc)) ||
      ((p_opcode == TOP_cmp_le) && (e_opcode == TOP_cmp_gt)) ||
      ((p_opcode == TOP_cmp_i_le) && (e_opcode == TOP_cmp_i_gt)) ||
      ((p_opcode == TOP_cmp_le_unc) && (e_opcode == TOP_cmp_gt_unc)) ||
      ((p_opcode == TOP_cmp_i_le_unc) && (e_opcode == TOP_cmp_i_gt_unc)) ||
      ((p_opcode == TOP_cmp4_le) && (e_opcode == TOP_cmp4_gt)) ||
      ((p_opcode == TOP_cmp4_i_le) && (e_opcode == TOP_cmp4_i_gt)) ||
      ((p_opcode == TOP_cmp4_le_unc) && (e_opcode == TOP_cmp4_gt_unc)) ||
      ((p_opcode == TOP_cmp4_i_le_unc) && (e_opcode == TOP_cmp4_i_gt_unc)) ||
      ((p_opcode == TOP_cmp_gt) && (e_opcode == TOP_cmp_le)) ||
      ((p_opcode == TOP_cmp_i_gt) && (e_opcode == TOP_cmp_i_le)) ||
      ((p_opcode == TOP_cmp_gt_unc) && (e_opcode == TOP_cmp_le_unc)) ||
      ((p_opcode == TOP_cmp_i_gt_unc) && (e_opcode == TOP_cmp_i_le_unc)) ||
      ((p_opcode == TOP_cmp4_gt) && (e_opcode == TOP_cmp4_le)) ||
      ((p_opcode == TOP_cmp4_i_gt) && (e_opcode == TOP_cmp4_i_le)) ||
      ((p_opcode == TOP_cmp4_gt_unc) && (e_opcode == TOP_cmp4_le_unc)) ||
      ((p_opcode == TOP_cmp4_i_gt_unc) && (e_opcode == TOP_cmp4_i_le_unc)) ||
      ((p_opcode == TOP_cmp_lt) && (e_opcode == TOP_cmp_ge)) ||
      ((p_opcode == TOP_cmp_i_lt) && (e_opcode == TOP_cmp_i_ge)) ||
      ((p_opcode == TOP_cmp_lt_unc) && (e_opcode == TOP_cmp_ge_unc)) ||
      ((p_opcode == TOP_cmp_i_lt_unc) && (e_opcode == TOP_cmp_i_ge_unc)) ||
      ((p_opcode == TOP_cmp4_lt) && (e_opcode == TOP_cmp4_ge)) ||
      ((p_opcode == TOP_cmp4_i_lt) && (e_opcode == TOP_cmp4_i_ge)) ||
      ((p_opcode == TOP_cmp4_lt_unc) && (e_opcode == TOP_cmp4_ge_unc)) ||
      ((p_opcode == TOP_cmp4_i_lt_unc) && (e_opcode == TOP_cmp4_i_ge_unc)) ||
      ((p_opcode == TOP_cmp_ge) && (e_opcode == TOP_cmp_lt)) ||
      ((p_opcode == TOP_cmp_i_ge) && (e_opcode == TOP_cmp_i_lt)) ||
      ((p_opcode == TOP_cmp_ge_unc) && (e_opcode == TOP_cmp_lt_unc)) ||
      ((p_opcode == TOP_cmp_i_ge_unc) && (e_opcode == TOP_cmp_i_lt_unc)) ||
      ((p_opcode == TOP_cmp4_ge) && (e_opcode == TOP_cmp4_lt)) ||
      ((p_opcode == TOP_cmp4_i_ge) && (e_opcode == TOP_cmp4_i_lt)) ||
      ((p_opcode == TOP_cmp4_ge_unc) && (e_opcode == TOP_cmp4_lt_unc)) ||
      ((p_opcode == TOP_cmp4_i_ge_unc) && (e_opcode == TOP_cmp4_i_lt_unc))) {
    TN *pr0 = OP_result(prev_op,0);
    TN *pr1 = OP_result(prev_op,1);
	EBO_TN_INFO *pr0_tninfo = prev_opinfo->actual_rslt[0];
	EBO_TN_INFO *pr1_tninfo = prev_opinfo->actual_rslt[1];
    TN *er0 = OP_result(elim_op,0);
    TN *er1 = OP_result(elim_op,1);
    TN *prp = OP_opnd(prev_op,OP_PREDICATE_OPND);
    TN *erp = OP_opnd(elim_op,OP_PREDICATE_OPND);

    if (p_opcode != e_opcode) {
     /* We are dealing with complement compare instructions. */
      TN *save_tn = pr0;
	  EBO_TN_INFO *save_tn_info = pr0_tninfo;

      if (((pr0 == True_TN) && (er0 == True_TN)) ||
          ((pr1 == True_TN) && (er1 == True_TN))) {
       /* We may be able to replace both compares with a single one. */

        if (prp != erp) goto can_not_combine_ops;

        for (INT i = 0; i < OP_results(prev_op); i++) {
          if (!TN_Is_Constant(OP_result(prev_op,i))) {
              EBO_TN_INFO *result_tninfo = prev_opinfo->actual_rslt[i];
            if ((result_tninfo == NULL) ||
                (result_tninfo->reference_count != 0)) goto can_not_combine_ops;
          }
        }
        if ( pr0 == True_TN ) {
            if ( (TN_register_class(pr1) ==  TN_register_class(er1))
              && (TN_register(pr1) ==  TN_register(er1)))
              goto can_not_combine_ops;
        } else {
            if ( (TN_register_class(pr0) ==  TN_register_class(er0))
              && (TN_register(pr0) ==  TN_register(er0)))
              goto can_not_combine_ops;
        }

        OP *new_op = Dup_OP(elim_op);
        if (pr0 == True_TN) {
          Set_OP_result(new_op, 0, pr1);
        } else {
          Set_OP_result(new_op, 1, pr0);
        }
        if (EBO_in_loop) EBO_Copy_OP_omega (new_op, elim_op);
        OP_srcpos(new_op) = OP_srcpos(elim_op);
        BB_Insert_Op_After(OP_bb(elim_op), elim_op, new_op);

        if (EBO_Trace_Optimization) {
          #pragma mips_frequency_hint NEVER
          fprintf(TFile,"%sMerge complementery compare operations.\n",EBO_trace_pfx);
        }

        return TRUE;
      }

     /* Interchange names for the results so that the opcodes can be viewed as the same. */
      save_tn = pr0;
      pr0 = pr1;
      pr1 = save_tn;

	  save_tn_info = pr0_tninfo;
	  pr0_tninfo = pr1_tninfo;
	  pr1_tninfo = save_tn_info;
      if (EBO_Trace_Optimization) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"%sCompare instructions are complements.\n",EBO_trace_pfx);
      }
    }

    if ((prp != erp) ||
        (EBO_in_pre &&
         (OP_bb(prev_op) != OP_bb(elim_op)))) {
     /* Predicates are not the same.  This could be a problem. */
     /* If BBs are not the same we could get into trouble with */
     /* predicates after hyperblock formation.                 */
     /*                                                        */
     /* There are a few special cases that could be caught,    */
     /* but it doesn't seem worth the effort.                  */
      if (EBO_Trace_Optimization) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"%sPredicates are different for redundant compare.\n",EBO_trace_pfx);
      }

      goto can_not_combine_ops;
    }

   /* The result registers may conflict, if they are being re-assigned. */
    if (((er0 != True_TN) && tn_registers_identical (er0, pr1)) ||
        ((er1 != True_TN) && tn_registers_identical (er1, pr0))) {

      if (EBO_Trace_Optimization) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"%sRegister re-use in redundant compare.\n",EBO_trace_pfx);
      }

      goto can_not_combine_ops;
    }

   /* But if the same registers are defined, we can just delete this OP. */
    if (tn_registers_identical (er0, pr0) &&
        tn_registers_identical (er1, pr1)) {
      if (EBO_Trace_Optimization) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"%sDelete redundant compare operation.\n",EBO_trace_pfx);
      }

      return TRUE;
    }

    /* Originally, in the else branch a complement predicate copy of this form:
      (p7) cmp.eq.unc p0,p11 = r0, r0
      is created if the $pr0 is a True TN. 
      The author wants this instruction to have p7||p11=true.
      However, this is wrong.
      If p7 is true, then according to the IA64 manual, p11 should false.
      If p7 is false, then p0 and p11 are both false.
    */
    if (er0 != True_TN) {
      if (pr0 != True_TN && EBO_tn_available(OP_bb(elim_op), pr0_tninfo)) {
        EBO_Copy_Predicate (er0, pr0, &ops);
      } else {
        goto can_not_combine_ops;
      }
    } 
    if (er1 != True_TN) {
      if (pr1 != True_TN && EBO_tn_available(OP_bb(elim_op), pr1_tninfo)) {
        EBO_Copy_Predicate (er1, pr1, &ops);
      } else {
        goto can_not_combine_ops;
      }
    }

    if (OPS_length(&ops) != 0) {
      if (EBO_in_loop) EBO_OPS_omega (&ops, NULL);
        OP *next_op = OPS_first(&ops);
        while (next_op != NULL) {
          OP_srcpos(next_op) = OP_srcpos(elim_op);
          next_op = OP_next(next_op);
        }
      BB_Insert_Ops(OP_bb(elim_op), elim_op, &ops, FALSE);
    }

    if (EBO_Trace_Optimization) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile,"%sRemove redundant compare operation.\n",EBO_trace_pfx);
    }

    return TRUE;
  }

can_not_combine_ops:

  if (EBO_Trace_Optimization) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile,"%sCan not remove compare operation.\n",EBO_trace_pfx);
  }

  return FALSE;
}

  


BOOL
combine_adjacent_loads(OP *op,
                       EBO_TN_INFO **opnd_tninfo,
		       EBO_OP_INFO *opinfo,
                       INT64 offset_pred,
                       INT64 offset_succ)
{
  OP *pred_op = opinfo->in_op;
  BB *bb = OP_bb(op);
  TOP opcode = OP_code(op);
  TOP new_opcode;
  TN *pred_result = OP_result(pred_op,0);
  TN *succ_result = OP_result(op,0);
  INT base_index = TOP_Find_Operand_Use(OP_code(op),OU_base);
  TN *base_tn;
  EBO_TN_INFO *base_tninfo;
  INT size_pred;
  INT size_succ;
  TN *r1;
  TN *r2;
  OPS ops = OPS_EMPTY;

  if (EBO_Trace_Data_Flow) {
    fprintf(TFile,"%s          OP in BB:%d    ",EBO_trace_pfx,BB_id(bb));
    Print_OP_No_SrcLine(op);
    fprintf(TFile," is adjacent to OP in BB:%d    ",BB_id(opinfo->in_bb));
    Print_OP_No_SrcLine(pred_op);
  }
  
  
  if (!OP_load(op) || !OP_load(pred_op) || (opcode != OP_code(pred_op)) ||
      (OP_results(op) != 1) || (OP_results(pred_op) != 1) || (base_index < 0) ||
      
      /* ld.s, ld.a, ld.sa, ld.c and ld in recovery block must be saved */
      
      CGTARG_Is_OP_Speculative(op) || CGTARG_Is_OP_Speculative(pred_op) ||
      CGTARG_Is_OP_Check_Load(op)  || CGTARG_Is_OP_Check_Load(pred_op)  ||
      BB_recovery(OP_bb(op))       || BB_recovery(OP_bb(pred_op))) {
    return FALSE;
  }

  if ((Opt_Level < 2) && (bb != opinfo->in_bb)) {
   /* Global TN's aren't supported at low levels of optimization. */
    return FALSE;
  }

  size_pred = CGTARG_Mem_Ref_Bytes(pred_op);
  size_succ = CGTARG_Mem_Ref_Bytes(op);
  if (size_pred != size_succ) return FALSE;

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

  if (!has_assigned_reg(pred_result) ||
      !has_assigned_reg(succ_result) ||
      (TN_register_class(pred_result) != TN_register_class(succ_result)) ||
      ((TN_register(pred_result) & 1) == (TN_register(succ_result) & 1))) {
    if (EBO_Trace_Data_Flow) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile,"%sParied loads requires odd/even registers.\n", EBO_trace_pfx);
    }
    return FALSE;
  }

  if ((opinfo->actual_rslt[0] == NULL) ||
      (opinfo->actual_rslt[0]->reference_count != 0) ||
      !EBO_tn_available (opinfo->in_bb, opinfo->actual_rslt[0])) {
    if (EBO_Trace_Data_Flow) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile,"%sThe result of the predecessor load has already been used.\n", EBO_trace_pfx);
    }
    return FALSE;
  }

  switch (opcode) {
    case TOP_ldfs: new_opcode = TOP_ldfps; break;
    case TOP_ldfd: new_opcode = TOP_ldfpd; break;
    case TOP_ldf8: new_opcode = TOP_ldfp8; break;
    default: return FALSE;
  }

  if (offset_pred < offset_succ) {
    base_tn = OP_opnd(pred_op, base_index);
    base_tninfo = opinfo->actual_opnd[base_index];
    if (!TN_Is_Constant(base_tn) && !EBO_tn_available (opinfo->in_bb, base_tninfo)) {
      if (EBO_Trace_Data_Flow) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"%sThe index TN of the predecessor load is not available.\n", EBO_trace_pfx);
      }
      return FALSE;
    }
    r1 = pred_result;
    r2 = succ_result;
  } else {
    base_tn = OP_opnd(op, base_index);
    base_tninfo = opnd_tninfo[base_index];
    r1 = succ_result;
    r2 = pred_result;
  }

/* NYI: The simulator does not yet support these instructions.  */
  if (EBO_Trace_Optimization) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile,"%sThe simulator does not yet support load-pair instructions.\n",EBO_trace_pfx);
  }
  return FALSE;

  Build_OP (new_opcode, r1, r2, OP_opnd(op,OP_PREDICATE_OPND),
            OP_opnd(op,1), OP_opnd(op,2), base_tn, &ops);
  Copy_WN_For_Memory_OP (OPS_first(&ops), op);
  OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
  if (EBO_in_loop) EBO_Set_OP_omega (OPS_first(&ops), opnd_tninfo[OP_PREDICATE_OPND],
                                     NULL, NULL, base_tninfo);
  BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);

  remove_op (opinfo);
  OP_Change_To_Noop(pred_op);
  opinfo->in_op = NULL;
  opinfo->in_bb = NULL;

  if (EBO_Trace_Optimization) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile,"%sReplace adjacent load with load pair.\n",EBO_trace_pfx);
  }

  return TRUE;
}



static
BOOL
hoist_predicate_of_duplicate_complement (OP *op,
                                         EBO_TN_INFO **opnd_tninfo,
                                         EBO_OP_INFO *opinfo)
{
  OP *pred_op = opinfo->in_op;
  TN *predicate1_tn = OP_opnd(pred_op,OP_PREDICATE_OPND);
  TN *predicate2_tn = OP_opnd(op,OP_PREDICATE_OPND);
  EBO_TN_INFO *predicate1_info = opinfo->actual_opnd[OP_PREDICATE_OPND];
  EBO_TN_INFO *predicate2_info = opnd_tninfo[OP_PREDICATE_OPND];
  BB *bb = OP_bb(op);
  INT i;
  INT val_idx;

  if (EBO_Trace_Data_Flow) {
    fprintf(TFile,"Enter hoist_predicate_of_duplicate_complement\n");
    Print_OP_No_SrcLine(pred_op);
    Print_OP_No_SrcLine(op);
    Print_TN(predicate1_tn,false);fprintf(TFile,"  ");
    Print_TN(predicate2_tn,false);fprintf(TFile,"\n");
    if (predicate1_info != NULL) tn_info_entry_dump(predicate1_info);
    if (predicate2_info != NULL) tn_info_entry_dump(predicate2_info);
  }

  if (OP_code(op) != OP_code(pred_op)) return FALSE;

 /* Pick up the new predicate that dominates both of those in the common expressions. */
  EBO_OP_INFO *new_predicate_opinfo = locate_opinfo_entry(predicate1_info);
  if (new_predicate_opinfo == NULL) return FALSE;

  EBO_TN_INFO *new_predicate_tninfo = new_predicate_opinfo->actual_opnd[OP_PREDICATE_OPND];
  TN *new_predicate_tn = OP_opnd(predicate1_info->in_op,OP_PREDICATE_OPND);

  if (!(TN_Is_Constant(new_predicate_tn) ||
        EBO_tn_available (bb, new_predicate_tninfo))) return FALSE;

  if (OP_store(op)) {
   /* For stores, we will replace both stores with a new one. */
   /* Note that the location of the store will be moved!      */
    if (OP_bb(op) != OP_bb(pred_op)) return FALSE;
    if (opinfo->op_must_not_be_removed) return FALSE;

    val_idx = TOP_Find_Operand_Use(OP_code(op),OU_storeval);
    EBO_TN_INFO *value_tninfo = opinfo->actual_opnd[val_idx];
   /* We will need to copy the value that is to be stored. */

   /* First, be sure that it will be available this point. */
    if ((value_tninfo == NULL) ||
        !EBO_tn_available (bb, value_tninfo)) return FALSE;
   /* Second, be sure that the target of the copy will be a valid result. */
    if (TN_is_const_reg(OP_opnd(op, val_idx))) return FALSE;
   /* Third, be sure that any results have not been used prior to the current OP. */
    if (OP_results(pred_op) != 0) {
      for (i = 0; i < OP_results(pred_op); i++) {
        EBO_TN_INFO *result_tninfo = opinfo->actual_rslt[i];
        if ((result_tninfo == NULL) ||
            (result_tninfo->reference_count != 0)) return FALSE;
      }
    }
   /* Fourth, the index that we need to use must also be generated
      with a predicate that dominates the new store. */
    INT base_idx = TOP_Find_Operand_Use(OP_code(op),OU_base);
    EBO_TN_INFO *input_info = opinfo->actual_opnd[base_idx];
    if ((input_info == NULL) ||
        (input_info->in_op == NULL) ||
        (input_info->local_tn == NULL) ||
        (input_info->predicate_tninfo != new_predicate_tninfo) ||
        OP_Defs_TN(pred_op, input_info->local_tn)) {
     /* The inputs to the hoisted expression must use the same predicate
        as the new (or modified) expression will. */
      if (EBO_Trace_Data_Flow) {
        fprintf(TFile,"predicates not hoisted because address not hoisted\n");
      }
      return FALSE;
    }
  } else if (OP_effectively_copy(op)) {
    INT val_indx = EBO_Copy_Operand(op);
    if ((val_indx <= 0) ||
        !TNs_Are_Equivalent(OP_opnd(op,val_indx), OP_opnd(pred_op,val_indx))) {
     /* The operands are different. We don't handle this and it may not be worthwhile. */
      if (EBO_Trace_Data_Flow) {
        fprintf(TFile,"copy operands are different\n");
      }
      return FALSE;
    }
  } else {
   /* For non-stores, we will use the results of the predecessor OP. */
    for (i = 0; i < OP_results(pred_op); i++) {
      if (opinfo->actual_rslt[i] == NULL) return FALSE;
      if (!EBO_tn_available (bb, opinfo->actual_rslt[i])) {
        if (EBO_Trace_Data_Flow) {
          fprintf(TFile,"previous result is not available\n");
        }
        return FALSE;
      }
    }
   /* If we are to leave the first OP in place but hoist it's predicate,
      the inputs to the OP must not be conditionally generated. */
    for (i = OP_PREDICATE_OPND+1; i < OP_opnds(pred_op); i++) {
      EBO_TN_INFO *input_info = opinfo->actual_opnd[i];
      if ((input_info == NULL) ||
          (input_info->in_op == NULL) ||
          (input_info->local_tn == NULL) ||
          (input_info->predicate_tninfo != new_predicate_tninfo) ||
          OP_Defs_TN(pred_op, input_info->local_tn)) {
       /* The inputs to the hoisted expression must use the same predicate
          as the new (or modified) expression will. */
        if (EBO_Trace_Data_Flow) {
          fprintf(TFile,"predicates not hoisted because inputs not hoisted\n");
        }
        return FALSE;
      }
    }
  }

  OPS ops = OPS_EMPTY;
  if (OP_store(op)) {
   /* Create copies of the different values that are to be stored. */
    TN *value_tn = OP_opnd(op, val_idx);
    if (!TNs_Are_Equivalent(OP_opnd(op, val_idx),OP_opnd(pred_op, val_idx))) {
     /* Need to merge the two values that are to be stored. */

     /* Note: it is probably not worthwhile making this transformation because
        it will only increase the tree height of the expressions. */
      if (has_assigned_reg(value_tn) || EBO_in_loop) {
        if (EBO_Trace_Data_Flow) {
          fprintf(TFile,"predicates not hoisted because inputs can not be merged\n");
        }
        return FALSE;
      }

      OPS ops1 = OPS_EMPTY;
      value_tn = Dup_TN(value_tn);

      EBO_Exp_COPY (predicate1_tn,
                    value_tn,
                    OP_opnd(pred_op, val_idx),
                    &ops1);
      OP_srcpos(OPS_last(&ops1)) = OP_srcpos(pred_op);
      if (EBO_in_loop) EBO_Set_OP_omega (OPS_last(&ops1), predicate1_info, opinfo->actual_opnd[val_idx]);
      OPS_Append_Ops( &ops, &ops1);

      OPS_Init(&ops1);
      EBO_Exp_COPY (predicate2_tn,
                    value_tn,
                    OP_opnd(op, val_idx),
                    &ops1);
      OP_srcpos(OPS_last(&ops1)) = OP_srcpos(op);
      if (EBO_in_loop) EBO_Set_OP_omega (OPS_last(&ops1), predicate2_info, opnd_tninfo[val_idx]);
      OPS_Append_Ops( &ops, &ops1);
    }

   /* Create and insert a new store OP. */
    OP *new_op = Dup_OP(op);
    Set_OP_opnd (new_op, OP_PREDICATE_OPND, new_predicate_tn);
    Set_OP_opnd (new_op, val_idx, value_tn);
    if (OP_memory(op)) {
      Copy_WN_For_Memory_OP (new_op, op);
    }
    OP_srcpos(new_op) = OP_srcpos(op);
    if (EBO_in_loop) {
      EBO_Copy_OP_omega( new_op, op);
      EBO_Set_Predicate_omega ( new_op, new_predicate_tninfo);
    }
    OPS_Append_Op( &ops, new_op);

    if (OP_results(op) != 0) {
      OPS ops1 = OPS_EMPTY;
      for (i = 0; i < OP_results(pred_op); i++) {
        EBO_Exp_COPY (predicate1_tn,
                      OP_result(pred_op, i),
                      OP_result(op, i),
                      &ops1);
        OP_srcpos(OPS_last(&ops1)) = OP_srcpos(pred_op);
        if (EBO_in_loop) EBO_Set_OP_omega (OPS_last(&ops1), predicate1_info, NULL);
      }
      OPS_Append_Ops( &ops, &ops1);
    }

   /* Remove the first OP. */
    remove_op (opinfo);
    OP_Change_To_Noop(opinfo->in_op);
    opinfo->in_op = NULL;
    opinfo->in_bb = NULL;
  } else {
   /* Replace the predicate of the previous op. */
    Set_OP_opnd (pred_op, OP_PREDICATE_OPND, new_predicate_tn);
    opinfo->actual_opnd[OP_PREDICATE_OPND] = new_predicate_tninfo;
    for (i = 0; i < OP_results(pred_op); i++) {
      if (opinfo->actual_rslt[i] != NULL) {
        if (opinfo->actual_rslt[i]->predicate_tninfo != NULL) {
          dec_ref_count(opinfo->actual_rslt[i]->predicate_tninfo);
        }
        opinfo->actual_rslt[i]->predicate_tninfo = new_predicate_tninfo;
        if (new_predicate_tninfo != NULL) {
          inc_ref_count(new_predicate_tninfo);
        }
      }
    }

    if (OP_effectively_copy(op)) {
     /* Leave the second OP the way it was. */
if (EBO_Trace_Optimization) fprintf(TFile,"hoist predicate for complementary copy\n");
      return FALSE;
    }

   /* Create copies to the different outputs. */
    OPS ops1 = OPS_EMPTY;
    for (i = 0; i < OP_results(op); i++) {
      if (!TNs_Are_Equivalent(OP_result(op, i),OP_result(pred_op, i))) {
        EBO_Exp_COPY (predicate2_tn,
                      OP_result(op, i),
                      OP_result(pred_op, i),
                      &ops1);
        OP_srcpos(OPS_last(&ops1)) = OP_srcpos(op);
        if (EBO_in_loop) EBO_Set_OP_omega (OPS_last(&ops1), predicate2_info, NULL);
      }
    }
    OPS_Append_Ops( &ops, &ops1);
  }

  BB_Insert_Ops(bb, op, &ops, FALSE);
if (EBO_Trace_Optimization) fprintf(TFile,"hoist predicate for complementary operations\n");
  return TRUE;
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

  /* ld.s, ld.a, ld.sa, ld.c and ld in recovery block must be saved */
  if (CGTARG_Is_OP_Speculative(op) ||
      CGTARG_Is_OP_Speculative(pred_op) ||
      CGTARG_Is_OP_Check_Load(op)  ||
      CGTARG_Is_OP_Check_Load(pred_op)  ||
      BB_recovery(OP_bb(op))   || 
      BB_recovery(OP_bb(pred_op)))    return FALSE;
  
  byte_offset = offset_succ - offset_pred;

  if (OP_load(op) && OP_load(pred_op) &&
      (OP_results(op) == 1) && (OP_results(pred_op) == 2) &&
      (2*size_succ == size_pred)) {
   /* Just use one of the 2 results for the predecessor. */
    pred_result = (byte_offset >= size_succ) ? OP_result(pred_op,1) : OP_result(pred_op,0);

    if (EBO_Trace_Optimization) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile,"%sRemove LoadPair - Load combination\n",EBO_trace_pfx);
    }

    EBO_Exp_COPY((OP_has_predicate(op)?OP_opnd(op,OP_PREDICATE_OPND):NULL),
                 succ_result, pred_result, &ops);
    if (EBO_in_loop) EBO_OPS_omega (&ops, (OP_has_predicate(op)?opnd_tninfo[OP_PREDICATE_OPND]:NULL));
    OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
    BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
    return TRUE;
  }

  if (byte_offset > 8) return FALSE;

  if (!OP_store(pred_op) || !OP_load(op)) {
   /* Can only optimize Store - Load pattern. */
    return FALSE;
  }

  if (TOP_Find_Operand_Use(OP_code(op), OU_postincr) >= 0) {
   /* The increment must be preserved. */
    return FALSE;
  }

  if ((TN_register_class(pred_result) != ISA_REGISTER_CLASS_integer) ||
      (TN_register_class(succ_result) != ISA_REGISTER_CLASS_integer)) {
   /* Can only play games with integers. */
    return FALSE;
  }

  TOP extr_op = TOP_extr_u; /* Loads are always zero-filled on IA-64! */
  INT bit_size = size_succ*8;
  INT succ_offset = (offset_succ - offset_pred);
  INT bit_offset = (Target_Byte_Sex == BIG_ENDIAN ?
                      size_pred-succ_offset-size_succ : succ_offset)*8;

  Build_OP(extr_op, succ_result, OP_opnd(op,OP_PREDICATE_OPND), pred_result,
           Gen_Literal_TN(bit_offset, 4), Gen_Literal_TN(bit_size, 4), &ops);
  if (EBO_in_loop) EBO_Set_OP_omega (OPS_first(&ops), opnd_tninfo[OP_PREDICATE_OPND],
                                     NULL, NULL, NULL);
  OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
  BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);

  if (EBO_Trace_Optimization) {
    fprintf(TFile,"%sReplace subset load with extract.\n",EBO_trace_pfx);
  }

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
  BB *bb = OP_bb(op);
  OPS ops = OPS_EMPTY;
  OP *pred_op = opinfo->in_op;
  OP *intervening_op = intervening_opinfo->in_op;
  TOP pred_opcode = OP_code(pred_op);
  TOP intervening_opcode = OP_code(intervening_op);
  INT pred_base_idx = TOP_Find_Operand_Use(pred_opcode, OU_base);
  INT intervening_base_idx =  TOP_Find_Operand_Use(intervening_opcode, OU_base);
  INT pred_inc_idx = TOP_Find_Operand_Use(pred_opcode, OU_postincr);
  INT intervening_inc_idx = TOP_Find_Operand_Use(intervening_opcode, OU_postincr);
  INT size_pred;
  INT size_succ;
  INT size_intervening;
  TN *predicate_tn = OP_has_predicate(op)?OP_opnd(op,OP_PREDICATE_OPND):NULL;

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

  if (TOP_Find_Operand_Use(OP_code(op), OU_postincr) >= 0) {
   /* The increment must be preserved. */
    if (EBO_Trace_Data_Flow) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile,"%sIncremented Load for intervening store combination\n",
              EBO_trace_pfx);
    }
    return FALSE;
  }

 /* Paired loads are not supported. */
  if (OP_results(op) == 2) return FALSE;
  if ((OP_store(pred_op) &&
      ((OP_results(pred_op) > 1) ||
       ((OP_results(pred_op) > 0) &&
        (pred_inc_idx < 0)))) ||
      (OP_store(intervening_op) &&
      ((OP_results(intervening_op) > 1) ||
       ((OP_results(intervening_op) > 0) &&
        (intervening_inc_idx < 0))))) return FALSE;
  if ((OP_load(pred_op) &&
      ((OP_results(pred_op) > 2) ||
       ((OP_results(pred_op) > 1) &&
        (pred_inc_idx < 0)))) ||
      (OP_load(intervening_op) &&
      ((OP_results(intervening_op) > 2) ||
       ((OP_results(intervening_op) > 1) &&
        (intervening_inc_idx < 0))))) return FALSE;

  /* ld.s, ld.a, ld.sa, ld.c and ld in recovery block must be saved */
  if (CGTARG_Is_OP_Speculative(op) ||
      CGTARG_Is_OP_Speculative(pred_op) ||
      CGTARG_Is_OP_Check_Load(op)  ||
      CGTARG_Is_OP_Check_Load(pred_op)  ||
      BB_recovery(OP_bb(op))   || 
      BB_recovery(OP_bb(pred_op)))    return FALSE;
  
 /* Capture the values in the preceeding memory OPs. */
  pred_result = OP_store(pred_op) ? OP_opnd(pred_op,
                                            TOP_Find_Operand_Use(pred_opcode, OU_storeval))
                                  : OP_result(pred_op,0);
  intervening_result = OP_opnd(intervening_op,
                               TOP_Find_Operand_Use(intervening_opcode, OU_storeval));

  if ((TN_register_class(intervening_result) != TN_register_class(pred_result)) ||
      (TN_register_class(intervening_result) != TN_register_class(OP_result(op,0)))) {
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

    if ((size_succ != 8) &&
        (TN_register_class(intervening_result) == ISA_REGISTER_CLASS_integer) &&
        (TN_register_class(pred_result) == ISA_REGISTER_CLASS_integer)) {
     /* Store - Load sequence for small integers will strip off sign bit. */
     /* Use extract instruction to simulate store-load sequence.          */
      TOP extr_op = TOP_extr_u; /* Loads are always zero-filled on IA-64! */
      INT bit_size = size_succ*8;
      INT bit_offset = 0; /* Assume right justified in register. */

      Build_OP(extr_op, OP_result(op, 0), predicate_tn, pred_result,
               Gen_Literal_TN(bit_offset, 4), Gen_Literal_TN(bit_size, 4), &ops);
      if (predicate_tn != True_TN) Set_OP_cond_def_kind(OPS_last(&ops), OP_PREDICATED_DEF);
      OP_srcpos(OPS_last(&ops)) = OP_srcpos(op);
    } else {
     /* Use full word copy. */
      EBO_Exp_COPY(predicate_tn, OP_result(op, 0), pred_result, &ops);
      if (predicate_tn != True_TN) Set_OP_cond_def_kind(OPS_last(&ops), OP_PREDICATED_DEF);
      OP_srcpos(OPS_last(&ops)) = OP_srcpos(op);
    }

    BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);

    if (EBO_Trace_Optimization) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile,"%sReload across intervening store is not needed.\n",EBO_trace_pfx);
      Print_OP_No_SrcLine(pred_op);
      Print_OP_No_SrcLine(intervening_op);
      Print_OP_No_SrcLine(op);
    }

    return TRUE;
  }

 /* We need to compare the addresses BEFORE they were incremented.
    If both OPs are incremented by the same value, we can compare
    the addresses AFTER the increment.
 */
  pred_index = OP_opnd(pred_op, pred_base_idx);
  intervening_index = OP_opnd(intervening_op, intervening_base_idx);

  if ((pred_inc_idx >= 0) ||
      (intervening_inc_idx >= 0)) {
    if ((pred_inc_idx < 0) || (intervening_inc_idx < 0)) {
      if (EBO_Trace_Data_Flow) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"%sBase address not available for compare because of increment\n",
                EBO_trace_pfx);
      }
      return FALSE;
    }
    TN *pred_inc_tn = OP_opnd(pred_op,pred_inc_idx);
    TN *intervening_inc_tn = OP_opnd(intervening_op,intervening_inc_idx);
    if ((TN_Is_Constant(pred_inc_tn) ||
         TN_Is_Constant(intervening_inc_tn)) &&
        (pred_inc_tn != intervening_inc_tn)) {
      if (EBO_Trace_Data_Flow) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"%sBase address not available for compare because of different constant increment\n",
                EBO_trace_pfx);
      }
      return FALSE;
    }
    if (opinfo->actual_opnd[pred_inc_idx] != intervening_opinfo->actual_opnd[intervening_inc_idx]) {
      if (EBO_Trace_Data_Flow) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"%sBase address not available for compare because of different increment\n",
                EBO_trace_pfx);
      }
      return FALSE;
    }
    pred_index = OP_load(pred_op) ? OP_result(pred_op,1) : OP_result(pred_op,0);
    intervening_index = OP_load(intervening_op) ? OP_result(intervening_op,1) : OP_result(intervening_op,0);
  }

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
  predicate1 = Dup_TN_Even_If_Dedicated(predicate_tn);
  predicate2 = Dup_TN_Even_If_Dedicated(predicate_tn);

 /* Compare the addresses. */
  Build_OP (TOP_cmp_eq_unc, predicate1, predicate2, predicate_tn, pred_index, intervening_index, &ops);
  OP_srcpos(OPS_last(&ops)) = OP_srcpos(op);

  if ((size_succ != 8) &&
      (TN_register_class(intervening_result) == ISA_REGISTER_CLASS_integer) &&
      (TN_register_class(pred_result) == ISA_REGISTER_CLASS_integer)) {
   /* Store - Load sequence for small integers will strip off sign bit. */
   /* Use extract instruction to simulate store-load sequence.          */
    TOP extr_op = TOP_extr_u; /* Loads are always zero-filled on IA-64! */
    INT bit_size = size_succ*8;
    INT bit_offset = 0; /* Assume right justified in register. */

   /* Copy the "address not equal value". */
    Build_OP(extr_op, OP_result(op, 0), predicate_tn, pred_result,
             Gen_Literal_TN(bit_offset, 4), Gen_Literal_TN(bit_size, 4), &ops);
    if (predicate_tn != True_TN) Set_OP_cond_def_kind(OPS_last(&ops), OP_PREDICATED_DEF);
    OP_srcpos(OPS_last(&ops)) = OP_srcpos(op);

   /* Copy the "address equal result". */
    Build_OP(extr_op, OP_result(op, 0), predicate1, intervening_result,
             Gen_Literal_TN(bit_offset, 4), Gen_Literal_TN(bit_size, 4), &ops);
    Set_OP_cond_def_kind(OPS_last(&ops), OP_PREDICATED_DEF);
    OP_srcpos(OPS_last(&ops)) = OP_srcpos(op);
  } else {
   /* Use full word copies. */
    OPS ops1 = OPS_EMPTY;

   /* Copy the "address not equal value". */
    EBO_Exp_COPY(predicate_tn, OP_result(op, 0), pred_result, &ops1);
    if (predicate_tn != True_TN) Set_OP_cond_def_kind(OPS_last(&ops1), OP_PREDICATED_DEF);
    OP_srcpos(OPS_last(&ops1)) = OP_srcpos(op);
    OPS_Append_Ops( &ops, &ops1);

   /* Copy the "address equal result". */
    OPS_Init(&ops1);
    EBO_Exp_COPY(predicate1, OP_result(op, 0), intervening_result, &ops1);
    Set_OP_cond_def_kind(OPS_last(&ops1), OP_PREDICATED_DEF);
    OP_srcpos(OPS_last(&ops1)) = OP_srcpos(op);
    OPS_Append_Ops( &ops, &ops1);
  }

  BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);

  if (EBO_Trace_Optimization) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile,"%sRemove Reload across intervening store.\n",EBO_trace_pfx);
  }

  return TRUE;
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
  TN *predicate_tn = OP_has_predicate(op)?OP_opnd(op,OP_PREDICATE_OPND):NULL;

  if (OP_prefetch(op)) {
   /* Remove the second OP for:
          Prefetch - Prefetch,
          Load - Prefetch,
          Store - Prefetch
   */
    if (EBO_Trace_Optimization) {
      fprintf(TFile,"%sRemove following Prefetch combination\n",EBO_trace_pfx);
    }
    return TRUE;
  }

  if (OP_prefetch(opinfo->in_op)) {
   /* Don't optimize:
          Prefetch - Load,
          Prefetch - Store,
   */
    return FALSE;
  }

  if (OP_unalign_mem(op) || OP_unalign_mem(opinfo->in_op)) return FALSE;

  size_pred = CGTARG_Mem_Ref_Bytes(opinfo->in_op);
  size_succ = CGTARG_Mem_Ref_Bytes(op);

   /* Replace the result tn of the second OP for:
          Load - Load,
    */

  /* ld.s, ld.a, ld.sa, ld.c and ld in recovery block must be saved */
  if (CGTARG_Is_OP_Speculative(op) ||
      CGTARG_Is_OP_Speculative(opinfo->in_op) ||
      CGTARG_Is_OP_Check_Load(op)  ||
      CGTARG_Is_OP_Check_Load(opinfo->in_op)  ||
      BB_recovery(OP_bb(op))   ||
      BB_recovery(OP_bb(opinfo->in_op)))    return FALSE;

  /* ld.fill and st.spill access UNAT, they should not be safely deleted 
   * because currently the EBO does not analyze the UNAT bit.
   */
  if (CGTARG_Load_with_UNAT (op) || CGTARG_Store_With_UNAT (op)) {
      return FALSE;
  }

  if (OP_load(op) && OP_load(opinfo->in_op)) {
    if (TOP_Find_Operand_Use(OP_code(op), OU_postincr) >= 0) {
     /* The increment must be preserved. */
      if (EBO_Trace_Data_Flow) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"%sIncremented Load for Load - Load combination\n",
                EBO_trace_pfx);
      }
      return FALSE;
    }
    if ((TN_register_class(OP_result(op,0)) !=
                 TN_register_class(OP_result(opinfo->in_op, 0))) ||
        (TN_is_fpu_int(OP_result(op,0)) !=
                 TN_is_fpu_int(OP_result(opinfo->in_op, 0)))) {
      if (EBO_Trace_Data_Flow) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"%sType mismatch for Load - Load combination\n",
                EBO_trace_pfx);
      }
      
      return FALSE;
    }
    if ((size_pred != size_succ) ||
        (OP_results(op) != OP_results(opinfo->in_op)) ||
        (TN_size(OP_result(opinfo->in_op, 0)) != TN_size(OP_result(op, 0)))) {
     /* Size of the data item loaded by the two loads is different,
        but the starting memory address is the same.  There is a chance
        that the predecessor load is a load-pair and that the new load
        matches one of the words that is loaded. */

      if (EBO_Trace_Data_Flow) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"%sSize mismatch for Load - Load combination: %d:%d %d:%d \n",
                EBO_trace_pfx,size_pred,size_succ,
                TN_size(OP_result(opinfo->in_op, 0)),TN_size(OP_result(op, 0)));
      }

      return delete_subset_mem_op (op, opnd_tninfo, opinfo, 0, 0);
    }
    if (!EBO_in_peep &&
        (OP_bb(op) != OP_bb(opinfo->in_op)) &&
        !TN_Is_Constant(OP_result(opinfo->in_op, 0)) &&
        has_assigned_reg(OP_result(opinfo->in_op, 0))) {
      if (EBO_Trace_Data_Flow) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"%sShouldn't move dedicated register references across blocks.\n",
                EBO_trace_pfx);
      }
      return FALSE;
    }
    if (EBO_Trace_Optimization) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile,"%sRemove Load - Load combination\n",EBO_trace_pfx);
    }

    EBO_Exp_COPY(predicate_tn, OP_result(op, 0), OP_result(opinfo->in_op, 0), &ops);
    if (OP_results(op) == 2) {
      EBO_Exp_COPY(predicate_tn, OP_result(op, 1), OP_result(opinfo->in_op, 1), &ops);
    }
    if (EBO_in_loop) EBO_OPS_omega (&ops, (OP_has_predicate(op)?opnd_tninfo[OP_PREDICATE_OPND]:NULL));
    OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
    BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
    return TRUE;
  } else if (OP_load(op) && OP_store(opinfo->in_op)) {
   /* Replace the result tn of the second OP for:
          Store - Load
   */
    INT storeval_idx = TOP_Find_Operand_Use(OP_code(opinfo->in_op),OU_storeval);
    TN *storeval_tn = OP_opnd(opinfo->in_op, storeval_idx);
    if (TOP_Find_Operand_Use(OP_code(op), OU_postincr) >= 0) {
     /* The increment must be preserved. */
      if (EBO_Trace_Data_Flow) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"%sIncremented Load for Load - Store combination\n",
                EBO_trace_pfx);
      }
      return FALSE;
    }
    if ((TN_register_class(OP_result(op,0)) !=
                 TN_register_class(storeval_tn)) ||
        (TN_is_fpu_int(OP_result(op,0)) !=
                 TN_is_fpu_int(storeval_tn))) {
      if (EBO_Trace_Data_Flow) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"%sType mismatch for Store - Load combination\n",
                EBO_trace_pfx);
      }
      return FALSE;
    }
    if (!EBO_in_peep &&
        (OP_bb(op) != OP_bb(opinfo->in_op)) &&
        !TN_Is_Constant(storeval_tn) &&
        has_assigned_reg(storeval_tn)) {
      if (EBO_Trace_Data_Flow) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"%sShouldn't move dedicated register references across blocks.\n",
                EBO_trace_pfx);
      }
      return FALSE;
    }

    if (size_pred == size_succ) {
     /* Size of source and target are the same. */
      if (TN_size(storeval_tn) > size_succ) {
       /* Although the size of the data moved to and from memory is
          the same, the size of the generated value is larger than
          the size of the value we want to load.  Call another
          routine to mask off the upper portion of the stored value. */

        if (EBO_Trace_Data_Flow) {
          #pragma mips_frequency_hint NEVER
          fprintf(TFile,"%sSize mismatch for Store - Load combination: %d %d %d\n",
                  EBO_trace_pfx,size_pred,TN_size(storeval_tn),size_succ);
        }

        return delete_subset_mem_op (op, opnd_tninfo, opinfo, 0, 0);
      }
      if (EBO_Trace_Optimization) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"%sRemove Store - Load combination\n",EBO_trace_pfx);
      }

      EBO_Exp_COPY(predicate_tn, OP_result(op, 0), storeval_tn, &ops);
      if (EBO_in_loop) EBO_Set_OP_omega (OPS_first(&ops),
                                         (OP_has_predicate(op)?opnd_tninfo[OP_PREDICATE_OPND]:NULL),
                                         opinfo->actual_opnd[storeval_idx]);
      OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
      BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
      return TRUE;
    } else {
     /* Size of source and target are different,
        but the starting memory address is the same.
        Call another routine to do the work. */
      return delete_subset_mem_op (op, opnd_tninfo, opinfo, 0, 0);
    }
  } else if (OP_store(op) && OP_store(opinfo->in_op) &&
             (OP_bb(op) == OP_bb(opinfo->in_op))) {
   /* Remove the first OP for:
          Store - Store
   */
    if (size_pred != size_succ) return FALSE;
    if (opinfo->op_must_not_be_removed) return FALSE;
    if (TOP_Find_Operand_Use(OP_code(opinfo->in_op), OU_postincr) >= 0) {
     /* The increment must be preserved. */
      if (EBO_Trace_Data_Flow) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"%sIncremented Store for Store - Store combination\n",
                EBO_trace_pfx);
      }
      return FALSE;
    }
    if (EBO_Trace_Optimization) {
      fprintf(TFile,"%sRemove Store - Store combination\n",EBO_trace_pfx);
    }
    remove_op (opinfo);
    OP_Change_To_Noop(opinfo->in_op);
    opinfo->in_op = NULL;
    opinfo->in_bb = NULL;
    return FALSE;
  } else if (OP_load(opinfo->in_op) && OP_store(op)) {
   /* The store may not be needed if the same value is put back for:
          Load - Store
   */
    INT storeval_idx = TOP_Find_Operand_Use(OP_code(op),OU_storeval);
    TN *storeval_tn = OP_opnd(op, storeval_idx);

    if (TOP_Find_Operand_Use(OP_code(op), OU_postincr) >= 0) {
     /* The increment must be preserved. */
      if (EBO_Trace_Data_Flow) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"%sIncremented Store for Load - Store combination\n",
                EBO_trace_pfx);
      }
      return FALSE;
    }
    if ((TN_register_class(OP_result(opinfo->in_op,0)) !=
                 TN_register_class(storeval_tn)) ||
        (TN_is_fpu_int(OP_result(opinfo->in_op,0)) !=
                 TN_is_fpu_int(storeval_tn))) {
      if (EBO_Trace_Data_Flow) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"%sType mismatch for Load - Store combination\n",
                EBO_trace_pfx);
      }
      return FALSE;
    }
    if (!EBO_in_peep &&
        (OP_bb(op) != OP_bb(opinfo->in_op)) &&
        !TN_Is_Constant(storeval_tn) &&
        has_assigned_reg(storeval_tn)) {
      if (EBO_Trace_Data_Flow) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"%sShouldn't move dedicated register references across blocks.\n",
                EBO_trace_pfx);
      }
      return FALSE;
    }

    if ((size_pred != size_succ) ||
        (TN_size(storeval_tn) != size_succ)) {

      if (EBO_Trace_Data_Flow) {
       #pragma mips_frequency_hint NEVER
        fprintf(TFile,"%sSize mismatch for Load - Store combination: %d %d %d\n",
                EBO_trace_pfx,size_pred,TN_size(storeval_tn),size_succ);
      }

      return FALSE;
    }

    if ((opnd_tninfo[storeval_idx] != NULL) &&
        (opnd_tninfo[storeval_idx]->in_op != NULL) &&
        (opinfo->in_op != NULL) &&
        (opnd_tninfo[storeval_idx]->in_op == opinfo->in_op)) {
     /* The Store is not needed! */

      if (EBO_Trace_Optimization) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"%sRemove Load - Store combination\n",EBO_trace_pfx);
      }

      return TRUE;
    }
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

  if ((Opt_Level < 2) && (OP_bb(op) != opinfo->in_bb)) {
    /* Global TN's aren't supported at low levels of optimization. */
    return FALSE;
  }

  if (OP_has_predicate(op) &&
      (OP_opnd(op,OP_PREDICATE_OPND) != OP_opnd(opinfo->in_op,OP_PREDICATE_OPND))) {
    OP *pred_op = opinfo->in_op;
    TN *predicate1_tn = OP_opnd(pred_op,OP_PREDICATE_OPND);
    TN *predicate2_tn = OP_opnd(op,OP_PREDICATE_OPND);
    EBO_TN_INFO *predicate1_info = opinfo->actual_opnd[OP_PREDICATE_OPND];
    EBO_TN_INFO *predicate2_info = opnd_tninfo[OP_PREDICATE_OPND];

    if ((OP_code(op) == OP_code(pred_op)) &&
        EBO_predicate_complements(predicate1_tn, predicate1_info,
                                  predicate2_tn, predicate2_info)) {
      return hoist_predicate_of_duplicate_complement (op, opnd_tninfo, opinfo);
    }
    if (OP_store(pred_op) && OP_store(op)) {
      if (!EBO_predicate_dominates(predicate2_tn, predicate2_info,
                                   predicate1_tn, predicate1_info)) {
        if (EBO_Trace_Data_Flow) {
          fprintf(TFile,"%sStores can not be combined because predicates do not match\n",EBO_trace_pfx);
        }
        return FALSE;
      }
    } else {
      if (!EBO_predicate_dominates(predicate1_tn, predicate1_info,
                                   predicate2_tn, predicate2_info)) {
        if (EBO_Trace_Data_Flow) {
          fprintf(TFile,"%sExpressions can not be combined because predicates do not match\n",EBO_trace_pfx);
        }
        return FALSE;
      }
    }
  }

  if (OP_memory(op)) {
   /* Separate load/store processing, but logically it's just a special case. */
    return delete_memory_op (op, opnd_tninfo, opinfo);
  } else if ((OP_results(op) == 2) &&
             ((OP_result(op,0) != NULL) &&
              ((OP_result(op,0) == True_TN) ||
               (TN_register_class(OP_result(op,0)) == ISA_REGISTER_CLASS_predicate)))) {
   /* Integer and float comparison operations. */
    return condition_redundant (op, opnd_tninfo, opinfo);
  } else {

    if (OP_has_predicate(op) &&
        (OP_opnd(op,OP_PREDICATE_OPND) != True_TN)) {
     /* Check for required copies of predicates that can not be supported. */

      for (resnum = 0; resnum < OP_results(op); resnum++) {
        if (TN_register_class(OP_result(op, resnum)) == ISA_REGISTER_CLASS_predicate) {
          if (EBO_Trace_Data_Flow) {
            fprintf(TFile,"%sStores can not be combined because predicate copies can not be predicated\n",
                   EBO_trace_pfx);
          }
          return FALSE;
        }
      }
    }

   /* Create copies of the result TN's. */

    for (resnum = 0; resnum < OP_results(op); resnum++) {
      TN *rslt = OP_result(op, resnum);
      if (TN_register_class(rslt) == ISA_REGISTER_CLASS_predicate) {
        Build_OP (TOP_cmp_eq_unc, rslt, True_TN, OP_result(opinfo->in_op, resnum), Zero_TN, Zero_TN, &ops);
      } else {
        EBO_Exp_COPY((OP_has_predicate(op)?OP_opnd(op,OP_PREDICATE_OPND):NULL),
                     rslt, OP_result(opinfo->in_op, resnum), &ops);
      }
      OP_srcpos(OPS_last(&ops)) = OP_srcpos(op);
    }

    if (EBO_in_loop) EBO_OPS_omega (&ops, OP_has_predicate(op)?opnd_tninfo[OP_PREDICATE_OPND]:NULL);
    BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
    return TRUE;
  }
  return FALSE;
}



/* ===================================================================== */
  
/*
 * Given a register compare instruction, return the immediate form.
 * Return TOP_UNDEFINED if there is no immediate form.
 */
static TOP Immediate_Compare(TOP opcode)
{
  switch (opcode) {
  case TOP_cmp_eq:       return TOP_cmp_i_eq;
  case TOP_cmp_eq_unc:   return TOP_cmp_i_eq_unc;
  case TOP_cmp_ne:       return TOP_cmp_i_ne;
  case TOP_cmp_ne_unc:   return TOP_cmp_i_ne_unc;
  case TOP_cmp_ge:       return TOP_cmp_i_ge;
  case TOP_cmp_ge_unc:   return TOP_cmp_i_ge_unc;
  case TOP_cmp_geu:      return TOP_cmp_i_geu;
  case TOP_cmp_geu_unc:  return TOP_cmp_i_geu_unc;
  case TOP_cmp_lt:       return TOP_cmp_i_lt;
  case TOP_cmp_lt_unc:   return TOP_cmp_i_lt_unc;
  case TOP_cmp_ltu:      return TOP_cmp_i_ltu;
  case TOP_cmp_ltu_unc:  return TOP_cmp_i_ltu_unc;
  case TOP_cmp_gt:       return TOP_cmp_i_gt;
  case TOP_cmp_gt_unc:   return TOP_cmp_i_gt_unc;
  case TOP_cmp_gtu:      return TOP_cmp_i_gtu;
  case TOP_cmp_gtu_unc:  return TOP_cmp_i_gtu_unc;
  case TOP_cmp_le:       return TOP_cmp_i_le;
  case TOP_cmp_le_unc:   return TOP_cmp_i_le_unc;
  case TOP_cmp_leu:      return TOP_cmp_i_leu;
  case TOP_cmp_leu_unc:  return TOP_cmp_i_leu_unc;
  case TOP_cmp4_eq:      return TOP_cmp4_i_eq;
  case TOP_cmp4_eq_unc:  return TOP_cmp4_i_eq_unc;
  case TOP_cmp4_ne:      return TOP_cmp4_i_ne;
  case TOP_cmp4_ne_unc:  return TOP_cmp4_i_ne_unc;
  case TOP_cmp4_ge:      return TOP_cmp4_i_ge;
  case TOP_cmp4_ge_unc:  return TOP_cmp4_i_ge_unc;
  case TOP_cmp4_geu:     return TOP_cmp4_i_geu;
  case TOP_cmp4_geu_unc: return TOP_cmp4_i_geu_unc;
  case TOP_cmp4_lt:      return TOP_cmp4_i_lt;
  case TOP_cmp4_lt_unc:  return TOP_cmp4_i_lt_unc;
  case TOP_cmp4_ltu:     return TOP_cmp4_i_ltu;
  case TOP_cmp4_ltu_unc: return TOP_cmp4_i_ltu_unc;
  case TOP_cmp4_gt:      return TOP_cmp4_i_gt;
  case TOP_cmp4_gt_unc:  return TOP_cmp4_i_gt_unc;
  case TOP_cmp4_gtu:     return TOP_cmp4_i_gtu;
  case TOP_cmp4_gtu_unc: return TOP_cmp4_i_gtu_unc;
  case TOP_cmp4_le:      return TOP_cmp4_i_le;
  case TOP_cmp4_le_unc:  return TOP_cmp4_i_le_unc;
  case TOP_cmp4_leu:     return TOP_cmp4_i_leu;
  case TOP_cmp4_leu_unc: return TOP_cmp4_i_leu_unc;
  }
  return TOP_UNDEFINED;
}


/*
 * Given a register compare instruction, return the instruction which
 * performs the reversed comparison, i.e. it produces the same results
 * as the original comparison, but with the operands reversed.
 * Return TOP_UNDEFINED if there is no reversed comparison.
 */
static TOP Reverse_Compare(TOP opcode)
{
  switch (opcode) {
  case TOP_cmp_eq:
  case TOP_cmp_eq_unc:
  case TOP_cmp_ne:
  case TOP_cmp_ne_unc:
  case TOP_cmp4_eq:
  case TOP_cmp4_eq_unc:
  case TOP_cmp4_ne:
  case TOP_cmp4_ne_unc:
    return opcode;
  case TOP_cmp_ge:       return TOP_cmp_le;
  case TOP_cmp_ge_unc:   return TOP_cmp_le_unc;
  case TOP_cmp_geu:      return TOP_cmp_leu;
  case TOP_cmp_geu_unc:  return TOP_cmp_leu_unc;
  case TOP_cmp_lt:       return TOP_cmp_gt;
  case TOP_cmp_lt_unc:   return TOP_cmp_gt_unc;
  case TOP_cmp_ltu:      return TOP_cmp_gtu;
  case TOP_cmp_ltu_unc:  return TOP_cmp_gtu_unc;
  case TOP_cmp_gt:       return TOP_cmp_lt;
  case TOP_cmp_gt_unc:   return TOP_cmp_lt_unc;
  case TOP_cmp_gtu:      return TOP_cmp_ltu;
  case TOP_cmp_gtu_unc:  return TOP_cmp_ltu_unc;
  case TOP_cmp_le:       return TOP_cmp_ge;
  case TOP_cmp_le_unc:   return TOP_cmp_ge_unc;
  case TOP_cmp_leu:      return TOP_cmp_geu;
  case TOP_cmp_leu_unc:  return TOP_cmp_geu_unc;
  case TOP_cmp4_ge:      return TOP_cmp4_le;
  case TOP_cmp4_ge_unc:  return TOP_cmp4_le_unc;
  case TOP_cmp4_geu:     return TOP_cmp4_leu;
  case TOP_cmp4_geu_unc: return TOP_cmp4_leu_unc;
  case TOP_cmp4_lt:      return TOP_cmp4_gt;
  case TOP_cmp4_lt_unc:  return TOP_cmp4_gt_unc;
  case TOP_cmp4_ltu:     return TOP_cmp4_gtu;
  case TOP_cmp4_ltu_unc: return TOP_cmp4_gtu_unc;
  case TOP_cmp4_gt:      return TOP_cmp4_lt;
  case TOP_cmp4_gt_unc:  return TOP_cmp4_lt_unc;
  case TOP_cmp4_gtu:     return TOP_cmp4_ltu;
  case TOP_cmp4_gtu_unc: return TOP_cmp4_ltu_unc;
  case TOP_cmp4_le:      return TOP_cmp4_ge;
  case TOP_cmp4_le_unc:  return TOP_cmp4_ge_unc;
  case TOP_cmp4_leu:     return TOP_cmp4_geu;
  case TOP_cmp4_leu_unc: return TOP_cmp4_geu_unc;
  }
  return TOP_UNDEFINED;
}


/*
 * Normalize an immediate value for use with a specific instruction.
 * In some cases the instruction may ignore parts of the immediate,
 * so we mimic that here.
 */
static INT64 Normalize_Immediate(TOP opcode, INT64 immed)
{
  switch (opcode) {
  case TOP_cmp4_i_eq:
  case TOP_cmp4_i_eq_unc:
  case TOP_cmp4_i_eq_and:
  case TOP_cmp4_i_eq_or:
  case TOP_cmp4_i_eq_or_andcm:
  case TOP_cmp4_i_ne_and:
  case TOP_cmp4_i_ne_or:
  case TOP_cmp4_i_ne_or_andcm:
  case TOP_cmp4_i_lt:
  case TOP_cmp4_i_lt_unc:
  case TOP_cmp4_i_eq_orcm:
  case TOP_cmp4_i_eq_andcm:
  case TOP_cmp4_i_eq_and_orcm:
  case TOP_cmp4_i_ne:
  case TOP_cmp4_i_ne_unc:
  case TOP_cmp4_i_ne_orcm:
  case TOP_cmp4_i_ne_andcm:
  case TOP_cmp4_i_ne_and_orcm:
  case TOP_cmp4_i_le:
  case TOP_cmp4_i_le_unc:
  case TOP_cmp4_i_gt:
  case TOP_cmp4_i_gt_unc:
  case TOP_cmp4_i_ge:
  case TOP_cmp4_i_ge_unc:
    /*
     * For 32-bit signed compares sign-extend the constant to 64 bits.
     */
    return SEXT_32(immed);
  case TOP_cmp4_i_ltu:
  case TOP_cmp4_i_ltu_unc:
  case TOP_cmp4_i_leu:
  case TOP_cmp4_i_leu_unc:
  case TOP_cmp4_i_gtu:
  case TOP_cmp4_i_gtu_unc:
  case TOP_cmp4_i_geu:
  case TOP_cmp4_i_geu_unc:
    /*
     * For 32-bit unsigned compares, zero-extend the constant to 64 bits.
     */
    return TRUNC_32(immed);
  }
  return immed;
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
  BB *bb = OP_bb(op);
  INT opndnum = OP_opnds(op);
  EBO_TN_INFO *tninfo;

  TOP opcode = OP_code(op);
  TN *tnr = OP_result(op,0);
  INT l0_opnd1_idx;
  INT l0_opnd2_idx;
  TN *tn0;
  TN *tn1;
  INT64 const_val;

  OP *pred_op;
  TOP pred_opcode;
  EBO_OP_INFO *pred_opinfo;

  if (EBO_Trace_Execution) {
    #pragma mips_frequency_hint NEVER
    INT i;
    fprintf(TFile, "%sin BB:%d special0 OP :- %s",
            EBO_trace_pfx, BB_id(OP_bb(op)),TOP_Name(OP_code(op)));
    for (i = 0; i < opndnum; i++) {
      fprintf(TFile," ");
      Print_TN(opnd_tn[i],FALSE);
    }
    fprintf(TFile,"\n");
  }

  if (!OP_has_predicate(op)) return FALSE;
  if (OP_opnds(op) < 1) return FALSE;

  if (TN_register_class(tnr) == ISA_REGISTER_CLASS_float) {
    if (OP_opnds(op) < 4) return FALSE;
    l0_opnd1_idx = 2;
    l0_opnd2_idx = 3;
    tn0 = opnd_tn[l0_opnd1_idx];
    tn1 = opnd_tn[l0_opnd2_idx];

    if (tn0 == FZero_TN) {
      if ((IEEE_Arithmetic >= IEEE_INEXACT) &&
          ((opcode == TOP_fmpy) ||
           (opcode == TOP_fmpy_s) ||
           (opcode == TOP_fmpy_d))) {
        OPS ops = OPS_EMPTY;
if (EBO_Trace_Optimization) fprintf(TFile,"replace fmpy of 0.0 with 0.0\n");
        EBO_Exp_COPY((OP_has_predicate(op)?OP_opnd(op,OP_PREDICATE_OPND):NULL),
                     tnr, FZero_TN, &ops);
        if (EBO_in_loop) EBO_Set_OP_omega (OPS_first(&ops),
                                           OP_has_predicate(op)?opnd_tninfo[OP_PREDICATE_OPND]:NULL,
                                           NULL);
        OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
        BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
        return TRUE;
      }
      if ((opcode == TOP_fadd) ||
          (opcode == TOP_fadd_s) ||
          (opcode == TOP_fadd_d)) {
        OPS ops = OPS_EMPTY;
if (EBO_Trace_Optimization) fprintf(TFile,"replace fadd of 0.0 with tn1\n");
        EBO_Exp_COPY((OP_has_predicate(op)?OP_opnd(op,OP_PREDICATE_OPND):NULL),
                     tnr, tn1, &ops);
        if (EBO_in_loop) EBO_Set_OP_omega (OPS_first(&ops),
                                           OP_has_predicate(op)?opnd_tninfo[OP_PREDICATE_OPND]:NULL,
                                           opnd_tninfo[l0_opnd2_idx]);
        OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
        BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
        return TRUE;
      }
    }
    if (tn0 == FOne_TN) {
      if ((opcode == TOP_fmpy) ||
          (opcode == TOP_fmpy_s) ||
          (opcode == TOP_fmpy_d)) {
        OPS ops = OPS_EMPTY;
if (EBO_Trace_Optimization) fprintf(TFile,"replace fmpy of 1.0 with tn1\n");
        EBO_Exp_COPY((OP_has_predicate(op)?OP_opnd(op,OP_PREDICATE_OPND):NULL),
                     tnr, tn1, &ops);
        if (EBO_in_loop) EBO_Set_OP_omega (OPS_first(&ops),
                                           OP_has_predicate(op)?opnd_tninfo[OP_PREDICATE_OPND]:NULL,
                                           opnd_tninfo[l0_opnd2_idx]);
        OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
        BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
        return TRUE;
      }
    }
  }

  if ((TN_register_class(tnr) != ISA_REGISTER_CLASS_integer) &&
      (TN_register_class(tnr) != ISA_REGISTER_CLASS_predicate)) {
   /* Can only do arithmatic on integers. */
    return FALSE;
  }

  l0_opnd1_idx = 1;
  l0_opnd2_idx = (OP_opnds(op) > 1) ? 2 : -1;
  tn0 = opnd_tn[l0_opnd1_idx];
  tn1 = (l0_opnd2_idx >= 0) ? opnd_tn[l0_opnd2_idx] : NULL;
  const_val = TN_Value(tn0);

  if (TN_is_symbol(tn0)) {
   /* Re-located constants not supported. */
    return FALSE;
  }

  if (TN_Is_Constant(tn1)) {
   /* Why are we here? */
    return FALSE;
  }

  if (((TN_register_class(tn1) != ISA_REGISTER_CLASS_integer) &&
       (TN_register_class(tn1) != ISA_REGISTER_CLASS_predicate)) ||
      (TN_is_fpu_int(tnr) != TN_is_fpu_int(tn1))) {
   /* Type changes indicate that something tricky is going on. */
    if (EBO_Trace_Data_Flow) {
      fprintf(TFile,"%sType mismatch between result and operand\n",EBO_trace_pfx);
    }
    return FALSE;
  }

  if ((const_val == 0) &&
      ((opcode == TOP_shl)    ||
       (opcode == TOP_shl_i)  ||
       (opcode == TOP_shr_u)  ||
       (opcode == TOP_shr_i)  ||
       (opcode == TOP_shr_i_u)||
       (opcode == TOP_shrp)   ||
       (opcode == TOP_and)    ||
       (opcode == TOP_and_i))) {
    OPS ops = OPS_EMPTY;
if (EBO_Trace_Optimization) fprintf(TFile,"shift of zero\n");
    EBO_Exp_COPY((OP_has_predicate(op)?OP_opnd(op,OP_PREDICATE_OPND):NULL),
                 tnr, Zero_TN, &ops);
    if (EBO_in_loop) EBO_OPS_omega (&ops, OP_has_predicate(op)?opnd_tninfo[OP_PREDICATE_OPND]:NULL);
    OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
    BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
    return TRUE;
  }

  if ((opcode == TOP_add) &&
      (!TN_Is_Constant(tn1)) &&
      (ISA_LC_Value_In_Class ( const_val, LC_i14))) {
    OP *new_op;
    new_op = Mk_OP(TOP_adds, tnr, OP_opnd(op,OP_PREDICATE_OPND),
                   Gen_Literal_TN(const_val, TN_size(tn1)), tn1);
    OP_srcpos(new_op) = OP_srcpos(op);
    if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND], NULL, opnd_tninfo[l0_opnd2_idx] );
    BB_Insert_Op_After(OP_bb(op), op, new_op);
if (EBO_Trace_Optimization) fprintf(TFile,"Replace add with adds\n");
    return TRUE;
  }

/* Skip transformations to addl.  Registers can not be properly assigned for the instruction.
  if ((opcode == TOP_add) &&
      (!TN_Is_Constant(tn1)) &&
      (!has_assigned_reg(tnr)) &&
      (ISA_LC_Value_In_Class ( const_val, LC_i22))) {
    OP *new_op;
    new_op = Mk_OP(TOP_addl, tnr, OP_opnd(op,OP_PREDICATE_OPND),
                   Gen_Literal_TN(const_val, TN_size(tn1)), tn1);
    OP_srcpos(new_op) = OP_srcpos(op);
    if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND], NULL, opnd_tninfo[l0_opnd2_idx] );
    BB_Insert_Op_After(OP_bb(op), op, new_op);
if (EBO_Trace_Optimization) fprintf(TFile,"Replace add with addl\n");
    return TRUE;
  }
end skip */

  if ((opcode == TOP_addl) &&
      (!TN_Is_Constant(tn1)) &&
      (ISA_LC_Value_In_Class ( const_val, LC_i14))) {
    OP *new_op;
    TOP new_opcode = TOP_adds;
    new_op = Mk_OP(new_opcode, tnr, OP_opnd(op,OP_PREDICATE_OPND),
                   Gen_Literal_TN(const_val, TN_size(tn1)), tn1);
    OP_srcpos(new_op) = OP_srcpos(op);
    if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND], NULL, opnd_tninfo[l0_opnd2_idx] );
    BB_Insert_Op_After(OP_bb(op), op, new_op);
if (EBO_Trace_Optimization) fprintf(TFile,"Replace addl with adds\n");
    return TRUE;
  }

  if ((opcode == TOP_sub) &&
      (!TN_Is_Constant(tn1)) &&
      (ISA_LC_Value_In_Class ( const_val, LC_i8))) {
    OP *new_op;
    TOP new_opcode = TOP_sub_i;
    new_op = Mk_OP(new_opcode, tnr, OP_opnd(op,OP_PREDICATE_OPND),
                   Gen_Literal_TN(const_val, TN_size(tn1)), tn1);
    OP_srcpos(new_op) = OP_srcpos(op);
    if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND], NULL, opnd_tninfo[l0_opnd2_idx] );
    BB_Insert_Op_After(OP_bb(op), op, new_op);
if (EBO_Trace_Optimization) fprintf(TFile,"Replace sub with sub_i\n");
    return TRUE;
  }

  if (opcode == TOP_and) {
    if (ISA_LC_Value_In_Class (const_val, LC_i8)) {
      OP *new_op;
if (EBO_Trace_Optimization) fprintf(TFile,"replace and with and_i\n");
      new_op = Mk_OP(TOP_and_i, tnr, OP_opnd(op,OP_PREDICATE_OPND),
                     Gen_Literal_TN(const_val, TN_size(tn1)), tn1);
      OP_srcpos(new_op) = OP_srcpos(op);
      if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND],
                                          NULL, opnd_tninfo[l0_opnd2_idx]);
      BB_Insert_Op_After(OP_bb(op), op, new_op);
      return TRUE;
    } else if (!EBO_in_loop) {
   /* Look for a mask in the constant and replace the and(load-immediate,tn) with an extr. */
   /* Note: we don't know exactly what instructions will be generated by the call to       */
   /* Expand_Special_And_Immed, so we don't know how big to make the new omega entry.      */
      OPS ops = OPS_EMPTY;
      if (Expand_Special_And_Immed (tnr, tn1, const_val, &ops)) {
if (EBO_Trace_Optimization) fprintf(TFile,"replace and with bit move\n");
        Set_OP_opnd(OPS_first(&ops), OP_PREDICATE_OPND, OP_opnd(op,OP_PREDICATE_OPND));
        OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
        BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
        return TRUE;
      }
    }
  }

  if (opcode == TOP_andcm) {
    if (ISA_LC_Value_In_Class (const_val, LC_i8)) {
      OP *new_op;
if (EBO_Trace_Optimization) fprintf(TFile,"replace andcm with andcm_i\n");
      new_op = Mk_OP(TOP_andcm_i, tnr, OP_opnd(op,OP_PREDICATE_OPND),
                     Gen_Literal_TN(const_val, TN_size(tn1)), tn1);
      OP_srcpos(new_op) = OP_srcpos(op);
      if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND],
                                          NULL, opnd_tninfo[l0_opnd2_idx]);
      BB_Insert_Op_After(OP_bb(op), op, new_op);
      return TRUE;
    }
  }

  if ((opcode == TOP_or) &&
      (ISA_LC_Value_In_Class (const_val, LC_i8))) {
    OP *new_op;
if (EBO_Trace_Optimization) fprintf(TFile,"replace or with or_i\n");
    new_op = Mk_OP(TOP_or_i, tnr, OP_opnd(op,OP_PREDICATE_OPND),
                   Gen_Literal_TN(const_val, TN_size(tn1)), tn1);
    OP_srcpos(new_op) = OP_srcpos(op);
    if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND], NULL, opnd_tninfo[l0_opnd2_idx]);
    BB_Insert_Op_After(OP_bb(op), op, new_op);
    return TRUE;
  }

  if ((opcode == TOP_xor) &&
      (ISA_LC_Value_In_Class (const_val, LC_i8))) {
    OP *new_op;
if (EBO_Trace_Optimization) fprintf(TFile,"replace xor with xor_i\n");
    new_op = Mk_OP(TOP_xor_i, tnr, OP_opnd(op,OP_PREDICATE_OPND),
                   Gen_Literal_TN(const_val, TN_size(tn1)), tn1);
    OP_srcpos(new_op) = OP_srcpos(op);
    if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND], NULL, opnd_tninfo[l0_opnd2_idx]);
    BB_Insert_Op_After(OP_bb(op), op, new_op);
    return TRUE;
  }

  if ((opcode == TOP_dep) &&
      ((const_val == 0) || (const_val == ~0))) {
    OP *new_op;
if (EBO_Trace_Optimization) fprintf(TFile,"replace dep with dep_i\n");
    new_op = Mk_OP(TOP_dep_i, tnr, OP_opnd(op,OP_PREDICATE_OPND),
                   Gen_Literal_TN(const_val, TN_size(tn1)), tn1,
                   OP_opnd(op,3), OP_opnd(op,4));
    OP_srcpos(new_op) = OP_srcpos(op);
    if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND],
                                        NULL, opnd_tninfo[l0_opnd2_idx], NULL, NULL);
    BB_Insert_Op_After(OP_bb(op), op, new_op);
    return TRUE;
  }

  if ((opcode == TOP_dep_z) &&
      (ISA_LC_Value_In_Class (const_val, LC_i8))) {
    OP *new_op;
if (EBO_Trace_Optimization) fprintf(TFile,"replace dep_z with dep_z_i\n");
    new_op = Mk_OP(TOP_dep_i, tnr, OP_opnd(op,OP_PREDICATE_OPND),
                   Gen_Literal_TN(const_val, 4), OP_opnd(op,3), OP_opnd(op,4));
    OP_srcpos(new_op) = OP_srcpos(op);
    if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND],
                                        NULL, NULL, NULL);
    BB_Insert_Op_After(OP_bb(op), op, new_op);
    return TRUE;
  }

  if (OP_icmp(op)) {
    TOP new_opcode = Immediate_Compare(opcode);
    INT64 new_const_val = Normalize_Immediate(opcode, const_val);
    if (new_opcode != TOP_UNDEFINED && 
	TOP_Can_Have_Immediate(new_const_val, new_opcode)) {
      TN *r1 = OP_result(op,0);
      TN *r2 = OP_result(op,1);
      OPS ops = OPS_EMPTY;
if (EBO_Trace_Optimization) fprintf(TFile,"replace cmp with cmp_i\n");
      Build_OP (new_opcode, r1, r2, OP_opnd(op,OP_PREDICATE_OPND),
                Gen_Literal_TN(new_const_val, TN_size(tn1)), tn1, &ops);
      if (EBO_in_loop) EBO_Set_OP_omega (OPS_first(&ops), opnd_tninfo[OP_PREDICATE_OPND],
                                         NULL, opnd_tninfo[l0_opnd2_idx]);
      OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
      BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
      return TRUE;
    }
  }

 /* Sequence optimizations. */

  if ((opcode == TOP_adds) &&
      (!TN_Is_Constant(tn1)) &&
      ISA_LC_Value_In_Class (const_val, LC_i9)) {
    TN *tnr = OP_result(op,0);
    OP *test_op = OP_prev(op);
    OP *memory_op = NULL;
    EBO_OP_INFO *memory_opinfo = NULL;
    TOP memory_opcode = TOP_add;
    EBO_TN_INFO *memory_index = NULL;
    EBO_TN_INFO *memory_r1 = NULL;
    EBO_TN_INFO *memory_r2 = NULL;
    EBO_TN_INFO *memory_sv = NULL;

    while (test_op) {
      if (OP_load(test_op) || OP_store(test_op)) {
        memory_op = test_op;
        test_op = NULL;
        break;
      }
      test_op = OP_prev(test_op);
    }

    if (memory_op != NULL) {
      memory_opinfo = find_opinfo_entry (memory_op);
      memory_opcode = OP_code(memory_op);
      if (memory_opinfo != NULL) {
        memory_index = memory_opinfo->actual_opnd[TOP_Find_Operand_Use(memory_opcode, OU_base)];
        if (OP_load(memory_op)) {
          memory_r1 = memory_opinfo->actual_rslt[0];
          if (OP_results(memory_op) > 1) {
	    memory_r2 = memory_opinfo->actual_rslt[1];
	  }
        } else if (OP_store(memory_op)) {
          memory_sv = memory_opinfo->actual_opnd[TOP_Find_Operand_Use(memory_opcode, OU_storeval)];
        }
      }
    }

   /* Make sure that this is load has a predicate that is compatible with the increment. */
    if ((memory_opinfo != NULL) &&
        ((TN_Is_Constant(OP_opnd(op,OP_PREDICATE_OPND)) &&
          TN_Is_Constant(OP_opnd(memory_op,OP_PREDICATE_OPND)) &&
          (TN_Value(OP_opnd(op,OP_PREDICATE_OPND)) == TN_Value(OP_opnd(memory_op,OP_PREDICATE_OPND)))) ||
         (!TN_Is_Constant(OP_opnd(op,OP_PREDICATE_OPND)) &&
          !TN_Is_Constant(OP_opnd(memory_op,OP_PREDICATE_OPND)) &&
          EBO_tn_available(bb, memory_opinfo->actual_opnd[OP_PREDICATE_OPND]) &&
          tn_registers_identical(OP_opnd(op,OP_PREDICATE_OPND), OP_opnd(memory_op,OP_PREDICATE_OPND)))) &&
        (OP_memory(memory_op)) &&
        ((memory_index != NULL) &&
         EBO_tn_available(bb, memory_index) &&
         tn_registers_identical(memory_index->local_tn, tn1)) &&
        ((memory_r1 == NULL) || (memory_r1->reference_count == 0)) &&
        ((memory_r2 == NULL) || (memory_r2->reference_count == 0)) &&
        ((memory_sv == NULL) || EBO_tn_available(bb, memory_sv)) &&
        ((memory_sv == NULL) || !tn_registers_identical(tnr, memory_sv->local_tn))) {
      TOP new_opcode;
      BOOL use_increment = TRUE;

      switch (memory_opcode) {
      case TOP_ld1: new_opcode = TOP_ld1_i; break;
      case TOP_ld2: new_opcode = TOP_ld2_i; break;
      case TOP_ld4: new_opcode = TOP_ld4_i; break;
      case TOP_ld8: new_opcode = TOP_ld8_i; break;
      case TOP_ld8_fill: new_opcode = TOP_ld8_i_fill; break;
      case TOP_ldfs: new_opcode = TOP_ldfs_i; break;
      case TOP_ldfd: new_opcode = TOP_ldfd_i; break;
      case TOP_ldfe: new_opcode = TOP_ldfe_i; break;
      case TOP_ldf8: new_opcode = TOP_ldf8_i; break;
      case TOP_ldf_fill: new_opcode = TOP_ldf_i_fill; break;
      case TOP_ldfps:
        new_opcode = TOP_ldfps_i;
        if (CGTARG_Mem_Ref_Bytes(memory_op) != const_val) use_increment = FALSE;
        break;
      case TOP_ldfpd:
        new_opcode = TOP_ldfpd_i;
        if (CGTARG_Mem_Ref_Bytes(memory_op) != const_val) use_increment = FALSE;
        break;
      case TOP_ldfp8:
        new_opcode = TOP_ldfp8_i;
        if (CGTARG_Mem_Ref_Bytes(memory_op) != const_val) use_increment = FALSE;
        break;
      case TOP_lfetch: new_opcode = TOP_lfetch_i; break;
      case TOP_lfetch_excl: new_opcode = TOP_lfetch_i_excl; break;
      case TOP_lfetch_fault: new_opcode = TOP_lfetch_i_fault; break;
      case TOP_lfetch_fault_excl: new_opcode = TOP_lfetch_i_fault_excl; break;
      case TOP_st1: new_opcode = TOP_st1_i; break;
      case TOP_st2: new_opcode = TOP_st2_i; break;
      case TOP_st4: new_opcode = TOP_st4_i; break;
      case TOP_st8: new_opcode = TOP_st8_i; break;
      case TOP_st8_spill: new_opcode = TOP_st8_i_spill; break;
      case TOP_stfs: new_opcode = TOP_stfs_i; break;
      case TOP_stfd: new_opcode = TOP_stfd_i; break;
      case TOP_stfe: new_opcode = TOP_stfe_i; break;
      case TOP_stf8: new_opcode = TOP_stf8_i; break;
      case TOP_stf_spill: new_opcode = TOP_stf_i_spill; break;
      case TOP_ldfps_i:
      case TOP_ldfpd_i:
      case TOP_ldfp8_i:
        use_increment = FALSE; break;
      default: 
        if (TOP_Find_Operand_Use(memory_opcode, OU_postincr) >= 0) {
          new_opcode = memory_opcode;
          const_val += TN_Value(OP_opnd(memory_op,TOP_Find_Operand_Use(memory_opcode, OU_postincr)));
          break;
        }
        use_increment = FALSE; break;
      }

     /* CGPREP doesn't like to see these instructions. */
      if (EBO_in_pre || EBO_in_loop) use_increment = FALSE;

     /* Scheduling may be better if we don't introduce dependencies. */
      if (!EBO_in_peep) use_increment = FALSE;
      
      /* Do not defined an upward-exposed-use var on a speculative train
       */
      if (OP_load (memory_op) && OP_speculative(memory_op)) {
         use_increment = FALSE;
      }

      /* load is constantly on a critical path, if we expand an extra mov OP,
       * the path will become even longer
       */
      if (use_increment && OP_load(memory_op) && 
         !tn_registers_identical(tnr, tn1)) {
         use_increment = FALSE; 
      }

      if (use_increment &&
          (TOP_Find_Operand_Use(new_opcode, OU_postincr) >= 0)) {
        OP *new_op;
        OPS ops = OPS_EMPTY;

        if (!tn_registers_identical(tnr, tn1)) {
          EBO_Exp_COPY(OP_opnd(op,OP_PREDICATE_OPND), tnr, tn1, &ops);
          if (EBO_in_loop) EBO_Set_OP_omega (OPS_first(&ops),
                                             opnd_tninfo[OP_PREDICATE_OPND],
                                             opnd_tninfo[l0_opnd2_idx]);
        }

        TN *save_opnds[OP_MAX_FIXED_OPNDS];
        INT i;

 	Is_True(OP_opnds(memory_op) <= OP_MAX_FIXED_OPNDS,
		("save_opnds is too small for %s", TOP_Name(OP_code(memory_op))));
        for (i=0; i<OP_opnds(memory_op); i++) save_opnds[i] = OP_opnd(memory_op, i);

        if (OP_load(memory_op)) {
          if (OP_results(memory_op) == 1) {
            new_op = Mk_OP (new_opcode, OP_result(memory_op, 0), tnr,
                            save_opnds[0], save_opnds[1], save_opnds[2], NULL, NULL);
          } else {
            new_op = Mk_OP (new_opcode, OP_result(memory_op, 0), OP_result(memory_op, 1), tnr,
                            save_opnds[0], save_opnds[1], save_opnds[2], NULL, NULL);
          }
        } else {
          if (OP_opnds(memory_op) == 4) {
            new_op = Mk_OP(new_opcode, tnr, save_opnds[0], save_opnds[1], save_opnds[2], NULL, NULL);
          } else {
            new_op = Mk_OP(new_opcode, tnr, save_opnds[0], save_opnds[1], save_opnds[2], NULL, NULL, NULL);
          }

          Set_OP_opnd(new_op, TOP_Find_Operand_Use(new_opcode, OU_storeval),
                      save_opnds[TOP_Find_Operand_Use(memory_opcode, OU_storeval)]);
        }

        Set_OP_opnd(new_op, TOP_Find_Operand_Use(new_opcode, OU_base), tnr);
        Set_OP_opnd(new_op, TOP_Find_Operand_Use(new_opcode, OU_postincr), Gen_Literal_TN(const_val, 4));
        Copy_WN_For_Memory_OP (new_op, memory_op);
        OP_srcpos(new_op) = OP_srcpos(memory_op);
        OPS_Append_Op( &ops, new_op);

        BB_Insert_Ops(bb, op, &ops, FALSE);

        remove_op (memory_opinfo);
        OP_Change_To_Noop(memory_op);
        memory_opinfo->in_op = NULL;
        memory_opinfo->in_bb = NULL;

if (EBO_Trace_Optimization) fprintf(TFile,"memory op merged with increment to produce %s (inc=%lld).\n",
TOP_Name(new_opcode),const_val);
        return TRUE;
      }
    }
  }

  tninfo = opnd_tninfo[l0_opnd2_idx];
  if (tninfo == NULL) return FALSE;
  pred_op = tninfo->in_op;
  if (pred_op == NULL) return FALSE;
  pred_opcode = OP_code(pred_op);
  if (pred_opcode == TOP_spadjust) return FALSE;
  pred_opinfo = locate_opinfo_entry (tninfo);
  if (pred_opinfo == NULL) return FALSE;

 /* First, look for some special cases involving loads. */
  if (OP_load(pred_op)) {
    if (OP_iand(op)) {
      if (((pred_opcode == TOP_ld1) &&
           (const_val == (~((~0) << 8)))) ||
          ((pred_opcode == TOP_ld2) &&
           (const_val ==  (~((~0) << 16))))) {
       /* No need to mask - it was done with the load. */
        OPS ops = OPS_EMPTY;
if (EBO_Trace_Optimization) fprintf(TFile,"no need to mask after load\n");
        EBO_Exp_COPY((OP_has_predicate(op)?OP_opnd(op,OP_PREDICATE_OPND):NULL),
                     tnr, tn1, &ops);
        if (EBO_in_loop) EBO_Set_OP_omega (OPS_first(&ops),
                                           OP_has_predicate(op)?opnd_tninfo[OP_PREDICATE_OPND]:NULL,
                                           opnd_tninfo[l0_opnd2_idx]);
        OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
        BB_Insert_Ops(bb, op, &ops, FALSE);
        return TRUE;
      }
    }
    return FALSE;
  }

  if ((opcode == TOP_dep) ||
      (opcode == TOP_dep_i)) {

   /* Is the input to this dep instruction another dep instruction? */
    if ((pred_opcode == TOP_dep) ||
        (pred_opcode == TOP_dep_i)) {
      TN *pred_insert = OP_opnd(pred_op,1);
      EBO_TN_INFO *pred_insert_tninfo = pred_opinfo->actual_opnd[1];
      TN *pred_tn = OP_opnd(pred_op,2);
      INT pred_start = TN_Value(OP_opnd(pred_op,3));
      INT pred_length = TN_Value(OP_opnd(pred_op,4));
      INT op_start = TN_Value(OP_opnd(op,3));
      INT op_length = TN_Value(OP_opnd(op,4));

     /* Pick up the best value we can for the predicessor's field value. */
      if (pred_opinfo->optimal_opnd[1] != NULL) {
        pred_insert_tninfo = pred_opinfo->optimal_opnd[1];
        pred_insert = pred_insert_tninfo->local_tn;
      }
      if ((pred_insert_tninfo != NULL) &&
          (pred_insert_tninfo->replacement_tn != NULL)) {
        pred_insert = pred_insert_tninfo->replacement_tn;
        pred_insert_tninfo = pred_insert_tninfo->replacement_tninfo;
      }

     /* The transformations that we want to make will require that
        we utilize a new register.  This is not problem if registers
        have not been assigned, and is not a problem if we can reuse
        the result register of the dep instruction - which we can not
        do if it is going to be an input to the new dep instruction.
     */
      if (has_assigned_reg(tnr) &&
          TNs_Are_Equivalent(tnr,pred_tn)) return FALSE;

      if (TN_Is_Constant(pred_insert) &&
          ((pred_start+pred_length) == op_start) &&
          ((op_length+pred_length) < 16)) {
       /* The new field is to the left of the previous field. */
        OPS ops = OPS_EMPTY;
        OP *new_op;
        TN *predicate_tn = OP_opnd(op,OP_PREDICATE_OPND);
        TN *new_const_tn = has_assigned_reg(tnr) ? tnr : Dup_TN_Even_If_Dedicated(tnr);
        UINT64 mask;

        mask = (~0);
        mask = mask << op_length;
        const_val = (const_val & ~mask) << pred_length;

        mask = (~0);
        mask = mask << pred_length;
        const_val |= (TN_Value(pred_insert) & ~mask);
if (EBO_Trace_Optimization) fprintf(TFile,"combine dep to the left of previous dep\n");
        EBO_Exp_COPY(predicate_tn, new_const_tn, Gen_Literal_TN(const_val, TN_size(tnr)), &ops);
        if (EBO_in_loop) EBO_Set_OP_omega (OPS_first(&ops),
                                           opnd_tninfo[OP_PREDICATE_OPND],
                                           pred_opinfo->actual_opnd[2]);
        OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);

        Build_OP(TOP_dep, tnr, predicate_tn, new_const_tn, OP_opnd(pred_op,2),
                 OP_opnd(pred_op,3), Gen_Literal_TN(op_length+pred_length, 4), &ops);
        if (predicate_tn != True_TN) Set_OP_cond_def_kind(OPS_last(&ops), OP_PREDICATED_DEF);
        OP_srcpos(OPS_last(&ops)) = OP_srcpos(op);
        if (EBO_in_loop) EBO_Set_OP_omega (OPS_last(&ops),
                                           opnd_tninfo[OP_PREDICATE_OPND], NULL,
                                           pred_opinfo->actual_opnd[2]);

        BB_Insert_Ops(bb, op, &ops, FALSE);
        return TRUE;
      }

      if (TN_Is_Constant(pred_insert) &&
          ((op_start+op_length) == pred_start) &&
          ((op_length+pred_length) < 16)) {
       /* The new field is to the right of the previous field. */
        OPS ops = OPS_EMPTY;
        OP *new_op;
        TN *predicate_tn = OP_opnd(op,OP_PREDICATE_OPND);
        TN *new_const_tn = has_assigned_reg(tnr) ? tnr : Dup_TN_Even_If_Dedicated(tnr);
        UINT64 mask;

        mask = (~0);
        mask = mask << op_length;
        const_val = (const_val & ~mask);

        mask = (~0);
        mask = mask << pred_length;
        const_val |= (TN_Value(pred_insert) & ~mask) << op_length;
if (EBO_Trace_Optimization) fprintf(TFile,"combine dep to the right of previous dep\n");
        EBO_Exp_COPY(predicate_tn, new_const_tn, Gen_Literal_TN(const_val, TN_size(tnr)), &ops);
        if (EBO_in_loop) EBO_Set_OP_omega (OPS_first(&ops),
                                           opnd_tninfo[OP_PREDICATE_OPND],
                                           pred_opinfo->actual_opnd[2]);
        OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);

        Build_OP(TOP_dep, tnr, predicate_tn, new_const_tn, OP_opnd(pred_op,2),
                 OP_opnd(op,3), Gen_Literal_TN(op_length+pred_length, 4), &ops);
        if (predicate_tn != True_TN) Set_OP_cond_def_kind(OPS_last(&ops), OP_PREDICATED_DEF);
        OP_srcpos(OPS_last(&ops)) = OP_srcpos(op);
        if (EBO_in_loop) EBO_Set_OP_omega (OPS_last(&ops),
                                           opnd_tninfo[OP_PREDICATE_OPND], NULL,
                                           pred_opinfo->actual_opnd[2]);

        BB_Insert_Ops(bb, op, &ops, FALSE);
        return TRUE;
      }
    }
  }

 /* Now, look into optimizations that depend on specific
    values for the actual operands of the predecessor op. */
  if (TOP_Find_Operand_Use(pred_opcode,OU_opnd2) >= 0) {
    INT ptn0_idx = TOP_Find_Operand_Use(pred_opcode,OU_opnd1);
    INT ptn1_idx = TOP_Find_Operand_Use(pred_opcode,OU_opnd2);
    TN *ptn0 = OP_opnd(pred_op, ptn0_idx);
    TN *ptn1 = OP_opnd(pred_op, ptn1_idx);
    EBO_TN_INFO *ptn0_tninfo;
    EBO_TN_INFO *ptn1_tninfo;
    INT64 pred_val = 0;

    OP *new_op;
    TOP new_opcode;
    INT64 new_const_val;

    ptn0_tninfo = pred_opinfo->actual_opnd[ptn0_idx];
    ptn1_tninfo = pred_opinfo->actual_opnd[ptn1_idx];

   /* Are the operands available for use? */
    if (!(TN_Is_Constant(ptn0) || EBO_tn_available (bb, ptn0_tninfo)) ||
        !(TN_Is_Constant(ptn1) || EBO_tn_available (bb, ptn1_tninfo))) return FALSE;

    if (opcode == TOP_adds) {
      if (((pred_opcode == TOP_add) && (const_val == 1)) ||
          ((pred_opcode == TOP_sub) && (const_val == -1))) {
       /* Use the special add+1 instruction. */
          new_opcode = (pred_opcode == TOP_add) ? TOP_add_1 : TOP_sub_1;
          new_op = Mk_OP (new_opcode, tnr, OP_opnd(op,OP_PREDICATE_OPND), ptn0, ptn1);
          OP_srcpos(new_op) = OP_srcpos(op);
          if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND], ptn0_tninfo, ptn1_tninfo );
          BB_Insert_Op_After(bb, op, new_op);
if (EBO_Trace_Optimization) fprintf(TFile,"combine add and add1\n");
          return TRUE;
      }
    }

    if (TN_Is_Constant(ptn1)) {
    } else if (TN_Is_Constant(ptn0) &&
               (OP_iadd(pred_op) || OP_ior(pred_op)  || OP_iand(pred_op))) {
     /* Treat ptn1 as if it were the constant operand. */
      TN *s = ptn0;
      ptn0 = ptn1;
      ptn0_tninfo = ptn1_tninfo;
      ptn1 = s;
    } else {
      return FALSE;
    }

    pred_val = TN_is_symbol(ptn1) ? TN_offset(ptn1) : TN_Value(ptn1);

   /* Look for compare operations where the constants can be combined. */
    if (OP_icmp(op) && OP_iadd(pred_op) && !TN_is_symbol(ptn1)) {
      INT cmp_size;

      if ((pred_val != 0) &&
          ((opcode == TOP_cmp4_ltu)       ||
           (opcode == TOP_cmp4_ltu_unc)   ||
           (opcode == TOP_cmp4_leu)       ||
           (opcode == TOP_cmp4_leu_unc)   ||
           (opcode == TOP_cmp4_gtu)       ||
           (opcode == TOP_cmp4_gtu_unc)   ||
           (opcode == TOP_cmp4_geu)       ||
           (opcode == TOP_cmp4_geu_unc)   ||
           (opcode == TOP_cmp_ltu)        ||
           (opcode == TOP_cmp_ltu_unc)    ||
           (opcode == TOP_cmp_leu)        ||
           (opcode == TOP_cmp_leu_unc)    ||
           (opcode == TOP_cmp_gtu)        ||
           (opcode == TOP_cmp_gtu_unc)    ||
           (opcode == TOP_cmp_geu)        ||
           (opcode == TOP_cmp_geu_unc)    ||
           (opcode == TOP_cmp4_i_ltu)     ||
           (opcode == TOP_cmp4_i_ltu_unc) ||
           (opcode == TOP_cmp4_i_leu)     ||
           (opcode == TOP_cmp4_i_leu_unc) ||
           (opcode == TOP_cmp4_i_gtu)     ||
           (opcode == TOP_cmp4_i_gtu_unc) ||
           (opcode == TOP_cmp4_i_geu)     ||
           (opcode == TOP_cmp4_i_geu_unc) ||
           (opcode == TOP_cmp_i_ltu)      ||
           (opcode == TOP_cmp_i_ltu_unc)  ||
           (opcode == TOP_cmp_i_leu)      ||
           (opcode == TOP_cmp_i_leu_unc)  ||
           (opcode == TOP_cmp_i_gtu)      ||
           (opcode == TOP_cmp_i_gtu_unc)  ||
           (opcode == TOP_cmp_i_geu)      ||
           (opcode == TOP_cmp_i_geu_unc))) {
       /* There may be sign-bit games going on with unsigned compares. */
        return FALSE;
      }
      new_const_val = const_val - pred_val; 

      switch (opcode) {
      case TOP_cmp4_i_eq:
      case TOP_cmp4_i_eq_unc:
      case TOP_cmp4_i_eq_and:
      case TOP_cmp4_i_eq_or:
      case TOP_cmp4_i_eq_or_andcm:
      case TOP_cmp4_i_ne_and:
      case TOP_cmp4_i_ne_or:
      case TOP_cmp4_i_ne_or_andcm:
      case TOP_cmp4_i_lt:
      case TOP_cmp4_i_lt_unc:
      case TOP_cmp4_i_ltu:
      case TOP_cmp4_i_ltu_unc:
      case TOP_cmp4_i_eq_orcm:
      case TOP_cmp4_i_eq_andcm:
      case TOP_cmp4_i_eq_and_orcm:
      case TOP_cmp4_i_ne:
      case TOP_cmp4_i_ne_unc:
      case TOP_cmp4_i_ne_orcm:
      case TOP_cmp4_i_ne_andcm:
      case TOP_cmp4_i_ne_and_orcm:
      case TOP_cmp4_i_le:
      case TOP_cmp4_i_le_unc:
      case TOP_cmp4_i_gt:
      case TOP_cmp4_i_gt_unc:
      case TOP_cmp4_i_ge:
      case TOP_cmp4_i_ge_unc:
      case TOP_cmp4_i_leu:
      case TOP_cmp4_i_leu_unc:
      case TOP_cmp4_i_gtu:
      case TOP_cmp4_i_gtu_unc:
      case TOP_cmp4_i_geu:
      case TOP_cmp4_i_geu_unc:
        new_const_val = Normalize_Immediate(opcode, new_const_val);
      case TOP_cmp_i_eq:
      case TOP_cmp_i_eq_unc:
      case TOP_cmp_i_eq_and:
      case TOP_cmp_i_eq_or:
      case TOP_cmp_i_eq_or_andcm:
      case TOP_cmp_i_ne_and:
      case TOP_cmp_i_ne_or:
      case TOP_cmp_i_ne_or_andcm:
      case TOP_cmp_i_lt:
      case TOP_cmp_i_lt_unc:
      case TOP_cmp_i_ltu:
      case TOP_cmp_i_ltu_unc:
      case TOP_cmp_i_eq_orcm:
      case TOP_cmp_i_eq_andcm:
      case TOP_cmp_i_eq_and_orcm:
      case TOP_cmp_i_ne:
      case TOP_cmp_i_ne_unc:
      case TOP_cmp_i_ne_orcm:
      case TOP_cmp_i_ne_andcm:
      case TOP_cmp_i_ne_and_orcm:
      case TOP_cmp_i_le:
      case TOP_cmp_i_le_unc:
      case TOP_cmp_i_gt:
      case TOP_cmp_i_gt_unc:
      case TOP_cmp_i_ge:
      case TOP_cmp_i_ge_unc:
      case TOP_cmp_i_leu:
      case TOP_cmp_i_leu_unc:
      case TOP_cmp_i_gtu:
      case TOP_cmp_i_gtu_unc:
      case TOP_cmp_i_geu:
      case TOP_cmp_i_geu_unc:
        if (TOP_Can_Have_Immediate(new_const_val, opcode)) {
          OPS ops = OPS_EMPTY;
if (EBO_Trace_Optimization) fprintf(TFile,"merge cmp_i with add: (%lld)\n",new_const_val);
          Build_OP (opcode, OP_result(op,0), OP_result(op,1), OP_opnd(op,OP_PREDICATE_OPND),
                    Gen_Literal_TN(new_const_val, TN_size(tn1)), ptn0, &ops);
          if (EBO_in_loop) EBO_Set_OP_omega (OPS_first(&ops), opnd_tninfo[OP_PREDICATE_OPND],
                                             NULL, ptn0_tninfo );
          OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
          BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
          return TRUE;
        }
        break;
      case TOP_cmp4_lt:
      case TOP_cmp4_lt_unc:
      case TOP_cmp4_le:
      case TOP_cmp4_le_unc:
      case TOP_cmp4_gt:
      case TOP_cmp4_gt_unc:
      case TOP_cmp4_ge:
      case TOP_cmp4_ge_unc:
      case TOP_cmp_lt:
      case TOP_cmp_lt_unc:
      case TOP_cmp_le:
      case TOP_cmp_le_unc:
      case TOP_cmp_gt:
      case TOP_cmp_gt_unc:
      case TOP_cmp_ge:
      case TOP_cmp_ge_unc:
        cmp_size = EBO_bit_length (op);
        if ((cmp_size > 1) &&
            (((const_val >> (cmp_size-1)) &1) != ((new_const_val >> (cmp_size-1)) &1))) {
         /* The sign bit on the constant changed.  This may be a problem! */
          break;
        }
      default:
        if (!has_assigned_reg(tn1)) {
          OPS ops = OPS_EMPTY;
          TN *tnr = Dup_TN_Even_If_Dedicated (OP_opnd(op,1));
          TN *tnc = Gen_Literal_TN(new_const_val, TN_size(tn1));
if (EBO_Trace_Optimization) fprintf(TFile,"merge cmp with add (0x%llx)\n",new_const_val);
          Expand_Immediate (tnr, tnc, (new_const_val < 0) ? TRUE : FALSE, &ops);
          if (OP_has_predicate(op)) {
            EBO_OPS_predicate (OP_opnd(op, OP_PREDICATE_OPND), &ops);
          }
          if (EBO_in_loop) EBO_OPS_omega (&ops, OP_has_predicate(op)?opnd_tninfo[OP_PREDICATE_OPND]:NULL);
          Build_OP (opcode, OP_result(op,0), OP_result(op,1), OP_opnd(op,OP_PREDICATE_OPND),
                    tnr, ptn0, &ops);
          if (EBO_in_loop) EBO_Set_OP_omega (OPS_last(&ops), opnd_tninfo[OP_PREDICATE_OPND],
                                             NULL, ptn0_tninfo );
          OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
          BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
          return TRUE;
        }
        break;
      }

    }

   /* Look for identical operations where the constants can be combined. */
    if (OP_iadd(op) && OP_iadd(pred_op)) {
      new_const_val = pred_val + const_val;

      if (ISA_LC_Value_In_Class (new_const_val, LC_i22)) {
        TN *new_tn;
        new_opcode = (ISA_LC_Value_In_Class (new_const_val, LC_i14)) ? TOP_adds : TOP_addl;
        if (new_opcode == TOP_addl) return FALSE; /* Skip transformations to addl. */

        if (TN_is_symbol(ptn1)) {
          ST *st = TN_var(ptn1);
          ST *base_st;
          INT64 base_ofst;
          INT32 rel = TN_relocs(ptn1);

          Base_Symbol_And_Offset (st, &base_st, &base_ofst);
          if (ST_on_stack(st) &&
              ((ST_sclass(st) == SCLASS_AUTO) ||
               (EBO_in_peep && (ST_sclass(st) == SCLASS_FORMAL))) &&
              ISA_LC_Value_In_Class (new_const_val+base_ofst, LC_i14)) {
            new_tn = Gen_Symbol_TN(st, new_const_val, rel);
          } else return FALSE;
        } else {
          new_tn = Gen_Literal_TN(new_const_val, TN_size(tn1));
        }

        new_op = Mk_OP (new_opcode, tnr, OP_opnd(op,OP_PREDICATE_OPND), new_tn, ptn0);
        OP_srcpos(new_op) = OP_srcpos(op);
        if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND], NULL, ptn0_tninfo );
        BB_Insert_Op_After(bb, op, new_op);
if (EBO_Trace_Optimization) fprintf(TFile,"combine immediate adds\n");
        return TRUE;
      }

      return FALSE;
    }

    if (TN_is_symbol(ptn1)) return FALSE;

    if (pred_val == const_val) {

     /* Look for identical operations that are redundant. */
      if ((OP_iand(op) && OP_iand(pred_op) && (const_val != ~0)) ||
          (OP_ior(op) && OP_ior(pred_op) && (const_val != 0))) {
        OPS ops = OPS_EMPTY;
if (EBO_Trace_Optimization) fprintf(TFile,"replace redundant and/or\n");
        EBO_Exp_COPY((OP_has_predicate(op)?OP_opnd(op,OP_PREDICATE_OPND):NULL),
                     tnr, OP_result(pred_op, 0), &ops);
        if (EBO_in_loop) EBO_OPS_omega (&ops, OP_has_predicate(op)?opnd_tninfo[OP_PREDICATE_OPND]:NULL);
        OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
        BB_Insert_Ops(bb, op, &ops, FALSE);
        return TRUE;
      }
    
    } /* end : if (pred_val == const_val) */

  } else if (((const_val >> EBO_bit_length(pred_op)) == 0) &&
             ((pred_opcode == TOP_sxt1) || (pred_opcode == TOP_sxt2) || (pred_opcode == TOP_sxt4) || 
              (pred_opcode == TOP_zxt1) || (pred_opcode == TOP_zxt2) || (pred_opcode == TOP_zxt4)) &&
             ((opcode == TOP_cmp_eq)    || (opcode == TOP_cmp_eq_unc) ||
              (opcode == TOP_cmp_i_eq)  || (opcode == TOP_cmp_i_eq_unc) ||
              (opcode == TOP_cmp_ne)    || (opcode == TOP_cmp_ne_unc) ||
              (opcode == TOP_cmp_i_ne)  || (opcode == TOP_cmp_i_ne_unc) ||
              (opcode == TOP_cmp4_eq)   || (opcode == TOP_cmp4_eq_unc) ||
              (opcode == TOP_cmp4_i_eq) || (opcode == TOP_cmp4_i_eq_unc) ||
              (opcode == TOP_cmp4_ne)   || (opcode == TOP_cmp4_ne_unc) ||
              (opcode == TOP_cmp4_i_ne) || (opcode == TOP_cmp4_i_ne_unc))) {
   /* Compares of 0 against a sign extended value may not need the sign-extension. */
    INT ptn0_idx = 1;  /* TOP_Find_Operand_Use(pred_opcode,OU_opnd1) does not work. */
    TN *ptn0 = OP_opnd(pred_op, ptn0_idx);
    EBO_TN_INFO *ptn0_tninfo;

    OP *new_op;
    TOP new_opcode;
    INT64 new_const_val;

    ptn0_tninfo = pred_opinfo->actual_opnd[ptn0_idx];

   /* Are the operands available for use? */
    if ((ptn0_tninfo == NULL) || 
        (ptn0_tninfo->in_op == NULL) ||
        !EBO_tn_available (bb, ptn0_tninfo)) return FALSE;

   /* Find the OP associated with the input to pred_op. */
    OP *pred_pred_op = ptn0_tninfo->in_op;

   /* Is the input to the pred_op a load? */
    if (!OP_load(pred_pred_op)) return FALSE;

   /* Are the lengths the same? */
    if (EBO_bit_length(pred_op) != EBO_bit_length(pred_pred_op)) return FALSE;

   /* Skip the sign extension instruction and just use the load. */
    new_op = Dup_OP (op);
    Set_OP_opnd (new_op, l0_opnd2_idx, ptn0);
    OP_srcpos(new_op) = OP_srcpos(op);
    if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND], NULL, ptn0_tninfo );
    BB_Insert_Op_After(bb, op, new_op);
if (EBO_Trace_Optimization) fprintf(TFile,"sign-extension in load-extend-cmp sequence is not needed\n");
    return TRUE;
  } /* end: Sequence optimizations. */

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
  INT opndnum = OP_opnds(op);
  EBO_TN_INFO *tninfo;

  TOP opcode = OP_code(op);
  TN *tnr = OP_result(op,0);
  INT l0_opnd1_idx;
  INT l0_opnd2_idx;
  TN *tn0;
  TN *tn1;
  INT64 const_val;

  OP *pred_op;
  TOP pred_opcode;
  EBO_OP_INFO *pred_opinfo;

  if (EBO_Trace_Execution) {
    #pragma mips_frequency_hint NEVER
    INT i;
    fprintf(TFile, "%sin BB:%d special1 OP :- %s ",
            EBO_trace_pfx, BB_id(bb),TOP_Name(OP_code(op)));
    for (i = 0; i < opndnum; i++) {
      fprintf(TFile," ");
      Print_TN(opnd_tn[i],FALSE);
    }
    fprintf(TFile,"\n");
  }

  if (!OP_has_predicate(op)) return FALSE;
  if (OP_opnds(op) < 2) return FALSE;

  if (TN_register_class(tnr) == ISA_REGISTER_CLASS_float) {
    if (OP_opnds(op) < 4) return FALSE;
    l0_opnd1_idx = 2;
    l0_opnd2_idx = 3;
    tn0 = opnd_tn[l0_opnd1_idx];
    tn1 = opnd_tn[l0_opnd2_idx];

    if (tn1 == FZero_TN) {
      if ((IEEE_Arithmetic >= IEEE_INEXACT) &&
          ((opcode == TOP_fmpy) ||
           (opcode == TOP_fmpy_s) ||
           (opcode == TOP_fmpy_d))) {
        OPS ops = OPS_EMPTY;
if (EBO_Trace_Optimization) fprintf(TFile,"replace fmpy of 0.0 with 0.0\n");
        EBO_Exp_COPY((OP_has_predicate(op)?OP_opnd(op,OP_PREDICATE_OPND):NULL),
                     tnr, FZero_TN, &ops);
        if (EBO_in_loop) EBO_Set_OP_omega (OPS_first(&ops),
                                           OP_has_predicate(op)?opnd_tninfo[OP_PREDICATE_OPND]:NULL,
                                           NULL);
        OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
        BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
        return TRUE;
      }
      if ((opcode == TOP_fadd) ||
          (opcode == TOP_fadd_s) ||
          (opcode == TOP_fadd_d) ||
          (opcode == TOP_fsub) ||
          (opcode == TOP_fsub_s) ||
          (opcode == TOP_fsub_d)) {
        OPS ops = OPS_EMPTY;
if (EBO_Trace_Optimization) fprintf(TFile,"replace fadd of 0.0 with tn0\n");
        EBO_Exp_COPY((OP_has_predicate(op)?OP_opnd(op,OP_PREDICATE_OPND):NULL),
                     tnr, tn0, &ops);
        if (EBO_in_loop) EBO_Set_OP_omega (OPS_first(&ops),
                                           OP_has_predicate(op)?opnd_tninfo[OP_PREDICATE_OPND]:NULL,
                                           opnd_tninfo[l0_opnd1_idx]);
        OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
        BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
        return TRUE;
      }
    }
    if (tn1 == FOne_TN) {
      if ((opcode == TOP_fmpy) ||
          (opcode == TOP_fmpy_s) ||
          (opcode == TOP_fmpy_d)) {
        OPS ops = OPS_EMPTY;
if (EBO_Trace_Optimization) fprintf(TFile,"replace fmpy of 1.0 with tn0\n");
        EBO_Exp_COPY((OP_has_predicate(op)?OP_opnd(op,OP_PREDICATE_OPND):NULL),
                     tnr, tn0, &ops);
        if (EBO_in_loop) EBO_Set_OP_omega (OPS_first(&ops),
                                           OP_has_predicate(op)?opnd_tninfo[OP_PREDICATE_OPND]:NULL,
                                           opnd_tninfo[l0_opnd1_idx]);
        OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
        BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
        return TRUE;
      }
    }
  }
  if ((TN_register_class(tnr) != ISA_REGISTER_CLASS_integer) &&
      (TN_register_class(tnr) != ISA_REGISTER_CLASS_predicate)) {
   /* Can only do arithmatic on integers. */
    return FALSE;
  }

  l0_opnd1_idx = 1;
  l0_opnd2_idx = 2;
  tn0 = opnd_tn[l0_opnd1_idx];
  tn1 = opnd_tn[l0_opnd2_idx];
  const_val = TN_Value(tn1);

  if (TN_is_symbol(tn1)) {
   /* Re-located constants not supported. */
    return FALSE;
  }

  if (TN_Is_Constant(tn0)) {
   /* Shouldn't be in this routine because there are no
      optimizations that are different from actually
      evaluating the expression. */
    return FALSE;
  }

  if (((TN_register_class(tn0) != ISA_REGISTER_CLASS_integer) &&
      (TN_register_class(tn0) != ISA_REGISTER_CLASS_predicate)) ||
      (TN_is_fpu_int(tnr) != TN_is_fpu_int(tn0))) {
   /* Type changes indicate that something tricky is going on. */
    if (EBO_Trace_Data_Flow) {
      fprintf(TFile,"%sType mismatch between result and operand\n",EBO_trace_pfx);
    }
    return FALSE;
  }

  if (const_val == 0) {

    if ((opcode == TOP_shl)    ||
        (opcode == TOP_shl_i)  ||
        (opcode == TOP_shr)    ||
        (opcode == TOP_shr_u)  ||
        (opcode == TOP_shr_i)  ||
        (opcode == TOP_shr_i_u)||
        (opcode == TOP_shrp)   ||
        (opcode == TOP_sub)    ||
        (opcode == TOP_sub_i)) {
      OPS ops = OPS_EMPTY;
if (EBO_Trace_Optimization) fprintf(TFile,"replace shift/subtract of zero\n");
      EBO_Exp_COPY((OP_has_predicate(op)?OP_opnd(op,OP_PREDICATE_OPND):NULL),
                   tnr, tn0, &ops);
      if (EBO_in_loop) EBO_Set_OP_omega (OPS_first(&ops),
                                         OP_has_predicate(op)?opnd_tninfo[OP_PREDICATE_OPND]:NULL,
                                         opnd_tninfo[l0_opnd1_idx]);
      OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
      BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
      return TRUE;
    }

    if ((opcode == TOP_and)    ||
        (opcode == TOP_and_i)) {
      OPS ops = OPS_EMPTY;
if (EBO_Trace_Optimization) fprintf(TFile,"replace and with zero\n");
      EBO_Exp_COPY((OP_has_predicate(op)?OP_opnd(op,OP_PREDICATE_OPND):NULL),
                   tnr, Zero_TN, &ops);
      if (EBO_in_loop) EBO_OPS_omega (&ops, OP_has_predicate(op)?opnd_tninfo[OP_PREDICATE_OPND]:NULL);
      OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
      BB_Insert_Ops(bb, op, &ops, FALSE);
      return TRUE;
    }

    if (opcode == TOP_dep) {
      OP *new_op;
if (EBO_Trace_Optimization) fprintf(TFile,"replace dep with dep_z\n");
      new_op = Mk_OP(TOP_dep_z, tnr, OP_opnd(op,OP_PREDICATE_OPND),
                     tn0, OP_opnd(op,3), OP_opnd(op,4));
      OP_srcpos(new_op) = OP_srcpos(op);
      if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND],
                                          opnd_tninfo[l0_opnd2_idx], NULL, NULL);
      BB_Insert_Op_After(OP_bb(op), op, new_op);
      return TRUE;
    }

  }

  if (const_val > 63) {

    if ((opcode == TOP_shl)    ||
        (opcode == TOP_shl_i)  ||
        (opcode == TOP_shr_u)  ||
        (opcode == TOP_shr_i_u)) {
      OPS ops = OPS_EMPTY;
if (EBO_Trace_Optimization) fprintf(TFile,"replace shift with zero\n");
      EBO_Exp_COPY((OP_has_predicate(op)?OP_opnd(op,OP_PREDICATE_OPND):NULL),
                   tnr, Zero_TN, &ops);
      if (EBO_in_loop) EBO_Set_OP_omega (OPS_first(&ops),
                                         OP_has_predicate(op)?opnd_tninfo[OP_PREDICATE_OPND]:NULL,
                                         NULL);
      OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
      BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
      return TRUE;
    }

  }

  if ((opcode == TOP_add) &&
      (ISA_LC_Value_In_Class ( const_val, LC_i22))) {
    OP *new_op;
    TOP new_opcode = (ISA_LC_Value_In_Class (const_val, LC_i14)) ? TOP_adds : TOP_addl;
    if (new_opcode == TOP_addl) return FALSE; /* Skip transformations to addl. */

    new_op = Mk_OP(new_opcode, tnr, OP_opnd(op,OP_PREDICATE_OPND),
                   Gen_Literal_TN(const_val, TN_size(tn0)), tn0);
    OP_srcpos(new_op) = OP_srcpos(op);
    if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND], NULL, opnd_tninfo[l0_opnd1_idx]);
    BB_Insert_Op_After(OP_bb(op), op, new_op);
if (EBO_Trace_Optimization) fprintf(TFile,"Reverse and replace add with adds\n");
    return TRUE;
  }

  if ((opcode == TOP_sub) &&
      (ISA_LC_Value_In_Class (-const_val, LC_i22))) {
    OP *new_op;
    TOP new_opcode = (ISA_LC_Value_In_Class ( -const_val, LC_i14)) ? TOP_adds : TOP_addl;
    if (new_opcode == TOP_addl) return FALSE; /* Skip transformations to addl. */

    new_op = Mk_OP(new_opcode, tnr, OP_opnd(op,OP_PREDICATE_OPND),
                   Gen_Literal_TN(-const_val, TN_size(tn0)), tn0);
    OP_srcpos(new_op) = OP_srcpos(op);
    if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND], NULL, opnd_tninfo[l0_opnd1_idx]);
    BB_Insert_Op_After(OP_bb(op), op, new_op);
if (EBO_Trace_Optimization) fprintf(TFile,"replace sub with add_i\n");
    return TRUE;
  }

  if (opcode == TOP_and) {
    if (ISA_LC_Value_In_Class (const_val, LC_i8)) {
      OP *new_op;
if (EBO_Trace_Optimization) fprintf(TFile,"replace and with and_i\n");
      new_op = Mk_OP(TOP_and_i, tnr, OP_opnd(op,OP_PREDICATE_OPND),
                     Gen_Literal_TN(const_val, TN_size(tn0)), tn0);
      OP_srcpos(new_op) = OP_srcpos(op);
      if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND], NULL, opnd_tninfo[l0_opnd1_idx]);
      BB_Insert_Op_After(OP_bb(op), op, new_op);
      return TRUE;
    } else {
   /* Look for a mask in the constant and replace the and(load-immediate,tn) with an extr. */
      OPS ops = OPS_EMPTY;
      if (Expand_Special_And_Immed (tnr, tn0, const_val, &ops)) {
if (EBO_Trace_Optimization) fprintf(TFile,"replace and with bit move\n");
        Set_OP_opnd(OPS_first(&ops), OP_PREDICATE_OPND, OP_opnd(op,OP_PREDICATE_OPND));
        if (EBO_in_loop) EBO_Set_OP_omega (OPS_first(&ops), opnd_tninfo[OP_PREDICATE_OPND],
                                           NULL, opnd_tninfo[l0_opnd1_idx]);
        OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
        BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
        return TRUE;
      }
    }
  }

  if (opcode == TOP_andcm) {
    if (ISA_LC_Value_In_Class (~const_val, LC_i8)) {
      OP *new_op;
if (EBO_Trace_Optimization) fprintf(TFile,"replace and with complement and_i\n");
      new_op = Mk_OP(TOP_and_i, tnr, OP_opnd(op,OP_PREDICATE_OPND),
                     Gen_Literal_TN(~const_val, TN_size(tn0)), tn0);
      OP_srcpos(new_op) = OP_srcpos(op);
      if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND], NULL, opnd_tninfo[l0_opnd1_idx]);
      BB_Insert_Op_After(OP_bb(op), op, new_op);
      return TRUE;
    } else if (!EBO_in_loop) {
     /* Look for a mask in the constant and replace the andcm(tn,load-immediate) with an extr. */
     /* Note: We don't know exactly what will be generated by Expand_Special_And_Immed, so we  */
     /* don't know how big to make the new omega entry.                                        */
      OPS ops = OPS_EMPTY;
      if (Expand_Special_And_Immed (tnr, tn0, ~const_val, &ops)) {
if (EBO_Trace_Optimization) fprintf(TFile,"replace andcm with bit move\n");
        Set_OP_opnd(OPS_first(&ops), OP_PREDICATE_OPND, OP_opnd(op,OP_PREDICATE_OPND));
        OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
        BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
        return TRUE;
      }
    }
  }

  if ((opcode == TOP_or) &&
      (ISA_LC_Value_In_Class (const_val, LC_i8))) {
    OP *new_op;
if (EBO_Trace_Optimization) fprintf(TFile,"replace or with or_i\n");
    new_op = Mk_OP(TOP_or_i, tnr, OP_opnd(op,OP_PREDICATE_OPND),
                   Gen_Literal_TN(const_val, TN_size(tn0)), tn0);
    OP_srcpos(new_op) = OP_srcpos(op);
    if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND], NULL, opnd_tninfo[l0_opnd1_idx]);
    BB_Insert_Op_After(OP_bb(op), op, new_op);
    return TRUE;
  }

  if ((opcode == TOP_xor) &&
      (ISA_LC_Value_In_Class (const_val, LC_i8))) {
    OP *new_op;
if (EBO_Trace_Optimization) fprintf(TFile,"replace xor with xor_i\n");
    new_op = Mk_OP(TOP_xor_i, tnr, OP_opnd(op,OP_PREDICATE_OPND),
                   Gen_Literal_TN(const_val, TN_size(tn0)), tn0);
    OP_srcpos(new_op) = OP_srcpos(op);
    if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND], NULL, opnd_tninfo[l0_opnd1_idx]);
    BB_Insert_Op_After(OP_bb(op), op, new_op);
    return TRUE;
  }

  if ((TOP_shl == opcode) && (const_val <= 63)) {
if (EBO_Trace_Optimization) fprintf(TFile,"Convert shl to shl_i\n");
    OP *new_op;
    new_op = Mk_OP(TOP_shl_i, tnr, OP_opnd(op,OP_PREDICATE_OPND), tn0,
                   Gen_Literal_TN(const_val, TN_size(tn0)));
    OP_srcpos(new_op) = OP_srcpos(op);
    if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND], opnd_tninfo[l0_opnd1_idx], NULL);
    BB_Insert_Op_After(bb, op, new_op);
    return TRUE;
  }

  if ((TOP_shr == opcode) && (const_val <= 63)) {
if (EBO_Trace_Optimization) fprintf(TFile,"Convert shr to shr_i\n");
    OP *new_op;
    new_op = Mk_OP(TOP_shr_i, tnr, OP_opnd(op,OP_PREDICATE_OPND), tn0,
                   Gen_Literal_TN(const_val, TN_size(tn0)));
    OP_srcpos(new_op) = OP_srcpos(op);
    if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND], opnd_tninfo[l0_opnd1_idx], NULL);
    BB_Insert_Op_After(bb, op, new_op);
    return TRUE;
  }

  if ((TOP_shr_u == opcode) && (const_val <= 63)) {
if (EBO_Trace_Optimization) fprintf(TFile,"Convert shr_u to shr_i_u\n");
    OP *new_op;
    new_op = Mk_OP(TOP_shr_i_u, tnr, OP_opnd(op,OP_PREDICATE_OPND), tn0,
                   Gen_Literal_TN(const_val, TN_size(tn0)));
    OP_srcpos(new_op) = OP_srcpos(op);
    if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND], opnd_tninfo[l0_opnd1_idx], NULL);
    BB_Insert_Op_After(bb, op, new_op);
    return TRUE;
  }

  if (OP_icmp(op)) {
    INT64 new_const_val = const_val;
    TOP new_opcode = Immediate_Compare(Reverse_Compare(opcode));
    if (new_opcode == TOP_UNDEFINED) return FALSE;
    if (TOP_Can_Have_Immediate(new_const_val, new_opcode)) {
     /* Transform the compare to use the immediate form. */
      OPS ops = OPS_EMPTY;
      TN *r1 = OP_result(op,0);
      TN *r2 = OP_result(op,1);
      Build_OP (new_opcode, r1, r2, OP_opnd(op,OP_PREDICATE_OPND),
                Gen_Literal_TN(new_const_val, TN_size(tn0)), tn0, &ops);
      if (EBO_in_loop) EBO_Set_OP_omega (OPS_first(&ops), opnd_tninfo[OP_PREDICATE_OPND],
                                         NULL, opnd_tninfo[l0_opnd1_idx]);
      OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
      BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
if (EBO_Trace_Optimization) fprintf(TFile,"reverse operands and replace cmp with cmp_i\n");
      return TRUE;
    }
  }

 /* Sequence optimizations. */

  tninfo = opnd_tninfo[l0_opnd1_idx];
  if (tninfo == NULL) return FALSE;
  pred_op = tninfo->in_op;
  if (pred_op == NULL) return FALSE;
  pred_opcode = OP_code(pred_op);
  pred_opinfo = locate_opinfo_entry (tninfo);
  if (pred_opinfo == NULL) return FALSE;

 /* First, look for some special cases involving loads. */
  if (OP_load(pred_op)) {
    if (OP_iand(op)) {
      if (((pred_opcode == TOP_ld1) &&
           (const_val == (~((~0) << 8)))) ||
          ((pred_opcode == TOP_ld2) &&
           (const_val ==  (~((~0) << 16))))) {
       /* No need to mask - it was done with the load. */
        OPS ops = OPS_EMPTY;
if (EBO_Trace_Optimization) fprintf(TFile,"no need to mask after load\n");
        EBO_Exp_COPY((OP_has_predicate(op)?OP_opnd(op,OP_PREDICATE_OPND):NULL),
                     tnr, tn0, &ops);
        if (EBO_in_loop) EBO_Set_OP_omega (OPS_first(&ops),
                                           OP_has_predicate(op)?opnd_tninfo[OP_PREDICATE_OPND]:NULL,
                                           opnd_tninfo[l0_opnd1_idx]);
        OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
        BB_Insert_Ops(bb, op, &ops, FALSE);
        return TRUE;
      }
    }
    return FALSE;
  }

 /* Now, look into optimizations that depend on specific
    values for the actual operands of the predecessor op. */
  if (TOP_Find_Operand_Use(OP_code(pred_op),OU_opnd2) >= 0) {
    TOP pred_opcode = OP_code(pred_op);
    INT ptn0_idx = TOP_Find_Operand_Use(pred_opcode,OU_opnd1);
    INT ptn1_idx = TOP_Find_Operand_Use(pred_opcode,OU_opnd2);
    TN *ptn0 = OP_opnd(pred_op, ptn0_idx);
    TN *ptn1 = OP_opnd(pred_op, ptn1_idx);
    EBO_TN_INFO *ptn0_tninfo;
    EBO_TN_INFO *ptn1_tninfo;
    INT64 pred_val = 0;

    OP *new_op;
    TOP new_opcode;
    UINT64 new_const_val;

    ptn0_tninfo = pred_opinfo->actual_opnd[ptn0_idx];
    ptn1_tninfo = pred_opinfo->actual_opnd[ptn1_idx];

   /* Are the operands available for use? */
    if (!(TN_Is_Constant(ptn0) || EBO_tn_available (bb, ptn0_tninfo)) ||
        !(TN_Is_Constant(ptn1) || EBO_tn_available (bb, ptn1_tninfo))) return FALSE;
    if (pred_opcode == TOP_spadjust) return FALSE;

    if (TN_Is_Constant(ptn1) && !TN_is_symbol(ptn1)) {
    } else if (TN_Is_Constant(ptn0) && !TN_is_symbol(ptn0) &&
               (OP_iadd(pred_op) || OP_ior(pred_op)  || OP_iand(pred_op))) {
      TN *s = ptn0;
      ptn0 = ptn1;
      ptn0_tninfo = ptn1_tninfo;
      ptn1 = s;
    } else return FALSE;

   /* Is the non-constant operand available for use? */
    if (!EBO_tn_available (bb, ptn0_tninfo)) {
      return FALSE;
    }

    pred_val = TN_Value(ptn1);

   /* Look for identical operations where the constants can be combined. */
    if ((opcode == pred_opcode) &&
        ((opcode == TOP_shl)    ||
         (opcode == TOP_shl_i)  ||
         (opcode == TOP_shr_u)  ||
         (opcode == TOP_shr_i_u))) {
      new_const_val = pred_val + const_val;

      if (new_const_val <= 63) {

        switch (opcode) {
        case TOP_shl:
        case TOP_shl_i: new_opcode = TOP_shl_i; break;
        case TOP_shr_u:
        case TOP_shr_i_u: new_opcode = TOP_shr_i_u; break;
        default: return FALSE;
        }

        new_op = Mk_OP (new_opcode, tnr, OP_opnd(op,OP_PREDICATE_OPND),
                        ptn0, Gen_Literal_TN(new_const_val, TN_size(tn0)));
        OP_srcpos(new_op) = OP_srcpos(op);
        if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND], ptn0_tninfo, NULL);
        BB_Insert_Op_After(bb, op, new_op);
if (EBO_Trace_Optimization) fprintf(TFile,"combine identical shifts\n");
        return TRUE;
      }
    }

    if (pred_val == const_val) {

     /* Look for identical operations that are redundant. */
      if ((OP_iand(op) && OP_iand(pred_op) && (const_val != ~0)) ||
          (OP_ior(op) && OP_ior(pred_op) && (const_val != 0))) {
        OPS ops = OPS_EMPTY;
if (EBO_Trace_Optimization) fprintf(TFile,"replace redundant and/or\n");
        EBO_Exp_COPY((OP_has_predicate(op)?OP_opnd(op,OP_PREDICATE_OPND):NULL),
                     tnr, OP_result(pred_op, 0), &ops);
        if (EBO_in_loop) EBO_OPS_omega (&ops, OP_has_predicate(op)?opnd_tninfo[OP_PREDICATE_OPND]:NULL);
        OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
        BB_Insert_Ops(bb, op, &ops, FALSE);
        return TRUE;
      }

     /* Look for complementary operations. */
      if ((OP_iadd(op) && OP_isub(pred_op)) ||
          (OP_isub(op) && OP_iadd(pred_op))) {
       /* Copy the original input. */
        OPS ops = OPS_EMPTY;
if (EBO_Trace_Optimization) fprintf(TFile,"replace complementary operations\n");
        EBO_Exp_COPY((OP_has_predicate(op)?OP_opnd(op,OP_PREDICATE_OPND):NULL),
                     tnr, ptn0, &ops);
        if (EBO_in_loop) EBO_Set_OP_omega (OPS_first(&ops), opnd_tninfo[OP_PREDICATE_OPND], ptn0_tninfo);
        OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
        BB_Insert_Ops(bb, op, &ops, FALSE);
        return TRUE;
      }
    
    } /* end : if (pred_val == const_val) */

  } /* end: Sequence optimizations. */
  
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
  BB *fall_bb;
  BB *branch_bb;

  if (BBlist_Len(BB_succs(bb)) != 2) return FALSE;

  fall_bb = BB_next(bb);
  branch_bb = BBLIST_item(BB_succs(bb));
  if (branch_bb == fall_bb) {
      branch_bb = BBLIST_item(BBLIST_next(BB_succs(bb)));
  }

  if ((OP_code(op) == TOP_br_cond) &&
      (TOP_Find_Operand_Use(OP_code(op),OU_target) >= 0) &&
       OP_has_predicate(op) &&
      ((OP_opnd (op, OP_PREDICATE_OPND) == True_TN) ||
       TN_Is_Constant(opnd_tn[OP_PREDICATE_OPND]))) {
      //A very weak fix added here,in order to avoid loop nodes
      //been deleted from node;
     if(IPFEC_Enable_Region_Formation && RGN_Formed) {
        if((Home_Region(bb)->Region_Type() == LOOP) && 
           ((Home_Region(branch_bb) != Home_Region(bb))||
           (Home_Region(fall_bb) != Home_Region(bb)))) {
               DevWarn("Give up resolve conditional branch in order to maintain region for BB %d.",BB_id(bb));
         
               return FALSE;
        }
    } 
    
    if (EBO_Trace_Optimization) {
      #pragma mips_frequency_hint NEVER
      INT i;
      INT opndnum = OP_opnds(op);
      fprintf(TFile, "%sin BB:%d Resolve conditional BR :- %s ",
              EBO_trace_pfx, BB_id(bb),TOP_Name(OP_code(op)));
      for (i = 0; i < opndnum; i++) {
        fprintf(TFile," ");
        Print_TN(opnd_tn[i],FALSE);
      }
      fprintf(TFile,"\n");
    }

      
    if (opnd_tn[OP_PREDICATE_OPND] == True_TN) {
     /* Branch IS taken - replace the conditional branch with a simple branch. */
      OPS ops = OPS_EMPTY;

      Build_OP (TOP_br, Gen_Enum_TN(ECV_ph_few), Gen_Enum_TN(ECV_dh),
                        OP_opnd(op,TOP_Find_Operand_Use(OP_code(op),OU_target)), &ops);
      OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
      BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);

      if(RGN_Formed && IPFEC_Enable_Region_Formation){
        RGN_Unlink_Pred_Succ(bb,fall_bb);
      } else {
        Unlink_Pred_Succ (bb, fall_bb);
      }  

      Change_Succ_Prob (bb, branch_bb, 1.0);

      return TRUE;;
    } else if (opnd_tn[OP_PREDICATE_OPND] == Zero_TN) {
      /* Branch NOT taken - delete it and fall through. */
      if(RGN_Formed && IPFEC_Enable_Region_Formation){
        RGN_Unlink_Pred_Succ(bb,branch_bb);
      } else {
        Unlink_Pred_Succ (bb, branch_bb);
      }
      Change_Succ_Prob (bb, fall_bb, 1.0);

      return TRUE;
    }
  }
  
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
  TOP opcode = OP_code(op);

  TN *tnr = OP_result(op,0);
  TN *tn0 = NULL;
  TN *tn1 = NULL;
  UINT64 tn0_uval = 0;
  INT64 tn0_val = 0;
  UINT64 tn1_uval = 0;
  INT64 tn1_val = 0;
  INT64 result_val;
  ST *result_sym = NULL;
  INT32 result_relocs;

  if (EBO_Trace_Execution) {
    #pragma mips_frequency_hint NEVER
    INT i;
    INT opndnum = OP_opnds(op);
    fprintf(TFile, "%sin BB:%d Constant OP :- %s ",
            EBO_trace_pfx, BB_id(OP_bb(op)),TOP_Name(opcode));
    for (i = 0; i < opndnum; i++) {
      fprintf(TFile," ");
      Print_TN(opnd_tn[i],TRUE);
    }
    fprintf(TFile,"\n");
  }

  if (OP_has_predicate(op) &&
      (opnd_tn[OP_PREDICATE_OPND] == Zero_TN)) {

    if (TOP_Is_Unconditional_Compare(opcode)) {
     /* Unconditional compares define results even if the predicate is "FALSE". */
      OPS ops = OPS_EMPTY;
      if (OP_result(op,0) != True_TN) {
        Build_OP (TOP_cmp_eq_unc, True_TN, OP_result(op,0), True_TN, Zero_TN, Zero_TN, &ops);
      }
      if (OP_result(op,1) != True_TN) {
        Build_OP (TOP_cmp_eq_unc, True_TN, OP_result(op,1), True_TN, Zero_TN, Zero_TN, &ops);
      }

      if (OP_glue(op)) {
        OP *next_op = OPS_first(&ops);
        while (next_op != NULL) {
          Set_OP_glue(next_op);
          next_op = OP_next(next_op);
        }
      }

      if (EBO_Trace_Optimization) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile, "%sin BB:%d Result of compare is FALSE for both ", EBO_trace_pfx,BB_id(OP_bb(op)));
        Print_TN(OP_result(op,0),FALSE); fprintf(TFile," and ");
        Print_TN(OP_result(op,1),FALSE); fprintf(TFile, "\n");
      }

      if (OPS_length(&ops) != 0) {
        OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
        BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
      }
      return TRUE;
    }

   /* The OP can be deleted. */
    return (!OP_glue(op));
  }


  if ((TN_register_class(tnr) != ISA_REGISTER_CLASS_integer) &&
      (TN_register_class(tnr) != ISA_REGISTER_CLASS_predicate)) {
   /* Can only do arithmatic on integers. */
    return FALSE;
  }

  if ((opcode == TOP_sxt4) ||
      (opcode == TOP_sxt2) ||
      (opcode == TOP_sxt1)) {
    INT length = EBO_bit_length(op);
    tn0 = opnd_tn[1];
    if (TN_is_symbol(tn0)) {
     /* What can we do? */
      return FALSE;
    }
    tn0_val = TN_Value (tn0);
    result_val = (tn0_val << (64-length)) >> (64-length);
    goto Constant_Created;
  }

  if ((opcode == TOP_zxt4) ||
      (opcode == TOP_zxt2) ||
      (opcode == TOP_zxt1)) {
    INT length = EBO_bit_length(op);
    tn0 = opnd_tn[1];
    if (TN_is_symbol(tn0)) {
     /* What can we do? */
      return FALSE;
    }
    tn0_uval = TN_Value (tn0);
    result_val = (tn0_uval << (64-length)) >> (64-length);
    goto Constant_Created;
  }

  if (OP_opnds(op) <= 2) {
    return FALSE;
  } 

  tn0 = opnd_tn[1];
  tn1 = opnd_tn[2];

  if (TN_is_symbol(tn0)) {
    if (TN_is_symbol(tn1)) {
     /* In theory, we could handle this but we won't. */
      return FALSE;
    }
    tn0_uval = TN_offset(tn0);
    tn0_val = TN_offset(tn0);
    result_sym = TN_var(tn0);
    result_relocs = TN_relocs(tn0);
  } else if (TN_is_symbol(tn1)) {
    if (OP_isub(op)) {
     /* In theory, we might be able to handle this but we won't. */
      return FALSE;
    }
    tn1_uval = TN_offset(tn1);
    tn1_val = TN_offset(tn1);
    result_sym = TN_var(tn1);
    result_relocs = TN_relocs(tn1);
  } else {
    tn0_uval = TN_Value (tn0);
    tn0_val = TN_Value (tn0);
    tn1_uval = TN_Value (tn1);
    tn1_val = TN_Value (tn1);
  }

  if ((TOP_add == opcode)   || (TOP_adds == opcode)  ||
      (TOP_addl == opcode)  || (TOP_addp4 == opcode) ||
      (TOP_addp4_i == opcode)) {
    result_val = tn0_uval + tn1_uval;
    goto Constant_Created;
  }

  if (TOP_add_1 == opcode) {
    result_val = tn0_uval + tn1_uval + 1;
    goto Constant_Created;
  }

  if ((TOP_sub == opcode) || (TOP_sub_i == opcode)) {
    result_val = tn0_uval - tn1_uval;
    goto Constant_Created;
  }

  if (TOP_sub_1 == opcode) {
    result_val = tn0_uval - tn1_uval - 1;
    goto Constant_Created;
  }

  if (TN_is_symbol(tn0) || TN_is_symbol(tn1)) {
   /* Can only do simple add or subtract with relocation. */
    return FALSE;
  }

  if ((TOP_and == opcode) || (TOP_and_i == opcode)) {
    result_val = tn0_uval & tn1_uval;
    goto Constant_Created;
  }

  if ((TOP_andcm == opcode) || (TOP_andcm_i == opcode)) {
    result_val = tn0_uval & (~tn1_uval);
    goto Constant_Created;
  }

  if ((TOP_or == opcode) || (TOP_or_i == opcode)) {
    result_val = tn0_uval | tn1_uval;
    goto Constant_Created;
  }

  if ((TOP_xor == opcode) || (TOP_xor_i == opcode)) {
    result_val = tn0_uval ^ tn1_uval;
    goto Constant_Created;
  }

  if (TOP_shl == opcode) {
    if ((tn1_val < 0) || (tn1_val > 63)) {
      result_val = 0;
    } else {
      result_val = tn0_val << tn1_val;
    }
    goto Constant_Created;
  }

  if (TOP_shladd == opcode) {
    if ((tn1_val < 0) || (tn1_val > 63)) {
      result_val = TN_Value(opnd_tn[3]);
    } else {
      result_val = (tn0_val << tn1_val) + TN_Value(opnd_tn[3]);
    }
    goto Constant_Created;
  }

  if (TOP_shr_u == opcode) {
    if ((tn1_val < 0) || (tn1_val > 63)) {
      result_val = 0;
    } else {
      result_val = tn0_uval >> tn1_val;
    }
    goto Constant_Created;
  }

  if (TOP_shr == opcode) {
    if ((tn1_val < 0) || (tn1_val > 63)) tn1_val = 63;
    result_val = tn0_val >> tn1_val;
    goto Constant_Created;
  }

  if (opcode == TOP_extr) {
    INT start = tn1_val;
    INT length = TN_Value(opnd_tn[3]);
    if ((length+start) > 64) return FALSE;
    result_val = (tn0_val << (64-(length+start))) >> (64-length);
    goto Constant_Created;
  }

  if (opcode == TOP_extr_u) {
    INT start = tn1_val;
    INT length = TN_Value(opnd_tn[3]);
    if ((length+start) > 64) return FALSE;
    result_val = (tn0_uval << (64-(length+start))) >> (64-length);
    goto Constant_Created;
  }

  if ((opcode == TOP_dep) || (opcode == TOP_dep_i)) {
    INT start = TN_Value(opnd_tn[3]);
    INT length = TN_Value(opnd_tn[4]);
    UINT64 mask = (~0);
    mask = mask << length;
    result_val = (tn1_uval & ~(~mask << start)) | ((tn0_uval & ~mask) << start);
    goto Constant_Created;
  }

  if ((opcode == TOP_dep_z) || (opcode == TOP_dep_i_z)) {
    INT start = tn1_val;
    INT length = TN_Value(opnd_tn[3]);
    UINT64 mask = (~0);
    mask = mask << length;
    result_val = ((tn0_uval & ~mask) << start);
    goto Constant_Created;
  }

  if (OP_select(op)) {
    return Constant_Operand0 (op, opnd_tn, opnd_tninfo);
  }

  if (OP_icmp(op)) {
   /* Integer comparison operation. */
    if (OP_opnd(op, OP_PREDICATE_OPND) != True_TN) return FALSE;

    switch (opcode) {
    case TOP_cmp_eq:
    case TOP_cmp_i_eq:
    case TOP_cmp_eq_unc:
    case TOP_cmp_i_eq_unc:
      result_val = (tn0_val == tn1_val);
      break;
    case TOP_cmp4_eq:
    case TOP_cmp4_i_eq:
    case TOP_cmp4_eq_unc:
    case TOP_cmp4_i_eq_unc:
      result_val = (TRUNC_32(tn0_val) == TRUNC_32(tn1_val));
      break;
    case TOP_cmp_ne:
    case TOP_cmp_i_ne:
    case TOP_cmp_ne_unc:
    case TOP_cmp_i_ne_unc:
      result_val = (tn0_val != tn1_val);
      break;
    case TOP_cmp4_ne:
    case TOP_cmp4_i_ne:
    case TOP_cmp4_ne_unc:
    case TOP_cmp4_i_ne_unc:
      result_val = (TRUNC_32(tn0_val) != TRUNC_32(tn1_val));
      break;
    case TOP_cmp_lt:
    case TOP_cmp_i_lt:
    case TOP_cmp_lt_unc:
    case TOP_cmp_i_lt_unc:
      result_val = (tn0_val < tn1_val);
      break;
    case TOP_cmp4_lt:
    case TOP_cmp4_i_lt:
    case TOP_cmp4_lt_unc:
    case TOP_cmp4_i_lt_unc:
      result_val = (SEXT_32(tn0_val) < SEXT_32(tn1_val));
      break;
    case TOP_cmp_gt:
    case TOP_cmp_i_gt:
    case TOP_cmp_gt_unc:
    case TOP_cmp_i_gt_unc:
      result_val = (tn0_val > tn1_val);
      break;
    case TOP_cmp4_gt:
    case TOP_cmp4_i_gt:
    case TOP_cmp4_gt_unc:
    case TOP_cmp4_i_gt_unc:
      result_val = (SEXT_32(tn0_val) > SEXT_32(tn1_val));
      break;
    case TOP_cmp_le:
    case TOP_cmp_i_le:
    case TOP_cmp_le_unc:
    case TOP_cmp_i_le_unc:
      result_val = (tn0_val <= tn1_val);
      break;
    case TOP_cmp4_le:
    case TOP_cmp4_i_le:
    case TOP_cmp4_le_unc:
    case TOP_cmp4_i_le_unc:
      result_val = (SEXT_32(tn0_val) <= SEXT_32(tn1_val));
      break;
    case TOP_cmp_ge:
    case TOP_cmp_i_ge:
    case TOP_cmp_ge_unc:
    case TOP_cmp_i_ge_unc:
      result_val = (tn0_val >= tn1_val);
      break;
    case TOP_cmp4_ge:
    case TOP_cmp4_i_ge:
    case TOP_cmp4_ge_unc:
    case TOP_cmp4_i_ge_unc:
      result_val = (SEXT_32(tn0_val) >= SEXT_32(tn1_val));
      break;
    case TOP_cmp_ltu:
    case TOP_cmp_i_ltu:
    case TOP_cmp_ltu_unc:
    case TOP_cmp_i_ltu_unc:
      result_val = (tn0_uval < tn1_uval);
      break;
    case TOP_cmp4_ltu:
    case TOP_cmp4_i_ltu:
    case TOP_cmp4_ltu_unc:
    case TOP_cmp4_i_ltu_unc:
      result_val = (TRUNC_32(tn0_uval) < TRUNC_32(tn1_uval));
      break;
    case TOP_cmp_gtu:
    case TOP_cmp_i_gtu:
    case TOP_cmp_gtu_unc:
    case TOP_cmp_i_gtu_unc:
      result_val = (tn0_uval > tn1_uval);
      break;
    case TOP_cmp4_gtu:
    case TOP_cmp4_i_gtu:
    case TOP_cmp4_gtu_unc:
    case TOP_cmp4_i_gtu_unc:
      result_val = (TRUNC_32(tn0_uval) > TRUNC_32(tn1_uval));
      break;
    case TOP_cmp_leu:
    case TOP_cmp_i_leu:
    case TOP_cmp_leu_unc:
    case TOP_cmp_i_leu_unc:
      result_val = (tn0_uval <= tn1_uval);
      break;
    case TOP_cmp4_leu:
    case TOP_cmp4_i_leu:
    case TOP_cmp4_leu_unc:
    case TOP_cmp4_i_leu_unc:
      result_val = (TRUNC_32(tn0_uval) <= TRUNC_32(tn1_uval));
      break;
    case TOP_cmp_geu:
    case TOP_cmp_i_geu:
    case TOP_cmp_geu_unc:
    case TOP_cmp_i_geu_unc:
      result_val = (tn0_uval >= tn1_uval);
      break;
    case TOP_cmp4_geu:
    case TOP_cmp4_i_geu:
    case TOP_cmp4_geu_unc:
    case TOP_cmp4_i_geu_unc:
      result_val = (TRUNC_32(tn0_uval) >= TRUNC_32(tn1_uval));
      break;

   /* The following opcodes are more difficult to do since both results may be set to identical values. */
    case TOP_cmp4_eq_and:
    case TOP_cmp4_eq_or:
    case TOP_cmp4_eq_or_andcm:
    case TOP_cmp4_ne_and:
    case TOP_cmp4_ne_or:
    case TOP_cmp4_ne_or_andcm:
    case TOP_cmp4_z1_lt_and:
    case TOP_cmp4_z1_lt_or:
    case TOP_cmp4_z1_lt_or_andcm:
    case TOP_cmp4_z1_le_and:
    case TOP_cmp4_z1_le_or:
    case TOP_cmp4_z1_le_or_andcm:
    case TOP_cmp4_z1_gt_and:
    case TOP_cmp4_z1_gt_or:
    case TOP_cmp4_z1_gt_or_andcm:
    case TOP_cmp4_z1_ge_and:
    case TOP_cmp4_z1_ge_or:
    case TOP_cmp4_z1_ge_or_andcm:
    case TOP_cmp4_i_eq_and:
    case TOP_cmp4_i_eq_or:
    case TOP_cmp4_i_eq_or_andcm:
    case TOP_cmp4_i_ne_and:
    case TOP_cmp4_i_ne_or:
    case TOP_cmp4_i_ne_or_andcm:
    case TOP_cmp4_eq_orcm:
    case TOP_cmp4_eq_andcm:
    case TOP_cmp4_eq_and_orcm:
    case TOP_cmp4_ne_orcm:
    case TOP_cmp4_ne_andcm:
    case TOP_cmp4_ne_and_orcm:
    case TOP_cmp4_z1_lt_orcm:
    case TOP_cmp4_z1_lt_andcm:
    case TOP_cmp4_z1_lt_and_orcm:
    case TOP_cmp4_z1_le_orcm:
    case TOP_cmp4_z1_le_andcm:
    case TOP_cmp4_z1_le_and_orcm:
    case TOP_cmp4_z1_gt_orcm:
    case TOP_cmp4_z1_gt_andcm:
    case TOP_cmp4_z1_gt_and_orcm:
    case TOP_cmp4_z1_ge_orcm:
    case TOP_cmp4_z1_ge_andcm:
    case TOP_cmp4_z1_ge_and_orcm:
    case TOP_cmp4_z2_lt_orcm:
    case TOP_cmp4_z2_lt_andcm:
    case TOP_cmp4_z2_lt_and_orcm:
    case TOP_cmp4_z2_le_orcm:
    case TOP_cmp4_z2_le_andcm:
    case TOP_cmp4_z2_le_and_orcm:
    case TOP_cmp4_z2_gt_orcm:
    case TOP_cmp4_z2_gt_andcm:
    case TOP_cmp4_z2_gt_and_orcm:
    case TOP_cmp4_z2_ge_orcm:
    case TOP_cmp4_z2_ge_andcm:
    case TOP_cmp4_z2_ge_and_orcm:
    case TOP_cmp4_z2_lt_and:
    case TOP_cmp4_z2_lt_or:
    case TOP_cmp4_z2_lt_or_andcm:
    case TOP_cmp4_z2_le_and:
    case TOP_cmp4_z2_le_or:
    case TOP_cmp4_z2_le_or_andcm:
    case TOP_cmp4_z2_gt_and:
    case TOP_cmp4_z2_gt_or:
    case TOP_cmp4_z2_gt_or_andcm:
    case TOP_cmp4_z2_ge_and:
    case TOP_cmp4_z2_ge_or:
    case TOP_cmp4_z2_ge_or_andcm:
    case TOP_cmp4_i_eq_orcm:
    case TOP_cmp4_i_eq_andcm:
    case TOP_cmp4_i_eq_and_orcm:
    case TOP_cmp4_i_ne_orcm:
    case TOP_cmp4_i_ne_andcm:
    case TOP_cmp4_i_ne_and_orcm:
    case TOP_cmp4_lt_and:
    case TOP_cmp4_lt_or:
    case TOP_cmp4_lt_or_andcm:
    case TOP_cmp4_le_and:
    case TOP_cmp4_le_or:
    case TOP_cmp4_le_or_andcm:
    case TOP_cmp4_gt_and:
    case TOP_cmp4_gt_or:
    case TOP_cmp4_gt_or_andcm:
    case TOP_cmp4_ge_and:
    case TOP_cmp4_ge_or:
    case TOP_cmp4_ge_or_andcm:
    case TOP_cmp4_lt_orcm:
    case TOP_cmp4_lt_andcm:
    case TOP_cmp4_lt_and_orcm:
    case TOP_cmp4_le_orcm:
    case TOP_cmp4_le_andcm:
    case TOP_cmp4_le_and_orcm:
    case TOP_cmp4_gt_orcm:
    case TOP_cmp4_gt_andcm:
    case TOP_cmp4_gt_and_orcm:
    case TOP_cmp4_ge_orcm:
    case TOP_cmp4_ge_andcm:
    case TOP_cmp4_ge_and_orcm:
     /* Don't implement these opcodes until we actually see them generated. */

    default:
      return FALSE;
      break;
    }

    OPS ops = OPS_EMPTY;
    TN *result1 = (result_val == 0) ? OP_result(op,1) : OP_result(op,0);
    TN *result2 = (result_val == 0) ? OP_result(op,0) : OP_result(op,1);
    if (result1 != True_TN){
      Build_OP (TOP_Is_Unconditional_Compare(opcode) ? TOP_cmp_eq_unc : TOP_cmp_eq,
               result1, True_TN, OP_opnd(op, OP_PREDICATE_OPND), Zero_TN, Zero_TN, &ops);
      OP_srcpos(OPS_last(&ops)) = OP_srcpos(op);
    }
    if (result2 != True_TN){
      Build_OP (TOP_Is_Unconditional_Compare(opcode) ? TOP_cmp_eq_unc : TOP_cmp_eq,
               True_TN, result2, OP_opnd(op, OP_PREDICATE_OPND), Zero_TN, Zero_TN, &ops);
      OP_srcpos(OPS_last(&ops)) = OP_srcpos(op);
    }

    if (EBO_Trace_Optimization) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "%sin BB:%d Result of compare is %s for ",
              EBO_trace_pfx, BB_id(OP_bb(op)),(result_val == 0) ? "FALSE" : "TRUE");
      Print_TN(OP_result(op,0),FALSE); fprintf(TFile," complement ");
      Print_TN(OP_result(op,1),FALSE); fprintf(TFile, "\n");
    }

    if (EBO_in_loop) EBO_OPS_omega (&ops, opnd_tninfo[OP_PREDICATE_OPND]);
    BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
    return TRUE;
  }

  if ((opcode == TOP_tbit_z) ||
      (opcode == TOP_tbit_nz) ||
      (opcode == TOP_tbit_z_unc) ||
      (opcode == TOP_tbit_nz_unc)) {
    result_val = (tn0_uval >> tn1_val) & 1;
    result_val = ((opcode == TOP_tbit_z) || (opcode == TOP_tbit_z_unc)) ? !result_val : result_val;
    OPS ops = OPS_EMPTY;
    TN *result1 = (result_val == 0) ? OP_result(op,1) : OP_result(op,0);
    TN *result2 = (result_val == 0) ? OP_result(op,0) : OP_result(op,1);
    Build_OP (TOP_Is_Unconditional_Compare(opcode) ? TOP_cmp_eq_unc : TOP_cmp_eq,
              result1, True_TN, OP_opnd(op, OP_PREDICATE_OPND), Zero_TN, Zero_TN, &ops);
    OP_srcpos(OPS_last(&ops)) = OP_srcpos(op);
    Build_OP (TOP_Is_Unconditional_Compare(opcode) ? TOP_cmp_eq_unc : TOP_cmp_eq,
              True_TN, result2, OP_opnd(op, OP_PREDICATE_OPND), Zero_TN, Zero_TN, &ops);
    OP_srcpos(OPS_last(&ops)) = OP_srcpos(op);

    if (EBO_Trace_Optimization) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "%sin BB:%d Result of tbit is %s for ",
              EBO_trace_pfx, BB_id(OP_bb(op)),(result_val == 0) ? "FALSE" : "TRUE");
      Print_TN(OP_result(op,0),FALSE); fprintf(TFile," complement ");
      Print_TN(OP_result(op,1),FALSE); fprintf(TFile, "\n");
    }

    if (EBO_in_loop) EBO_OPS_omega (&ops, opnd_tninfo[OP_PREDICATE_OPND]);
    BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
    return TRUE;
  }

  return FALSE;

Constant_Created:

  OPS ops = OPS_EMPTY;
  TN *tnc;

  if (result_sym != NULL) {
    tnc = Gen_Symbol_TN(result_sym, result_val, result_relocs);
  } 
  else {
    switch (TN_size(tnr)) {
    	case 1:     
    		FmtAssert (!((tn0_uval & 0x1111111111111110LL == 0x0000000000000010LL) && 
			     (tn1_uval & 0x1111111111111110LL == 0x0000000000000010LL) && 
			     (result_val & 0x0000000000000100LL)),
			     ("1-byte literal 0x%016llx is out-of-range", result_val));
	  	result_val &= 0x00000000000000ffLL;
		break;
    	case 2:     
    		FmtAssert (!((tn0_uval & 0x1111111111111000LL == 0x0000000000001000LL) && 
			     (tn1_uval & 0x1111111111111000LL == 0x0000000000001000LL) && 
			     (result_val & 0x0000000000010000LL)),
			     ("2-byte literal 0x%016llx is out-of-range", result_val));
	  	result_val &= 0x000000000000ffffLL;
		break;
    	case 4:     
    		FmtAssert (!((tn0_uval & 0x1111111110000000LL == 0x0000000010000000LL) && 
			     (tn1_uval & 0x1111111110000000LL == 0x0000000010000000LL) && 
			     (result_val & 0x0000000100000000LL)),
			     ("4-byte literal 0x%016llx is out-of-range", result_val));
	  	result_val &= 0x00000000ffffffffLL;
		break;
    	case 8: 
		break;
    	default:
       		FmtAssert(false, ("Invalid literal tn size: %d", TN_size(tnr)));
    } 
  
    tnc = Gen_Literal_TN(result_val, TN_size(tnr));
  }
  Expand_Immediate (tnr, tnc, OP_result_is_signed(op,0), &ops);
  if (OP_next(OPS_first(&ops)) != NULL) {
   /* What's the point in replacing one instruction with several? */
    return FALSE;
  }
  if (OP_has_predicate(op)) {
    EBO_OPS_predicate (OP_opnd(op, OP_PREDICATE_OPND), &ops);
  }
  if (EBO_in_loop) EBO_OPS_omega (&ops, opnd_tninfo[OP_PREDICATE_OPND]);
  OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
  if (opcode == OP_code(OPS_first(&ops))) {
    BOOL all_operands_are_the_same = TRUE;
    INT i;
    for (i = 0; i < OP_opnds(op); i++) {
      if (!TNs_Are_Equivalent(OP_opnd(op, i), OP_opnd(OPS_first(&ops), i))) {
        all_operands_are_the_same = FALSE;
        break;
      }
    }
    if (all_operands_are_the_same) {
     /* Avoid infinite loops caused by regenerating the same instruction. */
      return FALSE;
    }
  }
  BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);

  if (EBO_Trace_Optimization) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "%sin BB:%d Redefine ",
            EBO_trace_pfx, BB_id(OP_bb(op)));
    Print_TN(tnr,TRUE);
    fprintf(TFile," with load of ");
    Print_TN(tnc,FALSE);
    fprintf(TFile, "\n");
  }

  return TRUE;
}


/*
 * CGTARG_Copy_Operand already catches most of the case we care about,
 * but there are some extra cases involving a 0 first operand and
 * we want to treat int->float and float->int register moves as copies.
 */
INT EBO_Copy_Operand (OP *op)
{
  TOP opcode;
  INT opnd;

  if (OP_copy(op)) {
    return OP_COPY_OPND;
  }

  opnd = CGTARG_Copy_Operand(op);
  if (opnd >= 0) {
    return opnd;
  }

  if (OP_iadd(op) || OP_ior(op) || OP_ixor(op)) {
    if (OP_code(op) == TOP_spadjust) {
      return -1;
    }
    if ((TN_is_register(OP_opnd(op,1)) &&
         TN_register_and_class(OP_opnd(op,1)) == CLASS_AND_REG_zero) ||
        ((TN_has_value(OP_opnd(op,1)) && TN_value(OP_opnd(op,1)) == 0))) {
      return 2;
    }
    if ((TN_is_register(OP_opnd(op,2)) &&
         TN_register_and_class(OP_opnd(op,2)) == CLASS_AND_REG_zero) ||
        ((TN_has_value(OP_opnd(op,2)) && TN_value(OP_opnd(op,2)) == 0))) {
      return 1;
    }
  }

  if (OP_iand(op)) {
    if ((TN_has_value(OP_opnd(op,1)) && TN_value(OP_opnd(op,1)) == ~0)) {
      return 2;
    }
    if ((TN_has_value(OP_opnd(op,2)) && TN_value(OP_opnd(op,2)) == ~0)) {
      return 1;
    }
  }

  opcode = OP_code(op);
  if ((opcode == TOP_mov)   ||
      (opcode == TOP_mov_i) ||
      (opcode == TOP_movl)  ||
      (opcode == TOP_mov_f)) {
    return 1;
  }
  if ((opcode == TOP_mov_t_br) ||
      (opcode == TOP_mov_f_br)) {
    return 1;
  }
  if ((opcode == TOP_mov_t_ar_r)    ||
      (opcode == TOP_mov_t_ar_i)    ||
      (opcode == TOP_mov_f_ar)      ||
      (opcode == TOP_mov_f_ar_i)    ||
      (opcode == TOP_mov_t_ar_r_i)  ||
      (opcode == TOP_mov_t_ar_i_i)  ||
      (opcode == TOP_mov_f_ar_m)    ||
      (opcode == TOP_mov_t_ar_r_m)  ||
      (opcode == TOP_mov_t_ar_i_m)) {
    return 1;
  }
  if ((opcode == TOP_setf_s) || (opcode == TOP_setf_d) ||
      (opcode == TOP_getf_s) || (opcode == TOP_getf_d)) {
    return 1;
  }

  if (((opcode == TOP_cmp_eq_unc) ||
       (opcode == TOP_cmp_eq && OP_opnd(op,OP_PREDICATE_OPND) == True_TN)) &&
      (OP_opnd(op,1) == Zero_TN) &&
      (OP_opnd(op,2) == Zero_TN)) {
    return 0; /* The predicate value is the result. */
  }

  return -1;
}



/*
 * Function: fadd_fmult
 *
 * Look for the pattern:
 *       FADD(float1,FMULT(float2,float3))
 * and turn it into:
 *       FMADD(float1,float2,float3)
 *
 * To find the pattern, we need to look at 3 levels of operands:
 *    1) floating add;
 *    2) an operand and a floating multiply;
 *    3) the input TN's to the multiply.
 *
 */
static
BOOL
fadd_fmult (OP *op, EBO_TN_INFO **opnd_tninfo)
{
 /* Level 1 data: */
  BB *bb = OP_bb(op);
  OP *l1_op0 = op;
  TN *l1_tn0;

 /* Level 2 data: */
  OP *l2_op0;
  TN *l2_tn0;
  EBO_TN_INFO *l2_tninfo0;
  OP *l2_op1;
  TN *l2_tn1;
  EBO_TN_INFO *l2_tninfo1;
  EBO_OP_INFO *l2_opinfo1;

 /* Level 3 data: */
  TN *l3_tn1;
  EBO_TN_INFO *l3_tninfo1;
  TN *l3_tn2;
  EBO_TN_INFO *l3_tninfo2;

  TOP new_opcode = TOP_UNDEFINED;
  OP *new_fmadd;

  if (!Madd_Allowed) return FALSE;
  if (!CG_create_madds) return FALSE;

  if (OP_fadd(l1_op0) || OP_fsub(l1_op0)) {
    TOP l1_opcode = OP_code(l1_op0);
    INT l2_opnd1_idx = TOP_Find_Operand_Use(l1_opcode,OU_opnd1);
    INT l2_opnd2_idx = TOP_Find_Operand_Use(l1_opcode,OU_opnd2);

    l1_tn0 = OP_result(l1_op0,0);
    l2_tn0 = OP_opnd(op,l2_opnd1_idx);
    l2_tn1 = OP_opnd(op,l2_opnd2_idx);

    l2_tninfo0 = opnd_tninfo[l2_opnd1_idx];
    l2_tninfo1 = opnd_tninfo[l2_opnd2_idx];
    if ((l2_tninfo0 == NULL) ||
        (l2_tninfo1 == NULL)) {
      return FALSE;
    }

   /* One of the input operands must be a fmult. */
    l2_op0 = l2_tninfo0->in_op;
    l2_op1 = l2_tninfo1->in_op;

   /* Is the second operand a multiply? */
   /* Set up the logic so that:
        l2_op1 points to the multiply that we are going to use, and
        l2_tn1 is the result TN of that multiply */

    if ((l2_op1 == NULL) || !OP_fmul(l2_op1)) {
     /* Is the first operand a multiply? */
      if ((l2_op0 == NULL) || !OP_fmul(l2_op0)) {
        return FALSE;
      }

     /* Reverse the way we look at the operands. */
      l2_tn0 = l2_tn1;
      l2_tn1 = OP_opnd(op,l2_opnd1_idx);
      l2_tninfo0 = l2_tninfo1;
      l2_tninfo1 = opnd_tninfo[l2_opnd1_idx];
      l2_op0 = l2_op1;
      l2_op1 = l2_tninfo1->in_op;
    }

   /* Determine the inputs to the fmult instructions. */
    l2_opinfo1 = locate_opinfo_entry (l2_tninfo1);
    if ((l2_opinfo1 == NULL) ||
        (l2_opinfo1->in_op == NULL)) {
      return FALSE;
    }

    TOP l2_opcode = OP_code(l2_op1);
    INT l3_opnd1_idx = TOP_Find_Operand_Use(l2_opcode,OU_opnd1);
    INT l3_opnd2_idx = TOP_Find_Operand_Use(l2_opcode,OU_opnd2);

    l3_tn1 = OP_opnd(l2_op1, l3_opnd1_idx);
    l3_tn2 = OP_opnd(l2_op1, l3_opnd2_idx);
    l3_tninfo1 = l2_opinfo1->actual_opnd[l3_opnd1_idx];
    l3_tninfo2 = l2_opinfo1->actual_opnd[l3_opnd2_idx];
    if ((l3_tn1 == NULL) || TN_Is_Constant(l3_tn1) ||
        (l3_tn2 == NULL) || TN_Is_Constant(l3_tn2) ||
        (l3_tninfo1 == NULL) || (l3_tninfo2 == NULL)) {
      return FALSE;
    }

   /* Would the inputs to the fmult be available for use? */
    if (!EBO_tn_available (bb, l3_tninfo1) ||
        !EBO_tn_available (bb, l3_tninfo2)) {
      return FALSE;
    }

   /* Do the result sizes match? */
    if (OP_result_size(op,0) != OP_result_size(l2_op1,0)) {
      return FALSE;
    }

   /* We have matched the pattern:
         l1_tn0 = FADD(l2_tn0,FMULT(l3_tn1,l3_tn2))
      turn it into:
         l1_tn0 = FMADD(l2_tn0,l3_tn1,l3_tn2) */

    if (OP_fadd(l1_op0)) {
     /* We actually started with: l1_tn0 = FADD(l2_tn0,FMULT(l3_tn1,l3_tn2)) */
     /*                  or with: l1_tn0 = FADD(FMULT(l3_tn1,l3_tn2),l2_tn0) */
     /* It really doesn't matter.                                            */

      switch (l2_opcode) {
      case TOP_fpmpy: new_opcode = TOP_fpma; break;
      case TOP_fmpy: new_opcode = TOP_fma; break;
      case TOP_fmpy_s: new_opcode = TOP_fma_s; break;
      case TOP_fmpy_d: new_opcode = TOP_fma_d; break;
      case TOP_fpnmpy: new_opcode = TOP_fpnma; break;
      case TOP_fnmpy: new_opcode = TOP_fnma; break;
      case TOP_fnmpy_s: new_opcode =  TOP_fnma_s; break;
      case TOP_fnmpy_d: new_opcode =  TOP_fnma_d; break;
      default: return FALSE;
      }

    } else if (OP_fsub(l1_op0) && (l2_tn1 == OP_opnd(op,l2_opnd1_idx))) {
     /* We actually started with: l1_tn0 = FSUB(FMULT(l3_tn1,l3_tn2),l2_tn0) */
     /* A multiple subtract is what we want.  There is no negative-multiple  */
     /* subtract instruction available.                                      */

      switch (l2_opcode) {
      case TOP_fpmpy: new_opcode = TOP_fpms; break;
      case TOP_fmpy: new_opcode = TOP_fms; break;
      case TOP_fmpy_s: new_opcode = TOP_fms_s; break;
      case TOP_fmpy_d: new_opcode = TOP_fms_d; break;
      default: return FALSE;
      }

    } else {
     /* We actually started with: l1_tn0 = FSUB(l2_tn0,FMULT(l3_tn1,l3_tn2)) */
     /* The actual op_code for the mutiply determines what we need to do.    */

      switch (l2_opcode) {
      case TOP_fpmpy: new_opcode = TOP_fpnma; break;
      case TOP_fmpy: new_opcode = TOP_fnma; break;
      case TOP_fmpy_s: new_opcode = TOP_fnma_s; break;
      case TOP_fmpy_d: new_opcode = TOP_fnma_d; break;
      case TOP_fpnmpy: new_opcode = TOP_fpma; break;
      case TOP_fnmpy: new_opcode = TOP_fma; break;
      case TOP_fnmpy_s: new_opcode =  TOP_fma_s; break;
      case TOP_fnmpy_d: new_opcode =  TOP_fma_d; break;
      default: return FALSE;
      }

    }

    new_fmadd = Mk_OP (new_opcode, l1_tn0, OP_opnd(op,OP_PREDICATE_OPND),
                       Gen_Enum_TN(ECV_sf_s0), l3_tn1, l3_tn2, l2_tn0);
    OP_srcpos(new_fmadd) = OP_srcpos(l1_op0);
    if (EBO_in_loop) EBO_Set_OP_omega ( new_fmadd, opnd_tninfo[OP_PREDICATE_OPND],
                                        NULL, l3_tninfo1, l3_tninfo2, l2_tninfo0);
    BB_Insert_Op_After(bb, op, new_fmadd);

if (EBO_Trace_Optimization) fprintf(TFile,"%s(%s) was converted to a madd: %s.\n",
TOP_Name(OP_code(op)),TOP_Name(OP_code(l2_op1)),TOP_Name(OP_code(new_fmadd)));
    return TRUE;
  }

  return FALSE;
}
 



/*
 * Function: fneg_sequence
 *
 * Look for these patterns:    and convert to:
 *       FNEG(FCMPLT(f1,fzero),f1) FMERGE.S(fzero,f1)
 *       FNEG(FMADD(f1,f2,f3))     FNMADD(f1,f2,f3)
 *       FNEG(FNMADD(f1,f2,f3))    FMADD(f1,f2,f3)
 *       FNEG(FMSUB(f1,f2,f3))     FNMSUB(f1,f2,f3)
 *       FNEG(FNMSUB(f1,f2,f3))    FMSUB(f1,f2,f3)
 *       FNEG(FNEG(f1))            f1
 *       FNEG(FSUB(f1,f2))         FSUB(f2,f1)
 *
 * To find the pattern, we need to look at 3 levels of operands:
 *    1) floating negation;
 *    2) a complementary operation;
 *    3) the input TN's to the operation.
 *
 */
static
BOOL
fneg_sequence (OP *op, TN **opnd_tn, EBO_TN_INFO **opnd_tninfo)
{
 /* Level 1 data: */
  TOP opcode = OP_code(op);
  BB *bb = OP_bb(op);
  OP *l1_op0 = op;
  TN *l1_tn0;
  INT op1_idx = TOP_Find_Operand_Use(opcode,OU_opnd1);

 /* Level 2 data: */
  TOP l2_opcode;
  OP *l2_op0;
  EBO_TN_INFO *l2_tninfo0;
  EBO_OP_INFO *l2_opinfo0;

 /* Level 3 data: */
  TN *l3_tn0;
  EBO_TN_INFO *l3_tninfo0;
  TN *l3_tn1;
  EBO_TN_INFO *l3_tninfo1;
  TN *l3_tn2;
  EBO_TN_INFO *l3_tninfo2;

  TOP new_opcode = TOP_UNDEFINED;
  OP *new_op;

  if ((opcode != TOP_fneg) &&
      (opcode != TOP_fpneg)) {
    return FALSE;
  }

 /* Look for special case merge that is really and ABS function. */
 /* The pattern that we are looking for is:
     TN715 TN716 :- fcmp.lt.unc TN257(p0) (enum:.s0) TN713 TN129(f0) ;
     GTN717 :- mov_f TN715 TN713 ; copy cond_def
     GTN717 :- fneg TN716 TN713 ; cond_def
 */
  TN *predicate_tn = opnd_tn[OP_PREDICATE_OPND];
  EBO_TN_INFO *predicate_tninfo = opnd_tninfo[OP_PREDICATE_OPND];
  if ((opcode == TOP_fneg) &&
      (predicate_tninfo != NULL) &&
      (predicate_tninfo->in_op != NULL) &&
      (!TN_Is_Constant(predicate_tn))) {
   /* The current result is conditionally defined.  Can we find a previous definition? */
    TN *tnr = OP_result(op,0);
    EBO_TN_INFO *previous_tninfo = get_tn_info(tnr);
    EBO_OP_INFO *previous_opinfo = NULL;
    EBO_TN_INFO *previous_op0_tninfo = NULL;
    if ((previous_tninfo != NULL)  &&
        (previous_tninfo->reference_count == 0) &&
        (previous_tninfo->predicate_tninfo != NULL)) {
      previous_opinfo = locate_opinfo_entry (previous_tninfo);
      if (previous_opinfo != NULL) {
        previous_op0_tninfo = previous_opinfo->optimal_opnd[op1_idx];
      }
    }
    if ((previous_op0_tninfo != NULL) &&
        (opnd_tn[op1_idx] == previous_op0_tninfo->local_tn) &&
        EBO_predicate_complements (predicate_tn, predicate_tninfo,
                                   previous_tninfo->predicate_tninfo->local_tn, previous_tninfo->predicate_tninfo)) {
      OP *predicate_op = predicate_tninfo->in_op;
      TOP predicate_opcode = OP_code(predicate_op);
      EBO_OP_INFO *predicate_opinfo = locate_opinfo_entry (predicate_tninfo);
      if ((predicate_opinfo != NULL) &&
          (predicate_opinfo->in_op != NULL) &&
          (((predicate_opcode == TOP_fcmp_gt_unc) || (predicate_opcode == TOP_fcmp_ge_unc)) &&
           (predicate_opinfo->actual_rslt[1] == predicate_tninfo) &&
           (OP_opnd(predicate_op, TOP_Find_Operand_Use(predicate_opcode,OU_opnd2)) == FZero_TN) &&
           (predicate_opinfo->actual_opnd[TOP_Find_Operand_Use(predicate_opcode,OU_opnd1)] == opnd_tninfo[op1_idx]))) {
if (EBO_Trace_Optimization) fprintf(TFile,"simplify fneg(fcmp()) operation.\n");
        new_opcode = TOP_fmerge_s;
        new_op = Mk_OP (new_opcode, tnr,
                        OP_opnd(predicate_op,OP_PREDICATE_OPND), FZero_TN, OP_opnd(op,1));
        OP_srcpos(new_op) = OP_srcpos(op);
        if (EBO_in_loop) EBO_Set_OP_omega ( new_op,
                                            predicate_opinfo->actual_opnd[OP_PREDICATE_OPND],
                                            opnd_tninfo[op1_idx]);
        BB_Insert_Op_After(bb, op, new_op);

       /* Now get rid of the previous conditional move. */
        remove_op (previous_opinfo);
        OP_Change_To_Noop(previous_opinfo->in_op);
        previous_opinfo->in_op = NULL;
        previous_opinfo->in_bb = NULL;
        return TRUE;
      }
    }
  }

  l1_tn0 = OP_result(l1_op0,0);
  l2_tninfo0 = opnd_tninfo[op1_idx];
  if ((l2_tninfo0 == NULL) ||
      (l2_tninfo0->in_op == NULL)) {
    return FALSE;
  }
  l2_op0 = l2_tninfo0->in_op;

 /* Determine the inputs to the second instructions. */
  l2_opinfo0 = locate_opinfo_entry (l2_tninfo0);
  if ((l2_opinfo0 == NULL) ||
      (l2_opinfo0->in_op == NULL)) {
    return FALSE;
  }

  l2_opcode = OP_code(l2_op0);

  INT l2_opnd1_idx = TOP_Find_Operand_Use(l2_opcode,OU_opnd1);
  INT l2_opnd2_idx = TOP_Find_Operand_Use(l2_opcode,OU_opnd2);
  INT l2_opnd3_idx = TOP_Find_Operand_Use(l2_opcode,OU_maddend);
  if (l2_opnd1_idx >= 0) {
    l3_tn0 = OP_opnd(l2_op0, l2_opnd1_idx);
    l3_tninfo0 = l2_opinfo0->actual_opnd[l2_opnd1_idx];
    if ((l3_tn0 == NULL) || TN_Is_Constant(l3_tn0) ||
        !EBO_tn_available (bb, l3_tninfo0)) return FALSE;

    if (l2_opnd2_idx >= 0) {
      l3_tn1 = OP_opnd(l2_op0, l2_opnd2_idx);
      l3_tninfo1 = l2_opinfo0->actual_opnd[l2_opnd2_idx];
      if ((l3_tn1 == NULL) || TN_Is_Constant(l3_tn1) ||
          !EBO_tn_available (bb, l3_tninfo1)) return FALSE;

      if (l2_opnd3_idx >= 0) {
        l3_tn2 = OP_opnd(l2_op0, l2_opnd3_idx);
        l3_tninfo2 = l2_opinfo0->actual_opnd[l2_opnd3_idx];
        if ((l3_tn2 == NULL) || TN_Is_Constant(l3_tn2) ||
            !EBO_tn_available (bb, l3_tninfo2)) return FALSE;
      }
    }
  }

 /* We have matched a pattern that depends on the second opcode. */

  if (l2_opnd3_idx <= 0) {
   /* negate a negation, an add, a subtract, or a multiply */

    switch (l2_opcode) {
    case TOP_fneg:
    case TOP_fpneg: {
      OPS ops = OPS_EMPTY;
if (EBO_Trace_Optimization) fprintf(TFile,"simplify fneg(fneg()) operation.\n");
      Exp_COPY(l1_tn0, l3_tn0, &ops);
      OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
      if (EBO_in_loop) EBO_Set_OP_omega ( OPS_last(&ops), opnd_tninfo[OP_PREDICATE_OPND], l3_tninfo0);
      BB_Insert_Ops(bb, op, &ops, FALSE);
      return TRUE;
    }
    case TOP_fsub:
    case TOP_fsub_s:
    case TOP_fsub_d: {
if (EBO_Trace_Optimization) fprintf(TFile,"simplify fneg(fsub()) operation.\n");
      new_op = Mk_OP (l2_opcode, l1_tn0, OP_opnd(op,OP_PREDICATE_OPND),
                      Gen_Enum_TN(ECV_sf_s0), l3_tn1, l3_tn0);
      OP_srcpos(new_op) = OP_srcpos(op);
      if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND],
                                          NULL, l3_tninfo1, l3_tninfo0);
      BB_Insert_Op_After(bb, op, new_op);
      return TRUE;
    }
    case TOP_fpmpy: new_opcode = TOP_fpnmpy; break;
    case TOP_fmpy: new_opcode = TOP_fnmpy; break;
    case TOP_fmpy_s: new_opcode = TOP_fnmpy_s; break;
    case TOP_fmpy_d: new_opcode = TOP_fnmpy_d; break;
    case TOP_fpnmpy: new_opcode = TOP_fpmpy; break;
    case TOP_fnmpy: new_opcode = TOP_fmpy; break;
    case TOP_fnmpy_s: new_opcode =  TOP_fmpy_s; break;
    case TOP_fnmpy_d: new_opcode =  TOP_fmpy_d; break;
    default: return FALSE;
    }

if (EBO_Trace_Optimization) fprintf(TFile,"simplify fneg(fmpy()) operation.\n");
    new_op = Mk_OP (new_opcode, l1_tn0, OP_opnd(op,OP_PREDICATE_OPND),
                    Gen_Enum_TN(ECV_sf_s0), l3_tn0, l3_tn1);
    OP_srcpos(new_op) = OP_srcpos(op);
    if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND],
                                        NULL, l3_tninfo0, l3_tninfo1);
    BB_Insert_Op_After(bb, op, new_op);
    return TRUE;
  } else {
   /* negate a madd */

   /* Sorry, there is no negative-multiply-subtract instruction. */
    return FALSE;
  }

  return FALSE;
}
 


 


/*
 * Function: shl_add_sequence
 *
 * Look for add(shl) sequence and replace with a single shladd
 * instruction. 
 *
 */
static
BOOL
shl_add_sequence (OP *op,
                  TN **opnd_tn,
                  EBO_TN_INFO **opnd_tninfo)
{
 /* Only adds of registers are transformable. */
  TOP opcode = OP_code(op);
  if (opcode != TOP_add) return FALSE;

 /* Level 1 data: */
  BB *bb = OP_bb(op);
  OP *l1_op0 = op;
  TN *l1_tn0 = OP_result(l1_op0,0);

 /* Level 2 data: */
  OP *l2_op0;
  TN *l2_tn1 = OP_opnd(op,2);
  EBO_TN_INFO *l2_tninfo0 = opnd_tninfo[1];
  EBO_TN_INFO *l2_tninfo1 = opnd_tninfo[2];
  EBO_OP_INFO *l2_opinfo0;

 /* Level 3 data: */
  TN *l3_tn0;
  TN *l3_tn1;
  EBO_TN_INFO *l3_tninfo0;
  INT64 l3_val1;

 /* The input to the add must be a shift-left-immediate. */
 /* Note that a shift-left-add instruction with an add   */
 /* of zero, is treated as a shift-left-immediate.       */
  if ((l2_tninfo0 == NULL) ||
      (l2_tninfo0->in_op == NULL) ||
      ((OP_code(l2_tninfo0->in_op) != TOP_shl_i) &&
       ((OP_code(l2_tninfo0->in_op) != TOP_shladd) ||
        (OP_opnd(l2_tninfo0->in_op,3) != Zero_TN)))) {
   /* Try the other operand of the add. */
    l2_tn1 = OP_opnd(op,1);
    l2_tninfo0 = l2_tninfo1;
    l2_tninfo1 = opnd_tninfo[1];

    if ((l2_tninfo0 == NULL) ||
        (l2_tninfo0->in_op == NULL) ||
        ((OP_code(l2_tninfo0->in_op) != TOP_shl_i) &&
         ((OP_code(l2_tninfo0->in_op) != TOP_shladd) ||
          (OP_opnd(l2_tninfo0->in_op,3) != Zero_TN)))) return FALSE;
  }

 /* Immediate constants won't fit into the addend field. */
  if (TN_is_constant(l2_tn1) || TN_is_symbol(l2_tn1)) return FALSE;

 /* Determine the inputs to the second instructions. */
  l2_opinfo0 = locate_opinfo_entry(l2_tninfo0);
  if ((l2_opinfo0 == NULL) ||
      (l2_opinfo0->in_op == NULL)) return FALSE;

  l2_op0 = l2_opinfo0->in_op;
  l3_tn0 = OP_opnd(l2_op0, 1);
  l3_tn1 = OP_opnd(l2_op0, 2);
  l3_tninfo0 = l2_opinfo0->actual_opnd[1];
  if ((l3_tn0 == NULL) || TN_Is_Constant(l3_tn0) ||
      (l3_tn1 == NULL) || !TN_Is_Constant(l3_tn1) ||
      !EBO_tn_available (bb, l3_tninfo0)) return FALSE;

 /* Will the shift count fit into the shladd instruction? */
  l3_val1 = TN_value(l3_tn1);
  if ((l3_val1 <= 0) || (l3_val1 > 4)) return FALSE;

 /* Replace the current instruction. */
    OP *new_op;
    new_op = Mk_OP(TOP_shladd, l1_tn0, OP_opnd(op,OP_PREDICATE_OPND),
                   l3_tn0, l3_tn1, l2_tn1);
    OP_srcpos(new_op) = OP_srcpos(op);
    if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND],
                                        l3_tninfo0, NULL, l2_tninfo1);
    BB_Insert_Op_After(bb, op, new_op);
if (EBO_Trace_Optimization) fprintf(TFile,"Convert add(shl) to shladd\n");
    return TRUE;
}
 
 


 


/*
 * Function: iadd_special_case
 *
 * Look for integer add instructions where the first operand is
 * relocated address that is at the top of the stack.  The add
 * is unnecessary.
 */
static
BOOL
iadd_special_case (OP *op,
                   TN **opnd_tn,
                   EBO_TN_INFO **opnd_tninfo)
{
  if (OP_code(op) == TOP_spadjust) return FALSE;

  TN *tn = opnd_tn[1];
  if (TN_is_constant(tn) &&
      !TN_has_spill(tn) &&
      TN_is_symbol(tn)) {
    ST *st = TN_var(tn);
    ST *base_st;
    INT64 base_ofst;
    INT64 val = TN_offset(tn);

    Base_Symbol_And_Offset (st, &base_st, &base_ofst);
    if (ST_on_stack(st) &&
        ((ST_sclass(st) == SCLASS_AUTO) ||
         (EBO_in_peep && (ST_sclass(st) == SCLASS_FORMAL)))) {
      val += base_ofst;
      if (val == 0) {
        OPS ops = OPS_EMPTY;
        EBO_Exp_COPY(OP_opnd(op,OP_PREDICATE_OPND), OP_result(op, 0), opnd_tn[2], &ops);
        if (EBO_in_loop) EBO_OPS_omega (&ops, opnd_tninfo[OP_PREDICATE_OPND]);
        OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
        BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
if (EBO_Trace_Optimization) fprintf(TFile,"Replace iadd with copy\n");
        return TRUE;
      }
    }
  }
  return FALSE;
}
 


 


/*
 * Function: copy_rf_sequence
 *
 * Look for copies form/to integer registers to/from floating
 * point registers.  If the input if a copy in the other direction,
 * both copies are unnecessary.
 *
 */
static
BOOL
copy_rf_sequence (OP *op,
                  TN **opnd_tn,
                  EBO_TN_INFO **opnd_tninfo)
{
 /* Level 1 data: */
  BB *bb = OP_bb(op);
  OP *l1_op0 = op;
  TN *l1_tn0 = OP_result(l1_op0,0);

 /* Level 2 data: */
  TOP l2_opcode;
  OP *l2_op0;
  TN *l2_tn0 = OP_opnd(op,1);
  EBO_TN_INFO *l2_tninfo0 = opnd_tninfo[1];
  EBO_OP_INFO *l2_opinfo0;

 /* Level 3 data: */
  TN *l3_tn0;
  EBO_TN_INFO *l3_tninfo0;

  if ((l2_tninfo0 == NULL) ||
      (l2_tninfo0->in_op == NULL)) {
    return FALSE;
  }

 /* Locate the EBO information about the input entry. */
  l2_opinfo0 = locate_opinfo_entry(l2_tninfo0);
  if ((l2_opinfo0 == NULL) ||
      (l2_opinfo0->in_op == NULL)) return FALSE;
  l2_op0 = l2_opinfo0->in_op;
  l2_opcode = OP_code(l2_op0);

 /* If the input to a setf.sig is a load, try to 
    perform the load directly into a floating register. */
  if ((OP_code(op) == TOP_setf_sig) &&
      ((l2_opcode == TOP_ld8)   ||
       (l2_opcode == TOP_ld8_i))) {
    OP *new_op;
    TOP new_opcode;
    INT i;

   /* Be sure there is no store between here and the original load. */
    if (l2_opinfo0->op_must_not_be_moved) return FALSE;
    /*
     *  Here we turn off the transformation: setf.sig--->ldf .
     *  This is because ebo can not correctly handle some cases.
     *  The bug is tough. We can not fix it in short time.
     *  So, we turn off it.
     */ 
    return FALSE;

   /* Be sure all the operands are available. */
    for (i = OP_opnds(l2_op0); i >= 0; i--) {
      if ((l2_opinfo0->actual_opnd[i] != NULL) &&
         !EBO_tn_available (bb, l2_opinfo0->actual_opnd[i])) {
        return FALSE;
      }
    }  

   /* Determine the fp load opcode. */
    new_opcode = (l2_opcode == TOP_ld8) ? TOP_ldf8 : TOP_ldf8_i;

   /* Generate the new load OP - with or without an increment. */
    if (OP_opnds(l2_op0) == 4) {
      new_op = Mk_OP (new_opcode, l1_tn0, OP_opnd(op,OP_PREDICATE_OPND),
                      Gen_Enum_TN(ECV_fldtype), Gen_Enum_TN(ECV_ldhint), OP_opnd(l2_op0,3));
      if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND],
                                          l2_opinfo0->actual_opnd[1], l2_opinfo0->actual_opnd[2],
                                          l2_opinfo0->actual_opnd[3]);
    } else if (OP_opnds(l2_op0) == 5) {
      new_op = Mk_OP (new_opcode, l1_tn0, OP_opnd(op,OP_PREDICATE_OPND),
                      Gen_Enum_TN(ECV_fldtype), Gen_Enum_TN(ECV_ldhint), OP_opnd(l2_op0,3), OP_opnd(l2_op0,4));
      if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND],
                                          l2_opinfo0->actual_opnd[1], l2_opinfo0->actual_opnd[2],
                                          l2_opinfo0->actual_opnd[3], l2_opinfo0->actual_opnd[4]);
    } else return FALSE;
    Copy_WN_For_Memory_OP (new_op, l2_op0);
    OP_srcpos(new_op) = OP_srcpos(op);

    BB_Insert_Op_After(bb, op, new_op);
if (EBO_Trace_Optimization) fprintf(TFile,"load directly to fp reg.\n");
    return TRUE;
  }

 /* The input to the setf/getf must be a getf/setf. */
  if ((l2_opcode != TOP_setf_sig) &&
      (l2_opcode != TOP_getf_sig)) return FALSE;

 /* Determine the inputs to the second instructions. */
  l2_op0 = l2_opinfo0->in_op;
  l3_tn0 = OP_opnd(l2_op0, 1);
  l3_tninfo0 = l2_opinfo0->actual_opnd[1];
  if ((l3_tn0 == NULL) || TN_Is_Constant(l3_tn0) ||
      !EBO_tn_available (bb, l3_tninfo0)) return FALSE;

  if (l2_opcode == TOP_getf_sig) {
    if ((l3_tninfo0 == NULL) ||
        (l3_tninfo0->in_op == NULL) ||
        ((OP_code(l3_tninfo0->in_op) != TOP_fcvt_fx) &&
         (OP_code(l3_tninfo0->in_op) != TOP_fcvt_fx_trunc) &&
         (OP_code(l3_tninfo0->in_op) != TOP_fcvt_fxu) &&
         (OP_code(l3_tninfo0->in_op) != TOP_fcvt_fxu_trunc) &&
         (OP_code(l3_tninfo0->in_op) != TOP_xma_l) &&
         (OP_code(l3_tninfo0->in_op) != TOP_xma_h) &&
         (OP_code(l3_tninfo0->in_op) != TOP_xma_lu) &&
         (OP_code(l3_tninfo0->in_op) != TOP_xma_hu) &&
         (OP_code(l3_tninfo0->in_op) != TOP_xmpy_l) &&
         (OP_code(l3_tninfo0->in_op) != TOP_xmpy_h) &&
         (OP_code(l3_tninfo0->in_op) != TOP_xmpy_lu) &&
         (OP_code(l3_tninfo0->in_op) != TOP_xmpy_hu))) {
     /* The getf_sig/setf_sig sequence sets the sign bit to positive.
        The movf instruction does not have this property.
        It is therefore necessary to be sure that the input to the
        sequence is an instruction that has already set the sign
        bit to positive, in order to safely use the movf instruction. */
      return FALSE;
    }
  }

  if ((TN_register_class(l1_tn0) == TN_register_class(l3_tn0)) &&
      ((TN_register_class(l1_tn0) == ISA_REGISTER_CLASS_integer) &&
       (TN_register_class(l2_tn0) == ISA_REGISTER_CLASS_float)) ||
      ((TN_register_class(l1_tn0) == ISA_REGISTER_CLASS_float) &&
       (TN_register_class(l2_tn0) == ISA_REGISTER_CLASS_integer))) {

   /* The current instruction is not needed. */
    OPS ops = OPS_EMPTY;
    EBO_Exp_COPY(OP_opnd(op,OP_PREDICATE_OPND),
                 OP_result(op,0), l3_tn0, &ops);
    if (EBO_in_loop) EBO_Set_OP_omega (OPS_first(&ops),
                                       opnd_tninfo[OP_PREDICATE_OPND],
                                       l3_tninfo0);
    OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
    BB_Insert_Ops(bb, op, &ops, FALSE);
if (EBO_Trace_Optimization) fprintf(TFile,"copies between INT and FLOAT not needed.\n");
    return TRUE;
  }
  return FALSE;
}
 


 


/*
 * Function: store_sequence
 *
 * Look for stores of values that are moved between register types
 * and try to perform the store from the original register type.
 *
 */
static
BOOL
store_sequence (OP *op,
                TN **opnd_tn,
                EBO_TN_INFO **opnd_tninfo)
{
 /* Level 1 data: */
  TOP opcode = OP_code(op);
  BB *bb = OP_bb(op);
  TN *l1_tn0;
  INT storeval_idx = TOP_Find_Operand_Use(opcode,OU_storeval);
  TN *store_value;
  EBO_TN_INFO *store_value_info;
  INT storebase_idx = TOP_Find_Operand_Use(opcode,OU_base);
  TN *storebase;
  EBO_TN_INFO *store_base_info;
  INT store_incr_idx = TOP_Find_Operand_Use(opcode, OU_postincr);
  TN *store_incr;
  EBO_TN_INFO *store_incr_info;

 /* Level 2 data: */
  TOP l2_opcode;
  OP *l2_op0;
  TN *l2_tn0 = OP_opnd(op,1);
  EBO_TN_INFO *l2_tninfo0 = opnd_tninfo[1];
  EBO_OP_INFO *l2_opinfo0;

  TOP new_opcode = TOP_UNDEFINED;
  OP *new_op;

  if (!OP_store(op) ||
      (storeval_idx < 0) ||
      (storebase_idx < 0)) return FALSE;

 /* Locate the EBO information about the input entry. */
  storebase = OP_opnd(op,storebase_idx);
  store_value = OP_opnd(op,storeval_idx);
  store_value_info = opnd_tninfo[storeval_idx];
  store_base_info = opnd_tninfo[storebase_idx];
  store_incr_idx = TOP_Find_Operand_Use(opcode, OU_postincr);
  if (store_incr_idx >= 0) {
    store_incr = OP_opnd(op,store_incr_idx);
    store_incr_info = opnd_tninfo[store_incr_idx];
  }

 /* There are special requirements for "spill" temps
    that prevents their replacement. */
  if (!TN_Is_Constant(OP_opnd(op,storeval_idx)) &&
      TN_has_spill(OP_opnd(op,storeval_idx))) {
    return FALSE;
  }

  l2_opinfo0 = locate_opinfo_entry(store_value_info);
  if ((l2_opinfo0 == NULL) ||
      (l2_opinfo0->in_op == NULL)) return FALSE;
  l2_op0 = l2_opinfo0->in_op;
  l2_opcode = OP_code(l2_op0);

 /* If the input is a getf.sig, try to perform
    the store directly from the floating register. */
  if ((l2_opcode == TOP_getf_sig) &&
      ((opcode == TOP_st8)   ||
       (opcode == TOP_st8_i))) {
    OP *new_op;
    TOP new_opcode;

   /* Be sure there is no store between here and the original load. */
    if (l2_opinfo0->op_must_not_be_moved) return FALSE;

   /* Be sure the converted operand is available. */
    store_value = OP_opnd(l2_op0,1);
    store_value_info = l2_opinfo0->actual_opnd[1];
    if ((store_value_info != NULL) &&
        !EBO_tn_available (bb, store_value_info)) return FALSE;

   /* Determine the fp store opcode. */
    new_opcode = (opcode == TOP_st8) ? TOP_stf8 : TOP_stf8_i;

   /* Generate the new store OP - with or without an increment. */
    if (new_opcode == TOP_stf8_i) {
      new_op = Mk_OP (new_opcode, OP_result(op,0), OP_opnd(op,OP_PREDICATE_OPND),
                      Gen_Enum_TN(ECV_sthint), storebase, store_value, store_incr);
      if (EBO_in_loop) {
        EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND],
                           opnd_tninfo[1], store_base_info, store_value_info, store_incr_info);
      }
    } else {
      new_op = Mk_OP (new_opcode, OP_opnd(op,OP_PREDICATE_OPND),
                      Gen_Enum_TN(ECV_sthint), storebase, store_value);
      if (EBO_in_loop) {
        EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND],
                           opnd_tninfo[1], store_base_info, store_value_info);
      }
    }
    Copy_WN_For_Memory_OP (new_op, op);
    OP_srcpos(new_op) = OP_srcpos(op);

    BB_Insert_Op_After(bb, op, new_op);
if (EBO_Trace_Optimization) fprintf(TFile,"Store directly from fp reg\n");
    return TRUE;
  }

  if (OP_store(op) &&
      (storeval_idx >= 0) &&
      (storebase_idx >= 0) &&
      (OP_opnd(op,storeval_idx) != opnd_tn[storeval_idx])) {
    storebase = OP_opnd(op,storebase_idx);
    l1_tn0 = OP_opnd(op,storeval_idx);
    l2_tn0 = opnd_tn[storeval_idx];
    l2_tninfo0 = opnd_tninfo[storeval_idx];
    if ((l2_tn0 == NULL) ||
        TN_Is_Constant(l2_tn0) ||
        (l2_tninfo0 == NULL) ||
        (l2_tninfo0->in_op == NULL) ||
        (TN_size(l1_tn0) > TN_size(l2_tn0)) ||
        (CGTARG_Mem_Ref_Bytes(op) > TN_size(l2_tn0)) ||
        !EBO_tn_available (bb, l2_tninfo0)) {
      return FALSE;
    }

    switch (opcode) {
    case TOP_st4: 
      if (l2_opcode == TOP_getf_s) {
        new_opcode = TOP_stfs; 
      } else return FALSE;
      break;
    case TOP_st4_i: 
      if (l2_opcode == TOP_getf_s) {
        new_opcode = TOP_stfs_i; 
      } else return FALSE;
      break;
    case TOP_st8: 
      if (l2_opcode == TOP_getf_sig) {
        new_opcode = TOP_stf8;
      } else if (l2_opcode == TOP_getf_d) {
        new_opcode = TOP_stfd;
      } else return FALSE;
      break;
    case TOP_st8_i: 
      if (l2_opcode == TOP_getf_sig) {
        new_opcode = TOP_stf8_i;
      } else if (l2_opcode == TOP_getf_d) {
        new_opcode = TOP_stfd_i; 
      } else return FALSE;
      break;
    case TOP_stf8: new_opcode = TOP_st8; break;
    case TOP_stf8_i: new_opcode = TOP_st8_i; break;
    case TOP_stfs: /* Different format! */
    case TOP_stfd: /* Different format! */
    default: return FALSE;
    }

    if ((TN_register_class(l1_tn0) == ISA_REGISTER_CLASS_integer) &&
        (TN_register_class(l2_tn0) == ISA_REGISTER_CLASS_float)) {
      TN *st_hint = OP_opnd(op,2);
      if ((new_opcode == TOP_stf8_i) ||
          (new_opcode == TOP_stfd_i) ||
          (new_opcode == TOP_stfs_i)) {
        new_op = Mk_OP (new_opcode, OP_result(op,0), OP_opnd(op,OP_PREDICATE_OPND),
                        st_hint, storebase, l2_tn0,
                        OP_opnd(op, TOP_Find_Operand_Use(opcode, OU_postincr)));
        if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND],
                                            NULL, opnd_tninfo[storebase_idx], opnd_tninfo[storeval_idx], NULL);
      } else {
        new_op = Mk_OP (new_opcode, OP_opnd(op,OP_PREDICATE_OPND),
                        st_hint, storebase, l2_tn0);
        if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND], 
                                            NULL, opnd_tninfo[storebase_idx], opnd_tninfo[storeval_idx]);
      }

      Copy_WN_For_Memory_OP (new_op, op);
      OP_srcpos(new_op) = OP_srcpos(op);
      BB_Insert_Op_After( bb, op, new_op);
if (EBO_Trace_Optimization) fprintf(TFile,"Replace INT store with FP store\n");
      return TRUE;
    }

    if ((TN_register_class(l1_tn0) == ISA_REGISTER_CLASS_float) &&
        (TN_register_class(l2_tn0) == ISA_REGISTER_CLASS_integer)) {
      TN *st_hint = OP_opnd(op,1);
      if (new_opcode == TOP_st8_i) {
        new_op = Mk_OP (new_opcode, OP_result(op,0), OP_opnd(op,OP_PREDICATE_OPND),
                        Gen_Enum_TN(ECV_sttype), st_hint, storebase, l2_tn0,
                        OP_opnd(op, TOP_Find_Operand_Use(opcode, OU_postincr)));
        if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND],
                                            NULL, NULL, opnd_tninfo[storebase_idx], opnd_tninfo[storeval_idx], NULL);
      } else {
        new_op = Mk_OP (new_opcode, OP_opnd(op,OP_PREDICATE_OPND),
                        Gen_Enum_TN(ECV_sttype), st_hint, storebase, l2_tn0);
        if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND], 
                                            NULL, NULL, opnd_tninfo[storebase_idx], opnd_tninfo[storeval_idx]);
      }

      Copy_WN_For_Memory_OP (new_op, op);
      OP_srcpos(new_op) = OP_srcpos(op);
      BB_Insert_Op_After( bb, op, new_op);
if (EBO_Trace_Optimization) fprintf(TFile,"Replace FP store with INT store\n");
      return TRUE;
    }

  }

  return FALSE;
}

 
/* 
 * Generate an extract operation. The result will be a single
 * instruction that performs the extract. It may not be an extr or extr.u.
 */
static 
OP *
Generate_Extract(TOP extr_opc,
		 TN *dest,
		 TN *pred,
		 TN *src,
		 INT pos,
		 INT len)
{
  if (pos == 0) {
    BOOL is_signed = (extr_opc == TOP_extr);

    if (len == 8 || len == 16 || len == 32) {
      TOP new_opc;
      switch (len >> 3) {
      case 1: new_opc = is_signed ? TOP_sxt1 : TOP_zxt1; break;
      case 2: new_opc = is_signed ? TOP_sxt2 : TOP_zxt2; break;
      case 4: new_opc = is_signed ? TOP_sxt4 : TOP_zxt4; break;
      }
      return Mk_OP(new_opc, dest, pred, src);
    } else if (!is_signed) {
      UINT64 mask = -1ULL >> (64 - len);
      if (ISA_LC_Value_In_Class(mask, LC_i8)) {
	return Mk_OP(TOP_and_i, dest, pred, Gen_Literal_TN(mask, 4), src);
      }
    }
  } 

  return Mk_OP(extr_opc, dest, pred, src, 
	       Gen_Literal_TN(pos, 4), Gen_Literal_TN(len, 4));
}


/*
 * Function: sxt_sequence
 *
 * Look for sign extension operations that are not needed.
 *
 *	Translation Matrix 
 *             (RL is result length for sequence)
 *             (IL is input length for final operation)
 *             (ZXT is one of the following: and(mask), extr_u, zxt[124])
 *             (SXT is one of the following: extr, sxt[124])
 *             (EXTR_U is an instruction that performs an extr.u)
 *             (EXTR is an instruction that performs an extr)
 *
 * Note: shift_right is the same as an extr operation.
 *
 *  origional		      translated to, if ..
 *  sequence		  RL<IL		RL=IL	RL>IL
 * -----------		-----------	-----	-----`
 * ZXT(ZXT)		EXTR_U		EXTR_U	EXTR_U
 * ZXT(SXT)		EXTR_U		EXTR_U	Illegal
 * SXT(SXT)		EXTR		EXTR	EXTR
 * SXT(ZXT)		EXTR		EXTR	EXTR_U
 *
 * For the following, if ZXT/SXT is an extract, the 'pos' operand
 * must be zero.
 *
 * ld(ZXT)		Illegal		ld	ld
 * ld(SXT)		Illegal		Illegal	ld
 * cmp(SXT/ZXT)		cmp		cmp	Illegal
 * st(SXT/ZXT)		st		st	Illegal
 * dep(SXT/ZXT)		dep		dep	Illegal
 * dep_z(SXT/ZXT)	dep_z		dep_z	Illegal
 */
static
BOOL
sxt_sequence (OP *op,
              INT op_idx,
              TN **opnd_tn,
              EBO_TN_INFO **opnd_tninfo)
{
 /* Level 1 data: */
  TOP opcode = OP_code(op);
  BB *bb = OP_bb(op);
  INT sxt_idx = 1; /* TOP_Find_Operand_Use(OP_code(op),OU_opnd1); doesn't work */
  EBO_TN_INFO *sxt_tninfo;

  OP *pred_op;
  TOP pred_opcode;

  INT result_start = 0; /* assume the input is right justified in register. */
  INT result_length;
  INT input_start = 0; /* assume the input is right justified in register. */
  INT input_length;

  if (sxt_idx < 0) return FALSE;

  if (EBO_Trace_Execution) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile,"Enter sxt_sequence\n");
  }

  sxt_tninfo = opnd_tninfo[op_idx];

  if ((sxt_tninfo == NULL) ||
      (sxt_tninfo->in_op == NULL)) return FALSE;

 /* There are special requirements for "spill" temps
    that prevents their replacement. */
  if (OP_store(op)) {
    TN *store_value = OP_opnd(op,op_idx);
    if (!TN_Is_Constant(store_value) &&
        TN_has_spill(store_value)) {
      return FALSE;
    }
  }
 
  pred_op = sxt_tninfo->in_op;
  pred_opcode = OP_code(pred_op);

 /* What is the length of the result and input? */
  if ((pred_opcode == TOP_and) ||
      (pred_opcode == TOP_andcm)) {
      EBO_OP_INFO *and_opinfo = locate_opinfo_entry (sxt_tninfo);
      if ((and_opinfo == NULL) ||
          (and_opinfo->in_op == NULL)) return FALSE;
      EBO_TN_INFO *pred_info1 = and_opinfo->actual_opnd[1];
      EBO_TN_INFO *pred_info2 = and_opinfo->actual_opnd[2];
      TN *pred_op1;
      TN *pred_op2;
      TN *mask_tn;
      INT32 mask_size;
      INT32 mask_shift;

      if (pred_info1 != NULL) {
        if (pred_info1->replacement_tn != NULL) {
          pred_op1 = pred_info1->replacement_tn;
        } else {
          pred_op1 = pred_info1->local_tn;
        }
      } else {
        pred_op1 = OP_opnd(pred_op,1);
      }
      if (pred_info2 != NULL) {
        if (pred_info2->replacement_tn != NULL) {
          pred_op2 = pred_info2->replacement_tn;
        } else {
          pred_op2 = pred_info2->local_tn;
        }
      } else {
        pred_op2 = OP_opnd(pred_op,2);
      }

      if (TN_Is_Constant(pred_op1)) {
        mask_tn = pred_op1;
        mask_shift = Get_Mask_Shift_Count( TN_Value(mask_tn) );
        mask_size = Get_Right_Mask_Length( TN_Value(mask_tn) >> mask_shift);
      } else if (TN_Is_Constant(pred_op2)) {
        mask_tn = pred_op2;
        if ((pred_opcode == TOP_andcm) || (pred_opcode == TOP_andcm_i)) {
          mask_shift = Get_Mask_Shift_Count( ~TN_Value(mask_tn) );
          mask_size = Get_Right_Mask_Length( ~TN_Value(mask_tn) >> mask_shift);
        } else {
          mask_shift = Get_Mask_Shift_Count( TN_Value(mask_tn) );
          mask_size = Get_Right_Mask_Length( ((UINT64)TN_Value(mask_tn) >> mask_shift) );
        }
      } else return FALSE;
 
    input_start = mask_shift;
    input_length = mask_size;

    if (input_start != 0) return FALSE;

  } else {
    input_length = EBO_bit_length(pred_op);
  }

  result_length = EBO_bit_length (op);
  if ((result_length < 0) || (input_length < 0)) return FALSE;

 /* Determine start bit used by extr and shift instructions. */
  if ((opcode == TOP_extr)  || (opcode == TOP_extr_u) ||
      (opcode == TOP_shr_i) || (opcode == TOP_shr_i_u)) {
    result_start = TN_Value( OP_opnd(op,2) );
  }
  if ((pred_opcode == TOP_extr)  || (pred_opcode == TOP_extr_u) ||
      (pred_opcode == TOP_shr_i) || (pred_opcode == TOP_shr_i_u)) {
    input_start = TN_Value( OP_opnd(pred_op,2) );
  }
  if (pred_opcode == TOP_dep) {
    INT p_start = TN_Value( OP_opnd(pred_op,3) );
    if (((result_start + result_length) <= p_start) ||
        ((p_start + input_length) <= result_start)) {
     /* The OP does not access the field that was inserted by the dep OP. */
     /* Access the input to the dep and extract the data from it.         */
      input_start = 0;
      input_length = 64;
      sxt_idx = 2;
    } else if ((p_start == result_start) &&
               ((result_start + result_length) <= (p_start + input_length))) {
     /* The result is contained within the input. */
      result_start = 0;
    } else if ((opcode == TOP_extr_u) &&
               (result_start == 0) &&
               ((p_start + input_length) <= result_length)) {
     /* The dep does not extend the size of it's input, but just replaces a field. */
     /* If the result of the extr is greater than or equal to the size of the input
        to the dep, it may not be necessary. */
      EBO_OP_INFO *dep_opinfo = locate_opinfo_entry (sxt_tninfo);
      if ((dep_opinfo == NULL) ||
          (dep_opinfo->in_op == NULL)) return FALSE;
      EBO_TN_INFO *dep_info1 = dep_opinfo->optimal_opnd[2];
      if ((dep_info1 != NULL) &&
          (dep_info1->in_op != NULL) &&
          (EBO_bit_length(dep_info1->in_op) <= result_length)) {
       /* The result of the OP is greater than or equal to the size of the input to the dep. */

        OPS ops = OPS_EMPTY;
        EBO_Exp_COPY(OP_opnd(op,OP_PREDICATE_OPND),
                     OP_result(op,0), OP_opnd(op, op_idx), &ops);

        if (EBO_in_loop) EBO_Set_OP_omega (OPS_first(&ops),
                                           opnd_tninfo[OP_PREDICATE_OPND],
                                           opnd_tninfo[op_idx]);
        OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
        BB_Insert_Ops(bb, op, &ops, FALSE);
        if (EBO_Trace_Optimization) {
          #pragma mips_frequency_hint NEVER
          fprintf(TFile,"%s of %s replaced with copy of input\n",
                        TOP_Name(opcode), TOP_Name(pred_opcode));
        }

      return TRUE;
      }

      return FALSE;
    } else {
     /* There is some sort of overlap of fields. */
      return FALSE;
    }
  }
  if (pred_opcode == TOP_dep_z) {
    INT p_start = TN_Value( OP_opnd(pred_op,2) );
    if (((result_start + result_length) <= p_start) ||
        ((p_start + input_length) <= result_start)) {
     /* The OP does not access the field that was inserted by the dep OP. */
     /* The result of the dep must be "0", so use Zero_TN in place of     */
     /* the original result TN of the dep.                                */

      OP *new_op = Dup_OP(op);
      Set_OP_opnd (new_op, op_idx, Zero_TN);
      if (OP_store(op)) {
        Copy_WN_For_Memory_OP (new_op, op);
      }

      if (EBO_in_loop) {
        EBO_Copy_OP_omega (new_op, op);
        Set_OP_omega (op, op_idx, 0);
      }

      BB_Insert_Op_After(bb, op, new_op);
      if (EBO_Trace_Optimization) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"OP does not access inserted field of dep_z.\n");
      }

      return TRUE;
    } else if ((p_start <= result_start) &&
               ((result_start + result_length) <= (p_start + input_length))) {
     /* The result is contained within the input. */
      result_start = result_start - p_start;  // OSP-170
    } else {
     /* There is some sort of overlap of fields. */
      return FALSE;
    }
  }

  BOOL input_zxt  = (pred_opcode == TOP_and_i)   ||
		    (pred_opcode == TOP_and)     ||
		    (pred_opcode == TOP_andcm)   ||
		    (pred_opcode == TOP_andcm_i) ||
		    (pred_opcode == TOP_dep)     ||
		    (pred_opcode == TOP_dep_z)   ||
		    (pred_opcode == TOP_extr_u)  ||
		    (pred_opcode == TOP_shr_i_u) ||
		    (pred_opcode == TOP_zxt1)    ||
		    (pred_opcode == TOP_zxt2)    ||
		    (pred_opcode == TOP_zxt4);
  BOOL input_sxt  = (pred_opcode == TOP_extr)   ||
		    (pred_opcode == TOP_shr_i)  ||
		    (pred_opcode == TOP_sxt1)   ||
		    (pred_opcode == TOP_sxt2)   ||
		    (pred_opcode == TOP_sxt4);
  BOOL result_zxt = (opcode == TOP_and_i)   ||
		    (opcode == TOP_extr_u)  ||
		    (opcode == TOP_shr_i_u) ||
		    (opcode == TOP_zxt1)    ||
		    (opcode == TOP_zxt2)    ||
		    (opcode == TOP_zxt4);
  BOOL result_sxt = (opcode == TOP_extr)   ||
		    (opcode == TOP_shr_i)  ||
		    (opcode == TOP_sxt1)   ||
		    (opcode == TOP_sxt2)   ||
		    (opcode == TOP_sxt4);

  /* Look for sequences that can be replaced by an extract operation.
   * These are basically ANDs of ANDs with some funny side effects
   * to complicate things.
   */
  if ((input_zxt || input_sxt) && (result_zxt || result_sxt)) {

    /* If the first inst of the pair is an extract, we remember that
     * the input is shifted to be right-justified so a final correction
     * can be applied later. Other than that correction, we treat it
     * as a plain AND.
     */
    INT input_shift = 0;
    if ((pred_opcode == TOP_extr_u)  || (pred_opcode == TOP_extr) ||
        (pred_opcode == TOP_dep_z)   || (pred_opcode == TOP_dep)  ||
        (pred_opcode == TOP_shr_i_u) || (pred_opcode == TOP_shr_i)) {
      input_shift = input_start;
      input_start = 0;
    }

    /* Compute the start and length of the replacement extract operation.
     */
    INT input_end = input_start + input_length;
    INT result_end = result_start + result_length;
    INT new_end = MIN(input_end, result_end);
    INT new_start = MAX(input_start, result_start);
    INT new_length = new_end - new_start;
    new_start += input_shift;

    if (new_length <= 0) {

      if (input_sxt && (result_start >= input_length)) return FALSE;

    /* If there is no overlap between the two ANDs so the result is 0! */
      OPS ops = OPS_EMPTY;
      EBO_Exp_COPY(OP_opnd(op,OP_PREDICATE_OPND),
                   OP_result(op,0), Zero_TN, &ops);
      if (EBO_in_loop) EBO_Set_OP_omega (OPS_first(&ops),
                                         opnd_tninfo[OP_PREDICATE_OPND],
                                         NULL);
      OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
      BB_Insert_Ops(bb, op, &ops, FALSE);
      if (EBO_Trace_Optimization) {
	#pragma mips_frequency_hint NEVER
	fprintf(TFile,"%s of %s replaced with copy of zero\n",
		      TOP_Name(opcode), TOP_Name(pred_opcode));
      }

      return TRUE;
    }

    /* Determine if the replacement extract is signed or unsigned.
     */
    TOP new_opc = TOP_extr_u;
    if (result_sxt) {
      if (input_sxt || result_end <= input_end) new_opc = TOP_extr;
    } else if (input_sxt) {
      if (result_end > input_end) return FALSE;
    }

    /* Be sure the input operand is available.
     */
    EBO_OP_INFO *and_opinfo = locate_opinfo_entry (sxt_tninfo);
    INT and_idx = (pred_opcode == TOP_and_i) ? 2 : sxt_idx;
    if ((and_opinfo == NULL) ||
        (and_opinfo->in_op == NULL) ||
        (and_opinfo->actual_opnd[and_idx] == NULL) ||
        (!EBO_tn_available(bb, and_opinfo->actual_opnd[and_idx]))) return FALSE;

    /* Generate the replacement extract and insert into the BB.
     */
    TN *src = OP_opnd(pred_op,and_idx);
    OP *new_op = Generate_Extract(new_opc, OP_result(op,0), OP_opnd(op,0),
				  src, new_start, new_length);
    INT src_idx = TN_Opernum_In_OP (new_op, src);
    new_opc = OP_code(new_op);
    OP_srcpos(new_op) = OP_srcpos(op);
    if (EBO_in_loop) {
     /* We need to determine where the src operand was inserted,
        since we do not know what instruction was generated. */
      OPS ops = OPS_EMPTY;
      OPS_Insert_Op (&ops, NULL, new_op, TRUE);
      EBO_TN_INFO *and_tninfo = and_opinfo->actual_opnd[and_idx];
      EBO_OPS_omega (&ops, opnd_tninfo[OP_PREDICATE_OPND]);
      Set_OP_omega (new_op, src_idx, (and_tninfo != NULL) ? and_tninfo->omega : 0);
    }
    BB_Insert_Op_After(OP_bb(op), op, new_op);

    if (EBO_Trace_Optimization) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile,"%s of %s replaced with %s.\n",
		    TOP_Name(opcode), TOP_Name(pred_opcode), TOP_Name(new_opc));
    }
    return TRUE;
  }  else if (OP_load(pred_op)) {
    // ???? handle start > length
    if (result_start == 0 &&
	((result_zxt && result_length >= input_length) ||
	 (result_sxt && result_length > input_length)))
    {
     /* The current instruction is not needed. */
      OPS ops = OPS_EMPTY;
      EBO_Exp_COPY(OP_opnd(op,OP_PREDICATE_OPND),
                   OP_result(op,0), opnd_tn[op_idx], &ops);
      if (EBO_in_loop) EBO_Set_OP_omega (OPS_first(&ops),
                                         opnd_tninfo[OP_PREDICATE_OPND],
                                         opnd_tninfo[op_idx]);
      OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
      BB_Insert_Ops(bb, op, &ops, FALSE);
      if (EBO_Trace_Optimization) {
	#pragma mips_frequency_hint NEVER
	fprintf(TFile,"skip %s after load operation\n", TOP_Name(opcode));
      }

      return TRUE;
    }
  }  else if (OP_store(op) || OP_icmp(op) || 
	      opcode == TOP_dep || opcode == TOP_dep_z)
  {
    if (input_start == 0 && input_length >= result_length) {
     /* The previous sxt/zxt instruction is not needed for
        the correct execution of this instruction. Bypass it. */

     /* Be sure the input operand is available. */
      EBO_OP_INFO *sxt_opinfo = locate_opinfo_entry (sxt_tninfo);
      if ((sxt_opinfo == NULL) ||
          (sxt_opinfo->in_op == NULL) ||
          (sxt_opinfo->actual_opnd[sxt_idx] == NULL) ||
          (!EBO_tn_available(bb, sxt_opinfo->actual_opnd[sxt_idx]))) return FALSE;

     /* Copy the original instruction and replace the operand. */
      OP *new_op = Dup_OP (op);
      Set_OP_opnd (new_op, op_idx, OP_opnd(pred_op,sxt_idx));
      OP_srcpos(new_op) = OP_srcpos(op);
      if (OP_store(op)) {
        Copy_WN_For_Memory_OP (new_op, op);
        if (EBO_in_loop) {
          if (OP_opnds(op) == 5) {
            EBO_Set_OP_omega (new_op, opnd_tninfo[OP_PREDICATE_OPND],
                              NULL, NULL, NULL, sxt_opinfo->actual_opnd[sxt_idx]);
          } else if (OP_opnds(op) == 6) {
           /* Incremented stores - the last operand is a constant */
            EBO_Set_OP_omega (new_op, opnd_tninfo[OP_PREDICATE_OPND],
                              NULL, NULL, NULL, sxt_opinfo->actual_opnd[sxt_idx], NULL);
          } else return FALSE;
        }
      } else if (EBO_in_loop) {
        EBO_TN_INFO *sxt_tninfo = sxt_opinfo->actual_opnd[sxt_idx];
        EBO_TN_INFO *pred_tninfo = opnd_tninfo[OP_PREDICATE_OPND];
	if (OP_opnds(op) == 2) {
	  // compare with zero
	  EBO_Set_OP_omega (new_op, pred_tninfo, sxt_tninfo);
	} else if ((OP_opnds(op) == 3) && (op_idx == 1)) { 
	  // compare
	  EBO_Set_OP_omega (new_op, pred_tninfo, sxt_tninfo, opnd_tninfo[2]);
	} else if ((OP_opnds(op) == 3) && (op_idx == 2)) {
	  // compare
	  EBO_Set_OP_omega (new_op, pred_tninfo, opnd_tninfo[1], sxt_tninfo);
	} else if ((OP_opnds(op) == 4) && (op_idx == 1)) {
	  // dep_z
	  EBO_Set_OP_omega (new_op, pred_tninfo, sxt_tninfo, opnd_tninfo[2],
				    opnd_tninfo[3]);
        } else if ((OP_opnds(op) == 5) && (op_idx == 1)) {
	  // dep
	  EBO_Set_OP_omega (new_op, pred_tninfo, sxt_tninfo, opnd_tninfo[2],
				    opnd_tninfo[3], opnd_tninfo[4]);
	} else return FALSE;
      }
      BB_Insert_Op_After(OP_bb(op), op, new_op);
      if (EBO_Trace_Optimization) {
	#pragma mips_frequency_hint NEVER
	fprintf(TFile,"bypass %s before %s operation\n", 
		      TOP_Name(pred_opcode), TOP_Name(opcode));
      }

      return TRUE;
    }
  }

  return FALSE;
}



static
void
EBO_Identify_Base_and_Offset (EBO_TN_INFO **opnd_tninfo, TN **opnd_tn, INT64 *opnd_offset)
{
  EBO_TN_INFO *check_tninfo = *opnd_tninfo;
  TN *indx_tn = *opnd_tn;
  INT64 offset = 0;
  BOOL keep_going = TRUE;

  while (keep_going && (check_tninfo != NULL)) {
    keep_going = FALSE;
    EBO_OP_INFO *opnd_opinfo = locate_opinfo_entry(check_tninfo);
    if ((opnd_opinfo != NULL) &&
        (opnd_opinfo->in_op != NULL)) {
      OP *input_op = opnd_opinfo->in_op;
      if (OP_effectively_copy(input_op)) {
        check_tninfo = opnd_opinfo->actual_opnd[EBO_Copy_Operand(input_op)];
        indx_tn = OP_opnd(input_op,EBO_Copy_Operand(input_op));
        keep_going = TRUE;
      } else if (OP_memory(input_op)) {
        INT base_idx = TOP_Find_Operand_Use(OP_code(input_op),OU_base);
        INT inc_idx = TOP_Find_Operand_Use(OP_code(input_op),OU_postincr);
        if ((inc_idx < 0) || (!TNs_Are_Equivalent(OP_opnd(input_op,base_idx),check_tninfo->local_tn))) {
          check_tninfo = NULL;
          break;
        }
        offset += TN_Value(OP_opnd(input_op,inc_idx));
        check_tninfo = opnd_opinfo->actual_opnd[base_idx];
        indx_tn = OP_opnd(input_op,base_idx);
        keep_going = TRUE;
      } else if (OP_iadd(input_op)) {
        INT op1_idx = TOP_Find_Operand_Use(OP_code(input_op),OU_opnd1);
        INT op2_idx = TOP_Find_Operand_Use(OP_code(input_op),OU_opnd2);

        if ((op1_idx >= 0) && (op2_idx >= 0)) {
          EBO_TN_INFO *tmp1_tninfo = opnd_opinfo->actual_opnd[op1_idx];
          EBO_TN_INFO *tmp2_tninfo = opnd_opinfo->actual_opnd[op2_idx];
          TN *tmp1_tn;
          TN *tmp2_tn;
          if (tmp1_tninfo != NULL) {
            if ((tmp1_tninfo->replacement_tn) &&
                (TN_is_symbol(tmp1_tninfo->replacement_tn) || TN_Is_Constant(tmp1_tninfo->replacement_tn))) {
              tmp1_tn = tmp1_tninfo->replacement_tn;
              tmp1_tninfo = tmp1_tninfo->replacement_tninfo;
            } else {
              tmp1_tn = tmp1_tninfo->local_tn;
            }
          } else {
            tmp1_tn = OP_opnd(input_op,op1_idx);
          }
          if (tmp2_tninfo != NULL) {
            if ((tmp2_tninfo->replacement_tn) &&
                (TN_is_symbol(tmp2_tninfo->replacement_tn) || TN_Is_Constant(tmp2_tninfo->replacement_tn))) {
              tmp2_tn = tmp2_tninfo->replacement_tn;
              tmp2_tninfo = tmp2_tninfo->replacement_tninfo;
            } else {
              tmp2_tn = tmp2_tninfo->local_tn;
            }
          } else {
            tmp2_tn = OP_opnd(input_op,op2_idx);
          }

          if (!TN_is_symbol(tmp1_tn) && TN_Is_Constant(tmp1_tn)) {
            offset += TN_Value(tmp1_tn);
            check_tninfo = tmp2_tninfo;
            indx_tn = tmp2_tn;
            keep_going = TRUE;
          } else if (!TN_is_symbol(tmp2_tn) && TN_Is_Constant(tmp2_tn)) {
            offset += TN_Value(tmp2_tn);
            check_tninfo = tmp1_tninfo;
            indx_tn = tmp1_tn;
            keep_going = TRUE;
          }
        }

      } else if (EBO_Trace_Data_Flow) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"%sNon-simple expression in symbolic compare opnd[1] ",EBO_trace_pfx);
        Print_OP_No_SrcLine(input_op);
      }
    }
  }

  *opnd_tninfo = check_tninfo;
  *opnd_tn = indx_tn;
  *opnd_offset = offset;
}



static
BOOL
condition_optimization(OP *op,
                       TN **opnd_tn,
                       EBO_TN_INFO **opnd_tninfo)
{
  INT op1_idx = TOP_Find_Operand_Use(OP_code(op),OU_opnd1);
  INT op2_idx = TOP_Find_Operand_Use(OP_code(op),OU_opnd2);
  TN *op1 = opnd_tn[op1_idx];
  TN *op2 = opnd_tn[op2_idx];

  EBO_TN_INFO *op1_tninfo = opnd_tninfo[op1_idx];
  EBO_TN_INFO *op2_tninfo = opnd_tninfo[op2_idx];

  INT64 op1_offset = TN_Is_Constant(op1) ? TN_value(op1) : 0;
  INT64 op2_offset = TN_Is_Constant(op2) ? TN_value(op2) : 0;

/* Can the result of this comparison be predicted? */
  if ((op1 == op2) && (op1 != Zero_TN)) {
    OPS ops = OPS_EMPTY;

    TOP opcode = OP_code(op);
    TN *r0 = OP_result(op,0);
    TN *r1 = OP_result(op,1);
    TN *prd = OP_opnd(op,OP_PREDICATE_OPND);

   /* Replace the original operands with constants and allow
      it to be resolved by constant expression evaluation. */
    if (r0 != True_TN) {
      Build_OP (opcode, r0, True_TN, prd, Zero_TN, Zero_TN, &ops);
      OP_srcpos(OPS_last(&ops)) = OP_srcpos(op);
      if (OP_glue(op)) Set_OP_glue(OPS_first(&ops));
    }
    if (r1 != True_TN) {
      Build_OP (opcode, True_TN, r1, prd, Zero_TN, Zero_TN, &ops);
      OP_srcpos(OPS_last(&ops)) = OP_srcpos(op);
      if (OP_glue(op)) Set_OP_glue(OPS_first(&ops));
    }
    if (EBO_in_loop) EBO_OPS_omega (&ops, opnd_tninfo[OP_PREDICATE_OPND]);
    BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);

    if (EBO_Trace_Optimization) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile,"%sResolve compare symbolically.\n",EBO_trace_pfx);
    }

    return TRUE;
  }

  EBO_Identify_Base_and_Offset ( &op1_tninfo, &op1, &op1_offset);
  EBO_Identify_Base_and_Offset ( &op2_tninfo, &op2, &op2_offset);

  if ((op1_tninfo == op2_tninfo) && (op1_tninfo != NULL) && (op1 != Zero_TN)) {
    TOP opcode = OP_code(op);
    if ( opcode == TOP_cmp_geu ||
	 opcode == TOP_cmp_geu_unc ||
	 opcode == TOP_cmp_ltu ||
	 opcode == TOP_cmp_ltu_unc ||
	 opcode == TOP_cmp_gtu ||
	 opcode == TOP_cmp_gtu_unc ||
	 opcode == TOP_cmp_leu ||
	 opcode == TOP_cmp_leu_unc ||
	 opcode == TOP_cmp_i_geu ||
	 opcode == TOP_cmp_i_geu_unc ||
	 opcode == TOP_cmp_i_ltu ||
	 opcode == TOP_cmp_i_ltu_unc ||
	 opcode == TOP_cmp_i_gtu ||
	 opcode == TOP_cmp_i_gtu_unc ||
	 opcode == TOP_cmp_i_leu ||
	 opcode == TOP_cmp_i_leu_unc) {
      return FALSE;
    }
    if (EBO_Trace_Optimization) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile,"%sResolve compare symbolically by reducing to constants %lld : %lld.\n",
              EBO_trace_pfx,op1_offset,op2_offset);
    }

    TN *new_tn[OP_MAX_FIXED_OPNDS];
    TN **new_opnd_tn = &new_tn[0];
    EBO_TN_INFO *new_tninfo[OP_MAX_FIXED_OPNDS];
    EBO_TN_INFO **new_opnd_tninfo = &new_tninfo[0];
    for (INT i = 0; i < OP_opnds(op); i++) {
      new_tn[i] = NULL;
      new_tninfo[i] = NULL;
    }
    new_tn[OP_PREDICATE_OPND] = OP_opnd(op,OP_PREDICATE_OPND);
    new_tninfo[OP_PREDICATE_OPND] = opnd_tninfo[OP_PREDICATE_OPND];
    new_tn[op1_idx] = Gen_Literal_TN(op1_offset, 8);
    new_tn[op2_idx] = Gen_Literal_TN(op2_offset, 8);
    return Fold_Constant_Expression (op, new_opnd_tn, new_opnd_tninfo);

    return TRUE;
  }

  TOP opcode = OP_code(op);
  BB *bb = OP_bb(op);

  if ((opcode == TOP_cmp_eq) ||
      (opcode == TOP_cmp_eq_unc) ||
      (opcode == TOP_cmp_ne) ||
      (opcode == TOP_cmp_ne_unc) ||
      (opcode == TOP_cmp_ge) ||
      (opcode == TOP_cmp_ge_unc) ||
      (opcode == TOP_cmp_geu) ||
      (opcode == TOP_cmp_geu_unc) ||
      (opcode == TOP_cmp_lt) ||
      (opcode == TOP_cmp_lt_unc) ||
      (opcode == TOP_cmp_ltu) ||
      (opcode == TOP_cmp_ltu_unc) ||
      (opcode == TOP_cmp_gt) ||
      (opcode == TOP_cmp_gt_unc) ||
      (opcode == TOP_cmp_gtu) ||
      (opcode == TOP_cmp_gtu_unc) ||
      (opcode == TOP_cmp_le) ||
      (opcode == TOP_cmp_le_unc) ||
      (opcode == TOP_cmp_leu) ||
      (opcode == TOP_cmp_leu_unc) ||
      (opcode == TOP_cmp_i_eq) ||
      (opcode == TOP_cmp_i_eq_unc) ||
      (opcode == TOP_cmp_i_ne) ||
      (opcode == TOP_cmp_i_ne_unc) ||
      (opcode == TOP_cmp_i_ge) ||
      (opcode == TOP_cmp_i_ge_unc) ||
      (opcode == TOP_cmp_i_geu) ||
      (opcode == TOP_cmp_i_geu_unc) ||
      (opcode == TOP_cmp_i_lt) ||
      (opcode == TOP_cmp_i_lt_unc) ||
      (opcode == TOP_cmp_i_ltu) ||
      (opcode == TOP_cmp_i_ltu_unc) ||
      (opcode == TOP_cmp_i_gt) ||
      (opcode == TOP_cmp_i_gt_unc) ||
      (opcode == TOP_cmp_i_gtu) ||
      (opcode == TOP_cmp_i_gtu_unc) ||
      (opcode == TOP_cmp_i_le) ||
      (opcode == TOP_cmp_i_le_unc) ||
      (opcode == TOP_cmp_i_leu) ||
      (opcode == TOP_cmp_i_leu_unc)) {

   /* Level 2 data: */
    OP *l2_op0;
    TOP l2_opcode0 = TOP_UNDEFINED;
    TN *l2_tn0 = opnd_tn[1];
    INT64 l2_val0;
    EBO_TN_INFO *l2_tninfo0 = opnd_tninfo[1];
    EBO_OP_INFO *l2_opinfo0;
    OP *l2_op1;
    TOP l2_opcode1 = TOP_UNDEFINED;
    TN *l2_tn1 = opnd_tn[2];
    INT64 l2_val1;
    EBO_TN_INFO *l2_tninfo1 = opnd_tninfo[2];
    EBO_OP_INFO *l2_opinfo1;

   /* Level 3 data: */
    TOP new_opcode;
    TN *l3_tn0;
    EBO_TN_INFO *l3_tninfo0;
    TN *l3_tn1;
    EBO_TN_INFO *l3_tninfo1;

    if ((l2_tninfo1 != NULL) &&
        (l2_tninfo1->in_op != NULL)) {
      l2_op1 = l2_tninfo1->in_op;
      l2_opcode1 = OP_code(l2_op1);

      if ((l2_opcode1 != TOP_sxt4) &&
          (l2_opcode1 != TOP_zxt4)) return FALSE;

      l2_opinfo1 = locate_opinfo_entry (l2_tninfo1);
      if (l2_opinfo1 == NULL) return FALSE;
      if (l2_opinfo1->actual_opnd[1] == NULL) return FALSE;
      if (!EBO_tn_available(bb, l2_opinfo1->actual_opnd[1])) return FALSE;

      l3_tn1 = OP_opnd(l2_op1,1);
      l3_tninfo1 = l2_opinfo1->actual_opnd[1];
    } else if (TN_Is_Constant(l2_tn1)) {
      l2_val1 = TN_Value(l2_tn1);
      l3_tn1 = l2_tn1;
      l3_tninfo1 = NULL;
    } else return FALSE;

    if ((l2_tninfo0 != NULL) &&
        (l2_tninfo0->in_op != NULL)) {
      l2_op0 = l2_tninfo0->in_op;
      l2_opcode0 = OP_code(l2_op0);

      if ((l2_opcode0 != TOP_sxt4) &&
          (l2_opcode0 != TOP_zxt4)) return FALSE;

      l2_opinfo0 = locate_opinfo_entry (l2_tninfo0);
      if (l2_opinfo0 == NULL) return FALSE;
      if (l2_opinfo0->actual_opnd[1] == NULL) return FALSE;
      if (!EBO_tn_available(bb, l2_opinfo0->actual_opnd[1])) return FALSE;

      l3_tn0 = OP_opnd(l2_op0,1);
      l3_tninfo0 = l2_opinfo0->actual_opnd[1];
    } else if (TN_Is_Constant(l2_tn0)) {
      l2_val0 = TN_Value(l2_tn0);
      l3_tn0 = l2_tn0;
      l3_tninfo0 = NULL;
    } else return FALSE;

   /* Both constants? The OP should have been evaluated. */
    if (TN_Is_Constant(l3_tn0) && TN_Is_Constant(l3_tn1)) return FALSE;

   /* Conversion instructions not the same? One input must be a constant. */
    if ((l2_op0 != l2_op1) &&
        (!TN_Is_Constant(l3_tn0)) &&
        (!TN_Is_Constant(l3_tn1))) return FALSE;

   /* One input a constant? It's sign must agree with the conversion OP. */
    if (TN_Is_Constant(l3_tn0)) {
      if ((l2_opcode1 == TOP_sxt4) &&
          (SEXT_32(l2_val0) != l2_val0)) return FALSE;
      if ((l2_opcode1 == TOP_zxt4) &&
          (TRUNC_32(l2_val0) != l2_val0)) return FALSE;
    }

    if (TN_Is_Constant(l3_tn1)) {
      if ((l2_opcode0 == TOP_sxt4) &&
          (SEXT_32(l2_val1) != l2_val1)) return FALSE;
      if ((l2_opcode0 == TOP_zxt4) &&
          (TRUNC_32(l2_val1) != l2_val1)) return FALSE;
    }

   /* Recreate the compare using 32 bit inputs. */
    switch (opcode) {
    case TOP_cmp_eq: new_opcode = TOP_cmp4_eq; break;
    case TOP_cmp_eq_unc: new_opcode = TOP_cmp4_eq_unc; break;
    case TOP_cmp_ne: new_opcode = TOP_cmp4_ne; break;
    case TOP_cmp_ne_unc: new_opcode = TOP_cmp4_ne_unc; break;
    case TOP_cmp_ge: new_opcode = TOP_cmp4_ge; break;
    case TOP_cmp_ge_unc: new_opcode = TOP_cmp4_ge_unc; break;
    case TOP_cmp_geu: new_opcode = TOP_cmp4_geu; break;
    case TOP_cmp_geu_unc: new_opcode = TOP_cmp4_geu_unc; break;
    case TOP_cmp_lt: new_opcode = TOP_cmp4_lt; break;
    case TOP_cmp_lt_unc: new_opcode = TOP_cmp4_lt_unc; break;
    case TOP_cmp_ltu: new_opcode = TOP_cmp4_ltu; break;
    case TOP_cmp_ltu_unc: new_opcode = TOP_cmp4_ltu_unc; break;
    case TOP_cmp_gt: new_opcode = TOP_cmp4_gt; break;
    case TOP_cmp_gt_unc: new_opcode = TOP_cmp4_gt_unc; break;
    case TOP_cmp_gtu: new_opcode = TOP_cmp4_gtu; break;
    case TOP_cmp_gtu_unc: new_opcode = TOP_cmp4_gtu_unc; break;
    case TOP_cmp_le: new_opcode = TOP_cmp4_le; break;
    case TOP_cmp_le_unc: new_opcode = TOP_cmp4_le_unc; break;
    case TOP_cmp_leu: new_opcode = TOP_cmp4_leu; break;
    case TOP_cmp_leu_unc: new_opcode = TOP_cmp4_leu_unc; break;
    case TOP_cmp_i_eq: new_opcode = TOP_cmp4_i_eq; break;
    case TOP_cmp_i_eq_unc: new_opcode = TOP_cmp4_i_eq_unc; break;
    case TOP_cmp_i_ne: new_opcode = TOP_cmp4_i_ne; break;
    case TOP_cmp_i_ne_unc: new_opcode = TOP_cmp4_i_ne_unc; break;
    case TOP_cmp_i_ge: new_opcode = TOP_cmp4_i_ge; break;
    case TOP_cmp_i_ge_unc: new_opcode = TOP_cmp4_i_ge_unc; break;
    case TOP_cmp_i_geu: new_opcode = TOP_cmp4_i_geu; break;
    case TOP_cmp_i_geu_unc: new_opcode = TOP_cmp4_i_geu_unc; break;
    case TOP_cmp_i_lt: new_opcode = TOP_cmp4_i_lt; break;
    case TOP_cmp_i_lt_unc: new_opcode = TOP_cmp4_i_lt_unc; break;
    case TOP_cmp_i_ltu: new_opcode = TOP_cmp4_i_ltu; break;
    case TOP_cmp_i_ltu_unc: new_opcode = TOP_cmp4_i_ltu_unc; break;
    case TOP_cmp_i_gt: new_opcode = TOP_cmp4_i_gt; break;
    case TOP_cmp_i_gt_unc: new_opcode = TOP_cmp4_i_gt_unc; break;
    case TOP_cmp_i_gtu: new_opcode = TOP_cmp4_i_gtu; break;
    case TOP_cmp_i_gtu_unc: new_opcode = TOP_cmp4_i_gtu_unc; break;
    case TOP_cmp_i_le: new_opcode = TOP_cmp4_i_le; break;
    case TOP_cmp_i_le_unc: new_opcode = TOP_cmp4_i_le_unc; break;
    case TOP_cmp_i_leu: new_opcode = TOP_cmp4_i_leu; break;
    case TOP_cmp_i_leu_unc: new_opcode = TOP_cmp4_i_leu_unc; break;
    }

    OPS ops = OPS_EMPTY;
if (EBO_Trace_Optimization) fprintf(TFile,"replace 64 bit compare with 32 bit compare\n");
    Build_OP (new_opcode,
              OP_result(op,0), OP_result(op,1), OP_opnd(op,OP_PREDICATE_OPND),
              l3_tn0, l3_tn1, &ops);
    if (EBO_in_loop) EBO_Set_OP_omega (OPS_first(&ops), opnd_tninfo[OP_PREDICATE_OPND],
                                       l3_tninfo0, l3_tninfo1);
    OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
    BB_Insert_Ops(bb, op, &ops, FALSE);
    return TRUE;
  }

  return FALSE;
}

 


/*
 * Function: compare_bit
 *
 * Look for the pattern:
 *       CMP(zero,AND(integer0,(1-bit-constant)))
 * and turn it into:
 *       TBIT_Z(trailing_zero_count(1-bit-constant))
 *
 */
static
BOOL
compare_bit (OP *op,
             TN **opnd_tn,
             EBO_TN_INFO **opnd_tninfo)
{
 /* Level 1 data: */
  TOP opcode = OP_code(op);
  BB *bb = OP_bb(op);
  OP *l1_op0 = op;

 /* Level 2 data: */
  TN *l2_tn0;
  INT64 l2_val0;
  OP *l2_op1;
  TN *l2_tn1;
  EBO_TN_INFO *l2_tninfo1;
  EBO_OP_INFO *l2_opinfo1;

 /* Level 3 data: */
  OP *l3_op0;
  TN *l3_tn0;
  EBO_TN_INFO *l3_tninfo0;
  EBO_OP_INFO *l3_opinfo0;
  TN *l3_tn1;
  EBO_TN_INFO *l3_tninfo1;
  EBO_OP_INFO *l3_opinfo1;
  INT64 l3_value;

 /* Scratch data */
  TN *save_tn;

 /* New instruction information. */
  TOP new_opcode = TOP_tbit_z;
  TN *r_true  = OP_result(op,0);
  TN *r_false = OP_result(op,1);
  TN *test_word;
  EBO_TN_INFO *test_word_tninfo;
  INT bit_number;

  l2_tn0 = OP_opnd(op,1);
  if ((l2_tn0 == NULL) || !TN_Is_Constant(l2_tn0)) return FALSE;
  l2_val0 = TN_Value(l2_tn0);
  if (l2_val0 != 0) {
    if (l2_val0 != 1) return FALSE;

   /* Reverse the results and keep zero-bit test. */
    save_tn = r_true;
    r_true = r_false;
    r_false = save_tn;
  }

  switch (opcode) {

    case TOP_cmp_i_eq_unc:
    case TOP_cmp4_i_eq_unc:
      new_opcode = TOP_tbit_z_unc;
    case TOP_cmp_i_eq:
    case TOP_cmp4_i_eq:
      break;

    case TOP_cmp_i_ne_unc:
    case TOP_cmp4_i_ne_unc:
      new_opcode = TOP_tbit_z_unc;
    case TOP_cmp_i_ne:
    case TOP_cmp4_i_ne:
      save_tn = r_true;
      r_true = r_false;
      r_false = save_tn;
      break;

    default: return FALSE;
  }

 /* Get the other operand of the comparison. */
  l2_tn1 = OP_opnd(l1_op0,2);
  l2_tninfo1 = opnd_tninfo[2];
  if ((l2_tninfo1 == NULL) || (l2_tninfo1->in_op == NULL)) return FALSE;
  l2_op1 = l2_tninfo1->in_op;

 /* How many differend ways to test a bit do we want to look for?
      AND(var,(1-bit_mask))
      AND(right_shift(var,count),"1")
      AND(extr(var,count,length),"1")
      right_shift(var,63)
      extr(var,bitnum)
      ???
 */

  if ((OP_code(l2_op1) == TOP_and) ||
      (OP_code(l2_op1) == TOP_and_i)) {

    l2_opinfo1 = locate_opinfo_entry (l2_tninfo1);
    if (l2_opinfo1 == NULL) {
      return FALSE;
    }

   /* The input to the AND must be a 1 bit constant. */
    if ((l2_opinfo1->optimal_opnd[1] != NULL) &&
        (l2_opinfo1->optimal_opnd[1]->replacement_tn != NULL) &&
        (TN_Is_Constant(l2_opinfo1->optimal_opnd[1]->replacement_tn))) {
      l3_tn0 = l2_opinfo1->optimal_opnd[1]->replacement_tn;
      l3_tninfo0 = NULL;
    } else {
      l3_tn0 = OP_opnd(l2_op1,1);
      l3_tninfo0 = l2_opinfo1->actual_opnd[1];
      if ((l3_tninfo0 != NULL) &&
          (l3_tninfo0->replacement_tn != NULL) &&
          (TN_Is_Constant(l3_tninfo0->replacement_tn))) {
        l3_tn0 = l3_tninfo0->replacement_tn;
        l3_tninfo0 = l3_tninfo0->replacement_tninfo;
      }
    }
    if ((l2_opinfo1->optimal_opnd[2] != NULL) &&
        (l2_opinfo1->optimal_opnd[2]->replacement_tn != NULL) &&
        (TN_Is_Constant(l2_opinfo1->optimal_opnd[2]->replacement_tn))) {
      l3_tn1 = l2_opinfo1->optimal_opnd[2]->replacement_tn;
      l3_tninfo1 = NULL;
    } else {
      l3_tn1 = OP_opnd(l2_op1,2);
      l3_tninfo1 = l2_opinfo1->actual_opnd[2];
      if ((l3_tninfo1 != NULL) &&
          (l3_tninfo1->replacement_tn != NULL) &&
          (TN_Is_Constant(l3_tninfo1->replacement_tn))) {
        l3_tn1 = l3_tninfo1->replacement_tn;
        l3_tninfo1 = l3_tninfo1->replacement_tninfo;
      }
    }

    if ((l3_tn0 != NULL) && TN_Is_Constant(l3_tn0)) {
      if ((l3_tninfo1 == NULL) ||
          TN_is_symbol(l3_tn0)) {
        return FALSE;
      }
      l3_value = TN_Value(l3_tn0);
      l3_tn0 = l3_tn1;
      l3_tninfo0 = l3_tninfo1;
    } else if ((l3_tn1 != NULL) && TN_Is_Constant(l3_tn1)) {
      if ((l3_tninfo0 == NULL) ||
          TN_is_symbol(l3_tn1)) {
        return FALSE;
      }
      l3_value = TN_Value(l3_tn1);
    } else {
      return FALSE;
    }

    if ((l3_value == 1) &&
        (l3_tninfo0 != NULL) &&
        (l3_tninfo0->in_op != NULL)) {
      OP *l3_op = l3_tninfo0->in_op;
      TOP l3_opcode = OP_code(l3_op);

      if ((l3_opcode == TOP_shr_i) ||
          (l3_opcode == TOP_shr_i_u)) {

        l3_opinfo1 = find_opinfo_entry (l3_op);
        if ((l3_opinfo1 != NULL) &&
            (l3_opinfo1->actual_opnd[1] != NULL) &&
            (EBO_tn_available(bb, l3_opinfo1->actual_opnd[1]))) {
          l3_tn0 = OP_opnd(l3_op, 1);
          l3_tninfo0 = l3_opinfo1->actual_opnd[1];
          l3_value = 1 << TN_Value(OP_opnd(l3_op, 2));
        }
      } else if ((l3_opcode == TOP_extr) ||
                 (l3_opcode == TOP_extr_u)) {
        l3_opinfo1 = find_opinfo_entry (l3_op);
        if ((l3_opinfo1 != NULL) &&
            (l3_opinfo1->actual_opnd[1] != NULL) &&
            (EBO_tn_available(bb, l3_opinfo1->actual_opnd[1])) &&
            (EBO_bit_length(l3_op) >= 1)) {
          l3_tn0 = OP_opnd(l3_op, 1);
          l3_tninfo0 = l3_opinfo1->actual_opnd[1];
          l3_value = 1 << TN_Value(OP_opnd(l3_op, 2));
        }
      }

    }

   /* Is the constant a power of 2? */
    if ((l3_value <= 0) || ((l3_value & (l3_value-1)) != 0)) {
      return FALSE;
    }

   /* Determine the power of 2 that we are dealing with. */
    INT64 one= 1;
    for (bit_number = 0; bit_number < 64; bit_number++)
    {
      if ((one << bit_number) == l3_value)
        break;
    }

    test_word = l3_tn0;
    test_word_tninfo = l3_tninfo0;
  } else if ((OP_code(l2_op1) == TOP_shr_i) &&
             (TN_Is_Constant(OP_opnd(l2_op1,2))) &&
             (TN_Value(OP_opnd(l2_op1,2)) == 63)) {

    l2_opinfo1 = locate_opinfo_entry (l2_tninfo1);
    if (l2_opinfo1 == NULL) return FALSE;
    if (l2_opinfo1->actual_opnd[1] == NULL) return FALSE;

   /* Set up to test the left most bit. */
    bit_number = 63;
    test_word = OP_opnd(l2_op1,1);
    test_word_tninfo = l2_opinfo1->actual_opnd[1];

   /* When the input to the shift-right is a shift-left,
      we may be able to do even better. */
    if ((test_word_tninfo != NULL) &&
        (test_word_tninfo->in_op != NULL) &&
        (OP_code(test_word_tninfo->in_op) == TOP_shl_i) &&
        (TN_Is_Constant(OP_opnd(test_word_tninfo->in_op,2)))) {
      l3_op0 = test_word_tninfo->in_op;
      l3_opinfo0 = locate_opinfo_entry (test_word_tninfo);
      if (l3_opinfo0 != NULL) {
       /* Use the inputs to the shift-left. */
        bit_number = 63 - TN_Value(OP_opnd(l3_op0,2));
        test_word = OP_opnd(l3_op0,1);
        test_word_tninfo = l3_opinfo0->actual_opnd[1];
      }
    }

  } else if (((OP_code(l2_op1) == TOP_extr) ||
              (OP_code(l2_op1) == TOP_extr_u)) &&
             (EBO_bit_length(l2_op1) == 1) &&
             (TN_Is_Constant(OP_opnd(l2_op1,2)))) {

    l2_opinfo1 = locate_opinfo_entry (l2_tninfo1);
    if (l2_opinfo1 == NULL) return FALSE;
    if (l2_opinfo1->actual_opnd[1] == NULL) return FALSE;

    bit_number = TN_Value(OP_opnd(l2_op1,2));
    test_word = OP_opnd(l2_op1,1);
    test_word_tninfo = l2_opinfo1->actual_opnd[1];
  } else {
    return FALSE;
  }

 /* Is the input to the test a field reference instruction? */
  if ((test_word_tninfo->in_op != NULL) &&
      ((OP_code(test_word_tninfo->in_op) == TOP_sxt1) ||
       (OP_code(test_word_tninfo->in_op) == TOP_sxt2) ||
       (OP_code(test_word_tninfo->in_op) == TOP_sxt4) ||
       (OP_code(test_word_tninfo->in_op) == TOP_zxt1) ||
       (OP_code(test_word_tninfo->in_op) == TOP_zxt2) ||
       (OP_code(test_word_tninfo->in_op) == TOP_zxt4))) {
    INT l3_length;
    l3_op0 = test_word_tninfo->in_op;
    l3_length = EBO_bit_length(l3_op0);
    l3_opinfo1 = locate_opinfo_entry (test_word_tninfo);

    if ((l3_opinfo1 != NULL) &&
        (l3_opinfo1->actual_opnd[1] != NULL) &&
        (EBO_tn_available (bb, l3_opinfo1->actual_opnd[1]))) {
      if (bit_number < l3_length) {
        test_word = OP_opnd(l3_op0,1);
        test_word_tninfo = l3_opinfo1->actual_opnd[1];
      } else if ((OP_code(test_word_tninfo->in_op) == TOP_sxt1) ||
                 (OP_code(test_word_tninfo->in_op) == TOP_sxt2) ||
                 (OP_code(test_word_tninfo->in_op) == TOP_sxt4)) {
        bit_number = l3_length -1;
        test_word = OP_opnd(l3_op0,1);
        test_word_tninfo = l3_opinfo1->actual_opnd[1];
      } else if ((OP_code(test_word_tninfo->in_op) == TOP_zxt1) ||
                 (OP_code(test_word_tninfo->in_op) == TOP_zxt2) ||
                 (OP_code(test_word_tninfo->in_op) == TOP_zxt4)) {
        bit_number = 0;
        test_word = Zero_TN;
        test_word_tninfo = NULL;
      }
    }
  }

 /* Would the word that contains the bit be available for use? */
  if ((test_word_tninfo != NULL) &&
      (!EBO_tn_available (bb, test_word_tninfo))) {
    return FALSE;
  }

 /* Create the test-bit instruction. */
  OPS ops = OPS_EMPTY;
  Build_OP (new_opcode, r_true, r_false, OP_opnd(op,OP_PREDICATE_OPND),
            test_word, Gen_Literal_TN(bit_number, 4), &ops);
  OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
  if (EBO_in_loop) EBO_Set_OP_omega (OPS_first(&ops), opnd_tninfo[OP_PREDICATE_OPND],
                                     test_word_tninfo, NULL);
  BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
if (EBO_Trace_Optimization) fprintf(TFile,"Test_Bit created\n");
  return TRUE;
}




static
BOOL
copy_simplification (OP *op,
                     TN **opnd_tn,
                     EBO_TN_INFO **opnd_tninfo)
{
  INT cpix = EBO_Copy_Operand(op);
  INT prdix = OP_has_predicate(op) ? OP_PREDICATE_OPND : -1;
  TN *tnr;
  TN *current_tn;
  TN *optimal_tn;
  TN *predicate_tn;
  INT64 val;
  EBO_TN_INFO *tninfo;

  if (cpix < 0) return FALSE;
  if (prdix < 0) return FALSE;
  if (OP_results(op) != 1) return FALSE;

  tnr = OP_result(op,0);
  current_tn = OP_opnd(op,cpix);
  optimal_tn = opnd_tn[cpix];
  predicate_tn = opnd_tn[prdix];
  tninfo = opnd_tninfo[cpix];


  if ( (tninfo != NULL) &&
       (tninfo->omega == 0) &&
       (tninfo->in_bb != NULL) &&
       (tnr == current_tn) &&
        !has_assigned_reg(tnr) &&
        !TN_is_save_reg(tnr) &&
        !TN_Is_Constant(current_tn) &&
        !has_assigned_reg(current_tn) &&
        !TN_is_save_reg(current_tn) ) {
   /* Copies to and from the same TN are not needed. */
if (EBO_Trace_Optimization) fprintf(TFile,"Copy to self is not needed\n");
    if (!TN_is_global_reg(current_tn) && (OP_bb(op) != tninfo->in_bb)) {
      mark_tn_live_into_BB (current_tn, OP_bb(op), tninfo->in_bb);
    }
    return TRUE;
  }

  if ((TN_register_class(tnr) == ISA_REGISTER_CLASS_integer) &&
      (TN_has_value(optimal_tn)) &&
      (TN_value(optimal_tn) == 0)) {
    OP *new_op;
    new_op = Mk_OP(TOP_mov, tnr, predicate_tn, Zero_TN);
    OP_srcpos(new_op) = OP_srcpos(op);
    if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND], NULL );
    BB_Insert_Op_After(OP_bb(op), op, new_op);
if (EBO_Trace_Optimization) fprintf(TFile,"Use Zero_TN in place of immediate constant\n");
    return TRUE;
  }

 /* Look for special case merge that is really and ABS function. */
 /* The pattern that we are looking for is:
     TN715 TN716 :- fcmp.lt.unc TN257(p0) (enum:.s0) TN713 TN129(f0) ;
     GTN717 :- fneg TN716 TN713 ; cond_def
     GTN717 :- mov_f TN715 TN713 ; copy cond_def
 */
  EBO_TN_INFO *predicate_tninfo = opnd_tninfo[OP_PREDICATE_OPND];
  if ((predicate_tninfo != NULL) &&
      (predicate_tninfo->in_op != NULL) &&
      (!TN_Is_Constant(predicate_tn))) {
   /* The current result is conditionally defined.  Can we find a previous definition? */
    INT op1_idx = 1;
    TN *tnr = OP_result(op,0);
    EBO_TN_INFO *previous_tninfo = get_tn_info(tnr);
    EBO_OP_INFO *previous_opinfo = NULL;
    EBO_TN_INFO *previous_op0_tninfo = NULL;
    TOP previous_opcode = TOP_noop;
    if ((previous_tninfo != NULL)  &&
        (previous_tninfo->in_op != NULL) &&
        (previous_tninfo->reference_count == 0) &&
        (previous_tninfo->predicate_tninfo != NULL)) {
      previous_opinfo = locate_opinfo_entry (previous_tninfo);
      if ((previous_opinfo != NULL) &&
          (previous_opinfo->in_op != NULL)) {
        previous_opcode = OP_code(previous_opinfo->in_op);
        previous_op0_tninfo = previous_opinfo->optimal_opnd[op1_idx];
      }
    }
    if ((previous_op0_tninfo != NULL) &&
        (previous_opcode == TOP_fneg) &&
        (opnd_tn[op1_idx] == previous_op0_tninfo->local_tn) &&
        EBO_predicate_complements (predicate_tn, predicate_tninfo,
                                   previous_tninfo->predicate_tninfo->local_tn, previous_tninfo->predicate_tninfo)) {
      OP *predicate_op = predicate_tninfo->in_op;
      TOP predicate_opcode = OP_code(predicate_op);
      EBO_OP_INFO *predicate_opinfo = locate_opinfo_entry (predicate_tninfo);
      if ((predicate_opinfo != NULL) &&
          (predicate_opinfo->in_op != NULL) &&
          EBO_tn_available(OP_bb(op), predicate_opinfo->actual_opnd[OP_PREDICATE_OPND]) &&
          (((predicate_opcode == TOP_fcmp_lt_unc) || (predicate_opcode == TOP_fcmp_le_unc)) &&
           (predicate_opinfo->actual_rslt[1] == predicate_tninfo) &&
           (OP_opnd(predicate_op, TOP_Find_Operand_Use(predicate_opcode,OU_opnd2)) == FZero_TN) &&
           (predicate_opinfo->optimal_opnd[TOP_Find_Operand_Use(predicate_opcode,OU_opnd1)] == opnd_tninfo[op1_idx]))) {
if (EBO_Trace_Optimization) fprintf(TFile,"simplify fneg(fcmp()) operation.\n");
        BB *bb = OP_bb(op);
        TOP new_opcode = TOP_fmerge_s;
        OP *new_op = Mk_OP (new_opcode, tnr,
                            OP_opnd(predicate_op,OP_PREDICATE_OPND), FZero_TN, OP_opnd(op,1));
        OP_srcpos(new_op) = OP_srcpos(op);
        if (EBO_in_loop) EBO_Set_OP_omega ( new_op,
                                            predicate_opinfo->actual_opnd[OP_PREDICATE_OPND],
                                            opnd_tninfo[op1_idx]);
        BB_Insert_Op_After(bb, op, new_op);

       /* Now get rid of the previous fneg instruction. */
        remove_op (previous_opinfo);
        OP_Change_To_Noop(previous_opinfo->in_op);
        previous_opinfo->in_op = NULL;
        previous_opinfo->in_bb = NULL;
        return TRUE;
      }
    } else if ((previous_tninfo != NULL) &&
               (previous_tninfo->predicate_tninfo != NULL) &&
               (previous_opinfo != NULL) &&
               (previous_opinfo->in_op != NULL) &&
               OP_effectively_copy(previous_opinfo->in_op) &&
               EBO_predicate_complements (predicate_tn, predicate_tninfo,
                                   previous_tninfo->predicate_tninfo->local_tn, previous_tninfo->predicate_tninfo) &&
               (((previous_op0_tninfo != NULL) &&
                 (opnd_tn[op1_idx] == previous_op0_tninfo->local_tn)) ||
                (TN_Is_Constant(current_tn) &&
                 (current_tn == OP_opnd(previous_opinfo->in_op,EBO_Copy_Operand(previous_opinfo->in_op)))))) {
      OP *predicate_op = predicate_tninfo->in_op;
      TOP predicate_opcode = OP_code(predicate_op);
      EBO_OP_INFO *predicate_opinfo = locate_opinfo_entry (predicate_tninfo);
      if ((predicate_opinfo != NULL) &&
          (predicate_opinfo->in_op != NULL) &&
          OP_icmp(predicate_opinfo->in_op) &&
          EBO_tn_available(OP_bb(op), predicate_opinfo->actual_opnd[OP_PREDICATE_OPND]) &&
          TNs_Are_Equivalent(current_tn, OP_opnd(previous_opinfo->in_op,EBO_Copy_Operand(previous_opinfo->in_op)))) {
if (EBO_Trace_Optimization) fprintf(TFile,"complementary moves of the same value\n");
        OPS ops = OPS_EMPTY;
        TN * predicate_tn = OP_opnd(predicate_opinfo->in_op,OP_PREDICATE_OPND);
        EBO_Exp_COPY (predicate_tn,
                      OP_result(op, 0),
                      current_tn,
                      &ops);
        OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
        if (EBO_in_loop) EBO_Set_OP_omega (OPS_first(&ops), predicate_opinfo->actual_opnd[OP_PREDICATE_OPND],
                                           previous_opinfo->actual_opnd[EBO_Copy_Operand(previous_opinfo->in_op)]);
        if (predicate_tn != True_TN) Set_OP_cond_def_kind(OPS_last(&ops), OP_PREDICATED_DEF);
        BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
        return TRUE;
      }
    }
  }

  if (current_tn == optimal_tn) return FALSE;
  if (TN_is_const_reg(current_tn)) return FALSE;
  if ((predicate_tn == True_TN) ||
      (predicate_tn == Zero_TN) ||
      TN_is_constant(predicate_tn)) return FALSE;
  if (TN_register_class(tnr) != ISA_REGISTER_CLASS_integer) return FALSE;
  if (!TN_has_value(optimal_tn)) return FALSE;

 /* Generate a move-immediate */
  val = TN_value(optimal_tn);
  OP *new_op;
  TOP new_opcode = (ISA_LC_Value_In_Class (val, LC_i22)) ? TOP_mov_i : TOP_movl;
  new_op = Mk_OP(new_opcode, tnr, predicate_tn, optimal_tn);
  OP_srcpos(new_op) = OP_srcpos(op);
  if (EBO_in_loop) EBO_Set_OP_omega ( new_op, opnd_tninfo[OP_PREDICATE_OPND], NULL );
  BB_Insert_Op_After(OP_bb(op), op, new_op);
if (EBO_Trace_Optimization) fprintf(TFile,"Conditionally load an immediate constant\n");
  return TRUE;
}




/*
 *	It may be possible to to recognize the specific registers
 *	that are involved with certain instructions that would
 *	need to be treated as "barriers" to EBO optimization.
 *	If this can be done, a use or definition can be created
 *	in the TN_INFO_TABLE that will restrict forward propagation
 *	and other hazardous optimizations without stopping
 *	attempts to optimized all the other OPs in the block.
 *
 *	Forward propagation of specific registers is prevented by
 *	creating a new EBO_TN_INFO entry that acts as a dummy
 *	defintion.
 *
 *	Note that the use count of pre-existing EBO_TN_INFO
 *	entries is incremented.  This is done to assure that the
 *	previous definition of the register is preserved, since
 *	there is some ambiguity as to whether or not the register
 *	is modified/written by the group reference.
 */
BOOL Process_Group_Register_Reference (OP *op)
{
  REGISTER i;
  REGISTER first;
  REGISTER last;
  TN *pred_tn;

  switch (OP_code(op)) {
  case TOP_clrrrb:
    first = REGISTER_First_Rotating_Registers(ISA_REGISTER_CLASS_integer);
    last  = REGISTER_Last_Rotating_Registers (ISA_REGISTER_CLASS_integer);

    for (i=first; i<=last; i++) {
      TN *tn = Build_Dedicated_TN(ISA_REGISTER_CLASS_integer,i,0);
      EBO_TN_INFO *tninfo = get_tn_info(tn);
      if (tninfo != NULL) {
if(EBO_Trace_Optimization)fprintf(TFile,"TOP_clrrrb r%d\n",i);
        EBO_TN_INFO *new_tninfo = tn_info_def (OP_bb(op), op, tn, True_TN, NULL);
        inc_ref_count(tninfo);
      }
    }

    first = REGISTER_First_Rotating_Registers(ISA_REGISTER_CLASS_float);
    last  = REGISTER_Last_Rotating_Registers (ISA_REGISTER_CLASS_float);

    for (i=first; i<=last; i++) {
      TN *tn = Build_Dedicated_TN(ISA_REGISTER_CLASS_float,i,0);
      EBO_TN_INFO *tninfo = get_tn_info(tn);
      if (tninfo != NULL) {
if(EBO_Trace_Optimization)fprintf(TFile,"TOP_clrrrb f%d\n",i);
        EBO_TN_INFO *new_tninfo = tn_info_def (OP_bb(op), op, tn, True_TN, NULL);
        inc_ref_count(tninfo);
      }
    }

   /* Also clear the predicate registers by falling through to the next case. */
  case TOP_clrrrb_pr:
    first = REGISTER_First_Rotating_Registers(ISA_REGISTER_CLASS_predicate);
    last  = REGISTER_Last_Rotating_Registers (ISA_REGISTER_CLASS_predicate);

    for (i=first; i<=last; i++) {
      TN *tn = Build_Dedicated_TN(ISA_REGISTER_CLASS_predicate,i,0);
      EBO_TN_INFO *tninfo = get_tn_info(tn);
      if (tninfo != NULL) {
if(EBO_Trace_Optimization)fprintf(TFile,"TOP_clrrrb pr%d\n",i);
        EBO_TN_INFO *new_tninfo = tn_info_def (OP_bb(op), op, tn, True_TN, NULL);
        inc_ref_count(tninfo);
      }
    }

    return TRUE;
  case TOP_mov_t_pr_i:
    first = REGISTER_First_Rotating_Registers(ISA_REGISTER_CLASS_predicate);
    last  = REGISTER_Last_Rotating_Registers (ISA_REGISTER_CLASS_predicate);
    pred_tn = OP_has_predicate(op)?OP_opnd(op,OP_PREDICATE_OPND):True_TN;

    for (i=first; i<=last; i++) {
      TN *tn = Build_Dedicated_TN(ISA_REGISTER_CLASS_predicate,i,0);
      EBO_TN_INFO *tninfo = get_tn_info(tn);
      if (tninfo != NULL) {
if(EBO_Trace_Optimization)fprintf(TFile,"TOP_mov_t_pr_i pr%d\n",i);
        EBO_TN_INFO *new_tninfo = tn_info_def (OP_bb(op), op, tn, pred_tn, NULL);
        inc_ref_count(tninfo);
      }
    }

    return TRUE;
  case TOP_mov_t_pr:
    first = REGISTER_MIN + 1;
    last  = REGISTER_CLASS_last_register (ISA_REGISTER_CLASS_predicate);
    pred_tn = OP_has_predicate(op)?OP_opnd(op,OP_PREDICATE_OPND):True_TN;

    for (i=first; i<=last; i++) {
      TN *tn = Build_Dedicated_TN(ISA_REGISTER_CLASS_predicate,i,0);
      EBO_TN_INFO *tninfo = get_tn_info(tn);
      if (tninfo != NULL) {
if(EBO_Trace_Optimization)fprintf(TFile,"TOP_mov_t_pr pr%d\n",i);
        EBO_TN_INFO *new_tninfo = tn_info_def (OP_bb(op), op, tn, pred_tn, NULL);
        inc_ref_count(tninfo);
      }
    }

    return TRUE;
  case TOP_mov_f_pr:
    first = REGISTER_MIN + 1;
    last  = REGISTER_CLASS_last_register (ISA_REGISTER_CLASS_predicate);
    pred_tn = OP_has_predicate(op)?OP_opnd(op,OP_PREDICATE_OPND):True_TN;

    for (i=first; i<=last; i++) {
      TN *tn = Build_Dedicated_TN(ISA_REGISTER_CLASS_predicate,i,0);
      EBO_TN_INFO *tninfo = get_tn_info(tn);
      if (tninfo != NULL) {
if(EBO_Trace_Optimization)fprintf(TFile,"TOP_mov_f_pr pr%d\n",i);
        inc_ref_count(tninfo); /* This is a use and must be kept around. */
      }
    }

    return TRUE;
  }

  return FALSE;
}
 
 



/*
 * Look at an expression and it's inputs to identify special sequences
 * that can be simplified.
 */
BOOL Special_Sequence (OP *op,
                       TN **opnd_tn,
                       EBO_TN_INFO **opnd_tninfo)
{
  TOP opcode = OP_code(op);

  if (OP_iadd(op)) {
    return (shl_add_sequence ( op, opnd_tn, opnd_tninfo) ||
            iadd_special_case( op, opnd_tn, opnd_tninfo));
  }
  if (OP_store(op)) {
    return (store_sequence( op, opnd_tn, opnd_tninfo) ||
            sxt_sequence  ( op, TOP_Find_Operand_Use(OP_code(op),OU_storeval),
                            opnd_tn, opnd_tninfo));
  }
  if (OP_fadd(op)) {
    return (fadd_fmult    (op, opnd_tninfo));
  }
  if (OP_fsub(op)) {
    return (fadd_fmult    (op, opnd_tninfo));
  }
  if ((opcode == TOP_fneg) ||
      (opcode == TOP_fpneg)) {
    return (fneg_sequence (op, opnd_tn, opnd_tninfo));
  }
  if (OP_icmp(op)) {
    return (condition_optimization (op, opnd_tn, opnd_tninfo) ||
            compare_bit (op, opnd_tn, opnd_tninfo) ||
            sxt_sequence (op, TOP_Find_Operand_Use(OP_code(op),OU_opnd1),
                          opnd_tn, opnd_tninfo) ||
            sxt_sequence (op, TOP_Find_Operand_Use(OP_code(op),OU_opnd2),
                          opnd_tn, opnd_tninfo));
  }
  if ((opcode == TOP_sxt1)    ||
      (opcode == TOP_sxt2)    ||
      (opcode == TOP_sxt4)    ||
      (opcode == TOP_zxt1)    ||
      (opcode == TOP_zxt2)    ||
      (opcode == TOP_zxt4)    ||
      (opcode == TOP_dep)     ||
      (opcode == TOP_dep_z)   ||
      (opcode == TOP_extr)    ||
      (opcode == TOP_extr_u)  ||
      (opcode == TOP_shr_i)   ||
      (opcode == TOP_shr_i_u)) {
    return (sxt_sequence  (op, 1, opnd_tn, opnd_tninfo));
  }
  if (opcode == TOP_and_i) {
    return (sxt_sequence (op, 2, opnd_tn, opnd_tninfo));
  }
  if ((opcode == TOP_setf_sig) ||
      (opcode == TOP_getf_sig)) {
    return (copy_rf_sequence (op, opnd_tn, opnd_tninfo));
  }
  if (OP_effectively_copy(op)) {
    return (copy_simplification (op, opnd_tn, opnd_tninfo));
  }
  if (OP_access_reg_bank(op)) {
    return Process_Group_Register_Reference (op);
  }
  return FALSE;
}
