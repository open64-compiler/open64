/*

  Copyright (C) 2000 Silicon Graphics, Inc.  All Rights Reserved.

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


/* ====================================================================
 * ====================================================================
 *
 * Module: cgtarget.cxx
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/ia64/cgtarget.cxx,v $
 *
 * Description:
 *
 * Support routines for target-specific code generator functionality.
 *
 * ====================================================================
 * ====================================================================
 */

#include <ctype.h>

#include "defs.h"
#include "util.h"
#include "config.h"
#include "config_targ_opt.h"
#include "erglob.h"
#include "tracing.h"
#include "data_layout.h"
#include "const.h"
#include "wn.h"
#include "opt_alias_interface.h"
#include "opt_alias_mgr.h"
#include "cgir.h"
#include "cg.h"
#include "targ_sim_core.h"
#include "void_list.h"
#include "cg_dep_graph.h"
#include "cg_spill.h"
#include "cg_vector.h"
#include "whirl2ops.h"
#include "ti_errors.h"
#include "ti_latency.h"
#include "targ_isa_lits.h"
#include "targ_isa_registers.h"
#include "w2op.h"
#include "cgexp.h"
#include "cg_loop_recur.h"
#include "targ_proc_properties.h"
#include "targ_isa_bundle.h"
#include "targ_isa_pack.h"
#include "targ_isa_enums.h"
#include "ti_bundle.h"
#include "hb_sched.h"
#include "hb_hazards.h"
#include "bb.h"
#include "op.h"
#include "op_list.h"
#include "cg_grouping.h"
#include "calls.h"
#include "cgtarget.h"
#include "ipfec_options.h"
#include "cache_analysis.h"
#include "stblock.h"        // for Base_Symbol_And_Offset_For_Addressing

UINT32 CGTARG_branch_taken_penalty;
BOOL CGTARG_branch_taken_penalty_overridden = FALSE;

TOP CGTARG_Invert_Table[TOP_count+1];
TOP CGTARG_Immed_To_Reg_Table[TOP_count+1];

OPCODE CGTARG_Assoc_Base_Opr_Table[TOP_count];
mTOP CGTARG_Assoc_Base_Top_Table[TOP_count];
mTOP CGTARG_Assoc_Base_Fnc_Table[TOP_count];

mTOP CGTARG_Inter_RegClass_Copy_Table[ISA_REGISTER_CLASS_MAX+1][ISA_REGISTER_CLASS_MAX+1][2];

SI_RESOURCE_ID CGTARG_f0_unit_id;

/* Trace flags: */
BOOL Trace_TD = FALSE;	/* Target-dependent prep trace */
BOOL Trace_Eager = FALSE; /* gcm used to set this... */
extern BOOL Trace_Call_Exp;	/* Trace call expansion, from cgexp */


UINT32 CGTARG_Mem_Ref_Bytes(const OP *memop)
/* -----------------------------------------------------------------------
 * Requires: OP_load(memop) || OP_store(memop)
 * See interface description.
 * -----------------------------------------------------------------------
 */
{
  FmtAssert(OP_load(memop) || OP_store(memop), ("not a load or store"));
  switch (OP_code(memop)) {
  case TOP_ld1_r:
  case TOP_ld1_i:
  case TOP_ld1:
  case TOP_st1:
  case TOP_st1_i:
    return 1;
  case TOP_ld2_r:
  case TOP_ld2_i:
  case TOP_ld2:
  case TOP_st2:
  case TOP_st2_i:
    return 2;
  case TOP_ld4_r:
  case TOP_ld4_i:
  case TOP_ldfs_r:
  case TOP_ldfs_i:
  case TOP_ld4:
  case TOP_ldfs:
  case TOP_st4:
  case TOP_st4_i:
  case TOP_stfs:
  case TOP_stfs_i:
    return 4;
  case TOP_ld8_r:
  case TOP_ld8_i:
  case TOP_ldfd_r:
  case TOP_ldfd_i:
  case TOP_ldf8_r:
  case TOP_ldf8_i:
  case TOP_ldfps:
  case TOP_ldfps_i:
  case TOP_ld8_r_fill:
  case TOP_ld8_i_fill:
  case TOP_ld8:
  case TOP_ld8_fill:
  case TOP_ldfd:
  case TOP_ldf8:
  case TOP_st8:
  case TOP_st8_i:
  case TOP_st8_spill:
  case TOP_st8_i_spill:
  case TOP_stfd:
  case TOP_stfd_i:
  case TOP_stf8:
  case TOP_stf8_i:
    return 8;
  case TOP_ldfe_r:
  case TOP_ldfe_i:
  case TOP_ldfe:
  case TOP_stfe:
  case TOP_stfe_i:
    return 10;
  case TOP_ldf_i_fill:
  case TOP_ldfpd:
  case TOP_ldfpd_i:
  case TOP_ldfp8:
  case TOP_ldfp8_i:
  case TOP_ldf_fill:
  case TOP_stf_spill:
  case TOP_stf_i_spill:
    return 16;
  default:
    FmtAssert(FALSE, ("unrecognized op (%s) in CGTARG_Mem_Ref_Bytes",
		      TOP_Name(OP_code(memop))));
  }
  /*NOTREACHED*/
}

/* ====================================================================
 *
 * CGTARG_Is_OP_Speculative
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL
CGTARG_Is_OP_Speculative(OP *op)
{
  if (!OP_load(op)) return FALSE;

  // speculative and advanced loads are safe to speculate.
  if (CGTARG_Is_OP_Advanced_Load(op) || CGTARG_Is_OP_Speculative_Load(op))
    return TRUE;

  return FALSE;
}

/* ====================================================================
 *
 * CGTARG_Perform_THR_Code_Generation
 *
 * See interface description
 *
 * ====================================================================
 */
void CGTARG_Perform_THR_Code_Generation (OP *load_op, OP *chk_load,
					 THR_TYPE type)
{
  if (type & THR_DATA_SPECULATION_NO_RB) {
    FmtAssert(OP_load(load_op), ("CGTARG_Perform_THR_Code_Generation : not a load OP"));
    INT enum_pos = -1;
    
    BOOL float_op = OP_Is_Float_Mem(load_op) ? TRUE : FALSE;
    CGTARG_Return_Enum_TN(load_op, (float_op) ? ECV_fldtype : ECV_ldtype, &enum_pos);

    FmtAssert(enum_pos != -1, ("OP doesn't have enum operand"));
    
    // Convert TOP_ld* to TOP_ld_a* (advanced load type)

    TN *enum1_tn = (float_op) ?  Gen_Enum_TN(ECV_fldtype_a) :
                                 Gen_Enum_TN(ECV_ldtype_a);
    Set_OP_opnd(load_op, enum_pos, enum1_tn);
    Set_OP_speculative(load_op);               // set the OP_speculative flag.
    
    // Add check load now.
    OP *check_load = Dup_OP(load_op);
    TN *enum2_tn;
    enum2_tn = (float_op) ? Gen_Enum_TN(ECV_fldtype_c_nc) : 
                            Gen_Enum_TN(ECV_ldtype_c_nc);
    Set_OP_opnd(check_load, enum_pos, enum2_tn); // set the enum TN, need to 
                                                 // add generic enum accessor.
    Set_OP_cond_def_kind(check_load, OP_ALWAYS_COND_DEF); 

    BB_Insert_Op_After(OP_bb(load_op), load_op, check_load);
  } else if (type & THR_DATA_SPECULATION_NO_RB_CLEANUP) {
    
    INT check_base_idx = TOP_Find_Operand_Use(OP_code(load_op), OU_base);
    TN *check_base_tn = OP_opnd(load_op, check_base_idx);
    TN *adv_load_base_tn = OP_opnd(chk_load, check_base_idx);
    
    TN *check_result_tn = OP_result(load_op, 0);
    TN *adv_load_result_tn = OP_result(chk_load, 0);

    // Check if the base and result TNs are matching.

    if (TNs_Are_Equivalent(check_result_tn, adv_load_result_tn) &&
	TNs_Are_Equivalent(check_base_tn, adv_load_base_tn)) {
      BB_Remove_Op(OP_bb(load_op), load_op);
      
      INT enum_pos = -1;
      BOOL float_op = OP_Is_Float_Mem(chk_load) ? TRUE : FALSE;
      TN *enum_tn = 
	CGTARG_Return_Enum_TN(chk_load,  
			      (float_op) ? ECV_fldtype_c_nc : ECV_ldtype_c_nc, 
			      &enum_pos); 

      FmtAssert(enum_tn, ("OP doesn't have enum operand"));
      Set_TN_enum(enum_tn, (float_op) ? ECV_fldtype : ECV_ldtype);   
    }
  }
}

/* ====================================================================
 *
 * CGTARG_ARC_Sched_Latency
 *
 * See interface description
 *
 * ====================================================================
 */
INT CGTARG_ARC_Sched_Latency(
  ARC *arc
)
{
  if ( ARC_kind(arc) == CG_DEP_PREBR && PROC_has_same_cycle_branch_shadow() )
    return 0;
  else
    return ARC_latency(arc);
}


/* ====================================================================
 *
 * CGTARG_Bundle_Slot_Available
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL 
CGTARG_Bundle_Slot_Available(TI_BUNDLE              *bundle,
			     OP                     *op,
			     INT                     slot,
			     ISA_EXEC_UNIT_PROPERTY *prop, 
			     BOOL                    stop_bit_reqd,
			     const CG_GROUPING      *grouping)
{

  // If slot already filled, return FALSE.
  if (TI_BUNDLE_slot_filled(bundle, slot)) return FALSE;

  INT  inst_words = ISA_PACK_Inst_Words(OP_code(op));

  // If an <op> can execute in either I- or M- unit, set the property
  // accordingly. If <slot==0> execute in M-unit else I-unit. The
  // reason being that there are more M-units in slot0 and vice-versa.

  if (EXEC_PROPERTY_is_I_Unit(OP_code(op)) && 
      EXEC_PROPERTY_is_M_Unit(OP_code(op))) {
    REGISTER reg;

    // Special case any specific requirements for pseudo-ops first.
    switch (OP_code(op)) {
    case TOP_mov_t_ar_i:
    case TOP_mov_t_ar_r:
      reg = TN_register(OP_result(op, 0));

      // AR regs (0 - 47) use the M- unit.
      if (reg <= 47) {
	*prop = ISA_EXEC_PROPERTY_M_Unit;
	Set_OP_m_unit(op);
      } // AR regs (64 - 111) use the I- Unit.
      else if (64 <= reg && reg <= 111) {
	*prop = ISA_EXEC_PROPERTY_I_Unit;
	Reset_OP_m_unit(op);
      } else {
	FmtAssert(FALSE, ("unexpected register operand"));
      }
      break;
    case TOP_mov_f_ar:
      reg = TN_register(OP_opnd(op, 1));
      
      // AR regs (0 - 47) use the M- Unit
      if (reg <= 47) {
	*prop = ISA_EXEC_PROPERTY_M_Unit;
	Set_OP_m_unit(op);
      }  // AR regs (64 - 111) use the I- Unit
      else if (64 <= reg && reg <= 111) {
	*prop = ISA_EXEC_PROPERTY_I_Unit;
	Reset_OP_m_unit(op);
      } else {
	FmtAssert(FALSE, ("unexpected register operand"));
      }
      break;
    default:
      
      // Use the M- unit if slot position is <zero>, or the slot position
      // is <one> and there exists a bundle dependence, for maximal
      // template coverage.
      if (slot == 0 || (slot == 1 && stop_bit_reqd)) {
	*prop = ISA_EXEC_PROPERTY_M_Unit;
	Set_OP_m_unit(op);
      } else if (grouping != NULL){
	// This assumes we have reserved resources for all ops to be
	// grouped together, except those that have a choice between
	// resources.
	//
	if (slot == 2 || // No bundle exists with an M unit in slot 2
	    grouping->avail_i_units() > grouping->avail_m_units())
	{
	  *prop = ISA_EXEC_PROPERTY_I_Unit;
	  Reset_OP_m_unit(op);
	}
	else
	{
	  *prop = ISA_EXEC_PROPERTY_M_Unit;
	  Set_OP_m_unit(op);
	}
      } else {
	*prop = ISA_EXEC_PROPERTY_I_Unit;
	Reset_OP_m_unit(op);
      }
    }
  } else if (EXEC_PROPERTY_is_B_Unit(OP_code(op)) &&
	     EXEC_PROPERTY_is_B2_Unit(OP_code(op))) {
    *prop = (slot == 2) ? ISA_EXEC_PROPERTY_B2_Unit : ISA_EXEC_PROPERTY_B_Unit;

  } else if (EXEC_PROPERTY_is_I_Unit(OP_code(op)) &&
	     EXEC_PROPERTY_is_I2_Unit(OP_code(op))) {
    *prop = (slot == 2) ? ISA_EXEC_PROPERTY_I2_Unit : ISA_EXEC_PROPERTY_I_Unit;
  } else if (EXEC_PROPERTY_is_L_Unit(OP_code(op)) &&
	     EXEC_PROPERTY_is_I2_Unit(OP_code(op))) {
    *prop = (slot == 2) ? ISA_EXEC_PROPERTY_I2_Unit : ISA_EXEC_PROPERTY_L_Unit;
  }
  else {
    *prop = ISA_EXEC_Unit_Prop(OP_code(op));
  }

  // Need to check if extra slots are required and available within the
  // bundle (eg. movl)
  BOOL extra_slot_reqd = (inst_words == 2) ?
    TI_BUNDLE_Slot_Available(bundle, *prop, slot + 1) : TRUE;

  // if <stop_bit_reqd>, check for the availability of STOP bit being
  // available at position <slot - 1>.

  BOOL stop_bit_avail = (stop_bit_reqd) ? 
    CGTARG_Bundle_Stop_Bit_Available(bundle, slot - 1) : TRUE;
  
  if (slot > 0) {
    if (stop_bit_reqd && stop_bit_avail) {
      TI_BUNDLE_Reserve_Stop_Bit(bundle, slot - 1);
    }
  }
   
  BOOL slot_avail = extra_slot_reqd && stop_bit_avail &&
    TI_BUNDLE_Slot_Available (bundle, *prop, slot);

  if (slot > 0) {
    if (stop_bit_reqd && stop_bit_avail) {
      TI_BUNDLE_Unreserve_Stop_Bit(bundle, slot - 1);
    }
  }

  return slot_avail;
}

/* ====================================================================
 *
 * CGTARG_Bundle_Stop_Bit_Available
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL 
CGTARG_Bundle_Stop_Bit_Available(TI_BUNDLE *bundle, INT slot)
{
  // Return TRUE the stop-bit is already set.
  if (TI_BUNDLE_stop_bit(bundle, slot)) return TRUE;

  return TI_BUNDLE_Stop_Bit_Available(bundle, slot);
}

/* ====================================================================
 *
 * CGTARG_Handle_Bundle_Hazard
 *
 * See interface description
 *
 * ====================================================================
 */
void
CGTARG_Handle_Bundle_Hazard (OP                          *op, 
			     TI_BUNDLE                   *bundle, 
			     VECTOR                      *bundle_vector,
			     BOOL                        can_fill, 
			     INT                         slot_pos, 
			     INT                         max_pos,
			     BOOL                        stop_bit_reqd,
			     ISA_EXEC_UNIT_PROPERTY      prop) 
{

  INT ti_err = TI_RC_OKAY;

  // if <bundle> is empty, no need to do anything. just check to see if
  // a stop_bit is required.
  if (TI_BUNDLE_Is_Empty(bundle, &ti_err)) {
    OP *last_real_op = Last_Real_OP(op);
    FmtAssert(ti_err != TI_RC_ERROR, ("%s", TI_errmsg));
    if (stop_bit_reqd) Set_OP_end_group(last_real_op);
    return;
  }

  INT template_bit = TI_BUNDLE_Return_Template(bundle);

  FmtAssert (template_bit != -1, ("Illegal template encoding"));

  // Process simulated ops first.
  if (OP_simulated(op)) {
    TOP adjust_top = CGTARG_Simulated_Top(op, prop);
    OP_Change_Opcode(op, adjust_top);
  }

  // Adjust the slot_pos for TOPs which occupy more than 1 slot position,
  // eg. movl (2 slots), asm (3 slots) etc..
  INT adjusted_slot_pos = (can_fill && ISA_PACK_Inst_Words(OP_code(op)) > 1) ? 
    max_pos + ISA_PACK_Inst_Words(OP_code(op)) - 1 : max_pos;

  TOP nop;
  INT i;
  OP *prev_op = NULL;
  FOR_ALL_SLOT_MEMBERS(bundle, i) {
    if (i > adjusted_slot_pos) break;
    if (!TI_BUNDLE_slot_filled(bundle, i)) {
      if (i <= max_pos) {
	nop = CGTARG_Noop_Top (ISA_EXEC_Slot_Prop(template_bit, i));
      
	OP *noop = Mk_OP (nop, True_TN, Gen_Literal_TN(0, 4));
	
	// Check for conditions if noops need to be inserted before (or
	// after) <op>.
        // fix bug for chk is OP_xfer but not TOP_is_xfer;
	// if ((can_fill && !OP_xfer(op) && i >= slot_pos)) { 
	if ((can_fill && !TOP_is_xfer(OP_code(op)) && i >= slot_pos)) { 
	  BB_Insert_Op_After(OP_bb(op), (prev_op) ? prev_op : op, noop);
 	  OP_scycle(noop) = -1;
	  prev_op = noop;
        }
	else {
	  BB_Insert_Op_Before(OP_bb(op), op, noop);
 	  OP_scycle(noop) = -1;
	  prev_op = (can_fill) ? op : noop;
	}
	Set_OP_bundled (noop);
	if (OP_code(noop) == TOP_nop_m) 
	  Set_OP_m_unit(noop);
	TI_BUNDLE_Reserve_Slot (bundle, i, ISA_EXEC_Slot_Prop(template_bit, i));
      } else {
	TI_BUNDLE_Reserve_Slot (bundle, i, prop);
	prev_op = op;
      }
    }
  }

  // if the <bundle> is full, set the <end_group> marker appropriately.
  if (TI_BUNDLE_Is_Full(bundle, &ti_err)) {
    FmtAssert(ti_err != TI_RC_ERROR, ("%s", TI_errmsg));
    // fix bug for chk is OP_xfer but not TOP_is_xfer;
    // if (OP_xfer(op)) {
    if (TOP_is_xfer(OP_code(op))) {
      if (BB_last_real_op(OP_bb(op)) == op)	Set_OP_end_group(op);
      if (!can_fill && stop_bit_reqd) {
	Set_OP_end_group(prev_op);
	VECTOR_Reset (*bundle_vector);
      }
    }
    else if (prev_op && 
	     ((prev_op != op && stop_bit_reqd) || 
	      (BB_last_op(OP_bb(prev_op)) == prev_op))) {
      Set_OP_end_group(prev_op);
      VECTOR_Reset (*bundle_vector);
    }
    else if (can_fill && BB_last_op(OP_bb(op)) == op) {
      Set_OP_end_group(op);
      VECTOR_Reset (*bundle_vector);
    }
  }
}

/* ====================================================================
 *
 * CGTARG_Handle_Errata_Hazard
 *
 * See interface description
 *
 * ====================================================================
 */
void
CGTARG_Handle_Errata_Hazard (OP *op, INT erratnum, INT ops_to_check)
{
}

/* ====================================================================
 *
 * Reduce_Fraction
 *
 * Half hearted attempt to reduce a fraction. If we don't succeed
 * the only problem will be that we might round incorrectly on a
 * instruction rate.
 *
 * The algorithm is to first try the denominator as a factor and
 * then a few small primes.
 *
 * ====================================================================
 */
static void
Reduce_Fraction(INT frac[2])
{
  INT i;
  static const INT primes[] = {2, 3, 5, 7, 11, 13};
  INT n = frac[0];
  INT d = frac[1];
  INT p = d;

  if (d < -1 || d > 1) {
    for (i = sizeof(primes) / sizeof(primes[0]); ; p = primes[--i]) {
      while (n % p == 0 && d % p == 0) {
	n = n / p;
	d = d / p;
      }
      if (i == 0) break;
    }
  }

  frac[0] = n;
  frac[1] = d;
}


/* ====================================================================
 *
 * Harmonic_Mean
 *
 * Compute the harmonic weighted mean of two rates as follows:
 *
 *	  1        a                    b
 *	---- = ( ----- * a_rate ) + ( ----- * b_rate )
 *	mean     a + b                a + b
 *
 * Where:
 *
 *	"a" is the number of operations of class "a"
 *	"b" is the number of operations of class "b"
 *
 * ====================================================================
 */
static void
Harmonic_Mean(
  INT mean[2],
  INT a,
  const INT a_rate[2],
  INT b,
  const INT b_rate[2]
) {
  if (a == 0) {
    mean[0] = b_rate[0];
    mean[1] = b_rate[1];
  } else if (b == 0) {
    mean[0] = a_rate[0];
    mean[1] = a_rate[1];
  } else {
    mean[1] =   (a * a_rate[1] * b_rate[0]) 
	      + (b * b_rate[1] * a_rate[0]);
    mean[0] = (a + b) * a_rate[0] * b_rate[0];
    Reduce_Fraction(mean);
  }
}


/* ====================================================================
 *
 * CGTARG_Peak_Rate
 *
 * See interface description
 *
 * ====================================================================
 */
void CGTARG_Peak_Rate( PEAK_RATE_CLASS prc, PRC_INFO *info, INT ratio[2] )
{
  ratio[0] = 1;
  ratio[1] = 1;
  
  switch (prc) {
  case PRC_INST:
    ratio[0] = 6;
    break;
  case PRC_MADD:
  case PRC_MEMREF:
    ratio[0] = 2;
    break;
  case PRC_FLOP:
  case PRC_FADD:
  case PRC_FMUL:
    ratio[0] = 2;
    break;
  case PRC_IOP:
    ratio[0] = 4;
    break;
  default:
    ratio[0] = 2;
    break;
  }
}

/* =======================================================================
 *
 *  Plural
 *
 *  Return "s" if i != 1, "" otherwise.  Used to get the number of nouns
 *  right when printing.
 *
 * =======================================================================
 */
#define Plural(i) ((i) != 1 ? "s" : "")


/* =======================================================================
 *
 *  Percent_Of_Peak
 *
 *  Compute the percentage of peak instructions executed. Both the
 *  actual number of instructions executed and the peak attainable
 *  are expressed as a fraction of insts/cycle.
 *
 * =======================================================================
 */
static INT
Percent_Of_Peak(INT numer, INT denom, INT peak[2])
{
  if (numer == 0) return 0;
  return (numer * peak[1] * 100) / ((denom * peak[0]) + peak[1] - 1);
}

/* =======================================================================
 *
 *  CGTARG_Print_PRC_INFO
 *
 *  Print statistics for the PRC_INFO to a 'file'.
 *
 * =======================================================================
 */
void
CGTARG_Print_PRC_INFO(
  FILE       *file,
  PRC_INFO   *info,
  INT32      ii,
  const char *prefix,
  const char *suffix
)
{
  char *s;
  INT madds_per_cycle[2];
  INT memrefs_per_cycle[2];
  INT flops_per_cycle[2];
  INT fadds_per_cycle[2];
  INT fmuls_per_cycle[2];
  INT iops_per_cycle[2];
  INT insts_per_cycle[2];
  INT insts = info->refs[PRC_INST];
  INT memrefs = info->refs[PRC_MEMREF];
  INT flops = info->refs[PRC_FLOP];
  INT madds = info->refs[PRC_MADD];
  INT fadds = info->refs[PRC_FADD];
  INT fmuls = info->refs[PRC_FMUL];
  INT iops = info->refs[PRC_IOP];

  CGTARG_Peak_Rate(PRC_INST, info, insts_per_cycle);
  CGTARG_Peak_Rate(PRC_MEMREF, info, memrefs_per_cycle);
  CGTARG_Peak_Rate(PRC_FLOP, info, flops_per_cycle);
  CGTARG_Peak_Rate(PRC_MADD, info, madds_per_cycle);
  CGTARG_Peak_Rate(PRC_FADD, info, fadds_per_cycle);
  CGTARG_Peak_Rate(PRC_FMUL, info, fmuls_per_cycle);
  CGTARG_Peak_Rate(PRC_IOP, info, iops_per_cycle);

  if (flops != 0) {
    BOOL unbalanced_fpu = FALSE;

    if ( madds_per_cycle[0] != 0 ) {
      fprintf(file,"%s%5d flop%1s        (%3d%% of peak) (madds count as 2)%s"
                   "%s%5d flop%1s        (%3d%% of peak) (madds count as 1)%s"
                   "%s%5d madd%1s        (%3d%% of peak)%s",
		 prefix,
		 flops + madds,
		 Plural(flops + madds),
		 Percent_Of_Peak(flops + madds, ii * 2, madds_per_cycle),
		 suffix,
		 prefix,
		 flops,
		 Plural(flops),
		 Percent_Of_Peak(flops, ii, flops_per_cycle),
		 suffix,
		 prefix,
		 madds,
		 Plural(madds),
		 Percent_Of_Peak(madds, ii, madds_per_cycle),
		 suffix);
    }
    else {
      fprintf(file,"%s%5d flop%1s        (%3d%% of peak)%s",
		 prefix,
		 flops,
		 Plural(flops),
		 Percent_Of_Peak(flops, ii, flops_per_cycle),
		 suffix);
    }

    if ( unbalanced_fpu ) {
      INT fmuls2_per_cycle[2]; /* combined fmul/madd peak rate */
      INT fadds2_per_cycle[2]; /* combined fadd/madd peak rate */
      INT fadds2 = fadds + madds;
      INT fmuls2 = fmuls + madds;

      Harmonic_Mean(fmuls2_per_cycle,
		    fmuls, fmuls_per_cycle,
		    madds, madds_per_cycle);
      Harmonic_Mean(fadds2_per_cycle,
		    fadds, fadds_per_cycle,
		    madds, madds_per_cycle);

      fprintf(file,"%s%5d fmul%1s        (%3d%% of peak)%s%s",
		 prefix,
		 fmuls2,
		 Plural(fmuls2),
		 Percent_Of_Peak(fmuls2, ii, fmuls2_per_cycle),
		 madds_per_cycle[0] ? " (madds count as 1)" : "",
		 suffix);
      fprintf(file,"%s%5d fadd%1s        (%3d%% of peak)%s%s",
		 prefix,
		 fadds2,
		 Plural(fadds2),
		 Percent_Of_Peak(fadds2, ii, fadds2_per_cycle),
		 madds_per_cycle[0] ? " (madds count as 1)" : "",
		 suffix);
    }
  }

  s = "";
  if (FALSE) {
    iops += memrefs;
    s = " (mem refs included)";
  }

  fprintf(file,"%s%5d mem ref%1s     (%3d%% of peak)%s"
               "%s%5d integer op%1s  (%3d%% of peak)%s%s"
               "%s%5d instruction%1s (%3d%% of peak)%s",
               prefix,
               memrefs,
               Plural(memrefs),
               Percent_Of_Peak(memrefs, ii, memrefs_per_cycle),
	       suffix,
               prefix,
               iops,
               Plural(iops),
               Percent_Of_Peak(iops, ii, iops_per_cycle),
	       s,
	       suffix,
               prefix,
               insts,
               Plural(insts),
               Percent_Of_Peak(insts, ii, insts_per_cycle),
	       suffix);
}



/* =======================================================================
 *
 *  CGTARG_Compute_PRC_INFO
 *
 *  Compute some basic information about the given 'bb'. 
 *
 * =======================================================================
 */
void
CGTARG_Compute_PRC_INFO(
  BB *bb,
  PRC_INFO *info
)
{
  OP *op;

  bzero (info, sizeof (PRC_INFO));

  for ( op = BB_first_op(bb); op != NULL; op = OP_next(op) ) {
    INT num_insts = OP_Real_Ops (op);

    if (num_insts == 0) continue;

    info->refs[PRC_INST] += num_insts;

    if ( OP_flop(op) ) {
      BOOL is_single = (OP_result_size(op,0) == 32);

      ++info->refs[PRC_FLOP];
      info->refs[PRC_FLOP_S] += is_single;
      if (OP_madd(op)) {
        ++info->refs[PRC_MADD];
	info->refs[PRC_MADD_S] += is_single;
      }
      else if (OP_fadd(op) || OP_fsub(op)) {
	++info->refs[PRC_FADD];
	info->refs[PRC_FADD_S] += is_single;
      }
      else if (OP_fmul(op)) {
	++info->refs[PRC_FMUL];
	info->refs[PRC_FMUL_S] += is_single;
      }
    }
    else if (OP_memory(op))
      ++info->refs[PRC_MEMREF];
    else {
      INT k;

      /* Conditional moves and m[tf]c1 are not tagged as flops.
       * We certainly don't want to call them integer ops, so assume
       * anything that uses FP regs isn't an integer instruction.
       */
      if (OP_has_result(op) && TN_is_float(OP_result(op,0))) goto not_iop;

      for (k = 0; k < OP_opnds(op); k++) {
	if (TN_is_float(OP_opnd(op,k))) goto not_iop;
      }

      info->refs[PRC_IOP] += num_insts;

    not_iop:
      ;
    }
  }
}


/* ====================================================================
 *
 * CG_TARG_Branch_Info
 *
 * See interface description
 *
 * ====================================================================
 */
void
CGTARG_Branch_Info ( const OP  *op,
                    INT *tfirst,  /* Which operand is the first target? */
                    INT *tcount ) /* How many target operands are there? */
{
  INT i;
  TN *tn;

  /* Initialize results: */
  *tfirst = -1;
  *tcount = 0;

  /* Find the first target: */
  for ( i = 0; ; i++ ) {
    if ( i >= OP_opnds(op) ) return;
    tn = OP_opnd(op,i);
    if ( tn != NULL && TN_is_label(tn) ) break;
  }
  *tfirst = i;

  /* Count the targets: */
  *tcount = 1;
  for ( i++; i < OP_opnds(op); i++ ) {
    tn = OP_opnd(op,i);
    if ( tn == NULL || ! TN_is_label(tn) ) return;
    (*tcount)++;
  }
  return;
}


/* ====================================================================
 *
 * CGTARG_Can_Be_Speculative
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL
CGTARG_Can_Be_Speculative( OP *op )
{
  WN *wn;

  /* not allowed to speculate anything. */
  if (Eager_Level == EAGER_NONE) return FALSE;

  /* don't speculate volatile memory references. */
  if (OP_volatile(op)) return FALSE;

  if (TOP_Can_Be_Speculative(OP_code(op))) return TRUE;

  if (!OP_load(op)) return FALSE;

  /* Try to identify simple scalar loads than can be safely speculated:
   *  a) read only loads (literals, GOT-loads, etc.)
   *  b) load of a fixed variable (directly referenced)
   *  c) load of a fixed variable (base address is constant or
   *     known to be in bounds)
   *  d) speculative, advanced and advanced-speculative loads are safe.
   */

  /*  a) read only loads (literals, GOT-loads, etc.)
   */
  if (OP_no_alias(op)) goto scalar_load;

  /*  b) load of a fixed variable (directly referenced); this
   *     includes spill-restores.
   *  b') exclude cases of direct loads of weak symbols (#622949).
   */
  if (TN_is_symbol(OP_opnd(op, 1)) &&
      !ST_is_weak_symbol(TN_var(OP_opnd(op, 1)))) goto scalar_load;

  /*  c) load of a fixed variable (base address is constant or
   *     known to be in bounds), comment out the rematerizable bit check 
   *     since it doesn;t guarantee safeness all the time.
   */
  if (/*   TN_is_rematerializable(OP_opnd(op, 0)) || */
      (   (wn = Get_WN_From_Memory_OP(op))
	  && Alias_Manager->Safe_to_speculate(wn))) goto scalar_load;

  /* d) speculative, advanced, speculative-advanced loads are safe to 
   *    speculate. 
   */
  if (CGTARG_Is_OP_Speculative(op)) goto scalar_load;

  /* If we got to here, we couldn't convince ourself that we have
   * a scalar load -- no speculation this time...
   */
  return FALSE;

  /* We now know we have a scalar load of some form. Determine if they
   * are allowed to be speculated.
   */
scalar_load:
  return TRUE; 
}

/* ====================================================================
 *
 * CGTARG_Is_OP_Speculative_Load
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL
CGTARG_Is_OP_Speculative_Load( OP *memop )
{
  if (!OP_load(memop)) return FALSE;

  INT i;
  for (i = 0; i < OP_opnds(memop); ++i) {
    TN *opnd_tn = OP_opnd(memop, i);
    if (TN_is_enum(opnd_tn)) {
      if (TN_enum(opnd_tn) == ECV_ldtype_s ||
 	  TN_enum(opnd_tn) == ECV_ldtype_sa ||
 	  TN_enum(opnd_tn) == ECV_fldtype_sa ||
 	  TN_enum(opnd_tn) == ECV_fldtype_s) {
        return TRUE;
      }
    }
  }

  return FALSE;
}

/* ====================================================================
 *
 * CGTARG_Is_OP_Advanced_Load
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL
CGTARG_Is_OP_Advanced_Load( OP *memop )
{
  if (!OP_load(memop)) return FALSE;

  INT i;
  for (i = 0; i < OP_opnds(memop); ++i) {
    TN *opnd_tn = OP_opnd(memop, i);
    if (TN_is_enum(opnd_tn)) {
      if (TN_enum(opnd_tn) == ECV_ldtype_a  ||
	  TN_enum(opnd_tn) == ECV_ldtype_sa ||
 	  TN_enum(opnd_tn) == ECV_fldtype_a ||
 	  TN_enum(opnd_tn) == ECV_fldtype_sa) {
         return TRUE;
      }
    }
  }

  return FALSE;
}

/* ====================================================================
 *
 * CGTARG_Is_OP_Check_Load
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL
CGTARG_Is_OP_Check_Load( OP *memop )
{
  if (!OP_load(memop)) return FALSE;

  INT i;
  for (i = 0; i < OP_opnds(memop); ++i) {
    TN *opnd_tn = OP_opnd(memop, i);
    if (TN_is_enum(opnd_tn)) {
      if (TN_enum(opnd_tn) == ECV_ldtype_c_clr ||
	  TN_enum(opnd_tn) == ECV_ldtype_c_nc ||
	  TN_enum(opnd_tn) == ECV_ldtype_c_clr_acq ||
	  TN_enum(opnd_tn) == ECV_fldtype_c_clr ||
	  TN_enum(opnd_tn) == ECV_fldtype_c_nc) return TRUE;
    }
  }

  return FALSE;
}

/* ====================================================================
 *
 * CGTARG_Is_Form_For_Advanced_Load 
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL
CGTARG_Is_Form_For_Advanced_Load (ISA_ENUM_CLASS_VALUE ldform) {
    return ldform == ECV_ldtype_sa || 
           ldform == ECV_ldtype_a ||
           ldform == ECV_fldtype_a ||
           ldform == ECV_fldtype_sa;
}

/* ====================================================================
 *
 * CGTARG_OP_Defs_TN
 * CGTARG_OP_Refs_TN
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL
CGTARG_OP_Defs_TN( OP *op, TN *tn )
{
  return FALSE;
}

BOOL
CGTARG_OP_Refs_TN( OP *op, TN *tn )
{
  return FALSE;
}


/* ====================================================================
 *
 * CGTARG_Interference implementation starts here
 *
 * ====================================================================
 */

static MEM_POOL interference_pool;
static VOID_LIST** writing;     /* writing[i] is a list of live ranges being
                                   written into registers in cycle i */
static BOOL is_loop;            /* Are we working on a loop? */
static INT32 assumed_longest_latency = 40;
                                /* We need to allocate <writing> to be somewhat
                                   longer than the number of cycles in the
                                   schedule in order to accommodate writes
                                   initiated near the end of the schedule.
                                   We'll check and grow this number as
                                   necessary. */
static INT32 cycle_count;       /* Number of cycles in the schedule under
                                   consideration. */
static void (*make_interference)(void*,void*);
                                /* Client's interference call back. */

/* ====================================================================
 *
 * Increase_Assumed_Longest_Latency
 *
 * We need to increase our assumptions about the longest latency operation
 * in our target.  Also reallocate <writing>.
 *
 * ====================================================================
 */
static void
Increase_Assumed_Longest_Latency(INT32 new_longest_latency )
{
  DevWarn("Assumed longest latency should be at least %d",
          new_longest_latency);
  writing = TYPE_MEM_POOL_REALLOC_N(VOID_LIST*,&interference_pool,writing,
                                    cycle_count + assumed_longest_latency,
                                    cycle_count + new_longest_latency);
  assumed_longest_latency = new_longest_latency;
}

/* ====================================================================
 *
 * CGTARG_Interference_Required
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL
CGTARG_Interference_Required(void)
{
  return FALSE;
}

/* ====================================================================
 *
 * CGTARG_Interference_Initialize
 *
 * See interface description
 *
 * ====================================================================
 */
void
CGTARG_Interference_Initialize( INT32 cycle_count_local, BOOL is_loop_local,
                                void (*make_interference_local)(void*,void*) )
{
  static BOOL interference_pool_initialized = FALSE;

  if ( ! interference_pool_initialized ) {
    MEM_POOL_Initialize(&interference_pool,"Target_specific_interference",
                        TRUE);
    interference_pool_initialized = TRUE;
  }

  MEM_POOL_Push(&interference_pool);

  is_loop = is_loop_local;
  make_interference = make_interference_local;
  cycle_count = cycle_count_local;

  if ( is_loop )
    writing = TYPE_MEM_POOL_ALLOC_N(VOID_LIST*,&interference_pool,cycle_count);
  else
    writing = TYPE_MEM_POOL_ALLOC_N(VOID_LIST*,&interference_pool,
                                    cycle_count + assumed_longest_latency);
}

/* ====================================================================
 *
 * CGTARG_Result_Live_Range
 *
 * See interface description
 *
 * ====================================================================
 */
void
CGTARG_Result_Live_Range( void* lrange, OP* op, INT32 offset )
{
  VOID_LIST* l;
  INT32 cycle = OP_scycle(op);

  if ( OP_defs_fp(op) ) {
    INT32 reg_write_cyc;
    reg_write_cyc =   TI_LATENCY_Result_Available_Cycle(OP_code(op), 0 /*???*/)
		    + (OP_load(op) ? 0 : 1);

    if ( is_loop )
      reg_write_cyc = Mod(reg_write_cyc + cycle + offset,cycle_count);
    else {
      reg_write_cyc += cycle + offset;

      if ( reg_write_cyc >= cycle_count + assumed_longest_latency ) {
        /* Our assumed longest latency was too low, it seems */
        Increase_Assumed_Longest_Latency(1 + reg_write_cyc - cycle_count);
      }
    }

    for ( l = writing[reg_write_cyc]; l != NULL; l = VOID_LIST_rest(l) )
      make_interference(lrange,VOID_LIST_first(l));

    writing[reg_write_cyc] = VOID_LIST_Push(lrange,writing[reg_write_cyc],
                                            &interference_pool);
  }
}

/* ====================================================================
 *
 * CGTARG_Operand_Live_Range
 *
 * See interface description
 *
 * ====================================================================
 */
void
CGTARG_Operand_Live_Range( void* lrange, INT   opnd, OP*   op, INT32 offset )
{
  VOID_LIST* l;
  INT32 cycle = OP_scycle(op);
  INT32 reg_acc_cyc = TI_LATENCY_Operand_Access_Cycle(OP_code(op), opnd);

  if ( is_loop )
    reg_acc_cyc = Mod(reg_acc_cyc + cycle + offset,cycle_count);
  else {
    reg_acc_cyc += cycle + offset;

    if ( reg_acc_cyc >= cycle_count + assumed_longest_latency )
      return;   /* Nothing writing in this cycle anyway */
  }

  for ( l = writing[reg_acc_cyc]; l != NULL; l = VOID_LIST_rest(l) )
    make_interference(lrange,VOID_LIST_first(l));
}

/* ====================================================================
 *
 * CGTARG_Interference_Finalize
 *
 * See interface description
 *
 * ====================================================================
 */
void
CGTARG_Interference_Finalize(void)
{
  MEM_POOL_Pop(&interference_pool);
  writing = (VOID_LIST **) -1;
}


/* ====================================================================
 *
 * CGTARG_Preg_Register_And_Class
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL
CGTARG_Preg_Register_And_Class(
  WN_OFFSET preg,
  ISA_REGISTER_CLASS *p_rclass,
  REGISTER *p_reg
)
{
  ISA_REGISTER_CLASS rclass;
  INT regnum;
  REGISTER reg;

  /* Get the target register number and class associated with the
   * preg, if there is one that is.
   */
  if (Preg_Offset_Is_Int(preg)) {
    regnum = preg - (Int_Preg_Min_Offset - 1);
    rclass = ISA_REGISTER_CLASS_integer;
  }
  else if (Preg_Offset_Is_Float(preg)) {
    regnum = preg - Float_Preg_Min_Offset;
    rclass = ISA_REGISTER_CLASS_float;
  }
  // bug fix for OSP_87
  else if (Preg_Offset_Is_Branch(preg)) {
    regnum = preg - Branch_Preg_Min_Offset;
    rclass = ISA_REGISTER_CLASS_branch;
  }
  else if (preg == 0) {
    /* 0 not considered part of normal int group for error purposes,
     * but in our case it can be zero_tn. */
    regnum = 0;
    rclass = ISA_REGISTER_CLASS_integer;
  }
  else {
    return FALSE;
  }

  /* Find the CG register for the target register and class.
   */
  for ( reg = REGISTER_MIN; reg <= REGISTER_CLASS_last_register(rclass); reg++ ) {
    if ( REGISTER_machine_id(rclass,reg) == regnum ) {
      *p_reg = reg;
      *p_rclass = rclass;
      return TRUE;
    }
  }
  FmtAssert(FALSE, ("failed to map preg %d", preg));
  /*NOTREACHED*/
}


/* ====================================================================
 *
 * CGTARG_Compute_Branch_Parameters
 *
 * See interface description
 *
 * ====================================================================
 */
void CGTARG_Compute_Branch_Parameters(INT32 *mispredict, INT32 *fixed, INT32 *brtaken, double *factor)
{
  *mispredict = 0;
  *fixed = 0;
  *brtaken = 0;
  *factor = 0.0;

  if (Is_Target_Itanium())
  {
    *mispredict= 8; *fixed= 1; *brtaken= 1; *factor = 1.0;
  }
  else
  {
    FmtAssert(FALSE, ("invalid target"));
  }

 /*
  * override for command line options
  *	-CG:mispredicted_branch=N
  *	-CG:mispredicted_factor=N
  */
  if (CG_branch_mispredict_penalty >= 0)
    *mispredict= CG_branch_mispredict_penalty ;

  if (CG_branch_mispredict_factor >= 0)
    *factor= CG_branch_mispredict_factor * (.01);
}


/* ====================================================================
 *
 * CGTARG_Can_Change_To_Brlikely
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL CGTARG_Can_Change_To_Brlikely(OP *xfer_op, TOP *new_opcode)
{
  return FALSE;
}


/* ====================================================================
 *
 * CGTARG_Is_Long_Latency
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL CGTARG_Is_Long_Latency(TOP op)
{
  switch (op) {

    // floating point opcodes:
  case TOP_frcpa:
  case TOP_frsqrta:
  case TOP_fprcpa:
  case TOP_fprsqrta:

    // integer opcodes:
    return TRUE;
  }
  return FALSE;
}

/* ====================================================================
 *
 * CGTARG_Analyze_Branch
 *
 * See interface description
 *
 * ====================================================================
 */
VARIANT CGTARG_Analyze_Branch(
  OP *br,
  TN **tn1,
  TN **tn2)
{
  VARIANT variant;

  *tn1 = NULL;
  *tn2 = NULL;

  switch (OP_code(br)) {
  case TOP_br_cond:
  case TOP_br_r_cond:
  case TOP_br_call:
  case TOP_br_r_call:
  case TOP_br_ret:
  case TOP_br_ia:
    {
      TN* pred_tn = OP_opnd(br, OP_PREDICATE_OPND);

      if (TN_register_and_class(pred_tn) != CLASS_AND_REG_true) {
	*tn1 = pred_tn;
	variant = V_BR_P_TRUE;
	break;
      }
    }
    /*FALLTHROUGH*/

  case TOP_br:
  case TOP_br_r:
    variant = V_BR_ALWAYS;
    break;

  case TOP_br_cloop:
    variant = V_BR_CLOOP;
    break;

  case TOP_br_ctop:
    variant = V_BR_CTOP;
    break;

  case TOP_br_cexit:
    variant = V_BR_CEXIT;
    break;

  case TOP_br_wtop:
    variant = V_BR_WTOP;
    break;

  case TOP_br_wexit:
    variant = V_BR_WEXIT;
    break;

  default:
    variant = V_BR_NONE;
    break;
  }

  return variant;
}


/* ====================================================================
 *
 * CGTARG_Analyze_Compare
 *
 * See interface description
 *
 * ====================================================================
 */
VARIANT CGTARG_Analyze_Compare(
  OP *br,
  TN **tn1,
  TN **tn2,
  OP **compare_op)
{
  VARIANT variant;
  TN *cond_tn1;
  TN *cond_tn2;
  BOOL is_double;
  DEF_KIND kind;

  /* Classify the condition based on the branch instruction.
   */
  variant = CGTARG_Analyze_Branch(br, &cond_tn1, &cond_tn2);

  /*
   * Check for double precision float compare.
   */
  if (cond_tn1 && TN_is_float(cond_tn1) && TN_size(cond_tn1) == 8) {
    is_double = TRUE;
  } else {
    is_double = FALSE;
  }

  /* If the branch was predicated, the value being tested
   * was probably the result of a cmp*, in which case we
   * we can refine the actual condition.
   */
  if (variant == V_BR_P_TRUE) {
    OP *def_op;

    /* Attempt to find the defining OP for the tested value.
     */
    def_op = TN_Reaching_Value_At_Op(cond_tn1, br, &kind, TRUE);

    // TODO: <kind != VAL_KNOWN> may be too conservative. Analyze_Compare
    // need to work with compare defs which could be predicated themselves.
    if (!def_op || kind != VAL_KNOWN) goto no_cmp;

    /* Verify the defining OP is a cmp* and determine the actual
     * condition being tested.
     */
    switch (OP_code(def_op)) {
    case TOP_cmp_eq:
    case TOP_cmp_eq_unc:
    case TOP_cmp_i_eq:
    case TOP_cmp_i_eq_unc:
      variant = V_BR_I8EQ;
      break;
    case TOP_cmp4_eq:
    case TOP_cmp4_eq_unc:
    case TOP_cmp4_i_eq:
    case TOP_cmp4_i_eq_unc:
      variant = V_BR_I4EQ;
      break;
    case TOP_cmp_lt:
    case TOP_cmp_i_lt:
    case TOP_cmp_lt_unc:
    case TOP_cmp_i_lt_unc:
      variant = V_BR_I8LT;
      break;
    case TOP_cmp4_lt:
    case TOP_cmp4_i_lt:
    case TOP_cmp4_lt_unc:
    case TOP_cmp4_i_lt_unc:
      variant = V_BR_I4LT;
      break;
    case TOP_cmp_ltu:
    case TOP_cmp_i_ltu:
    case TOP_cmp_ltu_unc:
    case TOP_cmp_i_ltu_unc:
      variant = V_BR_U8LT;
      break;
    case TOP_cmp4_ltu:
    case TOP_cmp4_i_ltu:
    case TOP_cmp4_ltu_unc:
    case TOP_cmp4_i_ltu_unc:
      variant = V_BR_U4LT;
      break;
    case TOP_cmp_ne:
    case TOP_cmp_i_ne:
    case TOP_cmp_ne_unc:
    case TOP_cmp_i_ne_unc:
      variant = V_BR_I8NE;
      break;
    case TOP_cmp4_ne:
    case TOP_cmp4_i_ne:
    case TOP_cmp4_ne_unc:
    case TOP_cmp4_i_ne_unc:
      variant = V_BR_I4NE;
      break;
    case TOP_cmp_le:
    case TOP_cmp_i_le:
    case TOP_cmp_le_unc:
    case TOP_cmp_i_le_unc:
      variant = V_BR_I8LE;
      break;
    case TOP_cmp4_le:
    case TOP_cmp4_i_le:
    case TOP_cmp4_le_unc:
    case TOP_cmp4_i_le_unc:
      variant = V_BR_I4LE;
      break;
    case TOP_cmp_gt:
    case TOP_cmp_i_gt:
    case TOP_cmp_gt_unc:
    case TOP_cmp_i_gt_unc:
      variant = V_BR_I8GT;
      break;
    case TOP_cmp4_gt:
    case TOP_cmp4_i_gt:
    case TOP_cmp4_gt_unc:
    case TOP_cmp4_i_gt_unc:
      variant = V_BR_I4GT;
      break;
    case TOP_cmp_ge:
    case TOP_cmp_i_ge:
    case TOP_cmp_ge_unc:
    case TOP_cmp_i_ge_unc:
      variant = V_BR_I8GE;
      break;
    case TOP_cmp4_ge:
    case TOP_cmp4_i_ge:
    case TOP_cmp4_ge_unc:
    case TOP_cmp4_i_ge_unc:
      variant = V_BR_I4GE;
      break;
    case TOP_cmp_leu:
    case TOP_cmp_i_leu:
    case TOP_cmp_leu_unc:
    case TOP_cmp_i_leu_unc:
      variant = V_BR_U8LE;
      break;
    case TOP_cmp4_leu:
    case TOP_cmp4_i_leu:
    case TOP_cmp4_leu_unc:
    case TOP_cmp4_i_leu_unc:
      variant = V_BR_U4LE;
      break;
    case TOP_cmp_gtu:
    case TOP_cmp_i_gtu:
    case TOP_cmp_gtu_unc:
    case TOP_cmp_i_gtu_unc:
      variant = V_BR_U8GT;
      break;
    case TOP_cmp4_gtu:
    case TOP_cmp4_i_gtu:
    case TOP_cmp4_gtu_unc:
    case TOP_cmp4_i_gtu_unc:
      variant = V_BR_U4GT;
      break;
    case TOP_cmp_geu:
    case TOP_cmp_i_geu:
    case TOP_cmp_geu_unc:
    case TOP_cmp_i_geu_unc:
      variant = V_BR_U8GE;
      break;
    case TOP_cmp4_geu:
    case TOP_cmp4_i_geu:
    case TOP_cmp4_geu_unc:
    case TOP_cmp4_i_geu_unc:
      variant = V_BR_U4GE;
      break;

    case TOP_fcmp_eq:
    case TOP_fcmp_eq_unc:
      if (is_double) {
	variant = V_BR_DEQ;
      } else {
	variant = V_BR_FEQ;
      }
      break;
    case TOP_fcmp_ge:
    case TOP_fcmp_ge_unc:
      if (is_double) {
	variant = V_BR_DGE;
      } else {
	variant = V_BR_FGE;
      }
      break;
    case TOP_fcmp_gt:
    case TOP_fcmp_gt_unc:
      if (is_double) {
	variant = V_BR_DGT;
      } else {
	variant = V_BR_FGT;
      }
      break;
    case TOP_fcmp_le:
    case TOP_fcmp_le_unc:
      if (is_double) {
	variant = V_BR_DLE;
      } else {
	variant = V_BR_FLE;
      }
      break;
    case TOP_fcmp_lt:
    case TOP_fcmp_lt_unc:
      if (is_double) {
	variant = V_BR_DLT;
      } else {
	variant = V_BR_FLT;
      }
      break;
    case TOP_fcmp_neq:
    case TOP_fcmp_neq_unc:
      if (is_double) {
	variant = V_BR_DNE;
      } else {
	variant = V_BR_FNE;
      }
      break;
    case TOP_fcmp_nge:
    case TOP_fcmp_nge_unc:
      if (is_double) {
	variant = V_BR_DLT;
      } else {
	variant = V_BR_FLT;
      }
      break;
    case TOP_fcmp_ngt:
    case TOP_fcmp_ngt_unc:
      if (is_double) {
	variant = V_BR_DLE;
      } else {
	variant = V_BR_FLE;
      }
      break;
    case TOP_fcmp_nle:
    case TOP_fcmp_nle_unc:
      if (is_double) {
	variant = V_BR_DGT;
      } else {
	variant = V_BR_FGT;
      }
      break;
    case TOP_fcmp_nlt:
    case TOP_fcmp_nlt_unc:
      if (is_double) {
	variant = V_BR_DGE;
      } else {
	variant = V_BR_FGE;
      }
      break;
    case TOP_fcmp_ord:
    case TOP_fcmp_ord_unc:
      if (is_double) {
	variant = V_BR_DOR;
      } else {
	variant = V_BR_FOR;
      }
      break;
    case TOP_fcmp_unord:
    case TOP_fcmp_unord_unc:
      if (is_double) {
	variant = V_BR_DUO;
      } else {
	variant = V_BR_FUO;
      }
      break;
    case TOP_tbit_z:
    case TOP_tbit_z_unc:
     if (CG_Enable_Ipfec_Phases) {
         variant = V_BR_I4EQ;
         break;
     } else {
         goto no_cmp;
     }
     case TOP_tbit_nz:
     case TOP_tbit_nz_unc: 
        if ( CG_Enable_Ipfec_Phases) {
            variant = V_BR_I4NE;
            break;
        } else {
            goto no_cmp;
        }
      default:
      // unc?
      // parallel form?
      goto no_cmp;
    }


    /* If the branch is predicated by the false result of the cmp,
     * then the comparison needs to be negated.
     */
    if (OP_result(def_op, 1) == cond_tn1) {
      variant = Negate_BR_Variant(variant);
    }

    /* The arguments being compared are the args of the cmp,
     * and the comparison OP is the cmp.
     */
    if (OP_icmp(def_op)) {
      cond_tn1 = OP_opnd(def_op, 1);
      cond_tn2 = OP_opnd(def_op, 2);
    } else {
      cond_tn1 = OP_opnd(def_op, 2);
      cond_tn2 = OP_opnd(def_op, 3);
    }
    *compare_op = def_op;
  } else {

    /* No cmp: 
     */
  no_cmp:
    *compare_op = NULL;
  }

  *tn1 = cond_tn1;
  *tn2 = cond_tn2;
  return variant;
}


/* ====================================================================
 *
 * CGTARG_Equivalent_Nonindex_Memory_Op
 *
 * See interface description
 *
 * ====================================================================
 */
TOP
CGTARG_Equiv_Nonindex_Memory_Op ( OP *op )
{
  return TOP_UNDEFINED;
}

/* ====================================================================
 *
 * CGTARG_Which_OP_Select
 *
 * See interface description
 *
 * ====================================================================
 */
TOP
CGTARG_Which_OP_Select ( UINT16 bit_size, BOOL is_float, BOOL is_fcc )
{
  FmtAssert( FALSE, ( "CGTARG_Which_OP_Select: Unsupported Target") );
  /*NOTREACHED*/
}

/* ====================================================================
 *
 * Is_OP_fp_op1
 *
 * FP_OP1 = {sfmac, sfmisc, xma, xmpy, fmac, cvt.fx, fmisc}
 *
 * ====================================================================
 */
static BOOL
Is_OP_fp_op1(OP *op)
{
  switch (OP_code(op)) {
  case TOP_fma:
  case TOP_fma_d:
  case TOP_fma_s:
  case TOP_fms:
  case TOP_fms_d:
  case TOP_fms_s:
  case TOP_fnma:
  case TOP_fnma_d:
  case TOP_fnma_s:
  case TOP_famax:
  case TOP_famin:
  case TOP_fand:
  case TOP_fandcm:
  case TOP_fchkf:
  case TOP_fmax:
  case TOP_fmerge_ns:
  case TOP_fmerge_s:
  case TOP_fmerge_se:
  case TOP_fmin:
  case TOP_fmix_l:
  case TOP_fmix_lr:
  case TOP_fmix_r:
  case TOP_for:
  case TOP_fpack:
  case TOP_frcpa: 
  case TOP_frsqrta:
  case TOP_fselect: 
  case TOP_fsetc:
  case TOP_fswap:
  case TOP_fswap_nl:
  case TOP_fswap_nr:
  case TOP_fsxt_l:
  case TOP_fsxt_r:
  case TOP_fxor:
  case TOP_fclass_m:
  case TOP_fclass_m_unc:
  case TOP_fclass_nm:
  case TOP_fclass_nm_unc:
  case TOP_fcmp_eq:
  case TOP_fcmp_eq_unc:
  case TOP_fcmp_ge:
  case TOP_fcmp_ge_unc:
  case TOP_fcmp_gt:
  case TOP_fcmp_gt_unc:
  case TOP_fcmp_le:
  case TOP_fcmp_le_unc:
  case TOP_fcmp_lt:
  case TOP_fcmp_lt_unc:
  case TOP_fcmp_neq:
  case TOP_fcmp_neq_unc:
  case TOP_fcmp_nge:
  case TOP_fcmp_nge_unc:
  case TOP_fcmp_ngt:
  case TOP_fcmp_ngt_unc:
  case TOP_fcmp_nle:
  case TOP_fcmp_nle_unc:
  case TOP_fcmp_nlt:
  case TOP_fcmp_nlt_unc:
  case TOP_fcmp_ord:
  case TOP_fcmp_ord_unc:
  case TOP_fcmp_unord:
  case TOP_fcmp_unord_unc:
  case TOP_fpma:
  case TOP_fpms:
  case TOP_fpnma:
  case TOP_fpamax:
  case TOP_fpamin:
  case TOP_fpcmp_eq:
  case TOP_fpcmp_ge:
  case TOP_fpcmp_gt:
  case TOP_fpcmp_le:
  case TOP_fpcmp_lt:
  case TOP_fpcmp_neq:
  case TOP_fpcmp_nge:
  case TOP_fpcmp_ngt:
  case TOP_fpcmp_nle:
  case TOP_fpcmp_nlt:
  case TOP_fpcmp_ord:
  case TOP_fpcmp_unord:
  case TOP_fpmax:
  case TOP_fpmerge_ns:
  case TOP_fpmerge_s:
  case TOP_fpmerge_se:
  case TOP_fpmin:
  case TOP_fprcpa:
  case TOP_fprsqrta:
  case TOP_fcvt_fx:
  case TOP_fcvt_fx_trunc:
  case TOP_fcvt_fxu:
  case TOP_fcvt_fxu_trunc:
  case TOP_fcvt_xuf:
  case TOP_fcvt_xuf_d:
  case TOP_fcvt_xuf_s:
  case TOP_fpcvt_fx:
  case TOP_fpcvt_fx_trunc:
  case TOP_fpcvt_fxu:
  case TOP_fpcvt_fxu_trunc:
  case TOP_xma_h:
  case TOP_xma_hu:
  case TOP_xma_l:
  case TOP_xma_lu:
  case TOP_xmpy_h:
  case TOP_xmpy_hu:
  case TOP_xmpy_l:
  case TOP_xmpy_lu:
    return TRUE;
  default:
    return FALSE;
  }
}

/* ====================================================================
 *
 * Insert_Stop_Bits
 *
 * ====================================================================
 */
void
Insert_Stop_Bits(BB *bb)
{
  // check if appropriate flag is enabled.
  if (!Itanium_a0_step) return;

  INT slot = 0;	// position in bundle, assumes bb starts new bundle
  OP *op;
  // Assumes that bundles have already been laid out.
  FOR_ALL_BB_OPs_FWD (bb, op) {

    if (Is_OP_fp_op1(op)) {
      OP *noop1, *noop2, *noop3;

      // Need to find last op in bundle;
      // are 3 cases:  mmf, mfi, mfb.
      // never have m in last slot, so previous mm means mmf bundle.
      OP *last_op_in_bundle;
      if (slot == 2) {
	// mmf bundle, so in last slot of bundle
	last_op_in_bundle = op;
      }
      else {
	// mfi or mfb bundle, so in middle of bundle.
      	// Advance to the next OP, set the end_of_group marker.
      	last_op_in_bundle = OP_next(op);
      	Set_OP_end_group(last_op_in_bundle);
      }

      // Add an empty bundle <nop.m, nop.i, nop.i>;; with the stop bit
      // inserted.
      noop1 = Mk_OP (TOP_nop_m, True_TN, Gen_Literal_TN(0, 4));
      BB_Insert_Op_After(bb, last_op_in_bundle, noop1);
      noop2 = Mk_OP (TOP_nop_i, True_TN, Gen_Literal_TN(0, 4));
      BB_Insert_Op_After(bb, noop1, noop2);
      noop3 = Mk_OP (TOP_nop_i, True_TN, Gen_Literal_TN(0, 4));
      BB_Insert_Op_After(bb, noop2, noop3);

      Set_OP_end_group(noop3);
    }

    if (OP_dummy(op)) continue;
    if (OP_simulated(op)) continue;
    slot += ISA_PACK_Inst_Words(OP_code(op));
    if (slot >= ISA_MAX_SLOTS) slot = 0;
  }
}

/* ====================================================================
 *
 * CGTARG_Special_Min_II
 *
 * See interface description
 *
 * ====================================================================
 */
INT32 CGTARG_Special_Min_II(BB* loop_body, BOOL trace)
{
  return 0;
}

/* ====================================================================
 *
 * Hardware_Workarounds
 *
 * Placeholder for all Hardware workarounds. 
 *
 * ====================================================================
 */
void
Hardware_Workarounds(void)
{
}

/* ====================================================================
 *
 * CGTARG_Initialize
 *
 * See interface description
 *
 * ====================================================================
 */
void CGTARG_Initialize(void)
{
  INT32 i;

  /* Init all table entries to TOP_UNDEFINED.
   */
  for(i = 0; i <= TOP_count; ++i) {
    CGTARG_Invert_Table[i] = TOP_UNDEFINED;
    CGTARG_Immed_To_Reg_Table[i] = TOP_UNDEFINED;
  }

  for (i = 0; i <= ISA_REGISTER_CLASS_MAX; ++i) {
    INT j;
    for (j = 0; j <= ISA_REGISTER_CLASS_MAX; ++j) {
      CGTARG_Inter_RegClass_Copy_Table[i][j][FALSE] = TOP_UNDEFINED;
      CGTARG_Inter_RegClass_Copy_Table[i][j][TRUE] = TOP_UNDEFINED;
    }
  }

  /* Init table for CGTARG_Invert:
   */
  CGTARG_Invert_Table[TOP_fadd]    = TOP_fsub;
  CGTARG_Invert_Table[TOP_fadd_s]  = TOP_fsub_s;
  CGTARG_Invert_Table[TOP_fadd_d]  = TOP_fsub_d;
  CGTARG_Invert_Table[TOP_fma]     = TOP_fnma;
  CGTARG_Invert_Table[TOP_fma_s]   = TOP_fnma_s;
  CGTARG_Invert_Table[TOP_fma_d]   = TOP_fnma_d;
  CGTARG_Invert_Table[TOP_add]     = TOP_sub;
  CGTARG_Invert_Table[TOP_adds]    = TOP_sub_i;
  CGTARG_Invert_Table[TOP_addl]    = TOP_sub;

  CGTARG_Invert_Table[TOP_fsub]    = TOP_fadd;
  CGTARG_Invert_Table[TOP_fsub_s]  = TOP_fadd_s;
  CGTARG_Invert_Table[TOP_fsub_d]  = TOP_fadd_d;
  CGTARG_Invert_Table[TOP_fnma]    = TOP_fma;
  CGTARG_Invert_Table[TOP_fnma_s]  = TOP_fma_s;
  CGTARG_Invert_Table[TOP_fnma_d]  = TOP_fma_d;
  CGTARG_Invert_Table[TOP_sub_i]   = TOP_adds;

  CGTARG_Invert_Table[TOP_mov_f_ar] = TOP_mov_t_ar_r;
  CGTARG_Invert_Table[TOP_mov_t_ar_r] = TOP_mov_f_ar;
  CGTARG_Invert_Table[TOP_mov_f_ar_i] = TOP_mov_t_ar_r_i;
  CGTARG_Invert_Table[TOP_mov_t_ar_r_i] = TOP_mov_f_ar_i;
  CGTARG_Invert_Table[TOP_mov_f_ar_m] = TOP_mov_t_ar_r_m;
  CGTARG_Invert_Table[TOP_mov_t_ar_r_m] = TOP_mov_f_ar_m;

  CGTARG_Invert_Table[TOP_mov_f_br]  = TOP_mov_t_br;
  CGTARG_Invert_Table[TOP_mov_t_br]  = TOP_mov_f_br;

  CGTARG_Invert_Table[TOP_mov_f_cr]  = TOP_mov_t_cr;
  CGTARG_Invert_Table[TOP_mov_t_cr]  = TOP_mov_f_cr;

  CGTARG_Invert_Table[TOP_mov_f_dbr] = TOP_mov_t_dbr;
  CGTARG_Invert_Table[TOP_mov_t_dbr] = TOP_mov_f_dbr;

  CGTARG_Invert_Table[TOP_mov_f_ibr] = TOP_mov_t_ibr;
  CGTARG_Invert_Table[TOP_mov_t_ibr] = TOP_mov_f_ibr;

  CGTARG_Invert_Table[TOP_mov_f_pkr] = TOP_mov_t_pkr;
  CGTARG_Invert_Table[TOP_mov_t_pkr] = TOP_mov_f_pkr;

  CGTARG_Invert_Table[TOP_mov_f_pmc] = TOP_mov_t_pmc;
  CGTARG_Invert_Table[TOP_mov_t_pmc] = TOP_mov_f_pmc;

  CGTARG_Invert_Table[TOP_mov_f_pmd] = TOP_mov_t_pmd;
  CGTARG_Invert_Table[TOP_mov_t_pmd] = TOP_mov_f_pmd;

  CGTARG_Invert_Table[TOP_mov_f_rr]  = TOP_mov_t_rr;
  CGTARG_Invert_Table[TOP_mov_t_rr]  = TOP_mov_f_rr;

  CGTARG_Invert_Table[TOP_mov_f_pr]  = TOP_mov_t_pr;
  CGTARG_Invert_Table[TOP_mov_t_pr]  = TOP_mov_f_pr;

  CGTARG_Invert_Table[TOP_mov_f_psr] = TOP_mov_t_psr;
  CGTARG_Invert_Table[TOP_mov_t_psr] = TOP_mov_f_psr;

  CGTARG_Invert_Table[TOP_mov_f_psrum] = TOP_mov_t_psrum;
  CGTARG_Invert_Table[TOP_mov_t_psrum] = TOP_mov_f_psrum;

  /* Init table for CGTARG_Immed_To_Reg:
   */
  CGTARG_Immed_To_Reg_Table[TOP_addp4_i]  = TOP_addp4;
  CGTARG_Immed_To_Reg_Table[TOP_adds]     = TOP_add;
  CGTARG_Immed_To_Reg_Table[TOP_addl]     = TOP_add;
  CGTARG_Immed_To_Reg_Table[TOP_and_i]    = TOP_and;
  CGTARG_Immed_To_Reg_Table[TOP_andcm_i]  = TOP_andcm;
  CGTARG_Immed_To_Reg_Table[TOP_cmp_i_eq] = TOP_cmp_eq;
  CGTARG_Immed_To_Reg_Table[TOP_cmp_i_eq_unc] = TOP_cmp_eq_unc;
  CGTARG_Immed_To_Reg_Table[TOP_cmp_i_eq_and] = TOP_cmp_eq_and;
  CGTARG_Immed_To_Reg_Table[TOP_cmp_i_eq_or] = TOP_cmp_eq_or;
  CGTARG_Immed_To_Reg_Table[TOP_cmp_i_eq_or_andcm] = TOP_cmp_eq_or_andcm;
  CGTARG_Immed_To_Reg_Table[TOP_cmp_i_ne_and] = TOP_cmp_ne_and;
  CGTARG_Immed_To_Reg_Table[TOP_cmp_i_ne_or] = TOP_cmp_ne_or;
  CGTARG_Immed_To_Reg_Table[TOP_cmp_i_ne_or_andcm] = TOP_cmp_ne_or_andcm;
  CGTARG_Immed_To_Reg_Table[TOP_cmp_i_lt] = TOP_cmp_lt;
  CGTARG_Immed_To_Reg_Table[TOP_cmp_i_lt_unc] = TOP_cmp_lt_unc;
  CGTARG_Immed_To_Reg_Table[TOP_cmp_i_ltu] = TOP_cmp_ltu;
  CGTARG_Immed_To_Reg_Table[TOP_cmp_i_ltu_unc] = TOP_cmp_ltu_unc;
  CGTARG_Immed_To_Reg_Table[TOP_cmp_i_eq_orcm] = TOP_cmp_eq_orcm;
  CGTARG_Immed_To_Reg_Table[TOP_cmp_i_eq_andcm] = TOP_cmp_eq_andcm;
  CGTARG_Immed_To_Reg_Table[TOP_cmp_i_eq_and_orcm] = TOP_cmp_eq_and_orcm;
  CGTARG_Immed_To_Reg_Table[TOP_cmp_i_ne] = TOP_cmp_ne;
  CGTARG_Immed_To_Reg_Table[TOP_cmp_i_ne_unc] = TOP_cmp_ne_unc;
  CGTARG_Immed_To_Reg_Table[TOP_cmp_i_ne_orcm] = TOP_cmp_ne_orcm;
  CGTARG_Immed_To_Reg_Table[TOP_cmp_i_ne_andcm] = TOP_cmp_ne_andcm;
  CGTARG_Immed_To_Reg_Table[TOP_cmp_i_ne_and_orcm] = TOP_cmp_ne_and_orcm;
  CGTARG_Immed_To_Reg_Table[TOP_cmp_i_le] = TOP_cmp_le;
  CGTARG_Immed_To_Reg_Table[TOP_cmp_i_le_unc] = TOP_cmp_le_unc;
  CGTARG_Immed_To_Reg_Table[TOP_cmp_i_gt] = TOP_cmp_gt;
  CGTARG_Immed_To_Reg_Table[TOP_cmp_i_gt_unc] = TOP_cmp_gt_unc;
  CGTARG_Immed_To_Reg_Table[TOP_cmp_i_ge] = TOP_cmp_ge;
  CGTARG_Immed_To_Reg_Table[TOP_cmp_i_ge_unc] = TOP_cmp_ge_unc;
  CGTARG_Immed_To_Reg_Table[TOP_cmp_i_leu] = TOP_cmp_leu;
  CGTARG_Immed_To_Reg_Table[TOP_cmp_i_leu_unc] = TOP_cmp_leu_unc;
  CGTARG_Immed_To_Reg_Table[TOP_cmp_i_gtu] = TOP_cmp_gtu;
  CGTARG_Immed_To_Reg_Table[TOP_cmp_i_gtu_unc] = TOP_cmp_gtu_unc;
  CGTARG_Immed_To_Reg_Table[TOP_cmp_i_geu] = TOP_cmp_geu;
  CGTARG_Immed_To_Reg_Table[TOP_cmp_i_geu_unc] = TOP_cmp_geu_unc;
  CGTARG_Immed_To_Reg_Table[TOP_cmp4_i_eq] = TOP_cmp4_eq;
  CGTARG_Immed_To_Reg_Table[TOP_cmp4_i_eq_unc] = TOP_cmp4_eq_unc;
  CGTARG_Immed_To_Reg_Table[TOP_cmp4_i_eq_and] = TOP_cmp4_eq_and;
  CGTARG_Immed_To_Reg_Table[TOP_cmp4_i_eq_or] = TOP_cmp4_eq_or;
  CGTARG_Immed_To_Reg_Table[TOP_cmp4_i_eq_or_andcm] = TOP_cmp4_eq_or_andcm;
  CGTARG_Immed_To_Reg_Table[TOP_cmp4_i_ne_and] = TOP_cmp4_ne_and;
  CGTARG_Immed_To_Reg_Table[TOP_cmp4_i_ne_or] = TOP_cmp4_ne_or;
  CGTARG_Immed_To_Reg_Table[TOP_cmp4_i_ne_or_andcm] = TOP_cmp4_ne_or_andcm;
  CGTARG_Immed_To_Reg_Table[TOP_cmp4_i_lt] = TOP_cmp4_lt;
  CGTARG_Immed_To_Reg_Table[TOP_cmp4_i_lt_unc] = TOP_cmp4_lt_unc;
  CGTARG_Immed_To_Reg_Table[TOP_cmp4_i_ltu] = TOP_cmp4_ltu;
  CGTARG_Immed_To_Reg_Table[TOP_cmp4_i_ltu_unc] = TOP_cmp4_ltu_unc;
  CGTARG_Immed_To_Reg_Table[TOP_cmp4_i_eq_orcm] = TOP_cmp4_eq_orcm;
  CGTARG_Immed_To_Reg_Table[TOP_cmp4_i_eq_andcm] = TOP_cmp4_eq_andcm;
  CGTARG_Immed_To_Reg_Table[TOP_cmp4_i_eq_and_orcm] = TOP_cmp4_eq_and_orcm;
  CGTARG_Immed_To_Reg_Table[TOP_cmp4_i_ne] = TOP_cmp4_ne;
  CGTARG_Immed_To_Reg_Table[TOP_cmp4_i_ne_unc] = TOP_cmp4_ne_unc;
  CGTARG_Immed_To_Reg_Table[TOP_cmp4_i_ne_orcm] = TOP_cmp4_ne_orcm;
  CGTARG_Immed_To_Reg_Table[TOP_cmp4_i_ne_andcm] = TOP_cmp4_ne_andcm;
  CGTARG_Immed_To_Reg_Table[TOP_cmp4_i_ne_and_orcm] = TOP_cmp4_ne_and_orcm;
  CGTARG_Immed_To_Reg_Table[TOP_cmp4_i_le] = TOP_cmp4_le;
  CGTARG_Immed_To_Reg_Table[TOP_cmp4_i_le_unc] = TOP_cmp4_le_unc;
  CGTARG_Immed_To_Reg_Table[TOP_cmp4_i_gt] = TOP_cmp4_gt;
  CGTARG_Immed_To_Reg_Table[TOP_cmp4_i_gt_unc] = TOP_cmp4_gt_unc;
  CGTARG_Immed_To_Reg_Table[TOP_cmp4_i_ge] = TOP_cmp4_ge;
  CGTARG_Immed_To_Reg_Table[TOP_cmp4_i_ge_unc] = TOP_cmp4_ge_unc;
  CGTARG_Immed_To_Reg_Table[TOP_cmp4_i_leu] = TOP_cmp4_leu;
  CGTARG_Immed_To_Reg_Table[TOP_cmp4_i_leu_unc] = TOP_cmp4_leu_unc;
  CGTARG_Immed_To_Reg_Table[TOP_cmp4_i_gtu] = TOP_cmp4_gtu;
  CGTARG_Immed_To_Reg_Table[TOP_cmp4_i_gtu_unc] = TOP_cmp4_gtu_unc;
  CGTARG_Immed_To_Reg_Table[TOP_cmp4_i_geu] = TOP_cmp4_geu;
  CGTARG_Immed_To_Reg_Table[TOP_cmp4_i_geu_unc] = TOP_cmp4_geu_unc;
  CGTARG_Immed_To_Reg_Table[TOP_ld1_i]    = TOP_ld1_r;
  CGTARG_Immed_To_Reg_Table[TOP_ld2_i]    = TOP_ld2_r;
  CGTARG_Immed_To_Reg_Table[TOP_ld4_i]    = TOP_ld4_r;
  CGTARG_Immed_To_Reg_Table[TOP_ld8_i]    = TOP_ld8_r;
  CGTARG_Immed_To_Reg_Table[TOP_ld8_i_fill] = TOP_ld8_r_fill;
  CGTARG_Immed_To_Reg_Table[TOP_ldfs_i]   = TOP_ldfs_r;
  CGTARG_Immed_To_Reg_Table[TOP_ldfd_i]   = TOP_ldfd_r;
  CGTARG_Immed_To_Reg_Table[TOP_ldfe_i]   = TOP_ldfe_r;
  CGTARG_Immed_To_Reg_Table[TOP_ldf8_i]   = TOP_ldf8_r;
  CGTARG_Immed_To_Reg_Table[TOP_ldf_i_fill] = TOP_ldf_r_fill;
  CGTARG_Immed_To_Reg_Table[TOP_lfetch_i] = TOP_lfetch_r;
  CGTARG_Immed_To_Reg_Table[TOP_lfetch_i_excl] = TOP_lfetch_r_excl;
  CGTARG_Immed_To_Reg_Table[TOP_lfetch_i_fault] = TOP_lfetch_r_fault;
  CGTARG_Immed_To_Reg_Table[TOP_lfetch_i_fault_excl] = TOP_lfetch_r_fault_excl;
  CGTARG_Immed_To_Reg_Table[TOP_mov_t_ar_i] = TOP_mov_t_ar_r;
  CGTARG_Immed_To_Reg_Table[TOP_or_i]     = TOP_or;
  CGTARG_Immed_To_Reg_Table[TOP_probe_i_r] = TOP_probe_r;
  CGTARG_Immed_To_Reg_Table[TOP_probe_i_w] = TOP_probe_w;
  CGTARG_Immed_To_Reg_Table[TOP_pshl2_i]  = TOP_pshl2;
  CGTARG_Immed_To_Reg_Table[TOP_pshl4_i]  = TOP_pshl4;
  CGTARG_Immed_To_Reg_Table[TOP_pshr2_i]  = TOP_pshr2;
  CGTARG_Immed_To_Reg_Table[TOP_pshr2_i_u] = TOP_pshr2_u;
  CGTARG_Immed_To_Reg_Table[TOP_pshr4_i]  = TOP_pshr4;
  CGTARG_Immed_To_Reg_Table[TOP_pshr4_i_u] = TOP_pshr4_u;
  CGTARG_Immed_To_Reg_Table[TOP_shl_i]    = TOP_shl;
  CGTARG_Immed_To_Reg_Table[TOP_shr_i]    = TOP_shr;
  CGTARG_Immed_To_Reg_Table[TOP_shr_i_u]  = TOP_shr_u;
  CGTARG_Immed_To_Reg_Table[TOP_sub_i]    = TOP_sub;
  CGTARG_Immed_To_Reg_Table[TOP_xor_i]    = TOP_xor;

  /* Init table for CGTARG_Inter_RegClass_Copy_Table:
   */
  CGTARG_Inter_RegClass_Copy_Table[ISA_REGISTER_CLASS_float]
				  [ISA_REGISTER_CLASS_integer]
				  [FALSE] = TOP_getf_s;
  CGTARG_Inter_RegClass_Copy_Table[ISA_REGISTER_CLASS_float]
				  [ISA_REGISTER_CLASS_integer]
				  [TRUE]  = TOP_getf_d;
  CGTARG_Inter_RegClass_Copy_Table[ISA_REGISTER_CLASS_application]
				  [ISA_REGISTER_CLASS_integer]
				  [FALSE] = TOP_mov_f_ar;
  CGTARG_Inter_RegClass_Copy_Table[ISA_REGISTER_CLASS_branch]
				  [ISA_REGISTER_CLASS_integer]
				  [FALSE] = TOP_mov_f_br;
  CGTARG_Inter_RegClass_Copy_Table[ISA_REGISTER_CLASS_control]
				  [ISA_REGISTER_CLASS_integer]
				  [FALSE] = TOP_mov_f_cr;

  CGTARG_Inter_RegClass_Copy_Table[ISA_REGISTER_CLASS_integer]
				  [ISA_REGISTER_CLASS_float]
				  [FALSE] = TOP_setf_s;
  CGTARG_Inter_RegClass_Copy_Table[ISA_REGISTER_CLASS_integer]
				  [ISA_REGISTER_CLASS_float]
				  [TRUE]  = TOP_setf_d;
  CGTARG_Inter_RegClass_Copy_Table[ISA_REGISTER_CLASS_integer]
				  [ISA_REGISTER_CLASS_application]
				  [FALSE] = TOP_mov_t_ar_r;
  CGTARG_Inter_RegClass_Copy_Table[ISA_REGISTER_CLASS_integer]
				  [ISA_REGISTER_CLASS_branch]
				  [FALSE] = TOP_mov_t_br;
  CGTARG_Inter_RegClass_Copy_Table[ISA_REGISTER_CLASS_integer]
				  [ISA_REGISTER_CLASS_control]
				  [FALSE] = TOP_mov_t_cr;

  // Determine the f0_unit
  CGTARG_f0_unit_id = (SI_RESOURCE_ID) -1;
  for (SI_RESOURCE_ID res_id = 0; res_id < SI_resource_count; ++res_id) {
    if (strcmp("floating-point0", SI_RESOURCE_ID_Name(res_id)) == 0)
      CGTARG_f0_unit_id = res_id;
  }
}


/* ====================================================================
 *
 * CGTARG_Load_From_Memory
 *
 * See interface description
 *
 * ====================================================================
 */
void CGTARG_Load_From_Memory(TN *tn, ST *mem_loc, OPS *ops)
{
  TYPE_ID mtype = TY_mtype(ST_type(mem_loc));

  if (TN_register_class(tn) == ISA_REGISTER_CLASS_predicate) {
    /* Since we can't directly load a predicate TN, first load into
     * an integer register and then set the predicate by checking for
     * a non-zero value.
     */
    TN *temp_tn = Build_TN_Of_Mtype (MTYPE_I8);
    Exp_Load (mtype, mtype, temp_tn, mem_loc, 0, ops, V_NONE);
    Build_OP (TOP_cmp_i_ne, tn, True_TN, 
	      True_TN, Gen_Literal_TN(0, 4), temp_tn, ops);
  }
  else if (TN_register_class(tn) == ISA_REGISTER_CLASS_branch) {
	// first load into an integer register and then copy.
    	TN *temp_tn = Build_TN_Of_Mtype (MTYPE_I8);
    	Exp_Load (mtype, mtype, temp_tn, mem_loc, 0, ops, V_NONE);
    	Build_OP (TOP_mov_t_br, tn, True_TN, temp_tn, ops);
  } 
  else {

    /* Non-predicate TNs are just a simple load.
     */
    Exp_Load (mtype, mtype, tn, mem_loc, 0, ops, V_NONE);
  }
}


/* ====================================================================
 *
 * CGTARG_Store_To_Memory
 *
 * See interface description
 *
 * ====================================================================
 */
void CGTARG_Store_To_Memory(TN *tn, ST *mem_loc, OPS *ops)
{
  TYPE_ID mtype = TY_mtype(ST_type(mem_loc));

  if (TN_register_class(tn) == ISA_REGISTER_CLASS_predicate) {
    /* Since we can't directly store a predicate TN, first copy to
     * an integer register and then store.
     */
    TN *temp_tn = Build_TN_Of_Mtype (MTYPE_I8);
    Build_OP (TOP_mov, temp_tn, True_TN, Zero_TN, ops);
    Build_OP (TOP_mov_i, temp_tn, tn, Gen_Literal_TN(1, 4), ops);
    Exp_Store (mtype, temp_tn, mem_loc, 0, ops, V_NONE);
  }
  else if (TN_register_class(tn) == ISA_REGISTER_CLASS_branch) {
	// first copy to an integer register and then store.
    	TN *temp_tn = Build_TN_Of_Mtype (MTYPE_I8);
    	Build_OP (TOP_mov_f_br, temp_tn, True_TN, tn, ops);
    	Exp_Store (mtype, temp_tn, mem_loc, 0, ops, V_NONE);
  } 
  else {

    /* Non-predicate TNs are just a simple store.
     */
    Exp_Store (mtype, tn, mem_loc, 0, ops, V_NONE);
  }
}


/* ====================================================================
 *
 * CGTARG_Init_Assoc_Base
 *
 * See interface description
 *
 * ====================================================================
 */
void CGTARG_Init_Assoc_Base(void)
{
  INT i;
  OPCODE * const oprs = CGTARG_Assoc_Base_Opr_Table;
  mTOP * const fncs = CGTARG_Assoc_Base_Fnc_Table;
  mTOP * const tops = CGTARG_Assoc_Base_Top_Table;

  for(i = 0; i < TOP_count; i++) {
    oprs[i] = (OPCODE)0;
    fncs[i] = ASSOC_BASE_null;
    tops[i] = TOP_UNDEFINED;
  }

  /* Trivial cases
   */
  oprs[TOP_add]    = OPC_U4ADD;		tops[TOP_add]     = TOP_add;
  oprs[TOP_addl]   = OPC_U4ADD;		tops[TOP_addl]    = TOP_add;
  oprs[TOP_adds]   = OPC_U4ADD;		tops[TOP_adds]    = TOP_add;
  oprs[TOP_or]     = OPC_U8BIOR;	tops[TOP_or]      = TOP_or;
  oprs[TOP_or_i]   = OPC_U8BIOR;	tops[TOP_or_i]    = TOP_or;
  oprs[TOP_and]    = OPC_U8BAND;	tops[TOP_and]     = TOP_and;
  oprs[TOP_and_i]  = OPC_U8BAND;	tops[TOP_and_i]   = TOP_and;
  oprs[TOP_xor]    = OPC_U8BXOR;	tops[TOP_xor]     = TOP_xor;
  oprs[TOP_xor_i]  = OPC_U8BXOR;	tops[TOP_xor_i]   = TOP_xor;
  oprs[TOP_sub]    = OPC_U4ADD;		tops[TOP_sub]     = TOP_add;
					fncs[TOP_sub]     = ASSOC_BASE_inverse;

  if (Roundoff_Level >= ROUNDOFF_ASSOC) {

    /* Most FP ops can't be radically reassociated
     * without changing the roundoff error.
     */
    oprs[TOP_fadd_s] = OPC_F4ADD;	tops[TOP_fadd_s]  = TOP_fadd_s;
    oprs[TOP_fadd_d] = OPC_F8ADD;	tops[TOP_fadd_d]  = TOP_fadd_d;
    oprs[TOP_fadd  ] = OPC_F10ADD;	tops[TOP_fadd  ]  = TOP_fadd  ;
    oprs[TOP_fmpy_s] = OPC_F4MPY;	tops[TOP_fmpy_s]  = TOP_fmpy_s;
    oprs[TOP_fmpy_d] = OPC_F8MPY;	tops[TOP_fmpy_d]  = TOP_fmpy_d;
    oprs[TOP_fmpy  ] = OPC_F10MPY;	tops[TOP_fmpy  ]  = TOP_fmpy  ;

    /* We can reassociate the adds in mult-adds, but we'll
     * need to treat these specially.
     */
    oprs[TOP_fma_s]  = OPC_F4ADD;	tops[TOP_fma_s]   = TOP_fadd_s;
    oprs[TOP_fma_d]  = OPC_F8ADD;	tops[TOP_fma_d]   = TOP_fadd_d;
    oprs[TOP_fma  ]  = OPC_F10ADD;	tops[TOP_fma  ]   = TOP_fadd  ;

    /* These operations can be equivalent to the various trivial
     * cases, but they require special treatment since they're the
     * the inverse of associative operators, and not associative
     * themselves.
     */
    oprs[TOP_fsub_s]  = OPC_F4ADD;	tops[TOP_fsub_s]  = TOP_fadd_s;
					fncs[TOP_fsub_s]  = ASSOC_BASE_inverse;
    oprs[TOP_fsub_d]  = OPC_F8ADD;	tops[TOP_fsub_d]  = TOP_fadd_d;
					fncs[TOP_fsub_d]  = ASSOC_BASE_inverse;
    oprs[TOP_fsub  ]  = OPC_F10ADD;	tops[TOP_fsub  ]  = TOP_fadd;
					fncs[TOP_fsub  ]  = ASSOC_BASE_inverse;
    oprs[TOP_fnma_s]  = OPC_F4ADD;	tops[TOP_fnma_s]  = TOP_fadd_s;
					fncs[TOP_fnma_s]  = ASSOC_BASE_inverse;
    oprs[TOP_fnma_d]  = OPC_F8ADD;	tops[TOP_fnma_d]  = TOP_fadd_d;
					fncs[TOP_fnma_d]  = ASSOC_BASE_inverse;
    oprs[TOP_fnma  ]  = OPC_F10ADD;	tops[TOP_fnma  ]  = TOP_fadd;
					fncs[TOP_fnma  ]  = ASSOC_BASE_inverse;
  }

  /* Compound operator cases.  Each operator consists of more than
   * one machine op.  The function checks for the other ops and the
   * correct relationship.
   */
  oprs[TOP_xma_l]   = OPC_I8MPY;	tops[TOP_xma_l]   = TOP_xma_l;
					fncs[TOP_xma_l]   = ASSOC_BASE_imul;
  oprs[TOP_xma_lu]  = OPC_U8MPY;	tops[TOP_xma_lu]  = TOP_xma_lu;
					fncs[TOP_xma_lu]  = ASSOC_BASE_imul;
}


/* ====================================================================
 *
 * CGTARG_Copy_Operand
 *
 * See interface description
 *
 * ====================================================================
 */
INT CGTARG_Copy_Operand(OP *op)
{
  TOP opr = OP_code(op);
  switch (opr) {

  // NOTE: TOP_fandcm, TOP_for, and TOP_fxor could be handled like
  // their integer counterparts should that ever become useful.

  case TOP_adds:
  case TOP_addl:
  case TOP_or_i:
  case TOP_xor_i:
    if (TN_has_value(OP_opnd(op,1)) && TN_value(OP_opnd(op,1)) == 0) {
      return 2;
    }
    break;

  case TOP_add:
  case TOP_or:
  case TOP_xor:
    if (TN_register_and_class(OP_opnd(op,2)) == CLASS_AND_REG_zero) {
      return 1;
    }
    if (TN_register_and_class(OP_opnd(op,1)) == CLASS_AND_REG_zero) {
      return 2;
    }
    break;

  case TOP_andcm:
  case TOP_sub:
    if (TN_register_and_class(OP_opnd(op,2)) == CLASS_AND_REG_zero) {
      return 1;
    }
    break;

  case TOP_and_i:
    if (TN_has_value(OP_opnd(op,1)) && TN_value(OP_opnd(op,1)) == -1) {
      return 2;
    }
    break;

  case TOP_extr:
  case TOP_extr_u:
    if (   TN_has_value(OP_opnd(op,2)) && TN_value(OP_opnd(op,2)) == 0
	&& TN_has_value(OP_opnd(op,3)) && TN_value(OP_opnd(op,3)) == 64)
    {
      return 1;
    }
    break;

  case TOP_shl_i:
  case TOP_shr_i:
  case TOP_shr_i_u: 
    if (   (TN_register_and_class(OP_opnd(op,1)) == CLASS_AND_REG_zero)
	|| (TN_has_value(OP_opnd(op,2)) && TN_value(OP_opnd(op,2)) == 0))
    {
      return 1;
    }
    break;

  case TOP_shl:
  case TOP_shr: 
  case TOP_shr_u: 
    if (   (TN_register_and_class(OP_opnd(op,1)) == CLASS_AND_REG_zero)
	|| (TN_register_and_class(OP_opnd(op,2)) == CLASS_AND_REG_zero))
    {
      return 1;
    }
    break;

  case TOP_mov:
  case TOP_mov_f:
  case TOP_copy_br:
	return 1;
  }
  return -1;
}


/* ====================================================================
 *
 * CGTARG_Can_Fit_Immediate_In_Add_Instruction
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL
CGTARG_Can_Fit_Immediate_In_Add_Instruction (INT64 immed)
{
	return ISA_LC_Value_In_Class (immed, LC_i14);
}


/* ====================================================================
 *
 * CGTARG_Can_Load_Immediate_In_Single_Instruction
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL
CGTARG_Can_Load_Immediate_In_Single_Instruction (INT64 immed)
{
	return ISA_LC_Value_In_Class (immed, LC_i22);
}



/* ====================================================================
 *
 * CGTARG_Predicate_OP
 *
 * See interface description
 *
 * ====================================================================
 */
/*ARGSUSED*/
void
CGTARG_Predicate_OP(BB* bb, OP* op, TN* pred_tn)
{
  if (OP_has_predicate(op)) {
    Set_OP_opnd(op, OP_PREDICATE_OPND, pred_tn);
  }
}

/* ====================================================================
 *
 * CGTARG_Branches_On_True
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL
CGTARG_Branches_On_True(OP* br_op, OP* cmp_op)
{
  if (OP_opnd(br_op, OP_PREDICATE_OPND) == OP_result(cmp_op, 0)) {
    return TRUE;
  }
  return FALSE;
}



/* ====================================================================
 *
 * CGTARG_Parallel_Compare
 *
 * See interface description
 *
 * ====================================================================
 */
TOP
CGTARG_Parallel_Compare(OP* cmp_op, COMPARE_TYPE ctype)
{
  //
  // None for floats.  For gp registers, only support "eq" and "ne"
  // for arbitrary register compares.  Others must have one operand
  // be the zero register.
  //
  if (OP_flop(cmp_op)) {
    return TOP_UNDEFINED;
  }

  TOP cmp_top = OP_code(cmp_op);
  if (OP_opnd(cmp_op, 1) != Zero_TN && OP_opnd(cmp_op, 2) != Zero_TN) {
    switch (cmp_top) {
    case TOP_cmp_ne:
    case TOP_cmp4_ne:
    case TOP_cmp_i_ne:
    case TOP_cmp4_i_eq:
    case TOP_cmp4_eq:
    case TOP_cmp_i_eq:
    case TOP_cmp_eq:
      break;
    default:
      return TOP_UNDEFINED;
    }
  }
  switch (cmp_top) {
  case TOP_cmp_ne:
  case TOP_cmp_ne_unc:
    switch (ctype) {
    case COMPARE_TYPE_or:
      return TOP_cmp_ne_or;
    case COMPARE_TYPE_orcm:
      return TOP_cmp_ne_orcm;
    case COMPARE_TYPE_and:
      return TOP_cmp_ne_and;
    case COMPARE_TYPE_andcm:
      return TOP_cmp_ne_andcm;
    case COMPARE_TYPE_or_andcm:
      return TOP_cmp_ne_or_andcm;
    case COMPARE_TYPE_and_orcm:
      return TOP_cmp_ne_and_orcm;
    }

  case TOP_cmp4_ne:
  case TOP_cmp4_ne_unc:
    switch (ctype) {
    case COMPARE_TYPE_or:
      return TOP_cmp4_ne_or;
    case COMPARE_TYPE_orcm:
      return TOP_cmp4_ne_orcm;
    case COMPARE_TYPE_and:
      return TOP_cmp4_ne_and;
    case COMPARE_TYPE_andcm:
      return TOP_cmp4_ne_andcm;
    case COMPARE_TYPE_or_andcm:
      return TOP_cmp4_ne_or_andcm;
    case COMPARE_TYPE_and_orcm:
      return TOP_cmp4_ne_and_orcm;
    }

  case TOP_cmp_i_ne:
    switch (ctype) {
    case COMPARE_TYPE_or:
      return TOP_cmp_i_ne_or;
    case COMPARE_TYPE_orcm:
      return TOP_cmp_i_ne_orcm;
    case COMPARE_TYPE_and:
      return TOP_cmp_i_ne_and;
    case COMPARE_TYPE_andcm:
      return TOP_cmp_i_ne_andcm;
    case COMPARE_TYPE_or_andcm:
      return TOP_cmp_i_ne_or_andcm;
    }

 case TOP_cmp4_ge:
 case TOP_cmp4_ge_unc:
    switch (ctype) {
    case COMPARE_TYPE_or:
      return TOP_cmp4_ge_or;
    case COMPARE_TYPE_orcm:
      return TOP_cmp4_ge_orcm;
    case COMPARE_TYPE_and:
      return TOP_cmp4_ge_and;
    case COMPARE_TYPE_andcm:
      return TOP_cmp4_ge_andcm;
    case COMPARE_TYPE_or_andcm:
      return TOP_cmp4_ge_or_andcm;
    }

 case TOP_cmp4_gt:
 case TOP_cmp4_gt_unc:
    switch (ctype) {
    case COMPARE_TYPE_or:
      return TOP_cmp4_gt_or;
    case COMPARE_TYPE_orcm:
      return TOP_cmp4_gt_orcm;
    case COMPARE_TYPE_and:
      return TOP_cmp4_gt_and;
    case COMPARE_TYPE_andcm:
      return TOP_cmp4_gt_andcm;
    case COMPARE_TYPE_or_andcm:
      return TOP_cmp4_gt_or_andcm;
    }

 case TOP_cmp4_le:
 case TOP_cmp4_le_unc:
    switch (ctype) {
    case COMPARE_TYPE_or:
      return TOP_cmp4_le_or;
    case COMPARE_TYPE_orcm:
      return TOP_cmp4_le_orcm;
    case COMPARE_TYPE_and:
      return TOP_cmp4_le_and;
    case COMPARE_TYPE_andcm:
      return TOP_cmp4_le_andcm;
    case COMPARE_TYPE_or_andcm:
      return TOP_cmp4_le_or_andcm;
    }

 case TOP_cmp4_lt:
 case TOP_cmp4_lt_unc:
    switch (ctype) {
    case COMPARE_TYPE_or:
      return TOP_cmp4_lt_or;
    case COMPARE_TYPE_orcm:
      return TOP_cmp4_lt_orcm;
    case COMPARE_TYPE_and:
      return TOP_cmp4_lt_and;
    case COMPARE_TYPE_andcm:
      return TOP_cmp4_lt_andcm;
    case COMPARE_TYPE_or_andcm:
      return TOP_cmp4_lt_or_andcm;
    }

 case TOP_cmp_ge:
 case TOP_cmp_ge_unc:
    switch (ctype) {
    case COMPARE_TYPE_or:
      return TOP_cmp_ge_or;
    case COMPARE_TYPE_orcm:
      return TOP_cmp_ge_orcm;
    case COMPARE_TYPE_and:
      return TOP_cmp_ge_and;
    case COMPARE_TYPE_andcm:
      return TOP_cmp_ge_andcm;
    case COMPARE_TYPE_or_andcm:
      return TOP_cmp_ge_or_andcm;
    }

 case TOP_cmp_gt:
 case TOP_cmp_gt_unc:
    switch (ctype) {
    case COMPARE_TYPE_or:
      return TOP_cmp_gt_or;
    case COMPARE_TYPE_orcm:
      return TOP_cmp_gt_orcm;
    case COMPARE_TYPE_and:
      return TOP_cmp_gt_and;
    case COMPARE_TYPE_andcm:
      return TOP_cmp_gt_andcm;
    case COMPARE_TYPE_or_andcm:
      return TOP_cmp_gt_or_andcm;
    }

 case TOP_cmp_le:
 case TOP_cmp_le_unc:
    switch (ctype) {
    case COMPARE_TYPE_or:
      return TOP_cmp_le_or;
    case COMPARE_TYPE_orcm:
      return TOP_cmp_le_orcm;
    case COMPARE_TYPE_and:
      return TOP_cmp_le_and;
    case COMPARE_TYPE_andcm:
      return TOP_cmp_le_andcm;
    case COMPARE_TYPE_or_andcm:
      return TOP_cmp_le_or_andcm;
    }

 case TOP_cmp_lt:
 case TOP_cmp_lt_unc:
    switch (ctype) {
    case COMPARE_TYPE_or:
      return TOP_cmp_lt_or;
    case COMPARE_TYPE_orcm:
      return TOP_cmp_lt_orcm;
    case COMPARE_TYPE_and:
      return TOP_cmp_lt_and;
    case COMPARE_TYPE_andcm:
      return TOP_cmp_lt_andcm;
    case COMPARE_TYPE_or_andcm:
      return TOP_cmp_lt_or_andcm;
    }

 case TOP_cmp4_i_eq:
 case TOP_cmp4_i_eq_unc:
    switch (ctype) {
    case COMPARE_TYPE_or:
      return TOP_cmp4_i_eq_or;
    case COMPARE_TYPE_orcm:
      return TOP_cmp4_i_eq_orcm;
    case COMPARE_TYPE_and:
      return TOP_cmp4_i_eq_and;
    case COMPARE_TYPE_andcm:
      return TOP_cmp4_i_eq_andcm;
    case COMPARE_TYPE_or_andcm:
      return TOP_cmp4_i_eq_or_andcm;
    }


 case TOP_cmp4_eq:
 case TOP_cmp4_eq_unc:
    switch (ctype) {
    case COMPARE_TYPE_or:
      return TOP_cmp4_eq_or;
    case COMPARE_TYPE_orcm:
      return TOP_cmp4_eq_orcm;
    case COMPARE_TYPE_and:
      return TOP_cmp4_eq_and;
    case COMPARE_TYPE_andcm:
      return TOP_cmp4_eq_andcm;
    case COMPARE_TYPE_or_andcm:
      return TOP_cmp4_eq_or_andcm;
    }

 case TOP_cmp_i_eq:
 case TOP_cmp_i_eq_unc:
    switch (ctype) {
    case COMPARE_TYPE_or:
      return TOP_cmp_i_eq_or;
    case COMPARE_TYPE_orcm:
      return TOP_cmp_i_eq_orcm;
    case COMPARE_TYPE_and:
      return TOP_cmp_i_eq_and;
    case COMPARE_TYPE_andcm:
      return TOP_cmp_i_eq_andcm;
    case COMPARE_TYPE_or_andcm:
      return TOP_cmp_i_eq_or_andcm;
    }

 case TOP_cmp_eq:
 case TOP_cmp_eq_unc:
    switch (ctype) {
    case COMPARE_TYPE_or:
      return TOP_cmp_eq_or;
    case COMPARE_TYPE_orcm:
      return TOP_cmp_eq_orcm;
    case COMPARE_TYPE_and:
      return TOP_cmp_eq_and;
    case COMPARE_TYPE_andcm:
      return TOP_cmp_eq_andcm;
    case COMPARE_TYPE_or_andcm:
      return TOP_cmp_eq_or_andcm;
    }

  }

  return TOP_UNDEFINED;
}

BOOL
OP_def_use_stack_regs(OP* op)
{
  for (INT i = 0; i < OP_results(op); i++) {
    TN *tn = OP_result(op,i);
    if (TN_is_register(tn) &&
        REGISTER_Is_Stacked(TN_register_class(tn), TN_register(tn))) {
      return TRUE;
    }
  }

  for (INT i = 0; i < OP_opnds(op); i++) {
    TN* tn = OP_opnd(op,i);
    if (TN_is_register(tn) &&
        REGISTER_Is_Stacked(TN_register_class(tn), TN_register(tn))) {
      return TRUE;
    }
  }

  return FALSE;
}

BOOL
OP_use_sp(OP* op)
{
  for (INT i = 0; i < OP_opnds(op); i++) {
    TN* tn = OP_opnd(op,i);
    if (TN_is_register(tn) &&
        TN_is_sp_reg(tn)) {
      return TRUE;
    }
  }

  return FALSE;
}

/* ====================================================================
 *
 * CGTARG_Dependence_Required
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL
CGTARG_Dependence_Required(OP *pred_op, OP *succ_op)
{

  BOOL read_write_rotating_all,    // all TOPs which read/write rotating regs
    read_write_rotating_predicate, // all TOPs which read/write rotating 
                                   // predicate regs
    write_rotating_predicate,      // all TOPs which write rotating predicate
                                   // regs;
    read_write_predicate,	   // all TOPs which read/write predicate regs
    write_predicate,		   // all TOPs which write predicate regs
    read_write_status_field,       // all TOPs which read/write a specific
                                   // status field regs
    read_write_unat ;              // read/write unat register     

  read_write_rotating_all = FALSE;
  read_write_rotating_predicate = FALSE;
  write_rotating_predicate = FALSE;
  read_write_predicate = FALSE;
  write_predicate = FALSE;
  read_write_status_field = FALSE;
  read_write_unat = FALSE ;

  OP *hazard_op = NULL;

  // The dependences that need to be preserved are:

  // (1) clrrrb - change the rotating register base
  // read/write to all rotating registers cannot move above/below
  // this instruction.

  // (2) mov pr=r2,mask
  //     mov pr.rot=imm44
  // read/write to rotating predicate registers (ONLY) cannot move above/
  // below these instructions.

  // (3) mov r1=pr
  // write to rotating predicate registers (ONLY) cannot move above/below
  // these instructions.

  // (4) fsetc.xx
  // reads/writes to status field register 'xx' cannot move above/below
  // this instruction.

  switch (OP_code(pred_op)) {
  case TOP_alloc:
    hazard_op = pred_op;
    break;
  case TOP_spadjust:
    hazard_op = pred_op;
    break;
  case TOP_clrrrb:
  case TOP_clrrrb_pr:
    read_write_rotating_all = TRUE;
    hazard_op = pred_op;
    break;
  case TOP_mov_t_pr_i:
    read_write_rotating_predicate = TRUE;
    hazard_op = pred_op;
    break;
  case TOP_mov_t_pr:
    read_write_rotating_predicate = TRUE;
    read_write_predicate = TRUE;	// mov_t_pr restores all preds
    hazard_op = pred_op;
    break;
  case TOP_mov_f_pr:
    write_rotating_predicate = TRUE;
    write_predicate = TRUE;		// mov_f_pr saves all preds
    hazard_op = pred_op;
    break;
  case TOP_fsetc:
    read_write_status_field = TRUE;
    hazard_op = pred_op;
    break;

  case TOP_st8_spill:
  case TOP_mov_f_ar:
  case TOP_mov_f_ar_m:
  case TOP_ld8_fill:
  case TOP_mov_t_ar_r:
  case TOP_mov_t_ar_r_m:
    read_write_unat = TRUE ;
    hazard_op = pred_op ;
    break ;

  default:
    switch (OP_code(succ_op)) {
    case TOP_spadjust:
      hazard_op = succ_op;
      break;
    case TOP_clrrrb:
    case TOP_clrrrb_pr:
      read_write_rotating_all = TRUE;
      hazard_op = succ_op;
      break;
    case TOP_mov_t_pr_i:
      read_write_rotating_predicate = TRUE;
      hazard_op = succ_op;
      break;
    case TOP_mov_t_pr:
      read_write_rotating_predicate = TRUE;
      read_write_predicate = TRUE;
      hazard_op = succ_op;
      break;
    case TOP_mov_f_pr:
      write_rotating_predicate = TRUE;
      write_predicate = TRUE;
      hazard_op = succ_op;
      break;
    case TOP_fsetc:
      read_write_status_field = TRUE;
      hazard_op = succ_op;
      break;
    }
    break;
  }

  if (hazard_op) {
    OP *other_op = (hazard_op == pred_op) ? succ_op : pred_op;

    if ((OP_code(hazard_op) == TOP_alloc && OP_def_use_stack_regs(other_op)) ||
        (OP_code(hazard_op) == TOP_spadjust && OP_use_sp(other_op)))
      return TRUE;

    // read/write unat 
    if (read_write_unat) {
        mTOP hazard_op_code = OP_code (hazard_op) ;
        mTOP other_op_code  = OP_code (other_op) ;

        if (hazard_op_code == TOP_mov_f_ar_m) hazard_op_code = TOP_mov_f_ar;
        if (hazard_op_code == TOP_mov_t_ar_r_m) hazard_op_code = TOP_mov_t_ar_r;
        if (other_op_code == TOP_mov_f_ar_m) other_op_code = TOP_mov_f_ar;
        if (other_op_code == TOP_mov_t_ar_r_m) other_op_code = TOP_mov_t_ar_r;

        if (hazard_op_code == TOP_st8_spill  && other_op_code == TOP_mov_f_ar  || 
            hazard_op_code == TOP_mov_f_ar   && other_op_code == TOP_st8_spill || 
            hazard_op_code == TOP_ld8_fill   && other_op_code == TOP_st8_spill || 
            hazard_op_code == TOP_st8_spill  && other_op_code == TOP_ld8_fill || 
            hazard_op_code == TOP_ld8_fill  && other_op_code == TOP_ld8_fill || 
            hazard_op_code == TOP_st8_spill  && other_op_code == TOP_st8_spill || 
            hazard_op_code == TOP_mov_t_ar_r && other_op_code == TOP_ld8_fill  ||
            hazard_op_code == TOP_ld8_fill   && other_op_code == TOP_mov_t_ar_r)
        {
            return TRUE;               
        }
    }

    // Patch for a failed assertion in
    // GRA_Remove_Predicates_Save_Restore().
    // There isn't any arc between a spadjust and a mov_f_pr
    // or mov_t_pr. So scheduler may move mov_f_pr forward passing
    // spadjust or move spajust forward passing mov_t_pr. Both will
    // cause the assertion fail. Don't know why the old scheduler
    // doesn't have the problem.
    if ((read_write_rotating_predicate ||
        read_write_predicate ||
        write_rotating_predicate ||
        write_predicate) && OP_code(other_op) == TOP_spadjust)
      return TRUE;

    // Special case clrrrb instructions first since it implicitly 
    // reads/writes all rotating regs.

    if ((read_write_rotating_all || 
	 read_write_rotating_predicate ||
	 write_rotating_predicate) && OP_access_reg_bank(other_op))
      return TRUE;

    // Special case other instruction being an fsetc instruction as 
    // it has an implicit read of .s0 

    TN *status_field; // the status field reg written by hazard_op
    if (read_write_status_field) {
      status_field = OP_opnd(hazard_op, 1);
      if (OP_code(other_op) == TOP_fsetc &&
	  TN_enum(status_field) == ECV_sf_s0) return TRUE;
    }

    INT k;
    for (k = 0; k < OP_opnds(other_op); k++) {
      TN *opnd_tn = OP_opnd(other_op, k);

      if (TN_is_constant(opnd_tn)) {
	if (read_write_status_field &&
	    TN_is_enum(opnd_tn) &&
	    TN_enum(opnd_tn) == TN_enum(status_field)) return TRUE;
	continue;
      }

      if (TN_is_const_reg(opnd_tn)) continue;

      REGISTER reg = TN_register(opnd_tn);
      ISA_REGISTER_CLASS reg_class = TN_register_class(opnd_tn);

      if (read_write_predicate &&
	  reg_class == ISA_REGISTER_CLASS_predicate) return TRUE;

      if (reg == REGISTER_UNDEFINED) continue;

      if (read_write_rotating_all && 
	  REGISTER_Is_Rotating(reg_class, reg)) return TRUE;

      if (read_write_rotating_predicate &&
	  reg_class == ISA_REGISTER_CLASS_predicate &&
	  REGISTER_Is_Rotating(reg_class, reg)) return TRUE;
    }

    for (k = 0; k < OP_results(other_op); k++) {
      TN *result_tn = OP_result(other_op, k);

      if (TN_is_const_reg(result_tn)) continue;

      REGISTER reg = TN_register(result_tn);
      ISA_REGISTER_CLASS reg_class = TN_register_class(result_tn);

      // can have conflict with op that writes to predicate,
      // even though haven't assigned registers yet.
      if (write_predicate &&
	   reg_class == ISA_REGISTER_CLASS_predicate) return TRUE;

      if (reg == REGISTER_UNDEFINED) continue;

      if (read_write_rotating_all &&
	  REGISTER_Is_Rotating(reg_class, reg)) return TRUE;

      if (((read_write_rotating_predicate || write_rotating_predicate) &&
	   reg_class == ISA_REGISTER_CLASS_predicate) &&
	  REGISTER_Is_Rotating(reg_class, reg)) return TRUE;
    }
  }

  // The following descriptions below present target-specific ordering
  // constraints that must be respected. They can't fit in the same
  // instruction group.
  //
  // alloc
  //
  // WITH ANY OF THE BELOW
  //
  // flushrs, 
  // move from ar.bpstore, 
  // move from ar.rnat, 
  // br.cexit, 
  // br.ctop,
  // br.wexit, 
  // br.wtop, 
  // br.call, 
  // br.ia, 
  // br.ret,
  // clrrrb,
  // cover,
  // rfi;

  if (OP_code(pred_op)  == TOP_alloc &&
      (OP_code(succ_op) == TOP_flushrs ||
       OP_code(succ_op) == TOP_br_cexit ||
       OP_code(succ_op) == TOP_br_ctop ||
       OP_code(succ_op) == TOP_br_wexit ||
       OP_code(succ_op) == TOP_br_wtop ||
       OP_code(succ_op) == TOP_br_call ||
       OP_code(succ_op) == TOP_br_ia ||
       OP_code(succ_op) == TOP_br_ret ||
       OP_code(succ_op) == TOP_cover ||
       OP_code(succ_op) == TOP_rfi ||
       OP_code(succ_op) == TOP_clrrrb ||
       OP_code(succ_op) == TOP_clrrrb_pr)) return TRUE;
  
  return FALSE;
}
SCHED_INFO_CLASS Sched_Info_Class(OP *op)
{
  BOOL is_chk = CGTARG_Is_OP_Check_Load(op);  
  return TARG_Sched_Info_Class(OP_code(op), is_chk);
} 

/* ====================================================================
 *
 * CGTARG_Adjust_Latency
 *
 * See interface description
 *
 * ====================================================================
 */
void
CGTARG_Adjust_Latency(OP *pred_op, OP *succ_op, CG_DEP_KIND kind, UINT8 opnd, INT *latency)
{
  INT src_reg, dst_reg;
  BOOL pred_is_chk, succ_is_chk;
  TN *pred_tn = 0, *succ_tn = 0;
  const TOP pred_code = OP_code(pred_op);
  const TOP succ_code = OP_code(succ_op);
  

  if (OP_opnds(pred_op) > 1) {
      TN *src = OP_opnd(pred_op,1);
      if (TN_is_dedicated(src))
          src_reg = REGISTER_machine_id(TN_register_class(src), TN_register(src));
  }

  if (OP_results(pred_op) > 0) {
      TN *dst = OP_result(pred_op,0);
      pred_tn = dst;
      if (TN_is_dedicated(dst))
          dst_reg = REGISTER_machine_id(TN_register_class(dst), TN_register(dst));
  }

  if (pred_code == TOP_alloc && OP_opnds(succ_op) > 1) {
      succ_tn = OP_opnd(succ_op,1);
  }
  
  pred_is_chk = CGTARG_Is_OP_Check_Load(pred_op);  
  succ_is_chk = CGTARG_Is_OP_Check_Load(succ_op);
  TARG_Adjust_Latency(pred_code, succ_code, src_reg, dst_reg, opnd, latency, 
                      pred_is_chk, succ_is_chk);
  
  // TOP_alloc only cannot place one group with flushrs,loadrs,br.call,br1.call
  // br.ia,br.ret,clrrrb,cover,rfi;
  if (pred_code == TOP_alloc && OP_def_use_stack_regs(succ_op) && (!succ_tn || !pred_tn || !TN_is_register(succ_tn) || !TN_is_register(pred_tn) ||!TNs_Are_Equivalent(succ_tn, pred_tn))) {
       *latency = 0;
  }
  if (pred_code == TOP_alloc &&
     (succ_code == TOP_flushrs ||
     succ_code == TOP_br_cexit||
     succ_code == TOP_br_ctop ||
     succ_code == TOP_br_wexit ||
     succ_code == TOP_br_wtop ||
     succ_code == TOP_br_call ||
     succ_code == TOP_br_ia ||
     succ_code == TOP_br_ret ||
     succ_code == TOP_clrrrb ||
     succ_code == TOP_clrrrb_pr ||
     succ_code == TOP_cover ||
     succ_code == TOP_rfi ))
       *latency = 1;

  // LD -> LD => 0 (for memory dependences)
  // Since IA-64 preserves the order of execution of loads in an 
  // instruction group, dependent loads (i.e possibly or fully-aliased 
  // loads) can fit in the same instr. group. We don't need to special 
  // case check-loads since this applies uniformly to all load instructions 
  // including check loads.
  if (OP_load(pred_op) && OP_load(succ_op) &&
      ((kind == CG_DEP_MEMIN) || (kind == CG_DEP_MEMOUT) || 
       (kind == CG_DEP_MEMANTI))) {
    *latency = 0;
  }

  // base-update of postincr memop
  if (   OP_store(pred_op)
      || (   OP_load(pred_op)
	  && !CGTARG_Use_Load_Latency(pred_op, OP_opnd(succ_op, opnd)))) {
    *latency = 1;
  }

  // REGOUT dependence OP cannot be scheduled in the same cycle
  if (kind == CG_DEP_REGOUT) *latency = MAX(1, *latency);

  // REGANTI dependence:  if the pred uses F-unit and 
  // the succ must be in F0-unit, two OPs cannot be scheduled in the same cycle
  if (   kind == CG_DEP_REGANTI
      && EXEC_PROPERTY_is_F_Unit(pred_code)
      && EXEC_PROPERTY_is_F_Unit(succ_code))
  {
    SI_RESOURCE_TOTAL* rvec = TSI_Resource_Total_Vector(succ_code);
    const UINT         size = TSI_Resource_Total_Vector_Size(succ_code);
    for (UINT i = 0; i < size; ++i ) {
      SI_RESOURCE_ID id = SI_RESOURCE_TOTAL_Resource_Id(&rvec[i]);
      if (id == CGTARG_f0_unit_id) *latency = MAX(1, *latency);
    }
  }
  // REGIN dependence when pred op is an mov_f_ar or mov_t_ar pesudo op
  // they can not be the same cycle
  if (kind == CG_DEP_REGIN &&
      (OP_code(pred_op) == TOP_mov_f_ar ||
      OP_code(pred_op) == TOP_mov_t_ar_r ||
      OP_code(pred_op) == TOP_mov_t_ar_i))
       *latency = MAX(1, *latency);

  if ( (*latency) < 0 && kind == CG_DEP_POSTCHK)
    *latency =1;

  // When pred op is a mov_t_pr_i and succ op is a cmp_i_eq,
  // ( e.g. pred_op : mov pr.rot=0 ; succ_op : cmp.eq p17,p0=0,r0 )
  // We manually adjust the latency between them to 1, so that they can not
  // be scheduled into the same cycle.
  if (kind == CG_DEP_MISC &&
      (OP_code(pred_op)==TOP_mov_t_pr_i) &&
      (OP_code(succ_op)==TOP_cmp_i_eq) )
    *latency = MAX(1, *latency);

  //OSP_30
  // When pred op is a fsetc and succ op is a fcvt_fx,
  // ( e.g. pred_op : fsetc.s2 79,32; succ_op : fcvt.fx.s2 f6=f8 )
  // We manually adjust the latency between them to 1, so that they can not
  // be scheduled into the same cycle.
  if (kind == CG_DEP_MISC &&
      (OP_code(pred_op)==TOP_fsetc) &&
      (OP_code(succ_op)==TOP_fcvt_fx) )
    *latency = MAX(1, *latency);

  if ((OP_call (succ_op) || OP_code(succ_op) == TOP_br_ret) &&
      kind == CG_DEP_REGIN &&
      opnd >= OP_opnds(succ_op) - succ_op->hidden_opnds) {
    *latency = 0;
  } else if (OP_call (pred_op) && kind == CG_DEP_REGANTI &&
	     opnd >= OP_opnds(pred_op) - pred_op->hidden_opnds) {
    *latency = 1;
  }

  // "call" and "return" instruction has hidden operand, the latency for the
  // depenence arise form hidden operand should be 0 if we want the dependent
  // instruction fit in one cycle or one if we want them at least 1 cycle apart.
  //
  if ((OP_call (succ_op) || OP_code(succ_op) == TOP_br_ret) &&
      kind == CG_DEP_REGIN && Is_Hidden_Opnd(succ_op, opnd)) {
    *latency = 0;
  } else if (OP_call (pred_op) && kind == CG_DEP_REGANTI &&
	     Is_Hidden_Opnd(pred_op, opnd)) {
    *latency = 1;
  }	
}

BOOL
Is_MMX_Dependency (OP* pred_op, OP* succ_op, CG_DEP_KIND kind) {
        
    if (kind != CG_DEP_REGIN) { return FALSE; }

    switch (Sched_Info_Class(pred_op)) {
	case SIC_MMALU_A:
	case SIC_MMALU_I:
	case SIC_MMMUL:
    case SIC_MMSHF:
        break;
    default:
        return FALSE;
    }

    switch (Sched_Info_Class(succ_op)) {
	case SIC_IALU:
	case SIC_ISHF:
    case SIC_ILOG:
    case SIC_LD:
	case SIC_ST:
        return TRUE;
    }

    return FALSE;
}

INT32
MMX_Dep_Latency (void) { 
    return 4;
}

/* ====================================================================
 *
 * CGTARG_Generate_Remainder_Branch
 *
 * See interface description
 *
 * ====================================================================
 */
void
CGTARG_Generate_Remainder_Branch(TN *trip_count, TN *label_tn,
				 OPS *prolog_ops, OPS *body_ops)
{
  INT32 trip_size = TN_size(trip_count);

  LC_Used_In_PU = TRUE;
  if (TN_is_constant(trip_count)) {
    TN *tmp_tn = Gen_Register_TN (ISA_REGISTER_CLASS_integer,
				  trip_size);
    Exp_COPY(tmp_tn, Gen_Literal_TN(TN_value(trip_count)-1, trip_size), prolog_ops);
    Exp_COPY(LC_TN, tmp_tn, prolog_ops);
  } else {
    TN *trip_counter_minus_1 = Gen_Register_TN (ISA_REGISTER_CLASS_integer, trip_size);
    Exp_OP2(OPC_I8SUB,
	    trip_counter_minus_1,
	    trip_count,
	    Gen_Literal_TN(1, trip_size),
	    prolog_ops);
    Exp_COPY(LC_TN, trip_counter_minus_1, prolog_ops);
  }

if(PROCESSOR_Version == 2)
  Build_OP (TOP_br_cloop,
            LC_TN,
            Gen_Enum_TN(ECV_bwh_sptk),
            Gen_Enum_TN(ECV_ph_few),
            Gen_Enum_TN(ECV_dh),
            label_tn,
            LC_TN,
            body_ops);
else
  Build_OP (TOP_br_cloop,
            LC_TN,
            Gen_Enum_TN(ECV_bwh_dptk),
            Gen_Enum_TN(ECV_ph_few),
            Gen_Enum_TN(ECV_dh),
            label_tn,
            LC_TN,
            body_ops);


}


/* ====================================================================
 *
 * CGTARG_OP_is_counted_loop
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL CGTARG_OP_is_counted_loop(OP *op) 
{
  return OP_code(op) == TOP_br_cloop ||
    OP_code(op) == TOP_br_ctop;
}


/* ====================================================================
 *
 * CGTARG_Generate_Branch_Cloop
 *
 * See interface description
 *
 * ====================================================================
 */
void
CGTARG_Generate_Branch_Cloop(OP *br_op,
			     TN *unrolled_trip_count,
			     TN *trip_count_tn,
			     INT32 ntimes,
			     TN *label_tn,
			     OPS *prolog_ops,
			     OPS *body_ops)
{ 
  LC_Used_In_PU = TRUE;
  if (!CGTARG_OP_is_counted_loop(br_op)) {
    if(PROCESSOR_Version == 2)
        Build_OP (TOP_br_cloop,
          LC_TN,
          Gen_Enum_TN(ECV_bwh_sptk),
          Gen_Enum_TN(ECV_ph_few),
          Gen_Enum_TN(ECV_dh),
          label_tn,
          LC_TN,
          body_ops);
    else
        Build_OP (TOP_br_cloop,
	      LC_TN,
	      Gen_Enum_TN(ECV_bwh_dptk),
	      Gen_Enum_TN(ECV_ph_few),
	      Gen_Enum_TN(ECV_dh),
	      label_tn, 
	      LC_TN,
	      body_ops);
  }

  INT32 trip_size = TN_size(trip_count_tn);
  TN *unrolled_trip_count_minus_1;

  if (TN_is_constant(trip_count_tn)) {
    unrolled_trip_count_minus_1 = Gen_Literal_TN(TN_value(trip_count_tn) / ntimes - 1,
						 trip_size);
  } else {
    if (unrolled_trip_count == NULL) {
      unrolled_trip_count = Build_TN_Like(trip_count_tn);
      Exp_OP2(trip_size == 4 ? OPC_U4DIV : OPC_U8DIV,
	      unrolled_trip_count,
	      trip_count_tn,
	      Gen_Literal_TN(ntimes, trip_size),
	      prolog_ops);
    }
    unrolled_trip_count_minus_1 = Gen_Register_TN (ISA_REGISTER_CLASS_integer,
						   trip_size);
    Exp_OP2(OPC_I8SUB,
	    unrolled_trip_count_minus_1,
	    unrolled_trip_count,
	    Gen_Literal_TN(1, trip_size),
	    prolog_ops);
  } 

  if (TN_is_constant(unrolled_trip_count_minus_1)) {
	  INT64 imm=TN_value(unrolled_trip_count_minus_1);
	  const ISA_OPERAND_INFO *oinfo;
	  oinfo = ISA_OPERAND_Info(TOP_mov_t_ar_i_i);
	  const ISA_OPERAND_VALTYP *vtype = ISA_OPERAND_INFO_Operand(oinfo, 1);
	  ISA_LIT_CLASS lc = ISA_OPERAND_VALTYP_Literal_Class(vtype);
	  if (ISA_LC_Value_In_Class(imm, lc)){
		  Build_OP (TOP_mov_t_ar_i_i, LC_TN, True_TN, unrolled_trip_count_minus_1, prolog_ops);
	  }
	  else{
		  TN *tmp_tn = Gen_Register_TN (ISA_REGISTER_CLASS_integer,
					  trip_size);
		  Exp_COPY(tmp_tn, unrolled_trip_count_minus_1, prolog_ops);
		  Exp_COPY(LC_TN, tmp_tn, prolog_ops);
	  }
  }
  else{
	  Exp_COPY(LC_TN, unrolled_trip_count_minus_1, prolog_ops);
  }
}

static TN* asm_constraint_tn[10];
static ISA_REGISTER_SUBCLASS asm_constraint_sc[10];
static char asm_constraint_name[10][8];
static INT asm_constraint_index;

// must be called once per asm
void
CGTARG_Init_Asm_Constraints (void)
{
  // can use any type; it will be ignored
  Setup_Output_Parameter_Locations (MTYPE_To_TY(MTYPE_I8));
  for (INT i = 0; i < 10; ++i) {
    asm_constraint_tn[i] = NULL;
    asm_constraint_sc[i] = ISA_REGISTER_SUBCLASS_UNDEFINED;
    asm_constraint_name[i][0] = '\0';
  }
  asm_constraint_index = 0;
}


// -----------------------------------------------------------------------
// Given a constraint for an ASM parameter, and the load of the matching
// argument passed to ASM (possibly NULL), choose an appropriate TN for it
// -----------------------------------------------------------------------
extern TN* 
CGTARG_TN_For_Asm_Operand (const char* constraint, 
                           const WN* load,
                           TN* pref_tn,
                           ISA_REGISTER_SUBCLASS* subclass,
                           const WN* asm_wn)
{
  // skip constraint modifiers:
  // = input and output parameters are separated in the WHIRL for ASM
  // & early_clobber flag is set in Handle_ASM
  // % commutativity of operands is ignored for now
  static const char* modifiers = "=&%";
  while (strchr(modifiers, *constraint)) {
    constraint++;
  }
  
  // TODO: we should really look at multiple specified constraints
  // and the load in order to select the best TN, but for now we
  // assume that when we get here we can safely pick a TN

  // if 'm' is one of the choices, always prefer that one
  // TODO: we decide this in the front end, but it's not optimal
  if (*constraint != 'm') {
    const char* m = constraint;
    while (*++m) {
      if (*m == 'm') {
        constraint = m;
        break;
      }
    }
  }
  
  // prefer register/memory over immediates; this isn't optimal, 
  // but we may not always be able to generate an immediate
  static const char* immediates = "in";
  while (strchr(immediates, *constraint) && *(constraint+1)) {
    constraint++;
  }

  TN* ret_tn;
  
  // TODO: check that the operand satisifies immediate range constraint
  if (strchr(immediates, *constraint)) {
    if (load && WN_operator(load)==OPR_LDA &&
        ST_is_constant(WN_st(load)) ) {
      // OSP_315, 'i' constrait, constant string
      FmtAssert(WN_st(load) != NULL,
                ("WN_st is NULL when handling the const symbol for 'i' constraint"));
      ST *base_sym;
      INT64 base_ofst;
      ST *sym = WN_st(load);
      Allocate_Object(sym);
      Base_Symbol_And_Offset_For_Addressing (sym, WN_lda_offset(load), &base_sym, &base_ofst);
      ret_tn = Gen_Symbol_TN(base_sym, base_ofst, 0);
    }
    else {
      if (load && WN_operator(load)==OPR_LDID &&
          WN_class(load)==CLASS_PREG) {
        // immediate could have been put in preg by wopt
        load = Preg_Is_Rematerializable(WN_load_offset(load), NULL);
      }
      FmtAssert(load && WN_operator(load) == OPR_INTCONST,
                ("Cannot find immediate operand for ASM"));
      ret_tn = Gen_Literal_TN(WN_const_val(load),
                              MTYPE_bit_size(WN_rtype(load))/8);
      // Bugs 3177, 3043 - safety check from gnu/config/i386/i386.h.
      //FmtAssert(CONST_OK_FOR_LETTER(WN_const_val(load), *constraint),
      //          ("The value of immediate operand supplied is not within expected range."));
    }
  }

  // digit constraint means that we should reuse a previous operand
  else if (isdigit(*constraint)) {
    INT prev_index = *constraint - '0';
    FmtAssert(asm_constraint_tn[prev_index], 
              ("numeric matching constraint refers to NULL value"));
    ret_tn = asm_constraint_tn[prev_index];
  }
    
  else if (strchr("gmr", *constraint)) {
    TYPE_ID rtype = (load != NULL ? WN_rtype(load) : MTYPE_I8);
    FmtAssert(MTYPE_is_integral(rtype),
              ("ASM operand does not satisfy its constraint"));
    ret_tn = (pref_tn ? pref_tn : Build_TN_Of_Mtype(rtype));
  }
  
  else if (*constraint == 'f') {
    TYPE_ID rtype = (load != NULL ? WN_rtype(load) : MTYPE_F8);
    FmtAssert(MTYPE_is_float(rtype),
              ("ASM operand does not satisfy its constraint"));
    ret_tn = (pref_tn ? pref_tn : Build_TN_Of_Mtype(rtype));
  }

  else if (*constraint == 'b') {
    TYPE_ID rtype = (load != NULL ? WN_rtype(load) : Pointer_Mtype);
    FmtAssert(rtype == Pointer_Mtype,
              ("ASM operand does not satisfy its constraint"));
    ret_tn = (pref_tn ? pref_tn : Build_TN_Of_Mtype(MTYPE_A8));
  }

  else {
    FmtAssert(FALSE, ("ASM constraint <%s> not supported", constraint));
  }

  asm_constraint_tn[asm_constraint_index] = ret_tn;
  asm_constraint_index++;
  
  return ret_tn;
}


static char *
Get_TN_Assembly_Name (TN *tn)
{
        char *name = NULL;
        if (TN_is_register(tn)) {
                ISA_REGISTER_CLASS rc = TN_register_class(tn);
                REGISTER reg = TN_register(tn);
                if (ABI_PROPERTY_Is_stacked(rc, REGISTER_machine_id(rc, reg))
                        && REGISTER_Is_Stacked_Output(rc, reg) )
                {
                        name = REGISTER_Stacked_Output_Name (reg);
                }
                if (name == NULL) {
                        name = (char*) REGISTER_name(rc, reg);
                }
        }
        else {
                FmtAssert(FALSE, ("NYI:  non-register parameters in ASM"));
        }
        return name;
}

void
CGTARG_TN_And_Name_For_Asm_Constraint (char *constraint, TYPE_ID mtype,
	TYPE_ID desc, TN **tn, char **name)
{
	INT i;
	if (*constraint == '=') {
		// ignore
		CGTARG_TN_And_Name_For_Asm_Constraint (constraint+1, 
			mtype, desc, tn, name);
		return;
	}
	if (mtype == MTYPE_V) {
		// unknown parameter, so pick mtype from constraint
		if (*constraint == 'f') mtype = MTYPE_F8;
		else mtype = MTYPE_I8;
	}
	switch (*constraint) {
	case 'r':
		FmtAssert(MTYPE_is_integral(mtype), 
			("ASM constraint is integer but parameter is not"));
		break;
	case 'f':
		FmtAssert(MTYPE_is_float(mtype), 
			("ASM constraint is float but parameter is not"));
		break;
	case 'm':
		break;
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
		i = *constraint - '0';
		FmtAssert(asm_constraint_tn[i], 
		    ("numeric matching constraint refers to NULL value"));
		++asm_constraint_index;
		*tn = asm_constraint_tn[i];
		*name = asm_constraint_name[i];
		return;
	case 'i':
		// let caller figure out the name
		*tn = NULL;
		*name = NULL;
		return;
	default:
		FmtAssert(FALSE, ("ASM constraint <%s> not supported", constraint));
	}
	PLOC ploc = Get_Output_Parameter_Location (MTYPE_To_TY(mtype));
	*tn = PREG_To_TN (MTYPE_To_PREG(mtype), PLOC_reg(ploc));
	asm_constraint_tn[asm_constraint_index] = *tn;
	*name = Get_TN_Assembly_Name(*tn);
	if (*constraint == 'm') {
	    	sprintf(asm_constraint_name[asm_constraint_index], "[%s]", 
			*name);
	} else {
	    	sprintf(asm_constraint_name[asm_constraint_index], "%s", 
			*name);
	}
	*name = asm_constraint_name[asm_constraint_index];
	++asm_constraint_index;
}


/* ====================================================================
 * target specific modifiers for printing different versions
 * of register names when they appear as AM operands 
 * ====================================================================
 */
char CGTARG_Asm_Opnd_Modifiers[] = { 'r' };
INT  CGTARG_Num_Asm_Opnd_Modifiers = 1;

const char* 
CGTARG_Modified_Asm_Opnd_Name(char modifier, TN* tn, char *tn_name)
{
  if (modifier == 'r') {
    return tn_name;
  }
  else {
    FmtAssert(FALSE, ("Unknown ASM operand modifier '%c'", modifier));
  }
  /*NOTREACHED*/
}


/* ====================================================================
 *
 * CGTARG_Postprocess_Asm_String: remove %Pn postincrement modifiers
 * %Pn says that operand n "may" be a postincrement, and if the
 * compiler wants to use a postincrement form, then put the postincrement
 * value here.  But we don't want to bother ever trying to do this,
 * so we'll just ignore it.
 *
 * ====================================================================
 */
void 
CGTARG_Postprocess_Asm_String (char *s)
{
	char *p;
	while (TRUE) {
		p = strstr(s, "%P");
		if (p == NULL) break;
		// insert blanks cause easier than removing spaces
		*p = ' ';	// %
		++p;
		*p = ' ';	// P
		++p;
		*p = ' ';	// <number>
	}
}


/* ====================================================================
 *
 * CGTARG_Unconditional_Compare
 *
 * See interface description
 *
 * ====================================================================
 */
static 
BOOL CGTARG_Unconditional_Compare_Helper(TOP top, TOP* uncond_ver)
{
  switch (top) {
  case TOP_fcmp_eq_unc:
  case TOP_fcmp_ge_unc:
  case TOP_fcmp_gt_unc:
  case TOP_fcmp_le_unc:
  case TOP_fcmp_lt_unc:
  case TOP_fcmp_neq_unc:
  case TOP_fcmp_nge_unc:
  case TOP_fcmp_ngt_unc:
  case TOP_fcmp_nle_unc:
  case TOP_fcmp_nlt_unc:
  case TOP_fcmp_ord_unc:
  case TOP_fcmp_unord_unc:
  case TOP_cmp_eq_unc:
  case TOP_cmp_lt_unc:
  case TOP_cmp_ltu_unc:
  case TOP_cmp_i_eq_unc:
  case TOP_cmp_i_lt_unc:
  case TOP_cmp_i_ltu_unc:
  case TOP_cmp4_eq_unc:
  case TOP_cmp4_lt_unc:
  case TOP_cmp4_ltu_unc:
  case TOP_cmp4_i_eq_unc:
  case TOP_cmp4_i_lt_unc:
  case TOP_cmp4_i_ltu_unc:
  case TOP_cmp_ne_unc:
  case TOP_cmp_le_unc:
  case TOP_cmp_gt_unc:
  case TOP_cmp_ge_unc:
  case TOP_cmp_leu_unc:
  case TOP_cmp_gtu_unc:
  case TOP_cmp_geu_unc:
  case TOP_cmp_i_ne_unc:
  case TOP_cmp_i_le_unc:
  case TOP_cmp_i_gt_unc:
  case TOP_cmp_i_ge_unc:
  case TOP_cmp_i_leu_unc:
  case TOP_cmp_i_gtu_unc:
  case TOP_cmp_i_geu_unc:
  case TOP_cmp4_ne_unc:
  case TOP_cmp4_le_unc:
  case TOP_cmp4_gt_unc:
  case TOP_cmp4_ge_unc:
  case TOP_cmp4_leu_unc:
  case TOP_cmp4_gtu_unc:
  case TOP_cmp4_geu_unc:
  case TOP_cmp4_i_ne_unc:
  case TOP_cmp4_i_le_unc:
  case TOP_cmp4_i_gt_unc:
  case TOP_cmp4_i_ge_unc:
  case TOP_cmp4_i_leu_unc:
  case TOP_cmp4_i_gtu_unc:
  case TOP_cmp4_i_geu_unc:
  case TOP_tbit_nz_unc:
  case TOP_tbit_z_unc: 
  case TOP_tnat_nz_unc:
  case TOP_tnat_z_unc: 
  case TOP_frcpa:
  case TOP_frsqrta:
    *uncond_ver = top;
    return TRUE;

  case TOP_fcmp_eq:
    *uncond_ver = TOP_fcmp_eq_unc;
    break;
  case TOP_fcmp_ge:
    *uncond_ver = TOP_fcmp_ge_unc;
    break;
  case TOP_fcmp_gt:
    *uncond_ver = TOP_fcmp_gt_unc;
    break;
  case TOP_fcmp_le:
    *uncond_ver = TOP_fcmp_le_unc;
    break;
  case TOP_fcmp_lt:
    *uncond_ver = TOP_fcmp_lt_unc;
    break;
  case TOP_fcmp_neq:
    *uncond_ver = TOP_fcmp_neq_unc;
    break;
  case TOP_fcmp_nge:
    *uncond_ver = TOP_fcmp_nge_unc;
    break;
  case TOP_fcmp_ngt:
    *uncond_ver = TOP_fcmp_ngt_unc;
    break;
  case TOP_fcmp_nle:
    *uncond_ver = TOP_fcmp_nle_unc;
    break;
  case TOP_fcmp_nlt:
    *uncond_ver = TOP_fcmp_nlt_unc;
    break;
  case TOP_fcmp_ord:
    *uncond_ver = TOP_fcmp_ord_unc;
    break;
  case TOP_fcmp_unord:
    *uncond_ver = TOP_fcmp_unord_unc;
    break;
  case TOP_cmp_eq:
    *uncond_ver = TOP_cmp_eq_unc;
    break;
  case TOP_cmp_lt:
    *uncond_ver = TOP_cmp_lt_unc;
    break;
  case TOP_cmp_ltu:
    *uncond_ver = TOP_cmp_ltu_unc;
    break;
  case TOP_cmp_i_eq:
    *uncond_ver = TOP_cmp_i_eq_unc;
    break;
  case TOP_cmp_i_lt:
    *uncond_ver = TOP_cmp_i_lt_unc;
    break;
  case TOP_cmp_i_ltu:
    *uncond_ver = TOP_cmp_i_ltu_unc;
    break;
  case TOP_cmp4_eq:
    *uncond_ver = TOP_cmp4_eq_unc;
    break;
  case TOP_cmp4_lt:
    *uncond_ver = TOP_cmp4_lt_unc;
    break;
  case TOP_cmp4_ltu:
    *uncond_ver = TOP_cmp4_ltu_unc;
    break;
  case TOP_cmp4_i_eq:
    *uncond_ver = TOP_cmp4_i_eq_unc;
    break;
  case TOP_cmp4_i_lt:
    *uncond_ver = TOP_cmp4_i_lt_unc;
    break;
  case TOP_cmp4_i_ltu:
    *uncond_ver = TOP_cmp4_i_ltu_unc;
    break;
  case TOP_cmp_ne:
    *uncond_ver = TOP_cmp_ne_unc;
    break;
  case TOP_cmp_le:
    *uncond_ver = TOP_cmp_le_unc;
    break;
  case TOP_cmp_gt:
    *uncond_ver = TOP_cmp_gt_unc;
    break;
  case TOP_cmp_ge:
    *uncond_ver = TOP_cmp_ge_unc;
    break;
  case TOP_cmp_leu:
    *uncond_ver = TOP_cmp_leu_unc;
    break;
  case TOP_cmp_gtu:
    *uncond_ver = TOP_cmp_gtu_unc;
    break;
  case TOP_cmp_geu:
    *uncond_ver = TOP_cmp_geu_unc;
    break;
  case TOP_cmp_i_ne:
    *uncond_ver = TOP_cmp_i_ne_unc;
    break;
  case TOP_cmp_i_le:
    *uncond_ver = TOP_cmp_i_le_unc;
    break;
  case TOP_cmp_i_gt:
    *uncond_ver = TOP_cmp_i_gt_unc;
    break;
  case TOP_cmp_i_ge:
    *uncond_ver = TOP_cmp_i_ge_unc;
    break;
  case TOP_cmp_i_leu:
    *uncond_ver = TOP_cmp_i_leu_unc;
    break;
  case TOP_cmp_i_gtu:
    *uncond_ver = TOP_cmp_i_gtu_unc;
    break;
  case TOP_cmp_i_geu:
    *uncond_ver = TOP_cmp_i_geu_unc;
    break;
  case TOP_cmp4_ne:
    *uncond_ver = TOP_cmp4_ne_unc;
    break;
  case TOP_cmp4_le:
    *uncond_ver = TOP_cmp4_le_unc;
    break;
  case TOP_cmp4_gt:
    *uncond_ver = TOP_cmp4_gt_unc;
    break;
  case TOP_cmp4_ge:
    *uncond_ver = TOP_cmp4_ge_unc;
    break;
  case TOP_cmp4_leu:
    *uncond_ver = TOP_cmp4_leu_unc;
    break;
  case TOP_cmp4_gtu:
    *uncond_ver = TOP_cmp4_gtu_unc;
    break;
  case TOP_cmp4_geu:
    *uncond_ver = TOP_cmp4_geu_unc;
    break;
  case TOP_cmp4_i_ne:
    *uncond_ver = TOP_cmp4_i_ne_unc;
    break;
  case TOP_cmp4_i_le:
    *uncond_ver = TOP_cmp4_i_le_unc;
    break;
  case TOP_cmp4_i_gt:
    *uncond_ver = TOP_cmp4_i_gt_unc;
    break;
  case TOP_cmp4_i_ge:
    *uncond_ver = TOP_cmp4_i_ge_unc;
    break;
  case TOP_cmp4_i_leu:
    *uncond_ver = TOP_cmp4_i_leu_unc;
    break;
  case TOP_cmp4_i_gtu:
    *uncond_ver = TOP_cmp4_i_gtu_unc;
    break;
  case TOP_cmp4_i_geu:
    *uncond_ver = TOP_cmp4_i_geu_unc;
    break;
  case TOP_tbit_nz:
    *uncond_ver = TOP_tbit_nz_unc;
    break; 
  case TOP_tbit_z: 
    *uncond_ver = TOP_tbit_z_unc; 
    break; 
  case TOP_tnat_nz:
    *uncond_ver = TOP_tnat_nz_unc;
    break; 
  case TOP_tnat_z: 
    *uncond_ver = TOP_tnat_z_unc;
    break; 

  default:
    *uncond_ver = TOP_UNDEFINED;
  }
  return FALSE;
}

BOOL CGTARG_Unconditional_Compare(OP *op, TOP* uncond_ver)
{
  return (CGTARG_Unconditional_Compare_Helper(OP_code(op),uncond_ver));
}

/* ====================================================================
 *
 * CGTARG_Invert_Branch
 *
 * See interface description
 *
 * ====================================================================
 */
TOP CGTARG_Invert_Branch(BB* bb)
{
  TN* tn1;
  TN* tn2;
  OP* compare_op;
  OP* br_op = BB_branch_op(bb);

  if (!OP_has_predicate(br_op)) {
    return TOP_UNDEFINED;
  }
  TN* pred_tn = OP_opnd(br_op, OP_PREDICATE_OPND);
  if (TN_is_true_pred(pred_tn)) {
    return TOP_UNDEFINED;
  }
  CGTARG_Analyze_Compare(br_op, &tn1, &tn2, &compare_op);
  if (compare_op == br_op) {
    return TOP_UNDEFINED;
  }
  if (OP_result(compare_op, 0) == pred_tn) {
    Set_OP_opnd(br_op, OP_PREDICATE_OPND, OP_result(compare_op, 1));
  } else {
    Set_OP_opnd(br_op, OP_PREDICATE_OPND, OP_result(compare_op, 0));
  }
  return OP_code(br_op);
}


/* ====================================================================
 *
 * CGTARG_Init_OP_cond_def_kind
 *
 *  See interface description
 *
 * ====================================================================
 */
void CGTARG_Init_OP_cond_def_kind(OP *op)
{
  TOP top = OP_code(op);
  switch (top) {

    // The following OPs unconditionally define the predicate results.
    //
  case TOP_cmp_eq_unc:
  case TOP_cmp_lt_unc:
  case TOP_cmp_ltu_unc:
  case TOP_cmp_i_eq_unc:
  case TOP_cmp_i_lt_unc:
  case TOP_cmp_i_ltu_unc:
  case TOP_cmp4_eq_unc:
  case TOP_cmp4_lt_unc:
  case TOP_cmp4_ltu_unc:
  case TOP_cmp4_i_eq_unc:
  case TOP_cmp4_i_lt_unc:
  case TOP_cmp4_i_ltu_unc:
  case TOP_cmp_ne_unc:
  case TOP_cmp_le_unc:
  case TOP_cmp_gt_unc:
  case TOP_cmp_ge_unc:
  case TOP_cmp_leu_unc:
  case TOP_cmp_gtu_unc:
  case TOP_cmp_geu_unc:
  case TOP_cmp_i_ne_unc:
  case TOP_cmp_i_le_unc:
  case TOP_cmp_i_gt_unc:
  case TOP_cmp_i_ge_unc:
  case TOP_cmp_i_leu_unc:
  case TOP_cmp_i_gtu_unc:
  case TOP_cmp_i_geu_unc:
  case TOP_cmp4_ne_unc:
  case TOP_cmp4_le_unc:
  case TOP_cmp4_gt_unc:
  case TOP_cmp4_ge_unc:
  case TOP_cmp4_leu_unc:
  case TOP_cmp4_gtu_unc:
  case TOP_cmp4_geu_unc:
  case TOP_cmp4_i_ne_unc:
  case TOP_cmp4_i_le_unc:
  case TOP_cmp4_i_gt_unc:
  case TOP_cmp4_i_ge_unc:
  case TOP_cmp4_i_leu_unc:
  case TOP_cmp4_i_gtu_unc:
  case TOP_cmp4_i_geu_unc:
  case TOP_fcmp_eq_unc:
  case TOP_fcmp_lt_unc:
  case TOP_fcmp_le_unc:
  case TOP_fcmp_unord_unc:
  case TOP_fcmp_gt_unc:
  case TOP_fcmp_ge_unc:
  case TOP_fcmp_neq_unc:
  case TOP_fcmp_nlt_unc:
  case TOP_fcmp_nle_unc:
  case TOP_fcmp_ngt_unc:
  case TOP_fcmp_nge_unc:
  case TOP_fcmp_ord_unc:
  case TOP_tbit_nz_unc:
  case TOP_tbit_z_unc:
  case TOP_tnat_nz_unc:
  case TOP_tnat_z_unc:
    
    Set_OP_cond_def_kind(op, OP_ALWAYS_UNC_DEF);
    break;

    // The following OPs do not always update the result predicates.
  case TOP_cmp_eq_and:
  case TOP_cmp_eq_or:
  case TOP_cmp_eq_or_andcm:
  case TOP_cmp_ne_and:
  case TOP_cmp_ne_or:
  case TOP_cmp_ne_or_andcm:
  case TOP_cmp_z1_lt_and:
  case TOP_cmp_z1_lt_or:
  case TOP_cmp_z1_lt_or_andcm:
  case TOP_cmp_z1_le_and:
  case TOP_cmp_z1_le_or:
  case TOP_cmp_z1_le_or_andcm:
  case TOP_cmp_z1_gt_and:
  case TOP_cmp_z1_gt_or:
  case TOP_cmp_z1_gt_or_andcm:
  case TOP_cmp_z1_ge_and:
  case TOP_cmp_z1_ge_or:
  case TOP_cmp_z1_ge_or_andcm:
  case TOP_cmp_i_eq_and:
  case TOP_cmp_i_eq_or:
  case TOP_cmp_i_eq_or_andcm:
  case TOP_cmp_i_ne_and:
  case TOP_cmp_i_ne_or:
  case TOP_cmp_i_ne_or_andcm:
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
  case TOP_cmp_eq_orcm:
  case TOP_cmp_eq_andcm:
  case TOP_cmp_eq_and_orcm:
  case TOP_cmp_ne_orcm:
  case TOP_cmp_ne_andcm:
  case TOP_cmp_ne_and_orcm:
  case TOP_cmp_z1_lt_orcm:
  case TOP_cmp_z1_lt_andcm:
  case TOP_cmp_z1_lt_and_orcm:
  case TOP_cmp_z1_le_orcm:
  case TOP_cmp_z1_le_andcm:
  case TOP_cmp_z1_le_and_orcm:
  case TOP_cmp_z1_gt_orcm:
  case TOP_cmp_z1_gt_andcm:
  case TOP_cmp_z1_gt_and_orcm:
  case TOP_cmp_z1_ge_orcm:
  case TOP_cmp_z1_ge_andcm:
  case TOP_cmp_z1_ge_and_orcm:
  case TOP_cmp_z2_lt_orcm:
  case TOP_cmp_z2_lt_andcm:
  case TOP_cmp_z2_lt_and_orcm:
  case TOP_cmp_z2_le_orcm:
  case TOP_cmp_z2_le_andcm:
  case TOP_cmp_z2_le_and_orcm:
  case TOP_cmp_z2_gt_orcm:
  case TOP_cmp_z2_gt_andcm:
  case TOP_cmp_z2_gt_and_orcm:
  case TOP_cmp_z2_ge_orcm:
  case TOP_cmp_z2_ge_andcm:
  case TOP_cmp_z2_ge_and_orcm:
  case TOP_cmp_z2_lt_and:
  case TOP_cmp_z2_lt_or:
  case TOP_cmp_z2_lt_or_andcm:
  case TOP_cmp_z2_le_and:
  case TOP_cmp_z2_le_or:
  case TOP_cmp_z2_le_or_andcm:
  case TOP_cmp_z2_gt_and:
  case TOP_cmp_z2_gt_or:
  case TOP_cmp_z2_gt_or_andcm:
  case TOP_cmp_z2_ge_and:
  case TOP_cmp_z2_ge_or:
  case TOP_cmp_z2_ge_or_andcm:
  case TOP_cmp_i_eq_orcm:
  case TOP_cmp_i_eq_andcm:
  case TOP_cmp_i_eq_and_orcm:
  case TOP_cmp_i_ne_orcm:
  case TOP_cmp_i_ne_andcm:
  case TOP_cmp_i_ne_and_orcm:
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
  case TOP_cmp_lt_and:
  case TOP_cmp_lt_or:
  case TOP_cmp_lt_or_andcm:
  case TOP_cmp_le_and:
  case TOP_cmp_le_or:
  case TOP_cmp_le_or_andcm:
  case TOP_cmp_gt_and:
  case TOP_cmp_gt_or:
  case TOP_cmp_gt_or_andcm:
  case TOP_cmp_ge_and:
  case TOP_cmp_ge_or:
  case TOP_cmp_ge_or_andcm:
  case TOP_cmp_lt_orcm:
  case TOP_cmp_lt_andcm:
  case TOP_cmp_lt_and_orcm:
  case TOP_cmp_le_orcm:
  case TOP_cmp_le_andcm:
  case TOP_cmp_le_and_orcm:
  case TOP_cmp_gt_orcm:
  case TOP_cmp_gt_andcm:
  case TOP_cmp_gt_and_orcm:
  case TOP_cmp_ge_orcm:
  case TOP_cmp_ge_andcm:
  case TOP_cmp_ge_and_orcm:
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

    Set_OP_cond_def_kind(op, OP_ALWAYS_COND_DEF);
    break;

  default:
    if (OP_has_predicate(op))
      Set_OP_cond_def_kind(op, OP_PREDICATED_DEF);
    else
      Set_OP_cond_def_kind(op, OP_ALWAYS_UNC_DEF);
    break;
  }
}


/* ====================================================================
 *
 * CGTARG_Get_unc_Variant
 *
 *  Given a compare opcode, return the unconditional variant form. 
 *  Return the opcode if there is no such form.
 *
 * ====================================================================
 */
TOP CGTARG_Get_unc_Variant(TOP top)
{
  TOP result;

  CGTARG_Unconditional_Compare_Helper(top, &result);
  
  if (result == TOP_UNDEFINED) {
    result = top;
  }
  
  return (result);
}

////////////////////////////////////////////////////////////////
// If a BB ends in an unconditional branch, turn it into a conditional branch 
// With TRUE predicate, so we can predicate with something else later.
// If we can't find an unconditional branch, just give up and do nothing
//
void
Make_Branch_Conditional(BB *bb)
{
  OP *new_br;
  TOP new_top;
  OP* br_op = BB_branch_op(bb);

  if (!br_op) return;

  switch (OP_code(br_op)) {
  case TOP_br:
    new_top = TOP_br_cond;
    break;
  case TOP_br_r:
    new_top = TOP_br_r_cond;
    break;
  default:
    return;
  }

  new_br = Mk_OP(new_top,
		 True_TN,
		 Gen_Enum_TN(ECV_bwh_dptk),
		 Gen_Enum_TN(ECV_ph_few),
		 Gen_Enum_TN(ECV_dh),
		 OP_opnd(br_op,2));
  OP_srcpos(new_br) = OP_srcpos(br_op);
  BB_Insert_Op_After(bb, br_op, new_br);
  BB_Remove_Op(bb, br_op);
}

/* ====================================================================
 *
 * CGTARG_Check_OP_For_HB_Suitability
 *
 * Returns TRUE if OP is a suitable candidate for HBF. Otherwise, return
 * FALSE.
 *
 * ====================================================================
 */
BOOL 
CGTARG_Check_OP_For_HB_Suitability(OP *op)
{

  // If <op> has a qualifying predicate operand, return TRUE.
  if (OP_has_predicate(op)) return TRUE;

  // <xfer_op> without any qualifying predicate operand are still OK, since
  // they are eliminated as a result of predication.
  if (OP_xfer(op)) return TRUE;

  // #804007. Alloc instruction can be relaxed.
  if (OP_code(op) == TOP_alloc) return TRUE;

  return FALSE;  // default case
}

/* ====================================================================
 * Pad_Cycles_Before
 *
 * Pad cycles before OP in BB with Noop bundles. 
 * OP should be start of a bundle.
 * ====================================================================
*/
static void
Pad_Cycles_Before(BB *bb, OP *op, UINT cycles)
{
  UINT slot_mask, stop_mask, ibundle, one_stop_bundles, two_stop_bundles;
  INT slot;
  OP *new_op;
  OPS new_ops = OPS_EMPTY;  
  ISA_EXEC_UNIT_PROPERTY unit;

  if (!cycles) return;
  if (!OP_end_group(OP_prev(op))) {
      Set_OP_end_group(OP_prev(op));
      cycles--;
      if (!cycles) return;
  }
  one_stop_bundles = cycles % 2;
  two_stop_bundles = cycles / 2;
  
  /* Choose the first bundle with one stop bit. We add another
     stop bit at the end of bundle.
  */ 
  for (ibundle = 0; ibundle < ISA_MAX_BUNDLES; ++ibundle) {
      stop_mask = ISA_EXEC_Stop_Mask(ibundle);
      slot_mask = ISA_EXEC_Slot_Mask(ibundle);
      if (stop_mask == 2) break;
  }
  stop_mask |= 0x1;

  /* Padding with two-stop bundles */
  while (two_stop_bundles) { 
    slot = 0; 
    do {
      unit =  (ISA_EXEC_UNIT_PROPERTY)(
                (slot_mask >> (ISA_TAG_SHIFT * slot)) 
                    & ((1 << ISA_TAG_SHIFT) - 1));
      new_op = Mk_OP (CGTARG_Noop_Top(unit), True_TN, Gen_Literal_TN(0, 4));
      /* Set the stop bit*/
      if((stop_mask >> slot) & 0x1) Set_OP_end_group(new_op);
      BB_Insert_Op_Before (bb, op, new_op);
      op = new_op;
      slot++;
    } while (slot < ISA_MAX_SLOTS);
    Set_OP_start_bundle(op);
    two_stop_bundles--;
  }
  
  /* Choose the first bundle without stop bits. We add a stop
     bit at the end of bundle.
  */ 
  for (ibundle = 0; ibundle < ISA_MAX_BUNDLES; ++ibundle) {
    stop_mask = ISA_EXEC_Stop_Mask(ibundle);
    slot_mask = ISA_EXEC_Slot_Mask(ibundle);
    if (stop_mask == 0) break;
  }
  stop_mask |= 0x1;

  /* Padding with one-stop bundles */
  if (one_stop_bundles) {
    slot = 0;
    do {
      unit =  (ISA_EXEC_UNIT_PROPERTY)(
                (slot_mask >> (ISA_TAG_SHIFT * slot)) 
                    & ((1 << ISA_TAG_SHIFT) - 1));
        new_op = Mk_OP (CGTARG_Noop_Top(unit), True_TN, Gen_Literal_TN(0, 4));
        /* Set the stop bit*/
        if((stop_mask >> slot) & 0x1) Set_OP_end_group(new_op);
        BB_Insert_Op_Before (bb, op, new_op);
        op = new_op;
        slot++;
    } while (slot < ISA_MAX_SLOTS);
    Set_OP_start_bundle(op);
  }
}

/* ====================================================================
 * OP_Replace_With_Noop 
 *
 * Replace OP with Noop. Reserve its ISA_EXEC_Unit_Prop and flags.
 * ====================================================================
*/
static
OP *OP_Replace_With_Noop(BB *bb, OP *op)
{
    OP *new_op;
    ISA_EXEC_UNIT_PROPERTY unit;

    if ( EXEC_PROPERTY_is_M_Unit(OP_code(op)) && EXEC_PROPERTY_is_I_Unit(OP_code(op)) )
        unit = OP_m_unit(op)? ISA_EXEC_PROPERTY_M_Unit : ISA_EXEC_PROPERTY_I_Unit;
    else  unit =  ISA_EXEC_Unit_Prop(OP_code(op));

    // Fix bug when unit is B and B2 unit, 
    if (EXEC_PROPERTY_is_B_Unit(OP_code(op))) { unit = ISA_EXEC_PROPERTY_B_Unit; }

    new_op = Mk_OP (CGTARG_Noop_Top(unit), True_TN, Gen_Literal_TN(0, 4));
    OP_flags(new_op) = OP_flags(op);
    BB_Insert_Op_Before(bb, op, new_op);
    BB_Remove_Op(bb, op);
    return new_op; 
}
    
/* ====================================================================
 * Fix_MM_Latency
 *
 * Fix pipeline latency problem due to special cycles apart requirement.
 * On Itanium, it occurs between MMxxx -> IALU, ISHF, ILOG, LD, ST.
 * 
 * ====================================================================
*/
void
Fix_MM_Latency ( BB *bb, TOP_SET *src_op_class, TOP_SET *tgt_op_class, UINT8 cycles_apart)
{
  UINT i, j, k, cycles = 0;
  OP *op, *src_op, *tgt_op;
  ISA_EXEC_UNIT_PROPERTY unit;
  
  FOR_ALL_BB_OPs(bb,op) {
    if (OP_dummy(op) || OP_simulated(op)) continue;
    if (find((*src_op_class).begin (),
       (*src_op_class).end (),
       Sched_Info_Class(op)) != (*src_op_class).end()) {
      src_op = op;
      if (OP_end_group(op)) cycles++;
      tgt_op = OP_next(op);
      for (; tgt_op; tgt_op = OP_next(tgt_op)) {
        if (OP_dummy(op) || OP_simulated(op)) continue;
        if (cycles >= cycles_apart) {
          break;
        } else if (find((*tgt_op_class).begin(), (*tgt_op_class).end(), 
                        Sched_Info_Class(tgt_op)) != (*tgt_op_class).end()) {
          for (i = 0; i < OP_results(src_op); i++) {
            for (j = 0; j < OP_opnds(tgt_op); j++) { 
              if (OP_result(src_op, i) == OP_opnd(tgt_op, j)) {
                for (op = OP_next(src_op); op != tgt_op; op = OP_next(op)) if (OP_start_bundle(op)) break;
                /* If src_op and tgt_op are in the same bundle, split it into two bundles */
                if (op == tgt_op && !OP_start_bundle(tgt_op)) {
                  OPS new_ops = OPS_EMPTY;
                  OP *new_op;

                  /* Go backward to the first op in this bundle */
                  op = src_op;
                  while (!OP_start_bundle(op)) op = OP_prev(op);

                  /* Duplicate this bundle */
                  do { 
                    new_op = Dup_OP(op);
                    /* we must count the new cycles incurred because of split */
                    if (OP_end_group(op)) cycles++;
                    /* For we'll change tgt_op in the old bundle to nop, so we must store 
                       the tgt_op position in the new bundle
                    */
                    if (op == tgt_op) tgt_op = new_op;
                    OPS_Append_Op(&new_ops, new_op);
                    op = OP_next(op);
                  } while (op && !OP_start_bundle(op));

                  if (!op) op = BB_last_op(bb); else op = OP_prev(op);
                  BB_Insert_Ops_After(bb, op, &new_ops);
                    
                  /* Replace Ops after src_op in the old bundle to Noops */
                  for (op = OP_next(src_op); !OP_start_bundle(op); op = OP_next(op)) {
                    if (OP_dummy(op) || OP_simulated(op)) continue;
                    op = OP_Replace_With_Noop(bb, op);
                  }
                                   
                  /* Replace Ops before tgt_op in the new bundle to Noops */
                  for ( ; op != tgt_op; op = OP_next(op)) {
                    if (OP_dummy(op) || OP_simulated(op)) continue;
                    op = OP_Replace_With_Noop(bb, op);
                  }
                  
                  /* Pad cycles before the new bundle */
                  while (!OP_start_bundle(op)) op = OP_prev(op);
                  Pad_Cycles_Before(bb, op, cycles_apart - cycles);

                } else { /* In the separate bundles */
                  for (op = tgt_op; !OP_start_bundle(op); op = OP_prev(op));
                  Pad_Cycles_Before(bb, op, cycles_apart - cycles);
                }
                break;
              }
            }
            if (j < OP_opnds(tgt_op)) break;
          }
          if (j < OP_opnds(tgt_op)) break;
        }
      if (OP_end_group(tgt_op)) cycles++;
      }
    /* Restore the original op and continue the search */
    op = src_op;
    cycles = 0;
    }
  }
}
void Pad_Cycles_Into(BB *bb, OP* src_op, OP *tgt_op, INT cycles)
{
    if (cycles <=0) return;
    // Find suitable position for inserting noop
    OP *op;
    
    for (op = OP_next(src_op); op != tgt_op; op = OP_next(op)) 
        if (OP_start_bundle(op)) break;
    
     /* If src_op and tgt_op are in the same bundle, split it into two bundles */
    if (op == tgt_op && !OP_start_bundle(tgt_op)) {
        OPS new_ops = OPS_EMPTY;
        OP *new_op;

        /* Go backward to the first op in this bundle */
        op = src_op;
        while (!OP_start_bundle(op)) op = OP_prev(op);

        /* Duplicate this bundle */
        do { 
            new_op = Dup_OP(op);
            // has problem when creat a new tag
            if (OP_has_tag(op)) return;
            /* we must count the new cycles incurred because of split */
            if (OP_end_group(op)) cycles--;
            /* For we'll change tgt_op in the old bundle to nop, so we must store 
               the tgt_op position in the new bundle
            */
            if (op == tgt_op) tgt_op = new_op;
            OPS_Append_Op(&new_ops, new_op);
            op = OP_next(op);
        } while (op && !OP_start_bundle(op));

        // because we get imcomplete bundle, then ignore this case
        if ((OPS_length(&new_ops) % ISA_MAX_SLOTS) !=0) return;

        if (!op) op = BB_last_op(bb); 
        else op = OP_prev(op);
        BB_Insert_Ops_After(bb, op, &new_ops);
                    
        /* Replace Ops after src_op in the old bundle to Noops */
        for (op = OP_next(src_op); !OP_start_bundle(op); op = OP_next(op)) {
           if (OP_dummy(op) || OP_simulated(op) || OP_noop(op)) continue;
           op = OP_Replace_With_Noop(bb, op);
        }
                                   
        /* Replace Ops before tgt_op in the new bundle to Noops */
        for ( ; op != tgt_op; op = OP_next(op)) {
            if (OP_dummy(op) || OP_simulated(op)) continue;
            op = OP_Replace_With_Noop(bb, op);
        }
                  
        /* Pad cycles before the new bundle */
        while (!OP_start_bundle(op)) op = OP_prev(op);
        if (cycles > 0) Pad_Cycles_Before(bb, op, cycles);

    } else { /* In the separate bundles */
        for (op = tgt_op; !OP_start_bundle(op); op = OP_prev(op));
        Pad_Cycles_Before(bb, op, cycles);
    }
}
/* ==================================================
 * Fill all latency for cache conflict, otherwise more
 * penalty.
 */
void Fix_Cache_Conflict_latency(BB *bb){
    INT op_order = 0;
    BOOL trace = Get_Trace(TP_A_CANA, 0x1);
    OP *op;
    std::vector <OP *> mem_ops;
    OP_MAP reverse_order = OP_MAP32_Create();
    FOR_ALL_BB_OPs_REV(bb,op) {	
        // backward can give cycle, and give the distance of
        // conflict ops
        // Only for load or store op

        if (OP_prev(op) && OP_end_group(OP_prev(op))) {
            op_order++;
        }

        if (OP_dummy(op) && OP_simulated(op)) continue;
        if (!OP_load(op) && !OP_store(op)) continue;

        if (OP_prev(op) && OP_end_group(OP_prev(op)))
            OP_MAP32_Set(reverse_order,op,op_order-1);
        else
            OP_MAP32_Set(reverse_order,op,op_order);
        
	for (INT i=0; i<mem_ops.size(); i++) {
            // op may be removed in Pad_Cycles
            if (OP_bb(mem_ops[i]) != bb) continue;
            INT distance=0;
            BOOL equal;
            INT succ_order = OP_MAP32_Get(reverse_order,mem_ops[i]);
            INT cur_order  = OP_MAP32_Get(reverse_order,op);
	    // determine conflict or not, return true or false.
            if (Cache_Has_Conflict(op,mem_ops[i], &distance, &equal)) {
                   if (!equal) continue;
                   INT current_dist = cur_order - succ_order;
                   Is_True(current_dist >=0, ("order is not accurate!")); 
                   if (distance > current_dist) {
                       if (trace) {
                           fprintf(TFile, "fill %d cycles with nop in BB %d between\n",
                                   distance-current_dist,
                                   BB_id(bb));
                           Print_OP_No_SrcLine(op);
                           fprintf(TFile, "and\n");
                           Print_OP_No_SrcLine(mem_ops[i]);
                       }
                       Pad_Cycles_Into(bb,op,mem_ops[i],distance-current_dist);

                       // Update op_order by adding distance-current_dist,
                       for (INT j=mem_ops.size()-1; j>=0; j--) {

                           // skip update for op after mem_ops or ops in same cycle.
                           // reverse order is not changed
                           if (mem_ops[j] == mem_ops[i]) break;
                           INT updated_order = OP_MAP32_Get(reverse_order,mem_ops[j]);
                           if (updated_order == succ_order) break;
                           updated_order = updated_order+distance-current_dist;
                           OP_MAP32_Set(reverse_order,mem_ops[j],updated_order);
                       }
                       cur_order += distance-current_dist;
                       OP_MAP32_Set(reverse_order,op,cur_order);
                       op_order  += distance-current_dist;
                   }
             }
	}

        mem_ops.push_back(op);
    }
}

/* return the max number of hidden operands the given <top> may have */
INT32 CGTARG_Max_Number_of_Hidden_Opnd (mTOP top) {
  if (TOP_is_call (top)) {
    return MAX_NUMBER_OF_REGISTER_PARAMETERS*2;
  } else if (top == TOP_br_ret) {
    return MAX_NUMBER_OF_REGISTERS_FOR_RETURN*2;
  }
  
  return 0;
}

/* Go through all OP in the current PU and and hidden their hidden operands
    + */
void CGTARG_Add_Implict_Operands (void) {

  vector<TN*> opnds(16);
  OP* op;

  for (BB* blk = REGION_First_BB; blk != NULL; blk = BB_next(blk)) {
    if (!BB_call(blk) && !BB_exit (blk)) continue;
    
    opnds.clear ();
    FOR_ALL_BB_OPs (blk, op) {
      for (INT i = 0; i < OP_results(op); i++) {
	TN* res = OP_result(op, i);
	if (!TN_is_register (res) || !TN_is_dedicated(res)) {
	  continue;
	}
	
	if (BB_call(blk)) {
	  PREG_NUM pnum = TN_To_PREG (res);
	  if ((Is_Int_Output_Preg (pnum) || Is_Fp_Output_Preg
	       (pnum)) &&
	      find(opnds.begin (), opnds.end(), res) == opnds.end())
	    {
	      opnds.push_back (res);
	    }
	} else if (Is_Return_Preg (TN_To_PREG (res)) &&
		   find(opnds.begin (), opnds.end(), res) ==
		   opnds.end()) {
	  opnds.push_back (res);
	}
      }/* end of for */
    } /* FOR_ALL_BB_OPs */
    
    if (opnds.size () != 0) {
      OP* xfer = BB_xfer_op (blk);
      Is_True (TOP_is_call (OP_code(xfer)) ||
	       OP_code(xfer) == TOP_br_ret,
	       ("expected xfer op is either call or return"));
      Add_Hidden_Operands (xfer, opnds);
    }
  }
}


/* return TRUE iff op is load with UNAT bit (IA64)*/
BOOL CGTARG_Load_with_UNAT (OP* op) {

   switch (OP_code(op)) {
   case TOP_ld8_fill:
   case TOP_ld8_r_fill:
   case TOP_ld8_i_fill:
   case TOP_ldf_fill:
   case TOP_ldf_r_fill:
   case TOP_ldf_i_fill:
       return TRUE;
   }

   return FALSE;
}

/* return TRUE iff op is store with UNAT bit (IA64) */
BOOL CGTARG_Store_With_UNAT (OP* op) {
  
   switch (OP_code(op)) {
   case TOP_st8_spill:
   case TOP_st8_i_spill:
   case TOP_stf_spill:
   case TOP_stf_i_spill:
       return TRUE;
   }
  
   return FALSE;
}
