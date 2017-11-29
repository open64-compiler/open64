/*
 * Copyright 2005-2008 NVIDIA Corporation.  All rights reserved.
 */

/*
 * Copyright 2003, 2004, 2005 PathScale, Inc.  All Rights Reserved.
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


/* ====================================================================
 * ====================================================================
 *
 * Module: cgtarget.cxx
 * $Revision: 1.135 $
 * $Date: 05/07/08 10:47:17-07:00 $
 * $Author: tkong@hyalite.keyresearch $
 * $Source: /scratch/mee/Patch0002-taketwo/kpro64-pending/be/cg/x8664/SCCS/s.cgtarget.cxx $
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
#include "be_symtab.h"
#include "cgir.h"
#include "cg.h"
#include "void_list.h"
#include "cg_dep_graph.h"
#include "cg_spill.h"
#include "cg_vector.h"
#include "whirl2ops.h"
#include "ti_errors.h"
#include "w2op.h"
#include "cgexp.h"
#include "targ_proc_properties.h"
#include "ti_bundle.h"
#include "hb_sched.h"
#include "hb_hazards.h"
#include "bb.h"
#include "op.h"
#include "op_list.h"
#include "calls.h"
#include "cgtarget.h"
#include "calls.h"
#include "config_lno.h"  // for LNO_Prefetch_Ahead
#include "cg_region.h"
#include "eh_region.h"
#include "cg_sched_est.h"

UINT32 CGTARG_branch_taken_penalty;
BOOL CGTARG_branch_taken_penalty_overridden = FALSE;

TOP CGTARG_Invert_Table[TOP_count+1];
TOP CGTARG_Immed_To_Reg_Table[TOP_count+1];

OPCODE CGTARG_Assoc_Base_Opr_Table[TOP_count];
mTOP CGTARG_Assoc_Base_Top_Table[TOP_count];
mTOP CGTARG_Assoc_Base_Fnc_Table[TOP_count];

mTOP CGTARG_Inter_RegClass_Copy_Table[ISA_REGISTER_CLASS_MAX+1][ISA_REGISTER_CLASS_MAX+1][2];

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
  const TOP topcode = OP_code(memop);

  FmtAssert( false, ("Unknown mem ref bytes: %s", TOP_Name(topcode)) );
  return 0;
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
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
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
  if ( ARC_kind(arc) == CG_DEP_PREBR && 
		  PROC_has_same_cycle_branch_shadow() )
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
  return FALSE;
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
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
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
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
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
    ratio[0] = 4;
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
    ratio[0] = 2;
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

  FmtAssert( madds == 0, ("madds != 0 ") );

  if (flops != 0) {
    BOOL unbalanced_fpu = FALSE;

    if ( madds_per_cycle[0] != 0 ) {
      fprintf(file,"%s%5d flop%1s        (%3d%% of peak)%s",
		 prefix,
		 flops,
		 Plural(flops),
		 Percent_Of_Peak(flops, ii, flops_per_cycle),
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

#ifdef __MINGW32__
  memset (info, 0, sizeof (PRC_INFO));
#else
  bzero (info, sizeof (PRC_INFO));
#endif /* __MINGW32__ */

  for ( op = BB_first_op(bb); op != NULL; op = OP_next(op) ) {
    INT num_insts = OP_Real_Ops (op);

    if (num_insts == 0) continue;

    info->refs[PRC_INST] += num_insts;

    if( OP_memory(op) ){
      ++info->refs[PRC_MEMREF];
    }

    if( OP_memory(op) ){
      ;
    }
    else if ( OP_flop(op) ) {
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
  TN *sym_tn;

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
  sym_tn = OP_opnd(op, OP_find_opnd_use(op,OU_base));
  if (TN_is_symbol(sym_tn) && !ST_is_weak_symbol(TN_var(sym_tn))) 
    goto scalar_load;

  /*  c) load of a fixed variable (base address is constant or
   *     known to be in bounds), comment out the rematerizable bit check 
   *     since it doesn;t guarantee safeness all the time.
   */
  if (/*   TN_is_rematerializable(OP_opnd(op, OP_find_opnd_use(op,OU_offset))) || */
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
  return FALSE;
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
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
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
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
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
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
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
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
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
  /* Get the target register number and class associated with the
   * preg, if there is one that is.
   */
  if (!Preg_Is_Dedicated(preg))
    return FALSE;

  if (!Preg_Offset_Is_Int(preg)   &&
      !Preg_Offset_Is_Float(preg) )
    return FALSE;

  /* Get the target register number and class associated with the
   *    * preg, if there is one that is.
   */
  if (Preg_Offset_Is_Int32(preg)) {
    rclass = ISA_REGISTER_CLASS_integer;
    if (Is_Return_Preg(preg))
      regnum = ABI_PROPERTY_integer_func_val_First_Register
        + preg - First_Int32_Preg_Return_Offset;
    else if (Is_Formal_Preg(preg))
      regnum = ABI_PROPERTY_integer_func_arg_First_Register
        + preg - First_Int32_Preg_Param_Offset;
    else
      return FALSE;
  }
  else if (Preg_Offset_Is_Float32(preg)) {
    rclass = ISA_REGISTER_CLASS_float;
    if (Is_Return_Preg(preg))
      regnum = ABI_PROPERTY_float_func_val_First_Register
        + preg - First_Float32_Preg_Return_Offset;
    else if (Is_Formal_Preg(preg))
      regnum = ABI_PROPERTY_float_func_arg_First_Register
        + preg - First_Float32_Preg_Param_Offset;
    else
      return FALSE;
  }
  else if (Preg_Offset_Is_Int64(preg)) {
    rclass = ISA_REGISTER_CLASS_integer64;
    if (Is_Return_Preg(preg))
      regnum = ABI_PROPERTY_integer64_func_val_First_Register
        + preg - First_Int64_Preg_Return_Offset;
    else if (Is_Formal_Preg(preg))
      regnum = ABI_PROPERTY_integer64_func_arg_First_Register
        + preg - First_Int64_Preg_Param_Offset;
    else
      return FALSE;
  }
  else if (Preg_Offset_Is_Float64(preg)) {
    rclass = ISA_REGISTER_CLASS_float64;
    if (Is_Return_Preg(preg))
      regnum = ABI_PROPERTY_float64_func_val_First_Register 
        + preg - First_Float64_Preg_Return_Offset;
    else if (Is_Formal_Preg(preg))
      regnum = ABI_PROPERTY_float64_func_arg_First_Register
        + preg - First_Float64_Preg_Param_Offset;
    else
      return FALSE;
  }
  else {
    return FALSE;
  }

  /* Find the CG register for the target register and class. */
  for ( REGISTER reg = REGISTER_MIN;
	reg <= REGISTER_CLASS_last_register(rclass);
	reg++ )
  {
    if ( REGISTER_machine_id(rclass,reg) == regnum )
    {
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
  *mispredict = 4;
  *fixed = 4;
  *brtaken = 4;
  *factor = 1.0;

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
INT32 CGTARG_Latency( TOP op )
{
  FmtAssert( FALSE, ("NYI") );
  return 1;
}

BOOL CGTARG_Is_Long_Latency(TOP op)
{
  return ( CGTARG_Latency(op) > 2 );
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
  OP *cmp;
  return CGTARG_Analyze_Compare (br, tn1, tn2, &cmp);
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
  VARIANT variant = V_BR_NONE;
  *tn1 = NULL;
  *tn2 = NULL;
  *compare_op = NULL;

  switch (OP_code(br)) {
  case TOP_bra_uni:
    return V_BR_ALWAYS;
  case TOP_bra_uni_p:
  case TOP_bra_uni_np:
  case TOP_bra_p:
  case TOP_bra_np:
    *tn1 = OP_opnd(br, OP_PREDICATE_OPND);
    variant = V_BR_P_TRUE;
    break;
  default:
    FmtAssert( FALSE, ("unexpected branch") );
  }

  // Attempt to find the defining OP for the tested value.
  DEF_KIND kind;
  *compare_op = TN_Reaching_Value_At_Op(*tn1, br, &kind, TRUE);
  // FmtAssert(*compare_op, ("no compare for branch in bb%d?", BB_id(OP_bb(br))));
  if (*compare_op == NULL) {
    DevWarn("no compare for branch in bb%d?", BB_id(OP_bb(br)));
    return variant;
  }

  switch (OP_code(*compare_op)) {
        case TOP_setp_eq_s16:
        case TOP_setp_eq_s32:
		variant = V_BR_I4EQ;
		break;
        case TOP_setp_eq_s64:
		variant = V_BR_I8EQ;
		break;
        case TOP_setp_eq_u16:
        case TOP_setp_eq_u32:
		variant = V_BR_U4EQ;
		break;
        case TOP_setp_eq_u64:
		variant = V_BR_U8EQ;
		break;
        case TOP_setp_eq_f32:
		variant = V_BR_FEQ;
		break;
        case TOP_setp_eq_f64:
		variant = V_BR_DEQ;
		break;
        case TOP_setp_ne_s16:
        case TOP_setp_ne_s32:
		variant = V_BR_I4NE;
		break;
        case TOP_setp_ne_s64:
		variant = V_BR_I8NE;
		break;
        case TOP_setp_ne_u16:
        case TOP_setp_ne_u32:
		variant = V_BR_U4NE;
		break;
        case TOP_setp_ne_u64:
		variant = V_BR_U8NE;
		break;
        case TOP_setp_ne_f32:
		variant = V_BR_FNE;
		break;
        case TOP_setp_ne_f64:
		variant = V_BR_DNE;
		break;
        case TOP_setp_lt_s16:
        case TOP_setp_lt_s32:
		variant = V_BR_I4LT;
		break;
        case TOP_setp_lt_s64:
		variant = V_BR_I8LT;
		break;
        case TOP_setp_lt_u16:
        case TOP_setp_lt_u32:
		variant = V_BR_U4LT;
		break;
        case TOP_setp_lt_u64:
		variant = V_BR_U8LT;
		break;
        case TOP_setp_lt_f32:
		variant = V_BR_FLT;
		break;
        case TOP_setp_lt_f64:
		variant = V_BR_DLT;
		break;
        case TOP_setp_le_s16:
        case TOP_setp_le_s32:
		variant = V_BR_I4LE;
		break;
        case TOP_setp_le_s64:
		variant = V_BR_I8LE;
		break;
        case TOP_setp_le_u16:
        case TOP_setp_le_u32:
		variant = V_BR_U4LE;
		break;
        case TOP_setp_le_u64:
		variant = V_BR_U8LE;
		break;
        case TOP_setp_le_f32:
		variant = V_BR_FLE;
		break;
        case TOP_setp_le_f64:
		variant = V_BR_DLE;
		break;
        case TOP_setp_gt_s16:
        case TOP_setp_gt_s32:
		variant = V_BR_I4GT;
		break;
        case TOP_setp_gt_s64:
		variant = V_BR_I8GT;
		break;
        case TOP_setp_gt_u16:
        case TOP_setp_gt_u32:
		variant = V_BR_U4GT;
		break;
        case TOP_setp_gt_u64:
		variant = V_BR_U8GT;
		break;
        case TOP_setp_gt_f32:
		variant = V_BR_FGT;
		break;
        case TOP_setp_gt_f64:
		variant = V_BR_DGT;
		break;
        case TOP_setp_ge_s16:
        case TOP_setp_ge_s32:
		variant = V_BR_I4GE;
		break;
        case TOP_setp_ge_s64:
		variant = V_BR_I8GE;
		break;
        case TOP_setp_ge_u16:
        case TOP_setp_ge_u32:
		variant = V_BR_U4GE;
		break;
        case TOP_setp_ge_u64:
		variant = V_BR_U8GE;
		break;
        case TOP_setp_ge_f32:
		variant = V_BR_FGE;
		break;
        case TOP_setp_ge_f64:
		variant = V_BR_DGE;
		break;
        case TOP_setp_neu_f32:
		variant = V_BR_FUO;
		break;
        case TOP_setp_neu_f64:
		variant = V_BR_DUO;
		break;
        case TOP_not_pred:
                variant = V_BR_PNE;
		break;
        case TOP_mov_pred: 
                variant = V_BR_PEQ;
		break;
        case TOP_xor_pred:
	       if (OP_code(br) == TOP_bra_p) {
		 variant = V_BR_PNE;
	       } else if (OP_code(br) == TOP_bra_np) {
		 variant = V_BR_PEQ;
	       } else {
		 FmtAssert(FALSE, ("incompatible branch op with xor compare"));
	       }
	       break;
	default:
		FmtAssert(FALSE, ("defining op not a recognized compare"));
  } 
  *tn1 = OP_opnd(*compare_op, 0);
  *tn2 = OP_opnd(*compare_op, 1);
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
  return TOP_UNDEFINED;
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
#define Set_Inv_Table(a,b)        \
   do {                           \
     CGTARG_Invert_Table[a] = b;  \
     CGTARG_Invert_Table[b] = a;  \
   } while( 0 )

#define Set_Immed_To_Reg_Table(a,b)    \
   do {                                \
     CGTARG_Immed_To_Reg_Table[a] = b; \
     CGTARG_Immed_To_Reg_Table[b] = a; \
   } while( 0 )

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
//  Set_Inv_Table( TOP_addss, TOP_subss );

  /* Init table for CGTARG_Immed_To_Reg:
   */
//  Set_Immed_To_Reg_Table( TOP_addi32, TOP_add32 );


  /* Init table for CGTARG_Inter_RegClass_Copy_Table:
   */
  CGTARG_Inter_RegClass_Copy_Table[ISA_REGISTER_CLASS_float]
				  [ISA_REGISTER_CLASS_integer]
				  [FALSE] = TOP_UNDEFINED;
  CGTARG_Inter_RegClass_Copy_Table[ISA_REGISTER_CLASS_float]
				  [ISA_REGISTER_CLASS_integer]
				  [TRUE]  = TOP_UNDEFINED;

  CGTARG_Inter_RegClass_Copy_Table[ISA_REGISTER_CLASS_integer]
				  [ISA_REGISTER_CLASS_float]
				  [FALSE] = TOP_UNDEFINED;
  CGTARG_Inter_RegClass_Copy_Table[ISA_REGISTER_CLASS_integer]
				  [ISA_REGISTER_CLASS_float]
				  [TRUE]  = TOP_UNDEFINED;

#undef Set_Inv_Table
#undef Set_Immed_To_Reg_Table

  return;
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
  Exp_Load(mtype, mtype, tn, mem_loc, 0, ops, 0);
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
  Exp_Store(mtype, tn, mem_loc, 0, ops, 0);
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
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
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
  const TOP opr = OP_code(op);

  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));

  if( OP_cond_move( op ) &&
      TNs_Are_Equivalent( OP_result(op,0), OP_opnd(op,0) ) ){
    return 0;
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
BOOL CGTARG_Can_Fit_Immediate_In_Add_Instruction (INT64 immed)
{
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
  //return ISA_LC_Value_In_Class (immed, LC_simm32);
}


/* ====================================================================
 *
 * CGTARG_Can_Load_Immediate_In_Single_Instruction
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL CGTARG_Can_Load_Immediate_In_Single_Instruction (INT64 immed)
{
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
  //return ISA_LC_Value_In_Class (immed, LC_simm32);
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
    FmtAssert( FALSE, ( "CGTARG_Which_OP_Select: Unsupported Target") );
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
  return TOP_UNDEFINED;
}


static BOOL OP_Reads_Dedicated_TN( OP* op, TN* ded_tn )
{
  for( int i = 0; i < OP_opnds(op); i++ ){
    TN* tmp_tn = CGTARG_Gen_Dedicated_Subclass_TN( op, i, FALSE );
    if( tmp_tn != NULL &&
	TNs_Are_Equivalent( tmp_tn, ded_tn ) )
      return TRUE;
  }

  return FALSE;
}


static BOOL OP_Writes_Dedicated_TN( OP* op, TN* ded_tn )
{
  for( int i = 0; i < OP_results(op); i++ ){
    TN* tmp_tn = CGTARG_Gen_Dedicated_Subclass_TN( op, i, TRUE );
    if( tmp_tn != NULL &&
	TNs_Are_Equivalent( tmp_tn, ded_tn ) )
      return TRUE;
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
BOOL CGTARG_Dependence_Required( OP* pred_op, OP* succ_op )
{
  FmtAssert( OP_bb(pred_op) == OP_bb(succ_op), ("pred==succ NYI") );

  FmtAssert( FALSE, ("NYI") );
  /* Do not change the relative order for operations that store callee-saved
     registers for exception handling code. Refer to bug#1928 for detail.
     (bug#2205)
  */

  if( PU_Has_Exc_Handler ){
    /* Don't bother to scan thru <Saved_Callee_Saved_Regs> one-by-one to save
       us some compilation time.
    */
    if( BB_entry( OP_bb(pred_op) ) &&
	OP_store( pred_op )        &&
	OP_store( succ_op )        &&
	TN_is_save_reg( OP_opnd(pred_op,0) ) )
      return TRUE;
  }

  /* If exists a dependence between <pred_op> and <succ_op>, then we can return
     to avoid generating superfluous MISC arcs.
  */
  {
    for( ARC_LIST* arcs = OP_succs(pred_op);
	 arcs != NULL;
	 arcs = ARC_LIST_rest(arcs) ){
      ARC *arc = ARC_LIST_first(arcs);
      if( ARC_succ(arc) == succ_op )
	return FALSE;
    }
  }

  /* A sp adjustment operation should always be scheduled before a store operation,
     which could access the stack potentially.
  */
  {
    if( OP_results(pred_op) > 0              && 
	TN_is_sp_reg( OP_result(pred_op,0) ) &&
	OP_store( succ_op ) )
      return TRUE;
  }

  /* Bug #336
     Don't schedule an op before/after the entry/exit stack
     adjustment code; otherwise will cause spilling error if
     this op is spilled later (before the stack is formed).
   */
  {
    BB* bb = OP_bb( pred_op );
    if( BB_entry(bb) && !BB_handler(bb) ){
      if( pred_op == BB_entry_sp_adj_op(bb) )
	return TRUE;
    }

    if( BB_exit(bb) ){
      if( succ_op == BB_exit_sp_adj_op(bb) )
	return TRUE;
    }
  }


  /* Return TRUE if <pred_op> can change rflags, and <succ_op> is a
     comparison op. */

  /* Return TRUE for case where <succ_op>, like div, will over-write
     some registers implicitly, and these registers are used by <pred_op> also.
     However, there other way around will be the wrong code sequence introduced
     by some other optimizations, like ebo.
  */
  {
    for( int i = 0; i < OP_opnds(succ_op); i++ ){
      TN* tmp_tn = CGTARG_Gen_Dedicated_Subclass_TN( succ_op, i, FALSE );
      if( tmp_tn != NULL ){
	// for RAW
	if( OP_Writes_Dedicated_TN( pred_op, tmp_tn ) )
	  return TRUE;

	/* Bug#352
	   For an operation like shift, its opnd will be re-defined at
	   function Preallocate_Single_Register_Subclasses().
	   Thus, an implicit WAR is imposed here.
	*/
	if( !TN_is_dedicated( OP_opnd(succ_op,i) ) &&
	    OP_Reads_Dedicated_TN( pred_op, tmp_tn ) )
	  return TRUE;
      }
    }

    for( int i = 0; i < OP_results(succ_op); i++ ){
      TN* tmp_tn = CGTARG_Gen_Dedicated_Subclass_TN( succ_op, i, TRUE );
      if( tmp_tn != NULL ){
	// for WAW or WAR
	if( OP_Writes_Dedicated_TN( pred_op, tmp_tn ) ||
	    OP_Reads_Dedicated_TN( pred_op, tmp_tn ) ){
	  return TRUE;
	}
      }
    }
  }

  /* Misc. requirements.
   */

  return FALSE;
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
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
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
  return FALSE;
}

/* ====================================================================
 *
 * CGTARG_Generate_Countdown_Loop
 *
 * Tranform a loop that looks like:
 *    do i = 0, N, i ++
 *      body
 *    enddo
 * to:
 *    do i = N, 0, i --
 *	body
 *    enddo
 * The advantage of this tranformation is two-fold:
 *    (1) we can make use of the dec instruction in x86 and get rid of 
 *        extra moves/loop control variable update.
 *    (2) free a register used as a temporary to hold the loop
 *        control variable (instead we count down the upper bound).
 *
 * - inspired by bug 1254.
 *
 * ====================================================================
 */
void
CGTARG_Generate_Countdown_Loop ( TN *trip_count_tn,
				 BB *tail, 
				 OPS *prolog_ops,
				 OPS *body_ops, 
				 BOOL single_bb, 
				 LOOP_DESCR *loop )
{ 
  OP *cmp, *incr, *op, *branch;
  BOOL cmp_found = FALSE, incr_found = FALSE;
  INT opnd, result;

  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
  
  branch = BB_branch_op(tail);
  incr = cmp = NULL;


  return;  
}

/* Convert temporal stores to non-temporal stores if the amount of data that
   <loop> will access is larger than the cache can provide.
 */
void CGTARG_LOOP_Optimize( LOOP_DESCR* loop )
{
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
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
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
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
                           const WN* asm_wn, 
			   TYPE_ID type)
{
  // skip constraint modifiers:
  // = input and output parameters are separated in the WHIRL for ASM
  // & early_clobber flag is set in Handle_ASM
  // % commutativity of operands is ignored for now
  static const char* modifiers = "=&%";
  while (strchr(modifiers, *constraint))
  {
    constraint++;
  }
  
  // TODO: we should really look at multiple specified constraints
  // and the load in order to select the best TN, but for now we
  // assume that when we get here we can safely pick a TN

  // if 'm' or 'g' is one of the choices, always prefer that one
  // TODO: we decide this in the front end, but it's not optimal
  if (*constraint != 'm' && *constraint != 'g')
  {
    const char* m = constraint;
    while (*++m)
    {
      if (*m == 'm' || *m == 'g')
      {
        constraint = m;
        break;
      }
    }
  }
  
  TN* ret_tn;
  
  // digit constraint means that we should reuse a previous operand
  if (isdigit(*constraint))
  {
    // TODO: make sure that frontend checks that string is number
    INT prev_index = strtol(constraint, NULL, 10);
    if (prev_index < 0 || prev_index >= asm_constraint_index ||
        ! asm_constraint_tn[prev_index] ) {
       FmtAssert( FALSE, ("invalid matching constraint reference") );
    }
    ret_tn = asm_constraint_tn[prev_index];
  }
  else if (strchr("m", *constraint) || strchr("g", *constraint))
  {
    TYPE_ID rtype = (load != NULL ? WN_rtype(load) : MTYPE_I4);
    FmtAssert(MTYPE_is_integral(rtype),
              ("ASM operand does not satisfy its constraint %s", constraint));
    // for ptx, use symbol name rather than load into register 
    // ret_tn = (pref_tn ? pref_tn : Build_TN_Of_Mtype(rtype));
    if (pref_tn) ret_tn = pref_tn;
    else {
    	FmtAssert(load, ("no load for asm m constraint"));
	if (ST_class(WN_st(load)) == CLASS_PREG) {
		// use tn home to find symbol (may be reset in exp_ldst)
		load = TN_home(PREG_To_TN(WN_st(load),WN_load_offset(load)));
	}
	ret_tn = Gen_Symbol_TN (WN_st(load), 0, 0);
        // This is a hack, but if is a __T compiler temp,
        // look for the earlier store to that temp and use that info.
        // In particular, look for earlier tex variable.
	// This can happen with -O0 compiles since no wopt to propagate syms.
        if (strncmp(ST_name(WN_st(load)), "__cuda___T", 10) == 0) {
          OP *op;
          DevWarn("asm m param is __T temp, so replace with earlier def");
          FOR_ALL_OPS_OPs_REV(&New_OPs,op) {
            if (OP_store(op)) {
              TN *sym_tn = OP_opnd(op, OP_find_opnd_use(op,OU_base));
              if (TN_is_symbol(sym_tn) && TN_var(sym_tn) == WN_st(load)) {
                ret_tn = OP_opnd(op, OP_find_opnd_use(op,OU_storeval));
                if (TN_in_texture_mem(ret_tn) && TN_home(ret_tn) != NULL) {
                  DevWarn("texture ret_tn");
                  ret_tn = Gen_Symbol_TN (WN_st(TN_home(ret_tn)), 0, 0);
                  break;
                }
              }
            }
          }
        }
    }
  }
  else if ((*constraint == 'h'))
  {
    TYPE_ID mtype = MTYPE_I2;
    if (load)
      mtype = WN_desc(load);
    FmtAssert(MTYPE_is_integral(mtype) && MTYPE_bit_size(mtype) == 16,
              ("ASM operand does not satisfy its constraint %s", constraint));
    ret_tn = (pref_tn ? pref_tn : Build_TN_Of_Mtype(mtype));
  }
  else if ((*constraint == 'r'))
  {
    TYPE_ID mtype = MTYPE_I4;
    if (load)
      mtype = WN_rtype(load);
    FmtAssert(MTYPE_is_integral(mtype) && MTYPE_bit_size(mtype) == 32,
              ("ASM operand does not satisfy its constraint %s", constraint));
    ret_tn = (pref_tn ? pref_tn : Build_TN_Of_Mtype(mtype));
  }
  else if ((*constraint == 'l'))
  {
    TYPE_ID mtype = MTYPE_I8;
    if (load)
      mtype = WN_rtype(load);
    FmtAssert(MTYPE_is_integral(mtype) && MTYPE_bit_size(mtype) == 64,
              ("ASM operand does not satisfy its constraint %s", constraint));
    ret_tn = (pref_tn ? pref_tn : Build_TN_Of_Mtype(mtype));
  }
  else if (*constraint == 'f')
  {
    TYPE_ID mtype = MTYPE_F4;    
    if (load)
      mtype = WN_rtype(load);
    FmtAssert(MTYPE_is_float(mtype) && MTYPE_bit_size(mtype) == 32,
              ("ASM operand does not satisfy its constraint %s", constraint));
    ret_tn = (pref_tn ? pref_tn : Build_TN_Of_Mtype(mtype));
  }
  else if (*constraint == 'd')
  {
    TYPE_ID mtype = MTYPE_F8;    
    if (load)
      mtype = WN_rtype(load);
    FmtAssert(MTYPE_is_float(mtype) && MTYPE_bit_size(mtype) == 64,
              ("ASM operand does not satisfy its constraint %s", constraint));
    ret_tn = (pref_tn ? pref_tn : Build_TN_Of_Mtype(mtype));
  }
  else if (*constraint == 'n')
  {
    FmtAssert(load && WN_operator(load) == OPR_INTCONST,
              ("Cannot find immediate operand for ASM"));
    ret_tn = Gen_Literal_TN(WN_const_val(load),
                            MTYPE_byte_size(WN_rtype(load)));
  }
  else if (*constraint == 's')
  {
    ST *st;
    TY_IDX ty;
    INT64 offset;
    FmtAssert(load, ("no whirl for constraint s?"));
    if (WN_operator(load) == OPR_LDID
      && ST_class(WN_st(load)) == CLASS_PREG)
    {
      // use tn home to find symbol (may be reset in exp_ldst)
      load = TN_home(PREG_To_TN(WN_st(load),WN_load_offset(load)));
    }
    if (WN_operator(load)==OPR_LDID) {
      st = WN_st(load);
      offset = WN_offset(load);
      ty = WN_ty(load);
    }
    else if (WN_operator(load)==OPR_ILOAD
      && WN_operator(WN_kid0(load)) == OPR_ADD
      && WN_operator(WN_kid0(WN_kid0(load))) == OPR_LDA
      && WN_operator(WN_kid1(WN_kid0(load))) == OPR_INTCONST)
    {
      // iload(add(lda sym,const)) is same as ldid(const,sym)
      st = WN_st(WN_kid0(WN_kid0(load)));
      offset = WN_const_val(WN_kid1(WN_kid0(load)));
      ty = WN_ty(load);
    }
    else if (WN_operator(load) == OPR_LDA) {
      st = WN_st(load);
    }
    else
      FmtAssert(FALSE, ("unexpected whirl for constraint s"));

    if (ST_class(st) != CLASS_CONST) {
      INITV_IDX inv = ST_is_const_and_has_initv(st);
      FmtAssert(inv, ("s constraint operand not const initialized (%s)", ST_name(st)));
      if (INITV_kind(inv) == INITVKIND_BLOCK
        && TY_kind(ty) == KIND_POINTER)
      {
        // index into array
        INT index = offset / TY_size(ty);
        inv = INITV_blk(inv);
        INT i;
        for (i = 0; i < index; ++i) 
          inv = INITV_next(inv);
      }
      FmtAssert(INITV_kind(inv) == INITVKIND_SYMOFF, ("s constraint operand not a string?"));
      st = &St_Table[INITV_st(inv)]; 
    }
    FmtAssert(ST_class(st) == CLASS_CONST, ("s constraint operand not constant"));
    FmtAssert(TCON_ty(STC_val(st)) == MTYPE_STR, ("s constraint operand not a string"));
    ret_tn = Gen_Symbol_TN(st, 0, 0);
  }
  else
  {
    FmtAssert(FALSE, ("ASM constraint <%s> not supported", constraint));
  }

  asm_constraint_tn[asm_constraint_index] = ret_tn;
  asm_constraint_index++;
  
  return ret_tn;
}


static char *
Get_TN_Assembly_Name (TN *tn)
{
  return "moo";
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
	case 'h':
	case 'l':
		FmtAssert(MTYPE_is_integral(mtype), 
			("ASM constraint is integer but parameter is not"));
		break;
	case 'f':
	case 'd':
		FmtAssert(MTYPE_is_float(mtype), 
			("ASM constraint is float but parameter is not"));
		break;
	case 'm':
	case 'g':
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
	case '9': {
                i = strtol(constraint, NULL, 10);
                if (i < 0 || i >= asm_constraint_index || ! asm_constraint_tn[i] ) {
                    FmtAssert( FALSE, ("invalid matching constraint reference") );
                }

                *tn = asm_constraint_tn[i];
                asm_constraint_tn[asm_constraint_index] = *tn;

                *name = asm_constraint_name[i];
                strcpy(asm_constraint_name[asm_constraint_index],*name);

		++asm_constraint_index;
		return;
        }
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
	if (*constraint == 'm' || *constraint == 'g') {
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

static const char* int_reg_names[3][16] = {
  /* BYTE_REG: low 8-bit */
  { "%al", "%bl", "%bpl", "%spl", "%dil", "%sil", "%dl", "%cl",
    "%r8b",  "%r9b",  "%r10b", "%r11b", "%r12b", "%r13b", "%r14b", "%r15b" },
  /* WORD_REG: 16-bit */
  { "%ax", "%bx", "%bp", "%sp", "%di", "%si", "%dx", "%cx",
    "%r8w",  "%r9w",  "%r10w", "%r11w", "%r12w", "%r13w", "%r14w", "%r15w" },
  /* DWORD_REG: 32-bit */
  { "%eax", "%ebx", "%ebp", "%esp", "%edi", "%esi", "%edx", "%ecx",
    "%r8d",  "%r9d",  "%r10d", "%r11d", "%r12d", "%r13d", "%r14d", "%r15d" },
};
      
const char* 
CGTARG_Modified_Asm_Opnd_Name(char modifier, TN* tn, char *tn_name)
{
  return tn_name;

  if (TN_register_class(tn) == ISA_REGISTER_CLASS_float)
    return tn_name;

  if (modifier == 'r') {
    if (TN_size(tn) == 8)
      return tn_name;
    else { 
      // Bugs 482, 505, 626
      INT sub_reg_class = 2; // DWORD_REG
      if (TN_size(tn) == 2)
	sub_reg_class = 1;   // WORD_REG
      else if (TN_size(tn) == 1)
	sub_reg_class = 0;   // BYTE_REG
      const ISA_REGISTER_CLASS rc = ISA_REGISTER_CLASS_integer;
      for( REGISTER reg = REGISTER_MIN; reg <= REGISTER_CLASS_last_register( rc ); reg++ ){
	const char* n = REGISTER_name( rc, reg );
	if( strcmp( n, tn_name ) == 0 ){
	  const char *regname;
	  regname = int_reg_names[sub_reg_class][reg-REGISTER_MIN];
	  return regname;
	}
      }      
    } 
  }
  else {
    FmtAssert(FALSE, ("Unknown ASM operand modifier '%c'", modifier));
  }
  /*NOTREACHED*/
}

/* For the "m" constraint, there is no need to introduce op like
           TN108 :- lea32 TN4(%rsp) (sym:size+0)
   otherwise, all the registers will be used up pretty soon.
   The fix here is to remove the op that computes the address,
   and put the offset tn into opnd[num_opnds]. According this
   offset tn, later phase in Modify_Asm_String will generate
   the right offset and base info.   (bug#3111)
*/
TN* CGTARG_Process_Asm_m_constraint( WN* load, void** offset, OPS* ops )
{
  Is_True( load != NULL, ("Asm_m_constraint: load is NULL") );
  TN* asm_opnd = NULL;

  if( WN_operator(load) == OPR_LDA ){
    OP* lda_op = OPS_last( ops );
    asm_opnd = OP_iadd(lda_op) ? OP_opnd( lda_op, 1 ) : OP_opnd( lda_op, 0 );
    OPS_Remove_Op( ops, lda_op );

  } else if( WN_operator(load) == OPR_ADD ){
    OP* add_op = OPS_last( ops );
    TN* ofst_tn = OP_opnd( add_op, 1 );

    if( !TN_is_constant(ofst_tn) )
      return NULL;

    *offset = (void*)Gen_Literal_TN( TN_value(ofst_tn), 4 );

    asm_opnd = OP_opnd( add_op, 0 );
    OPS_Remove_Op( ops, add_op );

    /* Do some pattern matching to save one register by removing
       duplicated load.
    */

    OP* ld_op = OPS_last(ops);
    if( ld_op != NULL  &&
	OP_load(ld_op) &&
	OP_result(ld_op,0) == asm_opnd ){

      for( OP* prev_ld = OP_prev(ld_op);
	   prev_ld != NULL;
	   prev_ld = OP_prev(prev_ld) ){

	if( OP_store(prev_ld) )
	  break;

	if( OP_load(prev_ld) &&
	    OP_opnds(prev_ld) == OP_opnds(ld_op) ){
	  bool match = true;
	  for( int i = 0; i < OP_opnds(ld_op); i++ ){
	    if( OP_opnd(prev_ld,i) != OP_opnd(ld_op,i) ){
	      match = false;
	      break;
	    }
	  }

	  if( match ){
	    OPS_Remove_Op( ops, ld_op );
	    asm_opnd = OP_result( prev_ld, 0 );
	    break;
	  }
	}
      }
    }

  } else if( WN_operator(load) == OPR_LDID ){
    ;

  } else {
    DevWarn( "Asm_m_constraint: Unsupported opcode (%s)",
	     OPCODE_name(WN_opcode(load)) );
    return NULL;
  }

  return asm_opnd;
}

/* ====================================================================
 *
 * CGTARG_Postprocess_Asm_String: currently no-op for IA-64
 *
 * ====================================================================
 */
void 
CGTARG_Postprocess_Asm_String (char*)
{
}

/* ====================================================================
 *
 * CGTARG_Unconditional_Compare
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL CGTARG_Unconditional_Compare(OP *op, TOP* uncond_ver)
{
  return FALSE;
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
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
  return TOP_UNDEFINED;
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
  if( OP_has_predicate(op) ) {
    FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
  } else {
      {
	Set_OP_cond_def_kind(op, OP_ALWAYS_UNC_DEF);
      }
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
  /* This doesn't seem to be used. */
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
  return TOP_UNDEFINED;
}

////////////////////////////////////////////////////////////////
// If a BB ends in an unconditional branch, turn it into a conditional branch 
// With TRUE predicate, so we can predicate with something else later.
// If we can't find an unconditional branch, just give up and do nothing
//
void
Make_Branch_Conditional(BB *bb)
{
  return;
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
  switch(Eager_Level) {
  case EAGER_NONE:
    return FALSE;
  case EAGER_SAFE:
    if (OP_fadd(op) || 
	OP_fdiv(op) || 
	OP_fsub(op) || 
	OP_fmul(op) || 
	OP_load(op) ||
	OP_store(op) ||
	OP_prefetch(op) || 
	// idiv, imul use hilo registers
	OP_idiv(op) ||
	OP_imul(op) )
      return FALSE;
    else
      return TRUE;      
  case EAGER_ARITH:
    if (OP_load(op) ||
	OP_store(op) ||
	OP_prefetch(op) ||
	// Divide by zero
	OP_fdiv(op) || 
	OP_idiv(op) ||
	OP_imul(op) )
      return FALSE;
    else
      return TRUE;
  case EAGER_DIVIDE:
    if (OP_load(op) ||
	OP_store(op) ||
	OP_prefetch(op) ||
	OP_idiv(op) ||
	OP_imul(op) )
      return FALSE;
    else
      return TRUE;
  case EAGER_MEMORY:
  case EAGER_OTHER:
    if (OP_idiv(op) ||
	OP_imul(op) )
      return FALSE;
    else
      return TRUE;
  default:
    FmtAssert(FALSE, ("Handle this case"));
    return FALSE;
  }
}


TN* CGTARG_Gen_Dedicated_Subclass_TN( OP* op, int idx, BOOL is_result )
{
  const ISA_REGISTER_SUBCLASS subclass = is_result ?
    OP_result_reg_subclass( op, idx ) : OP_opnd_reg_subclass( op, idx );
  const REGISTER_SET subclass_regs = REGISTER_SUBCLASS_members(subclass);

  if( REGISTER_SET_Size(subclass_regs) != 1 ){
    TN* tn = is_result ? OP_result( op, idx ) : OP_opnd( op, idx );
    return TN_is_dedicated(tn) ? tn : NULL;
  }

  const REGISTER reg = REGISTER_SET_Choose(subclass_regs);
  const ISA_REGISTER_CLASS rc = REGISTER_SUBCLASS_register_class(subclass);

  return Build_Dedicated_TN( rc, reg, 0 );
}

ISA_LIT_CLASS
Lit_Class_For_Mtype (TYPE_ID mtype)
{
  switch (mtype) {
  case MTYPE_I1: return LC_i8;
  case MTYPE_U1: return LC_u8;
  case MTYPE_I2: return LC_i16;
  case MTYPE_U2: return LC_u16;
  case MTYPE_I4: return LC_i32;
  case MTYPE_U4: return LC_u32;
  case MTYPE_I8: return LC_i64;
  case MTYPE_U8: return LC_u64;
  case MTYPE_F4: return LC_f32;
  case MTYPE_F8: return LC_f64;
  default: return LC_UNDEFINED;
  }
}

BOOL
TN_Can_Use_Constant_Value (TN *tn, TYPE_ID mtype, INT64 *val)
{
  if (TN_has_value(tn))
        *val = TN_value(tn);
  else if (TN_is_rematerializable(tn) && WN_operator(TN_home(tn)) == OPR_INTCONST)
        *val = WN_const_val(TN_home(tn));
  else
        return FALSE;

  return TRUE;

  // if fits in instruction, use literal version
  return ISA_LC_Value_In_Class (*val, Lit_Class_For_Mtype(mtype));
}

static REGISTER_SET avail_set[ISA_REGISTER_CLASS_MAX+1];
static REGISTER last_reg_allocated[ISA_REGISTER_CLASS_MAX+1];

void
Assign_Virtual_Register (TN *tn)
{
  ISA_REGISTER_CLASS cl;
  REGISTER reg;
  if (TN_Is_Allocatable(tn) && TN_register(tn) == REGISTER_UNDEFINED) {
    cl = TN_register_class(tn);
    reg = REGISTER_SET_Choose(avail_set[cl]);
    FmtAssert(reg != REGISTER_UNDEFINED, ("ran out of registers in %s", 
	ISA_REGISTER_CLASS_INFO_Name(ISA_REGISTER_CLASS_Info(cl))));

    TN_Allocate_Register( tn, reg );
    last_reg_allocated[cl] = reg;
    if (Get_Trace(TP_ALLOC,1))
      fprintf(TFile, "assign reg %d to TN %d\n", reg, TN_number(tn));
    avail_set[cl] = REGISTER_SET_Difference1 (avail_set[cl], reg);
  }
}

// iterate thru code, assigning virtual registers
void
Assign_Virtual_Registers(void)
{
  BB *bb;
  OP *op;
  TN *tn;
  ISA_REGISTER_CLASS cl;
  FOR_ALL_ISA_REGISTER_CLASS(cl) {
    avail_set[cl] = REGISTER_CLASS_allocatable(cl);
    last_reg_allocated[cl] = REGISTER_UNDEFINED;
  }
  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    FOR_ALL_BB_OPs (bb, op) {
      for (INT i = 0; i < OP_results(op); i++) {
        tn = OP_result(op,i);
        Assign_Virtual_Register(tn);
      }
      for (INT i = 0; i < OP_opnds(op); i++) {
        tn = OP_opnd(op,i);
        Assign_Virtual_Register(tn);
      }
    }
  }
}

REGISTER
Last_Reg_Allocated (ISA_REGISTER_CLASS cl)
{
  return last_reg_allocated[cl];
}


// things that are not defined cause only doing a subset
extern "C" void EH_Generate_Range_List(WN * pu) { }
extern "C" void EH_Set_End_Label(EH_RANGE* p) { FmtAssert(FALSE, ("NYI")); }
extern "C" void EH_Set_Start_Label(EH_RANGE* p) { FmtAssert(FALSE, ("NYI")); }
extern "C" void EH_Set_Has_Call(EH_RANGE* p) { FmtAssert(FALSE, ("NYI")); }
BOOL FREQ_freqs_computed = FALSE;
OP_MAP _CG_LOOP_info_map = NULL;
BB_MAP _cg_dep_op_info = NULL;
INT32 EBO_Opt_Level_Default = 5;
INT32 EBO_Opt_Level = 0;
BOOL CG_LOOP_unroll_analysis = FALSE;
UINT32 CG_LOOP_unrolled_size_max;
UINT32 CG_LOOP_unroll_times_max;
BOOL CG_LOOP_ooo_unroll_heuristics = FALSE;
RID* Non_Transparent_RID (RID *rid) { return rid; }
INT BB_REGION_Exit( BB *bb, RID *rid ) { return 0; }
LABEL_IDX REGION_Exit_Whirl_Labels( WN *exit_whirl, BB *exit_bb, LABEL_IDX external_label, RID *rid ) { FmtAssert(FALSE, ("NYI")); return LABEL_IDX_ZERO; }
CGRIN *CGRIN_Create( INT num_exits ) { return NULL; }
void FREQ_Print_BB_Note(BB *bb, FILE *file) { }
ST * CGSPILL_Get_TN_Spill_Location (TN *tn, CGSPILL_CLIENT client) { return NULL; }
BOOL GRA_LIVE_TN_Live_Outof_BB (TN *tn, BB *bb) { FmtAssert(FALSE, ("NYI")); return FALSE; }
BOOL GRA_LIVE_TN_Live_Into_BB (TN *tn, BB *bb) { FmtAssert(FALSE, ("NYI")); return FALSE; }
void GRA_LIVE_Print_Liveness( BB *bp) { return; }
void LRA_Estimate_Fat_Points (BB* bb, mINT8* fatpoint, INT* regs_in_use, MEM_POOL* pool) { FmtAssert(FALSE, ("NYI")); }
void REG_LIVE_Prolog_Temps(BB *bb, OP *first, OP *last, REGISTER_SET *temps) { FmtAssert(FALSE, ("NYI")); }
CGRIN* RID_Find_Cginfo( BB *bb) { FmtAssert(FALSE, ("NYI")); return NULL; }
void HB_CFLOW_Remove_Block( BB *bp) { }
void HB_CFLOW_Replace_Block(BB *b, BB *pred) {}
void Setup_HB_bb_map(void) {};
BOOL HB_CFLOW_Can_Merge_BBs(BB *b, BB *pred) {return FALSE;}
BB_SET *FREQ_Find_Never_BBs(MEM_POOL *pool) { return NULL; }
void GRA_LIVE_Add_Live_Use_GTN(BB*, TN*) { }
void GRA_LIVE_Merge_Blocks(BB*, BB*, BB*) {}
void  GRA_LIVE_Compute_Liveness_For_BB(BB*) {}
void GRA_LIVE_Compute_Local_Info(BB*) {}
void GRA_LIVE_Region_Start(void) {}
void GRA_LIVE_Region_Entry(BB*) {}
void GRA_LIVE_Region_Exit(BB*) {}
void GRA_LIVE_Region_Compute_Global_Live_Info(void) {}
void EH_Prune_Range_List(void) {}
void EH_Dump_INITOs (WN *, FILE *) {}
void Rename_TNs_For_BB (BB *, TN_SET *, OP *rename_local_TN_op = NULL) {}
void CG_Set_Is_Stack_Used() { Fail_FmtAssertion("NYI"); }

CG_SCHED_EST* CG_SCHED_EST_Create( BB *bb, MEM_POOL *pool, SCHED_EST_TYPE type)
{
  CG_SCHED_EST *se = (CG_SCHED_EST*) MEM_POOL_Alloc(pool, sizeof(CG_SCHED_EST));
  se->contents = NULL;
  se->res_count = NULL;
  se->latency_to_map = NULL;
  se->order = NULL;
  se->use_dep_graph = FALSE;
  se->latency_to_map_dirty = FALSE;
  se->sched_cycles = BB_length(bb);
  return se;
}
UINT32 CG_SCHED_EST_Cycles(CG_SCHED_EST *se) 
{ 
  return se->sched_cycles; 
}
void CG_SCHED_EST_Ignore_Op(CG_SCHED_EST *se, OP *op) 
{ 
  se->sched_cycles--; 
}
void CG_SCHED_EST_Append_Scheds(CG_SCHED_EST *se, CG_SCHED_EST *other_se)
{ 
  se->sched_cycles += other_se->sched_cycles; 
}
void CG_SCHED_EST_Delete(CG_SCHED_EST *se) {}

#include "pqs_cg_stubs.h"
