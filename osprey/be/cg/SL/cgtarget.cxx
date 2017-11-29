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

/* ====================================================================
 * ====================================================================
 *
 * Module: cgtarget.cxx
 * $Revision: 1.4 $
 * $Date: 2006/06/13 02:11:08 $
 * $Author: weitang $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/cg/MIPS/cgtarget.cxx,v $
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
#include "void_list.h"
#include "cg_dep_graph.h"
#include "cg_spill.h"
#include "cg_vector.h"
#include "whirl2ops.h"
#include "ti_errors.h"
#include "ti_latency.h"
#include "w2op.h"
#include "cgexp.h"
#include "cg_loop_recur.h"
#include "targ_proc_properties.h"
#include "ti_bundle.h"
#include "hb_sched.h"
#include "hb_hazards.h"
#include "bb.h"
#include "op.h"
#include "op_list.h"
#include "cg_grouping.h"
#include "calls.h"
#include "cgtarget.h"
#include "calls.h"

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
  FmtAssert(OP_load(memop) || OP_store(memop), ("not a load or store"));

  TOP topcode = OP_code(memop);

  Is_True(topcode < TOP_count, ("unexpected topcode %d\n", topcode));

  if (topcode < TOP_count)
  {
    switch (topcode)
    {
    case TOP_lb:
    case TOP_lbu:
    case TOP_sb:
      return 1;
      
    case TOP_lh:
    case TOP_lhu:
    case TOP_sh:
      return 2;
	  
#ifdef TARG_SL
    case TOP_ldw16:
    case TOP_stw16:
    case TOP_push16:
    case TOP_pop16:
      return 4;

    case TOP_ldub16_rs:
      return 1;
    case TOP_lduh16_rs:
      return 2;
    case TOP_c3_dmac_a:            
    case TOP_c3_dmacn_a:           
    case TOP_c3_dmula_a:           
    case TOP_c3_dmulan_a:          
    case TOP_c3_ffe:               
    case TOP_c3_fftld:             
    case TOP_c3_ld:                
    case TOP_c3_fftst:             
    case TOP_c3_st:                
    case TOP_c3_mac_a:             
    case TOP_c3_macn_a:            
    case TOP_c3_mac_ar:            
    case TOP_c3_macn_ar:           
    case TOP_c3_mula_a:            
    case TOP_c3_mula_ar:           
    case TOP_c3_saadd_a:           
    case TOP_c3_sasub_a:           
    case TOP_c3_saaddh_a:          
    case TOP_c3_sasubh_a:          
    case TOP_c3_sadda_a:           
    case TOP_c3_samulh_a:          
      return 4;
#endif

#ifdef TARG_SL2
    //ld_v:  vbuf->rf
    case TOP_c2_ld_v_b_u:
    case  TOP_c2_ld_v_b:
    case  TOP_c2_ldi_v_b_u:
    case  TOP_c2_ldi_v_b:
      return 16;
	  
    case  TOP_c2_ld_v_h:
    case  TOP_c2_ldi_v_h:
      return 32;
	  
    case  TOP_c2_ld_v_w:
    case  TOP_c2_ldi_v_w:
      return 64;

    case  TOP_c2_ld_v_sw:
    case  TOP_c2_ld_v_m_b_u:
    case  TOP_c2_ld_v_m_b:
    case  TOP_c2_ld_v_m_h:
    case  TOP_c2_ld_v_m_w:
    case  TOP_c2_ldi_v_m_b_u:
    case  TOP_c2_ldi_v_m_b:
    case  TOP_c2_ldi_v_m_h:
    case  TOP_c2_ldi_v_m_w:
      return 32768;

    //ld_g: sbuf->gpr
    case  TOP_c2_ld_s_h_u_p:
    case  TOP_c2_ld_s_h_u:
    case  TOP_c2_ld_s_h_p:
    case  TOP_c2_ld_s_h:
    case  TOP_c2_ldi_s_h_u:
    case  TOP_c2_ldi_s_h:
      return 2;

    case  TOP_c2_ld_s_w_p:
    case  TOP_c2_ld_s_w:
    case  TOP_c2_ldi_s_w:
      return 4;

    //ld_v2g: vbuf->gpr
    case  TOP_c2_ld_v2g_b_u:
    case  TOP_c2_ld_v2g_b:
    case  TOP_c2_ldi_v2g_b_u:
    case  TOP_c2_ldi_v2g_b:
      return 1;

    case  TOP_c2_ld_v2g_h_u:
    case  TOP_c2_ld_v2g_h:
    case  TOP_c2_ldi_v2g_h_u:
    case  TOP_c2_ldi_v2g_h:
      return 17;

    case  TOP_c2_ld_v2g_w:
    case  TOP_c2_ldi_v2g_w:
      return 49;

    //ld_c_imm: sbuf->cr
    case  TOP_c2_ldi_c:
      return 4;
	  
    //st_v: rf->vbuf		
    case  TOP_c2_st_v_b:
    case  TOP_c2_sti_v_b:
      return 16;
	  
    case  TOP_c2_st_v_h:
    case  TOP_c2_sti_v_h:
      return 32;
	  
    case  TOP_c2_st_v_w:
    case  TOP_c2_sti_v_w:
      return 64;
	  
    case  TOP_c2_st_v_m_b:
    case  TOP_c2_st_v_m_h:
    case  TOP_c2_st_v_m_w:
    case  TOP_c2_sti_v_m_b:
    case  TOP_c2_sti_v_m_h:
    case  TOP_c2_sti_v_m_w:
      return 32768;

    //st_g: gpr->sbuf
    case  TOP_c2_st_s_h:
    case  TOP_c2_st_s_h_p:
    case  TOP_c2_sti_s_h:
      return 2;
	  
    case  TOP_c2_st_s_w:
    case  TOP_c2_st_s_w_p:
    case  TOP_c2_sti_s_w:
      return 4;
	  
    case  TOP_c2_sti_c:
      return 4;


    //st_g2v: gpr->vbuf
    case  TOP_c2_st_g2v_b:
    case  TOP_c2_sti_g2v_b:
      return 1;
    case  TOP_c2_st_g2v_h:
    case  TOP_c2_sti_g2v_h:
      return 17;
    case  TOP_c2_st_g2v_w:
    case  TOP_c2_sti_g2v_w:	  
      return 49;

#endif 
    case TOP_lw:
    case TOP_ll:
    case TOP_lwu:
    case TOP_lwc1:
    case TOP_lwxc1:
    case TOP_sw:
    case TOP_sc:
    case TOP_swc1:
    case TOP_swxc1:
      return 4;

    case TOP_ld:
    case TOP_lld:
    case TOP_ldc1:
    case TOP_ldxc1:
    case TOP_sd:
    case TOP_scd:
    case TOP_sdc1:
    case TOP_sdxc1:
      return 8;
    }
  }
  
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

  if (!Preg_Offset_Is_Int(preg) &&
      !Preg_Offset_Is_Float(preg) &&
      !Preg_Offset_Is_Fcc(preg))
    return FALSE;

  /* Get the target register number and class associated with the
   *    * preg, if there is one that is.
   */
  if (Preg_Offset_Is_Int(preg)) {
    regnum = preg - (Int_Preg_Min_Offset - 1);
    rclass = ISA_REGISTER_CLASS_integer;
  }
  else if (Preg_Offset_Is_Float(preg)) {
    regnum = preg - Float_Preg_Min_Offset;
    rclass = ISA_REGISTER_CLASS_float;
  }
  else if (Preg_Offset_Is_Fcc(preg)) {
    regnum = preg - Fcc_Preg_Min_Offset;
    rclass = ISA_REGISTER_CLASS_fcc;
  }
  else if (preg == 0) {
    regnum = 0;
    rclass = ISA_REGISTER_CLASS_integer;
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
  *mispredict = 0;
  *fixed = 0;
  *brtaken = 0;
  *factor = 0.0;

  if (Is_Target_Sb1() || Is_Target_R10K())
  {
    *mispredict= 7; *fixed= 1; *brtaken= 1; *factor = 1.0;
  }
#if defined(TARG_SL)
  else if (Is_Target_Sl1_pcore()  || Is_Target_Sl1_dsp() || Is_Target_Sl5()) {
    *mispredict= 7; *fixed= 1; *brtaken= 1; *factor = 1.0;
  }
  else if ( Is_Target_Sl2_pcore() || Is_Target_Sl2_mcore()) {
    *mispredict= 3; *fixed= 1; *brtaken= 1; *factor = 1.0;
  }
#endif
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
  return (TI_LATENCY_Result_Available_Cycle(op, 0) -
	  TI_LATENCY_Operand_Access_Cycle(op, 0)) > 2;
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
  INT variant;

  /* Branch operands are usually number 0 and 1. Where something
     different is used, we override below. */

  *tn1 = OP_opnd(br, 0);
  *tn2 = OP_opnd(br, 1);

  switch (OP_code(br))
  {
  case TOP_beq:
    variant = V_BR_I4EQ;
    break;

  case TOP_bne:
    variant = V_BR_I4NE;
    break;

  case TOP_bgez:
    variant = V_BR_I4GE;
    *tn2 = Gen_Literal_TN(0,4);
    break;
    
  case TOP_bltz:
    variant = V_BR_I4LT;
    *tn2 = Gen_Literal_TN(0,4);
    break;

  case TOP_bgtz:
    variant = V_BR_I4GT;
    *tn2 = Gen_Literal_TN(0,4);
    break;
    
  case TOP_blez:
    variant = V_BR_I4LE;
    *tn2 = Gen_Literal_TN(0,4);
    break;

  case TOP_bc1t:
    variant = V_BR_F_TRUE;
    *tn2 = NULL;
    break;

  case TOP_bc1f:
    variant = V_BR_F_FALSE;
    *tn2 = NULL;
    break;

  default:
    variant = V_BR_NONE;
    *tn1 = NULL;
    *tn2 = NULL;

    Is_True( !OP_cond( br ) || (OP_code(br) == TOP_auxbr), ("unexpected conditional branch %d\n", OP_code(br) ) );

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
  TN *cond_tn1;
  TN *cond_tn2;

  /* Classify the condition based on the branch instruction.  */

  INT variant = CGTARG_Analyze_Branch(br, &cond_tn1, &cond_tn2);

  /* Once we have varients for 'bf' and 'bt' (and possibly some
     others), we can attempt to find the comparison. For now, none of
     the branches have associated comparisons. */

  *compare_op = NULL;
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
  return TOP_UNDEFINED;
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
  return FALSE;
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
  CGTARG_Invert_Table[TOP_add_s]   = TOP_sub_s;
  CGTARG_Invert_Table[TOP_add_d]   = TOP_sub_d;
  CGTARG_Invert_Table[TOP_madd_s]   = TOP_nmadd_s;
  CGTARG_Invert_Table[TOP_madd_d]   = TOP_nmadd_d;
  CGTARG_Invert_Table[TOP_add]     = TOP_sub;
  CGTARG_Invert_Table[TOP_addu]    = TOP_subu;
  CGTARG_Invert_Table[TOP_dadd]     = TOP_dsub;
  CGTARG_Invert_Table[TOP_daddu]    = TOP_dsubu;

  CGTARG_Invert_Table[TOP_sub_s]   = TOP_add_s;
  CGTARG_Invert_Table[TOP_sub_d]   = TOP_add_d;
  CGTARG_Invert_Table[TOP_nmadd_s]   = TOP_madd_s;
  CGTARG_Invert_Table[TOP_nmadd_d]   = TOP_madd_d;
  CGTARG_Invert_Table[TOP_sub]     = TOP_add;
  CGTARG_Invert_Table[TOP_subu]    = TOP_addu;
  CGTARG_Invert_Table[TOP_dsub]     = TOP_dadd;
  CGTARG_Invert_Table[TOP_dsubu]    = TOP_daddu;

  CGTARG_Invert_Table[TOP_or]    = TOP_nor;
  CGTARG_Invert_Table[TOP_nor]    = TOP_or;

  CGTARG_Invert_Table[TOP_movf] = TOP_movt;
  CGTARG_Invert_Table[TOP_movt] = TOP_movf;
  CGTARG_Invert_Table[TOP_movz] = TOP_movn;
  CGTARG_Invert_Table[TOP_movn] = TOP_movz;
  CGTARG_Invert_Table[TOP_movf_s] = TOP_movt_s;
  CGTARG_Invert_Table[TOP_movt_s] = TOP_movf_s;
  CGTARG_Invert_Table[TOP_movz_s] = TOP_movn_s;
  CGTARG_Invert_Table[TOP_movn_s] = TOP_movz_s;
  CGTARG_Invert_Table[TOP_movf_d] = TOP_movt_d;
  CGTARG_Invert_Table[TOP_movt_d] = TOP_movf_d;
  CGTARG_Invert_Table[TOP_movz_d] = TOP_movn_d;
  CGTARG_Invert_Table[TOP_movn_d] = TOP_movz_d;

  CGTARG_Invert_Table[TOP_beq] = TOP_bne;
  CGTARG_Invert_Table[TOP_bne] = TOP_beq;

  CGTARG_Invert_Table[TOP_bgez] = TOP_bltz;
  CGTARG_Invert_Table[TOP_bgtz] = TOP_blez;
  CGTARG_Invert_Table[TOP_bltz] = TOP_bgez;
  CGTARG_Invert_Table[TOP_blez] = TOP_bgtz;
  CGTARG_Invert_Table[TOP_bgezal] = TOP_bltzal;
  CGTARG_Invert_Table[TOP_bltzal] = TOP_bgezal;

  CGTARG_Invert_Table[TOP_bc1f] = TOP_bc1t;
  CGTARG_Invert_Table[TOP_bc1t] = TOP_bc1f;

  CGTARG_Invert_Table[TOP_c_f_s] = TOP_c_t_s;
  CGTARG_Invert_Table[TOP_c_f_d] = TOP_c_t_d;
  CGTARG_Invert_Table[TOP_c_t_s] = TOP_c_f_s;
  CGTARG_Invert_Table[TOP_c_t_d] = TOP_c_f_d;
  CGTARG_Invert_Table[TOP_c_un_s] = TOP_c_or_s;
  CGTARG_Invert_Table[TOP_c_un_d] = TOP_c_or_d;
  CGTARG_Invert_Table[TOP_c_or_s] = TOP_c_un_s;
  CGTARG_Invert_Table[TOP_c_or_d] = TOP_c_un_d;
  CGTARG_Invert_Table[TOP_c_eq_s] = TOP_c_neq_s;
  CGTARG_Invert_Table[TOP_c_eq_d] = TOP_c_neq_d;
  CGTARG_Invert_Table[TOP_c_neq_s] = TOP_c_eq_s;
  CGTARG_Invert_Table[TOP_c_neq_d] = TOP_c_eq_d;
  CGTARG_Invert_Table[TOP_c_ueq_s] = TOP_c_olg_s;
  CGTARG_Invert_Table[TOP_c_ueq_d] = TOP_c_olg_d;
  CGTARG_Invert_Table[TOP_c_olg_s] = TOP_c_ueq_s;
  CGTARG_Invert_Table[TOP_c_olg_d] = TOP_c_ueq_d;
  CGTARG_Invert_Table[TOP_c_olt_s] = TOP_c_uge_s;
  CGTARG_Invert_Table[TOP_c_olt_d] = TOP_c_uge_d;
  CGTARG_Invert_Table[TOP_c_uge_s] = TOP_c_olt_s;
  CGTARG_Invert_Table[TOP_c_uge_d] = TOP_c_olt_d;
  CGTARG_Invert_Table[TOP_c_ult_s] = TOP_c_oge_s;
  CGTARG_Invert_Table[TOP_c_ult_d] = TOP_c_oge_d;
  CGTARG_Invert_Table[TOP_c_oge_s] = TOP_c_ult_s;
  CGTARG_Invert_Table[TOP_c_oge_d] = TOP_c_ult_d;
  CGTARG_Invert_Table[TOP_c_ole_s] = TOP_c_ugt_s;
  CGTARG_Invert_Table[TOP_c_ole_d] = TOP_c_ugt_d;
  CGTARG_Invert_Table[TOP_c_ugt_s] = TOP_c_ole_s;
  CGTARG_Invert_Table[TOP_c_ugt_d] = TOP_c_ole_d;
  CGTARG_Invert_Table[TOP_c_ule_s] = TOP_c_ogt_s;
  CGTARG_Invert_Table[TOP_c_ule_d] = TOP_c_ogt_d;
  CGTARG_Invert_Table[TOP_c_ogt_s] = TOP_c_ule_s;
  CGTARG_Invert_Table[TOP_c_ogt_d] = TOP_c_ule_d;
  CGTARG_Invert_Table[TOP_c_sf_s] = TOP_c_st_s;
  CGTARG_Invert_Table[TOP_c_sf_d] = TOP_c_st_d;
  CGTARG_Invert_Table[TOP_c_st_s] = TOP_c_sf_s;
  CGTARG_Invert_Table[TOP_c_st_d] = TOP_c_sf_d;
  CGTARG_Invert_Table[TOP_c_ngle_s] = TOP_c_gle_s;
  CGTARG_Invert_Table[TOP_c_ngle_d] = TOP_c_gle_d;
  CGTARG_Invert_Table[TOP_c_gle_s] = TOP_c_ngle_s;
  CGTARG_Invert_Table[TOP_c_gle_d] = TOP_c_ngle_d;
  CGTARG_Invert_Table[TOP_c_seq_s] = TOP_c_sne_s;
  CGTARG_Invert_Table[TOP_c_seq_d] = TOP_c_sne_d;
  CGTARG_Invert_Table[TOP_c_sne_s] = TOP_c_seq_s;
  CGTARG_Invert_Table[TOP_c_sne_d] = TOP_c_seq_d;
  CGTARG_Invert_Table[TOP_c_ngl_s] = TOP_c_gl_s;
  CGTARG_Invert_Table[TOP_c_ngl_d] = TOP_c_gl_d;
  CGTARG_Invert_Table[TOP_c_gl_s] = TOP_c_ngl_s;
  CGTARG_Invert_Table[TOP_c_gl_d] = TOP_c_ngl_d;
  CGTARG_Invert_Table[TOP_c_nlt_s] = TOP_c_lt_s;
  CGTARG_Invert_Table[TOP_c_nlt_d] = TOP_c_lt_d;
  CGTARG_Invert_Table[TOP_c_lt_s] = TOP_c_nlt_s;
  CGTARG_Invert_Table[TOP_c_lt_d] = TOP_c_nlt_d;
  CGTARG_Invert_Table[TOP_c_nge_s] = TOP_c_ge_s;
  CGTARG_Invert_Table[TOP_c_nge_d] = TOP_c_ge_d;
  CGTARG_Invert_Table[TOP_c_ge_s] = TOP_c_nge_s;
  CGTARG_Invert_Table[TOP_c_ge_d] = TOP_c_nge_d;
  CGTARG_Invert_Table[TOP_c_le_s] = TOP_c_nle_s;
  CGTARG_Invert_Table[TOP_c_le_d] = TOP_c_nle_d;
  CGTARG_Invert_Table[TOP_c_nle_s] = TOP_c_le_s;
  CGTARG_Invert_Table[TOP_c_nle_d] = TOP_c_le_d;
  CGTARG_Invert_Table[TOP_c_ngt_s] = TOP_c_gt_s;
  CGTARG_Invert_Table[TOP_c_ngt_d] = TOP_c_gt_d;
  CGTARG_Invert_Table[TOP_c_gt_s] = TOP_c_ngt_s;
  CGTARG_Invert_Table[TOP_c_gt_d] = TOP_c_ngt_d;

  CGTARG_Invert_Table[TOP_teq] = TOP_tne;
  CGTARG_Invert_Table[TOP_tne] = TOP_teq;
  CGTARG_Invert_Table[TOP_tge] = TOP_tlt;
  CGTARG_Invert_Table[TOP_tlt] = TOP_tge;
  CGTARG_Invert_Table[TOP_tgeu] = TOP_tltu;
  CGTARG_Invert_Table[TOP_tltu] = TOP_tgeu;

  CGTARG_Invert_Table[TOP_teqi] = TOP_tnei;
  CGTARG_Invert_Table[TOP_tnei] = TOP_teqi;
  CGTARG_Invert_Table[TOP_tgei] = TOP_tlti;
  CGTARG_Invert_Table[TOP_tlti] = TOP_tgei;
  CGTARG_Invert_Table[TOP_tgeiu] = TOP_tltiu;
  CGTARG_Invert_Table[TOP_tltiu] = TOP_tgeiu;

  /* Init table for CGTARG_Immed_To_Reg:
   */
  CGTARG_Immed_To_Reg_Table[TOP_addi]  = TOP_add;
  CGTARG_Immed_To_Reg_Table[TOP_daddi]  = TOP_dadd;
  CGTARG_Immed_To_Reg_Table[TOP_addiu]  = TOP_addu;
  CGTARG_Immed_To_Reg_Table[TOP_daddiu]  = TOP_daddu;
  CGTARG_Immed_To_Reg_Table[TOP_slti]  = TOP_slt;
  CGTARG_Immed_To_Reg_Table[TOP_sltiu]  = TOP_sltu;
  CGTARG_Immed_To_Reg_Table[TOP_andi]  = TOP_and;
  CGTARG_Immed_To_Reg_Table[TOP_ori]  = TOP_or;
  CGTARG_Immed_To_Reg_Table[TOP_xori]  = TOP_xor;
  CGTARG_Immed_To_Reg_Table[TOP_teqi]  = TOP_teq;
  CGTARG_Immed_To_Reg_Table[TOP_tgei]  = TOP_tge;
  CGTARG_Immed_To_Reg_Table[TOP_tgeiu]  = TOP_tgeu;
  CGTARG_Immed_To_Reg_Table[TOP_tlti]  = TOP_tlt;
  CGTARG_Immed_To_Reg_Table[TOP_tltiu]  = TOP_tltu;
  CGTARG_Immed_To_Reg_Table[TOP_tnei]  = TOP_tne;

  /* Init table for CGTARG_Inter_RegClass_Copy_Table:
   */
  CGTARG_Inter_RegClass_Copy_Table[ISA_REGISTER_CLASS_float]
				  [ISA_REGISTER_CLASS_integer]
				  [FALSE] = TOP_mfc1;
  CGTARG_Inter_RegClass_Copy_Table[ISA_REGISTER_CLASS_float]
				  [ISA_REGISTER_CLASS_integer]
				  [TRUE]  = TOP_dmfc1;

  CGTARG_Inter_RegClass_Copy_Table[ISA_REGISTER_CLASS_integer]
				  [ISA_REGISTER_CLASS_float]
				  [FALSE] = TOP_mtc1;
  CGTARG_Inter_RegClass_Copy_Table[ISA_REGISTER_CLASS_integer]
				  [ISA_REGISTER_CLASS_float]
				  [TRUE]  = TOP_dmtc1;
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
  TOP opr = OP_code(op);
  switch (opr)
  {
  case TOP_addi:
  case TOP_addiu:
  case TOP_ori:
  case TOP_xori:
  case TOP_sll:
  case TOP_srl:
  case TOP_sra:
    {
      TN *src1 = OP_opnd(op, 1);
      if (TN_is_constant(src1) && TN_has_value(src1)) {
        INT64 val = TN_value(src1);
        if (val == 0) return 0;
      }
      break;
    }
  case TOP_andi:
    {
      TN *src1 = OP_opnd( op, 1 );
      if (TN_is_constant(src1) && TN_has_value(src1)) {
        INT64 val = TN_value(src1);
        if (val == -1) return 0;
      }
      break;
    }
  case TOP_or:
  case TOP_xor:
  case TOP_add:
  case TOP_addu:
    {
      if (OP_opnd( op, 1) == Zero_TN)
        return 0;
      else if (OP_opnd( op, 0) == Zero_TN)
        return 1;
      break;
    }
  case TOP_sub:
  case TOP_subu:
  case TOP_sllv:
  case TOP_srlv:
  case TOP_srav:
    {
      if (OP_opnd( op, 1) == Zero_TN)
        return 0;
      break;
    }
  }

  if (OP_copy(op)) {
    if (opr == TOP_add   ||  opr == TOP_addu ||
        opr == TOP_sub   ||  opr == TOP_subu ||
        opr == TOP_or    ||  opr == TOP_xor)
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
BOOL
CGTARG_Can_Fit_Immediate_In_Add_Instruction (INT64 immed)
{
  return ISA_LC_Value_In_Class (immed, LC_simm16);
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
  return ISA_LC_Value_In_Class (immed, LC_simm16);
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
  /* TODO for MIPS */
#if defined(TARG_SL2)
  switch(OP_code(pred_op))
  {
    case TOP_c2_thctrl_lock:
    case TOP_c2_thctrl_unlock:
    case TOP_c2_thctrl_deact:
    case TOP_c2_thctrl_act:
    case TOP_c2_thctrl_mode4:
    case TOP_c2_thctrl_mode5:
    case TOP_c2_thctrl_mode6:
    case TOP_c2_thctrl_mode7:
    case TOP_c2_fork_m:
    case TOP_c2_fork_n:
    case TOP_peripheral_rw_begin:        
    case TOP_peripheral_rw_end:        
    return TRUE;
  }
  switch(OP_code(succ_op))
  {
    case TOP_c2_thctrl_lock:
    case TOP_c2_thctrl_unlock:
    case TOP_c2_thctrl_deact:
    case TOP_c2_thctrl_act:
    case TOP_c2_thctrl_mode4:
    case TOP_c2_thctrl_mode5:
    case TOP_c2_thctrl_mode6:
    case TOP_c2_thctrl_mode7:
    case TOP_c2_fork_m:
    case TOP_c2_fork_n:
    case TOP_c2_joint:
    case TOP_loop:
    case TOP_peripheral_rw_begin:
    case TOP_peripheral_rw_end:        
    return TRUE;
  }
#endif   
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
#if !defined(TARG_SL2)
// this function is used to check if bypass can be used between pred and succ
// if bypass can be used the latency is 1, producer-to-consumer latency is 3
// Following cases which can use bypass
//    1. gpr to gpr
//    2. instruction which use vmult, vadd 
// following cases which can not use bypass
//    1. vmov
//    2. vspel 
//    3. vspmacs
//    4. other simd register and control register 
//     
  TN* operand = NULL; 
  if(OP_opnds(succ_op) > opnd)
      operand = OP_opnd(succ_op, opnd);  
   
  if(OP_has_bypass(pred_op))
    *latency = 0;
  else if(operand && TN_is_register(operand) && TN_register_class(operand) == ISA_REGISTER_CLASS_cop_vreg)
  {
    *latency = 2;
  } 
#endif
}

/* ===================================================================
 * CGTARG_Mem_AR_Dep
 *   Set OP is ARDEP when there is dependence between C3-MEM and Normal MEM
 * ===================================================================
 */
void
CGTARG_Mem_AR_Dep(OP *pred_op, OP *succ_op, CG_DEP_KIND kind)
{
  //  1) insert nop when C3 mem op points to same address with normal memory op in same BB
  //     now implement version is based on sl0mpw
  switch (kind) {
    case CG_DEP_MEMIN:
    {
       /* case 1:  stw
                   c3.mac.a
          case 2:  c3.st      <- SL1 HW could identify kind 2-5 of memory dep
                   c3.mac.a      sl0mpw didn't identify 1 and 2
          case 3:  c3.st
                   c3.ld
          case 4:  c3.st
                   ldw
          case 5:  stw
                   ldw
       */
       BOOL mac_load = OP_c3_load(succ_op) && (!OP_memtrap(succ_op));

       if (OP_store(pred_op) && mac_load) {
         Set_OP_ARdep(succ_op);
       }
       break;
     }
     case CG_DEP_MEMANTI:
     {
       //  if bb is loop body, MEMANTI will cause same problem as MEMIN
       /* case 1: c3.mac.a
                  stw
          case 2: c3.mac.a   <- SL1 HW could identify kind 2-5 of memory dep
                  c3.st         sl0mpw didn't identify 1 and 2
          case 3: c3.ld
                  c3.st
          case 4: ldw
                  c3.st
          case 5: ldw
                  stw
       */
       if ((BB_zdl_body(OP_bb(pred_op)) || BB_loophead(OP_bb(pred_op)))) {
         BOOL mac_load = OP_c3_load(pred_op) && (!OP_memtrap(pred_op));
         if (mac_load && OP_store(succ_op)) {
           Set_OP_ARdep(pred_op);
         }
        }
        break;
    }
    default:
      break;
  }
  return;
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
                           const WN* asm_wn, TYPE_ID id)
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

  // if 'm' is one of the choices, always prefer that one
  // TODO: we decide this in the front end, but it's not optimal
  if (*constraint != 'm')
  {
    const char* m = constraint;
    while (*++m)
    {
      if (*m == 'm')
      {
        constraint = m;
        break;
      }
    }
  }
  
  // prefer register/memory over immediates; this isn't optimal, 
  // but we may not always be able to generate an immediate
  static const char* immediates = "in";
  while (strchr(immediates, *constraint) && *(constraint+1))
  {
    constraint++;
  }

  TN* ret_tn;
  
  // TODO: check that the operand satisifies immediate range constraint
  if (strchr(immediates, *constraint))
  {
    if (load && WN_operator(load)==OPR_LDID && WN_class(load)==CLASS_PREG)
    {
      // immediate could have been put in preg by wopt
      load = Preg_Is_Rematerializable(WN_load_offset(load), NULL);
    }
    FmtAssert(load && WN_operator(load) == OPR_INTCONST, 
              ("Cannot find immediate operand for ASM"));
    ret_tn = Gen_Literal_TN(WN_const_val(load), 
                            MTYPE_bit_size(WN_rtype(load))/8);
  }
  // digit constraint means that we should reuse a previous operand
  else if (isdigit(*constraint))
  {
    INT prev_index = *constraint - '0';
    FmtAssert(asm_constraint_tn[prev_index], 
              ("numeric matching constraint refers to NULL value"));
    ret_tn = asm_constraint_tn[prev_index];
  }
  else if (strchr("m", *constraint))
  {
    TYPE_ID rtype = (load != NULL ? WN_rtype(load) : MTYPE_I4);
    FmtAssert(MTYPE_is_integral(rtype),
              ("ASM operand does not satisfy its constraint"));
    ret_tn = (pref_tn ? pref_tn : Build_TN_Of_Mtype(rtype));
  }
  else if ((*constraint == 'r') || (*constraint == 'a') ||
	   (*constraint == 'b') || (*constraint == 'v') ||
	   (*constraint == 'h') || (*constraint == 'l') ||
	   (*constraint == 'd'))
  {
    TYPE_ID rtype;
    if (load != NULL) {
      rtype = WN_rtype(load);
    } else {
      /* We can't figure out what type the operand is, probably because the
	 optimizer deleted the references to it, so return some default
	 type based on the constraint. */
      rtype = ((*constraint == 'b') ? MTYPE_B : MTYPE_I4);
    }
    ret_tn = (pref_tn ? pref_tn : Build_TN_Of_Mtype(rtype));
  }
  else if (*constraint == 't')
  {
    FmtAssert(pref_tn && TN_is_dedicated(pref_tn) &&
	      TN_register_class(pref_tn)==ISA_REGISTER_CLASS_UNDEFINED,
		("ASM constraint 't' requires a state register"));
    ret_tn = pref_tn;
  }
  else if (*constraint == 'f')
  {
    if (load && (WN_desc(load) != MTYPE_F4 || WN_rtype(load) != MTYPE_F4)) {
      FmtAssert(FALSE, ("ASM operand does not satisfy its constraint"));
    }
    ret_tn = (pref_tn ? pref_tn : Build_TN_Of_Mtype(MTYPE_F4));
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
	OP_imul(op) || 
	(OP_code(op) == TOP_mflo) || 
	(OP_code(op) == TOP_mfhi))
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
	OP_imul(op) ||
	(OP_code(op) == TOP_mflo) || 
	(OP_code(op) == TOP_mfhi))
      return FALSE;
    else
      return TRUE;
  case EAGER_DIVIDE:
    if (OP_load(op) ||
	OP_store(op) ||
	OP_prefetch(op) ||
	OP_idiv(op) ||
	OP_imul(op) ||
	(OP_code(op) == TOP_mflo) || 
	(OP_code(op) == TOP_mfhi))
      return FALSE;
    else
      return TRUE;
  case EAGER_MEMORY:
  case EAGER_OTHER:
    if (OP_idiv(op) ||
	OP_imul(op) ||
	(OP_code(op) == TOP_mflo) || 
	(OP_code(op) == TOP_mfhi))      
      return FALSE;
    else
      return TRUE;
  default:
    FmtAssert(FALSE, ("Handle this case"));
    return FALSE;
  }
}

