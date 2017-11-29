/*
 * Copyright (C) 2008-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
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


/* ====================================================================
 * ====================================================================
 *
 * Module: cg.cxx
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/cg.cxx,v $
 *
 * Description:
 *
 * This file contains the main driver and initialization,termination
 * routines for the code generator.
 *
 * ====================================================================
 * ====================================================================
 */

#include "defs.h"
#include "wn.h"
#include "cg.h"
#include "cg_internal.h"
#include "cg_flags.h"
#include "config.h"
#include "config_list.h"
#include "tracing.h"
#include "timing.h"
#include "strtab.h"
#include "cgir.h"
#include "erglob.h"
#include "ercg.h"
#include "data_layout.h"
#include "whirl2ops.h"
#include "calls.h"
#include "bitset.h"
#include "tn_set.h"
#include "gtn_universe.h"
#include "bb_set.h"
#include "register.h"
#include "gra.h"
#include "freq.h"
#include "fb_whirl.h"
#include "lra.h"
#include "cgemit.h"
#include "cg_loop.h"
#include "glob.h"
#include "cgexp.h"
#include "igls.h"
#include "tn_map.h"
#include "cg_region.h"
#include "wn_util.h"
#include "cg_spill.h"
#include "localize.h"
#include "gra_live.h"
#include "opt_alias_interface.h"
#include "ir_reader.h"
#include "cflow.h"
#include "dwarf_DST_mem.h"
#include "region_util.h"
#include "eh_region.h"
#include "reg_live.h"
#include "findloops.h"
#include "cgdriver.h"
#include "label_util.h"
#include "cgtarget.h"
#include "ebo.h"
#include "hb.h"
#include "pqs_cg.h"
#include "tag.h"
#include <sstream>
#ifdef TARG_IA64
#include "ipfec.h"
#include "ipfec_defs.h"
#include "ipfec_options.h"
#include "region_update.h"
#include "region_verify.h"
#include "vt_region.h"
#include "recovery.h"
#include "edge_profile.h"
#include "val_prof.h"
#include "bb_verifier.h"
#include "config_opt.h"
#include "be_util.h"
#include "stride_prefetch.h"
#include "cache_analysis.h"
#include "multi_branch.h"

#define _value_profile_before_region_formation
#define can_invoke_profile_with_current_cg_opt_level (CG_opt_level>1)
//#define can_invoke_profile_with_current_cg_opt_level (1)
#endif
#ifdef KEY
#include "cg_gcov.h"
#endif
#ifdef TARG_NVISA
#include "dominate.h"
#include "vector_loadstore.h"
#endif
#if defined(TARG_SL)
#include "region.h"
#include "region_update.h"
#include "scheduler.h"
#include "disp_instr.h"
#endif
#ifdef TARG_LOONGSON
#include "ipfec_defs.h"
#include "ipfec_options.h"
#include "lgra_opt_spill.h"
#include "edge_profile.h"
#include "config_opt.h"
#endif
#include "cg_cfg.h"
#include "cgssa_core.h"
#include "gpo.h"

using namespace CGSSA_NAME;

MEM_POOL MEM_local_region_pool; /* allocations local to processing a region */
MEM_POOL MEM_local_region_nz_pool;

BOOL Trace_REGION_Interface = FALSE;
BOOL is_str_expand = FALSE;
#if defined(TARG_IA64) || defined(TARG_LOONGSON)
INT32 current_PU_handle = 0;
INT32 rse_budget;
#endif

BOOL PU_Has_Calls;
BOOL PU_References_GP;
#ifdef TARG_IA64
BOOL GRA_optimize_restore_pr;
BOOL GRA_optimize_restore_b0_ar_pfs;
BOOL GRA_optimize_restore_ar_lc;
BOOL EBO_data_spec;
// Control assemly output on file number
typedef mempool_allocator<INT> INT_ALLOC;
typedef std::vector<INT, INT_ALLOC>  INT_CONTAINER;
#endif
#if defined(TARG_IA64) || defined(TARG_SL) || defined(TARG_MIPS)
BOOL RGN_Formed = FALSE;
#endif

#ifdef KEY
BOOL PU_Has_Exc_Handler;
BOOL PU_Has_Nonlocal_Goto_Target;
BOOL CG_file_scope_asm_seen = FALSE;
#endif

BOOL gra_pre_create = TRUE;
#ifdef TARG_X8664
BOOL PU_has_local_dynamic_tls;
TN*  Local_Dynamic_TLS_Base;
BOOL PU_References_GOT;  // for -m32 -fpic
BOOL PU_has_avx128;      // cause emit of vzeroupper 
BOOL PU_has_builtin_apply_args; // __builtin_apply_args
BOOL PU_has_builtin_apply; // __builtin_apply
#endif

BOOL edge_done = FALSE;
BOOL CG_PU_Has_Feedback;
RID *Current_Rid;

TN_MAP TN_To_PREG_Map;
#ifdef TARG_X8664
BB_MAP BBs_Map = NULL;
#endif

#ifdef TARG_X8664
extern BOOL cg_load_execute_overridden;
#endif

#ifdef TARG_PPC32
extern void Expand_Start();
extern void Expand_Finish();
#endif
/* WOPT alias manager */
struct ALIAS_MANAGER *Alias_Manager;

static BOOL Orig_Enable_SWP;
#ifdef TARG_IA64
extern BOOL gra_self_recursive;
extern BOOL fat_self_recursive;
#endif

#if defined(TARG_SL) || defined(TARG_MIPS)
REGISTER_SET caller_saved_regs_used[ISA_REGISTER_CLASS_MAX+1];
#endif

extern void draw_vcg_flow_graph(const char* fname);

/* Stuff that needs to be done at the start of each PU in cg. */
void
CG_PU_Initialize (WN *wn_pu)
{
  static INT pu_num;

  MEM_POOL_Push ( &MEM_phase_pool );
  MEM_POOL_Push ( &MEM_local_pool );
  MEM_POOL_Push ( &MEM_phase_nz_pool );
  MEM_POOL_Push ( &MEM_local_nz_pool );

  PU_Has_Calls = FALSE;
  PU_References_GP = FALSE;

#if defined(TARG_SL) || defined(TARG_MIPS)
  ISA_REGISTER_CLASS rc;
  FOR_ALL_ISA_REGISTER_CLASS(rc)
    caller_saved_regs_used[rc] = REGISTER_SET_EMPTY_SET;
#endif 

#ifdef TARG_IA64
  GRA_optimize_restore_pr = TRUE;
  GRA_optimize_restore_b0_ar_pfs = TRUE;
  GRA_optimize_restore_ar_lc = TRUE;
  EBO_data_spec=FALSE;
#endif

#ifdef KEY
  PU_Has_Exc_Handler = FALSE;
  PU_Has_Nonlocal_Goto_Target = PU_has_nonlocal_goto_label(Get_Current_PU());
#endif

#ifdef TARG_X8664
  if (! cg_load_execute_overridden) {
    if ((Is_Target_EM64T() || Is_Target_Core() || Is_Target_Wolfdale()) &&
         PU_src_lang(Get_Current_PU()) != PU_C_LANG) {   // bug 10233
      CG_load_execute = 0;
    } else if (! Is_Target_32bit() &&
	(PU_src_lang(Get_Current_PU()) == PU_F77_LANG ||
	 PU_src_lang(Get_Current_PU()) == PU_F90_LANG)) {
      CG_load_execute = 2;
    } else {
      CG_load_execute = 1;
    }
  }

  // for local dynamic tls, all tls objects can share the same base
  PU_has_local_dynamic_tls = FALSE;
  Local_Dynamic_TLS_Base = NULL;

  PU_References_GOT = FALSE;

  PU_has_builtin_apply_args = FALSE;
  PU_has_builtin_apply = FALSE;

  if (CG_localize_x87_tns && Is_Target_SSE2()) {
    fprintf(stderr,
	    "Ignoring CG_localize_x87_tns since it has no effect under SSE2\n");
    CG_localize_x87_tns = FALSE;
  }
  if (CG_x87_store && Is_Target_SSE2()) {
    fprintf(stderr,
	    "Ignoring CG_x87_store since it has no effect under SSE2\n");
    CG_x87_store = FALSE;
  }

  // Select basic block instruction scheduling algorithm.
  if (!LOCS_Scheduling_Algorithm_set) {
    if (Is_Target_32bit()) {                    // 32-bit ABI
      // Forward scheduling.
      LOCS_Scheduling_Algorithm = 1;
    } else {                                    // 64-bit ABI
      // Bidirectional for Fortran, backward for C/C++.
      LOCS_Scheduling_Algorithm = PU_ftn_lang(Get_Current_PU()) ? 2 : 0;
    }
  }

  if (PU_cxx_lang(Get_Current_PU()) && Is_Target_64bit()) {// C++ & m64
    if (!GRA_prioritize_by_density_set)		// bug 14357
      GRA_prioritize_by_density = TRUE;
    if (!GRA_optimize_boundary_set)		// bug 14357
      GRA_optimize_boundary = TRUE;
  }
#endif

  Regcopies_Translated = FALSE;
#if defined(TARG_SL)
  /* HD - Check whether the PU should be skipped for optimization
   *      We need to consider the intersection parts
   */
  bool skip = false;

  if( CG_skip_before > 0 ) {
    if( CG_skip_after < INT32_MAX ) {
      if( pu_num > CG_skip_after && pu_num < CG_skip_before )
        skip = true;
    } else if ( pu_num < CG_skip_before ){
      skip = true;
    }
  } else {
    if( CG_skip_after >= 0 ) 
      if( pu_num > CG_skip_after )
        skip = true;
  }

  if( CG_skip_equal >= 0 ) {
    if( pu_num == CG_skip_equal )
      skip = true;
  }

  CG_Configure_Opt_Level( skip ? 0 : Opt_Level);
#else
  CG_Configure_Opt_Level((   pu_num < CG_skip_before
			  || pu_num > CG_skip_after
			  || pu_num == CG_skip_equal)
			 ? 0 : Opt_Level);
#endif // TARG_SL
  pu_num++;

  if (PU_has_syscall_linkage(Get_Current_PU())) {
    // turn off swp so stacked registers are preserved
    Orig_Enable_SWP = Enable_SWP;
    Enable_SWP = FALSE;
  }

  Reuse_Temp_TNs = (CG_opt_level == 0);
  if (Get_Trace (TP_CGEXP, 1024)) Reuse_Temp_TNs = FALSE;

  CGTARG_Initialize();
  BB_PU_Initialize ();
  Init_TNs_For_PU ();
  LOOP_DESCR_Init_For_PU();
  TN_MAP_Init();
  BB_MAP_Init();
#ifdef TARG_IA64
  REGION_MAP_Init();
#endif
  OP_MAP_Init();
  CFLOW_Initialize();
#if !defined (TARG_NVISA)
  CGSPILL_Initialize_For_PU ();
  CG_LOOP_Init();
  HB_Init();
  if (Enable_CG_Peephole) EBO_Init();
#endif 
  Init_Label_Info();

#if defined(EMULATE_LONGLONG) && !defined(TARG_PPC32)
  extern void Init_TN_Pair();
  Init_TN_Pair ();
#endif

  /* initialize register package for current pu */
  REGISTER_Pu_Begin();

  Init_Entry_Exit_Code (wn_pu);
  REGISTER_Reset_FP();  // in case $fp is used, must be after entry_exit init

#ifndef TARG_NVISA
  /* Initialize global tn universe */
  GTN_UNIVERSE_Pu_Begin();

  Trace_REGION_Interface = Get_Trace( TP_REGION, TT_REGION_CG_DEBUG ) ||
    Get_Trace(TP_REGION, TT_REGION_BOUND_DEBUG );

  Init_gen_quad_preg(); // init statics to hold quad preg STs
#endif

  /* data layout and lowering now happens earlier, in bedriver. */
  /* don't finalize stack frame until just before emit, after all spills. */

  if (Get_Trace (TKIND_ALLOC, TP_CG)) {
    // start tracing at beginning of cg.
    MEM_Tracing_Enable();
  }

}

void
CG_PU_Finalize(void)
{
  TAG_Finish();
  OP_MAP_Finish();
#ifndef TARG_NVISA
  GTN_UNIVERSE_Pu_End ();
  CGSPILL_Finalize_For_PU();
  if (Enable_CG_Peephole) EBO_Finalize();

  if (PU_has_syscall_linkage(Get_Current_PU())) {
    Enable_SWP = Orig_Enable_SWP;
  }
#endif

  /* TN_To_PREG_Map is allocated from MEM_pu_pool and so can't be popped
     but it can be put on the free list and cleared at the end of the PU */
  TN_MAP_Delete(TN_To_PREG_Map);
  TN_To_PREG_Map = NULL;

#ifdef TARG_X8664
  BB_MAP_Delete( BBs_Map );
  BBs_Map = NULL;

  Expand_Finish();
#endif

#if defined(TARG_PPC32)
  Expand_Finish();
#endif

  Free_BB_Memory();		    /* Free non-BB_Alloc space. */
  MEM_POOL_Pop ( &MEM_local_pool );
  MEM_POOL_Pop ( &MEM_local_nz_pool );
  MEM_POOL_Pop ( &MEM_phase_pool );
  MEM_POOL_Pop ( &MEM_phase_nz_pool );
#ifdef TARG_SL
  Expand_Finish();
#endif
}

/* Stuff that needs to be done at the start of each REGION in cg. */
static void
CG_Region_Initialize (WN *rwn, struct ALIAS_MANAGER *alias_mgr)
{
  MEM_POOL_Push (&MEM_local_region_pool);
  MEM_POOL_Push (&MEM_local_region_nz_pool);
  Init_CG_Expand ();
#ifndef TARG_NVISA
  FREQ_Region_Initialize ();
  BB_REGION_Initialize ();
  LRA_Init();
  GRA_Initialize();
#endif
  Init_TNs_For_REGION ();
  /*
   * Create Array to map PREGs into TNs
   * Must be done after Init_Entry_Exit_Code, since
   * Init_Entry_Exit_Code creates special PREGs to represent
   * save locations in WHIRL
   */
  PREG_NUM last_preg_num;
  last_preg_num = Get_Preg_Num (PREG_Table_Size(CURRENT_SYMTAB))+1;
  PREG_To_TN_Array = (TN **) Pu_Alloc (sizeof (TN *) * last_preg_num);
  PREG_To_TN_Mtype = (TYPE_ID *) Pu_Alloc (sizeof (TYPE_ID) * last_preg_num);

  PREG_To_TN_Clear();	/* this enforces different preg maps between regions */
  if (TN_To_PREG_Map == NULL)
    TN_To_PREG_Map = TN_MAP_Create();

#ifdef TARG_X8664
  if( BBs_Map == NULL ){
    BBs_Map = BB_MAP_Create();
  }
#endif

  TN_CORRESPOND_Free(); /* remove correspondence between tns (ex. divrem) */

#ifndef TARG_NVISA
  GTN_UNIVERSE_REGION_Begin();
#endif

  Whirl2ops_Initialize(alias_mgr);

  Current_Rid = REGION_get_rid( rwn );

#if defined(TARG_X8664) || defined(TARG_PPC32)
  Expand_Start();
#endif

#if defined(TARG_SL)
  extern void Initial_var2spe();
  Initial_var2spe();
  Expand_Start();
#endif

}

/*
 * Stuff that needs to be done at the end of each REGION in cg.
 * This includes making glue code to map TNs in CG'd code
 * from/to PREGs in WHIRL
 */
static void
CG_Region_Finalize (WN *result_before, WN *result_after,
		    WN *rwn, struct ALIAS_MANAGER *am, BOOL generate_glue_code)
{
  RID *rid;
  CGRIN *cgrin;
  WN *entry_fixup, *exit_fixup;
  INT32 i, num_exits;

  Is_True(REGION_consistency_check(rwn),("CG_Region_Finalize"));
  rid = REGION_get_rid( rwn );
  cgrin = RID_cginfo( rid );
  FmtAssert(rid != NULL && cgrin != NULL,
	    ("CG_Region_Finalize, inconsistent region"));

  REGION_set_level(rid, RL_CGSCHED);

#ifndef TARG_NVISA
  if (generate_glue_code) {
    /* region entry glue code */
    entry_fixup = CGRIN_entry_glue( cgrin );
    REGION_Entry_PREG_Whirl( rid, entry_fixup, CGRIN_tns_in( cgrin ), am );
    if ( Trace_REGION_Interface ) {
      fprintf( TFile, "<region> Entry glue code for RGN %d\n", RID_id(rid) );
      fdump_tree( TFile, entry_fixup );
    }
    WN_INSERT_BlockFirst( result_before, entry_fixup );

    num_exits = RID_num_exits( rid );
    for (i=0; i<num_exits; i++) {
      exit_fixup = CGRIN_exit_glue_i( cgrin, i );
      REGION_Exit_PREG_Whirl( rid, i, exit_fixup,
			     CGRIN_tns_out_i( cgrin, i ), am );
      if ( Trace_REGION_Interface ) {
	fprintf( TFile, "<region> Exit glue code for exit %d RGN %d\n",
		i, RID_id(rid) );
	fdump_tree( TFile, exit_fixup );
      }
      WN_INSERT_BlockLast( result_after, exit_fixup );
    }
  }
#endif

  Whirl2ops_Finalize();

  MEM_POOL_Pop (&MEM_local_region_pool);
  MEM_POOL_Pop (&MEM_local_region_nz_pool);
}

static int trace_count = 0;
static void Check_for_Dump_ALL(INT32 pass, BB *bb, char *s )
{
  trace_count++;
  char count_buf[20];
  int count = sprintf(count_buf, "%d: ", trace_count);
  char phase_buf[20+30]= "Tracing"; // Tracing%d
  strcat(phase_buf, count_buf);
  strcat(phase_buf, s);
  Set_Error_Phase(phase_buf);
  Check_for_Dump(pass, bb);
}

#if defined(TARG_SL)
// check minor region is used to check if there is a call in minor region 
// if a call exist, give an assertion

void 
Check_Minor_Region() 
{
  for(BB* bb = REGION_First_BB; bb; bb = BB_next(bb))
  {
    if(BB_rid(bb) && RID_TYPE_minor(BB_rid(bb)) && BB_call(bb))
	Fail_FmtAssertion("An function call BB:%d in minor region", BB_id(bb)); 
  }
  return; 
}
#endif


#if defined(TARG_SL)
void 
Collect_Simd_Register_Usage()
{
  if(!Get_Trace(TP_TEMP, 0x1))
    return; 

  vector < mTN_NUM > regs_read;
  vector < mTN_NUM > regs_write; 
  vector < mTN_NUM >::iterator iter; 
  vector < ST* > callee_in_pu;  
  vector < ST* >::iterator st_iter; 
  BB *bb; 
  OP *op; 
  for(bb = REGION_First_BB; bb; bb = BB_next(bb))
  {
// collect function call for this PU

    if(BB_call(bb)) 
    {
      ANNOTATION *callant = ANNOT_Get(BB_annotations(bb), ANNOT_CALLINFO);
      CALLINFO *callinfo = ANNOT_callinfo(callant);
      ST *st = CALLINFO_call_st(callinfo);
      callee_in_pu.push_back(st); 
    }
    FOR_ALL_BB_OPs(bb, op)
    {
      for(INT i = 0; i < OP_results(op); i++) {
        TN* tn = OP_result(op, i); 
        if(TN_is_register(tn) && (TN_register_class(tn) == ISA_REGISTER_CLASS_cop_vreg))
          regs_write.push_back((TN_register(tn) - 1)); 
      }
      for(INT i = 0; i < OP_opnds(op); i++) {
        TN* tn = OP_opnd(op, i); 
        if(TN_is_register(tn) && (TN_register_class(tn) == ISA_REGISTER_CLASS_cop_vreg))
          regs_read.push_back((TN_register(tn) - 1)); 
      }
    }
  }

  fprintf(TFile, "%sFunction %s : \n", DBar, ST_name(Get_Current_PU_ST())); 

  if(!regs_read.empty()) {
    sort(regs_read.begin(), regs_read.end()); 
    regs_read.erase(unique(regs_read.begin(), regs_read.end()), regs_read.end()); 
    fprintf(TFile, "read_simd_reg: "); 
    for(iter = regs_read.begin(); iter != regs_read.end(); iter++)
      fprintf(TFile, "%d  ", *iter); 
    fprintf(TFile, "\n"); 
  }

  if(!regs_write.empty()) {
    sort(regs_write.begin(), regs_write.end()); 
    regs_write.erase(unique(regs_write.begin(), regs_write.end()), regs_write.end()); 
    fprintf(TFile, "write_simd_reg: "); 
    for(iter = regs_write.begin(); iter != regs_write.end(); iter++) {
      fprintf(TFile, "%d  ", *iter); 
   }
    fprintf(TFile, "\n"); 
  }

  fprintf(TFile, "callee:\n"); 
  
  for(st_iter = callee_in_pu.begin(); st_iter != callee_in_pu.end(); st_iter++)
  {
       fprintf(TFile,  "                %s\n", ST_name(*st_iter)); 
  }
  return; 
}
#endif 

#if defined(TARG_IA64) || defined(TARG_LOONGSON)
static void Config_Ipfec_Flags() {
 
  /* copy ORC_... Flags to Ipfec_... Flags */
  Copy_Ipfec_Flags();

  /*start Config_Ipfec_Flags*/
  IPFEC_Enable_Edge_Profile = IPFEC_Enable_Edge_Profile || 
	  ((Instrumentation_Enabled || Instrumentation_Enabled_Before)
           && (Instrumentation_Phase_Num==4 && Instrumentation_Type_Num & CG_EDGE_PROFILE));

  IPFEC_Enable_Value_Profile= IPFEC_Enable_Value_Profile || ( (Instrumentation_Enabled || Instrumentation_Enabled_Before) 
      && (Instrumentation_Phase_Num==4 && Instrumentation_Type_Num & CG_VALUE_PROFILE) ); 
  IPFEC_Enable_Stride_Profile= IPFEC_Enable_Stride_Profile || ( (Instrumentation_Enabled || Instrumentation_Enabled_Before) 
  && (Instrumentation_Phase_Num==4 && Instrumentation_Type_Num & CG_STRIDE_PROFILE) ); 
  IPFEC_Enable_Value_Profile_Annot = IPFEC_Enable_Value_Profile_Annot || Feedback_Enabled[PROFILE_PHASE_BEFORE_REGION];
  IPFEC_Enable_Stride_Profile_Annot = IPFEC_Enable_Stride_Profile_Annot || Feedback_Enabled[PROFILE_PHASE_BEFORE_REGION];
  IPFEC_Enable_Edge_Profile_Annot = IPFEC_Enable_Edge_Profile_Annot || Feedback_Enabled[PROFILE_PHASE_BEFORE_REGION];
  IPFEC_Enable_Opt_after_schedule=IPFEC_Enable_Opt_after_schedule && CG_Enable_Ipfec_Phases && CG_opt_level > 1;
  IPFEC_Enable_Region_Formation = IPFEC_Enable_Region_Formation && CG_Enable_Ipfec_Phases && CG_opt_level > 1;
  IPFEC_Enable_If_Conversion = IPFEC_Enable_If_Conversion && CG_Enable_Ipfec_Phases;
  IPFEC_Force_If_Conv = IPFEC_Force_If_Conv && CG_Enable_Ipfec_Phases;
#ifndef TARG_LOONGSON
  IPFEC_Relaxed_If_Conv = IPFEC_Relaxed_If_Conv && CG_Enable_Ipfec_Phases;
#endif
  IPFEC_Force_Para_Comp_Gen = IPFEC_Force_Para_Comp_Gen && CG_Enable_Ipfec_Phases;
  IPFEC_Para_Comp_Gen = IPFEC_Para_Comp_Gen && CG_Enable_Ipfec_Phases;
  IPFEC_Disable_Merge_BB = IPFEC_Disable_Merge_BB && CG_Enable_Ipfec_Phases;
  IPFEC_Enable_PRDB = IPFEC_Enable_PRDB && CG_Enable_Ipfec_Phases && IPFEC_Enable_Region_Formation && (IPFEC_Enable_Prepass_GLOS || IPFEC_Enable_Postpass_LOCS);
  IPFEC_Enable_BB_Verify = IPFEC_Enable_BB_Verify && CG_Enable_Ipfec_Phases;
  IPFEC_Enable_Prepass_GLOS = IPFEC_Enable_Prepass_GLOS && CG_Enable_Ipfec_Phases;
  IPFEC_Enable_Postpass_GLOS = IPFEC_Enable_Postpass_GLOS && CG_Enable_Ipfec_Phases;
  IPFEC_Enable_Prepass_LOCS = IPFEC_Enable_Prepass_LOCS && CG_Enable_Ipfec_Phases;
  IPFEC_Enable_Postpass_LOCS = IPFEC_Enable_Postpass_LOCS && CG_Enable_Ipfec_Phases;
  IPFEC_Enable_Speculation = IPFEC_Enable_Speculation && CG_Enable_Ipfec_Phases;
  IPFEC_Enable_Data_Speculation = IPFEC_Enable_Data_Speculation && IPFEC_Enable_Speculation;
  IPFEC_Enable_Cntl_Speculation = IPFEC_Enable_Cntl_Speculation && IPFEC_Enable_Speculation;
  IPFEC_Enable_Compressed_Template = IPFEC_Enable_Compressed_Template && CG_Enable_Ipfec_Phases;
  IPFEC_Enable_Pre_Bundling = IPFEC_Enable_Pre_Bundling && CG_Enable_Ipfec_Phases;
  IPFEC_Force_CHK_Fail = IPFEC_Force_CHK_Fail && IPFEC_Enable_Speculation;
  IPFEC_Glos_Enable_Cntl_Spec_If_Converted_Code = IPFEC_Glos_Enable_Cntl_Spec_If_Converted_Code && IPFEC_Enable_Cntl_Speculation;
  IPFEC_Enable_Cascade = IPFEC_Enable_Cascade && IPFEC_Enable_Speculation;
  IPFEC_Hold_Uses = IPFEC_Hold_Uses && IPFEC_Enable_Speculation;
  IPFEC_Chk_Compact = IPFEC_Chk_Compact && IPFEC_Enable_Speculation;
  IPFEC_Enable_Safety_Load = IPFEC_Enable_Safety_Load && IPFEC_Enable_Speculation;
  IPFEC_Profitability = IPFEC_Profitability && CG_Enable_Ipfec_Phases;

  IPFEC_Enable_Multi_Branch = IPFEC_Enable_Multi_Branch && CG_Enable_Ipfec_Phases; 
  IPFEC_Enable_Pre_Multi_Branch = IPFEC_Enable_Pre_Multi_Branch && CG_Enable_Ipfec_Phases;
  IPFEC_Enable_Post_Multi_Branch = IPFEC_Enable_Post_Multi_Branch && CG_Enable_Ipfec_Phases;

  ORC_Enable_Cache_Analysis = ORC_Enable_Cache_Analysis && CG_Enable_Ipfec_Phases; 
#ifdef TARG_LOONGSON
  IPFEC_Enable_Cache_Profile_Annot = IPFEC_Enable_Cache_Profile_Annot || Feedback_Enabled[PROFILE_PHASE_BEFORE_REGION];
#endif

  if (IPFEC_Chk_Compact && locs_skip_bb) {
    DevWarn("Although chk_compact is turned on, it should be turned off since some BBs are forced to be skipped in local scheduling phase!");
    IPFEC_Chk_Compact = 0;
  } 
  if (IPFEC_Chk_Compact && !IPFEC_Enable_Postpass_LOCS) {
    DevWarn("Although chk_compact is turned on, it should be turned off since postpass local scheduling is disabled!");
    IPFEC_Chk_Compact = 0;
  }
}
#endif

/* Can be called two ways:
   1) on a region (pu_dst is NULL, returns code)
   2) on a PU (pu_dst is no NULL, returns NULL)
*/
WN *
CG_Generate_Code( 
    WN *rwn, 
    struct ALIAS_MANAGER *alias_mgr, 
    DST_IDX pu_dst, 
    BOOL region )
{
#ifdef TARG_IA64
  BOOL value_profile_need_gra = FALSE;
  /* Initialize RGN_Formed to FALSE. */
  RGN_Formed = FALSE;
#endif
/*later:  BOOL region = DST_IS_NULL(pu_dst); */
  BOOL orig_reuse_temp_tns = Reuse_Temp_TNs;
  Alias_Manager = alias_mgr;

  Set_Error_Phase( "Code Generation" );
  Start_Timer( T_CodeGen_CU );


#ifdef TARG_X8664
// Cannot enable emit_unwind_info if Force_Frame_Pointer is not set
// Need this flag set for C++ exceptions and for -g
  if (!CG_emit_unwind_info_Set)
  	CG_emit_unwind_info = (Force_Frame_Pointer 
	  || (PU_cxx_lang (Get_Current_PU()) && PU_has_region (Get_Current_PU())));

  // Don't eliminate prologue OPs in main because they guide cgemit.cxx on
  // where to insert OPs to set up the control registers.  Bug 8141.
  {
    static BOOL min_stack_size = CG_min_stack_size;
    CG_min_stack_size = min_stack_size;
    if (!strcmp(Cur_PU_Name, "MAIN__") ||
	!strcmp(Cur_PU_Name, "main"))
      CG_min_stack_size = FALSE;
  }
#endif

  // Use of feedback information can be disabled in CG using the 
  // -CG:enable_feedback=off flag. The flag CG_PU_Has_Feedback is used
  // all over CG instead of Cur_PU_Feedback for this reason.
  CG_PU_Has_Feedback = ((Cur_PU_Feedback != NULL) && CG_enable_feedback);
  BOOL frequency_verify = Get_Trace( TP_FEEDBACK, TP_CG_FEEDBACK );

#ifdef TARG_IA64  
  if (FALSE) {
     ST *func_st = Get_Current_PU_ST();
     rse_budget = PU_gp_group(Pu_Table [ST_pu (func_st)]);
     if (rse_budget == 0) DevWarn("FAINT THE RSE BUDGET IS ZERO!");
  }
#endif
        
  CG_Region_Initialize ( rwn, alias_mgr );

  Set_Error_Phase ( "Code_Expansion" );
  Start_Timer ( T_Expand_CU );

  // If this PU is simply a wrapper for assembly code to be placed
  // into the .s file, take care of that job and move on.
  if (WN_operator(rwn) == OPR_FUNC_ENTRY &&
      ST_asm_function_st(*WN_st(rwn))) {
    FmtAssert(Assembly && !Object_Code,
	      ("Cannot produce non-assembly output with file-scope asm"));
    fprintf(Asm_File, "\n%s\n", ST_name(WN_st(rwn)));
#ifdef KEY
    // Bug 14460: If the program has file-scope asm, it may have directly
    // used the .section attribute to allocate objects. As a result the 
    // compiler does not know the correct origin of objects to be allocated
    // after it. In this scenario, don't emit .org. Also emit a
    // proper .align based on the alignment of the symbol instead of
    // .align 0.
    if (LANG_Enable_Global_Asm || FILE_INFO_has_global_asm(File_info))
      CG_file_scope_asm_seen = TRUE;
#endif
    return rwn;
  }

#if defined (TARG_SL)
  if(CG_stack_layout)
    Pre_Allocate_Objects( rwn );
#endif

  Convert_WHIRL_To_OPs ( rwn );
#if defined(TARG_PPC32)
  extern void Set_Current_PU_WN(WN*);
  Set_Current_PU_WN(rwn);
#endif

#ifndef TARG_NVISA

#if defined(TARG_SL)
   Check_Minor_Region(); 
#endif

#ifdef TARG_X8664
  if (CG_x87_store) {
    extern void Add_Float_Stores();
    Add_Float_Stores();
  }
#endif

#if defined(KEY) && !defined(TARG_SL) && !defined(TARG_LOONGSON)
  extern BOOL profile_arcs;
  if (flag_test_coverage || profile_arcs)
//    CG_Compute_Checksum();
//  if (flag_test_coverage)
    CG_Gcov_Generation();
  if (profile_arcs)
    CG_Instrument_Arcs();
#endif

  // split large bb's to minimize compile speed and register pressure
  Split_BBs();

  if ( ! CG_localize_tns ) {
    // Localize dedicated tns involved in calls that cross bb's,
    // and replace dedicated TNs involved in REGION interface with the
    // corresponding allocated TNs from previously compiled REGIONs.
    Localize_or_Replace_Dedicated_TNs();
  }


  // If using feedback, incorporate into the CFG as early as possible.
  // This phase also fills in any missing feedback using heuristics.
  if (CG_PU_Has_Feedback) {
    Set_Error_Phase ("FREQ");
    Start_Timer (T_Freq_CU);
    FREQ_Incorporate_Feedback ( rwn );
    Stop_Timer (T_Freq_CU);
    Set_Error_Phase ( "Code_Expansion" );
    if (frequency_verify)
      FREQ_Verify("Feedback Incorporation");
  }
  
  // Prior to CG optimization, EH_Prune_Range_List is called to
  // eliminate unneeded EH ranges.  This gets rid of ranges which
  // do not have calls or throws. 

  EH_Prune_Range_List();

#if defined(TARG_IA64)
  // it's high time to compute pu_need_LSDA after EH_Prune_Range_List, 
  pu_need_LSDA = !PU_Need_Not_Create_LSDA ();
#endif
 
  // trace all the EH ranges 
  if (Get_Trace (TP_EH, 0x0002)) {
    fprintf (TFile, "\n=======================================================================\n");
    fprintf (TFile, "\t   EH RANGE INFO for PU: %s \n", ST_name(Get_Current_PU_ST()));
    fprintf (TFile, "\t   (After EH_Prune_Range_List) \t\n");
    fprintf (TFile, "=======================================================================\n");
    EH_Print_Range_List ();
  }

  Optimize_Tail_Calls( Get_Current_PU_ST() );
#endif // !TARG_NVISA

  Init_Callee_Saved_Regs_for_REGION( Get_Current_PU_ST(), region );
#if defined(TARG_IA64) || defined(TARG_LOONGSON)
  //this is a hack for edge profiling
  //when invoke edge profiling, it does not save/restore b0
  //while Generate_Entry_Exit_Code will do this instead, but it need to know
  //IPFEC_Enable_Edge_Profile in time.
  Config_Ipfec_Flags();
#endif
#if defined(TARG_PPC32)
extern void Generate_Return_Address(void);
  Generate_Return_Address();
#endif
#ifdef TARG_LOONGSON
  if ( (IPFEC_Enable_Edge_Profile || IPFEC_Enable_Stride_Profile || IPFEC_Enable_Cache_Profile)
       && (CG_opt_level>1)) 
     PU_Has_Calls = TRUE;
#endif
  Generate_Entry_Exit_Code ( Get_Current_PU_ST(), region );
#ifdef TARG_IA64
  if (!CG_localize_tns) {
    CGTARG_Add_Implict_Operands ();
  }
#endif
  Stop_Timer ( T_Expand_CU );
  Check_for_Dump ( TP_CGEXP, NULL );

#ifndef TARG_NVISA	// nvisa just emits initial assembly

#ifdef TARG_IA64
  if (IPFEC_Enable_Edge_Profile && can_invoke_profile_with_current_cg_opt_level )
  {
    Set_Error_Phase ( "edge profile instrument" );
    Start_Timer ( T_Ipfec_Profiling_CU );
    CG_Edge_Profile_Instrument(RID_cginfo(Current_Rid),PROFILE_PHASE_BEFORE_REGION);
    Stop_Timer( T_Ipfec_Profiling_CU );
    Check_for_Dump(TP_A_PROF, NULL);
    Set_Frame_Has_Calls(TRUE);
  } else if (IPFEC_Enable_Edge_Profile_Annot && can_invoke_profile_with_current_cg_opt_level ) {
    Set_Error_Phase ( "edge profile annotation" );
    CG_Edge_Profile_Annotation(RID_cginfo(Current_Rid),PROFILE_PHASE_BEFORE_REGION);
    Check_for_Dump(TP_A_PROF, NULL);
   
  }

#ifdef _value_profile_before_region_formation
  if ((IPFEC_Enable_Value_Profile||IPFEC_Enable_Stride_Profile) && can_invoke_profile_with_current_cg_opt_level )
  {
    Set_Error_Phase ( "value profile instrument" );
    if (EBO_Opt_Level != 0) 
    {
      DevWarn("Value profiling need -CG:ebo_level=0!! Set ebo_level to 0!!");
      EBO_Opt_Level = 0;
    }
     
    //We take all load instructions for example to show how to specify the 
    //OP's type and how it should be value profiled.
    inst2prof_list.clear();
    inst2prof_list.push_back( CXX_NEW(INST_TO_PROFILE(TOP_ld1,0,FALSE),&MEM_pu_pool) );
    inst2prof_list.push_back( CXX_NEW(INST_TO_PROFILE(TOP_ld2,0,FALSE),&MEM_pu_pool) );
    inst2prof_list.push_back( CXX_NEW(INST_TO_PROFILE(TOP_ld4,0,FALSE),&MEM_pu_pool) );
    inst2prof_list.push_back( CXX_NEW(INST_TO_PROFILE(TOP_ld8,0,FALSE),&MEM_pu_pool) );
    
    UINT32 Min_Instr_Pu_Id, Max_Instr_Pu_Id;
    Min_Instr_Pu_Id = Value_Instr_Pu_Id >> 16;
    Max_Instr_Pu_Id = Value_Instr_Pu_Id & 0xffff;
    Is_True((current_PU_handle<=Max_Instr_Pu_Id)&&(current_PU_handle>=Min_Instr_Pu_Id),("The number of PU exceed the boundery !"));
    Start_Timer ( T_Ipfec_Profiling_CU );
    CG_VALUE_Instrument(RID_cginfo(Current_Rid),PROFILE_PHASE_BEFORE_REGION,IPFEC_Enable_Stride_Profile, IPFEC_Enable_Value_Profile);
    value_profile_need_gra = TRUE;
    Stop_Timer( T_Ipfec_Profiling_CU );
    Check_for_Dump(TP_A_PROF, NULL);
  } else if ((IPFEC_Enable_Value_Profile_Annot||IPFEC_Enable_Stride_Profile_Annot)&& can_invoke_profile_with_current_cg_opt_level ) {
    //We take all load instructions for example to show how to specify the 
    //OP's type and how it should be value profiled.
    inst2prof_list.clear();
    inst2prof_list.push_back( CXX_NEW(INST_TO_PROFILE(TOP_ld1,0,FALSE),&MEM_pu_pool) );
    inst2prof_list.push_back( CXX_NEW(INST_TO_PROFILE(TOP_ld2,0,FALSE),&MEM_pu_pool) );
    inst2prof_list.push_back( CXX_NEW(INST_TO_PROFILE(TOP_ld4,0,FALSE),&MEM_pu_pool) );
    inst2prof_list.push_back( CXX_NEW(INST_TO_PROFILE(TOP_ld8,0,FALSE),&MEM_pu_pool) );

    Set_Error_Phase ( "value profile annotation" );
    UINT32 Min_Instr_Pu_Id, Max_Instr_Pu_Id;
    Min_Instr_Pu_Id = Value_Instr_Pu_Id >> 16;
    Max_Instr_Pu_Id = Value_Instr_Pu_Id & 0xffff;
    Is_True((current_PU_handle<=Max_Instr_Pu_Id)&&(current_PU_handle>=Min_Instr_Pu_Id),("The number of PU exceed the boundery !"));
//    CG_VALUE_Annotate(RID_cginfo(Current_Rid),PROFILE_PHASE_BEFORE_REGION);
  }
#endif
  if (CG_localize_tns && !value_profile_need_gra ) {
#elif TARG_LOONGSON
  if (IPFEC_Enable_Edge_Profile && (CG_opt_level>1)) {
    Set_Error_Phase ( "edge profile instrument" );
    Start_Timer ( T_Ipfec_Profiling_CU );
    CG_Edge_Profile_Instrument ( RID_cginfo(Current_Rid),PROFILE_PHASE_BEFORE_REGION );
    Stop_Timer ( T_Ipfec_Profiling_CU );
    Check_for_Dump ( TP_A_PROF, NULL );
    Set_Frame_Has_Calls ( TRUE );
  } else if (IPFEC_Enable_Edge_Profile_Annot && (CG_opt_level>1)) {
    Set_Error_Phase ( "edge profile annotation" );
    CG_Edge_Profile_Annotation ( RID_cginfo(Current_Rid),PROFILE_PHASE_BEFORE_REGION );
    Check_for_Dump ( TP_A_PROF, NULL );
  }
  if (CG_localize_tns) {
#else // TARG_IA64
  if (CG_localize_tns
#ifdef TARG_X8664
      || CG_localize_x87_tns
#endif
      ) {
#endif // TARG_IA64
    /* turn all global TNs into local TNs */
    Set_Error_Phase ( "Localize" );
    Start_Timer ( T_Localize_CU );
#ifdef KEY  // gra_live is called even if localize is on
    GRA_LIVE_Init(region ? REGION_get_rid( rwn ) : NULL);
#endif
    Localize_Any_Global_TNs(region ? REGION_get_rid( rwn ) : NULL);
    Stop_Timer ( T_Localize_CU );
    Check_for_Dump ( TP_LOCALIZE, NULL );
  } else {
    /* Initialize liveness info for new parts of the REGION */
    /* also compute global liveness for the REGION */
    Set_Error_Phase( "Global Live Range Analysis");
    Start_Timer( T_GLRA_CU );
    GRA_LIVE_Init(region ? REGION_get_rid( rwn ) : NULL);
    Stop_Timer ( T_GLRA_CU );
    Check_for_Dump ( TP_FIND_GLOB, NULL );
  }

  CFLOW_Optimize(CFLOW_UNREACHABLE, "CFLOW (GPO pass)");
  CG_PerformGPO(CG_GPO::GPO_Before_RA);
  GRA_LIVE_Recalc_Liveness(region ? REGION_get_rid( rwn) : NULL);	
  GRA_LIVE_Rename_TNs();

  if (Enable_CG_Peephole) {
    Set_Error_Phase("Extended Block Optimizer");
    Start_Timer(T_EBO_CU);
    EBO_Pre_Process_Region (region ? REGION_get_rid(rwn) : NULL);
    if (CG_opt_level > 1
#ifdef TARG_IA64		    
 	    || value_profile_need_gra 
#endif    
    ) {
      // ebo's optimization may break the live info, we need to
      // update the info before first pass cflow use these wrong info. see bug 313
      GRA_LIVE_Recalc_Liveness(region ? REGION_get_rid( rwn) : NULL);
      // bug fix for OSP_326
      //
      GRA_LIVE_Rename_TNs();
    }
    Stop_Timer ( T_EBO_CU );
    Check_for_Dump ( TP_EBO, NULL );
  }

  // Optimize control flow (first pass)
  if (CG_opt_level > 0 && CFLOW_opt_before_cgprep) {
    // Perform all the optimizations that make things more simple.
    // Reordering doesn't have that property.
    CFLOW_Optimize(  (CFLOW_ALL_OPTS|CFLOW_IN_CGPREP)
#if defined(TARG_SL)
                   & ~(CFLOW_COLD_REGION)
#endif
		   & ~(CFLOW_FREQ_ORDER | CFLOW_REORDER),
		   "CFLOW (first pass)");
    if (frequency_verify && CG_PU_Has_Feedback)
      FREQ_Verify("CFLOW (first pass)");
#if defined(TARG_SL)
    Check_for_Dump_ALL ( TP_CGEXP, NULL,"CFLOW 1" );
#endif
  }

#ifdef TARG_IA64
  extern void Perform_Loop_Invariant_Code_Motion (void);
  if (IPFEC_Enable_LICM && CG_opt_level > 1) {
    Perform_Loop_Invariant_Code_Motion ();
  }

  if (CG_Enable_Ipfec_Phases && CG_opt_level > 1 &&
      (IPFEC_Enable_If_Conversion || IPFEC_Enable_PRDB ||
       IPFEC_Enable_Prepass_GLOS  || IPFEC_Enable_Postpass_GLOS))
    IPFEC_Enable_Region_Formation = TRUE;

  REGION_TREE *region_tree = NULL;
  if (IPFEC_Enable_Region_Formation) {
    // Build Ipfec region tree.
    Set_Error_Phase("Ipfec region formation");
    Start_Timer(T_Ipfec_Region_CU);
    region_tree=CXX_NEW(REGION_TREE(REGION_First_BB),&MEM_pu_pool);
    Stop_Timer(T_Ipfec_Region_CU);
    RGN_Formed = TRUE;
  }
#endif
 
  // Invoke global optimizations before register allocation at -O2 and above.
  if (CG_opt_level > 1) {

    // Compute frequencies using heuristics when not using feedback.
    // It is important to do this after the code has been given a
    // cleanup by cflow so that it more closely resembles what it will
    // to the later phases of cg.
#ifdef TARG_IA64
    if (!CG_PU_Has_Feedback && !IPFEC_Enable_Edge_Profile_Annot) {
#else
      if (!CG_PU_Has_Feedback) {
#endif
      Set_Error_Phase("FREQ");
      Start_Timer (T_Freq_CU);
      FREQ_Compute_BB_Frequencies();
      Stop_Timer (T_Freq_CU);
      if (frequency_verify)
         FREQ_Verify("Heuristic Frequency Computation");
    }

#ifdef TARG_IA64
    if (IPFEC_Enable_Region_Formation) {
      // Build Ipfec region tree.
      Set_Error_Phase("Ipfec region formation");
      Start_Timer(T_Ipfec_Region_CU);
      REGION *root = region_tree->Root();
      IPFEC_Enable_Region_Decomposition = TRUE;
      if (IPFEC_Enable_Region_Decomposition) {
        region_tree->Decomposition(); 
        GRA_LIVE_Recalc_Liveness(region ? REGION_get_rid(rwn) : NULL);
      }

      Stop_Timer(T_Ipfec_Region_CU);

#ifdef Is_True_On
      if (Get_Trace(TP_IPFEC,TT_IPFEC_GRAPHIC)) {
        printf("After Region Formation draw global cfg\n"); 
        draw_global_cfg("after Decompose_Region_To_SEME");
        printf("After Region Formation draw region tree\n"); 
        draw_region_tree(region_tree->Root(),"After Region Formation");
      }
      Verify_Region_Tree(region_tree, REGION_First_BB);
#endif
    }

    // Perform stride prefetch 
    if (IPFEC_Enable_Stride_Prefetch && IPFEC_Enable_Stride_Profile_Annot){
        Set_Error_Phase( "Stride prefetch \n");
        Stride_Region(region_tree, IPFEC_Enable_Stride_Prefetch);
    }
#endif

    // Perform hyperblock formation (if-conversion).  Only works for
    // IA-64 at the moment. 
    //
#ifdef KEY
    // At Key, we form Hyperblocks although MIPS is not predicated architecture
    if (1) {
#else     
    if (CGTARG_Can_Predicate()) {
#endif	    
#ifdef TARG_IA64
      if (IPFEC_Enable_If_Conversion) {
        Set_Error_Phase( "Ipfec if conversion"); 
        IF_CONVERTOR convertor(region_tree);
#ifdef Is_True_On
        if (IPFEC_Enable_BB_Verify) {
          BB_Verify_Flags();
        }
        if (Get_Trace(TP_IPFEC,TT_IPFEC_GRAPHIC)) {
          printf("After If Conversion draw global cfg\n"); 
          draw_global_cfg("after if conversion");
          printf("After If Conversion draw tree\n");
          draw_region_tree(region_tree->Root(),"After If Conversion");
        }
        Verify_Region_Tree(region_tree, REGION_First_BB);
#endif 
#endif  // TARG_IA64

#ifdef TARG_IA64
      }
      else if (!IPFEC_Enable_Region_Formation) {
        // Initialize the predicate query system in the hyperblock
        // formation phase.
        HB_Form_Hyperblocks(region ? REGION_get_rid(rwn) : NULL, NULL);
        if (!PQSCG_pqs_valid()) {
          PQSCG_reinit(REGION_First_BB);
        }
#else
      // Initialize the predicate query system in the hyperblock formation phase
      HB_Form_Hyperblocks(region ? REGION_get_rid(rwn) : NULL, NULL);
#if defined(KEY) && !defined(TARG_LOONGSON)
      // We do not have a slot in the BB structure to store predicate TNs.
      // Instead, we remember the last seen block and the associated 
      // predicate TNs. So, we need to reinitialize the TNs and the basic block
      // once we finish the current hyper-block.
      HB_Reinit_Pred();	
      // CG_LOOP does not use the same mechanism for hammocks.
      hammock_region = FALSE;
#endif
      if (!PQSCG_pqs_valid()) {
	PQSCG_reinit(REGION_First_BB);
#endif // TARG_IA64
      }
      if (frequency_verify)
        FREQ_Verify("Hyberblock Formation");
    }
#ifdef TARG_IA64
    if (!CG_localize_tns || value_profile_need_gra ) {
      /* Initialize liveness info for new parts of the REGION */
      /* also compute global liveness for the REGION */
      Set_Error_Phase( "Global Live Range Analysis");
      Start_Timer( T_GLRA_CU );
      GRA_LIVE_Init(region ? REGION_get_rid( rwn ) : NULL);
      Stop_Timer ( T_GLRA_CU );
      Check_for_Dump ( TP_FIND_GLOB, NULL );
    }
    // Add analysis for Cache information
    if (ORC_Enable_Cache_Analysis) Cache_Location_Analysis(); 
#endif

    if (CG_enable_loop_optimizations) {
#ifdef KEY 
      /* bug#1443
	 Earlier phase, like cflow, does not maintain GTN info if -CG:localize is on,
	 we have to call GRA_LIVE_Init again to rebuild the consistency.
       */
      if (CG_localize_tns
#ifdef TARG_X8664
	  || CG_localize_x87_tns
#endif
         ){
	Set_Error_Phase( "Global Live Range Analysis" );
	GRA_LIVE_Init(region ? REGION_get_rid( rwn ) : NULL);
      }
#endif
      Set_Error_Phase("CGLOOP");
      Start_Timer(T_Loop_CU);
#ifdef TARG_IA64
      if (IPFEC_Enable_Region_Formation) {
        REGION_LOOP_UPDATE    *rgn_loop_update;
        rgn_loop_update = CXX_NEW(REGION_LOOP_UPDATE(region_tree,REGION_First_BB),&MEM_pu_pool);
        Perform_Loop_Optimizations(rgn_loop_update);
        CXX_DELETE(rgn_loop_update, &MEM_pu_pool);
#ifdef Is_True_On
        if (Get_Trace(TP_IPFEC,TT_IPFEC_GRAPHIC)) {
          draw_global_cfg("after loop opt");
          draw_region_tree(region_tree->Root());
        }
        Verify_Region_Tree(region_tree, REGION_First_BB);
#endif
      } else {
        Perform_Loop_Optimizations();
      }
#else
      // Optimize loops (mostly innermost)
      Perform_Loop_Optimizations();
#endif // TARG_IA64
      // detect GTN
      GRA_LIVE_Recalc_Liveness(region ? REGION_get_rid( rwn) : NULL);
      GRA_LIVE_Rename_TNs();  // rename TNs -- required by LRA
      Stop_Timer(T_Loop_CU);
      Check_for_Dump(TP_CGLOOP, NULL);
      if (frequency_verify)
	FREQ_Verify("CGLOOP");

#ifdef KEY
      /* bug#1442
	 Loop optimization will introduce new GTNs. If -CG:localize is on,
	 we should localize all the new created GTNs.
       */
      if (CG_localize_tns
#ifdef TARG_X8664
	  || CG_localize_x87_tns
#endif
         ){
	Set_Error_Phase ( "Localize (after CGLOOP)" );
	Start_Timer ( T_Localize_CU );
	Localize_Any_Global_TNs(region ? REGION_get_rid( rwn ) : NULL);
	Stop_Timer ( T_Localize_CU );
	Check_for_Dump ( TP_LOCALIZE, NULL );
      }
#endif
    }

    /* Optimize control flow (second pass) */
    if (CFLOW_opt_after_cgprep) {
#if defined (TARG_SL)
    CFLOW_Optimize(  (CFLOW_ALL_OPTS|CFLOW_IN_CGPREP)
                   & ~(CFLOW_COLD_REGION)
                   & ~(CFLOW_FREQ_ORDER),
		   "CFLOW (second pass)");
#else
      CFLOW_Optimize(CFLOW_ALL_OPTS, "CFLOW (second pass)");
#endif
#if defined(TARG_SL)
      Check_for_Dump_ALL ( TP_CGEXP, NULL,"CFLOW 1" );
#endif
      if (frequency_verify)
        FREQ_Verify("CFLOW (second pass)");
#ifdef TARG_IA64
#ifdef Is_True_On
        if (Get_Trace(TP_IPFEC,TT_IPFEC_GRAPHIC)) {
          draw_global_cfg("after cflow opt");
          draw_region_tree(region_tree->Root());
        }
        if (IPFEC_Enable_Region_Formation)
          Verify_Region_Tree(region_tree, REGION_First_BB);
#endif
#endif
    }

    if (Enable_CG_Peephole) {
      Set_Error_Phase( "Extended Block Optimizer");
      Start_Timer( T_EBO_CU );
      EBO_Process_Region (region ? REGION_get_rid(rwn) : NULL);
      PQSCG_reinit(REGION_First_BB);
      Stop_Timer ( T_EBO_CU );
      Check_for_Dump ( TP_EBO, NULL );
    }
  }


#ifdef TARG_IA64
  BOOL locs_bundle_value = LOCS_Enable_Bundle_Formation;
  BOOL emit_bundle_value = EMIT_explicit_bundles;
  LOCS_Enable_Bundle_Formation = IPFEC_Enable_Pre_Bundling;
  EMIT_explicit_bundles = IPFEC_Enable_Pre_Bundling;
#endif
  if (!Get_Trace (TP_CGEXP, 1024))
    Reuse_Temp_TNs = TRUE;  /* for spills */

  if (CGSPILL_Enable_Force_Rematerialization)
    CGSPILL_Force_Rematerialization();

  if (!region) {
    /* in case cgprep introduced a gp reference */
    Adjust_GP_Setup_Code( Get_Current_PU_ST(), FALSE /* allocate registers */ );
    /* in case cgprep introduced a lc reference */
    Adjust_LC_Setup_Code();

    // TODO:  when generate control speculation (ld.s) and st8.spill
    // of NaT bits, then need to save and restore ar.unat. 
  }

  /* Global register allocation, Scheduling:
   *
   * The overall algorithm is as follows:
   *   - Global code motion before register allocation
   *   - Local scheduling before register allocation
   *   - Global register allocation
   *   - Local register allocation
   *   - Global code motion phase (GCM) 
   *   - Local scheduling after register allocation
   */

#ifdef TARG_IA64
  if (!CG_localize_tns) {
    /* Initialize liveness info for new parts of the REGION */
    /* also compute global liveness for the REGION */
    /* Set_Error_Phase( "Global Live Range Analysis");
    Start_Timer( T_GLRA_CU );
    GRA_LIVE_Init(region ? REGION_get_rid( rwn ) : NULL);
    Stop_Timer ( T_GLRA_CU );
    Check_for_Dump ( TP_FIND_GLOB, NULL ); */
  }

  //Temporary solution for performance tuning.
  gra_self_recursive = FALSE;
  fat_self_recursive = FALSE;
  //Check_Self_Recursive();
  if (CG_opt_level > 1 && IPFEC_Enable_PRDB) PRDB_Init(region_tree);

  if (IPFEC_Enable_Prepass_GLOS && CG_opt_level > 1) {
    Start_Timer( T_GLRA_CU );
    GRA_LIVE_Init(region ? REGION_get_rid( rwn ) : NULL);
    Stop_Timer ( T_GLRA_CU );

    if(Enable_CG_Peephole)
      EBO_Pre_Process_Region (region ? REGION_get_rid(rwn) : NULL);
    Check_Self_Recursive();
    Global_Insn_Sched(region_tree, TRUE);
  } else if (IPFEC_Enable_Prepass_LOCS) {
    Local_Insn_Sched(TRUE);
  } else {
    IGLS_Schedule_Region (TRUE /* before register allocation */);
  }
  
  if (CG_opt_level > 1 && IPFEC_Enable_PRDB) PRDB_Init(region_tree);
  // bug fix for OSP_104, OSP_105, OSP_192
  /* actually GIS and recovery phase are two parts of one phase,
   * we shouldn't add a cflow between them, because they should 
   * see the same cfg, 3rd cflow should after recovery phase.
   */
  if (IPFEC_Enable_Prepass_GLOS && CG_opt_level > 1) {
     BOOL need_recalc_liveness = (Generate_Recovery_Code() > 0);
     if (need_recalc_liveness) 
        GRA_LIVE_Init(region ? REGION_get_rid( rwn ) : NULL);
  }

  if (IPFEC_Enable_Opt_after_schedule) {
    BOOL tmp = CG_localize_tns ;
    CG_localize_tns = TRUE;
    CFLOW_Optimize(CFLOW_BRANCH|CFLOW_UNREACHABLE|CFLOW_MERGE|CFLOW_REORDER /* |CFLOW_FREQ_ORDER */,
		    "CFLOW (third pass)");
    CG_localize_tns = tmp ;
  }
  
  if (CG_opt_level > 1 && IPFEC_Enable_Post_Multi_Branch) {
    GRA_LIVE_Init(region ? REGION_get_rid( rwn ) : NULL);
    Post_Multi_Branch_Collect();
  }
#ifdef Is_True_On
  if (IPFEC_Enable_BB_Verify) {
    BB_Verify_Flags();
  }
#endif

  LOCS_Enable_Bundle_Formation = locs_bundle_value;
  EMIT_explicit_bundles = emit_bundle_value;
#ifndef _value_profile_before_region_formation
   if (IPFEC_Enable_Value_Profile && can_invoke_profile_with_current_cg_opt_level )
  {
    Set_Error_Phase ( "value profile instrument" );
        if (EBO_Opt_Level != 0)
        {
                DevWarn("Value profiling need -CG:ebo_level=0!! Set ebo_level to 0!!");
                EBO_Opt_Level = 0;
        }
    //We take all load instructions for example to show how to specify the
    //OP's type and how it should be value profiled.
    inst2prof_list.clear();
    inst2prof_list.push_back( CXX_NEW(INST_TO_PROFILE(TOP_ld1,0,FALSE),&MEM_pu_pool) );
    inst2prof_list.push_back( CXX_NEW(INST_TO_PROFILE(TOP_ld2,0,FALSE),&MEM_pu_pool) );
    inst2prof_list.push_back( CXX_NEW(INST_TO_PROFILE(TOP_ld4,0,FALSE),&MEM_pu_pool) );
    inst2prof_list.push_back( CXX_NEW(INST_TO_PROFILE(TOP_ld8,0,FALSE),&MEM_pu_pool) );
        UINT32 Min_Instr_Pu_Id, Max_Instr_Pu_Id;
        UINT64 tmpmask=1;
        tmpmask = tmpmask << current_PU_handle;
        Min_Instr_Pu_Id = Value_Instr_Pu_Id >> 16;
        Max_Instr_Pu_Id = Value_Instr_Pu_Id & 0xffff;
        if (current_PU_handle >= Min_Instr_Pu_Id
                && current_PU_handle <= Max_Instr_Pu_Id
                && ((unsigned long long)( tmpmask &~ Value_Instr_Pu_Id_Mask ))                )
        {
        Start_Timer ( T_Ipfec_Profiling_CU );
            CG_VALUE_Instrument(RID_cginfo(Current_Rid),PROFILE_PHASE_LAST,FALSE,FALSE);
            value_profile_need_gra = TRUE;
        Stop_Timer( T_Ipfec_Profiling_CU );
        }
        Check_for_Dump(TP_A_PROF, NULL);

   } else if (IPFEC_Enable_Value_Profile_Annot && can_invoke_profile_with_current_cg_opt_level ) {
    //We take all load instructions for example to show how to specify the
    //OP's type and how it should be value profiled.
    inst2prof_list.clear();
    inst2prof_list.push_back( CXX_NEW(INST_TO_PROFILE(TOP_ld1,0,FALSE),&MEM_pu_pool) );
    inst2prof_list.push_back( CXX_NEW(INST_TO_PROFILE(TOP_ld2,0,FALSE),&MEM_pu_pool) );
    inst2prof_list.push_back( CXX_NEW(INST_TO_PROFILE(TOP_ld4,0,FALSE),&MEM_pu_pool) );
    inst2prof_list.push_back( CXX_NEW(INST_TO_PROFILE(TOP_ld8,0,FALSE),&MEM_pu_pool) );

    Set_Error_Phase ( "value profile annotation" );
        UINT32 Min_Instr_Pu_Id, Max_Instr_Pu_Id;
        UINT64 tmpmask=1;
        tmpmask = tmpmask << current_PU_handle;
        Min_Instr_Pu_Id = Value_Instr_Pu_Id >> 16;
        Max_Instr_Pu_Id = Value_Instr_Pu_Id & 0xffff;
        if (current_PU_handle >= Min_Instr_Pu_Id
                && current_PU_handle <= Max_Instr_Pu_Id
                && ((unsigned long long)( tmpmask &~ Value_Instr_Pu_Id_Mask ))                )
        {
//            CG_VALUE_Annotate(RID_cginfo(Current_Rid),PROFILE_PHASE_LAST,FALSE,FALSE);
        }
  }
  DevWarn("Now we are testing instrumentation after RegionFormation!");
#endif 
  current_PU_handle++;
#endif // TARG_IA64
  
#ifdef KEY 
  // Earlier phases (esp. CFLOW) might have introduced local definitions and
  // uses for global TNs. Rename them to local TNs so that LRA can accurately
  // compute register requests (called from scheduling).
  //
  // (Also, earlier phase, like cflow, does not maintain GTN info if
  // -CG:localize is on.  Rebuild the consistency for GCM.  Bug 7219.)

#ifdef TARG_SL //fork_joint
  //only opt level greater than 1, we recaluculate liveness informatino
  //otherwise it will reset GTN flag in glue code when compiling base on
  // region. This will cause assertion. 
  if(CG_opt_level > 1) {
    GRA_LIVE_Recalc_Liveness(region ? REGION_get_rid( rwn) : NULL);	
    GRA_LIVE_Rename_TNs();
  }

  BOOL should_do_gcm;
  should_do_gcm = GCM_Enable_Scheduling & RGN_Enable_All_Scheduling;

  if(should_do_gcm && CG_opt_level > 1) {
    IGLS_Schedule_Region( TRUE );
  } 
  Check_for_Dump_ALL ( TP_CGEXP, NULL, "GCM" );

  if( RGN_Enable_All_Scheduling && 
      CG_Enable_Regional_Local_Sched && 
      LOCS_PRE_Enable_Scheduling &&
      CG_opt_level > 1 ){
    Local_Insn_Sched( TRUE );
  }
  Check_for_Dump_ALL ( TP_CGEXP, NULL, "Pre LIS" );

#else
  if (!CG_localize_tns) {
    GRA_LIVE_Recalc_Liveness(region ? REGION_get_rid( rwn) : NULL);
    GRA_LIVE_Rename_TNs();
  }
#if !defined(TARG_PPC32)    //  PPC IGLS_Schedule_Region bugs
  IGLS_Schedule_Region (TRUE /* before register allocation */);
#ifdef TARG_X8664
  Examine_Loop_Info("after prescheduling", TRUE);
  void Counter_Merge (char*);
  if (CG_merge_counters_x86 == TRUE && CG_opt_level > 1) {
    if (Enable_CG_Peephole) {
      Set_Error_Phase ( "SIB counter merging" );
      GRA_LIVE_Recalc_Liveness(region ? REGION_get_rid( rwn) : NULL);
      GRA_LIVE_Rename_TNs ();
      Counter_Merge (Cur_PU_Name);
    }
  }
#endif

#endif 

#endif // TARG_SL
#endif

#ifdef TARG_LOONGSON
  current_PU_handle++;
#endif

#ifdef TARG_IA64
  if (!CG_localize_tns || value_profile_need_gra )
#else
    if (!CG_localize_tns)
#endif
  {
    // Earlier phases (esp. GCM) might have introduced local definitions
    // and uses for global TNs. Rename them to local TNs so that GRA 
    // does not have to deal with them.

    if (GRA_recalc_liveness) {
      Start_Timer( T_GLRA_CU);
      GRA_LIVE_Recalc_Liveness(region ? REGION_get_rid( rwn) : NULL);
      Stop_Timer ( T_GLRA_CU );
      Check_for_Dump (TP_FIND_GLOB, NULL);
#ifdef TARG_IA64
    } else if (!(IPFEC_Enable_Prepass_GLOS && (CG_opt_level > 1 || value_profile_need_gra))) {
      GRA_LIVE_Recalc_Liveness(region ? REGION_get_rid( rwn) : NULL);
#else
    } else {
#endif
      GRA_LIVE_Rename_TNs ();
    }
#ifdef TARG_IA64
    if (GRA_redo_liveness || IPFEC_Enable_Prepass_GLOS && (CG_opt_level > 1 || value_profile_need_gra)) {
#else
      if (GRA_redo_liveness
#ifdef KEY      // Inaccurate liveness info will break GRA's boundary BB code.
                // But don't always redo liveness, bug 4781.
        || GRA_optimize_boundary
#endif
	  )
	{
#endif
      Start_Timer( T_GLRA_CU );
      GRA_LIVE_Init(region ? REGION_get_rid( rwn ) : NULL);
      Stop_Timer ( T_GLRA_CU );
      Check_for_Dump ( TP_FIND_GLOB, NULL );
#if defined(TARG_SL)
      Check_for_Dump_ALL ( TP_CGEXP, NULL, "GLRA" );
#endif
    }

    GRA_Allocate_Global_Registers( region );
  }

  LRA_Allocate_Registers (!region);

#ifdef TARG_X8664
  GRU_Fuse_Global_Spills (!region);
#endif

#if defined(TARG_SL)
  if (Run_ipisr)
    IPISR_Insert_Spills();
#endif

#ifdef TARG_LOONGSON
  Initialize_Optimized_LRA_And_EBO();
#endif

#ifdef TARG_IA64
  if (!CG_localize_tns || value_profile_need_gra) {
#else
    if (!CG_localize_tns ) {
#endif
    Set_Error_Phase ( "GRA_Finish" );
    /* Done with all grant information */
    GRA_Finalize_Grants();
  }

#if defined(KEY) && !defined(TARG_SL)
  /* Optimize control flow (third pass).  Callapse empty GOTO BBs which GRA
     didn't find useful in placing spill code.  Bug 9063. */
  if (CFLOW_opt_after_cgprep &&
      !CG_localize_tns) {
    CFLOW_Optimize(CFLOW_BRANCH|CFLOW_UNREACHABLE, "CFLOW (third pass)");
  }
#endif

  if (!region) {
    /* Check that we didn't introduce a new gp reference */
    Adjust_GP_Setup_Code( Get_Current_PU_ST(), TRUE /* allocate registers */ );

    /* The stack frame is final at this point, no more spilling after this.
     * We can set the Frame_Len now.
     * Then we can go through all the entry/exit blocks and fix the SP 
     * adjustment OP or delete it if the frame length is zero.
     */
    Set_Frame_Len (Finalize_Stack_Frame());
    Set_Error_Phase ( "Final SP adjustment" );
    Adjust_Entry_Exit_Code ( Get_Current_PU_ST() );
  }

#if defined(TARG_SL)
  Check_for_Dump_ALL ( TP_CGEXP, NULL, "Adj Ent/exit" );
#endif
#ifdef TARG_IA64
  if (CG_opt_level > 0 && Enable_EBO_Post_Proc_Rgn) {
#else
  if (Enable_CG_Peephole) {
#endif
    Set_Error_Phase("Extended Block Optimizer");
    Start_Timer(T_EBO_CU);
    EBO_Post_Process_Region (region ? REGION_get_rid(rwn) : NULL);
    Stop_Timer ( T_EBO_CU );
    Check_for_Dump ( TP_EBO, NULL );
  }

#ifdef TARG_IA64
  if (IPFEC_Enable_Postpass_LOCS) {
    if (IPFEC_sched_care_machine!=Sched_care_bundle) {
      Local_Insn_Sched(FALSE);
      CGGRP_Bundle();
    }
    else if (CG_opt_level <1){
      IPFEC_sched_care_machine = Sched_care_nothing;
      CGGRP_Bundle();
    }
    else{
      Local_Insn_Sched(FALSE);
    }
  } else {
    if (PRDB_Valid()) Delete_PRDB();
    IGLS_Schedule_Region (FALSE /* after register allocation */);
  }
#ifdef Is_True_On
  if (IPFEC_Enable_BB_Verify) {
    BB_Verify_Flags();
  }
#endif

  if(PRDB_Valid()) Delete_PRDB();
  if (IPFEC_Force_CHK_Fail)
    Force_Chk_Fail();

  if (CG_opt_level > 1 && IPFEC_Enable_Post_Multi_Branch) {
    Post_Multi_Branch();
  }
  Reuse_Temp_TNs = orig_reuse_temp_tns;     /* restore */
  
  if (PRDB_Valid()) Delete_PRDB();
  if (IPFEC_Enable_Region_Formation) {
    /* Empty BB eliminating has no relation with region, 
     * so it can turn off IPFEC_Enable_Region_Formation temporarily,
     * Or it will cause many errors which deal with region
     */
     IPFEC_Enable_Region_Formation = FALSE;
     CFLOW_Delete_Empty_BB();  
     IPFEC_Enable_Region_Formation = TRUE;
  }
#else  // TARG_IA64

#if defined (TARG_SL)
  if (!region) {
    CFLOW_Optimize(CFLOW_FREQ_ORDER, "CFLOW (forth pass)");
    CFLOW_Optimize(CFLOW_BRANCH | CFLOW_UNREACHABLE | CFLOW_COLD_REGION, "CFLOW (fifth pass)");  
  }
  
  /*16-bit instr replaced*/
  if (!region && CG_Gen_16bit && (CG_opt_level > 1)) {
    Replace_Size16_Instr();
    Check_for_Dump_ALL ( TP_CGEXP, NULL, "gen16bit op" );
  }
  
  if (RGN_Enable_All_Scheduling) {
    if( CG_Enable_Regional_Local_Sched && 
	LOCS_POST_Enable_Scheduling &&
	CG_opt_level > 1) {
      Local_Insn_Sched(FALSE);
      Check_for_Dump_ALL ( TP_CGEXP, NULL, "Post LIS" );
    }
  }

  if (CG_Enable_Macro_Instr_Combine && CG_opt_level > 1)
    Move_Macro_Insn_Together(); 

  if (!region && CG_Gen_16bit)
    Guarantee_Paired_instr16();
  if (CG_check_quadword) {
    Check_Br16();	
  }

#else 
#if !defined(TARG_PPC32)
#ifdef TARG_LOONGSON
  // Mainly concerns about ld from GRA
  if (!CG_localize_tns){
     if (IPFEC_Enable_RA_OPT!=0){
         Optimize_St_In_Entry_BB();
     }
     if (IPFEC_Enable_Opt_Ld_After_LRA) {
        REG_LIVE_Analyze_Region();
        Optimize_Ld_In_Dom();
        REG_LIVE_Finish();
     }
     if (IPFEC_Enable_Opt_Mem_OP){
           Optimize_For_Needless_OP ();
     }
  }
  Finalize_Optimized_LRA_And_EBO();
#endif
  IGLS_Schedule_Region (FALSE /* after register allocation */);
  // use cflow to handle branch fusing cmp/jcc for Orochi and greater.
  if (Is_Target_Orochi() && CG_branch_fuse ) {
    CFLOW_Optimize(CFLOW_BR_FUSE, "CFLOW (fifth pass)");
  }
#endif
#endif

#if defined(TARG_MIPS) && !defined(TARG_SL)
  // Rerun EBO to delete useless spills and restores.  A spill and restore can
  // be deleted if the intervening instruction that writes to the register is
  // moved away.
  if (Enable_CG_Peephole) {
    Set_Error_Phase("Extended Block Optimizer (after second insn scheduling)");
    Start_Timer(T_EBO_CU);
    EBO_Post_Process_Region_2 (region ? REGION_get_rid(rwn) : NULL);
    Stop_Timer ( T_EBO_CU );
    Check_for_Dump ( TP_EBO, NULL );
  }
#endif

#ifdef TARG_X8664
  {
    /* Perform compute-to opts. */
    if ((Is_Target_Barcelona() || Is_Target_Orochi()) && CG_compute_to) {
      for( BB* bb = REGION_First_BB; bb != NULL; bb = BB_next(bb) ){
        EBO_Compute_To(bb);
      }
    }

    // Generate merge dependency clear if avx128 is being used.
    if (Is_Target_Orochi() && Is_Target_AVX() && PU_has_avx128) {
      Generate_Entry_Merge_Clear(region);
    }

    /* Convert all the x87 regs to stack-like regs. */
    extern void Convert_x87_Regs( MEM_POOL* );
    Convert_x87_Regs( &MEM_local_region_pool );

    /* When a function returns a structure under -m32, the value of SP will be
       increased by 4 bytes.
       For a function which has stdcall or fastcall attrubute, the SP also
       will be adjusted by the same amount which popped at the function
       return time. 
    */
    if( Is_Target_32bit() ){
      for( BB* bb = REGION_First_BB; bb != NULL; bb = BB_next(bb) ){
	if( BB_call(bb) )
	  Adjust_SP_After_Call( bb );
      }
    }
  }
#endif

#if defined(KEY) && (defined(TARG_MIPS) && !defined(TARG_SL))
  CFLOW_Fixup_Long_Branches();
#endif

  Reuse_Temp_TNs = orig_reuse_temp_tns;		/* restore */

#endif // TARG_IA64

#else // TARG_NVISA

  // Optimize control flow (first pass)
  if (CG_opt_level > 0 && CFLOW_opt_before_cgprep) {
    // Perform all the optimizations that make things more simple.
    // Reordering doesn't have that property.
    CFLOW_Optimize(  (CFLOW_ALL_OPTS|CFLOW_IN_CGPREP)
		   & ~(CFLOW_FREQ_ORDER | CFLOW_REORDER),
		   "CFLOW (first pass)");
  }

  if (CG_opt_level > 0) {
    // dominators needed for unique_defs and detect_loops passes.
    // as long as we don't change bb flow, we don't need to recompute this.
    Calculate_Dominators();

    // following optimizations work better when unique-def (one_def),
    // so try to create unique defs when possible.
    Set_Error_Phase("Create Unique Defs");
    Create_Unique_Defs_For_TNs();
    Check_for_Dump ( TP_EBO, NULL );

    // Do vector optimization before 16bit optimization
    // because 16bit opt may hinder vectors
    // if only some fields are converted to 16bit.
    // 16bit opt may ignore vectors, but coalescing 
    // is more important than 16bit opt if we have to choose.
    if (CG_vector_loadstore) {
      // do peephole-like vector load/store creation
      Set_Error_Phase("Extended Block Optimizer - vectors");
      Create_Vector_Load_Stores();
      Check_for_Dump ( TP_EBO, NULL );
    }

    // by changing ops to 16bit before register allocation,
    // we avoid wasting registers.
    if (CG_use_16bit_ops) {
      Set_Error_Phase("Use 16bit Ops");
      Use_16bit_Ops();
      Check_for_Dump ( TP_EBO, NULL );
    }
  }

    // iterate thru code, assigning registers.
    // also sets one_def property that following optimizations use
    Set_Error_Phase ( "Register Allocation" );
    Assign_Virtual_Registers();
    Check_for_Dump (TP_ALLOC, NULL);

  if (CG_opt_level > 0) {
    // various nvisa-specific optimizations:
    Start_Timer(T_EBO_CU);

    // setup data structures for finding reaching defs
    REG_LIVE_Analyze_Region();
    (void) LOOP_DESCR_Detect_Loops (&MEM_local_pool);

    if (CG_optimize_copies) {
      // see if we can optimize away copies by avoiding later src uses
      Set_Error_Phase("Optimize Copies");
      Optimize_Copy_Usage();
      Check_for_Dump ( TP_EBO, NULL );
    }

    if (CG_rematerialize_grf) {
      // iterate thru code, see if can rematerialize GRF (shared memory)
      // loads since they don't cost real memory and may save registers
      // by reducing spill ranges.
      Set_Error_Phase("Extended Block Optimizer - rematerialize");
      Rematerialize_GRF();
      Check_for_Dump ( TP_EBO, NULL );
    }
    REG_LIVE_Finish();
    Free_Dominators_Memory();

    Stop_Timer ( T_EBO_CU );
  }
#endif // TARG_NVISA
  
  if (region) {
#ifdef TARG_NVISA
    FmtAssert(FALSE, ("regions not supported"));
    return rwn;
#else
    /*--------------------------------------------------------------------*/
    /* old region: rwn, rid_orig                      */
    /* new region: rwn_new, rid_new (encloses old region)         */
    /*--------------------------------------------------------------------*/
    WN  *inner_body, *outer_body, *exitBlock, *comment;
    WN  *rwn_new, *result_block_before, *result_block_after;
    RID *rid_orig;
    char str[100];

    Is_True(REGION_consistency_check(rwn),("CG_Generate_Code"));
    rid_orig = REGION_get_rid(rwn);

    /* don't delete rwn, it contains the stub that helps find the MOPS
       that the region has been lowered to */

    outer_body = WN_CreateBlock();
    /* put inner region inside outer containment block */
    WN_INSERT_BlockFirst(outer_body, rwn);
    /* we assembled the new exit block earlier in Build_CFG()       */
    exitBlock = CGRIN_nested_exit(RID_cginfo(rid_orig));
    WN_region_exits(rwn) = exitBlock; /* PPP ??? */

    rwn_new = outer_body;

    /* put a note in the inner body that the code isn't there anymore */
    inner_body = WN_CreateBlock();
    WN_region_body(rwn) = inner_body; /* overwrite old body, now in MOPs */
    sprintf(str,"RGN %d has been lowered to MOPs, level=%s",
	    RID_id(rid_orig), RID_level_str(rid_orig));
    comment = WN_CreateComment(str);
    WN_INSERT_BlockFirst(inner_body, comment);

    /* Need to split result block for glue code into two parts: before and
       after the region body. The reason we can't just insert the glue code
       directly before or after the region directly is that we need to keep
       it separate for updating the alias info.
       If CG_LOOP has made some WHIRL glue, it is inserted in result_block. */
    result_block_before = WN_CreateBlock();
    result_block_after = WN_CreateBlock();

    /* fill-in blocks with glue code */
    Set_Error_Phase("Region Finalize");
    Start_Timer(T_Region_Finalize_CU);
    CG_Region_Finalize( result_block_before, result_block_after,
                       rwn, alias_mgr, TRUE /* generate_glue_code */ );
    Stop_Timer(T_Region_Finalize_CU);

    /* generate alias information for glue code */
    REGION_update_alias_info(result_block_before, alias_mgr);
    REGION_update_alias_info(result_block_after, alias_mgr);

    /* insert glue code before and after */
    WN_INSERT_BlockFirst( rwn_new, result_block_before );
    WN_INSERT_BlockLast( rwn_new, result_block_after );

    GRA_LIVE_Finish_REGION();
    PQSCG_term();

    Stop_Timer ( T_CodeGen_CU );
    Set_Error_Phase ( "Codegen Driver" );

#ifdef TARG_IA64
    if(IPFEC_Enable_Region_Formation) CXX_DELETE(region_tree, &MEM_pu_pool);
#endif

    return rwn_new;
#endif // TARG_NVISA
  } /* if (region */

#if defined(TARG_IA64)
  else { /* PU */
    // dump EH entry info
    if (Get_Trace (TP_EH, 0x0001)) {
      Print_PU_EH_Entry(Get_Current_PU(), WN_st(rwn), TFile);
    }
    
    /* Write the EH range table. 
     * if pu_need_LSDA is set for current PU, 
     * means no need to write EH range table
     */
    if (PU_has_exc_scopes(Get_Current_PU()) && 
        pu_need_LSDA ) {
      EH_Write_Range_Table(rwn);
    }
    // dump LSDA of current PU
    if (Get_Trace (TP_EH, 0x0008)) {
      EH_Dump_LSDA (TFile);
    }
#else
  else { /* PU */
#if !defined(TARG_NVISA)
    if (PU_has_exc_scopes(Get_Current_PU())) {
      EH_Write_Range_Table(rwn);
    }
#endif
#endif //TARG_IA64 This is not a good merge compared to the code in trunk

#if defined(TARG_SL)
     Collect_Simd_Register_Usage();
     // SL1 Hardware walkaround
     if (Is_Target_Sl1_pcore() || Is_Target_Sl1_dsp()) {
       SL1_patch();
     }
#endif 

#ifdef TARG_X8664
    if (Is_Target_Orochi() == TRUE)
    {
      extern void CG_Sched( MEM_POOL*, BOOL );
      Set_Error_Phase("Dispatch Scheduling");
      Start_Timer(T_Dispatch_Sched_CU);
      CG_Sched( &MEM_local_pool, Get_Trace( TP_DSCHED, 1 ) );
      Stop_Timer(T_Dispatch_Sched_CU);
    }
#endif

    /* Emit the code for the PU. This may involve writing out the code to
     * an object file or to an assembly file or both. Additional tasks
     * performed by this module are:
     *
     *   - convert long branches into a chain of short branches.
     *   - add nada's to quad-align branch targets for TFP.
     */
    Set_Error_Phase ( "Assembly" );
    Start_Timer ( T_Emit_CU );
#ifdef TARG_IA64
    if (Create_Cycle_Output)
        Cycle_Count_Initialize(Get_Current_PU_ST(), region);      
    /* Generate code to call mcount. See description of gcc -pg option.
     * This instrumentation is done just before code emission.
     */
    Instru_Call_Mcount();
#endif
    EMT_Emit_PU (Get_Current_PU_ST(), pu_dst, rwn);
    Check_for_Dump (TP_EMIT, NULL);
    Stop_Timer ( T_Emit_CU );

#ifndef TARG_NVISA
#ifdef TARG_IA64
    if (ORC_Enable_Cache_Analysis) Cache_Analysis_End();
#endif
    Set_Error_Phase("Region Finalize");
    Start_Timer(T_Region_Finalize_CU);
    CG_Region_Finalize( NULL, NULL, rwn, alias_mgr,
                        FALSE /* generate_glue_code */ );
    Stop_Timer(T_Region_Finalize_CU);

    GRA_LIVE_Finish_PU();
    PQSCG_term();
#endif // TARG_NVISA

    /* List local symbols if desired: */
    if ( List_Symbols )
	Print_symtab (Lst_File, CURRENT_SYMTAB);

    Stop_Timer ( T_CodeGen_CU );
    Set_Error_Phase ( "Codegen Driver" );
#ifdef TARG_IA64
    if(IPFEC_Enable_Region_Formation) CXX_DELETE(region_tree, &MEM_pu_pool);
#endif
    return rwn;
  }
}


/* ================================================================= */
/* routines for dumping/tracing the program */

void
Trace_IR(
#ifdef TARG_IA64
  INT phase,        /* Phase after which we're printing */
  const char *pname,    /* Print name for phase */
  BB *cur_bb,       /* BB to limit traces to */
  BOOL after)
#else
  INT phase,            /* Phase after which we're printing */
  const char *pname,    /* Print name for phase */
  BB *cur_bb)           /* BB to limit traces to */
#endif
{
  INT cur_bb_id = cur_bb ? BB_id(cur_bb) : 0;
  if (   Get_Trace(TKIND_IR, phase)
      && (cur_bb_id == 0 || Get_BB_Trace(cur_bb_id)))
  {
#ifdef TARG_IA64
    if(after)
      fprintf(TFile, "\n%s%s\tIR after %s(BEGIN)  PU:%d\n%s%s\n ",
            DBar, DBar, pname, Current_PU_Count(), DBar, DBar);
    else
      fprintf(TFile, "\n%s%s\tIR before %s(BEGIN)  PU:%d\n%s%s\n ",
            DBar, DBar, pname, Current_PU_Count(), DBar, DBar); 	
#else
    fprintf(TFile, "\n%s%s\tIR after %s\n%s%s\n",
            DBar, DBar, pname, DBar, DBar);
#endif
    if (cur_bb != NULL) {
      Print_BB(cur_bb);
    } else {
      BB *bb;
      for (bb = REGION_First_BB; bb; bb = BB_next(bb))  {
        if (Get_BB_Trace(BB_id(bb)) && Get_Trace(TKIND_IR, phase)) {
          Print_BB(bb);
        }
      }
    }
    fprintf(TFile, "%s%s\n", DBar, DBar);
#ifdef TARG_IA64
    if(after)
      fprintf(TFile, "\n%s%s\tIR after %s(END)  PU:%d\n%s%s\n ",
            DBar, DBar, pname, Current_PU_Count(), DBar, DBar);
    else
      fprintf(TFile, "\n%s%s\tIR before %s(END)  PU:%d\n%s%s\n ",
            DBar, DBar, pname, Current_PU_Count(), DBar, DBar);
#endif    	
  }
}
  
static void
Trace_VCG (
  INT phase,            /* Phase after which we're printing */
  const char *pname )   /* Print name for phase */
{
  if ( Get_Trace ( TKIND_VCG, phase ) ) {
    std::stringstream vcg_title_ss;
    char *proc_name = Get_Procedure_Name();
    if (proc_name) 
      vcg_title_ss << proc_name << ".";
    else
      vcg_title_ss << "noname" << "."; 
    char *phase_id = Get_Trace_Phase_Id(phase);
    if (phase_id)
      vcg_title_ss << phase_id << ".vcg";
    else
      vcg_title_ss << phase << ".vcg";
    MEM_POOL temp_pool;
    MEM_POOL_Initialize(&temp_pool, "temp pool", FALSE);
    char* vcg_title =
      (char *) MEM_POOL_Alloc(&temp_pool, vcg_title_ss.str().size()+1);
    strcpy(vcg_title, vcg_title_ss.str().c_str());
    draw_vcg_flow_graph(vcg_title);
    MEM_POOL_Delete(&temp_pool);
  }
}

static void
Trace_TN (
  INT phase,            /* Phase after which we're printing */
  const char *pname )   /* Print name for phase */
{
  if ( Get_Trace ( TKIND_TN, phase ) ) {
    fprintf ( TFile, "\n%s%s\tTNs after %s\n%s%s\n",
             DBar, DBar, pname, DBar, DBar );
    Print_TNs ();
  }
}

static void
Trace_ST (
  INT phase,            /* Phase after which we're printing */
  const char *pname )   /* Print name for phase */
{
  if ( Get_Trace ( TKIND_SYMTAB, phase ) ) {
    fprintf ( TFile, "\n%s%s\tSymbol table after %s\n%s%s\n",
              DBar, DBar, pname, DBar, DBar );
    SYMTAB_IDX level = CURRENT_SYMTAB;
    while (level >= GLOBAL_SYMTAB) {
        Print_symtab (TFile, level);
        --level;
    }
  }
}

static void Check_for_Dump_ALL(INT32 pass, BB *bb, const char * phase)
{

}
/* ====================================================================
 *
 * Check_for_Dump
 *
 * Check whether symbol table, TN, or IR dumps have been requested for
 * the given pass; if so, generate them to the trace file.  If a BB is
 * given, limit the dumps to that BB.
 *
 * ====================================================================
 */
void
Check_for_Dump ( INT32 pass, BB *bb )
{
  if (bb == NULL || Get_BB_Trace(BB_id(bb))) {
    const char *s = Get_Error_Phase();

    /* Check to see if we should dump the STAB.
     */
    Trace_ST ( pass, s );

    /* Check to see if we should dump the TNs.
     */
    Trace_TN ( pass, s );

    /* Check to see if we should dump the IR.  If yes, check each BB.
     */
    Trace_IR ( pass, s, bb );

    /* Check to see if we should create a VCG of the CFG.
     */
    Trace_VCG ( pass, s );
    
    /* Check to see if we should give a memory allocation trace.
     */
    Trace_Memory_Allocation ( pass, s );
  }
}

BOOL
Get_Trace ( INT func, INT arg, BB *bb )
{
  BOOL result = Get_Trace(func, arg);

  /* Check the BB if necessary: */
  if ( result && bb ) {
    result = Get_BB_Trace ( BB_id(bb) );
  }

  return result;
}


void
CG_Dump_Region(FILE *fd, WN *wn)
{
  RID   *rid = REGION_get_rid(wn);
  Is_True(rid != NULL, ("CG_Dump_Region, NULL RID"));
  if (rid && RID_level(rid) >= RL_CGSCHED) {
    CGRIN  *cgrin = RID_cginfo(rid);
    if (cgrin && CGRIN_entry(cgrin)) {
      BB *bb;
      for (bb=CGRIN_entry(cgrin); bb; bb=BB_next(bb))
        Print_BB( bb );
    }
  }
}

void 
CG_Dump_Cur_Region()
{
  BB *bb;
  for( bb=REGION_First_BB; bb; bb=BB_next(bb) ){
    Print_BB( bb );
  }
} 

/* just an externally-visible wrapper to cgemit function */
extern void
CG_Change_Elf_Symbol_To_Undefined (ST *st)
{
    EMT_Change_Symbol_To_Undefined(st);
}

