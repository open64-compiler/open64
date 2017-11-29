/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */


/*
 * Copyright 2002, 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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
 * Module: calls.cxx
 * $Revision: 1.66 $
 * $Date: 05/12/05 08:59:02-08:00 $
 * $Author: bos@eng-24.pathscale.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.calls.cxx $
 *
 * Revision history:
 *  04-Oct-91 - Original Version
 *  24-Oct-91 - Added support for entry/exit code.
 *  27-May-93 - Added support for PIC entry/exit code.
 *
 * Description:
 *
 * General support for call-related functionality in the back end.
 * Utilities for manipulating the call descriptors.
 *
 * ====================================================================
 * ====================================================================
 */

#include "defs.h"
#include "erglob.h"
#include "erbe.h"
#include "glob.h"
#include "tracing.h"
#include "config_targ.h"
#include "config.h"

#include "symtab.h"
#include "strtab.h"
#include "be_symtab.h"
#include "targ_isa_lits.h"
#include "topcode.h"
#include "cg.h"
#include "cg_internal.h"
#include "cg_flags.h"
#include "register.h"
#include "tn.h"
#include "tn_set.h"
#include "bb.h"
#include "op.h"
#include "tn_map.h"

#include "cgemit.h"
#include "ttype.h"
#include "calls.h"
#include "const.h"
#include "data_layout.h"
#include "cgexp.h"
#include "targ_sim.h"
#include "whirl2ops.h"
#include "cg_spill.h"
#include "reg_live.h"
#include "lra.h"
#include "cgtarget.h"
#include "entry_exit_targ.h"
#include "targ_abi_properties.h"
#include "cxx_template.h"
#include "targ_isa_registers.h"
#include "tls.h"              // TLS_get_addr_st
#if defined(TARG_PR) || defined(TARG_PPC32)
#include "cgexp_internals.h"  // Expand_SR_Adj
#endif
#ifdef KEY
#include "gtn_universe.h"
#include "gtn_set.h"
#endif
#ifdef TARG_SL
#include "config_debug.h"    // insert break op in debug mode
#endif
#ifdef TARG_LOONGSON
#include "ipfec_options.h"
#endif

INT64 Frame_Len;
extern BOOL IPFEC_Enable_Edge_Profile;
extern void Clean_Up(BB* bb);

/* Callee-saved register <-> save symbol/TN map: */
SAVE_REG *Callee_Saved_Regs;
INT32 Callee_Saved_Regs_Count;

#ifdef KEY
STACK<SAVE_REG_LOC> Saved_Callee_Saved_Regs(Malloc_Mem_Pool);
#endif

/* Special PREGs associated with save locations for Callee Saved registers */
PREG_NUM *Callee_Saved_Pregs;
PREG_NUM Caller_FP_Preg;
PREG_NUM Caller_GP_Preg;
PREG_NUM Return_Preg;
PREG_NUM GP_Preg;
PREG_NUM Return_Int_Preg[2];
PREG_NUM Return_Float_Preg[2];

/* Return address register map: */
static SAVE_REG Return_Address_Reg_Map;
SAVE_REG *Return_Address_Reg = &Return_Address_Reg_Map;

/* Do we need to setup a new GP for this procedure? */
static enum {
	undefined_code,	/* not set yet */
	never_code,	/* can access gp without setup */
	no_code,	/* doesn't have a gp setup */
	need_code,	/* needs a gp setup */
	final_code	/* has final version of gp setup */
} GP_Setup_Code = undefined_code;

BOOL LC_Used_In_PU;

/* TNs to save the callers GP and FP if needed */
#if defined(TARG_IA64)
TN *Caller_GP_TN;
TN *Caller_FP_TN;
TN *Caller_Pfs_TN;
TN *ra_intsave_tn;
#else
#if defined(TARG_MIPS) || defined(TARG_PPC32)
TN *Caller_GP_TN;
#else
static TN *Caller_GP_TN;
#endif
static TN *Caller_FP_TN;
static TN *Caller_Pfs_TN;
static TN *ra_intsave_tn;
#endif  // TARG_IA64

/* Keep track of a TN with the value of the current PU's stack pointer
 * adjustment (i.e. the frame length):
 */
static TN *Frame_Len_TN;
static TN *Neg_Frame_Len_TN;

BOOL Gen_Frame_Pointer;

void Split_BB_For_br(BB *bb);

/* Trace flags: */
static BOOL Trace_EE = FALSE;	/* Trace entry/exit processing */

#if defined(TARG_SL)
static BOOL Trace_Stack_Allocation = FALSE; /* Trace stack frame allocation */ 
#endif 
/* macro to test if we will use a scratch register to hold gp for
 * the pu.  we do this in leaf routines if there are no regions
 * and gra will be run.
 */
#define Use_Scratch_GP(need_gp_setup) \
((need_gp_setup) && !PU_Has_Calls && !PU_has_region(Get_Current_PU()))

/* ====================================================================
 *
 * Init_Pregs
 *
 * Initialize various pregs for PU wide use.
 * ====================================================================
 */

static void
Init_Pregs ( void )
{
  INT i;
  
  Caller_FP_Preg = Create_Preg( TY_mtype( Spill_Int_Type ), "caller_FP");
  Caller_GP_Preg = Create_Preg( TY_mtype( Spill_Int_Type ), "caller_GP");

  /* Now initialize the return address map: */
  Return_Preg = Create_Preg( TY_mtype( Spill_Int_Type ), "return");

  for ( i = 0; i < 2; i++ ) {
    Return_Int_Preg[i]   = Create_Preg( TY_mtype( Spill_Int_Type ),   "return_int");
    Return_Float_Preg[i] = Create_Preg( TY_mtype( Spill_Float_Type ), "return_float");
  }

  /* set up preg for gp tn */
  GP_Preg = Create_Preg( TY_mtype( Spill_Int_Type ), "GP");
}

/* =======================================================================
 *
 *  Setup_GP_TN_For_PU
 *
 *  set up GT_TN for the PU.  we may be able to place it in a caller saved
 *  register, and if so, allocate one for that purpose.  if not, use $gp.  
 *
 * =======================================================================
 */
#ifndef TARG_X8664
static void
Setup_GP_TN_For_PU( ST *pu)
{
  REGISTER reg;

  if (GP_Setup_Code != undefined_code) {
	/* This can only happen if called more than once per PU,
	 * i.e. when regions.  In this case, keep the initial setting
	 * (so that all bbs are consistent), and then possibly adjust
	 * everything in the PU in Adjust_GP_Entry_Exit */
	;
  }
  else if (Force_GP_Prolog) {
	GP_Setup_Code = need_code;
  }
  else if (!Is_Caller_Save_GP &&
	(Gen_PIC_Call_Shared || Gen_PIC_Shared) &&
	ST_visible_outside_dso(pu) )
  {
	/* if non-leaf that is visible outside dso, then must setup gp
	 * because may call internal routine that uses gp. */
	if (PU_Has_Calls || PU_References_GP) {
		PU_References_GP = TRUE;
		GP_Setup_Code = need_code;
	}
	else {
		/* Don't setup GP unless sure that it is needed.
		 * Adjust_GP_Entry_Exit can add gp setup/save/restore code
		 * at end after we are sure that it is needed.
		 * Otherwise we have to remove gp code, and that gets
		 * complicated with regions, because it can also affect
		 * the boundary sets. */
		GP_Setup_Code = no_code;
	}
  }
  else {
	GP_Setup_Code = never_code;
  }

  /* initialize the gp map */
  TN_MAP_Set( TN_To_PREG_Map, GP_TN, (void *)(INTPTR)GP_Preg );
  PREG_To_TN_Array[ GP_Preg ] = GP_TN;
  PREG_To_TN_Mtype[ GP_Preg ] = TY_mtype(Spill_Int_Type);

  /* we will put gp in a scratch register if this is a leaf routine
   * and gra is to be called (check for REGISTER_UNDEFINED protects
   * agains multiple entry PUs).  we also make gp available as a callee
   * saved register so that using the scratch register will not cause
   * any degradation due to register constraints.  we don't currently
   * have enough information to perform this optimization if regions
   * are present.
   */
#if defined(TARG_MIPS) || defined(TARG_LOONGSON)
  reg = REGISTER_gp;
#else
  if ( Use_Scratch_GP(GP_Setup_Code == need_code) ) {
    REGISTER_SET caller_saves;
    REGISTER_SET func_val;
    REGISTER_SET func_arg;

    REGISTER_Set_Allocatable(REGISTER_CLASS_gp, REGISTER_gp, TRUE);

    /* exclude function return and argument registers from our choices */
    caller_saves = REGISTER_CLASS_caller_saves(REGISTER_CLASS_gp);
    func_val = REGISTER_CLASS_function_value(REGISTER_CLASS_gp);
    caller_saves = REGISTER_SET_Difference(caller_saves, func_val);
    func_arg = REGISTER_CLASS_function_argument(REGISTER_CLASS_gp);
    caller_saves = REGISTER_SET_Difference(caller_saves, func_arg);

    reg = REGISTER_SET_Choose(caller_saves);
    if ( reg == REGISTER_UNDEFINED ) {
      /* no caller saved register available for some reason.  this
       * should not happen, but we'll fail gracefully and just use
       * gp.
       */
      DevWarn("No caller saved register to replace $gp in leaf routine.\n");
      reg = REGISTER_gp;
    }
  } else {
    /* use gp */
    reg = REGISTER_gp;
  }
#endif
  REGISTER_Set_Allocatable(REGISTER_CLASS_gp, reg, FALSE);
  Set_TN_register(GP_TN, reg);
}
#endif

/* =======================================================================
 *
 *  Init_Callee_Saved_Regs_for_REGION 
 *
 *  See interface description.
 *
 * =======================================================================
 */
void
Init_Callee_Saved_Regs_for_REGION ( ST *pu, BOOL is_region )
{
  INT i;
  ISA_REGISTER_CLASS cl;
  TN *stn;

#ifdef ABI_PROPERTY_global_ptr
  Setup_GP_TN_For_PU( pu );
#endif

  if (NULL != RA_TN /* IA-32 doesn't have ra reg. */) {
    /* initialize the return address map: */
    if ( stn = PREG_To_TN_Array[ Return_Preg ] )
      SAVE_tn(Return_Address_Reg) = stn;
    else {
      // we assume even if RA_TN is not integer, 
      // there must be a way to save RA to regular int regs
      SAVE_tn(Return_Address_Reg) = Build_RCLASS_TN(ISA_REGISTER_CLASS_integer);
      Set_TN_save_creg (SAVE_tn(Return_Address_Reg), TN_class_reg(RA_TN));
      TN_MAP_Set( TN_To_PREG_Map, SAVE_tn(Return_Address_Reg),
		  (void *)(INTPTR)Return_Preg );
      PREG_To_TN_Array[ Return_Preg ] = SAVE_tn(Return_Address_Reg);
      PREG_To_TN_Mtype[ Return_Preg ] = Spill_Int_Mtype;
    }
  }
  ra_intsave_tn = NULL;
  Caller_GP_TN = NULL;
  Caller_FP_TN	= NULL;
  Caller_Pfs_TN = NULL;
  if (Pfs_TN) {
  	Caller_Pfs_TN = Gen_Register_TN (ISA_REGISTER_CLASS_integer, Pointer_Size);
  	Set_TN_save_creg (Caller_Pfs_TN, TN_class_reg(Pfs_TN));
  }
 
  /* if called for a region, no need to set up TN's for callee saved
   * registers.  these will be handled in the PU.
   */
  if (is_region) {
    return;
  }

#ifdef TARG_X8664
  if( CG_opt_level > 0  &&
      Is_Target_32bit() &&
      Gen_PIC_Shared    &&
      !PU_References_GOT ){
    TN* ebx_tn = Ebx_TN();
    REGISTER_Set_Allocatable( TN_register_class(ebx_tn),
			      TN_register(ebx_tn),
			      TRUE );
  }  
#endif

  /* allocate the callee-saved register map: */
  Callee_Saved_Regs = (SAVE_REG *)Pu_Alloc (  (ISA_REGISTER_CLASS_MAX + 1) 
					    * (REGISTER_MAX + 1)
					    * sizeof(SAVE_REG) );

  /* build and initialize the save tn's for the callee saved registers */
  Callee_Saved_Regs_Count = 0;
  i = 0;
  FOR_ALL_ISA_REGISTER_CLASS( cl ) {
    REGISTER reg;
    REGISTER_SET regset = REGISTER_CLASS_callee_saves(cl);

    if (REGISTER_CLASS_multiple_save(cl)) continue;

    for ( reg = REGISTER_SET_Choose(regset);
	  reg != REGISTER_UNDEFINED;
	  reg = REGISTER_SET_Choose_Next(regset, reg), ++i
    ) {
      TN *callee_tn;
      TN *ded_tn = Build_Dedicated_TN(cl, reg, 0);
      DevAssert(ded_tn, ("Missing dedicated TN for callee-saved register %s",
			REGISTER_name(cl, reg)));
      CALLEE_ded_tn(i) = ded_tn;
      callee_tn = Build_RCLASS_TN(cl);
      Set_TN_save_creg (callee_tn, TN_class_reg(ded_tn));
      CALLEE_tn(i) = callee_tn;
    }

  }
  if (LC_TN) {
  	// Add ar.lc which is callee-save but not allocatable
  	// so doesn't show up in callee-save regset.
	// We set save_reg so that the copy will automatically
	// be generated, however it is not a normal copy in that
	// it is a copy from ar to int class.  Therefore the normal
	// preferencing will not make it look like a dead copy.
	// So instead of letting gra/lra remove it if unused,
	// we instead will have a separate pass to remove it after cgprep.
	// We could have instead only generated the save/restore after
	// cgprep when we know we need it, but then also need to update
	// all the gra live tn sets which is expensive for just one
	// new tn.  So instead is simpler to generate it up front
	// (so automatically added to tn sets), then remove it if not needed
	// (will still be in tn sets but no references).
	TN *callee_tn = Build_RCLASS_TN (ISA_REGISTER_CLASS_integer);
        Set_TN_save_creg (callee_tn, TN_class_reg(LC_TN));
	CALLEE_tn(i) = callee_tn;
	CALLEE_ded_tn(i) = LC_TN;
  	++i;
  }
  Callee_Saved_Regs_Count = i;
}


/* ===================================================================
 *
 * vararg_st8_2_st8_spill 
 * 
 * convert var-argument spill form from st8 to st8.spill.
 *
 * Since var-argument passed by stacked-register may contains NAT bit,
 * st8 instruction will raise nat-bit-comsumption exception.
 *
 * ===================================================================
 */
#ifdef TARG_IA64
static void
vararg_st8_2_st8_spill (BB * bb) {

  OP * op ;
  FOR_ALL_BB_OPs_REV (bb,op) {
    if (!OP_store(op)) continue ;

    TN * tn = OP_opnd(op,OP_find_opnd_use(op,OU_storeval));
    PREG_NUM preg = TN_To_PREG(tn);

    if (preg == 0)
      continue;

    if (preg < First_Int_Preg_Param_Offset || 
        preg > (First_Int_Preg_Param_Offset + MAX_NUMBER_OF_REGISTER_PARAMETERS))
      continue ;

    /*ST* st = vararg_symbols[preg-First_Int_Preg_Param_Offset];*/
    OP* new_op = Mk_OP(TOP_st8_spill, /* op code */
                OP_opnd(op,OP_find_opnd_use(op,OU_predicate)),
                Gen_Enum_TN(ECV_sthint), /* hint */
                OP_opnd(op,OP_find_opnd_use(op,OU_base)),
                OP_opnd(op,OP_find_opnd_use(op,OU_storeval)));
    BB_Insert_Op_Before (bb,op,new_op);
    BB_Remove_Op (bb,op);
    op=new_op;
  }
}
#endif

#if defined(KEY) && !defined(TARG_NVISA)
struct Save_user_allocated_saved_regs
{
  OPS *ops;
  BB *bb;
  Save_user_allocated_saved_regs(OPS *o, BB *b) : ops(o), bb(b) { }
  void operator() (UINT, ST_ATTR *st_attr) const {
    if (ST_ATTR_kind (*st_attr) != ST_ATTR_DEDICATED_REGISTER)
      return;
    PREG_NUM preg = ST_ATTR_reg_id(*st_attr);
    ISA_REGISTER_CLASS rclass;
    REGISTER reg;
    CGTARG_Preg_Register_And_Class(preg, &rclass, &reg);
    if (! ABI_PROPERTY_Is_callee(rclass, preg-REGISTER_MIN))
      return;
    SAVE_REG_LOC sr;
    sr.ded_tn = Build_Dedicated_TN(rclass, reg, 0);
    DevAssert(sr.ded_tn, ("Missing dedicated TN for callee-saved register %s",
		      REGISTER_name(rclass, reg)));
    if (Is_Unique_Callee_Saved_Reg (sr.ded_tn)) {
      sr.temp = CGSPILL_Get_TN_Spill_Location (sr.ded_tn, CGSPILL_LCL);
      sr.user_allocated = TRUE;
      /* Generate the spill ops */
      CGSPILL_Store_To_Memory (sr.ded_tn, sr.temp, ops, CGSPILL_LCL, bb);
      Set_OP_no_move_before_gra(OPS_last(ops));
      Saved_Callee_Saved_Regs.Push(sr);
    }
  }
};
#endif

/* ====================================================================
 *
 * Generate_Entry
 *
 * Generate entry code (in SGIR form) in the given BB for the given
 * entry symbol.  This currently includes the following:
 *
 *   1) Insert an update of the stack pointer.  If a frame pointer is
 *	required, first copy it from the stack pointer.  This also
 *	requires saving the caller's FP, since it's a callee-saved
 *	register, so we copy it to another temporary register.  (We
 *	can't just store it, since our stack frame hasn't been
 *	established yet.)
 *
 *   2)	Insert stores from dedicated parameter registers for all
 *	declared and possible undeclared formals for varargs routines.
 *
 *   3) If <gra_run>, then GRA will be run and each of the callee_saves
 *	registers is copied to its save TN.
 *
 * ====================================================================
 */

static void
Generate_Entry (BB *bb, BOOL gra_run )
{
  INT callee_num;
  TN *stn;
  OP *op;
  OPS ops = OPS_EMPTY;
  ANNOTATION *ant = ANNOT_Get (BB_annotations(bb), ANNOT_ENTRYINFO);
  ENTRYINFO *ent_info = ANNOT_entryinfo(ant);
  ST *st = ENTRYINFO_name(ent_info);	/* The entry's symtab entry */

  if ((BB_rid(bb) != NULL) && ( RID_level(BB_rid(bb)) >= RL_CGSCHED )) {
  	/* don't change bb's which have already been through CG */
  	return;
  }

#ifdef TARG_IA64
  /*
  Do not generate entry code for handler entry, it is a co-design with unwind directives generation in cgdwarf_targ.cxx
  */
  if (BB_handler(bb)) {
	return;
  }
#endif
  if ( Trace_EE ) {
    #pragma mips_frequency_hint NEVER
    fprintf ( TFile,
	      "\n%s<calls> Generating entry for %s (BB:%d)\n",
	      DBar, ST_name(st), BB_id(bb) );

  }

  if (!BB_handler(bb)) {

#ifndef TARG_LOONGSON // loongson doesn't support pfs
    EETARG_Save_Pfs (Caller_Pfs_TN, &ops);	// alloc
#endif

#ifdef ABI_PROPERTY_stack_ptr
#ifdef TARG_X8664
    /* Initialize the frame pointer if required: */
    if ( Gen_Frame_Pointer && PUSH_FRAME_POINTER_ON_STACK ) {
      Build_OP( Is_Target_64bit() ? TOP_pushq : TOP_pushl, SP_TN, FP_TN, SP_TN,
      		&ops );

      Exp_COPY( FP_TN, SP_TN, &ops );
    }
#endif

    /* Initialize the stack pointer (this is a placeholder; Adjust_Entry
     * will replace it to the actual sequence once we know the size of
     * the frame):
     */
    Exp_Spadjust (SP_TN, Neg_Frame_Len_TN, V_NONE, &ops);

    /* Initialize the frame pointer if required: */
    if ( Gen_Frame_Pointer && !PUSH_FRAME_POINTER_ON_STACK ) {
      // check if fp is callee reg
      // (check abi_property rather than register set
      //  because register_set unsets fp as callee when already allocated).
      if (ABI_PROPERTY_Is_callee (TN_register_class(FP_TN),
	  REGISTER_machine_id(TN_register_class(FP_TN), TN_register(FP_TN)) ))
      {
	// save old fp
      	if ( Caller_FP_TN == NULL ) {
      		/* Build a temp symbol for caller's FP if needed: */
        	if ( stn = PREG_To_TN_Array[ Caller_FP_Preg ] )
  			Caller_FP_TN = stn;
        	else {
			// Bug 13316: FP can hold 64 bit (non pointer) value
  			Caller_FP_TN = Gen_Register_TN ( 
			  ISA_REGISTER_CLASS_integer, TY_size(Spill_Int_Type));
  			Set_TN_save_creg (Caller_FP_TN, TN_class_reg(FP_TN));
  			TN_MAP_Set( TN_To_PREG_Map, Caller_FP_TN, 
				(void *)(INTPTR)Caller_FP_Preg );
  			PREG_To_TN_Array[ Caller_FP_Preg ] = Caller_FP_TN;
  			PREG_To_TN_Mtype[ Caller_FP_Preg ] = TY_mtype(Spill_Int_Type);
        	}
	}
      	/* Save the caller's FP in a temporary: */
      	Exp_COPY (Caller_FP_TN, FP_TN, &ops);
	Set_OP_no_move_before_gra(OPS_last(&ops));
      }
  
      /* Now recover the new FP from the new SP: */
      Exp_Spadjust (FP_TN, Frame_Len_TN, V_NONE, &ops);
    }
    ENTRYINFO_sp_adj(ent_info) = OPS_last(&ops);
#endif //ABI_PROPERTY_stack_ptr

#ifdef TARG_X8664
    if (PU_has_builtin_apply_args) {
        Setup_Builtin_Apply_Args(&ops);
    }
#endif

#ifdef TARG_SL
    // insert break after sp adjust
    if (DEBUG_Stack_Check & STACK_ENTRY_CHECK) {
      Build_OP(TOP_break, &ops);
      Set_OP_no_move_before_gra(OPS_last(&ops));
      Set_OP_volatile(OPS_last(&ops));
    }
#endif

#if defined(KEY) && !defined(TARG_NVISA)
    // bug 4583: save callee-saved registers that may get clobbered 
    // by inline asm
    for (INT i = 0; i < Saved_Callee_Saved_Regs.Elements(); i++) {
      SAVE_REG_LOC sr = Saved_Callee_Saved_Regs.Top_nth(i);

#ifdef TARG_X8664
      if (sr.temp == NULL)
	continue; // handled by push/pop under CG_push_pop_int_saved_regs
#endif
      CGSPILL_Store_To_Memory (sr.ded_tn, sr.temp, &ops, CGSPILL_LCL, bb);
      Set_OP_no_move_before_gra(OPS_last(&ops));
    }

    /* save callee-saved registers allocated to local user variables */
    if ( ST_ATTR_Table_Size (CURRENT_SYMTAB)) {
      For_all_entries(*Scope_tab[CURRENT_SYMTAB].st_attr_tab, 
		      Save_user_allocated_saved_regs(&ops, bb), 1);
    }
#endif
  }

  if ( gra_run ) {
#ifdef TARG_IA64
    /* Add explicit definition of formal registers */
    FOR_ALL_BB_OPs (REGION_First_BB, op){
      for (INT i = 0; i < OP_opnds(op); i++) {
        TN* opnd = OP_opnd(op, i);
        if (!TN_is_register (opnd) || !TN_is_dedicated(opnd)) {
          continue;
        }
        PREG_NUM pnum = TN_To_PREG (opnd);
        if (Is_Formal_Preg (pnum)){
          Exp_COPY(opnd,opnd,&ops);
          Set_OP_no_move_before_gra(OPS_last(&ops));
        }
      }
    }
#endif

    /* Copy from the callee saves registers to register TNs */
    for ( callee_num = 0; callee_num < Callee_Saved_Regs_Count; ++callee_num ) {
      TN *callee_tn = CALLEE_tn(callee_num);
      if (    TN_is_save_reg(callee_tn) 
	  && !REGISTER_CLASS_multiple_save(TN_register_class(callee_tn)))
      {
        Exp_COPY ( callee_tn, CALLEE_ded_tn(callee_num), &ops );
	Set_OP_no_move_before_gra(OPS_last(&ops));
      }
    }
  }

  /* If the return address builtin is required, save RA_TN to the 
   * memory location for __return_address. Otherwise, we copy RA_TN
   * to the save-tn for it. 
   */
  if (NULL != RA_TN) {
    if ( PU_has_return_address(Get_Current_PU()) ) {
#ifndef TARG_NVISA
      // This is broken for IA-32.  On IA-32, the return address is always
      // at a constant offset from the frame pointer, specifically it is
      // accessible as 4(%ebp), but it is never in a register.  Nor
      // does it need to be saved.
      ST *ra_sv_sym = Find_Special_Return_Address_Symbol();
#ifdef TARG_PPC32
      TN *ra_sv_tn = Build_TN_Like(RA_TN);
      Set_TN_spill(ra_sv_tn, ra_sv_sym);
      Exp_COPY (ra_sv_tn, RA_TN, &ops);
      Set_OP_no_move_before_gra(OPS_last(&ops));
      CGSPILL_Store_To_Memory (ra_sv_tn, ra_sv_sym, &ops, CGSPILL_LCL, bb);
#else
      // bug fix for OSP_357
      // When gra is enabled, we build this instruction:
      //       branch_reg1 (save register) copy.br branch_reg
      // just like the floating point/integer registers
      // GRA honors the save register property, and tries to allocate branch_reg1 to branch_reg
      // If it fails, then branch_reg would be spilled, and this instruction will be deleted
      // However, LRA does not honor this and we prefer to directly spill this branch register
      if ( gra_run ) {
	TN *ra_sv_tn = Build_TN_Like(RA_TN);
	Set_TN_save_creg (ra_sv_tn, TN_class_reg(RA_TN));
	Set_TN_spill(ra_sv_tn, ra_sv_sym);
	Exp_COPY (ra_sv_tn, RA_TN, &ops);
	if (MTYPE_byte_size(Pointer_Mtype) < MTYPE_byte_size(Spill_Int_Mtype) ) {
	  /* In n32 the __return_address is 4 bytes (pointer),
	   * but we need 8-byte save/restore to make kernel and dbx happy.
	   * So use dummy 8-byte base that was created. */
	  ra_sv_sym = ST_base(ra_sv_sym);		/* use 8-byte block */
	  Set_TN_spill(ra_sv_tn, ra_sv_sym);	/* so dwarf uses new addr */
	}
	CGSPILL_Store_To_Memory (ra_sv_tn, ra_sv_sym, &ops, CGSPILL_LCL, bb);
      } else {
	CGSPILL_Store_To_Memory (RA_TN, ra_sv_sym, &ops, CGSPILL_LCL, bb);
      }
#endif // TARG_PPC32
#endif // ! TARG_NVISA
    }
#if defined(TARG_PPC32)
    else if (PU_Has_Calls) {
      ST *ra_sym = Find_Special_Return_Address_Symbol();     
      TN *ra_sv_tn = Build_TN_Like(RA_TN);
      Set_TN_spill(ra_sv_tn, ra_sym);
      Exp_COPY (ra_sv_tn, RA_TN, &ops);
      Set_OP_no_move_before_gra(OPS_last(&ops));
      CGSPILL_Store_To_Memory (ra_sv_tn, ra_sym, &ops, CGSPILL_LCL, bb);
    }
#else
#if defined(TARG_IA64) 
    else if (PU_Has_Calls || IPFEC_Enable_Edge_Profile){
      // Some points need to be noted here:
      // First,
      //Currently we know there are two cases need to save/restore b0:
      //      1 :  PU_Has_Calls
      //      2 :  PU has "switch" statement, which will lead to such OP:
      //              "br.few b0","br.few b6" or "br.few b7".
      // for case1, we handle it here.
      // for case2, we do not handle it here. We just simply set <b0> not-allocatable
      //      in register allocation phase to prevent <b0> from being used. This should be safe
      //      because we know at least <b6> and <b7> can be used in "br.few" alike OPs. But when
      //      there are many BBs need more than 2 caller-saved branch registers, we will pay for
      //      more spills. Until now, I have not seen such case, in most case, one branch register
      //      is enough.
      // Another reason we set <b0> not allocatable (see register.cxx) is: we are not sure if there
      //      are other cases which could bring the usage of <b0>.
      // Second,
      //Always use "ISA_REGISTER_CLASS_integer" register to save/restore <b0>, in spite of "gra_run".
      // the purpose is to prevent such CGIR "TN766(sv:b0) :- copy.br TN257(p0) TN123(b0)", if such CGIR
      // appears, it will become "b6 :- copy.br p0 b0" after register allocation. The problem is: "copy.br"
      // is a simulated OP, cgdwarf_targ.cxx will make an assertion that no simulated OP appears.
      // I think this should be a bug. This solution is just to work around it.
      if ( TN_register_class(RA_TN) != ISA_REGISTER_CLASS_integer)
#elif defined(TARG_LOONGSON)
    else { 
      if ( TN_register_class(RA_TN) != ISA_REGISTER_CLASS_integer)
#else
    else {
      if (gra_run && PU_Has_Calls 
	&& TN_register_class(RA_TN) != ISA_REGISTER_CLASS_integer)
#endif
      {
	// because has calls, gra will need to spill this.
	// but if it is not already in an integer reg,
	// then gra will spill to memory whereas for ia64
	// it could use a stacked reg; ideally gra would handle
	// this, but it doesn't and is easy to just copy to int reg
	// by hand and then let gra use stacked reg.
      	 if (ra_intsave_tn == NULL) {
            ra_intsave_tn = Build_RCLASS_TN (ISA_REGISTER_CLASS_integer);
            Set_TN_save_creg (ra_intsave_tn, TN_class_reg(RA_TN));
        }
        Exp_COPY (ra_intsave_tn, RA_TN, &ops );
      }
#if defined(TARG_SL) 
      else if (CG_opt_level <= 1) {
#else
      else {
#endif
      Exp_COPY (SAVE_tn(Return_Address_Reg), RA_TN, &ops );
      }
      Set_OP_no_move_before_gra(OPS_last(&ops));
    }
#endif // TARG_PPC32
  }

  if ( gra_run ) 
    EETARG_Save_Extra_Callee_Tns (&ops);

#ifdef ABI_PROPERTY_global_ptr  // x86_64 does not use GP
  /* Save the old GP and setup a new GP if required */
  if (GP_Setup_Code == need_code) {

    /* we will put the gp in any available register if a leaf routine
     * to avoid the save/restore of the gp.  we will also make gp
     * available as a callee saved register so that in the worst case,
     * we will not generate worse code by using another register for
     * gp.
     */
    if ( !Use_Scratch_GP( TRUE ) ) {
      if ( Caller_GP_TN == NULL ) {
	if ( stn = PREG_To_TN_Array[ Caller_GP_Preg ] )
	  Caller_GP_TN = stn;
	else {
	  Caller_GP_TN = Gen_Register_TN (
		ISA_REGISTER_CLASS_integer, Pointer_Size);
	  Set_TN_save_creg (Caller_GP_TN, TN_class_reg(GP_TN));
	  TN_MAP_Set( TN_To_PREG_Map, Caller_GP_TN, (void *)(INTPTR)Caller_GP_Preg );
	  PREG_To_TN_Array[ Caller_GP_Preg ] = Caller_GP_TN;
	  PREG_To_TN_Mtype[ Caller_GP_Preg ] = TY_mtype(Spill_Int_Type);
	}
      }
      Exp_COPY (Caller_GP_TN, GP_TN, &ops);
    }

    // is possible for altentry to be marked as not_used
    // but we have gp_prolog for whole PU.  In that case,
    // don't generate prolog (that references altentry ST).
    if (ST_is_not_used(st))
	;
#ifndef TARG_LOONGSON
    else if (Gen_PIC_Call_Shared && CGEXP_gp_prolog_call_shared 
	&& (MTYPE_byte_size(Pointer_Mtype) == MTYPE_byte_size(MTYPE_I4)) )
    {
	/* pv 466125 */
	/* we actually ignore the st in this case, but pass it anyways
	 * so that tn structure is regular. */
	TN *gp_value_tn = Gen_Symbol_TN(st, 0, TN_RELOC_GPIDENT);
	Set_TN_size(gp_value_tn, 4);  /* text addresses are always 32bits */
	Exp_OP1 (OPC_I4INTCONST, GP_TN, gp_value_tn, &ops);
    }
#endif
    else {
#ifdef TARG_LOONGSON  // loongson generate setup gp here
     
    if (GP_Setup_Code == need_code) 
    {
	/* added gp reference after usual gp setup time,
	 * so now need to add in gp setup. */
	/* Create a symbolic expression for ep-gp */
	/* Generate_Entry() and Handle_Call_Site() have handled GP save and restore */
 	ST *st = ENTRYINFO_name(
		      ANNOT_entryinfo(ANNOT_Get(BB_annotations(bb),ANNOT_ENTRYINFO)));
		      OPS ops = OPS_EMPTY;

	TN *cur_pu_got_disp_tn = Gen_Symbol_TN(st, 0, TN_RELOC_HI_GPSUB);		   
	Set_TN_size(cur_pu_got_disp_tn, 2);
	TN *tmp=Build_TN_Of_Mtype(MTYPE_I4);      
	Build_OP (TOP_lui, tmp, True_TN, cur_pu_got_disp_tn, &ops);      

	cur_pu_got_disp_tn = Gen_Symbol_TN(st, 0, TN_RELOC_LO_GPSUB);
	Set_TN_size(cur_pu_got_disp_tn, 2);	      
	Build_OP (TOP_addiu, tmp, True_TN, tmp, cur_pu_got_disp_tn, &ops);
	Build_OP (TOP_daddu, GP_TN, True_TN, tmp, Ep_TN, &ops);

	/* Insert the ops at the top of the current BB */
	BB_Prepend_Ops (bb, &ops);

	if (Trace_EE) {
	  #pragma mips_frequency_hint NEVER
	  fprintf(TFile, "%s<calls> Insert spill and setup of GP for BB:%d\n", DBar, BB_id(bb));
	   Print_OPS(&ops);
	 }
    }
#else
	/* Create a symbolic expression for ep-gp */
	TN *cur_pu_got_disp_tn = Gen_Symbol_TN(st, 0, TN_RELOC_GPSUB);
	TN *got_disp_tn = Gen_Register_TN (
		ISA_REGISTER_CLASS_integer, Pointer_Size);
	// text addresses are always 32bits
	Set_TN_size(cur_pu_got_disp_tn, 4);  
	Exp_OP1 (OPC_I4INTCONST, got_disp_tn, cur_pu_got_disp_tn, &ops);

	/* Add it to ep to get the new GP: */
	Exp_ADD (Pointer_Mtype, GP_TN, Ep_TN, got_disp_tn, &ops);
#endif
    }
  } 
  else if (Is_Caller_Save_GP && PU_Has_Calls && !Constant_GP
	&& PREG_To_TN_Array[ Caller_GP_Preg ] != NULL) 
  {
	// need to save old gp but don't need to setup new gp.
	// caller_gp_tn should already be created by call code.
	Caller_GP_TN = PREG_To_TN_Array[ Caller_GP_Preg ];
      	Exp_COPY (Caller_GP_TN, GP_TN, &ops);
  }
#ifdef TARG_LOONGSON
  if (CG_Enable_FTZ 
	&& (PU_is_mainpu(Pu_Table[ST_pu(st)]) || (strcmp(ST_name(st), "main") == 0) 
	     || (strcmp(ST_name(st), "MAIN__") == 0)) )
  {
       // We turn on flush-to-zero mode at the entry of function.
       CGTARG_enable_FTZ(ops);
  }
#endif
#endif

  /* set the srcpos field for all the entry OPs */
  FOR_ALL_OPS_OPs(&ops, op)
    OP_srcpos(op) = ENTRYINFO_srcpos(ent_info);

  /* If we're tracing, print the new stuff before merging it: */
  if ( Trace_EE ) {
    #pragma mips_frequency_hint NEVER
    Print_OPS (&ops);
  }

#ifdef TARG_X8664
  if( Is_Target_32bit() && Gen_PIC_Shared ){
    EETARG_Generate_PIC_Entry_Code( bb, &ops );
  }
#endif
#ifdef TARG_IA64
  if (TY_is_varargs(Ty_Table[PU_prototype(Get_Current_PU())])) {
    vararg_st8_2_st8_spill (bb);
  }
#endif

  /* Merge the new operations into the beginning of the entry BB: */
  BB_Prepend_Ops(bb, &ops);
}


/* ====================================================================
 *
 * Is_Function_Value
 *
 * Return a boolean that indicates if <tn> is a function value TN.
 *
 * ====================================================================
 */
inline BOOL
Is_Function_Value(TN *tn)
{
  if (TN_is_dedicated(tn)) {
    REGISTER reg = TN_register(tn);
    ISA_REGISTER_CLASS rc = TN_register_class(tn);
    return REGISTER_SET_MemberP(REGISTER_CLASS_function_value(rc), reg);
  }
  return FALSE;
}


/* ====================================================================
 *
 * Find_Call_Addr_Load
 *
 * 'call_bb' contains a call. Search the BB for the load of t9 and
 * return it (by value) along with the index of the operand through the
 * out parameter 'iopnd'. If no definition is found, NULL is returned.
 *
 * ====================================================================
 */
static OP *
Find_Call_Addr_Load(BB *call_bb, INT *iopnd)
{
  OP *op;

  for (op = BB_last_op(call_bb); op; op = OP_prev(op)) {
    if (OP_results(op) == 1) {
      TN *tn = OP_result(op,0);
      if (TN_is_ep_reg(tn)) {
	INT i;

	if (!OP_load(op)) break;

	for (i = 0; i < OP_opnds(op); ++i) {
          tn = OP_opnd(op, i);
	  if (TN_is_symbol(tn)) {
	    *iopnd = i;
	    return op;
	  }
	}

	break;
      }
    }
  }

  return NULL;
}


/* ====================================================================
 *
 * Can_Be_Tail_Call
 *
 * Determine if the specified exit block <exit_bb> can be converted
 * to a tail call. If so, the corresponding call BB is returned;
 * otherwise, NULL is returned.
 *
 * ====================================================================
 */
static BB *
Can_Be_Tail_Call(ST *pu_st, BB *exit_bb)
{
  OP *op;
  hTN_MAP fvals;
  PLOC ploc;
  TY_IDX func_type;
  ST *st;
  INT addr_opnd;
  OP *addr_op;
  ANNOTATION *ant;
  CALLINFO *call_info;
  ST *call_st;
  WN *call_wn;
  BB *pred;

  /* The exit block can have only one pred, and it must be a call block.
   */
  pred = BB_Unique_Predecessor(exit_bb);
  if (!pred || !BB_call(pred)) return NULL;

  /* Bug 13846: The tail-call transformation discards the exit block's
   * labels.  Make sure a label is not marked addr_saved.
   */
  if (BB_Has_Addr_Taken_Label(exit_bb)) return NULL;

  /* Get some info about the call and the callee.
   */
  ant = ANNOT_Get(BB_annotations(pred), ANNOT_CALLINFO);
  call_info = ANNOT_callinfo(ant);
  call_st = CALLINFO_call_st(call_info);
  call_wn = CALLINFO_call_wn(call_info);

  /* Assume a call sequence A->B->C (i.e. A calls B which calls C)
   * We would like to change the call B->C to be a tail-call.
   *
   * If C does not setup a new gp (static/internal), it can be a tail-call
   * only if B also does not setup a new GP. Otherwise, C would get A's
   * GP which might be wrong.
   *
   * One complication is that there is another scenario where C will
   * use the caller's GP: if C is preemptible, then its RLD stub
   * uses GP as part of its interface. In these cases we [later] change
   * the relocation for the symbol so it is treated as a data reference,
   * and no stub is generated.
   */
  addr_op = NULL;
  if (   !Is_Caller_Save_GP
      && (Gen_PIC_Call_Shared || Gen_PIC_Shared)
      && ST_visible_outside_dso(pu_st))
  {
    /* If we're making an indirect call to 'C' then we can't be sure
     * whether or not it sets up a GP.
     */
    if (call_st == NULL) return NULL;


    /* __tls_get_addr
     * do not convert __tls_get_addr
     */
    if (call_st == TLS_get_addr_st) return NULL;

    /* 'C' does not setup a GP, so if we make 'B' into a tail call,
     * then 'C' may get an incorrect GP.
     */
    if (!ST_visible_outside_dso(call_st)) return NULL;

    /* Determine if valid GP might be necessary for an RLD stub.
     */
    if (ST_is_preemptible(call_st)) {
      TN *tn;

#ifdef TARG_X8664
      addr_op = BB_xfer_op( pred );
      addr_opnd = 0;
      if( addr_op == NULL ){
	return NULL;
      }
#else
      addr_op = Find_Call_Addr_Load(pred, &addr_opnd);
      if (addr_op == NULL) return NULL;
      tn = OP_opnd(addr_op, addr_opnd);
      if (TN_is_reloc_call16(tn)) {
	/* RE: pv812245, originally we changed the preemptible symbol
	 * to non-preemptible and made it weak. The later causes
	 * symbol preemption to behave differently than it should
	 * so we now just reject the case. The old code remains
	 * in case we decide to do it under some special switch.
	 */
	return NULL;
      } else if (TN_is_reloc_got_disp(tn)) {
	/* ok as is */
	addr_op = NULL;
      } else {
	return NULL;
      }
#endif
    }
  }

#ifdef KEY
  /* Bug 12718: If the callee uses __builtin_return_address, we need
   * to preserve the call. */
  if (call_st && PU_has_return_address(Pu_Table[ST_pu(call_st)]))
    return NULL;
#endif

  /* If any stack variables have had their address taken, it is
   * possible they might be used by the called PU, but if we do the
   * tail call conversion, the stack frame will have been removed.
   */
  if (!BE_ST_pu_has_valid_addr_flags(Get_Current_PU_ST())) return NULL;
  INT i;
  FOREACH_SYMBOL (CURRENT_SYMTAB, st, i) {     /* all local symbols in Current_Symtab */
    if (ST_class(st) != CLASS_VAR) continue;
    if (BE_ST_addr_used_locally(st) || BE_ST_addr_passed(st)) return NULL;
  }

  /* Make sure we don't use the stack to pass arguments to the called PU.
   * Defs have parm info in TY, but calls without prototypes do not,
   * so use whirl call node in that case.
   */
  func_type = call_st ? ST_pu_type(call_st) : WN_ty(call_wn);
  ploc = Setup_Output_Parameter_Locations(func_type);
  if (call_wn == NULL) {
    TYLIST_IDX tl;
    for (tl = TY_parms(func_type); tl != (TYLIST_IDX) NULL; tl = TYLIST_next(tl)) {
      ploc = Get_Output_Parameter_Location(TYLIST_item(tl));
      if (PLOC_on_stack(ploc)) return NULL;
    }
  } else {
    INT i;
    INT num_parms = WN_num_actuals(call_wn);
    for (i = 0; i < num_parms; i++) {
      ploc = Get_Output_Parameter_Location (TY_Of_Parameter(WN_actual(call_wn,i)));
      if (PLOC_on_stack(ploc)) return NULL;
    }
  }

#ifdef TARG_X8664
  /* Don't perform tail call optimization if the caller does not write the result to
     the x87 stack, but the callee does. (bug#2841)
   */
  if( Is_Target_32bit() ){
    bool caller_uses_stack = false;

    const RETURN_INFO caller_return_info =
      Get_Return_Info( TY_ret_type(ST_pu_type(pu_st)),
		       No_Simulated,
		       PU_ff2c_abi(Pu_Table[ST_pu(pu_st)]) );

    for( int i = 0; i < RETURN_INFO_count(caller_return_info); i++ ){
      const TYPE_ID type = RETURN_INFO_mtype( caller_return_info, i );
      if( MTYPE_is_float( type ) ){
	caller_uses_stack = true;
	break;
      }
    }

    const RETURN_INFO callee_return_info =
      Get_Return_Info( TY_ret_type(func_type),
		       No_Simulated,
		       call_st ? PU_ff2c_abi(Pu_Table[ST_pu(call_st)]) : FALSE ); 

    for( int i = 0; i < RETURN_INFO_count(callee_return_info); i++ ){
      const TYPE_ID type = RETURN_INFO_mtype( callee_return_info, i );
      if( MTYPE_is_float( type ) ){
	if( !caller_uses_stack )
	  return NULL;
      }
    }
  }

  /* Don't perform tail call optimization if the caller and callee have different stack
     adjustment size
     see: Generate_Exit
     */
  if ( Is_Target_32bit() && call_st ){
      // call_st might be NULL in indirect calls;
      const TY_IDX caller_ty = ST_pu_type(pu_st);
      const BOOL   caller_ff2c_abi = PU_ff2c_abi(Pu_Table[ST_pu(pu_st)]);
      const RETURN_INFO caller_return_info = 
          Get_Return_Info( TY_ret_type(caller_ty), No_Simulated, caller_ff2c_abi );

      const TY_IDX callee_ty = func_type;
      const BOOL   callee_ff2c_abi = PU_ff2c_abi(Pu_Table[ST_pu(call_st)]);
      const RETURN_INFO callee_return_info =
          Get_Return_Info( TY_ret_type(callee_ty), No_Simulated, callee_ff2c_abi );
      
      int   caller_sp_adjust = 0;
      int   callee_sp_adjust = 0;

      if( RETURN_INFO_return_via_first_arg(caller_return_info) ||
              TY_return_to_param( caller_ty ) ){
          caller_sp_adjust = Pointer_Size;
      }

      // callee adjust SP for stdcall/fastcall at return time
      if (TY_has_stdcall(caller_ty) || TY_has_fastcall(caller_ty)) {
          caller_sp_adjust += Get_PU_arg_area_size(caller_ty);
      }

      if( RETURN_INFO_return_via_first_arg(callee_return_info) ||
              TY_return_to_param( callee_ty ) ){
          callee_sp_adjust = Pointer_Size;
      }

      // callee adjust SP for stdcall/fastcall at return time
      if (TY_has_stdcall(callee_ty) || TY_has_fastcall(callee_ty)) {
          callee_sp_adjust += Get_PU_arg_area_size(callee_ty);
      }

      if ( caller_sp_adjust != callee_sp_adjust ) {
          return NULL;
      }
  }

  /* Under -m32 -fpic, don't do tail call optimization, because the caller
     needs to restore GOT before return.
  */
  if( Is_Target_32bit() &&
      PU_References_GOT ){
    return NULL;
  }
#endif // TARG_X8664

  /* We need to make sure that the function values for the current
   * PU are the same or a subset of the function values for the
   * called PU. We accomplish this by examining uses and defs
   * of the function value TNs. We only allow copies were the
   * ultimate source and destination TN are the same function value
   * TN (the copy may be through an interim local TN). If a
   * function value TN is used in any other way, we reject this case.
   */
  MEM_POOL_Push(&MEM_local_pool);
  fvals = hTN_MAP_Create(&MEM_local_pool);

  FOR_ALL_BB_OPs_FWD(exit_bb, op) {
    if (OP_copy(op)) {
      TN *src = OP_opnd(op,OP_COPY_OPND);
      TN *dst = OP_result(op,0);
      BOOL src_is_fval = Is_Function_Value(src);
      BOOL dst_is_fval = Is_Function_Value(dst);

      if (!src_is_fval) {
	src = (TN *) hTN_MAP_Get(fvals, src);
	if (src) src_is_fval = TRUE;
      }

      if (dst_is_fval) {
	if (   src_is_fval
	    && (TN_register_and_class(src) == TN_register_and_class(dst))
	) continue;
      } else if (src_is_fval) {
	hTN_MAP_Set(fvals, dst, src);
	continue;
      }
    }

    MEM_POOL_Pop(&MEM_local_pool);
    return NULL;
  }
  MEM_POOL_Pop(&MEM_local_pool);

#ifndef TARG_X8664
  /* If we had preemptible symbol for the callee, then change
   * its relocation so we avoid generating a stub for it.
   */
  if (addr_op) {
    TN *old_tn = OP_opnd(addr_op, addr_opnd);
    TN *new_tn = Dup_TN(old_tn);
    Set_TN_is_reloc_got_disp(new_tn);
    Set_OP_opnd(addr_op, addr_opnd, new_tn);
    EMT_Change_Symbol_To_Weak(call_st);
    Set_ST_is_weak_symbol(call_st);
  }
#endif

  return pred;
}


//
// Determine if there are some conditions for the whole PU that
// would disallow tail-call optimization. This acts as a first
// level filter before we analyze each exit block to see if there
// is a potential tail-call there.
//
static BOOL Can_Do_Tail_Calls_For_PU ()
{
  // If the PU performs dynamic stack allocations then it could somehow
  // pass a pointer to the stack to the tail-called routine.
  if (Current_PU_Stack_Model == SMODEL_DYNAMIC) return FALSE;

  // If PU has regions, cannot do tail-call optimization.
  if (PU_has_region(Get_Current_PU())) return FALSE;

  // If the PU calls a routine with the the following pragma:
  //	#pragma unknown_control_flow (func)
  // then we cannot do tail-call optimization for it.
  if (PU_has_unknown_control_flow (Get_Current_PU())) return FALSE;

  // If a PU has a call to setjmp and some other tail call from this
  // PU ultimately has a call to longjmp, then the stack frame will be
  // gone when we get back to the code following the setjmp. Therefore,
  // disable tail call opt for any PU with a setjmp. Also note that
  // setjmp can come in several variants (setjmp, _setjmp, sigsetjmp, etc)
  // so just look for any symbol ending in "setjmp" -- this may be
  // over cautious, but it's better to err on the safe side.
  for (BB *bb = REGION_First_BB; bb; bb = BB_next(bb)) {
    if (BB_call(bb)) {
      const char *name;
      INT len;
      ANNOTATION *callant = ANNOT_Get(BB_annotations(bb), ANNOT_CALLINFO);
      CALLINFO *callinfo = ANNOT_callinfo(callant);
      ST *st = CALLINFO_call_st(callinfo);
      if (st == NULL) continue;
      name = ST_name(st);
      len = strlen(name);
      if (len >= 6 && strcmp(name + len - 6, "setjmp") == 0) return FALSE;
      if (len >= 10 && strcmp(name+len-10, "getcontext") == 0) return FALSE;
    }
  }
  return TRUE;
}


/* ====================================================================
 *
 * Optimize_Tail_Calls
 *
 * Optimize tail calls for a PU by converting the sequence of a call
 * immediately followed by a return into a jump.
 *
 * Note that since we may reset PU_Has_Calls, it requires that
 * the tail call optimization occur before the normal entry/exit
 * processing because stack allocation and GP logic is intertwinned
 * with it.
 *
 * ====================================================================
 */
void
Optimize_Tail_Calls(ST *pu)
{
  BB_LIST *elist;
  BOOL have_tail_call = FALSE;

  // Don't optimize if disabled, -O0 or cannot do tail-calls for PU.
  if (   !CG_tail_call
      || CG_opt_level == 0 
      || !Can_Do_Tail_Calls_For_PU ()) return;

  /* Check each of the exit blocks
   */
  for ( elist = Exit_BB_Head; elist; elist = BB_LIST_rest(elist) ) {
    BB *call_bb;
    BB *exit_bb = BB_LIST_first(elist);

    /* If we have a tail call we can optimize, then do so.
     */
    if (call_bb = Can_Be_Tail_Call(pu, exit_bb)) {
      OP *jmp_op;
      OP *call_op = BB_last_op(call_bb);
      Is_True(OP_call(call_op), ("call block didn't end in a call inst"));

      if (Trace_EE) {
	#pragma mips_frequency_hint NEVER
	fprintf(TFile, "%s<calls> call block before tail call optimization:\n",
			DBar);
	Print_BB(call_bb);
	fprintf(TFile, "\n<calls> exit block before tail call optimization:\n");
	Print_BB(exit_bb);
      }

      /* Replace the call OP with a jump.
       */
      jmp_op = EETARG_Build_Jump_Instead_Of_Call (call_op);
      Set_OP_tail_call(jmp_op);
      BB_Insert_Op_Before(call_bb, call_op, jmp_op);
      BB_Remove_Op(call_bb, call_op);

      /* Transfer the exit info from the exit block to the call block.
       * The call block becomes the new exit block. The exit block
       * is removed from the succ chain and will be removed by cflow later.
       */
      BB_Transfer_Exitinfo(exit_bb, call_bb);
      Unlink_Pred_Succ(call_bb, exit_bb);
      Exit_BB_Head = BB_LIST_Delete(exit_bb, Exit_BB_Head);
      Exit_BB_Head = BB_LIST_Push(call_bb, Exit_BB_Head, &MEM_pu_pool);
      Remove_BB(exit_bb);

      if (Trace_EE) {
	#pragma mips_frequency_hint NEVER
	fprintf(TFile, "\n<calls> exit block after tail call optimization:\n");
	Print_BB(call_bb);
      }

      have_tail_call = TRUE;
    }
  }

  /* If we optimized at least one tail call, then we might have
   * removed the last "normal" call, in which case we might not
   * need a stack. Reseting PU_Has_Calls will make the right
   * things happen.
   */
  if (have_tail_call) {
    BB *bb;
    for (bb = REGION_First_BB; bb; bb = BB_next(bb)) {
      if (BB_call(bb) && !BB_exit(bb)) goto done;
    }
    PU_Has_Calls = FALSE;
  done:
    ;
  }
}


/* ====================================================================
 *
 * Target_Unique_Exit
 *
 * 'bb' is an exit block. Modify it so that it jumps to the unique
 * return block. This would be simple except that the dedicated return
 * value TNs cannot be live out of this block.
 *
 * ====================================================================
 */
static void
Target_Unique_Exit (
  BB *bb,
  BB *unique_exit_bb,
  TN *rtn_tns[ISA_REGISTER_CLASS_MAX+1][REGISTER_MAX+1])
{
  OP *op;

  /* Scan the OPs (in reverse order) in the exit block and handle any 
   * reads of or writes to a function value TN.
   */
  FOR_ALL_BB_OPs_REV(bb, op) {
    INT i;

    /* Handle writes to a function value TN.
     */
    for (i = OP_results(op) - 1; i >= 0; --i) {
      TN *tn = OP_result(op,i);
      if ( Is_Function_Value(tn) ) {
	TN *new_tn;
	REGISTER reg = TN_register(tn);
	ISA_REGISTER_CLASS rc = TN_register_class(tn);

	/* We have a write to a function value TN. These are special
	 * and can't be live out of this block, so we must make a copy.
	 * Get the TN for the copy.
	 */
	new_tn = rtn_tns[rc][reg];
	if ( new_tn == NULL ) {
	  new_tn = Dup_TN_Even_If_Dedicated(tn);
	  if (TN_is_float(tn)) {
	    /* For bug#478
	       OP_result_size measures a TN size in bits; yet
	       TN_size measures a TN size in bytes.
	     */
	    INT tn_size = OP_result_size(op,i) / 8;
#ifdef KEY
	    // When storing a value into a function return register where the
	    // size of the value is smaller than the size of the return
	    // register, Handle_STID will store the value into a temp TN and
	    // copy the TN to the return register.  Get the size from this TN
	    // since it has the correct size.  The correct size is needed to
	    // select the correct move OP code when copying from the temp TN to
	    // the return register in the exit BB.  Bug 14259.
	    if (OP_copy(op))
	      tn_size = TN_size(OP_opnd(op, OP_COPY_OPND));
#endif
	    Set_TN_size(new_tn, tn_size);
	  }
	}

	/* Adjust the result TN of the original function value write.
	 */
	Set_OP_result(op, i, new_tn);

	/* If this is the first time we've encountered this function
	 * value TN then make sure it's set, from the copy,
	 * in the new unique return block.
	 */
	if ( rtn_tns[rc][reg] == NULL ) {
	  ANNOTATION *ant = ANNOT_Get (BB_annotations(unique_exit_bb),
				       ANNOT_EXITINFO);
	  EXITINFO *exit_info = ANNOT_exitinfo(ant);
	  OPS ops = OPS_EMPTY;
	  rtn_tns[rc][reg] = new_tn;
	  Exp_COPY(tn, new_tn, &ops);
	  OP_srcpos(OPS_last(&ops)) = EXITINFO_srcpos(exit_info);
	  BB_Prepend_Ops(unique_exit_bb, &ops);
	}

#if defined(TARG_SL)
/*
 * Fix bug in _load_inttype.c in uclibc with O2
 *
 *   if (return type is int)
 *     return int_a ($2)
 *   else if(return type is long long) 
 *     return long_long_a ($2, $3 (shra.i $3, $2, 31))
 *
 * In the original code, $2 is copied to a new_tn, 
 * But the use of $2 in next top (shra.i $3, $2, 31) is not changed.
 */
  OP * opaf = OP_next(op);
  while (opaf != NULL) {
    for (int j = OP_opnds(opaf) - 1; j >= 0; --j) {
      TN * tnop = OP_opnd(opaf, j);
      if (tnop == tn) {
        if (Trace_EE) {
          #pragma mips_frequency_hint NEVER
          fprintf(TFile, "\nReplace TN %d with %d in Target_Unique_Exit line %d\n",
              TN_number(tnop), TN_number(new_tn), __LINE__);
        }			
        Set_OP_opnd(opaf, j, new_tn);
      }
    }	
    opaf = OP_next(opaf);
  }
#endif
	
      }
    }

    /* A select or unaligned load may have a use of func value TN.
     */
    if ( OP_same_res(op) ) {
      INT i;

      for ( i = 0; i < OP_opnds(op); ++i ) {
	TN *tn = OP_opnd(op,i);
	if ( Is_Function_Value(tn) ) {
	  REGISTER reg = TN_register(tn);
	  ISA_REGISTER_CLASS rc = TN_register_class(tn);
	  TN *new_tn = rtn_tns[rc][reg];
	  FmtAssert(new_tn, ("use before def of return value TN in BB:%d",
			    BB_id(bb)));
	  Set_OP_opnd(op, i, new_tn);
	}
      }
    }
  }

  /* Make sure this exit gets to the unique exit.
   */
  if ( BB_next(bb) == unique_exit_bb ) {
    Target_Simple_Fall_Through_BB(bb, unique_exit_bb);
  } else {
    Add_Goto(bb, unique_exit_bb);
  }
}


/* ====================================================================
 *
 * Generate_Unique_Exit
 *
 * Generate a unique exit and re-target all existing exits to it.
 *
 * ====================================================================
 */
static void
Generate_Unique_Exit(void)
{
  BB_LIST *elist;
  BB *unique_exit_bb;
  INT exits;

  /* Keep track of things we need for creating a unique exit from a PU.
   * We track the return value TNs as well as the BB containing the
   * real exit.
   */
  TN *rtn_tns[ISA_REGISTER_CLASS_MAX+1][REGISTER_MAX+1];

  /* Should we generate a unique exit?
   */
  if (   !CG_unique_exit
      || PU_has_region(Get_Current_PU())) /* until we're sure it works with
					  * regions.
					  */
  {
    return;
  }

  /* If there is less than 2 simple (not tail-call) exits, then this is
   * a pointless exercise.
   */
  exits = 0;
  for ( elist = Exit_BB_Head; elist; elist = BB_LIST_rest(elist) ) {
    BB *bb = BB_LIST_first(elist);
    exits += !BB_call(bb);
  }
  if (exits < 2) return;

  /* Generate the unique exits.
   */
  BZERO(rtn_tns, sizeof(rtn_tns));
  unique_exit_bb = NULL;
  for ( elist = Exit_BB_Head; elist; elist = BB_LIST_rest(elist) ) {
    BB *bb = BB_LIST_first(elist);

    /* Exclude tail call blocks
     */
    if (BB_call(bb)) continue;

    /* This block will no longer be a exit block. So for the first
     * block, transfer the exitinfo to the unique exit block. For
     * the others, just remove the exitinfo.
     */
    if (unique_exit_bb == NULL) {
      unique_exit_bb = Gen_And_Insert_BB_After(bb);
      BB_rid(unique_exit_bb) = BB_rid(bb);
      BB_Transfer_Exitinfo(bb, unique_exit_bb);
      Exit_BB_Head = BB_LIST_Push(unique_exit_bb, Exit_BB_Head, &MEM_pu_pool);
    } else {
      ANNOTATION *ant = ANNOT_Get(BB_annotations(bb), ANNOT_EXITINFO);
      BB_annotations(bb) = ANNOT_Unlink(BB_annotations(bb), ant);
      Reset_BB_exit(bb);
    }
    Exit_BB_Head = BB_LIST_Delete(bb, Exit_BB_Head);

    /* Target the unique exit block.
     */
    Target_Unique_Exit(bb, unique_exit_bb, rtn_tns);
  }
}

#ifdef TARG_X8664 // do we need this for other processors? - SC
void Adjust_SP_After_Call( BB* bb )
{
  OP* op = BB_last_op(bb);
  if( op == NULL || !OP_call( op ) )
    return;

  const ANNOTATION* ant = ANNOT_Get( BB_annotations(bb), ANNOT_CALLINFO );
  const CALLINFO* call_info = ANNOT_callinfo(ant);
  const ST* call_st = CALLINFO_call_st(call_info);
  const WN* call_wn = CALLINFO_call_wn(call_info);
  const TY_IDX call_ty = call_st != NULL ? ST_pu_type(call_st) : WN_ty(call_wn);
  const BOOL ff2c_abi =
    call_st != NULL ? PU_ff2c_abi( Pu_Table[ST_pu(call_st)] ) : FALSE;
  const RETURN_INFO return_info = Get_Return_Info( TY_ret_type(call_ty),
						   No_Simulated,
						   ff2c_abi );

  INT adjust_size = 0;
  /* The C++ front-end will add the first fake param, then convert the
     function return type to void. (bug#2424)
   */
  if( RETURN_INFO_return_via_first_arg(return_info) ||
      TY_return_to_param( call_ty ) ){
    if (!(call_st != NULL && strncmp(ST_name(call_st), "_TRANSFER", 9) == 0))
      adjust_size = 4;
  }

  // adjust sp for stdcall/fastcall
  // stdcall/fastcall adjusted sp at callee site, the orginal purpose is to
  // save caller sites stack adjustment, but when the calling convention 
  // is changed to allocate maximum stack frame for all calls in the function,
  // there is no need to adjust SP after call, so for stdcall/fastcall, we
  // need to adjust the SP in reverse way as in callee return site.
  if (Is_Target_32bit() && (TY_has_fastcall(call_ty) || TY_has_stdcall(call_ty))) {
    adjust_size += Get_PU_arg_area_size(call_ty);
  }
  
  if (adjust_size) {
    OPS ops = OPS_EMPTY;
    Exp_SUB( Pointer_Mtype, SP_TN, SP_TN, Gen_Literal_TN(adjust_size,0), &ops );
    BB_Append_Ops( bb, &ops );

    if( Trace_EE ){
#pragma mips_frequency_hint NEVER
      fprintf( TFile, "%sDecrease SP by %d bytes after call in BB:%d\n",
	       DBar, adjust_size, BB_id(bb) );
      Print_OPS( &ops );
    }
  }
}
#endif


/* ====================================================================
 *
 * Generate_Exit
 *
 * Generate exit code (in SGIR form) in the given BB for the given
 * subprogram.  This currently includes the following:
 *
 *   1) Insert an update of the stack pointer.
 *
 *   2) Restore GP if needed.
 *
 *   3) If <gra_run>, then GRA will be run and each of the callee_saves
 *	registers is copied its save TN.
 *
 * ====================================================================
 */

static void
Generate_Exit (
  ST *st,		/* The subprogram's symtab entry */
  BB *bb,		/* The exit BB to receive code */
  BOOL gra_run,         /* Make the preferencing copies for GRA */
  BOOL is_region)       /* Is a nested region */

{
  INT callee_num;
  TN *stn;
  OP *op;
  OPS ops = OPS_EMPTY;
  ANNOTATION *ant = ANNOT_Get (BB_annotations(bb), ANNOT_EXITINFO);
  EXITINFO *exit_info = ANNOT_exitinfo(ant);
  BB *bb_epi;

  if ( is_region && gra_run ) {
    /* get out if region and running gra.  epilog code handled with
     * PU in gra.
     */
    return;
  }

  if ((BB_rid(bb) != NULL) && ( RID_level(BB_rid(bb)) >= RL_CGSCHED )) {
    if (gra_run) {
      /* if the exit is from a region, then we will create a new block
       * that is part of the PU to hold the exit code for gra.
       */
      bb_epi = Gen_And_Insert_BB_After(bb);
      BB_Transfer_Exitinfo(bb, bb_epi);
      Target_Simple_Fall_Through_BB(bb,bb_epi);
      BB_rid(bb_epi) = Current_Rid;
      Exit_BB_Head = BB_LIST_Delete(bb, Exit_BB_Head);
      Exit_BB_Head = BB_LIST_Push(bb_epi, Exit_BB_Head, &MEM_pu_pool);
    } else {
      /* if gra is not being run, then the epilog code has been added
       * to the return block.
       */
      return;
    }
  } else {
    bb_epi = bb;
  }

  if ( Trace_EE ) {
    #pragma mips_frequency_hint NEVER
    fprintf ( TFile,
	      "\n%s<calls> Generating exit for %s (BB:%d)\n",
	      DBar, ST_name(st), BB_id(bb_epi) );
  }

  /* Restore the caller's GP: */
  if (GP_Setup_Code == need_code) {
    /* we will put the gp in any available register if a leaf routine
     * to avoid the save/restore of the gp.  we will also make gp
     * available as a callee saved register so that in the worst case,
     * we will not generate worse code by using another register for
     * gp.  we don't yet have the information necessary to perform
     * this optimization in the presence of regions.
     */
    if ( !Use_Scratch_GP( TRUE ) ) {
      if ( Caller_GP_TN == NULL ) {
	if ( stn = PREG_To_TN_Array[ Caller_GP_Preg ] )
	  Caller_GP_TN = stn;
	else {
	  Caller_GP_TN = Gen_Register_TN (
		ISA_REGISTER_CLASS_integer, Pointer_Size);
	  TN_MAP_Set( TN_To_PREG_Map, Caller_GP_TN, (void *)(INTPTR)Caller_GP_Preg );
	  PREG_To_TN_Array[ Caller_GP_Preg ] = Caller_GP_TN;
	  PREG_To_TN_Mtype[ Caller_GP_Preg ] = TY_mtype(Spill_Int_Type);
	}
      }
      Exp_COPY (GP_TN, Caller_GP_TN, &ops);
    }
  }

  if ( gra_run )
    EETARG_Restore_Extra_Callee_Tns (&ops);

#ifdef TARG_X8664
  if( Is_Target_32bit() && Gen_PIC_Shared ){
    EETARG_Generate_PIC_Exit_Code( bb_epi, &ops );
  }
#endif

  if (NULL != RA_TN) {
    if ( PU_has_return_address(Get_Current_PU()) ) {
#ifndef TARG_NVISA
      /* If the return address builtin is required, restore RA_TN from the 
       * memory location for __return_address. 
       */
      ST *ra_sv_sym = Find_Special_Return_Address_Symbol();
      TN *ra_sv_tn = Build_TN_Like(RA_TN);
      Set_TN_save_creg (ra_sv_tn, TN_class_reg(RA_TN));
      Set_TN_spill(ra_sv_tn, ra_sv_sym);
      if (MTYPE_byte_size(Pointer_Mtype) < MTYPE_byte_size(Spill_Int_Mtype) ) {
  	/* In n32 the return_address is 4 bytes (pointer),
  	 * but we need 8-byte save/restore to make kernel and dbx happy. */
  	ra_sv_sym = ST_base(ra_sv_sym);		/* use 8-byte block */
  	Set_TN_spill(ra_sv_tn, ra_sv_sym);	/* so dwarf uses new addr */
      }
      CGSPILL_Load_From_Memory (ra_sv_tn, ra_sv_sym, &ops, CGSPILL_LCL, bb_epi);
      Exp_COPY (RA_TN, ra_sv_tn, &ops);
#endif
    }
#ifdef TARG_PPC32
    else if (PU_Has_Calls) {
      ST *ra_sv_sym = Find_Special_Return_Address_Symbol();
      TN *ra_sv_tn = Build_TN_Like(RA_TN);
      Set_TN_spill(ra_sv_tn, ra_sv_sym);
      CGSPILL_Load_From_Memory (ra_sv_tn, ra_sv_sym, &ops, CGSPILL_LCL, bb_epi);
      Exp_COPY (RA_TN, ra_sv_tn, &ops);
    }
#else
#ifdef TARG_IA64
    else if( PU_Has_Calls || IPFEC_Enable_Edge_Profile) {
      // Please see comment in similar place in "Generate_Entry" PU.
      if ( TN_register_class(RA_TN) != ISA_REGISTER_CLASS_integer)
#elif defined(TARG_LOONGSON)
    else { 
      if (TN_register_class(RA_TN) != ISA_REGISTER_CLASS_integer)
#else
    else {
      if (gra_run && PU_Has_Calls 
	&& TN_register_class(RA_TN) != ISA_REGISTER_CLASS_integer)
#endif
      {
	// because has calls, gra will need to spill this.
	// but if it is not already in an integer reg,
	// then gra will spill to memory whereas for ia64
	// it could use a stacked reg; ideally gra would handle
	// this, but it doesn't and is easy to just copy to int reg
	// by hand and then let gra use stacked reg.
	Exp_COPY (RA_TN, ra_intsave_tn, &ops );
	Set_OP_no_move_before_gra(OPS_last(&ops));
      }
#if defined(TARG_SL) 
      else if (CG_opt_level <= 1) {
#else
      else {
#endif
        /* Copy back the return address register from the save_tn. */
      	Exp_COPY ( RA_TN, SAVE_tn(Return_Address_Reg), &ops );
	Set_OP_no_move_before_gra(OPS_last(&ops));
      }
    }
#endif // TARG_PPC32
  }

  if ( gra_run ) {
    /* Copy from register TNs to the callee saves registers */
    for ( callee_num = 0; callee_num < Callee_Saved_Regs_Count; ++callee_num ) {
      TN *callee_tn = CALLEE_tn(callee_num);
      if (    TN_is_save_reg(callee_tn) 
	  && !REGISTER_CLASS_multiple_save(TN_register_class(callee_tn)))
      {
        Exp_COPY ( CALLEE_ded_tn(callee_num), callee_tn, &ops );
	Set_OP_no_move_before_gra(OPS_last(&ops));
      }
    }
  }

#if defined(KEY) && !defined(TARG_NVISA)
  /* restore callee-saved registers allocated to local user variables */
  for (INT i = 0; i < Saved_Callee_Saved_Regs.Elements(); i++) {
    SAVE_REG_LOC sr = Saved_Callee_Saved_Regs.Top_nth(i);
#ifdef TARG_X8664
    if (sr.temp == NULL)
      continue; // handled by push/pop under CG_push_pop_int_saved_regs
#endif
    if (! sr.user_allocated)
      continue;
    /* generate the reload ops */
    CGSPILL_Load_From_Memory (sr.ded_tn, sr.temp, &ops, CGSPILL_LCL, bb_epi);
    Set_OP_no_move_before_gra(OPS_last(&ops));
  }
#endif

  if (PU_Has_Calls) {
#ifndef TARG_LOONGSON  // loongson doesn't support pfs
  	EETARG_Restore_Pfs (Caller_Pfs_TN, &ops);
#endif
  }

#ifdef ABI_PROPERTY_stack_ptr
  /* Restore the stack pointer.
   */
  if ( Gen_Frame_Pointer && !PUSH_FRAME_POINTER_ON_STACK ) {
    Exp_COPY (SP_TN, FP_TN, &ops);
  } 
  else {

    /* This is a placeholder; Adjust_Exit will replace it with the 
     * actual sequence once we know the size of the frame.
     */
    Exp_Spadjust (SP_TN, Frame_Len_TN, V_NONE, &ops);
  }
  EXITINFO_sp_adj(exit_info) = OPS_last(&ops);

  /* Restore the caller's frame pointer register if we used FP: */
  if ( Gen_Frame_Pointer 
        && !PUSH_FRAME_POINTER_ON_STACK
        && ABI_PROPERTY_Is_callee (
	TN_register_class(FP_TN),
	REGISTER_machine_id(TN_register_class(FP_TN), TN_register(FP_TN)) ))
  {
    if ( Caller_FP_TN == NULL ) {
      if ( stn = PREG_To_TN_Array[ Caller_FP_Preg ] )
	Caller_FP_TN = stn;
      else {
	// Bug 13316: FP can hold 64 bit (non pointer) value
	Caller_FP_TN = Gen_Register_TN (
	  ISA_REGISTER_CLASS_integer,  TY_size(Spill_Int_Type));
	TN_MAP_Set( TN_To_PREG_Map, Caller_FP_TN, (void *)(INTPTR)Caller_FP_Preg );
	PREG_To_TN_Array[ Caller_FP_Preg ] = Caller_FP_TN;
  	PREG_To_TN_Mtype[ Caller_FP_Preg ] = TY_mtype(Spill_Int_Type);
      }
    }
    Exp_COPY (FP_TN, Caller_FP_TN, &ops);
    Set_OP_no_move_before_gra(OPS_last(&ops));
  }
#endif

#ifdef TARG_SL
    // insert "break16" before exit
    if (DEBUG_Stack_Check & STACK_EXIT_CHECK) {
      Build_OP(TOP_break, &ops);
      Set_OP_no_move_before_gra(OPS_last(&ops));
      Set_OP_volatile(OPS_last(&ops));
    }
#endif

  /* Generate the return instruction, unless is this a tail call
   * block, in which case the xfer instruction is already there.
   */
  if (!BB_call(bb_epi)) { 
#ifdef TARG_X8664
    int sp_adjust = 0;

    if( Is_Target_32bit() ){
      const TY_IDX call_ty = ST_pu_type(st);
      const BOOL ff2c_abi = PU_ff2c_abi(Pu_Table[ST_pu(st)]);
      const RETURN_INFO return_info = Get_Return_Info( TY_ret_type(call_ty),
						       No_Simulated,
						       ff2c_abi );
      if( RETURN_INFO_return_via_first_arg(return_info) ||
	  TY_return_to_param( call_ty ) ){
	sp_adjust = Pointer_Size;
      }

      // callee adjust SP for stdcall/fastcall at return time
      if (TY_has_stdcall(call_ty) || TY_has_fastcall(call_ty)) {
        sp_adjust += Get_PU_arg_area_size(call_ty);
      }
          
    }

    Exp_Return( RA_TN, sp_adjust, &ops );
#else
    Exp_Return (RA_TN, &ops);
#endif // TARG_X8664
  }

  /* set the srcpos field for all the exit OPs */
  FOR_ALL_OPS_OPs(&ops, op)
    OP_srcpos(op) = EXITINFO_srcpos(exit_info);

  /* If we're tracing, print the new stuff before merging it: */
  if ( Trace_EE ) {
    #pragma mips_frequency_hint NEVER
    Print_OPS(&ops);
  }

  /* Merge the new operations into the end of the exit BB: */
  if (BB_call(bb_epi)) {

    /* If it's a tail call block we insert the new operations in front
     * of the jump.
     */
    OP *point = BB_last_op(bb_epi);
    Is_True(OP_br(point), ("last tail call OP of BB:%d not a jump", BB_id(bb_epi)));
    BB_Insert_Ops_Before(bb_epi, point, &ops);
  } else {
    BB_Append_Ops(bb_epi, &ops);
  }
}

// sets spadjust TN values
extern void 
Set_Frame_Len (INT64 val)
{
#ifdef TARG_X8664
  if (CG_min_stack_size &&
      !Stack_Frame_Has_Calls()) {	// Align stack before calls.  Bug 8017.
    extern BOOL Is_Stack_Used();
    if( !Is_Stack_Used() )
      val = 0;
  }
#endif

  Frame_Len = val;
  Set_TN_value(Frame_Len_TN, val);
  Set_TN_value(Neg_Frame_Len_TN, -val);
}

/* we now generate the final code after pu is processed,
 * but still need to init some stuff before processing pu. */
void
Init_Entry_Exit_Code (WN *pu_wn)
{
  Trace_EE = Get_Trace ( TP_CGEXP, 64 );

#if defined(TARG_SL)
  Trace_Stack_Allocation = Get_Trace(TP_CGEXP, 2048); 
#endif 


  GP_Setup_Code = undefined_code;
  Caller_GP_TN = NULL;
  Caller_FP_TN	= NULL;

  // initialize values to dummy value
  Frame_Len_TN = Gen_Unique_Literal_TN(0,8);
  Neg_Frame_Len_TN = Gen_Unique_Literal_TN(0,8);

  Gen_Frame_Pointer = (Current_PU_Stack_Model != SMODEL_SMALL);
#ifdef TARG_X8664
  if (Opt_Level == 0 || Force_Frame_Pointer || Call_Mcount ||
      Debug_Level > 0)
    Gen_Frame_Pointer = TRUE;// because return address always stored at offset 0
#endif
#ifdef KEY
  Saved_Callee_Saved_Regs.Clear();
#endif

  // target-specific code (e.g. for stacked registers)
  EETARG_Init_Entry_Exit_Code (pu_wn, Gen_Frame_Pointer);

  Init_Pregs ();

  LC_Used_In_PU = FALSE;
}

#ifdef TARG_X8664
/* ====================================================================
 *
 * Generate_Entry_Merge_Clear
 *
 * Generate Clear of Merge dependencies for YMM regs and usage of 
 * avx 128-bit insns.
 *
 * ====================================================================
 */

void Generate_Entry_Merge_Clear(BOOL is_region)
{
  OP *op;

  // Do some cleanup before we get to emit or CG_sched
  for( BB* bb = REGION_First_BB; bb != NULL; bb = BB_next(bb) ){
    for (op = BB_first_op(bb); op != NULL; op = OP_next(op)) {
      OP_Change_Aux_Opcode( op, 0, 0 );
    }
  }

  if (CG_NoClear_Avx_Simd) 
    return;

  // If we have avx128 bit instructions, at the entry block, add a 
  // vzeroupper insn to clear the upper 128bits and
  // avoid merge dependencies on the machine.  Otherwise we would have
  // to allow a 16 dst operand insn so that all the regs can show a def.
  // Doing this after final scheduling means we do not need any special
  // rules for placing this insn.
  for( BB* bb = REGION_First_BB; bb != NULL; bb = BB_next(bb) ){
    if (BB_entry(bb)) {
      OP *vzup = Mk_OP(TOP_vzeroupper);
      if (BB_first_op(bb) == NULL) {
        // we are in a main block
        BB *next = BB_next(bb);
        BB_Insert_Op_Before(next, BB_first_op(next), vzup);
      } else {
        BB_Insert_Op_Before(bb, BB_first_op(bb), vzup);
      }
    }
  }
}
#endif

/* ====================================================================
 *
 * Generate_Entry_Exit_Code
 *
 * Generate entry and exit code for the current PU.  This involves
 * calling Generate_Entry for each entry BB, and Generate_Exit
 * for each exit BB.
 *
 * ====================================================================
 */

void
Generate_Entry_Exit_Code ( ST *pu, BOOL is_region )
{
  BB_LIST *elist;
  BOOL gra_run = ! CG_localize_tns;

  Is_True(pu != NULL,("Generate_Entry_Exit_Code, null PU ST"));
  /* assume GP_Setup_Code already set */

  for ( elist = Entry_BB_Head; elist; elist = BB_LIST_rest(elist) ) {
    Generate_Entry (BB_LIST_first(elist), gra_run );
  }

  Generate_Unique_Exit ();

  for ( elist = Exit_BB_Head; elist; elist = BB_LIST_rest(elist) ) {
    Generate_Exit (pu, BB_LIST_first(elist), gra_run, is_region );
  }

 if (GP_Setup_Code == need_code && !is_region)
	/* don't change setting till done with regions */
	GP_Setup_Code = final_code;
}


static void
Adjust_GP_Entry(BB *bb)
{
  if (GP_Setup_Code == need_code) {
	/* added gp reference after usual gp setup time,
	 * so now need to add in gp setup. */
  	/* Save the old GP and setup a new GP */
	/* Create a symbolic expression for ep-gp */
#ifdef TARG_NVISA
	FmtAssert(FALSE, ("NYI"));
	return;
#endif
#ifndef TARG_LOONGSON // loongson already setup GP
	ST *st = ENTRYINFO_name(	/* The entry's symtab entry */
		ANNOT_entryinfo(ANNOT_Get(BB_annotations(bb),ANNOT_ENTRYINFO)));
	TN *got_disp_tn = Gen_Register_TN (
		ISA_REGISTER_CLASS_integer, Pointer_Size);

	ST *mem_loc = CGSPILL_Get_TN_Spill_Location (GP_TN, CGSPILL_LCL);
	OPS ops = OPS_EMPTY;
	TN *cur_pu_got_disp_tn = Gen_Symbol_TN(st, 0, TN_RELOC_GPSUB);

	/* Generate the spill ops */
	CGSPILL_Store_To_Memory (GP_TN, mem_loc, &ops, CGSPILL_LCL, bb);

	// text addresses are always 32bits
	Set_TN_size(cur_pu_got_disp_tn, 4);  
	Exp_OP1 (OPC_I4INTCONST, got_disp_tn, cur_pu_got_disp_tn, &ops);

    	/* Add it to ep to get the new GP: */
    	Exp_ADD (Pointer_Mtype, GP_TN, Ep_TN, got_disp_tn, &ops);

	/* allocate registers for any temps used in spill sequence */
	// Assign_Temp_Regs (&ops, bb);

	/* insert the ops in the op list for the current BB */
	CGSPILL_Prepend_Ops (bb, &ops);

	if (Trace_EE) {
		#pragma mips_frequency_hint NEVER
    		fprintf(TFile, "%s<calls> Insert spill and setup of GP for BB:%d\n", DBar, BB_id(bb));
		Print_OPS(&ops);
	}
#endif
  }
}

static void
Adjust_GP_Exit (BB *bb)
{
  if (GP_Setup_Code == need_code) {
	/* added gp reference after usual gp setup time,
	 * so now need to add in gp setup. */
#ifdef TARG_NVISA
	FmtAssert(FALSE, ("NYI"));
	return;
#endif
	ST *mem_loc = CGSPILL_Get_TN_Spill_Location (GP_TN, CGSPILL_LCL);
	OPS ops = OPS_EMPTY;

	/* generate the reload ops */
	CGSPILL_Load_From_Memory (GP_TN, mem_loc, &ops, CGSPILL_LCL, bb);
	/* allocate registers for any temps used in spill sequence */
	// Assign_Temp_Regs (&ops, bb);

	/* insert the ops in the op list for the current BB */
	CGSPILL_Append_Ops (bb, &ops);

	if (Trace_EE) {
		#pragma mips_frequency_hint NEVER
    		fprintf(TFile, "%s<calls> Insert restore of GP for BB:%d\n", DBar, BB_id(bb));
		Print_OPS (&ops);
	}
  }
}

/* possibly add the GP setup code */
void
Adjust_GP_Setup_Code (ST *pu, BOOL allocate_registers)
{
  BB_LIST *elist;

  if (GP_Setup_Code == no_code && PU_References_GP) {
	/*
	 * This can happen for several reasons:
	 * 1) when regions, don't see full PU at first;
	 * 2) spilling a fcc introduces a gp reference;
	 * 3) rematerialization may use gp.
	 * So we can introduce GP references as late as LRA.
	 * In this case, we have to add the gp setup/save/restore code,
	 * and allocate registers for it (since after GRA/LRA).
	 * The placement may not be optimal, but GRA can't shrinkwrap
	 * anyways since new uses may occur after GRA.
	 */
	/*
	 * CHANGE:  setup gp after cgprep, and don't allow new gp
	 * uses from fcc spills.  This way LRA sees that t9 and gp
	 * are used.
	 */
	FmtAssert(!allocate_registers, ("Created new GP reference during register allocation"));
	// Is_True(allocate_registers, ("expect to allocate registers now"));
	GP_Setup_Code = need_code;
  }
  else return;

  for (elist = Entry_BB_Head; elist; elist = BB_LIST_rest(elist)) {
    Adjust_GP_Entry(BB_LIST_first(elist));
  }

  for (elist = Exit_BB_Head; elist; elist = BB_LIST_rest(elist)) {
    Adjust_GP_Exit(BB_LIST_first(elist));
  }

  if (GP_Setup_Code == need_code)
	GP_Setup_Code = final_code;
}


static void
Adjust_LC_Entry (BB *bb)
{
  OP *op;
  INT i;
  FOR_ALL_BB_OPs_FWD(bb, op) {
    for ( i = 0; i < OP_opnds(op); ++i ) {
	if (OP_opnd(op,i) == LC_TN && OP_no_move_before_gra(op)) {
    		BB_Remove_Op(bb, op);
		if (Trace_EE) {
			#pragma mips_frequency_hint NEVER
    			fprintf(TFile, "<calls> remove save of LC in BB:%d\n", BB_id(bb));
		}
	}
    }
  }
}

static void
Adjust_LC_Exit (BB *bb)
{
  OP *op;
  INT i;
  FOR_ALL_BB_OPs_FWD(bb, op) {
    for ( i = 0; i < OP_results(op); ++i ) {
	if (OP_result(op,i) == LC_TN && OP_no_move_before_gra(op)) {
    		BB_Remove_Op(bb, op);
		if (Trace_EE) {
			#pragma mips_frequency_hint NEVER
    			fprintf(TFile, "<calls> remove restore of LC in BB:%d\n", BB_id(bb));
		}
	}
    }
  }
}

/* possibly remove save/restore of LC. */
/* Must call this after cgprep */
void
Adjust_LC_Setup_Code (void)
{
  if (LC_TN == NULL) return;	// doesn't exist for target
  if (LC_Used_In_PU) return;	// keep save/restore
  if (CG_localize_tns)	return;	// never generated initial save/restore

  BB_LIST *elist;
  for (elist = Entry_BB_Head; elist; elist = BB_LIST_rest(elist)) {
    Adjust_LC_Entry(BB_LIST_first(elist));
  }
  for (elist = Exit_BB_Head; elist; elist = BB_LIST_rest(elist)) {
    Adjust_LC_Exit(BB_LIST_first(elist));
  }
}


/* ====================================================================
 *
 * Assign_Prolog_Temps
 *
 * Very simplistic register allocation. Intended soley for use
 * on the sequence of insts to load the stack frame size constant.
 * TODO:  merge this with Assign_Temp_Regs.
 *
 * ====================================================================
 */
static void
Assign_Prolog_Temps(OP *first, OP *last, REGISTER_SET *temps)
{
  OP *op;
  ISA_REGISTER_CLASS cl;
  REGISTER reg;

  /* Assume we will only need one register and that its class will
   * be the same as $sp. Reserve the register now. Note that no
   * checking is performed to validate this assumption.
   */
  cl = TN_register_class(SP_TN);
  reg = REGISTER_SET_Choose(temps[cl]);
  FmtAssert(reg != REGISTER_UNDEFINED, ("no free temps"));
  temps[cl] = REGISTER_SET_Difference1(temps[cl], reg);

  /* Loop over the OPs in the sequence, allocating registers.
   */
  for (op = first; op != OP_next(last); op = OP_next(op)) {
    TN *tn;
    INT k;

    REGISTER_CLASS_OP_Update_Mapping(op);

    for (k = 0; k < OP_results(op); k++) {
      tn = OP_result(op,k);
      if (TN_register(tn) == REGISTER_UNDEFINED) {
	FmtAssert(TN_register_class(tn) == cl,
		  ("unexpected register class for unallocated register"));
	TN_Allocate_Register(tn, reg);
      }
    }

    for (k = 0; k < OP_opnds(op); k++) {
      tn = OP_opnd(op,k);
      if (TN_is_register(tn) && TN_register(tn) == REGISTER_UNDEFINED) {
	FmtAssert(TN_register_class(tn) == cl,
		  ("unexpected register class for unallocated register"));
	TN_Allocate_Register(tn, reg);
      }
    }
  }
}


/* ====================================================================
 *
 * Gen_Prolog_LDIMM64
 *
 * Generate an instruction sequence to load an arbitrary 64-bit value
 *
 * ====================================================================
 */
static TN *
Gen_Prolog_LDIMM64(UINT64 val, OPS *ops)
{  
#ifdef TARG_SL
  TN *src, *result;
  if (val >= 0 && val <= UINT32_MAX) {
    src = Gen_Literal_TN(val, 4);
    result = Build_TN_Of_Mtype (MTYPE_U4);
    Exp_Immediate (result, src, MTYPE_U4, ops);
  } else if (val > UINT32_MAX && val <= UINT64_MAX) {
    src = Gen_Literal_TN(val, 8);
    result = Build_TN_Of_Mtype (MTYPE_U4);
    TN *result_h = Build_TN_Of_Mtype (MTYPE_U4);
    
    extern void Add_TN_Pair(TN *key, TN *pair);
    Add_TN_Pair(result, result_h);
    Exp_Immediate (result, src, MTYPE_U8, ops);
  } else {
    FmtAssert(FALSE, ("Gen_Prolog_LDIMM64: error value"));
  }
#else
  TN *src = Gen_Literal_TN(val, 8);
  TN *result = Build_TN_Of_Mtype (MTYPE_I8);
  Exp_Immediate (result, src, TRUE, ops);
#endif

  return result;
}

/* ====================================================================
 *
 * Adjust_Entry
 *
 * Adjust the stack frame allocation code as necessary now that the
 * actual frame size is known.
 *
 * ====================================================================
 */
static void
Adjust_Entry(BB *bb)
{
  UINT64 frame_len = Frame_Len;
  ANNOTATION *ant = ANNOT_Get(BB_annotations(bb), ANNOT_ENTRYINFO);
  ENTRYINFO *ent_info = ANNOT_entryinfo(ant);
  OP *ent_adj = ENTRYINFO_sp_adj(ent_info);
  OP *fp_adj;
  OP *sp_adj;
  TN *sp_incr;
  TN *fp_incr;

  if (BB_handler(bb)) return;
#if defined(TARG_SL)
  if (Trace_EE || Trace_Stack_Allocation) {
#else 
  if (Trace_EE) {
#endif 
    #pragma mips_frequency_hint NEVER
    fprintf(TFile,
	    "\n%s<calls> Adjusting entry for %s (BB:%d)\n",
	    DBar, ST_name(ENTRYINFO_name(ent_info)), BB_id(bb));
    fprintf(TFile, "\nFinal frame size: %llu (0x%llx)\n", frame_len, frame_len);
  }

#ifdef TARG_X8664
  if (CG_push_pop_int_saved_regs && ! Gen_Frame_Pointer) {
    OPS ops = OPS_EMPTY;
    for (INT i = 0; i < Saved_Callee_Saved_Regs.Elements(); i++) {
      SAVE_REG_LOC sr = Saved_Callee_Saved_Regs.Top_nth(i);
      if (sr.temp != NULL)
	continue;
      Build_OP(Is_Target_64bit() ? TOP_pushq : TOP_pushl, SP_TN, sr.ded_tn, 
      	       SP_TN, &ops);
    }
    BB_Insert_Ops_Before(bb, ent_adj, &ops);
  }
#endif

  /* The ENTRYINFO annotation identifies the last instruction of the
   * stack frame allocation sequence. Therefore the instruction could
   * be either the adjustment of SP or FP. Find both the FP and SP
   * adjust OPs (will be the same when we have a virtual FP).
   */
  sp_adj = ent_adj;
  fp_adj = sp_adj;
  if ( Gen_Frame_Pointer && !PUSH_FRAME_POINTER_ON_STACK ) {
    do {
      sp_adj = OP_prev(sp_adj);
      //
      // Spills can be introduced now by GRA.  Skip 'em.
      //
    } while (!OP_result(sp_adj, 0) || !TN_is_sp_reg(OP_result(sp_adj,0)));
  }

  /* Get the operands that are the frame size increment.
   */
  sp_incr = OP_opnd(sp_adj, OP_find_opnd_use(sp_adj, OU_opnd2));
  fp_incr = (fp_adj != sp_adj) 
	? OP_opnd(fp_adj, OP_find_opnd_use(fp_adj, OU_opnd2))
	: NULL;

  /* Trace the sequence before we modify it.
   */
  if (Trace_EE) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "\nOld stack frame allocation:\n");
    Print_OP_No_SrcLine(sp_adj);
    if (fp_adj != sp_adj) Print_OP_No_SrcLine(fp_adj);
  }

  /* We make assumptions about what we generated in Generate_Entry.
   * Try to make sure our assumptions were right:
   *   SP-adjust: TOP_spadjust $sp, $sp, -Frame_Len_TN
   *   FP-adjust: TOP_spadjust $fp, $sp, Frame_Len_TN
   */
  FmtAssert(   OP_code(sp_adj) == TOP_spadjust
	    && OP_results(sp_adj) == 1
	    && TN_is_sp_reg(OP_result(sp_adj,0))
	    && TN_is_sp_reg(OP_opnd(sp_adj,
		OP_find_opnd_use(sp_adj, OU_opnd1)))
	    && sp_incr == Neg_Frame_Len_TN
	    && ( ! OP_has_predicate(sp_adj) 
		|| OP_opnd(sp_adj, OP_PREDICATE_OPND) == True_TN), 
	    ("Unexpected form of entry SP-adjust OP"));
  if (fp_adj != sp_adj) {
    FmtAssert(   OP_code(fp_adj) == TOP_spadjust 
	      && OP_results(fp_adj) == 1
	      /* && OP_result(fp_adj,0) == FP_TN */
	      && TN_is_dedicated_class_and_reg (OP_result(fp_adj,0), 
					TN_register_and_class(FP_TN))
	      && TN_is_sp_reg(OP_opnd(fp_adj,
		OP_find_opnd_use(fp_adj, OU_opnd1)))
	      && fp_incr != NULL 
	      && fp_incr == Frame_Len_TN 
	      && ( ! OP_has_predicate(fp_adj) 
		  || OP_opnd(fp_adj, OP_PREDICATE_OPND) == True_TN), 
	      ("Unexpected form of entry FP-adjust OP"));
  }

  /* Perform any adjustments. We will either remove, change, or
   * leave alone, the adjustment OP.
   */
  if (frame_len == 0) {
    BB_Remove_Op(bb, sp_adj);

    if (sp_adj == fp_adj) {
      ent_adj = NULL;
    } else {
      OPS ops = OPS_EMPTY;

      /* Replace the FP adjust placeholder with the new adjustment OP.
       * Note that we just do this for an artifical situation that
       * at one time could be created with -TENV:large_stack, and in
       * in fact even that currently doesn't cause a problem. But just
       * in case, we make the case work.
       */
      Exp_COPY(FP_TN, SP_TN, &ops);
      ent_adj = OPS_last(&ops);
      OP_srcpos(ent_adj) = OP_srcpos(fp_adj);
      BB_Insert_Ops_Before(bb, fp_adj, &ops);
      BB_Remove_Op(bb, fp_adj);
    }

    if (Trace_EE) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "\nNew stack frame allocation:\n"
		     "-- removed --\n");
    }
  } else {
    OPS ops = OPS_EMPTY;
    TN *incr;
    OP *op;

    /* Load the increment into a register if it is too large to
     * be used as an immediate operand.
     */
    if (!CGTARG_Can_Fit_Immediate_In_Add_Instruction (frame_len)) {
      REGISTER_SET temps[ISA_REGISTER_CLASS_MAX+1];

      /* Get the frame size into a register
       */
      REG_LIVE_Prolog_Temps(bb, sp_adj, fp_adj, temps);
      if (Trace_EE) {
	#pragma mips_frequency_hint NEVER
	ISA_REGISTER_CLASS cl;

	fprintf(TFile, "\nInteger temp register usage at prolog SP adjust:\n");
	FOR_ALL_ISA_REGISTER_CLASS(cl) {
	  if (cl == TN_register_class(SP_TN)) {
	    fprintf(TFile, "  avail=");
	    REGISTER_SET_Print(temps[cl], TFile);
	    fprintf(TFile, "\n");
	  }
	}
      }
#ifdef TARG_PPC32
      incr = Build_TN_Of_Mtype(MTYPE_I4);
      Exp_Immediate(incr, Gen_Literal_TN(-frame_len, 4), TRUE, &ops);
#else
      incr = Gen_Prolog_LDIMM64(frame_len, &ops);
#endif
      Assign_Prolog_Temps(OPS_first(&ops), OPS_last(&ops), temps);
    } else {

      /* Use the frame size symbol
       */
#ifdef TARG_PPC32
      incr = Gen_Literal_TN(-frame_len, 4);
#else
      incr = Frame_Len_TN;
#endif
    }

    /* Replace the SP adjust placeholder with the new adjustment OP
     */
#if defined(TARG_PR) || defined(TARG_PPC32)
    BOOL isAdd = TRUE;
    Expand_SR_Adj(isAdd, SP_TN, incr, &ops);
#else
    Exp_SUB (Pointer_Mtype, SP_TN, SP_TN, incr, &ops);
#endif
    FOR_ALL_OPS_OPs_FWD(&ops, op) OP_srcpos(op) = OP_srcpos(sp_adj);
    ent_adj = OPS_last(&ops);
    BB_Insert_Ops_Before(bb, sp_adj, &ops);
    BB_Remove_Op(bb, sp_adj);
    if (Trace_EE) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "\nNew stack frame allocation:\n");
      FOR_ALL_OPS_OPs_FWD(&ops, op) Print_OP_No_SrcLine(op);
    }

    if (fp_adj != sp_adj) {

      /* Replace the FP adjust placeholder with the new adjustment OP
       */
      OPS_Init(&ops);
#ifdef TARG_PPC32
      Exp_SUB(Pointer_Mtype, FP_TN, SP_TN, incr, &ops);
#else
      Exp_ADD (Pointer_Mtype, FP_TN, SP_TN, incr, &ops);
#endif
      ent_adj = OPS_last(&ops);
      OP_srcpos(ent_adj) = OP_srcpos(fp_adj);
      BB_Insert_Ops_Before(bb, fp_adj, &ops);
      BB_Remove_Op(bb, fp_adj);
      if (Trace_EE) {
	#pragma mips_frequency_hint NEVER
      	FOR_ALL_OPS_OPs_FWD(&ops, op) Print_OP_No_SrcLine(op);
      }
    }
  }

  /* Point to the [possibly] new SP adjust OP
   */
  ENTRYINFO_sp_adj(ent_info) = ent_adj;

  // possible do target-dependent fixups
  EETARG_Fixup_Entry_Code (bb);
}


/* ====================================================================
 *
 * Adjust_Exit
 *
 * Adjust the stack frame de-allocation code as necessary now that the
 * actual frame size is known.
 *
 * ====================================================================
 */
static void
Adjust_Exit(ST *pu_st, BB *bb)
{
  ANNOTATION *ant = ANNOT_Get(BB_annotations(bb), ANNOT_EXITINFO);
  EXITINFO *exit_info = ANNOT_exitinfo(ant);
  OP *sp_adj = EXITINFO_sp_adj(exit_info);
  UINT64 frame_len = Frame_Len;
  TN *incr = NULL;

  /* Trace the sequence before we modify it.
   */
  if (Trace_EE) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile,
	    "\n%s<calls> Adjusting exit for %s (BB:%d)\n",
            DBar, ST_name(pu_st), BB_id(bb));
    fprintf(TFile, "\nFinal frame size: %llu (0x%llx)\n", frame_len, frame_len);
    fprintf(TFile, "\nOld stack frame de-allocation:\n");
    Print_OP_No_SrcLine(sp_adj);
  }

  /* Get the operand that is the frame size increment.
   */
  if ( !Gen_Frame_Pointer || PUSH_FRAME_POINTER_ON_STACK) {
    incr = OP_opnd(sp_adj, OP_find_opnd_use(sp_adj, OU_opnd2));
  }

  /* We make assumptions about what we generated in Generate_Exit.
   * Try to make sure our assumptions were right:
   *   TOP_spadjust $sp, $sp, Frame_Len_TN
   * or
   *   <copy> $sp, ...
   */
  if (Gen_Frame_Pointer && !PUSH_FRAME_POINTER_ON_STACK) {
    FmtAssert(OP_copy(sp_adj) &&
	      TN_is_sp_reg(OP_result(sp_adj,0)),
	      ("Unexpected exit SP adjust OP"));
  } else {
    FmtAssert(   OP_code(sp_adj) == TOP_spadjust 
	      && OP_results(sp_adj) == 1
	      && TN_is_sp_reg(OP_result(sp_adj,0))
	      && TN_is_sp_reg(OP_opnd(sp_adj,
		OP_find_opnd_use(sp_adj, OU_opnd1)))
	      && incr == Frame_Len_TN
	      && ( ! OP_has_predicate(sp_adj) 
		  || OP_opnd(sp_adj, OP_PREDICATE_OPND) == True_TN), 
	      ("Unexpected form of exit SP-adjust OP"));
  }

#ifdef TARG_X8664
  if (CG_push_pop_int_saved_regs && ! Gen_Frame_Pointer) {
    OPS popops = OPS_EMPTY;
    for (INT i = Saved_Callee_Saved_Regs.Elements()-1; i >= 0; i--) {
      SAVE_REG_LOC sr = Saved_Callee_Saved_Regs.Top_nth(i);
      if (sr.temp != NULL)
	continue;
      Build_OP(Is_Target_64bit() ? TOP_popq : TOP_popl, sr.ded_tn, SP_TN, SP_TN, &popops);
    }
    BB_Insert_Ops_After(bb, sp_adj, &popops);
  }
#endif

  /* Perform any adjustments. We will either remove the adjustment
   * or leave it unchanged.
   */
  if (Gen_Frame_Pointer && PUSH_FRAME_POINTER_ON_STACK) {
    OP* op = EETARG_High_Level_Procedure_Exit ();
#ifdef KEY // bug 3600
    OP_srcpos(op) = OP_srcpos(sp_adj);
#endif
#ifdef TARG_X8664
    if (W2OPS_Pragma_Preamble_End_Seen())
      Set_OP_first_after_preamble_end(op);
#endif
    BB_Insert_Op_After (bb, sp_adj, op);
    BB_Remove_Op (bb, sp_adj);
    sp_adj = op;
  } else if (Gen_Frame_Pointer) {
    if ( Trace_EE ) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "\nNew stack frame de-allocation:\n"
		     "-- unchanged --\n");
    }
  } else if (frame_len == 0) {
    BB_Remove_Op(bb, sp_adj);
    sp_adj = NULL;

    if (Trace_EE) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "\nNew stack frame de-allocation:\n"
		     "-- removed --\n");
    }
  } else if (CGTARG_Can_Fit_Immediate_In_Add_Instruction (frame_len)) {
    OPS ops = OPS_EMPTY;
    OP *op;

    /* Replace the SP adjust placeholder with the new adjustment OP
     */
#if defined(TARG_PR)
    BOOL isAdd = FALSE;
    Expand_SR_Adj(isAdd, SP_TN, incr, &ops);
#else
    Exp_ADD (Pointer_Mtype, SP_TN, SP_TN, incr, &ops);
#endif
    BB_Insert_Ops_Before(bb, sp_adj, &ops);
    BB_Remove_Op(bb, sp_adj);
    FOR_ALL_OPS_OPs_FWD(&ops, op) OP_srcpos(op) = OP_srcpos(sp_adj);
    sp_adj = OPS_last(&ops);
    
#if !defined(TARG_PPC32) // local schedular error
    Set_OP_no_move_before_gra(sp_adj);
#endif

    if ( Trace_EE ) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "\nNew stack frame de-allocation:\n");
      FOR_ALL_OPS_OPs_FWD(&ops, op) Print_OP_No_SrcLine(op);
    }
  } else {
    FmtAssert(FALSE, ("Can't handle stack de-allocation of 0x%llx", frame_len));
  }

  /* Point to the [possibly] new SP adjust OP
   */
  EXITINFO_sp_adj(exit_info) = sp_adj;
#if defined(TARG_PPC32)
  EETARG_Fixup_Exit_Code(bb);
#endif
}

static void
Adjust_Alloca_Code (void)
{
  BB *bb;
  OP *op;
  OPS ops;
  OP *new_op;
  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
	// When we remove op and insert a new one,
	// that confuses the BB_OPs iterator.
	// So iterate by hand instead.
	op = BB_first_op(bb);
	while (op != NULL) {
		if (OP_code(op) != TOP_spadjust) {
			op = OP_next(op);
			continue;
		}
  		OPS_Init(&ops);
#if defined(TARG_IA64) || defined(TARG_LOONGSON)
		if (OP_spadjust_plus(op)) {
#else
		if (OP_variant(op) == V_ADJUST_PLUS) {
#endif
        		// dealloca does copy of kid to $sp 
			// (op1 is old sp value)
        		Exp_COPY (OP_result(op,0), 
			  	OP_opnd(op, OP_find_opnd_use(op, OU_opnd2)),
			  	&ops);
		}
#if defined(TARG_IA64) || defined(TARG_LOONGSON)
		else if (OP_spadjust_minus(op)) {
#else
                else if (OP_variant(op) == V_ADJUST_MINUS) {
#endif
    			Exp_SUB (Pointer_Mtype, OP_result(op,0),
			  	OP_opnd(op, OP_find_opnd_use(op, OU_opnd1)),
			  	OP_opnd(op, OP_find_opnd_use(op, OU_opnd2)),
				&ops);
		}
		else {
			FmtAssert(FALSE, ("non-alloca spadjust"));
		}
		FOR_ALL_OPS_OPs_FWD(&ops, new_op) {
			OP_srcpos(new_op) = OP_srcpos(op);
			Is_True(OP_has_predicate(new_op) == OP_has_predicate(op),
				("spadjust can't copy predicates"));
			// copy predicate to new copy/sub ops
			if (OP_has_predicate(new_op))
				Set_OP_opnd (new_op, OP_PREDICATE_OPND,
					OP_opnd(op, OP_PREDICATE_OPND) );
        	}
		BB_Insert_Ops_Before(bb, op, &ops);
    		BB_Remove_Op(bb, op);
		op = OPS_last(&ops);
#if defined(TARG_IA64) || defined(TARG_LOONGSON)
		Reset_BB_scheduled(bb);
#endif
	}
  }
}


/* ====================================================================
 *
 * Adjust_Entry_Exit_Code
 *
 * Adjust entry and exit code for the current PU.  This involves
 * calling Adjust_Entry for each entry BB, and Adjust_Exit
 * for each exit BB.
 *
 * ====================================================================
 */
void
Adjust_Entry_Exit_Code( ST *pu )
{
  BB_LIST *elist;

  for (elist = Entry_BB_Head; elist; elist = BB_LIST_rest(elist)) {
    Adjust_Entry(BB_LIST_first(elist));
  }

  for (elist = Exit_BB_Head; elist; elist = BB_LIST_rest(elist)) {
    Adjust_Exit(pu, BB_LIST_first(elist));
  }

  FmtAssert (!(GP_Setup_Code == no_code && PU_References_GP),
		("had gp reference without setting up new gp"));

  if (PU_has_alloca(Get_Current_PU())) {
	// replace spadjusts with "real" code.
	Adjust_Alloca_Code ();
  }
}

#ifdef KEY
// See the interface description

BOOL Is_Unique_Callee_Saved_Reg (TN * new_tn)
{
  for (INT i=0; i<Saved_Callee_Saved_Regs.Elements(); i++)
    if (TNs_Are_Equivalent (Saved_Callee_Saved_Regs.Top_nth(i).ded_tn,
                            new_tn))
      return FALSE;

  return TRUE;
}

INT Cgdwarf_Num_Callee_Saved_Regs (void)
{
  return Saved_Callee_Saved_Regs.Elements();
}

struct tn* Cgdwarf_Nth_Callee_Saved_Reg (INT n)
{
  return Saved_Callee_Saved_Regs.Top_nth(n).ded_tn;
}

ST* Cgdwarf_Nth_Callee_Saved_Reg_Location (INT n)
{
  return Saved_Callee_Saved_Regs.Top_nth(n).temp;
}
#endif

#ifdef TARG_X8664
INT Push_Pop_Int_Saved_Regs(void)
{
  INT size = 0;
  if (CG_push_pop_int_saved_regs && ! Gen_Frame_Pointer) {
    for (INT i=0; i<Saved_Callee_Saved_Regs.Elements(); i++) {
      SAVE_REG_LOC sr = Saved_Callee_Saved_Regs.Top_nth(i);
      if (sr.temp == NULL)
	size++;
    }
  }
  return size;
}
#endif


#ifdef TARG_IA64
/*
 *==============================================================
 * The following routines are used to realieze dynamical 
 *		cycle counting
 *==============================================================
 */


/*
 * ====================================================================
	Instrument code at the pu head to call mcount so that the target binary can
	generate data for gprof.
 * ====================================================================
 */
#ifdef TARG_IA64
void
Instru_Call_Mcount(void)
{
  BB_LIST *elist;

  for (elist = Entry_BB_Head; elist; elist = BB_LIST_rest(elist)) {
    EETARG_Call_Mcount(BB_LIST_first(elist));
  }
}
#endif


extern void Handle_All_Hazards(BB *bb);
INT R32;		/* first stack register */
REGISTER	reg_value_alloc1;
REGISTER	reg_value_alloc2;
#define AFTER TRUE 	/* after bb */
#define BEFORE FALSE 	/* before bb */
#define Set_TN_reg_value(tn, value)  (tn->u1.reg_tn.class_reg.class_reg.reg = value)
#define Cp_TN_reg_value(tn1, tn2)  (tn1->u1.reg_tn.class_reg.class_reg.reg = tn2->u1.reg_tn.class_reg.class_reg.reg)
#define TN_reg_value(tn) tn->u1.reg_tn.class_reg.class_reg.reg
#define TN_immd_value(tn) tn->u1.value
#define TO_SAVE_REGISTER	32
#define PU_IN_ONE_FILE		1000
#define BB_IN_ONE_PU		1000

typedef struct bb_symbol {
	mBB_NUM		id;
	char * 		name;
	struct bb_symbol * 	next;
	ST *		global_BB_st;
	TN* 		BB_var_name_tn;
} BB_symbol;
BB_symbol *BB_Symbol_Head;
BB_symbol *BB_Symbol_Point;

/*
 * Insert an ld8 r=[r] instruction after bb
 */
void Ld8_r_r(BB *new_bb, TN * src)
{
	OP *Ld8_Op;
	TN *Enum_Op1; 
	TN *Enum_Op2; 

	/* ld8 r=[r] */

	Enum_Op1 = Gen_Enum_TN(ECV_ldtype);
	Enum_Op2 = Gen_Enum_TN(ECV_ldhint);
	Ld8_Op = Mk_OP(TOP_ld8, src, True_TN, Enum_Op1, Enum_Op2, src);
  	BB_Append_Op(new_bb, Ld8_Op);
}

/*
 * Insert an st8.spill r1=[r2] instruction after bb
 */
void St8_spill_r(BB *new_bb, TN *src, TN *base)
{
	OP *St8_Op;
	TN *Enum_Op2;

	/* st8.spill [src]=base */

	Enum_Op2 = Gen_Enum_TN(ECV_sthint);
	St8_Op = Mk_OP(TOP_st8_spill, True_TN, Enum_Op2, src, base);
  	BB_Append_Op(new_bb, St8_Op);
}

/*
 * Insert an ld.fill r1=[r2] instruction after bb
 */
void Ld8_fill_r(BB *new_bb, TN *src, TN *base)
{
	OP *Ld8_Op;
	TN *Enum_Op2;

	/* ld8.fill base = [src] */

	Enum_Op2 = Gen_Enum_TN(ECV_ldhint);
	Ld8_Op = Mk_OP(TOP_ld8_fill, base, True_TN, Enum_Op2, src);
  	BB_Append_Op(new_bb, Ld8_Op);
}

/*
 * Insert an addl src=base,gp instruction after bb
 */
void Addl(BB *new_bb, TN *src, TN *base)
{
	OP *Addl_Op;

	/* addl src=base, gp */

  	Addl_Op = Mk_OP(TOP_addl, src, True_TN, base, GP_TN);
  	BB_Append_Op(new_bb, Addl_Op);
}

/*
 * Insert the assembly fragment which is used to call output function 
 */
static void Cycle_Output_Func_Insert(BB *bb, int reg_num, TN **spill_tn, TN *gp_tn, TN *pr_tn, TN **rg_tn, int output_reg_num, char *output_func)
{
	BB * new_bb;	
	TY_IDX ty;
	ST *call_st;
  	ST *Atexit_Fun;
	OP *Mv_Op, *Addl_Op, *Ld8_Op, *St8_Op, *Mov_Op, *Br_Call_Op;
	TN *var_name_tn;
	TN *src, *base, *target;
	TN *Enum_Op1, *Enum_Op2;
	INT reg_value;
  	TN *ar_ec = Build_Dedicated_TN( ISA_REGISTER_CLASS_application, (REGISTER)(REGISTER_MIN + 66), 8); 
	INT i;

	reg_value = R32 + reg_num + 1;
	new_bb = Gen_And_Insert_BB_After(bb);

	/* mov reg_value = pr */

	src = Gen_Register_TN(ISA_REGISTER_CLASS_integer, Pointer_Size);
	Set_TN_reg_value(src, reg_value + 1);
	Mov_Op = Mk_OP(TOP_mov_f_pr, src, True_TN);
  	BB_Append_Op(new_bb, Mov_Op);

	/* addl r=@ltoff(pr_Symbol_Name#), gp */

	src = Gen_Register_TN(ISA_REGISTER_CLASS_integer, Pointer_Size);
	Set_TN_reg_value(src, reg_value);
	Addl(new_bb, src, pr_tn);
	
	/* ld8 reg_value = [reg_value] */

	Ld8_r_r(new_bb, src);

	/* St8.spill src=baes */

	base = Gen_Register_TN(ISA_REGISTER_CLASS_integer, Pointer_Size);
	Set_TN_reg_value(base, reg_value + 1);
	St8_spill_r(new_bb, src, base);

	/* addl r=@ltoff(gp_Symbol_Name#), gp */

	src = Gen_Register_TN(ISA_REGISTER_CLASS_integer, Pointer_Size);
	Set_TN_reg_value(src, reg_value);
	Addl(new_bb, src, gp_tn);
	
	/* ld8 reg_value = [reg_value] */

	Ld8_r_r(new_bb, src);

	/* St8.spill src=baes */

	base = Gen_Register_TN(ISA_REGISTER_CLASS_integer, Pointer_Size);
	Set_TN_reg_value(base, reg_value - output_reg_num - 1);
	St8_spill_r(new_bb, src, base);

	/* mov reg_value - 1 = gp */

	src = Gen_Register_TN(ISA_REGISTER_CLASS_integer, Pointer_Size);
	Set_TN_reg_value(src, reg_value - output_reg_num - 1);
	Mov_Op = Mk_OP(TOP_mov, src, True_TN, GP_TN);
  	BB_Append_Op(new_bb, Mov_Op);

	for (i = 0; i < output_reg_num; i++) {

		/* addl r=@ltoff(Spill_Symbol_Name#), gp */

		src = Gen_Register_TN(ISA_REGISTER_CLASS_integer, Pointer_Size);
		Set_TN_reg_value(src, reg_value);
		Addl(new_bb, src, spill_tn[i]);
	
		Ld8_r_r(new_bb, src);/* ld8 r=[r] */

		/* st8.spill [r] = r1 */

		base = Gen_Register_TN(ISA_REGISTER_CLASS_integer, Pointer_Size);
		Set_TN_reg_value(base, reg_value - output_reg_num + i);
		St8_spill_r(new_bb, src, base);
	}

	for (i = 0; i < TO_SAVE_REGISTER; i++) {
		src = Gen_Register_TN(ISA_REGISTER_CLASS_integer, Pointer_Size);
		Set_TN_reg_value(src, reg_value);
		Addl(new_bb, src, rg_tn[i]);
	
		Ld8_r_r(new_bb, src);
		base = Gen_Register_TN(ISA_REGISTER_CLASS_integer, Pointer_Size);
		Set_TN_reg_value(base, i + 3);
		St8_spill_r(new_bb, src, base);
	}

	/* addl r=@ltoff(@fptr(test#)), gp */

	ty = Make_Function_Type(MTYPE_To_TY(MTYPE_V));
	call_st = Gen_Intrinsic_Function(ty, output_func);
	Clear_PU_no_side_effects(Pu_Table[ST_pu(call_st)]);
	Clear_PU_is_pure(Pu_Table[ST_pu(call_st)]);
	Set_PU_no_delete(Pu_Table[ST_pu(call_st)]);
	INT64 offset = 0;
	INT32 relocs = TN_RELOC_IA_LTOFF_FPTR;
	var_name_tn = Gen_Symbol_TN(call_st, offset, relocs);
	src = Gen_Register_TN(ISA_REGISTER_CLASS_integer, Pointer_Size);
	Set_TN_reg_value(src, reg_value - output_reg_num);
  	Addl_Op = Mk_OP(TOP_addl, src, True_TN, var_name_tn, GP_TN);
  	BB_Append_Op(new_bb, Addl_Op);

	/* ld8 r=[r] */

	Enum_Op1 = Gen_Enum_TN(ECV_ldtype);
	Enum_Op2 = Gen_Enum_TN(ECV_ldhint);
	Ld8_Op = Mk_OP(TOP_ld8, src, True_TN, Enum_Op1, Enum_Op2, src);
  	BB_Append_Op(new_bb, Ld8_Op);

	/* br.call.sptk.many b0=atexit# */

	src = Gen_Register_TN(ISA_REGISTER_CLASS_branch, Pointer_Size);
	Set_TN_reg_value(src, 1);
	ty = Make_Function_Type(MTYPE_To_TY(MTYPE_V));
	Atexit_Fun = Gen_Intrinsic_Function(ty, "atexit");
	Clear_PU_no_side_effects(Pu_Table[ST_pu(Atexit_Fun)]);
	Clear_PU_is_pure(Pu_Table[ST_pu(Atexit_Fun)]);
	Set_PU_no_delete(Pu_Table[ST_pu(Atexit_Fun)]);
	target = Gen_Symbol_TN(Atexit_Fun, 0, 0);
  	Br_Call_Op = Mk_OP (TOP_br_call, src, True_TN, Gen_Enum_TN(ECV_bwh_sptk), Gen_Enum_TN(ECV_ph_many), Gen_Enum_TN(ECV_dh), target, ar_ec);
  	BB_Append_Op(new_bb, Br_Call_Op);

	Handle_All_Hazards(new_bb); /* do with bundle and stop bit */

	/*
  	 * the following instructions are used to restore the first output register in register stack
	 */
	BB * back_bb;
	back_bb = Gen_And_Insert_BB_After(new_bb);

	/* mov gp = reg_value - 1 */

	src = Gen_Register_TN(ISA_REGISTER_CLASS_integer, Pointer_Size);
	Set_TN_reg_value(src, reg_value - output_reg_num - 1);
	Mov_Op = Mk_OP(TOP_mov, GP_TN, True_TN, src);
  	BB_Append_Op(back_bb, Mov_Op);

	/* addl r=@ltoff(gp_Symbol_Name#), gp */

	src = Gen_Register_TN(ISA_REGISTER_CLASS_integer, Pointer_Size);
	Set_TN_reg_value(src, reg_value);
	Addl(back_bb, src, gp_tn);
	
	/* ld8 reg_value = [reg_value] */

	Ld8_r_r(back_bb, src);

	/* Ld8.spill src=base */

	base = Gen_Register_TN(ISA_REGISTER_CLASS_integer, Pointer_Size);
	Set_TN_reg_value(base, reg_value - output_reg_num - 1);
	Ld8_fill_r(back_bb, src, base);

	/* addl r=@ltoff(pr_Symbol_Name#), gp */

	src = Gen_Register_TN(ISA_REGISTER_CLASS_integer, Pointer_Size);
	Set_TN_reg_value(src, reg_value);
	Addl(back_bb, src, pr_tn);
	
	/* ld8 reg_value = [reg_value] */

	Ld8_r_r(back_bb, src);

	/* St8.spill src=baes */

	base = Gen_Register_TN(ISA_REGISTER_CLASS_integer, Pointer_Size);
	Set_TN_reg_value(base, reg_value + 1);
	Ld8_fill_r(back_bb, src, base);

	/* mov reg_value = pr */

	src = Gen_Register_TN(ISA_REGISTER_CLASS_integer, Pointer_Size);
	Set_TN_reg_value(src, reg_value + 1);
	Mov_Op = Mk_OP(TOP_mov_t_pr, True_TN, src, Gen_Literal_TN(-1, 8));
  	BB_Append_Op(back_bb, Mov_Op);

	/* addl r=@ltoff(Spill_Symbol_Name#), gp */

	for (i = 0; i < output_reg_num; i++) {
		src = Gen_Register_TN(ISA_REGISTER_CLASS_integer, Pointer_Size);
		Set_TN_reg_value(src, reg_value);
		Addl(back_bb, src, spill_tn[i]);

		Ld8_r_r(back_bb, src);	/* ld8 r=[r] */

		/* ld8 r1 = [r] */
		base = Gen_Register_TN(ISA_REGISTER_CLASS_integer, Pointer_Size);
		Set_TN_reg_value(base, reg_value - output_reg_num + i);
		Ld8_fill_r(back_bb, src, base);
	}

	/* addl r=@ltoff(Register_Symbol_Name#), gp */

	for (i = 0; i < TO_SAVE_REGISTER; i++) {
		src = Gen_Register_TN(ISA_REGISTER_CLASS_integer, Pointer_Size);
		Set_TN_reg_value(src, reg_value);
		Addl(back_bb, src, rg_tn[i]);

		Ld8_r_r(back_bb, src);	

		base = Gen_Register_TN(ISA_REGISTER_CLASS_integer, Pointer_Size);
		Set_TN_reg_value(base, 3 + i);
		Ld8_fill_r(back_bb, src, base);
	}

	Handle_All_Hazards(back_bb); 	/* do with bundle and stop bit */
}

REGISTER Get_Value_Caller_GP(BB *bb)
{
	OP *op;
	REGISTER reg_value;

	FOR_ALL_BB_OPs_FWD(bb, op) {
		if ((op->opr == TOP_mov) && (op->res_opnd[2]->u1.reg_tn.class_reg.class_reg.reg == 2)){	
			reg_value = op->res_opnd[1]->u1.reg_tn.class_reg.class_reg.reg;	
			return reg_value;
		}
	}	
	return 0;
}

/*
 * Insert the cycle count assembly fragment into bbs
 */
static void Cycle_Count_Frag_Insert(BB *bb, INT reg_num, TN * var_name_tn, BOOL place, INT bb_cycle)
{
	BB * new_bb;
	INT reg_value;
	TN *Enum_Op1, *Enum_Op2;
	TN *src, *base;
	OP *Addl_Op, *Ld8_Op, *Ld4_Op, *Adds_Op, *St4_Op, *Nop_Op, *Mov_Op;
	OP *Nop_op;
	REGISTER caller_gp_reg;

	reg_value = R32 + reg_num + 1;

	/* Insert a new bb after the bb */
  	FmtAssert (bb != NULL, ("Null bb during insert frag ins in cycle counting"));
	if (place)
		new_bb = Gen_And_Insert_BB_After(bb);
	else
		new_bb = Gen_And_Insert_BB_Before(bb);

	/* GP = Caller_GP_TN */

	if (!(Caller_GP_TN == NULL) && !(TN_reg_value(Caller_GP_TN) == 0) && BB_call(BB_prev(new_bb))) {
		src = Gen_Register_TN(ISA_REGISTER_CLASS_integer, Pointer_Size);
		caller_gp_reg = Get_Value_Caller_GP(BB_next(new_bb));
		if (caller_gp_reg)
			Set_TN_reg_value(src, caller_gp_reg);
		else
			Cp_TN_reg_value(src, Caller_GP_TN);
		Mov_Op = Mk_OP(TOP_mov, GP_TN, True_TN, src);
  		BB_Append_Op(new_bb, Mov_Op);
	}
	
	/* The following seems do nothing , But it keep dependence violation away */
	src = Gen_Register_TN(ISA_REGISTER_CLASS_integer, Pointer_Size);
	Set_TN_reg_value(src, reg_value_alloc1);
	base = Gen_Register_TN(ISA_REGISTER_CLASS_integer, Pointer_Size);
	Set_TN_reg_value(base, reg_value_alloc2);
	Mov_Op = Mk_OP(TOP_mov, src, True_TN, base);
  	BB_Append_Op(new_bb, Mov_Op);
	Mov_Op = Mk_OP(TOP_mov, base, True_TN, src);
  	BB_Append_Op(new_bb, Mov_Op);

  	Nop_op = Mk_OP (TOP_nop_m, True_TN, Gen_Literal_TN(0, 4));
  	BB_Append_Op(new_bb, Nop_op);

	/* addl r=@ltoff(PU_Symbol_Name#), gp */

	src = Gen_Register_TN(ISA_REGISTER_CLASS_integer, Pointer_Size);
	Set_TN_reg_value(src, reg_value_alloc1);
  	Addl_Op = Mk_OP(TOP_addl, src, True_TN, var_name_tn, GP_TN);
  	BB_Append_Op(new_bb, Addl_Op);

  	Nop_op = Mk_OP (TOP_nop_m, True_TN, Gen_Literal_TN(0, 4));
  	BB_Append_Op(new_bb, Nop_op);

	/*ld8 r=[r] */

	Enum_Op1 = Gen_Enum_TN(ECV_ldtype);
	Enum_Op2 = Gen_Enum_TN(ECV_ldhint);
	Ld8_Op = Mk_OP(TOP_ld8, src, True_TN, Enum_Op1, Enum_Op2, src);
  	BB_Append_Op(new_bb, Ld8_Op);
	
  	Nop_op = Mk_OP (TOP_nop_m, True_TN, Gen_Literal_TN(0, 4));
  	BB_Append_Op(new_bb, Nop_op);

	/*ld4 r1 = [r] */

	base = Gen_Register_TN(ISA_REGISTER_CLASS_integer, Pointer_Size);
	Set_TN_reg_value(base, reg_value_alloc2);
	Ld4_Op = Mk_OP(TOP_ld4, base, True_TN, Enum_Op1, Enum_Op2, src);
  	BB_Append_Op(new_bb, Ld4_Op);

  	Nop_op = Mk_OP (TOP_nop_m, True_TN, Gen_Literal_TN(0, 4));
  	BB_Append_Op(new_bb, Nop_op);

	/* adds r1=bb_cycle, r1 */

  	Adds_Op = Mk_OP (TOP_adds, base, True_TN, Gen_Literal_TN(bb_cycle, 4), base);
  	BB_Append_Op(new_bb, Adds_Op);

	/* st4 [src] = base */

	Enum_Op1 = Gen_Enum_TN(ECV_sttype);
	Enum_Op2 = Gen_Enum_TN(ECV_sthint);
	St4_Op = Mk_OP(TOP_st4, True_TN, Enum_Op1, Enum_Op2, src, base);
  	BB_Append_Op(new_bb, St4_Op);

	Handle_All_Hazards(new_bb); 	/* do with bundle and stop bit */
}
/*
 * retarget the branch instructions
 */
void Branch_Retarget(BB *bb)
{
	BB *branch_bb;
	BB *temp_bb;
	OP *br;
	BBLIST *list;
	
	for (list = bb->preds; list != NULL; list = BBLIST_next(list)) {
		temp_bb = list->item;
		BB_Retarget_Branch(temp_bb, bb, bb->prev);
	}
}

/*
 * Insert 'nop' OP before bb
 */
void Prepend_Nop_Op(BB *bb, TOP opr)
{
	OP *op;

  	op = Mk_OP (opr, True_TN, Gen_Literal_TN(0, 4));
  	BB_Prepend_Op(bb, op);
}

/*
 * Insert 'alloc' instruction before the bb which is entry && exit bb, 
 * and it don't use alloc in it's entry. So we need to insert one 'alloc'
 * instruction to get the tmp registers which we need to caculate the cycles
 */
BB *Alloc_Inst_Insert_Before(BB *bb)
{
	OP *Alloc_Op, *Last_Op, *Mov_Op;
	BB *new_bb, *Last_bb;
	TN *saved_pfs;
	REGISTER reg_value;
	
  	FmtAssert(bb != NULL, ("NULL input bb in Alloc_Inst_Insert_Before"));
	new_bb = Gen_And_Insert_BB_Before(bb);
  	FmtAssert(new_bb != NULL, ("Failed to get new bb in Alloc_Inst_Insert_Before"));
	BB_Copy_Annotations(new_bb, bb, ANNOT_ENTRYINFO);
	REGION_First_BB = new_bb;

	/* create new alloc instruction and insert it in new bb*/
	reg_value = REGISTER_Request_Stacked_Register(ABI_PROPERTY_caller, ISA_REGISTER_CLASS_integer);
	saved_pfs = Gen_Register_TN(ISA_REGISTER_CLASS_integer, Pointer_Size);
	Set_TN_reg_value(saved_pfs, reg_value);
	Alloc_Op = Mk_OP(TOP_alloc, saved_pfs, Gen_Literal_TN(0, 4), Gen_Literal_TN(3, 4), Gen_Literal_TN(0, 4), Gen_Literal_TN(0, 4));
	Prepend_Nop_Op(new_bb, TOP_nop_i);
	Prepend_Nop_Op(new_bb, TOP_nop_m);
	BB_Prepend_Op(new_bb, Alloc_Op);

	/* do with property and list */
	new_bb->flags =  new_bb->flags | BBM_ENTRY;
	bb->flags =  bb->flags & ~BBM_ENTRY;
	Entry_BB_Head = BB_LIST_Delete(bb, Entry_BB_Head);
	Entry_BB_Head = BB_LIST_Push(new_bb, Entry_BB_Head, &MEM_pu_pool);

	Handle_All_Hazards(new_bb); /*do with stop bits and bundles */
	
	if (BB_call(bb) || (BB_branch_op(bb))) { /* it must be an branch bb, since it's the only one bb in the pu */
		Split_BB_For_br(bb);
		Last_bb = bb->next;
	} else {
  		FmtAssert(0, ("How can we see a pu that have no branch inst to finish itself "));
	}

	Mov_Op = Mk_OP(TOP_mov_t_ar_r_i, Pfs_TN, True_TN, saved_pfs);
	BB_Prepend_Op(Last_bb, Mov_Op);

	Handle_All_Hazards(Last_bb); 

	return new_bb;
}

/*
 * Split the entry BB: Insert a new BB before origional BB and put the origional
 * 'alloc' instruction in the new BB. 
 */
BB *Split_Entry_BB(BB *bb)
{
	BB *new_bb;
	OP *op, *temp_op;

	/* create new bb */
	new_bb = Gen_And_Insert_BB_Before(bb);
        
	Clean_Up(bb);
        for (op = BB_first_op(bb); op != NULL; op = temp_op) {
            temp_op = OP_next(op);
            BB_Remove_Op(bb, op);
            BB_Append_Op(new_bb, op);
            if (OP_code(op) != TOP_alloc) {
                    continue;
            } else {
                    break;
            }
         }
        
         FmtAssert( OP_code(op) == TOP_alloc, ("The BB has no 'alloc' instruction "));

         BB_Copy_Annotations(new_bb, bb, ANNOT_ENTRYINFO); /*copy the annotation of orignal bb about entry information to the new bb, since new bb become the entry bb */
         REGION_First_BB = new_bb;
         Set_BB_entry(new_bb);
         Entry_BB_Head = BB_LIST_Push(new_bb, Entry_BB_Head, &MEM_pu_pool); /* insert the new_bb to the Entry_BB_Head list */

         Entry_BB_Head = BB_LIST_Delete(bb, Entry_BB_Head);
         Reset_BB_entry(bb);
 
         Handle_All_Hazards(bb); /* do with stop bits and bundles with old bb */
         Handle_All_Hazards(new_bb); /* do with stop bits and bundles */

         return new_bb;
}

BOOL Selected_PU(char * PU_Name)
{
	char *str_point;
	char *point;
	char *options;
	int  str_length;
	int  i;

	str_length = strlen(Cycle_String);
	options = (char*)malloc(str_length + 1);
	strncpy(options, Cycle_String, str_length);
	options[str_length] = '\0';

	point = str_point = options;	
	i = 0;

	while (*str_point == '"' || *str_point == ' ' || *str_point == '%'){
		str_point ++;
		point ++;
		i ++;
	}
	while(*str_point != '\0') {
		while (*point != ' ' && *point != '"' && *point != '\0' && *point != '%') {
			point ++;
			i ++;
		}
		*point = '\0';

		if (strcmp(PU_Name, str_point) == 0){
			free(options);
			return TRUE;
		}

		if (i >= str_length)
			break;
		point++;
		i ++;
		str_point=point;
		while(*str_point == '%' && *str_point != '\0') {
			str_point ++;
			point ++;
			i++;
		}
	}
	free(options);
	return FALSE;
}
/*
 * Do instrumentation work, include: 
 *	Global Symbol, Cycle Count instructions , Output result instructions
 */
void Pu_Cycle_Count_Func_Insert( ST* pu, BOOL is_region )
{
	BB_LIST *elist;
  	BOOL gra_run = ! CG_localize_tns;
	BB *bb, *temp_bb;
	OP *op, *br;
	char *PU_Name;
	char *PU_Temp_Name;
	INT num_input, num_local, num_output, num_rotating;
	char *PU_Symbol_Name;
	char *Spill_Symbol_Name[ISA_REGISTER_MAX];
	char *Register_Symbol_Name[TO_SAVE_REGISTER];
	char *GP_Symbol_Name;
	char *PR_Symbol_Name;
	INT i;
	TN   *spill_var_name_tn[ISA_REGISTER_MAX];
	TN   *register_var_name_tn[TO_SAVE_REGISTER];
	TN   *gp_var_name_tn;
	TN   *pr_var_name_tn;
	
	INT max_used_register_stack = 0;
	
	num_input = num_local = num_output = num_rotating = 0;
  	FmtAssert(pu != NULL, ("pu to be null in Pu_Cycle_Count_Func_Insert\n"));
	/*
 	 * suppose there are at most one alloc instruction in each bb
	 * So we first get the information about the stack register using of each BB 
	 */
  	for ( elist = Entry_BB_Head; elist; elist = BB_LIST_rest(elist) ) {
		bb = BB_LIST_first(elist);
		/*
		 * I am not still sure if the alloc instruction will be the first instrcution 
		 * in a PU in ordinary program. So the following method will have some bad effect
		 * in certain situation
		 */
		FOR_ALL_BB_OPs_FWD(bb, op) {
			if (OP_code(op) != TOP_alloc) 
				continue;
			num_input = TN_immd_value(op->res_opnd[0]);
			num_local = TN_immd_value(op->res_opnd[1]);
			num_output = TN_immd_value(op->res_opnd[2]);
			num_rotating = TN_immd_value(op->res_opnd[3]);
			max_used_register_stack	= num_input + num_local + num_output;
			/* to get to output register to be used by us */
			Set_OP_opnd(op, 2, Gen_Literal_TN(num_output + 2, 4));
		}
  	}

        /* alloc two free register for stack registers */
	reg_value_alloc1 = REGISTER_Request_Stacked_Register(ABI_PROPERTY_caller, ISA_REGISTER_CLASS_integer);
	reg_value_alloc2 = REGISTER_Request_Stacked_Register(ABI_PROPERTY_caller, ISA_REGISTER_CLASS_integer);
	/* generate global symbol for count cycle */
	static MEM_POOL global_symbol_pool;

	PU_Name = ST_name(pu);
	MEM_POOL_Initialize(&global_symbol_pool, "instrumentation global symbol pool", FALSE);
	MEM_POOL_Push(&global_symbol_pool);

	PU_Symbol_Name = TYPE_MEM_POOL_ALLOC_N(char, &global_symbol_pool, sizeof(char) * (strlen(Src_File_Name) + strlen(PU_Name) + strlen("Cycle_Symbol") + 3));
	sprintf(PU_Symbol_Name, "%s_%s_%s", Src_File_Name, PU_Name, "Cycle_Symbol");
	while((PU_Temp_Name = strchr(PU_Symbol_Name, '.')) != NULL || (PU_Temp_Name = strchr(PU_Symbol_Name, '-')) != NULL)
	{
		*PU_Temp_Name = '_';
	}	

	/* the global symbol used to record the cycle of each PU */
	TY_IDX ty = MTYPE_To_TY(MTYPE_I8);
	ST *global_st = New_ST(GLOBAL_SYMTAB);
	ST_Init(global_st, Save_Str2(PU_Symbol_Name, ""), CLASS_VAR, SCLASS_UGLOBAL, EXPORT_PREEMPTIBLE, ty);
	Allocate_Object(global_st);
	INT64 offset = 0;
	INT32 relocs = TN_RELOC_IA_LTOFF22;
	TN *var_name_tn = Gen_Symbol_TN(global_st, offset, relocs);

	/* the global symbol used to record the cycle of each BB */

	if (Cycle_BB_Enable && Selected_PU(ST_name(pu))) {
		BB_Symbol_Point = BB_Symbol_Head = (BB_symbol *)malloc(sizeof(BB_symbol));
		BB_Symbol_Head->next = NULL;
		for (bb = REGION_First_BB; bb != NULL; bb = temp_bb) {
			BB_symbol *BB_Symbol_Name;
			char bb_id[10];
	
			temp_bb = BB_next(bb);
			br= BB_branch_op(bb);
			if (!BB_call(bb) && !br && !BB_exit(bb) && BB_length(bb)) { /* do with ordinary BB */ 
				sprintf(bb_id, "%d", bb->id);
				BB_Symbol_Point->name = (char *)malloc(sizeof(char)*(strlen(bb_id) + strlen(PU_Name) + 2));
				sprintf(BB_Symbol_Point->name, "%s_%d", PU_Name, bb->id);
				BB_Symbol_Name = (BB_symbol *)malloc(sizeof(BB_symbol));
				BB_Symbol_Point->id = bb->id;
				BB_Symbol_Point->next = BB_Symbol_Name;
				BB_Symbol_Name->next = NULL;
				BB_Symbol_Point = BB_Symbol_Name;
			}
		}

		/* symbol used to store the bb cycle */
		TY_IDX BB_ty = MTYPE_To_TY(MTYPE_I8);
		BB_Symbol_Point = BB_Symbol_Head;
		do {
			BB_Symbol_Point->global_BB_st = New_ST(GLOBAL_SYMTAB);
			ST_Init(BB_Symbol_Point->global_BB_st, Save_Str2(BB_Symbol_Point->name, ""), CLASS_VAR, SCLASS_UGLOBAL, EXPORT_PREEMPTIBLE, BB_ty);
			Allocate_Object(BB_Symbol_Point->global_BB_st);
 			BB_Symbol_Point->BB_var_name_tn = Gen_Symbol_TN(BB_Symbol_Point->global_BB_st, offset, relocs);
			BB_Symbol_Point = BB_Symbol_Point->next;
		} while (BB_Symbol_Point->next != NULL);
	}

      	if (strcmp(PU_Name, "main") == 0) {
		GP_Symbol_Name = TYPE_MEM_POOL_ALLOC_N(char, &global_symbol_pool, sizeof(char) * (strlen(PU_Name) + strlen("GP_Symbol") + 2));
		PR_Symbol_Name = TYPE_MEM_POOL_ALLOC_N(char, &global_symbol_pool, sizeof(char) * (strlen(PU_Name) + strlen("PR_Symbol") + 2));

		sprintf(GP_Symbol_Name, "%s_%s", PU_Name, "GP_Symbol");
		sprintf(PR_Symbol_Name, "%s_%s", PU_Name, "PR_Symbol");

		for (i = 0; i < num_output ; i++) {
			if (i > 9)
				Spill_Symbol_Name[i] = TYPE_MEM_POOL_ALLOC_N(char, &global_symbol_pool, sizeof(char) * (strlen(PU_Name) + strlen("Spill_Symbol") + 5));
			else
				Spill_Symbol_Name[i] = TYPE_MEM_POOL_ALLOC_N(char, &global_symbol_pool, sizeof(char) * (strlen(PU_Name) + strlen("Spill_Symbol") + 4));
			sprintf(Spill_Symbol_Name[i], "%s_%s_%d", PU_Name, "Spill_Symbol", i);
		}
	
		/* Registers need to be saved during instrumentation */
	
		for (i = 0; i < TO_SAVE_REGISTER; i++) {
			if (i > 9)
				Register_Symbol_Name[i] = TYPE_MEM_POOL_ALLOC_N(char, &global_symbol_pool, sizeof(char) * (strlen(PU_Name) + strlen("Register_Symbol") + 5));
			else
				Register_Symbol_Name[i] = TYPE_MEM_POOL_ALLOC_N(char, &global_symbol_pool, sizeof(char) * (strlen(PU_Name) + strlen("Register_Symbol") + 4));
			sprintf(Register_Symbol_Name[i], "%s_%s_%d", PU_Name, "Register_Symbol", i);
		}
	
		/* 
		 *create the global symbols which will be used during cycle couting 
		 */
	
		/* data symbol used to store the first output reg */
		TY_IDX spill_ty = MTYPE_To_TY(MTYPE_I8);
		for (i = 0; i < num_output ; i++) {
			ST* global_spill_st = New_ST(GLOBAL_SYMTAB);
			ST_Init(global_spill_st, Save_Str2(Spill_Symbol_Name[i], ""), CLASS_VAR, SCLASS_UGLOBAL, EXPORT_PREEMPTIBLE, spill_ty);
			Allocate_Object(global_spill_st);
 			spill_var_name_tn[i] = Gen_Symbol_TN(global_spill_st, offset, relocs);
		}
	
		/* data symbol used to store the r2~r31 */
		TY_IDX register_ty = MTYPE_To_TY(MTYPE_I8);
		for (i = 0; i < TO_SAVE_REGISTER ; i++) {
			ST* global_register_st = New_ST(GLOBAL_SYMTAB);
			ST_Init(global_register_st, Save_Str2(Register_Symbol_Name[i], ""), CLASS_VAR, SCLASS_UGLOBAL, EXPORT_PREEMPTIBLE, register_ty);
			Allocate_Object(global_register_st);
 			register_var_name_tn[i] = Gen_Symbol_TN(global_register_st, offset, relocs);
		}
		
		/* data symbol use to store GP */
		TY_IDX gp_ty = MTYPE_To_TY(MTYPE_I8);
		ST* gp_st = New_ST(GLOBAL_SYMTAB);
		ST_Init(gp_st, Save_Str2(GP_Symbol_Name, ""), CLASS_VAR, SCLASS_UGLOBAL, EXPORT_PREEMPTIBLE, gp_ty);
		Allocate_Object(gp_st);
		gp_var_name_tn = Gen_Symbol_TN(gp_st, offset, relocs);
	
		/* data symbol use to store Pr */
		TY_IDX pr_ty = MTYPE_To_TY(MTYPE_I8);
		ST* pr_st = New_ST(GLOBAL_SYMTAB);
		ST_Init(pr_st, Save_Str2(PR_Symbol_Name, ""), CLASS_VAR, SCLASS_UGLOBAL, EXPORT_PREEMPTIBLE, pr_ty);
		Allocate_Object(pr_st);
		pr_var_name_tn = Gen_Symbol_TN(pr_st, offset, relocs);
	}

	/* 
	 *output cycle counting global symbol into pu_output.h
	 */
	fprintf(Output_h_File, "extern long int %s;\n", PU_Symbol_Name);
	pu_string[pu_number] = (char *)malloc(strlen(PU_Symbol_Name) + 1);
	sprintf(pu_string[pu_number], "%s", PU_Symbol_Name);
	pu_number ++;

	if (pu_number > PU_IN_ONE_FILE)
    		FmtAssert(FALSE, ("Too many pu in one file, Can't to handle file in which the number of pu great than 1000"));

	if (Cycle_BB_Enable && Selected_PU(ST_name(pu))) {
		BB_Symbol_Point = BB_Symbol_Head;
		while (BB_Symbol_Point->next != NULL) {
			fprintf(Output_h_File, "extern long int %s;\n", BB_Symbol_Point->name);
			bb_string[bb_number] = (char *)malloc(strlen(BB_Symbol_Point->name) + 1);
			sprintf(bb_string[bb_number], "%s", BB_Symbol_Point->name);
			bb_number ++;
			BB_Symbol_Point = BB_Symbol_Point->next;
	
			if (bb_number > BB_IN_ONE_PU) 
    				FmtAssert(FALSE, ("Can't handle the PU in which the number of bbs is larege than 1000"));
		}
	}

	MEM_POOL_Pop(&global_symbol_pool);
	MEM_POOL_Delete(&global_symbol_pool);

	/*
	 * cycle count instrumentation 
	 */

	for (bb = REGION_First_BB; bb != NULL; bb = temp_bb) {
		temp_bb = BB_next(bb);
		br= BB_branch_op(bb);
		if (!BB_call(bb) && !br && !BB_exit(bb) && BB_length(bb)) { /* do with ordinary BB */ 
    			Cycle_Count_Frag_Insert (bb, max_used_register_stack, var_name_tn, AFTER, BB_cycle(bb));
			if (Cycle_BB_Enable && Selected_PU(ST_name(pu))) {
				BB_Symbol_Point = BB_Symbol_Head;
				while (BB_Symbol_Point->next != NULL && BB_Symbol_Point->id != bb->id) {
					BB_Symbol_Point = BB_Symbol_Point->next;
				}
				if (BB_Symbol_Point != NULL)
    					Cycle_Count_Frag_Insert (bb->next, max_used_register_stack, BB_Symbol_Point->BB_var_name_tn, AFTER, BB_cycle(bb));
			}
		}
		if (BB_exit(bb) && (!BB_entry(bb)) && BB_length(bb)) {  /* do with exit BB , but not entry BB at the same time */
   			Cycle_Count_Frag_Insert (bb, max_used_register_stack, var_name_tn, BEFORE, BB_cycle(bb));
			Branch_Retarget(bb); /* Since we insert a cycle count BB before exit BB, so, the original relations between BBs, such as prevs BB jmp to the exit BB, Now we must reshcedule these relationship, to maic it jump to the BB we insert, So that , the cycle count will do work */
		}
		/* do with the situation where there is only one  BB in a PU */
		if (BB_exit(bb) && BB_entry(bb) && BB_length(bb)) {
			BOOL alloc_in = FALSE;
			BB *new_bb;
			/* search if there are alloc inst during the bb */
			FOR_ALL_BB_OPs_FWD(bb, op) {
				if (OP_code(op) != TOP_alloc) 
					continue;
				else
					alloc_in = TRUE;
			}
			if (alloc_in) {
				new_bb = Split_Entry_BB(bb);/* Suppose the Alloc inst be the first inst in the BB */
    				Cycle_Count_Frag_Insert (BB_next(new_bb), max_used_register_stack, var_name_tn, BEFORE, BB_cycle(BB_next(new_bb)));
			} else {
				/* Don't have alloc inst in BB at all, Then we need insert alloc inst to get stack register */
				new_bb = Alloc_Inst_Insert_Before(bb);
				max_used_register_stack = 1;/* the reg used to save pfs*/
    				Cycle_Count_Frag_Insert (BB_next(new_bb), max_used_register_stack, var_name_tn, AFTER, BB_cycle(BB_next(new_bb)));
			}
		}
  	}

	/* output function instrumentation */
	/* insert output function call in every entry BB */
      	if (strcmp(ST_name(pu), "main") == 0) {
  		BB_LIST *entry_elist;
  		for ( entry_elist = Entry_BB_Head; entry_elist; 
			entry_elist = BB_LIST_rest(entry_elist) ) {
			Cycle_Output_Func_Insert( BB_LIST_first(entry_elist), max_used_register_stack, spill_var_name_tn, gp_var_name_tn, pr_var_name_tn, register_var_name_tn, num_output , "pu_output");
		}
	}
	if (Cycle_BB_Enable && Selected_PU(ST_name(pu))) {
		while (BB_Symbol_Head != NULL) {
			BB_Symbol_Point = BB_Symbol_Head;
			BB_Symbol_Head = BB_Symbol_Point->next;
			free(BB_Symbol_Point);	
		}
	}
}

/*
 * After split, Make the succs of old bbs to the second bb .
 */
void Change_Succs(BB *bb, BB *last_bb)
{
	BBLIST *list, *last_list, *temp_list, *preds_list;
	BB *temp_bb;

  	FmtAssert(bb != NULL || last_bb != NULL, ("Null BB during the Change_Succs "));
	/* Set the succs of last bb to bb */
	bb->succs = last_bb->succs;
	last_bb->succs = NULL;
	/*
 	 * We also must change the value of the previous bb of the successive bb of the bb
	 */
	for (list = bb->succs; list != NULL ; list = list->next) {
		temp_bb = list->item;
		temp_list = temp_bb->preds;
		last_list = NULL;
		for (preds_list = temp_bb->preds; 
			(temp_list != last_list) && (temp_list != NULL); preds_list = temp_list) {
			last_list = preds_list;
			temp_list = preds_list->next;
			if (preds_list->item == last_bb)
				preds_list->item = bb;
		}
	}
}

/*
 * Check if check inst
 */
int Check_Check(OP *op)
{
	if (op->opr == TOP_chk_s_i || op->opr == TOP_chk_s_m || 
		op->opr == TOP_chk_f_s || op->opr == TOP_chk_a || 
		op->opr == TOP_chk_f_a || op->opr == TOP_chk_s ) 
		return 1;
	return 0;
}

/*
 * Split BB, put 'br' instruction in 
 * a new bb which created to contain it.
 */
void Split_BB_For_br(BB *bb)
{
	BB *new_bb;
	OP *last_op, *nop_op;
	INT op_index = 0;
	OP *br = BB_branch_op(bb);
  	
        FmtAssert(bb != NULL, ("Null bb during the Split_BB_For_br "));
 
	last_op = BB_last_op(bb);
  	FmtAssert(last_op!=NULL, ("To get the last op of the bb failed "));
	if (BB_call(bb)) { /* br.call */
		op_index = 1;
		BB_Remove_Op(bb, last_op);
	}
	if (br) { /* br instructions */
		op_index = 2;
		BB_Remove_Op(bb, br);
		if (OP_noop(last_op)) { /* Last inst be a delay noop inst */
			op_index = 3;
			BB_Remove_Op(bb, last_op);
		}
	}

	/* move br op to new bb, and add other insts to keep the bb comprehensive*/
	new_bb = Gen_And_Insert_BB_After(bb);
	switch(op_index) {
		case 1: /* call instruction , we must set the flags*/
			Set_BB_call(new_bb);
			Reset_BB_call(bb);
		case 2: /* 
 			 * br inst and it's last inst, put 'br' inst into the new bb 
			 * and add nop inst to make bundle comprehensive 
                         */
			Prepend_Nop_Op(new_bb, TOP_nop_i);
			Prepend_Nop_Op(new_bb, TOP_nop_m);
		  	BB_Append_Op(new_bb, last_op);
			if (Check_Check(last_op)) 
  				nop_op = Mk_OP(TOP_nop_i, True_TN, Gen_Literal_TN(0, 4));
			else
  				nop_op = Mk_OP(TOP_nop_b, True_TN, Gen_Literal_TN(0, 4));
			BB_Append_Op(bb, nop_op);
			break;
		case 3: /* br inst , not the last inst */
			BB_Append_Op(new_bb, br);
			BB_Append_Op(new_bb, last_op);
			Prepend_Nop_Op(new_bb, TOP_nop_m);
  			nop_op = Mk_OP(TOP_nop_b, True_TN, Gen_Literal_TN(0, 4));
			BB_Append_Op(bb, nop_op);
			BB_Append_Op(bb, nop_op);
			break;
		case 0: /* not br inst */
		default:	
			FmtAssert(0, ("Fatal Error"));	
			break;
	}

	
	Change_Succs(new_bb, bb); /* Change the succs of bb to new bb */
}

/*
 * Split all the BBs in the pu which have 'br' op, 
 * so that we can insert code after each bb that have no br instruction.
 * By doing so, we can protect the flow the code running. 
 */
void Split_BB()
{
  	BB_LIST *elist;
	BB *bb, *temp_bb;
  	BOOL gra_run = !CG_localize_tns;

 	for (bb = REGION_First_BB; bb != NULL; bb = temp_bb) {
		temp_bb = BB_next(bb); 
		if (!BB_cycle(bb) && BB_rotating_kernel(bb)) {
			ANNOTATION *annot = ANNOT_Get(BB_annotations(bb), ANNOT_ROTATING_KERNEL);
			ROTATING_KERNEL_INFO *info = ANNOT_rotating_kernel(annot);
			INT ii = info->ii;
			BB_cycle(bb) = OP_scycle(BB_last_op(bb))%ii;
		}
		if (BB_call(bb) || (BB_branch_op(bb) && !BB_exit(bb)))
    			Split_BB_For_br(bb);
  	} 
}

/*
 * Do instrucmentation, So that we can caculate 
 * the cycles when the target code running
 */
void Cycle_Count_Initialize ( ST *pu, BOOL is_region )
{
	int reg_index;
        BB  *bb, *temp_bb;
	OP  *op;

  	FmtAssert(pu != NULL, ("Null pu during the initialize "));

	/* get the first stack reg's value */
	for(reg_index = ISA_REGISTER_FIRST; reg_index < ISA_REGISTER_MAX; reg_index ++){
		if (ABI_PROPERTY_Is_stacked(ISA_REGISTER_CLASS_integer, reg_index)) {
			R32 = reg_index;
			break;
		}
	}

  	FmtAssert(R32 != 0, ("Null pu during the initialize "));
	for (bb = REGION_First_BB; bb != NULL; bb = temp_bb) {/* to rebundle those unbundled_aligned bb */
                temp_bb = BB_next(bb);
		Clean_Up(bb);
                Handle_All_Hazards (bb);
        }
  
	Split_BB(); /* split all bb which have br instruction  */
	Pu_Cycle_Count_Func_Insert(pu, is_region); /* do instrumentation */
}
#endif
