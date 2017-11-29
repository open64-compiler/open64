/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
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
 *  Module: localize.c
 *  $Revision: 1.21 $
 *  $Date: 05/12/05 08:59:08-08:00 $
 *  $Author: bos@eng-24.pathscale.com $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.localize.cxx $
 *
 *  Description:
 *  ============
 *
 *  Localize any global TNs.
 *  First we make a pass through the bb's to find any global TNs,
 *  then we insert save/restore code to turn those into local TNs.
 *
 * =======================================================================
 * =======================================================================
 */

#include "defs.h"
#include "tracing.h"
#include "mempool.h"
#include "cgir.h"
#include "tn_map.h"
#include "tn_set.h"
#include "cg_spill.h"
#include "localize.h"
#include "register.h"
#include "bb_set.h"
#include "calls.h"
#include "cgexp.h"
#include "cg_region.h"
#include "cg_internal.h"
#include "targ_sim.h"
#include "whirl2ops.h"
#include "cg.h"
#if defined(TARG_PPC32)
#include <queue>
#include <set>
#endif

#if defined(TARG_SL)
#include <queue>
#include <set>
#endif

static BOOL Trace_Localize = FALSE;

#define Trace_Replace(msg1,old_tn,msg2,new_tn,bb) \
	if ( Trace_Localize) { 					\
		fprintf( TFile, "<loc> replace %s ", msg1);	\
		Print_TN( old_tn, FALSE );			\
		fprintf( TFile, " with %s ", msg2);		\
		Print_TN( new_tn, FALSE );			\
		fprintf(TFile, " in bb %d\n", BB_id(bb));	\
	}


static BOOL
BB_Is_Followed_By_Call (BB *current_bb)
{
	for (BB *bb = current_bb; bb != NULL; bb = BB_next(bb)) {
		if (BB_call(bb))
			return TRUE;
		if (BB_exit(bb))
			return FALSE;
	}
	return FALSE;
}

/*
 * A tn param reg spans multiple BB's.
 * Assume this is the case of a def that is not in the call block.
 * So replace it with a non-dedicated reg
 * and insert a copy of the non-dedicated reg to the dedicated reg
 * just before the call.
 * If bb was split because too large, then could possibly be uses of 
 * param regs (storing formals) before defs for call, all in same bb;
 * in that case, don't change the tns until reach the defs.
 */
static void
Localize_Global_Param_Reg (BB *current_bb, TN *parm_tn)
{
	BB *bb;
	OP *op;
	OPS ops = OPS_EMPTY;
	INT opndnum;
	INT resnum;
	BOOL reached_defs = FALSE;
	TN *new_tn = Dup_TN_Even_If_Dedicated(parm_tn);
	Set_TN_is_global_reg(new_tn);	/* will want to spill the copy */
	Trace_Replace("param",parm_tn, "new",new_tn, current_bb);

	/* search until come to bb after the call.
	 * note that current_bb might be a bb after call. */
	bb = current_bb;
	do {
		Set_BB_has_globals(bb);
		FOR_ALL_BB_OPs_FWD (bb, op) {
			for (resnum = 0; resnum < OP_results(op); resnum++) {
				if (OP_result(op, resnum) == parm_tn) {
					Set_OP_result(op, resnum, new_tn);
					reached_defs = TRUE;
				}
			}
			if (!reached_defs) continue;
			for (opndnum = 0; opndnum < OP_opnds(op); opndnum++) {
				if (OP_opnd(op, opndnum) == parm_tn) {
					/* replace with new tn */
					Set_OP_opnd(op, opndnum, new_tn);
				}
			}
		}
		bb = BB_next(bb);
	} while (bb != NULL && !BB_call(BB_prev(bb)));

	FmtAssert(bb != NULL && BB_call(BB_prev(bb)), 
		("didn't find call BB after bb %d for tn %d", 
		BB_id(current_bb), TN_number(parm_tn)));
	bb = BB_prev(bb);	/* go back to call bb */

	/* Insert copy just before call. */
	Exp_COPY (parm_tn, new_tn, &ops);
	BB_Insert_Ops_Before (bb, BB_last_op(bb), &ops);
}

/*
 * A def of a tn return reg spans multiple BB's.
 * So replace it with a non-dedicated reg
 * and insert a copy of the dedicated global reg to the non-dedicated reg.
 */
static void
Localize_Global_Return_Reg_Def (BB *current_bb, TN *ret_tn)
{
	BB *bb;
	OP *op;
	OPS ops = OPS_EMPTY;
	INT opndnum;
	INT resnum;
	BOOL reached_defs = FALSE;	/* only replace after def reached */
	TN *new_tn = Dup_TN_Even_If_Dedicated(ret_tn);
	Set_TN_is_global_reg(new_tn);	/* will want to spill the copy */
	Trace_Replace("return",ret_tn, "new",new_tn, current_bb);

	/* search until come to exit bb. */
	for (bb = current_bb; bb != NULL && !BB_exit(bb); bb = BB_next(bb)) {
		Set_BB_has_globals(bb);
		FOR_ALL_BB_OPs_FWD (bb, op) {
			for (resnum = 0; resnum < OP_results(op); resnum++) {
				if (OP_result(op, resnum) == ret_tn) {
					Set_OP_result(op, resnum, new_tn);
					reached_defs = TRUE;
				}
			}
			if (!reached_defs) continue;
			for (opndnum = 0; opndnum < OP_opnds(op); opndnum++) {
				if (OP_opnd(op, opndnum) == ret_tn) {
					/* replace with new tn */
					Set_OP_opnd(op, opndnum, new_tn);
				}
			}
		}
	}
	FmtAssert(bb != NULL && BB_exit(bb), ("didn't find exit BB after bb %d for tn %d", BB_id(current_bb), TN_number(ret_tn)));

	/* insert copy at beginning of bb */
	Exp_COPY (ret_tn, new_tn, &ops);
	BB_Prepend_Ops (bb, &ops);
}

/*
 * A tn return reg spans multiple BB's.
 * So replace it with a non-dedicated reg
 * and insert a copy of the dedicated global reg to the non-dedicated reg.
 */
static void
Localize_Global_Return_Reg (BB *current_bb, TN *ret_tn)
{
	BB *bb;
	OP *op;
	OPS ops = OPS_EMPTY;
	INT opndnum;
	INT resnum;
	BOOL reached_use = FALSE;	/* only replace after use reached */
	TN *new_tn = Dup_TN_Even_If_Dedicated(ret_tn);
	Set_TN_is_global_reg(new_tn);	/* will want to spill the copy */
	Trace_Replace ("return", ret_tn, "new", new_tn, current_bb);

	/* search backwards until come to call bb */
	for (bb = current_bb; bb != NULL && !BB_call(bb); bb = BB_prev(bb)) {
		Set_BB_has_globals(bb);
		FOR_ALL_BB_OPs_REV (bb, op) {
			for (opndnum = 0; opndnum < OP_opnds(op); opndnum++) {
				if (OP_opnd(op, opndnum) == ret_tn) {
					/* replace with new tn */
					Set_OP_opnd(op, opndnum, new_tn);
					reached_use = TRUE;
				}
			}
			if (!reached_use) continue;
			for (resnum = 0; resnum < OP_results(op); resnum++) {
				if (OP_result(op, resnum) == ret_tn) {
					Set_OP_result(op, resnum, new_tn);
				}
			}
		}
	}
#if defined(TARG_SL)	
	if (bb == NULL) return;
#endif	
	FmtAssert(bb != NULL && BB_call(bb), ("didn't find call BB before bb %d for tn %d", BB_id(current_bb), TN_number(ret_tn)));
	bb = BB_next(bb);	/* go back to bb after call */
	/* insert copy at beginning of bb */
	Exp_COPY (new_tn, ret_tn, &ops);
	BB_Prepend_Ops (bb, &ops);
}

/*
 * Check if the BB is entry BB:
 *   1. BB is entry BB or
 *   2. BB is original entry BB and now it's after the PIC entry due to PIC code on IA-32
 */
static BOOL
BB_like_entry(BB* bb)
{
    if ( BB_entry(bb) )
        return TRUE;

#if defined(TARG_X8664)
    if ( BB_after_pic_entry(bb) ) {
        Is_True( Is_Target_32bit() && Gen_PIC_Shared,
                 ("Only allow split_from_entry on IA-32 with -fPIC") );
        Is_True( BB_preds_len(bb) == 1,
                 ("after_pic_entry can only have one pred") );
        return TRUE;
    }
#endif

    return FALSE;
}

#ifdef TARG_X8664
/*
 * Handle the GNU regparm extension (including fastcall) on IA-32:
 *
 * Is this BB a non-standard call that takes the register as an argument?
 */
static BOOL
Is_Regparm_Call(BB *bb, INT regnum)
{
    if (!Is_Target_32bit() || !BB_call(bb))
        return FALSE;

    CALLINFO *call_info = ANNOT_callinfo(
        ANNOT_Get(BB_annotations(bb), ANNOT_CALLINFO));

    ST *call_st = CALLINFO_call_st(call_info);
    WN *call_wn = CALLINFO_call_wn(call_info);

    TY_IDX func_type = call_st ? ST_pu_type(call_st) : WN_ty(call_wn);
    INT regparm = TY_register_parm(func_type);

    return (regnum == RAX && regparm > 0) ||
        (regnum == RDX && regparm > 1) ||
        (regnum == RCX && regparm > 2);
}
#endif

/*
 * Is this BB a call that returns via the first arg?
 */
static BOOL
Return_Via_First_Arg(BB *bb)
{
    if (!BB_call(bb))
        return FALSE;

    CALLINFO *call_info = ANNOT_callinfo(
        ANNOT_Get(BB_annotations(bb), ANNOT_CALLINFO));

    ST *call_st = CALLINFO_call_st(call_info);
    WN *call_wn = CALLINFO_call_wn(call_info);

    TY_IDX func_type = call_st ? ST_pu_type(call_st) : WN_ty(call_wn);

    return RETURN_INFO_return_via_first_arg(Get_Return_Info(
        TY_ret_type(func_type),
        No_Simulated
#ifdef TARG_X8664
        , call_st ? PU_ff2c_abi(Pu_Table[ST_pu(call_st)]) : FALSE
#endif
        ));
}

/*
 * Check if a dedicated TN is global, and if so, make it local.
 * Our assumptions are that a use of a param reg should be in the
 * entry block, a def of a param reg should be in the call block,
 * the use of a return reg should be in the block following a call block,
 * and the def of a return reg should be in the exit block.
 * Also assume that if successor or predecessor is region,
 * then this is a glue-code block, and then okay to have dedicated tns.
 */
extern void
Check_If_Dedicated_TN_Is_Global (TN *tn, BB *current_bb, BOOL def)
{
	INT regnum = REGISTER_machine_id(TN_register_class(tn), TN_register(tn))
#ifdef TARG_X8664
	    	+ Int_Preg_Min_Offset
#endif
	    ;
	BOOL is_func_arg;
	BOOL is_func_retval;

	Is_True(TN_is_dedicated(tn), ("tn not dedicated"));

	/* first check for region glue-code blocks */
	if (def) {
		BB *bb = BB_Unique_Successor(current_bb);
		if (bb && BB_rid(bb) && (RID_level(BB_rid(bb)) >= RL_CGSCHED)) {
			/* is glue code for following region */
			return;
		}
	}
	else {	/* use */
		BB *bb = BB_Unique_Predecessor(current_bb);
		if (bb && BB_rid(bb) && (RID_level(BB_rid(bb)) >= RL_CGSCHED)) {
			/* is glue code for previous region */
			return;
		}
	}

	// for some targets, the func-arg and retval regs can overlap
	is_func_arg = REGISTER_SET_MemberP( 
	    REGISTER_CLASS_function_argument(TN_register_class(tn)), 
	    TN_register(tn));
	is_func_retval = REGISTER_SET_MemberP( 
	    REGISTER_CLASS_function_value(TN_register_class(tn)),
	    TN_register(tn));

	/*
	 *  uplevel reference use $2 as an implied argument to pass $fp
	 *  try to catch return value case (with risk of missing errors).
	 *  $2 can also be store of slink in nested pu.
	 */
	if ((PU_uplevel(Get_Current_PU()) || 
		PU_is_nested_func(Get_Current_PU()) )
	    && regnum==Static_Link_Preg_Offset 
	    && def)
	{
	 	/*  assume $2 is an argument of a call  (if def && !exit) */
	  	if (!BB_call(current_bb) && !BB_exit(current_bb)) {
			Localize_Global_Param_Reg (current_bb, tn);
		}
	}
	else if (regnum==Static_Link_Preg_Offset 
	    && PU_is_nested_func(Get_Current_PU())
	    && !def 
	    && BB_entry(current_bb) )
	{
		/* save of slink */
	}
#ifdef TARG_X8664
	else if (regnum==RAX && def && BB_call(current_bb)) {
	    	/* RAX is used to pass number of SSE regs used */
	}
    else if (def && Is_Regparm_Call(current_bb, regnum))
        ;    // okay
#endif
	else if (is_func_arg || is_func_retval) {
		if (def && is_func_arg && BB_call(current_bb))
			;	// okay
		else if (def && is_func_retval && BB_exit(current_bb))
			;	// okay
#if defined(TARG_IA32)
		else if (def && is_func_retval && BB_asm(current_bb))
			;	// okay
#endif // TARG_IA32
#if defined(TARG_X8664) || defined(TARG_SL)
		/* An inline asm could read whatever it wants (bug#3059,3257). */
		else if (BB_asm(current_bb))
			;	// okay
		/* An inline asm could write whatever it wants (bug#3059). */
		else if (!def && is_func_arg
			 && BB_prev(current_bb) != NULL 
			 && BB_asm(BB_prev(current_bb)))
			;	// okay	
#endif // TARG_X8664 || TARG_SL
		else if (def && is_func_arg && BB_asm(current_bb))
			;	// okay
        else if (def && is_func_retval
            && Return_Via_First_Arg(current_bb))
			// can return via first arg in retval reg.
			;	// okay
		else if (!def && is_func_retval && BB_entry(current_bb)
		    && RETURN_INFO_return_via_first_arg(Get_Return_Info(
			  TY_ret_type(PU_prototype(Get_Current_PU())), 
			  No_Simulated
#ifdef TARG_X8664
			  , PU_ff2c_abi(Get_Current_PU())
#endif
			  ) ) )
			// can return via first arg in retval reg.
			;	// okay
		// If arg and retval overlap,
		// how do we distinguish between def before call
		// and def before exit?
		// Could combine the Param_ and Return_ routines,
		// but instead we search to try and figure out which
		// case it is.
		else if (def && is_func_arg && is_func_retval
			&& !BB_call(current_bb) && !BB_exit(current_bb)) 
		{
			if (BB_Is_Followed_By_Call (current_bb)) 
				Localize_Global_Param_Reg (current_bb, tn);
			else
				Localize_Global_Return_Reg_Def (current_bb, tn);
		}
		else if (def && is_func_arg && !BB_call(current_bb)) {
			Localize_Global_Param_Reg (current_bb, tn);
		} 
#ifdef TARG_X8664
                else if (def && is_func_retval && PU_has_builtin_apply) {
                        ;       // okay
                }
#endif
		else if (def && is_func_retval && !BB_exit(current_bb)) {
			Localize_Global_Return_Reg_Def (current_bb, tn);
		}
		else if (!def && is_func_arg && BB_entry(current_bb)) 
			;	// okay
#ifdef KEY
		else if (!def && is_func_arg && BB_call(current_bb))
			;	// okay.  CSE might have replaced a use of a
				// non-dedicated TN with a use of a param reg,
				// bug 7219.
#endif
#ifdef TARG_X8664
		else if (!def && regnum == RAX && BB_entry(current_bb)) 
			;	// okay because RAX gives number of xmm args
		else if (!def && regnum == RDX &&
			 BB_entry(current_bb) && BB_handler(current_bb))
		  ;   // okay because RAX and RDX will be saved at the entry of a handler
                else if (!def && regnum == RDX &&
                        PU_has_builtin_apply_args && BB_entry(current_bb))
                    ;  // okay because RDX will be saved at the function entry
                else if (!def && Is_Target_32bit() && BB_like_entry(current_bb) &&
                          ( (regnum==RAX && TY_register_parm( Get_Current_PU_TY() ) > 0 ) ||
                            (regnum==RDX && TY_register_parm( Get_Current_PU_TY() ) > 1 ) ||
                            (regnum==RCX && TY_register_parm( Get_Current_PU_TY() ) > 2 ) ) )
                        ;       // okey because RAX, RDX, RCX can be args for regparm on IA-32
#endif
		else if (!def && is_func_retval 
		    && BB_prev(current_bb) != NULL 
		    && BB_call(BB_prev(current_bb)))
			;	// okay
		else if (!def && is_func_retval 
		    && BB_prev(current_bb) != NULL 
		    && BB_asm(BB_prev(current_bb)))
			;	// okay
#if defined(TARG_SL)
		else if (is_func_retval && BB_entry(current_bb) && BB_exit(current_bb))
		  ; // okay
		else if (is_func_retval && BB_exit(current_bb) && !def) {
		  ; // okay
		}
#endif
		else if (!def && is_func_retval
		    && (BB_prev(current_bb) == NULL 
			|| ! BB_call(BB_prev(current_bb)))) 
		{
			/* not after call, so must span bb's */
			Localize_Global_Return_Reg (current_bb, tn);
		}
#ifdef TARG_LOONGSON
		else if (!def && BB_handler(current_bb))
		  ;
#endif
		else {
			FmtAssert (FALSE, ("use of param reg TN%d in bb %d is global", TN_number(tn), BB_id(current_bb)));
		}
	} 
#ifdef HAS_STACKED_REGISTERS
	// func_arg applies to formal args, 
	// but actual args may be different numbered registers
	// that just end up matching on the stack.
	if (REGISTER_Is_Stacked_Output(
		TN_register_class(tn),
		TN_register(tn) )) 
	{
		if (def && BB_call(current_bb))
			;	// okay
		else if (BB_asm(current_bb))
			;	// okay
		else if (BB_prev(current_bb) != NULL
		         && BB_asm(BB_prev(current_bb)))
                        ;       // okay
		else if (def && !BB_call(current_bb)) {
			Localize_Global_Param_Reg (current_bb, tn);
		} 
		else {
			FmtAssert (FALSE, ("use of stacked param reg TN%d in bb %d is global", TN_number(tn), BB_id(current_bb)));
		}
	}
#endif
}

/* 
 * Check if a TN is global by seeing if it was also used in another bb.
 * (There are 2 states to consider:  only used in current bb, or used in
 * current_bb and some other bb).
 * If global, mark the TN with TN_Is_Global.
 * Also mark BB_has_globals for efficiency in later processing.
 */
static void
Check_If_TN_Is_Global (TN *tn, TN_MAP tn_in_bb, BB *current_bb, BOOL def)
{
  BB *mapped_bb;
  PREG_NUM preg;
  RID *rid = BB_rid(current_bb);
  
  Is_True(!TN_is_dedicated(tn), ("tn is dedicated"));

#if defined(TARG_SL)
  // workaround for vla where we generate 64bit inst from gcc tree
  if (TN_size(tn) == 8) {
    DevWarn("setting TN %d from 8 to 4", TN_number(tn));
    Set_TN_size(tn, 4);
  }
#endif

  mapped_bb = (BB*) TN_MAP_Get (tn_in_bb, tn);

  if (rid && (RID_bounds_exist(rid) == REGION_BOUND_EXISTS)
	&& (preg = (PREG_NUM)(INTPTR) TN_MAP_Get(TN_To_PREG_Map, tn))) 
  {
	/*
	 * If a preg tn is used in the region, 
	 * then must have come from front-end,
	 * and have to assume that the preg may exist 
	 * outside the region.
	 * Thus the preg tn must be marked as global 
	 * and given a spill location.
	 * This spill location will then be used to 
	 * create the glue code copies
	 * from the spill location back to the preg.
	 */
	Set_TN_is_global_reg(tn);
	if (Trace_Localize) {
		fPrint_TN (TFile, "%s is a global tn,", tn);
		fprintf (TFile, "(%d) corresponding to preg %d\n", TN_size(tn), preg);
	}
	if (def) {
	  for (INT i=0; i<RID_num_exits(rid); i++)
	    REGION_add_preg_out(rid, i, preg, MTYPE_V);
	} else {
	  REGION_add_preg_in(rid, preg, MTYPE_V);
	}
  }

  if (TN_is_global_reg(tn)) {
	Set_BB_has_globals(current_bb);
  } else {
	if (mapped_bb == NULL) {
		TN_MAP_Set (tn_in_bb, tn, current_bb);
	} else if (mapped_bb != current_bb) {
		/* in multiple bb's, so is global */
		Set_TN_is_global_reg(tn);
		Set_BB_has_globals(current_bb);
		Set_BB_has_globals(mapped_bb);
		if (Trace_Localize) {
			fPrint_TN (TFile, "%s is a global tn\n", tn);
		}
	}
  }
}

/*
 * Set TN_is_global_reg for each element of the input list
 */
static void
TN_LIST_Mark_Is_Global( TN_LIST *tnl0 ) 
{
  TN_LIST *tnl;
  TN *tn;

  for ( tnl = tnl0; tnl; tnl = TN_LIST_rest( tnl ) ) {
    tn = TN_LIST_first( tnl );
    Set_TN_is_global_reg( tn );
  }
}
  
extern void dump_tn(TN*);
/* 
 * Iterate through bb's to find any global TN's,
 * which are defined as any TN's that exist in multiple bb's.
 * Such TN's will be marked with the TN_Is_Global flag.
 */
static void
Find_Global_TNs ( RID *rid )
{
  BB *bb;
  OP *op;
  TN *tn;
  TN_MAP tn_in_bb = TN_MAP_Create();
  INT opndnum;
  INT resnum;
  RID *bbrid;
  CGRIN *cgrin;
  TN_LIST *tnl;
  INT num_exits, i;

  if ( rid ) {
    /*
     * if live in and out tns are available, mark them
     * all global.  This guarantees that we will spill/restore
     * them in the current REGION, and restore/spill them
     * in the glue code in the container
     */
    cgrin = RID_cginfo( rid );
    tnl = CGRIN_tns_in( cgrin );
    TN_LIST_Mark_Is_Global( tnl );
    num_exits = RID_num_exits( rid );
    for ( i = 0; i < num_exits; i++ ) {
	tnl = CGRIN_tns_out_i( cgrin, i );
	TN_LIST_Mark_Is_Global( tnl );
    }
  }

  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    if ((bbrid = BB_rid(bb)) && (RID_level(bbrid) >= RL_CGSCHED))
      /*
       * There is no overlap in TN's between the current REGION
       * and REGIONs which have already been through CG
       */
      continue;
    FOR_ALL_BB_OPs (bb, op) {
      /* process all the operand TNs. */
      for (opndnum = 0; opndnum < OP_opnds(op); opndnum++) {
        tn = OP_opnd(op, opndnum);
        if (tn == NULL || TN_is_constant(tn)) continue;
        if (TN_is_dedicated(tn)) {
          if (OP_same_res(op) && tn == OP_result(op,0)) {
            /* this use is just a copy of the def */
            continue;
	  }
#ifdef KEY
	  /* Bug#931
	     We cannot assume <op> has only one result. An extension
	     of the previous checking.
	   */
	  if( OP_same_res(op) && OP_Defs_TN(op,tn) ){
	    continue;
	  }
#endif
	  if (OP_copy(op) && tn == OP_result(op,0)) {
            /* this use is just a self-copy, will disappear */
            continue;
	  }
          Check_If_Dedicated_TN_Is_Global (tn, bb, FALSE /*def*/);
        } else {
          Check_If_TN_Is_Global (tn, tn_in_bb, bb, FALSE /*def*/);
        }
      }
      /* process all the result TN */
      for (resnum = 0; resnum < OP_results(op); resnum++) {
        tn = OP_result(op, resnum);
        if (TN_is_dedicated(tn)) {
          Check_If_Dedicated_TN_Is_Global (tn, bb, TRUE /*def*/);
        } else {
          if (OP_cond_def(op)) {
            // cond_def is an implicit use
            Check_If_TN_Is_Global (tn, tn_in_bb, bb, FALSE /*def*/);
          }
          Check_If_TN_Is_Global (tn, tn_in_bb, bb, TRUE /*def*/);
        }
      }
    }
  }
  TN_MAP_Delete (tn_in_bb);
}


/* Each global tn in a bb has an associated SPILL_TN_INFO.  */
typedef struct spill_tn_info {
	TN *local_tn;		/* the last local tn for the global */
				/* (may have multiple local tns for a global
				 * in the bb, one for each live-range) */
	OP *last_def;		/* op containing last definition of tn */
	OP *exposed_use;	/* op containing first use of tn before def */
	TN *exposed_use_tn;	/* local tn associated with exposed_use */
	struct spill_tn_info *next;	/* next spill tn, for traversing */
	BOOL callee_save;	/* no spilling needed if callee-save tn */
} SPILL_TN_INFO;
static SPILL_TN_INFO *first_spill_tninfo = NULL;
static SPILL_TN_INFO *last_spill_tninfo = NULL;

static INT num_stacked_regs = 0;

/*
 * Get a local tn for the global tn, either by creating a duplicate
 * or by using the local already associated with the global. 
 * The "reuse" boolean says whether to re-use the existing local tn.
 */
static SPILL_TN_INFO *
Get_Local_TN_For_Global (TN *global_tn, TN_MAP spill_tns, BB *bb, BOOL reuse)
{
  SPILL_TN_INFO *tninfo;
  TN *local_tn = NULL;
  tninfo = (SPILL_TN_INFO*) TN_MAP_Get (spill_tns, global_tn);
  if (tninfo == NULL) {
	tninfo = TYPE_MEM_POOL_ALLOC (SPILL_TN_INFO, &MEM_local_pool);
	TN_MAP_Set (spill_tns, global_tn, tninfo);
	if (first_spill_tninfo == NULL) {
		first_spill_tninfo = tninfo;
	} else {
		last_spill_tninfo->next = tninfo;
	}
	last_spill_tninfo = tninfo;
  }
  if (reuse) 
 	local_tn = tninfo->local_tn;
  if (local_tn == NULL) {
	PREG_NUM preg;
        /* if this tn is homeable, clobber its homing info */
  	if (TN_is_gra_homeable(global_tn)) {
		Reset_TN_is_gra_homeable(global_tn);
		Set_TN_home(global_tn, NULL);
	}

#ifdef HAS_STACKED_REGISTERS
	// Normally we just create a local non-dedicated tn
	// and spill it at bb boundaries.
	// But if callee-save stacked registers are available,
	// then is cheaper to use those (no spills).
	if (LOCALIZE_using_stacked_regs &&
		REGISTER_Has_Stacked_Registers(
			TN_register_class(global_tn)) )
	{
		// In this case we need to reuse local tn,
		// not create a new one for each bb.
		// problem is that tninfo is local to a bb.
		// So instead, just allocate the global tn
		// to the dedicated stacked register.
		if (TN_register(global_tn) == REGISTER_UNDEFINED) {
		    // HACK:  Leave at least 8 regs for LRA.
		    if ((num_stacked_regs + 8) < 
			REGISTER_Number_Stacked_Registers_Available(
				TN_register_class(global_tn))) 
		    {	
		    	REGISTER reg = 
				REGISTER_Request_Stacked_Register(
					ABI_PROPERTY_callee, 
					TN_register_class(global_tn) );
		    	if (reg == REGISTER_UNDEFINED) {
				DevWarn("localize ran out of stacked regs");
				tninfo->callee_save = FALSE;	// spill
			}
		        else {
				Set_TN_register(global_tn, reg);
		    		++num_stacked_regs;
				tninfo->callee_save = TRUE;
			}
	    	    }
		    else {
			if (Trace_Localize) fprintf(TFile, 
				"localize stopped using stacked regs");
			tninfo->callee_save = FALSE;	// spill
		    }
		}
		else {
			tninfo->callee_save = TRUE;
		}
		local_tn = global_tn;
	}
#endif
	if (tninfo->callee_save == FALSE) {
		local_tn = Dup_TN (global_tn);
		/* copy spill location 
	 	 * (possibly create spill location, but that's okay
	 	 * cause we know we have to spill this global tn). */
		Set_TN_spill(local_tn, CGSPILL_Get_TN_Spill_Location (
			global_tn, CGSPILL_LCL));
		tninfo->callee_save = FALSE;
		preg = (PREG_NUM)(INTPTR) TN_MAP_Get( TN_To_PREG_Map, global_tn );
#ifdef KEY
		Is_True(preg >= 0, ("Get_Local_TN_For_Global: "
				    "negative index to PREG_To_TN_Array"));
#endif
		if (preg) PREG_To_TN_Array[preg] = local_tn;
	}
	tninfo->local_tn = local_tn;
	Trace_Replace ("global",global_tn, "local",local_tn, bb);
  }
  return tninfo;
}

/*
 * Iterate over BBs and insert spills of any global TNs in the BB, 
 * so all TNs become local.
 * If use before def, then put load before first use; 
 * if def then put store after last def.
 * Originally we just put the load/stores at the beginning/end of the bb,
 * but by putting them adjacent to the first use/last def,
 * we help lra by having smaller live ranges (also helps lra to remove copies).
 * Also rename global TN's so they are local to bb.
 */
static void
Insert_Spills_Of_Globals (void)
{
  BB *bb;
  OP *op;
  TN *tn;
  TN_MAP spill_tns;	/* map of SPILL_TN_INFO */
  SPILL_TN_INFO *tninfo;
  INT opndnum;
  INT resnum;
  ST *spill_loc;
  OPS spill_ops;
  RID *bbrid;
  num_stacked_regs = 0;

  MEM_POOL_Push(&MEM_local_pool);

  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    if ((bbrid = BB_rid(bb)) && (RID_level(bbrid) >= RL_CGSCHED))
      /*
       * don't change bb's which have already been through CG
       */
      continue;
    if ( ! BB_has_globals(bb)) continue;	/* ignore this bb */

    /* clear map and sets for each BB */
    spill_tns = TN_MAP_Create();
    first_spill_tninfo = last_spill_tninfo = NULL;

    FOR_ALL_BB_OPs_FWD (bb, op) {
      /* process operand TNs first, so find uses before defs. */
      for (opndnum = 0; opndnum < OP_opnds(op); opndnum++) {
        tn = OP_opnd(op, opndnum);
        if (tn != NULL && TN_is_global_reg(tn)
#ifdef KEY
	    && !TN_is_dedicated(tn)
#endif
#ifdef TARG_X8664
	    // Localize all if CG_localize_tns=1, else localize only floats.
	    && (CG_localize_tns || TN_is_float(tn))
#endif
	    		) {
          tninfo = Get_Local_TN_For_Global (tn, spill_tns, bb, TRUE/*reuse*/);
          /* replace global tn with new local tn */
          Set_OP_opnd(op, opndnum, tninfo->local_tn);

          /* don't want to add loads for dummy pregtns */
          if (OP_code(op) == TOP_begin_pregtn) continue;
          if (OP_code(op) == TOP_end_pregtn) continue;
          
          /* if no def before use, then must re-load */
          if (tninfo->exposed_use == NULL && tninfo->last_def == NULL) {
            tninfo->exposed_use = op;
            tninfo->exposed_use_tn = tninfo->local_tn;
          }
        }
      }
      for (resnum = 0; resnum < OP_results(op); resnum++) {
        tn = OP_result(op, resnum);
        if (TN_is_global_reg(tn)
#ifdef KEY
	    && !TN_is_dedicated(tn)
#endif
#ifdef TARG_X8664
	    // Localize all if CG_localize_tns=1, else localize only floats.
	    && (CG_localize_tns || TN_is_float(tn))
#endif
	    		) {
          /* 
           * Each def of a global tn is given a new local tn,
           * unless it repeats the last def
           * (assumption: only repeat last def is in same_res OP;
           * if not then still correct just larger live range).
           * cond_def also requires reusing the tn.
           * Then the live-range (def->use) is smaller than
           * if we reused the local tn throughout the block.
           * We then only spill the last local tn.
           */
          tninfo = Get_Local_TN_For_Global (tn, spill_tns, bb, 
                                            OP_same_res(op) || OP_cond_def(op) /*reuse*/);
          /* replace global tn with new local tn */
          Set_OP_result(op, resnum, tninfo->local_tn);

          /* cond_def is an implicit use */
          if (OP_cond_def(op) && 
              tninfo->exposed_use == NULL && 
              tninfo->last_def == NULL) {
            tninfo->exposed_use = op;
            tninfo->exposed_use_tn = tninfo->local_tn;
          }

          /* if a def, then must spill */
          tninfo->last_def = op;
        }
      }
    }

    /*
     * now generate spill code for new local tns.
     * we iterate over the spill_tninfo list,
     * which is probably faster than searching through a set.
     */
    for (tninfo = first_spill_tninfo; tninfo != NULL; tninfo = tninfo->next) 
    {
      if (tninfo->callee_save) continue;
      if (tninfo->last_def != NULL) {
        /* spill */
        spill_loc = CGSPILL_Get_TN_Spill_Location (tninfo->local_tn, CGSPILL_LCL);
        OPS_Init(&spill_ops);
        CGSPILL_Store_To_Memory (tninfo->local_tn, spill_loc,
                                 &spill_ops, CGSPILL_LCL, bb);
        CGSPILL_Insert_Ops_After (bb, tninfo->last_def, &spill_ops);
      } 
      if (tninfo->exposed_use != NULL) {
        /* load */
        spill_loc = CGSPILL_Get_TN_Spill_Location (tninfo->exposed_use_tn, CGSPILL_LCL);
        OPS_Init(&spill_ops);
        CGSPILL_Load_From_Memory (tninfo->exposed_use_tn, spill_loc,
                                  &spill_ops, CGSPILL_LCL, bb);
        CGSPILL_Insert_Ops_Before (bb, tninfo->exposed_use, &spill_ops);
      }
    }
    /* Could clear each tn_map entry that is in def/use_tns,
     * but is faster to just delete/create tn_map with generation-count */
    TN_MAP_Delete (spill_tns);
  }
  MEM_POOL_Pop(&MEM_local_pool);	/* free tninfo */

  if (Trace_Localize && num_stacked_regs) 
	fprintf(TFile, "num_stacked_regs = %d\n", num_stacked_regs);
}

/*
 * Change any global TNs into local TNs
 * within the current REGION or the whole PU if rid is NULL
 */
extern void
Localize_Any_Global_TNs ( RID *rid )
{
	Trace_Localize = Get_Trace (TP_LOCALIZE, 1);

	/*
	 * It is possible that there will not be a return block
	 * (because function never returns), but still need to
	 * spill (save/restore) the RA so that stacktrace tools work.
	 * So force the RA to be spilled.
	 * But don't do this if a single-bb leaf routine.
	 * Even multiple-bb leaf routines will need the save/restore
	 * in case LRA allocates $31 to an inner block.
	 */
	if (NULL != RA_TN && PU_BB_Count > 1) {
		Set_TN_is_global_reg(SAVE_tn(Return_Address_Reg));
	}

	Find_Global_TNs( rid );

	Insert_Spills_Of_Globals();

}


// The following routines are for the case where we are doing GRA,
// but we don't want to see global dedicated tns.

#define BB_SEARCH_LIMIT 1000      // some ridiculously high number

/* =======================================================================
 *
 *  BB_Reaches_Call_or_Exit
 *
 *  Return start or any BB that can be reached from start by following
 *  unique successors which contains a CALL (or an asm) or an Exit.
 *
 * =======================================================================
 */
#ifndef TARG_LOONGSON
static BB *
BB_Reaches_Call_or_Exit( BB *start )
{
  BB *bb, *ans;
  RID *rid;
  BB_MAP visited = BB_MAP32_Create();

  ans = NULL;
  BB_MAP32_Set( visited, start, 1 );
  
  for ( bb=start; bb; bb=BB_Unique_Successor_Not_In_Set(bb, visited) ) {
    rid = BB_rid(bb);
    if (rid && RID_level(rid) >= RL_CGSCHED)
      break;
    if (BB_call(bb) || BB_exit(bb) || BB_asm(bb)) {
      ans = bb;
      break;
    }
    BB_MAP32_Set( visited, bb, 1 );
  }

  BB_MAP_Delete( visited );

  return ans;
}
#else
 /* For loongson, there will be some codes like:
  *                    IF:    $4 = ...
  *                            $5 = ...
  *                            $6 = ...
  *                            hi, lo = div ...
  *                            ... ...
  *                            beq ...
  *                 /      |
  *               /        |
  *       THEN:         |
  *           ... ...       |
  *              \          |
  *               \         |
  *                  CALL:
  *                      $7 = ...
  *                      printf(... )
  *
  * For the above example, $4,$5,$6,$7 are parameters of the call. But
  * when preparing those parameters, there is a branch. So, just looking
  * for the unique successor will not work any longer. We need a recursive
  * way to look for the reached CALL/EXIT bb.
  *
  */
static BB*
BB_Reaches_Call_or_Exit_Helper(BB* start, BB_MAP visited)
{
  BB *ans = NULL, *result = NULL;
  RID *rid;
  BBLIST *slist;
  BB *succ;

  if ( BB_MAP32_Get( visited, start) )
    return NULL;

  rid = BB_rid(start);
  if (rid && RID_level(rid) >= RL_CGSCHED)
    return NULL;

  BB_MAP32_Set( visited, start, 1 );

  if (BB_call(start) || BB_exit(start) || BB_asm(start)) {
     return start;
  }

  for ( slist = BB_succs( start ); slist; slist = BBLIST_next( slist ) ) {
    succ = BBLIST_item( slist );
    ans = BB_Reaches_Call_or_Exit_Helper(succ, visited);
    if (ans != NULL) {
      if (result == NULL) {
        result = ans;
      } else {
        Is_True(ans == result,
                      ("BB%d reaches two or more CALL/EXIT BBs.", start->id));
      }
    }
    return ans;
  }
}

static BB *
BB_Reaches_Call_or_Exit( BB *start )
{
  BB_MAP visited = BB_MAP32_Create();

  BB *ans = BB_Reaches_Call_or_Exit_Helper(start, visited);

  BB_MAP_Delete( visited );
  return ans;
}
#endif



/* =======================================================================
 *
 *  Call_or_Entry_Reaches_BB
 *
 *  Find the first BB containing a CALL (or asm) or an Entry that reaches
 *  <start> by following unique predecessors.
 *
 *  If <start> is reached by a call, return the block after the call.
 *  If <start> is reached by an entry, return the entry block.
 *
 * =======================================================================
 */
static BB *
Call_or_Entry_Reaches_BB(BB *start)
{
  BB *bb;
  RID *rid;
  // can get in situation where preds form infinite loop
  // (there is a succ out of loop, but preds are unique).
  // in that case we could mark as visited to avoid loop,
  // but for simplicity we just give up if looped too many times;
  // should be okay to do so as this means there is no nearby call/entry.
  INT count = 0;

  if (BB_entry( start))
    return start;
  for (bb = BB_Unique_Predecessor(start); bb; bb = BB_Unique_Predecessor(bb)) {
    ++count;
    if (count > BB_SEARCH_LIMIT) {
	DevWarn("Call_or_Entry_Reaches_BB looped too long so give up");
	return NULL;
    }
    rid = BB_rid(bb);
    if (rid && RID_level(rid) >= RL_CGSCHED)
	return NULL;
    if (BB_call(bb) || BB_entry(bb) || BB_asm(bb)) {
      // For calls, we want the next block, since that is the return block
      if (BB_call(bb) || BB_asm(bb))
	bb = BB_next(bb);
      return bb;
    }
  }

#if defined(TARG_SL) || defined(TARG_PPC32)
  /*
   * Fix a bug, in OspreyTest/SingleSource/gcc.c-torture/double/unsorted/poor.c
   * If  preds(bb1) = {bb2, bb3}, the original code only search bb2, but bb3 is omitted.
   * The added codes is a breadth-first search up travel.
   */
  std::queue<BB*> to_visit;
  std::set<BB*>   visited_bb;
  to_visit.push(start);
  int cnt = 0;

  /* CFG : breadth-first search up travel  */
  while ((!to_visit.empty()) && (cnt++ < BB_SEARCH_LIMIT)) {
    bb = to_visit.front();
    visited_bb.insert(bb);
    to_visit.pop();
    if (BB_entry(bb)) return bb;

    BBLIST* nxt;
    BBLIST* prevs;

	/* Push all not visited pred of BB to to_visit queue */
    for (prevs = BB_preds(bb); prevs; prevs = nxt) {
      BB* bb_prev = BBLIST_item(prevs);
      nxt = BBLIST_next(prevs);

      if (visited_bb.count(bb_prev) == 0) 
        to_visit.push(bb_prev);
    } 
  }
#endif  
  return NULL;
}

/* =======================================================================
 *
 *  BB_Successor_Compiled_Region
 *
 *  Return the BB_rid of the first BB which has already been through CG scheduling
 *  that can be reached from start by following unique successors.  If no such BB
 *  is found, return NULL.
 *
 * =======================================================================
 */
static RID *
BB_Successor_Compiled_Region( BB *start )
{
  BB *bb;
  RID *rid;
  
  for ( bb = BB_Unique_Successor( start ); bb; bb = BB_Unique_Successor(bb) ) {
    rid = BB_rid( bb );
    if ( rid == NULL )
      continue;
    if ( RID_level( rid ) >= RL_CGSCHED )
      return rid;
  }
  return NULL;
}

/* =======================================================================
 *
 *  BB_Predecessor_Compiled_Region
 *
 *  Return the BB_rid of the first BB which has already been through CG
 *  scheduling that can be reached from start by following unique
 *  predecessors.  If no such BB is found, return NULL.  If such a BB is
 *  found, return the index of the corresponding exit from the REGION
 *  through exit_num.
 *
 * =======================================================================
 */
static RID *
BB_Predecessor_Compiled_Region( INT *exit_num, BB *start )
{
  // see comment in other predecessor search about catching infinite loops.
  INT count = 0;
  for (BB *bb=BB_Unique_Predecessor(start); bb; bb=BB_Unique_Predecessor(bb)) {
    ++count;
    if (count > BB_SEARCH_LIMIT) {
	DevWarn("BB_Predecessor_Compiled_Region looped too long so give up");
	return NULL;
    }
    RID *rid = BB_rid(bb);
    if (rid == NULL)
      continue;
    if (RID_level( rid ) >= RL_CGSCHED) {
      *exit_num = BB_REGION_Exit( bb, rid),
      FmtAssert(*exit_num != NO_REGION_EXIT,
		("predecessor %d of %d in already compiled REGION %d is "
		 "not an exit", BB_id(bb), BB_id(start), RID_id(rid)));
      return rid;
    }
  }
  return NULL;
}

#ifdef TARG_SL
/* Is Tn defined in Local BB before end_op */
static BOOL 
TN_Def_Local(BB * bb, OP * end_op, TN * ded_tn)
{
  OP * op = NULL;
  TN * tn = NULL;
  FOR_ALL_BB_OPs (bb, op) {
  	if (op == end_op) {
	  break;
  	}
	for (INT i = 0; i < OP_results(op); i++) {
	  tn = OP_result(op,i);
	  if (tn == ded_tn) 
	  	return TRUE;
	}
  }

  return FALSE;
}
#endif

/* =======================================================================
 *
 *  Localize_or_Replace_Dedicated_TNs
 *
 *  This is for the GRA case only.
 *
 *  References to a dedicated TN in a BB that has not already been compiled
 *  should be associated with a CALL, an ENTRY, or a previously compiled REGION.
 *  The references associated with a previously compiled REGION
 *  are eliminated by replacing them with references to previously allocated, but
 *  non-dedicated TNs.  The references associated with CALLs and ENTRYs
 *  are localized, except we allow a reference to a return value in the block
 *  immediately following a CALL.
 *
 *  We assume that a def of a dedicated TN has no explicit uses, and exactly
 *  one implicit use, either a CALL or a previously compiled REGION.
 *  Note that OP_same_res operands are an exception to this rule.
 *  We assume that a use of a dedicated TN has no explicit reaching def
 *  and exactly one implicit reaching def, either a CALL or an ENTRY or a
 *  previously compiled REGION.
 *
 * =======================================================================
 */
void
Localize_or_Replace_Dedicated_TNs(void)
{
  BB *bb, *non_region_use_bb, *non_region_def_bb;
  RID *rid, *srid;
  OP *op;
  OPS ops;
  TN *tn, *allocated_tn, *new_tn = NULL, *prev_result = NULL;
  TN_LIST *tns_in, *tns_out;
  INT opndnum, exit_num;

  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {

    rid = BB_rid(bb);
    if ( rid && RID_level(rid) >= RL_CGSCHED ) // already compiled
      continue;

    if ( ! BB_has_globals(bb) ) // no dedicated tns referenced in this bb
      continue;

    Reset_BB_has_globals( bb );

    // look for defs of dedicated TNs
    non_region_use_bb = BB_Reaches_Call_or_Exit( bb );
    if ( non_region_use_bb )
      srid = NULL;
    else
      srid = BB_Successor_Compiled_Region( bb );

    FOR_ALL_BB_OPs (bb, op) {
      for (INT i = 0; i < OP_results(op); i++) {
	tn = OP_result(op,i);
	if ( ! TN_is_dedicated( tn ) ) continue;
	/* Use of non-allocatable registers ($sp, $gp) is OK */
	if (!REGISTER_allocatable(TN_register_class(tn), TN_register(tn)))
	  continue;
	if ( non_region_use_bb == bb )
	  continue; // the use is already local
	else if ( non_region_use_bb ) {
	  // replace this def by a def of new_tn and add a copy tn <- new_tn
	  // in non_region_use_bb
	  // We do have one exception:  OP_same_res operands, 
	  // which have multiple defs and and use all of the same tn.
	  // There are two case:
	  // ldl ded_tn, tn, zero_tn ; ldr ded_tn, tn, ded_tn
	  // and copy ded_tn, tn ; select ded_tn, tn, tn, ded_tn
	  // To handle these cases we make use of the fact that the
	  // operands pairs should always be adjacent (that's how cgexp 
	  // creates them).
	  if (OP_same_res(op) && (tn == prev_result)
	      && (OP_opnd(op,OP_opnds(op)-1) == prev_result)) {
	    // re-use new_tn everywhere
	    Set_OP_result( op, i, new_tn );
	    Set_OP_opnd (op, OP_opnds(op)-1, new_tn);
	  } else {
	    new_tn = Build_TN_Like( tn );
	    Set_OP_result( op, i, new_tn );
	    Exp_COPY( tn, new_tn, OPS_Init(&ops) );
	    BB_Prepend_Ops (non_region_use_bb, &ops);
	    prev_result = tn;
	  }
	}
	else if (srid != NULL) {
	  // tn used by a previously compiled REGION
	  tns_in = CGRIN_tns_in( RID_cginfo( srid ) );
	  allocated_tn = Find_TN_with_Matching_Register( tn, tns_in );
	  FmtAssert( allocated_tn != NULL,
		    ("dedicated TN %d in bb %d reaches REGION %d with no matching"
		     " exposed register", 
			  TN_number(tn), BB_id(bb), RID_id(srid)));
	  Set_OP_result( op, i, allocated_tn );
	  Set_OP_glue(op);
	  if ( Trace_REGION_Interface ) {
	    fprintf( TFile, "RGN %d, replacing def of dedicated TN ",
		    RID_id(rid) );
	    Print_TN( tn, FALSE );
	    fprintf( TFile, " with allocated TN " );
	    Print_TN( allocated_tn, FALSE );
	    fprintf( TFile, " in BB %d\n", BB_id(bb) );
	  }
	}
	else {
	  #pragma mips_frequency_hint NEVER
	  FmtAssert( FALSE,
		    ("def of %s in BB:%d does not reach either CALL or "
		     "allocated REGION",
		     REGISTER_name(TN_register_class(tn), TN_register(tn)),
		     BB_id(bb)));
	  /*NOTREACHED*/
	}
      }
    } // FOR_ALL_BB_OPs (bb, op)

    // look for uses of dedicated TNs
    non_region_def_bb = Call_or_Entry_Reaches_BB( bb );
    if ( non_region_def_bb )
      srid = NULL;
    else
      srid = BB_Predecessor_Compiled_Region( &exit_num, bb );

    FOR_ALL_BB_OPs (bb, op) {

#if defined(TARG_X8664) || defined(TARG_SL)
      /* Do not replace dedicated tn inside an inline asm. (bug#3067)
       */
      if( OP_code(op) == TOP_asm ){
	continue;
      }
#endif
      for ( opndnum = 0; opndnum < OP_opnds( op ); opndnum++ ) {
	tn = OP_opnd( op, opndnum );
	if ( TN_is_constant(tn) || TN_is_zero_reg(tn) )
	  continue;
	if ( ! TN_is_dedicated(tn) )
	  continue;
	// Use of non-allocatable registers ($sp, $gp) is OK
	if ( ! REGISTER_allocatable(TN_register_class(tn), TN_register(tn)) )
	  continue;
	// use of $25 in a call OP is OK. We will localize the def if needed
	if ( TN_is_ep_reg(tn) && OP_call(op) ) 
	  continue;
	// unaligned_loads have a copy of last def as an implicit use
	if ( OP_same_res(op) && tn == OP_result(op,0) )
	  continue;
#ifdef TARG_X8664
	// Almost all x86 insns have the OP_same_res property even though they
	// are not recorded as such in isa_properties.cxx.  Bug 6866.
	if ( tn == OP_result(op,0) )
	  continue;
#endif
	if ( non_region_def_bb == bb ) // the def is already local
	  continue;
#if defined(TARG_SL)	
	else if (TN_Def_Local(bb, op, tn))
	  continue;	
#endif	
	else if ( non_region_def_bb ) {
	  // replace this use by a use of new_tn and add a copy new_tn <- tn
	  // in non_region_def_bb
	  new_tn = Build_TN_Like( tn );
	  Set_OP_opnd( op, opndnum, new_tn );
	  Exp_COPY( new_tn, tn, OPS_Init(&ops) );
	  BB_Prepend_Ops(non_region_def_bb, &ops);
	} else {
	  // tn should be defined by a previously compiled REGION
	  FmtAssert( ( srid != NULL ),
		    ("use of %s (TN%d) in BB:%d at line %d not reached by "
		     "CALL/allocated REGION",
		     REGISTER_name(TN_register_class(tn), TN_register(tn)), 
		     TN_number(tn), BB_id(bb),
		     SRCPOS_linenum(OP_srcpos(op)) ));
	  tns_out = CGRIN_tns_out_i( RID_cginfo( srid ), exit_num );
	  allocated_tn = Find_TN_with_Matching_Register( tn, tns_out );
	  FmtAssert(allocated_tn != NULL,
		    ("dedicated TN %d reached by REGION %d with no matching "
		     "defined register", TN_number(tn), RID_id(srid)));
	  Set_OP_opnd( op, opndnum, allocated_tn );
	  Set_OP_glue( op );
	  if ( Trace_REGION_Interface ) {
	    fprintf( TFile, "RGN %d, replacing use of dedicated TN ",
		    RID_id(srid) );
	    Print_TN( tn, FALSE );
	    fprintf( TFile, " with allocated TN " );
	    Print_TN( allocated_tn, FALSE );
	    fprintf( TFile, " in BB %d\n", BB_id(bb) );
	  }
	}

      } // for ( opndnum = 0; opndnum < OP_opnds( op ); opndnum++ )
    } // FOR_ALL_BB_OPs (bb, op)
  } // for (bb = REGION_First_bb
}

