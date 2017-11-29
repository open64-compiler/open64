/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2008 PathScale, LLC.  All Rights Reserved.
 */

/*
 * Copyright (C) 2007 Pathscale, LLC.  All Rights Reserved.
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

#include "defs.h"
#include "tn.h"
#include "op.h"
#include "bb.h"
#include "wn.h"
#include "symtab.h"
#include "errors.h"
#include "targ_sim.h"
#include "ttype.h"
#include "topcode.h"
#include "register.h"
#include "entry_exit_targ.h"
#include "calls.h"
#include "be_util.h"
#include "data_layout.h"
#include "stblock.h"
#include "cgtarget.h"
#include "whirl2ops.h"
#include "cgexp.h"
#include "cxx_template.h" // for STACK
#include "cg_spill.h"
#include "cg.h"           // for PU_References_GOT

static ST* save_ebx_loc = NULL;
static INT32 last_pu_count = -1;
// Whether we need a function to retrieve current instruction pointer
static BOOL need_ip = FALSE;
// Function to retrieve current instruction pointer
const char * ip_calc_funcname = "getip.pathscale";

void EETARG_Generate_PIC_Exit_Code( BB* bb, OPS* ops )
{
  if( save_ebx_loc == NULL )
    return;

  CGSPILL_Load_From_Memory( Ebx_TN(), save_ebx_loc, ops, CGSPILL_LCL, bb );
  Set_OP_computes_got( OPS_last(ops) );
}


void EETARG_Generate_PIC_Entry_Code( BB* bb, OPS* ops )
{
  FmtAssert( BB_entry(bb), ("BB is not entry") );
  FmtAssert( Is_Target_32bit(), ("PIC entry for -m32 only") );

  if( BB_handler( bb ) )
    return;

  TN* ebx_tn = Ebx_TN();

  if( REGISTER_allocatable( TN_register_class(ebx_tn),
			    TN_register(ebx_tn) ) ){
    save_ebx_loc = NULL;
    return;
  }

  if( last_pu_count != Current_PU_Count() ){
    save_ebx_loc = CGSPILL_Get_TN_Spill_Location( ebx_tn, CGSPILL_LCL );
    last_pu_count = Current_PU_Count();
  }

  /* Put saved location info of %ebx for dwarf generation when gra is not running.
     (bug#2676)
   */
  {
    SAVE_REG_LOC sr;
    extern STACK<SAVE_REG_LOC> Saved_Callee_Saved_Regs;

    sr.user_allocated = FALSE;
    sr.ded_tn = ebx_tn;
    sr.temp = save_ebx_loc;
    Saved_Callee_Saved_Regs.Push(sr);
  }

  CGSPILL_Store_To_Memory( ebx_tn, save_ebx_loc, ops, CGSPILL_LCL, bb );
  //Set_OP_no_move_before_gra( OPS_last(ops) );

  // Create BB #1.
  BB* call_bb = Gen_And_Insert_BB_Before(bb);
  BB_Transfer_Entryinfo( bb, call_bb );
  Set_BB_after_pic_entry(bb); // bb is original entry and now it's after the pic entry  
  REGION_First_BB = call_bb;
  Entry_BB_Head = BB_LIST_Delete( bb, Entry_BB_Head );
  Entry_BB_Head = BB_LIST_Push( call_bb, Entry_BB_Head, &MEM_pu_pool );
  Chain_BBs( call_bb, bb );
  Link_Pred_Succ_with_Prob( call_bb, bb, 1.0 );
  BB_rid( call_bb ) = BB_rid( bb );
  BB_freq( call_bb ) = BB_freq( bb );
  if( BB_freq_fb_based( bb ) )
    Set_BB_freq_fb_based( call_bb );

  // Create BB #2.
  BB* bb2 = Gen_And_Insert_BB_Before(bb);
  Chain_BBs(bb2, bb);
  Link_Pred_Succ_with_Prob(bb2, bb, 1.0);
  BB_rid(bb2) = BB_rid(bb);
  BB_freq(bb2) = BB_freq(bb);
  if (BB_freq_fb_based(bb))
    Set_BB_freq_fb_based(bb2);
  Change_Succ(call_bb, bb, bb2);

  PU_IDX pu_idx;
  PU& pu = New_PU (pu_idx);
  ST* st = New_ST( GLOBAL_SYMTAB );
  TY_IDX func_ty_idx;
  TY &func_ty = New_TY(func_ty_idx);
  TY_Init( func_ty, 0, KIND_FUNCTION, MTYPE_UNKNOWN, STR_IDX_ZERO );
  Set_TY_align( func_ty_idx, 1 );

  TYLIST tylist_idx;
  Set_TYLIST_type( New_TYLIST(tylist_idx), MTYPE_To_TY(Pointer_Mtype) );
  Set_TY_tylist( func_ty, tylist_idx );
  Set_TYLIST_type( New_TYLIST (tylist_idx), 0 );

  Set_TY_has_prototype(func_ty_idx);

  PU_Init( pu, func_ty_idx, CURRENT_SYMTAB );

  if (Is_Target_EM64T()    ||
      Is_Target_Wolfdale() ||
      Is_Target_Core())
  {
    const LABEL_IDX bb_label = Gen_Label_For_BB(bb2);

    ST_Init( st, Save_Str(LABEL_name(bb_label)),
             CLASS_FUNC, SCLASS_EXTERN,
             EXPORT_PREEMPTIBLE,
             TY_IDX(pu_idx) );

    CALLINFO* call_info = TYPE_PU_ALLOC(CALLINFO);
    CALLINFO_call_st(call_info) = st;
    CALLINFO_call_wn(call_info) = WN_Call(MTYPE_V, MTYPE_V, 0, st);
    BB_Add_Annotation( call_bb, ANNOT_CALLINFO, call_info );
    Set_BB_call( call_bb );

    TN* label_tn = Gen_Label_TN( bb_label, 0 );
    Exp_Call( OPR_CALL, NULL, label_tn, ops );
    BB_Append_Ops( call_bb, ops );

    OPS_Remove_All( ops );

    Build_OP( TOP_popl, ebx_tn, SP_TN, SP_TN, ops );
    Set_OP_computes_got( OPS_last(ops) );
    Exp_ADD( Pointer_Mtype,
             ebx_tn, ebx_tn,
             Gen_Symbol_TN(st, 0, TN_RELOC_IA32_GLOBAL_OFFSET_TABLE), ops );
    Set_OP_computes_got( OPS_last(ops) );
    BB_Append_Ops(bb2, ops);
  }
  else // AMD architectures like Opteron.
  {
    // Create BB #3.
    BB *bb3 = Gen_And_Insert_BB_Before(bb);
    Chain_BBs(bb3, bb);
    Link_Pred_Succ_with_Prob(bb3, bb, 1.0 );
    BB_rid(bb3) = BB_rid(bb);
    BB_freq(bb3) = BB_freq(bb);
    if (BB_freq_fb_based(bb))
      Set_BB_freq_fb_based(bb3);
    Change_Succ(bb2, bb, bb3);

    // Generate a call to a function to retrieve current instruction pointer.
    ST_Init( st, Save_Str(ip_calc_funcname),
             CLASS_FUNC, SCLASS_TEXT,
             EXPORT_LOCAL,
             TY_IDX(pu_idx) );

    Allocate_Object (st);
    CALLINFO* call_info = TYPE_PU_ALLOC(CALLINFO);
    CALLINFO_call_st(call_info) = st;
    CALLINFO_call_wn(call_info) = WN_Call(MTYPE_V, MTYPE_V, 0, st);
    BB_Add_Annotation( call_bb, ANNOT_CALLINFO, call_info );
    Set_BB_call( call_bb );

    TN* tgt_tn = Gen_Symbol_TN(st, 0, 0);
    Exp_Call( OPR_CALL, RA_TN, tgt_tn, ops );
    BB_Append_Ops( call_bb, ops );

    OPS_Remove_All( ops );

    // Create ADD.  Put it in a BB by itself to avoid LRA, which might
    // otherwise insert spill OPs before the ADD (bug 14452).
    Exp_ADD(Pointer_Mtype,
            ebx_tn, ebx_tn,
            Gen_Symbol_TN(st, 0, TN_RELOC_IA32_GLOBAL_OFFSET_TABLE), ops);
    Set_OP_computes_got(OPS_last(ops));
    BB_Append_Ops(bb2, ops);
    need_ip = TRUE;
  }

  OPS_Remove_All(ops);

  return;
}


void
EETARG_Save_Pfs (TN *saved_pfs, OPS *ops)
{
}


void
EETARG_Restore_Pfs (TN *saved_pfs, OPS *ops)
{
}


void
EETARG_Adjust_SP_For_Entry( TN *incr, OPS *ops )
{
  FmtAssert(FALSE, ("NYI: EETARG_Adjust_SP_For_Entry"));
}

void
EETARG_Adjust_SP_For_Exit( TN *incr, OPS *ops )
{
  FmtAssert(FALSE, ("NYI: EETARG_Adjust_SP_For_Exit"));
}

void
EETARG_Fixup_Entry_Code (BB *bb)
{
}

void
EETARG_Init_Entry_Exit_Code (WN *pu_wn, BOOL need_frame_pointer)
{
}

void EETARG_Save_Extra_Callee_Tns (OPS *ops)
{
}

void EETARG_Restore_Extra_Callee_Tns (OPS *ops)
{
}


OP *
EETARG_Build_Jump_Instead_Of_Call (OP *call_op)
{
  const TOP jump_top = OP_ijump(call_op) ? TOP_ijmp : TOP_jmp;
  OP* jump_op = Mk_OP(jump_top, OP_opnd(call_op, 0));

  Is_True(OP_opnds(call_op) == 1, ("unexpected number of call opnds"));
  Is_True(OP_opnds(jump_op) == 1, ("unexpected number of jump opnds"));

  return jump_op;
}

OP* EETARG_High_Level_Procedure_Exit() 
{ 
  // This functions is called after LRA. So, need to name our TNs.
  TN *result = Build_Dedicated_TN( ISA_REGISTER_CLASS_integer, RSP, 0 );
  TN *src = Build_Dedicated_TN( ISA_REGISTER_CLASS_integer, RBP, 0 );
  return Mk_OP(TOP_leave, result, src); 
}

// Code to emit function for retrieving current instruction pointer.
extern void CGEMIT_Setup_IP_Calc(void);

void EETARG_Emit_IP_Calc_Func (void)
{
  if (need_ip)
    CGEMIT_Setup_IP_Calc();
}
