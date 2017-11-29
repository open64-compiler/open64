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

void
EETARG_Save_Pfs (TN *saved_pfs, OPS *ops)
{
	// generate alloc stmt
	Build_OP (TOP_alloc, saved_pfs, 
		// use zero values initially, then fix up at end.
		Gen_Literal_TN(0, 4),
		Gen_Literal_TN(0, 4),
		Gen_Literal_TN(0, 4),
		Gen_Literal_TN(0, 4),
		ops);
}


void
EETARG_Restore_Pfs (TN *saved_pfs, OPS *ops)
{
	// restore pfs
	Build_OP (TOP_mov_t_ar_r_i, Pfs_TN, True_TN, saved_pfs, ops);
}

void
EETARG_Call_Mcount(BB *bb)
{
	if (Call_Mcount) {
		// insert call of mcount at very beginning.
		// to avoid it confusing our analysis of code,
		// use asm string.
		// add space for mcount info:
		
		char data_name[20];
		sprintf(data_name, "mcount_data_%d", Current_PU_Count());
		ST *data_st = New_ST (GLOBAL_SYMTAB);
		ST_Init (data_st, Save_Str(data_name),
			CLASS_VAR, SCLASS_FSTATIC, 
			EXPORT_LOCAL, MTYPE_To_TY(Pointer_Mtype));
		// Set_ST_not_gprel(data_st);
		Set_ST_is_initialized(data_st);
		INITO_IDX ino = New_INITO(data_st);
		INITV_IDX inv = New_INITV();
		INITV_Init_Integer (inv, Pointer_Mtype, 0);
		Set_INITO_val(ino, inv);
		Allocate_Object(data_st);
		// create asm string:
		// see gcc/config/ia64/linux.h for parameter description.
		char asm_string[256];
		ST *base;
		INT64 ofst;
		Base_Symbol_And_Offset (data_st, &base, &ofst);
		sprintf(asm_string, "%s%s(%s+%lld)%s", 
			"\talloc out0 = ar.pfs, 8, 0, 4, 0\n"
			"\taddl out3 = @",
		    	(ST_gprel(base) ? "gprel" : "ltoff"),
			ST_name(base), ofst,
			", gp;;\n"
       			"\tmov out1 = gp\n"
			"\tmov out2 = b0\n"
			"\tbr.call.sptk.many b0 = _mcount;;\n");
  		// now create ASM op:
		TN* result[10];
		TN* opnd[10];
		OP* asm_op = Mk_VarOP(TOP_asm, 0, 0, result, opnd);
		Set_OP_volatile(asm_op);
		
		ASM_OP_ANNOT* asm_info = TYPE_PU_ALLOC(ASM_OP_ANNOT);
		bzero(asm_info, sizeof(ASM_OP_ANNOT));
		WN *asm_wn = WN_CreateAsm_Stmt (0, asm_string);
		ASM_OP_wn(asm_info) = asm_wn;
		OP_MAP_Set(OP_Asm_Map, asm_op, asm_info);
		BB_Prepend_Op (bb, asm_op);

	}
}

void
EETARG_Fixup_Entry_Code (BB *bb)
{
        // find alloc and replace values
	OP *op;
	INT num_output;
	INT num_local;
	INT num_rotating;
	FOR_ALL_BB_OPs_FWD(bb, op) {
		if (OP_code(op) != TOP_alloc) continue;
		num_local = REGISTER_Number_Stacked_Local(
			ISA_REGISTER_CLASS_integer);
		num_output = REGISTER_Number_Stacked_Output(
			ISA_REGISTER_CLASS_integer);
		num_rotating = REGISTER_Number_Stacked_Rotating(
			ISA_REGISTER_CLASS_integer);
		// set num_rotating not larg than num_local to prevent function
		// call parameter error.
		num_local = MAX(num_local, num_rotating);

		if (num_local + num_output + num_rotating == 0) {
			BB_Remove_Op (bb, op);
		}
		else {
			Set_OP_opnd(op, 1, Gen_Literal_TN(num_local, 4));
			Set_OP_opnd(op, 2, Gen_Literal_TN(num_output, 4));
			Set_OP_opnd(op, 3, Gen_Literal_TN(num_rotating, 4));
		}
		return;
	}
}

void
EETARG_Init_Entry_Exit_Code (WN *pu_wn, BOOL need_frame_pointer)
{
  // We now use r7 rather than a stacked reg for FP,
  // but keep this code just in case we ever want to go back to it.
  if (need_frame_pointer && REGISTER_fp == REGISTER_UNDEFINED) {
	// Create fp as stacked register.
	// But first must allocate all the formal stacked regs.
	PLOC ploc;
	INT i;
	ISA_REGISTER_CLASS rclass;
	REGISTER reg;
	ploc = Setup_Input_Parameter_Locations(ST_pu_type(WN_st(pu_wn)));
	for (i = 0; i < WN_num_formals(pu_wn); i++) {
		ploc = Get_Input_Parameter_Location (
			TY_Of_Parameter(WN_formal(pu_wn,i)));
		if ( ! PLOC_on_stack(ploc)
			&& CGTARG_Preg_Register_And_Class(
				PLOC_reg(ploc), &rclass, &reg) )
		{
			if (ABI_PROPERTY_Is_stacked(
				rclass,
				REGISTER_machine_id(rclass, reg) ))
			{
				reg = REGISTER_Allocate_Stacked_Register(
					ABI_PROPERTY_callee, rclass, reg);
			}
		}
	}
	if (TY_is_varargs (ST_pu_type(WN_st(pu_wn)))) {
		// use up remaining reg params
		if ( PLOC_is_nonempty(ploc) && ! PLOC_on_stack(ploc)) {
			ploc = Get_Vararg_Input_Parameter_Location (ploc);
		}
		while ( ! PLOC_on_stack(ploc)) {
			CGTARG_Preg_Register_And_Class(
				PLOC_reg(ploc), &rclass, &reg);
			if (ABI_PROPERTY_Is_stacked(
				rclass,
				REGISTER_machine_id(rclass, reg) ))
			{
				reg = REGISTER_Allocate_Stacked_Register(
					ABI_PROPERTY_callee, rclass, reg);
			}
			ploc = Get_Vararg_Input_Parameter_Location (ploc);
		}
	}
	FP_TN = Build_Dedicated_TN (ISA_REGISTER_CLASS_integer,
		REGISTER_Request_Stacked_Register(ABI_PROPERTY_frame_ptr,
			ISA_REGISTER_CLASS_integer), 0);
  }
}

void EETARG_Save_Extra_Callee_Tns (OPS *ops)
{
	// save predicates
	// This is handled differently cause is cheaper to just
	// save and restore the whole bank of predicates in one instruction.
	// Note that we assume GRA will remove the predicate save/restore
	// if no callee-save predicates are used.

	TN *callee_tn = CALLEE_tn(Callee_Saved_Regs_Count);
	// save callee_tn in callee-saved-regs array;
	// this works cause originally allocated space for all regs,
	// yet only use space for callee-save (so available space).
	// could get broken if ever allocated minimal space originally.
	if (callee_tn == NULL) {
		callee_tn = Build_RCLASS_TN(ISA_REGISTER_CLASS_integer);
		Set_TN_is_gra_cannot_split(callee_tn);
		CALLEE_tn(Callee_Saved_Regs_Count) = callee_tn;
	}
	Build_OP (TOP_mov_f_pr, callee_tn, True_TN, ops);
	Set_OP_no_move_before_gra(OPS_last(ops));
}

void EETARG_Restore_Extra_Callee_Tns (OPS *ops)
{
	// restore all predicates
	TN *callee_tn = CALLEE_tn(Callee_Saved_Regs_Count);
	Build_OP (TOP_mov_t_pr, True_TN, callee_tn, Gen_Literal_TN(-1, 4), ops);
	Set_OP_no_move_before_gra(OPS_last(ops));
}


OP *
EETARG_Build_Jump_Instead_Of_Call (OP *call_op)
{
  OP *jump_op;
  TOP jump_top;
  TOP call_top = OP_code(call_op);
  switch (call_top) {
  case TOP_br_r_call:
    jump_top = TOP_br_r_cond;
    break;
  case TOP_br_call:
    jump_top = TOP_br_cond;
    break;
  default:
    FmtAssert(FALSE, ("don't know how to generate tail call for %s",
		     TOP_Name(call_top)));
    /*NOTREACHED*/
  }

  Is_True(OP_opnds(call_op) == 6, ("unexpected number of call opnds"));
  jump_op = Mk_OP(jump_top, OP_opnd(call_op, 0),
			    OP_opnd(call_op, 1),
			    OP_opnd(call_op, 2),
			    OP_opnd(call_op, 3),
			    OP_opnd(call_op, 4));
  Is_True(OP_opnds(jump_op) == 5, ("unexpected number of jump opnds"));
  return jump_op;
}
