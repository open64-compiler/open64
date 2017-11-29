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


#include <stdio.h>
#include <stdlib.h>
#include <elf_stuff.h>
#include <elfaccess.h>
#include <libelf/libelf.h>
#include <sys/unwindP.h>
#include <list>

#include "defs.h"
#include "erglob.h"
#include "glob.h"
#include "flags.h"
#include "tracing.h"
#include "config.h"
#include "config_asm.h"
#include "be_util.h"
#include "cgir.h"
#include "register.h"
#include "tn_map.h"
#include "em_elf.h"
#include "em_dwarf.h"
#include "cgtarget.h"
#include "calls.h"
#include "cgemit.h"
#include "data_layout.h"
#include "cgdwarf_targ.h"

static BOOL Trace_Unwind = FALSE;

// rp == ra
// psp == fp
 
// Procedure regions:
typedef enum {
  UNDEFINED_UREGION,
  PROLOGUE_UREGION,
  EPILOGUE_BODY_UREGION,
  LABEL_BODY_UREGION,
  COPY_BODY_UREGION
} UREGION_TYPE;

// Unwind elements:
enum {
  UE_UNDEFINED,
  UE_CREATE_FRAME,
  UE_DESTROY_FRAME,
  UE_SAVE_GR,		// save a reg to a GR reg
  UE_SAVE_SP,		// save a reg to memory (sp)
  UE_SAVE_PSP,		// save a reg to memory (psp)
  UE_RESTORE_GR,	// restore a reg from a GR reg
  UE_RESTORE_MEM,	// restore a reg from memory
  UE_EPILOG,		// body epilog
  UE_LABEL,		// body label
  UE_COPY		// body copy 
};

typedef struct unwind_elem {
  mUINT32 when;
  BB *bb;
  mUINT8 kind;
  mUINT8 qp;			// reg number of qualifying predicate
  mUINT16 label;		// body label id
  CLASS_REG_PAIR rc_reg;	// reg whose state is changing
  CLASS_REG_PAIR save_rc_reg;	// reg being saved into
  union {
  	mINT64 offset;			// stack offset
	struct {
		mUINT32 size;		// region size
		mUINT32 start;		// when at start of region
	} region;
  } u;
} UNWIND_ELEM;

// use list not slist cause append to end
static std::list < UNWIND_ELEM > ue_list;
static std::list < UNWIND_ELEM >::iterator ue_iter;
static UINT last_when;
static BOOL simple_unwind = FALSE;
static BOOL has_asm = FALSE;
static UINT last_label = 0;
static BOOL has_create = FALSE;
static TN_MAP tn_def_op;

static const char *
UE_Register_Name (ISA_REGISTER_CLASS rc, REGISTER r)
{
	if (rc == ISA_REGISTER_CLASS_branch && r == REGISTER_MIN+0)
		return "rp";	// b0 is called rp in unwind directives
	else
		return REGISTER_name(rc,r);
}

static void
Print_Unwind_Elem (UNWIND_ELEM ue, char *msg)
{
	fprintf(TFile, "<%s> at bb %d when %d: ", msg, ue.bb->id, ue.when);
	switch (ue.kind) {
	case UE_CREATE_FRAME:
		fprintf(TFile, " create_frame, size %d, start %d", 
			ue.u.region.size, ue.u.region.start); 
		break;
	case UE_DESTROY_FRAME:
		fprintf(TFile, " destroy_frame");
		break;
	case UE_EPILOG:
		fprintf(TFile, " body epilog, size %d, start %d",
			ue.u.region.size, ue.u.region.start); 
		break;
	case UE_LABEL:  
		fprintf(TFile, " label state %d, size %d, start %d",
			ue.label, ue.u.region.size, ue.u.region.start);
		break;
	case UE_COPY:  
		fprintf(TFile, " copy label state %d, size %d, start %d",
			ue.label, ue.u.region.size, ue.u.region.start);
		break;
	case UE_SAVE_GR:  
		fprintf(TFile, " save %s in gr %s",
		    UE_Register_Name(
			CLASS_REG_PAIR_rclass(ue.rc_reg),
        		CLASS_REG_PAIR_reg(ue.rc_reg) ),
		    UE_Register_Name(
			CLASS_REG_PAIR_rclass(ue.save_rc_reg),
        		CLASS_REG_PAIR_reg(ue.save_rc_reg) ));
		break;
	case UE_SAVE_SP:
	case UE_SAVE_PSP:
		fprintf(TFile, "save %s", UE_Register_Name(
				CLASS_REG_PAIR_rclass(ue.rc_reg),
        			CLASS_REG_PAIR_reg(ue.rc_reg) ));
		fprintf(TFile, " to mem %lld(%s)",
			ue.u.offset,
			(ue.kind == UE_SAVE_SP ? "sp" : "psp") );
		break;
	case UE_RESTORE_GR:  
		fprintf(TFile, " restore %s", UE_Register_Name(
			CLASS_REG_PAIR_rclass(ue.rc_reg),
        		CLASS_REG_PAIR_reg(ue.rc_reg) ));
		break;
	case UE_RESTORE_MEM:  
		fprintf(TFile, " restore %s from mem", UE_Register_Name(
			CLASS_REG_PAIR_rclass(ue.rc_reg),
        		CLASS_REG_PAIR_reg(ue.rc_reg) ));
		break;
	}
	fprintf(TFile, "\n");
}

static void
Print_All_Unwind_Elem (char *msg)
{
  for (ue_iter = ue_list.begin(); ue_iter != ue_list.end(); ++ue_iter) {
    	Print_Unwind_Elem (*ue_iter, msg);
  }
}

static BOOL
TN_Is_Unwind_Reg (TN *tn)
{
	if (REGISTER_SET_MemberP(
		REGISTER_CLASS_callee_saves(TN_register_class(tn)),
		TN_register(tn)))
	{
		return TRUE;
	}
	else if (CLASS_REG_PAIR_EqualP(TN_class_reg(tn), CLASS_REG_PAIR_ra)) {
		return TRUE;
	}
	else if (CLASS_REG_PAIR_EqualP(TN_class_reg(tn), CLASS_REG_PAIR_pfs)) {
		return TRUE;
	}
	return FALSE;
}

static OP*
Find_Def_Of_TN (TN *tn, OP *last_op)
{
	// Initially tried to use TN_Reaching_Value_At_Op,
	// but that misses some cases cause the live-in sets are not
	// kept up-to-date by cgemit time.  Also requires a lot
	// of traversing the bb and op lists.
	// So instead, just create map of tn to defining op,
	// which is a little more space but less time
	// and is more accurate.
	OP *op = (OP*) TN_MAP_Get (tn_def_op, tn);
	if ( op == NULL && REGISTER_SET_MemberP(
			 REGISTER_CLASS_function_value(TN_register_class(tn)),
       		     	 TN_register(tn) ) )
	{
		// okay if store of function return val
		return NULL;
	}
	if (op == NULL && Trace_Unwind)
		fprintf(TFile, "couldn't find def of tn %d\n", TN_number(tn));
	return op;
}

static TN*
Find_Spill_TN (OP *last_op, TN *reg_tn)
{
	// search backwards to find spill symbol stored in reg.
	OP *op = Find_Def_Of_TN (reg_tn, last_op);
	if (op == NULL) return NULL;
	if (OP_code(op) == TOP_adds) {
		if (TN_is_symbol(OP_opnd(op,1)))
			return OP_opnd(op,1);
	}
	else if (OP_code(op) == TOP_add
		&& (OP_opnd(op,2) == FP_TN || OP_opnd(op,2) == SP_TN))
	{
		op = Find_Def_Of_TN (OP_opnd(op,1), op);
		if (op && OP_code(op) == TOP_movl) {
			if (TN_is_symbol(OP_opnd(op,1)))
				return OP_opnd(op,1);
		}
		else if (op && OP_code(op) == TOP_mov_i) {
			if (TN_is_symbol(OP_opnd(op,1)))
				return OP_opnd(op,1);
		}
	}
	return NULL;
}

// search for save-tn that "tn" is copied from.
static TN*
Get_Copied_Save_TN (TN *tn, OP *cur_op)
{
	// might already be a save-tn
	if (TN_is_save_reg(tn))
		return tn;
	if (TN_is_dedicated(tn))
		return NULL;
	// else find tn that this is a copy of.
	OP *op = Find_Def_Of_TN (tn, cur_op);
	if (!op) return NULL;
	TN *otn = CGTARG_Copy_Operand_TN(op);
	if (!otn) {
		if (OP_code(op) == TOP_alloc)
			otn = OP_result(op,0);
		if (OP_code(op) == TOP_mov_f_br)
			otn = OP_opnd(op,1);
	}
	if (otn && TN_is_save_reg(otn))
		return otn;
	else 
		return NULL;
}

// find tn that we are restoring from
// (go past temporary tn that holds address)
static TN*
Get_Source_Of_Restore (TN *tn, OP *cur_op)
{
	// if callee-save or stacked, just return
	if (REGISTER_SET_MemberP(
		REGISTER_CLASS_callee_saves(TN_register_class(tn)),
		TN_register(tn)))
	{
		return tn;
	}
	if (REGISTER_Is_Stacked(TN_register_class(tn), TN_register(tn))) {
		return tn;
	}
	// else search for address
	OP *op = Find_Def_Of_TN (tn, cur_op);
	if (!op) return tn;
	if (OP_load(op)) {
		// find spill-tn by looking backwards
		TN *spill_tn = Find_Spill_TN (op, 
	    		OP_opnd(op, TOP_Find_Operand_Use(OP_code(op), OU_base) ));
		if ( spill_tn) {
			return spill_tn;
		}
	}
	return tn;
}

static void
Analyze_OP_For_Unwind_Info (OP *op, UINT when, BB *bb)
{
  UNWIND_ELEM ue;
  ue.kind = UE_UNDEFINED;
  TN *tn;
  TOP opc = OP_code(op);

  FmtAssert(!OP_simulated(op) || opc== TOP_asm, 
	("Found a simulated OP: %s",TOP_Name(opc)));
  if (opc == TOP_asm) {
	has_asm = TRUE;
  }

  if (OP_has_result(op) && OP_result(op,0) == SP_TN) {
    ue.rc_reg = CLASS_REG_PAIR_sp;
    if (BB_entry(bb) && op == BB_entry_sp_adj_op (bb)) {
	ue.kind = UE_CREATE_FRAME;
    }
    else if (BB_exit(bb) && op == BB_exit_sp_adj_op (bb)) {
	ue.kind = UE_DESTROY_FRAME;
    }
  } 
  else if (OP_has_result(op) && OP_result(op,0) == FP_TN) {
    // even though is adjusting fp, want to pretend it is sp adjustment
    ue.rc_reg = CLASS_REG_PAIR_sp;
    if (BB_entry(bb) && op == BB_entry_sp_adj_op (bb)) {
	ue.kind = UE_CREATE_FRAME;
    }
    else if (BB_exit(bb) && op == BB_exit_sp_adj_op (bb)) {
	ue.kind = UE_DESTROY_FRAME;
    }
  } 
  if (ue.kind == UE_CREATE_FRAME) {
	if (has_create) {
		// Assume create frame will only appear once.
		// Altentries can have multiple create_frames,
		// but in that case the frame size is same for all
		// entry points, so can ignore subsequent entries.
		ue.kind = UE_UNDEFINED;
	}
	else has_create = TRUE;
  }

  if (ue.kind != UE_UNDEFINED) 
	;	// already found
  	// else might be something like restore of r7/fp
  else if (opc == TOP_alloc) {
    ue.kind = UE_SAVE_GR;
    ue.rc_reg = CLASS_REG_PAIR_pfs;
    ue.save_rc_reg = TN_class_reg(OP_result(op, 0));
  } 
  else if ((opc == TOP_mov_t_ar_r_i 
	 || opc == TOP_mov_t_ar_r_m)
	     && TN_register(OP_result(op,0)) == REGISTER_pfs)
  {
    ue.rc_reg = CLASS_REG_PAIR_pfs;
    ue.kind = UE_RESTORE_GR;
  }
  // can have either rx = mov_f_br (b0 sv:b0)
  // or (rx sv:b0) = mov_f_br b0
  else if (opc == TOP_mov_f_br 
	     && TN_is_save_reg(OP_opnd(op,1))
	     && TN_save_reg(OP_opnd(op,1)) == REGISTER_ra)
  {
    ue.kind = UE_SAVE_GR;
    ue.rc_reg = CLASS_REG_PAIR_ra;
    ue.save_rc_reg = TN_class_reg(OP_result(op, 0));
  } 
  else if (opc == TOP_mov_f_br 
	     && TN_is_save_reg(OP_result(op,0))
	     && TN_save_reg(OP_result(op,0)) == REGISTER_ra)
  {
    ue.kind = UE_SAVE_GR;
    ue.rc_reg = CLASS_REG_PAIR_ra;
    ue.save_rc_reg = TN_class_reg(OP_result(op, 0));
  } 
  else if (opc == TOP_mov_t_br 
	     && TN_is_save_reg(OP_result(op,0))
	     && TN_save_reg(OP_result(op,0)) == REGISTER_ra)
  {
    ue.rc_reg = CLASS_REG_PAIR_ra;
    ue.kind = UE_RESTORE_GR;
  } 
  else if (opc == TOP_mov_t_br 
	     && TN_is_save_reg(OP_opnd(op,1))
	     && TN_save_reg(OP_opnd(op,1)) == REGISTER_ra)
  {
    ue.rc_reg = CLASS_REG_PAIR_ra;
    ue.kind = UE_RESTORE_GR;
  } 
  else if ((opc == TOP_mov_f_ar_i || opc == TOP_mov_f_ar_m)
	     && TN_is_save_reg(OP_result(op,0)))
  {
    ue.kind = UE_SAVE_GR;
    ue.rc_reg = TN_save_creg(OP_result(op,0));
    ue.save_rc_reg = TN_class_reg(OP_result(op, 0));
  }
  else if ((opc == TOP_mov_t_ar_r_i || opc == TOP_mov_t_ar_r_m)
	     && TN_is_save_reg(OP_opnd(op,1)))
  {
    ue.kind = UE_RESTORE_GR;
    ue.rc_reg = TN_save_creg(OP_opnd(op,1));
  }
  else if (opc == TOP_mov
	     && TN_is_save_reg(OP_result(op,0)) )
  {
    ue.kind = UE_SAVE_GR;
    ue.rc_reg = TN_save_creg(OP_result(op,0));
    ue.save_rc_reg = TN_class_reg(OP_result(op, 0));
  } 
  else if (opc == TOP_mov
	     && TN_is_save_reg(OP_opnd(op,1)) )
  {
    ue.kind = UE_RESTORE_GR;
    ue.rc_reg = TN_save_creg(OP_opnd(op,1));
  } 
  else if (OP_store(op)) {
    // find def of storeval and see if it is copy of save reg.
    // also check if base comes from spill symbol.
    TN *save_tn;
    INT opndnum = TOP_Find_Operand_Use(opc, OU_storeval);
    FmtAssert(opndnum >= 0, ("no OU_storeval for %s", TOP_Name(opc)));
    save_tn = Get_Copied_Save_TN(OP_opnd(op,opndnum), op);
    if (save_tn) {
	opndnum = TOP_Find_Operand_Use(opc, OU_base);
	FmtAssert(opndnum >= 0, ("no OU_base for %s", TOP_Name(opc)));
	ue.rc_reg = TN_save_creg(save_tn);
	TN *store_tn = OP_opnd(op,opndnum);
	if (store_tn == SP_TN) {
		ue.kind = UE_SAVE_SP;
		ue.u.offset = 0;
	}
	else if (store_tn == FP_TN) {
		ue.kind = UE_SAVE_PSP;
		ue.u.offset = 0;
	} 
	else {
		TN *spill_tn = Find_Spill_TN (op, store_tn);
		if ( ! spill_tn) {
			DevWarn("unwind: save but no spill-tn for TN %d",
				TN_number(store_tn));
			return;
		}
		ST *base; INT64 offset;
		Base_Symbol_And_Offset (TN_var(spill_tn), &base, &offset);
		ue.u.offset = offset;
		ue.kind = (base == SP_Sym ? UE_SAVE_SP : UE_SAVE_PSP);
	}
    }
  }
  else if (OP_load(op)) {
    if (TN_is_save_reg(OP_result(op,0)) && TN_Is_Unwind_Reg(OP_result(op,0))) {
    	ue.kind = UE_RESTORE_MEM;
    	ue.rc_reg = TN_class_reg(OP_result(op,0));
    }
  }

  if (ue.kind != UE_UNDEFINED) {
	ue.qp = 0;
	if (OP_has_predicate(op)) {
		ue.qp = REGISTER_machine_id (ISA_REGISTER_CLASS_predicate,
			TN_register(OP_opnd(op, OP_PREDICATE_OPND)) );
	}
	if (ue.kind == UE_SAVE_GR) {
		ISA_REGISTER_CLASS rc = CLASS_REG_PAIR_rclass(ue.save_rc_reg);
		REGISTER reg = CLASS_REG_PAIR_reg(ue.save_rc_reg);
		if (ABI_PROPERTY_Is_stacked(rc, REGISTER_machine_id(rc, reg))
			&& ! BB_rotating_kernel(bb)
        		&& REGISTER_Is_Stacked_Output(rc, reg) )
		{
			reg = REGISTER_Translate_Stacked_Output(reg);
			Set_CLASS_REG_PAIR_reg(ue.save_rc_reg, reg);
		}
	}
	ue.when = when;
	ue.bb = bb;
	ue_list.push_back (ue);
    	if (Trace_Unwind) Print_Unwind_Elem (ue, "unwind1");
  }
}

static UINT
Get_BB_When_Length (BB *bb) 
{
  UINT w = 0;
  OP *op;
  FOR_ALL_BB_OPs_FWD(bb, op) {
  	if (OP_dummy(op)) continue;
  	w += OP_Real_Inst_Words(op);
  }
  return w;
}

static void
Find_Unwind_Info (void)
{
  OP *op;
  UINT when = 0;
  BB_Mark_Unreachable_Blocks ();

  // Create mapping of TN to defining OP.
  // This is used to find spill-tns, 
  // cause add and store may be in separate blocks.
  // TN_Reaching_Value_At_Op() doesn't catch all cases
  // (partly cause live-in/out sets not up-to-date),
  // so instead we do brute-force mapping.
  tn_def_op = TN_MAP_Create();
  for (BB *bb = REGION_First_BB; bb; bb = BB_next(bb)) {
    if (BB_unreachable(bb)) continue;
    FOR_ALL_BB_OPs_FWD(bb, op) {
	// if multiple stores, will take last one
	if (OP_has_result(op))
		TN_MAP_Set (tn_def_op, OP_result(op,0), op);
    }
  }

  for (BB *bb = REGION_First_BB; bb; bb = BB_next(bb)) {
    if (BB_unreachable(bb)) {
	when += Get_BB_When_Length(bb); 
	continue;
    }
    if (Trace_Unwind) Print_BB(bb);
    FOR_ALL_BB_OPs_FWD(bb, op) {
	if (OP_dummy(op)) continue;
	Analyze_OP_For_Unwind_Info(op, when, bb);
	when += OP_Real_Inst_Words(op);
    }
  }
  last_when = when - 1;
  TN_MAP_Delete (tn_def_op);
}



// enum of all preserved regs (PR) that can be saved/restored
typedef enum {
	PR_SP,
	PR_R4,
	PR_R5,
	PR_R6,
	PR_R7,
	PR_F2,
	PR_F3,
	PR_F4,
	PR_F5,
	PR_F16,
	PR_F17,
	PR_F18,
	PR_F19,
	PR_F20,
	PR_F21,
	PR_F22,
	PR_F23,
	PR_F24,
	PR_F25,
	PR_F26,
	PR_F27,
	PR_F28,
	PR_F29,
	PR_F30,
	PR_F31,
	PR_PRED,
	PR_RP,
	PR_B1,
	PR_B2,
	PR_B3,
	PR_B4,
	PR_B5,
	PR_PFS,
	PR_LC,
	PR_FPSR,
	PR_UNAT,
	PR_RNAT,
	PR_PRIUNAT,
	PR_BSP,
	PR_BSPSTORE,
	// note that ar.ec is marked as callee-save,
	// but is saved by hardware in ar.pfs so don't need unwind info.
	PR_LAST
} PR_TYPE;
#define PR_FIRST PR_SP
#define INCR(p)	(p = static_cast<PR_TYPE>(static_cast<INT>(p) +1))

static PR_TYPE
CR_To_PR (CLASS_REG_PAIR crp)
{
    ISA_REGISTER_CLASS rc = CLASS_REG_PAIR_rclass(crp);
    INT reg = CLASS_REG_PAIR_reg(crp) - REGISTER_MIN;
    switch (rc) {
    case ISA_REGISTER_CLASS_integer:
	if (reg == 12) return PR_SP;
	if (reg >= 4 && reg <= 7) return (PR_TYPE) (PR_R4 + (reg-4));
	break;
    case ISA_REGISTER_CLASS_float:
	if (reg >= 2 && reg <= 5) return (PR_TYPE) (PR_F2 + (reg-2));
	if (reg >= 16 && reg <= 31) return (PR_TYPE) (PR_F16 + (reg-16));
	break;
    case ISA_REGISTER_CLASS_branch:
	if (reg == 0) return PR_RP;
	if (reg >= 1 && reg <= 5) return (PR_TYPE) (PR_B1 + (reg-1));
	break;
    case ISA_REGISTER_CLASS_predicate:
	return PR_PRED;
	break;
    case ISA_REGISTER_CLASS_application:
	if (reg == 64) return PR_PFS;
	if (reg == 65) return PR_LC;
	if (reg == 40) return PR_FPSR;
	if (reg == 36) return PR_UNAT;
	if (reg == 19) return PR_RNAT;
	if (reg == 17) return PR_BSP;
	if (reg == 18) return PR_BSPSTORE;
	// TODO:  PR_PRIUNAT
	break;
    default:
	FmtAssert(FALSE, ("unexpected rclass in CR_To_PR"));
    }
    FmtAssert(FALSE, ("unexpected reg (%d,%d) in CR_To_PR", rc, reg));
}

static CLASS_REG_PAIR
PR_To_CR (PR_TYPE p)
{
  CLASS_REG_PAIR crp;
  switch (p) {
  case PR_SP:
	crp = CLASS_REG_PAIR_sp; break;
  case PR_R4:
  case PR_R5:
  case PR_R6:
  case PR_R7:
	Set_CLASS_REG_PAIR_rclass(crp, ISA_REGISTER_CLASS_integer);
	Set_CLASS_REG_PAIR_reg(crp, REGISTER_MIN+4 + p-PR_R4);
	break;
  case PR_F2:
  case PR_F3:
  case PR_F4:
  case PR_F5:
	Set_CLASS_REG_PAIR_rclass(crp, ISA_REGISTER_CLASS_float);
	Set_CLASS_REG_PAIR_reg(crp, REGISTER_MIN+2 + p-PR_F2);
	break;
  case PR_F16:
  case PR_F17:
  case PR_F18:
  case PR_F19:
  case PR_F20:
  case PR_F21:
  case PR_F22:
  case PR_F23:
  case PR_F24:
  case PR_F25:
  case PR_F26:
  case PR_F27:
  case PR_F28:
  case PR_F29:
  case PR_F30:
  case PR_F31:
	Set_CLASS_REG_PAIR_rclass(crp, ISA_REGISTER_CLASS_float);
	Set_CLASS_REG_PAIR_reg(crp, REGISTER_MIN+16 + p-PR_F16);
	break;
  case PR_RP:
	crp = CLASS_REG_PAIR_ra; break;
  case PR_B1:
  case PR_B2:
  case PR_B3:
  case PR_B4:
  case PR_B5:
	Set_CLASS_REG_PAIR_rclass(crp, ISA_REGISTER_CLASS_branch);
	Set_CLASS_REG_PAIR_reg(crp, REGISTER_MIN+1 + p-PR_B1);
	break;
  case PR_PFS:
	crp = CLASS_REG_PAIR_pfs; break;
  case PR_LC:
	crp = CLASS_REG_PAIR_lc; break;
  case PR_FPSR:
	Set_CLASS_REG_PAIR_rclass(crp, ISA_REGISTER_CLASS_application);
	Set_CLASS_REG_PAIR_reg(crp, REGISTER_MIN+40);
	break;
  case PR_UNAT:
	Set_CLASS_REG_PAIR_rclass(crp, ISA_REGISTER_CLASS_application);
	Set_CLASS_REG_PAIR_reg(crp, REGISTER_MIN+36);
	break;
  case PR_RNAT:
	Set_CLASS_REG_PAIR_rclass(crp, ISA_REGISTER_CLASS_application);
	Set_CLASS_REG_PAIR_reg(crp, REGISTER_MIN+19);
	break;
  case PR_BSP:
	Set_CLASS_REG_PAIR_rclass(crp, ISA_REGISTER_CLASS_application);
	Set_CLASS_REG_PAIR_reg(crp, REGISTER_MIN+17);
	break;
  case PR_BSPSTORE:
	Set_CLASS_REG_PAIR_rclass(crp, ISA_REGISTER_CLASS_application);
	Set_CLASS_REG_PAIR_reg(crp, REGISTER_MIN+18);
	break;
  case PR_PRED:
  case PR_PRIUNAT:
  default:
    FmtAssert(FALSE, ("unexpected pr (%d) in PR_To_CR", p));
  }
  return crp;
}

typedef UINT64 PR_BITSET;	// bit mask for PR enumeration
static inline BOOL Get_PR (PR_BITSET state, PR_TYPE p)
{
	return (BOOL) ((state >> p) & 1);
}
static inline BOOL Get_PR (PR_BITSET *state, BB *bb, PR_TYPE p)
{
	return (BOOL) ((state[bb->id] >> p) & 1);
}
static inline PR_BITSET Set_PR (PR_BITSET state, PR_TYPE p)
{
	return (state | (1LL << p));
}
static inline void Set_PR (PR_BITSET *state, BB *bb, PR_TYPE p)
{
	state[bb->id] |= (1LL << p);
}
static inline PR_BITSET Clear_PR (PR_BITSET state, PR_TYPE p)
{
	return (state & (~(1LL << p)));
}

// iterate thru list and mark all saves/restores in local state
static void
Mark_Local_Saves_Restores (PR_BITSET *local_save_state, 
	PR_BITSET *local_restore_state)
{
  PR_TYPE p;
  memset (local_save_state, 0, ((PU_BB_Count+1) * sizeof(PR_BITSET)));
  memset (local_restore_state, 0, ((PU_BB_Count+1) * sizeof(PR_BITSET)));

  for (ue_iter = ue_list.begin(); ue_iter != ue_list.end(); ++ue_iter) {
	switch (ue_iter->kind) {
	case UE_CREATE_FRAME:
	case UE_SAVE_GR:
	case UE_SAVE_SP:
	case UE_SAVE_PSP:
  		p = CR_To_PR (ue_iter->rc_reg);
  		Set_PR(local_save_state, ue_iter->bb, p);
		break;
	case UE_DESTROY_FRAME:
	case UE_RESTORE_GR:
	case UE_RESTORE_MEM:
  		p = CR_To_PR (ue_iter->rc_reg);
  		Set_PR(local_restore_state, ue_iter->bb, p);
		break;
	}
  }
}

static void
Propagate_Save_Restore_State (PR_BITSET *entry_state,
	PR_BITSET *local_save_state,
	PR_BITSET *local_restore_state,
	PR_BITSET *exit_state)
{
  BB *bb;
  BBLIST *blst;
  INT bbid;
  BOOL changed = TRUE;
  INT count = 0;
  memset (entry_state, 0, ((PU_BB_Count+1) * sizeof(PR_BITSET)));
  memset (exit_state, -1, ((PU_BB_Count+1) * sizeof(PR_BITSET)));

  while (changed) {
	++count;
	if (count > 150) {	// to avoid infinite loops
		DevWarn("infinite loop in propagating unwind info");
		break;
	}
	changed = FALSE;
	for (bb = REGION_First_BB; bb; bb = BB_next(bb)) {
		if (BB_unreachable(bb)) continue;
		if (Trace_Unwind) 
			fprintf (TFile, "curbb: %d, preds: ", BB_id(bb));
		if (BB_preds(bb) != NULL) {
			PR_BITSET new_entry_state = (PR_BITSET) -1; // all 1's
			FOR_ALL_BB_PREDS (bb, blst) {
				bbid = BB_id(BBLIST_item(blst));
				new_entry_state &= exit_state[bbid];
				if (Trace_Unwind) fprintf (TFile, "[%d %llx], ",
					bbid, exit_state[bbid]);
			}
        		entry_state[BB_id(bb)] = new_entry_state;
		}
		bbid = BB_id(bb);
		if (Trace_Unwind) 
			fprintf (TFile, "\n entry_state: %llx\n", entry_state[bbid]);
		// exit state bit is 1 if entry or local_save is 1
		// and local restore is 0.
		PR_BITSET new_exit_state = 
			entry_state[bbid] | local_save_state[bbid];
		new_exit_state &= ~local_restore_state[bbid];
		if (new_exit_state != exit_state[bbid]) {
			changed = TRUE;
			exit_state[bbid] = new_exit_state;
		}
	}
  }
  if (Trace_Unwind) {
	fprintf(TFile, "bb\tentry\t\tlocal_save\tlocal_restore\texit:\n");
	for (INT i = 1; i <= PU_BB_Count; ++i) {
		fprintf(TFile, "%4d:\t%12llx\t%12llx\t%12llx\t%12llx\n", 
			i, entry_state[i], local_save_state[i], local_restore_state[i], exit_state[i]);
	}
  }
}

static UNWIND_ELEM
Find_Prev_Save_UE_For_BB (std::list < UNWIND_ELEM > prev_ue, BB *bb, UINT level)
{
  BBLIST *blst;
  std::list < UNWIND_ELEM >::iterator prev_iter;
  FOR_ALL_BB_PREDS (bb, blst) {
	// find ue in nbb that does a save
  	for (prev_iter = prev_ue.begin(); prev_iter != prev_ue.end(); ++prev_iter) {
		if (prev_iter->bb != BBLIST_item(blst)) continue;
		if (prev_iter->kind == UE_SAVE_GR || prev_iter->kind == UE_SAVE_SP || prev_iter->kind == UE_SAVE_PSP) {
			return *prev_iter;
		}
	}
  }
  UNWIND_ELEM ue;
  ue.kind = UE_UNDEFINED;
  ++level;
  if (level > 10) return ue;
  // if not found recurse
  FOR_ALL_BB_PREDS (bb, blst) {
	ue = Find_Prev_Save_UE_For_BB (prev_ue, BBLIST_item(blst), level);
	if (ue.kind != UE_UNDEFINED) return ue;
  }
  return ue;
}

// overload some routines to add unwind elements
static void
Add_UE (std::list < UNWIND_ELEM > prev_ue, PR_TYPE p, UINT when, BB *bb)
{
  std::list < UNWIND_ELEM >::iterator prev_iter;
  UNWIND_ELEM ue;
  ue.kind = UE_UNDEFINED;
  UINT num_found = 0;
  for (prev_iter = prev_ue.begin(); prev_iter != prev_ue.end(); ++prev_iter) {
	// look for save
	if (prev_iter->kind == UE_SAVE_GR || prev_iter->kind == UE_SAVE_SP || prev_iter->kind == UE_SAVE_PSP) {
		ue = *prev_iter;
		++num_found;
	}
  }
  if (num_found == 0) {
	DevWarn("unwind: no pr_info found for %d", p);
	return;
  }
  if (num_found > 1) {
	// check if all are same
  	for (prev_iter = prev_ue.begin(); prev_iter != prev_ue.end(); ++prev_iter) {
		if (prev_iter->kind == ue.kind 
		  && CLASS_REG_PAIR_EqualP(prev_iter->rc_reg, ue.rc_reg))
			--num_found;
	}
	++num_found;	// original still counts
  }
  if (num_found > 1) {
	UNWIND_ELEM nue;
	nue = Find_Prev_Save_UE_For_BB (prev_ue, bb, 0);
	if (nue.kind == UE_UNDEFINED) {
		// just use memory save if exists
  		for (prev_iter = prev_ue.begin(); prev_iter != prev_ue.end(); ++prev_iter) {
			if (prev_iter->kind == UE_SAVE_SP || prev_iter->kind == UE_SAVE_PSP) {
				nue = *prev_iter;
				break;
			}
		}
		if (nue.kind == UE_UNDEFINED) {
			DevWarn("couldn't find unwind save for %d", p);
		}
		else
			ue = nue;
	}
	else
		ue = nue;
  }
  ue.when = when;
  ue.bb = bb;
  ue_list.insert(ue_iter, ue);
  if (Trace_Unwind) 
	fprintf(TFile, "state change for %d at entry to bb %d\n", p, BB_id(bb));
}

static void
Add_UE (INT8 kind, PR_TYPE p, UINT when, BB *bb)
{
  UNWIND_ELEM ue;
  ue.kind = kind;
  ue.qp = 0;
  ue.rc_reg = PR_To_CR(p);
  ue.when = when;
  ue.bb = bb;
  ue_list.insert(ue_iter, ue);
  if (Trace_Unwind) 
	fprintf(TFile, "state change for %d at entry to bb %d\n", p, BB_id(bb));
}

static void
Add_UE (INT8 kind, UINT label, UINT when, BB *bb)
{
  UNWIND_ELEM ue;
  ue.kind = kind;
  ue.label = label;
  ue.when = when;
  ue.bb = bb;
  ue_list.insert(ue_iter, ue);
  if (Trace_Unwind) 
	fprintf(TFile, "add ue kind %d label %d at bb %d\n", kind, label, BB_id(bb));
}

static void
Do_Control_Flow_Analysis_Of_Unwind_Info (void)
{
  // we know what unwind changes happen in each block;
  // now have to propagate that info so each bb has correct info upon entry.

  // QUESTION:  can we assume that saved reg is only saved to one place?
  // (we did on MIPS, except for local spills via prev_fd_to_spill_loc).
  // E.g. can b1 be saved to r33 in one path and 8(sp) in another path?
  // or saved to r33 in one path and r34 in another path?
  // A:  I think only one stack location will be used, but multiple saveregs
  // can be used.

  // have 4 bit vectors for each bb:  
  // entry, local-save, local-restore, and exit state.
  // first fill in the local state info with changes that happen in that bb.

#ifdef linux
  PR_BITSET *entry_state         =
	(PR_BITSET *) alloca((PU_BB_Count+1) * sizeof(PR_BITSET));
  PR_BITSET *local_save_state    =
	(PR_BITSET *) alloca((PU_BB_Count+1) * sizeof(PR_BITSET));
  PR_BITSET *local_restore_state =
	(PR_BITSET *) alloca((PU_BB_Count+1) * sizeof(PR_BITSET));
  PR_BITSET *exit_state          =
	(PR_BITSET *) alloca((PU_BB_Count+1) * sizeof(PR_BITSET));
#else
  PR_BITSET entry_state[PU_BB_Count+1];
  PR_BITSET local_save_state[PU_BB_Count+1];
  PR_BITSET local_restore_state[PU_BB_Count+1];
  PR_BITSET exit_state[PU_BB_Count+1];
#endif /* linux */

  // mark all saves/restores in local state
  Mark_Local_Saves_Restores (local_save_state, local_restore_state);

  // now propagate the save/restore state thru the control flow.
  // foreach pred bb, copy its exit-state to the entry-state.
  Propagate_Save_Restore_State (entry_state, local_save_state,
	local_restore_state, exit_state);

  PR_TYPE p;
  // keep list of ue's for each pr.
  std::list < UNWIND_ELEM > pr_last_info[PR_LAST];
  for (ue_iter = ue_list.begin(); ue_iter != ue_list.end(); ++ue_iter) {
		p = CR_To_PR (ue_iter->rc_reg);
		// put last ue for bb on list
		if ( ! pr_last_info[p].empty()
		    && pr_last_info[p].front().bb == ue_iter->bb)
		{
			pr_last_info[p].pop_front();
		}
		pr_last_info[p].push_front (*ue_iter);
  }

  // now determine save/restore changes at each when point
  // and update ue_list with changes
  PR_BITSET current_state = 0;
  INT bbid;
  UINT lwhen = 0;
  ue_iter = ue_list.begin();
  // bug fix for OSP_226
  //
  bool label_state = FALSE;
  bool copy_state = FALSE;
  for (BB *bb = REGION_First_BB; bb; bb = BB_next(bb)) {
	if (label_state && BB_prev(bb) != NULL && BB_exit(BB_prev(bb))) {
	  copy_state = TRUE;
	}
	if (BB_unreachable(bb)) {
		lwhen += Get_BB_When_Length(bb); 
		continue;
	}
	if (BB_length(bb) == 0) {
		// empty, so ignore
		continue;
	}
	bbid = BB_id(bb);

	// if an alternate entry point,
	// reset current state so no implicit changes at entry.
	if (BB_entry(bb)) {
		current_state = entry_state[bbid];
	}
	// in case have exit that follows exit,
	// first copy previous label then do new label.
	// if (label_state && BB_prev(bb) != NULL && BB_exit(BB_prev(bb))) {
	if (copy_state) {
		// in bb that follows exit, so copy above label
		Add_UE (UE_COPY, last_label, lwhen, bb);
		current_state = entry_state[BB_id(BB_prev(bb))];
		label_state = FALSE;
	}
	if (BB_exit(bb) && BB_next(bb) != NULL) {
		// if have an exit that is followed by another bb
		// then want to create body label before exit and
		// copy from label after exit (i.e. skip destroy frame)
		if (BB_entry(bb) && BB_prev(bb) == NULL) {
			// if in first bb, then make sure label
			// comes after create_frame.
			while (ue_iter != ue_list.end() && ue_iter->bb == bb) {
				if (ue_iter->kind == UE_CREATE_FRAME) {
					// bug fix for OSP_226
					//
					label_state = TRUE;
					++ue_iter;
					Add_UE (UE_LABEL, ++last_label, 
						ue_iter->when, bb);
					break;
				}
				++ue_iter;
			}
		}
		else {  
			// bug fix for OSP_226
			//
			label_state = TRUE;
			Add_UE (UE_LABEL, ++last_label, lwhen, bb);
		}
	}

	// add implicit changes upon entry
	// co-design of entry generation, see cflow.cxx::generate_entry
	// if (current_state != entry_state[bbid]) {
 	if (0) { 
  		for (p = PR_FIRST; p < PR_LAST; INCR(p)) {
			// ignore implicit sp changes,
			// as label/copy should handle those.
			if (p == PR_SP) continue;
			if (Get_PR(current_state,p) != Get_PR(entry_state[bbid],p)) 
			{
				// add into ue_list
				if (Get_PR(entry_state[bbid],p) == FALSE) {
					// add restore
					Add_UE (UE_RESTORE_GR, p, lwhen, bb);
				}
				else {
					// add save
					Add_UE (pr_last_info[p], p, lwhen, bb);
				}
			}
  		}
		current_state = entry_state[bbid];
	}

	// look for changes in bb
	while (ue_iter != ue_list.end() && ue_iter->bb == bb) {
		p = CR_To_PR (ue_iter->rc_reg);
		if (Trace_Unwind) fprintf(TFile, 
			"state change for %d in bb %d\n", p, bbid);
		if (ue_iter->kind == UE_RESTORE_GR
		 || ue_iter->kind == UE_RESTORE_MEM)
		{
			current_state = Clear_PR(current_state, p);
		}
		else {
			current_state = Set_PR(current_state, p);
		}
		++ue_iter;
	}
	lwhen += Get_BB_When_Length(bb);
  }
}

// does unwind follow simple pattern of saves in entry, restores in exit?
static BOOL
Is_Unwind_Simple (void)
{
  if (has_asm) return FALSE;

  for (ue_iter = ue_list.begin(); ue_iter != ue_list.end(); ++ue_iter) {
    	// if not first or last bb, then not a simple unwind
    	if (BB_prev(ue_iter->bb) != NULL && BB_next(ue_iter->bb) != NULL) {
		return FALSE;
    	}
	// if not entry or exit bb, then not a simple unwind
	if ( ! BB_entry(ue_iter->bb) && ! BB_exit(ue_iter->bb)) {
		return FALSE;
	}
  }
  return TRUE;
}

static void
Insert_Epilogs (void)
{
  std::list < UNWIND_ELEM >::iterator prev_ue;
  UNWIND_ELEM ue;
  for (ue_iter = ue_list.begin(); ue_iter != ue_list.end(); ++ue_iter) {
    switch (ue_iter->kind) {
    case UE_DESTROY_FRAME:
	// go backwards, until first restore in exit bb
	prev_ue = ue_iter;
	--prev_ue;
	while (prev_ue != ue_list.begin()) {
		if (prev_ue->bb != ue_iter->bb) break;
		if (prev_ue->kind != UE_RESTORE_GR && prev_ue->kind != UE_RESTORE_MEM)
			break;
		--prev_ue;
	}
	++prev_ue;
  	ue.kind = UE_EPILOG;
  	ue.when = prev_ue->when;
  	ue.bb = prev_ue->bb;
  	ue_list.insert(prev_ue, ue);
  	if (Trace_Unwind) 
		fprintf(TFile, "add epilog at bb %d, when %d\n", BB_id(ue.bb), ue.when);
    }
  }
}

// compute sizes of each prolog and body region
static void
Compute_Region_Sizes (void)
{
  std::list < UNWIND_ELEM >::iterator current_ue = ue_list.end();
  for (ue_iter = ue_list.begin(); ue_iter != ue_list.end(); ++ue_iter) {
    switch (ue_iter->kind) {
    case UE_CREATE_FRAME:
	// assume create frame will be first, and only appear once
	Is_True (current_ue == ue_list.end(), ("multiple unwind create frames?"));
	current_ue = ue_iter;
	current_ue->u.region.start = 0;
	break;
    case UE_EPILOG:
    case UE_LABEL:
    case UE_COPY:
	// set size of previous region
	current_ue->u.region.size = ue_iter->when - current_ue->u.region.start;
	current_ue = ue_iter;
	current_ue->u.region.start = ue_iter->when;
	break;
    }
  }
  if (current_ue != ue_list.end())
  	current_ue->u.region.size = last_when - current_ue->u.region.start + 1;
}

static UINT next_when;
static UREGION_TYPE proc_region;

// call per-PU
void 
Init_Unwind_Info (BOOL trace)
{
  Trace_Unwind = trace;
  has_asm = FALSE;
  has_create = FALSE;

  Find_Unwind_Info ();
  simple_unwind = Is_Unwind_Simple();

  last_label = 0;
  next_when = 0;
  proc_region = UNDEFINED_UREGION;

  if ( ! simple_unwind) {
	if (Trace_Unwind) fprintf (TFile, "need to propagate unwind info\n");
	// need to propagate unwind info to each block,
	// and update ue_list with state changes
	Do_Control_Flow_Analysis_Of_Unwind_Info ();
	if ( ! has_asm) simple_unwind = TRUE;
  }
  Insert_Epilogs();
  Compute_Region_Sizes();

  if (Trace_Unwind) {
	fprintf (TFile, "%s unwind\n", (simple_unwind ? "simple" : "complicated"));
	Print_All_Unwind_Elem ("unwind2");
  }

  // for use in emit_unwind
  ue_iter = ue_list.begin();
}

void 
Finalize_Unwind_Info(void)
{
  ue_list.clear();
}


// some regs have special unwind record descriptors,
// while others use general spill mechanism.
static BOOL
Use_Spill_Record (CLASS_REG_PAIR crp)
{
	if (CLASS_REG_PAIR_EqualP(crp, CLASS_REG_PAIR_ra))
		return FALSE;
	if (CLASS_REG_PAIR_EqualP(crp, CLASS_REG_PAIR_pfs))
		return FALSE;
	if (CLASS_REG_PAIR_EqualP(crp, CLASS_REG_PAIR_lc))
		return FALSE;
	return TRUE;
}

void 
Emit_Unwind_Directives_For_OP(OP *op, FILE *f)
{
  char prefix[3];
  if ( CG_emit_unwind_directives)
	strcpy(prefix, "");
  else
	strcpy(prefix, "//");	// emit as comments

  if (ue_iter == ue_list.end()) {	// none left
	if (proc_region == UNDEFINED_UREGION) {
		// no unwind entries at all,
		// but do want function info.
		fprintf(f, "%s\t.unwentry\n", prefix);
		proc_region = PROLOGUE_UREGION;
	}
	return;	// none left
  }
  if (OP_dummy(op)) return;

  if (proc_region == UNDEFINED_UREGION) {
    fprintf(f, "%s\t.prologue\n", prefix);
    proc_region = PROLOGUE_UREGION;
  } 

  while (ue_iter != ue_list.end() && ue_iter->when == next_when) {
    ISA_REGISTER_CLASS rc = CLASS_REG_PAIR_rclass(ue_iter->rc_reg);
    REGISTER reg = CLASS_REG_PAIR_reg(ue_iter->rc_reg);

    switch (ue_iter->kind) {
    case UE_CREATE_FRAME:
      if (Current_PU_Stack_Model == SMODEL_SMALL) {
	fprintf(f, "%s\t.fframe %lld\n", prefix, Frame_Len);
      } else {
	fprintf(f, "%s\t.vframe %s\n", 
		   prefix,
		   UE_Register_Name(TN_register_class(FP_TN), TN_register(FP_TN)));
      }
      break;
    case UE_EPILOG:
      fprintf(f, "%s\t.body\n", prefix);
      proc_region = EPILOGUE_BODY_UREGION;
      break;
    case UE_DESTROY_FRAME:
      /* when -foptimize-regions, temp close the error: 
       * Epilogue count of 4294967296 exceeds number of nested prologues (0)
       * please refer to psABI
       */
      if (Omit_UE_DESTROY_FRAME)
	break;
      else
      fprintf(f, "%s\t.restore %s\n", 
		   prefix,
		   UE_Register_Name(TN_register_class(SP_TN), TN_register(SP_TN)));
      break;
    case UE_LABEL:
      fprintf(f, "%s\t.body\n", prefix);
      proc_region = LABEL_BODY_UREGION;
      fprintf(f, "%s\t.label_state %d\n", prefix, ue_iter->label);
      break;
    case UE_COPY:
      fprintf(f, "%s\t.body\n", prefix);
      proc_region = COPY_BODY_UREGION;
      fprintf(f, "%s\t.copy_state %d\n", prefix, ue_iter->label);
      break;
    case UE_RESTORE_MEM:
      // can ignore restores from memory, as memory will survive
      break;
    case UE_RESTORE_GR:
      // can ignore restores in epilog, as all is restored
      if (proc_region == EPILOGUE_BODY_UREGION)
	break;
      if (ue_iter->qp != 0) {
      	fprintf(f, "%s\t.restorereg.p p%d, %s\n", prefix, 
		ue_iter->qp,
		UE_Register_Name(rc, reg) );
      }
      else {
      	fprintf(f, "%s\t.restorereg %s\n", prefix, 
		UE_Register_Name(rc, reg) );
      }
      break;
    case UE_SAVE_GR:
      if (ue_iter->qp != 0) {
      	fprintf(f, "%s\t%s p%d, %s, %s\n", prefix, ".spillreg.p",
		ue_iter->qp,
		UE_Register_Name(rc, reg),
		UE_Register_Name( CLASS_REG_PAIR_rclass(ue_iter->save_rc_reg),
				CLASS_REG_PAIR_reg(ue_iter->save_rc_reg) ) );
      }
      else {
      	fprintf(f, "%s\t%s %s, %s\n", prefix, 
		(Use_Spill_Record (ue_iter->rc_reg) ? ".spillreg" : ".save"),
		UE_Register_Name(rc, reg),
		UE_Register_Name( CLASS_REG_PAIR_rclass(ue_iter->save_rc_reg),
				CLASS_REG_PAIR_reg(ue_iter->save_rc_reg) ) );
      }
      break;
    case UE_SAVE_SP:
      if (ue_iter->qp != 0) {
      	fprintf(f, "%s\t%s p%d, %s, %lld\n", prefix, ".spillsp.p",
		ue_iter->qp,
		UE_Register_Name(rc, reg), 
		ue_iter->u.offset);
      }
      else {
      	fprintf(f, "%s\t%s %s, %lld\n", prefix, 
		Use_Spill_Record (ue_iter->rc_reg) ? ".spillsp" : ".savesp",
		UE_Register_Name(rc, reg), 
		ue_iter->u.offset);
      }
      break;
    case UE_SAVE_PSP:
      if (ue_iter->qp != 0) {
      	fprintf(f, "%s\t%s p%d, %s, %lld\n", prefix, ".spillpsp.p",
		ue_iter->qp,
		UE_Register_Name(rc, reg), 
		ue_iter->u.offset);
      }
      else {
      	fprintf(f, "%s\t%s %s, %lld\n", prefix, 
		Use_Spill_Record (ue_iter->rc_reg) ? ".spillpsp" : ".savepsp",
		UE_Register_Name(rc, reg), 
		ue_iter->u.offset);
      }
      break;
    default:
      FmtAssert(FALSE, ("Unhandled UNWIND_ELEM kind (%d)\n", ue_iter->kind));
      /*NOTREACHED*/
    }

    ++ue_iter;
  }

  next_when += OP_Real_Inst_Words(op);
}



static void
Add_Prologue_Header (__unw_info_t *uinfo, UINT64 size)
{
  	__unw_error_t st = 0;
  	if (Trace_Unwind) fprintf(TFile, "prolog header size %llu\n", size);
  	st |= unwind_info_add_prologue_header(uinfo, size);
  	Is_True(st == __UNW_OK, ("unwind_info prolog error (%d)", st));
}
static void
Add_Body_Header (__unw_info_t *uinfo, UINT64 size)
{
  	__unw_error_t st = 0;
  	if (Trace_Unwind) fprintf(TFile, "body header size %llu\n", size);
  	st = unwind_info_add_body_header(uinfo, size);
  	Is_True(st == __UNW_OK, ("unwind_info body error (%d)", st));
}

// process info we've collected and create the unwind descriptors
static void
Create_Unwind_Descriptors (__unw_info_t *uinfo)
{
  __unw_error_t st = 0;
  ISA_REGISTER_CLASS rc;
  REGISTER reg;
  UREGION_TYPE uregion = UNDEFINED_UREGION;
  UINT start_when = 0;
  UINT end_when = 0;

  if (!simple_unwind) return;	// TODO

  for (ue_iter = ue_list.begin(); ue_iter != ue_list.end(); ++ue_iter) {
    if (ue_iter->kind == UE_CREATE_FRAME)
	break;
  }
  if (ue_iter == ue_list.end()) {
    // no frame
    Add_Prologue_Header (uinfo, last_when);
  }
  else {
    Add_Prologue_Header (uinfo, ue_iter->u.region.size);

    if (Current_PU_Stack_Model == SMODEL_SMALL) {
      if (Trace_Unwind) fprintf(TFile, "fixed stack frame of size %lld, set at when %d\n", 
		Frame_Len, ue_iter->when);
      st |= unwind_info_add_prologue_mem_stack_f_info(uinfo, 
			ue_iter->when, Frame_Len / INST_BYTES);
    }
    else {
      if (Trace_Unwind) fprintf(TFile, "large or variable-size stack frame, when = %d\n", ue_iter->when); 
      st |= unwind_info_add_prologue_mem_stack_v_info (uinfo, ue_iter->when);
      st |= unwind_info_add_prologue_psp_gr_info (uinfo,
		REGISTER_machine_id (
			TN_register_class(FP_TN), TN_register(FP_TN) ) );
    }
    Is_True(st == __UNW_OK, ("unwind_info mem_stack error (%d)", st));
  }
  uregion = PROLOGUE_UREGION;

  for (ue_iter = ue_list.begin(); ue_iter != ue_list.end(); ++ue_iter) {
    switch (ue_iter->kind) {
    case UE_EPILOG:
	Add_Body_Header (uinfo, ue_iter->u.region.size);
	uregion = EPILOGUE_BODY_UREGION;
	start_when = ue_iter->u.region.start;
	end_when = start_when + ue_iter->u.region.size;
	break;
    case UE_DESTROY_FRAME:
	{
	  INT when_from_end = end_when - ue_iter->when - 1;
	  Is_True(end_when != 0, ("unwind: no epilog before destroy_frame?"));
	  if (Trace_Unwind) fprintf(TFile, "body epilogue at when %d\n", when_from_end);
	  st |= unwind_info_add_body_epilogue_info(uinfo, when_from_end, 0);
	  Is_True(st == __UNW_OK, ("unwind_info frame restore error (%d)",st));
	}
	break;
    case UE_LABEL:
	Add_Body_Header (uinfo, ue_iter->u.region.size);
	uregion = LABEL_BODY_UREGION;

	if (Trace_Unwind) fprintf(TFile, "body label at when %d\n", ue_iter->when);
	st |= unwind_info_add_body_label_state_info(uinfo, ue_iter->label);
	Is_True(st == __UNW_OK, ("unwind_info label error (%d)",st));
	start_when = ue_iter->u.region.start;
	break;
    case UE_COPY:
	Add_Body_Header (uinfo, ue_iter->u.region.size);
	uregion = COPY_BODY_UREGION;

	if (Trace_Unwind) fprintf(TFile, "body copy at when %d\n", ue_iter->when);
	st |= unwind_info_add_body_copy_state_info(uinfo, ue_iter->label);
	Is_True(st == __UNW_OK, ("unwind_info copy error (%d)",st));
	start_when = ue_iter->u.region.start;
	break;

    case UE_SAVE_GR:
      {
	rc = CLASS_REG_PAIR_rclass(ue_iter->rc_reg);
	reg = CLASS_REG_PAIR_reg(ue_iter->rc_reg);
	ISA_REGISTER_CLASS save_rc = CLASS_REG_PAIR_rclass(ue_iter->save_rc_reg);
	REGISTER save_reg = CLASS_REG_PAIR_reg(ue_iter->save_rc_reg);
	if (Trace_Unwind) fprintf(TFile, "save reg to reg at when %d\n", ue_iter->when);

	if (uregion == PROLOGUE_UREGION) {
		st |= unwind_info_add_prologue_info_reg (
			  uinfo, 
			  rc,
			  REGISTER_machine_id(rc, reg),
			  ue_iter->when - start_when,
			  save_rc,
			  REGISTER_machine_id(save_rc, save_reg));
	}
	else {
		// need to handle saves and restores in copy and label bodies
		st |= unwind_info_add_body_info_reg (
			  uinfo, 
			  rc,
			  REGISTER_machine_id(rc, reg),
			  ue_iter->when - start_when,
			  save_rc,
			  REGISTER_machine_id(save_rc, save_reg));
	}
      }
      Is_True(st == __UNW_OK, ("unwind_info save error (%d) on reg %s", 
			       st, UE_Register_Name(rc, reg) ));
      break;

    case UE_SAVE_SP:
	rc = CLASS_REG_PAIR_rclass(ue_iter->rc_reg);
	reg = CLASS_REG_PAIR_reg(ue_iter->rc_reg);
	if (Trace_Unwind) fprintf(TFile, "save reg to sp mem at when %d\n", ue_iter->when);
	if (uregion == PROLOGUE_UREGION) {
		st |= unwind_info_add_prologue_info_sp_offset(
				uinfo,
				rc,
				REGISTER_machine_id(rc, reg),
				ue_iter->when - start_when,
				ue_iter->u.offset);
	}
	else {
		st |= unwind_info_add_body_info_sp_offset(
				uinfo,
				rc,
				REGISTER_machine_id(rc, reg),
				ue_iter->when - start_when,
				ue_iter->u.offset);
	}
        Is_True(st == __UNW_OK, ("unwind_info prolog error (%d) on reg %s", 
			       st, UE_Register_Name(rc, reg) ));
      	break;

    case UE_SAVE_PSP:
	rc = CLASS_REG_PAIR_rclass(ue_iter->rc_reg);
	reg = CLASS_REG_PAIR_reg(ue_iter->rc_reg);

	if (Trace_Unwind) fprintf(TFile, "save reg to psp mem at when %d\n", ue_iter->when);
	if (uregion == PROLOGUE_UREGION) {
	  	st |= unwind_info_add_prologue_info_psp_offset(
				uinfo, 
				rc,
				REGISTER_machine_id(rc, reg),
				ue_iter->when - start_when,
				ue_iter->u.offset);
	}
	else {
	  	st |= unwind_info_add_body_info_psp_offset(
				uinfo, 
				rc,
				REGISTER_machine_id(rc, reg),
				ue_iter->when - start_when,
				ue_iter->u.offset);
	}
        Is_True(st == __UNW_OK, ("unwind_info prolog error (%d) on reg %s", 
			       st, UE_Register_Name(rc, reg) ));
       	break;

    case UE_RESTORE_MEM:
	// can ignore restores in memory, as memory survives
	break;
    case UE_RESTORE_GR:
	// can ignore restores in epilog (all are restored)
	if (uregion == EPILOGUE_BODY_UREGION)
		break;
	rc = CLASS_REG_PAIR_rclass(ue_iter->rc_reg);
	reg = CLASS_REG_PAIR_reg(ue_iter->rc_reg);
	if (Trace_Unwind) fprintf(TFile, "restore reg at when %d\n", ue_iter->when);
	st |= unwind_info_add_body_info_restore (uinfo, 
		rc, 
		REGISTER_machine_id(rc, reg),
		ue_iter->when - start_when);
	Is_True(st == __UNW_OK, ("unwind_info restore error (%d) on reg %s", 
		st, UE_Register_Name(rc, reg) ));
	break;
    }
  }
}

// dump unwind table and info to .s file
__unw_error_t 
unwind_dump2asm (char *unwind_table_ptr,
		__uint64_t unwind_table_size,
		char *unwind_info_ptr,
		__uint64_t unwind_info_size,
		void *arg) // last_pc-first_pc
{
	static __uint64_t last_info_size = 0;
	static __uint64_t last_table_size = 0;
        __uint64_t i;
        __uint64_t unwind_table_size_in_entries =
                        unwind_table_size/sizeof(__unw_table_entry_t);
	fprintf(Asm_File, "// emit unwind info\n");
	// gas knows what attributes to give unwind sections
	fprintf(Asm_File, "\t%s %s\n", AS_SECTION, IA64_UNWIND_INFO);
	// dump section in 8-byte chunks
	fprintf(Asm_File, ".Lunwind_info_%d:\n", Current_PU_Count());
	for (i = last_info_size; i < unwind_info_size; i+=8) {
		fprintf(Asm_File, "\t%s %#llx\n", AS_DWORD, (UINT64)*(__uint64_t *)(unwind_info_ptr+i));
	}
	fprintf(Asm_File, "\t%s %s\n", AS_SECTION, IA64_UNWIND);
	// should always be 3 double-words
	i = last_table_size;
	fprintf(Asm_File, "\t%s @segrel(%s#)\n", AS_DWORD, Cur_PU_Name);
	i+=8;
	fprintf(Asm_File, "\t%s @segrel(%s#+%p)\n", AS_DWORD, Cur_PU_Name, arg);
	fprintf(Asm_File, "\t%s @segrel(.Lunwind_info_%d)\n", AS_DWORD, Current_PU_Count());
	last_info_size = unwind_info_size;
	last_table_size = unwind_table_size;
}

/* construct the fde for the current procedure. */
extern Dwarf_P_Fde
Build_Fde_For_Proc (Dwarf_P_Debug dw_dbg, BB *firstbb,
		    LABEL_IDX begin_label,
		    LABEL_IDX end_label,
		    INT32     end_offset,
		    // The following two arguments need to go away
		    // once libunwind gives us an interface that
		    // supports symbolic ranges.
		    INT       low_pc,
		    INT       high_pc)
{
  __unw_info_t *uinfo = NULL;
  __unw_error_t st;

  if ( ! CG_emit_unwind_info) return NULL;
  if ( CG_emit_unwind_directives) return NULL;
  // else emit unwind info directly as data

  st = unwind_info_initialize(&uinfo, low_pc, high_pc);
  Is_True(st == __UNW_OK, ("unwind_info initialize error (%d)", st));

  // process info we've collected and create the unwind descriptors
  Create_Unwind_Descriptors (uinfo);

  st = unwind_info_finalize(uinfo);
  FmtAssert(st == __UNW_OK, ("unwind_info finalize error (%d)", st));

  if (has_asm)
	DevWarn("no unwind info cause PU has asm");
  else if ( ! simple_unwind)
	DevWarn("no unwind info cause PU is too complicated");
  if (simple_unwind) {
	unwind_process (unwind_dump2asm, (void*)(INTPTR)(high_pc-low_pc));
  }
  return NULL;	// no fde when generate unwind stuff
}

#include <elf_stuff.h>
#include <elfaccess.h>

void
Check_Dwarf_Rel(const Elf32_Rel &current_reloc)
{
  FmtAssert(REL32_type(current_reloc) == R_IA_64_DIR32MSB,
	    ("Unimplemented 32-bit relocation type %d",
	     REL32_type(current_reloc)));
}

void
Check_Dwarf_Rel(const Elf64_AltRel &current_reloc)
{
  FmtAssert(REL64_type(current_reloc) == R_IA_64_DIR64MSB,
	    ("Unimplemented 64-bit relocation type %d",
	     REL64_type(current_reloc)));
}

void
Check_Dwarf_Rela(const Elf64_AltRela &current_reloc)
{
  FmtAssert(FALSE,
	    ("Unimplemented 64-bit relocation type %d",
	     REL64_type(current_reloc)));
}

void
Check_Dwarf_Rela(const Elf32_Rela &current_reloc)
{
  FmtAssert(FALSE,
	    ("Unimplemented 32-bit relocation type %d",
	     REL32_type(current_reloc)));
}
static char *drop_these[] = {
      // Assembler generates .debug_line from directives itself, so we
      // don't output it.
	".debug_line",
     // gdb does not use the MIPS sections
     // debug_weaknames, etc.
	".debug_weaknames",
	".debug_varnames",
	".debug_typenames",
	".debug_funcnames",
     // we don't use debug_frame in IA-64.
	".debug_frame",
	".eh_frame", /* it has a problem unfortunately -- shuxin yang */
	0
};
// return TRUE if we want to emit the section (IA-64).
// return FALSE if we do not want to for IA-64.
extern BOOL Is_Dwarf_Section_To_Emit(const char *name)
{

	for(int  i = 0; drop_these[i]; ++i) {
	  if(strcmp(name,drop_these[i]) == 0) {
	    return FALSE;
	  }
	}
	if(Debug_Level < 1 && strncmp(name, ".debug_", 7) == 0) {
	  //do not emit .debug_ sections if no '-g'
	  return FALSE;
	}
        return TRUE;
}

