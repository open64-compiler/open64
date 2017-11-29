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
}


void
EETARG_Restore_Pfs (TN *saved_pfs, OPS *ops)
{
}

void
EETARG_Fixup_Entry_Code (BB *bb)
{ 
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
}

void EETARG_Restore_Extra_Callee_Tns (OPS *ops)
{
}


OP *
EETARG_Build_Jump_Instead_Of_Call (OP *call_op)
{
  OP *jump_op;
  TOP jump_top;
  TOP call_top = OP_code(call_op);
  switch (call_top) {
  case TOP_jalr:{
    jump_top = TOP_jr;
    Is_True(OP_opnds(call_op) == 3, ("unexpected number of call opnds"));
    jump_op = Mk_OP(jump_top, OP_opnd(call_op,0),
    				OP_opnd(call_op,2));
    Is_True(OP_opnds(jump_op) == 2, ("unexpected number of jump opnds"));
    break;
  }
  case TOP_jal:{
  	jump_top = TOP_j;
  	Is_True(OP_opnds(call_op) == 2, ("unexpected number of call opnds"));
    jump_op = Mk_OP(jump_top, OP_opnd(call_op,0),
    				OP_opnd(call_op,1));
    Is_True(OP_opnds(jump_op) == 2, ("unexpected number of jump opnds"));
    break;
  }
  default:
    FmtAssert(FALSE, ("don't know how to generate tail call for %s",
		     TOP_Name(call_top)));
    /*NOTREACHED*/
  }
  	
  return jump_op;
}

