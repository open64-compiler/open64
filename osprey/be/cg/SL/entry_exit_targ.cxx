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
  OP *jump_op;
  TOP jump_top;
  TOP call_top = OP_code(call_op);
  switch (call_top) {
  case TOP_jal:
    jump_top = TOP_j;
    break;
  case TOP_jalr:
    jump_top = TOP_jr;
    break;
  default:
    FmtAssert(FALSE, ("don't know how to generate tail call for %s",
		     TOP_Name(call_top)));
    /*NOTREACHED*/
  }

  Is_True(OP_opnds(call_op) == 1, ("unexpected number of call opnds"));
#if defined(TARG_SL)
  if (jump_top == TOP_jr) {
    if (OP_opnd(call_op, 0) != JA_TN) {
      OP *mvop =  Mk_OP(TOP_mvtc, JA_TN, OP_opnd(call_op, 0));
      OP_srcpos(mvop) = OP_srcpos(call_op);
      BB_Insert_Op_Before(call_op->bb, call_op, mvop);  
    }
    jump_op = Mk_OP(jump_top, JA_TN);	  
    OP_srcpos(jump_op) = OP_srcpos(call_op);
    Is_True(OP_opnds(jump_op) == 1, ("unexpected number of jump opnds"));
    return jump_op;	
  } 
#endif
  jump_op = Mk_OP(jump_top, OP_opnd(call_op, 0));
  OP_srcpos(jump_op) = OP_srcpos(call_op);

  Is_True(OP_opnds(jump_op) == 1, ("unexpected number of jump opnds"));
  return jump_op;
}
