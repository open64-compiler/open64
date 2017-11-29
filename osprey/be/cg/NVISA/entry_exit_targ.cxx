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

void EETARG_Generate_PIC_Exit_Code( BB* bb, OPS* ops )
{
  FmtAssert(FALSE, ("NYI"));
}


void EETARG_Generate_PIC_Entry_Code( BB* bb, OPS* ops )
{
  FmtAssert( BB_entry(bb), ("BB is not entry") );
  FmtAssert( Is_Target_32bit(), ("PIC entry for -m32 only") );

  if( BB_handler( bb ) )
    return;

  FmtAssert(FALSE, ("NYI"));

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
  FmtAssert(FALSE, ("NYI"));

  return NULL;
}

OP* EETARG_High_Level_Procedure_Exit() 
{ 
  // This functions is called after LRA. So, need to name our TNs.
  FmtAssert(FALSE, ("NYI"));
  return NULL;
}
