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


// target-specific entry_exit code

// PFS = Previous Function State
extern void EETARG_Save_Pfs (TN *saved_pfs, OPS *ops);

extern void EETARG_Restore_Pfs (TN *saved_pfs, OPS *ops);

extern void EETARG_Init_Entry_Exit_Code (WN *pu_wn, BOOL need_frame_pointer);

// save and restore any extra callee-tns that need special processing
extern void EETARG_Save_Extra_Callee_Tns (OPS *ops);
extern void EETARG_Restore_Extra_Callee_Tns (OPS *ops);

#ifdef TARG_IA64
// Instrument code to call _mcount function in libc.a for -pg option
extern void EETARG_Call_Mcount(BB *bb);
#endif

// target-specific adjustments to entry ops
extern void EETARG_Fixup_Entry_Code (BB *bb);
extern void EETARG_Fixup_Exit_Code (BB *bb);

//  Replace the call OP with a jump.
extern OP *EETARG_Build_Jump_Instead_Of_Call(OP *call_op);

#ifdef USE_HIGH_LEVEL_PROCEDURE_EXIT
extern OP* EETARG_High_Level_Procedure_Exit();
#else
inline OP* EETARG_High_Level_Procedure_Exit() { return NULL; }
#endif

#ifdef TARG_X8664
void EETARG_Generate_PIC_Entry_Code( BB* bb, OPS* ops );
void EETARG_Generate_PIC_Exit_Code( BB* bb, OPS* ops );
#endif
