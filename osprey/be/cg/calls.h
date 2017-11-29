/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


#ifndef calls_INCLUDED
#define calls_INCLUDED

/* ====================================================================
 * ====================================================================
 *
 * Module: calls.h
 * $Revision: 1.12 $
 * $Date: 05/12/05 08:59:03-08:00 $
 * $Author: bos@eng-24.pathscale.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.calls.h $
 *
 * Revision history:
 *  03-Oct-91 - Original Version
 *  24-Oct-91 - Added support for entry/exit code.
 *
 * Description:
 *
 * General support for call-related functionality in the back end.
 *
 * Defines a descriptor for calls, to be used to contain such
 * information as which registers are actually used to pass parameters,
 * which registers are killed by the call, and so on.
 *
 * At this time, the contents of this descriptor are minimal; it is
 * expected to expand when we add inter-procedural register allocation
 * or similar features.
 *
 * TODO:  should change this to entry_exit.{h,cxx} to reflect real usage.
 *
 * ====================================================================
 * ====================================================================
 */


/* for ST */
#include "symtab.h"
#include "wn.h"
#ifdef KEY
#include "bb.h"
#endif
#include "tn.h"

/* ================================================================= */

/* frame length */
extern INT64 Frame_Len;
extern void Set_Frame_Len (INT64 val);		// sets spadjust TN values

/* ================================================================= */

/* We need to keep track of the callee-saved registers for save/restore
 * purposes.  For this purpose, we keep an array of structures (with
 * size determined by the actual number of callee saved regs) which
 * maps each callee-saved register to a "save-tn". We keep track of 
 * the return-address register the same way.
 */
typedef struct save_reg {
  TN 	*ded_tn; /* the dedicated TN for the callee-saved register */
  TN	*sv_tn;	 /* the save-tn for the callee-saved register */
} SAVE_REG;

/* Define the access macros, including indirect TN access: */
#define SAVE_ded_tn(s)	((s)->ded_tn)
#define SAVE_tn(s)	((s)->sv_tn)

/* Return address register map: */
extern SAVE_REG *Return_Address_Reg;

/* Callee saved register map: */
extern SAVE_REG *Callee_Saved_Regs;	/* Really an array */
extern INT32 Callee_Saved_Regs_Count;	/* Size of array */
#define CALLEE_ded_tn(s)	SAVE_ded_tn(Callee_Saved_Regs+s)
#define CALLEE_tn(s)		SAVE_tn(Callee_Saved_Regs+s)

/* Special PREGs corresponding to special TNs */
extern PREG_NUM *Callee_Saved_Pregs;
extern PREG_NUM Caller_FP_Preg;
extern PREG_NUM Caller_GP_Preg;
extern PREG_NUM Return_Preg;
extern PREG_NUM GP_Preg;
extern PREG_NUM Return_Int_Preg[2];
extern PREG_NUM Return_Float_Preg[2];

extern BOOL Gen_Frame_Pointer;

/* assign a special preg to each CALLEE_tn.  also ra, and gp */
extern void Init_Callee_Saved_Regs_for_REGION( ST *pu, BOOL is_region );

/* Init subprogram entry/exit code: */
extern void Init_Entry_Exit_Code ( WN *pu_wn);

#ifdef TARG_X8664
/* Generate Clear of Merge dependencies for YMM regs */
extern void Generate_Entry_Merge_Clear( BOOL is_region );
#endif

/* Produce subprogram entry/exit code: */
extern void Generate_Entry_Exit_Code ( ST* pu, BOOL is_region );
extern void Adjust_Entry_Exit_Code ( ST* pu );
extern void Adjust_GP_Setup_Code ( ST* pu, BOOL allocate_registers );
extern void Adjust_LC_Setup_Code ( void);
extern BOOL LC_Used_In_PU;	/* flag whether LC_TN was used */

#ifdef TARG_IA64
/* Cycle Count Call */
extern void Cycle_Count_Initialize ( ST *pu, BOOL is_region );  

/* Instrument code to call _mcount */
extern void Instru_Call_Mcount(void );
#endif

/* Tail calls: */
extern void Optimize_Tail_Calls( ST* pu );

#ifdef TARG_X8664
void Adjust_SP_After_Call( BB* );
extern INT Push_Pop_Int_Saved_Regs (void);
#endif

#ifdef KEY
// The following are interfaces into calls.cxx Callee saved registers stack
typedef struct save_reg_loc {
  TN	 	*ded_tn; /* the dedicated TN for the callee-saved register */
  ST 		*temp;   /* the save location */
  BOOL		user_allocated; /* true if allocated by user via asm */
} SAVE_REG_LOC;

// Return TRUE if this tn is not in the list of callee-saved TNs
extern BOOL Is_Unique_Callee_Saved_Reg (TN *);
// Number of callee saved registers
extern INT Cgdwarf_Num_Callee_Saved_Regs (void);
// Nth callee saved register dedicated TN
extern TN* Cgdwarf_Nth_Callee_Saved_Reg (INT n);
// The location on the stack that corresponds to the nth TN on the stack.
extern ST* Cgdwarf_Nth_Callee_Saved_Reg_Location (INT n);
#endif
#ifdef TARG_MIPS
extern TN *Caller_GP_TN;
#endif
#endif /* calls_INCLUDED */
