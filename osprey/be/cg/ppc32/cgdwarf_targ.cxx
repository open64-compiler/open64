/*

  Copyright (C) 2006-2009 Tsinghua University.  All Rights Reserved.

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

*/

#include <stdio.h>
#include <stdlib.h>
#include "libelf/libelf.h"
#include <sys/unwindP.h>
#include <list>
#include "elf_stuff.h"
#include <elfaccess.h>
#include "dwarf.h"

#include "defs.h"
#include "erglob.h"
#include "glob.h"
#include "flags.h"
#include "tracing.h"
#include "em_elf.h"
#include "em_dwarf.h"
#include "config.h"
#include "config_asm.h"
#include "cgir.h"
#include "register.h"
#include "cgtarget.h"
#include "calls.h"
#include "stblock.h"
#include "data_layout.h"
#include "cg_spill.h"
#include "cgdwarf.h"
#include "cgdwarf_targ.h"
#include "dwarf_incl.h"
#include "pro_opaque.h"
#include "pro_frame.h"
#include "be_util.h"

 #if !defined(TARG_SL)
 #define   FD_S0   0
 #define   FD_S1   1
 #define   FD_S2   2
 #define   FD_S3   3
 #define   FD_S4   4
 #define   FD_S5   5
 #define   FD_S6   6
 #define   FD_S7   7
 //#define   FD_S8   8
 #define   FD_GP   9
 //#define   FD_RA   10
 #define   FD_JA   11
 //#define   FD_SL   FD_JA
 #define   FD_F20  12
 #define   FD_F21  13
 #define   FD_F22  14
 #define   FD_F23  15
 #define   FD_F24  16
 #define   FD_F25  17
 #define   FD_F26  18
 #define   FD_F27  19
 #define   FD_F28  20
 #define   FD_F29  21
 #define   FD_F30  22
 #define   FD_F31  23
 //#define   FD_CFA  24
 //#define   FD_FIRST FD_S0
 //#define   FD_LAST  25
#endif


static Dwarf_Error dw_error;
static BOOL Trace_Unwind = TRUE;
static UINT last_when;
static BOOL simple_unwind = FALSE;
static BOOL has_asm = FALSE;
static UINT last_label = 0;
static BOOL has_create = FALSE;

/* define all the fd registers that we track. This is a mapping from all
 * the physical registers to a number that fits into a 0-31 range. We 
 * do this so that we can use a 32bit word in our global dataflow 
 * computation.
 */

#define   FD_R0   0
#define   FD_RA   FD_R0  // we don't anticipate R0 save/spill
#define   FD_R1   1
#define   FD_R2   2
#define   FD_SL   FD_R1
#define   FD_R3   3
#define   FD_R4   4
#define   FD_R5   5
#define   FD_R6   6
#define   FD_R7   7
#define   FD_R8   8
#define   FD_R9   9
#define   FD_R10  10
#define   FD_R11  11
#define   FD_R12  12
#define   FD_R13  13
#define   FD_R14  14
#define   FD_R15  15
#define   FD_R16  16
#define   FD_R17  17
#define   FD_R18  18
#define   FD_R19  19
#define   FD_R20  20
#define   FD_R21  21
#define   FD_R22  22
#define   FD_R23  23
#define   FD_R24  24
#define   FD_R25  25
#define   FD_R26  26
#define   FD_R27  27
#define   FD_CFA  28  // I don't anticipate gp used for spill/save
#define   FD_R29  29
#define   FD_R30  30
#define   FD_R31  31
#define   FD_S8   FD_R31 // fp

/*
#define   FD_F0   32
#define   FD_F1   33
#define   FD_F2   34
#define   FD_F3   35
#define   FD_F4   36
#define   FD_F5   37
#define   FD_F6   38
#define   FD_F7   39
#define   FD_F8   40
#define   FD_F9   41
#define   FD_F10  42
#define   FD_F11  43
#define   FD_F12  44
#define   FD_F13  45
#define   FD_F14  46
#define   FD_F15  47
#define   FD_F16  48
#define   FD_F17  49
#define   FD_F18  50
#define   FD_F19  51
#define   FD_F20  52
#define   FD_F21  53
#define   FD_F22  54
#define   FD_F23  55
#define   FD_F24  56
#define   FD_F25  57
#define   FD_F26  58
#define   FD_F27  59
#define   FD_F28  60  // I don't anticipate gp used for spill/save
#define   FD_F29  61
#define   FD_F30  62
#define   FD_F31  63
*/
#define   FD_FIRST FD_R0
#define   FD_LAST  32

typedef UINT32 FD_REG;

static const UINT32 fd_to_track[FD_LAST] = {
  // BAD inherently HARD CODED callee save register numbering assumed here
  // a map to indicate which register whose state we need to keep track of
  // basically all the callee saved regs
  1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0,
};

/* mapping of each fd register to the corresponding DW_FRAME register. */
static const UINT32 fd_to_dwarf[FD_LAST] = {
  DW_FRAME_REG1, DW_FRAME_REG2, DW_FRAME_REG3,
  DW_FRAME_REG4, DW_FRAME_REG5, DW_FRAME_REG6, DW_FRAME_REG7,
  DW_FRAME_REG8, DW_FRAME_REG9, DW_FRAME_REG10, DW_FRAME_REG11,
  DW_FRAME_REG12, DW_FRAME_REG13, DW_FRAME_REG14, DW_FRAME_REG15,
  DW_FRAME_REG16, DW_FRAME_REG17, DW_FRAME_REG18, DW_FRAME_REG19,
  DW_FRAME_REG20, DW_FRAME_REG21, DW_FRAME_REG22, DW_FRAME_REG23,
  DW_FRAME_REG24, DW_FRAME_REG25, DW_FRAME_REG26, DW_FRAME_REG27,
  0, DW_FRAME_REG29, DW_FRAME_REG30, DW_FRAME_REG31,
  /* 0 is for cfa. */
};

/* for each fd register, a mapping to the corresponding spill ST */
static ST *fd_to_spill_loc[FD_LAST];
static ST *curr_fd_to_spill_loc[FD_LAST];
/* keep copy of previous spill_loc in case locally spilling to new loc */
static ST *prev_fd_to_spill_loc[FD_LAST];


/* The different "states" that registers or the CFA may be in */
typedef enum {
  DS_In_Register,	/* register's entry value is in register */
  DS_In_Memory,		/* register's entry value is in memory */
  DS_CFA_Is_SP,		/* the call-frame is off of the stack ptr */
  DS_CFA_Is_Adj_SP,	/* the call-frame is off of adjusted stack ptr*/
  DS_CFA_Is_FP,		/* the call-frame is off of the frame ptr */
  DS_CFA_Last
} CGD_STATES;

static const char *CGD_Name[DS_CFA_Last] = {
   "DS_In_Register",
   "DS_In_Memory",
   "DS_CFA_Is_SP",
   "DS_CFA_Is_Adj_SP",
   "DS_CFA_Is_FP"
};

static char *fmt_string = "\t%s\t0x%x\n";
#define AS_4BYTE ".4byte"
#define AS_1BYTE ".byte"
#define AS_ASCII ".ascii"
#define AS_ULEBW ".uleb128"
#define AS_SLEBW ".sleb128"

static INT unw_info_pu_start_count;

static void 
CIE_dump2asm(Dwarf_P_Cie cie)
{
  unw_info_pu_start_count = Current_PU_Count();
  // gas knows what attributes to give unwind sections
  fprintf(Asm_File, "\t%s\t%s\n", AS_SECTION, ".debug_frame,\"\",@progbits");
  fprintf(Asm_File, ".Lunwind_info_%d:\n", unw_info_pu_start_count);
  fprintf(Asm_File, "\t%s\t.CIE_E%d-.CIE_B%d\n", AS_4BYTE, Current_PU_Count(), Current_PU_Count()); // length of CIE
  fprintf(Asm_File, ".CIE_B%d:\n", Current_PU_Count());
  fprintf(Asm_File, fmt_string, AS_4BYTE, -1); // CIE id 
  fprintf(Asm_File, fmt_string, AS_BYTE, cie->cie_version); 

  if (*(cie->cie_aug))
    fprintf(Asm_File, "\t%s\t\"%s\"\n", AS_ASCII, cie->cie_aug); 
  else
     fprintf(Asm_File, "\t%s \"\\000\"\n", ".ascii");
  fprintf(Asm_File, fmt_string, AS_ULEBW, 1); 
  fprintf(Asm_File, fmt_string, AS_SLEBW, cie->cie_data_align); 
  fprintf(Asm_File, fmt_string, AS_1BYTE, cie->cie_ret_reg); 
	  
  Is_True(cie->cie_personality == 0, ("size does not account for personality != 0"));
  Is_True(cie->cie_next == 0, ("not the last cie"));
  Is_True(*(cie->cie_inst) == 0, ("not extra instruction expected"));
}


/*
 * header of Frame Description Entry
 */
static void 
FDE_dump2asm(INT low_pc, INT high_pc)
{
  Is_True((high_pc-low_pc) >= 0, ("invalid pc pair"));
  if ( Trace_Dwarf ) {
    fprintf ( TFile, "dump2asm pc %x -> %x\n", low_pc, high_pc );
  }

  fprintf(Asm_File, "\t%s\t%s\n", AS_SECTION, ".debug_frame");
  fprintf(Asm_File, "\t%s\t.LFE%d-.LFB%d\n", AS_4BYTE, Current_PU_Count(), Current_PU_Count()); // length of FDE
  fprintf(Asm_File, ".LFB%d:\n", Current_PU_Count());
  fprintf(Asm_File, "\t%s\t.Lunwind_info_%d\n", AS_4BYTE,unw_info_pu_start_count); // CIE_pointer
  fprintf(Asm_File, "\t%s\t%s\n", AS_4BYTE, Cur_PU_Name); // initial location
  fprintf(Asm_File, fmt_string, AS_4BYTE, high_pc-low_pc); // address_range
}

static void 
CIE_end_dump2asm(void)
{
  // close out the current FDE
  fprintf(Asm_File, "\t.ALIGN\t2\n");  
  fprintf(Asm_File, ".CIE_E%d:\n", Current_PU_Count());
}

static void 
FDE_end_dump2asm(void)
{
  // close out the current FDE
  fprintf(Asm_File, "\t.ALIGN\t2\n");  
  fprintf(Asm_File, ".LFE%d:\n", Current_PU_Count());
}

static INT
CFA_dump2asm(Dwarf_Small dw_op, Dwarf_Unsigned op1, Dwarf_Unsigned op2)
{
  if (dw_op == DW_CFA_offset) {
    if (op1 != 35) {
		dw_op = (dw_op & 0xc0) | ((Dwarf_Small)op1 & 0x1F);
    } else {
    	dw_op = (dw_op & 0xc0) | ((Dwarf_Small)op1 & 0x3F);
    }
    fprintf(Asm_File, fmt_string, AS_1BYTE, dw_op); 
    fprintf(Asm_File, fmt_string, AS_ULEBW, op2);
    return 9;
  }
  fprintf(Asm_File, fmt_string, AS_1BYTE, dw_op); 
  switch(dw_op) {
  case DW_CFA_def_cfa:
  case DW_CFA_register:
    fprintf(Asm_File, fmt_string, AS_ULEBW, op1);
    fprintf(Asm_File, fmt_string, AS_ULEBW, op2);
    return 9;
  case DW_CFA_advance_loc:
  case DW_CFA_advance_loc4:
    fprintf(Asm_File, fmt_string, AS_4BYTE, op1);
    return 5;
  case DW_CFA_def_cfa_offset:
  case DW_CFA_def_cfa_register:
  case DW_CFA_same_value:
    fprintf(Asm_File, fmt_string, AS_ULEBW, op1);
    return 5;
  default:
    Is_True(0, ("unsupported DW_op %x\n",dw_op));
    break;
  }
  return 0;
} 


/* keep track of the current PC location and the PC location of the
 * last fde entry that was generated.
 */
static INT32 curloc, lastloc;

/* wrapper for the call to dwarf_add_fde_inst. There is a check to see 
 * if an advance_loc instruction should be generated.
 */
static Dwarf_P_Fde
Add_Fde_Inst (
  Dwarf_P_Fde fde, 
  Dwarf_Small dw_op, 
  Dwarf_Unsigned op1, 
  Dwarf_Unsigned op2) 
{
  /* emit a advance_loc instruction if needed. */
  if (lastloc < curloc) {
    fde = dwarf_add_fde_inst (fde, DW_CFA_advance_loc, curloc - lastloc,
			0, &dw_error);
    if (Assembly)
      CFA_dump2asm(DW_CFA_advance_loc4, curloc-lastloc, 0);
    if (Trace_Dwarf) 
      fprintf(TFile, "add_fde_inst DW_CFA_advance_loc of %d\n", curloc-lastloc);
    lastloc = curloc;
  }
  fde = dwarf_add_fde_inst (fde, dw_op, op1, op2, &dw_error);
  if (Assembly)
    CFA_dump2asm(dw_op, op1, op2);
  if (Trace_Dwarf) 
    fprintf(TFile, "add_fde_inst %x of %lld,%lld\n", dw_op, op1, op2);
  return fde;
}


/* tell dwarf what the new state of the register is, and update our
 * current state to note that.
 */
static Dwarf_P_Fde
update_state (
  Dwarf_P_Fde fde,
  FD_REG fd_reg, 
  CGD_STATES *current_state,
  CGD_STATES new_state )
{
  if ( Trace_Dwarf ) {
    fprintf ( TFile, "<update_state> curloc:%x:%x: %d(r%d) curr:%s new:%s\n",
	      curloc, lastloc, fd_reg, fd_to_dwarf[fd_reg],
	      CGD_Name[current_state[fd_reg]], CGD_Name[new_state] );
  }

  // for SL, return address reg can never change state
  if (fd_reg == FD_RA) {
    return fde;
  }

  if (fd_reg == FD_CFA) {
    if ( new_state == DS_CFA_Is_SP ) {
      /* were we using a different register before? */
      if ( current_state[FD_CFA] == DS_CFA_Is_FP ) {
	fde = Add_Fde_Inst (fde, DW_CFA_def_cfa_register, DW_FRAME_REG29, 0);
      }
      else if (Frame_Len != 0) {
        /* the adjustment is now 0 */
        fde = Add_Fde_Inst (fde, DW_CFA_def_cfa_offset, 0, 0);
      }
    }
    else if ( new_state == DS_CFA_Is_Adj_SP ) {
      if ( current_state[FD_CFA] != DS_CFA_Is_SP)
	DevWarn ("Jumping from state %s to DS_CFA_Is_Adj_SP",
	 CGD_Name[current_state[FD_CFA]] );

      if (Frame_Len != 0) {
        fde = Add_Fde_Inst (fde, DW_CFA_def_cfa_offset, 
				      Frame_Len, 0);
      }
    }
    else if ( new_state == DS_CFA_Is_FP ) {
      if ( current_state[FD_CFA] == DS_CFA_Is_Adj_SP ) {
	fde = Add_Fde_Inst (fde, DW_CFA_def_cfa_offset, 0, 0);
      }

      fde = Add_Fde_Inst (fde, DW_CFA_def_cfa_register, DW_FRAME_REG30, 0);
    }
    else {
      ErrMsg( EC_Misc_Int, "Bad new state for CFA", 
	      (INT) current_state[FD_CFA] );
    }
  } /* end CFA handling */
  else {

    Is_True( new_state == DS_In_Memory || new_state == DS_In_Register,
      ("Invalid new state for fd:%d = %d", fd_reg, (INT) new_state) );

    if ( current_state[fd_reg] == DS_In_Memory ) {
      /* Check that we are not transitioning to the same state. */
      if ((current_state[fd_reg] == new_state) &&
	  	(prev_fd_to_spill_loc[fd_reg] == curr_fd_to_spill_loc[fd_reg])) {
	  	printf("<update_state> ignore redundancy (Mem->Mem)\n");
	if (Trace_Dwarf) fprintf (TFile, "<update_state> ignore redundancy (Mem->Mem)\n");
		return fde;
      }
      /* the debugger wants to use RA_COL to track return address. So, 
       * update its location whenever RA location is changed.
       */
      if (fd_reg == FD_RA) {
	fde = Add_Fde_Inst (fde, DW_CFA_register, DW_FRAME_RA_COL,
				    fd_to_dwarf[fd_reg]);
      }
      else if (fd_reg == FD_SL) {
	fde = Add_Fde_Inst (fde, DW_CFA_register, DW_FRAME_STATIC_LINK,
				    fd_to_dwarf[fd_reg]);
      }
      else {
	// if we are restoring from stack, we switched to same value rule

	    fde = Add_Fde_Inst (fde, DW_CFA_offset, fd_to_dwarf[fd_reg], 
	    	Offset_from_FP (curr_fd_to_spill_loc[fd_reg]) / 
							data_alignment_factor);
      }
    }
    else if ( current_state[fd_reg] == DS_In_Register ) {
      if (fd_to_track[fd_reg] == 0) {
	// we probably don't do anything, but not sure yet - sc
      }
      
      /* Check that we are not transitioning to the same state. */
      if (current_state[fd_reg] == new_state) {
	if (Trace_Dwarf) fprintf (TFile, "<update_state> ignore redundancy (Reg->Reg)\n");
		return fde;
      }
      if (fd_reg == FD_RA) {
        fde = Add_Fde_Inst (fde, DW_CFA_offset, 
		    DW_FRAME_RA_COL, 
		    Offset_from_FP (fd_to_spill_loc[fd_reg]) / 
							data_alignment_factor);
      }
      else if (fd_reg == FD_SL) {
        fde = Add_Fde_Inst (fde, DW_CFA_offset, 
		    DW_FRAME_STATIC_LINK, 
		    Offset_from_FP (fd_to_spill_loc[fd_reg]) / 
							data_alignment_factor);
      }
      else {
        fde = Add_Fde_Inst (fde, DW_CFA_offset, 
		    fd_to_dwarf[fd_reg], 
		    Offset_from_FP (fd_to_spill_loc[fd_reg]) / 
							data_alignment_factor);
      }
    }
    else {
      ErrMsg( EC_Misc_Int, "Invalid current state", 
	      (INT) current_state[fd_reg] );
    }
  }

  current_state[fd_reg] = new_state;
  return fde;
}

/* For a given tn, map it to the corresponding fd_register. */
static FD_REG Machine_Reg_To_FD (TN *tn, UINT32 mc_id, ISA_REGISTER_CLASS c)
{
  FD_REG fd_num;
  if (TN_is_float(tn)) {
    fd_num = mc_id + FD_F20 -20;
  }
  else if (mc_id >= 16 && mc_id <= 23) {
    fd_num = mc_id + FD_S0 - 16;
  }
  else if (mc_id == 30) {
    fd_num = FD_S8;
  }
  else if (mc_id == 28) {
    fd_num = FD_GP;
  }
  else if (mc_id == 2) {
    fd_num = FD_JA;
  }
  else if (mc_id == 31){
    fd_num = FD_RA;
  }
  else {
    //    ErrMsg (EC_Unimplemented, "Save_TN_to_fd: illegal mc_id for TN");
  }
  return fd_num;
}

extern void dump_tn(TN*);
/* For a given tn, map it to the corresponding fd_register. */
static FD_REG Save_TN_to_fd (TN *tn)
{
  UINT32 mc_id;
  FD_REG fd_num;

  Is_True (TN_is_save_reg(tn) || TN_is_ra_reg(tn), 
	   ("illegal TN input to Save_TN_to_fd"));

  mc_id = REGISTER_machine_id (TN_save_rclass(tn), TN_save_reg(tn));
  fd_num = Machine_Reg_To_FD (tn, mc_id, TN_save_rclass(tn));
  if (TN_spill(tn) != NULL) {
    if (fd_to_spill_loc[fd_num] == NULL)
	  fd_to_spill_loc[fd_num] = TN_spill(tn);
  }
  return fd_num;
}


/* For a given tn, map it to the corresponding fd_register. */
static FD_REG TN_to_fd (TN *tn)
{
  UINT32 mc_id;
  FD_REG fd_num;

  mc_id = TN_register(tn);
  fd_num = Machine_Reg_To_FD (tn, mc_id, TN_register_class(tn));
  if (TN_spill(tn) != NULL) {
    fd_to_spill_loc[fd_num] = TN_spill(tn);
  }
  return fd_num;
}


static INT *local_state, *entry_state, *exit_state;

#define LOCAL_STATE(bb)	(local_state[BB_id(bb)])
#define ENTRY_STATE(bb)	(entry_state[BB_id(bb)])
#define EXIT_STATE(bb)	(exit_state[BB_id(bb)])

/* For each basic block, compute the state of the callee save registers.
 * The state can either be that the register has not been saved to memory
 * or that it has been saved to memory. This computation is done in two
 * steps. 
 * 	  1. A local step to determine which registers are saved in each
 *	     basic block.
 *	  2. A global propagation step to determine the state at the 
 *	     start and end of each basic block.
 */
static void Compute_Reg_State (BB *firstbb)
{
  BB *bb;
  OP *op;
  BBLIST *blst;
  BOOL changes;
  INT i;
  ST *slink_sym = NULL;

  if (PU_is_nested_func(Get_Current_PU())) {
	slink_sym = Find_Slink_Symbol(CURRENT_SYMTAB);
  }
  local_state = TYPE_PU_ALLOC_N (INT, PU_BB_Count+2);
  entry_state = TYPE_PU_ALLOC_N (INT, PU_BB_Count+2);
  exit_state = TYPE_PU_ALLOC_N (INT, PU_BB_Count+2);
  memset (exit_state, -1, (PU_BB_Count+2) * sizeof(INT));

  BB_Mark_Unreachable_Blocks ();
  for ( bb = firstbb; bb; bb = BB_next(bb) ) {
    if (BB_unreachable(bb)) continue;
    for (op = BB_first_op(bb); op; op = OP_next(op)) {
      /* we are only interested in saves of callee save registers. */
      if (OP_store(op) && TN_is_save_reg(OP_opnd(op,0))) {
        LOCAL_STATE(bb) |= 1 << Save_TN_to_fd(OP_opnd(op,0));
      }
#if !defined(TARG_SL)
      // look for spill of static link
      if (PU_is_nested_func(Get_Current_PU()) && BB_entry(bb) && OP_store(op)) {
	if (TN_is_register(OP_opnd(op,0)) 
		&& TN_is_static_link_reg(OP_opnd(op,0))
		&& TN_is_symbol(OP_opnd(op,2)) 
		&& TN_var(OP_opnd(op,2)) == slink_sym) 
	{
        	LOCAL_STATE(bb) |= 1 << FD_SL;
	}
      }
#endif
    }
  }

  changes = TRUE;
  while (changes) {
    changes = FALSE;
    for ( bb = firstbb; bb; bb = BB_next(bb) ) {
      INT old_exit_state;
      if (BB_unreachable(bb)) continue;
      if (Trace_Dwarf) fprintf (TFile, "curbb: %d, preds: ", BB_id(bb));
      if (BB_preds(bb) != NULL) {
	INT new_entry_state = -1; /* all 1's */
        FOR_ALL_BB_PREDS (bb, blst) {
	  new_entry_state &= EXIT_STATE(BBLIST_item(blst));
	  if (Trace_Dwarf) fprintf (TFile, "[%d %x], ", 
		BB_id(BBLIST_item(blst)), EXIT_STATE(BBLIST_item(blst)));
	}
	ENTRY_STATE(bb) = new_entry_state;
      } 
      /*
       * If handler of FP-based routine, add FP to entry state,
       * since runtime will restore the SP and FP value.
       */
      if (BB_handler(bb) && (Current_PU_Stack_Model != SMODEL_SMALL))
      {
        ENTRY_STATE(bb) |= 1 << FD_S8;
      }
      if (Trace_Dwarf) fprintf (TFile, "\n entry_state: %x\n", ENTRY_STATE(bb));
      old_exit_state = EXIT_STATE(bb);
      EXIT_STATE(bb) = ENTRY_STATE(bb) | LOCAL_STATE(bb);
      if (old_exit_state != EXIT_STATE(bb)) 
	changes = TRUE;
    }
  }
  if (Trace_Dwarf) {
    fprintf (TFile, "\n**** REG_STATE ****\n");
    for (i = 1; i <= PU_BB_Count; i++) {
      fprintf (TFile, "%4d %8x %8x %8x\n", i, entry_state[i], local_state[i], 
		      exit_state[i]);
    }
  }
}

// search bb for a matching load from spill address to dest tn 
static BOOL
Has_Matching_Load_In_BB ( BB *bb, TN *dest, TN *spill)
{
  OP *op;
  for (op = BB_first_op(bb); op; op = OP_next(op)) {
	if (OP_load(op) && OP_has_result(op)
		&& CLASS_REG_PAIR_EqualP (TN_class_reg(OP_result(op,0)),
					  TN_class_reg(dest) )
		&& OP_opnd(op,1) == spill)
	{
		return TRUE;
	}
  }
  return FALSE;
}

static BOOL
Has_Matching_Store_In_BB ( BB *bb, TN *dest, TN *spill)
{
  OP *op;
  for (op = BB_first_op(bb); op; op = OP_next(op)) {
	if (OP_store(op) 
		&& CLASS_REG_PAIR_EqualP (TN_class_reg(OP_opnd(op,0)),
					  TN_class_reg(dest) )
		&& OP_opnd(op,2) == spill)
	{
		return TRUE;
	}
  }
  return FALSE;
}

#ifdef TARG_SL
/* Generate register masks for every function entry. The mask variables were
   emitted as user-defined CFA in debug_frame FDE. */
static void
Gen_Register_Mask(BB *firstbb, REGISTER_SET *interrupt_saved,
                  REGISTER_SET *ctrl_saved, REGISTER_SET *spe_saved)
{
  BB *bb;
  OP *op;

  REGISTER_SET caller_saved = REGISTER_CLASS_caller_saves(ISA_REGISTER_CLASS_integer);
  REGISTER_SET used_gpr = REGISTER_SET_EMPTY_SET;
  REGISTER_SET used_spe_reg = REGISTER_SET_EMPTY_SET; // special register
  REGISTER_SET used_ctr_reg = REGISTER_SET_EMPTY_SET; // control register

  for(bb = firstbb; bb; bb = BB_next(bb))
  {
    FOR_ALL_BB_OPs(bb, op)
    {
      for(INT i = 0; i < OP_results(op); i++) {
        TN* tn = OP_result(op, i);
        if (TN_is_register(tn) && (TN_register_class(tn) == ISA_REGISTER_CLASS_integer)) {
           used_gpr =  REGISTER_SET_Union(used_gpr, (1 << (TN_register(tn)-1)));
        } else if (tn == JA_TN) {
           used_ctr_reg = REGISTER_SET_Union (used_ctr_reg, 1);
        } else if (tn == RA_TN) {
           used_ctr_reg = REGISTER_SET_Union (used_ctr_reg, 2);
        } else if (TN_is_LoopRegister(tn)) {
          // loop register
           used_ctr_reg = REGISTER_SET_Union(used_ctr_reg, (1 << (TN_number(tn)-TN_number(LC0_TN)+2))); 
        } else if (tn == HI_TN) {
          // HI register
           used_spe_reg = REGISTER_SET_Union(used_spe_reg, 21);
        } else if (TN_is_AddrRegister(tn)) {
           used_spe_reg = REGISTER_SET_Union(used_spe_reg, (1 << (TN_number(tn)-TN_number(Addr0_TN))));
        } else if (TN_is_AccRegister(tn)) {
           used_spe_reg = REGISTER_SET_Union(used_spe_reg, (1 << (TN_number(tn)-TN_number(Acc0_TN)+16)));
        } else if (TN_is_AddrSizeRegister(tn)) {
           used_spe_reg = REGISTER_SET_Union(used_spe_reg, (1 << (TN_number(tn)-TN_number(Addrsize0_TN)+8)));
        }
      }
      if (CG_ISR == 2) {
        for(INT i = 0; i < OP_opnds(op); i++) {
          TN* tn = OP_opnd(op, i);
          if (TN_is_register(tn) && (TN_register_class(tn) == ISA_REGISTER_CLASS_integer)) {
            used_gpr =  REGISTER_SET_Union(used_gpr, (1 << (TN_register(tn)-1)));
          } else if (tn == JA_TN) {
            used_ctr_reg = REGISTER_SET_Union (used_ctr_reg, 1);
          } else if (tn == RA_TN) {
            used_ctr_reg = REGISTER_SET_Union (used_ctr_reg, 2);
          } else if (TN_is_LoopRegister(tn)) {
            // loop register
            used_ctr_reg = REGISTER_SET_Union(used_ctr_reg, (1 << (TN_number(tn)-TN_number(LC0_TN)+2)));
          } else if (tn == HI_TN) {
            // HI register
            used_spe_reg = REGISTER_SET_Union(used_spe_reg, 21);
          } else if (TN_is_AddrRegister(tn)) {
            used_spe_reg = REGISTER_SET_Union(used_spe_reg, (1 << (TN_number(tn)-TN_number(Addr0_TN))));
          } else if (TN_is_AccRegister(tn)) {
            used_spe_reg = REGISTER_SET_Union(used_spe_reg, (1 << (TN_number(tn)-TN_number(Acc0_TN)+16)));
          } else if (TN_is_AddrSizeRegister(tn)) {
           used_spe_reg = REGISTER_SET_Union(used_spe_reg, (1 << (TN_number(tn)-TN_number(Addrsize0_TN)+8)));
          }
        }
      } // CG_ISR == 2
    }
  }
  *interrupt_saved = REGISTER_SET_Intersection(caller_saved, used_gpr);
  *ctrl_saved = used_ctr_reg;
  *spe_saved = used_spe_reg;
}
#endif

/* construct the fde for the current procedure. */
extern Dwarf_P_Fde
Build_Fde_For_Proc (Dwarf_P_Debug dw_dbg, BB *firstbb, LABEL_IDX begin_label, LABEL_IDX end_label,  INT end_offset, INT low_pc, INT high_pc)
{
  return NULL;

  Dwarf_P_Fde fde;
  fde = dwarf_new_fde (dw_dbg, &dw_error);
/*  if (Assembly) // ensure section is now the right one
    fprintf(Asm_File, "\t%s\t%s\n", AS_SECTION, ".debug_frame");
  if (Assembly && (Current_PU_Count() == 0)) {
    CIE_dump2asm(dw_dbg->de_last_cie);
    // only need to emit this once at CIE level
    fde = Add_Fde_Inst (fde,DW_CFA_def_cfa, DW_FRAME_REG29, 0);
  }
  if (Assembly) {
    CIE_end_dump2asm();
    FDE_dump2asm(low_pc, high_pc);
  }
  if (Assembly)
    FDE_end_dump2asm();*/
  return fde;

}

// does unwind follow simple pattern of saves in entry, restores in exit?
static BOOL
Is_Unwind_Simple (void)
{
  if (has_asm) return FALSE;

  return TRUE;
}

// call per-PU
void 
Init_Unwind_Info (BOOL trace)
{
  Trace_Unwind = trace;
  has_asm = FALSE;
  has_create = FALSE;

  //  Find_Unwind_Info ();
  simple_unwind = Is_Unwind_Simple();
  if (Trace_Unwind) {
	fprintf (TFile, "%s unwind\n", (simple_unwind ? "simple" : "complicated"));
	//	Print_All_Unwind_Elem ("unwind2");
  }

  // for use in emit_unwind
  //  ue_iter = ue_list.begin();
}

void 
Finalize_Unwind_Info(void)
{
  //  ue_list.clear();
}

void 
Emit_Unwind_Directives_For_OP(OP *op, FILE *f)
{
}

void
Check_Dwarf_Rel(Elf32_Rel const &current_reloc)
{
  FmtAssert(REL32_type(current_reloc) == R_IA_64_DIR32MSB,
	    ("Unimplemented 32-bit relocation type %d",
	     REL32_type(current_reloc)));
}

void
Check_Dwarf_Rel(Elf64_AltRel const &current_reloc)
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
	0
};

extern BOOL Is_Dwarf_Section_To_Emit(const char *name)
{

	for(int  i = 0; drop_these[i]; ++i) {
	  if(strcmp(name,drop_these[i]) == 0) {
	    return FALSE;
	  }
	}
        return TRUE;
}

