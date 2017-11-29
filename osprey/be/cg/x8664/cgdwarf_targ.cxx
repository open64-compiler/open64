/*
 * Copyright (C) 2010-2011 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright (C) 2007. PathScale, LLC. All Rights Reserved.
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

#include <stdio.h>
#include <stdlib.h>
#include "libelf/libelf.h"
#include <list>
#include "elf_stuff.h"
#include <elfaccess.h>

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

// call per-PU
void
Init_Unwind_Info (BOOL trace)
{
  return;
}

void
Emit_Unwind_Directives_For_OP(OP *op, FILE *f)
{
  return;
}

void
Finalize_Unwind_Info(void)
{
  return;
}

/* construct the fde for the current procedure. */
extern Dwarf_P_Fde
Build_Fde_For_Proc (Dwarf_P_Debug dw_dbg, BB *firstbb,
		    Dwarf_Unsigned begin_label,
		    Dwarf_Unsigned end_label,
		    Dwarf_Unsigned pushbp_label,
		    Dwarf_Unsigned movespbp_label,
		    Dwarf_Unsigned adjustsp_label,
		    Dwarf_Unsigned callee_saved_reg,
		    INT32     end_offset,
		    // The following two arguments need to go away
		    // once libunwind gives us an interface that
		    // supports symbolic ranges.
		    INT       low_pc,
		    INT       high_pc)
{
  BOOL stack_used = FALSE;
  Dwarf_Error dw_error;
  Dwarf_P_Fde fde;

  if ( ! CG_emit_unwind_info) return NULL;

  {
    OP *op;
    for (op = BB_first_op(firstbb); op != NULL; op = OP_next(op))
    {
      if (OP_result(op, 0) == SP_TN  || 
        OP_code(op) == TOP_pushq || OP_code(op) == TOP_pushl)
      {
        stack_used = TRUE;
        break;
      }
    }
  }

  fde = dwarf_new_fde (dw_dbg, &dw_error);

  if (Current_PU_Stack_Model != SMODEL_SMALL)
  {
    // Generate FDE instructions
    dwarf_add_fde_inst (fde, DW_CFA_advance_loc4, 
		      begin_label, movespbp_label, &dw_error);
    dwarf_add_fde_inst (fde, DW_CFA_def_cfa_offset, 
		      Is_Target_64bit() ? 0x10 : 0x8, 
		      0x0, &dw_error);
    dwarf_add_fde_inst (fde, DW_CFA_offset, Is_Target_64bit() ? 0x6 : 0x5, 
		      0x2, &dw_error);
    dwarf_add_fde_inst (fde, DW_CFA_advance_loc4, 
		      movespbp_label, adjustsp_label, &dw_error);
    dwarf_add_fde_inst (fde, DW_CFA_def_cfa_register, 
		      Is_Target_64bit() ? 0x6 : 0x5, 0x0, &dw_error);
  } else {
    dwarf_add_fde_inst (fde, DW_CFA_advance_loc4, 
		      begin_label, adjustsp_label, &dw_error);
    if (stack_used && Frame_Len != 0)
      dwarf_add_fde_inst (fde, DW_CFA_def_cfa_offset, 
		      Frame_Len + (Push_Pop_Int_Saved_Regs() + 1)*(Is_Target_64bit()?8:4), 
		      0x0, &dw_error);
    else
      dwarf_add_fde_inst (fde, DW_CFA_def_cfa_offset, 
		      Is_Target_64bit()?8:4,
		      0x0, &dw_error);
  }
  if (stack_used && Cgdwarf_Num_Callee_Saved_Regs() != 0) {
    INT num = Cgdwarf_Num_Callee_Saved_Regs();    
    dwarf_add_fde_inst (fde, DW_CFA_advance_loc4, 
			adjustsp_label,
		 	callee_saved_reg, &dw_error);
    for (INT i = num - 1; i >= 0; i --) {
      TN* tn = Cgdwarf_Nth_Callee_Saved_Reg(i);
      ST* sym = Cgdwarf_Nth_Callee_Saved_Reg_Location(i);
      INT n = Is_Target_64bit() ? 16 : 8;
      // data alignment factor
      INT d_align = Is_Target_64bit() ? 8 : 4;
      INT64 d_frame_len;
      mUINT8 reg_id = REGISTER_machine_id (TN_register_class(tn), TN_register(tn));

      d_frame_len = Frame_Len/d_align + (Push_Pop_Int_Saved_Regs() + 1);
      // If we need the DWARF register id's for all registers, we need a 
      // general register mapping from REGISTER_machine_id to DWARF register
      // id. But the following suffices for this case,
      // The machine_id is the same as the DWARF id for all callee-saved 
      // registers except rbx, so give it the proper id here.
      //
      // And for -m32, handle the 2 additional callee-saved registers
      if (Is_Target_32bit())
      {
      	if (reg_id == 5) // %esi
	  reg_id = 6;
	else if (reg_id == 4) // %edi
	  reg_id = 7;
	else if (reg_id == 2) // %ebp
	  reg_id = 5; 
      } else {
	if (reg_id == 2) //%rbp
	  reg_id = 6; 
      }
      if (reg_id == 1) reg_id = 3; // %rbx
      if (Current_PU_Stack_Model != SMODEL_SMALL)
        dwarf_add_fde_inst (fde, DW_CFA_offset, reg_id,
	          (((ST_base(sym) == FP_Sym) ? -1 : 1)*ST_ofst(sym)+n)/d_align,
	          &dw_error);
      else 
        dwarf_add_fde_inst (fde, DW_CFA_offset, reg_id,
	          d_frame_len - (ST_ofst(sym))/d_align,
	          &dw_error);

    }
  }
  
  return fde;
}


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
static const char *drop_these[] = {
#if ! defined(BUILD_OS_DARWIN)
      // Assembler generates .debug_line from directives itself, so we
      // don't output it.
	DEBUG_LINE_SECTNAME,
#endif /* defined(BUILD_OS_DARWIN) */
#if defined(BUILD_OS_DARWIN)
     /* Note following comment: if gdb doesn't use them, why aren't we omitting
      * them? */
     // gdb does not use the MIPS sections
     // debug_weaknames, etc.
	DEBUG_VARNAMES_SECTNAME,
	DEBUG_TYPENAMES_SECTNAME,
	DEBUG_WEAKNAMES_SECTNAME,
	DEBUG_FUNCNAMES_SECTNAME,
     // we don't use debug_frame in IA-64.
#endif
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
        // Bug 1516 - do not emit .debug_* sections if not -g
	if (Debug_Level < 1 &&
#if defined(BUILD_OS_DARWIN)
	is_debug_section(name)
#else /* defined(BUILD_OS_DARWIN) */
	strncmp(name, ".debug_", 7) == 0
#endif /* defined(BUILD_OS_DARWIN) */
	)
	  return FALSE;
        return TRUE;
}

