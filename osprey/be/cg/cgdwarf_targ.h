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


#ifndef cgdwarf_targ_INCLUDED
#define cgdwarf_targ_INCLUDED

extern BOOL Trace_Dwarf;

/* construct the fde for the current procedure. */
#ifndef TARG_X8664
extern Dwarf_P_Fde Build_Fde_For_Proc (Dwarf_P_Debug  dw_dbg,
				       BB            *firstbb,
				       LABEL_IDX      begin_label,
				       LABEL_IDX      end_label,
				       INT32          end_offset,
				       // The following two arguments
				       // need to go away once
				       // libunwind gives us an
				       // interface that supports
				       // symbolic ranges.
				       INT       low_pc,
				       INT       high_pc);
#else
extern Dwarf_P_Fde Build_Fde_For_Proc (Dwarf_P_Debug  dw_dbg,
				       BB            *firstbb,
				       Dwarf_Unsigned      begin_label,
				       Dwarf_Unsigned      end_label,
				       Dwarf_Unsigned      pushbp_label,
				       Dwarf_Unsigned      movespbp_label,
				       Dwarf_Unsigned      adjustsp_label,
				       Dwarf_Unsigned      callee_saved_reg,
				       INT32          end_offset,
				       // The following two arguments
				       // need to go away once
				       // libunwind gives us an
				       // interface that supports
				       // symbolic ranges.
				       INT       low_pc,
				       INT       high_pc);
#endif // TARG_X8664

extern void Check_Dwarf_Rel(const Elf32_Rel &);
extern void Check_Dwarf_Rel(const Elf64_AltRel &);
extern void Check_Dwarf_Rela(const Elf64_AltRela &);
extern void Check_Dwarf_Rela(const Elf32_Rela &);
extern BOOL Is_Dwarf_Section_To_Emit(const char *name);

extern void Init_Unwind_Info (BOOL trace);
extern void Finalize_Unwind_Info(void);
extern void Emit_Unwind_Directives_For_OP(OP *op, FILE *f);

#endif /* cgdwarf_targ_INCLUDED */
