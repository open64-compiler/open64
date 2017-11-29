/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License 
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU Lesser General Public 
  License along with this program; if not, write the Free Software 
  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
  USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


/*

  xlate_expand_reg.c

  $Revision: 1.1.1.1 $

  These are used little: mainly by elfdump.

*/

#include "xlateincl.h"

#ifdef _LIBELF_XTND_EXPANDED_DATA
#pragma weak xlate_expand_reg_info2_xtnd = _xlate_expand_reg_info2_xtnd
#elif defined(BUILD_OS_DARWIN)
#else
#pragma weak xlate_expand_reg_info2 = _xlate_expand_reg_info2
#endif

/* preferred version */
int
xlate_expand_reg_info2(xlate_table_con tab,
	Elf64_Xword * num_instrs,
	xlate_reg_instr2 ** instructions)
{
        Dwarf_Regtable regtab;
	int retstatus;

        retstatus = _xlate_expand_reg_info_internal(tab,
                ALL_INSTRUCTIONS,
                /* pc */0,
                &regtab,
                num_instrs,
		instructions);

	return retstatus;
}

