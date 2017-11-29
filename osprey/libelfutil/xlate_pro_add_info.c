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
   xlate_pro_add_info.c
   $Revision: 1.1.1.1 $


*/

#include "xlateincl.h"

#ifdef _LIBELF_XTND_EXPANDED_DATA
#pragma weak xlate_pro_add_info_xtnd = _xlate_pro_add_info_xtnd
#elif defined(BUILD_OS_DARWIN)
#else
#pragma weak xlate_pro_add_info = _xlate_pro_add_info
#endif

int xlate_pro_add_info(xlate_table_pro     table,
    Elf64_Sxword         data_moved,
    Elf64_Addr          startup_fwa,
    Elf64_Addr          startup_lwa,
    Elf32_Word          old_text_exists,
    Elf32_Word          old_text_alloc)

{
   int retstatus  = XLATE_TB_STATUS_NO_ERROR;
   if(table->tb_magic != PRO_MAGIC_VAL) {
        return XLATE_TB_STATUS_INVALID_TABLE;
   }
   if(table->tb_no_new_entries_allowed ) {
	return XLATE_TB_STATUS_ADD_TOO_LATE;
   }
        /* from xlate_pro_add_info */
   table->tb_data_moved = data_moved;
   table->tb_startup_fwa = startup_fwa;
   table->tb_startup_lwa = startup_lwa;
   table->tb_old_text_exists = (unsigned char)old_text_exists;
   table->tb_old_text_alloc = (unsigned char)old_text_alloc;

   return retstatus;
}


