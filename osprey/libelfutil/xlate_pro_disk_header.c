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
   xlate_pro_disk_header.c
   $Revision: 1.1.1.1 $


*/

#include "xlateincl.h"

#ifdef _LIBELF_XTND_EXPANDED_DATA
#pragma weak xlate_pro_disk_header_xtnd = _xlate_pro_disk_header_xtnd
#elif defined(BUILD_OS_DARWIN)
#else
#pragma weak xlate_pro_disk_header = _xlate_pro_disk_header
#endif


#define XLATE_PRO_STANDARD_SETUP  1
#define XLATE_PRO_DEBUG_SETUP     2

int xlate_pro_disk_header(xlate_table_pro  table,
    int         standard_or_debug,
    Elf64_Xword *       ototal_memory_req,
    Elf64_Xword *       onum_blocks)

{
    Uword       total_memory_req = 0;
    Uword       num_blocks = 0;
    int retstatus = XLATE_TB_STATUS_NO_ERROR;

    int tretstatus;

    if(table->tb_magic != PRO_MAGIC_VAL) {
        return XLATE_TB_STATUS_INVALID_TABLE;
    }
 
    /* Finish up any tentative output if present
    */
    retstatus = _xlate_add_tentative_new_range(table,0, 0, 0, 0);
    if(retstatus != XLATE_TB_STATUS_NO_ERROR) {
	return retstatus;
    }

    table->tb_no_new_entries_allowed = 1;
    table->tb_std_header_returned = 0;
    table->tb_debug_header_returned = 0;
    table->tb_reginfoRet = 0;
    table->tb_header_set_to_std_or_debug = standard_or_debug;
    if(standard_or_debug == XLATE_PRO_STANDARD_SETUP) {
	table->tb_header_called_on_std = 1;
	table->tb_blockRet = table->tb_std_block_head;
	_xlate_final_update_highwater_addrs(table,
		table->tb_std_block_tail,
		&table->tb_std_highwater);

	/* determine if the output is empty */
	if(table->tb_blockRet == 0 && table->tb_regInfoOffset == 0) {
	   xlate_table_con contab = table->tb_con_table;
	   if(contab) {
		if(contab->xc_hdr.ich_total_reginfo_bytes == 0) {
		   /* No need to look at con_table translation
	           ** bytes since there is no input range to
		   ** merge them with.  
		   ** just check con table reginfo.
		   */
    		   *ototal_memory_req = 0;
    		   *onum_blocks = 0;
    		   return retstatus;
		}
	   } else {
    		*ototal_memory_req = 0;
    		*onum_blocks = 0;
    		return retstatus;
	   }
	}

    } else if (standard_or_debug == XLATE_PRO_DEBUG_SETUP) {
	table->tb_header_called_on_debug = 1;
	table->tb_blockRet = table->tb_debug_block_head;
	/* determine if the output is empty */
	if(table->tb_blockRet == 0 && table->tb_regInfoOffset == 0) {
    		*ototal_memory_req = 0;
    		*onum_blocks = 0;
    		return retstatus;
        }
	_xlate_final_update_highwater_addrs(table,
		table->tb_debug_block_tail,
		&table->tb_debug_highwater);



    } else {
	retstatus = XLATE_TB_STATUS_PRO_REQ_INVALID;
	return retstatus;
    }


    /* do an initial call on each to get total size */
    for(;;) {
	Elf64_Xword len;
	tretstatus = xlate_pro_disk_next_block(table,
		 /*data=*/0,
		 &len);
	if(tretstatus == XLATE_TB_STATUS_NO_ERROR) {
		total_memory_req += len;
		num_blocks += 1;
		continue;
	} else if (tretstatus == XLATE_TB_STATUS_ALREADY_DONE) {
		break;
	}
	/* major failure!? */
	return tretstatus;
    }



    /* now reset for user to call for real. */
    table->tb_no_new_entries_allowed = 1;
    table->tb_std_header_returned = 0;
    table->tb_debug_header_returned = 0;
    table->tb_reginfoRet = 0;
    table->tb_header_set_to_std_or_debug = standard_or_debug;
    if(standard_or_debug == XLATE_PRO_STANDARD_SETUP) {
	table->tb_header_called_on_std = 1;
	table->tb_blockRet = table->tb_std_block_head;
    } else if (standard_or_debug == XLATE_PRO_DEBUG_SETUP) {
	table->tb_header_called_on_debug = 1;
	table->tb_blockRet = table->tb_debug_block_head;
    } else {
	retstatus = XLATE_TB_STATUS_PRO_REQ_INVALID;
	return retstatus;
    }

    *ototal_memory_req = total_memory_req;
    *onum_blocks = num_blocks;
    return retstatus;
}

