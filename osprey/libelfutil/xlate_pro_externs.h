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
   Contains the list of internal-to-xlate
   producer
   externally-defined functions.

   xlate_pro_externs.h

   $Revision: 1.1.1.1 $
   $Date: 2005/10/21 19:00:00 $

*/

#ifdef _LIBELF_XTND_EXPANDED_DATA
#define _xlate_pro_add_range_ps32 _xlate_pro_add_range_ps32_xtnd
#define _xlate_pro_add_range_ps64 _xlate_pro_add_range_ps64_xtnd
#define _xlate_pro_add_range_ge32 _xlate_pro_add_range_ge32_xtnd
#define _xlate_pro_add_range_ge64 _xlate_pro_add_range_ge64_xtnd
#define _xlate_pro_add_range_po32 _xlate_pro_add_range_po32_xtnd
#define _xlate_pro_add_range_po64 _xlate_pro_add_range_po64_xtnd
#define _xlate_merge_range _xlate_merge_range_xtnd
#define _xlate_final_update_highwater_addrs _xlate_final_update_highwater_addrs_xtnd
#define _xlate_pro_reset_saved_range  _xlate_pro_reset_saved_range_xtnd
#define _xlate_add_tentative_new_range _xlate_add_tentative_new_range_xtnd
#endif

int _xlate_pro_add_range_ps32(xlate_table_pro tab,
        Block_s     **ppblk_head,
        Block_s     **ppblk_tail,
        Elf64_Addr    new_addr,
        Elf64_Xword   new_range,
        Elf64_Addr    old_addr,
        Elf64_Xword   old_range,
        highwater_mark *highwater);
int _xlate_pro_add_range_ps64(xlate_table_pro tab,
        Block_s     **ppblk_head,
        Block_s     **ppblk_tail,
        Elf64_Addr    new_addr,
        Elf64_Xword   new_range,
        Elf64_Addr    old_addr,
        Elf64_Xword   old_range,
        highwater_mark *highwater);

int _xlate_pro_add_range_ge32(xlate_table_pro tab,
        Block_s     **ppblk_head,
        Block_s     **ppblk_tail,
        Elf64_Addr    new_addr,
        Elf64_Xword   new_range,
        Elf64_Addr    old_addr,
        Elf64_Xword   old_range,
        highwater_mark *highwater);
int _xlate_pro_add_range_ge64(xlate_table_pro tab,
        Block_s     **ppblk_head,
        Block_s     **ppblk_tail,
        Elf64_Addr    new_addr,
        Elf64_Xword   new_range,
        Elf64_Addr    old_addr,
        Elf64_Xword   old_range,
        highwater_mark *highwater);


int _xlate_pro_add_range_po32(xlate_table_pro tab,
        Block_s     **ppblk_head,
        Block_s     **ppblk_tail,
        Elf64_Addr    new_addr,
        Elf64_Xword   new_range,
        Elf64_Addr    old_addr,
        Elf64_Xword   old_range,
        highwater_mark *highwater);
int _xlate_pro_add_range_po64(xlate_table_pro tab,
        Block_s     **ppblk_head,
        Block_s     **ppblk_tail,
        Elf64_Addr    new_addr,
        Elf64_Xword   new_range,
        Elf64_Addr    old_addr,
        Elf64_Xword   old_range,
        highwater_mark *highwater);


int _xlate_merge_range(xlate_table_pro     table,
        Elf64_Addr  new_address,
        Elf64_Xword new_range,
        Elf64_Addr  old_address,
        Elf64_Xword old_range);

void _xlate_final_update_highwater_addrs(xlate_table_pro tab,
        Block_s *blk, highwater_mark *highwater);

void _xlate_pro_reset_saved_range(xlate_table_pro tab);

int
_xlate_add_tentative_new_range(xlate_table_pro     table,
        Elf64_Addr  new_address,
        Elf64_Xword new_range,
        Elf64_Addr  old_address,
        Elf64_Xword old_range);

