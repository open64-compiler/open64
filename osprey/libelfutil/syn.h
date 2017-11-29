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

$Revision: 1.1.1.1 $

	syn.h
is the 'traditional' name of the header file containing
#defines to change the global function names from the weak
names (appearing in public headers) to the strong names so the
compilation generates strong names.

The point is the same as for libc: one codes the library using
public names and the public header using public (what will be
weak) names.  Then #include this before the public headers and
we get full type checking of the prototypes while the library
compiles and calls using the strong names.

This is a private header, used only in compiling the library.

*/

#ifdef _LIBELF_XTND_EXPANDED_DATA
#define xlate_init_fd_xtnd                   _xlate_init_fd_xtnd
#define xlate_init_elf_xtnd                  _xlate_init_elf_xtnd
#define xlate_named_init_fd_xtnd             _xlate_named_init_fd_xtnd
#define xlate_named_init_elf_xtnd            _xlate_named_init_elf_xtnd
#define xlate_get_info_xtnd                  _xlate_get_info_xtnd
#define xlate_address_xtnd                   _xlate_address_xtnd
#define xlate_get_reg_rule_xtnd              _xlate_get_reg_rule_xtnd
#define xlate_get_all_reg_rules_xtnd         _xlate_get_all_reg_rules_xtnd
#define xlate_expand_reg_info_xtnd           _xlate_expand_reg_info_xtnd
#define xlate_expand_reg_info2_xtnd          _xlate_expand_reg_info2_xtnd
#define xlate_finish_xtnd                    _xlate_finish_xtnd

#define xlate_pro_init_xtnd                  _xlate_pro_init_xtnd
#define xlate_pro_add_info_xtnd              _xlate_pro_add_info_xtnd
#define xlate_pro_add_reg_info_xtnd          _xlate_pro_add_reg_info_xtnd
#define xlate_pro_add_range_xtnd             _xlate_pro_add_range_xtnd
#define xlate_pro_disk_header_xtnd           _xlate_pro_disk_header_xtnd
#define xlate_pro_disk_next_block_xtnd       _xlate_pro_disk_next_block_xtnd
#define xlate_pro_finish_xtnd                _xlate_pro_finish_xtnd
#else
#define xlate_init_fd                   _xlate_init_fd
#define xlate_init_elf                  _xlate_init_elf
#define xlate_named_init_fd             _xlate_named_init_fd
#define xlate_named_init_elf            _xlate_named_init_elf
#define xlate_get_info                  _xlate_get_info
#define xlate_address                   _xlate_address
#define xlate_get_reg_rule              _xlate_get_reg_rule
#define xlate_get_all_reg_rules         _xlate_get_all_reg_rules
#define xlate_expand_reg_info           _xlate_expand_reg_info
#define xlate_expand_reg_info2          _xlate_expand_reg_info2
#define xlate_finish                    _xlate_finish

#define xlate_pro_init                  _xlate_pro_init
#define xlate_pro_add_info              _xlate_pro_add_info
#define xlate_pro_add_reg_info          _xlate_pro_add_reg_info
#define xlate_pro_add_range             _xlate_pro_add_range
#define xlate_pro_disk_header           _xlate_pro_disk_header
#define xlate_pro_disk_next_block       _xlate_pro_disk_next_block
#define xlate_pro_finish                _xlate_pro_finish
#endif
