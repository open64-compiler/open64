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
   externally-defined functions.

   xlatexterns.h

   $Revision: 1.1.1.1 $
   $Date: 2005/10/21 19:00:00 $

*/

#ifdef _LIBELF_XTND_EXPANDED_DATA
#define _xlate_get_consumer_reginfo_block _xlate_get_consumer_reginfo_block_xtnd
#define _xlate_get_infov1    _xlate_get_infov1_xtnd
#define _xlate_get_infov2_32 _xlate_get_infov2_32_xtnd
#define _xlate_get_infov2_64 _xlate_get_infov2_64_xtnd
#define _xlate_binary_search_for_addr _xlate_binary_search_for_addr_xtnd
#define _xlate_special_search_for_addr _xlate_special_search_for_addr_xtnd
#define _xlate_index_into_blockhdrs_v1 _xlate_index_into_blockhdrs_v1_xtnd
#define _xlate_index_into_blockhdrs_v2_32 _xlate_index_into_blockhdrs_v2_32_xtnd
#define _xlate_index_into_blockhdrs_v2_64 _xlate_index_into_blockhdrs_v2_64_xtnd
#define _xlate_get_range_from_block_v1_ps _xlate_get_range_from_block_v1_ps_xtnd
#define _xlate_get_range_from_block_v1_po _xlate_get_range_from_block_v1_po_xtnd
#define _xlate_get_range_from_block_v1_ge _xlate_get_range_from_block_v1_ge_xtnd
#define _xlate_get_range_from_block_v2_32_ps _xlate_get_range_from_block_v2_32_ps_xtnd
#define _xlate_get_range_from_block_v2_32_po _xlate_get_range_from_block_v2_32_po_xtnd
#define _xlate_get_range_from_block_v2_32_ge _xlate_get_range_from_block_v2_32_ge_xtnd
#define _xlate_get_range_from_block_v2_64_ps _xlate_get_range_from_block_v2_64_ps_xtnd
#define _xlate_get_range_from_block_v2_64_po _xlate_get_range_from_block_v2_64_po_xtnd
#define _xlate_get_range_from_block_v2_64_ge _xlate_get_range_from_block_v2_64_ge_xtnd
#define _xlate_expand_reg_info_internal _xlate_expand_reg_info_internal_xtnd
#define _xlate_expand_reg_info_internal_given_ptrs _xlate_expand_reg_info_internal_given_ptrs_xtnd
#define _xlate_get_out_from_ranges _xlate_get_out_from_ranges_xtnd
#define _xlate_fix_addresses_for_dso_movement _xlate_fix_addresses_for_dso_movement_xtnd
#endif

void
_xlate_get_consumer_reginfo_block(xlate_table_con tab,
	char **regdata,
	Uword regdata_len);
	

void _xlate_get_infov1(xlate_table_con tab,
    Elf64_Sxword *      dataMoved,
    Elf64_Addr *        startup_fwa,
    Elf64_Addr *        startup_lwa,
    Elf64_Xword *       number_of_ranges,
    int        *        old_text_exists,
    int        *        old_text_alloc);
void _xlate_get_infov2_32(xlate_table_con tab,
    Elf64_Sxword *      dataMoved,
    Elf64_Addr *        startup_fwa,
    Elf64_Addr *        startup_lwa,
    Elf64_Xword *       number_of_ranges,
    int        *        old_text_exists,
    int        *        old_text_alloc);
void _xlate_get_infov2_64(xlate_table_con tab,
    Elf64_Sxword *      dataMoved,
    Elf64_Addr *        startup_fwa,
    Elf64_Addr *        startup_lwa,
    Elf64_Xword *       number_of_ranges,
    int        *        old_text_exists,
    int        *        old_text_alloc);

int _xlate_binary_search_for_addr(xlate_table_con tab,
        int isNewAddress,
        Elf64_Addr addr_in,
        Elf64_Addr* addr_out,
        xlate_block *range_out);
int _xlate_special_search_for_addr(xlate_table_con tab,
        int isNewAddress,
        Elf64_Addr addr_in,
        Elf64_Addr* addr_out,
        xlate_block *range_out);

int _xlate_index_into_blockhdrs_v1(xlate_table_con tab,
        int   isNewAddress,
        Uword blockindex,
        Elf64_Addr *address_low_out,
        Elf64_Addr *address_high_out,
        uniform_block_hdr * blk_info_out);
int _xlate_index_into_blockhdrs_v2_32(xlate_table_con tab,
        int   isNewAddress,
        Uword blockindex,
        Elf64_Addr *address_low_out,
        Elf64_Addr *address_high_out,
        uniform_block_hdr * blk_info_out);
int _xlate_index_into_blockhdrs_v2_64(xlate_table_con tab,
        int   isNewAddress,
        Uword blockindex,
        Elf64_Addr *address_low_out,
        Elf64_Addr *address_high_out,
        uniform_block_hdr * blk_info_out);




int _xlate_get_range_from_block_v1_ps(xlate_table_con tab,
	int isNewAddress, /* 0,1, or 2 */
        Elf64_Addr addr_in,
	int restart,
        uniform_block_hdr *blk_hdr,
        Elf64_Addr* addr_out,
        xlate_block *range_out);
int _xlate_get_range_from_block_v1_po(xlate_table_con tab,
	int isNewAddress, /* 0,1, or 2 */
        Elf64_Addr addr_in,
	int restart,
        uniform_block_hdr *blk_hdr,
        Elf64_Addr* addr_out,
        xlate_block *range_out);
int _xlate_get_range_from_block_v1_ge(xlate_table_con tab,
	int isNewAddress, /* 0,1, or 2 */
        Elf64_Addr addr_in,
	int restart,
        uniform_block_hdr *blk_hdr,
        Elf64_Addr* addr_out,
        xlate_block *range_out);
int _xlate_get_range_from_block_v2_32_ps(xlate_table_con tab,
	int isNewAddress, /* 0,1, or 2 */
        Elf64_Addr addr_in,
	int restart,
        uniform_block_hdr *blk_hdr,
        Elf64_Addr* addr_out,
        xlate_block *range_out);
int _xlate_get_range_from_block_v2_32_po(xlate_table_con tab,
	int isNewAddress, /* 0,1, or 2 */
        Elf64_Addr addr_in,
	int restart,
        uniform_block_hdr *blk_hdr,
        Elf64_Addr* addr_out,
        xlate_block *range_out);
int _xlate_get_range_from_block_v2_32_ge(xlate_table_con tab,
	int isNewAddress, /* 0,1, or 2 */
        Elf64_Addr addr_in,
	int restart,
        uniform_block_hdr *blk_hdr,
        Elf64_Addr* addr_out,
        xlate_block *range_out);
int _xlate_get_range_from_block_v2_64_ps(xlate_table_con tab,
	int isNewAddress, /* 0,1, or 2 */
        Elf64_Addr addr_in,
	int restart,
        uniform_block_hdr *blk_hdr,
        Elf64_Addr* addr_out,
        xlate_block *range_out);
int _xlate_get_range_from_block_v2_64_po(xlate_table_con tab,
	int isNewAddress, /* 0,1, or 2 */
        Elf64_Addr addr_in,
	int restart,
        uniform_block_hdr *blk_hdr,
        Elf64_Addr* addr_out,
        xlate_block *range_out);
int _xlate_get_range_from_block_v2_64_ge(xlate_table_con tab,
	int isNewAddress, /* 0,1, or 2 */
        Elf64_Addr addr_in,
	int restart,
        uniform_block_hdr *blk_hdr,
        Elf64_Addr* addr_out,
        xlate_block *range_out);


int _xlate_expand_reg_info_internal(xlate_table_con tab,
        int wantInstrs, /* 0 if want instrs (ignore pc),
                1 if want dwarf reg rules (at a pc),
                2 if want one reg rule (at a pc) */
        Elf64_Addr       pc,
        Dwarf_Regtable * regtable,
        Elf64_Xword    * num_instrs,
        xlate_reg_instr2 ** instrs);


int _xlate_expand_reg_info_internal_given_ptrs(
        char *reginfo,
        char *reginfo_end,
        int  is64bit,
	int  table_version,
        int wantInstrs, /* 0 if want instrs (ignore pc),
                1 if want dwarf reg rules for all rules (at a pc),
                2 if want one reg rule (at a pc) */
        Elf64_Addr       inputpc,
        Dwarf_Regtable * regtable, /* required */
        Elf64_Xword    * num_instrs, /* may be 0 */
        xlate_reg_instr2 ** instrs_out);/* may be 0 */


Elf64_Addr _xlate_get_out_from_ranges(Elf64_Addr addr_in,
        Elf64_Addr  base_addr1,
        Elf64_Addr  base_addr2,
        Elf64_Xword range2);

