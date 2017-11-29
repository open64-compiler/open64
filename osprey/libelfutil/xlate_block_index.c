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

  xlate_block_index.c

  $Revision: 1.1.1.1 $

  The index functions return 0 or a negative error code.
  In addition they set the out-addresses 
  giving the range of addresses covered by the block
  if the returned value is 0.

  These functions are referenced to initialize function pointers
  so they all all get linked in from the archive.

*/

#include "xlateincl.h"

/*  Setup the uniform block header as well as possible
**  without calling anyone.
**  Must set all fields in uniform_block_hdr with something.
*/
static void
_xlate_basic_ubh_setup_v1(xlate_table_con tab,
	Uword index,
	xlate_blockheader_v1 *bhdrp,
        uniform_block_hdr * blk_info_out)
{
	/* setting  ub_low_old_addr imprecisesly */
	blk_info_out->ub_low_old_addr   =  tab->xc_hdr.ich_old_addr_low;
	/* setting ub_high_old_addr imprecisely */
        blk_info_out->ub_high_old_addr  = tab->xc_hdr.ich_old_addr_high;

	
	if(index < (tab->xc_hdr.ich_num_blocks-1) ) {
	     blk_info_out->ub_high_new_addr = (bhdrp+1)->bh_first_new_addr;
	} else {
	     blk_info_out->ub_high_new_addr   = tab->xc_hdr.ich_new_addr_high;
	}
        blk_info_out->ub_low_new_addr = bhdrp->bh_first_new_addr;

        blk_info_out->ub_first_new_addr = bhdrp->bh_first_new_addr;
        blk_info_out->ub_first_old_addr = bhdrp->bh_first_old_addr;

        blk_info_out->ub_entrycount     = bhdrp->bh_num_entries;
        blk_info_out->ub_data_bytes     = 
		tab->xc_leb_data_blocks + 
		index*tab->xc_hdr.ich_block_size;
        blk_info_out->ub_data_end       =
                blk_info_out->ub_data_bytes +
                tab->xc_hdr.ich_block_size;

	blk_info_out->ub_block_index = index;
	blk_info_out->ub_ubh_is_set_up = 1;
}

/*
  Revised 10/98. No longer needs to do the xc_find_old_high_addr_of_block.
*/
int _xlate_index_into_blockhdrs_v1(xlate_table_con tab,
        int   isNewAddress,
        Uword blockindex,
        Elf64_Addr *address_low_out,
        Elf64_Addr *address_high_out,
        uniform_block_hdr * blk_info_out)
{
	int retstatus = XLATE_TB_STATUS_NO_ERROR;

	xlate_blockheader_v1 *bhdrp = 
	  (xlate_blockheader_v1 *)tab->xc_block_headers;

	if(blockindex >= tab->xc_hdr.ich_num_blocks) {
		return XLATE_TB_STATUS_BAD_BLOCK_INDEX;
	}
	bhdrp = bhdrp + blockindex;
        if(!blk_info_out->ub_ubh_is_set_up ) {
	   _xlate_basic_ubh_setup_v1(tab,
        	blockindex,
        	bhdrp,
        	blk_info_out);

        }
	if(isNewAddress) {
	  *address_low_out = bhdrp->bh_first_new_addr;
	  if(blockindex < (tab->xc_hdr.ich_num_blocks-1) ) {
	     *address_high_out = (bhdrp+1)->bh_first_new_addr;
	  } else {
	     /* end case */
	     *address_high_out = tab->xc_hdr.ich_new_addr_high;
	  }
	} else {
	    *address_low_out = bhdrp->bh_first_old_addr;
	    if(blockindex < (tab->xc_hdr.ich_num_blocks-1) ) {
	        *address_high_out = (bhdrp+1)->bh_first_old_addr;
	    } else {
		/* end case */
	        *address_high_out = tab->xc_hdr.ich_old_addr_high;
	    }
	}
        return retstatus;

}
int _xlate_index_into_blockhdrs_v2_32(xlate_table_con tab,
        int   isNewAddress,
        Uword blockindex,
        Elf64_Addr *address_low_out,
        Elf64_Addr *address_high_out, 
        uniform_block_hdr * blk_info_out )

{
	int retstatus = XLATE_TB_STATUS_NO_ERROR;
        xlate_blockheader32_v2 *bhdrp =
          (xlate_blockheader32_v2 *)tab->xc_block_headers;


        if(blockindex >= tab->xc_hdr.ich_num_blocks) {
                return XLATE_TB_STATUS_BAD_BLOCK_INDEX;
        }
        bhdrp = bhdrp + blockindex;

        if(isNewAddress) {
          *address_low_out = bhdrp->bh_first_new_addr;
          if(blockindex < (tab->xc_hdr.ich_num_blocks-1) ) {
             *address_high_out = (bhdrp+1)->bh_first_new_addr;
          } else {
             *address_high_out = tab->xc_hdr.ich_new_addr_high;
          }
        } else {
          *address_low_out = bhdrp->bh_low_old_addr;
	  *address_high_out = bhdrp->bh_high_old_addr;
        }

        if(blk_info_out->ub_ubh_is_set_up == 1) {
		return retstatus;
	}

	if(blockindex < (tab->xc_hdr.ich_num_blocks-1) ) {
	            blk_info_out->ub_high_new_addr = 
				(bhdrp+1)->bh_first_new_addr;
	} else {
	            blk_info_out->ub_high_new_addr = 
				tab->xc_hdr.ich_new_addr_high;
	}
        blk_info_out->ub_low_new_addr =
                        bhdrp->bh_first_new_addr;

        blk_info_out->ub_first_new_addr = bhdrp->bh_first_new_addr;
        blk_info_out->ub_first_old_addr = bhdrp->bh_first_old_addr;

        blk_info_out->ub_low_old_addr =
                        bhdrp->bh_low_old_addr;
        blk_info_out->ub_high_old_addr =
                        bhdrp->bh_high_old_addr;

        blk_info_out->ub_entrycount = 
                        bhdrp->bh_num_entries;
	blk_info_out->ub_data_bytes =
			tab->xc_leb_data_blocks +
                        blockindex*tab->xc_hdr.ich_block_size;
	blk_info_out->ub_data_end =
			blk_info_out->ub_data_bytes + 
			tab->xc_hdr.ich_block_size;
	blk_info_out->ub_block_index = blockindex;
	blk_info_out->ub_ubh_is_set_up = 1;


        return retstatus;

}
int _xlate_index_into_blockhdrs_v2_64(xlate_table_con tab,
        int   isNewAddress,
        Uword blockindex,
        Elf64_Addr *address_low_out,
        Elf64_Addr *address_high_out, 
        uniform_block_hdr * blk_info_out )
{
	int retstatus = XLATE_TB_STATUS_NO_ERROR;
        xlate_blockheader64_v2 *bhdrp =
          (xlate_blockheader64_v2 *)tab->xc_block_headers;
        if(blockindex >= tab->xc_hdr.ich_num_blocks) {
                return XLATE_TB_STATUS_BAD_BLOCK_INDEX;
        }
        bhdrp = bhdrp + blockindex;

        if(isNewAddress) {
          *address_low_out = bhdrp->bh_first_new_addr;
          if(blockindex < (tab->xc_hdr.ich_num_blocks-1) ) {
             *address_high_out = (bhdrp+1)->bh_first_new_addr;
          } else {
             *address_high_out = tab->xc_hdr.ich_new_addr_high;
          }
        } else {
          *address_low_out = bhdrp->bh_low_old_addr;
	  *address_high_out = bhdrp->bh_high_old_addr;
        }
        if(blk_info_out->ub_ubh_is_set_up == 1) {
                return retstatus;
        }


	if(blockindex < (tab->xc_hdr.ich_num_blocks-1) ) {
	            blk_info_out->ub_high_new_addr = 
				(bhdrp+1)->bh_first_new_addr;
	} else {
	            blk_info_out->ub_high_new_addr = 
				tab->xc_hdr.ich_new_addr_high;
	}
        blk_info_out->ub_high_old_addr =
                        bhdrp->bh_high_old_addr;

        blk_info_out->ub_first_new_addr = bhdrp->bh_first_new_addr;
        blk_info_out->ub_first_old_addr = bhdrp->bh_first_old_addr;

        blk_info_out->ub_low_new_addr =
                        bhdrp->bh_first_new_addr;
        blk_info_out->ub_low_old_addr =
                        bhdrp->bh_low_old_addr;

        blk_info_out->ub_entrycount = 
                        bhdrp->bh_num_entries;
        blk_info_out->ub_data_bytes =
                        tab->xc_leb_data_blocks +
                        blockindex*tab->xc_hdr.ich_block_size;
        blk_info_out->ub_data_end =
                        blk_info_out->ub_data_bytes +
                        tab->xc_hdr.ich_block_size;
	blk_info_out->ub_block_index = blockindex;
        blk_info_out->ub_ubh_is_set_up = 1;


        return retstatus;
}
