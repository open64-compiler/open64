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

  xlate_block_search.c

  $Revision: 1.1.1.1 $

  These functions are referenced to initialize function pointers
  so they all all get linked in from the archive.

*/

#include "xlateincl.h"

#ifndef TURN_OFF_MEMO
/*
    See if the requested address 
    is in this block we recall using last.

*/
static int
look_in_memo_data(xlate_table_con tab,
	int isNewAddress,
	struct memo_s *memodata,
        Elf64_Addr addr_in,
        Elf64_Addr* addr_out,
        xlate_block *range_out)
{
  int retstatus;
  if(isNewAddress) {
	xlate_block *xbp;
	if(addr_in < memodata->mm_low_new_addr ||
		addr_in >= memodata->mm_high_new_addr) {
  		return XLATE_TB_STATUS_NO_SUCH_ADDR_IN_TABLE;
	}
	xbp = &memodata->mm_block;
	if(addr_in >= xbp->xe_new_address &&
		addr_in < (xbp->xe_new_address + xbp->xe_new_range)) {
		*addr_out = _xlate_get_out_from_ranges(addr_in,
			xbp->xe_new_address,
			xbp->xe_old_address,
			xbp->xe_old_range);
		if(range_out) {
		  *range_out = *xbp;
		}
	        return XLATE_TB_STATUS_NO_ERROR;
		
	}
	/* Maybe later in this block: continue
	   where we left off in block
 	*/
        retstatus = tab->xc_get_range_from_block(tab,
		isNewAddress,addr_in,
		/*restart=*/ 1,
		tab->xc_ubh_array+memodata->mm_ublock_index,
		addr_out,
		range_out);
	return retstatus;

  } 
  { 
	xlate_block *xbp;
	if(addr_in < memodata->mm_low_old_addr ||
		addr_in >= memodata->mm_high_old_addr) {
  		return XLATE_TB_STATUS_NO_SUCH_ADDR_IN_TABLE;
	}
	xbp = &memodata->mm_block;
	if(addr_in >= xbp->xe_old_address &&
		addr_in < (xbp->xe_old_address + xbp->xe_old_range)) {
		*addr_out = _xlate_get_out_from_ranges(addr_in,
			xbp->xe_old_address,
			xbp->xe_new_address,
			xbp->xe_new_range);
		if(range_out) {
		  *range_out = *xbp;
		}
	        return XLATE_TB_STATUS_NO_ERROR;
	}
	/* Maybe later in this block: continue
	   where we left off in block
 	*/
        retstatus = tab->xc_get_range_from_block(tab,
		isNewAddress,addr_in,
		/*restart=*/ 1,
		tab->xc_ubh_array+memodata->mm_ublock_index,
		addr_out,
		range_out);
	return retstatus;
  }
}
#endif

/*
The search functions search for an applicable block.
We don't want to look into any block that we don't have to.

*/
int _xlate_binary_search_for_addr(xlate_table_con tab,
        int isNewAddress,
        Elf64_Addr addr_in,
        Elf64_Addr* addr_out,
        xlate_block *range_out)
{
  Uword last_block_index = tab->xc_hdr.ich_num_blocks - 1;
  Uword high = last_block_index;
  Uword low = 0;
  Uword probe = 0;
  Uword final;
  Elf64_Addr low_lowaddr = 0; 
  Elf64_Addr lowaddr; 
  Elf64_Addr high_lowaddr = 0; 
  Elf64_Addr highaddr;

  int retstatus = XLATE_TB_STATUS_NO_ERROR;
  struct memo_s *memodata;

#ifndef TURN_OFF_MEMO
  if(isNewAddress) {
	memodata = &tab->xc_memo_new;
  } else {
	memodata = &tab->xc_memo_old;
  }
  if(memodata->mm_is_valid) {
	retstatus = look_in_memo_data(tab,isNewAddress,
		memodata,
		addr_in, addr_out,range_out);
	if(retstatus == XLATE_TB_STATUS_NO_ERROR) {

		return retstatus;
	}
  }
  memodata->mm_is_valid = 0;
#endif
  
  retstatus = tab->xc_block_header_indexer(tab,
		isNewAddress, low,&low_lowaddr,
		&highaddr,tab->xc_ubh_array+low);
  if(retstatus != XLATE_TB_STATUS_NO_ERROR) {
		/* impossible */
		return retstatus;
  }
  if(high == low) {
	high_lowaddr = low_lowaddr;
  } else {
    retstatus = tab->xc_block_header_indexer(tab,
		isNewAddress, high,&high_lowaddr,
		&highaddr,tab->xc_ubh_array+high);
    if(retstatus != XLATE_TB_STATUS_NO_ERROR) {
		/* impossible */
		return retstatus;
    }
  }

  while((high-low)>1) { 
	probe = (low+high)/2;
	retstatus = tab->xc_block_header_indexer(tab,
		isNewAddress, probe,&lowaddr,
		&highaddr,tab->xc_ubh_array+probe);
	if(retstatus != XLATE_TB_STATUS_NO_ERROR) {
		/* impossible */
		return retstatus;
	}
	if(lowaddr <= addr_in) {
		low = probe;
	 	low_lowaddr = lowaddr;
	} else {
		high = probe;
	 	high_lowaddr = lowaddr;
	}
  };

  if(addr_in < high_lowaddr) {
	final = low;
  }  else {
	final = high;
  }

  retstatus = tab->xc_block_header_indexer(tab,
		isNewAddress,
                final,&lowaddr,
		&highaddr,tab->xc_ubh_array+final);
  if(retstatus != XLATE_TB_STATUS_NO_ERROR) {
                /* impossible */
                return retstatus;
  }

  if(addr_in >= highaddr || addr_in < lowaddr) {
	return XLATE_TB_STATUS_NO_SUCH_ADDR_IN_TABLE;
  }
  /* now find the range, since we have a block */
  retstatus = tab->xc_get_range_from_block(tab,
		isNewAddress,addr_in,
		/*restart=*/ 0,
		tab->xc_ubh_array+final,
		addr_out,
		range_out);

  
  return retstatus;
}

/*
For preserve size and general tables when searching for
an old-to-new translation, one cannot do binary search
since there is no ordering of the table.

So what we do instead is a linear search of the block headers.
For *each* block header with the address in range we search
the block to see if the address is *really* there.

We accept the first hit as the real and only translation.

It would be nice to memo-ize the result of a successful search for a block.

*/
int _xlate_special_search_for_addr(xlate_table_con tab,
        int isNewAddress, /* will always be 0 */
        Elf64_Addr addr_in,
        Elf64_Addr* addr_out,
        xlate_block *range_out)
{
  Uword last_block_index = tab->xc_hdr.ich_num_blocks - 1;
  Uword curblock;
  Elf64_Addr lowaddr;
  Elf64_Addr highaddr;
  int retstatus = XLATE_TB_STATUS_NO_ERROR;
  struct memo_s *memodata;
  

#ifndef TURN_OFF_MEMO
  if(isNewAddress) {
        memodata = &tab->xc_memo_new;
  } else {
        memodata = &tab->xc_memo_old;
  }
  if(memodata->mm_is_valid) {
	retstatus = look_in_memo_data(tab,isNewAddress,
		memodata,
		addr_in, addr_out,range_out);
	if(retstatus == XLATE_TB_STATUS_NO_ERROR) {
		return retstatus;
	}
  }

  memodata->mm_is_valid = 0;
#endif
  for(curblock = 0; curblock <= last_block_index; ++curblock) {
	retstatus = tab->xc_block_header_indexer(tab,
                isNewAddress, curblock,&lowaddr,
                &highaddr,tab->xc_ubh_array+curblock);
 	if(retstatus != XLATE_TB_STATUS_NO_ERROR) {
                /* impossible */
                return retstatus;
        }
	if(addr_in < lowaddr || addr_in >= highaddr) {
		continue;
	}

        /* now find the range, since we have a block */
        retstatus = tab->xc_get_range_from_block(tab,
		isNewAddress,addr_in,
		/*restart=*/0,
		tab->xc_ubh_array+curblock,
		addr_out,
		range_out);
	if(retstatus == XLATE_TB_STATUS_NO_SUCH_ADDR_IN_TABLE) {
		/* not here: look in another block */
		continue;
	}
        /* now we have passed or failed but at any rate
	** we now take whatever we have as the answer.
	*/
	return retstatus;

  }
  return XLATE_TB_STATUS_NO_SUCH_ADDR_IN_TABLE;

}


