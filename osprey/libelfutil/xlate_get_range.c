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

  xlate_get_range.c

  $Revision: 1.1.1.1 $

  These are the 9 possible versions implementing all combinations of
  table format and tablekind.
  Called thru function pointers. 

	table format  
          v1
	  v2 32 bit
	  v2 64 bit
	tablekind
	  po
	  ps       
	  general

ps: values are:
	unsigned delta previous new address
	signed delta previous old address
po: values are:
	unsigned delta previous new address
	unsigned delta previous old address

The minor difference between ps and po seems forced and unnecessary.
Making both be a signed delta in v2 would have been possible.

ge v1: values are (this table never exists)
	unsigned new length (new range)
        unsigned delta old address
        unsigned (new_length - old_length)
ge v2: values are
	unsigned length (new range)
        unsigned delta old address
        signed (new_length - old_length)

Note that the v1 ps and v1 po tables have an initial leading
'entry' per block which is read and skipped: it is garbage.

  These functions are referenced to initialize function pointers
  so they all all get linked in from the archive.

*/

#include "xlateincl.h"

static struct xlate_block_s  zero_xlate_block;

static void __inline set_memodata_valid(
	struct memo_s *memodata,
	xlate_block *xbp,
	Uword entryct,
	Uword entrymax,
	int isNewAddress,
	int restart,
	uniform_block_hdr * blk_hdr)
{
        memodata->mm_is_valid       =1;
        memodata->mm_block          =*xbp;
        memodata->mm_entryct        =entryct;
        memodata->mm_entrymax       =entrymax;
        memodata->mm_is_new_address =isNewAddress;
        memodata->mm_ublock_index   =blk_hdr->ub_block_index;
        if(!restart) {
          memodata->mm_low_new_addr  = blk_hdr->ub_low_new_addr;
          memodata->mm_high_new_addr = blk_hdr->ub_high_new_addr;
          memodata->mm_low_old_addr  = blk_hdr->ub_low_old_addr;
          memodata->mm_high_old_addr = blk_hdr->ub_high_old_addr;
        }
}


/*
	Return -1,0, or 1 depending on if addr_in is
	lessthan, inside, greater-orequal the range of addresses
	(only 'inside' is a match.)
*/
static int __inline _xlate_found_address(Elf64_Addr addr_in,
	int isNewAddress, xlate_block *blkp)
{
	if(isNewAddress) {
		if(addr_in < blkp->xe_new_address) {
		    return -1;
		}
		if(addr_in >= (blkp->xe_new_address+
			blkp->xe_new_range)) {
		    return 1;
		}
		return 0;
	}

	/* old address */
	if(addr_in < blkp->xe_old_address) {
	    return -1;
	}
	if(addr_in >= (blkp->xe_old_address+
		blkp->xe_old_range)) {
	    return 1;
	}
	return 0;
}

/*
	Return 1 if the address is in the block (we return
	the interpolated address thru addr_out in that case).

        Return 0 if the address not in the block. addr_out ignored.
*/
static int __inline
_xlate_ck_in_range_and_calc_out_addr(
		Elf64_Addr addr_in,
		int isNewAddress, 
		xlate_block *xbp, 
		Elf64_Addr * addr_out)
{
	if(_xlate_found_address(addr_in,isNewAddress,xbp) != 0)  {
		return 0;
        }

	/* ok, we are in the range. */
        if(isNewAddress) {
                 /* fall to end, we found entry */
                 *addr_out = _xlate_get_out_from_ranges(addr_in,
                        xbp->xe_new_address,
                        xbp->xe_old_address,xbp->xe_old_range);
        } else {
                 *addr_out = _xlate_get_out_from_ranges(addr_in,
                        xbp->xe_old_address,
                        xbp->xe_new_address,xbp->xe_new_range);
        }
        return 1;
}

/*
	Do binary search on a block table.
	Return 1 if successful, 0 if not.
	Return block index thru *outindex iff successful.
*/
static int
_xlate_binary_search_xlate_block_table(
	xlate_block * xbp, /* *base* of block table, not a
				pointer to entry 'entryct' */
	Uword entryct,
	Uword entrymax, /* 1 past end. (# of entries, really).*/
	int isNewAddress,
	Elf64_Addr addr_in,
	Uword *outindex)
{
	Uword low = entryct;
	Uword high = entrymax-1;
	Uword probe;
	int v;

	if(low > high) {
		return 0; /* in case insane input */
	}
	while(high-low> 1) {

	   probe = (high+low)/2;
	   v =_xlate_found_address(addr_in,isNewAddress,xbp+probe);
	   if(v == 0) {
		*outindex = probe;
		return 1;
	   }
	   if(v < 0) {
		/* look lower */
		high = probe;
           } else {
		/* look higher */
		low = probe;
	   }
	}
	/* either high, low, or neither is the one 
	   high and low might be identical block numbers.
	*/

	v =_xlate_found_address(addr_in,isNewAddress,xbp+low);
	if(0 == v) {
	   *outindex = low;
	   return 1;
	}  else if ( v < 0) {
	   /* below low block: not present */
	   return 0;
	}
	if(0 ==_xlate_found_address(addr_in,isNewAddress,xbp+high)) {
	   *outindex = high;
	   return 1;
	}
	return 0;
}



/* Given an addr in the range base1, base1+addr,
   calculate what addr it is in base2 and return that.

   Always take the smaller range as the max in the mapping.
*/
Elf64_Addr
_xlate_get_out_from_ranges(Elf64_Addr addr_in,
	Elf64_Addr  base_addr1,
	Elf64_Addr  base_addr2,
	Elf64_Xword range2)
{
	Elf64_Xword addrincr = addr_in - base_addr1;
	Elf64_Addr final_addr;

	if(addrincr >= range2) {
		/* point to last valid addr, not past end of the range */
		addrincr = range2 - INSTRUCTION_SIZE;
	}
	final_addr =  base_addr2 + addrincr;
	return final_addr;
}

/*
ps: values are:
        unsigned range 
        signed delta previous old address
	the v1 algorithm is different 
	than v2 (in spite of the deltas
	being the same) because the v1 outout was 
	wrong.  
	
	Of course v1 got the 0th entry in each block wrong.

	The side effect of the bad 0th entry was confusion:
	There was no legitimate 0th entry so one starts with
	the 1st entry and must treat the  signed old address
	delta as the delta to the *next* old address.
	Of course the input did not work this way: one output
	each pair of deltas by comparing with the last, not
	by looking ahead to the next!

	This is hard to think about.  Suffice it to say that
	the v2 reader is slightly different and the signed
	old-address delta (and range)  apply to the 
	very entry itself.


*/
/*ARGSUSED */
static int
_xlate_expand_block_v1_ps(xlate_table_con tab,
        uniform_block_hdr *blk_hdr)
{
  int retstatus = XLATE_TB_STATUS_NO_ERROR;
  char *startpt;
  char *endpt;
  Uword entrymax;
  Uword entryct = 0;
  __uint32_t uval = 0;
  __int32_t sval = 0;
  int len;
  xlate_block lblock = zero_xlate_block;
  xlate_block *xbp = 0;

  startpt  = blk_hdr->ub_data_bytes;
  endpt    = blk_hdr->ub_data_end;
  entrymax = blk_hdr->ub_entrycount-1;
    /* discard the first entry set: they are garbage */
  len      = _leb128_unsigned_decode32(startpt,&uval);
  startpt += len;
  len      = _leb128_signed_decode32(startpt,&sval);
  startpt += len;

  lblock.xe_new_address = blk_hdr->ub_first_new_addr;
  lblock.xe_old_address = blk_hdr->ub_first_old_addr;
  uval = 0;
  sval = 0;

  /* The question is, how to calcuate the correct old
  ** address for v1 stuff
  */
  /* ASSERT: blk_hdr->ub_expanded_entries == 0 */
  blk_hdr->ub_expanded_entries = calloc(entrymax,sizeof(xlate_block));
  if(blk_hdr->ub_expanded_entries == 0) {
        return XLATE_TB_STATUS_ALLOC_FAIL;
  }
  xbp = blk_hdr->ub_expanded_entries;


  for( ; entryct < entrymax && startpt < endpt; ) {

        lblock.xe_new_address += uval;
        lblock.xe_old_address += sval;

        len = _leb128_unsigned_decode32(startpt,&uval);
        startpt += len;
        len = _leb128_signed_decode32(startpt,&sval);
        startpt += len;
        ++entryct;
        uval <<=2;
        sval <<=2;

        lblock.xe_new_range = uval;
        lblock.xe_old_range = uval;
        /* A range entry is completely filled out now */
	*xbp = lblock;

   }

   /* if we are unlucky, the only entry will be the bogus 0th one,
	so entryct could be zero. 
   */

   blk_hdr->ub_v1_entrycount = entryct; /* should == entrymax */
   
   /* NOW what about the last entry: not filled in */
   /*I guess we don't need it */
   return retstatus;
}
int
_xlate_get_range_from_block_v1_ps(xlate_table_con tab,
        int isNewAddress,
        Elf64_Addr addr_in,
	int restart,
        uniform_block_hdr *blk_hdr,
        Elf64_Addr* addr_out,
        xlate_block *range_out)
{
  int retstatus = XLATE_TB_STATUS_NO_ERROR;
  Uword entrymax;
  Uword entryct = 0;
  Elf64_Addr our_out_addr;
  struct memo_s *memodata;
  xlate_block *xbp = 0;

  if(blk_hdr->ub_expanded_entries == 0) {
        retstatus = _xlate_expand_block_v1_ps(tab,
                        blk_hdr);
        if(retstatus != XLATE_TB_STATUS_NO_ERROR) {
                return retstatus;
        }
  }
#ifndef TURN_OFF_MEMO
  if(isNewAddress) {
	memodata = &tab->xc_memo_new;
  } else {
	memodata = &tab->xc_memo_old;
  }
  if(restart) {
    entryct  = memodata->mm_entryct;
    entrymax = 	memodata->mm_entrymax;
  } else {
#endif
    entrymax = blk_hdr->ub_v1_entrycount;
#ifndef TURN_OFF_MEMO
  }
  memodata->mm_is_valid = 0;
#endif


  xbp = blk_hdr->ub_expanded_entries;

  /* Only call binary search if entrymax is sensible (> 0), which
     it might not be for version 1 objects (in a very very rare
     case we hope never happens).
  */

  if(isNewAddress && (entryct < entrymax)) {
        Uword newindex;
        int res;
        res = _xlate_binary_search_xlate_block_table(xbp,entryct,entrymax,
                isNewAddress,addr_in,&newindex);
        if(!res) {
           return XLATE_TB_STATUS_NO_SUCH_ADDR_IN_TABLE;
        }
        /* found the block, start sequential search
                   at exactly the right block */
        entryct = newindex;
  }

  xbp += entryct;

  /* The question is, how to calcuate the correct old
  ** address for v1 stuff
  */

  for( ; entryct < entrymax ; ++entryct, ++xbp ) {
        int res = _xlate_ck_in_range_and_calc_out_addr(addr_in,
		isNewAddress,
                xbp,&our_out_addr);
        if(!res) {
           continue;
        }


	/* yes, we found a range */
	*addr_out = our_out_addr;
	if(range_out) {
		*range_out = *xbp;
	}
	/* done! */
#ifndef TURN_OFF_MEMO

	set_memodata_valid(memodata,xbp,entryct,entrymax,isNewAddress,
		restart,blk_hdr);
#endif
	return retstatus;

  }

  return XLATE_TB_STATUS_NO_SUCH_ADDR_IN_TABLE;

}
/*
po: values are:
        unsigned new range
        unsigned old range

Due to a bug in the v1 producer, the final deltas are put
in the *next*block so we have to fake the final range here.

*/
/* ARGSUSED */
static int
_xlate_expand_block_v1_po(xlate_table_con tab,
        uniform_block_hdr *blk_hdr)
{
  int retstatus = XLATE_TB_STATUS_NO_ERROR;
  char *startpt;
  char *endpt;
  Uword entrymax;
  Uword entryct = 0;
  __uint32_t uval = 0;
  __uint32_t uval2 = 0;
  int len;
  xlate_block lblock = zero_xlate_block;
  Elf64_Addr low_old_addr = INITIAL_LOW_ADDR;
  Elf64_Addr high_old_addr = 0;
  xlate_block *xbp;

  startpt  = blk_hdr->ub_data_bytes;
  endpt    = blk_hdr->ub_data_end;
  entrymax = blk_hdr->ub_entrycount-1;
  /* discard the first entry set: they are garbage */
  len      = _leb128_unsigned_decode32(startpt,&uval);
  startpt += len;
  len      = _leb128_unsigned_decode32(startpt,&uval);
  startpt += len;
  lblock.xe_new_address = blk_hdr->ub_first_new_addr;
  lblock.xe_old_address = blk_hdr->ub_first_old_addr;
  uval = 0;
  uval2 = 0;
  /* ASSERT: blk_hdr->ub_expanded_entries == 0 */
  blk_hdr->ub_expanded_entries = calloc(entrymax,sizeof(xlate_block));
  if(blk_hdr->ub_expanded_entries == 0) {
        return XLATE_TB_STATUS_ALLOC_FAIL;
  }
  xbp = blk_hdr->ub_expanded_entries;

  /* Because we skipped one input entry, that leaves 
     The final output entry not touched.. Yuck.
  */
  for( ; entryct < entrymax && startpt < endpt; ++xbp) {

        lblock.xe_new_address += uval;
        lblock.xe_old_address += uval2;

        len = _leb128_unsigned_decode32(startpt,&uval);
        startpt += len;
        len = _leb128_unsigned_decode32(startpt,&uval2);
        startpt += len;
        ++entryct;
        uval <<=2;
        uval2 <<=2;

        lblock.xe_new_range = uval;
        lblock.xe_old_range = uval2;
        if(low_old_addr > lblock.xe_old_address) {
                 low_old_addr = lblock.xe_old_address;
        }
        if(high_old_addr <
                  (lblock.xe_old_address + lblock.xe_old_range)) {
                 high_old_addr = lblock.xe_old_address +
                                lblock.xe_old_range;
        }
        /* A range entry is completely filled out now */

        *xbp = lblock;
  }
   
  /*If we are unlucky the only entry would be the bogus
    initial one, so we have no legitimate entries at all.
    So entryct == 0
  */
  blk_hdr->ub_v1_entrycount = entryct; /* should == entrymax */

  if(high_old_addr > blk_hdr->ub_high_old_addr) {
	blk_hdr->ub_high_old_addr = high_old_addr;
  }
  if(low_old_addr < blk_hdr->ub_low_old_addr) {
	blk_hdr->ub_low_old_addr = low_old_addr;
  }

  return retstatus;
}
int
_xlate_get_range_from_block_v1_po(xlate_table_con tab,
        int isNewAddress,
        Elf64_Addr addr_in,
	int restart,
        uniform_block_hdr *blk_hdr,
        Elf64_Addr* addr_out,
        xlate_block *range_out)
{
  int retstatus = XLATE_TB_STATUS_NO_ERROR;
  Uword entrymax;
  Uword entryct = 0;
  xlate_block lblock = zero_xlate_block;
  Elf64_Addr our_out_addr;
  struct memo_s *memodata;
  xlate_block *xbp;
  int res;
  Uword newindex;



  if(blk_hdr->ub_expanded_entries == 0) {
        retstatus = _xlate_expand_block_v1_po(tab,
                        blk_hdr);
        if(retstatus != XLATE_TB_STATUS_NO_ERROR) {
                return retstatus;
        }

  }

#ifndef TURN_OFF_MEMO
  if(isNewAddress) {
	memodata = &tab->xc_memo_new;
  } else {
	memodata = &tab->xc_memo_old;
  }
  if(restart) {
    entryct  = memodata->mm_entryct;
    entrymax = memodata->mm_entrymax;
  } else {
#endif
    entrymax = blk_hdr->ub_v1_entrycount;
    ++entryct;
#ifndef TURN_OFF_MEMO
  }
  memodata->mm_is_valid = 0;
#endif

  xbp = blk_hdr->ub_expanded_entries;
  /* Only call binary search if entrymax is sensible (> 0), which
     it might not be for version 1 objects (in a very very rare
     case we hope never happens).
  */
  if(entryct < entrymax) {
    res = _xlate_binary_search_xlate_block_table(xbp,
                entryct,entrymax,isNewAddress,addr_in,&newindex);
    if(!res) {
        return XLATE_TB_STATUS_NO_SUCH_ADDR_IN_TABLE;
    }
    entryct = newindex;
  }

  /* The loop will find the first entry for sure
        (ie, will only iterate once).
  */


  xbp += entryct;
  for( ; entryct < entrymax ; ++xbp,++entryct) {
        int res = _xlate_ck_in_range_and_calc_out_addr(addr_in,
		isNewAddress,
                xbp,&our_out_addr);
        if(!res) {
           continue;
        }


	/* yes, we found a range */
	*addr_out = our_out_addr;
	if(range_out) {
		*range_out = *xbp;
	}
	/* done! */
#ifndef TURN_OFF_MEMO
	set_memodata_valid(memodata,xbp,entryct,entrymax,isNewAddress,
		restart,blk_hdr);
#endif
	return retstatus;

  }
  /* A v1 po block end is bogus: the final block entry gets put in the next
  **   block.
        Synthesize one from what we have available.
	The blk_hdr has what we need.
  */
  if(!restart) {
    /* Only even try this if not a resart.
    ** If a restart let this fail and we try the long  slow way.
    */
    if(isNewAddress) {
	if(addr_in  < blk_hdr->ub_high_new_addr) {
		/*synthesize missing entry */
		lblock.xe_new_range = 
			 blk_hdr->ub_high_new_addr -lblock.xe_new_address;
		lblock.xe_old_range = 
			blk_hdr->ub_high_old_addr -lblock.xe_old_address;
		if(range_out) {
		 *range_out = lblock;
		}
		return retstatus;
	} else {
	}
    } else {
	if(addr_in < blk_hdr->ub_high_old_addr) {
		/*synthesize missing entry */
		lblock.xe_new_range = 
			 blk_hdr->ub_high_new_addr -lblock.xe_new_address;
		lblock.xe_old_range = 
			blk_hdr->ub_high_old_addr -lblock.xe_old_address;
		if(range_out) {
		 *range_out = lblock;
		}
		return retstatus;
	} else {
	}
    }
  

  } /* !restart */
  return XLATE_TB_STATUS_NO_SUCH_ADDR_IN_TABLE;

}

/*
 ge v1: values are
           unsigned delta new length (new range)
           unsigned delta old address
           unsigned delta of (new_length - old_length)

 There is no way this table form can be produced
 (no tool creates it).
 No such table exists in any file or will ever be created.

*/
/* ARGSUSED */
int
_xlate_get_range_from_block_v1_ge(xlate_table_con tab,
        int isNewAddress,
        Elf64_Addr addr_in,
	int restart,
        uniform_block_hdr *blk_hdr,
        Elf64_Addr* addr_out,
        xlate_block *range_out)
{
  int retstatus = XLATE_TB_STATUS_NOT_YET_IMPLEMENT;

  return retstatus;
}



/*
ps: values are:
        unsigned range 
        signed delta previous old address


   We translate (decode) a block completely.
*/
/* ARGSUSED */
static int
_xlate_expand_block_v2_32_ps(xlate_table_con tab,
        uniform_block_hdr *blk_hdr)
{
  int retstatus = XLATE_TB_STATUS_NO_ERROR;
  char *startpt;
  char *endpt ;
  Uword entrymax;
  Uword entryct = 0;
  __uint32_t uval = 0;
  __int32_t sval = 0;
  int len;
  xlate_block *xbp = 0;
  xlate_block lblock = zero_xlate_block;


  startpt  = blk_hdr->ub_data_bytes;
  endpt    = blk_hdr->ub_data_end;
  entrymax = blk_hdr->ub_entrycount;
  /* ASSERT: blk_hdr->ub_expanded_entries == 0 */
  blk_hdr->ub_expanded_entries = calloc(entrymax,sizeof(xlate_block));
  if(blk_hdr->ub_expanded_entries == 0) {
	return XLATE_TB_STATUS_ALLOC_FAIL;
  }
  xbp = blk_hdr->ub_expanded_entries;
  lblock.xe_new_address = blk_hdr->ub_first_new_addr;
  lblock.xe_old_address = blk_hdr->ub_first_old_addr;


  for( ; entryct < entrymax  && startpt < endpt; ++xbp) {

        lblock.xe_new_address += uval;

        len = _leb128_unsigned_decode32(startpt,&uval);
        startpt += len;
        len = _leb128_signed_decode32(startpt,&sval);
        startpt += len;
        ++entryct;
        uval <<=2;
        sval <<=2;

        lblock.xe_new_range = uval;
        lblock.xe_old_range = uval;
        lblock.xe_old_address += sval;

	/* range entry complete now */

	*xbp = lblock;
  }
  return retstatus;
}

int
_xlate_get_range_from_block_v2_32_ps(xlate_table_con tab,
        int isNewAddress,
        Elf64_Addr addr_in,
	int restart,
        uniform_block_hdr *blk_hdr,
        Elf64_Addr* addr_out,
        xlate_block *range_out)
{
  int retstatus = XLATE_TB_STATUS_NO_ERROR;
  Uword entrymax;
  Uword entryct = 0;
  Elf64_Addr our_out_addr;
  struct memo_s *memodata;
  xlate_block *xbp;


  if(blk_hdr->ub_expanded_entries == 0) {
	retstatus = _xlate_expand_block_v2_32_ps(tab,
			blk_hdr);
	if(retstatus != XLATE_TB_STATUS_NO_ERROR) {
		return retstatus;
	}

  }

#ifndef TURN_OFF_MEMO
  if(isNewAddress) {
	memodata = &tab->xc_memo_new;
  } else {
	memodata = &tab->xc_memo_old;
  }
  if(restart) {
    entryct  = memodata->mm_entryct;
    entrymax = memodata->mm_entrymax;
  } else {
#endif
    entrymax = blk_hdr->ub_entrycount;
#ifndef TURN_OFF_MEMO
  }
  memodata->mm_is_valid = 0;
#endif

  xbp = blk_hdr->ub_expanded_entries;
  if(isNewAddress) {
	Uword newindex;
	int res;
	res = _xlate_binary_search_xlate_block_table(xbp,entryct,entrymax,
		isNewAddress,addr_in,&newindex);
	if(!res) {
	   return XLATE_TB_STATUS_NO_SUCH_ADDR_IN_TABLE;
	}
	/* found the block, start sequential search
		   at exactly the right block */
	entryct = newindex;
  }
  xbp += entryct;

  for( ; entryct < entrymax  ; ++xbp,++entryct) {
        int res = _xlate_ck_in_range_and_calc_out_addr(addr_in,
		isNewAddress,
                xbp,&our_out_addr);
        if(!res) {
           continue;
        }

	/* yes, we found a range */
	*addr_out = our_out_addr;
	if(range_out) {
		*range_out = *xbp;
	}
	/* done! */
#ifndef TURN_OFF_MEMO
	set_memodata_valid(memodata,xbp,entryct,entrymax,isNewAddress,
		restart,blk_hdr);
#endif
	return retstatus;

  }
  return XLATE_TB_STATUS_NO_SUCH_ADDR_IN_TABLE;

}
/*

po: values are:
        unsigned new range
        unsigned old range

*/
/* ARGSUSED */
static int
_xlate_expand_block_v2_32_po(xlate_table_con tab,
        uniform_block_hdr *blk_hdr)
{
  int retstatus = XLATE_TB_STATUS_NO_ERROR;
  char *startpt;
  char *endpt;
  Uword entrymax;
  Uword entryct = 0;
  __uint32_t uval = 0;
  __uint32_t uval2 = 0;
  int len;
  xlate_block lblock = zero_xlate_block;
  xlate_block *xbp;

  startpt  = blk_hdr->ub_data_bytes;
  endpt    = blk_hdr->ub_data_end;
  entrymax = blk_hdr->ub_entrycount;
  /* ASSERT: blk_hdr->ub_expanded_entries == 0 */
  blk_hdr->ub_expanded_entries = calloc(entrymax,sizeof(xlate_block));
  if(blk_hdr->ub_expanded_entries == 0) {
        return XLATE_TB_STATUS_ALLOC_FAIL;
  }

  xbp = blk_hdr->ub_expanded_entries;

  lblock.xe_new_address = blk_hdr->ub_first_new_addr;
  lblock.xe_old_address = blk_hdr->ub_first_old_addr;
  for( ; entryct < entrymax && startpt < endpt; ++xbp) {

        lblock.xe_new_address += uval;
        lblock.xe_old_address += uval2;

        len = _leb128_unsigned_decode32(startpt,&uval);
        startpt += len;
        len = _leb128_unsigned_decode32(startpt,&uval2);
        startpt += len;
        ++entryct;
        uval <<=2;
        uval2 <<=2;

        lblock.xe_new_range = uval;
        lblock.xe_old_range = uval2;
        /* A range entry is completely filled out now */

        *xbp = lblock;
  }
  return retstatus;
}
int
_xlate_get_range_from_block_v2_32_po(xlate_table_con tab,
        int isNewAddress,
        Elf64_Addr addr_in,
	int restart,
        uniform_block_hdr *blk_hdr,
        Elf64_Addr* addr_out,
        xlate_block *range_out)
{
  int retstatus = XLATE_TB_STATUS_NO_ERROR;
  Uword entrymax;
  Uword entryct = 0;
  Elf64_Addr our_out_addr;
  struct memo_s *memodata;
  Uword newindex;
  int res;
  xlate_block *xbp;

  if( blk_hdr->ub_expanded_entries == 0) {
         retstatus = _xlate_expand_block_v2_32_po( tab,
                blk_hdr);
         if(retstatus != XLATE_TB_STATUS_NO_ERROR) {
                return retstatus;
         }
  }

#ifndef TURN_OFF_MEMO
  if(isNewAddress) {
	memodata = &tab->xc_memo_new;
  } else {
	memodata = &tab->xc_memo_old;
  }
  if(restart) {
    entryct  = memodata->mm_entryct;
    entrymax = memodata->mm_entrymax;
  } else {
#endif
    entrymax = blk_hdr->ub_entrycount;
#ifndef TURN_OFF_MEMO
  }
  memodata->mm_is_valid = 0;
#endif
  xbp = blk_hdr->ub_expanded_entries;
  /* This is preserve order, so both old and new addresses
     are in increasing order.
     So can binary search either.
  */
  res = _xlate_binary_search_xlate_block_table(xbp,
                entryct,entrymax,isNewAddress,addr_in,&newindex);
  if(!res) {
        return XLATE_TB_STATUS_NO_SUCH_ADDR_IN_TABLE;
  }

  /* The loop will find the first entry for sure 
	(ie, will only iterate once).
  */

  xbp += entryct;
  for( ; entryct < entrymax ; ++xbp, ++entryct) {
        int res = _xlate_ck_in_range_and_calc_out_addr(addr_in,
		isNewAddress,
                xbp,&our_out_addr);
        if(!res) {
           continue;
        }


	/* yes, we found a range */
	*addr_out = our_out_addr;
	if(range_out) {
		*range_out = *xbp;
	}
	/* done! */
#ifndef TURN_OFF_MEMO
	set_memodata_valid(memodata,xbp,entryct,entrymax,isNewAddress,
		restart,blk_hdr);
#endif
	return retstatus;

  }
	
  return XLATE_TB_STATUS_NO_SUCH_ADDR_IN_TABLE;

}
/*
ge v2: values are
        unsigned length (new range)
        unsigned delta old address
        signed (new_length - old_length)

*/
/* ARGSUSED */
static int
_xlate_expand_block_v2_32_ge(xlate_table_con tab,
        uniform_block_hdr *blk_hdr)
{

  int retstatus = XLATE_TB_STATUS_NO_ERROR;
  char *startpt;
  char *endpt ;
  Uword entrymax;
  Uword entryct = 0;
  __uint32_t uval = 0;
  __int32_t sval = 0;
  __int32_t sval2 = 0;
  int len;
  xlate_block lblock = zero_xlate_block;
  xlate_block *xbp;
  startpt  = blk_hdr->ub_data_bytes;
  endpt    = blk_hdr->ub_data_end;
  entrymax = blk_hdr->ub_entrycount;
  /* ASSERT: blk_hdr->ub_expanded_entries == 0 */
  blk_hdr->ub_expanded_entries = calloc(entrymax,sizeof(xlate_block));
  if(blk_hdr->ub_expanded_entries == 0) {
        return XLATE_TB_STATUS_ALLOC_FAIL;
  }

  xbp = blk_hdr->ub_expanded_entries;

  lblock.xe_new_address = blk_hdr->ub_first_new_addr;
  lblock.xe_old_address = blk_hdr->ub_first_old_addr;

 for( ; entryct < entrymax && startpt < endpt; ++xbp) {

        lblock.xe_new_address += uval;

        /* new length */
        len = _leb128_unsigned_decode32(startpt,&uval);
        startpt += len;
        /* old address delta  */
        len = _leb128_signed_decode32(startpt,&sval2);
        startpt += len;
        /* new_length - old_length */
        len = _leb128_signed_decode32(startpt,&sval);
        startpt += len;
        ++entryct;
        uval  <<=2;
        sval2 <<=2;
        sval  <<=2;

        lblock.xe_old_address += sval2;
        lblock.xe_new_range   = uval;
                /* new len - (new_len - old_len) ==> oldlen */
        lblock.xe_old_range   = uval -sval;
        /* A range entry is completely filled out now */


        *xbp = lblock;

  }
  return retstatus;
}
int
_xlate_get_range_from_block_v2_32_ge(xlate_table_con tab,
        int isNewAddress,
        Elf64_Addr addr_in,
	int restart,
        uniform_block_hdr *blk_hdr,
        Elf64_Addr* addr_out,
        xlate_block *range_out)
{

  int retstatus = XLATE_TB_STATUS_NO_ERROR;
  Uword entrymax;
  Uword entryct = 0;
  Elf64_Addr our_out_addr;
  struct memo_s *memodata;
  xlate_block *xbp;

  if( blk_hdr->ub_expanded_entries == 0) {
         retstatus = _xlate_expand_block_v2_32_ge( tab,
                blk_hdr);
         if(retstatus != XLATE_TB_STATUS_NO_ERROR) {
                return retstatus;
         }
  }

#ifndef TURN_OFF_MEMO
  if(isNewAddress) {
	memodata = &tab->xc_memo_new;
  } else {
	memodata = &tab->xc_memo_old;
  }
  if(restart) {
    entryct  = memodata->mm_entryct;
    entrymax = memodata->mm_entrymax;
  } else {
#endif
    entrymax = blk_hdr->ub_entrycount;
#ifndef TURN_OFF_MEMO
  }
  memodata->mm_is_valid = 0;
#endif

  xbp = blk_hdr->ub_expanded_entries;
  if(isNewAddress) {
	Uword newindex;
	int res;
	res = _xlate_binary_search_xlate_block_table(xbp,
		entryct,entrymax,isNewAddress,addr_in,&newindex);
	if(!res) {
	   return XLATE_TB_STATUS_NO_SUCH_ADDR_IN_TABLE;
	}
	entryct = newindex;
  }
  xbp += entryct;

  for( ; entryct < entrymax ; ++xbp,++entryct) {
        int res = _xlate_ck_in_range_and_calc_out_addr(addr_in,
		isNewAddress,
                xbp,&our_out_addr);
        if(!res) {
           continue;
        }


	/* yes, we found a range */
	*addr_out = our_out_addr;
	if(range_out) {
		*range_out = *xbp;
	}
	/* done! */
#ifndef TURN_OFF_MEMO
	set_memodata_valid(memodata,xbp,entryct,entrymax,isNewAddress,
		restart,blk_hdr);
#endif
	return retstatus;

  }
  return XLATE_TB_STATUS_NO_SUCH_ADDR_IN_TABLE;

}

/*
ps: values are:
        unsigned  range
        signed delta previous old address
*/
/* ARGSUSED */
static int
_xlate_expand_block_v2_64_ps(xlate_table_con tab,
        uniform_block_hdr *blk_hdr)
{
  int retstatus = XLATE_TB_STATUS_NO_ERROR;
  char *startpt;
  char *endpt ;
  Uword entrymax;
  Uword entryct = 0;
  __uint64_t uval = 0;
  __int64_t sval = 0;
  xlate_block lblock = zero_xlate_block;
  xlate_block *xbp;
  int len;

  startpt  = blk_hdr->ub_data_bytes;
  endpt    = blk_hdr->ub_data_end;
  entrymax = blk_hdr->ub_entrycount;
  /* ASSERT: blk_hdr->ub_expanded_entries == 0 */
  blk_hdr->ub_expanded_entries = calloc(entrymax,sizeof(xlate_block));
  if(blk_hdr->ub_expanded_entries == 0) {
        return XLATE_TB_STATUS_ALLOC_FAIL;
  }

  xbp = blk_hdr->ub_expanded_entries;
  lblock.xe_new_address = blk_hdr->ub_first_new_addr;
  lblock.xe_old_address = blk_hdr->ub_first_old_addr;

  for( ; entryct < entrymax && startpt < endpt; ++xbp) {

        lblock.xe_new_address += uval;

        len = _leb128_unsigned_decode64(startpt,&uval);
        startpt += len;
        len = _leb128_signed_decode64(startpt,&sval);
        startpt += len;
        ++entryct;
        uval <<=2;
        sval <<=2;

        lblock.xe_new_range = uval;
        lblock.xe_old_range = uval;
        lblock.xe_old_address += sval;
        /* A range entry is completely filled out now */

        *xbp = lblock;
  }

  return retstatus;

}
int
_xlate_get_range_from_block_v2_64_ps(xlate_table_con tab,
        int isNewAddress,
        Elf64_Addr addr_in,
	int restart,
        uniform_block_hdr *blk_hdr,
        Elf64_Addr* addr_out,
        xlate_block *range_out)
{
  int retstatus = XLATE_TB_STATUS_NO_ERROR;
  Uword entrymax;
  Uword entryct = 0;
  Elf64_Addr our_out_addr;
  struct memo_s *memodata;
  xlate_block *xbp;


  if( blk_hdr->ub_expanded_entries == 0) {
         retstatus = _xlate_expand_block_v2_64_ps( tab,
                blk_hdr);
         if(retstatus != XLATE_TB_STATUS_NO_ERROR) {
                return retstatus;
         }
  }


#ifndef TURN_OFF_MEMO
  if(isNewAddress) {
	memodata = &tab->xc_memo_new;
  } else {
	memodata = &tab->xc_memo_old;
  }
  if(restart) {
    entryct  = memodata->mm_entryct;
    entrymax = memodata->mm_entrymax;
  } else {
#endif
    entrymax = blk_hdr->ub_entrycount;
#ifndef TURN_OFF_MEMO
  }
  memodata->mm_is_valid = 0;
#endif
  xbp = blk_hdr->ub_expanded_entries;
  if(isNewAddress) {
     Uword newindex;
     int res;
     res = _xlate_binary_search_xlate_block_table(xbp, entryct,
                entrymax,isNewAddress,addr_in,&newindex);
     if(!res) {
        return XLATE_TB_STATUS_NO_SUCH_ADDR_IN_TABLE;
     }
     entryct = newindex;
  }
  xbp += entryct;

  for( ; entryct < entrymax ;++xbp,++entryct) {
        int res = _xlate_ck_in_range_and_calc_out_addr(addr_in,
		isNewAddress,
                xbp,&our_out_addr);
        if(!res) {
           continue;
        }

	/* yes, we found a range */
	*addr_out = our_out_addr;
	if(range_out) {
		*range_out = *xbp;
	}
	/* done! */
#ifndef TURN_OFF_MEMO
	set_memodata_valid(memodata,xbp,entryct,entrymax,isNewAddress,
		restart,blk_hdr);
#endif
	return retstatus;

  }
  return XLATE_TB_STATUS_NO_SUCH_ADDR_IN_TABLE;

}
/*

po: values are:
        unsigned new range
        unsigned old range

*/
/* ARGSUSED */
static int
_xlate_expand_block_v2_64_po(xlate_table_con tab,
        uniform_block_hdr *blk_hdr)
{
  int retstatus = XLATE_TB_STATUS_NO_ERROR;
  char *startpt;
  char *endpt ;
  Uword entrymax;
  Uword entryct = 0;
  __uint64_t uval = 0;
  __uint64_t uval2 = 0;
  int len;
  xlate_block lblock = zero_xlate_block;
  xlate_block *xbp;

  startpt  = blk_hdr->ub_data_bytes;
  endpt    = blk_hdr->ub_data_end;
  entrymax = blk_hdr->ub_entrycount;
  /* ASSERT: blk_hdr->ub_expanded_entries == 0 */
  blk_hdr->ub_expanded_entries = calloc(entrymax,sizeof(xlate_block));
  if(blk_hdr->ub_expanded_entries == 0) {
        return XLATE_TB_STATUS_ALLOC_FAIL;
  }

  xbp = blk_hdr->ub_expanded_entries;

  lblock.xe_new_address = blk_hdr->ub_first_new_addr;
  lblock.xe_old_address = blk_hdr->ub_first_old_addr;

  for( ; entryct < entrymax && startpt < endpt; ++xbp) {

        lblock.xe_new_address += uval;
        lblock.xe_old_address += uval2;

        len = _leb128_unsigned_decode64(startpt,&uval);
        startpt += len;
        len = _leb128_unsigned_decode64(startpt,&uval2);
        startpt += len;
        ++entryct;
        uval <<=2;
        uval2 <<=2;

        lblock.xe_new_range = uval;
        lblock.xe_old_range = uval2;
        /* A range entry is completely filled out now */
        *xbp = lblock;

  }
  return retstatus;
}
int
_xlate_get_range_from_block_v2_64_po(xlate_table_con tab,
        int isNewAddress,
        Elf64_Addr addr_in,
	int restart,
        uniform_block_hdr *blk_hdr,
        Elf64_Addr* addr_out,
        xlate_block *range_out)
{
  int retstatus = XLATE_TB_STATUS_NO_ERROR;
  Uword entrymax;
  Uword entryct = 0;
  Elf64_Addr our_out_addr;
  struct memo_s *memodata;
  Uword newindex;
  xlate_block *xbp;
  int res;

  if( blk_hdr->ub_expanded_entries == 0) {
         retstatus = _xlate_expand_block_v2_64_po( tab,
                blk_hdr);
         if(retstatus != XLATE_TB_STATUS_NO_ERROR) {
                return retstatus;
         }
  }
  if(isNewAddress) {
	memodata = &tab->xc_memo_new;
  } else {
	memodata = &tab->xc_memo_old;
  }
#ifndef TURN_OFF_MEMO
  if(restart) {
    entryct  = memodata->mm_entryct;
    entrymax = memodata->mm_entrymax;
  } else {
#endif
    entrymax = blk_hdr->ub_entrycount;
#ifndef TURN_OFF_MEMO
  }
  memodata->mm_is_valid = 0;
#endif
  xbp = blk_hdr->ub_expanded_entries;

  /* This is preserve order, so both old and new addresses
     are in increasing order.
     So can binary search either.
  */
  res = _xlate_binary_search_xlate_block_table(xbp,
		entryct,entrymax,isNewAddress,addr_in,&newindex);
  if(!res) {
	return XLATE_TB_STATUS_NO_SUCH_ADDR_IN_TABLE;
  }
  entryct = newindex;

  /* the loop will find it on the first entry, for sure: loop
	not needed. */

  xbp += entryct;
  for( ; entryct < entrymax ; ++xbp,++entryct) {
	int res = _xlate_ck_in_range_and_calc_out_addr(addr_in,
		isNewAddress,
        	xbp,&our_out_addr);
        if(!res) {
           continue;
        }

	/* yes, we found a range */
	*addr_out = our_out_addr;
	if(range_out) {
		*range_out = *xbp;
	}
	/* done! */
#ifndef TURN_OFF_MEMO
	set_memodata_valid(memodata,xbp,entryct,entrymax,isNewAddress,
		restart,blk_hdr);
#endif
	return retstatus;

  }
	
  return XLATE_TB_STATUS_NO_SUCH_ADDR_IN_TABLE;

}


/* ARGSUSED */
static int
_xlate_expand_block_v2_64_ge(xlate_table_con tab,
        uniform_block_hdr *blk_hdr)
{
  int retstatus = XLATE_TB_STATUS_NO_ERROR;
  char *startpt;
  char *endpt ;
  Uword entrymax;
  Uword entryct = 0;
  __uint64_t uval = 0;
  __int64_t sval = 0;
  __int64_t sval2 = 0;
  int len;
  xlate_block lblock = zero_xlate_block;
  xlate_block *xbp;

  startpt  = blk_hdr->ub_data_bytes;
  endpt    = blk_hdr->ub_data_end;
  entrymax = blk_hdr->ub_entrycount;
  /* ASSERT: blk_hdr->ub_expanded_entries == 0 */
  blk_hdr->ub_expanded_entries = calloc(entrymax,sizeof(xlate_block));
  if(blk_hdr->ub_expanded_entries == 0) {
        return XLATE_TB_STATUS_ALLOC_FAIL;
  }
  xbp = blk_hdr->ub_expanded_entries;
  lblock.xe_new_address = blk_hdr->ub_first_new_addr;
  lblock.xe_old_address = blk_hdr->ub_first_old_addr;
  for( ; entryct < entrymax && startpt < endpt;++xbp) {

        lblock.xe_new_address += uval;

        /* new length delta */
        len = _leb128_unsigned_decode64(startpt,&uval);
        startpt += len;
        /* old address delta  */
        len = _leb128_signed_decode64(startpt,&sval2);
        startpt += len;
        /* new_length - old_length */
        len = _leb128_signed_decode64(startpt,&sval);
        startpt += len;
        ++entryct;
        uval  <<=2;
        sval2 <<=2;
        sval  <<=2;

        lblock.xe_old_address += sval2;
        lblock.xe_new_range   = uval;
          /* new len - (new_len - old_len) ==> oldlen */
        lblock.xe_old_range   = uval - sval;
        /* A range entry is completely filled out now */

	*xbp = lblock;
   }

   return retstatus;
}


int
_xlate_get_range_from_block_v2_64_ge(xlate_table_con tab,
        int isNewAddress,
        Elf64_Addr addr_in,
	int restart,
        uniform_block_hdr *blk_hdr,
        Elf64_Addr* addr_out,
        xlate_block *range_out)
{

  int retstatus = XLATE_TB_STATUS_NO_ERROR;
  Uword entrymax;
  Uword entryct = 0;
  Elf64_Addr our_out_addr;
  xlate_block *xbp;
  struct memo_s *memodata;

  if( blk_hdr->ub_expanded_entries == 0) {
	 retstatus = _xlate_expand_block_v2_64_ge( tab,
		blk_hdr);
	 if(retstatus != XLATE_TB_STATUS_NO_ERROR) {
		return retstatus;
	 }
  }
#ifndef TURN_OFF_MEMO
  if(isNewAddress) {
	memodata = &tab->xc_memo_new;
  } else {
	memodata = &tab->xc_memo_old;
  }
  if(restart) {
    entrymax = memodata->mm_entrymax;
    entryct  = memodata->mm_entryct;
  } else {
#endif
    entrymax = blk_hdr->ub_entrycount;
#ifndef TURN_OFF_MEMO
  }
  memodata->mm_is_valid = 0;
#endif
  xbp = blk_hdr->ub_expanded_entries;
  if(isNewAddress) {
     Uword newindex;
     int res;
     res = _xlate_binary_search_xlate_block_table(xbp, entryct,
		entrymax,isNewAddress,addr_in,&newindex);
     if(!res) {
	return XLATE_TB_STATUS_NO_SUCH_ADDR_IN_TABLE;
     }
     entryct = newindex;
  }

  xbp += entryct;

  for( ; entryct < entrymax ; ++xbp,++entryct) {

	int res = _xlate_ck_in_range_and_calc_out_addr(addr_in,
		isNewAddress,
		xbp,&our_out_addr);
	if(!res) {
	   continue;
	}

	/* yes, we found a range */
	*addr_out = our_out_addr;
	if(range_out) {
		*range_out = *xbp;
	}
	/* done! */
#ifndef TURN_OFF_MEMO
        set_memodata_valid(memodata,xbp,entryct,entrymax,isNewAddress,
                restart,blk_hdr);
#endif
	return retstatus;

  }
  return XLATE_TB_STATUS_NO_SUCH_ADDR_IN_TABLE;

}
