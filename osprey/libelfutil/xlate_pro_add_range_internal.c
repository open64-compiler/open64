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
   xlate_pro_add_range_internal.c
   $Revision: 1.1.1.1 $

   'range' is the same meaning as 'length' and is length in bytes
   of instructions. (not numbers of instructions)

   These functions do essentially the same thing, but each is
   for a different special case.

   All produce version 2 blocks.

   2 sizes (32 and 64 bit addresses) and 3 types (ps, po, ge)
   mean 6 functions total.

*/

#include "xlateincl.h"

void
_xlate_final_update_highwater_addrs(xlate_table_pro tab,
	Block_s *blk, highwater_mark *highwater)
{
	if(blk == 0) {
		/* in case erroneously has no xlate ranges */
		return;
	}
	if(highwater->hw_lowOldAddr > blk->bk_low_old_addr ) {
	  highwater->hw_lowOldAddr = blk->bk_low_old_addr;
	}
	if(highwater->hw_lowNewAddr > blk->bk_low_new_addr ) {
	  highwater->hw_lowNewAddr = blk->bk_low_new_addr;
	}
	if(highwater->hw_highOldAddr < blk->bk_high_old_addr ) {
	  highwater->hw_highOldAddr = blk->bk_high_old_addr;
	}
	if(highwater->hw_highNewAddr < blk->bk_high_new_addr ) {
	  highwater->hw_highNewAddr = blk->bk_high_new_addr;
	}
}
static void
update_block_highwater(Block_s *blk,
	Elf64_Addr inew_begin,
        Elf64_Addr inew_end,
	Elf64_Addr iold_begin,
	Elf64_Addr iold_end)
{
	if(blk->bk_low_new_addr > inew_begin) {
		blk->bk_low_new_addr = inew_begin;
	}
	if(blk->bk_low_old_addr > iold_begin) {
		blk->bk_low_old_addr = iold_begin;
	}
	if(blk->bk_high_new_addr < inew_end) {
		blk->bk_high_new_addr = inew_end;
	}
	if(blk->bk_high_old_addr < iold_end) {
		blk->bk_high_old_addr = iold_end;
	}
}


static int
do_blk_alloc(xlate_table_pro tab,
        Block_s     **ppblk_head,
        Block_s     **ppblk_tail,
	Elf64_Addr    inew_addr,
	Elf64_Xword   inew_range,
	Elf64_Addr    iold_addr,
	Elf64_Xword   iold_range)
{
	/* malloc our control block and our data space all at once.  */

	Block_s *newb = (Block_s *)malloc(sizeof(Block_s));
	if(newb == 0) {
	  return XLATE_TB_STATUS_ALLOC_FAIL;
	}
	newb->bk_firstNewAddr = inew_addr;
	newb->bk_firstOldAddr = iold_addr;
	newb->bk_low_old_addr  = iold_addr; 
	newb->bk_low_new_addr  = inew_addr;
	newb->bk_high_old_addr =  iold_addr + iold_range; 
	newb->bk_high_new_addr =  inew_addr + inew_range;

	newb->bk_prev_new_addr  = inew_addr;
	newb->bk_prev_new_range = inew_range;
	newb->bk_prev_old_addr  = iold_addr;
	newb->bk_prev_old_range = iold_range;

	newb->bk_numEntries     = 0;
        newb->bk_next_data_to_use = 0;
	newb->bk_next = 0;

	/* leaving the bk_data random : will all be
	** initialized one way or another before return
	** of pointer to bk_data
	** to xlate caller.
	*/

	if(*ppblk_tail) {
	   /* Already have list set up.
		Add new block as tail.
	   */
	   (*ppblk_tail)->bk_next = newb;
	   (*ppblk_tail) = newb;
	} else {
	   (*ppblk_head) = newb;
	   (*ppblk_tail) = newb;
	}
	
	return  XLATE_TB_STATUS_NO_ERROR;
}

int _xlate_pro_add_range_ps32(xlate_table_pro tab,
        Block_s     **ppblk_head,
        Block_s     **ppblk_tail,
        Elf64_Addr    inew_addr,
        Elf64_Xword   inew_range,
        Elf64_Addr    iold_addr,
        Elf64_Xword   iold_range,
        highwater_mark *highwater)
{
	int retstatus = XLATE_TB_STATUS_NO_ERROR;

	Elf32_Addr new_addr = (Elf32_Addr)inew_addr;
	Elf32_Addr old_addr = (Elf32_Addr)iold_addr;
	Elf32_Word new_range = (Elf32_Addr)inew_range;
	Elf32_Word old_range = (Elf32_Addr)iold_range;
	Block_s *pblk_tail;
	char *range_buffer;
	Elf32_Word lnewaddr;
	Elf32_Sword loldaddr;
	int bufused;
	Uword byteoff;

        if(*ppblk_head == 0) {
	  retstatus = do_blk_alloc(tab,ppblk_head,ppblk_tail,
		inew_addr,inew_range,iold_addr,iold_range);
	  if(retstatus != XLATE_TB_STATUS_NO_ERROR) {
		return retstatus;
	  }
	  pblk_tail = *ppblk_tail;
	  byteoff = pblk_tail->bk_next_data_to_use;
	  range_buffer = pblk_tail->bk_data + byteoff;
	  lnewaddr = (Elf32_Word)inew_range;
	  loldaddr = 0;
        } else {
	  pblk_tail = *ppblk_tail;
	  if(inew_addr !=  (pblk_tail->bk_prev_new_addr
			+ pblk_tail->bk_prev_new_range)) {
		return XLATE_TB_STATUS_INVALID_SEQUENCE;
	  }
	  byteoff = pblk_tail->bk_next_data_to_use;
	  range_buffer = pblk_tail->bk_data + byteoff;
	  lnewaddr = (Elf32_Word)inew_range;
	  loldaddr = (Elf32_Sword)
		((Elf32_Addr)old_addr - 
			(Elf32_Addr)(pblk_tail->bk_prev_old_addr));
	}
	if(inew_range != iold_range) {
		return XLATE_TB_STATUS_UNEQUAL_RANGE;
	}


	bufused = _leb128_unsigned_encode32(lnewaddr>>2,range_buffer);
	bufused += _leb128_signed_encode32(loldaddr>>2,range_buffer + bufused);

	if((bufused + byteoff) > TB_BLOCK_SIZE) {
	  Block_s *newtail;
	  char * lclbuf;
	  /* Ran off end of desired area into slop area. 
	  */
	  _xlate_final_update_highwater_addrs(tab,pblk_tail,highwater);

	  /*
		Get new block 
	  */
	  retstatus = do_blk_alloc(tab,ppblk_head,ppblk_tail,
                inew_addr,inew_range,iold_addr,iold_range);
          if(retstatus != XLATE_TB_STATUS_NO_ERROR) {
                return retstatus;
          }
          newtail = *ppblk_tail;
	  lclbuf = newtail->bk_data;
	  lnewaddr = (Elf32_Word)inew_range;
	  loldaddr = 0;
  
	  bufused = _leb128_unsigned_encode32(lnewaddr>>2,lclbuf);
	  bufused += _leb128_signed_encode32(loldaddr>>2,lclbuf + bufused);
	  newtail->bk_next_data_to_use += bufused;
	  newtail->bk_numEntries++;
	  
	} else {
	  /* it fit ! */
	  pblk_tail->bk_next_data_to_use += bufused;
	  update_block_highwater(pblk_tail,inew_addr,
		inew_addr+inew_range,iold_addr,iold_addr+iold_range);
	  pblk_tail->bk_prev_new_addr = new_addr;
	  pblk_tail->bk_prev_old_addr = old_addr;
          pblk_tail->bk_prev_new_range = new_range;
          pblk_tail->bk_prev_old_range = old_range;
	  pblk_tail->bk_numEntries++;
	}
	return retstatus;
}
int _xlate_pro_add_range_ps64(xlate_table_pro tab,
        Block_s     **ppblk_head,
        Block_s     **ppblk_tail,
        Elf64_Addr    inew_addr,
        Elf64_Xword   inew_range,
        Elf64_Addr    iold_addr,
        Elf64_Xword   iold_range,
        highwater_mark *highwater)
{

	int retstatus = XLATE_TB_STATUS_NO_ERROR;

	Elf64_Addr new_addr = inew_addr;
	Elf64_Addr old_addr = iold_addr;
	Elf64_Xword new_range = inew_range;
	Elf64_Xword old_range = iold_range;
	Block_s *pblk_tail;
	char *range_buffer;
	Elf64_Xword lnewaddr;
	Elf64_Sxword loldaddr;
	int bufused;
	Uword byteoff;

        if(*ppblk_head == 0) {
	  retstatus = do_blk_alloc(tab,ppblk_head,ppblk_tail,
		inew_addr,inew_range,iold_addr,iold_range);
	  if(retstatus != XLATE_TB_STATUS_NO_ERROR) {
		return retstatus;
	  }
	  pblk_tail = *ppblk_tail;
	  byteoff = pblk_tail->bk_next_data_to_use;
	  range_buffer = pblk_tail->bk_data + byteoff;
	  lnewaddr = inew_range;
	  loldaddr = 0;
        } else {
	  pblk_tail = *ppblk_tail;
	  if((inew_addr !=  (pblk_tail->bk_prev_new_addr
			+ pblk_tail->bk_prev_new_range))) {
		return XLATE_TB_STATUS_INVALID_SEQUENCE;
	  }
	  byteoff = pblk_tail->bk_next_data_to_use;
	  range_buffer = pblk_tail->bk_data + byteoff;
	  lnewaddr = inew_range;
	  loldaddr = iold_range;
	  loldaddr = old_addr - (Elf64_Addr)(pblk_tail->bk_prev_old_addr);
	}
	if(inew_range != iold_range) {
		return XLATE_TB_STATUS_UNEQUAL_RANGE;
	}



	bufused = _leb128_unsigned_encode64(lnewaddr>>2,range_buffer);
	bufused += _leb128_signed_encode64(loldaddr>>2,range_buffer + bufused);

	if((bufused + byteoff) > TB_BLOCK_SIZE) {
	  Block_s *newtail;
	  char * lclbuf;
	  /* Ran off end of desired area into slop area. 
	  */
	  _xlate_final_update_highwater_addrs(tab,pblk_tail,highwater);

	  /*
		Get new block 
	  */
	  retstatus = do_blk_alloc(tab,ppblk_head,ppblk_tail,
                inew_addr,inew_range,iold_addr,iold_range);
          if(retstatus != XLATE_TB_STATUS_NO_ERROR) {
                return retstatus;
          }
          newtail = *ppblk_tail;
	  lclbuf = newtail->bk_data;
	  lnewaddr = inew_range;
	  loldaddr = 0;
  
	  bufused = _leb128_unsigned_encode64(lnewaddr>>2,lclbuf);
	  bufused += _leb128_signed_encode64(loldaddr>>2,lclbuf + bufused);
	  newtail->bk_next_data_to_use += bufused;
	  newtail->bk_numEntries++;
	  
	} else {
	  /* it fit ! */
	  pblk_tail->bk_next_data_to_use += bufused;
	  update_block_highwater(pblk_tail,inew_addr,
		inew_addr+inew_range,iold_addr,iold_addr+iold_range);
	  pblk_tail->bk_prev_new_addr = new_addr;
	  pblk_tail->bk_prev_old_addr = old_addr;
          pblk_tail->bk_prev_new_range = new_range;
          pblk_tail->bk_prev_old_range = old_range;
	  pblk_tail->bk_numEntries++;
	}
	return retstatus;
}

int _xlate_pro_add_range_ge32(xlate_table_pro tab,
        Block_s     **ppblk_head,
        Block_s     **ppblk_tail,
        Elf64_Addr    inew_addr,
        Elf64_Xword   inew_range,
        Elf64_Addr    iold_addr,
        Elf64_Xword   iold_range,
        highwater_mark *highwater)
{
	int retstatus = XLATE_TB_STATUS_NO_ERROR;

	Elf32_Addr new_addr  = (Elf32_Word)inew_addr;
	Elf32_Addr old_addr  = (Elf32_Word)iold_addr;
	Elf32_Word new_range = (Elf32_Word)inew_range;
	Elf32_Word old_range = (Elf32_Word)iold_range;
	Block_s *pblk_tail;
	char *range_buffer;
	Elf32_Sword loldaddr = 0;
	Elf32_Sword rangedelta;
	int bufused;
	Uword byteoff;

        if(*ppblk_head == 0) {
	  retstatus = do_blk_alloc(tab,ppblk_head,ppblk_tail,
		inew_addr,inew_range,iold_addr,iold_range);
	  if(retstatus != XLATE_TB_STATUS_NO_ERROR) {
		return retstatus;
	  }
	  pblk_tail = *ppblk_tail;
	  byteoff = pblk_tail->bk_next_data_to_use;
	  range_buffer = pblk_tail->bk_data + byteoff;
	  rangedelta = (Elf32_Sword)(new_range - old_range);
	  loldaddr = 0;
        } else {
	  pblk_tail = *ppblk_tail;
	  if((inew_addr !=  (pblk_tail->bk_prev_new_addr
			+ pblk_tail->bk_prev_new_range))) {
		return XLATE_TB_STATUS_INVALID_SEQUENCE;
	  }
	  byteoff = pblk_tail->bk_next_data_to_use;
	  range_buffer = pblk_tail->bk_data + byteoff;
	  rangedelta = (Elf32_Sword)(new_range - old_range);
	  loldaddr = (Elf32_Sword)
		(old_addr - (Elf32_Addr)(pblk_tail->bk_prev_old_addr));
	}


	bufused = _leb128_unsigned_encode32(new_range>>2,range_buffer);
	bufused += _leb128_signed_encode32(loldaddr>>2,range_buffer + 
			bufused);
	bufused += _leb128_signed_encode32(rangedelta>>2,range_buffer + 
			bufused);

	if((bufused + byteoff) > TB_BLOCK_SIZE) {
	  Block_s *newtail;
	  char * lclbuf;
	  /* Ran off end of desired area into slop area. 
	  */
	  _xlate_final_update_highwater_addrs(tab,pblk_tail,highwater);

	  /*
		Get new block 
	  */
	  retstatus = do_blk_alloc(tab,ppblk_head,ppblk_tail,
                inew_addr,inew_range,iold_addr,iold_range);
          if(retstatus != XLATE_TB_STATUS_NO_ERROR) {
                return retstatus;
          }
          newtail = *ppblk_tail;
	  lclbuf = newtail->bk_data;
	  loldaddr = 0;
  
	  bufused = _leb128_unsigned_encode32(new_range>>2,lclbuf);
	  bufused += _leb128_signed_encode32(loldaddr>>2,lclbuf + bufused);
	  bufused += _leb128_signed_encode32(rangedelta>>2,lclbuf +bufused);
	  newtail->bk_next_data_to_use += bufused;
	  newtail->bk_numEntries++;
	  
	} else {
	  /* it fit ! */
	  pblk_tail->bk_next_data_to_use += bufused;
	  update_block_highwater(pblk_tail,inew_addr,
		inew_addr+inew_range,iold_addr,iold_addr+iold_range);
	  pblk_tail->bk_prev_new_addr = new_addr;
	  pblk_tail->bk_prev_old_addr = old_addr;
          pblk_tail->bk_prev_new_range = new_range;
          pblk_tail->bk_prev_old_range = old_range;
	  pblk_tail->bk_numEntries++;
	}
	return retstatus;
}
int _xlate_pro_add_range_ge64(xlate_table_pro tab,
        Block_s     **ppblk_head,
        Block_s     **ppblk_tail,
        Elf64_Addr    inew_addr,
        Elf64_Xword   inew_range,
        Elf64_Addr    iold_addr,
        Elf64_Xword   iold_range,
        highwater_mark *highwater)
{
	int retstatus = XLATE_TB_STATUS_NO_ERROR;

	Elf64_Addr new_addr = inew_addr;
	Elf64_Addr old_addr = iold_addr;
	Elf64_Sxword new_range = inew_range;
	Elf64_Sxword old_range = iold_range;
	Block_s *pblk_tail;
	char *range_buffer;
	Elf64_Sxword loldaddr;
	Elf64_Sxword rangedelta;
	int bufused;
	Uword byteoff;

        if(*ppblk_head == 0) {
	  retstatus = do_blk_alloc(tab,ppblk_head,ppblk_tail,
		inew_addr,inew_range,iold_addr,iold_range);
	  if(retstatus != XLATE_TB_STATUS_NO_ERROR) {
		return retstatus;
	  }
	  pblk_tail = *ppblk_tail;
	  byteoff = pblk_tail->bk_next_data_to_use;
	  range_buffer = pblk_tail->bk_data + byteoff;
	  rangedelta = new_range - old_range;
	  loldaddr = 0;
        } else {
	  pblk_tail = *ppblk_tail;
	  if((inew_addr !=  (pblk_tail->bk_prev_new_addr
			+ pblk_tail->bk_prev_new_range))) {
		return XLATE_TB_STATUS_INVALID_SEQUENCE;
	  }
	  byteoff = pblk_tail->bk_next_data_to_use;
	  range_buffer = pblk_tail->bk_data + byteoff;
	  rangedelta = new_range - old_range;
	  loldaddr = old_addr - (Elf64_Addr)(pblk_tail->bk_prev_old_addr);
	}



	bufused = _leb128_unsigned_encode64(new_range>>2,range_buffer);
	bufused += _leb128_signed_encode64(loldaddr>>2,range_buffer + 
			bufused);
	bufused += _leb128_signed_encode64(rangedelta>>2,range_buffer + 
			bufused);

	if((bufused + byteoff) > TB_BLOCK_SIZE) {
	  Block_s *newtail;
	  char * lclbuf;
	  /* Ran off end of desired area into slop area. 
	  */
	  _xlate_final_update_highwater_addrs(tab,pblk_tail,highwater);

	  /*
		Get new block 
	  */
	  retstatus = do_blk_alloc(tab,ppblk_head,ppblk_tail,
                inew_addr,inew_range,iold_addr,iold_range);
          if(retstatus != XLATE_TB_STATUS_NO_ERROR) {
                return retstatus;
          }
          newtail = *ppblk_tail;
	  lclbuf = newtail->bk_data;
	  loldaddr = 0;
  
	  bufused = _leb128_unsigned_encode64(new_range>>2,lclbuf);
	  bufused += _leb128_signed_encode64(loldaddr>>2,lclbuf + bufused);
	  bufused += _leb128_signed_encode64(rangedelta>>2,lclbuf +bufused);
	  newtail->bk_next_data_to_use += bufused;
	  newtail->bk_numEntries++;
	  
	} else {
	  /* it fit ! */
	  pblk_tail->bk_next_data_to_use += bufused;
	  update_block_highwater(pblk_tail,inew_addr,
		inew_addr+inew_range,iold_addr,iold_addr+iold_range);
	  pblk_tail->bk_prev_new_addr = new_addr;
	  pblk_tail->bk_prev_old_addr = old_addr;
          pblk_tail->bk_prev_new_range = new_range;
          pblk_tail->bk_prev_old_range = old_range;
	  pblk_tail->bk_numEntries++;
	}
	return retstatus;
}


int _xlate_pro_add_range_po32(xlate_table_pro tab,
        Block_s     **ppblk_head,
        Block_s     **ppblk_tail,
        Elf64_Addr    inew_addr,
        Elf64_Xword   inew_range,
        Elf64_Addr    iold_addr,
        Elf64_Xword   iold_range,
        highwater_mark *highwater)
{
	int retstatus = XLATE_TB_STATUS_NO_ERROR;


	Elf32_Addr new_addr = (Elf32_Word)inew_addr;
	Elf32_Addr old_addr = (Elf32_Word)iold_addr;
	Elf32_Word new_range = (Elf32_Word)inew_range;
	Elf32_Word old_range = (Elf32_Word)iold_range;
	Block_s *pblk_tail;
	char *range_buffer;
	Elf32_Word lnewrange;
	Elf32_Word loldrange;
	int bufused;
	Uword byteoff;

        if(*ppblk_head == 0) {
	  retstatus = do_blk_alloc(tab,ppblk_head,ppblk_tail,
		inew_addr,inew_range,iold_addr,iold_range);
	  if(retstatus != XLATE_TB_STATUS_NO_ERROR) {
		return retstatus;
	  }
	  pblk_tail = *ppblk_tail;
	  byteoff = pblk_tail->bk_next_data_to_use;
	  range_buffer = pblk_tail->bk_data + byteoff;
	  lnewrange = (Elf32_Word)inew_range;
	  loldrange = (Elf32_Word)iold_range;
        } else {

	  pblk_tail = *ppblk_tail;
	  if((inew_addr !=  (pblk_tail->bk_prev_new_addr
			+ pblk_tail->bk_prev_new_range))) {
		return XLATE_TB_STATUS_INVALID_SEQUENCE;
	  }
	  if((iold_addr !=  (pblk_tail->bk_prev_old_addr
			+ pblk_tail->bk_prev_old_range))) {
		return XLATE_TB_STATUS_INVALID_SEQUENCE;
	  }
	  byteoff = pblk_tail->bk_next_data_to_use;
	  range_buffer = pblk_tail->bk_data + byteoff;
	  lnewrange = (Elf32_Word)inew_range;
	  loldrange = (Elf32_Word)iold_range;
	}


	bufused = _leb128_unsigned_encode32(lnewrange>>2,range_buffer);
	bufused += _leb128_unsigned_encode32(loldrange>>2,range_buffer + bufused);

	if((bufused + byteoff) > TB_BLOCK_SIZE) {
	  Block_s *newtail;
	  char * lclbuf;
	  /* Ran off end of desired area into slop area. 
	  */
	  _xlate_final_update_highwater_addrs(tab,pblk_tail,highwater);

	  /*
		Get new block 
	  */
	  retstatus = do_blk_alloc(tab,ppblk_head,ppblk_tail,
                inew_addr,inew_range,iold_addr,iold_range);
          if(retstatus != XLATE_TB_STATUS_NO_ERROR) {
                return retstatus;
          }
          newtail = *ppblk_tail;
	  lclbuf = newtail->bk_data;
	  lnewrange = (Elf32_Word)inew_range;
	  loldrange = (Elf32_Word)iold_range;
  
	  bufused = _leb128_unsigned_encode32(lnewrange>>2,lclbuf);
	  bufused += _leb128_unsigned_encode32(loldrange>>2,lclbuf + bufused);
	  newtail->bk_next_data_to_use += bufused;
	  newtail->bk_numEntries++;
	  
	} else {
	  /* it fit ! */
	  pblk_tail->bk_next_data_to_use += bufused;
	  update_block_highwater(pblk_tail,inew_addr,
		inew_addr+inew_range,iold_addr,iold_addr+iold_range);
	  pblk_tail->bk_prev_new_addr = new_addr;
	  pblk_tail->bk_prev_old_addr = old_addr;
          pblk_tail->bk_prev_new_range = new_range;
          pblk_tail->bk_prev_old_range = old_range;
	  pblk_tail->bk_numEntries++;
	}
	return retstatus;
}
int _xlate_pro_add_range_po64(xlate_table_pro tab,
        Block_s     **ppblk_head,
        Block_s     **ppblk_tail,
        Elf64_Addr    inew_addr,
        Elf64_Xword   inew_range,
        Elf64_Addr    iold_addr,
        Elf64_Xword   iold_range,
        highwater_mark *highwater)
{
	int retstatus = XLATE_TB_STATUS_NO_ERROR;



	Elf64_Addr new_addr = inew_addr;
	Elf64_Addr old_addr = iold_addr;
	Elf64_Xword new_range = inew_range;
	Elf64_Xword old_range = iold_range;
	Block_s *pblk_tail;
	char *range_buffer;
	Elf64_Xword lnewrange;
	Elf64_Xword loldrange;
	int bufused;
	Uword byteoff;

        if(*ppblk_head == 0) {
	  retstatus = do_blk_alloc(tab,ppblk_head,ppblk_tail,
		inew_addr,inew_range,iold_addr,iold_range);
	  if(retstatus != XLATE_TB_STATUS_NO_ERROR) {
		return retstatus;
	  }
	  pblk_tail = *ppblk_tail;
	  byteoff = pblk_tail->bk_next_data_to_use;
	  range_buffer = pblk_tail->bk_data + byteoff;
	  lnewrange = inew_range;
	  loldrange = iold_range;
        } else {

	  pblk_tail = *ppblk_tail;

	  if((inew_addr !=  (pblk_tail->bk_prev_new_addr
			+ pblk_tail->bk_prev_new_range))) {
		return XLATE_TB_STATUS_INVALID_SEQUENCE;
	  }
	  if((iold_addr !=  (pblk_tail->bk_prev_old_addr
			+ pblk_tail->bk_prev_old_range))) {
		return XLATE_TB_STATUS_INVALID_SEQUENCE;
	  }
	  byteoff = pblk_tail->bk_next_data_to_use;
	  range_buffer = pblk_tail->bk_data + byteoff;
	  lnewrange = inew_range;
	  loldrange = iold_range;
	}

	bufused = _leb128_unsigned_encode64(lnewrange>>2,range_buffer);
	bufused += _leb128_unsigned_encode64(loldrange>>2,range_buffer + bufused);

	if((bufused + byteoff) > TB_BLOCK_SIZE) {
	  Block_s *newtail;
	  char * lclbuf;
	  /* Ran off end of desired area into slop area. 
	  */
	  _xlate_final_update_highwater_addrs(tab,pblk_tail,highwater);

	  /*
		Get new block 
	  */
	  retstatus = do_blk_alloc(tab,ppblk_head,ppblk_tail,
                inew_addr,inew_range,iold_addr,iold_range);
          if(retstatus != XLATE_TB_STATUS_NO_ERROR) {
                return retstatus;
          }
          newtail = *ppblk_tail;
	  lclbuf = newtail->bk_data;
	  lnewrange = inew_range;
	  loldrange = iold_range;
  
	  bufused = _leb128_unsigned_encode64(lnewrange>>2,lclbuf);
	  bufused += _leb128_unsigned_encode64(loldrange>>2,lclbuf + bufused);
	  newtail->bk_next_data_to_use += bufused;
	  newtail->bk_numEntries++;
	  
	} else {
	  /* it fit ! */
	  pblk_tail->bk_next_data_to_use += bufused;
	  update_block_highwater(pblk_tail,inew_addr,
		inew_addr+inew_range,iold_addr,iold_addr+iold_range);
	  pblk_tail->bk_prev_new_addr = new_addr;
	  pblk_tail->bk_prev_old_addr = old_addr;
          pblk_tail->bk_prev_new_range = new_range;
          pblk_tail->bk_prev_old_range = old_range;
	  pblk_tail->bk_numEntries++;
	}
	return retstatus;
}
