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
   xlate_pro_disk_next_block.c
   $Revision: 1.1.1.1 $

   If this is the first call, we return a pointer to a set of
   headers (xlate header plus array of block headers)

   This also does the necessary merge for headers and reginfo.

   As long as block data exists, return the next block data.

   And, last, return the the register info block (if any).

*/

#include "xlateincl.h"

#ifdef _LIBELF_XTND_EXPANDED_DATA
#pragma weak xlate_pro_disk_next_block_xtnd = _xlate_pro_disk_next_block_xtnd
#elif defined(BUILD_OS_DARWIN)
#else
#pragma weak xlate_pro_disk_next_block = _xlate_pro_disk_next_block
#endif

static int 
do_64bit_merge_header(xlate_table_pro  table,
    char**        data,
    Elf64_Xword *       data_size)
{


    Uword bytes_used  = 0;
    char * data_ret   = 0;
    Uword block_count = 0;
    Uword hdr_size    = sizeof(xlate_header64_v2);
    Uword block_size  = sizeof(xlate_blockheader64_v2);
    xlate_table_con contab   = table->tb_con_table;


    xlate_header64_v2 *hdrp;
    xlate_blockheader64_v2 *base_oblkp;
    xlate_blockheader64_v2 *cur_oblkp;

    Block_s *blk      = table->tb_blockRet;
    Uword numentries = 0;

    while(blk) {
	++block_count;
	numentries += blk->bk_numEntries;
	blk = blk->bk_next;
    }

    bytes_used = block_count*block_size + hdr_size;

    if(data == 0) {
       *data_size = bytes_used;
       return XLATE_TB_STATUS_NO_ERROR;
    }
    data_ret = malloc(bytes_used);
    if(data_ret == 0) {
	return XLATE_TB_STATUS_ALLOC_FAIL;
    }

    /*to ensure no random bits in the output. */
    BZERO(data_ret,bytes_used);

    hdrp = (xlate_header64_v2 *)data_ret;
    base_oblkp = (xlate_blockheader64_v2*) (hdrp+1);

    hdrp->hd_version = XLATE_TB_64_V2;
	
      hdrp->hd_tablekind = table->tb_std_tablekind;
      hdrp->hd_new_addr_low = table->tb_std_highwater.hw_lowNewAddr;
      hdrp->hd_new_addr_high = table->tb_std_highwater.hw_highNewAddr;
      hdrp->hd_old_addr_low = table->tb_std_highwater.hw_lowOldAddr;
      hdrp->hd_old_addr_high = table->tb_std_highwater.hw_highOldAddr;
    {
      Elf64_Sxword data_moved;
      Elf64_Addr newlow;
      Elf64_Addr oldlow;
      Elf64_Addr newhigh;
      Elf64_Addr oldhigh;
      Elf64_Addr startfwa;
      Elf64_Addr startlwa;
      int               old_text_exists;
      int               old_text_alloc;
      int               is64bit ;
      xlate_tablekind   tablekind ;
      int                tableversion ;
      int	        retstatus;

      retstatus = xlate_get_info(contab,
	&data_moved,&newlow, &oldlow, &newhigh, &oldhigh,
	&startfwa,&startlwa,
	/*numEntries=*/0,
	&old_text_exists,&old_text_alloc,
	&is64bit,&tablekind,&tableversion);
      if(retstatus  != XLATE_TB_STATUS_NO_ERROR) {
	return retstatus;
      }

      hdrp->hd_old_text_exists = old_text_exists;
      hdrp->hd_old_text_alloc = old_text_alloc;
      hdrp->hd_block_size =  TB_BLOCK_SIZE;
      hdrp->hd_reg_info_size = table->tb_regInfoOffset;
      hdrp->hd_num_blocks = block_count;
      hdrp->hd_num_entries = numentries;

  
      hdrp->hd_data_moved = 
	data_moved +  (Elf64_Sxword)table->tb_data_moved;
      hdrp->hd_startup_fwa = table->tb_startup_fwa;
      if(hdrp->hd_startup_fwa == 0) {
	hdrp->hd_startup_fwa = startfwa;
      }
      hdrp->hd_startup_lwa = table->tb_startup_lwa;
      if(hdrp->hd_startup_lwa == 0) {
	hdrp->hd_startup_lwa = startlwa;
      }

    }
    blk      = table->tb_blockRet;
    cur_oblkp = base_oblkp;
    for( ; blk; blk = blk->bk_next, ++cur_oblkp ) {
	cur_oblkp->bh_first_new_addr = blk->bk_firstNewAddr;
	cur_oblkp->bh_first_old_addr = blk->bk_firstOldAddr;
	cur_oblkp->bh_num_entries    = blk->bk_numEntries;
	cur_oblkp->bh_high_old_addr  = blk->bk_high_old_addr;
	cur_oblkp->bh_low_old_addr   = blk->bk_low_old_addr;
    }

    *data = data_ret;
    *data_size = bytes_used;
    return XLATE_TB_STATUS_NO_ERROR;
}

static int 
do_32bit_merge_header(xlate_table_pro  table,
    char**        data,
    Elf64_Xword *       data_size)
{

    Uword bytes_used  = 0;
    char * data_ret   = 0;
    Uword block_count = 0;
    Uword hdr_size    = sizeof(xlate_header32_v2);
    Uword block_size  = sizeof(xlate_blockheader32_v2);
    xlate_table_con contab   = table->tb_con_table;


    xlate_header32_v2 *hdrp;
    xlate_blockheader32_v2 *base_oblkp;
    xlate_blockheader32_v2 *cur_oblkp;

    Block_s *blk      = table->tb_blockRet;
    Uword numentries = 0;

    while(blk) {
	++block_count;
	numentries += blk->bk_numEntries;
	blk = blk->bk_next;
    }

    bytes_used = block_count*block_size + hdr_size;

    if(data == 0) {
       *data_size = bytes_used;
       return XLATE_TB_STATUS_NO_ERROR;
    }
    data_ret = malloc(bytes_used);
    if(data_ret == 0) {
	return XLATE_TB_STATUS_ALLOC_FAIL;
    }

    /*to ensure no random bits in the output. */
    BZERO(data_ret,bytes_used);

    hdrp = (xlate_header32_v2 *)data_ret;
    base_oblkp = (xlate_blockheader32_v2*) (hdrp+1);

    hdrp->hd_version = XLATE_TB_32_V2;
	
      hdrp->hd_tablekind = table->tb_std_tablekind;
      hdrp->hd_new_addr_low = 
		(Elf32_Word)table->tb_std_highwater.hw_lowNewAddr;
      hdrp->hd_new_addr_high = 
		(Elf32_Word)table->tb_std_highwater.hw_highNewAddr;
      hdrp->hd_old_addr_low = 
		(Elf32_Word)table->tb_std_highwater.hw_lowOldAddr;
      hdrp->hd_old_addr_high = 
		(Elf32_Word)table->tb_std_highwater.hw_highOldAddr;
    {
      Elf64_Sxword data_moved;
      Elf64_Addr newlow;
      Elf64_Addr oldlow;
      Elf64_Addr newhigh;
      Elf64_Addr oldhigh;
      Elf64_Addr startfwa;
      Elf64_Addr startlwa;
      int               old_text_exists;
      int               old_text_alloc;
      int               is64bit ;
      xlate_tablekind   tablekind ;
      int                tableversion ;
      int	         retstatus;
  
      retstatus = xlate_get_info(contab,
	&data_moved,&newlow, &oldlow, &newhigh, &oldhigh,
	&startfwa,&startlwa,
	/*num_of_entries=*/0,
	&old_text_exists,&old_text_alloc,
	&is64bit,&tablekind,&tableversion);
      if(retstatus  != XLATE_TB_STATUS_NO_ERROR) {
	return retstatus;
      }

      hdrp->hd_old_text_exists = (unsigned char)old_text_exists;
      hdrp->hd_old_text_alloc = (unsigned char)old_text_alloc;
      hdrp->hd_block_size =  TB_BLOCK_SIZE;
      hdrp->hd_reg_info_size = (Elf32_Sword)table->tb_regInfoOffset;
      hdrp->hd_num_blocks = (Elf32_Sword)block_count;
      hdrp->hd_num_entries = (Elf32_Sword)numentries;

  
      hdrp->hd_data_moved =(Elf32_Sword)
	((Elf64_Sxword)data_moved + (Elf64_Sxword)table->tb_data_moved);
      hdrp->hd_startup_fwa = (Elf32_Word)table->tb_startup_fwa;
      if(hdrp->hd_startup_fwa == 0) {
	hdrp->hd_startup_fwa = 
		(Elf32_Word)startfwa;
      }
      hdrp->hd_startup_lwa = (Elf32_Word)table->tb_startup_lwa;
      if(hdrp->hd_startup_lwa == 0) {
	  hdrp->hd_startup_lwa = 
		(Elf32_Word)startlwa;
      }
    }

    blk      = table->tb_blockRet;
    cur_oblkp = base_oblkp;
    for( ; blk; blk = blk->bk_next, ++cur_oblkp ) {
	cur_oblkp->bh_first_new_addr = 
		(Elf32_Word)blk->bk_firstNewAddr;
	cur_oblkp->bh_first_old_addr = 
		(Elf32_Word)blk->bk_firstOldAddr;
	cur_oblkp->bh_num_entries    = 
		(Elf32_Word)blk->bk_numEntries;
	cur_oblkp->bh_high_old_addr  = 
		(Elf32_Word)blk->bk_high_old_addr;
	cur_oblkp->bh_low_old_addr   = 
		(Elf32_Word)blk->bk_low_old_addr;
    }

    *data = data_ret;
    *data_size = bytes_used;
    return XLATE_TB_STATUS_NO_ERROR;
}

/*
This is *wrong* if the contab has any reginfo with
a set_loc or advance_loc.

We would need to modify those for this to be right.

This is a bug/restriction.

*/
static int
do_reginfo_merge(xlate_table_pro  table,
    char**        data,
    Elf64_Xword *       data_size)
{
    Uword new_bytes_used     = table->tb_regInfoOffset;
    xlate_table_con contab   = table->tb_con_table;


    char *  base_reginfo     = contab->xc_reginfo_data;
    Uword   base_reginfo_len = contab->xc_hdr.ich_total_reginfo_bytes;

    Uword bytes_used  = new_bytes_used + base_reginfo_len;
    char* data_ret = 0;

    if(bytes_used == 0) {
	  return XLATE_TB_STATUS_ALREADY_DONE;
    }

    if(data == 0) {
       *data_size = bytes_used;
       return XLATE_TB_STATUS_NO_ERROR;
    }
    data_ret = (char *)malloc(bytes_used);
    if(data_ret == 0) {
        return XLATE_TB_STATUS_ALLOC_FAIL;
    }
    if(base_reginfo_len > 0) {
      memcpy(data_ret,base_reginfo, base_reginfo_len);
    }
    if(new_bytes_used > 0) {
       memcpy(data_ret+base_reginfo_len,table->tb_regInfo,new_bytes_used);
    }
    *data_size = bytes_used;
    *data = data_ret;
    return XLATE_TB_STATUS_NO_ERROR;

}
/* prepare and malloc space for the xlate table header plus
** space for N blocks of data
*/
static int
_xlate_do_simple_32bit_header(xlate_table_pro  table,
    char**        data,
    Elf64_Xword *       data_size)
{
    Uword bytes_used  = 0;
    char * data_ret   = 0;
    Uword block_count = 0;
    Uword hdr_size    = sizeof(xlate_header32_v2);
    Uword block_size  = sizeof(xlate_blockheader32_v2);

    xlate_header32_v2 *hdrp;
    xlate_blockheader32_v2 *base_oblkp;
    xlate_blockheader32_v2 *cur_oblkp;

    Block_s *blk      = table->tb_blockRet;
    Uword numentries = 0;

    while(blk) {
	++block_count;
	numentries += blk->bk_numEntries;
	blk = blk->bk_next;
    }

    bytes_used = block_count*block_size + hdr_size;

    if(data == 0) {
       *data_size = bytes_used;
       return XLATE_TB_STATUS_NO_ERROR;
    }
    data_ret = malloc(bytes_used);
    if(data_ret == 0) {
	return XLATE_TB_STATUS_ALLOC_FAIL;
    }

    /*to ensure no random bits in the output. */
    BZERO(data_ret,bytes_used);

    hdrp = (xlate_header32_v2 *)data_ret;
    base_oblkp = (xlate_blockheader32_v2*) (hdrp+1);

    hdrp->hd_version = XLATE_TB_32_V2;
    if(table->tb_header_set_to_std_or_debug == XLATE_PRO_STANDARD_SETUP){
      hdrp->hd_tablekind = table->tb_std_tablekind;
      hdrp->hd_new_addr_low = 
		(Elf32_Word)table->tb_std_highwater.hw_lowNewAddr;
      hdrp->hd_new_addr_high = 
		(Elf32_Word)table->tb_std_highwater.hw_highNewAddr;
      hdrp->hd_old_addr_low = 
		(Elf32_Word)table->tb_std_highwater.hw_lowOldAddr;
      hdrp->hd_old_addr_high = 
		(Elf32_Word)table->tb_std_highwater.hw_highOldAddr;
    } else {
      hdrp->hd_tablekind = table->tb_debug_tablekind;
      hdrp->hd_new_addr_low = 
		(Elf32_Word)table->tb_debug_highwater.hw_lowNewAddr;
      hdrp->hd_new_addr_high = 
		(Elf32_Word)table->tb_debug_highwater.hw_highNewAddr;
      hdrp->hd_old_addr_low = 
		(Elf32_Word)table->tb_debug_highwater.hw_lowOldAddr;
      hdrp->hd_old_addr_high = 
		(Elf32_Word)table->tb_debug_highwater.hw_highOldAddr;
    }

    hdrp->hd_old_text_exists = (unsigned char)table->tb_old_text_exists;
    hdrp->hd_old_text_alloc = (unsigned char)table->tb_old_text_alloc;
    hdrp->hd_block_size =  TB_BLOCK_SIZE;
    hdrp->hd_reg_info_size = (Elf32_Word)table->tb_regInfoOffset;
    hdrp->hd_num_blocks = (Elf32_Word)block_count;
    hdrp->hd_num_entries = (Elf32_Sword)numentries;

  
    hdrp->hd_data_moved = (Elf32_Sword)table->tb_data_moved;
    hdrp->hd_startup_fwa = (Elf32_Word)table->tb_startup_fwa;
    hdrp->hd_startup_lwa = (Elf32_Word)table->tb_startup_lwa;

    blk      = table->tb_blockRet;
    cur_oblkp = base_oblkp;
    for( ; blk; blk = blk->bk_next, ++cur_oblkp ) {
	cur_oblkp->bh_first_new_addr = 
				(Elf32_Word)blk->bk_firstNewAddr;
	cur_oblkp->bh_first_old_addr = 
				(Elf32_Word)blk->bk_firstOldAddr;
	cur_oblkp->bh_num_entries    = 
				(Elf32_Word)blk->bk_numEntries;
	cur_oblkp->bh_high_old_addr  = 
				(Elf32_Word)blk->bk_high_old_addr;
	cur_oblkp->bh_low_old_addr   = 
				(Elf32_Word)blk->bk_low_old_addr;
    }

    *data = data_ret;
    *data_size = bytes_used;
    return XLATE_TB_STATUS_NO_ERROR;
}

/* prepare and malloc space for the xlate table header plus
** space for N blocks of data
*/
static int
_xlate_do_simple_64bit_header(xlate_table_pro  table,
    char**        data,
    Elf64_Xword *       data_size)
{
    Uword bytes_used  = 0;
    char * data_ret   = 0;
    Uword block_count = 0;
    Uword hdr_size    = sizeof(xlate_header64_v2);
    Uword block_size  = sizeof(xlate_blockheader64_v2);

    xlate_header64_v2 *hdrp;
    xlate_blockheader64_v2 *base_oblkp;
    xlate_blockheader64_v2 *cur_oblkp;

    Block_s *blk      = table->tb_blockRet;
    Uword numentries = 0;

    while(blk) {
	++block_count;
	numentries += blk->bk_numEntries;
	blk = blk->bk_next;
    }

    bytes_used = block_count*block_size + hdr_size;
    if(data == 0) {
       *data_size = bytes_used;
       return XLATE_TB_STATUS_NO_ERROR;
    }

    data_ret = malloc(bytes_used);
    if(data_ret == 0) {
	return XLATE_TB_STATUS_ALLOC_FAIL;
    }

    /*to ensure no random bits in the output */
    BZERO(data_ret,bytes_used);

    hdrp = (xlate_header64_v2 *)data_ret;
    base_oblkp = (xlate_blockheader64_v2*) (hdrp+1);

    hdrp->hd_version = XLATE_TB_64_V2;
    if(table->tb_header_set_to_std_or_debug == XLATE_PRO_STANDARD_SETUP){
      hdrp->hd_tablekind = table->tb_std_tablekind;
      hdrp->hd_new_addr_low = table->tb_std_highwater.hw_lowNewAddr;
      hdrp->hd_new_addr_high = table->tb_std_highwater.hw_highNewAddr;
      hdrp->hd_old_addr_low = table->tb_std_highwater.hw_lowOldAddr;
      hdrp->hd_old_addr_high = table->tb_std_highwater.hw_highOldAddr;
    } else {
      hdrp->hd_tablekind = table->tb_debug_tablekind;
      hdrp->hd_new_addr_low = table->tb_debug_highwater.hw_lowNewAddr;
      hdrp->hd_new_addr_high = table->tb_debug_highwater.hw_highNewAddr;
      hdrp->hd_old_addr_low = table->tb_debug_highwater.hw_lowOldAddr;
      hdrp->hd_old_addr_high = table->tb_debug_highwater.hw_highOldAddr;
    }

    hdrp->hd_old_text_exists = table->tb_old_text_exists;
    hdrp->hd_old_text_alloc = table->tb_old_text_alloc;
    hdrp->hd_block_size =  TB_BLOCK_SIZE;
    hdrp->hd_reg_info_size = table->tb_regInfoOffset;
    hdrp->hd_num_blocks = block_count;
    hdrp->hd_num_entries = numentries;

  
    hdrp->hd_data_moved = table->tb_data_moved;
    hdrp->hd_startup_fwa = table->tb_startup_fwa;
    hdrp->hd_startup_lwa = table->tb_startup_lwa;

    blk      = table->tb_blockRet;
    cur_oblkp = base_oblkp;
    for( ; blk; blk = blk->bk_next, ++cur_oblkp ) {
	cur_oblkp->bh_first_new_addr = blk->bk_firstNewAddr;
	cur_oblkp->bh_first_old_addr = blk->bk_firstOldAddr;
	cur_oblkp->bh_num_entries    = blk->bk_numEntries;
	cur_oblkp->bh_high_old_addr  = blk->bk_high_old_addr;
	cur_oblkp->bh_low_old_addr   = blk->bk_low_old_addr;
    }

    *data = data_ret;
    *data_size = bytes_used;
    return XLATE_TB_STATUS_NO_ERROR;
}


static int
_xlate_do_local_reginfo(xlate_table_pro  table,
    char**        data,
    Elf64_Xword *       data_size)
{
    Uword bytes_used = table->tb_regInfoOffset;
    char *data_ret;

    if(data == 0) {
       *data_size = bytes_used;
       return XLATE_TB_STATUS_NO_ERROR;
    }
    data_ret = (char *)malloc(bytes_used);
    if(data_ret == 0) {
	return XLATE_TB_STATUS_ALLOC_FAIL;
    }
    memcpy(data_ret,table->tb_regInfo, bytes_used);
    *data_size = bytes_used;
    *data = data_ret;
    return XLATE_TB_STATUS_NO_ERROR;
}


static int
_xlate_do_current_block(xlate_table_pro  table,
    char**        data,
    Elf64_Xword *       data_size)
{
    char *data_ret;
    Block_s *curblock = table->tb_blockRet;
    Uword bytes_initialized = curblock->bk_next_data_to_use;

    table->tb_blockRet = curblock->bk_next;

    if(data == 0) {
       *data_size = TB_BLOCK_SIZE;
       return XLATE_TB_STATUS_NO_ERROR;
    }
    data_ret = (char *)malloc(TB_BLOCK_SIZE);
    if(data_ret == 0) {
	return XLATE_TB_STATUS_ALLOC_FAIL;
    }
    memcpy(data_ret,curblock->bk_data, bytes_initialized);
    if(bytes_initialized <TB_BLOCK_SIZE) {
        BZERO(data_ret + bytes_initialized,TB_BLOCK_SIZE - bytes_initialized);
    }

    /* set for next block */

    *data_size = TB_BLOCK_SIZE;
    *data = data_ret;
    return XLATE_TB_STATUS_NO_ERROR;
}

int xlate_pro_disk_next_block(xlate_table_pro  table,
    char**        data,
    Elf64_Xword *       data_size)
{
    int retstatus = XLATE_TB_STATUS_NO_ERROR;
    if(table->tb_magic != PRO_MAGIC_VAL) {
        return XLATE_TB_STATUS_INVALID_TABLE;
    }
    if(table->tb_no_new_entries_allowed == 0) {
	/* asked for block before calling xlate_pro_disk_header */
	return XLATE_TB_STATUS_BLOCK_REQ_SEQ_ERR;
    }
    if(table->tb_header_set_to_std_or_debug == XLATE_PRO_STANDARD_SETUP) {
     if(table->tb_con_table) {
	/* merge required */
       if(table->tb_std_header_returned == 0) {
	  if(table->tb_is64bit) {
	    retstatus = do_64bit_merge_header(table,data,data_size);
	  } else {
	    retstatus = do_32bit_merge_header(table,data,data_size);
	  }
	  table->tb_std_header_returned = 1;
       } else if(table->tb_blockRet) {
	   retstatus = _xlate_do_current_block(table,data,data_size);
       } else if(table->tb_reginfoRet == 0) {
	  /* might be no reginfo data present! beware. 
	     do_reginfo_merge
		returns XLATE_TB_STATUS_ALREADY_DONE in that case
	  */
	  retstatus = do_reginfo_merge(table,data,data_size);
	  table->tb_reginfoRet = 1;
       } else {
	  return XLATE_TB_STATUS_ALREADY_DONE;
       }
     } else {
       if(table->tb_std_header_returned == 0) {
	  if(table->tb_is64bit) {
	    retstatus = _xlate_do_simple_64bit_header(table,data,data_size);
	  } else {
	    retstatus = _xlate_do_simple_32bit_header(table,data,data_size);
	  }
	  table->tb_std_header_returned = 1;
       } else if(table->tb_blockRet) {
	   retstatus = _xlate_do_current_block(table,data,data_size);
       } else if(table->tb_reginfoRet == 0 && table->tb_regInfoOffset > 0) {
	   retstatus = _xlate_do_local_reginfo(table,data,data_size);
	   table->tb_reginfoRet = 1;
       } else {
	  return XLATE_TB_STATUS_ALREADY_DONE;
       }
     }
    } else if(table->tb_header_set_to_std_or_debug == XLATE_PRO_DEBUG_SETUP){
     if(table->tb_std_header_returned == 0) {
	if(table->tb_is64bit) {
	    retstatus = _xlate_do_simple_64bit_header(table,data,data_size);
	} else {
	    retstatus = _xlate_do_simple_32bit_header(table,data,data_size);
	}
	table->tb_std_header_returned = 1;
     } else if(table->tb_blockRet) {
	   retstatus = _xlate_do_current_block(table,data,data_size);
     } else if(table->tb_reginfoRet == 0 && table->tb_regInfoOffset > 0) {
	   retstatus = _xlate_do_local_reginfo(table,data,data_size);
	   table->tb_reginfoRet = 1;
     } else {
	return XLATE_TB_STATUS_ALREADY_DONE;
     }
    } else {
	return XLATE_TB_STATUS_PRO_REQ_INVALID;
    }


    return retstatus;
}

