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
    xlate_proTypes.h

    $Revision: 1.1.1.1 $

    This is internal to libxlate.

    Though all evidence of mixed C, C++ variable naming
    styles has been removed from the public interface
    such has not been removed from the private interface
    on grounds that it's not all that embarrassing to leave
    it internally this way.

    The blocks of data are merged appropriately while
    producing the blocks pointed to by tb_std_block_head.

*/

#define  PRO_MAGIC_VAL 0xeff7a102

/* 
Initial low set high so real addr will be lower.
*/
#define INITIAL_LOW_ADDR 0xffffffffffffffffULL

/* Size in bytes of a unit of allocation for the RegInfo buffer. 
   This size is large enough for a typical case so that only
   one malloc is needed (realloc is used, but you get the idea).
*/
#define TB_REGINFO_SIZE	200

	/* 30 bytes (3 64 bit numbers taking 10 byte each)
           is really enough: the extra is just safety.
	*/
#define EXTRA_FOR_FAILSAFE_ENCODE 40

/* Struct to represent each data block being created by producer.  */
typedef struct pro_block_s {
    struct pro_block_s	*bk_next;
	/* First, low, and high addresses 
        ** in this data block. 
	** First needed for understanding block contents.
        ** Low and high used to  set low and high for
	** the applicable output table.
	** Low_old_addr also appears in block header output.
	*/
    Elf64_Addr 		bk_firstNewAddr;	
    Elf64_Addr 		bk_firstOldAddr;	
    Elf64_Addr 		bk_low_old_addr;
    Elf64_Addr 		bk_low_new_addr;
    Elf64_Addr 		bk_high_old_addr;
    Elf64_Addr 		bk_high_new_addr;

    /* following 4 used to do deltas */
    Elf64_Addr          bk_prev_new_addr;
    Elf64_Xword         bk_prev_new_range;
    Elf64_Addr          bk_prev_old_addr;
    Elf64_Xword         bk_prev_old_range;

    /* Number of range entries in this data block. */
    Uword               bk_numEntries;		

    /* next byte of bk_data to write to */
    Uword		bk_next_data_to_use;

	/* Chaining these structs. */
    char                bk_data[EXTRA_FOR_FAILSAFE_ENCODE + TB_BLOCK_SIZE];
} Block_s;

/* These are updated on closing each Block_s
** 
*/
typedef struct highwater_mark_s {
    Elf64_Addr          hw_lowNewAddr;
    Elf64_Addr          hw_lowOldAddr;
    Elf64_Addr          hw_highNewAddr;
    Elf64_Addr          hw_highOldAddr;
} highwater_mark;

/* the form for the set of functions actually adding ranges */
typedef int (*add_range_func) (xlate_table_pro tab,
	Block_s       **ppblk_head,
	Block_s       **ppblk_tail,
        Elf64_Addr      new_addr,
        Elf64_Xword     new_range,
        Elf64_Addr      old_addr,
        Elf64_Xword     old_range,
	highwater_mark *highwater);


	

    /* Main handle to the table for the producer interface. */
struct xlate_table_pro_s {
    int			tb_magic;
	/* For blocks representing standard (normal) output 
	   Meaning the input and the xlate_table_con data (if any)was merged
	   appropriately.
	*/

    /* input table for merging  or NULL*/
    xlate_table_con      tb_con_table;

    /* standard/normal (composed final)output data */
    Block_s		*tb_std_block_head;
    Block_s		*tb_std_block_tail;
    highwater_mark       tb_std_highwater;
    add_range_func	 tb_std_add_range;
    xlate_tablekind	 tb_std_tablekind;

    /* debug (one stage) output data */
    Block_s		*tb_debug_block_head;
    Block_s		*tb_debug_block_tail;
    highwater_mark       tb_debug_highwater;
    add_range_func	 tb_debug_add_range;
    xlate_tablekind	 tb_debug_tablekind;

	/* from xlate_pro_add_info */
    Elf64_Sxword         tb_data_moved;
    Elf64_Addr           tb_startup_fwa;
    Elf64_Addr           tb_startup_lwa;
    char          	 tb_old_text_exists;
    char                 tb_old_text_alloc;

    char		 tb_is64bit;

	/*
	    xlate_pro_disk_header() has been
	    called, and total memory required has been notified
	    to user.  No further xlate_pro_add_entry() calls
	    should be made after this flag is set.
	*/
    char		 tb_no_new_entries_allowed;

	/* set non-zero once xlate_pro_disk_header called on std */
    char		 tb_header_called_on_std;
	/* set non-zero once xlate_pro_disk_header called on debug */
    char		 tb_header_called_on_debug;

	/* 0 means xlate_pro_disk_header not called yet. */
	/* 1 means called with XLATE_PRO_STANDARD_SETUP (1) */
	/* 2 means called with XLATE_PRO_DEBUG_SETUP   (2) */
    char                 tb_header_set_to_std_or_debug;

	/* 
	    tb_headerRet is flag that is set to TRUE when the
	    main table header has been returned to the user on 
	    the first xlate_pro_disk_next_block() call.
	    Subsequent xlate_pro_disk_next_block() calls must
	    return successive blocks of data.
	*/
    char		 tb_std_header_returned;
    char		 tb_debug_header_returned;


	/*non-zero once register info returned to caller via
	** xlate_pro_next_disk_block call
	*/
    char		 tb_reginfoRet; 

	/* 
	    Pointer to the last data block returned to the user
	    with a xlate_pro_disk_next_block() call.
	    This is used to iterate through the lists
	    headed by tb_std_block_head and tb_debug_block_head.
	*/
    Block_s		*tb_blockRet;
  
        /* the register info is the info input in the current
	** production  (meaning this is the debug info, as
 	   input here. Nothing from the input xlate_table_con).
	*/
	/* Pointer to buffer for register info. */
    char		*tb_regInfo;

	/* Size of currently malloc-ed tb_regInfo area. */
    Uword		 tb_regInfoSize;

	/* Index into tb_regInfo to write the next entry. */
    Uword		 tb_regInfoOffset;

/* these are for doing tentative merge ranges
*/
    int			tb_non_empty_range;
    Elf64_Addr  	tb_save_new_address;
    Elf64_Xword 	tb_save_new_range;
    Elf64_Addr  	tb_save_old_address;
    Elf64_Xword 	tb_save_old_range;
};
