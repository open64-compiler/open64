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
    xlateTypes.h

    $Revision: 1.1.1.1 $

    This is internal to the xlate functions.

    Though all evidence of mixed C, C++ variable naming
    styles has been removed from the public interface
    such has not been removed from the private interface
    on grounds that it's not all that embarrassing to leave
    it internally this way.

    In many cases, rather than do run-time tests of the
    tablekind  and the table version to determine
    how to process, we initialize a function pointer
    in the xlate_table_con structure, choosing
    a function specific to the tablekind and/or table version.

    See xlate_init_elf.c for the initialization logic.

*/

#define FALSE	0
#define TRUE	1


    /* Size in bytes of a data block. */
#define TB_BLOCK_SIZE	4096

    /* magic number for consumer table */
#define VALID_TABLE_MAGIC 0xa100f034


/* these macros are used to control the internal register rule code */
#define ALL_INSTRUCTIONS 0
#define ALL_REG_RULES    1
#define REG_RULE_AT_PC   2


#ifndef TURN_OFF_MEMO
/*
	This is used to hold data so that the xlate_address()
	code does not always have to start from scratch.
	It is used purely to get better performance.

	Because the xlate_address() routines are set at init time
	there is no need to record which xlate routine is using
	this data.
*/
struct memo_s {
	char mm_is_valid; /* 0 means invalid (and the
			** rest of the data here must be
			** ignored, non-zero means
			** the rest of the data here is valid 
			*/
	char mm_is_new_address; /* non-zero if last use 
			** was new-to-old xlate_address,
			**  zero if last was old-to-new.
			*/


	Elf64_Xword  mm_entryct; /* memo of loop counter in
				xlate routine */

	Uword        mm_ublock_index; /* memo of the uniform_block
			index of the memo data */
	Uword        mm_entrymax; /* entry count, actually 
			number of entries in entry array
			and in code byte stream  */

	Elf64_Addr   mm_low_new_addr;
        Elf64_Addr   mm_high_new_addr;
        Elf64_Addr   mm_low_old_addr;
        Elf64_Addr   mm_high_old_addr;


	xlate_block mm_block; /* the last translation block
		returned */
};

#endif


/* this is a common form for block headers.
*/
typedef  struct uniform_blk_hdr_s {
  Elf64_Addr  ub_first_new_addr;
  Elf64_Addr  ub_first_old_addr;

  Elf64_Addr  ub_low_new_addr;
  Elf64_Addr  ub_low_old_addr;

  Elf64_Addr  ub_high_new_addr; 
  Elf64_Addr  ub_high_old_addr; 
  xlate_block *ub_expanded_entries; /* 0 initially.
		When non-null, points at array of ub_entrycount
		expanded table entries. 
		These are the data bytes, expanded into xlate blocks.
		Always a complete set, or a 0 pointer.
		*/

  char *      ub_data_bytes;
  char *      ub_data_end;
  Uword       ub_entrycount; /* count, the number of entries
			in the coded and also the unpacked,
			entry areas.  Think of this as a 'max'
			*/
  Uword       ub_v1_entrycount; /* v1 is wierd: we don't want to
		screw up ub_entrycount but need a limit that
		is decided at run time. 
		We don't want to screw up entry count in case we
		ever decide to freeup ub_expanded_entries
		which would mean recalcaluating the count and
		we don't want to goof up ub_entrycount by
		changing it from what was on disk.
		*/

  Uword       ub_block_index; /* The index of this block
		in the array.  This is  slightly redundant
		but it means we can pass the block pointer
		and the block knows what number it is, rather
		than having to pass both.
		Only set once the block header itself is initialized.
		*/
  char        ub_ubh_is_set_up; /* non-zero after data entered
		in block , though ub_expanded_entries can
		be 0: the ub_expanded_entries are a time
		optimization (at a cost in malloc space) */

} uniform_block_hdr;

typedef void (*get_info_func) (xlate_table_con tab,
    Elf64_Sxword * dataMoved,
    Elf64_Addr   * startup_fwa,
    Elf64_Addr   * startup_lwa,
    Elf64_Xword   * number_of_ranges,
    int 	 * old_text_exists,
    int 	 * old_text_alloc);

typedef int (*search_for_addr)(xlate_table_con tab,
	int          isNewAddress,
	Elf64_Addr   addr_in,
	Elf64_Addr*  addr_out,
	xlate_block* range_out);


typedef int (*get_range_from_block)(xlate_table_con tab,
	int                isNewAddress, /* 0,1, or 2 */
	Elf64_Addr         addr_in,
	int		   restart,
	uniform_block_hdr *blk_hdr,

	Elf64_Addr*        addr_out, 
	xlate_block*       range_out);

typedef int (*index_into_blockhdrs)(xlate_table_con tab,
	int                isNewAddress,
	Uword              blockindex,
	Elf64_Addr        *address_low_out,
	Elf64_Addr        *address_high_out,
	uniform_block_hdr *blk_info_out);


struct internal_con_hdr_s {
	int    ich_version;
	char   ich_tablekind;
	Uword  ich_block_size;
	Uword  ich_num_blocks;
	Uword  ich_total_num_entries;
	Uword  ich_total_reginfo_bytes;
	Xuword ich_new_addr_low;
	Xuword ich_new_addr_high;
	Xuword ich_old_addr_low;
	Xuword ich_old_addr_high;
};

/* 
   The handle for the Consumer Library. 
*/
struct xlate_table_con_s {
    	/* this magic number validates that the passed
        ** xlate_table_con_s is real and open
	** and not corrupted by malloc overrun somewhere
	*/
    Uword		xc_valid_table;
 

    char		xc_is64bit; /* non-zero if 64-bit elf file */
    char		xc_did_elf_begin; /* non-zero if
				we opened elf file ourself.*/


	/* 
	    The Elf handle used to
	    read the executable/DSO. 
	    Keeping this means no need malloc space for the xlate
	    section data.
	*/
    Elf			*xc_elf;

	/* the xlate data bytes, beginning with the header */
    Uword               xc_data_size;
    char 	* 	xc_section_data;
    char 	* 	xc_block_headers;
    char 	* 	xc_leb_data_blocks;
    char	*       xc_reginfo_data;

	/* function ptrs */
    get_info_func                  xc_get_info_func;
    search_for_addr                xc_search_for_addr_old;
    search_for_addr                xc_search_for_addr_new;
    index_into_blockhdrs           xc_block_header_indexer;
    get_range_from_block           xc_get_range_from_block;


    uniform_block_hdr   *          xc_ubh_array; /* xc_hdr.ich_num_blocks
			block headers in an array. Initially
			zeroed, each filled in as needed (any given
			one might never get filled in).
			Each in its turn pointing to its data bytes
			and possibly an expanded table.
			*/

	
    struct internal_con_hdr_s      xc_hdr;
    
#ifndef TURN_OFF_MEMO
	/* prof(1) has a habit of switching back and forth
           in its translations. (537714).
	   So we now keep one memo buffer for each
	   direction to maximize the possibility of getting
	   a value quickly.
	*/
    struct memo_s		   xc_memo_new; /* memoize
	state for new->old translations. */

    struct memo_s		   xc_memo_old; /* memoize
	state for old->new translations */

#endif

}; 

