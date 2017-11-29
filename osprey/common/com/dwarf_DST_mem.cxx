/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


/* ====================================================================
 * ====================================================================
 *
 * Module: dwarf_DST_mem.c
 *
 * Revision history:
 *  25-Apr-93 - Original Version
 *
 * Description:
 *
 * The DST info is internally represented as an array of block headers,
 * where each block header contains the kind, size, and offset of a block
 * of DST data.  In particular, there should be one block each for the
 * file-scope, include, file-names, and macro info, then one block for
 * each local-scope PU info.  
 *
 * When a block grows beyond its size, we allocate a new block.
 * We can't realloc space because there are places where we use a pointer
 * to the info in order to get the attributes.
 * In the intermediate file, only the exact size of the blocks 
 * is written and read.
 * ====================================================================
 * ====================================================================
 */

#ifdef USE_PCH
#include "common_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#include "defs.h"
#include "mempool.h"
#define USE_DST_INTERNALS
#include "dwarf_DST_mem.h"
#include "errors.h"
#include "stab.h"

static inline void *
Symtab_Alloc (size_t bytes, BOOL is_global)
{
    if ( is_global ) {
	return (void *) Src_Alloc ( bytes );
    } else {
	return (void *) Pu_Alloc ( bytes );
    }
}


#define NUM_BLOCK_HEADERS 1024		/* initial # of block headers */
#define BLOCK_SIZE 256			/* size of individual block */

DST_TYPE Current_DST = NULL;
DST_Type *current_DST = NULL;


/* Represents a NULL DST_IDX value. Always initialize a declared
 * DST_IDX with this value!!
*/
const DST_IDX DST_INVALID_IDX = {DST_INVALID_BLOCK_IDX, DST_INVALID_BYTE_IDX};


         /*----------------*
          * Error Checking *
          *----------------*/

#ifdef Is_True_On
/* then do Error Checking */

static DST_BLOCK_IDX
DST_CHECK_BLOCK_IDX(DST_BLOCK_IDX b)
{
   Is_True((b != DST_INVALID_BLOCK_IDX && b >= 0 &&
	    b <= current_DST->last_block_header),
	      ("Illegal DST block idx"));
   return b;
}

static DST_IDX
DST_CHECK_IDX(DST_IDX i)
{
   Is_True( ((i.byte_idx >= 0 &&
       i.byte_idx < current_DST->dst_blocks[DST_CHECK_BLOCK_IDX(i.block_idx)].size)) || DST_IS_FOREIGN_OBJ(i),
      ("Illegal DST idx"));
   return i;
}

#else  /* do not do Error Checking */

#define DST_CHECK_BLOCK_IDX(b) (b)
#define DST_CHECK_IDX(i) (i)

/* Error Checking */
#endif


extern DST_TYPE
New_DST (void)
{
	INT32 i;
	DST_Type *dst = TYPE_MEM_POOL_ALLOC(DST_Type, MEM_src_pool_ptr);

	dst->last_block_header = -1;
	dst->max_block_header = NUM_BLOCK_HEADERS;
	dst->current_block_header = -1;

	/* the block_list should point to the first block
	   for each of the block kinds */
	for (i = 0; i < DST_noblock; i++) {
		dst->block_list[i] = DST_INVALID_BLOCK_IDX;
	}

	return (DST_TYPE)dst;
}


extern void
DST_Init (char *start, INT32 num_blocks)
{
	INT i;
	block_header *blocks;
	DST_BLOCK_IDX *blklist;


	if (Current_DST == NULL) 
	    Current_DST = New_DST();
	
	current_DST = (DST_Type *)Current_DST;

	if (start == NULL) {
		/* create new list of blocks */
		blocks = (block_header*) Symtab_Alloc (NUM_BLOCK_HEADERS*sizeof(block_header), TRUE);
	} else {
		/* use existing space as list of blocks */
		blocks = (block_header *) start;
		current_DST->max_block_header = num_blocks;
		current_DST->last_block_header = num_blocks - 1;

		blklist = &current_DST->block_list[0];
		FOREACH_DST_BLOCK(i) {
			/* Would be nice if global blocks came first,
			 * but that doesn't seem to be true, so search
			 * thru all blocks. */
			/* Assume that when multiple blocks are used for a
			 * kind, first block will be earliest in the array. */
			if (blklist[blocks[i].kind] == DST_INVALID_BLOCK_IDX)
				blklist[blocks[i].kind] = i;
		}
#if defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER) || defined(MONGOOSE_BE)
		blocks[current_DST->last_block_header].allocsize = 
			blocks[current_DST->last_block_header].size;
#endif
	}
	current_DST->dst_blocks = blocks;
	current_DST->current_dst = blocks;
}

static void
set_current_dst_to_current ( void )
{
  current_DST->current_block_header = current_DST->last_block_header;
  current_DST->current_dst =
	&current_DST->dst_blocks[current_DST->current_block_header];
}

/*
#if !(defined(MONGOOSE_BE)) || defined(_STANDALONE_INLINER) || defined(_SUPPORT_IPA)
*/

static block_header *
new_block (DST_BLOCK_KIND kind, INT32 size)
{
	INT32 allocsize = (size < BLOCK_SIZE ? BLOCK_SIZE : size);
	DST_BLOCK_IDX last_block = current_DST->last_block_header;
	DST_BLOCK_IDX max_block = current_DST->max_block_header;
	block_header *blocks = current_DST->dst_blocks;
	block_header *current;

	if (++last_block >= max_block) {
		blocks = TYPE_MEM_POOL_REALLOC_N(block_header,
						 MEM_src_pool_ptr ,
						 blocks, max_block,
						 2 * max_block);
		max_block *= 2;

		current_DST->dst_blocks = blocks;
		current_DST->max_block_header = max_block;
	}
	current_DST->last_block_header = last_block;
	current_DST->current_block_header = last_block;

#if defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER) || defined(MONGOOSE_BE)
	/* force Symtab_Alloc to allocate from the Src_block, not the PU_block
	 */
	blocks[last_block].offset = (char*) Symtab_Alloc(allocsize,
		TRUE);
#else
	blocks[last_block].offset = (char*) Symtab_Alloc(allocsize,
		(kind == DST_local_scope_block) ? FALSE : TRUE);
#endif
	current = &blocks[last_block];
	current->kind = kind;
	current->size = 0;
	current->allocsize = allocsize;
	current_DST->current_dst = current;

	return current;
}


extern void
DST_begin_block (DST_BLOCK_KIND block_kind)
{
	DST_BLOCK_IDX *blklist;
	current_DST = (DST_Type *)Current_DST;
	blklist = &current_DST->block_list[0];
	current_DST->current_dst = new_block (block_kind, 0);
	if (blklist[block_kind] == DST_INVALID_BLOCK_IDX)
		blklist[block_kind] = current_DST->last_block_header;
}

extern DST_IDX
DST_allocate (INT32 size, INT32 align)
{
	DST_IDX new_idx;
	INT32 total_size;
	INT32 align_mod, align_padding;
	block_header *current;
	current_DST = (DST_Type *)Current_DST;

#if defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER) || defined(MONGOOSE_BE)
	    set_current_dst_to_current();
	    current = current_DST->current_dst;
#else
	    current = current_DST->current_dst;
#endif


	/* Calculate the alignment padding in the current_block */
	align_mod = current->size % align;
	align_padding = (align_mod ? (align - align_mod) : 0);
	total_size = current->size + size + align_padding;

	if (total_size > current->allocsize) {
	/* Turn off the following if inliner/ipa is the one calling this
	 * the first time
   	 * because by this time current->allocsize is NOT the amount
	 * of space you can use anymore.  FE only reserved current->size
	 * + align_padding for this current block
	 */
		/* allocate new block */
#if defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER) || defined(MONGOOSE_BE)
		current = new_block (DST_local_scope_block, size);
#else
		current = new_block (current->kind, size);
#endif
		/* recalculate the alignment padding in the current_block */
		align_padding = 0;
		total_size = size;
	}

	new_idx.block_idx = current_DST->current_block_header;
	new_idx.byte_idx = current->size + align_padding;
	current->size = total_size;
	return new_idx;
}


/* Resets "current_block" to become the last block in the block-list
 * into which idx points.
*/
extern void
DST_return_to_block(DST_IDX idx)
{
	DST_BLOCK_IDX current_block;
	current_DST = (DST_Type *)Current_DST;
	current_block = DST_CHECK_IDX(idx).block_idx;
	current_DST->current_block_header = current_block;
	current_DST->current_dst = &current_DST->dst_blocks[current_block];
}
/*
#endif * !MONGOOSE_BE  || _STANDALONE_INLINER  || _SUPPORT_IPA */


/* Returns a pointer to the beginning of an allocated data area.  It
 * is up to the caller to ensure that access does not occur beyond the
 * end of the allocated area.  The accessed block must not yet be
 * written to file.
*/
extern char * 
DST_idx_to_string(DST_IDX idx)
{
	block_header *block;
	current_DST = (DST_Type *)Current_DST;
	block = &current_DST->dst_blocks[DST_CHECK_IDX(idx).block_idx];
	if (idx.byte_idx == DST_INVALID_BYTE_IDX)
	    return NULL;
	else
	    return &(block->offset[idx.byte_idx]);
}


static DST_IDX
DST_get_block_list(DST_BLOCK_KIND block_kind)
{
	DST_IDX idx;
	DST_BLOCK_IDX bidx = current_DST->block_list[block_kind];

	if ((bidx == DST_INVALID_BLOCK_IDX) || 
	    (current_DST->dst_blocks[DST_CHECK_BLOCK_IDX(bidx)].size <= 0)) {
		idx = DST_INVALID_IDX;
	} else {
		idx.byte_idx  = 0;   /* First byte in block */
		idx.block_idx = bidx;
		current_DST->current_block_header = bidx;
		current_DST->current_dst = &current_DST->dst_blocks[bidx];
	}
	return idx;
}


/* Returns an index to the first DST_INCLUDE_DIR entry, which may be 
 * DST_INVALID_IDX in the abscence of any DST_INCLUDE_DIR entries.
*/
extern DST_IDX
DST_get_include_dirs(void)
{
	current_DST = (DST_Type *)Current_DST;
	return DST_get_block_list(DST_include_dirs_block);
}


/* Returns an index to the first DST_FILE_NAME entry, which may be 
 * DST_INVALID_IDX in the abscence of any DST_FILE_NAME entries.
*/
extern DST_IDX
DST_get_file_names(void)
{
	current_DST = (DST_Type *)Current_DST;
	return DST_get_block_list(DST_file_names_block);
}


/* Returns an index to the first DST_MACRO_INFO entry, which may be 
 * DST_INVALID_IDX in the abscence of any DST_MACRO_INFO entries.
*/
extern DST_IDX
DST_get_macro_info(void)
{
	current_DST = (DST_Type *)Current_DST;
	return DST_get_block_list(DST_macro_info_block);
}


/* Returns an index to the DST_COMPILE_UNIT entry, which may be 
 * DST_INVALID_IDX in the abscence of any DST_COMPILE_UNIT entry.
*/
extern DST_IDX
DST_get_compile_unit(void)
{
	current_DST = (DST_Type *)Current_DST;
	return DST_get_block_list(DST_file_scope_block);
}

