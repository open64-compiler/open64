/*
 * Copyright 2005-2007 NVIDIA Corporation.  All rights reserved.
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


#ifndef dwarf_DST_mem_INCLUDED
#define dwarf_DST_mem_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif



#ifdef _KEEP_RCS_ID
static char *dwarf_DST_mem_rcs_id = "$Source: /scratch/mee/Patch0002-taketwo/kpro64-pending/common/com/SCCS/s.dwarf_DST_mem.h $ $Revision: 1.2 $";
#endif /* _KEEP_RCS_ID */

#include "defs.h"

         /*-------------------------------------------------*
          * Definition of DST_IDX (index to allocated data) *
          *-------------------------------------------------*/


typedef mINT32 DST_BYTE_IDX;
typedef mINT32 DST_BLOCK_IDX;


typedef struct DST_idx
{
   DST_BYTE_IDX  byte_idx;
   DST_BLOCK_IDX block_idx;
} DST_IDX;

#define DST_INVALID_BLOCK_IDX -1
#define DST_INVALID_BYTE_IDX -1
#define DST_FOREIGN_BLOCK_IDX -2
#define DST_FOREIGN_BYTE_IDX -2


/* Represents a NULL DST_IDX value. Always initialize a declared
 * DST_IDX with DST_INVALID_IDX or DST_INVALID_INIT!!
*/
extern const DST_IDX DST_INVALID_IDX;
#define DST_INVALID_INIT {DST_INVALID_BYTE_IDX, DST_INVALID_BLOCK_IDX}
#define DST_FOREIGN_INIT {DST_FOREIGN_BYTE_IDX, DST_FOREIGN_BLOCK_IDX}


/* Use this macro to test for a NULL DST_IDX value
*/
#define DST_IS_NULL(i) (((i).byte_idx == DST_INVALID_BYTE_IDX) ||\
			((i).block_idx == DST_INVALID_BLOCK_IDX))

/* Use this macro to test for a FOREIGN DST_IDX value */
#define DST_IS_FOREIGN_OBJ(i)	(((i).byte_idx == DST_FOREIGN_BYTE_IDX) || \
				 ((i).block_idx == DST_FOREIGN_BLOCK_IDX))


#ifdef __cplusplus

// == != operators can conflict with windows.h,
// so just define an explicit equality routine.
inline BOOL DST_ARE_EQUAL (const DST_IDX x, const DST_IDX y) {
  return x.byte_idx == y.byte_idx && x.block_idx == y.block_idx;
}

inline bool operator < (const DST_IDX& x, const DST_IDX& y) {
  return (x.block_idx < y.block_idx) ||
         (x.block_idx == y.block_idx && x.byte_idx < y.byte_idx);
}

inline DST_IDX make_DST_IDX(DST_BYTE_IDX byte_idx, DST_BLOCK_IDX block_idx) {
  DST_idx result;
  result.byte_idx = byte_idx;
  result.block_idx = block_idx;
  return result;
}

// Hash function for DST indices.
struct DST_IDX_hash {
  size_t operator()(DST_IDX idx) const {
    return (static_cast<size_t>(idx.block_idx) << 10) +
           static_cast<size_t>(idx.byte_idx);
  }
};

#endif

         /*----------------------------*
          * General Purpose Functions  *
          *----------------------------*/


/* Converts an index to a char pointer
*/
extern char * DST_idx_to_string(DST_IDX);

         /*-----------------------*
          * General purpose types *
          *-----------------------*/


/* The different kinds of a block of bytes.  WARNING: Any changes to
 * this structure must result in similar changes in the .c file.  The
 * order of the constants is paramount for the implementation in
 * dwarf_DST_mem.c.
*/
typedef enum DST_block_kind
{
   DST_include_dirs_block = 0,  /* For include directory path names */
   DST_file_names_block   = 1,  /* For file names */
   DST_macro_info_block   = 2,  /* For macro information */
   DST_file_scope_block   = 3,  /* For file-scope symbols */
   DST_local_scope_block  = 4,  /* For local-scope symbols */
   DST_noblock            = 5   /* Invalid block */
} DST_BLOCK_KIND;

/* DST_TYPE is a dummy pointer type to be used where USE_DST_INTERNALS
 * is not defined */
typedef void* DST_TYPE;
extern DST_TYPE Current_DST;

#ifdef USE_DST_INTERNALS

typedef struct block_header_struct {
	DST_BLOCK_KIND kind;
	mINT32 size;
	mINT32 allocsize;
	char *offset;
} block_header;

typedef struct dst_rec {
	block_header *dst_blocks;	/* array of block headers */
	block_header *current_dst;	/* the current dst header */
	DST_BLOCK_IDX last_block_header;
	DST_BLOCK_IDX max_block_header;
	DST_BLOCK_IDX current_block_header;
	DST_BLOCK_IDX block_list[DST_noblock];
} DST_Type;

#define FOREACH_DST_BLOCK(i) \
	for (i = 0; i <= ((DST_Type *)Current_DST)->last_block_header; i++)

#endif /* USE_DST_INTERNALS */


/* Create a new DST.  For backward-compatibility this is called automatically
 * from DST_Init when Current_DST is NULL.  For IPA, this function must be
 * called explicitly for each input file.
*/
extern DST_TYPE
New_DST (void);

/* Initialize the DST memory system.  If start == NULL, then initializes
 * with no blocks allocated.  Else it initializes with num_blocks allocated,
 * starting at the start address.
*/
extern void 
DST_Init (char *start, INT32 num_blocks);

/* Allocates the first block of a new block-list and makes it the current
 * block for DST_allocate().  This function can AT MOST be called once for
 * each of: DST_include_dirs_blocks, DST_file_names_blocks, 
 * DST_macro_info_block, and DST_file_scope_blocks.  It should be called
 * for each Program Unit (PU), such that the entries belonging to a PU 
 * will be in a separate block-list from the entries belonging to other 
 * PUs.  Never call this function with DST_noblock!
*/
extern void 
DST_begin_block(DST_BLOCK_KIND block_kind);


/* Resets the current block to become the last block in the block-list
 * into which idx points.
*/
extern void 
DST_return_to_block(DST_IDX idx);


/* Allocates the given number of bytes in the current memory block,
 * aligned according to the given align.  Will add a new block to
 * the block list, when there is not enough space left in the current 
 * block.  DST_begin_block() must have been called at least once.  
 * Returns an index to the newly allocated memory area.
 */
extern DST_IDX 
DST_allocate(INT32 bytes, INT32 align);


/* Returns an index to the first DST_INCLUDE_DIR entry, which may be 
 * DST_INVALID_IDX in the abscence of any DST_INCLUDE_DIR entries.
*/
extern DST_IDX
DST_get_include_dirs(void);


/* Returns an index to the first DST_FILE_NAME entry, which may be 
 * DST_INVALID_IDX in the abscence of any DST_FILE_NAME entries.
*/
extern DST_IDX
DST_get_file_names(void);


/* Returns an index to the first DST_MACRO_INFO entry, which may be 
 * DST_INVALID_IDX in the abscence of any DST_MACRO_INFO entries.
*/
extern DST_IDX
DST_get_macro_info(void);


/* Returns an index to the DST_COMPILE_UNIT entry, which may be 
 * DST_INVALID_IDX in the abscence of any DST_COMPILE_UNIT entry.
*/
extern DST_IDX
DST_get_compile_unit(void);

#ifdef __cplusplus
}
#endif
#endif /* dwarf_DST_mem_INCLUDED */
