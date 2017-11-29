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

/* --------------------------------------------------------------
 *
 * Block stack to maintain DO/IF/PDO etc blocks.
 * A list of statements to be deferred to the end of the block
 * may be maintained - these are added when the block is popped.
 * Similarly statements may be appended to the enclosing BLOCK 
 * when the block is popped.
 *
 * A block can be a WN REGION (mp DO etc), or some form of
 * control flow structure
 *
 */

typedef struct {
   WN *wn;		/* may be a block or a region */
   union {
    struct {
      WN *deferred;	/* deferred statements for end of block */
      WN *append;	/* deferred statements to append after block */
    } block;
    WN *region_parent;   /* block holding this region */
   } u;

   UINT32 is_parallel_do:1 ; /* this loop is a parallel loop (may be a pseudo nested one */
   UINT32  is_top_pdo:1    ; /* this loop is the top DO of a C$PAR PDO */
			
} block_stack_t;

static block_stack_t *block_stack;
static INT32 block_stack_size = 0;
static INT32 block_stack_top = -1;
#define BLOCK_CHUNK_SIZE 20


/* WN blocks */

static WN * WN_block         ;     /* Current OPC_BLOCK to tack WNs onto      */
static WN * WN_decl_block    ;     /* statements created at declaration time, */
			           /* to be placed first into PU block        */
static WN * WN_copyin_block  ;     /* statements created at declaration time, */
			           /* to be placed last in the preamble block */
static WN * WN_defer_block   ;     /* Block of statements to add at end of    */
                                   /* current statement. By call to fei_stmt. */
static WN * WN_top_of_loop_block ; /* MP directives added to top of DO loop   */


static BOOL cwh_block_add_debug_line ; /* in routine's preamble = FALSE    */

/* forward references */

static void cwh_block_bump(void) ;
static WN** cwh_block_find_address(enum block_id id);

#define cwh_block_current_inline() WN_block


