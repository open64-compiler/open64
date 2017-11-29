/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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
 * Module: cwh_block
 * $Revision: 1.7 $
 * $Date: 05/09/22 10:54:46-07:00 $
 * $Author: gautam@jacinth.keyresearch $
 *
 * Revision history:
 *  dd-mmm-95 - Original Version
 *
 * Description: contains routines to maintain 
 *
 *              a) a block stack for control flow and parallel regions
 *              b) special WN_Blocks used for pragmas, entry preamble
 *                 loops etc. These are filled in at odd times, then
 *                 added to the main WN_tree when convenient.
 *              
 * ====================================================================
 * ====================================================================
 */

static const char *source_file = __FILE__;

/* sgi includes */

#include "defs.h"
#include "glob.h"  
#include "symtab.h"
#include "errors.h"
#include "wn.h"
#include "wn_util.h"
#include "ir_reader.h"

/* conversion includes */

#include "cwh_defines.h"
#include "cwh_stmt.h"
#include "cwh_directive.h"
#include "cwh_block.h"
#include "cwh_block.i"

/*==============================================
 * 
 * cwh_block_push_block
 *
 * push the current block onto the block stack.
 * 
 *===============================================
 */
extern void 
cwh_block_push_block(WN *deferred, WN *append, BOOL is_top_pdo)
{
  cwh_block_bump();   

  block_stack[block_stack_top].wn = cwh_block_current_inline();
  block_stack[block_stack_top].u.block.deferred = deferred;
  block_stack[block_stack_top].u.block.append = append;

  if (parallel_do_count) {
    block_stack[block_stack_top].is_parallel_do = TRUE;
    parallel_do_count--;

  } else {
    block_stack[block_stack_top].is_parallel_do = FALSE;
  }

  block_stack[block_stack_top].is_top_pdo = is_top_pdo;
}

/*==============================================
 * 
 * cwh_block_pop_block
 *
 * pop the region/do/block on top of the block stack
 *
 * Appends any deferred statements to the current block.
 * Makes TOS current block, and appends any thing
 * that's on the append list.
 *
 *===============================================
 */
extern void 
cwh_block_pop_block(void)
{
   WN *block, *append;
   BOOL is_parallel_do, is_top_pdo;

   DevAssert((block_stack_top >= 0),("block stack underflow"));

   block = block_stack[block_stack_top].wn;
   is_parallel_do = block_stack[block_stack_top].is_parallel_do;
   is_top_pdo = block_stack[block_stack_top].is_top_pdo;

   DevAssert((WN_opcode(block)!=OPC_REGION),("stack mismatch, expected BLOCK"));

   if (block_stack[block_stack_top].u.block.deferred) {
      cwh_block_append(block_stack[block_stack_top].u.block.deferred);
   }

   append = block_stack[block_stack_top].u.block.append;
   cwh_block_set_current(block);
   --block_stack_top;

   if (append) 
     cwh_block_append(append);

   /* is implicit end of parallel loop region ?  */
   /* don't do this for outer-most loop of PDO   */
   /* because the endpdo will pop off the region */

   if (is_parallel_do && !is_top_pdo) 
     cwh_block_pop_region();
}

/*==============================================
 * 
 * cwh_block_pop_region
 *
 * pop the MP region on top of the block stack.
 * 
 *===============================================
 */
extern WN *
cwh_block_pop_region(void)
{
   WN *region, *block;
   DevAssert((block_stack_top >= 0),("region stack underflow"));

   region = block_stack[block_stack_top].wn;
   block  = block_stack[block_stack_top].u.region_parent;

   DevAssert((WN_opcode(region)==OPC_REGION),("stack mismatch, expected REGION"));

   --block_stack_top;

   /* the current block is now set to the parent block of the region */

   cwh_block_set_current(block);
   return region;
}

/*==============================================
 * 
 * cwh_block_set_region
 *
 * set the current block to the nearest enclosing 
 * region's pragma block. 
 * 
 * Return the current block mostly, but
 * oometimes we want to check the block stack
 * but don't mind if there isn't an enclosing
 * region. If so, then return NULL.
 *
 *===============================================
 */
extern WN *
cwh_block_set_region_pragmas(BOOL assert)
{
  int i;
  WN *wn;
  for (i=block_stack_top; i>=0; i--) {
    wn = block_stack[i].wn;
    if (WN_operator(wn)==OPR_REGION) {
      return (cwh_block_exchange_current(WN_region_pragmas(wn)));
    }
  }

  if (assert) {
    DevAssert((0),("no enclosing region"));
    return cwh_block_current_inline() ;
  } 
  return NULL;
}

/*==============================================
 * 
 * cwh_block_add_to_enclosing_regions
 *
 * add the given pragma to pragma blocks 
 * of all regions on the block stack.
 * 
 *===============================================
 */
extern void
cwh_block_add_to_enclosing_regions(WN_PRAGMA_ID id, ST * st)
{
  int i;

  WN_VECTOR regions ;

  DevAssert((id == WN_PRAGMA_LOCAL), ("only local required just now"));

  for (i = block_stack_top; i>=0 ;  i--) {
    WN * wn = block_stack[i].wn;

    if (WN_operator(wn)==OPR_REGION) {
      if (WN_region_kind(wn) == REGION_KIND_MP)
	regions.insert(regions.end(),wn);
    }
  }

  Add_Pragma_To_MP_Regions (&regions, 
			    id,
			    st,
			    0,
			    0,
			    FALSE);
}

/*==============================================
 * 
 * cwh_block_push_region
 *
 * push the MP region on top of the block stack.
 * 
 *===============================================
 */
extern void
cwh_block_push_region(WN *region)
{
   DevAssert((WN_opcode(region)==OPC_REGION),(" not region")) ;

   cwh_block_bump();

   block_stack[block_stack_top].wn = region;
   block_stack[block_stack_top].u.region_parent = cwh_block_current_inline();
}

/*==============================================
 * 
 * cwh_block_dump()
 *
 * dumps the block stack to stdout
 * 
 *===============================================
 */
extern void
cwh_block_dump(void)
{
  WN * wn ;
  int i   ;

  printf ("\n Block Stack --- \n\n");

  for (i = block_stack_top ; i >= 0 ; i-- ) {

    wn = block_stack[i].wn ;

    dump_wn(wn);

    if (WN_opcode(wn) == OPC_REGION) {

      printf ("      parent %p \n",
	      block_stack[i].u.region_parent);
    
    } else {

      printf ("      deferred %p, append %p, pdo %d, top pdo %d \n",
	      block_stack[i].u.block.deferred,
	      block_stack[i].u.block.append,
	      block_stack[i].is_parallel_do,
	      block_stack[i].is_top_pdo);
    }
  }
}

/*==============================================
 * 
 * cwh_block_bump
 *
 * bump the block stack along
 * 
 * Allocate in chunks of 20 
 *
 *===============================================
 */
static void
cwh_block_bump(void)
{
   ++block_stack_top;

   if (block_stack_size == 0) {

      block_stack = (block_stack_t *) malloc(BLOCK_CHUNK_SIZE * sizeof(block_stack_t));
      block_stack_size = BLOCK_CHUNK_SIZE;

   } else if (block_stack_top >= block_stack_size) {

      block_stack_size += BLOCK_CHUNK_SIZE;
      block_stack = (block_stack_t *) realloc(block_stack, block_stack_size * sizeof(block_stack_t));
   }
}

/*==============================================
 * 
 * cwh_block_current
 *
 * get the current block
 * 
 *===============================================
 */
extern WN *
cwh_block_current(void)
{
  return WN_block;
}

/*===============================================
 *
 * cwh_block_set_current
 *
 * Set current block, ie: default location to add
 * statements,to blk
 *
 *===============================================
 */ 
extern void
cwh_block_set_current(WN * blk )
{
  WN_block = blk;
}
/*===============================================
 *
 * cwh_block_new_and_current
 *
 * Create a new block and set it up as the
 * current block. Return the block that was 
 * current;
 *
 *===============================================
 */ 
extern WN *
cwh_block_new_and_current(void)
{
  WN * wn ; 

  wn = cwh_block_current_inline();
  cwh_block_set_current(WN_CreateBlock()) ;

  return (wn);
}

/*===============================================
 *
 * cwh_block_exchange_current
 *
 * Set current block to blk and return the old value
 *
 *===============================================
 */ 
extern WN *
cwh_block_exchange_current(WN * blk )
{
  WN * rtrn = cwh_block_current_inline();
  cwh_block_set_current(blk);
  return rtrn;
}

/*===============================================
 *
 * cwh_block_append
 *
 * Append the WN argument to the current block node,
 * by default, or anoth block if given
 *
 *===============================================
 */ 
extern void
cwh_block_append(WN *wn)
{
  cwh_block_append_given_block(wn,cwh_block_current_inline());
}

/*===============================================
 *
 * cwh_block_append_given_block
 *
 * Append the WN argument to the given block node,
 *
 *===============================================
 */ 
extern void
cwh_block_append_given_block(WN *wn, WN* block)
{
  if (WN_opcode(wn) != OPC_BLOCK) {
    if (cwh_block_add_debug_line) 
      WN_Set_Linenum (wn, USRCPOS_srcpos(current_srcpos) );
    else
      WN_Set_Linenum (wn, 0);
  }

  WN_INSERT_BlockLast(block,wn)   ;
}

/*===============================================
 *
 * cwh_block_insert_after
 *
 * Insert a node after a given node within the current block.
 *
 *===============================================
 */ 
extern void
cwh_block_insert_after(WN *wn, WN*in)
{

  if (cwh_block_add_debug_line) 
    WN_Set_Linenum (in, USRCPOS_srcpos(current_srcpos) );
  else
    WN_Set_Linenum (in, 0);


  WN_INSERT_BlockAfter(cwh_block_current_inline(), wn, in);
}


/*===============================================
 *
 * cwh_block_append_given_id
 *
 * Append the WN to the block given. If the block
 * is NULL, it's created. Several such blocks
 * are available either for declarations just
 * before or after the preamble, or to defer
 * statements after a block.
 *
 *===============================================
 */ 
extern void
cwh_block_append_given_id(WN* wn, enum block_id id, BOOL first)
{
  WN ** block ;

  block = cwh_block_find_address(id) ;

  if (!(*block)) 
    *block = WN_CreateBlock();


  if (first)
    WN_INSERT_BlockFirst(*block,wn);
  else
    WN_INSERT_BlockLast(*block,wn);
}

/*===============================================
 *
 * cwh_block_append_given
 *
 * Append given block to the current block.
 * Clears the block pointer associated with flag
 *
 *===============================================
 */ 
extern void
cwh_block_append_given(enum block_id id)
{
  WN ** block ;

  block = cwh_block_find_address(id) ;

  if (*block != NULL) {
    cwh_block_append(*block);
    *block = NULL;
  }
}

/*===============================================
 *
 * cwh_block_find_address
 *
 * Get the address of the pointer to the WN_block
 * given the ID.
 *
 *===============================================
 */ 
static WN **
cwh_block_find_address(enum block_id id)
{
#ifdef KEY /* Bug 10177 */
  WN ** block = 0;
#else /* KEY Bug 10177 */
  WN ** block ;
#endif /* KEY Bug 10177 */

  switch (id) {
  case Defer_Block:
    block = &WN_defer_block; 
    break;

  case Preamble_Block:
    block = &WN_copyin_block; 
    break;

  case First_Block:
    block = &WN_decl_block;
    break;

  case Top_of_Loop_Block:
    block = &WN_top_of_loop_block;
    break;

  default:
    DevAssert((0),("odd block"));
  }
  return block;
}

/*===============================================
 *
 * cwh_block_init
 *
 * Initalize block varbls.
 *
 *===============================================
 */ 
extern void
cwh_block_init_pu()
{
  WN_defer_block  = NULL;
  WN_decl_block   = NULL;
  WN_copyin_block = NULL;
  WN_top_of_loop_block = NULL;
  cwh_block_add_debug_line = FALSE;

}

/*===============================================
 *
 * cwh_block_toggle_debug
 *
 * toggle the debug flag which puts out WN src lines.
 * returns old setting.
 *
 *===============================================
 */ 
extern BOOL
cwh_block_toggle_debug(BOOL debug)
{
  BOOL old =   cwh_block_add_debug_line;
  cwh_block_add_debug_line = debug;
  return old;
}
