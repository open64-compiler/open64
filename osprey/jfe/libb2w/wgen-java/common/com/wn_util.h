/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


/*---------------------------------------------------------------------
***                  WN walk, insert and copy routines        
***
*** Description:
***  
***  This file contains routines that operate on a given WHIRL tree
***  The operations performed on the tree include walking the tree
***  inserting a node into a block of nodes and copying a tree 
***
***
*** Reserved prefix:
*** WN_WALK    for WHIRL node walk routines
*** WN_INSERT  for WHIRL node insert routines
*** WN_COPY    for WHIRL node copy routines
*** WN_DELETE  for WHIRL node delete routines
*** WN_EXTRACT for WHIRL node extraction (but not delete) routines
*** WN_LOOP    for WHIRL node loop routines
***
***
***
*** Exported types:
***
***
***  WN_ITER
***
*** A tree node iterator. This type has one client visible field
***
***     WN *wn
***   
***       Gives the current tree node.
***
***
***
*** Exported functions:
***
***
***   WN_ITER* WN_WALK_TreeIter(
***      WN* wn)
***   
***
***   Creates and returns a tree iterator. The tree iterator iterates over
***   all nodes in the tree including structured control flow nodes,
***   statement nodes, and expression nodes
*** 
***
***   WN_ITER* WN_WALK_TreeNext(
***      WN_ITER *wni)
*** 
***
***   Walks the tree in preorder and returns the tree iterator 
***   containing the next node in the tree walk
***   
***
***
***   WN_ITER *WN_WALK_SCFIter(
***      WN_ITER *wni)
***
***   
***   Creates and returns a tree iterator. The tree iterator iterates over 
***   nodes at the structured control flow level
***
*** 
***   WN_ITER *WN_WALK_SCFNext(
***      WN_ITER *wni)
***
***   Walks the tree in preorder and returns the next node whose
***   opcode falls into the structured control flow category
***
***
***
***   WN_ITER *WN_WALK_StmtIter(
***      WN *wn)
***
***   Creates and returns a tree iterator. The tree iterator will return
***   all statement nodes and structured control flow nodes. For expressions
***   it will only return them if they are kids of structured control flow
***   nodes. 
***
***
***
***  WN_ITER *WN_WALK_StmtNext(
***     WN_ITER *wni)
***
***  Walks the tree in preorder and returns the next node, the node
***  must fall into the category described in WN_StmtIter
***
***
***
***  WN_ITER *WN_WALK_Abort(
***     WN_ITER *wni)
***
***  To exit prematurely from a tree walk a call to this routine must
***  be made. It frees the data structures used during a walk operation
***
***   
***
***  void WN_INSERT_BlockBefore(
***     WN* b, 
***     WN* wn,
***     WN* in)
***
***  To insert a node before a given node in a block node. Node "in" is
***  inserted before node "wn" in block "b". Note, wn may be NULL, in that
***  case "in" is inserted at the end. Also, "in" may be a block,
***  in which case all the statements in the block are inserted
***
***  void WN_INSERT_BlockFirst(
***	WN* b,
***	WN* in)
***
***  To insert a node before the first node in a block.  Note, "in" may be
***  a block, in which case all the statements in the block are inserted.
***
***
***  void WN_INSERT_BlockAfter(
***     WN* b, 
***     WN* wn,
***     WN* in)
***
***  To insert a node after a given node in a block node. Node "in" is
***  inserted after node "wn" in block "b".  Note, "wn" may be NULL, in that
***  case "in" is inserted at the beginning. Also, "in" may be a block,
***  in which case all the statements in the block are inserted
***
***  void WN_INSERT_BlockLast(
***	WN* b,
***	WN* in)
***
***  To insert a node after the last node in a block.  Note, "in" may be
***  a block, in which case all the statements in the block are inserted.
***
***
***
***  WN* WN_COPY_Tree(
***    WN* wn)
***
***  To copy node wn.
***  No annotations are copied at this point. This is strictly a tree copy,
***  and a recursive one
***
***
***  WN* WN_COPY_Tree_With_Map(
***    WN* wn)
***
***  To copy node wn and its map.
***  This is strictly a tree copy, and a recursive one
***
***
***  void WN_COPY_All_Maps(
***    WN* dst,
***    WN* src)
***
***  To copy all maps from src to dst.
***
***
***  void WN_DELETE_Tree(
***     WN *tree)
***
***  Deletes a tree recursively. No annotations are deleted at this point. 
***  
***
***  WN* WN_DELETE_FromBlock(
***       WN* blck, 
***       WN* wn)
***
***  To delete node "wn" from a BLOCK node. Note, WN_DELETE_Tree is called 
***  which is a recursive delete, it deletes all the kids.
*** 
***  WN *WN_EXTRACT_FromBlock(
***       WN *parent,
***       WN *item)
***
***  Extract node "item" from a BLOCK, but don't delete it.
***
***  WN *WN_EXTRACT_ItemsFromBlock(
***       WN *parent,
***       WN *first_item,
***       WN *last_item)
***  Extract nodes between first_item and last_item from a BLOCK,
***  but don't delete them.
***
***  WN *WN_LOOP_InductionVariable(
***	  const WN *loop)
***
*** Determine the loop's induction variable.  Returns either an
*** IDNAME or LDID.  If unable to determine it, returns NULL.
***
***  WN *WN_LOOP_LowerBound(
***	  const WN *loop)
***
*** Determine the lower bound of the loop, which is basically
*** the value stored to the induction variable to initialize
*** the loop.  If unable to determine it, returns NULL.
***
***  WN *WN_LOOP_UpperBound(
***	  const WN *loop,
***       OPCODE *compare)
***
*** Determine the upper bound of the loop, which is basically
*** the value which can not be exceeded (going in either
*** direction (positive dir if increment is positive, or
*** negative direction is increment is negative
*** Also returns the comparison operator to know how the iv is
*** related to this upper bound.  We canonicalize it so the iv
*** is always on the lhs of the comparison, and the upper bound
*** is always on the rhs:  iv <,<=,==,!=,>=,> upper bound
*** If unable to determine it, returns NULL.
***
***  WN *WN_LOOP_Increment(
***	  const WN *loop,
***       BOOL *is_incr)
***
*** Determine the increment (if is_incr = TRUE) value
*** the value which can not be exceeded (going in either
*** direction (positive dir if is_incr is true, or negative
*** direction if is_incr is false (actually decrements iv)
*** If unable to determine it, returns NULL.
***
***  WN *WN_LOOP_TripCount(
***	  const WN *loop)
***
***  Determine the trip count, which may be a LDID, or INTCONST
***  If unable to determine it, returns NULL.
***
***  WN_object_ty(const WN *) 
***    Obtain the high level type of the object being accessed.
***
***  WN_hl_object_ty (const WN*, TY_IDX& ty, UINT32& fld_id)
***    Obtain the higher level type of object being accessed.
***
***  WN_object_size(const WN *)
***    Obtain the size of the object being accessed.
***
***
***
***
***
***  void Add_Pragma_To_MP_Regions (WN_VECTOR *wnv, 
***                                 WN_PRAGMA_ID pragma_id,
***                                 ST *st, WN_OFFSET ofst,
***                                 WN_MAP parent_map,
***                                 BOOL make_compiler_generated)
***     Given a vector of WHIRL-MP regions innermost to outermost
***     (i.e. the innermost MP region is first, outermost is last)
***     add a pragma of the given type for the given ST *as required* 
***     to the MP regions. The algorithm for adding pragmas 
***     currently works for LOCAL and SHARED pragmas only.
***     Also, if the parent_map is non-NULL, then update the
***     parent-map when inserting.
***
***
***------------------------------------------------------------------*/


/** $Revision: 1.1.1.1 $
*** $Date: 2005/10/21 19:00:00 $
*** $Author: marcel $
*** $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/wn_util.h,v $
**/


#ifndef wn_util_INCLUDED
#define wn_util_INCLUDED

#include "wn.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct wn_stack{
 WN** stack;
 WN** sp;
 INT size;
} WN_STACK;

typedef struct wn_iter {
  WN *wn;               /* current tree node */
  WN_STACK *stack;      /* stack used during the walk */
} WN_ITER;

/* access macro */

#define WN_STACK_stack(wns) ((wns)->stack)
#define WN_STACK_sp(wns) ((wns)->sp)
#define WN_STACK_size(wns) ((wns)->size)

#define WN_ITER_wn(wni) ((wni)->wn)
#define WN_ITER_stack(wni) ((wni)->stack)

extern WN_ITER* WN_WALK_TreeIter(
       WN*
);

extern WN_ITER* WN_WALK_TreeNext(
       WN_ITER*
);

extern WN_ITER* WN_WALK_StmtIter(
       WN*
);

extern WN_ITER* WN_WALK_StmtNext(
       WN_ITER*
);

extern WN_ITER* WN_WALK_SCFIter(
       WN *wn
);

extern WN_ITER* WN_WALK_SCFNext(
       WN_ITER*
);

extern void WN_WALK_Abort(
       WN_ITER*
);

extern void WN_INSERT_BlockBefore(
       WN* b, 
       WN* wn,
       WN* in
);

#define WN_INSERT_BlockFirst(b, in) \
    WN_INSERT_BlockBefore(b, WN_first(b), in)

extern void WN_INSERT_BlockAfter(
       WN* b, 
       WN* wn, 
       WN* in
);

#define WN_INSERT_BlockLast(b, in) \
    WN_INSERT_BlockAfter(b, WN_last(b), in)

extern WN* WN_COPY_Tree(
       WN *tree_node
);

extern WN* WN_COPY_Tree_With_Map(
       WN *tree_node
);

extern void WN_COPY_All_Maps(
       WN *dst,
       WN *src
);

extern void WN_DELETE_Tree(
       WN* tree
);

extern void WN_DELETE_FromBlock(
       WN *blck,
       WN *wn
);

WN *WN_EXTRACT_FromBlock(
       WN *parent,
       WN *item
);

WN *WN_EXTRACT_ItemsFromBlock(
	WN *parent, 
	WN *first_item, 
	WN *last_item
);

WN *WN_LOOP_LowerBound(
	const WN *loop
);

WN *WN_LOOP_UpperBound(
	const WN *loop,
	OPCODE *compare,
#ifdef KEY
        BOOL  enhanced = FALSE
#endif
);

WN *WN_LOOP_Increment(
	const WN *loop,
	BOOL *is_incr
);

WN *WN_LOOP_TripCount(
	const WN *loop,
#ifdef KEY
        BOOL  enhanced = FALSE
#endif
);

WN *WN_LOOP_InductionVariable(
	const WN *loop
);



/*  Other WN utilities */

/* Obtain the high-level type of the item accessed */
extern TY_IDX WN_object_ty(const WN *);

/* Obtain the higher level type of object being accessed. */
extern void WN_hl_object_ty (const WN*, TY_IDX& ty, UINT32& fld_id);

/* Obtain the size of object being accessed */
extern INT32 WN_object_size(const WN *);

inline BOOL WN_is_black_box(const WN *wn) 
{ 
  return OPCODE_is_black_box( WN_opcode(wn) ); 
}

#ifdef __cplusplus
} /* close extern "C" */


/* Needed for the STL vector class used below
 */
#include "vector"
#include "mempool_allocator.h"

typedef mempool_allocator<WN*> VEC_POOL_ALLOCATOR;
typedef vector<WN*, VEC_POOL_ALLOCATOR> WN_VECTOR;

typedef std::vector< bool, mempool_allocator<bool> > BOOL_VECTOR;

extern "C" void Add_Pragma_To_MP_Regions (WN_VECTOR *wnv, 
                                          WN_PRAGMA_ID pragma_id,
                                          ST *st, WN_OFFSET ofst,
                                          WN_MAP parent_map,
                                          BOOL make_compiler_generated);

#endif /* __cplusplus */

#endif /* wn_util_INCLUDED */
