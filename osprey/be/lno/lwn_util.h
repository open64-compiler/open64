/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

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


//-*-c++-*-
/*---------------------------------------------------------------------------
 *                  WN walk, insert and copy routines        
 *
 * Description:
 *  This file contains routines that operate on a given WHIRL tree
 *  The operations performed on the tree include walking the tree
 *  inserting a node into a block of nodes and copying a tree 
 *
 * Exported types:
 *
 * LWN_ITER
 *
 *  A tree node iterator.
 *
 *    WN *wn
 *
 *	The current tree node
 *
 * Exported functions:
 *
 *   LWN_ITER* LWN_WALK_TreeIter(
 *      WN* wn)
 *   
 *
 *   Creates and returns a tree iterator. The tree iterator iterates over
 *   all nodes in the tree including structured control flow nodes,
 *   statement nodes, and expression nodes
 * 
 *
 *   LWN_ITER* LWN_WALK_TreeNext(
 *      LWN_ITER *wni)
 * 
 *
 *   Walks the tree in preorder and returns the tree iterator 
 *   containing the next node in the tree walk
 *   
 *
 *
 *   LWN_ITER *LWN_WALK_SCFIter(
 *      WN *wn)
 *
 *   
 *   Creates and returns a tree iterator. The tree iterator iterates over 
 *   nodes at the structured control flow level
 *
 * 
 *   LWN_ITER *LWN_WALK_SCFNext(
 *      LWN_ITER *wni)
 *
 *   Walks the tree in preorder and returns the next node whose
 *   opcode falls into the structured control flow category
 *
 *
 *
 *   LWN_ITER *LWN_WALK_StmtIter(
 *      WN *wn)
 *
 *   Creates and returns a tree iterator. The tree iterator will return
 *   all statement nodes and structured control flow nodes. For expressions
 *   it will only return them if they are kids of structured control flow
 *   nodes. 
 *
 *
 *
 *  LWN_ITER *LWN_WALK_StmtNext(
 *     LWN_ITER *wni)
 *
 *  Walks the tree in preorder and returns the next node, the node
 *  must fall into the category described in LWN_StmtIter
 *
 *
 *
 *  LWN_ITER *LWN_WALK_Abort(
 *     LWN_ITER *wni)
 *
 *  To exit prematurely from a tree walk a call to this routine must
 *  be made. It frees the data structures used during a walk operation
 *
 *   
 * extern WN* LWN_Get_Next_Tree_Node (const WN*);
 *   Walks the tree and returns the next tree node in in-order
 *   traversal 
 *
 * extern WN* LWN_Get_Next_Stmt_Node (const WN*);
 *   Walks the tree, and returns the next stmt node.
 *
 * extern WN* LWN_Get_Next_SCF_Node (const WN*);
 *   Walks the tree, and returns the next control flow node.
 *
 * extern void LWN_Insert_Block_Before(
 *       WN* block,
 *       WN* wn,
 *       WN* in
 * );
 *   Inserts the node 'in' before the node 'wn', assuming that 'wn'
 *   is the child of a BLOCK node.  If 'wn' is NULL, inserts at end
 *   of list.
 *
 * extern void LWN_Insert_Block_After(
 *     WN* block,
 *     WN* wn,
 *     WN* in);
 *   Insert the node 'in' after the node 'wn', assuming that 'wn'
 *   is the child of a BLOCK node.  If 'wn' is NULL, inserts at
 *   beginning of list.
 * 
 * extern void LWN_Delete_From_Block(
 *     WN *block, 
 *     WN* wn, 
 *     );
 *   Delete the node 'wn' assuming that 'wn' is the child of a BLOCK
 *   (block) node.
 * 
 * extern void LWN_Delete_Tree_From_Block(WN* wn)
 *   Delete the tree 'wn' assuming that 'wn' is the child of a BLOCK node.
 *
 *  WN* LWN_Copy_Tree(
 *    WN* wn, 
 *    BOOL copy_access=FALSE,
 *    WN_MAP access_map=0,
 *    BOOL copy_version = FALSE,
 *    WN_MAP version_map=0,
 *    BOOL copy_all_nodes=FALSE)
 *
 *   Copy node wn and below. 
 *   Copies the parent annotations. This is strictly a tree copy.
 *   If copy_access=TRUE, also copies the access arrays
 *
 *  WN * LWN_Int_Type_Conversion( 
 *    WN *wn, 
 *    TYPE_ID to_type )
 *
 *    Return the integer-typed wn converted to the given
 *    integer to_type. (handles only int->int conversions)
 *
 *  TYPE_ID Promote_Type(TYPE_ID mtype)
 *
 *    Promote the less than 4 byte types to 4 bytes
 *
 * BOOL Is_Descendent(WN *low, WN *high)
 *
 *   Is low a descendent of high
 *
 * extern void LWN_Copy_Def_Use(WN* from_exp, WN* to_exp, DU_MANAGER*);
 *
 *   Copies the def/use information in from to the corresponding nodes
 *   in to.  This is special case code only where both are expressions.
 *
 * extern void LWN_Parentize (WN* wn);
 *   Given a tree, walk it and set all the parent pointers.
 *   Note: the pointer for the node wn itself is NOT changed, but only for
 *      its kids and below.
 *
 * BOOL LWN_Check_Parentize (WN* wn);
 *   Given a tree, walk it and check all the parent pointers.
 *
 * extern WN* LWN_Get_Parent (const WN* wn);
 *   Return the parent pointer of node 'wn'.
 *
 * BOOL inside_parallelizable_loop( WN *wn );
 *   Return TRUE iff wn is inside a loop marked Parallizable.
 *
 * extern void LWN_Set_Parent (WN* wn, const WN* pwn);
 *   Set the parent pointer of node 'wn' to be 'pwn'.
 *   This is needed to, say, NULL the parent pointer for the root of
 *   a tree.
 *
 * extern WN* LWN_Get_Statement(WN *wn)
 *  Get the wn for the statement (or control flow node)
 *  surrounding this wn (return wn if
 *  wn is a statement)
 *
 * extern void LWN_Delete_Tree(WN *wn);
 *   Delete the node and all its descendants.
 *
 * extern void LWN_Delete_SR(WN *wn);
 *  Delete CALL_INFO nodes' scalar references associated with wn
 *
 * extern void LWN_Delete_DU(WN *wn)
 *  Delete the DU chain associated with wn
 *
 * extern void LWN_Delete_LNO_dep_graph(WN *wn)
 *  Delete the edges and vertices associated with this WN's array dep graph
 *
 * extern void LWN_Delete_CG_dep_graph(WN *wn)
 *  Delete the edges and vertices associated with this WN's CG dependence graph
 *
 * void LWN_Delete_Name_Manager(WN *wn)
 *   Remove a wn from the name manager

 *
 * extern void LWN_Update_Def_Use_Delete_Tree(WN *wn, DU_MANAGER* = NULL)
 *   Update the def-use chain in preperation for deleting the tree rooted
 *   at wn.  If the optional parameter is not supplied or NULL, use Du_Mgr.
 *
 * extern void LWN_Update_Dg_Delete_Tree(WN *wn, ARRAY_DIRECTED_GRAPH16*)
 *
 *   Update the dependence graph in prepratation for deleting the tree rooted
 *   at wn.
 *
 * extern WN *LWN_Extract_From_Block(WN *item);
 * extern WN *LWN_Extract_From_Block(WN* parent, WN *item);
 *   Takes item from the block that its on, removes it, and returns it.
 *   Also set's item's parent to NULL.  The second version also checks
 *   that the parent is the correct one.
 *
 * extern WN *LWN_CreateStid(OPCODE opc, WN *orig_op, WN *value)
 *   Create an Stid to the same location as orig_op.  While you can also
 *   use the mimicking LWN_CreateStid, using this version will give you
 *   more accurate alias information.
 *  
 * extern WN *LWN_CreateLdid(OPCODE opc, WN *orig_op)
 *   Create an Stid to the same location as orig_op. 
 *
 * extern WN *LWN_CreateDivceil(TYPE_ID type, WN *kid0, WN *kid1);
 *   Return the intrinsic op for divceil(kid0, kid1)
 *
 * extern WN *LWN_CreateDivfloor(TYPE_ID type, WN *kid0, WN *kid1);
 *   Return the intrinsic op for divfloor(kid0, kid1)
 *
 * WN* LWN_Create...
 *   These are identical with the WN_Create routines in wn.h, except that
 *   they only exists for those WN_Create that are passed children, and for
 *   those non NULL children passed, the parent is set to be the created WN.
 *
 * void Copy_Linenumber(const WN* from, WN* to)
 *
 *   Copy the line annotation from the from wn to the to wn.
 *
 *
 * extern BOOL Tree_Equiv (WN *wn1, WN* wn2);
 *
 *      Return true if the sub-trees wn1 and wn2 and "equiv"
 *      (the equiv notion is similar to a recursive WN_Equiv).
 * 
 * extern WN* LWN_Simplify_Tree(WN* wn)
 * 
 *    	Simplify the tree rooted at 'wn'. 
 *
 *  extern WN *LWN_Loop_Trip_Count(const WN *loop);
 *
 *	Return a trip count expression.  If unable to determine
 *      it, returns NULL. 
 *
 * inline void LWN_Set_Frequency(const WN* wn, const INT32 count);
 *    
 *      Set the frequency to count if the PU has feedback
 *
 * inline void LWN_Copy_Frequency(const WN* wn, const WN* wn_from);
 *    
 *      Set the frequency to that of wn_from if the PU has feedback
 *
 * extern void LWN_Set_Frequency_Tree(const WN* wn, const INT32 count);
 *
 *      Set the frequency of the tree rooted at wn to count
 *
 * extern void LWN_Copy_Frequency_Tree(const WN* wn, const WN *wn_from);
 *
 *      Set the frequency of the tree rooted at wn to that of wn_from
 *
 * extern void LWN_Adjust_Frequency_Tree(const WN* wn, const INT32 count);
 * 
 *      Increase the frequency of wn tree by count (can be negative) amount.
 *
 * extern void LWN_Scale_Frequency_Tree(const WN* wn, const float ratio);
 * 
 *      Scale the frequency of wn tree by ratio.
 *
 * extern void LWN_Scale_Frequency(const WN* wn, const float ratio);
 * 
 *      Scale the frequency of wn by ratio.
 *
 * extern BOOL Inside_Loop_With_Goto(WN *wn);
 *
 *	Is wn inside any loops that contain gotos
 *
 */

#ifndef lwn_util_INCLUDED
#define lwn_util_INCLUDED

#ifndef wn_INCLUDED
#include "wn.h"
#endif
#ifndef wn_util_INCLUDED
#include "wn_util.h"
#endif

#include "fb_whirl.h"
#include "cxx_hash.h"

extern WN_MAP Parent_Map;
#ifdef LNO
extern HASH_TABLE<WN*, BOOL> *Deleted_Loop_Map;
#endif

#define LWN_Get_Parent(wn)	((WN*)WN_MAP_Get(Parent_Map, (WN*)wn))
#define LWN_Set_Parent(wn, p)	(WN_MAP_Set(Parent_Map, wn, (void *)p))
inline BOOL  LWN_Is_Ancestor(const WN* wn, WN* ancestor)
{
  while( wn && wn != ancestor)
    wn = LWN_Get_Parent(wn);
  return wn == ancestor;
}

inline void LWN_Copy_Linenumber(const WN* from, WN* to)
{
  if (OPCODE_has_next_prev(WN_opcode(from)) &&
      OPCODE_has_next_prev(WN_opcode(to))) {
    WN_linenum(to) = WN_linenum(from);
  }
}

#ifdef _USE_OLD_FEEDBACK
inline void LWN_Set_Frequency(const WN* wn, const INT32 count)
{
  if (Cur_PU_Feedback && WN_opcode(wn) != OPC_BLOCK) 
    WN_MAP32_Set(WN_MAP_FEEDBACK, (WN *) wn, count);
}

inline void LWN_Copy_Frequency(const WN* wn, const WN* wn_from)
{
  if (Cur_PU_Feedback && WN_opcode(wn) != OPC_BLOCK) 
    WN_MAP32_Set(WN_MAP_FEEDBACK, (WN *) wn, WN_MAP32_Get(WN_MAP_FEEDBACK, wn_from));
}

inline void LWN_Scale_Frequency(const WN* wn, const float ratio)
{
  if (Cur_PU_Feedback && WN_opcode(wn) != OPC_BLOCK) 
    WN_MAP32_Set(WN_MAP_FEEDBACK, (WN *) wn, (INT32) ratio*WN_MAP32_Get(WN_MAP_FEEDBACK, wn));
}

inline void LWN_Adjust_Frequency(const WN* wn, const INT32 count)
{
  if (Cur_PU_Feedback && WN_opcode(wn) != OPC_BLOCK) 
    WN_MAP32_Set(WN_MAP_FEEDBACK, (WN *) wn, WN_MAP32_Get(WN_MAP_FEEDBACK, wn) - count);
}

extern void LWN_Set_Frequency_Tree(const WN* wn, const INT32 count);
extern void LWN_Copy_Frequency_Tree(const WN* wn, const WN *wn_from);
extern void LWN_Adjust_Frequency_Tree(const WN* wn, const INT32 count);
extern void LWN_Scale_Frequency_Tree(const WN* wn, const float ratio);
    
#else

/* to be implemented */
inline void LWN_Set_Frequency(const WN*, const INT32) {}
inline void LWN_Copy_Frequency(const WN*, const WN*) {}
inline void LWN_Scale_Frequency(const WN*, const float) {}
inline void LWN_Adjust_Frequency(const WN*, const INT32) {}

extern void LWN_Set_Frequency_Tree(const WN*, const INT32);
extern void LWN_Copy_Frequency_Tree(const WN*, const WN *);
extern void LWN_Adjust_Frequency_Tree(const WN*, const INT32);
extern void LWN_Scale_Frequency_Tree(const WN*, const float);

#endif // _USE_OLD_FEEDBACK


/* Mimic the utilities in wn_util.h */
#define LWN_ITER 		WN_ITER
#define LWN_WALK_TreeIter 	WN_WALK_TreeIter
#define LWN_WALK_TreeNext 	WN_WALK_TreeNext
#define LWN_WALK_SCFIter 	WN_WALK_SCFIter
#define LWN_WALK_SCFNext 	WN_WALK_SCFNext
#define LWN_WALK_StmtIter 	WN_WALK_StmtIter
#define LWN_WALK_StmtNext 	WN_WALK_StmtNext
#define LWN_WALK_Abort 		WN_WALK_Abort

extern WN* LWN_Get_Next_Tree_Node (const WN*);
extern WN* LWN_Get_Statement(WN *wn);

extern WN* LWN_Get_Next_Stmt_Node (const WN*);

extern WN* LWN_Get_Next_SCF_Node (const WN*);

extern void LWN_Insert_Block_Before(
       WN* block,
       WN* wn,
       WN* in
);

extern void LWN_Insert_Block_After(
       WN* block,
       WN* wn, 
       WN* in
);

extern void LWN_Delete_From_Block
  (WN *block, WN* wn);

extern void LWN_Delete_Tree_From_Block (WN* wn);


extern WN* LWN_Extract_From_Block(WN* item);
extern WN* LWN_Extract_From_Block(WN* parent, WN* item);

extern WN* LWN_Copy_Tree(
       WN *wn, 
       BOOL copy_access=FALSE,
       WN_MAP access_map=0,
       BOOL copy_version=FALSE,
       WN_MAP version_map=0,
       BOOL copy_all_nodes=FALSE
);

extern void LWN_Parentize (WN* wn);

extern BOOL LWN_Check_Parentize (const WN* wn);

extern BOOL inside_parallelizable_loop( WN *wn );

extern void LWN_Delete_Tree(WN *wn);

#ifdef LNO
class DU_MANAGER;
class ARRAY_DIRECTED_GRAPH16;

extern void LWN_Copy_Map(WN *, WN *, WN_MAP);
extern void LWN_Copy_Def_Use_Node(WN*, WN*, DU_MANAGER*);
extern void LWN_Copy_Def_Use(WN*, WN*, DU_MANAGER*);
extern void LWN_Delete_DU(WN *wn);
extern void LWN_Delete_SR(WN *wn);
extern void LWN_Delete_LNO_dep_graph(WN *wn);
extern void LWN_Delete_CG_dep_graph(WN *wn);
extern void LWN_Delete_Name_Manager(WN *wn);
extern void LWN_Update_Def_Use_Delete_Tree(WN *wn, DU_MANAGER* = NULL);
extern void LWN_Update_Dg_Delete_Tree(WN *wn, ARRAY_DIRECTED_GRAPH16*);
#endif

#define Block_is_empty(b) (WN_first(b) == NULL)

/* The WN_Create routines */

extern WN *LWN_CreateStid(OPCODE opc, WN *orig_op, WN *value);

extern WN *LWN_CreateLdid(OPCODE opc, WN *orig_op);

extern WN *LWN_CreateDivfloor(TYPE_ID type, WN *kid0, WN *kid1);

extern WN *LWN_CreateDivceil(TYPE_ID type, WN *kid0, WN *kid1);

extern WN *LWN_CreateDO(WN *index,
		       WN *start,
		       WN *end,
		       WN *step,
		       WN *body);

extern WN *LWN_CreateLoopInfo(WN *induction,
		       WN *trip,
		       UINT16 trip_est,
		       UINT16 depth,
		       INT32 flags);

extern WN *LWN_CreateDoWhile(WN *test,
			  WN *body);

extern WN *LWN_CreateWhileDo(WN *test,
			  WN *body);

extern WN *LWN_CreateIf(WN *test,
		       WN *if_then,
		       WN *if_else);

extern WN *LWN_CreateCondbr( INT32 label_number,
			   WN *exp);

extern WN *LWN_CreateReturn();

extern WN *LWN_CreateCompgoto(INT32 num_entries,
			     WN *value,
			     WN *block,
			     WN *deflt);

#ifndef KEY
extern WN *LWN_CreateIstore(OPCODE opc,
			  WN_OFFSET offset, 
			  TY_IDX ty,
			  WN *value, 
			  WN *addr);
#else
extern WN *LWN_CreateIstore(OPCODE opc,
			  WN_OFFSET offset, 
			  TY_IDX ty,
			  WN *value, 
			  WN *addr, 
			  UINT field_id = 0);
#endif /* KEY */

extern WN *LWN_CreateMstore(WN_OFFSET offset,
			   TY_IDX ty,
			   WN *value,
			   WN *addr,
			   WN *num_bytes);

extern WN *LWN_CreateStid(OPCODE opc,
			 WN_OFFSET offset, 
			 ST* st, 
			 TY_IDX ty, 
			 WN *value);

extern WN *LWN_CreateEval(WN *exp);

extern WN *LWN_CreateExp1(OPCODE opc,
			 WN *kid0);

extern WN *LWN_CreateExp2(OPCODE opc,
			 WN *kid0,
			 WN *kid1);

#ifndef KEY
extern WN *LWN_CreateIload(OPCODE opc,
			 WN_OFFSET offset, 
			 TY_IDX ty1,
			 TY_IDX ty2,
			 WN *addr);
#else
extern WN *LWN_CreateIload(OPCODE opc,
			 WN_OFFSET offset, 
			 TY_IDX ty1,
			 TY_IDX ty2,
			 WN *addr, 
			 UINT field_id = 0);
#endif /* KEY */

extern WN *LWN_CreateMload(WN_OFFSET offset, 
			  TY_IDX ty,
			  WN *addr,
			  WN *num_bytes);

extern WN *LWN_CreateCvtl(OPCODE opc,
			 INT16 cvtl_bits,
			 WN *kid0);

#ifdef KEY
// Count number of prefetches generated for a particular loop.
extern INT Num_Prefetches;
#endif
extern WN *LWN_CreatePrefetch (WN_OFFSET offset,
                               UINT32 flag,
                               WN* addr);
extern WN *LWN_CreateParm(TYPE_ID rtype,
			 WN *parm_node,
			 TY_IDX ty, 
			 UINT32 flag);

extern BOOL Is_Descendent(WN *low, WN *high);

extern WN *LWN_Int_Type_Conversion(WN *wn, TYPE_ID to_type);
extern TYPE_ID Promote_Type(TYPE_ID mtype);

extern BOOL Tree_Equiv (WN *wn1, WN* wn2);

#ifdef LNO
extern WN *LWN_Loop_Trip_Count(const WN *loop);

extern BOOL Inside_Loop_With_Goto(WN *wn);
#endif

extern WN* LWN_Simplify_Tree(WN* wn); 

#endif
