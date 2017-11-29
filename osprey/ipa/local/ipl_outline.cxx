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


#include "xstats.h"			// Count_WN_Operator
#include "wn_tree_util.h"		// tree iterators

#include "ipc_pu_size.h"		// PU_Weight ()
#include "ipl_outline.h"

// check if the last statement of the block is a "return"
static BOOL
block_ends_with_return (const WN* wn)
{
    Is_True (WN_operator (wn) == OPR_BLOCK, ("Expected an OPR_BLOCK"));

    const WN* last = WN_last (wn);

    return (last != NULL && (WN_operator (last) == OPR_RETURN ||
			     WN_operator (last) == OPR_RETURN_VAL));
} // block_ends_with_return


namespace {

    struct SPLIT_CANDIDATE {
	UINT32 PU_size;
	UINT32 max_size;		// max size of inlined part
	UINT32 min_size;		// min size of outlined part

	SPLIT_CANDIDATE (UINT32 _pu, UINT32 _max, UINT32 _min) :
	    PU_size (_pu), max_size (_max), min_size (_min) {}

	BOOL operator() (UINT32 block_size) const {
	    // the outlined block must be "large"
	    if (block_size <= min_size)
		return FALSE;
	    // the inlined block must be "small"
	    // size of inlined block is computed by subtracting the
	    // outlined block from the total PU size, plus the size of the
	    // added function call, which is 1 bb + 1 return stmt + 1 call
	    return (PU_size - block_size + PU_Weight (1, 1, 1) < max_size);
	}
    };

} // namespace
		

// a split point is a pair of WN* for the splitting node and the size of the 
// block to be outlined.  Only one part of the pair contains valid
// information.  The size of the block is used only if the "IF" node is NULL.
typedef pair<const WN*, UINT32> SPLIT_POINT;

// Given a loop, we return the size of all statements within the loop
static void
Count_Loop_Size (CONST_TREE_ITER& iter, INT32& bb_cnt, INT32& stmt_cnt,
		 INT32& call_cnt) 
{
    Is_True (OPERATOR_is_scf (WN_operator (iter.Wn ())),
	     ("Expecting a SCF WHIRL node"));

    CONST_TREE_ITER next (iter);
    next.Skip ();

    const WN* last = next.Wn ();
    ++iter;
    while (iter.Wn () != last) {
	const WN* wn = iter.Wn ();
	Count_WN_Operator (WN_operator (wn), WN_rtype (wn), bb_cnt,
			   stmt_cnt, call_cnt);
	++iter;
    }

} // Count_Loop_Size


// Given a WHRIL tree, find if it can be splitted into two part according
// to the criteria specified by "split_candidate".  
static SPLIT_POINT
Find_Split_Point (CONST_TREE_ITER& iter, const WN* last,
		  const SPLIT_CANDIDATE& split_candidate)
{
    INT32 bb_cnt = 0;
    INT32 stmt_cnt = 0;
    INT32 call_cnt = 0;

    UINT32 size_under_OPR_IF = 0;	// size of blocks undef an IF node

    while (iter.Wn () != last) {
	const WN* wn = iter.Wn ();
	Is_True (wn != NULL, ("Deferencing iterator past end of tree"));
	OPERATOR opr = WN_operator (wn);
	Count_WN_Operator (opr, WN_rtype (wn), bb_cnt, stmt_cnt, call_cnt);
	if (opr == OPR_IF) {

	    // count the size of the condition
	    CONST_TREE_ITER expr (WN_kid0 (wn));
	    while (expr.Wn () != NULL) {
		const WN* expr_wn = expr.Wn ();
		Count_WN_Operator (WN_operator (expr_wn), WN_rtype (expr_wn),
				   bb_cnt, stmt_cnt, call_cnt);
		++expr;
	    }

	    // check the "then" part
	    CONST_TREE_ITER then_iter (WN_kid1(wn));
	    SPLIT_POINT then_block =
		Find_Split_Point (then_iter, NULL, split_candidate);

	    if (then_block.first != NULL)
		// a better candidate has been found
		return then_block;
	    else if (split_candidate (then_block.second)) {
		then_block.first = wn;
		return then_block;
	    } 
	    
	    // now try the "else" part
	    CONST_TREE_ITER else_iter (WN_kid2 (wn));
	    SPLIT_POINT else_block =
		Find_Split_Point (else_iter, NULL, split_candidate);
	    if (else_block.first != NULL)
		return else_block;
	    else if (split_candidate (else_block.second)) {
		else_block.first = wn;
		return else_block;
	    }

	    size_under_OPR_IF += then_block.second + else_block.second;

	    const WN* parent = iter.Get_parent_wn ();
	    iter.Skip ();
	    
	    if (parent == NULL || WN_operator (parent) != OPR_BLOCK) 
		continue;

	    if (iter.Get_parent_wn () != parent)
		continue;

	    // there are other statements under the same block after this
	    // "if" node.
	    
	    BOOL return_in_then_block = block_ends_with_return (WN_kid1 (wn));
	    BOOL return_in_else_block = block_ends_with_return (WN_kid2 (wn));
	    if (return_in_then_block || return_in_else_block) {
		// if one of the branch ends with a "return" statement,
		// then the outlined portion should include those
		// statements following this "if" node.
		CONST_TREE_ITER next_stmt (iter);
		next_stmt.Skip (1);	// skip to sibling of the enclosing 
					// OPR_BLOCK
		SPLIT_POINT split_pt =
		    Find_Split_Point (iter, next_stmt.Wn (), split_candidate);
		if (split_pt.first != NULL)
		    return split_pt;
		else if (split_candidate (split_pt.second +
					  (return_in_then_block ?
					   else_block.second :
					   then_block.second))) {
		    split_pt.first = wn;
		    return split_pt;	
		} else
		    size_under_OPR_IF += split_pt.second;
	    }
	} else if (OPERATOR_is_scf (opr)) {
	    switch (opr) {
	    case OPR_BLOCK:
	    case OPR_REGION:
	    case OPR_FUNC_ENTRY:
	    case OPR_WHERE:
		++iter;
		break;
	    default:
		Count_Loop_Size (iter, bb_cnt, stmt_cnt, call_cnt);
		break;
	    }
	} else
	    ++iter;
    }

    return std::make_pair ((const WN*) NULL,
		      size_under_OPR_IF + PU_Weight (bb_cnt, stmt_cnt,
						     call_cnt));  
} // Find_Split_Point


// Analyze the WHIRL tree if there is any part that can be cut out as a
// separate PU for partial inlining.
// Return NULL if none is found.
const WN*
Outline_Split_Point (const WN* tree_root, UINT32 max_size, UINT32 min_size)
{
    // ignore PU that could be inlined completely
    if (PU_Weight (PU_WN_BB_Cnt, PU_WN_Stmt_Cnt, PU_WN_Call_Cnt) < min_size)
	return NULL;

    // recompute the size of the PU.  This is needed because the size given 
    // by PU_WN_BB_Cnt, etc. was computed before lowering, and is not
    // correct.  

    CONST_TREE_ITER iter (tree_root);

    PU_WN_BB_Cnt = PU_WN_Stmt_Cnt = PU_WN_Call_Cnt = 0;
    while (iter.Wn () != NULL) {
	Count_WN_Operator (WN_operator (iter.Wn ()), WN_rtype (iter.Wn()),
			   PU_WN_BB_Cnt, PU_WN_Stmt_Cnt, PU_WN_Call_Cnt);
	++iter;
    }

    UINT32 pu_size = PU_Weight (PU_WN_BB_Cnt, PU_WN_Stmt_Cnt, PU_WN_Call_Cnt);

    CONST_TREE_ITER tree_iter (tree_root);
    
    SPLIT_POINT split_pt =
	Find_Split_Point (tree_iter, (const WN*) NULL,
			  SPLIT_CANDIDATE (pu_size, max_size, min_size));

    return split_pt.first;
} // Outline_Split_Point
	
	    
