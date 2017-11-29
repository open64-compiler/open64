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


//-*-c++-*-
/* ====================================================================
 * ====================================================================
 *
 * Module: ori.cxx
 * $Revision: 1.6 $
 * $Date: 05/12/05 08:59:31-08:00 $
 * $Author: bos@eng-24.pathscale.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/region/SCCS/s.ori.cxx $
 *
 * Description:
 * Olimit Region Insertion (ORI):
 * First we do goto conversion to get better structured code.
 * Then we build a graph of the blocks,
 * and try to find a set of blocks that would make a good region.
 * Then we create a region around those blocks.
 * See doc/Mongoose/region.olimit
 *
 * TODO:  currently we don't look inside an existing region; 
 * might need to re-run ori on nested regions if came from front-end.
 * e.g. mp and olimit interaction.
 *
 * TODO:  would like loop regions to include initialization code,
 * which means looking backwards for init statements.
 *
 * ====================================================================
 * ====================================================================
 */

#include <list>
#include "defs.h"
#include "tracing.h"
#include "errors.h"
#include "erglob.h"
#include "mempool.h"
#include "wn.h"
#include "wn_util.h"
#include "opcode.h"
#include "ir_reader.h"
#include "config.h"
#include "xstats.h"
#include "targ_sim.h"
#include "timing.h"
#include "ori.h"
#include "opt_goto.h"
#include "region_util.h"

typedef struct wnlist {
  WN		*item;
  struct wnlist	*next;
} WNLIST;
#define WNLIST_item(s)  ((s)->item)
#define WNLIST_next(s)  ((s)->next)

typedef struct ori_block {
	mUINT32 id;			// unique id for debugging
	WN *tree;			// head of WN tree for block
	WN *end_tree;			// last stmt of WN tree for block
	WN *container;			// block that contains tree
	mINT32 num_stmts;
	mINT32 num_bbs;
	WNLIST *labels;
	struct ori_block *parent;
	struct ori_block *child;
	struct ori_block *sibling;
	mBOOL expanded;			// true if all siblings are exposed
	mBOOL no_more_merge;		// true if completely merged
	mBOOL nested_region;		// one of the children is a region
	mBOOL never_in_region;		// never want this block in a region
	mBOOL illegal;			// this block makes an illegal region
} ORI_BLOCK;

#define OB_id(b)		((b) ? (b)->id : 0)
#define Set_OB_id(b,v)		((b)->id = v)
#define OB_tree(b)		((b)->tree)
#define OB_end_tree(b)		((b)->end_tree)
#define OB_container(b)		((b)->container)
#define OB_num_stmts(b)		((b)->num_stmts)
#define OB_num_bbs(b)		((b)->num_bbs)
#define OB_labels(b)		((b)->labels)
#define OB_parent(b)		((b)->parent)
#define OB_child(b)		((b)->child)
#define OB_sibling(b)		((b)->sibling)
#define OB_expanded(b)		((b)->expanded)
#define OB_no_more_merge(b)	((b)->no_more_merge)
#define OB_nested_region(b)	((b)->nested_region)
#define OB_never_in_region(b)	((b)->never_in_region)
#define OB_illegal(b)		((b)->illegal)

// OB_size is the olimit size of the block
// this must match PU_Olimit computation
#define OB_size(b)	COMPUTE_OLIMIT(OB_num_bbs(b),OB_num_stmts(b))
// OB_sub_size is the olimit size of block a without block b;
// it is assumed that b is nested inside of a
#define OB_sub_size(a,b)	\
	COMPUTE_OLIMIT((OB_num_bbs(a) - OB_num_bbs(b)),(OB_num_stmts(a) - OB_num_stmts(b)))
#define OB_first_sibling(b)	(OB_child(OB_parent(b)))
	
typedef struct oblist {
  ORI_BLOCK *item;
  struct oblist	*next;
} OBLIST;
#define OBLIST_item(s)  ((s)->item)
#define OBLIST_next(s)  ((s)->next)

static BOOL Trace_ORI = FALSE;		// high-level debugging output
static BOOL Run_Goto_Conversion = FALSE;
	// is problem with goto_conversion creating loop that
	// crosses compgoto scope, so leave this off for now.
static BOOL Trace_Goto_Conversion = FALSE;
static BOOL Trace_Merging = FALSE;	// trace merging process
static BOOL Trace_Blocks = FALSE;	// trace ori-block list
static MEM_POOL Ori_pool;		// allocations local to ORI
static ORI_BLOCK *pu_block;		// block for pu
static ORI_BLOCK *cur_block;		// current block
static INT last_id = 0;			// last ORI_BLOCK id
static INT num_regions = 0;		// number of regions created

static UINT32 *blocks_in_region;	// array of block ids in a region
static INT last_block_in_region_index = 0;	// index of last id in array
static INT max_block_in_region_index = 0;	// end of array
static WNLIST **labels_in_region;	// array of labels in a region
static INT last_label_in_region_index = 0;	// index of last label in array

// label_branches is a list indexed by label number of the ORI_BLOCKs that 
// contain branches to the label.  This is used when merging label blocks.
static OBLIST **label_branches;

static void
Initialize_Trace_Flags (void)
{
	Trace_ORI = Get_Trace (TP_ORI, 1);
	Trace_Merging = Get_Trace (TP_ORI, 2);
	Trace_Blocks = Get_Trace (TP_ORI, 4);
	Run_Goto_Conversion = Get_Trace (TP_ORI, 8);
	Trace_Goto_Conversion = Get_Trace (TP_ORI, 16);
}

// this must happen after goto conversion since it depends on
// the number of labels in the PU.
static void
Initialize_ORI (void)
{
	num_regions = 0;
	last_id = 0;
	last_block_in_region_index = 0;
	blocks_in_region = NULL;
	// allocate label_branches list
	INT last_label_in_PU = LABEL_Table_Size(CURRENT_SYMTAB);
	label_branches = (OBLIST**) MEM_POOL_Alloc (&Ori_pool, 
		sizeof(OBLIST*) * (last_label_in_PU+1));
	INT i;
	for (i = 0; i <= last_label_in_PU; i++) {
		label_branches[i] = NULL;
	}
}

static void
Check_Dump (WN *tree, const char *msg)
{
	if (Get_Trace(TKIND_IR, TP_ORI)) {
		fputs(DBar, TFile);
		fprintf(TFile, "%s\n", msg);
		fdump_tree(TFile, tree);
		fputs(DBar, TFile);
	}
}

// prepend wn to list of nodes
static void
Add_Wn_To_List (WN *wn, WNLIST **wnlist)
{
	WNLIST *p;
	p = (WNLIST *) MEM_POOL_Alloc (&Ori_pool, sizeof(WNLIST));
	WNLIST_item(p) = wn;
	WNLIST_next(p) = *wnlist;
	*wnlist = p;
}

// remove wn from list
// (have to search to find it, but lists should be short, and this routine
// rarely invoked).
static void
Remove_Wn_From_List (WN *wn, WNLIST **wnlist)
{
	WNLIST *p = *wnlist;
	WNLIST *prev = NULL;
	while (p != NULL) {
		if (WNLIST_item(p) == wn) break;	// found it
		prev = p;
		p = WNLIST_next(p);
	}
	FmtAssert (p != NULL, ("Remove_Wn_From_List didn't find wn"));
	if (prev != NULL) {
		WNLIST_next(prev) = WNLIST_next(p);
	}
	else {
		// must be first item in list
		*wnlist = WNLIST_next(p);
	}
}

// prepend ob to list of nodes
static void
Add_Ob_To_List (ORI_BLOCK *ob, OBLIST **oblist)
{
	OBLIST *p;
	// don't add if already in the list
	if (*oblist != NULL && OBLIST_item(*oblist) == ob) return;

	p = (OBLIST *) MEM_POOL_Alloc (&Ori_pool, sizeof(OBLIST));
	OBLIST_item(p) = ob;
	OBLIST_next(p) = *oblist;
	*oblist = p;
}

// remove ob from list
// (have to search to find it, but lists should be short, and this routine
// rarely invoked).
static void
Remove_Ob_From_List (ORI_BLOCK *ob, OBLIST **oblist)
{
	OBLIST *p = *oblist;
	OBLIST *prev = NULL;
	while (p != NULL) {
		if (OBLIST_item(p) == ob) break;	// found it
		prev = p;
		p = OBLIST_next(p);
	}
	// because we only add to list if unique, 
	// may remove first instance and then not find it for later instance.
	if (p == NULL) 
		return;
	else if (prev != NULL) {
		OBLIST_next(prev) = OBLIST_next(p);
	}
	else {
		// must be first item in list
		*oblist = OBLIST_next(p);
	}
}

// find previous sibling
static ORI_BLOCK *
OB_prev (ORI_BLOCK *b)
{
	ORI_BLOCK *prev = OB_first_sibling(b);
	for (; prev != NULL; prev = OB_sibling(prev)) {
		if (OB_sibling(prev) == b)
			return prev;
	}
	return NULL;
}

// return nesting level of ORI_BLOCK */
static INT
OB_level (ORI_BLOCK *b)
{
	INT level = 0;
	for (; b != NULL; b = OB_parent(b)) {
		level++;
	}
	return level;
}

static void
Print_Ori_Block (FILE *f, ORI_BLOCK *b)
{
	if (b == NULL) return;
	fprintf(f, "ORI_BLOCK %d ", OB_id(b));
	switch (WN_opcode(OB_tree(b))) {
	case OPC_BLOCK:
		fprintf(f, "(%s)", (OB_sibling(b) ? "OPC_THEN" : "OPC_ELSE") );
		break;
	case OPC_LABEL:
		fprintf(f, "(OPC_LABEL %d)", WN_label_number(OB_tree(b)) );
		break;
	case OPC_REGION:
		fprintf(f, "(OPC_REGION %d)", WN_region_id(OB_tree(b)) );
		break;
	default:
		fprintf(f, "(%s %d)", OPCODE_name(WN_opcode(OB_tree(b))),
			Srcpos_To_Line(WN_linenum(OB_tree(b))) );
		break;
	}
	if (OB_end_tree(b) != OB_tree(b)) {
		fprintf(f, " to (%s %d)", 
			OPCODE_name(WN_opcode(OB_end_tree(b))),
			Srcpos_To_Line(WN_linenum(OB_end_tree(b))) );
	}
	if (OB_expanded(b))		fprintf(f, " [expanded]");
	if (OB_no_more_merge(b))	fprintf(f, " [merged]");
	if (OB_nested_region(b))	fprintf(f, " [nested]");
	if (OB_never_in_region(b))	fprintf(f, " [never]");
	if (OB_illegal(b))		fprintf(f, " [illegal]");
	fprintf(f, ":\n");
	fprintf(f, "\tsize = %d (%d * %d)\n", OB_size(b), OB_num_bbs(b), OB_num_stmts(b));
	fprintf(f, "\tparent = %d, child = %d, sibling = %d\n",
		OB_id(OB_parent(b)), OB_id(OB_child(b)), OB_id(OB_sibling(b)) );
	// don't look underneath a region anymore
	if (WN_opcode(OB_tree(b)) != OPC_REGION || OB_end_tree(b) != OB_tree(b))
		Print_Ori_Block (f, OB_child(b));
	Print_Ori_Block (f, OB_sibling(b));
}

// get first stmt and the containing tree of a block.
// either by finding body block, or by returning stmt.
static void
Get_First_Stmt_And_Container (ORI_BLOCK *b, WN **stmt, WN **container)
{
	WN *tree = OB_tree(b);
	// loop until find block or stmt
	while (1) {
		switch (WN_opcode(tree)) {
		case OPC_BLOCK:
			*stmt = WN_first(tree);
			*container = tree;
			return;
		case OPC_FUNC_ENTRY:
			tree = WN_func_body(tree);
			break;
		case OPC_IF:
			tree = WN_then(tree);
			break;
		case OPC_WHILE_DO:
		case OPC_DO_WHILE:
			tree = WN_while_body(tree);
			break;
		case OPC_DO_LOOP:
			tree = WN_do_body(tree);
			break;
		case OPC_LABEL:
			*stmt = WN_next(tree);
			*container = OB_container(b);
			return;
		case OPC_COMPGOTO:
			*stmt = OB_child(b) ? OB_tree(OB_child(b)) : NULL;
			*container = OB_container(b);
			return;
		case OPC_REGION:
			// may surround other stmts
			*stmt = tree;
			*container = OB_container(b);
			return;
		default:
			if (OPCODE_is_stmt(WN_opcode(tree))) {
				*stmt = tree;
				*container = OB_container(b);
			}
			else {
				FmtAssert (FALSE, ("unexpected opcode in Get_First_Stmt"));
			}
			return;
		}
	}
}

static ORI_BLOCK *
New_Ori_Block (WN *tree, ORI_BLOCK *parent)
{
	ORI_BLOCK *b;
	WN *t;
	b = (ORI_BLOCK *) MEM_POOL_Alloc (&Ori_pool, sizeof(ORI_BLOCK));
	Set_OB_id(b, ++last_id);
	OB_tree(b) = tree;
	OB_end_tree(b) = tree;
	if (WN_opcode(tree) == OPC_BLOCK)
		OB_container(b) = tree;
	else if (parent)
		Get_First_Stmt_And_Container (parent, &t, &OB_container(b));
	else
		OB_container(b) = NULL;
	OB_parent(b) = parent;
	OB_child(b) = OB_sibling(b) = NULL;
	OB_labels(b) = NULL;
	OB_num_stmts(b) = OB_num_bbs(b) = 0;
	OB_expanded(b) = FALSE;
	OB_no_more_merge(b) = FALSE;
	OB_nested_region(b) = FALSE;
	OB_never_in_region(b) = FALSE;
	OB_illegal(b) = FALSE;
	return b;
}

static void
Propagate_Size_Info (ORI_BLOCK *b, INT32 num_bbs, INT32 num_stmts)
{
	while (b != NULL) {
		OB_num_bbs(b) += num_bbs;
		OB_num_stmts(b) += num_stmts;
		b = OB_parent(b);
	}
}

// Have to add child's size info to parents.
// Parents may have own info which also needs to be copied up.
// So, walk down the tree and copy info all the way up 
// as soon as we see it.  This means copying the block info
// before the child info is propagated to it, so that the info
// is only added once per parent block.
static void
Propagate_Child_Info (ORI_BLOCK *b)
{
	// propagate info
	Propagate_Size_Info (OB_parent(b), OB_num_bbs(b), OB_num_stmts(b));

	// recurse
	if (OB_child(b))
		Propagate_Child_Info (OB_child(b));
	if (OB_sibling(b))
		Propagate_Child_Info (OB_sibling(b));
}

#define IS_RETURN_PREG(wn) \
        (ST_class(WN_st(wn)) == CLASS_PREG \
	&& (Is_Return_Preg(WN_load_offset(wn)) \
	    || WN_st(wn) == Return_Val_Preg ) )

// traverse expression tree to find any use of return preg
// (may be more complicated than just a load/store,
// e.g. may be shift of value, or a COMPGOTO for alternate return).
static BOOL Uses_Return_Preg (WN *tree)
{
  WN_ITER *wni;
  WN *wn;

  for (wni = WN_WALK_TreeIter (tree);
       wni != NULL;
       wni = WN_WALK_TreeNext (wni))
  {
    wn = WN_ITER_wn (wni);
    if (WN_operator(wn) == OPR_LDID && IS_RETURN_PREG(wn) )
      return TRUE;
  }
  return FALSE;
}

// alternate returns mean that call is followed by compgoto
// that loads return preg.
static BOOL 
Is_Call_With_Alternate_Return (WN *call, WN *stmt) 
{
	if (call != NULL && OPCODE_is_call(WN_opcode(call))
		&& WN_opcode(stmt) == OPC_COMPGOTO) 
	{
		WN *val = WN_kid0(stmt);
		if (WN_operator(val) == OPR_ADD
	 		&& OPCODE_is_load(WN_opcode(WN_kid0(val)))
			&& IS_RETURN_PREG(WN_kid0(val)) )
		{
			return TRUE;
		}
	}
	return FALSE;
}


// walk the WN tree and create ORI_BLOCKs for sub-blocks of code
static ORI_BLOCK *
Build_Ori_Blocks (WN *tree, INT32 olimit)
{
	OPCODE opcode = WN_opcode(tree);
	WN *wn;
	ORI_BLOCK *b = NULL;
	ORI_BLOCK *save_block = NULL;
	// make switch_block be list stack variable in case are nested switches
	std::list<ORI_BLOCK*> switch_list;

	switch (opcode) {
	case OPC_FUNC_ENTRY:
		b = cur_block = New_Ori_Block (tree, NULL);
		OB_child(b) = Build_Ori_Blocks (WN_func_body(tree), olimit);
		break;
	case OPC_BLOCK:
		ORI_BLOCK *tmp;
		// walk block and look for sub-blocks
		wn = WN_first(tree);
		while (wn) {
			if (!switch_list.empty() && WN_opcode(wn) == OPC_LABEL)
			{ 
			    std::list<ORI_BLOCK*>::iterator switch_it;
			    for (switch_it = switch_list.begin();
				switch_it != switch_list.end();
				++switch_it)
			    {
				if (WN_last_label(OB_tree(*switch_it)) 
				    == WN_label_number(wn)) 
					break;
			    }
			    if (switch_it != switch_list.end()) {
				// found end of switch
				if (Trace_ORI) fprintf(TFile, "ori: found end label %d for switch\n", WN_label_number(wn));
				// want end-label of switch to be in 
				// switch block to reduce # of region-exits.
				// end-label is compiler-generated,
				// so only reached by code in switch.
				OB_end_tree(*switch_it) = wn;
				// if accumulating stmts, set end_tree
				if (b != NULL && OB_end_tree(b) == NULL) {
					OB_end_tree(b) = WN_prev(wn);
				}
				// we do some extra processing here
				// while we know the context.
				// create block for end-label
				tmp = New_Ori_Block (wn, *switch_it);
				OB_num_bbs(*switch_it)++;
				Add_Wn_To_List (wn, &OB_labels(tmp));
				if (b != NULL) OB_sibling(b) = tmp;
				cur_block = OB_parent(*switch_it);
				b = *switch_it;
				switch_list.erase(switch_it);
				// skip label since already processed.
				wn = WN_next(wn);
				if (wn == NULL) break;
			    }
			}
			tmp = Build_Ori_Blocks (wn, olimit);
			if (tmp != NULL) {
			    if (b == NULL) {
				b = tmp;
				if (!switch_list.empty()) {
					// last switch is first on list
					OB_child(*switch_list.begin()) = b;
				}
				else {
					save_block = b;
				}
			    }
			    else if (WN_opcode(wn) == OPC_PRAGMA
				&& WN_pragma(wn) == WN_PRAGMA_PREAMBLE_END) 
			    {
				// e.g. if label in preamble block.
				// preamble block covers all preamble code,
				// so remove previous block.
				OB_child(OB_parent(b)) = tmp;
				OB_parent(tmp) = OB_parent(b);
				if (b == cur_block) cur_block = OB_parent(b);
				b = tmp;
				save_block = b;
			    }
			    else {
				OB_sibling(b) = tmp;
				// if accumulating stmts, set end_tree
				if (OB_end_tree(b) == NULL) {
					if (wn != OB_tree(tmp)
					&& !Is_Call_With_Alternate_Return (OB_tree(tmp), wn))
					{
						DevWarn("ORI:  current wn != tree(tmp)");
					}
					// old:  OB_end_tree(b) = WN_prev(wn);
					OB_end_tree(b) = WN_prev(OB_tree(tmp));
				}
				// by default we made sub-blocks
				// be children rather than siblings,
				// so change parent and container.
				while (tmp != NULL) {
				    OB_parent(tmp) = OB_parent(b);
				    OB_container(tmp) = OB_container(b);
				    tmp = OB_sibling(tmp);
				}
				// if cur_block popped back to prev
				// (not accumulating stmts),
				// then really want cur_block to be
				// parent block.
				if (cur_block == b) 
					cur_block = OB_parent(b);
			    }
			    while (OB_sibling(b) != NULL) {
				b = OB_sibling(b);
			    }
			    if (WN_opcode(OB_tree(b)) == OPC_COMPGOTO
				&& WN_last_label(OB_tree(b)) != 0)
			    {
				// Create block around whole switch body,
			 	// with labels going into block, 
				// so won't merge cases.
			 	// Unfortunately, WHIRL doesn't preserve 
				// scoping of switch, so we need to 
				// special-case this code.
				switch_list.push_front (b);
				cur_block = b;
				b = NULL;
			    }
			}
			wn = WN_next(wn);
		}
		if (!switch_list.empty()) 
			DevWarn("ORI: didn't find end of switch/compgoto");
		// if accumulating stmts, set end_tree
		if (b != NULL && OB_end_tree(b) == NULL) {
			OB_end_tree(b) = WN_last(tree);
		}
		// set end_tree if cur_block didn't begin with a block
		// want to do this if accumulating statements in b block,
		// but not if stmts go into parent block.
		if (b == cur_block && WN_opcode(OB_tree(b)) != OPC_BLOCK) {
			OB_end_tree(cur_block) = WN_last(tree);
		}
		b = save_block;
		break;
	case OPC_DO_LOOP:
		save_block = cur_block;
		b = New_Ori_Block (tree, cur_block);
		cur_block = b;
		OB_num_bbs(cur_block)++;
		OB_child(b) = Build_Ori_Blocks (WN_do_body(tree), olimit);
		cur_block = save_block;
		break;
	case OPC_WHILE_DO:
	case OPC_DO_WHILE:
		save_block = cur_block;
		b = New_Ori_Block (tree, cur_block);
		cur_block = b;
		OB_num_bbs(cur_block)++;
		OB_child(b) = Build_Ori_Blocks (WN_while_body(tree), olimit);
		cur_block = save_block;
		break;
	case OPC_IF:
		// build block for if, and for then and else blocks
		save_block = cur_block;
		b = New_Ori_Block (tree, cur_block);	// if-block
		cur_block = b;
		OB_num_bbs(cur_block)++;
		if (WN_else(tree) != NULL) OB_num_bbs(cur_block)++;
		if (WN_else(tree) == NULL) {
			// merge then block into if
			OB_child(b) = Build_Ori_Blocks (WN_then(tree), olimit);
		}
		else {
			// if has else block, so build blocks for
			// then and else under the if block.
			// else is sibling of then block.
			ORI_BLOCK *ifb = cur_block;	// if-block
			OB_child(ifb) = New_Ori_Block (WN_then(tree), ifb);
			b = OB_child(ifb);
			OB_sibling(b) = New_Ori_Block (WN_else(tree), ifb);
			cur_block = b;
			OB_child(b) = Build_Ori_Blocks (WN_then(tree), olimit);
			b = OB_sibling(b);
			cur_block = b;
			OB_child(b) = Build_Ori_Blocks (WN_else(tree), olimit);
			b = ifb;
		}
		cur_block = save_block;
		break;
	case OPC_LABEL:
	case OPC_EXC_SCOPE_BEGIN:
	case OPC_EXC_SCOPE_END:
	case OPC_ALTENTRY:
		// Assume cur_block is parent;
		// if is sibling, will fix up when return to BLOCK
		b = New_Ori_Block (tree, cur_block);
		OB_end_tree(b) = NULL;	// mark that accumulating stmts
		cur_block = b;

		if (opcode == OPC_ALTENTRY) {
			// don't want altentry node in a region.
			// we assume label follows altentry node,
			// so altentry block will be small
			OB_never_in_region(b) = TRUE;
		} 
		else if (opcode == OPC_LABEL && WN_Label_Is_Handler_Begin(tree)) {
			// handlers are like altentries,
			// except they are not followed by label,
			// so want following statements to be separate block.
			OB_never_in_region(b) = TRUE;
			OB_end_tree(b) = tree;
			OB_sibling(b) = New_Ori_Block (WN_next(tree), OB_parent(b));
			cur_block = OB_sibling(b);
			OB_end_tree(cur_block) = NULL;	// mark that accumulating stmts
		}
		else {
			// labels start new bb;
			// don't count altentries cause 
			// can't put them in region anyways.
			OB_num_bbs(cur_block)++;
			Add_Wn_To_List (tree, &OB_labels(b));
		}
		break;
	case OPC_PRAGMA:
		// MP programs want preamble_end at PU-level
		if (WN_pragma(tree) == WN_PRAGMA_PREAMBLE_END) {
			if (WN_opcode(cur_block->tree) == OPC_ALTENTRY) {
				// combine this into altentry block
				break;
			}
			// treat like label node and start new block.
			// create 1 block for pragma, 
			// which is never_in_region.
			b = New_Ori_Block (tree, cur_block);
			OB_never_in_region(b) = TRUE;
			// also want all previous statements to be
			// in this never block.
			OB_tree(b) = WN_first(OB_container(b));
			OB_num_stmts(b) = OB_num_stmts(cur_block);
			OB_num_bbs(b) = OB_num_bbs(cur_block);
			OB_num_stmts(cur_block) = 0;
			OB_num_bbs(cur_block) = 0;
			if (OB_size(b) > olimit) {
				// eventually want to create region before 
				// preamble-end when safe, but that is too
				// risky right now.  So far this only happens
				// when large skip_flist block.
				DevWarn("ORI: Can't create region before preamble_end");
				OB_num_stmts(b) = 0;	// so never any regions
				OB_num_bbs(b) = 0;	// so never any regions
			}
		}
		break;
	case OPC_SWITCH:
	case OPC_COMPGOTO:
		if (WN_last_label(tree) != 0) {
			// 
			// Create block around whole switch body,
			// with labels going in block, so won't merge cases.
			// Put special-case code inside BLOCK for this.
			//
			b = New_Ori_Block (tree, cur_block);
			if (Trace_ORI) fprintf(TFile, "ori: create block %d around switch body\n", OB_id(b));
			cur_block = b;
		}
		else {
			// fortran comp-goto:  not structured.
			// can't have exit blocks for each compgoto target, 
			// so don't allow region around compgoto's.
			b = New_Ori_Block (tree, cur_block);
			OB_never_in_region(b) = TRUE;
			if (Is_Call_With_Alternate_Return (WN_prev(tree), tree))
			{
				// put call in same block as compgoto,
				// so not split
				OB_tree(b) = WN_prev(tree);
				// count call in new block, not old block
				Count_WN_Node (WN_prev(tree), 
					&OB_num_bbs(b), &OB_num_stmts(b));
				OB_num_bbs(cur_block) -= OB_num_bbs(b);
				OB_num_stmts(cur_block) -= OB_num_stmts(b);
				if (Trace_ORI) fprintf(TFile, "ori: create block %d around call with alternate return\n", OB_id(b));
			}
			else if (Trace_ORI) fprintf(TFile, "ori: create block %d around compgoto list\n", OB_id(b));
		}
		OB_num_bbs(b)++;
		// children of switch/compgoto should be a block of 
		// casegotos/gotos and a default goto
		for (wn = WN_first(WN_switch_table(tree)); wn != NULL; wn = WN_next(wn)) {
			Add_Ob_To_List (b, &label_branches[WN_label_number(wn)]);
		}
		wn = WN_switch_default(tree);
		if (wn != NULL) {
			Add_Ob_To_List (b, &label_branches[WN_label_number(wn)]);
		}
		break;
	case OPC_CASEGOTO:
		Add_Ob_To_List (cur_block, &label_branches[WN_label_number(tree)]);
		break;
	case OPC_GOTO:
	case OPC_TRUEBR:
	case OPC_FALSEBR:
		OB_num_bbs(cur_block)++;
		Add_Ob_To_List (cur_block, &label_branches[WN_label_number(tree)]);
		break;
	case OPC_IO:
		INT32 i;
		for (i=0; i<WN_kid_count(tree); i++) {
			WN *kid = WN_kid(tree, i);
			if (WN_opcode(kid) == OPC_IO_ITEM
			    && (WN_io_item(kid) == IOC_END 
			     || WN_io_item(kid)==IOC_ERR)
			    && WN_opcode(WN_kid0(kid)) == OPC_GOTO)
			{
				// found implicit branch to label
				WN *branch_wn = WN_kid0(kid);
				LABEL_IDX lnum = WN_label_number(branch_wn);
				// this will get lowered to goto,
				// so add to branch list.
				Add_Ob_To_List (cur_block, &label_branches[lnum]);
			}
		}
		Count_WN_Node (tree, &OB_num_bbs(cur_block), &OB_num_stmts(cur_block));
		break;
	case OPC_REGION:
		// skip user regions
		OB_num_bbs(cur_block)++;
		break;
	default:
		Count_WN_Opcode (opcode, &OB_num_bbs(cur_block), &OB_num_stmts(cur_block));
		break;
	}
	return b;
}


// have a block that we want a region around, so create region node
static void
Insert_Region_Around_Block (ORI_BLOCK *b)
{
	WN *wn;
	WN *region;
	Set_PU_has_region(Get_Current_PU());
	num_regions++;

	if (WN_opcode(OB_tree(b)) == OPC_BLOCK) {
		// insert region under this block
		// don't want to copy nodes, so just change pointers
		wn = WN_CreateBlock();
		WN_first(wn) = WN_first(OB_tree(b));
		WN_last(wn) = WN_last(OB_tree(b));
		region = WN_CreateRegion (REGION_KIND_OLIMIT, wn, NULL, NULL, 
					  RID_CREATE_NEW_ID, INITO_IDX_ZERO);
		WN_first(OB_tree(b)) = WN_last(OB_tree(b)) = region;
	}
	else {
		// insert region in container block
		WN *prev = WN_prev(OB_tree(b));
		wn = WN_CreateBlock();
		WN_first(wn) = WN_EXTRACT_ItemsFromBlock (
			OB_container(b), OB_tree(b), OB_end_tree(b));
		WN_last(wn) = OB_end_tree(b);
		region = WN_CreateRegion (REGION_KIND_OLIMIT, wn, NULL, NULL, 
					  RID_CREATE_NEW_ID, INITO_IDX_ZERO);
		WN_INSERT_BlockAfter (OB_container(b), prev, region);
		// if parent's tree points to same tree as b,
		// then need to update parent to point to region
		ORI_BLOCK *parent;
		for (parent = OB_parent(b); 
			parent != NULL && OB_tree(parent) == OB_tree(b); 
			parent = OB_parent(parent)) 
		{
			OB_tree(parent) = region;
		}
		for (parent = OB_parent(b); 
			parent != NULL && OB_end_tree(parent) == OB_end_tree(b); 
			parent = OB_parent(parent)) 
		{
			OB_end_tree(parent) = region;
		}
	}

	WN_Set_Linenum(region, WN_Get_Linenum(OB_tree(b)));
	if (Trace_ORI) fprintf(TFile, "ori: insert region %d of size %d around block %d\n", WN_region_id(region), OB_size(b), OB_id(b));
	// subtract region size from parents
	Propagate_Size_Info (OB_parent(b), -(OB_num_bbs(b)), -(OB_num_stmts(b)));
	OB_nested_region(OB_parent(b)) = TRUE;
	// clear region block size,
	// but leave in tree so expand will know this is a break-point
	// for creating sibling blocks 
	// (i.e. avoid unnecessarily nesting this region).
	// also leave in child blocks so can find nested branches.
	OB_num_bbs(b) = OB_num_stmts(b) = 0;
	// OB_child(b) = NULL;
	OB_tree(b) = OB_end_tree(b) = region;
}

static void
Move_Branches_To_Child (ORI_BLOCK *parent, ORI_BLOCK *child)
{
	WN *stmt;
	WN *wn;
	OBLIST **branches;
	Get_First_Stmt_And_Container (child, &stmt, &wn);
	while (stmt != NULL) {
		switch (WN_opcode(stmt)) {
		case OPC_SWITCH:
		case OPC_COMPGOTO:
			for (wn = WN_first(WN_switch_table(stmt)); wn != NULL; wn = WN_next(wn)) {
				branches = &label_branches[WN_label_number(wn)];
				Remove_Ob_From_List (parent, branches);
				Add_Ob_To_List (child, branches);
				if (Trace_Merging) fprintf(TFile, "ori: move branch to label %d from block %d to block %d\n", WN_label_number(stmt), OB_id(parent), OB_id(child));
			}
			wn = WN_switch_default(stmt);
			if (wn != NULL) {
				branches = &label_branches[WN_label_number(wn)];
				Remove_Ob_From_List (parent, branches);
				Add_Ob_To_List (child, branches);
				if (Trace_Merging) fprintf(TFile, "ori: move branch to label %d from block %d to block %d\n", WN_label_number(stmt), OB_id(parent), OB_id(child));
			}
			break;
		case OPC_CASEGOTO:
		case OPC_GOTO:
		case OPC_TRUEBR:
		case OPC_FALSEBR:
			branches = &label_branches[WN_label_number(stmt)];
			Remove_Ob_From_List (parent, branches);
			Add_Ob_To_List (child, branches);
			if (Trace_Merging) fprintf(TFile, "ori: move branch to label %d from block %d to block %d\n", WN_label_number(stmt), OB_id(parent), OB_id(child));
			break;
		}
		if (stmt == OB_end_tree(child)) break;
		stmt = WN_next(stmt);
	}
}

// Create a child block and move some of the parent's info into it.
static ORI_BLOCK *
Create_New_Child (ORI_BLOCK *parent, ORI_BLOCK *prev, ORI_BLOCK *sibling, WN *tree, WN *end_tree, INT num_bbs, INT num_stmts)
{
	ORI_BLOCK *b;
	WN *first_stmt, *container;
	Get_First_Stmt_And_Container (parent, &first_stmt, &container);
	b = New_Ori_Block (tree, parent);
	OB_end_tree(b) = end_tree;
	OB_container(b) = container;
	OB_sibling(b) = sibling;
	if (prev) 
		OB_sibling(prev) = b;
	else
		OB_child(parent) = b;
	OB_num_bbs(b) = num_bbs;
	OB_num_stmts(b) = num_stmts;
	if (Trace_ORI) fprintf(TFile, "ori: add block %d as child of %d, sibling of %d\n", OB_id(b), OB_id(parent), OB_id(prev));
	Move_Branches_To_Child (parent, b);
	return b;
}

// split the block in half.
// original block becomes the parent of 2 half-blocks.
static void
Split_Block (ORI_BLOCK *b)
{
	INT goal = OB_size(b) / 2;
	INT num_bbs = 0;
	INT num_stmts = 0;
	ORI_BLOCK *first, *second;
	WN *first_stmt, *stmt;
	WN *container;
	WN *end_stmt;
	BOOL in_stmt_group = FALSE;

	FmtAssert(OB_child(b) == NULL, ("Split_Block: can't handle blocks with chilren"));
	// get first stmt and containing tree in the block
	Get_First_Stmt_And_Container (b, &first_stmt, &container);

	// find half-way point
	stmt = first_stmt;
	while (stmt != NULL && COMPUTE_OLIMIT(num_bbs,num_stmts) < goal) {
		Count_WN_Node (stmt, &num_bbs, &num_stmts);
		if (WN_opcode(stmt) == OPC_PRAGMA 
		    && WN_pragma(stmt) == WN_PRAGMA_START_STMT_CLUMP) 
			in_stmt_group = TRUE;
		if (WN_opcode(stmt) == OPC_PRAGMA 
		    && WN_pragma(stmt) == WN_PRAGMA_END_STMT_CLUMP) 
			in_stmt_group = FALSE;
		stmt = WN_next(stmt);
	}
	FmtAssert(stmt != NULL, ("Split_Block: couldn't split %d", OB_id(b)));

	// don't split inside a stmt-group (e.g. io statements)
	if (in_stmt_group && stmt != OB_end_tree(b)) {
		if (Trace_ORI) fprintf(TFile, "ori: don't split block %d in middle of stmt group\n", OB_id(b));
	}
	// don't split between call and store of return preg
	// (could be two stores of return pregs after call).
	// also need to check for case of splitting between 
	// 2 return stmts after call, so just make sure that
	// split doesn't happen where return preg is seen.
	while (Uses_Return_Preg(stmt)) {
		Count_WN_Node (stmt, &num_bbs, &num_stmts);
		stmt = WN_next(stmt);
	}
	// don't split between load of return preg and return statement.
	if (WN_opcode(stmt) == OPC_RETURN) {
		Count_WN_Node (stmt, &num_bbs, &num_stmts);
		stmt = WN_next(stmt);
	}
	// don't split between pragma and statement;
	// can be multiple pragmas before statement.
	while (WN_opcode(WN_prev(stmt)) == OPC_PRAGMA 
	    || WN_opcode(WN_prev(stmt)) == OPC_XPRAGMA) 
	{
		Count_WN_Node (stmt, &num_bbs, &num_stmts);
		stmt = WN_next(stmt);
	}

	// find last stmt
	if (WN_opcode(OB_tree(b)) == OPC_BLOCK)
		end_stmt = WN_last(OB_tree(b));
	else if (OB_end_tree(b) == OB_tree(b))
		// e.g. loop node; container == body block
		end_stmt = WN_last(container);
	else
		end_stmt = OB_end_tree(b);

	// create nested split blocks
	first = Create_New_Child (b, NULL, NULL, first_stmt, WN_prev(stmt), 
		num_bbs, num_stmts);
	second = Create_New_Child (b, first, NULL, stmt, end_stmt, 
		OB_num_bbs(b) - num_bbs, OB_num_stmts(b) - num_stmts);
	if (OB_never_in_region(b)) {
		// assume never property applies to first statement in block
		OB_never_in_region(first) = TRUE;
	}
	if (Trace_ORI) fprintf(TFile, "ori: split block %d into %d and %d\n", OB_id(b), OB_id(first), OB_id(second));
	if (Trace_Blocks) Print_Ori_Block (TFile, b);
}

// Initially there may be stmts between the sibling blocks
// that are only counted in the parent block.
// (e.g. stmt, loop, stmt, stmt, if, stmt)
// To make things easier, first create sibling blocks
// for any of these extra stmts.
// Could do this when first create blocks, but then may waste memory
// with a lot of extra blocks.
// Warning:  may have artificial blocks like switch blocks.
static void
Expand_Sibling_Blocks (ORI_BLOCK *first)
{
	ORI_BLOCK *parent = OB_parent(first);
	ORI_BLOCK *sibling, *prev;
	WN *first_stmt, *stmt;
	WN *container;
	INT num_bbs, num_stmts;

	if (WN_opcode(OB_tree(parent)) == OPC_IF) {
		// if already has expanded form with then/else under it
		OB_expanded(first) = TRUE;
		return;
	}
	if (Trace_ORI) fprintf(TFile, "ori: expand siblings of %d\n", OB_id(first));

	// get first stmt and containing tree in the block
	Get_First_Stmt_And_Container (parent, &first_stmt, &container);
	stmt = first_stmt;
	sibling = first;
	prev = NULL;
	num_bbs = num_stmts = 0;
	while (stmt != NULL) {
		Count_WN_Node (stmt, &num_bbs, &num_stmts);
		if (sibling && OB_tree(sibling) == stmt) {
			// reached a nested block.
			if (stmt != first_stmt) {
				// create new block
				Create_New_Child (parent, prev, sibling, 
					first_stmt, WN_prev(stmt), 
					num_bbs, num_stmts);
			}
			num_bbs = num_stmts = 0;
			// skip over all of sibling's stmts
			while (stmt != OB_end_tree(sibling)) stmt = WN_next(stmt);
			first_stmt = WN_next(stmt);
			prev = sibling;
			sibling = OB_sibling(sibling);
		}
		// compgoto case means end-tree of blocks not necessarily 
		// end of wn block.
		if (stmt == OB_end_tree(parent)) {
			if (first_stmt == stmt || first_stmt == WN_next(stmt))
				// don't expand any further
				first_stmt = NULL;
			break;
		}
		stmt = WN_next(stmt);
	}
	if (sibling) DevWarn("ORI: nested block not found when expanding siblings for %d", OB_id(sibling));
	if (first_stmt != NULL) {
		// create new block.
		// if stmt not null, then is last in compgoto block;
		// else find last in container.
		if (stmt == NULL) {
		    for (stmt = first_stmt; WN_next(stmt) != NULL; stmt = WN_next(stmt))
			;	// finding last stmt
		}
		Create_New_Child (parent, prev, NULL, 
			first_stmt, stmt, num_bbs, num_stmts);
	}
	OB_expanded(OB_child(parent)) = TRUE;
	if (Trace_Blocks) Print_Ori_Block (TFile, parent);
}

// Create a merged block that becomes parent block over blocks from
// start to last, inserted between before_start and after_last.
static ORI_BLOCK *
Create_Merged_Block (ORI_BLOCK *start, ORI_BLOCK *last, ORI_BLOCK *before_start, ORI_BLOCK *after_last)
{
	ORI_BLOCK *b;
	b = New_Ori_Block (OB_tree(start), OB_parent(start));
	OB_end_tree(b) = OB_end_tree(last);
	OB_container(b) = OB_container(start);
	OB_labels(b) = OB_labels(start);	// propagate beginning labels
	OB_expanded(b) = TRUE;			// so don't re-expand
	ORI_BLOCK *tmp;
	for (tmp = start; tmp != after_last; tmp = OB_sibling(tmp)) {
		OB_num_bbs(b) += OB_num_bbs(tmp);
		OB_num_stmts(b) += OB_num_stmts(tmp);
		OB_parent(tmp) = b;
	} 
	OB_child(b) = start;
	OB_sibling(b) = after_last;
	OB_sibling(last) = NULL;
	if (before_start)
		OB_sibling(before_start) = b;
	else
		OB_child(OB_parent(b)) = b;
	if (Trace_ORI) fprintf(TFile, "ori: merge blocks %d to %d into %d\n", OB_id(start), OB_id(last), OB_id(b));
	return b;
}

// Check that label is referenced, and that branches are at same level.
// This simplifies the assumptions that we can later make.
static void
Update_Label_Info (ORI_BLOCK *b)
{
	WNLIST *labs;
	INT lnum;
	INT label_level = OB_level(b);

	for (labs = OB_labels(b); labs != NULL; labs = WNLIST_next(labs)) {
		if (WN_opcode(WNLIST_item(labs)) != OPC_LABEL)
			// can be altentry or exc_scope node,
			// which can never be removed.
			continue;
		lnum = WN_label_number(WNLIST_item(labs));
		if (label_branches[lnum] == NULL) {
			// no branches to label, so ignore
			Remove_Wn_From_List (WNLIST_item(labs), &OB_labels(b));
			if (Trace_Merging) fprintf(TFile, "ori: label %d is unused\n", lnum);
		}
		else {
			OBLIST *branches;
			ORI_BLOCK *branch, *new_branch;
			INT branch_level;
			branches = label_branches[lnum];
			for (; branches != NULL; branches = OBLIST_next(branches)) {
				branch = OBLIST_item(branches);
				// ensure that branch at same
				// level as label
				branch_level = OB_level(branch);
				INT i;
				i = branch_level - label_level;
				if (i == 0) continue;	// okay
				// else propagate branch info upwards
				new_branch = branch;
				for (; i > 0; i--) {
					new_branch = OB_parent(new_branch);
				}
				// remove and add so duplicates are merged
				Remove_Ob_From_List (branch, &label_branches[lnum]);
				Add_Ob_To_List (new_branch, &label_branches[lnum]);
			}
		}
	}
}

// try to merge across labels by combining all branch blocks with label block.
static BOOL
Merge_Across_Labels (ORI_BLOCK **first, INT32 olimit)
{
	ORI_BLOCK *cur = *first;
	BOOL did_merge = FALSE;
	while (cur != NULL) {
	    if (OB_labels(cur)) {
		Update_Label_Info(cur);
		if (OB_labels(cur) == NULL) {
			// label must have been deleted
			did_merge = TRUE;	// so try again
			continue;
		}
		else if (WNLIST_next(OB_labels(cur))) {
// ??? Currently always start new block for new labels, so this never happens
			// give up if > 1 label (possible TODO for later)
			if (Trace_Merging) fprintf(TFile, "ori: too many labels at block %d\n", OB_id(cur));
			continue;
		}
		else if (WN_opcode(WNLIST_item(OB_labels(cur))) != OPC_LABEL) {
			// altentry or exc_scope node can never be merged
			continue;
		}

		// else is a single label
		INT lnum;
		lnum = WN_label_number(WNLIST_item(OB_labels(cur)));
		if (OBLIST_next(label_branches[lnum])) {
			// give up if > 1 branch 
			// (possible TODO for later)
			if (Trace_Merging) fprintf(TFile, "ori: too many branches to label %d\n", lnum);
		}
		else {
			// exactly one branch
			ORI_BLOCK *branch;
			branch = OBLIST_item(label_branches[lnum]);
			if (Trace_Merging) fprintf(TFile, "ori: try to merge label %d in block %d with branch in block %d\n", lnum, OB_id(cur), OB_id(branch));
			INT num_bbs = OB_num_bbs(branch);
			INT num_stmts = OB_num_stmts(branch);
			ORI_BLOCK *b;
			for (b = OB_sibling(branch); b != NULL && b != cur; b = OB_sibling(b)) {
				if (OB_never_in_region(b))
					break;	// can't merge this
				if (OB_labels(b)) 
					break;	// intermediate label
				num_bbs += OB_num_bbs(b);
				num_stmts += OB_num_stmts(b);
				if (COMPUTE_OLIMIT(num_bbs,num_stmts) > olimit)
					break;	// too big
			}
			if (b == cur) {
				// add final size
				num_bbs += OB_num_bbs(b);
				num_stmts += OB_num_stmts(b);
			}
			if (b == cur && !OB_never_in_region(branch)
			    && (COMPUTE_OLIMIT(num_bbs,num_stmts) <= olimit) )
			{
				// can safely merge the blocks together
				cur = Create_Merged_Block (branch, cur, 
					OB_prev(branch), OB_sibling(cur));
				if (branch == *first) *first = cur;
				did_merge = TRUE;
				// once we do one label merge,
				// we retry normal merge to see
				// what other opportunities arise,
				// and to avoid merging too much.
				return TRUE;
			}
		}
	    }
	    cur = OB_sibling(cur);
	}
	return did_merge;
}

// Merge_Blocks should create combined parent blocks around 2 or more
// sibling blocks.  Are passed the first child/sibling.
// Cannot merge blocks if an inner block has a label that will be
// entered from outside the merged block.
static void
Merge_Blocks (ORI_BLOCK *first, INT32 olimit)
{
	ORI_BLOCK *parent = OB_parent(first);
	ORI_BLOCK *start = first;
	ORI_BLOCK *cur = first;
	ORI_BLOCK *prev = NULL;
	ORI_BLOCK *prev_start= NULL;
	ORI_BLOCK *b;
	INT num_bbs = 0;
	INT num_stmts = 0;
	BOOL merge_previous = FALSE;
	BOOL did_merge = FALSE;
	if (Trace_Merging) fprintf(TFile, "ori: must merge siblings of %d\n", OB_id(first));
	while (cur != NULL) {
		if (COMPUTE_OLIMIT(num_bbs+OB_num_bbs(cur),num_stmts+OB_num_stmts(cur)) > olimit) {
			merge_previous = TRUE;
		}
		if (OB_labels(cur)) {
			// can't merge this block
			if (Trace_Merging) fprintf(TFile, "ori: can't merge block %d\n", OB_id(cur));
			merge_previous = TRUE; // try to merge prev blocks
		}
		if (OB_never_in_region(cur)) {
			// don't put in merged block.
			merge_previous = TRUE; // try to merge prev blocks
		}
		if (WN_opcode(OB_tree(cur)) == OPC_REGION) {
			// would prefer to not include region blocks
			// in merged blocks, to avoid nesting.
			// But might later need to do this to merge across
			// labels?
			merge_previous = TRUE; // try to merge prev blocks
		}
		if (merge_previous) {
			if (prev && start != prev) {
				// create merged block
				b = Create_Merged_Block (start, prev, 
					prev_start, cur);
				did_merge = TRUE;
				prev = b;	// for prev_start
			}
			prev_start = prev;
			start = cur;
			num_bbs = num_stmts = 0;
			merge_previous = FALSE;
			while (cur != NULL && (OB_never_in_region(cur)
				|| (WN_opcode(OB_tree(cur)) == OPC_REGION)) )
			{
				// don't put at start of block.
				prev_start = cur;
				cur = OB_sibling(cur);
				start = cur;
				prev = cur;
			}
			if (cur == NULL) break;
		}
		num_bbs += OB_num_bbs(cur);
		num_stmts += OB_num_stmts(cur);
		prev = cur;
		cur = OB_sibling(cur);
	}
	if (start != prev && start != first) {
		// merge last blocks
		// (unless is all blocks, which can happen if pu too small).
		b = Create_Merged_Block (start, prev, prev_start, NULL);
		did_merge = TRUE;
	}

	if (!did_merge) {
		did_merge = Merge_Across_Labels (&first, olimit);
	}
	if (!did_merge) {
		// don't try to merge again
		OB_no_more_merge(OB_child(parent)) = TRUE;
		if (Trace_ORI) {
			fprintf(TFile, "ori: couldn't merge anything\n");
			Print_Ori_Block (TFile, parent);
		}
	}
	if (Trace_Blocks) Print_Ori_Block (TFile, parent);
}

// recursively build list of blocks in region
// return false if find a never_in_region block
static BOOL
Build_Blocks_In_Region_List (ORI_BLOCK *b, WNLIST *entry_label)
{
	if (b == NULL) return TRUE;
	if (last_block_in_region_index >= max_block_in_region_index) {
		// realloc
		FmtAssert(last_id > max_block_in_region_index, ("ORI overflow in Build_Blocks_In_Region"));
		DevWarn("ORI: had to realloc");
		blocks_in_region = TYPE_MEM_POOL_REALLOC_N (
			UINT32, &Ori_pool, blocks_in_region,
				max_block_in_region_index, last_id);
		labels_in_region = TYPE_MEM_POOL_REALLOC_N (
			WNLIST *, &Ori_pool, labels_in_region,
			max_block_in_region_index, last_id);
		max_block_in_region_index = last_id;
	}
	blocks_in_region[last_block_in_region_index++] = OB_id(b);
	if (OB_labels(b) != NULL && OB_labels(b) != entry_label
		&& WN_opcode(WNLIST_item(OB_labels(b))) == OPC_LABEL)
	{
		labels_in_region[last_label_in_region_index++] = OB_labels(b);
	}
	if (OB_never_in_region(b)) {
		if (Trace_ORI) fprintf(TFile, "ori:  region contains never block %d\n", OB_id(b));
		return FALSE;
	}
	BOOL okay = TRUE;
	okay = Build_Blocks_In_Region_List (OB_child(b), entry_label);
	okay &= Build_Blocks_In_Region_List (OB_sibling(b), entry_label);
	return okay;
}

// check whether branch block id is in list of blocks_in_region
static BOOL
Branch_In_Region_List (UINT32 branch_id)
{
	INT i;
	for (i = 0; i < last_block_in_region_index; i++) {
		if (branch_id == blocks_in_region[i])
			return TRUE;
	}
	return FALSE;
}

// check that any nested labels (other than labels at head of region)
// do not have goto's from outside the proposed region.
static BOOL
Region_Is_Illegal (ORI_BLOCK *b)
{
	if (OB_illegal(b)) return TRUE;

	// iterate over blocks,
	// building list of blocks in region,
	// and list of inner labels.
	// then check label_branches for each inner label,
	// that sources are in the list of region blocks.
	if (blocks_in_region == NULL) {
		max_block_in_region_index = last_id + 20;
		blocks_in_region = (UINT32 *) MEM_POOL_Alloc (&Ori_pool, 
			sizeof(UINT32) * max_block_in_region_index);
		labels_in_region = (WNLIST **) MEM_POOL_Alloc (&Ori_pool, 
			sizeof(WNLIST *) * max_block_in_region_index);
	}
	last_block_in_region_index = 0;
	last_label_in_region_index = 0;

	blocks_in_region[last_block_in_region_index++] = OB_id(b);
	BOOL okay;
	okay = Build_Blocks_In_Region_List (OB_child(b), OB_labels(b));
	if (!okay) {
		OB_illegal(b) = TRUE;
		return TRUE;
	}

	INT i;
	INT lnum;
	INT branch_id;
	OBLIST *branches;
	for (i = 0; i < last_label_in_region_index; i++) {
		lnum = WN_label_number(WNLIST_item(labels_in_region[i]));
		branches = label_branches[lnum];
		for (; branches != NULL; branches = OBLIST_next(branches)) {
			branch_id = OB_id(OBLIST_item(branches));
			if ( ! Branch_In_Region_List(branch_id)) {
				if (Trace_ORI) fprintf(TFile, "branch in %d to label %d is outside region\n", branch_id, lnum);
				OB_illegal(b) = TRUE;
				return TRUE;
			}
			// else continue
		}
	}
	return FALSE;
}

// enumerate the different cases of block sizes
typedef enum {TOO_SMALL, JUST_RIGHT, TOO_BIG} BLOCK_SIZE;

#define MIN_BLOCK_SIZE(olimit)	(olimit / 4)

// does block have size that matches olimit restrictions?
static BLOCK_SIZE
Olimit_Size (ORI_BLOCK *b, INT32 olimit)
{
	INT32 minsize = MIN_BLOCK_SIZE(olimit);
	if (OB_size(b) > olimit) 
		return TOO_BIG;
	else if (OB_size(b) < minsize) 
		return TOO_SMALL;
	// check that remaining parent block will not be too small.
	else if (b != pu_block && OB_sub_size(pu_block,b) < minsize) 
		return TOO_BIG;
	else if (OB_never_in_region(b) || OB_illegal(b))
		return TOO_SMALL;	// pretend too small
	else 
		return JUST_RIGHT;
}

// Find largest-sized block
static ORI_BLOCK *
Find_Largest_Block (ORI_BLOCK *first)
{
	ORI_BLOCK *b;
	ORI_BLOCK *tmp;
	ORI_BLOCK *max = NULL;
	INT32 maxsize = 0;
	for (b = first; b != NULL; b = OB_sibling(b)) {
		if (OB_never_in_region(b)) {
			// don't want to ever put region around this
			continue;
		}
		else if (OB_illegal(b)) {
			if (OB_size(b) <= maxsize) {
				continue;	// ignore
			} else {
				// look inside illegal region
				tmp = Find_Largest_Block(OB_child(b));
			}
		}
		else {
			tmp = b;
		}
		if (tmp != NULL && OB_size(tmp) > maxsize) {
			maxsize = OB_size(tmp);
			max = tmp;
		}
	}
	if (max == NULL) {
		DevWarn("ORI: size %d > olimit, but can't create anymore regions", 
			OB_size(OB_parent(first)));
		if (Trace_ORI) fprintf(TFile, "ori:  couldn't find any nonzero blocks under block %d\n", OB_id(OB_parent(first)));
		return NULL;
	}
	if (Region_Is_Illegal(max)) {
		/* try again; this time block will be marked as illegal */
		max = Find_Largest_Block(first);
	}
	return max;
}

// If can't merge blocks, then forced to create smaller regions
// until remaining parent block is small enough.
// We do this by finding the largest possible sub-blocks
// and putting regions around those first.
static BOOL 
Insert_Smaller_Regions (ORI_BLOCK *parent, INT32 olimit)
{
	ORI_BLOCK *b;
	while (Olimit_Size(parent,olimit) == TOO_BIG) {
		// could be too-big cause pu too small
		if (OB_size(pu_block) <= olimit) break;
		b = Find_Largest_Block(OB_child(parent));
		if (b == NULL) return FALSE;
		if (Trace_ORI) fprintf(TFile, "insert smaller region at block %d of size %d\n", OB_id(b), OB_size(b));
		Insert_Region_Around_Block (b);
	}
	return TRUE;
}

// Choose the best block for a region
static ORI_BLOCK *
Choose_Region_Block (ORI_BLOCK *b, INT32 olimit, BLOCK_SIZE *bs)
{
	*bs = Olimit_Size (b, olimit);
	if (*bs == JUST_RIGHT && Region_Is_Illegal(b)) {
		// if region has entry point in middle, then is illegal;
		// so mark as too small and hope to merge into legal region.
		if (Trace_ORI) fprintf(TFile, "ori: illegal region %d\n", OB_id(b));
		DevWarn ("ORI: wanted to create illegal region");
		*bs = TOO_SMALL;
	}
	switch (*bs) {
	case JUST_RIGHT:
		// Normally want region to be as big as possible,
		// but if are nested regions then prefer new regions
		// to be parallel to other region rather than nested
		// around it (because nesting causes boundary info to grow).
// ??? How to do this???
		if (OB_nested_region(b)) {
			ORI_BLOCK *kid = OB_child(b);
			for ( ; kid != NULL; kid = OB_sibling(kid)) { 
				if (WN_opcode(OB_tree(kid)) != OPC_REGION
			  	    && Olimit_Size(kid, olimit) == JUST_RIGHT)
					return kid;
			}
			// else merge children?
		}
		return b;
	case TOO_BIG:
		FmtAssert(WN_opcode(OB_tree(b)) != OPC_EXC_SCOPE_BEGIN,
			("Exception scope is bigger than region olimit size"));
		if (OB_child(b)) {
			return Choose_Region_Block (OB_child(b), olimit, bs);
		}
		else {
			Split_Block (b);
			return Choose_Region_Block (b, olimit, bs);
		}
	case TOO_SMALL:
		// Note that parent must be too big
		if (OB_sibling(b)) {
			return Choose_Region_Block (OB_sibling(b), olimit, bs);
		}
		else {
			if (OB_expanded(OB_first_sibling(b))) {
				// all siblings too small, so merge
				Merge_Blocks (OB_first_sibling(b), olimit);
				if (OB_no_more_merge(OB_first_sibling(b))
			  	    && Olimit_Size(OB_parent(b), olimit) == TOO_BIG)
				{
					// if no more merging possible,
					// insert smaller regions.
					*bs = TOO_BIG;	// parent size
					return OB_parent(b);
				}
			}
			else {
				// setup sibling blocks.
				Expand_Sibling_Blocks (OB_first_sibling(b));
			}
			return Choose_Region_Block (OB_parent(b), olimit, bs);
		}
	default:
		// should never reach here
		return NULL;
	}
}

extern WN*
Olimit_Region_Insertion (WN *pu_tree, INT32 olimit)
{
	ORI_BLOCK *b;
	BLOCK_SIZE size;
	INT pu_size;
	Set_Error_Phase("ORI");
	Start_Timer(T_ORI_CU);
	Initialize_Trace_Flags();

	if (PU_has_alloca(Get_Current_PU())) {
		DevWarn("ORI: has alloca, so don't create regions");
		return pu_tree;
	}
	if (PU_has_namelist(Get_Current_PU())) {
		DevWarn("ORI: has namelist, so don't create regions");
		return pu_tree;
	}
	if (PU_has_mp(Get_Current_PU())) {
		DevWarn("ORI: has MP, so don't create regions");
		return pu_tree;
	}
	if (PU_has_exc_scopes(Get_Current_PU()))
	  {
	    // need region around catches as well as try,
	    // so don't create any regions if has exception scopes.
	    DevWarn("ORI: has exception scopes, so don't create regions");
	    return pu_tree;
	  }

	MEM_POOL_Initialize (&Ori_pool, "ORI_pool", FALSE);
	MEM_POOL_Push (&Ori_pool);

	if (Run_Goto_Conversion)
	{
	  // goto conversion
	  GOTO_TABLE goto_table( pu_tree, &Ori_pool);
	  goto_table.Remove_Gotos();
	  if (Trace_Goto_Conversion) goto_table.Print(TFile);
	  // goto_table must be destructed before we pop the pool
	  Check_Dump (pu_tree, "After ORI goto conversion:");
	}

	Initialize_ORI();	// must be after goto conversion

	// build ori blocks
	pu_block = Build_Ori_Blocks (pu_tree, olimit);
	Propagate_Child_Info (pu_block);
	pu_size = OB_size(pu_block);
	if (Trace_ORI) Print_Ori_Block (TFile, pu_block);	// initial structure

	// find appropriate blocks for regions.
	// iterate, eliminating new regions, until pu is small enough.
	while (OB_size(pu_block) > olimit) {
		b = Choose_Region_Block (pu_block, olimit, &size);
		// Either we find a block for a region, 
		// or the block is too big 
		// and all its children are too small.  
		// In the latter case, just create smaller regions around
		// the largest children until the parent is a good size.
		if (size == JUST_RIGHT) {
			Insert_Region_Around_Block (b);
		}
		else {
			if (!Insert_Smaller_Regions (b, olimit))
				break;
		}
		if (Trace_ORI) Print_Ori_Block (TFile, pu_block);
	}

	if (num_regions > 0) {
        	ErrMsg (EC_ORI_Invoked, ST_name(WN_st(pu_tree)), pu_size);
        	DevWarn("splitting function %s into %d regions", 
			ST_name(WN_st(pu_tree)), num_regions);
	}
	else {
		DevWarn("ORI invoked, but no regions created");
	}
	Check_Dump (pu_tree, "After ORI region insertion:");

	MEM_POOL_Pop (&Ori_pool);
	MEM_POOL_Delete (&Ori_pool);
	Stop_Timer(T_ORI_CU);
	return pu_tree;
}

/*
Here's a brief outline of the ORI phase:

ORI:  Olimit Region Insertion
-----------------------------

only run if ir_reader stats say PU is > olimit

use opt_goto analysis to convert gotos to structured control flow
	this helps with fortran code

build block flow-graph, keep info about size of each block
	two kinds of blocks:
		nested blocks, e.g. if
		label blocks, e.g. goto or switch
	build list of nested blocks and label blocks; each block has:
		whirl pointer
		size (#stmts, #bbs; total = stmts*bbs)
		child, sibling, parent
			(label blocks are siblings of prev block)
		list of gotos out of block & returns
			(so can later replace with region_exits)
		list of labels
	must propagate child info to parent
		e.g. propagate labels and then propagate gotos that
			jump to labels not in label list.

pick insertion point
	regions can only have 1 entry point (but many exits)
		so can't have inner label if outside goto
	ideal:  want all regions < olimit
		want smallest # regions AND largest size for smallest region
		keep loop nests intact
		bunch calls together
		bunch pointers together
	walk down tree:
		good size == < olimit 
			and > 1/n olimit 
			and parent region > 1/n olimit
		if block with good size then
			make region (and adjust parent region size)
		else if too big then
			recurse on child
			if no child then split block
		else if too small then
			(note that parent must be too big)
			recurse on sibling
			if last sibling try merging siblings
				have to check entry points
				find block that is target of only 1 other block
					and merge those
			if no siblings split parent

insert region and region_exit nodes

==============================================================================

Restrictions on regions around exception scopes:
* Cannot have region inside a try block
  (but can be inside a cleanup scope)
* If region around a try block, must also be around handlers of try block.

Optimal:  for region around loops, including init code.
but for if-else, best to leave condition outside of region
(e.g. regions around then and else blocks, not around if).

For merging multiple blocks with labels:
first must find all branches to a label;
can either keep list of such branches per label,
or put list of branches inside ob, then propagate branches
to parent if label not in sibling.
Then must find set of blocks that can be merged.
need label block plus branch blocks to that label,
plus any blocks between those blocks (so is contiguous).
Start with label block:  find branch block, see if any label
blocks between the two blocks; if are then stop, else try next
branch, until all branches are done then have contiguous legal block
or else go on to next label block (will want to recurse, as once
merge label blocks that may open up new possibilities).
C rule:  goto can go anywhere in PU, even to nested level.
This means that branches will not necessarily be sibling of label!
So limit attempts and give up if branch not a sibling.
If label has no branches to it, then ignore label.
Have array indexed by label number that points to ob for each
block that has branch to label.  Then check if levels of branch
and label ob match; if branch deeper, then replace with sibling block.
Only add branch ob to list if not already on list.

Switches:  if can't put region around whole switch,
then could put lots of little regions around each case label.
Or can combine case labels if region also includes jump-table,
and is represented as if goto, not as compgoto.
If were to put region around compgoto and only some labels,
then would have region that crossed scopes:
region-begin
 switch () {
 case 1:
region-end
 case 2:
 }
We don't want front ends to accept this, and if be does it then
whirl2c will show it.  So if did want to do this then have to
lower compgoto to if goto sequence (so never made back into switch).
Or could break switch into multiple switches, except have to worry
about fall-thru branches.

Illegal regions:  ORI may create illegal region, e.g.
region-begin
  loop {
	stmt; L1: stmt
  }
region-end
goto L1;
(pv 454555)
In that case would like region-initialize to remove region rather than
give error that region is illegal.  But then there would be no region,
so instead should try to catch this case and not create region around loop
(unless includes goto).
Or: check for nested labels inside proposed region;
if any found, check label_branches array for any that are outside 
the region; if so, try to merge with those branches; if can't,
declare the block too small.
Idea:  create array (realloced) of ob ids that are in region;
also keep array of label numbers; then check label_branches for
each label number, to see if inside the array of ob ids;
if not, just declare too small and hope merging covers it.

If ori can't create suitable region, should give up and not create region,
and then normal olimit goes into effect; either that or create lots of
little regions....

We put labels at head of region inside the region and consider it a
valid entry point.  Region_init may then want to split that label if
are external goto's to it, since wopt won't guarantee that the internal
label will stay at the head (pv 444186).

pv 457032: block 4 is illegal region, and is size 99.
We pretend too small and try to merge;
we repeatedly merge blocks 93->4->25,
creating a too big block, so we again go to block 4.
One fix is to make sure that we don't merge across labels and create
a too-large block.
Problem is really that we have branch from inner region to middle
of parent region, and we think that is illegal
(don't see blocks inside nested region).

cotoken100: inside insert_smaller, stays too_big because pu is just
2 > parent, so parent fits (100) but then pu too small; need to
switch to region inside illegal while.
also, bb count is wrong: 48 vs 47.

To make sure code before preamble-end is not part of a region,
could remove that code from func-entry, and create dummy block
marked never-in-region as containing that code.
*/
