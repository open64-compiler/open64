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
//                     Deal with the array dependence graph during MP lowering
//                     -------------------------------------------------------
//
//
//

/* ====================================================================
 * ====================================================================
 *
 * Module: wn_mp_dg.cxx  
 * $Revision: 1.1 $
 *
 * Revision history:
 *  dd-mmm-94 - Original Version
 *
 * Description: array dependence graph and mp
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: /depot/CVSROOT/javi/src/sw/cmplr/be/com/wn_mp_dg.cxx,v $ $Revision: 1.1 $";
#endif /* _KEEP_RCS_ID */

#include <sys/types.h>
#include <elf.h>

#define USE_STANDARD_TYPES	    /* override unwanted defines in "defs.h" */

#include <bstring.h>
#include "wn.h"
#include "wn_util.h"
#include "erglob.h"
#include "errors.h"
#include "strtab.h"		    /* for strtab */
#include "symtab.h"		    /* for symtab */
#include "irbdata.h"		    /* for inito */
#include "dwarf_DST_mem.h"	    /* for DST */
#include "pu_info.h"
#include "ir_bwrite.h"
#include "ir_bcom.h"
#include "region_util.h"	    /* for RID */
#include "dep_graph.h"
#include "cxx_hash.h"
#include "wn_mp.h"                  /* for wn_mp_dg.cxx's extern functions */

typedef  HASH_TABLE<VINDEX16,VINDEX16> VV_HASH_TABLE; 
typedef STACK<VINDEX16> V_STACK;
static MEM_POOL MP_Dep_Pool;
static BOOL mp_dep_pool_initialized;
void Create_Vertices(WN *wn, VV_HASH_TABLE *parent_to_child,
		V_STACK *parent_vertices,
		ARRAY_DIRECTED_GRAPH16 *parent_graph,
		ARRAY_DIRECTED_GRAPH16 *child_graph);

// Fix up the array dependence graph during MP lowering
void MP_Fix_Dependence_Graph(PU_Info *parent_pu_info,
				PU_Info *child_pu_info, WN *child_wn)
{
  ARRAY_DIRECTED_GRAPH16 *parent_graph = 
     (ARRAY_DIRECTED_GRAPH16 *) PU_Info_depgraph_ptr(parent_pu_info);
  if (!parent_graph) { // no parent, no child
    Set_PU_Info_depgraph_ptr(child_pu_info,NULL);
    return;
  } 
  if (!mp_dep_pool_initialized) {
    MEM_POOL_Initialize(&MP_Dep_Pool,"MP_Dep_Pool",FALSE);
    mp_dep_pool_initialized = TRUE;
  }
  MEM_POOL_Push(&MP_Dep_Pool);

  // Create a new dependence graph for the child region
  ARRAY_DIRECTED_GRAPH16 *child_graph  = 
	    CXX_NEW(ARRAY_DIRECTED_GRAPH16(100, 500,
		WN_MAP_DEPGRAPH, DEP_ARRAY_GRAPH), Malloc_Mem_Pool);
  Set_PU_Info_depgraph_ptr(child_pu_info,child_graph);
  Set_PU_Info_state(child_pu_info,WT_DEPGRAPH,Subsect_InMem);

  // mapping from the vertices in the parent graph to the corresponding 
  // ones in the child
  VV_HASH_TABLE *parent_to_child = 
    CXX_NEW(VV_HASH_TABLE(200,&MP_Dep_Pool),&MP_Dep_Pool);
  // a list of the parent vertices in the region
  V_STACK *parent_vertices = CXX_NEW(V_STACK(&MP_Dep_Pool),&MP_Dep_Pool);
  Create_Vertices(child_wn,parent_to_child,parent_vertices,parent_graph,
							child_graph);

  // copy the edges, erase them from the parent graph
  INT i;
  for (i=0; i<parent_vertices->Elements(); i++) {
    VINDEX16 parent_v = parent_vertices->Bottom_nth(i);
    VINDEX16 child_v = parent_to_child->Find(parent_v);
    Is_True(child_v,("child_v missing "));
    EINDEX16 e;
    while (e = parent_graph->Get_Out_Edge(parent_v)) {
      VINDEX16 parent_sink = parent_graph->Get_Sink(e);
      VINDEX16 child_sink = parent_to_child->Find(parent_sink);
      Is_True(child_sink,("child_sink missing "));
      child_graph->Add_Edge(child_v,child_sink,
		parent_graph->Dep(e),parent_graph->Is_Must(e));

      parent_graph->Remove_Edge(e);
    }
  }
  for (i=0; i<parent_vertices->Elements(); i++) {
    // remove the vertex from the parent graph
    // since removing the vertex cleans the wn map, reset it
    VINDEX16 parent_v = parent_vertices->Bottom_nth(i);
    VINDEX16 child_v = parent_to_child->Find(parent_v);
    WN *wn = parent_graph->Get_Wn(parent_v);
    parent_graph->Delete_Vertex(parent_v);
    child_graph->Set_Wn(child_v,wn);
  }
  CXX_DELETE(parent_to_child,&MP_Dep_Pool);
  CXX_DELETE(parent_vertices,&MP_Dep_Pool);
  MEM_POOL_Pop(&MP_Dep_Pool);
}

// walk the child, find all the vertices, create corresponding vertices
// in the child graph, fill up the hash table and stack
void Create_Vertices(WN *wn, VV_HASH_TABLE *parent_to_child,
		V_STACK *parent_vertices,
		ARRAY_DIRECTED_GRAPH16 *parent_graph,
		ARRAY_DIRECTED_GRAPH16 *child_graph)
{
  OPCODE opcode = WN_opcode(wn);
  if (opcode == OPC_BLOCK) {
    WN *kid = WN_first (wn);
    while (kid) {
      Create_Vertices(kid,parent_to_child,parent_vertices,parent_graph,
							child_graph);
      kid = WN_next(kid);
    }
    return;
  } 
  if (OPCODE_is_load(opcode) || OPCODE_is_store(opcode) 
      || OPCODE_is_call(opcode)) {
    VINDEX16 parent_v = parent_graph->Get_Vertex(wn);
    if (parent_v) {
      // can't overflow since parent graph has 
      // at least the same number of vertices
      VINDEX16 child_v = child_graph->Add_Vertex(wn);
      parent_to_child->Enter(parent_v,child_v);
      parent_vertices->Push(parent_v);
    }
  } 
  for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
    Create_Vertices(WN_kid(wn,kidno),parent_to_child,parent_vertices,
					parent_graph,child_graph);
  }
}


/*
Transform "lastprivate" non-POD finalization code from this:

#pragma omp for
  for (...) {
    ...
    if (__omp_non_pod_lastlocal) {
      ... // finalization code
    }
  }

to this:

#pragma omp for
 {
  for (...) {
    ...
  }
  ... // finalization code
 }

Note that due to pseudolowering, the lastprivate IF could appear deeper
in the Whirl tree than in this example, but in no case will it be beneath a
PDO region.

The block argument must be either the body_block of the PDO region, or (for
the serial version of MP code) a BLOCK that contains the DO_LOOP that in
turn contains the non-POD lastprivate IF. If the block doesn't contain a
DO_LOOP or the DO_LOOP body doesn't contain a non-POD lastprivate IF, the
block is left unchanged. Because the serial version of MP code can contain
serial DO loops before any serialized PDOs or multiple serialized PDOs, we
do this tranformation on every outermost loop we find in block
*/

static WN *Find_Non_POD_Finalization_Code(WN *wn, WN **final_if_parent);
static void Move_Non_POD_Finalization_Code_Rec(WN *wn);
static void Find_And_Move_Finalization_Code(WN *parent, WN *do_wn);

void Move_Non_POD_Finalization_Code(WN *block)
{
  Is_True(block && WN_operator(block) == OPR_BLOCK, ("bad block"));
  Move_Non_POD_Finalization_Code_Rec(block);
}

// This routine does all the recursion. We don't recurse into DO loops.
static void Move_Non_POD_Finalization_Code_Rec(WN *wn)
{
  Is_True(wn, ("NULL wn"));
  Is_True(WN_operator(wn) != OPR_DO_LOOP, ("recursed into a DO loop"));

  if (WN_operator(wn) == OPR_BLOCK) {
    for (WN *stmt = WN_first(wn); stmt; stmt = WN_next(stmt)) {
      if (WN_operator(stmt) == OPR_DO_LOOP)
        Find_And_Move_Finalization_Code(wn, stmt);
      else
        Move_Non_POD_Finalization_Code_Rec(stmt);
    }

  } else {
    for (INT kidno = 0; kidno < WN_kid_count(wn); kidno++)
      Move_Non_POD_Finalization_Code_Rec(WN_kid(wn, kidno));
  }
}

static void Find_And_Move_Finalization_Code(WN *parent, WN *do_wn)
{
  Is_True(parent && WN_operator(parent) == OPR_BLOCK, ("bad parent"));
  Is_True(do_wn && WN_operator(do_wn) == OPR_DO_LOOP, ("bad do_wn"));

  WN *final_if, *final_if_parent, *do_body = WN_do_body(do_wn);

  final_if = Find_Non_POD_Finalization_Code(do_body, &final_if_parent);

  if (!final_if)
    return;

    // remove mem barriers around the IF
  WN *bar1 = WN_prev(final_if), *bar2 = WN_next(final_if),
     *then = WN_then(final_if), *bar3 = WN_first(then),
     *bar4 = WN_last(then);
  Is_True(bar1 && WN_operator(bar1) == OPR_FORWARD_BARRIER,
          ("bad bar1"));
  Is_True(bar2 && WN_operator(bar2) == OPR_BACKWARD_BARRIER,
          ("bad bar2"));
  Is_True(bar3 && WN_operator(bar3) == OPR_BACKWARD_BARRIER,
          ("bad bar3"));
  Is_True(bar4 && WN_operator(bar4) == OPR_FORWARD_BARRIER,
          ("bad bar4"));
  WN_DELETE_FromBlock(final_if_parent, bar1);
  WN_DELETE_FromBlock(final_if_parent, bar2);
  WN_DELETE_FromBlock(then, bar3);
  WN_DELETE_FromBlock(then, bar4);

    // move finalization code after the DO loop
  WN_EXTRACT_FromBlock(final_if_parent, final_if);
  WN *final_code = WN_then(final_if);
  WN_then(final_if) = NULL;
  WN_DELETE_Tree(final_if);
  WN_INSERT_BlockAfter(parent, do_wn, final_code);
}


static WN *Find_Non_POD_Finalization_Code(WN *wn, WN **final_if_parent)
{
  Is_True(wn, ("NULL wn"));
  if (WN_operator(wn) == OPR_BLOCK) {
      // Search block for final_if. Recurse into all other statements,
      // except PDO regions.
    for (WN *stmt = WN_first(wn); stmt; stmt = WN_next(stmt)) {
      BOOL first_and_last;
      if (Is_Nonpod_Finalization_IF(stmt, &first_and_last)) {
        *final_if_parent = wn;
        return stmt;
      }

      if (WN_operator(stmt) == OPR_REGION &&
          WN_first(WN_region_pragmas(stmt)) &&
          WN_pragma(WN_first(WN_region_pragmas(stmt))) == WN_PRAGMA_PDO_BEGIN)
        continue; // don't recurse into PDO region

        // recuse into stmt
      WN *retval = Find_Non_POD_Finalization_Code(stmt, final_if_parent);
      if (retval)
        return retval;
    }

  } else {  // Recurse into all kids.
    for (INT kidno = 0; kidno < WN_kid_count(wn); kidno++) {
      WN *retval = Find_Non_POD_Finalization_Code(WN_kid(wn, kidno),
                                                  final_if_parent);
      if (retval)
        return retval;
    }
  }

  return NULL;  // didn't find it
}


/*  Copy a tree skipping all MP pragma nodes (except CRITICAL SECTION's).  */
/*  Copy the CG dependence graph if it exists */
/*  Move fecc-generated finalization code for non-POD "lastprivate" vars
    to the appropriate place */

static WN * Copy_Non_MP_Tree_Rec ( WN * tree , V_STACK *mp_vertices,
					VV_HASH_TABLE *mp_to_nonmp);

WN * Copy_Non_MP_Tree( WN * tree )
{
  V_STACK *mp_vertices=0;
  VV_HASH_TABLE *mp_to_nonmp = 0;
  if (Current_Dep_Graph) {
    if (!mp_dep_pool_initialized) {
      MEM_POOL_Initialize(&MP_Dep_Pool,"MP_Dep_Pool",FALSE);
      mp_dep_pool_initialized = TRUE;
    }
    MEM_POOL_Push(&MP_Dep_Pool);
    mp_vertices = CXX_NEW(V_STACK(&MP_Dep_Pool),&MP_Dep_Pool);
    mp_to_nonmp = CXX_NEW(VV_HASH_TABLE(200,&MP_Dep_Pool),&MP_Dep_Pool);
  }
  WN *result = Copy_Non_MP_Tree_Rec(tree,mp_vertices,mp_to_nonmp);

  // copy the edges
  if (Current_Dep_Graph) {
    for (INT i=0; i<mp_vertices->Elements(); i++) {
      VINDEX16 mp_v = mp_vertices->Bottom_nth(i);
      VINDEX16 nonmp_v = mp_to_nonmp->Find(mp_v);
      Is_True(nonmp_v,("nonmp_v missing "));
      EINDEX16 e = Current_Dep_Graph->Get_Out_Edge(mp_v);
      while (e) {
        VINDEX16 mp_sink = Current_Dep_Graph->Get_Sink(e);
        VINDEX16 nonmp_sink = mp_to_nonmp->Find(mp_sink);
        Is_True(nonmp_sink,("nonmp_sink missing "));
        if ((nonmp_v != nonmp_sink) || !
			Current_Dep_Graph->Get_Edge(nonmp_v,nonmp_v)) {
          if (!Current_Dep_Graph->Add_Edge(nonmp_v,nonmp_sink,
		Current_Dep_Graph->Dep(e),Current_Dep_Graph->Is_Must(e))) {
	    Current_Dep_Graph->Erase_Graph();
	    Current_Dep_Graph = NULL;
	    Set_PU_Info_depgraph_ptr(Current_PU_Info,Current_Dep_Graph);
	    Set_PU_Info_state(Current_PU_Info,WT_DEPGRAPH,Subsect_InMem);

	    return result;
          }
        }
	e = Current_Dep_Graph->Get_Next_Out_Edge(e);
      }
    }
  }
  return result;
}

static WN * Copy_Non_MP_Tree_Rec ( WN * tree , V_STACK *mp_vertices,
					VV_HASH_TABLE *mp_to_nonmp)
{
  INT32 kidno;
  ST *lock_st;
  WN *new_wn;
  WN *kid;
  WN *prev_kid;
  WN *next_kid;
  WN *new_kid;
#define STACK_CHUNK 10
  INT32 *spr_stack = NULL;      /* serialized parallel region?
                                 *      No? 0
                                 *      yes -- parallel region? 1
                                 *      yes -- doacross?        2
                                 */
  WN **kid_stack = NULL;
  INT32 kptr = 0;               /* index into kid_stack and spr_stack */
  INT32 kptr_max = 0;           /* size of kid_stack */
  ST **lock_stack = NULL;
  INT32 lptr = 0;               /* index into lock_stack */
  INT32 lptr_max = 0;           /* size of lock_stack */
                                /* need to call
                                   Move_Non_POD_Finalization_Code() on
                                   copied version of code */
  BOOL must_move_non_pod = FALSE;

  if (tree == NULL)
    return (NULL);

#ifdef Is_True_On
  if (WN_opcode(tree) == OPC_REGION) {
    RID *rid = REGION_get_rid(tree);
    Is_True(rid != NULL, ("Copy_Non_MP_Tree_Rec, NULL rid"));
  }
#endif

  new_wn = WN_CopyNode (tree);

  if (Current_Dep_Graph) {
    VINDEX16 mp_v = Current_Dep_Graph->Get_Vertex(tree);
    if (mp_v) {
      VINDEX16 nonmp_v = Current_Dep_Graph->Add_Vertex(new_wn);
      if (!nonmp_v) {
	Current_Dep_Graph->Erase_Graph();
	Current_Dep_Graph = NULL;
      } else {
        mp_vertices->Push(mp_v);
	mp_to_nonmp->Enter(mp_v,nonmp_v);
      }
    }
  }

  if (WN_opcode(tree) == OPC_BLOCK) {

    prev_kid = new_kid = NULL;
    for (kid = WN_first(tree); kid; kid = next_kid) {
      next_kid = WN_next(kid);
      if (((WN_opcode(kid) == OPC_PRAGMA) ||
	   (WN_opcode(kid) == OPC_XPRAGMA)) &&
	  (WN_pragmas[WN_pragma(kid)].users & PUSER_MP)) {
	/* translate critical section begin & end nodes, ignore all other mp
	   pragma nodes */
	if (WN_pragma(kid) == WN_PRAGMA_CRITICAL_SECTION_BEGIN) {
	  if ((WN_opcode(kid) == OPC_XPRAGMA) &&
	      (WN_operator(WN_kid0(kid)) == OPR_LDA))
	    lock_st = WN_st(WN_kid0(kid));
	  else if ((WN_opcode(kid) == OPC_PRAGMA) && WN_st(kid))
	    lock_st = WN_st(kid);
	  else
	    lock_st = NULL;
          if (lptr == lptr_max) {
            lock_stack = (ST**) MEM_POOL_Realloc (Malloc_Mem_Pool, lock_stack,
                                           sizeof(ST*)*lptr_max,
                                           sizeof(ST*)*(lptr_max+STACK_CHUNK));
            lptr_max += STACK_CHUNK;
          }
	  lock_stack[lptr++] = lock_st;
	  if (lock_st)
	    new_kid = Gen_MP_Getlock ( lock_st );
	  else
	    new_kid = Gen_MP_Setlock ( );
	  WN_prev(new_kid) = prev_kid;
	  if (prev_kid)
	    WN_next(prev_kid) = new_kid;
	  else
	    WN_first(new_wn) = new_kid;
	  prev_kid = new_kid;
	} else if (WN_pragma(kid) == WN_PRAGMA_CRITICAL_SECTION_END) {
	  lock_st = lock_stack[--lptr];
	  if (lock_st)
	    new_kid = Gen_MP_Unlock ( lock_st );
	  else
	    new_kid = Gen_MP_Unsetlock ( );
	  WN_prev(new_kid) = prev_kid;
	  if (prev_kid)
	    WN_next(prev_kid) = new_kid;
	  else
	    WN_first(new_wn) = new_kid;
	  prev_kid = new_kid;
	}
      } // if (((WN_opcode(kid) == OPC_PRAGMA) ...
      else if ((WN_opcode(kid) == OPC_REGION) &&
		 WN_first(WN_region_pragmas(kid)) &&
		 (WN_pragmas[WN_pragma(WN_first(WN_region_pragmas(kid)))].users
								& PUSER_MP)) {

        WN_PRAGMA_ID pragma =
          (WN_PRAGMA_ID) WN_pragma(WN_first(WN_region_pragmas(kid)));
        if (kptr == kptr_max) {
          kid_stack = (WN**) MEM_POOL_Realloc (Malloc_Mem_Pool, kid_stack,
                                         sizeof(WN*) * kptr_max,
                                         sizeof(WN*) * (kptr_max+STACK_CHUNK));
          spr_stack = (INT32*) MEM_POOL_Realloc (Malloc_Mem_Pool, spr_stack,
                                         sizeof(INT32)*kptr_max,
                                         sizeof(INT32)*(kptr_max+STACK_CHUNK));
          kptr_max += STACK_CHUNK;
        }

        spr_stack[kptr] = 0;
        if (WN_pragma_omp(WN_first(WN_region_pragmas(kid))) &&
            (pragma == WN_PRAGMA_DOACROSS ||
             pragma == WN_PRAGMA_PARALLEL_DO ||
             pragma == WN_PRAGMA_PARALLEL_BEGIN)) {

          MP_process_type mpt = (pragma == WN_PRAGMA_PARALLEL_BEGIN ?
                                 MPP_PARALLEL_REGION :
                                 MPP_PARALLEL_DO);
          new_kid = Gen_OMP_Begin_SPR(mpt);
          WN_prev(new_kid) = prev_kid;
          if (prev_kid)
            WN_next(prev_kid) = new_kid;
          else
            WN_first(new_wn) = new_kid;
          prev_kid = new_kid;
          spr_stack[kptr] = (mpt == MPP_PARALLEL_REGION ? 1 : 2);
        } else if (WN_pragma_omp(WN_first(WN_region_pragmas(kid))) &&
                   pragma == WN_PRAGMA_PDO_BEGIN) {
          must_move_non_pod = TRUE;
        }
	// only copy the kids of mp regions
	kid_stack[kptr++] = next_kid;
	next_kid = WN_first(WN_region_body(kid));
      } else {
	// copy everything else
	new_kid = Copy_Non_MP_Tree_Rec ( kid, mp_vertices,mp_to_nonmp );
	WN_prev(new_kid) = prev_kid;
	if (prev_kid)
	  WN_next(prev_kid) = new_kid;
	else
	  WN_first(new_wn) = new_kid;
	prev_kid = new_kid;

	// make a new rid for non-mp region
	if (WN_opcode(kid) == OPC_REGION) {
	  RID *rid = REGION_get_rid(kid);
	  if (!RID_TYPE_mp(rid)) { // not MP region
	      // new_kid (serial code) replaces kid (parallel code) in
	      // parent PU, so new_kid must get kid's RID
	    mUINT32 new_region_id = WN_region_id(new_kid);
	    mUINT32 old_region_id = WN_region_id(kid);

	    WN_set_region_id(new_kid, WN_region_id(kid));
	    REGION_new_wn(new_kid, kid);
	    WN_set_region_id(kid, New_Region_Id());
	    REGION_clone(new_kid, kid, NULL);
	  }
	  
	}
      }
      if ((next_kid == NULL) && kptr) {
        if (spr_stack[kptr-1] != 0) {
          /* generate an end-spr */
          new_kid = Gen_OMP_End_SPR(spr_stack[kptr-1] == 1?
                                    MPP_PARALLEL_REGION :
                                    MPP_PARALLEL_DO);
          WN_prev(new_kid) = prev_kid;
          if (prev_kid)
            WN_next(prev_kid) = new_kid;
          else
            WN_first(new_wn) = new_kid;
          prev_kid = new_kid;
        }

	next_kid = kid_stack[--kptr];
      }

    } // for (kid = WN_first(tree); kid; kid = next_kid)

    if (new_kid)
      WN_next(new_kid) = NULL;
    else
      WN_first(new_wn) = NULL;
    WN_last(new_wn) = new_kid;

    if (must_move_non_pod)
      Move_Non_POD_Finalization_Code(new_wn);

  } // if (WN_opcode(tree) == OPC_BLOCK)
  else {

    for (kidno = 0; kidno < WN_kid_count(tree); kidno++) {
      kid = WN_kid(tree, kidno);
      if (kid)
	WN_kid(new_wn, kidno) = Copy_Non_MP_Tree_Rec ( kid, mp_vertices,
						      mp_to_nonmp );
      else
	WN_kid(new_wn, kidno) = NULL;
    }

  }

  if (lock_stack) MEM_POOL_FREE (Malloc_Mem_Pool, lock_stack);
  if (kid_stack)  MEM_POOL_FREE (Malloc_Mem_Pool, kid_stack);
  if (spr_stack)  MEM_POOL_FREE (Malloc_Mem_Pool, spr_stack);
  return (new_wn);
}
