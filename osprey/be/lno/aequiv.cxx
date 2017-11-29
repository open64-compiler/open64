/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


//                     Equivalencing Local Arrays
//                     --------------------------
//
// Description:
//
//	Equivalence local arrays
//	Two local arrays with non-overlapping live ranges can share
//	the same storage.
//	As we don't yet summarize array accesses, we must consider
//      each array access to access the entire array. Thus there is
//      no kills.  This simplifiies the algorithm.  This fact, combined
//	with the fact that we do plan to do array summaries for release
//	2, make this a semi-throwaway algorithm.
//
//	This algorithm assumes that any two references to the same local
//	array use the same ST pointer.
//	
//	Algorithm:
//
//	Step 1. We walk through the symbol table.  Number and put all the local
//	arrays in a stack and in the _hash_table.  Only continue if there
//	are at least 2 used local arrays.  If there aren't at least
//	2, then there is no point in running the algorithm
//	We sort the symbols according to array size (bigger one firsts).
//      This is a heuristic to help the coloring
//
//	Step 2. Build a control flow graph of the routine.  We take advantage
//	of the graph's use in order to make this simpler.
//	In particular, because there are no kills, any array used in an
//	outer loop, will be live the whole outer loop.  So, we collapse
//	outer loops into single control flow nodes.  This should make our
//	graph much smaller than normal control flow graphs.
//
//	While we build the graph, we set cyclic_bit_vector[v] to be the
//	set of local arrays accesed in the code corresponding to vertex v
//
//	Step 3.  We convert the cfg into an acyclic graph.  Every cycle
//	is collapsed to a single node.  acyclic_bit_vector[v2] is set
//	to the union of the bit vectors from all the nodes that were
//	collapsed into v2.
//
//	Step 4.  We perform "data-flow analysis" to find out which arrays
//	are live in which vertices.  Since our graph is acyclic, this
//	step is a simple 2-pass over the vertices.  In one forward pass
//	we propogate what's already been references.  In one backward
//	pass we propogate what's no longer reference.
//
//      Step 5.  We create a bit vector, _array_bit_vector, 
//	for each variable giving the set of vertices in which array i
//	is live
//
//	Step 6. Greedy coloring algorithm.  Start from the first array,
//	compare every other array to it, if they don't overlap, equivalence
//	them.
//
//	Step 7. Remove stores to unread arrays.  Invalidate the alias info.
//
//
/* ====================================================================
 * ====================================================================
 *
 * Module: aequiv.cxx
 * $Revision: 1.5 $
 * $Date: 05/02/25 12:21:40-08:00 $
 * $Author: kannann@iridot.keyresearch $
 * $Source: be/lno/SCCS/s.aequiv.cxx $
 *
 * Revision history:
 *  dd-mmm-94 - Original Version
 *
 * Description: Equivalence local arrays
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

static char *source_file = __FILE__;
static char *rcs_id = "$Source: be/lno/SCCS/s.aequiv.cxx $ $Revision: 1.5 $";

#include <sys/types.h>
#include "pu_info.h"
#include "lnopt_main.h"
#include "aequiv.h"
#include "stab.h"
#include "lwn_util.h"
#include "stblock.h"
#include "opt_alias_interface.h"
#include "lego_pragma.h"

MEM_POOL AEQUIV_pool;
static BOOL AEQUIV_pool_initialized = FALSE;


void AEQUIV::Equivalence_Arrays()
{
  if (Get_Trace(TP_LNOPT,TT_LNO_AEQUIV)) {
    fprintf(TFile,"Equivalencing local arrays \n");
  }

  if (!AEQUIV_pool_initialized) {
    MEM_POOL_Initialize(_pool, "AEQUIV_pool", FALSE);
    AEQUIV_pool_initialized = TRUE;
  }
  MEM_POOL_Push(_pool);
  _la_stack = CXX_NEW(LOCAL_ARRAY_STACK(_pool),_pool);


  Enter_Locals_Stack();
  if (_la_stack->Elements() == 0) {
    if (Get_Trace(TP_LNOPT,TT_LNO_AEQUIV)) {
      fprintf(TFile,"no local arrays\n");
    }
    CXX_DELETE(_la_stack,_pool);
    MEM_POOL_Pop(_pool);
    return;  
  }
  _la_hash_table = 
    CXX_NEW(LOCAL_ARRAY_HASH_TABLE(2*_la_stack->Elements(),_pool),_pool);
  Enter_Locals_Hash();

  _cyclic_bit_vector = CXX_NEW(BIT_VECTOR_STACK(_pool),_pool);

  if (Build_CFG() == -1) {
    CXX_DELETE(_la_stack,_pool);
    CXX_DELETE(_la_hash_table,_pool); 
    CXX_DELETE(_cfg,_pool);
    MEM_POOL_Pop(_pool);
    return;
  }

  if (_la_stack->Elements() == 1) {
    // can't equivalence anything but may still eliminate write only 
    if (_la_stack->Elements() == 1){
      if (Get_Trace(TP_LNOPT,TT_LNO_AEQUIV)) {
        fprintf(TFile,"Only one local array\n");
      }
      LOCAL_ARRAY_DESC *lad = _la_hash_table->Find(_la_stack->Bottom_nth(0));
      if (!lad->_is_read){
        mBOOL equiv_array = TRUE;
        Update_Code(_func_nd,&equiv_array);
      }
      CXX_DELETE(_la_stack,_pool);
      CXX_DELETE(_la_hash_table,_pool); 
      CXX_DELETE(_cfg,_pool);
      MEM_POOL_Pop(_pool);
      return;
    }
  }

  if (Get_Trace(TP_LNOPT,TT_LNO_AEQUIV)) {
     fprintf(TFile,"The cfg graph is \n"); 
     Print_Graph(TFile,_cfg);
  }

  Set_Acyclic();
  if (Get_Trace(TP_LNOPT,TT_LNO_AEQUIV)) {
     fprintf(TFile,"The acfg graph is \n"); 
     Print_Graph(TFile,_acfg);
  }

  Do_Dataflow();
  if (Get_Trace(TP_LNOPT,TT_LNO_AEQUIV)) {
     fprintf(TFile,"After dataflow the acfg graph is \n"); 
     Print_Graph(TFile,_acfg);
  }

  Set_Array_Bit_Vector();
  if (Get_Trace(TP_LNOPT,TT_LNO_AEQUIV)) {
    fprintf(TFile,"the array bit vector is \n");
    for (INT i=0; i<Num_Arrays(); i++) {
      fprintf(TFile,"a[%d] =",i); 
      _array_bit_vector->Bottom_nth(i)->Print(TFile);
    }
  }

  mBOOL *equivalenced_array = CXX_NEW_ARRAY(mBOOL,Num_Arrays(),_pool);
  if (Do_Color(equivalenced_array)) {
    Update_Code(_func_nd,equivalenced_array);
  }


  CXX_DELETE(_cfg,_pool);
  CXX_DELETE(_acfg,_pool);
  CXX_DELETE_ARRAY(equivalenced_array,_pool); 
  CXX_DELETE(_la_hash_table,_pool); 
  CXX_DELETE(_la_stack,_pool); 
  CXX_DELETE(_cyclic_bit_vector,_pool); 
  CXX_DELETE(_acyclic_bit_vector,_pool); 
  MEM_POOL_Pop(_pool);
}

// Walk the symbol table, enter all the local arrays into the stack 
void AEQUIV::Enter_Locals_Stack()
{
  ST *st;
  INT i;
  FOREACH_SYMBOL (CURRENT_SYMTAB,st,i) {
    if ((ST_class(st) == CLASS_VAR) && !ST_is_not_used(st) &&
	(ST_sclass(st) == SCLASS_AUTO) &&
        !ST_addr_saved(st) && !ST_addr_passed(st) &&
	ST_base_idx(st) == ST_st_idx(st) &&
	!ST_has_nested_ref(st) &&
	(TY_size(ST_type(st)) > 0) &&
	(!ST_is_initialized(st)) &&
        (!ST_is_reshaped(st)) &&
	(!da_hash || !da_hash->Find(st)) &&
	(TY_kind(ST_type(st)) == KIND_ARRAY)) {
	  _la_stack->Push(st);
    }
  }
  Sort_Stack();
}

// Sort the stack, biggest arrays first
// We use an insertion sort since it's very unlikely there will be too 
// many arrays and array sizes could easily cause quick-sort to degenerate
// into O(n^2) anyways
void AEQUIV::Sort_Stack()
{
  INT num_var = _la_stack->Elements();
  for (INT i=0; i<num_var; i++) {
    INT best_var = i;
    INT best_size = TY_size(ST_type(_la_stack->Bottom_nth(i)));
    for (INT j=i+1; j<num_var; j++) {
      ST *st = _la_stack->Bottom_nth(j);
      INT size = TY_size(ST_type(st));
      if (size > best_size) {  
	best_var = j;
	best_size = size;
      }
    }
    if (best_var != i) { // swap
      ST *temp = _la_stack->Bottom_nth(best_var);
      _la_stack->Bottom_nth(best_var) = _la_stack->Bottom_nth(i);
      _la_stack->Bottom_nth(i) = temp;
    }
  }
  if (Get_Trace(TP_LNOPT,TT_LNO_AEQUIV)) {
    for (INT i=0; i<_la_stack->Elements(); i++) {
      fprintf(TFile,"local array %d is %s \n",i,
	ST_name(ST_base(_la_stack->Bottom_nth(i))));
    }
  }
}

// Walk the stack, enter all the local arrays into the hash table 
void AEQUIV::Enter_Locals_Hash()
{
  ST *st;
  for (INT id=0; id<_la_stack->Elements(); id++) {
    ST *st = _la_stack->Bottom_nth(id);
    LOCAL_ARRAY_DESC *array = CXX_NEW(LOCAL_ARRAY_DESC(id),_pool);
    _la_hash_table->Enter(st,array);
  }
}



// Build the control flow graph
// Walk the code, creating a vertex for each 'BB'.  Recall that we collapse
// outer loops into single nodes.  For each vertex, create a BIT_VECTOR
// telling whether array 'i' is accessed in the BB.
// Set the bits (read,written) in the local array hash table
//
// Create a stack of all the gotos and all the labels and
//  a hash table mapping labels to vertex
//  numbers.  We use these to 
//  backpatch all the edges from the gotos to the labels.
//
// Also set the is_written/is_read/address_taken bits for all the local arrays
//
// Return -1 on error
INT AEQUIV::Build_CFG()
{
  _cfg = CXX_NEW(SCC_DIRECTED_GRAPH16(200,200),_pool);
  GOTO_VERTEX_STACK goto_stack(_pool);
  LABEL_STACK label_stack(_pool);
  LABEL_VERTEX_HASH label_hash(200,_pool);

  _head_vertex =
	Add_CFG_Vertex(CXX_NEW(BIT_VECTOR(Num_Arrays(),_pool),_pool));
  _tail_vertex =
	Add_CFG_Vertex(CXX_NEW(BIT_VECTOR(Num_Arrays(),_pool),_pool));

  if (Build_CFG_Rec(WN_func_body(_func_nd),
      &_head_vertex,_tail_vertex,&goto_stack,&label_stack, &label_hash)==-1) {
    return -1;
  }

  if (Backpatch_CFG(&goto_stack,&label_stack,&label_hash) == -1) {
    return -1;
  }
  return 1;
}



// recursively build
// wn is the wn we're working on
// la_hash_table maps local arrays to their descriptors
// current_v is the vertex number of the current block
// next_v is the the vertex number of the 'next' block
//   at the top level it points to the return block
//   inside an if, it points to the statement following the if
//   we need this in order to put edges from the bottom of ifs to the nodes
//   following ifs
INT AEQUIV::Build_CFG_Rec(WN *wn, VINDEX16 *current_v, 
    VINDEX16 next_v,GOTO_VERTEX_STACK *goto_stack,LABEL_STACK *label_stack,
    LABEL_VERTEX_HASH *label_hash) 
{
  OPCODE opcode = WN_opcode(wn);

  if ((opcode == OPC_DO_LOOP) || (opcode == OPC_DO_WHILE) || 
	(opcode == OPC_WHILE_DO)) {

    // Create a vertex for the loop
    VINDEX16 loop_v = 
      Add_CFG_Vertex(CXX_NEW(BIT_VECTOR(Num_Arrays(),_pool),_pool));
    if (!loop_v) return -1;

    // add an edge from the current vertex to the loop
    if (!Add_CFG_Edge(*current_v,loop_v)) return -1;

    // Process the loop
    if (Build_CFG_Loop(wn,loop_v,goto_stack,label_stack,label_hash) == -1) { 
      return -1;
    }

    // bump up current_v
    *current_v =
	Add_CFG_Vertex(CXX_NEW(BIT_VECTOR(Num_Arrays(),_pool),_pool));
    if (!*current_v) return -1;

    // Add an edge from the loop to new current_v
    if (!Add_CFG_Edge(loop_v,*current_v)) return -1;

  } else if (opcode == OPC_BLOCK) {
    WN *kid = WN_first(wn);
    while (kid) {
      if (Build_CFG_Rec(kid,current_v,next_v,goto_stack,label_stack,label_hash) 
								== -1) {
	return -1;
      }
      kid = WN_next(kid);
    }
    if (!Add_CFG_Edge(*current_v,next_v)) return -1;
  } else if (opcode == OPC_IF) {
    Handle_Rhs(WN_if_test(wn),*current_v);
    VINDEX16 old_current = *current_v;

    // add a new vertex for the block following the if
    VINDEX16 newv=
      Add_CFG_Vertex(CXX_NEW(BIT_VECTOR(Num_Arrays(),_pool),_pool));
    if (!newv) return -1;

    BOOL if_then_else=TRUE; 
    if (WN_first(WN_then(wn))) { // then exists
      *current_v=Add_CFG_Vertex(CXX_NEW(BIT_VECTOR(Num_Arrays(),_pool),_pool));
      if (!*current_v) return -1;
      if (!Add_CFG_Edge(old_current,*current_v)) return -1;
      if (Build_CFG_Rec(WN_then(wn),current_v,newv,
		goto_stack,label_stack,label_hash) == -1) {
	return -1;
      }
    } else {
      if_then_else = FALSE;
    }
    if (WN_first(WN_else(wn))) { // else exists
      *current_v=Add_CFG_Vertex(CXX_NEW(BIT_VECTOR(Num_Arrays(),_pool),_pool)); 
      if (!*current_v) return -1;
      if (!Add_CFG_Edge(old_current,*current_v)) return -1;
      if (Build_CFG_Rec(WN_else(wn),current_v,newv,goto_stack,
					label_stack,label_hash) == -1) {
	return -1;
      }
    } else {
      if_then_else = FALSE;
    }
    if (!if_then_else) { // an edge from the if to the block following
      if (!Add_CFG_Edge(old_current,newv)) return -1;
    }
    *current_v = newv;
  } else if ((opcode == OPC_GOTO) || (opcode == OPC_AGOTO)) {
    GOTO_VERTEX gv;
    gv._wn = wn;
    gv._vindex = *current_v;
    goto_stack->Push(gv);
    *current_v = Add_CFG_Vertex(CXX_NEW(BIT_VECTOR(Num_Arrays(),_pool),_pool)); 
    if (!*current_v) return -1;
  } else if (opcode == OPC_IO) {  // implicit gotos for IOC_ERR and IOC_EOR
    Handle_Call(wn,*current_v);
    for (INT32 i = 0; i < WN_kid_count(wn); i++) {
      WN *kid = WN_kid(wn, i);
      OPCODE kid_opcode = WN_opcode(kid);
      Is_True(OPCODE_has_inumber(kid_opcode),
		("illegal OPC_IO_ITEM within iostmt"));
      if (kid_opcode == OPC_IO_ITEM) {
        if (WN_io_item(kid) == IOC_END || WN_io_item(kid) == IOC_ERR ||
					WN_io_item(kid) == IOC_EOR) {
          WN *kid0 = WN_kid0(kid);
	  Is_True(WN_operator(kid0) == OPR_GOTO,
				  ("IOC_END/ERR with non-GOTO kid"));
          GOTO_VERTEX gv;
          gv._wn = kid0;
          gv._vindex = *current_v;
          goto_stack->Push(gv);
          VINDEX16 old_current = *current_v;
          *current_v = Add_CFG_Vertex(CXX_NEW(BIT_VECTOR(
				Num_Arrays(),_pool),_pool)); 
          if (!*current_v) return -1;
          if (!Add_CFG_Edge(old_current,*current_v)) return -1;
        }
      }
    }
  } else if (opcode == OPC_COMPGOTO) {
    GOTO_VERTEX gv;
    Handle_Rhs(WN_kid0(wn),*current_v);
    WN *goto_wn= WN_first(WN_kid1(wn));
    while (goto_wn) {
      gv._wn = goto_wn;
      gv._vindex = *current_v;
      goto_stack->Push(gv);
      goto_wn = WN_next(goto_wn);
    }
    *current_v =
	Add_CFG_Vertex(CXX_NEW(BIT_VECTOR(Num_Arrays(),_pool),_pool)); 
    if (!*current_v) return -1;
  } else if (opcode == OPC_ALTENTRY) {
    if (!Add_CFG_Edge(_head_vertex,*current_v)) return -1;
    *current_v = Add_CFG_Vertex(CXX_NEW(BIT_VECTOR(Num_Arrays(),_pool),_pool)); 
    if (!*current_v) return -1;
  } else if (opcode == OPC_TRUEBR || opcode == OPC_FALSEBR) {
    for (INT kidno=0; kidno < WN_kid_count(wn); kidno++) {
      Handle_Rhs(WN_kid(wn,kidno),*current_v);
    }
    GOTO_VERTEX gv;
    gv._wn = wn;
    gv._vindex = *current_v;
    goto_stack->Push(gv);
    VINDEX16 old_current = *current_v;
    *current_v = Add_CFG_Vertex(CXX_NEW(BIT_VECTOR(Num_Arrays(),_pool),_pool)); 
    if (!*current_v) return -1;
    if (!Add_CFG_Edge(old_current,*current_v)) return -1;
  } else if (opcode == OPC_RETURN
#ifdef KEY
  	     || opcode == OPC_GOTO_OUTER_BLOCK
#endif
             ) {
    if (!Add_CFG_Edge(*current_v,_tail_vertex)) return -1;
  } else if (opcode == OPC_LABEL) {
    VINDEX16 old_current = *current_v;
    *current_v = Add_CFG_Vertex(CXX_NEW(BIT_VECTOR(Num_Arrays(),_pool),_pool)); 
    if (!*current_v) return -1;
    if (!Add_CFG_Edge(old_current,*current_v)) return -1;
    label_hash->Enter(WN_label_number(wn),*current_v);
    label_stack->Push(*current_v);
  } else if (OPCODE_is_store(opcode)) {
    Handle_Store(wn,*current_v);
  } else if (OPCODE_is_load(opcode)) {
    ST *st = NULL;
    if (OPCODE_operator(opcode) == OPR_LDID) st = WN_st(wn);
    else {
      // handle the simple cases, cross your fingers
      if (WN_operator(WN_kid0(wn)) == OPR_LDID) {
        st = WN_st(WN_kid0(wn));
      } else if (WN_operator(WN_kid0(wn)) == OPR_ARRAY &&
                 (WN_operator(WN_array_base(WN_kid0(wn))) == OPR_LDID ||
                  WN_operator(WN_array_base(WN_kid0(wn))) == OPR_LDA)) {
        st = WN_st(WN_array_base(WN_kid0(wn)));
      }
    }

    LOCAL_ARRAY_DESC *lad = _la_hash_table->Find(st);
    if (lad && TY_is_volatile(WN_ty(wn))) {
      lad->_address_taken=TRUE;
    }
  } else if (OPCODE_is_call(opcode)) {
    Handle_Call(wn,*current_v);
  } else if (!OPCODE_is_expression(opcode)) {
    for (INT kidno=0; kidno < WN_kid_count(wn); kidno++) {
      if (Build_CFG_Rec(WN_kid(wn,kidno),current_v,next_v,
		goto_stack,label_stack,label_hash) == -1) {
	return -1;
      }
    }
  } else {
    Handle_Rhs(wn,*current_v);
  }
  return 1;
}

//-----------------------------------------------------------------------
// NAME: Build_St_Stack_And_Skip 
// FUNCTION: Push an entry onto 'st_stack' corresponding to the ST* of 
//   each mode with a symbol in the tree rooted at 'wn_tree'. (Except
//   for an ovbvious case we want to skip, i.e. real references that 
//   we know don't take the address).
//-----------------------------------------------------------------------

static void Build_St_Stack_And_Skip(WN* wn_tree, 
			            STACK<ST*>* st_stack)
{ 
  // Skipping the obvious most important case. 
  if (WN_operator(wn_tree) == OPR_ILOAD 
      && WN_operator(WN_kid0(wn_tree)) == OPR_ARRAY 
      && OPCODE_has_sym(WN_opcode(WN_array_base(WN_kid0(wn_tree)))))
    return; 

  if (OPCODE_has_sym(WN_opcode(wn_tree)))
    st_stack->Push(WN_st(wn_tree));

  if (WN_operator(wn_tree) == OPR_BLOCK) { 
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))  
      Build_St_Stack_And_Skip(wn, st_stack);
  } else { 
    for (INT i = 0; i < WN_kid_count(wn_tree); i++) 
      Build_St_Stack_And_Skip(WN_kid(wn_tree, i), st_stack);
  } 
} 

// Process a store
// For the lhs, if we're writing to a local array, set the bits in the 
// bitmask and in the la_hash_table
// For the rhs, if we're reading from a local array, do the same
void AEQUIV::Handle_Store(WN *store, VINDEX16 v)
{
  Handle_Rhs(WN_kid0(store),v);
  if (TY_is_volatile(WN_ty(store)) ||
      ((WN_operator(store) != OPR_STID) &&
	       TY_is_volatile(TY_pointed(WN_ty(store))))) {

    ST *st = NULL;
    if (WN_operator(store) == OPR_STID) {
      st = WN_st(store);
      LOCAL_ARRAY_DESC *lad = _la_hash_table->Find(st);
      if (lad) 
	lad->_address_taken = TRUE;
    } else {
      // is an ISTORE; finding the ST is trickier
      // handle just the simple cases, and hope that's good enough!
      WN *addr = WN_kid1(store);
      STACK<ST*> st_stack(_pool);
      Build_St_Stack_And_Skip(addr, &st_stack);
      for (INT i = 0; i < st_stack.Elements(); i++) { 
	ST* st = st_stack.Bottom_nth(i);
	LOCAL_ARRAY_DESC *lad = _la_hash_table->Find(st);
	if (lad) 
	  lad->_address_taken = TRUE;
      } 
    }
  }
  if (WN_kid_count(store) >= 2) {
    Handle_Lhs(WN_kid1(store),v);
  } else if (WN_operator(store) == OPR_STID) {
    LOCAL_ARRAY_DESC *lad = _la_hash_table->Find(WN_st(store));
    if (lad ) {
      lad->_address_taken = TRUE;
    }
  }
}

void AEQUIV::Handle_Lhs(WN *wn, VINDEX16 v)
{
  if (WN_operator(wn) == OPR_LDA) {
    LOCAL_ARRAY_DESC *lad = _la_hash_table->Find(WN_st(wn));
    if (lad) {
      lad->_is_written = TRUE;
      _cyclic_bit_vector->Bottom_nth(v)->Set(lad->_id);
    }
  } else if (WN_operator(wn) == OPR_LDID) {
    LOCAL_ARRAY_DESC *lad = _la_hash_table->Find(WN_st(wn));
    if (lad) {
      lad->_address_taken = TRUE;
    }
  } else if (OPCODE_is_load(WN_opcode(wn))) {
    for (INT kidno=0; kidno < WN_kid_count(wn); kidno++) {
      Handle_Rhs(WN_kid(wn,kidno),v);
    }
  } else {
    for (INT kidno=0; kidno < WN_kid_count(wn); kidno++) {
      Handle_Lhs(WN_kid(wn,kidno),v);
    }
  }
}

void AEQUIV::Handle_Rhs(WN *wn, VINDEX16 v)
{
  if (WN_operator(wn) == OPR_LDA) {
    LOCAL_ARRAY_DESC *lad = _la_hash_table->Find(WN_st(wn));
    if (lad) {
      WN *parent = LWN_Get_Parent(wn);
      if (WN_operator(parent) == OPR_ARRAY) {
	OPCODE grandparent_opcode = (WN_opcode(LWN_Get_Parent(parent)));
	OPERATOR grandparent = OPCODE_operator(grandparent_opcode);
        if ((grandparent == OPR_ILOAD) || 
	    (OPCODE_is_prefetch(grandparent_opcode)) ||
	    ((grandparent == OPR_ISTORE) && 
	     (parent = WN_kid1(LWN_Get_Parent(parent))))) {
          lad->_is_read = TRUE;
          _cyclic_bit_vector->Bottom_nth(v)->Set(lad->_id);
        } else {
	  lad->_address_taken = TRUE;
        }
      } else {
	lad->_address_taken = TRUE;
      }
    }
  } else if (WN_operator(wn) == OPR_LDID) {
    LOCAL_ARRAY_DESC *lad = _la_hash_table->Find(WN_st(wn));
    if (lad) {
      lad->_address_taken = TRUE;
    }
  } else {
    for (INT kidno=0; kidno < WN_kid_count(wn); kidno++) {
      Handle_Rhs(WN_kid(wn,kidno),v);
    }
  }
}

// Handle calls or I/O, 
// like above except we assume the array is both written and read
void AEQUIV::Handle_Call(WN *wn, VINDEX16 v)
{
  if (WN_operator(wn) == OPR_LDA) {
    LOCAL_ARRAY_DESC *lad = _la_hash_table->Find(WN_st(wn));
    if (lad) {
      if (WN_operator(LWN_Get_Parent(wn)) == OPR_ARRAY) {
#ifndef KEY
        lad->_is_read = TRUE;
        lad->_is_written = TRUE;
        _cyclic_bit_vector->Bottom_nth(v)->Set(lad->_id);
#else
	// Bug 5651 - handle Fortran Pass By Reference
	// If the parameter is passed by reference then the address could be 
	// taken inside the callee unless the PARM node has the flag 
	// WN_PARM_PASSED_NOT_SAVED set (IPA or the compiler front-end could
	// do this).
	if (WN_operator(LWN_Get_Parent(LWN_Get_Parent(wn))) == OPR_PARM &&
	    // The condition above guarantees a pass by reference.
	    !(WN_flag(LWN_Get_Parent(LWN_Get_Parent(wn))) &
	      WN_PARM_PASSED_NOT_SAVED)) {
	  FmtAssert(WN_flag(LWN_Get_Parent(LWN_Get_Parent(wn))) &
		    WN_PARM_BY_REFERENCE, ("Handle this case"));
	  lad->_address_taken = TRUE;
	} else {
	  lad->_is_read = TRUE;
	  lad->_is_written = TRUE;
	  _cyclic_bit_vector->Bottom_nth(v)->Set(lad->_id);
	}	  
#endif
      } else {
	lad->_address_taken = TRUE;
      }
    }
  } else if (WN_operator(wn) == OPR_LDID) {
    LOCAL_ARRAY_DESC *lad = _la_hash_table->Find(WN_st(wn));
    if (lad) {
      lad->_is_read = TRUE;
      lad->_is_written = TRUE;
      _cyclic_bit_vector->Bottom_nth(v)->Set(lad->_id);
    }
  } else if (WN_operator(wn) == OPR_BLOCK) {
    WN *kid = WN_first(wn);
    while (kid) {
      Handle_Call(kid,v);
      kid = WN_next(kid);
    }
  } else {
    for (INT kidno=0; kidno < WN_kid_count(wn); kidno++) {
      Handle_Call(WN_kid(wn,kidno),v);
    }
  }
}

      


      

// Process an outer loop
INT AEQUIV::Build_CFG_Loop(WN *wn,VINDEX16 loopv,
			GOTO_VERTEX_STACK *goto_stack,LABEL_STACK *label_stack,
		        LABEL_VERTEX_HASH *label_hash)
{
  OPCODE opcode = WN_opcode(wn);

  if (opcode == OPC_BLOCK) {
    WN *kid = WN_first(wn);
    while (kid) {
      if (Build_CFG_Loop(kid,loopv,goto_stack,label_stack,label_hash) == -1) {
	return -1;
      }
      kid = WN_next(kid);
    }
    return 1;
  } 

  if (opcode == OPC_GOTO || opcode == OPC_AGOTO ||
      opcode == OPC_TRUEBR || opcode == OPC_FALSEBR) {
    for (INT kidno=0; kidno < WN_kid_count(wn); kidno++) {
      Handle_Rhs(WN_kid(wn,kidno),loopv);
    }
    GOTO_VERTEX gv;
    gv._wn = wn;
    gv._vindex = loopv;
    goto_stack->Push(gv);
  } else if (opcode == OPC_IO) {  // implicit gotos for IOC_ERR and IOC_EOR
    Handle_Call(wn,loopv);
    for (INT32 i = 0; i < WN_kid_count(wn); i++) {
      WN *kid = WN_kid(wn, i);
      OPCODE kid_opcode = WN_opcode(kid);
      Is_True(OPCODE_has_inumber(kid_opcode),
		("illegal OPC_IO_ITEM within iostmt"));
      if (kid_opcode == OPC_IO_ITEM) {
        if (WN_io_item(kid) == IOC_END || WN_io_item(kid) == IOC_ERR ||
					WN_io_item(kid) == IOC_EOR) {
          WN *kid0 = WN_kid0(kid);
	  Is_True(WN_operator(kid0) == OPR_GOTO,
				  ("IOC_END/ERR with non-GOTO kid"));
          GOTO_VERTEX gv;
          gv._wn = kid0;
          gv._vindex = loopv;
          goto_stack->Push(gv);
        }
      }
    }
  } else if (opcode == OPC_COMPGOTO) {
    Handle_Rhs(WN_kid0(wn),loopv);
    GOTO_VERTEX gv;
    WN *goto_wn= WN_first(WN_kid1(wn));
    while (goto_wn) {
      gv._wn = goto_wn;
      gv._vindex = loopv;
      goto_stack->Push(gv);
      goto_wn = WN_next(goto_wn);
    }
  } else if (opcode == OPC_ALTENTRY) {
    if (!Add_CFG_Edge(_head_vertex,loopv)) return -1;
  } else if (opcode == OPC_RETURN
#ifdef KEY
  	     || opcode == OPC_GOTO_OUTER_BLOCK
#endif
             ) {
   if (!Add_CFG_Edge(loopv,_tail_vertex)) return -1;
  } else if (opcode == OPC_LABEL) {
    label_hash->Enter(WN_label_number(wn),loopv);
    label_stack->Push(loopv);
  } else if (OPCODE_is_store(opcode)) {
    Handle_Store(wn,loopv);
  } else if (OPCODE_is_call(opcode)) {
    Handle_Call(wn,loopv);
  } else if (!OPCODE_is_expression(opcode)) {
    for (INT kidno=0; kidno < WN_kid_count(wn); kidno++) {
      if (Build_CFG_Loop(WN_kid(wn,kidno),
		loopv,goto_stack,label_stack,label_hash) == -1) {
	return -1;
      }
    }
  } else {
    Handle_Rhs(wn,loopv);
  }

  return 1;
}


      

void AEQUIV::Print_Graph(FILE *fp,SCC_DIRECTED_GRAPH16 *graph)
{
  VINDEX16 i;
  EINDEX16 e;
  fprintf(fp,"Printing a control-flow graph \n");

  for (i=graph->Get_Vertex(); i; i = graph->Get_Next_Vertex(i)) {
    fprintf(fp,"Vertex %d has bitmask =",i);
    if (graph == _cfg) {
      _cyclic_bit_vector->Bottom_nth(i)->Print(fp);
    } else {
      _acyclic_bit_vector->Bottom_nth(i)->Print(fp);
    }
    fprintf(fp,"\n");

    e = graph->Get_Out_Edge(i);
    while (e) {
      fprintf(fp,"Edge to vertex %d \n",graph->Get_Sink(e));
      e = graph->Get_Next_Out_Edge(e);
    }
  }
}

// Add edges for every goto we visisted
INT AEQUIV::Backpatch_CFG(GOTO_VERTEX_STACK *goto_stack,
		LABEL_STACK *label_stack, LABEL_VERTEX_HASH *label_hash)
{
  for (INT i=0; i<goto_stack->Elements(); i++) {
    GOTO_VERTEX *gv = &goto_stack->Bottom_nth(i);
    WN *goto_wn = gv->_wn;
    VINDEX16 goto_v = gv->_vindex;
    OPCODE opcode = WN_opcode(goto_wn);
    if (opcode == OPC_AGOTO) {  // dependent on all labels
      for (INT j=0; j<label_stack->Elements(); j++) {
	Add_CFG_Edge(goto_v,label_stack->Bottom_nth(j));
      }
    } else {
      VINDEX16 labelv = label_hash->Find(WN_label_number(goto_wn));
      Is_True(labelv,
	("Goto to non-existant label in AEQUIV::Backpatch_CFG"));
       if (!Add_CFG_Edge(goto_v,labelv)) return -1;
    }
  }
  return 1;
}

// Add a vertex to the CFG graph
// Also add the vertex and bit vector to the bit vector stack
VINDEX16 AEQUIV::Add_CFG_Vertex(BIT_VECTOR *bit_vector) {
  VINDEX16 result = _cfg->Add_Vertex();
  if (result != 0) {
    while (_cyclic_bit_vector->Elements() <= result) {
      _cyclic_bit_vector->Push(NULL);
    }
    _cyclic_bit_vector->Bottom_nth(result) = bit_vector;
  }
  return result;
}

// Create an acyclic graph from the cyclic
void AEQUIV::Set_Acyclic()
{
  _acfg = _cfg->Acyclic_Condensation(_pool);

  // now set the bit vectors, this relies on the fact the scc ids of
  // cfg are the same as the vertex numbers of acfg 

  _acyclic_bit_vector = CXX_NEW(BIT_VECTOR_STACK(_pool),_pool);
  for (VINDEX16 v=_cfg->Get_Vertex(); v; v = _cfg->Get_Next_Vertex(v)) {
    VINDEX16 scc = _cfg->Get_Scc_Id(v);
    while (_acyclic_bit_vector->Elements() <= scc) {
      _acyclic_bit_vector->Push(CXX_NEW(BIT_VECTOR(Num_Arrays(),_pool),_pool));
    }
    if (_acyclic_bit_vector->Bottom_nth(scc)) {
      *(_acyclic_bit_vector->Bottom_nth(scc)) |= 
				*_cyclic_bit_vector->Bottom_nth(v);
    } else {
      *(_acyclic_bit_vector->Bottom_nth(scc)) = 
		*_cyclic_bit_vector->Bottom_nth(v);
    }
  }
}

// on input, _acyclic_bit_vector[v] is one for every variable referenced in
// block v.  On output, it's one for every variable alive in block v
void AEQUIV::Do_Dataflow()
{
  MEM_POOL_Push(&LNO_local_pool);

  // find a topological sorting of the vertices
  INT num_v = _acfg->Get_Vertex_Count();
  VINDEX16 *vsorted = CXX_NEW_ARRAY(VINDEX16,num_v,&LNO_local_pool);
  _acfg->Level_Sort(vsorted);

  // forward dataflow, has_been_ref = 1 for things that have been referenced
  // somewhere between the start and the current vertex
  BIT_VECTOR_STACK *has_been_ref = 
	CXX_NEW(BIT_VECTOR_STACK(&LNO_local_pool),&LNO_local_pool);
  INT i;
  for (i=0; i<num_v; i++) {
    VINDEX16 v = vsorted[i];
    while (has_been_ref->Elements() <= v) {
      has_been_ref->Push(CXX_NEW(BIT_VECTOR(Num_Arrays(),&LNO_local_pool),
							&LNO_local_pool));
    }

    // initialize this to its counterpart in the acyclic_bit vector
    BIT_VECTOR *this_vector = has_been_ref->Bottom_nth(v);
    *this_vector = *_acyclic_bit_vector->Bottom_nth(v);

    // or in the bit vectors of all the predecessors
    EINDEX16 e = _acfg->Get_In_Edge(v);
    while (e) {
      VINDEX16 pred = _acfg->Get_Source(e);
      *this_vector |= *has_been_ref->Bottom_nth(pred);
      e = _acfg->Get_Next_In_Edge(e);
    }
  }

  // backward dataflow, still_ref = 1 for things that are referenced in this
  // or a later block
  BIT_VECTOR_STACK *still_ref = 
	CXX_NEW(BIT_VECTOR_STACK(&LNO_local_pool),&LNO_local_pool);
  for (i=num_v-1; i>=0; i--) {
    VINDEX16 v = vsorted[i];
    while (still_ref->Elements() <= v) {
      still_ref->Push(CXX_NEW(BIT_VECTOR(Num_Arrays(),&LNO_local_pool),
						&LNO_local_pool));
    }

    // initialize this to its counterpart in acyclic_bit vector
    BIT_VECTOR *this_vector = still_ref->Bottom_nth(v);
    *this_vector = *_acyclic_bit_vector->Bottom_nth(v);

    // or in the successors
    EINDEX16 e = _acfg->Get_Out_Edge(v);
    while (e) {
      VINDEX16 succ = _acfg->Get_Sink(e);
      *this_vector |= *still_ref->Bottom_nth(succ);
      e = _acfg->Get_Next_Out_Edge(e);
    }
  }

  // combine the two, set _acyclic_bit_vector to has_been_ref & still_ref
  for (i=0; i<num_v; i++) {
    VINDEX16 v = vsorted[i];
    *_acyclic_bit_vector->Bottom_nth(v) = *has_been_ref->Bottom_nth(v) &
    				          *still_ref->Bottom_nth(v);
  }

  MEM_POOL_Pop(&LNO_local_pool);
}


// _array_bit_vector[i] is a bit vector with 1 set for each vertex in which
// array[i] is live
void AEQUIV::Set_Array_Bit_Vector()
{
  INT num_v = _acfg->Get_Vertex_Count();
  INT num_a = Num_Arrays();

  // Initialize the bit vector 
  _array_bit_vector = CXX_NEW(BIT_VECTOR_STACK(_pool),_pool);
  INT i;
  for (i=0; i<num_a; i++) {
    _array_bit_vector->Push(CXX_NEW(BIT_VECTOR(num_v,_pool),_pool));
  }

  // walk the graph
  for (VINDEX16 v=_acfg->Get_Vertex(); v; v = _acfg->Get_Next_Vertex(v)) {
    BIT_VECTOR *ac_bit_vector = _acyclic_bit_vector->Bottom_nth(v);
    for (i=0; i<num_a; i++) {
      if (ac_bit_vector->Test(i)) {
	_array_bit_vector->Bottom_nth(i)->Set(v-1);
      }
    }
  }
}


// Color the arrays
//
// on exit, equivalenced_array[i] = TRUE iff array i is either equivalenced
//	or marked for removal
//
// return whether we equivalenced (or marked as unread) an array
BOOL AEQUIV::Do_Color(mBOOL *equivalenced_array) 
{
  BOOL result = FALSE;
  MEM_POOL_Push(&LNO_local_pool);
  INT num_array = Num_Arrays();
  mBOOL *did_color = CXX_NEW_ARRAY(mBOOL,num_array,&LNO_local_pool);
  BIT_VECTOR *bv = CXX_NEW(BIT_VECTOR(_acfg->Get_Vertex_Count(),
				&LNO_local_pool),&LNO_local_pool);

  // initialize 
  INT i;
  for (i=0; i<num_array; i++) {
    LOCAL_ARRAY_DESC *lad = _la_hash_table->Find(_la_stack->Bottom_nth(i));
    if (lad->_address_taken) {
      did_color[i] = TRUE; // don't equivalence it with anything
      equivalenced_array[i] = FALSE;
    } else if (!lad->_is_read) {
      result = TRUE;
      did_color[i] = TRUE; // if it's not read, we don't need to color
      equivalenced_array[i] = TRUE;
      Set_ST_is_not_used(_la_stack->Bottom_nth(i));
      if (Get_Trace(TP_LNOPT,TT_LNO_AEQUIV)) {
	fprintf(TFile,"eliminating all references to array %d \n",i);
      }
    } else {
      did_color[i] = FALSE;
      equivalenced_array[i] = FALSE;
    }
  }

  for (i=0; i<num_array; i++) {
    if (!did_color[i]) {  // color array i
      *bv = *_array_bit_vector->Bottom_nth(i);
      did_color[i] = TRUE;
      for (INT j=0; j<num_array; j++) {
	if (!did_color[j]) {
	  if (!bv->Intersects(_array_bit_vector->Bottom_nth(j))) { 
	    // they don't interfere
	    result = TRUE;
	    St_Block_Union(_la_stack->Bottom_nth(i),_la_stack->Bottom_nth(j));
	    *bv |= *_array_bit_vector->Bottom_nth(j);
	    did_color[j] = TRUE;
            equivalenced_array[i] = TRUE;
            equivalenced_array[j] = TRUE;
            if (Get_Trace(TP_LNOPT,TT_LNO_AEQUIV)) {
	      fprintf(TFile,"equivalencing arrays %d and %d\n",i,j);
            }
          }
        }
      }
    }
  }
  MEM_POOL_Pop(&LNO_local_pool);
  return result;
}


// Throw away all stores to unread arrays
// Update alias info for equivalenced arrays
void AEQUIV::Update_Code(WN *wn,mBOOL *equivalenced_array)
{
  OPCODE opcode = WN_opcode(wn);
  OPERATOR oper = OPCODE_operator(opcode);
  if (opcode == OPC_BLOCK) {
    WN *kid = WN_first(wn);
    while (kid) {
      Update_Code(kid,equivalenced_array);
      kid = WN_next(kid);
    }
  } else if ((OPCODE_is_store(opcode) || OPCODE_is_prefetch(opcode)) &&
             (Contains_Unread_Array(wn,equivalenced_array))) {
    LWN_Delete_Tree(wn);
  } else if (oper == OPR_LDA) {
    LOCAL_ARRAY_DESC *lad = _la_hash_table->Find(WN_st(wn));
    if (lad && equivalenced_array[lad->_id]) {

      // Invalidate IPA-alias info for this array
      Note_Invalid_IP_Alias_Class(Alias_Mgr, wn);

      WN *tmp = LWN_Get_Parent(wn);
      while (tmp) {
	OPCODE opc_tmp = WN_opcode(tmp);
	if (OPCODE_is_store(opc_tmp) || OPCODE_is_load(opc_tmp)) {
	  Create_alias(Alias_Mgr,tmp);
          // We should invalidate just the ld/st that actually
          // references this object, not all others that use the
          // value. For instance consider a[b[i]], where "b" is
          // equivd. We should invalidate the iload-b, not iload-a.
          // So we break here.
          break;
	}
	tmp = LWN_Get_Parent(tmp);
      }
    }
  } else if (oper == OPR_PRAGMA &&
             (WN_pragma(wn) == WN_PRAGMA_LASTLOCAL ||
              WN_pragma(wn) == WN_PRAGMA_LOCAL)) {
    ST *st = WN_st(wn);
    LOCAL_ARRAY_DESC *lad = _la_hash_table->Find(st);
    if (lad &&
        !lad->_address_taken &&
        equivalenced_array[lad->_id] &&
        !lad->_is_read) {
      LWN_Delete_Tree(wn);
    }
  } else {
    for (INT kidno=0; kidno < WN_kid_count(wn); kidno++) {
      Update_Code(WN_kid(wn,kidno),equivalenced_array);
    }
  }
}


BOOL AEQUIV::Contains_Unread_Array(WN *wn,mBOOL *equivalenced_array)
{
  OPCODE opcode = WN_opcode(wn);
  OPERATOR oper = OPCODE_operator(opcode);
  if (oper == OPR_LDA) {
    LOCAL_ARRAY_DESC *lad = _la_hash_table->Find(WN_st(wn));
    if (lad && !lad->_address_taken) {
      if (equivalenced_array[lad->_id]) {
	if (!lad->_is_read) {
	  return TRUE;
        }
      }
    }
  } 
  for (INT kidno=0; kidno < WN_kid_count(wn); kidno++) {
    if (Contains_Unread_Array(WN_kid(wn,kidno),equivalenced_array)) {
      return TRUE;
    }
  }
  return FALSE;
}

