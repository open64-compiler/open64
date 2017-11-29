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


// ====================================================================
// ====================================================================
//
// Module: ipa_cprop.cxx
// $Source: /proj/osprey/CVS/open64/osprey1.0/ipa/main/analyze/ipa_cprop.cxx,v $
//
// Revision history:
//  28-Jun-95 - Original Version
//
// Description:
//
// Constant propagation analysis.
//
// ====================================================================
// ====================================================================

#include <stdint.h>
#include <alloca.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include "defs.h"
#include "mempool.h"
#include "clone.h"
#include "tlog.h"                       // Generate_Tlog
#include "ipc_symtab_merge.h"           // AUX_PU_node
#include "ipl_summary.h"		// summary info
#include "ipaa.h"                       // Mod_Ref_Info
#include "ipa_cg.h"                     // call graph
#include "ipa_cprop.h"
#include "ipa_cprop_annot.h"		// Union_node_annot
#include "ipa_feedback.h"
#include "ipa_inline.h"			// Total_Prog_Size
#include "ipa_option.h"			// IPA_TRACE flags
#include "ipa_summary.h"                // IPA_get_ functions

// Maximum and count for the total number of clone nodes
UINT32 IPA_Max_Total_Clones;
UINT32 IPA_Num_Total_Clones = 0;

INT IPA_Constant_Count = 0; // number of constants found
MEM_POOL Global_mem_pool; 


static void
Ipa_cprop_tlog (IPA_NODE* node)
{       
  char buffer[64];
  
  VALUE_DYN_ARRAY* formal_annot = node->Cprop_Annot();
  if (formal_annot && formal_annot != (VALUE_DYN_ARRAY*) -1) {
    for (INT32 i = 0; i < formal_annot->Elements(); ++i) {
      if ((*formal_annot)[i].Is_constant()) {
        sprintf(buffer, "Position %d", i);
        Generate_Tlog("IPA", "Constant_Propagation", 0, 
                      DEMANGLE(node->Name()), "Formal parameter constant", 
                      buffer, "");
      }
    }
  }

  if (IPA_Enable_Common_Const) {
    GLOBAL_ANNOT* gannot = node->Global_Annot();
    for (UINT32 i = 0; i < GLOBAL_ANNOT::Size; ++i) {
      if (!gannot->Top(i) && !gannot->Bottom(i)) {
        const GLOBAL_DYN_ARRAY& gvals = gannot->Global_Value_Array(i);
        for (INT32 j = 0; j < gvals.Elements(); ++j) {
          if (gvals[j].Value()) {
            sprintf(buffer, "offset = %lld  size = %d",
                    gvals[j].Offset(), gvals[j].Size());
            Generate_Tlog("IPA", "Constant_Propagation", 0, 
                          DEMANGLE(node->Name()), "Common block constant", 
                          ST_name(GLOBAL_ANNOT::Common_ST[i]), buffer);
          }
        }
      }
    }
  }
}

//-------------------------------------------------------
// walk the graph and call initialize node and edge on
// each node and edge respectively
//-------------------------------------------------------
void
Init_Cprop_Annotations (IPA_NODE* node)
{
  if (IPA_Enable_Common_Const) {
    if (node->Is_Quasi_Clone()) {
      // copy the annotation from the origin to the clone
      IPA_NODE* origin = IPA_Call_Graph->Clone_Origin(node);
      node->Set_Global_Annot(CXX_NEW(GLOBAL_ANNOT(origin->Global_Annot(),
                                                  &Global_mem_pool),
                                     &Global_mem_pool));
    }
    else {
      GLOBAL_ANNOT* gannot = CXX_NEW(GLOBAL_ANNOT(&Global_mem_pool), 
                                     &Global_mem_pool);
      node->Set_Global_Annot(gannot);
      // other posible conditions that could cause incorrect constants:
      // c->Has_equivalences()
      // c->Has_unstructured_cflow()
      // node->Is_Nested_PU()
      if (node->Is_Externally_Callable() ||
          node->Summary_Proc()->Is_alt_entry() ||
          node->Summary_Proc()->Has_alt_entry()) {
        gannot->Set_Bottom();
      }
      else {
        for (UINT32 i = 0; i < GLOBAL_ANNOT::Size; ++i) {
          INT32 key = ST_IDX_index(GLOBAL_ANNOT::Common_ST[i]);
          // if common is not referenced by this node, just skip it
          if (!node->Mod_Ref_Info()->Is_eref_elmt(key)) {
            gannot->Set_Bottom(i);
          }
        }
      }
    }
  }

  INT count = node->Num_Formals();
  if (count > 0) {
    if (node->Is_Externally_Callable() && !IPA_Enable_Cloning) {
      node->Set_Cprop_Annot (NULL);
      node->Clear_Propagated_Const();
      return;
    }
	
    VALUE_DYN_ARRAY* annot = CXX_NEW(VALUE_DYN_ARRAY(Malloc_Mem_Pool), 
                                     Malloc_Mem_Pool);
    annot->Force_Alloc_array (count);
    annot->Setidx (count-1);
    for (INT j = 0; j < count; ++j) {
      (*annot)[j].Init();
    }
    node->Set_Cprop_Annot(annot);
    if (node->Has_Varargs()) {
      (*annot)[count-1].Set_not_const();
    }
  } 
  else {
    node->Set_Cprop_Annot (NULL);
    node->Clear_Propagated_Const();
  }
} // Init_Cprop_Annotations

//-------------------------------------------------------
// interprocedural constant propagation node annotation
// create an array of SUMMARY_VALUE entries, one for
// each formal parameter
// for globals, propagate global values
//-------------------------------------------------------
void
IPA_CPROP_DF_FLOW::InitializeNode(void* node)
{
  Init_Cprop_Annotations ((IPA_NODE*)node);
} // IPA_CPROP_DF_FLOW::InitializeNode

//----------------------------------------------------
// check if the edge has annotation that is different
// from those of edges already registered for cloning
//----------------------------------------------------
static BOOL
Edge_has_new_cprop_annot(const IPA_EDGE* edge, 
                         IPA_EDGE** clone_edges,
                         UINT32 num_clone_edges)
{
  if (edge->Cprop_Annot()) {
    for (UINT32 i = 0; i < num_clone_edges; ++i) {
      if (Edges_Have_Equiv_Cprop_Annots(edge, clone_edges[i])) {
        return FALSE;
      }
    }
  }
  return TRUE;
}

#ifdef KEY
static void
Clone_Trace (const IPA_NODE * clone, const IPA_EDGE * e)
{
  Is_True (IPA_Clone_List_Actions, ("Tracing when it is disabled"));

  VALUE_DYN_ARRAY * actuals = e->Cprop_Annot();
  Is_True (actuals && actuals != (void*)-1, ("Clone_Trace: Invalid actuals"));

  for (UINT32 i=0; i<actuals->Elements(); ++i)
  {
    SUMMARY_VALUE * value = &(*actuals)[i];
    if (value->Is_int_const())
      fprintf (stderr, "argument %d has constant value %lld\n", i+1, value->Get_int_const_value());
    else if (value->Is_const_st() && MTYPE_is_integral (value->Get_mtype()))
      fprintf (stderr, "argument %d has constant value %lld\n", i+1, TCON_i0 (Tcon_Table[value->Get_tcon_idx()]));
  }
}

static void
Report_Reason (const IPA_EDGE * e, const char * reason, float count=0.0, 
	       float limit=0.0)
{
  if (!IPA_Clone_List_Actions) return;

  fprintf (stderr, "%s not cloned for call in %s: ", DEMANGLE (IPA_Call_Graph->Callee (e)->Name()), DEMANGLE (IPA_Call_Graph->Caller (e)->Name()));
  if (limit)
    fprintf (stderr, reason, count, limit);
  else
    fprintf (stderr, reason);
}

// TODO: tune these consts
const UINT32 max_param_count = 10;
const float min_hotness = 7.0;
const UINT32 callee_limit = 500;

// taken from inliner
static float
compute_hotness_during_cloning (IPA_EDGE *e, IPA_NODE *c, INT s)
{
    FB_FREQ cycle_ratio = (e->Get_frequency () / c->Get_frequency () *
                           c->Get_cycle_count ()) / Total_cycle_count;
    float cycle_ratio_float = cycle_ratio.Value();
    float size_ratio = (float) s / ((float) MIN (Total_Prog_Size, Orig_Prog_Weight));
    float result_float = (cycle_ratio_float / size_ratio * 100.0);
    return (result_float);
}

static BOOL
Node_should_be_cloned (IPA_NODE * n /* callee */, IPA_EDGE * e)
{
  if (e->Has_frequency() && e->Get_frequency().Value() == 0.0)
  {
    Report_Reason (e, "Edge is never invoked\n");
    return FALSE;
  }

  IPA_NODE * caller = IPA_Call_Graph->Caller (e);
  if (caller->Summary_Proc()->Is_Never_Invoked() ||
      n->Summary_Proc()->Is_Never_Invoked())
  {
    Report_Reason (e, "Edge is never invoked\n");
    return FALSE;
  }

  VALUE_DYN_ARRAY * formals = n->Cprop_Annot();
  VALUE_DYN_ARRAY * actuals = e->Cprop_Annot();

  // No parameteres, or the function Is_Externally_Callable, or ...
  if (formals == NULL || actuals == NULL ||
      formals == (void*)-1 || actuals == (void*)-1)
  {
#ifdef Is_True_On
    if (formals == (void*)-1 || actuals == (void*)-1)
      DevWarn ("cprop_annot == -1 during ipa cloning analysis");
#endif
    Report_Reason (e, "No parameters?\n");
    return FALSE;
  }

  UINT32 param_count = formals->Elements() < actuals->Elements() ?
  			formals->Elements() : actuals->Elements();

  // Lots of parameters, so may not be of much help
  if (param_count > max_param_count)
  {
    Report_Reason (e, "Number of parameters %f > max (%f)\n", (float) param_count, (float) max_param_count);
    return FALSE;
  }

  UINT32 callee_weight = Effective_weight (n);
  if (n->Get_feedback())
  {
    float hotness = compute_hotness_during_cloning (e, n, callee_weight);
    if (hotness < min_hotness)
    {
      Report_Reason (e, "Hotness (%f) < min hotness (%f)\n", hotness, min_hotness);
      return FALSE;
    }
  }

  if (callee_weight > callee_limit)
  {
    Report_Reason (e, "Callee size (%f) > callee limit (%f)\n", (float) callee_weight, (float) callee_limit);
    return FALSE;
  }

  return TRUE;
}
#endif // KEY

// ----------------------------------------------------
// check if the node can be further cloned 
// don't clone if:
// 1. the total number of clones allowed is reached
// 2. the number of clones allowed per node is reached
// 3. the node represents an alterntate entry point
// 4. the node contains alternate entry points
// 5. the node is already a clone (use the original)
// 6. the node represents a nested procedure (f90)
// 7. the node contains nested procedures (f90)
// 8. the node has an incoming recursive edge
// ----------------------------------------------------
static BOOL
Node_can_be_cloned(IPA_NODE* node)
{
  if (IPA_Max_Node_Clones == 0 ||
      IPA_Num_Total_Clones >= IPA_Max_Total_Clones ||
      node->Summary_Proc()->Is_alt_entry() ||
      node->Summary_Proc()->Has_alt_entry() ||
      node->Is_Quasi_Clone() ||
      node->Is_Nested_PU() ||
      PU_Info_child(node->PU_Info()) ||
      node->Has_Incoming_Back_Edge()) {
    return FALSE;
  }

  IPA_CLONE_ARRAY* clone_array = IPA_Call_Graph->Clone_Array (node);

  return (clone_array == NULL ||
          clone_array->Elements() < IPA_Max_Node_Clones);
}


//------------------------------------------------------
// meet operation for forward constant propagation
//-------------------------------------------------------
void*
IPA_CPROP_DF_FLOW::Meet (void*, void* vertex, INT* change)
{
  IPA_NODE* n = (IPA_NODE*) vertex;
  BOOL call_from_alt_entry = FALSE;

  // Temporary array for storing edges that require cloning
  IPA_EDGE** clone_edges = 
    (IPA_EDGE**) alloca(IPA_Max_Node_Clones * sizeof(IPA_EDGE*));
  UINT32 num_clone_edges = 0;

  // Iterate over all incoming edges
  IPA_PRED_ITER edge_iter(n);
  for (edge_iter.First(); !edge_iter.Is_Empty(); edge_iter.Next()) {

    IPA_EDGE* e = edge_iter.Current_Edge();
    if (e && e->Cprop_Annot() != (void*) -1) { 

      // Don't clone nodes that are called from alt entries
      IPA_NODE* caller = IPA_Call_Graph->Caller (e);
      // need to check if caller is ROOT -- Wilson
      if (caller->Summary_Proc()->Has_alt_entry() || 
          caller->Summary_Proc()->Is_alt_entry()) {
        call_from_alt_entry = TRUE;
      }

      // If the edge is equivalent to one of clone_edges then skip it
      if (Edge_has_new_cprop_annot(e, clone_edges, num_clone_edges)) {

        // Apply the more precise union ehnanced by cloning
        if (num_clone_edges < IPA_Max_Node_Clones && Node_can_be_cloned(n)
#ifdef KEY
	    && Node_should_be_cloned(n, e)
#endif
	) {
          IPA_CLONING_ACTION action = 
            Union_Formal_Cprop_Annot_With_Cloning(n, e);
          if (action == NEEDS_CLONING) {
            clone_edges[num_clone_edges++] = e;
            IPA_Num_Total_Clones++;
          }
          else if (action == ANNOTATION_CHANGED) {
            *change = TRUE;
          }
        }
        else {
          // Apply the standard union of caller annots with 
          // callee annots placing the result in callee
          if (Union_Formal_Cprop_Annot(n, e)) {
            *change = TRUE;
          }
        }
      }
    }
  }

  if (num_clone_edges > 0 && !call_from_alt_entry) {
    for (UINT32 i = 0; i < num_clone_edges; ++i) {

      // Make a clone node for each one of the clone_edges
      IPA_NODE* clone_node=IPA_Call_Graph->Create_Quasi_Clone(clone_edges[i]);

      // Initialize clone's cprop annotations and propagate them
      (void) Meet(0, clone_node, change);
      (void) Trans(0, 0, clone_node, change);

      if (Get_Trace(TP_IPA, IPA_TRACE_CPROP_CLONING)) {
        extern char* IPA_Node_Name(IPA_NODE*);
        fprintf(TFile, "%s in file %s is cloned: %s\n",
                DEMANGLE(n->Name()), 
                n->Input_File_Name(), 
                IPA_Node_Name(clone_node));
      }
#ifdef KEY
      if (IPA_Clone_List_Actions)
      {
        extern char* IPA_Node_Name(IPA_NODE*);
        fprintf(stderr, "%s is cloned (%s) for call from %s\n",
                DEMANGLE(n->Name()), 
                IPA_Node_Name(clone_node),
		DEMANGLE (IPA_Call_Graph->Caller (clone_edges[i])->Name()));
        Clone_Trace (clone_node, clone_edges[i]);
      }
#endif
    }

    extern BOOL IPA_Should_Rebuild_DFN;
    IPA_Should_Rebuild_DFN = TRUE;
    *change = TRUE;
  }

  return 0;
}

//-------------------------------------------------------
// trans operation for forward constant propagation
// in this case walk the successors of this node and update
// the edge info using the node annotation
//-------------------------------------------------------
void*
IPA_CPROP_DF_FLOW::Trans ( void *, void *, void *vertex, INT *change )
{
  IPA_NODE *n = (IPA_NODE*)vertex;

  MEM_POOL_Push (&Ipa_cprop_pool);

  if (Intra_PU_Formal_Cprop (n)) {
    *change = TRUE;
  }

  if (IPA_Enable_Common_Const) {
    if (Intra_PU_Global_Cprop(n)) {
      *change = TRUE;
    }
  }

  MEM_POOL_Pop (&Ipa_cprop_pool);

  return 0;
}

//-------------------------------------------------------
// print each annotation
//-------------------------------------------------------
void
IPA_CPROP_DF_FLOW::Print_entry ( FILE *fp, void *, void *node )
{
  if (node) {
    IPA_NODE* n = (IPA_NODE*) node;
    if (n->Has_Propagated_Const()) {
      Print_constants(fp, n);
      if (IPA_Enable_Common_Const) {
        GLOBAL_ANNOT* gannot = n->Global_Annot();
        if (gannot && !gannot->Bottom()) {
          fprintf(fp, "\n%s common block constants:\n", DEMANGLE(n->Name()));
          gannot->Print(fp);
        }
      }
    }
  }
}


//-------------------------------------------------------
// print constants only. NOTE: this routine is shared by
// the Trace routines as well as the user option IPA:feedback.
// To avoid the chance that trace clutter might creep in to
// the user's ...ipa.con (feedback file for constants), a 
// decidedly duplicate but more user friendly  version exists.
//-------------------------------------------------------
void 
IPA_CPROP_DF_FLOW::Print_constants(FILE* fp, IPA_NODE* n)
{
  VALUE_DYN_ARRAY* outc = n->Cprop_Annot();
  if (outc == NULL || outc == (VALUE_DYN_ARRAY*) -1) return;

  INT idx = outc->Lastidx();
  INT i;

#ifdef TODO
  if( fp == IPA_Feedback_con_fd ) {
    BOOL has_printed = FALSE;
    for (i = 0; i <= idx; i++) {
      SUMMARY_VALUE *value = &((*outc)[i]);
	
      if (value->Is_constant ()) {
        if (has_printed)
          fputs (", ", fp);
        else
          fprintf (fp, "\n\nProcedure '%s' formal args:\n ", 
                   DEMANGLE (n->Name()));
	    
        fprintf (fp, "  #%d = ", i+1);
        if (value->Is_addr_of ()) {
          fprintf (fp, "&%s",
                   ST_name (St_Table[value->Get_global_st_idx ()]));
        } else
          value->Print_const_value (fp);
        has_printed = TRUE;
      }
    }
    if (has_printed)
      fputc ('\n', fp);
  }
  else {
#endif // TODO

    fprintf(fp, "%s formal constants:\n", DEMANGLE(n->Name()));
    for (i = 0; i <= idx; i++) {
      SUMMARY_VALUE *value = &((*outc)[i]);
      if (value->Is_constant ()) {
        fprintf (fp, "  F#%d: ", i);
	value->Print_const_value (fp);
        fputs("\n", fp);
        ++IPA_Constant_Count;   
      }
    }

#ifdef TODO
  }
#endif
}


//-----------------------------------------------------------------------
// check to see if it has predecessors that is not an entry node
//-----------------------------------------------------------------------
BOOL
IPA_CPROP_DF_FLOW::Valid_predecessors(IPA_NODE* node)
{
    NODE_ITER v_iter(IPA_Call_Graph->Graph(), node->Node_Index()); 

    for (NODE_INDEX vtx = v_iter.First_Pred(); 
         vtx != -1; 
         vtx = v_iter.Next_Pred()) { 

	if (IPA_Call_Graph->Graph()->Node_User(vtx) != NULL) {
	    return TRUE;
	}
    }

    return FALSE; // only if no predecessors or pred is entry node
}

//-----------------------------------------------------------------------
// valid successors
//-----------------------------------------------------------------------
BOOL
IPA_CPROP_DF_FLOW::Valid_successors_without_cprop(IPA_NODE* node)
{
  VALUE_DYN_ARRAY *cprop_annot;

  // has constants ?
  if (cprop_annot = node->Cprop_Annot()) {
    for (INT i=0; i<=cprop_annot->Lastidx(); ++i) {
      SUMMARY_VALUE *value = &((*cprop_annot)[i]);
      if (value->Is_constant ()) {
        return TRUE;
      }
    }
  }

  NODE_ITER v_iter(IPA_Call_Graph->Graph(), node->Node_Index()); 
  
  for (NODE_INDEX vtx = v_iter.First_Succ(); 
       vtx != -1; 
       vtx = v_iter.Next_Succ()) { 

    if (IPA_Call_Graph->Graph()->Node_User(vtx) != NULL) {
      return TRUE;
    }
  }

  return FALSE; // only if no successors or succ is exit node
}

//-----------------------------------------------------------------------
// clone a subroutine if I have performed constant propagation AND
// valuable constants have been found, but there are routines that are
// preemptible. By cloning such routines, we internalize the calls
// and are able to propagate the constants
// store all the nodes to be cloned in an array
//-----------------------------------------------------------------------
void
IPA_CPROP_DF_FLOW::PerformCloning (IPA_NODE* node)
{
    // don't clone alt. entry cause it is not a complete procedure.
    if (node->Summary_Proc()->Is_alt_entry() || 
        node->Summary_Proc()->Has_alt_entry())
	return;

    VALUE_DYN_ARRAY* cprop_annot = node->Cprop_Annot();

    if (cprop_annot == NULL || cprop_annot == (VALUE_DYN_ARRAY *) -1)
	return;

    BOOL fix_caller = FALSE;

    if (node->Is_Externally_Callable()) {
	// if the node has constants and is externally callable then
	// it is a candidate for cloning

	for (INT i=0; i<=cprop_annot->Lastidx(); ++i) {
	    if ((*cprop_annot)[i].Is_constant()) {
		// don't add it to the constant list if it
		// is in the cloned list

		node->Set_Clone_Candidate();

		// make sure to set the no_aggr bit for successors
                // of this node

		NODE_ITER v_iter(IPA_Call_Graph->Graph(), node->Node_Index());
		for (NODE_INDEX to = v_iter.First_Succ(); 
                     to != -1; 
		     to = v_iter.Next_Succ())
		  {
		    IPA_NODE *to_node =  IPA_Call_Graph->Graph()->Node_User(to);
		    if (to_node)
			to_node->Set_No_Aggr_Cprop();
		  }

#ifdef TODO
		Total_Prog_Size += node->Weight ();
#endif
		fix_caller = TRUE;
		break;
	    }
	}
    } else /* TODO:  if (!node->Skip()) */ {
	// if the node is not a candidate for cloning, record to see if it
	// has constants  
	for (INT i=0; i<=cprop_annot->Lastidx(); ++i) {
	    if ((*cprop_annot)[i].Is_constant()) {
		node->Set_Propagated_Const();
		fix_caller = TRUE;
		break;
	    }
	}
    }

    /* mark all callers of this function so that we can fix up the call
       sites later */
    if (fix_caller) {
	IPA_PRED_ITER edge_iter(node);
	for (edge_iter.First(); !edge_iter.Is_Empty(); edge_iter.Next()) {
	    IPA_EDGE *edge = edge_iter.Current_Edge();
	    if (edge)
		edge->Set_Propagated_Const();
	}
    } else {
	CXX_DELETE (cprop_annot, Malloc_Mem_Pool);
	node->Set_Cprop_Annot (NULL);
        node->Clear_Propagated_Const();
    }
} // IPA_CPROP_DF_FLOW::PerformCloning


void
IPA_CPROP_DF_FLOW::Delete_dead_call (IPA_NODE* node)
{
#ifdef TODO
    if (node->Should_Be_Skipped())
	return;
#endif // TODO

    if (!IPA_Enable_DCE)
	return;

    IPA_SUCC_ITER edge_iter (node);

    for (edge_iter.First (); !edge_iter.Is_Empty(); edge_iter.Next()) {
	IPA_EDGE *e = edge_iter.Current_Edge();
	if (e && e->Cprop_Annot() == (void *) -1)
	    e->Set_Deletable ();
    }
    
    if (IPA_Enable_DFE && node->Cprop_Annot () == (void *) -1) {
	// node is deleted
	if (!node->Is_Undeletable())
	    node->Set_Deletable ();
    }

} // IPA_CPROP_DF_FLOW::Delete_dead_call


// If the value of a function pointer is known, replace the indirect call
// by a direct call.  Update the call graph
static void
Connect_indirect_call (IPA_NODE* caller)
{
    
    if (caller->Should_Be_Skipped())
	return;

    const IPA_ICALL_LIST& icall_list = caller->Icall_List ();

    for (IPA_ICALL_LIST::const_iterator icall_iter = icall_list.begin ();
	 icall_iter != icall_list.end (); ++icall_iter) {

	SUMMARY_CALLSITE *call = (*icall_iter)->Callsite();

	if (!call->Is_func_ptr ())
	    continue;

	SUMMARY_VALUE &value = (*icall_iter)->Value();
	if (!value.Is_addr_of () || !value.Is_global ())
	    continue;

	ST* func_st = &St_Table[value.Get_global_st_idx ()];

	Is_True (ST_class (func_st) == CLASS_FUNC,
		 ("expecting a function symbol"));

	if (ST_is_weak_symbol (func_st))
	    func_st = ST_strong (func_st);

	NODE_INDEX callee_idx = AUX_PU_node (Aux_Pu_Table[ST_pu (func_st)]);

	if (callee_idx != INVALID_NODE_INDEX) {
          (void) IPA_Call_Graph->Add_New_Edge (call,
                                               caller->Node_Index (),
                                               callee_idx);
        }
        else {
          IPA_ICALL_NODE* opq = CXX_NEW(IPA_ICALL_NODE(call), Malloc_Mem_Pool);
          caller->Ocall_List().push_back(opq);
        }

#ifdef TODO
	if (call->Has_callsite_freq ())
	    Total_call_freq += call->Get_frequency_count ();
#endif
	Clear_ST_is_not_used (func_st);
    }
} // Connect_indirect_call  

//-----------------------------------------------------------------------
// at the end of the data flow problem, call the post process routine
// which is responsible for setting up the TCON array that needs to be
// written out and the constant propagation annotation
//-----------------------------------------------------------------------
void
IPA_CPROP_DF_FLOW::PostProcessIO (void *n)
{
  if (!n) return;
  IPA_NODE* node = (IPA_NODE*) n;

  Delete_dead_call(node);
  PerformCloning(node);

  if (! node->Icall_List ().empty ())
      Connect_indirect_call(node);

  if (IPA_Enable_Common_Const) {
    Map_Global_Constants(node);
  }

  if (Get_Trace(TP_PTRACE1, TP_PTRACE1_IPA_CPROP)) {
    Ipa_cprop_tlog (node);
  }
}


UINT32 GLOBAL_ANNOT::Size;
ST_IDX* GLOBAL_ANNOT::Common_ST;
OFFSET_SIZE_TO_ST_IDX_MAP* GLOBAL_ANNOT::Offset_Size_To_ST;

// ----------------------------------
// Constructor for IPA_CPROP_DF_FLOW
// Determine the size of GLOBAL_ANNOT
// ----------------------------------
IPA_CPROP_DF_FLOW::IPA_CPROP_DF_FLOW (DF_DIRECTION ddf, MEM_POOL* m) 
  : IPA_DATA_FLOW (ddf, m) 
{
  // There's not much sense in propagating global constants
  // if we don't have basic mod/ref information
  if (!IPA_Enable_Simple_Alias) {
    IPA_Enable_Common_Const = FALSE;
  }
  
  if (IPA_Enable_Common_Const) {
    ST* st;
    INT i;

    // count the number of common blocks for which we
    // may be able to propagate constants
    UINT32 count = 0;
    FOREACH_SYMBOL (GLOBAL_SYMTAB, st, i) {
      if (ST_class(st) == CLASS_VAR &&
          ST_sclass(st) == SCLASS_COMMON &&
          ST_base_idx(st) == ST_st_idx(st) &&
          !ST_addr_saved(st) && 
          !AUX_ST_flags(Aux_St_Table[ST_st_idx(st)], COMMON_USED_IN_IO)) {
        ++count;
      }
    }

    GLOBAL_ANNOT::Size = count;

    if (count > 0) {

      // fill in Common_ST array with ST_IDXs of common blocks
      GLOBAL_ANNOT::Common_ST = CXX_NEW_ARRAY(ST_IDX, count,  Malloc_Mem_Pool);
      count = 0;
      FOREACH_SYMBOL (GLOBAL_SYMTAB, st, i) {
        if (ST_class(st) == CLASS_VAR &&
            ST_sclass(st) == SCLASS_COMMON &&
            ST_base_idx(st) == ST_st_idx(st) &&
            !ST_addr_saved(st) && 
            !AUX_ST_flags(Aux_St_Table[ST_st_idx(st)], COMMON_USED_IN_IO)) {
          
          GLOBAL_ANNOT::Common_ST[count++] = ST_st_idx(st);
        }
      }
      
      // initialize maps from (offset,size) pairs to ST_IDXs
      // for common block elements
      GLOBAL_ANNOT::Offset_Size_To_ST = 
        CXX_NEW_ARRAY(OFFSET_SIZE_TO_ST_IDX_MAP, count, Malloc_Mem_Pool);
      
      FOREACH_SYMBOL (GLOBAL_SYMTAB, st, i) {
        if (ST_class(st) == CLASS_VAR && 
            ST_sclass(st) == SCLASS_COMMON &&
            ST_base(st) != st &&
            ST_base(ST_base(st)) == ST_base(st) &&
            !ST_addr_saved(ST_base(st)) && 
            !AUX_ST_flags(Aux_St_Table[ST_base_idx(st)], COMMON_USED_IN_IO) &&
            MTYPE_is_integral(TY_mtype(ST_type(st)))) {

          INT32 common_index = GLOBAL_ANNOT::Index(ST_base_idx(st));
          OFFSET_SIZE_TO_ST_IDX_MAP& map = 
            GLOBAL_ANNOT::Offset_Size_To_ST[common_index];
          map[OFFSET_SIZE(ST_ofst(st), TY_size(ST_type(st)))] = ST_st_idx(st);
        }
      }
    }
    else {
      IPA_Enable_Common_Const = FALSE;
    }
  }
}
  
