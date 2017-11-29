/*
 * Copyright 2003, 2004, 2005 PathScale, Inc.  All Rights Reserved.
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


#include <stdint.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <alloca.h>

#include "defs.h"
#include "mempool.h"
#include "tracing.h"
#include "cxx_template.h"
#include "wn.h"
#include "wn_simp.h"			// WN_Simplify
#include "wn_util.h"                    // WN_ITER
#include "lwn_util.h"                   // LWN_Get_Parent

#include "ipc_symtab_merge.h"		// IPC_GLOBAL_IDX_MAP
#include "ipl_summary.h"		// summary info data structures
#include "ipa_option.h"                 // IPA trace flags
#include "ipa_cg.h"			// call graph
#include "ipa_cprop.h"			// VALUE_DYN_ARRAY
#include "ipa_summary.h"		// summary info access routines
#include "ipaa.h"			// IPAA_NODE_INFO 
#include "ipo_defs.h"                   // IPA_NODE_CONTEXT

#include "ipa_cprop_annot.h"

/* status of conditional statements */
enum CONDITION_STATE {
    CD_UNKNOWN = 0,			// can't tell
    CD_TRUE = 1,			// evaluated to TRUE
    CD_FALSE = 2,			// evaluated to FALSE
    CD_DEAD = 3,			// stmt never executed
};

CONDITION_STATE *cd_status;		// status of all conditional stmts
					// in a PU

MEM_POOL Ipa_cprop_pool;		// mem pool for expression evaluations
MEM_POOL local_cprop_pool;

// used in Evaluate_chi, which needs the caller's IPA_NODE for mapping a
// callsite id to callee. 
static IPA_NODE *Current_caller;
static IPA_NODE **callsite_map;

// in order to combine global and formal constants 
// Evaluate_value needs access to global const annotations
static GLOBAL_ANNOT *current_gannot;

static SUMMARY_PROCEDURE *ipa_proc;
static SUMMARY_CALLSITE *ipa_callsite;
static SUMMARY_FORMAL *ipa_formal;
static SUMMARY_ACTUAL *ipa_actual;
static SUMMARY_VALUE *ipa_value;
static SUMMARY_EXPR *ipa_expr;
static SUMMARY_PHI *ipa_phi;
static SUMMARY_CHI *ipa_chi;
static const SUMMARY_SYMBOL *ipa_symbol;
static SUMMARY_STMT *ipa_stmt;
static SUMMARY_CONTROL_DEPENDENCE *ipa_ctrl_dep;
static SUMMARY_STID *ipa_stid;

// The array of propagated formal parameter values
static VALUE_DYN_ARRAY *formal_value;

// Array keeping the temp. values in expression evaluation
typedef SEGMENTED_ARRAY<SUMMARY_VALUE> VALUE_VECTOR;
static VALUE_VECTOR* eval_value;

struct eval_hasher
{
    size_t operator()(const void* s) const {
	return reinterpret_cast<size_t>(s);
    }
};

typedef __gnu_cxx::hash_map<const void *, UINT32, eval_hasher, std::equal_to<const void *>,
    mempool_allocator<pair<const void *, UINT32> > > EVAL_HASH;
static EVAL_HASH *eval_hash;

static void
Evaluate_chi (const SUMMARY_CHI *chi, SUMMARY_VALUE &return_value);

static void
Evaluate_phi (const SUMMARY_PHI *phi, SUMMARY_VALUE &return_value);

static void
Evaluate_expr (const SUMMARY_EXPR *expr, SUMMARY_VALUE &return_value);

#ifdef KEY
static void
Evaluate_value (const SUMMARY_VALUE&, SUMMARY_VALUE&, WN * = NULL);
#else
static void
Evaluate_value (const SUMMARY_VALUE& value, SUMMARY_VALUE &return_value);
#endif


//-----------------------------
// set the summary information
//-----------------------------
static  void  
Set_summary_info(const IPA_NODE* node)
{
  ipa_proc = node->Summary_Proc();

  ipa_callsite     = IPA_get_callsite_array (node);
  ipa_actual       = IPA_get_actual_array (node);
  ipa_symbol       = IPA_get_symbol_array (node);
  ipa_formal       = IPA_get_formal_array (node);
  ipa_ctrl_dep     = IPA_get_ctrl_dep_array (node);
  ipa_stmt         = IPA_get_stmt_array (node);
  ipa_value        = IPA_get_value_array (node);
  ipa_phi          = IPA_get_phi_array (node);
  ipa_chi          = IPA_get_chi_array (node);
  ipa_expr         = IPA_get_expr_array (node);
  ipa_stid         = IPA_get_stid_array (node);
}


//-------------------------------
// are two summary values equal?
//-------------------------------
static inline BOOL
Values_are_equal(const SUMMARY_VALUE* v1, const SUMMARY_VALUE* v2)
{
    if (v1 == v2) {
	return TRUE;
    }
  
    if (v1->Is_int_const() && v2->Is_int_const()) {
	return (v1->Get_int_const_value() == v2->Get_int_const_value());
    }

    if (v1->Is_const_st() && v2->Is_const_st()) {
	Is_True(v1->Is_merged_const_st_idx() && v2->Is_merged_const_st_idx(),
		("const_st values can be compared only after merging"));
	return (v1->Get_const_st_idx() == v2->Get_const_st_idx() &&
		v1->Get_tcon_idx () == v2->Get_tcon_idx ());
    }

    // if we reach here then return false
    return FALSE; 
}


static inline BOOL
Compatible_Mtypes (TYPE_ID mtype1, TYPE_ID mtype2)
{
    if (mtype1 == mtype2)
	return TRUE;
    return (MTYPE_is_integral (mtype1) && MTYPE_is_integral (mtype2));
} // Compatible_Mtypes


// check if the mtypes of the actual and formal parameters are compatible
static BOOL
Compatible_Parameters (TYPE_ID formal_mtype, BOOL is_ref_param,
		       const SUMMARY_VALUE& actual)
{
    if (!actual.Is_addr_of ()) {
	if (is_ref_param)
	    return MTYPE_is_pointer (actual.Get_mtype ());
	else
	    return Compatible_Mtypes (formal_mtype, actual.Get_mtype ());
    } else {
	if (is_ref_param)
	    return Compatible_Mtypes (formal_mtype, actual.Target_mtype ());
	else
	    return MTYPE_is_pointer (formal_mtype);
    }
} // Compatible_Parameters


//----------------------------------------------------------------
// Update the node information for VALUE_CONST
//----------------------------------------------------------------
static BOOL
Update_node_const (SUMMARY_VALUE& formal, const SUMMARY_VALUE& actual,
		   BOOL is_ref_parm)
{
    BOOL formal_update = TRUE;

    Is_True (formal.Is_merged_const_st_idx () &&
	     actual.Is_merged_const_st_idx (),
	     ("Invalid ST_IDX for VALUE_CONST"));
    if (! formal.Is_addr_of ()) {
	// scalar is passed, just compare the values
	if (formal.Get_tcon_idx () == actual.Get_tcon_idx ())
	    return FALSE;
    } else {
	// an address is passed
	if (formal.Get_const_st_idx () != 0 &&
	    formal.Get_const_st_idx () == actual.Get_const_st_idx ()) {
	    // same address, must be ok
	    if (formal.Is_convertible_to_global () &&
		!actual.Is_convertible_to_global ()) {
		formal.Clear_convertible_to_global ();
		return TRUE;
	    }
	    return FALSE;
	} else if (is_ref_parm) {
	    // for reference parameter, it might be ok as long as the
	    // values are the same, even when the addresses are not.
	    if (formal.Get_tcon_idx () == actual.Get_tcon_idx ()) {
		// The values are equal, we can still propagate the constant
		// value, but we cannot substitute the formal
		if (formal.Is_convertible_to_global () ==
		    actual.Is_convertible_to_global ()) {
		    
		    if (formal.Get_const_st_idx () != 0) {
			formal.Set_const_st_idx (0, actual.Get_tcon_idx ());
			return TRUE;
		    } else
			return FALSE;
		}
	    }
	}
    }

    formal.Set_not_const ();
    return TRUE;
} // Update_node_const


// check if the value of two global symbols is the same
static BOOL
Same_Global_Value (ST_IDX st_idx1, ST_IDX st_idx2)
{
    if (st_idx1 == st_idx2)
	return TRUE;

    Is_True (ST_IDX_level (st_idx1) == GLOBAL_SYMTAB &&
	     ST_IDX_level (st_idx2) == GLOBAL_SYMTAB,
	     ("Expecting global symbols"));

    const ST& st1 = St_Table[st_idx1];
    const ST& st2 = St_Table[st_idx2];

    Is_True (ST_is_const_var (st1) && ST_is_const_var (st2),
	     ("Variable is not constant"));

    if (ST_type (st1) != ST_type (st2))
	return FALSE;

    BOOL st1_is_zero = !ST_is_initialized (st1) || ST_init_value_zero (st1);
    BOOL st2_is_zero = !ST_is_initialized (st2) || ST_init_value_zero (st2);

    if (st1_is_zero != st2_is_zero)
	return FALSE;

    if (st1_is_zero)
	return TRUE;		      

    // both are initialized variables
    ST_TO_INITO_MAP::const_iterator iter = ST_To_INITO_Map.find (st_idx1);
    if (iter == ST_To_INITO_Map.end ())
	return FALSE;
    INITV_IDX initv1 = INITO_val (Inito_Table[(*iter).second]);

    iter = ST_To_INITO_Map.find (st_idx2);
    if (iter == ST_To_INITO_Map.end ())
	return FALSE;
    INITV_IDX initv2 = INITO_val (Inito_Table[(*iter).second]);

    return initv1 == initv2;

} // Same_Global_Value


//-------------------------------------------------------
// update the formal annotation based on the actual node
//-------------------------------------------------------
static BOOL
Update_node(SUMMARY_VALUE& formal, const SUMMARY_VALUE& actual,
	    TYPE_ID formal_type, BOOL is_ref_parm)
{
    // should we verify type compatibility here?

    if (formal.Is_not_const ())
	return FALSE;

    if (! Compatible_Parameters (formal_type, is_ref_parm, actual)) {
	formal.Set_not_const ();
	return TRUE;
    }
	
    if (formal.Is_unknown ()) {
	formal = actual;
	return TRUE;
    }

    // compare actual values
    if (actual.Is_addr_of () != formal.Is_addr_of () ||
	actual.Get_const_type () != formal.Get_const_type ()) {
	formal.Set_not_const ();
	return TRUE;
    }

    switch (formal.Get_const_type ()) {
    case VALUE_INT_CONST:
	if (formal.Get_int_const_value () == actual.Get_int_const_value ())
	    return FALSE;
	break;
	
    case VALUE_CONST:
	return Update_node_const (formal, actual, is_ref_parm);

    case VALUE_GLOBAL:
	if (formal.Get_global_st_idx () == actual.Get_global_st_idx ())
	    return FALSE;
	if (!formal.Is_addr_of () && !is_ref_parm) {
	    // simple value parameter, check the value of the globals
	    if (Same_Global_Value (formal.Get_global_st_idx (),
				   actual.Get_global_st_idx ()))
		return FALSE;
	}
	break;

    default:
	break;
    }

    formal.Set_not_const ();
    return TRUE;
}


//--------------------------------------------------------------
// this routine does the following
// it looks at the edge information, and updates each of the
// formal parameter status based on the type 
// of the actual parameter node annotation
//--------------------------------------------------------------
extern BOOL
Union_Formal_Cprop_Annot (IPA_NODE* callee, IPA_EDGE *e)
{
    VALUE_DYN_ARRAY* formals = callee->Cprop_Annot();
    VALUE_DYN_ARRAY* actuals = e->Cprop_Annot();

    if (formals == NULL || actuals == NULL) {
	return FALSE;
    }

    // Get the mtype of the formals
    Set_summary_info (callee);

    // check to see there are more actuals than formals
    UINT32 count = (formals->Elements() < actuals->Elements()) ? 
	formals->Elements() : actuals->Elements();
  
    BOOL updated = FALSE;
    for (UINT32 pos = 0, i = ipa_proc->Get_formal_index(); 
	 pos < count; 
	 ++pos, ++i) {
	const SUMMARY_FORMAL& sum_formal = ipa_formal[i];
	TYPE_ID mtype = ipa_symbol[sum_formal.Get_symbol_index()].Get_btype();
	updated |= Update_node ((*formals)[pos], (*actuals)[pos], mtype,
				sum_formal.Is_ref_parm ());
    }

    return updated;
} // Union_Formal_Cprop_Annot


//--------------------------------------------------------------------
// Union formal annotations on quasi clone with those of original node
//--------------------------------------------------------------------
extern void
Union_Quasi_Clone_Cprop_Annot (IPA_NODE* origin, IPA_NODE* clone)
{
  VALUE_DYN_ARRAY* orig_annots = origin->Cprop_Annot();
  VALUE_DYN_ARRAY* clone_annots = clone->Cprop_Annot();

  if (orig_annots == NULL || clone_annots == NULL) {
    return;
  }
  
  // Get the mtype of the formals
  Set_summary_info (origin);    
  
  UINT32 count = orig_annots->Elements();

  for (UINT32 pos = 0, i = ipa_proc->Get_formal_index(); 
       pos < count; 
       ++pos, ++i) {
    const SUMMARY_FORMAL& sum_formal = ipa_formal[i];
    TYPE_ID mtype = ipa_symbol[sum_formal.Get_symbol_index()].Get_btype();
    Update_node((*orig_annots)[pos], (*clone_annots)[pos], mtype,
		sum_formal.Is_ref_parm ());
  }
} // Union_Quasi_Clone_Cprop_Annot


// -------------------------------------------------------------
// Check if two values are equivalent wrt constant propagation
// They must be either both non-constants or identical constants
// -------------------------------------------------------------
static BOOL
Values_are_cprop_equivalent(const SUMMARY_VALUE* val1,
                            const SUMMARY_VALUE* val2)
{
  if (val1 == val2) {
    return TRUE;
  }
  
  if (val1->Get_const_type () != val2->Get_const_type ()) {
    return FALSE;
  }

  switch (val1->Get_const_type ()) {
  
    case VALUE_INT_CONST:
      return (val1->Get_int_const_value() == val2->Get_int_const_value());
      
    case VALUE_CONST:
      Is_True(val1->Is_merged_const_st_idx() && val2->Is_merged_const_st_idx(),
              ("const_st values can be compared only after merging"));
      return (val1->Get_const_st_idx() == val2->Get_const_st_idx() &&
	      val1->Get_tcon_idx() == val2->Get_tcon_idx());

    case VALUE_NOT_CONST:
      return TRUE;
      
    case VALUE_FORMAL:
    case VALUE_GLOBAL:
    case VALUE_SYMBOL:
      if (val1->Is_addr_of ()) {
        return memcmp (val1, val2, sizeof(SUMMARY_VALUE)) == 0;
      }
  }

  return FALSE;
} // Values_are_cprop_equivalent

//---------------------------------------------------------
// Check if two IPA_EDGEs are equivalent with respect to
// constant actual arguments that are passed to the callee
// TODO: nenad, 12/31/97
// This really should be a method on IPA_EDGE
//---------------------------------------------------------
extern BOOL
Edges_Have_Equiv_Cprop_Annots(const IPA_EDGE* e1,
                              const IPA_EDGE* e2)
{
  if (e1 == e2) {
    return TRUE;
  }
  
  VALUE_DYN_ARRAY* args1 = e1->Cprop_Annot();
  VALUE_DYN_ARRAY* args2 = e2->Cprop_Annot();

  if (args1 == NULL || args2 == NULL ||
      args1 == (void*)-1 || args2 == (void*)-1) {
    return FALSE;
  }

  // For edges to be equivalent their argument counts must match
  UINT32 count = args1->Elements();
  if (count != args2->Elements()) {
    return FALSE;
  }

  for (UINT32 i = 0; i < count; ++i) {
    if (!Values_are_cprop_equivalent(&((*args1)[i]), &((*args2)[i]))) {
      return FALSE;
    }
  }
  
  return TRUE;
}


//------------------------------------------------------------------
// This routine for updating cprop node annotation when the actual
// argument is an integral constant is similar to Update_node_int_const,
// but it is more precise in the sense that it will request
// cloning when formal and actual represent two different constants.
//------------------------------------------------------------------
static IPA_CLONING_ACTION
Precise_int_const_node_update(SUMMARY_VALUE* formal, 
                              SUMMARY_VALUE* actual)
{
  if (formal->Is_unknown()) {
    // unknown (TOP) is the initial value that the state is set to
    *formal = *actual;
    return ANNOTATION_CHANGED;
  }

  if (formal->Is_int_const() &&
      formal->Get_int_const_value() == actual->Get_int_const_value()) {
    return NO_CHANGE;
  }
  
  return NEEDS_CLONING;
}

//------------------------------------------------------------------
// Updating cprop node annotation when the actual argument is an 
// integral const_st (much like Precise_int_const_node_update).
//------------------------------------------------------------------
static IPA_CLONING_ACTION
Precise_const_st_node_update(SUMMARY_VALUE* formal, 
                             SUMMARY_VALUE* actual)
{
  if (formal->Is_unknown()) {
    // unknown (TOP) is the initial value that the state is set to
    *formal = *actual;
    return ANNOTATION_CHANGED;
  }
  
  if (formal->Is_const_st() &&
      formal->Get_const_st_idx() == actual->Get_const_st_idx() &&
      formal->Get_tcon_idx() == actual->Get_tcon_idx ()) {
    return NO_CHANGE;
  }
  
  return NEEDS_CLONING;
}

//-----------------------------------------------------------------
// This routine for updating cprop annotation of a formal parameter
// based on the actual argument passed is very similar to the 
// standard Update_node version, but it is more precise in that
// it will request cloning (instead of going to BOTTOM) when two 
// different INTEGRAL constants are encountered.
//-----------------------------------------------------------------
static IPA_CLONING_ACTION
Precise_node_update (SUMMARY_VALUE* formal, 
                     SUMMARY_VALUE* actual, 
                     TYPE_ID formal_type,
		     BOOL is_ref_parm)
{
  if (MTYPE_is_integral(formal_type)) {
    if (actual->Is_int_const()) {
      return Precise_int_const_node_update(formal, actual);
    }
    if (actual->Is_const_st() && MTYPE_is_integral(actual->Get_mtype())) {
      return Precise_const_st_node_update(formal, actual);
    }
  }

  if (Update_node (*formal, *actual, formal_type, is_ref_parm)) {
    return ANNOTATION_CHANGED;
  } 
  
  return NO_CHANGE;
}

// --------------------------------------------------------
// Should the callee be cloned to increase the potential of 
// finding constants for the formal at the given position
// --------------------------------------------------------
static BOOL
Clone_for_this_formal(const IPA_NODE* callee,
                      UINT32 position)
{
  // if cloning was explicitly requested, always return TRUE
  // this needs tuning to find the right heuristics for cloning
  if (IPA_Max_Node_Clones_Set) {
    return TRUE;
  }

  // otherwise, clone only to improve array section analysis
  INT32 f_idx = callee->Summary_Proc()->Get_formal_index() + position;
  INT32 s_idx = IPA_get_formal_array(callee)[f_idx].Get_symbol_index();
  return IPA_get_symbol_array(callee)[s_idx].Used_in_array_section();
}

//--------------------------------------------------------------
// This routine for unioning node annotations is very similar
// to the standard Union_node_annot, but it is more precise
// because it will request cloning when two different constants
// (whose meet would normally produce BOTTOM) are encountered.
//--------------------------------------------------------------
extern IPA_CLONING_ACTION
Union_Formal_Cprop_Annot_With_Cloning (IPA_NODE* callee, 
                                       IPA_EDGE* edge)
{
  VALUE_DYN_ARRAY* formals = callee->Cprop_Annot();
  VALUE_DYN_ARRAY* actuals = edge->Cprop_Annot();

  if (formals == NULL || actuals == NULL) {
    return NO_CHANGE;
  }

  // check to see if there are more actuals than formals
  UINT32 count = (formals->Elements() < actuals->Elements()) ? 
                  formals->Elements() : actuals->Elements();
  
  Set_summary_info (callee);

  IPA_CLONING_ACTION action = NO_CHANGE;
  for (UINT32 pos = 0, formal_idx = ipa_proc->Get_formal_index(); 
       pos < count; 
       ++pos, ++formal_idx) {

    const SUMMARY_FORMAL& sum_formal=ipa_formal[formal_idx];
    const SUMMARY_SYMBOL& formal_sym=ipa_symbol[sum_formal.Get_symbol_index()];
    TYPE_ID mtype = formal_sym.Get_btype();
    BOOL is_ref_param = sum_formal.Is_ref_parm();
    
    // This formal is a candidate for cloning
    if (Clone_for_this_formal(callee, pos)) {
      IPA_CLONING_ACTION node_action = Precise_node_update(&(*formals)[pos], 
                                                           &(*actuals)[pos], 
                                                           mtype, 
                                                           is_ref_param);
      // 3-way logic: NO_CHANGE, ANNOTATION_CHANGED, NEEDS_CLONING
      if (node_action == NEEDS_CLONING) {
        action = NEEDS_CLONING;
      }
      else if (action == NO_CHANGE) {
        action = node_action;
      }
    }
    else {
      if (Update_node((*formals)[pos], (*actuals)[pos], mtype, is_ref_param) &&
          action == NO_CHANGE) {
        action = ANNOTATION_CHANGED;
      }
    }
  }

  return action;

} // Precise_node_annot_union



// convert the value to and integer constant, if possible
// for const global variable, try to convert to integer constant or a tcon
static void
Simplify_value (SUMMARY_VALUE& value)
{
  if (value.Is_addr_of ())
      return;

  if (value.Is_global ()) {
    const ST& st = St_Table[value.Get_global_st_idx ()];
    Is_True (ST_is_const_var (st),
	     ("Non-constant global variables found in constant propagation"));

    const TY& ty = Ty_Table[ST_type (st)];
    if (TY_kind (ty) != KIND_SCALAR && TY_kind (ty) != KIND_POINTER)
      return;

    // readonly global variable
    if (!ST_is_initialized (st) || ST_init_value_zero (st)) {
      if (MTYPE_is_integral (value.Get_mtype ())) {
	value.Set_int_const ();
	value.Set_int_const_value (0);
      } else if (value.Get_mtype () != MTYPE_M) {
	value.Set_const_st ();
	TCON_IDX tcon_idx =
	  Enter_tcon (Host_To_Targ_Float (value.Get_mtype (), (double) 0));
	value.Set_const_st_idx (ST_st_idx (st), tcon_idx);
	value.Set_merged_const_st_idx ();
      }
      return;
    } 
    else {

      if (ST_base_idx (st) != ST_st_idx (st))
	return;				// can't handle common block element.

      // initialized global variable, find the corresponding INITO and see
      // if it can be converted to integer constant
      ST_TO_INITO_MAP::const_iterator iter =
	ST_To_INITO_Map.find (ST_st_idx (st));
      FmtAssert (iter != ST_To_INITO_Map.end (),
		 ("Missing value for initialized global variable %s",
		  ST_name (st)));
      const INITO& inito = Inito_Table[(*iter).second];
      Is_True (INITO_st_idx (inito) == ST_st_idx (st),
	       ("Mismatched inito and initialized st"));
      const INITV& initv = Initv_Table[INITO_val (inito)];

      switch (INITV_kind (initv)) {
      case INITVKIND_ZERO:
	value.Set_int_const ();
	value.Set_int_const_value (0);
	return;

      case INITVKIND_ONE:
	value.Set_int_const ();
	value.Set_int_const_value (1);
	return;

      case INITVKIND_VAL:
	value.Set_const_st ();
	value.Set_const_st_idx (ST_st_idx (st), INITV_tc (initv));
	value.Set_merged_const_st_idx ();
	// break out to see if it can further be convertd to integer constant
	break;

      case INITVKIND_SYMOFF:
	  if (TY_kind (ty) != KIND_POINTER)
	      return;
	  if (INITV_ofst (initv) != 0)
	      return;
	  value.Set_is_addr_of ();
	  value.Set_global_st_idx (INITV_st (initv));
	  break;
	  
      default:
	return;
      }
    }
  }

  if (! MTYPE_is_integral (value.Get_mtype ()))
    return;

  if (value.Is_const_st ()) {
    TCON& tc = Tcon_Table[value.Get_tcon_idx()];
    INT64 v = Targ_To_Host (tc);
    value.Set_int_const ();
    value.Set_int_const_value (v);
  } 
} // Simplify_value


// Map a callsite idx to the corresponding IPA_NODE of the callee
// If the symbol is not passed as parameter at this call, return -1
// If the callee node cannot be found, return NULL
static IPA_NODE *
Get_callee (SUMMARY_CALLSITE *callsite, INT sym_idx, BOOL must_be_an_actual)
{
  if (must_be_an_actual) {

    // first, check if this symbol is passed as parameter in this call
    SUMMARY_ACTUAL *actuals =&ipa_actual[callsite->Get_actual_index()];
    BOOL found = FALSE;
    for (INT i = 0; i < callsite->Get_param_count (); i++) {
      if (actuals[i].Get_symbol_index () == sym_idx &&
          actuals[i].Get_pass_type () == PASS_LDA) {
        found = TRUE;
        break;
      }
    }
    
    if (!found) return (IPA_NODE *) -1;
  }
    
  static SUMMARY_CALLSITE* base;
    
  if (callsite_map == 0) {
    // build the map if not already done so
    INT size = sizeof(IPA_NODE *) * ipa_proc->Get_callsite_count ();
    callsite_map = (IPA_NODE **) MEM_POOL_Alloc (&Ipa_cprop_pool, size);
    bzero (callsite_map, size);
	
    base = &ipa_callsite[ipa_proc->Get_callsite_index()];

    IPA_SUCC_ITER edge_iter (Current_caller);
    for (edge_iter.First (); !edge_iter.Is_Empty(); edge_iter.Next()) {
      IPA_EDGE* e = edge_iter.Current_Edge();
      if (e) {
        INT idx = e->Summary_Callsite() - base;
        IPA_NODE* callee = IPA_Call_Graph->Callee(e);
        callsite_map[idx] = callee;
      }
    }
  }

  Is_True(callsite >= base,
          ("Get_callee: negative callsite index %d", callsite - base));

  return callsite_map[callsite - base];
} // Get_callee


static void
Get_chi_operand (const SUMMARY_CHI *chi, SUMMARY_VALUE &return_value)
{
    if (chi->Is_chi_value ()) {
	const SUMMARY_VALUE& value = ipa_value[chi->Get_node_index ()];
	Evaluate_value (value, return_value);
    } else if (chi->Is_chi_expr ()) {
	SUMMARY_EXPR *expr = ipa_expr + chi->Get_node_index ();
	Evaluate_expr (expr, return_value);
    } else if (chi->Is_chi_phi ()) {
	SUMMARY_PHI *phi = ipa_phi + chi->Get_node_index ();
	Evaluate_phi (phi, return_value);
    } else if (chi->Is_chi_chi ()) {
	SUMMARY_CHI *sub_chi = ipa_chi + chi->Get_node_index ();
	Evaluate_chi (sub_chi, return_value);
    } else {
	return_value.Set_not_const ();
	(*eval_hash)[chi] = 0;
	return;
    }

    (*eval_hash)[chi] = eval_value->Insert (return_value);
} // Get_chi_operand


// check for three conditions:
// 1) the symbol is passed as actual parameter
// 2) the symbol is passed by reference (LDA)
// 3) the symbol is not modified by the callee
static void
Process_formal_imod (const SUMMARY_CALLSITE *callsite,
		     const IPAA_NODE_INFO *callee_info,
		     const SUMMARY_CHI *chi,
		     SUMMARY_VALUE &return_value)
{
    SUMMARY_ACTUAL *actuals = ipa_actual + callsite->Get_actual_index ();
    
    INT count = callsite->Get_param_count ();
    if (count > callee_info->Get_fcount ())
	count = callee_info->Get_fcount ();
    
    while (count--) {
	if (chi->Get_symbol_index () == actuals[count].Get_symbol_index () &&
	    actuals[count].Get_pass_type () == PASS_LDA &&
	    callee_info->Is_formal_imod_elmt (count)) {

	    return_value.Set_not_const ();
	    (*eval_hash)[chi] = 0;
	    return;
	}
    }

    Get_chi_operand (chi, return_value);
} // Process_formal_imod


// check if the given symbol specified in this chi node is modified by this
// particular callsite
static void
Process_reference_parameter (const SUMMARY_CHI *chi,
			     SUMMARY_VALUE &return_value)
{
    SUMMARY_CALLSITE *callsite = ipa_callsite + chi->Get_call_index ();
    IPA_NODE *callee = Get_callee (callsite, chi->Get_symbol_index (), TRUE);

    if (callee == (IPA_NODE *) -1) {
	// this symbol is not passed as parameter
	Get_chi_operand (chi, return_value);
	return;
    }

    if (callee == NULL) {
	return_value.Set_not_const ();
	(*eval_hash)[chi] = 0;
	return;
    }
    
    Process_formal_imod (callsite, callee->Mod_Ref_Info(), chi, return_value);
    
} //Process_reference_parameter


static void
Evaluate_chi (const SUMMARY_CHI *chi, SUMMARY_VALUE &return_value)
{
    EVAL_HASH::const_iterator idx = eval_hash->find (chi);

    if (idx != eval_hash->end ()) {
	return_value = (*eval_value)[(*idx).second];
	return;
    }

    if (!IPA_Enable_Addressing || 
        !IPA_Enable_Simple_Alias ||
        Current_caller->Summary_Proc()->Is_alt_entry()) {
	return_value.Set_not_const ();
	return;
    }

    const SUMMARY_SYMBOL &sym = ipa_symbol[chi->Get_symbol_index ()];

    if (sym.Is_global ()) {
	// check if the global symbol is a known constant, if so, no need
	// to evaluate the chi node
	ST_IDX st_idx = sym.St_idx();
        if (ST_is_const_var(ST_ptr(st_idx))) {
          // can't handle pointers yet
          if (return_value.Is_addr_of()) { 
            return_value.Set_not_const ();
            (*eval_hash)[chi] = 0;
          }
          else {
            return_value.Set_global ();
            return_value.Set_global_st_idx (st_idx);
            return_value.Set_global_index (-1);
            
            TYPE_ID mtype = Mtype_comparison (TY_mtype (ST_type (st_idx)));
            return_value.Set_mtype (mtype);

            Simplify_value (return_value);

	    (*eval_hash)[chi] = eval_value->Insert (return_value);
          }
          return;
        }
    }

    if (sym.Is_static() ||		// see PV 477092
	sym.Is_addr_saved ()) {
	return_value.Set_not_const ();
	return;
    }

    if (sym.Is_local () && !sym.Is_common ()) {
	if (!sym.Is_addr_passed()) {
	    // should never happened, but ok if it does
	    Get_chi_operand (chi, return_value);
	    return;
	}


	// last hope:  if this symbol is passed by reference in this
	// particular callsite, and is not indirectly modified, we can
	// still proceed.
	Process_reference_parameter (chi, return_value);
	return;
    } else {
	// symbol is global and it is not constant

	ST* st = &St_Table[sym.St_idx ()];

	while (ST_base_idx (st) != ST_st_idx (st))
	    st = ST_base (st);
	
	if (ST_addr_saved (st)) {
	    return_value.Set_not_const ();
	    return;
	}
	
	// check if it is modified by this callsite
	
	IPA_NODE *callee = Get_callee (ipa_callsite + chi->Get_call_index (),
				       chi->Get_symbol_index (), FALSE);

	// if callee == NULL, the callee is defined in either a relocatable
	// object of a dso.  Since all global symbols that are used
	// externally are always marked addr_saved, and this is not the
	// case here, we know that this symbol cannot be modified directly
	// by this call.   But we still need to check if this symbol is
	// indireclty modified if this callee calls back to a function that 
	// modifies it.

	if (callee == NULL) {
	    if (icall_def != NULL &&
		! icall_def->Is_elmt (ST_IDX_index (ST_st_idx (st)))) {
		Get_chi_operand (chi, return_value);
		return;
	    }
	} else if (! callee->Mod_Ref_Info ()->
		   Is_def_elmt (ST_IDX_index (ST_st_idx (st)))) {
	    /* check if the symbol's address is passed, if so, check if it
	       is modified indirectly */

	    Process_formal_imod (ipa_callsite + chi->Get_call_index (),
				 callee->Mod_Ref_Info (), chi, return_value);
	    return;
	}

	return_value.Set_not_const ();
	(*eval_hash)[chi] = 0;
	return;
    }

} // Evaluate_chi


static void
Get_phi_operand (const SUMMARY_PHI *phi, INT operand,
		 SUMMARY_VALUE &return_value)
{
  if (phi->Is_value (operand)) {
    const SUMMARY_VALUE& value = ipa_value[phi->Get_node_index (operand)];
    Evaluate_value (value, return_value);
  } else if (phi->Is_expr (operand)) {
    SUMMARY_EXPR *expr = ipa_expr + phi->Get_node_index (operand);
    Evaluate_expr (expr, return_value);
  } else if (phi->Is_phi (operand)) {
    SUMMARY_PHI *sub_phi = ipa_phi + phi->Get_node_index (operand);
    Evaluate_phi (sub_phi, return_value);
  } else if (phi->Is_chi (operand)) {
    SUMMARY_CHI *chi = ipa_chi + phi->Get_node_index (operand);
    Evaluate_chi (chi, return_value);
  } else {
    // unknown value
    return_value.Set_not_const ();
    (*eval_hash)[phi] = 0;
    return;
  }

  (*eval_hash)[phi] = eval_value->Insert (return_value);
} // Get_phi_operand


static void
Evaluate_phi (const SUMMARY_PHI *phi, SUMMARY_VALUE &return_value)
{
    if (!IPA_Enable_Flow_Analysis) {
	return_value.Set_not_const ();
	return;
    }

    EVAL_HASH::const_iterator idx = eval_hash->find (phi);
    if (idx != eval_hash->end ()) {
	return_value = (*eval_value)[(*idx).second];
	return;
    }

    INT operand = -1;

    INT cd_idx = phi->Get_ctrl_dep_index (0) - ipa_proc->Get_ctrl_dep_index ();

    switch (cd_status[cd_idx]) {
    case CD_DEAD:
	operand = 1;
	break;
    case CD_FALSE:
	if (phi->Get_branch (0))
	    operand = 1;
	break;
    case CD_TRUE:
	if (!phi->Get_branch (0))
	    operand = 1;
	break;
    case CD_UNKNOWN:
	break;
    }

    if (operand == -1) {
	cd_idx = phi->Get_ctrl_dep_index (1) - ipa_proc->Get_ctrl_dep_index ();

	switch (cd_status[cd_idx]) {
	case CD_DEAD:
	    operand = 0;
	    break;
	case CD_FALSE:
	    if (phi->Get_branch (1))
		operand = 0;
	    break;
	case CD_TRUE:
	    if (!phi->Get_branch (1))
		operand = 0;
	    break;
	case CD_UNKNOWN:
	    break;
	}
    }

    if (operand == -1) {
	/* we don't know which operand to choose, but if they both turn out
	   to have the same value, we can still proceed */
	SUMMARY_VALUE v1, v2;
	v1.Init ();
	Get_phi_operand (phi, 0, v1);
	if (v1.Is_constant ()) {
	    v2.Init ();
	    Get_phi_operand (phi, 1, v2);
	    if (memcmp (&v1, &v2, sizeof(SUMMARY_VALUE)) == 0) {
		return_value = v1;
		return;
	    }
	}
	return_value.Set_not_const ();
	(*eval_hash)[phi] = 0;
	return;
    }

    Get_phi_operand (phi, operand, return_value);

} // Evaluate_phi


static void
Get_expr_operand (const SUMMARY_EXPR *expr, INT operand,
		  SUMMARY_VALUE &return_value) 
{
  if (expr->Has_const_operand () && expr->Get_kid () != operand) {
    return_value.Set_int_const ();
    return_value.Set_int_const_value (expr->Get_const_value ());
    return_value.Set_mtype (expr->Get_mtype ());
    return;
  }
    
  if (expr->Is_expr_value (operand)) {
    const SUMMARY_VALUE& value = ipa_value[expr->Get_node_index (operand)];
    Evaluate_value (value, return_value);
  } else if (expr->Is_expr_expr (operand)) {
    SUMMARY_EXPR *sub_expr = ipa_expr + expr->Get_node_index (operand);
    Evaluate_expr (sub_expr, return_value);
  } else if (expr->Is_expr_phi (operand)) {
    SUMMARY_PHI *phi = ipa_phi + expr->Get_node_index (operand);
    Evaluate_phi (phi, return_value);
  } else if (expr->Is_expr_chi (operand)) {
    SUMMARY_CHI *chi = ipa_chi + expr->Get_node_index (operand);
    Evaluate_chi (chi, return_value);
  } else {
    return_value.Set_not_const ();
    return;
  }

  Simplify_value (return_value);

  return;
} // Get_expr_operand


static void
Evaluate_expr (const SUMMARY_EXPR *expr, SUMMARY_VALUE &return_value)
{
    SUMMARY_VALUE first_operand, second_operand;
    WN *kid0, *kid1, *result;
    OPCODE op;

    EVAL_HASH::const_iterator idx = eval_hash->find (expr);
    if (idx != eval_hash->end ()) {
	return_value = (*eval_value)[(*idx).second];
	return;
    }
    
    return_value.Set_mtype (expr->Get_mtype ());

    if (expr->Is_expr_unknown ()) {	// not a constant
	return_value.Set_not_const ();
	(*eval_hash)[expr] = 0;
	return;
    }

    first_operand.Init ();
    Get_expr_operand (expr, 0, first_operand);

    if (first_operand.Is_not_const ()) {
	return_value.Set_not_const ();
	(*eval_hash)[expr] = 0;
	return;
    } else if (!first_operand.Is_int_const ()) {
	/* ignore floating point constant for now */
	return_value.Set_not_const ();
	(*eval_hash)[expr] = 0;
	return;
    }

    if (OPCODE_nkids (expr->Get_opcode ()) == 2) {
	second_operand.Init ();
	Get_expr_operand (expr, 1, second_operand);
	
	if (second_operand.Is_not_const ()) {
	    return_value.Set_not_const ();
	    (*eval_hash)[expr] = 0;
	    return;
	} else if (!second_operand.Is_int_const ()) {
	    /* ignore floating point constant for now */
	    return_value.Set_not_const ();
	    (*eval_hash)[expr] = 0;
	    return;
	}
    }

    op = OPCODE_make_op (OPR_INTCONST, first_operand.Get_mtype (), MTYPE_V);
    kid0 = WN_CreateIntconst (op, first_operand.Get_int_const_value ());

    if (OPCODE_nkids (expr->Get_opcode ()) == 2) {
	op = OPCODE_make_op (OPR_INTCONST, second_operand.Get_mtype (),
			     MTYPE_V);
	kid1 = WN_CreateIntconst (op, second_operand.Get_int_const_value ());

	result = WN_SimplifyExp2 (expr->Get_opcode (), kid0, kid1);

    } else if (OPCODE_nkids (expr->Get_opcode ()) == 1) {
	if (OPCODE_operator (expr->Get_opcode ()) == OPR_CVTL)
	    result = WN_SimplifyCvtl (expr->Get_opcode (),
				      expr->Get_const_value (), kid0);
	else
	    result = WN_SimplifyExp1 (expr->Get_opcode (), kid0);
    }
    
    if (result != NULL &&
	WN_operator(result) == OPR_INTCONST) {
	return_value.Set_int_const ();
	return_value.Set_int_const_value (WN_const_val (result));
    } else
	return_value.Set_not_const ();

    (*eval_hash)[expr] = eval_value->Insert (return_value);

} // Evaluate_expr


// ---------------------------------------------------------------
// This is currently used only for elements of common blocks.
// When they are passed in arguments, they show up as VALUE_GLOBAL
// ---------------------------------------------------------------
static BOOL
Evaluate_common_const (const SUMMARY_SYMBOL* sym, 
                       const SUMMARY_VALUE& value,
                       SUMMARY_VALUE& return_value)
{
  if (current_gannot && sym && sym->Is_common()) {
    const ST& st = St_Table[sym->St_idx()];
    const GLOBAL_VALUE* gval = 
      current_gannot->Find(GLOBAL_ANNOT::Index(ST_base_idx(st)), 
                           ST_ofst(st), 
                           (UINT32) TY_size(ST_type(st)));
    if (gval) {
      SUMMARY_VALUE* val = gval->Value();
      if (val) {
        Evaluate_value(*val, return_value);
        if (value.Is_addr_of ()) {
          return_value.Set_is_addr_of ();
          return_value.Set_target_mtype (value.Target_mtype ());
	  return_value.Set_mtype (value.Get_mtype ());
        }
        return TRUE;
      }
    }
  }
  return FALSE;
}


#ifdef KEY
// Return TRUE if symbol idx is "private" (in OpenMP sense) in MP-region
// region.
static BOOL
Var_is_private (ST_IDX idx, WN * region)
{
  Is_True (region && (WN_region_kind (region) & REGION_KIND_MP),
           ("Var_is_private: Valid MP region expected"));

  Is_True (idx, ("Var_is_private: Invalid symbol idx"));

  WN * pragmas = WN_first (WN_region_pragmas (region));

  while (pragmas)
  {
    ST_IDX sym = WN_st_idx (pragmas);
    if (sym == idx)
    {
      switch (WN_pragma (pragmas))
      {
	case WN_PRAGMA_LOCAL:
	case WN_PRAGMA_FIRSTPRIVATE:
	case WN_PRAGMA_LASTLOCAL:
	  return TRUE;

	default:
	  break;
      }
    }
    
    pragmas = WN_next (pragmas);
  }

  return FALSE;
}

// If we are processing the actual parameters from a callsite, mp_region
// is any MP region around the callsite.
static void
Evaluate_value (const SUMMARY_VALUE& value, SUMMARY_VALUE& return_value,
                WN * mp_region)
#else
static void
Evaluate_value (const SUMMARY_VALUE& value, SUMMARY_VALUE &return_value)
#endif // KEY
{
    EVAL_HASH::const_iterator idx = eval_hash->find (&value);

    if (idx != eval_hash->end ()) {
	return_value = (*eval_value)[(*idx).second];
	return;
    }

    return_value = value;

    if (value.Is_int_const ())		// known constant
	return;
    else if (value.Is_const_st ()) {	// a tcon
	ST_IDX st_idx = return_value.Get_const_st_idx ();
	if (st_idx == 0)
	    return;
	const ST& st = St_Table[st_idx];
	Is_True (ST_sym_class (st) == CLASS_CONST,
		 ("Inconsistent summary info: symbol should be CLASS_CONST"));
	return_value.Set_const_st_idx (st_idx, ST_tcon (st));
	return_value.Set_merged_const_st_idx();
	Simplify_value (return_value);
	return;
    } else if (value.Is_formal ()) {
	if (formal_value == 0) {
	    return_value.Set_not_const ();
	    return;			// no value propagated
	}
	
	const SUMMARY_FORMAL& sum_formal =
	    ipa_formal[value.Get_formal_index ()];

	if (value.Is_addr_of () && !sum_formal.Is_ref_parm ()) {
	    // don't handle pointer yet
	    return_value.Set_not_const ();
	    return;
	}

	INT idx = sum_formal.Get_position ();
	const SUMMARY_VALUE& formal = (*formal_value)[idx];

	return_value = formal;

	if (sum_formal.Is_ref_parm ()) {
          if (!value.Is_addr_of ()) {
            // normal reference parameter
            if (return_value.Is_addr_of () &&
                (!return_value.Is_global() ||
                 ST_is_const_var(ST_ptr(return_value.Get_global_st_idx())))) {
              // lowering of reference parameter from LDA x to x.
              return_value.Clear_is_addr_of ();
              return_value.Set_mtype (return_value.Target_mtype ());
              Simplify_value (return_value);
            } else {
              return_value.Set_not_const ();
              return;
            }
          }
	} else {
	    // not a FORMAL_REF
	    if (value.Is_addr_of ()) {
		// passing address of the formal, which is a stack address
		return_value.Set_not_const ();
		return;
	    }

	    if (return_value.Is_addr_of ()) {
		// actual is an "LDA x" and the formal is a value parameter.
		// We don't know if "x" has been changed, so we pass down
		// the address of x (which is still a constant) but make
		// sure that we don't assume value of x is constant, unless 
		// it is known to be a global constant.
		if (!return_value.Is_convertible_to_global ()) {
		    switch (return_value.Get_const_type ()) {
		    case VALUE_INT_CONST:
		    case VALUE_CONST:
			return_value.Set_symbol ();
		    }
		}
	    }
	}
	    
	if (return_value.Is_addr_of ()) {
	    if (return_value.Get_mtype () != Pointer_type)
		return_value.Set_not_const ();
	    return;
	}

	switch (return_value.Get_const_type ()) {
	case VALUE_NOT_CONST:
	    return_value.Set_not_const ();
	    break;

	case VALUE_INT_CONST:
	    if (MTYPE_is_integral (return_value.Get_mtype ()) &&
		MTYPE_is_integral (value.Get_mtype ()))
		// keep the mtype of the value
		return_value.Set_mtype (value.Get_mtype ());
	    else
		return_value.Set_not_const ();
	    break;

	case VALUE_CONST:
	case VALUE_GLOBAL:
	    if (MTYPE_type_class (return_value.Get_mtype ()) !=
		MTYPE_type_class (value.Get_mtype ()))
		return_value.Set_not_const ();
	    break;

	default:
	    return_value.Set_not_const ();
	    break;
	}
    } else if (value.Is_symbol ()) {
	return_value.Set_not_const ();
	return;				// don't handle local var. yet.
    } else if (value.Is_global ()) {
	INT idx = value.Get_global_index ();
	ST_IDX st_idx;
	const SUMMARY_SYMBOL* sym = NULL;
	if (idx != -1) {
	    sym = ipa_symbol + idx;
	    st_idx = sym->St_idx ();
	} else
	    st_idx = value.Get_global_st_idx ();
	return_value.Set_global_st_idx (st_idx);
	return_value.Set_global_index (-1);

#ifdef KEY
        if (mp_region && Var_is_private (st_idx, mp_region)) {
	  return_value.Set_not_const ();
	  return;
	}
#endif // KEY
        if (!ST_is_const_var (St_Table[st_idx])) {
          if (Evaluate_common_const(sym, value, return_value)) {
            return;
          }
          if (!value.Is_addr_of()) {
            return_value.Set_not_const ();
            (*eval_hash)[&value] = 0;
            return;
          }
        }
        Simplify_value (return_value);

	if (return_value.Is_global () &&
	    return_value.Is_addr_of () &&
	    ! return_value.Is_convertible_to_global ()) {

	    // this is the address of a stack variable that has value equal
	    // to that of the specified global variable, and that global
	    // variable cannot be simplified to a literal constant
	    // in this case, we cannot treat it as constant.
	    // Note:  We could have changed the transformation phase to
	    // recognize this case and generate an assigment statement at
	    // the beginning of the callee.  But this is probably worse
	    // because we will be loading from the global variable again
	    // instead of using the value in the register passed down by
	    // the caller.  See pv 658444
	    return_value.Set_not_const ();
            (*eval_hash)[&value] = 0;
            return;
	}

    } else if (value.Is_expr ()) {
	SUMMARY_EXPR *expr = ipa_expr + value.Get_expr_index ();
	Evaluate_expr (expr, return_value);
    } else if (value.Is_phi ()) {
	SUMMARY_PHI *phi = ipa_phi + value.Get_phi_index ();
	Evaluate_phi (phi, return_value);
    } else if (value.Is_chi ()) {
	SUMMARY_CHI *chi = ipa_chi + value.Get_chi_index ();
	Evaluate_chi (chi, return_value);
    }

    if (value.Is_addr_of ()) {
	return_value.Set_is_addr_of ();
	return_value.Set_target_mtype (value.Target_mtype ());
	return_value.Set_mtype (value.Get_mtype ());
    }

    (*eval_hash)[&value] = eval_value->Insert (return_value);
} // Evaluate_value



// Check if a PU is dead as a result of all calls to it are deleted.  If
// so, delete all calls originated from this PU.
static BOOL
PU_is_dead (IPA_NODE *node, BOOL *updated)
{
    IPA_EDGE *e;
    IPA_PRED_ITER pred_iter (node);

    if (node->Is_Externally_Callable () || 
        node->Summary_Proc()->Is_alt_entry () ||
	node->Summary_Proc()->Has_alt_entry ())
	return FALSE;

    for (pred_iter.First (); !pred_iter.Is_Empty (); pred_iter.Next ()) {
	e = pred_iter.Current_Edge ();
	if (e && e->Cprop_Annot() != (void *) -1)
	    return FALSE;
    }

    // now, we know all calls to me have been deleted

    IPA_SUCC_ITER succ_iter (node);

    for (succ_iter.First (); !succ_iter.Is_Empty (); succ_iter.Next ()) {
	e = succ_iter.Current_Edge ();
	if (e == NULL)
	    continue;
	if (e->Cprop_Annot () != (void *) -1) {
	    e->Set_Cprop_Annot ((VALUE_DYN_ARRAY*) -1);
	    *updated = TRUE;
	}
    }

    if (node->Cprop_Annot () != (void *) -1) {
	if (node->Cprop_Annot () != NULL)
	    CXX_DELETE (node->Cprop_Annot (), Malloc_Mem_Pool);
	node->Set_Cprop_Annot ((VALUE_DYN_ARRAY*) -1);
	*updated = TRUE;
    }

    return TRUE;

} // PU_is_dead

// Delete the statements on the branch that is never taken
static void
Delete_stmts (SUMMARY_CONTROL_DEPENDENCE *cd, BOOL branch, BOOL *call_deleted)
{
    SUMMARY_STMT *stmts;
    INT count;

    if (branch) {
	stmts = ipa_stmt + cd->Get_true_stmt_index ();
	count = cd->Get_true_count ();
    } else {
	stmts = ipa_stmt + cd->Get_false_stmt_index ();
	count = cd->Get_false_count ();
    }

    for (INT i = 0; i < count; i++) {
	INT idx;
	if (stmts[i].Is_cond ()) {
	    idx = stmts[i].Get_cond_index () - ipa_proc->Get_ctrl_dep_index ();
	    cd_status[idx] = CD_DEAD;
	} else if (stmts[i].Is_call ()) {
	    idx = stmts[i].Get_call_index () - ipa_proc->Get_callsite_index ();
	    call_deleted[idx] = TRUE;
	}
    }
} // Delete_stmts


// evaluate all conditional branches and see if any dead code can be found
static void
Process_cond_branches (BOOL *call_deleted)
{
    cd_status = (CONDITION_STATE *)
	MEM_POOL_Alloc (&Ipa_cprop_pool, sizeof(CONDITION_STATE) *
			ipa_proc->Get_ctrl_dep_count ());

    bzero (cd_status, sizeof(CONDITION_STATE) * ipa_proc->Get_ctrl_dep_count ());

    for (INT i = 0; i < ipa_proc->Get_ctrl_dep_count (); i++) {

	SUMMARY_CONTROL_DEPENDENCE *cd;

	cd = ipa_ctrl_dep + ipa_proc->Get_ctrl_dep_index () + i;

	if (cd_status[i] == CD_DEAD) {
	    Delete_stmts (cd, TRUE, call_deleted); 
	    Delete_stmts (cd, FALSE, call_deleted); 
	    continue;
	}

	if (cd->Is_do_loop () || cd->Is_entry ())
	    continue;			// ignore do loops and entry point

	if (cd->Get_expr_index () == -1)
	    continue;			// too complicated expression

	SUMMARY_EXPR *expr = ipa_expr + cd->Get_expr_index ();
	SUMMARY_VALUE value;
	value.Init ();

	Evaluate_expr (expr, value);

	if (!value.Is_int_const ())
	    continue;

	cd_status[i] = value.Get_int_const_value () ? CD_TRUE : CD_FALSE;

	Delete_stmts (cd, !value.Get_int_const_value (), call_deleted);  

    }
    
} // Process_cond_branches

#ifdef KEY
#include "ipo_parent.h"
// Find any MP region enclosing callsite 'e' in caller 'c'. If found,
// set corresponding field in 'e'.
static void
Get_enclosing_mp_region (IPA_NODE * c, IPA_EDGE * e)
{
  PU caller = c->Get_PU ();

  if (!PU_has_mp (caller)) return;

  // Get context of caller
  IPA_NODE_CONTEXT context (c);

  // Set callsite information in call-graph edges
  IPA_Call_Graph->Map_Callsites (c);

  WN * call_wn = e->Whirl_Node ();
  Is_True (call_wn, ("Get_enclosing_mp_region: NULL callsite in IPA_EDGE"));

  WN * parent = WN_Get_Parent (call_wn, Parent_Map, Current_Map_Tab);

  for (; parent; parent = WN_Get_Parent (parent, Parent_Map, Current_Map_Tab))
  {
    // Nested parallelism not supported
    if (WN_operator (parent) == OPR_REGION && 
        (WN_region_kind (parent) & REGION_KIND_MP))
      {
        e->Set_MP_Whirl_Node (parent);
        break;
      }
  }
}
#endif // KEY

// KEY: The following function has been extracted out of 
// Intra_PU_Formal_Cprop.
// Called by:
//    Intra_PU_Formal_Cprop
//    Evaluate_actuals
// Determine if it is OK to propagate down the formal parameters
// of this node. Return NULL if not OK.
static VALUE_DYN_ARRAY *
Get_cprop_annot (IPA_NODE *node)
{
    // if this procedure is cloned, we cannot propagate the value of the
    // formal parameter down.  But we can still delete dead code if the
    // condition expressions involve only global variables that are found
    // to be constant.
    if ((node->Is_Externally_Callable() && IPA_Enable_Cloning) ||
        node->Summary_Proc()->Is_alt_entry () || 
        node->Summary_Proc()->Has_alt_entry ()) {
      return NULL;
    }

#ifdef KEY
    {
        // Also if there is a recursive in-edge, we cannot propagate the
        // formal parameter value down. Because the const-ness of the
        // parameters may not have been determined yet. So, don't consider
        // the formal parameter values in flow analysis below.
        IPA_PRED_ITER edge_iter (node);
        for (edge_iter.First (); !edge_iter.Is_Empty(); edge_iter.Next()) {
            IPA_EDGE *e = edge_iter.Current_Edge();
            if (e && e->Is_Recursive()) {
                return NULL;
            }
        }
    }
#endif

    return node->Cprop_Annot();
}

// evaluate all actual parameters of the given callsite
static void
Evaluate_actuals (IPA_NODE *caller, IPA_NODE *callee, IPA_EDGE *edge)
{
    INT count = edge->Summary_Callsite()->Get_param_count ();
    VALUE_DYN_ARRAY* annot = edge->Cprop_Annot ();
    INT i;
    INT actual_idx = edge->Summary_Callsite()->Get_actual_index ();

    if (count > callee->Num_Formals())
	count = callee->Num_Formals();
    
    if (annot == NULL) {
	annot = CXX_NEW (VALUE_DYN_ARRAY (MEM_local_nz_pool_ptr),
                         MEM_local_nz_pool_ptr);
	annot->Force_Alloc_array (count);
	annot->Setidx (count - 1);
	for (i = 0; i < count; i++)
	    (*annot)[i].Init ();
	edge->Set_Cprop_Annot (annot);
    }

    const PU& caller_pu = caller->Get_PU ();
    const PU& callee_pu = callee->Get_PU ();

    BOOL check_for_readonly_ref =
	(IPA_Enable_Readonly_Ref && IPA_Enable_Simple_Alias &&
	 IPA_Enable_Addressing && !PU_f77_lang (caller_pu) &&
	 !PU_f90_lang (caller_pu));
				   
    const IPAA_NODE_INFO *callee_info = check_for_readonly_ref ?
	callee->Mod_Ref_Info () : NULL;
	
    const IPAA_NODE_INFO *caller_info = caller->Mod_Ref_Info ();
	
    BOOL Need_to_verify_duplicated = FALSE;

    for (i = 0; i < count; i++, actual_idx++) {

        const SUMMARY_VALUE* actual;
      
	INT value_idx = ipa_actual[actual_idx].Get_value_index ();
	if (value_idx == -1) {

          INT symbol_idx = ipa_actual[actual_idx].Get_symbol_index();
          if (symbol_idx != -1 && ipa_symbol[symbol_idx].Is_formal()) {
            INT position = 
              ipa_formal[ipa_symbol[symbol_idx].Get_findex()].Get_position();
#ifdef KEY
            // See if the formal belongs to the caller or to a parent PU
            INT func_st_idx = ipa_symbol[symbol_idx].Get_st_idx_func();
            if (func_st_idx != ST_st_idx (caller->Func_ST())) {

              // formal does not belong to caller, find its PU
              NODE_INDEX node_id =
                  AUX_PU_node (Aux_Pu_Table[ST_pu (St_Table[func_st_idx])]);
              IPA_NODE * parent = IPA_Call_Graph->Graph()->Node_User (node_id);

              Is_True (PU_ftn_lang (parent->Get_PU()),
                       ("Nested PUs supported only in Fortran"));

              VALUE_DYN_ARRAY * parents_formal_value = Get_cprop_annot (parent);
              if (parents_formal_value &&
                  !parent->Mod_Ref_Info()->Is_formal_dmod_elmt(position) &&
                  !parent->Mod_Ref_Info()->Is_formal_imod_elmt(position)) {
                actual = &(*parents_formal_value)[position];
              }
              else {
                (*annot)[i].Set_not_const ();
                continue;
              }
            } else
#endif
            if (formal_value &&
                !caller_info->Is_formal_dmod_elmt(position) &&
                !caller_info->Is_formal_imod_elmt(position)) {
              actual = &(*formal_value)[position];
            }
            else {
              (*annot)[i].Set_not_const ();
              continue;
            }
          }
          else {
            (*annot)[i].Set_not_const ();
            continue;
          }
	}
        else {
          actual = &ipa_value[value_idx];
        }
        
#ifdef KEY
	// If the callsite is inside an MP region, and this parameter
	// is a global variable marked private, then it cannot be
	// const-propagated.
	if (!edge->MP_Whirl_Node ())
	  Get_enclosing_mp_region (caller, edge);
	WN * mp_region = edge->MP_Whirl_Node ();

	Evaluate_value (*actual, (*annot)[i], mp_region);
#else

	Evaluate_value (*actual, (*annot)[i]);
#endif // KEY

	if (check_for_readonly_ref &&
	    ipa_actual[actual_idx].Get_pass_type() == PASS_LDID) {

	    Need_to_verify_duplicated = TRUE;
	    
	    if ((*annot)[i].Is_global () && (*annot)[i].Is_addr_of ()) {

		ST_IDX st_idx = (*annot)[i].Get_global_st_idx ();
		
		if (ST_addr_saved (St_Table[st_idx])) {
		    edge->Clear_Param_Readonly(i);
		    edge->Clear_Param_Pass_Not_Saved(i);
		    continue;
		}

		if (callee_info->Is_def_elmt (st_idx) ||
		    callee_info->Is_formal_imod_elmt (i))
		    edge->Clear_Param_Readonly(i);
		else
		    edge->Set_Param_Readonly(i);

		if (callee_info->Is_formal_dref_elmt (i))
		    edge->Clear_Param_Pass_Not_Saved(i);
		else
		    edge->Set_Param_Pass_Not_Saved (i);
		
	    } else {
		edge->Clear_Param_Readonly(i);
		edge->Clear_Param_Pass_Not_Saved(i);
	    }
	}
    }

    if (!Need_to_verify_duplicated)
	return;
    
    for (i = 0; i < count; ++i) {
	if (! ((*annot)[i].Is_global () && (*annot)[i].Is_addr_of ()))
	    continue;
	for (INT j = i + 1; j < count; ++j) {
	    const SUMMARY_VALUE& value = (*annot)[j];
	    if (! (value.Is_global () && value.Is_addr_of ()))
		continue;
	    if ((*annot)[i].Get_global_st_idx () != value.Get_global_st_idx ())
		continue;
	    edge->Clear_Param_Readonly(i);
	    edge->Clear_Param_Readonly(j);
	    edge->Clear_Param_Pass_Not_Saved(i);
	    edge->Clear_Param_Pass_Not_Saved(j);
	    Set_ST_addr_saved (St_Table[value.Get_global_st_idx ()]);
	}
    }
} // Evaluate_actuals


extern BOOL
Intra_PU_Formal_Cprop (IPA_NODE *node)
{
    BOOL updated = FALSE;

    if (IPA_Enable_DFE && PU_is_dead (node, &updated))
	return updated;

    current_gannot = node->Global_Annot ();
    formal_value = Get_cprop_annot (node);

    // if this procedure is cloned, we cannot propagate the value of the
    // formal parameter down.  But we can still delete dead code if the
    // condition expressions involve only global variables that are found
    // to be constant.
    if ((node->Is_Externally_Callable() && IPA_Enable_Cloning) ||
        node->Summary_Proc()->Is_alt_entry () || 
        node->Summary_Proc()->Has_alt_entry ()) {
      formal_value = 0;
    }

#ifdef KEY
    {
        // Also if there is a recursive in-edge, we cannot propagate the
        // formal parameter value down. Because the const-ness of the
        // parameters may not have been determined yet. So, don't consider
        // the formal parameter values in flow analysis below
        IPA_PRED_ITER edge_iter (node);
        for (edge_iter.First (); !edge_iter.Is_Empty(); edge_iter.Next()) {
            IPA_EDGE *e = edge_iter.Current_Edge();
            if (e && e->Is_Recursive()) {
                formal_value = 0;
                break;
            }
        }
    }
#endif
    // set up summary info arrays
    Set_summary_info (node);

    WN_mem_pool_ptr = &Ipa_cprop_pool;

    eval_value = CXX_NEW (VALUE_VECTOR (&Ipa_cprop_pool), &Ipa_cprop_pool);
    eval_hash = CXX_NEW (EVAL_HASH (100, eval_hasher(),
				    std::equal_to<const void *> (),
				    &Ipa_cprop_pool),
			 &Ipa_cprop_pool);

    SUMMARY_VALUE value;
    value.Init ();
    value.Set_not_const ();
    // first node always a non-constant
    eval_value->Insert (value);
    Is_True ((*eval_value)[0].Is_not_const (),
	     ("incorrectly initialized eval_value array"));
    

    INT call_count = ipa_proc->Get_callsite_count ();
    BOOL* call_deleted = (BOOL*) alloca (sizeof(BOOL) * call_count);
    bzero (call_deleted, sizeof(BOOL) * call_count);

    Current_caller = node;
    callsite_map = 0;

    if (IPA_Enable_Flow_Analysis)
	Process_cond_branches (call_deleted);

    SUMMARY_CALLSITE* callsite = &ipa_callsite[ipa_proc->Get_callsite_index()];
    IPA_SUCC_ITER edge_iter (node);

    for (edge_iter.First (); !edge_iter.Is_Empty(); edge_iter.Next()) {
	IPA_EDGE *e = edge_iter.Current_Edge();

	if (e == NULL)
	    continue;

	INT idx = e->Summary_Callsite() - callsite;
	
	if (call_deleted[idx] && e->Cprop_Annot() != (void *) -1) {
	    e->Set_Cprop_Annot ((VALUE_DYN_ARRAY*) -1);
	    updated = TRUE;
	} else if (!call_deleted[idx]) {
	    IPA_NODE *callee = IPA_Call_Graph->Callee (e);
	    if (e->Cprop_Annot() == (void *) -1) {
		e->Set_Cprop_Annot (NULL);
		updated = TRUE;
	    
		if (callee->Cprop_Annot () == ((void *) -1)) {
		    /* this node previously marked deleted, but now we found
		       that it is not dead */ 
		    Init_Cprop_Annotations (callee);
		}
	    } else {
		Evaluate_actuals (node, callee, e);
	    }
	} 
    }

    if (! node->Icall_List().empty ()) {
        const IPA_ICALL_LIST& icall_list = node->Icall_List ();

        for (IPA_ICALL_LIST::const_iterator icall_iter = icall_list.begin ();
             icall_iter != icall_list.end (); ++icall_iter) {

            SUMMARY_CALLSITE *call = (*icall_iter)->Callsite();
	    if (call == NULL)
		continue;

	    if (call_deleted[call - callsite])
		continue;

            if ((*icall_iter)->Value().Is_not_const ())
		continue;

	    SUMMARY_VALUE return_value;
	    return_value.Init ();
	    Evaluate_value(ipa_value[call->Get_value_index()], return_value);
            
            if (Update_node ((*icall_iter)->Value (),
                             return_value,
                             ipa_value[call->Get_value_index()].Get_mtype(),
			     FALSE /* is_ref_param */))
                updated = TRUE;
	}
    }

    return updated;
} // Intra_PU_Formal_Cprop


//==========================================================================
//==========================================================================
//==========================================================================
//============== GLOBAL INTERPROCEDURAL CONSTANT PROPAGATION ===============
//==========================================================================
//==========================================================================
//==========================================================================

// ========================================================================
// Implementation of member functions for classes used in global IPA cprop:
//   GLOBAL_VALUE
//   GLOBAL_ANNOT
// Full interface is defined in ipa_cprop.h
// ========================================================================

// --------------------------------------------------------------------
// Merge the incoming value with the one in the global value annotation
// --------------------------------------------------------------------
BOOL
GLOBAL_VALUE::Union(SUMMARY_VALUE* value, BOOL overwrite)
{
  if (overwrite) {
    if (_value != value) {
      _value = value;
      return TRUE;
    }
    return FALSE;
  }

  // value == 0 means BOTTOM
  if (_value == 0) { 
    return FALSE;
  }
  
  if (value == 0 || !Values_are_equal(_value, value)) {
    _value = 0;
    return TRUE;
  }
    
  return FALSE;
}

// --------------------------------------------------------------------
// Given ST_IDX of a common block, find its index in GLOBAL_ANNOT array
// --------------------------------------------------------------------
INT32
GLOBAL_ANNOT::Index (ST_IDX st_idx) 
{
  for (UINT32 i = 0; i < Size; ++i) {
    if (Common_ST[i] == st_idx) {
      return i;
    }
  }
  return -1;
}

// -------------------------------------------------------
// Find GLOBAL_VALUE, given common index, offset, and size
// size of 0 is used a wildcard (it will match any size)
// -------------------------------------------------------
const GLOBAL_VALUE*
GLOBAL_ANNOT::Find (INT32 idx, UINT64 offset, UINT32 size) const
{
  if (idx >= 0 && idx < Size && !Top(idx) && !Bottom(idx)) {
    const GLOBAL_DYN_ARRAY& gvals = Global_Value_Array(idx);
    for (UINT32 i = 0; i < gvals.Elements(); ++i) {
      if (gvals[i].Offset() == offset && 
          (size == 0 || gvals[i].Size() == size)) {
        return &(gvals[i]);
      }
    }
  }
  return 0;
}

// --------------------------------------------------
// check whether two segments overlap
// the assumption is that they do NOT match perfectly
// --------------------------------------------------
static inline BOOL
Overlap (UINT64 start1, UINT32 size1, UINT64 start2, UINT32 size2)
{
  UINT64 end1 = start1 + size1;
  UINT64 end2 = start2 + size2;
  
  return ((start2 < end1 && end1 <= end2) || (start1 < end2 && end2 <= end1));
}

// ---------------------------------------------------------------
// Merge incoming GLOBAL_VALUE with existing ones for given common 
// ---------------------------------------------------------------
BOOL
GLOBAL_ANNOT::Union (UINT32 common_idx, 
                     const GLOBAL_VALUE& gval,
                     BOOL overwrite_when_found,
                     BOOL add_when_not_found)
{
  if (Top(common_idx) || Bottom(common_idx)) {
    if (add_when_not_found) {
      _gvals_array[common_idx] = CXX_NEW(GLOBAL_DYN_ARRAY(_pool), _pool);
      _gvals_array[common_idx]->Force_Alloc_array(8);
      _gvals_array[common_idx]->AddElement(gval);
      return TRUE;
    }
    else {
      return FALSE;
    }
  }
  
  UINT64 offset = gval.Offset();
  UINT32 size = gval.Size();
  GLOBAL_DYN_ARRAY& gvals = *(_gvals_array[common_idx]);

  for (UINT32 i = 0; i < gvals.Elements(); ++i) {
    // check for the exact match
    if (gvals[i].Offset() == offset && gvals[i].Size() == size) {
      return gvals[i].Union(gval.Value(), overwrite_when_found);
    }
    // check for the overlap, and be very conservative for now
    else if (Overlap(gvals[i].Offset(), gvals[i].Size(), offset, size)) {
      Set_Bottom(common_idx);
      return TRUE;
    }
  }

  // neither exact match nor overlap found
  if (add_when_not_found) {
    gvals.AddElement(gval);
    return TRUE;
  }
  
  return FALSE;
}

// -------------------------------------------------------------------
// Union global values from the incoming annotation with existing ones
// -------------------------------------------------------------------
BOOL
GLOBAL_ANNOT::Union (const GLOBAL_ANNOT* gannot)
{
  BOOL change = FALSE;
  
  for (UINT32 i = 0; i < Size; ++i) {
    if (Bottom(i) || gannot->Top(i)) {
      continue;
    }
    if (gannot->Bottom(i)) {
      Set_Bottom(i);
      change = TRUE;
    }
    else if (Top(i)) {
      _gvals_array[i] = CXX_NEW(GLOBAL_DYN_ARRAY(_pool), _pool);
      *(_gvals_array[i]) = gannot->Global_Value_Array(i);
      change = TRUE;
    }
    else {
      UINT32 j;
      // perform meet operation: LHS = LHS /\ RHS
      const GLOBAL_DYN_ARRAY& lhs = Global_Value_Array(i);
      const GLOBAL_DYN_ARRAY& rhs = gannot->Global_Value_Array(i);

      // first, set to not constant (SUMMARY_VALUE* == 0) all
      // GLOBAL_VALUEs that are only in LHS and not in RHS
      for (j = 0; j < lhs.Elements(); ++j) {
        GLOBAL_VALUE& lhs_gval = lhs[j];
        if (!gannot->Find(i, lhs_gval.Offset(), lhs_gval.Size())) {
          new (&lhs_gval) GLOBAL_VALUE(lhs_gval.Offset(), lhs_gval.Size(), 0);
        }
      }
      
      // second, merge GLOBAL_VALUEs that are in both LHS and RHS,
      // and add to LHS those that are only in RHS 
      BOOL overwrite_if_found = FALSE;
      BOOL add_if_not_found = TRUE;
      for (j = 0; j < rhs.Elements(); ++j) {
        change |= Union(i, rhs[j], overwrite_if_found, add_if_not_found);
      }
    }
  }
  
  return change;
}

//------------------------------------------------
// print the global constant values per procedure
//------------------------------------------------
void 
GLOBAL_ANNOT::Print(FILE *fp)
{
  for (UINT32 i = 0; i < Size; ++i) {
    if (!Top(i) && !Bottom(i)) {
      fprintf(fp, "  common block %s:\n", ST_name(Common_ST[i]));
      const GLOBAL_DYN_ARRAY& gvals = Global_Value_Array(i);
      for (UINT32 j = 0; j < gvals.Elements(); ++j) {
        if (gvals[j].Value()) {
          fprintf(fp, "    offset = %lld, element_size = %d, value = ",
                  gvals[j].Offset(), gvals[j].Size());
          gvals[j].Value()->Print_const_value(fp);
          fprintf(fp, "\n");
        }
      }
    }
  }
}

// ====================================================================
// ====================================================================
// Static variables and helper functions used in global cprop algorithm
// ====================================================================
// ====================================================================

static INT indent = 0;

// ----------------------------------------------
// Propagate effects of an STID into GLOBAL_ANNOT
// ----------------------------------------------
static void 
Process_stid (const IPA_NODE* node, 
              const SUMMARY_STID* stid, 
              BOOL always_executed,
              GLOBAL_ANNOT* gannot)
{
  const ST& st = 
    St_Table[IPA_get_symbol_array(node)[stid->Get_symbol_index()].St_idx()];

  INT32 common_idx = GLOBAL_ANNOT::Index(ST_base_idx(st));

  // if common is not found, don't do anything
  if (common_idx != -1) {

    UINT64 offset;
    UINT32 size;

    if (stid->Is_array_assignment()) {
      // assume that non-constant array subscript can modify enitre common
      if (!stid->Has_constant_subscript()) {
        if (!gannot->Bottom(common_idx)) {
          gannot->Set_Bottom(common_idx);
        }
        return;
      }
      size = TY_size(TY_AR_etype(ST_type(st)));
      offset = ST_ofst(st) + stid->Get_array_subscript() * size;
    }
    else {
      size = TY_size(ST_type(st));
      offset = ST_ofst(st);
    }

    SUMMARY_VALUE* value = 0;
    INT32 value_index = stid->Get_value_index();
    if (value_index != -1) {
      value = IPA_get_value_array(node) + value_index;
      // if value is not already constant, try to simplify it
      if (!value->Is_int_const() && !value->Is_const_st()) {
        SUMMARY_VALUE tmp_value;
        tmp_value.Init();
        Evaluate_value(*value, tmp_value);
        if (tmp_value.Is_int_const() || tmp_value.Is_const_st()) {
          // if it does, copy it to the heap because it may be used in IPO
          value = CXX_NEW(SUMMARY_VALUE(), Malloc_Mem_Pool);
          *value = tmp_value;
        }
        else {
          value = 0;
        }
      }
    }

    BOOL overwrite_if_found = always_executed;
    BOOL add_if_not_found = always_executed;
    gannot->Union (common_idx, 
                   GLOBAL_VALUE(offset, size, value),
                   overwrite_if_found,
                   add_if_not_found);

    // NOTE: Make sure that it is safe not to add NOT_CONSTANT value
    // when GLOBAL_VALUE is not found and STID is NOT always executed!
  }
}

//------------------------------------------------------
// propagate the global annotation to all the callsites
//------------------------------------------------------
static BOOL
Propagate_vals_in_unstructured_cflow (IPA_NODE* node, GLOBAL_ANNOT* gannot)
                                     
{
  if (Get_Trace(TP_IPA, IPA_TRACE_COMMON_CONST)) {
    for (INT i = 0; i < indent; ++i) putc(' ', TFile);
    fprintf(TFile, "Propagate_vals_in_unstructured_cflow\n");
  }
  indent += 2;

  BOOL change = FALSE;
  
  SUMMARY_PROCEDURE* proc = node->Summary_Proc();
  INT num_calls = proc->Get_callsite_count();
  SUMMARY_CALLSITE* callsite = &ipa_callsite[proc->Get_callsite_index()];

  for (INT i = 0; i < num_calls; ++i, ++callsite) {
    if (!callsite->Is_intrinsic()) {
      IPA_NODE* callee = Get_callee(callsite, -1, FALSE);	  
      if (callee) {
        change |= callee->Global_Annot()->Union(gannot);
      }
    }
  }

  indent -= 2;
  return change;
}

//------------------------------------------------
// update the iarray based on the mod information
//------------------------------------------------
static void
Update_annot_with_callee_mod (IPA_NODE* callee, GLOBAL_ANNOT* gannot)
{
  IPAA_NODE_INFO* modref_info = callee->Mod_Ref_Info();
  for (UINT32 i = 0; i < GLOBAL_ANNOT::Size; ++i) {
    const char* is_mod = "is NOT";
    ST_IDX common_st = GLOBAL_ANNOT::Common_ST[i];
    if (modref_info->Is_def_elmt(ST_IDX_index(common_st))) {
      gannot->Set_Bottom(i);
      is_mod = "IS";
    }
    if (Get_Trace(TP_IPA, IPA_TRACE_COMMON_CONST)) {
      for (INT i = 0; i < indent; ++i) putc(' ', TFile);
      fprintf(TFile, "%-10s %s modified\n", ST_name(common_st), is_mod);
    }
  }
}

//--------------------------------------------------------------------
// check to see if any of the callsites modify the commons indirectly
//--------------------------------------------------------------------
static void 
Update_annot_with_all_callee_mods (IPA_NODE* node, GLOBAL_ANNOT* gannot)
{
  if (Get_Trace(TP_IPA, IPA_TRACE_COMMON_CONST)) {
    for (INT i = 0; i < indent; ++i) putc(' ', TFile);
    fprintf(TFile, "Update_annot_with_all_callee_mods\n");
  }
  indent += 2;

  SUMMARY_PROCEDURE* proc = node->Summary_Proc();
  INT num_calls = proc->Get_callsite_count();
  SUMMARY_CALLSITE* callsite = &ipa_callsite[proc->Get_callsite_index()];

  for (INT i = 0; i < num_calls; ++i, ++callsite) {
    if (!callsite->Is_intrinsic()) {
      IPA_NODE* callee = Get_callee(callsite, -1, FALSE);	  
      if (callee && !callee->Is_Externally_Callable()) {
        Update_annot_with_callee_mod(callee, gannot);
      }
      else {
        // assume the worst case for unknown or externally callable callees
        gannot->Set_Bottom();
      }
    }
  }

  indent -= 2;
}

//------------------------------------------------------------------------
// collect global values when the control flow for this PU is unstructured
//------------------------------------------------------------------------
static void  
Collect_stids_in_unstructured_cflow (IPA_NODE* node, 
                                     GLOBAL_ANNOT* gannot)
{
  if (Get_Trace(TP_IPA, IPA_TRACE_COMMON_CONST)) {
    for (INT i = 0; i < indent; ++i) putc(' ', TFile);
    fprintf(TFile, "Propagate_global_unstructured\n");
  }
  indent += 2;

  SUMMARY_PROCEDURE* proc = node->Summary_Proc();
  INT cd_count = proc->Get_ctrl_dep_count();
  INT cd_index = proc->Get_ctrl_dep_index();

  for (INT i = 0; i < cd_count; ++i) {

    SUMMARY_CONTROL_DEPENDENCE* cd = &ipa_ctrl_dep[cd_index+i];
    INT num_stmt_counts = (cd->Is_if_stmt()) ? 2 : 1;

    for (INT j = 0; j < num_stmt_counts; ++j) {

      INT stmt_count, stmt_index;
      if (cd->Is_if_stmt() && j == 1) {
        stmt_count = cd->Get_false_count();
        stmt_index = cd->Get_false_stmt_index();
      }
      else {
        stmt_count = cd->Get_true_count();
        stmt_index = cd->Get_true_stmt_index();
      }

      for (INT k = 0; k < stmt_count; ++k) {
        SUMMARY_STMT* cur_stmt = &ipa_stmt[stmt_index+k];
        if (cur_stmt->Is_stid()) {
          const SUMMARY_STID* stid = &ipa_stid[cur_stmt->Get_stid_index()];
          Process_stid(node, 
                       stid, 
                       cd->Is_entry() && stid->Is_always_executed(), 
                       gannot);
        }
      }
    }
  }
  
  indent -= 2;
}

//----------------------------------------------------------------------
// process a call stmt: 
// propagate common block information to the callee and update mod info
//----------------------------------------------------------------------
static BOOL
Process_global_call (SUMMARY_CALLSITE* callsite, GLOBAL_ANNOT* gannot)
{
  BOOL change = FALSE;
  IPA_NODE* callee = Get_callee(callsite, -1, FALSE);

  if (callee && !callee->Is_Externally_Callable()) {

    if (Get_Trace(TP_IPA, IPA_TRACE_COMMON_CONST)) {
      for (INT i = 0; i < indent; ++i) putc(' ', TFile);
      fprintf(TFile, "Process_global_call to %s\n", callee->Name());
    }
    indent += 2;
  
    if (callsite->Get_loopnest() > 0) {
      Update_annot_with_callee_mod(callee, gannot);
      change |= callee->Global_Annot()->Union(gannot);
    }
    else {
      change |= callee->Global_Annot()->Union(gannot);
      Update_annot_with_callee_mod(callee, gannot);
    }

    indent -= 2;
  }
  else {
    // assume the worst case for unknown or externally callable callees
    gannot->Set_Bottom();
  }

  return change;
}

//-----------------------------------------------------------------------
// propagate global values if the control flow for this PU is structured
//-----------------------------------------------------------------------
static BOOL
Propagate_global_vals (IPA_NODE* node, 
                       SUMMARY_CONTROL_DEPENDENCE* cd, 
                       GLOBAL_ANNOT* gannot)
{
  if (Get_Trace(TP_IPA, IPA_TRACE_COMMON_CONST)) {
    for (INT i = 0; i < indent; ++i) putc(' ', TFile);
    fprintf(TFile, "Propagate_global_vals\n");
  }
  indent += 2;

  BOOL change = FALSE;

  INT num_cd_counts = (cd->Is_if_stmt()) ? 2 : 1;

  for (INT i = 0; i < num_cd_counts; ++i) {

    INT stmt_count, stmt_index;

    if (cd->Is_if_stmt() && i == 1) {
      stmt_count = cd->Get_false_count();
      stmt_index = cd->Get_false_stmt_index();
    }
    else {
      stmt_count = cd->Get_true_count();
      stmt_index = cd->Get_true_stmt_index();
    }

    for (INT j = 0; j < stmt_count; ++j) {

      const SUMMARY_STMT& cur_stmt = ipa_stmt[j+stmt_index];

      if (cur_stmt.Is_stid()) {
        Process_stid(node, 
                     &ipa_stid[cur_stmt.Get_stid_index()], 
                     cd->Is_entry(),
                     gannot);
      }
      else if (cur_stmt.Is_call()) {
        change |= 
          Process_global_call(&ipa_callsite[cur_stmt.Get_call_index()],
                              gannot);
      }
      else if (cur_stmt.Is_cond()) {
        change |= 
          Propagate_global_vals(node, 
                                &ipa_ctrl_dep[cur_stmt.Get_cond_index()], 
                                gannot);
      }
    }
  }
  indent -= 2;
  
  return change;
}

//--------------------------------------------------------
// compute the number of conditioanl statements in the PU
// initially, cd is the control dependence for PU entry
//--------------------------------------------------------
static void
Compute_disconnected_graph (SUMMARY_CONTROL_DEPENDENCE* cd, 
                            INT* total_cd_count)
{
  INT num_cd_counts = (cd->Is_if_stmt()) ? 2 : 1;

  for (INT i = 0; i < num_cd_counts; ++i) {

    INT stmt_count, stmt_index;

    if (cd->Is_if_stmt() && i == 1) {
      stmt_count = cd->Get_false_count();
      stmt_index = cd->Get_false_stmt_index();
    }
    else {
      stmt_count = cd->Get_true_count();
      stmt_index = cd->Get_true_stmt_index();
    }

    for (INT j = 0; j < stmt_count; ++j) {
      SUMMARY_STMT* cur_stmt = &ipa_stmt[j+stmt_index];
      if (cur_stmt->Is_cond()) {
        ++(*total_cd_count);
        Compute_disconnected_graph(&ipa_ctrl_dep[cur_stmt->Get_cond_index()],
                                   total_cd_count);
      }
    }
  }
}

// ====================================================================
// ====================================================================
// External functions used in global cprop data-flow algorithm:
//
//   Intra_PU_global_cprop (called from IPA_CPROP_DF_FLOW::Trans)
//
//   Map_constant_values (called from IPA_CPROP_DF_FLOW::PostProcessIO)
// ====================================================================
// ====================================================================

//--------------------------------------------------------------------------
// propagate common block constants, forward. We still need to do
// backward jump functions
// 1) walk the control dependence structure
// 2)  walk the stmts in the cd structure
// 3)   if there is a stid_common and the value is constant then
//        record it in the current annotation. 
// 4)   if we encounter a call node, then we need to get the values of
//        the and propagate them into the caller, use the common block
//        summary information as an annotation
// 5)  for the call node, also check the mod information of the
//     common(s) passed. If there is mod, then mark the annotation as
//     not constant
//--------------------------------------------------------------------------

extern BOOL
Intra_PU_Global_Cprop (IPA_NODE* node)
{
  MEM_POOL_Push(&local_cprop_pool);

  if (Get_Trace(TP_IPA, IPA_TRACE_COMMON_CONST)) {
    fprintf(TFile, "\nIntra_PU_global_cprop in %s\n", node->Name());
  }
  indent += 2;

  BOOL change = FALSE;

#ifdef KEY // bug 2175
  if (IPA_Enable_DFE && PU_is_dead(node, &change))
#else
  if (PU_is_dead(node, &change))
#endif
  {
    return change;
  }

  // set up summary info arrays
  Set_summary_info (node);

  // re-initialize static variables
  Current_caller = node;
  callsite_map = 0;

  SUMMARY_PROCEDURE* proc = node->Summary_Proc();
  INT num_cds = proc->Get_ctrl_dep_count();
  if (num_cds) {

    SUMMARY_CONTROL_DEPENDENCE* cd = &ipa_ctrl_dep[proc->Get_ctrl_dep_index()];
    INT total_cd_count = 1;

    if (!proc->Has_unstructured_cflow()) {
      Compute_disconnected_graph(cd, &total_cd_count);
      if (total_cd_count != num_cds) {
        proc->Set_has_unstructured_cflow();
      }
    }

    // work annotation for intra-pu propagation
    GLOBAL_ANNOT gannot (node->Global_Annot(), &local_cprop_pool);

    if (!proc->Has_unstructured_cflow()) {
      change |= Propagate_global_vals(node, cd, &gannot);
    }
    else {
      Collect_stids_in_unstructured_cflow(node, &gannot);
      Update_annot_with_all_callee_mods(node, &gannot);
      change |= Propagate_vals_in_unstructured_cflow(node, &gannot);
    }
  }

  MEM_POOL_Pop(&local_cprop_pool);
  indent -= 2;

  return change;
}

//---------------------------------------------------------------------
// create a mapping of the constant value and the summary symbol entry
// for this variable so that it is easy to find the symbol table entry
// for it creating the constant
//---------------------------------------------------------------------
extern void
Map_Global_Constants (IPA_NODE *n)
{
  GLOBAL_ANNOT* gannot = n->Global_Annot();
  for (UINT32 i = 0; i < GLOBAL_ANNOT::Size; ++i) {
    if (!gannot->Top(i) && !gannot->Bottom(i)) {
      const GLOBAL_DYN_ARRAY& gvals = gannot->Global_Value_Array(i);
      for (UINT32 j = 0; j < gvals.Elements(); ++j) {
        if (gvals[j].Value()) {
          n->Set_Propagated_Const();
          return;
        }
      }
    }
  }
}
