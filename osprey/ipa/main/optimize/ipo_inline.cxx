/*
 * Copyright (C) 2008-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
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


/* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */

#include <stdint.h>
#include <alloca.h>

#include "assert.h"
#include "defs.h"
#include "config_ipa.h"			// options and flags
#include "lwn_util.h"			// for LWN_Get_Parent()
#include "targ_sim.h"			// for RETURN_INFO
#include "wn_simp.h"			// for simplifier
#include "wn_tree_util.h"		// for tree iterators

#include "ipa_cg.h"			// for call graph stuff
#include "ipa_inline.h"			// for Report_Reason ()
#include "ipo_clone.h"			// for IPO_Copy_Tree ()
#include "ipo_parent.h"			// for WN_Get_Parent ()
#include "ipl_summarize.h"		// for SUMMARY_CREF_SYMBOL
#include "ipo_tlog_utils.h"		// for tlog
#include "ipa_option.h"			// for Trace_IPA
#include "dwarf_DST_producer.h"
#include "dwarf_DST.h"
#include "dwarf_DST_dump.h"
#include "ipaa.h"
#include "clone_DST_utils.h"

#include "ipo_inline.h"
#include "ipa_nystrom_alias_analyzer.h"
static INT initial_initv_tab_size;

MEM_POOL Ipo_mem_pool;
WN_MAP Parent_Map;

extern FILE *Y_inlining;
static BOOL Identify_Partial_Inline_Candiate(IPA_NODE *, WN *,
                                             BOOL, IPO_INLINE_AUX *);
extern DST_IDX
get_abstract_origin(DST_IDX concrete_instance);

#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))
extern BOOL Alias_Nystrom_Analyzer;
#endif

// ======================================================================
// For easy switching of Scope_tab
// ======================================================================
class SCOPE_CONTEXT
{
private:
    SCOPE* _scope_tab;

public:
    SCOPE_CONTEXT (SCOPE *scope) {
        _scope_tab = Scope_tab;
        Scope_tab = scope;
    }

    ~SCOPE_CONTEXT () {
        Scope_tab = _scope_tab;
    }
}; // SCOPE_CONTEXT

#define BASETYPE	TY_mtype
#define ST_ADDR_SAVED	ST_addr_saved
#define ST_ADDR_PASSED	ST_addr_passed
#define SET_ST_ADDR_SAVED	Set_ST_addr_saved
#define SET_ST_ADDR_PASSED	Set_ST_addr_passed
#define RESET_ST_ADDR_SAVED	Clear_ST_addr_saved
#define RESET_ST_ADDR_PASSED	Clear_ST_addr_passed

#define PARTIAL_INL_SIZE        50

//--------------------------------------------
// For an OPR_ARRAY node 'actual' check if all 
// but the leading 'n' indices are equal to 0
//--------------------------------------------
static BOOL 
Subscripts_All_0(const WN* actual, INT n)
{
  INT numdim = WN_num_dim(actual);
  for (INT i = n; i < numdim; i++) {
    const WN* index = WN_array_index(actual, i);
    if (WN_operator(index) != OPR_INTCONST || WN_const_val(index) != 0) {
      return FALSE;
    }
  }
  return TRUE;
}


//---------------------------------------------------------------------
// check if the dimensions are equal
//---------------------------------------------------------------------
static BOOL
Equal_dims(ARB_HANDLE formal_arb, ARB_HANDLE actual_arb)
{
    // Until we have reliable way to locating the ST that holds the upper
    // bound of a variable size array, we only handle constant bound arrays.

    if (ARB_const_lbnd (actual_arb) && ARB_const_ubnd (actual_arb) &&
	ARB_const_lbnd (formal_arb) && ARB_const_ubnd (formal_arb)) {

	INT64 formal_size = abs (ARB_ubnd_val (formal_arb) -
				 ARB_lbnd_val (formal_arb)) + 1;

	INT64 actual_size = abs (ARB_ubnd_val (actual_arb) -
				 ARB_lbnd_val (actual_arb)) + 1;

	return (formal_size == actual_size);
    }

    return FALSE;
}

//-----------------------------------------------------------------------
// return the size of the array dimension
//-----------------------------------------------------------------------
static inline INT
Dim_size (ARB_HANDLE arb)
{
  Is_True(ARB_const_lbnd(arb) && ARB_const_ubnd(arb),
          ("Dim_size: Expected constant array bounds"));
  return abs(ARB_ubnd_val(arb)-ARB_lbnd_val(arb)) + 1;
}

//---------------------------------------------------------------------
// check if the size of an array dimension is equal to 1
//---------------------------------------------------------------------
static inline BOOL
Dim_size_is_1(ARB_HANDLE arb)
{
  return (ARB_const_lbnd(arb) && ARB_const_ubnd(arb) && Dim_size(arb) == 1);
}

//---------------------------------------------------
// check if the size of an array dimension is unknown
// In FORTRAN it's common (and we allow it) to use 
// real x(1) with the same meaning as real x(*)
//---------------------------------------------------
static BOOL
Dim_size_unknown(ARB_HANDLE arb)
{
  return ((!ARB_const_ubnd(arb) && ARB_ubnd_var(arb) == ST_IDX_ZERO) ||
          Dim_size_is_1(arb));
}

// aux. functions for PARAMETER_ATTRIBUTES
static PARM_ITER
Lookup_Parm (PARM_ATTR_VEC& vec, const ST* st)
{
    for (PARM_ITER first (vec.begin ()); first != vec.end(); ++first) {

	if ((*first).Formal_St () == st)
	    return first;
    }
    return vec.end ();
}


// check if the formal parameter is an array type
static BOOL
Formal_Is_Array (const ST* st)
{
    const TY& ty = Ty_Table[ST_type (st)];

    return (ST_sclass (st) == SCLASS_FORMAL && TY_kind (ty) == KIND_POINTER &&
	    TY_kind (TY_pointed (ty)) == KIND_ARRAY);
}


//---------------------------------------------------------------
// Do_Linearize, return TRUE if we will linearize this reference 
//---------------------------------------------------------------
static RESHAPE_METHOD
Do_Linearize (WN* actual, ST* formal_st)
{
  TY_IDX f_tyr = ST_type(formal_st);
  if (ST_sclass(formal_st) != SCLASS_FORMAL_REF) {
    f_tyr = TY_pointed(f_tyr);
  }

  WN* base = (WN_operator(actual) == OPR_ARRAY ? 
              WN_array_base(actual) : actual);

  OPERATOR base_opr = WN_operator(base);
  
  TY_IDX a_tyr;

  if (base_opr == OPR_LDID) {
    a_tyr = ST_type(WN_st(base));
    if (TY_kind(a_tyr) != KIND_POINTER) {
      return RS_EQ_COMMON;
    }
    a_tyr = TY_pointed(a_tyr);
  }
  else if (base_opr == OPR_LDA) {
    a_tyr = ST_type(WN_st(base));
  }
  else {
    return RS_EQ_COMMON;
  }
  
  if (TY_kind(a_tyr) != KIND_ARRAY ||
      TY_size(TY_etype(a_tyr)) != TY_size(TY_etype(f_tyr))) {
    return RS_EQ_COMMON;
  }

  INT f_dims = TY_AR_ndims(f_tyr);
  INT a_dims = TY_AR_ndims(a_tyr);

  // Simple translation of array indices after inlining is only safe 
  // when we are passing the entire array or when indices in the actual 
  // argument are equal to 0 in all dimensions but the MAX(a_dims-f_dims,1) 
  // leading ones, and the dimension sizes of the actual argument in all
  // but the MAX(a_dims-f_dims,1) leading dimensions are equal to the 
  // corresponding dimensions sizes in the formal parameter.
  //
  // That can be overriden with -INLINE:array_bounds flag, which will
  // skip the test for the leading dimension of the formal parameter
  // when that dimension is unknown (x(*) or x(1)).

  if (f_dims == a_dims) {
    if (base == actual || Subscripts_All_0(actual, 1)) {
      for (INT i = 1; i < a_dims; i++) {
        if (!(Equal_dims(TY_arb(f_tyr)[i], TY_arb(a_tyr)[i]))) {
          return RS_LINEARIZE;
        }
      }
      return RS_EQ;
    }
  }

  else if (f_dims < a_dims) {
    INT diff = a_dims - f_dims;

    if (base == actual || 
        Subscripts_All_0(actual, diff) ||
        (INLINE_Array_Bounds && Subscripts_All_0(actual, diff+1))) {

      for (INT f_idx = 0, a_idx = diff; f_idx < f_dims; f_idx++, a_idx++) {

        ARB_HANDLE f_arb = TY_arb(f_tyr)[f_idx];
        ARB_HANDLE a_arb = TY_arb(a_tyr)[a_idx];

        if (!(f_idx == 0 && INLINE_Array_Bounds && Dim_size_unknown(f_arb)) &&
            !(Equal_dims(f_arb, a_arb))) {
          return RS_LINEARIZE;
        }
      }
      return RS_LT;
    }
  } 

#ifdef _WEIRD_CASE_
  else {
    if (base == actual || Subscripts_All_0(actual, 0)) {
      for (INT a_idx = 0, f_idx = f_dims - a_dims; 
           a_idx < a_dims; 
           a_idx++, f_idx++) {
        if (!Equal_dims(TY_arb(f_tyr)[f_idx], TY_arb(a_tyr)[a_idx])) {
          return RS_LINEARIZE;
        }
      }
      // remaining leading dimensions in the formal must be of size 1
      for (f_idx = 0; f_idx < f_dims - a_dims; f_idx++) {
        if (!Equal_dim_1(TY_arb(f_tyr)[f_idx])) {
          return RS_LINEARIZE;
        }
      }
      return RS_GT;
    }
  }
#endif 

  return RS_LINEARIZE;
}


//---------------------------------------------------------------
// check for linearization of ANY parameter                      
//---------------------------------------------------------------
static BOOL
IPA_Do_Linearization (IPA_NODE* callee_node, WN* call, SCOPE* caller_scope) 
{
  WN* callee_wn = callee_node->Whirl_Tree();
  INT num_formals = WN_num_formals(callee_wn);

#ifdef KEY // bug 3898
  // Currently we do inlining even if # of actuals != # of formals. So
  // be careful!
  if (num_formals > WN_kid_count (call))
    num_formals = WN_kid_count (call);
#endif // KEY

  vector<ST*> formals;
  formals.reserve (num_formals);

  // callee scope
  for (INT j = 0; j < num_formals; ++j) {
    formals.push_back (WN_st(WN_formal(callee_wn,j)));
  }

  SCOPE_CONTEXT switch_scope(caller_scope);
    
  for (INT i = 0; i < num_formals; ++i) {
    ST* formal_st = formals[i];
    if (ST_sclass (formal_st) != SCLASS_FORMAL ||
        ST_is_value_parm (formal_st))
      continue;

    if (! Formal_Is_Array (formal_st))
      continue;

    WN* actual = WN_kid(call, i);
    if (WN_operator(actual) == OPR_PARM)
      actual = WN_kid0(actual);

    if (Do_Linearize(actual, formal_st) == RS_LINEARIZE) 
      return TRUE;
  }
  return FALSE;
}


#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))

#ifdef KEY
static BOOL 
Match_Loop_Indexes (WN* wn, vector<ST*> formals, INT num_formals)
{
  OPCODE opc = WN_opcode(wn);
  if (opc == OPC_DO_LOOP) {
    WN* index = WN_index(wn);

    for (INT i = 0; i < num_formals; ++i) {
      ST* formal_st = formals[i];
      if (ST_sclass (formal_st) != SCLASS_FORMAL_REF)
	continue;
      if (formal_st == WN_st(index))
	return TRUE;
    }
  } else if (opc == OPC_BLOCK) {
    for (WN* stmt = WN_first(wn); stmt; stmt = WN_next(stmt)) {
      if (WN_opcode(stmt) == OPC_DO_LOOP) {
	WN* index = WN_index(stmt);

	for (INT i = 0; i < num_formals; ++i) {
	  ST* formal_st = formals[i];
	  if (ST_sclass (formal_st) != SCLASS_FORMAL_REF)
	    continue;
	  if (formal_st == WN_st(index))
	    return TRUE;
	}	
      }      
      for (UINT kidno = 0; kidno < WN_kid_count(stmt); kidno++) {
	if (Match_Loop_Indexes (WN_kid(stmt, kidno), formals, num_formals))
	  return TRUE;
      }
    }
  } else {
    for (UINT kidno = 0; kidno < WN_kid_count(wn); kidno++) {
      if (Match_Loop_Indexes(WN_kid(wn, kidno), formals, num_formals))
	return TRUE;
    }
  }
  return FALSE;
}

static BOOL
Formal_Is_Loop_Index (IPA_NODE* callee, const IPA_EDGE* edge)
{
  WN * call = edge->Whirl_Node();
  WN* callee_wn = callee->Whirl_Tree(FALSE);
  INT num_formals = WN_num_formals(callee_wn);
  INT count_formals_to_test = 0;

  // now that we allow inlining even if there is a mismatch..
  if (edge->Num_Actuals() < num_formals)
    num_formals = edge->Num_Actuals();

  vector<ST*> formals;
  formals.reserve (num_formals);

  for (INT j = 0; j < num_formals; ++j) {
    WN* actual = WN_kid(call, j);
    if (WN_operator(actual) == OPR_PARM &&
	WN_operator(WN_kid0(actual)) == OPR_ARRAY) {
      formals.push_back (WN_st(WN_formal(callee_wn,j)));
      count_formals_to_test ++;
    }
  }

  if (Match_Loop_Indexes(callee_wn, formals, count_formals_to_test))
    return TRUE;

  return FALSE;
}
#endif
// Final verification for incompatible parameters, etc.
// Should move to inline analysis phase.
BOOL
Can_Inline_Call (IPA_NODE* caller, IPA_NODE* callee, const IPA_EDGE* edge) 
{
  SCOPE_CONTEXT switch_scope(callee->Scope());

  if (callee->Has_Varargs()) {
     if (Identify_Partial_Inline_Candiate(callee, callee->Whirl_Tree(),
                                          FALSE, NULL)) {
        if( Get_Trace ( TP_IPA, IPA_TRACE_TUNING) ){
           fprintf (Y_inlining, "Finding a partialn inline candidate %s with var args\n",
                 callee->Name());
        }
     } else {
        return FALSE;
     }
  }

  // if we end up linearizing certain references in fortran
  // then we should not inline
  if (! (edge->Has_Must_Inline_Attrib () ||
         callee->Has_Must_Inline_Attrib () || INLINE_All ||
         IPA_Enable_Linearization)) {
	
    if (IPA_Do_Linearization (callee, edge->Whirl_Node(), caller->Scope())) {
      Report_Reason ( callee, caller,
                      "array reference will be linearized ", edge );

      if (callee->Lexical_Level() > caller->Lexical_Level()) {
        // Callee is a nested function within this caller
        // cannot inline caller if callee has not been inlined
        caller->Set_Noinline_Attrib();
      }

      return FALSE;
    }
  }

  return TRUE;
    
} // Can_Inline_Call

#endif // !_STANDALONE_INLINER



// ----------------------------------------------------------------------
// alloca processing related code: if the callee has an alloca
// inlining it means the procedure boundary where implicit dealloc
// is done is no longer available; so we need to hack whirl to
// a. save stack pointer before call: Process_Alloca_Preamble
// b. restore stack pointer after call: Process_Alloca_Postamble
// ----------------------------------------------------------------------
// helper function:  (from be/lno/scalar_expand.cxx)
static
ST* Find_Return_Registers(TYPE_ID type, PREG_NUM *rreg1, PREG_NUM *rreg2)
{
  TYPE_ID	mtype1;
  TYPE_ID	mtype2;

  if (WHIRL_Return_Info_On) {

    RETURN_INFO return_info = Get_Return_Info (Be_Type_Tbl(type), Use_Simulated);

    if (RETURN_INFO_count(return_info) <= 2) {

      mtype1 = RETURN_INFO_mtype (return_info, 0);
      mtype2 = RETURN_INFO_mtype (return_info, 1);
      *rreg1 = RETURN_INFO_preg (return_info, 0);
      *rreg2 = RETURN_INFO_preg (return_info, 1);
    }

    else
      Fail_FmtAssertion ("Find_Return_Registers: more than 2 return registers");
  }

  else {
    Get_Return_Mtypes(Be_Type_Tbl(type), Use_Simulated, &mtype1, &mtype2);
    Get_Return_Pregs(mtype1, mtype2, rreg1, rreg2);
  }

  if (Preg_Offset_Is_Int(*rreg1))
    return Int_Preg;
  else
    return Float_Preg;
}


//-----------------------------------------------------------------------
// given the base, create an array opcode
//-----------------------------------------------------------------------
static WN*
Create_array(WN* base)
{

  if (WN_operator (base) == OPR_ARRAY)
      return IPO_Copy_Tree (base);

  INT ndims, i;
  OPCODE opc, opc1;
  WN* size_wn, *one, *upper, *lower;
  TY_IDX ty;

  TYPE_ID rtype = WN_rtype(base);
  OPCODE array_op = OPCODE_make_op(OPR_ARRAY, rtype, MTYPE_V);
 
  switch(WN_operator(base)) {
  case OPR_LDID:
    ty = TY_pointed(WN_ty(base));
    break;

  case OPR_LDA:
    ty = ST_type(WN_st(base));
    break;
  }

  assert(TY_kind(ty) == KIND_ARRAY);
  ndims = TY_AR_ndims(ty);
  WN* array_wn =   WN_Create(array_op, ndims + ndims + 1);
  WN* b = IPO_Copy_Tree(base);
  WN_st_idx(array_wn) = WN_st_idx(base);
  WN_array_base(array_wn) = b;
  
  WN_element_size(array_wn) =  TY_size(TY_AR_etype(ty));

  opc = OPC_I4INTCONST;

  // create the size for each dimension 
  for (i=0; i< ndims; ++i) {  

    // if the dimension sizes are constant
    if (TY_AR_const_lbnd(ty, i) && TY_AR_const_ubnd(ty,i))
      {
	INT sum = Dim_size(TY_arb(ty)[i]);
	size_wn = WN_CreateIntconst(opc, sum);
	WN_array_dim(array_wn, i) = size_wn;
	WN_array_index(array_wn,i) = WN_CreateIntconst(opc,0);
	continue;
      }
    // if unknown bounds
    else if
	((TY_AR_lbnd_var(ty, i) == 0) ||
	 (TY_AR_ubnd_var(ty, i) == 0))
	  {
	    WN_array_dim(array_wn, i) = WN_CreateIntconst(opc,0);
	    WN_array_index(array_wn,i) = WN_CreateIntconst(opc,0);
	    continue;
	  }
    // if only the lower bound is constant
    else if (TY_AR_const_lbnd(ty, i))
      {
	upper = WN_CreateLdid (OPCODE_make_op(OPR_LDID,
                                  TY_mtype(ST_type(TY_AR_ubnd_var(ty,i))),
                                  TY_mtype(ST_type(TY_AR_ubnd_var(ty,i)))),
                               0, TY_AR_ubnd_var(ty,i),
                               ST_type(TY_AR_ubnd_var(ty,i)));
	lower = WN_CreateIntconst(opc, TY_AR_lbnd_val(ty, i));

      }

    // if only the upper bound is constant
    else if (TY_AR_const_ubnd(ty, i))
      {
	upper =  WN_CreateIntconst(opc, TY_AR_ubnd_val(ty, i));
	lower =  WN_CreateLdid (OPCODE_make_op(OPR_LDID,
                                    TY_mtype(ST_type(TY_AR_lbnd_var(ty,i))),
                                    TY_mtype(ST_type(TY_AR_lbnd_var(ty,i)))),
                                0, TY_AR_lbnd_var(ty,i),
                                ST_type(TY_AR_lbnd_var(ty,i)));

      }

    else
      // if both bounds are non constant
      {
	lower = WN_CreateLdid (OPCODE_make_op(OPR_LDID,
                                    TY_mtype(ST_type(TY_AR_lbnd_var(ty,i))),
                                    TY_mtype(ST_type(TY_AR_lbnd_var(ty,i)))),
                                0, TY_AR_lbnd_var(ty,i),
                                ST_type(TY_AR_lbnd_var(ty,i)));

	upper = WN_CreateLdid (OPCODE_make_op(OPR_LDID,
                                  TY_mtype(ST_type(TY_AR_ubnd_var(ty,i))),
                                  TY_mtype(ST_type(TY_AR_ubnd_var(ty,i)))),
                               0, TY_AR_ubnd_var(ty,i),
                               ST_type(TY_AR_ubnd_var(ty,i)));

      }
    
    opc1 = OPC_I4SUB;
    size_wn = LWN_CreateExp2( opc1, upper, lower);
    opc1 = OPC_I4INTCONST;
    one = WN_CreateIntconst(opc1, 1);
    opc1 = OPC_I4ADD;
    size_wn = LWN_CreateExp2(opc1, size_wn, one);
    WN_array_dim(array_wn,i) = size_wn;
    opc1 = OPC_I4INTCONST;
    WN_array_index(array_wn,i) = WN_CreateIntconst(opc1, 0);
  }

  // parentize the node and return it
  LWN_Parentize(array_wn);

  return array_wn;
}


//-------------------------------------------------------------------
// Get the original position of a given parameter in the callee BEFORE
// Constant propagation got rid of the constant parameters
//-------------------------------------------------------------------
static INT
Get_orig_param_position (INT curpos, const VALUE_DYN_ARRAY *cprop_annot)
{
    INT i = -1;
    INT idx_of_nonconst_param = -1;

    if (cprop_annot == 0)
	return curpos;
    while (idx_of_nonconst_param != curpos) {
      if (!(*cprop_annot)[i+1].Is_remove_param()) {
	idx_of_nonconst_param++;
      }
      i++;
    }
    return i;
}


// check if the actual parameter is the address of a symbol
static ST *
Get_actual_st_if_passed (WN *actual)
{
    TREE_ITER p (actual); 
    while (p.Wn () != NULL) {
	WN* wn = p.Wn ();

	switch (WN_operator (wn)) {
	case OPR_ILOAD:
	case OPR_MLOAD:
	    p.Skip ();
	    continue;

	case OPR_LDA:
	    {
		ST* st = WN_st (wn);
		if (ST_class (st) == CLASS_CONST)
		    break;
		return st;
	    }
#ifdef KEY
	case OPR_BLOCK:
	    {
	        WN * parent = p.Get_parent_wn();
	    	if (parent && WN_operator(parent) == OPR_COMMA) {
	    	    p.Skip ();
	    	    continue;
	        }
	    }
#endif
	}
	++p;
    }

    return NULL;
} // Get_actual_st_if_passed


// SETTING GLOBAL CALLER/CALLEE TABLES
// ======================================================================
// caller tables MUST be set when operating on the caller              
// ======================================================================
// ASSUME: (for caller and callee)
// (a) Unique Global_Symtab: ST, TY, TYLIST, INITV, INITO, FLD, ARB, PU
// (b) Unique Const (TCON) Table 
// (c) Unique String (STR) Table       
// (d) Unique: MTYPE_To_TY_array and MTYPE_To_PREG_array
//     accessed via MTYPE_To_TY(i) and  MTYPE_To_PREG(t)

// Set the globals: 
// a. SCOPE *Scope_tab, 
// b. SYMTAB_IDX Current_scope (to which CURRENT_SYMTAB is #defined)
// c. PU * Current_pu
// TODO: mempools?
static void
Set_Tables (IPA_NODE* node)
{
    Scope_tab = node->Scope_Table ();
    Current_scope = node->Lexical_Level ();
    Current_pu = &Pu_Table[ST_pu (Scope_tab[Current_scope].st)];
    Parent_Map = node->Parent_Map ();
    Current_Map_Tab = node->Map_Table ();
}


//-------------------------------------------------------------------
// check to see if we can substitute the formal with the actual
//-------------------------------------------------------------------
/*ARGSUSED*/ 
BOOL
IPO_INLINE::SubstituteFormal (ST* formal, WN* actual, INT position)
{
#ifdef _LIGHTWEIGHT_INLINER
    return FALSE;
    // This code will be totally rewritten later, NO NEED TO PORT
    // IPA MODE
#else
#ifndef _STANDALONE_INLINER
  
    if (!IPA_Enable_Copy_Prop)
	return FALSE;

    if (! (IPA_Enable_Simple_Alias && IPA_Enable_Addressing))
	return FALSE;

    const IPAA_NODE_INFO *callee_info = Callee_node ()->Mod_Ref_Info ();

    INT original_param_position = Get_orig_param_position (position,
							   Callee_node ()->Cprop_Annot ());

    if (callee_info->Is_formal_dmod_elmt (original_param_position)) {
	return FALSE;
    } else if (callee_info->Is_formal_dref_elmt (original_param_position))
	return FALSE;

    if (ST_addr_saved (formal) || ST_addr_passed (formal))
	return FALSE;

    if (TY_kind(ST_type(formal)) == KIND_STRUCT)
    return FALSE;

    ST *st = Get_actual_st_if_passed (actual);

    if (st == NULL) {
	return FALSE;
    }

    // assumes the prev test for NULL is false so st is non-null

    if (ST_class(st) == CLASS_CONST || ST_addr_saved (st)) {
	return FALSE;
    }
    

    return TRUE;
    
#else
    // INLINE MODE
    // Do Copy Propogation in the StandAlone Inliner
    // IF INLINE_Enable_Copy_Prop is set
    //  return FALSE;
    
    if (!INLINE_Enable_Copy_Prop) {
	return FALSE;
    }
    
    
    BOOL same_file =
	(Caller_node ()->File_Index () == Callee_node ()->File_Index ()) ;

    if (!same_file) {
      return FALSE;
    }

    if (OPCODE_has_sym(WN_opcode(actual)) && WN_operator(actual) == OPR_LDA) {
	ST *s = WN_st(actual);
	TY_IDX t = WN_ty(actual);
	if ((ST_class(s) != CLASS_CONST) && (ST_ADDR_SAVED(s))) {
	    return FALSE;
	}
	
	// If actual is a ptr to a volatile object do not substitute
	if ((TY_kind(t)== KIND_POINTER) && TY_pointed(t) &&
	    TY_is_volatile(TY_pointed(t))) {
	    return FALSE;
	}

	// substitute also for actuals being structs
	// later: fix pv 411944: substitute also for actuals being arrays
	if ((TY_kind(ST_type(s)) == KIND_SCALAR ||
	     ((ST_class(s) == CLASS_VAR) && TY_kind(t) == KIND_POINTER)) &&
	    (TY_kind(ST_type(formal)) == KIND_POINTER)) {

	    if ((ST_sclass(s) != SCLASS_AUTO) && 
		(ST_sclass(s) != SCLASS_FORMAL) &&
		// Ok to substitute actual if its a FSTATIC constant var
		(! (ST_is_const_var(s) && (ST_sclass(s) == SCLASS_FSTATIC)))) {
		// actual is global
		return FALSE;
	    }

	    // make sure that the types are the same
	    INT formal_element_size = TY_size(TY_pointed(ST_type(formal)));
	    INT actual_element_size = TY_size(ST_type(s));
	    // are the sizes different?
	    // later: check for sizes of elementtype if the actual is an array
	    if (formal_element_size != actual_element_size) {
		return FALSE;
	    }
	} else {
	    // actual is not a scalar or formal is not a pointer
	    return FALSE;
	}
    }  else if (OPCODE_has_sym(WN_opcode(actual)) && 
		WN_operator(actual) == OPR_LDID) {
	// can also substitute if actual is LDID but be
	// very conservative; check below
	// a. callee has NO ISTORE
	// b. callee is leaf at this point (ie all calls in callee inlined)
	// c. formal is not dmod in callee : no stid to formal
	  // actual is not lda
	return FALSE;
    } else {
	// can't figure out the sizes etc
	// actual is not LDA
	return FALSE;
    }
    
    const SUMMARY_PROCEDURE *proc_node = Callee_Summary_Proc ();
    
    // position of this formal entry
    INT formal_index = proc_node->Get_formal_index();
    
    SUMMARY_FORMAL *formal_node =
	Summary->Get_formal (formal_index +
			     Get_orig_param_position (position,
						      Callee_node ()->Cprop_Annot ()));

    INT s_f_indx = formal_node->Get_symbol_index();

    SUMMARY_SYMBOL *f_symbols = Summary->Get_symbol (0);

    SUMMARY_CREF_SYMBOL *cref_symbols = Summary->Get_symbol_crefcount (0);
    INT cref_indx = Summary->Find_symbol_crefcount_index(s_f_indx);

    // case when actual is LDID; substitute if
    // a. callee has NO ISTORE
    // b. callee is leaf at this point (ie all calls in callee inlined)
    // c. formal is not dmod in callee : no stid to formal
    if (OPCODE_has_sym(WN_opcode(actual)) && 
	(WN_operator(actual) == OPR_LDID) ||
	(WN_operator(actual) == OPR_LDA)) {

      if (!(OPCODE_has_sym (WN_opcode(actual)) &&
	       WN_operator(actual) == OPR_LDA))
        // actual is an LDID but cannot substitute
	{
	  return FALSE;
	}
    }

    BOOL cdref;

    // for void calls if the only cref/dref was an arg to STID into a preg
    // then that cref/dref can be ignored.
    if (f_symbols[s_f_indx].Is_cdref_preg_only() &&
	WN_opcode (Call_Wn ()) == OPC_VCALL)
      {
	cdref = FALSE; 
	if (Trace_CopyProp)
		fprintf(TFile, "ignore cref/dref (due to void call) for formal: %s",ST_name(formal));
	Inline_tlog("SubstituteFormal",0,"ignore cref/dref due to void call for formal: %s",ST_name(formal));
      }
    else if ( (cref_indx != -1) && 
	     (cref_symbols[cref_indx].Get_cur_cref_count() == 0))
      {
	cdref = f_symbols[s_f_indx].Is_dref();
	if (Trace_CopyProp)
	   fprintf(TFile, "ignore cref due to refcount being 0 for formal: %s \n",ST_name(formal));
      }
    else 
      cdref = f_symbols[s_f_indx].Is_dref() || f_symbols[s_f_indx].Is_cref();

    
    if (f_symbols[s_f_indx].Is_dmod() ||
	f_symbols[s_f_indx].Is_aref() || 
	cdref ||
	(f_symbols[s_f_indx].Is_addr_saved())) {

	return FALSE;
    } else {
	return TRUE;
    }
#endif
#endif
}

#ifdef KEY // taken from wn_mp.cxx
static const char * const dope_str_prefix = ".dope." ;
static const INT dope_str_prefix_len = 6;
                                                                                
BOOL
ST_Has_Dope_Vector(const ST *st) {
  if (ST_class(st) != CLASS_VAR)
    return FALSE;
                                                                                
  if ( TY_is_f90_pointer(ST_type(st)) )
    return TRUE;
                                                                                
  TY_IDX ty = ST_type(st);
  while (TY_kind(ty) == KIND_POINTER)
    ty = TY_pointed(ty);
                                                                                
  if (TY_kind(ty) == KIND_STRUCT &&
      strncmp(TY_name(ty), dope_str_prefix, dope_str_prefix_len) == 0)
    return TRUE;
                                                                                
  return FALSE;
}
#endif

static BOOL
ST_might_be_modified (const ST* st)
{
    if (ST_class (st) == CLASS_PREG || ST_class (st) == CLASS_CONST ||
	ST_is_const_var (st))
	return FALSE;
    
    switch (ST_sclass (st)) {
    case SCLASS_AUTO:
    case SCLASS_FORMAL:
    case SCLASS_FORMAL_REF:
	if (!ST_addr_saved (st) && !ST_addr_passed (st) &&
	    !ST_has_nested_ref (st))
	    return FALSE;
    }

#ifdef KEY // bug 7871
    if (ST_Has_Dope_Vector (st)) return FALSE;
#endif

    return TRUE;
} // ST_might_be_modified

// MP related functions
// ======================================================================
// update caller MP pragmas 
// (called from Process, Process_Formal, Process_Actual)
// ======================================================================

static void 
Update_Caller_MP_Pragmas(ST* s, WN *wn)
{
  // CALLER SIDE OPTIMIZATION

  // this is the first mp node
  // update all the mp nodes

  WN_VECTOR wnv(Malloc_Mem_Pool);

  WN *tmp = wn;
  while (tmp) {
    if (WN_opcode(tmp) == OPC_REGION && REGION_is_mp(tmp)) {
      	wnv.push_back (tmp);
    }
    tmp = LWN_Get_Parent(tmp);
  }

  if (ST_sclass(s) == SCLASS_AUTO)
#ifdef KEY
      // bug 5149
      Add_Pragma_To_MP_Regions (&wnv, WN_PRAGMA_LOCAL,
                            s, 0, Parent_Map, TRUE);
#else
      Add_Pragma_To_MP_Regions (&wnv, WN_PRAGMA_LOCAL,
                            s, 0, Parent_Map, FALSE);
#endif // KEY
  else
      Add_Pragma_To_MP_Regions (&wnv, WN_PRAGMA_SHARED,
                            s, 0, Parent_Map, FALSE);

}

// create a statement to copy an expression to a temp variable.
// return both the statement and the temp.
static pair<WN*, ST*>
Create_Copy_Expr_For_Ptr (WN* expr, WN* call, BOOL is_mp)
{
    ST* copy_st = Gen_Temp_Symbol(MTYPE_To_TY(Pointer_type), "");
    WN* copy_stmt = WN_Stid (Pointer_type, 0, copy_st, ST_type (copy_st),
                             IPO_Copy_Tree (expr));
    if (is_mp)
         Update_Caller_MP_Pragmas(copy_st, call);

    return std::make_pair (copy_stmt, copy_st);
}

// create a statement to copy an expression to a temp variable.
// return both the statement and the temp.
static pair<WN*, ST*>
Create_Copy_Expr (WN* expr, WN* call, BOOL is_mp)
{
    TYPE_ID desc = WN_rtype (expr);
    ST* copy_st = Gen_Temp_Symbol(MTYPE_To_TY(desc), "");
    WN* copy_stmt = WN_Stid (desc, 0, copy_st, ST_type (copy_st),
			     IPO_Copy_Tree (expr));
    if (is_mp)
	 Update_Caller_MP_Pragmas(copy_st, call);

    return std::make_pair (copy_stmt, copy_st);
}


//------------------------------------------------------------------
// check to see if the array subscript expression need to be copied
//------------------------------------------------------------------
static void 
Process_Actual (WN* actual, WN* copy_in_block, IPO_INLINE& inliner)
{
    UINT count = WN_kid_count (actual);

    for (UINT i = 0; i < count; ++i) {
	WN* wn = WN_kid (actual, i);
	switch (WN_operator (wn)) {
	case OPR_INTCONST:
	case OPR_LDA:
	    continue;

	case OPR_ILOAD:
	    if (WN_operator (WN_kid0 (wn)) != OPR_LDA)
		break;
	    if (ST_might_be_modified (WN_st (WN_kid0 (wn))))
		break;
	    else
		continue;
	    
	case OPR_LDID:
	    if (ST_might_be_modified (WN_st (wn)))
		break;
	    else
		continue;

	case OPR_ARRAY:
	    Process_Actual (wn, copy_in_block, inliner);
	    continue;

	default:
	    break;
	}

	// not a simple constant expression, so generate an assignment
	// statement 

	pair<WN*, ST*> copy_stmt = Create_Copy_Expr (wn, inliner.Call_Wn(),
		inliner.Caller_node()->Summary_Proc ()->Has_parallel_pragma () ||
		inliner.Caller_node()->Summary_Proc ()->Has_parallel_region_pragma());


	LWN_Insert_Block_Before (copy_in_block, NULL, copy_stmt.first);

        WN_kid (actual, i) = WN_Ldid (WN_rtype (wn), 0,
				      ST_st_idx (copy_stmt.second),
				      ST_type (copy_stmt.second));
    }
} // Process_Actual

//---------------------------------------------------------------
// if any actual argument is an array whose subscripts are not constant,
// make a copy of these subscript expressions because we may want to do
// direct substitution for the array and don't want to recompute the
// subscript expressions.
//---------------------------------------------------------------
static void
Copy_Subscript_Expressions (IPO_INLINE_AUX& aux, IPO_INLINE& inliner)
{
    
    PARM_ATTR_VEC& parm_attr = aux.parm_attr;
    for (PARM_ITER iter (parm_attr.begin ()); iter != parm_attr.end ();
	 ++iter) {

	WN* actual = (*iter).Actual_Wn ();
	if (WN_operator(actual) == OPR_ARRAY &&
	    ((*iter).Pass_Method () == PM_UNKNOWN ||
	     (*iter).Pass_Method () == PM_REPLACE)) {
	    
	    Process_Actual (actual, aux.copy_in_block, inliner);
	}
    }
}


//--------------------------------------------------------------------------
// check if the bits need to be set
//--------------------------------------------------------------------------
// TK: Also do this in standalone inliner
#ifdef _STANDALONE_INLINER
static void
Reset_Addr_Bits(ST* s, INT position, IPA_EDGE* edge, IPA_NODE* callee) 
{
    BOOL do_tracing;

    //we came in through inliner copy propogation

    do_tracing = Trace_CopyProp || Trace_IPA;

    // get the symbols array
    SUMMARY_SYMBOL* symbols= Summary->Get_symbol (0);

    // get the entry into the symbols array
    const SUMMARY_CALLSITE* callsites = edge->Summary_Callsite ();

    INT aindx = callsites->Get_actual_index();
    
    INT32 sindx =
	Summary->Get_actual (aindx +
			     Get_orig_param_position (position,
						      callee->Cprop_Annot ()))-> Get_symbol_index ();
    

    if (sindx == -1) {
	if (do_tracing)
	    fprintf(TFile, "Actual index is -1; perhaps never set?: %s\n",
		    ST_name(s));
	return;
    }
    
    // check if a valid symbol node index has been obtained
    SUMMARY_SYMBOL* snode = &symbols[sindx];

    if (!snode->Is_local()) {
	if (do_tracing)
	    fprintf(TFile,
		    "Actual is not local. ST_addr bits NOT RESET for %s\n",
		    ST_name(s));
	return;
    }


    if (snode->Is_addr_saved()) {
	if ((ST_class(s) == CLASS_VAR) || (ST_class(s) == CLASS_FUNC)) {
	    SET_ST_ADDR_SAVED(s);
	    if (do_tracing)
		fprintf(TFile, "ST_addr_saved for symbol %s is true \n",
			ST_name(s));
	} else if (do_tracing)
	    fprintf(TFile, "ST_addr_saved for symbol %s NOT marked \n",
		    ST_name(s));
	
    } else {			/* if (snode->Is_addr_saved()) */
	if (do_tracing)
	    fprintf(TFile, "ST_addr_saved for symbol %s is false\n",
		    ST_name(s));
	if (!snode->Is_addr_passed_inliner()) {
	    // first time around, we need to reset this bit
	    snode->Set_addr_passed_inliner();

	    // reuse st_id bit
	    snode->Reset_cur_addr_passed_count();
	}
	// incr the addr passed count
	snode->Incr_cur_addr_passed_count();

	// if count equals passed count then we can reset the
	// addr_taken_passed bit, if the addr_saved bit has not
	// been set
	if (snode->Get_cur_addr_passed_count() == snode->Get_addr_count()) {
	    RESET_ST_ADDR_SAVED(s);
	    RESET_ST_ADDR_PASSED(s);
	    if (do_tracing) {
		fprintf(TFile, "ST_addr_passed and ST_addr_saved reset for %s \n", 
			ST_name(s));
	    }
	}  /* all calls inlined */
	else if (do_tracing) {
	    // report that all calls didn't get inlined
	    fprintf(TFile, "Addr counts for %s DIFFER; Summary says %d but current count is %d\n",
		    ST_name(s),
		    snode->Get_addr_count(),
		    snode->Get_cur_addr_passed_count()); 
	}
    } /* else part of Is_addr_saved */
}
#endif // _STANDALONE_INLINER


// If we cannot copy-propagate the actual to the callee, we generate an
// assignment of the actual to the formal (now becomes a local variable).  In
// this case, we have to set the dref bit of the actual.  Otherwise, when we
// inline the caller to someother routines, we might mistakenly
// copy-propagate.  
// For example:
//
// 	main () {
// 	    int a;
// 	    foo (&a);
// 	}
//
// 	foo (int *p) {
// 	    bar (p);
// 	}
//
// 	bar (int *q) {
// 	    ... = *q;
// 	}
//
// If we don't mark p as dref, we might copy-propagate &a into p and not set
// the addr_saved bit for a.  But we actually have "q = &a".
#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))
static void
Update_formal_dref (WN *actual, IPA_NODE* caller)
{
    if (!WN_has_sym(actual))
	return;

    IPAA_NODE_INFO *caller_info = caller->Mod_Ref_Info ();

    if (caller_info == NULL)
	return;

    WN *func = caller->Whirl_Tree ();
    ST *st = WN_st (actual);

    switch (ST_sclass (st)) {
    case SCLASS_FORMAL:
    case SCLASS_FORMAL_REF:
	for (INT i = 0; i < WN_num_formals (func); ++i) {
	    if (WN_st (WN_formal (func, i)) != st)
		continue;
	    INT orig_position =
		Get_orig_param_position (i, (VALUE_DYN_ARRAY *)
					 (caller->Cprop_Annot())); 
	    caller_info->Add_formal_dref_elmt (orig_position);
	    break;
	}
	break;
    }
} // IPO_INLINE::Update_formal_dref
#endif // _STANDALONE_INLINER


//------------------------------------------------------------------
// return the element btype given that we have an array as the
// actual parameter
//------------------------------------------------------------------
static 
TYPE_ID Get_element_mtype(WN* actual)
{
  WN* base;
  TY_IDX ty;

  if (WN_operator(actual) == OPR_ARRAY) {
    base = WN_array_base(actual);
    return Get_element_mtype(base);
  }
  else
    base = actual;
      
  switch(WN_operator(base)) {

  case OPR_LDID:
    ty = TY_pointed(WN_ty(base));
    break;

  case OPR_LDA:
    ty = ST_type(WN_st(base));
    break;
    
  default:
    Fail_FmtAssertion("unknown base type\n");
    break;
  }
  
  return 
   BASETYPE(TY_AR_etype(ty));
}

//------------------------------------------------------------------
// return the element type pointer given that we have an array as
// the actual parameter
//------------------------------------------------------------------
static 
TY_IDX Get_element_ty(WN* actual)
{
   WN* base;
   TY_IDX ty;
   TY_IDX result_ty;
   if (WN_operator(actual) == OPR_ARRAY)
       base = WN_array_base(actual);
   else
       base = actual;
      
   switch (WN_operator(base)) {
	
      case OPR_LDID:
	ty = TY_pointed(WN_ty(base));
	break;

      case OPR_LDA:
	ty = ST_type(WN_st(base));
	break;
    
      default:
	for(INT i=0; i<WN_kid_count(base); ++i) {
	   if (result_ty = Get_element_ty(WN_kid(base, i)))
	       return result_ty;
	}
	Is_True(result_ty, ("Get_element_ty cannot find a good ty\n"));
	break;
   }
   result_ty = TY_AR_etype(ty);
   if (result_ty == 0) {  // Not able to get this from the above, try to
			  // get it from WN_ty
	ty = WN_ty(base);
	if (TY_pointed(ty)) {
	    ty = TY_pointed(ty);
	    Is_True((TY_kind(ty) == KIND_ARRAY), ("Must be an array"));
	}
	result_ty = TY_AR_etype(ty);
   }
   return  result_ty;
}


// ======================================================================
// Alloca related processing
// ======================================================================
// alloca processing related code: if the callee has an alloca
// inlining it means the procedure boundary where implicit dealloc
// is done is no longer available; so we need to hack whirl to
// a. save stack pointer before call: Process_Alloca_Preamble
// b. restore stack pointer after call: Process_Alloca_Postamble


// ======================================================================
// helper function:  (from be/lno/scalar_expand.cxx)
// ======================================================================

// save stack pointer 
PREG_NUM
IPO_INLINE::Process_Alloca_Preamble() 
{
    Set_Tables (Caller_node ());

  OPCODE        stop = OPCODE_make_op(OPR_STID, MTYPE_V, Pointer_type);
  OPCODE        ldop = OPCODE_make_op(OPR_LDID, Pointer_type, Pointer_type);
  TY_IDX        pty = Make_Pointer_Type(Be_Type_Tbl(Pointer_type), TRUE);

  PREG_NUM      rreg1;
  PREG_NUM      rreg2;

  // tmp = get_sp()
  char newstname[32] = "$save_sp_before_alloca";
  PREG_NUM saved_sp = Create_Preg_explicit(Pointer_type, (char*)newstname,
				    Caller_Scope(), Caller_level());

  OPCODE      op = OPCODE_make_op(OPR_INTRINSIC_CALL, Pointer_type, MTYPE_V);
  WN*         intrinsic_call = WN_Create(op, 0);
  WN*           sp_tmp_def;

  WN_intrinsic(intrinsic_call) = Pointer_Size == 8 ?
    INTRN_U8READSTACKPOINTER :
  INTRN_U4READSTACKPOINTER;
  ST*         rst = Find_Return_Registers(Pointer_type, &rreg1, &rreg2);
  FmtAssert(rreg1 != 0 && rreg2 == 0, ("Bad pointer type ret regs"));

  LWN_Insert_Block_Before(LWN_Get_Parent(Call_Wn ()), Call_Wn (),
			  intrinsic_call);

  WN* rreg_ldid = WN_CreateLdid(ldop, rreg1, ST_st_idx(rst), pty);
  sp_tmp_def = LWN_CreateStid(stop, saved_sp, MTYPE_To_PREG(Pointer_type),
			      pty, rreg_ldid);

  LWN_Insert_Block_Before(LWN_Get_Parent(Call_Wn ()),Call_Wn (), 
			  sp_tmp_def);
  return saved_sp;
}


// pop/restore stack pointer
void IPO_INLINE::Process_Alloca_Postamble(PREG_NUM saved_sp) 
{

  Set_Tables (Caller_node ());

  TY_IDX        pty = Make_Pointer_Type(Be_Type_Tbl(Pointer_type), TRUE);
  OPCODE        ldop = OPCODE_make_op(OPR_LDID, Pointer_type, Pointer_type);
  OPCODE icallop = OPCODE_make_op(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V);
  WN *intrinsic_call = WN_Create(icallop, 1);
  WN* wn_callarg;
  WN_intrinsic(intrinsic_call) = Pointer_Size == 8 ?
			 INTRN_U8I8SETSTACKPOINTER :
			 INTRN_U4I4SETSTACKPOINTER;
  wn_callarg = WN_CreateLdid(ldop, saved_sp,
			       ST_st_idx(MTYPE_To_PREG(Pointer_type)),
			       Be_Type_Tbl(Pointer_type) );


  WN* tmp=WN_CreateParm(Pointer_type,wn_callarg,pty,WN_PARM_BY_VALUE);
  LWN_Set_Parent(wn_callarg, tmp);
  wn_callarg = tmp;
  
  WN_kid0(intrinsic_call) = wn_callarg;
  LWN_Set_Parent(wn_callarg, intrinsic_call);
  LWN_Insert_Block_After(LWN_Get_Parent(Call_Wn ()), Call_Wn (), intrinsic_call);

}



//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
extern void
Init_inline()
{
  // initialize the opcode to operator table
  Init_Operator_To_Opcode_Table();
}



// GLOBAL MAPS

WN_MAP_TAB *caller_map_tab;
WN_MAP_TAB *callee_map_tab;

// CONSTRUCTOR
//======================================================================
// constructor for IPA/INLINER
// When called from standalone inliner CALLEE_STATE is NULL
//======================================================================
IPO_INLINE::IPO_INLINE (IPA_NODE *caller_node,
			IPA_NODE *callee_node,
			IPA_EDGE *edge) :
    _caller_node(caller_node), _callee_node(callee_node), _call_edge(edge)
{
   

  IP_FILE_HDR& file_hdr_caller = Caller_node()->File_Header ();
  IP_FILE_HDR& file_hdr_callee = Callee_node()->File_Header ();

  // This File Globals (WN_MAP_TAB, WN_MAP)
  caller_map_tab = 
  PU_Info_maptab(file_hdr_caller.proc_info[Caller_node()->Proc_Info_Index()].info);

  callee_map_tab = 
  PU_Info_maptab(file_hdr_callee.proc_info[Callee_node()->Proc_Info_Index()].info);

  Parent_Map = caller_node->Parent_Map();

  // Extern Global (WN_MAP_TAB)
  Current_Map_Tab = caller_map_tab;

  Set_symtab(NULL);

#ifndef _LIGHTWEIGHT_INLINER
  WN_mem_pool_ptr = Caller_node()->Mem_Pool();
#else // _LIGHTWEIGHT_INLINER
  if (INLINE_Use_Malloc_Mempool)
      WN_mem_pool_ptr = Malloc_Mem_Pool;
  else
      WN_mem_pool_ptr = Caller_node()->Mem_Pool();
#endif // _LIGHTWEIGHT_INLINER


  _flags = 0; // clear the flag

#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))
  if (IPA_Enable_DST) {
      // Enter the files in callee dst to caller dst, 
      DST_Enter_Callee_File_Dst(Caller_file_dst(), Callee_file_dst());

      // Set cross file id
      Set_callee_cross_file_id(DST_get_cross_file_id (Caller_dst(),
						      Callee_dst(),
						      Caller_file_dst(),
						      Callee_file_dst()));
  }
#endif // _STANDALONE_INLINER

} // IPO_INLINE::IPO_INLINE


// Propagate Flags
// ======================================================================
// a Propagate PU level flags from callee to caller
// PU_HAS_EXC_SCOPES, PU_NEEDS_FILL_ALIGN_LOWERING, PU_HAS_VERY_HIGH_WHIRL
// PU_MP_NEEDS_LNO,PU_HAS_ALLOCA, PU_HAS_MP, PU_HAS_NAMELIST, PU_HAS_REGION
// TODO: PU_CALLS_SETJMP PU_CALLS_LONGJMP
// Propagate also File_info flags from callee to caller (cross-file inlining)
// FI_NEEDS_LNO,  FI_HAS_INLINES,  FI_HAS_MP
// ======================================================================
static void
Propagate_Flags (IPA_NODE* caller_node, IPA_NODE* callee_node)
{
  PU_IDX caller_pu_idx = ST_pu (caller_node->Func_ST());
  PU_IDX callee_pu_idx = ST_pu (callee_node->Func_ST());

  // caller certainly has inlines (we just inlined callee into it!)
  Pu_Table[caller_pu_idx].flags |= PU_HAS_INLINES;

  // Propagate PU level flags
  // TODO: PU_CALLS_SETJMP PU_CALLS_LONGJMP
  // NOTE: Do not propagate PU_UPLEVEL: since we shouldn't be inlining
  // functions which have PU_UPLEVEL set (as that would cause multiple parents)

  Pu_Table[caller_pu_idx].flags |= 
    (Pu_Table[callee_pu_idx].flags & (PU_HAS_EXC_SCOPES | 
                                      PU_NEEDS_FILL_ALIGN_LOWERING |
                                      PU_HAS_VERY_HIGH_WHIRL |
                                      PU_MP_NEEDS_LNO |
                                      PU_HAS_ALLOCA |
                                      PU_HAS_MP |
                                      PU_HAS_NAMELIST |
                                      PU_HAS_REGION ));

}


// ======================================================================
// MP processing in caller, RETURNS (for subsequent consumption in callee)
// ======================================================================

// Recognize_Caller_MP needs to be invoked BEFORE Process_Callee
// since Process_Callee consumes the two data that this function produces

static void 
Recognize_Caller_MP (IPA_NODE* caller, IPA_NODE* callee)
{
    // CALLER SIDE ANALYSIS

    SUMMARY_PROCEDURE* caller_proc = caller->Summary_Proc ();
    SUMMARY_PROCEDURE* callee_proc = callee->Summary_Proc ();

    if (callee_proc->Has_parallel_pragma()) {
	caller_proc->Set_has_parallel_pragma();
    }

    if (callee_proc->Has_parallel_region_pragma()) {
	caller_proc->Set_has_parallel_region_pragma();
    }
} // Recognize_Caller_MP()


// insert NODES before and after wn 
static void
Insert_Block_Around (WN* parent, WN* wn, WN* before, WN* after)
{
    static char msg[] = "Invalid argument to Insert_Block_Around";

    Is_True (wn && parent && WN_operator (parent) == OPR_BLOCK, (msg));

    if (before) {
	if (WN_operator (before) == OPR_BLOCK) {
	    if (WN_first (before) != NULL) {
		// non-empty block
		WN* first = WN_first (before);
		WN* last = WN_last (before);
		WN_prev (first) = WN_prev (wn);
		WN_next (last) = wn;
		WN_prev (wn) = last;
		if (WN_prev (first))
		    WN_next (WN_prev (first)) = first;
		else
		    WN_first (parent) = first;
	    }
	} else {
	    Is_True (OPERATOR_has_next_prev (WN_operator (before)), (msg));
	    WN_prev (before) = WN_prev (wn);
	    WN_next (before) = wn;
	    WN_prev (wn) = before;
	    if (WN_prev (before))
		WN_next (WN_prev (before)) = before;
	    else
		WN_first (parent) = before;
	}
    }

    if (after) {
	if (WN_operator (after) == OPR_BLOCK) {
	    if (WN_first (after) != NULL) {
		WN* first = WN_first (after);
		WN* last = WN_last (after);
		WN_next (last) = WN_next (wn);
		WN_prev (first) = wn;
		WN_next (wn) = first;
		if (WN_next (last))
		    WN_prev (WN_next (last)) = last;
		else
		    WN_last (parent) = last;
	    }
	} else {
	    Is_True (OPERATOR_has_next_prev (WN_operator (after)), (msg));
	    WN_next (after) = WN_next (wn);
	    WN_prev (after) = wn;
	    WN_next (wn) = after;
	    if (WN_next (after))
		WN_prev (WN_next (after)) = after;
	    else
		WN_last (parent) = after;
	}
    }
} // Insert_Block_Around

    
// ======================================================================
// Insert_Labels: 
// a. Creates a return label and inserts it in the caller after call
// b. Creates an entry label and inserts it before the call
// c. if the last stmt in the inlined block is a goto, remove it
// ======================================================================
static void
Insert_Labels (WN *Call, LABEL_IDX return_label, LABEL_IDX entry_label,
	       WN* inlined_block)  
{
    // CALLER SIDE OPTIMIZATION

    WN* parent_wn = WN_Get_Parent(Call, Parent_Map, Current_Map_Tab);

    Is_True((parent_wn != NULL), ("Invalid Block in Insert Label \n"));
    Is_True(((WN_operator(parent_wn) == OPR_BLOCK)), 
            ("Illegal Parent for the call statement"));

    // generate a label that marks the end of an inlined block 

    WN* ret_wn;
    if (return_label) {
	ret_wn = WN_CreateLabel (return_label, 0, NULL);

	// delete unwanted GOTO
	Is_True (WN_operator (inlined_block) == OPR_BLOCK,
		 ("invalid inlined body"));
	WN* wn_goto = WN_last (inlined_block);
	if ((wn_goto != NULL) && (WN_operator (wn_goto) == OPR_GOTO)) {
	    if (WN_label_number (wn_goto) == return_label) {
		WN_last (inlined_block) = WN_prev (wn_goto);
		if (WN_prev (wn_goto))
		    WN_next (WN_prev (wn_goto)) = NULL;
		else
		    WN_first (inlined_block) = NULL;
	    }
	}
    } else
	ret_wn = NULL;

    WN* entry_wn = entry_label ? WN_CreateLabel (entry_label, 0, NULL) : NULL;

    Insert_Block_Around (parent_wn, Call, entry_wn, ret_wn);

} // Insert_Labels

// CALLER SIDE PRE-PROCESSING
// ======================================================================
// Pre-processing in caller returns
// (c) insert dealloca if callee has alloca
// (d) return label (in caller, after call)
// ======================================================================

// save and restore the current stack pointer before and after the call
static void
Save_And_Restore_Stack (WN* call)
{
    // CALLER SIZE OPERATION
     WN* parent_wn = WN_Get_Parent(call, Parent_Map, Current_Map_Tab);

     Is_True((parent_wn != NULL), ("Invalid Block in Insert Label \n"));
     Is_True(((WN_operator(parent_wn) == OPR_BLOCK)), 
	     ("Illegal Parent for the call statement"));

     PREG_NUM sp = Create_Preg (Pointer_Mtype, "sp");
     WN* save = WN_StidIntoPreg (Pointer_Mtype, sp,
				 MTYPE_To_PREG (Pointer_Mtype),
				 WN_CreateAlloca (WN_Intconst (MTYPE_U4, 0)));

     WN* restore = WN_CreateDealloca (1);
     WN_kid0 (restore) = WN_LdidPreg (Pointer_Mtype, sp);

     Insert_Block_Around (parent_wn, call, save, restore);

} // Save_And_Restore_Stack

void
IPO_INLINE::Pre_Process_Caller (LABEL_IDX& return_label)
{
    // CALLER SIDE OPTIMIZATION
    Set_Tables (Caller_node ()); // Set the globals: Scope_tab, Current_scope, Current_pu

    // MP Processing in the caller 
    // We need to create pragma local for locals from callee that
    // get exposed in a MP region in caller after inlining
    // 
    Recognize_Caller_MP (Caller_node (), Callee_node ());
      
    if (Callee_Summary_Proc()->Has_early_returns())
	New_LABEL (Caller_level (), return_label);
    else
	return_label = 0;

} // IPO_INLINE::Pre_Process_Caller() 


// CALLEE SIDE PROCESSING
static inline void
Replace_Return_By_Goto (TREE_ITER& iter, LABEL_IDX ret_label) 
{
    // CALLEE SIDE OPTIMIZATION
    iter.Replace (WN_CreateGoto( (ST_IDX) 0, ret_label));
}

// CALLEE SIDE CLONING
// ======================================================================
// Uses IPO_CLONE to create a copy of the callee in _block;
// ======================================================================
WN *
IPO_INLINE::Clone_Callee(BOOL same_file)
{
  // CALLEE SIDE OPTIMIZATION

  IPO_CLONE copy(caller_map_tab, callee_map_tab, 
                 Parent_Map,
                 Callee_Scope(), Caller_Scope(),
		 Callee_level(), Caller_level(),
		 Callee_node()->Cloned_Symtab(),
                 &Ipo_mem_pool,
                 same_file, 0);


  if (!Callee_node()->Cloned_Symtab()) {
      if (Callee_node()->Has_Pstatics ())  {
          copy.Promote_Statics();
          Callee_node()->Clear_Has_Pstatics();   
      }

      copy.Get_sym()->Update_Symtab(FALSE);

      Set_symtab(copy.Get_sym());

      Callee_node()->Set_Cloned_Symtab(Symtab());
  }
  else {
      copy.Get_sym()->Update_Symtab(TRUE);  // Only copy LABEL and INITO table
      Set_symtab(Callee_node()->Cloned_Symtab());
  }

  WN *result = copy.Clone_Tree(WN_func_body(Callee_Wn ()));

#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))

  if (Caller_node ()->Has_frequency ()) {
      FB_FREQ call_freq = Call_edge()->Get_frequency();
      if (call_freq.Known ()) {
	  FB_FREQ callee_freq = Callee_node ()->Has_frequency () ?
	      Callee_node()->Get_frequency() : FB_FREQ_UNKNOWN;
	  FB_FREQ scale (FB_FREQ_ZERO);
	  if (! callee_freq.Known ()) {
	      DevWarn ("Inconsistent freq. count between callsite in %s and"
		       " callee %s",  Caller_node()->Name (),
		       Callee_node()->Name ());
	  } else {
	      scale = call_freq / callee_freq;
	      SUMMARY_FEEDBACK* fb = Callee_node ()->Get_feedback ();
	      fb->Set_frequency_count (callee_freq - call_freq);
	  }
	  
#ifdef _DEBUG_TEST
printf("scale for %s in %s is %f", Callee_node()->Name(), Caller_node()->Name(), scale);
printf(", callee_freq = %f", callee_freq.Value());
printf(", call_freq = %f\n", call_freq.Value());
#endif // _DEBUG_TEST

	  FB_IPA_Inline (Callee_node()->Feedback_Info(),
			 Caller_node()->Feedback_Info(),
			 Callee_Wn (), result, scale);

      }
  }
#endif // _STANDALONE_INLINER

  return (result);

}


void
IPO_INLINE::Process_OPR_REGION(WN* wn, IPA_NODE* caller_node)
{
    // create unique (in caller) id for the region before inlining
    // by BUMPING UP the max_region_id in caller
    PU& caller_pu = Pu_Table[ST_pu(caller_node->Func_ST())];

    if (PU_has_region(caller_pu)) {
	// need to renumber the callee region id
	UINT32 region_id = caller_node->Max_Region_Id();

	if (region_id == 0) { // hasn't yet been computed
	    region_id = Compute_max_region_id (caller_node->Whirl_Tree ());
	}

	++region_id;
	caller_node-> Set_Max_Region_Id(region_id);
	WN_set_region_id(wn, region_id);
    } else 
	caller_node-> Set_Max_Region_Id(WN_region_id(wn));
	

    if (WN_region_is_EH(wn)) {
	INITO_IDX init_idx = WN_ereg_supp (wn);

	if (init_idx) {
		
	    pair<ST*, WN_OFFSET> sym = Create_Copy_In_Symbol (INITO_st(init_idx)); 
	    ST* copy_st = sym.first;
            Clear_ST_is_temp_var (copy_st);
            Set_ST_is_not_used (INITO_st(init_idx));
            Set_ST_storage_class (*copy_st, ST_storage_class(*INITO_st(init_idx)));
	    Set_INITO_st_idx(init_idx, ST_st_idx(copy_st));

	    INITV_IDX val_idx = INITO_val(init_idx);

	    if (val_idx < initial_initv_tab_size) {
		Set_INITO_val(init_idx,
		    Symtab()->Clone_INITVs_For_EH(INITO_val(init_idx), init_idx)); 
	    }
#ifdef KEY
// For certain cases we don't have enclosing EH regions across a function
// call. e.g. a function with empty specification is NOT supposed to throw,
// so we do not enclose a call to it inside an EH region.
	    if (WN_block_empty (WN_region_pragmas (wn)) &&
	    	Call_edge()->EH_Whirl_Node()) {// not a try-region
	    	INITV_IDX initv = INITV_blk (INITO_val (init_idx));

		INITO_IDX enclosing = WN_ereg_supp (Call_edge()->EH_Whirl_Node());
		INITV_IDX e_initv = INITV_blk (INITO_val (enclosing));
		if (INITV_kind(initv) == INITVKIND_ZERO) {
		// copy all the information from caller.
		    if (INITV_kind(e_initv) != INITVKIND_ZERO) {
		      INITV_IDX insert = New_INITV();
		      INITV_Init_Block (INITO_val(init_idx), insert);
		      while (e_initv) {
		      	memcpy (&Initv_Table[insert], &Initv_Table[e_initv], sizeof(INITV));
			e_initv = INITV_next (e_initv);
			if (e_initv) {
			    INITV_IDX prev = insert;
			    insert = New_INITV();
			    Set_INITV_next (prev, insert);
			} // if
		      } // while
		    } // != INITVKIND_ZERO
		} else {
		// merge: we don't need to do anything with the caller landing
		// pad (should be taken care of while handling _unwind_resume).
		// Merge typeinfos: maintain their declaration order
		    INITV_IDX last_initv = INITV_IDX_ZERO;
		    while (initv)
		    {
			if (INITV_kind (initv) == INITVKIND_VAL)
			{
			  int sym = TCON_uval (INITV_tc_val (initv));
			  if (sym < 0)
			  {
			    int new_ofst = -(mINT32)(-sym + 
			    			caller_node->EH_spec_size());
			    INITV_IDX bkup = INITV_next (initv);
			    INITV_Set_VAL (Initv_Table[initv],
					   Enter_tcon (Host_To_Targ (MTYPE_I4, 
					   new_ofst)), 1);
			    Set_INITV_next (initv, bkup);
			  }
			}
			last_initv = initv;
		    	initv = INITV_next(initv);
		    }
		    initv = last_initv;
		    // remember: e_initv points to the landing-pad, the next
		    // field is the typeinfo
		    e_initv = INITV_next (e_initv);
		    while (e_initv) {
		        INITV_IDX insert = New_INITV();
			memcpy (&Initv_Table[insert], &Initv_Table[e_initv], sizeof(INITV));
			Set_INITV_next (initv, insert);
			initv = insert;
		    	e_initv = INITV_next (e_initv);
		    }
		}
	    } else if (WN_block_empty (WN_region_pragmas (wn))) {
	    // Call_edge()->EH_Whirl_Node() == NULL (no eh region around 
	    // this callsite). Fix any eh-spec offsets
	    	INITV_IDX initv = INITV_blk (INITO_val (init_idx));
		if (INITV_kind (initv) != INITVKIND_ZERO) {
		    while (initv) {
			if (INITV_kind (initv) == INITVKIND_VAL) {
			  int sym = TCON_uval (INITV_tc_val (initv));
			  if (sym < 0) {
			    int new_ofst = -(mINT32)(-sym + 
			    			caller_node->EH_spec_size());
			    INITV_IDX bkup = INITV_next (initv);
			    INITV_Set_VAL (Initv_Table[initv],
					   Enter_tcon (Host_To_Targ (MTYPE_I4, 
					   new_ofst)), 1);
			    Set_INITV_next (initv, bkup);
			  }
			}
		    	initv = INITV_next(initv);
		    }
		}
	    }
#endif
	}
	//The following code creat a new TY for the ST that is updated
	//above.In the code above, it changes the INITO which is 
	//attached a ST,but it don't update the size of the TY.
	//Because in CG, we will get the size of the ST from its TY,
	//we should get its right size from the INITO attach with it,
	//and write it into a new TY
	UINT inito_size = Get_INITO_Size(init_idx);
	ST* temp_st = INITO_st (init_idx);
	TY_IDX tyi;
	TY& zty = New_TY (tyi);
	TY_Init (zty, inito_size, KIND_STRUCT, MTYPE_M,temp_st->u1.name_idx);
        Set_TY_align (tyi, 4);          
        temp_st->u2.type = tyi;
    }

    Set_PU_has_region(caller_pu);
} // Process_OPR_REGION

static vector<TY_IDX, mempool_allocator<TY_IDX> > processed_types (Malloc_Mem_Pool);

static BOOL
ty_processed(TY_IDX ty)
{
    for (int i = 0; i < processed_types.size(); ++i) {
	if (TY_IDX_index(processed_types[i]) == TY_IDX_index(ty))
	    return TRUE;
    }
    return FALSE;
}

static void
fix_var_dim_array(TY_IDX ty, IPO_SYMTAB* symtab)
{
    if (TY_kind(ty) == KIND_POINTER)
        ty = TY_pointed(ty);

    if (TY_kind(ty) == KIND_ARRAY) {

	if (ty_processed(ty))
	   // Already processed
	   return;

        ARB_ITER arb_iter = Make_arb_iter (ARB_HANDLE (TY_arb(ty)));
        UINT dim = ARB_dimension (arb_iter);
	processed_types.push_back(ty);
        for (UINT i = 0; i < dim; ++i) {
            ARB_HANDLE arb (arb_iter);
            if (!ARB_const_lbnd(arb)) {
                ST_IDX idx = ARB_lbnd_var(arb);
		if (ST_IDX_index(idx) != 0)
		    Set_ARB_lbnd_var(arb, ST_st_idx(symtab->Get_ST(ST_ptr(idx))));
	    }
            if (!ARB_const_ubnd(arb)) {
                ST_IDX idx = ARB_ubnd_var(arb);
		if (ST_IDX_index(idx) != 0)
		    Set_ARB_ubnd_var(arb, ST_st_idx(symtab->Get_ST(ST_ptr(idx))));
	    }
            if (!ARB_const_stride(arb)) {
                ST_IDX idx = ARB_stride_var(arb);
		if (ST_IDX_index(idx) != 0)
		    Set_ARB_stride_var(arb, ST_st_idx(symtab->Get_ST(ST_ptr(idx))));
	    }
            ++arb_iter;
        }
        fix_var_dim_array(TY_etype(ty), symtab);
    }
}
// ======================================================================
// Process_Op_Code:
// 	For opcodes like RETURN, GOTO, LABEL, REGION, ARRAY
//	etc, need to process each one individually
// ======================================================================

void
IPO_INLINE::Process_Op_Code (TREE_ITER& iter, IPO_INLINE_AUX& aux)
{
    WN* wn = iter.Wn ();
    OPERATOR oper = WN_operator (wn);
    switch(oper) {
    case OPR_RETURN_VAL:
	// the conversion is done is two steps:
	// 1) convert the return_val <expr> into stid <expr> followed by a
	//    regular return
	// 2) we then fix up the <expr>, the "faked" return node will be
	//    eventually converted into a goto or simply deleted
	if (WN_operator (WN_kid0 (wn)) == OPR_LDA) {
	    ST* st = WN_st(WN_kid0 (wn));
	    if (st != NULL)
	        Set_ST_addr_saved(st);
	}
	else
	if (WN_operator (WN_kid0 (wn)) == OPR_COMMA) {
	    WN *wn1 = WN_kid0 (wn); // comma node
	    // loop till we get the result value of comma node
	    do {
		wn1 = WN_kid1 (wn1);
	    } while (WN_operator (wn1) == OPR_COMMA);
	    if (WN_operator (wn1) == OPR_LDA) {
		ST* st = WN_st(wn1);
		if (st != NULL)
		    Set_ST_addr_saved(st);
	    }
	}
	if (WN_rtype (WN_kid0 (wn)) == MTYPE_M) {
	    ST* tmp_st = aux.rp.find_st ();
#ifdef KEY
	    TY_IDX stid_ty = ST_type(tmp_st);
#else
	    TY_IDX stid_ty = WN_ty(WN_kid0(wn));
#endif // KEY

#if (defined(_STANDALONE_INLINER) || defined(_LIGHTWEIGHT_INLINER))
	    if (stid_ty == 0) { // This is potentially a COMMA node

		// if (WN_operator(WN_kid0(wn)) == OPR_COMMA)
		    stid_ty = WN_ty(WN_kid1(WN_kid0(wn)));
	    }
#endif // _STANDALONE_INLINER
	    iter.Insert (WN_Stid (MTYPE_M, 0, tmp_st, stid_ty,
				  WN_kid0 (wn))); 
	} else 
	    iter.Insert (WN_StidIntoPreg (TY_mtype (ST_type (aux.rp.find_st())),
					  aux.rp.find ((PREG_IDX) -1),
					  aux.rp.find_st (),
					  WN_kid0 (wn)));
	// at this point the iterator points to the newly inserted node
	++iter;				// skip the STID node

	// we should have generated a real OPR_RETURN node, but a fake
	// should be enough in this case
	WN_set_operator (wn, OPR_RETURN);
#ifdef KEY
	// make sure we have proper types
	WN_set_rtype (wn, MTYPE_V);
	WN_set_desc (wn, MTYPE_V);
#endif
	return;
	
    case OPR_RETURN:
	/* translate RETURN to goto
	   only if the callee is NOT marked single-return */
	if (Callee_Summary_Proc()->Has_early_returns())
	    Replace_Return_By_Goto (iter, aux.return_label);
	else {
	    iter.Delete ();
	    return;
	}
	break;
  
	// Exceptions now come as REGIONS (not as EXC_SCOPE_BEGINS)
    case OPR_EXC_SCOPE_BEGIN:
	Fail_FmtAssertion ("Invalid opcode from old style Exception Processing\n");
	break;
  
    case OPR_REGION:
	Process_OPR_REGION (wn, Caller_node ());
	break;
      
    case OPR_PRAGMA:
    case OPR_XPRAGMA:

	if (oper == OPR_PRAGMA && WN_pragma(wn) == WN_PRAGMA_PREAMBLE_END) {
	    iter.Delete ();
	    return;
	} else {
	    const ST* st;
	    switch (WN_pragma(wn)) {
	    case WN_PRAGMA_DISTRIBUTE:
	    case WN_PRAGMA_DISTRIBUTE_RESHAPE:
	    case WN_PRAGMA_DYNAMIC:
		st = WN_st (wn);
		if (st && (ST_sclass(st) == SCLASS_FSTATIC ||
			   ST_sclass(st) == SCLASS_EXTERN ||
			   ST_sclass(st) == SCLASS_UGLOBAL ||
			   ST_sclass(st) == SCLASS_DGLOBAL ||
			   ST_sclass(st) == SCLASS_COMMON ||
			   (ST_base(st) &&
			    ST_sclass(ST_base(st)) == SCLASS_COMMON))) {
		    iter.Delete ();
		    return;
		}
	    default:
		break;
  	    }
	}
	break;
	
#ifdef KEY
    case OPR_CALL:
    {
        WN * kid0;
	if (WN_kid_count(wn) == 1 && (kid0 = WN_kid0 (WN_kid0 (wn))))
	{
	    if (WN_operator (kid0) == OPR_LDID &&
	        ST_one_per_pu (&St_Table[WN_st_idx (kid0)]))
	    {
    		ST_IDX i = WN_st_idx (wn);
		char * func_name = ST_name (St_Table[i]);
		Is_True (func_name && 
			   (!strcmp (func_name, "_Unwind_Resume") || 
			    !strcmp (func_name, "__cxa_call_unexpected") || 
			    !strcmp (func_name, "__cxa_begin_catch") ||
			    !strcmp (func_name, "__cxa_get_exception_ptr")), 
			   ("Function %s has one-per-pu paramter", func_name));
	    	// Fixup parameter
	    	INITV_IDX exc_ptr = INITO_val ((INITO_IDX) PU_misc_info (Get_Current_PU ()));
		WN_st_idx (kid0) = TCON_uval (INITV_tc_val (exc_ptr));
	    }
	}
    }
	break;
    case OPR_EQ:
        if (WN_operator (WN_kid0 (wn)) == OPR_LDID &&
	    ST_one_per_pu (&St_Table[WN_st_idx (WN_kid0 (wn))]))
	{
// We have something like: cmp <LDID from_address>, <filter_integer>
// (1) Get the correct from_address from Get_Current_PU()
// (2) The filter value is obtained from the typeinfo table for the PU.
// The callee typeinfo table must have been merged already into the 
// caller typeinfo table.
// (2a) From the callee PU information, find out the ST_IDX corresponding to
// the current filter value.
// (2b) Get the corresponding typeinfo in the caller table.
// (2c) From the caller typeinfo table, get the filter value.
//
	    // fix the first parameter, take the proper input from runtime
	    // (1)
	    INITV_IDX first = INITO_val ((INITO_IDX) PU_misc_info (Get_Current_PU ()));
	    WN_st_idx (WN_kid0 (wn)) = TCON_uval (INITV_tc_val (INITV_next (first)));
	    // fix the 2nd parameter -- filter value
	    int current_filter = WN_const_val (WN_kid1 (wn));

	    if (current_filter < 0)
	    { // filter for exception specification
	    	WN_const_val (WN_kid1 (wn)) = - (mINT32)(-current_filter + Caller_node()->EH_spec_size());
		break;
	    }
	    Set_Tables (Callee_node());

	    INITO_IDX ino = PU_misc_info (Pu_Table [ST_pu (Callee_node()->Func_ST())]);
	    INITV_IDX tinfo = INITV_next (INITV_next (INITO_val (ino)));
	    INITV_IDX ttable = INITO_val (TCON_uval (INITV_tc_val (tinfo)));
	    int filter;
	    // (2a)
	    while (ttable)
	    {
	    	filter = TCON_uval (INITV_tc_val (INITV_next (INITV_blk (ttable))));
		if (filter == current_filter)
		    break;
		ttable = INITV_next (ttable);
	    }
	    FmtAssert (filter == current_filter, ("No matching ST for exception filter in callee typeinfo-table"));
	    ST_IDX current_st = 0;
	    {
	      INITV_IDX i = INITV_blk (ttable);
	      FmtAssert (INITV_kind (i) != INITVKIND_ZERO, ("Unexpected 0 st_idx for cmp operand"));
	      current_st = TCON_uval (INITV_tc_val (i));
	    }
#ifdef Is_True_On
	    {// DGLOBAL: user defined types. EXTERN: builtin types like 'char'
	    	ST_SCLASS s = ST_sclass (St_Table[current_st]);
	    	FmtAssert (s == SCLASS_DGLOBAL || s == SCLASS_EXTERN, ("Typeinfo symbol should be global"));
	    }
#endif
	    Set_Tables (Caller_node());

	    ttable = INITO_val (TCON_uval (INITV_tc_val (INITV_next (INITV_next (first)))));
	    // (2b)
	    ST_IDX search;
	    while (ttable)
	    {
		INITV_IDX i = INITV_blk (ttable);
		if (INITV_kind (i) == INITVKIND_ZERO)
		    search = 0;
		else
	    	    search = TCON_uval (INITV_tc_val (i));
		if (search == current_st)
		    break;
		ttable = INITV_next (ttable);
	    }
	    FmtAssert (search == current_st, ("No matching ST for exception filter in merged caller typeinfo-table"));
	    // (2c)
	    WN_const_val (WN_kid1 (wn)) = TCON_uval (INITV_tc_val (INITV_next (INITV_blk (ttable))));
	}
        break;
#endif

    default:
	break;
    }

    if (OPERATOR_has_sym (oper))
	Process_ST (iter, aux);
    else
	++iter;
} // Process_Op_Code

//-----------------------------------------------------------------------
// Process Pregs, pregs are always entered in the global symtab
//-----------------------------------------------------------------------
static void
Process_Pregs(WN* w, const RETURN_PREG& rp)
{
    INT offst;

    // if the ste has been processed before, 
    // then get it from the array
    if (WN_operator(w) == OPR_PRAGMA)
	offst = WN_pragma_arg1(w);
    else
	offst = WN_offset(w);

    // this code involves fixing dedicated pregs, i.e. return registers
    if (WN_operator (w) == OPR_STID) {
	if (offst == -1) {
	    WN_st_idx (w) = ST_st_idx (rp.find_st ());
	    if (WN_desc (w) == MTYPE_M)
		WN_offset (w) = 0;
	    else
		WN_offset (w) = rp.find (offst);
	} else if (Preg_Is_Dedicated(offst))
	    WN_offset (w) = rp.find (offst);
    }
}
  

// return TRUE if there is any references to formal parameters that are of
// type *ARRAY
static BOOL
has_array_reference (WN* wn)
{
    TREE_ITER iter (wn);
    while (iter.Wn () != NULL) {
	WN* wn = iter.Wn ();
	OPERATOR opr = WN_operator (wn);
	if (OPERATOR_has_sym (opr)) {
	    ST* st = WN_st (wn);
	    if (st != NULL && ST_sclass (st) == SCLASS_FORMAL &&
		TY_kind (ST_type (st)) == KIND_POINTER &&
		TY_kind (TY_pointed (ST_type (st))) == KIND_ARRAY)
		return TRUE;
	} 
	++iter;
    }
    return FALSE;
}

// return TRUE if it is helpful to generate barriers/unique_pointers (see
// Disambiguate_Aliased_Actuals below) to tell the backend that the actual
// parameters are actually not aliased.
//
// Note that generating these barriers are not always good.  So we do that
// only if the callee uses these formal parameters as arrays within a
// loop. 
static BOOL
Unaliased_Actuals_Useful (WN* wn)
{
    TREE_ITER iter (wn);
    while (iter.Wn () != NULL) {
	OPERATOR opr = WN_operator (iter.Wn ());
	switch (opr) {
	case OPR_DO_LOOP:
	case OPR_DO_WHILE:
	case OPR_WHILE_DO:
	    if (has_array_reference (iter.Wn ()))
		return TRUE;
	    iter.Skip ();
	    break;

	default:
	    if (OPERATOR_is_stmt (opr))
		iter.Skip ();
	    else
		++iter;
	    break;
	}
    }
    return FALSE;
} // Unaliased_Actuals_Useful


// ======================================================================
// Return a named array ST for a given wn.
// ======================================================================
static ST *
Get_Array_ST (WN *wn)
{
    TREE_ITER iter (wn);
    while (iter.Wn () != NULL) { 
	WN* w = iter.Wn ();
	ST* st;
	switch (WN_operator (w)) {
	case OPR_LDA:
	    st = WN_st (w);
	    if (TY_kind (ST_type (st)) == KIND_ARRAY)
		return st;
	    ++iter;
	    break;
	case OPR_LDID:
	    st = WN_st (w);
	    {
		const TY& ty = Ty_Table[ST_type (st)];
		if (TY_kind (ty) == KIND_POINTER &&
		    TY_kind (TY_pointed (ty)) == KIND_ARRAY)
		    return st;
	    }
	    ++iter;
	    break;
	case OPR_ILOAD:
	    iter.Skip();
	    break;
	default:
	    ++iter;
	}
    }
    return NULL;
} // Get_Array_ST


static inline BOOL
Is_Scalar_Formal_Ref (const ST* st)
{
    if (ST_sclass (st) != SCLASS_FORMAL_REF)
	return FALSE;

    TY& ty = Ty_Table[ST_type (st)];

    if (TY_kind (ty) == KIND_POINTER || TY_kind (ty) == KIND_ARRAY)
	return FALSE;			// not a scalar
    return TRUE;
}

// ======================================================================
// Go through the actual argument list and mark potential name aliased 
// arguments, e.g., foo( a(1), a(k) ).
// Apply Fortran's no-aliased argument rules to mark parameters as
// non-aliased.   Use either "copy-in/copy-out" or "unique point with
// barrier" to tell the backend that it is safe to assume these arrays
// don't overlap.
//
// return FALSE if no disambiguation is needed
// ======================================================================
static void
Disambiguate_Aliased_Actuals (PARM_ATTR_VEC& parm_attr, PU& pu)
{
    UINT  i;
    const UINT num_parm = parm_attr.size ();
    ST** actuals = (ST**) alloca (num_parm * sizeof(ST*));
    
    // get the base ST of each array parameter
    for (i = 0; i < num_parm; ++i) {
	actuals[i] = Get_Array_ST (parm_attr[i].Actual_Wn ());
    }

    BOOL done = TRUE;
    BOOL* aliased = (BOOL*) alloca (num_parm * sizeof(BOOL));
    BOOL* scalar = (BOOL*) alloca (num_parm * sizeof(BOOL));

    BZERO (aliased, sizeof(BOOL)*num_parm);
    BZERO (scalar, sizeof(BOOL)*num_parm);

    // record those ST that are aliased
    for (i = 0; i < num_parm; ++i) {
	ST* st = actuals[i];
	if (st == NULL) {
	    aliased[i] = scalar[i] = FALSE;
	    continue;
	} else if (aliased[i])
	    continue;
	
	for (INT j = i + 1; j < num_parm; ++j) {
	    if (st == actuals[j]) {
		aliased[j] = aliased[i] = TRUE;
		done = FALSE;
		scalar[j] = Is_Scalar_Formal_Ref (parm_attr[j].Formal_St ());
	    }
	}
	if (aliased[i])
	    scalar[i] = Is_Scalar_Formal_Ref (parm_attr[i].Formal_St ());
    }

    if (done)				// no aliased found
	return;
    
    // if the formal is a scalar and the actual is an array element, we
    // don't need to generate barrier for this array.  Instead, we use
    // copy-in/copy-out for the scalar and direct substitution for the
    // array parameter.  We represent this by setting the actual_aliased
    // bit for the scalar parameter but clear the bit for the aliased array
    // parameter.
    done = FALSE;
    for ( i = 0; i < num_parm; ++i) {
	if (! aliased[i] || parm_attr[i].Actual_Aliased () != NULL)
	    continue;
	if (scalar[i]) {
	    parm_attr[i].Set_actual_aliased (actuals[i]);
	    parm_attr[i].Set_pass_method (PM_COPY_IN_OUT);
	    parm_attr[i].Set_fixup_method (FM_REPLACE_ST);
	    continue;
	}
	// look for other aliased arrays
	for (UINT j = i + 1; j < num_parm; ++j) {
	    if (aliased[j] && !scalar[j] && actuals[i] == actuals[j]) {

		parm_attr[i].Set_actual_aliased (actuals[i]);
		parm_attr[i].Set_pass_method (PM_NEED_BARRIER);
		parm_attr[i].Set_fixup_method (FM_REPLACE_ST);

		parm_attr[j].Set_actual_aliased (actuals[j]);
		parm_attr[j].Set_pass_method (PM_NEED_BARRIER);
		parm_attr[j].Set_fixup_method (FM_REPLACE_ST);

		Set_PU_smart_addr_analysis (pu);
	    }
	}
    }
} // Disambiguate_Aliased_Actuals


namespace {
    // function object used by Generate_Barriers
    typedef pair<ST*, BOOL> BARRIER_ST;

    struct barrier_st_cmp {
	BOOL operator() (BARRIER_ST st1, BARRIER_ST st2) const {
	    if (st1.first == st2.first)
		return st1.second < st2.second;
	    return st1.first < st2.first;
	}
    };
	
}
// generate around the inlined body:
//
// forward barrier <list of actuals>
//   backward barrier <list of copied actuals>
//     inlined body
//   forward barrier <list of copied actuals>
// backward barrier <list of actuals>
static void
Generate_Barriers (WN* parent, WN* call, IPO_INLINE_AUX& aux)
{
    PARM_ATTR_VEC& parm_attr = aux.parm_attr;

    vector<BARRIER_ST, mempool_allocator<BARRIER_ST> >
	actual_list (&Ipo_mem_pool);
    actual_list.reserve (parm_attr.size ());

    UINT num_barrier_formal = 0;
    PARM_ITER p (parm_attr.begin ());
    for (; p != parm_attr.end (); ++p) {
	ST* st = p->Actual_Aliased ();
	if (st != NULL) {
	    BOOL use_lda = p->Restrict_Pointer ();
	    if (! use_lda) {
		if (TY_kind (ST_type (st)) == KIND_ARRAY)
		    use_lda = TRUE;
		else {
		    // use ldld
		    Is_True (TY_kind (ST_type (st)) == KIND_POINTER &&
			     TY_kind (TY_pointed (ST_type (st))) == KIND_ARRAY,
			     ("Invalid symbol for barriers"));
		}
	    }
	    actual_list.push_back (BARRIER_ST (st, use_lda));
	}
	if (p->Pass_Method () == PM_NEED_BARRIER)
	    ++num_barrier_formal;
    }

    if (actual_list.size () == 0)
	return;

    // get rid of duplicates
    sort (actual_list.begin (), actual_list.end (), barrier_st_cmp ());
    UINT size = unique (actual_list.begin (), actual_list.end ()) -
	actual_list.begin ();
    
    // create barriers for the actual parameters
    WN* before = WN_CreateBarrier (TRUE /* forward */, size);
    WN* after =  WN_CreateBarrier (FALSE /* backward */, size);

    for (UINT i = 0; i < size; ++i) {
	ST* st = actual_list[i].first;
	WN* lvalue = actual_list[i].second ?
	    WN_Lda (Pointer_Mtype, 0, st) :
	    WN_Ldid (Pointer_Mtype, 0, ST_st_idx (st), ST_type (st));

	WN_kid (before, i) = lvalue;
	WN_kid (after, i) = IPO_Copy_Tree (lvalue);
    }

    Insert_Block_Around (parent, call, before, after);

    // create barriers for the formal parameters
    
    before = WN_CreateBarrier (FALSE /* backward */, num_barrier_formal);
    after = WN_CreateBarrier (TRUE /* forward */, num_barrier_formal);

    UINT j = 0;
    for ( p = parm_attr.begin (); p != parm_attr.end (); ++p) {
	if (p->Pass_Method () != PM_NEED_BARRIER)
	    continue;

	Is_True (TY_kind (ST_type (p->Formal_St ())) == KIND_POINTER,
		 ("Invalid formal symbol for barriers"));
	Is_True (p->Replace_St () != 0, ("Missing barrier symbols"));
	WN* lvalue = WN_Ldid (Pointer_Mtype, 0, p->Replace_St (),
			      ST_type (p->Formal_St ()));
	WN_kid (before, j) = lvalue;
	WN_kid (after, j) = IPO_Copy_Tree (lvalue);
	++j;
    }

    Insert_Block_Around (parent, call, before, after);
    
} // Generate_Barriers


//-----------------------------------------------------------------------
// the formal and actual num dims match. The actual mapping operation is
//  a(i,j,k)  = a(i + i',j + j', k + k')
//-----------------------------------------------------------------------
static void
Array_reshape_eq (WN* actual, TREE_ITER& iter)
{
    WN* wn = iter.Wn ();

    Is_True (WN_operator (wn) == OPR_ARRAY,
	     ("invalid opcode in Reshape_Array"));

    if (WN_operator (actual) != OPR_ARRAY ||
	WN_num_dim (wn) != WN_num_dim (actual)) {
	WN_array_base (wn) = WN_Type_Conversion (IPO_Copy_Tree (actual),
						 WN_rtype (wn)); 
	iter.Skip ();
	return;
    }

    WN* array_node = IPO_Copy_Tree (actual);

    INT dim = WN_num_dim (array_node);

    for (UINT i = 0; i < dim; ++i) {
	WN* index_actual = WN_array_index (array_node, i);
	WN* index_formal = WN_array_index (wn, i);
	WN_array_index (array_node, i) =
	    WN_CreateExp2 (OPR_ADD, WN_rtype (index_formal), MTYPE_V,
			   index_actual, index_formal);
    }

    iter.Replace (array_node);
    iter.Skip ();
} // Array_reshape_eq


//-----------------------------------------------------------------------
// formal array parameter has more dimensions than the actual parameter
//-----------------------------------------------------------------------
static void
Array_reshape_gt (WN* actual, TREE_ITER& iter)
{
    WN* wn = iter.Wn ();

    Is_True (WN_operator (wn) == OPR_ARRAY,
	     ("invalid opcode in Reshape_Array"));

    WN* array_node = Create_array (actual);

    INT actual_dim = WN_num_dim (array_node);
    INT delta = WN_num_dim (wn) - actual_dim ;

    if (delta <=0) {
	// something is inconsistent
	WN_array_base (wn) = WN_Type_Conversion (array_node, WN_rtype (wn));
	iter.Skip ();
	return;
    }

    for (UINT i = 0; i < actual_dim; ++i) {
	WN* index_formal = WN_array_index (wn, i + delta);
	WN* index_actual = WN_array_index (array_node, i);
	WN_array_index (array_node, i) =
	    WN_CreateExp2 (OPR_ADD, WN_rtype (index_formal), MTYPE_V,
			   index_actual, index_formal);
    }

    iter.Replace (array_node);
    iter.Skip ();
} // Array_reshape_gt


//-----------------------------------------------------------------------
// reshape if formal array is smaller than the actual array
//-----------------------------------------------------------------------
static void
Array_reshape_lt (WN* actual, TREE_ITER& iter)
{
    WN* wn = iter.Wn ();

    Is_True (WN_operator (wn) == OPR_ARRAY,
	     ("invalid opcode in Reshape_Array"));

    WN* array_node = Create_array (actual);

    INT formal_dim = WN_num_dim (wn);
    INT delta = WN_num_dim (array_node) - formal_dim;

    if (delta <= 0) {
	// something is inconsistent
	WN_array_base (wn) = WN_Type_Conversion (array_node, WN_rtype (wn));
	iter.Skip ();
	return;
    }

    for (UINT i = 0; i < formal_dim; ++i) {
	WN* index_formal = WN_array_index (wn, i);
	WN* index_actual = WN_array_index (array_node, i + delta);
	WN_array_index (array_node, i + delta) =
	    WN_CreateExp2 (OPR_ADD, WN_rtype (index_formal), MTYPE_V,
			   index_actual, index_formal);
    }

    iter.Replace (array_node);
    iter.Skip ();
} // Array_reshape_lt


// fix up the array subscripts
void
IPO_INLINE::Reshape_Array (TREE_ITER& iter, PARAMETER_ATTRIBUTES& parm,
			   IPO_INLINE_AUX& aux)
{
    if (ST* actual_st = parm.Addr_Passed_Actual ()) {
	if (! ST_addr_saved (actual_st)) {
	    // check if the formal is saved, if so, set the addr_saved bit
	    // of the actual
	    TREE_ITER i = iter;
	    while (WN* wn = i.Wn ()) {
		OPERATOR opr = WN_operator (wn);
		if (opr == OPR_STID) {
		    Set_ST_addr_saved (actual_st);
		    parm.Set_addr_passed_actual (NULL);
		    break;
		}
		if (OPERATOR_is_stmt (opr) ||
		    opr == OPR_ILOAD || opr == OPR_MLOAD)
		    break;
		if (i.Get_parent_wn () == NULL)
		    break;
		i.Pop ();
	    }
	}
    }

    WN* parent = iter.Get_parent_wn ();
    if (WN_operator (parent) != OPR_ARRAY) {
	// referencing only the array base (e.g., in passing the entire
	// array as parameter). We do a direct substitution
	WN* new_wn = WN_Type_Conversion (IPO_Copy_Tree (parm.Actual_Wn ()),
					 WN_rtype (iter.Wn()));
	iter.Replace (new_wn);
	iter.Skip ();
	return;
    }

    // make sure this is a proper array node.
    Is_True (WN_array_base (parent) == iter.Wn (), ("Invalid array node"));

    // need to fixup the subscript nodes first, in case they reference other
    // formal parameters
    {
	TREE_ITER p (parent);
	while (p.Wn () != NULL && p.Wn () != WN_kid1(parent))
	    ++p;			// skip pass the array base
	while (p.Wn () != NULL)
	    Process_Op_Code (p, aux);
    }

    iter.Pop ();			// iterator now points to ARRAY node

    // fixup array subscript
    switch (parm.Reshape_Method ()) {
    case RS_EQ:
	Array_reshape_eq (parm.Actual_Wn (), iter);
	break;
    case RS_GT:
	Array_reshape_gt (parm.Actual_Wn (), iter);
	break;
    case RS_LT:
	Array_reshape_lt (parm.Actual_Wn (), iter);
	break;
    default:
	Fail_FmtAssertion ("Incorrect array reshape analysis");
	break;
    }
}


#ifdef Is_True_On
// create assert statement to verify if "formal" has value equals to
// "const_wn" 
static inline WN*
Create_Assert (ST* formal, WN* const_wn, TYPE_ID desc)
{
    WN* load = WN_Ldid (desc, 0, formal, ST_type (formal));
    return WN_CreateAssert (0, WN_EQ (Mtype_comparison (desc), load,
				      WN_COPY_Tree (const_wn))); 
	
} // Create_Assert
#endif // Is_True_On

void
IPO_INLINE::Process_Formal_ST (TREE_ITER& iter, ST* cp, IPO_INLINE_AUX& aux)
{ 
#ifdef KEY
    WN *wn = iter.Wn ();
    PARM_ITER p = Lookup_Parm (aux.parm_attr, cp);
    if (p == aux.parm_attr.end())
    { // There is no match for this parameter, so this must be a case where
      // # of parameters at the callsite < # of formals, and we are looking
      // for the parameter missing at the callsite.
	ST_IDX idx = 0;
	for (replace_st_vec::iterator i=aux.replace_st.begin(); 
	     i!=aux.replace_st.end(); ++i)
	    if ((*i).formal == cp)
	    {
	    	idx = (*i).replace;
	    	break;
	    }
	Is_True (idx, ("No ST to replace formal"));
	WN_st_idx (wn) = idx;
    	iter++;
	return;
    }
    PARAMETER_ATTRIBUTES& parm = *p;
#else
    PARAMETER_ATTRIBUTES& parm = *(Lookup_Parm (aux.parm_attr, cp));
    WN *wn = iter.Wn ();
#endif

    switch (parm.Fixup_Method ()) {
	WN* new_wn;
	WN* actual;

    case FM_REPLACE_ST:
	Is_True (parm.Replace_St () != 0,
		 ("Missing symbol for formal parameter replacement"));
	WN_st_idx (wn) = parm.Replace_St ();
	WN_offset (wn) += parm.Formal_Preg ();
	if (WN_offset (wn) == 0 && WN_operator (wn) == OPR_LDID) {
	    // check for LDID of a CONST_ST
	    ST* st = WN_st (wn);
	    if (ST_class (st) == CLASS_CONST) {
		if (MTYPE_is_integral (TY_mtype (ST_type (st)))) {
		    TYPE_ID mtype = TY_mtype (ST_type (st));
		    TCON& tcon = Tcon_Table[ST_tcon (st)];
		    INT64 value = Targ_To_Host (tcon);
		    WN* const_wn = WN_Intconst (Mtype_comparison (mtype), value);
		    iter.Replace (WN_Int_Type_Conversion(const_wn, WN_rtype(wn)));
		} else if (MTYPE_is_float (TY_mtype (ST_type (st)))) {
		    TCON& tcon = Tcon_Table[ST_tcon (st)];
		    iter.Replace (WN_CreateConst (OPR_CONST, TCON_ty (tcon),
						  MTYPE_V, ST_st_idx (st)));
		}
	    }
	}
	break;

    case FM_REPLACE_ACTUAL:
	if (WN_offset (wn) == 0 && WN_operator (wn) == OPR_LDID) {
	    new_wn = WN_Type_Conversion (IPO_Copy_Tree (parm.Actual_Wn ()),
				     WN_rtype (wn));
	    iter.Replace (new_wn);
	    iter.Skip ();
	}
	else { // Anything else is invalid, just delete the code
	    iter.Delete ();
#ifdef Is_True_On
	    Create_Assert(WN_st(wn), parm.Actual_Wn (), TY_mtype (ST_type (WN_st(wn))));
#endif // Is_True_On
	}
	return;
	
    case FM_REPLACE_ARRAY:
        // if we have other than a LDID, we need to lower instead
        if (WN_operator (wn) == OPR_LDID) {
	  Reshape_Array (iter, parm, aux);
	  return;
        }

    case FM_LOWER_FORMAL_REF:
	// lower the formal_ref, i.e.:
	// ldid f --> iload (actual parm)
	// lda f  --> actual parm
	// stid f --> istore (actual parm)

	actual = IPO_Copy_Tree (parm.Actual_Wn ());

	switch (WN_operator (wn)) {
	case OPR_LDID:
	    new_wn = WN_CreateIload (OPR_ILOAD, WN_rtype (wn),
				     WN_desc (wn), WN_load_offset (wn),
				     WN_ty (wn),
				     Make_Pointer_Type (WN_ty (wn)),
				     actual);
	    break;
	    
	case OPR_LDA:
	    new_wn = actual;
	    if (WN_offset (wn) != 0) {
		WN* offset = WN_Intconst (MTYPE_U4, WN_offset (wn));
		new_wn = WN_CreateExp2 (OPR_ADD, WN_rtype (wn), MTYPE_V,
					new_wn, offset);
	    }
	    break;
	    
	case OPR_STID:
	    {
		// we need to fix up the rhs of the store
		TREE_ITER i (wn);
		++i;			// skip to the rhs of the STID
		while (i.Wn () != NULL)
		    Process_Op_Code (i, aux);
	    }
	    new_wn = WN_Istore (WN_desc (wn), WN_store_offset (wn),
				Make_Pointer_Type (WN_ty (wn)), actual,
				WN_kid0 (wn)); 
	    break;
	    
	default:
	    Fail_FmtAssertion ("Unexpected references of formal parameter");
	}

	iter.Replace (new_wn);
	iter.Skip ();
	return;
	
    default:
	Fail_FmtAssertion ("uninitialized parameter fixup method");
    }

    ++iter;
} // Process_Formal_ST


// ======================================================================
// Process_ST:
// 	For each WN that has an ST, make sure it is
//	fixed appropriately
// ======================================================================

void
IPO_INLINE::Process_ST (TREE_ITER& iter, IPO_INLINE_AUX& aux)
{
    ST* cp = WN_st (iter.Wn ());
    if (cp == NULL) {
	++iter;
	return;
    }
  

    // update the caller pragma list
    SUMMARY_PROCEDURE* caller_proc = Caller_node()->Summary_Proc ();

    BOOL Is_DoAcross  = caller_proc->Has_parallel_pragma ();
    BOOL Is_MP_Region = caller_proc->Has_parallel_region_pragma();

    switch(ST_sclass(cp)) {
    case SCLASS_PSTATIC:
	Set_ST_sclass(cp, SCLASS_FSTATIC);
    case SCLASS_FSTATIC:
	// if the class is pu static and the files are not the same,
	// promote it to global
	if (ST_class (cp) != CLASS_CONST &&
	    Callee_node ()->File_Index () != Caller_node ()->File_Index ()) {
	    // Need to generate a new name for this
	    static INT Temp_Index = 0;
	    STR_IDX new_name = Save_Str2i(ST_name(cp),"..", Temp_Index++);
	    Set_ST_name_idx (*cp, new_name);
	    if (ST_is_initialized(cp) && !ST_init_value_zero(cp))
		Set_ST_sclass(cp, SCLASS_DGLOBAL);
	    else 
		Set_ST_sclass(cp, SCLASS_UGLOBAL);

	    if (ST_ADDR_SAVED(cp) || ST_ADDR_PASSED(cp)) {
		Set_ST_export(cp, EXPORT_HIDDEN);
	    } else
		Set_ST_export(cp, EXPORT_INTERNAL);

	    if (ST_base_idx (cp) != ST_st_idx (cp)) {
		Set_ST_sclass(ST_ptr(ST_base_idx(cp)), ST_sclass(cp));
		Set_ST_export(ST_ptr(ST_base_idx(cp)), ST_export(cp));
	    }
	}
#if (defined(KEY) && (defined(_STANDALONE_INLINER) || defined(_LIGHTWEIGHT_INLINER)))
        if ((Is_DoAcross)  || (Is_MP_Region)) {
	    Update_Caller_MP_Pragmas(cp, Call_Wn());
 	}
#endif // KEY && _STANDALONE_INLINER
  
        break;
      
    case SCLASS_EXTERN:
    case SCLASS_UGLOBAL:
    case SCLASS_DGLOBAL:
    case SCLASS_COMMON:
#if (defined(_STANDALONE_INLINER) || defined(_LIGHTWEIGHT_INLINER))
        if ((Is_DoAcross)  || (Is_MP_Region)) {
	    Update_Caller_MP_Pragmas(cp, Call_Wn());
 	}
#endif // _STANDALONE_INLINER
	break;
  
    case SCLASS_AUTO:
	// for local stack based variables  simply perform a
	// replacement. The actual entries are created before
	// walk tree routine 
	if (!ST_is_export_local(cp))
	    Fail_FmtAssertion("unsupported ST_export %d and class %d \n",
			      ST_sclass(cp), ST_export(cp));  

	FmtAssert(cp != NULL, ("LOCAL must have a cloned version\n"));
	Clear_ST_is_return_var(cp);
      
        if ((Is_DoAcross)  || (Is_MP_Region)) {
	    if (aux.Local_ST_Not_Processed(cp)) {
	        Update_Caller_MP_Pragmas(cp, Call_Wn());
	    }
	} 
	break;
  
    case SCLASS_TEXT:
#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))
	FmtAssert(cp != NULL, ("LOCAL must have a cloned version\n"));
#endif
	break;
      
    case SCLASS_FORMAL:
    case SCLASS_FORMAL_REF:
	if (ST_is_not_used (cp)) {
	    // all formal ST's of the callee should have been marked
	    // NOT_USED.  Any ST marked formal but not NOT_USED must be
	    // up-level references to formal parameter of the callee (or
	    // parent PU) and we should skip them.
	    Process_Formal_ST (iter, cp, aux);
	    return;
	}
	break;
      
    case SCLASS_REG:
	Process_Pregs (iter.Wn (), aux.rp);
	break;

    default: // if the SCLASS is unknown
	if (ST_class(cp) != CLASS_NAME) {
	  Fail_FmtAssertion("SCLASS Type %s: unsupported \n",
			    Sclass_Name(ST_sclass(cp)));
	  Print_ST ( TFile,cp,0);
	}
	break;
    }

    ++iter;
} // Process_ST


// ======================================================================
// Simplify Tree 
// ======================================================================

static WN *
Simplify_Tree(WN *block)
{
    // caller size optimization
    WN_SimpParentMap = Parent_Map;
    WN* tmp_callee = WN_Simplify_Tree(block);
    WN_SimpParentMap = (WN_MAP)0;
    return tmp_callee;
}


pair<ST*, WN_OFFSET>
IPO_INLINE::Create_Copy_In_Symbol (ST* formal_st)
{
    ST* copy_st = NULL;
    WN_OFFSET wn_offset = 0;

    copy_st = Symtab()->IPO_Copy_ST (formal_st, Caller_level());
    Set_ST_base_idx (copy_st, ST_st_idx(copy_st));
    Set_ST_ofst (copy_st, 0);
    Set_ST_sclass (copy_st, SCLASS_AUTO);
    Clear_ST_is_value_parm (copy_st);
    Clear_ST_is_not_used (copy_st);
    Clear_ST_is_optional_argument (copy_st);
    Set_ST_is_temp_var (copy_st);
#ifdef KEY // bug 6674
    Clear_TY_is_restrict (copy_st->u2.type);
#endif

    // update the caller pragma list
    SUMMARY_PROCEDURE* caller_proc = Caller_node()->Summary_Proc ();

    BOOL Is_DoAcross  = caller_proc->Has_parallel_pragma ();
    BOOL Is_MP_Region = caller_proc->Has_parallel_region_pragma();

    if ((Is_DoAcross)  || (Is_MP_Region)) {
	Update_Caller_MP_Pragmas(copy_st, Call_Wn());
    } 

    return std::make_pair (copy_st, wn_offset);
} 

// create a copy statement
static inline WN*
Copy_Scalar (ST* dest, WN_OFFSET offset, TYPE_ID desc, WN* src)
{
    return WN_Stid (desc, offset, dest, ST_type (dest),
		    WN_Type_Conversion (src, desc));
} 

// create a copy statement for struct
static WN*
Copy_Struct (ST* dest, WN* src, UINT64 size)
{
    if (WHIRL_Mldid_Mstid_On)
#ifdef KEY // bug 7760: src may not be M type due to fix to bug 7741.
	return WN_Stid (WN_rtype(src), 0, dest, ST_type (dest), src);  
#else
	return WN_Stid (MTYPE_M, 0, dest, ST_type (dest), src);  
#endif
    else
	return WN_CreateMstore (0, Make_Pointer_Type (ST_type (dest)),
				src, WN_Lda (Pointer_Mtype, 0, dest),
				WN_Intconst (MTYPE_U4, size));
}

// generate copy statement for value parameters
void
IPO_INLINE::Process_Copy_In (PARM_ITER parm, WN* copy_in_block)
{
    Is_True (Scope_tab == Caller_Scope (), ("expecting caller scope"));
    Is_True (parm->Pass_Method () == PM_COPY_IN,
	     ("Incorrect parameter passing method"));

    if (parm->Fixup_Method () == FM_REPLACE_ST) {

	WN* actual = IPO_Copy_Tree (parm->Actual_Wn ());
	ST* formal_st = parm->Formal_St ();
	const TY& formal_ty = Ty_Table[ST_type (formal_st)];
	TYPE_ID mtype = TY_mtype (formal_ty);
	
	ST* copy_st;
	WN_OFFSET wn_offset;
	
	pair<ST*, WN_OFFSET> sym = Create_Copy_In_Symbol (formal_st); 
	copy_st = sym.first;
	wn_offset = sym.second;
    
	WN* copy_wn;
    
	if (mtype != MTYPE_M) {

	    if (ST* st = Get_actual_st_if_passed (actual)) {
		Set_ST_addr_saved (st);
	    }
#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))
	    // since we generate an assignment statement, the actual now
	    // becomes directly referenced.
	    Update_formal_dref (actual, Caller_node ());
#endif
	    copy_wn = Copy_Scalar (copy_st, wn_offset, mtype, actual);
	} else {
	    // copy a struct
	    copy_wn = Copy_Struct (copy_st, actual, TY_size (formal_ty));
	}

	LWN_Insert_Block_Before (copy_in_block, NULL, copy_wn);


	parm->Set_formal_preg (wn_offset);
	parm->Set_replace_st (ST_st_idx (copy_st));
#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))
        if (Alias_Nystrom_Analyzer) {
          ConstraintGraph::updateCloneStIdxMap(ST_st_idx(formal_st),
                                               ST_st_idx(copy_st));
          IPA_NystromAliasAnalyzer::aliasAnalyzer()->processInlineFormal(
              Caller_node(), Callee_node(), actual, copy_st);
        }
#endif

    } else {
	
        WN* actual = parm->Actual_Wn ();
        ST* formal_st = parm->Formal_St ();
        pair<WN*, ST*> copy_stmt;

        if (! ST_is_value_parm (formal_st) &&
                (WN_operator(actual) == OPR_INTCONST && ST_is_optional_argument
(formal_st))) {
            copy_stmt = Create_Copy_Expr_For_Ptr (actual, Call_Wn(),
                Caller_node()->Summary_Proc ()->Has_parallel_pragma () ||
                Caller_node()->Summary_Proc ()->Has_parallel_region_pragma());

            LWN_Insert_Block_Before (copy_in_block, NULL, copy_stmt.first);

            parm->Set_actual (WN_Ldid (Pointer_type, 0,
                                   ST_st_idx (copy_stmt.second),
                                   ST_type (copy_stmt.second)));
        }
        else {
            copy_stmt = Create_Copy_Expr (actual, Call_Wn(),
                Caller_node()->Summary_Proc ()->Has_parallel_pragma () ||
                Caller_node()->Summary_Proc ()->Has_parallel_region_pragma());


            LWN_Insert_Block_Before (copy_in_block, NULL, copy_stmt.first);

            parm->Set_actual (WN_Ldid (WN_rtype (actual), 0,
                                   ST_st_idx (copy_stmt.second),
                                   ST_type (copy_stmt.second)));
        }

    }

} // Process_Copy_In


// directly replace the formal ST with the actual
static void
Process_Direct_Replacement (PARM_ITER parm)
{
    Is_True (parm->Pass_Method () == PM_REPLACE,
	     ("Incorrect parameter passing method"));

    if (parm->Fixup_Method () == FM_REPLACE_ST) {
	WN* wn = parm->Actual_Wn ();
	Is_True (OPERATOR_has_sym (WN_operator (wn)),
		 ("expecting a WHIRL node with ST"));
	parm->Set_replace_st (WN_st_idx (wn));
	parm->Set_formal_preg (WN_offset (wn));
	ST* formal_st = parm->Formal_St ();
	if (ST_addr_saved(formal_st))
	    Set_ST_addr_saved (WN_st(wn));
    }

} // Process_Direct_Replacement


// generate copy-in/copy-out statements
// copy in:
//	    actual
//	  iload
//	stid preg/tmp
//
// copy out:
//	  ldid preg/tmp
//	istore actual
void
IPO_INLINE::Process_Copy_In_Copy_Out (PARM_ITER p, IPO_INLINE_AUX& aux)
{
    Is_True (Scope_tab == Caller_Scope (), ("expecting caller scope"));
    Is_True (p->Pass_Method () == PM_COPY_IN_OUT,
	     ("Incorrect parameter passing method"));


    WN* actual = IPO_Copy_Tree (p->Actual_Wn ());
    ST* formal_st = p->Formal_St ();
    const TY& formal_ty = Ty_Table[ST_type (formal_st)];
    TYPE_ID mtype = TY_mtype (formal_ty);

    Is_True (mtype != MTYPE_M, ("copy in/copy out parameter is MTYPE_M"));
    Is_True (p->Fixup_Method () != FM_LOWER_FORMAL_REF,
	     ("Invalid fixup method for copy in/copy out parameters"));
    
    pair<ST*, WN_OFFSET> sym = Create_Copy_In_Symbol (formal_st); 
    ST* copy_st = sym.first;
    WN_OFFSET wn_offset = sym.second;
    
    WN* wn_iload = WN_CreateIload (OPR_ILOAD, mtype, mtype, 0,
				   ST_type (formal_st),
				   Make_Pointer_Type (ST_type (formal_st)),
				   actual);

    WN* copy_in = WN_Stid (mtype, wn_offset, copy_st, ST_type (copy_st),
			   WN_Type_Conversion (wn_iload, mtype));

    LWN_Insert_Block_Before (aux.copy_in_block, NULL, copy_in);

    // now, handle the copy out part

    // need another copy for actual parameter
    actual = IPO_Copy_Tree (p->Actual_Wn ());

    WN* load_wn = WN_Ldid (mtype, wn_offset, ST_st_idx (copy_st),
			   ST_type (copy_st));
    WN* copy_wn = WN_Istore (mtype, 0, Make_Pointer_Type (ST_type (copy_st)),
			     actual, load_wn);

    if (aux.copy_out_block == NULL)
	aux.copy_out_block = WN_CreateBlock ();

    LWN_Insert_Block_After (aux.copy_out_block, NULL, copy_wn);

    p->Set_formal_preg (wn_offset);
    p->Set_replace_st (ST_st_idx (copy_st));

#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))
    if (Alias_Nystrom_Analyzer){
      ConstraintGraph::updateCloneStIdxMap(ST_st_idx(formal_st),
                                           ST_st_idx(copy_st));
      IPA_NystromAliasAnalyzer::aliasAnalyzer()->processInlineFormal(
            Caller_node(), Callee_node(), wn_iload, copy_st);
    }
#endif
    
    
} // Process_Copy_In_Copy_Out



// copy the array base to a unique pointer
// no need to set addr_saved because this is guarded by barriers
void
IPO_INLINE::Process_Barriers (PARM_ITER parm, WN* copy_in_block)
{
    Is_True (Scope_tab == Caller_Scope (), ("expecting caller scope"));
    Is_True (parm->Pass_Method () == PM_NEED_BARRIER,
	     ("Incorrect parameter passing method"));

    ST* formal_st = parm->Formal_St ();
    TY& formal_ty = Ty_Table[ST_type (formal_st)];

    Is_True (TY_kind (formal_ty) == KIND_POINTER,
	     ("incorrect type for array argument"));

    ST* copy_st = Symtab()->IPO_Copy_ST (formal_st, Caller_level());
    Set_ST_base_idx (copy_st, ST_st_idx(copy_st));
    Set_ST_ofst (copy_st, 0);
    Set_ST_sclass (copy_st, SCLASS_AUTO);
    Clear_ST_is_value_parm (copy_st);
    Clear_ST_is_not_used (copy_st);
    Set_ST_is_temp_var (copy_st);
    if (! parm->Restrict_Pointer ())
	Set_ST_pt_to_unique_mem (copy_st);

    // update the caller pragma list
    SUMMARY_PROCEDURE* caller_proc = Caller_node()->Summary_Proc ();

    BOOL Is_DoAcross  = caller_proc->Has_parallel_pragma ();
    BOOL Is_MP_Region = caller_proc->Has_parallel_region_pragma();

    if ((Is_DoAcross)  || (Is_MP_Region)) {
	Update_Caller_MP_Pragmas(copy_st, Call_Wn());
    } 

    WN* copy_wn = Copy_Scalar (copy_st, 0, TY_mtype (formal_ty),
			       IPO_Copy_Tree (parm->Actual_Wn ()));

    LWN_Insert_Block_Before (copy_in_block, NULL, copy_wn);

    parm->Set_replace_st (ST_st_idx (copy_st));
    Set_PU_smart_addr_analysis (*Current_pu);

#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))
    if (Alias_Nystrom_Analyzer)
      ConstraintGraph::updateCloneStIdxMap(ST_st_idx(formal_st),
                                           ST_st_idx(copy_st));
#endif
}


// check if the array base is constant
static inline BOOL
constant_array_base (WN* base)
{
    OPERATOR opr = WN_operator (base);
    if (opr == OPR_LDA)
	return TRUE;

    if (opr == OPR_LDID) {
	const ST* st = WN_st (base);
	return (ST_sclass (st) == SCLASS_FORMAL && !ST_is_value_parm (st));
    }

    return FALSE;
}


// if the actual parm might be modified by the callee, we make a copy of it 
// first and replace the formal by the copy instead.
static void
Copy_Non_Constant_Parm (WN* parm, WN* copy_in_block, PARAMETER_ATTRIBUTES& p, IPO_INLINE& inliner)
{
    switch (WN_operator (parm)) {
    case OPR_INTCONST:
    case OPR_LDA:
	return;

    case OPR_ILOAD:
	if (WN_operator (WN_kid0 (parm)) != OPR_LDA)
	    break;
	if (ST_might_be_modified (WN_st (WN_kid0 (parm))))
	    break;
	else
	    return;

    case OPR_LDID:
	if (ST_might_be_modified (WN_st (parm)))
	    break;
	else
	    return;

    default:
	break;
    }

    // not a simple constant expression, so generate an assignment
    // statement 
    
    pair<WN*, ST*> copy_stmt = Create_Copy_Expr (parm, inliner.Call_Wn(),
		inliner.Caller_node()->Summary_Proc ()->Has_parallel_pragma () ||
		inliner.Caller_node()->Summary_Proc ()->Has_parallel_region_pragma());

    
    LWN_Insert_Block_Before (copy_in_block, NULL, copy_stmt.first);
    
    p.Set_actual (WN_Ldid (WN_rtype (parm), 0, ST_st_idx (copy_stmt.second),
			   ST_type (copy_stmt.second)));
	    
} // Copy_Non_Constant_Parm


static void
Compute_Pass_Method (PARM_ITER p, IPO_INLINE_AUX& aux, IPO_INLINE& inliner)
{
    PARM_ATTR_VEC& parm_attr = aux.parm_attr;
    PARAMETER_ATTRIBUTES& parm = *p;
    WN* wn = parm.Actual_Wn ();
    OPERATOR opr = WN_operator (wn);
    ST* formal_st = parm.Formal_St ();

    if (!ST_is_value_parm (formal_st) && Formal_Is_Array (formal_st)) {
	RESHAPE_METHOD r = Do_Linearize(wn, formal_st);
	parm.Set_pass_method (PM_REPLACE);
	parm.Set_fixup_method (FM_REPLACE_ARRAY);
	parm.Set_reshape_method (r);
	if (ST* actual_st = Get_actual_st_if_passed (wn)) {
	    if (ST_addr_passed (actual_st) && !ST_addr_saved (actual_st))
		parm.Set_addr_passed_actual (actual_st);
	}
	switch (r) {
	case RS_EQ:
	case RS_LT:
	case RS_GT:
	    // direct substitution of the array and fix up the subscripts
	    return;
	    
	case RS_LINEARIZE:
	case RS_EQ_COMMON:
	default:
	    // "messy" array form, don't attempt any array-specific
	    // transformation 
	    break;
	}
    }

    if (! ST_is_value_parm (formal_st)) {
	if (opr == OPR_INTCONST && ST_is_optional_argument (formal_st)) {
	    // optional arguement that is not present should not directly
	    // replace the formal st, otherwise we might create code like
	    // ILOAD (INTCONST 0) 
	    parm.Set_pass_method (PM_COPY_IN);
	    if (ST_sclass (formal_st) == SCLASS_FORMAL_REF)
		parm.Set_fixup_method (FM_LOWER_FORMAL_REF);
	    else
		parm.Set_fixup_method (FM_REPLACE_ST);
	} else if (ST_sclass (formal_st) == SCLASS_FORMAL_REF) {
	    parm.Set_pass_method (PM_REPLACE);
	    if (opr == OPR_LDA)
		parm.Set_fixup_method (FM_REPLACE_ST);
	    else { 
		parm.Set_fixup_method (FM_LOWER_FORMAL_REF);
		if (opr != OPR_ARRAY)
		    // ARRAY nodes have already been "cleaned"
		    Copy_Non_Constant_Parm (wn, aux.copy_in_block, parm, inliner);
	    }
	} else {
	    // a reference parameter, and we already know that the actual and
	    // formal are not both array
	    parm.Set_pass_method (PM_REPLACE);
	    parm.Set_fixup_method (FM_REPLACE_ACTUAL);
	}
    } else {
	    
	if (INLINE_Enable_Restrict_Pointers) {
	    if (TY_is_restrict (ST_type (formal_st)) &&
		TY_kind (ST_type (formal_st)) == KIND_POINTER) {
		parm.Set_actual_aliased (Get_actual_st_if_passed (wn));
		parm.Set_pass_method (PM_NEED_BARRIER);
		parm.Set_fixup_method (FM_REPLACE_ST);
		parm.Set_restrict_pointer ();
		return;
	    }
	}
	
	// we treat array expression as leaf node, given that all non-constant
	// subscripts have been copied to temp.
	if (! OPERATOR_is_leaf (opr) && opr != OPR_ARRAY) {
	    parm.Set_pass_method (PM_COPY_IN);
	    parm.Set_fixup_method (FM_REPLACE_ST);
	} else if (inliner.SubstituteFormal (formal_st, wn,
					     p - parm_attr.begin ())) { 

	    // should cleanup SubstitueFormal and make it not a member
	    // function of IPO_INLINE.  For the time being, ... well ...
	    parm.Set_pass_method (PM_REPLACE);
	    parm.Set_fixup_method (FM_REPLACE_ACTUAL);
	} else {
	    parm.Set_pass_method (PM_COPY_IN);
	    parm.Set_fixup_method (FM_REPLACE_ST);
	}
    }
} // Compute_Pass_Method


static ST*
get_formal(SCOPE* callee_scope, WN *formal)
{
    SCOPE_CONTEXT switch_scope(callee_scope);
    ST* formal_st = WN_st(formal);
    return (formal_st);
}


// ======================================================================
// Process Formal parameters in the callee
// ======================================================================
void
IPO_INLINE::Process_Formals (IPO_INLINE_AUX& aux)
{
    PARM_ATTR_VEC& parm_attr = aux.parm_attr;
    Is_True (parm_attr.size () == 0,
	     ("uninitialized parameter attributes list"));

    WN* callee = Callee_Wn ();
    WN* call = Call_Wn ();
    BOOL has_array = FALSE;

    // initialize the vector holding all the aux. data structures for each
    // parameter 
#ifdef KEY
    INT num_params = Call_edge()->Num_Actuals() < WN_num_formals(callee) ?
    		Call_edge()->Num_Actuals() : WN_num_formals(callee);

    for (INT j=0; j < num_params; ++j)
#else
    for (INT j=0; j < WN_num_formals(callee); ++j)
#endif
    {
        ST* ste = get_formal(Callee_Scope(), WN_formal(callee,j));

#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))
        if (Alias_Nystrom_Analyzer)
          ConstraintGraph::updateOrigToCloneStIdxMap(
                              ST_st_idx(ste), ST_st_idx(Symtab()->Get_ST(ste)));
#endif

	ste = Symtab()->Get_ST(ste);	// Get the ST version in the caller side
	Is_True (ST_sclass (ste) == SCLASS_FORMAL ||
		 ST_sclass (ste) == SCLASS_FORMAL_REF,
		 ("ProcessFormals:sclass %s not supported \n", 
		  Sclass_Name(ST_sclass(ste))));

	WN* actual = WN_kid(call, j);
	if (WN_operator(actual) == OPR_PARM) 
	    actual = WN_kid0(actual);
	if (WN_operator(actual) == OPR_ARRAY)
	    has_array = TRUE;
	parm_attr.push_back (PARAMETER_ATTRIBUTES (actual, ste)); 
    }

#ifdef KEY
    // For formals that are missing from the list of actuals, store the
    // dummy ST with which to replace it.
    for (; num_params < WN_num_formals(callee); ++num_params)
    {
        ST* ste = get_formal(Callee_Scope(), WN_formal(callee, num_params));
	ste = Symtab()->Get_ST(ste);	// Get the ST version in the caller side
	pair<ST*, WN_OFFSET> sym = Create_Copy_In_Symbol (ste); 
	Is_True (!sym.second, ("Non-zero offset not handled yet"));

	formal_to_replace_st param (ste, ST_st_idx (sym.first));
	aux.replace_st.push_back (param);
    }
#endif

    aux.copy_in_block = WN_CreateBlock(); // Empty block into which assignments
					  // to formals will go

    if (has_array) {
	if (IPA_Enable_BarrierFarg) {
	    PU& pu = Pu_Table[ST_pu (Callee_node ()->Func_ST ())];
	    // check if we can/should apply fortran's no aliased-parameter
	    // rules. 
	    if ((PU_f77_lang (pu) || PU_f90_lang (pu)) &&
		!PU_args_aliased (pu) &&
		Unaliased_Actuals_Useful (aux.inlined_body))
		Disambiguate_Aliased_Actuals (parm_attr, pu);
	}

	// check for non-constant array expression
	Copy_Subscript_Expressions (aux, *this);
	
    }
	    

    // create code to copy/map the actuals to formals
    for (PARM_ITER p (parm_attr.begin()); p != parm_attr.end(); ++p) {

	if ((*p).Pass_Method () == PM_UNKNOWN)
	    Compute_Pass_Method (p, aux, *this);
	    
	switch ((*p).Pass_Method ()) {
	    
	case PM_COPY_IN:
	    Process_Copy_In (p, aux.copy_in_block);
	    break;
	    
	case PM_REPLACE:
	    Process_Direct_Replacement (p);
	    break;

	case PM_COPY_IN_OUT:
	    Process_Copy_In_Copy_Out (p, aux);
	    break;

	case PM_NEED_BARRIER:
	    Process_Barriers (p, aux.copy_in_block);
	    break;

	default:
	    Fail_FmtAssertion ("Undetermined parameter passing method");
	    break;
	}
    }

} // Process_Formals


// ======================================================================
// processing in callee
// ======================================================================
void
IPO_INLINE::Walk_and_Update_Callee (IPO_INLINE_AUX& aux)
{
    // Process formals, collect info, process arrays, simply
    Process_Formals (aux);
  
    // PU specific static variable
    // walk the callee, locate such variables, modify the symbol table
    // sclass entry temporarily to be SCLASS_LAST

    TREE_ITER iter (aux.inlined_body);

    while (iter.Wn () != NULL) {
	Process_Op_Code (iter, aux);
    }

    WN_Parentize (aux.inlined_body, Caller_node()->Parent_Map (),
		  Caller_Map_Table ());

    aux.inlined_body = Simplify_Tree(aux.inlined_body);
}

#ifdef KEY
void
IPO_INLINE::Merge_EH_Spec_Tables (void)
{
    INITV_IDX start, blk;
    if (!PU_cxx_lang (Callee_node()->Get_PU()))
      return;

    // callee side
    INITO_IDX tmp = PU_misc_info (Callee_node()->Get_PU());
    if (tmp)
    	start = INITO_val (tmp);
    else return;

    const INITO_IDX callee_spec = TCON_uval (INITV_tc_val (INITV_next (INITV_next (INITV_next (start)))));
    if (!callee_spec)
    	return;		// no callee specification

    blk = INITV_blk (INITO_val (callee_spec));
    mUINT32 callee_spec_size=0;
    vector<ST_IDX> callee_spec_types;
    while (blk)
    {
    	callee_spec_size++;
	if (INITV_kind (blk) == INITVKIND_ZERO)
	    callee_spec_types.push_back (0);
	else
	    callee_spec_types.push_back (TCON_uval (INITV_tc_val (blk)));
	blk = INITV_next (blk);
    }
    if (!Callee_node()->EH_spec_size())
    	Callee_node()->Set_EH_spec_size (callee_spec_size);
#ifdef Is_True_On
    else FmtAssert (Callee_node()->EH_spec_size() == callee_spec_size, ("EH specification size mismatch"));
    FmtAssert (!callee_spec_types.empty(), ("EH Specification merge error"));
#endif

    // caller side, change tables temporarily
    Set_Tables (Caller_node());
    start = INITO_val (PU_misc_info (Pu_Table [ST_pu (Caller_node()->Func_ST())]));
    const INITO_IDX caller_spec = TCON_uval (INITV_tc_val (INITV_next (INITV_next (INITV_next (start)))));
    if (caller_spec)
    	blk = INITV_blk (INITO_val (caller_spec));
    else
    	blk = 0;	// no specification in caller
    mUINT32 caller_spec_size=0;
    INITV_IDX last_ti = INITV_IDX_ZERO;
    while (blk)
    {
    	caller_spec_size++;
	last_ti = blk;
	blk = INITV_next (blk);
    }
    if (!Caller_node()->EH_spec_size())
    	Caller_node()->Set_EH_spec_size (caller_spec_size);
#ifdef Is_True_On
    else FmtAssert (Caller_node()->EH_spec_size() == caller_spec_size, ("EH specification size mismatch"));
#endif

    INITV_IDX first_insert = INITV_IDX_ZERO, prev_insert = INITV_IDX_ZERO;
    for (vector<ST_IDX>::iterator i = callee_spec_types.begin();
    	 i != callee_spec_types.end();
	 i++)
    {
	INITV_IDX insert = New_INITV();
	if (*i == 0)
	  INITV_Set_ZERO (Initv_Table[insert], MTYPE_U4, 1);
	else
	  INITV_Set_VAL (Initv_Table[insert], 
		         Enter_tcon (Host_To_Targ (MTYPE_U4, *i)), 1);
        if (prev_insert)
	    Set_INITV_next (prev_insert, insert);
	else
	    first_insert = insert;
	prev_insert = insert;
    }
    if (!caller_spec_size)
    { // no table in caller
	IPO_SYMTAB* cloned = Callee_node()->Cloned_Symtab();
	// return the already cloned inito and get its ST
	INITO_IDX cloned_spec = cloned->Get_INITO_IDX (callee_spec);
	ST * new_st = Copy_ST (INITO_st (cloned_spec), CURRENT_SYMTAB);
	INITV_IDX begin = New_INITV();
	INITV_Init_Block (begin, first_insert);
	INITO_IDX new_spec = New_INITO (ST_st_idx (new_st), begin);

	// now insert it
	INITV_IDX pos = INITV_next (INITV_next (INITV_next (start)));
	INITV_IDX bkup = INITV_next (pos);
	FmtAssert (!bkup, ("Exception specification info not last entry"));
	INITV_Set_VAL (Initv_Table[pos],
	               Enter_tcon (Host_To_Targ (MTYPE_U4, new_spec)), 1);
    	Set_INITV_next (pos, bkup);
    }
    else
    	Set_INITV_next (last_ti, first_insert);
    Set_Tables (Callee_node());
}

void
IPO_INLINE::Merge_EH_Typeinfo_Tables (void)
{
    vector<ST_IDX> callee_typeinfos, caller_typeinfos;
    INITV_IDX start, blk, last_blk=0;
    if (!PU_cxx_lang (Callee_node()->Get_PU()))
      return;
    // callee side
    INITO_IDX tmp = PU_misc_info (Callee_node()->Get_PU());
    if (tmp)
    	start = INITO_val (tmp);
    else
    	return;
    const INITO_IDX callee_ttable = TCON_uval (INITV_tc_val (INITV_next (INITV_next (start))));
    if (!callee_ttable) // nothing to merge
	return;
    blk = INITO_val (callee_ttable);
    while (blk)
    {
	INITV_IDX ti = INITV_blk (blk);
	if (INITV_kind (ti) == INITVKIND_ZERO)
	  callee_typeinfos.push_back (0);
	else
          callee_typeinfos.push_back (TCON_uval (INITV_tc_val (ti)));
        blk = INITV_next (blk);
    }
    // caller side
    // change tables temporarily
    Set_Tables (Caller_node());
    start = INITO_val (PU_misc_info (Pu_Table [ST_pu (Caller_node()->Func_ST())]));
    INITO_IDX caller_ttable = TCON_uval (INITV_tc_val (INITV_next (INITV_next (start))));
    if (caller_ttable) blk = INITO_val (caller_ttable);
    else blk = 0;
    while (blk)
    {
	INITV_IDX ti = INITV_blk (blk);
	if (INITV_kind (ti) == INITVKIND_ZERO)
	  caller_typeinfos.push_back (0);
	else
          caller_typeinfos.push_back (TCON_uval (INITV_tc_val (ti)));
        last_blk = blk;
        blk = INITV_next (blk);
    }
    sort (caller_typeinfos.begin(), caller_typeinfos.end());
    int last_caller_filter = caller_typeinfos.size();
    if (!last_caller_filter)
    { // special case, there is no typeinfo table in caller
        FmtAssert (!last_blk && !caller_ttable, ("EH Tables processing error in inliner"));
	IPO_SYMTAB* cloned = Callee_node()->Cloned_Symtab();
	// return the already cloned inito and get its st
	INITO_IDX cloned_ttable = cloned->Get_INITO_IDX (callee_ttable);
	ST * new_st = Copy_ST (INITO_st (cloned_ttable), CURRENT_SYMTAB);
	INITV_IDX to_iter = New_INITV();
	INITO_IDX new_ttable = New_INITO (ST_st_idx (new_st), to_iter);
    	INITV_IDX from_iter = INITO_val (cloned_ttable);
    	while (from_iter)
    	{
	    memcpy (&Initv_Table[to_iter], &Initv_Table[from_iter], sizeof(INITV));

	    INITV_IDX from = INITV_blk (from_iter);
	    INITV_IDX to_ti = New_INITV();
	    INITV_Init_Block (to_iter, to_ti);

	    memcpy (&Initv_Table[to_ti], &Initv_Table[from], sizeof(INITV));
	    from = INITV_next (from);
	    INITV_IDX to_filter = New_INITV();
	    memcpy (&Initv_Table[to_filter], &Initv_Table[from], sizeof(INITV));
	    Set_INITV_next (to_ti, to_filter);
	    from_iter = INITV_next (from_iter);
	    if (from_iter)
	    {
	        INITV_IDX tmp = New_INITV();
	        Set_INITV_next (to_iter, tmp);
	        to_iter = tmp;
	    }
        }

	INITV_IDX insert = INITV_next (INITV_next (start));
	INITV_IDX bkup = INITV_next (insert);
	INITV_Set_VAL (Initv_Table[insert],
	               Enter_tcon (Host_To_Targ (MTYPE_U4, new_ttable)), 1);
    	Set_INITV_next (insert, bkup);
    }
    else
    {
        FmtAssert (last_blk, ("EH Tables processing error in inliner"));
        int filter = last_caller_filter+1;
        vector<ST_IDX>::iterator i = callee_typeinfos.begin();
        for (; i!=callee_typeinfos.end(); ++i)
        {
            if (!binary_search (caller_typeinfos.begin(),
                                caller_typeinfos.end(), (*i)))
            { // insert the entry
		INITV_IDX st = New_INITV();
		if (*i == 0)
		  INITV_Set_ZERO (Initv_Table[st], MTYPE_U4, 1);
		else
                  INITV_Set_VAL (Initv_Table[st],
			       Enter_tcon (Host_To_Targ (MTYPE_U4, *i)), 1);
                INITV_IDX f = New_INITV();
                INITV_Set_VAL (Initv_Table[f],
                        Enter_tcon (Host_To_Targ (MTYPE_U4, filter)), 1);
                Set_INITV_next (st, f);

		INITV_IDX next_blk = New_INITV();
                INITV_Init_Block (next_blk, st);
                Set_INITV_next (last_blk, next_blk);
                last_blk = next_blk;
		filter++;
            }
        }
    }
    // change back tables to callee
    Set_Tables (Callee_node());
}
void
IPO_INLINE::Merge_EH_Tables (void)
{
    Merge_EH_Typeinfo_Tables ();
    Merge_EH_Spec_Tables ();
}
#endif

/* Prune the WN tree in the leftover (cloned) callee.
*/
static void
Prune_Callee(WN* wn) 
{
   int    i;
   WN     *wn2 = NULL;
   if (!wn) {
       return;
   }

   OPERATOR opr = WN_operator(wn);

   if( Get_Trace ( TP_IPA, IPA_TRACE_TUNING) ){
      fprintf (Y_inlining, "In Prune_Callee(): encountering opr %d\n", opr);
   }

   switch (opr) {
      case OPR_BLOCK:
         wn2 = WN_first(wn);
         while (wn2) {
            OPERATOR opr2 = WN_operator(wn2);
            if (opr2 == OPR_WHILE_DO) {
               // For the WHILE_DO case, since we may iterate through
               // the WHILE_DO, keep it in the callee.
               return;
            }
            if (opr2 == OPR_IF) {
               // Delete this IF wn.
               if (wn2 == WN_first(wn)) {
                  // The IF is the first wn in the block.
                  WN_first(wn) = WN_next(wn2);
                  WN_prev(WN_next(wn2)) = NULL;
               } else if (wn2 == WN_last(wn)) {               
                  // The IF is the last wn in the block.
                  WN_last(wn) = WN_prev(wn2);
                  WN_next(WN_prev(wn2)) = NULL;
               } else {
                  WN_next(WN_prev(wn2)) = WN_next(wn2);
                  WN_prev(WN_next(wn2)) = WN_prev(wn2);
               }
               IPA_WN_DELETE_Tree(caller_map_tab, wn2);
               return;
            } 
            // TODO: to delete the wn's before the conditional contruct,
            // such as IF and WHILE_DO.
            wn2 = WN_next(wn2);
         }
         return;

      case OPR_FUNC_ENTRY:
      case OPR_PRAGMA:
      case OPR_IDNAME:   
         for (i = 0; i < WN_kid_count(wn); i++) {
            wn2 = WN_kid(wn,i);
            Prune_Callee(wn2);
         }
         return;

      default:
         return;
   }

   return;
}

/* Check if the wn returns an int constant value.
   If so, set the value in *const_val.
 */
static BOOL
PU_Rtn_Const(WN *wn, INT64 *const_val)
{
   WN *wn2;
   BOOL has_seen_rtn_val = FALSE;

   // walk the tree
   for (WN_TREE_ITER<PRE_ORDER, WN*> iter (wn); iter.Wn () != NULL; ++iter) {
      wn2 = iter.Wn ();
      OPERATOR opr = WN_operator(wn2);
      if (opr == OPR_RETURN_VAL) {
         WN *kid = WN_kid0(wn2);
         if (WN_operator(kid) == OPR_INTCONST ) {
            INT64 this_const = WN_const_val(kid);
            if (has_seen_rtn_val) {
               if (*const_val != this_const) {
                  /* Could return different const values. */
                  return FALSE;
               }
            } else {
               *const_val = this_const;
               has_seen_rtn_val = TRUE;
            }
         } else {
            // Return a non int const.
            return FALSE;
         }
      }
   }

   if (has_seen_rtn_val) {
       if( Get_Trace ( TP_IPA, IPA_TRACE_TUNING) ){
          fprintf (Y_inlining, "PU returns a const value %d\n",
                   (INT32) (*const_val));
       }
       return TRUE;
   }

   return FALSE;
}

/* Chcek whether wn may generate a side effect in the context of
   partial inlining for the given wn (no recursive checking). 
   The following cases are treated to have
   a side effect.
   - "Call" in any form
   - "STID" to a global var or formal parameter.
   - All other operators (except for loads): just to be conservative.
 */
static BOOL
Check_Side_Effect(WN* wn) 
{
   if (!wn)   return FALSE;

   OPERATOR opr = WN_operator(wn);
   if (OPERATOR_is_call(opr)) {
      return TRUE;
   }

   // If the store is to a global var or formal parameter, it has a side
   // effect.
   if (opr == OPR_STID) {
      ST_IDX st_idx = WN_st_idx(wn);
      if (Is_Local_Symbol(ST_ptr(st_idx)) &&
          ST_sclass(st_idx) != SCLASS_FORMAL) {
         return FALSE;
      }
   }

   if (OPERATOR_is_load(opr)) {
      return FALSE;
   }

   return TRUE;
}

/* Check if wn contains a return and no side effect. */
static BOOL
Check_Return(WN* wn) 
{
   if (!wn)   return FALSE;

   OPERATOR opr = WN_operator(wn);
   if (opr == OPR_RETURN ||
       opr == OPR_RETURN_VAL) {
      return TRUE;
   }

   if (opr == OPR_BLOCK) {
      WN* kid = WN_first (wn);
      while (kid) {
         OPERATOR kid_opr = WN_operator(kid);
         if (kid_opr == OPR_RETURN ||
             kid_opr == OPR_RETURN_VAL) {
            return TRUE;
         } else if (Check_Side_Effect(kid)) {
            return FALSE;
         }
         kid = WN_next (kid);
      }
   }
   return FALSE;
}

/* Check if the wn is an OPR_BLOCK with an empty block body.
 */
static BOOL
Is_Empty_Block(WN *wn)
{
   if (wn == NULL) {
      return FALSE;
   }

   OPERATOR opr = WN_operator(wn);
   if (opr != OPR_BLOCK) {
      return FALSE;
   }

   if (WN_first(wn) == NULL) {
      return TRUE;
   }
   return FALSE;
}

#define FREQ_RTN_THRESHOLD 	0.9

/*-------------------------------------------------------------*/
/* 
   Check to see if this callee is a candidate for partial     
   inlining.                                                         
   There are different considerations for partial inlining, e.g.
   1. When the callee is too large to inline the whole function,
      consider to inline only part of it.
   2. When certain paths in callee are particularly hot while
      the others are cold, inline only the hot paths.
   3. When callee has certain contructs (e.g. var args) that we 
      cannot inline them, try to inline only part of the function
      without these constraining constructs.
   The current selection of partial inlining candidate is 
   limited, and the select heuristics are to be extended.
   We are currently identifying candidates like
  
   foo() { 
     if (cond)     // This if clause to be partially inlined.
       return; 
     ....    
   }

   => // call site after partial inlining 

   if (cond) {
     goto cont;
   } else {
     foo();
   }
   cont:

   or

   bar(p) {
      while( p != <cond> ) {
      }
   }
 
   => // call site after partial inlining 

   if (p == <cond>) {
     goto cont;
   } else {
     foo();
   }
   cont:

   The current form of partial inlining is called "idempotent
   partial inlining", which peels and inlines the part of the
   callee, that can be re-executed. The function body of the
   callee is not altered in any way. 
      The biggest advantage of idempotent partial inlining is
   its simplicity in implementation. Not having to split the
   callee avoids dealing with many issues, such as splitting
   live ranges of local vars, splitting EH table, updating symbol
   tables, profile annotation, and IPA summary info, etc.
   Although there is a small runtime cost for re-execution in
   case of wrong speculation of the execution paths, the current
   partial inlining tends to peel only a very small part of the
   callee and hence the re-execution cost is expected very
   minimal. Not having to be concerned with splitting local
   live ranges and EH tables could enable more candidates which
   may otherwise be excluded.

      The current identification of partial inlining candidates
   is performed at the IPO phase, which is too late. This is not
   done at the analysis phase is because the WN trees of callees
   are unavailable there. We should move this to the IPL phase
   and propagate information to the analysis to decide partial
   inlining candidates. This will better match the current IPA
   and inlining fraemwork and also solve the problem of inaccurate
   tracking and update function sizes due to partial inlining.
*/
/*-------------------------------------------------------------*/
static BOOL
Identify_Partial_Inline_Candiate(IPA_NODE *callee, WN *wn, BOOL prune_tree, 
                                 IPO_INLINE_AUX *aux)
{
#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))
   // Skip these cases until we better understand how to make partial
   // inlining work with these cases.
   if (!IPA_Enable_Partial_Inline ||
       callee->Is_Clone_Candidate() ||
       callee->Is_Quasi_Clone() ||
       callee->PU_Can_Throw()
      ) {
      return FALSE;
   }

   // If this is a non var-args case but has no profiling feedback
   // or is a small function, don't consider partial inlining.
   if (!callee->Has_Varargs() &&
       (!callee->Has_frequency () ||
        callee->Weight() <= PARTIAL_INL_SIZE)) {
      return FALSE;
   }

   WN *parent_blk = NULL;

   // traverse the WN tree
   for (WN_TREE_ITER<PRE_ORDER, WN*> iter (wn); iter.Wn () != NULL; ++iter) {
      WN *wn2 = iter.Wn ();

      OPERATOR opr = WN_operator(wn2);
      switch (opr) { 
         case OPR_FUNC_ENTRY:  
         case OPR_PRAGMA:
         case OPR_IDNAME:
            continue;
         case OPR_BLOCK:
            if (prune_tree) {
               // Capture the parent BLOCK containing the conditional
               // statment. Because the parent map of the inlined body
               // has not been set up yet, go through this scan to track
               // the containing BLOCK.
               WN* kid = WN_first (wn2);
               while (kid) {
                 OPERATOR kid_opr = WN_operator(kid);
                 if (kid_opr == OPR_IF ||
                     kid_opr == OPR_WHILE_DO) {
                    parent_blk = wn2;
                 }
                 kid = WN_next (kid);
               }
            }
            continue;
         case OPR_IF:
            if (Check_Return(WN_then(wn2)) &&
                Is_Empty_Block(WN_else(wn2))) {
               /* The THEN clause contains a return and the ELSE clause
                  is empty.
                */
               if (prune_tree) {
                  aux->part_inl_leftover_call_site = WN_else(wn2);
               } else {
                  /* Since the full inliner does not consider a var-arg
                     callee, we are more aggressively considering the
                     callee for partial inlining.
                   */
                  if (callee->Has_Varargs()) {
                     return TRUE;
                  } else {
                     // Select this as a partial inlining candidate
                     // only if the return path is very frequent.
                     FB_FREQ then_freq = Cur_PU_Feedback->Query(wn2,
                                       FB_EDGE_BRANCH_TAKEN);
                     FB_FREQ else_freq = Cur_PU_Feedback->Query(wn2,
                                       FB_EDGE_BRANCH_NOT_TAKEN);

                     if ( (then_freq/(then_freq+else_freq) ) > FREQ_RTN_THRESHOLD )
                        return TRUE;
                     else
                        return FALSE;
                  }
               }
            } else if (Check_Return(WN_else(wn2)) && 
                       Is_Empty_Block(WN_then(wn2))) {
               /* The ELSE clause contains a return and the THEN clause
                  is empty.
                */
               if (prune_tree) {
                  aux->part_inl_leftover_call_site = WN_then(wn2);
               } else {
                  if (callee->Has_Varargs()) {
                     return TRUE;
                  } else {
                     // Select this as a partial inlining candidate
                     // only if the return path is very frequent.
                     FB_FREQ then_freq = Cur_PU_Feedback->Query(wn2,
                                       FB_EDGE_BRANCH_TAKEN);
                     FB_FREQ else_freq = Cur_PU_Feedback->Query(wn2,
                                       FB_EDGE_BRANCH_NOT_TAKEN);

                     if ( (else_freq/(then_freq+else_freq) ) > FREQ_RTN_THRESHOLD )
                        return TRUE;
                     else
                        return FALSE;
                  }
               }
            } else {
               return FALSE;
            }
            // Will reach here only when prune_tree is TRUE.
            if (parent_blk) {
               WN* wn_next = WN_next(wn2); 

               // Delete all WN's following the IF in the block.
               WN_next(wn2) = NULL;
               while (wn_next) {
                  /* Save the next kid before deleting the current one. */
                  WN* wn3 = WN_next(wn_next);
                  IPA_WN_DELETE_Tree(caller_map_tab, wn_next); 
                  wn_next = wn3; 
               }
               WN_last(parent_blk) = wn2;
               return TRUE;
            }
            continue; // This is actually a dead code as the execution
                      // should never reach here.
         case OPR_WHILE_DO:
            if (Check_Return(WN_next(wn2))) {
               /* There is a return in the block immediately following
                  the WHILE_DO loop.
                */
               if (prune_tree) {
                  WN* wn_prev = WN_prev(wn2); 
                  // This is a return or a BLOCK containing a return.
                  WN* wn_rtn = WN_next(wn2);

                 /* We want to clone
                    while( p != <cond> ) {
                    }
                    return;

                    and turn that into 

                    if (p != <cond>) {
                    } else {
                      return;
                    }

                    to be partially inlined. 
                  */

                  WN* wn_test = WN_while_test(wn2);
                  // Don't point to wn_test any more.
                  WN_while_test(wn2) = NULL;
                  // Create an IF to replace the WHILE_DO test.
                  WN* wn_if_then = WN_CreateBlock();
                  WN* wn_if_else = WN_CreateBlock();
                  WN* wn_if = WN_CreateIf(WN_COPY_Tree(wn_test), 
                                          wn_if_then, wn_if_else);
                  aux->part_inl_leftover_call_site = wn_if_then;

                  if (wn_prev) {
                     WN_next(wn_prev) = wn_if;
                     WN_prev(wn_if) = wn_prev;
                  }

                  // Move the return to the ELSE clause. 
                  WN_next(wn_if) = WN_next(wn_rtn);
                  if(WN_next(wn_rtn) != NULL) {
                     WN_prev(WN_next(wn_rtn)) = wn_if;
                  }
                  WN_prev(wn_rtn) = NULL;
                  WN_next(wn_rtn) = NULL;
                  LWN_Insert_Block_Before(wn_if_else,  NULL, wn_rtn);

                  // Update the links in the containing BLOCK.
                  if(parent_blk != NULL) { 
                     if (WN_first(parent_blk) == wn_rtn) {
                        WN_first(parent_blk) = wn_if;
                     }
                     if (WN_last(parent_blk) == wn_rtn) {
                        WN_last(parent_blk) = wn_if;
                     }
                  }

                  // Delete the entire while loop body.
                  IPA_WN_DELETE_Tree(caller_map_tab, WN_while_body(wn2));

                  return TRUE;
               } else {
                  if (callee->Has_Varargs()) 
                     return TRUE;

                  // Select this as a partial inlining candidate
                  // only if the loop body is entered very few times
                  // and the return path is very frequent.
                  FB_FREQ iter_freq = Cur_PU_Feedback->Query(wn2,
                                       FB_EDGE_LOOP_ITERATE);
                  FB_FREQ rtn_freq = Cur_PU_Feedback->Query(WN_next(wn2),
                                       FB_EDGE_INCOMING);
                  FB_FREQ do_freq = Cur_PU_Feedback->Query(wn2,
                                       FB_EDGE_LOOP_ZERO) + iter_freq;

                  if ((iter_freq / do_freq) < (1.0 - FREQ_RTN_THRESHOLD) &&
                      (rtn_freq / do_freq) > FREQ_RTN_THRESHOLD) {
                     return TRUE;
                  } else {
                     return FALSE;
                  }
               }
            } else {
               return FALSE;
            }

         default:
            // If prune_tree is TRUE, we have already scanned this
            // callee and determined this is partial inlining candidate.
            // Symbol table info may not be valid during the pruning
            // pass.
            if (!prune_tree &&
                Check_Side_Effect(wn2)) {
               return FALSE;
            }
      }
   }
#endif 
   return FALSE;
}

// This function does modify the Caller.
void
IPO_INLINE::Process_Callee (IPO_INLINE_AUX& aux, BOOL same_file)
{
    BOOL use_lowered_return_preg =
	Callee_Summary_Proc()->Use_lowered_return_preg (); 

    Is_True (use_lowered_return_preg ==
	     Caller_Summary_Proc()->Use_lowered_return_preg (),
	     ("Incompatible WHIRL between caller and callee"));

#ifndef KEY
// The following modifies the caller to change the store of return value
// after the callee is inlined. For recursive inlining, caller == callee.
// So, change the caller (i.e. the callee) after the callee is cloned below.
//
    // CALLEE SIDE OPTIMIZATION
    Compute_Return_Preg_Offset (Callee_Wn (), aux.rp, use_lowered_return_preg,
				Caller_Scope(), Caller_level()); 

    if (aux.rp.size () > 0 && WN_opcode (Call_Wn ()) != OPC_VCALL)
	Fix_Return_Pregs(Call_Wn (), aux.rp);
#endif // !KEY

    Set_Tables (Callee_node ());

    /* Exclude recursive callees from partial inlining for now since
       inlining of recursive callees require much extra handling as
       seen in ipo_main.cxx.
     */
    if (!IPA_Call_Graph->Graph()->Is_Recursive_Edge(Call_edge()->Edge_Index()) &&
        Identify_Partial_Inline_Candiate(Callee_node(),
                                  Callee_node()->Whirl_Tree(), FALSE, &aux)) {

#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))
        IPA_NODE *callee = Callee_node(); 

        Call_edge()->Set_Partial_Inline_Attrib();
 
        if( Get_Trace ( TP_IPA, IPA_TRACE_TUNING) ){
           fprintf (Y_inlining, "IPO_INLINE::Process_Callee: func %s for partial inlining \n",
                    Callee_node()->Name());
           fprintf (Y_inlining, "Dump original callee %s \n", 
                    Callee_node()->Name());
           fdump_tree(Y_inlining, Callee_node()->Whirl_Tree());
        }

        aux.inlined_body = Clone_Callee (same_file);

        BOOL prune_flag;
        prune_flag = Identify_Partial_Inline_Candiate(callee, aux.inlined_body,
                     TRUE, &aux);  

        if( Get_Trace ( TP_IPA, IPA_TRACE_TUNING) ){
           fprintf (Y_inlining, "Callee %s with prune_flag %d\n",
                    callee->Name(), prune_flag);
           fprintf (Y_inlining, "Dump inlined_body after pruning\n");
           fdump_tree(Y_inlining, aux.inlined_body);
        }

        // Don't delete callee in the partial inlining case as we currently
        // keep a call to the original callee.
        callee->Clear_Deletable();
   
        if (callee->Leftover_Body() == NULL) {
           // Encounter partial inlining of "callee" the first time.
           WN *leftover_wn = Clone_Callee (same_file);

           if( Get_Trace ( TP_IPA, IPA_TRACE_TUNING) ) {
              fprintf (Y_inlining, "Dump cloned body %s for partial inlining \n",
                    callee->Name());
              fdump_tree(Y_inlining, leftover_wn);
           }

           // Prune the partially inlined path cloned from the callee.
           // We are currently targeting idempotent partial inlining,
           // so we don't actually use the pruned callee other than 
           // detecting a constant return value. But in the future, we
           // may generate a call to the pruned leftover function.
           Prune_Callee(leftover_wn);

           callee->Set_Leftover_Body(leftover_wn);

           if( Get_Trace ( TP_IPA, IPA_TRACE_TUNING) ){
              fprintf (Y_inlining, "Dump cloned body %s after pruning\n",
                    callee->Name()); 
              fdump_tree(Y_inlining, leftover_wn);
           }

           // leftover_wn has a return value and check to see if
           // it returns a constant value.
           if (aux.rp.size () > 0 && 
               WN_opcode (Call_Wn ()) != OPC_VCALL) {
              INT64 const_val = 0;
              if (PU_Rtn_Const(leftover_wn, &const_val)) {
                 callee->Set_Leftover_Rtn_Const();
                 callee->Set_Leftover_Rtn_Val(const_val);
              }
           }
        }
#endif 
    } else {
        // clone the callee: set freq ratio, promote statics, and clone tree
        // initialize block to result
        aux.inlined_body = Clone_Callee (same_file);
    }

    // Need to recreate the parent pointers for the cloned callee_block
    // in the caller-side
    WN_Parentize (aux.inlined_body, Caller_node()->Parent_Map(),
		  Caller_Map_Table()); 

#ifdef KEY
    Merge_EH_Tables();
#endif
    Set_Tables (Caller_node ());
#ifdef KEY
    // CALLEE SIDE OPTIMIZATION
    Compute_Return_Preg_Offset (Callee_Wn (), aux.rp, use_lowered_return_preg,
				Caller_Scope(), Caller_level()); 

    if (aux.rp.size () > 0 && WN_opcode (Call_Wn ()) != OPC_VCALL) {
	Fix_Return_Pregs(Call_Wn (), aux.rp);
    }
#endif // !KEY
    Walk_and_Update_Callee (aux);

#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))
    // Clone Constraint Graph nodes and StInfos into caller
    if (Alias_Nystrom_Analyzer) {
      ConstraintGraph::promoteLocals(Callee_node());
      ConstraintGraph::cloneConstraintGraphMaps(Caller_node(), Callee_node());
      IPA_NystromAliasAnalyzer::aliasAnalyzer()->solveInlineConstraints(Callee_node(), Caller_node());
      IPA_NystromAliasAnalyzer::aliasAnalyzer()->updateCloneTreeWithCgnode(aux.inlined_body);
      ConstraintGraph::clearOrigToCloneStIdxMap(Caller_node(), Callee_node());
    }
#endif

    // This walk involves:
    // EH Processing in the callee 
    // a. Process_Callee_EH(callee_block);
    // b. MP related processing in callee: 
    // eg with generation of pregs for actuals, we need to create 
    // pragma "shared" for such pregs IF there is a mp region in callee

} // Process_Callee


// CALLER SIDE POST PROCESSING
// ======================================================================
// Post-processing in caller
// Done with inlining; During post processing, 
//
// insert, if necessary, the following:
//
// INLINE_BODY_START pragma
//   ALLOCA (save and restore stack if callee has alloca's)
//     BARRIER (for fortran array parameters)
//       COPY-IN parameters (fortran)
//         entry label (if DST is on)
//	     inlined body
//         return label (if has early return)
//       COPY-OUT parameters
//     BARRIER
//   DEALLOCA
// INLINE_BODY_END pragma
// ======================================================================
void
IPO_INLINE::Post_Process_Caller (IPO_INLINE_AUX& aux)

{
    Set_Tables (Caller_node ());	// Set the globals: Scope_tab,
					// Current_scope, Current_pu  
#ifdef KEY
    Caller_node()->Set_EH_spec_size (Caller_node()->EH_spec_size() +
    				     Callee_node()->EH_spec_size());
#endif
    WN* call = Call_Wn ();
    WN* parent_wn = WN_Get_Parent(call, Parent_Map, Current_Map_Tab);

    if( Get_Trace ( TP_IPA, IPA_TRACE_TUNING) ){
       fprintf (Y_inlining, "Dump caller %s before inlining, \n", Caller_node()->Name());
       fdump_tree(Y_inlining, Caller_node()->Whirl_Tree());
    }

    Is_True (((WN_operator(parent_wn) == OPR_BLOCK)),
	     ("Illegal Parent for the call statement")); 

    const PU& pu = Pu_Table[ST_pu (Callee_node ()->Func_ST ())];

    {
        DST_INFO_IDX dst1 = DST_INVALID_INIT;
	INT64 lang = PU_src_lang (pu);
	ST* cp = Callee_node ()->Func_ST ();
        ST_IDX stidx = ST_st_idx(cp);

#if !defined(TARG_SL)
        // for TARG_SL, the following code will generate an assertion in cgdwarf.cxx
        // so, currently, we still disable the following code.
        if(Caller_file_dst() == Callee_file_dst())
        {
          dst1 = DST_mk_inlined_subroutine(stidx,
                                           stidx+ST_index(cp),
                                           get_abstract_origin(Callee_dst()),
                                           Callee_file_dst());
          DST_RESET_assoc_fe (DST_INFO_flag(DST_INFO_IDX_TO_PTR(dst1)));
          DST_append_child (Caller_dst(), dst1);       
        }
#endif

        // pragma node that mark the begin and end of the inlined block
	WN* pragma_node_begin =
	    WN_CreatePragma (WN_PRAGMA_INLINE_BODY_START, cp, lang, 0); 
  
	// everything from here to the inlined body uses the line number of 
	// the call
	WN_Set_Linenum (pragma_node_begin, WN_Get_Linenum (call));

	WN* pragma_node_end =
	    WN_CreatePragma (WN_PRAGMA_INLINE_BODY_END, cp, lang, 0);  
    
	Insert_Block_Around (parent_wn, call, pragma_node_begin,
			     pragma_node_end);
    }

    // if callee has alloca, generate code for restore of sp
    if (INLINE_Optimize_Alloca) {
	PREG_NUM saved_sp;
	if (Alloca_Dealloca_On) {
	    if (PU_has_user_alloca (pu)) {
		Save_And_Restore_Stack (call);
	    }
	} else if (PU_has_alloca (pu)) {
	    saved_sp = Process_Alloca_Preamble();
	    Process_Alloca_Postamble (saved_sp);
	} 
    }
  
    // create barriers
    if (IPA_Enable_BarrierFarg || INLINE_Enable_Restrict_Pointers) {
	Generate_Barriers (parent_wn, call, aux);
    }

    // throw in all the assignment statements for call by value parameters 
    // as well as copy-in/copy-out fortran parameter

    Insert_Block_Around (parent_wn, call, aux.copy_in_block,
			 aux.copy_out_block);

    if (Callee_Summary_Proc()->Has_early_returns()) {
	if (IPA_Enable_DST)
	    New_LABEL (Caller_level (), aux.entry_label);
	else
	    aux.entry_label = 0;

	Insert_Labels (call, aux.return_label, aux.entry_label,
		       aux.inlined_body);
    }
  
    // Finally, replace the call by the inlined block
    if (WN_first (aux.inlined_body) != NULL) {
	WN_next (WN_prev (call)) = WN_first (aux.inlined_body);
	WN_prev (WN_first (aux.inlined_body)) = WN_prev (call);

	WN_next (WN_last (aux.inlined_body)) = WN_next (call);
	WN_prev (WN_next (call)) = WN_last (aux.inlined_body);
    } else {
	WN_next (WN_prev (call)) = WN_next (call);
	WN_prev (WN_next (call)) = WN_prev (call);
    }

    // all the cleanup code should have the same line number as the call
    WN_Set_Linenum (WN_next (call), WN_Get_Linenum (call));

    // Process partial inlining call edge.
    // Move the call site to the original callee under the inverse of
    // the condition being partially inlined.
    if ( Call_edge()->Has_Partial_Inline_Attrib() ) {
       // Place the "call" wn into the ELSE clause of the IF from 
       // partial inlining. Modify the "call" to call the leftover
       // function (for now we still call the original callee since
       // the partially inlined codes are idempontent).
       /*
         foo() {
           if (cond)     // This if clause to be partially inlined.
             return;
           ....
         }

         => // call site after partial inlining

         if (cond) {
           goto cont;
         } else {
           r = foo();        // move the original call site
           r = <const-val>;  // if applicable
         }
         cont:
       */
       Is_True (((WN_operator(aux.part_inl_leftover_call_site) == OPR_BLOCK)),
	     ("Illegal Parent for the partial inlining leftover call site")); 

       WN_prev (call) = NULL;
       WN_next (call) = NULL;

       // Check to see if "call" has a return value.
       if (aux.rp.size () > 0 && WN_opcode (call) != OPC_VCALL &&
           WN_opcode (call) != OPC_VICALL && WN_opcode (call) != OPC_VINTRINSIC_CALL) {
          // Place the call site in a block.
          LWN_Insert_Block_Before(aux.part_inl_leftover_call_site, NULL, call);
          WN_Set_Parent(call, aux.part_inl_leftover_call_site, Parent_Map, Current_Map_Tab);

          // Create a LDID for the return value.
          ST* tmp_st = aux.rp.find_st ();
          TY_IDX stid_ty = ST_type(tmp_st);
          WN *wn_ldid = WN_Ldid(TY_mtype(stid_ty), -1, Return_Val_Preg,
                                stid_ty); 

          // Create an STID to store to a tmp.
          WN *wn_stid = WN_StidPreg(TY_mtype (stid_ty), 
                                    aux.rp.find((PREG_IDX) -1), 
                                    wn_ldid);
          WN_Set_Parent(wn_ldid, wn_stid, Parent_Map, Current_Map_Tab);

          // Insert the STID as a kid of the ELSE block.
          LWN_Insert_Block_Before(aux.part_inl_leftover_call_site,
                                  NULL, wn_stid);
          WN_Set_Parent(wn_stid, aux.part_inl_leftover_call_site,
                        Parent_Map, Current_Map_Tab);

          // If the leftover function has a constant return value,
          // insert an assignment of the const value to the return
          // value after the call. Although we don't call the leftover
          // function currently, we create a whirl tree corresponding to
          // that to check if there is a constant return value on the
          // inverse path.
          if (Callee_node()->Is_Leftover_Rtn_Const()) {
             OPCODE op = OPCODE_make_op (OPR_INTCONST, MTYPE_I8, 
                                         MTYPE_V);
             WN *kid = WN_CreateIntconst (op, 
                                          Callee_node()->Leftover_Rtn_Val());
             // Create an STID to assign the return var with the const.
             WN *wn_stid2 = WN_StidPreg(TY_mtype (stid_ty), 
                                        aux.rp.find((PREG_IDX) -1), 
                                        kid);
             LWN_Insert_Block_After(aux.part_inl_leftover_call_site,
                                    wn_stid, wn_stid2);
             WN_Set_Parent(wn_stid2, aux.part_inl_leftover_call_site,
                           Parent_Map, Current_Map_Tab);
          }
       } else {
          // The callee has no return value. Just place "call" at the
          // ELSE clause.
          LWN_Insert_Block_Before(aux.part_inl_leftover_call_site,
                                  NULL, call);
          WN_Set_Parent(call, aux.part_inl_leftover_call_site,
                        Parent_Map, Current_Map_Tab);
       }

       if( Get_Trace ( TP_IPA, IPA_TRACE_TUNING) ){
          fprintf (Y_inlining, "Dump caller %s after partial inlining\n", 
                   Caller_node()->Name());
          fdump_tree(Y_inlining, Caller_node()->Whirl_Tree());
       }
    } else {
       if( Get_Trace ( TP_IPA, IPA_TRACE_TUNING) ){
          fprintf (Y_inlining, "Dump caller %s after full inlining, \n", Caller_node()->Name());
          fdump_tree(Y_inlining, Caller_node()->Whirl_Tree());
       }
    }

} // IPO_INLINE::Post_Process_Caller() 



//======================================================================
// MAIN EXPORTED entry point: perform the actual inlining  
//======================================================================
void
IPO_INLINE::Process()
{

  Temporary_Error_Phase ephase ("Inline processing");

  initial_initv_tab_size = Initv_Table.Size();

  BOOL same_file =    // caller & callee from same file?, used many places
      (Caller_node ()->File_Index () == Callee_node ()->File_Index ()) ;


  IPO_INLINE_AUX aux_info (Callee_node ()->Num_Formals (), &Ipo_mem_pool);

  // Pre_Process_Caller to initialize return_label

  Pre_Process_Caller (aux_info.return_label);

  // Process the callee: clone callee, MP processing, substitute formals, 
  // EH processing, Region Processing,.. replace RETURN by goto return_label

  Process_Callee (aux_info, same_file);

  Post_Process_Caller (aux_info);

  // propagate flags from callee to caller,
  Propagate_Flags (Caller_node (), Callee_node ());

#ifdef _LIGHTWEIGHT_INLINER
  if (INLINE_Use_Malloc_Mempool) {
      WN* wn = WN_func_body(Callee_Wn ());
      UINTPS inlined_body = (UINTPS)aux_info.inlined_body;
      if (OPCODE_has_next_prev(WN_opcode(wn)))
	    inlined_body = inlined_body - ((UINTPS)&(WN_real_fields((STMT_WN *)wn)) - UINTPS(wn));
      Caller_node()->Add_to_inlined_list ((char *)inlined_body);
  }
#endif

  if (Cur_PU_Feedback) {
#ifdef KEY
    const FB_VERIFY_STATUS status = Cur_PU_Feedback->Verify("IPA/inline");
    if( status == FB_VERIFY_UNBALANCED ){
      ;

    } else if( status == FB_VERIFY_INVALID ){
      DevWarn( "Feedback Verify fails after inlining %s to %s",
	       Callee_node()->Name(), Caller_node()->Name() );
    }
#else
    BOOL not_pass = Cur_PU_Feedback->Verify("IPA/inline");
    if ( not_pass ) { //FB_VERIFY_CONSISTENT = 0 
      DevWarn("Feedback Verify fails after inlining %s to %s",
	      Callee_node()->Name (), Caller_node()->Name ());
    }
#endif // KEY
  }

#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))
  if (IPA_Enable_DST) {
    /* the following is to set DST_SUBPROGRAM_decl_inline flag
     * appropiately for the callee
     */
    Current_DST = Callee_file_dst();

    Set_FILE_INFO_has_inlines(Caller_file_info());

    DST_subprogram_concrete_to_abstract(Callee_dst());

    /* the following is to add the DST_TAG_inlined_subroutine DST entries
     * for the inlined routine in the caller's DST
     */

    DST_enter_inlined_subroutine(Caller_dst(), Callee_dst(),
				 aux_info.entry_label, aux_info.return_label,
				 Caller_file_dst(),
				 Callee_file_dst(),
				 Symtab(), Caller_node ()->Mem_Pool (),
				 Callee_node ()->Mem_Pool (),
				 same_file ? 0 : Callee_cross_file_id());
  }
#endif // _STANDALONE_INLINER

} // IPO_INLINE::Process

