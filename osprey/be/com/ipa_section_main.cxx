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


//* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */
//
// ====================================================================
// ====================================================================
//
// Module: ipa_section_main.cxx
// $Revision: 
// $Date: 
// $Author:
// $Source:
//
// Revision history:
//  4-Sept-97 - Original Version
//
// Description:
//
// This module contains the array sections utils needed by main IPA.
//
// ====================================================================
// ====================================================================

#include <stdint.h>
#include <sys/types.h>
#include <alloca.h>

#ifdef __MINGW32__
#include <WINDOWS.h>
#endif /* __MINGW32__ */
#include "defs.h"
#include "cxx_memory.h"
#include "soe.h"
#include "ipa_section.h"
#include "ipa_lno_util.h"
#include "opt_sys.h"

#ifndef IPA_SUMMARY
INT IPA_Ivar_Global_Count;
INT IPA_Ivar_Count;
mINT32
SYSTEM_OF_EQUATIONS::_work_cols;
mINT32
SYSTEM_OF_EQUATIONS::_work_rows_eq;
mINT32
SYSTEM_OF_EQUATIONS::_work_rows;
template <> MEM_POOL*
MAT<int>::_default_pool;
#endif

//=========================================================================
// free the terms
//=========================================================================
void
LINEX::Free_terms()
{
  _larray.Free_array();
  _larray.Resetidx();
}
 
//=========================================================================
// reset the node fields
//=========================================================================
void
PROJECTED_NODE::Reset_node()
{
  LINEX *l;
  if (l = Get_upper_linex()) 
    l->Free_terms();
  if (l = Get_lower_linex()) 
    l->Free_terms();
  if (l = Get_step_linex())
    l->Free_terms();
  if (l = Get_segment_length_linex())
    l->Free_terms();
  if (l = Get_segment_stride_linex())
    l->Free_terms();

  Set_flags(0);
}

//=========================================================================
// Set_constant_linexs. Set the linexs to contain constant values
//=========================================================================
void
PROJECTED_NODE::Set_constant_linexs(INT32 upper, 
				    INT32 lower, 
				    INT32 step, 
				    INT32 segment_length,
				    INT32 segment_stride)
{
  MEM_POOL* pool = Mem_Pool();
  Set_upper_linex((LINEX*)CXX_NEW(LINEX(pool),pool));
  Get_upper_linex()->Set_term(LTKIND_CONST, upper, CONST_DESC,0);
  Set_lower_linex((LINEX*)CXX_NEW(LINEX(pool),pool));
  Get_lower_linex()->Set_term(LTKIND_CONST, lower, CONST_DESC,0);
  Set_step_linex((LINEX*)CXX_NEW(LINEX(pool),pool));
  Get_step_linex()->Set_term(LTKIND_CONST, step, CONST_DESC,0);
  if (segment_length <= 0) {
    Set_segment_length_linex(NULL);
    Set_segment_stride_linex(NULL);
  } else { 
    Set_segment_length_linex((LINEX*)CXX_NEW(LINEX(pool),pool));
    Get_segment_length_linex()->Set_term(LTKIND_CONST, segment_length, 
      CONST_DESC,0);
    Set_segment_stride_linex((LINEX*)CXX_NEW(LINEX(pool),pool));
    Get_segment_stride_linex()->Set_term(LTKIND_CONST, segment_stride, 
      CONST_DESC,0);
  } 
  Set_flags(0);
}

//=====================================================
// Set linexes for a constant two-strided array section
//=====================================================
void
PROJECTED_NODE::Set_constant_two_strided_section(INT32 lower, 
                                                 INT32 upper, 
                                                 INT32 step, 
                                                 INT32 seg_len,
                                                 INT32 seg_stride)
{
  MEM_POOL* pool = Mem_Pool(); 
  Set_lower_linex(CXX_NEW(LINEX(pool), pool));
  Get_lower_linex()->Set_term(LTKIND_CONST, lower, CONST_DESC, 0);

  Set_upper_linex(CXX_NEW(LINEX(pool), pool));
  Get_upper_linex()->Set_term(LTKIND_CONST, upper, CONST_DESC, 0);

  Set_step_linex(CXX_NEW(LINEX(pool), pool));
  Get_step_linex()->Set_term(LTKIND_CONST, step, CONST_DESC, 0);

  Set_segment_length_linex(CXX_NEW(LINEX(pool), pool));
  Get_segment_length_linex()->Set_term(LTKIND_CONST, seg_len, CONST_DESC, 0);

  Set_segment_stride_linex(CXX_NEW(LINEX(pool), pool));
  Get_segment_stride_linex()->Set_term(LTKIND_CONST, seg_stride, CONST_DESC,0);

  Set_flags(0);
}

//-----------------------------------------------------------------------
// NAME: PROJECTED_NODE::Fill_Out
// FUNCTION: Converts a projected node of the form [lb:NULL:NULL] to 
//   [lb:lb:1]. 
//-----------------------------------------------------------------------

void PROJECTED_NODE::Fill_Out()
{ 
  LINEX* lx_lower = Get_lower_linex(); 
  LINEX* lx_upper = Get_upper_linex(); 
  LINEX* lx_step = Get_step_linex(); 
  if (lx_upper != NULL && lx_upper->Num_terms() >= 0 
      && lx_step != NULL && lx_step->Num_terms() >= 0)
    return; 
  Reset_is_unprojected();
  if (lx_upper != NULL)
    lx_upper->Free_terms(); 
  if (lx_step != NULL) 
    lx_step->Free_terms(); 
  lx_step->Set_term(LTKIND_CONST, (INT32) 1, CONST_DESC, 0); 
  for (INT i = 0; i <= lx_lower->Num_terms(); i++)
    lx_upper->Set_term(lx_lower->Get_term(i));
}

//=====================================
// Set the linexs in the projected node
//=====================================
void
PROJECTED_NODE::Set_linexs(LINEX* low_new, 
			   LINEX* up_new, 
			   LINEX* step_new,
			   LINEX* segment_length_new,
			   LINEX* segment_stride_new)
{
  Reset_node();

  if (low_new)
    low_new->Copy(Get_lower_linex());
  if (up_new)
    up_new->Copy(Get_upper_linex());
  if (step_new)
    step_new->Copy(Get_step_linex());
  if (segment_length_new)
 
  segment_length_new->Copy(Get_segment_length_linex());
  if (segment_stride_new)
    segment_stride_new->Copy(Get_segment_stride_linex());
}

//================================================
// copy the terms from one linex to another
// discard empty terms that may show up during
// Set_to_kernel_image merge or subtract operation
//================================================
void
LINEX::Copy(LINEX *to)
{
  if (this == to)
    return;

  for (INT i = 0; i <= Num_terms(); ++i) {
    if ((Get_term(i)->Get_coeff() == 0 && 
         Get_term(i)->Get_type() == LTKIND_CONST) ||
        (Get_term(i)->Get_coeff() != 0))
      to->Set_term(Get_term(i));
  }
}

//========================
// copy the projected node
//========================
void
PROJECTED_NODE::Copy(PROJECTED_NODE* to)
{
  MEM_POOL* m = Mem_Pool();
  if (this == to)
    return;

  to->Set_flags(Get_flags());
  
  to->Set_lower_linex(CXX_NEW(LINEX(m), m));
  Get_lower_linex()->Copy(to->Get_lower_linex());
  
  to->Set_upper_linex(CXX_NEW(LINEX(m), m));
  Get_upper_linex()->Copy(to->Get_upper_linex());

  to->Set_step_linex(CXX_NEW(LINEX(m), m));
  Get_step_linex()->Copy(to->Get_step_linex());

  if (Get_segment_length_linex() != NULL) {
    to->Set_segment_length_linex(CXX_NEW(LINEX(m), m));
    Get_segment_length_linex()->Copy(to->Get_segment_length_linex());
  } else { 
    to->Set_segment_length_linex(NULL);
  } 

  if (Get_segment_stride_linex() != NULL) {
    to->Set_segment_stride_linex(CXX_NEW(LINEX(m), m));
    Get_segment_stride_linex()->Copy(to->Get_segment_stride_linex());
  } else { 
    to->Set_segment_stride_linex(NULL);
  } 
}

//======================================
// construct and copy the projected node
//======================================
void
PROJECTED_REGION::Copy_projected_node(PROJECTED_NODE* node)
{
  PROJECTED_NODE* to = Get_projected_node(Get_projected_array()->Newidx());
  node->Copy(to);
}

//===================================================================
// projected region constructor
//===================================================================
PROJECTED_REGION::PROJECTED_REGION(PROJECTED_REGION* p) 
{
  Set_Mem_Pool(Mem_Pool());
  Set_num_dims(p->Get_num_dims());
  Set_type(p->Get_type());
  Set_depth(p->Get_depth());
  Set_projected_kernel(p->Get_projected_kernel());
  Set_projected_array(CXX_NEW(PROJECTED_ARRAY(Mem_Pool()), Mem_Pool()));
  for (INT i = 0; i < Get_num_dims(); ++i) {
    Copy_projected_node(p->Get_projected_node(i));
  }
}

//===============================================
// check if the LINEX represents a constant value
//===============================================
BOOL LINEX::Is_const()
{
  return ((Num_terms() == 0) && (Get_term(0)->Get_type() == LTKIND_CONST));
}

//===================================================================
// return the max value assuming both the linexs have constant terms
//===================================================================
INT
LINEX::Max(LINEX* l)
{
  Is_True(Is_const(), ("LINEX::Max - Expecting constant LINEX"));
  Is_True(l->Is_const(), ("LINEX::Max - Expecting constant LINEX"));

  INT32 val1 = Get_term(0)->Get_coeff();
  INT32 val2 = l->Get_term(0)->Get_coeff();

  return MAX(val1, val2);
}

//=============================================================================
// 2 linex structures are equivalent if their terms are the same
//=============================================================================
BOOL
LINEX::Equivalent(LINEX &b )
{
  INT32 num_terms = Num_terms();
  if (num_terms != b.Num_terms()) {
    return FALSE;
  }

  for (INT i = 0; i <= num_terms; ++i) {
    if (!Get_term(i)->Equivalent(*b.Get_term(i))) {
      return FALSE;
    }
  }
  
  return TRUE;
}

//=====================================================================
// 2 term structures are equivalent if their coeff, types etc are the
// the same
//=====================================================================
BOOL
TERM::Equivalent (TERM& t)
{
  return (Get_type() == t.Get_type() &&
          Get_coeff() == t.Get_coeff() &&
          Get_desc() == t.Get_desc());
}

//=================================================================
// set the terms of the linex
//=================================================================
void 
LINEX::Set_linex_terms(INT start_index, INT end_index, TERM* term)
{
  for (INT i = start_index; i < end_index; ++i) {
    Set_term(&term[i]);
  }
}

//====================================================================
// this function is used to create the linex structure from the
// information in the summary. 
// MEM_POOL *pool : mem pool
// TERM* term     : the term array from the summary
//====================================================================
void
PROJECTED_NODE::Create_linex(TERM* term)
{
  MEM_POOL* pool = Mem_Pool();
  INT uterm_index = Get_ub_term_index();
  INT lterm_index = Get_lb_term_index();
  INT sterm_index = Get_step_term_index();
  INT segment_length_index = Get_segment_length_term_index();
  INT segment_stride_index = Get_segment_stride_term_index();
  
  INT uterm_count = Get_ub_term_count();
  INT lterm_count = Get_lb_term_count();
  INT sterm_count = Get_step_term_count();
  INT segment_length_count = Get_segment_length_term_count();
  INT segment_stride_count = Get_segment_stride_term_count();
  
  // Let the constructor do the initializiation
  Set_upper_linex((LINEX*) CXX_NEW(LINEX(pool), pool));
  Set_lower_linex((LINEX*) CXX_NEW(LINEX(pool), pool));
  Set_step_linex((LINEX*) CXX_NEW(LINEX(pool), pool));
  if (segment_length_count > 0)
    Set_segment_length_linex((LINEX*) CXX_NEW(LINEX(pool), pool));
  else 
    Set_segment_length_linex(NULL);
  if (segment_stride_count > 0)
    Set_segment_stride_linex((LINEX*) CXX_NEW(LINEX(pool), pool));
  else 
    Set_segment_stride_linex(NULL);

  Get_upper_linex()->Set_linex_terms(uterm_index, uterm_index+uterm_count, 
    term);
  Get_lower_linex()->Set_linex_terms(lterm_index, lterm_index+lterm_count, 
    term);
  Get_step_linex()->Set_linex_terms(sterm_index, sterm_index+sterm_count, 
    term);
  if (segment_length_count > 0)
    Get_segment_length_linex()->Set_linex_terms(segment_length_index, 
      segment_length_index+segment_length_count, term);
  if (segment_stride_count > 0)
    Get_segment_stride_linex()->Set_linex_terms(segment_stride_index, 
      segment_stride_index+segment_stride_count, term);
}

//====================================================================
// this function is used to create the linex structure from the
// information in the summary. 
// MEM_POOL *pool : memory pool
// TERM *term : the term array from the summary
//====================================================================
void
LOOPINFO::Create_linex(TERM* term)
{
  MEM_POOL* pool = Mem_Pool();

  // NOTE: because indices and counts are unioned with LINEX pointers,
  // their values must be fetched before pointers are assigned
  INT ub_start   = Get_ub_term_index();
  INT ub_end     = ub_start + Get_ub_term_count();
  INT lb_start   = Get_lb_term_index();
  INT lb_end     = lb_start + Get_lb_term_count();
  INT step_start = Get_step_term_index();
  INT step_end   = step_start + Get_step_term_count();
  
  if (!Is_messy_ub()) {
    u1.u3._upper_linex = CXX_NEW(LINEX(pool), pool);
    u1.u3._upper_linex->Set_linex_terms(ub_start, ub_end, term);
  }

  if (!Is_messy_lb()) {
    u1.u3._lower_linex = CXX_NEW(LINEX(pool), pool);
    u1.u3._lower_linex->Set_linex_terms(lb_start, lb_end, term);
  }

  if (!Is_messy_step()) {
    u1.u3._step_linex = CXX_NEW(LINEX(pool), pool);
    u1.u3._step_linex->Set_linex_terms(step_start, step_end, term);
  }
}

//==================================================================
// initialize the linex dynamic array structure
//==================================================================
void
LINEX::Init(MEM_POOL *m)
{
  // call placement new to initialize linex
  new (this) LINEX(m);
}

//===================================================================
// Find the position of this symbol in the symbol list
// if found return the position else add this symbol to the list
// and add a variable to the system of equations
//===================================================================
extern INT 
Locate_symbol(LOOP_SYMBOL_ARRAY* syms, 
              SYSTEM_OF_EQUATIONS* soe, 
              const LOOP_SYMBOL& symbol)
{
  INT i;
  
  for (i = 0; i < syms->Elements(); ++i) {
    if ((*syms)[i] == symbol) {
      return i; 
    }
  }
 
  syms->AddElement(symbol);
  soe->Add_Vars(1);

  return i; 
}

//========================================================================
// Add a linex to the system of equations
// The system of equations is setup as:
// A*X + B*Y + C*Z <= b
// X is a vector representing dimensions of the region
// Y is a vector representing the loop indices
// Z is a list of the symbolic variables in the linear term
// b is a vector of constant offsets
//========================================================================
void 
LINEX::Add_access(SYSTEM_OF_EQUATIONS *soe,
		  mUINT8 depth, 
                  INT num_dim,  
                  INT axle,
		  INT num_syms, 
                  ACTION_TYPE act, 
                  LOOP_SYMBOL_ARRAY* sym, 
		  BOOL trace)
{
  INT pos, c = 0;
  BOOL ltkind_subscr = FALSE;

  if (trace) {
    fprintf(stderr, "\n Add_access: Adding a LINEX to the SOE\n");
    Print(stderr);
    fprintf(stderr, "\n to SOE:\n");
    soe->Print(stderr);
  }
  
  // the size of the vector is = # of dimensions (for coupled
  // subscript values) + # of symbolic constants + # of loop indices + 1
  INT vector_size = num_dim + depth + num_syms + 1;
  if (trace) {
    printf("num_dim = %d, depth = %d, num_syms = %d vector size %d \n", 
           num_dim, depth, num_syms,vector_size);
  }

  mINT32* v = (mINT32*) alloca(sizeof(mINT32) * vector_size);
  BZERO(v, sizeof(mINT32) * vector_size);

  // for each term store the coeff in the right place in the vector
  for (INT i = 0; i <= Num_terms(); ++i) {
    TERM* term = Get_term(i);
    switch (term->Get_type()) {

      case LTKIND_CONST:
        c = term->Get_coeff();
        break;

      case LTKIND_LINDEX:
        // set the value of matrix A
        // note, when we first start out this is all zeros
        // we are setting this term to one in the case of the equality 
        // constraint implying that this particular dimension or axle is
        // equal to the subscript expression
        v[num_dim+term->Get_desc()] = term->Get_coeff();
        break;

      case LTKIND_SUBSCR:
        v[term->Get_desc()] = term->Get_coeff();
        ltkind_subscr = TRUE;
        break;
	  
      case LTKIND_IV:
        pos = Locate_symbol(sym, soe, LOOP_SYMBOL(term->Get_desc()));
        v[num_dim + depth + pos] = term->Get_coeff();
        break;

      case  LTKIND_NONE:
        Fail_FmtAssertion("Add_access: invalid ltkind =  LTKIND_NONE");
        break;
    }
  }

  if (!ltkind_subscr) {
    if (act == ACTION_LO)
      v[axle] = -1;
    else  
      v[axle] = 1;
  }

  // if the action is equal implying this is an
  // equality constraint or an upper bound constraint
  // and hence we reverse coefficients since
  // once they move to the other side of the equation they
  // will change signs
  if (act != ACTION_LO) {
    for (INT i = num_dim; i < vector_size; ++i) 
      v[i] = -v[i];
  }

  // throw in the constant offset on the rhs
  if (act == ACTION_LO) 
    c = -c;
  
  // note, we need to add in the equality constraints for
  // the subscript expressions
  if (act != ACTION_EQ)
    soe->Add_Le(v, c);
  else
    // for equality constraints add to the set of soes 
    // containing equalities
    soe->Add_Eq(v,c);

  if (trace) {
    fprintf(stderr,"\n Add_access: New SOE is:\n");
    soe->Print(stderr);
  }
}

//==========================================================================
//      Add the axle 'pos' of REGION 'a' to the SOE 'soe'.
// Convert one equation into two inequalities if convert_equation is TRUE
//==========================================================================
extern void
Add_to_SOE(PROJECTED_REGION* a, 
           const INT pos, 
           SYSTEM_OF_EQUATIONS* soe,
	   const BOOL convert_equation, 
	   LOOP_SYMBOL_ARRAY* sym, 
           INT depth, 
           BOOL trace)
{
#ifdef IPA_SUMMARY
  INT num_syms = Ivar->Elements();
#else 
  INT num_syms = IPA_Ivar_Count;
#endif

  PROJECTED_ARRAY* array = a->Get_projected_array();
  PROJECTED_NODE* node = &(*array)[pos];

  if (!node->Is_unprojected() &&
      node->Get_upper_linex()->Num_terms() != -1) {
    node->Get_lower_linex()->Add_access(soe, depth, a->Get_num_dims(), pos,
                                        num_syms, ACTION_LO, 
                                        sym, trace);

    node->Get_upper_linex()->Add_access(soe, depth, a->Get_num_dims(), pos,
                                        num_syms, ACTION_UP, 
                                        sym, trace);
  } 
  else {
    if (convert_equation) {
      node->Get_lower_linex()->Add_access(soe, depth, a->Get_num_dims(), pos,
                                          num_syms, ACTION_LO,
                                          sym, trace);

      node->Get_lower_linex()->Add_access(soe, depth, a->Get_num_dims(), pos,
                                          num_syms, ACTION_UP,
                                          sym, trace);
    } 
    else {
      node->Get_lower_linex()->Add_access(soe, depth, a->Get_num_dims(), pos,
                                          num_syms, ACTION_EQ,
                                          sym, trace);
    }
  }
}

//===================================================================
// projected region constructor
//===================================================================
PROJECTED_REGION::PROJECTED_REGION(mINT16 type,
                                   mUINT8 depth, 
                                   mUINT8 dim, 
                                   MEM_POOL* mem_pool) 
  : _type (type),
    _num_dims (dim),
    _depth (depth),
    _mem_pool (mem_pool)
{
  u2._projected_kernel = 0;
  u1._region = CXX_NEW(PROJECTED_ARRAY(mem_pool), mem_pool);
  u1._region->Force_Alloc_array(dim);
  u1._region->Setidx(dim-1);
  for (INT i = 0; i < dim; i++) {
    (*(u1._region))[i].Init(mem_pool);
  }
}

//===================================================================
// Initialize a projected node
//===================================================================
void
PROJECTED_NODE::Init(MEM_POOL *m)
{
  Set_Mem_Pool(m);

  Set_upper_linex(CXX_NEW(LINEX(m), m));
  Get_upper_linex()->Init(m);

  Set_lower_linex(CXX_NEW(LINEX(m), m));
  Get_lower_linex()->Init(m);

  Set_step_linex(CXX_NEW(LINEX(m), m));
  Get_step_linex()->Init(m);

  Set_segment_length_linex(NULL);
  Set_segment_stride_linex(NULL);

  Set_flags(0);
}

//=============================================================================
// Two nodes are equal if their upper, lower and step linex
// structures are equivalent
//=============================================================================
BOOL
PROJECTED_NODE::Equivalent(PROJECTED_NODE &b)
{
  return ((Get_flags() == b.Get_flags()) &&
          (Get_upper_linex()->Equivalent(*b.Get_upper_linex())) && 
          (Get_lower_linex()->Equivalent(*b.Get_lower_linex())) && 
          (Get_step_linex()->Equivalent(*b.Get_step_linex())) && 
	  (Get_segment_length_linex() == NULL && 
	     b.Get_segment_length_linex() == NULL ||
	   Get_segment_length_linex() != NULL && 
             b.Get_segment_length_linex() == NULL && 
	   Get_segment_length_linex()->
	     Equivalent(*b.Get_segment_length_linex())) &&
	  (Get_segment_stride_linex() == NULL && 
	     b.Get_segment_stride_linex() == NULL ||
	   Get_segment_stride_linex() != NULL && 
             b.Get_segment_stride_linex() == NULL && 
	   Get_segment_stride_linex()->
	     Equivalent(*b.Get_segment_stride_linex())));
}

//==========================================================================
//      PROJECTED_REGION::Equivalent
//      return TRUE if 2 projected regions have the same section     
//==========================================================================
BOOL
PROJECTED_REGION::Equivalent(PROJECTED_REGION* p)
{
  if (Is_messy_region() && p->Is_messy_region()) {
    return TRUE;
  }
  
  INT num_dims = Get_num_dims();
  if (num_dims != p->Get_num_dims()) {
    return FALSE;
  }

  // WARNING:
  // This function is currently called only from Same_Shape
  // to compare shapes of two declaration regions. Becuase of
  // that, it can ignore the highest order dimension, but if
  // the function is to be used for general comparison of
  // PROJECTED_REGIONs, it would have to be changed!

  for (INT i = 1; i < num_dims; ++i) {
    PROJECTED_NODE* p1 = Get_projected_node(i);
    PROJECTED_NODE* p2 = p->Get_projected_node(i);
    Is_True(p1 && p2, ("PROJECTED_REGION::Equivalent: NULL projected node\n"));
    if (!p1->Equivalent(*p2)) {
      return FALSE;
    }
  }

  return TRUE;
}

//==========================================================================
//       PROJECTED_REGION::May_Union
//       MERGE SECTIONS
// ALGORITHM: walk the control dependence graph
// For each reference, check if it is in a loop, control flow node or
// not projected. For each of them check if the section is projected,
// bottom (i.e. non-constant symbolic terms, or complex terms).
// PS. make sure that there is no memory leak in here
//==========================================================================
BOOL
PROJECTED_REGION::May_Union(PROJECTED_REGION& b, 
			    BOOL trace)
{
  if (trace) {
    fprintf(stderr,"Union two PROJECTED REGIONs \n");
    b.Print(stderr);
    Print(stderr);
  }

  // First, the simple tests.
  if (Is_messy_region()) {
    return FALSE;
  }
  else if (b.Is_messy_region()) {
    Set_messy_region();
    return TRUE;
  }

  BOOL change = FALSE; 
  MEM_POOL* local_pool = Mem_Pool();
  LOOPINFO l(local_pool); 

  for (INT i = 0; i < Get_num_dims(); ++i) {

    // skip the dimension that's already messy
    PROJECTED_NODE* ax = Get_projected_node(i);
    if (ax->Has_all_messy_bounds()) {
      continue;
    }

    // check if the merged dimension is messy
    PROJECTED_NODE* bx = b.Get_projected_node(i);
    if (ax->Is_messy_lb() || ax->Is_messy_ub() ||
        bx->Is_messy_lb() || bx->Is_messy_ub()) {
      ax->Set_all_messy_bounds();
      change = TRUE;
      continue;
    }
    
    ax->Fill_Out(); 
    bx->Fill_Out(); 

    // Handle mismatched steps using GCD rule. 
    LINEX* lx_a_step = ax->Get_step_linex();
    LINEX* lx_b_step = bx->Get_step_linex();
    if (!lx_a_step->Is_const() || !lx_b_step->Is_const()) { 
      if (!lx_a_step->Is_const() || lx_a_step->Get_constant_term() != 1) { 
        lx_a_step->Free_terms();
        lx_a_step->Set_term(LTKIND_CONST, (mINT32) 1, CONST_DESC, 0);
        change = TRUE; 
      } 
    } 
    else { 
      INT a_coeff = lx_a_step->Get_term(0)->Get_coeff();
      INT b_coeff = lx_b_step->Get_term(0)->Get_coeff();
      INT gcd = Gcd(a_coeff, b_coeff); 
      LINEX* ax_lower = ax->Get_lower_linex();
      LINEX* bx_lower = bx->Get_lower_linex();
      LINEX* lx_diff = ax_lower->Subtract(bx_lower);
      lx_diff->Simplify();
      if (!lx_diff->Is_const()) { 
        if (lx_a_step->Get_term(0)->Get_coeff() != 1) { 
          lx_a_step->Get_term(0)->Set_coeff(1);
          change = TRUE; 
        } 
      } 
      else { 
        INT diff = lx_diff->Get_term(0)->Get_coeff();
        if (diff < 0)
          diff = -diff; 
        if (diff % gcd != 0) { 
          if (lx_a_step->Get_term(0)->Get_coeff() != 1) { 
            lx_a_step->Get_term(0)->Set_coeff(1);
            change = TRUE; 
          } 
        } 
        else { 
          if (lx_a_step->Get_term(0)->Get_coeff() != gcd) { 
            lx_a_step->Get_term(0)->Set_coeff(gcd);
            change = TRUE; 
          } 
        } 
      }
    }

    // Now, try to merge them
    SYSTEM_OF_EQUATIONS* soe = 
      CXX_NEW(SYSTEM_OF_EQUATIONS(0, 0, b.Get_num_dims() + b.Get_depth(),
                                  local_pool), local_pool);
    LOOP_SYMBOL_ARRAY* syms = 
      CXX_NEW(LOOP_SYMBOL_ARRAY(local_pool), local_pool);

    // Build the system of inequalities for axle 'i'
    Add_to_SOE(this, i, soe, TRUE, syms, Get_depth(), trace);
    Add_to_SOE(&b, i, soe, TRUE, syms, Get_depth(), trace);
    
    // Get rid of the redundant constraints to find the
    // intersection.
    if (soe->Copy_To_Work()) {
      BOOL* is_redundant = 
        CXX_NEW_ARRAY(BOOL, soe->Num_Le_Constraints(), local_pool);
      INT num_con = soe->Num_Le_Constraints();
      // The following is the first part of SOE::Elim_Simple_Redundant()
      num_con -= soe->Mark_Simple_Redundant(is_redundant);
      if (num_con > 2) {
        // SOE::Mark_New_Redundant() skips the inequalities that
        // are already marked as redundant in the parameter.
        num_con -= soe->Mark_New_Redundant(is_redundant);
      } 
      // If we have only two constraints left, the redundant pairs
      // constitute the union.
      if (num_con == 2) {
        if (!((is_redundant[0] && !is_redundant[2] 
               || !is_redundant[0] && is_redundant[2])
	      && (is_redundant[1] && !is_redundant[3]
                  || !is_redundant[1] && is_redundant[3]))) { 
          ax->Set_all_messy_bounds();
          change = TRUE;
          continue;
        } 
        if (!ax->Matching_Segment_Stride(bx)
            || ax->Get_segment_length_linex() != NULL 
            && !ax->Get_segment_length_linex()->Is_const()
            || bx->Get_segment_length_linex() != NULL 
            && !bx->Get_segment_length_linex()->Is_const()) {
          LINEX* lx_seg_length = ax->Get_segment_length_linex();
          lx_seg_length->Free_terms();
          LINEX* lx_seg_stride = ax->Get_segment_stride_linex();
          lx_seg_stride->Free_terms();
        } 
        else if (ax->Get_segment_stride_linex() != NULL) { 
          LINEX* lx_a_seg_length = ax->Get_segment_length_linex();
          INT a_const_value = lx_a_seg_length->Get_constant_term();
          LINEX* lx_b_seg_length = bx->Get_segment_length_linex();
          INT b_const_value = lx_b_seg_length->Get_constant_term();
          INT difference = a_const_value > b_const_value 
            ? a_const_value - b_const_value : b_const_value - a_const_value;
          ax->Get_segment_length_linex()->Set_term(LTKIND_CONST, 
                                                   difference, CONST_DESC, 0);
          ax->Get_segment_length_linex()->Simplify();
          LINEX* ax_lower = ax->Get_lower_linex();
          LINEX* bx_lower = bx->Get_lower_linex();
          LINEX* lx_diff = ax_lower->Subtract(bx_lower);
          if (!lx_diff->Is_const()) { 
            ax->Get_segment_length_linex()->Free_terms();
            ax->Set_segment_length_linex(NULL);
            ax->Get_segment_stride_linex()->Free_terms();
            ax->Set_segment_stride_linex(NULL);
          } 
          else { 
            INT difference = lx_diff->Get_constant_term();
            if (difference < 0)
              difference = -difference; 
            ax->Get_segment_length_linex()->Set_term(LTKIND_CONST,
                                                     difference, CONST_DESC,0);
            ax->Get_segment_length_linex()->Simplify();
          } 
        } 

        // The redundant equations are the ones we want to retain 
        LINEX* lb = ax->Get_lower_linex();
        if (lb && !lb->Equivalent(*(bx->Get_lower_linex()))) { 
          if (!is_redundant[0]) {
            lb->Free_terms();
            (bx->Get_lower_linex())->Copy(lb);
            change = TRUE; 
          }
        } 

        LINEX* ub = ax->Get_upper_linex();
        if (ub && !ub->Equivalent(*(bx->Get_upper_linex()))) { 
          if (!is_redundant[1]) { 
            ub->Free_terms();
            (bx->Get_upper_linex())->Copy(ub);
            change = TRUE; 
          } 
        } 

      }
      else {
        // num constraints is not equal to NULL
        ax->Set_all_messy_bounds();
        change = TRUE;
      }
    }
    else {
      // if not soe->Copy_To_Work
      ax->Set_all_messy_bounds();
      change = TRUE;
    }
  }

  if (trace) {
    fprintf(stderr,"Result of Unioning two PROJECTED REGIONs \n");
    Print(stderr);
  }

  return change; 
}

//=====================================================================
// check if all the terms are the same
//=====================================================================
BOOL
TERM::Is_equal(TERM* t, INT count)
{
  for (INT i = 0; i <= count; ++i) {
    if (! t[i].Equivalent(this[i])) {
      return FALSE;
    }
  }
  return TRUE;
}

//===================================================================
// Copy all the relevant fields when writing out the projected region
//===================================================================
void
PROJECTED_REGION::Copy_write(PROJECTED_REGION *p_in)
{
  Set_type(p_in->Get_type());
  Set_num_dims(p_in->Get_num_dims());
  Set_depth(p_in->Get_depth());
  if (p_in->Is_passed())
    {
      Set_callsite_id(p_in->Get_callsite_id());
      Set_actual_id(p_in->Get_actual_id());
    }
}

void 
Print_Symbol_Array(LOOP_SYMBOL_ARRAY* sa, FILE *fp)
{ 
  for (INT i = 0; i <= sa->Lastidx(); i++) { 
    fprintf(fp, "[%d] ", i);
    (*sa)[i].Print(fp);
  }
} 

//=====================================================================
//       PROJECTED_REGION::Constant_bounds
//       check if dimensions first_dim..num_dims-1 have constant bounds
//=====================================================================
BOOL
PROJECTED_REGION::Constant_bounds(mUINT8 first_dim)
{
  for (INT i = first_dim; i < _num_dims; ++i) {
    PROJECTED_NODE *node = Get_projected_node(i);
    LINEX* lower = node->Get_upper_linex();
    LINEX* upper = node->Get_upper_linex();
    if (!lower || !lower->Is_const() || !upper || !upper->Is_const()) {
      return FALSE;
    }
  }
  return TRUE;
}

//====================================================================
// return the value in the constant term
//====================================================================
INT
LINEX::Get_constant_term()
{
  for (INT i = 0; i <= Num_terms(); ++i) {
    if (Get_term(i)->Get_type() == LTKIND_CONST)
      return Get_term(i)->Get_coeff();
  }
  // if no constant term found
  return 0;
}

//====================================================================
// return the projected region number (i) 
//====================================================================
PROJECTED_REGION*
REGION_ARRAYS::Get_Projected_Region(INT i)
{
  PROJECTED_REGION_INFO_ARRAY *info = Get_projected_region_array();

  Is_True(info && info->Lastidx() != -1, ("Expecting at least 1 projected region in Reshape \n"));
  Is_True(info->Lastidx() >= i, ("Projected_Region %d exceeded size of array %d \n", i, info->Lastidx()));

  PROJECTED_REGION_INFO *region_info =  &(*info)[i];
  PROJECTED_REGION *proj_shape = region_info->Get_projected_region();
  Is_True(proj_shape, ("NULL Projected Region in REGION_ARRAYS::Get_Projected_Region \n"));

  return proj_shape;
}

//-----------------------------------------------------------------------
// NAME: LINEX::Remove_Zero_Terms
// FUNCTION: Remove all terms with zero coefficients from the LINEX.
//-----------------------------------------------------------------------

void LINEX::Remove_Zero_Terms()
{
  INT j = 0;
  INT num_init_terms = Num_terms();
  INT i;
  
  for (i = 0; i <= Num_terms(); i++) {
    TERM* tm = Get_term(i);
    if (tm->Get_coeff() != 0) {
      if (i != j)
        _larray[j] = _larray[i];
      j++;
    }
  }

  INT difference = i - j;
  for (i = 0; i < difference; i++)
    _larray.Decidx();


  // if there was a zero term then put 1 term back if all terms
  // have been eliminated
  if ((Num_terms() == -1) && (num_init_terms != -1))
    {
      Set_term(LTKIND_CONST, 0, CONST_DESC, 0);
    }
}

//-----------------------------------------------------------------------
// NAME: LINEX::Simplify
// FUNCTION: Simplify the LINEX by combining terms which match on all
//   components except possibly coefficients.
//-----------------------------------------------------------------------

void LINEX::Simplify()
{
  for (INT i = 0; i <= Num_terms(); i++) {
    TERM* tm_i = Get_term(i);
    for (INT j = i + 1; j <= Num_terms(); j++) {
      TERM* tm_j = Get_term(j);
      if (tm_i->Get_type() == tm_j->Get_type()
          && tm_i->Get_desc() == tm_j->Get_desc()
          && tm_i->Get_projected_level() == tm_j->Get_projected_level()) {
        tm_i->Set_coeff(tm_i->Get_coeff() + tm_j->Get_coeff());
        tm_j->Set_coeff(0);
      }
    }
  }
  Remove_Zero_Terms();
}

//-----------------------------------------------------------------------
// NAME: PROJECTED_NODE::Simplify
// FUNCTION: Simplify the LINEXes in the PROJECTED_NODE.
//-----------------------------------------------------------------------

void PROJECTED_NODE::Simplify()
{
  if (!Is_messy_lb()) {
    LINEX* lx_lower = Get_lower_linex();
    lx_lower->Simplify();
  }
  if (!Is_messy_ub()) {
    LINEX* lx_upper = Get_upper_linex();
    lx_upper->Simplify();
  }
  if (!Is_messy_step()) {
    LINEX* lx_step = Get_step_linex();
    lx_step->Simplify();
  }
  if (Get_segment_length_linex() != NULL)
    Get_segment_length_linex()->Simplify();
  if (Get_segment_stride_linex() != NULL)
    Get_segment_stride_linex()->Simplify();
}

//-----------------------------------------------------------------------
// NAME: PROJECTED_REGION::LNO_Simplify
// FUNCTION: Simplify the LINEXes in the PROJECTED_REGION.
//-----------------------------------------------------------------------

void PROJECTED_REGION::Simplify()
{
  if (Is_messy_region())
    return;
  for (INT i = 0; i < Get_num_dims(); i++) {
    PROJECTED_NODE* pn = Get_projected_node(i);
    pn->Simplify();
  }
}

BOOL PROJECTED_REGION::Has_Messy_Bounds()
{
  for (INT i = 0; i < Get_num_dims(); i++) {
    PROJECTED_NODE* pn = Get_projected_node(i);
    if (pn->Has_a_messy_bound())
      return TRUE;
  }
  return FALSE;
}

BOOL PROJECTED_REGION::Has_Important_Messy_Bounds()
{
  for (INT i = 1; i < Get_num_dims(); i++) {
    PROJECTED_NODE* pn = Get_projected_node(i);
    if (pn->Has_a_messy_bound())
      return TRUE;
  }
  return FALSE;
}

void PROJECTED_REGION::Fill_Out()
{
  if (Is_messy_region())
    return; 
  Reset_is_unprojected();
  for (INT i = 0; i < Get_num_dims(); i++) {
    PROJECTED_NODE* pn = Get_projected_node(i);
    pn->Fill_Out();
  }
}

BOOL PROJECTED_NODE::Matching_Segment_Stride(PROJECTED_NODE* pn)
{ 
  if (pn == NULL)
    return FALSE; 

  LINEX* ss_one = Get_segment_stride_linex();
  LINEX* ss_two = pn->Get_segment_stride_linex();

  if (ss_one == NULL && ss_two == NULL)
    return TRUE; 
  if (ss_one == NULL || ss_two == NULL)
    return FALSE; 
  return ss_one->Equivalent(*ss_two);
}

BOOL PROJECTED_REGION::Matching_Segment_Stride(PROJECTED_REGION* pr)
{
  if (pr == NULL)
    return FALSE; 
  if (Is_messy_region() || pr->Is_messy_region())  
    return (Is_messy_region() == pr->Is_messy_region());
  if (Get_num_dims() != pr->Get_num_dims())
    return FALSE;
  for (INT i = 0; i < Get_num_dims(); i++) {
    PROJECTED_NODE* pn_one = Get_projected_node(i);
    PROJECTED_NODE* pn_two = pr->Get_projected_node(i);
    if (!pn_one->Matching_Segment_Stride(pn_two))
      return FALSE; 
  } 
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: LINEX::Merge
// FUNCTION: Return l1 + this. 
//-----------------------------------------------------------------------

LINEX* LINEX::Merge(LINEX *l1)
{
  INT c = 0;

  // Set up the mem pool
  FmtAssert(_larray.Get_Mem_Pool() == l1->_larray.Get_Mem_Pool(),
    ("LINEX::Merge: Inconsistent mem pools"));
  MEM_POOL* mem_pool = _larray.Get_Mem_Pool();

  // Determine the size of the coefficient arrays
  INT coeff_max = -1; 
  INT sub_coeff_max = -1; 
  INT isym_coeff_max = -1;
  INT i;
  
  for (i = 0; i <= Num_terms(); i++) {
    TERM* term = Get_term(i);
    switch (term->Get_type()) {
    case LTKIND_LINDEX:
      if (term->Get_desc() > coeff_max)
	coeff_max = term->Get_desc();
      break; 
    case LTKIND_SUBSCR:
      if (term->Get_desc() > sub_coeff_max)
        sub_coeff_max = term->Get_desc();
      break; 
    case LTKIND_IV:
      if (term->Get_desc() > isym_coeff_max)
        isym_coeff_max = term->Get_desc();
      break;
    }   
  }

  for (i = 0; i <= l1->Num_terms(); i++) {
    TERM* term = l1->Get_term(i);
    switch (term->Get_type()) {
    case LTKIND_LINDEX:
      if (term->Get_desc() > coeff_max)
	coeff_max = term->Get_desc();
      break; 
    case LTKIND_SUBSCR:
      if (term->Get_desc() > sub_coeff_max)
        sub_coeff_max = term->Get_desc();
      break; 
    case LTKIND_IV:
      if (term->Get_desc() > isym_coeff_max)
        isym_coeff_max = term->Get_desc();
      break;
    }   
  }

  // Allocate them.
  INT* coeff = (INT*) alloca(sizeof(INT) * (coeff_max + 1));
  INT* sub_coeff = (INT*) alloca(sizeof(INT) * (sub_coeff_max + 1));
  INT* isym_coeff = (INT*) alloca(sizeof(INT) * (isym_coeff_max + 1));
  BZERO(coeff, sizeof(INT) * (coeff_max + 1));
  BZERO(sub_coeff, sizeof(INT) * (sub_coeff_max + 1));
  BZERO(isym_coeff, sizeof(INT) * (isym_coeff_max + 1));

  LINEX* l = CXX_NEW(LINEX(mem_pool), mem_pool);

  // For each term, store the coeff in the right place in the vector.
  for (i = 0; i <= Num_terms(); i++) {
    TERM *term = Get_term(i);
    switch (term->Get_type()) {
    case LTKIND_NONE:
      break;
    case LTKIND_CONST:
      c += term->Get_coeff();
      break;
    case LTKIND_LINDEX:
      coeff[term->Get_desc()] += term->Get_coeff();
      break;
    case LTKIND_SUBSCR:
      sub_coeff[term->Get_desc()] += term->Get_coeff();
      break;
    case LTKIND_IV:
      isym_coeff[term->Get_desc()] += term->Get_coeff();
      break;
    }
  }
  for (i = 0; i<= l1->Num_terms(); ++i) {
    TERM *term = l1->Get_term(i);
    switch (term->Get_type()) {
    case  LTKIND_NONE:
      break;
    case LTKIND_CONST:
      c += term->Get_coeff();
      break;
    case LTKIND_LINDEX:
      coeff[term->Get_desc()] += term->Get_coeff();
      break;
    case LTKIND_SUBSCR:
      sub_coeff[term->Get_desc()] += term->Get_coeff();
      break;
    case LTKIND_IV:
      isym_coeff[term->Get_desc()] += term->Get_coeff();
      break;
    }
  }

  // Put in the appropriate terms.
  l->Set_term(LTKIND_CONST, c, CONST_DESC, 0);
  for (i = 0; i <= coeff_max; i++) 
    if (coeff[i])
      l->Set_term(LTKIND_LINDEX, coeff[i], i, 0);
  for (i = 0; i <= sub_coeff_max; i++)
    if (sub_coeff[i])
      l->Set_term(LTKIND_SUBSCR, sub_coeff[i], i, 0);
  for (i = 0; i <= isym_coeff_max; i++)
    if (isym_coeff[i])
      l->Set_term(LTKIND_IV, isym_coeff[i], i, 0);
    
  return l;
}
//-----------------------------------------------------------------------
// NAME: LINEX::Subtract
// FUNCTION: Return this - l1. 
//-----------------------------------------------------------------------

LINEX* LINEX::Subtract(LINEX *l1)
{
  INT c = 0;

  // Set up the mem pool
  FmtAssert(_larray.Get_Mem_Pool() == l1->_larray.Get_Mem_Pool(),
    ("LINEX::Subtract: Inconsistent mem pools"));
  MEM_POOL* mem_pool = _larray.Get_Mem_Pool();

  // Determine the size of the coefficient arrays
  INT coeff_max = -1; 
  INT sub_coeff_max = -1; 
  INT isym_coeff_max = -1;
  INT i;
  
  for (i = 0; i <= Num_terms(); i++) {
    TERM* term = Get_term(i);
    switch (term->Get_type()) {
    case LTKIND_LINDEX:
      if (term->Get_desc() > coeff_max)
	coeff_max = term->Get_desc();
      break; 
    case LTKIND_SUBSCR:
      if (term->Get_desc() > sub_coeff_max)
        sub_coeff_max = term->Get_desc();
      break; 
    case LTKIND_IV:
      if (term->Get_desc() > isym_coeff_max)
        isym_coeff_max = term->Get_desc();
      break;
    }   
  }

  for (i = 0; i <= l1->Num_terms(); i++) {
    TERM* term = l1->Get_term(i);
    switch (term->Get_type()) {
    case LTKIND_LINDEX:
      if (term->Get_desc() > coeff_max)
	coeff_max = term->Get_desc();
      break; 
    case LTKIND_SUBSCR:
      if (term->Get_desc() > sub_coeff_max)
        sub_coeff_max = term->Get_desc();
      break; 
    case LTKIND_IV:
      if (term->Get_desc() > isym_coeff_max)
        isym_coeff_max = term->Get_desc();
      break;
    }   
  }

  // Allocate them.
  INT* coeff = (INT*) alloca(sizeof(INT) * (coeff_max + 1));
  INT* sub_coeff = (INT*) alloca(sizeof(INT) * (sub_coeff_max + 1));
  INT* isym_coeff = (INT*) alloca(sizeof(INT) * (isym_coeff_max + 1));
  BZERO(coeff, sizeof(INT) * (coeff_max + 1));
  BZERO(sub_coeff, sizeof(INT) * (sub_coeff_max + 1));
  BZERO(isym_coeff, sizeof(INT) * (isym_coeff_max + 1));

  LINEX* l = CXX_NEW(LINEX(mem_pool), mem_pool);

  // For each term, store the coeff in the right place in the vector.
  for (i = 0; i <= Num_terms(); i++) {
    TERM *term = Get_term(i);
    switch (term->Get_type()) {
    case  LTKIND_NONE:
      break;
    case LTKIND_CONST:
      c += term->Get_coeff();
      break;
    case LTKIND_LINDEX:
      coeff[term->Get_desc()] += term->Get_coeff();
      break;
    case LTKIND_SUBSCR:
      sub_coeff[term->Get_desc()] += term->Get_coeff();
      break;
    case LTKIND_IV:
      isym_coeff[term->Get_desc()] += term->Get_coeff();
      break;
    }
  }
  for (i = 0; i<= l1->Num_terms(); ++i) {
    TERM *term = l1->Get_term(i);
    switch (term->Get_type()) {
    case LTKIND_NONE:
      break;
    case LTKIND_CONST:
      c -= term->Get_coeff();
      break;
    case LTKIND_LINDEX:
      coeff[term->Get_desc()] -= term->Get_coeff();
      break;
    case LTKIND_SUBSCR:
      sub_coeff[term->Get_desc()] -= term->Get_coeff();
      break;
    case LTKIND_IV:
      isym_coeff[term->Get_desc()] -= term->Get_coeff();
      break;
    }
  }

  // Put in the appropriate terms.
  l->Set_term(LTKIND_CONST, c, CONST_DESC, 0);
  for (i = 0; i <= coeff_max; i++) 
    if (coeff[i])
      l->Set_term(LTKIND_LINDEX, coeff[i], i, 0);
  for (i = 0; i <= sub_coeff_max; i++)
    if (sub_coeff[i])
      l->Set_term(LTKIND_SUBSCR, sub_coeff[i], i, 0);
  for (i = 0; i <= isym_coeff_max; i++)
    if (isym_coeff[i])
      l->Set_term(LTKIND_IV, isym_coeff[i], i, 0);
    
  return l;
}

