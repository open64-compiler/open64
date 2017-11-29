/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

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
// Module: ipa_section.cxx
// $Revision: 
// $Date: 
// $Author:
// $Source:
//
// Revision history:
//  07-Dec-96 - Original Version
//
// Description:
//
// This module contains the IPA implementation of array sections.  See
// the header (ipa_section.h) for more interface information.
//
// ====================================================================
// ====================================================================

#include <stdint.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else
#include <elf.h>
#include <sys/elf_whirl.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <sys/types.h>
#include <alloca.h>

#include "defs.h"
#include "erglob.h"
#include "tracing.h"
#include "cxx_memory.h"
#include "access_vector.h"
#include "loop_info.h"
#include "soe.h"
#include "lno_bv.h"
#include "ipl_summary.h"
#include "ipl_summarize.h"
#include "ipl_summarize_util.h"
#include "ipl_main.h"
#include "ipl_array_bread_write.h"
#include "ipa_section_main.h"
#include "ipa_lno_file.h"
#include "ipl_lno_util.h"


extern SUMMARY *Summary;  // from IPL
extern ARRAY_SUMMARY Array_Summary; // from ipl_linex

BOOL Trace_Sections = FALSE;

IVAR_ARRAY *Ivar = NULL;

// Maps used to carry over the loop and array access info
LOOPINFO_TO_DLI_MAP*             IPL_Loopinfo_Map     = NULL;
PROJ_REGION_TO_ACCESS_ARRAY_MAP* IPL_Access_Array_Map = NULL;

#ifdef SHARED_BUILD
mINT32
SYSTEM_OF_EQUATIONS::_work_cols;
mINT32
SYSTEM_OF_EQUATIONS::_work_rows_eq;
mINT32
SYSTEM_OF_EQUATIONS::_work_rows;
#endif

//====================================================================
// initialize ivar and ivar global arrays
//====================================================================
extern void
Init_ivar_arrays()
{
  Ivar = Array_Summary_Output->Get_ivar_array();
  Ipl_Summary_Symbol = Summary->Get_symbol(0);
}

//====================================================================
// find the node in the ivar array
//====================================================================
static INT32
Get_ivar(const IVAR_ARRAY& iv_array, const IVAR& iv)
{
  for (INT32 i = 0; i < iv_array.Elements(); ++i) {
    if (iv_array[i] == iv) {
      return i;
    }
  }
  return -1;
}

// -----------------------------------------------------------
// The only linear symbols that are currently supported are:
//   named constants
//   global variables (possibly with offsets)
//   formal parameters (of the current PU, not the parent one)
// -----------------------------------------------------------
static BOOL
Access_vector_is_too_messy(ACCESS_VECTOR* av)
{
  if (av->Too_Messy || 
      av->Delinearized_Symbol ||
      av->Contains_Non_Lin_Symb()) {
    return TRUE;
  }
  
  if (av->Contains_Lin_Symb()) {

    INTSYMB_ITER iter(av->Lin_Symb);
    for (INTSYMB_NODE* cur = iter.First(); 
         !iter.Is_Empty(); 
         cur = iter.Next()) {

      ST* st = cur->Symbol.St();

      if (ST_class(st) == CLASS_CONST) {
        INT64 const_value;
        if (!Targ_Is_Integral(STC_val(st), &const_value) ||
            const_value * cur->Coeff < INT32_MIN ||
            const_value * cur->Coeff > INT32_MAX) {
          return TRUE;
        }
      }
      else if (ST_class(st) != CLASS_VAR) {
        return TRUE;
      }
      else if (ST_level(st) != GLOBAL_SYMTAB) {
        if ((ST_sclass(st) != SCLASS_FORMAL && 
             ST_sclass(st) != SCLASS_FORMAL_REF) ||
            ST_level(st) != CURRENT_SYMTAB) {
          return TRUE;
        }
      }
    }
  }
  
  return FALSE;
}


//====================================================================
// loopinfo constructor
//====================================================================
LOOPINFO::LOOPINFO(MEM_POOL* m, INT16 cd_idx) 
  : _nest_level(0),
    _flags(0),
    _mem_pool(m),
    _kernel(CXX_NEW(PROJECTED_KERNEL_ARRAY(m),m))
{
  u1.u3._upper_linex = 0;
  u1.u3._lower_linex = 0;
  u1.u3._step_linex = 0;
  u1.u3._symbols = CXX_NEW(LOOP_SYMBOL_ARRAY(m), m);
  u1.u3._cd_idx = cd_idx;
}

//====================================================================
// build the LINEX information from the access vector
//====================================================================
LINEX* 
LOOPINFO::Build_linex(ACCESS_VECTOR* av)
{
  if (!av || 
      av->Too_Messy || 
      av->Delinearized_Symbol ||
      av->Contains_Non_Lin_Symb() ||
      Access_vector_is_too_messy(av)) {
    return 0;
  }

  LINEX* linex = CXX_NEW(LINEX(Mem_Pool()), Mem_Pool());
  linex->Map_access_vector(av, FALSE, NULL);
  return linex;
}

//====================================================================
// create the mapping from the do loop info which contains access
// vectors to the loop-info structure
//====================================================================
void
LOOPINFO::Map_do_loop_info(DO_LOOP_INFO_BASE *dli)
{
  // Enter LOOPINFO* to DO_LOOP_INFO_BASE* mapping into hash table
  IPL_Loopinfo_Map->Enter(this, dli);

  Set_nest_level(dli->Get_depth());

  ACCESS_ARRAY* lb = dli->Get_lb();
  ACCESS_ARRAY* ub = dli->Get_ub();
  ACCESS_VECTOR* step = dli->Get_step();

  if (lb->Num_Vec() > 1 || ub->Num_Vec() > 1 || !step->Is_Const()) {
    if (Trace_Sections)	{
      fprintf(TFile, "----LOOP has messy bounds---- \n");
      dli->Print(TFile);
      fprintf(stdout, "----LOOP has messy bounds---- \n");
      dli->Print(stdout);
    }
    Set_messy_bounds();
    return;
  }

  if (!(u1.u3._upper_linex = Build_linex(ub->Dim(0)))) {
    Set_messy_ub();
  }
  if (!(u1.u3._lower_linex = Build_linex(lb->Dim(0)))) {
    Set_messy_lb();
  }
  // Need to take the absolute value of the step, since we want 
  // all array sections to have positive steps.  Hence a loop 
  // like: 
  //    do i = n, 1, -1
  //	a(i) = 0  
  //    end do 
  // Will have a section that looks like a(1:n:1) rather than 
  // a(1:n:-1) (or a(n:1:-1) for that matter).
  INT64 const_offset = step->Const_Offset; 
  if (step->Const_Offset < 0) {
    step->Const_Offset = -step->Const_Offset; 
  }
  u1.u3._step_linex = Build_linex(step);
  step->Const_Offset = const_offset; 
}

//====================================================================
// Map an access vector to a linex
// NOTE: the ordering is IMPORTANT: the loop coeff terms are first 
// ====================================================================
void
LINEX::Map_access_vector(ACCESS_VECTOR *av,
			 BOOL Is_LNO,
			 IPA_LNO_READ_FILE* IPA_LNO_File)
{
  // The precondition is that the access vector is not too messy for mapping
  Is_True(!Access_vector_is_too_messy(av),
          ("Messy access vectos can't be mapped into LINEX"));
  
  BOOL processed = FALSE;

  for (INT i = 0; i < av->Nest_Depth(); ++i) {
    if (av->Loop_Coeff(i) != 0) {
      Set_term(LTKIND_LINDEX, (COEFF)av->Loop_Coeff(i), i, 0);
      processed = TRUE;
    }
  }

  if (av->Const_Offset != 0) {
    Set_term(LTKIND_CONST, (COEFF)av->Const_Offset, CONST_DESC, 0);
    processed = TRUE;
  }

  if (av->Contains_Lin_Symb()) {

    INTSYMB_ITER iter(av->Lin_Symb);
    for (INTSYMB_NODE* cur = iter.First(); 
         !iter.Is_Empty(); 
         cur = iter.Next()) {

      // linear symbols must be named constants, globals or formals
      ST* st = cur->Symbol.St();

      if (ST_class(st) == CLASS_CONST) {
        INT64 const_value = Targ_To_Host(STC_val(st));
        Set_term (LTKIND_CONST, (COEFF)const_value*cur->Coeff, CONST_DESC, 0);
          processed = TRUE;
      }
      else {
        Is_True(ST_class(st) == CLASS_VAR,
                ("LINEX::Map_access_vector(): Unexpected ST_clas"));

        IVAR ivar;
        
        if (ST_IDX_level(ST_st_idx(st)) == GLOBAL_SYMTAB) {
          new (&ivar) IVAR(st, cur->Symbol.WN_Offset(), cur->Symbol.Type);
        }
        else {
          UINT32 pos = Formal_Position(st);
          new (&ivar) IVAR(pos, cur->Symbol.WN_Offset(), cur->Symbol.Type);
        }
        
        // lookup this IVAR, and add it if not found
        INT32 ivar_index = -1; 
        if (!Is_LNO) { 
	  ivar_index = Get_ivar(*Ivar, ivar);

	  if (ivar_index == -1) { 
	    Ivar->AddElement(ivar);
	    ivar_index = Ivar->Lastidx();
          } 
        } else {  
    	  INT i;

	  for (i = 0; i < IPA_LNO_File->Ivar_Count(); i++) 
            if (*(IPA_LNO_File->Ivar(i)) == ivar)
	      break; 

	  if (i < IPA_LNO_File->Ivar_Count()) { 
	    ivar_index = i;
          } else { 
	    ivar_index = IPA_LNO_File->Add_Translated_Ivar(ivar);
	  } 
        } 

        Set_term(LTKIND_IV, cur->Coeff, ivar_index, 0);
        processed = TRUE;
      } 
    }
  }

  // if no terms have been added in
  if (!processed) {
    Set_term(LTKIND_CONST, (COEFF)av->Const_Offset, CONST_DESC, 0);
  }
}

//=================================================================
// number of loop coefficient terms
//=================================================================
INT
LINEX::Num_loop_coeff_terms()
{
  INT num_loop_coeff = 0;
  for (INT i = 0; i <= Num_terms(); ++i) {
    if (Get_term(i)->Get_type() == LTKIND_LINDEX) {
      ++num_loop_coeff;
    }
  }
  return num_loop_coeff;
}

//=================================================================
// check if the coeff terms are the same
//=================================================================
BOOL
LINEX::Loop_coeff_terms_equal(LINEX *kernel_linex)
{
  INT i;
  INT num_coeffs = Num_loop_coeff_terms();
  INT num_coeffs_kernel = kernel_linex->Num_loop_coeff_terms();

  if (num_coeffs != num_coeffs_kernel)
    return FALSE;
  
  for (i=0; i < num_coeffs; ++i)
    {
      TERM *kernel_term = kernel_linex->Get_term(i);
      TERM *term = Get_term(i);
      FmtAssert((term->Get_type() == LTKIND_LINDEX), 
        ("Expecting a term with kind == LTKIND_LINDEX"));
      if (term->Get_coeff() != kernel_term->Get_coeff()
	  || term->Get_desc() != kernel_term->Get_desc())
	return FALSE;
    }

  // if we reach here then the coefficients of these terms are the same
  return TRUE;
}

//====================================================================
// check to see if it has a term with this particular loop coefficient
//====================================================================
BOOL 
LINEX::Has_loop_coeff(INT depth)
{
  INT i;

  for (i=0; i <= _larray.Lastidx(); ++i)
    {
      TERM *t = &(_larray)[i];
      if ((t->Get_type() == LTKIND_LINDEX) &&
	(t->Get_desc() == depth))
	return TRUE;
    }
  return FALSE;
}


//===================================================================
// the kernel contains an array of LINEX structures, one for each
// dimension of the array 
//===================================================================
LINEX_ARRAY*
PROJECTED_REGION::Map_to_linex_array()
{
  MEM_POOL* m = Mem_Pool();
  LINEX_ARRAY* l = CXX_NEW(LINEX_ARRAY(m), m);
  PROJECTED_ARRAY* p = Get_projected_array();
   
  for (INT i = 0; i < p->Elements(); ++i) {
    PROJECTED_NODE* node = &(*p)[i];
    FmtAssert(node->Is_unprojected(), ("Node has been projected\n"));
    FmtAssert(!node->Is_messy_lb(), ("Messy lower bound\n"));
    
    // add a linex and copy into it
    LINEX* to = &(*l)[l->Newidx()];
    new (to) LINEX(m);
    node->Get_lower_linex()->Copy(to);
  } 

  return l;
}

//==========================================================================
// DESCR:
// Add_coupled_terms, add the coupled terms to the linex
//==========================================================================
void
LINEX::Add_coupled_terms(LINEX* from)
{
  for (INT i = 0; i<=from->Num_terms(); ++i) {
    TERM* term = from->Get_term(i);
    switch (term->Get_type()) {
      case LTKIND_NONE:
      case LTKIND_CONST:
      case LTKIND_LINDEX:
      case LTKIND_IV:
        break;
      case LTKIND_SUBSCR:
        Set_term(term);
        break;
      default:
        Fail_FmtAssertion("Unknown term type encountered \n");
        break;
    }
  }
}

//====================================================================
// Set_to_kernel_image, create the projected node which includes
// the offset
//====================================================================
void
PROJECTED_NODE::Set_to_kernel_image(PROJECTED_NODE* pn_kernel, 
				    LINEX* lx_offset)
{
  LINEX* lx_lower = pn_kernel->Get_lower_linex();
  LINEX* lx_upper = pn_kernel->Get_upper_linex(); 
  LINEX* lx_step = pn_kernel->Get_step_linex(); 

  if (lx_offset != NULL) {
    LINEX* lx_new_lower = lx_lower->Merge(lx_offset);
    Get_lower_linex()->Free_terms();
    Set_lower_linex(lx_new_lower);
    if (lx_upper->Num_terms() >= 0) {
      LINEX* lx_new_upper = lx_upper->Merge(lx_offset);
      Get_upper_linex()->Free_terms();
      Set_upper_linex(lx_new_upper);
    }
  } 
  else { 
    Get_lower_linex()->Free_terms();
    Get_upper_linex()->Free_terms();
    lx_lower->Copy(Get_lower_linex());
    lx_upper->Copy(Get_upper_linex());
  }
  Get_step_linex()->Free_terms();
  lx_step->Copy(Get_step_linex());

  if (pn_kernel->Get_segment_length_linex() &&
      pn_kernel->Get_segment_stride_linex()) {
    MEM_POOL* apool = Mem_Pool();
    if (Get_segment_length_linex()) {
      Get_segment_length_linex()->Free_terms();
    } 
    else {
      Set_segment_length_linex(CXX_NEW(LINEX(apool), apool));
    }
    if (Get_segment_stride_linex()) {
      Get_segment_stride_linex()->Free_terms();
    }
    else {
      Set_segment_stride_linex(CXX_NEW(LINEX(apool), apool));
    }
    pn_kernel->Get_segment_length_linex()->Copy(Get_segment_length_linex());
    pn_kernel->Get_segment_stride_linex()->Copy(Get_segment_stride_linex());
  }

  if (pn_kernel->Is_unprojected())
    Set_unprojected();
  else 
    Reset_is_unprojected();
} 

//====================================================================
// project this region onto the loop
//
// This function handles the region projection.  For uncoupled 
// subscript, the dimensions that are already ranges need not 
// be changed.
// For instance: 
//    A(1:I,I) after projection on 1<=I<=n becomes 
//             {A(1:d,d),1<=d<=n}
//
// For coupled subscript, the dimensions that are already ranges
// will have to be splitted if the loop index of the projection
// appears in the range expressions.
// For instance:
//    A(I:I+m,J) after projection on 1<=I<=n becomes 
//               {A(min(1,1+m):max(m+1,m+n),J)}
// and there might be some problems such as the stride of I etc.
// The key issue is that after the projection, the region may not be
// a convex set (there are holes in the set and the stride may not be
// regular).  
// On the other hand, if 'I' also occurs in another dimension
//    A(I:I+m,I) then the projection is the same as the first case:
//    {A(d:d+m,d),1<=d<=n}
// The key is that the access matrix cannot be singular.  If it is not,
// the integer lattice theory based on HNF comes to rescue.
//
//====================================================================
void
PROJECTED_REGION::Project(INT depth, LOOPINFO* loop_info)
{
  if (Is_messy_region()) {
    return;
  }
  
  // if it is independent of the loop, there is nothing to project away
  PROJECTED_KERNEL* p = Get_projected_kernel();
  FmtAssert(p, ("Null projected kernel encounterd\n"));
  if (p->Is_independent(depth)) {
    return;
  }

  // check if the kernel has already been projected on the current loop
  if (p->Get_projected_level() > depth) {
    p->Project(depth, loop_info);
  }
 
  if (p->Get_region() && p->Get_region()->Is_messy_region()) {
    Set_messy_region();
    return;
  }
  
  // after the kernel has been projected, copy the kernel and
  // adjust to account for the constant offset
  PROJECTED_ARRAY* array = Get_projected_array();
  
  for (INT i = 0; i < Get_num_dims(); ++i) {
    
    PROJECTED_NODE* node = &(*array)[i];
      
    // for each dimension check if we need to update the projected node 
    // this is only necessary if the subscript expression contains the 
    // loop index variable
    // if unprojected then check the lower bound only
    // reset unprojected flag if we do project things away
    if (node->Get_lower_linex()->Has_loop_coeff(depth) ||
        (!Is_unprojected_region() &&
         node->Get_upper_linex()->Has_loop_coeff(depth))) {
      PROJECTED_NODE* pnode = p->Get_region()->Get_projected_node(i);
      node->Set_to_kernel_image(pnode, p->Get_Difference(i));
      if (p->Get_Difference(i)) {
        p->Get_Difference(i)->Free_terms();
      }
    }
  }
  
  if (p->Get_Difference()) {
    p->Get_Difference()->Resetidx();
  }
  
  return;
}

//===================================================================
// return the constant term of a lower linex
//===================================================================
INT
PROJECTED_NODE::Get_constant_term()
{
  LINEX* lower = Get_lower_linex();
  for (INT i=0; i<lower->Num_terms(); ++i)
    {
      TERM *term = lower->Get_term(i);
      if (term->Get_type() == LTKIND_CONST)
	return term->Get_coeff();
      
    }
  return 0;
}

//===================================================================
// build a linex according to line 'i' of 'soe'
// using the symbols in sym for linear terms.
// dim = dimension of the system
// depth = number of enclosing loops
// which_array  is used for coefficients
// which_array = 0 (Work), = 1 (Eq) = 2 (Le)
// is_lower_bound
//===================================================================
void
LINEX::Map_from_SOE(const SYSTEM_OF_EQUATIONS* soe, 
                    INT i,
		    const LOOP_SYMBOL_ARRAY* syms,
		    INT depth,  
                    INT dim,
		    INT which_array,
		    BOOL is_lower_bound)
{
  switch (which_array) {
    case 0: {
      INT k;
      INT32 sign = is_lower_bound ? 1 : -1;

      // loop index terms
      for (k = 0; k < depth; ++k) {
        if (soe->Work(i, dim+k) != 0)
          Set_term(LTKIND_LINDEX, sign * soe->Work(i, dim+k), k, 0);
      }

      // constant term
      INT64 Const_Offset = -sign * soe->Work_Const(i);
      Set_term(LTKIND_CONST, (COEFF)Const_Offset, CONST_DESC, 0);

      // IVAR terms
      for (INT iter = 0; 
           k+dim < soe->Num_Vars() && iter < syms->Elements();
           ++k, ++iter) {
        if (soe->Work(i, k+dim)) {
          Set_term(LTKIND_IV, 
                   sign * soe->Work(i,k+dim), 
                   (*syms)[iter].Ivar_Index(), 0);
        } 
      }
      break;
    }
      
    case 1: {
      const IMAT& aeq = soe->Aeq();
      const INT64* beq = soe->Beq();
      INT k;
      
      // Must be lower_bound
      for (k = 0; k < depth; ++k) {
        if (aeq(i, dim+k) != 0)
          Set_term(LTKIND_LINDEX, -aeq(i, dim+k), k, 0);
      }

      INT64 Const_Offset = beq[i];
      Set_term(LTKIND_CONST, (COEFF)Const_Offset, CONST_DESC, 0);

      for (INT iter=0; 
           k+dim < soe->Num_Vars() && iter < syms->Elements();
           ++k, ++iter)	{
        if (aeq(i,k+dim)) {
          Set_term(LTKIND_IV, aeq(i, k+dim), (*syms)[iter].Ivar_Index(), 0);
        }
      } 
      break;
    }

    case 2: {
      const IMAT& ale = soe->Ale();
      const INT64* ble = soe->Ble();
      INT k;
      INT32 sign = is_lower_bound ? 1 : -1;

      for (k = 0; k < depth; ++k) {
        if (ale(i, dim+k) != 0)
          Set_term(LTKIND_LINDEX, sign * ale(i, dim+k), k, 0);
      }

      INT64 Const_Offset = -sign * ble[i];
      Set_term(LTKIND_CONST, (COEFF)Const_Offset, CONST_DESC, 0);
        
      for (INT iter = 0; 
           k+dim < soe->Num_Vars() && iter < syms->Elements();
           ++k, ++iter) {
        if (ale(i, k+dim)) {
          Set_term(LTKIND_IV, 
                   sign * ale(i, k+dim), 
                   (*syms)[iter].Ivar_Index(), 
                   0);
        }
      }
      break;
    }
     
    default:
      Fail_FmtAssertion("Illegal Which Array \n");
      break;
  }
}

//-----------------------------------------------------------------------
// NAME: LINEX::Substitute_Lindex
// FUNCTION: Substitute the value of 'lx_substitute' into the LINDEX for 
//   all occurrences of loop index number 'lindex'.
//-----------------------------------------------------------------------

void LINEX::Substitute_Lindex(INT lindex, 
			      LINEX* lx_substitute)
{
  INT lindex_coeff = 0;
  LINEX lx_temp(_larray.Get_Mem_Pool());
  INT freeze_num_terms = Num_terms();
  INT i;

  for (i = 0; i <= freeze_num_terms; i++) { 
    TERM* tm = Get_term(i); 
    if (tm->Get_type() == LTKIND_LINDEX && tm->Get_desc() == lindex) {
      lindex_coeff += tm->Get_coeff();
    } else {
      lx_temp.Set_term(tm);
    }
  }
  Free_terms();
  for (i = 0; i <= lx_temp.Num_terms(); i++)
    Set_term(lx_temp.Get_term(i));
  if (lindex_coeff == 0)
    return;
  for (i = 0; i <= lx_substitute->Num_terms(); i++) {
    TERM* tm = lx_substitute->Get_term(i);
    Set_term(tm->Get_type(), tm->Get_coeff() * lindex_coeff, tm->Get_desc(),
      tm->Get_projected_level());
  }
  Simplify();
} 
			
//==================================================================
// set the region according to the SOE, pivot_row and stride
//===================================================================
void
PROJECTED_REGION::Set_region(SYSTEM_OF_EQUATIONS* soe,  
                             LOOP_SYMBOL_ARRAY* syms, 
			     INT strides[], 
                             INT pivot_row, 
                             INT pos,
			     INT loop_step, 
                             INT projected_axle)
{
  FmtAssert(soe, ("NULL SOE pointer passed to Set_Region"));

  if (Trace_Sections) { 
    fprintf(stdout, "PROJECTED_REGION::Set_region() BEGIN\n");
    fprintf(stdout, "pivot_row = %d\n", pivot_row);
    fprintf(stdout, "pos = %d\n", pos);
    fprintf(stdout, "loop_step = %d\n", loop_step);
    fprintf(stdout, "projected_axle = %d\n", projected_axle);
    fprintf(TFile, "PROJECTED_REGION::Set_region() BEGIN\n");
    fprintf(TFile, "pivot_row = %d\n", pivot_row);
    fprintf(TFile, "pos = %d\n", pos);
    fprintf(TFile, "loop_step = %d\n", loop_step);
    fprintf(TFile, "projected_axle = %d\n", projected_axle);
  } 

  Set_type(NON_MESSY_REGION);

  // set the projected array, i.e. alloc memory for it
  PROJECTED_ARRAY* array = Get_projected_array();
  if (!array) {
    array = CXX_NEW(PROJECTED_ARRAY(Mem_Pool()), Mem_Pool());
    Set_projected_array(array);
  }

  // now create numdim entries of the projected array
  INT num_dims = Get_num_dims();
  INT depth = Get_depth();
  array->Force_Alloc_array(num_dims);
  array->Setidx(num_dims-1);
  for (INT ii = 0; ii < num_dims; ++ii) {
    (*array)[ii].Init(Mem_Pool());
    (*array)[ii].Set_unprojected();
  }

  // Set the stride of the pivoting axle
  // this is the variable that we are interested in, it is in this
  // particular column in the system of equations
  INT pivot_column = num_dims+pos;
  INT multiple = -soe->Aeq()(pivot_row, pivot_column);
  INT i;

  for (i = 0; i < num_dims; ++i) {
    if (soe->Aeq()(pivot_row,i) != 0) {
      strides[i] = multiple*loop_step;
      break;
    }
  }

  {
    BIT_VECTOR *ancestor_lo = CXX_NEW(BIT_VECTOR(num_dims,
      Array_Summary.Get_local_pool()), Array_Summary.Get_local_pool());
    BIT_VECTOR *ancestor_up = CXX_NEW(BIT_VECTOR(num_dims,
      Array_Summary.Get_local_pool()), Array_Summary.Get_local_pool());

    // Set the equality axles first
    INT k = 0;
    for (i = 0; i < soe->Num_Eq_Constraints(); ++i) {
      if (i != pivot_row) {
	for (; k < num_dims; ++k) {
	  if (k != projected_axle && soe->Aeq()(i,k) != 0) {
	    (*array)[k].Set_linex_eq(soe, i, k, syms, depth, num_dims, 
                                     strides[k]);
	    ancestor_lo->Set(k);
	    ancestor_up->Set(k);
	    break;
	  }
        }
      }
    }
  
    // Filling the axles according to the topologic order (dependence)
    BOOL progress = TRUE;
    while (progress && 
           ((ancestor_up->Pop_Count() != num_dims) ||
            (ancestor_lo->Pop_Count() != num_dims))) {
      progress = FALSE;
      for (i = 0; i < soe->Num_Le_Constraints(); ++i) {
	INT axle = -1;
	for (k = 0; k < num_dims; ++k) {
	  if (soe->Ale()(i,k) < 0 && !ancestor_lo->Test(k) ||
	      soe->Ale()(i,k) > 0 && !ancestor_up->Test(k)) {
	    if (axle>=0) {
	      axle = -1;
	      break;
	    } 
            else {
	      axle = k;
            }
	  }
        }
        
	if (axle >= 0) { 

          // Found one that only depends on the already converted axles.
	  progress = TRUE;
	  if (soe->Ale()(i,axle) < 0) {
	    (*array)[axle].Set_linex_le(soe, i, axle, syms, depth, num_dims, 
                                        strides[axle]);
	    ancestor_lo->Set(axle);
	  } 
          else {
            (*array)[axle].Set_linex_le(soe, i, axle, syms, depth, num_dims, 
                                        strides[axle]);
            ancestor_up->Set(axle);
          }
	  (*array)[axle].Reset_is_unprojected();
	}
      }
    }
    if (!progress) {
      Set_messy_region();
      if (Trace_Sections) {
        fprintf(TFile, "PROJECTED_REGION::Set_Region: No progress\n");
        fprintf(stdout, "PROJECTED_REGION::Set_Region: No progress\n");
      }
    }
  } 
}

//========================================================================
//
// Determine if two inequalities are actually one equality.
// It returns TRUE if the vector sum of them is a zero vector.
//
//========================================================================
static BOOL
is_equality(const SYSTEM_OF_EQUATIONS *soe, const INT i, const INT j)
{
  for (INT k = 0; k < soe->Num_Vars(); ++k) 
    if ((soe->Work(i,k)+soe->Work(j,k)) != 0) 
      return FALSE;

  return (soe->Work_Const(i) + soe->Work_Const(j) == 0);
}

//=========================================================================
// set the linexs
// Set_Axle in LNO's code
//=========================================================================
void
PROJECTED_NODE::Set_linexs(const SYSTEM_OF_EQUATIONS *soe,
                           INT i, 
                           INT j, 
                           const LOOP_SYMBOL_ARRAY* syms,
                           INT depth, 
                           INT dim,
                           INT stride)
{
  INT k;
  BOOL Has_subscr_ltkind = FALSE;

  // reset the node, clear all fields and existing linex structures
  Reset_node();

  LINEX* lower = Get_lower_linex();
  LINEX* upper = Get_upper_linex();
  LINEX* step = Get_step_linex();

  step->Set_term(LTKIND_CONST, abs(stride), CONST_DESC, 0);

  // Two inequalities form an equality
  if (is_equality(soe, i, j)) {
    lower->Map_from_SOE(soe, i, syms, depth, dim, 0, TRUE);

    // search for LTKIND = LTKIND_SUBSCR
    for (k = 0; k < dim; ++k) {
      if (soe->Work(i,k) != 0 && 2*k != i) {
	Has_subscr_ltkind = TRUE;
	break;
      }
    }

    if (Has_subscr_ltkind) {
      for (k = 0; k<dim; ++k) {
        if (soe->Work(i,k) != 0) {
          lower->Set_term(LTKIND_SUBSCR, soe->Work(i,k),k,0);
        }
      }
    }
    
    return;
  }
  
  // Set the lower and upper bounds
  lower->Map_from_SOE(soe, i, syms, depth,dim, 0, TRUE);

  for (k = 0; k < dim; ++k) {
    if (soe->Work(i,k) != 0 && 2*k != i) {
      Has_subscr_ltkind = TRUE;
      break;
    }
  }

  if (Has_subscr_ltkind) {
    for (k = 0 ; k < dim; ++k) {
      if (soe->Work(i,k)) {
        lower->Set_term(LTKIND_SUBSCR, soe->Work(i,k),k,0);
      }
    }
  }

  // reset this flag for use by upper bound
  Has_subscr_ltkind = FALSE;

  upper->Map_from_SOE(soe, i, syms, depth, dim, 0, FALSE);
  
  for (k = 0; k < dim; ++k) {
    if (soe->Work(i,k) != 0 && 2*k != i) {
      Has_subscr_ltkind = TRUE;
      break;
    }
  }

  if (Has_subscr_ltkind) {
    for (k = 0; k < dim; ++k) {
      if (soe->Work(i,k)) {
        upper->Set_term(LTKIND_SUBSCR, soe->Work(i,k), k, 0);
      }
    }
  }
}



//========================================================================
// set the linexs
// Set_Axle_Eq in LNO's code
//=========================================================================  
void 
PROJECTED_NODE::Set_linex_eq(const SYSTEM_OF_EQUATIONS *soe,
                             INT i, 
                             INT j, 
                             const LOOP_SYMBOL_ARRAY *syms,
                             INT depth, 
                             INT dim,
                             INT stride)
{
  INT k;

  if (Trace_Sections) {
    fprintf(TFile, "Entered set_linex_eq: \n");
    fprintf(stdout, "Entered set_linex_eq: \n");
  }
  
  // reset the node, clear all fields and existing linex structures
  Reset_node();
  Set_unprojected();

  // map step to the constant linex value of stride
  LINEX* step = Get_step_linex();
  step->Set_term(LTKIND_CONST, abs(stride),  CONST_DESC, 0);
  if (Trace_Sections) {
    fprintf(TFile, "i = %d, depth = %d, dim = %d \n", i, depth, dim);
    fprintf(stdout, "i = %d, depth = %d, dim = %d \n", i, depth, dim);
  }

  LINEX* lower = Get_lower_linex();
  if (Trace_Sections) {
    fprintf(stdout, "lower linex before mapping is\n");
    lower->Print(stdout);
    fprintf(TFile, "lower linex before mapping is\n");
    lower->Print(TFile);
  }

  lower->Map_from_SOE(soe, i, syms, depth, dim, 1, TRUE);  

  // Are there any  LTKIND_SUBSCR terms
  BOOL has_coupled_subscript_terms = FALSE;
  for (k = 0; k < dim; ++k) {
    if (soe->Aeq()(i,k) && k != j) {
      has_coupled_subscript_terms = TRUE;
      break;
    }
  }

  if (has_coupled_subscript_terms) {
    for (k=0; k< dim; ++k) {
      if (soe->Aeq()(i,k) && k != j) {
        lower->Set_term(LTKIND_SUBSCR, -soe->Aeq()(i,k), k, 0);
      }
    }
  }

  if (Trace_Sections) {
    fprintf(TFile, "lower linex after mapping is\n");
    lower->Print(TFile);
    fprintf(stdout, "lower linex after mapping is\n");
    lower->Print(stdout);
  }
}

//=========================================================================
// Set_Linex_Le
// in LNOs code, Set_Axle_Le
//=========================================================================
void
PROJECTED_NODE::Set_linex_le(const SYSTEM_OF_EQUATIONS *soe,
                             INT i, 
                             INT j, 
                             const LOOP_SYMBOL_ARRAY *syms,
                             INT depth, 
                             INT dim,
                             INT stride)
{
  INT k;

  // map step to the constant linex value of stride
  LINEX* step = Get_step_linex();
  FmtAssert(step,  ("Set_linex_le: Null  step  encountered\n"));
  step->Free_terms();
  step->Set_term(LTKIND_CONST, abs(stride),  CONST_DESC, 0);
  
  // Set the lower bound
  if (soe->Ale()(i,j)<0) {
    if (Trace_Sections) {
      fprintf(TFile, "Set_linex_le: setting the lower bound\n");
      fprintf(stdout, "Set_linex_le: setting the lower bound\n");
    }
    
    LINEX* lower = Get_lower_linex();      
    FmtAssert(lower, ("Set_linex_le: Null lower encountered\n"));
    lower->Free_terms();
    lower->Map_from_SOE(soe, i, syms, depth, dim, 2, TRUE);

    BOOL Has_subscr_ltkind = FALSE;
    for (k = 0; k<dim; ++k) {
      if (soe->Ale()(i,k) != 0 && k != j) {
        Has_subscr_ltkind = TRUE;
        break;
      }
    }
	  
    if (Has_subscr_ltkind) {
      for (k = 0; k < dim; ++k) {
        if (soe->Ale()(i,k) && k != j) {
          lower->Set_term(LTKIND_SUBSCR, soe->Ale()(i,k), k, 0);
        }
      }
    }
  }
  
  else {
    if (Trace_Sections) {
      fprintf(TFile, "Set_linex_le: setting the upper bound\n");
      fprintf(stdout, "Set_linex_le: setting the upper bound\n");
    }

    LINEX* upper = Get_upper_linex();
    FmtAssert(upper, ("Set_linex_le: Null upper encountered\n"));
    upper->Free_terms();
    upper->Map_from_SOE(soe, i, syms, depth,dim, 2, FALSE);
	
    BOOL Has_subscr_ltkind = FALSE;
    for (k = 0; k<dim; ++k) {
      if (soe->Ale()(i,k) != 0 && k != j) {
        Has_subscr_ltkind = TRUE;
        break;
      }
    }
    
    if (Has_subscr_ltkind ) {
      for (k = 0; k < dim; ++k) {
        if (soe->Ale()(i,k) && k != j) {
          upper->Set_term(LTKIND_SUBSCR, -soe->Ale()(i,k), k, 0);
        }
      }
    }
  }
}


//====================================================================
// create a PROJECTED_REGION from an access array
//====================================================================
PROJECTED_REGION::PROJECTED_REGION(ACCESS_ARRAY* array, 
				   MEM_POOL* m, 
                                   LOOPINFO* loop, 
                                   BOOL in_ipl,
				   IPA_LNO_READ_FILE* IPA_LNO_File) 
{
  mUINT8 num_dim = array->Num_Vec();
  mUINT8 depth = loop ? loop->Get_nest_level()+1 : 0;
  INT i;

  // initialize with defaults for unknown array regions
  Set_projected_array(NULL);
  Set_type(MESSY_REGION);
  Set_num_dims(num_dim);
  Set_depth(depth);
  Set_projected_kernel(NULL);
  Set_Mem_Pool(m);

  // enter PROJECTED_REGION* to ACCESS_ARRAY* mapping into hash table
  if (in_ipl) {
    IPL_Access_Array_Map->Enter(this, array);
  }

  // check for the cases that result in a messy region
  if (!array || array->Too_Messy) {
    return;
  }
  
  for (i = 0; i < num_dim; ++i) {
    if (Access_vector_is_too_messy(array->Dim(i))) {
      return;
    }
  }

  // NOW we have a non-messy region  
  // set this region as unprojected when we first create it
  // when we project it then we will set it to FALSE;
  Set_type(NON_MESSY_REGION);
  Set_unprojected();
  Set_projected_array(CXX_NEW(PROJECTED_ARRAY(m), m));
  Get_projected_array()->Force_Alloc_array(num_dim);
  Get_projected_array()->Setidx(num_dim-1);

  for (i = 0; i < num_dim; ++i) {
    PROJECTED_NODE* pn = Get_projected_node(i);
    pn->Init(m);
    pn->Get_lower_linex()->Map_access_vector(array->Dim(i), !in_ipl, 
      IPA_LNO_File);
    pn->Set_unprojected();
  }

  // if the loop is not found, leave it unprojected
  if (!loop) {
    return;
  }

  // update kernel information; add a kernel to this region
  BOOL match = TRUE;
  PROJECTED_KERNEL_ARRAY* kernels = loop->Get_kernels();
  for (i = 0; i <= kernels->Lastidx(); ++i) {

    PROJECTED_KERNEL* kernel = &(*kernels)[i];
    INT j;

    // check depth, number of dimensions, loop coeffs
    if (kernel->Get_depth() != depth ||
        kernel->Get_num_dims() != num_dim) {
      continue;
    }
    
    for (j = 0; j < num_dim; ++j) {
      // check to see if the lower linex matches up 
      // with each LINEX in the projected kernel
      LINEX* lower = Get_projected_node(j)->Get_lower_linex();
      LINEX* kernel_linex = kernel->Get_linex(j);
      if (!lower->Loop_coeff_terms_equal(kernel_linex)) {
        break;
      }
    }

    if (j != num_dim) {
      match = FALSE;
    }
    
    if (match) {  
      kernel->Set_Difference(this);
      Set_projected_kernel(kernel);
      break;
    }
  }

  // if the kernel has not been found, then create one and add it to
  // the list of kernels for the current loop nest
  if (Get_projected_kernel() == NULL) {
    PROJECTED_KERNEL* k = &(*kernels)[kernels->Newidx()];
    k->Init(this, loop);
    Set_projected_kernel(k);
  }
}




//===================================================================
// compare 2 regions, 
// 0 ---- cannot compare
// 1 ---- a <= b 
// 2 ---- b <= a
// 3 ---- a == b
//===================================================================
INT
PROJECTED_REGION::Compare(PROJECTED_REGION *b)
{
  INT i;
  INT result = 0;

  if (Trace_Sections)
    {
      fprintf(TFile,"Compare two PROJECTED REGIONs \n");
      b->Print(TFile);
      Print(TFile);
      fprintf(stdout,"Compare two PROJECTED REGIONs \n");
      b->Print(stdout);
      Print(stdout);
    }

  // First, the simple tests.
  if (Get_type() != b->Get_type())
    return 0;

  if (Get_num_dims() != b->Get_num_dims())
    return 0;

  // check if they are equivalent unprojected regions
  PROJECTED_ARRAY *array_a = Get_projected_array();
  PROJECTED_ARRAY *array_b = b->Get_projected_array();

  for (i=0; i< Get_num_dims(); ++i) 
    {
      PROJECTED_NODE *node_a, *node_b;
      node_a = &(*array_a)[i];
      node_b = &(*array_b)[i];
      if (!node_a->Equivalent(*node_b))
	return 0;
    }

  MEM_POOL *local_pool = Array_Summary.Get_local_pool();
  MEM_POOL_Push(local_pool);
  {
    // Build the SOE to find the relations.
    SYSTEM_OF_EQUATIONS* soe = 
      CXX_NEW(SYSTEM_OF_EQUATIONS(0, 0, Get_num_dims()+Get_depth(), 
                                  local_pool), local_pool);
    LOOP_SYMBOL_ARRAY* syms = 
      CXX_NEW(LOOP_SYMBOL_ARRAY(local_pool), local_pool);

    // An array to hold the relation of each axle of 'a' and 'b'
    // relations[i] = 3  a._axle[i] == b._axle[i]
    //              = 1  a._axle[i] <  b._axle[i]
    //              = 2  a._axle[i] >  b._axle[i]
    //              = 0  relation unknown
    for (i = 0; i < Get_num_dims(); ++i) {
      Add_to_SOE(this, i, soe, TRUE, syms, Get_depth(), Trace_Sections);
      Add_to_SOE(b, i, soe, TRUE, syms, Get_depth(), Trace_Sections);
    }

    if (!soe->Copy_To_Work()) goto return_point;
    { 
    INT16 * lower = CXX_NEW_ARRAY(INT16, Get_num_dims(), local_pool);
    INT16 * upper = CXX_NEW_ARRAY(INT16, Get_num_dims(), local_pool);

    for (i = 0; i < Get_num_dims(); ++i) 
      {
	lower[i] = soe->Simple_Redundant(4*i, 4*i+2);
	upper[i] = soe->Simple_Redundant(4*i+1, 4*i+3);
      }

    // First test if they are all the same
    for (i = 0; i < Get_num_dims(); ++i) {
      if (lower[i]!=3 || upper[i]!=3) 
	break;
    }

    if (i==Get_num_dims()) {
      CXX_DELETE(lower, local_pool);
      CXX_DELETE(upper, local_pool);
      result = 3;
      goto return_point;
    }

    BOOL exists_1 = FALSE;
    BOOL exists_2 = FALSE;

    for (; i < Get_num_dims(); ++i) {
      exists_1 = (exists_1 || lower[i]==1 || upper[i]==1);
      exists_2 = (exists_2 || lower[i]==2 || upper[i]==2);
      if (exists_1 && exists_2) {
	CXX_DELETE(lower, local_pool);
	CXX_DELETE(upper, local_pool);
	result = 0;
	goto return_point;
      }
    }

    CXX_DELETE(lower, local_pool);
    CXX_DELETE(upper, local_pool);
    
    // Now, they are some undecided axles.  Try to use the more powerful
    // (expensive) redundancy identification technique.
    BOOL redundant_1 = FALSE;
    BOOL redundant_2 = FALSE;

    if (!exists_2) redundant_1 = soe->Prove_Redundant(0,Get_num_dims());
    if (redundant_1 && exists_1) {
      result = 2;
      goto return_point;
    }

    if (!exists_1) redundant_2 = soe->Prove_Redundant(1,Get_num_dims());
    if (redundant_2 && exists_2) {
      result = 1;
      goto return_point;
    }

    if (redundant_1 && redundant_2) {
      result = 3;
      goto return_point;
    }
  } 
  return_point:
    CXX_DELETE(syms, local_pool);
    CXX_DELETE(soe, local_pool);

  }
  MEM_POOL_Pop(local_pool);

  return result;

}

//===================================================================
// Add a linex to the system of equations
// for UB and LB, similar to Add_Access
// The system of equations is setup as:
// A*X + B*Y + C*Z <= b
// X is a vector representing dimensions of the region
// Y is a vector representing the loop indices
// Z is a list of the symbolic variables in the linear term
// b is a vector of constant offsets
//===================================================================
void 
LOOPINFO::Add_bound(LINEX *l, 
                    SYSTEM_OF_EQUATIONS *soe,
		    mUINT8 depth, 
                    INT num_dim,  
		    INT num_syms, 
		    LOOP_SYMBOL_ARRAY* sym)
{
  INT pos, c = 0;

  if (Trace_Sections) {
    fprintf(TFile, "\n Add_bound: Adding a LINEX to the SOE\n");
    l->Print(TFile);
    fprintf(stdout, "\n Add_bound: Adding a LINEX to the SOE\n");
    l->Print(stdout);
  }

  // the size of the vector is = # of dimensions (for coupled
  // subscript values) + # of symbolic constants + # of loop indices + 1
  INT vector_size = num_dim + depth + num_syms;
  if (Trace_Sections) {
    fprintf(TFile, "num_dim = %d, depth = %d, num_syms = %d vector size %d \n", 
           num_dim, depth, num_syms,vector_size);
    fprintf(stdout,"num_dim = %d, depth = %d, num_syms = %d vector size %d \n", 
           num_dim, depth, num_syms,vector_size);
  }

  mINT32* v = (mINT32*) alloca(sizeof(mINT32) * vector_size);
  bzero (v, sizeof(mINT32)*vector_size);
  
   // for each term store the coeff in the right place in the
  // vector
  for (INT i=0; i<= l->Num_terms(); ++i) {
    TERM* term = l->Get_term(i);
    switch (term->Get_type()) {
      case LTKIND_CONST:
        c = term->Get_coeff();
        break;

      case LTKIND_LINDEX:
        // store the coefficient in the appropriate place
        v[num_dim+term->Get_desc()] = term->Get_coeff();
        break;

      case LTKIND_IV:
        pos = Locate_symbol(sym, soe, LOOP_SYMBOL(term->Get_desc()));
        v[num_dim+depth+pos] = term->Get_coeff();
        break;

      case LTKIND_SUBSCR:
        Fail_FmtAssertion("Add_bound:: LTKIND_SUBSCR not supported\n");
        break;
	  
      case  LTKIND_NONE:
        Fail_FmtAssertion("Add_bound:: unknown term kind LTKIND_NONE\n");
        break;
    }
  }
  
  if (Trace_Sections) {
    int k;
    fprintf(TFile, "vector size = %d \n", vector_size);

    for (k=0; k<vector_size; ++k)
      fprintf (TFile, "v[%d] = %d \n", k, v[k]);

    fprintf(stdout, "vector size = %d \n", vector_size);

    for (k=0; k<vector_size; ++k)
      fprintf (stdout, "v[%d] = %d \n", k, v[k]);
  } 

  soe->Add_Le(v, c);

  if (Trace_Sections) {
    fprintf(TFile,"\n Add_bound: New SOE is: \n");
    soe->Print(TFile);
    fprintf(stdout,"\n Add_bound: New SOE is: \n");
    soe->Print(stdout);
  }
}

//-----------------------------------------------------------------------
// NAME: LOOPINFO::Min_value
// FUNCTION: Return a LINEX equal to the minimum value of the LOOPINFO's
//   loop index variable.  Return NULL if this is too hard to compute. 
//-----------------------------------------------------------------------

LINEX* LOOPINFO::Min_value()
{
  INT i;
  MEM_POOL* pool = Mem_Pool();

  if (Is_messy_lb())
    return NULL;

  INT lx_coeff = 0;
  LINEX* lx_lower = Get_lower_linex();

  for (i = 0; i <= lx_lower->Num_terms(); i++) {
    TERM* tm = lx_lower->Get_term(i);
    if (tm->Get_type() == LTKIND_LINDEX) {
      // Give up on trapezoidal loops for now.
      if (tm->Get_desc() != Get_nest_level())
        return NULL;
      lx_coeff += tm->Get_coeff();
    }
  }
  // No LINEX representation can include divide
  if (lx_coeff != -1)
    return NULL;

  LINEX* lx_result = CXX_NEW(LINEX(pool), pool);
  for (i = 0; i <= lx_lower->Num_terms(); i++) {
    TERM* tm = lx_lower->Get_term(i);
    if (tm->Get_type() != LTKIND_LINDEX) 
      lx_result->Set_term(tm->Get_type(), tm->Get_type() == LTKIND_CONST 
        ? -tm->Get_coeff() : tm->Get_coeff(), tm->Get_desc(),
        tm->Get_projected_level());
  }

  return lx_result;
}

//-----------------------------------------------------------------------
// NAME: LOOPINFO::Max_value
// FUNCTION: Return a LINEX equal to the maximum value of the LOOPINFO's
//   loop index variable.  Return NULL if this is too hard to compute. 
//-----------------------------------------------------------------------

LINEX* LOOPINFO::Max_value()
{
  MEM_POOL* pool = Mem_Pool();
  INT i;

  if (Is_messy_ub())
    return NULL;

  INT lx_coeff = 0;
  LINEX* lx_upper = Get_upper_linex();

  for (i = 0; i <= lx_upper->Num_terms(); i++) {
    TERM* tm = lx_upper->Get_term(i);
    if (tm->Get_type() == LTKIND_LINDEX) {
      // Give up on trapezoidal loops for now.
      if (tm->Get_desc() != Get_nest_level())
        return NULL;
      lx_coeff += tm->Get_coeff();
    }
  }

  // No LINEX representation can include divide
  if (lx_coeff != 1)
    return NULL;

  LINEX* lx_result = CXX_NEW(LINEX(pool), pool);
  for (i = 0; i <= lx_upper->Num_terms(); i++) {
    TERM* tm = lx_upper->Get_term(i);

    if (tm->Get_type() != LTKIND_LINDEX)
      lx_result->Set_term(tm->Get_type(), tm->Get_type() == LTKIND_CONST 
        ? tm->Get_coeff() : -tm->Get_coeff(), tm->Get_desc(),
        tm->Get_projected_level());
  }

  return lx_result;
}

// -------------------------------------------------------
// Check if the inner loop is nested within the outer loop
// -------------------------------------------------------
static BOOL
Is_nested_within(WN* inner, WN* outer)
{
  Is_True(WN_operator(inner) == OPR_DO_LOOP &&
          WN_operator(outer) == OPR_DO_LOOP,
          ("Is_nested_within: Both nodes must be DO loops"));

  for (WN* parent = LWN_Get_Parent(inner); 
       parent; 
       parent = LWN_Get_Parent(parent)) {
    if (parent == outer) {
      return TRUE;
    }
  }
  return FALSE;
}

//==================================================
// Return the number of loops surrounding this loop
//==================================================
static INT 
Get_surrounding_loop_count(LOOPINFO *l)
{
  INT count = 1;
  WN* current = Get_cd_by_idx(l->Get_cd_idx())->Get_wn();
#if _RELY_ON_CD_
  SUMMARY_CONTROL_DEPENDENCE* cd = Get_controlling_stmt(current);
  while (cd) {
    WN* parent = cd->Get_wn();
    if (cd->Is_do_loop() && Is_nested_within(current, parent)) {
      ++count;
      current = parent;
    }
    cd = Get_controlling_stmt(parent);
  }
#else
  Is_True(WN_operator(current) == OPR_DO_LOOP,
          ("WN for the LOOPINFO is not a OPR_DO_LOOP"));
  while (current = LWN_Get_Parent(current)) {
    if (WN_operator(current) == OPR_DO_LOOP) {
      ++count;
    }
  }
#endif
  return count;
}

//---------------------------------------------------------------
// Return the parent loop given the loopinfo of the current loop
//---------------------------------------------------------------
static LOOPINFO*
Get_parent(LOOPINFO *l)
{
 WN* wn = Get_cd_by_idx(l->Get_cd_idx())->Get_wn();
 SUMMARY_CONTROL_DEPENDENCE* cd = Get_controlling_stmt(wn);
 while (cd && !(cd->Is_do_loop() && Is_nested_within(wn, cd->Get_wn()))) {
   cd = Get_controlling_stmt(cd->Get_wn());
 }
 if (cd && cd->Is_do_loop()) {
   return Array_Summary.Get_cfg_node_array(Get_cd_idx(cd))->Get_loopinfo();
 }
 return NULL;
}


//===================================================================
// project the kernel
// given a kernel, find its region of access AFTER all the enclosing
// loops from level up are projected away
//===================================================================
void 
PROJECTED_KERNEL::Project(mUINT8 level, LOOPINFO* loop)
{
  if (_projected_level <= level ) return;
  _projected_level = level;

  MEM_POOL* local_array_pool = Array_Summary.Get_local_pool();
  
  MEM_POOL_Push(local_array_pool);
  {
    INT num_vectors = Get_num_dims();  

    // build the system of equations
    // number of variables = num of dimensions + depth?
    SYSTEM_OF_EQUATIONS* soe = 
      CXX_NEW(SYSTEM_OF_EQUATIONS(0, 0, num_vectors+_depth, local_array_pool),
              local_array_pool);

    LOOP_SYMBOL_ARRAY *syms = CXX_NEW(LOOP_SYMBOL_ARRAY(local_array_pool), local_array_pool);

    INT* strides = CXX_NEW_ARRAY(INT, num_vectors, local_array_pool);
    INT* which_axle = CXX_NEW_ARRAY(INT, num_vectors, local_array_pool);
    INT eq_count = 0;
    INT i;

    PROJECTED_REGION* region;
    // Test if the region was ever projected.
    if ((region = Get_region()) != NULL) {
      for (i = 0; i < num_vectors; ++i) {
        Add_to_SOE(region, i, soe, FALSE, syms, _depth, Trace_Sections);

        LINEX* step = region->Get_projected_node(i)->Get_step_linex();
        FmtAssert(step->Is_const(), ("Expecting a constant step"));
        strides[i] = step->Get_term(0)->Get_coeff();
        if (region->Get_projected_node(i)->Is_unprojected()) { 
          which_axle[eq_count] = i;
          eq_count++;
        }
      }
    }

    // if region has not been projected, build the SOE from the
    // linex array (which contains the set of inequalities)
    else {
      for (i = 0; i < num_vectors; ++i) {
        INT num_syms = Ivar->Elements();
        Get_linex(i)->Add_access(soe, _depth, num_vectors, i, num_syms,
                                 ACTION_EQ, syms, Trace_Sections);
        strides[i] = 1;
        which_axle[i] = i;	  
      }

      region = CXX_NEW(PROJECTED_REGION(MESSY_REGION, _depth, num_vectors, 
	Array_Summary.Get_array_pool()), Array_Summary.Get_array_pool());
      Set_region(region);
      eq_count = num_vectors; 
    }

    // set the lower and upper bounds of the loop indices that need
    // to be projected 
    INT pivot_row;
    INT step;
    INT num_do_loops = Get_surrounding_loop_count(loop);
    INT endloop = num_do_loops - level;

    LOOPINFO *cur_loop = loop;
    for (i = 0 ; i < endloop; ++i) {
      if (cur_loop->Is_messy_any_bounds()) {
        if (Trace_Sections) {
          fprintf(TFile, "Messy bounds for loop during projection");
          cur_loop->Print(TFile);
          fprintf(stdout, "Messy bounds for loop during projection");
          cur_loop->Print(stdout);
        } 
        goto pop_and_return;
      }
      else {
        LINEX *step_linex = cur_loop->Get_step_linex();

        FmtAssert(!cur_loop->Is_messy_step(), 
                  ("Project: expecting non-messy step "));
        FmtAssert(step_linex->Is_const(), 
                  ("Project: step is not a constant \n"));

        step = step_linex->Get_term(0)->Get_coeff();

        // check if any of the loop bounds are messy
        // if they are then we cannot project away anything useful
        INT num_syms = Ivar->Elements();

        if ( !cur_loop->Is_messy_lb() )
          cur_loop->Add_bound(cur_loop->Get_lower_linex(), soe,
                              _depth, num_vectors, num_syms, syms);

        if ( !cur_loop->Is_messy_ub() )
          cur_loop->Add_bound(cur_loop->Get_upper_linex(), soe, 
                              _depth, num_vectors, num_syms, syms);

        // BOOL is_inconsistent = FALSE;
	  
        if (Trace_Sections) {
          fprintf(TFile, "Num vectors = %d \n", num_vectors);
          fprintf(TFile, "Base = %d \n", num_do_loops-i-1);
          fprintf(stdout, "Num vectors = %d \n", num_vectors);
          fprintf(stdout, "Base = %d \n", num_do_loops-i-1);
        }
        pivot_row = soe->Change_Base(num_vectors,
                                     num_do_loops - i - 1,
                                     local_array_pool);

        if (Trace_Sections) {
          fprintf(TFile, "After base change, the SOE is");
          soe->Print(TFile);
          fprintf(stdout, "After base change, the SOE is");
          soe->Print(stdout);
        }

        if (pivot_row < 0) {
          if (Trace_Sections) {
            fprintf(TFile, "pivot row < 0  during projection");
            cur_loop->Print(TFile);
            fprintf(stdout, "pivot row < 0  during projection");
            cur_loop->Print(stdout);
          } 
          goto pop_and_return;
        }
      }
      cur_loop = Get_parent(cur_loop);
    }
    FmtAssert(pivot_row >= 0 && pivot_row < eq_count, 
      ("PROJECTED_KERNEL::Project(): Invalid indexing of which_axle[]"));
    region->Set_region(soe, syms, strides, 
                       pivot_row, level, step, which_axle[pivot_row]);
    
    if (Trace_Sections) {
      fprintf(TFile, "PROJECTED_KERNEL:: region generated is: \n");
      if (region) {
        region->Print(TFile);
      }
      fprintf(stdout, "PROJECTED_KERNEL:: region generated is: \n");
      if (region) {
        region->Print(stdout);
      }
    }
  }

pop_and_return:
  MEM_POOL_Pop(local_array_pool);
}


//===================================================================
// create a projected kernel given the initial region of the array
//===================================================================
void 
PROJECTED_KERNEL::Init(PROJECTED_REGION* a, LOOPINFO* loop)
{
  INT i;
  bzero(this, sizeof(PROJECTED_KERNEL));
  _mem_pool = a->Mem_Pool();
  _depth = loop->Get_nest_level() + 1;
  _projected_level = _depth + 1;
  _is_independent = CXX_NEW_ARRAY(BOOL,_depth,Array_Summary.Get_array_pool());

  for (i = 0; i < _depth; ++i) {
    _is_independent[i] = TRUE;
  }

  _array = a->Map_to_linex_array();
  _difference = CXX_NEW(LINEX_ARRAY(Array_Summary.Get_array_pool()),
                        Array_Summary.Get_array_pool());

  // go through each dimension, get the lower linex since this
  // is the one that is set given that the node has not been
  // projected away
  PROJECTED_ARRAY* p = a->Get_projected_array();
  for (i = 0; i < p->Elements(); ++i) {
    PROJECTED_NODE *node = &(*p)[i];

    FmtAssert((node->Is_unprojected()), (" Node has been projected\n"));
    FmtAssert(!(node->Is_messy_lb()), (" Messy lower bound\n"));

    LINEX* lower =  node->Get_lower_linex();
    for (INT j = 0; j <= lower->Num_terms(); ++j) {
      TERM* term = lower->Get_term(j);
      // check if the term kind is one of the loop index
      // variables. If it is then set the is_independent to false
      // for that particular loop
      if (term->Get_type() == LTKIND_LINDEX) {
        _is_independent[term->Get_desc()] = FALSE;
      }
    }
  }
}

//===================================================================
// initialize the region arrays
//===================================================================
void
REGION_ARRAYS::Init(mINT32 index, mINT32 element_size, MEM_POOL *m)
{
  u1._regions = CXX_NEW(PROJECTED_REGION_INFO_ARRAY(m), m);
  _type = 0;
  _sym_index  = index;
  _element_size = element_size;
}

//===================================================================
// Copy the relevant fields over to the new regions array
//===================================================================
void 
REGION_ARRAYS::Copy_write(REGION_ARRAYS *r)
{
  Set_type(r->Get_type());
  Set_sym_id(r->Get_sym_id());
  Set_element_size(r->Get_element_size());
}

//===================================================================
// kill set for array regions
//===================================================================
void
CFG_NODE_INFO::Add_def_array(PROJECTED_REGION* p, 
			     mINT32 element_size,
			     mINT32 sym_index)
{
  ARRAY_OF_REGION_ARRAYS *def = Get_def_array();

  // search for it in the list of arrays
  PROJECTED_REGION_INFO_ARRAY* proj_array;
  for (INT i = 0; i <= def->Lastidx(); ++i) {
    REGION_ARRAYS* region_array = &(*def)[i];
    if (region_array->Get_sym_id() == sym_index) {
      // add the projected region to the array
      proj_array = region_array->Get_projected_region_array();
      INT id = proj_array->Newidx();
      (*proj_array)[id].Set_projected_region(p);
      return;
    }
  }
  // if not found, extend the def array and add it in
  INT idx = def->Newidx();
  REGION_ARRAYS *r = &(*def)[idx];
  r->Init(sym_index, element_size, Array_Summary.Get_array_pool());
  r->Set_is_def();
  proj_array = r->Get_projected_region_array();
  idx = proj_array->Newidx();
  (*proj_array)[idx].Set_projected_region(p);

  if (Trace_Sections) {
    fprintf(TFile, "adding array kill projected region node \n");
    r->Print(TFile);
    fprintf(TFile, "finished adding array kill node \n");
    fprintf(stdout, "adding array kill projected region node \n");
    r->Print(stdout);
    fprintf(stdout, "finished adding array kill node \n");
  }
}

//===================================================================
// kill set for array regions
//===================================================================
void
CFG_NODE_INFO::Add_may_def_array(PROJECTED_REGION* p, 
				 mINT32 element_size,
				 mINT32 sym_index)
{
  INT i;
  ARRAY_OF_REGION_ARRAYS *def = Get_def_array();

  p->Set_is_may_kill();
  // search for it in the list of arrays
  PROJECTED_REGION_INFO_ARRAY* proj_array;
  for (i = 0; i <= def->Lastidx(); ++i) {
    REGION_ARRAYS* region_array = &(*def)[i];
    if (region_array->Get_sym_id() == sym_index) {
      // add the projected region to the array
      proj_array = region_array->Get_projected_region_array();
      INT id = proj_array->Newidx();
      (*proj_array)[id].Set_projected_region(p);
      return;
    }
  }
  // if not found, extend the use array and add it in
  INT idx = def->Newidx();
  REGION_ARRAYS *r = &(*def)[idx];
  r->Init(sym_index, element_size, Array_Summary.Get_array_pool());
  r->Set_is_def();
  proj_array = r->Get_projected_region_array();
  idx = proj_array->Newidx();
  (*proj_array)[idx].Set_projected_region(p);

  if (Trace_Sections) {
    fprintf(TFile, "adding array kill projected region node \n");
    r->Print(TFile);
    fprintf(TFile, "finished adding array kill node \n");
    fprintf(stdout, "adding array kill projected region node \n");
    r->Print(stdout);
    fprintf(stdout, "finished adding array kill node \n");
  }
}

//===================================================================
// upwardly exposed use set for for array regions
//===================================================================
void
CFG_NODE_INFO::Add_use_array(PROJECTED_REGION *p, 
			     mINT32 element_size,
			     mINT32 sym_index)

{
  INT i;
  ARRAY_OF_REGION_ARRAYS *use = Get_use_array();

  // search for it in the list of arrays
  PROJECTED_REGION_INFO_ARRAY* proj_array;
  for (i = 0; i <= use->Lastidx(); ++i) {
    REGION_ARRAYS* region_array = &(*use)[i];
    if (region_array->Get_sym_id() == sym_index) {
      // add the projected region to the array
      proj_array = region_array->Get_projected_region_array();
      INT id = proj_array->Newidx();
      (*proj_array)[id].Set_projected_region(p);
      return;
    }
  }
  // if not found, extend the use array and add it in
  INT idx = use->Newidx();
  REGION_ARRAYS *r = &(*use)[idx];
  r->Init(sym_index, element_size, Array_Summary.Get_array_pool());
  r->Set_is_use();
  proj_array = r->Get_projected_region_array();
  idx = proj_array->Newidx();
  (*proj_array)[idx].Set_projected_region(p);
}

//===================================================================
// upwardly exposed use set for for array regions
//===================================================================
void
CFG_NODE_INFO::Add_may_use_array(PROJECTED_REGION *p, 
				 mINT32 element_size,
			         mINT32 sym_index)
{
  INT i;
  ARRAY_OF_REGION_ARRAYS *use = Get_use_array();
  p->Set_is_may_use();

  // search for it in the list of arrays
  PROJECTED_REGION_INFO_ARRAY* proj_array;
  for (i = 0; i <= use->Lastidx(); ++i) {
    REGION_ARRAYS* region_array = &(*use)[i];
    if (region_array->Get_sym_id() == sym_index) {
      // add the projected region to the array
      proj_array = region_array->Get_projected_region_array();
      INT id = proj_array->Newidx();
      (*proj_array)[id].Set_projected_region(p);
      return;
    }
  }

  // if not found, extend the use array and add it in
  INT idx = use->Newidx();
  REGION_ARRAYS *r = &(*use)[idx];
  r->Init(sym_index, element_size, Array_Summary.Get_array_pool());
  r->Set_is_use();
  proj_array = r->Get_projected_region_array();
  idx = proj_array->Newidx();
  (*proj_array)[idx].Set_projected_region(p);
}

//===================================================================
// store the array sections passed 
//===================================================================
void
CFG_NODE_INFO::Add_array_param(PROJECTED_REGION *p, 
			       mINT32 sym_index,
		 	       mINT32 element_size,
			       INT16 callsite_id, 
			       INT16 actual_id)
{
  ARRAY_OF_REGION_ARRAYS *calls = Get_param_array();

  p->Set_is_passed();
  if (Trace_Sections) {
    fprintf(TFile, "Callsite id = %d, actual_id = %d \n", 
            callsite_id, actual_id);
    fprintf(stdout, "Callsite id = %d, actual_id = %d \n", 
            callsite_id, actual_id);
  }
  
  p->Set_callsite_id(callsite_id);
  p->Set_actual_id(actual_id);

  // search for it in the list of arrays
  PROJECTED_REGION_INFO_ARRAY* proj_array;

  // extend the array and add it in
  INT call_idx = calls->Newidx();
  REGION_ARRAYS *r = &(*calls)[call_idx];
  r->Init(sym_index, element_size, Array_Summary.Get_array_pool());
  r->Set_is_passed();
  proj_array = r->Get_projected_region_array();
  INT idx = proj_array->Newidx();
  (*proj_array)[idx].Set_projected_region(p);
  SUMMARY_CALLSITE* callsite = Summary->Get_callsite(callsite_id);
  INT actual_index = callsite->Get_actual_index() + actual_id;
  SUMMARY_ACTUAL* actual = Summary->Get_actual(actual_index);
  actual->Set_pass_type(PASS_ARRAY_SECTION);
  actual->Set_index(call_idx);
}

//===================================================================
// store the array sections passed
//===================================================================
void
CFG_NODE_INFO::Add_formal_array(PROJECTED_REGION *p,
				mINT32 element_size,
                                mINT32 idx_symbol,
                                mINT32 idx_formal)
{
  ARRAY_OF_REGION_ARRAYS* ara_formal = Get_formal_array();
  p->Set_is_formal();
  INT formal_idx = ara_formal->Newidx();
  REGION_ARRAYS* r = &(*ara_formal)[formal_idx];
  r->Init(idx_symbol, element_size, Array_Summary.Get_array_pool());
  r->Set_is_formal();
  PROJECTED_REGION_INFO_ARRAY* proj_array = r->Get_projected_region_array();
  INT idx = proj_array->Newidx();
  (*proj_array)[idx].Set_projected_region(p);
  SUMMARY_FORMAL* sf = Summary->Get_formal(idx_formal);
  sf->Set_region_index(formal_idx);
}

//===================================================================
// upwardly exposed use set for scalars
//===================================================================
void
CFG_NODE_INFO::Add_scalar_use(mINT32 id)
{
  INT i;
  INT_ARRAY* use;
  use = Get_scalar_use_array();

  for (i=0; i<=use->Lastidx();++i)
    {
      SCALAR_INFO *val = &(*use)[i];
      if (val->Get_id() == id)
	{
	  val->Set_use();

	  // set the euse bit, if the scalar is not killed or modified
	  // this means that there is an exposed use in the node of
	  // the cfg
	  if (!val->Is_kill() && !val->Is_may_kill())
	    val->Set_euse();
	  return;
	}
    }
  
  i = use->Newidx();
  SCALAR_INFO* v = &(*use)[i];
  v->Init();
  v->Set_id(id);
  v->Set_use();
  // if the scalar is not found, then the euse bit must be set
  v->Set_euse();
}

//===================================================================
// kill set for scalars
//===================================================================
void
CFG_NODE_INFO::Add_scalar_def(mINT32 id)
{
  INT i;
  INT_ARRAY* def;
  def = Get_scalar_def_array();

  for (i=0; i<=def->Lastidx();++i)
    {
      SCALAR_INFO *val = &(*def)[i];
      if (val->Get_id() == id)
	{
	  val->Set_kill();
	  return;
	}
    }
  
  i = def->Newidx();
  SCALAR_INFO* v = &(*def)[i];
  v->Init();
  v->Set_id(id);
  v->Set_kill();

}

//===================================================================
// may kill set for scalars
//===================================================================
void
CFG_NODE_INFO::Add_scalar_may_def(mINT32 id)
{
  INT i;
  INT_ARRAY* def;
  def = Get_scalar_def_array();

  for (i=0; i<=def->Lastidx();++i)
    {
      SCALAR_INFO *val = &(*def)[i];
      if (val->Get_id() == id)
	{
	  val->Set_may_kill();
	  return;
	}
    }
  
  i = def->Newidx();
  SCALAR_INFO* v = &(*def)[i];
  v->Init();
  v->Set_id(id);
  v->Set_may_kill();

}

//===================================================================
// kill set for scalars
//===================================================================
void
CFG_NODE_INFO::Add_scalar_may_use(mINT32 id)
{
  INT i;
  INT_ARRAY* def;
  def = Get_scalar_def_array();

  for (i=0; i<=def->Lastidx();++i)
    {
      SCALAR_INFO *val = &(*def)[i];
      if (val->Get_id() == id)
	{
	  val->Set_may_use();
	  return;
	}
    }
  
  i = def->Newidx();
  SCALAR_INFO* v = &(*def)[i];
  v->Init();
  v->Set_id(id);
  v->Set_may_use();

}

//===================================================================
// kill set for scalars
//===================================================================
void
CFG_NODE_INFO::Add_scalar_may_reduc(mINT32 id)
{
  INT i;
  INT_ARRAY* def;
  def = Get_scalar_def_array();

  for (i=0; i<=def->Lastidx();++i)
    {
      SCALAR_INFO *val = &(*def)[i];
      if (val->Get_id() == id)
	{
	  val->Set_may_reduc();
	  return;
	}
    }
  
  i = def->Newidx();
  SCALAR_INFO* v = &(*def)[i];
  v->Init();
  v->Set_id(id);
  v->Set_may_reduc();

}

//===================================================================
// reduc set for scalars
//===================================================================
void
CFG_NODE_INFO::Add_scalar_reduc(mINT32 id)
{
  INT i;
  INT_ARRAY* reduc;
  reduc = Get_scalar_reduc_array();

  for (i=0; i<=reduc->Lastidx();++i)
    {
      SCALAR_INFO *val = &(*reduc)[i];
      if (val->Get_id() == id)
	{
	  val->Set_reduc();
	  return;
	}
    }
  
  i = reduc->Newidx();
  
  SCALAR_INFO* v = &(*reduc)[i];
  v->Init();
  v->Set_id(id);
  v->Set_reduc();

}

//===================================================================
// set scalar passed for reference parameters
//===================================================================
INT
CFG_NODE_INFO::Add_scalar_ref_passed(mINT32 id, mINT16 callsite_id)
{
  INT i;
  INT_ARRAY* scalar_array;
  scalar_array = Get_scalar_array();

  for (i=0; i<=scalar_array->Lastidx();++i)
    {
      SCALAR_INFO *val = &(*scalar_array)[i];
      if (val->Get_id() == id)
	{
	  val->Set_passed_ref();
	  // record the last callsite id for nodes that have not
	  // been killed, since this is used in the case of exposed use
	  if ((val->Get_callsite_id() == 0) && (!val->Is_kill()))
	    {
	      val->Set_callsite_id(callsite_id);

	    }
	  return i;
	}
    }
  
  i = scalar_array->Newidx();
  SCALAR_INFO* v = &(*scalar_array)[i];
  v->Init();
  v->Set_id(id);
  v->Set_passed_ref();
  v->Set_callsite_id(callsite_id);

  return i;
}


//===================================================================
// set scalar passed for reference parameters
//===================================================================
INT
CFG_NODE_INFO::Add_scalar_ref_may_passed(mINT32 id, mINT16 callsite_id)
{
  INT i;
  INT_ARRAY* scalar_array;
  scalar_array = Get_scalar_array();

  for (i=0; i<=scalar_array->Lastidx();++i)
    {
      SCALAR_INFO *val = &(*scalar_array)[i];
      if (val->Get_id() == id)
	{
	  val->Set_may_passed_ref();
	  if (val->Get_callsite_id() == 0)
	    val->Set_callsite_id(callsite_id);
	  return i;
	}
    }
  
  i = scalar_array->Newidx();
  SCALAR_INFO* v = &(*scalar_array)[i];
  v->Init();
  v->Set_id(id);
  v->Set_may_passed_ref();
  v->Set_callsite_id(callsite_id);
  return i;
}

//===================================================================
// reduc set for arrays
//===================================================================
void
CFG_NODE_INFO::Add_array_reduc(mINT32 id)
{
  INT i;
  INT_ARRAY* reduc;
  reduc = Get_array_reduc();

  for (i=0; i<=reduc->Lastidx();++i)
    {
      SCALAR_INFO *val = &(*reduc)[i];
      if (val->Get_id() == id)
	{
	  val->Set_array_reduc();
	  return;
	}
    }
  
  i = reduc->Newidx();
  SCALAR_INFO* v = &(*reduc)[i];
  v->Init();
  v->Set_id(id);
  v->Set_array_reduc();

}


//===================================================================
// reduc set for arrays
//===================================================================
void
CFG_NODE_INFO::Add_array_may_reduc(mINT32 id)
{
  INT i;
  INT_ARRAY* reduc;
  reduc = Get_array_reduc();

  for (i=0; i<=reduc->Lastidx();++i)
    {
      SCALAR_INFO *val = &(*reduc)[i];
      if (val->Get_id() == id)
	{
	  val->Set_array_may_reduc();
	  return;
	}
    }
  
  i = reduc->Newidx();
  SCALAR_INFO* v = &(*reduc)[i];
  v->Init();
  v->Set_id(id);
  v->Set_array_may_reduc();

}

//---------------------------------------------------------------------
// get an ivar
//---------------------------------------------------------------------
IVAR*
ARRAY_SUMMARY::Get_ivar_array(INT i)
{
  FmtAssert(i <= _ivar->Lastidx(),("illegal ivar index \n"));
  return &(*_ivar)[i];
}

//---------------------------------------------------------------------
// get a term
//---------------------------------------------------------------------
TERM* 
ARRAY_SUMMARY::Get_term_array(INT i)  
{ 
  FmtAssert(i <= _term_array->Lastidx(),("illegal term index \n"));
  return &(*_term_array)[i];
}

//---------------------------------------------------------------------
// get a projected array
//---------------------------------------------------------------------
PROJECTED_NODE* 
ARRAY_SUMMARY::Get_projected_array(INT i) 
{ 
  FmtAssert(i <= _project_nodes->Lastidx(),("illegal project index\n"));
  return &(*_project_nodes)[i]; 
}

//---------------------------------------------------------------------
// get a projected region array
//---------------------------------------------------------------------
PROJECTED_REGION* 
ARRAY_SUMMARY::Get_projected_region_array(INT i) 
{ 
 FmtAssert(i <= _projected_regions->Lastidx(),("illegal region index\n"));
 return &(*_projected_regions)[i];
}

//---------------------------------------------------------------------
// get a region array
//---------------------------------------------------------------------
REGION_ARRAYS* 
ARRAY_SUMMARY::Get_region_array(INT i) 
{ 
  FmtAssert(i <= _region_arrays->Lastidx(),("illegal region array index\n"));
  return &(*_region_arrays)[i]; 
}

//---------------------------------------------------------------------
// get a cfg node array
//---------------------------------------------------------------------
CFG_NODE_INFO*
ARRAY_SUMMARY::Get_cfg_node_array(INT i) 
{ 
  FmtAssert(i <= _cfg_nodes->Lastidx(),("illegal cfg node index\n"));
  return &(*_cfg_nodes)[i];
}

//---------------------------------------------------------------------
// get a loopinfo array
//---------------------------------------------------------------------
LOOPINFO*
ARRAY_SUMMARY::Get_loopinfo_array(INT i)
{ 
  FmtAssert(i <= _loop_nodes->Lastidx(),("illegal loopinfo index\n"));
  return &(*_loop_nodes)[i];
}

//---------------------------------------------------
// for array section tlogs record the following:
// 1) number of LTKIND_CONST  terms
// 2) number of LTKIND_LINDEX terms
// 4) number of LTKIND_IV terms
// 5) number of LTKIND_SUBSCR terms
//---------------------------------------------------
void
ARRAY_SUMMARY::Record_tlogs(TERM_ARRAY *tarray, INT offset)
{
  TLOG_INFO* tlog =  Get_tlog_info();
  //TERM_ARRAY *tarray = Get_term_array();
  if (tarray)
    {
      for (INT i=offset; i<=tarray->Lastidx();++i)
	{
	  TERM *t = &(*tarray)[i];
	  switch (t->Get_type())
	    {
	    case LTKIND_CONST:
	      tlog->Set_cterm_count(tlog->Get_cterm_count()+1);
	      break;
	      
	    case LTKIND_LINDEX:
	      tlog->Set_lterm_count(tlog->Get_lterm_count()+1);
	      break;
	      
 	    case LTKIND_IV:
	      tlog->Set_iv_term_count(tlog->Get_iv_term_count()+1);
	      break;
	      
	    case LTKIND_SUBSCR:
	      tlog->Set_sub_term_count(tlog->Get_sub_term_count()+1);
	      break;
	    }
	}
     }
}

//-----------------------------------------------------------------------
// NAME: PROJECTED_KERNEL::Set_Difference
// FUNCTION: Set the "_difference" field in the PROJECTED_KERNEL to 
//  'pr' - that PROJECTED_KERNEL.  
//-----------------------------------------------------------------------

void PROJECTED_KERNEL::Set_Difference(PROJECTED_REGION* pr)
{ 
  for (INT i = 0; i < Get_num_dims(); i++) { 
    LINEX* lx_kernel = Get_linex(i);
    LINEX* lx_region = pr->Get_projected_node(i)->Get_lower_linex();
    LINEX* lx_diff = lx_region->Subtract(lx_kernel);
    INT idx = _difference->Newidx();
    LINEX* lx_new = &(*_difference)[idx];
    lx_new->Init(Array_Summary.Get_array_pool());
    lx_diff->Copy(lx_new);
  }    
} 
