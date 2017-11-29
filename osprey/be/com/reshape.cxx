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


#include <sys/types.h>
#include "tracing.h"
#include "ipa_section.h"
#include "reshape.h"

//-----------------------------------------------------------------------
// NAME: RESHAPE::RESHAPE
// FUNCTION: Save a list of PROJECTED_REGIONs for reshaping analysis.
//-----------------------------------------------------------------------

RESHAPE::RESHAPE(PROJECTED_REGION* caller_shape_proj_region,
                 PROJECTED_REGION* callee_shape_proj_region,
                 PROJECTED_REGION* callee_proj_region,
                 PROJECTED_REGION* callsite_region,
                 MEM_POOL* m,
		 BOOL trace) :
  _caller_shape_proj_region (caller_shape_proj_region),
  _callee_shape_proj_region (callee_shape_proj_region),
  _callee_proj_region (callee_proj_region),
  _callsite_region (callsite_region),
  _callee_proj_reshaped_region (NULL),
  _m (m)
{
#ifdef Is_True_On
  if (trace) { 
    fprintf(TFile, "\nRESHAPE::RESHAPE\n");
    fprintf(TFile, "  caller_shape: ");
    if (_caller_shape_proj_region) {
      _caller_shape_proj_region->Print(TFile);
    } else {
      fprintf(TFile, "NULL\n");
    }
    fprintf(TFile, "  callee_shape: ");
    if (_callee_shape_proj_region) {
      _callee_shape_proj_region->Print(TFile);
    } else {
      fprintf(TFile, "NULL\n");
    }
    fprintf(TFile, "  callee_proj_region: ");
    if (_callee_proj_region) {
      _callee_proj_region->Print(TFile);
    } else {
      fprintf(TFile, "NULL\n");
    }
    fprintf(TFile, "  callsite_region: ");
    if (_callsite_region) {
      _callsite_region->Print(TFile);
    } else {
      fprintf(TFile, "NULL\n");
    }
  }
#endif // Is_True_On
}

//-----------------------------------------------------------------------
// NAME: RESHAPE::Constant_Type_Reshape
// FUNCTION: Return TRUE if this reshaping is "constant".  Return FALSE 
//   otherwise. 
// NOTE: By "constant" we mean:
//   (1)  Caller and callee shapes have all constant dimensions, with 
//        the possible exception of the first dimension.
//   (2)  The region in the callee has all constant dimensions. 
//-----------------------------------------------------------------------

BOOL RESHAPE::Constant_Type_Reshape()
{
  return _caller_shape_proj_region->Constant_bounds(1)  
    && _callee_shape_proj_region->Constant_bounds(1)  
    && _callee_proj_region->Constant_bounds(0);
}

//-----------------------------------------------------------------------
// NAME: RESHAPE::Reshape_Callee_To_Caller
// FUNCTION: Reshape the callee to the caller.  (This does not include 
//   handling any passed section).  We return the PROJECTED_REGION re-
//   shaped to the caller, if the reshaping is successful, and NULL 
//   otherwise. 
//-----------------------------------------------------------------------

PROJECTED_REGION* RESHAPE::Reshape_Callee_To_Caller(BOOL trace)
{

#ifdef Is_True_On
  if (trace) { 
    fprintf(TFile, "RESHAPE::Perform_Reshape\n");
  }
#endif

  Is_True(_callee_proj_region != NULL && _caller_shape_proj_region != NULL,
    ("Reshape_Callee_To_Caller: NULL projected region\n"));

  UINT depth = _caller_shape_proj_region->Get_depth();
  UINT num_dims = _caller_shape_proj_region->Get_num_dims();

  // Screen out cases where:
  // - callee region is messy, or
  // - the actual passed is messy, or
  // - we have non-constant region reshaping
  if (_callee_proj_region->Is_messy_region() ||
      _callee_proj_region->Has_Messy_Bounds() ||
      _caller_shape_proj_region->Is_messy_region() ||
      _caller_shape_proj_region->Has_Important_Messy_Bounds() ||
      (_callsite_region && (_callsite_region->Is_messy_region() ||
      _callsite_region->Has_Messy_Bounds())) ||
      ! Constant_Type_Reshape()) {
    _callee_proj_reshaped_region =
      CXX_NEW (PROJECTED_REGION (MESSY_REGION, depth, num_dims, _m), _m);
  } else {
    // Handle the case where is the section is a constant
    // and the caller shape bounds for all except the last
    // is also a constant. The number of dimensions being reshaped
    // to and from should not matter
    _callee_proj_reshaped_region = Reshape_Constant_Shape();
  }

#ifdef Is_True_On
  if (trace) { 
    fprintf(TFile, "  callee_proj_reshaped_region: ");
    _callee_proj_reshaped_region->Print(TFile);
  }
#endif

  Is_True(_callee_proj_reshaped_region->Get_num_dims() == num_dims,
   ("Reshape_Callee_To_Caller: Dims reshaped callee (%d) != caller shape (%d)", 
   num_dims, _callee_proj_reshaped_region->Get_num_dims()));

  return _callee_proj_reshaped_region;
}

//-----------------------------------------------------------------------
// NAME: Linearize_Shape
// FUNCTION: Return values for 'lin_lower', 'lin_upper', and 'lin_stride',
//   the linearized dimensions of the 'pr_callee' currently shaped as 
//   'pr_callee_shape'.
//-----------------------------------------------------------------------

static void Linearize_Shape(PROJECTED_REGION* pr_callee,
			    PROJECTED_REGION* pr_callee_shape, 
			    INT* lin_lower,
			    INT* lin_upper,
			    INT* lin_stride)
{
  // compute the upper bound on region size from the _callee_proj_region
  *lin_upper = 0;
  *lin_lower = 0;
  INT dim_size = 1;
  INT last_callee_dim = pr_callee->Get_num_dims() - 1;
  for (INT i = last_callee_dim; i >= 0; i--) {
    PROJECTED_NODE* region_node = pr_callee->Get_projected_node(i);
    LINEX* stride = region_node->Get_step_linex();
    LINEX* ub = region_node->Get_upper_linex();
    LINEX* lb = region_node->Get_lower_linex();
    if (i == last_callee_dim) {
      Is_True(stride->Num_terms() == 0 
	&& stride->Get_term(0)->Get_type() == LTKIND_CONST,
	("Linearize_Shape: Dim %d ub for callee is NOT constant", i));
      *lin_stride = stride->Get_constant_term();
    } 
    Is_True(ub->Num_terms() == 0 
      && ub->Get_term(0)->Get_type() == LTKIND_CONST,
      ("Linearize_Shape: Dim %d ub for callee is NOT constant", i));
    Is_True(lb->Num_terms() == 0 
      && lb->Get_term(0)->Get_type() == LTKIND_CONST,
      ("Linearize_Shape: Dim %d lb for callee is NOT constant", i));
    *lin_upper += ub->Get_constant_term() * dim_size;
    *lin_lower += lb->Get_constant_term() * dim_size;
    // Get the dimension sizes from the callee shape
    if (i != 0) {
      PROJECTED_NODE* node = pr_callee_shape->Get_projected_node(i);
      LINEX* upper = node->Get_upper_linex();
      LINEX* lower = node->Get_lower_linex();
      Is_True(upper->Num_terms() == 0 
        && upper->Get_term(0)->Get_type() == LTKIND_CONST,
        ("Linearize_Shape: Dim %d ub for callee shape is NOT constant", i));
      Is_True(lower->Num_terms() == 0 
        && lower->Get_term(0)->Get_type() == LTKIND_CONST,
        ("Linearize_Shape: Dim %d lb for callee shape is NOT constant", i));
      dim_size *= abs(upper->Get_constant_term() 
	- lower->Get_constant_term() + 1);
    }
  }
} 

//-----------------------------------------------------------------------
// NAME: Dim_Length
// FUNCTION: Return the length of the 'k'th dimension of 'pr_shape'.
//-----------------------------------------------------------------------

static INT Dim_Length(PROJECTED_REGION* pr_shape,
		      INT k)
{
  PROJECTED_NODE* p = pr_shape->Get_projected_node(k);
  Is_True(!p->Is_messy_ub(), ("Dim_Length: Messy upper bound"));
  Is_True(!p->Is_messy_lb(), ("Dim_Length: Messy lower bound"));
  Is_True(!p->Is_messy_step(), ("Dim_Length: Messy step"));
  Is_True(p->Get_upper_linex()->Is_const(), 
    ("Dim_Length: Non-constant upper bound"));
  Is_True(p->Get_lower_linex()->Is_const(), 
    ("Dim_Length: Non-constant lower bound"));
  Is_True(p->Get_step_linex()->Is_const(), 
    ("Dim_Length: Non-constant step"));
  INT upper_val = p->Get_upper_linex()->Get_term(0)->Get_coeff(); 
  INT lower_val = p->Get_lower_linex()->Get_term(0)->Get_coeff(); 
  INT step_val = p->Get_step_linex()->Get_term(0)->Get_coeff(); 
  Is_True(lower_val == 0, ("Dim_Length: Expecting shape lower bound of 0"));
  Is_True(step_val == 1, ("Dim_Length: Expecting shape step of 1"));
  return upper_val + 1;
} 

//-----------------------------------------------------------------------
// NAME: Delinearize_Shape
// FUNCTION: Delinearize the 'pr_caller' into 'pr_caller_shape' from the 
//   linearized section with lower bound 'lin_lower', upper bound 
//   'lin_upper' and stride 'lin_stride'.  Fill in the 'lb_caller_dim' 
//   to 'ub_caller_dim - 1' dimensions of 'pr_caller' with the result 
//   (which has a total of 'total_dims' dimensions).  
//-----------------------------------------------------------------------

static void Delinearize_Shape(PROJECTED_REGION* pr_caller, 
			      PROJECTED_REGION* pr_caller_shape, 
			      INT lin_lower, 
			      INT lin_upper,
			      INT lin_stride, 
			      INT lb_caller_dim,
			      INT ub_caller_dim,
			      INT total_dims)
{
  INT lb_current = lin_lower;
  INT ub_current = lin_upper; 
  BOOL fill_in_max = FALSE; 
  for (INT j = lb_caller_dim; j < ub_caller_dim; j++) { 
    INT dim_size = 1;
    for (INT k = j + 1; k < total_dims; k++)
      dim_size *= Dim_Length(pr_caller_shape, k);
    INT lb_current_dim = -1;
    INT ub_current_dim = -1;
    if (fill_in_max) {
      PROJECTED_NODE* p = pr_caller_shape->Get_projected_node(j);
      lb_current_dim = 0; 
      ub_current_dim = p->Get_upper_linex()->Get_term(0)->Get_coeff();
    } else {   
      lb_current_dim = lb_current / dim_size; 
      ub_current_dim = ub_current / dim_size; 
    } 
    PROJECTED_NODE* pn_return = pr_caller->Get_projected_node(j);
    INT stride = j == total_dims - 1 ? lin_stride : 1; 
    pn_return->Set_constant_linexs(ub_current_dim, lb_current_dim, 
      lin_stride, 0, 0);
    lb_current -= lb_current_dim * dim_size; 
    ub_current -= ub_current_dim * dim_size; 
    if (lb_current_dim < ub_current_dim)
      fill_in_max = TRUE; 
  }
}

//-----------------------------------------------------------------------
// NAME: RESHAPE::Reshape_Constant_Shape
// FUNCTION: Reshape the constant dimensioned callee-based projected 
//   region into a constant dimensioned caller-based projected region.
//-----------------------------------------------------------------------

PROJECTED_REGION* RESHAPE::Reshape_Constant_Shape(BOOL trace)
{

#ifdef Is_True_On
  if (trace) { 
    fprintf(TFile, "RESHAPE::Reshape_Constant_Shape\n");
  }
#endif

  Is_True(Constant_Type_Reshape(),
    ("Reshape_Constant_Shape: Expecting constant shapes"));

  UINT caller_depth = _caller_shape_proj_region->Get_depth();
  UINT caller_dims = _caller_shape_proj_region->Get_num_dims();
  PROJECTED_REGION* region =
   CXX_NEW(PROJECTED_REGION(NON_MESSY_REGION,caller_depth,caller_dims,_m),_m);

  // Linearize from the callee. 
  INT lin_upper = 0;
  INT lin_lower = 0;
  INT lin_stride = 0;
  Linearize_Shape(_callee_proj_region, _callee_shape_proj_region, 
    &lin_lower, &lin_upper, &lin_stride);

#ifdef Is_True_On
  if (trace) { 
    fprintf(TFile, "  Linearized callee_projected_region: [%d:%d:1]\n",
            lin_lower, lin_upper);
  }
#endif

  Is_True(lin_upper >= lin_lower, 
    ("Reshape_Constant_Shape: After linearization lb (%d) > ub (%d)\n", 
    lin_lower, lin_upper));

  // Determine if we are working with two-strided segments   
  INT segment_length = -1; 
  INT segment_stride = -1;
  PROJECTED_NODE* pr_node = _callee_proj_region->Get_projected_node(0); 
  if (pr_node->Get_segment_length_linex() != NULL
      && pr_node->Get_segment_length_linex()->Is_const()
      && pr_node->Get_segment_length_linex()->Is_const()) {
    segment_length = pr_node->Get_segment_length_linex()->Get_constant_term(); 
    segment_stride = pr_node->Get_segment_stride_linex()->Get_constant_term(); 
  } 

  // Delinearize into the caller. 
  if (segment_stride == -1) { 
    Delinearize_Shape(region, _caller_shape_proj_region, lin_lower, 
      lin_upper, lin_stride, 0, caller_dims, caller_dims); 
  } else { 
    INT stride_dims = 0; 
    INT local_stride = 1;
    for (INT i = caller_dims - 1; i >= 0; i--) {
      stride_dims++;
      local_stride *= Dim_Length(_caller_shape_proj_region, i);
      if (local_stride >= segment_stride)
	break;
    } 
    if (local_stride != segment_stride) { 
      Delinearize_Shape(region, _caller_shape_proj_region, lin_lower,
        lin_upper, lin_stride, 0, caller_dims, caller_dims);
    } else { 
      Delinearize_Shape(region, _caller_shape_proj_region, lin_lower,
        lin_upper, 1, 0, caller_dims - stride_dims, caller_dims);
      Delinearize_Shape(region, _caller_shape_proj_region, 0,
        segment_length - 1, lin_stride, caller_dims - stride_dims, 
        caller_dims, caller_dims);
    } 
  }  

  return region;
}

//-----------------------------------------------------------------------
// NAME: RESHAPE::Reshapeable_Passed_Section
// FUNCTION: Return TRUE if the section passed can be added to the caller
//   reshape section without violating the constraints of the caller  
//   shapes bounds.  Return FALSE otherwise. 
//-----------------------------------------------------------------------

BOOL RESHAPE::Reshapeable_Passed_Section(BOOL trace)
{

#ifdef Is_True_On
  if (trace) { 
    fprintf(TFile, "RESHAPE::Actual_Passed_Reshapable\n");
  }
#endif

  Is_True(_callsite_region != NULL,
          ("Actual_Passed_Reshapeable: NULL callsite region"));

  if (_callsite_region->Is_messy_region()) {
    return FALSE;
  }
    
  Is_True(_callsite_region->Is_unprojected_region(),
          ("Actual_Passed_Reshapeable: Projected callsite region"));

  for (INT i = 1; i < _callsite_region->Get_num_dims(); i++) {
    PROJECTED_NODE* pn_callsite 
     = _callsite_region->Get_projected_node(i);
    PROJECTED_NODE* pn_caller_shape 
     = _caller_shape_proj_region->Get_projected_node(i);
    PROJECTED_NODE* pn_caller 
     = _callee_proj_reshaped_region->Get_projected_node(i);
    LINEX* lx_callsite = pn_callsite->Get_lower_linex();
    LINEX* lx_caller_lower = pn_caller->Get_lower_linex();
    LINEX* lx_caller_upper = pn_caller->Get_upper_linex();
    LINEX* lx_caller_shape_lower = pn_caller_shape->Get_lower_linex();
    LINEX* lx_caller_shape_upper = pn_caller_shape->Get_upper_linex();
    LINEX* lx_caller_diff = lx_caller_upper->Subtract(lx_caller_lower);
    lx_caller_diff->Simplify();

#ifdef Is_True_On
    if (trace) { 
      fprintf(TFile, "  dim[%d] - ", i);
      fprintf(TFile, "callsite: ");
      pn_callsite->Print(TFile);
      fprintf(TFile, " caller_shape: ");
      pn_caller_shape->Print(TFile);
    } 
#endif

    if (!lx_caller_diff->Is_const() 
	|| lx_caller_diff->Get_constant_term() != 0) {
      LINEX* lx_high = lx_caller_upper->Merge(lx_callsite);
      LINEX* lx_over = lx_high->Subtract(lx_caller_shape_upper);
      if (!lx_over->Is_const() || lx_over->Get_constant_term() > 0)
	return FALSE; 
      LINEX* lx_low = lx_caller_lower->Merge(lx_callsite);
      LINEX* lx_under = lx_low->Subtract(lx_caller_shape_lower);
      if (!lx_under->Is_const() || lx_under->Get_constant_term() > 0)
	return FALSE; 
    }
  } 

  return TRUE;
}

//-----------------------------------------------------------------------
// NAME: RESHAPE::Reshape_Passed_Section
// FUNCTION: Add the callsite to the 'callee_reshaped' section to get
//   the final reshaped result.
//-----------------------------------------------------------------------

void RESHAPE::Reshape_Passed_Section(PROJECTED_REGION* callee_reshaped, 
				     BOOL trace)
{

#ifdef Is_True_On
  if (trace) { 
    fprintf(TFile, "RESHAPE::Reshape_Passed_Section\n");
  }
#endif

  INT num_dims = callee_reshaped->Get_num_dims();
  for (INT dim_idx = 0; dim_idx < num_dims; dim_idx++) {
    PROJECTED_NODE* pn_callsite = _callsite_region->Get_projected_node(dim_idx);
    LINEX* lx_callsite = pn_callsite->Get_lower_linex();
    PROJECTED_NODE* pn_reshaped = callee_reshaped->Get_projected_node(dim_idx);
    LINEX* lx_lower = lx_callsite->Merge(pn_reshaped->Get_lower_linex());
    LINEX* lx_upper = lx_callsite->Merge(pn_reshaped->Get_upper_linex());
    pn_reshaped->Get_lower_linex()->Free_terms();
    pn_reshaped->Set_lower_linex(lx_lower);

    INT i;
    
    for (i = 0; i <= pn_reshaped->Get_lower_linex()->Num_terms(); i++) { 
      TERM* tm = pn_reshaped->Get_lower_linex()->Get_term(i); 
      switch (tm->Get_type()) { 
      case LTKIND_LINDEX: 
      case LTKIND_SUBSCR: 
	pn_reshaped->Set_messy_lb();
	break; 
      } 
    } 

    pn_reshaped->Get_upper_linex()->Free_terms();
    pn_reshaped->Set_upper_linex(lx_upper);

    for (i = 0; i <= pn_reshaped->Get_upper_linex()->Num_terms(); i++) { 

      TERM* tm = pn_reshaped->Get_upper_linex()->Get_term(i); 
      switch (tm->Get_type()) { 
      case LTKIND_LINDEX: 
      case LTKIND_SUBSCR: 
	pn_reshaped->Set_messy_ub();
	break; 
      } 
    } 
  } 
  

#ifdef Is_True_On
  if (trace) { 
    fprintf(TFile, "New reshaped section\n");
    callee_reshaped->Print(TFile);
  }
#endif

}
