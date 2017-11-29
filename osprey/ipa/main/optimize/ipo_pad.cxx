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
// ====================================================================
// ====================================================================
//
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/ipa/main/optimize/ipo_pad.cxx,v $
//
// Revision history:
//  2-May-98 - Original Version
//
// Description:
//
// Implementation of the utilities necessary to pad array
// columns to avoid cash-thrashing whenever possible.
//
// ====================================================================
// ====================================================================

#include <stdint.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <assert.h>		// for assert()
#include <cmplrs/host.h>        // for ipc_ld.h
#define USE_STANDARD_TYPES      /* override unwanted defines in defs.h */

#include "defs.h"
#include "stab.h"		// symtab stuff
#include "mempool.h"		// for MEM_POOL
#include "cxx_template.h"	// DYN_ARRAY<T>
#include "tracing.h"		// for TFile
#include "dwarf_DST_mem.h"	// for ipc_file.h
#include "mtypes.h"		// for ipc_file.h
#include "const.h"
#include "irbdata.h"
#include "ipc_file.h"		// for ipa_inline.h
#include "ipa_option.h"		// for Trace_IPA
#include "ipa_pad.h"     	// for COMMON_SNODE
#include "ipo_main.h"		// for IPO_Gprel_Sym_Count
#include "ipa_cg.h"             // call graph
#include "ir_reader.h"          // dump tree
#define MAX_ALIGN 16

static
BOOL trace_split_common = FALSE;
//=================================================================
// DESCR: Add_Element
//        Enter and element into the array
//=================================================================
void 
Add_Element_ST_TO_FLD_MAP(FLD_HANDLE fld, ST* s, 
			  ST_TO_FLD_MAP_ARRAY& st_to_fld_map)
{
  INT idx = st_to_fld_map.Newidx();
  ST_TO_FLD_MAP *entry = &(st_to_fld_map)[idx];
  entry->Init(fld, s);
}

//=================================================================
// DESCR: Create
// For each ST in the symbol-table, based at the given base,
// match it up with the corresponding field in the base.
//=================================================================
void
Create_ST_TO_FLD_MAP(COMMON_SNODE_TBL* common_snode_tbl)
{
  ST* st;
  INT i;
  FOREACH_SYMBOL(GLOBAL_SYMTAB, st, i) {

    if (Is_Common_Based_Symbol(st)) {

      ST* common_st = ST_base(st);
      COMMON_SNODE_LIST* common_snode_list = 
        common_snode_tbl->Find(ST_name_idx(common_st));
      Is_True(common_snode_list, ("NULL common list in Create_ST_TO_FLD_MAP"));

      if (!common_snode_list->No_Pad()) {

        ST_TO_FLD_MAP_ARRAY* st_to_fld_map = common_snode_list->Get_Map();
        Is_True(st_to_fld_map, ("NULL fld map list in Create_ST_TO_FLD_MAP"));
      
        BOOL done = FALSE;
        FLD_ITER fld_iter= Make_fld_iter(TY_fld(Ty_Table[ST_type(common_st)]));
        do {
          FLD_HANDLE fld (fld_iter);
          if (FLD_ofst(fld) == ST_ofst(st) && FLD_type(fld) == ST_type(st)) {
            INT idx = st_to_fld_map->Newidx();
            ST_TO_FLD_MAP  *item = &(*st_to_fld_map)[idx];
            item->Init(fld,st);
            done = TRUE;
          }
        } while (!FLD_last_field(fld_iter++) && !done);

        Is_True(done, 
                ("Missing FLD for ST %s in Create_ST_TO_FLD_MAP",ST_name(st)));
      }
    }
  }
}

//=================================================================
// DESCR: Get_FLD
//        Return the FLD given the ST
//=================================================================
/*ARGSUSED*/
FLD_HANDLE
Get_FLD(ST* s, ST* common_st, ST_TO_FLD_MAP_ARRAY& st_to_fld_map)
{

  ST* st = NULL;
  FLD_HANDLE fld;
  for (UINT32 idx = 0; idx < st_to_fld_map.Elements(); idx++)
    {
      ST_TO_FLD_MAP *entry = &(st_to_fld_map)[idx];
      if (s == entry->Get_ST())
	return entry->Get_FLD();
    } 
  return fld;
}

//=================================================================
// DESCR: Get_ST
//        Return the ST given the field
//=================================================================
/*ARGSUSED*/
ST *
Get_ST(FLD_HANDLE fld, ST_TO_FLD_MAP_ARRAY& st_to_fld_map)
{
  ST  * st = NULL;
  for (mUINT32 idx = 0; idx < st_to_fld_map.Elements(); idx++)
    {
      ST_TO_FLD_MAP *entry = &(st_to_fld_map)[idx];
      if (fld == entry->Get_FLD())
	return entry->Get_ST();
    }
  return st;

} // Get_ST

// ==================================================================
// DESCR: Is_Const_Bounds
// ==================================================================
inline INT64
Is_Const_Bounds(ARB_HANDLE arb)
{
  return (ARB_const_ubnd(arb) && ARB_const_lbnd(arb) && 
           ARB_const_stride(arb));
}

// ==================================================================
// DESCR: Num_Elements
//        return the number of elements in a particular dimension
// ==================================================================
static INT64
Num_Elements(ARB_HANDLE arb)
{
#ifdef KEY
// bug 2132: for dimensions -n1:n1, # of elements is n1 - (-n1) + 1
   return abs(ARB_ubnd_val(arb) - ARB_lbnd_val(arb)) + 1;
#else
 INT64 count = 0;
   return abs(abs(ARB_ubnd_val(arb)) - abs((ARB_lbnd_val(arb)))) + 1;
#endif
}
// ===========================================================
// DESCR: Create_New_Array_Type
//        Create a new array type
// ===========================================================
static TY_IDX 
Create_New_Array_Type(TY_IDX old_array_ty)
{
  INT num_dims = TY_AR_ndims(old_array_ty);
  TY_IDX etype_idx = TY_etype(old_array_ty);
  const TY& etype = Ty_Table[etype_idx];

  TY_IDX new_array_ty = 
    Make_Array_Type(TY_mtype(etype), num_dims, 1);

  Set_TY_name_idx(Ty_Table[new_array_ty], TY_name_idx(Ty_Table[old_array_ty]));
  Set_TY_size(Ty_Table[new_array_ty],TY_size(Ty_Table[old_array_ty]));

  num_dims = TY_AR_ndims(new_array_ty);
  ARB_HANDLE arb_base = TY_arb(new_array_ty);
  ARB_HANDLE old_arb_base = TY_arb(old_array_ty);
  for (UINT i = 0; i < num_dims ; ++i)
    {
      ARB_HANDLE  arb = arb_base[i];
      ARB_HANDLE  old_arb = old_arb_base[i];
      ARB_Init(arb, ARB_lbnd_val(old_arb), 
 	       ARB_ubnd_val(old_arb), 
	       ARB_stride_val(old_arb));
      Set_ARB_dimension(arb, num_dims-i);
      if (i==0)
	 Set_ARB_first_dimen(arb_base[0]);
      if (i == num_dims-1)
	 Set_ARB_last_dimen(arb_base[i]);	
    }
  
  return new_array_ty;
}

// ===========================================================
// DESCR: Create_New_Array_Type
//        Create a new array type
// ===========================================================
static TY_IDX 
Create_Multi_Dim_Array_Type(TY_IDX old_array_ty, BOUNDS_ARRAY *bounds)
{
  TY_IDX ty_index = Create_New_Array_Type(old_array_ty);

  TY&  new_array_ty = Ty_Table[ty_index];

  INT num_dims = TY_AR_ndims(new_array_ty);
  ARB_HANDLE arb_base = TY_arb(new_array_ty);
  INT64 element_size = ARB_stride_val(arb_base[num_dims-1]);
  INT64 new_stride = element_size;
  
  for (UINT i = 0; i < num_dims ; ++i)
     {
	// get to the array bounds information in column major order
	ARB_HANDLE  arb = arb_base[num_dims-i-1];
	BOUNDS *belement = &(*bounds)[i];
	INT64 upper = belement->Get_Upper(); 
	Set_ARB_ubnd_val(arb,upper);
	Set_ARB_stride_val(arb,new_stride);
	new_stride = new_stride*Num_Elements(arb);
     }
  return ty_index;
}



// ===========================================================
// DESCR: Get_New_Size_Padding
//        return the padding for dimensions starting at 
//        start dim
//        if start_dim =0, returns total padding
//        if start_dim =1, returns padding for all dims except 
//           last dimension
// ===========================================================
INT64
Get_New_Size_Padding(TY_IDX new_array_ty_idx, TY_IDX old_array_ty_idx,
		     INT32 start_dim)
{
 TY&  new_array_ty = Ty_Table[new_array_ty_idx];
 TY& old_array_ty = Ty_Table[old_array_ty_idx];

  INT64 old_size = 1;
  INT64 new_size = 1;
  INT num_dims = TY_AR_ndims(new_array_ty);
  ARB_HANDLE old_arb = TY_arb(old_array_ty);
  ARB_HANDLE new_arb = TY_arb(new_array_ty);

  for (INT i=start_dim; i<num_dims; ++i)
    {
      old_size = old_size*Num_Elements(old_arb[i]);
      new_size = new_size*Num_Elements(new_arb[i]);
    }
  new_size = new_size - old_size;
  return new_size;
}

//=================================================================
// DESCR: Update_Split_Array
//        update the entries of the split array after padding
//=====================================================================
static void
Update_Split_Array(SPLIT_COMMON_DYN_ARRAY *split_array, ST* s)
{
  INT id = 0;

  FLD_ITER fld_iter = Make_fld_iter (TY_fld (Ty_Table[ST_type (s)]));
  do {
    FLD_HANDLE fld (fld_iter);
    if (!FLD_equivalence (fld)) {
      INT64 element_size = 0;
      const TY& fld_type = Ty_Table[FLD_type (fld)];
      if (TY_kind(fld_type) == KIND_ARRAY)
        element_size = TY_size(TY_etype(fld_type));
      else
        element_size = TY_size(fld_type);

      SPLIT_COMMON *node = &(*split_array)[id++];
      node->Set_Vals(FLD_ofst(fld), TY_size(fld_type)/element_size, element_size, node->Get_split_position(), node->Get_group_position());
    }
  }  while (!FLD_last_field (fld_iter++));

}

// ===========================================================
// DESCR: Pad_Multi_Dim_Common_ST
//        Pad multiple dimensions of a common array
// ===========================================================
static void
Pad_Multi_Dim_Common_ST(ST_TO_FLD_MAP_ARRAY &st_map, ST* st,
			ST* common_st, BOUNDS_ARRAY *bounds,
                        COMMON_SNODE_LIST *snode_list)
{

  FLD_HANDLE fld = Get_FLD(st, common_st, st_map);
  Is_True(!fld.Is_Null(), ("NULL field in Pad_Multi_Dim_Common_ST \n"));
  const TY& fld_type = Ty_Table[FLD_type(fld)];

  // Only pad arrays with scalar element types
  TY_IDX etype_idx = TY_etype(fld_type);
  const TY& etype = Ty_Table[etype_idx];

  if (TY_kind(etype) == KIND_SCALAR) {
    // Duplicate the array, such that 
    // we are guaranteed not modify some
    // shared array type (e.g. a TY_pointed() array).

    TY_IDX old_array_ty_idx = FLD_type(fld);
    UINT old_size = Ty_tab.Size();
    TY_IDX new_array_ty_idx = 
      Create_Multi_Dim_Array_Type(FLD_type(fld), bounds);
    TY&  new_array_ty = Ty_Table[new_array_ty_idx];
    TY& old_array_ty = Ty_Table[old_array_ty_idx];
    TY& common_ty = Ty_Table[ST_type(common_st)];
    ARB_HANDLE old_arb_base = TY_arb(old_array_ty);
    ARB_HANDLE new_arb_base = TY_arb(new_array_ty);

    // Update the type attributes of the field and the 
    // corresponding split out ST.
    Set_FLD_type(fld, new_array_ty_idx);
    ST *s = Get_ST(fld, st_map);
    Is_True(s != NULL, ("NULL st to Pad_Multi_Dim_Common_ST \n"));
    Set_ST_type(*s, new_array_ty_idx);

    // Get the array type attributes
    INT64 fld_start = FLD_ofst(fld);
    INT64 fld_old_end = fld_start + TY_size(new_array_ty);
    INT num_dims = TY_AR_ndims(new_array_ty);
    INT64 add_size  = Get_New_Size_Padding(new_array_ty_idx,
					old_array_ty_idx,0);
    add_size = add_size*TY_size(etype_idx);
    
    if (trace_split_common)
      fprintf(TFile, "size added is %lld \n", add_size);

    Set_TY_size(common_ty, TY_size(common_ty) + add_size);
    Set_TY_size(new_array_ty, TY_size(old_array_ty) + add_size);
    
    // compute these for the first dimension
    INT64 padding = Get_New_Size_Padding(new_array_ty_idx, 
					 old_array_ty_idx,1)*
					   TY_size(etype_idx);
    INT64 stride1 = ARB_stride_val(TY_arb(old_array_ty));

    FLD_ITER fld_iter = 
      Make_fld_iter(TY_fld(Ty_Table[ST_type(common_st)]));
    do {
      FLD_HANDLE current_fld (fld_iter);
      TY& fld_type = Ty_Table[FLD_type(current_fld)];
      if (FLD_ofst(current_fld) >= fld_old_end)
	{
	  s = Get_ST(current_fld, st_map);
	  if (s != NULL) { 
	    Set_ST_ofst(*s, ST_ofst(*s) + add_size);
	    Set_FLD_ofst(current_fld, FLD_ofst(current_fld) + add_size);
	  } 
	}

      else if (FLD_ofst(current_fld) >=  (fld_start + stride1))
	{
	  s = Get_ST(current_fld, st_map);
	  if (s != NULL) { 
	    INT64 incr = 
	      ((FLD_ofst(current_fld) - fld_start)/stride1)*padding;
	    Set_ST_ofst(*s, (ST_ofst(*s) + incr));
	    Set_FLD_ofst(current_fld, FLD_ofst(current_fld) + incr);
	  } 
	}
    } while (!FLD_last_field(fld_iter++));

    if (!snode_list->No_Split()) {
      Update_Split_Array(snode_list->Get_Split_Array(), common_st);
    }

  } // if scalar type
}


// ===========================================================
// DESCR: Pad an element of a common
// =========================================================== 
static void 
Pad_Common_ST (ST_TO_FLD_MAP_ARRAY &st_map, ST *st, 
	        ST* common_st, INT padding, COMMON_SNODE_LIST *snode_list)
{
  FLD_HANDLE fld = Get_FLD(st,common_st, st_map);
  Is_True(!fld.Is_Null(), ("NULL field in Pad_Common_ST \n"));
  const TY& fld_type = Ty_Table[FLD_type(fld)];
  // Only pad arrays with scalar element types
  TY_IDX etype_idx = TY_etype(fld_type);
  const TY& etype = Ty_Table[etype_idx];
  if (TY_kind(etype) == KIND_SCALAR) {
 
    // Duplicate the array, such that 
    // we are guaranteed not modify some
    // shared array type (e.g. a TY_pointed() array).
    //
    TY_IDX old_array_ty_idx = FLD_type(fld);
    UINT old_size = Ty_tab.Size();
    TY_IDX new_array_ty_idx = Create_New_Array_Type(FLD_type(fld));

    Is_True(Ty_tab.Size() == old_size+1, ("TY is not entered in Pad_Common_ST \n"));

    TY&  new_array_ty = Ty_Table[new_array_ty_idx];
    TY& old_array_ty = Ty_Table[old_array_ty_idx];
    TY& common_ty = Ty_Table[ST_type(common_st)];

    ARB_HANDLE old_arb_base = TY_arb(old_array_ty);
    ARB_HANDLE new_arb_base = TY_arb(new_array_ty);

    // Update the type attributes of the field and the 
    // corresponding split out ST.
    Set_FLD_type(fld, new_array_ty_idx);
    ST *s = Get_ST(fld, st_map);
    Is_True(s != NULL, ("NULL st to Pad_Common_ST \n"));
    Set_ST_type(*s, new_array_ty_idx);

    // Get the array type attributes
    INT64 fld_start = FLD_ofst(fld);
    INT64 fld_old_end = fld_start + TY_size(new_array_ty);
    INT num_dims = TY_AR_ndims(new_array_ty);
    INT64 stride1 = ARB_stride_val(TY_arb(old_array_ty));

    // Modify the array-type and estimate the increase in size.
    INT64 num_cols = TY_size(old_array_ty)/stride1;
    INT64 add_size = num_cols*padding;

    if (trace_split_common)
      fprintf(TFile, "size added is %lld \n", add_size);
    Set_TY_size(common_ty, TY_size(common_ty) + add_size);
    Set_TY_size(new_array_ty, TY_size(old_array_ty) + add_size);

    ARB_HANDLE old_arb = old_arb_base[num_dims-1];
    ARB_HANDLE new_arb = new_arb_base[num_dims-1];
    
    Is_True((ARB_stride_val(old_arb) != 0),("Zero stride value found in Pad_Common_St\n"));

    Set_ARB_ubnd_val(new_arb, 
		     ARB_ubnd_val(old_arb)+
		     padding/ARB_stride_val(old_arb));
    // Update the arb fields for the array
    for (INT dim = 1; dim < TY_AR_ndims(old_array_ty); dim++) {
      // Update the stride for each dimension by adding:
      //
      //    (num_cols_per_stride * padding_per_col)
      //
      //  add_stride = (TY_AR_stride_val(array_ty, dim)/stride1) * padding;
      //    TY_AR_stride_val(array_ty, dim) += add_stride;
      INT numdims = TY_AR_ndims(old_array_ty);
      ARB_HANDLE new_arb = old_arb_base[num_dims-1-dim];      
      ARB_HANDLE old_arb = new_arb_base[num_dims-1-dim];

      INT add_stride = (ARB_stride_val(old_arb)/stride1) * padding;
      Set_ARB_stride_val(new_arb, ARB_stride_val(new_arb)+add_stride);
    }

    // Increase the offsets for fields that overlap or follow
    // this one (no field should straddle a column).
    //
    FLD_ITER fld_iter = 
      Make_fld_iter(TY_fld(Ty_Table[ST_type(common_st)]));
    do {
      FLD_HANDLE current_fld (fld_iter);
      TY& fld_type = Ty_Table[FLD_type(current_fld)];
      if (FLD_ofst(current_fld) >= fld_old_end)
	{
	  s = Get_ST(current_fld, st_map);
	  if (s != NULL) { 
	    Set_ST_ofst(*s, ST_ofst(*s) + add_size);
	    Set_FLD_ofst(current_fld, FLD_ofst(current_fld) + add_size);
	  } 
	}
      else if (FLD_ofst(current_fld) >=  (fld_start + stride1))
	{
	  s = Get_ST(current_fld, st_map);
	  if (s != NULL) { 
	    INT64 incr = 
	      ((FLD_ofst(current_fld) - fld_start)/stride1)*padding;
	    Set_ST_ofst(*s, (ST_ofst(*s) + incr));
	    Set_FLD_ofst(current_fld, FLD_ofst(current_fld) + incr);
	  } 
	}

    } while (!FLD_last_field(fld_iter++));
    if (IPA_Enable_Split_Common)
      Update_Split_Array(snode_list->Get_Split_Array(), common_st);
   }
} // Pad_Common_ST


// =========================================================== 
// DESCR: Padding_Size
//        return the pad size
// =========================================================== 
static INT64
Padding_Size(const ST* s)
{
  Is_True((TY_kind(ST_type(s)) == KIND_ARRAY), ("Expecting an ST of KIND_ARRAY \n"));
  INT lower, upper, stride;
  INT64 pad_size = 0;
  INT64 padding = 0;

  INT num_dims = ARB_dimension(TY_arb(ST_type(s)));
  if (num_dims > 1) 
    {
      ARB_HANDLE arb = TY_arb(ST_type(s))[num_dims - 1];
      if (ARB_const_lbnd(arb) && ARB_const_ubnd(arb) &&
	  ARB_const_stride(arb))
	{
	  lower = ARB_lbnd_val(arb);
	  upper = ARB_ubnd_val(arb);
	  stride = ARB_stride_val(arb);
	  pad_size = abs(upper - lower + 1);
	  padding = Common_Array_Pad_Size(pad_size*stride);
	  return padding;
	}
      else
	Fail_FmtAssertion("Non constant bounds in Padding_Size \n");
    }
  Fail_FmtAssertion("Expecting num dims > 1 in Padding_Size \n");
  return 0;
}

// =========================================================== 
// DESCR: Pad_Multi_Dim_Global_ST
//        Pad a global ST
// =========================================================== 
static void   
Pad_Multi_Dim_Global_ST(ST *s, BOUNDS_ARRAY* bounds)
{
  const TY& fld_type = Ty_Table[ST_type(s)];

  Is_True(TY_kind(fld_type) == KIND_ARRAY, ("Expecting KIND_ARRAY in Pad_Multi_Dim_Global_ST \n"));

  // Only pad arrays with scalar element types
  TY_IDX etype_idx = TY_etype(fld_type);
  const TY& etype = Ty_Table[etype_idx];

  if (TY_kind(etype) == KIND_SCALAR) {
    // Duplicate the array, such that 
    // we are guaranteed not modify some
    // shared array type (e.g. a TY_pointed() array).
    TY_IDX old_array_ty_idx = ST_type(s);
    TY_IDX new_array_ty_idx = 
      Create_Multi_Dim_Array_Type(ST_type(s), bounds);
    TY&  new_array_ty = Ty_Table[new_array_ty_idx];
    TY& old_array_ty = Ty_Table[old_array_ty_idx];
    Set_ST_type(*s, new_array_ty_idx);
    INT num_dims = TY_AR_ndims(new_array_ty);
    INT64 add_size  = Get_New_Size_Padding(new_array_ty_idx,
					   old_array_ty_idx,0);
    add_size = add_size*TY_size(etype_idx);
    if (trace_split_common)
      fprintf(TFile, "size added is %lld \n", add_size);
    Set_TY_size(new_array_ty, TY_size(old_array_ty) + add_size);
  }
} 


// =========================================================== 
// DESCR: Pad_Multi_Dim_Common
//        pad multiple dimensions of arrays
// =========================================================== 
static void
Pad_Multi_Dim_Common(COMMON_SNODE_LIST* common_snode_list)
{
  COMMON_SNODE_LIST_ITER  common_snode_list_iter(common_snode_list);
  // walk the commons in this list and see if any of them
  // need to be padded
  for (common_snode_list_iter.First();
       !common_snode_list_iter.Is_Empty();
       common_snode_list_iter.Next()) 
    {
      COMMON_SNODE *common_snode = common_snode_list_iter.Cur();
      ST *s  = common_snode->Get_ST();
      
      if (ST_sclass(s) == SCLASS_COMMON) {
	ST *common_st = ST_base(s);
	ST_TO_FLD_MAP_ARRAY *st_map = common_snode_list->Get_Map();
	INT64 padding = 0;
	if (common_snode->Pad())
	  {
	    Pad_Multi_Dim_Common_ST(*st_map, s, common_st,
				    common_snode->Get_Bounds_Array(),
				    common_snode_list);
	  }
      }
      else 
	if ((ST_sclass(s) == SCLASS_UGLOBAL) && common_snode->Pad())
	  {
	    Pad_Multi_Dim_Global_ST(s,
				    common_snode->Get_Bounds_Array());

	  }
    }
}

// =========================================================== 
// DESCR: Pad_Common
//        Pad elements of a common
// =========================================================== 
static void
Pad_Common (COMMON_SNODE_LIST *common_snode_list)
{
  if (common_snode_list->No_Pad())
    return;

  COMMON_SNODE_LIST_ITER  common_snode_list_iter(common_snode_list);
  // walk the commons in this list and see if any of them
  // need to be padded

  for (common_snode_list_iter.First();
       !common_snode_list_iter.Is_Empty();
       common_snode_list_iter.Next()) 
    {
      COMMON_SNODE *common_snode = common_snode_list_iter.Cur();
      ST *s  = common_snode->Get_ST();
      ST *common_st = ST_base(s);

      ST_TO_FLD_MAP_ARRAY *st_map = common_snode_list->Get_Map();

      INT64 padding = 0;
      if (common_snode->Pad())
	{
	  padding = Padding_Size(s);
	  if (trace_split_common) 
	    fprintf (TFile, "st = %s, pad size = %lli \n", ST_name(s),padding);
      
	  // Make the padding both a multiple of stride0 and
	  // the largest possible alignment constrain (16).
	  // NOTE: The largest stride we
	  //
	  INT64 div_padding;
	  INT64 stride0;

	  INT num_dims = ARB_dimension(TY_arb(ST_type(s)));
	  ARB_HANDLE arb = TY_arb(ST_type(s))[num_dims-1];
	  stride0 = ARB_stride_val(arb);
	  
	  if (stride0 > MAX_ALIGN)
	    div_padding = stride0;
	  else
	    div_padding = MAX_ALIGN;
	  
	  if (div_padding > padding)
	    padding = div_padding;
	  else
	    padding = (div_padding - (padding % div_padding)) + padding;

	  Pad_Common_ST (*st_map,s,common_st,padding,common_snode_list);
	} // if paddable array
    }
}


// =========================================================== 
// DESCR: IPO_Pad_Symtab
//        update the symbol table
// =========================================================== 
extern void
IPO_Pad_Symtab(COMMON_SNODE_TBL *common_snode_tbl)
{
  trace_split_common =  Get_Trace(TP_IPA, IPA_TRACE_SPLIT_COMMON);

  COMMON_SNODE_LIST *snode_list;
  COMMON_SNODE_TBL_ITER common_snode_tbl_iter(common_snode_tbl);
  
  Create_ST_TO_FLD_MAP(common_snode_tbl);

  STR_IDX common_name;
  while (common_snode_tbl_iter.Step (&common_name, &snode_list)) {
    if (!ST_is_not_used(snode_list->Get_ST()) && !snode_list->No_Pad()) {
      Pad_Multi_Dim_Common(snode_list);
    }
  }
}

// =========================================================== 
// DESCR: IPO_Pad_Arrays
//        update the whirl to reflect the new size
// =========================================================== 

static void 
IPO_Pad_Arrays(WN* func_nd)
{
   // pad the arrays after the symtab has been fixed.
   OPCODE opc = WN_opcode(func_nd);
   if (OPCODE_operator(opc) == OPR_ARRAY && (WN_num_dim(func_nd) > 1)) {
      WN *base = WN_array_base(func_nd);
      if ((OPCODE_operator(WN_opcode(base)) == OPR_LDA) && 
	  (TY_kind(ST_type(WN_st(base))) == KIND_ARRAY)) {
	 ARB_HANDLE arb_base = TY_arb(ST_type(WN_st(base)));
	 BOOL set_base = FALSE;
	 for (INT i=0; i<WN_num_dim(func_nd); ++i) {
	    WN *dim = WN_array_dim(func_nd,i);
	    if (OPCODE_operator(WN_opcode(dim)) == OPR_INTCONST) {
	       ST *st = WN_st(base);
	       INT num_dim = WN_num_dim(func_nd);
	       ARB_HANDLE arb = arb_base[i];
	       if (Is_Const_Bounds(arb)) {
		  INT64 st_size = Num_Elements(arb);
		  if (WN_const_val(dim) != st_size) {
		     WN_const_val(dim) = st_size;
		     if (TY_pointed(WN_ty(base)) != ST_type(st) &&
			 TY_kind(TY_pointed(WN_ty(base))) == KIND_ARRAY)
			{
			   if (!set_base) {
			      TY_IDX idx = Make_Pointer_Type(ST_type(st));
			      WN_set_ty(base, idx);
			   }
			}
		  }
	       }
	    }
	 }
      }
   }
   if (opc == OPC_BLOCK) {
      for (WN* wn = WN_first(func_nd); wn; wn = WN_next(wn)) {
	 IPO_Pad_Arrays(wn);
      }
   } else {
      for (INT kid=0; kid<WN_kid_count(func_nd); kid++) {
	 IPO_Pad_Arrays(WN_kid(func_nd,kid));
      }
   }
} // IPO_Pad_Arrays


// =========================================================== 
// DESCR: IPO_Pad_Whirl
//        update the whirl to reflect the new size
// =========================================================== 
extern void
IPO_Pad_Whirl(IPA_NODE* node)
{
  WN *w = node->Whirl_Tree();
  Is_True(w != NULL, (" NULL whirl encountered \n"));
  IPO_Pad_Arrays(w);
#ifdef Is_True  
  WN_verifier(w);
  Verify_GLOBAL_SYMTAB();
#endif
}


