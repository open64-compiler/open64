/*
 * Copyright (C) 2011 Advanced Micro Devices, Inc.  All Rights Reserved.
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


// -*-C++-*-
/* ====================================================================
 * ====================================================================
 *
 * Module: pad.cxx
 * $Revision: 1.4 $
 * $Date: 04/12/21 14:58:29-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.pad.cxx $
 *
 *
 * Description:
 *
 * Pad local arrays
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include "lnopt_main.h"
#include "lwn_util.h"
#include "lego_util.h"
#include "stab.h"
#include "strtab.h"
#include "stblock.h"
#include "pad.h"
typedef HASH_TABLE<ST*,BOOL> S_HTABLE;

static ST* Create_Local_Array_ST (TY_IDX elem_ty, INT num,char *name) {
  ST *st;
  TY *arr_ty;
  
  UINT32 elem_size = TY_size(elem_ty);

  TY_IDX ty_idx;
  TY& ty = New_TY (ty_idx);
  TY_Init(ty, elem_size * num, KIND_ARRAY, MTYPE_UNKNOWN,
	       Save_Str (name));
  ARB_HANDLE arb = New_ARB ();
  ARB_Init (arb, 0, num-1, elem_size);
  Set_ARB_first_dimen (arb);
  Set_ARB_last_dimen (arb);
  Set_TY_etype(ty,elem_ty);

  Set_TY_arb (ty, arb);
  Set_TY_align_exp (ty_idx, 8);

  st = New_ST(CURRENT_SYMTAB);

  ST_Init (st,
           Save_Str(name),
           CLASS_VAR,
           SCLASS_AUTO,
           EXPORT_LOCAL,
           ty_idx);
  Set_ST_is_temp_var(st);
  Set_ST_pt_to_unique_mem(st);

  return st;
}

static INT Array_Pad_Size(INT column_size)
{
  // Given a multidimensional array, come up with a pad size
  // (in bytes) for the first dimension.
  INT bad_dim_size = 256*8;
  INT min_size = 230*8;
  INT mod = column_size % bad_dim_size;
  if ((column_size > min_size) && (mod < bad_dim_size/20)) {
    return (bad_dim_size/20 - mod);
  } else if ((column_size > bad_dim_size) &&
             ((bad_dim_size - mod) < bad_dim_size/20)) {
    return (bad_dim_size/20 + (bad_dim_size - mod));
  }

  bad_dim_size = 128*8;
  min_size = 115*8;
  mod = column_size % bad_dim_size;
  if ((column_size > min_size) && (mod < bad_dim_size/20)) {
    return (bad_dim_size/20 - mod);
  } else if ((column_size > bad_dim_size) &&
             ((bad_dim_size - mod) < bad_dim_size/20)) {
    return (bad_dim_size/20 + (bad_dim_size - mod));
  } else {
    return 0;
  }
} 

TY_IDX Create_New_Array_Type(TY_IDX old_array_ty)
{
   TY_IDX new_array_ty = Copy_TY(old_array_ty);
   ARB_HANDLE old_arb_base = TY_arb(old_array_ty);
   INT old_arb_idx=0;
   BOOL first_time = TRUE;
   ARB_HANDLE first_arb;
   BOOL done = FALSE;
   while (!done) {
     ARB_HANDLE tmp = old_arb_base[old_arb_idx];
     ARB_HANDLE arb = New_ARB ();
     if (first_time) {
       first_arb = arb;
       first_time = FALSE;
     }
     ARB_copy(arb,tmp);
     old_arb_idx++;
     done = ARB_last_dimen(tmp);
   };

   Set_TY_arb (new_array_ty, first_arb);

   return new_array_ty;
} // Create_New_Array_Type



// pad the arrays after the symtab has been fixed.
static void Pad_Arrays(WN* wn)
{
  OPCODE opc = WN_opcode(wn);
  if (OPCODE_operator(opc) == OPR_ARRAY && (WN_num_dim(wn) > 1)) {
    WN *base = WN_array_base(wn);
    if (WN_operator(base) == OPR_LDA) {
      WN *last_dim = WN_array_dim(wn,WN_num_dim(wn)-1);
      if (WN_operator(last_dim) == OPR_INTCONST) {
        ST *st = WN_st(base);
        INT st_size =
            TY_AR_stride_val(ST_type(st), TY_AR_ndims(ST_type(st))-2) / WN_element_size(wn);
        if (WN_const_val(last_dim) != st_size) {
          WN_const_val(last_dim) = st_size;
          if (TY_pointed(WN_ty(base)) != ST_type(st) &&
            TY_kind(TY_pointed(WN_ty(base))) == KIND_ARRAY)
            WN_set_ty(base, Make_Pointer_Type(ST_type(st)));
          }
        }
      }
    }
    if (opc == OPC_BLOCK) {
      for (WN* tmp = WN_first(wn); tmp; tmp = WN_next(tmp)) {
        Pad_Arrays(tmp);
      }
    } else {
      for (INT kid=0; kid<WN_kid_count(wn); kid++) {
        Pad_Arrays(WN_kid(wn,kid));
      }
    }
} 

static BOOL Local_Multid_Array(ST *st)
{
  return ((ST_sclass(st) == SCLASS_AUTO) &&
	  ST_base_idx(st) == ST_st_idx(st) &&
	  (ST_class(st) == CLASS_VAR) && !ST_is_not_used(st) &&
	  !ST_has_nested_ref(st) &&
	  (TY_kind(ST_type(st)) == KIND_ARRAY) &&
          (TY_kind(TY_etype(ST_type(st))) == KIND_SCALAR) &&
	  (TY_AR_ndims(ST_type(st)) > 1) &&
	  (TY_size(ST_type(st)) > 0) &&
          (!ST_is_reshaped(st)) &&
          (!ST_is_fill_align(st)) &&
	  (!ST_is_initialized(st)));
}

// Is this a simple read of a variable
static BOOL IO_element_read(WN *item)
{
  if (WN_operator(item) == OPR_IO_ITEM) {
    if (WN_io_item(item) == IOL_VAR) {
      WN *parent = LWN_Get_Parent(item);
      if (WN_opcode(parent) == OPC_IO) {
        if (WN_io_statement(parent) == IOS_READ) {
          return TRUE;
        } else if (WN_io_statement(parent) == IOS_CR_FRF) {
          return TRUE;
        } else if (WN_io_statement(parent) == IOS_CR_FRU) {
          return TRUE;
        }
      }
    }
  }
  return FALSE;
}

// Normally local arrays will have constant bounds, but if a procedure is
// inlined, the arrays that were passed to the inlined procedure may be 
// promoted from passed arrays to local arrays.  In this case, local 
// arrays that children of IO_ITEMs may have variable bounds.  The code
// below does not deal wiith variable bounds correctly (it assumes the
// counds were constant). Unfortunately, there are no DU chains for these
// variable bounds and their definitions, so LNO will have a hard time
// constant propagating them. But maybe inlining or constant propagation
// could be smarter and propagate the values.  (PV 652513)

static BOOL Has_Variable_Bounds(WN* wn_array)
{ 
  FmtAssert(WN_operator(wn_array) == OPR_ARRAY,
    ("Has_Variable_Bounds: Expecting an OPR_ARRAY node"));
  for (INT i = 0; i < WN_num_dim(wn_array); i++) { 
    WN* wn = WN_array_dim(wn_array, i);
    if (WN_operator(wn) != OPR_INTCONST)
      return TRUE; 
  } 
  return FALSE; 
} 

// find every local array
// make sure it's accessed as a local array and
// make sure it's never passed
// enter the bad ones into the hash table
static void Check_Arrays(WN *wn,S_HTABLE *bad_behaved)
{
  OPCODE opc = WN_opcode(wn);
  if (opc == OPC_BLOCK) {
    WN *kid=WN_first(wn);
    while (kid) {
      Check_Arrays(kid,bad_behaved);
      kid = WN_next(kid);
    }
  } else if (OPCODE_operator(opc) == OPR_LDA) {
    if (Local_Multid_Array(WN_st(wn))) {
      TY_IDX array_ty = Lego_Get_Array_Type(WN_st(wn));
      BOOL ok = FALSE;
      WN *parent = LWN_Get_Parent(wn);
      if ((WN_operator(parent) == OPR_ARRAY) &&
	  (wn == WN_array_base(parent)) &&
	  (!WN_offset(wn)) &&
          (WN_element_size(parent) > 0) &&    // Not a F90 strided array
	  (!Has_Variable_Bounds(parent)) && 
	  (TY_AR_ndims(array_ty) == WN_num_dim(parent)) &&
	  // make sure the offset is less than the element size
	  (abs(WN_offset(wn)) < TY_AR_stride_val(array_ty,TY_AR_ndims(array_ty)-1))) {
        WN *grandparent = LWN_Get_Parent(parent);
	OPCODE gopc = WN_opcode(grandparent);
	if (OPCODE_is_load(gopc) ||
	    ((OPCODE_operator(gopc) == OPR_ISTORE) &&
	     (parent == WN_kid1(grandparent)))
	     
	     ||
            (IO_element_read(grandparent))) {
	  ok = TRUE;
        }
      }
      if (!ok) {
	bad_behaved->Enter_If_Unique(WN_st(wn),1);
      }
    }
  } else {
    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      Check_Arrays(WN_kid(wn,kidno),bad_behaved);
    }
  }
}

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
inline INT64
Num_Elements(ARB_HANDLE arb)
{
  Is_True((ARB_const_ubnd(arb) && ARB_const_lbnd(arb) && ARB_const_stride(arb)), ("Expecting constant bounds in Num_Elements\n"));

  return abs(ARB_ubnd_val(arb) - ARB_lbnd_val(arb) + 1);
}

//=======================================================
// DESCR: Store_Orig_Dims
//        store the original dimensions  
//=======================================================
static BOUNDS_ARRAY*
Store_Orig_Dims(ST* s)
{
 Is_True((TY_kind(ST_type(s)) == KIND_ARRAY), (" Expecting KIND_ARRAY in Store_Orig_Dims \n"));

 BOUNDS_ARRAY* b = CXX_NEW(BOUNDS_ARRAY(&LNO_local_pool), &LNO_local_pool);

 INT lower, upper, stride;
 INT num_dims =
   ARB_dimension(TY_arb(ST_type(s)));
 ARB_HANDLE arb_base = TY_arb(ST_type(s));

 if (num_dims > 1) {
   for (INT i=0; i<num_dims; ++i)
     {
       const ARB_HANDLE arb = arb_base[num_dims-1-i];
       if (ARB_const_lbnd(arb) && ARB_const_ubnd(arb) &&
           ARB_const_stride(arb))
         {
           lower = ARB_lbnd_val(arb);
           upper = ARB_ubnd_val(arb);
           stride = ARB_stride_val(arb);
           INT id = b->Newidx();
           BOUNDS *belement =  &(*b)[id];
           belement->Init(upper, lower,stride);
           belement->Set_Constant();
         }
       else
         {
           INT id = b->Newidx();
           BOUNDS *belement =  &(*b)[id];
           belement->Init();
         }
     } 
 }
 return b;
}

//=========================================================================
// DESCR: Padding_Threshold
//        return TRUE if size increase is < 10% else return FALSE
//=========================================================================
static BOOL
Padding_Threshold(INT64 pad_size, INT pad_dim, BOUNDS_ARRAY *b,
		  ARB_HANDLE arb_in)
{
  Is_True(pad_dim <= b->Lastidx(), ("pad dim is TOO high in Padding Threshold \n"));
  BOUNDS *belement = &(*b)[pad_dim];
  ARB_HANDLE arb = arb_in[b->Lastidx()-pad_dim];
  Is_True((ARB_const_ubnd(arb) && ARB_const_lbnd(arb) && ARB_const_stride(arb)), ("constant bounds expected \n"));

  INT size = abs(ARB_ubnd_val(arb) - ARB_lbnd_val(arb) + 1);
  float padt = ((pad_size/(*b)[0].Get_Stride() + size)*100)/size;
  if (padt > 110) {
    return FALSE;
  }
  return TRUE;
}


//=============================================================
// DESCR: Pad_Size
//        return the size of the padding for dimension pad_dim
//        add extra padding for certain dimensions
//=============================================================
INT64
Pad_Size(INT pad_dim, INT padding, INT end_dim,
         BOUNDS_ARRAY *bounds, BOOL extra_pad)
{
  INT size = 1;
  INT size2 = 1;
  INT pad_dim_size = 1;
  if (end_dim == 0)
    return padding;
  for (INT i=0; i<=end_dim; ++i) {
    BOUNDS *b = &(*bounds)[i];
    INT upper = b->Get_Upper();
    INT lower = b->Get_Lower();
    if (i != pad_dim)
      size = size* abs(upper-lower+1);
    else
      pad_dim_size = (upper-lower+1);
    size2 = size2*abs(upper-lower+1);
  }
  INT total_size = size2+(padding/(*bounds)[0].Get_Stride());
  float fpad_size = float(total_size/size - pad_dim_size);
  if (fpad_size > 0.001)
    fpad_size = fpad_size - .001;
  INT64 pad_size;
  if (extra_pad)
    pad_size = (INT64)(fpad_size+2)*(*bounds)[0].Get_Stride();
  else
    pad_size = (INT64)(fpad_size+1)*(*bounds)[0].Get_Stride();
  if (pad_size == 0)
    return (*bounds)[0].Get_Stride();
  else 
    return pad_size;
}


//========================================================
// DESCR: Pad_Dim
// returns TRUE if size is a power of 2
//========================================================
static
BOOL Pad_Dim(BOUNDS *b)
{
  Is_True(b->Is_Constant(), ("Expecting constant bounds in Pad_Dim \n"));
  INT one_count = 0;
  INT size = abs (b->Get_Upper() - b->Get_Lower() + 1);
  while (size != 0)
    {
      size = size >> 1;
      if (size & 1)
        ++one_count;
    }
  if (one_count > 1)
    return FALSE;
  return TRUE;
}

// ===========================================================
// DESCR: Get_Pad_Dim
//        Get dimension to pad
// ===========================================================
static INT
Get_Pad_Dim(INT last_dim, INT element_size, BOUNDS_ARRAY *b)
{
  INT size, lower, upper, max_element, max_dim;
  max_element = 0;
  max_dim = last_dim;
  // try just padding the last dimension. Pick the last
  // dimension even if other dimensions are bigger due to
  // avoiding backtracking
  INT last_dim_size = abs((*b)[last_dim].Get_Upper() -
                          (*b)[last_dim].Get_Lower() + 1);
  for (INT i=last_dim; i >= 0; --i)
    {
      BOUNDS* bounds_elem = &(*b)[i];
      lower = bounds_elem->Get_Lower();
      upper = bounds_elem->Get_Upper(); 
      size = abs(upper-lower+1);

      if (((size*element_size) % (128*8)) == 0)
	return i;
      if (size > max_element)
        {
          if (size > (last_dim_size*130)/100)
            {
              max_element = size;
              max_dim = i;
            }
          else
            {
              max_element = last_dim_size;
              max_dim = last_dim;
            }
        }
    }
   return max_dim;
 }

// ===========================================================
// DESCR: Create_New_Array_Type
//        Create a new array type
// ===========================================================
static TY_IDX 
Create_New_Array_Type_To_Pad(TY_IDX old_array_ty)
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
  TY_IDX ty_index = Create_New_Array_Type_To_Pad(old_array_ty);

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

//========================================================
// DESCR: Update_Pad_Size
//        update the pad size after padding 
//========================================================
static INT64 
Update_Pad_Size(INT last_dim, BOUNDS_ARRAY *barray)
{
  INT64 pad_size = 1;
  for (INT i=0; i<= last_dim; ++i)
    { 
     INT upper =  (*barray)[i].Get_Upper();
     INT lower =  (*barray)[i].Get_Lower();
  
     pad_size = pad_size* (abs(upper - lower + 1));
    } 

  return pad_size;
}

//=======================================================
// DESCR: Update_Bounds_After_Padding
//        update the bounds information after padding
//=======================================================
static void
Update_Bounds_After_Padding(INT64 pad_size, 
			    INT pad_dim, 
                            BOUNDS_ARRAY *barray)
{
  INT64 element_size = (*barray)[0].Get_Stride();
  INT64 stride = (*barray)[pad_dim].Get_Stride();

  for (INT i = pad_dim; i < barray->Elements(); ++i) {	
    BOUNDS& bounds = (*barray)[i];
    if (i == pad_dim) {
      // override padding size if requested by flag
      pad_size = (LNO_Local_Pad_Size != (UINT32)-1) ? 
                 LNO_Local_Pad_Size :
                 (pad_size + element_size) / element_size;
      bounds.Set_Upper(pad_size + bounds.Get_Upper());
    }
    bounds.Set_Stride(stride);
    stride *= abs(bounds.Get_Upper() - bounds.Get_Lower() + 1);
  }
}
// ===========================================================
// DESCR: Get_New_Size_Padding
//        return the padding for dimensions starting at 
//        start dim
//        if start_dim =0, returns total padding
//        if start_dim =1, returns padding for all dims except 
//           last dimension
// ===========================================================
static INT64
Get_New_Size_Padding(TY_IDX new_array_ty_idx, TY_IDX old_array_ty_idx,
		     INT32 start_dim)
{
 TY&  new_array_ty = Ty_Table[new_array_ty_idx];
 TY& old_array_ty = Ty_Table[old_array_ty_idx];

  INT64 old_size = 1;
  INT64 new_size = 1;
  INT num_dims = TY_AR_ndims(new_array_ty);
  ARB_HANDLE old_arb_base = TY_arb(old_array_ty);
  ARB_HANDLE new_arb_base = TY_arb(new_array_ty);

 Is_True((start_dim < num_dims), ("start dim = %d , num_dims = %d  in Get_New_Size_Padding \n", start_dim, num_dims));

  for (INT i=start_dim; i<num_dims; ++i)
    {
      ARB_HANDLE old_arb = old_arb_base[i];
      ARB_HANDLE new_arb = new_arb_base[i];
      old_size = old_size*Num_Elements(old_arb);
      new_size = new_size*Num_Elements(new_arb);
    }
  new_size = new_size - old_size;
  return new_size;
}

//===================================================================
// DESCR: Pad_ST
//        create a new type for the ST
//===================================================================
static void
Pad_Local_ST(ST* s, BOUNDS_ARRAY *bounds)
{
  Is_True((TY_kind(ST_type(s)) == KIND_ARRAY) && (ST_sclass(s) ==SCLASS_AUTO),("Expecting a local ARRAY in Pad_Local_ST \n"));
;

  TY_IDX old_array_ty_idx = ST_type(s);
  TY_IDX new_array_ty_idx = 
    Create_Multi_Dim_Array_Type(ST_type(s), bounds);

  TY& new_array_ty = Ty_Table[new_array_ty_idx];
  TY& old_array_ty = Ty_Table[old_array_ty_idx];
  TY_IDX etype_idx = TY_etype(old_array_ty);

  INT64 add_size  = Get_New_Size_Padding(new_array_ty_idx,
					 old_array_ty_idx,0);
  add_size = add_size*TY_size(etype_idx);
  Set_TY_size(new_array_ty, TY_size(old_array_ty) + add_size);
  Set_ST_type(*s, new_array_ty_idx);  
}

//===================================================================
// DESCR: Pad_Local_Array
//        check if padding is needed, is so pad the array
//===================================================================
static void  
Pad_Local_Array(ST* s)
{
  Is_True((TY_kind(ST_type(s)) == KIND_ARRAY) && (ST_sclass(s)==SCLASS_AUTO),("Expecting a local ARRAY in Pad_Local_Array \n"));

  BOUNDS_ARRAY *bounds_array = Store_Orig_Dims(s);
  Is_True(bounds_array!=NULL, (" NULL bounds array in Pad_Local_Array \n"));
  BOOL to_pad = FALSE;
  INT64 lower, upper, stride;
  INT num_dims = ARB_dimension(TY_arb(ST_type(s)));
  ARB_HANDLE arb_base = TY_arb(ST_type(s));
  if (num_dims > 1) {
    INT64 pad_size = 1;
    INT64 padding;
    INT pad_dim;
    BOUNDS *b = &(*bounds_array)[0];
    if (b->Is_Constant()) {
      pad_size = abs(b->Get_Upper() - b->Get_Lower() + 1);
    }
    else return; // non constant bounds found
    pad_size = 1;
    for (INT i=0; i<num_dims-1; ++i) {
      b = &(*bounds_array)[i];
      if (b->Is_Constant()) {
	lower = b->Get_Lower();
	upper = b->Get_Upper();
	pad_size = pad_size * abs(upper-lower+1);
	padding =
	  Array_Pad_Size(pad_size*(*bounds_array)[0].Get_Stride());
	if (padding) {
	  INT element_size = (*bounds_array)[0].Get_Stride();
	  if (padding) {
	    if ((8 % element_size) == 0) {
	      if (((pad_size*(*bounds_array)[0].Get_Stride() + padding)
		   % 16) == 0) {
		padding += 8;
	      }
	    }
	    for (INT j =i; j >= 0; --j) {
	      if ((j != i) && Pad_Dim(&(*bounds_array)[j])) 
		pad_dim = j;
	      else
		pad_dim =
		  Get_Pad_Dim(i,((*bounds_array)[0]).Get_Stride(),bounds_array);
	      INT pad_size1=0;
	      if (j == i)
		pad_size1 = Pad_Size(pad_dim,padding,i,bounds_array,TRUE);
	      else if (Pad_Dim(&(*bounds_array)[j]))
		pad_size1 =
		  Pad_Size(pad_dim,padding,i,bounds_array,FALSE);
		
	      if (Padding_Threshold(pad_size1,pad_dim,bounds_array,arb_base))
		{
		  to_pad = TRUE;
		  Update_Bounds_After_Padding(pad_size1, pad_dim, bounds_array);
		  pad_size = Update_Pad_Size(i,bounds_array);
		}
	    }
	  }
	}
      }
    }
  }
  if (to_pad)
    {
      Pad_Local_ST(s, bounds_array);
    }
}

//====================================================================
// DESCR: Pad_Local_Arrays_In_Whirl()
//        walk whirl and update size of symbol if padded
//====================================================================
static void 
Pad_Local_Arrays_In_Whirl(WN* func_nd,S_HTABLE *bad_behaved)
{
  // pad the arrays after the symtab has been fixed.
  OPCODE opc = WN_opcode(func_nd);
  if (OPCODE_operator(opc) == OPR_ARRAY &&
      (WN_num_dim(func_nd) > 1) &&
      // don't pad F90 strided arrays
      WN_element_size(func_nd) > 0) {
    WN *base = WN_array_base(func_nd);
    if (OPCODE_operator(WN_opcode(base)) == OPR_LDA) {
      if (Local_Multid_Array(WN_st(base)) && !bad_behaved->Find(WN_st(base))) {
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
	        if ((TY_pointed(WN_ty(base)) != ST_type(st)) &&
		    (TY_kind(TY_pointed(WN_ty(base))) == KIND_ARRAY))
		  {
		    if (!set_base) {
		      TY_IDX idx = Make_Pointer_Type(ST_type(st));
		      WN_set_ty(base, idx);
		      set_base = TRUE;
		    }
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
     Pad_Local_Arrays_In_Whirl(wn,bad_behaved);
    }
  } else {
    for (INT kid=0; kid<WN_kid_count(func_nd); kid++) {
      Pad_Local_Arrays_In_Whirl(WN_kid(func_nd,kid),bad_behaved);
    }
  }
} // Pad_Local_Arrays_In_Whirl

//---------------------------------------------------------------------
// DESCR: Pad_Multi_Dim
//        Pad "clean" local arrays if dimension sizes are close to
//        power of 2
//
//---------------------------------------------------------------------
extern void 
Pad_Multi_Dim(WN* func_nd)
{
  MEM_POOL_Push(&LNO_local_pool);
  S_HTABLE *bad_behaved = 
    CXX_NEW(S_HTABLE(50,&LNO_local_pool),&LNO_local_pool);
  Check_Arrays(func_nd,bad_behaved);

  // look at all local arrays
  ST *st;
  INT i;
  FOREACH_SYMBOL (CURRENT_SYMTAB,st,i) {
    if (Local_Multid_Array(st) && !bad_behaved->Find(st)) { 
      Pad_Local_Array(st);
    }
  }
  Pad_Local_Arrays_In_Whirl(func_nd,bad_behaved);

  MEM_POOL_Pop(&LNO_local_pool);
}

//=======================================================================
// make sure that the first dimension of a local array
// isn't near a degenerate boundary
// since we're reshaping the array, we need to make sure the array
// isn't passed not inited
//=======================================================================
extern void Pad_First_Dim_Degenerates(WN *func_nd)
{
  Pad_Multi_Dim(func_nd);
}


// make sure size is greater than 10% more than 1600 or 256K
extern void Pad_Degenerates()
{
  ST *st;
  // look at all local arrays
  INT i;
  FOREACH_SYMBOL (CURRENT_SYMTAB,st,i) {
    if ((ST_sclass(st) == SCLASS_AUTO) &&
	(ST_class(st) == CLASS_VAR) && !ST_is_not_used(st) &&
	(TY_size(ST_type(st)) > 0) &&
	!ST_is_fill_align(st) &&
	!ST_has_nested_ref(st) &&
	(TY_kind(ST_type(st)) == KIND_ARRAY) &&
        (!ST_is_reshaped(st))) { 
      INT size = TY_size(ST_type(st));
      INT set_size = 256*1024;
      INT mod = size % set_size;
      INT pad_size=0;
      if ((size > 0.9*set_size) && (mod < set_size/20)) {
        pad_size = set_size/20 - mod;
      } else if ((size > 0.9*set_size) && ((set_size - mod) < set_size/20)) {
        pad_size = set_size/20 + (set_size - mod);
      }
      size += pad_size;
      set_size = 16*1024;
      mod = size % set_size;
      if ((size > 0.9*set_size) && (mod < set_size/20)) {
        pad_size += set_size/20 - mod;
      } else if ((size > 0.9*set_size) && ((set_size - mod) < set_size/20)) {
        pad_size += set_size/20 + (set_size - mod);
      }
      size += pad_size;
#ifdef TARG_X8664
      /* Bug 4902 - Avoid the Store and Load Forward [31:12] penalty. */
      if (!Is_Target_Anyx86() &&
	  !Is_Target_EM64T() && 
//	  !Is_Target_Core() &&  //good for core
	  !Is_Target_Pentium4() &&
	  size >= 4096) {
	UINT tmp = size;
	tmp = ~tmp;
	tmp += 1;
	tmp = ~tmp;
	if ((tmp & size) == 0) { // power of 2 >= 4096
	  pad_size += 64; // cache line size
          size += pad_size;
	}
      }
#endif
      if (pad_size) {
        static INT count;
        char name[64];
        sprintf (name, "pad_%d", count);
        count++;
	ST *pad_symbol = 
	  Create_Local_Array_ST(Be_Type_Tbl(MTYPE_I1),size,name); 
	St_Block_Union(st,pad_symbol);
      }
    }
  }
}

