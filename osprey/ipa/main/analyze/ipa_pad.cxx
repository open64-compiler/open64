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


/* ====================================================================
 * ====================================================================
 *
 *  ipa_pad.cxx
 *
 * Description:
 * Implementation of the utilities necessary to pad array
 * columns to avoid cash-thrashing whenever possible.
 *
 * ====================================================================
 * ====================================================================
 */

#include <stdint.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <cmplrs/host.h>		// for ipc_interafce.h
#define USE_STANDARD_TYPES		// override unwanted defines in defs.h 
#include "defs.h"
#include "errors.h"			// for FmtAssert
#include "tracing.h"			// for TFile
#include "ipa_cg.h"			// for IPA_CALL_GRAPH
#include "ipc_bread.h"			// for IP_READ_str_table ()
#include "ipa_summary.h"		// for IPA_get_common_file_array()
#include "ipa_inline.h"			// for IPA_NODE
#include "ipa_option.h"			// various IPA option flags
#include "ipc_symtab_merge.h"           // IPC_GLOBAL_IDX_MAP
#include "ipc_file.h"                   // for IP_File_header
#include "ipa_pad.h"
#define MAX_ALIGN 16

INT IPO_Pad_Count = 0;
COMMON_SNODE_TBL* IPA_Common_Table = NULL;

static MEM_POOL IPA_Pad_Split_Mem_Pool;
static BOOL trace_split_common = FALSE;

//===========================================================
// DESCR: New_Append
//        Append an entry into the list
//===========================================================
void
COMMON_SNODE_LIST::New_Append(ST* st, MEM_POOL *m)
{
  Append(CXX_NEW(COMMON_SNODE(m, st), m));
}

//===========================================================
// DESCR: Free_Nodes
//        Free all the elements in the list
//===========================================================
void 
COMMON_SNODE_LIST::Free_Nodes (MEM_POOL *m)
{
  COMMON_SNODE_LIST_ITER list_iter(this);
  COMMON_SNODE          *last_node;

  list_iter.First();
  while (!list_iter.Is_Empty()) {
      last_node = list_iter.Cur();
      list_iter.Next();
      CXX_DELETE(last_node, m);
  }
} // COMMON_SNODE_LIST::Free_Nodes

//===========================================================
// DESCR: COMMON_SNODE_LIST
//        constructor
//===========================================================
COMMON_SNODE_LIST::COMMON_SNODE_LIST(MEM_POOL *m, ST* s)
{
  _map = CXX_NEW(ST_TO_FLD_MAP_ARRAY(m), m);
  _flags = 0;
  _st = s;
  _split_array = CXX_NEW(SPLIT_COMMON_DYN_ARRAY(m), m);
  if (!IPA_Enable_Padding)
    Set_No_Pad();
  if (!IPA_Enable_Split_Common)
    Set_No_Split();
}

//===========================================================
// DESCR: COMMON_SNODE
//        constructor
//===========================================================
COMMON_SNODE::COMMON_SNODE(MEM_POOL *m, ST* s)
{
  _st = s;
  _flags = 0;
  _bounds_array = CXX_NEW(BOUNDS_ARRAY(m),m);
}

//=============================================================
// DESCR: Intersects_Col_Boundary
// check if the equivalence intersects a column boundary
//===========================================================
static BOOL
Intersects_Col_Boundary (WN_OFFSET start1, WN_OFFSET size1,
			 WN_OFFSET col_size,
			 WN_OFFSET start2, WN_OFFSET size2) 
{
  /* Assuming a data-block at location start1 and of size1,
   * partitioned into segments of size col_size:  Return
   * TRUE if the data-block represented by start2 and size2
   * interesects any column within the first data-block.
   */
  BOOL intersects;

  if (start2 > (start1 + size1))
    intersects = FALSE;  /* block2 begins after block1 */
  else if (size2 <= 0)
    intersects = TRUE;   /* block2 has unknown size */
  else if (start2+size2 < start1)
    intersects = FALSE;  /* block2 ends before block1 */
  else if (start2 < start1)
    intersects = TRUE;   /* block2 straddles first column in block1 */
  else
    /* We have an intersection if block2 extends beyond the end
     * of the column in which it begins.
     */
    intersects = (((start2-start1) % col_size) + size2) > col_size;
  
  return intersects;
} /* Intersects_Col_Boundary */

//===========================================================
// DESCR: Is_Common_Based_Symbol
//        return TRUE is the symbol is part of 
//        a common block entry
//===========================================================
extern BOOL
Is_Common_Based_Symbol (const ST* st)
{
  if (ST_base_idx (st) == ST_st_idx (st))
    return FALSE;

  const ST* base = ST_base (st);

  return ((ST_sclass(base) == SCLASS_COMMON ||
	   ST_sclass(base) == SCLASS_DGLOBAL) &&
	  TY_kind (ST_type (base)) == KIND_STRUCT);
}

//===========================================================
// DESCR: Is_Global_Array
//        returns TRUE is global array
//===========================================================
static inline
BOOL
Is_Global_Array(const ST* st)
{
  if (ST_sclass(st) == SCLASS_UGLOBAL)
    {
     if ((TY_kind(ST_type(st)) == KIND_ARRAY))
       return TRUE;
   }
  return FALSE;
}

//===========================================================
// DESCR: FLD_equivalent
//        returns TRUE is the flds are the same
//===========================================================
static BOOL
FLD_equivalent(FLD_HANDLE fld1, FLD_HANDLE fld2)
{
  if (FLD_name_idx(fld1) != FLD_name_idx(fld2))
    return FALSE;

  if (FLD_ofst(fld1) != FLD_ofst(fld2) ||
      FLD_bsize(fld1) != FLD_bsize(fld2) ||
      FLD_bofst(fld1) != FLD_bofst(fld2) ||
      FLD_flags(fld1) != FLD_flags(fld2) ||
      FLD_st(fld1) != FLD_st(fld2))
    return FALSE;
  return TRUE;
}

//===========================================================
// DESCR: Is_Bad_Equivalence
//        returns true if bad equivalence
//===========================================================
static BOOL
Is_Bad_Equivalence (const ST* st)
{
  /* Return TRUE if an equivalence field intersects a column
   * of a common block field.  Return FALSE if the ST is not a
   * common block.
   */
  const TY& common_ty = Ty_Table[ST_type (st)];
  BOOL      intersected = FALSE;

  if (ST_sclass(st) != SCLASS_COMMON || TY_kind(common_ty) != KIND_STRUCT)
    return FALSE;
    
  FLD_ITER fld_iter1 = Make_fld_iter (TY_fld (common_ty));

  do {
    FLD_HANDLE fld1 (fld_iter1);
    const TY& fld1_ty = Ty_Table[FLD_type (fld1)];
    if (FLD_equivalence (fld1)		&& // not equivalence
	TY_kind (fld1_ty) == KIND_ARRAY	&& // an array
	TY_AR_ndims (fld1_ty) > 1		&& // more than one dimension
	TY_AR_const_stride (fld1_ty, 0)	&& // with constant column size
	TY_size (fld1_ty) > 0) {		   // known size
      
      /* Check that equivalenced fields within the struct does
       * either not touch this array, or fits within a column.
       *
       * TODO: As a future improvement, we could allow:
       *
       *   real c(100, 2)
       *   real a(100)
       *   real b(100)
       *   common /x/ a b
       *   equivalence(a, c(1, 1))
       *   equivalence(b, c(1, 2))
       *
       * and similar difficult cases.
       */

      FLD_ITER fld_iter2 = Make_fld_iter (TY_fld (common_ty));

      do {
	FLD_HANDLE fld2 (fld_iter2);
	if (!FLD_equivalent(fld1, fld2) && 
	    (FLD_type(fld1) != FLD_type(fld2)) 
	    && FLD_equivalence(fld2))
	  {
	    // check only if the offsets overlap
	    if (FLD_ofst(fld2) >= FLD_ofst(fld1) &&
		(FLD_ofst(fld2) <= FLD_ofst(fld1) + TY_size(fld1_ty)))
	      intersected =
		(FLD_equivalence(fld2) &&
		 Intersects_Col_Boundary(FLD_ofst(fld1),
					 TY_size(fld1_ty),
					 TY_AR_stride_val(fld1_ty, 0),
					 FLD_ofst(fld2),
					 TY_size(FLD_type(fld2))));  
	  }
      } while (!FLD_last_field (fld_iter2++));
    }
  } while (!FLD_last_field (fld_iter1++));
  return intersected;
} /* Is_Bad_Equivalence */


//===========================================================
// DESCR: FLD_Intersects
//        field intersects
//===========================================================
static BOOL
FLD_Intersects (FLD_HANDLE fld2, FLD_HANDLE fld1)
{
  TY_IDX fld1_ty = FLD_type (fld1);
  TY_IDX fld2_ty = FLD_type (fld2);

  return (FLD_ofst (fld1) == FLD_ofst (fld2) &&
	  TY_size (fld1_ty) <= TY_size (fld2_ty) &&
	  TY_align_exp (fld1_ty) == TY_align (fld2_ty));
	
} // FLD_intersects

//===========================================================
// DESCR: Is_Bad_Split_Equivalence
//        returns true if any equivalence does not intersect 
//        with an element in the common
//===========================================================
static BOOL 
Is_Bad_Split_Equivalence (const ST* s)
{
  const TY& common_ty = Ty_Table[ST_type (s)];

  if (ST_sclass(s) != SCLASS_COMMON || TY_kind(common_ty) !=  KIND_STRUCT)
    return FALSE;

  BOOL intersected = FALSE;
  FLD_ITER fld_iter1 = Make_fld_iter (TY_fld (common_ty));

  do {
    // for each equivalenced field check to see if it maps
    // completely with another non-equivalenced field in terms
    // of size, alignment, and element type

    if (FLD_equivalence (fld_iter1)) {
      FLD_ITER fld_iter2 = Make_fld_iter (TY_fld (common_ty));
      
      do {
	// if the field is not equivalenced then check to 
	// see if the equivalenced field is identical to
	// an element of the common
	if (!FLD_equivalence(fld_iter2)) {
	  if (FLD_Intersects(fld_iter2, fld_iter1))
	    intersected = TRUE;
	}
      } while (!FLD_last_field (fld_iter2++));

      // if we reach here and not intersected, then there is a
      // problem and simply return false
      if (!intersected)
	return TRUE;
    }
  } while (!FLD_last_field (fld_iter1++));
  
  return FALSE;
} /* Is_Bad_Split_Equivalence */


// ===========================================================
// DESCR: Group_Common_STs
//        For each common in the current symbol table, 
//        store an array of STs belonging to the same common
// ===========================================================
static void
Group_Common_STs(COMMON_SNODE_TBL *common_st_tbl)
{     
  ST *st;
  INT i;
  COMMON_SNODE_LIST *common_snode_list;
  FOREACH_SYMBOL(GLOBAL_SYMTAB, st, i) {
    if (Is_Common_Based_Symbol(st)) {
      STR_IDX common_name = ST_name_idx(ST_base(st));
      common_snode_list = common_st_tbl->Find(common_name);
      if (!common_snode_list)
	{
	  common_snode_list = 
	    CXX_NEW(COMMON_SNODE_LIST(&IPA_Pad_Split_Mem_Pool, ST_base(st)), 
                    &IPA_Pad_Split_Mem_Pool);
	  common_st_tbl->Enter(common_name, common_snode_list);
	  if (!common_snode_list->No_Split())
	    if (Is_Bad_Split_Equivalence(ST_base(st)))
	      common_snode_list->Set_No_Split();

	  if (!common_snode_list->No_Pad())
	    if (Is_Bad_Equivalence(ST_base(st)))
	      {
		common_snode_list->Set_No_Pad();
		common_snode_list->Set_No_Split();
	      }

	  INT st_idx = ST_st_idx(ST_base(st));
	  if ((ST_export(St_Table[st_idx]) != EXPORT_INTERNAL) &&
	      (ST_export(St_Table[st_idx]) != EXPORT_HIDDEN))
	    {
	      common_snode_list->Set_No_Pad();
	      common_snode_list->Set_No_Split();
	    }

	  if (ST_is_initialized(ST_base(st)) ||
	      ST_is_initialized(st))
	    {
	      common_snode_list->Set_No_Pad();
	      common_snode_list->Set_No_Split();
	    }
	}
      if (ST_is_equivalenced(st))
	{
	  //common_snode_list->Set_No_Pad();
	  common_snode_list->Set_No_Split();
	}
      common_snode_list->New_Append(st, &IPA_Pad_Split_Mem_Pool);
    }

    // pad global arrays that are not address taken
    if (Is_Global_Array(st) && (ST_export(st) == EXPORT_INTERNAL) &&
	!ST_addr_saved(st) && !ST_addr_passed(st))
      {
	common_snode_list = common_st_tbl->Find(ST_name_idx(st));
	if (!common_snode_list)
	  {
	    common_snode_list = 
	      CXX_NEW(COMMON_SNODE_LIST(&IPA_Pad_Split_Mem_Pool, st),
					&IPA_Pad_Split_Mem_Pool);
	    common_st_tbl->Enter(ST_name_idx(st), common_snode_list);
	    common_snode_list->New_Append(st,
					  &IPA_Pad_Split_Mem_Pool);
	    common_snode_list->Set_No_Split();
	  }
      }
  }
}

//===========================================================
// DESCR: Update_Summaries
//        walk the summaries looking for any of the bad bits
//===========================================================
static void
Update_Summaries (COMMON_SNODE_TBL* common_snode_tbl, INT num_ir)
{
  for (INT i = 0; i < num_ir; ++i) { 
    INT32 symbol_count;
    SUMMARY_SYMBOL* symbol_node_array = 
      IPA_get_symbol_file_array(IP_File_header[i], symbol_count);

    for (INT j = 0; j < symbol_count; ++j) {
      SUMMARY_SYMBOL* s = &symbol_node_array[j];
      if (s->Is_common_block() && 
	  (s->Common_io_no_pad() || s->Is_parm() || s->Is_addr_saved())) {
        COMMON_SNODE_LIST* common_snode_list = 
          common_snode_tbl->Find(ST_name_idx(ST_ptr(s->St_idx())));
        if (common_snode_list) {
          common_snode_list->Set_No_Pad();
        }
      }
    }
  }
}

//===========================================================
// DESCR: Common_Array_Pad_Size
//        returns a pad size value
//===========================================================
extern INT64
Common_Array_Pad_Size (INT column_size)
{
#ifdef TARG_X8664
  if (column_size % 16 != 0)
    return column_size % 16;
#endif
  // Given a multidimensional array, come up with a pad size
  // (in bytes) for the first dimension.

  // if size is close to a multiple of 256*8, move it away by
  // a distance of at least .1*256*8
  INT64 bad_dim_size = 256*8;
  INT64 mod = column_size % bad_dim_size;
  if ((column_size > bad_dim_size) && (mod < bad_dim_size/20)) {
    return (bad_dim_size/10 - mod);
  } else if ((column_size > bad_dim_size) &&
	     ((bad_dim_size - mod) < bad_dim_size/20)) {
    return (bad_dim_size/20 + (bad_dim_size - mod));
  } 
  
  // if size is close to a multiple of 128*8, move it away by
  // a distance of at least .1*128*8
  bad_dim_size = 128*8;
  mod = column_size % bad_dim_size;
  if ((column_size > bad_dim_size) && (mod < bad_dim_size/20)) {
    return (bad_dim_size/10 - mod);
  } else if ((column_size > bad_dim_size) &&
	     ((bad_dim_size - mod) < bad_dim_size/20)) {
    return (bad_dim_size/20 + (bad_dim_size - mod));
  } else {
    return 0;
  }
} // Common_Array_Pad_Size


//=======================================================
// DESCR: Store_Orig_Dims
//        store the original dimensions  
//=======================================================
static void
Store_Orig_Dims(ST* s, BOUNDS_ARRAY* b)
{
 Is_True((TY_kind(ST_type(s)) == KIND_ARRAY), (" Expecting KIND_ARRAY in Store_Orig_Dims \n"));

 Is_True( (b!=NULL), ("NULL bounds in Store_Orig_Dims \n"));

 INT lower, upper, stride;
 ARB_HANDLE arb_base = TY_arb(ST_type(s));
 INT num_dims = ARB_dimension(arb_base);
 
 if (num_dims > 1) {
   for (INT i=0; i<num_dims; ++i)
     {
       ARB_HANDLE arb = arb_base[num_dims-1-i];
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

  if (trace_split_common)
    fprintf(TFile,"end_dim = %d, padding = %d \n", end_dim, padding);

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
  if (fpad_size > 0.0)
    fpad_size = fpad_size - .001;

  if (trace_split_common)
    fprintf(TFile, "fpad_size = %f \n", fpad_size);

  INT64 pad_size;
  if (extra_pad)
    pad_size = (INT64)(fpad_size+2)*(*bounds)[0].Get_Stride();
  else
    pad_size = (INT64)(fpad_size+1)*(*bounds)[0].Get_Stride();

  if (pad_size == 0)
    {
      if (trace_split_common)
	fprintf(TFile, "pad size = %lld for dim %d \n",(*bounds)[0].Get_Stride(), pad_dim);
      return (*bounds)[0].Get_Stride();
    }
  else 
    {
      if (trace_split_common)
	fprintf(TFile, "pad size = %lld for dim %d \n", pad_size,pad_dim);
      return pad_size;
    }
}

//============================================================
// DESCR: Padding_Threshold
//        Check to see if the size increase is more than 10%
//        If it is then do not pad
//        return TRUE if size increase is < 10% 
//        else return FALSE
//============================================================
static BOOL
Padding_Threshold(INT64 pad_size, INT pad_dim, BOUNDS_ARRAY *b,
		  ARB_HANDLE idx)
{
  Is_True(pad_dim <= b->Lastidx(), ("pad dim is TOO high in Padding Threshold \n"));

  BOUNDS *belement = &(*b)[pad_dim];
  
//  INT size = abs(belement->Get_Upper() - belement->Get_Lower() + 1);

  ARB_HANDLE arb = idx[b->Lastidx()-pad_dim];
  Is_True((ARB_const_ubnd(arb) && ARB_const_lbnd(arb) && ARB_const_stride(arb)), ("constant bounds expected \n"));

  INT size = abs(ARB_ubnd_val(arb) - ARB_lbnd_val(arb) + 1);
  float padt = ((pad_size/(*b)[0].Get_Stride() + size)*100)/size;
  if (padt > 110)
    {
      if (trace_split_common)
	fprintf(TFile, "padding threshold = %f, pad_size = %lld, dim_size = %d \n", padt, pad_size, size);
      return FALSE;
    }
  if (trace_split_common)
    fprintf(TFile, "padding threshold = %f, pad_size = %lld, dim_size = %d  \n", padt, pad_size, size);
  return TRUE;

}

//=======================================================
// DESCR: Get_Pad_Dim
//        return the dimension to PAD
//=======================================================
static INT
Get_Pad_Dim(INT last_dim, INT element_size, BOUNDS_ARRAY *b)
{
  INT size, lower, upper, max_element, max_dim;
  max_element = 0;
  max_dim = last_dim;
  // try just padding the last dimension. Pick the last
  // dimension even if other dimensions are bigger due to
  // avoiding backtracking
  if (trace_split_common)
    fprintf(TFile, "last dimension = %d \n", last_dim);

  INT last_dim_size = abs((*b)[last_dim].Get_Upper() -
			  (*b)[last_dim].Get_Lower() + 1);
  for (INT i=last_dim; i >= 0; --i)
    {
      BOUNDS* bounds_elem = &(*b)[i];
      lower = bounds_elem->Get_Lower();
      upper = bounds_elem->Get_Upper(); 
      size = abs(upper-lower+1);

      if (((size*element_size) % (128*8)) == 0)
      {
	if (trace_split_common)
	  fprintf(TFile, "Get_Pad_Dim = %d, Size = %d \n", i, size);
	return i;
      }
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
  if (trace_split_common)
    fprintf(TFile, "Get_Pad_Dim = %d \n", max_dim);
  return max_dim;
}

//========================================================
// Pad Dim ? returns TRUE if size is a power of 2
//========================================================
static
BOOL Pad_Dim(BOUNDS *b)
{
  INT one_count = 0;
  INT size = abs (b->Get_Upper() - b->Get_Lower() + 1);
  while (size != 0)
    {
      size = size >> 1;
      if (size & 1)
	++one_count;
    }

  if (trace_split_common)
    fprintf(TFile, "Size =  %d , one_count = %d \n", abs(b->Get_Upper() - b->Get_Lower() + 1), one_count);

  if (one_count > 1)
    return FALSE;
  return TRUE;
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
			    INT pad_dim, BOUNDS_ARRAY *barray)
{
  BOUNDS *bounds = &(*barray)[pad_dim];
  INT64 stride = bounds->Get_Stride();
  for (INT i=pad_dim; i<=barray->Lastidx();++i)   
    {	
      bounds = &(*barray)[i];
      if (i == pad_dim) 
	{
	  if (pad_size > (*barray)[0].Get_Stride())
	    pad_size = pad_size + pad_size %
	      (*barray)[0].Get_Stride();
	  pad_size = pad_size/(*barray)[0].Get_Stride();
          if (IPA_Common_Pad_Size) {
            // override padding size if requested by flag
            pad_size = IPA_Common_Pad_Size;
          }
          bounds->Set_Upper(pad_size+bounds->Get_Upper());
          if (trace_split_common)
	    fprintf(TFile, "dimension %d padded by %lld\n", pad_dim, pad_size);
	}
      bounds->Set_Stride(stride);
      stride = stride * abs(bounds->Get_Upper() - bounds->Get_Lower() + 1);
      if (trace_split_common)
	fprintf(TFile, "upper = %lld, lower = %lld, stride = %lld \n", bounds->Get_Upper(), bounds->Get_Lower(), bounds->Get_Stride());
    }
}

//=======================================================
// DESCR: Pad_Multi_Dims
//        Pad multiple dimensions of an array
//=======================================================
static BOOL
Pad_Multi_Dims(COMMON_SNODE *common_snode)
{
  ST* s = common_snode->Get_ST();
  BOOL to_pad = FALSE;


  if (common_snode->Pad())
    return TRUE;

  if ((TY_kind(ST_type(s)) == KIND_ARRAY)) {

    if (trace_split_common)
     fprintf(TFile, "Padding common %s \n", ST_name(s));

    Store_Orig_Dims(s, common_snode->Get_Bounds_Array());
    INT64 lower, upper, stride;
    ARB_HANDLE arb_base = TY_arb(ST_type(s));
    INT num_dims = ARB_dimension(arb_base);

    if (num_dims > 1) {
      INT64 pad_size = 1;
      INT64 padding;
      INT pad_dim;

      BOUNDS_ARRAY *bounds_array = common_snode->Get_Bounds_Array();
      Is_True(bounds_array!=NULL, (" NULL bounds array in Pad_Multi_Dims \n"));

      BOUNDS *b = &(*bounds_array)[0];
      if (b->Is_Constant()) {
	pad_size = abs(b->Get_Upper() - b->Get_Lower() + 1);
      }
      else return to_pad; // non constant bounds found

      pad_size = 1;
      for (INT i=0; i<num_dims-1; ++i) {
	b = &(*bounds_array)[i];
	if (b->Is_Constant()) {
	  lower = b->Get_Lower();
	  upper = b->Get_Upper();
	  pad_size = pad_size * abs(upper-lower+1);
	  padding = Common_Array_Pad_Size(pad_size*(*bounds_array)[0].Get_Stride());
	  if (trace_split_common)
	    fprintf(TFile, "padding = %lld, pad_size = %lld \n",padding, pad_size);
	  if (padding) {
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
		    IPO_Pad_Count += pad_size1;
		    to_pad = TRUE;
		    common_snode->Set_Pad();
		    if (trace_split_common)
		      fprintf(TFile, "Padding array %s for common %s \n", ST_name(s), ST_name(ST_base(s)));
		    Update_Bounds_After_Padding(pad_size1, pad_dim, bounds_array);
		    pad_size = Update_Pad_Size(i,bounds_array);
		  }
	      }
	    }
	  }
	
	else
	  return to_pad;
      }
    }
  }
  return to_pad;
  }

//=======================================================
// DESCR: Pad_Common_Elements
//        check if any of the elements need to be padded
//=======================================================
static BOOL
Pad_Common_Elements(COMMON_SNODE_LIST* common_snode_list)
{
  BOOL to_pad = FALSE;

  COMMON_SNODE_LIST_ITER  common_snode_list_iter(common_snode_list);
  
  for (common_snode_list_iter.First();
       !common_snode_list_iter.Is_Empty();
       common_snode_list_iter.Next()) 
    {
      COMMON_SNODE *common_snode = common_snode_list_iter.Cur();
      if (!to_pad)
	to_pad = Pad_Multi_Dims(common_snode);
      else
	Pad_Multi_Dims(common_snode);
    }
  return to_pad;

}


//=======================================================
// DESCR: Update_Padding_Size
//        Update the padding possible information based 
//        on the size
//=======================================================
static void
Update_Padding_Size(COMMON_SNODE_TBL *common_snode_tbl)
{
  BOOL to_pad = FALSE;
  Is_True(common_snode_tbl != NULL, ("Null Table in Update_Padding_Size \n"));

  COMMON_SNODE_TBL_ITER common_snode_tbl_iter(common_snode_tbl);
  COMMON_SNODE_LIST *snode_list;
  STR_IDX common_name;
  // walk the common hash table
   while (common_snode_tbl_iter.Step (&common_name, &snode_list)) 
     {
       if (!snode_list->No_Pad())
	 {
	   if (!Pad_Common_Elements(snode_list))
	     snode_list->Set_No_Pad();
	 }
     }
}

//=======================================================
// DESCR:  Padding_Count
//         Return the number of commons that need padding
//=======================================================
static INT
Padding_Count(COMMON_SNODE_TBL *common_snode_tbl)
{
  INT count = 0;
  STR_IDX common_name;
  COMMON_SNODE_LIST *snode_list;
  COMMON_SNODE_TBL_ITER common_snode_tbl_iter(common_snode_tbl);

  // walk the common hash table
   while (common_snode_tbl_iter.Step (&common_name, &snode_list)) 
     {
       if (!snode_list->No_Pad())
	 ++count;
     }
  return count;
}

//=======================================================
// DESCR:  Free_Common_Tbl
//         Free all the elements if there are no commons 
//         to pad
//=======================================================
static void
Free_Common_Tbl(COMMON_SNODE_TBL *common_snode_tbl)
{
  COMMON_SNODE_LIST *snode_list;
  COMMON_SNODE_TBL_ITER common_snode_tbl_iter(common_snode_tbl);

  STR_IDX common_name;
  while (common_snode_tbl_iter.Step (&common_name, &snode_list)) 
    snode_list->Free_Nodes(&IPA_Pad_Split_Mem_Pool);
}

// ===========================================================
// Print out the split information for the current common
// ===========================================================
static void
Print_Split(char *name, FILE *fp, SPLIT_COMMON_DYN_ARRAY *Split_Common_Shape)
{
  //  Split_Common_Shape
  INT last_idx = Split_Common_Shape->Lastidx();

  fprintf(fp, 
    "=======Recording Split Information for common %s ======= \n",name);
  for (INT i=0 ; i<=last_idx;++i)
    {
      SPLIT_COMMON *split = &(*Split_Common_Shape)[i];
      fprintf(fp, "offset = %lld, size = %lld, element_size = %d, split = %d\n",
        split->Get_offset(), split->Get_size(), split->Get_element_size(), 
        split->Get_split_position());
    }
}
// ===========================================================
// Dror's split needed heuristic. It uses cache sizes to
// determine if there is a benefit to splitting
// A common block is split only if the following criteria are met
//
//  1.  field represents an array >= 16384 bytes
//
//  2.  offset of field is >= 16384 - 819
//
//  3.  offset of field modulo 16384 is within 5% of 16384.
//      this offset is with respect to start of current split common block
//      as well as arrays >= 16384 in current split common block which
//      did not cause a split
//
//  4.  offset of field modulo 512 * 1024 is within 5% of 512 * 1024
//      (this evaluates to 26214).
//      this offset is with respect to start of current split common block
//      as well as arrays >=16384 in current split common block which
//      did not cause a split.
// ===========================================================
INT64 Primary_Cache =  16*1024;
INT64 Secondary_Cache = 512*1024;
INT64 Primary_Delta = 819;
INT64 Secondary_Delta = 26214;

//---------------------------------------------------------------
// in the case that padding has occured, split the arrays
// completely, update the size if padding has occured during the
// opt phase
//---------------------------------------------------------------
static void
Full_Split(SPLIT_COMMON_DYN_ARRAY *split_common_shape)
{
  INT count = split_common_shape->Lastidx();
  if (count)  
    {
      INT group = (*split_common_shape)[0].Get_group_position();
      INT cur_pos = 0;
     for (INT i=0; i<=count; ++i)
      {
       if ((*split_common_shape)[i].Get_group_position() == group)
         (*split_common_shape)[i].Set_split_position(cur_pos);
     else
       {
	 group = (*split_common_shape)[i].Get_group_position();
	 cur_pos++;
	 (*split_common_shape)[i].Set_split_position(cur_pos);
       }
   }
 }
}

//==============================================================
// split based on equivalenced groups
//==============================================================
static void
Compute_Equivalenced_Split_Regions(SPLIT_COMMON_DYN_ARRAY 
				   *split_common_shape)
{
  Full_Split(split_common_shape);
}

//==============================================================
// DESCR: Compute_Split_Regions
//==============================================================
static void
Compute_Split_Regions(SPLIT_COMMON_DYN_ARRAY *split_common_shape)
{
  INT64 offset, size, element_size, size_bytes;
  INT64 current_split = 0;
  INT count = split_common_shape->Lastidx();
  INT64 current_split_index = 0;
  INT current_split_id = 0;
  BOOL split_useful = FALSE;

  if ((count) && (*split_common_shape)[count].Get_group_position() > 0)
    Compute_Equivalenced_Split_Regions(split_common_shape);

  for (INT i=0; i<=count; ++i)
    {
      offset = (*split_common_shape)[i].Get_offset();
      size = (*split_common_shape)[i].Get_size();
      element_size = (*split_common_shape)[i].Get_element_size();
      size_bytes =  size*element_size;
      split_useful = TRUE;

      (*split_common_shape)[i].Set_split_position(current_split_id);

      // conditions 1 and 2
      if ((size_bytes >= Primary_Cache) && 
	  (offset >= Primary_Cache - Primary_Delta)
#ifdef KEY
	  && (i != 0)	// Don't split first field.  Bug 8784.
#endif
	  )
	{
	  offset = offset - current_split;

	  for (INT j = current_split_index; j <= i && split_useful; j++)
	    {
	      INT64 offset_j = (*split_common_shape)[j].Get_offset();
	      INT64 size_bytes_j =
		(*split_common_shape)[j].Get_element_size()*
		  (*split_common_shape)[j].Get_size();  
	     
	      // for arrays within the current split common that
	      // are >= Primary_Cache size, 
	      // 
	      if (size_bytes_j >= Primary_Cache)
		{
		  INT64  current_offset = offset - offset_j;
		  INT64  mod_primary = current_offset % Primary_Cache;
		  INT64  mod_second = current_offset % Secondary_Cache;
		  if (trace_split_common)
		    {
		      fprintf(TFile, "current_offset = %lli, mod_primary = %lli \n", current_offset, mod_primary);
		      fprintf(TFile, "current_offset = %lli, mod_second = %lli \n", current_offset, mod_second);
		    }
		  if ((mod_primary > Primary_Delta) &&
		      (mod_second > Secondary_Delta))
		    {
		      split_useful = FALSE;
		    }
		}
	    }
	  
	  if (split_useful)
	    {
	      (*split_common_shape)[i].Set_split_position(current_split_id+1);
	      ++current_split_id;
	      current_split_index = i+1;
	      current_split = (*split_common_shape)[i].Get_offset();
	    }
	}
    }
}

//================================================================
// check the summary information to see if all files have been
// compiled -O3
//================================================================
static BOOL 
All_O3(INT num_ir)
{
  for (INT i=0; i<num_ir; ++i)
    {
      SUMMARY_FILE_HEADER *s = IP_FILE_HDR_file_header(IP_File_header[i]);
      if (s->Get_opt_level() != 3)
	return FALSE;
    }
  return TRUE;
}

//=======================================================
// DESCR: Build split common dynamic array for a common
//=======================================================
static void
Build_Split_Array(COMMON_SNODE_LIST* snode_list)
{
  ST *s = snode_list->Get_ST();
  SPLIT_COMMON_DYN_ARRAY *split_array = 
    snode_list->Get_Split_Array();

  Is_True(((ST_sclass(s) == SCLASS_COMMON || ST_sclass(s) ==
	    SCLASS_DGLOBAL) && TY_kind(ST_type(s)) == KIND_STRUCT), ("Expecting a common ST in Build_Split_Array \n"));
  
  Is_True(split_array != NULL, ("NULL split array in Build_Split_Array \n"));

  // get to the fields, walk them and build the split common dynamic
  // array
  
  INT64 ofst = 0;
  INT64 size = 0;
  INT group = 0;
  FLD_ITER fld_iter = Make_fld_iter (TY_fld (Ty_Table[ST_type (s)]));
  do {
    
    FLD_HANDLE fld (fld_iter);
    if (FLD_equivalence(fld) && (ofst == 0)) {
      ofst = FLD_ofst(fld);
      size = TY_size(Ty_Table[FLD_type(fld)]);
      group++;
    }

    // check if this element is part of the current group
    else if (FLD_equivalence(fld)) {
      if ((FLD_ofst(fld) >= ofst) && (FLD_ofst(fld) <= ofst+size))
	;
      else if (TY_size(Ty_Table[FLD_type(fld)]) > size)
	size = TY_size(Ty_Table[FLD_type(fld)]);
    } 
    else if (ofst != 0) {
      ofst = 0; 
      size = 0; 
      group++;
    }
    
   // if (!FLD_equivalence (fld))  {
      INT64 element_size = 0;
      const TY& fld_type = Ty_Table[FLD_type (fld)];
      if (TY_kind(fld_type) == KIND_ARRAY)
	element_size = TY_size(TY_etype(fld_type));
      else
	element_size = TY_size(fld_type);
      
      INT64 total_size = (element_size ? TY_size(fld_type)/element_size : 0);
      INT id = split_array->Newidx();
      SPLIT_COMMON *node = &(*split_array)[id];
      node->Set_Vals(FLD_ofst(fld), total_size, element_size, 0, group);
    
  }  while (!FLD_last_field (fld_iter++));
}

//=======================================================
// DESCR: Build the splits
//=======================================================
static void
Build_Splits(COMMON_SNODE_TBL *common_snode_tbl)
{
  STR_IDX common_name;
  COMMON_SNODE_LIST *snode_list;
  COMMON_SNODE_TBL_ITER common_snode_tbl_iter(common_snode_tbl);

  // walk the common hash table
  while (common_snode_tbl_iter.Step (&common_name, &snode_list)) 
    {
      if (!snode_list->No_Split())
	{
	  Build_Split_Array(snode_list);
	  if (snode_list->Get_Split_Array()->Elements())
	    {
	      SPLIT_COMMON_DYN_ARRAY *split_array = 
		snode_list->Get_Split_Array();
	      
	      if (!snode_list->No_Pad())
		Full_Split(split_array);
	      else
		Compute_Split_Regions(split_array);
	      SPLIT_COMMON *split =
		&(*split_array)[split_array->Lastidx()];

	      // don't bother to split if only 1 split is created
	      if (split->Get_split_position() == 0)
		snode_list->Set_No_Split(); 
	    }
	  else
	    snode_list->Set_No_Split();
	}
    }
}

//=======================================================
// DESCR: Update_Split_Size
//        update size information
//=======================================================
static INT
Split_Count(COMMON_SNODE_TBL *common_snode_tbl)
{
  INT count = 0;
  Is_True(common_snode_tbl != NULL, ("Null Table in Split_Count \n"));

  COMMON_SNODE_TBL_ITER common_snode_tbl_iter(common_snode_tbl);
  COMMON_SNODE_LIST *snode_list;
  STR_IDX common_name;
  // walk the common hash table
   while (common_snode_tbl_iter.Step (&common_name, &snode_list)) 
     {
       if (!snode_list->No_Split())
	 ++count;
     }
  return count;
}

//=======================================================
// DESCR: Print_Splits
//=======================================================
void Print_Splits(COMMON_SNODE_TBL *common_snode_tbl)
{
  Is_True(common_snode_tbl != NULL, ("Null Table in Split_Count \n"));

  COMMON_SNODE_TBL_ITER common_snode_tbl_iter(common_snode_tbl);
  COMMON_SNODE_LIST *snode_list;
  STR_IDX common_name;
  // walk the common hash table
  while (common_snode_tbl_iter.Step (&common_name, &snode_list)) 
    {
      if (!snode_list->No_Split())
	Print_Split(ST_name(snode_list->Get_ST()), TFile, 
		    snode_list->Get_Split_Array());
    }
}

//=======================================================
// DESCR: Padding_Analysis
//        Walk the global symtab, get to each common
//        if clean then mark for padding
//=======================================================
void
Padding_Analysis(INT num_ir)
{
  MEM_POOL_Initialize(&IPA_Pad_Split_Mem_Pool, "IPA pad-split pool", 0);
  MEM_POOL_Push(&IPA_Pad_Split_Mem_Pool);

  trace_split_common = Get_Trace(TP_IPA, IPA_TRACE_SPLIT_COMMON);
  if (trace_split_common)
    fprintf(TFile, "Padding and Split Common Analysis\n");

  // attach this information to the call graph
  COMMON_SNODE_TBL *common_snode_tbl;
  common_snode_tbl = CXX_NEW(COMMON_SNODE_TBL(512, &IPA_Pad_Split_Mem_Pool), 
			     &IPA_Pad_Split_Mem_Pool);  
  
  if (!IPA_Enable_Picopt) {
    if (trace_split_common)
      {
	if (IPA_Enable_Padding)
	  fprintf(TFile, "Padding is OFF since PICOPT is OFF \n");
	if (IPA_Enable_Split_Common)
	  fprintf(TFile, "Splitting is OFF since PICOPT is OFF \n");
      }
    IPA_Enable_Split_Common = FALSE;
    IPA_Enable_Padding = FALSE;
    return;
  }

  if (IPA_Enable_Split_Common && !All_O3(num_ir))
    {
      IPA_Enable_Split_Common = FALSE;
      if (trace_split_common)
	fprintf(TFile, "IPA_Enable_Split_Common is OFF, file(s) NOT compiled -O3 -IPA \n");
    }

  // walk the symbol table and group commons and their STs
  Group_Common_STs(common_snode_tbl);

  INT pad_count = 0;
  INT split_count = 0;
  if (IPA_Enable_Padding)
    {
      // update padding possible based on summary information
      Update_Summaries (common_snode_tbl, num_ir);
      // update padding based on size information
      Update_Padding_Size(common_snode_tbl);
      // if no common block entries then return
      // no need to pad anything
      pad_count = Padding_Count(common_snode_tbl);
      if (!pad_count)
	IPA_Enable_Padding = FALSE;
      if (trace_split_common)
	fprintf(TFile, "pad count = %d \n", pad_count);
    }

  if (IPA_Enable_Split_Common)
    {
      Build_Splits(common_snode_tbl);
      split_count = Split_Count(common_snode_tbl);
      if (!split_count)
	IPA_Enable_Split_Common = FALSE;
      if (trace_split_common)
	{
	  fprintf(TFile, "split count = %d \n", split_count);
	  Print_Splits(common_snode_tbl);
	}
    }

  if ((pad_count == 0) && (split_count == 0))
    {
      MEM_POOL_Pop(&IPA_Pad_Split_Mem_Pool);
      MEM_POOL_Delete(&IPA_Pad_Split_Mem_Pool);
    }
  else
    IPA_Common_Table = common_snode_tbl;
}
