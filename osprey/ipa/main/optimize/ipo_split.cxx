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


//* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */
// ====================================================================
// ====================================================================
//
// Module: 
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/ipa/main/optimize/ipo_split.cxx,v $
//
// Revision history:
//  20-May-98 - Original Version
//
// Description:
//
// Implementation of the utilities necessary to split commons
//
// ====================================================================
// ====================================================================

#include <stdint.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <assert.h>             // for assert()
#include <cmplrs/host.h>        // for ipc_ld.h
#define USE_STANDARD_TYPES      /* override unwanted defines in defs.h */
#include "defs.h"
#include "stab.h"               // symtab stuff
#include "mempool.h"            // for MEM_POOL
#include "cxx_template.h"       // DYN_ARRAY<T>
#include "tracing.h"            // for TFile
#include "mtypes.h"             // for ipc_file.h
#include "const.h"
#include "ipc_file.h"           // for ipa_inline.h
#include "ipa_option.h"         // for Trace_IPA
#include "ipo_main.h"           // for IPO_Gprel_Sym_Count
#include "ipa_cg.h"             // call graph
#include "ir_reader.h"          // dump tree
#include "ipa_pad.h"

static BOOL trace_split_common = FALSE;
static MEM_POOL IPO_split_pool;

// ===========================================================
// map a field element to its split common
// ===========================================================
class FIELD_MAP
{
private:
  FLD_HANDLE _fld;
  INT _split_position;

public:
  void Set_fld(FLD_HANDLE f) { _fld = f;};
  FLD_HANDLE Get_fld() const { return _fld;};

  INT Get_split_position() const { return _split_position;};
  void Set_split_position(INT s) { _split_position = s;};
  
  FIELD_MAP() : _fld (), _split_position (0) {}

  void Print(FILE* fp = stderr) { 
    fprintf(fp, "field info %s : split_pos = %d \n", FLD_name(_fld),_split_position);
  };
};

typedef DYN_ARRAY<FIELD_MAP> FIELD_MAP_DYN_ARRAY;
static FIELD_MAP_DYN_ARRAY *Field_Map = NULL;
static char Split_Name[1000];
//--------------------------------------------------------------------
// create a new name for the split common
//--------------------------------------------------------------------
static char*
GetName(char *common_name, INT64 offset)
{
  static INT Name_Count = 0;

  sprintf(Split_Name, "%s.%lli", common_name, offset);
  ++Name_Count;
  return Split_Name;
}

//--------------------------------------------------------------------
// get the size of the split common from the common node information
// computed during the analysis phase
//--------------------------------------------------------------------
static INT64
Get_Size(SPLIT_COMMON_DYN_ARRAY *common_split_array,
	 INT split_number,
	 INT current_split_position)
{
  INT last_idx = common_split_array->Lastidx();
  BOOL done = FALSE;
  INT64 size = 0;
  for (INT i = current_split_position; i<= last_idx && !done;  ++i) 
    {
      SPLIT_COMMON *element = &(*common_split_array)[i];
      if (element->Get_split_position() > split_number)
	done = TRUE;
      else
	size = size + element->Get_size()*element->Get_element_size();
    }
  return size;
}

//--------------------------------------------------------------------
// get the alignment
//--------------------------------------------------------------------
static INT
Get_Align(SPLIT_COMMON_DYN_ARRAY *common_split_array,
	  INT split_number,
	  INT current_split_position)
{
  INT last_idx = common_split_array->Lastidx();
  BOOL done = FALSE;
  INT align = 0;
  for (INT i = current_split_position; i<= last_idx && !done;  ++i) 
    {
      SPLIT_COMMON *element = &(*common_split_array)[i];
      if (element->Get_split_position() > split_number)
	done = TRUE;
      
      else if (align < element->Get_element_size())
	{
	  align = element->Get_element_size();
	}
    }
  return align;
}
// ===========================================================
// DESCR: Create_Struct_Type
//        create a new type of KIND_STRUCT for a common
// ===========================================================
static TY_IDX
Create_Struct_Type(ST* split_st, INT align, INT64 size)
{
  TY_IDX ty_idx;
  TY &ty = New_TY(ty_idx);
  TY_Init(ty, size, KIND_STRUCT, MTYPE_M, Save_Str(ST_name(split_st)));
  Set_TY_align(ty_idx, align);
  return ty_idx;
}
// ===========================================================
// DESCR: Map_Field_To_STs
//        map field entries to split common
// ===========================================================
void  
Map_Field_To_STs(TY_IDX common_ty_idx, 
		 SPLIT_COMMON_DYN_ARRAY *split_common_array)
{
  SPLIT_COMMON *element;
  BOOL done = FALSE;
  INT position = 0;
  INT64 ofst = 0;

  const TY& common_ty = Ty_Table[common_ty_idx];
  FmtAssert((TY_kind(common_ty) == KIND_STRUCT), ("Invalid TY in Map_Field_To_STs \n"));

  INT current_split = 0;
  INT current_split_position = 0;
  // get the max offset of this split common
  INT last_idx = split_common_array->Lastidx();
  INT num_splits =
    (*split_common_array)[last_idx].Get_split_position() + 1;

  if (num_splits == 1)
    {
      element = &(*split_common_array)[last_idx];
      ofst = element->Get_offset() + 
	element->Get_size()*element->Get_element_size() - 1;
    }
  else
    {
      BOOL found = FALSE;
      while (!found)
	{
	  element =
	    &(*split_common_array)[current_split_position];
	  if (element->Get_split_position() == 1)
	    {
	      // ofst = element->Get_offset() + element->Get_size()*
	      // element->Get_element_size() - 1;
	      ofst = element->Get_offset();
	      found = TRUE;
	    }
	  else
	    ++current_split_position;
	}
    }
  // end find the max offset of this split common
  FLD_ITER fld_iter = Make_fld_iter(TY_fld(common_ty));
  do  {
    FLD_HANDLE fld1 (fld_iter);

    // if we reach an offset > current offset then we need to find
    // the next current_split_position, and the new offset
    if (FLD_ofst(fld1) >= ofst)
      {
	++current_split;
	done = FALSE;

	// if we are at the last split then we get the offset from
	// the last element
	if (current_split == num_splits-1)
	  {
	    element = &(*split_common_array)[last_idx];
	    ofst = element->Get_offset() + 
	      element->Get_size()*element->Get_element_size();
	  }

	else {
	  while (!done)
	    {
	      ++current_split_position;
	      if
		((*split_common_array)[current_split_position].Get_split_position() == current_split+1)
		  {
		    element = &(*split_common_array)[current_split_position]; 
		    done = TRUE;
		    // ofst = element->Get_offset() - 1;
		    ofst = element->Get_offset();
		  }
	    }
	}
      }

    position = Field_Map->Newidx();
    FIELD_MAP *node = &(*Field_Map)[position];

    node->Set_fld(fld1);
    node->Set_split_position(current_split);
    if (trace_split_common)
      fprintf(TFile, "fld1 %s is in split position %d \n", FLD_name(fld1), current_split);
  } while (!FLD_last_field(fld_iter++));
}

// ==================================================================
// DESCR: Split_Individual_Common
//
// 1) Create sclass_common symbols for each split common
// 2) Update the ST base entry for SCLASS_BASED symbols belonging
//    to the split common
// 3) For the original common, create a new type entry containing the
//    split commons as fields
//
// Example /a/ x(1024,8), y(1024,8), z(1024,8)
//
// after split
// common a -> 3 fields a_0, a_1024*8, a_2*1024*8
// common a_0 -> 1 field x, ST_full points to a, ST_base of z points
// to a_0
// common a_1024*8 -> 1 field y, ST_full points to a, ST_base of y
// points to  a_1024*8
// common a_2*1024*8 -> 1 field z, ST_full points to a, ST_base of z
// points to a_2*1024*8
//
// ==================================================================
static void
Split_Individual_Common(COMMON_SNODE_LIST *snode)

{
  ST *s;
  SPLIT_COMMON_DYN_ARRAY *split_common_array;
  FLD *last_field = NULL;
  Field_Map = NULL;
  ST* new_st;

  MEM_POOL_Push(&IPO_split_pool);
  split_common_array = snode->Get_Split_Array();
  s = snode->Get_ST();

  Field_Map =  CXX_NEW(FIELD_MAP_DYN_ARRAY(&IPO_split_pool),&IPO_split_pool);

  // current position in the split array
  INT current_split_position = 0;
  // Create a new ty of kind struct for the original common
  TY_IDX orig_ty_idx = ST_type(s);
  
  if (trace_split_common)
    fprintf(TFile, "Looking for commons %s \n", ST_name(s));
  
  // map the original field entries to the respective commons
  Map_Field_To_STs(orig_ty_idx, split_common_array);

  // TY_flist(orig_ty) = NULL;
    
  if (!split_common_array)
    Fail_FmtAssertion("Unable to find split common shape array \n");

  // Compute number of splits for this common
  INT last_idx = split_common_array->Lastidx();

  if (last_idx == -1)
    Fail_FmtAssertion("Unable to find elements of common %s \n", ST_name(s));

  INT num_splits =
    (*split_common_array)[last_idx].Get_split_position() + 1;

  // temporary array to store the 
  ST** temp_st = CXX_NEW_ARRAY(ST*, num_splits+1, &IPO_split_pool);

  // Create a new fld list FLD_common for the original st entry
  if (trace_split_common)
    fprintf(TFile, "num_splits  = %d for common %s  \n", num_splits,ST_name(s));
  // create the new st entries for the split commons
  for (INT i=0; i<num_splits;++i)
    {
      new_st = Copy_ST(s);
      INT64 ofst =
	(*split_common_array)[current_split_position].Get_offset();

      INT align = 
	Get_Align(split_common_array,i,
		  current_split_position);

      INT64 size =
	Get_Size(split_common_array,i,
		 current_split_position);
      INT ty_idx =  Create_Struct_Type(new_st, align, size);
      ST_Init(new_st, Save_Str(GetName(ST_name(s), ofst)), CLASS_VAR, 
	      ST_sclass(s), ST_export(s), ty_idx);
      
      Set_TY_name_idx(Ty_Table[ty_idx], ST_name_idx(*new_st));
      Set_TY_split(Ty_Table[ty_idx]);
      Set_ST_is_split_common(new_st);
      if (ST_is_thread_private(s))
	Set_ST_is_thread_private(new_st);

      // attach the field information
      if (Field_Map)
	{
	  FLD_HANDLE current_fld;
	  for (INT ii=0; ii<=Field_Map->Lastidx();++ii)
	    {
	      FIELD_MAP *node = &(*Field_Map)[ii];
	      FIELD_MAP *node_next = NULL;
	      if (ii != Field_Map->Lastidx())
		node_next = &(*Field_Map)[ii+1];

	      if (node->Get_split_position() == i)
		{
		  if (current_fld.Is_Null ())
		    {
		      Set_TY_fld(Ty_Table[ty_idx],node->Get_fld());
		      current_fld = node->Get_fld();
		      Set_FLD_ofst(current_fld, (FLD_ofst(current_fld)
						 - ofst));
		      if (ii == Field_Map->Lastidx() ||
			  (node_next->Get_split_position() == i+1))
			Set_FLD_last_field(current_fld);
		    }
		  else
		    {
		      current_fld = node->Get_fld();
		      Set_FLD_ofst(current_fld, 
				   (FLD_ofst(current_fld)-ofst));
		      if (ii == Field_Map->Lastidx() ||
			  (node_next->Get_split_position() == i+1))
			Set_FLD_last_field(current_fld);
		    }
		}
	      if (node->Get_split_position() > i)
		break;
	    }
	}
      
      Set_ST_sclass(new_st, SCLASS_COMMON);
	  if (ST_gprel(s)) {
	      Set_ST_gprel(new_st);
	      //IPO_Gprel_Sym_Count++;
	  }
	  Set_ST_export(new_st, ST_export(s));
      Set_ST_full_idx(*new_st, ST_st_idx(s));
      //ST_full(new_st) = s;
      //ST_class(new_st) =  ST_class(s);
      //Set_ST_base_idx(*new_st, ST_st_idx(new_st));
      //ST_base(new_st) = new_st;
      // ST_ofst(new_st) = ofst;
      Set_ST_ofst(*new_st, 0);
      //ST_ofst(new_st) = 0;
      // MAKE SURE THIS is OKAY!!! ///
      //ST_ofst(new_st) = ST_ofst(s);

      //Enter_ST(new_st);

      if (trace_split_common)
	fprintf(TFile, "entered new ST %s \n", ST_name(new_st));

      temp_st[i] = new_st;

      //FLD *field = 
      FLD_HANDLE field = New_FLD ();
      FLD_Init(field,
	       Save_Str(ST_name(new_st)), ST_type(new_st), 
	       (*split_common_array)[current_split_position].Get_offset());
      if (i==0)
	{
	  Set_TY_fld(Ty_Table[orig_ty_idx], field);
	  //TY_flist(orig_ty) = field;
	  //last_field = field;
	  if (i== (num_splits-1))
	    Set_FLD_last_field(field);	    
	}
      else
	{
	  if (i==(num_splits-1))
	    Set_FLD_last_field(field);	    
	  // TY_next(last_field) = field;
	  // last_field = field;
	}
      

      // FLD_name(field) = Save_Str(ST_name(new_st));
      // FLD_type(field) = ST_type(new_st);
      // get the field offset from the common_node structure
      // FLD_ofst(field) = (*split_common_array)[current_split_position].Get_offset();
      // FLD_next(field) = NULL;
      // Set_TY_split(FLD_type(field));
      //last_field = field;

      // get to the next split position
      INT split_position = i;
      if (i != (num_splits-1))
	while (split_position != (i+1))
	  {
	    current_split_position++;
	    if
	      ((*split_common_array)[current_split_position].Get_split_position() == (i+1))
		split_position++;
      

	  }

      INT64 next_ofst;
      SPLIT_COMMON *element;
      if (i == num_splits-1)
	{

	  element = 
	    &(*split_common_array)[split_common_array->Lastidx()];
	  next_ofst =
	    element->Get_offset() + 
	      element->Get_size()*element->Get_element_size() - 1;
	}
      else
 	{
	  element =
	    &(*split_common_array)[current_split_position];
	  next_ofst = element->Get_offset() - 1;
	};

      //ST_NODE_DYN_ARRAY *st_array =
      //ST_node_tbl->Find(ST_name(s));

      //if (!st_array)
      //Fail_FmtAssertion("Unable to find NODE_DYN_ARRAY for symbol %s \n",ST_name(s));

      // update the ST_base entries for the sts in the split common
      COMMON_SNODE_LIST_ITER iter(snode);
      for (iter.First(); !iter.Is_Empty(); iter.Next())
	{
	  COMMON_SNODE* common_snode = iter.Cur();
	  ST* stbase = 	common_snode->Get_ST();
	  if ((ST_ofst(*stbase) <= next_ofst) && (ST_ofst(*stbase) >=
	      ofst) && (!ST_is_split_common(ST_base(*stbase))))
	    {
	      // ST_base(stbase) = new_st;
	      // get the new offset based on the starting offset of
	      // the new base
	      Set_ST_base(*stbase, *new_st);
	      Set_ST_ofst(*stbase, ST_ofst(*stbase) - ofst);
	      // ST_ofst(stbase) = ST_ofst(stbase) - ofst;
	    }

	}
    }

  MEM_POOL_Pop(&IPO_split_pool);
}

//===================================================================
// DESCR: IPO_Split_Common
// The exported function
//===================================================================
void
IPO_Split_Common()
{
  STR_IDX common_name;
  COMMON_SNODE_LIST *snode_list;

  MEM_POOL_Constructor split_pool (&IPO_split_pool, "IPO split pool", 0);

  trace_split_common = Get_Trace(TP_IPA, IPA_TRACE_SPLIT_COMMON);

  if (IPA_Common_Table) {
    COMMON_SNODE_TBL_ITER common_snode_tbl_iter(IPA_Common_Table);
    while (common_snode_tbl_iter.Step(&common_name, &snode_list)) {
      if (!ST_is_not_used(snode_list->Get_ST()) && !snode_list->No_Split()) {
        Split_Individual_Common(snode_list);
      }
    }
  }
}
