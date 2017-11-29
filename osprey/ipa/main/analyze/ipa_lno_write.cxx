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


#include <stdint.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <sys/elf_whirl.h>
#include <cmplrs/host.h>        // for ipc_bwrite.h
#include <assert.h>
#define USE_STANDARD_TYPES          /* override unwanted defines in defs.h */
#include "defs.h"
#include "config.h"
#include "wn.h"                 // for ipc_bread.h
#include "strtab.h"             // for ipc_file.h, ipc_option.h
#include "stab.h"               // for ipc_file.h
#include "const.h"              // for ipc_file.h
#include "irbdata.h"            // for ipc_file.h
#define USE_DST_INTERNALS
#include "dwarf_DST_mem.h"          // for ipc_file.h
#include "pu_info.h"
#include "ir_bwrite.h"
#include "ipc_file.h"
#include "ipl_summary.h"
#include "ipa_cg.h"
#include "ipa_section_annot.h"  // for IPA_NODE_SECTION_INFO
#include "erglob.h"
#include "ipc_file.h"
#include "ipa_lno_info.h"
#include "ipa_lno_file.h"
#include "ipa_lno_write.h"
#include "ipa_lno_summary.h"
#include "ipc_compile.h"
#include "ipa_section.h"
#include "ipaa.h"
#include "ipa_option.h"
#include "ipa_summary.h"
#include "linker.h" 
#include "process.h" 
#include "ipo_defs.h" 
#include "ipc_symtab_merge.h"
#include "ipa_reshape.h"

//----------------------------------------------------------------
// NAME: IPA_LNO_Map_Term
// FUNCTION: Map the values of 'term' for 'IPA_LNO_Summary'. 
//   Use 'node' to get the file header. 
//----------------------------------------------------------------

static void IPA_LNO_Map_Term(IPA_NODE* node, 
			     TERM* term, 
			     IPA_LNO_WRITE_SUMMARY* IPA_LNO_Summary) 
{ 
  INT32 size;
  if (term->Get_type() == LTKIND_IV) {
    INT ivar_idx = term->Get_desc(); 
    SUMMARY_PROCEDURE* osp = node->Summary_Proc();
    IVAR* ivar = IPA_get_ivar_array(node, size);
    ivar += ivar_idx;
    IVAR ivar_mapped = *ivar; 
    if (ivar->Is_Formal()) { 
      UINT32 old_formal_position = ivar_mapped.Formal_Position();
      IPA_NODE_SECTION_INFO* sec = node->Section_Annot();
      STATE* state = sec->Get_formal(old_formal_position); 
      if (state->Is_removed()) { 
        VALUE_DYN_ARRAY* formal_array = node->Cprop_Annot();
	SUMMARY_VALUE* sv = &(*formal_array)[old_formal_position];
	if (sv->Is_constant()) { 
	  INT64 int_value = sv->Get_int_const_value();
	  term->Set_type(LTKIND_CONST);
	  term->Set_coeff(int_value);
	  term->Set_desc(CONST_DESC);
	  term->Set_projected_level(0);
	  return; 
        } else if (sv->Is_const_st()) { 
	  ST_IDX st_idx = sv->Get_const_st_idx();
	  ivar_mapped.Set_St_Idx(st_idx);
	  ivar_mapped.Set_Offset(0);
	  ivar_mapped.Set_Mtype(TY_mtype(ST_type(st_idx)));
	} else {
	  FmtAssert(FALSE,
	    ("IPA_LNO_Map_Term: Unexpected constant index type"));
	} 
      } else { 
	INT removed_count = 0; 
	SUMMARY_PROCEDURE* osp = node->Summary_Proc();
	for (INT i = 0; i < old_formal_position; i++) { 
	  STATE* state_local = sec->Get_formal(i);
	  if (state_local->Is_removed())
	    removed_count++; 
	} 
	INT32 new_formal_position = ivar_mapped.Formal_Position();
	new_formal_position -= removed_count; 
	FmtAssert(new_formal_position >= 0,  
	  ("IPA_LNO_Map_Term: negative formal position"));
	ivar_mapped.Set_Formal_Position(new_formal_position);
      } 
    } 
    INT mapped_idx = -1;
    INT i;

    for (i = 0; i < IPA_LNO_Summary->Ivar_Count(); i++) { 
      if (*IPA_LNO_Summary->Ivar(i) == ivar_mapped)
	break;
    } 
    if (i < IPA_LNO_Summary->Ivar_Count()) { 
      mapped_idx = i;
    } else { 
      IPA_LNO_Summary->Ivar_Array()->AddElement(ivar_mapped);
      mapped_idx = IPA_LNO_Summary->Ivar_Array()->Lastidx();
    } 
    term->Set_desc(mapped_idx);
  } 
} 

//----------------------------------------------------------------
// NAME: IPA_LNO_Get_Key
// FUNCTION: Return a UINT64 which is a key mapping the 'term' with 
//   the given 'num_terms' into a term hash table. 
//----------------------------------------------------------------

UINT64 IPA_LNO_Get_Key(TERM* term, INT num_terms)
{
  UINT64 key = 0;
  // We create the key as follows:
  // Use the following fields of the first term:
  //   COEFF, DESCR, TYPE
  //   number of terms
  UINT64 temp = 0;
  temp = (INT64) num_terms << 56;
  key = key | temp;
  temp = (INT64) term->Get_type() << 48;
  key = key | temp;
  temp = (INT64) term->Get_desc() << 32;
  key = key | temp;
  temp = (INT64) term->Get_coeff();
  key = key | temp;
  return key;
} 

//----------------------------------------------------------------
// NAME: IPA_LNO_Insert_Terms
// FUNCTION: Enter into the term hash table an entry for 'term_in' 
//   which is stored in the term array at index 'idx' and has 'count'
//   elements.  Use 'IPA_LNO_Summary' to get at the term hash table. 
//----------------------------------------------------------------

static void IPA_LNO_Insert_Terms(TERM* term_in,
				 INT idx,
				 INT count,
				 IPA_LNO_WRITE_SUMMARY* IPA_LNO_Summary)
{
  MEM_POOL* mem_pool = IPA_LNO_Summary->Mem_Pool();
  UINT64 key = IPA_LNO_Get_Key(term_in, count);
  TERM_HASH_TABLE *hash_table = IPA_LNO_Summary->Term_Hash_Table();
  INTEGER_ARRAY* int_array;
  INT id;

  if (int_array = hash_table->Find(key)) {
    for (INT i = 0; i <= int_array->Lastidx(); i++) {
      id = (*int_array)[i];
      TERM* start_term = IPA_LNO_Summary->Term(id);
      if (term_in->Is_equal(start_term, count)) 
        return;
    }
    // If we reach here then we need to insert this into the int array
    id = int_array->Newidx();
    (*int_array)[id] = idx;
    return;
  }

  // If we reach here then we need to create the int array.
  // Enter this element into the int array and enter the int array
  //   into the hash table
  int_array = (INTEGER_ARRAY*) CXX_NEW(INTEGER_ARRAY(mem_pool), mem_pool);
  id = int_array->Newidx();
  (*int_array)[id] = idx;
  hash_table->Enter(key, int_array);
} 

//----------------------------------------------------------------
// NAME: IPA_LNO_Search_For_Terms
// FUNCTION: Return "1 + the index into the TERM array of the term"
//   which is has already been created and is equal to the LINEX 'lx'. 
//   Return 0 if there is no such TERM. 
//----------------------------------------------------------------

static INT IPA_LNO_Search_For_Terms(LINEX* lx,
				    IPA_LNO_WRITE_SUMMARY* IPA_LNO_Summary)
{
  TERM* term = lx->Get_term(0);
  UINT64 key = IPA_LNO_Get_Key(term, lx->Num_terms());
  TERM_HASH_TABLE* hash_table = IPA_LNO_Summary->Term_Hash_Table();
  INTEGER_ARRAY* int_array = hash_table->Find(key);
  if (!int_array)
    return 0;

  for (INT i = 0; i <= int_array->Lastidx(); i++) {
    INT id = (*int_array)[i];
    TERM* term_local = IPA_LNO_Summary->Term(id);
    if (term_local->Is_equal(term, lx->Num_terms())) {
      // Since terms start at 0, return id + 1
      return id + 1;
    }
  }
  return 0;
} 

//----------------------------------------------------------------
// NAME: IPA_LNO_Map_Projected_Node_Array
// FUNCTION: Map the PROJECTED_ARRAY 'pna_in' to the 'IPA_LNO_Summary'.
//   Use 'node' to get the file header. 
//----------------------------------------------------------------

static INT IPA_LNO_Map_Projected_Node_Array(IPA_NODE* node, 
					    PROJECTED_ARRAY* pna_in,
					    IPA_LNO_WRITE_SUMMARY* 
					      IPA_LNO_Summary)
{ 
  INT new_idx = -1; 
  INT reuse_linex_idx = -1;
  PROJECTED_ARRAY* projected_node_array 
    = IPA_LNO_Summary->Projected_Node_Array();
  TERM_ARRAY* term_array = IPA_LNO_Summary->Term_Array();

  for (INT i = 0; i <= pna_in->Lastidx(); i++) {
    INT idx = projected_node_array->Newidx(); 
    if (i == 0)
      new_idx = idx; 
    PROJECTED_NODE* pn_in = &(*pna_in)[i]; 
    PROJECTED_NODE* pn_out = &(*projected_node_array)[idx];
    pn_out->Set_flags(pn_in->Get_flags()); 

    // Map the upper bound 
    LINEX* lx_upper = pn_in->Get_upper_linex(); 
    if (!pn_in->Is_messy_ub() && lx_upper->Num_terms() != -1) { 
      for (INT j = 0; j <= lx_upper->Num_terms(); j++) {
	IPA_LNO_Map_Term(node, lx_upper->Get_term(j), IPA_LNO_Summary);
      }
      INT reuse_linex_idx = IPA_LNO_Search_For_Terms(lx_upper,IPA_LNO_Summary);
      if (reuse_linex_idx == 0) { 
        for (INT j = 0; j <= lx_upper->Num_terms(); j++) { 
          term_array->AddElement(*(lx_upper->Get_term(j)));
        }
        INT first_term_idx = term_array->Lastidx() - lx_upper->Num_terms();
        IPA_LNO_Insert_Terms(lx_upper->Get_term(0), first_term_idx, 
                             lx_upper->Num_terms(), IPA_LNO_Summary);
        pn_out->Set_ub_term_index(first_term_idx);
      } else { 
        pn_out->Set_ub_term_index(reuse_linex_idx-1);
      }
      pn_out->Set_ub_term_count(lx_upper->Num_terms()+1);
    } else { 
      pn_out->Set_ub_term_index(-1);
      pn_out->Set_ub_term_count(0);
    } 

    // Map the lower bound 
    LINEX* lx_lower = pn_in->Get_lower_linex(); 
    if (!pn_in->Is_messy_lb() && lx_lower->Num_terms() != -1) { 
      for (INT j = 0; j <= lx_lower->Num_terms(); j++) {
	IPA_LNO_Map_Term(node, lx_lower->Get_term(j), IPA_LNO_Summary);
      }
      INT reuse_linex_idx = IPA_LNO_Search_For_Terms(lx_lower,IPA_LNO_Summary);
      if (reuse_linex_idx == 0) { 
        for (INT j = 0; j <= lx_lower->Num_terms(); j++) { 
          term_array->AddElement(*(lx_lower->Get_term(j)));
        }
        INT first_term_idx = term_array->Lastidx() - lx_lower->Num_terms();
        IPA_LNO_Insert_Terms(lx_lower->Get_term(0), first_term_idx, 
                             lx_lower->Num_terms(), IPA_LNO_Summary);
        pn_out->Set_lb_term_index(first_term_idx);
      } else { 
        pn_out->Set_lb_term_index(reuse_linex_idx-1);
      }
      pn_out->Set_lb_term_count(lx_lower->Num_terms()+1);
    } else { 
      pn_out->Set_lb_term_index(-1);
      pn_out->Set_lb_term_count(0);
    } 

    // Map the step 
    LINEX* lx_step = pn_in->Get_step_linex(); 
    if (!pn_in->Is_messy_step() && lx_step->Num_terms() != -1) { 
      for (INT j = 0; j <= lx_step->Num_terms(); j++) {
	IPA_LNO_Map_Term(node, lx_step->Get_term(j), IPA_LNO_Summary);
      }
      INT reuse_linex_idx = IPA_LNO_Search_For_Terms(lx_step,IPA_LNO_Summary);
      if (reuse_linex_idx == 0) { 
        for (INT j = 0; j <= lx_step->Num_terms(); j++) { 
          term_array->AddElement(*(lx_step->Get_term(j)));
        }
        INT first_term_idx = term_array->Lastidx() - lx_step->Num_terms();
        IPA_LNO_Insert_Terms(lx_step->Get_term(0), first_term_idx, 
                             lx_step->Num_terms(), IPA_LNO_Summary);
        pn_out->Set_step_term_index(first_term_idx);
      } else { 
        pn_out->Set_step_term_index(reuse_linex_idx-1);
      }
      pn_out->Set_step_term_count(lx_step->Num_terms()+1);
    } else { 
      pn_out->Set_step_term_index(-1);
      pn_out->Set_step_term_count(0);
    }

    // Map the segment length
    LINEX* lx_segment_length = pn_in->Get_segment_length_linex();
    if (lx_segment_length != NULL) { 
      for (INT j = 0; j <= lx_segment_length->Num_terms(); j++) {
        IPA_LNO_Map_Term(node, lx_segment_length->Get_term(j),IPA_LNO_Summary);
      }
      INT reuse_linex_idx = IPA_LNO_Search_For_Terms(lx_segment_length,
                                                     IPA_LNO_Summary);
      if (reuse_linex_idx == 0) { 
        for (INT j = 0; j <= lx_segment_length->Num_terms(); j++) { 
          term_array->AddElement(*(lx_segment_length->Get_term(j)));
        }
        INT first_term_idx = term_array->Lastidx() - 
          lx_segment_length->Num_terms();
        IPA_LNO_Insert_Terms(lx_segment_length->Get_term(0), first_term_idx, 
                             lx_segment_length->Num_terms(), IPA_LNO_Summary);
        pn_out->Set_segment_length_term_index(first_term_idx);
      } else { 
        pn_out->Set_segment_length_term_index(reuse_linex_idx-1);
      }
      pn_out->Set_segment_length_term_count(lx_segment_length->Num_terms()+1);
    } else { 
      pn_out->Set_segment_length_term_index(-1);
      pn_out->Set_segment_length_term_count(0);
    }
    
    // Map the segment stride
    LINEX* lx_segment_stride = pn_in->Get_segment_stride_linex();
    if (lx_segment_stride != NULL) { 
      for (INT j = 0; j <= lx_segment_stride->Num_terms(); j++) {
        IPA_LNO_Map_Term(node, lx_segment_stride->Get_term(j),IPA_LNO_Summary);
      }
      INT reuse_linex_idx = IPA_LNO_Search_For_Terms(lx_segment_stride,
                                                     IPA_LNO_Summary);
      if (reuse_linex_idx == 0) { 
        for (INT j = 0; j <= lx_segment_stride->Num_terms(); j++) { 
          term_array->AddElement(*(lx_segment_stride->Get_term(j)));
        }
        INT first_term_idx = term_array->Lastidx() - 
          lx_segment_stride->Num_terms();
        IPA_LNO_Insert_Terms(lx_segment_stride->Get_term(0), first_term_idx, 
                             lx_segment_stride->Num_terms(), IPA_LNO_Summary);
        pn_out->Set_segment_stride_term_index(first_term_idx);
      } else { 
        pn_out->Set_segment_stride_term_index(reuse_linex_idx-1);
      }
      pn_out->Set_segment_stride_term_count(lx_segment_stride->Num_terms()+1);
    } else { 
      pn_out->Set_segment_stride_term_index(-1);
      pn_out->Set_segment_stride_term_count(0);
    }
  }

  return new_idx;
} 

//----------------------------------------------------------------
// NAME: IPA_LNO_Map_Projected_Region
// FUNCTION: Map the PROJECTED_REGION 'pr_in' to the 'IPA_LNO_Summary' 
//   Use 'node' to get the file header. 
//----------------------------------------------------------------

static INT IPA_LNO_Map_Projected_Region(IPA_NODE* node, 
					PROJECTED_REGION* pr_in,
				        IPA_LNO_WRITE_SUMMARY* 
				          IPA_LNO_Summary)
{
  PROJECTED_REGION_ARRAY* projected_region_array = 
    IPA_LNO_Summary->Projected_Region_Array(); 
  INT idx = projected_region_array->Newidx();
  PROJECTED_REGION* pr_out = &(*projected_region_array)[idx];
  pr_out->Copy_write(pr_in);
  if (pr_in->Is_messy_region()) {
    pr_out->Set_id(-1);
  } else {
    INT new_idx = pr_in->Get_projected_array()  
      ? IPA_LNO_Map_Projected_Node_Array(node, 
      pr_in->Get_projected_array(), IPA_LNO_Summary) : -1;
    pr_out->Set_id(new_idx);
  } 
  return idx;
} 

//----------------------------------------------------------------
// NAME: IPA_LNO_Map_Formal_Array
// FUNCTION: Map the 'position'th formal parameter of the function with
//   IPA_NODE 'node' and STATE entry 'state' to the 'IPA_LNO_Summary'. 
//----------------------------------------------------------------

static INT IPA_LNO_Map_Formal_Array(IPA_NODE* node, 
				    INT old_position, 
				    INT new_position, 
				    STATE* state, 
                                    IPA_LNO_WRITE_SUMMARY* IPA_LNO_Summary)
{
  IPA_LNO_SUMMARY_FORMAL_ARRAY* formal_array 
    = IPA_LNO_Summary->Formal_Array();
  INT idx = formal_array->Newidx();
  IPA_LNO_SUMMARY_FORMAL* sf = &((*formal_array)[idx]);
  SUMMARY_PROCEDURE* osp = node->Summary_Proc();
  INT formal_index = osp->Get_formal_index() + old_position;
  SUMMARY_FORMAL* old_formal_array = IPA_get_formal_array(node);
  SUMMARY_FORMAL* osf = &old_formal_array[formal_index];
  INT symbol_index = osf->Get_symbol_index();
  SUMMARY_SYMBOL* old_symbol_array = IPA_get_symbol_array(node);
  SUMMARY_SYMBOL* oss = &old_symbol_array[symbol_index];
  sf->Set_Machine_Type(osf->Get_machine_type());
  sf->Clear_State();
  sf->Set_Position(new_position); 
  if (state->Is_scalar()) {
    sf->Set_Scalar();
    IPAA_NODE_INFO* ipaa = node->Mod_Ref_Info();
    if (ipaa == NULL || ipaa->Is_formal_dref_elmt(old_position) 
	|| ipaa->Is_formal_iref_elmt(old_position))
      sf->Set_Use();
    if (ipaa == NULL || ipaa->Is_formal_dmod_elmt(old_position) 
	|| ipaa->Is_formal_imod_elmt(old_position))
      sf->Set_May_Kill();
    sf->Set_Mod_Array_Section_Index(-1);
    sf->Set_Ref_Array_Section_Index(-1);
    sf->Set_Decl_Array_Section_Index(-1);
  } else if (osf->Get_region_index() != -1) { 
    sf->Set_Array();
    sf->Reset_Scalar(); 
    if (state->Get_projected_mod_region() == NULL) {
      sf->Set_Mod_Array_Section_Index(-1);
    } else { 
      sf->Set_May_Kill();
      INT idx_mod = IPA_LNO_Map_Projected_Region(node, 
        state->Get_projected_mod_region(), IPA_LNO_Summary);
      sf->Set_Mod_Array_Section_Index(idx_mod);
    }
    if (state->Get_projected_ref_region() == NULL) { 
      sf->Set_Ref_Array_Section_Index(-1);
    } else { 
      sf->Set_Use();
      INT idx_ref = IPA_LNO_Map_Projected_Region(node, 
        state->Get_projected_ref_region(), IPA_LNO_Summary);
      sf->Set_Ref_Array_Section_Index(idx_ref);
    }
    PROJECTED_REGION* proj_region_array
      = IPA_get_proj_region_array(node);
    INT idx_region = osf->Get_region_index();
    REGION_ARRAYS* region_array = IPA_get_region_array(node);
    REGION_ARRAYS* ra = &region_array[idx_region];
    INT idx_proj_region = ra->Get_idx();
    PROJECTED_REGION* pr_decl_old =  &proj_region_array[idx_proj_region];
    PROJECTED_REGION* pr_decl_new = Projected_Region_To_Memory(node,
      pr_decl_old, &IPA_LNO_mem_pool);
    INT idx_decl = IPA_LNO_Map_Projected_Region(node, pr_decl_new,
      IPA_LNO_Summary);
    sf->Set_Decl_Array_Section_Index(idx_decl);
  } else { 
    sf->Set_Unknown();
    sf->Set_Mod_Array_Section_Index(-1);
    sf->Set_Ref_Array_Section_Index(-1);
    sf->Set_Decl_Array_Section_Index(-1);
  } 
  return idx;
}

//----------------------------------------------------------------
// NAME: IPA_LNO_Map_Global_Array
// FUNCTION: Create a IPA_LNO_SUMMARY_GLOBAL for the given global array 
//   and add it to the 'IPA_LNO_Summary'. 
//     The basic information about the common is given in 
//   the GLOBAL_ARRAY_INFO 'css'.  The 'cs' 
//   tells us about the element in the common which we are accessing. 
//   In particular, we need it to get the name of the common and the 
//   offset at which the element in the common appears.  MOD and REF 
//   information for this element in the common is given by the 'css'. 
//   which was computed during array section propagation. 
//     The 'node' is used to get the current file index. 
//----------------------------------------------------------------

static void IPA_LNO_Map_Global_Array(IPA_NODE* node,
                                     GLOBAL_ARRAY_INFO* css,
                                     IPA_LNO_WRITE_SUMMARY* IPA_LNO_Summary)
{
  PROJECTED_REGION* pr_mod = css->Get_projected_mod_region();
  INT idx_mod = (pr_mod == NULL) ? 
    -1 : IPA_LNO_Map_Projected_Region(node, pr_mod, IPA_LNO_Summary); 

  PROJECTED_REGION* pr_ref = css->Get_projected_ref_region(); 
  INT idx_ref = (pr_ref == NULL) ? 
    -1 : IPA_LNO_Map_Projected_Region(node, pr_ref, IPA_LNO_Summary); 

  IPA_LNO_SUMMARY_GLOBAL_ARRAY* globals = IPA_LNO_Summary->Global_Array();
  globals->AddElement(IPA_LNO_SUMMARY_GLOBAL(css->St_Idx(),
                                             idx_mod, idx_ref,
                                             FALSE, FALSE));
}

//----------------------------------------------------------------
// NAME: IPA_LNO_Map_Value
// FUNCTION: Create a new VALUE which is either nearly or completely 
//   identical to the value at 'svv_old' and return the index of this 
//   newly created VALUE.  If the VALUE is a formal, its value is in-
//   creased by 'formal_index_base'.  If it is GLOBAL, it is increased    
//   by 'global_index_base'.  
//----------------------------------------------------------------

static INT IPA_LNO_Map_Value(SUMMARY_VALUE* svv_old, 
			     INT formal_index_base, 
			     INT global_index_base, 
			     IPA_LNO_WRITE_SUMMARY* IPA_LNO_Summary)
{ 
  DYN_ARRAY<SUMMARY_VALUE>* value_array = IPA_LNO_Summary->Value_Array();
  INT idx = value_array->Newidx();
  SUMMARY_VALUE* svv_new = &((*value_array)[idx]);
  bcopy(svv_old, svv_new, sizeof(SUMMARY_VALUE));
  if (svv_old->Is_formal()) { 
    svv_new->Set_formal_index(formal_index_base + svv_old->Get_formal_index());
  } else if (svv_old->Is_global()) { 
    svv_new->Set_global_index(global_index_base + svv_old->Get_global_index());
  } 
  return idx; 
} 

//----------------------------------------------------------------
// NAME: IPA_LNO_Map_Expr_Node
// FUNCTION: Map the EXPR 'sxx_old' to the EXPR 'sxx_new' by adding 
//   the index of the first VALUE for this program unit 'value_index_base' 
//   or index of the first EXPR for this program unit 'expr_index_base' to 
//   the 'kid'th part of 'sxx_old', depeding on whether it is a VALUE or 
//   EXPR index.      
//----------------------------------------------------------------

static void IPA_LNO_Map_Expr_Node(SUMMARY_EXPR* sxx_old,
				  SUMMARY_EXPR* sxx_new, 
				  INT value_index_base,
				  INT expr_index_base,
				  INT kid)
{
  if (sxx_old->Is_expr_value(kid)) { 
    INT old_index = sxx_old->Get_node_index(kid);
    sxx_new->Set_node_index(kid, value_index_base + old_index);
  } else if (sxx_old->Is_expr_expr(kid)) {
    INT old_index = sxx_old->Get_node_index(kid);
    sxx_new->Set_node_index(kid, expr_index_base + old_index);
  } else { 
    FmtAssert(FALSE, 
      ("IPA_LNO_Map_Expr: Not handling other cases yet"));
  }
}  

//----------------------------------------------------------------
// NAME: IPA_LNO_Map_Expr
// FUNCTION: Map the EXPR 'sxx_old' to the EXPR 'sxx_new' by adding 
//   the index of the first VALUE for this program unit 'value_index_base' 
//   to each VALUE index and the index of the first EXPR for this program 
//   unit 'expr_index_base' to each EXPR index in 'sxx_old'. 
//----------------------------------------------------------------

static INT IPA_LNO_Map_Expr(SUMMARY_EXPR* sxx_old, 
			    INT value_index_base, 
			    INT expr_index_base, 
			    IPA_LNO_WRITE_SUMMARY* IPA_LNO_Summary)
{
  DYN_ARRAY<SUMMARY_EXPR>* expr_array = IPA_LNO_Summary->Expr_Array();
  INT idx = expr_array->Newidx();
  SUMMARY_EXPR* sxx_new = &((*expr_array)[idx]);
  bcopy(sxx_old, sxx_new, sizeof(SUMMARY_EXPR));
  if (sxx_old->Has_const_operand()) {
    IPA_LNO_Map_Expr_Node(sxx_old, sxx_new, value_index_base,
      expr_index_base, 0);
  } else { 
    IPA_LNO_Map_Expr_Node(sxx_old, sxx_new, value_index_base,
      expr_index_base, 0);
    IPA_LNO_Map_Expr_Node(sxx_old, sxx_new, value_index_base,
      expr_index_base, 1);
  }
  return idx; 
}

//----------------------------------------------------------------
// NAME: IPA_LNO_Map_Node 
// FUNCTION: Map information needed for LNO parallelization
//----------------------------------------------------------------

extern void IPA_LNO_Map_Node(IPA_NODE* node,
                             IPA_LNO_WRITE_SUMMARY* IPA_LNO_Summary)
{
  // Set the context for this node. 
  IPA_NODE* ipan_main_entry = Main_Entry(node);
  IPA_NODE_CONTEXT context(ipan_main_entry);

  // Map per function information
  if (Get_Trace(TP_IPA, IPA_TRACE_LNO_WRITE))
    fprintf(stdout, "\nLNO mapping for function %s\n", node->Name());

  IPA_LNO_SUMMARY_PROCEDURE_ARRAY* procedure_array 
    = IPA_LNO_Summary->Procedure_Array();
  INT idx = procedure_array->Newidx();
  IPA_LNO_SUMMARY_PROCEDURE* sp = &((*procedure_array)[idx]);
  sp->Reset_Has_Incomplete_Array_Info();
  SUMMARY_PROCEDURE* osp = node->Summary_Proc();
  if (osp->Has_incomplete_array_info()) 
    sp->Set_Has_Incomplete_Array_Info();
  sp->Set_St_Idx(ST_st_idx(node->Func_ST()));

  // Handle formals
  sp->Set_Formal_Index(-1);
  sp->Set_Formal_Count(0);
  IPA_NODE_SECTION_INFO* sec = node->Section_Annot();
  if (sec != NULL) { 
    INT k = 0; 
    for (INT i = 0; i < osp->Get_formal_count(); i++) {
      STATE* state = sec->Get_formal(i);
      if (!state->Is_removed()) { 
	INT idx = IPA_LNO_Map_Formal_Array(node, i, k, state, 
	  IPA_LNO_Summary); 
	if (k++ == 0) 
	  sp->Set_Formal_Index(idx); 
      } 
    } 
    sp->Set_Formal_Count(k);
  }

  IPAA_NODE_INFO* modref_info = node->Mod_Ref_Info();

  // Handle global scalars 
  IPA_LNO_SUMMARY_GLOBAL_ARRAY* globals = IPA_LNO_Summary->Global_Array();
  INT global_count = 0;
  sp->Set_Global_Index(-1);
  ST* st;
  INT i;
  FOREACH_SYMBOL (GLOBAL_SYMTAB, st, i) {
    if (ST_class(st) == CLASS_VAR && 
        ST_base_idx(st) == ST_st_idx(st) &&
        TY_kind(ST_type(st)) != KIND_ARRAY &&
	!ST_is_split_common(st)) {
      ST_IDX st_idx = ST_st_idx(st);
      BOOL scalar_mod = modref_info == NULL 
	|| modref_info->Is_def_elmt(ST_IDX_index(st_idx));
      BOOL scalar_ref = modref_info == NULL
	|| modref_info->Is_eref_elmt(ST_IDX_index(st_idx));
      if (scalar_mod || scalar_ref || ST_sclass(st) == SCLASS_PSTATIC) {
        // PV 655188: Need to go conservative on these for now.
	if (ST_sclass(st) == SCLASS_PSTATIC) {
	  if (!ST_is_const_var(st))
	    scalar_mod = TRUE; 
   	  scalar_ref = TRUE; 
        } 
        // check if it's a common block 
        COMMON_BLOCK_ELEMENTS_MAP::const_iterator block_iter = 
          Common_Block_Elements_Map->find(st_idx);
        if (block_iter == Common_Block_Elements_Map->end()) {
          // if not, enter only the global st_idx
          globals->AddElement(IPA_LNO_SUMMARY_GLOBAL(st_idx, 
                                                     -1, -1,
                                                     scalar_mod, scalar_ref));
          if (global_count++ == 0)
            sp->Set_Global_Index(globals->Lastidx());
        }
        else {
          // for a common block, enter all its scalar elements
          const BLOCK_ELEMENTS* block = (*block_iter).second;
          BLOCK_ELEMENTS::const_iterator element = block->begin();
          BLOCK_ELEMENTS::const_iterator element_end = block->end();

          for (; element != element_end; ++element) {
            ST_IDX elem_st_idx = (*element).second;
            if (ST_class(elem_st_idx) == CLASS_VAR && 
                TY_kind(ST_type(elem_st_idx)) != KIND_ARRAY) {

              globals->AddElement(IPA_LNO_SUMMARY_GLOBAL(elem_st_idx, 
                                                         -1, -1,
                                                         scalar_mod, 
                                                         scalar_ref));
              if (global_count++ == 0)
                sp->Set_Global_Index(globals->Lastidx());
            }
          }
        }
      }
    }
  } 
  // Handle global arrays  
  if (sec != NULL) { 
    ST_IDX st_idx;
    GLOBAL_ARRAY_LIST* csl = NULL; 
    GLOBAL_ARRAY_TABLE* cst = sec->Global_Array_Table();
    GLOBAL_ARRAY_TABLE_ITER cst_iter(cst);
    while (cst_iter.Step(&st_idx, &csl)) {
      GLOBAL_ARRAY_LIST_ITER iter(csl);
      if (csl->Is_messy()) 
	sp->Set_Has_Incomplete_Array_Info();
      for (iter.First(); !iter.Is_Empty(); iter.Next()) {
	IPA_LNO_Map_Global_Array(node, iter.Cur(), IPA_LNO_Summary);
	if (global_count++ == 0) 
	  sp->Set_Global_Index(globals->Lastidx());
      }
    }
  } 
  sp->Set_Global_Count(global_count);

  // Handle execution cost values
  sp->Set_Value_Index(-1);
  sp->Set_Value_Count(0);
  INT value_count_base = IPA_LNO_Summary->Value_Count();
  if (sec != NULL) { 
    DYN_ARRAY<SUMMARY_VALUE>* sv = sec->Get_value();
    for (INT i = 0; i <= sv->Lastidx(); i++) { 
      SUMMARY_VALUE* svv = &(*sv)[i];
      INT idx = IPA_LNO_Map_Value(svv, sp->Formal_Index(), 
	sp->Global_Index(), IPA_LNO_Summary);
      if (i == 0)
	sp->Set_Value_Index(idx);
    } 
    sp->Set_Value_Count(sv->Lastidx() + 1);
  }  

  // Handle execution cost exprs 
  sp->Set_Expr_Index(-1);
  sp->Set_Expr_Count(0);
  INT expr_count_base = IPA_LNO_Summary->Expr_Count();
  if (sec != NULL) { 
    DYN_ARRAY<SUMMARY_EXPR>* sx = sec->Get_expr();
    for (INT i = 0; i <= sx->Lastidx(); i++) { 
      SUMMARY_EXPR* sxx = &(*sx)[i];
      INT idx = IPA_LNO_Map_Expr(sxx, value_count_base, expr_count_base, 
        IPA_LNO_Summary);
      if (i == 0)
	sp->Set_Expr_Index(idx);
    } 
    sp->Set_Expr_Count(sx->Lastidx() + 1);
  } 
}

//-----------------------------------------------------------------------
// NAME: IPA_LNO_Open_Output_Info
// FUNCTION: Open the IPALNO file 'output_file' and return an IPA_LNO_
//   WRITE_IO with information describing the memory map of of its contents
//   and other information.  Log error message EC_IPALNO_Create if appro-
//   priate.
//-----------------------------------------------------------------------

static IPA_LNO_WRITE_FILE *IPA_LNO_Open_Output_Info(char* output_file)
{
  Set_Error_Phase("Writing IPA LNO file");
  IPA_LNO_WRITE_FILE* IPA_LNO_Output_File =
    CXX_NEW(IPA_LNO_WRITE_FILE(), Malloc_Mem_Pool);
  IPA_LNO_Output_File->Open_Write_File(output_file);
  if (IPA_LNO_Output_File->ofl == NULL)
    ErrMsg(EC_IPALNO_Create, IPA_LNO_Output_File->ofl->file_name, errno);
  return IPA_LNO_Output_File;
}

//-----------------------------------------------------------------------
// NAME: IPA_LNO_Write_Sections
// FUNCTION: Write the section information given in 'IPA_LNO_Summary' to 
//   the 'IPA_LNO_Output_File'. 
//-----------------------------------------------------------------------

static void IPA_LNO_Write_Sections(IPA_LNO_WRITE_SUMMARY* IPA_LNO_Summary,
				   IPA_LNO_WRITE_FILE* IPA_LNO_Output_File)
{
  char* buffer = NULL; 
  INT buffer_size = 0;
  if (Get_Trace(TP_IPA, IPA_TRACE_LNO_WRITE))  
    fprintf(stdout, "\n+++ BEGIN WRITING IPA LNO FILE +++\n"); 
  if (IPA_LNO_Summary->Ivar_Count() > 0) { 
    if (Get_Trace(TP_IPA, IPA_TRACE_LNO_WRITE)) { 
      fprintf(stdout, "Writing %d ivars:\n", IPA_LNO_Summary->Ivar_Count());
      for (INT i = 0; i < IPA_LNO_Summary->Ivar_Count(); i++)
	IPA_LNO_Summary->Ivar(i)->IPA_LNO_Print_File(stdout, i);
    } 
    buffer = (char *) IPA_LNO_Summary->Ivar(0);
    buffer_size = sizeof(IVAR) * IPA_LNO_Summary->Ivar_Count();
    IPA_LNO_Output_File->Write_Section(IPA_IVAR, IPA_IVAR_NAME,
      buffer, buffer_size);
  } 
  if (IPA_LNO_Summary->Procedure_Count() > 0) { 
    if (Get_Trace(TP_IPA, IPA_TRACE_LNO_WRITE)) { 
      fprintf(stdout, "Writing %d procedures:\n", 
	IPA_LNO_Summary->Procedure_Count());
      for (INT i = 0; i < IPA_LNO_Summary->Procedure_Count(); i++)
	IPA_LNO_Summary->Procedure(i)->Print(stdout, i);
    } 
    buffer = (char *) IPA_LNO_Summary->Procedure(0);
    buffer_size = sizeof(IPA_LNO_SUMMARY_PROCEDURE) 
      * IPA_LNO_Summary->Procedure_Count();
    IPA_LNO_Output_File->Write_Section(IPA_PROCEDURE, IPA_PROCEDURE_NAME,
      buffer, buffer_size);
  } 
  if (IPA_LNO_Summary->Formal_Count() > 0) { 
    if (Get_Trace(TP_IPA, IPA_TRACE_LNO_WRITE)) { 
      fprintf(stdout, "Writing %d formals:\n", 
	IPA_LNO_Summary->Formal_Count());
      for (INT i = 0; i < IPA_LNO_Summary->Formal_Count(); i++)
	IPA_LNO_Summary->Formal(i)->Print(stdout, i);
    } 
    buffer = (char *) IPA_LNO_Summary->Formal(0);
    buffer_size = sizeof(IPA_LNO_SUMMARY_FORMAL) 
      * IPA_LNO_Summary->Formal_Count();
    IPA_LNO_Output_File->Write_Section(IPA_FORMAL, IPA_FORMAL_NAME,
      buffer, buffer_size);
  } 
  if (IPA_LNO_Summary->Global_Count() > 0) { 
    if (Get_Trace(TP_IPA, IPA_TRACE_LNO_WRITE)) { 
      fprintf(stdout, "Writing %d globals:\n", 
	IPA_LNO_Summary->Global_Count());
      for (INT i = 0; i < IPA_LNO_Summary->Global_Count(); i++)
	IPA_LNO_Summary->Global(i)->Print(stdout, i);
    } 
    buffer = (char *) IPA_LNO_Summary->Global(0);
    buffer_size = sizeof(IPA_LNO_SUMMARY_GLOBAL) 
      * IPA_LNO_Summary->Global_Count();
    IPA_LNO_Output_File->Write_Section(IPA_GLOBAL, IPA_GLOBAL_NAME,
      buffer, buffer_size);
  } 
  if (IPA_LNO_Summary->Projected_Region_Count() > 0) { 
    if (Get_Trace(TP_IPA, IPA_TRACE_LNO_WRITE)) { 
      fprintf(stdout, "Writing %d projected regions:\n", 
	IPA_LNO_Summary->Projected_Region_Count());
      for (INT i = 0; i < IPA_LNO_Summary->Projected_Region_Count(); i++)
	IPA_LNO_Summary->Projected_Region(i)->IPA_LNO_Print_File(stdout, i);
    } 
    buffer = (char *) IPA_LNO_Summary->Projected_Region(0);
    buffer_size = sizeof(PROJECTED_REGION) 
      * IPA_LNO_Summary->Projected_Region_Count();
    IPA_LNO_Output_File->Write_Section(IPA_PROJECTED_REGION, 
      IPA_PROJECTED_REGION_NAME, buffer, buffer_size);
  } 
  if (IPA_LNO_Summary->Projected_Node_Count() > 0) { 
    if (Get_Trace(TP_IPA, IPA_TRACE_LNO_WRITE)) { 
      fprintf(stdout, "Writing %d projected nodes:\n", 
	IPA_LNO_Summary->Projected_Node_Count());
      for (INT i = 0; i < IPA_LNO_Summary->Projected_Node_Count(); i++)
	IPA_LNO_Summary->Projected_Node(i)->IPA_LNO_Print_File(stdout, i);
    }
    buffer = (char *) IPA_LNO_Summary->Projected_Node(0);
    buffer_size = sizeof(PROJECTED_NODE) 
      * IPA_LNO_Summary->Projected_Node_Count();
    IPA_LNO_Output_File->Write_Section(IPA_PROJECTED_ARRAY, 
      IPA_PROJECTED_ARRAY_NAME, buffer, buffer_size);
  } 
  if (IPA_LNO_Summary->Term_Count() > 0) { 
    if (Get_Trace(TP_IPA, IPA_TRACE_LNO_WRITE)) { 
      fprintf(stdout, "Writing %d terms:\n", 
	IPA_LNO_Summary->Term_Count());
      for (INT i = 0; i < IPA_LNO_Summary->Term_Count(); i++)
	IPA_LNO_Summary->Term(i)->IPA_LNO_Print_File(stdout, i);
    }
    buffer = (char *) IPA_LNO_Summary->Term(0);
    buffer_size = sizeof(TERM) * IPA_LNO_Summary->Term_Count();
    IPA_LNO_Output_File->Write_Section(IPA_TERM_ARRAY, IPA_TERM_ARRAY_NAME,
      buffer, buffer_size);
  } 
  if (IPA_LNO_Summary->Value_Count() > 0) { 
    if (Get_Trace(TP_IPA, IPA_TRACE_LNO_WRITE)) { 
      fprintf(stdout, "Writing %d values:\n", 
	IPA_LNO_Summary->Value_Count());
      for (INT i = 0; i < IPA_LNO_Summary->Value_Count(); i++)
	IPA_LNO_Summary->Value(i)->WB_Print(stdout, i);
    }
    buffer = (char *) IPA_LNO_Summary->Value(0);
    buffer_size = sizeof(SUMMARY_VALUE) * IPA_LNO_Summary->Value_Count();
    IPA_LNO_Output_File->Write_Section(IPA_VALUE, IPA_VALUE_NAME,
      buffer, buffer_size);
  } 
  if (IPA_LNO_Summary->Expr_Count() > 0) { 
    if (Get_Trace(TP_IPA, IPA_TRACE_LNO_WRITE)) { 
      fprintf(stdout, "Writing %d exprs:\n", 
	IPA_LNO_Summary->Expr_Count());
      for (INT i = 0; i < IPA_LNO_Summary->Expr_Count(); i++)
	IPA_LNO_Summary->Expr(i)->WB_Print(stdout, i);
    }
    buffer = (char *) IPA_LNO_Summary->Expr(0);
    buffer_size = sizeof(SUMMARY_EXPR) * IPA_LNO_Summary->Expr_Count();
    IPA_LNO_Output_File->Write_Section(IPA_EXPR, IPA_EXPR_NAME,
      buffer, buffer_size);
  } 
  if (Get_Trace(TP_IPA, IPA_TRACE_LNO_WRITE))  
    fprintf(stdout, "+++ END WRITING IPA LNO FILE +++\n\n"); 
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_Close_Output_Info
// FUNCTION: Write out section information for the file described in
//   'IPA_LNO_Output_File'.  Log error message EC_IPALNO_Close if
//   appropriate.
//-----------------------------------------------------------------------

static void IPA_LNO_Close_Output_Info(IPA_LNO_WRITE_FILE* IPA_LNO_Output_File)
{
  Output_File* ofl = IPA_LNO_Output_File->ofl;
  if (ofl->output_fd < 0)
    ErrMsg(EC_IPALNO_Close, ofl->file_name, errno);
  INT error_code = IPA_LNO_Output_File->Close_Write_File();
  if (error_code == IPALNO_CLOSE_ERROR)
    ErrMsg(EC_IPALNO_Close, ofl->file_name, errno);
}
     
//-----------------------------------------------------------------------
// NAME: IPA_LNO_Write_Summary
// FUNCTION: Write out the summary information stored in the 'IPA_LNO_
//   Summary' to the IPALNO file. 
//-----------------------------------------------------------------------

extern void IPA_LNO_Write_Summary(IPA_LNO_WRITE_SUMMARY* IPA_LNO_Summary)
{
  FmtAssert(IPA_LNO_Summary != NULL, 
    ("IPA_LNO_Write_Summary: NULL summary"));

  extern string tmpdir __attribute__((weak));
  // Open the output file for LNO information from IPA
  char* dirname = (char *) tmpdir; 
  char* buffer = (char *) MEM_POOL_Alloc(Malloc_Mem_Pool,
    sizeof(char) * (strlen(dirname) + strlen("/IPA.LNO") + 1));    
  sprintf(buffer, "%s%s", dirname, "/IPA.LNO");
  IPA_LNO_WRITE_FILE* 
    IPA_LNO_Output_File = IPA_LNO_Open_Output_Info(buffer);
  MEM_POOL_FREE(Malloc_Mem_Pool, buffer);

  // Write out the sections that we have completed so far. 
  IPA_LNO_Write_Sections(IPA_LNO_Summary, IPA_LNO_Output_File);

  // Close the output file for LNO information from IPA
  IPA_LNO_Close_Output_Info(IPA_LNO_Output_File);
}
