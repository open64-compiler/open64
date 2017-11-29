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
#include <cmplrs/host.h>
#include "assert.h"
#define USE_STANDARD_TYPES
#include "defs.h"
#include "cxx_memory.h"
#include "cxx_hash.h"
#include "erglob.h"
#include "glob.h"
#include "mempool.h"
#include "sparse_bv.h"
#include "tracing.h"
#include "strtab.h"
#include "stab.h"
#include "wn.h"
#include "const.h"
#include "pu_info.h"
#include "irbdata.h"
#include "dwarf_DST_mem.h"
#include "ipc_defs.h"
#include "ipc_weak.h"
#include "ipc_file.h"
#include "opt_du.h"
#include "opt_alias_interface.h"
#include "dep_graph.h"
#include "wb_util.h"
#include "wb_browser.h"
#include "wb.h"
#include "wb_ipa.h"
#include "cg_browser.h"
#include "ipaa.h"
#include "ipa_section_annot.h"
#include "ipa_df.h"
#include "ipa_cprop.h"
#include "ipl_summary.h"
#include "ipa_summary.h"
#include "ipa_cg.h"
#include "ipa_reshape.h"
#include <alloca.h>

//-----------------------------------------------------------------------
// NAME: Projected_Region_To_Memory
// FUNCTION: Copy the PROJECTED_REGION 'pr' which is located on the file
//   with the given 'file_index' into memory and return a pointer to it. 
//   Use memory from the given 'mem_pool'. 
//-----------------------------------------------------------------------

extern 
PROJECTED_REGION* Projected_Region_To_Memory(IPA_NODE* node, 
                                             PROJECTED_REGION* pr,
                                             MEM_POOL* mem_pool)
{
  PROJECTED_REGION* pr_memory = CXX_NEW(PROJECTED_REGION(pr->Get_type(),
                                                         pr->Get_depth(),
                                                         pr->Get_num_dims(),
                                                         mem_pool), mem_pool);
  if (pr->Is_messy_region()) {
    return pr_memory;
  }
  
  pr_memory->Set_projected_array(CXX_NEW(PROJECTED_ARRAY(mem_pool), mem_pool));
  PROJECTED_NODE* pn = &IPA_get_projected_node_array(node)[pr->Get_id()];
  INT pr_count = pr->Get_num_dims();
  PROJECTED_NODE* pn_memory = 
    TYPE_MEM_POOL_ALLOC_N(PROJECTED_NODE, mem_pool, pr_count);
  bcopy(pn, pn_memory, pr_count * sizeof(PROJECTED_NODE));
  TERM* term_array = IPA_get_term_array(node);
  for (INT i = 0; i < pr_count; i++) { 
    pn_memory[i].Set_Mem_Pool(mem_pool);
    pn_memory[i].Create_linex(term_array);
    pr_memory->Set_projected_node(&pn_memory[i]);
  } 
  return pr_memory;
} 

//-----------------------------------------------------------------------
// NAME: Region_To_Memory
// FUNCTION: Copy the REGION_ARRAYS 'ra' which is located on the file
//   with the given 'file_index' into memory and return a pointer to it. 
//   Use memory from the given 'mem_pool'. 
//-----------------------------------------------------------------------
static REGION_ARRAYS* 
Region_To_Memory(IPA_NODE* node, 
		 REGION_ARRAYS* ra, 
		 MEM_POOL* mem_pool)
{
  FmtAssert(ra != NULL, 
    ("Region_To_Memory: Expecting non-NULL REGION_ARRAYS"));
  INT symbol_index = ra->Get_sym_id();
  REGION_ARRAYS* ra_new  
    = CXX_NEW(REGION_ARRAYS(mem_pool, symbol_index), mem_pool);
  PROJECTED_REGION_INFO_ARRAY* pria_new = ra_new->Get_projected_region_array();
  PROJECTED_REGION* proj_region_array = IPA_get_proj_region_array(node);
  for (INT i = ra->Get_idx(); i < ra->Get_idx() + ra->Get_count(); i++) { 
    PROJECTED_REGION* pr = &proj_region_array[i]; 
    PROJECTED_REGION* pr_new = Projected_Region_To_Memory(node, 
      pr, mem_pool);
    INT idx = pria_new->Newidx();
    PROJECTED_REGION_INFO* pri_new = &((*pria_new)[idx]);
    pri_new->Set_projected_region(pr_new);
  } 
  ra_new->Set_count(ra->Get_count());
  ra_new->Set_element_size(ra->Get_element_size());
  return ra_new; 
} 


//-----------------------------------------------------------------------
// NAME: Formal_Projected_Region
// FUNCTION: For the 'ra' representing the declaration of an array formal,
//   return the associated PROJECTED_REGION. 
//-----------------------------------------------------------------------

static PROJECTED_REGION* 
Formal_Projected_Region(REGION_ARRAYS* ra)
{ 
  FmtAssert(ra->Get_count() == 1,
    ("Formal_Projected_Region: Expecting single projected region"));
  PROJECTED_REGION_INFO_ARRAY* pria = ra->Get_projected_region_array();
  PROJECTED_REGION_INFO* pri = &(*pria)[0];
  return pri->Get_projected_region();
} 

//-----------------------------------------------------------------------
// NAME: Translate_Array
// FUNCTION: Consider the callsite 'sk' which is a call to 'ipan_callee' 
//   from 'ipan_caller' and which has an array as its 'position'-nth 
//   formal argument.  Rewrite the dimensions of that array in terms of 
//   the actual argument at that position and return this in a REGION_
//   ARRAYS.  Allocate memory for the return value from 'mem_pool'. 
//-----------------------------------------------------------------------

static REGION_ARRAYS* Translate_Array(IPA_NODE* ipan_caller, 
				      IPA_NODE* ipan_callee,
				      SUMMARY_CALLSITE* sk, 
				      INT position,
				      MEM_POOL* mem_pool)
{
  FmtAssert(position >= 0 && position < sk->Get_param_count(),
    ("Translate_Through_Call: 'position' out of range"));
  INT actual_index = sk->Get_actual_index() + position;
  SUMMARY_ACTUAL* actual_array = IPA_get_actual_array(ipan_caller);
  SUMMARY_ACTUAL* sa = &actual_array[actual_index];
  INT symbol_index = sa->Get_symbol_index();
  if (symbol_index == -1)
    return NULL;
  SUMMARY_PROCEDURE* sp_callee = ipan_callee->Summary_Proc();   
  FmtAssert(position >= 0 && position < sp_callee->Get_formal_count(), 
    ("Translate_Array: 'position' out of range"));
  INT formal_index = sp_callee->Get_formal_index() + position;
  SUMMARY_FORMAL* formal_array = IPA_get_formal_array(ipan_callee);
  SUMMARY_FORMAL* sf = &formal_array[formal_index];
  INT region_index = sf->Get_region_index();
  if (region_index == -1)
    return NULL;
  REGION_ARRAYS* region_array = IPA_get_region_array(ipan_callee);
  REGION_ARRAYS* ra = &region_array[region_index];
  FmtAssert(ra->Get_count() == 1, 
    ("Translate_Array: Expecting single projected region in formal")); 
  FmtAssert(ra->Is_formal(), ("Translate_Array: Expecting a formal")); 
  INT pr_index = ra->Get_idx();
  PROJECTED_REGION* proj_region_array
    = IPA_get_proj_region_array(ipan_callee);
  PROJECTED_REGION* pr = &proj_region_array[pr_index];
  PROJECTED_REGION* pr_memory = Projected_Region_To_Memory(ipan_callee, pr, 
    mem_pool);
  PROJECTED_REGION* pr_new = CXX_NEW(PROJECTED_REGION(MESSY_REGION, 0, 
    pr->Get_num_dims(), mem_pool), mem_pool);
  Map_callee_region_to_caller(ipan_caller, ipan_callee, sk, pr_new, pr_memory);
  REGION_ARRAYS* ra_new  
    = CXX_NEW(REGION_ARRAYS(mem_pool, symbol_index), mem_pool);
  PROJECTED_REGION_INFO_ARRAY* pria_new = ra_new->Get_projected_region_array();
  INT idx = pria_new->Newidx();
  PROJECTED_REGION_INFO* pri_new = &((*pria_new)[idx]);
  pri_new->Set_projected_region(pr_new);
  ra_new->Set_count(1);
  ra_new->Set_element_size(ra->Get_element_size());
  return ra_new; 
}

//-----------------------------------------------------------------------
// NAME: Is_Scalar
// FUNCTION: Return TRUE if 'ss' stored on the file with the given 
//   'file_index' is a scalar.  Return FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Is_Scalar(IPA_NODE* node,
		      SUMMARY_SYMBOL* ss)
{
  if (ss->Is_formal()) {
    SUMMARY_FORMAL* formal_array = IPA_get_formal_array(node);
    SUMMARY_FORMAL* sf = &formal_array[ss->Get_findex()];
    return sf->Get_region_index() == -1;
  } else if (ST_IDX_level(ss->St_idx()) == GLOBAL_SYMTAB) {
    return TY_kind(ST_type(ss->St_idx())) != KIND_ARRAY;
  } else {
    return !ss->Is_array();
  }
}

//-----------------------------------------------------------------------
// NAME: Formal_Region_Arrays
// FUNCTION: Return the REGION_ARRAYS associated with the declaration of 
//   'ss' which must be a formal argument to some subprogram on the file
//   with the given 'file_index'. 
//-----------------------------------------------------------------------
static REGION_ARRAYS* 
Formal_Region_Arrays(IPA_NODE* node, 
		     SUMMARY_SYMBOL* ss)
{
    SUMMARY_FORMAL* formal_array = IPA_get_formal_array(node);
    SUMMARY_FORMAL* sf = &formal_array[ss->Get_findex()];
    REGION_ARRAYS* region_array = IPA_get_region_array(node);
    REGION_ARRAYS* ra = NULL;
    if (sf->Get_region_index() != -1)
	ra = &region_array[sf->Get_region_index()];
    return ra; 
} 

//-----------------------------------------------------------------------
// DESCR:  Formal_Projected_Region
//         return the projected region representing the declaration of
//         an array foraml
//        !! make this obtainable from the summary procedure node!!
//-----------------------------------------------------------------------
static PROJECTED_REGION *
Formal_Projected_Region(IPA_NODE* node, SUMMARY_SYMBOL *s)
{
  REGION_ARRAYS *r = Formal_Region_Arrays(node, s);
  return Formal_Projected_Region(r);
}

//-----------------------------------------------------------------------
// NAME: Dim_Count
// FUNCTION: Return the number of dimensions of the symbol 'ss' stored on
//   the file with the given 'file_index'.  'ss' must be either a formal 
//   or common array. 
//-----------------------------------------------------------------------

static INT Dim_Count(IPA_NODE* node,
		     SUMMARY_SYMBOL* ss)
{
  if (ss->Is_formal()) {
    SUMMARY_FORMAL* formal_array = IPA_get_formal_array(node);
    SUMMARY_FORMAL* sf = &formal_array[ss->Get_findex()];
    REGION_ARRAYS* region_array = IPA_get_region_array(node);
    REGION_ARRAYS* ra = &region_array[sf->Get_region_index()];
    return ra->Get_count();
  } 
  else if (ST_IDX_level(ss->St_idx()) == GLOBAL_SYMTAB) {
    return TY_AR_ndims(ST_type(ss->St_idx()));
  } 
  else {
    return -1;
  }
}

//-----------------------------------------------------------------------
// NAME: Dim_Size
// FUNCTION: For the global array 'ss' stored on the file with the given
//   'file_index', return the number of elements in the given 'dim'. 
//-----------------------------------------------------------------------

static INT Dim_Size(SUMMARY_SYMBOL* ss,
                    INT dim)
{ 
  TY_IDX ty_idx = ST_type(ss->St_idx());
  return TY_AR_ubnd_val(ty_idx,dim) - TY_AR_lbnd_val(ty_idx,dim) + 1;
}

//-----------------------------------------------------------------------
// NAME: Formal_Dim_Count
// FUNCTION: For the 'ra' representing the declaration of a formal array 
//   return the number of dimensions of that array.
//-----------------------------------------------------------------------

static INT Formal_Dim_Count(REGION_ARRAYS* ra)
{ 
  PROJECTED_REGION* pr = Formal_Projected_Region(ra);
  return pr->Get_num_dims();
} 

//-----------------------------------------------------------------------
// NAME: Are_Equal_Dims
// FUNCTION: Return TRUE if 'ra_caller' and 'ra_callee' are equal in the 
//   'position'-th dimension.  Return FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Are_Equal_Dims(REGION_ARRAYS* ra_caller, 
			   REGION_ARRAYS* ra_callee, 
			   INT position)
{ 
  // Don't forget to deal with the assumed size dimension
  PROJECTED_REGION* pr_caller = Formal_Projected_Region(ra_caller);
  PROJECTED_NODE* pn_caller = pr_caller->Get_projected_node(position);
  // Don't bother with these cases right now.
  if (pn_caller->Is_messy_lb() || pn_caller->Is_messy_ub()
      || pn_caller->Is_messy_step())
    return FALSE; 
  PROJECTED_REGION* pr_callee = Formal_Projected_Region(ra_callee);  
  PROJECTED_NODE* pn_callee = pr_callee->Get_projected_node(position);
  // Don't bother with these cases right now.
  if (pn_callee->Is_messy_lb() || pn_callee->Is_messy_ub()
      || pn_callee->Is_messy_step())
    return FALSE; 
  LINEX* lx_lb_caller = pn_caller->Get_lower_linex();
  LINEX* lx_lb_callee = pn_callee->Get_lower_linex();
  if (!lx_lb_caller->Equivalent(*lx_lb_callee))
    return FALSE; 
  LINEX* lx_ub_caller = pn_caller->Get_upper_linex();
  LINEX* lx_ub_callee = pn_callee->Get_upper_linex();
  if (!lx_ub_caller->Equivalent(*lx_ub_callee))
    return FALSE; 
  LINEX* lx_step_caller = pn_caller->Get_step_linex();
  LINEX* lx_step_callee = pn_callee->Get_step_linex();
  if (!lx_step_caller->Equivalent(*lx_step_callee))
    return FALSE; 
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: Constant_Linex_Value
// FUNCTION: Return TRUE if the LINEX 'lx' has a constant value.  Return 
//   FALSE otherwise.  If we return TRUE, '*value' will be set to the 
//   constant value of the linex.  
//-----------------------------------------------------------------------

static BOOL Constant_Linex_Value(LINEX* lx, 
				 INT64* value)
{ 
  INT64 local_value = 0; 
  for (INT i = 0; i <= lx->Num_terms(); i++) {
    TERM* tm = lx->Get_term(i);
    if (tm->Get_type() != LTKIND_CONST)
      return FALSE; 
    local_value += tm->Get_coeff();
  } 
  *value = local_value;
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: Are_Equal_Dims
// FUNCTION: Return TRUE if the symbol 'ss_caller' (which is stored on the
//   file with the given 'file_index') has a 'position'-th dimension which 
//   of the same value as the 'position'-th dimension in 'ra_callee' (which 
//   is stored on the file with file index 'fi_callee'.  Return FALSE 
//   otherwise.  
//-----------------------------------------------------------------------

static BOOL Are_Equal_Dims(SUMMARY_SYMBOL* ss_caller, 
			   REGION_ARRAYS* ra_callee, 
			   INT position)
{
  Is_True(ST_IDX_level(ss_caller->St_idx()) == GLOBAL_SYMTAB, 
            ("Are_Equal_Dims: Expected a global symbol"));
  PROJECTED_REGION* pr_callee = Formal_Projected_Region(ra_callee);  
  PROJECTED_NODE* pn_callee = pr_callee->Get_projected_node(position);
  // Don't bother with these cases right now.
  if (pn_callee->Is_messy_lb() || pn_callee->Is_messy_ub()
      || pn_callee->Is_messy_step())
    return FALSE; 
  INT64 caller_dim_size = Dim_Size(ss_caller, position);
  LINEX* lx_lb_callee = pn_callee->Get_lower_linex();
  INT64 lx_lower_value = 0;
  if (!Constant_Linex_Value(lx_lb_callee, &lx_lower_value))
    return FALSE; 
  LINEX* lx_ub_callee = pn_callee->Get_upper_linex();
  INT64 lx_upper_value = 0;
  if (!Constant_Linex_Value(lx_ub_callee, &lx_upper_value))
    return FALSE; 
  INT64 callee_dim_size = lx_upper_value - lx_lower_value + 1;
  if (caller_dim_size != callee_dim_size)
    return FALSE; 
  return TRUE; 
} 


//-----------------------------------------------------------------------
// check for mismatched type
//----------------------------------------------------------------------- 
extern BOOL 
Mismatched_Types(IPA_NODE* ipan_caller, 
		 IPA_NODE* ipan_callee,
		 SUMMARY_CALLSITE *sk,
		 INT position,
		 MEM_POOL *mem_pool)
{
  FmtAssert(position >= 0 && position < sk->Get_param_count(),
    ("Translate_Through_Call: 'position' out of range"));
  SUMMARY_ACTUAL* actual_array = IPA_get_actual_array(ipan_caller);
  INT actual_index = sk->Get_actual_index() + position;
  SUMMARY_ACTUAL* sa = &actual_array[actual_index];
  SUMMARY_SYMBOL* cr_symbol_array = IPA_get_symbol_array(ipan_caller);
  switch (sa->Get_pass_type()) { 
  // The cases PASS_LOAD and PASS_MLOAD are C only, I'll handle them later  
  case PASS_LOAD:
  case PASS_MLOAD: 
    if (Get_Trace(TP_IPA, IPA_TRACE_SECTION_CORRECTNESS)) 
      fprintf(stdout, "SHAPE: Odd Pass Type in %s calling %s ARG %d\n",
        ipan_caller->Name(), ipan_callee->Name(), position);  
    return TRUE; 
  case PASS_LDID: 
  case PASS_LDA: {
    SUMMARY_PROCEDURE* sp = ipan_callee->Summary_Proc();
    INT formal_base_index = sp->Get_formal_index();
    INT formal_count = sp->Get_formal_count(); 
    if (!(position >= 0 && position < formal_count))
      return TRUE; 
    SUMMARY_FORMAL* formal_array = IPA_get_formal_array(ipan_callee);
    SUMMARY_FORMAL* sf = &formal_array[formal_base_index + position];
    INT symbol_index = sf->Get_symbol_index();
    SUMMARY_SYMBOL* symbol_array = IPA_get_symbol_array(ipan_callee);
    SUMMARY_SYMBOL* ss = &symbol_array[symbol_index];
    if (!ss->Is_dmod() && !ss->Is_imod() && !ss->Is_ref())
      return FALSE; 
    } 
    break; 
    
    // this is the case of sections
  case PASS_ARRAY_SECTION:
    break;

  case PASS_UNKNOWN:
    break;

  default:
    FmtAssert(TRUE, ("Try_Reshape: Unexpected pass type"));
    break;
  }   
  INT caller_symbol_index = sa->Get_symbol_index(); 
  if (caller_symbol_index == -1) {
    // Constant passed by value
    if (sa->Get_value_index() != -1)
      return FALSE; 
    if (Get_Trace(TP_IPA, IPA_TRACE_SECTION_CORRECTNESS))
      fprintf(stdout, 
	"SHAPE: Unknown Actual Symbol Index in %s calling %s ARG %d\n",
        ipan_caller->Name(), ipan_callee->Name(), position);
    return TRUE; 
  }
  SUMMARY_PROCEDURE* sp_callee = ipan_callee->Summary_Proc();
  INT formal_index = sp_callee->Get_formal_index();
  SUMMARY_FORMAL* ce_formal_array = IPA_get_formal_array(ipan_callee);
  SUMMARY_FORMAL* sf = &ce_formal_array[formal_index + position]; 
  INT callee_symbol_index = sf->Get_symbol_index();
  SUMMARY_SYMBOL* ce_symbol_array = IPA_get_symbol_array(ipan_callee);
  SUMMARY_SYMBOL* ss_caller = &cr_symbol_array[caller_symbol_index];
  SUMMARY_SYMBOL* ss_callee = &ce_symbol_array[callee_symbol_index];  

  // callee formal is not an array
  if (sf->Get_region_index() == -1) { 
    if (Is_Scalar(ipan_caller, ss_caller)) 
      return FALSE; 
  }
  if (Is_Scalar(ipan_caller, ss_caller)) {
    if (Get_Trace(TP_IPA, IPA_TRACE_SECTION_CORRECTNESS))
      fprintf(stdout, 
	"SHAPE: Scalar Passed to Array in %s calling %s ARG %d\n",
	ipan_caller->Name(), ipan_callee->Name(), position);
    return TRUE;
  }  
 
  REGION_ARRAYS* ra_callee  
    = Translate_Array(ipan_caller, ipan_callee, sk, position, mem_pool);
  if (ra_callee == NULL) { 
    if (Get_Trace(TP_IPA, IPA_TRACE_SECTION_CORRECTNESS))
      fprintf(stdout,
	"SHAPE: Could not Translate_Array in %s calling %s ARG %d\n",
	ipan_caller->Name(), ipan_callee->Name(), position);
    return TRUE; 
  } 
  if (ss_caller->Is_formal()) {
    REGION_ARRAYS* ra_caller = Formal_Region_Arrays(ipan_caller, ss_caller);
    ra_caller = Region_To_Memory(ipan_caller, ra_caller, mem_pool);
    if (ra_caller->Get_element_size() != ra_callee->Get_element_size()) { 
      if (Get_Trace(TP_IPA, IPA_TRACE_SECTION_CORRECTNESS))
        fprintf(stdout, 
	  "SHAPE: Mismatched base sizes in %s calling %s ARG %d\n",
          ipan_caller->Name(), ipan_callee->Name(), position);
      return TRUE;
    } 
  }
  else if (ST_IDX_level(ss_caller->St_idx()) == GLOBAL_SYMTAB) {
    if (TY_size(TY_etype(ST_type(ss_caller->St_idx())))
        != ra_callee->Get_element_size()) { 
      if (Get_Trace(TP_IPA, IPA_TRACE_SECTION_CORRECTNESS))
	fprintf(stdout, 
                "Reshape: Mismatched base sizes in %s calling %s ARG %d\n",
                ipan_caller->Name(), ipan_callee->Name(), position);
      return TRUE;
    } 
  }
  return FALSE; 
}

//-----------------------------------------------------------------------
// NAME: Try_Reshape_Callee_Formal
// FUNCTION: Consider the callsite 'sk' which represents a call from 
//   'ipan_caller' to 'ipan_callee'.  Return TRUE if the 'position'-th
//   argument in this call has a problem which MAY be resolved by 
//   reshaping.  Return FALSE otherwise. 
//-----------------------------------------------------------------------

extern BOOL Try_Reshape_Callee_Formal(IPA_NODE* ipan_caller, 
				      IPA_NODE* ipan_callee,
				      SUMMARY_CALLSITE* sk, 
				      INT position,
				      MEM_POOL* mem_pool)
{
  FmtAssert(position >= 0 && position < sk->Get_param_count(),
    ("Translate_Through_Call: 'position' out of range"));
  INT actual_index = sk->Get_actual_index() + position;
  SUMMARY_ACTUAL* sa = &IPA_get_actual_array(ipan_caller)[actual_index];
  SUMMARY_SYMBOL* cr_symbol_array = IPA_get_symbol_array(ipan_caller);
  switch (sa->Get_pass_type()) { 
  case PASS_UNKNOWN:
  // The cases PASS_LOAD and PASS_MLOAD are C only, I'll handle them later  
  case PASS_LOAD:
  case PASS_MLOAD: 
    if (Get_Trace(TP_IPA, IPA_TRACE_SECTION_CORRECTNESS))
      fprintf(stdout, "SHAPE: Odd Pass Type in %s calling %s ARG %d\n",
        ipan_caller->Name(), ipan_callee->Name(), position);  
    return TRUE; 
  case PASS_ARRAY_SECTION: 
    if (Get_Trace(TP_IPA, IPA_TRACE_SECTION_CORRECTNESS))
      fprintf(stdout, "SHAPE: Array Section in %s calling %s ARG %d\n", 
        ipan_caller->Name(), ipan_callee->Name(), position);
    return TRUE; 
  case PASS_LDID: 
  case PASS_LDA: {
    SUMMARY_PROCEDURE* sp = ipan_callee->Summary_Proc();
    INT formal_base_index = sp->Get_formal_index();
    INT formal_count = sp->Get_formal_count(); 
    if (!(position >= 0 && position < formal_count))
      return TRUE; 
    SUMMARY_FORMAL* formal_array = IPA_get_formal_array(ipan_callee);
    SUMMARY_FORMAL* sf = &formal_array[formal_base_index + position];
    INT symbol_index = sf->Get_symbol_index();
    SUMMARY_SYMBOL* symbol_array = IPA_get_symbol_array(ipan_callee);
    SUMMARY_SYMBOL* ss = &symbol_array[symbol_index];
    if (!ss->Is_dmod() && !ss->Is_imod())
      return FALSE; 
    } 
    break; 
  default:
    FmtAssert(TRUE, ("Try_Reshape: Unexpected pass type"));
    break;
  }   
  INT caller_symbol_index = sa->Get_symbol_index(); 
  if (caller_symbol_index == -1) {
    // Constant passed by value
    if (sa->Get_value_index() != -1)
      return FALSE; 
    if (Get_Trace(TP_IPA, IPA_TRACE_SECTION_CORRECTNESS))
      fprintf(stdout, 
	"SHAPE: Unknown Actual Symbol Index in %s calling %s ARG %d\n",
        ipan_caller->Name(), ipan_callee->Name(), position);
    return TRUE; 
  }
  SUMMARY_PROCEDURE* sp_callee = ipan_callee->Summary_Proc();
  INT formal_index = sp_callee->Get_formal_index();
  SUMMARY_FORMAL* ce_formal_array = IPA_get_formal_array(ipan_callee);
  SUMMARY_FORMAL* sf = &ce_formal_array[formal_index + position]; 
  INT callee_symbol_index = sf->Get_symbol_index();
  SUMMARY_SYMBOL* ce_symbol_array = IPA_get_symbol_array(ipan_callee);
  SUMMARY_SYMBOL* ss_caller = &cr_symbol_array[caller_symbol_index];
  SUMMARY_SYMBOL* ss_callee = &ce_symbol_array[callee_symbol_index];  
  if (sf->Get_region_index() == -1) { 
    if (Is_Scalar(ipan_caller, ss_caller)) {
      return FALSE; 
  }
  if (Get_Trace(TP_IPA, IPA_TRACE_SECTION_CORRECTNESS))
    fprintf(stdout, 
	"SHAPE: Array Passed to Scalar in %s calling %s ARG %d\n",
        ipan_caller->Name(), ipan_callee->Name(), position);
    return TRUE; 
  }
  if (Is_Scalar(ipan_caller, ss_caller)) {
    if (Get_Trace(TP_IPA, IPA_TRACE_SECTION_CORRECTNESS))
      fprintf(stdout, 
	"SHAPE: Scalar Passed to Array in %s calling %s ARG %d\n",
        ipan_caller->Name(), ipan_callee->Name(), position);
    return TRUE;
  }
  REGION_ARRAYS* ra_callee  
    = Translate_Array(ipan_caller, ipan_callee, sk, position, mem_pool);
  if (ra_callee == NULL) { 
    if (Get_Trace(TP_IPA, IPA_TRACE_SECTION_CORRECTNESS))
      fprintf(stdout,
	"SHAPE: Could not Translate_Array in %s calling %s ARG %d\n",
	ipan_caller->Name(), ipan_callee->Name(), position);
    return TRUE; 
  } 
  if (ss_caller->Is_formal()) {
    REGION_ARRAYS* ra_caller = Formal_Region_Arrays(ipan_caller, ss_caller);
    ra_caller = Region_To_Memory(ipan_caller, ra_caller, mem_pool);
    if (ra_caller->Get_element_size() != ra_callee->Get_element_size()) { 
      if (Get_Trace(TP_IPA, IPA_TRACE_SECTION_CORRECTNESS))
        fprintf(stdout, 
	  "SHAPE: Mismatched base sizes in %s calling %s ARG %d\n",
          ipan_caller->Name(), ipan_callee->Name(), position);
      return TRUE;
    } 
    if (Formal_Dim_Count(ra_caller) != Formal_Dim_Count(ra_callee)) {
      if (Get_Trace(TP_IPA, IPA_TRACE_SECTION_CORRECTNESS))
        fprintf(stdout, 
	  "TRY RESHAPE: Mismatched Array Dims in %s calling %s ARG %d\n",
	  ipan_caller->Name(), ipan_callee->Name(), position);
      return TRUE; 
    } 
    INT dim_count = Formal_Dim_Count(ra_caller);
    for (INT i = 1; i < dim_count; i++) { 
      if (!Are_Equal_Dims(ra_caller, ra_callee, i)) {
        if (Get_Trace(TP_IPA, IPA_TRACE_SECTION_CORRECTNESS))
	  fprintf(stdout, 
	    "SHAPE: Mismatched Array Subs in %s calling %s ARG %d\n",
	    ipan_caller->Name(), ipan_callee->Name(), position);
	return TRUE; 
      }
    }
  } 
  else if (ST_IDX_level(ss_caller->St_idx()) == GLOBAL_SYMTAB) {
    if (TY_size(TY_etype(ST_type(ss_caller->St_idx())))
	!= ra_callee->Get_element_size()) { 
      if (Get_Trace(TP_IPA, IPA_TRACE_SECTION_CORRECTNESS))
        fprintf(stdout, 
	  "SHAPE: Mismatched base sizes in %s calling %s ARG %d\n",
          ipan_caller->Name(), ipan_callee->Name(), position);
      return TRUE;
    } 
    if (Dim_Count(ipan_caller, ss_caller) != Formal_Dim_Count(ra_callee)) {
      if (Get_Trace(TP_IPA, IPA_TRACE_SECTION_CORRECTNESS))
        fprintf(stdout, 
	  "TRY RESHAPE: Mismatched Array Dims in %s calling %s ARG %d\n",
	  ipan_caller->Name(), ipan_callee->Name(), position);
      return TRUE;
    }
    INT dim_count = Dim_Count(ipan_caller, ss_caller);
    for (INT i = 1; i < dim_count; i++) {
      if (!Are_Equal_Dims(ss_caller, ra_callee, i)) {
        if (Get_Trace(TP_IPA, IPA_TRACE_SECTION_CORRECTNESS))
	  fprintf(stdout, 
	    "TRY RESHAPE: Mismatched Array Subs in %s calling %s ARG %d\n",
	    ipan_caller->Name(), ipan_callee->Name(), position);
	return TRUE;
      }
    }
  } 
  return FALSE; 
}
