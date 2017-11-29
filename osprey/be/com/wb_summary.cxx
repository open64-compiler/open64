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


#include <stdint.h>
#include <sys/types.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <ctype.h>
#include "wn.h"
#include "wn_map.h"
#include "wn_util.h"
#include <stdio.h>
#include "dwarf_DST.h"
#include "ipc_file.h"
#include "ipc_symtab_merge.h"
#include "ipa_section.h" 

//-----------------------------------------------------------------------
// NAME: SUMMARY_SYMBOL::WB_Print
// FUNCTION: Print SUMMARY_SYMBOL in whirl browser format
//-----------------------------------------------------------------------

void SUMMARY_SYMBOL::WB_Print(FILE* fp,
			      INT symbol_index, 
			      BOOL is_list,
			      const char* name, 
			      const char* func_name,
			      INT fancy_level)
{ 
  if (func_name == NULL || func_name[0] == '\0') {
    fprintf(fp, "SYMBOL[%d]: \"%s\" ", symbol_index, name);
  } else {
    fprintf(fp, "SYMBOL[%d]: \"%s\":\"%s\" ", symbol_index, name, func_name);
  }
  if (fancy_level >= 2)  
    fprintf(fp, "ST_IDX(%d) ", St_idx());
  fprintf(fp, "%sLOCAL %sSTATIC %sPASSED %sARRAY %s", Is_local() ? "" :
    "non-", Is_static() ? "" : "non-", Is_parm() ? "" : "non-",
    Is_array() ? "" : "non-", Is_common() ? "COMMON " : "");
  if (Is_common_block())
    fprintf(fp, "COMMON-BLOCK ");
  if (Is_function())
    fprintf(fp, "FUNCTION ");
  if (Get_btype() == MTYPE_UNKNOWN)
    fprintf(fp, "BTYPE(UNKNOWN) ");
  else
    fprintf(fp, "BTYPE(%s) ", MTYPE_name(Get_btype()));
  if (Is_formal())
    fprintf(fp, "FORMAL[%d]", Get_findex());
  fprintf(fp, "\n");
  if (!is_list || fancy_level >= 2) {
    fprintf(fp, "           ");
    fprintf(fp, "ADDRESS: ");
    if (Is_addr_saved())
      fprintf(fp, "ADDR_SAVED ");
    if (Is_addr_f90_target())
      fprintf(fp, "ADDR_F90_TARGET ");
    if (Is_addr_passed())
      fprintf(fp, "ADDR_PASSED ");
    if (Is_addr_passed_inliner())
      fprintf(fp, "ADDR_PASSED_INLINE ");
    fprintf(fp, "\n");
    fprintf(fp, "           ");
    fprintf(fp, "MOD/REF: ");
    if (Is_imod())
      fprintf(fp, "IMOD ");
    if (Is_dmod())
      fprintf(fp, "DMOD ");
    if (Is_iref())
      fprintf(fp, "IREF ");
    if (Is_aref())
      fprintf(fp, "AREF ");
    if (Is_dref())
      fprintf(fp, "DREF ");
    if (Is_cref())
      fprintf(fp, "CREF ");
    if (Is_cdref_preg_only())
      fprintf(fp, "CREF_PREG_ONLY ");
    if (Is_ikill())
      fprintf(fp, "IKILL ");
    if (Is_dkill())
      fprintf(fp, "DKILL ");
    if (Is_cmod())
      fprintf(fp, "CMOD ");
    if (Is_modcount())
      fprintf(fp, "MODCOUNT ");
    if (Is_parm())
      fprintf(fp, "PARM ");
    if (Is_ref())
      fprintf(fp, "REF ");
    if (Is_modref())
      fprintf(fp, "MODREF_ANY ");
    fprintf(fp, "\n");
  }
} 

//-----------------------------------------------------------------------
// NAME: IVAR::WB_Print
// FUNCTION: Print IVAR in whirl browser format 
//-----------------------------------------------------------------------

void IVAR::WB_Print(FILE* fp,
                    INT ivar_index)
{
  fprintf(fp, "IVAR[%d]: ", ivar_index);
  if (Is_Formal()) {
    fprintf(fp, "FORMAL_POSITION(%d) OFFSET(%d) MTYPE(%s)\n",
      Formal_Position(), Offset(), Machine_Types[Mtype()].name);
  } else {
    fprintf(fp, "GLOBAL(%s) OFFSET(%d) MTYPE(%s)\n",
      ST_name(St_Idx()), Offset(), Machine_Types[Mtype()].name);
  }
}

//-----------------------------------------------------------------------
// NAME: SUMMARY_FORMAL::WB_Print
// FUNCTION: Print SUMMARY_FORMAL in whirl browser format 
//-----------------------------------------------------------------------

void SUMMARY_FORMAL::WB_Print(FILE* fp, 
			      INT formal_index, 
			      const char* name,
                              const char* func_name)
{
  if (func_name == NULL || func_name[0] == '\0') {
    fprintf(fp, "FORMAL[%d]: SYMBOL[%d] REGION[%d] \"%s\" POSITION(%d) ",
      formal_index, Get_symbol_index(), Get_region_index(),
        name, Get_position());
    fprintf(fp, "MTYPE(%s) ", MTYPE_name(Get_machine_type()));
  } else {
    fprintf(fp, "FORMAL[%d]: SYMBOL[%d] REGION[%d] \"%s\":\"%s\" POSITION(%d) ",      formal_index, Get_symbol_index(), Get_region_index(),
        name, func_name, Get_position());
    fprintf(fp, "MTYPE(%s) ", MTYPE_name(Get_machine_type()));
  }
  if (Is_ref_parm())
    fprintf(fp, "REF ");
  if (Is_var_dim_array())
    fprintf(fp, "VAR_DIM_ARRAY ");  
  fprintf(fp, "\n");
}

//-----------------------------------------------------------------------
// NAME: SUMMARY_COMMON::WB_Print
// FUNCTION: Print SUMMARY_COMMON in whirl browser format 
//-----------------------------------------------------------------------

void SUMMARY_COMMON::WB_Print(FILE* fp, 
			      INT common_index) 
{
  fprintf(fp, "COMMON[%d]: SYMBOL[%d] COMMON_SHAPE[%d:%d]",
    common_index, Get_symbol_index(), Get_common_shape_index(),
    Get_common_shape_count());
  fprintf(fp, "\n");
} 

//-----------------------------------------------------------------------
// NAME: SUMMARY_COMMON_SHAPE::WB_Print
// FUNCTION: Print SUMMARY_COMMON_SHAPE in whirl browser format 
//-----------------------------------------------------------------------

void SUMMARY_COMMON_SHAPE::WB_Print(FILE* fp,
                                    INT common_shape_index)
{
  fprintf(fp, "COMMON_SHAPE[%d]: SYMBOL[%d] ",
    common_shape_index, Get_symbol_index());
  if (Is_kind_scalar()) {
    fprintf(fp, "Scalar ELEMENT_SIZE(%d) OFFSET(%lld) ",
      Get_element_size(), Get_offset());
  } else if (Is_kind_array()) {
    fprintf(fp, "Array  ");
    if (Is_symbolic_bounds())
      fprintf(fp, "<SYMBOLIC_BOUNDS> ");
    else
      fprintf(fp, "ELEMENT_SIZE(%d) DIM_COUNT(%d) [%d:%d:%d] ",
        Get_element_size(), Get_dim_count(), Get_lower(),
        Get_upper(), Get_stride());
  }
  fprintf(fp, "\n");
}

//-----------------------------------------------------------------------
// NAME: SUMMARY_PROCEDURE::WB_Print
// FUNCTION: Print SUMMARY_PROCEDURE in whirl browser format 
//-----------------------------------------------------------------------

void SUMMARY_PROCEDURE::WB_Print(FILE* fp,
                                 INT procedure_index,
				 const char* name,
				 INT fancy_level)
{
  INT symbol_index = Get_symbol_index();
  fprintf(fp, "PROCEDURE[%d]: SYMBOL[%d] \"%s\" ", procedure_index,
    symbol_index, name);
  if (Get_formal_count() > 0)
    fprintf(fp, "FORMAL[%d:%d] ", Get_formal_index(),
      Get_formal_count());
  if (Get_global_count() > 0)
    fprintf(fp, "GLOBAL[%d:%d] ", Get_global_index(),
      Get_global_count());
  if (Get_callsite_count() > 0)
    fprintf(fp, "CALLSITE[%d:%d] ", Get_callsite_index(),
      Get_callsite_count());
  if (Get_ctrl_dep_count() > 0)
    fprintf(fp, "CTRL_DEP[%d:%d] ", Get_ctrl_dep_index(),
      Get_ctrl_dep_count());
  if (Get_array_section_count() > 0)
    fprintf(fp, "CFG_NODE[%d:%d] ", Get_array_section_index(),
      Get_array_section_count());
  fprintf(fp, "\n  ");
  fprintf(fp, "X(VALUE[%d:%d]) X(EXPR[%d:%d]) ", Get_ex_value_index(), 
    Get_ex_value_count(), Get_ex_expr_index(), Get_ex_expr_count());
  if (fancy_level >= 2) { 
    if (Is_may_inline()) 
      fprintf(fp, "MAY_INLINE ");
    if (Is_must_inline())
      fprintf(fp, "MUST_INLINE ");
    if (Is_no_inline())
      fprintf(fp, "NO_INLINE "); 
    if (Is_varargs())
      fprintf(fp, "VARARGS ");
    if (Is_alt_entry())
      fprintf(fp, "IS_ALT_ENTRY ");
    if (Has_alt_entry())
      fprintf(fp, "HAS_ALT_ENTRY ");
    if (Has_pstatic())
      fprintf(fp, "PSTATIC ");
    if (Is_no_delete())
      fprintf(fp, "NO_DELETE ");
    if (Is_block_data())
      fprintf(fp, "BLOCK_DATA ");
    if (Is_direct_mod_ref())
      fprintf(fp, "DIRECT_MOD_REF ");
    if (Is_exc_inline())
      fprintf(fp, "EXC_INLINE ");
    if (Has_addr_taken_reset())
      fprintf(fp, "ADDR_TAKEN_RESET ");
    if (Has_PU_freq())
      fprintf(fp, "PU_FREQ ");
    if (Has_formal_pragma())
      fprintf(fp, "FORMAL_PRAGMA ");
    if (Has_parallel_pragma())
      fprintf(fp, "PARALLEL_PRAGMA ");
    if (Has_parallel_region_pragma())
      fprintf(fp, "PARALLEL_REGION_PRAGMA ");
    if (Has_fstatic())
      fprintf(fp, "FSTATIC ");
    if (Use_lowered_return_preg())
      fprintf(fp, "LOWERED_RETURN_PREG ");
    if (Has_unknown_calls())
      fprintf(fp, "UNKNOWN_CALLS ");
    if (Has_incomplete_array_info())
      fprintf(fp, "INCOMPLETE_ARRAY_INFO ");
    if (Has_mp_needs_lno())
      fprintf(fp, "MP_NEEDS_LNO ");
    if (Has_exc_try())
      fprintf(fp, "EXC_TRY ");
    if (Has_pragma_side_effect())
      fprintf(fp, "PRAGMA_SIDE_EFFECT ");
    if (Has_messy_regions())
      fprintf(fp, "MESSY_REGIONS ");
    if (Has_early_returns())
      fprintf(fp, "EARLY_RETURNS ");
  } 
  fprintf(fp, "\n");
}

//-----------------------------------------------------------------------
// NAME: SUMMARY_GLOBAL::WB_Print
// FUNCTION: Print SUMMARY_GLOBAL in whirl browser format 
//-----------------------------------------------------------------------

void SUMMARY_GLOBAL::WB_Print(FILE* fp,
                              INT global_index)
{
  fprintf(fp, "GLOBAL[%d]: SYMBOL[%d] MODS(%d) REFS(%d) ",
    global_index,  Get_symbol_index(), Get_modcount(),
    Get_refcount());
  if (Is_imod())
    fprintf(fp, "IMOD ");
  if (Is_dmod())
    fprintf(fp, "DMOD ");
  if (Is_iref())
    fprintf(fp, "IREF ");
  if (Is_dref())
    fprintf(fp, "DREF ");
  if (Is_ikill())
    fprintf(fp, "IKILL ");
  if (Is_dkill())
    fprintf(fp, "DKILL ");
  fprintf(fp, "\n");
} 

//-----------------------------------------------------------------------
// NAME: SUMMARY_CALLSITE::WB_Print
// FUNCTION: Print SUMMARY_CALLSITE in whirl browser format 
//-----------------------------------------------------------------------

void SUMMARY_CALLSITE::WB_Print(FILE* fp,
                                INT callsite_index,
			        const char* name, 
				const char* func_name)
{
  fprintf(fp, "CALLSITE[%d]: ACTUAL[%d:%d] MAP_ID(%d) ", callsite_index,
    Get_actual_index(), Get_param_count(), Get_map_id());
  if (Is_intrinsic())
    fprintf(fp, "<INTRINSIC>");
  else if (Is_func_ptr())
    fprintf(fp, "<FUNC_PTR> VALUE[%d]", Get_value_index());
  else {
    if (func_name == NULL || func_name[0] == '\0')
      fprintf(fp, "SYMBOL[%d] \"%s\" ", Get_symbol_index(), name);
    else
      fprintf(fp, "SYMBOL[%d] \"%s\":\"%s\" ", Get_symbol_index(),
        name, func_name);
  }
  if (Is_must_inline())
    fprintf(fp, "MUST_INLINE");
  if (Is_no_inline())
    fprintf(fp, "NO_INLINE");
  fprintf(fp, "\n");
}

//-----------------------------------------------------------------------
// NAME: SUMMARY_CONTROL_DEPENDENCE::WB_Print
// FUNCTION: Print SUMMARY_CONTROL_DEPENDENCE in whirl browser format 
//-----------------------------------------------------------------------

void SUMMARY_CONTROL_DEPENDENCE::WB_Print(FILE* fp,
                                          INT control_index)
{
  if (Is_entry()) {
    fprintf(fp, "CTRL_DEP[%d]: <ENTRY> STMT[%d:%d] ", control_index,
      Get_true_stmt_index(), Get_true_count());
  } else if (Is_if_stmt()) {
    fprintf(fp, "CTRL_DEP[%d]: <IF_STMT> MAP_ID(%d) ", control_index,
      Get_map_id());
    fprintf(fp, "<TRUE> STMTS[%d:%d] <FALSE> STMTS[%d:%d] ",
      Get_true_stmt_index(), Get_true_count(),
      Get_false_stmt_index(), Get_false_count());
  } else if (Is_do_loop()) {
    fprintf(fp, "CTRL_DEP[%d]: <DO_LOOP> MAP_ID(%d) STMTS[%d:%d] ",
      control_index, Get_map_id(), Get_true_stmt_index(),
      Get_true_count());
  }
  fprintf(fp, "\n");
}  

//-----------------------------------------------------------------------
// NAME: SUMMARY_ACTUAL::WB_Print
// FUNCTION: Print SUMMARY_ACTUAL in whirl browser format 
//-----------------------------------------------------------------------

void SUMMARY_ACTUAL::WB_Print(FILE* fp,
                              INT actual_index,
			      const char* name,
			      const char* func_name)
{
  fprintf(fp, "ACTUAL[%d]: ", actual_index);
  if (Pass_type_name() != NULL) {
    switch (Get_pass_type()) {
    case PASS_UNKNOWN:
      fprintf(fp, "PASS(UNKNOWN) ");
      break;
    case PASS_LDID:
      fprintf(fp, "PASS(LDID)    ");
      break;
    case PASS_LOAD:
      fprintf(fp, "PASS(LOAD)    ");
      break;
    case PASS_MLOAD:
      fprintf(fp, "PASS(MLOAD)   ");
      break;
    case PASS_LDA:
      fprintf(fp, "PASS(LDA)     ");
      break;
    case PASS_ARRAY_SECTION:
      fprintf(fp, "PASS(ARRAY)   ");
      break;
    }
  }
  if (Get_symbol_index() != -1) {
    if (func_name == NULL || func_name[0] == '\0')
      fprintf(fp, "SYMBOL[%d] \"%s\" ", Get_symbol_index(), name);
    else
      fprintf(fp, "SYMBOL[%d] \"%s\":\"%s\" ", Get_symbol_index(), name,
        func_name);
  }
  if (Get_value_index() != -1)
    fprintf(fp, "VALUE[%d] ", Get_value_index());
  if (Get_index() != -1)
    if (Get_pass_type())
      fprintf(fp, "REGION[%d] ", Get_index());
    else
      fprintf(fp, "SCALAR[%d] ", Get_index());
  fprintf(fp, "\n");
}

//-----------------------------------------------------------------------
// NAME: SUMMARY_VALUE::WB_Print
// FUNCTION: Print SUMMARY_VALUE in whirl browser format 
//-----------------------------------------------------------------------

void SUMMARY_VALUE::WB_Print(FILE* fp,
                             INT value_index)
{
  fprintf(fp, "VALUE[%d]: ", value_index);
  if (Get_mtype() != MTYPE_UNKNOWN)
    fprintf(fp, "%s ", MTYPE_name(Get_mtype()));
  if (Is_unknown())
    fprintf(fp, "UNKNOWN");
  else if (Is_int_const())
    fprintf(fp, "(%lld) ", Get_int_const_value());
  else if (Is_two_consts())
    fprintf(fp, "(%d,%d) ",
      Get_first_of_two_values(), Get_second_of_two_values());
  else if (Is_const_st())
      fprintf(fp, "CONST");
  else if (Is_formal())
    fprintf(fp, "FORMAL[%d] ", Get_formal_index());
  else if (Is_global()) { 
    if (Is_global_st_idx())
      fprintf(fp, "GLOBAL(ST_IDX(%d)) ", Get_global_st_idx());
    else 
      fprintf(fp, "GLOBAL(SYMBOL[%d]) ", Get_global_index());
  } else if (Is_symbol())
    fprintf(fp, "SYMBOL[%d] ", Get_symbol_index());
  else if (Is_expr())
    fprintf(fp, "EXPR[%d] ", Get_expr_index());
  else if (Is_phi())
    fprintf(fp, "PHI[%d] ", Get_phi_index());
  else if (Is_chi())
    fprintf(fp, "CHI[%d] ", Get_chi_index());
  else if (Is_callsite())
    fprintf(fp, "CALLSITE[%d] ", Get_callsite_index());
  else if (Is_not_const())
    fprintf(fp, "NOT_CONST");
  fprintf(fp, "\n");
} 

//-----------------------------------------------------------------------
// NAME: CFG_NODE_INFO::WB_Print
// FUNCTION: Print CFG_NODE_INFO in whirl browser format 
//-----------------------------------------------------------------------

void CFG_NODE_INFO::WB_Print(FILE* fp,
                             INT cfg_index)
{
  fprintf(fp, "CFG_NODE[%d]: ", cfg_index);
  if (Is_do_loop()) { 
    fprintf(fp, "DO_LOOP: INDEX(%d) ", Get_loop_index());
  } else if (Is_if()) {
    fprintf(fp, "IF: ELSE_INDEX(%d) ", Get_else_index());
  } else if (Is_else()) { 
    fprintf(fp, "IF: IF_INDEX(%d) ", Get_if_index());
  } else if (Is_entry()) { 
    fprintf(fp, "ENTRY: ");
  } else { 
    fprintf(fp, "<UNKNOWN>: ");
  }
  fprintf(fp, "CFG_INDEX(%d) ", Get_cd_index());
  if (Has_calls()) 
    fprintf(fp, "HAS_CALLS ");
  if (Get_def_count() > 0)
    fprintf(fp, "DEF REGION[%d:%d] ", Get_def_index(),
      Get_def_count());
  if (Get_use_count() > 0)
    fprintf(fp, "USE REGION[%d:%d] ", Get_use_index(),
      Get_use_count());
  if (Get_param_count() > 0)
    fprintf(fp, "PARAM REGION[%d:%d] ", Get_param_index(),
      Get_param_count());
  if (Get_formal_count() > 0)
    fprintf(fp, "FORMAL REGION[%d:%d] ", Get_formal_index(),
      Get_formal_count());
  if (Get_scalar_count() > 0)
    fprintf(fp, "SCALAR[%d:%d] ", Get_scalar_index(),
      Get_scalar_count());
  fprintf(fp, "\n");
} 

//-----------------------------------------------------------------------
// NAME: SCALAR_INFO::WB_Print
// FUNCTION: Print SCALAR_INFO in whirl browser format 
//-----------------------------------------------------------------------

void SCALAR_INFO::WB_Print(FILE* fp,
                           INT scalar_index,
			   const char* name, 
			   const char* func_name)
{
  if (func_name == NULL || func_name[0] == '\0')
    fprintf(fp, "SCALAR[%d]: SYMBOL[%d] \"%s\" ",
      scalar_index, Get_id(), name);
  else
    fprintf(fp, "SCALAR[%d]: SYMBOL[%d] \"%s\":\"%s\" ",
      scalar_index, Get_id(), name, func_name);
  if (Get_callsite_id() != -1)
    fprintf(fp, "CALLSITE[%d] ", Get_callsite_id());
  if (Is_may_kill())
    fprintf(fp, "MAY_KILL ");
  if (Is_may_use())
    fprintf(fp, "MAY_USE ");
  if (Is_may_reduc())
    fprintf(fp, "MAY_REDUC ");
  if (Is_kill())
    fprintf(fp, "KILL ");
  if (Is_use())
    fprintf(fp, "USE ");
  if (Is_euse())
    fprintf(fp, "EUSE ");
  if (Is_call_euse())
    fprintf(fp, "CALL_EUSE ");
  if (Is_reduc())
    fprintf(fp, "REDUC ");
  if (Is_array_reduc())
    fprintf(fp, "ARRAY_REDUC ");
  if (Is_array_may_reduc())
    fprintf(fp, "ARRAY_MAY_REDUC ");
  if (Is_passed_ref())
    fprintf(fp, "PASSED ");
  if (Is_may_passed_ref())
    fprintf(fp, "MAY_PASS ");
  fprintf(fp, "\n");
}

//-----------------------------------------------------------------------
// NAME: REGION_ARRAYS::WB_Print
// FUNCTION: Print REGION_ARRAYS in whirl browser format 
//-----------------------------------------------------------------------

void REGION_ARRAYS::WB_Print(FILE* fp,
                             INT region_index,
			     const char* name,
			     const char* func_name)
{ 
  if (func_name == NULL || func_name[0] == '\0')
    fprintf(fp, "REGION[%d]: SYMBOL[%d] \"%s\" ", region_index,
      Get_sym_id(), name);
  else
    fprintf(fp, "REGION[%d]: SYMBOL[%d] \"%s\":\"%s\" ", region_index,
      Get_sym_id(), name, func_name);
  fprintf(fp, "PROJ_REGION[%d:%d] ", Get_idx(), Get_count());
  if (Get_element_size() != 0)
    fprintf(fp, "ELEMENT_SIZE(%d) ", Get_element_size());
  if (Is_use())
    fprintf(fp, "USE ");
  else if (Is_def())
    fprintf(fp, "DEF ");
  else if (Is_passed())
    fprintf(fp, "PASSED ");
  else if (Is_may_def())
    fprintf(fp, "MAY_DEF ");
  else if (Is_may_use())
    fprintf(fp, "MAY_USE ");
  else if (Is_formal())
    fprintf(fp, "FORMAL ");
  if (Is_bad_alias())
    fprintf(fp, "<BAD_ALIAS>");
  if (Is_loop_invariant())
    fprintf(fp, "<LOOP_INVARIANT>");
  fprintf(fp, "\n");
}

//-----------------------------------------------------------------------
// NAME: PROJECTED_REGION::WB_Print
// FUNCTION: Print PROJECTED_REGION in whirl browser format 
//-----------------------------------------------------------------------

void PROJECTED_REGION::WB_Print(FILE* fp,
                                INT proj_region_index)
{
  fprintf(fp, "PROJ_REGION[%d]: ", proj_region_index);
  fprintf(fp, "PROJ_NODE[%d:%d] ", Get_id(),
    Get_num_dims());
  fprintf(fp, "DEPTH(%d) ", Get_depth());
  if (Is_messy_region())
    fprintf(fp, "<MESSY> ");
  if (Is_unprojected_region())
    fprintf(fp, "<UNPROJECTED> ");
  if (Is_may_kill()) {
    fprintf(fp, "MAY_KILL ");
  } else if (Is_may_use()) {
    fprintf(fp, "MAY_USE ");
  } else if (Is_passed()) {
    fprintf(fp, "                  ");
    fprintf(fp, "\n");
    fprintf(fp, "PASSED ");
    fprintf(fp, "CALLSITE[%d] ", Get_callsite_id());
    fprintf(fp, "ACTUAL_POSITION(%d) ",  Get_actual_id());
  } else if (Is_formal()) {
    fprintf(fp, "FORMAL ");
  }
  fprintf(fp, "\n");
} 
	
//-----------------------------------------------------------------------
// NAME: PROJECTED_NODE::WB_Print
// FUNCTION: Print PROJECTED_NODE in whirl browser format 
//-----------------------------------------------------------------------

void PROJECTED_NODE::WB_Print(FILE* fp,
                              INT proj_node_index)
{
  fprintf(fp, "PROJ_NODE[%d]: ", proj_node_index);
  if (Is_unprojected()) {
    fprintf(fp, "<UNPROJECTED> TERM[%d:%d] ",
      Get_lb_term_index(), Get_lb_term_count());
  } else {
    if (Is_messy_lb())
      fprintf(fp, "LB <MESSY> ");
    else
      fprintf(fp, "LB TERM[%d:%d] ", Get_lb_term_index(),
        Get_lb_term_count());
    if (Is_messy_ub())
      fprintf(fp, "UB <MESSY> ");
    else
      fprintf(fp, "UB TERM[%d:%d] ", Get_ub_term_index(),
        Get_ub_term_count());
    if (Is_messy_step())
      fprintf(fp, "STEP <MESSY> ");
    else
      fprintf(fp, "STEP TERM[%d:%d] ", Get_step_term_index(),
        Get_step_term_count());
    if (Get_segment_length_term_count() > 0)
      fprintf(fp, "SL TERM[%d:%d] ", Get_segment_length_term_index(),
        Get_segment_length_term_count());
    if (Get_segment_stride_term_count() > 0)
      fprintf(fp, "SS TERM[%d:%d] ", Get_segment_stride_term_index(),
        Get_segment_stride_term_count());
  }
  if (Is_assumed_shape())
    fprintf(fp, "<ASSUMED_SHAPE> ");
  fprintf(fp, "\n");
} 			

//-----------------------------------------------------------------------
// NAME: TERM::WB_Print
// FUNCTION: Print TERM in whirl browser format 
//-----------------------------------------------------------------------

void TERM::WB_Print(FILE* fp,
                    INT term_index)
{ 
  fprintf(fp, "TERM[%d]: ", term_index);
  switch (Get_type()) {
  case LTKIND_NONE:
    fprintf(fp, "NONE ");
    break;
  case LTKIND_CONST:
    fprintf(fp, "CONST(%d)", Get_coeff());
    break;
  case LTKIND_LINDEX:
    fprintf(fp, "LINDEX(%d) * (%d) ", Get_desc(), Get_coeff());
    break;
  case LTKIND_SUBSCR:
    fprintf(fp, "SUBSCR(%d) * (%d) ", Get_desc(), Get_coeff());
    break;
  case LTKIND_IV:
    fprintf(fp, "IVAR[%d] * (%d) ", Get_desc(), Get_coeff());
    break;
  default:
    fprintf(fp, " ");
    break;
  }
  fprintf(fp, "\n");
} 

//-----------------------------------------------------------------------
// NAME: LOOPINFO::WB_Print
// FUNCTION: Print LOOPINFO in whirl browser format 
//-----------------------------------------------------------------------

void LOOPINFO::WB_Print(FILE* fp,
                        INT loop_info_index)
{
  fprintf(fp, "LOOP[%d]: ", loop_info_index);
  if (Is_messy_lb())
    fprintf(fp, "LB <MESSY> ");
  else
    fprintf(fp, "LB TERM[%d:%d] ", Get_lb_term_index(),
      Get_lb_term_count());
  if (Is_messy_ub())
    fprintf(fp, "UB <MESSY> ");
  else
    fprintf(fp, "UB TERM[%d:%d] ", Get_ub_term_index(),
      Get_ub_term_count());
  if (Is_messy_step())
    fprintf(fp, "STEP <MESSY> ");
  else
    fprintf(fp, "STEP TERM[%d:%d] ", Get_step_term_index(),
      Get_step_term_count());
  fprintf(fp, "NEST_LEVEL(%d)\n", Get_nest_level());
} 

//-----------------------------------------------------------------------
// NAME: SUMMARY_PHI::WB_Print
// FUNCTION: Print SUMMARY_PHI in whirl browser format 
//-----------------------------------------------------------------------

void SUMMARY_PHI::WB_Print(FILE* fp,
                           INT phi_index)
{
  fprintf(fp, "PHI[%d]: ", phi_index);
  for (INT i = 0; i < 2; i++) {
    fprintf(fp, "(");
    fprintf(fp, "CTRL_DEP[%d] ", Get_ctrl_dep_index(i));
    fprintf(fp, "%s", Get_branch(i) ? "T " : "F ");
    if (Is_value(i))
      fprintf(fp, "VALUE[%d]", Get_node_index(i));
    if (Is_expr(i))
      fprintf(fp, "EXPR[%d]", Get_node_index(i));
    if (Is_phi(i))
      fprintf(fp, "PHI[%d]", Get_node_index(i));
    if (Is_chi(i))
      fprintf(fp, "CHI[%d]", Get_node_index(i));
    fprintf(fp, ") ");
  }
  fprintf(fp, "\n");
}

//-----------------------------------------------------------------------
// NAME: SUMMARY_CHI::WB_Print
// FUNCTION: Print SUMMARY_CHI in whirl browser format 
//-----------------------------------------------------------------------

void SUMMARY_CHI::WB_Print(FILE* fp,
                           INT chi_index,
			   const char* name,
			   const char* func_name)
{
  fprintf(fp, "CHI[%d]: CALLSITE[%d] ", chi_index, Get_call_index());
  if (Get_symbol_index() != -1) {
    if (func_name == NULL || func_name[0] == '\0')
      fprintf(fp, "SYMBOL[%d] \"%s\" ", Get_symbol_index(), name);
    else
      fprintf(fp, "SYMBOL[%d] \"%s\":\"%s\" ", Get_symbol_index(),
        name, func_name);
  }
  if (Is_chi_value())
    fprintf(fp, "VALUE[%d] ", Get_node_index());
  if (Is_chi_phi())
    fprintf(fp, "PHI[%d] ", Get_node_index());
  if (Is_chi_expr())
    fprintf(fp, "EXPR[%d] ", Get_node_index());
  if (Is_chi_chi())
    fprintf(fp, "CHI[%d] ", Get_node_index());
  fprintf(fp, "\n");
} 

//-----------------------------------------------------------------------
// NAME: SUMMARY_EXPR::Node
// FUNCTION: Print SUMMARY_EXPR for a single node in whirl browser format 
//-----------------------------------------------------------------------

void SUMMARY_EXPR::Node(FILE* fp,
                        INT kid)
{
  if (Is_expr_value(kid))
    fprintf(fp, "VALUE[%d] ", Get_node_index(kid));
  if (Is_expr_phi(kid))
    fprintf(fp, "PHI[%d] ", Get_node_index(kid));
  if (Is_expr_expr(kid))
    fprintf(fp, "EXPR[%d] ", Get_node_index(kid));
  if (Is_expr_chi(kid))
    fprintf(fp, "CHI[%d] ", Get_node_index(kid));
}

//-----------------------------------------------------------------------
// NAME: SUMMARY_EXPR::WB_Print
// FUNCTION: Print SUMMARY_EXPR in whirl browser format 
//-----------------------------------------------------------------------

void SUMMARY_EXPR::WB_Print(FILE* fp,
                            INT expr_index)
{
  fprintf(fp, "EXPR[%d]: ", expr_index);
  if (Is_expr_unknown()) {
    fprintf(fp, "<UNKNOWN>\n");
    return;
  }
  fprintf(fp, "%s ", Get_mtype() == MTYPE_UNKNOWN
     ? "?" : MTYPE_name(Get_mtype()));
  fputs(OPCODE_name(Get_opcode()), fp);
  fprintf(fp, " ");
  if (Has_const_operand()) {
    if (Get_kid() == 0) {
      Node(fp, 0);
      if (OPCODE_nkids(Get_opcode()) == 2)
        fprintf(fp, "%lld ", Get_const_value());
    } else {
      fprintf(fp, "%lld ", Get_const_value());
      Node(fp, 0);
    }
  } else {
    Node(fp, 0);
    Node(fp, 1);
  }
  if (Is_trip_count())
    fprintf(fp, " <TRIP COUNT> ");
  fprintf(fp, "\n");
}

//-----------------------------------------------------------------------
// NAME: SUMMARY_STID::WB_Print
// FUNCTION: Print SUMMARY_STID in whirl browser format 
//-----------------------------------------------------------------------

void SUMMARY_STID::WB_Print(FILE* fp,
                            INT stid_index,
			    const char* name,
			    const char* func_name) 
{
  fprintf(fp, "STID[%d]: ", stid_index);
  if (func_name == NULL || func_name[0] == '\0')
    fprintf(fp, "Scalar SYMBOL[%d] \"%s\" ",
      Get_symbol_index(), name);
  else
    fprintf(fp, "Scalar SYMBOL[%d] \"%s\":\"%s\" ",
      Get_symbol_index(), name, func_name);
  if (Get_value_index() != -1)
    fprintf(fp, "VALUE[%d] ", Get_value_index());
  if (Is_always_executed())
    fprintf(fp, "<ALWAYS EXECUTED> ");
  fprintf(fp, "\n");
} 

//-----------------------------------------------------------------------
// NAME: SUMMARY_STMT::WB_Print
// FUNCTION: Print SUMMARY_STMT in whirl browser format 
//-----------------------------------------------------------------------

void SUMMARY_STMT::WB_Print(FILE* fp,
                            INT stmt_index,
			    const char* name,
			    const char* func_name)
{ 
  fprintf(fp, "STMT[%d]: ", stmt_index);
  if (Is_expr())
    fprintf(fp, "EXPR[%d] ", Get_expr_index());
  else if (Is_var()) {
    if (func_name == NULL || func_name[0] == '\0')
      fprintf(fp, "SYMBOL[%d] \"%s\" ", Get_var_index(), name);
    else
      fprintf(fp, "SYMBOL[%d] \"%s\":\"%s\" ", Get_var_index(),
        name, func_name);
    fprintf(fp, "MOD_COUNT(%d) REF_COUNT(%d) ADDR_TAKEN_COUNT(%d) ",
      Get_write_count(), Get_ref_count(),
      Get_addr_taken_count());
  } else if (Is_call())
    fprintf(fp, "CALLSITE[%d] ", Get_call_index());
  else if (Is_cond())
    fprintf(fp, "CTRL_DEP[%d] ", Get_cond_index());
  else if (Is_array_ref())
    fprintf(fp, "ARRAY MAP_ID(%d) ", Get_array_ref_map_id());
  else if (Is_stid())
    fprintf(fp, "STID[%d] ", Get_stid_index());
  else
    fprintf(fp, "UNKNOWN_TYPE ");
  fprintf(fp, "\n");
} 

//-----------------------------------------------------------------------
// NAME: SUMMARY_FEEDBACK::WB_Print
// FUNCTION: Print SUMMARY_FEEDBACK in whirl browser format 
//-----------------------------------------------------------------------

void SUMMARY_FEEDBACK::WB_Print(FILE* fp,
                                INT feedback_index)
{
  fprintf(fp, "FEEDBACK[%d]: ", feedback_index);
  fprintf(fp, "CYCLE(%f) FREQ(%f) BB_COUNT(%d) STMT_COUNT(%d) ",
    Get_cycle_count().Value(), Get_frequency_count().Value(),
    Get_effective_bb_count(), Get_effective_stmt_count());
  fprintf(fp, "\n");
} 
