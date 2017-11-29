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
#include "wb_util.h"
#include "dwarf_DST.h"
#include "ipc_file.h"
#include "ipa_summary.h"
#include "opt_du.h" 
#include "opt_alias_mgr.h"
#include "dep_graph.h"
#include "wb_browser.h"
#include "ipa_section_annot.h"
#include "ipc_symtab_merge.h" 
#include "ipl_summary.h" 
#include "ipa_section.h" 

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Summary_Symbol()
// CALL GRAPH BROWSER COMMAND: 'SS'
// FUNCTION: Print info about i-th SUMMARY_SYMBOL.
//-----------------------------------------------------------------------

void WB_BROWSER::Summary_Symbol(FILE* fp, 
				INT symbol_index,
				BOOL is_list)
{ 
  if (Scalar_Summary() == NULL || symbol_index < 0 
      || symbol_index > Scalar_Summary()->Get_symbol_idx()) {
    Error_Cleanup();
    return; 
  } 
  SUMMARY_SYMBOL* ss = Scalar_Summary()->Get_symbol(symbol_index);
  const char* name = ST_name(ss->St_idx());
  ss->WB_Print(fp, symbol_index, is_list, name, "",  Fancy_Level());
} 
  
//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Summary_Ivar()
// CALL GRAPH BROWSER COMMAND: 'SI'
// FUNCTION: Print info about IVARs.
//-----------------------------------------------------------------------

void WB_BROWSER::Summary_Ivar(FILE* fp, 
			      INT ivar_index)
{ 
  if (Array_Summary() == NULL || ivar_index < 0 
      || ivar_index >= Array_Summary()->Get_ivar_array_count()) {
    Error_Cleanup();
    return; 
  } 
  IVAR* ivar = Array_Summary()->Get_ivar_array(ivar_index);
  ivar->WB_Print(fp, ivar_index);
}   

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Summary_Formal()
// CALL GRAPH BROWSER COMMAND: 'SF'
// FUNCTION: Print info about i-th SUMMARY_FORMAL.
//-----------------------------------------------------------------------

void WB_BROWSER::Summary_Formal(FILE* fp, 
				INT formal_index)
{ 
  if (Scalar_Summary() == NULL || formal_index < 0 
      || formal_index > Scalar_Summary()->Get_formal_idx()) {
    Error_Cleanup();
    return; 
  } 
  SUMMARY_FORMAL* sf = Scalar_Summary()->Get_formal(formal_index);
  INT symbol_index = sf->Get_symbol_index();
  SUMMARY_SYMBOL* ss = Scalar_Summary()->Get_symbol(symbol_index);
  const char* name = ST_name(ss->St_idx());
  sf->WB_Print(fp, formal_index, name, "");
} 

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Summary_Common()
// CALL GRAPH BROWSER COMMAND: 'SC'
// FUNCTION: Print info about i-th SUMMARY_COMMON.
//-----------------------------------------------------------------------

void WB_BROWSER::Summary_Common(FILE* fp, 
			        INT common_index)
{ 
  if (Scalar_Summary() == NULL || common_index < 0 
      || common_index > Scalar_Summary()->Get_common_idx()) {
    Error_Cleanup();
    return; 
  } 
  SUMMARY_COMMON* sc = Scalar_Summary()->Get_common(common_index);
  sc->WB_Print(fp, common_index);
} 

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Summary_Common_Shape()
// CALL GRAPH BROWSER COMMAND: 'SK'
// FUNCTION: Print info about i-th SUMMARY_COMMON_SHAPE.
//-----------------------------------------------------------------------

void WB_BROWSER::Summary_Common_Shape(FILE* fp, 
				      INT common_shape_index)
{ 
  if (Scalar_Summary() == NULL || common_shape_index < 0 
      || common_shape_index > Scalar_Summary()->Get_common_shape_idx()) {
    Error_Cleanup();
    return; 
  } 
  SUMMARY_COMMON_SHAPE* sc = 
    Scalar_Summary()->Get_common_shape(common_shape_index);
  sc->WB_Print(fp, common_shape_index);
} 

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Summary_Procedure()
// CALL GRAPH BROWSER COMMAND: 'SP'
// FUNCTION: Print info about i-th SUMMARY_PROCEDURE.
//-----------------------------------------------------------------------

void WB_BROWSER::Summary_Procedure(FILE* fp, 
				   INT procedure_index)
{ 
  if (Scalar_Summary() == NULL || procedure_index < 0 
      || procedure_index > Scalar_Summary()->Get_procedure_idx()) {
    Error_Cleanup();
    return; 
  } 
  SUMMARY_PROCEDURE* sp = Scalar_Summary()->Get_procedure(procedure_index);
  INT symbol_index = sp->Get_symbol_index();
  SUMMARY_SYMBOL* ss = Scalar_Summary()->Get_symbol(symbol_index);
  char* name = ST_name(ss->St_idx());
  sp->WB_Print(fp, procedure_index, name, Fancy_Level());
}  

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Summary_Global()
// CALL GRAPH BROWSER COMMAND: 'SG'
// FUNCTION: Print info about i-th SUMMARY_GLOBAL.
//-----------------------------------------------------------------------

void WB_BROWSER::Summary_Global(FILE* fp, 
				INT global_index)
{
  if (Scalar_Summary() == NULL || global_index < 0 
      || global_index > Scalar_Summary()->Get_procedure_idx()) {
    Error_Cleanup();
    return; 
  } 
  SUMMARY_GLOBAL* sg = Scalar_Summary()->Get_global(global_index);
  sg->WB_Print(fp, global_index);
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Summary_Callsite()
// CALL GRAPH BROWSER COMMAND: 'SL'
// FUNCTION: Print info about i-th SUMMARY_CALLSITE.
//-----------------------------------------------------------------------

void WB_BROWSER::Summary_Callsite(FILE* fp, 
				  INT callsite_index)
{ 
  if (Scalar_Summary() == NULL || callsite_index < 0 
      || callsite_index > Scalar_Summary()->Get_callsite_idx()) {
    Error_Cleanup();
    return; 
  } 
  SUMMARY_CALLSITE* sc = Scalar_Summary()->Get_callsite(callsite_index);
  const char* name = NULL; 
  if (!sc->Is_intrinsic() && !sc->Is_func_ptr()) {
    INT symbol_index = sc->Get_symbol_index();
    SUMMARY_SYMBOL* ss = Scalar_Summary()->Get_symbol(symbol_index);
    name = ST_name(ss->St_idx());
  } 
  sc->WB_Print(fp, callsite_index, name, "");
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Summary_Control_Dependence()
// CALL GRAPH BROWSER COMMAND: 'SD'
// FUNCTION: Print info about i-th SUMMARY_CONTROL_DEPENDENCE.
//-----------------------------------------------------------------------

void WB_BROWSER::Summary_Control_Dependence(FILE* fp, 
					    INT control_index)
{
  if (Scalar_Summary() == NULL || control_index < 0 
      || control_index > Scalar_Summary()->Get_ctrl_dep_idx()) {
    Error_Cleanup();
    return; 
  } 
  SUMMARY_CONTROL_DEPENDENCE* sd 
    = Scalar_Summary()->Get_ctrl_dep(control_index);
  sd->WB_Print(fp, control_index);
} 

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Summary_Actual()
// CALL GRAPH BROWSER COMMAND: 'SA'
// FUNCTION: Print info about i-th SUMMARY_ACTUAL.
//-----------------------------------------------------------------------

void WB_BROWSER::Summary_Actual(FILE* fp, 
				INT actual_index)
{
  if (Scalar_Summary() == NULL || actual_index < 0 
      || actual_index > Scalar_Summary()->Get_actual_idx()) {
    Error_Cleanup();
    return; 
  } 
  SUMMARY_ACTUAL* sa = Scalar_Summary()->Get_actual(actual_index);
  const char* name = NULL; 
  if (sa->Get_symbol_index() != -1) {
    INT symbol_index = sa->Get_symbol_index();
    SUMMARY_SYMBOL* ss = Scalar_Summary()->Get_symbol(symbol_index);
    name = ST_name(ss->St_idx());
  } 
  sa->WB_Print(fp, actual_index, name, "");
}  

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Summary_Value()
// CALL GRAPH BROWSER COMMAND: 'SV'
// FUNCTION: Print info about i-th SUMMARY_VALUE.
//-----------------------------------------------------------------------

void WB_BROWSER::Summary_Value(FILE* fp, 
			       INT value_index)
{ 
  if (Scalar_Summary() == NULL || value_index < 0 
      || value_index > Scalar_Summary()->Get_value_idx()) {
    Error_Cleanup();
    return; 
  } 
  SUMMARY_VALUE* sv = Scalar_Summary()->Get_value(value_index);
  sv->WB_Print(fp, value_index);
}
  
//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Summary_Cfg_Node()
// CALL GRAPH BROWSER COMMAND: 'Sa'
// FUNCTION: Print info about i-th CFG_NODE_INFO.
//-----------------------------------------------------------------------

void WB_BROWSER::Summary_Cfg_Node(FILE* fp, 
				  INT cfg_index)
{ 
  if (Array_Summary() == NULL || cfg_index < 0 
      || cfg_index >= Array_Summary()->Get_cfg_node_array_count()) {
    Error_Cleanup();
    return; 
  } 
  CFG_NODE_INFO* cfg = Array_Summary()->Get_cfg_node_array(cfg_index);
  cfg->WB_Print(fp, cfg_index);
} 

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Summary_Region()
// CALL GRAPH BROWSER COMMAND: 'SR'
// FUNCTION: Print info about i-th REGION_ARRAYS.
//-----------------------------------------------------------------------

void WB_BROWSER::Summary_Region(FILE* fp, 
			        INT region_index)
{ 
  if (Array_Summary() == NULL || region_index < 0 
      || region_index >= Array_Summary()->Get_region_array_count()) {
    Error_Cleanup();
    return; 
  } 
  REGION_ARRAYS* ra = Array_Summary()->Get_region_array(region_index);
  INT symbol_index = ra->Get_sym_id();
  SUMMARY_SYMBOL* ss = Scalar_Summary()->Get_symbol(symbol_index);
  const char* name = ST_name(ss->St_idx());
  ra->WB_Print(fp, region_index, name, "");
} 

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Summary_Region()
// CALL GRAPH BROWSER COMMAND: 'Sr'
// FUNCTION: Print info about i-th PROJECTED_REGION.
//-----------------------------------------------------------------------

void WB_BROWSER::Summary_Projected_Region(FILE* fp, 
					  INT proj_region_index)
{ 
  if (Array_Summary() == NULL || proj_region_index < 0 
      || proj_region_index 
      >= Array_Summary()->Get_projected_region_array_count()) {
    Error_Cleanup();
    return; 
  } 
  PROJECTED_REGION* pr 
    = Array_Summary()->Get_projected_region_array(proj_region_index);
  pr->WB_Print(fp, proj_region_index);
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Summary_Projected_Node()
// CALL GRAPH BROWSER COMMAND: 'Sn'
// FUNCTION: Print info about i-th PROJECTED_NODE.
//-----------------------------------------------------------------------

void WB_BROWSER::Summary_Projected_Node(FILE* fp, 
					INT proj_node_index)
{ 
  if (Array_Summary() == NULL || proj_node_index < 0 
      || proj_node_index >= Array_Summary()->Get_projected_array_count()) {
    Error_Cleanup();
    return; 
  } 
  PROJECTED_NODE* pn 
    = Array_Summary()->Get_projected_array(proj_node_index);
  pn->WB_Print(fp, proj_node_index);
} 

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Summary_Term()
// CALL GRAPH BROWSER COMMAND: 'St'
// FUNCTION: Print info about i-th TERM.
//-----------------------------------------------------------------------

void WB_BROWSER::Summary_Term(FILE* fp, 
			      INT term_index)
{ 
  if (Array_Summary() == NULL || term_index < 0 
      || term_index >= Array_Summary()->Get_term_array_count()) {
    Error_Cleanup();
    return;
  }
  TERM* tm = Array_Summary()->Get_term_array(term_index);
  tm->WB_Print(fp, term_index);
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Summary_Loop_Info()
// CALL GRAPH BROWSER COMMAND: 'Sl'
// FUNCTION: Print info about i-th LOOPINFO.
//-----------------------------------------------------------------------

void WB_BROWSER::Summary_Loop_Info(FILE* fp, 
				   INT loop_info_index)
{ 
  if (Array_Summary() == NULL || loop_info_index < 0 
      || loop_info_index >= Array_Summary()->Get_loopinfo_array_count()) {
    Error_Cleanup();
    return;
  }
  LOOPINFO* li = Array_Summary()->Get_loopinfo_array(loop_info_index);
  li->WB_Print(fp, loop_info_index);
} 

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Summary_Phi()
// CALL GRAPH BROWSER COMMAND: 'Sp'
// FUNCTION: Print info about i-th SUMMARY_PHI.
//-----------------------------------------------------------------------

void WB_BROWSER::Summary_Phi(FILE* fp, 
			     INT phi_index)
{ 
  if (Scalar_Summary() == NULL || phi_index < 0 
      || phi_index > Scalar_Summary()->Get_phi_idx()) {
    Error_Cleanup();
    return; 
  } 
  SUMMARY_PHI* sp = Scalar_Summary()->Get_phi(phi_index);
  sp->WB_Print(fp, phi_index);
} 

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Summary_Chi()
// CALL GRAPH BROWSER COMMAND: 'Sx'
// FUNCTION: Print info about i-th SUMMARY_CHI.
//-----------------------------------------------------------------------

void WB_BROWSER::Summary_Chi(FILE* fp, 
			     INT chi_index)
{ 
  if (Scalar_Summary() == NULL || chi_index < 0 
      || chi_index > Scalar_Summary()->Get_chi_idx()) {
    Error_Cleanup();
    return; 
  } 
  SUMMARY_CHI* sc = Scalar_Summary()->Get_chi(chi_index);
  const char* name = NULL; 
  if (sc->Get_symbol_index() != -1) { 
    INT symbol_index = sc->Get_symbol_index();
    SUMMARY_SYMBOL* ss = Scalar_Summary()->Get_symbol(symbol_index);
    name = ST_name(ss->St_idx());
  } 
  sc->WB_Print(fp, chi_index, name, "");
} 

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Summary_Expr()
// CALL GRAPH BROWSER COMMAND: 'SE'
// FUNCTION: Print info about i-th SUMMARY_EXPR.
//-----------------------------------------------------------------------

void WB_BROWSER::Summary_Expr(FILE* fp, 
			      INT expr_index)
{ 
  if (Scalar_Summary() == NULL || expr_index < 0 
      || expr_index > Scalar_Summary()->Get_expr_idx()) {
    Error_Cleanup();
    return; 
  } 
  SUMMARY_EXPR* se = Scalar_Summary()->Get_expr(expr_index);
  se->WB_Print(fp, expr_index);
} 

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Summary_Stid()
// CALL GRAPH BROWSER COMMAND: 'SX'
// FUNCTION: Print info about i-th SUMMARY_STID.
//-----------------------------------------------------------------------

void WB_BROWSER::Summary_Stid(FILE* fp, 
			      INT stid_index)

{
  if (Scalar_Summary() == NULL || stid_index < 0 
      || stid_index > Scalar_Summary()->Get_global_stid_idx()) {
    Error_Cleanup();
    return; 
  } 
  SUMMARY_STID* stid = Scalar_Summary()->Get_global_stid(stid_index);
  const char* name = NULL; 
  if (!stid->Is_array_assignment()) { 
    INT symbol_index = stid->Get_symbol_index();
    SUMMARY_SYMBOL* ss = Scalar_Summary()->Get_symbol(symbol_index);
    name = ST_name(ss->St_idx());
  } 
  stid->WB_Print(fp, stid_index, name, "");
} 

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Summary_Stmt()
// CALL GRAPH BROWSER COMMAND: 'SY'
// FUNCTION: Print info about i-th SUMMARY_STMT.
//-----------------------------------------------------------------------

void WB_BROWSER::Summary_Stmt(FILE* fp, 
			      INT stmt_index)
{ 
  if (Scalar_Summary() == NULL || stmt_index < 0 
      || stmt_index > Scalar_Summary()->Get_stmt_idx()) {
    Error_Cleanup();
    return; 
  } 
  SUMMARY_STMT* stmt = Scalar_Summary()->Get_stmt(stmt_index);
  const char* name = NULL; 
  if (stmt->Is_var()) { 
    INT symbol_index = stmt->Get_var_index();
    SUMMARY_SYMBOL* ss = Scalar_Summary()->Get_symbol(symbol_index);
    name = ST_name(ss->St_idx());
  } 
  stmt->WB_Print(fp, stmt_index, name, "");
} 

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Summary_Feedback()
// CALL GRAPH BROWSER COMMAND: 'Sf'
// FUNCTION: Print info about i-th SUMMARY_FEEDBACK.
//-----------------------------------------------------------------------

void WB_BROWSER::Summary_Feedback(FILE* fp, 
				  INT feedback_index)
{
  if (Scalar_Summary() == NULL || feedback_index < 0 
      || feedback_index > Scalar_Summary()->Get_feedback_idx()) {
    Error_Cleanup();
    return; 
  } 
  SUMMARY_FEEDBACK* sf = Scalar_Summary()->Get_feedback(feedback_index);
  sf->WB_Print(fp, feedback_index);
} 

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Summary_Scalar_Command()
// FUNCTION: Return TRUE if the character 'ch' represents a valid scalar 
//   summary command, returns FALSE otherwise. 
//-----------------------------------------------------------------------

BOOL WB_BROWSER::Summary_Scalar_Command(char ch)
{ 
  switch (ch) { 
  case 'S':
  case 'F':
  case 'G':
  case 'A':
  case 'V':
  case 'C':
  case 'K':
  case 'D':
  case 'L':
  case 'p': 
  case 'x': 
  case 'E':  
  case 'X': 
  case 'Y': 
  case 'f': 
  case 'P':
    return TRUE;
  default: 
    return FALSE;
  } 
} 

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Summary_Array_Command()
// FUNCTION: Return TRUE if the character 'ch' represents a valid array
//   summary command, returns FALSE otherwise. 
//-----------------------------------------------------------------------

BOOL WB_BROWSER::Summary_Array_Command(char ch)
{ 
  switch (ch) { 
  case 'I': 
  case 'J':
  case 'a':
  case 's':
  case 'R': 
  case 'r': 
  case 'n':
  case 't':
  case 'l': 
    return TRUE;
  default: 
    return FALSE;
  } 
} 

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Summary_Valid_Command()
// FUNCTION: Return TRUE if the character 'ch' represents a valid summary 
//   command, returns FALSE otherwise. 
//-----------------------------------------------------------------------

BOOL WB_BROWSER::Summary_Valid_Command(char ch)
{ 
  return (Summary_Scalar_Command(ch) || Summary_Array_Command(ch));
} 

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Summary_Size()
// FUNCTION: Return the size of the summary associated with the character
//   'ch' and current IPA_NODE.
//-----------------------------------------------------------------------

INT WB_BROWSER::Summary_Size(char ch)
{ 
  switch (ch) { 
  case 'S':
    return Scalar_Summary()->Get_symbol_idx() + 1;
  case 'I':
    return Array_Summary()->Get_ivar_array_count();
  case 'F':
    return Scalar_Summary()->Get_formal_idx() + 1;
  case 'G':
    return Scalar_Summary()->Get_global_idx() + 1;
  case 'A': 
    return Scalar_Summary()->Get_actual_idx() + 1;
  case 'V': 
    return Scalar_Summary()->Get_value_idx() + 1;
  case 'C': 
    return Scalar_Summary()->Get_common_idx() + 1;
  case 'K': 
    return Scalar_Summary()->Get_common_shape_idx() + 1;
  case 'D': 
    return Scalar_Summary()->Get_ctrl_dep_idx() + 1;
  case 'L': 
    return Scalar_Summary()->Get_callsite_idx() + 1;
  case 'a': 
    return Array_Summary()->Get_cfg_node_array_count();
  case 'R': 
    return Array_Summary()->Get_region_array_count();
  case 'r': 
    return Array_Summary()->Get_projected_region_array_count();
  case 'n': 
    return Array_Summary()->Get_projected_array_count();
  case 't': 
    return Array_Summary()->Get_term_array_count();
  case 'l': 
    return Array_Summary()->Get_loopinfo_array_count();
  case 'p': 
    return Scalar_Summary()->Get_phi_idx() + 1;
  case 'x': 
    return Scalar_Summary()->Get_chi_idx() + 1;
  case 'T': 
    return 0; // no more tcon in summary info. 
  case 'E': 
    return Scalar_Summary()->Get_expr_idx() + 1;
  case 'X': 
    return Scalar_Summary()->Get_global_stid_idx() + 1;
  case 'Y': 
    return Scalar_Summary()->Get_stmt_idx() + 1;
  case 'f': 
    return Scalar_Summary()->Get_feedback_idx() + 1; 
  case 'P': 
    return Scalar_Summary()->Get_procedure_idx() + 1;
  default:
    FmtAssert(FALSE, ("WB_BROWSER::Summary_Size(): Unexpected command"));
    return -1; 
  }
} 

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Summary_Single()
// FUNCTION: Print summary info the 'index'-th entry in the summary 
//   indicated by character 'ch'.  
//-----------------------------------------------------------------------

void WB_BROWSER::Summary_Single(FILE* fp, 
				char ch, 
				INT index,
				BOOL is_list)
{ 
  switch (ch) {
  case 'S':
    Summary_Symbol(fp, index, is_list);
    break;
  case 'I':
    Summary_Ivar(fp, index);
    break;
  case 'F':
    Summary_Formal(fp, index);
    break; 
  case 'G':
    Summary_Global(fp, index);
    break;
  case 'A': 
    Summary_Actual(fp, index);
    break;
  case 'V': 
    Summary_Value(fp, index);
    break; 
  case 'C': 
    Summary_Common(fp, index);
    break; 
  case 'K': 
    Summary_Common_Shape(fp, index); 
    break; 
  case 'D': 
    Summary_Control_Dependence(fp, index);
    break; 
  case 'L': 
    Summary_Callsite(fp, index); 
    break;
  case 'a': 
    Summary_Cfg_Node(fp, index);
    break; 
  case 'R': 
    Summary_Region(fp, index);
    break; 
  case 'r': 
    Summary_Projected_Region(fp, index);
    break; 
  case 'n':
    Summary_Projected_Node(fp, index);
    break; 
  case 't':
    Summary_Term(fp, index);
    break; 
  case 'l': 
    Summary_Loop_Info(fp, index);
    break; 
  case 'p': 
    Summary_Phi(fp, index); 
    break;
  case 'x': 
    Summary_Chi(fp, index);
    break;
  case 'T': 
    break; // no more tcon in summary info. 
  case 'X': 
    Summary_Stid(fp, index);
    break; 
  case 'Y': 
    Summary_Stmt(fp, index);
    break; 
  case 'E':
    Summary_Expr(fp, index);
    break;
  case 'f': 
    Summary_Feedback(fp, index);
    break; 
  case 'P': 
    Summary_Procedure(fp, index);
    break;
  default:
    FmtAssert(FALSE, ("WB_BROWSER::Summary_Single(): Unexpected command"));
    break; 
  }
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Summary()
// CALL GRAPH BROWSER COMMAND: 'S'
// FUNCTION: Print summary info at this node
//-----------------------------------------------------------------------

void WB_BROWSER::Summary(FILE* fp)
{
  Set_Subcommand('~');
  char ch = Buffer().Scan_Character();
  if (Summary_Valid_Command(ch)) { 
    if (Summary_Scalar_Command(ch) && Scalar_Summary() == NULL
        || Summary_Array_Command(ch) && Array_Summary() == NULL) {
      Error_Cleanup();
      return; 
    }  
    INT index = -1;
    INT count = Summary_Size(ch);
    if (Buffer().Is('[')) { 
      Buffer().Scan_Character();
      INT low_index = -1;
      Buffer().Scan_Integer(&low_index);
      if (low_index < 0 || low_index >= count) {
	Error_Cleanup();
	return; 
      }  
      char separator = Buffer().Scan_Character();
      switch (separator) { 
      case ':': {
	INT local_count = -1;
        Buffer().Scan_Integer(&local_count);
	if (local_count < 0 || local_count > count) {
	  Error_Cleanup(); 
	  return; 
	} 
	for (INT i = low_index; i < low_index + local_count; i++)
	  Summary_Single(fp, ch, i, TRUE);  
        break;
      }
      case '-': {
	INT high_index = -1;
        Buffer().Scan_Integer(&high_index);
	if (high_index < low_index || high_index > count) {
	  Error_Cleanup(); 
	  return; 
	} 
	for (INT i = low_index; i <= high_index; i++)
	  Summary_Single(fp, ch, i, TRUE);  
	break; 
      }
      default: 
	Error_Cleanup();
	return; 
      }  
      Buffer().Skip_Chars(1);
    } else if (Buffer().Is_Integer()) { 
      Buffer().Scan_Integer(&index);
      if (index < 0 || index >= count) {
	Error_Cleanup();
	return;
      }
      Summary_Single(fp, ch, index, FALSE); 
    } else {   
      for (INT i = 0; i < count; i++)
	Summary_Single(fp, ch, i, TRUE);
    } 
  } else if (ch == '=') { 
    fprintf(stdout, "Summary Locate Not Implemented\n");
    Buffer().Pushback_Character();
  } else if (ch == 'H') { 
    Help();
  } else if (ch == '\n') { 
    fprintf(stdout, "Missing character\n");
    Buffer().Pushback_Character();
  } else { 
    fprintf(stdout, "Bad character: %c\n", ch);
  } 
  Reset_Subcommand();
} 

extern "C" void WB_BROWSER_Summary (FILE *fp, WB_BROWSER *wb)
{
  wb->Summary(fp);
}
