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
#include "cg_browser.h"
#include "ipa_section_annot.h"
#include "ipc_symtab_merge.h" 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Summary_Symbol()
// CALL GRAPH BROWSER COMMAND: 'SS'
// FUNCTION: Print info about i-th SUMMARY_SYMBOL.
//-----------------------------------------------------------------------

void CG_BROWSER::Summary_Symbol(FILE* fp, 
				INT symbol_index,
				BOOL is_list)
{ 
  SUMMARY_SYMBOL* symbol_array = IPA_get_symbol_array(Cnode());
  SUMMARY_SYMBOL* ss = &symbol_array[symbol_index]; 
  char* func_name = NULL; 
  char* name = Symbol_Name(symbol_index, &func_name);
  ss->WB_Print(fp, symbol_index, is_list, name, func_name, Fancy_Level());
}

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Summary_Ivar()
// CALL GRAPH BROWSER COMMAND: 'SI'
// FUNCTION: Print info about IVARs.
//-----------------------------------------------------------------------

void CG_BROWSER::Summary_Ivar(FILE* fp, 
			      INT ivar_index)
{ 
  INT32 ivar_count;
  IVAR* ivar = IPA_get_ivar_array(Cnode(), ivar_count) + ivar_index;
  ivar->WB_Print(fp, ivar_index);
}

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Summary_Formal()
// CALL GRAPH BROWSER COMMAND: 'SF'
// FUNCTION: Print info about i-th SUMMARY_FORMAL.
//-----------------------------------------------------------------------

void CG_BROWSER::Summary_Formal(FILE* fp, 
				INT formal_index)
{ 
  SUMMARY_FORMAL* formal_array = IPA_get_formal_array(Cnode());   
  SUMMARY_FORMAL* sf = &formal_array[formal_index]; 
  char* func_name = NULL; 
  char* name = Symbol_Name(sf->Get_symbol_index(), &func_name);
  sf->WB_Print(fp, formal_index, name, func_name);
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Summary_Common()
// CALL GRAPH BROWSER COMMAND: 'SC'
// FUNCTION: Print info about i-th SUMMARY_COMMON.
//-----------------------------------------------------------------------

void CG_BROWSER::Summary_Common(FILE* fp, 
			        INT common_index)
{ 
  SUMMARY_COMMON* common_array = IPA_get_common_array(Cnode());
  SUMMARY_COMMON* sc = &common_array[common_index]; 
  sc->WB_Print(fp, common_index);
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Summary_Common_Shape()
// CALL GRAPH BROWSER COMMAND: 'SK'
// FUNCTION: Print info about i-th SUMMARY_COMMON_SHAPE.
//-----------------------------------------------------------------------

void CG_BROWSER::Summary_Common_Shape(FILE* fp, 
				      INT common_shape_index)
{ 
  SUMMARY_COMMON_SHAPE* common_shape_array 
    = IPA_get_common_shape_array(Cnode());
  SUMMARY_COMMON_SHAPE* scs = &common_shape_array[common_shape_index]; 
  scs->WB_Print(fp, common_shape_index);
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Summary_Procedure()
// CALL GRAPH BROWSER COMMAND: 'SP'
// FUNCTION: Print info about i-th SUMMARY_PROCEDURE.
//-----------------------------------------------------------------------

void CG_BROWSER::Summary_Procedure(FILE* fp, 
				   INT procedure_index)
{ 
  SUMMARY_PROCEDURE* procedure_array = IPA_get_procedure_array(Cnode());
  SUMMARY_PROCEDURE* sp = &procedure_array[procedure_index]; 
  INT symbol_index = sp->Get_symbol_index();
  sp->WB_Print(fp, procedure_index, Symbol_Name(symbol_index), Fancy_Level());
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Summary_Global()
// CALL GRAPH BROWSER COMMAND: 'SG'
// FUNCTION: Print info about i-th SUMMARY_GLOBAL.
//-----------------------------------------------------------------------

void CG_BROWSER::Summary_Global(FILE* fp, 
				INT global_index)
{ 
  SUMMARY_GLOBAL* global_array = IPA_get_global_array(Cnode());
  SUMMARY_GLOBAL* sg = &global_array[global_index]; 
  sg->WB_Print(fp, global_index);
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Summary_Callsite()
// CALL GRAPH BROWSER COMMAND: 'SL'
// FUNCTION: Print info about i-th SUMMARY_CALLSITE.
//-----------------------------------------------------------------------

void CG_BROWSER::Summary_Callsite(FILE* fp, 
				  INT callsite_index)
{ 
  SUMMARY_CALLSITE* callsite_array = IPA_get_callsite_array(Cnode());
  SUMMARY_CALLSITE* sc = &callsite_array[callsite_index]; 
  char* func_name = NULL;
  char* name = NULL;
  if (!sc->Is_intrinsic() && !sc->Is_func_ptr())
    name = Symbol_Name(sc->Get_symbol_index(), &func_name);
  sc->WB_Print(fp, callsite_index, name, func_name);
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Summary_Control_Dependence()
// CALL GRAPH BROWSER COMMAND: 'SD'
// FUNCTION: Print info about i-th SUMMARY_CONTROL_DEPENDENCE.
//-----------------------------------------------------------------------

void CG_BROWSER::Summary_Control_Dependence(FILE* fp, 
					    INT control_index)
{
  SUMMARY_CONTROL_DEPENDENCE* control_array 
    = IPA_get_ctrl_dep_array(Cnode());
  SUMMARY_CONTROL_DEPENDENCE* sc = &control_array[control_index]; 
  sc->WB_Print(fp, control_index);
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Summary_Actual()
// CALL GRAPH BROWSER COMMAND: 'SA'
// FUNCTION: Print info about i-th SUMMARY_ACTUAL.
//-----------------------------------------------------------------------

void CG_BROWSER::Summary_Actual(FILE* fp, 
				INT actual_index)
{
  SUMMARY_ACTUAL* actual_array = IPA_get_actual_array(Cnode());
  SUMMARY_ACTUAL* aa = &actual_array[actual_index]; 
  char* func_name = NULL; 
  char* name = NULL;
  if (aa->Get_symbol_index() != -1)
    name = Symbol_Name(aa->Get_symbol_index(), &func_name);
  aa->WB_Print(fp, actual_index, name, func_name);
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Summary_Value()
// CALL GRAPH BROWSER COMMAND: 'SV'
// FUNCTION: Print info about i-th SUMMARY_VALUE.
//-----------------------------------------------------------------------

void CG_BROWSER::Summary_Value(FILE* fp, 
			       INT value_index)
{ 
  SUMMARY_VALUE* value_array = IPA_get_value_array(Cnode());
  SUMMARY_VALUE* sv = &value_array[value_index]; 
  sv->WB_Print(fp, value_index);
}

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Summary_Cfg_Node()
// CALL GRAPH BROWSER COMMAND: 'Sa'
// FUNCTION: Print info about i-th CFG_NODE_INFO.
//-----------------------------------------------------------------------

void CG_BROWSER::Summary_Cfg_Node(FILE* fp, 
				  INT cfg_index)
{ 
  CFG_NODE_INFO* cfg_array = IPA_get_cfg_node_array(Cnode());
  CFG_NODE_INFO* cfg = &cfg_array[cfg_index]; 
  cfg->WB_Print(fp, cfg_index);
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Summary_Scalar()
// CALL GRAPH BROWSER COMMAND: 'Ss'
// FUNCTION: Print info about i-th SCALAR_INFO.
//-----------------------------------------------------------------------

void CG_BROWSER::Summary_Scalar(FILE* fp, 
				INT scalar_index) 
{
  SCALAR_INFO* scalar_array = IPA_get_scalar_array(Cnode());
  SCALAR_INFO* si = &scalar_array[scalar_index]; 
  char* func_name = NULL; 
  char* name = Symbol_Name(si->Get_id(), &func_name);
  si->WB_Print(fp, scalar_index, name, func_name);
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Summary_Region()
// CALL GRAPH BROWSER COMMAND: 'SR'
// FUNCTION: Print info about i-th REGION_ARRAYS.
//-----------------------------------------------------------------------

void CG_BROWSER::Summary_Region(FILE* fp, 
			        INT region_index)
{ 
  REGION_ARRAYS* region_array = IPA_get_region_array(Cnode());
  REGION_ARRAYS* ra = &region_array[region_index]; 
  char* func_name = NULL; 
  char* name = Symbol_Name(ra->Get_sym_id(), &func_name);
  ra->WB_Print(fp, region_index, name, func_name);
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Summary_Region()
// CALL GRAPH BROWSER COMMAND: 'Sr'
// FUNCTION: Print info about i-th PROJECTED_REGION.
//-----------------------------------------------------------------------

void CG_BROWSER::Summary_Projected_Region(FILE* fp, 
					  INT proj_region_index)
{ 
  PROJECTED_REGION* proj_region_array 
    = IPA_get_proj_region_array(Cnode());
  PROJECTED_REGION* pr = &proj_region_array[proj_region_index]; 
  pr->WB_Print(fp, proj_region_index);
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Summary_Projected_Node()
// CALL GRAPH BROWSER COMMAND: 'Sn'
// FUNCTION: Print info about i-th PROJECTED_NODE.
//-----------------------------------------------------------------------

void CG_BROWSER::Summary_Projected_Node(FILE* fp, 
					INT proj_node_index)
{ 
  PROJECTED_NODE* proj_node_array 
    = IPA_get_projected_node_array(Cnode());
  PROJECTED_NODE* pn = &proj_node_array[proj_node_index]; 
  pn->WB_Print(fp, proj_node_index);
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Summary_Term()
// CALL GRAPH BROWSER COMMAND: 'St'
// FUNCTION: Print info about i-th TERM.
//-----------------------------------------------------------------------

void CG_BROWSER::Summary_Term(FILE* fp, 
			      INT term_index)
{ 
  TERM* term_array = IPA_get_term_array(Cnode());
  TERM* tm = &term_array[term_index]; 
  tm->WB_Print(fp, term_index);
}

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Summary_Loop_Info()
// CALL GRAPH BROWSER COMMAND: 'Sl'
// FUNCTION: Print info about i-th LOOPINFO.
//-----------------------------------------------------------------------

void CG_BROWSER::Summary_Loop_Info(FILE* fp, 
				   INT loop_info_index)
{ 
  LOOPINFO* loopinfo_array = IPA_get_loopinfo_array(Cnode());
  LOOPINFO* li = &loopinfo_array[loop_info_index]; 
  li->WB_Print(fp, loop_info_index);
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Summary_Phi()
// CALL GRAPH BROWSER COMMAND: 'Sp'
// FUNCTION: Print info about i-th SUMMARY_PHI.
//-----------------------------------------------------------------------

void CG_BROWSER::Summary_Phi(FILE* fp, 
			     INT phi_index)
{ 
  SUMMARY_PHI* phi_array = IPA_get_phi_array(Cnode()); 
  SUMMARY_PHI* phi = &phi_array[phi_index]; 
  phi->WB_Print(fp, phi_index);
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Summary_Chi()
// CALL GRAPH BROWSER COMMAND: 'Sx'
// FUNCTION: Print info about i-th SUMMARY_CHI.
//-----------------------------------------------------------------------

void CG_BROWSER::Summary_Chi(FILE* fp, 
			     INT chi_index)
{ 
  SUMMARY_CHI* chi_array = IPA_get_chi_array(Cnode());
  SUMMARY_CHI* chi = &chi_array[chi_index]; 
  char* func_name = NULL;
  char* name = NULL; 
  if (chi->Get_symbol_index() != -1) 
    name = Symbol_Name(chi->Get_symbol_index(), &func_name);
  chi->WB_Print(fp, chi_index, name, func_name);
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Summary_Expr_Node()
// FUNCTION: Print info about i-th 'kid' of SUMMARY_EXPR 'expr'.
//-----------------------------------------------------------------------

void CG_BROWSER::Summary_Expr_Node(FILE* fp, 
				   SUMMARY_EXPR* expr, 
				   INT kid)
{ 
  expr->Node(fp, kid);
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Summary_Expr()
// CALL GRAPH BROWSER COMMAND: 'SE'
// FUNCTION: Print info about i-th SUMMARY_EXPR.
//-----------------------------------------------------------------------

void CG_BROWSER::Summary_Expr(FILE* fp, 
			      INT expr_index)
{ 
  SUMMARY_EXPR* expr_array = IPA_get_expr_array(Cnode());
  SUMMARY_EXPR* expr = &expr_array[expr_index];
  expr->WB_Print(fp, expr_index);
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Summary_Stid()
// CALL GRAPH BROWSER COMMAND: 'SX'
// FUNCTION: Print info about i-th SUMMARY_STID.
//-----------------------------------------------------------------------

void CG_BROWSER::Summary_Stid(FILE* fp, 
			      INT stid_index)

{
  SUMMARY_STID* stid_array = IPA_get_stid_array(Cnode());
  SUMMARY_STID* stid = &stid_array[stid_index]; 
  fprintf(fp, "STID[%d]: ", stid_index);
  char* func_name = NULL; 
  char* name = Symbol_Name(stid->Get_symbol_index(), &func_name);
  stid->WB_Print(fp, stid_index, name, func_name);
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Summary_Stmt()
// CALL GRAPH BROWSER COMMAND: 'SY'
// FUNCTION: Print info about i-th SUMMARY_STMT.
//-----------------------------------------------------------------------

void CG_BROWSER::Summary_Stmt(FILE* fp, 
			      INT stmt_index)
{ 
  SUMMARY_STMT* stmt_array = IPA_get_stmt_array(Cnode()); 
  SUMMARY_STMT* stmt = &stmt_array[stmt_index];
  char* func_name = NULL;
  char* name = NULL;
  if (stmt->Is_var())
    name = Symbol_Name(stmt->Get_var_index(), &func_name); 
  stmt->WB_Print(fp, stmt_index, name, func_name);
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Summary_Feedback()
// CALL GRAPH BROWSER COMMAND: 'Sf'
// FUNCTION: Print info about i-th SUMMARY_FEEDBACK.
//-----------------------------------------------------------------------

void CG_BROWSER::Summary_Feedback(FILE* fp, 
				  INT feedback_index)
{
  SUMMARY_FEEDBACK* feedback_array = IPA_get_feedback_array(Cnode());
  SUMMARY_FEEDBACK* feedback = &feedback_array[Cnode()->File_Index()]; 
  feedback->WB_Print(fp, feedback_index);
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Summary_Valid_Command()
// FUNCTION: Return TRUE if the character 'ch' represents a valid summary 
//   command, returns FALSE otherwise. 
//-----------------------------------------------------------------------

BOOL CG_BROWSER::Summary_Valid_Command(char ch)
{ 
  switch (ch) { 
  case 'S':
  case 'I': 
  case 'F':
  case 'G':
  case 'A':
  case 'V':
  case 'C':
  case 'K':
  case 'D':
  case 'L':
  case 'a':
  case 's':
  case 'R': 
  case 'r': 
  case 'n':
  case 't':
  case 'l': 
  case 'p': 
  case 'x': 
  case 'T': 
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
// NAME: CG_BROWSER::Summary_Size()
// FUNCTION: Return the size of the summary associated with the character
//   'ch' and current IPA_NODE.
//-----------------------------------------------------------------------

INT CG_BROWSER::Summary_Size(char ch)
{ 
  SUMMARY_FILE_HEADER* file_header = 
    IP_FILE_HDR_file_header(Cnode()->File_Header());
  switch (ch) { 
  case 'S':
    return file_header->Get_symbol_size();
  case 'I':
    return file_header->Get_ivar_size();
  case 'F':
    return file_header->Get_formal_size();
  case 'G':
    return file_header->Get_global_size();
  case 'A': 
    return file_header->Get_actual_size();
  case 'V': 
    return file_header->Get_value_size();
  case 'C': 
    return file_header->Get_common_size();
  case 'K': 
    return file_header->Get_common_shape_size();
  case 'D': 
    return file_header->Get_ctrl_dep_size();
  case 'L': 
    return file_header->Get_callsite_size();
  case 'a': 
    return file_header->Get_cfg_node_size();
  case 's': 
    return file_header->Get_scalar_node_size();
  case 'R': 
    return file_header->Get_regions_array_size();
  case 'r': 
    return file_header->Get_projected_region_size();
  case 'n': 
    return file_header->Get_projected_array_size();
  case 't': 
    return file_header->Get_term_array_size();
  case 'l': 
    return file_header->Get_loopinfo_size();
  case 'p': 
    return file_header->Get_phi_size();
  case 'x': 
    return file_header->Get_chi_size();
  case 'T': 
    return 0; // no more tcon in summary info. 
  case 'E': 
    return file_header->Get_expr_size();
  case 'X': 
    return file_header->Get_global_stid_size();
  case 'Y': 
    return file_header->Get_stmt_size();
  case 'f': 
    return file_header->Get_feedback_size(); 
  case 'P': 
    return file_header->Get_proc_size();
  default:
    FmtAssert(FALSE, ("CG_BROWSER::Summary_Size(): Unexpected command"));
    return -1; 
  }
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Summary_Single()
// FUNCTION: Print summary info the 'index'-th entry in the summary 
//   indicated by character 'ch'.  
//-----------------------------------------------------------------------

void CG_BROWSER::Summary_Single(FILE* fp, 
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
  case 's': 
    Summary_Scalar(fp, index);
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
    FmtAssert(FALSE, ("CG_BROWSER::Summary_Single(): Unexpected command"));
    break; 
  }
}

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Summary()
// CALL GRAPH BROWSER COMMAND: 'S'
// FUNCTION: Print summary info at this node
//-----------------------------------------------------------------------

void CG_BROWSER::Summary(FILE* fp)
{
  if (Bad_File()) { 
    Error_Cleanup();
    return; 
  } 
  Set_Subcommand('S');
  char ch = Buffer().Scan_Character();
  if (Summary_Valid_Command(ch)) { 
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
    Summary_Locate(stdout);
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

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Summary_Locate()
// CALL GRAPH BROWSER COMMAND: 'S='
// FUNCTION: Locate the summary at the given address
//-----------------------------------------------------------------------

void CG_BROWSER::Summary_Locate(FILE* fp)
{
  if (Bad_File()) { 
    Error_Cleanup();
    return; 
  } 
  INT summary_address; 
  Buffer().Scan_HexInteger(&summary_address);   
  SUMMARY_SYMBOL* symbol_array = IPA_get_symbol_array(Cnode());
  INT32 size = Summary_Size('S');
  INT i = (SUMMARY_SYMBOL*)(INTPTR)summary_address - symbol_array;
  if (i >= 0 && i < size) {
    Summary_Symbol(fp, i, FALSE);
    return;
  } 
  IVAR* ivar_array = IPA_get_ivar_file_array(Cnode()->File_Header(), size);
  i = (IVAR*)(INTPTR) summary_address - ivar_array;
  if (i >= 0 && i < size) {
    Summary_Ivar(fp, i);
    return;
  } 
  SUMMARY_FORMAL* formal_array = IPA_get_formal_array(Cnode());
  size = Summary_Size('F');
  i = (SUMMARY_FORMAL*)(INTPTR) summary_address - formal_array;
  if (i >= 0 && i < size) {
    Summary_Formal(fp, i);
    return;
  } 
  SUMMARY_GLOBAL* global_array = IPA_get_global_array(Cnode());
  size = Summary_Size('G');
  i = (SUMMARY_GLOBAL*)(INTPTR) summary_address - global_array;
  if (i >= 0 && i < size) {
    Summary_Global(fp, i);
    return;
  } 
  SUMMARY_ACTUAL* actual_array = IPA_get_actual_array(Cnode());
  size = Summary_Size('A');
  i = (SUMMARY_ACTUAL*)(INTPTR) summary_address - actual_array;
  if (i >= 0 && i < size) {
    Summary_Actual(fp, i);
    return;
  } 
  SUMMARY_VALUE* value_array = IPA_get_value_array(Cnode());
  size = Summary_Size('V');
  i = (SUMMARY_VALUE*)(INTPTR) summary_address - value_array;
  if (i >= 0 && i < size) {
    Summary_Value(fp, i);
    return;
  } 
  SUMMARY_COMMON* common_array = IPA_get_common_array(Cnode());
  size = Summary_Size('C');
  i = (SUMMARY_COMMON*)(INTPTR) summary_address - common_array;
  if (i >= 0 && i < size) {
    Summary_Common(fp, i);
    return;
  } 
  SUMMARY_COMMON_SHAPE* common_shape_array  
    = IPA_get_common_shape_array(Cnode());
  size = Summary_Size('K');
  i = (SUMMARY_COMMON_SHAPE*)(INTPTR) summary_address - common_shape_array;
  if (i >= 0 && i < size) {
    Summary_Common_Shape(fp, i);
    return;
  } 
  SUMMARY_CONTROL_DEPENDENCE* control_array
    = IPA_get_ctrl_dep_array(Cnode());
  size = Summary_Size('D');
  i = (SUMMARY_CONTROL_DEPENDENCE*)(INTPTR) summary_address - control_array;
  if (i >= 0 && i < size) {
    Summary_Control_Dependence(fp, i);
    return;
  } 
  SUMMARY_CALLSITE* callsite_array = IPA_get_callsite_array(Cnode());
  size = Summary_Size('L');
  i = (SUMMARY_CALLSITE*)(INTPTR) summary_address - callsite_array;
  if (i >= 0 && i < size) {
    Summary_Callsite(fp, i);
    return;
  } 
  CFG_NODE_INFO* cfg_array = IPA_get_cfg_node_array(Cnode());
  size = Summary_Size('a');
  i = (CFG_NODE_INFO*)(INTPTR) summary_address - cfg_array;
  if (i >= 0 && i < size) {
    Summary_Cfg_Node(fp, i);
    return;
  } 
  SCALAR_INFO* scalar_array = IPA_get_scalar_array(Cnode());
  size = Summary_Size('s');
  i = (SCALAR_INFO*)(INTPTR) summary_address - scalar_array;
  if (i >= 0 && i < size) {
    Summary_Scalar(fp, i);
    return;
  } 
  REGION_ARRAYS* region_array = IPA_get_region_array(Cnode());
  size = Summary_Size('R');
  i = (REGION_ARRAYS*)(INTPTR) summary_address - region_array;
  if (i >= 0 && i < size) {
    Summary_Region(fp, i);
    return;
  } 
  PROJECTED_REGION* proj_region_array
    = IPA_get_proj_region_array(Cnode());
  size = Summary_Size('r');
  i = (PROJECTED_REGION*)(INTPTR) summary_address - proj_region_array;
  if (i >= 0 && i < size) {
    Summary_Projected_Region(fp, i);
    return;
  } 
  PROJECTED_NODE* proj_node_array
    = IPA_get_projected_node_array(Cnode());
  size = Summary_Size('n');
  i = (PROJECTED_NODE*)(INTPTR) summary_address - proj_node_array;
  if (i >= 0 && i < size) {
    Summary_Projected_Node(fp, i);
    return;
  } 
  TERM* term_array = IPA_get_term_array(Cnode());
  size = Summary_Size('t');
  i = (TERM*)(INTPTR) summary_address - term_array;
  if (i >= 0 && i < size) {
    Summary_Term(fp, i);
    return;
  } 
  LOOPINFO* loopinfo_array = IPA_get_loopinfo_array(Cnode());
  size = Summary_Size('l');
  i = (LOOPINFO*)(INTPTR) summary_address - loopinfo_array;
  if (i >= 0 && i < size) {
    Summary_Loop_Info(fp, i);
    return;
  } 
  SUMMARY_PROCEDURE* procedure_array = IPA_get_procedure_array(Cnode());
  size = Summary_Size('P');
  i = (SUMMARY_PROCEDURE*)(INTPTR) summary_address - procedure_array;
  if (i >= 0 && i < size) {
    Summary_Procedure(fp, i);
    return;
  } 
  SUMMARY_PHI* phi_array = IPA_get_phi_array(Cnode()); 
  size = Summary_Size('p');
  i = (SUMMARY_PHI*)(INTPTR) summary_address - phi_array; 
  if (i >= 0 && i < size) {
    Summary_Phi(fp, i);
    return; 
  } 
  SUMMARY_CHI* chi_array = IPA_get_chi_array(Cnode());
  size = Summary_Size('x');
  i = (SUMMARY_CHI*)(INTPTR) summary_address - chi_array; 
  if (i >= 0 && i < size) {
    Summary_Chi(fp, i);
    return; 
  } 
  SUMMARY_EXPR* expr_array = IPA_get_expr_array(Cnode());
  size = Summary_Size('E');
  i = (SUMMARY_EXPR*)(INTPTR) summary_address - expr_array; 
  if (i >= 0 && i < size) {
    Summary_Expr(fp, i);
    return; 
  } 
  SUMMARY_STID* stid_array = IPA_get_stid_array(Cnode()); 
  size = Summary_Size('X'); 
  i = (SUMMARY_STID*)(INTPTR) summary_address - stid_array; 
  if (i >= 0 && i < size) {
    Summary_Stid(fp, i);
    return;
  } 
  SUMMARY_STMT* stmt_array = IPA_get_stmt_array(Cnode()); 
  size = Summary_Size('Y'); 
  i = (SUMMARY_STMT*)(INTPTR) summary_address - stmt_array; 
  if (i >= 0 && i < size) {
    Summary_Stmt(fp, i);
    return;
  } 
  SUMMARY_FEEDBACK* feedback_array = IPA_get_feedback_array(Cnode());
  size = Summary_Size('f');
  i = (SUMMARY_FEEDBACK*)(INTPTR) summary_address - feedback_array; 
  if (i >= 0 && i < size) {
    Summary_Feedback(fp, i);
    return; 
  } 
  Error_Cleanup();
} 

