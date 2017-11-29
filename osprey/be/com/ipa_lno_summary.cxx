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


#include <stdio.h>
#include "defs.h"
#include "symtab.h"
#include "ipa_lno_summary.h"
#include "ipa_section.h" 
#include "ipa_lno_file.h"

//-----------------------------------------------------------------------
// NAME: IPA_LNO_SUMMARY_PROCEDURE::IPA_LNO_Print
// FUNCTION: Print the IPA_LNO_SUMMARY_PROCEDURE, assuming that it appears
//   at PROCEDURE index 'procedure_index'. 
//-----------------------------------------------------------------------

void IPA_LNO_SUMMARY_PROCEDURE::Print(FILE* fp,
				      INT procedure_index)
{
  if (procedure_index == -1)
    fprintf(fp, "PROCEDURE: %s", ST_name(St_Idx()));
  else 
    fprintf(fp, "PROCEDURE[%d]: %s ", procedure_index, ST_name(St_Idx()));
  if (Has_Incomplete_Array_Info()) { 
    fprintf(fp, "INCOMPLETE_ARRAY_INFO ");
  } else {
    fprintf(fp, "FORMALS[%d:%d] GLOBALS[%d:%d] \n", 
      _formal_index, _formal_count, _global_index, _global_count);
    fprintf(fp, "  VALUE[%d:%d] EXPR[%d:%d] ", _value_index, _value_count, 
      _expr_index, _expr_count);
  } 
  fprintf(fp, "\n");
}

//-----------------------------------------------------------------------
// NAME: IPA_LNO_SUMMARY_FORMAL::Print
// FUNCTION: Print the IPA_LNO_SUMMARY_FORMAL, assuming that it appears
//   at FORMAL index 'formal_index'. 
//-----------------------------------------------------------------------

void IPA_LNO_SUMMARY_FORMAL::Print(FILE* fp,
				   INT formal_index)
{
  if (formal_index == -1)
    fprintf(stdout, "FORMAL: ");
  else 
    fprintf(stdout, "FORMAL[%d]: ", formal_index);
  if (Is_Scalar()) {
    fprintf(fp, "POSITION(%d) Scalar  MOD[%s] REF[%s] MTYPE(%s)\n", 
      _position, Is_May_Kill() ? "T" : "F", Is_Use() ? "T" : "F", 
      MTYPE_name(_machine_type));
  } else if (Is_Array()) {
    fprintf(fp, "POSITION(%d) Array   MOD[%d] REF[%d] DECL[%d] ",
      _position, _mod_array_section_index, _ref_array_section_index, 
      _decl_array_section_index);
    fprintf(fp, "MTYPE(%s) \n", MTYPE_name(_machine_type));
  } else {
    fprintf(fp, "POSITION(%d) Unknown \n", _position);
  } 
}

//-----------------------------------------------------------------------
// NAME: IPA_LNO_SUMMARY_GLOBAL::Print
// FUNCTION: Print the IPA_LNO_SUMMARY_GLOBAL, assuming that it appears
//   at GLOBAL index 'global_index'. 
//-----------------------------------------------------------------------

void IPA_LNO_SUMMARY_GLOBAL::Print(FILE* fp,
				   INT global_index)
{ 
  if (global_index == -1) 
    fprintf(stdout, "GLOBAL: ");
  else 
    fprintf(stdout, "GLOBAL[%d] ", global_index);
  if (Is_Scalar()) 
    fprintf(fp, "Scalar %s MOD[%s] REF[%s]\n", 
      ST_name(St_Idx()), Is_May_Kill() ? "T" : "F", Is_Use() ? "T" : "F");
  else 
    fprintf(fp, "Array  %s MOD[%d] REF[%d]\n",
      ST_name(St_Idx()), Mod_Array_Section_Index(), Ref_Array_Section_Index());
} 

//-----------------------------------------------------------------------
// NAME: IVAR::IPA_LNO_Print
// FUNCTION: Print the IVAR, assuming it is in "memory" format.
//-----------------------------------------------------------------------

void IVAR::IPA_LNO_Print(FILE* fp,
                         IPA_LNO_READ_FILE* IPA_LNO_File)
{
  if (Is_Formal()) {
    fprintf(fp, "FORMAL_POSITION = %d, OFFSET = %d, MTYPE = %s\n",
            Formal_Position(), Offset(), Machine_Types[Mtype()].name);
  }
  else {
    fprintf(fp, "GLOBAL %s, OFFSET = %d, MTYPE = %s\n",
            ST_name(St_Idx()), Offset(), Machine_Types[Mtype()].name);
  }
}

//-----------------------------------------------------------------------
// NAME: TERM::IPA_LNO_Print
// FUNCTION: Print the TERM, assuming it is in "memory" format.
//-----------------------------------------------------------------------

void TERM::IPA_LNO_Print(FILE* fp,
                         IPA_LNO_READ_FILE* IPA_LNO_File)
{
  switch (Get_type()) {
  case LTKIND_NONE:
    fprintf(fp, "NONE ");
    break;
  case LTKIND_CONST:
    fprintf(fp, "CONST(%d) \n", Get_coeff());
    break;
  case LTKIND_LINDEX:
    fprintf(fp, "LINDEX(%d) * (%d) \n", Get_desc(), Get_coeff());
    break;
  case LTKIND_SUBSCR:
    fprintf(fp, "SUBSCR(%d) * (%d) \n", Get_desc(), Get_coeff());
    break;
  case LTKIND_IV:
    fprintf(fp, "IVAR[%d] * (%d): ", Get_desc(), Get_coeff());
    break;
  default:
    fprintf(fp, "<UNKNOWN> \n");
    break;
  }
}

//-----------------------------------------------------------------------
// NAME: PROJECTED_NODE::IPA_LNO_Print
// FUNCTION: Print the PROJECTED_NODE, assuming it is in "memory" format.
//-----------------------------------------------------------------------

void PROJECTED_NODE::IPA_LNO_Print(FILE* fp,
                                   IPA_LNO_READ_FILE* IPA_LNO_File)
{
  fprintf(fp, "++++++PROJECTED_NODE_START++++++\n");
  if (Is_unprojected()) {
    fprintf(fp, "<UNPROJECTED> \n");
    if (Is_messy_lb()) {
      fprintf(fp, "LB <MESSY> \n");
      LINEX* lx = Get_lower_linex();
      for (INT i = 0; i <= lx->Num_terms(); i++) {
        TERM* tm = lx->Get_term(i);
        tm->IPA_LNO_Print(fp, IPA_LNO_File);
      }
    } else {
      fprintf(fp, "LB: ");
      LINEX* lx = Get_lower_linex();
      for (INT i = 0; i <= lx->Num_terms(); i++) {
        TERM* tm = lx->Get_term(i);
        tm->IPA_LNO_Print(fp, IPA_LNO_File);
      }
    }
  } else {
    if (Is_messy_lb()) {
      fprintf(fp, "LB <MESSY> \n");
      LINEX* lx = Get_lower_linex();
      for (INT i = 0; i <= lx->Num_terms(); i++) {
        TERM* tm = lx->Get_term(i);
        tm->IPA_LNO_Print(fp, IPA_LNO_File);
      }
    } else {
      fprintf(fp, "LB: ");
      LINEX* lx = Get_lower_linex();
      for (INT i = 0; i <= lx->Num_terms(); i++) {
        TERM* tm = lx->Get_term(i);
        tm->IPA_LNO_Print(fp, IPA_LNO_File);
      }
    }
    if (Is_messy_ub()) {
      fprintf(fp, "UB <MESSY> \n");
    } else {
      fprintf(fp, "UB: ");
      LINEX* lx = Get_upper_linex();
      for (INT i = 0; i <= lx->Num_terms(); i++) {
        TERM* tm = lx->Get_term(i);
        tm->IPA_LNO_Print(fp, IPA_LNO_File);
      }
    }
    if (Is_messy_step()) {
      fprintf(fp, "STEP <MESSY> \n");
    } else {
      fprintf(fp, "STEP: ");
      LINEX* lx = Get_step_linex();
      for (INT i = 0; i <= lx->Num_terms(); i++) {
        TERM* tm = lx->Get_term(i);
        tm->IPA_LNO_Print(fp, IPA_LNO_File);
      }
    }
    if (Get_segment_length_linex() != NULL) { 
      fprintf(fp, "STEP: ");
      LINEX* lx = Get_segment_length_linex();
      for (INT i = 0; i <= lx->Num_terms(); i++) {
        TERM* tm = lx->Get_term(i);
        tm->IPA_LNO_Print(fp, IPA_LNO_File);
      }
    }   
    if (Get_segment_stride_linex() != NULL) { 
      fprintf(fp, "STEP: ");
      LINEX* lx = Get_segment_stride_linex();
      for (INT i = 0; i <= lx->Num_terms(); i++) {
        TERM* tm = lx->Get_term(i);
        tm->IPA_LNO_Print(fp, IPA_LNO_File);
      }
    }   
  }
  if (Is_assumed_shape())
    fprintf(fp, "<ASSUMED_SHAPE> \n");
  fprintf(fp, "++++++PROJECTED_NODE_FINISHED++++++\n");
}

//-----------------------------------------------------------------------
// NAME: PROJECTED_REGION::IPA_LNO_Print
// FUNCTION: Print the PROJECTED_REGION, assuming it is in "memory" format.
//-----------------------------------------------------------------------

void PROJECTED_REGION::IPA_LNO_Print(FILE* fp,
                                     IPA_LNO_READ_FILE* IPA_LNO_File)
{
  fprintf(fp, "++++++++PROJECTED_REGION_START+++++++++\n");
  for (INT i = 0; i < Get_num_dims(); i++) {
    PROJECTED_NODE* pn = Get_projected_node(i);
    pn->IPA_LNO_Print(fp, IPA_LNO_File);
  }
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
    fprintf(fp, "ACTUAL_POSITION(%d) ",  Get_actual_id());
  } else if (Is_formal()) {
    fprintf(fp, "FORMAL ");
  }
  fprintf(fp, "\n");
  fprintf(fp, "++++++++PROJECTED_REGION_FINISHED+++++++++\n");
}

