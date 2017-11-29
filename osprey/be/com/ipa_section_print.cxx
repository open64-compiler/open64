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


//* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */
//
// ====================================================================
// ====================================================================
//
// Module: ipa_section_print.cxx
// $Revision: 
// $Date: 
// $Author:
// $Source:
//
// Revision history:
//  30-JAN-97 - Original Version
//
// Description:
//
// This module contains the IPA array section summary 
// file related print routines
//
// ====================================================================
// ====================================================================

#include <stdint.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#include <sys/elf_whirl.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <sys/types.h>

#include "defs.h"
#include "ipa_section.h"
#include "ipl_summary.h"

#ifndef IPA_SUMMARY

#include "ipc_file.h"           // IP_FILE_HDR
#include "ipc_symtab_merge.h"   // Aux_Pu_Table
#include "ipo_defs.h"           // IPA_NODE_CONTEXT
#include "ipa_section_main.h"   // IPA_Ivar_Count, IPA_Ivar_Global_Count


// file static arrays used to get access to summary 
// information when print methods are invoked from IPA
static SUMMARY_SYMBOL* IPA_Symbol = NULL;

void
Init_IPA_Print_Arrays (IPA_NODE* node)
{
  IPA_Symbol = IPA_get_symbol_array (node);
  IPA_get_ivar_array (node, IPA_Ivar_Count);
}

static char* 
Symbol_Name(SUMMARY_SYMBOL* ss)
{
  if (ST_IDX_level(ss->St_idx()) > GLOBAL_SYMTAB) {
    ST_IDX func_st_idx = ss->Get_st_idx_func();
    PU_IDX pu_idx = ST_pu(&St_Table[func_st_idx]);
    NODE_INDEX node_index = AUX_PU_node(Aux_Pu_Table[pu_idx]);
    IPA_NODE* cnode = IPA_Call_Graph->Graph()->Node_User(node_index);
    IPA_NODE_CONTEXT context(cnode);
    return ST_name(ss->St_idx());
  } 
  else {
    return ST_name(St_Table[ss->St_idx()]);
  }
}

#else

#include "ipl_summarize.h"

extern SUMMARY *Summary;
extern IVAR_ARRAY *Ivar;

#endif

//===================================================================
//      Print the loop info
//===================================================================
void LOOPINFO::Print_file(FILE *fp)
{
  if (Is_messy_ub())
    fprintf(fp, "messy upper, ");
  else
    fprintf(fp, "not messy upper, ");
  
  if (Is_messy_lb())
    fprintf(fp, "messy lower, ");
  else
    fprintf(fp, "not messy lower, ");

  if (Is_messy_step())
    fprintf(fp, "messy step, ");
  else
    fprintf(fp, "not messy step, ");
  
  if (Is_messy_bounds())
    fprintf(fp, "messy bounds \n");

  fprintf(fp, "nest-level = %d \n", Get_nest_level());

  fprintf(fp, "Lower bound terms: term array index = %d , count = %d \n", 
          Get_lb_term_index(), Get_lb_term_count());

  fprintf(fp, "Upper bound terms: term array index = %d , count = %d \n", 
          Get_ub_term_index(), Get_ub_term_count());

  fprintf(fp, "Step bound terms: term array index = %d , count = %d \n", 
          Get_step_term_index(), Get_step_term_count());          

}

//===================================================================
//      Print the region arrays information as written out to file, with
//      the index, size information
//===================================================================
void
REGION_ARRAYS::Print_file(FILE* fp)
{
  if (Is_bad_alias())
    fprintf(fp, "Has bad alias, ");
  if (Is_loop_invariant())
    fprintf(fp, "Is loop invariant, ");
  if (Is_use())
    fprintf(fp, "Is_use, ");
  else if (Is_def())
    fprintf(fp, "Is_def, ");
  else if (Is_passed())
    fprintf(fp, "Is_passed, ");
  else if (Is_may_def())
    fprintf(fp, "Is_may_def, ");
  else if (Is_may_use())
    fprintf(fp, "Is_may_use, ");
  else if (Is_formal())
    fprintf(fp, "Is_formal, ");
  else
    fprintf(fp, "UNKNOWN type. ");

  fprintf(fp, "\nSymbol id = %d \n", _sym_index);
  fprintf(fp, "Element size = %d \n", _element_size);

  fprintf(fp, "Id into PROJECTED_REGION_ARRAY = %d, count = %d \n", 
          Get_idx(), Get_count());
}

//===================================================================
//      Print the projected node information as written out to file, with
//      index and size information
//===================================================================
void
PROJECTED_NODE::Print_file(FILE *fp)
{
  fprintf(fp, "%smessy lower, ", Is_messy_lb() ? "" : "non-");
  fprintf(fp, "%smessy upper, ", Is_messy_ub() ? "" : "non-");
  fprintf(fp, "%smessy step, ", Is_messy_step() ? "" : "non-");
  fprintf(fp, "%sprojected", Is_unprojected() ? "NOT" : "");
  if (Is_assumed_shape()) 
    fprintf(fp, ", assumed_shape");
  fprintf(fp, "\n terms in lower: idx = %d, count = %d\n", 
          Get_lb_term_index(), Get_lb_term_count());
  fprintf(fp, " terms in upper: idx = %d, count = %d\n", 
          Get_ub_term_index(), Get_ub_term_count());
  fprintf(fp, " terms in step:  idx = %d, count = %d \n", 
          Get_step_term_index(), Get_step_term_count());
  if (Get_segment_length_term_count() > 0)
    fprintf(fp, " terms in segment length: idx = %d, count = %d\n", 
            Get_segment_length_term_index(), Get_segment_length_term_count());
  if (Get_segment_stride_term_count() > 0)
    fprintf(fp, " terms in segment stride: idx = %d, count = %d\n", 
            Get_segment_stride_term_index(), Get_segment_stride_term_count());
}

//===================================================================
//      Print the control flow information as written out to file, with
//      index and size information
//===================================================================
void
CFG_NODE_INFO::Print_file(FILE *fp)
{
  if (Is_do_loop()) {
    fprintf(fp, "TYPE: CFG_DO_LOOP, loopinfo index = %d", Get_loop_index());
    if (Is_executed()) {
      fprintf(fp, "ALWAYS executed");
    }
    fprintf(fp, "\n");
  }
  else if (Is_if()) {
    fprintf(fp, "TYPE: CFG_IF\n");
    fprintf(fp, "ELSE cfg node index = %d \n", Get_else_index());
  }
  else if (Is_else()) {
    fprintf(fp, "TYPE: CFG_ELSE \n");
    fprintf(fp, "IF cfg node index = %d \n", Get_if_index());
  }
  else if (Is_entry()) {
    fprintf(fp, "TYPE: CFG_ENTRY \n");
  }
  else {
    fprintf(fp, "TYPE: UNKNOWN \n");
  }

  fprintf(fp, "Control Dependence Index: %d\n", Get_cd_index());

  if (Has_calls()) {
    fprintf(fp, "Has calls \n");
  }

  // print all the array kills
  fprintf(fp,"array kills: offset into REGIONS_ARRAY = %d, count = %d\n", 
          Get_def_index(), Get_def_count());

  // print all the array uses
  fprintf(fp, "array uses: offset into REGIONS_ARRAY = %d, count = %d\n", 
          Get_use_index(), Get_use_count());

  // print all the parameter array sections
  fprintf(fp,"array params: offset into REGIONS_ARRAY = %d, count = %d\n",
          Get_param_index(), Get_param_count());

  // print all the formal array sections
  fprintf(fp,"array formals: offset into REGIONS_ARRAY = %d, count = %d\n",
          Get_formal_index(), Get_formal_count());

  // print all the scalar 
  fprintf(fp, "scalar info: offset into INT_ARRAY = %d, count = %d \n", 
          Get_scalar_index(), Get_scalar_count());
}

//===================================================================
//      Print the projected region information as written out to file, with
//      index and size information
//===================================================================
void 
PROJECTED_REGION::Print_file(FILE *fp)
{
  if (Is_messy_region())
    fputs("messy region", fp);
  else
    fputs("non-messy region", fp);

  if (Is_unprojected_region())
    fputs(", unprojected region, ", fp);
  else
    fputs(", projected region, ", fp);

  fprintf(fp, "num dims = %d, depth = %d \n", Get_num_dims(), Get_depth());

  if (Is_passed())
    fprintf(fp, "parameter array section, callsite_id = %d, actual_pos = %d\n",
            Get_callsite_id(), Get_actual_id());

  fprintf(fp, "id into projected node array = %d \n", Get_id());
}

//====================================================================
//       Print the linex array, which is simply a linear expression 
//====================================================================
void
LINEX::Print_file(FILE* fp)
{
  for (INT i = 0; i < _larray.Elements(); ++i) {
    _larray[i].Print_file(fp);
  }
}

//====================================================================
//      Print a linex array, which is simply a linear expression
//====================================================================
void
LINEX::Print(FILE *fp)
{
#ifdef IPA_SUMMARY
  if (Trace_Sections) {
    if (_larray.Lastidx() == -1) { 
      fprintf(TFile, "NULL linex \n");
      fprintf(stdout, "NULL linex \n");
    } 
  }
#endif
  for (INT i = 0; i <= _larray.Lastidx(); ++i) {
    _larray[i].Print(fp, FALSE);
    if (i < _larray.Lastidx()) {
      fprintf(fp, "+");
    }
  }
}

//
//====================================================================
//       Print an ivar
// ====================================================================
void 
IVAR::Print (FILE* fp)
{
  fprintf(fp, "IVAR: ");
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
// NAME: IVAR::IPA_LNO_Print_File
// FUNCTION: Print the IVAR at index 'ivar_index' to file 'fp' in a format 
//   compatible for the IPA_LNO_WRITE trace. 
//-----------------------------------------------------------------------

void IVAR::IPA_LNO_Print_File(FILE* fp, INT ivar_index)
{
  fprintf(fp, "IVAR");
  if (ivar_index == -1) {
    fprintf(fp, ":");
  }
  else {
    fprintf(fp, "[%d]:", ivar_index);
  }
  if (Is_Formal()) {
    fprintf(fp, "FORMAL_POSITION = %d, OFFSET = %d, MTYPE = %s\n",
            Formal_Position(), Offset(), Machine_Types[Mtype()].name);
  }
  else {
    fprintf(fp, "GLOBAL %s, OFFSET = %d, MTYPE = %s\n",
            ST_name(St_Idx()), Offset(), Machine_Types[Mtype()].name);
  }
} 

//====================================================================
//       Print a TERM of a linex array
// ====================================================================
void
TERM::Print(FILE *fp, BOOL newline)
{
  switch (Get_type()) {
  
  case LTKIND_NONE:
    fprintf(fp,"?");
    break;

  case LTKIND_CONST:
    fprintf(fp,"%d", Get_coeff());
    break;

  case LTKIND_LINDEX:
    fprintf(fp,"loop_index(%d)*%d", Get_desc(), Get_coeff());
    break;

  case LTKIND_SUBSCR:
    fprintf(fp,"dim(%d)*%d", Get_desc(), Get_coeff());
    break;

  case LTKIND_IV:
    fprintf(fp,"IVAR[%d]*%d", Get_desc(), Get_coeff());
    break;
  }
  
  if (newline) {
    fprintf(fp, "\n");
  }
}

// ====================================================================
//      Print a linex term to a file
// ====================================================================
void 
TERM::Print_file (FILE* fp)
{
  switch (Get_type())
    {
    case LTKIND_NONE:
      fprintf(fp,"unknown TERM\n");
      break;

    case LTKIND_CONST:
      fprintf(fp,"CONST:+ %d\n", Get_coeff());
      break;

    case LTKIND_LINDEX:
      fprintf(fp,"LINDEX: + loop_index(%d)*%d\n", Get_desc(), Get_coeff());
      break;

    case LTKIND_SUBSCR:
      fprintf(fp,"SUBSCR: + dim(%d)*%d\n", Get_desc(), Get_coeff());
      break;

    case LTKIND_IV:
      fprintf(fp,"IVAR: + IVAR[%d]*%d\n", Get_desc(), Get_coeff());
      break;

    default:
      fprintf(fp, "UNKNOWN TYPE:%d  something is wrong\n", Get_type());
      break;
    }
}

//-----------------------------------------------------------------------
// NAME: TERM::IPA_LNO_Print_File
// FUNCTION: Print the TERM at index 'term_index' to file 'fp' in a format 
//   compatible for the IPA_LNO_WRITE trace. 
//-----------------------------------------------------------------------

void TERM::IPA_LNO_Print_File(FILE* fp, 
			      INT term_index)
{ 
  if (term_index == -1)
    fprintf(fp, "TERM: ");
  else 
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

//====================================================================
//       Print the information written out to file
//====================================================================
void 
SCALAR_INFO::Print_file(FILE *fp)
{
  fprintf(fp, "SCALAR_INFO: ");
  fprintf(fp, "id:%d, call_id:%d, TYPE: ", Get_id(), Get_callsite_id());
  if (Is_may_kill())
    fprintf(fp, "may kill: ");
  if (Is_may_use())
    fprintf(fp, "may use: ");
  if (Is_may_reduc())
    fprintf(fp, "may reduc: ");
  if (Is_kill())
    fprintf(fp, "kill: ");
  if (Is_use())
    fprintf(fp, "use: ");
  if (Is_euse())
    fprintf(fp, "euse: ");
  if (Is_call_euse())
    fprintf(fp, "call euse: ");
  if (Is_reduc())
    fprintf(fp, "reduc: ");
  if (Is_array_reduc())
    fprintf(fp, "array reduc: ");
  if (Is_array_may_reduc())
    fprintf(fp, "may array reduc: ");
  if (Is_passed_ref())
    fprintf(fp, "passed ref: ");
  if (Is_may_passed_ref())
    fprintf(fp, "may passed ref: ");
  fprintf(fp, "\n");
}
//===================================================================
//      Print the projected kernel
//===================================================================
void PROJECTED_KERNEL::Print(FILE *fp)
{
  INT i;

  fprintf(fp, "+++++PROJECTED_KERNEL_START+++++\n");

  if (Is_messy_kernel())
    fprintf(fp, "messy, ");
  else
    fprintf(fp, "non-messy, ");

  if (Is_projected())
    fprintf(fp, "projected, ");
  else
    fprintf(fp, "unprojected, ");

  fprintf(fp, "num dims = %d, ", Get_num_dims());
  fprintf(fp, "depth = %d, ", Get_depth());
  fprintf(fp, "proj level = %d ", Get_projected_level());
  fprintf(fp, "\n");

  // check for is independent
  fprintf(fp, "is_independent = [");
  for (i = 0; i < Get_depth(); i++) {
    if (Is_independent(i))
      fprintf(fp, "TRUE");
    else 
      fprintf(fp, "FALSE");
    if (i < Get_depth() - 1)
      fprintf(fp, ",");
  } 
  fprintf(fp, "]\n");
  for (i=0; i < Get_num_dims(); ++i) {
    LINEX *l;
    if (l = Get_linex(i)) {
      fprintf(fp, "input_linex[%d] = ", i);
      l->Print(fp);
      fprintf(fp, "\n");
    }
  }
  if (Get_region() != NULL)
    fprintf(fp, "region = 0x%p\n", Get_region());
  for (i = 0; i < Get_num_dims(); i++) {
    LINEX *l;
    if (l = Get_Difference(i)) { 
      fprintf(fp, "difference[%d] = ", i);
      l->Print(fp); 
      fprintf(fp, "\n");
    }
  }
  fprintf(fp, "+++++PROJECTED_KERNEL_FINISHED+++++\n");
}

//===================================================================
//      Print the loop info
//===================================================================
void LOOPINFO::Print(FILE *fp)
{
  LINEX *l;
  INT i;

  fprintf(fp, "\n+++++LOOPINFO_START+++++\n");
  if (Is_messy_ub())
    fprintf(fp, "messy upper, ");
  else
    fprintf(fp, "not messy upper, ");
  
  if (Is_messy_lb())
    fprintf(fp, "messy lower, ");
  else
    fprintf(fp, "not messy lower, ");

  if (Is_messy_step())
    fprintf(fp, "messy step, ");
  else
    fprintf(fp, "not messy step, ");
  
  if (Is_messy_bounds())
    fprintf(fp, "messy bounds \n");

  fprintf(fp, "nest-level = %d\n", Get_nest_level());

  l = Get_lower_linex();
  if (l)
    {
      fprintf(fp, "\n Lower bound linex \n");
      l->Print(fp);
    }
  else
    fprintf(fp, "\n NULL lower bound linex \n");


  l = Get_upper_linex();
  if (l) 
    {
      fprintf(fp, "\n Upper bound linex \n");
      l->Print(fp);
    }
  else
    fprintf(fp, "\n NULL upper bound linex \n");


  l = Get_step_linex();
  if (l)
    {
      fprintf(fp, "\n Step linex \n");
      l->Print(fp);
    }
  else
    fprintf(fp, "\n NULL step linex \n");

  if (Get_kernels() != NULL)
    {
      PROJECTED_KERNEL_ARRAY *k = Get_kernels();
      for (i=0; i<k->Lastidx();++i)
	{
	  PROJECTED_KERNEL* kernel = &(*k)[i];
	  kernel->Print(fp);
	}
    }

  fprintf(fp, "+++++LOOPINFO_FINISHED+++++\n");
}


//===================================================================
//       Print the projected node
//===================================================================
void PROJECTED_NODE::Print(FILE* fp)
{
  LINEX* l;

  fprintf(fp, "[");

  if (Is_messy_lb()) {
    fprintf(fp, "messy");
  }
  else if (l = Get_lower_linex()) {
    l->Print(fp);
  }
  else {
    fprintf(fp, "?");
  }

  fprintf(fp, ":");  

  if (Is_messy_ub()) {
    fprintf(fp, "messy");
  }
  else if (l = Get_upper_linex()) {
    l->Print(fp);
  }
  else {
    fprintf(fp, "?");
  }

  fprintf(fp, ":");  

  if (Is_messy_step()) {
    fprintf(fp, "messy");
  }
  else if (l = Get_step_linex()) {
    l->Print(fp);
  }
  else {
    fprintf(fp, "?");
  }

  if (Get_segment_length_linex() && Get_segment_stride_linex()) {
    fprintf(fp, ":");
    Get_segment_length_linex()->Print(fp);
    fprintf(fp, ":");
    Get_segment_stride_linex()->Print(fp);
  }

  fprintf(fp, "]");  
  fflush(fp);
}

//-----------------------------------------------------------------------
// NAME: PROJECTED_NODE::IPA_LNO_Print_File
// FUNCTION: Print the PROJECTED_NODE at index 'pn_index' to file 'fp' 
//   in a format compatible for the IPA_LNO_WRITE trace. 
//-----------------------------------------------------------------------

void PROJECTED_NODE::IPA_LNO_Print_File(FILE* fp, 
			                INT pn_index)
{ 
  if (pn_index == -1)
    fprintf(fp, "PROJ_NODE: ");
  else 
    fprintf(fp, "PROJ_NODE[%d]: ", pn_index);
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
    fprintf(fp, "\n  ");
    fprintf(fp, "SEG LEN TERM[%d:%d] ", Get_segment_length_term_index(),
      Get_segment_length_term_count());
    fprintf(fp, "SEG STR TERM[%d:%d] ", Get_segment_stride_term_index(),
      Get_segment_stride_term_count());
  }
  if (Is_assumed_shape())
    fprintf(fp, "<ASSUMED_SHAPE> ");
  fprintf(fp, "\n");
} 

//===================================================================
//      Print the projected region
//===================================================================
void PROJECTED_REGION::Print(FILE *fp)
{
  if (Is_unprojected_region()) {
    fprintf(fp, "UN-");
  }
  fprintf(fp, "PROJECTED_REGION: ");

  if (Is_messy_region()) {
    fprintf(fp, "messy region");
  }
  else {
    PROJECTED_ARRAY* pa = Get_projected_array();
    if (pa && pa != (void*)-1) {
      for (INT i = 0; i < Get_num_dims(); ++i) {
        Get_projected_node(i)->Print(fp);
      } 
    }
  }

  fprintf(fp, "  depth = %d\n", Get_depth());

#ifdef IPA_SUMMARY  
  if (Is_passed())
    fprintf(fp, "parameter array section, callsite_id = %d, actual_pos = %d\n",
            Get_callsite_id(), Get_actual_id());
  fprintf(fp, "\n");

// projected kernel exists in the case of the summary phase
  PROJECTED_KERNEL* p;
  if (p = Get_projected_kernel()) 
    p->Print(fp);
  fprintf(fp, "\n");
#endif
}

//-----------------------------------------------------------------------
// NAME: PROJECTED_REGION::IPA_LNO_Print_File
// FUNCTION: Print the PROJECTED_REGION at index 'pr_index' to file 'fp' 
//   in a format compatible for the IPA_LNO_WRITE trace. 
//-----------------------------------------------------------------------

void PROJECTED_REGION::IPA_LNO_Print_File(FILE* fp, 
				          INT pr_index)
{
  if (pr_index == -1)
    fprintf(fp, "PROJ_REGION: ");
  else 
    fprintf(fp, "PROJ_REGION[%d]: ", pr_index);
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

//===================================================================
//      Print the projected region 
//===================================================================
void 
PROJECTED_REGION_INFO::Print(FILE *fp)
{
  if (Get_projected_region())
    Get_projected_region()->Print(fp);
}

//===================================================================
// Print the region arrays
//===================================================================
void 
REGION_ARRAYS::Print(FILE *fp)
{
  fprintf(fp, "REGION_ARRAYS for ");
#ifdef IPA_SUMMARY
  fprintf(fp, "%s", Summary->Get_symbol(Get_sym_id())->Get_Name());
#else
  fprintf(fp, "%s", Symbol_Name(&IPA_Symbol[Get_sym_id()]));
#endif

  fprintf(fp, ", element size = %d, ", Get_element_size());

  if (Is_bad_alias())
    fprintf(fp, "bad alias, ");

  if (Is_loop_invariant())
    fprintf(fp, "loop invariant, ");

  if (Is_def())
    fprintf(fp, "def\n");
  else if (Is_use())
    fprintf(fp, "use\n");
  else if (Is_passed())
    fprintf(fp, "passed\n");
  else if (Is_may_def())
    fprintf(fp, "may def\n");
  else if (Is_may_use())
    fprintf(fp, "may use\n");
  else if (Is_formal())
    fprintf(fp, "formal\n");
  else
    fprintf(fp, "UNKNOWN type\n");

  if (PROJECTED_REGION_INFO_ARRAY* p = Get_projected_region_array()) {
    for (INT i = 0; i < p->Elements(); ++i) {
      (*p)[i].Print(fp);
    }
  }
}

//===================================================================
// Print the control flow node information
//===================================================================
void 
CFG_NODE_INFO::Print(FILE *fp)
{
  if (Has_calls())
    fprintf(fp, "Has calls \n");

  INT i;

  // print all the array kills
  fprintf(fp," +++++++++ARRAY KILLS_START+++++++++\n"); 
  ARRAY_OF_REGION_ARRAYS* defs = Get_def_array();
  for (i = 0; i < defs->Elements(); ++i) {
    (*defs)[i].Print(fp);
  }  
  fprintf(fp," +++++++++ARRAY KILLS_FINISHED+++++++++\n");

  // print all the array uses
  ARRAY_OF_REGION_ARRAYS* uses = Get_use_array();
  fprintf(fp," +++++++++ARRAY EUSES_START+++++++++\n");
  for (i = 0 ; i < uses->Elements(); ++i) {
    (*uses)[i].Print(fp);
  }
  fprintf(fp," +++++++++ARRAY EUSES_FINISHED+++++++++\n");

  // print all the scalar kills
  fprintf(fp," +++++++++SCALAR_INFO_START+++++++++\n");
  INT_ARRAY* scalar_defs = Get_scalar_def_array();
  for (i = 0; i < scalar_defs->Elements();++i) {
    SCALAR_INFO* idr = &(*scalar_defs)[i];
#ifdef IPA_SUMMARY
    fprintf(fp, "symbol: %s\n", Summary->Get_symbol(idr->Get_id())->Get_Name());
#else
    fprintf(fp, "symbol: %s\n", Symbol_Name(&IPA_Symbol[idr->Get_id()]));
#endif
  }
  fprintf(fp," +++++++++SCALAR_INFO_FINISHED+++++++++\n");
}
