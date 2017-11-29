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
#include "tlog.h" 
#include "ipa_summary.h"
#include "ipc_symtab_merge.h"           // IPC_GLOBAL_IDX_MAP

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Ivar_Name
// FUNCTION: Returns the name of the 'i'th independent variable. 
//-----------------------------------------------------------------------

char* CG_BROWSER::Ivar_Name(INT i)
{
  INT32 ivar_array_size;
  IVAR* ivar = IPA_get_ivar_array(Cnode(), ivar_array_size);
  FmtAssert(0 <= i && i < ivar_array_size,
            ("CG_BROWSER::Ivar_Name: Index out of range"));
  if (ivar[i].Is_Formal()) {
    SUMMARY_FORMAL* formals = IPA_get_formal_array(Cnode());
    INT32 formal_index = Cnode()->Summary_Proc()->Get_formal_index()
                       + ivar[i].Formal_Position();
    INT32 symbol_index = formals[formal_index].Get_symbol_index();
    return Symbol_Name(symbol_index); 
  }
  else {
    return ST_name(ivar[i].St_Idx());
  }
}

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Print_Linex
// FUNCTION: Print the TERM 'tm'. 
//-----------------------------------------------------------------------

INT CG_BROWSER::Print_Linex(char* cp, 
			    INT cc, 
			    LINEX* lx)
{
  INT term_count = 0;
  INT i;

  for (i = 0; i <= lx->Num_terms(); i++) {
    TERM* tm = lx->Get_term(i);
    if (tm->Get_type() == LTKIND_LINDEX) {
      if (tm->Get_coeff() == 1)
	cc += sprintf(cp + cc, "%s%%%d", term_count > 0 ? "+" : "", 
	  tm->Get_desc());
      else if (tm->Get_coeff() > 0)
	cc += sprintf(cp + cc, "%s%d*%d", term_count > 0 ? "+" : "", 
	  tm->Get_coeff(), tm->Get_desc());
      else if (tm->Get_coeff() == -1)
	cc += sprintf(cp + cc, "-%d", tm->Get_desc());
      else if (tm->Get_coeff() < 0)
	cc += sprintf(cp + cc, "%d*%d", tm->Get_coeff(), tm->Get_desc());
      if (tm->Get_coeff() != 0)
	term_count++; 
    }
  }
  for (i = 0; i <= lx->Num_terms(); i++) {
    TERM* tm = lx->Get_term(i);
    if (tm->Get_type() == LTKIND_SUBSCR) {
      if (tm->Get_coeff() == 1)
	cc += sprintf(cp + cc, "%s$%d", term_count > 0 ? "+" : "", 
	  tm->Get_desc());
      else if (tm->Get_coeff() > 0)
	cc += sprintf(cp + cc, "%s%d*$%d", term_count > 0 ? "+" : "", 
          tm->Get_coeff(), tm->Get_desc());
      else if (tm->Get_coeff() == -1)
	cc += sprintf(cp + cc, "-$%d", tm->Get_desc());
      else if (tm->Get_coeff() < 0)
	cc += sprintf(cp + cc, "%d*$%d", tm->Get_coeff(), tm->Get_desc());
      if (tm->Get_coeff() != 0)
	term_count++; 
    } 
  }    
  for (i = 0; i <= lx->Num_terms(); i++) {
    TERM* tm = lx->Get_term(i);
    if (tm->Get_type() == LTKIND_IV) {
      if (tm->Get_coeff() == 1)
	cc += sprintf(cp + cc, "%s%s", term_count > 0 ? "+" : "", 
	  Ivar_Name(tm->Get_desc()));
      else if (tm->Get_coeff() > 0)
	cc += sprintf(cp + cc, "%s%d*%s", term_count > 0 ? "+" : "", 
          tm->Get_coeff(), Ivar_Name(tm->Get_desc()));
      else if (tm->Get_coeff() == -1)
	cc += sprintf(cp + cc, "-%s", Ivar_Name(tm->Get_desc()));
      else if (tm->Get_coeff() < 0)
	cc += sprintf(cp + cc, "%d*%s", tm->Get_coeff(), 
	  Ivar_Name(tm->Get_desc()));
      if (tm->Get_coeff() != 0)
	term_count++; 
    } 
  }    
  INT const_value = 0;
  for (i = 0; i <= lx->Num_terms(); i++) {
    TERM* tm = lx->Get_term(i);
    if (tm->Get_type() == LTKIND_CONST) 
      const_value += tm->Get_coeff();
  } 
  if (term_count == 0 || const_value != 0) {
    if (const_value > 0 && term_count > 0)
      cc += sprintf(cp + cc, "+%d", const_value);
    else 
      cc += sprintf(cp + cc, "%d", const_value);
  }
  return cc;
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Print_Projected_Node_Lower_Bound 
// FUNCTION: Print the lower bound of the PROJECTED_NODE 'pn'. 
//-----------------------------------------------------------------------

INT CG_BROWSER::Print_Projected_Node_Lower_Bound(char* cp, 
						 INT cc, 
						 PROJECTED_NODE* pn)
{
  LINEX* lx_lower = pn->Get_lower_linex(); 
  if (pn->Is_messy_lb()) {
    cc += sprintf(cp + cc, "<MESSY>");
  } else { 
    cc = Print_Linex(cp, cc, lx_lower);
  } 
  return cc; 
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Print_Projected_Node_Upper_Bound 
// FUNCTION: Print the upper bound of the PROJECTED_NODE 'pn'. 
//-----------------------------------------------------------------------

INT CG_BROWSER::Print_Projected_Node_Upper_Bound(char* cp, 
						 INT cc, 
						 PROJECTED_NODE* pn)
{
  LINEX* lx_upper = pn->Get_upper_linex(); 
  if (pn->Is_messy_ub()) {
    cc += sprintf(cp + cc, "<MESSY>");
  } else { 
    cc = Print_Linex(cp, cc, lx_upper);
  } 
  return cc;
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Print_Projected_Node_Step
// FUNCTION: Print the step of the PROJECTED_NODE 'pn'. 
//-----------------------------------------------------------------------

INT CG_BROWSER::Print_Projected_Node_Step(char* cp, 
					  INT cc, 
					  PROJECTED_NODE* pn)
{ 
  LINEX* lx_step = pn->Get_step_linex();
  if (pn->Is_messy_step()) {
    cc += sprintf(cp + cc, "<MESSY>");
  } else { 
    cc = Print_Linex(cp, cc, lx_step);
  } 
  return cc;
}

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Print_Projected_Node_Segment_Length
// FUNCTION: Print the step of the PROJECTED_NODE 'pn'. 
//-----------------------------------------------------------------------

INT CG_BROWSER::Print_Projected_Node_Segment_Length(char* cp, 
					            INT cc, 
					            PROJECTED_NODE* pn)
{ 
  LINEX* lx_segment_length = pn->Get_segment_length_linex();
  cc = Print_Linex(cp, cc, lx_segment_length);
  return cc;
}

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Print_Projected_Node_Segment_Stride
// FUNCTION: Print the step of the PROJECTED_NODE 'pn'. 
//-----------------------------------------------------------------------

INT CG_BROWSER::Print_Projected_Node_Segment_Stride(char* cp, 
					            INT cc, 
					            PROJECTED_NODE* pn)
{ 
  LINEX* lx_segment_stride = pn->Get_segment_stride_linex();
  cc = Print_Linex(cp, cc, lx_segment_stride);
  return cc;
}

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Print_Projected_Node
// FUNCTION: Print the PROJECTED_NODE 'pn'. 
//-----------------------------------------------------------------------

INT CG_BROWSER::Print_Projected_Node(char* cp, 
				     INT cc, 
				     PROJECTED_NODE* pn)
{ 
  cc = Print_Projected_Node_Lower_Bound(cp, cc, pn);
  cc += sprintf(cp + cc, ":"); 
  cc = Print_Projected_Node_Upper_Bound(cp, cc, pn);
  cc += sprintf(cp + cc, ":"); 
  cc = Print_Projected_Node_Step(cp, cc, pn);
  if (pn->Get_segment_length_linex() > 0) {
    cc += sprintf(cp + cc, ":"); 
    cc = Print_Projected_Node_Segment_Length(cp, cc, pn);
  } 
  if (pn->Get_segment_stride_linex() > 0) {
    cc += sprintf(cp + cc, ":"); 
    cc = Print_Projected_Node_Segment_Stride(cp, cc, pn);
  } 
  return cc;
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Print_Projected_Array
// FUNCTION: Print the PROJECTED_ARRAY 'pa'. 
//-----------------------------------------------------------------------

INT CG_BROWSER::Print_Projected_Array(char* cp, 
				      INT cc, 
				      PROJECTED_ARRAY *pa)
{ 
  for (INT i = 0; i <= pa->Lastidx(); i++) { 
    cc += sprintf(cp + cc, "["); 
    PROJECTED_NODE* pn = &(*pa)[i];
    cc = Print_Projected_Node(cp, cc, pn); 
    cc += sprintf(cp + cc, "]"); 
  }
  return cc;
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Print_Projected_Region
// FUNCTION: Print the PROJECTED_ARRAY 'pr'. 
//-----------------------------------------------------------------------

INT CG_BROWSER::Print_Projected_Region(char* cp, 
				       INT cc, 
				       PROJECTED_REGION *pr)
{ 
  if (pr->Is_messy_region()) {
    cc += sprintf(cp + cc, "[<MESSY REGION>]");
  } else { 
    cc = Print_Projected_Array(cp, cc, pr->Get_projected_array());
  } 
  return cc;
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Tlog_Mod_Ref_Formals
// FUNCTION: Print mod/ref information for formals at current node to 
//   Tlog file.
//-----------------------------------------------------------------------

void CG_BROWSER::Tlog_Mod_Ref_Formals()
{ 
  INT cc = 0;
  char buffer[CGB_MAX_STRING_LENGTH];
  IPA_NODE_SECTION_INFO* ipas = Cnode()->Section_Annot();
  IPAA_NODE_INFO* ipaa = Cnode()->Mod_Ref_Info();
  INT formal_count = ipas->Get_formal_count();
  if (formal_count > 0) {
    for (INT i = 0; i < formal_count; i++) {
      cc += sprintf(buffer + cc, "F#%d: ", i);
      STATE* ipaf = ipas->Get_formal(i);
      if (ipaf->Is_scalar()) {
        BOOL indent = FALSE;
	if (ipaa != NULL) { 
	  if (ipaa->Is_formal_dmod_elmt(i) || ipaa->Is_formal_imod_elmt(i)) {
	    cc += sprintf(buffer + cc, "MOD ");
	    cc += sprintf(buffer + cc, "%s", Formal_Name(i));
	    indent = TRUE;
	  }
	  if (ipaa->Is_formal_dref_elmt(i) || ipaa->Is_formal_iref_elmt(i)) {
	    if (indent)
	      cc += sprintf(buffer + cc, "     ");
	    cc += sprintf(buffer + cc, "REF ");
	    cc += sprintf(buffer + cc, "%s", Formal_Name(i));
	    indent = TRUE;
	  }
        }
        if (!indent)
          cc += sprintf(buffer + cc, "    %s", Formal_Name(i));
        if (cc >= CGB_MAX_STRING_LENGTH - 1)
	  buffer[CGB_MAX_STRING_LENGTH - 1] = '\0';
	Generate_Tlog("IPA", "Array_Section", (SRCPOS) 0, "", "", "", 
	  buffer);
	cc = 0; 
      } else {
        BOOL indent = FALSE;
        if (ipaf->Get_projected_mod_region() != NULL) {
          cc += sprintf(buffer + cc, "MOD ");
          cc += sprintf(buffer + cc, "%s", Formal_Name(i));
          cc = Print_Projected_Region(buffer, cc, 
	    ipaf->Get_projected_mod_region());
          indent = TRUE;
          if (cc >= CGB_MAX_STRING_LENGTH - 1)
	    buffer[CGB_MAX_STRING_LENGTH - 1] = '\0';
	  Generate_Tlog("IPA", "Array_Section", (SRCPOS) 0, "", "", "", 
	    buffer);
	  cc = 0; 
        }
        if (ipaf->Get_projected_ref_region() != NULL) {
          if (indent)
            cc += sprintf(buffer + cc, "     ");
          cc += sprintf(buffer + cc, "REF ");
          cc += sprintf(buffer + cc, "%s", Formal_Name(i));
          cc = Print_Projected_Region(buffer, cc,  
            ipaf->Get_projected_ref_region());
          if (cc >= CGB_MAX_STRING_LENGTH - 1)
	    buffer[CGB_MAX_STRING_LENGTH - 1] = '\0';
          indent = TRUE;
	  Generate_Tlog("IPA", "Array_Section", (SRCPOS) 0, "", "", "", 
	    buffer);
	  cc = 0; 
        }
/* NEED TO OMIT THIS UNTIL TLOGS ARE UPDATED TO INCLUDE DECLARATIONS
        if (ipaf->Get_projected_dcl_region() != NULL) {
          if (indent)
            cc += sprintf(buffer + cc, "     ");
          cc += sprintf(buffer + cc, "DCL ");
          cc += sprintf(buffer + cc, "%s", Formal_Name(i));
          cc = Print_Projected_Region(buffer, cc,  
            ipaf->Get_projected_dcl_region());
          if (cc >= CGB_MAX_STRING_LENGTH - 1)
	    buffer[CGB_MAX_STRING_LENGTH - 1] = '\0';
          indent = TRUE;
	  Generate_Tlog("IPA", "Array_Section", (SRCPOS) 0, "", "", "", 
	    buffer);
	  cc = 0; 
        }
*/
        if (!indent)
          cc += sprintf(buffer + cc, "    %s", Formal_Name(i));
	Generate_Tlog("IPA", "Array_Section", (SRCPOS) 0, "", "", "", 
	  buffer);
        cc = 0; 	
      }
    }
  }
}

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::TlogMod_Ref_Commons
// FUNCTION: Print mod/ref information for commons at current node to the 
//   Tlog file. 
//-----------------------------------------------------------------------

void CG_BROWSER::Tlog_Mod_Ref_Commons()
{
  INT cc = 0; 
  char buffer[CGB_MAX_STRING_LENGTH];

  // Print information about global scalars
  IPAA_NODE_INFO* ipaa = Cnode()->Mod_Ref_Info();
  ST* st;
  INT i;
  FOREACH_SYMBOL (GLOBAL_SYMTAB, st, i) {
    if (ST_class(st) == CLASS_VAR &&
        ST_base_idx(st) == ST_st_idx(st) &&
        TY_kind(ST_type(st)) != KIND_ARRAY &&
        !ST_is_split_common(st)) {
      UINT32 mod_ref_key = ST_IDX_index(ST_st_idx(st));
      if (ipaa->Is_def_elmt(mod_ref_key)) {
        cc += sprintf(buffer + cc, "MOD /%s/", ST_name(st));
        Generate_Tlog("IPA", "Array_Section", (SRCPOS) 0, "", "", "", 
          buffer);
        cc = 0; 
      } 
      if (ipaa->Is_eref_elmt(mod_ref_key)) {
        cc += sprintf(buffer + cc, "REF /%s/", ST_name(st));
        Generate_Tlog("IPA", "Array_Section", (SRCPOS) 0, "", "", "", 
	  buffer);
        cc = 0; 
      } 
    }
  }

  // Print information about individual arrays
  IPA_NODE_SECTION_INFO* ipas = Cnode()->Section_Annot();
  GLOBAL_ARRAY_TABLE* cst = ipas->Global_Array_Table();
  GLOBAL_ARRAY_TABLE_ITER cst_iter(cst);
  GLOBAL_ARRAY_LIST* list = NULL;
  ST_IDX base_st_idx;
  while (cst_iter.Step(&base_st_idx, &list)) {
    if (list->Is_messy()) { 
      cc += sprintf(buffer + cc, "/%s/ <MESSY>", ST_name(base_st_idx));
      Generate_Tlog("IPA", "Array_Section", (SRCPOS) 0, "", "", "", 
        buffer);
      cc = 0; 
      continue;
    } 
    GLOBAL_ARRAY_LIST_ITER iter(list);
    for (iter.First(); !iter.Is_Empty(); iter.Next()) {
      GLOBAL_ARRAY_INFO* css = iter.Cur();
      if (css->Get_projected_mod_region() != NULL) {
        cc += sprintf(buffer + cc, "MOD ");
        cc += sprintf(buffer + cc, "/%s/ ", ST_name(base_st_idx));
        cc += sprintf(buffer + cc, "%s", ST_name(css->St_Idx()));
        cc = Print_Projected_Region(buffer, cc,
          css->Get_projected_mod_region());
        if (cc >= CGB_MAX_STRING_LENGTH - 1)
	  buffer[CGB_MAX_STRING_LENGTH - 1] = '\0';
        Generate_Tlog("IPA", "Array_Section", (SRCPOS) 0, "", "", "", 
	  buffer);
	cc = 0; 
      }
      if (css->Get_projected_ref_region() != NULL) {
        cc += sprintf(buffer + cc, "REF ");
        cc += sprintf(buffer + cc, "/%s/ ", ST_name(base_st_idx));
        cc += sprintf(buffer + cc, "%s", ST_name(css->St_Idx()));
        cc = Print_Projected_Region(buffer, cc,
          css->Get_projected_ref_region());
        if (cc >= CGB_MAX_STRING_LENGTH - 1)
	  buffer[CGB_MAX_STRING_LENGTH - 1] = '\0';
        Generate_Tlog("IPA", "Array_Section", (SRCPOS) 0, "", "", "", 
	  buffer);
	cc = 0; 
      }
    }
  }
}

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Print_Summary_Value
// FUNCTION: Print the SUMMARY_VALUE 'sv'. 
//-----------------------------------------------------------------------

void CG_BROWSER::Print_Summary_Value(FILE* fp,
                                     SUMMARY_VALUE* sv)
{
  if (sv->Get_mtype() != MTYPE_UNKNOWN)
    fprintf(fp, "%s ", MTYPE_name(sv->Get_mtype()));
  if (sv->Is_int_const())
    fprintf(fp, "(%lld) ", sv->Get_int_const_value());
  else if (sv->Is_two_consts())
    fprintf(fp, "(%d,%d) ",
      sv->Get_first_of_two_values(), sv->Get_second_of_two_values());
  else if (sv->Is_const_st()) { 
    ST_IDX st_idx = sv->Get_const_st_idx();
    ST* st = &St_Table[st_idx];
    fprintf(fp, "<ST_IDX(%d) VALUE(%s)> ", sv->Get_const_st_idx(),
      Targ_Print(NULL, Tcon_Table[st->u1.tcon]));
  } else if (sv->Is_formal())
    fprintf(fp, "FORMAL[%d] ", sv->Get_formal_index());
  else if (sv->Is_global())
    fprintf(fp, "GLOBAL[%d] ", sv->Get_global_index());
  else if (sv->Is_symbol())
    fprintf(fp, "SYMBOL[%d] ", sv->Get_symbol_index());
  else if (sv->Is_expr())
    fprintf(fp, "EXPR[%d] ", sv->Get_expr_index());
  else if (sv->Is_phi())
    fprintf(fp, "PHI[%d] ", sv->Get_phi_index());
  else if (sv->Is_chi())
    fprintf(fp, "CHI[%d] ", sv->Get_chi_index());
  else if (sv->Is_callsite())
    fprintf(fp, "CALLSITE[%d] ", sv->Get_callsite_index());
}
