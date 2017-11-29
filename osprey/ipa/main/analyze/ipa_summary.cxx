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


/* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */

// -------------------------------------------------------------------
// These functions provide interface for accessing summary information
// from the inliner and IPA. Since we want to be able to regenerate
// summary arrays for a specific PU during IPA, it is safer to access
// them via IPA_NODE instead of IP_FILE_HDR. However, this is not 
// always possible, since some arrays are read early in IPA, before
// the call graph is built. For those, we provide dual interfaces.
// -------------------------------------------------------------------

#include <stdint.h>
#include "ipa_cg.h"                     // IPA_NODE, IPA_Call_Graph_Built
#include "ipa_summary.h"        
#include "ipl_summarize.h"              // SUMMARY
#include "ipa_be_summary.h"

// --------------------------------------------------------------
// Functions that take IP_FILE_HDR parameter also return the size
// of the summary array from the file. 
// -----------------------------------------------------------------
static SUMMARY_PROCEDURE* 
get_procedure_file_array (const IP_FILE_HDR& hdr, INT32& size)
{
  const SUMMARY_FILE_HEADER* summary_header = IP_FILE_HDR_file_header (hdr);
  if (size = summary_header->Get_proc_size ()) {
    return (SUMMARY_PROCEDURE*)
      (IP_FILE_HDR_summary (hdr) + summary_header->Get_proc_offset ()); 
  } 
  return NULL;
}

static SUMMARY_SYMBOL* 
get_symbol_file_array(const IP_FILE_HDR& hdr, INT32& size)
{
  const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header(hdr);
  if (size = summary_header->Get_symbol_size()) {
    return (SUMMARY_SYMBOL *)
      (IP_FILE_HDR_summary (hdr) + summary_header->Get_symbol_offset());
  } 
  return NULL;
}

static SUMMARY_GLOBAL*
get_global_file_array (const IP_FILE_HDR& hdr, INT32& size)
{
  Is_True( ! IPA_Call_Graph_Built,
           ("IPA_get_global_file_array must use IPA_NODE when call graph is built"));
  const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header (hdr);
  if (size = summary_header->Get_global_size()) {
    return (SUMMARY_GLOBAL *)
      (IP_FILE_HDR_summary (hdr) + summary_header->Get_global_offset ());
  } 
  return NULL;
}
/*--------------------------------------------------------------*/
/*IPA_get_struct_access_array() is only called before IPA_CALL_GRAPH construction*/
/*--------------------------------------------------------------*/

SUMMARY_STRUCT_ACCESS*
IPA_get_struct_access_file_array (const IP_FILE_HDR& hdr, INT32& size)
{
  const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header (hdr);
  if (size = summary_header->Get_struct_access_size()) {
    return (SUMMARY_STRUCT_ACCESS *)
      (IP_FILE_HDR_summary (hdr) + summary_header->Get_struct_access_offset ());
  } 
  return NULL;
}

#ifdef KEY
SUMMARY_TY_INFO*
IPA_get_ty_info_file_array (const IP_FILE_HDR& hdr, INT32& size)
{
  const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header (hdr);
  if (size = summary_header->Get_ty_info_size()) {
    return (SUMMARY_TY_INFO *)
      (IP_FILE_HDR_summary (hdr) + summary_header->Get_ty_info_offset ());
  } 
  return NULL;
}
#endif

SUMMARY_CONSTRAINT_GRAPH_NODE*
IPA_get_constraint_graph_nodes_array(const IP_FILE_HDR &hdr, INT32 &size)
{
  const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header(hdr);
  if (size = summary_header->Get_constraint_graph_nodes_size()) {
    return (SUMMARY_CONSTRAINT_GRAPH_NODE *)
      (IP_FILE_HDR_summary (hdr) + 
                   summary_header->Get_constraint_graph_nodes_offset());
  } 
  return NULL;
}

SUMMARY_CONSTRAINT_GRAPH_EDGE*
IPA_get_constraint_graph_edges_array(const IP_FILE_HDR &hdr, INT32 &size)
{
  const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header(hdr);
  if (size = summary_header->Get_constraint_graph_edges_size()) {
    return (SUMMARY_CONSTRAINT_GRAPH_EDGE *)
      (IP_FILE_HDR_summary (hdr) + 
                   summary_header->Get_constraint_graph_edges_offset());
  } 
  return NULL;
}

SUMMARY_CONSTRAINT_GRAPH_STINFO*
IPA_get_constraint_graph_stinfos_array(const IP_FILE_HDR &hdr, INT32 &size)
{
  const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header(hdr);
  if (size = summary_header->Get_constraint_graph_stinfos_size()) {
    return (SUMMARY_CONSTRAINT_GRAPH_STINFO *)
      (IP_FILE_HDR_summary (hdr) + 
                   summary_header->Get_constraint_graph_stinfos_offset());
  } 
  return NULL;
}

SUMMARY_CONSTRAINT_GRAPH_CALLSITE*
IPA_get_constraint_graph_callsites_array(const IP_FILE_HDR &hdr, INT32 &size)
{
  const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header(hdr);
  if (size = summary_header->Get_constraint_graph_callsites_size()) {
    return (SUMMARY_CONSTRAINT_GRAPH_CALLSITE *)
      (IP_FILE_HDR_summary (hdr) + 
                   summary_header->Get_constraint_graph_callsites_offset());
  } 
  return NULL;
}

SUMMARY_CONSTRAINT_GRAPH_MODRANGE*
IPA_get_constraint_graph_modranges_array(const IP_FILE_HDR &hdr, INT32 &size)
{
  const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header(hdr);
  if (size = summary_header->Get_constraint_graph_modranges_size()) {
    return (SUMMARY_CONSTRAINT_GRAPH_MODRANGE *)
      (IP_FILE_HDR_summary (hdr) + 
                   summary_header->Get_constraint_graph_modranges_offset());
  } 
  return NULL;
}

UINT32 *
IPA_get_constraint_graph_node_ids_array(const IP_FILE_HDR &hdr, INT32 &size)
{
  const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header(hdr);
  if (size = summary_header->Get_constraint_graph_node_ids_size()) {
    return (UINT32 *)(IP_FILE_HDR_summary (hdr) + 
                      summary_header->Get_constraint_graph_node_ids_offset());
  } 
  return NULL;
}

static SUMMARY_GLOBAL*
get_global_array (const IPA_NODE* node)
{
  if (node->Is_Preoptimized()) {
    SUMMARY* summary = 
      (SUMMARY*) IPA_Call_Graph->New_Summary_Ptrs(node)->Summary();
    INT32 size = summary->Get_global_idx() + 1;
    if (size) {
      return summary->Get_global(0);
    }
  }
  else {
    const IP_FILE_HDR& hdr = node->File_Header();
    const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header(hdr);
    INT32 size = summary_header->Get_global_size();
    if (size) {
      return (SUMMARY_GLOBAL*)
        (IP_FILE_HDR_summary (hdr) + summary_header->Get_global_offset());
    }
  } 
  return NULL;
}

static SUMMARY_PROCEDURE*
get_procedure_array (const IPA_NODE* node)
{
  INT32 size;
  return IPA_get_procedure_file_array (node->File_Header(), size);
}

static SUMMARY_SYMBOL* 
get_symbol_array(const IPA_NODE* node)
{
  INT32 size;
  return IPA_get_symbol_file_array (node->File_Header(), size);
}

static SUMMARY_CALLSITE* 
get_callsite_array (const IPA_NODE* node)
{
  if (node->Is_Preoptimized()) {
    SUMMARY* summary = 
      (SUMMARY*) IPA_Call_Graph->New_Summary_Ptrs(node)->Summary();
    if (summary->Has_callsite_entry()) {
      return summary->Get_callsite(0);
    }
  }
  else {
    const IP_FILE_HDR& hdr = node->File_Header();
    const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header (hdr);
    if (summary_header->Get_callsite_size ()) {
      return (SUMMARY_CALLSITE *)
        (IP_FILE_HDR_summary (hdr) + summary_header->Get_callsite_offset());
    }
  }
  return NULL;
}

static SUMMARY_FORMAL*
get_formal_array (const IPA_NODE* node)
{
  if (node->Is_Preoptimized()) {
    SUMMARY* summary = 
      (SUMMARY*) IPA_Call_Graph->New_Summary_Ptrs(node)->Summary();
    if (summary->Has_formal_entry()) {
      return summary->Get_formal(0);
    }
  }
  else {
    const IP_FILE_HDR& hdr = node->File_Header();
    const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header (hdr);
    if (summary_header->Get_formal_size ()) {
      return (SUMMARY_FORMAL *)
        (IP_FILE_HDR_summary (hdr) + summary_header->Get_formal_offset ());
    }
  }
  return NULL; 
} 

static SUMMARY_ACTUAL*
get_actual_array (const IPA_NODE* node)
{
  if (node->Is_Preoptimized()) {
    SUMMARY* summary = 
      (SUMMARY*) IPA_Call_Graph->New_Summary_Ptrs(node)->Summary();
    if (summary->Has_actual_entry()) {
      return summary->Get_actual(0);
    }
  }
  else {
    const IP_FILE_HDR& hdr = node->File_Header();
    const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header (hdr);
    if (summary_header->Get_actual_size ()) {
      return (SUMMARY_ACTUAL *)
        (IP_FILE_HDR_summary (hdr) + summary_header->Get_actual_offset ());
    }
  }
  return NULL; 
} 

SUMMARY_VALUE* 
IPA_get_value_file_array (const IP_FILE_HDR& hdr, INT32& size)
{
  Is_True(! IPA_Call_Graph_Built,
          ("IPA_get_value_file_array must use IPA_NODE after call graph is built"));
  const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header (hdr);
  if (size = summary_header->Get_value_size ()) {
    return (SUMMARY_VALUE *)
      (IP_FILE_HDR_summary (hdr) + summary_header->Get_value_offset());
  } 
  return NULL;
}

SUMMARY_FORMAL* 
IPA_get_formal_file_array (const IP_FILE_HDR& hdr, INT32& size)
{
  Is_True(! IPA_Call_Graph_Built,
          ("IPA_get_formal_file_array must use IPA_NODE after call graph is built"));
  const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header (hdr);
  if (size = summary_header->Get_formal_size ()) {
    return (SUMMARY_FORMAL *)
      (IP_FILE_HDR_summary (hdr) + summary_header->Get_formal_offset());
  } 
  return NULL;
}

SUMMARY_ACTUAL* 
IPA_get_actual_file_array (const IP_FILE_HDR& hdr, INT32& size)
{
  Is_True(! IPA_Call_Graph_Built,
          ("IPA_get_actual_file_array must use IPA_NODE after call graph is built"));
  const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header (hdr);
  if (size = summary_header->Get_actual_size ()) {
    return (SUMMARY_ACTUAL *)
      (IP_FILE_HDR_summary (hdr) + summary_header->Get_actual_offset());
  } 
  return NULL;
}

SUMMARY_CALLSITE* 
IPA_get_callsite_file_array (const IP_FILE_HDR& hdr, INT32& size) 
{ 
  Is_True(! IPA_Call_Graph_Built, 
          ("IPA_get_actual_file_array must use IPA_NODE after call graph is built")); 
  const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header (hdr); 
  if (size = summary_header->Get_callsite_size ()) { 
    return (SUMMARY_CALLSITE *) 
      (IP_FILE_HDR_summary (hdr) + summary_header->Get_callsite_offset()); 
  } 
  return NULL; 
} 

IVAR* 
IPA_get_ivar_file_array (const IP_FILE_HDR& hdr, INT32& size)
{
  const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header (hdr);
  if (size = summary_header->Get_ivar_size ()) {
    return (IVAR *)
      (IP_FILE_HDR_summary (hdr) + summary_header->Get_ivar_offset());
  }
  return NULL;
}

static SUMMARY_FEEDBACK* 
get_feedback_array (const IPA_NODE* node)
{
  if (node->Is_Preoptimized()) {
    SUMMARY* summary = 
      (SUMMARY*) IPA_Call_Graph->New_Summary_Ptrs(node)->Summary();
    if (summary->Has_feedback_entry()) {
      return summary->Get_feedback(0);
    }
  }
  else {
    const IP_FILE_HDR& hdr = node->File_Header();
    const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header (hdr);
    if (summary_header->Get_feedback_size ()) {
      return (SUMMARY_FEEDBACK *)
        (IP_FILE_HDR_summary (hdr) + summary_header->Get_feedback_offset());
    } 
  }
  return NULL;
}
#if (defined(_STANDALONE_INLINER) || defined(_LIGHTWEIGHT_INLINER))

// --------------------------------------------------------------
// Functions that take IP_FILE_HDR parameter also return the size
// of the summary array from the file. 
// --------------------------------------------------------------
SUMMARY_PROCEDURE*
IPA_get_procedure_file_array (const IP_FILE_HDR& hdr, INT32& size)
{
  if (IP_FILE_HDR_file_header(hdr)) {
      return get_procedure_file_array(hdr, size);
  }
  else {
      SUMMARY* summary = (SUMMARY*) IP_FILE_HDR_summary (hdr);
      if (size = summary->Get_procedure_idx()+1) {
          return summary->Get_procedure (0);
      }
      return NULL;
  }
}

SUMMARY_SYMBOL* 
IPA_get_symbol_file_array (const IP_FILE_HDR& hdr, INT32& size)
{
  if (IP_FILE_HDR_file_header(hdr)) {
      return get_symbol_file_array(hdr, size);
  }
  else {
      SUMMARY* summary = (SUMMARY*) IP_FILE_HDR_summary (hdr);
      if (size = summary->Get_symbol_idx()+1) {
        return summary->Get_symbol (0);
      }
      return NULL;
  }
}

// --------------------------------------------------------------
// After IPA_Call_Graph is built, SUMMARY_GLOBAL array may change
// (SUMMARY_GLOBALs are PU-segmented), and they must be accessed
// through the alternative interface that uses IPA_NODE.
// --------------------------------------------------------------
SUMMARY_GLOBAL*
IPA_get_global_file_array (const IP_FILE_HDR& hdr, INT32& size)
{
    return get_global_file_array(hdr, size);
}

// -----------------------------------------------------------------
// These functions should be used as soon as IPA_Call_Graph is built
// -----------------------------------------------------------------
SUMMARY_PROCEDURE* 
IPA_get_procedure_array (const IPA_NODE* node)
{
  if (IP_FILE_HDR_file_header(node->File_Header())) {
      return get_procedure_array(node);
  }
  else {
      SUMMARY* summary = (SUMMARY*) IP_FILE_HDR_summary (node->File_Header());
      return (summary->Has_procedure_entry()? summary->Get_procedure (0) : NULL);
  }
}
				 
SUMMARY_SYMBOL* 
IPA_get_symbol_array (const IPA_NODE* node)
{
  if (IP_FILE_HDR_file_header(node->File_Header())) {
      return get_symbol_array(node);
  }
  else {
      SUMMARY* summary = (SUMMARY*) IP_FILE_HDR_summary (node->File_Header());
      return (summary->Has_symbol_entry()? summary->Get_symbol (0) : NULL);
  }
}

SUMMARY_GLOBAL*
IPA_get_global_array (const IPA_NODE* node)
{
  if (IP_FILE_HDR_file_header(node->File_Header())) {
      return get_global_array(node);
  }
  else {
      SUMMARY* summary = (SUMMARY*) IP_FILE_HDR_summary (node->File_Header());
      return (summary->Has_global_entry() ? summary->Get_global (0) : NULL);
  }
}

SUMMARY_CALLSITE* 
IPA_get_callsite_array (const IPA_NODE* node)
{
  if (IP_FILE_HDR_file_header(node->File_Header())) {
      return get_callsite_array(node);
  }
  else {
      SUMMARY* summary = (SUMMARY*) IP_FILE_HDR_summary (node->File_Header());
      return (summary->Has_callsite_entry()? summary->Get_callsite (0) : NULL);
  }
}

SUMMARY_FORMAL*
IPA_get_formal_array (const IPA_NODE* node)
{
  if (IP_FILE_HDR_file_header(node->File_Header())) {
      return get_formal_array(node);
  }
  else {
      SUMMARY* summary = (SUMMARY*) IP_FILE_HDR_summary (node->File_Header());
      return (summary->Has_formal_entry()? summary->Get_formal (0) : NULL);
  }
} 

SUMMARY_ACTUAL*
IPA_get_actual_array (const IPA_NODE* node)
{
  if (IP_FILE_HDR_file_header(node->File_Header())) {
      return get_actual_array(node);
  }
  else {
      SUMMARY* summary = (SUMMARY*) IP_FILE_HDR_summary (node->File_Header());
      return (summary->Has_actual_entry()? summary->Get_actual (0) : NULL);
  }
} 

SUMMARY_FEEDBACK* 
IPA_get_feedback_array (const IPA_NODE* node)
{
  if (IP_FILE_HDR_file_header(node->File_Header())) {
      return get_feedback_array(node);
  }
  else {
      SUMMARY* summary = (SUMMARY*) IP_FILE_HDR_summary (node->File_Header());
      return (summary->Has_feedback_entry()? summary->Get_feedback (0) : NULL);
  }
  return NULL;
}

#else // _STANDALONE_INLINER


#include "ipl_array_bread_write.h"      // ARRAY_SUMMARY_OUTPUT
#include "ipa_section_annot.h"          // SECTION_FILE_ANNOT

// --------------------------------------------------------------
// Functions that take IP_FILE_HDR parameter also return the size
// of the summary array from the file. 
// -----------------------------------------------------------------
SUMMARY_PROCEDURE* 
IPA_get_procedure_file_array (const IP_FILE_HDR& hdr, INT32& size)
{
  return get_procedure_file_array(hdr, size);
}

SUMMARY_SYMBOL* 
IPA_get_symbol_file_array(const IP_FILE_HDR& hdr, INT32& size)
{
  return get_symbol_file_array(hdr, size);
}

// ---------------------------------------------------------------
// After IPA_Call_Graph is built, the following summary arrays may 
// change (become PU-segmented), and they must be accessed through 
// the alternative interface that uses IPA_NODE.
// ---------------------------------------------------------------
SUMMARY_COMMON* 
IPA_get_common_file_array (const IP_FILE_HDR& hdr, INT32& size)
{
  Is_True( ! IPA_Call_Graph_Built,
           ("IPA_get_common_file_array must use IPA_NODE when call graph is built"));
  const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header(hdr);
  if (size = summary_header->Get_common_size()) {
    return (SUMMARY_COMMON *)
      (IP_FILE_HDR_summary (hdr) + summary_header->Get_common_offset());
  } 
  return NULL;
}

SUMMARY_GLOBAL*
IPA_get_global_file_array (const IP_FILE_HDR& hdr, INT32& size)
{
  return get_global_file_array(hdr, size);
}


// ----------------------------------------------------------------
// Access to the following SUMMARY arrays will go through the file
// header, even if the IPA_NODE has been preoptimized. These access
// functions are provided for convinience and completeness.
// ----------------------------------------------------------------
SUMMARY_PROCEDURE*
IPA_get_procedure_array (const IPA_NODE* node)
{
  return get_procedure_array(node);
}

SUMMARY_SYMBOL* 
IPA_get_symbol_array(const IPA_NODE* node)
{
  return get_symbol_array(node);
}

IVAR* 
IPA_get_ivar_array (const IPA_NODE* node, INT32& size)
{
  SECTION_FILE_ANNOT* annot = IP_FILE_HDR_section_annot(node->File_Header());
  if (annot && annot->Get_ivar_array()) {
    IVAR_ARRAY* iv_array = annot->Get_ivar_array();
    size = iv_array->Elements();
    return &(*iv_array)[0];
  }
  return IPA_get_ivar_file_array (node->File_Header(), size);
}

// -----------------------------------------------------------------------
// The following SUMMARY arrays will change when IPA_NODE is preoptimized, 
// and they shouldn't be accessed directly through the file header.
// -----------------------------------------------------------------------
SUMMARY_COMMON* 
IPA_get_common_array (const IPA_NODE* node)
{
  if (node->Is_Preoptimized()) {
    SUMMARY* summary = 
      (SUMMARY*) IPA_Call_Graph->New_Summary_Ptrs(node)->Summary();
    if (summary->Has_common_entry()) {
      return summary->Get_common(0);
    }
  }
  else {
    const IP_FILE_HDR& hdr = node->File_Header();
    const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header (hdr);
    if (summary_header->Get_common_size ()) {
      return (SUMMARY_COMMON *)
        (IP_FILE_HDR_summary (hdr) + summary_header->Get_common_offset());
    } 
  }
  return NULL;
}

SUMMARY_COMMON_SHAPE* 
IPA_get_common_shape_array (const IPA_NODE* node)
{
  if (node->Is_Preoptimized()) {
    SUMMARY* summary = 
      (SUMMARY*) IPA_Call_Graph->New_Summary_Ptrs(node)->Summary();
    if (summary->Has_common_shape_entry()) {
      return summary->Get_common_shape(0);
    }
  }
  else {
    const IP_FILE_HDR& hdr = node->File_Header();
    const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header(hdr);
    if (summary_header->Get_common_shape_size()) {
      return (SUMMARY_COMMON_SHAPE *)
        (IP_FILE_HDR_summary(hdr) + summary_header->Get_common_shape_offset());
    } 
  }
  return NULL;    
}

SUMMARY_VALUE* 
IPA_get_value_array (const IPA_NODE* node)
{
  if (node->Is_Preoptimized()) {
    SUMMARY* summary = 
      (SUMMARY*) IPA_Call_Graph->New_Summary_Ptrs(node)->Summary();
    if (summary->Has_value_entry()) {
      return summary->Get_value(0);
    }
  }
  else {
    const IP_FILE_HDR& hdr = node->File_Header();
    const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header (hdr);
    if (summary_header->Get_value_size ()) {
      return (SUMMARY_VALUE *)
        (IP_FILE_HDR_summary (hdr) + summary_header->Get_value_offset());
    } 
  }
  return NULL;
}

SUMMARY_GLOBAL*
IPA_get_global_array (const IPA_NODE* node)
{
  return get_global_array(node);
}

SUMMARY_CALLSITE* 
IPA_get_callsite_array (const IPA_NODE* node)
{
  return get_callsite_array(node);
}

SUMMARY_FORMAL*
IPA_get_formal_array (const IPA_NODE* node)
{
  return get_formal_array(node);
} 

SUMMARY_ACTUAL*
IPA_get_actual_array (const IPA_NODE* node)
{
  return get_actual_array(node); 
} 

SUMMARY_CONTROL_DEPENDENCE*
IPA_get_ctrl_dep_array (const IPA_NODE* node)
{
  if (node->Is_Preoptimized()) {
    SUMMARY* summary = 
      (SUMMARY*) IPA_Call_Graph->New_Summary_Ptrs(node)->Summary();
    if (summary->Has_ctrl_dep_entry()) {
      return summary->Get_ctrl_dep(0);
    }
  }
  else {
    const IP_FILE_HDR& hdr = node->File_Header();
    const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header (hdr);
    if (summary_header->Get_ctrl_dep_size ()) {
      return (SUMMARY_CONTROL_DEPENDENCE *)
        (IP_FILE_HDR_summary (hdr) + summary_header->Get_ctrl_dep_offset ());
    }
  }
  return NULL; 
} 

SUMMARY_STMT*
IPA_get_stmt_array (const IPA_NODE* node)
{
  if (node->Is_Preoptimized()) {
    SUMMARY* summary = 
      (SUMMARY*) IPA_Call_Graph->New_Summary_Ptrs(node)->Summary();
    if (summary->Has_stmt_entry()) {
      return summary->Get_stmt(0);
    }
  }
  else {
    const IP_FILE_HDR& hdr = node->File_Header();
    const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header (hdr);
    if (summary_header->Get_stmt_size ()) {
      return (SUMMARY_STMT *)
        (IP_FILE_HDR_summary (hdr) + summary_header->Get_stmt_offset ());
    }
  }
  return NULL; 
} 

SUMMARY_STID* 
IPA_get_stid_array (const IPA_NODE* node)
{
  if (node->Is_Preoptimized()) {
    SUMMARY* summary = 
      (SUMMARY*) IPA_Call_Graph->New_Summary_Ptrs(node)->Summary();
    if (summary->Has_global_stid_entry()) {
      return summary->Get_global_stid(0);
    }
  }
  else {
    const IP_FILE_HDR& hdr = node->File_Header();
    const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header (hdr);
    if (summary_header->Get_global_stid_size ()) {
      return (SUMMARY_STID *)
        (IP_FILE_HDR_summary (hdr) + summary_header->Get_global_stid_offset());
    } 
  }
  return NULL;
}

SUMMARY_EXPR*
IPA_get_expr_array (const IPA_NODE* node)
{
  if (node->Is_Preoptimized()) {
    SUMMARY* summary = 
      (SUMMARY*) IPA_Call_Graph->New_Summary_Ptrs(node)->Summary();
    if (summary->Has_expr_entry()) {
      return summary->Get_expr(0);
    }
  }
  else {
    const IP_FILE_HDR& hdr = node->File_Header();
    const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header (hdr);
    if (summary_header->Get_expr_size ()) {
      return (SUMMARY_EXPR *)
        (IP_FILE_HDR_summary (hdr) + summary_header->Get_expr_offset ());
    }
  }
  return NULL; 
} 

SUMMARY_PHI*
IPA_get_phi_array (const IPA_NODE* node)
{
  if (node->Is_Preoptimized()) {
    SUMMARY* summary = 
      (SUMMARY*) IPA_Call_Graph->New_Summary_Ptrs(node)->Summary();
    if (summary->Has_phi_entry()) {
      return summary->Get_phi(0);
    }
  }
  else {
    const IP_FILE_HDR& hdr = node->File_Header();
    const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header (hdr);
    if (summary_header->Get_phi_size ()) {
      return (SUMMARY_PHI *)
        (IP_FILE_HDR_summary (hdr) + summary_header->Get_phi_offset ());
    }
  }
  return NULL; 
} 

SUMMARY_CHI*
IPA_get_chi_array (const IPA_NODE* node)
{
  if (node->Is_Preoptimized()) {
    SUMMARY* summary = 
      (SUMMARY*) IPA_Call_Graph->New_Summary_Ptrs(node)->Summary();
    if (summary->Has_chi_entry()) {
      return summary->Get_chi(0);
    }
  }
  else {
    const IP_FILE_HDR& hdr = node->File_Header();
    const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header (hdr);
    if (summary_header->Get_chi_size ()) {
      return (SUMMARY_CHI *)
        (IP_FILE_HDR_summary (hdr) + summary_header->Get_chi_offset ());
    }
  }
  return NULL; 
} 

CFG_NODE_INFO *
IPA_get_cfg_node_array (const IPA_NODE* node)
{
  if (node->Is_Preoptimized()) {
    ARRAY_SUMMARY_OUTPUT* array_summary = (ARRAY_SUMMARY_OUTPUT*)
      IPA_Call_Graph->New_Summary_Ptrs(node)->Array_Summary();
    if (array_summary->Get_cfg_node_count() != -1) {
      return array_summary->Get_cfg_node(0);
    }
  }
  else {
    const IP_FILE_HDR& hdr = node->File_Header();
    const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header (hdr);
    if (summary_header->Get_cfg_node_size ()) {
      return (CFG_NODE_INFO *)
        (IP_FILE_HDR_summary (hdr) + summary_header->Get_cfg_node_offset());
    } 
  }
  return NULL;
}

REGION_ARRAYS* 
IPA_get_region_array (const IPA_NODE* node)
{
  if (node->Is_Preoptimized()) {
    ARRAY_SUMMARY_OUTPUT* array_summary = (ARRAY_SUMMARY_OUTPUT*)
      IPA_Call_Graph->New_Summary_Ptrs(node)->Array_Summary();
    if (array_summary->Get_region_count() != -1) {
      return array_summary->Get_region_array(0);
    }
  }
  else {
    const IP_FILE_HDR& hdr = node->File_Header();
    const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header (hdr);
    if (summary_header->Get_regions_array_size ()) {
      return (REGION_ARRAYS *)
        (IP_FILE_HDR_summary (hdr) + summary_header->Get_regions_array_offset());
    }
  }
  return NULL;
}

PROJECTED_REGION* 
IPA_get_proj_region_array (const IPA_NODE* node)
{
  if (node->Is_Preoptimized()) {
    ARRAY_SUMMARY_OUTPUT* array_summary = (ARRAY_SUMMARY_OUTPUT*)
      IPA_Call_Graph->New_Summary_Ptrs(node)->Array_Summary();
    if (array_summary->Get_projected_region_count() != -1) {
      return array_summary->Get_projected_region(0);
    }
  }
  else {
    const IP_FILE_HDR& hdr = node->File_Header();
    const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header (hdr);
    if (summary_header->Get_projected_region_size ()) {
      return (PROJECTED_REGION *) (IP_FILE_HDR_summary (hdr) + 
                                   summary_header->Get_projected_region_offset());
    } 
  }
  return NULL;
}

PROJECTED_NODE* 
IPA_get_projected_node_array (const IPA_NODE* node)
{
  if (node->Is_Preoptimized()) {
    ARRAY_SUMMARY_OUTPUT* array_summary = (ARRAY_SUMMARY_OUTPUT*)
      IPA_Call_Graph->New_Summary_Ptrs(node)->Array_Summary();
    if (array_summary->Get_projected_node_count() != -1) {
      return array_summary->Get_projected_node(0);
    }
  }
  else {
    const IP_FILE_HDR& hdr = node->File_Header();
    const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header (hdr);
    if (summary_header->Get_projected_array_size ()) {
      return (PROJECTED_NODE *) (IP_FILE_HDR_summary (hdr) +
                                 summary_header->Get_projected_array_offset());
    } 
  }
  return NULL;
}

LOOPINFO* 
IPA_get_loopinfo_array (const IPA_NODE* node)
{
  if (node->Is_Preoptimized()) {
    ARRAY_SUMMARY_OUTPUT* array_summary = (ARRAY_SUMMARY_OUTPUT*)
      IPA_Call_Graph->New_Summary_Ptrs(node)->Array_Summary();
    if (array_summary->Get_loopinfo_count() != -1) {
      return array_summary->Get_loopinfo(0);
    }
  }
  else {
    const IP_FILE_HDR& hdr = node->File_Header();
    const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header (hdr);
    if (summary_header->Get_loopinfo_size ()) {
      return (LOOPINFO *)
        (IP_FILE_HDR_summary (hdr) + summary_header->Get_loopinfo_offset());
    } 
  }
  return NULL;
}

TERM* 
IPA_get_term_array (const IPA_NODE* node)
{
  if (node->Is_Preoptimized()) {
    ARRAY_SUMMARY_OUTPUT* array_summary = (ARRAY_SUMMARY_OUTPUT*)
      IPA_Call_Graph->New_Summary_Ptrs(node)->Array_Summary();
    if (array_summary->Get_term_count() != -1) {
      return array_summary->Get_term(0);
    }
  }
  else {
    const IP_FILE_HDR& hdr = node->File_Header();
    const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header (hdr);
    if (summary_header->Get_term_array_size ()) {
      return (TERM *)
        (IP_FILE_HDR_summary (hdr) + summary_header->Get_term_array_offset());
    } 
  }
  return NULL;
}

SCALAR_INFO* 
IPA_get_scalar_array (const IPA_NODE* node)
{
  if (node->Is_Preoptimized()) {
    ARRAY_SUMMARY_OUTPUT* array_summary = (ARRAY_SUMMARY_OUTPUT*)
      IPA_Call_Graph->New_Summary_Ptrs(node)->Array_Summary();
    if (array_summary->Get_scalars_count() != -1) {
      return array_summary->Get_scalars(0);
    }
  }
  else {
    const IP_FILE_HDR& hdr = node->File_Header();
    const SUMMARY_FILE_HEADER *summary_header = IP_FILE_HDR_file_header (hdr);
    if (summary_header->Get_scalar_node_size ()) {
      return (SCALAR_INFO *)
        (IP_FILE_HDR_summary (hdr) + summary_header->Get_scalar_offset());
    }
  }
  return NULL;
}

SUMMARY_FEEDBACK* 
IPA_get_feedback_array (const IPA_NODE* node)
{
  return get_feedback_array(node); 
}
#endif  // _STANDALONE_INLINER
