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

#ifndef cxx_ipa_summary_INCLUDED
#define cxx_ipa_summary_INCLUDED

class IPA_NODE;
class IP_FILE_HDR;
class SUMMARY_FILE_HEADER;
class SUMMARY_PROCEDURE;
class SUMMARY_SYMBOL;
class SUMMARY_GLOBAL;
class SUMMARY_CALLSITE;
class SUMMARY_VALUE;
class SUMMARY_FORMAL;
class SUMMARY_ACTUAL;
class SUMMARY_COMMON;
class SUMMARY_COMMON_SHAPE;
class SUMMARY_CONTROL_DEPENDENCE;
class SUMMARY_STMT;
class SUMMARY_STID;
class SUMMARY_EXPR;
class SUMMARY_PHI;
class SUMMARY_CHI;
class SUMMARY_FEEDBACK;
class CFG_NODE_INFO;
class REGION_ARRAYS;
class PROJECTED_REGION;
class PROJECTED_NODE;
class LOOPINFO;
class TERM;
class IVAR;
class SCALAR_INFO;
class SUMMARY_STRUCT_ACCESS;
#ifdef KEY
class SUMMARY_TY_INFO;
#endif
class SUMMARY_CONSTRAINT_GRAPH_NODE;
class SUMMARY_CONSTRAINT_GRAPH_EDGE;
class SUMMARY_CONSTRAINT_GRAPH_STINFO;
class SUMMARY_CONSTRAINT_GRAPH_CALLSITE;
class SUMMARY_CONSTRAINT_GRAPH_MODRANGE;

class IPL_SUMMARY_PTRS
{
private:
  void* _summary;
  void* _array_summary;

public:
  IPL_SUMMARY_PTRS (void* summary, void* array_summary) :
    _summary (summary), 
    _array_summary (array_summary) 
  {}

  void* Summary() const { return _summary; }
  void* Array_Summary() const { return _array_summary; }
};

inline SUMMARY_FILE_HEADER*
IPA_get_file_header (char* summary_base)
{
  Elf64_Word offset = *((Elf64_Word *) summary_base);
  return (SUMMARY_FILE_HEADER *) (summary_base + offset);
}

extern SUMMARY_VALUE* 
IPA_get_value_file_array (const IP_FILE_HDR& hdr, INT32& size);

extern SUMMARY_FORMAL* 
IPA_get_formal_file_array (const IP_FILE_HDR& hdr, INT32& size);

extern SUMMARY_ACTUAL* 
IPA_get_actual_file_array (const IP_FILE_HDR& hdr, INT32& size);

extern IVAR* 
IPA_get_ivar_file_array (const IP_FILE_HDR& hdr, INT32& size);

#ifdef _STANDALONE_INLINER

/*ARGSUSED*/

// --------------------------------------------------------------
// Functions that take IP_FILE_HDR parameter also return the size
// of the summary array from the file. 
// --------------------------------------------------------------
SUMMARY_PROCEDURE* 
IPA_get_procedure_file_array (const IP_FILE_HDR& hdr, INT32& size);
SUMMARY_SYMBOL* 
IPA_get_symbol_file_array (const IP_FILE_HDR& hdr, INT32& size);
SUMMARY_GLOBAL* 
IPA_get_global_file_array (const IP_FILE_HDR& hdr, INT32& size);
SUMMARY_STRUCT_ACCESS*
IPA_get_struct_access_file_array (const IP_FILE_HDR& hdr, INT32& size);
#ifdef KEY
SUMMARY_TY_INFO*
IPA_get_ty_info_file_array (const IP_FILE_HDR& hdr, INT32& size);
#endif


// ---------------------------------------------------------------
// Access to the SUMMARY arrays in the inliner always goes through
// the file header. These access functions are provided for 
// convinience and completeness.
// ---------------------------------------------------------------
SUMMARY_PROCEDURE* 
IPA_get_procedure_array (const IPA_NODE* node);
SUMMARY_SYMBOL* 
IPA_get_symbol_array (const IPA_NODE* node);
SUMMARY_GLOBAL* 
IPA_get_global_array (const IPA_NODE* node);
SUMMARY_CALLSITE* 
IPA_get_callsite_array (const IPA_NODE* node);
SUMMARY_FORMAL* 
IPA_get_formal_array (const IPA_NODE* node);
SUMMARY_ACTUAL* 
IPA_get_actual_array (const IPA_NODE* node);
SUMMARY_FEEDBACK* 
IPA_get_feedback_array (const IPA_NODE* node);

#else // _STANDALONE_INLINER

// --------------------------------------------------------------
// Functions that take IP_FILE_HDR parameter also return the size
// of the summary array from the file. 
// -----------------------------------------------------------------
SUMMARY_PROCEDURE* 
IPA_get_procedure_file_array (const IP_FILE_HDR& hdr, INT32& size);
SUMMARY_SYMBOL* 
IPA_get_symbol_file_array (const IP_FILE_HDR& hdr, INT32& size);
SUMMARY_COMMON* 
IPA_get_common_file_array (const IP_FILE_HDR& hdr, INT32& size);
IVAR* 
IPA_get_ivar_file_array (const IP_FILE_HDR& hdr, INT32& size);
SUMMARY_GLOBAL* 
IPA_get_global_file_array (const IP_FILE_HDR& hdr, INT32& size);
SUMMARY_VALUE* 
IPA_get_value_file_array (const IP_FILE_HDR& hdr, INT32& size);
SUMMARY_FORMAL* 
IPA_get_formal_file_array (const IP_FILE_HDR& hdr, INT32& size);
SUMMARY_ACTUAL* 
IPA_get_actual_file_array (const IP_FILE_HDR& hdr, INT32& size);
SUMMARY_CALLSITE* 
IPA_get_callsite_file_array (const IP_FILE_HDR& hdr, INT32& size); 
SUMMARY_STRUCT_ACCESS*
IPA_get_struct_access_file_array (const IP_FILE_HDR& hdr, INT32& size);
#ifdef KEY
SUMMARY_TY_INFO*
IPA_get_ty_info_file_array (const IP_FILE_HDR& hdr, INT32& size);
#endif
SUMMARY_CONSTRAINT_GRAPH_NODE *
IPA_get_constraint_graph_nodes_array (const IP_FILE_HDR& hdr, INT32& size);
SUMMARY_CONSTRAINT_GRAPH_EDGE *
IPA_get_constraint_graph_edges_array (const IP_FILE_HDR& hdr, INT32& size);
SUMMARY_CONSTRAINT_GRAPH_STINFO *
IPA_get_constraint_graph_stinfos_array (const IP_FILE_HDR& hdr, INT32& size);
SUMMARY_CONSTRAINT_GRAPH_CALLSITE *
IPA_get_constraint_graph_callsites_array (const IP_FILE_HDR& hdr, INT32& size);
UINT32 *
IPA_get_constraint_graph_node_ids_array (const IP_FILE_HDR& hdr, INT32& size);
SUMMARY_CONSTRAINT_GRAPH_MODRANGE *
IPA_get_constraint_graph_modranges_array (const IP_FILE_HDR& hdr, INT32& size);


// ----------------------------------------------------------------
// Access to the following SUMMARY arrays will go through the file
// header, even if the IPA_NODE has been preoptimized. These access
// functions are provided for convinience and completeness.
// ----------------------------------------------------------------
SUMMARY_PROCEDURE* IPA_get_procedure_array (const IPA_NODE* node);
SUMMARY_SYMBOL* IPA_get_symbol_array (const IPA_NODE* node);
IVAR* IPA_get_ivar_array (const IPA_NODE* node, INT32& size);

// -----------------------------------------------------------------------
// The following SUMMARY arrays will change when IPA_NODE is preoptimized, 
// and they shouldn't be accessed directly through the file header.
// -----------------------------------------------------------------------
SUMMARY_CALLSITE* IPA_get_callsite_array (const IPA_NODE* node);
SUMMARY_FORMAL* IPA_get_formal_array (const IPA_NODE* node);
SUMMARY_ACTUAL* IPA_get_actual_array (const IPA_NODE* node);
SUMMARY_VALUE* IPA_get_value_array (const IPA_NODE* node);
SUMMARY_GLOBAL* IPA_get_global_array (const IPA_NODE* node);
SUMMARY_COMMON* IPA_get_common_array (const IPA_NODE* node);
SUMMARY_COMMON_SHAPE* IPA_get_common_shape_array (const IPA_NODE* node);
SUMMARY_CONTROL_DEPENDENCE* IPA_get_ctrl_dep_array(const IPA_NODE* node);
SUMMARY_STMT* IPA_get_stmt_array (const IPA_NODE* node);
SUMMARY_STID* IPA_get_stid_array (const IPA_NODE* node);
SUMMARY_EXPR* IPA_get_expr_array (const IPA_NODE* node);
SUMMARY_PHI* IPA_get_phi_array (const IPA_NODE* node);
SUMMARY_CHI* IPA_get_chi_array (const IPA_NODE* node);
CFG_NODE_INFO* IPA_get_cfg_node_array (const IPA_NODE* node);
REGION_ARRAYS* IPA_get_region_array (const IPA_NODE* node);
PROJECTED_REGION* IPA_get_proj_region_array (const IPA_NODE* node);
PROJECTED_NODE* IPA_get_projected_node_array (const IPA_NODE* node);
LOOPINFO* IPA_get_loopinfo_array (const IPA_NODE* node);
TERM* IPA_get_term_array (const IPA_NODE* node);
SCALAR_INFO* IPA_get_scalar_array (const IPA_NODE* node);
SUMMARY_FEEDBACK* IPA_get_feedback_array (const IPA_NODE* node);

#endif  // _STANDALONE_INLINER



#endif // cxx_ipa_summary_INCLUDED
