/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

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
/* ====================================================================
 * ====================================================================
 *
 * Module: ipl_summary.h
 * $Source: /proj/osprey/CVS/open64/osprey1.0/ipa/local/ipl_summary.h,v $
 *
 * Description:
 *    Describe data structures of the summary information.  We only define
 *    those structures that are written out to the file.  Aux. data
 *    structures should be defined elsewhere.
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef ipl_summary_INCLUDED
#define ipl_summary_INCLUDED

#ifndef language_INCLUDED
#include "language.h"
#endif // language_INCLUDED

#ifndef opcode_INCLUDED
#include "opcode.h"
#endif // opcode_INCLUDED

#ifndef tracing_INCLUDED
#include "tracing.h"			// for TFile
#endif // tracing_INCLUDED

#ifndef xstats_INCLUDED
#include "xstats.h"			// for bb counts
#endif // xstats_INCLUDED

#ifndef cxx_template_INCLUDED
#include "cxx_template.h" 
#endif 

#ifndef fb_whirl_INCLUDED
#include "fb_whirl.h" 
#endif 

#include <search.h> //for  qsort in reorder's Set_hot_fld()
#ifndef mempool_INCLUDED
#include "mempool.h" //for MEM_POOL in SUMMARY_STRUCT_ACCESS
#endif

#define IPA_SUMMARY_REVISION 29//will later to 29:0
#define IPA_SUMMARY_MINOR_REVISION 0

// Tracing flags

#define TT_IPL_IPA	0x01
#define TT_IPL_MODREF   0x02
#define TT_IPL_VERBOSE	0x04 
#define TT_IPL_SECTION  0x08 
#define TT_IPL_EXCOST	0x10 
#define TT_IPL_SIMPLIFY 0x20
#define TT_IPL_SUMMARY  0x40

struct IPC_GLOBAL_IDX_MAP;              // forward declaration

extern INT    ICALL_MAX_PROMOTE_PER_CALLSITE;

//////////////////////////////////////////////////////////////////
//                      IMPORTANT
// if you change any of the following -- data types, adding classes etc
// you MUST change IPA_SUMMARY_REVISION and the verifier in
// ipa/common/ipc_bread.c -- check_correct_summary_sizes
//////////////////////////////////////////////////////////////////
//----------------------------------------------------------------
// header information for the file
//----------------------------------------------------------------
class SUMMARY_FILE_HEADER
{
private:

  // version number
  mINT32 _version_number;
  mINT32 _minor_version_number;

  // offset for these arrays 
  Elf64_Word _symbol_offset, _proc_offset, _feedback_offset;
  Elf64_Word _callsite_offset, _stmt_offset, _ctrl_dep_offset;
  Elf64_Word _formal_offset, _actual_offset;
  Elf64_Word _value_offset, _expr_offset, _phi_offset, _chi_offset;
  Elf64_Word _global_offset;
  Elf64_Word _common_offset, _common_shape_offset;
  Elf64_Word _struct_access_offset;
#ifdef KEY
  Elf64_Word _ty_info_offset;
#endif
  // Constraint graph summary for Nystrom Alias Analyzer
  Elf64_Word _constraint_graph_nodes_offset;
  Elf64_Word _constraint_graph_edges_offset;
  Elf64_Word _constraint_graph_stinfos_offset;
  Elf64_Word _constraint_graph_callsites_offset;
  Elf64_Word _constraint_graph_node_ids_offset;
  Elf64_Word _constraint_graph_modranges_offset;

  // array section flow sensitive analysis information
  Elf64_Word _scalar_node_offset, _cfg_node_offset, _regions_array_offset;
  Elf64_Word _projected_region_offset;
  Elf64_Word _projected_array_offset, _term_array_offset;
  Elf64_Word _ivar_global_offset, _ivar_offset, _loopinfo_offset;
  Elf64_Word _global_stid_offset; 

  // number of entries for each of these arrays
  mINT32 _symbol_size, _proc_size, _feedback_size;
  mINT32 _callsite_size, _stmt_size, _ctrl_dep_size;
  mINT32 _formal_size, _actual_size;
  mINT32 _value_size, _expr_size, _phi_size, _chi_size, _global_size;
  mINT32 _common_size, _common_shape_size, _global_stid_size;
  mINT32 _struct_access_size;
#ifdef KEY
  mINT32 _ty_info_size;
#endif
  // Constraint graph summary for Nystrom Alias Analyzer
  mINT32 _constraint_graph_nodes_size;
  mINT32 _constraint_graph_edges_size;
  mINT32 _constraint_graph_stinfos_size;
  mINT32 _constraint_graph_callsites_size;
  mINT32 _constraint_graph_node_ids_size;
  mINT32 _constraint_graph_modranges_size;

  // array section flow sensitive analysis information
  mINT32 _scalar_node_size, _cfg_node_size, _regions_array_size; 
  mINT32 _projected_region_size;
  mINT32 _projected_array_size, _term_array_size;
  mINT32 _ivar_global_size, _ivar_size, _loopinfo_size;

  // size of each entry
  mINT32 _symbol_entry_size, _proc_entry_size, _feedback_entry_size;
  mINT32 _callsite_entry_size, _stmt_entry_size, _ctrl_dep_entry_size;
  mINT32 _formal_entry_size, _actual_entry_size;
  mINT32 _value_entry_size, _expr_entry_size, _phi_entry_size;
  mINT32 _chi_entry_size, _global_entry_size;
  mINT32 _common_entry_size, _common_shape_entry_size, _global_stid_entry_size;
  mUINT32 _struct_access_entry_size;
#ifdef KEY
  mUINT32 _ty_info_entry_size;
#endif
  // Constraint graph summary for Nystrom Alias Analyzer
  mUINT32 _constraint_graph_nodes_entry_size;
  mUINT32 _constraint_graph_edges_entry_size;
  mUINT32 _constraint_graph_stinfos_entry_size;
  mUINT32 _constraint_graph_callsites_entry_size;
  mUINT32 _constraint_graph_node_ids_entry_size;
  mUINT32 _constraint_graph_modranges_entry_size;

   // array section flow sensitive analysis information
  mINT32 _scalar_node_entry_size, _cfg_node_entry_size;
  mINT32 _regions_array_entry_size; 
  mINT32 _projected_region_entry_size;
  mINT32 _projected_array_entry_size, _term_array_entry_size;
  mINT32 _ivar_global_entry_size, _ivar_entry_size, _loopinfo_entry_size;

  mUINT8 _opt_level;
  mBOOL _run_autopar;

public:

  SUMMARY_FILE_HEADER() {
    BZERO(this, sizeof(SUMMARY_FILE_HEADER));
  };

  void Set_version_number(mINT32 i) { _version_number = i;};
  void Set_minor_version_number(mINT32 i) {_minor_version_number = i;};
  void Set_symbol_offset(Elf64_Word s) { _symbol_offset = s; };
  void Set_proc_offset(Elf64_Word s) { _proc_offset = s;};
  void Set_feedback_offset(Elf64_Word s) { _feedback_offset = s;};
  void Set_callsite_offset(Elf64_Word s) { _callsite_offset = s;};
  void Set_stmt_offset(Elf64_Word s) { _stmt_offset = s;};
  void Set_ctrl_dep_offset(Elf64_Word s) { _ctrl_dep_offset = s;};
  void Set_formal_offset(Elf64_Word s) { _formal_offset = s;};
  void Set_actual_offset(Elf64_Word s) { _actual_offset = s;};
  void Set_value_offset(Elf64_Word s) { _value_offset = s;};
  void Set_expr_offset(Elf64_Word s) { _expr_offset = s;};
  void Set_phi_offset(Elf64_Word s) { _phi_offset = s;};
  void Set_chi_offset(Elf64_Word s) { _chi_offset = s;};
  void Set_global_offset(Elf64_Word s) { _global_offset = s;};
  void Set_common_offset(Elf64_Word s) { _common_offset = s;};
  void Set_common_shape_offset(Elf64_Word s) { _common_shape_offset = s;};
  void Set_global_stid_offset(Elf64_Word s) { _global_stid_offset = s;};
  void Set_struct_access_offset(Elf64_Word s) { _struct_access_offset =	 s;};
#ifdef KEY
  void Set_ty_info_offset(Elf64_Word s) { _ty_info_offset = s;};
#endif
  // Constraint graph summary for Nystrom Alias Analyzer
  void Set_constraint_graph_nodes_offset(Elf64_Word s) 
  {
    _constraint_graph_nodes_offset = s;
  }
  void Set_constraint_graph_edges_offset(Elf64_Word s) 
  {
    _constraint_graph_edges_offset = s;
  }
  void Set_constraint_graph_stinfos_offset(Elf64_Word s) 
  {
    _constraint_graph_stinfos_offset = s;
  }
  void Set_constraint_graph_callsites_offset(Elf64_Word s) 
  {
    _constraint_graph_callsites_offset = s;
  }
  void Set_constraint_graph_node_ids_offset(Elf64_Word s) 
  {
    _constraint_graph_node_ids_offset = s;
  }
  void Set_constraint_graph_modranges_offset(Elf64_Word s) 
  {
    _constraint_graph_modranges_offset = s;
  }

  void Set_opt_level(mUINT8 opt_level) { _opt_level = opt_level;};

  // array section flow sensitive analysis information
  void Set_scalar_node_offset(Elf64_Word s) { _scalar_node_offset = s;};
  void Set_cfg_node_offset(Elf64_Word s) { _cfg_node_offset = s;};
  void Set_regions_array_offset(Elf64_Word s){ _regions_array_offset = s;};
  void Set_projected_region_offset(Elf64_Word s)
    { _projected_region_offset = s ;};
  void Set_projected_array_offset(Elf64_Word s)
    { _projected_array_offset = s;};
  void Set_term_array_offset(Elf64_Word s)
    { _term_array_offset = s;};
  void Set_ivar_global_offset(Elf64_Word s)
    {_ivar_global_offset = s; };
  void Set_ivar_offset(Elf64_Word s )
    {_ivar_offset = s; };
  void Set_loopinfo_offset(Elf64_Word s )
    {_loopinfo_offset = s; };

  void Set_symbol_size(mINT32 s) { _symbol_size = s; };
  void Set_proc_size(mINT32 s) { _proc_size = s;};
  void Set_feedback_size(mINT32 s) { _feedback_size = s;};
  void Set_callsite_size(mINT32 s) { _callsite_size = s;};
  void Set_stmt_size(mINT32 s) { _stmt_size = s;};
  void Set_ctrl_dep_size(mINT32 s) { _ctrl_dep_size = s;};
  void Set_formal_size(mINT32 s) { _formal_size = s;};
  void Set_actual_size(mINT32 s) { _actual_size = s;};
  void Set_value_size(mINT32 s) { _value_size = s;};
  void Set_expr_size(mINT32 s) { _expr_size = s;};
  void Set_phi_size(mINT32 s) { _phi_size = s;};
  void Set_chi_size(mINT32 s) { _chi_size = s;};
  void Set_global_size(mINT32 s) { _global_size = s;};
  void Set_common_size(mINT32 s) { _common_size = s;};
  void Set_common_shape_size(mINT32 s) { _common_shape_size =
s;};
  void Set_global_stid_size(mINT32 s) { _global_stid_size =
s;};
  void Set_struct_access_size(mINT32 s) { _struct_access_size = s;};
#ifdef KEY
  void Set_ty_info_size(mINT32 s) { _ty_info_size = s;};
#endif
  // Constraint graph summary for Nystrom Alias Analyzer
  void Set_constraint_graph_nodes_size(mINT32 s) 
  { 
    _constraint_graph_nodes_size = s; 
  }
  void Set_constraint_graph_edges_size(mINT32 s) 
  { 
    _constraint_graph_edges_size = s; 
  }
  void Set_constraint_graph_stinfos_size(mINT32 s) 
  { 
    _constraint_graph_stinfos_size = s; 
  }
  void Set_constraint_graph_callsites_size(mINT32 s) 
  { 
    _constraint_graph_callsites_size = s; 
  }
  void Set_constraint_graph_node_ids_size(mINT32 s) 
  { 
    _constraint_graph_node_ids_size = s; 
  }
  void Set_constraint_graph_modranges_size(mINT32 s) 
  { 
    _constraint_graph_modranges_size = s; 
  }

  // array section flow sensitive analysis information
  void Set_scalar_node_size(mINT32 s) { _scalar_node_size = s;};
  void Set_cfg_node_size(mINT32 s) { _cfg_node_size = s; };
  void Set_regions_array_size(mINT32 s) { _regions_array_size = s;};
  void Set_projected_region_size(mINT32 s) 
    { _projected_region_size = s ;};
  void Set_projected_array_size(mINT32 s) { _projected_array_size  = s;};
  void Set_term_array_size(mINT32 s) { _term_array_size = s;};
  void Set_ivar_global_size(mINT32 s) {_ivar_global_size = s;};
  void Set_ivar_size(mINT32 s) {_ivar_size = s;};
  void Set_loopinfo_size(mINT32 s) {_loopinfo_size = s;};

  void Set_symbol_entry_size(mINT32 s) { _symbol_entry_size = s; };
 
  void Set_proc_entry_size(mINT32 s) { _proc_entry_size = s;};
  void Set_feedback_entry_size(mINT32 s) { _feedback_entry_size = s;};
  void Set_callsite_entry_size(mINT32 s) { _callsite_entry_size = s;};
  void Set_stmt_entry_size(mINT32 s) { _stmt_entry_size = s;};
  void Set_ctrl_dep_entry_size(mINT32 s) { _ctrl_dep_entry_size = s;};
  void Set_formal_entry_size(mINT32 s) { _formal_entry_size = s;};
  void Set_actual_entry_size(mINT32 s) { _actual_entry_size = s;};
  void Set_value_entry_size(mINT32 s) { _value_entry_size = s;};
  void Set_expr_entry_size(mINT32 s) { _expr_entry_size = s;};
  void Set_phi_entry_size(mINT32 s) { _phi_entry_size = s;};
  void Set_chi_entry_size(mINT32 s) { _chi_entry_size = s;};
  void Set_global_entry_size(mINT32 s) { _global_entry_size = s;};
  void Set_common_entry_size(mINT32 s) { _common_entry_size = s;};
  void Set_common_shape_entry_size(mINT32 s) {
    _common_shape_entry_size = s;};
  void Set_global_stid_entry_size(mINT32 s) {
    _global_stid_entry_size = s;};
  void Set_struct_access_entry_size(mINT32 s) {
    _struct_access_entry_size = s;};
#ifdef KEY
  void Set_ty_info_entry_size(mINT32 s) { _ty_info_entry_size = s;};
#endif   
  // Constraint graph summary for Nystrom Alias Analyzer
  void Set_constraint_graph_nodes_entry_size(mINT32 s) { _constraint_graph_nodes_entry_size = s;};
  void Set_constraint_graph_edges_entry_size(mINT32 s) { _constraint_graph_edges_entry_size = s;};
  void Set_constraint_graph_stinfos_entry_size(mINT32 s) { _constraint_graph_stinfos_entry_size = s;};
  void Set_constraint_graph_callsites_entry_size(mINT32 s) { _constraint_graph_callsites_entry_size = s;};
  void Set_constraint_graph_node_ids_entry_size(mINT32 s) { _constraint_graph_node_ids_entry_size = s;};
  void Set_constraint_graph_modranges_entry_size(mINT32 s) { _constraint_graph_modranges_entry_size = s;};

  void Set_scalar_node_entry_size(mINT32 s) { _scalar_node_entry_size = s;};
  void Set_cfg_node_entry_size(mINT32 s) {_cfg_node_entry_size = s;};
  void Set_regions_array_entry_size(mINT32 s)
      { _regions_array_entry_size = s;};
  void Set_projected_region_entry_size(mINT32 s) 
      { _projected_region_entry_size = s ;};
  void Set_projected_array_entry_size(mINT32 s)
      { _projected_array_entry_size = s;};
  void Set_term_array_entry_size(mINT32 s) 
    { _term_array_entry_size = s;};
  void Set_ivar_global_entry_size(mINT32 s) 
    { _ivar_global_entry_size = s;};
  void Set_ivar_entry_size(mINT32 s)
    { _ivar_entry_size = s; };
  void Set_loopinfo_entry_size(mINT32 s)
    { _loopinfo_entry_size = s; };

  Elf64_Word Get_symbol_offset() const	{ return _symbol_offset;};
  Elf64_Word Get_proc_offset() const	{ return _proc_offset;};
  Elf64_Word Get_feedback_offset() const{ return _feedback_offset;};
  Elf64_Word Get_callsite_offset() const{ return _callsite_offset;};
  Elf64_Word Get_stmt_offset() const	{ return _stmt_offset;};
  Elf64_Word Get_ctrl_dep_offset() const{ return _ctrl_dep_offset;};
  Elf64_Word Get_formal_offset() const	{ return _formal_offset;};
  Elf64_Word Get_actual_offset() const	{ return _actual_offset;};
  Elf64_Word Get_value_offset() const	{ return _value_offset;};
  Elf64_Word Get_expr_offset() const	{ return _expr_offset;};
  Elf64_Word Get_phi_offset() const	{ return _phi_offset;};
  Elf64_Word Get_chi_offset() const	{ return _chi_offset;};
  Elf64_Word Get_global_offset() const	{ return _global_offset;};
  Elf64_Word Get_common_offset() const	{ return _common_offset;};
  Elf64_Word Get_common_shape_offset() const { return _common_shape_offset;};
  Elf64_Word Get_global_stid_offset() const { return _global_stid_offset;};
  Elf64_Word Get_struct_access_offset() const { return _struct_access_offset;};
#ifdef KEY
  Elf64_Word Get_ty_info_offset() const    { return _ty_info_offset;};
#endif
  // Constraint graph summary for Nystrom Alias Analyzer
  Elf64_Word Get_constraint_graph_nodes_offset() const  
  { 
    return _constraint_graph_nodes_offset;
  };
  Elf64_Word Get_constraint_graph_edges_offset() const  
  { 
    return _constraint_graph_edges_offset;
  };
  Elf64_Word Get_constraint_graph_stinfos_offset() const  
  { 
    return _constraint_graph_stinfos_offset;
  };
  Elf64_Word Get_constraint_graph_callsites_offset() const  
  { 
    return _constraint_graph_callsites_offset;
  };
  Elf64_Word Get_constraint_graph_node_ids_offset() const  
  { 
    return _constraint_graph_node_ids_offset;
  };
  Elf64_Word Get_constraint_graph_modranges_offset() const  
  { 
    return _constraint_graph_modranges_offset;
  };

  mUINT8  Get_opt_level() const { return _opt_level;};

  // array section flow sensitive analysis information
  Elf64_Word Get_cfg_node_offset() const { return _cfg_node_offset;};
  Elf64_Word Get_regions_array_offset() const 
      { return _regions_array_offset; };
  Elf64_Word Get_projected_region_offset() const
      { return _projected_region_offset;};
  Elf64_Word  Get_projected_array_offset() const
      { return _projected_array_offset; };
  Elf64_Word  Get_term_array_offset() const
      { return _term_array_offset;};
  Elf64_Word  Get_ivar_global_offset() const
      { return  _ivar_global_offset; };
  Elf64_Word Get_ivar_offset() const
      { return _ivar_offset; };
  Elf64_Word Get_loopinfo_offset() const
      { return _loopinfo_offset; };
  Elf64_Word Get_scalar_offset() const
      { return _scalar_node_offset; };

  mINT32 Get_version_number() const	{ return _version_number;};
  mINT32 Get_minor_version_number() const { return _minor_version_number;};
  mINT32 Get_symbol_size() const	{ return _symbol_size;};
  mINT32 Get_proc_size() const		{ return _proc_size;};
  mINT32 Get_feedback_size() const	{ return _feedback_size;};
  mINT32 Get_callsite_size() const	{ return _callsite_size;};
  mINT32 Get_stmt_size() const		{ return _stmt_size;};
  mINT32 Get_ctrl_dep_size() const	{ return _ctrl_dep_size;};
  mINT32 Get_formal_size() const	{ return _formal_size;};
  mINT32 Get_actual_size() const	{ return _actual_size;};
  mINT32 Get_value_size() const		{ return _value_size;};
  mINT32 Get_expr_size() const		{ return _expr_size;};
  mINT32 Get_phi_size() const		{ return _phi_size;};
  mINT32 Get_chi_size() const		{ return _chi_size;};
  mINT32 Get_global_size() const	{ return _global_size;};
  mINT32 Get_common_size() const	{ return _common_size;};
  mINT32 Get_common_shape_size() const	{ return _common_shape_size;};
  mINT32 Get_global_stid_size() const	{ return _global_stid_size;};
  mINT32 Get_struct_access_size() const	{ return _struct_access_size;};
#ifdef KEY
  mINT32 Get_ty_info_size() const          { return _ty_info_size;};
#endif
  // Constraint graph summary for Nystrom Alias Analyzer
  mINT32 Get_constraint_graph_nodes_size() const
  {
    return _constraint_graph_nodes_size;
  }
  mINT32 Get_constraint_graph_edges_size() const
  {
    return _constraint_graph_edges_size;
  }
  mINT32 Get_constraint_graph_stinfos_size() const
  {
    return _constraint_graph_stinfos_size;
  }
  mINT32 Get_constraint_graph_callsites_size() const
  {
    return _constraint_graph_callsites_size;
  }
  mINT32 Get_constraint_graph_node_ids_size() const
  {
    return _constraint_graph_node_ids_size;
  }
  mINT32 Get_constraint_graph_modranges_size() const
  {
    return _constraint_graph_modranges_size;
  }
  
  // array section flow sensitive analysis information
  mINT32 Get_scalar_node_size()  const { return _scalar_node_size; };
  mINT32 Get_cfg_node_size()  const { return _cfg_node_size; };
  mINT32 Get_regions_array_size() const { return _regions_array_size;};
  mINT32 Get_projected_region_size() const
    { return _projected_region_size;};
  mINT32 Get_projected_array_size() const { return _projected_array_size;};
  mINT32 Get_term_array_size() const { return _term_array_size;};
  mINT32 Get_ivar_global_size() const {return _ivar_global_size;};
  mINT32 Get_ivar_size() const { return _ivar_size; };
  mINT32 Get_loopinfo_size() const { return _loopinfo_size; };

  mINT32 Get_symbol_entry_size() { return _symbol_entry_size;};
  mINT32 Get_proc_entry_size() {return _proc_entry_size;};
  mINT32 Get_feedback_entry_size() {return _feedback_entry_size;};
  mINT32 Get_callsite_entry_size() {return _callsite_entry_size;};
  mINT32 Get_stmt_entry_size() {return _stmt_entry_size;};
  mINT32 Get_ctrl_dep_entry_size() {return _ctrl_dep_entry_size;};
  mINT32 Get_formal_entry_size() {return _formal_entry_size;};
  mINT32 Get_actual_entry_size() {return _actual_entry_size;};
  mINT32 Get_value_entry_size() {return _value_entry_size;};
  mINT32 Get_expr_entry_size() {return _expr_entry_size;};
  mINT32 Get_phi_entry_size() {return _phi_entry_size;};
  mINT32 Get_chi_entry_size() {return _chi_entry_size;};
  mINT32 Get_global_entry_size() { return _global_entry_size;};
  mINT32 Get_common_entry_size() { return _common_entry_size;};
  mINT32 Get_common_shape_entry_size() { return _common_shape_entry_size;};
  mINT32 Get_struct_access_entry_size() { return _struct_access_entry_size;};
#ifdef KEY
  mINT32 Get_ty_info_entry_size() { return _ty_info_entry_size;};
#endif
  // Constraint graph summary for Nystrom Alias Analyzer
  mINT32 Get_constraint_graph_nodes_entry_size() 
  { 
    return _constraint_graph_nodes_entry_size;
  };
  mINT32 Get_constraint_graph_edges_entry_size() 
  { 
    return _constraint_graph_edges_entry_size;
  };
  mINT32 Get_constraint_graph_stinfos_entry_size() 
  { 
    return _constraint_graph_stinfos_entry_size;
  };
  mINT32 Get_constraint_graph_callsites_entry_size() 
  { 
    return _constraint_graph_callsites_entry_size;
  };
  mINT32 Get_constraint_graph_node_ids_entry_size() 
  { 
    return _constraint_graph_node_ids_entry_size;
  };
  mINT32 Get_constraint_graph_modranges_entry_size() 
  { 
    return _constraint_graph_modranges_entry_size;
  };
    
  mINT32 Get_scalar_node_entry_size() const  { 
    return _scalar_node_entry_size ;};
  mINT32 Get_cfg_node_entry_size() const  { return _cfg_node_entry_size ;};
  mINT32 Get_regions_array_entry_size() const {
    return _regions_array_entry_size;};
  mINT32 Get_projected_region_entry_size() const
    { return _projected_region_entry_size;};
  mINT32 Get_projected_array_entry_size() const { 
    return _projected_array_entry_size;};
  mINT32 Get_term_array_entry_size() const
    { return _term_array_entry_size;};
  mINT32 Get_ivar_global_entry_size() const
    { return _ivar_global_entry_size;};
  mINT32 Get_ivar_entry_size() const
    { return _ivar_entry_size;};
  mINT32 Get_loopinfo_entry_size() const
    { return _loopinfo_entry_size;};
  mINT32 Get_global_stid_entry_size() const
    { return _global_stid_entry_size;};

  BOOL Run_AutoPar() const { return _run_autopar; }
  void Set_AutoPar() { _run_autopar = TRUE; }

}; // SUMMARY_FILE_HEADER


//-----------------------------------------------------------------
// all information for a procedure must be stored here
//-----------------------------------------------------------------
class SUMMARY_PROCEDURE
{
private:
    
    mINT32 _symbol_index;
    mINT32 _callsite_index;
    mINT32 _ctrl_dep_index;
    mINT32 _formal_index;
    mINT32 _global_index;
    mINT32 _common_index;
    mINT32 _feedback_index;
    
    mINT32 _array_section_index;
    mINT32 _array_section_count;
    mINT32 _ex_value_index; 
    mINT32 _ex_value_count; 
    mINT32 _ex_expr_index; 
    mINT32 _ex_expr_count; 

    mUINT32 _state;

#ifdef KEY // bug 10289
    mUINT32 _bb_count;
    mUINT32 _stmt_count;
    mUINT32 _call_count;
#else
    mUINT16 _bb_count;
    mUINT16 _stmt_count;
    mUINT16 _call_count;
#endif
    // number of alternate entry points in the procedure
    mUINT16 _alt_entry_count; 

#ifdef KEY // bug 10289
    mUINT32 _callsite_count;
#else
    mUINT16 _callsite_count;
#endif
    mUINT16 _ctrl_dep_count;
    mUINT16 _formal_count;
    mUINT16 _global_count;
    mUINT16 _common_count;

// bit fields for _state 
#define PROC_MAY_INLINE			0x00000001
#define PROC_MUST_INLINE		0x00000002
#define PROC_NO_INLINE		        0x00000004
#define PROC_VARARGS		        0x00000008
#define PROC_ALT_ENTRY		        0x00000010
#define PROC_HAS_ALT_ENTRY	        0x00000020
#define PROC_HAS_PSTATIC                0x00000040
#define PROC_NO_DELETE                  0x00000080
#define PROC_BLOCK_DATA                 0x00000100
#define PROC_DMOD_DREF                  0x00000200
#define PROC_EXC_SCOPE 	                0x00000400
#define PROC_HAS_UNSTRUCTURED_CPROP     0x00000800
#define PROC_HAS_ADDR_TAKEN_RESET	0x00001000
#define PROC_HAS_PU_FREQ		0x00002000
#define PROC_HAS_FORMAL_PRAGMA          0x00004000
#define PROC_HAS_PARALLEL_PRAGMA        0x00008000
#define PROC_HAS_PARALLEL_REGION_PRAGMA 0x00010000
#define PROC_HAS_FSTATIC		0x00020000
#define PROC_USE_LOWERED_RETURN_PREG	0x00040000 // should be removed by 7.3
#ifdef KEY
#define PROC_HAS_SIDE_EFFECT		0x00080000
#else
// obsolete				0x00080000
#endif
#define PROC_HAS_UNKNOWN_CALLS		0x00100000 // this PU has unknown calls
#define PROC_HAS_INCOMPLETE_ARRAY_INFO  0x00200000 
#define PROC_HAS_MP_NEEDS_LNO		0x00400000 // this PU's local symtab has 
						   // bit SYMTAB_mp_needs_lno set
#define PROC_HAS_EXC_TRY_REGION		0x00800000 // does it have a try region?
#ifdef KEY
#define PROC_HAS_PRAGMA_SIDE_EFFECT	0x01000000 // this PU's contains pragma 
#else
#define PROC_HAS_SIDE_EFFECT		0x01000000 // this PU's contains pragma 
#endif
                                                   // with side effect
#define PROC_HAS_MESSY_REGIONS		0x02000000 // messy PROJECTED_REGIONs
#define PROC_HAS_EARLY_RETURNS		0x04000000 // has RETURN stmts not
						   // at the end of the PU
#define PROC_HAS_VAR_DIM_ARRAY		0x08000000 // has variable-dimensioned
						   // array as formals

#define PROC_HAS_NOINLINE_PARALLEL_PRAGMA 0x10000000 // not inline PUs with 
						   // WN_PRAGMA_PARALLEL_BEGIN
						   // WN_PRAGMA_PARALLEL_DO
						   // WN_PRAGMA_DOACROSS9
						   // WN_PRAGMA_PARALLEL_SECTIONS
#define PROC_HAS_PDO_PRAGMA 		0x20000000 // TY_is_non_pod set for
						   // LOCAL pragma's st 
#define PROC_NEVER_INVOKED 		0x40000000 // INLINING_TUNING
    LANGUAGE _l;

    // For Nystrom alias analyzer
    mUINT32 _constraint_graph_nodes_idx;
    mUINT32 _constraint_graph_nodes_count;
    mUINT32 _constraint_graph_edges_idx;
    mUINT32 _constraint_graph_edges_count;
    mUINT32 _constraint_graph_stinfos_idx;
    mUINT32 _constraint_graph_stinfos_count;
    mUINT32 _constraint_graph_callsites_idx;
    mUINT32 _constraint_graph_callsites_count;
    mUINT32 _constraint_graph_node_ids_idx;
    mUINT32 _constraint_graph_node_ids_count;
    mUINT32 _constraint_graph_formal_parm_idx;
    mUINT32 _constraint_graph_formal_parm_count;
    mUINT32 _constraint_graph_formal_ret_idx;
    mUINT32 _constraint_graph_formal_ret_count;

public:

    /* access functions */

    void Set_size (UINT16 bb, UINT16 stmt, UINT16 call) {
	_bb_count = bb;
	_stmt_count = stmt;
	_call_count = call;
    }

    UINT16 Get_bb_count () const		{ return _bb_count; }
    UINT16 Get_stmt_count () const		{ return _stmt_count; }
    UINT16 Get_call_count () const		{ return _call_count; }

    void Set_altentry_count (mUINT16 a)		{ _alt_entry_count = a;}
    mUINT16 Get_altentry_count () const		{ return _alt_entry_count;}


    void Set_symbol_index (INT32 s)		{ _symbol_index = s; }
    INT32 Get_symbol_index () const		{ return _symbol_index; }

#ifdef KEY
    void Set_bb_count (UINT16 bbs)		{ _bb_count = bbs; }
    void Set_stmt_count (UINT16 stmts)		{ _stmt_count = stmts; }
    void Set_call_count (UINT16 calls)		{ _call_count = calls; }

// Use proper types of mINT32, otherwise the value gets truncated while
// assigning.
    void Set_callsite_index (mINT32 c)		{ _callsite_index = c;}
    mINT32 Get_callsite_index () const		{ return _callsite_index;}

    void Set_ctrl_dep_index (mINT32 c)		{ _ctrl_dep_index = c;}
    mINT32 Get_ctrl_dep_index () const		{ return _ctrl_dep_index;}

    void Set_formal_index (mINT32 f)		{ _formal_index = f;}
    mINT32 Get_formal_index () const		{ return _formal_index;}
#else
    void Set_callsite_index (mUINT16 c)		{ _callsite_index = c;}
    mUINT16 Get_callsite_index () const		{ return _callsite_index;}

    void Set_ctrl_dep_index (mUINT16 c)		{ _ctrl_dep_index = c;}
    mUINT16 Get_ctrl_dep_index () const		{ return _ctrl_dep_index;}

    void Set_formal_index (mUINT16 f)		{ _formal_index = f;}
    mUINT16 Get_formal_index () const		{ return _formal_index;}
#endif
    
    void Set_global_index (INT g)		{ _global_index = g;}
    INT Get_global_index() const		{ return _global_index;}

    void Set_common_index (INT32 i)		{ _common_index = i;}
    INT32 Get_common_index () const		{ return _common_index; }

    void Set_feedback_index (INT32 i)		{ _feedback_index = i; }
    INT32 Get_feedback_index () const		{ return _feedback_index; }

    void Set_array_section_index (INT32 i)	{ _array_section_index = i;}
    INT32 Get_array_section_index () const	{return _array_section_index; }

    void Set_callsite_count (mUINT16 c)		{ _callsite_count = c;}
    UINT16 Get_callsite_count () const		{ return _callsite_count;}

    void Set_ctrl_dep_count (mUINT16 c)		{ _ctrl_dep_count = c;}
    UINT16 Get_ctrl_dep_count () const		{ return _ctrl_dep_count;}

    void Set_formal_count (mUINT16 cnt)		{ _formal_count = cnt;}
    mUINT16 Get_formal_count () const		{ return _formal_count;}

    void Set_global_count (INT g)		{ _global_count  = g;}
    INT Get_global_count () const		{ return  _global_count;}

    void Set_common_count (INT32 i)		{ _common_count = i;}
    INT32 Get_common_count () const		{ return _common_count;}

    void Set_array_section_count (INT32 i)	{ _array_section_count = i;}
    INT32 Get_array_section_count () const	{ return
						    _array_section_count;}

    INT32 Get_ex_value_index() const		{ return _ex_value_index;}
    INT32 Get_ex_expr_index() const	 	{ return _ex_expr_index;}
    void Set_ex_value_index(INT32 i) 		{ _ex_value_index = i;}
    void Set_ex_expr_index(INT32 i) 		{ _ex_expr_index = i;}

    INT32 Get_ex_value_count() const		{ return _ex_value_count;}
    INT32 Get_ex_expr_count() const	 	{ return _ex_expr_count;}
    void Set_ex_value_count(INT32 i) 		{ _ex_value_count = i;}
    void Set_ex_expr_count(INT32 i) 		{ _ex_expr_count = i;}

    UINT32 Get_state () const		{ return _state; }

    void Set_has_unstructured_cflow()   { _state |=
					    PROC_HAS_UNSTRUCTURED_CPROP; }
    BOOL Has_unstructured_cflow() const { return (_state & 
					    PROC_HAS_UNSTRUCTURED_CPROP); }

    void Set_may_inline ()		{ _state |= PROC_MAY_INLINE; }
    BOOL Is_may_inline () const		{ return (_state & PROC_MAY_INLINE); }

    void Set_must_inline ()		{ _state |= PROC_MUST_INLINE; }
    BOOL Is_must_inline () const	{ return (_state & PROC_MUST_INLINE); }

    void Set_no_inline ()		{ _state |= PROC_NO_INLINE;}
    BOOL Is_no_inline() const		{ return (_state & PROC_NO_INLINE);}

    void Set_is_varargs()		{ _state |= PROC_VARARGS;}
    BOOL Is_varargs() const		{ return (_state & PROC_VARARGS);}

    void Set_alt_entry()		{ _state |= PROC_ALT_ENTRY;}
    BOOL Is_alt_entry() const		{ return (_state & PROC_ALT_ENTRY);}

    void Set_has_alt_entry()		{ _state |= PROC_HAS_ALT_ENTRY;}
    BOOL Has_alt_entry() const		{ return (_state & PROC_HAS_ALT_ENTRY);}

    void Set_has_pstatic()		{ _state |= PROC_HAS_PSTATIC;}
    void Reset_has_pstatics()		{ _state &= ~PROC_HAS_PSTATIC; }
    BOOL Has_pstatic() const		{ return (_state & PROC_HAS_PSTATIC);}

    void Set_no_delete()		{ _state |= PROC_NO_DELETE;}
    BOOL Is_no_delete() const		{ return (_state & PROC_NO_DELETE);}
    
    void Set_block_data()		{ _state |= PROC_BLOCK_DATA;}
    BOOL Is_block_data() const		{ return (_state & PROC_BLOCK_DATA);}
			   
    void Set_direct_mod_ref()		{ _state |= PROC_DMOD_DREF; }
    BOOL Is_direct_mod_ref() const	{ return _state & PROC_DMOD_DREF;}

    void Set_exc_inline()		{ _state |= PROC_EXC_SCOPE;}
    BOOL Is_exc_inline() const		{ return _state & PROC_EXC_SCOPE;}

    void Set_has_addr_taken_reset()	{ _state |= PROC_HAS_ADDR_TAKEN_RESET; }
    BOOL Has_addr_taken_reset() const	{ return (_state & PROC_HAS_ADDR_TAKEN_RESET); }

    void Set_has_PU_freq ()		{ _state |= PROC_HAS_PU_FREQ; }
    BOOL Has_PU_freq () const		{ return _state & PROC_HAS_PU_FREQ; }
    void Clear_has_PU_freq () 	 	{ _state &= ~PROC_HAS_PU_FREQ; }

 
 // INLINING_TUNING^
    void Set_Never_Invoked ()		{ _state |= PROC_NEVER_INVOKED; }
    BOOL Is_Never_Invoked() const		{ return _state & PROC_NEVER_INVOKED; }
    void Clear_Never_Invoked () 	 	{ _state &= ~PROC_HAS_PU_FREQ; }
 // INLINING_TUNING$
	
    void Set_has_formal_pragma()	{ _state |= PROC_HAS_FORMAL_PRAGMA;}
    BOOL Has_formal_pragma() const	{ return _state & PROC_HAS_FORMAL_PRAGMA;}

    void Set_has_parallel_pragma()	{ _state |= PROC_HAS_PARALLEL_PRAGMA;}
    BOOL Has_parallel_pragma() const	{ return _state & PROC_HAS_PARALLEL_PRAGMA;}

    void Set_has_parallel_region_pragma() { _state |= PROC_HAS_PARALLEL_REGION_PRAGMA;}
    BOOL Has_parallel_region_pragma() const { return _state & PROC_HAS_PARALLEL_REGION_PRAGMA;}
 
    void Set_has_noinline_parallel_pragma() { _state |= PROC_HAS_NOINLINE_PARALLEL_PRAGMA;}
    BOOL Has_noinline_parallel_pragma() const { return _state & PROC_HAS_NOINLINE_PARALLEL_PRAGMA;}
 
    void Set_has_pdo_pragma() 		{ _state |= PROC_HAS_PDO_PRAGMA;}
    BOOL Has_pdo_pragma() const 	{ return _state & PROC_HAS_PDO_PRAGMA;}

    void Set_has_fstatic ()		{ _state |= PROC_HAS_FSTATIC; }
    BOOL Has_fstatic() const		{ return (_state & PROC_HAS_FSTATIC); }

    void Set_use_lowered_return_preg () { _state |=
					      PROC_USE_LOWERED_RETURN_PREG; 
    }
    BOOL Use_lowered_return_preg () const { return _state & PROC_USE_LOWERED_RETURN_PREG; }

    void Set_has_unknown_calls ()       { _state |= PROC_HAS_UNKNOWN_CALLS; }
    BOOL Has_unknown_calls () const	{ return _state & PROC_HAS_UNKNOWN_CALLS; }

    void Set_has_incomplete_array_info () 
      { _state |= PROC_HAS_INCOMPLETE_ARRAY_INFO; }
    BOOL Has_incomplete_array_info () const 
      { return _state & PROC_HAS_INCOMPLETE_ARRAY_INFO; }

    void Set_has_mp_needs_lno ()  { _state |= PROC_HAS_MP_NEEDS_LNO; }
    BOOL Has_mp_needs_lno () const { return _state & PROC_HAS_MP_NEEDS_LNO; }

    void Set_exc_try() { _state |= PROC_HAS_EXC_TRY_REGION;};
    BOOL Has_exc_try() const { return _state &
				 PROC_HAS_EXC_TRY_REGION; };

#ifdef KEY
    void Set_has_pragma_side_effect ()  { _state |= PROC_HAS_PRAGMA_SIDE_EFFECT; }
    BOOL Has_pragma_side_effect () const { return _state & PROC_HAS_PRAGMA_SIDE_EFFECT; }
#endif
    void Set_has_side_effect ()  { _state |= PROC_HAS_SIDE_EFFECT; }
    BOOL Has_side_effect () const { return _state & PROC_HAS_SIDE_EFFECT; }

    void Set_has_messy_regions ()  { _state |= PROC_HAS_MESSY_REGIONS; }
    BOOL Has_messy_regions () const { return _state & PROC_HAS_MESSY_REGIONS; }

    void Set_has_early_returns ()	{ _state |= PROC_HAS_EARLY_RETURNS; }
    BOOL Has_early_returns() const	{ return (_state & PROC_HAS_EARLY_RETURNS); }

    void Set_has_var_dim_array ()	{ _state |= PROC_HAS_VAR_DIM_ARRAY; }
    BOOL Has_var_dim_array() const	{ return (_state & PROC_HAS_VAR_DIM_ARRAY); }

    void Set_lang (LANGUAGE lang)	{ _l = lang; }
    LANGUAGE Get_lang() const		{ return _l; }

    // For the Nystrom alias analyzer
    mUINT32 Get_constraint_graph_nodes_idx() const
    {
      return _constraint_graph_nodes_idx;
    }
    void Set_constraint_graph_nodes_idx(mUINT32 i)
    {
      _constraint_graph_nodes_idx = i;
    }
    mUINT32 Get_constraint_graph_nodes_count() const
    {
      return _constraint_graph_nodes_count;
    }
    void Set_constraint_graph_nodes_count(mUINT32 c)
    {
      _constraint_graph_nodes_count = c;
    }

    mUINT32 Get_constraint_graph_edges_idx() const
    {
      return _constraint_graph_edges_idx;
    }
    void Set_constraint_graph_edges_idx(mUINT32 i)
    {
      _constraint_graph_edges_idx = i;
    }
    mUINT32 Get_constraint_graph_edges_count() const
    {
      return _constraint_graph_edges_count;
    }
    void Set_constraint_graph_edges_count(mUINT32 c)
    {
      _constraint_graph_edges_count = c;
    }

    mUINT32 Get_constraint_graph_stinfos_idx() const
    {
      return _constraint_graph_stinfos_idx;
    }
    void Set_constraint_graph_stinfos_idx(mUINT32 i)
    {
      _constraint_graph_stinfos_idx = i;
    }
    mUINT32 Get_constraint_graph_stinfos_count() const
    {
      return _constraint_graph_stinfos_count;
    }
    void Set_constraint_graph_stinfos_count(mUINT32 c)
    {
      _constraint_graph_stinfos_count = c;
    }

    mUINT32 Get_constraint_graph_callsites_idx() const
    {
      return _constraint_graph_callsites_idx;
    }
    void Set_constraint_graph_callsites_idx(mUINT32 i)
    {
      _constraint_graph_callsites_idx = i;
    }
    mUINT32 Get_constraint_graph_callsites_count() const
    {
      return _constraint_graph_callsites_count;
    }
    void Set_constraint_graph_callsites_count(mUINT32 c)
    {
      _constraint_graph_callsites_count = c;
    }

    mUINT32 Get_constraint_graph_node_ids_idx() const
    {
      return _constraint_graph_node_ids_idx;
    }
    void Set_constraint_graph_node_ids_idx(mUINT32 i)
    {
      _constraint_graph_node_ids_idx = i;
    }
    mUINT32 Get_constraint_graph_node_ids_count() const
    {
      return _constraint_graph_node_ids_count;
    }
    void Set_constraint_graph_node_ids_count(mUINT32 c)
    {
      _constraint_graph_node_ids_count = c;
    }

    mUINT32 Get_constraint_graph_formal_parm_idx() const
    {
      return _constraint_graph_formal_parm_idx;
    }
    void Set_constraint_graph_formal_parm_idx(mUINT32 i)
    {
      _constraint_graph_formal_parm_idx = i;
    }
    mUINT32 Get_constraint_graph_formal_parm_count() const
    {
      return _constraint_graph_formal_parm_count;
    }
    void Set_constraint_graph_formal_parm_count(mUINT32 c)
    {
      _constraint_graph_formal_parm_count = c;
    }

    mUINT32 Get_constraint_graph_formal_ret_idx() const
    {
      return _constraint_graph_formal_ret_idx;
    }
    void Set_constraint_graph_formal_ret_idx(mUINT32 i)
    {
      _constraint_graph_formal_ret_idx = i;
    }
    mUINT32 Get_constraint_graph_formal_ret_count() const
    {
      return _constraint_graph_formal_ret_count;
    }
    void Set_constraint_graph_formal_ret_count(mUINT32 c)
    {
      _constraint_graph_formal_ret_count = c;
    }
   
    /* operations */

    void Init (void) {
	BZERO (this, sizeof(SUMMARY_PROCEDURE));
	_bb_count = MIN (PU_WN_BB_Cnt, UINT16_MAX);
	_stmt_count = MIN (PU_WN_Stmt_Cnt, UINT16_MAX);

#if (defined(_STANDALONE_INLINER) || defined(_LIGHTWEIGHT_INLINER))
	PU_WN_BB_Cnt = 0;
	PU_WN_Stmt_Cnt = 0;
#endif // _STANDALONE_INLINER
    }
    
    void Incr_altentry_count ()		{ _alt_entry_count++;}
    void Incr_call_count ()		{ _call_count++; }
    void Incr_callsite_count ()		{ _callsite_count++; }

    void Decr_call_count ()		{ _call_count--; }
    void Decr_callsite_count ()		{ _callsite_count--; }

    // Tracing:
    void Print ( FILE *fp, INT32 i ) const;
    void Trace ( INT32 i ) const;
    void Print_array ( FILE *fp, INT32 size ) const;
    void Trace_array ( INT32 size ) const;
    void WB_Print(FILE* fp, INT procedure_index, const char* name, 
      INT fancy_level);

}; // class SUMMARY_PROCEDURE


// feedback info (per procedure).  
class SUMMARY_FEEDBACK
{
private:
    
    FB_FREQ _cycle_count;
    FB_FREQ _frequency;			// # of times this PU is invoked
    mUINT16 _effective_bb_count;	// # of bb with non-zero freq.
    mUINT16 _effective_stmt_count;	// # of stmt with non-zero freq.
	UINT16	_wn_count;	//INLINING_TUNING
	FB_FREQ	_cycle_count_2;	//INLINING_TUNING
#ifdef KEY
    UINT64  _func_runtime_addr;         // Function runtime addr used
                                        // by -IPA:icall_opt.
#endif
    
public:

    /* access functions */

	//INLINING_TUNING^
    void Set_wn_count (UINT16 count)	{ _wn_count = count;}
    UINT16 Get_wn_count () const	{ return _wn_count; }
    void Inc_wn_count (UINT16 count){ _wn_count += count;}

    void Set_cycle_count_2 (FB_FREQ count)	{ _cycle_count_2 = count; };
    FB_FREQ Get_cycle_count_2 () const		{ return _cycle_count_2; };
    void Inc_cycle_count_2 (FB_FREQ count)	{ _cycle_count_2 += count; }
	//INLINING_TUNING$

    void Set_cycle_count (FB_FREQ count)	{ _cycle_count = count; };
    FB_FREQ Get_cycle_count () const		{ return _cycle_count; };
    void Inc_cycle_count (FB_FREQ count)	{ _cycle_count += count; }

    void Set_frequency_count (FB_FREQ freq )	{ _frequency = freq; };
    FB_FREQ Get_frequency_count () const	{ return _frequency; };

    void Set_effective_bb_count (UINT16 count)	{ _effective_bb_count = count;}
    UINT16 Get_effective_bb_count () const	{ return _effective_bb_count; }
    void Inc_effective_bb_count (UINT16 count){ _effective_bb_count += count;}

    void Set_effective_stmt_count (UINT16 count) {
	_effective_stmt_count = count;
    }
    UINT16 Get_effective_stmt_count () const {
	return _effective_stmt_count;
    }
    void Inc_effective_stmt_count (UINT16 count) {
	_effective_stmt_count += count;
    }

#ifdef KEY
    void Set_func_runtime_addr (UINT64 addr) {
	_func_runtime_addr = addr;
    }
    UINT64 Get_func_runtime_addr () const {
	return _func_runtime_addr;
    }
#endif

    /* operations */

    void Init () { BZERO (this, sizeof(SUMMARY_FEEDBACK)); }

    // Tracing
    void Print (FILE *f) const;
    void Print_array (FILE *f, INT32 size) const;
    void WB_Print(FILE* fp, INT feedback_index);

}; // SUMMARY_FEEDBACK


//-----------------------------------------------------------------
// all summary information for a callsite node must be stored here
//-----------------------------------------------------------------
#define IPL_FUNC_PTR		0x01
#define IPL_INTRINSIC_FUNC	0x02
#define IPL_HAS_CALLSITE_FREQ	0x04

    // for pragma support of inline/noinline
#define IPL_CALL_MUST_INLINE	0x08
#define IPL_CALL_NO_INLINE	0x10

#ifdef KEY
#define IPL_ICALL_TARGET        0x20  
#endif

#define IPL_IN_CASE_CLAUSE      0x40
#define IPL_IS_VIRTUAL_CALL     0x80

/*
File 3: osprey/ipa/local/ipl_summary.h
    I added a new state flag IPL_VIRTUAL_FUNCTION_TARGET in SUMMARY_CALLSITE 
    and functions to set, reset and test this flag. This flag is set on the dummy callsite
    inserted by Process_virtual_function. This flag is reset after the 
    devirtualization pass has replaced a dummy call site with a real call, to the 
    inferred direct call.
    function that sets this flag: Set_virtual_function_target
    function that resets this flag: Reset_virtual_function_target
    function that checks this flag: Is_virtual_function_target

*/

#define IPL_VIRTUAL_FUNCTION_TARGET     0x100
#define IPL_DUMMY_CALLSITE              0x200
    
class SUMMARY_CALLSITE
{
private:

    union {
	WN* w;				// for the standalone inliner, we need
					// the wn
	mINT32 _map_id;			// map id corresponding to the whirl
					// node
	mUINT64 _targ_runtime_addr;	// used by icall-optimization
					// IPL_ICALL_TARGET must be set.
    } u1;

    union {
	mINT32 _symbol_index;		// index into the symbols array
	mINT32 _value_index;		// index into the value array
					// used for function pointer
    } u2;

    mINT32 _actual_index;		// index into the array of const nodes
    mUINT16 _par_count;			// number of parameters in the call

    mUINT16 _state;			// state information about the callsite
    FB_FREQ _frequency;			// callsite frequency count;
    mUINT16 _callsite_id;		// postorder number
    mUINT16 _loopnest;			// 0 = not in loop, n = nth-level nested loop
#ifdef KEY
    float _probability;			// if inside a branch, probability of it being taken
#endif
    TYPE_ID _return_type:8;		// Return type of this CALL

    TY_IDX _virtual_class;              // the class of the virtual function 
    mUINT32 _vtable_offset;             // virtual function position, the offset of the vtable 
    mUINT64 _vptr_offset;               // vtable field offset of this call 
    mINT32  _matching_map_id;           // for dummy call site, the associated WN node's map id
                                        // used to match dummy callsite and the WN node

    mUINT32 _constraint_graph_callsite_id; // For the Nystrom alias analyzer

public:

    /* access functions */

    void Set_wn (WN* w)			{ u1.w = w;}
    WN* Get_wn () const			{ return u1.w; }

    void Set_map_id (mINT32 map_id)	{ u1._map_id = map_id;}
    INT32 Get_map_id () const		{ return u1._map_id;}

#ifdef KEY
    void Set_targ_runtime_addr (mUINT64 addr)	{ u1._targ_runtime_addr = addr;}
    mUINT64 Get_targ_runtime_addr () const	{ return u1._targ_runtime_addr;}
#endif

    void Set_symbol_index (mINT32 s) {
	Is_True (!Is_func_ptr (),
		 ("Symbol index not allowed for indirect callsite"));
	u2._symbol_index = s;
    }
    INT32 Get_symbol_index (void) const {
	Is_True (!Is_func_ptr (),
		 ("Symbol index not allowed for indirect callsite"));
	return u2._symbol_index;
    }

    void Set_value_index (INT32 s) {
	Is_True (Is_func_ptr (),
		 ("Value index for indirect callsite only"));
	u2._value_index = s;
    }
    INT32 Get_value_index (void) const {
	Is_True (Is_func_ptr (),
		 ("Value index for indirect callsite only"));
	return u2._value_index;
    }

    void Set_matching_map_id(mINT32 map_id)  { _matching_map_id = map_id; }
    mINT32 Get_matching_map_id(void) const  { return _matching_map_id; }

    mUINT16 Get_state() const { return _state;}

    void Set_actual_index (mINT32 a)	{ _actual_index = a; }
    INT Get_actual_index () const	{ return _actual_index; }

    void Set_param_count (mUINT16 par_count) { _par_count = par_count; }
    mUINT16 Get_param_count () const	{ return _par_count;};

    void Set_func_ptr ()		{ _state |= IPL_FUNC_PTR; }
    BOOL Is_func_ptr () const		{ return (_state & IPL_FUNC_PTR); }
    void Reset_func_ptr ()              { _state &= ~IPL_FUNC_PTR; }

#ifdef KEY
    void Set_icall_target ()		{ _state |= IPL_ICALL_TARGET; }
    void Reset_icall_target ()		{ _state &= ~IPL_ICALL_TARGET; }
    BOOL Is_icall_target () const	{ return (_state & IPL_ICALL_TARGET); }

    void Set_probability (float p)	{ _probability = p; }
    float Get_probability () const	{ return _probability; }
#endif

    BOOL Is_in_case_clause (void) const	{ return (_state & IPL_IN_CASE_CLAUSE); }
    void Set_in_case_clause (void)	{ _state |= IPL_IN_CASE_CLAUSE; }

    void Set_intrinsic()		{ _state |= IPL_INTRINSIC_FUNC; }
    BOOL Is_intrinsic() const		{ return _state & IPL_INTRINSIC_FUNC; };

    void Set_callsite_freq ()		{ _state |= IPL_HAS_CALLSITE_FREQ;}
    BOOL Has_callsite_freq () const	{ return _state & IPL_HAS_CALLSITE_FREQ; }

    // these are set due to user pragmas
    void Set_must_inline ()		{ _state |= IPL_CALL_MUST_INLINE; }
    BOOL Is_must_inline () const	{ return _state & IPL_CALL_MUST_INLINE;}

    // these are set due to user pragmas
    void Set_no_inline ()		{ _state |= IPL_CALL_NO_INLINE;}
    BOOL Is_no_inline () const		{ return _state & IPL_CALL_NO_INLINE;}

    void Set_frequency_count (FB_FREQ freq) { _frequency = freq; }
    FB_FREQ Get_frequency_count () const { return _frequency; }

    void Set_callsite_id (mUINT16 c)	{ _callsite_id = c; }
    mUINT16 Get_callsite_id() const	{ return _callsite_id; }

    void Set_loopnest (UINT16 c)	{ _loopnest = c; }
    mUINT16 Get_loopnest () const	{ return _loopnest; }

    void Set_return_type (TYPE_ID return_type)	{ _return_type = return_type;}
    TYPE_ID Get_return_type () const		{ return _return_type;}

    void Set_virtual_function_target() { _state |= IPL_VIRTUAL_FUNCTION_TARGET; }
    void Reset_virtual_function_target() { _state &= ~IPL_VIRTUAL_FUNCTION_TARGET; }
    BOOL Is_virtual_function_target() { return (_state & IPL_VIRTUAL_FUNCTION_TARGET); }

    void Set_virtual_class(TY_IDX func) { _virtual_class = func; } 
    TY_IDX Get_virtual_class() { return _virtual_class; } 
  
    void Set_vtable_offset(UINT32 offset) { _vtable_offset = offset; } 
    UINT32 Get_vtable_offset() { return _vtable_offset; } 
  
    void Set_vptr_offset(UINT64 ofst) { _vptr_offset = ofst; } 
    UINT64 Get_vptr_offset() { return _vptr_offset; } 
  
    void Set_is_virtual_call() { _state |= IPL_IS_VIRTUAL_CALL; } 
    void Reset_is_virtual_call() { _state &= ~IPL_IS_VIRTUAL_CALL; }
    BOOL Is_virtual_call() { return _state & IPL_IS_VIRTUAL_CALL; } 

    // For the Nystrom alias analyzer
    UINT32 Get_constraint_graph_callsite_id() const
    { 
      return _constraint_graph_callsite_id; 
    }
    void Set_constraint_graph_callsite_id(UINT32 callSiteId)
    {
      _constraint_graph_callsite_id = callSiteId;
    }

    void Set_dummy_callsite ()		{ _state |= IPL_DUMMY_CALLSITE; }
    void Reset_dummy_callsite ()	{ _state &= ~IPL_DUMMY_CALLSITE; }
    BOOL Is_dummy_callsite () const	{ return (_state & IPL_DUMMY_CALLSITE); }

    /* operations */


    void Init () {
	BZERO (this, sizeof(SUMMARY_CALLSITE));
    }
	
    // Tracing:
    void Print ( FILE *fp ) const;
    void Trace ( void ) const;
    void Print_array ( FILE *fp, INT32 size ) const;
    void Trace_array ( INT32 size ) const;
    void WB_Print(FILE* fp, INT callsite_index, const char* name, const char* func_name);
    
}; // class SUMMARY_CALLSITE


//-----------------------------------------------------------------
// summary information for a formal parameter
//-----------------------------------------------------------------

class SUMMARY_FORMAL
{
private:
    mINT32 _symbol_index;

    // Attribute flags for a formal:
#define IPL_FORMAL_REF_PARM		0x01 // if this is a reference argument
#define IPL_FORMAL_VAR_DIM_ARRAY	0x02 // if this is a variable-dim array
#ifdef KEY
#define IPL_FORMAL_LOOP_INDEX		0x04 // ref_parm used as a loop index,
                            		     // REF_PARM must be also set.
#endif

    mINT16 _flags;    	

    mINT16 _position;	    // specify the position of the formal parameter 
    mINT32 _region_index;   // index into projected region array 
                            //   describing dimensions (needed 
                            //   for array section analysis)
    mINT32 _machine_type;   // mtype of element of formal being passed 
			    //   (NOT of the address being passed)
    TY_IDX _ty;	    	    // Type info assoc. with this formal 
public:

    /* access functions */

    void Set_symbol_index (INT32 s)	{ _symbol_index = s; }
    mINT32 Get_symbol_index () const	{ return _symbol_index; }

    void Set_region_index(mINT32 region_index) { _region_index = region_index;}
    mINT32 Get_region_index() const    { return _region_index;}

    void Set_is_ref_parm ()		{ _flags |= IPL_FORMAL_REF_PARM; }
    BOOL Is_ref_parm () const		{ return (_flags & IPL_FORMAL_REF_PARM); }

    void Set_is_var_dim_array ()	{ _flags |= IPL_FORMAL_VAR_DIM_ARRAY; }
    BOOL Is_var_dim_array () const	{ return (_flags & IPL_FORMAL_VAR_DIM_ARRAY); }

#ifdef KEY
    void Set_is_loop_index ()		{ _flags |= IPL_FORMAL_LOOP_INDEX; }
    BOOL Is_loop_index () const		{ return (_flags & IPL_FORMAL_LOOP_INDEX); }
#endif

    void Set_position (mINT32 position) { _position = position;}
    mINT32 Get_position () const	{ return _position;};

    void Set_machine_type(TYPE_ID machine_type) 
      { _machine_type = machine_type;}
    TYPE_ID Get_machine_type() const {return _machine_type;}

    void Set_ty(TY_IDX ty) 		{ _ty = ty;}
    TY_IDX Get_ty() const  		{ return _ty;};

    /* operations */

    void Process(WN*, INT, INT);

    // Tracing:
    void Print ( FILE *fp ) const;
    void Trace ( void ) const;
    void Print_array ( FILE *fp, INT32 size ) const;
    void Trace_array ( INT32 size ) const;
    void WB_Print(FILE* fp, INT formal_index, const char* name, const char* func_name);

}; // class SUMMARY_FORMAL


//-----------------------------------------------------------------
// summary information for a global common assignment
//-----------------------------------------------------------------
class SUMMARY_STID
{
private:
  mINT32 _symbol_index;         // index of the lhs SUMMARY_SYMBOL 
  mUINT32 _array_subscript;     // constant-valued array subscript
  mINT32 _value_index : 29;	// index of the rhs SUMMARY_VALUE
  mUINT32 _always_executed : 1; // under func_entry control dep.
  mUINT32 _array_assignment : 1;// lhs is an array element
  mUINT32 _const_subscript : 1; // lhas array subscript is constant

public:
  // access functions
  void Set_symbol_index (mINT32 idx) { _symbol_index = idx; }
  mINT32 Get_symbol_index () const   { return _symbol_index; }

  void Set_value_index (mINT32 val_idx) { _value_index = val_idx; }
  mINT32 Get_value_index () const       { return _value_index; }

  void Set_array_subscript (mUINT32 idx) { _array_subscript = idx; }
  mUINT32 Get_array_subscript () const   { return _array_subscript; }

  void Set_always_executed ()      { _always_executed = 1; }
  BOOL Is_always_executed () const { return _always_executed; }

  void Set_array_assignment ()      { _array_assignment = 1; }
  BOOL Is_array_assignment () const { return _array_assignment; }

  void Set_constant_subscript ()       { _const_subscript = 1; }
  BOOL Has_constant_subscript () const { return _const_subscript; }

  // operations

  void Init () { 
    _symbol_index = -1; 
    _array_subscript = 0;
    _value_index = -1; 
    _always_executed = 0;
    _array_assignment = 0;
    _const_subscript = 0;
  }
  
  // Tracing
  void Print ( FILE* fp, INT32 id ) const;
  void Trace ( INT32 id ) const;
  void Print_array ( FILE* fp, INT32 size ) const;
  void Trace_array ( INT32 size ) const;
  void WB_Print(FILE* fp, INT stid_index, const char* name, const char* func_name);

}; // class SUMMARY_STID



// for actual parameters we categorize the way they are passed
// so for actuals that do have a symbol table entry, they may
// be passed as *p, p, or &p
typedef enum ipa_pass_type
{
    PASS_UNKNOWN = 0,			// actual not a simiple variable,
					// so pass type not applicable
    PASS_LDID = 1,			// passed as p
    PASS_LOAD = 2,			// passed as *p
    PASS_MLOAD = 3,			// passed as *p for structures
    PASS_LDA = 4,			// passed as &p 
    PASS_ARRAY_SECTION = 5,             // passed array section
#ifdef KEY
    PASS_ARRAY = 6,                     // passed as OPR_ARRAY
#endif
} IPA_PASS_TYPE;


/* describe a actual parameter: its name, value (including jump functions)
   and how it is passed. */
class SUMMARY_ACTUAL
{
private:
    mINT32 _symbol_index;
    mINT32 _value_index : 24;		// index into SUMMARY_VALUE array
    IPA_PASS_TYPE _pass_type : 8;	
    mINT32 _index;  // index into REGION if type == PASS_ARRAY_SECTION
		    // index into SCALAR_INFO for other types
    TY_IDX _ty;    // Type info assoc. with this actual 
    BOOL   _is_value_parm;  // whether this actual is passed by value

public:

    /* access functions */

    void Set_symbol_index (INT32 s)	{ _symbol_index = s; }
    INT32 Get_symbol_index () const	{ return _symbol_index; }

    void Set_value_index (INT32 v)	{ _value_index = v; }
    INT32 Get_value_index () const	{ return _value_index; }

    void Set_pass_type (IPA_PASS_TYPE p) { _pass_type = p; }
    IPA_PASS_TYPE Get_pass_type () const { return _pass_type; }

    void Set_index(INT32 a) 		{ _index = a;}
    INT32 Get_index() const 		{ return _index;}

    void Set_ty(TY_IDX ty) 		{ _ty = ty;}
    TY_IDX Get_ty() const  		{ return _ty;}

    void Set_is_value_parm() 		{ _is_value_parm = TRUE;}
    BOOL Is_value_parm() const		{ return _is_value_parm;}

    /* operations */
    
    void Init () {
	_symbol_index = -1;
	_value_index = -1;
	_pass_type = PASS_UNKNOWN;
	_index = -1;
	_ty = 0;
	_is_value_parm = FALSE;
    }

    // return the name of a IPA_PASS_TYPE
    const char *Pass_type_name (void) const;

    void Print (FILE *f, INT32 position) const;

    void Print_array (FILE *f, INT32 size) const;

    void Trace (INT32 id) const		{ Print (TFile, id); }
    void WB_Print(FILE* fp, INT actual_index, const char* name, const char* func_name);

}; // SUMMARY_ACTUAL 


class SUMMARY_SYMBOL;			// forward declaration

enum IPA_CONST_TYPE {

    VALUE_UNKNOWN = 0,			// initial state
    VALUE_INT_CONST = 1,		// is an integer constant
    VALUE_TWO_CONSTS = 2,		// is one of two constant values
    VALUE_CONST = 3,			// is an st of CLASS_CONST
    VALUE_FORMAL = 4,			// is a formal parameter
    VALUE_GLOBAL = 5,			// is a global or static variable
    VALUE_SYMBOL = 6,			// symbol of unknown type
    VALUE_EXPR = 7,			// is a simple expression
    VALUE_PHI = 8,			// is the value return by a phi func.
    VALUE_CHI = 9,			// is const. if the chi function
					// does not modify the value
    VALUE_CALLSITE = 10, 		// used in expressions for 
					//   execution cost of calls 
    VALUE_NOT_CONST = 11,		// found to be *NOT* a constant
};

/* describe a (possibly) constant value */
/* The first two entries in the SUMMARY_VALUE array are always the constant
   0 and 1 respectivley with mtype == I4.  
 */
class SUMMARY_VALUE
{
private:

    union {
	mINT64 _int_const;		// integer constant

	struct {			// two integer constants
	    mINT32 val1;		// variable has one of these two values
	    mINT32 val2;		// so (x != val1) implies x == val2
	} _two_values;

	struct {
	    ST_IDX _const_st_idx;	// CONST symbol (floating pt, string)
	    TCON_IDX _tcon_idx;		// TCON IDX (used only in IPA)
					// In IPA, it is possible that
					// _const_st_idx is 0
	} _const_st;
      
	mINT32 _formal_index;		/* idx to SUMMARY_FORMAL array.
					   For pass-through jump functions
					   */  

      
	struct {
	    mINT32 _global_index;	/* idx to SUMMARY_SYMBOL array for
					   a static or global variable.
					   Use only if IPA finds out that
					   it is a constant. */  
 
	    ST_IDX _global_st_idx;	// the ST_IDX of a global symbol
					// during IPA's constant
					// propagation phase.
	} _global;

	mINT32 _symbol_index;		/* idx to SUMMARY_SYMBOL array for
					   a variable passed as actual
					   parameter and has undetermined
					   type */ 

	mINT32 _expr_index;		// index to SUMMARY_EXPR

	mINT32 _phi_index;		// index to SUMMARY_PHI

	mINT32 _chi_index;		// index to SUMMARY_CHI

	mINT32 _callsite_index;		// index to SUMMARY_CALLSITE

    } _const_value;

    /* 64-bit aligned at this point */

    mTYPE_ID _mtype : 8;		// result type
    mTYPE_ID _target_mtype : 8;		// used only when _addr_of is true: 
					// specify the mtype of what this
					// value points to
    
    // the following determine which of the above union is used.
    IPA_CONST_TYPE _const_type : 8;
    
    // distinguish between pre- and post-merge const_st_idx
    mBOOL _merged_const_st_idx : 1;

    // if this value is successfully propagated to a formal, mark if the
    // formal can be removed;
    mBOOL _remove_param : 1;

    // used when _const_type equals to VALUE_FORMAL, VALUE_GLOBAL, or
    // VALUE_SYMBOL, and when set, indicate that the address instead of the
    // value of the symbol is passed. This can also be set for integer
    // constant, in which case it means the address of the memory location
    // holding the integer value (typical in Fortran).
    mBOOL _addr_of : 1;

    // valid only if _addr_of is TRUE. Indicates that this value can be
    // converted to the address of a global variable/constant. Used
    // primarily in constant propagation in fortran's reference parameter,
    // where we need to decide if we can store a constant value to it.
    mBOOL _convertible_to_global : 1;
    
    // Used for execution count estimates. 
    mBOOL _is_trip_count : 1; 

    // Set when globals in execution cost formulas are represented as 
    // ST_IDXs rather than as symbol indices.
    mBOOL _is_global_st_idx : 1; 

    /* private member functions */

    // return the name of a IPA_CONST_TYPE
    const char *Const_type_name (void) const;

public:

    /* access functions */

    void Set_mtype (TYPE_ID m)	{ _mtype = m; }
    TYPE_ID Get_mtype () const	{ return _mtype; }

    void Set_target_mtype (TYPE_ID m) { _target_mtype = m; }
    TYPE_ID Target_mtype () const { return _target_mtype; }

    IPA_CONST_TYPE Get_const_type () const { return _const_type; }

    void Set_unknown ()		{ _const_type = VALUE_UNKNOWN; }
    BOOL Is_unknown () const	{ return _const_type == VALUE_UNKNOWN; }
    
    void Set_int_const ()	{ _const_type = VALUE_INT_CONST; }
    BOOL Is_int_const () const	{ return _const_type == VALUE_INT_CONST; }
    
    void Set_two_consts ()	{ _const_type = VALUE_TWO_CONSTS; }
    BOOL Is_two_consts () const	{ return _const_type == VALUE_TWO_CONSTS; }

    void Set_const_st ()	{ _const_type = VALUE_CONST; }
    BOOL Is_const_st () const	{ return _const_type == VALUE_CONST; }

    void Set_formal ()		{ _const_type = VALUE_FORMAL; }
    BOOL Is_formal () const	{ return _const_type == VALUE_FORMAL; }

    void Set_global ()		{ _const_type = VALUE_GLOBAL; }
    BOOL Is_global () const	{ return _const_type == VALUE_GLOBAL; }
    
    void Set_symbol ()		{ _const_type = VALUE_SYMBOL; }
    BOOL Is_symbol () const	{ return _const_type == VALUE_SYMBOL; }
    
    void Set_expr ()		{ _const_type = VALUE_EXPR; }
    BOOL Is_expr () const	{ return _const_type == VALUE_EXPR; }
    
    void Set_phi ()		{ _const_type = VALUE_PHI; }
    BOOL Is_phi () const	{ return _const_type == VALUE_PHI; }
    
    void Set_chi ()		{ _const_type = VALUE_CHI; }
    BOOL Is_chi () const	{ return _const_type == VALUE_CHI; }

    void Set_not_const ()	{ _const_type = VALUE_NOT_CONST; }
    BOOL Is_not_const () const	{ return _const_type == VALUE_NOT_CONST; }

    void Set_callsite ()	{ _const_type = VALUE_CALLSITE; }
    BOOL Is_callsite () const	{ return _const_type == VALUE_CALLSITE; }

    // Still a constant after propagation
    BOOL Is_constant () const	{
	switch (_const_type) {
	case VALUE_INT_CONST:
	case VALUE_CONST:
	    return TRUE;
	case VALUE_FORMAL:
	case VALUE_GLOBAL:
	case VALUE_SYMBOL:
	    if (_addr_of)
		return TRUE;
	default:
	    return FALSE;
	}
    }

    void Set_int_const_value (INT64 v) { _const_value._int_const = v; }
    INT64 Get_int_const_value () const { return _const_value._int_const; }

    void Set_two_values (INT32 val1, INT32 val2) {
	_const_value._two_values.val1 = val1;
	_const_value._two_values.val2 = val2;
    }
    INT32 Get_first_of_two_values () const {
	return _const_value._two_values.val1;
    }
    INT32 Get_second_of_two_values () const {
	return _const_value._two_values.val2;
    }

    void Set_const_st_idx (ST_IDX st_idx, TCON_IDX tcon_idx = 0) {
	_const_value._const_st._const_st_idx = st_idx;
	_const_value._const_st._tcon_idx = tcon_idx;
    }
    ST_IDX Get_const_st_idx () const {
	return _const_value._const_st._const_st_idx;
    }

    void Set_tcon_idx (TCON_IDX tcon_idx) {
	_const_value._const_st._const_st_idx = 0;
	_const_value._const_st._tcon_idx = tcon_idx;
    }
    TCON_IDX Get_tcon_idx () const {
	return _const_value._const_st._tcon_idx;
    }

    void Set_formal_index (INT32 _f) { _const_value._formal_index = _f; }
    INT32 Get_formal_index () const { return _const_value._formal_index; }

    void Set_global_index (INT32 _g) {
	_const_value._global._global_index = _g;
    }
    INT32 Get_global_index () const {
	return _const_value._global._global_index;
    }

    void Set_global_st_idx (ST_IDX _g) {
	_const_value._global._global_st_idx = _g;
    }
    ST_IDX Get_global_st_idx () const {
	return _const_value._global._global_st_idx;
    }

    void Set_symbol_index (INT32 _g) { _const_value._symbol_index = _g; }
    INT32 Get_symbol_index () const { return _const_value._symbol_index; }

    void Set_expr_index (INT32 _e)	{ _const_value._expr_index = _e; }
    INT32 Get_expr_index () const	{ return _const_value._expr_index; }

    void Set_phi_index (INT32 _p)	{ _const_value._phi_index = _p; }
    INT32 Get_phi_index () const	{ return _const_value._phi_index; }
    
    void Set_chi_index (INT32 k)	{ _const_value._chi_index = k; }
    INT32 Get_chi_index () const	{ return _const_value._chi_index; }

    void Set_callsite_index (INT32 k)	{ _const_value._callsite_index = k; }
    INT32 Get_callsite_index () const	{ return _const_value._callsite_index; }

    void Set_merged_const_st_idx ()      { _merged_const_st_idx = TRUE; }
    BOOL Is_merged_const_st_idx () const { return _merged_const_st_idx; }

    void Remove_param ()		{ _remove_param = TRUE; }
    void Reset_remove_param ()		{ _remove_param = FALSE; }
    BOOL Is_remove_param () const	{ return _remove_param; }
    
    void Set_is_addr_of ()		{ _addr_of = TRUE; }
    void Clear_is_addr_of ()		{ _addr_of = FALSE; }
    BOOL Is_addr_of () const		{ return _addr_of; }

    void Set_convertible_to_global ()	{ _convertible_to_global = TRUE; }
    void Clear_convertible_to_global ()	{ _convertible_to_global = FALSE; }
    BOOL Is_convertible_to_global () const { return _convertible_to_global; }

    BOOL Is_trip_count() const		{ return _is_trip_count; } 
    void Clear_is_trip_count()		{ _is_trip_count = FALSE; } 
    void Set_is_trip_count()		{ _is_trip_count = TRUE; } 

    BOOL Is_global_st_idx() const	{ return _is_global_st_idx; } 
    void Clear_is_global_st_idx()	{ _is_global_st_idx = FALSE; } 
    void Set_is_global_st_idx()		{ _is_global_st_idx = TRUE; } 

    /* operations */
    
    void Init ()		{ BZERO (this, sizeof(SUMMARY_VALUE)); }

    // print the constant value in ascii form
    void Print_const_value (FILE *f, const SUMMARY_SYMBOL* symbol = NULL) const;

    void Print (FILE *, INT32) const;

    void Print_array (FILE *, INT32) const;

    void Trace (INT32 id) const { Print (TFile, id); }
    void WB_Print(FILE* fp, INT value_index);
    BOOL Equal(SUMMARY_VALUE* sv);

}; // SUMMARY_VALUE


/* node that denotes a phi function -- use in flow sensitive const prop. */
class SUMMARY_PHI
{
private:
    enum PHI_VALUE_TYPE {
	PHI_UNKNOWN = 0,
	PHI_VALUE = 1,			// points to a SUMMARY_VALUE 
	PHI_EXPR = 2,			// points to an expression
	PHI_PHI  = 3,			// points to another phi function
	PHI_CHI = 4,			// points to an chi node
    };

    struct {
	mINT32 _cd_index : 24;		// control dependence
	mBOOL _branch : 8;		// which branch am I on?
	mINT32 _node_index : 24;	// idx to either SUMMARY_EXPR
					// or another SUMMARY_PHI
	PHI_VALUE_TYPE _type : 8;	// tell which array _node_index points

    } kids[2];

public:

    /* access functions */

    void Set_value (INT kid)	{ kids[kid]._type = PHI_VALUE; }
    BOOL Is_value (INT kid) const { return kids[kid]._type == PHI_VALUE; }

    void Set_expr (INT kid)	{ kids[kid]._type = PHI_EXPR; }
    BOOL Is_expr (INT kid) const { return kids[kid]._type == PHI_EXPR; }

    void Set_phi (INT kid)	{ kids[kid]._type = PHI_PHI; }
    BOOL Is_phi (INT kid) const	{ return kids[kid]._type == PHI_PHI; }

    void Set_chi (INT kid)	{ kids[kid]._type = PHI_CHI; }
    BOOL Is_chi (INT kid) const	{ return kids[kid]._type == PHI_CHI; }

    void Set_ctrl_dep_index (INT kid, INT idx) { kids[kid]._cd_index = idx; }
    INT Get_ctrl_dep_index (INT kid) const { return kids[kid]._cd_index; }

    void Set_branch (INT kid, BOOL br)	{ kids[kid]._branch = br; }
    BOOL Get_branch (INT kid) const	{ return kids[kid]._branch; }

    void Set_node_index (INT kid, INT idx){ kids[kid]._node_index = idx; }
    INT Get_node_index (INT kid) const	{ return kids[kid]._node_index; }
    
    /* operations */

    void Init ()		{ BZERO (this, sizeof(SUMMARY_PHI)); }

    /* print functions */

    void Print (FILE *f) const;
    void Print_array (FILE *f, INT32 size) const;
    void WB_Print(FILE* fp, INT phi_index);

}; // SUMMARY_PHI


/* node that denotes a chi function - use when mod/ref info is available */
class SUMMARY_CHI
{
private:
    mINT32 _call_index;			// index to SUMMARY_CALLSITE
    mINT32 _symbol_index;		// symbol that might be modified

    // describe which array _node_index is pointing to
    enum CHI_TYPE {
	CHI_UNKNOWN = 0,
	CHI_VALUE = 1,
	CHI_PHI = 2,
	CHI_EXPR = 3,
	CHI_CHI = 4,
    };
	
    mINT32 _node_index : 24;		// index to one of SUMMARY_VALUE, etc.
					// this describe the value should
					// the call do not really modify
					// the symbol

    mINT32 _type : 8;			// specify type of _node_index
    
public:

    /* access functions */

    void Set_call_index (INT idx)	{ _call_index = idx; }
    INT32 Get_call_index () const	{ return _call_index; }

    void Set_symbol_index (INT idx)	{ _symbol_index = idx; }
    INT32 Get_symbol_index () const	{ return _symbol_index ; }

    void Set_chi_value ()		{ _type = CHI_VALUE; }
    BOOL Is_chi_value () const		{ return _type == CHI_VALUE; }

    void Set_chi_phi ()			{ _type = CHI_PHI; }
    BOOL Is_chi_phi () const		{ return _type == CHI_PHI; }

    void Set_chi_expr ()		{ _type = CHI_EXPR; }
    BOOL Is_chi_expr () const		{ return _type == CHI_EXPR; }

    void Set_chi_chi ()			{ _type = CHI_CHI; }
    BOOL Is_chi_chi () const		{ return _type == CHI_CHI; }

    void Set_node_index (INT idx)	{ _node_index = idx; }
    INT32 Get_node_index () const	{ return _node_index; }
    
    /* operations */

    void Init ()			{ _call_index = _symbol_index = -1; }
    
    void Print (FILE *f) const;

    void Print_array (FILE *f, INT32 size) const;
    void WB_Print(FILE* fp, INT chi_index, const char* name, const char* func_name);

}; // SUMMARY_CHI


/* Describe a simple expression.  For now, it is in the form of
   <var> <op> <int const> where <var> could be a variable of another
   expression. 
 */
class SUMMARY_EXPR
{
private:
    enum IDX_TYPE {
	EXPR_UNKNOWN = 0,
	EXPR_VALUE = 1,			// describe which array
	EXPR_PHI   = 2,			// _node_index is pointing to.
	EXPR_EXPR  = 3,
	EXPR_CHI   = 4,
    };

    union {

	/* first case, one operand is constant, the other is not */
	struct {
	    mINT64 _const_value;	// integer constant value

	    mINT32 _node_index : 24;	/* index to either a SUMMARY_VALUE,
					   SUMMARY_PHI, or SUMMARY_EXPR. */
	    IDX_TYPE _type : 6;

	    mINT32 _kid : 2;		// specify which kid the non-constant
					// operand is.
	} _s1;

	/* second case, both operands are not integer constant */
	struct {
	    mINT32 _node_index : 24;	/* index to either a SUMMARY_VALUE,
					   SUMMARY_PHI, or SUMMARY_EXPR. */
	    IDX_TYPE _type : 8;
	} _s2[2];
    } _u;

    OPCODE _opcode : 20;

    mINT32 _pad    : 2;

    mBOOL _is_trip_count : 1;	        // is a trip count (can be simplified) 
    mBOOL _has_const_operand : 1;	// specify which struct in _u is used

    mTYPE_ID _mtype: 8;			// result type of this expression.

public:

    /* access functions */

    void Set_has_const_operand ()	{ _has_const_operand = TRUE; }
    void Clear_has_const_operand ()	{ _has_const_operand = FALSE; }
    BOOL Has_const_operand () const	{ return _has_const_operand; }

    BOOL Is_trip_count() const		{ return _is_trip_count; } 
    void Clear_is_trip_count()		{ _is_trip_count = FALSE; } 
    void Set_is_trip_count()		{ _is_trip_count = TRUE; } 

    void Set_kid (INT kid) {
	if (_has_const_operand)
	    _u._s1._kid = kid;
    }
    INT Get_kid () const {
	if (_has_const_operand)
	    return _u._s1._kid;
	else
	    return -1;
    }

    void Set_expr_unknown () {
	_has_const_operand = TRUE;
	_u._s1._type = EXPR_UNKNOWN;
    }
    BOOL Is_expr_unknown () const {
	return (_has_const_operand == TRUE && _u._s1._type == EXPR_UNKNOWN);
    }

    void Set_expr_value (INT kid) {
	if (_has_const_operand) {
	    _u._s1._type = EXPR_VALUE;
	    _u._s1._kid = kid;
	} else
	    _u._s2[kid]._type = EXPR_VALUE;
    }
    BOOL Is_expr_value (INT kid) const {
	if (_has_const_operand)
	    return _u._s1._type == EXPR_VALUE;
	else
	    return _u._s2[kid]._type == EXPR_VALUE;
    }

    void Set_expr_phi (INT kid) {
	if (_has_const_operand) {
	    _u._s1._type = EXPR_PHI;
	    _u._s1._kid = kid;
	} else
	    _u._s2[kid]._type = EXPR_PHI;
    }
    BOOL Is_expr_phi (INT kid) const {
	if (_has_const_operand)
	    return _u._s1._type == EXPR_PHI;
	else
	    return _u._s2[kid]._type == EXPR_PHI;
    }

    void Set_expr_expr (INT kid) {
	if (_has_const_operand) {
	    _u._s1._type = EXPR_EXPR;
	    _u._s1._kid = kid;
	} else
	    _u._s2[kid]._type = EXPR_EXPR;
    }
    BOOL Is_expr_expr (INT kid) const {
	if (_has_const_operand)
	    return _u._s1._type == EXPR_EXPR;
	else
	    return _u._s2[kid]._type == EXPR_EXPR;
    }

    void Set_expr_chi (INT kid) {
	if (_has_const_operand) {
	    _u._s1._type = EXPR_CHI;
	    _u._s1._kid = kid;
	} else
	    _u._s2[kid]._type = EXPR_CHI;
    }
    BOOL Is_expr_chi (INT kid) const {
	if (_has_const_operand)
	    return _u._s1._type == EXPR_CHI;
	else
	    return _u._s2[kid]._type == EXPR_CHI;
    }

    void Set_const_value (INT64 _v) { _u._s1._const_value = _v; }
    INT64 Get_const_value () const { return _u._s1._const_value; }

    void Set_node_index (INT kid, INT32 _n) {
	if (_has_const_operand)
	    _u._s1._node_index = _n;
	else
	    _u._s2[kid]._node_index = _n;
    }
    INT32 Get_node_index (INT kid) const {
	if (_has_const_operand)
	    return _u._s1._node_index;
	else
	    return _u._s2[kid]._node_index;
    } 

    void Set_opcode (OPCODE _op) { _opcode = _op; }
    OPCODE Get_opcode () const	{ return _opcode; }

    void Set_mtype (TYPE_ID _t)	{ _mtype = _t; }
    TYPE_ID Get_mtype () const	{ return _mtype; }

    /* operations */

    void Init ()		{ BZERO (this, sizeof(SUMMARY_EXPR)); }

    void Print_node (FILE *f, INT kid = 0) const;

    void Print (FILE *f) const;

    void Print_array (FILE *f, INT32 size) const;

    void Node(FILE* fp, INT kid);
    void WB_Print(FILE* fp, INT expr_index);
    BOOL Is_Canonical_Constant(DYN_ARRAY<SUMMARY_VALUE>* sv);
    INT64 Canonical_Constant_Value(DYN_ARRAY<SUMMARY_VALUE>* sv);
    BOOL Equal(SUMMARY_EXPR* sx);
    BOOL Equal_Node(INT kid, SUMMARY_EXPR* sx);

}; // SUMMARY_EXPR


/* statements that might be deleted depending on the control flow */
class SUMMARY_STMT
{
private:

    union {
	mINT32 _node_index;		// index to either SUMMARY_EXPR,
					// SUMMARY_CALLSITE, or
					// SUMMARY_SYMBOL_OFFSET_NODE.
	WN *_wn;			// used for ARRAY_REF stmts
	mINT32 _map_id;			// used for ARRAY_REF in file.
    } u1;
    
    enum {
	STMT_EXPR = 1,			// simple expression
	STMT_VAR  = 2,			// global/static variable
	STMT_CALL = 3,			// function call
	STMT_CD = 4,			// control dependence node
	STMT_ARRAY_REF = 6,		// array reference
	STMT_STID = 7,                  // direct assignment
    } _stmt_type : 8;			// describe what _node_index points to

    /* following used for STMT_VAR only */
    mUINT8 _ref_count;
    mUINT8 _write_count;
    mUINT8 _addr_taken_count;
    
public:

    /* access functions */

    BOOL Is_expr () const	{ return _stmt_type == STMT_EXPR; }
    BOOL Is_var () const	{ return _stmt_type == STMT_VAR; }
    BOOL Is_call () const	{ return _stmt_type == STMT_CALL; }
    BOOL Is_cond () const	{ return _stmt_type == STMT_CD; }
    BOOL Is_array_ref () const	{ return _stmt_type == STMT_ARRAY_REF; }
    BOOL Is_stid() const        { return _stmt_type == STMT_STID;};

    void Set_expr_index (INT32 idx) {
	_stmt_type = STMT_EXPR;
	u1._node_index = idx;
    }

    INT32 Get_expr_index () const {
	return !(_stmt_type == STMT_EXPR) ?  -1 : u1._node_index;
      }

    void Set_var_index (INT32 idx) {
      _stmt_type = STMT_VAR;
      u1._node_index = idx;
    }

    INT32 Get_var_index () const {
      return !(_stmt_type == STMT_VAR) ? -1 : u1._node_index;
    }

    void Set_call_index (INT32 idx) {
      _stmt_type = STMT_CALL;
      u1._node_index = idx;
    }

    INT32 Get_call_index () const {
      return !(_stmt_type == STMT_CALL) ? -1 : u1._node_index;
    }

    void Set_cond_index (INT32 idx) {
      _stmt_type = STMT_CD;
      u1._node_index = idx;
    }

    INT32 Get_cond_index () const {
      return !(_stmt_type == STMT_CD) ? -1 : u1._node_index;
    }

    void Set_array_ref_wn (WN *w) {
	_stmt_type = STMT_ARRAY_REF;
	u1._wn = w;
    }
    WN *Get_array_ref_wn () const {
	return !(_stmt_type == STMT_ARRAY_REF) ? 0 : u1._wn;
    }

    void Set_array_ref_map_id (mINT32 map_id) {
	_stmt_type = STMT_ARRAY_REF;
	u1._map_id = map_id;
    }
    INT32 Get_array_ref_map_id () const {
	return !(_stmt_type == STMT_ARRAY_REF) ? 0 : u1._map_id;
    } 

    void Set_stid_index (INT32 idx) {
	_stmt_type = STMT_STID;
	u1._node_index = idx;
      }

    INT32 Get_stid_index () const {
      return !(_stmt_type == STMT_STID) ? -1 : u1._node_index;
    }

    void Set_ref_count (UINT8 count)	{ _ref_count = count; }
    UINT8 Get_ref_count () const	{ return _ref_count; }

    void Set_write_count (UINT8 count)	{ _write_count = count; }
    UINT8 Get_write_count () const	{ return _write_count; }

    void Set_addr_taken_count (UINT8 count) { _addr_taken_count = count; }
    UINT8 Get_addr_taken_count () const	{ return _addr_taken_count; }

    /* operations */

    void Init ()	{ BZERO (this, sizeof(SUMMARY_STMT)); }

    /* print functions */

    void Print (FILE *f) const;
    void Print_array (FILE *f, INT32 size) const;
    void WB_Print(FILE* fp, INT stmt_index, const char* name, const char* func_name);
    
}; // SUMMARY_STMT


class SUMMARY_CONTROL_DEPENDENCE
{
private:
    
    union {
	WN *_w;				// map to the corrresponding WHIRL
	mINT32 _map_id;			// stmt.
    } u1;

    union {
	mINT32 _expr_index;		// index to SUMMARY_EXPR
	mINT32 _do_loop_index;		// index to SUMMARY_DO_LOOP
    } u2;

    mINT32 _true_stmt_index;		/* list of reachable statements
					   that IPA cares if expression is
					   true.  Index to SUMMARY_STMT */
    mINT32 _false_stmt_index : 24;	/* likewise for the false branch */

    enum {
	CD_IF = 0,			// this is an if-then-else or equiv.
	CD_DO_LOOP = 1,			// a fortran do loop
	CD_ENTRY = 2,			// dummy node for procedure entry
    } _cd_type : 8;

    mINT16 _true_count, _false_count;	// # of entries in SUMMARY_STMT array
	
public:

    /* access functions */

    void Set_wn (WN *w)			{ u1._w = w; }
    WN *Get_wn () const			{ return u1._w; }

    void Set_map_id (mINT32 map_id)     { u1._map_id = map_id; }
    INT32 Get_map_id () const           { return u1._map_id; }

    void Set_expr_index (INT32 idx)	{ u2._expr_index = idx; }
    INT32 Get_expr_index () const	{ return u2._expr_index; }

    void Set_do_loop_index (INT32 idx)	{ u2._do_loop_index = idx; }
    INT32 Get_do_loop_index () const	{ return u2._do_loop_index; }

    void Set_true_stmt_index (INT32 idx) { _true_stmt_index = idx; }
    INT32 Get_true_stmt_index () const	{ return _true_stmt_index;}

    void Set_false_stmt_index (INT32 idx) { _false_stmt_index = idx; }
    INT32 Get_false_stmt_index () const	{ return _false_stmt_index;}

    void Set_if_stmt ()			{ _cd_type = CD_IF; }
    BOOL Is_if_stmt () const		{ return _cd_type == CD_IF; }

    void Set_do_loop ()			{ _cd_type = CD_DO_LOOP; }
    BOOL Is_do_loop () const		{ return _cd_type == CD_DO_LOOP; }

    void Set_entry ()			{ _cd_type = CD_ENTRY; }
    BOOL Is_entry () const		{ return _cd_type == CD_ENTRY; }
    
    void Set_true_count (INT32 count)	{ _true_count = count; }
    INT32 Get_true_count () const	{ return _true_count; }

    void Set_false_count (INT32 count)	{ _false_count = count; }
    INT32 Get_false_count () const	{ return _false_count; }

    /* operations */

    void Init ()	{ BZERO (this, sizeof(SUMMARY_CONTROL_DEPENDENCE)); }

    /* print functions */
    
    void Print (FILE *f) const;
    void Print_array (FILE *f, INT32 size) const;
    void WB_Print(FILE* fp, INT control_index);
    
}; // SUMMARY_CONTROL_DEPENDENCE


// ====================================================================
// ====================================================================
//
// SUMMARY_SYMBOL
//
// ====================================================================
// ====================================================================
class SUMMARY_SYMBOL
{
    
private:

    union {
	struct {
	    mINT32 _findex;		// formal array idx
	    mINT32 _addr_count;		// addr passed count for locals/formals
	} s;
	mINT64 _const_value;		// only use when IPL_CMOD bit
					// is set
	struct {                        // only used for common block
	  mINT32 _common_index;         // elements
	  mINT32 _common_shape_index;   
	} c;
    } u1;

    union {  
      mINT32 _cur_addr_count;		// for formal/locals during inlining
      ST_IDX _st_idx_func; 		// for locals 
    } u2; 

    ST_IDX st_idx;			// id of the symbol in the symtab
    
    mUINT32 _type   :  8;               // type information about the symbol
    mUINT32 _state  :  8;               // state of the symbol
    mUINT32 _modref : 16;               // mod ref state of the symbol
    mTYPE_ID _btype :  8;               // backend type
    
public:
    /* access functions */
    void Set_findex (INT findex)	{ u1.s._findex = findex;};
    mINT32 Get_findex() const		{ return u1.s._findex;};

    void Set_common_index(INT index)    { u1.c._common_index = index;};
    mINT32 Get_common_index() const { return
					u1.c._common_index;};

    void Set_common_shape_index(INT index) { u1.c._common_shape_index
					       = index;};
    mINT32 Get_common_shape_index() const { return
					      u1.c._common_shape_index;};

    // addr passed count for locals and formals ONLY if their addr
    // taken and saved bit has not been set
    void Incr_addr_count()		{ u1.s._addr_count++;};
    mINT32 Get_addr_count() const	{ return u1.s._addr_count;};

    void Set_const_value (INT64 value)	{ u1._const_value = value; };
    void Clear_const_value ()		{ u1._const_value = 0; };
    INT64 Get_const_value () const	{ return u1._const_value; };

    void Set_st_idx (ST_IDX stid)	{ st_idx = stid; }
    ST_IDX St_idx () const		{ return st_idx; }

    // cur_addr_passed is used during the inlining process
    void Reset_cur_addr_passed_count()	{ u2._cur_addr_count = 0;};
    void Incr_cur_addr_passed_count()	{ u2._cur_addr_count++;};
    mINT32 Get_cur_addr_passed_count() const { return u2._cur_addr_count;};

    ST_IDX Get_st_idx_func() const      { return u2._st_idx_func;};
    void Set_st_idx_func(ST_IDX st_idx)	{ u2._st_idx_func = st_idx;};

    // Attribute flags for a symbol:
#define IPL_LOCAL_SYMBOL	0x01
#define IPL_STATIC_SYMBOL	0x02
#define IPL_FORMAL_SYMBOL       0x04
#define IPL_COMMON_ELT_SYMBOL   0x08
#define IPL_COMMON_BLOCK_SYMBOL	0x10 
#define IPL_FUNCTION_SYMBOL	0x20
#define IPL_OPTIONAL_SYMBOL	0x40
#define IPL_ARRAY_SYMBOL	0x80

    // Set/check scope attributes of the symbol:
    void Set_local()		{ _type |= IPL_LOCAL_SYMBOL; }
    BOOL Is_local() const	{ return _type & IPL_LOCAL_SYMBOL; }
    BOOL Is_global() const	{ return (_type & IPL_LOCAL_SYMBOL) == 0; }

    void Set_static()		{ _type |= IPL_STATIC_SYMBOL; }
    BOOL Is_static() const	{ return _type & IPL_STATIC_SYMBOL; }

    BOOL Is_formal() const	{ return _type & IPL_FORMAL_SYMBOL; }
    void Set_formal()		{ _type |= IPL_FORMAL_SYMBOL; }

    BOOL Is_common() const      { return _type & IPL_COMMON_ELT_SYMBOL; }
    void Set_common()           { _type |= IPL_COMMON_ELT_SYMBOL;};

    BOOL Is_common_block() const { return _type & IPL_COMMON_BLOCK_SYMBOL; }
    void Set_common_block()      { _type |= IPL_COMMON_BLOCK_SYMBOL;};

    BOOL Is_function() const	{ return _type & IPL_FUNCTION_SYMBOL; }
    void Set_function()		{ _type |= IPL_FUNCTION_SYMBOL; }

    BOOL Is_optional() const	{ return _type & IPL_OPTIONAL_SYMBOL; }
    void Set_optional()		{ _type |= IPL_OPTIONAL_SYMBOL; }

    void Set_array()		{ _type |= IPL_ARRAY_SYMBOL; }
    BOOL Is_array() const	{ return _type & IPL_ARRAY_SYMBOL; }
    BOOL Is_scalar() const	{ return (_type & IPL_ARRAY_SYMBOL) == 0; }

// address taken states
#define IPL_ADDR_SAVED		0x01 // ST_addr_taken_and_saved
#define IPL_ADDR_F90_TARGET	0x02 // ST_is_f90_target
#define IPL_ADDR_PASSED		0x04 // ST_addr_taken_and_passed 
#define IPL_ADDR_PASSED_INLINE  0x08 // the address passed for
				     // locals/formals during inlining has
				     // occured  
#define IPL_USED_IN_ARRAY_SECTION 0x10 // symbol used in array section summary
#define IPL_COMMON_IO_NO_PAD      0x20 // common in i/o statement: don't pad
#define IPL_COMMON_READ_NO_CPROP  0x40 // common read in: no common cprop

    // Set/check address-taken attributes:
    void Set_addr_saved()	{ _state |= IPL_ADDR_SAVED; }
    BOOL Is_addr_saved() const	{ return _state & IPL_ADDR_SAVED; }

    void Set_addr_f90_target()	{ _state |= IPL_ADDR_F90_TARGET; }
    BOOL Is_addr_f90_target() const { return _state & IPL_ADDR_F90_TARGET; }
    
    void Set_addr_passed()	{ _state |= IPL_ADDR_PASSED; }
    void Reset_addr_passed()	{ _state &= ~IPL_ADDR_PASSED; }
    BOOL Is_addr_passed() const	{ return _state & IPL_ADDR_PASSED; }
    
    void Set_addr_passed_inliner() { _state |= IPL_ADDR_PASSED_INLINE; }
    BOOL Is_addr_passed_inliner()  { return _state & IPL_ADDR_PASSED_INLINE; }

    void Set_used_in_array_section()   { _state |= IPL_USED_IN_ARRAY_SECTION; }
    BOOL Used_in_array_section() const { return _state & IPL_USED_IN_ARRAY_SECTION; }

    void Set_common_io_no_pad()   { _state |= IPL_COMMON_IO_NO_PAD; }
    BOOL Common_io_no_pad() const { return _state & IPL_COMMON_IO_NO_PAD; }

    void Set_common_read_no_cprop()   { _state |= IPL_COMMON_READ_NO_CPROP; }
    BOOL Common_read_no_cprop() const { return _state & IPL_COMMON_READ_NO_CPROP; }

    // mod/ref stats
#define IPL_IMOD	1	  // 0x01 indirect mod, i.e. *p
#define IPL_DMOD	2	  // 0x02 direct mod, i.e. p
#define IPL_IREF	4	  // 0x04 indirect ref, i.e. *p
#define IPL_DREF	8	  // 0x08 direct ref, i.e. p
#define IPL_AREF	16        // 0x10 address ref, i.e. &p
#define IPL_IKILL	32	  // 0x20 indirect kill, i.e. *p
#define IPL_DKILL	64	  // 0x40 direct kill, i.e. p
#define IPL_CMOD	128	  // 0x80 direct assignment of a constant
#define IPL_MODCOUNT	256	  // 0x100 mod count is non-zero
#define IPL_PARM        512       // 0x200 passed as a parameter to a
				  // routine
#define IPL_COPY_REF    1024      // 0x400 ref types that will be used by
                                  // copy propagation
#define IPL_CDREF_PREG_ONLY 2048  // 0x800 cref or dref only because the LDID
                                  // is under a STID return-preg
                                  // used by copy prop toreset addr passed bit

#define IPL_REF		61	  // 0x3d any ref, i.e. p, *p, or &p
				  //      including indirect mod, kill
#define IPL_MODREF_ANY	127	  // 0x7f any mod or ref

    // Set/test mod/ref flags:
    void Set_imod()		{ _modref = _modref | IPL_IMOD;};
    BOOL Is_imod() const	{ return _modref & IPL_IMOD;};
    void Set_dmod()		{ _modref = _modref | IPL_DMOD;};
    BOOL Is_dmod() const	{ return _modref & IPL_DMOD;};
    void Set_iref()		{ _modref = _modref | IPL_IREF;};
    BOOL Is_iref() const	{ return _modref & IPL_IREF;};
    void Set_aref()		{ _modref = _modref | IPL_AREF;};
    BOOL Is_aref() const	{ return _modref & IPL_AREF;};
    void Set_dref()		{ _modref =  _modref | IPL_DREF;};
    BOOL Is_dref() const	{ return _modref & IPL_DREF;};
    void Set_cref()		{ _modref =  _modref | IPL_COPY_REF;};
    BOOL Is_cref() const	{ return _modref & IPL_COPY_REF;};
    void Set_cdref_preg_only()  { _modref = _modref | IPL_CDREF_PREG_ONLY;}
    BOOL Is_cdref_preg_only() const   { return _modref & IPL_CDREF_PREG_ONLY;}

    // TODO:  Kill information is not currently set
    void Set_ikill()		{ _modref = _modref | IPL_IKILL;};
    BOOL Is_ikill() const	{ return _modref & IPL_IKILL;};
    void Set_dkill()		{ _modref = _modref | IPL_DKILL;};
    BOOL Is_dkill() const	{ return _modref & IPL_DKILL;};

    void Set_cmod()		{ _modref |= IPL_CMOD; };
    void Clear_cmod()		{ _modref &= ~IPL_CMOD; };
    BOOL Is_cmod() const	{ return _modref & IPL_CMOD; };

    void Set_modcount()		{ _modref |= IPL_MODCOUNT; };
    void Clear_modcount()	{ _modref &= ~IPL_MODCOUNT; };
    BOOL Is_modcount() const	{ return _modref & IPL_MODCOUNT; };

    BOOL Is_parm() const        { return _modref & IPL_PARM;};
    void Set_parm()             { _modref |= IPL_PARM;};
    
    // Any reference, including indirect mod or kill:
    BOOL Is_ref() const		{ return _modref & IPL_REF;};
    // Any mod/ref at all:
    BOOL Is_modref() const	{ return _modref & IPL_MODREF_ANY;};

    
    void Set_btype (mTYPE_ID b) {  _btype = b;};
    mTYPE_ID Get_btype() const	{ return _btype;};


    /* operations */
       
    void Init ()		{ BZERO (this, sizeof(SUMMARY_SYMBOL)); }

    
    // Tracing:
    const char * Get_Name ( void ) const;
    void Print ( FILE *fp , INT id = -1, char* symbol_name = NULL, 
      char* function_name = NULL) const;
    void WB_Print(FILE* fp, INT symbol_index, BOOL is_list, const char* name,
      const char* func_name, INT fancy_level);
    void Trace ( void ) const;
    void Print_array ( FILE *fp, INT32 size, 
      DYN_ARRAY<char*>* symbol_names = NULL, 
      DYN_ARRAY<char*>* function_names = NULL) const;
    void Trace_array ( INT32 size ) const;

}; // class SUMMARY_SYMBOL


//------------------------------------------------------------------
// an array of globals and information about their mod/ref state in
// each procedure. This may be extended to handle globals with 
// section information
//------------------------------------------------------------------
class SUMMARY_GLOBAL {

private:

    mINT32 _symbol_index;
    mINT32 _refcount;			// # of times this symbol is read
    mINT32 _modcount;			// # of times this symbol is written
    mUINT16 _modref;			// mod/ref state information

public:

    /* access functions */

    void Set_symbol_index ( mINT32 s )	{ _symbol_index = s; };
    INT32 Get_symbol_index () const	{ return _symbol_index; };

    void Inc_refcount ()		{ _refcount++; };
    mINT32 Get_refcount () const	{ return _refcount; };

    void Inc_modcount ()		{ _modcount++; };
    mINT32 Get_modcount () const	{ return _modcount; };
    
    void Set_imod ()			{ _modref |= IPL_IMOD;};
    BOOL Is_imod() const		{ return _modref & IPL_IMOD;};

    void Set_dmod()			{ _modref |= IPL_DMOD;};
    BOOL Is_dmod() const		{ return _modref & IPL_DMOD;};

    void Set_iref()			{ _modref |= IPL_IREF;};
    BOOL Is_iref() const		{ return _modref & IPL_IREF;};
    
    void Set_dref()			{_modref |= IPL_DREF;};
    BOOL Is_dref() const		{ return _modref & IPL_DREF;};  
    
    void Set_aref()			{ _modref |= IPL_AREF;};
    BOOL Is_aref() const		{ return _modref & IPL_AREF;};

    // TODO:  Kill information is not currently set
    void Set_ikill()			{ _modref |= IPL_IKILL;};
    BOOL Is_ikill() const		{ return _modref & IPL_IKILL;};

    void Set_dkill()			{ _modref |= IPL_DKILL;}
    BOOL Is_dkill() const		{ return _modref & IPL_DKILL;};

    // Any reference, including indirect mod or kill:
    BOOL Is_ref() const			{ return _modref & IPL_REF;};

    // Any mod/ref at all:
    BOOL Is_modref() const		{ return _modref & IPL_MODREF_ANY;};


    /* operations */

    void Init (void)		{ BZERO (this, sizeof(SUMMARY_GLOBAL));}

    // Tracing:
    void Print ( FILE *fp ) const;
    void Trace ( void ) const;
    void Print_array ( FILE *fp, INT32 size ) const;
    void Trace_array ( INT32 size ) const;
    void WB_Print(FILE* fp, INT global_index);

}; // class SUMMARY_GLOBALS

//-----------------------------------------------------------------
// this is the header for the common array shape descriptor
// note if the IPL_COMMON_BAD_EQUIV is true then we ignore all the
// other fields, i,e, we don't set the  shape information
// if IPL_COMMON_IS_INIT, then again we ignore all the other fields
// 1) an index into the symbols array
// 2) state information
// 3) index into the structure containing the shape of each dimension
//-----------------------------------------------------------------
class SUMMARY_COMMON
{
private:
  mINT32 _symbol_index;
  mINT32 _common_shape_index;		// index into the first element
  mUINT16 _common_shape_count;	// shape count is the number of
                                // elements in the common block
  mUINT16 _state;
#define IPL_COMMON_BAD_EQUIV   0x1 // equivalences to common blocks
#define IPL_COMMON_IS_INIT     0x2 // common has been initialized
#define IPL_COMMON_BAD_SPLIT_EQUIV 0x4 // equivalences to common for
				      // splitting
#define IPL_COMMON_SPLIT       0x8    // during IPA phase set to true if we
				      // are going to split the common.
#define IPL_COMMON_CONSISTENT 16      // are the commons consistent?
#define IPL_HAS_CONST_ARRAY 32        // has array elements that are
				      // assigned constant values
#define IPL_IS_BOTTOM 64              // unknown values may be assigned to it

// has unstructured flow and assignments to the common 
// within that unstructured flow  
#define IPL_HAS_UNSTRUCTURED_FLOW_WITH_ASSIGN 128 
#define IPL_HAS_IMOD    256           // has been indirectly modified via
                                      // callsites
#define IPL_HAS_EQUIVALENCES 512      // this common HAS equivalences

public:

  /* access functions */
  void Set_symbol_index ( INT32 s )	{ _symbol_index = s; };
  INT32 Get_symbol_index () const	{ return _symbol_index; };  

  void Set_common_shape_index (mINT32 i) { _common_shape_index = i;};
  mINT32 Get_common_shape_index () const { return _common_shape_index;};

  void Set_common_shape_count (mINT32 i) { _common_shape_count = i;};
  mINT32 Get_common_shape_count() const { return _common_shape_count;};
    
  void Set_has_bad_equiv()	{ _state |= IPL_COMMON_BAD_EQUIV;};
  mBOOL Has_bad_equiv() const { return _state & IPL_COMMON_BAD_EQUIV;};

  void Set_is_initialized()	{ _state |= IPL_COMMON_IS_INIT;};
  mBOOL Is_initialized() const {return _state & IPL_COMMON_IS_INIT;};

  void Set_has_bad_split_equiv() { _state |= IPL_COMMON_BAD_SPLIT_EQUIV;};
  mBOOL Has_bad_split_equiv() const { return _state & IPL_COMMON_BAD_SPLIT_EQUIV;};

  void Set_has_array_constants() { _state |= IPL_HAS_CONST_ARRAY;};
  mBOOL Has_array_constants () const { return _state &
					   IPL_HAS_CONST_ARRAY; };
 
  INT16  Is_bottom () const { return _state & IPL_IS_BOTTOM;};
  void Set_bottom() { _state |= IPL_IS_BOTTOM;};

  // used during main IPA
  void Set_need_split()	{ _state |= IPL_COMMON_SPLIT;};
  mBOOL Need_split() const	{ return _state & IPL_COMMON_SPLIT;};

  void Set_is_consistent()	{ _state |= IPL_COMMON_CONSISTENT;};
  INT16 Is_consistent() const	{ return _state & IPL_COMMON_CONSISTENT;};

  void Set_has_unstructured_cflow() 
  { _state |= IPL_HAS_UNSTRUCTURED_FLOW_WITH_ASSIGN;};

  INT16 Has_unstructured_cflow() const 
      { return _state & IPL_HAS_UNSTRUCTURED_FLOW_WITH_ASSIGN;};

  void Set_imod()	{ _state |= IPL_HAS_IMOD;};
  INT16 Is_imod() const	{ return _state & IPL_HAS_IMOD;};

  void Set_has_equivalences()	 { _state |= IPL_HAS_EQUIVALENCES;};
  INT16 Has_equivalences() const { return  _state & IPL_HAS_EQUIVALENCES;};

  /* operations */
  void Init ()		{ BZERO (this, sizeof(SUMMARY_COMMON)); }

  void Print_array(FILE *fp, INT32 size) const;
  void Trace_array(INT32 size) const ;
  void Print(FILE *f, INT32 id) const;
  void Trace(INT32) const;
  void WB_Print(FILE* fp, INT common_index);

}; // SUMMARY_COMMON

//-----------------------------------------------------------------
// shape of each dimension
//-----------------------------------------------------------------
class SUMMARY_COMMON_SHAPE
{
private:
  union {
    mINT64 _offset;  // used for scalars
    struct {
      mINT32 _upper; // used for array elements
      mINT32 _lower; // used for array elements
    } _bounds;
  } _u1; 

  union {
    mINT32 _stride;
    mINT32 _value_index;
    SUMMARY_VALUE* _value;
  } _u2;

  mINT32 _symbol_index;
  mINT32 _element_size;
  mINT16 _dim_count;

#define IPL_IS_KIND_ARRAY       0x01
#define IPL_IS_KIND_SCALAR      0x02
#define IPL_IS_SYMBOLIC_BOUNDS  0x04
#define IPL_HAS_CONST_VALUE     0x08
#define IPL_NOT_CONST_VALUE     0x10

  mINT16 _state;

public:
  /* access functions */
  void Set_dim_count(INT32 i )	{ _dim_count = i;};
  INT32 Get_dim_count() const	{ return _dim_count;};

  INT32 Get_symbol_index() const { return _symbol_index; };
  void Set_symbol_index(INT32 i) { _symbol_index = i;};

  void Set_upper (INT32 i)  { _u1._bounds._upper = i;};
  INT32 Get_upper() const   { return _u1._bounds._upper;};

  void Set_lower(INT32 i)	{ _u1._bounds._lower = i;};
  INT32 Get_lower() const	{ return _u1._bounds._lower;};

  void Set_stride(INT32 i)	{ _u2._stride = i;};
  INT32 Get_stride() const	{ return _u2._stride;};
 
  void Set_value_index(INT32 i)	{ _u2._value_index = i;};
  INT32 Get_value_index() const	{ return _u2._value_index;};

  void Set_value(SUMMARY_VALUE* val) { _u2._value = val;};
  SUMMARY_VALUE *Get_value() const   { return _u2._value;};

  void Set_all_sizes (INT32 upper, INT32 lower, INT32 stride) {
	_u1._bounds._upper = upper;
	_u1._bounds._lower = lower;
	_u2._stride = stride;
  }

  void Set_element_size (INT32 i)	{_element_size = i;};
  INT32 Get_element_size() const	{ return _element_size;};

  INT64 Get_offset()  const    { return _u1._offset;};
  void Set_offset(INT64  ofst) { _u1._offset = ofst; };

  void Set_is_kind_array()	 { _state |= IPL_IS_KIND_ARRAY;};
  BOOL Is_kind_array() const { return _state & IPL_IS_KIND_ARRAY;};
    
  void Set_is_kind_scalar()	  { _state |= IPL_IS_KIND_SCALAR;};
  BOOL Is_kind_scalar() const { return _state & IPL_IS_KIND_SCALAR;};

  void Set_symbolic()		      { _state |= IPL_IS_SYMBOLIC_BOUNDS;};
  BOOL Is_symbolic_bounds() const { return _state & IPL_IS_SYMBOLIC_BOUNDS;};

  void Set_has_const()	 { _state |= IPL_HAS_CONST_VALUE;};
  void Reset_has_const() { _state &= ~IPL_HAS_CONST_VALUE;};
  BOOL Is_const() const  { return _state & IPL_HAS_CONST_VALUE;};

  void Set_not_const()      { _state |= IPL_NOT_CONST_VALUE;};
  void Reset_not_const()    { _state &= ~IPL_NOT_CONST_VALUE; };
  BOOL Is_not_const() const { return _state & IPL_NOT_CONST_VALUE;};

  /* operations */
  void Init ()	{ 
    BZERO(this, sizeof(SUMMARY_COMMON_SHAPE)); 
    Set_symbol_index(-1); 
  }

  BOOL operator == (const SUMMARY_COMMON_SHAPE &c) const {
    return ((c.Get_upper() == _u1._bounds._upper) &&
            (c.Get_lower() == _u1._bounds._lower) &&
            (c.Get_stride() == _u2._stride) &&
            (c.Get_element_size() == _element_size) &&
            (c.Get_dim_count() == _dim_count));
  }

  void Print_array(FILE *fp, INT32 size) const;
  void Trace_array(INT32 size) const ;
  void Print(FILE *f, INT32 id) const;
  void Trace(INT32) const;
  void WB_Print(FILE* fp, INT common_shape_index);

}; // SUMMARY_COMMON_SHAPE
//field_reordering
//-----------------------------------------------------------------
// summary information for a field_reorder parameter
//-----------------------------------------------------------------
	struct STRUCT_ACCESS{//UGLY, to put it outside of SUMMARY! BUT...
	//should put into SUMMARY_STRUCT_ACCESS
	//BUT I cannot make qsort() running with Cmp_FLD_COUNT1() as a member func
	//qsort() cannot find it!!!!
		mUINT32 field_id;
		mUINT64 count;
	};
	/*assist function for Set_hot_fld()*/
	inline INT Cmp_FLD_COUNT1(const void *p1,const void*p2){
		STRUCT_ACCESS* t1,*t2;
		t1=(STRUCT_ACCESS*)p1;
		t2=(STRUCT_ACCESS*)p2;
		  if ( t1->count <t2->count )
		    return 1;
		  else if ( t1->count >t2->count  )
		    return -1;
		  else
		    return 0;
	};


class SUMMARY_STRUCT_ACCESS
{
	struct Cmp_FLD_COUNT{// no use now! we now use array instead of vector
		bool operator() (const STRUCT_ACCESS &t1,const STRUCT_ACCESS &t2) const{
			  if ( t1.count <t2.count )
			    return 1;
			  else if ( t1.count >t2.count  )
			    return -1;
			  else
			    return 0;
		};
	};
	#define max_hot_num 8
private:
	MEM_POOL *_mem;
	mUINT32 _ty; //just struct_type
	mUINT32 _flatten_flds;
	union{
	   	STRUCT_ACCESS hot_fld[max_hot_num];// just fld access of hot fields;
	   	STRUCT_ACCESS* flds; // access count of all fld; field_id is just for sorting
   	}_u;
public:

    /* access functions */

    void Set_ty(mUINT32 ty) {	_ty = ty; }		//set ty, and get flatten_flds, malloc flds[]
    mUINT32 Get_ty() const  		{ return _ty;};
    mUINT64 Get_hot_fld(mUINT32 hot_num)const	{return _u.hot_fld[hot_num].count;}
    mUINT32 Get_hot_fld_id(mUINT32 hot_num)const	{return _u.hot_fld[hot_num].field_id;}

    void Set_flatten_flds(mUINT32 flatten_flds) {_flatten_flds=flatten_flds;}
    mUINT32 Get_flatten_flds() const { return _flatten_flds;};
    void  Inc_fld_count(mUINT32 fld_id, mUINT64 add_count)
#ifdef KEY // bug 5372
    	{_u.flds[fld_id-1].count+=add_count;};
#else
    	{_u.flds[fld_id].count+=add_count;};
#endif
    char* Get_ty_name (void) const { TY& ty=Ty_tab[_ty];
		 return Index_To_Str(ty.name_idx); }


    void Set_hot_fld (void) // get hottness sorting, put  to hot_fld[], dealloc flds[]
    {
		mUINT32 flds;
		UINT i,num=Get_flatten_flds();
		FmtAssert(num>=1,
		("in Set_hot_fld() type fld_num>=1!\n"));

		STRUCT_ACCESS *old,*hot;
		old=_u.flds; //record flds[]
		hot=_u.hot_fld;
		for(i=0;i<num;i++)
			old[i].field_id=i+1;
		qsort( old,num, sizeof(STRUCT_ACCESS), Cmp_FLD_COUNT1);
		//std::sort(old,old+num-1, Cmp_FLD_COUNT());
		if(num>max_hot_num)
			num=max_hot_num;
		for(i=0;i<num;i++)
			hot[i]=old[i];
		return;
	}

    void Set_hot_fld_array(INT32 size)
    	{ INT i;
		    for ( i=0; i<size; ++i ) 
			this[i].Set_hot_fld ();
	    }; //for all summary_entry, set hot_fld_access info

    /* operations */

    void Init (mUINT32 ty_index,mUINT32 flatten_flds,MEM_POOL * mem){ 
	    BZERO(this, sizeof(SUMMARY_STRUCT_ACCESS)); 
	    _mem=mem;
	    Set_ty(ty_index); 
	    Set_flatten_flds(flatten_flds);
	    _u.flds= (STRUCT_ACCESS*)MEM_POOL_Alloc_P(_mem,
			sizeof(STRUCT_ACCESS)*flatten_flds,
			TRUE,NULL);
#ifndef KEY
	    fprintf(stderr,"new summary: type%d, \n",ty_index);
#endif // !KEY
  }

	/*assist subroutine*/


    // Tracing:
    void Print ( FILE *fp ,INT32 id) const;
    void Trace ( INT32 id ) const;
    void Print_array ( FILE *fp, INT32 size ) const;
    void Trace_array ( INT32 size ) const;
    void WB_Print(FILE* fp, INT fld_access_index);

}; // class SUMMARY_STRUCT_ACCESS

#ifdef KEY
class SUMMARY_TY_INFO
{
  private:
    TY_IDX _ty;
    mUINT32 _flags;

#define IPL_TY_NO_SPLIT 0x00000001

  public:

    void Set_ty (TY_IDX type)        { _ty = type; }
    TY_IDX Get_ty (void) const       { return _ty; }

    void Set_ty_no_split (void)      { _flags |= IPL_TY_NO_SPLIT; }
    BOOL Is_ty_no_split (void) const { return _flags & IPL_TY_NO_SPLIT; }

    void Init (void)                 { BZERO (this, sizeof(SUMMARY_TY_INFO)); }
    void Print_array(FILE *fp, INT32 size) const;
    void Trace_array(INT32 size) const ;
    void Print(FILE *f) const;
    void Trace(void) const;
};
#endif

extern SUMMARY_SYMBOL *Ipl_Summary_Symbol;
extern BOOL IPA_Trace_Mod_Ref;          /* Trace log for Mod_Ref */
extern char Modref_Buf[];                                                      


#endif /* ipl_summary_INCLUDED */

