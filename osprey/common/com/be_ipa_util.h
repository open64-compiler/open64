/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it would be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * Further, this software is distributed without any warranty that it is
 * free of the rightful claim of any third person regarding infringement
 * or the like.  Any license provided herein, whether implied or
 * otherwise, applies only to this software file.  Patent licenses, if
 * any, provided herein do not apply to combinations of this program with
 * other software, or any other product whatsoever.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write the Free Software Foundation, Inc., 59
 * Temple Place - Suite 330, Boston MA 02111-1307, USA.
 */

#ifndef be_ipa_util_INCLUDED
#define be_ipa_util_INCLUDED

#include "segmented_array.h"
#include "cxx_template.h"
#include "mempool.h"
#include "ipa_be_summary.h"

class IPA_BE_SUMMARY 
{
private:
  PU_IDX _pu_idx;

  SUMMARY_CONSTRAINT_GRAPH_PU_HEADER* _cg_proc_headers_array;
  SUMMARY_CONSTRAINT_GRAPH_NODE* _cg_nodes_array;
  SUMMARY_CONSTRAINT_GRAPH_STINFO* _cg_stinfos_array;
  SUMMARY_CONSTRAINT_GRAPH_CALLSITE* _cg_callsites_array;
  UINT32* _cg_nodeids_array;
  SUMMARY_CONSTRAINT_GRAPH_MODRANGE* _cg_modranges_array;
  SUMMARY_SILOED_REFERENCE *_siloed_references_array;

  mINT32 _pu_hdrs_count;
  mINT32 _cg_nodes_count;
  mINT32 _cg_stinfos_count;
  mINT32 _cg_callsites_count;
  mINT32 _cg_nodeids_count;
  mINT32 _cg_modranges_count;

public:
  IPA_BE_SUMMARY(MEM_POOL* m) {
  }

  PU_IDX GetPUIndex() { return _pu_idx; }
  SUMMARY_CONSTRAINT_GRAPH_PU_HEADER* GetProcHeadersArray()
  { return _cg_proc_headers_array; }
  SUMMARY_CONSTRAINT_GRAPH_NODE* GetCGNodesArray()
  { return _cg_nodes_array; }
  SUMMARY_CONSTRAINT_GRAPH_STINFO* GetCGStInfosArray()
  { return _cg_stinfos_array; }
  SUMMARY_CONSTRAINT_GRAPH_CALLSITE* GetCGCallsitesArray()
  { return _cg_callsites_array; }
  UINT32* GetCGNodeIdsArray()
  { return _cg_nodeids_array; }
  SUMMARY_CONSTRAINT_GRAPH_MODRANGE* GetCGModRangesArray()
  { return _cg_modranges_array; }

  SUMMARY_SILOED_REFERENCE* GetSiloedReferenceArray()
  { return _siloed_references_array; }

  mINT32 GetPUHdrsCount()      { return _pu_hdrs_count; }
  mINT32 GetCGNodesCount()     { return _cg_nodes_count; }
  mINT32 GetCGStInfosCount()   { return _cg_stinfos_count; }
  mINT32 GetCGCallsitesCount() { return _cg_callsites_count; }
  mINT32 GetCGNodeIdsCount()   { return _cg_nodeids_count; }
  mINT32 GetCGModRangesCount() { return _cg_modranges_count; }

  void SetPUIndex(PU_IDX idx) { _pu_idx = idx; }
  void SetProcHeadersArray(SUMMARY_CONSTRAINT_GRAPH_PU_HEADER* p)
  { _cg_proc_headers_array = p; }
  void SetCGNodesArray(SUMMARY_CONSTRAINT_GRAPH_NODE* p) 
  { _cg_nodes_array = p; }
  void SetCGStInfosArray(SUMMARY_CONSTRAINT_GRAPH_STINFO* p)
  { _cg_stinfos_array = p; }
  void SetCGCallsitesArray(SUMMARY_CONSTRAINT_GRAPH_CALLSITE* p)
  { _cg_callsites_array = p; }
  void SetCGNodeIdsArray(UINT32* p)
  { _cg_nodeids_array = p; }
  void SetCGModRangesArray(SUMMARY_CONSTRAINT_GRAPH_MODRANGE* p)
  { _cg_modranges_array = p; }

  void SetSiloedReferenceArray(SUMMARY_SILOED_REFERENCE* p)
  { _siloed_references_array = p; }
 
  void SetPUHdrsCount(mINT32 c)
  { _pu_hdrs_count = c; }
  void SetCGNodesCount(mINT32 c)
  { _cg_nodes_count = c; }
  void SetCGStInfosCount(mINT32 c)
  { _cg_stinfos_count = c; }
  void SetCGCallsitesCount(mINT32 c)
  { _cg_callsites_count = c; }
  void SetCGNodeIdsCount(mINT32 c)
  { _cg_nodeids_count = c; }
  void SetCGModRangesCount(mINT32 c)
  { _cg_modranges_count = c; }
};

struct BE_SUMMARY_HEADER
{
  mUINT64 offset;           // offset from beginning of section
  mUINT64 size;             // size in bytes
  mUINT32 entsize;          // size of each entry
};

class NEW_BE_SUMMARY_HEADER
{
private:
  mINT32 _version_number;
  mINT32 _minor_version_number;
  
  // procedure info
  // PU_IDX pu_idx;

  Elf64_Word _procedure_headers_offset;   

  // mUINT64 _mod_ref_info_offset;
  Elf64_Word _constraint_graph_nodes_offset;
  Elf64_Word _constraint_graph_stinfos_offset;
  Elf64_Word _constraint_graph_callsites_offset;
  Elf64_Word _constraint_graph_nodeids_offset;
  Elf64_Word _constraint_graph_modranges_offset;

  // be_summary for siloed references
  Elf64_Word _siloed_references_offset;

  // Constraint graph summary for Nystrom Alias Analyzer
  //   mUINT64 mod_ref_info_size;
  mINT32 _procedure_headers_size;
  mINT32 _constraint_graph_nodes_size;
  mINT32 _constraint_graph_stinfos_size;
  mINT32 _constraint_graph_callsites_size;
  mINT32 _constraint_graph_nodeids_size;
  mINT32 _constraint_graph_modranges_size;

  // be_summary for siloed references
  mINT32 _siloed_references_size;

public:
  NEW_BE_SUMMARY_HEADER() {
    BZERO(this, sizeof(NEW_BE_SUMMARY_HEADER));
  };
  
  void SetProcHeadersOffset(Elf64_Word s)
  { _procedure_headers_offset = s; }
  void SetCGNodesOffset(Elf64_Word s) 
  { _constraint_graph_nodes_offset = s; }
  void SetCGStInfosOffset(Elf64_Word s) 
  { _constraint_graph_stinfos_offset = s; }
  void SetCGCallsitesOffset(Elf64_Word s) 
  { _constraint_graph_callsites_offset = s; }
  void SetCGNodeIdsOffset(Elf64_Word s) 
  { _constraint_graph_nodeids_offset = s; }
  void SetCGModRangesOffset(Elf64_Word s) 
  { _constraint_graph_modranges_offset = s; }

  void SetSiloedReferencesOffset(Elf64_Word s)
  { _siloed_references_offset = s; }

  void SetProcHeadersSize(mINT32 s) 
  { _procedure_headers_size = s; }
  void SetCGNodesSize(mINT32 s) 
  { _constraint_graph_nodes_size = s; }
  void SetCGStInfosSize(mINT32 s) 
  { _constraint_graph_stinfos_size = s; }
  void SetCGCallsitesSize(mINT32 s) 
  { _constraint_graph_callsites_size = s; }
  void SetCGNodeIdsSize(mINT32 s) 
  { _constraint_graph_nodeids_size = s; }
  void SetCGModRangesSize(mINT32 s) 
  { _constraint_graph_modranges_size = s; }

  void SetSiloedReferencesSize(mINT32 s)
  { _siloed_references_size = s; }

  Elf64_Word GetProcHeadersOffset()
  { return _procedure_headers_offset; }
  Elf64_Word GetCGNodesOffset() 
  { return _constraint_graph_nodes_offset; }
  Elf64_Word GetCGStInfosOffset() 
  { return _constraint_graph_stinfos_offset; }
  Elf64_Word GetCGCallsitesOffset() 
  { return _constraint_graph_callsites_offset; }
  Elf64_Word GetCGNodeIdsOffset() 
  { return _constraint_graph_nodeids_offset; }
  Elf64_Word GetCGModRangesOffset() 
  { return _constraint_graph_modranges_offset; }

  Elf64_Word GetSiloedReferencesOffset()
  { return _siloed_references_offset; }

  mINT32 GetProcHeadersSize()
  { return _procedure_headers_size; }
  mINT32 GetCGNodesSize() 
  { return _constraint_graph_nodes_size; }
  mINT32 GetCGStInfosSize() 
  { return _constraint_graph_stinfos_size; }
  mINT32 GetCGCallsitesSize() 
  { return _constraint_graph_callsites_size; }
  mINT32 GetCGNodeIdsSize() 
  { return _constraint_graph_nodeids_size; }
  mINT32 GetCGModRangesSize() 
  { return _constraint_graph_modranges_size; }

  mINT32 GetSiloedReferencesSize()
  { return _siloed_references_size; }
};

struct pu_mod_ref_info
{
  PU_IDX pu_idx;            // pu id
  mUINT32 size;             // size in bytes of mod info (must equal ref)
  mUINT8 * mod;             // bit-vector for mod
  mUINT8 * ref;             // bit-vector for ref
  mUINT8 * same_entry_exit_value_or_1; // bit-vector for global vars whose
                                       // function exit value is the same as
                                       // their entry value, or that value is 1
};

typedef SEGMENTED_ARRAY<pu_mod_ref_info> MOD_REF_INFO_TAB;

extern MOD_REF_INFO_TAB Mod_Ref_Info_Table;

inline pu_mod_ref_info&
New_Mod_Ref_Info (UINT32 &index)
{
  return Mod_Ref_Info_Table.New_entry (index);
}

inline UINT
Mod_Ref_Info_Table_Size (void)  { return Mod_Ref_Info_Table.Size(); }

#endif // be_ipa_util_INCLUDED
