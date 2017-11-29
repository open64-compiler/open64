/*
 * Copyright (C) 2008-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
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
#ifndef cxx_ipa_cg_INCLUDED
#define cxx_ipa_cg_INCLUDED

#include <vector>

#ifndef mempool_allocator_INCLUDED
#include <mempool_allocator.h>
#endif

#ifndef pu_info_INCLUDED
#include "pu_info.h"
#endif /* pu_info_INCLUDED */

#ifndef ip_graph_INCLUDED
#include "ip_graph.h"
#endif

#ifndef cxx_ip_graph_trav_INCLUDED
#include "ip_graph_trav.h"
#endif

#ifndef xstats_INCLUDED
#include "xstats.h"
#endif

#ifndef ipl_summary_INCLUDED
#include "ipl_summary.h"
#endif

#ifndef ip_call_INCLUDED
#include "ip_call.h"
#endif // ip_call_INCLUDED

#ifndef dwarf_DST_mem_INCLUDED
#include "dwarf_DST_mem.h"            // Needed by ipc_file.h
#endif // dwarf_DST_mem_INCLUDED

#ifndef ipc_file_INCLUDED
#include "ipc_file.h"
#endif

#ifndef ip_bwrite_INCLUDED
#include "ipc_bwrite.h"
#endif

#ifndef cxx_ipa_cprop_INCLUDED
#include "ipa_cprop.h"
#endif

#ifndef cxx_ipa_summary_INCLUDED
#include "ipa_summary.h"
#endif

#ifndef ipc_pu_size_INCLUDED
#include "ipc_pu_size.h"
#endif

#ifndef fb_whirl_INCLUDED
#include "fb_whirl.h"
#endif

// forward class declarations to minimize included headers
class CALLEE_STATE;
class IPAA_NODE_INFO;
class IPA_NODE_SECTION_INFO;
class IPA_CALL_GRAPH;
class IPL_SUMMARY_PTRS;


// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// nenad, 04/21/97:
// 1. IPA_ICALL_NODE and IPA_ICALL_LIST will go away
//    when call graph is updated to contain dummy nodes
//    and edges corresponding to indirect calls
// 2. Passing parameters by non-const reference, as in
//    IPA_ICALL_NODE::Set_Value is against Mongoose coding 
//    rules and it needs to be revisited
// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++

// ====================================================================
//
// IPA_ICALL_NODE
//
// Contains an entry for an indirect call (i.e. via a function
// pointer).  Also used for lists of opaque calls (i.e. to routines
// we can't see, such as library routines).
//
// ====================================================================

class IPA_ICALL_NODE
{
private:
  SUMMARY_CALLSITE *_c;		// pointer into the callsite local info
  SUMMARY_VALUE _v;		// if func. ptr. is constant,
				// describe the callee here.
public:
  // Constructor:
  IPA_ICALL_NODE ( SUMMARY_CALLSITE *cs ) 
  {
    _c = cs;
    _v.Init ();
  }

  void Set_Callsite ( SUMMARY_CALLSITE *cs )    { _c = cs; }
  SUMMARY_CALLSITE* Callsite() const		{ return _c; }

  void Set_Value ( SUMMARY_VALUE &value )       { _v = value; }
  SUMMARY_VALUE& Value () 		        { return _v; }
};

// ====================================================================
//
// IPA_ICALL_LIST
//
// List of indirect calls (or opaque calls).
//
// ====================================================================

typedef vector<IPA_ICALL_NODE*> IPA_ICALL_LIST;

#ifdef _LIGHTWEIGHT_INLINER
typedef vector<char*> INLINED_BODY_LIST;
#endif // _LIGHTWEIGHT_INLINER

typedef UINT32 IPA_NODE_INDEX;		// index to the IPA_NODE_ARRAY in
					// IPA_CALL_GRAPH
typedef UINT32 IPA_EDGE_INDEX;

// node of the IPA's call graph.  Each node represent a PU
class IPA_NODE
{
private:
  // IPA_NODE flags
  static const mUINT32 _clone =                  0x1;		  
  static const mUINT32 _visited =                0x2;		  
  static const mUINT32 _mempool_init =           0x4;	
  static const mUINT32 _deletable =              0x8; 
  static const mUINT32 _dont_delete =           0x10;	
  static const mUINT32 _processed =             0x20;	
  static const mUINT32 _recursive_in_edge =     0x40;	
  static const mUINT32 _feedback =              0x80;	
  static const mUINT32 _constants =            0x100;	
  static const mUINT32 _clone_candidate	=      0x200;
  static const mUINT32 _padding	=              0x400;	
  static const mUINT32 _non_local =            0x800;	
  static const mUINT32 _no_aggressive_cprop = 0x1000;	
  static const mUINT32 _split_commons =       0x2000;	
  static const mUINT32 _internal_part =       0x4000; // obsolete ?
  static const mUINT32 _use_kill =            0x8000;	
  static const mUINT32 _incoming_back_edge = 0x10000;   
  static const mUINT32 _quasi_clone =        0x20000;   
  static const mUINT32 _preoptimized =       0x40000;   
  static const mUINT32 _has_aliased_formal = 0x80000;
#ifdef KEY
  static const mUINT32 _builtin =           0x100000;	// IPA builtin
  static const mUINT32 _pu_write_complete = 0x200000;	// all EH info processed
  static const mUINT32 _recursive =	    0x400000;	// recursive
  static const mUINT32 _merged =	    0x800000;	// Merged node
  static const mUINT32 _can_throw = 	   0x1000000;   // PU can throw exc.
  static const mUINT32 _ehinfo_updated =   0x2000000;   // summary updated
  static const mUINT32 _pending_icalls =   0x4000000;   // need icall conversion
#endif
  static const mUINT32 _part_inl_candidate = 0x8000000; // whether this PU 
                                                        // a partial inlining
                                                        // candidate
  static const mUINT32 _leftover_rtn_const = 0x10000000;
                                             // whether the leftover function
                                             // from partial inlining returns
                                             // an (int) constant.

  static const mUINT32 _pending_vfuns =   0x20000000;   // need icall conversion
  // map to the file I/O info
  mINT32 _file_index;			// index into the file header structure
  mINT32 _proc_info_index;		// index into the proc info structure
  mINT32 _summary_proc_index;	        // index into SUMMARY_PROCEDURE array
  NODE_INDEX _vertex_index;		// the graph node id 
  IPA_NODE_INDEX _array_index;		// index into the IPA_NODE_ARRAY
					// (in IPA_CALL_GRAPH)

  // attributes of this node
  ST                *_func_st;		// ST corresponds to this function
  SCOPE             *_scope_tab;	// Scope table for this node
  IPO_SYMTAB        *_cloned_symtab;	// Callee's SYMTAB cloned as a result
  CALLEE_STATE      *_callee_state;	// callee state

  PU_SIZE            _pu_size;		// estimated size of this PU
#if defined(TARG_SL)
  mINT32             _emit_id;          // emit id of the PU in the *.I, start from 0
#endif // TARG_SL

#ifdef KEY
  struct pu_info    *_builtin_pu_info;
  mUINT32	    _sizeof_eh_spec;	// # of types in eh-specification
  mINT32	    _file_id;		// id of the file containing this pu
#endif

  IPAA_NODE_INFO*        _mod_ref_info; // mod/ref information
  VALUE_DYN_ARRAY*       _cprop_annot;  // annotation for parameter constants
  GLOBAL_ANNOT*          _global_annot; // annotation for global constants
  IPA_NODE_SECTION_INFO* _array_annot;  // array section annotation
  FEEDBACK*		 _feedback_info; // feedback info

  MEM_POOL _mem_pool;			// this mempool is used for annotations
  WN_MAP   _parent_map;		    	// parent map used in transformation
					// of inlining or cloning
  IPA_ICALL_LIST _icall_list;           // List of indirect calls from this PU
  IPA_ICALL_LIST _ocall_list;           // List of opaque calls from this PU

  // this includes icalls (and ocalls?) 
  // it should be removed when icalls are added to the call graph
  UINT16 _total_succ;		        // total number of successors
  mUINT32 _max_region_id;		// max region id 

  mUINT32          _flags;		// various Boolean attribute flags
  INT32            _partition_num;
#ifdef _LIGHTWEIGHT_INLINER
  INLINED_BODY_LIST  _inlined_list;     // Hold pts to all inlined callees
                                        // for this node
#endif // _LIGHTWEIGHT_INLINER

  // Auxiliary fields added for partial inlining.
  WN*       _leftover_body;           // leftover function body
  INT64     _leftover_rtn_val;        // The return const value of the leftover
                                      // function from partial inlining.
                                      // Meaningful only when 
                                      // _leftover_rtn_const is set.

public:

  // Constructor
  IPA_NODE (ST* st, 
            INT32 file_idx,
            INT32 p_idx, 
            INT32 summary_idx,
            NODE_INDEX index,
	    IPA_NODE_INDEX array_index) :
    _proc_info_index (p_idx),
    _file_index (file_idx),
    _summary_proc_index (summary_idx),
    _vertex_index (index),
    _array_index (array_index),
    _func_st (st),
    _scope_tab (NULL),
    _cloned_symtab (NULL),
    _callee_state (NULL),
    _mod_ref_info (NULL),
    _cprop_annot (NULL),
    _array_annot (NULL),
    _global_annot (NULL),
    _feedback_info (NULL),
    _parent_map (0),
    _icall_list (),
    _ocall_list (),
    _max_region_id (0),
#ifdef _LIGHTWEIGHT_INLINER
    _inlined_list (),
#endif // _LIGHTWEIGHT_INLINER
    _flags (0),
    _partition_num(0)
#ifdef KEY
    ,_builtin_pu_info (NULL)
    ,_sizeof_eh_spec (0)
    ,_file_id (-1)
#endif
    ,_leftover_body (NULL)
  {
#ifdef KEY
    // If we are constructing for a builtin, then skip the info that a builtin
    // doesn't have.
    if (_file_index == -1) {
      _total_succ = 0;
      Is_True ((_flags & _mempool_init) == 0,
               ("Uninitialized IPA NODE mempool"));
      Is_True(st != 0, ("IPA NODE must have valid st"));
      return;
    }
#endif

    SUMMARY_PROCEDURE* summary_proc = this->Summary_Proc();
    _pu_size.Set_PU_Size (summary_proc->Get_bb_count (), 
                          summary_proc->Get_stmt_count (),
                          summary_proc->Get_call_count ());
    _total_succ = summary_proc->Get_callsite_count();

    Is_True ((_flags & _mempool_init) == 0,
             ("Uninitialized IPA NODE mempool"));
    Is_True(st != 0, ("IPA NODE must have valid st"));
    Is_True(p_idx >= 0 && p_idx < IP_FILE_HDR_num_procs(File_Header()),
            ("Bad proc index, should be in range 0 <= idx < %d",
             IP_FILE_HDR_num_procs(File_Header())));
  }
    
  // Access functions

  void Set_File_Index ( INT32 i )	{ _file_index = i; }
  INT32 File_Index () const		{ return _file_index; }
  void Set_Partition_Num(INT32 num)     { _partition_num = num; }
  INT32 Get_Partition_Num(void)         { return _partition_num; } 
  void Set_Proc_Info_Index ( INT32 i )  { _proc_info_index = i; }
  INT32 Proc_Info_Index () const
  { 
    Is_True(_proc_info_index >= 0 && 
            _proc_info_index < IP_FILE_HDR_num_procs(File_Header()),
            ("Proc idx %d should be in range 0 <= idx < %u (input file %s)",
             _proc_info_index, 
             IP_FILE_HDR_num_procs(File_Header()), 
             Input_File_Name()));
    return _proc_info_index; 
  }

  void Set_Summary_Proc_Index (INT32 i) { _summary_proc_index = i;}
  INT32 Summary_Proc_Index () const	{ return _summary_proc_index; }

  NODE_INDEX Node_Index () const	{ return _vertex_index; }

  IPA_NODE_INDEX Array_Index () const	{ return _array_index; }

  void Set_Func_ST (ST* st)		{ _func_st = st; }
  ST *Func_ST () const	                { return _func_st; }

  void Set_Scope (SCOPE* scope_tab)     { _scope_tab = scope_tab; }
  SCOPE* Scope_Table ()			{ return _scope_tab; }
  SCOPE* Scope ();

  void Set_Cloned_Symtab (IPO_SYMTAB *symtab)   { _cloned_symtab = symtab; }
  void Clear_Cloned_Symtab ();
  IPO_SYMTAB* Cloned_Symtab ()	                { return _cloned_symtab; }

  void Set_PU_Size (const PU_SIZE& sze)		{ _pu_size = sze;};
  const PU_SIZE& PU_Size () const		{ return _pu_size;};
  void UpdateSize (IPA_NODE* callee, IPA_EDGE* edge);

  void Set_Callee_State (CALLEE_STATE* ste)     { _callee_state = ste; }
  CALLEE_STATE *Callee_State () const           { return _callee_state; }

  void Set_Mod_Ref_Info (IPAA_NODE_INFO* info)  { _mod_ref_info = info; }
  IPAA_NODE_INFO* Mod_Ref_Info () const         { return _mod_ref_info; }

  void Set_Cprop_Annot (VALUE_DYN_ARRAY* annot) { _cprop_annot = annot; }
  VALUE_DYN_ARRAY* Cprop_Annot() const          { return _cprop_annot; }

  void Set_Global_Annot (GLOBAL_ANNOT* annot) { _global_annot = annot; }
  GLOBAL_ANNOT* Global_Annot () const     { return _global_annot; }

  void Set_Feedback_Info (FEEDBACK* fb)		{ _feedback_info = fb; }
  FEEDBACK* Feedback_Info () const		{ return _feedback_info; }

  void Set_Section_Annot (IPA_NODE_SECTION_INFO* annot) 
  { 
    _array_annot = annot; 
  }
  IPA_NODE_SECTION_INFO* Section_Annot () const { return _array_annot;};

  MEM_POOL* Mem_Pool () const { return (MEM_POOL*)(&_mem_pool); }

  void Set_Parent_Map(WN_MAP map)       { _parent_map = map; }
  WN_MAP Parent_Map(void) const         { return _parent_map; }

  IPA_ICALL_LIST& Icall_List ()         { return _icall_list; }

  IPA_ICALL_LIST& Ocall_List ()         { return _ocall_list; }

  void Set_Total_Succ (INT32 i)	        { _total_succ = i; }
  void Incr_Total_Succ ()	        { _total_succ++; }
  UINT16 Total_Succ () const            { return _total_succ; }

  void Set_Max_Region_Id (mUINT32 i)	{ _max_region_id = i; }
  mUINT32 Max_Region_Id () const        { return _max_region_id; }

  // Access to IPA_NODE flags
  void Set_Flags (UINT32 flags)	        { _flags |= flags; }
  void Clear_Flags (UINT32 flags)	{ _flags &= ~flags; }
  UINT32 Flags () const		        { return _flags; }

#ifdef _LIGHTWEIGHT_INLINER
  INLINED_BODY_LIST& Inlined_list ()    { return _inlined_list; }
#endif // _LIGHTWEIGHT_INLINER

  // return true if the call expresion node has an edge in the 
  // IPA_NODE
  BOOL has_edge_for_node(WN *wn) const;

  // node is the result of cloning  
  void Set_Clone ()	        { _flags |= _clone; }
  BOOL Is_Clone () const        { return _flags & _clone; }

  // node is visited in a callgraph walk
  void Set_Visited ()		{ _flags |= _visited; }
  void Clear_Visited ()         { _flags &= ~_visited; }
  BOOL Is_Visited () const	{ return _flags & _visited; }

  // mem pool is initialized
  void Set_Mempool_Initialized ()       { _flags |= _mempool_init; }
  void Clear_Mempool_Initialized ()     { _flags &= ~_mempool_init; }
  BOOL Is_Mempool_Initialized () const	{ return _flags & _mempool_init; }

  // function can be DCE-d
  void Set_Deletable () 
  {
    if ((_flags & _dont_delete) == 0)
      _flags |= _deletable;
  }
  void Clear_Deletable () 
  {
    _flags &= ~_deletable;
  }
  BOOL Is_Deletable () const 
  {
    return  ((_flags & _deletable) && !(_flags & _dont_delete));
  }
  
  // function must not be deleted
  void Set_Undeletable () 
  {
    _flags = (_flags & ~_deletable) | _dont_delete;
  }
  BOOL Is_Undeletable () const 
  {
    return Summary_Proc()->Is_no_delete () || (_flags & _dont_delete);
  }

  // node has been completely processed
  void Set_Processed ()		{ _flags |= _processed;}
  BOOL Is_Processed () const	{ return _flags & _processed;}

  // node has a recursive in edge
  void Set_Recursive_In_Edge () { _flags |= _recursive_in_edge; }
  BOOL Has_Recursive_In_Edge () const 
  {	
    return ( _flags & _recursive_in_edge); 
  }
  
  // node has feedback frequency map
  void Set_Feedback ()		{ _flags |= _feedback; }
  BOOL Has_Feedback () const	{ return _flags & _feedback; }

  // node has propagated constants
  void Set_Propagated_Const ()          { _flags |= _constants; }
  void Clear_Propagated_Const ()        { _flags &= ~_constants; }
  BOOL Has_Propagated_Const ()          { return _flags & _constants; }

  // node needs to be cloned
  void Set_Clone_Candidate ()	        { _flags |= _clone_candidate; }
  void Clear_Clone_Candidate ()		{ _flags &= ~_clone_candidate; }
  BOOL Is_Clone_Candidate () const	{ return _flags & _clone_candidate; }

  // node needs to be partially inlined
  void Set_Part_Inl_Candidate ()	{ _flags |= _part_inl_candidate; }
  void Clear_Part_Inl_Candidate ()	{ _flags &= ~_part_inl_candidate; }
  BOOL Is_Part_Inl_Candidate () const	{ return _flags & _part_inl_candidate; }

  // Whether the leftover function (from partial inlining) returns an 
  // int const.
  void Set_Leftover_Rtn_Const ()	{ _flags |= _leftover_rtn_const; }
  void Clear_Leftover_Rtn_Const ()	{ _flags &= ~_leftover_rtn_const; }
  BOOL Is_Leftover_Rtn_Const () const	{ return _flags & _leftover_rtn_const; }

  // node has common blocks that need to be padded
  void Set_Needs_Padding ()     { _flags |= _padding; }
  BOOL Needs_Padding () const   { return _flags & _padding; }

  // node is non-local (used by the standalone inliner)
  void Set_Non_Local ()	        { _flags |= _non_local; }
  BOOL Is_Non_Local () const	{ return _flags & _non_local; }

  // don't do aggressive cprop for this node
  void Set_No_Aggr_Cprop ()       { _flags |= _no_aggressive_cprop; }
  BOOL Has_No_Aggr_Cprop () const { return _flags & _no_aggressive_cprop; }

  // do splitting of commons for this node
  void Set_Split_Commons ()	        { _flags |= _split_commons; }
  void Clear_Split_Commons ()	        { _flags &= ~_split_commons; }
  BOOL Needs_Split_Commons () const	{ return _flags & _split_commons; }

  // node has kill/euse info for scalars
  void Set_Use_Kill ()		{ _flags |= _use_kill;};
  BOOL Has_Use_Kill () const	{ return _flags & _use_kill;};

  // node has an incoming back edge
  void Set_Incoming_Back_Edge () { _flags |= _incoming_back_edge; }
  BOOL Has_Incoming_Back_Edge () const { return _flags & _incoming_back_edge; }

  // node is a partial clone (edges are set, but not WN, ST)
  void Set_Quasi_Clone ()       { _flags |= _quasi_clone; }
  void Clear_Quasi_Clone ()     { _flags &= ~_quasi_clone; }
  BOOL Is_Quasi_Clone () const  { return _flags & _quasi_clone; }

  // node was run through preopt to rebuild array section summaries
  void Set_Preoptimized ()      { _flags |= _preoptimized; }
  BOOL Is_Preoptimized () const { return _flags & _preoptimized; }

#ifdef KEY
  // node is for a IPA builtin
  void Set_Builtin ()           { _flags |= _builtin; }
  BOOL Is_Builtin () const      { return _flags & _builtin; }
  // PU has been written out, i.e. all EH info have been fixed, don't try
  // to fix again.
  void Set_PU_Write_Complete () { _flags |= _pu_write_complete; }
  BOOL Is_PU_Write_Complete () const { return _flags & _pu_write_complete; }

  // number of typeinfos in exception specification for this PU
  void Set_EH_spec_size (mUINT32 s) { _sizeof_eh_spec = s; }
  mUINT32 EH_spec_size () const	    { return _sizeof_eh_spec; }

  // is node recursive?
  void Set_Recursive () { _flags |= _recursive; }
  BOOL Is_Recursive () { return _flags & _recursive; }

  void Set_Merged () { _flags |= _merged; }
  BOOL Is_Merged () { return _flags & _merged; }

  void Set_PU_Can_Throw () { _flags |= _can_throw; }
  BOOL PU_Can_Throw () { return _flags & _can_throw; }

  void Set_EHinfo_Updated () { _flags |= _ehinfo_updated; }
  BOOL EHinfo_Updated () { return _flags & _ehinfo_updated; }

  void Set_File_Id (mINT32 f) { _file_id = f; }
  mINT32 File_Id () const	{ return _file_id; }

  void Set_Pending_Icalls () { _flags |= _pending_icalls; }
  BOOL Has_Pending_Icalls () const { return _flags & _pending_icalls; }

  void Set_Pending_Virtual_Functions () { _flags |= _pending_vfuns; }
  BOOL Has_Pending_Virtual_Functions () const { return _flags & _pending_vfuns; }

  static mINT32 next_file_id; // public field
#endif

  // node contains SCLASS_FORMAL variables that are based on another formal.
  // When we convert a formal parameter to a local variable, we need to know
  // if there are other STs that based on this variable, and convert their
  // storage_class accordingly.  This rarely happens--only in K&R-style
  // parameter declarations--and we don't want to scan the entire local
  // symtab just for that.  See IPA_Fix_Formal_Constants in ipo_const.cxx.
  void Set_Aliased_Formal ()	   { _flags |= _has_aliased_formal; }
  BOOL Has_Aliased_Formal () const { return _flags & _has_aliased_formal; }


  SUMMARY_PROCEDURE* Summary_Proc () const
  { 
    return IPA_get_procedure_array(this) + _summary_proc_index; 
  }

  SUMMARY_SYMBOL* Summary_Symbol () const	
  { 
    return IPA_get_symbol_array(this) + Summary_Proc()->Get_symbol_index(); 
  }

  // Attributes from the SUMMARY_PROCEDURE
  void Clear_Has_Pstatics ()	 { Summary_Proc()->Reset_has_pstatics(); }
  BOOL Has_Pstatics ()		 { return Summary_Proc()->Has_pstatic(); }

  BOOL Has_Direct_Mod_Ref() 
  { 
    return (Summary_Proc() ? Summary_Proc()->Is_direct_mod_ref() : TRUE); 
  }

  mUINT16 Num_Formals () const { return Summary_Proc()->Get_formal_count();};

  // Interface to symbol table
  PU& Get_PU () const
  {
    Is_True(_func_st != 0, ("IPA_NODE: null ST pointer"));
    Is_True(ST_pu(_func_st) > 0 && ST_pu(_func_st) < Pu_Table.size(),
            ("PU index %d should be in range 0 < idx < %d",
             ST_pu(_func_st), Pu_Table.size()));
    return Pu_Table[ST_pu(_func_st)];
  }

  // Attributes from the PU
  void Set_Must_Inline_Attrib ()        { Set_PU_must_inline (Get_PU ()); }
  void Clear_Must_Inline_Attrib ()	{ Clear_PU_must_inline (Get_PU ()); }
  BOOL Has_Must_Inline_Attrib () const	{ return PU_must_inline (Get_PU ()); }
  
  void Set_Noinline_Attrib ()		{ Set_PU_no_inline (Get_PU ()); }
  void Clear_Noinline_Attrib ()		{ Clear_PU_no_inline (Get_PU ()); }
  BOOL Has_Noinline_Attrib () const	{ return PU_no_inline (Get_PU ()); }

  void Set_Inline_Attrib ()	  { Set_PU_is_inline_function (Get_PU ()); }
  void Clear_Inline_Attrib ()     { Clear_PU_is_inline_function (Get_PU ()); }
  BOOL Has_Inline_Attrib () const { return PU_is_inline_function (Get_PU ()); }

  void Set_Leftover_Body(WN* wn)        { _leftover_body = wn; }
  WN  *Leftover_Body()                  { return _leftover_body; }
  void      Set_Leftover_Rtn_Val(INT64 val)  
                                        { _leftover_rtn_val = val; }
  INT64     Leftover_Rtn_Val()          { return _leftover_rtn_val; }

  BOOL Is_Nested_PU () const { return PU_is_nested_func(Get_PU()); }
    
  BOOL Has_Varargs () const 
  {
    return TY_is_varargs (Ty_Table[PU_prototype (Get_PU ())]);
  }

  BOOL Is_Lang_F77() const      { return PU_f77_lang (Get_PU()); }
  BOOL Is_Lang_F90() const      { return PU_f90_lang (Get_PU()); }
#ifdef KEY
  BOOL Is_Lang_CXX() const      { return PU_cxx_lang (Get_PU()); }
#endif


  UINT32 Weight (void) const	{ return _pu_size.Weight (); }

  SYMTAB_IDX Lexical_Level(void) const  { return PU_lexical_level(Get_PU()); }

  IP_FILE_HDR& File_Header(void) const
  {
    Is_True(_file_index >= 0 && _file_index < IP_File_header.size(),
            ("IPA_NODE: file index %d should be in range 0 <= idx < %d",
             _file_index, IP_File_header.size()));
    return IP_File_header[_file_index];
  }

  FILE_INFO& File_Info(void) const 
  { 
    return IP_FILE_HDR_file_info(File_Header()); 
  }

  DST_TYPE File_Dst(void) const
  {
    return IP_FILE_HDR_dst(File_Header());
  }

  const char *Input_File_Name () const 
  { 
    return IP_FILE_HDR_file_name (File_Header()); 
  }
  const char *Output_File_Name () const 
  { 
    DevWarn("IPA_NODE::Output_File_Name is not yet implemented");
    return "Unknown_Output_File.I";
  }

  char *Name (void) const
  {
    Is_True (_func_st != 0, ("IPA_NODE: null st"));
    return ST_name(_func_st);
  }

  struct pu_info *PU_Info(void) const 
  {
    Is_True(_func_st != 0, ("IPA NODE must have valid st"));

#ifdef KEY
    if (this->Is_Builtin())
      return _builtin_pu_info;
#endif

    return IP_FILE_HDR_proc_info (File_Header())[Proc_Info_Index()].info;
  }

#ifdef KEY
  void Set_Builtin_PU_Info (struct pu_info *p) { _builtin_pu_info = p; }

  struct pu_info *Builtin_PU_Info() { return _builtin_pu_info; }
#endif

  WN_MAP_TAB* Map_Table() const
  {
      return (PU_Info_maptab(PU_Info()));
  }

  DST_IDX Dst_Index() const
  {
      return (PU_Info_pu_dst(PU_Info()));
  }

  // can the function potentially be called from outside
  // ipa_cg.cxx:Externally_Callable
  BOOL Is_Externally_Callable (); 

  void Read_PU (BOOL = TRUE);

  WN* Whirl_Tree (BOOL = TRUE);

  void Set_Whirl_Tree (WN *wn);

  void Write_PU ();

  void Un_Read_PU ();

  void Set_Global_Tables(IPA_CALL_GRAPH*) 
  {
    DevWarn("Use IPA_NODE_CONTEXT instead of IPA_NODE::Set_Global_Tables");
  }

  // Are we suppressing optimization for this node? 
  BOOL Should_Be_Skipped () const 
  {
    static BOOL reported = FALSE;
    if (!reported) {
	reported = TRUE;
	DevWarn("IPA_NODE::Skip is not yet implemented");
    }
    return FALSE;
  }

  // state information for recycling sts
  void Cleanup_State(IPA_CALL_GRAPH*)
  {
    DevWarn("IPA_NODE::Cleanup_State is not yet implemented");
  }

  void Print (FILE* fp) const 
  {
    fprintf (fp, "%s\n", Name());
  }

  void Print(FILE *fp, IPA_CALL_GRAPH *cg);

  void Trace () const 
  {
    Print (TFile);
  }

  BOOL Has_frequency() const 	{ return Summary_Proc()->Has_PU_freq(); };


  SUMMARY_FEEDBACK *Get_feedback () const {
#if (defined(_STANDALONE_INLINER) || defined(_LIGHTWEIGHT_INLINER))
    return NULL;
#else 
#ifdef KEY
    /* If a proc is never invoked, then Summary_Proc()->Get_feedback_index()
       will always return 0 by default, which will give ipa some other function's
       feedback info.
    */
    if( Summary_Proc()->Is_Never_Invoked() ){
      return NULL;
    }
#endif
    return IPA_get_feedback_array (this) + Summary_Proc()->Get_feedback_index ();
#endif 
  }

  FB_FREQ Get_frequency() {
    SUMMARY_FEEDBACK* fb = Get_feedback();
    return (fb? fb->Get_frequency_count (): FB_FREQ_UNKNOWN);
  };


  UINT16 Get_wn_count () {
    SUMMARY_FEEDBACK* fb = Get_feedback();
    return (fb? fb->Get_wn_count(): 0);
  };

  FB_FREQ Get_cycle_count_2 () {
    SUMMARY_FEEDBACK* fb = Get_feedback();
    return (fb? fb->Get_cycle_count_2(): FB_FREQ_UNKNOWN);
  };

  FB_FREQ Get_cycle_count () {
    SUMMARY_FEEDBACK* fb = Get_feedback();
    return (fb? fb->Get_cycle_count (): FB_FREQ_UNKNOWN);
  };

#ifdef KEY
  UINT64 Get_func_runtime_addr () {
    SUMMARY_FEEDBACK * fb = Get_feedback();
    return (fb ? fb->Get_func_runtime_addr () : 0);
  }
#endif

#ifdef _LIGHTWEIGHT_INLINER
  void Add_to_inlined_list (char *body) {
    _inlined_list.push_back(body);
  }

  void Free_inlined_list();

#endif // _LIGHTWEIGHT_INLINER

#if defined(TARG_SL)
  void Set_Emit_Id(mINT32 i) { _emit_id = i; }
  mINT32 Emit_Id() const     { return _emit_id; }
#endif // TARG_SL

}; // IPA_NODE

#ifdef KEY
#include <ext/hash_map>
#include <functional>
struct option_cmp : public std::binary_function<char *, char *, bool>
{
  bool operator() (char * s1, char * s2)
  {
        return (strcmp (s1, s2) < 0);
  }
};

struct hashfn
{
    size_t operator()(const IPA_NODE* n) const {
        return reinterpret_cast<size_t>(n);
    }
};

struct eqnode
{
  bool operator()(const IPA_NODE* n1, const IPA_NODE* n2) const
  {
    return n1 == n2;
  }
};
extern vector<IPA_NODE *> emit_order;

class Nodes_To_Edge
{
  NODE_INDEX caller_id, callee_id;
  IPA_EDGE * e;
  public:
  Nodes_To_Edge (NODE_INDEX from, NODE_INDEX to, IPA_EDGE * edge=0) :
  		caller_id (from), callee_id (to), e (edge) {}
  IPA_EDGE * Edge (void) const { return e; }
  NODE_INDEX Caller (void) const { return caller_id; }
  NODE_INDEX Callee (void) const { return callee_id; }
  bool operator== (const Nodes_To_Edge * o)
  {
  	return (caller_id == o->caller_id && callee_id == o->callee_id);
  }
};
#endif

class IPA_EDGE
{
private:
  // IPA_EDGE flags
  static const mUINT32 _processed	= 0x01;
  static const mUINT32 _constants	= 0x02;
  static const mUINT32 _deletable	= 0x04;
  static const mUINT32 _recursive	= 0x08;
  static const mUINT32 _inline		= 0x10;
  static const mUINT32 _must_inline	= 0x20;
  static const mUINT32 _no_inline	= 0x40;
  static const mUINT32 _partial_inline	= 0x80;
  static const mUINT32 _updated_caller  = 0x100; // for a deletable edge caller has been updated.  
  static const mUINT32 _orig_devirtualized  = 0x200;
  static const mUINT32 _new_devirtualized  = 0x400;
  
  EDGE_INDEX _edge_index;			// index to the edge array in graph
  IPA_EDGE_INDEX _array_index;		// index into the IPA_EDGE_ARRAY
  SUMMARY_CALLSITE *_c;                 // summary information
  WN *_w;				// WHIRL node of the callsite
#ifdef KEY
  WN *_eh_wn;				// enclosing eh-region wn
  LABEL_IDX try_label;			// try label from enclosing try-region if any
  WN *_mp_wn;				// enclosing mp-region wn
#endif

  VALUE_DYN_ARRAY       *_cprop_annot;  // constant propagation annotation

  mUINT32 _flags;			// various attributes of edge
  mUINT32 _readonly_actuals;		// bitmap for readonly actual param.
  mUINT32 _pass_not_saved_actuals;	// bitmap for addr_passed_but_not_saved
  UINT32 _reason_ID; float _reason_data; 	//IPA_TRACE_TUNING

public:
  // constructor
  IPA_EDGE (SUMMARY_CALLSITE *c,
	    EDGE_INDEX index,
	    IPA_EDGE_INDEX array_index) : 
    _edge_index(index),
    _array_index (array_index),
    _c(c),
    _w(0),
#ifdef KEY
    _eh_wn(0),
    try_label(0),
    _mp_wn(0),
#endif
    _cprop_annot(0),
    _flags(0),
    _readonly_actuals(0),
    _pass_not_saved_actuals(0),
	_reason_ID(0),
	_reason_data(0.0)
  {}

    
  UINT32 reason_id() {return _reason_ID;}
  float reason_data() {return _reason_data;}
  void Set_reason_id(UINT32 i) { _reason_ID = i;}
  void Set_reason_data(float i) { _reason_data = i;}
  // access functions
  void Set_Edge_Index (EDGE_INDEX i)	{ _edge_index = i; }
  EDGE_INDEX Edge_Index () const	{ return _edge_index; }

  IPA_EDGE_INDEX Array_Index () const	{ return _array_index; }

  SUMMARY_CALLSITE* Summary_Callsite () const    { return _c; }

  void Set_Whirl_Node (WN* w)   { _w = w; }
  WN* Whirl_Node () const       { return _w; }

#ifdef KEY
  void Set_EH_Whirl_Node (WN* w) { _eh_wn = w; }
  WN* EH_Whirl_Node () const	 { return _eh_wn; }

  // try_label is not being used currently
  void Set_Try_Label (LABEL_IDX l) { try_label = l; }
  LABEL_IDX Try_Label () const	   { return try_label; }

  void Set_MP_Whirl_Node (WN * w) { _mp_wn = w; }
  WN * MP_Whirl_Node () const     { return _mp_wn; }
#endif

  void Set_Cprop_Annot (VALUE_DYN_ARRAY* annot)	{ _cprop_annot = annot; }
  VALUE_DYN_ARRAY* Cprop_Annot () const	        { return _cprop_annot; }

  // access to flags
  void Set_Processed ()			{ _flags |= _processed; }
  BOOL Is_Processed () const		{ return _flags & _processed; }

  void Set_Propagated_Const ()	        { _flags |= _constants; } 
  BOOL Has_Propagated_Const () const    { return _flags & _constants; }

  void Set_Deletable ()			{ _flags |= _deletable; }
  BOOL Is_Deletable () const		{ return _flags & _deletable; }

  void Set_Updated_Caller()      	{ _flags |= _updated_caller; }
  BOOL Has_Updated_Caller() const	{ return _flags & _updated_caller; }

  void Set_Recursive ()			{ _flags |= _recursive; }
  BOOL Is_Recursive () const		{ return _flags & _recursive;}

  void Set_Must_Inline_Attrib ()	{ _flags |= _must_inline; }
  BOOL Has_Must_Inline_Attrib () const	{ return _flags & _must_inline; }

  void Set_Inline_Attrib ()		{ _flags |= _inline; }
  BOOL Has_Inline_Attrib () const	{ return _flags & _inline; }

  void Set_Noinline_Attrib ()		{ _flags |= _no_inline; }
  BOOL Has_Noinline_Attrib () const	{ return _flags & _no_inline; }

  void Set_Partial_Inline_Attrib ()	{ _flags |= _partial_inline; }
  void Clear_Partial_Inline_Attrib ()	{ _flags &= ~_partial_inline; }
  BOOL Has_Partial_Inline_Attrib () const { return _flags & _partial_inline; }

  void Clear_All_Inline_Attrib ()	{ _flags &= ~(_inline|_must_inline|
                                                      _partial_inline); }

  void Set_Orig_Devirtualized ()		{ _flags |= _orig_devirtualized; }
  BOOL Is_Orig_Devirtualized () const	{ return _flags & _orig_devirtualized; }

  void Set_New_Devirtualized ()		{ _flags |= _new_devirtualized; }
  BOOL Is_New_Devirtualized () const	{ return _flags & _new_devirtualized; }

  // we use a bit vector to represent actual parameters that are readonly
  // up to a max. of 32 parameters are recorded, the rests are ignored.
  // If the actual is an LDA x, then x is not changed by the call.
  // If the actual is LDID p, then *p is not changed by the call.

  void Set_Param_Readonly (INT32 pos)   { _readonly_actuals |= (1 << pos); }
  void Clear_Param_Readonly (INT32 pos) { _readonly_actuals &= ~(1 << pos); }
  BOOL Is_Param_Readonly (INT32 pos) const 
  {
    return _readonly_actuals & (1 << pos);
  }
  BOOL Has_Readonly_Param () const	{ return _readonly_actuals != 0; }
    
  // do the same for address passed but not saved
  void Set_Param_Pass_Not_Saved (INT32 pos) 
  {
    _pass_not_saved_actuals |= (1 << pos);
  }
  void Clear_Param_Pass_Not_Saved (INT32 pos) 
  {
    _pass_not_saved_actuals &= ~(1 << pos);
  }
  BOOL Is_Param_Pass_Not_Saved (INT32 pos) const 
  {
    return _pass_not_saved_actuals & (1 << pos);
  }
  BOOL Has_Pass_Not_Saved_Param () const 
  {
    return _pass_not_saved_actuals != 0;
  }

  static INT Max_Num_Readonly_Actuals() { return sizeof(mUINT32) * 8; }
    
  // CallSiteId
  UINT16 Callsite_Id () const   { return _c->Get_callsite_id(); }

  // ActualCount
  UINT16 Num_Actuals () const   { return _c->Get_param_count(); }

  void Print (const FILE *f, 
              const IPA_CALL_GRAPH *cg,
              BOOL invert = FALSE ) const;

  void Trace ( const IPA_CALL_GRAPH *cg, BOOL invert = FALSE ) const;

  // Copy an edge; make this a copy constructor
  IPA_EDGE* Copy (MEM_POOL*) 
  {
    DevWarn("IPA_EDGE::Copy is not yet implemented");
    return 0;
  }

  // Feedback support:
  BOOL Has_frequency () const {
    return Summary_Callsite() ?  Summary_Callsite()->Has_callsite_freq() : FALSE;
  }

  FB_FREQ Get_frequency ( void ) {
    return Summary_Callsite() ?  Summary_Callsite()->Get_frequency_count() : FB_FREQ_UNKNOWN;
  }

#ifdef KEY
  void Set_frequency ( FB_FREQ freq ) {
    if (Summary_Callsite()) 
    	Summary_Callsite()->Set_frequency_count (freq) ;
  }
#endif

}; // IPA_EDGE


typedef GRAPH_TEMPLATE<IPA_NODE*, IPA_EDGE*> IPA_GRAPH;
typedef DYN_ARRAY<IPA_NODE*> IPA_CLONE_ARRAY;


class IPA_CALL_GRAPH
{
  typedef SEGMENTED_ARRAY<IPA_NODE> IPA_NODE_ARRAY;
  typedef SEGMENTED_ARRAY<IPA_EDGE> IPA_EDGE_ARRAY;

  typedef HASH_TABLE<IPA_NODE*, IPA_CLONE_ARRAY*> IPA_NODE_TO_IPA_CLONES_MAP;
  typedef HASH_TABLE<IPA_NODE*, IPA_NODE*> IPA_CLONE_TO_IPA_NODE_MAP;
  typedef HASH_TABLE<const IPA_NODE*, IPL_SUMMARY_PTRS*> IPA_NODE_TO_IPL_SUMMARY_MAP;

private:
  MEM_POOL* _pool;
  IPA_GRAPH* _graph;
  IPA_NODE_ARRAY* _nodes;
  IPA_EDGE_ARRAY* _edges;
  IPA_CLONE_TO_IPA_NODE_MAP* _clone_to_orig_node_map;
  IPA_NODE_TO_IPA_CLONES_MAP* _orig_node_to_clones_map;
  IPA_NODE_TO_IPL_SUMMARY_MAP* _preopt_node_to_new_summary_map;

public:
  IPA_CALL_GRAPH (MEM_POOL* pool) 
  {
    _pool = pool;
    _graph = CXX_NEW (IPA_GRAPH(pool), pool);
    _nodes = CXX_NEW (IPA_NODE_ARRAY, pool);
    _edges = CXX_NEW (IPA_EDGE_ARRAY, pool);
    _clone_to_orig_node_map = NULL;
    _orig_node_to_clones_map = NULL;
    _preopt_node_to_new_summary_map = NULL;
  }

  IPA_GRAPH* Graph () const	{ return _graph; }

  UINT Edge_Size () const	{ return _edges->Size (); }
  UINT Node_Size () const	{ return _nodes->Size (); }

  IPA_NODE* Node (IPA_NODE_INDEX idx) { return &(_nodes->Entry (idx)); }
  IPA_EDGE* Edge (IPA_EDGE_INDEX idx) { return &(_edges->Entry (idx)); }

  void Set_Root (NODE_INDEX root) { GRAPH_root(_graph) = root; }
  NODE_INDEX Root () const { return GRAPH_root(_graph); }

  // Create a new node and add it to the call graph
  IPA_NODE* Add_New_Node (ST* st, 
                          INT32 file_index,
                          INT32 proc_info_index,
                          INT32 summary_proc_index)
  {
    // Get IPA_NODE entry from IPA_NODE_ARRAY
    UINT32 index;
    IPA_NODE* node = &(_nodes->New_entry(index));

    // initialize it (call constructor)
    new (node) IPA_NODE (st, 
                         file_index, 
                         proc_info_index,
                         summary_proc_index,
                         _graph->Add_Node(node),
			 index);
    return node;
  }

  // Create a new edge and add it to the call graph
  IPA_EDGE* Add_New_Edge (SUMMARY_CALLSITE* callsite,
                          NODE_INDEX caller_index, 
                          NODE_INDEX callee_index)
  {
    // Get IPA_EDGE entry from IPA_EDGE_ARRAY
    UINT32 index;
    IPA_EDGE* edge = &(_edges->New_entry(index));
    
    // initialize it (call constructor)
    new (edge) IPA_EDGE (callsite, 
                         _graph->Add_Edge (caller_index, callee_index, edge),
			 index);
    return edge;
  }

  // Add an already exisiting edge to the call graph
  // This is used when edges are moved/copied (e.g., when cloning)
  void Add_Edge (IPA_NODE* caller, IPA_NODE* callee, IPA_EDGE* edge)
  {
    EDGE_INDEX edge_index = 
      _graph->Add_Edge (caller->Node_Index(), callee->Node_Index(), edge);
    if (edge) {
      edge->Set_Edge_Index (edge_index);
    }
    caller->Incr_Total_Succ();
  }

  void Delete_Edge(IPA_EDGE *edge)
  {
      _graph->Delete_Edge(edge->Edge_Index());
  }
    
  IPA_NODE* Caller (EDGE_INDEX edge_idx) const
  {
    return _graph->Node_User(EDGE_from(&GRAPH_e_i(_graph, edge_idx)));
  }
  IPA_NODE* Caller (const IPA_EDGE* edge) const 
  {
    return Caller(edge->Edge_Index());
  }

  IPA_NODE* Callee (EDGE_INDEX edge_idx) const
  {
    return _graph->Node_User(EDGE_to(&GRAPH_e_i(_graph, edge_idx)));
  }
  IPA_NODE* Callee (const IPA_EDGE* edge) const 
  {
    return Callee(edge->Edge_Index());
  }

  INT32 Num_Out_Edges (const IPA_NODE* n) const 
  {
    return NODE_fcnt(&GRAPH_v_i(_graph, n->Node_Index()));
  }
  INT32 Num_In_Edges (const IPA_NODE* n) const 
  {
    return NODE_tcnt(&GRAPH_v_i(_graph, n->Node_Index()));
  }

  INT32 Node_Depth (IPA_NODE* node) const
  { 
    return NODE_level(&GRAPH_v_i(_graph, node->Node_Index()));
  }

#ifdef KEY
  void Merge_Nodes (NODE_INDEX, NODE_INDEX);
#endif

  // Number of edges from the caller to the callee
  INT32 Num_Calls (IPA_NODE* caller, IPA_NODE* callee) const;

  // Print all nodes and edges in the call graph
  void Print (FILE*);
  
  // Print all node indices in the specified order
  void Print (FILE*, TRAVERSAL_ORDER);


  void Print_vobose (FILE*);
  void Print_vobose (FILE*, TRAVERSAL_ORDER, BOOL);

  
  // map callsites in the caller to WN nodes
  void Map_Callsites(IPA_NODE* caller);

  // Create a clone of the given node
  IPA_NODE* Create_Clone (IPA_NODE* node, BOOL update_cg = TRUE);

  // Quasi clones are present only as IPA_NODEs with cprop annotations
  // their WHIRL and ST are not cloned
  IPA_NODE* Create_Quasi_Clone (IPA_EDGE* call_edge);

  // Turn a quasi clone into a real one with its own WHIRL and ST
  void Quasi_To_Real_Clone (IPA_NODE* clone);

  // Remove a quasi clone and update edges and annotations
  void Remove_Quasi_Clone (IPA_NODE* clone);

  // Return the original node from which the clone was derived
  IPA_NODE* Clone_Origin (IPA_NODE* clone) const
  {
    Is_True(_clone_to_orig_node_map,
            ("IPA_CALL_GRAPH::Clone_Origin: _clone_to_orig_node_map is NULL"));
    return _clone_to_orig_node_map->Find (clone);
  }
  
  // Return the array of clones derived from the given node
  IPA_CLONE_ARRAY* Clone_Array (IPA_NODE* node) const
  {
    return (_orig_node_to_clones_map ?
            _orig_node_to_clones_map->Find(node) : NULL);
  }

  // rebuild edges for a node cleaned up by preopt
  void Update_Node_After_Preopt (IPA_NODE*,
                                 WN*,
                                 SUMMARY_CALLSITE*,
                                 IPL_SUMMARY_PTRS*);

  // return preopt regenarated summary pointers for the node
  IPL_SUMMARY_PTRS* New_Summary_Ptrs (const IPA_NODE* node) const
  {
    Is_True (_preopt_node_to_new_summary_map,
             ("IPA_CALL_GRAPH::New_Summary_Ptrs: _preopt_node_to_new_summary_map is NULL"));
    Is_True (node->Is_Preoptimized(),
             ("IPA_CALL_GRAPH::New_Summary_Ptrs: node is not preoptimized"));
    return _preopt_node_to_new_summary_map->Find(node);
  }

}; // IPA_CALL_GRAPH


extern IPA_CALL_GRAPH *IPA_Call_Graph;
extern BOOL IPA_Call_Graph_Built;

extern void IPA_Process_File (IP_FILE_HDR& hdr);
extern void Build_Call_Graph ();
#ifdef KEY
extern IPA_CALL_GRAPH *IPA_Graph_Undirected;
extern void IPA_Convert_Icalls( IPA_CALL_GRAPH* );
#endif

//INLINING_TUNING^
extern UINT32 Orig_Prog_WN_Count;
extern UINT32 Prog_WN_Count;
extern UINT32 Total_Dead_Function_WN_Count;
extern FB_FREQ Total_cycle_count_2;
//INLINING_TUNING$

// ====================================================================
//
// Auxiliary information associated with nodes and edges
//
// These are arrays parallel to the IPA_NODE_ARRAY and IPA_EDGE_ARRAY for
// holding extra information that is local to a particular operation.
//
// Note that these arrays shared the same index to IPA_NODE_ARRAY or
// IPA_EDGE_ARRAY, BUT this index is NOT the same as NODE_INDEX or
// EDGE_INDEX. 
//
// ASSUMPTIONS:  Size of IPA_NODE_ARRAY and IPA_EDGE_ARRAY do not change.
//
// ====================================================================

template <class EDGE>
class AUX_IPA_EDGE
{
private:
    EDGE* data;
    MEM_POOL* pool;
    UINT edge_size;

public:

    AUX_IPA_EDGE (const IPA_CALL_GRAPH* cg, MEM_POOL* m = Malloc_Mem_Pool) :
	pool (m), edge_size (cg->Edge_Size()) {
	UINT size = sizeof(EDGE) * edge_size;
	data = (EDGE*) MEM_POOL_Alloc (pool, size);
	BZERO (data, size);
    }

    ~AUX_IPA_EDGE () { MEM_POOL_FREE (pool, data); }

    EDGE& operator[] (const IPA_EDGE* edge) {
	Is_True (edge->Array_Index () < edge_size, ("Subscript out of bound"));
	return data[edge->Array_Index ()];
    }
    const EDGE& operator[] (const IPA_EDGE* edge) const {
	Is_True (edge->Array_Index () < edge_size, ("Subscript out of bound"));
	return data[edge->Array_Index ()];
    }

    EDGE& operator[] (UINT32 n_idx) {
	Is_True (n_idx < edge_size, ("Subscript out of bound"));
	return data[n_idx];
    }
    const EDGE& operator[] (UINT32 n_idx) const {
	Is_True (n_idx < edge_size, ("Subscript out of bound"));
	return data[n_idx];
    }
}; // AUX_IPA_EDGE


template <class NODE>
class AUX_IPA_NODE
{
private:
    NODE* data;
    MEM_POOL* pool;
    UINT node_size;

public:

    AUX_IPA_NODE (const IPA_CALL_GRAPH* cg, MEM_POOL* m = Malloc_Mem_Pool) :
	pool (m), node_size (cg->Node_Size()) {
	UINT size = sizeof(NODE) * node_size;
	data = (NODE*) MEM_POOL_Alloc (pool, size);
	BZERO (data, size);
    }

    ~AUX_IPA_NODE () { MEM_POOL_FREE (pool, data); }

// KEY: Added '|| !_STANDALONE_INLINER' to enable code for IPA in all the 
// following functions
    NODE& operator[] (const IPA_NODE* node) {
#if defined(_LIGHTWEIGHT_INLINER) || !defined(_STANDALONE_INLINER)
        if (node->Array_Index () >= node_size) {
            UINT size = sizeof(NODE) * node_size;
            node_size *= 2;
            data = (NODE*) MEM_POOL_Realloc (pool, data, size, size*2);
	    BZERO (((char *)data)+size, size);
        }
#else // _LIGHTWEIGHT_INLINER || !_STANDALONE_INLINER
	Is_True (node->Array_Index () < node_size, ("Subscript out of bound"));
#endif // _LIGHTWEIGHT_INLINER || !_STANDALONE_INLINER
	return data[node->Array_Index ()];
    }
    const NODE& operator[] (const IPA_NODE* node) const {
#if defined(_LIGHTWEIGHT_INLINER) || !defined(_STANDALONE_INLINER)
        if (node->Array_Index () >= node_size) {
            UINT size = sizeof(NODE) * node_size;
            node_size *= 2;
            data = (NODE*) MEM_POOL_Realloc (pool, data, size, size*2);
	    BZERO (data+size, size);
        }
#else // _LIGHTWEIGHT_INLINER || !_STANDALONE_INLINER
	Is_True (node->Array_Index () < node_size, ("Subscript out of bound"));
#endif // _LIGHTWEIGHT_INLINER || !_STANDALONE_INLINER
	return data[node->Array_Index ()];
    }

    NODE& operator[] (UINT32 n_idx) {
#if defined(_LIGHTWEIGHT_INLINER) || !defined(_STANDALONE_INLINER)
        if (n_idx >= node_size) {
            UINT size = sizeof(NODE) * node_size;
            node_size *= 2;
            data = (NODE*) MEM_POOL_Realloc (pool, data, size, size*2);
	    BZERO (data+size, size);
        }
#else // _LIGHTWEIGHT_INLINER || !_STANDALONE_INLINER
	Is_True (n_idx < node_size, ("Subscript out of bound"));
#endif // _LIGHTWEIGHT_INLINER || !_STANDALONE_INLINER
	return data[n_idx];
    }
    const NODE& operator[] (UINT32 n_idx) const {
#if defined(_LIGHTWEIGHT_INLINER) || !defined(_STANDALONE_INLINER)
        if (n_idx >= node_size) {
            UINT size = sizeof(NODE) * node_size;
            node_size *= 2;
            data = (NODE*) MEM_POOL_Realloc (pool, data, size, size*2);
	    BZERO (data+size, size);
	}
#else // _LIGHTWEIGHT_INLINER || !_STANDALONE_INLINER
	Is_True (n_idx < node_size, ("Subscript out of bound"));
#endif // _LIGHTWEIGHT_INLINER || !_STANDALONE_INLINER
	return data[n_idx];
    }
}; // AUX_IPA_NODE


// ====================================================================
//
// Miscellaneous queries
//
// ====================================================================

extern UINT32 Total_Dead_Function_Weight;
extern UINT32 Orig_Prog_Weight;

extern FB_FREQ Total_call_freq;
extern FB_FREQ Total_cycle_count;

extern INT Total_Must_Inlined;
extern INT Total_Must_Not_Inlined;


// Given a pu, get its corresponding call graph node.
IPA_NODE* Get_Node_From_PU(PU_Info*);


// ======================================================================
//
// Call graph iterators:  these are actually adaptor functions for the
// graph package in ipa/common/ip_graph.h.
//
// ======================================================================

/*---------------------------------------------------------------*/
/* iterator class that iterates over the nodes and edges of the  */
/* call graph in a specific ordering. Currently, DF POSTORDER    */
/* is implemented. Note, the iterator must be used only if       */
/* an ordering has already been implemented                      */
/*---------------------------------------------------------------*/
class IPA_NODE_ITER 
{
private:
  IPA_GRAPH*        _graph;
  TRAVERSAL_ORDER   _order;
  ORDERED_NODE_ITER _node_iter;

public:
  // constructors
  IPA_NODE_ITER (TRAVERSAL_ORDER order, MEM_POOL* pool) :
    _graph     (IPA_Call_Graph->Graph()),
    _order     (order),
    _node_iter (_graph, _order, pool)
  {}

  IPA_NODE_ITER (IPA_CALL_GRAPH* cg, 
                 TRAVERSAL_ORDER order, 
                 MEM_POOL* pool = Malloc_Mem_Pool) :
    _graph     (cg->Graph()),
    _order     (order),
    _node_iter (_graph, _order, pool)
  {}

  void First () { _node_iter.Reset (); }
  void Next ()  { ++_node_iter; }

  BOOL Is_Empty () const { return _node_iter.Is_Empty (); }
    
  IPA_NODE* Current (void) const 
  {
    return _graph->Node_User (_node_iter.Current ());
  }

  void Print (FILE *fp) { _node_iter.Print (fp); }

}; // IPA_NODE_ITER


/*---------------------------------------------------------------*/
/* iterator class that iterates over the from edges              */
/*---------------------------------------------------------------*/
class IPA_SUCC_ITER 
{
private:
  IPA_GRAPH* _graph;
  NODE_INDEX _node_idx;
  NODE_ITER  _node_iter;

public:

  IPA_SUCC_ITER (NODE_INDEX n) : 
    _graph     (IPA_Call_Graph->Graph()),
    _node_idx  (INVALID_NODE_INDEX),
    _node_iter (_graph, n)
  {}
    
  IPA_SUCC_ITER (const IPA_NODE* n) : 
    _graph     (IPA_Call_Graph->Graph()),
    _node_idx  (INVALID_NODE_INDEX),
    _node_iter (_graph, n->Node_Index())
  {}

  IPA_SUCC_ITER (IPA_CALL_GRAPH* cg, IPA_NODE* n) : 
    _graph     (cg->Graph()),
    _node_idx  (INVALID_NODE_INDEX),
    _node_iter (_graph, n->Node_Index())
  {}

  void First () { _node_idx = _node_iter.First_Succ(); }
  void Next ()  { _node_idx = _node_iter.Next_Succ();  }
    
  BOOL Is_Empty () const { return (_node_idx == INVALID_NODE_INDEX); }
    
  EDGE_INDEX Current_Edge_Index () const 
  { 
    return _node_iter.Current_Edge_Index(); 
  }

  IPA_EDGE* Current_Edge () const 
  { 
    return _graph->Edge_User(Current_Edge_Index()); 
  }

  void Set_Current_Edge (IPA_EDGE *e) 
  { 
    _graph->Set_Edge_User(Current_Edge_Index(), e);
  }

};


/*---------------------------------------------------------------*/
/* iterator class that iterates over the from edges              */
/*---------------------------------------------------------------*/
class IPA_PRED_ITER 
{
  IPA_GRAPH* _graph;
  NODE_INDEX _node_idx;
  NODE_ITER  _node_iter;

public:
  // constructors

  IPA_PRED_ITER (NODE_INDEX n) : 
    _graph     (IPA_Call_Graph->Graph()), 
    _node_idx  (INVALID_NODE_INDEX),
    _node_iter (_graph, n)
  {}
    
  IPA_PRED_ITER (const IPA_NODE* n) : 
    _graph     (IPA_Call_Graph->Graph()), 
    _node_idx  (INVALID_NODE_INDEX),
    _node_iter (_graph, n->Node_Index())
  {}

  IPA_PRED_ITER (IPA_CALL_GRAPH* cg, IPA_NODE* n) : 
    _graph     (cg->Graph()), 
    _node_idx  (INVALID_NODE_INDEX),
    _node_iter (_graph, n->Node_Index())
  {}

  void First () { _node_idx = _node_iter.First_Pred(); }
  void Next ()  { _node_idx = _node_iter.Next_Pred();  }
    
  BOOL Is_Empty () const { return (_node_idx == INVALID_NODE_INDEX); }
    
  EDGE_INDEX Current_Edge_Index () const 
  { 
    return _node_iter.Current_Edge_Index(); 
  }

  IPA_EDGE* Current_Edge () const 
  {	
    return _graph->Edge_User(Current_Edge_Index()); 
  }

  void Set_Current_Edge (IPA_EDGE *e) 
  { 
    _graph->Set_Edge_User(Current_Edge_Index(), e);
  }

};

extern UINT32 Eliminate_Dead_Func (BOOL update_modref_count = TRUE);
extern IPA_NODE* Main_Entry (IPA_NODE* ipan_alt);
extern void IPA_update_summary_st_idx (const IP_FILE_HDR& hdr);
extern char* IPA_Node_Name(IPA_NODE* node);

#if defined(KEY) && !defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER)
extern void Mark_PUs_With_File_Id (PU_Info *, UINT);
extern BOOL Opt_Options_Inconsistent;
#endif // KEY && !_STANDALONE_INLINER && !_LIGHTWEIGHT_INLINER

#ifdef _LIGHTWEIGHT_INLINER
extern BOOL Is_Node_Inlinable_In_Call_Graph(ST_IDX idx);

extern IPA_NODE* Add_One_Node (IP_FILE_HDR& s, INT32 file_idx, INT i, NODE_INDEX& index);
extern void Add_Edges_For_Node (IP_FILE_HDR& s, INT i, SUMMARY_PROCEDURE* proc_array, SUMMARY_SYMBOL* symbol_array);
extern BOOL Pred_Is_Root(const IPA_NODE* node);
#endif // _LIGHTWEIGHT_INLINER

#endif // cxx_ipa_cg_INCLUDED
