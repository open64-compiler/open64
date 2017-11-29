/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/* -*- c++ -*-
 *
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
 * Module: ipl_summarize.h
 *
 * Description:
 *	Generation of summary information.
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef ipl_summarize_INCLUDED
#define ipl_summarize_INCLUDED

#ifndef cxx_template_INCLUDED
#include "cxx_template.h"
#endif // cxx_template_INCLUDED

#ifndef cxx_hash_INCLUDED
#include "cxx_hash.h"
#endif // cxx_hash_INCLUDED

#ifndef wn_util_INCLUDED
#include "wn_util.h"                    // for WN, iterators,
#endif // wn_util_INCLUDED

#ifndef wn_map_INCLUDED
#include "wn_map.h"			// for WN mappings
#endif // wn_map_INCLUDED

#ifndef lwn_util_INCLUDED
#include "lwn_util.h"                   // for LWN_Parentize
#endif // lwn_util_INCLUDED

#ifndef config_ipa_INCLUDED
#include "config_ipa.h"			// for INLINE_Enable_Copy_Prop
#endif // config_ipa_INCLUDED

#ifndef opt_du_INCLUDED
#include "opt_du.h"                     // for DU-manager
#endif // opt_du__INCLUDED

#ifndef opt_alias_interface_INCLUDED
#include "opt_alias_interface.h"        // for alias manager
#endif // opt_alias_interface_INCLUDED

#ifndef opt_ssa_INCLUDED
#include "opt_ssa.h"			// for PHI_NODE
#endif // opt_ssa_INCLUDED

#ifndef ipl_summary_INCLUDED
#include "ipl_summary.h"                // summary info data structures
#endif // ipl_summary_INCLUDED

#ifndef CXX_MEMORY_INCLUDED
#include "cxx_memory.h" //CXX_DELETE()  for reorder
#endif

#ifndef ipl_reorder_INCLUDED // for reorder_ipl_pool
#include "ipl_reorder.h"
#endif

// Constraint graph summary for Nystrom Alias Analyzer
#include "ipa_be_summary.h"
#include "constraint_graph.h"

//---------------------------------------------------------------
// alternate entry point array
// contain all the alternate entry points encountered in the
// subroutine
//---------------------------------------------------------------
class ALT_ENTRY
{

private:
  WN* _w;
  INT _position;
  INT _formal_count;

public:
  WN* Get_wn() const		{ return _w;};
  void Set_wn(WN* w)		{ _w = w;};
  
  INT Get_position() const	{ return _position;};
  void Set_position(INT p)	{ _position = p;};

  INT Get_formal_count() const	{ return _formal_count;};
  void Set_formal_count(INT f)	{ _formal_count = f;};

  void Init()			{ BZERO (this, sizeof(ALT_ENTRY)); }
}; // ALT_ENTRY


/* buffer up all inline-related pragmas -- use later to update the callsite
   node inline attributes */
class INLINE_ATTR {

#define IPL_FILE_INLINE		0x1
#define IPL_NO_FILE_INLINE	0x2
#define IPL_PU_INLINE		0x4
#define IPL_NO_PU_INLINE	0x8

private:

    char *name;				// function named by the pragma
    INT inline_attr;			// inline attributes

public:

    /* access functions */

    void Set_name (char *str)	{ name = str; }
    char *Get_name () const	{ return name; }

    void Set_file_inline()	{ inline_attr |= IPL_FILE_INLINE;};
    BOOL Is_file_inline() const { return inline_attr & IPL_FILE_INLINE;};
    
    void Set_no_file_inline()	{ inline_attr |= IPL_NO_FILE_INLINE;};
    BOOL Is_no_file_inline() const { return inline_attr & IPL_NO_FILE_INLINE;};

    void Set_pu_inline()	{ inline_attr |= IPL_PU_INLINE;};
    BOOL Is_pu_inline() const	{ return inline_attr & IPL_PU_INLINE;};

    void Set_no_pu_inline()	{ inline_attr |= IPL_NO_PU_INLINE;};
    BOOL Is_no_pu_inline() const { return inline_attr & IPL_NO_PU_INLINE;};

}; // INLINE_ATTR



typedef HASH_TABLE<const ST *, INT> GLOBAL_HASH_TABLE;
typedef HASH_TABLE<INT, ST *> COMMON_HASH_TABLE;

class EMITTER;
class SUMMARY_CHECK_POINT;


/* describe what kind of summary structure is needed for a given WHIRL node */
class SUMMARY_DESC
{
private:
    
    WN *_w;
    union {
	PHI_NODE *_phi;
	CODEREP *_chi_cr;
    } u;
    IPA_CONST_TYPE _type;
    BOOL _addr_of;			// see description in SUMMARY_VALUE
    BOOL _convertible_to_global;	// see description in SUMMARY_VALUE
    TYPE_ID _target_mtype : 8;		// see description in SUMMARY_VALUE

public:

    SUMMARY_DESC () {
	BZERO (this, sizeof(SUMMARY_DESC));
    }

    void Set_wn (WN *w)			{ _w = w; }
    WN *Get_wn () const			{ return _w; }

    void Set_type (IPA_CONST_TYPE t)	{ _type = t;}
    IPA_CONST_TYPE Get_type () const	{ return _type; }

    void Set_phi (PHI_NODE *p)		{ u._phi = p; }
    PHI_NODE *Get_phi () const		{ return u._phi; };

    void Set_chi_cr (CODEREP *cr)	{ u._chi_cr = cr; }
    CODEREP *Get_chi_cr () const	{ return u._chi_cr; };

    void Set_is_addr_of ()		{ _addr_of = TRUE; }
    void Reset_is_addr_of ()		{ _addr_of = FALSE; }
    BOOL Is_addr_of () const		{ return _addr_of; }
	
    void Set_convertible_to_global ()	{ _convertible_to_global = TRUE; }
    void Reset_convertible_to_global ()	{ _convertible_to_global = FALSE; }
    BOOL Is_convertible_to_global () const { return _convertible_to_global; }
	
    void Set_target_mtype (TYPE_ID m)	{ _target_mtype = m; }
    TYPE_ID Target_mtype () const      	{ return _target_mtype; }
	
}; // SUMMARY_DESC


enum PROGRAM { IPL, INLINER };

// ======================================================================
// The next two classes are ONLY for inlining purpose
// ======================================================================

// class SUMMARY_CREF_SYMBOL used ONLY by inliner to keep cref count
// ======================================================================

class SUMMARY_CREF_SYMBOL {
private:
  INT32 _symbol_index;// mirrors the SUMMARY_SYMBOL's index
  INT  _cur_cref_count;
public:
  void Set_symbol_index(INT32 idx)	{ _symbol_index = idx;};
  INT32 Get_symbol_index() const	{ return _symbol_index;};
  void Reset_cur_cref_count()	{ _cur_cref_count = 0;};
  void Incr_cur_cref_count()	{ _cur_cref_count++;};
  void Decr_cur_cref_count()	{ _cur_cref_count--;};
  INT Get_cur_cref_count() const { return _cur_cref_count;}
}; // SUMMARY_CREF_SYMBOL

// ======================================================================
// class SUMMARY_PROC_INFO used ONLY by the inliner to keep 
// remaining-call-count,  and ISTORE/MSTORE/STID/ info
// to enable substitution of actuals for formals when actual is LDID
// ======================================================================

class SUMMARY_PROC_INFO {
private:
    mINT32  _symbol_index;// mirrors the SUMMARY_PROCEDURE's index
    INT32   _call_count; // current # of calls in the procedure
    mUINT32 _state; 
// bit fields for _state
#define PROC_HAS_ISTORE                0x001 // There is an ISTORE in the body
public:
    void Set_symbol_index (INT32 s)	{ _symbol_index = s; }
    INT32 Get_symbol_index () const	{ return _symbol_index; }
    UINT32 Get_state () const		{ return _state; }
    void Set_has_istore()               { _state |= PROC_HAS_ISTORE;}
    BOOL Has_istore() const             { return (_state & PROC_HAS_ISTORE);}
    void Incr_call_count ()		{ ++_call_count; }
    void Decr_call_count ()		{ --_call_count; }
    BOOL Is_Leaf_now () const           { return (_call_count == 0);}
    void Init (void) {
      BZERO (this, sizeof(SUMMARY_PROC_INFO));
      _call_count = 0;
    }

}; // SUMMARY_PROC_INFO

extern void
Init_Aux_Symbol_Info (SYMTAB_IDX level);

// ======================================================================

// Template for class SUMMARIZE:
//
// Shared by ipl.so and standalone inliner, instantiated by the enumeration
// type PROGRAM.
//
// This class contains all the functions for creating summary information.
template <PROGRAM program>
class SUMMARIZE
{

private:

    MEM_POOL *mem;

    WN *entry_point;			// original entry point, for alt
					// entry point processing.
                                        // used by inliner to keep proc

    struct DU_MANAGER *du_mgr;
    struct ALIAS_MANAGER *alias_mgr;
    EMITTER *emitter;

    DYN_ARRAY<SUMMARY_PROCEDURE> _procedure;
    DYN_ARRAY<SUMMARY_CALLSITE> _callsite;
    DYN_ARRAY<SUMMARY_STMT> _stmt;
    DYN_ARRAY<SUMMARY_CONTROL_DEPENDENCE> _ctrl_dep;
    DYN_ARRAY<SUMMARY_FORMAL> _formal;
    DYN_ARRAY<SUMMARY_ACTUAL> _actual;
    DYN_ARRAY<SUMMARY_VALUE> _value;
    DYN_ARRAY<SUMMARY_EXPR> _expr;
    DYN_ARRAY<SUMMARY_PHI> _phi;
    DYN_ARRAY<SUMMARY_CHI> _chi;
    DYN_ARRAY<SUMMARY_SYMBOL> _symbol;
    // _proc_info and _crefcount used ONLY by inliner
    DYN_ARRAY<SUMMARY_CREF_SYMBOL> _symbol_crefcount; 
    DYN_ARRAY<SUMMARY_PROC_INFO> _proc_info; 
    DYN_ARRAY<SUMMARY_GLOBAL> _global;
    DYN_ARRAY<SUMMARY_FEEDBACK> _feedback;
    DYN_ARRAY<SUMMARY_COMMON> _common;
    DYN_ARRAY<SUMMARY_COMMON_SHAPE> _common_shape;
    DYN_ARRAY<SUMMARY_STID> _global_stid;
    DYN_ARRAY<TCON *> _tcon;
    DYN_ARRAY<ALT_ENTRY>  _alt_entry;
    DYN_ARRAY<INLINE_ATTR> _inline_attr;
    DYN_ARRAY<SUMMARY_STRUCT_ACCESS> _struct_access;//reordering
    
#ifdef KEY
    DYN_ARRAY<SUMMARY_TY_INFO> _ty_info;
#endif

    // Constraint graph specific data for Nystrom Alias Analuzer
    DYN_ARRAY<SUMMARY_CONSTRAINT_GRAPH_NODE> _constraint_graph_nodes;
    DYN_ARRAY<SUMMARY_CONSTRAINT_GRAPH_EDGE> _constraint_graph_edges;
    DYN_ARRAY<SUMMARY_CONSTRAINT_GRAPH_STINFO> _constraint_graph_stinfos;
    DYN_ARRAY<SUMMARY_CONSTRAINT_GRAPH_CALLSITE> _constraint_graph_callsites;
    // The above summary information may contain a variable list of
    // CGnode ids. They are stored in a separate array with the start index
    // and count stored in the corresponding SUMMARY_CONSTRAINT_GRAPH_*
    DYN_ARRAY<UINT32> _constraint_graph_node_ids;
    DYN_ARRAY<SUMMARY_CONSTRAINT_GRAPH_MODRANGE> _constraint_graph_modranges;

    BOOL Trace_Modref;			// trace mod/ref analysis

    /* used as cache to keep track of which global symbols have been
       entered into the SUMMARY_SYMBOL array */ 
    DYN_ARRAY<INT> *Global_index;

    /* For each procedure, record if a SUMMARY_GLOBAL entry is created, and
       if so, the index corresponds to that entry into the SUMMARY_GLOBAL
       array */ 
    GLOBAL_HASH_TABLE *Global_hash_table;

    //for reordering
    typedef hash_map<mUINT32, mUINT32> TY_TO_ACCESS_MAP;
    TY_TO_ACCESS_MAP *Ty_to_access_map;// mapping ty_index to SUMMARY_STRUCT_ACCESS

#ifdef KEY
    typedef HASH_TABLE<UINT32, INT> TY_INFO_HASH_TABLE;
    TY_INFO_HASH_TABLE * Ty_info_hash_table;
#endif

    typedef STACK<UINT64> LOOP_COUNT_STACK;
	LOOP_COUNT_STACK *loop_count_stack;
    INT first_struct_access_of_PU, last_struct_access_of_PU;
    #define max_hot_loops 5 //just consider partial loops, how to find it?

    BOOL File_Pragmas;			// has file based pragmas

    /* private access functions */
    void Set_entry_point (WN *w)	{ entry_point = w; }

    /* private operations */

    SUMMARY_PROCEDURE *New_procedure () {
	INT new_idx = _procedure.Newidx ();
	_procedure[new_idx].Init ();
	return &(_procedure[new_idx]);
    }

    SUMMARY_PROC_INFO *New_proc_info () {
	INT new_idx = _proc_info.Newidx ();
	_proc_info[new_idx].Init ();
	return &(_proc_info[new_idx]);
    }

    SUMMARY_CALLSITE *New_callsite () {
	INT new_idx = _callsite.Newidx ();
	_callsite[new_idx].Init ();
	return &(_callsite[new_idx]);
    }
	    
    SUMMARY_STMT *New_stmt () {
	INT new_idx = _stmt.Newidx ();
	_stmt[new_idx].Init ();
	return &(_stmt[new_idx]);
    }
	    
    SUMMARY_CONTROL_DEPENDENCE *New_ctrl_dep () {
	INT new_idx = _ctrl_dep.Newidx ();
	_ctrl_dep[new_idx].Init ();
	return &(_ctrl_dep[new_idx]);
    }
	    
    SUMMARY_ACTUAL *New_actual () {
	INT new_idx = _actual.Newidx ();
	_actual[new_idx].Init ();
	return &(_actual[new_idx]);
    }
	    
    SUMMARY_FORMAL *New_formal () {
	INT new_idx = _formal.Newidx ();
	return &(_formal[new_idx]);
    }
	    
    SUMMARY_VALUE *New_value () {
	INT new_idx = _value.Newidx ();
	_value[new_idx].Init ();
	return &(_value[new_idx]);
    }
	    
    SUMMARY_EXPR *New_expr () {
	INT new_idx = _expr.Newidx ();
	_expr[new_idx].Init ();
	return &(_expr[new_idx]);
    }

    SUMMARY_PHI *New_phi () {
	INT new_idx = _phi.Newidx ();
	_phi[new_idx].Init ();
	return &(_phi[new_idx]);
    }

    SUMMARY_CHI *New_chi () {
	INT new_idx = _chi.Newidx ();
	_chi[new_idx].Init ();
	return &(_chi[new_idx]);
    }

    SUMMARY_SYMBOL *New_symbol () {
	INT new_idx = _symbol.Newidx ();
	_symbol[new_idx].Init ();
	return &(_symbol[new_idx]);
    }

    SUMMARY_CREF_SYMBOL *New_symbol_crefcount () {
	INT new_idx = _symbol_crefcount.Newidx ();
	_symbol_crefcount[new_idx].Reset_cur_cref_count(); 
	// initialize ref count to 0
	return &(_symbol_crefcount[new_idx]);
    }

    SUMMARY_GLOBAL *New_global () {
	INT new_idx = _global.Newidx ();
	_global[new_idx].Init ();
	return &(_global[new_idx]);
    }

    SUMMARY_FEEDBACK *New_feedback () {
	INT new_idx = _feedback.Newidx ();
	_feedback[new_idx].Init ();
	return &(_feedback[new_idx]);
    }

    SUMMARY_COMMON *New_common () {
	INT new_idx = _common.Newidx ();
	_common[new_idx].Init ();
	return &(_common[new_idx]);
    }

    SUMMARY_COMMON_SHAPE *New_common_shape () {
	INT new_idx = _common_shape.Newidx ();
	_common_shape[new_idx].Init ();
	return &(_common_shape[new_idx]);
    }

    SUMMARY_STID *New_global_stid () {
	INT new_idx = _global_stid.Newidx ();
	_global_stid[new_idx].Init ();
	return &(_global_stid[new_idx]);
    }

    TCON **New_tcon () {
	INT new_idx = _tcon.Newidx ();
	return &(_tcon[new_idx]);
    }
	    
    ALT_ENTRY *New_alt_entry () {
	INT new_idx = _alt_entry.Newidx ();
	_alt_entry[new_idx].Init ();
	return &(_alt_entry[new_idx]);
    }

    INLINE_ATTR *New_inline_attr () {
	INT new_idx = _inline_attr.Newidx ();
	return &(_inline_attr[new_idx]);
    }
    mUINT32 New_struct_access(mUINT32 ty_index, mUINT32 flatten_flds){
	mUINT32 new_idx = _struct_access.Newidx ();
	_struct_access[new_idx].Init (ty_index,flatten_flds,&reorder_ipl_pool);
	return new_idx;
    }

#ifdef KEY
    SUMMARY_TY_INFO *New_ty_info () {
	INT new_idx = _ty_info.Newidx ();
	_ty_info[new_idx].Init ();
	return &(_ty_info[new_idx]);
    }
#endif

    // Constraint graph specific data for Nystrom Alias Analyzer

    SUMMARY_CONSTRAINT_GRAPH_NODE *New_constraint_graph_node () 
    {
      INT new_idx = _constraint_graph_nodes.Newidx();
      _constraint_graph_nodes[new_idx].Init();
      return &(_constraint_graph_nodes[new_idx]);
    }

    SUMMARY_CONSTRAINT_GRAPH_EDGE *New_constraint_graph_edge () 
    {
      INT new_idx = _constraint_graph_edges.Newidx();
      _constraint_graph_edges[new_idx].Init();
      return &(_constraint_graph_edges[new_idx]);
    }

    SUMMARY_CONSTRAINT_GRAPH_STINFO *New_constraint_graph_stinfo () 
    {
      INT new_idx = _constraint_graph_stinfos.Newidx();
      _constraint_graph_stinfos[new_idx].Init();
      return &(_constraint_graph_stinfos[new_idx]);
    }

    SUMMARY_CONSTRAINT_GRAPH_CALLSITE *New_constraint_graph_callsite () 
    {
      INT new_idx = _constraint_graph_callsites.Newidx();
      _constraint_graph_callsites[new_idx].Init();
      return &(_constraint_graph_callsites[new_idx]);
    }

    SUMMARY_CONSTRAINT_GRAPH_MODRANGE *New_constraint_graph_modrange () 
    {
      INT new_idx = _constraint_graph_modranges.Newidx();
      _constraint_graph_modranges[new_idx].Init();
      return &(_constraint_graph_modranges[new_idx]);
    }

    void Process_alt_procedure (WN *w, INT formal_index, INT formal_count);
    void Process_callsite (WN *w, INT id, INT loopnest, float =-1);
#if defined(KEY) && !defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER)
    void Process_icall (SUMMARY_PROCEDURE *, WN *, INT, float);
    SUMMARY_CALLSITE * Create_dummy_callsite(SUMMARY_PROCEDURE *, WN *, INT, float,
                                             ST *, UINT64, UINT64);
#endif
/*
    this function is added to enable IPA to apply
    virtual function optimization. Look in 
    osprey/ipa/local/ipl_summarize_template.h for
    more information.
*/
    
    void Process_virtual_function (SUMMARY_PROCEDURE * , 
        WN * , INT , float );
    void Process_formal (WN *w, INT num_formals, SUMMARY_PROCEDURE *proc);
    void Process_formal_alt (WN *w, INT kid_count);
    void Process_actual (WN *actual);
    void Process_alt_entry (WN *w);
    void Process_IO(WN* w);
    void Set_IO(WN *w, BOOL is_read);
    void Process_inline_attr (WN *pragma_node);
    void Process_commons_in_whirl(WN* w, SUMMARY_PROCEDURE *proc);

    inline void Restore_from_check_point (const SUMMARY_CHECK_POINT *cp);
    void Process_constant_jump_function (WN *w, SUMMARY_VALUE *value);
    void Process_phi_operand (INT phi_index, WN *orig_wn, CODEREP *cr, INT kid);
    INT Process_phi_jump_function (WN *orig_wn, PHI_NODE *phi);
    INT Process_chi_jump_function (WN *wn, const SUMMARY_DESC &desc);
    void Process_operand (WN *w, INT kid, INT expr_idx);
    INT Process_polynomial_jump_function (WN *w);
    BOOL Mismatched_load_store (CODEREP *cr, BOOL is_ptr_var, ST *st, INT
				load_offset, TYPE_ID load_type);
    void Classify_indirect (SUMMARY_DESC &result, WN *w);
    void Classify_const_value (SUMMARY_DESC &result, WN *w);
    void Classify_const_value (SUMMARY_DESC &result, WN *w, CODEREP *cr);
    INT Process_jump_function (SUMMARY_DESC *desc);
    void Process_jump_function (WN *w, INT value_idx);

    void Record_global_ref (WN *w, ST *s, OPERATOR op, BOOL refcount_only);
    void Record_ref_formal (WN *);
    void Record_ref_all_formal (WN *w, BOOL parm_store = FALSE);
    void Check_kid_ref (WN* w);
    void Record_ref (WN *w);
    
    void Record_global_dmod (const WN *w, const WN *rhs, const ST *st);
    void Record_mod_formal (WN *w);
    void Record_mod_common (WN *w, const ST *st);
    void Record_mod (WN *w);
    
    INT Process_cd_for_phi_node (IDTYPE cd_bb_idx);
    BOOL Process_control_dependence (WN *w, INT node_index);
    void Copy_summary_ctrl_dep (SUMMARY_CONTROL_DEPENDENCE *cd);
    inline void Generate_summary_control_dependence (void);
    
    void Update_Addr_Passed_Count (WN* opr_parm);
    void Set_local_addr_taken_attrib ();

    void Process_pragma_node (WN* w);
    void Process_pragmas (WN *w);
    void Update_call_pragmas (SUMMARY_CALLSITE *callsite);
    void Start_PU_process_struct_access(){//del later::debug
    	first_struct_access_of_PU=Get_struct_access_idx()+1;
    }

    void Record_struct_access(WN *w, mUINT64 loop_count);//reorder
    UINT Finish_PU_process_struct_access(){// del later ::debug
    	last_struct_access_of_PU=Get_struct_access_idx()+1;
    	INT num_ele;
    	num_ele=last_struct_access_of_PU-first_struct_access_of_PU;
    	return num_ele;
    };

#ifdef KEY
    void Record_ty_info_for_type (TY_IDX ty, TY_FLAGS flags);
#endif

    // Constraint graph specific data for Nystrom Alias Analyzer
    void generateConstraintGraphSummary(WN *w);
    void processPointsToSet(SUMMARY_CONSTRAINT_GRAPH_NODE *sumCGNode,
                            const PointsTo &gbl,
                            const PointsTo &hz,
                            const PointsTo &dn,
                            mUINT32 &numNodeIds);
    UINT32 processModRange(ModulusRange *mr);

    // Functions needed for execution cost analysis
    INT IPL_GEN_Value(WN* wn_value, DYN_ARRAY<SUMMARY_VALUE>* sv,
      DYN_ARRAY<SUMMARY_EXPR>* sx);
    INT IPL_GEN_Expr(OPERATOR opr, INT exp_one, INT exp_two, 
      DYN_ARRAY<SUMMARY_EXPR>* sx);
    INT IPL_GEN_Const(INT value, DYN_ARRAY<SUMMARY_VALUE>* sv,
      DYN_ARRAY<SUMMARY_EXPR>* sx);
    BOOL Easy_Trip_Count(WN* wn_loop, WN** wn_addr_ub, WN** wn_addr_lb, 
      INT* addr_intconst);
    INT IPL_EX_Expr(WN* wn_expr, DYN_ARRAY<SUMMARY_VALUE>* sv, 
      DYN_ARRAY<SUMMARY_EXPR>* sx);
    INT IPL_EX_Trip_Count(WN* wn_loop, DYN_ARRAY<SUMMARY_VALUE>* sv,
      DYN_ARRAY<SUMMARY_EXPR>* sx, BOOL constant_estimate);
    INT IPL_EX_Call(WN* wn_call, DYN_ARRAY<SUMMARY_VALUE>* sv,
      DYN_ARRAY<SUMMARY_EXPR>* sx);
    INT IPL_EX_Statement(WN* wn_statement, DYN_ARRAY<SUMMARY_VALUE>* sv, 
      DYN_ARRAY<SUMMARY_EXPR>* sx, BOOL constant_estimate);
    INT IPL_EX_Block(WN* wn_block, DYN_ARRAY<SUMMARY_VALUE>* sv, 
      DYN_ARRAY<SUMMARY_EXPR>* sx, BOOL constant_estimate);
    void IPL_Execution_Cost(WN* wn_func, SUMMARY_PROCEDURE* sp, 
      MEM_POOL* mem_pool, BOOL constant_estimate);
#ifdef KEY
    void Process_eh_globals (void);
    void Process_eh_region (WN *);
#endif

    // Helper function of Process_procedure() 
    void Identify_switch_clause_labels (WN* node, INT& default_lab_num, 
      BS* &case_labels, MEM_POOL* mem);
    void Collect_calls_in_switch (WN* first_stmt, INT default_lab_num, 
      BS* case_label, BS* &calls_in_switch, MEM_POOL* mp);

public:

    /* public access functions */

    WN *Get_entry_point () const { return entry_point; }

    BOOL Has_global_symbol_index(const ST* st);

    void Set_mem_pool (MEM_POOL *m)	{ mem = m; }
    MEM_POOL *Get_mem_pool () const	{ return mem; }

    void Set_du_mgr (struct DU_MANAGER *du)	{ du_mgr = du; }
    struct DU_MANAGER *Get_du_mgr () const	{ return du_mgr; } 

    void Set_alias_mgr (struct ALIAS_MANAGER *alias)	{ alias_mgr = alias; }
    struct ALIAS_MANAGER *Get_alias_mgr () const	{ return alias_mgr; } 

    void Set_emitter (EMITTER *e)		{ emitter = e; }
    EMITTER *Get_emitter () const		{ return emitter; }

    SUMMARY_PROCEDURE *Get_procedure (INT id) const { return &(_procedure[id]); }
    SUMMARY_PROC_INFO *Get_proc_info (INT id) const { return &(_proc_info[id]); }
    SUMMARY_CALLSITE *Get_callsite (INT id) const { return &(_callsite[id]); }
    SUMMARY_STMT *Get_stmt (INT id) const	{ return &(_stmt[id]); }
    SUMMARY_CONTROL_DEPENDENCE *Get_ctrl_dep (INT id) const {
	return &(_ctrl_dep[id]);
    }
    SUMMARY_FORMAL *Get_formal (INT idx) const	{ return &(_formal[idx]); }
    SUMMARY_ACTUAL *Get_actual (INT idx) const	{ return &(_actual[idx]); }
    SUMMARY_VALUE *Get_value (INT idx) const	{ return &(_value[idx]); }
    SUMMARY_EXPR *Get_expr (INT idx) const	{ return &(_expr[idx]); }
    SUMMARY_PHI *Get_phi (INT idx) const	{ return &(_phi[idx]); }
    SUMMARY_CHI *Get_chi (INT idx) const	{ return &(_chi[idx]); }
    SUMMARY_SYMBOL *Get_symbol (INT idx) const	{ return &(_symbol[idx]); }
    SUMMARY_CREF_SYMBOL *Get_symbol_crefcount (INT idx) const	{ return &(_symbol_crefcount[idx]);}
    SUMMARY_GLOBAL *Get_global (INT idx) const	{ return &(_global[idx]); }
    SUMMARY_FEEDBACK *Get_feedback (INT idx) const { return &(_feedback[idx]);}
    SUMMARY_COMMON *Get_common (INT idx) const	{ return &(_common[idx]); }
    SUMMARY_COMMON_SHAPE *Get_common_shape (INT idx) const {
	return &(_common_shape[idx]);
    }
    SUMMARY_STID *Get_global_stid (INT idx) const {
	return &(_global_stid[idx]);
    }
    TCON **Get_tcon (INT idx) const		{ return &(_tcon[idx]); }
    ALT_ENTRY *Get_alt_entry (INT idx) const	{ return &(_alt_entry[idx]); }
    INLINE_ATTR *Get_inline_attr (INT idx) const { return &(_inline_attr[idx]); }
    SUMMARY_STRUCT_ACCESS * Get_struct_access(INT idx)const{return &(_struct_access[idx]);}
    
#ifdef KEY
    SUMMARY_TY_INFO *Get_ty_info (INT idx) const      { return &(_ty_info[idx]); }
#endif
    // Constraint graph summary for Nystrom Alias Analyzer
    SUMMARY_CONSTRAINT_GRAPH_NODE *Get_constraint_graph_node(INT idx) const 
    {
      return &(_constraint_graph_nodes[idx]);
    }
    SUMMARY_CONSTRAINT_GRAPH_EDGE *Get_constraint_graph_edge(INT idx) const 
    {
      return &(_constraint_graph_edges[idx]);
    }
    SUMMARY_CONSTRAINT_GRAPH_STINFO *Get_constraint_graph_stinfo(INT idx) const 
    {
      return &(_constraint_graph_stinfos[idx]);
    }
    SUMMARY_CONSTRAINT_GRAPH_CALLSITE *
    Get_constraint_graph_callsite(INT idx) const 
    {
      return &(_constraint_graph_callsites[idx]);
    }
    UINT32 *Get_constraint_graph_node_id(INT idx) const 
    {
      return &(_constraint_graph_node_ids[idx]);
    }
    SUMMARY_CONSTRAINT_GRAPH_MODRANGE *
    Get_constraint_graph_modrange(INT idx) const 
    {
      return &(_constraint_graph_modranges[idx]);
    }
    
    BOOL Has_procedure_entry () const	{ return _procedure.Lastidx () != -1; }
    BOOL Has_proc_info_entry () const	{ return _proc_info.Lastidx () != -1; }
    BOOL Has_callsite_entry () const	{ return _callsite.Lastidx () != -1; }
    BOOL Has_stmt_entry () const	{ return _stmt.Lastidx () != -1; }
    BOOL Has_ctrl_dep_entry () const	{ return _ctrl_dep.Lastidx () != -1; }
    BOOL Has_formal_entry () const	{ return _formal.Lastidx () != -1; }
    BOOL Has_actual_entry () const	{ return _actual.Lastidx () != -1; }
    BOOL Has_value_entry () const	{ return _value.Lastidx () != -1; }
    BOOL Has_expr_entry () const	{ return _expr.Lastidx () != -1; }
    BOOL Has_phi_entry () const		{ return _phi.Lastidx () != -1; }
    BOOL Has_chi_entry () const		{ return _chi.Lastidx () != -1; }
    BOOL Has_symbol_entry () const	{ return _symbol.Lastidx () != -1; }
    BOOL Has_symbol_crefcount_entry () const { return _symbol_crefcount.Lastidx () != -1; }
    BOOL Has_global_entry () const	{ return _global.Lastidx () != -1; }
    BOOL Has_feedback_entry () const	{ return _feedback.Lastidx () != -1; }
    BOOL Has_common_entry () const	{ return _common.Lastidx () != -1; }
    BOOL Has_common_shape_entry () const{ return _common_shape.Lastidx () != -1; }
    BOOL Has_alt_entry () const		{ return _alt_entry.Lastidx () != -1; }
    BOOL Has_inline_attr () const	{ return _inline_attr.Lastidx
					    () != -1; }
    BOOL Has_global_stid_entry () const	{ return _global_stid.Lastidx () != -1; }
    BOOL Has_struct_access_entry() const{return _struct_access.Lastidx()!=-1;}
#ifdef KEY
    BOOL Has_ty_info_entry() const      { return _ty_info.Lastidx () != -1; }
#endif
    // Constraint graph summary for Nystrom Alias Analyzer
    BOOL Has_constraint_graph_nodes() const { return _constraint_graph_nodes.Lastidx () != -1; }
    BOOL Has_constraint_graph_edges() const { return _constraint_graph_edges.Lastidx () != -1; }
    BOOL Has_constraint_graph_stinfos() const { return _constraint_graph_stinfos.Lastidx () != -1; }
    BOOL Has_constraint_graph_callsites() const { return _constraint_graph_callsites.Lastidx () != -1; }
    BOOL Has_constraint_graph_node_ids() const { return _constraint_graph_node_ids.Lastidx () != -1; }
    BOOL Has_constraint_graph_modranges() const { return _constraint_graph_modranges.Lastidx () != -1; }

    INT Get_procedure_idx () const	{ return _procedure.Lastidx (); }
    INT Get_proc_info_idx () const	{ return _proc_info.Lastidx (); }
    INT Get_callsite_idx () const	{ return _callsite.Lastidx (); }
    INT Get_stmt_idx () const		{ return _stmt.Lastidx (); }
    INT Get_ctrl_dep_idx () const	{ return _ctrl_dep.Lastidx (); }
    INT Get_formal_idx () const		{ return _formal.Lastidx (); }
    INT Get_actual_idx () const		{ return _actual.Lastidx (); }
    INT Get_value_idx () const		{ return _value.Lastidx (); }
    INT Get_expr_idx () const		{ return _expr.Lastidx (); }
    INT Get_phi_idx () const		{ return _phi.Lastidx (); }
    INT Get_chi_idx () const		{ return _chi.Lastidx (); }
    INT Get_symbol_idx () const		{ return _symbol.Lastidx (); }
    INT Get_symbol_crefcount_idx () const { return _symbol_crefcount.Lastidx (); }
    INT Get_global_idx () const		{ return _global.Lastidx (); }
    INT Get_feedback_idx () const	{ return _feedback.Lastidx (); }
    INT Get_common_idx () const		{ return _common.Lastidx (); }
    INT Get_common_shape_idx () const	{ return _common_shape.Lastidx (); }
    INT Get_alt_entry_idx () const	{ return _alt_entry.Lastidx (); }
    INT Get_inline_attr_idx () const	{ return _inline_attr.Lastidx (); }
    INT Get_global_stid_idx () const	{ return _global_stid.Lastidx (); }
    INT Get_struct_access_idx() const	{return _struct_access.Lastidx();}
#ifdef KEY
    INT Get_ty_info_idx () const        { return _ty_info.Lastidx (); }
#endif
    // Constraint graph summary for Nystrom Alias Analyzer
    INT Get_constraint_graph_nodes_idx() const { return _constraint_graph_nodes.Lastidx(); }
    INT Get_constraint_graph_edges_idx() const { return _constraint_graph_edges.Lastidx(); }
    INT Get_constraint_graph_stinfos_idx() const { return _constraint_graph_stinfos.Lastidx(); }
    INT Get_constraint_graph_callsites_idx() const { return _constraint_graph_callsites.Lastidx(); }
    INT Get_constraint_graph_node_ids_idx() const { return _constraint_graph_node_ids.Lastidx(); }
    INT Get_constraint_graph_modranges_idx() const { return _constraint_graph_modranges.Lastidx(); }

    // constructor

    SUMMARIZE (MEM_POOL *m) {
        SUMMARY_VALUE* value = NULL; 
	Set_mem_pool (m);
	_procedure.Set_Mem_Pool (m);
	_proc_info.Set_Mem_Pool (m);
	_callsite.Set_Mem_Pool (m);
	_stmt.Set_Mem_Pool (m);
	_ctrl_dep.Set_Mem_Pool (m);
	_formal.Set_Mem_Pool (m);
	_actual.Set_Mem_Pool (m);
	_value.Set_Mem_Pool (m);
	_expr.Set_Mem_Pool (m);
	_phi.Set_Mem_Pool (m);
	_chi.Set_Mem_Pool (m);
	_symbol.Set_Mem_Pool (m);
	_symbol_crefcount.Set_Mem_Pool (m);
	_global.Set_Mem_Pool (m);
	_feedback.Set_Mem_Pool (m);
	_common.Set_Mem_Pool (m);
	_common_shape.Set_Mem_Pool (m);
	_tcon.Set_Mem_Pool (m);
	_alt_entry.Set_Mem_Pool (m);
	_inline_attr.Set_Mem_Pool (m);
	_global_stid.Set_Mem_Pool (m);
	_struct_access.Set_Mem_Pool (m);
#ifdef KEY
	_ty_info.Set_Mem_Pool (m);
#endif

        // Constraint graph specific data for Nystrom Alias Analyzer
        _constraint_graph_nodes.Set_Mem_Pool(m);
        _constraint_graph_edges.Set_Mem_Pool(m);
        _constraint_graph_stinfos.Set_Mem_Pool(m);
        _constraint_graph_callsites.Set_Mem_Pool(m);
        _constraint_graph_node_ids.Set_Mem_Pool(m);
        _constraint_graph_modranges.Set_Mem_Pool(m);

	Trace_Modref = FALSE;
	entry_point = NULL;
	File_Pragmas = FALSE;

	Global_index = CXX_NEW (DYN_ARRAY<INT>(m), m);

	INT Global_index_size = ST_Table_Size (GLOBAL_SYMTAB);
	Global_index->Setidx (Global_index_size);
	Global_index->Bzero_array ();

	Global_hash_table = 0;
	//reorder added:
    	Ty_to_access_map=CXX_NEW(TY_TO_ACCESS_MAP(20),mem);
	loop_count_stack=CXX_NEW(LOOP_COUNT_STACK(mem),mem);;
#ifdef KEY
    	Ty_info_hash_table = CXX_NEW (TY_INFO_HASH_TABLE(113, mem), mem);
#endif

	Init_Aux_Symbol_Info (GLOBAL_SYMTAB);

	if (program == INLINER)
	    return;
	
	/* set up the two most common constants */
	value = New_value ();
	value->Set_mtype (MTYPE_I4);
	value->Set_int_const ();
	value->Set_int_const_value (0);

	value = New_value ();
	value->Set_mtype (MTYPE_I4);
	value->Set_int_const ();
	value->Set_int_const_value (1);

    };

    // public operations
    
    void Summarize (WN *w);
    void Process_procedure (WN *w);
    INT Get_symbol_index (const ST *st);
    void Set_global_addr_taken_attrib (void);

    INT Get_symbol_crefcount_index (INT32 i); // only for inliner
    INT Find_symbol_crefcount_index (INT32 i); // only for inliner
    INT Find_proc_info_index(INT32 i); // only for inliner
    void Finish_collect_struct_access(void){ //only for reorder
		INT num_summary_access=Get_struct_access_idx()+1;
	    if (num_summary_access) {
	      // fill in hot_fld[] according to flds[],
	      Get_struct_access(0)->Set_hot_fld_array(num_summary_access);
	    } 
		MEM_POOL_Pop (&reorder_ipl_pool);
		/*free Ptr_to_ty_vector, local_cands, flds[] of each summary_struct_access*/

	};


    void Trace(FILE* fp);
  
}; // SUMMARIZE

#ifdef IPA_SUMMARY

#define PROGRAM_NAME IPL

#define Trace_CopyProp (0)

#else // IPA_SUMMARY

#define PROGRAM_NAME INLINER
extern BOOL Trace_CopyProp;

#endif // IPA_SUMMARY

typedef SUMMARIZE<PROGRAM_NAME> SUMMARY;

extern SUMMARY *Summary;

#endif /* ipl_summarize_INCLUDED */
