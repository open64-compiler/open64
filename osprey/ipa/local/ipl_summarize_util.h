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
 * Module: ipl_summarize_util.h
 *
 * Description:
 *	utility routines used by templates function in template SUMMARIZE
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef ipl_summarize_util_INCLUDED
#define ipl_summarize_util_INCLUDED

#include <vector>

#ifndef FB_WHIRL_INCLUDED
#include "fb_whirl.h"
#endif

#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))
// return the callsite id of a particular call
INT32 IPL_get_callsite_id(WN* w);
#endif
// return the depth of the deepest loop surrounding this WN. If
// none exists return 0
INT IPL_deepest_depth(WN* w);

// return the stmt or scf surrounding an expression node
extern 
WN* IPL_get_stmt_scf(WN* av);

// aux. information for each ST that is not part of the summary.
struct IPL_ST_INFO
{
    UINT32 summary_symbol_idx;		// idx to the SUMMARY_SYMBOL array
    mBOOL addr_passed;			// computed by IPL
    mBOOL addr_saved;			// computed by IPL
    mBOOL addr_saved_reset;		// ST_addr_saved bit in the symbol
					// table has been reset by IPL

    IPL_ST_INFO () :
	summary_symbol_idx ((UINT32) -1),
	addr_passed (FALSE),
	addr_saved (FALSE),
	addr_saved_reset (FALSE) {}
};    

typedef vector<vector<IPL_ST_INFO> > AUX_SYMBOL_INFO;
extern AUX_SYMBOL_INFO Aux_Symbol_Info;

struct AUX_SYMBOL_ACCESS
{
    IPL_ST_INFO& operator[] (ST_IDX st_idx) const {
	Is_True (ST_IDX_index (st_idx) <
		 Aux_Symbol_Info[ST_IDX_level (st_idx)].size (),
		 ("array out of bound for Aux_Symbol"));
	return Aux_Symbol_Info[ST_IDX_level (st_idx)][ST_IDX_index (st_idx)];
    }

    IPL_ST_INFO& operator() (const ST* st) const {
	ST_IDX st_idx = ST_st_idx (st);
	Is_True (ST_IDX_index (st_idx) <
		 Aux_Symbol_Info[ST_IDX_level (st_idx)].size (),
		 ("array out of bound for Aux_Symbol"));
	return Aux_Symbol_Info[ST_IDX_level (st_idx)][ST_IDX_index (st_idx)];
    }

};
extern AUX_SYMBOL_ACCESS Aux_Symbol;

extern void
Init_Aux_Symbol_Info (SYMTAB_IDX level);

extern void 
Set_lang (SUMMARY_PROCEDURE *proc);

extern FLD_HANDLE
get_field_entry_nme (TY_IDX t, STR_IDX nme);

extern BOOL
Fortran_string_compare (char *pragma, char *func);

extern WN *
get_mod_target (WN *wn);

extern void
Inc_modcount (SUMMARY_GLOBAL *global, SUMMARY_SYMBOL *symbol, const WN *rhs);

extern BOOL
Is_Value_Restored (WN *w, WN *orig_w, WN_MAP wn_to_cr_map);

void
Init_cdg (MEM_POOL *m);

extern INT
Get_stmt_id (void);

extern void
Reset_cd_and_stmt (WN_MAP Summary_Map, INT saved_cd_idx, INT saved_stmt_id);

extern SUMMARY_CONTROL_DEPENDENCE *
Get_new_cd (void);

extern SUMMARY_CONTROL_DEPENDENCE *
Get_cd_by_idx (INT idx);

extern INT
Get_cd_idx (SUMMARY_CONTROL_DEPENDENCE *cd);

extern void
Inc_cd_call_count (INT idx, BOOL branch);

// returns the number of STMT_CALL SUMMARY_STMT node under the given cd.
extern INT
Get_cd_call_count (INT idx);

extern INT
Get_cd_call_count (INT idx, BOOL branch);

extern void
Set_cd_head_of_chain (INT idx);

extern void
Set_cd_ctrl_index (SUMMARY_CONTROL_DEPENDENCE *cd, INT index);

extern INT
Get_cd_real_idx (SUMMARY_CONTROL_DEPENDENCE *cd);

extern SUMMARY_STMT *
Get_new_stmt (INT idx, BOOL branch, WN *w);

extern SUMMARY_STMT *
Get_last_stmt (INT idx, BOOL branch) ;

extern SUMMARY_CONTROL_DEPENDENCE *
Get_next_cd_chain (void);

extern SUMMARY_CONTROL_DEPENDENCE *
Get_next_cd (void);

extern SUMMARY_STMT *
Get_summary_stmts (SUMMARY_CONTROL_DEPENDENCE *cd, BOOL branch);

// return the SUMMARY_CONTROL_DEPENDENCE node controlling the given stmt.
extern SUMMARY_CONTROL_DEPENDENCE *
Get_controlling_stmt (WN *stmt_node);

// search the SUMMARY_CONTROL_DEPENDENCE and SUMMARY_STMT trees for the
// SUMMARY_STMT node pointer, the index into the SUMMARY_STMT array, and
// which branch of the control flow stmt it's under.
extern SUMMARY_STMT *
Search_for_summary_stmt (WN *stmt_node, BOOL &branch, INT &stmt_idx);

/* given a control flow stmt (if or do-loop) that is known to have a
   ctrl. dep. structure, return the corresponding pointer to
   SUMMARY_CONTROL_DEPENDENCE */
extern SUMMARY_CONTROL_DEPENDENCE *
Search_for_own_cd (WN *cond_stmt);

// Given a description of a SUMMARY_STMT node, return its corresponding
// WHIRL node.
extern WN *
Get_stmt_node (SUMMARY_CONTROL_DEPENDENCE *cd, BOOL branch, INT stmt_idx);

// return the size of the control dependence structure
extern INT
Get_max_cd_idx();

extern void
Fix_phi_node_ctrl_dep_index (SUMMARY_PHI *phi, INT num);

// a cache for removing duplicated summary info entries (e.g.,
// SUMMARY_VALUE, SUMMARY_EXPR.)
enum SUMMARY_TYPE {
    SUM_VALUE = 0,
    SUM_EXPR = 1,
    SUM_PHI = 2,
    SUM_CHI = 3
};

class SUMMARY_ENTRY_CACHE
{
private:
    struct hash_node {
	struct hash_node *next;
	SUMMARY_TYPE _type : 8;
	INT _idx : 24;
    } *table[0x80];

    MEM_POOL *mem;

    INT hash (INT64 key) const {
	return ((UINT64) key) & ((UINT64) 0x7f);
    }

public:

    SUMMARY_ENTRY_CACHE (MEM_POOL *m) {
	mem = m;
	BZERO (table, sizeof(table));
    }

    void Insert (SUMMARY_TYPE type, INT idx) {
	struct hash_node *p = CXX_NEW (struct hash_node, mem);
	p->_type = type;
	p->_idx = idx;
	
	INT hash_value;

	switch (type) {
	case SUM_VALUE:
	    hash_value =
		hash (Summary->Get_value (idx)->Get_int_const_value ());
	    break;

	case SUM_EXPR:
	    hash_value =
		hash (Summary->Get_expr (idx)->Get_const_value ());
	    break;

	case SUM_PHI: {
	      SUMMARY_PHI *phi = Summary->Get_phi (idx);
	      hash_value =
		  hash (phi->Get_node_index (0) + phi->Get_node_index (1));
            } 
	    break;

	case SUM_CHI:
	    hash_value =
		hash (Summary->Get_chi (idx)->Get_node_index ());
	    break;
	}

	p->next = table[hash_value];
	table[hash_value] = p;
    } // Insert

    INT Lookup (SUMMARY_TYPE type, void *node) const;

}; // SUMMARY_ENTRY_CACHE

extern SUMMARY_ENTRY_CACHE *entry_cache;

extern BOOL IsStidToReturnPreg(WN *wn, WN* entry_point);

extern INT GetChildIndex(WN *parent, WN*child);

extern TY *GetArgType(TYLIST*, INT);

extern void
Count_tree_size (FEEDBACK& fb, WN *w, INT32 &bbs, INT32 &stmts, FB_FREQ& cycles, FB_FREQ& freq_count);

//INLINING_TUNING^
extern void
Count_tree_size_tuning (FEEDBACK& fb, WN *wn, INT32 &bbs, INT32 &stmts, FB_FREQ& cycles, FB_FREQ &freq_count, UINT16 &WNs, FB_FREQ &cycle_tuning );
//INLINING_TUNING$


extern BOOL DoPreopt;

// -----------------------------------------------------------------
// SUMMARY_ENTRY_CACHE is not very effective for PHI and CHI nodes.
// Instead of fully tracing chains of PHIs and CHIs and then hashing 
// on the corresponding SUMMARY_* structs, these data structures 
// enable hashing on CODEREP and PHI_NODE pointers. Dynamic arrays
// of these pointers are used to memorize the entries that should
// be deleted from hash tables in Restore_from_check_point.
// -----------------------------------------------------------------

// OSP, remove the unnamed namespace to make GCC 4.1.x and above happy
// namespace {
    template <class X>
    struct ptr_hash {
	size_t operator() (const X* s) const {
	    return reinterpret_cast<size_t> (s);
	}
    };
// }

typedef hash_map<CODEREP*, INT, ptr_hash<CODEREP>, std::equal_to<CODEREP*>,
    mempool_allocator<CODEREP*> > CHI_CR_TO_INT_MAP;

extern CHI_CR_TO_INT_MAP* Chi_To_Idx_Map;
typedef vector<CODEREP*, mempool_allocator<CODEREP*> > CHI_CR_ARRAY;
extern CHI_CR_ARRAY* Hashed_Chis;
extern INT Num_Chis_On_PU_Start;

typedef hash_map<PHI_NODE*, INT, ptr_hash<PHI_NODE>, std::equal_to<PHI_NODE*>,
    mempool_allocator<PHI_NODE*> > PHI_NODE_TO_INT_MAP;
extern PHI_NODE_TO_INT_MAP* Phi_To_Idx_Map;
typedef vector<PHI_NODE*, mempool_allocator<PHI_NODE*> > PHI_NODE_ARRAY;
extern PHI_NODE_ARRAY* Hashed_Phis;
extern INT Num_Phis_On_PU_Start;

// -------------------------------------------------------
// This class is used for checkpointing of indices into 
// various summary arrays. It is highly recommended to
// use it with SUMMARIZE<>::Restore_from_check_point.
// With very few exceptions, using Decidx on an individual 
// summary array is not safe.
// -------------------------------------------------------

class SUMMARY_CHECK_POINT
{
private:
    
    INT _value_idx;
    INT _expr_idx;
    INT _phi_idx;
    INT _hashed_phi_idx;
    INT _chi_idx;
    INT _hashed_chi_idx;
    INT _cd_idx;
    INT _stmt_id;

public:

    SUMMARY_CHECK_POINT (const void *s) {
	const SUMMARIZE<IPL> *sum = (const SUMMARIZE<IPL> *) s;
	_value_idx = sum->Get_value_idx ();
	_expr_idx = sum->Get_expr_idx ();
	if (DoPreopt) {
	    _phi_idx = sum->Get_phi_idx ();
            _hashed_phi_idx = Hashed_Phis->size() - 1;
	    _chi_idx = sum->Get_chi_idx ();
            _hashed_chi_idx = Hashed_Chis->size() - 1;
	    _cd_idx = Get_max_cd_idx ();
	    _stmt_id = Get_stmt_id ();
	}
    }

    INT value_idx () const	{ return _value_idx; }
    INT expr_idx () const	{ return _expr_idx; }
    INT phi_idx () const	{ return _phi_idx; }
    INT hashed_phi_idx () const	{ return _hashed_phi_idx; }
    INT chi_idx () const	{ return _chi_idx; }
    INT hashed_chi_idx () const	{ return _hashed_chi_idx; }
    INT cd_idx () const		{ return _cd_idx; }
    INT stmt_id () const	{ return _stmt_id; }
    
}; // SUMMARY_CHECK_POINT


#endif /* ipl_summarize_util_INCLUDED */
