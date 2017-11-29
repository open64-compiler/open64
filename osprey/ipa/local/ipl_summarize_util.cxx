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


/* ====================================================================
 * ====================================================================
 *
 * Module: ipl_summarize_util.cxx
 *
 * Description:
 *	utility routines used by templates function in template SUMMARIZE
 *
 * ====================================================================
 * ====================================================================
 */

#include <stdint.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include "defs.h"
#include "stab.h"			// for ST*
#include "wn.h"
#include "targ_sim.h" // for Complex_not_simulated, Get_Return_Mtypes, and 
                      // Get_Return_Pregs

#include "cxx_template.h"		// for DYN_ARRAY
#include "cxx_memory.h"			// for CXX_NEW

#include "opt_du.h"                     // for DU-manager

#include "ipl_summary.h"		// for summary info structures
#include "ipl_summarize.h"		// for SUMMARY
#include "ipl_summarize_util.h"		// for SUMMARY_ENTRY_CACHE
#include "ipl_main.h"			// for Stmt_Map

#ifndef opt_emit_INCLUDED
#include "opt_emit.h"			// for EMITTER class
#endif // opt_emit_INCLUDED

#ifndef fb_whirl_INCLUDED
#include "fb_whirl.h"			// for Query
#endif // fb_whirl_INCLUDED

#pragma weak Def_at_entry__7CODEREPCGv
#pragma weak Print_kind__7CODEREPCGv

// Hashing on CODEREP and PHI_NODE pointers is much faster than tracing
// chains of PHIs and CHIs and then hashing on the SUMMARY_* structs
CHI_CR_TO_INT_MAP* Chi_To_Idx_Map;
CHI_CR_ARRAY* Hashed_Chis;
INT Num_Chis_On_PU_Start;

PHI_NODE_TO_INT_MAP* Phi_To_Idx_Map;
PHI_NODE_ARRAY* Hashed_Phis;
INT Num_Phis_On_PU_Start;


#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))
// given a call node, return the id into the callsite array
// this is stored as a map
INT32 IPL_get_callsite_id(WN* w)
{
  FmtAssert(WN_operator(w) == OPR_CALL 
    || WN_operator(w) == OPR_ICALL
    || WN_operator(w) == OPR_INTRINSIC_CALL, 
    ("IPL_get_callsite_id: Expecting CALL node"));

  return 
    WN_MAP32_Get(Summary_Map, w) - 1;
}
#endif
//------------------------------------------------------------------------
// get the statement surrounding this node
//--------------------------------------------------------------------------
WN* IPL_get_stmt_scf(WN* av)
{
  BOOL done = FALSE;
  WN* parent_w = NULL;

  parent_w = av;
  while (!done)
    {
     parent_w = LWN_Get_Parent(parent_w);
      if (parent_w == NULL)
	done = TRUE;
      else if (OPCODE_is_stmt(WN_opcode(parent_w)) || 
	       OPCODE_is_scf(WN_opcode(parent_w)))
	return parent_w;
   }

  Fail_FmtAssertion("NULL wn in Get_stmt_scf\n");
  return NULL;

}

//--------------------------------------------------------------------------
// given a wn, figure out how many loops are surrounding it
//--------------------------------------------------------------------------
INT
IPL_deepest_depth(WN* w)
{
  SUMMARY_CONTROL_DEPENDENCE *d;
  INT depth = 0;
  WN* control_node = NULL;

  WN* stmt = IPL_get_stmt_scf(w);

  d = Get_controlling_stmt(stmt);
  while (d)
    {
      if (d->Is_do_loop())
        ++depth;
      control_node = d->Get_wn();
      d = Get_controlling_stmt(control_node);
    }
  return depth;
}

AUX_SYMBOL_INFO Aux_Symbol_Info (1);
AUX_SYMBOL_ACCESS Aux_Symbol;

void
Init_Aux_Symbol_Info (SYMTAB_IDX level)
{
    FmtAssert (Aux_Symbol_Info.size () >= level,
		("Processing a nested procedure before it's parent"));
    
    // delete vector for previous PUs
    while (Aux_Symbol_Info.size () > level)
	Aux_Symbol_Info.pop_back ();

    // create a new vector
    UINT32 table_size = ST_Table_Size (level);

    typedef AUX_SYMBOL_INFO::value_type AUX_ST_INFO;

    AUX_ST_INFO new_level (table_size);
    Aux_Symbol_Info.push_back (AUX_ST_INFO ());
    AUX_ST_INFO& st_info = Aux_Symbol_Info.back ();
    st_info.swap (new_level);
    
} // Init_Aux_Symbol_Info
    

//-----------------------------------------------------------
// set the language for the pu
//-----------------------------------------------------------
void 
Set_lang (SUMMARY_PROCEDURE *proc)
{
    switch (PU_src_lang (Get_Current_PU ())) {
    case PU_C_LANG:
	proc->Set_lang (LANG_ANSI_C);
	break;
	
    case PU_CXX_LANG:
 	proc->Set_lang (LANG_CPLUS);
	break;

    case PU_F77_LANG:
	proc->Set_lang (LANG_F77);
	break;

    case PU_F90_LANG:
	proc->Set_lang (LANG_F90);
	break;
    }
} // Set_lang




//-----------------------------------------------------------
// get field entry from the name
//-----------------------------------------------------------
FLD_HANDLE
get_field_entry_nme (TY_IDX t, STR_IDX nme)
{
    FLD_ITER fld_iter = Make_fld_iter (TY_fld (Ty_Table[t]));
    
    do {
	FLD_HANDLE fld (fld_iter);
	if (FLD_name_idx (fld) == nme)
	    return fld;
    } while (!FLD_last_field (fld_iter++));

    Fail_FmtAssertion("null field entry for common? \n");
    // should never reach here
    return fld_iter;
}


/* in Fortran, variable names are converted to lower case and appended with
   a '_'.  The function names specified in the pragma are passed as is */
BOOL
Fortran_string_compare (char *pragma, char *func)
{
    INT p_len = strlen (pragma);
    INT f_len = strlen (func);

    if (p_len + 1 != f_len)
	return FALSE;

    return (strncasecmp (pragma, func, p_len) == 0 && func[f_len-1] == '_');
    
} // Fortran_string_compare


/* Given an expression tree, search for the node containing the ST of the
   variable that is being modified */
WN *
get_mod_target (WN *wn)
{
    OPERATOR opr = WN_operator (wn);
    INT i;

    if (!OPERATOR_is_expression (WN_operator(wn)))
	return 0;

    switch (opr) {
    case OPR_LDA:
	return wn;

    case OPR_ARRAY:
	return get_mod_target (WN_array_base(wn));

    case OPR_ILOAD:
    case OPR_LDID:
	return 0;

    default:
	for (i = 0; i < WN_kid_count(wn); i++) {
	    WN *w = get_mod_target (WN_kid(wn, i));
	    if (w)
		return w;	    // return first node found
	}
    }

    return 0;
	
} // get_mod_target 


void
Inc_modcount (SUMMARY_GLOBAL *global, SUMMARY_SYMBOL *symbol, const WN *rhs)
{
    OPERATOR opr = WN_operator(rhs);

    if (symbol->Is_cmod()) {
	if (opr != OPR_INTCONST ||
	    symbol->Get_const_value () != WN_const_val(rhs)) {
	    symbol->Clear_cmod ();
	    symbol->Clear_const_value ();
	}
    } else if (!symbol->Is_modcount () && opr == OPR_INTCONST) {
	symbol->Set_cmod ();
	symbol->Set_const_value (WN_const_val(rhs));
    }
    global->Inc_modcount ();
    symbol->Set_modcount ();
} // Inc_modcount

#ifndef _LIGHTWEIGHT_INLINER
/* Given a STID node, find out if the variable is being restored to its
   original value upon entry of the function.  If so, this mod can be
   discarded.
 */
BOOL
Is_Value_Restored (WN *w, WN *orig_w, WN_MAP wn_to_cr_map)
{
    WN *wn_ldid = WN_kid0(w);	    // the LDID node
    OPCODE op_ldid = WN_opcode(wn_ldid);

    if (OPCODE_operator (op_ldid) != OPR_LDID)
	return FALSE;

    if (WN_desc(orig_w) != OPCODE_rtype(op_ldid))
	return FALSE;

    CODEREP *cr = (CODEREP *) WN_MAP_Get (wn_to_cr_map, wn_ldid);

    if (cr == NULL || cr->Kind () != CK_VAR || cr->Is_flag_set (CF_DEF_BY_PHI))
	return FALSE;

    if (cr->Def_at_entry ()) {
	if (WN_st(wn_ldid) == WN_st(orig_w) &&
	    WN_store_offset(orig_w) == WN_load_offset(wn_ldid))
	    return TRUE;
    } else {

	if (cr->Is_flag_set (CF_DEF_BY_CHI))
	    return FALSE;

	STMTREP *stmt = cr->Defstmt ();
	if (stmt == NULL)
	    // volatiles have no defstmt
	    return FALSE;
	
	WN *def = stmt->Wn();

	if (WN_operator (def) == OPR_STID)
	    return Is_Value_Restored (def, orig_w, wn_to_cr_map);
    }

    return FALSE;
} // Is_Value_Restored
#endif


/* ----------------------------------------------------------------------
 *
 * Routines for building control dependence information
 *
 * ----------------------------------------------------------------------
 */

// We need to build the SUMMARY_CONTROL_DEPENDENCE and SUMMARY_STMT structures
// in a nice order without duplications.  So we build the whole thing in
// memory first, and then generate the summary info.  


typedef DYN_ARRAY<SUMMARY_STMT> STMT_ARRAY;
typedef DYN_ARRAY<INT> STMT_ID_ARRAY;
typedef DYN_ARRAY<WN *> WN_ARRAY;

class CTRL_DEP {
private:

    SUMMARY_CONTROL_DEPENDENCE _cd;
    
    STMT_ARRAY _true_stmts;		// stmts executed if cond. is true
    STMT_ARRAY _false_stmts;		// stmts executed if cond. is false

    // the followings are 1-1 mapping of the WN * corresponding to each
    // STMT_ARRAY entry
    WN_ARRAY _true_wns;
    WN_ARRAY _false_wns;
    
    // 1-1 mapping of stmt id for backtracking identification
    STMT_ID_ARRAY _true_stmt_id;
    STMT_ID_ARRAY _false_stmt_id;

    INT _true_call_count;		// number of STMT_CALL under _cd
    INT _false_call_count;

    /* head_of_chain: For each chain of control dependency, it should end
       up control dependent on the entry of the function.  In some cases,
       we give up following the chain when the conditional expression is
       too compliated.  Thus, we might end up with multiple chains, and we
       set head_of_chain to true for the cd node of the beginning of each
       chain.  In main IPA, we do a forward pass through all the
       conditional expression and mark dead stmts accordingly.  We would
       like to evaluate each condition AFTER all its control dependent
       nodes. Thus, we need to write out each node in the right order. */

    BOOL _head_of_chain;

    INT ctrl_index;			// index to the cd node in the
					// summary info
    
public:

    /* access functions */

    SUMMARY_CONTROL_DEPENDENCE *Get_cd () 	{ return &_cd; }

    SUMMARY_STMT *Get_true_stmts () const	{ return &(_true_stmts[0]); }
    SUMMARY_STMT *Get_false_stmts () const	{ return &(_false_stmts[0]); }

    WN **Get_true_wns () const			{ return &(_true_wns[0]); }
    WN **Get_false_wns () const			{ return &(_false_wns[0]); }

    INT Get_true_stmt_id (INT idx) const	{ return _true_stmt_id[idx]; }
    INT Get_false_stmt_id (INT idx) const	{ return _false_stmt_id[idx]; }
    
    void Reset_true_stmts (INT idx) {
	_true_stmts.Setidx (idx);
	_true_wns.Setidx (idx);
	_true_stmt_id.Setidx (idx);
	_cd.Set_true_count (idx + 1);
    }

    void Reset_false_stmts (INT idx) {
	_false_stmts.Setidx (idx);
	_false_wns.Setidx (idx);
	_false_stmt_id.Setidx (idx);
	_cd.Set_false_count (idx + 1);
    }


    void Inc_call_count (BOOL branch) {
	if (branch)
	    ++_true_call_count;
	else
	    ++_false_call_count;
    }
    INT Get_call_count () const	{
	return _true_call_count + _false_call_count;
    }
    INT Get_call_count (BOOL branch) const {
	return branch ? _true_call_count : _false_call_count;
    }

    void Set_head_of_chain ()			{ _head_of_chain = TRUE; }
    BOOL Is_head_of_chain () const		{ return _head_of_chain; }

    void Set_ctrl_index (INT idx)		{ ctrl_index = idx; }
    INT Get_ctrl_index () const			{ return ctrl_index; }

    /* operations */

    void Init () {
	BZERO (this, sizeof(CTRL_DEP));
	_head_of_chain = FALSE;
    }

    inline SUMMARY_STMT *Get_new_stmt (BOOL branch, WN *w);
    inline SUMMARY_STMT *Get_last_stmt (BOOL branch);

}; // class CTRL_DEP


typedef DYN_ARRAY<CTRL_DEP> CTRL_DEP_ARRAY;
static CTRL_DEP_ARRAY *cdg;		// control dependence graph
static MEM_POOL *CTRL_DEP_mem;		// mem pool used for the cd graph
static INT stmt_id;			// id for backtracking
static INT iterator_idx;		// should be an iterator class

// create a new SUMMARY_STMT node in the specified branch of the given
// control dependence node
inline SUMMARY_STMT *
CTRL_DEP::Get_new_stmt (BOOL branch, WN *w)
{
    INT idx;

    if (branch == TRUE) {
	if (_cd.Get_true_count () == 0) {
	    new (&_true_stmts) STMT_ARRAY (CTRL_DEP_mem);
	    new (&_true_wns) WN_ARRAY (CTRL_DEP_mem);
	    new (&_true_stmt_id) STMT_ID_ARRAY (CTRL_DEP_mem);
	}
	_true_stmt_id.AddElement (++stmt_id);
	_true_wns.AddElement (w);
	idx = _true_stmts.Newidx ();
	_cd.Set_true_count (idx + 1);
	_true_stmts[idx].Init ();
	return &(_true_stmts[idx]);
    } else {
	if (_cd.Get_false_count () == 0) {
	    new (&_false_stmts) STMT_ARRAY (CTRL_DEP_mem);
	    new (&_false_wns) WN_ARRAY (CTRL_DEP_mem);
	    new (&_false_stmt_id) STMT_ID_ARRAY (CTRL_DEP_mem);
	}
	_false_stmt_id.AddElement (++stmt_id);
	_false_wns.AddElement (w);
	idx = _false_stmts.Newidx ();
	_cd.Set_false_count (idx + 1);
	_false_stmts[idx].Init ();
	return &(_false_stmts[idx]);
    }
} // CTRL_DEP::Get_new_stmt 


inline SUMMARY_STMT *
CTRL_DEP::Get_last_stmt (BOOL branch)
{
    if (branch == TRUE) {
	if (_cd.Get_true_count () == 0)
	    return 0;
	return &(_true_stmts[_true_stmts.Lastidx()]);
    } else {
	if (_cd.Get_false_count () == 0)
	    return 0;
	return &(_false_stmts[_false_stmts.Lastidx()]);
    }
    
} // CTRL_DEP::Get_last_stmt

void
Init_cdg (MEM_POOL *m)
{
    CTRL_DEP_mem = m;
    cdg = CXX_NEW (CTRL_DEP_ARRAY (m), m);
    iterator_idx = -1;
    stmt_id = 0;
}


INT
Get_stmt_id (void)
{
    return stmt_id;
}

// reset the cd and stmt strucutres to the saved checkpoint.
void
Reset_cd_and_stmt (WN_MAP Summary_Map, INT saved_cd_idx, INT saved_stmt_id)
{
    if (stmt_id > saved_stmt_id) {
	for (INT cd_idx = cdg->Lastidx (); cd_idx >= 0; --cd_idx) {
	    if (cd_idx > saved_cd_idx)
		continue;
	    CTRL_DEP &p = (*cdg)[cd_idx];
	    INT stmt_idx = p.Get_cd()->Get_true_count ();
	    BOOL need_to_reset = FALSE;
	    while (--stmt_idx >= 0 &&
		   p.Get_true_stmt_id (stmt_idx) > saved_stmt_id)
		need_to_reset = TRUE;
	    if (need_to_reset)
		p.Reset_true_stmts (stmt_idx);

	    stmt_idx = p.Get_cd ()->Get_false_count ();
	    need_to_reset = FALSE;
	    while (--stmt_idx >= 0 &&
		   p.Get_false_stmt_id (stmt_idx) > saved_stmt_id)
		need_to_reset = TRUE;
	    if (need_to_reset) 
		p.Reset_false_stmts (stmt_idx);
	}
    }

    while (saved_cd_idx < cdg->Lastidx ()) {
	WN_MAP32_Set (Summary_Map, (*cdg)[cdg->Lastidx()].Get_cd ()->Get_wn (),
		      0);
	cdg->Decidx ();
    }
    
} // Reset_cd_and_stmt


SUMMARY_CONTROL_DEPENDENCE *
Get_new_cd (void)
{
    INT idx = cdg->Newidx ();
    (*cdg)[idx].Init ();
    return (*cdg)[idx].Get_cd ();
} // Get_new_cd


// Given an index to the control dependence graph array, return the element.
SUMMARY_CONTROL_DEPENDENCE *
Get_cd_by_idx (INT idx)
{
    if (idx <= cdg->Lastidx())
	return (*cdg)[idx].Get_cd ();
    else
	return NULL;
} // Get_cd_by_idx

// return the size of the control dependence array
INT Get_max_cd_idx()
{
  return cdg->Lastidx();
}

// return the index to the control dependence graph array
INT
Get_cd_idx (SUMMARY_CONTROL_DEPENDENCE *cd)
{
    return cdg->Idx ((CTRL_DEP *) cd);
} 


void
Inc_cd_call_count (INT idx, BOOL branch)
{
    (*cdg)[idx].Inc_call_count (branch);
}

INT
Get_cd_call_count (INT idx)
{
  return (*cdg)[idx].Get_call_count ();
}

INT
Get_cd_call_count (INT idx, BOOL branch)
{
    return (*cdg)[idx].Get_call_count (branch);
}

void
Set_cd_head_of_chain (INT idx)
{
    (*cdg)[idx].Set_head_of_chain ();
    
} // Set_cd_head_of_chain


void
Set_cd_ctrl_index (SUMMARY_CONTROL_DEPENDENCE *cd, INT index)
{
    INT idx = cdg->Idx ((CTRL_DEP *) cd);

    (*cdg)[idx].Set_ctrl_index (index);
} // Set_cd_ctrl_index


INT
Get_cd_real_idx (SUMMARY_CONTROL_DEPENDENCE *cd)
{
    INT idx = cdg->Idx ((CTRL_DEP *) cd);

    return (*cdg)[idx].Get_ctrl_index ();
} // Get_cd_real_idx


SUMMARY_STMT *
Get_new_stmt (INT idx, BOOL branch, WN *w)
{
    return (*cdg)[idx].Get_new_stmt (branch, w);
    
} // Get_new_stmt


SUMMARY_STMT *
Get_last_stmt (INT idx, BOOL branch)
{
    return (*cdg)[idx].Get_last_stmt (branch);
} // Get_last_stmt


// return the next SUMMARY_CONTROL_DEPENDENCE node that is a head of chain
SUMMARY_CONTROL_DEPENDENCE *
Get_next_cd_chain (void)
{
    iterator_idx++;
    while (iterator_idx < cdg->Elements ()) {
	if ((*cdg)[iterator_idx].Is_head_of_chain ())
	    return (*cdg)[iterator_idx].Get_cd ();
	iterator_idx++;
    }

    iterator_idx = -1;
    return NULL;
}


// simply loop through all elements of cdg
SUMMARY_CONTROL_DEPENDENCE *
Get_next_cd (void)
{
    static INT idx = 0;

    while (idx < cdg->Elements ())
	return (*cdg)[idx++].Get_cd ();
    idx = 0;
    return NULL;
}

    
SUMMARY_STMT *
Get_summary_stmts (SUMMARY_CONTROL_DEPENDENCE *cd, BOOL branch)
{
    INT idx = cdg->Idx ((CTRL_DEP *) cd);

    if (branch == TRUE)
	return (*cdg)[idx].Get_true_stmts ();
    else
	return (*cdg)[idx].Get_false_stmts ();
} // Get_summary_stmts


// utility functions for traversing the control dependence and stmt nodes

SUMMARY_CONTROL_DEPENDENCE *
Get_controlling_stmt (WN *stmt_node)
{
    INT idx = WN_MAP32_Get (Stmt_Map, stmt_node) - 1;

    if (idx < 0 || idx > cdg->Lastidx ())
	return NULL;

    return (*cdg)[idx].Get_cd ();
} // Get_controlling_stmt


SUMMARY_STMT *
Search_for_summary_stmt (WN *stmt_node, BOOL &branch, INT &stmt_idx)
{
    INT idx = WN_MAP32_Get (Stmt_Map, stmt_node) - 1;

    if (idx < 0 || idx > cdg->Lastidx ())
	return NULL;
    
    SUMMARY_CONTROL_DEPENDENCE *cd = (*cdg)[idx].Get_cd ();

    INT count; 
    if ((count = cd->Get_true_count ()) > 0) {
	WN **wn_array = (*cdg)[idx].Get_true_wns ();
	for (INT i = 0; i < count; i++)
	    if (wn_array[i] == stmt_node) {
		branch = TRUE;
		stmt_idx = i;
		return (*cdg)[idx].Get_true_stmts () + i;
	    }
    }

    if ((count = cd->Get_false_count ()) > 0) {
	WN **wn_array = (*cdg)[idx].Get_false_wns ();
	for (INT i = 0; i < count; i++)
	    if (wn_array[i] == stmt_node) {
		branch = FALSE;
		stmt_idx = i;
		return (*cdg)[idx].Get_false_stmts () + i;
	    }
    }

    return NULL;

} // Search_for_summary_stmt


/* given a control flow stmt (if or do-loop) that is known to have a
   ctrl. dep. structure, return the corresponding pointer to
   SUMMARY_CONTROL_DEPENDENCE */
SUMMARY_CONTROL_DEPENDENCE *
Search_for_own_cd (WN *cond_stmt)
{
    BOOL branch;
    INT stmt_idx;

    SUMMARY_STMT *stmt = Search_for_summary_stmt (cond_stmt, branch, stmt_idx);

    if (stmt == NULL || ! (stmt->Is_cond ()))
	return NULL;
 
    return Get_cd_by_idx(stmt->Get_cond_index());
} // Search_for_own_cd


WN *
Get_stmt_node (SUMMARY_CONTROL_DEPENDENCE *cd, BOOL branch, INT stmt_idx)
{
    CTRL_DEP *ctrl = (CTRL_DEP *) cd;

    if (branch) {
	if (stmt_idx >= cd->Get_true_count ())
	    return NULL;
	else
	    return ctrl->Get_true_wns()[stmt_idx];
    } else {
	if (stmt_idx >= cd->Get_false_count ())
	    return NULL;
	else
	    return ctrl->Get_false_wns()[stmt_idx];
    }

} // Get_stmt_node


// fix up the ctrl_dep index in the phi nodes
void
Fix_phi_node_ctrl_dep_index (SUMMARY_PHI *phi, INT num)
{
    for (INT i = 0; i < num; ++i) {
	INT idx = phi[i].Get_ctrl_dep_index (0);
	phi[i].Set_ctrl_dep_index (0, (*cdg)[idx].Get_ctrl_index ());
	idx = phi[i].Get_ctrl_dep_index (1);
	phi[i].Set_ctrl_dep_index (1, (*cdg)[idx].Get_ctrl_index ());
    }
} // Fix_phi_node_ctrl_dep_index


SUMMARY_ENTRY_CACHE *entry_cache;

INT
SUMMARY_ENTRY_CACHE::Lookup (SUMMARY_TYPE type, void *node) const
{
    INT hash_value;
    struct hash_node *p;
    SUMMARY_VALUE *value;
    SUMMARY_EXPR *expr;
    SUMMARY_PHI *phi;
    SUMMARY_CHI *chi;

    switch (type) {
    case SUM_VALUE: 
	value = (SUMMARY_VALUE *) node;
	hash_value = hash (value->Get_int_const_value ());
	for (p = table[hash_value]; p; p = p->next) {
	    if (p->_type == type && p->_idx <= Summary->Get_value_idx () &&
		memcmp (value, Summary->Get_value (p->_idx),
			sizeof(SUMMARY_VALUE)) == 0)
		return p->_idx;
	}
	break;

    case SUM_EXPR:
	expr = (SUMMARY_EXPR *) node;
	hash_value = hash (expr->Get_const_value ());
	for (p = table[hash_value]; p; p = p->next) {
	    if (p->_type == type && p->_idx <= Summary->Get_expr_idx () &&
		memcmp (expr, Summary->Get_expr (p->_idx),
			sizeof(SUMMARY_EXPR)) == 0)
		return p->_idx;
	}
	break;

    case SUM_PHI:
	phi = (SUMMARY_PHI *) node;
	hash_value = hash (phi->Get_node_index (0) + phi->Get_node_index (1));
	for (p = table[hash_value]; p; p = p->next) {
	    if (p->_type == type && p->_idx <= Summary->Get_phi_idx () &&
		memcmp (phi, Summary->Get_phi (p->_idx),
			sizeof(SUMMARY_PHI)) == 0)
		return p->_idx;
	}
	break;

    case SUM_CHI:
	chi = (SUMMARY_CHI *) node;
	hash_value = hash (chi->Get_node_index ());
	for (p = table[hash_value]; p; p = p->next) {
	    if (p->_type == type && p->_idx <= Summary->Get_chi_idx () &&
		memcmp (chi, Summary->Get_chi (p->_idx),
			sizeof(SUMMARY_CHI)) == 0)
		return p->_idx;
	}
	break;
    }

    return -1;
} // Lookup 

/* ----------------------------------------------------------------------
 *
 * Routines for determining if a wn tree is an stid to a return preg
 * If so for void_calls the cref/dref due to ldid beneath this stid
 * can be ignored, thus giving us better cref/dref analysis
 * ----------------------------------------------------------------------
 */

static BOOL HasReturnPreg(WN *w, TY_IDX type) {
  TYPE_ID       ty1, ty2;
  PREG_NUM	reg1, reg2;

  if (WHIRL_Return_Info_On) {

    RETURN_INFO return_info = Get_Return_Info (type,
					       Complex_Not_Simulated);

    if (RETURN_INFO_count(return_info) <= 2) {

      ty1 = RETURN_INFO_mtype (return_info, 0);
      ty2 = RETURN_INFO_mtype (return_info, 1);
      reg1 = RETURN_INFO_preg (return_info, 0);
      reg2 = RETURN_INFO_preg (return_info, 1);
    }

    else
      Fail_FmtAssertion ("HasReturnPreg: more than 2 return registers");
  }

  else {
    Get_Return_Mtypes(type, Complex_Not_Simulated, &ty1, &ty2);
    Get_Return_Pregs(ty1, ty2, &reg1, &reg2);
  }
  return ((WN_store_offset(w) == reg1) || (WN_store_offset(w) == reg2));
} // HasReturnPreg

BOOL
IsStidToReturnPreg (WN *wn, WN* entry_point)
{
    if (!WN_operator_is (wn, OPR_STID))
	return FALSE;

    ST *st = wn ? WN_st(wn) : NULL;
    WN *wnkid = wn ? WN_kid0(wn) : NULL;
    if ((st == NULL) || (wnkid==NULL)) 
	return FALSE;
    TY_IDX tykid = WN_ty(wnkid);
    TY_IDX proctype = ST_pu_type(WN_entry_name(entry_point));
    TY_IDX returnty = Tylist_Table[TY_tylist (Ty_Table[proctype])];
    return ((ST_class(st) == CLASS_PREG) &&
	    HasReturnPreg(wn,returnty) &&
	    WN_operator_is(wnkid,OPR_LDID) && (!TY_is_volatile(tykid)));
}
// IsStidToReturnPreg

INT GetChildIndex(WN *parent, WN* child) {
    INT pos = 0;
    while ((pos < WN_kid_count(parent)) && (WN_kid(parent,pos) != child))
	++pos;
    return pos;
} // GetChildIndex


static void 
count_stats(WN *w, INT32& bbs, INT32& stmts, FB_FREQ& cycles, FB_FREQ freq)
{

    OPERATOR opr = OPCODE_operator(WN_opcode(w));

    TYPE_ID rtype = OPCODE_rtype(WN_opcode(w));

    /* count nscf stmts as bbs, not stmts */
    if (OPERATOR_is_non_scf(opr)) {
	++bbs;
	    if (freq.Known())
	        cycles += freq;
    } else if (OPERATOR_is_stmt(opr)) {
	if (OPERATOR_is_call(opr)) {
	    ++bbs;
	    if (freq.Known())
	        cycles += freq;
	} else if (opr == OPR_IO) {
	    ++bbs;
	    if (freq.Known())
	        cycles += freq;
	} else if (! OPERATOR_is_not_executable(opr)) {
	    ++stmts;
	    if (freq.Known())
	        cycles += freq;
	    if (MTYPE_is_complex(rtype) && OPERATOR_is_store(opr)) {
	        if (freq.Known())
		    cycles += freq;
                ++stmts;
            }
	}
    } else if (OPERATOR_is_scf(opr)) {
	if (opr != OPR_BLOCK) {
	    /* blocks are counted by parent node */
	    ++bbs;
	    if (freq.Known())
	        cycles += freq;
	}
    } else if ((rtype == MTYPE_FQ || rtype == MTYPE_CQ) &&
	       OPERATOR_is_expression(opr) &&
	       !OPERATOR_is_load(opr) &&
	       !OPERATOR_is_leaf(opr) ) {
	/* quad operators get turned into calls */
	++bbs;
	if (freq.Known())
	    cycles += freq;
    } else if (opr == OPR_CAND || opr == OPR_CIOR) {
	/* these may get expanded to if-then-else sequences,
	 * or they may be optimized to logical expressions.
	 * use the halfway average of 1 bb */
	++bbs;
	if (freq.Known())
	    cycles += freq;
    }

}

// When feedback info is available, count the "effective" number of basic
// blocksand statements, i.e., those whose frequence is non-zero.
void
Count_tree_size (FEEDBACK& fb, WN *wn, INT32 &bbs, INT32 &stmts, FB_FREQ& cycles, FB_FREQ &freq_count)
{

  static BOOL init_invoke_seen = FALSE;
  static FB_FREQ init_invoke;

  if (!init_invoke_seen)
      init_invoke = freq_count;

#if (defined(_STANDALONE_INLINER) || defined(_LIGHTWEIGHT_INLINER))
  BOOL IPL_Enable_Unknown_Frequency = FALSE;
#endif // _STANDALONE_INLINER

  if (!freq_count.Known()) {
      if (!IPL_Enable_Unknown_Frequency) {
	  cycles = FB_FREQ_UNKNOWN;
	  DevWarn ("Unknown frequency found in Count_tree_size, this should never happen");
	  return;
      } else 
	  freq_count = init_invoke;
  }
	
  if (wn) {
    WN * wn2;

    switch (WN_operator(wn)) {

    case OPR_BLOCK:
      wn2 = WN_first(wn);
      while (wn2) {
	Count_tree_size(fb, wn2, bbs, stmts, cycles, freq_count);
	wn2 = WN_next(wn2);
      }
      break;

    case OPR_REGION:
      Count_tree_size(fb, WN_region_body(wn), bbs, stmts, cycles, freq_count);
      break;
      
    case OPR_IF:
      {
      Count_tree_size(fb, WN_if_test(wn), bbs, stmts, cycles, freq_count);
      FB_Info_Branch info_branch = fb.Query_branch( wn );
      if (!info_branch.freq_taken.Known() ||
	  !info_branch.freq_not_taken.Known()) {
	  if (!IPL_Enable_Unknown_Frequency) {
	      cycles = FB_FREQ_UNKNOWN;
	      DevWarn ("Unknown frequency found in IF, this should never happen");
	      return;
	  } else {
	      info_branch.freq_taken = init_invoke;
	      info_branch.freq_not_taken = init_invoke;
	  }
      }

      if (WN_then(wn)) {
	Count_tree_size(fb, WN_then(wn), bbs, stmts, cycles, info_branch.freq_taken);
      }
      if (WN_else(wn)) {
	Count_tree_size(fb, WN_else(wn), bbs, stmts, cycles, info_branch.freq_not_taken);
      }
      break;
      }

    case OPR_DO_LOOP:
      {
      FB_Info_Loop fb_info = fb.Query_loop( wn );
      if (!fb_info.freq_iterate.Known() ||
	  !fb_info.freq_exit.Known()) {
	  if (!IPL_Enable_Unknown_Frequency) {
	      cycles = FB_FREQ_UNKNOWN;
	      DevWarn ("Unknown frequency found in OPR_DO_LOOP, this should never happen");
	      return;
	  } else {
	      fb_info.freq_iterate = init_invoke;
	      fb_info.freq_exit = init_invoke;
	  }
      }
      Count_tree_size(fb, WN_start(wn), bbs, stmts, cycles, freq_count);
      Count_tree_size(fb, WN_step(wn), bbs, stmts, cycles, freq_count);
      Count_tree_size(fb, WN_end(wn), bbs, stmts, cycles, freq_count);
      Count_tree_size(fb, WN_do_body(wn), bbs, stmts, cycles, fb_info.freq_iterate);
      freq_count = fb_info.freq_exit;
      break;
      }

    case OPR_WHILE_DO:
    case OPR_DO_WHILE:
      {
      Count_tree_size(fb, WN_while_test(wn), bbs, stmts, cycles, freq_count);
      FB_Info_Loop fb_info = fb.Query_loop( wn );
      if (!fb_info.freq_iterate.Known() ||
	  !fb_info.freq_exit.Known()) {
	  if (!IPL_Enable_Unknown_Frequency) {
	      cycles = FB_FREQ_UNKNOWN;
	      DevWarn ("Unknown frequency found in OPR_WHILE_DO/OPR_DO_WHILE, this should never happen");
	      return;
	  } else {
	      fb_info.freq_iterate = init_invoke;
	      fb_info.freq_exit = init_invoke;
	  }
      }
      Count_tree_size(fb, WN_while_body(wn), bbs, stmts, cycles, fb_info.freq_iterate);
      freq_count = fb_info.freq_exit;
      break;
      }

    case OPR_SWITCH:
    case OPR_COMPGOTO:
    case OPR_XGOTO:
      {
      FB_Info_Switch fb_info =  fb.Query_switch( wn );
      WN *targ_blk = WN_kid1(wn);
      wn2 = WN_first(targ_blk);
      INT t = WN_num_entries(wn) - 1;
      for ( ; t >= 0; --t, wn2 = WN_next(wn2) ) {
          if (!fb_info[t].Known()) {
	      if (!IPL_Enable_Unknown_Frequency) {
		  cycles = FB_FREQ_UNKNOWN;
		  DevWarn ("Unknown frequency found in OPR_SWITCH/OPR_COMPGOTO/OPR_XGOTO, this should never happen");
		  return;
	      } else 
		  fb_info[t] = init_invoke;
	  }

          Count_tree_size(fb, wn2, bbs, stmts, cycles, fb_info[t]);
      }
      break;
      }

    case OPR_LABEL:
      {
      FB_Info_Invoke info_invoke = fb.Query_invoke( wn );
      if (!info_invoke.freq_invoke.Known()) {
	  if (!IPL_Enable_Unknown_Frequency) {
	      cycles = FB_FREQ_UNKNOWN;
	      DevWarn ("Unknown frequency found in OPR_LABEL, this should never happen");
	      return;
	  } else
	      info_invoke.freq_invoke = init_invoke;
      }

      freq_count = info_invoke.freq_invoke;
      break;
      }

    default: 
      {
      INT i;
      for (i = 0; i < WN_kid_count(wn); i++) {
	  wn2 = WN_kid(wn,i);
	  if (wn2) {
	      Count_tree_size(fb, wn2, bbs, stmts, cycles, freq_count);
	 }
      }
      }
    }

    count_stats (wn, bbs, stmts, cycles, freq_count);
  } 

} // Count_tree_size 

//INLINING_TUNING^
static void 
count_stats_tuning(WN *w, INT32& bbs, INT32& stmts, FB_FREQ& cycles, FB_FREQ freq, UINT32 &WNs, FB_FREQ &cycle_tuning)
{

    OPERATOR opr = OPCODE_operator(WN_opcode(w));

    TYPE_ID rtype = OPCODE_rtype(WN_opcode(w));

	if(freq.Known())
	  cycle_tuning += freq;

    /* count nscf stmts as bbs, not stmts */
    if (OPERATOR_is_non_scf(opr)) {
	++bbs;
	    if (freq.Known())
	        cycles += freq;
    } else if (OPERATOR_is_stmt(opr)) {
	if (OPERATOR_is_call(opr)) {
	    ++bbs;
	    if (freq.Known())
	        cycles += freq;
	} else if (opr == OPR_IO) {
	    ++bbs;
	    if (freq.Known())
	        cycles += freq;
	} else if (! OPERATOR_is_not_executable(opr)) {
	    ++stmts;
	    if (freq.Known())
	        cycles += freq;
	    if (MTYPE_is_complex(rtype) && OPERATOR_is_store(opr)) {
	        if (freq.Known())
		    cycles += freq;
                ++stmts;
            }
	}
    } else if (OPERATOR_is_scf(opr)) {
	if (opr != OPR_BLOCK) {
	    /* blocks are counted by parent node */
	    ++bbs;
	    if (freq.Known())
	        cycles += freq;
	}
    } else if ((rtype == MTYPE_FQ || rtype == MTYPE_CQ) &&
	       OPERATOR_is_expression(opr) &&
	       !OPERATOR_is_load(opr) &&
	       !OPERATOR_is_leaf(opr) ) {
	/* quad operators get turned into calls */
	++bbs;
	if (freq.Known())
	    cycles += freq;
    } else if (opr == OPR_CAND || opr == OPR_CIOR) {
	/* these may get expanded to if-then-else sequences,
	 * or they may be optimized to logical expressions.
	 * use the halfway average of 1 bb */
	++bbs;
	if (freq.Known())
	    cycles += freq;
    }

}

// When feedback info is available, count the "effective" number of basic
// blocksand statements, i.e., those whose frequence is non-zero.
void
Count_tree_size_tuning (FEEDBACK& fb, WN *wn, INT32 &bbs, INT32 &stmts, FB_FREQ& cycles, FB_FREQ &freq_count, UINT16 &WNs, FB_FREQ &cycle_tuning )
{

  static BOOL init_invoke_seen = FALSE;
  static FB_FREQ init_invoke;

  if (!init_invoke_seen)
      init_invoke = freq_count;

#if (defined(_STANDALONE_INLINER) || defined(_LIGHTWEIGHT_INLINER))
  BOOL IPL_Enable_Unknown_Frequency = FALSE;
#endif // _STANDALONE_INLINER

  if (!freq_count.Known()) {
      if (!IPL_Enable_Unknown_Frequency) {
	  cycles = FB_FREQ_UNKNOWN;
	  cycle_tuning = FB_FREQ_UNKNOWN;
	  DevWarn ("Unknown frequency found in Count_tree_size_tuning, this should never happen");
	  return;
      } else 
	  freq_count = init_invoke;
  }
	
  if (wn) {
    WN * wn2;

	WNs++;
	if(freq_count.Known())
		cycle_tuning += freq_count;

    switch (WN_operator(wn)) {

    case OPR_BLOCK:
      wn2 = WN_first(wn);
      while (wn2) {
	Count_tree_size_tuning(fb, wn2, bbs, stmts, cycles, freq_count, WNs, cycle_tuning);
	wn2 = WN_next(wn2);
      }
      break;

    case OPR_REGION:
      Count_tree_size_tuning(fb, WN_region_body(wn), bbs, stmts, cycles, freq_count, WNs, cycle_tuning);
      break;
      
    case OPR_IF:
      {
      Count_tree_size_tuning(fb, WN_if_test(wn), bbs, stmts, cycles, freq_count, WNs, cycle_tuning);
      FB_Info_Branch info_branch = fb.Query_branch( wn );
      if (!info_branch.freq_taken.Known() ||
	  !info_branch.freq_not_taken.Known()) {
	  if (!IPL_Enable_Unknown_Frequency) {
	      cycles = FB_FREQ_UNKNOWN;
	      cycle_tuning = FB_FREQ_UNKNOWN;
	      DevWarn ("Unknown frequency found in IF, this should never happen");
	      return;
	  } else {
	      info_branch.freq_taken = init_invoke;
	      info_branch.freq_not_taken = init_invoke;
	  }
      }

      if (WN_then(wn)) {
	Count_tree_size_tuning(fb, WN_then(wn), bbs, stmts, cycles, info_branch.freq_taken, WNs, cycle_tuning);
      }
      if (WN_else(wn)) {
	Count_tree_size_tuning(fb, WN_else(wn), bbs, stmts, cycles, info_branch.freq_not_taken, WNs, cycle_tuning);
      }
      break;
      }

    case OPR_DO_LOOP:
      {
      FB_Info_Loop fb_info = fb.Query_loop( wn );
      if (!fb_info.freq_iterate.Known() ||
	  !fb_info.freq_exit.Known()) {
	  if (!IPL_Enable_Unknown_Frequency) {
	      cycles = FB_FREQ_UNKNOWN;
	      cycle_tuning = FB_FREQ_UNKNOWN;
	      DevWarn ("Unknown frequency found in OPR_DO_LOOP, this should never happen");
	      return;
	  } else {
	      fb_info.freq_iterate = init_invoke;
	      fb_info.freq_exit = init_invoke;
	  }
      }
      Count_tree_size_tuning(fb, WN_start(wn), bbs, stmts, cycles, freq_count, WNs, cycle_tuning);
      Count_tree_size_tuning(fb, WN_step(wn), bbs, stmts, cycles, freq_count, WNs, cycle_tuning);
      Count_tree_size_tuning(fb, WN_end(wn), bbs, stmts, cycles, freq_count, WNs, cycle_tuning);
      Count_tree_size_tuning(fb, WN_do_body(wn), bbs, stmts, cycles, fb_info.freq_iterate, WNs, cycle_tuning);
      freq_count = fb_info.freq_exit;
      break;
      }

    case OPR_WHILE_DO:
    case OPR_DO_WHILE:
      {
      Count_tree_size_tuning(fb, WN_while_test(wn), bbs, stmts, cycles, freq_count, WNs, cycle_tuning);
      FB_Info_Loop fb_info = fb.Query_loop( wn );
      if (!fb_info.freq_iterate.Known() ||
	  !fb_info.freq_exit.Known()) {
	  if (!IPL_Enable_Unknown_Frequency) {
	      cycles = FB_FREQ_UNKNOWN;
	      cycle_tuning = FB_FREQ_UNKNOWN;
	      cycle_tuning = FB_FREQ_UNKNOWN;
	      DevWarn ("Unknown frequency found in OPR_WHILE_DO/OPR_DO_WHILE, this should never happen");
	      return;
	  } else {
	      fb_info.freq_iterate = init_invoke;
	      fb_info.freq_exit = init_invoke;
	  }
      }
      Count_tree_size_tuning(fb, WN_while_body(wn), bbs, stmts, cycles, fb_info.freq_iterate, WNs, cycle_tuning);
      freq_count = fb_info.freq_exit;
      break;
      }

    case OPR_SWITCH:
    case OPR_COMPGOTO:
    case OPR_XGOTO:
      {
      FB_Info_Switch fb_info =  fb.Query_switch( wn );
      WN *targ_blk = WN_kid1(wn);
      wn2 = WN_first(targ_blk);
      INT t = WN_num_entries(wn) - 1;
      for ( ; t >= 0; --t, wn2 = WN_next(wn2) ) {
          if (!fb_info[t].Known()) {
	      if (!IPL_Enable_Unknown_Frequency) {
		  cycles = FB_FREQ_UNKNOWN;
	      cycle_tuning = FB_FREQ_UNKNOWN;
		  DevWarn ("Unknown frequency found in OPR_SWITCH/OPR_COMPGOTO/OPR_XGOTO, this should never happen");
		  return;
	      } else 
		  fb_info[t] = init_invoke;
	  }

          Count_tree_size_tuning(fb, wn2, bbs, stmts, cycles, fb_info[t], WNs, cycle_tuning);
      }
      break;
      }

    case OPR_LABEL:
      {
      FB_Info_Invoke info_invoke = fb.Query_invoke( wn );
      if (!info_invoke.freq_invoke.Known()) {
	  if (!IPL_Enable_Unknown_Frequency) {
	      cycles = FB_FREQ_UNKNOWN;
	      cycle_tuning = FB_FREQ_UNKNOWN;
	      DevWarn ("Unknown frequency found in OPR_LABEL, this should never happen");
	      return;
	  } else
	      info_invoke.freq_invoke = init_invoke;
      }

      freq_count = info_invoke.freq_invoke;
      break;
      }

    default: 
    {
      INT i;
      for (i = 0; i < WN_kid_count(wn); i++) 
	  {
	    wn2 = WN_kid(wn,i);
	    if (wn2) 
		{
	      Count_tree_size_tuning(fb, wn2, bbs, stmts, cycles, freq_count, WNs, cycle_tuning);
	    }
      }
     }//default
    }//switch

    count_stats (wn, bbs, stmts, cycles, freq_count);
	
  } //if(wn)

} // Count_tree_size_tuning 

//INLINING_TUNING$
