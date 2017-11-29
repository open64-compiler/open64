/*
 * Copyright (C) 2010-2011 Advanced Micro Devices, Inc.  All Rights Reserved.
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
 * Module: ipl_analyze.cxx
 * $Source: /proj/osprey/CVS/open64/osprey1.0/ipa/local/ipl_analyze_template.h,v $
 *
 * Description:
 *	Functions for analyzing the procedures for the summary phase,
 *	includes jump function processing, mod/ref analysis, etc.
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef ipl_analyze_template_INCLUDED
#define ipl_analyze_template_INCLUDED

#ifndef ipl_summarize_util_INCLUDED
#include "ipl_summarize_util.h"
#endif // ipl_summarize_util_INCLUDED

#ifndef opt_base_INCLUDED
#include "opt_base.h"			// needed by opt_mu_chi.h
#endif

#ifndef opt_emit_INCLUDED
#include "opt_emit.h"			// for EMITTER class
#endif // opt_emit_INCLUDED

#ifndef opt_mu_chi_INCLUDED
#include "opt_mu_chi.h"			// for MU_NODE class
#endif // opt_mu_chi_INCLUDED

extern "C" {
#ifdef SHARED_BUILD
 // is replaced in ipa build but not for inliner
 void _ZN10DU_MANAGER14CD_is_br_takenEj(void) __attribute__ ((weak));
 void _ZN10DU_MANAGER15CD_is_fall_thruEj(void) __attribute__((weak));
 void _ZNK7CODEREP12Def_at_entryEv(void) __attribute__((weak));
 void _ZNK7CODEREP5DefbbEv(void) __attribute__((weak));
 void _ZN10DU_MANAGER8DominateEjj(void) __attribute__((weak));
 void _ZN10DU_MANAGER6Get_cdEj(void) __attribute__((weak));
 void _ZN10DU_MANAGER13Get_last_stmtEj(void) __attribute__((weak));
#endif

#ifdef BUILD_SKIP_IPA
 void _ZN10DU_MANAGER14CD_is_br_takenEj(void) {}
 void _ZN10DU_MANAGER15CD_is_fall_thruEj(void) {}
 void _ZNK7CODEREP12Def_at_entryEv(void) {}
 void _ZNK7CODEREP5DefbbEv(void) {}
 void _ZN10DU_MANAGER8DominateEjj(void) {}
 void _ZN10DU_MANAGER6Get_cdEj(void) {}
 void _ZN10DU_MANAGER13Get_last_stmtEj(void) {}
#endif
}

extern BOOL CXX_Alias_Const;


// ------------------------------------------------------------
// This function restores the indices into summary arrays for 
// VALUEs, EXPRs, PHIs and CHIs to the state recorded in the
// checkpoint. Furthermore, PHIs and CHIs that had already been
// entered into their respective hash tables must be deleted.
// ------------------------------------------------------------
template <PROGRAM program>
inline void
SUMMARIZE<program>::Restore_from_check_point (const SUMMARY_CHECK_POINT *cp)
{
    if (cp->value_idx () == -1)
	_value.Resetidx ();
    else
	_value.Setidx (cp->value_idx ());

    if (cp->expr_idx () == -1)
	_expr.Resetidx ();
    else
	_expr.Setidx (cp->expr_idx ());

    if (!DoPreopt)
	return;

    INT i;

    PHI_NODE_ARRAY::iterator first_phi =
	Hashed_Phis->begin () + cp->hashed_phi_idx();
    for (PHI_NODE_ARRAY::iterator last_phi = Hashed_Phis->end () - 1;
	 last_phi != first_phi; --last_phi) 
	Phi_To_Idx_Map->erase(*last_phi);
    Hashed_Phis->erase (first_phi + 1, Hashed_Phis->end ());

    INT phi_idx = cp->phi_idx();
    if (phi_idx == -1) {
	_phi.Resetidx ();
    }
    else {
	_phi.Setidx (phi_idx);
    }

#if Is_True_On
    for (first_phi = Hashed_Phis->begin (); first_phi != Hashed_Phis->end ();
	 ++first_phi) {
	PHI_NODE_TO_INT_MAP::const_iterator iter =
	    Phi_To_Idx_Map->find(*first_phi);
	if (iter != Phi_To_Idx_Map->end () && iter->second > phi_idx) {
	    Fail_FmtAssertion("Incomplete PHI restore: %d > %d is still in the map",
			      iter->second, phi_idx);
	}
    }
#endif    

    CHI_CR_ARRAY::iterator first =
	Hashed_Chis->begin () + cp->hashed_chi_idx();
    for (CHI_CR_ARRAY::iterator last = Hashed_Chis->end () - 1;
	 last != first; --last) {
	Chi_To_Idx_Map->erase(*last);
    }
    Hashed_Chis->erase (first + 1, Hashed_Chis->end ());

    INT chi_idx = cp->chi_idx();
    if (chi_idx == -1) {
	_chi.Resetidx ();
    }
    else {
	_chi.Setidx (chi_idx);
    }

#if Is_True_On
    for (first = Hashed_Chis->begin(); first != Hashed_Chis->end (); ++first) {
	CHI_CR_TO_INT_MAP::const_iterator iter = Chi_To_Idx_Map->find (*first);
	if (iter != Chi_To_Idx_Map->end () && iter->second > chi_idx)
	    Fail_FmtAssertion("Incomplete CHI restore: %d > %d is still in the map",
			      iter->second, chi_idx);
    }
#endif    

    Reset_cd_and_stmt (Summary_Map, cp->cd_idx (), cp->stmt_id ());

} // SUMMARIZE<program>::Restore_from_check_point

//-----------------------------------------------------------
// process constant actual parameters. Check if the tcon exists
// if it does then get the index into the tcon array, else
// store it in the hash table. Don't bother about string constants
// since their use in constant propagation is not clear
//-----------------------------------------------------------
template <PROGRAM program>
void
SUMMARIZE<program>::Process_constant_jump_function (WN* w, SUMMARY_VALUE *value)
{
    const ST* st = WN_st (w);
    value->Set_const_st ();
    value->Set_const_st_idx (ST_st_idx (st));
    TYPE_ID mtype = TY_mtype (ST_type (st));
    if (value->Is_addr_of ()) {
	value->Set_mtype (Pointer_type);
	value->Set_target_mtype (mtype);
    } else
	value->Set_mtype (mtype);

} // SUMMARIZE::Process_constant_jump_function


template <PROGRAM program>
void
SUMMARIZE<program>::Process_phi_operand (INT phi_index, WN *orig_wn,
					 CODEREP *cr, INT kid)
{
    WN *wn;

    if (cr->Def_at_entry () || cr->Is_flag_set (CF_DEF_BY_PHI) ||
	cr->Is_flag_set (CF_DEF_BY_CHI))
	wn = orig_wn;
    else {
	STMTREP *stmt = cr->Defstmt ();
	if (stmt == NULL)
	    return;
	else
	    wn = stmt->Wn ();
    }

    SUMMARY_DESC desc;
    Classify_const_value (desc, wn, cr);

    INT idx = Process_jump_function (&desc);

    SUMMARY_PHI *phi = Get_phi (phi_index);

    phi->Set_node_index (kid, idx);

    if (idx == -1)
	return;

    switch (desc.Get_type ()) {

    case VALUE_INT_CONST:
    case VALUE_CONST:
    case VALUE_FORMAL:
    case VALUE_GLOBAL:
	phi->Set_value (kid);
	break;

    case VALUE_EXPR:
	phi->Set_expr (kid);
	break;

    case VALUE_PHI:
	phi->Set_phi (kid);
	break;

    case VALUE_CHI:
	phi->Set_chi (kid);
	break;

    case VALUE_NOT_CONST:
    default:
	break;
    }

} // SUMMARIZE<program>::Process_phi_operand


template <PROGRAM program>
INT
SUMMARIZE<program>::Process_phi_jump_function (WN *orig_wn, PHI_NODE *phi)
{
    if (phi->Visited ())
	return -1;

    phi->Set_visited ();

    if (phi->Size () != 2) {
	DevWarn ("phi node has more than 2 elements");
	return -1;
    }

    CODEREP *cr0 = phi->OPND (0);
    CODEREP *cr1 = phi->OPND (1);
    if (cr0->Is_flag_set(CF_IS_ZERO_VERSION) ||
	cr1->Is_flag_set(CF_IS_ZERO_VERSION))
	return -1;

    // Check the hash table based on CODEREP/PHI_NODE pointers
    {
	PHI_NODE_TO_INT_MAP::const_iterator result =
	    Phi_To_Idx_Map->find (phi);
	if (result != Phi_To_Idx_Map->end () &&
	    result->second <= Get_phi_idx ()) {
	    if (result->second != -1)
		phi->Reset_visited ();
	    return result->second;
	}
    }
    
    struct DU_MANAGER *du = Get_du_mgr (); 

    IDTYPE cd0_bb_idx = cr0->Def_at_entry () ?
	du->Get_entry_bb () : du->Get_cd (cr0->Defbb()->Id());
    IDTYPE cd1_bb_idx = cr1->Def_at_entry () ?
	du->Get_entry_bb () : du->Get_cd (cr1->Defbb()->Id());
    IDTYPE merged_cd = -1;
    
    if (cd0_bb_idx == 0 || cd1_bb_idx == 0) 
	return -1;

    if (cd0_bb_idx == cd1_bb_idx) {
	if (du->CD_is_fall_thru (cr0->Defbb()->Id()) ==
	    du->CD_is_fall_thru (cr1->Defbb()->Id()))
	    return -1;			// infinite loop ?
	merged_cd = cd0_bb_idx;
    } else {
	if (du->Dominate (cd0_bb_idx, cd1_bb_idx)) {
	    if (!du->Dominate (cd1_bb_idx, phi->Bb()->Id()))
		return -1;
	    merged_cd = cd1_bb_idx;
	} else if (du->Dominate (cd1_bb_idx, cd0_bb_idx)) {
	    if (!du->Dominate (cd0_bb_idx, phi->Bb()->Id()))
		return -1;
	    merged_cd = cd0_bb_idx;
	}
    }

    if (merged_cd != -1) {
	const SUMMARY_CHECK_POINT chk_pt (this);
	INT ctrl_dep_idx = Process_cd_for_phi_node (merged_cd);

	if (ctrl_dep_idx == -1) {
	    Restore_from_check_point (&chk_pt);
	    Phi_To_Idx_Map->insert (std::make_pair (phi, -1));
	    return -1;
        }

	SUMMARY_PHI *phi_node = New_phi ();

	phi_node->Set_ctrl_dep_index (0, ctrl_dep_idx);
	phi_node->Set_ctrl_dep_index (1, ctrl_dep_idx);

	switch (WN_opcode (du->Get_last_stmt (merged_cd))) {
	case OPC_FALSEBR:
	case OPC_IF:
	    if (merged_cd == cd0_bb_idx) {
		phi_node->Set_branch (0,
				      du->CD_is_fall_thru(cr0->Defbb()->Id()));
		phi_node->Set_branch (1, !phi_node->Get_branch (0));
	    } else {
		phi_node->Set_branch (1,
				      du->CD_is_fall_thru(cr1->Defbb()->Id()));
		phi_node->Set_branch (0, !phi_node->Get_branch (1));
	    }
	    break;

	case OPC_TRUEBR:
	    if (merged_cd == cd0_bb_idx) {
		phi_node->Set_branch (0,
				      du->CD_is_br_taken(cr0->Defbb()->Id()));
		phi_node->Set_branch (1, !phi_node->Get_branch (0));
	    } else {
		phi_node->Set_branch (1,
				      du->CD_is_br_taken(cr1->Defbb()->Id()));
		phi_node->Set_branch (0, !phi_node->Get_branch (1));
	    }
	    break;
	}
	
	INT phi_idx = Get_phi_idx ();
	
	Process_phi_operand (phi_idx, orig_wn, cr0, 0);
	Process_phi_operand (phi_idx, orig_wn, cr1, 1);

	phi_node = Get_phi (phi_idx);
	if (phi_node->Get_node_index (0) == -1 &&
	    phi_node->Get_node_index (1) == -1) {
	    Restore_from_check_point (&chk_pt);
            Phi_To_Idx_Map->insert (std::make_pair (phi, -1));
	    return -1;
	} else {
            phi->Reset_visited ();
            Phi_To_Idx_Map->insert (std::make_pair (phi, phi_idx));
            Hashed_Phis->push_back (phi);
            return phi_idx;
	}
    }

    DevWarn ("phi node has 2 different control dep.\n");

    return -1;
} // SUMMARIZE<program>::Process_phi_jump_function


template <PROGRAM program>
INT
SUMMARIZE<program>::Process_chi_jump_function (WN *wn,
					       const SUMMARY_DESC &desc)
{

    CODEREP *cr = desc.Get_chi_cr ()->Defchi ()->OPND ();

    if (cr->Is_flag_set(CF_IS_ZERO_VERSION)) 
	return -1;

    STMTREP *stmt = desc.Get_chi_cr ()->Defstmt ();
    if (stmt == NULL)
	return -1;			// could be volatile

#ifdef KEY
    // bug 3121
    // We should not create a chi for const. Also, such a summary_chi
    // will have symbol_index == -1 (invalid). TODO: check if it is coming
    // from preopt, and fix it there.
    if (WN_st (wn) && ST_sym_class (*WN_st (wn)) == CLASS_CONST)
        return -1;
#endif // KEY

    
    // Check the hash table based on CODEREP/PHI_NODE pointers
    {
	CHI_CR_TO_INT_MAP::const_iterator result =
	    Chi_To_Idx_Map->find (desc.Get_chi_cr ());
	if (result != Chi_To_Idx_Map->end () &&
	    result->second <= Get_chi_idx ())
	    return result->second;
    }

    const SUMMARY_CHECK_POINT chk_pt (this);
    SUMMARY_CHI *chi_node = New_chi ();

    chi_node->Set_symbol_index (Get_symbol_index (WN_st (wn)));

    WN *call = stmt->Wn ();
    chi_node->Set_call_index (WN_MAP32_Get (Summary_Map, call) - 1);
    if (chi_node->Get_call_index () < 0) {
	Restore_from_check_point (&chk_pt);
	Chi_To_Idx_Map->insert (std::make_pair (desc.Get_chi_cr(), -1));
	return -1;
    }

    INT chi_idx = Get_chi_idx ();

    SUMMARY_DESC chi_desc;
    Classify_const_value (chi_desc, wn, cr);
    INT idx = Process_jump_function (&chi_desc);

    if (idx == -1) {
	Restore_from_check_point (&chk_pt);
	Chi_To_Idx_Map->insert (std::make_pair (desc.Get_chi_cr(), -1));
	return -1;
    }

    chi_node = Get_chi (chi_idx);

    chi_node->Set_node_index (idx);

    switch (chi_desc.Get_type ()) {
    case VALUE_INT_CONST:
    case VALUE_CONST:
    case VALUE_FORMAL:
    case VALUE_GLOBAL:
	chi_node->Set_chi_value ();
	break;

    case VALUE_EXPR:
	chi_node->Set_chi_expr ();
	break;

    case VALUE_PHI:
	chi_node->Set_chi_phi ();
	break;

    case VALUE_CHI:
	Is_True (idx != chi_idx, 
                 ("CHI: sym%dv%d (cr%d), OPND: sym%dv%d (cr%d)",
                  desc.Get_chi_cr()->Aux_id(),
                  desc.Get_chi_cr()->Version(),
                  desc.Get_chi_cr()->Coderep_id(),
                  cr->Aux_id(),
                  cr->Version(),
                  cr->Coderep_id()));
        
	chi_node->Set_chi_chi ();
	break;

    case VALUE_NOT_CONST:
    default:
	Restore_from_check_point (&chk_pt);
	Chi_To_Idx_Map->insert (std::make_pair (desc.Get_chi_cr(), -1));
	return -1;
    }

    // Enter coderep to chi_idx mapping into hash table
    // Since 0 means NOT FOUND, always add 2 to the index
    // before Enter, and subtract 2 from the result of Find
    // That way index -1 is entered as 1 (!= NOT FOUND).
    Chi_To_Idx_Map->insert (std::make_pair (desc.Get_chi_cr(), chi_idx));
    Hashed_Chis->push_back (desc.Get_chi_cr());

    return chi_idx;

} // SUMMARIZE<program>::Process_chi_jump_function


template <PROGRAM program>
void
SUMMARIZE<program>::Process_operand (WN *w, INT kid, INT expr_idx)
{
    SUMMARY_DESC desc;

    Classify_const_value (desc, w);

    INT idx = Process_jump_function (&desc);

    SUMMARY_EXPR *expr = Get_expr (expr_idx);

    if (idx == -1) {
	expr->Set_expr_unknown ();
	return;
    }

    expr->Set_node_index (kid, idx);
    
    switch (desc.Get_type ()) {

    case VALUE_INT_CONST:
    case VALUE_CONST:
    case VALUE_FORMAL:
    case VALUE_GLOBAL:
	expr->Set_expr_value (kid);
	break;

    case VALUE_EXPR:
	expr->Set_expr_expr (kid);
	break;
	
    case VALUE_PHI:
	expr->Set_expr_phi (kid);
	break;

    case VALUE_CHI:
	expr->Set_expr_chi (kid);
	break;

    case VALUE_NOT_CONST:
    default:
	expr->Set_expr_unknown ();
	break;
    }
} // SUMMARIZE<program>::Process_operand


template <PROGRAM program>
INT
SUMMARIZE<program>::Process_polynomial_jump_function (WN *w)
{
    OPCODE op = WN_opcode (w);

    if (!MTYPE_is_integral (OPCODE_rtype (op)))
	return -1;

    if (!OPCODE_is_expression(op))
	return -1;

    if (OPCODE_is_load(op))
	return -1;

    const SUMMARY_CHECK_POINT chk_pt (this);
    SUMMARY_EXPR *expr = New_expr ();
    expr->Set_expr_unknown ();
    INT expr_idx = Get_expr_idx ();

    if (OPCODE_nkids(op) == 2) {
	// binary operators

	expr->Set_mtype (OPCODE_rtype (op));
	expr->Set_opcode (op);

	if (WN_operator(WN_kid1(w)) == OPR_INTCONST) {
	    expr->Set_has_const_operand ();
	    expr->Set_kid (0);
	    expr->Set_const_value (WN_const_val(WN_kid1(w)));
	    Process_operand (WN_kid0(w), 0, expr_idx);
	} else if (WN_operator (WN_kid0(w)) == OPR_INTCONST) {
	    expr->Set_has_const_operand ();
	    expr->Set_kid (1);
	    expr->Set_const_value (WN_const_val(WN_kid0(w)));
	    Process_operand (WN_kid1(w), 1, expr_idx);
	} else {
	    /* both operands not constant */
	    expr->Clear_has_const_operand ();
	    Process_operand (WN_kid0(w), 0, expr_idx);
	    if (! Get_expr (expr_idx)->Is_expr_unknown ())
		Process_operand (WN_kid1(w), 1, expr_idx);
	}
    } else if (OPCODE_nkids(op) == 1) {
	// unary operators
	// treat it as a binary operator with const value zero.  The
	// evaluator will have to discard the const value based on the
	// opcode.

	// in the case where the opcode is a cvtl, use the const value
	// field for the conversion bits.

	expr->Set_mtype (OPCODE_rtype (op));
	expr->Set_opcode (op);
	expr->Set_has_const_operand ();
	if (OPCODE_operator (op) == OPR_CVTL)
	    expr->Set_const_value (WN_cvtl_bits (w));
	expr->Set_kid (0);
	Process_operand (WN_kid0(w), 0, expr_idx);
    }

    expr = Get_expr (expr_idx);
    if (expr->Is_expr_unknown ()) {
	Restore_from_check_point (&chk_pt);
	expr_idx = -1;
    } else {
	INT idx = entry_cache->Lookup (SUM_EXPR, expr);
	if (idx != -1 && idx < expr_idx) {
	    Restore_from_check_point (&chk_pt);
	    return idx;
	} else
	    entry_cache->Insert (SUM_EXPR, expr_idx);
    }

    return expr_idx;
} // SUMMARIZE<program>::Process_polynomial_jump_function


struct LOAD_STORE_ENTRY
{
  CODEREP* cr;
  ST* st;
  INT offset;
  TYPE_ID type : 8;
  BOOL is_ptr;
  BOOL ret_val;

  LOAD_STORE_ENTRY(CODEREP* c, ST* s, INT o, TYPE_ID t, BOOL p) : 
    cr(c), st(s), offset(o), type(t), is_ptr(p) {}

  bool operator== (const LOAD_STORE_ENTRY& rhs) {
    return (cr == rhs.cr && 
            st == rhs.st && 
            offset == rhs.offset && 
            type == rhs.type &&
            is_ptr == rhs.is_ptr);
  }
};

template <PROGRAM program>
BOOL
SUMMARIZE<program>::Mismatched_load_store (CODEREP *cr, BOOL is_ptr_var,
					   ST *st, INT load_offset,
					   TYPE_ID load_type)
{
    if (cr == NULL || cr->Is_flag_set (CF_IS_ZERO_VERSION))
	return TRUE;

    // cache for previosuly seen LOAD_STORE_ENTRY's
    static vector<LOAD_STORE_ENTRY> depot;
    static INT proc_idx = -1;
    if (proc_idx != Get_procedure_idx()) {
        depot.clear();
        proc_idx = Get_procedure_idx();
    }
 
    // look up the entry in the cache
    INT idx; 
    LOAD_STORE_ENTRY ldst(cr, st, load_offset, load_type, is_ptr_var);
    for (idx = 0; idx < depot.size(); idx++) {
        if (depot[idx] == ldst) {
            return depot[idx].ret_val;
        }
    }
    depot.push_back(ldst);

    if (cr->Is_flag_set (CF_DEF_BY_CHI)) {
	if (cr->Def_at_entry ()) {
            depot[idx].ret_val = is_ptr_var;
	    return is_ptr_var;
        }
        // Trace back CHI's created by calls
	STMTREP *stmt = cr->Defstmt ();
	if (stmt == NULL || WN_operator (stmt->Wn ()) != OPR_CALL) {
            depot[idx].ret_val = TRUE;
            return TRUE;
        }
#ifndef KEY
        depot[idx].ret_val =
          Mismatched_load_store (cr->Defchi()->OPND(), is_ptr_var, 
                                 st, load_offset, load_type);
#else
// The following code looks same as above, but depot is a vector which would
// be changed in the callee. So depot[idx] calculated before the call can 
// be invalid after the callee returns. So separate it out.
	BOOL r = 
          Mismatched_load_store (cr->Defchi()->OPND(), is_ptr_var, 
                                 st, load_offset, load_type);
        depot[idx].ret_val = r;
#endif // !KEY
        return depot[idx].ret_val;
    }
    
    if (cr->Is_flag_set (CF_DEF_BY_PHI)) {
	PHI_NODE *phi = cr->Defphi ();

	if (!phi->Live() || phi->Visited ()) {
            depot[idx].ret_val = TRUE;
	    return TRUE;
        }
	phi->Set_visited ();

	for (INT i = 0; i < phi->Size (); i++) {
	    cr = phi->OPND (i);
	    if (Mismatched_load_store (cr, is_ptr_var, st, load_offset,
				       load_type)) {
		phi->Reset_visited ();
                depot[idx].ret_val = TRUE;
		return TRUE;
	    }
	}

	phi->Reset_visited ();
        depot[idx].ret_val = FALSE;
	return FALSE;
	
    } else {
	STMTREP *stmt = cr->Defstmt ();

	if (stmt == NULL) {
            depot[idx].ret_val = FALSE;
	    return FALSE;
        }
	WN *def = stmt->Wn ();

	if (!is_ptr_var) {
	    depot[idx].ret_val = !(WN_operator (def) == OPR_STID &&
		                   WN_st (def) == st);
            return depot[idx].ret_val;
        }
	else if (WN_operator (def) == OPR_ISTORE) {
	    WN *kid1 = WN_kid1(def);
	    if (!OPCODE_has_sym (WN_opcode (kid1))) {
                depot[idx].ret_val = TRUE;
		return TRUE;
            }
	    if (WN_st(kid1) != st || load_offset != WN_store_offset(def) ||
		load_type != WN_desc(def)) {
                depot[idx].ret_val = TRUE;
		return TRUE;
            }
            depot[idx].ret_val = FALSE;
	    return FALSE;
	} else {
            depot[idx].ret_val = TRUE;
	    return TRUE;
        }
    }
} // SUMMARIZE<program>::Mismatched_load_store


template <PROGRAM program>
void
SUMMARIZE<program>::Classify_indirect (SUMMARY_DESC &result, WN *w)
{
    Is_True (WN_operator (w) == OPR_ILOAD || WN_operator (w) == OPR_PARM,
	     ("Invalid opcode"));
    
    WN *parm = w;
    w = WN_kid0(parm);
    result.Set_wn (w);

    BOOL is_ptr_variable = WN_operator (w) != OPR_LDA;

    if (!OPCODE_has_sym (WN_opcode (w)))
	return;

    ST* st = WN_st (w);

    BOOL is_iload;
    INT load_offset; 

    if (WN_operator (parm) == OPR_ILOAD) {
	is_iload = TRUE;
	load_offset = WN_load_offset(parm);
	if (is_ptr_variable)
	    return;
	else
	    result.Set_target_mtype (TY_mtype (ST_type (st)));
    } else {
	is_iload = FALSE;
	load_offset = 0;
	if (!is_ptr_variable) {
	    result.Set_is_addr_of ();
	    result.Set_target_mtype (TY_mtype (ST_type (st)));
	    if (ST_level (st) == GLOBAL_SYMTAB) {
		switch (ST_sclass (st)) {
		case SCLASS_FSTATIC:
		case SCLASS_COMMON:
		case SCLASS_EXTERN:
		case SCLASS_UGLOBAL:
		case SCLASS_DGLOBAL:
		case SCLASS_TEXT:
		    result.Set_convertible_to_global ();
		}
	    }
	}
    }
				 


    switch (TY_kind(ST_type(st))) {
    case KIND_SCALAR:
#ifdef KEY // bug 6229
    case KIND_ARRAY:
#endif
	if (ST_class(st) == CLASS_CONST) {
	    // symbolic constant (e.g. floating point constant)
	    result.Set_type (VALUE_CONST);
	    return;
	}

#ifdef KEY // bug 7718
	if (TY_kind(ST_type(st)) == KIND_ARRAY) return;
#endif
	break;
    case KIND_POINTER:
	if (is_ptr_variable)
	    break;
	// fall through
    default:
	return;
    }
    

    if (!DoPreopt)
	return;

    CODEREP *cr = (CODEREP *)
	WN_MAP_Get (*(Get_emitter ()->Wn_to_cr_map ()), parm);  

    if (cr == NULL)
	// this might mean parm is within an IO statement
	return;

    cr = cr->Ivar_mu_node ()->OPND ();

    if (cr->Kind() != CK_VAR)
	return;

    if (cr->Is_flag_set (CF_DEF_BY_CHI)) {
	if (cr->Def_at_entry ()) {
	    if (is_ptr_variable)
		return;
		    
	    switch (ST_sclass (st)) {
	    case SCLASS_FORMAL:
	    case SCLASS_FORMAL_REF:
		if (ST_level (st) == CURRENT_SYMTAB) // ignore up-level ref.
		    result.Set_type (VALUE_FORMAL);
		break;
	    case SCLASS_FSTATIC:
	    case SCLASS_DGLOBAL:
	    case SCLASS_UGLOBAL:
	    case SCLASS_COMMON:
	    case SCLASS_EXTERN:
		result.Set_type (VALUE_GLOBAL);
		break;
	    default:
		// probably a local variable, ignored
		break;
	    }
            return;
	}

	// ignore the chi node (we can't handle two-level of indirection
	// E.g.
	//		*p = <expr>
	//		p = foo (...)
	//		use *p
	// we don't have a mechanism to check if the call to foo modifies *p.
	
	// handle the chi node if it is created by a call
	STMTREP *stmt = cr->Defstmt ();
	if (stmt == NULL ||
            WN_operator (stmt->Wn ()) != OPR_CALL)
	    return;

	// now, we have a may-modify situation by a function call
	result.Set_type (VALUE_CHI);
	result.Set_chi_cr (cr);
	return;
    }

    if (Mismatched_load_store (cr, is_ptr_variable, st, load_offset,
			       is_iload ? WN_desc(parm) :
			       ST_btype(st)))
	return;

    if (cr->Is_flag_set (CF_DEF_BY_PHI)) {
	Classify_const_value (result, w, cr);
	return;
    }

    STMTREP *stmt = cr->Defstmt ();
    if (stmt == NULL)
	return;
    
    WN *def = stmt->Wn ();
    cr = stmt->Rhs ();

    if (!is_ptr_variable)
	result.Reset_convertible_to_global ();
    
    Classify_const_value (result, WN_kid0(def), cr);
    
} // SUMMARIZE<program>::Classify_indirect


template <PROGRAM program>
void
SUMMARIZE<program>::Classify_const_value (SUMMARY_DESC &result, WN *w,
					  CODEREP *cr)
{
    result.Set_wn (w);

    if (cr == NULL || cr->Is_flag_set (CF_IS_ZERO_VERSION))
	return;

    switch (cr->Kind ()) {
    case CK_LDA:
	if (cr->Offset () == 0) {
	    ST *st = cr->Lda_base_st ();
	    result.Set_is_addr_of ();
	    result.Set_target_mtype (TY_mtype (ST_type (st)));
	    switch (ST_sclass (st)) {
	    case SCLASS_TEXT:
		if (ST_level (st) != GLOBAL_SYMTAB) {
		    result.Set_type (VALUE_SYMBOL);
		    break;
		}
		// else fall through 
	    case SCLASS_FSTATIC:
	    case SCLASS_DGLOBAL:
	    case SCLASS_UGLOBAL:
	    case SCLASS_COMMON:
	    case SCLASS_EXTERN:
		result.Set_convertible_to_global ();
		result.Set_type (VALUE_GLOBAL);
		break;
	    case SCLASS_FORMAL:
	    case SCLASS_FORMAL_REF:
		if (ST_level (st) == CURRENT_SYMTAB) // ignore up-level ref.
		    result.Set_type (VALUE_FORMAL);
		break;
		
	    case SCLASS_AUTO:
		if (ST_level (st) == CURRENT_SYMTAB)
		    result.Set_type (VALUE_SYMBOL);
		break;
	    }
	}
	return;
	
    case CK_CONST:
	result.Set_type (VALUE_INT_CONST);
	return;

    case CK_RCONST:
	result.Set_type (VALUE_CONST);
	return;
	
    case CK_VAR:
	break;
	
    case CK_IVAR:
	return;				// should be indirect processing

    case CK_OP:
	result.Set_type (VALUE_EXPR);
	return;

    default:
	return;
    }

    if (cr->Is_flag_set (CF_DEF_BY_PHI)) {
	PHI_NODE *phi = cr->Defphi ();

	if (!phi->Live() || phi->Visited ())
	    return;

	phi->Set_visited ();
	
	cr = phi->OPND (0);
	BOOL same_phi = TRUE;
	
	// check if all the phi operands are the same
	for (INT i = 1; i < phi->Size(); i++) {
	    if (cr != phi->OPND(i)) {
		same_phi = FALSE;
		break;
	    }
	}

	if (!same_phi) {
	    result.Set_type (VALUE_PHI);
	    result.Set_phi (phi);
	    phi->Reset_visited ();
	    return;
	}

	// phi has only one unique element, treated as simple assignment

	Classify_const_value (result, w, cr);
	phi->Reset_visited ();
	return;
    }

    if (cr->Is_flag_set (CF_DEF_BY_CHI)) {
	if (cr->Def_at_entry ()) {
	    const ST* st = WN_st (w);
	    switch (ST_sclass (st)) {
	    case SCLASS_FSTATIC:
	    case SCLASS_DGLOBAL:
	    case SCLASS_UGLOBAL:
	    case SCLASS_COMMON:
	    case SCLASS_EXTERN:
		result.Set_type (VALUE_GLOBAL);
		break;
	    case SCLASS_FORMAL:
	    case SCLASS_FORMAL_REF:
		if (ST_level (st) == CURRENT_SYMTAB)
		    result.Set_type (VALUE_FORMAL);
		break;
	    default:
		break;
	    }
	    return;
	}

	// handle the chi node if it is created by call
	STMTREP *stmt = cr->Defstmt ();
	if (stmt == NULL ||
            WN_operator (stmt->Wn ()) != OPR_CALL)
            return;

	// now, we have a may-modify situation by a function call
	result.Set_type (VALUE_CHI);
	result.Set_chi_cr (cr);
	return;
    }

    // This is the definition, follow the chain

    STMTREP *stmt = cr->Defstmt ();

    if (stmt == NULL)
	return;

    w = stmt->Wn ();
    cr = stmt->Rhs ();
    if (WN_operator (w) == OPR_STID)
	Classify_const_value (result, WN_kid0(w), cr);
} // SUMMARIZE<program>::Classify_const_value


template <PROGRAM program>
void
SUMMARIZE<program>::Classify_const_value (SUMMARY_DESC &result, WN *w)
{
    result.Set_wn (w);
    const ST* st;

    switch (WN_operator (w)) {
    case OPR_ILOAD:
    case OPR_PARM:
	Classify_indirect (result, w);
	return;

    case OPR_INTCONST:
	result.Set_type (VALUE_INT_CONST);
	return;

    case OPR_LDA:
	if (WN_lda_offset (w) != 0)
	    return;
	
	st = WN_st (w);
	result.Set_is_addr_of ();
	result.Set_target_mtype (TY_mtype (ST_type (st)));
	switch (ST_sclass (st)) {
	case SCLASS_TEXT:
	    if (ST_level (st) != GLOBAL_SYMTAB) {
		result.Set_type (VALUE_SYMBOL);
		break;
	    }
	    // else fall through 
	case SCLASS_FSTATIC:
	case SCLASS_DGLOBAL:
	case SCLASS_UGLOBAL:
	case SCLASS_COMMON:
	case SCLASS_EXTERN:
	    // seema:
	    result.Set_type (VALUE_GLOBAL);
	    result.Set_convertible_to_global ();
	    break;
	case SCLASS_FORMAL:
	case SCLASS_FORMAL_REF:
	    if (ST_level (st) == CURRENT_SYMTAB)
		result.Set_type (VALUE_FORMAL);
	    break;
	    
	case SCLASS_AUTO:
	    if (ST_level (st) == CURRENT_SYMTAB)
		result.Set_type (VALUE_SYMBOL);
	    break;
	}
	return;

    default:
	if (!OPCODE_has_sym (WN_opcode (w))) {
	    // expression
	    result.Set_type (VALUE_EXPR);
	    return;
	}
	break;
    }

    st = WN_st (w);
    switch (TY_kind(ST_type(st))) {
    case KIND_SCALAR:
#ifdef KEY // bug 6229
    case KIND_ARRAY:
#endif
    case KIND_POINTER:
	break;
    default:
	return;
    }

    if (ST_class(st) == CLASS_CONST) {
	// symbolic constant (e.g. floating point constant)
	result.Set_type (VALUE_CONST);
	return;
    }

#ifdef KEY // bug 7718
    if (TY_kind(ST_type(st)) == KIND_ARRAY) return;
#endif

    // by now, it can only be a variable

    switch (ST_sclass(st)) {
    case SCLASS_FSTATIC:
    case SCLASS_DGLOBAL:
    case SCLASS_UGLOBAL:
    case SCLASS_COMMON:
    case SCLASS_EXTERN:
    case SCLASS_FORMAL:
    case SCLASS_FORMAL_REF:
    case SCLASS_AUTO:
	if (!DoPreopt)
	    // if preopt is not used then we have to be very conservative
	    break;

	CODEREP *cr = (CODEREP *)
	    WN_MAP_Get (*(Get_emitter()->Wn_to_cr_map ()), w);

	Classify_const_value (result, w, cr);
	break;
    }

    return;
} // SUMMARIZE<program>::Classify_const_value


template <PROGRAM program>
INT
SUMMARIZE<program>::Process_jump_function (SUMMARY_DESC *desc)
{
    SUMMARY_VALUE *value = NULL;
    SUMMARY_SYMBOL *sym;
    INT idx;

    WN *w = desc->Get_wn ();
    
    switch (desc->Get_type ()) {

    case VALUE_INT_CONST:
	if (WN_rtype (w) == MTYPE_I4 &&
	    (WN_const_val(w) == 0 || WN_const_val(w) == 1))
	    return WN_const_val(w);
	
	value = New_value ();
	value->Set_mtype (WN_rtype (w));
	value->Set_int_const ();
	value->Set_int_const_value (WN_const_val (w));

	break;

    case VALUE_EXPR:
	return Process_polynomial_jump_function (w);

    case VALUE_CONST:
	value = New_value ();
	Process_constant_jump_function (w, value);
	break;

    case VALUE_PHI:
	return Process_phi_jump_function (w, desc->Get_phi ());

    case VALUE_CHI:
	idx = Process_chi_jump_function (w, *desc);
	if (idx >= 0)
	    return idx;

        // nenad, 02/23/98:
        // This looks very suspicious; in the IPA phase it will
        // be impossible to know which CHIs are real and which are
        // just crude approximations. This works now because 
        // VALUE_GLOBALs are used only when proven completely
        // constant, but it may cause problems when more precise
        // analysis is attempted. For example, elements of common
        // blocks will also appear as VALUE_GLOBALs, but imprecise 
        // CHIs must not be used for them, since they participate
        // in value expression evaluation during IPA cprop.

	/* if we cannot determine the chi node, and if the symbol is a
	   global variable, we should just record the symbol because CGI
	   may find out that it is a constant after all */
	
	switch (ST_sclass (WN_st (w))) {
	case SCLASS_FSTATIC:
	case SCLASS_DGLOBAL:
	case SCLASS_UGLOBAL:
	case SCLASS_COMMON:
	case SCLASS_EXTERN:
	    break;
            // break out and fall through down to VALUE_GLOBAL
	default:
	    return idx;
	}

	desc->Set_type (VALUE_GLOBAL);
	// fall through

    case VALUE_GLOBAL:
	value = New_value ();
	if (desc->Is_addr_of ()) {
	    value->Set_is_addr_of ();
	    value->Set_target_mtype (desc->Target_mtype ());
	    if (desc->Is_convertible_to_global ())
		value->Set_convertible_to_global ();
	}
	value->Set_mtype (WN_rtype (w));
	value->Set_global ();
        {
          const ST* st = WN_st (w);
          INT32 idx = Get_symbol_index (st);
          value->Set_global_index (idx);
          if (idx == -1)
	  {
            value->Set_global_st_idx (ST_st_idx (st));
#ifdef KEY
            value->Set_is_global_st_idx ();
#endif
	  }
        }
	break;

    case VALUE_FORMAL:
	value = New_value ();
	if (desc->Is_addr_of ()) {
	    value->Set_is_addr_of ();
	    value->Set_target_mtype (desc->Target_mtype ());
	}
	value->Set_mtype (WN_rtype (w));
	value->Set_formal ();
	sym = Get_symbol (Get_symbol_index (WN_st(w)));
	value->Set_formal_index (sym->Get_findex ());
	break;

    case VALUE_SYMBOL:
	value = New_value ();
	if (desc->Is_addr_of ()) {
	    value->Set_is_addr_of ();
	    value->Set_target_mtype (desc->Target_mtype ());
	}
	value->Set_mtype (WN_rtype (w));
	value->Set_symbol ();
	value->Set_symbol_index (Get_symbol_index (WN_st(w)));
	break;

    default:
	return -1;
    }

    // should only reach here if a new SUMMARY_VALUE is created
    Is_True (value != NULL, ("Uninitialized SUMMARY_VALUE pointer\n"));

    // check for duplicates
    idx = entry_cache->Lookup (SUM_VALUE, value);
    if (idx != -1 && idx < Get_value_idx ())
	_value.Decidx ();
    else {
	idx = Get_value_idx ();
	entry_cache->Insert (SUM_VALUE, idx);
    }
    return idx;

} // SUMMARIZE<program>::Process_jump_function


template <PROGRAM program>
void
SUMMARIZE<program>::Process_jump_function (WN *w, INT value_idx)
{
    INT idx;
    SUMMARY_SYMBOL *sym;
    SUMMARY_DESC summary_desc;

    Classify_const_value (summary_desc, w);

    SUMMARY_VALUE *value = Get_value (value_idx);
    value->Set_mtype (WN_rtype (w));

    if (summary_desc.Is_addr_of ()) {
	value->Set_is_addr_of ();
	value->Set_target_mtype (summary_desc.Target_mtype ());
	if (summary_desc.Is_convertible_to_global ())
	    value->Set_convertible_to_global ();
    }

    w = summary_desc.Get_wn ();
    switch (summary_desc.Get_type ()) {
    case VALUE_INT_CONST:
	value->Set_int_const();
	value->Set_int_const_value (WN_const_val(w));
	break;

    case VALUE_EXPR:
	value->Set_expr ();

	idx = Process_polynomial_jump_function (w);

	if (idx == -1)
	    Get_value (value_idx)->Set_not_const ();
	else
	    Get_value (value_idx)->Set_expr_index (idx);

	break;
	
    case VALUE_CONST:
	Process_constant_jump_function (w, value);
	break;

    case VALUE_PHI:
	value->Set_phi ();
	idx = Process_phi_jump_function (w, summary_desc.Get_phi ());
	if (idx == -1)
	    Get_value (value_idx)->Set_not_const ();
	else
	    Get_value (value_idx)->Set_phi_index (idx);
	break;
	
    case VALUE_CHI:
	value->Set_chi ();
	idx = Process_chi_jump_function (w, summary_desc);
	if (idx != -1) {
	    Get_value (value_idx)->Set_chi_index (idx);
	    break;
	}

        // See nenad's comment for the similar function above.

	/* if we cannot determine the chi node, and if the symbol is a
	   global variable, we should just record the symbol because CGI
	   may find out that it is a constant after all */
	
	switch (ST_sclass (WN_st (w))) {
	case SCLASS_FSTATIC:
	case SCLASS_DGLOBAL:
	case SCLASS_UGLOBAL:
	case SCLASS_COMMON:
	case SCLASS_EXTERN:
	    break;
	    // break out and fall through down to VALUE_GLOBAL
	default:
	    Get_value (value_idx)->Set_not_const ();
	    return;
	}

	// fall through
	
    case VALUE_GLOBAL:
	value->Set_global ();
	{
	    const ST* st = WN_st (w);
	    INT idx = Get_symbol_index (st);
	    value->Set_global_index (idx);
	    if (idx == -1)
	    {
		value->Set_global_st_idx (ST_st_idx (st));
#ifdef KEY
		value->Set_is_global_st_idx ();
#endif
	    }
	}
	break;

    case VALUE_FORMAL:
	sym = Get_symbol (Get_symbol_index (WN_st(w)));
	value->Set_formal ();
	value->Set_formal_index (sym->Get_findex ());
	break;

    case VALUE_SYMBOL:
	value->Set_symbol ();
	value->Set_symbol_index (Get_symbol_index (WN_st(w)));
	break;

    case VALUE_NOT_CONST:
    default:
	value->Set_not_const ();
	break;
    }

} // SUMMARIZE<program>::Process_jump_function


//-----------------------------------------------------------
// record variables that have been referenced
// corresponds to an LDID, ILOAD, or LDA
//-----------------------------------------------------------

template <PROGRAM program>
void
SUMMARIZE<program>:: Record_global_ref (WN* w, ST *s, OPERATOR op, BOOL refcount_only) 
{
    SUMMARY_GLOBAL *global = NULL;

#ifdef KEY
    FmtAssert((WN_operator(w) == OPR_LDID) ||
	      (WN_operator(w) == OPR_ILOAD) ||
	      (WN_operator(w) == OPR_LDA) ||
	      (WN_operator(w) == OPR_PRAGMA),
	      ("Expecting LDID/ILOAD/LDA/PRAGMA in Record_Global_Ref "));

    // don't bother with constants and register variables
    if ((ST_class(s) == CLASS_CONST) ||
	(ST_class(s) == CLASS_PREG)) {
	return;
    }
#else
    FmtAssert((WN_operator(w) == OPR_LDID) ||
	      (WN_operator(w) == OPR_ILOAD) ||
	      (WN_operator(w) == OPR_LDA),
	      ("Expecting OPR_LDID/OPR_ILOAD/OPR_LDA in Record_Global_Ref "));


    // don't bother with constants and register variables
    if ((ST_class(WN_st(w)) == CLASS_CONST) ||
	(ST_class(WN_st(w)) == CLASS_PREG)) {
	return;
    }
#endif

    if ( Trace_Modref ) {
	fprintf ( TFile, " global %s referenced", ST_name(s) );
    }

    INT index = Global_hash_table->Find(s);
    
    if (index == 0) {
	/* This is the first time we see this symbol in the procedure */
	global = New_global ();
	global->Set_symbol_index (Get_symbol_index(s));
	Global_hash_table->Enter (s, Get_global_idx() + 1);
    } else
	global = Get_global (index - 1);

    global->Inc_refcount ();

    if (refcount_only)
	return;

    switch (op) {
    case OPR_LDID:
	global->Set_dref();
	break;
    case OPR_ILOAD:
#ifdef KEY
        if (IPA_Enable_Pure_Call_Opt)
          Get_procedure (Get_procedure_idx())->Set_has_side_effect();
#endif
	global->Set_iref();
	break;
    case OPR_LDA:
	global->Set_aref();
	break;
    }
} // SUMMARIZE::Record_global_ref


// ====================================================================
//
// Record_ref_formal
//
// Record ref information for formal scalar parameters.
//
// An LDID is a direct ref, a LOAD is an indirect mod.
//
// TODO: Modification of an array is always classified as a direct mod.
//
// NOTE: For now even array parameters are treated as scalars, and
// there is imod information for only one level of indirection.  If
// there are more levels of indirection,, this routine is conservative.
//
// ====================================================================

template <PROGRAM program>
void
SUMMARIZE<program>:: Record_ref_formal ( WN* w )
{
    INT i;
    WN* w2;
    WN *parent_w;

    // don't bother with constants and register variables
    if (OPCODE_has_sym (WN_opcode (w)) && (ST_class(WN_st(w)) == CLASS_PREG))
	return;

    switch ( WN_operator(w) ) {

    case OPR_ILOAD:
#ifdef KEY
        if (IPA_Enable_Pure_Call_Opt)
          Get_procedure (Get_procedure_idx())->Set_has_side_effect();
#endif
	switch ( WN_operator(WN_kid0(w)) ) {
	case OPR_ARRAY:
	    w2 = WN_array_base(WN_kid0(w));

	    if (!OPCODE_has_sym(WN_opcode(w2))) {
		return;
	    }
	    // later: for array case don't set dref
	    i = Get_symbol_index(WN_st(w2));
	    Get_symbol (i)->Set_dref ();
	    if ( Trace_CopyProp )
		fprintf ( TFile, "\n  formal: %s : set dref",
			 ST_name(WN_st(w2)) );
	    break;

	default:
	    w2 = WN_kid0(w);
	    i = Get_symbol_index(WN_st(w2));
	    Get_symbol (i)->Set_iref();
	    
	    if ( Trace_CopyProp ) 
		fprintf ( TFile, "\n  formal: %s : set iref\n",
			 ST_name(WN_st(w2)) );
	    break;
	}
	break;

    case OPR_LDID:

	i = Get_symbol_index(WN_st(w));
	// for STID of (LDID w) into a preg set _cdref_preg_only 
	
	parent_w = LWN_Get_Parent(w);
	
	if (IsStidToReturnPreg(parent_w,Get_entry_point()) &&
	    !(Get_symbol(i)->Is_dref()) && !(Get_symbol(i)->Is_cref()))
	  {
	    Get_symbol (i)-> Set_cdref_preg_only();
	    if ( Trace_CopyProp )
	       fprintf ( TFile, "\n formal: %s : set cdref_preg_only\n",
			 ST_name(WN_st(w)));
         }

	// in the case of FORMAL_REF, and ldid really refers to an
	// indirect reference
	if (ST_sclass(WN_st(w)) == SCLASS_FORMAL_REF ||
	    WN_operator (parent_w) == OPR_ICALL)
	    Get_symbol (i)->Set_iref ();
	else {
	    Get_symbol (i)->Set_dref();
	    if ( Trace_CopyProp ) 
		fprintf ( TFile, "\n  formal: %s : set dref\n",
			 ST_name(WN_st(w)) );
	    }
	break;

    case OPR_LDA:
      // For ASM_INPUT, set dref to the symbol
      i = Get_symbol_index(WN_st(w));
      parent_w = LWN_Get_Parent(w);
      OPERATOR opr = parent_w ?
	WN_operator(parent_w) : (OPERATOR) 0;
      if (opr == OPR_ASM_INPUT) {
	// in the case of FORMAL_REF, and ldid really refers to an
	// indirect reference
	if (ST_sclass(WN_st(w)) == SCLASS_FORMAL_REF ||
	    WN_operator (parent_w) == OPR_ICALL)
	  Get_symbol (i)->Set_iref ();
	else {
	  Get_symbol (i)->Set_dref();
	  if ( Trace_CopyProp )
	    fprintf ( TFile, "\n  formal: %s : set dref\n",
		      ST_name(WN_st(w)) );
	}
      }
      else {
	Get_symbol (i)->Set_aref();
	if ( Trace_CopyProp )
	  fprintf ( TFile, "\n  formal: %s : set aref\n",
		    ST_name(WN_st(w)) );
      }
      break;
    }

    if ( Trace_Modref ) {
	if (WN_operator(w) == OPR_ILOAD)
	  	fprintf ( TFile, " formal %s referenced", ST_name(WN_st(w2)) );
	else
	  	fprintf ( TFile, " formal %s referenced", ST_name(WN_st(w)) );
    }

} // SUMMARIZE::Record_ref_formal


template <PROGRAM program>
void
SUMMARIZE<program>:: Record_ref_all_formal ( WN* w, BOOL parm_store )
{
    INT i,j;
    WN* w2;
    WN *parent_w;
    WN *grandparent_w;
    const ST *st;
    const ST *st2;
    TY *proctype;
    TY *arg_type;
    TYLIST *argtypes;

    if (parm_store) {
	WN* parent_w = LWN_Get_Parent(w);
	OPERATOR opr = parent_w ?
	    WN_operator(parent_w) : (OPERATOR) 0;
	if (opr == OPR_ISTORE && WN_kid0(parent_w) != w) {
	    if (WN_operator(w) == OPR_LDID)
		// check if the opcode of the kid is LDID. Since
		// that means it is *p
		return;
	}
    }
    
    st = WN_st(w);
    // don't bother with constants and register variables
    if (ST_class(st) == CLASS_PREG)
	return;

    switch ( WN_operator(w) ) {
    case OPR_ILOAD:
#ifdef KEY
        if (IPA_Enable_Pure_Call_Opt)
          Get_procedure (Get_procedure_idx())->Set_has_side_effect();
#endif
	switch ( WN_operator(WN_kid0(w)) ) {
	case OPR_ARRAY:
	    w2 = WN_array_base(WN_kid0(w));
	    st2 = WN_st(w2);
	    if (!OPCODE_has_sym(WN_opcode(w2))) {
		return;
	    }
	    
	    // later: for array case don't set cref
	    i = Get_symbol_index(st2);
	    Get_symbol (i)->Set_cref ();

	    if (program == INLINER) {
		j = Get_symbol_crefcount_index(i);
		Get_symbol_crefcount(j)->Incr_cur_cref_count();
		if ( Trace_CopyProp ) 
		    fprintf ( TFile, "\n  formal: %s : cref count: %d\n",
			      ST_name(st2),
			      Get_symbol_crefcount(j)->Get_cur_cref_count());
	    }

	    if ( Trace_CopyProp ) 
		fprintf ( TFile, "\n  formal: %s : set cref",
			 ST_name(st2));
	    break;

	    // this is equivalent to ref of *p, in that case we don't record
	    // the cref
	default:
	    w2 = WN_kid0(w);
	    st2 = WN_st(w2);
	    if (WN_operator(w2) == OPR_LDID)
		return;
	    else if (OPCODE_has_sym(WN_opcode(w2))) {
		i = Get_symbol_index(st2);
		Get_symbol (i)->Set_cref ();

		if (program == INLINER) {
		    j = Get_symbol_crefcount_index(i);
		    Get_symbol_crefcount(j)->Incr_cur_cref_count();
		    if ( Trace_CopyProp ) 
			fprintf ( TFile, "\n  formal: %s : cref count: %d\n",
				  ST_name(st2),
				  Get_symbol_crefcount(j)->Get_cur_cref_count());
		}

		if ( Trace_CopyProp ) 
		    fprintf ( TFile, "\n  formal: %s : set cref",
			      ST_name(st2));
	    }
	    break;
	}
	break;

    case OPR_LDID:
	i = Get_symbol_index(st);
	// for STID of (LDID w) into a preg set _cdref_preg_only 
	parent_w = LWN_Get_Parent(w);

	if (IsStidToReturnPreg(parent_w,Get_entry_point()) &&
	    !(Get_symbol(i)->Is_cref()))
	  {
	    Get_symbol (i)-> Set_cdref_preg_only();
	    if ( Trace_CopyProp )
	       fprintf ( TFile, "\n  formal: %s : set cdref_preg_only\n",
			 ST_name(st));
	}
	Get_symbol (i)->Set_cref ();

	if (program == INLINER) {
	    j = Get_symbol_crefcount_index(i);
	    Get_symbol_crefcount(j)->Incr_cur_cref_count();
	    if ( Trace_CopyProp ) 
		fprintf ( TFile, "\n  formal: %s : cref count: %d\n",
			  ST_name(st),
			  Get_symbol_crefcount(j)->Get_cur_cref_count());
	}
	

	if ( Trace_CopyProp ) 
	    fprintf ( TFile, "\n  formal: %s : set cref", ST_name(st) );
	break;

    case OPR_LDA:
	i = Get_symbol_index(st);
	Get_symbol (i)->Set_cref ();

	if (program == INLINER) {
	    j = Get_symbol_crefcount_index(i);
	    Get_symbol_crefcount(j)->Incr_cur_cref_count();
	    if ( Trace_CopyProp ) 
		fprintf ( TFile, "\n  formal: %s : cref count: %d\n",
			  ST_name(st),
			  Get_symbol_crefcount(j)->Get_cur_cref_count());
	}

	if ( Trace_CopyProp ) 
	    fprintf ( TFile, "\n  formal: %s : set cref", ST_name(st) );
	break;
    }
} // SUMMARIZE::Record_ref_all_formal



//-----------------------------------------------------------
// record global variables that have been referenced inside ILOAD's
//-----------------------------------------------------------
template <PROGRAM program>
void
SUMMARIZE<program>:: Check_kid_ref (WN* w)
{
    INT i;

    for (i = 0; i < WN_kid_count(w); i++) {
	WN *wn2;
	wn2 = WN_kid(w, i);
	Check_kid_ref (wn2);
	switch (WN_operator(wn2)) {

	case OPR_LDID:
	case OPR_LDA:
	case OPR_ILOAD:
	    Record_ref (wn2);
	    break;
	default:
	    break;
	}
    }
} // SUMMARIZE::Check_kid_ref


static void
Record_unknown_memory_op (WN* wn)
{
  if (WN_operator(wn) == OPR_ARRAY) {
    WN* base = WN_array_base(wn);
    OPERATOR base_opr = WN_operator(base);
    if (OPERATOR_has_sym(base_opr)) {
      if (base_opr == OPR_LDA) {
        return;
      }
      ST* base_st = WN_st(base);        
      switch (ST_sclass(base_st)) {
        case SCLASS_FORMAL:
        case SCLASS_FORMAL_REF: 
        case SCLASS_AUTO:
        case SCLASS_PSTATIC: {
          TY_IDX ty = ST_type(base_st);
          if (TY_kind(ty) == KIND_ARRAY ||
              (TY_kind(ty) == KIND_POINTER && 
               TY_kind(TY_pointed(ty)) == KIND_ARRAY)) {
            return;
          }
        }
      }
    }
  }
  
  // we didn't recongnize memory location as known
  Summary->Get_procedure(Summary->Get_procedure_idx())->
    Set_has_incomplete_array_info();
}


//-----------------------------------------------------------
// record global variables that have been referenced
//-----------------------------------------------------------
template <PROGRAM program>
void
SUMMARIZE<program>:: Record_ref (WN *w)
{
    ST* s;
    WN* w2;
    BOOL refcount_only = FALSE;
    BOOL Inliner_copy_prop = (program == INLINER && INLINE_Enable_Copy_Prop);

    if ( Trace_Modref )
	fprintf (TFile, "<mr> Record_Ref -- %s:", OPCODE_name(WN_opcode(w)) );

#ifdef KEY
    // Covers all ILOADs.
    if (WN_operator (w) == OPR_ILOAD)
      if (IPA_Enable_Pure_Call_Opt)
        Get_procedure (Get_procedure_idx())->Set_has_side_effect();
#endif // KEY

    // note, we should NOT be recording actual parameters as being
    // referenced (seema).

    // note, we should also NOT be recording the children of OPR_ISTORE
    // as being referenced! (seema)

    WN* parent_w = LWN_Get_Parent(w);
    OPERATOR opr = parent_w ?
	WN_operator(parent_w) : (OPERATOR) 0;
    if (opr == OPR_PARM || (opr == OPR_ISTORE && WN_kid0(parent_w) != w)) {
	refcount_only = TRUE;
    } else if (opr == OPR_ARRAY && WN_kid0(parent_w) == w &&
	       WN_has_sym(w) && TY_kind(ST_type(WN_st(w))) == KIND_ARRAY) {
	/* we don't want to count a ref if it is just a store to an array,
	   or it is a load from an array that's already counted via the
	   ILOAD */
	WN *grandparent_w = LWN_Get_Parent (parent_w);
	opr = grandparent_w ?
	    WN_operator(grandparent_w) : (OPERATOR) 0;
	if ((opr == OPR_ISTORE && WN_kid1(grandparent_w) == parent_w) ||
	    (opr == OPR_ILOAD && WN_kid0(grandparent_w) == parent_w))
	    return;
    }


    if (OPCODE_has_sym (WN_opcode (w))) {
	s = WN_st (w);
	if (ST_st_idx (s) != ST_base_idx (s) && !ST_is_weak_symbol (s))
	    s = ST_base (s);
    } else
	s = NULL;

#ifdef KEY
    // Consider any access of globals
    if (s)
      switch (ST_sclass(s))
      {
        case SCLASS_EXTERN:
	case SCLASS_UGLOBAL:
	case SCLASS_DGLOBAL:
	case SCLASS_FSTATIC:
	case SCLASS_COMMON:
          if (IPA_Enable_Pure_Call_Opt)
            Get_procedure (Get_procedure_idx())->Set_has_side_effect();
	  break;

	default:
	  break;
      }
#endif // KEY

    switch (WN_operator(w)) {

	// to handle direct ref and ref and aref of global variables

    case OPR_LDID:

#ifdef KEY
	{
	  TY_IDX st_type = ST_type(s);
	  TY_IDX wn_type = WN_ty(w);
	  if (st_type != wn_type /* type-cast */ &&
	      TY_kind(st_type) == KIND_POINTER) {

	    TY_IDX pst_type = TY_pointed(st_type);
	    if (TY_kind(pst_type) == KIND_STRUCT)
	      Record_ty_info_for_type (pst_type, TY_NO_SPLIT);

	    if (TY_kind(wn_type) == KIND_POINTER &&
	        TY_kind(TY_pointed(wn_type)) == KIND_STRUCT)
	      Record_ty_info_for_type (TY_pointed(wn_type), TY_NO_SPLIT);
	  }
	}
#endif
	parent_w = LWN_Get_Parent(w); 
	// don't record this as a direct ref since it is actually an
	// indirect ref that was captured by OPR_ILOAD
	if (parent_w && WN_operator(parent_w) == OPR_ILOAD)
	    return;


	switch(ST_sclass(s)) {

	case SCLASS_EXTERN:
	case SCLASS_UGLOBAL:
	case SCLASS_DGLOBAL:
	case SCLASS_FSTATIC:
	case SCLASS_COMMON:

	    if (Inliner_copy_prop)
		break;
	    opr = (WN_operator (parent_w) == OPR_ICALL) ?
		OPR_ILOAD : OPR_LDID;
	    Record_global_ref (w, s, opr, refcount_only);
	    break;


        case SCLASS_FORMAL_REF:
	    if (Inliner_copy_prop)
		break;
	    
	case SCLASS_FORMAL:
	    if ( ! refcount_only )
		Record_ref_formal ( w );
	    Record_ref_all_formal (w, refcount_only);
	    break;

	default:
	    break;
	}
	break;

    case OPR_LDA:
#ifdef KEY
	{
	  TY_IDX type = WN_type(w);
	  if (TY_kind(type) == KIND_STRUCT)
	    Record_ty_info_for_type (type, TY_NO_SPLIT);
	}
#endif
	if (Inliner_copy_prop)
	    break;
	
	switch ( ST_sclass(s) ) {
	case SCLASS_EXTERN:
	case SCLASS_UGLOBAL:
	case SCLASS_DGLOBAL:
	case SCLASS_FSTATIC:
	case SCLASS_COMMON:

	    Record_global_ref ( w, s, OPR_LDA, refcount_only ); 
	    break;

	    
	default:
	  if ( opr == OPR_ASM_INPUT)
	    Record_ref_formal (w);
	  break;
	}
	break;

    case OPR_ILOAD:
    case OPR_MLOAD:
        Record_unknown_memory_op(WN_kid0(w));
	switch ( WN_operator(WN_kid0(w)) ) {
	case OPR_LDID:
	case OPR_ARRAY:
	    if (WN_operator(WN_kid0(w)) == OPR_LDID) {
		w2 = WN_kid0(w);
	    }
	    else {
		w2 = WN_array_base(WN_kid0(w));
	    }

	    // cases where the ref is not known
	    if (!OPCODE_has_sym(WN_opcode(w2))) {
		return;
	    }

	    s = WN_st(w2);
	    if (ST_st_idx (s) != ST_base_idx (s) && !ST_is_weak_symbol (s))
		s = ST_base (s);
	    switch(ST_sclass(s)) {
	    case SCLASS_COMMON:
		if (Inliner_copy_prop)
		    break;
		Record_global_ref (w2, s, OPR_ILOAD, refcount_only);
		break;


	    case SCLASS_EXTERN:
	    case SCLASS_UGLOBAL:
	    case SCLASS_DGLOBAL:
	    case SCLASS_FSTATIC:
		if (Inliner_copy_prop)
		    break;
		Record_global_ref (w2, s, OPR_ILOAD, refcount_only); 
		break;

	    case SCLASS_FORMAL_REF:
		if (Inliner_copy_prop)
		    break;

	    case SCLASS_FORMAL:
		if ( ! refcount_only )
		    Record_ref_formal ( w );
		break;

	    default:
		if (Trace_Modref || (Trace_CopyProp && Inliner_copy_prop)) {
		    fprintf ( TFile, "ignoring ILOAD->LDID/ARRAY\n");
		    Print_ST ( TFile, WN_st(w2), 0 );
		}
		break;
	    }
	    break;

	default:
	    Check_kid_ref (WN_kid0(w));
	    break;
	}
	break;
    }

    if ( Trace_Modref ) {
	fprintf ( TFile, "\n" );
    }
} // SUMMARIZE::Record_ref


//-----------------------------------------------------------
// record variables that have been directly modified
// corresponds to an STID
//-----------------------------------------------------------
template <PROGRAM program>
void
SUMMARIZE<program>::Record_global_dmod (const WN* w, const WN *rhs,
					const ST *s)
{
    SUMMARY_GLOBAL *global = NULL;

    // don't bother with constants and register variables
    // We should not assert here because the user program might contain
    // code that modifies a constant (esp. under -cckr). See PV 592390.
    if ((ST_class(WN_st(w)) == CLASS_CONST) ||
	(ST_class(WN_st(w)) == CLASS_PREG))
	return;

    if ( Trace_Modref ) {
	fprintf ( TFile, " global %s modified", ST_name(WN_st(w)) );
    }

#ifdef KEY
    if (IPA_Enable_Pure_Call_Opt)
      Get_procedure (Get_procedure_idx())->Set_has_side_effect();
#endif

    INT index = Global_hash_table->Find (s);

    if (index == 0) {
	/* This is the first time we see this symbol in the procedure */
	global = New_global ();
	global->Set_symbol_index (Get_symbol_index(s));
	Global_hash_table->Enter (s, Get_global_idx() + 1);
    } else
	global = Get_global (index - 1);

    global->Set_dmod();
    Inc_modcount (global, Get_symbol (global->Get_symbol_index()), rhs);

} // SUMMARIZE::Record_global_dmod


// ====================================================================
//
// Record_mod_formal
//
// Record mod information for formal scalar parameters.
//
// A STID is a direct mod, a STORE is an indirect mod.
//
// TODO: Modification of an array is always classified as a direct mod.
//
// NOTE: For now even array parameters are treated as scalars, and
// there is imod information for only one level of indirection.  If
// there are more levels of indirection,, this routine is conservative.
//
// ====================================================================

template <PROGRAM program>
void
SUMMARIZE<program>:: Record_mod_formal ( WN* w )
{
    INT i;
    WN* w2;

#ifdef KEY
    if (IPA_Enable_Pure_Call_Opt)
    {
      ST * tmp_st = WN_st (w);
      if (ST_st_idx (tmp_st) != ST_base_idx (tmp_st) && 
          !ST_is_weak_symbol (tmp_st))
	tmp_st = ST_base (tmp_st);
      if (ST_sclass (tmp_st) == SCLASS_FORMAL_REF)
        Get_procedure (Get_procedure_idx())->Set_has_side_effect();
    }
#endif

    switch ( WN_operator(w) ) {

    case OPR_ISTORE:
    case OPR_MSTORE: // OSP_418, Both kid1 of MSTORE and ISTORE are the address
	switch ( WN_operator(WN_kid1(w)) ) {
	case OPR_ARRAY:
	    // check to see if the base of the array is an LDA or an
	    // LDID. If it is an LDA then it is a direct mod else it
	    // is an indirect mod
	    w2 = WN_array_base(WN_kid1(w));

	    if (!OPCODE_has_sym(WN_opcode(w2))) {
		return;
	    }
	    
	    switch (WN_operator(w2)) {
	    case OPR_LDID:
		
		i = Get_symbol_index(WN_st(w2));
		Get_symbol (i)->Set_imod ();
		if ( Trace_CopyProp ) 
		    fprintf ( TFile, "\n  formal: %s : set imod\n",
			     ST_name(WN_st(w2)) );
		break;

	    case OPR_LDA:
		i = Get_symbol_index(WN_st(w2));
		Get_symbol (i)->Set_dmod ();
		
		if ( Trace_CopyProp ) 
		    fprintf ( TFile, "\n  formal: %s : set dmod\n",
			     ST_name(WN_st(w2)) );
		break;

	    default:
		Fail_FmtAssertion("unsupported array base address opcode \n");
		break;
	    }
	    break;
	    
	default:
	    
	    w2 = WN_kid1(w);
	    if (OPCODE_has_sym(WN_opcode(w2)) && WN_st(w2)) {
		i = Get_symbol_index(WN_st(w2));
		Get_symbol (i)->Set_imod ();
		if ( Trace_CopyProp ) 
		    fprintf ( TFile, "\n  formal: %s : set imod\n",
			     ST_name(WN_st(w2)) );
	    } 
	    break;
	}
	break;
	
    case OPR_STID:
	i = Get_symbol_index(WN_st(w));
	// an stid of SCLASS_FORMAL_REF really refers to an direct mod
	if (ST_sclass(WN_st(w)) == SCLASS_FORMAL_REF) {
	  // Get_symbol (i)->Set_dmod ();
	   Get_symbol (i)->Set_imod();
	} else {
	    Get_symbol (i)->Set_dmod ();
	    if ( Trace_CopyProp ) 
		fprintf ( TFile, "\n  formal: %s : set dmod\n",
			 ST_name(WN_st(w)) );
	}
        w2 = w; // needed for trace
	break;
    }
    
    if ( Trace_Modref ) {
	fprintf ( TFile, " formal %s modified", ST_name(WN_st(w2)) );
    }
} // SUMMARIZE::Record_mod_formal


//-------------------------------------------------------------------
// record indirect mod for common block variables: input is [IM]STORE
//-------------------------------------------------------------------
template <PROGRAM program>
void 
SUMMARIZE<program>:: Record_mod_common (WN *w, const ST *st)
{
  WN* dest = WN_kid1(w);
  if (WN_operator(dest) == OPR_ARRAY) {
    dest = WN_array_base(dest);
  }
  if (!OPERATOR_has_sym(WN_operator(dest))) {
    return;
  }

#ifdef KEY
  if (IPA_Enable_Pure_Call_Opt)
    Get_procedure (Get_procedure_idx())->Set_has_side_effect();
#endif

  SUMMARY_GLOBAL* global;
  INT index = Global_hash_table->Find(st);

  if (index == 0) {
    global = New_global();
    global->Set_symbol_index(Get_symbol_index(st));
    Global_hash_table->Enter(st, Get_global_idx() + 1);
  } 
  else {
    global = Get_global(index - 1);
  }
  
  global->Set_dmod();
  global->Inc_modcount();
  Get_symbol(global->Get_symbol_index())->Set_modcount();

} // SUMMARIZE::Record_mod_common
          
// It is the responsibility of the caller to ensure "w" has ty_idx
// information.
static TY_IDX
get_access_type (WN * w)
{
  Is_True (OPERATOR_has_1ty(WN_operator(w)) ||
           OPERATOR_has_2ty(WN_operator(w)),
           ("get_access_type needs WN with ty_idx information"));
  TY_IDX w_type = WN_ty(w);

  if (WN_field_id(w) == 0)
    return w_type;

  UINT cur_field_id = 0;
  FLD_HANDLE fld = FLD_get_to_field (w_type, WN_field_id(w), cur_field_id);

  return FLD_type(fld);
}

//-----------------------------------------------------------
// record variables that have been modified
// note the current language of the procedure
// for fortran do the following: record stids for formal
// parameters, and global variables
//-----------------------------------------------------------
template <PROGRAM program>
void 
SUMMARIZE<program>:: Record_mod (WN* w)
{
    if ( Trace_Modref ) {
	fprintf ( TFile, "<mr> Record_Mod -- %s:", OPCODE_name(WN_opcode(w)) );
    }


    const ST* st;
    const WN* w2;

#ifdef KEY
    // Consider all ISTOREs
    if (WN_operator (w) == OPR_ISTORE)
      if (IPA_Enable_Pure_Call_Opt)
        Get_procedure (Get_procedure_idx())->Set_has_side_effect();
#endif // KEY

    switch (PU_src_lang (Get_Current_PU ())) {
    case PU_C_LANG:
    case PU_CXX_LANG:

	switch (WN_operator (w)) {
	case OPR_ISTORE:
	case OPR_MSTORE:
            Record_unknown_memory_op(WN_kid1(w));
	    w2 = get_mod_target (WN_kid1(w));
	    if (w2 && OPCODE_has_sym(WN_opcode(w2))) {
		st = WN_st(w2);
		if (ST_base_idx (st) != ST_st_idx (st) &&
		    !ST_is_weak_symbol (st))
		    st = ST_base (st);
		
		switch (ST_sclass(st)) {

		case SCLASS_COMMON:
		case SCLASS_EXTERN:
		case SCLASS_UGLOBAL:
		case SCLASS_DGLOBAL:
		case SCLASS_FSTATIC:
		    Record_global_dmod (w2, WN_kid0(w), st);
		    break;
#ifdef KEY
                default:
    		    if (IPA_Enable_Pure_Call_Opt)
      		      Get_procedure (Get_procedure_idx())->Set_has_side_effect();
#endif
		}
	    } else {
		// can't find the mod target, record as an indirect mod
		w2 = WN_kid1(w);
		if (OPCODE_has_sym(WN_opcode(w2))) {
		    if (st = WN_st (w2)) {
			INT i = Get_symbol_index(st);
			Get_symbol (i)->Set_imod ();
			if ( Trace_CopyProp ) 
			    fprintf ( TFile, "\n  formal: %s : set imod\n",
				      ST_name(st) );
		    }
		}
	    }

	    break;

	case OPR_STID:
	    // record stids and stores for formals and globals
	    st = WN_st (w);
	    if (ST_st_idx (st) != ST_base_idx (st) && !ST_is_weak_symbol (st))
		st = ST_base (st);
#ifdef KEY
	    // TODO: Find the actual base from the potentially complex
	    // rhs expression.
	    if (WN_operator(WN_kid0(w)) == OPR_LDID) {

	      TY_IDX rhs_ty = get_access_type(WN_kid0(w));
	      TY_IDX w_ty = get_access_type(w);

	      if (rhs_ty != w_ty) {

	        if (TY_kind(rhs_ty) == KIND_POINTER &&
	            TY_kind(TY_pointed(rhs_ty)) == KIND_STRUCT)
	          Record_ty_info_for_type (TY_pointed(rhs_ty), TY_NO_SPLIT);

	        if (TY_kind(w_ty) == KIND_POINTER &&
	            TY_kind(TY_pointed(w_ty)) == KIND_STRUCT)
	          Record_ty_info_for_type (TY_pointed(w_ty), TY_NO_SPLIT);
	      }
	    }
#endif
	    
	    switch ( ST_sclass(st) ) {
	    case SCLASS_AUTO:
	    case SCLASS_FORMAL:
	    case SCLASS_FORMAL_REF:
		Record_mod_formal (w);
		break;

	    case SCLASS_EXTERN:
	    case SCLASS_UGLOBAL:
	    case SCLASS_DGLOBAL:
	    case SCLASS_FSTATIC:
	    case SCLASS_COMMON:
		if (DoPreopt &&
		    Is_Value_Restored (w, w,
				       *(Get_emitter ()->Wn_to_cr_map ())))
		    break;

		Record_global_dmod (w, WN_kid0(w), WN_st(w));
		break;

	    }
	}
	break;

    case PU_F77_LANG:
    case PU_F90_LANG:
	switch ( WN_operator(w) ) {
	    // to handle direct mod and mod of formal parameters

	case OPR_STID:
	    st = WN_st (w);
	    if (ST_st_idx (st) != ST_base_idx (st) && !ST_is_weak_symbol (st))
		st = ST_base (st);
	    
	    switch ( ST_sclass(st) ) {
	    case SCLASS_EXTERN:
	    case SCLASS_UGLOBAL:
	    case SCLASS_DGLOBAL:
	    case SCLASS_FSTATIC:
	    case SCLASS_COMMON:
		Record_global_dmod(w, WN_kid0(w), st); // done
		break;

	    case SCLASS_FORMAL:
	    case SCLASS_FORMAL_REF:
		Record_mod_formal (w); // done
		break;

	    default:
		break;
	    }
	    break;

	case OPR_ISTORE:
	case OPR_MSTORE:
            Record_unknown_memory_op(WN_kid1(w));
	    switch ( WN_operator(WN_kid1(w)) ) {
	    case OPR_LDID:
	    case OPR_ARRAY:
	    case OPR_LDA:
		if (WN_operator(WN_kid1(w)) == OPR_ARRAY)
		    w2 = WN_array_base(WN_kid1(w));
		else
		    w2 = WN_kid1(w);

		if (!OPCODE_has_sym(WN_opcode(w2))) {
		    return;
		}

		st = WN_st(w2);

		switch(ST_sclass(st)) {
		case SCLASS_COMMON:
		    if (ST_st_idx (st) != ST_base_idx (st) &&
		        	!ST_is_weak_symbol (st))
		    	st = ST_base (st);
		
		    Record_mod_common(w, st); // done
		    break;      
		    
		case SCLASS_DGLOBAL:
		    if (ST_st_idx (st) != ST_base_idx (st) &&
		        	!ST_is_weak_symbol (st)) {  
			// Must be COMMON turned DGLOBAL due to DATA stmt
		    	st = ST_base (st);
		        Record_mod_common(w, st); // done
		    }
		    break;      
		
		case SCLASS_EXTERN:
		case SCLASS_UGLOBAL:
		case SCLASS_FSTATIC:
		    break;
		    
		case SCLASS_FORMAL:
 	        case SCLASS_FORMAL_REF:
		    Record_mod_formal (w); // done
		    break;

		default:
		    if ( Trace_Modref ) {
			fprintf ( TFile, " ignoring ISTORE->LDID/ARRAY\n");
			Print_ST ( TFile, st, 0 );
		    }
		    break;
		}
		break;

	    }
	}
    }

    if ( Trace_Modref ) {
	fprintf ( TFile, "\n" );
    }
} // SUMMARIZE::Record_mod 



//----------------------------------------------------------------------
// Routines for handling all the control dependence analysis
//----------------------------------------------------------------------

template <PROGRAM program>
INT
SUMMARIZE<program>::Process_cd_for_phi_node (IDTYPE cd_bb_idx)
{
    SUMMARY_CONTROL_DEPENDENCE *cd;
    struct DU_MANAGER *du = Get_du_mgr (); 

    if (cd_bb_idx == du->Get_entry_bb ())
	return -1;

    WN *cond_stmt = du->Get_last_stmt (cd_bb_idx);

#ifdef KEY
    // Bug 9110: WOPT may have inserted a dummy edge to represent the
    // control-dependence for OpenMP single pragma, which does not have
    // corresponding WN.
    if (!cond_stmt)
      return -1;
#endif
    switch (WN_opcode (cond_stmt)) {
    case OPC_TRUEBR:
    case OPC_FALSEBR:
    case OPC_IF:
	break;

    default:
	return -1;
    }

    if (WN_MAP32_Get (Summary_Map, cond_stmt)) {
	// we've seen this stmt before
	cd = Get_cd_by_idx (WN_MAP32_Get (Summary_Map, cond_stmt) - 1);
	return Get_cd_idx (cd);
    }

    cd = Get_new_cd ();
    INT cd_idx = Get_cd_idx (cd);
    cd->Set_if_stmt ();
    cd->Set_wn (cond_stmt);
    WN_MAP32_Set (Summary_Map, cond_stmt, cd_idx + 1);

    INT expr_idx = Process_polynomial_jump_function (WN_kid0(cond_stmt));

    cd = Get_cd_by_idx (cd_idx);
    cd->Set_expr_index (expr_idx);
	

    if (!Process_control_dependence (cond_stmt, cd_idx))
	Set_cd_head_of_chain (cd_idx);

    return cd_idx;
	
} // SUMMARIZE<program>::Process_cd_for_phi_node


template <PROGRAM program>  
BOOL
SUMMARIZE<program>::Process_control_dependence (WN *w, INT node_index)
{
    if (w == Get_entry_point())
      return TRUE;

    WN* stmt_node = w;
    while (stmt_node != NULL && 
           WN_opcode(LWN_Get_Parent(stmt_node)) != OPC_BLOCK && 
           !OPCODE_is_scf(WN_opcode(stmt_node)))
      stmt_node = LWN_Get_Parent (stmt_node);

    struct DU_MANAGER *du = Get_du_mgr();
    IDTYPE bb_idx = du->Get_bb_id(stmt_node);
    FmtAssert(bb_idx != 0, 
              ("Process_control_dependence: Could not find bb_idx"));

    IDTYPE cd_bb_idx = du->Get_cd(bb_idx);
    WN* cond_stmt;
    BOOL ctrl_dep_on_entry = FALSE;

    if (cd_bb_idx == du->Get_entry_bb()) {
      // always executed
      ctrl_dep_on_entry = TRUE;
      cond_stmt = Get_entry_point();
    } 
    else {
      if (cd_bb_idx == 0) 
	return FALSE;
      cond_stmt = du->Get_last_stmt(cd_bb_idx);
#ifdef KEY
      // Bug 9088: WOPT may have inserted a dummy edge to represent the
      // control-dependence for OpenMP single pragma, which does not have
      // corresponding WN.
      if (!cond_stmt)
        return FALSE;
#endif
      switch (WN_opcode(cond_stmt)) {
      case OPC_TRUEBR:
      case OPC_FALSEBR:
      case OPC_IF:
      case OPC_DO_LOOP:
	break;
      default:
	return FALSE;
      }
    } 

    SUMMARY_CONTROL_DEPENDENCE* cd;
    BOOL new_cd_created = FALSE;
    INT cd_idx = WN_MAP32_Get (Summary_Map, cond_stmt) - 1;
    if (!(cd_idx >= 0 && cd_idx <= Get_max_cd_idx ())) {
      cd = Get_new_cd ();
      cd_idx = Get_cd_idx (cd);

      WN_MAP32_Set (Summary_Map, cond_stmt, cd_idx + 1);
      cd->Set_wn (cond_stmt);

      if (ctrl_dep_on_entry)
        cd->Set_entry ();
      else if (WN_opcode (cond_stmt) != OPC_DO_LOOP) {
        cd->Set_if_stmt ();
	    
	// Now, get the conditional expression
        INT expr_idx = Process_polynomial_jump_function (WN_kid0(cond_stmt));
        cd = Get_cd_by_idx (cd_idx);
        cd->Set_expr_index (expr_idx);
      } 
      else
        cd->Set_do_loop ();
	
      new_cd_created = TRUE;
    }

    BOOL branch = TRUE;
    SUMMARY_STMT* stmt;
    switch (WN_opcode (cond_stmt)) {
    case OPC_FALSEBR:
    case OPC_IF:
      branch = du->CD_is_fall_thru(bb_idx);
      stmt = Get_new_stmt (cd_idx, branch, stmt_node);
      break;
    case OPC_TRUEBR:
      branch = du->CD_is_br_taken(bb_idx);
      stmt = Get_new_stmt (cd_idx, branch, stmt_node);
      break;
    case OPC_DO_LOOP:
    case OPC_FUNC_ENTRY:
      stmt = Get_new_stmt (cd_idx, TRUE, stmt_node);
      break;
    }
    
    if (Do_Par)
      WN_MAP32_Set (Stmt_Map, stmt_node, cd_idx + 1);

    switch (WN_operator (w)) {
    case OPR_CALL:
    case OPR_ICALL:
      stmt->Set_call_index (node_index);
      Inc_cd_call_count (cd_idx, branch);
      break;

    case OPR_ARRAY: {
      WN_MAP_Set_ID (Current_Map_Tab, w);

      if (WN_is_istore_or_mstore(w) &&
          ST_is_common_element(WN_st(WN_array_base(w)))) {

        WN* istore = LWN_Get_Parent(w);

        SUMMARY_STID* stid_val = New_global_stid();
        stmt->Set_stid_index(Get_global_stid_idx());

        ST* array_st = WN_st(WN_array_base(w));
        stid_val->Set_symbol_index(Get_symbol_index(array_st));
        stid_val->Set_array_assignment();
        if (Get_procedure(Get_procedure_idx())->Get_call_count() == 0 &&
            Get_cd_by_idx(cd_idx)->Is_entry()) {
          stid_val->Set_always_executed();
        }
        if (TY_kind(ST_type(array_st)) == KIND_ARRAY &&
            WN_num_dim(w) == 1 && 
            WN_operator(istore) == OPR_ISTORE) {
          WN* subscript = WN_array_index(w, 0);
          if (WN_operator(subscript) == OPR_INTCONST) {
            INT64 const_subscript = WN_const_val(subscript);
            if (const_subscript <= UINT32_MAX) {
              stid_val->Set_constant_subscript();
              stid_val->Set_array_subscript((UINT32)const_subscript);
            
              WN* rhs = WN_kid0(istore);
              // only record constant assignments to array elements
              if (WN_operator(rhs) == OPR_INTCONST) {

                SUMMARY_VALUE* value = New_value();
                value->Set_int_const();
                value->Set_int_const_value(WN_const_val(rhs));
                value->Set_mtype(WN_rtype(rhs));

                INT val_idx = entry_cache->Lookup(SUM_VALUE, value);
                if (val_idx != -1 && val_idx < Get_value_idx()) {
                  _value.Decidx();
                } 
                else {
                  val_idx = Get_value_idx();
                  entry_cache->Insert(SUM_VALUE, val_idx);
                }
                stid_val->Set_value_index(val_idx);
              }
            }
          }
        }
      }
      break;
    } 

    case OPR_STID: {
      SUMMARY_STID* stid_val = New_global_stid();
      stmt->Set_stid_index(Get_global_stid_idx());

      stid_val->Set_symbol_index(Get_symbol_index(WN_st(w)));
      if (Get_procedure(Get_procedure_idx())->Get_call_count() == 0 &&
          Get_cd_by_idx(cd_idx)->Is_entry()) {
        stid_val->Set_always_executed();
      }

      const SUMMARY_CHECK_POINT chk_pt(this);
      SUMMARY_VALUE* value = New_value();
      INT value_idx = Get_value_idx();
      value->Set_not_const();

      Process_jump_function(WN_kid0(w), value_idx);

      if (value->Is_not_const()) {
        Restore_from_check_point(&chk_pt);
      }
      else {
        INT tmp_idx = entry_cache->Lookup (SUM_VALUE, value);
        if (tmp_idx != -1 && tmp_idx < value_idx) {
          Restore_from_check_point(&chk_pt);
          stid_val->Set_value_index(tmp_idx);
        } 
        else {
          entry_cache->Insert(SUM_VALUE, value_idx);
          stid_val->Set_value_index(value_idx);
        }
      }
    } 
    break;

    default:
      if (OPCODE_has_sym (WN_opcode (w)))
	stmt->Set_var_index (node_index);
      else if (OPCODE_is_scf(WN_opcode(w)) || OPCODE_is_non_scf(WN_opcode(w)))
	stmt->Set_cond_index (node_index);
      else
	stmt->Set_expr_index (node_index);
      break;
    }

    
    /* we need to following the cd-chain all the way to the function entry
       point, but don't want to repeat if the chain has already been
       established (hence the check for "new_cd_created").
     */
    if (new_cd_created) {
      if (cd->Is_entry ())
	Set_cd_head_of_chain (cd_idx);
      else if (!Process_control_dependence (cond_stmt, cd_idx))
	Set_cd_head_of_chain (cd_idx);
    }
    
    return TRUE;
} // SUMMARIZE<program>::Process_control_dependence


template <PROGRAM program>
void
SUMMARIZE<program>::Copy_summary_ctrl_dep (SUMMARY_CONTROL_DEPENDENCE *cd)
{
    SUMMARY_STMT *stmts;
    SUMMARY_CONTROL_DEPENDENCE *ctrl_dep = New_ctrl_dep ();
    INT stmt_idx = Get_stmt_idx ();

    *ctrl_dep = *cd;
    Set_cd_ctrl_index (cd, Get_ctrl_dep_idx ());
    ctrl_dep->Set_map_id (WN_map_id (cd->Get_wn ()));
    
    _stmt.Setidx (cd->Get_true_count () + cd->Get_false_count () + stmt_idx);

    if (cd->Get_true_count ()) {
	ctrl_dep->Set_true_stmt_index (stmt_idx + 1);
	stmts = Get_stmt (ctrl_dep->Get_true_stmt_index ());
	bcopy (Get_summary_stmts (cd, TRUE), stmts,
	       sizeof(SUMMARY_STMT) * cd->Get_true_count ());
    }

    if (cd->Get_false_count ()) {
	ctrl_dep->Set_false_stmt_index (stmt_idx + cd->Get_true_count () + 1);
	stmts = Get_stmt (ctrl_dep->Get_false_stmt_index ());
	bcopy (Get_summary_stmts (cd, FALSE), stmts,
	       sizeof(SUMMARY_STMT) * cd->Get_false_count ());
    }

    // fix up the node indices
    INT stmt_lastidx = Get_stmt_idx ();
    for (INT i = stmt_idx + 1; i <= stmt_lastidx; i++) {
	SUMMARY_STMT *stmt = Get_stmt (i);
	if (stmt->Is_cond ()) {
	    SUMMARY_CONTROL_DEPENDENCE *src_cd =
		Get_cd_by_idx (stmt->Get_cond_index ());
	    Copy_summary_ctrl_dep (src_cd);
	    Get_stmt (i)->Set_cond_index (Get_cd_real_idx (src_cd));
	} else if (stmt->Is_array_ref ()) {
	    stmt->Set_array_ref_map_id (WN_map_id (stmt->Get_array_ref_wn ()));
	}
    }
} // SUMMARIZE<program>::Copy_summary_ctrl_dep 


template <PROGRAM program>
inline void
SUMMARIZE<program>::Generate_summary_control_dependence (void)
{
    SUMMARY_CONTROL_DEPENDENCE *cd;

    while (cd = Get_next_cd_chain ())
	Copy_summary_ctrl_dep (cd);

} // SUMMARIZE<program>::Generate_summary_control_dependence

//-----------------------------------------------------------------
// check the IO stmt and mark all symbols occuring in 
// IO stmts as being MODIFIED and also mark common symbols as
// having bad nodes
//-----------------------------------------------------------------
template <PROGRAM program>
void 
SUMMARIZE<program>::Process_IO(WN* w)
{ 
  Is_True(WN_operator(w) == OPR_IO, ("Unexpected operator in Process_IO \n"));

  switch (WN_io_statement(w)) {
    case IOS_READ:
    case IOS_ACCEPT:
    case IOS_CR_FRF:
    case IOS_CR_FRU:
      Set_IO(w, TRUE);
      break;

    default:
      Set_IO(w, FALSE);
      break;
  }
} // SUMMARIZE<program>::Process_IO(WN* w)

//-----------------------------------------------------------------
// Set the IO bits
//-----------------------------------------------------------------
template <PROGRAM program>
void
SUMMARIZE<program>::Set_IO(WN* w, BOOL is_read)
{
  OPERATOR opr = WN_operator(w);
  if (OPERATOR_has_sym(opr)) {
    ST* st = WN_st(w);
    if (st && ST_class(st) != CLASS_CONST) {

      if (ST_base_idx(st) != ST_st_idx(st) && !ST_is_weak_symbol(st)) {
        st = ST_base(st);
      }
      
      // set HAS_IO flag for common block symbols
      ST_SCLASS sclass = ST_sclass(st);
      if ((sclass == SCLASS_COMMON || sclass == SCLASS_DGLOBAL) &&
          TY_kind(ST_type(st)) == KIND_STRUCT) {
        if (WN_operator(LWN_Get_Parent(w)) != OPR_ARRAY) {
          Get_symbol(Get_symbol_index(st))->Set_common_io_no_pad();
        }
        if (is_read) {
          Get_symbol(Get_symbol_index(st))->Set_common_read_no_cprop();
        }
      }

      // record MODs to globals for READ IO
      if (is_read) {
        SUMMARY_SYMBOL* symbol = Get_symbol(Get_symbol_index(st));
        symbol->Set_imod();
        switch (ST_sclass(st)) {
          case SCLASS_COMMON:
          case SCLASS_EXTERN:
          case SCLASS_UGLOBAL:
          case SCLASS_DGLOBAL:
          case SCLASS_FSTATIC: {
            SUMMARY_GLOBAL* global;
            INT index = Global_hash_table->Find(st);
            if (index == 0) {
              global = New_global();
              global->Set_symbol_index(Get_symbol_index(st));
              Global_hash_table->Enter(st, Get_global_idx()+1);
            } 
            else {
              global = Get_global(index-1);
            }
            global->Set_dmod();
            global->Inc_modcount();
            symbol->Set_modcount();
            break;
          }
        }
      }
    }
  }
    
  if (opr == OPR_BLOCK) {
    for (WN* wn = WN_first(w); wn; wn = WN_next(wn)) {
      Set_IO(wn, is_read);
    }
  }
  else {
    for (INT kid = 0; kid < WN_kid_count(w); ++kid) {
      Set_IO(WN_kid(w,kid), is_read);
    }
  }
}

#endif // ipl_analyze_template_INCLUDED
