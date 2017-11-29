//-*-c++-*-

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_expr.cxx,v $
//
// ====================================================================
//
// Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
//
// Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
// Mountain View, CA 94043, or:
//
// http://www.sgi.com
//
// For further information regarding this notice, see:
//
// http://oss.sgi.com/projects/GenInfo/NoticeExplan
//
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#include "defs.h"
#include "config_wopt.h"
#include "opt_defs.h"
#include "mempool.h"
#include "opt_base.h"
#include "opt_htable.h"
#include "opt_mu_chi.h"
#include "opt_fold.h"


// check if the CODEREP cr is redefined within the number of statement
// starting from statement 'stmt'.  Returns FALSE if it reach the end
// of the statement list, and it has not found any STID for the same
// variable.  If it exhausted the exam_stmtcnt, return TRUE even it 
// has not reach any statement that redefines the same variable.
static BOOL
Has_overlapping_liverange(INT exam_stmtcnt, CODEREP *cr, STMTREP *stmt)
{
  if (stmt == NULL) return FALSE;

  for (INT i = 0; i < exam_stmtcnt; i++) {
    if (OPERATOR_is_scalar_store (stmt->Opr())) {
      if (stmt->Lhs()->Aux_id() == cr->Aux_id() && stmt->Lhs() != cr ) {
        // overlapped live range 
        return TRUE;
      }
    } else if (stmt->Has_chi()) {
      CHI_LIST_ITER chi_iter;
      CHI_NODE *cnode;
      FOR_ALL_NODE ( cnode, chi_iter, Init(stmt->Chi_list()) ) {
	if (cnode->Aux_id() == cr->Aux_id())
	  return TRUE;
      }
    }
    stmt = stmt->Next();
    if (stmt == NULL)
      return FALSE;
  }
  return TRUE;
}

//  Recursively rehash an expression tree.
//  See CODEMAP::Rehash_tree() for details
//
static CODEREP *
Rehash_tree_rec(CODEREP *newcr,
                BOOL     prop,
                BOOL    *changed,
                CODEMAP *htable,
                BB_NODE *bb,
                OPT_STAB *sym)
{
  switch (newcr->Kind()) {
  case CK_LDA:
  case CK_CONST:
  case CK_RCONST:
    return newcr;

  case CK_VAR:
    Is_True(!newcr->Is_flag_set(CF_IS_ZERO_VERSION),
	    ("Rehash_tree_rec: CK_VAR is zero version."));
    if (prop &&
        !newcr->Is_var_volatile() &&
        newcr->Defstmt() != NULL &&
        !newcr->Is_flag_set((CR_FLAG)(CF_DEF_BY_PHI|CF_DEF_BY_CHI))) {
      CODEREP *rhs = newcr->Defstmt()->Rhs();

      BOOL fold_it = FALSE;

      if (inCODEKIND( rhs->Kind(), CK_LDA|CK_CONST|CK_RCONST)) fold_it = TRUE;
      else if (rhs->Kind() == CK_VAR) {
        ST *s = sym->St(rhs->Aux_id());
        if ((ST_class(s) == CLASS_PREG && Preg_Is_Dedicated(rhs->Offset())))
          return newcr;

        STMTREP *defstmt = newcr->Defstmt();
        BB_NODE *defbb = defstmt->Bb();
        BOOL     overlapped = FALSE;
        if (defbb == bb) {
          fold_it = ! Has_overlapping_liverange
            (WOPT_Enable_Ocopy_Lookupstmt+bb->Loopdepth(), rhs, defstmt);
        }
        else if (defbb == bb->Prev() && bb->Idom() == defbb) {
          fold_it = ! Has_overlapping_liverange
            (WOPT_Enable_Ocopy_Lookupstmt+bb->Loopdepth(), rhs, bb->First_stmtrep());
          if (fold_it)
            fold_it = ! Has_overlapping_liverange
              (WOPT_Enable_Ocopy_Lookupstmt+bb->Loopdepth(), rhs, defstmt);
	  if (fold_it) {
	    // Fix 481883:  fix overlapped live range with dead phis
	    PHI_LIST_ITER phi_iter;
	    PHI_NODE     *phi;
	    AUX_ID aux_id = rhs->Aux_id();
	    FOR_ALL_ELEM (phi, phi_iter, Init(bb->Phi_list())) {
	      if (phi->Aux_id() == aux_id)
		fold_it = FALSE;
	    }
	  }
        }
      }
      if (fold_it) {
        Is_Trace(htable->Tracing(), (TFile, "  Rehash tree replace:"));
        Is_Trace_cmd(htable->Tracing(), newcr->Print(0,TFile));
        Is_Trace(htable->Tracing(), (TFile, "  with:"));
        Is_Trace_cmd(htable->Tracing(), rhs->Print(0,TFile));
        *changed = TRUE;
#ifdef KEY // fix bug 2657
	rhs = newcr->Convert_type(htable, rhs, FALSE);
#endif
        return rhs;
      }
    }
    return newcr;

  case CK_IVAR:
    {
      BOOL kid_changed = FALSE;
      CODEREP *ilod_base = newcr->Ilod_base();
      CODEREP *istr_base = newcr->Istr_base();
      UINT orig_cr_id = newcr->Coderep_id();
      
      if (newcr->Coderep_id() != 0) {
	// newcr points to a coderep that's already hashed. Assume,
	// therefore, that its mu node and ivar occurrence are already
	// stored in safe memory, and we don't have to duplicate them.
	CODEREP *copy_cr;
	copy_cr = Alloc_stack_cr(newcr->Extra_ptrs_used());
	copy_cr->Copy(*newcr);
	newcr = copy_cr;
      }
      else {
	// Now we need to clone memory pointed to by insert_cr
	// (mu_node and ivar_occ).
	if (newcr->Ivar_mu_node() != NULL)
	  newcr->Set_ivar_mu_node(CXX_NEW(MU_NODE(*newcr->Ivar_mu_node()),
					  htable->Sym()->Occ_pool()));
	else
	  newcr->Set_ivar_mu_node(NULL);

	if (newcr->Ivar_occ() != NULL)
	  newcr->Set_ivar_occ(CXX_NEW(OCC_TAB_ENTRY(*newcr->Ivar_occ()),
				      htable->Sym()->Occ_pool()));
	else
	  newcr->Set_ivar_occ(NULL);

	newcr->Set_coderep_id(0);
      }

      // Now newcr points to a coderep that isn't in the htable, but
      // whose ivar occ and mu node are in safe memory. We are ready
      // to rehash the kids and then newcr.

      if (ilod_base == istr_base) {
	CODEREP *x = Rehash_tree_rec(ilod_base, prop, &kid_changed,
                                     htable, bb, sym);
	if (x != ilod_base) {
	  newcr->Set_ilod_base(x);
	  newcr->Set_istr_base(x);
	}
      } else {
	if (ilod_base) {
	  CODEREP *x = Rehash_tree_rec(ilod_base, prop, &kid_changed,
                                       htable, bb, sym);
	  newcr->Set_ilod_base(x);
	}
	if (istr_base) {
	  CODEREP *x = Rehash_tree_rec(istr_base, prop, &kid_changed,
                                       htable, bb, sym);
	  newcr->Set_istr_base(x);
	}
      }
      CODEREP *cr_after_simp = NULL;
      if (kid_changed) {
	FOLD ftmp;
	cr_after_simp = ftmp.Fold_Expr(newcr);
	*changed = TRUE;
      }
      if (cr_after_simp == NULL) {
	newcr = htable->Rehash(newcr);
	if (orig_cr_id != 0 && 
	    orig_cr_id != newcr->Coderep_id())
	  *changed = TRUE;
      } else
	newcr = cr_after_simp;

#ifdef KEY // bug 5285
      if (newcr->Kind() == CK_CONST)
	return newcr;
#endif

      // Why isn't CK_VAR possible here?
      Is_True(newcr->Kind()==CK_IVAR || (newcr->Kind()==CK_OP &&
					 (newcr->Opr()==OPR_CVT ||
					  newcr->Opr()==OPR_CVTL)),
	      ("Rehash_tree_rec: illegal CK_IVAR after folding"));

      CODEREP *newcr_ivar;
      if (newcr->Kind() == CK_IVAR) {
	newcr_ivar = newcr;
      } else {
	newcr_ivar = newcr->Opnd(0);
	*changed = TRUE;
      }
      if (prop &&
	  !newcr_ivar->Is_var_volatile() &&
	  newcr_ivar->Ivar_defstmt() != NULL &&
	  !newcr_ivar->Is_flag_set((CR_FLAG)(CF_DEF_BY_PHI|CF_DEF_BY_CHI)) &&
	  inCODEKIND( newcr_ivar->Ivar_defstmt()->Rhs()->Kind(), CK_LDA|CK_CONST|CK_RCONST)) {
	Is_Trace(htable->Tracing(), (TFile, "  Rehash tree replace:"));
	Is_Trace_cmd(htable->Tracing(), newcr->Print(0,TFile));
	Is_Trace(htable->Tracing(), (TFile, "  with:"));
	Is_Trace_cmd(htable->Tracing(), newcr_ivar->Ivar_defstmt()->Rhs()->Print(0,TFile));
	*changed = TRUE;

	if (newcr->Kind() != CK_IVAR) {
	  newcr->Set_opnd(0, newcr_ivar->Ivar_defstmt()->Rhs());
	  newcr->Set_coderep_id(0);
	  return htable->Rehash(newcr);
	} else
	  return newcr->Ivar_defstmt()->Rhs();

      } else
	return newcr;
    }
	
  case CK_OP:
    {
      if (newcr->Coderep_id() != 0) {
	CODEREP *copy_cr = Alloc_stack_cr(newcr->Extra_ptrs_used());
	copy_cr->Copy(*newcr);
	newcr = copy_cr;
      }
      for (INT i = 0; i < newcr->Kid_count(); i++) {
	BOOL kid_changed = FALSE;
	newcr->Set_opnd(i, Rehash_tree_rec(newcr->Opnd(i), prop,
                                           &kid_changed, htable, bb, sym));
	*changed |= kid_changed;
      }
      if (*changed && 
	  (prop || newcr->Opr() == OPR_CVT || newcr->Opr() == OPR_CVTL)) {
	FOLD ftmp;
	CODEREP *x = ftmp.Fold_Expr(newcr);
	if (x != NULL) 
	  return x;
      }
      // assume this routine called only in SSAPRE, no canonicalization for compare
      return htable->Rehash(newcr, !OPCODE_is_compare(newcr->Op()));  
    }

  default:
    Is_True(FALSE, ("unexpected CK_KIND."));
  }
  return NULL;
}


//  Rehash an expression tree recursively:
//   - will overwrite CODEREP's pointed to by newcr;
//   - if prop is TRUE, try to perform propagation and simplification;
//   - changed is set to TRUE if tree changed;
//   - bb is the containing BB_NODE, assuming the new expression is
//     appended at the end of this BB_NODE
//  
CODEREP *
CODEMAP::Rehash_tree(CODEREP *newcr, BOOL prop, BOOL *changed, BB_NODE *bb)
{
  BOOL     local_changed = FALSE;
  CODEREP *retval       = Rehash_tree_rec(newcr, prop, &local_changed,
					  this, bb, Sym());
  if (changed != NULL) {
    *changed = local_changed;
  }
  return retval;
}
