//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_min_essa.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_min_essa.cxx,v $
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


#include "opt_etable.h"
#include "opt_dfs.h"
#include "tracing.h"

class REPLACING_OCCUR_SEARCH {
private:
  static BOOL         _tracing;
  static EXP_WORKLST *_worklst;

  EXP_PHI *const _cur_node;

  EXP_WORKLST *Worklst(void) const { return _worklst; }

public:
  static void Set_tracing(const BOOL tracing) { _tracing = tracing; }
  static void Set_worklst(EXP_WORKLST *const worklst)
    { _worklst = worklst; }

  BOOL        Tracing(void) const { return _tracing; }

  typedef EXP_PHI          node_type;
  typedef USE_LIST_ENTRY   adj_list_type;
  typedef USE_LIST_ITER    adj_list_iter_type;
  typedef EXP_PHI_OCC_ITER node_iterator;

  REPLACING_OCCUR_SEARCH(node_type *const cur_node) : _cur_node(cur_node)
    {
      Is_True(Worklst() != NULL,
	      ("REPLACING_OCCUR_SEARCH: must not construct "
	       "with NULL worklist"));
    }

  void Set_seen(node_type *const phi) const
    {
      Is_True(phi->Will_b_avail(),
	      ("REPLACING_OCCUR_SEARCH: searched nodes must be Will_b_avail"));

#if !Is_True_On
      // Skip over the search if the answer is already known -- only
      // for efficiency.
      if (phi->Identical_to() == NULL) {
#endif
	for (INT i = 0; i < phi->Opnd_count(); i++) {
	  EXP_OCCURS *identical_to = phi->Opnd(i)->Occ_identical_to();

	  if (identical_to != NULL) {
	    if (phi->Identical_to() == NULL) {
	      phi->Set_identical_to(identical_to);
	      if (phi->Injured(i)) {
		phi->Set_identity_injured();
	      }
	    }
	    else {
	      Is_True(phi->Identical_to() == identical_to,
		      ("REPLACING_OCCUR_SEARCH: Identity must be "
		       "identical to only one occurrence"));
	      Is_True(phi->Identity_injured() || !phi->Injured(i),
		      ("REPLACING_OCCUR_SEARCH: Identity must be "
		       "either injured or not"));
	    }
	  }
	}
#if !Is_True_On
      }
#endif
      FmtAssert(phi->Identical_to() != NULL,
		("REPLACING_OCCUR_SEARCH: replacing occurrence must be "
		 "known"));
      phi->Set_replacing_occur_known();
    }

  BOOL Seen(const node_type *const phi) const
    { return phi->Replacing_occur_known(); }

  /* ARGSUSED */
  void Reach_from_to(const node_type *const def_phi,
		     const INT              opnd_idx,
		           node_type *const use_phi) const
    {
      if (use_phi->Will_b_avail() &&
	  use_phi->Identity() &&
	  use_phi->Identical_to() == NULL) {
	use_phi->Set_identical_to(def_phi->Identical_to());
	if (def_phi->Identity_injured() ||
	    use_phi->Any_opnd_injured()) {
	  use_phi->Set_identity_injured();
	}
      }
      else {
	Is_True(!use_phi->Will_b_avail() || !use_phi->Identity() ||
		use_phi->Identical_to() == def_phi->Identical_to(),
		("REPLACING_OCCUR_SEARCH: Identity must be identical "
		 "to only one occurrence"));
      }
    }

  BOOL Start_from(const node_type *const phi) const
    {
      if (phi->Will_b_avail() && phi->Identity()) {
	for (INT i = 0; i < phi->Opnd_count(); i++) {
	  if (phi->Opnd(i)->Occ_identical_to() != NULL) {
	    return TRUE;
	  }
	}
      }
      return FALSE;
    }

  /* ARGSUSED */
  BOOL Continue_from_to(const node_type *const def_phi,
			const INT              opnd_idx,
			const node_type *const use_phi) const
    { return use_phi->Will_b_avail() && use_phi->Identity(); }

  /* ARGSUSED */
  void Postorder_processing(node_type *const phi) const
    { }

  node_type *Current_node(void) const { return _cur_node; }

  adj_list_type *Neighbors(node_type *def_phi) const
    { return def_phi->Uses(); }

  EXP_OCCURS_CONTAINER &Nodes(void) const
    { return Worklst()->Phi_occurs(); }

  const char *Search_name(void) const
    { return "REPLACING_OCCUR_SEARCH"; }
};

BOOL         REPLACING_OCCUR_SEARCH::_tracing = FALSE;
EXP_WORKLST *REPLACING_OCCUR_SEARCH::_worklst = NULL;

static void
Require_phi(const ETABLE  *const etable,
	    const BB_NODE *const bb,
	    const CODEREP *const expr,
	    const BOOL           tracing)
{
  BB_NODE_SET_ITER  bns_iter;
  BB_NODE          *df_bb;

  Is_Trace(tracing, (TFile, "Defining occurrence in BB%d\n",
		     bb->Id()));
  Is_Trace(tracing, (TFile, "Dominance frontier: "));
  Is_Trace_cmd(tracing, bb->Dom_frontier()->Print(TFile));
  Is_Trace(tracing, (TFile, "\n"));

  FOR_ALL_ELEM (df_bb, bns_iter, Init(bb->Dom_frontier())) {
    EXP_PHI *phi = etable->Lookup_exp_phi(df_bb, expr);
    // phi could be NULL because of an optimization in phi-placement
    // that avoids placing phi where the expression is not partially
    // anticipated.
    if (phi != NULL && phi->Will_b_avail() && phi->Identity()) {
      phi->Reset_identity();

      Is_Trace(tracing, (TFile, "  -- required phi in BB%d:\n",
			 df_bb->Id()));
      Is_Trace_cmd(tracing, phi->Print(TFile));

      Require_phi(etable, df_bb, expr, tracing);
    }
  }
}

void
EXP_WORKLST::Minimize_temp_ssa(const ETABLE *const etable,
			       const BOOL          tracing)
{
  EXP_PHI_OCC_ITER  exp_phi_iter;
  EXP_PHI          *phi;

  // If we run ssa minimization, all Will_b_avail() phi's had their
  // Identity() flags set as a side effect of the Finalize step
  // (opt_efinalize.cxx).

  FOR_ALL_NODE(phi, exp_phi_iter, Init(Phi_occurs())) {
    if (phi->Will_b_avail()) {
      for (INT i = 0; i < phi->Opnd_count(); i++) {
	// Phi nodes for the temp are required on the iterated
	// dominance frontier of assignments to the temp's preg. In
	// the presence of strength reduction it's impossible to know
	// before CodeMotion exactly where the assignments
	// corresponding to injury repairs will be, but we do know
	// that it's sufficient to require phi's on the dominance
	// frontier of:
	//  Inserted occurrences;
	//  Real compute-and-save occurrences; and
	//  Phi preds corresponding to injured operands.
	if (phi->Injured(i)) {
	  Is_Trace(tracing, (TFile, "EXP_PHI with non-identity operand:\n"));
	  Is_Trace_cmd(tracing, phi->Print(TFile));

	  Require_phi(etable, phi->Pred(i)->Bb(), Exp(), tracing);
	}
	else if (phi->Opnd(i)->Is_real_avail_def()) {
	  Is_Trace(tracing, (TFile, "EXP_PHI with non-identity operand:\n"));
	  Is_Trace_cmd(tracing, phi->Print(TFile));

	  Require_phi(etable, phi->Opnd(i)->Bb(), Exp(), tracing);
	}
      }
    }
  }

  Is_Trace(tracing, (TFile, "==== After required-phi analysis\n"));
  Is_Trace_cmd(tracing, Print(TFile));

  REPLACING_OCCUR_SEARCH::Set_tracing(tracing);
  REPLACING_OCCUR_SEARCH::Set_worklst(this);

  REPLACING_OCCUR_SEARCH repl_srch(NULL);

  Perform_dfs(repl_srch);

  Is_Trace(tracing, (TFile, "==== After EXP_WORKLST::Minimize_temp_ssa\n"));
  Is_Trace_cmd(tracing, Print(TFile));
}
