//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_efinalize.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_efinalize.cxx,v $
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


#include "id_map.h"

#include "opt_vertab.h"
#include "opt_etable.h"
#include "opt_htable.h"
#include "opt_efinalize.h"
#include "opt_ssa.h"
#include "opt_mu_chi.h"
#include "opt_fold.h"
#include "opt_verify.h"		// Def_before_use
#include "tracing.h"

#include "opt_config.h"		// WOPT_Enable_Output_Copy,
				// WOPT_Enable_SSA_Minimization

void
EXP_WORKLST::Insert_one_operand(      ETABLE  *const etable,
				      CODEMAP *const htable,
				      EXP_PHI *const phi,
				const INT            opnd_num)
{
  const CODEREP    *op   = phi->Result()->Occurrence();
  const EXP_OCCURS *opnd = phi->Opnd(opnd_num);

  // Perform insertion for this operand.
  Is_True(!phi->Not_down_safe(),
	  ("Insertion for opnd %d of phi in BB%d must be down-safe",
	   opnd_num, phi->Bb()->Id()));
  FmtAssert(!phi->Not_down_safe(),
	    ("Insertion must be down-safe"));
  FmtAssert(op != NULL,
	    ("Occurrence() coderep must be non-NULL "
	     "for down-safe phi"));

  // Obtain the insert expression -- usually already cached in etable.
  CODEREP *insert_cr = 
    etable->Alloc_and_generate_cur_expr(op, phi->Bb(), 
					opnd_num, etable->Per_expr_pool(),
					TRUE /* convert zero version */);

#if Is_True_On
  BOOL was_integral_load_store = FALSE;
#endif

  /* CVTL-RELATED start (correctness) */
  // Insert CVT/CVTL if necessary
  if (insert_cr->Is_integral_load_store()) {
    insert_cr = Save_use_cr(etable, insert_cr);
#if Is_True_On
    was_integral_load_store = TRUE;
#endif
  }
  /* CVTL-RELATED finish */

  // Rehash the insert_cr into htable.  
  // htable->Rehash_tree() may overwrite some fields of insert_cr.
  // Even though we don't allow constant/copy prop, the expression
  // tree can change because of the addition or removal of CVT/CVTL to
  // handle sign extension.
  BOOL tree_changed = FALSE;
  insert_cr = htable->Rehash_tree(insert_cr, FALSE /* no copy prop */,
				  &tree_changed,
				  phi->Pred(opnd_num)->Bb());

  // In the following assertion, checking
  // (was_integral_load_store && Is_the_same_as(insert_cr->Opnd(0)))
  // first is not OK because insert_cr can be a CK_VAR node.
  Is_Trace(etable->Tracing(), (TFile, "EXP_WORKLST::Insert_one_operand\n"));
  Is_Trace(etable->Tracing(), (TFile, "Inserted expression:\n"));
  Is_Trace_cmd(etable->Tracing(), insert_cr->Print(3, TFile));
  Is_Trace(etable->Tracing(), (TFile, "CFG \n"));
  Is_Trace_cmd(etable->Tracing(), etable->Cfg()->Print(TFile));
  Is_True(Is_the_same_as(insert_cr) ||
	  (was_integral_load_store && 
	   Is_the_same_as(insert_cr->Opnd(0))),
	  ("EXP_WORKLST::Insert_one_operand: Rehashing must not "
	   "change expression"));
  Is_Trace(etable->Tracing(), (TFile, "Inserted expression:\n"));
  Is_Trace_cmd(etable->Tracing(), insert_cr->Print(3, TFile));

  Is_True(Def_before_use(insert_cr,
			 phi->Bb()->Nth_pred(opnd_num)),
	  ("EXP_WORKLST::Insert_one_operand: "
	   "Inserted use before def"));

  // We don't create the assignment to PREG here. Instead, CodeMotion
  // will create that assignment. All we need to do is flag this
  // occurrence as an inserted computation (with a _bb field instead
  // of a _stmt field because the assignment doesn't exist yet),
  // create an e-version for it if there isn't one already, and
  // replace the corresponding NULL or !Will_b_avail() phi operand
  // with that e-version.
  ++_insert_cnt;

  // Get the phi-pred occurrence corresponding to the
  // inserted phi operand, and embed the inserted real
  // occurrence in it.
  EXP_OCCURS *insert_occ = phi->Pred(opnd_num);
  insert_occ->Set_inserted();
  insert_occ->Set_occurrence(insert_cr);
  if (tree_changed) {
    insert_occ->Set_rehash_changed_expr();
  }

  // When it generates the statement to save the inserted occurrence,
  // CodeMotion will put the new occurrence on the list of inserted
  // items to undergo output copy prop after CodeMotion.

  // We know the computation for this occurrence should not
  // be deleted, and that it should be saved to a temporary
  // because the inserted occurrence participates in some
  // redundancy.
  insert_occ->Set_save_to_temp();
  insert_occ->Set_def_occur(NULL);

  if (phi->Opnd(opnd_num) == NULL) {
    insert_occ->Set_e_version(Cur_e_version());
    New_e_version();
  }
  else {
    insert_occ->Set_e_version(phi->Opnd(opnd_num)->E_version());
  }

  // Replace the expression phi operand with the inserted
  // occurrence.
  phi->Set_opnd(opnd_num, insert_occ);

  // The operand could have referred before insertion to the result
  // of a phi that turned out to be unavailable but was injured from
  // the point of view of strength reduction. It is impossible for
  // the inserted computation to be injured, so we make sure the
  // phi operand doesn't look injured any more.
  phi->Reset_injured(opnd_num);

  // Note an additional appearance of this coderep's
  // expression along with its kids.
  //
  // This is being done in the CodeMotion step (opt_cse.cxx) when the
  // expression value gets saved to temporary
  // (CSE::Save_real_occurrence()).  Also, the recursive increment
  // might not be the right one to use. Where can I learn the
  // semantics of CODEREP::Usecnt()?
  // insert_cr->IncUsecnt_rec();
}

BOOL
EXP_PHI::Need_insertion(INT opnd_num) const
{
  Is_True(Will_b_avail(),
	  ("EXP_PHI::Need_insertion: *this must be Will_b_avail()"));

  const EXP_OCCURS *const opnd = Opnd(opnd_num);
  return (opnd == NULL ||
	  (opnd->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR &&
	   !Has_real_occ(opnd_num) &&
	   !opnd->Exp_phi()->Will_b_avail()));
}

// check if it is necessary to restore the avail_def to the earlier lvalue
// occurrence (called only when doing live range shrinking in LPRE)
static inline void
Check_restore_sunk_lvalue_def(E_VER_TAB *e_ver_tab, EXP_OCCURS *occ) 
{
  IDTYPE e_vers = occ->E_version();
  EXP_OCCURS *defocc = e_ver_tab->Avail_def(e_vers);
  if (defocc == NULL)
    return;
  if (defocc->Bb()->Dominates(occ->Bb()))
    return;
  if (defocc->Occurs_as_lvalue())
    return;
  if (defocc->Sunk_lvalue()) { // occ for the first use after the sunk def
    defocc = defocc->Def_occur();
    e_ver_tab->Set_real_avail_def(e_vers, defocc);
  }
}


BOOL
EXP_WORKLST::Determine_inserts_saves_deletions(CODEMAP         *htable,
					       ETABLE          *etable,
					       E_VER_TAB       *e_ver_tab,
					       EXP_OCCURS_PAIR *comp_occurs)
{
  BOOL optimization_happened = FALSE;

  // First, set up a mapping from CODEREP ID to occurrence nodes, so
  // we can keep track of which occurrences share each coderep for the
  // current expression. This mapping is used in determining which
  // occurrence will own the coderep for the purpose of avoiding
  // rehash during the CodeMotion step.
  //
  // The initial capacity (Cur_e_version() / 4) is just a guess at the
  // number of distinct CODEREPS used by this expression. Undoubtedly
  // tuning will be required, since we don't want too many
  // enlargements of the mapping, and we don't want too great an
  // initial size.

  ID_MAP<REHASH_INFO *, INT32> id_map(Cur_e_version() / 4,
				      NULL,
				      etable->Etable_local_pool(),
				      etable->Tracing());

  if (WOPT_Enable_Avoid_Rehash) {
    id_map.Init();
  }
  else {
    // id_map is not built. NO REFERENCE TO id_map IS ALLOWED.
  }

  // For each spot where an expression needs to be inserted, determine
  // the current versions of both expression operands (there can be
  // two at most), build a coderep referring to them, and insert the
  // coderep in the appropriate BB.
  EXP_ALL_OCCURS_ITER  occ_iter(Real_occurs().Head(),
				comp_occurs,
				Phi_occurs().Head(),
				Phi_pred_occurs().Head(),
				NULL);   // disregard exit occurrences
  EXP_OCCURS          *occ;

  // For each phi node for this expression, find its operands and its
  // predecessor BB's, and perform insertion if required (i.e., if the
  // operand should be available but isn't).
  //
  // For each real occurrence node for this expression, determine
  // whether the node should be saved, and whether it should be
  // deleted. We make heavy use of the property that the
  // EXP_ALL_OCCURS_ITER (via dominator preorder) gives us a
  // definition for each version before we see any uses.
  FOR_ALL_NODE(occ, occ_iter, Init()) {
    switch (occ->Occ_kind()) {
    case EXP_OCCURS::OCC_PHI_OCCUR:
      if (occ->Exp_phi()->Will_b_avail()) {
	// Because the result of this phi will be available, this phi
	// is the available definition for its result's
	// e-version. Save the occurrence in the e_ver_tab.
	Is_True(e_ver_tab->Avail_def(occ->E_version()) == NULL,
		("e-version %d already has an available definition",
		 occ->E_version()));
	e_ver_tab->Set_avail_def(occ->E_version(), occ);
      }
      else {
	// This phi occurrence will not be available. I don't think we
	// need to do anything.
      }

      // In preparation for the SSA minimization step, zero out the
      // _identical_to field, which is a union with a field used
      // during the DownSafety step. Also make the default condition
      // be Identity() if SSA minimization is enabled. If SSA
      // minimization is disabled, we want the default condition to
      // remain Not_identity(). We leave the former assignment
      // outside the "if", and both of these outside the check for
      // Will_b_avail() above for improved debugging.
      occ->Exp_phi()->Set_identical_to(NULL);
      if (WOPT_Enable_SSA_Minimization) {
	occ->Exp_phi()->Set_identity();
      }
      break;
    case EXP_OCCURS::OCC_REAL_OCCUR:
      {
	if (LR_shrink_cand())
	  Check_restore_sunk_lvalue_def(e_ver_tab, occ);

	// In the beginning, all real occurrences are assumed not to be
	// deleted so if we turn off Finalize, CodeMotion will still
	// generate correct output. We set the "delete" bit for this
	// occurrence if some other occurrence will serve as the
	// available definition for this e-version (i.e. the avail_def
	// entry for this e-version is non-NULL).
	//
	// If this occurrence is not the available definition for its
	// e-version, we will certainly not save it and we will
	// certainly delete it. Therefore it requires no further
	// processing. If is is the available definition, though, we
	// cannot determine its "save" bit, since we don't know whether
	// it has more than one use or not. So we save it in the
	// e_ver_tab, which will get traversed after we've seen
	// all the occurrences for this expression.

	EXP_OCCURS *def = e_ver_tab->Avail_def(occ->E_version());

	if (def == NULL || !def->Bb()->Dominates(occ->Bb())) {
	  // This is the first possible available definition we've seen
	  // of this version along the present control flow path, so
	  // this is the available definition. We know we won't delete
	  // this occurrence, but whether we save it or not will depend
	  // on whether the same version gets used anywhere else (as a
	  // real occurrence or as an operand of a phi whose result will
	  // be available).
/* START EXTRACTED CODE */
	  e_ver_tab->Set_real_avail_def(occ->E_version(), occ);
	  occ->Set_def_occur(NULL);
	  // The occurrence may have been marked injured on the
	  // assumption that its SSA def would be its available
	  // def. Since the SSA def turned out not to be available (if
	  // it wasn't this occurrence in the first place), there is no
	  // longer any justification for saying this occurrence is
	  // injured.
	  occ->Reset_injured_occ();
	  if (LR_shrink_cand() &&
	      occ->Occurs_as_lvalue() && 
	      inCODEKIND(occ->Stmt()->Rhs()->Kind(), CK_LDA|CK_RCONST|CK_CONST))
	    occ->Set_sunk_lvalue();   // def is to be sunk to shrink live range

	  if (WOPT_Enable_Avoid_Rehash) {
	    // The available definition needs to be entered into the
	    // competition to determine who owns its occurrence CODEREP. The
	    // owner is the one who will replace by coderep ID rather than
	    // rehash; we would therefore like the owner to be the real
	    // available definition occurrence for which rehashing will be
	    // the most expensive.
	    if (occ->Temp_eligible_as_owner(this)) {
	      // CK_IVAR codereps that occur as lvalues should never be
	      // ownable by temporaries because the Finalize step cannot
	      // handle replacing them sensibly on the LHS of ISTORE
	      // statements. Temp_eligible_as_owner() includes that
	      // restriction.
	      //
	      // This occurrence's coderep should not be owned by temp
	      // at this point because the lvalue occurrence must be a
	      // definition, and hence must dominate any other
	      // occurrences of the same CODEREP.
	      Is_True(!occ->Occurrence()->Is_flag_set(CF_OWNED_BY_TEMP),
		      ("EXP_WORKLST::Determine_inserts_saves_deletions: "
		       "Lvalue coderep must not be owned by temp"));
	      occ->Render_coderep_unownable(etable, id_map);
	    }
	    else {
	      occ->Bid_for_coderep(etable, id_map, occ->Rehash_cost());
	    }
	  }
/* END EXTRACTED CODE */
  
	  if (occ->Mult_real()) {
	    e_ver_tab->Note_version_use(occ->E_version());
	    optimization_happened = TRUE;
	  }
        }
	else {
	  // The available definition for this version is by a
	  // dominating real or phi occurrence with the same
	  // e-version. If the definition is a real occurrence, it must
	  // be saved to PREG and this real occurrence will be deleted
	  // (reloaded).
	  Is_True(def->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR ||
		  (def->Occ_kind() == EXP_OCCURS::OCC_REAL_OCCUR &&
		   e_ver_tab->Occ_count(occ->E_version()) >= 1),
		  ("Inconsistent occurrence count for e-version %d",
		   occ->E_version()));
	  Is_True(!occ->Occurs_as_lvalue(),
		  ("EXP_WORKLST::Determine_inserts_saves_deletions: "
		   "Lvalue must not be reload"));

	  occ->Set_def_occur(def);
	  if (def->Occurs_as_lvalue() && def->Sunk_lvalue()) {
	    e_ver_tab->Set_real_avail_def(occ->E_version(), occ);
	    occ->Set_sunk_lvalue(); // mark as first use before which the sunk
				    // def is to be inserted later
	  }
	  else {
	    // This occurrence will be a reload. Set the bit to say its
	    // computation will be deleted, and hook up the defining
	    // occurrence field.
	    occ->Set_delete_comp();
	  }

	  if (WOPT_Enable_Avoid_Rehash) {
	    // The available definition needs to be entered into the
	    // competition to determine who owns its occurrence CODEREP. The
	    // owner is the one who will replace by coderep ID rather than
	    // rehash; we would therefore like the owner to be the real
	    // available definition occurrence for which rehashing will be
	    // the most expensive. In the presence of strength
	    // reduction, it may be the case that this occurrence's
	    // coderep is different from that of the available
	    // definition. In that situation, we simply don't include
	    // this occurrence's temporary among the candidates for
	    // ownership of any coderep. TODO: Revisit this strategy and
	    // do something more intelligent.
	    if (def->Occurrence() == occ->Occurrence()) {
	      def->Bid_for_coderep(etable, id_map, occ->Rehash_cost());
	    }
	    else {
	      // Because of an interaction between alias analysis
	      // involving unique pointers and copy propagation (and
	      // also maybe due to other reasons) we have no way to
	      // guarantee that the codereps for two ILOADs of the
	      // same value and same e-version will be the
	      // same. Therefore it may happen that we assign the same
	      // temporary version to two (or more) different
	      // codereps, and this will be a problem if we have
	      // allowed the temp to own any of the ILOAD codereps
	      // involved.

	      if (def->Occurrence()->Kind() == CK_IVAR) {
		// We cannot assert here because there can always be
		// different codereps that are only exposed as the
		// same value (and hence e-version) as late as
		// ESSA-renaming time.
		DevWarn("Same-value / different CODEREP* limits "
			"avoid_rehash in SSAPRE");

		def->Render_coderep_unownable(etable, id_map);
	      }
	      else {
		// We assert that the def and use codereps can be
		// different only for indirects, except in the presence
		// of strength reduction:
		FmtAssert(WOPT_Enable_New_SR,
			  ("EXP_WORKLST::Determine_inserts_saves_deletions: "
			   "D/U occurrence codereps must match"));

		Warn_todo("Injured occurrences should take part in "
			  "coderep auction");
	      }
	    }
	  }

	  e_ver_tab->Note_version_use(occ->E_version(),
				      (occ->Mult_real() ? 2 : 1));
	  optimization_happened = TRUE;
	}
      }
      break;
    case EXP_OCCURS::OCC_COMP_OCCUR:
      if (occ->E_version() != EXP_OCCURS::ILLEGAL_E_VERSION &&
	  e_ver_tab->Avail_def(occ->E_version()) != NULL &&
	  e_ver_tab->Avail_def(occ->E_version())->Bb()->Dominates(occ->Bb())) {
	occ->Set_def_occur(e_ver_tab->Avail_def(occ->E_version()));
      }
      else {
	Is_True(occ->Def_occur() == NULL,
		("EXP_WORKLST::Determine_inserts_saves_deletions: "
		 "Comparison must not have trashed Def_occur()"));
      }
      break;
    case EXP_OCCURS::OCC_PHI_PRED_OCCUR:
      {
	// If any operand of a will-be-available phi corresponding to
	// this phi pred occurrence uses the current version, we need
	// to save the version's available definition. If any operand
	// of a will-be-available phi corresponding to this phi pred
	// occurrence is NULL or is not will-be-available, we need to
	// perform insertion for that operand.

	BB_LIST_ITER  succ_bb_iter;
	BB_NODE      *succ_bb;
	BB_NODE      *occ_bb = occ->Bb();
#if Is_True_On
	BOOL          found_phi = FALSE;
#endif
	FOR_ALL_ELEM(succ_bb, succ_bb_iter, Init(occ_bb->Succ())) {
	  EXP_PHI *phi = etable->Lookup_exp_phi(succ_bb, Exp());

	  if (phi != NULL) {
	    Is_True(phi->Bb() == succ_bb,
		    ("EXP_WORKLST::Determine_inserts_saves_deletions: "
		     "ETABLE::Lookup_exp_phi failed"));

	    // There is an expression phi in this successor block.
#if Is_True_On
	    found_phi = TRUE;
#endif
	    if (phi->Will_b_avail()) {
	      // POSSIBLE BUG: If occ_bb appears in succ_bb->Pred()
	      // more than once, we have a problem. A bug I fixed once
	      // in IRM suggests that this can happen. -- RK
	      const INT32 opnd_num = succ_bb->Pred()->Pos(occ_bb);
	      EXP_OCCURS *const opnd = phi->Opnd(opnd_num);

	      Is_True(phi->Pred(opnd_num) == occ,
		      ("EXP_WORKLST::Determine_inserts_saves_deletions: "
		       "phi->Pred() / phi-pred inconsistency"));

	      if (phi->Need_insertion(opnd_num)) {
		BOOL opnd_had_e_version = phi->Opnd(opnd_num) != NULL;

		// Perform insertion for this operand.
		Insert_one_operand(etable, htable, phi, opnd_num);

		// If the phi operand had an e-version already, the
		// insertion could be redundant with other occurrences
		// (phi operands and/or real occurrences). In the case
		// where the phi operand had an e-version already, the
		// e-version number was retained by the insertion and
		// we save the Inserted_computation phi-pred
		// occurrence as the available definition for this
		// e-version.
		//
		// Note: In the current implementation, it cannot
		// happen that an insertion for a NULL phi operand is
		// redundant with any other use. The reason is that
		// the WillBeAvail step will not allow insertion for
		// such phi operands (see the function
		// Requires_edge_placement() and its call sites in
		// opt_eavail.cxx).
		if (opnd_had_e_version) {
		  
		}
	      }
	      else {
		// No insertion required for this operand; note its
		// use of its available definition.

		// Must use opnd->E_version() in the following because
		// phi-pred occurrences aren't allowed to use the
		// E_version() member function.

		optimization_happened = TRUE;
		if (LR_shrink_cand())
		  Check_restore_sunk_lvalue_def(e_ver_tab, opnd);
		EXP_OCCURS *def = e_ver_tab->Avail_def(opnd->E_version());
		if (def->Occurs_as_lvalue() && def->Sunk_lvalue()
#if defined(TARG_SL) //PARA_EXTENSION
                  && !occ_bb->SL2_para_region()
#endif
		    ) {             
		  // this phi operand is first use before which the sunk def 
		  // is to be inserted later
		  occ->Set_sunk_lvalue();
		  occ->Set_occurrence(def->Occurrence()); // to give def stmt
		  phi->Set_opnd(opnd_num, occ); // Phi opnd pts to phi-pred nd
		}
		else {
#if defined(TARG_SL) //PARA_EXTENSION
                  if(occ_bb->SL2_para_region() && def->Sunk_lvalue())
                    def->Reset_sunk_lvalue(); 
#endif
		  e_ver_tab->Note_version_use(opnd->E_version());
  
		  phi->Set_opnd(opnd_num,
			        e_ver_tab->Avail_def(opnd->E_version()));
		}
	      }
	    }
	  }
	}
	Is_True(found_phi,
		("EXP_WORKLST::Determine_inserts_saves_deletions: "
		 "phi-pred must have successor phi"));
      }
      break;
    default:
      FmtAssert(FALSE, ("EXP_WORKLST::Determine_inserts_saves_deletions: "
			"Bad occurrence kind"));
    }
  }
  return optimization_happened;
}

#if Is_True_On
static inline BOOL bxor(const BOOL a, const BOOL b)
{
  return (a && !b) || (!a && b);
}

void
EXP_WORKLST::Verify_saved_occurs(E_VER_TAB *e_ver_tab)
{
  EXP_OCCURS_ITER  real_occ_iter;
  EXP_OCCURS      *real_occ;

  // Dump the version table to the trace file before checking
  // anything.
  Is_Trace(e_ver_tab->Tracing(),
           (TFile, "================ Version table: ==========\n"));
  UINT ver;
  for (ver = 1; ver < e_ver_tab->N_versions(); ver++) {
    if (e_ver_tab->Avail_def(ver) != NULL) {
      Is_Trace(e_ver_tab->Tracing(),
               (TFile, "Version %d: Occ_count() == %d; Avail_def()%s:\n",
                ver, e_ver_tab->Occ_count(ver),
		e_ver_tab->Redefined(ver) ? " (redefined)" : ""));
      Is_Trace_cmd(e_ver_tab->Tracing(),
                   e_ver_tab->Avail_def(ver)->Print(TFile));
    }
    else {
      Is_Trace(e_ver_tab->Tracing(),
               (TFile, "Version %d: Occ_count() == %d; "
                "Avail_def() is NULL\n", ver, e_ver_tab->Occ_count(ver)));
    }
  }

  // For each real occurrence node for this expression
  FOR_ALL_NODE(real_occ, real_occ_iter, Init(Real_occurs().Head())) {
    if (real_occ->Save_to_temp()) {
      Is_True(!real_occ->Delete_comp(),
	      ("EXP_WORKLST::Verify_saved_occurs: Saved occurrence "
	       "must not be deleted"));
      Is_True((real_occ ==
	       e_ver_tab->Avail_def(real_occ->E_version())) ||
	      e_ver_tab->Redefined(real_occ->E_version()),
	      ("EXP_WORKLST::Verify_saved_occurs: Saved occurrence "
	       "must be avail_def if version not redef"));
      Is_True((e_ver_tab->Occ_count(real_occ->E_version()) > 1) ||
	      e_ver_tab->Redefined(real_occ->E_version()),
	      ("EXP_WORKLST::Verify_saved_occurs: Saved occurrence "
	       "must have Occ_count > 1"));
    }
    if (real_occ->Delete_comp()) {
      Is_True(real_occ != e_ver_tab->Avail_def(real_occ->E_version()),
	      ("EXP_WORKLST::Verify_saved_occurs: Deleted occurrence "
	       "must not be avail_def"));
    }
    if (real_occ != e_ver_tab->Avail_def(real_occ->E_version())) {
      Is_True((real_occ->Def_occur() ==
	       e_ver_tab->Avail_def(real_occ->E_version())) ||
	      e_ver_tab->Redefined(real_occ->E_version()),
	      ("EXP_WORKST::Verify_saved_occurs: Non-avail_def real "
	       "occ must have avail_def as its Def_occur()"));
    }
  }

  // Now check the version table contents for consistency.
  for (ver = 1; ver < e_ver_tab->N_versions(); ver++) {
    if (e_ver_tab->Avail_def(ver) != NULL) {
      Is_True((e_ver_tab->Avail_def(ver)->Occ_kind() ==
	       EXP_OCCURS::OCC_PHI_OCCUR) ||
	      !e_ver_tab->Avail_def(ver)->Delete_comp(),
	      ("EXP_WORKLST::Verify_saved_occurs: Real avail_def must not "
	       "be deleted"));
      Is_True((e_ver_tab->Avail_def(ver)->Occ_kind() ==
	       EXP_OCCURS::OCC_PHI_OCCUR) ||
	      (e_ver_tab->Avail_def(ver)->Def_occur() == NULL) ||
	      e_ver_tab->Avail_def(ver)->Sunk_lvalue(),
	      ("EXP_WORKLST::Verify_saved_occurs: Avail_def real occurrence "
	       "must have Def_occur() == NULL"));

      // Check that the available definition is exactly one of
      // 1) Will-be-availabile phi occurrence,
      // 2) Saved real occurrence with occurrence count > 1, or
      // 3) Unsaved real occurrence with occurrence count == 1.
      Is_True(bxor((e_ver_tab->Avail_def(ver)->Occ_kind() ==
		   EXP_OCCURS::OCC_PHI_OCCUR) &&
		  e_ver_tab->Avail_def(ver)->Exp_phi()->Will_b_avail(),
		  (e_ver_tab->Avail_def(ver)->Occ_kind() ==
		   EXP_OCCURS::OCC_REAL_OCCUR) &&
		  bxor(e_ver_tab->Avail_def(ver)->Save_to_temp() &&
		      (e_ver_tab->Occ_count(ver) > 1),
		      !e_ver_tab->Avail_def(ver)->Save_to_temp() &&
		      (e_ver_tab->Occ_count(ver) == 1))),
	      ("EXP_WORKLST::Verify_saved_occurs: Avail_def(%d) "
	       "inconsistent", ver));
    }
    else {
      Is_True(e_ver_tab->Occ_count(ver) == 0,
	      ("EXP_WORKLST::Verify_saved_occurs: Occ_count must be zero"));
    }
  }
}
#endif

// Perform insertions to fulfill the prophecy of
// EXP_WORKLST::Compute_forward_attributes and determine which
// occurrences of the expression computation will be saved to
// temporary and which will be deleted.

BOOL
EXP_WORKLST::Compute_save_delete(// inserted exprs go into htable
				  CODEMAP         *htable,
				  // etable carries
				  // Etable_local_pool()
				  ETABLE          *etable,
				  EXP_OCCURS_PAIR *comp_occurs)
{
  // Prepare for e_ver_tab allocation.
  OPT_POOL_Push(etable->Etable_local_pool(), -1);

  E_VER_TAB *e_ver_tab =
    CXX_NEW(E_VER_TAB(etable->Etable_local_pool(),
		      Cur_e_version(),
		      etable->Tracing()),
	    etable->Etable_local_pool());

  BOOL optimization_happened =
    Determine_inserts_saves_deletions(htable, etable, e_ver_tab,
				      comp_occurs);

  Is_Trace(etable->Tracing(),
	   (TFile,
	    "==== After EXP_WORKLST::Determine_inserts_saves_deletions\n"));
  Is_Trace_cmd(etable->Tracing(), Print(TFile, comp_occurs));

#if Is_True_On
  Verify_saved_occurs(e_ver_tab);
#endif

  CXX_DELETE(e_ver_tab, etable->Etable_local_pool());

  OPT_POOL_Pop(etable->Etable_local_pool(), -1);

  return optimization_happened;
}
