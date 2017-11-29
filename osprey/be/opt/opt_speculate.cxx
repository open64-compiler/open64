//-*-c++-*-

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_speculate.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_speculate.cxx,v $
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


#define USE_STANDARD_TYPES

#include "defs.h"
#include "config_wopt.h"
#include "opt_etable.h"
#include <algorithm>
#include <vector>
#include "connected_components.h"

using SGI::find_representative_and_compress_path;
using SGI::connect_components;
using SGI::extend_components_and_ranks;

void
EXP_WORKLST::Save_flags()
{
  EXP_OCCURS_ITER phi_occ_iter(Phi_occurs().Head());
  EXP_OCCURS *occur;
  FOR_ALL_NODE(occur, phi_occ_iter, Init()) {
    occur->Exp_phi()->Save_flags();
    occur->Save_flags();
  }
  EXP_OCCURS_ITER real_occ_iter(Real_occurs().Head());
  FOR_ALL_NODE(occur, real_occ_iter, Init()) {
    occur->Save_flags();
  }
  EXP_OCCURS_ITER phi_pred_occ_iter(Phi_pred_occurs().Head());
  FOR_ALL_NODE(occur, phi_pred_occ_iter, Init()) {
    occur->Save_flags();
  }
}


void
EXP_WORKLST::Estimate_cost(ETABLE *etable, PRE_KIND pre_kind)
{
  const char *note = pre_kind_name(pre_kind);

  EXP_OCCURS_ITER phi_occ_iter(Phi_occurs().Head());
  EXP_OCCURS_ITER real_occ_iter(Real_occurs().Head());
  EXP_OCCURS *occur;

  // Assign a unique ID to each phi node.
  // Keep a mapping from e_version to EXP_OCCURS of phi.
  // Ignore phi nodes that are not partial avail.
  // All phi node are assumed to be partial anticipated.
  //
  INT32 phi_count = 0;
  vector<EXP_OCCURS *> e_version_table;
  e_version_table.insert(e_version_table.end(), Cur_e_version(), (EXP_OCCURS*)NULL);
  FOR_ALL_NODE(occur, phi_occ_iter, Init()) {
    occur->Exp_phi()->id = phi_count++;
    if (occur->Exp_phi()->Partial_avail())
      e_version_table[occur->E_version()] = occur;
  }

  // Put connected phi's into the same component.
  // component is indexed by phi-id
  vector<int> component;
  {
    vector<unsigned char> rank;
    extend_components_and_ranks(component, rank, phi_count - 1);
    FOR_ALL_NODE(occur, phi_occ_iter, Init()) {
      EXP_PHI *phi = occur->Exp_phi();
      if (e_version_table[occur->E_version()] == NULL) continue;
      Is_True(phi->Partial_avail(), ("Estimate_cost: phi is not partial avail."));
      for (int i = 0; i < phi->Opnd_count(); ++i) {
	if (phi->Opnd(i) == NULL) continue;
	EXP_OCCURS *opnd_occ = e_version_table[phi->Opnd(i)->E_version()];
	if (opnd_occ == NULL) continue;
	Is_True(opnd_occ->Exp_phi()->Partial_avail(), 
		("Estimate_cost: phi is not partial avail."));
	connect_components(component.begin(),
			   rank,
			   phi->id,
			   opnd_occ->Exp_phi()->id);
      }
    }
    for (int i = 0; i < component.size(); ++i)
      find_representative_and_compress_path(component.begin(), i);
  }

#ifdef Is_True_On
  // Verify that every non-empty component has at least one real occ.
  {
    vector<bool> component_has_real_occ;
    component_has_real_occ.insert(component_has_real_occ.end(), component.size(), 0);
    
    FOR_ALL_NODE(occur, real_occ_iter, Init()) {
      EXP_OCCURS *phi_occ = e_version_table[occur->E_version()];
      if (phi_occ != NULL) {
	int equiv_class = component[phi_occ->Exp_phi()->id];
	component_has_real_occ[equiv_class] = 1;
      }
    }

    FOR_ALL_NODE(occur, phi_occ_iter, Init()) {
      EXP_PHI *phi = occur->Exp_phi();
      if (e_version_table[occur->E_version()] == NULL) continue;
      int equiv_class = component[phi->id];
      FmtAssert(component_has_real_occ[equiv_class], 
		("Estimate_cost: equiv class %d has no real occ.", equiv_class));
    }
  }
#endif

  // Count the number of real occurrence for each connect components
  // Note: real occurrence not defined by any phi are ignored.
  vector<int> original_count;
  original_count.insert(original_count.end(), component.size(), 0);

  FOR_ALL_NODE(occur, real_occ_iter, Init()) {
    if (occur->Occurs_as_lvalue()) continue;
    EXP_OCCURS *phi_occ = e_version_table[occur->E_version()];
    if (phi_occ == NULL) continue;
    int freq = occur->Stmt()->Bb()->Freq();	 
    int equiv_class = component[phi_occ->Exp_phi()->id];
    original_count[equiv_class] += freq;
  }
  
  // Count the number of computation after PRE, i.e.,
  //  the number of real occurrence not deleted +
  //  the number of inserted occurrence
  // Note: real occurrence not connect with any phi are ignored.
  vector<int> PRE_count;
  PRE_count.insert(PRE_count.end(), component.size(), 0);

  FOR_ALL_NODE(occur, real_occ_iter, Init()) {
    if (occur->Occurs_as_lvalue()) continue;
    if (occur->Delete_comp()) continue;
    EXP_OCCURS *phi_occ = e_version_table[occur->E_version()];
    if (phi_occ == NULL) continue;
    int freq = occur->Stmt()->Bb()->Freq();	 
    int equiv_class = component[phi_occ->Exp_phi()->id];
    PRE_count[equiv_class] += freq;
  }

  FOR_ALL_NODE(occur, phi_occ_iter, Init()) {
    if (e_version_table[occur->E_version()] == NULL) continue;
    EXP_PHI *phi = occur->Exp_phi();
    int equiv_class = component[phi->id];

    if (phi->Will_b_avail()) {
      for (int i = 0; i < phi->Opnd_count(); ++i) {
	if (phi->Need_insertion(i)) {
	  int freq = occur->Exp_phi()->Bb()->Nth_pred(i)->Freq();
	  PRE_count[equiv_class] += freq;
	}
      }
    }
  }

  // Count the number of computation after speculative PRE, i.e.,
  //  the number of real occurrence not deleted by spec-PRE,
  //  the number of inserted occurrence by spec-PRE
  // Note: real occurrence not connect with any phi are ignored.
  vector<int> FB_PRE_count;
  FB_PRE_count.insert(FB_PRE_count.end(), component.size(), 0);

  FOR_ALL_NODE(occur, phi_occ_iter, Init()) {
    if (e_version_table[occur->E_version()] == NULL) continue;
    EXP_PHI *phi = occur->Exp_phi();
    int equiv_class = component[phi->id];

    for (int i = 0; i < phi->Opnd_count(); ++i) {
      if (phi->Has_real_occ(i)) continue;
      if (phi->Opnd(i) == NULL ||
	  (phi->Will_b_avail() && phi->Need_insertion(i)) ||
	  (phi->Opnd(i)->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR &&
	   e_version_table[phi->Opnd(i)->E_version()] == NULL)) {
	int freq = occur->Exp_phi()->Bb()->Nth_pred(i)->Freq();
	FB_PRE_count[equiv_class] += freq;
      }
    }
  }

  
  // restore flags
  FOR_ALL_NODE(occur, phi_occ_iter, Init()) {
    EXP_PHI *phi = occur->Exp_phi();
    phi->Set_uses(NULL);
    for (int i = 0; i < phi->Opnd_count(); ++i) {
      if (phi->Opnd(i) != NULL &&
	  phi->Opnd(i)->Inserted_computation()) {
	fprintf(TFile, "%s: reset phi (ver=%d) opnd %d\n",
		note,
		occur->E_version(), i);
	phi->Set_opnd(i, e_version_table[phi->Opnd(i)->E_version()]);
      }
    }
  }

  // Update DownSafety
  FOR_ALL_NODE(occur, phi_occ_iter, Init()) {
    occur->Exp_phi()->Restore_flags();
    int equiv_class = component[occur->Exp_phi()->id]; 
    if (FB_PRE_count[equiv_class] < PRE_count[equiv_class]) 
      occur->Exp_phi()->Reset_not_down_safe();
  }

  FOR_ALL_NODE(occur, real_occ_iter, Init()) {
    occur->Restore_flags();
  }
  
  EXP_OCCURS_ITER phi_pred_occ_iter(Phi_pred_occurs().Head());
  FOR_ALL_NODE(occur, phi_pred_occ_iter, Init()) {
    occur->Restore_flags();
  }

  if (Get_Trace(TP_WOPT2, FB_PRE_FLAG)) {
    fprintf(TFile, "==== Estimate cost ====\n");
#ifdef KEY /* Mac port */
    fprintf(TFile, " num components=%ld\n", (long) component.size());
#else /* KEY Mac port */
    fprintf(TFile, " num components=%d\n", (INT)component.size());
#endif /* KEY Mac port */
    for (int i = 0; i < component.size(); ++i) {
      if (original_count[i] > 0) {
	fprintf(TFile, "%s: enum=%d %s=%lld no_opt=%d pre=%d fb_pre=%d\n",
		note,
		E_num(),
		(Exp()->Kind() == CK_CONST) ? "const" : 
		((Exp()->Kind() == CK_VAR) ? "sym" : "symconst"),
		(Exp()->Kind() == CK_CONST) ? Exp()->Const_val() :
		(INT64)((Exp()->Kind() == CK_VAR) ? Exp()->Aux_id() : 0),
		original_count[i],
		PRE_count[i],
		FB_PRE_count[i]);
      }
    }
  }
}

