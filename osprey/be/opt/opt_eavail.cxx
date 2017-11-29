//-*-c++-*-

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_eavail.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_eavail.cxx,v $
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
//
// Description:
//
// This file implements the WillBeAvail step from the "new PRE" paper.
// The algorithm consists of two forward propagation passes in
// sequence.
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
#include "opt_config.h"		// WOPT_Enable_Edge_Placement,
				// WOPT_Enable_Backedge_Placement

#if Is_True_On
#include "opt_htable.h"		// for CODEREP printing in traces
#endif

// forward declaration
template <class PRE_DIRECTION> class CANT_BE_AVAIL_SEARCH;

class USER_AVAIL_SEARCH {
private:
  static BOOL         _tracing;
  static EXP_WORKLST *_worklst;

  EXP_WORKLST *Worklst(void) const       { return _worklst; }

  EXP_PHI *const _cur_node;

public:
  static void Set_tracing(const BOOL tracing) { _tracing = tracing; }
  static void Set_worklst(EXP_WORKLST *const worklst)
    { _worklst = worklst; }

  BOOL        Tracing(void) const { return _tracing; }

  typedef     EXP_PHI            node_type;
  typedef     USE_LIST_ENTRY     adj_list_type;
  typedef     USE_LIST_ITER      adj_list_iter_type;
  typedef     EXP_PHI_OCC_ITER   node_iterator;

  USER_AVAIL_SEARCH(node_type *const cur_node) : _cur_node(cur_node)
    {
      Is_True(Worklst() != NULL,
	      ("USER_AVAIL_SEARCH: must not construct with NULL worklist"));
    }

  void Set_seen(node_type *phi) const
    { phi->Set_not_user_avail(); }

  BOOL Seen(const node_type *const phi) const
    { return phi->Not_user_avail(); }

  /* ARGSUSED */
  void Reach_from_to(const node_type *const def_phi,
		     const INT              opnd_idx,
		     const node_type *const use_phi) const
    { }

  BOOL Start_from(const node_type *const phi) const
    {
      for (INT i = 0; i < phi->Opnd_count(); i++) {
	if (phi->Opnd(i) == NULL) {
	  return TRUE;
	}
      }
      return FALSE;
    }

  /* ARGSUSED */
  BOOL Continue_from_to(const node_type *const def_phi,
			const INT              opnd_idx,
			const node_type *const use_phi) const
    { return !use_phi->Has_real_occ(opnd_idx); }

  /* ARGSUSED */
  void Postorder_processing(node_type *const phi) const
    { }

  node_type *Current_node(void) const { return _cur_node; }

  adj_list_type *Neighbors(const node_type *const def_phi) const
    { return def_phi->Uses(); }

  EXP_OCCURS_CONTAINER &Nodes(void) const
    { return Worklst()->Phi_occurs(); }

  const char *Search_name(void) const
    { return "USER_AVAIL_SEARCH";}
};

BOOL         USER_AVAIL_SEARCH::_tracing = FALSE;
EXP_WORKLST *USER_AVAIL_SEARCH::_worklst = NULL;

struct FORWARD_PRE {
  BB_LIST *Upward_neighbors(const BB_NODE *bb) const
    { return bb->Pred(); }

  BB_LIST *Downward_neighbors(const BB_NODE *bb) const
    { return bb->Succ(); }
};

struct BACKWARD_PRE {
  BB_LIST *Upward_neighbors(const BB_NODE *bb) const
    { return bb->Succ(); }

  BB_LIST *Downward_neighbors(const BB_NODE *bb) const
    { return bb->Pred(); }
};

template <class PRE_DIRECTION>
class DEFEAT_INSERT_SEARCH {
private:
  static BOOL       _tracing;

  EXP_PHI *const    _cur_node;

public:
  static void Set_tracing(const BOOL tracing)
    { _tracing = tracing; }

  BOOL        Tracing(void) const { return _tracing; }

  typedef     EXP_PHI               node_type;
  typedef     EXP_PHI_OPND_USE_ITER adj_list_type;
  typedef     EXP_PHI_OPND_USE_ITER adj_list_iter_type;

  DEFEAT_INSERT_SEARCH(node_type *const cur_node) :
                      _cur_node(cur_node)
    { }

  void Set_seen(node_type *const phi) const
    { phi->Set_cant_be_avail(); }

  BOOL Seen(const node_type *const phi) const
    { return phi->Cant_be_avail(); }

  /* ARGSUSED */
  void Reach_from_to(const node_type *const use_phi,
		     const INT              opnd_idx,
		     const node_type *const def_phi) const
    { }

  BOOL Start_from(const node_type *const use_phi) const
    { return use_phi->Not_user_avail(); }

  /* ARGSUSED */
  BOOL Continue_from_to(const node_type *const use_phi,
			const INT              opnd_idx,
			const node_type *const def_phi) const
    { return def_phi->Not_user_avail(); }

  /* ARGSUSED */
  void Postorder_processing(node_type *const phi) const
    {
      CANT_BE_AVAIL_SEARCH<PRE_DIRECTION> cba_search(phi);

      Df_search(cba_search);
    }

  node_type *Current_node(void) const
    { return _cur_node; }

  adj_list_type *Neighbors(node_type *use_phi) const
    {
      static EXP_PHI_OPND_USE_ITER adj_list;

      adj_list.Init(use_phi);
      return &adj_list;
    }

const   char *Search_name(void) const;
};

template <> BOOL DEFEAT_INSERT_SEARCH<FORWARD_PRE>::_tracing = FALSE;
template <> BOOL DEFEAT_INSERT_SEARCH<BACKWARD_PRE>::_tracing = FALSE;

template <> const char *
DEFEAT_INSERT_SEARCH<FORWARD_PRE>::Search_name(void) const
{
  return "DEFEAT_INSERT_SEARCH<FORWARD_PRE>";
}

template <> const char *
DEFEAT_INSERT_SEARCH<BACKWARD_PRE>::Search_name(void) const
{
  return "DEFEAT_INSERT_SEARCH<BACKWARD_PRE>";
}

// An expression phi requires edge placement if it has a NULL operand
// corresponding to a BB that has multiple successors. This function
// is used only to determine whether a down-safe phi with one or more
// NULL operands would require edge placement to achieve
// availability. It is not used for phi's with operands that are uses
// of phi results.

/* ARGSUSED */
template <class PRE_DIRECTION>
BOOL
Requires_edge_placement(const EXP_PHI       *phi,
			const BOOL           tracing,
			const PRE_DIRECTION &direction)
{
  INT           i = 0;
  BB_LIST_ITER  neighbor_iter;
  BB_NODE      *upward_neighbor;
  FOR_ALL_ELEM(upward_neighbor,
	       neighbor_iter,
	       Init(direction.Upward_neighbors(phi->Bb()))) {
    if (phi->Opnd(i) == NULL &&
	direction.Downward_neighbors(upward_neighbor)->Multiple_bbs()) {
      if (WOPT_Enable_Edge_Placement &&
	  WOPT_Enable_Backedge_Placement) {
	DevWarn("EXP_PHI::Requires_edge_placement: Critical edge found "
		"under full edge placement");
      }
      Is_Trace(tracing,
	       (TFile, "Defeating early insertion in BB%d of\n",
		upward_neighbor->Id()));
      Is_Trace_cmd(tracing,
		   phi->Result()->Occurrence()->Print(4, TFile));
      return TRUE;
    }
    i++;
  }
  return FALSE;
}

template <class PRE_DIRECTION>
class CANT_BE_AVAIL_SEARCH {
private:
  static BOOL         _tracing;
  static EXP_WORKLST *_worklst;

  EXP_PHI *const            _cur_node;

  EXP_WORKLST *Worklst(void) const       { return _worklst; }

public:
  static void Set_tracing(const BOOL tracing)
    { _tracing = tracing; }

  static void Set_worklst(EXP_WORKLST *const worklst)
    { _worklst = worklst; }

  BOOL        Tracing(void) const { return _tracing; }

  typedef     EXP_PHI            node_type;
  typedef     USE_LIST_ENTRY     adj_list_type;
  typedef     USE_LIST_ITER      adj_list_iter_type;
  typedef     EXP_PHI_OCC_ITER   node_iterator;

  CANT_BE_AVAIL_SEARCH(node_type *const cur_node) :
                      _cur_node(cur_node)
    {
      Is_True(Worklst() != NULL,
	      ("CANT_BE_AVAIL_SEARCH: must not construct "
	       "with NULL worklist"));
    }

  void Set_seen(node_type *const phi) const
    { phi->Set_cant_be_avail(); }

  BOOL Seen(const node_type *const phi) const
    { return phi->Cant_be_avail(); }

  /* ARGSUSED */
  void Reach_from_to(const node_type *const def_phi,
		     const INT              opnd_idx,
		     const node_type *const use_phi) const
    {
      // Maybe set the phi operand in question to NULL here.
    }

  BOOL Start_from(node_type *const phi) const
    {
      if (phi->Not_down_safe()) {
	for (INT i = 0; i < phi->Opnd_count(); i++) {
	  if (phi->Opnd(i) == NULL) {
	    return TRUE;
	  }
	}
      }
      PRE_DIRECTION dir;
      if (Requires_edge_placement(phi, Tracing(), dir)) {
	// use_phi can't be available because an insertion on a
	// critical edge would be required to establish its
	// availability. Since use_phi won't be available, there may
	// be insertions at points that use the e-version it defines,
	// and we want to make sure we don't perform insertions
	// "above" use_phi that would be partially redundant with
	// those later insertions. Therefore we defeat insertions
	// above use_phi via a search based on DEFEAT_INSERT_SEARCH.
	DEFEAT_INSERT_SEARCH<PRE_DIRECTION>::Set_tracing(Tracing());
	DEFEAT_INSERT_SEARCH<PRE_DIRECTION> srch(phi);

	Df_search(srch);

	return TRUE;
      }
      else {
	return FALSE;
      }
    }

  /* ARGSUSED */
  BOOL Continue_from_to(const node_type     *const def_phi,
			const INT                  opnd_idx,
			      node_type     *const use_phi) const;

  /* ARGSUSED */
  void Postorder_processing(node_type *const phi) const
    { }

  node_type *Current_node(void) const
    { return _cur_node; }

  adj_list_type *Neighbors(node_type *def_phi) const
    { return def_phi->Uses(); }

  EXP_OCCURS_CONTAINER &Nodes(void) const
    { return Worklst()->Phi_occurs(); }

  const char *Search_name(void) const;
};

template <> BOOL CANT_BE_AVAIL_SEARCH<FORWARD_PRE>::_tracing = FALSE;
template <> EXP_WORKLST *CANT_BE_AVAIL_SEARCH<FORWARD_PRE>::_worklst = NULL;

// For EPRE/LPRE:
template <> const char *
CANT_BE_AVAIL_SEARCH<FORWARD_PRE>::Search_name(void) const
{ return "CANT_BE_AVAIL_SEARCH<FORWARD_PRE>"; }

template <> BOOL
CANT_BE_AVAIL_SEARCH<FORWARD_PRE>::
Continue_from_to(const node_type     *const def_phi,
		 const INT                  opnd_idx,
		 node_type     *const use_phi) const
{
  if (use_phi->Has_real_occ(opnd_idx)) {
    return FALSE;
  }
  if (use_phi->Not_down_safe()) {
    return TRUE;
  }
  if (use_phi->Bb()->Nth_pred(opnd_idx)->Succ()->Multiple_bbs()) {
    FmtAssert(!WOPT_Enable_Edge_Placement ||
	      !WOPT_Enable_Backedge_Placement,
	      ("CANT_BE_AVAIL_SEARCH<FORWARD_PRE>::Continue_from_to: "
	       "Critical edge found under full edge placement"));
    Is_Trace(Tracing(),
	     (TFile, "Defeating insertion in BB%d of\n",
	      use_phi->Bb()->Nth_pred(opnd_idx)->Id()));
    Is_Trace_cmd(Tracing(),
		 use_phi->Result()->Occurrence()->Print(4, TFile));

    // use_phi can't be available because an insertion on a
    // critical edge would be required to establish its
    // availability. Since use_phi won't be available, there may
    // be insertions at points that use the e-version it defines,
    // and we want to make sure we don't perform insertions
    // "above" use_phi that would be partially redundant with
    // those later insertions. Therefore we defeat insertions
    // above use_phi via a search based on DEFEAT_INSERT_SEARCH.
    DEFEAT_INSERT_SEARCH<FORWARD_PRE>::Set_tracing(_tracing);

    DEFEAT_INSERT_SEARCH<FORWARD_PRE> srch(use_phi);
      
    Df_search(srch);
    return TRUE;
  }
  return FALSE;
}

template <> BOOL CANT_BE_AVAIL_SEARCH<BACKWARD_PRE>::_tracing = FALSE;
template <> EXP_WORKLST *CANT_BE_AVAIL_SEARCH<BACKWARD_PRE>::_worklst = NULL;

// For SPRE:
template <> const char *
CANT_BE_AVAIL_SEARCH<BACKWARD_PRE>::Search_name(void) const
{ return "CANT_BE_AVAIL_SEARCH<BACKWARD_PRE>"; }

template <> BOOL
CANT_BE_AVAIL_SEARCH<BACKWARD_PRE>::
Continue_from_to(const node_type     *const def_phi,
		 const INT                  opnd_idx,
		 node_type     *const use_phi) const
{
  // TODO: For SPRE, stop representing arcs with the
  // Has_real_occ() flag set. We need them in EPRE for SSA
  // minimization, but there's no need for them in SPRE. Once
  // they're gone in SPRE, we can modify the following check to
  // happen only under EPRE if we decide it's worth it.
  if (use_phi->Has_real_occ(opnd_idx)) {
    return FALSE;
  }
  if (use_phi->Not_down_safe()) {
    return TRUE;
  }
  if (use_phi->Bb()->Nth_succ(opnd_idx)->Pred()->Multiple_bbs()) {
    FmtAssert(!WOPT_Enable_Edge_Placement ||
	      !WOPT_Enable_Backedge_Placement,
	      ("CANT_BE_AVAIL_SEARCH<BACKWARD_PRE>::Continue_from_to: "
	       "Critical edge found under full edge placement"));
    Is_Trace(Tracing(),
	     (TFile, "Defeating insertion in BB%d of\n",
	      use_phi->Bb()->Nth_pred(opnd_idx)->Id()));
    Is_Trace_cmd(Tracing(),
		 use_phi->Result()->Occurrence()->Print(4, TFile));

    // use_phi can't be available because an insertion on a
    // critical edge would be required to establish its
    // availability. Since use_phi won't be available, there may
    // be insertions at points that use the e-version it defines,
    // and we want to make sure we don't perform insertions
    // "above" use_phi that would be partially redundant with
    // those later insertions. Therefore we defeat insertions
    // above use_phi via a search based on DEFEAT_INSERT_SEARCH.
    DEFEAT_INSERT_SEARCH<BACKWARD_PRE>::Set_tracing(_tracing);

    DEFEAT_INSERT_SEARCH<BACKWARD_PRE> srch(use_phi);

    Df_search(srch);
    return TRUE;
  }
  return FALSE;
}

class STOPS_SEARCH {
private:
  static BOOL         _tracing;
  static EXP_WORKLST *_worklst;

  EXP_PHI *const _cur_node;

  EXP_WORKLST *Worklst(void) const       { return _worklst; }

public:
  static void Set_tracing(const BOOL tracing) { _tracing = tracing; }

  static void Set_worklst(EXP_WORKLST *const worklst)
    { _worklst = worklst; }

  BOOL        Tracing(void) const { return _tracing; }

  typedef     EXP_PHI            node_type;
  typedef     USE_LIST_ENTRY     adj_list_type;
  typedef     USE_LIST_ITER      adj_list_iter_type;
  typedef     EXP_PHI_OCC_ITER   node_iterator;

  STOPS_SEARCH(node_type *const cur_node) : _cur_node(cur_node)
    {
      Is_True(Worklst() != NULL,
	      ("STOPS_SEARCH: must not construct with NULL worklist"));
    }

  void Set_seen(node_type *const phi) const
    { phi->Set_stops(); }

  BOOL Seen(const node_type *const phi) const
    { return phi->Stops(); }

  /* ARGSUSED */
  void Reach_from_to(const node_type *const def_phi,
		     const INT              opnd_idx,
		           node_type *const use_phi) const
    {
      // If we aren't going to insert for this operand, the operand
      // stops forward movement.
      //
      // Actually, the operand stops forward movement regardless.
	use_phi->Set_opnd_stops(opnd_idx);
    }

  BOOL Start_from(const node_type *const phi) const
    {
      for (INT i = 0; i < phi->Opnd_count(); i++) {
	if (phi->Opnd_stops(i)) {
	  return TRUE;
	}
      }
      return FALSE;
    }

  /* ARGSUSED */
  BOOL Continue_from_to(const node_type *const def_phi,
			const INT              opnd_idx,
			const node_type *const use_phi) const
    { return Start_from(use_phi); }

  /* ARGSUSED */
  void Postorder_processing(node_type *const phi) const
    { }

  node_type *Current_node(void) const
    { return _cur_node; }

  adj_list_type *Neighbors(node_type *def_phi) const
    { return def_phi->Uses(); }

  EXP_OCCURS_CONTAINER &Nodes(void) const
    { return Worklst()->Phi_occurs(); }

  const char *Search_name(void) const
    { return "STOPS_SEARCH"; }
};

BOOL         STOPS_SEARCH::_tracing = FALSE;
EXP_WORKLST *STOPS_SEARCH::_worklst = NULL;


class PARTIAL_AVAIL_SEARCH {
private:
  static BOOL         _tracing;
  static EXP_WORKLST *_worklst;

  EXP_WORKLST *Worklst(void) const       { return _worklst; }

  EXP_PHI *const _cur_node;

public:
  static void Set_tracing(const BOOL tracing) { _tracing = tracing; }
  static void Set_worklst(EXP_WORKLST *const worklst)
    { _worklst = worklst; }

  BOOL        Tracing(void) const { return _tracing; }

  typedef     EXP_PHI            node_type;
  typedef     USE_LIST_ENTRY     adj_list_type;
  typedef     USE_LIST_ITER      adj_list_iter_type;
  typedef     EXP_PHI_OCC_ITER   node_iterator;

  PARTIAL_AVAIL_SEARCH(node_type *const cur_node) : _cur_node(cur_node)
    {
      Is_True(Worklst() != NULL,
	      ("PARTIAL_AVAIL_SEARCH: must not construct with NULL worklist"));
    }

  void Set_seen(node_type *phi) const
    { phi->Set_partial_avail(); }

  BOOL Seen(const node_type *const phi) const
    { return phi->Partial_avail(); }

  /* ARGSUSED */
  void Reach_from_to(const node_type *const def_phi,
		     const INT              opnd_idx,
		     const node_type *const use_phi) const
    { }

  BOOL Start_from(const node_type *const phi) const
    {
      for (INT i = 0; i < phi->Opnd_count(); i++) {
	if (phi->Has_real_occ(i)) {
	  return TRUE;
	}
      }
      return FALSE;
    }

  /* ARGSUSED */
  BOOL Continue_from_to(const node_type *const def_phi,
			const INT              opnd_idx,
			const node_type *const use_phi) const
    { return TRUE; } 

  /* ARGSUSED */
  void Postorder_processing(node_type *const phi) const
    { }

  node_type *Current_node(void) const { return _cur_node; }

  adj_list_type *Neighbors(const node_type *const def_phi) const
    { return def_phi->Uses(); }

  EXP_OCCURS_CONTAINER &Nodes(void) const
    { return Worklst()->Phi_occurs(); }

  const char *Search_name(void) const
    { return "PARTIAL_AVAIL_SEARCH";}
};

BOOL   PARTIAL_AVAIL_SEARCH::_tracing = FALSE;
EXP_WORKLST *PARTIAL_AVAIL_SEARCH::_worklst = NULL;

void
EXP_WORKLST::Compute_du_info(MEM_POOL *const def_use_pool)
{
  EXP_OCCURS_ITER  phi_occ_iter;
  EXP_OCCURS      *phi_occ;

  // For each phi node for this expression
  FOR_ALL_NODE(phi_occ, phi_occ_iter, Init(Phi_occurs().Head())) {

    EXP_PHI *phi = phi_occ->Exp_phi();

    for (INT i = 0; i < phi->Opnd_count(); ++i) {
      EXP_OCCURS *def = phi->Opnd(i);
      // Annotate each phi operand's def with the use by the phi
      // operand if the def is a phi.
      if (def != NULL) {
	// Record the def-use arc if the def is by phi
	if (def->Occ_kind() == EXP_OCCURS::OCC_PHI_OCCUR) {
	  def->Exp_phi()->Add_use(phi, i, def_use_pool);
	}
	else {
	  Is_True(def->Occ_kind() == EXP_OCCURS::OCC_REAL_OCCUR,
		  ("EXP_WORKLST::Compute_du_info: Phi opnd must be "
		   "real or phi"));
	}
      }
    }
  }
}


void
EXP_WORKLST::Compute_user_avail(const BOOL tracing)
{
  // Determines which phi occurrences have real occurrences available
  // for each and every phi operand.  This applies transitively for 
  // operands that are themselves phi occurrences.  A value or 
  // expression is fully available at a phi node when: 
  //
  //    Not_user_avail() == FALSE
  //
  USER_AVAIL_SEARCH::Set_worklst(this);
  USER_AVAIL_SEARCH::Set_tracing(tracing);

  USER_AVAIL_SEARCH ua_srch(NULL);

  Perform_dfs(ua_srch);
} // EXP_WORKLST::Compute_user_avail


void
EXP_WORKLST::Compute_avail(const BOOL      tracing)
{
  if (!WOPT_Enable_Edge_Placement ||
      !WOPT_Enable_Backedge_Placement) {
    // There could be one or more critical edges in the CFG, and hence
    // one or more critical potential insertion points in the
    // expression SSA graph. To systematically avoid unsafe insertions
    // at these critical points, we need to know which expression
    // phi's are fully available in the user program.
    Compute_user_avail(tracing);
  }

  if (Real_occurs().Head()->For_spre()) {
    CANT_BE_AVAIL_SEARCH<BACKWARD_PRE>::Set_worklst(this);
    CANT_BE_AVAIL_SEARCH<BACKWARD_PRE>::Set_tracing(tracing);

    CANT_BE_AVAIL_SEARCH<BACKWARD_PRE> cba_srch(NULL);

    Perform_dfs(cba_srch);
  }
  else {
    CANT_BE_AVAIL_SEARCH<FORWARD_PRE>::Set_worklst(this);
    CANT_BE_AVAIL_SEARCH<FORWARD_PRE>::Set_tracing(tracing);

    CANT_BE_AVAIL_SEARCH<FORWARD_PRE> cba_srch(NULL);

    Perform_dfs(cba_srch);
  }
}


void
EXP_WORKLST::Compute_partial_avail(const BOOL      tracing)
{
  PARTIAL_AVAIL_SEARCH::Set_worklst(this);
  PARTIAL_AVAIL_SEARCH::Set_tracing(tracing);
  PARTIAL_AVAIL_SEARCH pa_srch(NULL);
  Perform_dfs(pa_srch);
}

void
EXP_WORKLST::Compute_stops(const BOOL tracing)
{
  EXP_OCCURS_ITER  phi_occ_iter;
  EXP_OCCURS      *phi_occ;

  // TODO: Remove the following loop. The initial condition for
  // Stops() and Opnd_stops() should be computed during the
  // unavailability search.
  FOR_ALL_NODE(phi_occ, phi_occ_iter, Init(Phi_occurs().Head())) {
    EXP_PHI *phi = phi_occ->Exp_phi();
    if (phi->Cant_be_avail()) {
      phi->Set_stops();
    }
    for (INT i = phi->Opnd_count() - 1; i >= 0; i--) {
      if (phi->Has_real_occ(i)) {
	// This phi operand stops forward movement from would-be
	// insertion points.
	phi->Set_opnd_stops(i);
      }
    }
  }

  STOPS_SEARCH::Set_worklst(this);
  STOPS_SEARCH::Set_tracing(tracing);

  STOPS_SEARCH srch(NULL);

  Perform_dfs(srch);
}

// For the current worklist (i.e., the current expression), decide
// for each expression phi whether that phi node will be available
// after insertions. This implicitly determines the insertion points.

void
EXP_WORKLST::Compute_forward_attributes(// etable carries
					// Etable_local_pool()
					ETABLE     *etable, BOOL compute_partial_avail)
{
  Is_Trace(etable->Tracing(),
	   (TFile,
	    "==== On entry to EXP_WORKLST::Compute_forward_attributes\n"));
  Is_Trace_cmd(etable->Tracing(), Print(TFile));

  // Compute the local def-use information
  Compute_du_info(etable->Etable_local_pool());

  Compute_avail(etable->Tracing());
 
  if (compute_partial_avail)
    Compute_partial_avail(etable->Tracing());

  Is_Trace(etable->Tracing(),
	   (TFile, "==== After EXP_WORKLST::Compute_avail\n"));
  Is_Trace_cmd(etable->Tracing(), Print(TFile));

  Compute_stops(etable->Tracing());

  Is_Trace(etable->Tracing(),
	   (TFile, "==== After EXP_WORKLST::Compute_stops\n"));
  Is_Trace_cmd(etable->Tracing(), Print(TFile));
}


// For the current worklist (i.e., the current expression/value), decide
// for each expression/value phi whether that phi node is fully available
// without any insertions.
//
void
EXP_WORKLST::Compute_fully_avail(ETABLE *etable)
{
  Is_Trace(etable->Tracing(),
	   (TFile, "==== On entry to EXP_WORKLST::Compute_fully_avail\n"));
  Is_Trace_cmd(etable->Tracing(), Print(TFile));

  // Compute the local def-use information and calculate availability.
  //
  Compute_du_info(etable->Etable_local_pool()); // Allocates du-info
  Compute_user_avail(etable->Tracing());

  Is_Trace(etable->Tracing(),
	   (TFile, "==== After EXP_WORKLST::Compute_fully_avail\n"));
  Is_Trace_cmd(etable->Tracing(), Print(TFile));
} // EXP_WORKLST::Compute_fully_avail
