//-*-c++-*-

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_cond_const.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_cond_const.cxx,v $
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
// The module enables limited forms of conditional constant 
// propagation by cloning, and also enables limited form of
// partial redundant branch elimination.
//
// The basic idea is to identify an expression that is a constant
// from some path but not the other.  Then we represent that as
// a zone.  
//
// NOTE: this is very limited because it is only enabled for branches.
//
// ====================================================================
// ====================================================================


#include <set>
#include <map>

#define USE_STANDARD_TYPE
#include "opt_defs.h"
#include "opt_cfg_trans.h"
#include "opt_transform.h"
#include "opt_dce.h"
#include "opt_main.h"
#include "opt_util.h"

using std::set;

typedef set<int> Paths;

struct cond_const_path {
  CODEREP *cr;     // value of conditional const
  Paths    paths;  // set of basic block on the path
  BB_NODE *entry;  // entry to the paths

  void print(FILE *fp);

  cond_const_path(CODEREP *c, BB_NODE *e):
    cr(c), entry(e) {}
};

typedef vector<cond_const_path> Set_of_paths;


static void print_paths(FILE *fp, Paths &paths)
{
  for (Paths::iterator p = paths.begin(); p != paths.end(); ++p) {
    fprintf(fp, "%d ", *p);
  }
  fprintf(fp, "\n");
}


void cond_const_path::print(FILE *fp)
{
  if (cr->Kind() == CK_VAR) {
    fprintf(fp, "  paths from cr%d in BB%d (value 0x%llx): ", 
	    cr->Coderep_id(),
	    cr->Defstmt()->Bb()->Id(),
	    cr->Defstmt()->Rhs()->Const_val());
  } else {
    fprintf(fp, "  paths for cr%d: ", cr->Coderep_id());
  }
  print_paths(fp, paths);
}


void print_set_of_paths(FILE *fp, Set_of_paths& set_of_paths)
{
  for (Set_of_paths::iterator s = set_of_paths.begin();
       s != set_of_paths.end();
       ++s) {
    (*s).print(fp);
  }
}



// trace_path will collect all paths starting at def_bb and ends at bb.
// paths is represented as a set of BBs.
//
static void trace_paths(BB_NODE *bb, BB_NODE *def_bb, Paths &paths)
{
  Is_True(def_bb->Dominates(bb), ("trace_paths: Definition must dominate use."));

  pair<Paths::iterator, bool> res = paths.insert(bb->Id());
  if (!res.second) // not inserted because the bb is already in the path
    return;
  if (bb == def_bb)  // stop when reached def_bb
    return;

  BB_NODE *pred;
  BB_LIST_ITER bb_pred_iter;
  FOR_ALL_ELEM( pred, bb_pred_iter, Init(bb->Pred())) 
    trace_paths(pred, def_bb, paths);
}


//  Find conditional constants:  at the position specified <bb,cr>
//
static bool find_conditional_const(BB_NODE *bb, CODEREP *cr, 
				   Set_of_paths &set_of_paths,
				   BB_LOOP *loop,
				   bool trace,
				   BVECTOR & visited,
				   BVECTOR & cr_visited)
{

  // avoid processing same bb or same cr 
  if (visited[bb->Id()] || cr_visited[cr->Coderep_id()]) return false;

  visited[bb->Id()] = true;
  cr_visited[cr->Coderep_id()] = true;

  if (!cr->Is_flag_set(CF_DEF_BY_PHI)) return false;

  PHI_NODE *phi = cr->Defphi();
  BB_NODE *bbp = phi->Bb();

  // do not go into higher nest level
  if (bbp->Innermost() != loop) return false;

  // do not go into lower nest level
  if (bbp->Loop() && bbp->Loop()->Header() == bbp) return false;

  bool found_conditional_const = false;

  for (INT i = 0; i < phi->Size(); ++i) {
    CODEREP *opnd = phi->OPND(i);
    
    if (!opnd->Is_flag_set(CF_IS_ZERO_VERSION) &&
	!opnd->Is_flag_set(CF_DEF_BY_PHI) &&
	!opnd->Is_flag_set(CF_DEF_BY_CHI)) {

      if (opnd->Defstmt() != NULL &&
	  opnd->Defstmt()->Rhs()->Kind() == CK_CONST) {
	set_of_paths.push_back(cond_const_path(opnd, phi->Bb()->Nth_pred(i)));
	found_conditional_const = true;
      }
    } else if (opnd->Is_flag_set(CF_DEF_BY_PHI)) {
      if (find_conditional_const(phi->Bb()->Nth_pred(i), opnd, set_of_paths, loop, trace, visited, cr_visited))
	found_conditional_const = true;
    }
  }
  if (found_conditional_const) {
    Paths paths;
    trace_paths(bb, phi->Bb(), paths);
    for (Set_of_paths::iterator p = set_of_paths.begin();
	 p != set_of_paths.end();
	 ++p) {
      Paths *cur_paths = &(*p).paths;
      (*cur_paths).insert(paths.begin(), paths.end());
    }
  }
  return found_conditional_const;
}


//  cmp is the test in bb,  
//  pred determines the incoming path to bb that we want to consider
//  context_sr and context_bb is the STMTREP and BB_NODE with the pre-defined
//  condition.
//  
static bool is_redundant_cmp(CODEREP *cmp, BB_NODE *bb, BB_NODE *pred, 
			     STMTREP *context_sr, BB_NODE *context_bb)
{
  Is_True(context_sr->Op() == OPC_TRUEBR || context_sr->Op() == OPC_FALSEBR,
	  ("is_redundant_cmp: expecting OPC_TRUEBR or OPC_FALSEBR"));

  BB_NODE *found_succ = NULL;
  BB_NODE *search_succ = (pred == context_bb) ? bb : pred;
    
  BB_LIST_ITER bb_iter;
  BB_NODE *succ;
  FOR_ALL_ELEM( succ, bb_iter, Init(context_bb->Succ()) ) {
    if (search_succ->Postdominates(succ)) {
      found_succ = succ;
      break;
    }
  }

  if (!found_succ) return false;

  COND_EVAL ce;
  if (context_sr->Op() == OPC_TRUEBR)
    ce = (found_succ == context_bb->Next()) ? EVAL_FALSE : EVAL_TRUE;
  else
    ce = (found_succ == context_bb->Next()) ? EVAL_TRUE : EVAL_FALSE;

  COND_EVAL res = Eval_redundant_cond_br( cmp, context_sr->Rhs(), ce);
  return (res == EVAL_TRUE || res == EVAL_FALSE);
}


static bool find_redundant_br(STMTREP *stmt, BB_NODE *bb, Set_of_paths &set_of_paths)
{
  BB_NODE *dom = bb->Idom();
  BB_NODE *pred;
  BB_LIST_ITER bb_pred_iter;

  bool found_redundant_br = false;
  FOR_ALL_ELEM( pred, bb_pred_iter, Init(bb->Pred())) {
    BB_NODE *cur = pred;
    while (cur && dom->Dominates(cur)) {
      STMTREP *br = cur->Branch_stmtrep();
      if (br && (br->Op() == OPC_TRUEBR || br->Op() == OPC_FALSEBR)) {
	if (is_redundant_cmp(stmt->Rhs(), bb, pred, br, cur)) {
	  set_of_paths.push_back(cond_const_path(stmt->Rhs(), pred));
	  (*(set_of_paths.end()-1)).paths.insert(bb->Id());
	  found_redundant_br = true;
	  break;
	}
      }
      cur = cur->Idom();
    }
  }
  return found_redundant_br;
}

static void add_to_zone_container(CFG *cfg, Set_of_paths &set_of_paths, zone_container& zones)
{
  for (Set_of_paths::iterator s = set_of_paths.begin();
       s != set_of_paths.end();
       ++s) {
    CODEREP *cr = (*s).cr;
    Paths *paths = &(*s).paths;
    BB_NODE *entry = (*s).entry;
    zones.push_back(zone(zones.size()));
    zone_container::iterator cur_zone = zones.end()-1;

    BB_LIST_ITER bb_succ_iter;
    BB_LIST_ITER bb_pred_iter;
    BB_NODE *succ;
    BB_NODE *pred;
    int entry_id = entry->Id();
    FOR_ALL_ELEM(succ, bb_succ_iter, Init(entry->Succ())) {
      int succ_id = succ->Id();
      if ((*paths).find(succ_id) != (*paths).end()) 
	(*cur_zone).entry.push_back(edge(entry_id, succ_id));
    }
    for (Paths::iterator p = (*paths).begin(); p != (*paths).end(); ++p) {
      int bb_id = *p;
      BB_NODE *bb = cfg->Get_bb(bb_id);
      FOR_ALL_ELEM(succ, bb_succ_iter, Init(bb->Succ())) {
	int succ_id = succ->Id();
	if ((*paths).find(succ_id) != (*paths).end()) 
	  (*cur_zone).clone.push_back(edge(bb_id, succ_id));
	else
	  (*cur_zone).exit.push_back(edge(bb_id, succ_id));
      }
      FOR_ALL_ELEM(pred, bb_pred_iter, Init(bb->Pred())) {
	int pred_id = pred->Id();
	if (pred_id != entry_id &&
	    (*paths).find(pred_id) == (*paths).end())
	   (*cur_zone).side_entry.push_back(edge(pred_id, bb_id));
      }
    }
  }
}


struct CONDITIONAL_CONST : public NULL_TRANSFORM {
  const char *Name() const { return "Scanning for conditional constant"; }
  COMP_UNIT *cu;
  zone_container *zones;
  bool  trace;
  bool  do_cond_const;

  CODEREP *Apply_cr(CODEREP *cr, bool is_mu, STMTREP *stmt, BB_NODE *bb, CODEMAP *htable) const 
  {
    if (!do_cond_const) return NULL;

    if (cr->Kind() == CK_VAR && !is_mu) {
      Set_of_paths set_of_paths;
      BVECTOR visited(cu->Cfg()->Total_bb_count(), false, BVECTOR_ALLOCATOR(cu->Cfg()->Loc_pool()));  
      BVECTOR cr_visited(htable->Coderep_id_cnt()+1, false, BVECTOR_ALLOCATOR(cu->Cfg()->Loc_pool()));
      bool found = find_conditional_const(bb, cr, set_of_paths, bb->Innermost(), trace, visited, cr_visited);
      if (found && trace) {
	fprintf(TFile, "CONDITIONAL CONST found for cr%d in BB%d\n", cr->Coderep_id(), bb->Id());
	print_set_of_paths(TFile, set_of_paths);
      }
      if (found) 
	add_to_zone_container(cu->Cfg(), set_of_paths, *zones);
    }
    return NULL;
  }

  void Apply_sr(STMTREP *stmt, BB_NODE *bb, CODEMAP *htable) 
  {
    do_cond_const = false;
    if (stmt->Op() == OPC_TRUEBR || stmt->Op() == OPC_FALSEBR) {
      do_cond_const = true;
      Set_of_paths set_of_paths;
      BOOL found = find_redundant_br(stmt, bb, set_of_paths);
      if (found && trace) {
	fprintf(TFile, "REDUNDANT BR found in BB%d\n", bb->Id());
	print_set_of_paths(TFile, set_of_paths);
      }
      if (found)
	add_to_zone_container(cu->Cfg(), set_of_paths, *zones);
    }
  }

  // Setup control which combinations are legal
  void Setup(PER_BB_CACHE *, DONT_TRACK_CUR_VERSION *) {}

  CONDITIONAL_CONST(COMP_UNIT *c, zone_container *r, bool t):
    cu(c),zones(r),trace(t) {}
};


void generate_conditional_const_zones(COMP_UNIT *cu, successor_graph &g, vector<zone>& zones, bool trace)
{
  OPT_POOL_Push(cu->Cfg()->Loc_pool(), -1);
  {
  CONDITIONAL_CONST conditional_const(cu, &zones, trace);
  UPDATE<CONDITIONAL_CONST, PER_BB_CACHE>
    SCAN_conditional_const(cu, &conditional_const, trace);
  SCAN_conditional_const.Process_PU();
  }
  OPT_POOL_Pop(cu->Cfg()->Loc_pool(), -1);
}
