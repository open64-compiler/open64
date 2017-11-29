//-*-c++-*-

/*
 *  Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_transform.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_transform.h,v $
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

#ifndef opt_transform_INCLUDED
#define opt_transform_INCLUDED	

#include "defs.h"
#include "stab.h"
#include "opt_defs.h"
#include "opt_config.h"
#include "opt_base.h"
#include "opt_bb.h"
#include "opt_sym.h"
#include "opt_htable.h"
#include "opt_cfg.h"
#include "opt_mu_chi.h"
#include "opt_ssa.h"
#include "opt_main.h"

// Search in a CODEREP expr
//
template <class Pred>
BOOL find(CODEREP *cr, Pred pred) 
{
  if (pred(cr)) return TRUE;

  switch ( cr->Kind() ) {
  case CK_IVAR:
    {
      CODEREP *base =
	(cr->Istr_base()) ? cr->Istr_base() : cr->Ilod_base();
      if (find(cr, pred))
	return TRUE;
      if (cr->Opr() == OPR_MLOAD &&
	  find(cr->Mload_size(), pred))
	return TRUE;
      if (cr->Opr() == OPR_ILOADX &&
	  find(cr->Index(), pred))
	return TRUE;
      MU_NODE *mnode = cr->Ivar_mu_node();
      if (mnode) {
	CODEREP *opnd = mnode->OPND();
	if (opnd && find(opnd, pred))
	  return TRUE;
      }
    }
    return FALSE;

  case CK_OP:
    // make sure all of the kids are invariant
    for (INT32 ikid = 0; ikid < cr->Kid_count(); ikid++ ) {
      if (find(cr->Opnd(ikid), pred))
	return TRUE;
    }
    return FALSE;
  }
  return FALSE;
}


struct DONT_TRACK_CUR_VERSION {
  void Enter_BB(BB_NODE *bb, CODEMAP *htable) { }
  void Enter_SR(STMTREP *stmt, BB_NODE *bb, CODEMAP *htable) { }
  void Exit_SR(STMTREP *stmt, BB_NODE *bb, CODEMAP *htable) { }
  void Exit_BB(BB_NODE *bb, CODEMAP *htable) { }
  DONT_TRACK_CUR_VERSION(COMP_UNIT *cu) {}
};



//  Track current version
struct TRACK_CUR_VERSION {
  MEM_POOL          _mem_pool;
  STACK<CODEREP*> **_vec_of_stack;
  OPT_STAB         *_opt_stab;

  OPT_STAB    *Opt_stab() const { return _opt_stab; }

  void Push(AUX_ID id, CODEREP *cr) { _vec_of_stack[id]->Push(cr); }
  void Pop(AUX_ID id) { _vec_of_stack[id]->Pop(); }
  int  Size(AUX_ID id) const { return _vec_of_stack[id]->Elements(); }
  CODEREP *Top(AUX_ID id) const { return _vec_of_stack[id]->Top(); }
  CODEREP *Non_zero_top(AUX_ID id) {
    int n_elm = Size(id);
    for (int i = 0; i < n_elm; i++) {
      CODEREP *cr = _vec_of_stack[id]->Top_nth(i);
      if (!cr->Is_flag_set(CF_IS_ZERO_VERSION))
	return cr;
    }
    return NULL;
  }
  BOOL Is_volatile(AUX_ID id) { return _opt_stab->Is_volatile(id); }

  void Print_stack(AUX_ID id, FILE *fp) {
    STACK<CODEREP*> *s = _vec_of_stack[id];
    for (int i = 0; i < s->Elements(); ++i) {
      CODEREP *top = s->Top_nth(i);
      if (top)
	fprintf(fp, "cr%d ", top->Coderep_id());
      else
	fprintf(fp, "null ");
    }
    fprintf(fp,"\n");
  }

  void Enter_BB(BB_NODE *bb, CODEMAP *htable) {
    PHI_NODE *phi; 
    PHI_LIST_ITER phi_iter;
    //  Iterate through each phi-node 
    FOR_ALL_ELEM (phi, phi_iter, Init(bb->Phi_list())) {
      if (phi->Live()) 
	Push(phi->Aux_id(), phi->RESULT());
      else 
	Push(phi->Aux_id(), 
	     htable->Ssa()->Get_zero_version_CR(phi->Aux_id(), Opt_stab(), 0));
    }
  }
  
  void Enter_SR(STMTREP *stmt, BB_NODE *bb, CODEMAP *htable) {
    if (OPERATOR_is_scalar_store (stmt->Opr())) {
      CODEREP *lhs = stmt->Lhs();
      Push(lhs->Aux_id(), lhs);
    }
    if (stmt->Has_chi()) {
      CHI_LIST_ITER chi_iter;
      CHI_NODE *cnode;
      CHI_LIST *chi_list = stmt->Chi_list();
      // ENTRY CHI are different.  It needs to provide at least one
      // definition for each use (including dead uses)!
      if (stmt->Opr() == OPR_OPT_CHI) {
	FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
	  if (!cnode->Dse_dead())
	    Push(cnode->Aux_id(), cnode->RESULT());
	  else 
	    Push(cnode->Aux_id(),
		 htable->Ssa()->Get_zero_version_CR(cnode->Aux_id(), Opt_stab(), 0));
	} 
      } else {
	FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
	  if (cnode->Live())
	    Push(cnode->Aux_id(), cnode->RESULT());
	}
      }
    }
  }

  void Exit_SR(STMTREP *stmt, BB_NODE *bb, CODEMAP *htable) {
    if (stmt->Has_chi()) {
      CHI_LIST_ITER chi_iter;
      CHI_NODE *cnode;
      CHI_LIST *chi_list = stmt->Chi_list();
      if (stmt->Opr() == OPR_OPT_CHI) {
	FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
	  Pop(cnode->Aux_id());
	} 
      }else {
	FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
	  if (cnode->Live())
	    Pop(cnode->Aux_id());
	}
      }
    } 
    if (OPERATOR_is_scalar_store (stmt->Opr()))
      Pop(stmt->Lhs()->Aux_id());
  }

  void Exit_BB(BB_NODE *bb, CODEMAP *htable) {
    PHI_LIST_ITER phi_iter;
    PHI_NODE *phi;
    FOR_ALL_ELEM (phi, phi_iter, Init(bb->Phi_list())) {
      Pop(phi->Aux_id());
    }
  }

  TRACK_CUR_VERSION(COMP_UNIT *cu):
    _opt_stab(cu->Opt_stab())
  {
    OPT_POOL_Initialize(&_mem_pool, "Current version pool", FALSE, 0);
    OPT_POOL_Push(&_mem_pool, IVR_DUMP_FLAG); 
    INT32 count = Opt_stab()->Lastidx()+1;
    _vec_of_stack = (STACK<CODEREP*> **)CXX_NEW_ARRAY( STACK<CODEREP*>*, count, &_mem_pool);
    for (INT32 i = 0; i < count; i++) {
      _vec_of_stack[i] = CXX_NEW(STACK<CODEREP*>(&_mem_pool), &_mem_pool);
    }
   }
  ~TRACK_CUR_VERSION() {
    OPT_POOL_Pop(&_mem_pool, 0); 
    OPT_POOL_Delete(&_mem_pool, 0);
  }
};

struct leaf_is_not_cur_ver_pred {
  TRACK_CUR_VERSION *cur_ver;
  BOOL operator()(CODEREP *cr) {
    if (cr->Kind() == CK_VAR && cur_ver->Top(cr->Aux_id()) != cr)
      return TRUE;
    else
      return FALSE;
  }
  leaf_is_not_cur_ver_pred(TRACK_CUR_VERSION *t):cur_ver(t) {}
};


inline BOOL is_current_version(TRACK_CUR_VERSION *cur_ver, CODEREP *cr) {
  return ! find(cr, leaf_is_not_cur_ver_pred(cur_ver));
} 



struct NO_CACHE {
  typedef COMP_UNIT* key_type;
  const char *_name;
  INT32     _count_unique;      // unique expr simplified
  INT32     _count_total;       // total number of expressions simplified
  INT32     _count_processed;
  BOOL      _trace;

  CODEREP *Lookup(CODEREP *, COMP_UNIT *) { return NULL; }
  void     Update(CODEREP *, CODEREP *newcr, COMP_UNIT *) {
    if (newcr) {
      ++_count_unique;
      ++_count_total;
    }
    ++_count_processed;
  }

  INT32 Count_unique() const { return _count_unique; }
  INT32 Count_total()  const { return _count_total; }
  INT32 Count_processed()  const { return _count_processed; }
  BOOL  Trace() const { return _trace; }

  NO_CACHE(const char *name, BOOL trace):
    _name(name),
    _count_unique(0),
    _count_total(0),
    _count_processed(0),
    _trace(trace)
  {}
  ~NO_CACHE() {
    if (Trace()) {
      fprintf(TFile, "CACHE<%s>: unique=%d, total=%d, processed=%d\n",
	      _name,
	      Count_unique(), 
	      Count_total(),
	      Count_processed());
    }
  }
};


struct PER_PU_CACHE : public NO_CACHE {
  MEM_POOL      _mem_pool;
  CODEMAP      *_htable; 
  CODEREP     **_cached_result;    // indexed by coderep-id

  CODEREP *Lookup(CODEREP *cr, key_type key) {
    INT32 id = cr->Coderep_id();
    if (_cached_result[id] != NULL)
      ++_count_total;
    return _cached_result[id];
  }

  void Update(CODEREP *cr, CODEREP *newcr, key_type key) {
    INT32 id = cr->Coderep_id();
    _cached_result[id] = newcr;
    if (newcr) {
      ++_count_unique;
      ++_count_total;
    }
    ++_count_processed;
  }

  PER_PU_CACHE(COMP_UNIT *cu, const char *name, BOOL trace):
    NO_CACHE(name, trace),
    _htable(cu->Htable()) {
      OPT_POOL_Initialize(&_mem_pool, "SIMP BOOL pool", FALSE, 0);
      OPT_POOL_Push(&_mem_pool, IVR_DUMP_FLAG); 
      _cached_result = (CODEREP **) 
	CXX_NEW_ARRAY(CODEREP *, _htable->Coderep_id_cnt(), &_mem_pool);
      for (INT32 i = 0; i < _htable->Coderep_id_cnt(); i++) 
	_cached_result[i] = NULL;
  }
  ~PER_PU_CACHE() {
    OPT_POOL_Pop(&_mem_pool, 0); 
    OPT_POOL_Delete(&_mem_pool, 0);
  }
};


template <class CACHE_KEY>     // e.g., CACHE_KEY usually are STMTREP*, or BB_NODE*
struct CACHE_TEMPLATE : public PER_PU_CACHE {
  typedef CACHE_KEY key_type;
  key_type   *_visited;          // indexed by coderep-id

  CODEREP *Lookup(CODEREP *cr, key_type key) {
    INT32 id = cr->Coderep_id();
    if (_visited[id] == key) {
      if (_cached_result[id] != NULL)
	++_count_total;
      return _cached_result[id];
    }
    return NULL;
  }

  void Update(CODEREP *cr, CODEREP *newcr, key_type key) {
    INT32 id = cr->Coderep_id();
    _visited[id] = key;
    _cached_result[id] = newcr;
    if (newcr) {
      ++_count_unique;
      ++_count_total;
    }
    ++_count_processed;
  }

  void Clear_visited() {
    for (INT32 i = 0; i < _htable->Coderep_id_cnt(); i++) 
      _visited[i] = NULL;
  }

  CACHE_TEMPLATE(COMP_UNIT *cu, const char *name, BOOL trace):
    PER_PU_CACHE(cu, name, trace) {
      _visited = (CACHE_KEY *) 
	CXX_NEW_ARRAY(CACHE_KEY* , _htable->Coderep_id_cnt(), &_mem_pool);
      Clear_visited();
  }
  ~CACHE_TEMPLATE(void) { }
};



inline STMTREP   *Get_cache_key(STMTREP *, STMTREP *stmt, BB_NODE *bb) { return stmt; }
inline BB_NODE   *Get_cache_key(BB_NODE *, STMTREP *stmt, BB_NODE *bb) { return bb; }
inline COMP_UNIT *Get_cache_key(COMP_UNIT *, STMTREP *stmt, BB_NODE *bb) { return NULL; }



typedef CACHE_TEMPLATE<STMTREP*>   PER_SR_CACHE;
typedef CACHE_TEMPLATE<BB_NODE*>   PER_BB_CACHE;





#ifdef Is_True_On
// Debugging support for UPDATE template
void set_transform_stop_at(int bb_id, int stmt_id);
void transform_stopped();
void check_transform_stop_at(int bb_id, int stmt_id);
#endif

template <class TRANSFORM, class CACHE = PER_PU_CACHE, class VERSION = DONT_TRACK_CUR_VERSION>
struct UPDATE {
  CODEMAP          *_htable; 
  CFG              *_cfg;
  BOOL              _trace;
  TRANSFORM        *_trans;      // coderep transform function
  VERSION           _version;
  CACHE             _cache;

  CODEMAP *Htable() const { return _htable; }
  CODEREP *Process_CR_no_repeat(CODEREP *cr, bool is_mu, STMTREP *stmt, BB_NODE *bb);
  void     Process_PU(BB_NODE *bb);
  
  public:
  UPDATE(COMP_UNIT *cu, TRANSFORM *trans, BOOL trace):
    _htable(cu->Htable()),
    _cfg(cu->Cfg()),
    _trace(trace),
    _trans(trans),
    _version(cu),
    _cache(cu, trans->Name(), trace) {
    trans->Setup(&_cache, &_version); 
  }
  ~UPDATE() {
  }

  void     Process_PU() { Process_PU(_cfg->Entry_bb()); }
  void     Process_BB(BB_NODE *bb);
  void     Process_SR(STMTREP *stmt, BB_NODE *bb);
  CODEREP *Process_CR(CODEREP *cr, bool is_mu, STMTREP *stmt, BB_NODE *bb);
};



template <class TRANSFORM, class CACHE, class VERSION>
CODEREP *
UPDATE<TRANSFORM, CACHE, VERSION>::Process_CR(CODEREP *cr, bool is_mu, STMTREP *stmt, BB_NODE *bb)
{
  typename CACHE::key_type key = Get_cache_key((typename CACHE::key_type)NULL, stmt, bb);
  CODEREP *newcr = _cache.Lookup(cr, key);
  if (newcr == cr)
    return NULL;
  if (newcr)
    return newcr;
  newcr = Process_CR_no_repeat(cr, is_mu, stmt, bb);
  _cache.Update(cr, newcr != NULL ? newcr : cr, key);
  return newcr;
}  


template <class TRANSFORM, class CACHE, class VERSION>
CODEREP *
UPDATE<TRANSFORM, CACHE, VERSION>::Process_CR_no_repeat(CODEREP *cr, bool is_mu, STMTREP *stmt, BB_NODE *bb)
{
  CODEREP *simp = _trans->Apply_cr(cr, is_mu, stmt, bb, Htable());
  if (simp) {
    if (_trace) {
      fprintf(TFile, "UPDATE<%s>: BB%d\n", _trans->Name(), bb->Id());
      cr->Print(10, TFile);
      simp->Print(10, TFile);
    }
    return simp;
  }

  switch (cr->Kind()) {
  case CK_LDA: 
  case CK_CONST: 
  case CK_RCONST:
  case CK_VAR: 
    return NULL;
  case CK_IVAR: 
    {
      CODEREP *ilod_base = Process_CR(cr->Ilod_base(), false, stmt, bb);
      CODEREP *mload_size = (cr->Opr() == OPR_MLOAD) ? 
	Process_CR(cr->Mload_size(), false, stmt, bb) : NULL;

      CODEREP *mu = NULL;
      if (cr->Ivar_mu_node())
	mu = Process_CR(cr->Ivar_mu_node()->OPND(), true, stmt, bb);
      
      if (ilod_base || mload_size || mu) {
	CODEREP *newcr = Alloc_stack_cr(cr->Extra_ptrs_used());
	newcr->Copy(*cr);  
#ifdef KEY // bug 12390: without this, would do wrong ivar copy propagation
	newcr->Set_ivar_defstmt(NULL);
#endif
	if (ilod_base) 
	  newcr->Set_ilod_base(ilod_base);
	newcr->Set_istr_base(NULL);
	if (mload_size)
	  newcr->Set_mload_size(mload_size);
	if (mu) {
	  // Fix 622235, 621101:
	  //   Clone the mu-node.  Update the version of mu-operand.
	  //   Rehash the CK_IVAR node if mu-operand has been changed.
	  MU_NODE *mnode = (MU_NODE*) CXX_NEW(MU_NODE, Htable()->Mem_pool());
	  mnode->Clone(cr->Ivar_mu_node());
	  mnode->Set_OPND(mu);
	  newcr->Set_ivar_mu_node(mnode);
	}
	newcr->Set_ivar_occ(cr->Ivar_occ());

	CODEREP *ret = Htable()->Add_expr_and_fold(newcr);
	// Fix 620842:  copy prop expects all C_P_PROCESSED must be cleared
	//  only clear newly rehashed ones,  opt_rename.cxx does not see
	//  these nodes and therefore not able to clear them.
	ret->Reset_flag(CF_C_P_PROCESSED);
	ret->Reset_flag(CF_C_P_REHASHED);
	return ret;
      } 
      break;
    }
  case CK_OP: 
    {
      BOOL need_rehash = FALSE;
      CODEREP *newcr = Alloc_stack_cr(cr->Extra_ptrs_used());
      newcr->Copy(*cr);  
      for (INT32 i = 0; i < cr->Kid_count(); i++) {
	CODEREP *opnd = Process_CR(cr->Opnd(i), false, stmt, bb);
	if (opnd) {
	  need_rehash = TRUE;
	  newcr->Set_opnd(i, opnd);
	}
      }
      if (need_rehash) {
	CODEREP *ret = Htable()->Add_expr_and_fold(newcr);
	// Fix 620842:  copy prop expects all C_P_PROCESSED must be cleared
	ret->Reset_flag(CF_C_P_PROCESSED);
// Fix bug 1614
#ifdef KEY
        if (ret->Kind() == CK_OP)
#endif
	  ret->Reset_flag(CF_C_P_REHASHED);
	return ret;
      }
      break;
    }
  }
  return NULL;
}


template <class TRANSFORM, class CACHE, class VERSION>
void
UPDATE<TRANSFORM, CACHE, VERSION>::Process_SR(STMTREP *stmt, BB_NODE *bb)
{
  _trans->Apply_sr(stmt, bb, Htable());

  CODEREP *cr;
  if (stmt->Rhs()) {
    if (cr = Process_CR(stmt->Rhs(), false, stmt, bb))
      stmt->Set_rhs(cr);
  }
  switch (stmt->Opr()) { 
  case OPR_ISTORE:
  case OPR_ISTBITS:
    cr = Process_CR(stmt->Lhs()->Istr_base(), false, stmt, bb);
    if (cr) 
      stmt->Lhs()->Set_istr_base(cr);
    break;
  case OPR_MSTORE:
    cr = Process_CR(stmt->Lhs()->Istr_base(), false, stmt, bb);
    if (cr) 
      stmt->Lhs()->Set_istr_base(cr);
    CODEREP *num_bytes = (CODEREP *)stmt->Lhs()->Mstore_size();
    cr = Process_CR(num_bytes, false, stmt, bb);
    if (cr)
      stmt->Lhs()->Set_mstore_size(cr);
    break;
  }
}


template <class TRANSFORM, class CACHE, class VERSION>
void
UPDATE<TRANSFORM, CACHE, VERSION>::Process_BB(BB_NODE *bb)
{
  _trans->Apply_bb(bb, Htable());

  _version.Enter_BB(bb, Htable());

#ifdef Is_True_On
  int stmt_count = 0;
#endif

  // iterate through each statement in this bb
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {
#ifdef Is_True_On
    check_transform_stop_at(bb->Id(), stmt_count++);
#endif
    Process_SR(stmt, bb);
    _version.Enter_SR(stmt, bb, Htable());
  }

  _trans->Apply_bb_post(bb, Htable());
}


template <class TRANSFORM, class CACHE, class VERSION>
void
UPDATE<TRANSFORM, CACHE, VERSION>::Process_PU(BB_NODE *bb)
{
  Process_BB(bb);

  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb->Dom_bbs())) {
    Process_PU(dom_bb);
  }

  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE_REVERSE(stmt, stmt_iter, Init()) {
    _version.Exit_SR(stmt, bb, Htable());
  }
  _version.Exit_BB(bb, Htable());
}


struct NULL_TRANSFORM {
  //  These functions should be preempted by the TRANSFORM definitions
  const char *Name() const { return "undefined"; }
  CODEREP *Apply_cr(CODEREP *cr, bool is_mu, STMTREP *stmt, BB_NODE *bb, CODEMAP *htable) const { return NULL; }
  void     Apply_sr(STMTREP *stmt, BB_NODE *bb, CODEMAP *htable) const { }
  void     Apply_sr_post(STMTREP *stmt, BB_NODE *bb, CODEMAP *htable) const { }
  void     Apply_bb(BB_NODE *bb, CODEMAP *htable) const { }
  void     Apply_bb_post(BB_NODE *bb, CODEMAP *htable) const { }
};


// substitue a CR with another
//
struct SUBSTITUE : public NULL_TRANSFORM {
  const char  *Name() const { return "Substitute"; }
  CODEREP *_oldcr;
  CODEREP *_newcr;
  CODEREP *Apply_cr(CODEREP *cr, bool is_mu, STMTREP *stmt, BB_NODE *, CODEMAP *htable) const
  {
    if (cr == _oldcr)
      return _newcr;
    else 
      // update mu and might generate a new expr
      return NULL;
  }

  void Init(CODEREP *o, CODEREP *n) { _oldcr = o; _newcr = n; }

  void Setup(PER_SR_CACHE *, DONT_TRACK_CUR_VERSION *) {}

  SUBSTITUE():
    _oldcr(NULL),
    _newcr(NULL) {}
};



#endif // opt_transform_INCLUDED
