//-*-c++-*-

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_exc.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_exc.cxx,v $
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
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#ifdef _KEEP_RCS_ID
#define opt_exc_CXX	"opt_exc.cxx"
static char *rcs_id = 	opt_exc_CXX"$Revision: 1.5 $";
#endif /* _KEEP_RCS_ID */

#include "defs.h"
#include "opt_bb.h"
#include "opt_cfg.h"
#include "opt_exc.h"
#include "opt_htable.h"

EXC_SCOPE::EXC_SCOPE(WN *begin_wn, EXC *exc)
{
  _exc = exc;
  _parent = NULL;
  _begin_wn = begin_wn;
  _vcall = NULL;
  _call_list = CXX_NEW(DYN_ARRAY<WN*>(exc->Mem_pool()), exc->Mem_pool());
}


// Returns TRUE if this exception region is for try region
BOOL
EXC_SCOPE::Is_try_region(void) const
{
  Is_True(WN_operator(_begin_wn) == OPR_REGION && REGION_is_EH(_begin_wn),
	  ("EXC_SCOPE::Is_try_region, invalid input"));
  WN *stmt, *pragmas = WN_region_pragmas(_begin_wn);
  STMT_ITER stmt_iter;
  FOR_ALL_ELEM(stmt, stmt_iter, Init(WN_first(pragmas),WN_last(pragmas))) {
    if (WN_operator(stmt) == OPR_GOTO)
      return TRUE;
  }
  return FALSE;
}

EXC_SCOPE_TRY_ITER::EXC_SCOPE_TRY_ITER(EXC_SCOPE *exc_scope)
{
  INT32 goto_label;
  BB_NODE *handler_bb;
  WN *optchi;

  _exc_scope = exc_scope;
  // local mem_pool for chi list copy
  OPT_POOL_Initialize(&_mem_pool, "EXC_SCOPE_TRY_ITER mem pool", 
		      FALSE, EXC_TRACE_FLAG);
  OPT_POOL_Push(&_mem_pool, EXC_TRACE_FLAG);

  _chi_list = NULL;

  if (_exc_scope->Is_try_region()) {
    // Creates the chi list for this call, may be a conglomerate of the
    // CHIs from many catches so need to allocate and copy
    WN *pragmas = WN_region_pragmas(_exc_scope->Begin_wn());
    WN *stmt;
    STMT_ITER stmt_iter;
    FOR_ALL_ELEM(stmt, stmt_iter, Init(WN_first(pragmas),WN_last(pragmas))) {
      if (WN_opcode(stmt) == OPC_GOTO) {
        goto_label = WN_label_number(stmt);
        handler_bb = _exc_scope->Exc()->Cfg()->Get_bb_from_label(goto_label);
        Is_True(handler_bb != NULL,
	        ("EXC_SCOPE_TRY_ITER::EXC_SCOPE_TRY_ITER NULL label BB"));
        optchi = handler_bb->Firststmt();
        Is_True(WN_operator(optchi) == OPR_OPT_CHI,
            ("EXC_SCOPE_TRY_ITER::EXC_SCOPE_TRY_ITER: cannot find chi-list"));
        CHI_LIST *chi =_exc_scope->Exc()->Opt_stab()->Get_stmt_chi_list(optchi);
        Is_True(chi != NULL,
	        ("EXC_SCOPE_TRY_ITER::EXC_SCOPE_TRY_ITER, NULL chi"));
  
        // now copy chi to conglomerate list
        CHI_NODE *cnode;
        if (_chi_list == NULL)
          _chi_list = CXX_NEW(CHI_LIST, &_mem_pool);
        FOR_ALL_NODE(cnode, _chi_iter, Init(chi))
	  _chi_list->Append(cnode->Copy_chi_node(&_mem_pool));
      }
    }
  }
  else if (WN_operator(_exc_scope->Begin_wn()) == OPR_REGION &&
	   REGION_is_EH(_exc_scope->Begin_wn())) { // Is_cleanup_region()
    WN *region_wn = _exc_scope->Begin_wn();
    INITO_IDX iidx = WN_ereg_supp(region_wn);
    INITV_IDX iv_idx = INITO_val(iidx);
    if (iv_idx != 0 && INITV_kind(iv_idx) == INITVKIND_BLOCK) {
      iv_idx = INITV_blk(iv_idx);
      if (INITV_kind(iv_idx) == INITVKIND_LABEL) {
        goto_label = INITV_lab(iv_idx);
        handler_bb = _exc_scope->Exc()->Cfg()->Get_bb_from_label(goto_label);
        Is_True(handler_bb != NULL,
	        ("EXC_SCOPE_TRY_ITER::EXC_SCOPE_TRY_ITER NULL label BB"));
	optchi = handler_bb->Firststmt();
	Is_True(WN_operator(optchi) == OPR_OPT_CHI,
	    ("EXC_SCOPE_TRY_ITER::EXC_SCOPE_TRY_ITER: cannot find chi-list"));
	CHI_LIST *chi =_exc_scope->Exc()->Opt_stab()->Get_stmt_chi_list(optchi);
	Is_True(chi != NULL,
		("EXC_SCOPE_TRY_ITER::EXC_SCOPE_TRY_ITER, NULL chi"));

	// now copy chi to conglomerate list
	CHI_NODE *cnode;
        if (_chi_list == NULL)
	  _chi_list = CXX_NEW(CHI_LIST, &_mem_pool);
	FOR_ALL_NODE(cnode, _chi_iter, Init(chi))
	  _chi_list->Append(cnode->Copy_chi_node(&_mem_pool));
      }
    }
  }

  // set up iterator
  _chi_iter.Init(_chi_list);
}

AUX_ID
EXC_SCOPE_TRY_ITER::Elem(CHI_NODE *chi)
{
  if (chi == NULL) return 0;
  if (!chi->Live()) return 0;
  AUX_ID    aux_id = chi->Aux_id();
  POINTS_TO *pt = _exc_scope->Exc()->Opt_stab()->Points_to(aux_id);
  if (pt->Local())
    return aux_id;
  return 0; // null aux_id
}

EXC_SCOPE*
EXC::Push_exc_scope(WN *scope_begin)
{
  if (_exc_scope == NULL)
    return NULL;
  EXC_SCOPE *exc_scope = CXX_NEW( EXC_SCOPE(scope_begin, this), _mem_pool);
  EXC_SCOPE *top = (NULL_exc_scope()) ? NULL : Top_exc_scope();
  exc_scope->Set_parent(top);
  Push_exc_scope(exc_scope);
  _exc_scope_list->AddElement(exc_scope);
  return exc_scope;
}

// point the call to the enclosing exc_scope
void
EXC::Link_top_es(WN *wn)
{
  if (_exc_scope == NULL || NULL_exc_scope())
    return;
  EXC_SCOPE *es = Top_exc_scope();
  Link_wn_es(wn, es);
}

