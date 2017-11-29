/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_mu_chi.cxx
// $Revision$
// $Date$
// $Author$
// $Source$
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


#ifdef _KEEP_RCS_ID
#define opt_mu_chi_CXX	"opt_mu_chi.cxx"
static char *rcs_id = 	opt_mu_chi_CXX"$Revision$";
#endif /* _KEEP_RCS_ID */

#include "defs.h"
#include "opt_alias_class.h"
#include "opt_defs.h"
#include "opt_config.h"
#include "opt_base.h"
#include "opt_mu_chi.h"
#include "opt_htable.h"
#include "opt_ssa.h"
#include "opt_bb.h"
#include "pf_cg.h"


OCC_TAB_ENTRY *
#ifdef KEY
OPT_STAB::Enter_occ_tab(WN *wn, AUX_ID aux_id, POINTS_TO *pt)
#else
OPT_STAB::Enter_occ_tab(WN *wn, AUX_ID aux_id)
#endif
{
  OCC_TAB_ENTRY *occ = CXX_NEW(OCC_TAB_ENTRY, Occ_pool());
  occ->Set_aux_id(aux_id);
  occ->Set_wn(wn);
  occ->Set_lno_dep_vertex_load(0);
  occ->Set_lno_dep_vertex_store(0);

  // Note that Is_stmt() is false for many statements -- ISTORE is one
  // example. See the call sites for Enter_occ_tab() in
  // opt_alias_analysis.cxx, and the definition if Is_stmt() in
  // opt_mu_chi.h.
  if (occ->Is_stmt()) {
    occ->Set_stmt_mu_list( CXX_NEW(MU_LIST, Occ_pool()));
    occ->Set_stmt_chi_list( CXX_NEW(CHI_LIST, Occ_pool()));
    occ->Set_pf_list(NULL);
    occ->Set_pt_list(NULL);
  } else {
#ifdef KEY
    if (WOPT_Enable_New_Vsym_Allocation && pt /* probably temporary */) {
      occ->Points_to()->Copy_fully (pt);
    } else
#endif
    {
      occ->Points_to()->Init();
      // HACK: subject to review
      if (WN_operator(wn) == OPR_PARM && aux_id == _default_vsym)
        occ->Points_to()->Set_expr_kind(EXPR_IS_ANY);
      occ->Points_to()->Set_base_kind(BASE_IS_UNKNOWN);
      occ->Points_to()->Set_ofst_kind(OFST_IS_INVALID);
    }
    // preserve LNO dependence info
    if (occ->Is_load()) {
      occ->Set_mem_mu_node(NULL);
      INT32 vertex = WN_get_dep_graph_vertex(wn);   
      WN_detach_wn_from_dep_graph(vertex);
      occ->Set_lno_dep_vertex_load(vertex);
    } else {
      occ->Set_mem_chi_list( CXX_NEW(CHI_LIST, Occ_pool()));
      INT32 vertex = WN_get_dep_graph_vertex(wn);   
      WN_detach_wn_from_dep_graph(vertex);
      occ->Set_lno_dep_vertex_store(vertex);
    }
    if (! OPERATOR_is_scalar_store (WN_operator(wn)))
      Set_virtual_var(aux_id);  // put aux_id in the list of virtual var
    PF_POINTER *pf_pointer = WN_get_pf_pointer(wn);
    occ->Set_pf_pointer(pf_pointer);

    // transfer the mem-op annotation 
    if ((occ->Is_load() || occ->Is_store()) && 
      WN_MEMOP_ANNOT_MGR::WN_mem_annot_mgr()) {
      MEMOP_ANNOT* annot = WN_MEMOP_ANNOT_MGR::WN_mem_annot_mgr()->Get_annot(wn);
      if (annot) {
        MEMOP_ANNOT* t = Cr_sr_annot_mgr()->Import_annot (annot);
        occ->Points_to()->Mem_annot().Set_annots (t);
      }
    }
  }

  WN_MAP_Set(WN_sym_map(), wn, occ);

  return occ;
}


//  Update the WN * in the prefetch info node to point
//  to the STMTREP.  It will be replaced with another WN again
//  at emit time.
//
void
OPT_STAB::Update_pf_list(WN *pref_wn, STMTREP *srep)
{
  PF_LIST *pf_list = Get_occ(pref_wn)->Pf_list();
  PF_POINTER *p;
  PF_NODE *pf_node;
  PF_LIST_ITER pf_iter;
  FOR_ALL_NODE(pf_node, pf_iter, Init(pf_list)) {
    p = pf_node->Pf_pointer();
    if (PF_PTR_wn_pref_1L(p) == pref_wn)
      PF_PTR_wn_pref_1L(p) = (WN *) srep;
    if (PF_PTR_wn_pref_2L(p) == pref_wn)
      PF_PTR_wn_pref_2L(p) = (WN *) srep;
  }
}



OCC_TAB_ENTRY *
OPT_STAB::Get_occ(const WN *wn) const
{
  OCC_TAB_ENTRY *occ = (OCC_TAB_ENTRY *) WN_MAP_Get(WN_sym_map(), wn);
  return occ;
}

CHI_LIST *
OPT_STAB::Get_mem_chi_list(const WN *wn) const
{
  OCC_TAB_ENTRY *occ = (OCC_TAB_ENTRY *) WN_MAP_Get(WN_sym_map(), wn);
  return (occ == NULL) ? NULL : occ->Mem_chi_list();
}

CHI_LIST *
OPT_STAB::Get_stmt_chi_list(const WN *wn) const
{
  OCC_TAB_ENTRY *occ = (OCC_TAB_ENTRY *) WN_MAP_Get(WN_sym_map(), wn);
  return (occ == NULL) ? NULL : occ->Stmt_chi_list();
}

// slower version
CHI_LIST *
OPT_STAB::Get_generic_chi_list(const WN *wn) const
{
  OCC_TAB_ENTRY *occ = (OCC_TAB_ENTRY *) WN_MAP_Get(WN_sym_map(), wn);
  if (occ == NULL) return NULL;
  return occ->Is_stmt() ? occ->Stmt_chi_list() : occ->Mem_chi_list();
}


MU_NODE *
OPT_STAB::Get_mem_mu_node(const WN *wn) const
{
  OCC_TAB_ENTRY *occ = (OCC_TAB_ENTRY *) WN_MAP_Get(WN_sym_map(), wn);
  if (occ == NULL) return NULL;
  Is_True(occ->Wn() == wn,
          ("OPT_STAB::Get_mem_mu_node: occ gets attached to wrong wn"));
  return occ->Mem_mu_node();
}


MU_LIST *
OPT_STAB::Get_stmt_mu_list(const WN *wn) const
{
  OCC_TAB_ENTRY *occ = (OCC_TAB_ENTRY *) WN_MAP_Get(WN_sym_map(), wn);
  if (occ == NULL) return NULL;
  Is_True(occ->Wn() == wn,
          ("OPT_STAB::Get_stmt_mu_list: occ gets attached to wrong wn"));
  return occ->Stmt_mu_list();
}
  

void
OPT_STAB::Update_iload_vsym(OCC_TAB_ENTRY *occ)
{
  BOOL trace = Get_Trace(TP_GLOBOPT, ALIAS_DUMP_FLAG);

  if (occ->Points_to()->Base_kind() != BASE_IS_FIXED)
    return;
  if (occ->Points_to()->Unique_pt() || 
      occ->Points_to()->Restricted())
    return;
    
  Is_True(occ->Points_to()->Base_kind() == BASE_IS_FIXED, ("base is not fixed."));
  ST *st = occ->Points_to()->Base();
  AUX_ID vsym_id;
  if (occ->Points_to()->Ofst_kind() == OFST_IS_FIXED) {
    INT64 byte_ofst = occ->Points_to()->Byte_Ofst();
    INT64 byte_size = occ->Points_to()->Byte_Size();
    UINT8 bit_ofst = occ->Points_to()->Bit_Ofst();
    UINT8 bit_size = occ->Points_to()->Bit_Size();
    vsym_id = Find_vsym_with_base_ofst_and_size(st, byte_ofst, byte_size,
						bit_ofst, bit_size);
  } else
    vsym_id = Find_vsym_with_base_ofst_and_size(st, 0, 0, 0, 0);
  if (vsym_id == 0) return;
  
  // track list of values referenced here (for RVI)
  if (aux_stab[vsym_id].Aux_id_list() == NULL) 
    Update_aux_id_list(vsym_id);

  // update alias info for the virtual variable
  POINTS_TO *pt = aux_stab[vsym_id].Points_to();
  pt->Meet(occ->Points_to(), NULL);
  Is_True(vsym_id == Default_vsym() || pt->Base_is_fixed() || pt->Based_sym() 
	  || aux_stab[vsym_id].Unique_vsym(), ("base is disrupted."));

  MU_NODE *mu = occ->Mem_mu_node();
  VER_ID ver = mu->Opnd();
  while (1) {
    INT32 vtype = Ver_stab_entry(ver)->Type();
    switch (vtype) {
    case ENTRY_STMT:
      // not handled
      return;
    case WHIRL_STMT:
    case CHI_STMT:
      {
	WN *wn;
	if (vtype == WHIRL_STMT) {
	  wn = Ver_stab_entry(ver)->Wn();
	}
	else {
	  wn = Ver_stab_entry(ver)->Chi_wn();
	  if (OPERATOR_is_scalar_store ( WN_operator(wn) )) {
	    if ( Du_aux_id(WN_ver(wn)) == vsym_id ) {
	      occ->Set_aux_id(vsym_id);
	      mu->Set_aux_id(vsym_id);
	      mu->Set_opnd(WN_ver(wn));
	      return;
	    }
	  }
	}
	
	CHI_LIST *chi_list = Get_generic_chi_list(wn);
	Is_True(chi_list, ("chi list is null."));
	CHI_LIST_ITER chi_iter;
	CHI_NODE *cnode;
	FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
	  if (cnode->Aux_id() == vsym_id) {
	    occ->Set_aux_id(vsym_id);
	    mu->Set_aux_id(vsym_id);
	    mu->Set_opnd(cnode->Result());
	    cnode->Set_live(TRUE);
	    if (trace) {
	      fprintf(TFile, "Update_iload_vsym: set aux_id %d chi live.\n",
		      cnode->Aux_id());
	    }
	    // ssa->Resurrect_chi(cnode);
	    return;
	  }
	}
	// follows the ud-chain of the original vsym
	ver = Ver_stab_entry(ver)->Chi()->Opnd();
      }
      break;
    case PHI_STMT:
      {
	BB_NODE *bb = Ver_stab_entry(ver)->Bb();
	PHI_LIST_ITER phi_iter;
	PHI_NODE *phi;
	FOR_ALL_ELEM (phi, phi_iter, Init(bb->Phi_list())) {
	  if (phi->Aux_id() == vsym_id) {
	    occ->Set_aux_id(vsym_id);
	    mu->Set_aux_id(vsym_id);
	    mu->Set_opnd(phi->Result());
	    phi->Set_live();
	    if (trace) {
	      fprintf(TFile, "Update_iload_vsym: set aux_id %d phi at BB%d to live.\n",
		      phi->Aux_id(), bb->Id());
	    }
	      // ssa->Resurrect_phi(phi);
	    return;
	  }
	}
	
	phi = Ver_stab_entry(ver)->Phi();
	// If the desired vsym is not defined in the loop, 
	// continue chasing the use-def chain of the default vsym.
	BB_NODE *pred;
	BB_LIST_ITER bb_iter;
	ver = (VER_ID) -1;
	INT32 i = -1;
	FOR_ALL_ELEM(pred, bb_iter, Init(bb->Pred())) {
	  i++;
	  if (bb->Dominates(pred)) continue;
	  if (ver != -1) return;  // found two definition outside of loop
	  ver = phi->Opnd(i);     // get next ver
	}
	// return;
      }
    }
  }
}


void
OPT_STAB::Update_istore_vsym(OCC_TAB_ENTRY *occ)
{
  if (occ->Points_to()->Base_kind() != BASE_IS_FIXED)
    return;
  if (occ->Points_to()->Unique_pt() ||
      occ->Points_to()->Restricted())
    return;

  Is_True(occ->Points_to()->Base_kind() == BASE_IS_FIXED, ("base is not fixed."));
  ST *st = occ->Points_to()->Base();
  AUX_ID vsym_id;
  if (occ->Points_to()->Ofst_kind() == OFST_IS_FIXED) {
    INT64 byte_ofst = occ->Points_to()->Byte_Ofst();
    INT64 byte_size = occ->Points_to()->Byte_Size();
    UINT8 bit_ofst = occ->Points_to()->Bit_Ofst();
    UINT8 bit_size = occ->Points_to()->Bit_Size();
    vsym_id = Find_vsym_with_base_ofst_and_size(st, byte_ofst, byte_size,
						bit_ofst, bit_size);
  } else
    vsym_id = Find_vsym_with_base_ofst_and_size(st, 0, 0, 0, 0);
  if (vsym_id == 0) return;
  occ->Set_aux_id(vsym_id);

  // track list of values referenced here (for RVI)
  if (aux_stab[vsym_id].Aux_id_list() == NULL) 
    Update_aux_id_list(vsym_id);

  // update alias info for the virtual variable
  POINTS_TO *pt = aux_stab[vsym_id].Points_to();
  pt->Meet(occ->Points_to(), NULL);
  Is_True(vsym_id == Default_vsym() || pt->Base_is_fixed() || pt->Based_sym() 
	  || aux_stab[vsym_id].Unique_vsym(), ("base is disrupted."));

}


void
OCC_TAB_ENTRY::Print(FILE *fp)
{
  fprintf(fp, "auxid %d\n", Aux_id());
  fdump_tree_no_st(fp, Wn());
  if (Is_mem())
    Points_to()->Print(fp);
  else
    fprintf(fp, "\n");
}

BOOL
MU_LIST::Contains(AUX_ID var)
{
  MU_LIST_ITER mu_iter;
  MU_NODE *mnode;
  FOR_ALL_NODE(mnode, mu_iter, Init(this)) {
    if ( mnode->Aux_id() == var )
      return TRUE;
  }
  return FALSE;
}

void
MU_LIST::Delete_def_at_entry_mus(OPT_STAB *opt_stab)
{
  VER_STAB_ENTRY *vse;
  MU_NODE *prev = NULL;
  MU_NODE *cur;

  cur = Head();
  while (cur != NULL) {
    vse = opt_stab->Ver_stab_entry(cur->Opnd());
    if (vse->Zero_vers() || 
	vse->Type() == ENTRY_STMT ||
	vse->Type() == CHI_STMT && WN_operator(vse->Chi_wn()) == OPR_OPT_CHI) {
      Remove(prev, cur);
      if (prev != NULL)
        cur = prev->Next();
      else cur = Head();
      }
    else {
      prev = cur;
      cur = cur->Next(); 
    }
  }
}


void
OCC_TAB_ENTRY::Clone(OCC_TAB_ENTRY *occ, MEM_POOL *pool) 
{
  Set_aux_id(occ->Aux_id());
  Set_wn(occ->Wn());
  if (occ->Is_stmt()) {
    _u1._is_stmt = occ->_u1._is_stmt;
    Set_stmt_mu_list(NULL);
    Set_stmt_chi_list(NULL);
  } else {
    _u1._is_mem = occ->_u1._is_mem;
    Set_lno_dep_vertex_load(0);
    Set_lno_dep_vertex_store(0);
    Set_pf_pointer(NULL);
    if (Is_load())
      Set_mem_mu_node(NULL);
    else
      Set_mem_chi_list(NULL);
  }
  _points_to.Copy_fully(&(occ->_points_to));
}
