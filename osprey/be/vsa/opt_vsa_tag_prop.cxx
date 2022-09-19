/*
   Copyright (C) 2019-2022 Xcalibyte (Shenzhen) Limited.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
 */

// ==================================================================
// opt_vsa_tag_prop.cxx
//
// VSA inter-procedure tag propagation
//
// ==================================================================
#include <ext/hash_set>
#include "defs.h"
#include "opt_bb.h"
#include "bb_node_set.h"
#include "opt_cfg.h"
#include "opt_ssa.h"
#include "opt_mu_chi.h"
#include "opt_htable.h"
#include "opt_main.h"
#include "opt_vsa.h"
#include "opt_vsa_util.h"
#include "opt_defs.h"
#include "cxx_memory.h"
#include "report.h"
#include "erbe.h"
#include "opt_vsa_rsc.h"
#include "opt_vsa_rbc.h"
#include "config_vsa.h"
#include "rbc_base.h"
#include "opt_vsa_tag_prop.h"

void
TAG_PROP::Add_pending_op(TOR_DEF_ATTR attr,STMTREP *sr,  CODEREP *tgt, CODEREP *src1,
                         CODEREP *src2, TAG_BASE *tag_base, IDTYPE attr_id)
{
  PENDING_OP *pending_op = CXX_NEW(PENDING_OP(attr, sr, tgt,
                                              src1, src2, tag_base, attr_id),
                                   Loc_pool());
  _pending_ops.push_back(pending_op);
}

void
TAG_PROP::Do_prop()
{
  Is_Trace(Tracing(), (TFile, "%sLOCAL_TAG_PROP tracing for %s \n%s", DBar, Dna()->Fname(), DBar));

  Propagate_tag_entry();

  Propagate_tag_callee();

  POBB_ITER po_iter(Comp_unit()->Cfg());
  BB_NODE *bb;
  FOR_ALL_ELEM_REVERSE(bb, po_iter, Init()) {
    Propagate_tag_bb(bb);
  }

  Finalize();

  Is_Trace_cmd(Tracing(), Print(TFile));
  Is_Trace(Tracing(),(TFile, "%s", SBar));
}

// =============================================================================
//
// TAG_PROP::Propagate_tag_entry, propagate tag on entry BB chi list
//
// =============================================================================
void
TAG_PROP::Propagate_tag_entry()
{
  STMTREP *stmt = Vsa()->Get_entry_chi_stmt();
  BB_NODE *bb = stmt->Bb();
  Is_True_Ret(stmt && stmt->Opr() == OPR_OPT_CHI, ("Can't find entry chi in entry bb."));
  Is_Trace(Tracing(), (TFile, "---- Propagate_tag_entry:\n"));
  Is_Trace_cmdn(Tracing(), Vsa()->Print_sr(stmt, TFile), TFile);
  TB_LIST *tag_base_list = Ipsa()->Rbc()->Tag_base_list();
  Is_True(tag_base_list && tag_base_list->Real_use_cnt() > 0, ("no tag created"));

  CHI_NODE *cnode;
  CHI_LIST_ITER chi_iter;
  // create entry tag chi for vor
  FOR_ALL_NODE(cnode, chi_iter, Init(Vsa()->Stmt_vor_chi(stmt))) {
    CVOR *cvor = (CVOR *) cnode->RESULT();
    VSYM_OBJ_REP *vor = cvor->first;
    CODEREP *cr = cvor->second;
    IDTYPE parm_idx = Dna()->Is_param(cr);
    AUX_STAB_ENTRY* aux = Opt_stab()->Aux_stab_entry(cr->Aux_id());
    // skip create entry chi for non-param or non-global
    if (parm_idx == INVALID_VAR_IDX && !aux->Is_global()) {
      continue;
    }
    if (vor->Attr() == ROR_DEF_BY_CHI) {
      ST *st = aux->St();
      if(st && ST_is_const_var(st)) {
        continue;
      }
      TB_LIST_ITER tb_list_iter;
      TAG_BASE *tb;
      TOR_LIST *tor_list = Allocate_tor_list(vor, bb);
      FOR_ALL_ELEM(tb, tb_list_iter, Init(tag_base_list)) {
        if (tb->Is_set_flag(RSC_TAG_USED)) {
          Allocate_tag_obj(tor_list, tb, vor, stmt, bb, TO_DEF_BY_ENTRY_CHI);
        }
      }
    }
  }
  // create entry tag chi for cr
  FOR_ALL_NODE(cnode, chi_iter, Init(stmt->Chi_list())) {
    if (!cnode->Live())
      continue;
    CODEREP *cr = cnode->RESULT();
    IDTYPE parm_idx = Dna()->Is_param(cr);
    AUX_STAB_ENTRY* aux = Opt_stab()->Aux_stab_entry(cnode->Aux_id());
    // skip create entry chi for non-param or non-global
    if (parm_idx == INVALID_VAR_IDX && !aux->Is_global()) {
      continue;
    }
    ST *st = aux->St();
    if(st && ST_is_const_var(st)) {
      continue;
    }
    TAG_BASE *tb;
    TB_LIST_ITER tb_list_iter;
    TOR_LIST *tor_list = Allocate_tor_list(cr, stmt->Bb());
    FOR_ALL_ELEM(tb, tb_list_iter, Init(tag_base_list)) {
      if (!tb->Is_set_flag(RSC_TAG_USED)) {
        continue;
      }
      // mark all attr set for const, treat them as sanitized
      TAG_OBJ_REP *tor = Allocate_tag_obj(tor_list, tb, cr, stmt, bb, TO_DEF_BY_ENTRY_CHI);
      if(parm_idx != INVALID_VAR_IDX &&
         Dna()->Is_set_parm_tag(parm_idx, tb->Id())) {
        tor->Set_def_attr(TO_DEF_BY_CREATE);
      }
    }
  }
}

// =============================================================================
//
// TAG_PROP::Propagate_tag_callee, propagate tags with callee side-effect
//
// =============================================================================
void
TAG_PROP::Propagate_tag_callee()
{
  RNODE_VECTOR *call_list = Dna()->Call_list();
  for (INT i = VAR_INIT_ID; i < call_list->size(); ++i) {
    RNA_NODE *rna = call_list->at(i);
    Is_True(rna != NULL && rna->Callstmt() != NULL,
            ("invalid rna"));
    STMTREP *stmt = rna->Callstmt();

    Is_Trace(Tracing(), (TFile, "\n[sr%d]: Propagate_tag_callee\n",
                         stmt->Stmtrep_id()));
    const CALLEE_VECTOR& callee_list = rna->Callee_list();
    for (CALLEE_VECTOR::const_iterator iter = callee_list.begin(); iter != callee_list.end(); iter++) {
      DNA_NODE *callee = Ipsa()->Get_dna(iter->Callee());
      if (callee == NULL) {
        continue;
      }
      if (callee->Is_set_rbc_flag(DNA_RBC_TAG_CREATE)) {
        Propagate_tag_rbc_create(rna, callee);
      }
      if (callee->Non_functional()) {
        continue;
      }
      // propagate tag by callee side-effect
      // [ step 1]: propgate return value
      CODEREP *ret_cr = Comp_unit()->Find_return_value(stmt);
      if (!callee->Get_ret_tag_node()->Is_set_any_tags()) {
        CONTEXT_SWITCH callee_ctx(callee);
        VSA *callee_vsa = callee->Comp_unit()->Vsa();
        for (INT i = VAR_INIT_ID; i < callee->Retv_list()->size(); ++ i) {
          PDV_NODE *pdv = (*callee->Retv_list())[i];
          STMTREP* pdv_sr = pdv->Stmt();
          if (pdv->Kind() == BY_RETURNSTMT && pdv->Oparam() == 0 && ret_cr &&
              pdv_sr->Lhs()->Offset() == ret_cr->Offset()) {
            Is_True(pdv_sr->Opr() == OPR_STID, ("bad stmt"));
            Is_True(pdv_sr->Next() != NULL && pdv_sr->Next()->Opr() == OPR_RETURN,
                    ("bad return stmt"));
            CODEREP *callee_ret = pdv_sr->Lhs();
            TOR_LIST *callee_tors = callee_vsa->Find_tor_list_from_cr(pdv_sr, callee_ret);
            if (callee_tors)
            {
              CONTEXT_SWITCH caller_ctx(Dna());
              Bind_tor_list_to_cr<TO_DEF_BY_CHI>(stmt->Bb(), stmt, ret_cr, callee_tors);
            }
          }
        }
      }
    } // for rna->Callee_list()

    // create tor chi for vsym chi
    // TODO: only create chi for vsym which has vsym mu on callee return stmt,
    //       others add to pending to copy tag from chi opnd
    TB_LIST *tag_base_list = Rbc()->Tag_base_list();
    if (tag_base_list == NULL || tag_base_list->Real_use_cnt() == 0) {
      continue;
    }
    if (VSA_Enable_Prop_Tag && rna->Callee_cnt() == 0 || rna->Is_flag_set(RBC_SE_TAG_OP)) {
      // if enabled default propagation for non-callee rna or 
      // rna has tag op, do not create chi, follow Propagate_tag_rna logic to process
      continue;
    }
    CHI_NODE *cnode;
    CHI_LIST_ITER chi_iter;
    FOR_ALL_NODE(cnode, chi_iter, Init(Vsa()->Stmt_vor_chi(stmt))) {
      CVOR *cvor = (CVOR *) cnode->RESULT();
      VSYM_OBJ_REP *vor = cvor->first;
      TB_LIST_ITER tb_list_iter;
      TAG_BASE *tb;
      FOR_ALL_ELEM(tb, tb_list_iter, Init(tag_base_list)) {
        if (!tb->Is_set_flag(RSC_TAG_USED)) {
          continue;
        }
        TOR_LIST *tor_list = Tor_list(vor);
        // older tor already added by tag create
        if (tor_list && tor_list->Find(tb->Id())) {
          continue;
        }
        Allocate_tag_obj(tor_list, tb, vor, stmt, stmt->Bb(), TO_DEF_BY_CHI);
      }
    }
  } // for dna Call_list()
}

// =============================================================================
//
// TAG_PROP::Propagate_tag_rbc_create, propagate tag from rbc tag create
//
// =============================================================================
void
TAG_PROP::Propagate_tag_rbc_create(RNA_NODE *rna, DNA_NODE *callee)
{
  if (!callee->Is_set_rbc_flag(DNA_RBC_TAG_CREATE)) {
    return;
  }
  STMTREP *call_stmt = rna->Callstmt();
  // dna set ret tags
  if (callee->Get_ret_tag_node()->Is_set_any_tags()) {
    std::vector<IDTYPE> tag_id_vec;
    TAG_NODE *tn = callee->Get_ret_tag_node();
    tn->Collect_all_tags(tag_id_vec);
    // create tag obj for return
    CODEREP *cr = Comp_unit()->Find_return_value(call_stmt);
    if (cr && !tag_id_vec.empty()) {
      std::vector<IDTYPE>::const_iterator iter;
      for (iter = tag_id_vec.begin(); iter != tag_id_vec.end(); ++iter) {
        IDTYPE tag_id = *iter;
        TAG_BASE *tb = Rbc()->Find_tag_base(tag_id);
        Is_True(tb != NULL, ("tag base is null."));
        CHI_LIST_ITER chi_iter;
        CHI_NODE     *cnode;
        BOOL bind_to_vor = FALSE;
        FOR_ALL_NODE(cnode, chi_iter, Init(Vsa()->Stmt_vor_chi(call_stmt))) {
          CVOR *cvor = (CVOR*)cnode->RESULT();
          if (cvor->second == cr) {
            VSYM_OBJ_REP *res_vor = cvor->first;
            Allocate_tag_obj(Tor_list(res_vor), tb, res_vor, call_stmt,
                             call_stmt->Bb(), TO_DEF_BY_CREATE);
            bind_to_vor = TRUE;
          }
        }
        if (!bind_to_vor) {
          Allocate_tag_obj(Tor_list(cr), tb, cr, call_stmt,
                          call_stmt->Bb(), TO_DEF_BY_CREATE);
        }
      }
    }
  }

  for (INT arg = VAR_INIT_ID; arg < callee->Parm_list()->size(); arg++) {
    if (arg >= rna->Arg_list()->size())
      continue;
    if (!callee->Parm_list()->at(arg)->Get_tag_node()->Is_set_any_tags())
      continue;
    CODEREP *cr = rna->Get_arg(arg);
    if (cr->Kind() == CK_OP)
      cr = Find_ilod_base(cr);
    if (cr) {
      std::vector<IDTYPE> tag_id_vec;
      TAG_NODE *tn = callee->Parm_list()->at(arg)->Get_tag_node();
      tn->Collect_all_tags(tag_id_vec);
      TOR_LIST *tor_list = NULL;
      VSYM_OBJ_REP *bind_vor = Vsa()->Find_vor_with_tag(call_stmt, cr, FALSE);
      std::vector<IDTYPE>::const_iterator iter;
      if (!tag_id_vec.empty()) {
        Is_Trace(Tracing(), (TFile, "\n[sr%d]: RBC: Create tag for arg cr%d\n",
                            call_stmt->Stmtrep_id(), cr->Coderep_id()));
      }
      for (iter = tag_id_vec.begin(); iter != tag_id_vec.end(); ++iter) {
        IDTYPE tag_id = *iter;
        TAG_BASE *tb = Rbc()->Find_tag_base(tag_id);
        Is_True(tb != NULL, ("tag base is null."));
        Is_True(bind_vor, ("Create tag for parm need vsym created to save tag"));
        if (bind_vor) {
          Allocate_tag_obj(Tor_list(bind_vor), tb, bind_vor, call_stmt,
                          call_stmt->Bb(), TO_DEF_BY_CREATE);
        }
      }
    }
  } // for callee Parm_list()
}

// =============================================================================
//
// TAG_PROP::Propagate_tag_bb, propagate tag on BB and it's dom bbs
//
// =============================================================================
void
TAG_PROP::Propagate_tag_bb(BB_NODE *bb)
{
  Is_Trace(Tracing(),
           (TFile, "\n---- TAG_PROP BB=%d\n", bb->Id()));

  Propagate_tag_for_phi(bb);

  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {
    Propagate_tag_stmt(stmt, bb);
  }
}

void
TAG_PROP::Propagate_tag_for_phi(BB_NODE *bb)
{
  PHI_NODE *phi;
  PHI_LIST_ITER phi_iter;
  FOR_ALL_ELEM(phi, phi_iter, Init(bb->Phi_list())) {
    if (!phi->Live())
      continue;
    if (phi->Aux_id() == Opt_stab()->Default_vsym() ||
        phi->Aux_id() == Opt_stab()->Return_vsym())
      continue;
    BOOL pending_phi = FALSE;
    for (INT i = 0; i < phi->Size(); i++) {
      // Q: do we need to consier find from vor?
      TOR_LIST *opnd_tor_list = Tor_list(phi->OPND(i)); // Find_tor_list_from_cr(NULL, phi->OPND(i), TRUE);
      if (opnd_tor_list) {
        Is_Trace(Tracing(), (TFile, "\n---- Processing PHI[%d]: ", i));
        Is_Trace_cmd(Tracing(), phi->Print(TFile));
        Bind_tor_list<CODEREP*, TO_DEF_BY_PHI>(bb, NULL, phi->RESULT(), opnd_tor_list);
      }
      hash_set<IDTYPE> visited;
      if (Is_cr_pending(phi->OPND(i), NULL, visited)) {
        pending_phi = TRUE;
      }
    }
    if (pending_phi) {
      Add_pending_phi(phi);
    }
  }
  // merge tor from vor phi opnd
  FOR_ALL_NODE(phi, phi_iter, Init(Vsa()->Bb_vo_philist(bb))) {
    VSYM_OBJ_REP *res_vor = (VSYM_OBJ_REP *)phi->RESULT();
    BOOL pending_phi = FALSE;
    for (INT i = 0; i < phi->Size(); i++) {
      TOR_LIST *opnd_tor_list = Tor_list((VSYM_OBJ_REP *) phi->OPND(i));
      if (opnd_tor_list) {
        Is_Trace(Tracing(), (TFile, "\n---- Processing VOR PHI[%d]: ", i));
        Is_Trace_cmd(Tracing(), Vsa()->Print_vor_phi(phi, TFile));
        Bind_tor_list<VSYM_OBJ_REP *, TO_DEF_BY_PHI>(bb, NULL, res_vor, opnd_tor_list);
      }
      hash_set<IDTYPE> visited;
      if (Is_vor_pending((VSYM_OBJ_REP *) phi->OPND(i), visited)) {
        pending_phi = TRUE;
      }
    }
    if (pending_phi) {
      Add_pending_phi(phi);
    }
  }
}

// =============================================================================
//
// TAG_PROP::Propagate_tag_stmt, propagate tag on stmt
//
// =============================================================================
void
TAG_PROP::Propagate_tag_stmt(STMTREP *sr, BB_NODE *bb)
{
  OPERATOR opr = sr->Opr();
  switch (opr) {
    case OPR_INTRINSIC_CALL:
    case OPR_CALL:
    case OPR_ICALL:
    {
      RNA_NODE *rna = Dna()->Get_callsite_rna(sr);
      if (rna) {
        Propagate_tag_rna(rna);
      }
    }
    break;
    case OPR_STID:
      Bind_or_pending_tag<TO_DEF_BY_COPY>(bb, sr, sr->Lhs(), sr->Rhs());
      if (VSA_Enable_Chi_Prop_Tag) {
        Propagate_tag_chi(sr);
      }
    break;
    case OPR_ISTORE:
    {
      // if the ISTORE is partial access vo, propagate tag from vor mu
      VSYM_OBJ_REP *lhs_vor = Vsa()->Cr_2_vor(sr->Lhs());
      VSYM_OBJ_REP *vor_mu = NULL;
      if (lhs_vor && lhs_vor->Vsym_obj()->Fld_rep().Is_any()) {
        MU_NODE *mu = Vsa()->Find_vor_mu(sr, lhs_vor->Vsym_obj());
        vor_mu = mu ? ((CVOR*)mu->OPND())->first : NULL;
      }
      if (vor_mu) {
        Bind_or_pending_tag<TO_DEF_BY_OR>(bb, sr, sr->Lhs(), sr->Lhs(), sr->Rhs());
      } else {
        Bind_or_pending_tag<TO_DEF_BY_COPY>(bb, sr, sr->Lhs(), sr->Rhs());
      }
      // merge tag of ISTORE base with rhs
      CODEREP *base = sr->Lhs()->Istr_base();
      Bind_or_pending_tag<TO_DEF_BY_OR>(bb, sr, base, base, sr->Rhs());
    }
    break;
    default:
      if (sr->Lhs() && sr->Rhs()) {
        Bind_or_pending_tag<TO_DEF_BY_COPY>(bb, sr, sr->Lhs(), sr->Rhs());
      }
    break;
  }
}

void
TAG_PROP::Propagate_tag_chi(STMTREP *sr)
{
  if (sr->Chi_list() == NULL || sr->Chi_list()->Is_Empty()) {
    return;
  }

  Is_Trace(Tracing(), (TFile, "\n---- Processing stmt chi: \n"));
  Is_Trace_cmd(Tracing(), Vsa()->Print_sr(sr, TFile));

  CODEREP *lhs = sr->Lhs();
  TOR_LIST *lhs_tor_list = Tor_list(lhs);
  CHI_NODE *cnode;
  CHI_LIST_ITER chi_iter;
  FOR_ALL_NODE(cnode, chi_iter, Init(sr->Chi_list())) {
    if (!cnode->Live())
      continue;
    CODEREP *opnd = cnode->OPND();
    CODEREP *res = cnode->RESULT();
    TOR_LIST *opnd_tor_list = Tor_list(opnd);

    // 1. propagate tor from chi opnd to chi result,
    //    only when chi result does not have a tor of the same base
    if (opnd_tor_list) {
      Bind_tor_list<CODEREP*, TO_DEF_BY_COPY>(sr->Bb(), sr, res, opnd_tor_list);
    }

    // 2. update/create aliased chi result tor with lhs tor
    if (lhs->Kind() != CK_VAR || lhs_tor_list == NULL) {
      continue;
    }
    AUX_ID lhs_aux = lhs->Aux_id();
    AUX_ID cur_aux = Opt_stab()->St_group(lhs_aux);
    AUX_ID chi_aux = cnode->Aux_id();
    while (cur_aux != 0 && cur_aux != lhs_aux) {
      if (cur_aux == chi_aux) {
        Bind_tor_list<CODEREP*, TO_DEF_BY_COPY>(sr->Bb(), sr, res, lhs_tor_list);
      }
      cur_aux = Opt_stab()->St_group(cur_aux);
    }
  }
}

void
TAG_PROP::Propagate_tag_rna(RNA_NODE *rna)
{
  STMTREP *stmt = rna->Callstmt();
  if (rna->Is_flag_set(RBC_SE_TAG_OP)) {
    Is_True_Ret(Vsa()->Tag_prop(), ("vsa tag_prop not set"));
    Rbc()->Eval__mvsa_tag_op(Dna(), rna, Loc_pool());
    return;
  }

  CODEREP *cr = stmt->Rhs();
  // process java intrinsic side-effect
  // TODO: write rule for java intrinic, need to consider how to generate java function name
  //       the same as the intrinsic name
  if (stmt->Opr() == OPR_INTRINSIC_CALL && stmt->Rhs()->Intrinsic() == INTRN_CHECK_CAST) {
    CODEREP *ret_cr = Comp_unit()->Find_return_value(stmt);
    if (ret_cr) {
      Bind_or_pending_tag<TO_DEF_BY_COPY>(stmt->Bb(), stmt, ret_cr, cr->Opnd(1)->Ilod_base());
    }
    return;
  }

  if (stmt->Opr() != OPR_INTRINSIC_CALL && rna->Callee_cnt() == 0) {
    // propagate call, don't have callee; if have return, propagate to return value
    // otherwise, if first arg is pointer type, then propagate to first arg
    if (VSA_Enable_Prop_Tag) {
      CODEREP *ret_cr = Comp_unit()->Find_return_value(stmt);
      // bind to return
      if (ret_cr) {
        for (INT i = 0; i < cr->Kid_count(); i++) {
          Add_pending_op(TO_DEF_BY_PHI, stmt, ret_cr, cr->Opnd(i));
          TOR_LIST *tor_list = Vsa()->Find_tor_list_from_cr(stmt, cr->Opnd(i));
          if (tor_list) {
            Bind_tor_list_to_cr<TO_DEF_BY_PHI>(stmt->Bb(), stmt, ret_cr, tor_list);
            break;
          }
        }
      } else {
        // bind to first arg that is pointer type
        CODEREP *first_arg = NULL;
        if (cr->Kid_count() > 0 && TY_kind(cr->Opnd(0)->object_ty()) == KIND_POINTER) {
          CODEREP *opnd0 = cr->Opnd(0);
          // C icall:
          //   LDID ptr
          //  ICALL
          // first child is CK_VAR, maybe should find another pointer
          if (opnd0->Kind() == CK_VAR) {
            first_arg = opnd0;
          } else if (opnd0->Kind() == CK_IVAR) {
            first_arg = opnd0->Ilod_base();
          }
        }
        if (first_arg) {
          for (INT i = 1; i < cr->Kid_count(); i++) {
            Add_pending_op(TO_DEF_BY_PHI, stmt, first_arg, cr->Opnd(i));
            TOR_LIST *tor_list = Vsa()->Find_tor_list_from_cr(stmt, cr->Opnd(i));
            if (tor_list) {
              Bind_tor_list_to_cr<TO_DEF_BY_PHI>(stmt->Bb(), stmt, first_arg, tor_list);
              break;
            }
          }
        }
      }
    }

    // propagate tag from vor chi opnd to result
    // TODO: need to add to pending ops
    CHI_LIST_ITER chi_iter;
    CHI_NODE     *cnode;
    FOR_ALL_NODE(cnode, chi_iter, Init(Vsa()->Stmt_vor_chi(stmt))) {
      CVOR *cvor = (CVOR*)cnode->RESULT();
      VSYM_OBJ_REP *res_vor = cvor->first;
      VSYM_OBJ_REP *opnd_vor = ((CVOR*)cnode->OPND())->first;
      TOR_LIST *opnd_tor_list = Tor_list(opnd_vor);
      if (opnd_tor_list) {
        Bind_tor_list<VSYM_OBJ_REP *, TO_DEF_BY_PHI>(stmt->Bb(), stmt, res_vor, opnd_tor_list);
      }
    }
  }
}

void
TAG_PROP::Propagate_pending_ops()
{
  Is_Trace(Tracing(), (TFile, "\n---- Processing pending ops\n"));
  PENDING_OPS::iterator op_iter;
  for (op_iter = _pending_ops.begin(); op_iter != _pending_ops.end(); op_iter++) {
    PENDING_OP *op_info = *op_iter;
    TOR_LIST *src_list1 = op_info->Src() ?
      Vsa()->Find_tor_list_from_cr(op_info->Sr(), op_info->Src(), TRUE) : NULL;
    TOR_LIST *src_list2 = op_info->Src2() ?
      Vsa()->Find_tor_list_from_cr(op_info->Sr(), op_info->Src2(), TRUE) : NULL;
    if (src_list1 == NULL && src_list2 == NULL) {
      continue;
    }
    BOOL src1_updated = src_list1 ? Tor_list_updated(src_list1) : FALSE;
    BOOL src2_updated = src_list2 ? Tor_list_updated(src_list2) : FALSE;

    switch (op_info->Attr()) {
      case TO_DEF_BY_PHI:
        if (src1_updated) {
          Bind_tor_list_to_cr<TO_DEF_BY_PHI>(op_info->Sr()->Bb(),
                                             op_info->Sr(), op_info->Tgt(),
                                             src_list1);
        }
      break;
      case TO_DEF_BY_COPY:
        if (src1_updated) {
          Bind_tor_list_to_cr<TO_DEF_BY_COPY>(op_info->Sr()->Bb(),
                                              op_info->Sr(), op_info->Tgt(),
                                              src_list1);
        }
      break;
      case TO_DEF_BY_CHI:
        if (src1_updated) {
          Bind_tor_list_to_cr<TO_DEF_BY_CHI>(op_info->Sr()->Bb(),
                                             op_info->Sr(), op_info->Tgt(),
                                             src_list1);
        }
      break;
      case TO_DEF_BY_SE:
        if (src1_updated) {
          Bind_tor_list_to_cr<TO_DEF_BY_SE>(op_info->Sr()->Bb(),
                                            op_info->Sr(), op_info->Tgt(),
                                            src_list1);
        }
      break;
      case TO_DEF_BY_OR:
        if (src1_updated || src2_updated) {
          Bind_tor_list_to_cr<TO_DEF_BY_OR>(op_info->Sr()->Bb(),
                                            op_info->Sr(), op_info->Tgt(),
                                            src_list1, src_list2);
        }
      break;
      case TO_DEF_BY_TAG_ATTR:
        if (src1_updated) {
          Bind_tor_list_to_cr<TO_DEF_BY_TAG_ATTR>(op_info->Sr()->Bb(),
                                                  op_info->Sr(), op_info->Tgt(),
                                                  src_list1, NULL,
                                                  op_info->Tag_base(), op_info->Attr_id());
        }
      break;
      default:
      Is_True(FALSE, ("invalid op tag attr %d", op_info->Attr()));
    }
  }
}

void
TAG_PROP::Finalize()
{
  BB_LIST_CONTAINER worklist;
  BB_NODE_SET_ITER  bns_iter;
  BB_NODE          *bbx, *bby;
  BS_ELT            bbs = Cfg()->Total_bb_count();
  MEM_POOL          bbset_pool;

  OPT_POOL_Initialize(&bbset_pool, "TAG_PROP bb set pool", FALSE, SSA_DUMP_FLAG);
  OPT_POOL_Push(&bbset_pool, SSA_DUMP_FLAG);

  BB_NODE_SET inserted(bbs, Cfg(), &bbset_pool, BBNS_EMPTY);
  BB_NODE_SET everonlist(bbs, Cfg(), &bbset_pool, BBNS_EMPTY);

  for (int idx = 0; idx < _tors_definfo.size(); idx++) {
    TLIST_DEF_INFO *def_info = Tor_list_def_info(idx);
    Is_True(Tor_list_updated(idx), ("Tor_list[%d] should set updated", idx));
    BB_NODE *def_bb = def_info->Def_bb();
    if (everonlist.MemberP(def_bb) == FALSE){
      everonlist.Union1D(def_bb);
      worklist.Append(def_bb, &bbset_pool);
    }
  }
  do {
    Is_Trace(Tracing(), (TFile, "\n---- Finalize[%d]:\n", Round()));
    Inc_round();
    inserted.ClearD();
    // Go through the work list.
    while (bbx = worklist.Remove_head(&bbset_pool)) {
      Is_Trace(Tracing(), (TFile, "\n---- [R%d] Processing BB[%d]:", Round() -1 , bbx->Id()));
      //  Go through the dominator frontier of bbx.
      FOR_ALL_ELEM (bby, bns_iter, Init(bbx->Dom_frontier())) {

        Is_Trace(Tracing(), (TFile, "\n Processing BB[%d] df BB[%d]" , bbx->Id(), bby->Id()));
        if (inserted.MemberP(bby)) {
          continue;
        }
        PHI_LIST *phi_list = bby->Phi_list();
        PHI_NODE *phi;
        PHI_LIST_ITER phi_iter;
        FOR_ALL_ELEM(phi, phi_iter, Init(phi_list)) {
          if (!phi->Live())
            continue;
          if (phi->Aux_id() == Opt_stab()->Default_vsym() ||
              phi->Aux_id() == Opt_stab()->Return_vsym())
            continue;
          for (INT i = 0; i < phi->Size(); i++) {
            TOR_LIST *opnd_tor_list = Tor_list(phi->OPND(i));
            if (opnd_tor_list && Tor_list_updated(opnd_tor_list)) {
              Is_Trace(Tracing(), (TFile, "\n---- Processing PHI opnd[%d]: ", i));
              Is_Trace_cmd(Tracing(), phi->Print(TFile));
              Bind_tor_list<CODEREP*, TO_DEF_BY_PHI>(bby, NULL, phi->RESULT(), opnd_tor_list);
            } 
          }
        }
        FOR_ALL_NODE(phi, phi_iter, Init(Vsa()->Bb_vo_philist(bby))) {
          VSYM_OBJ_REP *res_vor = (VSYM_OBJ_REP *)phi->RESULT();
          for (INT i = 0; i < phi->Size(); i++) {
            TOR_LIST *opnd_tor_list = Tor_list((VSYM_OBJ_REP *) phi->OPND(i));
            if (opnd_tor_list && Tor_list_updated(opnd_tor_list)) {
              Is_Trace(Tracing(), (TFile, "\n---- Processing VOR PHI[%d]: ", i));
              Is_Trace_cmd(Tracing(), Vsa()->Print_vor_phi(phi, TFile));
              Bind_tor_list<VSYM_OBJ_REP *, TO_DEF_BY_PHI>(bby, NULL, res_vor, opnd_tor_list);
            }
          }
        }
        inserted.Union1D(bby);
        if (everonlist.MemberP(bby) == FALSE) {
          everonlist.Union1D(bby);
          worklist.Append(bby, &bbset_pool);
        }
      }
    }
    for (int phi_idx = 0; phi_idx < _pending_phis.size(); phi_idx++) {
      PHI_NODE *phi = _pending_phis[phi_idx];
      if (VSA_PHI_NODE(phi).Res_is_vor()) {
        for (INT i = 0; i < phi->Size(); i++) {
          TOR_LIST *opnd_tor_list = Tor_list((VSYM_OBJ_REP *) phi->OPND(i));
          if (opnd_tor_list && Tor_list_updated(opnd_tor_list)) {
            Is_Trace(Tracing(), (TFile, "\n---- Processing VOR PHI[%d]: ", i));
            Is_Trace_cmd(Tracing(), Vsa()->Print_vor_phi(phi, TFile));
            Bind_tor_list<VSYM_OBJ_REP *, TO_DEF_BY_PHI>(phi->Bb(), NULL, (VSYM_OBJ_REP *)phi->RESULT(), opnd_tor_list);
          }
        }
      } else {
        for (INT i = 0; i < phi->Size(); i++) {
          TOR_LIST *opnd_tor_list = Tor_list(phi->OPND(i));
          if (opnd_tor_list && Tor_list_updated(opnd_tor_list)) {
            Is_Trace(Tracing(), (TFile, "\n---- Processing PHI opnd[%d]: ", i));
            Is_Trace_cmd(Tracing(), phi->Print(TFile));
            Bind_tor_list<CODEREP*, TO_DEF_BY_PHI>(phi->Bb(), NULL, phi->RESULT(), opnd_tor_list);
          } 
        }
      }
    }
    // update tors on pending ops for newly added tors
    Propagate_pending_ops();

    worklist.Clear();
    everonlist.ClearD();
    Is_Trace(Tracing(), (TFile, "\n---- Round[%d] candidates: ", Round()));
    for (int idx = 0; idx < _tors_definfo.size(); idx++) {
      TLIST_DEF_INFO *def_info = Tor_list_def_info(idx);
      if (def_info->Round() == Round()) {
        Is_Trace(Tracing(), (TFile, "\ntor_list[%d] ", idx));
        Is_Trace_cmd(Tracing(), def_info->Print(TFile));
        BB_NODE *def_bb = def_info->Def_bb();
        if (everonlist.MemberP(def_bb) == FALSE){
          everonlist.Union1D(def_bb);
          worklist.Append(def_bb, &bbset_pool);
        }
      }
    }
  } while (!worklist.Is_Empty());

  OPT_POOL_Pop(&bbset_pool, SSA_DUMP_FLAG);
  OPT_POOL_Delete(&bbset_pool, SSA_DUMP_FLAG);
}

// VSA util functions

// =============================================================================
//
// VSA::Find_vor_with_tag, find cr vor from stmt
// if mu: search vor mu list returns vor has tags
// if chi: search from chi list
//
// =============================================================================
VSYM_OBJ_REP *
VSA::Find_vor_with_tag(STMTREP *sr, CODEREP *cr, BOOL mu) const
{
  if (sr == NULL) {
    return NULL;
  }

  CODEREP *tag_cr = Find_cr_with_tag(cr);
  VSYM_OBJ_REP *vor = Cr_2_vor(tag_cr);
  CODEREP *cr_base = tag_cr == sr->Lhs() ? tag_cr : Find_ilod_base(tag_cr);
  HEAP_OBJ_REP* hor = cr_base ? Cr_2_heap_obj(cr_base) : NULL;
  if (hor == NULL && vor) {
    // if no hor, read tag from Cr_2_vor
    hor = vor->Vsym_obj()->Base_hor();
    cr_base = tag_cr;
  }
  if (hor && sr) {
    VS_FLD_KIND vf_kind = Get_vfr_kind(tag_cr);
    VSYM_FLD_REP vfr(vf_kind, 0, 0);
    if (mu) {
      MU_LIST *mu_list = Stmt_vor_mu(sr);
      if (mu_list && !mu_list->Is_Empty()) {
        MU_NODE *mnode;
        MU_LIST_ITER mu_iter;
        FOR_ALL_NODE(mnode, mu_iter, Init(mu_list)) {
          CVOR *cvor = (CVOR*)mnode->OPND();
          VSYM_OBJ_REP *mu_vor = cvor->first;
          if (cvor->second == cr_base && mu_vor->Vsym_obj()->Base_hor() == hor &&
              mu_vor->Tor_list<TOR_LIST>()) {
            if (Vsym_match(sr, cr_base, &vfr, mu_vor->Vsym_obj()->Fld_rep_ptr())) {
              vor = cvor->first;
              break;
            } else if (vfr.Is_any()) {
              vor = cvor->first;   // set vor to element if not find exact match
            }
          }
        }
      }
    }
    else
    {
      VSYM_OBJ_REP *vor_chi = Find_vor_chi_vor(sr, cr_base, &vfr);
      vor = vor_chi ? vor_chi : vor;
    }
  }
  if (vor && Is_special_vor(vor)) {
    return NULL;
  }
  return vor;
}

BOOL
TAG_PROP::Is_vor_pending(VSYM_OBJ_REP *vor, hash_set<IDTYPE> &visited)
{
  PENDING_STATUS sts = Is_rsc_pending(vor);
  if (sts != PENDING_UNKNOWN) {
    return sts == PENDING_YES ? TRUE : FALSE;
  }
  BOOL res = FALSE;
  TOR_LIST *tor_list = Tor_list(vor);
  switch (vor->Attr()) {
    case ROR_DEF_BY_PHI:
    case ROR_DEF_BY_HORPHI:
    {
      PHI_NODE *phi = vor->Phi_def();
      Is_True(phi != NULL, ("bad vor phi"));
      if (visited.find(phi->Bb()->Id()) != visited.end()) {
        res = TRUE;
      } else {
        visited.insert(phi->Bb()->Id());
        BB_NODE* bb_pred;
        BB_LIST_ITER bb_iter;
        INT i = 0;
        FOR_ALL_ELEM(bb_pred, bb_iter, Init(phi->Bb()->Pred())) {
          VSYM_OBJ_REP *vor_opnd = (VSYM_OBJ_REP*) phi->OPND(i);
          if (Is_vor_pending(vor_opnd, visited)) {
            res = TRUE;
            break;
          }
          i++;
        }
      }
    }
    break;
    case ROR_DEF_BY_CHI:
    {
      STMTREP *def_sr = vor->Stmt_def();
      if (def_sr) {
        if (OPERATOR_is_call(def_sr->Opr())) {
          res = TRUE;
        } else if (OPERATOR_is_store(def_sr->Opr())) {
          res = Is_cr_pending(def_sr->Rhs(), def_sr, visited);
        }
      }
    }
    break;
    case ROR_DEF_BY_COPY:
    case ROR_DEF_BY_ISTORE:
    {
      STMTREP *def_sr = vor->Stmt_def();
      Is_True(def_sr != NULL && def_sr->Rhs() != NULL && OPERATOR_is_store(def_sr->Opr()),
              ("invalid vor def stmt"));
      res = Is_cr_pending(def_sr->Rhs(), def_sr, visited);
    }
    break;
    case ROR_DEF_BY_NULL:
    break;
    default:
      Enter_pending_cache(vor, PENDING_NO);
      Is_True_Ret(FALSE, ("unsupported vor attr%d", vor->Attr()), FALSE);
    break;
  }
  Enter_pending_cache(vor, res);
  return res;
}

BOOL
TAG_PROP::Is_cr_pending(CODEREP *cr, STMTREP *sr, hash_set<IDTYPE> &visited)
{
  Is_True_Ret(cr, ("null cr"), FALSE);
  BOOL res = FALSE;
  cr = Vsa()->Find_cr_with_tag(cr);
  PENDING_STATUS sts = Is_rsc_pending(cr);
  if (sts != PENDING_UNKNOWN) {
    return sts == PENDING_YES ? TRUE : FALSE;
  }
  VSYM_OBJ_REP *vor = Vsa()->Find_vor_with_tag(sr, cr, TRUE);
  if (vor) {
    res = Is_vor_pending(vor, visited);
    Enter_pending_cache(cr, res);
    return res;
  }
  switch (cr->Kind()) {
    case CK_LDA:
    case CK_CONST:
    case CK_RCONST:
    case CK_OP:
      res = FALSE;
      break;
    case CK_VAR:
    {
      if (cr->Is_flag_set(CF_DEF_BY_CHI)) {
        STMTREP *def_sr = cr->Defstmt();
        Is_True(def_sr != NULL, ("bad stmt"));
        if (OPERATOR_is_call(def_sr->Opr())) {
          res = TRUE;
        }
      }
      else if (cr->Is_flag_set(CF_DEF_BY_PHI)) {
        PHI_NODE *phi = cr->Defphi();
        if (visited.find(phi->Bb()->Id()) != visited.end()) {
          res = TRUE;
        } else {
          visited.insert(phi->Bb()->Id());
          BB_NODE* bb_pred;
          BB_LIST_ITER bb_iter;
          INT i = 0;
          FOR_ALL_ELEM(bb_pred, bb_iter, Init(phi->Bb()->Pred())) {
            CODEREP* opnd = phi->OPND(i);
            if (Is_cr_pending(opnd, NULL, visited)) {
              res = TRUE;
              break;
            }
            i++;
          }
        }
      }
      else if (!cr->Is_var_nodef()) {
        STMTREP *def_sr = cr->Defstmt();
        Is_True(def_sr != NULL, ("bad stmt"));
        res = Is_cr_pending(def_sr->Rhs(), def_sr, visited);
      }
    break;
    case CK_IVAR:
      vor = Vsa()->Cr_2_vor(cr);
      Is_True_Ret(vor == NULL || Vsa()->Is_special_vor(vor), ("Is_cr_pending: IVAR should have vsym attached"), FALSE);
    break;
    default:
      Is_True_Ret(FALSE, ("unknown cr kind"), FALSE);
    }
  }
  Enter_pending_cache(cr, res);
  return res;
}

TOR_LIST *
TAG_PROP::Find_or_pending_stmt(STMTREP *sr, CODEREP *cr, BOOL &pending)
{
  CODEREP *tag_cr = Vsa()->Find_cr_with_tag(cr);
  TOR_LIST *tor_list = NULL;
  hash_set<IDTYPE> visited;
  pending = Is_cr_pending(tag_cr, sr, visited);
  VSYM_OBJ_REP *vor = Vsa()->Find_vor_with_tag(sr, tag_cr, TRUE);
  if (vor) {
    tor_list = Tor_list(vor);
  }
  if (tor_list == NULL) {
    tor_list = Tor_list(tag_cr);
  }
  return tor_list;
}

void
TAG_PROP::Print(FILE *fp) const
{
  fprintf(fp, "\nTOR_LIST cnt:%ld, TAG_OBJ_REP cnt:%d\n", _tors_definfo.size(), _last_tor_id - 1);
}

CODEREP *
VSA::Find_cr_with_tag(CODEREP *cr) const
{
  Is_True_Ret(cr, ("Find_cr_with_tag: cr is NULL."), NULL);
  switch (cr->Kind()) {
    case CK_IVAR:
      if (cr->Opr() == OPR_PARM) {
        cr = cr->Ilod_base();
      }
      break;
    case CK_OP:
      if (cr->Opr() == OPR_CVT || cr->Opr() == OPR_CVTL) {
        cr = cr->Opnd(0);
      }
    break;
    default:
    break;
  }
  return cr;
}
// =============================================================================
//
// VSA::Find_tor_list_from_cr, find tor list from cr
// search priority vor > cr
//
// =============================================================================
TOR_LIST *
VSA::Find_tor_list_from_cr(STMTREP *sr, CODEREP *cr, BOOL mu) const
{
  CODEREP *tag_cr = Find_cr_with_tag(cr);
  Is_True_Ret(tag_cr, ("Find_tor_list_from_cr: cr is NULL."), NULL);
  TOR_LIST *tor_list = NULL;
  VSYM_OBJ_REP *vor = Find_vor_with_tag(sr, tag_cr, mu);
  if (vor) {
    tor_list = vor->Tor_list<TOR_LIST>();
  }
  if (tor_list == NULL) {
    tor_list = Cr_2_tor_list<TOR_LIST>(tag_cr);
  }
  return tor_list;
  /* shall we continue find from base
  if(cr->Kind() == CK_IVAR) {
    CODEREP *base = (cr == sr->Lhs()) ? cr->Istr_base() : cr->Ilod_base();
    CODEREP *base_cr = Find_ilod_base(base);
    if(base_cr != NULL) {
      return Find_tor_list_from_cr(sr, base_cr, kind, mu);
    }
  }
  */
}

// =============================================================================
//
// VSA::Perform_tag_analysis, create tag, forward propagate tag
//
// =============================================================================
void
VSA::Perform_tag_analysis()
{
  if (!VSA_Enable_TAG) return;  // turn off TAG for triaging aid

  if (Dna()->Non_functional()) return;

  TB_LIST *tag_base_list = Ipsa()->Rbc()->Tag_base_list();
  if (tag_base_list == NULL || tag_base_list->Real_use_cnt() == 0) {
    return;  // if no tag created, no need to propagate
  }

  TAG_PROP tag_prop(Ipsa(), Dna()->Comp_unit());
  tag_prop.Do_prop();

  Is_Trace(Get_Trace(TP_VSA, VSA_TAG_PROP_DUMP_FLAG),
           (TFile, "%sAfter VSA::Perform_tag_analysis: Dump tag object\n%s",  SBar, SBar));
  Is_Trace_cmd(Get_Trace(TP_VSA, VSA_TAG_PROP_DUMP_FLAG), Print_obj("Tag Object", TFile));
}
