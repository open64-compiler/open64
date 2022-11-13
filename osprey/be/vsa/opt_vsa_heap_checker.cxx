//-*-c++-*-

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

// ====================================================================
// ====================================================================
//
// Module: opt_vsa_heap_checker.cxx
//
// ====================================================================
//

// ====================================================================
// VSA HEAP CHECKER
//   Check the HEAP issue:
//     * DBF: Double Free
//     * UAF: Use After Free
//     * MSF: Missing Free
//
// Algorithm:
//   DBF: start from call which frees memory and follow the U-D of
//        parameter to be freed and check the HEAP_OBJ along the U-D
//        traversal
//   UAF: start from IVAR and check if the HEAP_OBJ annotated on CODEREP
//        is freed or not
//   MSF: Stage 1: Single function MSF start from return sr and if the hor
//        mu is def by allocate and not escaped from current function 
//        Stage 2: cross function support
//
// ====================================================================

#include "defs.h"
#include "config_vsa.h"
#include "erglob.h"
#include "erbe.h"
#include "opt_defs.h"
#include "opt_bb.h"
#include "opt_main.h"
#include "opt_vsa_util.h"
#include "opt_vsa_eh.h"
#include "opt_vsa.h"
#include "opt_dna.h"
#include "opt_addr_util.h"
#include "opt_vra.h"
#include "opt_vsa_rbc.h"
#include "opt_vsa_checker.h"
#include "opt_vsa_hva.h"
#include "builtin_rule_defs.h"
#include "pro_core.h"

#define TRACE_INDENT  "         "

class HEAP_CHECKER : public SPOS_BASE {
  typedef CHECKER_TRAVELER<HEAP_CHECKER> CHECKER;
public:
  typedef enum { FOR_NONE, FOR_DBF, FOR_UAF, FOR_MSF } CHKIND;

  // traversal from dereference
  enum { SUSPECT = CS_DEREFERENCE | CS_VAR_DEF | CS_CONTAINER_UD |
                   CS_CALL | CS_HEAP_OBJ | CS_VSYM_OBJ | CS_RETURN };
  // need to check coderep
  enum { ENTITY = TE_CODEREP };
  // need to track USE_SRCPOS
  enum { USE_SRCPOS = TRUE };
  // TODO: need to follow eh path?
  enum { FOLLOW_EH = FALSE };
  // allocate status
  typedef enum { ALLOC_UNK, ALLOC_UNCHKED, ALLOC_SUCC, ALLOC_FAIL } ALLOC_STATUS;

private:
  CHECKER *_checker;
  CHKIND   _kind;
  BOOL     _chk_null;

  void Append_malloc_spos(HEAP_OBJ_REP* hor, TRAV_CONTEXT* ctx);

  ALLOC_STATUS Get_realloc_status(VSA* vsa, BB_NODE* use, STMTREP* def);

  BOOL Is_link_list(VSA *vsa, CODEREP* cr, CODEREP* tbf,
                    BOOL *maybe, hash_set<uintptr_t> &visited);

  BOOL Is_in_loop_last_trip(VSA *vsa, STMTREP *sr);

  BOOL Is_leaf_free(VSA *vsa, STMTREP *sr);

  void Add_vor_escaped_hos(VSYM_OBJ_REP *vor, hash_map<IDTYPE, BOOL> &escaped_hos,
                           hash_set<uintptr_t> &visited_vors, TRAV_CONTEXT *ctx);

  void Add_cr_escaped_hos(CODEREP *cr, hash_map<IDTYPE, BOOL> &escaped_hos,
                          hash_set<uintptr_t> &visited_crs, TRAV_CONTEXT *ctx);

  const char *Chkname(void);

  void Set_chk_null(BOOL v)   { _chk_null = v;    }
  BOOL Chk_null()             { return _chk_null; }


public:
  // HEAP_CHECKER
  // Constructor
  HEAP_CHECKER(TRAV_CONTEXT& ctx, CHECKER *checker)
   : SPOS_BASE(ctx), _checker(checker), _kind(FOR_NONE), _chk_null(TRUE) {
    ctx.Set_Tracing(Get_Trace(TP_CHECKER, CHK_HEAP_TRACE_FLAG));
  }

public:
  // Checker_name
  // return the name of the checker
  const char*     Checker_name() const  { return "HEAP"; }

  void            Set_kind(UINT32 kind) { _kind = (CHKIND)kind; }
  const           CHKIND Kind()         { return _kind; }
  BOOL            Set_check_kind(CHECKER_SUSPECT suspect);

public:
  // Check_coderep
  // call back to check coderep
  template<CODEKIND  _KIND> CHECKER_STATUS
  Check_coderep(CHECK_OBJ &obj, TRAV_CONTEXT* ctx);

  // Check_stmtrep
  // call back to check stmtrep
  template<OPERATOR _OPR> CHECKER_STATUS
  Check_stmtrep(CHECK_OBJ &obj, TRAV_CONTEXT* ctx);

  // Check_heap_obj
  CHECKER_STATUS
  Check_heap_obj(HEAP_OBJ_REP* hor, CHECK_OBJ &obj, TRAV_CONTEXT* ctx, BOOL back_edge = FALSE);

  CHECKER_STATUS
  Check_heap_obj(CHECK_OBJ &obj, TRAV_CONTEXT *ctx);

  // Check alias
  CHECKER_STATUS
  Check_alias(HEAP_OBJ_REP *alias_hor, HEAP_OBJ_REP *phi_hor,
              CHECK_OBJ &obj, TRAV_CONTEXT *ctx, BOOL back_edge);

  // Check_vsym_obj
  CHECKER_STATUS
  Check_vsym_obj(CHECK_OBJ &obj, TRAV_CONTEXT *ctx);

  void
  Add_target(UINT32 file_idx, ST_IDX idx) { }
};


BOOL
HEAP_CHECKER::Set_check_kind(CHECKER_SUSPECT suspect)
{
  switch (suspect) {
    case CS_DEREFERENCE:
      Set_kind(FOR_UAF);
      return TRUE;
    case CS_RETURN:
      Set_kind(FOR_MSF);
      return TRUE;
    default:
      return FALSE;
  }
  return FALSE;
}

const char *
HEAP_CHECKER::Chkname(void)
{
  switch (Kind()) {
    case FOR_NONE:
      return "None";
    case FOR_DBF:
      return "DBF";
    case FOR_UAF:
      return "UAF";
    case FOR_MSF:
      return "MSF";
    default:
      return "INVALID";
  }
}

// ====================================================================
// HEAP_CHECKER::Append_malloc_spos(HEAP_OBJ_REP* hor, TRAV_CONTEXT* ctx)
//   Append malloc srcpos to current srcpos_handle
// ====================================================================
void
HEAP_CHECKER::Append_malloc_spos(HEAP_OBJ_REP* hor, TRAV_CONTEXT* ctx)
{
  while (hor != NULL) {
    switch (hor->Attr()) {
    case ROR_DEF_BY_PHI:
      {
        PHI_NODE* phi = hor->Phi_def();
        Is_True(phi != NULL, ("phi is null"));
        if (phi == NULL || ctx->Visited(phi->Bb())) {
          hor = NULL;
          break;
        }
        BB_NODE* bb = phi->Bb();
        Append_data(bb, ctx->Dna(), PATHINFO_PHI);
        IDTYPE sym = hor->Heap_obj()->Sym_id();
        PHI_NODE* var_phi = bb->Phi_list()->Search_phi_node(sym);
        if (var_phi != NULL && var_phi->Live()) {
          // find the first opnd with has the ho
          BB_NODE* pred;
          BB_LIST_ITER bb_iter;
          INT pred_idx = 0;
          FOR_ALL_ELEM(pred, bb_iter, Init(bb->Pred())) {
            CODEREP* opnd = var_phi->OPND(pred_idx);
            HEAP_OBJ_REP* opnd_hor = ctx->Vsa()->Cr_2_heap_obj(opnd);
            if (opnd_hor != NULL && opnd_hor->Heap_obj() == hor->Heap_obj()) {
              Append_data(pred, ctx->Dna(), PATHINFO_BRANCH);
              hor = (HEAP_OBJ_REP*)phi->OPND(pred_idx);
              break;
            }
            ++ pred_idx;
          }
          if (pred_idx == phi->Size())
            hor = NULL;
        }
        else {
          hor = NULL;
        }
      }
      break;
    case ROR_DEF_BY_CHI:
      {
        STMTREP* stmt = hor->Stmt_def();
        Is_True(hor->Is_entry_chi() || stmt != NULL, ("defstmt is null"));
        if (stmt == NULL) {
          hor = NULL;
          break;
        }
        else if (stmt->Opr() == OPR_OPT_CHI) {
          Append_data(stmt, ctx->Dna(), PATHINFO_PARM);
        }
        else if (OPERATOR_is_call(stmt->Opr())) {
          if (stmt->Callee_returns_new_heap_memory()) {
            Append_data(stmt, ctx->Dna(), PATHINFO_ALLOC);
          }
          else {
            Append_data(stmt, ctx->Dna(), PATHINFO_CALL_CHI);
          }
        }
        else {
          Is_True(FALSE, ("not def by entry or call"));
          Append_data(stmt, ctx->Dna(), PATHINFO_CHI);
        }
        hor = NULL;
        break;
      }
      break;
    case ROR_DEF_BY_ALLOC:
      {
        STMTREP* stmt = hor->Stmt_def();
        Is_True(stmt != NULL, ("defstmt is null"));
        if (stmt != NULL) {
          Append_data(stmt, ctx->Dna(), PATHINFO_ALLOC);
        }
        hor = NULL;
        break;
      }
      break;
    case ROR_DEF_BY_IPARM:
    case ROR_DEF_BY_AUTO:
      {
        hor = NULL;
        break;
      }
    default:
      hor = hor->Prev_ver();
      break;
    }
  }
}

// ====================================================================
// HEAP_CHECKER::Get_realloc_status(BB_NODE* use, STMTREP* def)
//   Get reallocation status
// ====================================================================
inline HEAP_CHECKER::ALLOC_STATUS
HEAP_CHECKER::Get_realloc_status(VSA* vsa, BB_NODE* use, STMTREP* def)
{
  // check if def is realloc
  if (!vsa->Callee_frees_heap_memory(def) ||
      !vsa->Callee_returns_new_heap_memory(def))
    return ALLOC_UNK;

  // check vra
  VRA* vra = vsa->Comp_unit()->Vra();
  if (vra == NULL)
    return ALLOC_UNK;

  // get hor/ho on realloc return value
  CODEREP* ret = vsa->Comp_unit()->Find_return_value(def);
  if (ret == NULL)   // return value eliminated
    return ALLOC_UNK;
  HEAP_OBJ_REP* ret_hor = vsa->Cr_2_heap_obj(ret);
  Is_True_Ret(ret_hor != NULL, ("not find hor on return value"), ALLOC_UNK);
  HEAP_OBJ* ret_ho = ret_hor->Heap_obj();

  // check control dependency between use and def
  ALLOC_STATUS ret_sts = ALLOC_UNCHKED;
  std::vector< std::pair<BB_NODE*, BB_NODE*> > cds;
  vra->Collect_control_dependencies(use, def->Bb(), cds);
  std::vector< std::pair<BB_NODE*, BB_NODE*> >::iterator it;
  for (it = cds.begin(); it != cds.end(); ++it) {
    STMTREP* sr = it->first->Last_stmtrep();
    // ignore non-truebr/falsebr
    if (sr == NULL ||
        (sr->Opr() != OPR_TRUEBR &&
         sr->Opr() != OPR_FALSEBR))
      continue;

    // check crs used in control dependency
    hash_set<CODEREP*> crs;
    vsa->Find_coderep_in_last_stmtrep(it->first, crs);
    for (hash_set<CODEREP*>::iterator it = crs.begin();
         it != crs.end(); ++it) {
      CODEREP* cr = *it;
      HEAP_OBJ_REP* cr_hor = vsa->Cr_2_heap_obj(cr);
      // skip this cr is not the same hor
      if (cr_hor == NULL || cr_hor->Heap_obj() != ret_ho)
        continue;

      // check if cr equls to 0 and return TRUE if cr == 0
      PATH_SELECTED paths;
      VRA_RESULT va = vra->Var_cmp_val<OPR_EQ>(cr, use->Id(), (INT64)0, paths);
      if (va == VA_YES) {
        ret_sts = ALLOC_FAIL;
        break;
      }
      else {
        ret_sts = ALLOC_SUCC;
      }
    }
  }

  // by default return false
  return ret_sts;
}

// ====================================================================
// HEAP_CHECKER::Is_link_list(CODEREP* cr, CODEREP* tbf)
//   Check if cr/tbf are nodes on linked list
// ====================================================================
BOOL
HEAP_CHECKER::Is_link_list(VSA *vsa, CODEREP* cr, CODEREP* tbf,
                           BOOL *maybe, hash_set<uintptr_t> &visited)
{
  // cannot be non-var nor zero-version/def-by-chi/def-by-phi
  if (cr->Kind() != CK_VAR ||
      cr->Is_flag_set(CF_IS_ZERO_VERSION) ||
      cr->Is_flag_set(CF_DEF_BY_CHI)) {
    if (maybe)
      *maybe = TRUE;
    return FALSE;
  }

  // check phi for the following pattern
  // case 1:
  // while (p) {
  //   cur = p;
  //   p = p->next;
  //   free(cur);
  // }
  // case 2:
  // while (p != tail) {
  //   p = phi(p, p);
  //   next = p->next;
  //   free(p);  <-- tbf
  //   p = p->next;
  // }
  // p = phi(p, p);
  // free(p); <-- cr
  if (cr->Is_flag_set(CF_DEF_BY_PHI)) {
    PHI_NODE* phi = cr->Defphi();
    Is_True(phi != NULL, ("phi node is NULL"));
    // visited again, assume is linked list
    if (visited.find((uintptr_t)phi) != visited.end())
      return TRUE;
    visited.insert((uintptr_t)phi);
    BB_NODE* phi_bb = phi->Bb();
    BB_NODE* bb_pred;
    BB_LIST_ITER bb_iter;
    INT32 i = 0;
    // case 1:
    FOR_ALL_ELEM(bb_pred, bb_iter, Init(phi_bb->Pred())) {
      if (!phi_bb->Dominates(bb_pred)) {
        ++i;
        continue;
      }
      CODEREP* opnd = phi->OPND(i);
      // ignore phi now
      if (opnd->Is_flag_set(CF_DEF_BY_PHI))
        return FALSE;
      return Is_link_list(vsa, opnd, cr, maybe, visited);
    }
    // case 2:
    i = 0;
    FOR_ALL_ELEM(bb_pred, bb_iter, Init(phi_bb->Pred())) {
      if (bb_pred->Dominates(phi_bb)) {
        ++i;
        continue;
      }
      CODEREP* opnd = phi->OPND(i);
      if (opnd->Is_flag_set(CF_DEF_BY_PHI))
        return FALSE;
      return Is_link_list(vsa, opnd, tbf, maybe, visited);
    }
  }

  // must have a defstmt
  if (cr->Defstmt() == NULL || cr->Defstmt()->Rhs() == NULL)
    return FALSE;

  CODEREP* rhs = cr->Defstmt()->Rhs();
  // check stid-ldid
  if (rhs->Kind() == CK_VAR)
    return Is_link_list(vsa, rhs, tbf, maybe, visited);

  // check stid-iload-stid for the following pattern
  // case 3:
  // while (p) {
  //   next = p->next;
  //   free(p);
  //   p = next;
  // }
  // check istore-free-iload for the following pattern
  // case 4:
  // while (p) {
  //   *q = p->next;
  //   free(p);
  //   p = *q;
  // }
  if (rhs->Kind() == CK_IVAR) {
    // check for case 3
    if (rhs->Ilod_base()->Contains(tbf))
      return TRUE;
    // check for case 4
    VSYM_OBJ_REP *vor = vsa->Cr_2_vor(rhs);
    if (vor && vor->Attr() == ROR_DEF_BY_ISTORE) {
      STMTREP* vor_def = vor->Stmt_def();
      Is_True(vor_def && vor_def->Opr() == OPR_ISTORE,
              ("bad vor_def"));
      rhs = vor_def->Rhs();
      if (rhs->Kind() == CK_IVAR &&
          rhs->Ilod_base()->Contains(tbf))
        return TRUE;
    }
  }

  // return FALSE by default
  return FALSE;
}

// ====================================================================
// HEAP_CHECKER::Is_leaf_free(VSA *vsa, STMTREP *sr)
//   Check if the free call is leaf free by call std free or rbc 
//   annoted free
// ====================================================================
BOOL
HEAP_CHECKER::Is_leaf_free(VSA *vsa, STMTREP *sr)
{
  // free call
  if (sr->Callee_frees_heap_memory()) {
    return TRUE;
  } else {
    RNA_NODE *rna = vsa->Dna()->Get_callsite_rna(sr);
    CALLEE_VECTOR::const_iterator callee_iter;
    for (callee_iter = rna->Callee_list().begin();
         callee_iter != rna->Callee_list().end();
         ++callee_iter) {
      DNA_NODE *callee = vsa->Ipsa()->Get_dna(callee_iter->Callee());
      // free marked by rbc
      if (callee && callee->Deallocate() && callee->Non_functional()) {
        return TRUE;
      }
    }
  }
  return FALSE;
}

// ====================================================================
// HEAP_CHECKER::Is_in_loop_last_trip(STMTREP *sr)
//   Check if sr in loop's last trip
// ====================================================================
BOOL
HEAP_CHECKER::Is_in_loop_last_trip(VSA *vsa, STMTREP *sr)
{
  VRA* vra = vsa->Comp_unit()->Vra();
  if (vra == NULL || sr == NULL)
    return FALSE;

  BB_NODE *bb = sr->Bb();
  BB_LOOP *loop_info = bb->Innermost();
  // no loop info or inner another loop return FALSE
  if(loop_info == NULL || loop_info->Parent() != NULL)
    return FALSE;

  CODEREP *iv = loop_info->Iv();
  BB_NODE *loop_body = loop_info->Body();
  if(iv == NULL || loop_body == NULL)
    return FALSE;

  CODEREP *loop_lb = NULL;
  CODEREP *loop_ub = NULL;
  CODEREP *cur_min = NULL;
  CODEREP *cur_max = NULL;
  BOOL has_bound = vra->Get_bounds(iv, loop_body, loop_lb, loop_ub);
  BOOL has_bound2 = vra->Get_bounds(iv, bb, cur_min, cur_max);
  if(has_bound && has_bound2 && loop_ub && cur_min && cur_min == cur_max) {
    PATH_SELECTED paths;
    VRA_RESULT res = vra->Compare_cr<OPR_EQ>(cur_max, bb, loop_ub, paths);
    if(res == VA_YES) {
      return TRUE;
    }
  }
  return FALSE;
}


// ====================================================================
// HEAP_CHECKER::Add_vor_escaped_hos
//   add escaped hors by iterate vor phi opnd/vor chi opnd rhs hor
// ====================================================================
void
HEAP_CHECKER::Add_vor_escaped_hos(VSYM_OBJ_REP *vor, hash_map<IDTYPE, BOOL> &escaped_hos,
                                  hash_set<uintptr_t> &visited_vors, TRAV_CONTEXT* ctx)
{
  if (visited_vors.find((uintptr_t)vor) != visited_vors.end()) {
    return;
  }
  visited_vors.insert((uintptr_t)vor);
  switch (vor->Attr()) {
    case ROR_DEF_BY_PHI:
    {
      PHI_NODE* phi = vor->Phi_def();
      for (INT i = 0; i < phi->Size(); ++i) {
        VSYM_OBJ_REP *vor_opnd = (VSYM_OBJ_REP *) phi->OPND(i);
        if (vor_opnd->Hor()) {
          Is_Trace(ctx->Tracing(),
                  (TFile, "  [%s]: add escape by return vo%dv%d mu phi opnd rhs hor:",
                    Chkname(), vor_opnd->Vsym_obj()->Id(), vor_opnd->Version()));
          Is_Trace_cmdn(ctx->Tracing(), vor_opnd->Hor()->Print_detail(TFile), TFile);
          escaped_hos[vor_opnd->Hor()->Heap_obj()->Id()] = FALSE;
        }
        Add_vor_escaped_hos(vor_opnd, escaped_hos, visited_vors, ctx);
      }
      break;
    }
    case ROR_DEF_BY_CHI:
    {
      STMTREP *def_sr = vor->Stmt_def();
      if (def_sr) {
        VSYM_OBJ_REP *vor_opnd = ctx->Vsa()->Find_stmt_cur_vor(def_sr, vor->Vsym_obj());
        if (vor_opnd) {
          if (vor_opnd->Hor()) {
            Is_Trace(ctx->Tracing(),
                    (TFile, "  [%s]: add escape by return vo%dv%d mu chi opnd rhs hor:",
                      Chkname(), vor_opnd->Vsym_obj()->Id(), vor_opnd->Version()));
            Is_Trace_cmdn(ctx->Tracing(), vor_opnd->Hor()->Print_detail(TFile), TFile);
            escaped_hos[vor_opnd->Hor()->Heap_obj()->Id()] = FALSE;
          }
          Add_vor_escaped_hos(vor_opnd, escaped_hos, visited_vors, ctx);
        }
      }
      break;
    }
  }
}

// ====================================================================
// HEAP_CHECKER::Add_cr_escaped_hos
//   add escaped hors by iterate cr phi opnd/cr chi opnd rhs hor
// ====================================================================
void
HEAP_CHECKER::Add_cr_escaped_hos(CODEREP *cr, hash_map<IDTYPE, BOOL> &escaped_hos,
                                 hash_set<uintptr_t> &visited_crs, TRAV_CONTEXT* ctx)
{
  Is_True(cr && cr->Kind() == CK_VAR, ("not var cr"));
  if (visited_crs.find((uintptr_t)cr) != visited_crs.end()) {
    return;
  }
  visited_crs.insert((uintptr_t)cr);
  STMTREP *def_sr;
  if (cr->Is_flag_set(CF_DEF_BY_PHI)) {
    PHI_NODE* phi = cr->Defphi();
    for (INT i = 0; i < phi->Size(); ++i) {
      CODEREP *opnd = phi->OPND(i);
      HEAP_OBJ_REP *hor = ctx->Vsa()->Find_cr_heap_obj(opnd);
      if (hor) {
        Is_Trace(ctx->Tracing(),
                (TFile, "  [%s]: add escape by return sym%dv%d cr%d mu phi opnd rhs hor:",
                  Chkname(), opnd->Aux_id(), opnd->Version(), opnd->Coderep_id()));
        Is_Trace_cmdn(ctx->Tracing(), hor->Print_detail(TFile), TFile);
        escaped_hos[hor->Heap_obj()->Id()] = FALSE;
      }
      Add_cr_escaped_hos(opnd, escaped_hos, visited_crs, ctx);
    }
  }
  else if ((def_sr = cr->Defstmt()) != NULL) {
    CODEREP *opnd = NULL;
    if (cr->Is_flag_set(CF_DEF_BY_CHI)) {
      if (cr->Defchi()->Live())
        opnd = cr->Defchi()->OPND();
    }
    else {
      if (def_sr->Rhs() && def_sr->Rhs()->Kind() == CK_VAR)
        opnd = def_sr->Rhs();
    }

    if (opnd) {
      HEAP_OBJ_REP *hor = ctx->Vsa()->Find_stmt_hor_mu(def_sr, opnd);
      if (hor) {
        Is_Trace(ctx->Tracing(),
                (TFile, "  [%s]: add escape by return sym%dv%d cr%d mu chi opnd rhs hor:",
                  Chkname(), opnd->Aux_id(), opnd->Version(), opnd->Coderep_id()));
        Is_Trace_cmdn(ctx->Tracing(), hor->Print_detail(TFile), TFile);
        escaped_hos[hor->Heap_obj()->Id()] = FALSE;
      }
      Add_cr_escaped_hos(opnd, escaped_hos, visited_crs, ctx);
    }
  }
}

// ====================================================================
// HEAP_CHECKER::Check_heap_obj(CHECK_OBJ &obj, TRAV_CONTEXT *ctx)
//   Callback to check HEAP_OBJ_REP
// ====================================================================
CHECKER_STATUS inline
HEAP_CHECKER::Check_heap_obj(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  Is_True_Ret(obj.Is_vsym(), ("CHECK_OBJ should be vsym"), CS_DONE);
  VSYM_OBJ_REP *vor = obj.Vor();
  Is_True_Ret(vor, ("CHECK_OBJ vor is null"), CS_DONE);
  HEAP_OBJ_REP *hor = vor->Hor();
  if (hor) {
    HEAP_OBJ_REP *cur_hor = ctx->Vsa()->Find_stmt_cur_hor(obj.Stmtrep(), hor->Heap_obj());
    if (cur_hor) {
      hor = cur_hor;
    }
    Is_Trace(ctx->Tracing(), (TFile, "  [%s]: check vor rhor: ", Chkname()));
    Is_Trace_cmd(ctx->Tracing(), vor->Print(TFile));
    Is_True_Ret(obj.Vor_cr(), ("null vor cr"), CS_DONE);
    ctx->Tracker()->Pop();
    obj.Update_var(obj.Vor_cr());
    return Check_heap_obj(hor, obj, ctx);
  }
  return CS_CONT;
}

// ====================================================================
// HEAP_CHECKER::Check_alias
// check aliased heap status
// ====================================================================
CHECKER_STATUS inline
HEAP_CHECKER::Check_alias(HEAP_OBJ_REP *alias_hor, HEAP_OBJ_REP *phi_hor,
                          CHECK_OBJ &obj, TRAV_CONTEXT *ctx, BOOL back_edge)
{
  Is_True_Ret(phi_hor->Is_phi(), ("alias is not phi"), CS_DONE);
  if (phi_hor->Attr() == ROR_DEF_BY_PHI) {
    // for normal phi, the ulist coming from phi result propagation, skip check for now
    Is_Trace(ctx->Tracing(), (TFile, "  [%s]: CS_DONE alias is normal phi ", Chkname()));
    Is_Trace_cmd(ctx->Tracing(), ctx->Vsa()->Print_hor_phi(phi_hor->Phi_def(), TFile, TRUE));
    return CS_DONE;
  }
  Is_True_Ret(phi_hor != alias_hor, ("alias hor is the same as phi"), CS_DONE);
  Is_Trace(ctx->Tracing(), (TFile, "  [%s]: Check alias hor:", Chkname()));
  Is_Trace_cmd(ctx->Tracing(), alias_hor->Print(TFile));
  Is_Trace(ctx->Tracing(), (TFile, "  from varphi "));
  Is_Trace_cmdn(ctx->Tracing(), phi_hor->Print(TFile), TFile);

  PHI_NODE *phi = phi_hor->Phi_def();
  BB_NODE *phi_bb = phi->Bb();
  if (ctx->Visited(phi_bb)) {
    Is_Trace(ctx->Tracing(), (TFile, "%signored. phi already visited.\n", TRACE_INDENT));
    return CS_DONE;
  }
  Is_Trace_cmd(ctx->Tracing(), ctx->Vsa()->Print_hor_phi(phi, TFile, TRUE));
  Append_data(phi_bb, ctx->Dna(), PATHINFO_PHI);
  SRCPOS_TREENODE* cur_node = Add_children(phi->Size());
  IDTYPE parent_idx = Cur_idx();
  for (INT i = 0; i < phi->Size(); ++i) {
    HEAP_OBJ_REP* hor_opnd = (HEAP_OBJ_REP*) phi->OPND(i);
    BB_NODE *pred_bb = phi_bb->Nth_pred(i);
    if (hor_opnd->Heap_obj() != alias_hor->Heap_obj()) {
      Is_Trace(ctx->Tracing(), (TFile, "  Phi BB%d: select BB%d hor:", phi_bb->Id(), pred_bb->Id()));
      Is_Trace_cmd(ctx->Tracing(), hor_opnd->Print_detail(TFile));
      HEAP_OBJ_MAP* ho_ids = ctx->Vsa()->Find_ignore_ho_ids(pred_bb);
      HEAP_OBJ_MAP::iterator ho_iter;
      if (ho_ids != NULL &&
          (ho_iter = ho_ids->find(alias_hor->Heap_obj()->Id())) != ho_ids->end()) {
        if (ho_iter->second == VA_NO) {
          // compared with NULL
          Is_Trace(ctx->Tracing(), (TFile, "  - Skip: hor is in BB%d msf ignore ho list: ", pred_bb->Id()));
          Is_Trace_cmdn(ctx->Tracing(), alias_hor->Print_detail(TFile), TFile);
          continue;
        }
        else {
          // compared with something else, set to M
          Sp_h()->Set_flag(SRCPOS_FLAG_MAYBE);
        }
      }
      Set_cur_node(cur_node, i);
      Sp_h()->Append_data(pred_bb, ctx->Dna(), PATHINFO_BRANCH);
      HEAP_OBJ_REP *check_hor = alias_hor;
      if (alias_hor->Attr() == ROR_DEF_BY_PHI && alias_hor->Phi_def()->Bb() == phi_bb) {
        Is_Trace(ctx->Tracing(), (TFile, "  Adjust alias_hor for same phi bb: "));
        Is_Trace_cmd(ctx->Tracing(), ctx->Vsa()->Print_hor_phi(alias_hor->Phi_def(), TFile, TRUE));
        check_hor = (HEAP_OBJ_REP*)(alias_hor->Phi_def()->OPND(i));
      }
      CHECK_OBJ alias_obj(obj);
      alias_obj.Set_phi(phi, pred_bb);
      HOR_LIST *ulist = hor_opnd->Attr() == ROR_DEF_BY_VARPHI ? hor_opnd->Ulist() : NULL;
      if (ulist && ulist->Find(alias_hor) != NULL) {
        Check_alias(check_hor, hor_opnd, alias_obj, ctx, back_edge);
      } else {
        Check_heap_obj(check_hor, alias_obj, ctx, back_edge);
      }
    }
  }
  Reset_cur_node(cur_node, parent_idx);
  return CS_DONE;
}

// ====================================================================
// HEAP_CHECKER::Check_heap_obj(HEAP_OBJ_REP* hor, CODEREP* cr,
//                              STMTREP* sr, TRAV_CONTEXT* ctx)
//   Check HEAP_OBJ_REP
// ====================================================================
CHECKER_STATUS inline
HEAP_CHECKER::Check_heap_obj(HEAP_OBJ_REP* hor, CHECK_OBJ &obj,
                             TRAV_CONTEXT* ctx, BOOL back_edge)
{
  Is_True(hor != NULL, ("invalid hor"));
  STMTREP *sr = obj.Stmtrep();
  VSA* vsa = ctx->Vsa();
  CODEREP *cr = obj.Is_var() ? obj.Coderep() : obj.Vor_cr();
  if (cr == NULL) {
    Is_True(sr != NULL, ("null sr"));
    cr = vsa->Find_vor_chi_cr(sr, obj.Vor());
    if (cr == NULL) {
      Is_Trace(ctx->Tracing(), (TFile, "  [%s]: Done: check cr is null\n", Chkname()));
      return CS_DONE;
    }
  }

  Is_Trace(ctx->Tracing(), (TFile, "  [%s]: Check hor:", Chkname()));
  Is_Trace_cmd(ctx->Tracing(), hor->Print_detail(TFile));
  switch (hor->Attr()) {
  case ROR_DEF_BY_FREE:
  case ROR_DEF_BY_DANGLE:
    {
      STMTREP* def_stmt = hor->Stmt_def();
      BOOL maybe = FALSE;
      CODEREP* tbf = vsa->Callee_frees_heap_memory(def_stmt, &maybe);
      if (Kind() == FOR_MSF) {
        if (tbf && hor->Attr() == ROR_DEF_BY_FREE) {
          HEAP_OBJ_REP *free_hor = vsa->Cr_2_heap_obj(def_stmt->Rhs());
          Is_True_Ret(free_hor, ("null free hor"), CS_DONE);
          if (!free_hor->Prev_ver()) {
            // realloc
            if (sr && sr->Opr() == OPR_RETURN) {
              ALLOC_STATUS realloc_sts = Get_realloc_status(vsa, sr->Bb(), def_stmt);
              if (realloc_sts == ALLOC_FAIL || realloc_sts == ALLOC_UNCHKED ) {
                // realloc reaches to RETURN, MSF when realloc failed to realloc new memory
                if (_checker->Issue_reported(def_stmt, NULL, Kind())) {
                  Is_Trace(ctx->Tracing(),
                          (TFile, "--HEAP: MSF on sr%d line %d already reported.\n",
                                  def_stmt->Stmtrep_id(),
                                  Srcpos_To_Line(def_stmt->Linenum())));
                  return CS_DONE;
                }
                Sp_h()->Set_key_srcpos(ctx->Dna(), def_stmt, NULL);
                Append_data(def_stmt, ctx->Dna(), PATHINFO_ALLOC);
                ISSUE_CERTAINTY ic = VSA_Issue_Certainty_Maybe ? IC_MAYBE : IC_DEFINITELY;
                vsa->Report_vsa_error(cr, (char *)NULL, MSF, ic, Sp_h());
                if (VSA_Xsca) {
                  vsa->Report_xsca_error(cr, Sp_h()->Orig_stname(), "MSR_22_1", Sp_h());
                }
                Sp_h()->Remove_last_key_srcpos();
              }
            }
            Is_Trace(ctx->Tracing(), (TFile, "  [%s]: Done: realloc, free prev is not set\n", Chkname()));
            return CS_DONE;
          }
          free_hor = free_hor->Prev_ver();
          // aliased free, assume the aliased hor is not selected in phi, check
          // the heap status before free
          if (free_hor->Attr() == ROR_DEF_BY_VARPHI &&
              free_hor->Heap_obj() != hor->Heap_obj()) {
            Is_Trace_cmdn(ctx->Tracing(), vsa->Print_sr(def_stmt, TFile), TFile);
            hor = vsa->Find_stmt_cur_hor(def_stmt, hor->Heap_obj());
            Is_True_Ret(hor, ("null hor prev for free"), CS_DONE);
            if (hor->Attr() == ROR_DEF_BY_VARPHI) {
              Is_Trace(ctx->Tracing(), (TFile, "  skip varphi\n"));
              break;
            }
            Is_Trace(ctx->Tracing(), (TFile, "       - freed by alias "));
            Is_Trace_cmd(ctx->Tracing(), free_hor->Print_detail(TFile));
            obj.Set_stmtrep(def_stmt);
            Check_alias(hor, free_hor, obj, ctx, back_edge);
            return CS_DONE;
          }
        }
        Is_Trace(ctx->Tracing(), (TFile, "  [%s]: Done: free \n", Chkname()));
        return CS_DONE;
      }
      // exclude wrong UAF/DBF for cr contains iv inside loop
      // for (int i...) {
      //    free(p[i]);
      // }
      if (back_edge && cr == tbf && obj.Bb() != NULL) {
        BB_LOOP *loop_info = obj.Bb()->Loop();
        if (loop_info && loop_info->Iv() && cr->Contains(loop_info->Iv())) {
          Is_Trace(ctx->Tracing(), (TFile, "  [%s]: Done: free with iv is in a loop\n", Chkname()));
          return CS_DONE;
        }
      }
      if (def_stmt) {
        // fix bug 798, FP on free(cur) when it frees up a linked list
        if (back_edge) {
          // comment out check for stmt reachable to reduce FP (sr maybe null)
          // (sr == def_stmt || vsa->Is_stmt_reaches(sr, def_stmt))) {
          // if the def_stmt is from the back edge for a loop, there is no error
          Is_Trace(ctx->Tracing(), (TFile, "  [%s]: Done: free is in a loop\n", Chkname()));
          return CS_DONE;
        }
        Append_data(def_stmt, ctx->Dna(), PATHINFO_FREE);
      }
      if (Kind() == FOR_DBF)
        Append_malloc_spos(hor, ctx);
      // exclude wrong UAF on q in the pattern like
      // q = p->next;
      // free(p);
      // p = q;
      hash_set<uintptr_t> visited;
      if (tbf != NULL && tbf != cr && Is_link_list(vsa, cr, tbf, &maybe, visited))
        return CS_DONE;

      // exclude wrong UAF or DBF in pattern like
      // for(int i = 0; i<10; i++) {
      //   p[i] = 'a';
      //   if(i == 9)  free(p);
      // }
      if(tbf != NULL && vsa->Is_stmt_dominates(sr, def_stmt) &&
         Is_in_loop_last_trip(vsa, def_stmt))
        return CS_DONE;

      // exclude wrong UAF or DBF on the pattern like
      // q = realloc(p, sz);
      // if (q == 0) {
      //   x = *p;   // Not UAF
      //   free(p);  // Not DBF
      // }
      Is_True(obj.Bb() != NULL, ("no bb on obj"));
      if (Get_realloc_status(vsa, obj.Bb(), def_stmt) == ALLOC_FAIL)
        return CS_DONE;

      if (_checker->Issue_reported(def_stmt, NULL, Kind())) {
        Is_Trace(ctx->Tracing(),
                 (TFile, "--HEAP: %s caused by sr%d line %d already reported.\n",
                         Kind() == FOR_UAF ? "UAF.1" : "DBF.1",
                         def_stmt->Stmtrep_id(),
                         Srcpos_To_Line(def_stmt->Linenum())));
        return CS_DONE;
      }
      // if hor is injured (merged hor for linked list, array, etc), report M UAF
      if (maybe == FALSE) {
        maybe = hor->Injured();
      }
      // if not leaf free, free is created by propagation, set maybe to true
      if (maybe == FALSE) {
        maybe = !Is_leaf_free(vsa, def_stmt);
      }
      // if this is an aliased free, report M UAF
      HEAP_OBJ_REP *free_hor = vsa->Cr_2_heap_obj(def_stmt->Rhs());
      if (maybe == FALSE && free_hor != NULL) {
        maybe = (hor->Heap_obj() != free_hor->Heap_obj());
      }

      // if memory maybe freed, set srcpos maybe flag
      if (maybe)
        Sp_h()->Set_flag(SRCPOS_FLAG_MAYBE);
      // treat the free stmt as the "key" srcpos for UAF/DBF issue
      Sp_h()->Set_key_srcpos(ctx->Dna(), def_stmt, NULL);
      // cr->Is_flag_set(CF_DEF_BY_PHI) is TRUE, Need to filter out free from back edge
      Sp_h()->Set_msgid(Kind() == FOR_UAF ? "UAF.1" : "DBF.1");
      ISSUE_CERTAINTY ic = VSA_Issue_Certainty_Maybe ? IC_MAYBE : IC_DEFINITELY;
      vsa->Report_vsa_error(cr, (const char*)NULL,
                            Kind() == FOR_UAF ? UAF : DBF, ic, Sp_h());
      if (VSA_Xsca) {
        if (Kind() != FOR_UAF) {
          vsa->Report_xsca_error(cr, Sp_h()->Orig_stname(), "MSR_22_2", Sp_h());
        }
      }
      Sp_h()->Remove_last_key_srcpos();
      return CS_DONE;
    }
    break;
  case ROR_DEF_BY_VARPHI:
  case ROR_DEF_BY_VORPHI:
    if (Kind() == FOR_MSF) {
      Is_Trace(ctx->Tracing(), (TFile, "  [%s]: skip var/vor phi hor:", Chkname()));
      Is_Trace_cmdn(ctx->Tracing(), hor->Print(TFile), TFile);
      break;
    }
    // fall through
  case ROR_DEF_BY_PHI:
    {
      PHI_NODE *phi = hor->Phi_def();
      Is_True(phi != NULL, ("not find PHI hor phi node"));
      BB_NODE  *phi_bb = phi->Bb();
      CODEREP  *phi_iv = phi_bb->Loop() ? phi_bb->Loop()->Iv() : NULL;
      if (ctx->Visited(phi_bb)) {
        Is_Trace(ctx->Tracing(), (TFile, "%signored. phi already visited.\n", TRACE_INDENT));
        return CS_DONE;
      }

      PHI_NODE *var_phi = NULL;
      STMTREP *var_def = NULL;
      BOOL is_vor = FALSE;
      if (hor->Attr() == ROR_DEF_BY_VARPHI ||
          hor->Attr() == ROR_DEF_BY_VORPHI) {
        var_phi = hor->Heap_obj()->Base_phi();
        Is_True_Ret(var_phi, ("var/vor phi base phi not set"), CS_DONE);
        is_vor = Phi_res_is_vor(var_phi);
      } else {
        VSYM_OBJ_REP *vor;
        if (cr->Kind() == CK_VAR) {
          if (cr->Is_flag_set(CF_DEF_BY_PHI)) {
            var_phi = cr->Defphi();
          }
          else {
            var_def = cr->Defstmt();
          }
        }
        else if (cr->Kind() == CK_IVAR &&
                 (vor = vsa->Cr_2_vor(cr)) != NULL) {
          is_vor = TRUE;
          switch (vor->Attr()) {
          case ROR_DEF_BY_PHI:
            var_phi = vor->Phi_def();
            break;
          case ROR_DEF_BY_ISTORE:
          case ROR_DEF_BY_COPY:
          case ROR_DEF_BY_CHI:
            var_def = vor->Stmt_def();
            break;
          default:
            break;
          }
        }
      }
      BOOL def_in_phi = var_phi ||
                        (is_vor && phi_iv && cr->Contains(phi_iv)) ||
                        (var_def && phi_bb->Dominates(var_def->Bb()));

      Append_data(phi_bb, ctx->Dna(), PATHINFO_PHI);
      SRCPOS_TREENODE* cur_node = Add_children(phi->Size());
      IDTYPE parent_idx = Cur_idx();
      Push_mark(phi);
      BB_NODE* bb_pred;
      BB_LIST_ITER bb_iter;
      INT32 i = -1;
      Is_Trace(ctx->Tracing(), (TFile, "%s", TRACE_INDENT));
      Is_Trace_cmd(ctx->Tracing(), ctx->Vsa()->Print_hor_phi(phi, TFile, TRUE));
      FOR_ALL_ELEM(bb_pred, bb_iter, Init(phi_bb->Pred())) {
        i++;
        HEAP_OBJ_REP* hor_opnd = (HEAP_OBJ_REP*) phi->OPND(i);
        Is_Trace(ctx->Tracing(), (TFile, "  [%s]: Check hor phi opnd[%d/%d]:",
                                  Chkname(), i, phi_bb->Pred()->Len() - 1));
        Is_Trace_cmd(ctx->Tracing(), hor->Print(TFile));
        Is_Trace(ctx->Tracing(), (TFile, ":"));
        Is_Trace_cmd(ctx->Tracing(), hor_opnd->Print(TFile));
        // get hor opnd current version on check sr
        // if opnd hor's def phi bb is the same as current bb do not adjust it
        if (sr && hor_opnd->Heap_obj() != hor->Heap_obj()) {
          HEAP_OBJ_REP * cur_hor = ctx->Vsa()->Find_stmt_cur_hor(sr, hor_opnd->Heap_obj());
          if (cur_hor && (!cur_hor->Is_phi() || cur_hor->Phi_def()->Bb() != phi_bb)) {
            hor_opnd = cur_hor;
            Is_Trace(ctx->Tracing(), (TFile, ":"));
            Is_Trace_cmd(ctx->Tracing(), hor_opnd->Print(TFile));
          }
        }
        CODEREP *cr_opnd = cr;
        if (var_phi != NULL && var_phi->Bb() == phi_bb) {
          // in the following pattern, 
          // if we only check UAF by HOR, a FP will be reported on ho0v1
          // if (...) {
          //   free(p0);  // ho0v1 freed
          //   p1 = blah;
          // }
          // ho0v2 = phi(ho0v0(chi), ho0v1(free))
          // p2 = phi(p0, p1);  free(p2); // no DBF here
          // return p2;   // no UAF on p2, if free(p2) does not exist
          HEAP_OBJ_REP* var_hor = is_vor ? ((VSYM_OBJ_REP*)var_phi->OPND(i))->Hor()
                                         : vsa->Cr_2_heap_obj(var_phi->OPND(i));
          if (var_hor && !vsa->Hor_same_ulist(var_hor, hor_opnd)) {
            Is_Trace(ctx->Tracing(), (TFile, "%s- Skip: not in same ulist\n", TRACE_INDENT));
            continue;
          }
          if (!is_vor) {
            cr_opnd = var_phi->OPND(i);
          }
        }
        if (!Is_path_possible(phi, i, cur_node)) {
          Is_Trace(ctx->Tracing(), (TFile, "  - Skip: path not possible\n"));
          continue;
        }
        if (hor_opnd->Attr() == ROR_DEF_BY_CHI && cr_opnd == cr) {
          Is_Trace(ctx->Tracing(), (TFile, "  - Skip: hor chi not updated with cr\n"));
          continue;
        }
        if (Kind() == FOR_MSF) {
          HEAP_OBJ_MAP* ho_ids = ctx->Vsa()->Find_ignore_ho_ids(bb_pred);
          HEAP_OBJ_MAP::iterator ho_iter;
          if (ho_ids != NULL &&
              (ho_iter = ho_ids->find(hor_opnd->Heap_obj()->Id())) != ho_ids->end()) {
            if (ho_iter->second == VA_NO) {
              // compared with NULL
              Is_Trace(ctx->Tracing(), (TFile, "  - Skip: hor is in msf ignore ho list\n"));
              continue;
            }
            else {
              // compared with something else, set to M
              Sp_h()->Set_flag(SRCPOS_FLAG_MAYBE);
            }
          }
        }
        Set_cur_node(cur_node, i);

        Is_Trace(ctx->Tracing(), (TFile, "  - Possible:%s\n", Cur_node()->Is_flag_set(SRCPOS_FLAG_MAYBE) ? "maybe" : "yes"));
        Add_bb(bb_pred);
        Sp_h()->Append_data(bb_pred, ctx->Dna(), PATHINFO_BRANCH);

        // if var_phi is NULL, means there is no var redefined from back edge,
        // pass back_edge FALSE to Check_heap_obj
        CHECK_OBJ opnd_obj(cr_opnd, phi, bb_pred);
        Check_heap_obj(hor_opnd, opnd_obj, ctx,
                       back_edge || (def_in_phi && phi_bb->Dominates(bb_pred)));
        Pop_mark(phi, FALSE);
      }
      Reset_cur_node(cur_node, parent_idx);
      Pop_mark(phi, TRUE);
      return CS_DONE;
    }
    break;
  case ROR_DEF_BY_COPY:
    {
      HEAP_OBJ_REP *prev = hor->Prev_ver();
      if (prev == NULL) {
        Is_Trace(ctx->Tracing(), (TFile, "  [%s]: Done. COPY hor no prev:\n", Chkname()));
        Is_Trace_cmd(ctx->Tracing(), hor->Print(TFile));
        Is_Trace(ctx->Tracing(), (TFile, "\n"));
        return CS_DONE;
      }
      return Check_heap_obj(prev, obj, ctx, back_edge);
    }
    break;
  case ROR_DEF_BY_CHI:
    {
      Is_Trace(ctx->Tracing(), (TFile, "  [%s]: hor def-by chi:", Chkname()));
      Is_Trace_cmd(ctx->Tracing(), hor->Print(TFile));
      STMTREP *def = hor->Stmt_def();
      if (!def) {
        Is_Trace(ctx->Tracing(), (TFile, " CS_DONE: by no def\n"));
        return CS_DONE;
      }
      if (Kind() == FOR_DBF && def->Opr() == OPR_MSTORE &&
          def->Rhs()->Kind() == CK_CONST && def->Rhs()->Const_val() == 0) {
        Is_Trace(ctx->Tracing(), (TFile, " CS_DONE: def by MSTORE with 0\n"));
        return CS_DONE;
      }
      CHECKER_STATUS sts = CS_DONE;
      if (obj.Is_vsym()) {
        sts = CS_VSYM_UD;
        Is_True(obj.Vor(), ("vsym is NULL"));
        // update vor on def stmt
        CHI_NODE *chi = vsa->Find_vor_chi(def, obj.Vor()->Vsym_obj());
        Is_True_Ret(chi, ("not find vor chi on defstmt"), CS_DONE);
        CVOR *res = (CVOR*)chi->RESULT();
        obj.Update_vsym(res->first, def, res->second);
      } else {
        // update cr on def stmt
        cr = vsa->Find_hor_chi_cr(def, hor);
        Is_True_Ret(cr, ("not find def_cr"), CS_DONE);
        HEAP_OBJ_REP *cr_hor = vsa->Cr_2_heap_obj(cr);
        Is_True_Ret(cr_hor, ("not find def hor"), CS_DONE);
        if ((cr->Kind() == CK_LDA || def->Opr() == OPR_OPT_CHI) &&
            cr_hor->Heap_obj() != hor->Heap_obj()) {
          // if not same heap_obj, recover the vfrs
          VSYM_OBJ_REP *matched_vor = ctx->Tracker()->Expand(vsa, def, hor, cr_hor, cr);
          Is_True_Ret(matched_vor, ("hor not match with chi cr\n"), CS_DONE);
          sts = CS_VSYM_UD;
          // update vor on def stmt
          CHI_NODE *chi = vsa->Find_vor_chi(def, matched_vor->Vsym_obj());
          Is_True_Ret(chi, ("not find vor chi on defstmt"), CS_DONE);
          CVOR *res = (CVOR*)chi->RESULT();
          obj.Update_vsym(res->first, def, res->second);
        } else {
          Is_Trace(ctx->Tracing(), (TFile, "   switch to cr UD cr%d\n", cr->Coderep_id()));
          obj.Update_var(cr, def);
          switch (cr->Kind()) {
            case CK_VAR:
              sts = CS_VAR_UD;
            break;
            case CK_IVAR:
              sts = CS_IVAR_UD;
            break;
            default:
              Is_Trace(ctx->Tracing(), (TFile, "  [%s]: CS_DONE cr Kind %d not supported:", Chkname(), cr->Kind()));
            break;
          }
        }
      }
      if (sts != CS_DONE) {
        VSYM_TRACKER::VSYM_VECTOR vsym_stack;
        ctx->Tracker()->Save_stack(&vsym_stack);
        UD_TRAVELER<HEAP_CHECKER> heap_chi_helper(*this, *ctx);
        heap_chi_helper.Continue_trav(obj, sts);
        ctx->Tracker()->Restore_stack(&vsym_stack);
      }
      return CS_DONE;
    }
    break;
  case ROR_DEF_BY_LDA:
  case ROR_DEF_BY_ALLOCA:
    {
      STMTREP *def = hor->Stmt_def() ? hor->Stmt_def() : sr;
      if (Kind() == FOR_DBF && VSA_Udr()) {
        Sp_h()->Set_msgid("UDR.1");
        Append_data(def, ctx->Dna(), PATHINFO_ALLOC);
        ISSUE_CERTAINTY ic = VSA_Issue_Certainty_Maybe ? IC_MAYBE : IC_DEFINITELY;
        vsa->Report_vsa_error(cr, (char*)NULL, UDR, ic, Sp_h());
        if (VSA_Xsca) {
          vsa->Report_xsca_error(cr, Sp_h()->Orig_stname(), "MSR_22_2", Sp_h());
        }
      }
      return CS_DONE;
    }
    break;
  case ROR_DEF_BY_ALLOC:
    if (Kind() == FOR_MSF) {
      STMTREP *def = hor->Stmt_def();
      Is_True(def != NULL, ("hor defstmt is NULL"));
      // treat the alloc stmt as the "key" srcpos for the msf issue
      if (_checker->Issue_reported(def, NULL, Kind())) {
        Is_Trace(ctx->Tracing(),
                 (TFile, "--HEAP: MSF on sr%d line %d already reported.\n",
                         def->Stmtrep_id(),
                         Srcpos_To_Line(def->Linenum())));
        return CS_DONE;
      }
      Sp_h()->Set_key_srcpos(ctx->Dna(), def, NULL);
      Append_data(def, ctx->Dna(), PATHINFO_ALLOC);
      ISSUE_CERTAINTY ic = VSA_Issue_Certainty_Maybe ? IC_MAYBE: IC_DEFINITELY;
      vsa->Report_vsa_error(cr, (char *)NULL, MSF, ic, Sp_h());
      if (VSA_Xsca) {
        vsa->Report_xsca_error(cr, Sp_h()->Orig_stname(), "MSR_22_1", Sp_h());
      }
      Sp_h()->Remove_last_key_srcpos();
    }
    break;
  case ROR_DEF_BY_NULL:
    {
      if (VSA_Udr_Null == FALSE) {
        Is_Trace(ctx->Tracing(), (TFile, "  [%s]: CS_DONE VSA_Udr_Null is off\n",
                                  Chkname()));
        return CS_DONE;
      }
      STMTREP *def = cr->Get_defstmt();
      BOOL def_at_entry = FALSE;
      if (!Chk_null() && def) {
        BOOL is_vra = FALSE;
        VAL_RANGE_RESULT rr = ctx->Vsa()->Check_expr_zero(cr, def->Bb(), is_vra);
        if (rr == VAL_OOR) {
          Is_Trace(ctx->Tracing(), (TFile, "  [%s]: CS_DONE Check_null is off\n",
                                    Chkname()));
          return CS_DONE;
        }
      }
      if (def && def->Opr() == OPR_OPT_CHI) {
        def_at_entry = TRUE;
      }
      // EH entry, FOLLOW_EH is off
      if (def_at_entry && def->Bb()->Labnam() > 0) {
        return CS_DONE;
      } else if (Kind() == FOR_DBF && VSA_Udr()) {
        if (def_at_entry) {
          AUX_STAB_ENTRY *sym = ctx->Opt_stab()->Aux_stab_entry(cr->Aux_id());
          ST *st = sym->St();
          if (st) {
            Sp_h()->Append_data(st, def->Bb(), ctx->Dna(), PATHINFO_ST_DECLARE);
          }
        } else if (def && def != sr) {
          Sp_h()->Append_data(def, ctx->Dna(), PATHINFO_COPY);
        }
        Sp_h()->Set_msgid("UDR.1");
        ISSUE_CERTAINTY ic = VSA_Issue_Certainty_Maybe ? IC_MAYBE : IC_DEFINITELY;
        vsa->Report_vsa_error(cr, (char*)NULL, UDR, ic, Sp_h());
        return CS_DONE;
      }
    }
    break;
  default:
    Is_Trace(ctx->Tracing(), (TFile, "  [%s]: CS_DONE with hor attr %d\n", Chkname(), hor->Attr()));
  }

  return CS_DONE;
}

// ====================================================================
// HEAP_CHECKER::Check_coderep<CK_LDA>
//   Check HEAP if coderep is LDA
// ====================================================================
template<> CHECKER_STATUS inline
HEAP_CHECKER::Check_coderep<CK_LDA>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  Is_True(obj.Coderep()->Kind() == CK_LDA, ("not lda"));
  if (Kind() == FOR_DBF && VSA_Udr()) {
    Sp_h()->Set_msgid("UDR.1");
    Append_data(obj.Stmtrep(), ctx->Dna(), PATHINFO_ALLOC);
    ISSUE_CERTAINTY ic = VSA_Issue_Certainty_Maybe ? IC_MAYBE : IC_DEFINITELY;
    ctx->Vsa()->Report_vsa_error(obj.Coderep(), (char*)NULL, UDR, ic, Sp_h());
  }
  else {
    Is_Trace(ctx->Tracing(), (TFile, "  [%s]: CS_DONE: LDA\n", Chkname()));
  }
  return CS_DONE;
}

// ====================================================================
// HEAP_CHECKER::Check_coderep<CK_LDA>
//   Check HEAP if coderep is CONST
// ====================================================================
template<> CHECKER_STATUS inline
HEAP_CHECKER::Check_coderep<CK_CONST>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  Is_True(obj.Coderep()->Kind() == CK_CONST, ("not const"));
  if (Kind() == FOR_DBF && VSA_Udr()) {
    CODEREP *cr = obj.Coderep();
    STMTREP *sr = obj.Stmtrep();
    Sp_h()->Set_msgid("UDR.1");
    if (cr->Const_val() == 0 && (!VSA_Udr_Null || !Chk_null())) {
      Is_Trace(ctx->Tracing(), (TFile, "  [%s]: CS_DONE Check_null is off\n",
                                Chkname()));
      return CS_DONE;
    }
    if (cr != Sp_h()->Root_x()) {
      Append_stpath(sr, cr, ctx->Dna(), FALSE);
    }
    ISSUE_CERTAINTY ic = VSA_Issue_Certainty_Maybe ? IC_MAYBE : IC_DEFINITELY;
    ctx->Vsa()->Report_vsa_error(cr, (char*)NULL, UDR, ic, Sp_h());
    Is_Trace(ctx->Tracing(), (TFile, "  [%s]: Report UDR with const %lld\n",
                              Chkname(), obj.Coderep()->Const_val()));
    return CS_DONE;
  }
  Is_Trace(ctx->Tracing(), (TFile, "  [%s]: CS_DONE with const %lld\n",
                            Chkname(), obj.Coderep()->Const_val()));
  return CS_DONE;
}

// ====================================================================
// HEAP_CHECKER::Check_coderep<CK_LDA>
//   Check HEAP if coderep is RCONST
// ====================================================================
template<> CHECKER_STATUS inline
HEAP_CHECKER::Check_coderep<CK_RCONST>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  Is_True(obj.Coderep()->Kind() == CK_RCONST, ("not rconst"));
  Is_Trace(ctx->Tracing(), (TFile, "  [%s]: CS_DONE with rconst\n", Chkname()));
  return CS_DONE;
}

// ====================================================================
// HEAP_CHECKER::Check_coderep<CK_LDA>
//   Check HEAP if coderep is OP
// ====================================================================
template<> CHECKER_STATUS inline
HEAP_CHECKER::Check_coderep<CK_OP>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  CODEREP *cr = obj.Coderep();
  STMTREP *sr = obj.Stmtrep();
  Is_True(cr->Kind() == CK_OP, ("not var"));
  if (Kind() == FOR_NONE) {
    Is_True(cr->Opr() == OPR_CALL || cr->Opr() == OPR_ICALL, ("not call"));

    VSA* vsa = ctx->Vsa();
    BOOL maybe = FALSE;
    CODEREP* arg = vsa->Callee_frees_heap_memory(sr, &maybe);
    if (arg == NULL || !Is_leaf_free(vsa, sr)) {
      Is_Trace(ctx->Tracing(), (TFile, "  [%s]: Done CK_OP not leaf free\n", Chkname()));
      return CS_DONE;   // not leaf free, only start check for leaf free
    }

    // DBF is off, do nothing
    if (!(VSA_Dbf() && !PU_java_lang(Get_Current_PU())))
      return CS_DONE;

    // if memory maybe freed, set srcpos maybe flag
    if (maybe || vsa->Is_conditional_free(sr))
      Sp_h()->Set_flag(SRCPOS_FLAG_MAYBE);

    // Set kind for DBF
    Set_kind(FOR_DBF);
    obj.Update_var(arg);
    Set_orig_stname(obj, ctx);
    Sp_h()->Set_key_srcpos(ctx->Dna(), sr, NULL);

    BOOL is_vra = FALSE;
    VAL_RANGE_RESULT rr = ctx->Vsa()->Check_expr_zero(arg, sr->Bb(), is_vra);
    if (rr == VAL_INR && is_vra == TRUE) {
      Is_Trace(ctx->Tracing(), (TFile, "  [%s]: Disable check zero, as VRA says not zero\n", Chkname()));
      Set_chk_null(FALSE);
      HEAP_OBJ_REP *arg_hor = vsa->Cr_2_heap_obj(arg);
      if (arg_hor && arg_hor->Attr() == ROR_DEF_BY_NULL) {
        Is_Trace(ctx->Tracing(), (TFile, "  [%s]: CS_DONE: skip null hor if vra says not zero\n", Chkname()));
        return CS_DONE;
      }
    }

    HEAP_OBJ_REP* hor = vsa->Cr_2_heap_obj(cr);
    if (hor == NULL) { // This should be an error.  Why not an internal error
      Is_Trace(ctx->Tracing(), (TFile, "  [%s]: no hor, continue with OP UD\n", Chkname()));
      Is_Trace_cmd(ctx->Tracing(), cr->Print(TFile));
      return CS_OP;
    }

    if (hor == vsa->Null_hor()) {
      // This udr has been reported before
      // This udr has been reported in heap_obj creation phase
      // Or, report UDR here?
      return CS_DONE;
    }

    Is_True(hor->Attr() == ROR_DEF_BY_FREE ||
            hor->Attr() == ROR_DEF_BY_DANGLE ||
            hor->Heap_obj()->Kind() == RSC_KIND_LDA ||
            vsa->Callee_returns_new_heap_memory(sr), ("hor is not freed"));

    hor = vsa->Cr_2_heap_obj_ref(cr);
    //Is_True(hor != NULL, ("no hor for free argument"));
    if (hor == NULL) { // This should be an error, why not an internal error
      Is_Trace(ctx->Tracing(), (TFile, "  [%s]: no hor, continue with OP UD\n", Chkname()));
      Is_Trace_cmd(ctx->Tracing(), arg->Print(TFile));
      return CS_OP;
    }
    return Check_heap_obj(hor, obj, ctx);
  } else if (Kind() == FOR_UAF) {
    CODEREP* base = ctx->Comp_unit()->Analyze_base_info(sr, cr, FALSE);
    if (base != NULL) {
      obj.Update_var(base);
      // set status to CS_OP to trigger callback first before check UD
      return CS_OP;
    }
  }

  Is_Trace(ctx->Tracing(), (TFile, "  [%s]: Done CK_OP\n", Chkname()));
  return CS_DONE;
}

// ====================================================================
// HEAP_CHECKER::Check_coderep<CK_VAR>
//   Check HEAP if coderep is VAR
// ====================================================================
template<> CHECKER_STATUS
HEAP_CHECKER::Check_coderep<CK_VAR>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  CODEREP *cr = obj.Coderep();
  Is_True(cr->Kind() == CK_VAR, ("not var"));

  Is_True(cr->Aux_id() != ctx->Opt_stab()->Default_vsym() &&
          cr->Aux_id() != ctx->Opt_stab()->Return_vsym(),
          ("hit default/return vsym"));
  Is_True(!cr->Is_flag_set(CF_IS_ZERO_VERSION) &&
          !cr->Is_var_volatile(),
          ("hit zero-ver/volatile"));

  STMTREP *sr = obj.Stmtrep();

  VSA* vsa = ctx->Vsa();
  HEAP_OBJ_REP* hor = vsa->Cr_2_heap_obj(cr);
  if (hor == NULL) {
    if (Kind() == FOR_NONE) {
      Is_Trace(ctx->Tracing(), (TFile, "  [%s]: Done no hor\n", Chkname()));
      return CS_DONE;
    }
    if (sr)
      Append_data(sr, ctx->Dna(), PATHINFO_COPY);
    Is_Trace(ctx->Tracing(), (TFile, "  [%s]: no hor, switch to VAR UD cr%d \n",
                              Chkname(), cr->Coderep_id()));
    return CS_VAR_UD;
  }

  if (sr)
    Append_data(sr, ctx->Dna(), PATHINFO_COPY);

  // hor on cr may not be the correct version
  HEAP_OBJ_REP *cur_hor = hor;
  if (sr) {
    if (VSA_New_HVA)
      cur_hor = ctx->Vsa()->Find_stmt_hor_mu(sr, cr);
    else
      cur_hor = ctx->Vsa()->Find_fsm_mu_ror(sr, hor);
    if (cur_hor == NULL) {
      Is_Trace(ctx->Tracing(), (TFile, "  [%s]: Done no hor\n", Chkname()));
      return CS_DONE;
    }
  }

#if 1
  return Check_heap_obj(cur_hor, obj, ctx);
#else
  if (! VSA_Hor_Unification)
    return Check_heap_obj(cur_hor, cr, sr, ctx);
  else {
    HOR_LIST *ul = cur_hor->Ulist();
    if (ul == NULL)
      return Check_heap_obj(cur_hor, cr, sr, ctx);
    else {
      HEAP_OBJ_REP *heap_obj_rep;
      HOR_LIST_ITER hor_list_iter;
      FOR_ALL_ELEM (heap_obj_rep, hor_list_iter, Init(ul)) {
        Check_heap_obj(heap_obj_rep, cr, sr, ctx);
      }
      return CS_DONE;
    }
  }
#endif
}

// ====================================================================
// HEAP_CHECKER::Check_coderep<CK_IVAR>
//   Check HEAP if coderep is IVAR
// ====================================================================
template<> CHECKER_STATUS
HEAP_CHECKER::Check_coderep<CK_IVAR>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  CODEREP *cr = obj.Coderep();
  STMTREP *sr = obj.Stmtrep();
  Is_True(cr->Kind() == CK_IVAR, ("not ivar"));

  if (cr->Opr() == OPR_PARM) {
    obj.Update_var(cr->Ilod_base());
    // if a check is started via annotated dereference, skip for now
    if (Kind() == FOR_NONE)
      return CS_DONE;
    return CS_OP;
  }

  if (Kind() == FOR_NONE) {
    // UAF is off, do nothing
    if (!(VSA_Uaf() && !PU_java_lang(Get_Current_PU())))
      return CS_DONE;
    CODEREP *base = (OPERATOR_is_store(sr->Opr()) && cr == sr->Lhs()) ?
                    cr->Istr_base() : cr->Ilod_base();
    base = Find_ilod_base(base);
    if (base == NULL) {
      Is_Trace(ctx->Tracing(), (TFile, "  [%s]: Done. cannot find base cr\n", Chkname()));
      return CS_DONE;
    }
    HEAP_OBJ_REP *hor = ctx->Vsa()->Cr_2_heap_obj(base);
    if (hor == NULL) {
      Is_Trace(ctx->Tracing(), (TFile, "  [%s]: Done. no base hor\n", Chkname()));
      return CS_DONE;
    }

    HEAP_OBJ_REP *cur_hor;
    if (VSA_New_HVA)
      cur_hor = ctx->Vsa()->Find_stmt_hor_mu(sr, base);
    else
      cur_hor = ctx->Vsa()->Find_fsm_mu_ror(sr, hor);
    if (cur_hor == NULL) {
      Is_Trace(ctx->Tracing(), (TFile, "  [%s]: Done no hor\n", Chkname()));
      return CS_DONE;
    }
    Set_kind(FOR_UAF);
    Set_orig_stname(obj, ctx);
    obj.Update_var(base);
    return Check_heap_obj(cur_hor, obj, ctx);
  }

  if (Kind() != FOR_DBF && Kind() != FOR_UAF) {
    return CS_DONE;  // TODO: MSF?
  }

  return CS_IVAR_UD;
}

// ====================================================================
// HEAP_CHECKER::Check_stmtrep<OPR_INTRINSIC_CALL>
//   Check HEAP if stmtrep is INTRINSIC_CALL
// ====================================================================
template<> CHECKER_STATUS
HEAP_CHECKER::Check_stmtrep<OPR_INTRINSIC_CALL>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  STMTREP *sr = obj.Stmtrep();
  Is_True(sr->Opr() == OPR_INTRINSIC_CALL, ("not intrinsic call"));
  if (Kind() == FOR_MSF) {
    Is_Trace(ctx->Tracing(), (TFile, "  [%s]: Done msf check reach to call chi\n",
                              Chkname()));
    return CS_DONE;
  }

  INTRINSIC intrn = sr->Rhs()->Intrinsic();
  if (intrn == INTRN_CHECK_CAST) {
    CODEREP* rhs = sr->Rhs();
    Is_True(rhs->Kid_count() == 2, ("bad rhs kids"));
    Is_True(rhs->Opnd(1)->Kind() == CK_IVAR && rhs->Opnd(1)->Opr() == OPR_PARM, ("bad first kid"));
    obj.Update_var(rhs->Opnd(1)->Ilod_base());
    return CS_OP;
  }
  // TODO: other intrinsics

  RNA_NODE *rna = ctx->Vsa()->Sr_2_rna(sr);
  RBC_OP get_ops[] = RBC_OP_CONTAINER_GET;
  if (rna->Is_container_op() && 
      ctx->Ipsa()->Rna_has_rbc_ops(rna, get_ops, RBC_OPS_CNT(get_ops)))
    return CS_CONT;
  return CS_DONE;        
}

template<> CHECKER_STATUS
HEAP_CHECKER::Check_stmtrep<OPR_CALL>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  STMTREP *sr = obj.Stmtrep();
  if (Kind() == FOR_MSF) {
    Is_Trace(ctx->Tracing(), (TFile, "  [%s]: Done msf check reach to call chi\n",
                              Chkname()));
    return CS_DONE;
  }
  HEAP_OBJ_REP *hor = obj.Is_var() ? ctx->Vsa()->Find_stmt_hor_chi(sr, obj.Coderep()) : obj.Vor()->Hor();
  if (hor && hor->Attr() == ROR_DEF_BY_FREE) {
    return Check_heap_obj(hor, obj, ctx);
  }
  return CS_CONT;
}

template<> CHECKER_STATUS
HEAP_CHECKER::Check_stmtrep<OPR_ICALL>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  if (Kind() == FOR_MSF) {
    Is_Trace(ctx->Tracing(), (TFile, "  [%s]: Done msf check reach to call chi\n",
                              Chkname()));
    return CS_DONE;
  }
  return CS_CONT;
}

template<> CHECKER_STATUS
HEAP_CHECKER::Check_stmtrep<OPR_OPT_CHI>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  Is_True(obj.Stmtrep()->Opr() == OPR_OPT_CHI, ("Heap checker: not chi stmt"));
  if (Kind() == FOR_MSF) {
    Is_Trace(ctx->Tracing(), (TFile, "  [%s]: Done msf check reach to entry chi\n",
                              Chkname()));
    return CS_DONE;
  }
  // EH entry, FOLLOW_EH is off
  if (obj.Stmtrep()->Bb()->Labnam() > 0) {
    return CS_DONE;
  }
  if (obj.Is_var() && ctx->Tracker()->Empty()) {
    CODEREP *cr = obj.Coderep();
    Is_True(cr->Kind() == CK_VAR, ("Cr kind is not CK_VAR, cr%d kind: %d.", cr->Coderep_id(), cr->Kind()));
    Is_True(cr->Is_flag_set((CR_FLAG) CF_DEF_BY_CHI), ("Cr is not def by chi, cr%d.", cr->Coderep_id()));
    AUX_STAB_ENTRY *sym = ctx->Opt_stab()->Aux_stab_entry(cr->Aux_id());
    ST *st = sym->St();
    if (cr->Def_at_entry() && st != NULL) {
      SRCPOS st_pos = ST_Srcpos(*st);
      if(st_pos == 0) {
        st_pos = ctx->Comp_unit()->Cfg()->Entry_spos();
      }
      switch (ST_sclass(st)) {
        // just handle SCLASS_AUTO for now
        case SCLASS_AUTO: {
          BOOL ignore = (st == NULL) ? FALSE : Vsa_check_sym_ignore(ST_name(st));
          if (ignore) {
            Is_Trace(ctx->Tracing(), (TFile, "  [%s]: Done with ignored symbol, symbol name: %s.\n",
                                      Chkname(), ST_name(st)));
            return CS_DONE;
          }
          Sp_h()->Append_data(st, obj.Stmtrep()->Bb(), ctx->Dna(), PATHINFO_ST_DECLARE);
          Is_Trace(ctx->Tracing(), (TFile, "  [%s]: Report UDR with class auto cr%d\n",
                                    Chkname(), cr->Coderep_id()));
          if (Kind() == FOR_DBF && VSA_Udr()) {
            Sp_h()->Set_msgid("UDR.1");
            const char *name = Sp_h()->Find_cr_stname(cr, obj.Stmtrep(), ctx->Dna());
            Sp_h()->Set_key_srcpos(ctx->Dna(), NULL, st_pos, name);
            ISSUE_CERTAINTY ic = VSA_Issue_Certainty_Maybe ? IC_MAYBE : IC_DEFINITELY;
            ctx->Vsa()->Report_vsa_error(cr, (char*)NULL, UDR, ic, Sp_h());
            Sp_h()->Remove_last_key_srcpos();
            return CS_DONE;
          }
          return CS_DONE;
        }
        default:
          break;
      }
    }
  }
  return CS_CONT;
}


template<> CHECKER_STATUS
HEAP_CHECKER::Check_stmtrep<OPR_RETURN>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  if (Kind() != FOR_MSF) {
    return CS_DONE;
  }

  if (!VSA_Msf() || PU_java_lang(Get_Current_PU())) {
    return CS_DONE;
  }
  STMTREP *sr = obj.Stmtrep();
  Is_True(sr != NULL, ("obj sr is NULL"));
  typedef hash_map<IDTYPE, BOOL> ESCAPED_HO_MAP;
  ESCAPED_HO_MAP escaped_hos;  // pair of <ho_id, maybe>
  OPT_STAB *opt_stab = ctx->Comp_unit()->Opt_stab();

  // add escaped hor from vor mu
  MU_LIST *vor_mu_list = ctx->Vsa()->Stmt_vor_mu(sr);
  if (vor_mu_list != NULL && !vor_mu_list->Is_Empty()) {
    MU_NODE *vor_mu;
    MU_LIST_ITER vor_mu_iter;
    FOR_ALL_NODE(vor_mu, vor_mu_iter, Init(vor_mu_list)) {
      CVOR *cvor = (CVOR*)vor_mu->OPND();
      VSYM_OBJ_REP *vor = cvor->first;
      Is_Trace(ctx->Tracing(), (TFile, "  [%s]: add escape by return vo%dv%d mu base_hor:", Chkname(), vor->Vsym_obj()->Id(), vor->Version()));
      Is_Trace_cmdn(ctx->Tracing(), vor->Vsym_obj()->Base_hor()->Print_detail(TFile), TFile);
      escaped_hos[vor->Vsym_obj()->Base_hor()->Heap_obj()->Id()] = FALSE;
      if (vor->Hor()) {
        Is_Trace(ctx->Tracing(), (TFile, "  [%s]: add escape by return vor%d mu rhs hor:", Chkname(), vor->Vsym_obj()->Id()));
        Is_Trace_cmdn(ctx->Tracing(), vor->Hor()->Print_detail(TFile), TFile);
        escaped_hos[vor->Hor()->Heap_obj()->Id()] = FALSE;
      }
      hash_set<uintptr_t> visited_vors;
      Add_vor_escaped_hos(vor, escaped_hos, visited_vors, ctx);
    }
  }

  // add escaped hor from return value
  STMTREP* prev = sr->Prev();
  while (prev != NULL && OPERATOR_is_scalar_store(prev->Opr()) &&
         opt_stab->Aux_stab_entry(prev->Lhs()->Aux_id())->Is_dedicated_preg()) {
    HEAP_OBJ_REP *hor = ctx->Vsa()->Find_cr_heap_obj(prev->Lhs());
    if (hor != NULL) {
      Is_Trace(ctx->Tracing(), (TFile, "  [%s]: add escape by return cr%d hor:",
                                Chkname(), prev->Lhs()->Coderep_id()));
      Is_Trace_cmdn(ctx->Tracing(), hor->Print_detail(TFile), TFile);
      escaped_hos[hor->Heap_obj()->Id()] = FALSE;
    }
    prev = prev->Prev();
  }

  // add escaped hor from global cr
  MU_NODE *mu;
  MU_LIST_ITER mu_iter;
  FOR_ALL_NODE(mu, mu_iter, Init(sr->Mu_list())) {
    if (mu->Aux_id() == opt_stab->Default_vsym() ||
        mu->Aux_id() == opt_stab->Return_vsym())
      continue;
    CODEREP *opnd = mu->OPND();
    if (opnd && opnd->Kind() == CK_VAR) {
      AUX_STAB_ENTRY *sym = opt_stab->Aux_stab_entry(opnd->Aux_id());
      if (sym->Is_global()) {
        HEAP_OBJ_REP *hor = ctx->Vsa()->Find_stmt_hor_mu(sr, opnd);
        if (hor) {
          Is_Trace(ctx->Tracing(), (TFile, "  [%s]: add escape by maybe for global hor: ", Chkname()));
          Is_Trace_cmdn(ctx->Tracing(), hor->Print_detail(TFile), TFile);
          escaped_hos[hor->Heap_obj()->Id()] = TRUE;
        }
        else if (TY_kind(sym->Ty()) == KIND_POINTER) {
          hash_set<uintptr_t> visited_crs;
          Add_cr_escaped_hos(opnd, escaped_hos, visited_crs, ctx);
        }
      }
    }
  }

  // add ulist to escaped hos; add address passed ho to escaped
  hash_map<IDTYPE, HEAP_OBJ_REP*> alias_escaped;
  MU_LIST *hor_mu_list = ctx->Vsa()->Stmt_hor_mu(sr);
  if (hor_mu_list != NULL && !hor_mu_list->Is_Empty()) {
    MU_NODE *mnode;
    MU_LIST_ITER mu_iter;
    FOR_ALL_NODE(mnode, mu_iter, Init(hor_mu_list)) {
      CHOR *chor = (CHOR*)mnode->OPND();
      HEAP_OBJ_REP* hor = chor->first;
      if (hor->Heap_obj()->Flag() & (RSC_ADDR_PASSED_OUT|RSC_ADDR_PASSED_IN)) {
        // if turn on VSA_MSF_Addr_Pass, report Maybe
        Is_Trace(ctx->Tracing(), (TFile, "  [%s]: add escape by addr passed in/out: ", Chkname()));
        Is_Trace_cmdn(ctx->Tracing(), hor->Print_detail(TFile), TFile);
        escaped_hos[hor->Heap_obj()->Id()] = VSA_MSF_Addr_Pass ? TRUE : FALSE;
      }
      if (hor->Ulist() && hor->Is_phi()) {
        ESCAPED_HO_MAP::iterator escaped_it = escaped_hos.find(hor->Heap_obj()->Id());
        if (escaped_it != escaped_hos.end()) {
          HEAP_OBJ_REP *heap_obj_rep;
          HOR_LIST_ITER hor_list_iter;
          Is_Trace(ctx->Tracing(), (TFile, "  [%s]: add alias escaped mapping for hor:", Chkname()));
          Is_Trace_cmd(ctx->Tracing(), hor->Print(TFile));
          Is_Trace(ctx->Tracing(), (TFile, " <- "));
          FOR_ALL_ELEM (heap_obj_rep, hor_list_iter, Init(hor->Ulist())) {
            Is_Trace_cmd(ctx->Tracing(), heap_obj_rep->Print(TFile));
            alias_escaped[heap_obj_rep->Heap_obj()->Id()] = hor;
            Is_Trace(ctx->Tracing(), (TFile, "|"));
          }
          Is_Trace(ctx->Tracing(), (TFile, "\n"));
        }
      }
    }
  }

  if (hor_mu_list != NULL && !hor_mu_list->Is_Empty()) {
    MU_NODE *hor_mu;
    MU_LIST_ITER hor_mu_iter;
    // Note: the ignore_ho_ids are stored in VSA local pool, which will be poped
    // after each function's heap checker, when msf checker support cross function
    // the lifecycle should be changed
    HEAP_OBJ_MAP* ho_ids = ctx->Vsa()->Find_ignore_ho_ids(sr->Bb());
    FOR_ALL_NODE(hor_mu, hor_mu_iter, Init(hor_mu_list)) {
      CHOR *chor = (CHOR*)hor_mu->OPND();
      HEAP_OBJ_REP *hor = chor->first;
      Is_Trace(ctx->Tracing(), (TFile, "\n##MSF: Find candidates for "));
      Is_Trace_cmdn(ctx->Tracing(), hor->Print(TFile), TFile);
      if (hor->Is_entry_chi()) {
        Is_Trace(ctx->Tracing(), (TFile, "  [%s]: skip entry chi hor:", Chkname()));
        Is_Trace_cmdn(ctx->Tracing(), hor->Print(TFile), TFile);
        continue;
      }
      if (hor->Attr() == ROR_DEF_BY_VARPHI || hor->Attr() == ROR_DEF_BY_VORPHI) {
        Is_Trace(ctx->Tracing(), (TFile, "  [%s]: skip var/vor phi hor:", Chkname()));
        Is_Trace_cmdn(ctx->Tracing(), hor->Print(TFile), TFile);
        continue;
      }

      BOOL maybe = FALSE;
      HEAP_OBJ_MAP::iterator ho_iter;
      if (ho_ids != NULL &&
          (ho_iter = ho_ids->find(hor->Heap_obj()->Id())) != ho_ids->end()) {
        if (ho_iter->second == VA_NO) {
          // compared with NULL
          Is_Trace(ctx->Tracing(), (TFile, "  [%s]: skip hor in ignore ho list:", Chkname()));
          Is_Trace_cmdn(ctx->Tracing(), hor->Print(TFile), TFile);
          continue;
        }
        else {
          // compared with something else, set to M
          maybe = TRUE;
        }
      }

      ESCAPED_HO_MAP::iterator escaped_it = escaped_hos.find(hor->Heap_obj()->Id());
      if (escaped_it != escaped_hos.end()) {
        if (escaped_it->second) {
          maybe = TRUE;
        }
        if (!maybe || !VSA_Maybe_Report) {
          Is_Trace(ctx->Tracing(), (TFile, "  [%s]: skip hor in escaped ho list: ", Chkname()));
          Is_Trace_cmdn(ctx->Tracing(), hor->Print(TFile), TFile);
          continue;
        }
      }

      if (maybe == FALSE && hor->Escaped()) {
        Is_Trace(ctx->Tracing(), (TFile, "  [%s]: set maybe TRUE because hor escaped: ", Chkname()));
        Is_Trace_cmdn(ctx->Tracing(), hor->Print(TFile), TFile);
        maybe = TRUE;
      }

      // start a new checker as the context(visited bb/frame) should not be shared 
      // with different hor
      CODEREP *cr = chor->second;
      CHECK_OBJ hor_obj(cr, sr);
      TRAV_CONTEXT hor_ctx(ctx->Comp_unit(), sr, cr);
      HEAP_CHECKER msf_checker(hor_ctx, _checker);
      msf_checker.Set_orig_stname(hor_obj, &hor_ctx, TRUE);
      msf_checker.Set_kind(FOR_MSF);
      if (maybe) {
        msf_checker.Sp_h()->Set_flag(SRCPOS_FLAG_MAYBE);
      }
      // append a fake exit of the function except the following 2 cases:
      // 1. if the end of function srcpos is 0
      // 2. if the return is after a noreturn call
      if (hor_ctx.Comp_unit()->End_srcpos()) {
        if (prev == NULL || prev->Opr() != OPR_CALL ||
            !PU_has_attr_noreturn((*Pu_Table_ptr)[ST_pu(prev->St())])) {
          msf_checker.Sp_h()->Root()->Remove_last();
          msf_checker.Sp_h()->Append_data(hor_ctx.Comp_unit()->End_srcpos(), NULL,
                                          hor_ctx.Dna(), PATHINFO_VUL_SPOT_SO);
          msf_checker.Sp_h()->Append_data(sr, hor_ctx.Comp_unit()->Dna(), PATHINFO_FUN_EXIT);
        }
      }
      hash_map<IDTYPE, HEAP_OBJ_REP*>::iterator alias_it = alias_escaped.find(hor->Heap_obj()->Id());
      if (alias_it != alias_escaped.end()) {
        msf_checker.Check_alias(hor, alias_it->second, hor_obj, &hor_ctx, FALSE);
      } else {
        msf_checker.Check_heap_obj(hor, hor_obj, &hor_ctx);
      }
    }
  }
  return CS_DONE;
}

CHECKER_STATUS
HEAP_CHECKER::Check_vsym_obj(CHECK_OBJ &obj, TRAV_CONTEXT *ctx)
{
  Is_True_Ret(obj.Is_vsym(), ("check_obj is not vsym"), CS_DONE);
  if (Kind() == FOR_MSF && obj.Vor()->Attr() == ROR_DEF_BY_CHI) {
    Is_Trace(ctx->Tracing(), (TFile, "  [%s]: Done msf check reach to entry chi\n",
                              Chkname()));
    return CS_DONE;
  }
  return CS_CONT;
}
// ====================================================================
// VSA::Check_heap_new
//   Test driver for HEAP checker
// ====================================================================
void
VSA::Check_heap_new()
{
  Is_Trace(Get_Trace(TP_CHECKER, CHK_HEAP_TRACE_FLAG),
           (TFile, "%sScan HEAP NEW for func %s\n%s", DBar, Dna()->Fname(), DBar));
  CHECKER_TRAVELER<HEAP_CHECKER> traveler(Comp_unit());
  traveler.Process();
  Is_Trace(Get_Trace(TP_CHECKER, CHK_HEAP_TRACE_FLAG),
           (TFile, "%sEND Scan HEAP NEW for func %s\n%s", DBar, Dna()->Fname(), DBar));
}

// ====================================================================
// VSA::Check_uaf
//   Check uaf given cr/sr
// ====================================================================
void
VSA::Check_uaf(CODEREP *cr, STMTREP *sr)
{
  Is_Trace(Get_Trace(TP_CHECKER, CHK_HEAP_TRACE_FLAG),
           (TFile, "%sScan HEAP UAF for cr%d sr%d \n%s",
            DBar, cr->Coderep_id(), sr->Stmtrep_id(), DBar));

  CHECKER_TRAVELER<HEAP_CHECKER> traveler(Comp_unit());
  traveler.Start_check(cr, sr, CS_DEREFERENCE);

  Is_Trace(Get_Trace(TP_CHECKER, CHK_HEAP_TRACE_FLAG),
           (TFile, "%sEND Scan HEAP UAF for cr%d\n%s",
            DBar,cr->Coderep_id(), DBar));
}
