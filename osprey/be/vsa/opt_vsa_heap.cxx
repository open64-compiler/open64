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

#include "defs.h"
#include "opt_bb.h"
#include "bb_node_set.h"
#include "opt_cfg.h"
#include "opt_ssa.h"
#include "opt_mu_chi.h"
#include "opt_htable.h"
#include "opt_main.h"
#include "opt_vsa.h"
#include "opt_vra.h"
#include "opt_addr_util.h"
#include "opt_vsa_util.h"
#include "config_vsa.h"
#include "cxx_memory.h"
#include "report.h"
#include "erbe.h"
#include "builtin_rule_defs.h"
#include "intrn_info.h"
#include "pro_core.h"
#include "vsa_annot.h"
#include <ext/hash_set>
using __gnu_cxx::hash_set;

const char* ROR_KIND_NAME(ROR_ATTR attr)
{  
  static const char* name[ROR_DEF_BY_LAST + 1] = {
    // must sync with ROR_ATTR defined in opt_vsa_rsc.h
    "none", "phi", "chi", "istore", "iparm", "auto", "alloc", "free",
    "copy", "lda", "null", "transit", "dangle", "alloca", "varphi", "vorphi",
    "horphi",
  };
  Is_True((attr&ROR_DEF_ATTRS) >= ROR_DEF_BY_NONE &&
          (attr&ROR_DEF_ATTRS) <= ROR_DEF_BY_LAST,
          ("invalid attr"));
  return name[(attr&ROR_DEF_ATTRS)];
}


// ====================================================================
//
// HEAP_OBJ_REP::Print
//
// ====================================================================
void
HEAP_OBJ_REP::Print_detail(FILE* fp) const
{
  fprintf(fp, "ho%dv%d(%s%s%s,0x%x)", (_heap_obj)? _heap_obj->Id() : -1,
          _version, ROR_KIND_NAME(_attr),
          _escaped ? ",escaped" : "", _fld_any ? ",any" : "",
          _attr&ROR_ASGN_ATTRS);
  if (_prev_ver != 0) {
    fprintf(fp, "<-ho%dv%d(%s%s)",
            _prev_ver->_heap_obj->Id(), _prev_ver->_version,
            ROR_KIND_NAME(_prev_ver->_attr),
            _prev_ver->_escaped ? ",escaped" : "");
  }
  if (Injured()) {
    fprintf(fp, ",injured");
  }
  if (Heap_obj()->Sym_id() != INVALID_ID) {
    fprintf(fp, ":sym_id=%d", Heap_obj()->Sym_id());
  }
  TOR_LIST_OLD *tor_list = Tor_list();
  if (tor_list) {
    fprintf(fp, ":");
    tor_list->Print(fp);
  }

  FIELD_OBJ_REP* fl = Flist();
  if (fl != NULL) {
    fprintf(fp, " <FIELD>{");
    do {
      if (fl != Flist())
        fprintf(fp, ", ");
      fl->Print(fp);
      fl = fl->Next();
    } while (fl != NULL);
    fprintf(fp, "}");
  }

  HOR_LIST* ul = Ulist();
  if (ul != NULL) {
    HEAP_OBJ_REP *heap_obj_rep;
    HOR_LIST_ITER hor_list_iter;
    fprintf(fp, " <ULIST>{ ");
    FOR_ALL_ELEM (heap_obj_rep, hor_list_iter, Init(ul)) {
      fprintf(fp, "ho%dv%d(%s) ",
                  heap_obj_rep->Heap_obj()->Id(), heap_obj_rep->Version(),
                  ROR_KIND_NAME(heap_obj_rep->Attr()));
    }
    fprintf(fp, "}");
  }
  fprintf(fp, "\n");
}

void
HEAP_OBJ_REP::Print() const
{
  Print_detail(stdout);
}

// ====================================================================
// VSYM_OBJ_REP::Print(FILE *fp)
// ====================================================================
void
VSYM_OBJ_REP::Print_detail(FILE *fp) const
{
  fprintf(fp, "vo%dv%d:", Vsym_obj()->Id(), (mINT16) _version);
  fprintf(fp, "{");
  Vsym_obj()->Fld_rep().Print(fp);
  if (VSA_Enable_TAG_OLD) {
    TOR_LIST_OLD *tor_list = Tor_list<TOR_LIST_OLD>();
    if (tor_list) {
      fprintf(fp, ":");
      tor_list->Print(fp);
    }
  } else {
    TOR_LIST *tor_list2 = Tor_list<TOR_LIST>();
    if (tor_list2) {
      fprintf(fp, ":");
      tor_list2->Print(fp);
    }
  }
  HEAP_OBJ_REP* base_hor = Vsym_obj()->Base_hor();
  if (base_hor)
    fprintf(fp, " base_hor(ho%dv%d)", base_hor->Heap_obj()->Id(), base_hor->Version());
  VSYM_OBJ* based_on = Vsym_obj()->Based_on();
  if (based_on) {
    fprintf(fp, " based_on(vo%d:", based_on->Id());
    based_on->Fld_rep().Print(fp);
    fprintf(fp, ")");
  }
  HEAP_OBJ_REP* rhs_hor = Hor();
  if (rhs_hor)
    fprintf(fp, " rhs_hor(ho%dv%d)", rhs_hor->Heap_obj()->Id(), rhs_hor->Version());
  if (Vsa_annot()) {
    fprintf(fp, " ");
    VANT_UTIL::Dump(fp, Vsa_annot());
  }
  fprintf(fp, "}");
}

void
VSYM_OBJ_REP::Print() const
{
  Print_detail(stdout);
  printf("\n");
}

void
VSA::Print_vo(VSYM_OBJ* vo, FILE *fp) const
{
  vo->Print(fp);
  CODEREP *cr = vo->Base_hor()->Heap_obj()->Ho_cr();
  fprintf(fp, "  { cr%d ho%dv%d ",
              cr ? cr->Coderep_id() : 0,
              vo->Base_hor()->Heap_obj()->Id(), vo->Base_hor()->Version());
  vo->Fld_rep().Print(fp);
  fprintf(fp, " }");
  while (cr != NULL) {
    if (cr->Kind() == CK_IVAR) {
      VSYM_OBJ_REP *vor = Cr_2_vor(cr);
      if (vor) {
        HEAP_OBJ_REP *hor = vor->Vsym_obj()->Base_hor();
        Is_True(hor != NULL, ("not find base hor"));
        cr = hor->Heap_obj()->Ho_cr();
        fprintf(fp, " <- { cr%d ho%dv%d ",
                    cr ? cr->Coderep_id() : 0,
                    hor->Heap_obj()->Id(), hor->Version());
        vor->Vsym_obj()->Fld_rep().Print(fp);
        fprintf(fp, " }");
      }
      else {
        fprintf(fp, " <- { ILOAD no vor }");
        break;
      }
    }
    else if (cr->Kind() == CK_VAR) {
      IDTYPE param = Dna()->Is_param(cr);
      if (param != INVALID_VAR_IDX) {
        fprintf(fp, " <- { LDID sym%dv%d:param_%d }",
                    cr->Aux_id(), cr->Version(), param);
      }
      else {
        ST *st = Opt_stab()->Aux_stab_entry(cr->Aux_id())->St();
        fprintf(fp, " <- { LDID sym%dv%d%s%s }",
                    cr->Aux_id(), cr->Version(),
                    st ? ":" : "",
                    st ? ST_name(st) : "");
      }
      break;
    }
    else if (cr->Kind() == CK_LDA) {
      fprintf(fp, " <- { LDA sym%d:%s }",
              cr->Lda_aux_id(), ST_name(cr->Lda_base_st()));
      break;
    }
    else {
      Is_True(FALSE, ("ho cr is not lda/var/ivar"));
      break;
    }
  }
  fprintf(fp, "\n");
}

// ====================================================================
//
// VSA::Print_chor
//
// ====================================================================
void
VSA::Print_chor(CHOR* chor, FILE* fp) const
{
  if (chor->second != NULL)
    fprintf(fp, "cr%d:", chor->second->Coderep_id());
  else
    fprintf(fp, "<nil>:");
  chor->first->Print(fp);
}

// ====================================================================
//
// VSA::Print_cvor
//
// ====================================================================
void
VSA::Print_cvor(CVOR* cvor, FILE* fp) const
{
  if (cvor->second != NULL)
    fprintf(fp, "cr%d:", cvor->second->Coderep_id());
  else
    fprintf(fp, "<nil>:");
  cvor->first->Print(fp);

  DEF_OBJS *value_objs = (DEF_OBJS *)Vor_2_value_objs(cvor->first);
  if (value_objs) {
    value_objs->Print(fp);
  }
}

void
VSA::Print_vor_phi(PHI_NODE* phi, FILE *fp) const
{
  Print_rscor_phi(phi, (VSYM_OBJ_REP *)phi->RESULT(), fp);
}

void
VSA::Print_hor_phi(PHI_NODE* phi, FILE* fp, BOOL bb_header) const
{
  BB_NODE *bb = phi->Bb();
  if (bb_header) {
    fprintf(fp, "---- BB%d ", bb->Id());
    BB_NODE *kid;
    BB_LIST_ITER kid_iter;
    // BB preds & succs
    fprintf(fp, " Pred: <");
    FOR_ALL_ELEM(kid, kid_iter, Init(bb->Pred())) {
      fprintf(fp, "BB%d ", kid->Id());
    }
    fprintf(fp, "> Succ: <");
    FOR_ALL_ELEM(kid, kid_iter, Init(bb->Succ())) {
      fprintf(fp, "BB%d ", kid->Id());
    }
    fprintf(fp, ">  ");
  }
  Print_rscor_phi(phi, (HEAP_OBJ_REP*)phi->RESULT(), fp);
}

char *
VSA::Get_st_name_from_cr(CODEREP *cr, BOOL demangle) const
{
  const char *st_name = "";
  if (cr->Kind() == CK_VAR) {
    AUX_STAB_ENTRY *sym = Opt_stab()->Aux_stab_entry(cr->Aux_id());
    st_name = Vsa_sym_name(sym);
  } else if (cr->Kind() == CK_IVAR) {
    CODEREP *base = cr->Ilod_base() ? cr->Ilod_base() : cr->Istr_base();
    UINT fld_id = cr->I_field_id();
    TY_IDX ilod_ty = cr->Ilod_ty();
    if(Is_Structure_Type(ilod_ty) && fld_id > 0 && TY_kind(ilod_ty) != KIND_SCALAR) {
      if (TY_kind(ilod_ty) == KIND_POINTER)
        ilod_ty = TY_pointed(ilod_ty);
      if (TY_kind(ilod_ty) == KIND_STRUCT) {
        st_name = Get_fld_name(ilod_ty, fld_id);
      }
    }
  } else if (cr->Kind() == CK_LDA) {
    ST *st = cr->Lda_base_st();
    if (cr->Is_flag_set(CF_LDA_LABEL)) {
      st_name = "lda_label";
    }
    else if (ST_class(st) == CLASS_CONST &&
        TCON_ty(ST_tcon_val(st)) == MTYPE_STR) {
      st_name = Index_to_char_array(TCON_str_idx(ST_tcon_val(st)));
    }
    else {
      AUX_STAB_ENTRY *sym = Opt_stab()->Aux_stab_entry(cr->Lda_aux_id());
      st_name = Vsa_sym_name(sym);
    }
  }
  char *ret_st_name;
  if (demangle)
   ret_st_name = Vsa_demangle(st_name);
  else
    ret_st_name = strdup(st_name);
  // trim right side of symbol, may contains new line character
  for (INT i = strlen(ret_st_name) - 1; i >=0; i--) {
    if (isspace(ret_st_name[i])) {
      ret_st_name[i] = '\0';
    } else if (ret_st_name[i] == 'n' && i > 0 && ret_st_name[i-1] == '\\') {
      i--;
      ret_st_name[i] = '\n';
    } else {
      break;
    }
  }
  return ret_st_name;
}

void
VSA::Print_cr(CODEREP* cr, INT indent, FILE* fp) const
{
  if (cr->Kind() == CK_OP) {
    for (INT i = 0; i < cr->Kid_count(); ++i)
      Print_cr(cr->Get_opnd(i), indent + 1, fp);
  }
  else if (cr->Kind() == CK_IVAR) {
    CODEREP *base = cr->Ilod_base() ? cr->Ilod_base() : cr->Istr_base();
    Print_cr(base, indent + 1, fp);
  }
  cr->Print_node(indent, fp);
  HEAP_OBJ_REP* hor = Cr_2_heap_obj(cr);
  if (hor != NULL) {
    fprintf(fp, " ");
    hor->Print(fp);
  }
  VSYM_OBJ_REP* vor = Cr_2_vor(cr);
  if (vor) {
    fprintf(fp, " ");
    vor->Print(fp);
  }
  FOR_ARRAY* for_array = Cr_2_for_array(cr);
  if (for_array) {
    for (INT i = 0; i < for_array->size(); i++) {
      FSM_OBJ_REP* forep = (*for_array)[i];
      if (forep) {
        fprintf(fp, " ");
        forep->Print(fp);
      }
    }
  }

  if (VSA_Enable_TAG_OLD) {
    TOR_LIST_OLD *tor_list = Cr_2_tor_list<TOR_LIST_OLD>(cr);
    if (tor_list) {
      fprintf(fp, " ");
      tor_list->Print(fp);
    }
  } else {
    TOR_LIST *tor_list2 = Cr_2_tor_list<TOR_LIST>(cr);
    if (tor_list2) {
      fprintf(fp, " ");
      tor_list2->Print(fp);
    }
  }
  if (cr->Kind() == CK_IVAR &&
      cr->Ivar_mu_node() &&
      cr->Ivar_mu_node()->OPND()) {
    fprintf(fp, " mu<");
    CODEREP *opnd = cr->Ivar_mu_node()->OPND();
    fprintf(fp, "%d/cr%d", cr->Ivar_mu_node()->Aux_id(), opnd->Coderep_id());
    HEAP_OBJ_REP* hor = Cr_2_heap_obj(opnd);
    if (hor) {
      fprintf(fp, ":");
      hor->Print(fp);
    }
    VSYM_OBJ_REP* vor = Cr_2_vor(opnd);
    if (vor) {
      fprintf(fp, ":");
      vor->Print(fp);
    }
    for_array = Cr_2_for_array(opnd);
    if (for_array) {
      for (INT i = 0; i < for_array->size(); i++) {
        FSM_OBJ_REP* forep = (*for_array)[i];
        if (forep) {
          fprintf(fp, " ");
          forep->Print(fp);
        }
      }
    }
    fprintf(fp, ">");
  } // if (stmt->Has_mu())
  if (cr->Kind() == CK_VAR || cr->Kind() == CK_IVAR || cr->Kind() == CK_LDA) {
    char *st_name = Get_st_name_from_cr(cr);
    if (st_name) {
      if (st_name[0] != '\0')
        fprintf(fp, " #%s", st_name);
      free(st_name);
    }
  }
  if (cr->Kind() == CK_VAR && cr->Vsa_annot()) {
    fprintf(fp, " flags: ");
    VANT_UTIL::Dump(fp, cr->Vsa_annot());
  }
  fprintf(fp, "\n");
}

void
VSA::Print_sr(STMTREP *stmt, FILE *fp) const
{
  // var mu list
  if (stmt->Has_mu() && stmt->Mu_list() && !stmt->Mu_list()->Is_Empty()) {
    fprintf(fp, " mu<");
    MU_NODE *mnode;
    MU_LIST_ITER mu_iter;
    CODEREP *opnd;
    BOOL comma = FALSE;
    FOR_ALL_NODE(mnode, mu_iter, Init(stmt->Mu_list())) {
      if (comma)
        fprintf(fp, ", ");
      else
        comma = TRUE;
      opnd = mnode->OPND();
      if (opnd != NULL) {
        fprintf(fp, "%d/cr%d", mnode->Aux_id(), mnode->OPND()->Coderep_id());
        HEAP_OBJ_REP* hor = Cr_2_heap_obj(opnd);
        if (hor) {
          fprintf(fp, ":");
          hor->Print(fp);
        }
        VSYM_OBJ_REP* vor = Cr_2_vor(opnd);
        if (vor) {
          fprintf(fp, ":");
          vor->Print(fp);
        }
        FOR_ARRAY* for_array = Cr_2_for_array(opnd);
        if (for_array) {
          for (INT i = 0; i < for_array->size(); i++) {
            FSM_OBJ_REP* forep = (*for_array)[i];
            if (forep) {
              fprintf(fp, " ");
              forep->Print(fp);
            }
          }
        }
      }
    }
    fprintf(fp, ">\n");
  }

  // hor mu list
  if (Stmt_hor_mu(stmt) && !Stmt_hor_mu(stmt)->Is_Empty()) {
    fprintf(fp, " hor_mu<");
    MU_NODE *mnode;
    MU_LIST_ITER mu_iter;
    CHOR *hor;
    BOOL comma = FALSE;
    FOR_ALL_NODE(mnode, mu_iter, Init(Stmt_hor_mu(stmt))) {
      if (comma)
        fprintf(fp, ", ");
      else
        comma = TRUE;
      hor = (CHOR*)mnode->OPND();
      Print_chor(hor, fp);
    }
    fprintf(fp, ">\n");
  }
  // vor mu list
  if (Stmt_vor_mu(stmt) && !Stmt_vor_mu(stmt)->Is_Empty()) {
    fprintf(fp, " vor_mu<");
    MU_NODE *mnode;
    MU_LIST_ITER mu_iter;
    CVOR *vor;
    BOOL comma = FALSE;
    FOR_ALL_NODE(mnode, mu_iter, Init(Stmt_vor_mu(stmt))) {
      if (comma) 
        fprintf(fp, ",\n        ");
      else
        comma = TRUE;
      vor = (CVOR*)mnode->OPND();
      Print_cvor(vor, fp);
    }
    fprintf(fp, ">\n");
  }
  // hor mu list
  if (stmt->Opr() == OPR_RETURN || stmt->Opr() == OPR_RETURN_VAL) {
    HOR_ARRAY *array = _ret_2_hor_array_map.Lookup(stmt->Bb()->Id());
    if (array != NULL && array->size() > 0) {
      fprintf(fp, " ret_hor_mu<");
      for (INT i = 0; i < array->size(); ++i) {
        if (i != 0)
          fprintf(fp, ", ");
        (*array)[i]->Print(fp);
      }
      fprintf(fp, ">\n");
    }
  }
  // fsm mu list
  if (Stmt_fsm_mu(stmt) && !Stmt_fsm_mu(stmt)->Is_Empty()) {
    fprintf(fp, " fsm_mu<");
    MU_NODE *mnode;
    MU_LIST_ITER mu_iter;
    FSM_OBJ_REP *forep;
    BOOL comma = FALSE;
    FOR_ALL_NODE(mnode, mu_iter, Init(Stmt_fsm_mu(stmt))) {
      if (comma)
        fprintf(fp, ", ");
      else
        comma = TRUE;
      forep = (FSM_OBJ_REP *) mnode->OPND();
      if (forep->Rsc_obj()->Kind() == RSC_KIND_FSM) {
        forep->Print(fp);
      } else {
        HEAP_OBJ_REP *hor = (HEAP_OBJ_REP *) forep;
        hor->Print(fp);
      }
    }
    fprintf(fp, ">\n");
  }
  // print sr
  if (stmt->Rhs())
    Print_cr(stmt->Rhs(), 1, fp);
  OPERATOR opr = stmt->Opr();
  if (opr == OPR_ISTORE || opr == OPR_ISTOREX || opr == OPR_MSTORE)
    Print_cr(stmt->Lhs()->Istr_base(), 1, fp);
  if (opr == OPR_MSTORE)
    Print_cr(stmt->Lhs()->Mstore_size(), 1, fp);
  else if (opr == OPR_ISTOREX)
    Print_cr(stmt->Lhs()->Index(), 1, fp);
  stmt->Print_node(fp);
  fprintf(fp, " id=%d", stmt->Stmtrep_id());
  HEAP_OBJ_REP* lhor = stmt->Lhs() ? Cr_2_heap_obj(stmt->Lhs()) : NULL;
  if (lhor) {
    fprintf(fp, " ");
    lhor->Print(fp);
  }
  VSYM_OBJ_REP* lvor = stmt->Lhs() ? Cr_2_vor(stmt->Lhs()) : NULL;
  if (lvor) {
    fprintf(fp, " ");
    lvor->Print(fp);
  }
  FOR_ARRAY* for_array = NULL;
  if (stmt->Lhs() != NULL) {
    for_array = Cr_2_for_array(stmt->Lhs());
  }
  if (for_array) {
    for (INT i = 0; i < for_array->size(); i++) {
      FSM_OBJ_REP* lfor = (*for_array)[i];
      if (lfor) {
        fprintf(fp, " ");
        lfor->Print(fp);
      }
    }
  }
  if (VSA_Enable_TAG_OLD) {
    TOR_LIST_OLD *tor_list = stmt->Lhs() ? Cr_2_tor_list<TOR_LIST_OLD>(stmt->Lhs()) : NULL;
    if (tor_list) {
      fprintf(fp, " ");
      tor_list->Print(fp);
    }
  } else {
    TOR_LIST *tor_list2 = stmt->Lhs() ? Cr_2_tor_list<TOR_LIST>(stmt->Lhs()) : NULL;
    if (tor_list2) {
      fprintf(fp, " ");
      tor_list2->Print(fp);
    }
  }

  if (OPERATOR_is_call(opr) && OPERATOR_has_sym(opr)) {
    ST* st = stmt->St();
    if (st && ST_name(*st))
      fprintf(fp, " #%s", ST_name(*st));
  } else if(opr == OPR_ICALL) {
    RNA_NODE *rna = Dna()->Get_callsite_rna(stmt);
    if(rna) {
      fprintf(fp, " #candidates:");
      for(CALLEE_VECTOR::const_iterator it = rna->Callee_list().begin();
      it != rna->Callee_list().end(); it++) {
        DNA_NODE* callee = Ipsa()->Get_dna(it->Callee());
        fprintf(fp, "%s ", callee->Fname());
      }
    }
  } else if (opr == OPR_INTRINSIC_CALL) {
    fprintf(fp, " #%s", INTRINSIC_name(stmt->Rhs()->Intrinsic()));
  } else if (opr == OPR_STID || opr == OPR_ISTORE) {
    char *st_name = Get_st_name_from_cr(stmt->Lhs());
    if (st_name) {
      if (st_name[0] != '\0')
        fprintf(fp, " #%s", st_name);
      free(st_name);
    }
    if (opr == OPR_STID && stmt->Lhs()->Vsa_annot()) {
      fprintf(fp, " flags: ");
      VANT_UTIL::Dump(fp, stmt->Lhs()->Vsa_annot());
    }
  }
  if (stmt->Stmtrep_id() != 0) {
    fprintf(fp, " sr%d", stmt->Stmtrep_id());
  }
  if (Srcpos_To_Line(stmt->Linenum()) != 0) {
    fprintf(fp, " LINE %d___", Srcpos_To_Line(stmt->Linenum()));
  }
  fprintf(fp, "\n");

  // var chi list
  if (stmt->Has_chi() && stmt->Chi_list() && !stmt->Chi_list()->Is_Empty()) {
    fprintf(fp, " chi<");
    CHI_NODE *cnode;
    CHI_LIST_ITER chi_iter;
    BOOL comma = FALSE;
    FOR_ALL_NODE(cnode, chi_iter, Init(stmt->Chi_list())) {
      if (cnode->Live()) {
        if (comma)
          fprintf(fp, ", ");
        else
          comma = TRUE;
        CODEREP *res = cnode->RESULT(), *opnd = cnode->OPND();
        fprintf(fp, "%d/cr%d", cnode->Aux_id(), res->Coderep_id());
        HEAP_OBJ_REP* hor_res = Cr_2_heap_obj(res);
        if (hor_res != NULL) {
          fprintf(fp, ":");
          hor_res->Print(fp);
        }
        VSYM_OBJ_REP* vor_res = Cr_2_vor(res);
        if (vor_res != NULL) {
          fprintf(fp, ":");
          vor_res->Print(fp);
        }
        for_array = Cr_2_for_array(res);
        if (for_array) {
          for (INT i = 0; i < for_array->size(); i++) {
            FSM_OBJ_REP* for_res = (*for_array)[i];
            if (for_res != NULL) {
              fprintf(fp, ":");
              for_res->Print(fp);
            }
          }
        }
        fprintf(fp, "/cr%d", opnd->Coderep_id());
        HEAP_OBJ_REP* hor_opnd = Cr_2_heap_obj(opnd);
        if (hor_opnd != NULL) {
          fprintf(fp, ":");
          hor_opnd->Print(fp);
        }
        VSYM_OBJ_REP* vor_opnd = Cr_2_vor(opnd);
        if (vor_opnd != NULL) {
          fprintf(fp, ":");
          vor_opnd->Print(fp);
        }
        for_array = Cr_2_for_array(opnd);
        if (for_array) {
          for (INT i = 0; i < for_array->size(); i++) {
            FSM_OBJ_REP* for_opnd = (*for_array)[i];
            if (for_opnd != NULL) {
              fprintf(fp, ":");
              for_opnd->Print(fp);
            }
          }
        }
      }
    }
    fprintf(fp, ">\n");
  }
  // hor chi list
  if (Stmt_hor_chi(stmt) && !Stmt_hor_chi(stmt)->Is_Empty()) {
    fprintf(fp, " hor_chi<");
    CHI_NODE *cnode;
    CHI_LIST_ITER chi_iter;
    BOOL comma = FALSE;
    FOR_ALL_NODE(cnode, chi_iter, Init(Stmt_hor_chi(stmt))) {
      if (comma)
        fprintf(fp, ", ");
      else
        comma = TRUE;
      CHOR* res = (CHOR*) cnode->RESULT();
      Print_chor(res, fp);
      fprintf(fp, "/");
      CHOR* opnd = (CHOR*) cnode->OPND();
      Print_chor(opnd, fp);
    }
    fprintf(fp, ">\n");
  }
  // for chi list
  // vor chi list
  if (Stmt_vor_chi(stmt) && !Stmt_vor_chi(stmt)->Is_Empty()) {
    fprintf(fp, " vor_chi<");
    CHI_NODE *cnode;
    CHI_LIST_ITER chi_iter;
    BOOL comma = FALSE;
    FOR_ALL_NODE(cnode, chi_iter, Init(Stmt_vor_chi(stmt))) {
      if (comma)
        fprintf(fp, ",\n         ");
      else
        comma = TRUE;
      CVOR* res = (CVOR*) cnode->RESULT();
      Print_cvor(res, fp);
      fprintf(fp, "/");
      CVOR* opnd = (CVOR*) cnode->OPND();
      Print_cvor(opnd, fp);
    }
    fprintf(fp, ">\n");
  }
  // for chi list
  if (Stmt_fsm_chi(stmt) && !Stmt_fsm_chi(stmt)->Is_Empty()) {
    fprintf(fp, " for_chi<");
    CHI_NODE *cnode;
    CHI_LIST_ITER chi_iter;
    FSM_OBJ_REP *forep;
    BOOL comma = FALSE;
    FOR_ALL_NODE(cnode, chi_iter, Init(Stmt_fsm_chi(stmt))) {
      if (comma)
        fprintf(fp, ", ");
      else
        comma = TRUE;

      forep = (FSM_OBJ_REP *) cnode->RESULT();
      if (forep->Rsc_obj()->Kind() == RSC_KIND_FSM) {
        forep->Print(fp);
      } else {
        HEAP_OBJ_REP *hor = (HEAP_OBJ_REP *) forep;
        hor->Print(fp);
      }
      fprintf(fp, "/");
      forep = (FSM_OBJ_REP *) cnode->OPND();
      if (forep->Rsc_obj()->Kind() == RSC_KIND_FSM) {
        forep->Print(fp);
      } else {
        HEAP_OBJ_REP *hor = (HEAP_OBJ_REP *) forep;
        hor->Print(fp);
      }

    }
    fprintf(fp, ">\n");
  }
}

void
VSA::Print_bb_hor_list(BB_NODE *bb, FILE *fp) const
{
  if (Bb_ho_philist(bb) == NULL)
    return;
  PHI_LIST_ITER phi_iter;
  PHI_NODE *pnode;
  FOR_ALL_NODE(pnode, phi_iter, Init(Bb_ho_philist(bb))) {
    Print_rscor_phi(pnode, (HEAP_OBJ_REP *)pnode->RESULT(), fp);
  }
}

void
VSA::Print_bb_vor_list(BB_NODE *bb, FILE *fp) const
{
  if (Bb_vo_philist(bb) == NULL)
    return;
  PHI_LIST_ITER phi_iter;
  PHI_NODE *pnode;
  FOR_ALL_NODE(pnode, phi_iter, Init(Bb_vo_philist(bb))) {
    Print_rscor_phi(pnode, (VSYM_OBJ_REP *)pnode->RESULT(), fp);
  }
}

void
VSA::Print_bb_for_list(BB_NODE *bb, FILE *fp) const
{
  if (Bb_fo_philist(bb) == NULL)
    return;
  PHI_LIST_ITER phi_iter;
  PHI_NODE *pnode;
  FOR_ALL_NODE(pnode, phi_iter, Init(Bb_fo_philist(bb))) {
    Print_rscor_phi(pnode, (FSM_OBJ_REP *)pnode->RESULT(), fp);
  }
}

void
VSA::Print_bb(BB_NODE *bb, FILE *fp) const
{
  fprintf(fp, "---- BB%d ", bb->Id());
  BB_NODE *kid;
  BB_LIST_ITER kid_iter;
  // BB preds & succs
  fprintf(fp, " Pred: <");
  FOR_ALL_ELEM(kid, kid_iter, Init(bb->Pred())) {
    fprintf(fp, "BB%d ", kid->Id());
  }
  fprintf(fp, "> Succ: <");
  FOR_ALL_ELEM(kid, kid_iter, Init(bb->Succ())) {
    fprintf(fp, "BB%d ", kid->Id());
  }
  fprintf(fp, "> Dom: <");
  FOR_ALL_ELEM(kid, kid_iter, Init(bb->Dom_bbs())) {
    fprintf(fp, "BB%d ", kid->Id());
  }
  if (bb->Dom_frontier()) {
    fprintf(fp, "> Dom_frontier: <");
    bb->Dom_frontier()->Print(fp);
  }
  fprintf(fp, ">\n");
  // var phi list
  if (bb->Phi_list() && bb->Phi_list()->Len() > 0) {
    bb->Phi_list()->Print(fp);
  }
  // hor phi list
  Print_bb_hor_list(bb, fp);
  // vor phi list
  Print_bb_vor_list(bb, fp);
  // for phi list
  Print_bb_for_list(bb, fp);
  // stmt list
  STMTREP* stmt = bb->First_stmtrep();
  for (; stmt != NULL; stmt = stmt->Next()) {
    Print_sr(stmt, fp);
  }
}

void
VSA::Print_hor(FILE* fp) const
{
  Print_obj("Heap Object", fp);
}

void
VSA::Print_obj(const char *obj_title, FILE* fp) const
{
  BB_NODE *bb;
  BB_NODE *kid;
  BB_LIST_ITER kid_iter;
  CFG_ITER cfg_iter(_cfg);
  fprintf( fp, "\n%sCFG Tracing with %s Annotation: %s[%d]\n%s",
           DBar, obj_title ? obj_title : "", Comp_unit()->Dna()->Fname(),
           Comp_unit()->Dna()->Dna_idx(), DBar );
  FOR_ALL_NODE (bb, cfg_iter, Init()) {
    Print_bb(bb, fp);
  }
  fprintf( fp, "%sEnd CFG Tracing with %s Annotation: %s\n%s",
           DBar, obj_title ? obj_title : "", Comp_unit()->Dna()->Fname(), DBar );
}

// ====================================================================
//
// BOOL VSA::Generate_ho_exit_mu(BB_NODE *bb)
//
// ====================================================================
void
VSA::Generate_ho_exit_mu(BB_NODE *bb) {
  HOR_ARRAY* array = _ret_2_hor_array_map.Lookup(bb->Id());
  Is_True(array == NULL,
          ("return hor array for BB%d already created.", bb->Id()));
  array = CXX_NEW(HOR_ARRAY(mempool_allocator<HEAP_OBJ_REP*>(Mem_pool())),
                  Mem_pool());
  array->resize(_heap_obj_list->Len());
  _ret_2_hor_array_map.Insert(bb->Id(), array);

  HEAP_OBJ *obj;
  HO_LIST_ITER ho_iter;
  INT i = 0;
  FOR_ALL_ELEM (obj, ho_iter, Init(_heap_obj_list)) {
    (*array)[i] = obj->Top_of_stack();
    i ++;
  }
}

// ====================================================================

// void VSA::Check_var_escaped(BB_NODE *bb, STMTREP *sr,
//                             CODEREP *res, CODEREP *opnd, BOOL fwd)
// ====================================================================

void
VSA::Check_var_escaped(BB_NODE *bb, STMTREP *sr, CODEREP *res, CODEREP *opnd, BOOL fwd, BOOL escaped) {
  HEAP_OBJ_REP *hor = Cr_2_heap_obj(fwd ? opnd : res);
  if (hor == NULL) {
    if (fwd && opnd->Is_flag_set(CF_DEF_BY_PHI)) {
      PHI_NODE *phi = opnd->Defphi();
      PHI_OPND_ITER phi_opnd_iter(phi);
      CODEREP *opnd2;
      HEAP_OBJ_REP* first_hor = NULL;
      BOOL first_opnd = TRUE;
      FOR_ALL_ELEM(opnd2, phi_opnd_iter, Init()) {
        if (opnd2->Is_flag_set(CF_IS_ZERO_VERSION)) {
          first_hor = NULL;
          break;
        }
        if(first_opnd) {
          first_opnd = FALSE;
          first_hor = Cr_2_heap_obj(opnd2);
          if(first_hor == NULL)
            break;
        }
        else {
          HEAP_OBJ_REP *hor2 = Cr_2_heap_obj(opnd2);
          if(hor2 == NULL || hor2->Heap_obj() != first_hor->Heap_obj()) {
            first_hor = NULL;
            break;
          }
        }
      } 
      if(first_hor) {
        HEAP_OBJ_REP* top_hor = first_hor->Heap_obj()->Top_of_stack();
        //disable this assertion because we don't annotate hor on phi result
        //Is_True(top_hor->Attr() == ROR_DEF_BY_PHI, ("top_hor is not phi node"));
        top_hor->Set_escaped(TRUE);
        Is_Trace(Tracing(), (TFile, "HOR-ESC fwd on var cr%d ", opnd->Coderep_id()));
        Is_Trace_cmd(Tracing(), top_hor->Print(TFile));
        Is_Trace(Tracing(), (TFile, " on stmt_id=%d:\n", sr->Stmtrep_id()));
        Is_Trace_cmd(Tracing(), Print_sr(sr, TFile));
      }
    }
    return;
  }
  if (fwd) {
    if (!hor->Escaped()) {
      // need a new version based on the top HOR
      HEAP_OBJ_REP *top_hor = hor->Heap_obj()->Top_of_stack();
      Is_True(top_hor != NULL, ("VSA-MISF-ESC: hor stack is empty"));
      HEAP_OBJ_REP *new_hor = Clone_heap_obj(top_hor, bb, _mem_pool);
      new_hor->Set_attr(ROR_DEF_BY_COPY);
      new_hor->Set_stmt_def(sr, Comp_unit()->Dna());
      new_hor->Set_escaped(TRUE);
      new_hor->Gen_name(sr);
      Enter_cr_heap_obj_refmap(res, top_hor); // preserve for UAF test
      Enter_cr_heap_obj_map(res, new_hor, TRUE); // allow replace old hor with new hor
      if (Tracing()) {
        fprintf(TFile, "HOR-ESC fwd create new version ");
        new_hor->Print(TFile);
        fprintf(TFile, " = COPY ");
        top_hor->Print(TFile);
        fprintf(TFile, " for stmt_id=%d LDID: ", sr->Stmtrep_id());
        opnd->Print(0, TFile);
        if (res != opnd)
          res->Print(0, TFile);
        fprintf(TFile, " on stmt:\n");
        Print_sr(sr, TFile);
      }
    }
  }
  else {
    if (escaped && !hor->Escaped()) {
      hor->Set_escaped(TRUE);  // shouldn't set escaped directlr. refine later
      Is_Trace(Tracing(), (TFile, "HOR-ESC mark escaped on var cr%d ", opnd->Coderep_id()));
      Is_Trace_cmd(Tracing(), hor->Print(TFile));
      Is_Trace(Tracing(), (TFile, " on stmt_id=%d:\n", sr->Stmtrep_id()));
      Is_Trace_cmd(Tracing(), Print_sr(sr, TFile));
    }

    if(hor->Heap_obj()->Top_match_sr(sr)) {
      //Is_True(hor->Heap_obj()->Top_of_stack()->Escaped(), ("hor is not escaped"));
      hor->Heap_obj()->Pop();
    }
  }
}

// ====================================================================

// BOOL VSA::Check_heap_obj_escaped(BB_NODE *bb, STMTREP *sr,
//                                  CODEREP *cr, BOOL fwd)
// ====================================================================

void
VSA::Check_heap_obj_escaped(BB_NODE *bb, STMTREP *sr, CODEREP *cr, BOOL fwd, BOOL escaped) {
  switch (cr->Kind()) {
  case CK_CONST:
  case CK_RCONST:
    // nothing
    return;
  case CK_LDA:
    {
      if (cr->Is_flag_set(CF_LDA_LABEL))
        return;
      // can escape
      BOOL is_lda = TY_kind(ST_type(cr->Lda_base_st())) != KIND_ARRAY;
      HEAP_OBJ *ho = Find(cr->Lda_aux_id(), is_lda);
      if (ho != NULL) {
        HEAP_OBJ_REP *hor = ho->Top_of_stack();
        if (ho->Is_entry_chi(hor))
          break;
        if (fwd) {
          if (hor->Escaped())
            break;
          // need a new version
          HEAP_OBJ_REP *new_hor = Clone_heap_obj(hor, bb, _mem_pool);
          new_hor->Set_attr(ROR_DEF_BY_COPY);
          new_hor->Set_stmt_def(sr, Comp_unit()->Dna());
          new_hor->Set_escaped(TRUE);
          new_hor->Gen_name(sr);
          Enter_cr_heap_obj_map(cr, new_hor);
          if (Tracing()) {
            fprintf(TFile, "VSA-MISF-ESC: create new version for LDA:\n");
            new_hor->Print(TFile);
            fprintf(TFile, " = COPY ");
            hor->Print(TFile);
            fprintf(TFile, " on stmt_id=%d for LDA: ", sr->Stmtrep_id());
            cr->Print(0, TFile);
            fprintf(TFile, " on stmt:\n");
            Print_sr(sr, TFile);
          }
        }
        else {
          if (ho->Top_match_sr(sr)) {
            Is_True(!escaped || ho->Top_of_stack()->Escaped(), ("hor is not escaped"));
            ho->Pop();
          }
        }
      }
    }
    return;
  case CK_VAR:
    {
      CODEREP* res = sr->Opr() == OPR_STID && sr->Rhs()->Kind() == CK_VAR ? sr->Lhs() : cr;
      Check_var_escaped(bb, sr, res, cr, fwd, escaped);
    }
    return;
  case CK_IVAR:
    // ???
    return;
  case CK_OP:
    {
      INT i;
      for (i = 0; i < cr->Kid_count(); ++i) {
        Check_heap_obj_escaped(bb, sr, cr->Opnd(i), fwd, escaped);
      }
    }
    return;
  default:
    Is_True(FALSE, ("invalid cr kind %d", cr->Kind()));
  }
}


// ====================================================================
//
// BOOL VSA::Check_heap_obj_escaped(BB_NODE *bb, STMTREP *sr,
//                                  BOOL fwd)
// ====================================================================

void
VSA::Check_heap_obj_escaped(BB_NODE *bb, STMTREP *sr, BOOL fwd) {
  return; // disable escape analysis
  OPERATOR opr = sr->Opr();
  CODEREP *lhs, *rhs;
  // for RSC obj
  if (opr == OPR_CALL && OPERATOR_has_sym(opr)) {
    DNA_NODE *dna = Comp_unit()->Dna();
    RNA_NODE *rna = dna->Get_callsite_rna(sr);
    DNA_NODE *callee = Ipsa()->Get_dna(rna->Uniq_callee());
    if (callee && callee->Non_functional())
      return;
  }
  else if (opr == OPR_ICALL) {
    DNA_NODE *dna = Comp_unit()->Dna();
    RNA_NODE *rna = dna->Get_callsite_rna(sr);
    const CALLEE_VECTOR& callee_list = rna->Callee_list();
    for (CALLEE_VECTOR::const_iterator iter = callee_list.begin(); iter != callee_list.end(); iter++) {
      DNA_NODE *callee = Ipsa()->Get_dna(iter->Callee());
      if (callee && callee->Non_functional())
        return;
    }
  }
  // end for RSC obj
  if ((opr == OPR_CALL || opr == OPR_ICALL) &&
      !Callee_frees_heap_memory(sr) &&
      !Callee_returns_new_heap_memory(sr)) {
    // parameter escaped
#if 0
    rhs = sr->Rhs();
    UINT i = sr->Opr() == OPR_CALL ? 0 : 1;
    for (; i < rhs->Kid_count(); ++i) {
      CODEREP *opnd = rhs->Opnd(i);
      Is_True(opnd != NULL && opnd->Kind() == CK_IVAR,
              ("call opnd is not IVAR"));
      Check_heap_obj_escaped(bb, sr, opnd->Ilod_base(), fwd);
    }
#endif
    if (fwd) {
      // parameter escaped via mu
      MU_NODE *mnode;
      MU_LIST_ITER mu_iter;
      FOR_ALL_NODE(mnode, mu_iter, Init(sr->Mu_list())) {
        if (!mnode->OPND())
          continue;
        Check_var_escaped(bb, sr, mnode->OPND(), mnode->OPND(), fwd, TRUE);
      }
    }
    // global variable escaped via chi
    CHI_NODE *cnode;
    CHI_LIST_ITER chi_iter;
    FOR_ALL_NODE(cnode, chi_iter, Init(sr->Chi_list())) {
      if (!cnode->Live())
        continue;
      CODEREP* cr = cnode->RESULT();
      AUX_STAB_ENTRY *sym = Opt_stab()->Aux_stab_entry(cr->Aux_id());
      if(sym->Is_return_preg()) // ignore the return preg
        continue;
      Check_var_escaped(bb, sr, cnode->RESULT(), cnode->OPND(), fwd, TRUE);
    }
    if (!fwd) {
      // parameter escaped via mu
      MU_NODE *mnode;
      MU_LIST_ITER mu_iter;
      FOR_ALL_NODE(mnode, mu_iter, Init(sr->Mu_list())) {
        if (!mnode->OPND())
          continue;
        Check_var_escaped(bb, sr, mnode->OPND(), mnode->OPND(), fwd, TRUE);
      }
    }
  }
  else if (opr == OPR_RETURN || opr == OPR_RETURN_VAL) {
    MU_NODE *mnode;
    MU_LIST_ITER mu_iter;
    FOR_ALL_NODE (mnode, mu_iter, Init(sr->Mu_list())) {
      if (!mnode->OPND())
        continue;
      Check_var_escaped(bb, sr, mnode->OPND(), mnode->OPND(), fwd, TRUE);
    }
  }
  else {
    BOOL escaped = FALSE;
    CODEREP* base = NULL;
    if (opr == OPR_ISTORE || opr == OPR_ISTBITS ||
        opr == OPR_MSTORE || opr == OPR_ISTOREX) {
      base = Find_base_pointer_load(sr->Lhs()->Istr_base());
      if (base != NULL) {
        HEAP_OBJ_REP *lhs_hor = Cr_2_heap_obj(base);
        if (lhs_hor != NULL && lhs_hor->Escaped())
          escaped = TRUE;
      }
    }
    else if (opr == OPR_STID || opr == OPR_STBITS) {
      base = sr->Lhs();
      AUX_STAB_ENTRY *aux = _opt_stab->Aux_stab_entry(base->Aux_id());
      if (aux->Is_preg() && Is_Return_Preg(aux->St_ofst()) &&
          sr->Next() != NULL && sr->Next()->Opr() == OPR_RETURN)
        escaped = TRUE;
    }
    if (escaped == FALSE && base != NULL) {
      ST* st = NULL;
      if (base->Kind() == CK_VAR)
        st = Opt_stab()->Aux_stab_entry(base->Aux_id())->St();
      else if (base->Kind() == CK_LDA)
        st = base->Lda_base_st();
      if (st != NULL) {
        switch (ST_sclass(st)) {
        case SCLASS_COMMON:
        case SCLASS_FSTATIC:
        case SCLASS_PSTATIC:
        case SCLASS_UGLOBAL:
        case SCLASS_DGLOBAL:
        case SCLASS_EXTERN:
        case SCLASS_FORMAL_REF:
          escaped = TRUE;
          break;
        case SCLASS_FORMAL:
          if (base->Kind() == CK_VAR && !OPERATOR_is_scalar_store(opr))
            escaped = TRUE;
        default:
          break;
        }
      }
    }
    if (sr->Rhs() && (escaped || fwd == FALSE)) // make sure hor is pop'ed correctly
      Check_heap_obj_escaped(bb, sr, sr->Rhs(), fwd, escaped);
  }

#if 0
  if (sr->Chi_list() && sr->Rhs() != NULL) {
    BOOL escaped = FALSE;
    CHI_NODE *cnode;
    CHI_LIST_ITER chi_iter;
    FOR_ALL_NODE(cnode, chi_iter, Init(sr->Chi_list())) {
      if (!cnode->Live())
        continue;
      if (cnode->Aux_id() == _opt_stab->Default_vsym() ||
          cnode->Aux_id() == _opt_stab->Return_vsym()) {
        escaped = TRUE;
        break;
      }
    }
    if (escaped)
      Check_heap_obj_escaped(bb, sr, sr->Rhs(), fwd);
  }

  else if (opr == OPR_ISTORE || opr == OPR_ISTBITS || opr == OPR_MSTORE || opr == OPR_ISTOREX) {
    BOOL escaped = FALSE;
    CHI_NODE *cnode;
    CHI_LIST_ITER chi_iter;
    FOR_ALL_NODE(cnode, chi_iter, Init(sr->Chi_list())) {
      if (!cnode->Live())
        continue;
      if (cnode->Aux_id() == _opt_stab->Default_vsym() ||
          cnode->Aux_id() == _opt_stab->Return_vsym()) {
        escaped = TRUE;
        break;
      }
      AUX_STAB_ENTRY *aux = _opt_stab->Aux_stab_entry(cnode->Aux_id());
      ST *st = aux->St();
      // aux->Base() ??
      if (st != NULL &&
          (ST_sclass(st) == SCLASS_FSTATIC ||
           ST_sclass(st) == SCLASS_UGLOBAL ||
           ST_sclass(st) == SCLASS_DGLOBAL ||
           ST_sclass(st) == SCLASS_EXTERN ||
           ST_sclass(st) == SCLASS_COMMON ||
           ST_sclass(st) == SCLASS_FORMAL ||
           ST_sclass(st) == SCLASS_FORMAL_REF)) {
        escaped = TRUE;
        break;
      }
    } 
    if (escaped == TRUE) {
      Check_heap_obj_escaped(bb, sr, sr->Rhs(), fwd);
    }
  }
#endif
}

// =============================================================================
//
// VSYM_TRACKER::Init, initialize the tracker stack, it iterate through the
//               ilod/istr base and construct the sequence of iload till there
//               is a non-NULL vor.
//
// =============================================================================
CHECKER_STATUS
VSYM_TRACKER::Init(const VSA *vsa, CODEREP **x, STMTREP *sr, MEM_POOL *mem_pool)
{
  CODEREP *cr = *x;
  Is_True(cr->Kind() == CK_IVAR && cr->Opr() != OPR_PARM, ("invalid ivar"));
  Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
           (TFile, " -%s: Checking ivar U-D for cr%d:\n",
            "VSYM_TRACKER", cr->Coderep_id()));
  Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG), cr->Print(TFile));
#if 0
  IDTYPE fld_id;  // we reconstruct index of an array load if possible
  if (TY_kind(base_tyidx) == KIND_ARRAY)
    fld_id = new_sym->Base_byte_ofst()/new_size;
  else if (TY_kind(base_tyidx) == KIND_STRUCT)
    fld_id = new_sym->Field_id();
#endif
  VSYM_OBJ_REP* vor = vsa->Cr_2_vor(cr);
  IDTYPE        fldid = (vor)?
    vsa->Synthesize_fldid(vor->Vsym_obj()->Base_hor(), cr->I_field_id(), cr->Offset()) :
    cr->I_field_id();
  VSYM_FLD_REP *vfr = CXX_NEW(VSYM_FLD_REP(FLD_K_ID, fldid, vsa->Cr_ofst(cr)), mem_pool);
  while (vor == NULL) {
    Is_True(cr->Kind() == CK_IVAR && cr->Opr() != OPR_PARM,
            ("invalid cr"));
    Push(vfr);
    CODEREP* base = (sr && cr == sr->Lhs()) ? cr->Istr_base() : cr->Ilod_base();
    CODEREP* base_cr = Find_ilod_base(base);
    if (base_cr == NULL) {
      Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
               (TFile, " -%s: Done. missing base for cr%d:\n",
                "VSYM_TRACKER", cr->Coderep_id()));
      Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG), cr->Print(TFile));
      return CS_DONE;
    }
    Is_True(base_cr != NULL, ("invalid base cr"));
    if (base_cr->Kind() == CK_LDA) {
      MU_NODE* mu = cr->Ivar_mu_node();
      Is_True(mu != NULL && mu->Aux_id() == base_cr->Lda_aux_id(),
              ("no ivar_mu for lda"));
      if (mu == NULL || mu->Aux_id() != base_cr->Lda_aux_id()) {
        Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
                 (TFile, " -%s: Done. missing mu for cr%d:\n",
                  "VSYM_TRACKER", cr->Coderep_id()));
        Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG), cr->Print(TFile));
        return CS_DONE;
      }
      Is_True(mu->OPND() != NULL, ("invalid mu node"));
      base_cr = mu->OPND();
    }
    if (base_cr->Kind() == CK_VAR) {
      //Is_True(FALSE, ("should not happen"));
      *x = base_cr;
      return CS_VAR_UD;
    }
    *x = cr = base_cr;
    Is_True(cr->Kind() == CK_IVAR, ("not ivar"));
    vor = vsa->Cr_2_vor(cr);
    fldid = (vor)?
      vsa->Synthesize_fldid(vor->Vsym_obj()->Base_hor(), cr->I_field_id(), cr->Offset()) :
      cr->I_field_id();
    vfr = CXX_NEW(VSYM_FLD_REP(FLD_K_ID, fldid, vsa->Cr_ofst(cr)), mem_pool);
  }
  Is_True(vor != NULL &&
          (vor->Vsym_obj()->Fld_rep().Is_any() ||
           vor->Vsym_obj()->Fld_rep().Match(vfr)),
          ("field id mismatch"));
  Push(vfr);
  return CS_VSYM_UD;
}

// =============================================================================
//
// VSYM_TRACKER::Compress; ask Jianxin for the objective of this procedure
//
// =============================================================================
VSYM_OBJ_REP*
VSYM_TRACKER::Compress_old(VSA *vsa, HEAP_OBJ_REP *hor, VSYM_FLD_REP *vfr,
                       VSYM_OBJ_REP *vor, STMTREP *stmt, CODEREP *cr)
{
  // this section of code was in DNA_NODE::Check_caller_argument_for_aob
  Pop();
  while (!Empty()) {
    Is_True(hor != NULL, ("base hor is null"));
    FIELD_OBJ_REP* fobj = hor->Find_fld(vfr);
    if (fobj == NULL)
      break;
    Is_True(vfr->Match(fobj->Fld_rep()), ("fld id mismatch"));
    VSYM_OBJ_REP *fld_vor = vsa->Find_hor_mu_vor(stmt, fobj->Hor(), Fld_rep(), cr);
    if (fld_vor == NULL) {
      // have fld hor but no vor, means there is no assignment to the hor
      // TODO: talk to CHECKER about U-D is broken
      break;
    }
    vor = fld_vor;
    hor = fobj->Hor();
    vfr = Fld_rep();
    Pop();
  }
  Push(vfr);
  return vor;
}

VSYM_OBJ_REP*
VSYM_TRACKER::Compress(VSA *vsa, HEAP_OBJ_REP *hor,
                       STMTREP *stmt, CODEREP *cr, BOOL mu, BOOL &maybe)
{
  Is_Trace(Tracing(), (TFile, "  @VSYM_TRACKER_Compress for %s ", mu ? "mu" : "chi"));
  Is_Trace_cmd(Tracing(), Print(TFile));
  VSYM_OBJ_REP *vor = NULL;
  VSYM_FLD_REP* last_vfr = Fld_rep();
  int level = 1;
  while (!Empty()) {
    Is_Trace(Tracing(), (TFile, "    [L%d]:  ", level++));
    VSYM_FLD_REP* vfr = Fld_rep();
    if (hor == NULL) {
      Is_Trace(Tracing(), (TFile, "No heap obj found\n"));
      break;
    }
    VSYM_OBJ_REP *matched_vor = NULL;
    if (mu) {
      matched_vor = vsa->Find_hor_mu_vor_any(stmt, hor, vfr, cr, !VSA_Checker_Vfr_Exact_Match);
    } else {
      matched_vor = vsa->Find_hor_chi_vor_any(stmt, hor, vfr, cr, !VSA_Checker_Vfr_Exact_Match);
    }
    if (matched_vor == NULL) {
      Is_Trace(Tracing(), (TFile, "No vor mu/chi found\n"));
      break;
    }
    BOOL may_def = matched_vor->Vsym_attr() & ROR_VSYM_MAY_DEF;
    if (may_def) {
      maybe = TRUE;
    }
    vor = matched_vor;
    hor = vor->Hor();
    Is_Trace(Tracing(), (TFile, "Matched vsym[%s] ", may_def ? "M" : "D"));
    Is_Trace_cmdn(Tracing(), vor->Print(TFile), TFile);
    last_vfr = vfr;
    Pop();
  }
  if (vor) {
    if (Empty()) {
      Is_Trace(Tracing(), (TFile, "  @VSYM_TRACKER_Compress res: exact match \n"));
      Push(last_vfr);
    } else {
      if (VSA_Checker_Vfr_Exact_Match) {
        // if require exact match, return null vor
        vor = NULL;
      } else {
        Push(last_vfr);
      }
      Is_Trace(Tracing(), (TFile, "  @VSYM_TRACKER_Compress res: partitial match."
                                  "  Vfr_Exact_match:%s\n", VSA_Checker_Vfr_Exact_Match ? "on" : "off"));
    }
  } else {
    Is_Trace(Tracing(), (TFile, "  @VSYM_TRACKER_Compress res: no vsym matched, switch back to VAR-UD\n"));
  }
  return vor;
}

// =============================================================================
//
// VSYM_TRACKER::Expand returns matched vor
// Expand vfr gaps between current cur_hor and base_hor to make sure switch to
// right vsym level during cross function
//
// =============================================================================
VSYM_OBJ_REP *
VSYM_TRACKER::Expand(VSA *vsa, STMTREP *stmt, HEAP_OBJ_REP *cur_hor, HEAP_OBJ_REP *base_hor, CODEREP *base_cr)
{
  Is_True_Ret(cur_hor && base_hor, ("@VSYM_TRACKER_Expand: null cur_hor"), NULL);
  Is_True_Ret(cur_hor->Heap_obj() != base_hor->Heap_obj(), ("@VSYM_TRACKER_Expand: same ho"), NULL);
  Is_Trace(Tracing(), (TFile, "  @VSYM_TRACKER_Expand hor from "));
  Is_Trace_cmd(Tracing(), cur_hor->Print(TFile));
  Is_Trace(Tracing(), (TFile, " to base hor "));
  Is_Trace_cmdn(Tracing(), base_hor->Print(TFile), TFile);
  HEAP_OBJ *cur_ho = cur_hor->Heap_obj();
  HEAP_OBJ *base_ho = base_hor->Heap_obj();
  CODEREP *cur_cr = cur_ho->Ho_cr();
  VSYM_OBJ_REP *base_vor  = NULL;
  while (cur_ho != base_ho) {
    base_vor  = NULL;
    VSYM_OBJ *base_vo = NULL;
    if (cur_cr && cur_cr->Kind() == CK_IVAR) {
      base_vor = vsa->Cr_2_vor(cur_cr);
    }
    // if unable to get base vo from Ho_cr(), try get base_vo from hor's owner_vo
    // to fix the sqlite cve-2020-13632 NPD FN.
    // The hor's owner vo may has cycles, to not break the normal logic, use option
    // -VSA:exp=1 to control
    base_vo = base_vor ? base_vor->Vsym_obj() : (VSA_Experimental() ? cur_hor->Owner_vo() : NULL);
    if (base_vo) {
      cur_hor = base_vo->Base_hor();
      Is_True(cur_hor != NULL, ("not find base hor"));
      cur_ho = cur_hor->Heap_obj();
      cur_cr = cur_ho->Ho_cr();
      Is_Trace(Tracing(), (TFile, "  "));
      Push(base_vo->Fld_rep_ptr());
      continue;
    }
    break;
  }
  if (cur_ho == base_ho) {
    Is_Trace(Tracing(), (TFile, ("  @VSYM_TRACKER_Expand res: hor matched\n")));
    return base_vor;
  } else {
    CHI_LIST *chi_list = vsa->Stmt_vor_chi(stmt);
    if (chi_list && !chi_list->Is_Empty()) {
      CHI_NODE *cnode;
      CHI_LIST_ITER chi_iter;
      FOR_ALL_NODE(cnode, chi_iter, Init(chi_list)) {
        CVOR *cvor_res = (CVOR*)cnode->RESULT();
        VSYM_OBJ_REP *res_vor = cvor_res->first;
        VSYM_OBJ_REP *opnd_vor = ((CVOR*)cnode->OPND())->first;
        HEAP_OBJ *chi_ho = res_vor->Hor() ? res_vor->Hor()->Heap_obj() : NULL;
        HEAP_OBJ *opnd_ho = opnd_vor->Hor() ? opnd_vor->Hor()->Heap_obj() : NULL;

        // only match two level
        if (res_vor->Vsym_obj()->Base_hor()->Heap_obj() == base_ho &&
            (chi_ho == cur_ho || opnd_ho == cur_ho)) {
          Is_Trace(Tracing(), (TFile, "  "));
          Push(res_vor->Vsym_obj()->Fld_rep_ptr());
          Is_Trace(Tracing(), (TFile, ("  @VSYM_TRACKER_Expand res: hor matched with vor chi\n")));
          return res_vor;
        }
      }
    }
    if (base_cr->Kind() == CK_LDA &&
        cur_cr && cur_cr->Kind() == CK_VAR &&
        cur_cr->Aux_id() == base_cr->Lda_aux_id()) {
      VSYM_FLD_REP zero_fld(FLD_K_ID, 0, 0);
      VSYM_OBJ_REP *vor = vsa->Find_vor_chi_vor(stmt, base_cr, &zero_fld);
      if (vor) {
        Is_Trace(Tracing(), (TFile, "  "));
        Push(vor->Vsym_obj()->Fld_rep_ptr());
        Is_Trace(Tracing(), (TFile, ("  @VSYM_TRACKER_Expand res: hor matched with lda\n")));
        return vor;
      } else {
        Is_Trace(Tracing(), (TFile, ("    no lda vor chi found\n")));
      }
    }
  }
  Is_Trace(Tracing(), (TFile, ("  @VSYM_TRACKER_Expand res: hor unmatched\n")));
  return NULL;
}

// =============================================================================
//
// VSYM_TRACKER::Expand returns TRUE if success, FALSE if failed
// Expand vfr graps between current vor and base_cr to make sure switch to right
// vsym level during cross function
//
// =============================================================================
BOOL
VSYM_TRACKER::Expand(VSA *vsa, STMTREP *stmt, VSYM_OBJ_REP *vor, CODEREP *base_cr)
{
  // if the vor passed through function, pop the current vfr
  // For ex: foo(var->fld); // base_cr = var->fld, vor = var->fld
  if (base_cr->Kind() == CK_IVAR) {
    VSYM_OBJ_REP *base_vor = vsa->Cr_2_vor(base_cr);
    if (base_vor && (base_vor->Vsym_obj() == vor->Vsym_obj())) {
      Pop();
      Is_Trace(Tracing(), (TFile, ("  @VSYM_TRACKER_Expand res: vsym as parm, hor matched\n")));
      return TRUE;
    }
  }
  HEAP_OBJ_REP *cur_hor = vor->Vsym_obj()->Base_hor();
  if (cur_hor == NULL) {
    Is_Trace(Tracing(), (TFile, "  @VSYM_TRACKER_Expand: No base hor for vor\n"));
    Is_Trace_cmd(Tracing(), vor->Print(TFile));
    return FALSE;
  }

  HEAP_OBJ_REP *base_hor = vsa->Cr_2_heap_obj(base_cr);
  if (base_hor == NULL) {
    Is_Trace(Tracing(),
             (TFile, "  @VSYM_TRACKER_Expand: No base hor for base cr%d\n",
              base_cr->Coderep_id()));
    return FALSE;
  }
  if (base_hor->Heap_obj() == cur_hor->Heap_obj()) {
    Is_Trace(Tracing(), (TFile, ("  @VSYM_TRACKER_Expand res: hor matched\n")));
    return TRUE;
  }
  if (Expand(vsa, stmt, cur_hor, base_hor, base_cr)) {
    return TRUE;
  }
  return FALSE;
}

// =============================================================================
//
// VSA::Create_intconst returns a CODEREP whose Constval() returns the value
//      passed in as a parameter
//
// =============================================================================
CODEREP*
VSA::Create_intconst(INT64 val)
{
  return Comp_unit()->Htable()->Add_const(MTYPE_I8, val);
}

// =============================================================================
//
// VSA::Create_alloca_rscobj, to crate a heap_obj representation for the alloca
//      operator.
//
// =============================================================================
HEAP_OBJ_REP*
VSA::Create_alloca_rscobj(CODEREP *x, STMTREP *sr)
{
  HEAP_OBJ_REP *heap_obj_rep = Allocate_heap_obj(x, sr->Bb());

  heap_obj_rep->Set_attr(ROR_DEF_BY_ALLOCA);
  CODEREP* size = x->Opnd(0);
  heap_obj_rep->Heap_obj()->Set_byte_size(size);
  Enter_cr_heap_obj_map(x, heap_obj_rep);

  return heap_obj_rep;
  
}

// =============================================================================
//
// VSA::Create_aux_rscobj, to create a heap_obj representation for a variable
//      of any kind, including array, struct. ROR_DEF_BY_LDA is used for this.
//      Since such object is a real variable, it will only have one version &
//      the entry_chi version will be sufficien
//
//      The Aux_id is flatten out for the individual field access for simplier
//      optimization, the structure is different from the VSYM, where aggregage
//      based on the same pointer will use the same HEAP_OBJ.  To make it
//      consistent, we will dive into the Base() of the Aux_stab_entry(auxid)
//      and make use of the same HEAP_OBJ for multiple Lda_aux_id().
//      We will create flag, merge_fld_base, to enable this additional logic.
//
// =============================================================================
HEAP_OBJ_REP*
VSA::Create_aux_rscobj(CODEREP *x, STMTREP *sr, BOOL merge_fld_base)
{
  Is_True(x->Kind()==CK_VAR || x->Kind()==CK_LDA, ("Create_aux_rscobj, Invalid CODEREP kind called"));
  BOOL is_lda = x->Kind() == CK_LDA &&
                TY_kind(ST_type(x->Lda_base_st())) != KIND_ARRAY;
  AUX_ID auxid = x->Kind() == CK_LDA ? x->Lda_aux_id()
                                     : x->Aux_id();

  AUX_STAB_ENTRY *sym = Opt_stab()->Aux_stab_entry(auxid);
  HEAP_OBJ     *ho_w_matching_sym;
  HEAP_OBJ_REP *heap_obj_rep = NULL;

  if (merge_fld_base) {
    ho_w_matching_sym = Find(sym, is_lda);
  }
  else {
    ho_w_matching_sym = Find(auxid, is_lda);
  }
  if (ho_w_matching_sym) {
    heap_obj_rep = ho_w_matching_sym->Entry_chi();
  }
  else {
    heap_obj_rep = Allocate_heap_obj(x, sr->Bb());
    Is_True(heap_obj_rep->Heap_obj()->Sym_id() == auxid, ("aux id mismatch"));
    heap_obj_rep->Set_attr(ROR_DEF_BY_LDA);

    std::pair<CODEREP*, RNA_NODE*> size = Get_object_length(x, sr, MIN_VALUE);
  
    heap_obj_rep->Heap_obj()->Set_byte_size(size.first);
  }
  return heap_obj_rep;
}


// =============================================================================
//
// VSA::Create_lda_rscobj, to create a heap_obj representation for a local
//      variable of any kind, including array, struct. ROR_DEF_BY_LDA is used.
//      Since such object is a real variable, it will only have one version &
//      the entry_chi version will be sufficient
//
// =============================================================================
CODEREP*
VSA::Create_lda_rscobj(CODEREP *x, STMTREP *sr)
{
  if (VSA_Model_Lda() == FALSE) return NULL;
  Is_True(x->Kind() == CK_LDA, ("calling Create_lda_rscobj with non-LDA node"));
  // no rsc for LDA_LABEL
  if (x->Is_flag_set(CF_LDA_LABEL)) return NULL;

  HEAP_OBJ_REP *heap_obj_rep = Cr_2_heap_obj(x);
  if (heap_obj_rep != NULL) return x;

  heap_obj_rep = Create_aux_rscobj(x, sr, TRUE);
  Enter_cr_heap_obj_map(x, heap_obj_rep); // for rename

  return x;
}


// =============================================================================
//
// VSA::Corelate_hor(basehor, newhor) find the parent-child relation if there is
//      one.  The basehor represent the type of a base object which contains a
//      list of fields or members.   
//
// =============================================================================
IDTYPE
VSA::Synthesize_fldid(HEAP_OBJ_REP *hor, IDTYPE original, mINT32 offset) const
{
  if (VSA_Model_Lda() == 0) return original;
  if (hor == NULL || original > 0) return original;

  AUX_ID          auxid = hor->Heap_obj()->Sym_id();
  AUX_STAB_ENTRY *sym = Opt_stab()->Aux_stab_entry(auxid);

  if (sym->St() == NULL) return original;

  TY_IDX          tyidx = ST_type(sym->St());

  if (sym->Byte_size() == 0) return original;

  offset += sym->Base_byte_ofst();
  if (TY_kind(tyidx) == KIND_ARRAY) {
    INT sz = TY_size(TY_etype(tyidx));
    return (sz > 1) ? offset / sz : offset;
  }
  else if (TY_kind(tyidx) == KIND_POINTER) {
    INT sz = TY_size(TY_pointed(tyidx));
    return (sz > 1) ? offset / sz : offset;
  }
  else {
    return original;
  }
}


// =============================================================================
//
// VSA::Corelate_hor, depends on the algorithm creating the basehor and newhor,
//      They might point to the same HEAP_OBJ, but the Aux_id may be different
//      due the field_id difference or offset difference.  The FIELD_OBJ_REP
//      representations are created for the field or subscript, depending on
//      the type of the Base() object defined in the symbol table.
//
// =============================================================================
HEAP_OBJ_REP*
VSA::Corelate_hor(HEAP_OBJ_REP *basehor, IDTYPE base_auxid,
                  HEAP_OBJ_REP *newhor, IDTYPE new_auxid)
{
  if (basehor == NULL) return newhor;
  AUX_STAB_ENTRY *base_sym = Opt_stab()->Aux_stab_entry(base_auxid);
  AUX_STAB_ENTRY *new_sym = Opt_stab()->Aux_stab_entry(new_auxid);

  if (base_sym->Base() != new_sym->Base()) return basehor; // not the same base
  if (new_sym->Base_byte_ofst() == 0) {
    if (base_sym->Base_byte_ofst() != 0 ||
        base_sym->Byte_size() < new_sym->Byte_size()) { // both sym has 0 offset
      std::swap(basehor, newhor);
      std::swap(base_sym, new_sym);
    }
  }
  if (base_sym->Byte_size() + base_sym->Base_byte_ofst() >
      new_sym->Byte_size() + new_sym->Base_byte_ofst()) {
    // add newhor to basehor's Flist
    TY_IDX base_tyidx = ST_type(base_sym->St());
    INT64  new_size = new_sym->Byte_size();
    IDTYPE fld_id;
    FIELD_OBJ_REP *fl;
    if (TY_kind(base_tyidx) == KIND_ARRAY)
      fld_id = (new_size != 0) ? new_sym->Base_byte_ofst() / new_size : 0;
    else if (TY_kind(base_tyidx) == KIND_STRUCT)
      fld_id = new_sym->Field_id();
    fl = CXX_NEW(FIELD_OBJ_REP(newhor, FLD_K_ID, fld_id, new_sym->Base_byte_ofst()), Mem_pool());
    basehor->Prepend_fl(fl);
  }

  return basehor;
}

// =============================================================================
//
// VSA::Create_aggregate_heapobj, for the LHS of the stmt parameter, to create a
//      heap_obj representation for a local variable of an array, struct.
//
// =============================================================================
void
VSA::Create_aggregate_heapobj(STMTREP *stmt)
{
  if (VSA_Model_Lda() == FALSE) return;
  if (stmt->Rhs()->Kind() != CK_LDA) return;
  CODEREP *rhs = stmt->Rhs();
  HEAP_OBJ_REP *rhor = Cr_2_heap_obj(rhs);
  if (rhor->Heap_obj()->Kind() != RSC_KIND_LDA) return;

  CODEREP *lhs = stmt->Lhs();
  if (lhs->Kind() != CK_VAR) return;  // will add other kind in the future

  HEAP_OBJ_REP *hor;
  hor = Cr_2_heap_obj(lhs);
  if (hor == NULL)
    hor = Create_aux_rscobj(lhs, stmt, FALSE);

#if 0
  // shouldn't create vsym in Vsa_stmt phase because hor isn't renamed
  { // create a vsym version of lhs; should it be a function call
    UINT ifldid = lhs->Field_id();
    VSYM_OBJ_REP *vor = Allocate_vsym_obj(NULL, hor, ifldid, lhs->Offset(), NULL);
    vor->Set_srcpos_node(stmt, Comp_unit()->Dna(), PATHINFO_COPY);
    vor->Set_attr(ROR_DEF_BY_COPY);
    vor->Set_stmt_def(stmt, Comp_unit()->Dna());
    vor->Set_hor(rhor);
    Enter_cr_vor_map(lhs, vor); // the value of the vor is the rhs's hor
    // Enter_cr_heap_obj_map(lhs, rhor, TRUE); // the heap_obj_map keeps the value
  }
#endif

  HEAP_OBJ_REP *base_hor = hor;
  AUX_ID base_auxid = lhs->Aux_id();
  Enter_fsm_chi_hor(stmt, base_hor);

  // go through chi list to find any node is directly related to the stmt->Lhs()
  CHI_LIST_ITER chi_iter;
  CHI_NODE *cnode;
  CHI_LIST *chi_list = stmt->Chi_list();

  FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
    if (cnode->Live()) {
      CODEREP* cr = cnode->RESULT();
      IDTYPE   cauxid = cr->Aux_id();
      if (cauxid == Opt_stab()->Return_vsym()) continue;
      if (cauxid == Opt_stab()->Default_vsym()) continue;
      if (cr->Is_flag_set(CF_IS_ZERO_VERSION)) continue;
      AUX_STAB_ENTRY *sym = Opt_stab()->Aux_stab_entry(cauxid);
      if(sym->Is_return_preg() && cr->Usecnt() > 0) continue;
      HEAP_OBJ_REP *cnode_hor = Create_aux_rscobj(cr, stmt, TRUE);
      // real work starts here
      base_hor = Corelate_hor(base_hor, base_auxid, cnode_hor, cauxid);
      Enter_fsm_chi_hor(stmt, cnode_hor);
      Enter_cr_heap_obj_map(cr, cnode_hor);
    }
  }
}

// =============================================================================
//
// VSA::Get_heap_obj_state, find the state of the heap object of the side effect
//                          stored in the heap_obj_rep
//
// =============================================================================
HEAPSTATE
VSA::Get_heap_obj_state(HEAP_OBJ_REP *hor) const
{
   if (hor == NULL)
    return HS_NONE;
  else {
    switch (hor->Attr()) {
    case ROR_DEF_BY_FREE:
      return HS_DANGLE;
    case ROR_DEF_BY_ALLOC:
      return HS_GOOD;
    case ROR_DEF_BY_COPY:
      return Get_heap_obj_state(hor->Prev_ver());
    default:
      return HS_NONE;      
    }
  }
  return HS_NONE;
}


// Find all the codereps appear in bb's last stmtrep
//
void 
VSA::Find_coderep_in_last_stmtrep(BB_NODE *bb, hash_set<CODEREP*>& crs) const
{
  VRA* vra = _cu->Vra();
  STMTREP *sr = bb->Last_stmtrep();
  Is_True(sr != NULL, ("last stmtrep in cd BB is NULL"));
  if (sr == NULL || sr->Rhs() == NULL)
    return;
  OPERATOR opr = sr->Opr();
  if (opr != OPR_AGOTO)
  {
      // ignore AGOTO
    Is_True(opr == OPR_TRUEBR || opr == OPR_FALSEBR ||
        opr == OPR_COMPGOTO,
        ("TODO: handle %s in last stmtrep in cd BB", OPERATOR_name(opr)));
    CODEREP *cr = sr->Rhs();
    Is_True(cr != NULL, ("invalid stmt rhs coderep"));
    if(!vra->Analyze_coderep_vars(cr, crs))
      crs.clear();
  }
}

// Check if sr is called under checking of pointer to be freed
INT
VSA::Is_conditional_free(STMTREP *stmt) const
{
  if (!OPERATOR_is_call(stmt->Opr()) ||
      !stmt->Callee_frees_heap_memory())
    return FALSE;

  CODEREP *opnd = Vsa_free_ptr_opnd(stmt);
  Is_True_Ret(opnd != NULL, ("no free opnd found"), TRUE);

  // find cr used in control dependency
  hash_map<CODEREP*, STMTREP*> crs;
  Find_coderep_in_cd_bbs(stmt->Bb(), crs);

  // no control dependency vars
  if (crs.size() == 0)
    return FALSE;

  hash_map<CODEREP*, STMTREP*>::iterator it = crs.find(opnd);
  if (it == crs.end())
    return FALSE;

  STMTREP *ctrl = it->second;
  Is_True(ctrl != NULL && ctrl->Rhs() != NULL, ("bad ctrl stmtrep"));

  CODEREP *cr = ctrl->Rhs();
  // for the following cases, still treat it unconditionally:
  // 1. if (p) or if (*p)
  // 2. if (a+b), etc
  // 3. if (p cmp 0)
  if (cr->Kind() != CK_OP ||
      !OPERATOR_is_compare(cr->Opr()) ||
      (cr->Opnd(0)->Kind() == CK_CONST &&
       cr->Opnd(0)->Const_val() == 0) ||
      (cr->Opnd(1)->Kind() == CK_CONST &&
       cr->Opnd(1)->Const_val() == 0))
    return FALSE;

  // free conditionally
  return TRUE;
}

// Find all the codereps appears in  the last stmtrep
// of all the bb's control dependence BBs
//
void
VSA::Find_coderep_in_cd_bbs(BB_NODE *bb, hash_map<CODEREP*, STMTREP*>& crs) const
{
  BB_LIST_CONTAINER worklist;
  MEM_POOL          bbset_pool;
  BS_ELT            bbs = Cfg()->Total_bb_count();

  OPT_POOL_Initialize(&bbset_pool, "SSA bb set pool", FALSE, SSA_DUMP_FLAG);
  OPT_POOL_Push(&bbset_pool, SSA_DUMP_FLAG);
  BB_NODE_SET       visited(bbs, Cfg(), &bbset_pool, BBNS_EMPTY);

  BB_NODE_SET *cd = bb->Rcfg_dom_frontier();
  BB_NODE_SET_ITER cd_iter;
  BB_NODE *cd_bb;
  FOR_ALL_ELEM (cd_bb, cd_iter, Init(cd)) {
    worklist.Append(cd_bb, &bbset_pool);
  }

  BB_NODE *bbx;
  hash_set<CODEREP*> crs0;

  while(bbx = worklist.Remove_head(&bbset_pool)) {
    if (visited.MemberP(bbx) == FALSE)
    {
      visited.Union1D(bbx);
      Find_coderep_in_last_stmtrep(bbx, crs0);
      for(hash_set<CODEREP*>::iterator it = crs0.begin();
          it != crs0.end(); ++it)
      {
        CODEREP* cr = *it;
        crs.insert(std::make_pair(cr, bbx->Last_stmtrep()));
        Is_Trace(Tracing(),
            (TFile, ("Find_coderep_in_cd_bbs: insert cr%d\n"), cr->Coderep_id()));
      }

      cd = bbx->Rcfg_dom_frontier();
      FOR_ALL_ELEM(cd_bb, cd_iter, Init(cd)) {
        worklist.Append(cd_bb, &bbset_pool);
      }
    }
  }

  OPT_POOL_Pop(&bbset_pool, SSA_DUMP_FLAG);
  OPT_POOL_Delete(&bbset_pool, SSA_DUMP_FLAG);
}


// ====================================================================
// VSA::Initialize_ignore_ho_ids
// ====================================================================
void
VSA::Initialize_ignore_ho_ids(MEM_POOL* mp)
{
  Is_True(_ignore_ho_map == NULL, ("_ignore_ho_map already initialized"));
  Is_True(_ignore_ho_map_mp == NULL, ("_ignore_ho_map_mp already initialized"));

  OPT_POOL_Push(mp, VSA_DUMP_FLAG);

  _ignore_ho_map = TYPE_OPT_POOL_ALLOC_N(HEAP_OBJ_MAP*,
                                         mp,
                                         Cfg()->Total_bb_count(),
                                         VSA_DUMP_FLAG);
  memset(_ignore_ho_map, 0,
         sizeof(HEAP_OBJ_MAP*) * Cfg()->Total_bb_count());
  _ignore_ho_map_mp = mp;
}

// ====================================================================
// VSA::Finalize_ignore_ho_ids
// ====================================================================
void
VSA::Finalize_ignore_ho_ids(MEM_POOL* mp)
{
  Is_True(_ignore_ho_map != NULL, ("_ignore_ho_map not initialized"));
  Is_True(_ignore_ho_map_mp == mp, ("bad mempool used"));
  _ignore_ho_map = NULL;
  _ignore_ho_map_mp = NULL;

  OPT_POOL_Pop(mp, VSA_DUMP_FLAG);
}

// ====================================================================
// VSA::Find_ignore_ho_ids help screening out the follow pattern:
//      p = malloc(...);
//      if (p == NULL)
//        return;  // return without free
//      In the above code section, p is NULL and therefore it should
//      not be reported as MSF.
//      In rare situation like the following
//      if (cond)
//        p = malloc(...);
//      else
//        p = malloc(...);
//      if (p == NULL)
//        return;
//      There will be two heap_obj.  It is a case that we need more
//      work to do.  For the time being, we will not report MSF at all
// ====================================================================
HEAP_OBJ_MAP*
VSA::Find_ignore_ho_ids(BB_NODE *bb)
{
  Is_True_Ret(_ignore_ho_map != NULL, ("_ignore_ho_map not initialized"), NULL);
  Is_True_Ret(_ignore_ho_map_mp != NULL, ("_ignore_ho_map_mp not initialized"), NULL);
  Is_True_Ret(bb->Id() < Cfg()->Total_bb_count(), ("bb id out of bound"), NULL);

  VRA* vra = _cu->Vra();
  if (vra == NULL)   // Need vra
    return NULL;

  IDTYPE bb_id = bb->Id();
  if (_ignore_ho_map[bb_id] != NULL)
    return _ignore_ho_map[bb_id];

  HEAP_OBJ_MAP* ho_ids = CXX_NEW(HEAP_OBJ_MAP(3,
                                              __gnu_cxx::hash<IDTYPE>(),
                                              __gnu_cxx::equal_to<IDTYPE>(),
                                              HEAP_OBJ_ALLOCATOR(_ignore_ho_map_mp)),
                                 _ignore_ho_map_mp);

  hash_map<CODEREP*, STMTREP*> crs;
  Find_coderep_in_cd_bbs(bb, crs);
  for (hash_map<CODEREP*, STMTREP*>::iterator it = crs.begin();
       it != crs.end(); ++ it) {
    CODEREP* cr = it->first;
    HEAP_OBJ_REP* hor = Cr_2_heap_obj(cr);
    if (hor == NULL) {
      VSYM_OBJ_REP *vor = Cr_2_vor(cr);
      if (vor) {
        hor = vor->Hor();
      }
    }
    if (hor == NULL || hor->Is_entry_chi())
      continue;
    IDTYPE ho_id = hor->Heap_obj()->Id();
    if (ho_ids->find(ho_id) != ho_ids->end())
      continue;

    PATH_SELECTED paths;
    VRA_RESULT va = vra->Var_cmp_val<OPR_NE>(cr, bb_id, (INT64)0, paths);
    // check if cr is compared with NULL
    if (va == VA_YES || va == VA_POSSIBLE) {
      STMTREP *sr = it->second;
      Is_True(sr != NULL && sr->Rhs() != NULL, ("bad cond stmtrep"));
      CODEREP *cr = sr->Rhs();
      // for the following cases, still report D MSF
      // 1. if(p) or if(*p)
      // 2. if (a + b), etc
      // 3. if (p cmp 0)
      if (cr->Kind() != CK_OP ||
          !OPERATOR_is_compare(cr->Opr()) ||
          (cr->Opnd(0)->Kind() == CK_CONST &&
           cr->Opnd(0)->Const_val() == 0) ||
          (cr->Opnd(1)->Kind() == CK_CONST &&
           cr->Opnd(1)->Const_val() == 0))
        continue;
    }

    // cr of this hor is probably NULL, not report or report M MSF on this hor
    Is_Trace(Tracing(),
             (TFile, "Find_ignore_ho_ids(BB%d): cr is null and msf on hor is possibly ignored. VA=%d\n", bb_id, va));
    Is_Trace_cmd(Tracing(), cr->Print(0, TFile));
    Is_Trace_cmd(Tracing(), hor->Print(TFile));
    Is_Trace(Tracing(), (TFile, ("\n")));
    HOR_LIST *ul = hor->Ulist();
    if (ul != NULL  && hor->Is_phi()) {
      HEAP_OBJ_REP *heap_obj_rep;
      HOR_LIST_ITER hor_list_iter;
      FOR_ALL_ELEM (heap_obj_rep, hor_list_iter, Init(ul)) {
        ho_ids->insert(HEAP_OBJ_PAIR(heap_obj_rep->Heap_obj()->Id(), va));
        Is_Trace(Tracing(),
                 (TFile, "Find_ignore_ho_ids(BB%d): hor is possibly ignored. VA=%d\n", bb_id, va));
        Is_Trace_cmd(Tracing(), heap_obj_rep->Print(TFile));
        Is_Trace(Tracing(), (TFile, ("\n")));
      }
    }
    else {
      ho_ids->insert(HEAP_OBJ_PAIR(ho_id, va));
    }
  }

  // cache the value
  _ignore_ho_map[bb_id] = ho_ids;
  return ho_ids;
}

void
VSA::Classify_msf_error(BB_NODE *bb)
{
  HOR_ARRAY* array = _ret_2_hor_array_map.Lookup(bb->Id());
  if (array == NULL)
    return;
  // get heap_objs' id whose cr's value is NULL in this BB,
  HEAP_OBJ_MAP* ho_ids = Find_ignore_ho_ids(bb);

  INT i;
  for (i = 0; i < array->size(); ++i) {
    HEAP_OBJ_REP* hor = (*array)[i];
    if (ho_ids != NULL &&
        ho_ids->find(hor->Heap_obj()->Id()) == ho_ids->end()) { // only check non-null heap object
      STMTREP *sr = bb->Last_stmtrep();
      //SRCPOS_HANDLE srcpos_h(NULL, sr, Dna(), Loc_pool());
      SRCPOS_HANDLE srcpos_h(Dna(), Loc_pool());
      srcpos_h.Path()->Push_mark(Comp_unit());
      srcpos_h.Path()->Add_bb(sr->Bb());
      // append a fake exit of the function except the following 2 cases:
      // 1. if the end of function srcpos is 0
      // 2. if the return is after a noreturn call
      if (Comp_unit()->End_srcpos()) {
        STMTREP *prev = sr->Prev();
        if (prev == NULL || prev->Opr() != OPR_CALL ||
            !PU_has_attr_noreturn((*Pu_Table_ptr)[ST_pu(prev->St())]))
          srcpos_h.Append_data(Comp_unit()->End_srcpos(), NULL,
                               Dna(), PATHINFO_VUL_SPOT_SO);
      }
      if (sr != NULL) {
        srcpos_h.Append_data(sr, Comp_unit()->Dna(), PATHINFO_FUN_EXIT);
      }
      else {
        srcpos_h.Append_data(bb, Comp_unit()->Dna(), PATHINFO_FUN_EXIT);
      }

      switch (hor->Attr()) {
      case ROR_DEF_BY_ALLOC:
      case ROR_DEF_BY_COPY:
        // if the LHS of defstmt is return register, we will not report MSF
        // if the LHS of defstmt is global or static, we report may_MSF
        // otherwise, check its prev_ver()
        //if (hor->Asgn_attr() == ROR_ASGN_TO_RETREG ||
        //    hor->Asgn_attr() == ROR_ASGN_TO_OPARM)
        //  break;
        // fail through
      case ROR_DEF_BY_PHI:
        {
          // exclude the case like
          // q = malloc();
          // if (...) p = q;
          // return p;   // should still have a MSF on q
          if ((hor->Attr() != ROR_DEF_BY_PHI ||
               !hor->Is_phi_identical()) &&
              (hor->Asgn_attr() == ROR_ASGN_TO_RETREG ||
               hor->Asgn_attr() == ROR_ASGN_TO_OPARM))
            break;
          ISSUE_CERTAINTY ic = hor->Asgn_attr() == ROR_ASGN_TO_GLOBAL ? IC_MAYBE
                                                                      : IC_DEFINITELY;
          hash_set<IDTYPE> visited_bb;
          Classify_msf_error(bb, hor, &srcpos_h, visited_bb, ic);
        }
        break;
      default:
        break;
      }

#if 0
      if(!hor->Escaped()) {
        hash_set<IDTYPE> visited_bb;
        Classify_msf_error(bb, hor, &srcpos_h, visited_bb);
      }
      else if (hor->Attr() == ROR_DEF_BY_COPY &&
               hor->Prev_ver() != NULL && !hor->Prev_ver()->Escaped() &&
               hor->Prev_ver()->Attr() == ROR_DEF_BY_PHI &&
               !hor->Prev_ver()->Is_phi_identical()) {
        // hor might be escaped due to the return statement itself, which
        // presents itself as an assignment to a PREG that represent the
        // return register.  Checking the flag Escaped on hor along would
        // result in false negative as it is presented in the regression test
        // 'msf_false_negtive2.c'.  This block of code is intended to fix it.
        hash_set<IDTYPE> visited_bb;
        Classify_msf_error(bb, hor->Prev_ver(), &srcpos_h, visited_bb);
      }
#endif
    }
  } 
}


void
VSA::Classify_msf_error(BB_NODE *bb, HEAP_OBJ_REP* hor, SRCPOS_HANDLE *srcpos_h, hash_set<IDTYPE> &visited_bb, ISSUE_CERTAINTY ic)
{
  // java memory is freed by GC, no need to report MSF
  if (PU_java_lang(Get_Current_PU())) return;
  HEAP_OBJ *ho;
  STMTREP  *def;

  switch (hor->Attr()) {
  case ROR_DEF_BY_ALLOC:
    // if exit_mu contains a HOR that is defined by alloc, it is a leak.
    Is_Trace(Tracing(),
             (TFile, "VSA ERROR:Alloc w/o Free: heapID:%d, VerID:%d in the following stmt\n", 
              hor->Heap_obj()->Id(), hor->Version()));
    ho = hor->Heap_obj();
    srcpos_h->Append_data(hor->Srcpos_node());
    def = hor->Stmt_def();
    Is_True(def != NULL, ("hor defstmt is NULL"));
    srcpos_h->Set_orig_stname(srcpos_h->Find_orig_stname(def->Rhs(),
                                                         def,
                                                         Comp_unit()->Dna()));
    // treat the alloc stmt as the "key" srcpos for the msf issue
    srcpos_h->Set_key_srcpos(Dna(), def, NULL);
    Classify_msf_error(ho->Sym_id(), bb, srcpos_h, ic);
    srcpos_h->Remove_last_key_srcpos();
    srcpos_h->Remove_last();
    break;
  case ROR_DEF_BY_PHI: {
    // if exit_mu contains a HOR defined by phi, it is a parital leak
    // if any of phi operand is defined by alloc.
    PHI_NODE* phi = hor->Phi_def();
    Is_True(phi != NULL, ("phi is NULL"));
    if (visited_bb.find(phi->Bb()->Id()) != visited_bb.end())
      return;
    visited_bb.insert(phi->Bb()->Id());

    SRCPOS_TREENODE *cur_node = srcpos_h->Add_children(phi->Size());
    srcpos_h->Path()->Push_mark(phi);

    BB_NODE         *bb_pred;
    BB_LIST_ITER     bb_iter;
    INT32            which_pred = 0;
    INT              i = 0;
    HEAP_OBJ_REP    *prev_opnd = NULL;

    FOR_ALL_ELEM(bb_pred, bb_iter, Init(phi->Bb()->Pred())) {
      HEAP_OBJ_REP *opnd = (HEAP_OBJ_REP *)phi->OPND(i);
      Is_True(opnd != NULL, ("phi opnd is NULL"));
      i++;

      if (srcpos_h->Path()->Is_path_possible(phi, i-1) == PATH_NOT_REACHABLE)
        continue;

      if (opnd->Asgn_attr() == ROR_ASGN_TO_OPARM)
        continue;

      prev_opnd = opnd;
      HEAP_OBJ_MAP* ho_ids = Find_ignore_ho_ids(bb_pred);
      if (ho_ids != NULL &&
          ho_ids->find(opnd->Heap_obj()->Id()) == ho_ids->end()) {
        srcpos_h->Set_cur_node(cur_node, i-1);
        srcpos_h->Append_data(opnd->Srcpos_node());
        srcpos_h->Path()->Add_bb(bb_pred);
        Classify_msf_error(bb_pred, opnd, srcpos_h, visited_bb, ic);
        srcpos_h->Path()->Pop_mark(phi, FALSE);
      }
    }
    srcpos_h->Path()->Pop_mark(phi, TRUE);
  }
    break;
  case ROR_DEF_BY_COPY: {
    HEAP_OBJ_REP *prev = hor->Prev_ver();
    switch (hor->Asgn_attr()) {
    case ROR_ASGN_TO_RETREG:
    case ROR_ASGN_TO_OPARM:
      break;
    case ROR_ASGN_TO_GLOBAL:
      srcpos_h->Append_data(hor->Srcpos_node());
      Classify_msf_error(bb, prev, srcpos_h, visited_bb, IC_MAYBE);
      srcpos_h->Remove_last();
      break;
    default:
      srcpos_h->Append_data(prev->Srcpos_node());
      Classify_msf_error(bb, prev, srcpos_h, visited_bb, ic);
      srcpos_h->Remove_last();
      break;
    }
  } // ROR_DEF_BY_COPY
    break;
  default:
    break;
  }
}


void
VSA::Classify_msf_error(AUX_ID sym, BB_NODE *bb, SRCPOS_HANDLE *srcpos_h, ISSUE_CERTAINTY ic) const
{
  const char *output_var_name = (sym > 0) ? Sym_name(sym) : NULL;
  srcpos_h->Set_msgid("MSF.1");
  Report_vsa_error(NULL, output_var_name, MSF, ic, srcpos_h);
  if (VSA_Xsca) {
    Report_xsca_error(NULL, output_var_name, "MSR_22_1", srcpos_h);
  }
}


void
VSA::Classify_uaf_error(BB_NODE *bb, STMTREP *sr)
{
  if (sr->Lhs() != NULL && sr->Lhs()->Kind() != CK_VAR)
    Classify_uaf_error(bb, sr, sr->Lhs());
  if (sr->Rhs() != NULL)
    Classify_uaf_error(bb, sr, sr->Rhs());
}


void
VSA::Classify_uaf_error(BB_NODE *bb, STMTREP *sr, CODEREP *cr)
{
  if (VSA_New_Heap_Checker())
    return; // do nothing if new heap checker is on

//  printf( "%sClassify_uaf_error for BB:%d \n%s", SBar, bb->Id(), SBar );
  switch (cr->Kind()) {
  case CK_VAR: {
    HEAP_OBJ_REP *hor = Cr_2_heap_obj(cr);
    if (hor) {
      HEAP_OBJ_REP *cur_hor = Find_fsm_mu_ror(sr, hor);  // find the current version
      if (cur_hor) hor = cur_hor;
#if 0
      if (hor->Attr() == ROR_DEF_BY_COPY) { // work around HOR-ESC side effect
        HEAP_OBJ_REP *refhor = Cr_2_heap_obj_ref(cr);
        if (refhor != NULL)
          hor = refhor;
      }
#endif
      if (hor->Attr() == ROR_DEF_BY_FREE) {
        STMTREP *defstmt = hor->Stmt_def();
        SRCPOS_HANDLE srcpos_h(cr, sr, _cu->Dna(), Loc_pool());
        srcpos_h.Append_data(hor->Srcpos_node());
        // treat the free stmt as the "key" srcpos for uaf issue
        srcpos_h.Set_key_srcpos(Dna(), sr, cr);
        Classify_uaf_error(cr, bb, &srcpos_h);
      }
      if (hor->Attr() == ROR_DEF_BY_PHI) {
        SRCPOS_HANDLE srcpos_h(cr, sr, _cu->Dna(), Loc_pool());
        hash_set<IDTYPE> visited;
        Classify_uaf_error(hor, cr, bb, &srcpos_h, visited);
      }
    }
  }
    break;
  case CK_IVAR:
    if (cr->Opr() == OPR_PARM) {
      Classify_uaf_error(bb, sr, cr->Ilod_base());
    }
    else {
      VSYM_OBJ_REP* vor = Cr_2_vor(cr);
      if (vor == NULL)
        break;
      VSYM_OBJ* vo = vor->Vsym_obj();
      if (vo == NULL || vo->Kind() != RSC_KIND_VSYM)
        break;
      HEAP_OBJ_REP* hor = vo->Base_hor();
      if (hor != NULL) {
        SRCPOS_HANDLE srcpos_h(cr, sr, _cu->Dna(), Loc_pool());
        hash_set<IDTYPE> visited;
        Classify_uaf_error(hor, cr, bb, &srcpos_h, visited);
      }
    }
    break;
  case CK_OP:
    for (INT i = 0; i < cr->Kid_count(); ++i) {
      Classify_uaf_error(bb, sr, cr->Opnd(i));
    }
    break;
    
  default:
    break;
  }
}

 
void
VSA::Classify_uaf_error(HEAP_OBJ_REP *hor, CODEREP *x, BB_NODE *bb, SRCPOS_HANDLE *srcpos_h, hash_set<IDTYPE>& visited) const
{
  if (hor->Attr() == ROR_DEF_BY_FREE) {
    srcpos_h->Append_data(hor->Srcpos_node());
    STMTREP* def = hor->Stmt_def();
    Is_True(def != NULL, ("hor defstmt is NULL"));
    // treat the free stmt as the "key" srcpos for the uaf issue
    srcpos_h->Set_key_srcpos(Dna(), def, x);
    Classify_uaf_error(x, bb, srcpos_h);
    srcpos_h->Remove_last_key_srcpos();
  }
  else if (hor->Attr() == ROR_DEF_BY_COPY) {
    HEAP_OBJ_REP *prev = hor->Prev_ver();
    Is_True(prev != NULL, ("prev version is NULL"));
    if (prev != NULL) {
      srcpos_h->Append_data(hor->Srcpos_node());
      Classify_uaf_error(prev, x, bb, srcpos_h, visited);
    }
  }
  else if (hor->Attr() == ROR_DEF_BY_PHI) {
    PHI_NODE* phi = hor->Phi_def();
    if (visited.find(phi->Bb()->Id()) != visited.end())
      return;
    visited.insert(phi->Bb()->Id());
    Is_True(phi != NULL, ("phi is NULL"));
    for (INT i = 0; i < phi->Size(); ++i) {
      if (srcpos_h->Path()->Is_path_possible(phi, i) != PATH_NOT_REACHABLE) {
        HEAP_OBJ_REP *opnd = (HEAP_OBJ_REP *)phi->OPND(i);
        Is_True(opnd != NULL, ("phi opnd is NULL"));
        Classify_uaf_error(opnd, x, bb, srcpos_h, visited);
      }
    }
  }
}

 
void
VSA::Classify_uaf_error(CODEREP *x, BB_NODE *bb, SRCPOS_HANDLE *srcpos_h) const
{
  const char *output_var_name = x->Kind() == CK_VAR ?
                                Sym_name(x->Aux_id()) : "";
  srcpos_h->Set_msgid("UAF.1");
  Report_vsa_error(x, output_var_name, UAF, IC_DEFINITELY, srcpos_h);
}

