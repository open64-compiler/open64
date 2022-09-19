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

// ============================================================================
// ============================================================================
//
// Module: opt_vsa_hva.cxx
//
// ============================================================================
//

// ============================================================================
// VSA::Perform_heap_vsym_analysis
//   Perform heap and vsym creation and renaming iteratively
//   NO heap obj renaming during VO creation/renaming, hence no pure HOR phi
//   Heap obj and its unique hor created at:
//     OPT_CHI for param and param's sub hor
//     CALL for return value and output param and their sub hor
//     MALLOC/CALLOC
//     LDA/ALLOC
//     VARPHI/VORPHI for CR and VOR's phi where their operands have different HO
//  Once the CR/VOR is annotated with HO/HOR, it never changes now.
//
//  Any VSYM_FLD_REP:
//    Only 1 any vfr for each ho at most, which is always the first VO on HOR's
//    VO list.
//    Created when VOR offset is variable, or HO is escaped (passwd as parameter,
//    output parameter, or return value)
//
//  Basic algorithm:
//   do {
//     Create_vsym_obj();
//     Rename_vsym_obj();
//   } while (Has_pending_ivar() or max iteration count reached);
// ============================================================================

#include "opt_vsa_hva.h"
#include "erglob.h"
#include "config_vsa.h"
#include "builtin_rule_defs.h"
#include "opt_main.h"
#include "opt_addr_util.h"
#include "opt_vsa_util.h"
#include "opt_vsa_eh.h"
#include "opt_vsa_rbc.h"
#include "intrn_info.h"
#include "vsa_annot.h"
#include <list>


// ============================================================================
// HEAP_VSYM_ANALYSIS::Process_call_for_rsc
//
// Process_call_for_rsc: process call to handle RSC objects
// ============================================================================
void
HEAP_VSYM_ANALYSIS::Process_call_for_rsc(STMTREP* sr)
{
  RNA_NODE *rna = Dna()->Get_callsite_rna(sr);
  Is_True_Ret(rna != NULL && sr != NULL && rna->Callstmt() == sr,
              ("not find rna node"));

  if (rna->Is_flag_set(RBC_SE_MODEL)) {
    Vsa()->Ipsa()->Rbc()->Eval__mvsa_model(Dna(), rna, Defbb_pool());
  }
}

// process call for malloc
void
HEAP_VSYM_ANALYSIS::Process_call_for_malloc(STMTREP *sr)
{
  Is_True(sr && Vsa()->Callee_returns_new_heap_memory(sr),
          ("sr does not return new memory"));
  CODEREP* ret = Comp_unit()->Find_return_value(sr);
  HEAP_OBJ_REP* hor = Create_heap_obj(ret ? ret : sr->Rhs(), sr->Bb(), Defbb_pool());
  hor->Set_attr(ROR_DEF_BY_ALLOC);
  hor->Set_stmt_def(sr, Dna());
  hor->Set_srcpos_node(sr, Dna(), PATHINFO_ALLOC);
  Vsa()->Enter_cr_heap_obj_map(sr->Rhs(), hor);
  // get size
  CODEREP* size = Vsa()->Vsa_malloc_size(sr, ret, Comp_unit());
  if (size != NULL) {
    hor->Heap_obj()->Set_byte_size(size);
  }
  if (ret) {
    Vsa()->Enter_cr_heap_obj_map(ret, hor);
  }
}

// process cr for lda
void
HEAP_VSYM_ANALYSIS::Process_cr_for_lda(STMTREP *sr, CODEREP *cr)
{
  Is_True(sr && cr, ("bad sr or cr"));
  HEAP_OBJ_REP *hor;
  switch (cr->Kind()) {
  case CK_CONST:
  case CK_RCONST:
  case CK_VAR:
    break;
  case CK_LDA:
    hor = Vsa()->Cr_2_heap_obj(cr);
    if (hor == NULL) {
      hor = Create_heap_obj_for_lda(cr);
      Enter_cr_heap_obj_map(cr, hor, FALSE);
      if (hor->Stmt_def() == NULL)
        hor->Set_stmt_def(sr, Dna());
    }
    break;
  case CK_IVAR:
    cr = (cr == sr->Lhs()) ? cr->Istr_base() : cr->Ilod_base();
    Process_cr_for_lda(sr, cr);
    break;
  case CK_OP:
    if (cr->Opr() == OPR_ALLOCA) {
      hor = Vsa()->Cr_2_heap_obj(cr);
      if (hor == NULL) {
        CODEREP *bind_cr = (cr == sr->Rhs()) ? sr->Lhs() : cr;
        hor = Create_heap_obj(bind_cr, sr->Bb(), Defbb_pool());
        hor->Set_attr(ROR_DEF_BY_ALLOCA);
        hor->Set_stmt_def(sr, Dna());
        hor->Set_srcpos_node(sr, Dna(), PATHINFO_ALLOC);
        hor->Heap_obj()->Set_byte_size(cr->Opnd(0));
        if (bind_cr != cr)
          Vsa()->Enter_cr_heap_obj_map(bind_cr, hor);
        Vsa()->Enter_cr_heap_obj_map(cr, hor);
      }
    }
    for (INT i = 0; i < cr->Kid_count(); ++i) {
      Process_cr_for_lda(sr, cr->Opnd(i));
    }
    break;
  default:
    Is_True(FALSE, ("bad cr kind"));
    break;
  }
}

// process call and lda for vsym creation/renaming
// TODO: this pass should be merged with some other previous pass
void
HEAP_VSYM_ANALYSIS::Process_call_and_lda_for_vsym()
{
  for (BB_NODE *bb = Cfg()->First_bb(); bb; bb = bb->Next()) {
    for (STMTREP *sr = bb->First_stmtrep(); sr; sr = sr->Next()) {
      if (OPERATOR_is_call(sr->Opr())) {
        Process_call_for_rsc(sr);
        if (Vsa()->Callee_returns_new_heap_memory(sr)) {
          Process_call_for_malloc(sr);
        }
      }
      if (sr->Rhs()) {
        Process_cr_for_lda(sr, sr->Rhs());
      }
      if (sr->Lhs()) {
        Process_cr_for_lda(sr, sr->Lhs());
      }
    }
  }
}

pair<CODEREP *, CODEREP *>
HEAP_VSYM_ANALYSIS::Callee_frees_heap_memory(STMTREP *sr)
{
  CODEREP* arg = Vsa()->Callee_frees_heap_memory(sr);
  if (arg) {
    return make_pair(arg, (CODEREP *)NULL);
  } else {
    RNA_NODE *rna = Vsa()->Sr_2_rna(sr);
    if (rna == NULL) {
      return make_pair((CODEREP *)NULL, (CODEREP *)NULL);;
    }
    for (IDTYPE i = VAR_INIT_ID; i <= rna->Arg_cnt(); i++) {
      CODEREP* arg = rna->Get_arg(i);
      Is_True(arg != NULL, ("can not get actual %d.", i));
      if (arg->Kind() == CK_LDA) {
        VSYM_FLD_REP zero_fld(FLD_K_ID, 0, 0);
        MU_NODE *mu = Vsa()->Find_stmt_var_mu(sr, arg->Lda_base_st(), &zero_fld);
        CODEREP *mu_cr = mu ? mu->OPND() : NULL;
        if (mu_cr) {
          V_ANNOT annot = mu_cr->Kind() == CK_VAR ? mu_cr->Vsa_annot() : VANT_UTIL::Empty();
          if (annot != VANT_UTIL::Empty() && VANT_UTIL::Get(annot, ANT_FREE) != ANT_NO) {
            return make_pair(mu_cr, arg);;
          }
        }
      }
    }
  }
  return make_pair((CODEREP *)NULL, (CODEREP *)NULL);
}

typedef struct pending_free_stmt{
  STMTREP      *_sr;
  HEAP_OBJ_REP *_hor;
  CODEREP      *_cr;
  pending_free_stmt(STMTREP *sr, HEAP_OBJ_REP *hor, CODEREP *cr) : _sr(sr), _hor(hor), _cr(cr) {}
} PENDING_FREE_STMT;

// process call for heap_obj creation/renaming
void
HEAP_VSYM_ANALYSIS::Process_call_for_heap()
{
  vector<PENDING_FREE_STMT> pending_stmts;
  RNODE_VECTOR *call_list = Dna()->Call_list();
  for (INT i = VAR_INIT_ID; i < call_list->size(); ++i) {
    RNA_NODE *rna = (*call_list)[i];
    Is_True(rna != NULL && rna->Callstmt() != NULL,
            ("invalid rna"));
    STMTREP *sr = rna->Callstmt();

    pair<CODEREP *, CODEREP *> arg_pair = Callee_frees_heap_memory(sr);
    CODEREP *arg = arg_pair.first;
    if (arg != NULL) {
      CODEREP *base = (arg->Kind() == CK_OP) ? Find_ilod_base(arg)
                                             : arg;
      if (base == NULL) {
        // free non-pointer, report D UDR here
        SRCPOS_HANDLE sp_h(arg, sr, Dna(), Local_pool(), Vsa());
        const char *name = sp_h.Find_cr_stname(arg, sr, Dna());
        sp_h.Set_key_srcpos(Dna(), sr->Bb(), sr->Linenum(), name);
        sp_h.Set_msgid("UDR.1");
        ISSUE_CERTAINTY ic = VSA_Issue_Certainty_Maybe ? IC_MAYBE : IC_DEFINITELY;
        Vsa()->Report_vsa_error(arg, (char*)NULL, UDR, ic, &sp_h);
        continue;
      }
      HEAP_OBJ_REP* hor = Cr_2_heap_obj(base);
      if (hor == NULL) {
        hor = Find_cr_hor(sr, base);
        Is_True(hor != NULL, ("no hor for free arg"));
      }
      if (!Vsa()->Is_special_hor(hor)) {
        PENDING_FREE_STMT free_stmt(sr, hor, arg);
        pending_stmts.push_back(free_stmt);
      }
    }
  }

  // merge ulist after all hos been created
  Merge_ulist();

  for (INT idx = 0; idx < pending_stmts.size(); idx++) {
    PENDING_FREE_STMT &pending_free = pending_stmts[idx];
    STMTREP *sr = pending_free._sr;
    HEAP_OBJ_REP *hor = pending_free._hor;
    CODEREP *arg = pending_free._cr;

    CHI_NODE *chi = Vsa()->Find_stmt_hor_chi(sr, hor->Heap_obj());
    if (chi == NULL) {
      Vsa()->Append_stmt_hor_chi(sr, hor, arg, ROR_DEF_BY_FREE, Defbb_pool());
    } else {
      // hor chi already added by vor chi, update the attribute
      HEAP_OBJ_REP *res = ((CHOR*)chi->RESULT())->first;
      res->Set_attr(ROR_DEF_BY_FREE);
    }
    HOR_LIST *ulist = hor->Ulist();
    if (ulist != NULL) {
      // create chi for ulist
      HEAP_OBJ_REP *cur_hor;
      HOR_LIST_ITER hor_list_iter;
      FOR_ALL_NODE(cur_hor, hor_list_iter, Init(ulist)) {
        Is_True(!Vsa()->Is_special_hor(cur_hor),
                ("special hor"));
        Vsa()->Append_stmt_hor_chi(sr, cur_hor, arg, ROR_DEF_BY_FREE, Defbb_pool());
      }
    }
  }
}

template <class _Key> struct HASH_PHI { };
template <>
struct HASH_PHI<PHI_NODE> {
  size_t operator() (const PHI_NODE* phi) const {
    IDTYPE bb_id = phi->Bb()->Id();
    int kind = 0;
    IDTYPE id = 0;
    if (Phi_res_is_vor(phi)) {
      kind = 0x1;
      id = ((VSYM_OBJ_REP*)phi->RESULT())->Vsym_obj()->Id();
    }
    else if(Phi_res_is_hor(phi)) {
      kind = 0x2;
      id = ((HEAP_OBJ_REP*)phi->RESULT())->Heap_obj()->Id();
    }
    else {
      kind = 0x4;
      id = ((CODEREP*)phi->RESULT())->Coderep_id();
    }
    Is_True(bb_id <= 0x3fffffff, ("HASH_PHI key may not uniq"));
    return (((size_t)bb_id << 34) + ((size_t)kind << 32) + id);
  } 
};
// ============================================================================
// HOR_FINDER
//
// Find the ho from given CR or PHI
// ============================================================================
class HOR_FINDER {
public:
  // candidate kind
  enum CAND_KIND {
    CAND_FIXED    = 0,   // candidate is fixed
    CAND_IN_STACK = 1,   // candidate is in stack
    CAND_UNKNOWN  = 2,   // candidate is unknown
  };
  // cr/vor to its hor candidate map
  typedef pair<uintptr_t, HEAP_OBJ_REP*> PTR_CAND_PAIR;
  typedef hash_map<uintptr_t, HEAP_OBJ_REP*, __gnu_cxx::hash<uintptr_t>,
                   std::equal_to<uintptr_t>,
                   mempool_allocator<PTR_CAND_PAIR> > PTR_CAND_MAP;
  // phi to its hor candidates map
  typedef pair<PHI_NODE*, HOR_PTR_SET*> PHI_CAND_PAIR;
  typedef hash_map<PHI_NODE*, HOR_PTR_SET*, HASH_PHI<PHI_NODE>,
                   std::equal_to<PHI_NODE *>,
                   mempool_allocator<PHI_CAND_PAIR> > PHI_CAND_MAP;

  // in-stack hor
  static HEAP_OBJ_REP *In_stack_hor() { return (HEAP_OBJ_REP *)CAND_IN_STACK; }
  // unknown hor
  static HEAP_OBJ_REP *Unknown_hor()  { return (HEAP_OBJ_REP *)CAND_UNKNOWN;  }

private:
  HEAP_VSYM_ANALYSIS  *_hva;        // hva
  HOR_CACHE           *_hor_cache;  // hor cache
  PHI_CAND_MAP        *_phi_map;    // phi->hor candidates map
  PTR_CAND_MAP        *_cr_map;     // cr ->hor candidate map
  PTR_CAND_MAP        *_vor_map;    // vor->hor candidate map
  MEM_POOL             _lpool;      // local temporary pool
  BOOL                 _trace;      // trace flag

  HOR_FINDER(const HOR_FINDER&);            // no copy-ctor
  HOR_FINDER& operator=(const HOR_FINDER&); // no assign-oper

public:
  // constructor
  HOR_FINDER(HEAP_VSYM_ANALYSIS *hva, HOR_CACHE *hor_cache)
    : _hva(hva), _hor_cache(hor_cache) {
    _trace = Get_Trace(TP_VSA, VSA_VSYM_TRACE_FLAG);
    OPT_POOL_Initialize(&_lpool, "hor finder", FALSE, VSA_DUMP_FLAG);
    OPT_POOL_Push(&_lpool, VSA_DUMP_FLAG);
    _phi_map = CXX_NEW(PHI_CAND_MAP(127, HASH_PHI<PHI_NODE>(),
                                    std::equal_to<PHI_NODE*>(),
                                    mempool_allocator<PHI_CAND_PAIR>(&_lpool)),
                       &_lpool);
    _cr_map  = CXX_NEW(PTR_CAND_MAP(127, __gnu_cxx::hash<uintptr_t>(),
                                    std::equal_to<uintptr_t>(),
                                    mempool_allocator<PTR_CAND_PAIR>(&_lpool)),
                       &_lpool);
    _vor_map = CXX_NEW(PTR_CAND_MAP(127, __gnu_cxx::hash<uintptr_t>(),
                                    std::equal_to<uintptr_t>(),
                                    mempool_allocator<PTR_CAND_PAIR>(&_lpool)),
                       &_lpool);
  }

  // destructor
  ~HOR_FINDER() {
    OPT_POOL_Pop(&_lpool, VSA_DUMP_FLAG);
    OPT_POOL_Delete(&_lpool, VSA_DUMP_FLAG);
  }

private:
  // create varphi/varphi hot
  void Finalize_phi_hor(HEAP_OBJ_REP *hor, PHI_NODE *phi, BOOL need_ulist) {
    Is_True(hor->Attr() == ROR_DEF_BY_VARPHI ||
            hor->Attr() == ROR_DEF_BY_VORPHI, ("bad hor"));
    Is_True(hor->Heap_obj()->Base_phi() == phi, ("had base phi"));
    BOOL is_vor = Phi_res_is_vor(phi);
    // ulist
    if (need_ulist && (hor->Ulist()) == NULL) {
      _hva->Create_ulist(hor, _hva->Vsa()->Mem_pool());
    }
    // new hor phi node
    PHI_LIST *phi_list = _hva->Vsa()->Bb_ho_philist(phi->Bb());
    Is_True(phi_list, ("phi list is null"));
    PHI_NODE *hor_phi = _hva->New_phi_node(_hva->Ho_phi_cache(),
                                           phi_list,
                                           hor->Heap_obj()->Id(),
                                           _hva->Vsa()->Mem_pool(),
                                           phi->Bb(),
                                           TRUE);
    hor_phi->Set_live();
    hor_phi->Set_result((CODEREP *)hor);
    VSA_PHI_NODE(hor_phi).Set_res_is_hor();
    VSA_PHI_NODE(hor_phi).Set_opnd_mismatch();
    hor->Set_phi_def(hor_phi);
    // process opnds
    for (INT i = 0; i < phi->Size(); ++i) {
      HEAP_OBJ_REP *opnd;
      opnd = is_vor ? Fixup_vor_hor((VSYM_OBJ_REP *)phi->OPND(i))
                    : Fixup_var_hor(phi->OPND(i));
      Is_True(opnd != NULL, ("no hor for phi opnd %d", i));
      // set hor phi's OPND
      hor_phi->Set_opnd(i, (CODEREP *)opnd);
      if (_hva->Vsa()->Is_special_hor(opnd))
        continue;
      if (need_ulist) {
        BOOL found = _hva->Find_or_append_ulist_hor(hor, opnd);
#if 0
        // postpone unfold opnd ulist to Merge_ulist
        // may affect alias vsym UD, enable below code if needed
        if (!found) {
          _hva->Append_opnd_ulist(hor, ulist, opnd, FALSE);
        }
#endif
        // TODO: check this again. should res hor be added to opnd's ulist?
        //HOR_LIST *opnd_ulist = opnd->Ulist();
        //if (opnd_ulist == NULL) {
        //  opnd_ulist = CXX_NEW(HOR_LIST, _hva->Vsa()->Mem_pool());
        //  opnd->Set_ulist(opnd_ulist);
        //}
        //if (opnd_ulist->Find(hor) == NULL) {
        //  opnd_ulist->Append(CXX_NEW(HOR_NODE(hor), _hva->Vsa()->Mem_pool()));
        //}
      }
    }
  }

  // annotate hor to cr
  void Enter_hor(CODEREP *cr, HEAP_OBJ_REP *hor) {
    HEAP_OBJ_REP *prev;
    Is_True((prev = _hva->Cr_2_heap_obj(cr)) == NULL || prev == hor,
            ("cr hor already set"));
    Is_Trace(_trace,
             (TFile, "HOR_FINDER: annotate cr%d with ho%d.\n",
                     cr->Coderep_id(), hor->Heap_obj()->Id()));
    _hva->Enter_cr_heap_obj_map(cr, hor, FALSE);
  }

  // annotate hor to vor
  void Enter_hor(VSYM_OBJ_REP *vor, HEAP_OBJ_REP *hor) {
    Is_True(vor != NULL &&
            (vor->Hor() == NULL || vor->Hor() == hor),
            ("hor mismatch"));
    Is_True(!_hva->Vo_created(vor),
            ("annotate vor not renamed yet"));
    Is_Trace(_trace,
             (TFile, "HOR_FINDER: annotate vo%dv%d with ho%d.\n",
                     vor->Vsym_obj()->Id(), vor->Version(),
                     hor->Heap_obj()->Id()));
    if (vor->Hor() == NULL)
      vor->Set_hor(hor);
  }

  // annotate hor_cand to cr
  void Annot_hor(CODEREP *cr, HEAP_OBJ_REP *hor) {
    if (hor == Unknown_hor()) {
      _hor_cache->Cache_hor(cr, Unknown_hor());
    }
    else if (hor == In_stack_hor()) {
      _cr_map->insert(PTR_CAND_PAIR((uintptr_t)cr, In_stack_hor()));
    }
    else {
      Is_True(hor != NULL, ("invalid hor"));
      Enter_hor(cr, hor);
    }
  }

  // annotate hor_cand to vor
  void Annot_hor(VSYM_OBJ_REP *vor, HEAP_OBJ_REP *hor) {
    if (vor != NULL && !_hva->Vo_created(vor)) {
      Is_True(!_hva->Vo_created(vor),
              ("annotate vor not renamed yet"));
      if (hor == Unknown_hor()) {
        _hor_cache->Cache_hor(vor, Unknown_hor());
      }
      else if (hor == In_stack_hor()) {
        _vor_map->insert(PTR_CAND_PAIR((uintptr_t)vor, In_stack_hor()));
      }
      else {
        Is_True(hor != NULL, ("invalid hor"));
        Enter_hor(vor, hor);
      }
    }
  }

private:
   // find hor for CK_VAR
   HEAP_OBJ_REP *Is_var_fixable(CODEREP *cr) {
    Is_True(cr->Kind() == CK_VAR, ("bad var"));
    HEAP_OBJ_REP *hor = _hva->Cr_2_heap_obj(cr);
    if (hor != NULL)
      return hor;

    hor = _hor_cache->Find_hor(cr);
    if (hor == Unknown_hor())
      return Unknown_hor();

    if (cr->Is_flag_set(CF_DEF_BY_PHI)) {
      Is_True(cr->Defphi(), ("bad defphi"));
      return Is_phi_fixable<CODEREP>(cr, cr->Defphi());
    }
    else if (cr->Is_flag_set(CF_IS_ZERO_VERSION) ||
        cr->Is_var_volatile()) {
      hor = _hva->Vsa()->Default_hor();
      Enter_hor(cr, hor);
    }
    else if (cr->Is_flag_set(CF_DEF_BY_CHI)) {
      STMTREP *def = cr->Get_defstmt();
      Is_True(def, ("bad defchi"));
      BOOL create_hor = FALSE;
      if (def->Opr() == OPR_OPT_CHI) {
        create_hor = TRUE;
      } else if (OPERATOR_is_call(def->Opr())) {
        if (cr == _hva->Comp_unit()->Find_return_value(def)) {
          create_hor = TRUE;
        } else {
          RNA_NODE * rna = _hva->Vsa()->Sr_2_rna(def);
          Is_True(rna, ("nul rna for call"));
          // if cr is address passed to extern call, create hor chi
          // on the call stmt
          if (rna && rna->Callee_cnt() == 0) {
            pair<IDTYPE, BOOL> arg = rna->Get_arg(cr, _hva->Vsa());
            if (arg.first != INVALID_VAR_IDX && arg.second == TRUE) {
              create_hor = TRUE;
            }
          }
        }
      }
      if (create_hor) {
        hor = _hva->Create_heap_obj(cr, def->Bb(), _hva->Defbb_pool());
        Is_Trace(_trace,
                 (TFile, "HOR_FINDER: create ho%d on %s sr%d LINE %d.\n",
                         hor->Heap_obj()->Id(),
                         def->Opr() == OPR_OPT_CHI ? "entry" : "call",
                         def->Stmtrep_id(),
                         Srcpos_To_Line(def->Linenum())));
        hor->Set_attr(ROR_DEF_BY_CHI);
        hor->Set_stmt_def(def, _hva->Dna());
        _hva->Vsa()->Append_stmt_hor_chi(def, hor,
                                         hor->Heap_obj()->Entry_chi(), cr);
        Enter_hor(cr, hor);
        if (OPERATOR_is_call(def->Opr())) {
          hor->Heap_obj()->Set_flag(RSC_ADDR_PASSED_IN);
        }
      }
      else {
        CHI_NODE* chi = cr->Defchi();
        Is_True(chi, ("bad chinode"));
        hor = Is_var_fixable(chi->OPND());
        Annot_hor(cr, hor);
      }
    }
    else {
      STMTREP *def = cr->Get_defstmt();
      Is_True(def && def->Lhs() == cr && def->Rhs(), ("bad defstmt"));
      hor = Is_cr_fixable(def->Rhs());
      Annot_hor(cr, hor);
    }

    return hor;
  }

  // find hor for CK_IVAR
  HEAP_OBJ_REP *Is_ivar_fixable(CODEREP *cr) {
    Is_True(cr->Kind() == CK_IVAR, ("bad ivar"));
    VSYM_OBJ_REP *vor = _hva->Cr_2_vor(cr);
    HEAP_OBJ_REP *hor = _hva->Cr_2_heap_obj(cr);
    if (hor != NULL) {
      //Is_True(vor == NULL || _hva->Vo_created(vor) || vor->Hor() == hor,
      //        ("hor on vor not set"));
      return hor;
    }

    hor = _hor_cache->Find_hor(cr);
    if (hor == Unknown_hor())
      return Unknown_hor();

    if (vor == NULL) {
      CODEREP *base = Find_ilod_base(cr->Ilod_base());
      if (base != NULL) {
        hor = Is_cr_fixable(base);
        if (hor != In_stack_hor()) {
          hor = Unknown_hor();
        }
        Is_True(hor == Unknown_hor() ||
                hor == In_stack_hor() ||
                _hva->Cr_2_heap_obj(base) == hor,
                ("ilod base hor mismatch"));
      }
      else {
        hor = _hva->Vsa()->Null_hor();
      }
    }
    else {
      hor = Is_vor_fixable(cr, vor);
    }

    // add to map
    Annot_hor(cr, hor);
    return hor;
  }

  // find hor for cr
  HEAP_OBJ_REP *Is_cr_fixable(CODEREP *cr) {
    HEAP_OBJ_REP *hor;
    switch(cr->Kind()) {
    case CK_LDA:
      hor = _hva->Cr_2_heap_obj(cr);
      if (hor == NULL) {
        hor = _hva->Create_heap_obj_for_lda(cr);
        Is_Trace(_trace,
                 (TFile, "HOR_FINDER: create ho%d for LDA cr%d sym%d.\n",
                         hor->Heap_obj()->Id(),
                         cr->Coderep_id(), cr->Lda_aux_id()));
        Enter_hor(cr, hor);
      }
      return hor;
    case CK_CONST:
    case CK_RCONST:
      return _hva->Vsa()->Null_hor();
    case CK_VAR:
      return Is_var_fixable(cr);
    case CK_IVAR:
      return Is_ivar_fixable(cr);
    case CK_OP:
      if (cr->Opr() == OPR_ALLOCA) {
        hor = _hva->Cr_2_heap_obj(cr);
        Is_True(hor, ("hor not created for ALLOCA"));
        return hor ? hor : _hva->Vsa()->Null_hor();
      }
      cr = Find_ilod_base(cr);
      return cr ? Is_cr_fixable(cr) : _hva->Vsa()->Null_hor();
    default:
      Is_True(FALSE, ("unknown cr kind"));
      return _hva->Vsa()->Null_hor();
    }
  }

  // find hor for vor
  HEAP_OBJ_REP *Is_vor_fixable(CODEREP *cr, VSYM_OBJ_REP *vor) {
    Is_True(cr && cr->Kind() == CK_IVAR &&
            cr->Opr() != OPR_PARM,
            ("bad cr"));

    HEAP_OBJ_REP *hor = vor->Hor();
    if (hor != NULL)
      return hor;

    hor = _hor_cache->Find_hor(vor);
    if (hor == Unknown_hor())
      return Unknown_hor();

    Is_True(!_hva->Vsa()->Is_special_vor(vor), ("no hor for special vor"));
    // need rename to get correct hor
    if (vor->Is_entry_chi() &&
        (_hva->Vo_created(vor) || _hva->Vo_updated(vor))) {
      return Unknown_hor();
    }

    STMTREP *def;
    switch (vor->Attr()) {
    case ROR_DEF_BY_PHI:
    case ROR_DEF_BY_HORPHI:
      Is_True(vor->Phi_def(), ("bad vor phidef"));
      return Is_phi_fixable<VSYM_OBJ_REP>(cr, vor->Phi_def());

    case ROR_DEF_BY_CHI:
      // TODO: improve def_by_chi
      def = vor->Stmt_def();
      if (def == NULL) {
        Is_True(vor->Is_entry_chi() ||
                _hva->Vsa()->Is_special_vor(vor), ("chi vor defstmt is null"));
        hor = _hva->Vsa()->Null_hor();
      }
      else {
        hor = vor->Vsym_obj()->Base_hor();
        Is_True(hor, ("bad base hor"));
        if (!_hva->Vsa()->Is_special_hor(hor)) {
          if (hor->Attr() == ROR_DEF_BY_LDA ||
              hor->Attr() == ROR_DEF_BY_ALLOCA) {
            // vsym u-d reaches the LDA or ALLOCA, means the vor is not defined
            // annotate null_hor on the vor
            hor = _hva->Vsa()->Null_hor();
          }
          else if (def->Opr() == OPR_OPT_CHI ||
                   OPERATOR_is_call(def->Opr()) ||
                   def->Opr() == OPR_MSTORE) {
            Is_True(hor->Attr() != ROR_DEF_BY_ALLOCA, ("alloca hit"));
            CODEREP *def_cr = _hva->Find_stmt_vor_chi_cr(def, vor);
            Is_True(def_cr != NULL,
                    ("not find def_cr for vor"));
            VSYM_OBJ_REP *opnd_vor = _hva->Find_stmt_cur_vor(def, vor->Vsym_obj());
            if (opnd_vor && opnd_vor->Hor() && !_hva->Vsa()->Is_special_hor(opnd_vor->Hor())) {
              hor = _hva->Create_heap_obj(opnd_vor->Hor()->Heap_obj());
            } else {
              hor = _hva->Create_heap_obj(cr, def->Bb(), _hva->Defbb_pool());
            }
            hor->Set_attr(ROR_DEF_BY_CHI);
            hor->Set_stmt_def(def, _hva->Dna());
            _hva->Vsa()->Append_stmt_hor_chi(def, hor, hor->Heap_obj()->Entry_chi(), def_cr);
            Is_Trace(_trace,
                     (TFile, "HOR_FINDER: create ho%d on %s sr%d LINE %d.\n",
                             hor->Heap_obj()->Id(),
                             OPERATOR_name(def->Opr()) + 4,
                             def->Stmtrep_id(),
                             Srcpos_To_Line(def->Linenum())));
          }
          else if (def->Opr() == OPR_ISTORE) {
            Is_True(vor->Stmt_def(), ("bad defstmt"));
            hor = Is_cr_fixable(vor->Stmt_def()->Rhs());
          }
          else {
            Is_True(FALSE, ("TODO: chi def by %s", OPERATOR_name(def->Opr()) + 4));
            hor = _hva->Create_heap_obj(cr, def->Bb(), _hva->Defbb_pool());
            hor->Set_attr(ROR_DEF_BY_CHI);
            hor->Set_stmt_def(def, _hva->Dna());
            CODEREP *def_cr = _hva->Find_stmt_vor_chi_cr(def, vor);
            Is_True(def_cr != NULL,
                    ("not find def_cr for vor"));
            _hva->Vsa()->Append_stmt_hor_chi(def, hor, hor->Heap_obj()->Entry_chi(), def_cr);
          }
        }
      }
      break;

    case ROR_DEF_BY_COPY:
    case ROR_DEF_BY_ISTORE:
      Is_True(vor->Stmt_def(), ("bad defstmt"));
      hor = Is_cr_fixable(vor->Stmt_def()->Rhs());
      break;

    case ROR_DEF_BY_NONE:
    default:
      Is_True(FALSE, ("vor def by none?"));
      hor = _hva->Vsa()->Null_hor();
      break;
    }

    // add to map
    Annot_hor(vor, hor);
    return hor;
  }

  // find hor for phi
  template<typename _OBJECT>
  HEAP_OBJ_REP *Is_phi_fixable(CODEREP *cr, PHI_NODE *phi) {
    Is_True(!Phi_res_is_hor(phi), ("bad hor phi"));
    BOOL is_vor = Phi_res_is_vor(phi);
    _OBJECT *phi_res = (_OBJECT *)phi->RESULT();

    if (_hor_cache->Push_phi(phi) == FALSE) {
      return In_stack_hor();
    }
    if (_phi_map->find(phi) != _phi_map->end()) {
      _hor_cache->Pop_phi(phi);
      return In_stack_hor();
    }

    HEAP_OBJ_REP *hor = NULL;
    BOOL in_stack = FALSE;
    PHI_OPND_ITER phi_opnd_iter(phi);
    CODEREP *opnd;
    FOR_ALL_ELEM(opnd, phi_opnd_iter, Init()) {
      hor = is_vor ? Is_vor_fixable(cr, (VSYM_OBJ_REP*)opnd)
                    : Is_var_fixable(opnd);
      // fail to find hor for this operand, give up
      if (hor == Unknown_hor())
        break;
      // find a fixed candidate, add to phi's hor candidates
      if (hor != In_stack_hor()) {
        Is_True(hor != NULL, ("null hor ptr for opnd"));
        _hor_cache->Add_phi_hor(phi, hor);
      }
      else if (in_stack == FALSE) {
        // set in stack flag
        in_stack = TRUE;
      }
    }

    _hor_cache->Pop_phi(phi);
    HOR_PTR_SET *hor_cand = _hor_cache->Phi_cand(phi);

    if (hor != Unknown_hor()) {
      if (in_stack == TRUE) {
        hor = In_stack_hor();
        _phi_map->insert(PHI_CAND_PAIR(phi, hor_cand));
      }
      else if (hor_cand->size() > 1) {
        hor = _hva->Create_heap_obj(cr, phi->Bb(), _hva->Defbb_pool());
        hor->Set_attr(is_vor ? ROR_DEF_BY_VORPHI : ROR_DEF_BY_VARPHI);
        hor->Heap_obj()->Set_base_phi(phi);
        Is_Trace(_trace,
                 (TFile, "HOR_FINDER: create ho%d for %s%dv%d phi in BB%d LINE %d.\n",
                         hor->Heap_obj()->Id(),
                         is_vor ? "vo" : "sym",
                         is_vor ? ((VSYM_OBJ_REP*)phi->RESULT())->Vsym_obj()->Id()
                                : phi->RESULT()->Aux_id(),
                         is_vor ? ((VSYM_OBJ_REP*)phi->RESULT())->Version()
                                : phi->RESULT()->Version(),
                         phi->Bb()->Id(),
                         Srcpos_To_Line(phi->Bb()->Linenum())));
        Finalize_phi_hor(hor, phi, TRUE);
      }
    }
    else {
      _hor_cache->Cache_hor(phi, Unknown_hor());
    }

    // add to map
    Annot_hor(phi_res, hor);
    return hor;
  }

private:
  // fix up for for CK_VAR
  HEAP_OBJ_REP *Fixup_var_hor(CODEREP *cr) {
    Is_True(cr != NULL, ("bad cr"));
    HEAP_OBJ_REP *hor = _hva->Cr_2_heap_obj(cr);
    if (hor != NULL)
      return hor;

    Is_True(!cr->Is_flag_set(CF_IS_ZERO_VERSION) &&
            !cr->Is_var_volatile(),
            ("zero version or volatile cr"));
    Is_True(!cr->Is_flag_set(CF_DEF_BY_PHI),
            ("no hor for phi result"));

    if (cr->Is_flag_set(CF_DEF_BY_CHI)) {
      STMTREP *def = cr->Get_defstmt();
      Is_True(def && def->Opr() != OPR_OPT_CHI,
              ("bad defchi"));
      CHI_NODE* chi = cr->Defchi();
      Is_True(chi, ("bad chinode"));
      hor = Fixup_var_hor(chi->OPND());
    }
    else {
      STMTREP *def = cr->Get_defstmt();
      Is_True(def && def->Lhs() == cr && def->Rhs(), ("bad defstmt"));
      hor = Fixup_cr_hor(def->Rhs());
    }

    Is_True(hor != NULL, ("not find hor"));
    _hva->Enter_cr_heap_obj_map(cr, hor, FALSE);
    return hor;
  }

  // fix up hor for CK_IVAR
  HEAP_OBJ_REP *Fixup_ivar_hor(CODEREP *cr) {
    Is_True(cr->Kind() == CK_IVAR, ("bad ivar"));
    VSYM_OBJ_REP *vor = _hva->Cr_2_vor(cr);
    HEAP_OBJ_REP *hor = _hva->Cr_2_heap_obj(cr);

    if (hor != NULL) {
      Is_True(vor == NULL || vor->Hor() == hor,
              ("bad vor hor"));
      return hor;
    }

    if (vor == NULL) {
      CODEREP *base = Find_ilod_base(cr->Ilod_base());
      while (base->Kind() == CK_IVAR &&
             base->object_ty() != cr->object_ty()) {
        // skip intermediate iload with different type
        base = Find_ilod_base(base->Ilod_base());
      }
      Is_True(base != NULL, ("null base"));
      hor = Fixup_cr_hor(base);
      Is_True(hor != NULL, ("not find hor for base"));
      // set injured because ilod_base hor used
      hor->Set_injured();
    }
    else {
      hor = Fixup_vor_hor(vor);
    }

    Is_True(hor != NULL, ("not find hor"));
    Annot_hor(cr, hor);
    if (vor) {
      Annot_hor(vor, hor);
    }
    return hor;
  }

  // fix up hor for CODEREP
  HEAP_OBJ_REP *Fixup_cr_hor(CODEREP *cr) {
    Is_True(cr != NULL, ("bad cr"));

    switch (cr->Kind()) {
    case CK_LDA:
      Is_True(_hva->Cr_2_heap_obj(cr) != NULL, ("no hor for lda"));
      return _hva->Cr_2_heap_obj(cr);
    case CK_CONST:
    case CK_RCONST:
      return _hva->Vsa()->Null_hor();
    case CK_VAR:
      return Fixup_var_hor(cr);
    case CK_IVAR:
      return Fixup_ivar_hor(cr);
    case CK_OP:
      cr = Find_ilod_base(cr);
      return cr ? Fixup_cr_hor(cr)
                : _hva->Vsa()->Null_hor();
    default:
      Is_True(FALSE, ("unknown cr kind"));
      return NULL;
    }
  }

  // fix up hor for VSYM_OBJ_REP which can't be PHI result
  HEAP_OBJ_REP *Fixup_vor_hor(VSYM_OBJ_REP *vor) {
    Is_True(vor != NULL, ("bad vor"));
    HEAP_OBJ_REP *hor = vor->Hor();
    if (hor != NULL)
      return hor;

    Is_True(!_hva->Vsa()->Is_special_vor(vor),
            ("special vor"));

    switch (vor->Attr()) {
    case ROR_DEF_BY_PHI:
    case ROR_DEF_BY_HORPHI:
      Is_True(FALSE, ("no hor for phi result"));
      return NULL;
    case ROR_DEF_BY_CHI:
    {
      STMTREP *def = vor->Stmt_def();
      // aliased vor def by Istore, find rhs hor from Rhs
      if (def && def->Opr() == OPR_ISTORE) {
        hor = Fixup_cr_hor(vor->Stmt_def()->Rhs());
        Is_True(hor != NULL, ("no hor for cr"));
        vor->Set_hor(hor);
        return hor;
      }
      Is_True(FALSE, ("no hor for chi result"));
      return NULL;
    }
    case ROR_DEF_BY_COPY:
    case ROR_DEF_BY_ISTORE:
      Is_True(vor->Stmt_def(), ("bad defstmt"));
      hor = Fixup_cr_hor(vor->Stmt_def()->Rhs());
      Is_True(hor != NULL, ("no hor for cr"));
      vor->Set_hor(hor);
      return hor;
    case ROR_DEF_BY_NONE:
    default:
      Is_True(FALSE, ("vor def by none?"));
      return NULL;
    }
  }

  // fix up hor for all IN_STACK candidates
  HEAP_OBJ_REP *Fixup_hor(CODEREP *cr) {
    // fix up hor for phi result
    for (PHI_CAND_MAP::iterator it = _phi_map->begin();
         it != _phi_map->end(); ++it) {
      PHI_NODE *phi = (PHI_NODE *)it->first;
      HOR_PTR_SET *cand = it->second;
      BOOL is_vor = Phi_res_is_vor(phi);
      Is_True((is_vor && ((VSYM_OBJ_REP*)phi->RESULT())->Hor() == NULL) ||
              (!is_vor && _hva->Cr_2_heap_obj(phi->RESULT()) == NULL),
              ("result hor already created"));
      Is_True(phi && cand && cand->size() > 0,
              ("not find cand for phi"));

      HEAP_OBJ_REP *hor;
      if (cand->size() == 1) {
        // unique candidate, reuse the hor
        // TODO: but, should create a new version here?
        hor = *(cand->begin());
        // no hor phi node at this stage
        Is_Trace(_trace,
                 (TFile, "HOR_FINDER: reuse ho%dv%d for %s%dv%d phi in BB%d LINE %d.\n",
                         hor->Heap_obj()->Id(), hor->Version(),
                         is_vor ? "vo" : "sym",
                         is_vor ? ((VSYM_OBJ_REP*)phi->RESULT())->Vsym_obj()->Id()
                                : phi->RESULT()->Aux_id(),
                         is_vor ? ((VSYM_OBJ_REP*)phi->RESULT())->Version()
                                : phi->RESULT()->Version(),
                         phi->Bb()->Id(),
                         Srcpos_To_Line(phi->Bb()->Linenum())));
        hor->Set_injured();
      }
      else {
        // multiple candidates, create a new hor
        hor = _hva->Create_heap_obj(NULL, phi->Bb(), _hva->Defbb_pool());
        hor->Set_attr(is_vor ? ROR_DEF_BY_VORPHI : ROR_DEF_BY_VARPHI);
        hor->Heap_obj()->Set_base_phi(phi);
        // no hor phi node at this stage
        Is_Trace(_trace,
                 (TFile, "HOR_FINDER: create ho%d for %s%dv%d phi in BB%d LINE %d.\n",
                         hor->Heap_obj()->Id(),
                         is_vor ? "vo" : "sym",
                         is_vor ? ((VSYM_OBJ_REP*)phi->RESULT())->Vsym_obj()->Id()
                                : phi->RESULT()->Aux_id(),
                         is_vor ? ((VSYM_OBJ_REP*)phi->RESULT())->Version()
                                : phi->RESULT()->Version(),
                         phi->Bb()->Id(),
                         Srcpos_To_Line(phi->Bb()->Linenum())));
      }
      if (is_vor) {
        Annot_hor((VSYM_OBJ_REP*)phi->RESULT(), hor);
      }
      else {
        Annot_hor(phi->RESULT(), hor);
      }
    }

    // fix up hor for vor
    PTR_CAND_MAP::iterator it;
    for (it = _vor_map->begin(); it != _vor_map->end(); ++it) {
      VSYM_OBJ_REP *vor = (VSYM_OBJ_REP *)it->first;
      Is_True(it->second != Unknown_hor(), ("unknown hor"));
      if (vor->Attr() == ROR_DEF_BY_PHI) {
        Is_True(vor->Hor() != NULL,
                ("phi result not fixed"));
        continue;
      }
      Fixup_vor_hor(vor);
    }

    // fix up hor for cr
    for (it = _cr_map->begin(); it != _cr_map->end(); ++it) {
      CODEREP *var = (CODEREP *)it->first;
      Is_True(it->second != Unknown_hor(), ("unknown hor"));
      if (cr->Is_flag_set(CF_DEF_BY_PHI)) {
        Is_True(_hva->Cr_2_heap_obj(cr) != NULL,
                ("phi result not fixed"));
        continue;
      }
      Fixup_cr_hor(cr);
    }

    // update phi opnd hor and create ulist for phi result
    for (PHI_CAND_MAP::iterator it = _phi_map->begin();
         it != _phi_map->end(); ++it) {
      PHI_NODE *phi = (PHI_NODE *)it->first;
      BOOL is_vor = Phi_res_is_vor(phi);
      HEAP_OBJ_REP *hor = is_vor ? ((VSYM_OBJ_REP *)phi->RESULT())->Hor()
                                 : _hva->Cr_2_heap_obj(phi->RESULT());
      Is_True(hor != NULL, ("no hor for phi result"));
      // hor is reused for phi result above, so need to check base phi
      if ((hor->Attr() == ROR_DEF_BY_VARPHI ||
           hor->Attr() == ROR_DEF_BY_VORPHI) &&
          hor->Heap_obj()->Base_phi() == phi) {
        Finalize_phi_hor(hor, phi, it->second->size() > 1);
      }
    }

    HEAP_OBJ_REP *hor = _hva->Cr_2_heap_obj(cr);
    Is_True(hor != NULL, ("find find hor for cr"));
    return hor;
  }

public:
  HEAP_OBJ_REP *Find_cr_hor(CODEREP *cr) {
    HEAP_OBJ_REP *res = Is_cr_fixable(cr);
    if (res == Unknown_hor()) {
      return NULL;
    }
    else if (res != In_stack_hor()) {
      Is_True(res != NULL, ("null hor ptr"));
      return res;
    }
    else {
      return Fixup_hor(cr);
    }
  }

}; // HOR_FINDER

// ============================================================================
// HVA_HO_RENAMING
//
// Rename heap_obj
// ============================================================================
class HVA_HO_RENAMING : public CFG_VISITOR_BASE<HVA_HO_RENAMING> {
public:
  // set visit flag
  enum {
    // visit LDA, CONST, VAR, IVAR and OP CODEREP
    CR_VFLAG = V_LDA | V_CONST | V_VAR | V_IVAR | V_OP,
    // visit store and call STMTREP
    SR_VFLAG = V_ANY_STORE | V_ANY_CALL | V_ANY_BRANCH | V_OPT_CHI | V_RETURN | V_ASM_STMT,
  };

  HVA_HO_RENAMING(HEAP_VSYM_ANALYSIS *hva)
   : CFG_VISITOR_BASE(hva, this) { }

private:
  void
  Update_ulist_w_ho_rename(HEAP_OBJ_REP *hor, HEAP_OBJ_REP *prev, STMTREP *sr,
                           CODEREP *cr, ROR_ATTR attr);

  void
  Update_ulist_w_ho_rename_rev(HEAP_OBJ_REP *hor, STMTREP *sr);

  template<BOOL _FWD> void
  Process_chi(STMTREP* sr);

  template<BOOL _FWD> void
  Process_call(STMTREP* sr, BOOL handle_rhs);

  template<BOOL _FWD> void
  Process_stid(STMTREP* sr, BOOL handle_rhs);

  template<BOOL _FWD> void
  Process_istore(STMTREP* sr, BOOL handle_rhs);

public:
  BOOL Visit_stmt(STMTREP *sr) { return TRUE; }
  // rename phi when entering the bb
  void Enter_bb(BB_NODE* bb) {
    Is_True(bb != NULL, ("invalid bb"));

    PHI_LIST* phi_list = Vsa()->Bb_ho_philist(bb);
    if (!phi_list)
      return;
    PHI_NODE     *phi;
    PHI_LIST_ITER phi_iter;
    FOR_ALL_ELEM (phi, phi_iter, Init(phi_list)) {
      HEAP_OBJ_REP *hor = (HEAP_OBJ_REP*)phi->RESULT();
      Is_True(hor, ("invalid hor phi result"));
      Is_True(!Vsa()->Is_special_hor(hor), ("special hor"));

      // update ulist if this hor is defined by varphi or vorphi
      if (hor->Attr() == ROR_DEF_BY_VARPHI ||
          hor->Attr() == ROR_DEF_BY_VORPHI) {
        Is_True(hor->Ulist(), ("var/vor phi ulist should be created before rename"));
      }
      else {
        Is_True(hor->Attr() == ROR_DEF_BY_PHI,
                ("not def by phi"));
        HEAP_OBJ_REP *tos = hor->Heap_obj()->Top_of_stack();
        HOR_LIST *tos_ulist = tos->Ulist();
        if (tos_ulist != NULL && hor != tos) {
          HOR_LIST *ulist = hor->Ulist();
          if (ulist == NULL) {
            _hva->Clone_ulist(hor, tos, Vsa()->Mem_pool());
          }
        }
      }
      // push hor to stack
      IDTYPE version = _hva->Gen_name(hor, NULL);
      Is_True(hor->Phi_def() == phi, ("phi def mismatch"));
      Is_Trace(Tracing(), (TFile, "HOR[%d]: push hor phi result ", _hva->Round()));
      Is_Trace_cmd(Tracing(), hor->Print(TFile));
      Is_Trace(Tracing(), (TFile, " in BB%d.\n", bb->Id()));
    }

    // update hor on var phi result
    phi_list = bb->Phi_list();
    FOR_ALL_ELEM (phi, phi_iter, Init(phi_list)) {
      if (phi->Live()) {
        CODEREP *res = phi->RESULT();
        HEAP_OBJ_REP *hor = _hva->Vsa()->Cr_2_heap_obj(res);
        if (hor != NULL && !_hva->Vsa()->Is_special_hor(hor)) {
          HEAP_OBJ_REP *tos = hor->Heap_obj()->Top_of_stack();
          if (tos != hor)
            _hva->Enter_cr_heap_obj_map(res, tos, TRUE);
        }
      }
    }

    // update hor on vor phi list
    phi_list = _hva->Vsa()->Bb_vo_philist(bb);
    FOR_ALL_ELEM (phi, phi_iter, Init(phi_list)) {
      if (phi->Live()) {
        VSYM_OBJ_REP *res = (VSYM_OBJ_REP *)phi->RESULT();
        HEAP_OBJ_REP *hor = res->Hor();
        if (hor != NULL && !_hva->Vsa()->Is_special_hor(hor)) {
          HEAP_OBJ_REP *tos = hor->Heap_obj()->Top_of_stack();
          if (tos != hor)
            res->Set_hor(tos);
        }
      }
    }
  }

  // rename succ's phi operand before entering dom bb
  void Enter_dom_bb(BB_NODE* bb) {
    Is_True(bb != NULL, ("invalid bb"));

    BB_NODE     *succ;
    BB_LIST_ITER bb_iter;
    FOR_ALL_ELEM (succ, bb_iter, Init(bb->Succ())) {
      PHI_LIST* phi_list = Vsa()->Bb_ho_philist(succ);
      if (!phi_list)
        continue;
      INT pos = succ->Pred()->Pos(bb);
      Is_True(pos != -1, ("invalid pos %d", pos));
      PHI_NODE     *phi;
      PHI_LIST_ITER phi_iter;
      FOR_ALL_ELEM (phi, phi_iter, Init(phi_list)) {
        HEAP_OBJ_REP *res = (HEAP_OBJ_REP *) phi->RESULT();
        HEAP_OBJ_REP *opnd = Phi_opnd_mismatch(phi)
                               ? (HEAP_OBJ_REP *) phi->OPND(pos)
                               : res;
        Is_True(opnd != NULL, ("found null hor phi opnd"));
        if (Vsa()->Is_special_hor(opnd))
          continue;

        HEAP_OBJ_REP *tos = opnd->Heap_obj()->Top_of_stack();
        Is_True(((tos->Attr() == ROR_DEF_BY_PHI ||
                  tos->Attr() == ROR_DEF_BY_VARPHI ||
                  tos->Attr() == ROR_DEF_BY_VORPHI) &&
                 tos->Phi_def() != NULL) ||
                tos->Stmt_def() != NULL ||
                tos->Is_entry_chi(),
                ("hor not defined"));
        phi->Set_opnd(pos, (CODEREP *)tos);
        Is_Trace(Tracing(),
                 (TFile, "HOR[%d]: update opnd %d for hor ",
                         _hva->Round(), pos));
        Is_Trace_cmd(Tracing(), opnd->Print(TFile));
        Is_Trace(Tracing(), (TFile, " to hor "));
        Is_Trace_cmd(Tracing(), tos->Print(TFile));
        Is_Trace(Tracing(), (TFile, " from BB%d to BB%d.\n",
                                    bb->Id(), succ->Id()));
        if (!Phi_opnd_mismatch(phi)) {
          // normal hor phi, add tos's ulist to res
          HOR_LIST *ulist = tos->Ulist();
          if (ulist != NULL) {
            HOR_LIST *res_ulist = res->Ulist();
            if (res_ulist == NULL) {
              _hva->Clone_ulist(res, tos, Vsa()->Mem_pool());
            }
          }
        }
      }
    }
  }

  // pop phi result from renaming stack before exits the bb
  void Exit_bb(BB_NODE* bb) {
    Is_True(bb != NULL, ("invalid bb"));

    PHI_LIST* phi_list = Vsa()->Bb_ho_philist(bb);
    if (!phi_list)
      return;
    PHI_NODE     *phi;
    PHI_LIST_ITER phi_iter;
    FOR_ALL_ELEM (phi, phi_iter, Init(phi_list)) {
      HEAP_OBJ_REP *hor = (HEAP_OBJ_REP *) phi->RESULT();
      //if (hor->Attr() == ROR_DEF_BY_VARPHI ||
      //    hor->Attr() == ROR_DEF_BY_VORPHI) {
        Is_True(!Vsa()->Is_special_hor(hor), ("special hor"));
        Is_True(hor == hor->Heap_obj()->Top_of_stack(), ("stack mismatch"));
        hor->Heap_obj()->Pop();
        Is_Trace(Tracing(), (TFile, "HOR[%d]: pop hor phi result ", _hva->Round()));
        Is_Trace_cmd(Tracing(), hor->Print(TFile));
        Is_Trace(Tracing(), (TFile, " in BB%d.\n", bb->Id()));
      //}
    }
  }

  // forward process the stmtrep
  void Process_stmt_fwd(STMTREP* sr);

  // backward process the stmtrep
  void Process_stmt_rev(STMTREP* sr);

  // process the rhs of the stmtrep
  template<OPERATOR opr, BOOL _FWD>
  void Process_sr(STMTREP* sr) {
    Is_True(sr->Lhs() == NULL, ("TODO: %s", OPERATOR_name(sr->Opr()) + 4));
    if (sr->Rhs())
      Process_coderep<void>(sr, sr->Rhs(), FALSE);
  }

  // process coderep
  template<CODEKIND kind>
  void Process_cr(STMTREP* sr, CODEREP* cr, UINT flag) { }

  // perform HEAP_OBJ_REP renaming phase
  void Perform() {
    DOM_WALKER<HVA_HO_RENAMING> walker(_comp_unit->Cfg(), this);
    walker.Perform();
  }

  // Initialize HEAP_OBJ_REP renaming phase
  void Initialize();

  // finalize HEAP_OBJ_REP renaming phase
  void Finalize();
};

// Update ulist and rename (push) all hos in hor list
void
HVA_HO_RENAMING::Update_ulist_w_ho_rename(HEAP_OBJ_REP *hor, HEAP_OBJ_REP *prev, STMTREP *sr,
                                            CODEREP *cr, ROR_ATTR attr)
{
  HOR_LIST *hor_list = prev->Ulist();
  if (hor_list == NULL)
    return;

#ifdef Is_True_On
  hash_set<uintptr_t> hor_set;
#endif
  _hva->Create_ulist(hor, Vsa()->Mem_pool());
  HOR_LIST_ITER hor_list_iter;
  HEAP_OBJ_REP *cur_hor;
  FOR_ALL_NODE(cur_hor, hor_list_iter, Init(hor_list)) {
    if (cur_hor->Heap_obj() == hor->Heap_obj() ||
        Vsa()->Is_special_hor(cur_hor))
      continue;
    if (cur_hor->Attr() == ROR_DEF_BY_LDA ||
        cur_hor->Attr() == ROR_DEF_BY_ALLOCA)
      continue;
#ifdef Is_True_On
    Is_True(hor->Heap_obj() != cur_hor->Heap_obj(),
            ("ho itself in hor ulist"));
    Is_True(hor_set.find((uintptr_t)cur_hor) == hor_set.end(),
            ("Update_ulist_w_ho_rename: ho added again"));
    hor_set.insert((uintptr_t)cur_hor);
#endif
    HEAP_OBJ_REP *new_hor;
    HEAP_OBJ_REP *tos_hor = cur_hor->Heap_obj()->Top_of_stack();
    CHI_NODE *chi = Vsa()->Find_stmt_hor_chi(sr, cur_hor->Heap_obj());
    if (chi != NULL) {
      CHOR *chi_opnd = (CHOR*)chi->OPND();
      chi_opnd->first = tos_hor;
      CHOR *chi_res = (CHOR*)chi->RESULT();
      new_hor = chi_res->first;
      Is_True((new_hor->Attr() == ROR_DEF_BY_CHI ||
               new_hor->Attr() == ROR_DEF_BY_FREE) &&
              new_hor->Stmt_def() == sr,
              ("attr or stmt_def mismatch"));
    }
    else {
      new_hor = Vsa()->Clone_heap_obj(tos_hor, sr->Bb(), Vsa()->Mem_pool());
      new_hor->Set_attr(attr);
      new_hor->Set_srcpos_node(sr, Dna(), PATHINFO_PARM);
      new_hor->Set_stmt_def(sr, Dna());
      new_hor->Set_prev_ver(cur_hor);
      Is_True(Vsa()->Find_stmt_hor_chi(sr, tos_hor->Heap_obj()) == NULL,
              ("hor chi already added"));
      Vsa()->Append_stmt_hor_chi(sr, new_hor, tos_hor, cr);
    }
    new_hor->Gen_name(sr);
    _hva->Find_or_append_ulist_hor(hor, new_hor);
    Is_Trace(Tracing(),
             (TFile, "Update_ulist_w_ho_rename: push hor "));
    Is_Trace_cmd(Tracing(), new_hor->Print(TFile));
    Is_Trace(Tracing(), (TFile, " in the ulist of "));
    Is_Trace_cmd(Tracing(), hor->Print(TFile));
    Is_Trace(Tracing(), (TFile, " in sr%d %s\n",
                         sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4));
  }
}

// Pop all hoos in hor list
void
HVA_HO_RENAMING::Update_ulist_w_ho_rename_rev(HEAP_OBJ_REP *hor, STMTREP *sr)
{
  HOR_LIST *hor_list = hor->Ulist();
  if (hor_list == NULL)
    return;

  HOR_LIST_ITER hor_list_iter;
  HEAP_OBJ_REP *cur_hor;
  FOR_ALL_NODE(cur_hor, hor_list_iter, Init(hor_list)) {
    if (Vsa()->Is_special_hor(cur_hor) ||
        cur_hor->Attr() == ROR_DEF_BY_LDA ||
        cur_hor->Attr() == ROR_DEF_BY_ALLOCA)
      continue;
    if (cur_hor->Heap_obj()->Top_match_sr(sr)) {
      Is_Trace(Tracing(),
               (TFile, "Update_ulist_w_ho_rename_rev: pop hor "));
      Is_Trace_cmd(Tracing(),
                   cur_hor->Heap_obj()->Top_of_stack()->Print(TFile));
      Is_Trace(Tracing(), (TFile, " in the ulist of "));
      Is_Trace_cmd(Tracing(), hor->Print(TFile));
      Is_Trace(Tracing(), (TFile, " in sr%d %s\n",
                           sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4));

      cur_hor->Heap_obj()->Pop();
    }
  }
}

// Process_chi<TRUE>: forward pass to rename chi
template<> void
HVA_HO_RENAMING::Process_chi<TRUE>(STMTREP* sr)
{
  Is_True(sr &&
          (sr->Opr() == OPR_OPT_CHI ||
           OPERATOR_is_store(sr->Opr()) ||
           OPERATOR_is_call(sr->Opr())), ("invalid sr"));

  CHI_LIST *chi_list = Vsa()->Stmt_hor_chi(sr);
  pair<CODEREP *, CODEREP *> arg_pair = _hva->Callee_frees_heap_memory(sr);
  CODEREP *free_arg = arg_pair.first;
  if (chi_list != NULL) {
    CHI_NODE *chi;
    CHI_LIST_ITER chi_iter;
    FOR_ALL_NODE(chi, chi_iter, Init(chi_list)) {
      CHOR *copnd = (CHOR *)chi->OPND();
      CHOR *cres = (CHOR*)chi->RESULT();
      HEAP_OBJ_REP *hor = cres->first;
      CODEREP *cr = cres->second;
      Is_True(hor->Heap_obj() == copnd->first->Heap_obj(),
              ("Heap obj mismatch"));
      Is_True(cr && cr == copnd->second,
              ("cr mismatch"));
      // skip rename for free_arg, already processed in Process_call
      if (cr == free_arg || cr == arg_pair.second) {
        continue;
      }
      //if (_hva->Visit_heap_obj(hor)) {
        HEAP_OBJ_REP* tos = hor->Heap_obj()->Top_of_stack();
        copnd->first = tos;
        _hva->Gen_name(hor, sr);
        Is_Trace(Tracing(), (TFile, "HOR[%d]: push hor ", _hva->Round()));
        Is_Trace_cmd(Tracing(), hor->Print(TFile));
        Is_Trace(Tracing(),
                 (TFile, " to chi cr%d in sr%d %s.\n",
                         cr->Coderep_id(), sr->Stmtrep_id(),
                         OPERATOR_name(sr->Opr()) + 4));
      //}
    }
  }
}

// Process_chi<FALSE>: forward pass to rename chi
template<> void
HVA_HO_RENAMING::Process_chi<FALSE>(STMTREP* sr)
{
  Is_True(sr &&
          (sr->Opr() == OPR_OPT_CHI ||
           OPERATOR_is_store(sr->Opr()) ||
           OPERATOR_is_call(sr->Opr())), ("invalid sr"));

  CHI_LIST *chi_list = Vsa()->Stmt_hor_chi(sr);
  pair<CODEREP *, CODEREP *> arg_pair = _hva->Callee_frees_heap_memory(sr);
  CODEREP* free_arg = arg_pair.first;
  if (chi_list != NULL) {
    CHI_NODE *chi;
    CHI_LIST_ITER chi_iter;
    FOR_ALL_NODE(chi, chi_iter, Init(chi_list)) {
      CHOR *copnd = (CHOR *)chi->OPND();
      CHOR *cres = (CHOR*)chi->RESULT();
      HEAP_OBJ_REP *hor = cres->first;
      CODEREP *cr = cres->second;
      Is_True(!Vsa()->Is_special_hor(hor),
              ("TODO: special hor?"));
      Is_True(hor->Heap_obj() == copnd->first->Heap_obj(),
              ("Heap obj mismatch"));
      Is_True(cr && cr == copnd->second,
              ("cr mismatch"));
      // skip rename for free_arg/lda_arg, already processed in Process_call
      if (cr == free_arg || cr == arg_pair.second) {
        continue;
      }
      if (hor->Heap_obj()->Top_match_sr(sr)) {
        hor->Heap_obj()->Pop();
        Is_Trace(Tracing(), (TFile, "HOR[%d]: pop hor chi result ", _hva->Round()));
        Is_Trace_cmd(Tracing(), hor->Print(TFile));
        Is_Trace(Tracing(),
                 (TFile, " from chi cr%d in sr%d %s.\n",
                         cr->Coderep_id(), sr->Stmtrep_id(),
                         OPERATOR_name(sr->Opr()) + 4));
      }
    }
  }
}

// Process_call<TRUE>: forward pass to rename call
template<> void
HVA_HO_RENAMING::Process_call<TRUE>(STMTREP* sr, BOOL handle_rhs)
{
  Is_True(sr && OPERATOR_is_call(sr->Opr()), ("invalid call sr"));

  CODEREP* rhs = sr->Rhs();
  if (handle_rhs)
    Process_coderep<void>(sr, rhs, FALSE);

  pair<CODEREP *, CODEREP *> arg_pair = _hva->Callee_frees_heap_memory(sr);
  CODEREP *arg = arg_pair.first;
  if (arg != NULL) {
    HEAP_OBJ_REP* hor = Vsa()->Cr_2_heap_obj(arg);
    if (hor != NULL && !Vsa()->Is_special_hor(hor)) {
      HEAP_OBJ_REP* tos = hor->Heap_obj()->Top_of_stack();
      hor = Vsa()->Update_stmt_hor_chi(sr, tos, arg);
      // perform hor unification
      if (VSA_Hor_Unification || VSA_New_HVA_Unification) {
        Update_ulist_w_ho_rename(hor, tos, sr, arg, ROR_DEF_BY_FREE);
      }

      // if free set by lda annoation, process vor chi: if vor has chi on sr,
      // add hor chi for vor rhs
      // Note: remove the code when heap checker changes to traverse by var/vsym UD 
      //       (currently it is traversed by hor UD)
      if (arg_pair.second) {
        CHI_LIST *chi_list = Vsa()->Stmt_vor_chi(sr);
        if (chi_list) {
          CHI_NODE     *cnode;
          CHI_LIST_ITER chi_iter;
          FOR_ALL_NODE (cnode, chi_iter, Init(chi_list)) {
            CVOR* cvor_opnd = (CVOR*)cnode->OPND();
            VSYM_OBJ_REP* vor_opnd = cvor_opnd->first;
            HEAP_OBJ_REP *rhs_hor = vor_opnd->Hor();
            if (rhs_hor && !Vsa()->Is_special_hor(rhs_hor)) {
              CHI_NODE *chi = Vsa()->Find_stmt_hor_chi(sr, rhs_hor->Heap_obj());
              if (chi == NULL) {
                HEAP_OBJ_REP *res_hor = Vsa()->Append_stmt_hor_chi(sr, 
                                                                   rhs_hor->Heap_obj()->Top_of_stack(),
                                                                   arg_pair.second,
                                                                   ROR_DEF_BY_CHI,
                                                                   Defbb_pool());
                res_hor->Gen_name(sr);
              } else {
                HEAP_OBJ_REP *res_hor = ((CHOR*)chi->RESULT())->first;
                res_hor->Gen_name(sr);
              }
            }
          }
        }
      }

      if (!Vsa()->Callee_returns_new_heap_memory(sr))
        Vsa()->Enter_cr_heap_obj_map(sr->Rhs(), hor);
      if (VSA_New_HVA_Compat) {
        hor->Set_prev_ver(tos);
        Vsa()->Enter_cr_heap_obj_refmap(sr->Rhs(), tos);
      }
    }
  }

  if (Vsa()->Callee_returns_new_heap_memory(sr)) {
    HEAP_OBJ_REP* hor = Vsa()->Cr_2_heap_obj(rhs);
    if (hor != NULL && !Vsa()->Is_special_hor(hor)) {
      _hva->Gen_name(hor, sr);
      Is_Trace(Tracing(), (TFile, "HOR[%d]: push hor ", _hva->Round()));
      Is_Trace_cmd(Tracing(), hor->Print(TFile));
      Is_Trace(Tracing(),
               (TFile, " to malloc CALL cr%d in sr%d %s.\n",
                       rhs->Coderep_id(), sr->Stmtrep_id(),
                       OPERATOR_name(sr->Opr()) + 4));

      CODEREP* ret = Comp_unit()->Find_return_value(sr);
      if (ret != NULL)
        Vsa()->Enter_cr_heap_obj_map(ret, hor);
    }
  }

  // process chi but skip chi cr which is free arg
  Process_chi<TRUE>(sr);

}

// Process_call<FALSE>: reverse pass to rename CALL
template<> void
HVA_HO_RENAMING::Process_call<FALSE>(STMTREP* sr, BOOL handle_rhs)
{
  Is_True(sr && OPERATOR_is_call(sr->Opr()), ("invalid call sr"));

  pair<CODEREP *, CODEREP *> arg_pair = _hva->Callee_frees_heap_memory(sr);
  CODEREP *arg = arg_pair.first;
  if (arg != NULL) {
    HEAP_OBJ_REP* hor = Vsa()->Find_stmt_hor_chi(sr, arg);
    if (hor != NULL &&
        !Vsa()->Is_special_hor(hor)) {
      if ((VSA_Hor_Unification || VSA_New_HVA_Unification) &&
          hor->Heap_obj()->Top_match_sr(sr)) {
        Update_ulist_w_ho_rename_rev(hor, sr);
      }
      if (hor->Heap_obj()->Top_match_sr(sr)) {
        hor->Heap_obj()->Pop();
        Is_Trace(Tracing(), (TFile, "HOR[%d]: pop hor free param ", _hva->Round()));
        Is_Trace_cmd(Tracing(), hor->Print(TFile));
        Is_Trace(Tracing(),
                 (TFile, " from param cr%d in sr%d %s.\n",
                         arg->Coderep_id(), sr->Stmtrep_id(),
                         OPERATOR_name(sr->Opr()) + 4));
      }

      // if free set by lda annoation, process vor chi: if vor has chi on sr,
      // add hor chi for vor rhs
      // Note: remove the code when heap checker changes to traverse by var/vsym UD 
      //       (currently it is traversed by hor UD)
      if (arg_pair.second) {
        CHI_LIST *chi_list = Vsa()->Stmt_vor_chi(sr);
        if (chi_list) {
          CHI_NODE     *cnode;
          CHI_LIST_ITER chi_iter;
          FOR_ALL_NODE (cnode, chi_iter, Init(chi_list)) {
            CVOR* cvor_opnd = (CVOR*)cnode->OPND();
            VSYM_OBJ_REP* vor_opnd = cvor_opnd->first;
            HEAP_OBJ_REP *rhs_hor = vor_opnd->Hor();
            if (rhs_hor && !Vsa()->Is_special_hor(rhs_hor)) {
              if (rhs_hor->Heap_obj()->Top_match_sr(sr)) {
                rhs_hor->Heap_obj()->Pop();
              }
            }
          }
        }
      }
    }
  }

  HEAP_OBJ_REP* hor = Vsa()->Cr_2_heap_obj(sr->Rhs());
  if (hor != NULL &&
      !Vsa()->Is_special_hor(hor) &&
      hor->Heap_obj()->Top_match_sr(sr)) {
    hor->Heap_obj()->Pop();
    Is_Trace(Tracing(), (TFile, "HOR[%d]: pop hor call rhs ", _hva->Round()));
    Is_Trace_cmd(Tracing(), hor->Print(TFile));
    Is_Trace(Tracing(),
             (TFile, " from rhs of sr%d %s.\n",
                     sr->Stmtrep_id(),
                     OPERATOR_name(sr->Opr()) + 4));
  }

  CODEREP* ret = Comp_unit()->Find_return_value(sr);
  if (ret != NULL && TY_kind(ret->object_ty()) == KIND_POINTER) {
    hor = Vsa()->Cr_2_heap_obj(ret);
    if (hor != NULL &&
        !Vsa()->Is_special_hor(hor) &&
        hor->Heap_obj()->Top_match_sr(sr)) {
      hor->Heap_obj()->Pop();
      Is_Trace(Tracing(), (TFile, "HOR[%d]: pop hor call retval ", _hva->Round()));
      Is_Trace_cmd(Tracing(), hor->Print(TFile));
      Is_Trace(Tracing(),
               (TFile, " from retval cr%d of sr%d %s.\n",
                       ret->Coderep_id(), sr->Stmtrep_id(),
                       OPERATOR_name(sr->Opr()) + 4));
    }
  }

  // process chi but skip chi cr which is free arg
  Process_chi<FALSE>(sr);
}

template<> void
HVA_HO_RENAMING::Process_istore<TRUE>(STMTREP* sr, BOOL handle_rhs)
{
  Is_True(sr &&
          (sr->Opr() == OPR_ISTORE || sr->Opr() == OPR_MSTORE),
          ("invalis istore"));

  // rename rhs
  Process_coderep<void>(sr, sr->Rhs(), 0);

  // rename lhs
  CODEREP *lhs = sr->Lhs();
  Process_coderep<void>(sr, lhs, 0);

  // propagate rhs hor to lhs if missing
  HEAP_OBJ_REP *hor = Vsa()->Cr_2_heap_obj(lhs);
  if (hor == NULL) {
    CODEREP *rhs_base = Find_ilod_base(sr->Rhs());
    HEAP_OBJ_REP *rhs_hor;
    if (rhs_base != NULL &&
        (rhs_hor = Vsa()->Cr_2_heap_obj(rhs_base)) != NULL) {
      if (!Vsa()->Is_special_hor(rhs_hor))
        rhs_hor = rhs_hor->Heap_obj()->Top_of_stack();
      Vsa()->Enter_cr_heap_obj_map(lhs, rhs_hor);
      Is_Trace(Tracing(), (TFile, "HOR[%d]: Annotate hor ", _hva->Round()));
      Is_Trace_cmd(Tracing(), rhs_hor->Print(TFile));
      Is_Trace(Tracing(),
               (TFile, " to cr%d in sr%d %s line %d.\n",
                       lhs->Coderep_id(), sr->Stmtrep_id(),
                       OPERATOR_name(sr->Opr()) + 4,
                       Srcpos_To_Line(sr->Linenum())));
    }
  }

  // process hor chi annotated on this stmtrep
  HVA_HO_RENAMING::Process_chi<TRUE>(sr);
}

template<> void
HVA_HO_RENAMING::Process_istore<FALSE>(STMTREP* sr, BOOL handle_rhs)
{
  Is_True(sr &&
          (sr->Opr() == OPR_ISTORE || sr->Opr() == OPR_MSTORE),
          ("invalis istore"));

  // process hor chi annotated on this stmtrep
  HVA_HO_RENAMING::Process_chi<FALSE>(sr);

  // rename lhs
  HEAP_OBJ_REP *hor = Vsa()->Cr_2_heap_obj(sr->Lhs());
  if (hor &&
      hor->Attr() == ROR_DEF_BY_ALLOCA &&
      hor->Stmt_def() == sr) {
    Is_True(hor->Heap_obj()->Top_match_sr(sr), ("hor tos mismatch"));
    hor->Heap_obj()->Pop();
    Is_Trace(Tracing(), (TFile, "HOR[%d]: pop hor istore lhs ", _hva->Round()));
    Is_Trace_cmd(Tracing(), hor->Print(TFile));
    Is_Trace(Tracing(),
             (TFile, " from lhs cr%d of sr%d for ALLOCA-ISTORE.\n",
                     sr->Lhs()->Coderep_id(), sr->Stmtrep_id()));
  }
  Is_True(hor == NULL ||
          Vsa()->Is_special_hor(hor) ||
          !hor->Heap_obj()->Top_match_sr(sr),
          ("istore creates new hor"));
}

// forward process stmtrep
void
HVA_HO_RENAMING::Process_stmt_fwd(STMTREP* sr)
{
  // Process stmt ho mu list
  MU_LIST *mu_list = Vsa()->Stmt_hor_mu(sr);
  if (mu_list) {
    MU_NODE     *mnode;
    MU_LIST_ITER mu_iter;
    FOR_ALL_NODE (mnode, mu_iter, Init(Vsa()->Stmt_hor_mu(sr))) {
      CHOR *chor = (CHOR *)mnode->OPND();
      HEAP_OBJ_REP *hor = chor->first;
      if (Vsa()->Is_special_hor(hor))
        continue;
      HEAP_OBJ_REP *tos = hor->Heap_obj()->Top_of_stack();
      if (hor != tos) {
        chor->first = tos;
        Is_Trace(Tracing(), (TFile, "HOR[%d]: set mu opnd ",  _hva->Round()));
        Is_Trace_cmd(Tracing(), tos->Print(TFile));
        Is_Trace(Tracing(), (TFile, " in sr%d %s.\n",
                             sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4));
      }
    }
  }

  // process stmt
  Process_stmtrep<TRUE>(sr);

  // do not process hor chi list because only call can have hor chi and it's
  // handled in Process_call.

  // Process stmt vo chi list, update vor hor
  CHI_LIST *chi_list = Vsa()->Stmt_vor_chi(sr);
  if (chi_list) {
    CHI_NODE     *cnode;
    CHI_LIST_ITER chi_iter;
    FOR_ALL_NODE (cnode, chi_iter, Init(chi_list)) {
      CVOR* cvor_opnd = (CVOR*)cnode->OPND();
      CVOR* cvor_res = (CVOR*)cnode->RESULT();
      VSYM_OBJ_REP* vor_opnd = cvor_opnd->first;
      VSYM_OBJ_REP* vor_res = cvor_res->first;
      HEAP_OBJ_REP *hor = vor_res->Hor() ? vor_res->Hor() : vor_opnd->Hor();
      if (hor && !Vsa()->Is_special_hor(hor)) {
        HEAP_OBJ_REP *tos = hor->Heap_obj()->Top_of_stack();
        vor_res->Set_hor(tos);
      }
    }
  }

  // Process stmt vo mu list, if vor hor version mismatches with tos,
  // append the lastest version to hor mu
  MU_LIST *vor_mu_list = Vsa()->Stmt_vor_mu(sr);
  if (vor_mu_list) {
    MU_NODE     *mnode;
    MU_LIST_ITER mu_iter;
    FOR_ALL_NODE (mnode, mu_iter, Init(vor_mu_list)) {
      CVOR *cvor = (CVOR*)mnode->OPND();
      VSYM_OBJ_REP *vor = cvor->first;
      HEAP_OBJ_REP *hor = vor->Hor();
      if (hor && !Vsa()->Is_special_hor(hor)) {
        HEAP_OBJ_REP *tos = hor->Heap_obj()->Top_of_stack();
        // hor chi may be added to track vor free
        if ((tos->Attr() == ROR_DEF_BY_FREE || tos->Attr() == ROR_DEF_BY_CHI) &&
             tos->Stmt_def() == sr) {
          tos = tos->Prev_ver();
        }
        if (tos && tos != hor) {
          Vsa()->Update_stmt_hor_mu(sr, tos, cvor->second, TRUE);
        }
      }
    }
  }
}

// backward process stmtrep
void
HVA_HO_RENAMING::Process_stmt_rev(STMTREP* sr)
{
  Process_stmtrep<FALSE>(sr);
}

// Process_cr<CK_VAR>: rename VAR coderep
template<> void
HVA_HO_RENAMING::Process_cr<CK_VAR>(STMTREP* sr, CODEREP* cr, UINT flag)
{
  Is_True(sr && cr && cr->Kind() == CK_VAR, ("invalid var cr"));

  HEAP_OBJ_REP* hor = Vsa()->Find_stmt_hor_mu(sr, cr);
  if (hor != NULL && !Vsa()->Is_special_hor(hor)) {
    HEAP_OBJ_REP *tos = hor->Heap_obj()->Top_of_stack();
    if (tos != hor) {
      //if (_hva->Ho_created(hor))
        Vsa()->Append_stmt_hor_mu(sr, tos, cr);
      //else if (_hva->Ho_updated(hor))
      //  Vsa()->Update_stmt_hor_mu(sr, hor, cr, TRUE);
      //else
      //  return;

      Is_Trace(Tracing(), (TFile, "HOR[%d]: Annotate hor mu ", _hva->Round()));
      Is_Trace_cmd(Tracing(), tos->Print(TFile));
      Is_Trace(Tracing(),
               (TFile, " to sym%dv%d cr%d in sr%d %s.\n",
                       cr->Aux_id(), cr->Version(), cr->Coderep_id(),
                       sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4));
    }
    HOR_LIST *ulist = tos->Ulist();
    if (ulist) {
      HEAP_OBJ_REP *cur_hor;
      HOR_LIST_ITER hor_list_iter;
      FOR_ALL_NODE(cur_hor, hor_list_iter, Init(ulist)) {
        Is_True(!Vsa()->Is_special_hor(cur_hor), ("special hor"));
        HEAP_OBJ_REP *cur_tos = cur_hor->Heap_obj()->Top_of_stack();
        if (cur_tos != cur_hor) {
          Vsa()->Update_stmt_hor_mu(sr, cur_tos, cr, TRUE);
          Is_Trace(Tracing(), (TFile, "HOR[%d]: Annotate hor mu for ulist ", _hva->Round()));
          Is_Trace_cmd(Tracing(), cur_tos->Print(TFile));
          Is_Trace(Tracing(),
               (TFile, " to sym%dv%d cr%d in sr%d %s.\n",
                       cr->Aux_id(), cr->Version(), cr->Coderep_id(),
                       sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4));
        }
      }
    }
  }
}

// Process_cr<CK_IVAR>: renmae IVAR coderep
template<> void
HVA_HO_RENAMING::Process_cr<CK_IVAR>(STMTREP* sr, CODEREP* cr, UINT flag)
{
  Is_True(sr && cr && cr->Kind() == CK_IVAR, ("invalid ivar cr"));

  if (cr->Opr() == OPR_PARM) {
    // mark heap obj address passed flag, control MSF check
    // to reduce FP for those ho been passed to another call
    // and may escaped outof current scope
    CODEREP *base = Find_ilod_base(cr->Ilod_base());
    if (base && base->Kind() == CK_LDA) {
        ST *st = base->Lda_base_st();
        Is_True(st != NULL, ("Lda base st is NULL"));
        MU_NODE *mu;
        MU_LIST_ITER mu_iter;
        FOR_ALL_NODE(mu, mu_iter, Init(sr->Mu_list())) {
          AUX_STAB_ENTRY *aux_entry = Opt_stab()->Aux_stab_entry(mu->Aux_id());
          if (aux_entry->St() != st)
            continue;
          CODEREP *opnd = mu->OPND();
          HEAP_OBJ_REP *opnd_hor = Vsa()->Cr_2_heap_obj(opnd);
          if (opnd_hor) {
            opnd_hor->Heap_obj()->Set_flag(RSC_ADDR_PASSED_OUT);
          }
        }
    }
    return Process_coderep<void>(sr, cr->Ilod_base(), flag);
  }

  CODEREP* base = sr->Lhs() == cr ? cr->Istr_base() : cr->Ilod_base();
  Process_coderep<void>(sr, base, flag);

  if (cr->Opr() == OPR_MLOAD)
    Process_coderep<void>(sr, cr->Mload_size(), flag);

  HEAP_OBJ_REP* hor = Vsa()->Cr_2_heap_obj(cr);
  if (hor != NULL &&
      !Vsa()->Is_special_hor(hor) &&
      hor->Heap_obj()->Kind() != RSC_KIND_LDA) {
    HEAP_OBJ_REP *tos;
    if (hor->Attr() == ROR_DEF_BY_ALLOCA &&
        hor->Stmt_def() == sr && cr == sr->Lhs()) {
      _hva->Gen_name(hor, sr);
      Is_Trace(Tracing(), (TFile, "HOR[%d]: push hor ", _hva->Round()));
      Is_Trace_cmd(Tracing(), hor->Print(TFile));
      Is_Trace(Tracing(),
               (TFile, " to cr%d in sr%d for ALLOCA-ISTORE.\n",
                       cr->Coderep_id(), sr->Stmtrep_id()));
      tos = hor;
    }
    else {
      tos = hor->Heap_obj()->Top_of_stack();
    }
    if (hor != tos) {
      Vsa()->Append_stmt_hor_mu(sr, tos, cr);
      VSYM_OBJ_REP *vor = Vsa()->Cr_2_vor(cr);
      if (vor != NULL && vor->Hor() != tos) {
        Is_True(!Vsa()->Is_special_vor(vor), ("set hor on special vor"));
        vor->Set_hor(tos);
      }
      Is_Trace(Tracing(), (TFile, "HOR[%d]: Annotate hor ", _hva->Round()));
      Is_Trace_cmd(Tracing(), tos->Print(TFile));
      if (vor != NULL) {
        Is_Trace(Tracing(), (TFile, " to vor "));
        Is_Trace_cmd(Tracing(), vor->Print(TFile));
      }
      Is_Trace(Tracing(),
               (TFile, " to cr%d in sr%d %s.\n",
                       cr->Coderep_id(), sr->Stmtrep_id(),
                       OPERATOR_name(sr->Opr()) + 4));
    }
    HOR_LIST *ulist = tos->Ulist();
    if (ulist) {
      HEAP_OBJ_REP *cur_hor;
      HOR_LIST_ITER hor_list_iter;
      FOR_ALL_NODE(cur_hor, hor_list_iter, Init(ulist)) {
        Is_True(!Vsa()->Is_special_hor(cur_hor), ("special hor"));
        HEAP_OBJ_REP *cur_tos = cur_hor->Heap_obj()->Top_of_stack();
        if (cur_tos != cur_hor) {
          Vsa()->Update_stmt_hor_mu(sr, cur_tos, cr, TRUE);
          Is_Trace(Tracing(), (TFile, "HOR[%d]: Annotate hor mu for ulist ", _hva->Round()));
          Is_Trace_cmd(Tracing(), cur_tos->Print(TFile));
          Is_Trace(Tracing(),
              (TFile, " to cr%d in sr%d %s.\n",
                      cr->Coderep_id(),
                      sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4));
        }
      }
    }
  }
}

// Process_cr<CK_OP>: rename OP coderep
template<> void
HVA_HO_RENAMING::Process_cr<CK_OP>(STMTREP* sr, CODEREP* cr, UINT flag)
{
  Is_True(sr && cr && cr->Kind() == CK_OP, ("invalid op cr"));

  for (INT32 i = 0; i < cr->Kid_count(); ++i) {
    Process_coderep<void>(sr, cr->Opnd(i), flag);
  }
}

// Process_sr<OPR_STID, TRUE>: forward pass to rename stid
template<> void
HVA_HO_RENAMING::Process_sr<OPR_STID, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_STID, ("invalid stid sr"));

  Process_coderep<void>(sr, sr->Rhs(), 0);

  CODEREP *lhs = sr->Lhs();
  HEAP_OBJ_REP *hor = Vsa()->Cr_2_heap_obj(lhs);
  if (hor && hor->Attr() == ROR_DEF_BY_ALLOCA &&
      hor->Stmt_def() == sr) {
    _hva->Gen_name(hor, sr);
    Is_Trace(Tracing(), (TFile, "HOR[%d]: push hor ", _hva->Round()));
    Is_Trace_cmd(Tracing(), hor->Print(TFile));
    Is_Trace(Tracing(),
             (TFile, " to cr%d in sr%d for ALLOCA-STID.\n",
                     lhs->Coderep_id(), sr->Stmtrep_id()));
    return;
  }

  HEAP_OBJ_REP *tos = hor;
  if (hor == NULL) {
    CODEREP *rhs_base = Find_ilod_base(sr->Rhs());
    if (rhs_base != NULL)
      tos = Vsa()->Cr_2_heap_obj(rhs_base);
  }
  if (tos != NULL) {
    if (!Vsa()->Is_special_hor(tos))
      tos = tos->Heap_obj()->Top_of_stack();
    if (hor != tos) {
      Vsa()->Enter_cr_heap_obj_map(lhs, tos);
      Is_Trace(Tracing(), (TFile, "HOR[%d]: Annotate hor ", _hva->Round()));
      Is_Trace_cmd(Tracing(), tos->Print(TFile));
      Is_Trace(Tracing(),
               (TFile, " to sym%dv%d cr%d in sr%d %s line %d.\n",
                       lhs->Aux_id(), lhs->Version(), lhs->Coderep_id(),
                       sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4,
                       Srcpos_To_Line(sr->Linenum())));
    }
  }
}

// Process_sr<OPR_STID, FALSE>: reverse pass to rename stid
template<> void
HVA_HO_RENAMING::Process_sr<OPR_STID, FALSE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_STID, ("invalid stid sr"));

  CODEREP *lhs = sr->Lhs();
  HEAP_OBJ_REP *hor = Vsa()->Cr_2_heap_obj(lhs);
  if (hor &&
      hor->Attr() == ROR_DEF_BY_ALLOCA &&
      hor->Stmt_def() == sr) {
    Is_True(hor->Heap_obj()->Top_match_sr(sr), ("hor tos mismatch"));
    hor->Heap_obj()->Pop();
    Is_Trace(Tracing(), (TFile, "HOR[%d]: pop hor ", _hva->Round()));
    Is_Trace_cmd(Tracing(), hor->Print(TFile));
    Is_Trace(Tracing(),
             (TFile, " from cr%d in sr%d for ALLOCA-STID.\n",
                     lhs->Coderep_id(), sr->Stmtrep_id()));
    return;
  }
  Is_True(hor == NULL ||
          Vsa()->Is_special_hor(hor) ||
          !hor->Heap_obj()->Top_match_sr(sr),
          ("stid creates new hor"));
}

// Process_sr<OPR_ISTORE, TRUE>: forward pass to rename istore
template<> void
HVA_HO_RENAMING::Process_sr<OPR_ISTORE, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_ISTORE, ("invalid istore sr"));
  Process_istore<TRUE>(sr, TRUE);
}

// Process_sr<OPR_ISTORE, FALSE>: reverse pass to rename istore
template<> void
HVA_HO_RENAMING::Process_sr<OPR_ISTORE, FALSE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_ISTORE, ("invalid istore sr"));
  Process_istore<FALSE>(sr, TRUE);
}

// Process_sr<OPR_MSTORE, TRUE>: forward pass to rename mstore
template<> void
HVA_HO_RENAMING::Process_sr<OPR_MSTORE, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_MSTORE, ("invalid mstore sr"));
  // process mstore size
  Process_coderep<void>(sr, sr->Lhs()->Mstore_size(), 0);
  // process istore
  Process_istore<TRUE>(sr, TRUE);
}

// Process_sr<OPR_MSTORE, FALSE>: reverse pass to rename mstore
template<> void
HVA_HO_RENAMING::Process_sr<OPR_MSTORE, FALSE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_MSTORE, ("invalid mstore sr"));
  Process_istore<FALSE>(sr, TRUE);
}

// Process_sr<OPR_CALL, TRUE>: forward pass to rename call/icall
template<> void
HVA_HO_RENAMING::Process_sr<OPR_CALL, TRUE>(STMTREP* sr)
{
  Is_True(sr->Opr() == OPR_CALL || sr->Opr() == OPR_ICALL, ("not a call"));
  Process_call<TRUE>(sr, TRUE);
}

// Process_sr<OPR_CALL, FALSE>: reverse pass to rename call/icall
template<> void
HVA_HO_RENAMING::Process_sr<OPR_CALL, FALSE>(STMTREP* sr)
{
  Is_True(sr->Opr() == OPR_CALL || sr->Opr() == OPR_ICALL, ("not a call"));
  Process_call<FALSE>(sr, FALSE);
}

// Process_sr<OPR_INTRINSIC_CALL, TRUE>: forward pass rename intrinsic call
template<> void
HVA_HO_RENAMING::Process_sr<OPR_INTRINSIC_CALL, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_INTRINSIC_CALL, ("invalid intrn call sr"));
  Process_call<TRUE>(sr, TRUE);
}

// Process_sr<OPR_INTRINSIC_CALL, FALSE>: reverse pass rename intrinsic call
template<> void
HVA_HO_RENAMING::Process_sr<OPR_INTRINSIC_CALL, FALSE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_INTRINSIC_CALL, ("invalid intrn call sr"));
  Process_call<FALSE>(sr, FALSE);
}

// Process_sr<OPR_OPT_CHI, TRUE>: rename hor annotated on entry chi
template<> void
HVA_HO_RENAMING::Process_sr<OPR_OPT_CHI, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_OPT_CHI, ("invalid opt_chi sr"));
  Process_chi<TRUE>(sr);
}

// Process_sr<OPR_OPT_CHI, FALSE>: reverse pass rename entry chi
template<> void
HVA_HO_RENAMING::Process_sr<OPR_OPT_CHI, FALSE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_OPT_CHI, ("invalid opt_chi sr"));
  Process_chi<FALSE>(sr);
}

// Process_sr<OPR_RETURN, TRUE>: rename hor annotated on return stmt
template<> void
HVA_HO_RENAMING::Process_sr<OPR_RETURN, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_RETURN, ("invalid return sr"));

  HEAP_OBJ *cur_ho;
  HO_LIST_ITER ho_iter;
  FOR_ALL_NODE(cur_ho, ho_iter, Init(_vsa->Heap_obj_list())) {
    HEAP_OBJ_REP *hor = cur_ho->Top_of_stack();
    Is_True(!_vsa->Is_special_hor(hor), ("special hor"));
    CODEREP *cr = cur_ho->Ho_cr();
    // Is_True(cr, ("null ho cr"));
    if (cr) {
      Vsa()->Append_stmt_hor_mu(sr, hor, cr);
    }
  }

  if (VSA_New_HVA_Compat) {
    IDTYPE first_ho_id = _hva->First_ho_id();
    IDTYPE ho_size = _hva->Vsa()->Last_heapobj_id() - first_ho_id;
    BB_NODE *bb = sr->Bb();
    HOR_ARRAY *array = Vsa()->Bb_horarr_map()->Lookup(bb->Id());
    Is_True(array == NULL, ("array already created"));
    if (array == NULL) {
      MEM_POOL* mp = Vsa()->Mem_pool();
      array = CXX_NEW(HOR_ARRAY(mempool_allocator<HEAP_OBJ_REP*>(mp)),
                      mp);
      Is_True(array != NULL, ("out of memory"));
      Vsa()->Bb_horarr_map()->Insert(bb->Id(), array);
      array->resize(ho_size);
    }
    else {
      Is_True(FALSE, ("array already created"));
      array->resize(ho_size);
    }

    HEAP_OBJ *ho;
    HO_LIST_ITER iter;
    FOR_ALL_NODE(ho, iter, Init(_vsa->Heap_obj_list())) {
      HEAP_OBJ_REP *hor = ho->Top_of_stack();
      Is_True(!_vsa->Is_special_hor(hor), ("special hor"));
      INT index = hor->Heap_obj()->Id() - first_ho_id;
      Is_True(index >= 0 && index < array->size(),
              ("index out of bound"));
      (*array)[index] = hor;
    }
  }

}

// initialize HEAP_OBJ_REP renaming phase
void
HVA_HO_RENAMING::Initialize() {
  // place phi for ho created or updated in this iteration
  VSA::PHILIST_MAP *bb_ro_philist = _vsa->Bb_ho_philist();
  HEAP_OBJ         *heap_obj;
  HEAP_OBJ_REP     *hor;
  HO_LIST_ITER iter;
  BOOL vsa_tracing = _vsa->Tracing();
  _vsa->_tracing = Tracing();
  _vsa->Place_ro_phi_node(bb_ro_philist, heap_obj, _vsa->Heap_obj_list(), &iter, hor, TRUE, _hva->Ho_phi_cache());
  _vsa->_tracing = vsa_tracing;
}

// finalize HEAP_OBJ_REP renaming phase
void
HVA_HO_RENAMING::Finalize() {
  // verify ho renaming stack
#ifdef Is_True_On
  Vsa()->Verify_heap_obj_stack();
#endif

  // trace CFG with ho/vo annotation
  Is_Trace(Tracing(), (TFile, "HOR[%d]: ---------------------------------------------------------------\n", _hva->Round()));
  Is_Trace(Tracing(), (TFile, "HOR[%d]: FINISH HVA_HO_RENAMING.\n", _hva->Round()));
  Is_Trace(Tracing(), (TFile, "HOR[%d]: ---------------------------------------------------------------\n", _hva->Round()));
  //Is_Trace_cmd(Tracing(), Ipsa()->Print_fld_name_map(TFile));
  Is_Trace(Tracing(), (TFile, "HOR[%d]: HEAP OBJECT ANNOTATION dump:\n", _hva->Round()));
  Is_Trace_cmd(Tracing(), Vsa()->Print_hor(TFile));
}

// ============================================================================
// HVA_VO_CREATION
//
// Check IR and create VSYM based on HEAP_OBJ
// ============================================================================
class HVA_VO_CREATION : public CFG_VISITOR_BASE<HVA_VO_CREATION> {
private:
  // flags for traversing coderep
  enum CR_FLAG {
    CF_NONE    = 0x1,      // no special flag
    CF_IN_CALL = 0x2,      // CR used in call
    CF_IN_PARM = 0x4,      // CR used in parm
  };

  // a vector for all pending call stmt which has HEAP_OBJ used in callee
  typedef vector<STMT_HOR_SET*,
                 mempool_allocator<STMT_HOR_SET*> > PENDING_STMT_VEC;

  // helper function to collect HO used by a call
  class STMT_HOR_SET_HELPER {
  private:
    PENDING_STMT_VEC   *_vec;
    MEM_POOL           *_mpool;
    HEAP_VSYM_ANALYSIS *_hva;
    STMTREP            *_stmt;
    STMT_HOR_SET       *_set;

  public:
    // constructor
    STMT_HOR_SET_HELPER(PENDING_STMT_VEC *vec, MEM_POOL *mpool, HEAP_VSYM_ANALYSIS *hva, STMTREP *stmt)
      : _hva(hva), _vec(vec), _mpool(mpool), _stmt(stmt), _set(NULL) { }

    // add hor and its field hor to _ho_set of the stmt
    void Add(HEAP_OBJ_REP *hor, CODEREP *cr, UINT mr) {
      if (_set == NULL) {
        if (_vec->empty() || _vec->back()->Stmtrep() != _stmt) {
          _set = CXX_NEW(STMT_HOR_SET(_hva, _stmt, _mpool), _mpool);
          _vec->push_back(_set);
        }
        else {
          _set = _vec->back();
        }
      }
      Is_True(!_hva->Vsa()->Is_special_hor(hor), ("special hor added"));
      _set->Add(hor, cr, mr);
    }
  };

  PENDING_STMT_VEC *_pending_stmts;     // calls and ho it refered

public:
  // set visit flag
  enum {
    // visit LDA, CONST, VAR, IVAR and OP CODEREP
    CR_VFLAG = V_LDA | V_CONST | V_VAR | V_IVAR | V_OP,
    // visit store and call STMTREP
    SR_VFLAG = V_ANY_STORE | V_ANY_CALL | V_ANY_BRANCH | V_ASM_STMT,
  };

  // constructor
  HVA_VO_CREATION(HEAP_VSYM_ANALYSIS *hva)
   : CFG_VISITOR_BASE(hva, this) {
    // create _pending_stmts on local pool. this will be destroyed when
    // current iteration is done
    _pending_stmts = CXX_NEW(PENDING_STMT_VEC(), _hva->Local_pool());
    Is_True(_pending_stmts, ("out of memory?"));
  }

private:
  // process call
  template<BOOL _FWD>
  void Process_call(STMTREP *sr);

  // process istore
  template<BOOL _FWD>
  void Process_istore(STMTREP *sr);

  // process param
  VSYM_OBJ_REP *Process_param(STMTREP *sr, CODEREP *cr, UINT flag);

  // process return
  void Process_rets();

  // process pending stmts
  void Process_pending_calls();

public:
  // enter bb
  void Enter_bb(BB_NODE *bb) {
  }

  // process rhs of the stmtrep
  template<OPERATOR opr, BOOL _FWD>
  void Process_sr(STMTREP *sr) {
    Is_True(sr->Lhs() == NULL, ("TODO: %s", OPERATOR_name(sr->Opr()) + 4));
    if (sr->Rhs())
      Process_coderep<VSYM_OBJ_REP *>(sr, sr->Rhs(), FALSE);
  }

  // process coderep
  template<CODEKIND kind>
  VSYM_OBJ_REP *Process_cr(STMTREP *sr, CODEREP *cr, UINT flag) { return NULL; }

  // perform the VSYM_OBJ_REP creation phase
  void Perform() {
    CFG_WALKER<HVA_VO_CREATION> walker(_comp_unit->Cfg(), this);
    walker.Perform();
  }

  // initialize the VSYM_OBJ_REP creation phase
  void Initialize();

  // finalize the VSYM_OBJ_REP creation phase
  void Finalize();
};

// Process_call<TRUE>: forward pass to create vsym for call
template<> void
HVA_VO_CREATION::Process_call<TRUE>(STMTREP *sr)
{
  Is_True(sr && OPERATOR_is_call(sr->Opr()), ("invalid call sr"));

  // STMT_HOR_SET for this call
  STMT_HOR_SET_HELPER ho_set(_pending_stmts, _hva->Local_pool(), _hva, sr);

  // check if call frees memory
  pair<CODEREP *, CODEREP *> arg_pair = _hva->Callee_frees_heap_memory(sr);
  CODEREP *arg = arg_pair.first;
  if (arg != NULL) {
    HEAP_OBJ_REP *hor = Vsa()->Cr_2_heap_obj(sr->Rhs());
    if (hor != NULL && _hva->Visit_heap_obj(hor)) {
      // add to _pending calls and handled later
      ho_set.Add(hor, arg, HO_MOD);
    }
  }

  // check if call mallocs memory
  if (Vsa()->Callee_returns_new_heap_memory(sr)) {
    HEAP_OBJ_REP *hor = Vsa()->Cr_2_heap_obj(sr->Rhs());
    CODEREP* ret = Comp_unit()->Find_return_value(sr);
    CODEREP* bind_cr = sr->Rhs();
    // check if return value has hor
    if (ret != NULL && TY_kind(ret->object_ty()) == KIND_POINTER) {
      HEAP_OBJ_REP *ret_hor = Vsa()->Cr_2_heap_obj(ret);
      Is_True(ret_hor == hor, ("hor mismatch for malloc"));
      // bind to return value
      bind_cr = ret;
    }
    if (hor != NULL && _hva->Visit_heap_obj(hor)) {
      // add to _pending calls and handled later
      ho_set.Add(hor, bind_cr, HO_MOD);
    }
  }

  CHI_LIST *chi_list = Vsa()->Stmt_hor_chi(sr);
  if (arg == NULL && chi_list != NULL) {
    CHI_NODE *chi;
    CHI_LIST_ITER chi_iter;
    FOR_ALL_NODE(chi, chi_iter, Init(chi_list)) {
      CHOR *copnd = (CHOR *)chi->OPND();
      CHOR *cres = (CHOR*)chi->RESULT();
      HEAP_OBJ_REP *hor = cres->first;
      CODEREP *cr = cres->second;
      Is_True(hor->Heap_obj() == copnd->first->Heap_obj(),
              ("Heap obj mismatch"));
      Is_True(cr && cr == copnd->second,
              ("cr mismatch"));
      if (_hva->Visit_heap_obj(hor)) {
        ho_set.Add(hor, cr, HO_MOD);
      }
    }
  }
}

// Process_istore<TRUE>: forward pass to create vsym for istore
template<> void
HVA_VO_CREATION::Process_istore<TRUE>(STMTREP *sr)
{
  Is_True(sr &&
          (sr->Opr() == OPR_ISTORE || sr->Opr() == OPR_MSTORE),
          ("invalid istore"));

  // process rhs
  CODEREP *rhs = sr->Rhs();
  Process_coderep<VSYM_OBJ_REP*>(sr, rhs, 0);

  // process lhs
  CODEREP *lhs = sr->Lhs();
  CODEREP *base = lhs->Istr_base();
  // process lhs base
  Process_coderep<VSYM_OBJ_REP*>(sr, base, 0);

  // check if vsym_obj already annotated
  VSYM_OBJ_REP* vor = Vsa()->Cr_2_vor(lhs);
  if (vor != NULL)
    return;

  // create vor for lhs
  HEAP_OBJ_REP* hor;
  base = Find_ilod_base(base);
  if (base == NULL) {
    hor = Vsa()->Null_hor();
  }
  else {
    hor = _hva->Find_cr_hor(sr, base);
    if (hor == NULL) {
      _hva->Set_visit_next(sr);
      return;
    }
  }
  Is_True(hor != NULL, ("invalid hor"));

  if (!Vsa()->Is_special_hor(hor)) {
    VSYM_FLD_REP vfr = Vsa()->Cr_vfr(lhs);
    vor = _hva->Create_vsym_obj_def(sr->Bb(), hor, &vfr, Defbb_pool());

    // check if vfr is FLD_K_ANY and set hor flag
    if (vfr.Is_any() && !hor->Field_any())
      hor->Set_field_any(TRUE);

    if (vor->Vsym_obj()->Ref_cr() == NULL)
      vor->Vsym_obj()->Set_ref_cr(lhs);
    vor->Set_srcpos_node(sr, Dna(), PATHINFO_ISTORE);
    vor->Set_attr(ROR_DEF_BY_ISTORE);
    vor->Set_stmt_def(sr, Dna());
    // create vsym chi if hor has ulist
    HOR_LIST *ulist = hor->Ulist();
    if (ulist) {
      HEAP_OBJ_REP *cur_hor;
      HOR_LIST_ITER hor_list_iter;
      FOR_ALL_NODE(cur_hor, hor_list_iter, Init(ulist)) {
        VSYM_OBJ_REP *cur_vor;
        if (cur_hor->Attr() != ROR_DEF_BY_LDA &&
            cur_hor->Is_entry_chi())
          continue;

        if (vfr.Is_any() && !cur_hor->Field_any())
          cur_hor->Set_field_any(TRUE);

        cur_vor = _hva->Create_vsym_obj_use(cur_hor, &vfr);
        if (cur_vor->Vsym_obj()->Ref_cr() == NULL)
          vor->Vsym_obj()->Set_ref_cr(lhs);
        // create vor chi on this stmt
        _hva->Append_stmt_vor_chi(lhs, sr, cur_vor, Defbb_pool());

        Is_Trace(Tracing(), (TFile, "VOC[%d]: Create chi vor ", _hva->Round()));
        Is_Trace_cmd(Tracing(), cur_vor->Print(TFile));
        Is_Trace(Tracing(), (TFile, " on ulist hor "));
        Is_Trace_cmd(Tracing(), cur_hor->Print(TFile));
        Is_Trace(Tracing(),
                 (TFile, " to ISTORE lhs cr%d:\n", lhs->Coderep_id()));
        Is_Trace_cmd(Tracing(), lhs->Print(2, TFile));
      }
    }

    // for vfr any, if istore doesn't access whole base ho, append a mu node
    if (vfr.Is_any() && Vsa()->Vor_access_whole_ho(sr, lhs, vor) != 1) {
      _hva->Append_stmt_vor_mu(lhs, sr, vor);
      Is_Trace(Tracing(), (TFile, "VOC[%d]: Create mu vor ", _hva->Round()));
      Is_Trace_cmd(Tracing(), vor->Print(TFile));
      Is_Trace(Tracing(), (TFile, " because stmt doesn't access whole base ho.\n"));
    }

    // for partitial store, add vor any mu and chi if exists by putting it to
    // pending stmts and prcessed in Finalize phase - as the any vo may not created
    // yet when processing the ISTORE statement
    if (!vfr.Is_any() && VSA_Enable_TAG && !VSA_Enable_TAG_OLD) {
      STMT_HOR_SET_HELPER ho_set(_pending_stmts, _hva->Local_pool(), _hva, sr);
      if (_hva->Visit_heap_obj(hor)) {
        ho_set.Add(hor, base, ANY_VO_MOD|ANY_VO_REF);
      }
    }

    // for istore's base hor def by map operator[], add vor chi to connect with hor def
    // map[key] = data   ->   res = &(map[key])
    //                        *res = data
    // generate vor chi as below
    //       vor_mu: vo1v-1(map)
    //      >res = &(map[key])
    //       vor_chi: vo1v1(map), vo2v1(res)
    //      >*res = data
    //        vor_chi vo2v2(res)
    if (hor->Attr() == ROR_DEF_BY_CHI) {
      STMTREP *def_sr = hor->Stmt_def();
      if (def_sr && OPERATOR_is_call(def_sr->Opr())) {
        RNA_NODE *rna = Vsa()->Sr_2_rna(def_sr);
        if (rna && Vsa()->Ipsa()->Rna_has_rbc_op(rna, RBC_OP_SET_FUNC_MAP_GET_REF)) {
          CODEREP *base_cr = rna->Get_arg_with_flag(REF_BASE);
          CODEREP *ret_cr = Comp_unit()->Find_return_value(def_sr);
          Is_True(base_cr, ("null base for map get"));
          Is_True(ret_cr, ("null ret for map get"));
          if (base_cr && ret_cr) {
            HEAP_OBJ_REP *base_hor = _hva->Find_cr_hor(def_sr, base_cr);
            Is_True(base_hor, ("null heap obj for map get's base"));
            if (base_hor) {
              _hva->Append_stmt_vor_chi(ret_cr, def_sr, vor, Defbb_pool());
              STMT_HOR_SET_HELPER ho_set(_pending_stmts, _hva->Local_pool(), _hva, def_sr);
              ho_set.Add(base_hor, base_cr, HO_MOD);
              Is_Trace(Tracing(), (TFile, "VOC[%d]: Create chi vor ", _hva->Round()));
              Is_Trace_cmd(Tracing(), vor->Print(TFile));
              Is_Trace(Tracing(), (TFile, " because vor is based on map element ref\n"));
            }
          }
        }
      }
    }

    // annotate rhs_hor to lhs vor and set as hor field
    HEAP_OBJ_REP* rhs_hor;
    rhs = Find_ilod_base(rhs);
    if (rhs != NULL &&
        (rhs_hor = _hva->Find_cr_hor(sr, rhs)) != NULL) {
      vor->Set_hor(rhs_hor);
      //FIELD_OBJ_REP* fl = CXX_NEW(FIELD_OBJ_REP(rhs_hor, vfr),
      //                            Vsa()->Mem_pool());
      //fl->Set_Next(hor->Flist());
      //hor->Set_flist(fl);
      if(!_vsa->Is_special_hor(rhs_hor)) {
        STMT_HOR_SET_HELPER ho_set(_pending_stmts, _hva->Local_pool(), _hva, sr);
        ho_set.Add(rhs_hor, rhs, HO_REF);
      }
    }
  }
  else if (hor == Vsa()->Default_hor()) {
    // use Default_vor for Default_hor
    vor = Vsa()->Default_vor();
  }
  else {
    // use Null_vor for NULL or Null_hor
    vor = Vsa()->Default_vor();
  }

  Vsa()->Enter_cr_vor_map(lhs, vor);
  Is_Trace(Tracing(), (TFile, "VOC[%d]: Create vor ", _hva->Round()));
  Is_Trace_cmd(Tracing(), vor->Print(TFile));
  Is_Trace(Tracing(), (TFile, " on hor "));
  Is_Trace_cmd(Tracing(), hor->Print(TFile));
  Is_Trace(Tracing(),
           (TFile, " to ISTORE lhs cr%d:\n", lhs->Coderep_id()));
  Is_Trace_cmd(Tracing(), lhs->Print(2, TFile));
}

VSYM_OBJ_REP *
HVA_VO_CREATION::Process_param(STMTREP *sr, CODEREP *cr, UINT flag)
{
  Is_True(sr && cr && cr->Opr() == OPR_PARM, ("invalid ivar cr"));
  UINT mod_ref = 0;
  BOOL parm_need_vsym = FALSE;
  CODEREP *base = cr->Ilod_base();
  IDTYPE   param = INVALID_VAR_IDX;
  RNA_NODE *rna = NULL;
  if (OPERATOR_is_call(sr->Opr())) {
    rna = Dna()->Get_callsite_rna(sr);
    param = rna->Get_arg_with_cr(base);
    // do a kack for intrinsic call for testing purpose
    INTRINSIC intrn = Get_call_intrinsic(sr);
    if (intrn == INTRN_STRCPY || intrn == INTRN_STRNCPY ||
        intrn == INTRN_MEMCPY) {
      if (param == 1)
        mod_ref |= HO_MOD;
      else if (param == 2)
        mod_ref |= HO_REF;
    }
    else if (intrn == INTRN_MEMSET && param == 1)
      mod_ref |= HO_MOD;

    if (rna->Is_set_arg_flag(param, REF_ILOAD))
      mod_ref |= HO_REF;
    if (rna->Is_set_arg_flag(param, REF_ISTORE))
      mod_ref |= HO_MOD;

    // no callee dna, check param type
    if (mod_ref == 0 &&
        flag != TY_IDX_ZERO &&
        rna->Callee_list().empty()) {
      if (TY_kind(flag) == KIND_POINTER) {
        mod_ref |= HO_REF;
        if (!TY_is_const(TY_pointed(flag)))
          mod_ref |= HO_MOD;
      }
    }

    if (mod_ref != 0) {
      // if the base contents is not modified in current PU
      // do not need to create vsym or vsym mu/chi on stmt
      // UD follows with base pointer
      V_ANNOT annot = base->Kind() == CK_VAR ? base->Vsa_annot() : VANT_UTIL::Empty();
      parm_need_vsym = TRUE;
      if (annot != VANT_UTIL::Empty() && (mod_ref & HO_MOD == 0) &&
          VANT_UTIL::Get(annot, ANT_VWRITE) == ANT_NO) {
        parm_need_vsym = FALSE;
      }
    }
  }
  VSYM_OBJ_REP *base_vor = Process_coderep<VSYM_OBJ_REP*>(sr, base, flag);
  if (parm_need_vsym) {
    base = Find_ilod_base(base);
    HEAP_OBJ_REP *hor = base ? _hva->Find_cr_hor(sr, base) : NULL;
    if (hor && !Vsa()->Is_special_hor(hor)) {
      Is_True_Ret(rna, ("null rna"), base_vor);
      Is_True_Ret(param != INVALID_VAR_IDX, ("invalid param_idx"), base_vor);
      if (rna->Is_set_arg_flag(param, REF_COPIED) ||
          (rna->Callee_list().empty() && !Vsa()->Callee_frees_heap_memory(sr))) {
        hor->Heap_obj()->Set_flag(RSC_ADDR_PASSED_OUT);
      }
      // STMT_HOR_SET for this call, add sr to pending stmts to create vsym mu/chi
      // on sr in Finalize phase
      VSYM_OBJ *base_vo = base_vor ? base_vor->Vsym_obj() : NULL;
      STMT_HOR_SET_HELPER ho_set(_pending_stmts, _hva->Local_pool(), _hva, sr);
      if (mod_ref & HO_REF) {
        ho_set.Add(hor, base, HO_REF);
        // if base is an vsym, add the vsym to vor mu list
        if (base_vo) {
          _hva->Append_stmt_vor_mu(base, sr, base_vor);
        }
      }
      if (mod_ref & HO_MOD) {
        ho_set.Add(hor, base, HO_MOD);
        // if base is an vsym, add the vsym to vor chi list
        // to represent chi for any field
        if (base_vo) {
          _hva->Append_stmt_vor_chi(base, sr, base_vo->Entry_chi(), Defbb_pool());
          if (!_hva->Vo_created(base_vor)) {
            _hva->Set_vo_updated(base_vor->Vsym_obj());
          }
        }
      }
      // if base is LDA, add base's mu's hor. for example:
      // b = &a; foo(&b);
      // add hor of LDA.
      if (base->Kind() == CK_LDA) {
        ST *st = base->Lda_base_st();
        Is_True(st != NULL, ("Lda base st is NULL"));
        // iterate through var mu list
        if (mod_ref & HO_REF) {
          MU_NODE *mu;
          MU_LIST_ITER mu_iter;
          FOR_ALL_NODE(mu, mu_iter, Init(sr->Mu_list())) {
            // ignore special vsym
            if (mu->Aux_id() == Opt_stab()->Default_vsym() ||
                mu->Aux_id() == Opt_stab()->Return_vsym())
              continue;
            AUX_STAB_ENTRY *aux_entry = Opt_stab()->Aux_stab_entry(mu->Aux_id());
            if (aux_entry->St() != st)
              continue;
            CODEREP *opnd = mu->OPND();
            VSYM_OBJ_REP *vor = Vsa()->Cr_2_vor(opnd);
            HEAP_OBJ_REP *opnd_hor = NULL;
            if (vor && vor->Hor()) {
              opnd_hor = vor->Hor();
            } else {
              V_ANNOT annot = opnd->Vsa_annot();
              if ((VANT_UTIL::Get(annot, ANT_VWRITE) != ANT_NO) ||
                  (VANT_UTIL::Get(annot, ANT_VREAD) != ANT_NO)) {
                opnd_hor = _hva->Find_cr_hor(sr, opnd);
              }
            }
            if (opnd_hor && !Vsa()->Is_special_hor(opnd_hor)) {
              ho_set.Add(opnd_hor, base, mod_ref);
            }
          }
        }
        // iterate through var chi list
        if (mod_ref & HO_MOD) {
          CHI_NODE *chi;
          CHI_LIST_ITER chi_iter;
          FOR_ALL_NODE (chi, chi_iter, Init(sr->Chi_list())) {
            if (!chi->Live() ||
                chi->Aux_id() == Opt_stab()->Default_vsym() ||
                chi->Aux_id() == Opt_stab()->Return_vsym())
              continue;
            CODEREP *opnd = chi->OPND();
            if (opnd->Is_flag_set(CF_IS_ZERO_VERSION) ||
                opnd->Is_var_volatile())
              continue;
            AUX_STAB_ENTRY *aux = Opt_stab()->Aux_stab_entry(chi->Aux_id());
            Is_True(aux, ("aux entry is NULL"));
            if (aux->St() != st)
              continue;
            VSYM_OBJ_REP *vor = Vsa()->Cr_2_vor(opnd);
            HEAP_OBJ_REP *opnd_hor = NULL;
            if (vor && vor->Hor()) {
              opnd_hor = vor->Hor();
            } else {
              V_ANNOT annot = opnd->Vsa_annot();
              if ((VANT_UTIL::Get(annot, ANT_VWRITE) != ANT_NO) ||
                  (VANT_UTIL::Get(annot, ANT_VREAD) != ANT_NO)) {
                opnd_hor = _hva->Find_cr_hor(sr, opnd);
              }
            }
            if (opnd_hor && !Vsa()->Is_special_hor(opnd_hor)) {
              // TODO: add with chi RESULT/OPND?
              ho_set.Add(opnd_hor, base, mod_ref);
            }
          }
        }
      }

      // if vsym is needed for the base pointer, create vsym with zero field
      VSYM_FLD_REP zero_fld(FLD_K_ID, 0, 0);
      VSYM_FLD_REP any_fld(FLD_K_ANY, 0, 0);
      VSYM_FLD_REP *vfr = &any_fld;
      if (base->Kind() == CK_LDA) {
        if (Vsa()->Get_vfr_kind(base) == FLD_K_ID) {
          vfr = &zero_fld;
        }
      }
      if ((mod_ref & HO_REF)) {
        VSYM_OBJ_REP *mu_vor = _hva->Create_vsym_obj_use(hor, vfr);
        if (_hva->Vo_created(mu_vor)) {
          if (mu_vor->Vsym_obj()->Ref_cr() == NULL)
            mu_vor->Vsym_obj()->Set_ref_cr(base);
        } else {
          _hva->Append_stmt_vor_mu(base, sr, mu_vor);
        }

        Is_Trace(Tracing(), (TFile, "VOC[%d]: Create vor ", _hva->Round()));
        Is_Trace_cmd(Tracing(), mu_vor->Print(TFile));
        Is_Trace(Tracing(), (TFile, " on hor "));
        Is_Trace_cmd(Tracing(), hor->Print(TFile));
        Is_Trace(Tracing(),
                (TFile, " to call mu for parm cr%d\n", cr->Coderep_id()));
      }
      if ((mod_ref & HO_MOD)) {
        VSYM_OBJ_REP *chi_vor = _hva->Create_vsym_obj_def(sr->Bb(), hor, vfr, Defbb_pool());
        if (_hva->Vo_created(chi_vor)) {
          if (chi_vor->Vsym_obj()->Ref_cr() == NULL)
            chi_vor->Vsym_obj()->Set_ref_cr(base);
        } else {
          // if chi_vor not created in this iteration,
          // it won't be processed in Finalize phase
          // add vor chi here and set vo_updated flag for renaming
          _hva->Append_stmt_vor_chi(base, sr, chi_vor->Vsym_obj()->Entry_chi(), Defbb_pool());
          _hva->Set_vo_updated(chi_vor->Vsym_obj());
        }
        chi_vor->Set_srcpos_node(sr, Dna(), PATHINFO_CHI);
        chi_vor->Set_attr(ROR_DEF_BY_CHI);
        chi_vor->Set_stmt_def(sr, Dna());
        Is_Trace(Tracing(), (TFile, "VOC[%d]: Create vor ", _hva->Round()));
        Is_Trace_cmd(Tracing(), chi_vor->Print(TFile));
        Is_Trace(Tracing(), (TFile, " on hor "));
        Is_Trace_cmd(Tracing(), hor->Print(TFile));
        Is_Trace(Tracing(),
                (TFile, " to call chi for parm cr%d\n", cr->Coderep_id()));
      }
    } // hor && !Vsa()->Is_special_hor(hor)
    else if (base && hor == NULL) {
      _hva->Set_visit_next(sr);
    }
  } // parm_need_vsym

  return base_vor;
}

// Process_cr<CK_IVAR>: create VSYM for IVAR
template<> VSYM_OBJ_REP*
HVA_VO_CREATION::Process_cr<CK_IVAR>(STMTREP* sr, CODEREP* cr, UINT flag)
{
  Is_True(sr && cr && cr->Kind() == CK_IVAR, ("invalid ivar cr"));

  if (cr->Opr() == OPR_PARM) {
    return Process_param(sr, cr, flag);
  }

  CODEREP* base = sr->Lhs() == cr ? cr->Istr_base() : cr->Ilod_base();
  // create VOR for ICALL vptr/vtable?
  // if we don't need vor for ICALL vptr/vtable, the last kid of ICALL shouldn't visit
  //if (!Vsa()->Is_ivar_need_vsym(cr, sr))
  //  return NULL;

  // create vsym_obj in base
  Process_coderep<VSYM_OBJ_REP*>(sr, base, flag);

  if (cr->Opr() == OPR_MLOAD) {
    // handle MLOAD size
    Process_coderep<VSYM_OBJ_REP*>(sr, cr->Mload_size(), FALSE);
  }

  // check if vor already created
  VSYM_OBJ_REP *vor = Vsa()->Cr_2_vor(cr);
  if (vor != NULL)
    return vor;

  HEAP_OBJ_REP *hor;
  base = Find_ilod_base(base);
  if (base == NULL) {
    hor = Vsa()->Null_hor();
  }
  else {
    hor = _hva->Find_cr_hor(sr, base);
    if (hor == NULL) {
      _hva->Set_visit_next(sr);
      return NULL;
    }
  }
  Is_True(hor != NULL, ("invalid hor"));

  if (!Vsa()->Is_special_hor(hor) &&
      (hor->Attr() == ROR_DEF_BY_LDA || !hor->Is_entry_chi())) {
    Is_True(hor->Attr() != ROR_DEF_BY_ISTORE && hor->Attr() != ROR_DEF_BY_COPY &&
            hor->Attr() != ROR_DEF_BY_DANGLE,
            ("TODO: phi, varphi, vorphi, istore, copy, dangle"));

    VSYM_FLD_REP vfr = Vsa()->Cr_vfr(cr);
    vor = _hva->Create_vsym_obj_use(hor, &vfr);

    if (vfr.Is_any() && !hor->Field_any())
      hor->Set_field_any(TRUE);

    // create vor entry chi def on stmt which defs cur_hor
    if (_hva->Vo_created(vor)) {
      if (vor->Vsym_obj()->Ref_cr() == NULL)
        vor->Vsym_obj()->Set_ref_cr(cr);
    }
  }
  else {
    vor = Vsa()->Null_vor();
  }
  Is_True(vor && hor, ("invalid vor"));
  Is_Trace(Tracing(), (TFile, "VOC[%d]: Create vor ", _hva->Round()));
  Is_Trace_cmd(Tracing(), vor->Print(TFile));
  Is_Trace(Tracing(), (TFile, " on hor "));
  Is_Trace_cmd(Tracing(), hor->Print(TFile));
  Is_Trace(Tracing(),
          (TFile, " to IVAR cr%d:\n", cr->Coderep_id()));
  Vsa()->Enter_cr_vor_map(cr, vor);
  Is_Trace_cmd(Tracing(), cr->Print(2, TFile));
  return vor;
}

// Process_cr<CK_OP>: create VSYM for OP cr
template<> VSYM_OBJ_REP*
HVA_VO_CREATION::Process_cr<CK_OP>(STMTREP* sr, CODEREP* cr, UINT flag)
{
  Is_True(sr && cr && cr->Kind() == CK_OP, ("invalid op cr"));

  for (INT32 i = 0; i < cr->Kid_count(); ++i) {
    Process_coderep<VSYM_OBJ_REP*>(sr, cr->Opnd(i), flag);
  }
  return NULL;
}

// Process_sr<OPR_STID, TRUE>: forward process STID
template<> void
HVA_VO_CREATION::Process_sr<OPR_STID, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_STID, ("invalid stid sr"));

  // check rhs
  Process_coderep<VSYM_OBJ_REP*>(sr, sr->Rhs(), 0);

  CODEREP *lhs = sr->Lhs();
  Is_True(lhs->Kind() == CK_VAR, ("bad lhs cr"));

  AUX_ID lhs_id = lhs->Aux_id();

  if (Vsa()->Cr_2_vor(lhs))
    return;

  HEAP_OBJ *ho = NULL;
  AUX_ID aux_id = lhs_id;
  do {
    ho = Vsa()->Find(aux_id, TRUE);
    if (ho != NULL)
      break;
    aux_id = Opt_stab()->St_group(aux_id);
  } while (aux_id != lhs_id && aux_id != 0);

  if (ho == NULL)
    return;

  Is_True(ho->Kind() == RSC_KIND_LDA, ("bad ho attr"));

  VSYM_FLD_REP vfr = Vsa()->Cr_vfr(lhs);
  VSYM_OBJ_REP *vor = _hva->Create_vsym_obj_def(sr->Bb(),
                                                ho->Entry_chi(),
                                                &vfr,
                                                Defbb_pool());

  if (vor->Vsym_obj()->Ref_cr() == NULL)
    vor->Vsym_obj()->Set_ref_cr(lhs);
  vor->Set_srcpos_node(sr, Dna(), PATHINFO_COPY);
  vor->Set_attr(ROR_DEF_BY_COPY);
  vor->Set_stmt_def(sr, Dna());
  Vsa()->Enter_cr_vor_map(lhs, vor);

  Is_Trace(Tracing(), (TFile, "VOC[%d]: Create vor ", _hva->Round()));
  Is_Trace_cmd(Tracing(), vor->Print(TFile));
  Is_Trace(Tracing(), (TFile, " on hor "));
  Is_Trace_cmd(Tracing(), ho->Entry_chi()->Print(TFile));
  Is_Trace(Tracing(), (TFile, " for STID sym%dv%d cr%d sr%d.\n",
                              lhs->Aux_id(), lhs->Version(),
                              lhs->Coderep_id(), sr->Stmtrep_id()));

  // if sr is memory store, add base hor to pending set
  // all vor based on the hor will add chi in Finalize phase
  if (lhs->Dtyp() == MTYPE_M) {
    STMT_HOR_SET_HELPER ho_set(_pending_stmts, _hva->Local_pool(), _hva, sr);
    HEAP_OBJ_REP *base_hor = ho->Entry_chi();
    if (base_hor != NULL && !Vsa()->Is_special_hor(base_hor)) {
      ho_set.Add(base_hor, lhs, HO_MOD);
    }
  }

  CODEREP *rhs = sr->Rhs();
  BOOL is_alloca = (rhs->Kind() == CK_OP && rhs->Opr() == OPR_ALLOCA) ? TRUE : FALSE;
  rhs = is_alloca ? rhs : Find_ilod_base(rhs);
  HEAP_OBJ_REP *old_hor = Vsa()->Cr_2_heap_obj(sr->Lhs());
  Is_True(is_alloca || old_hor == NULL || old_hor->Heap_obj() != ho,
          ("lhs hor already set"));
  // annotate rhs_hor to lhs vor and set as hor field
  HEAP_OBJ_REP* rhs_hor;
  HEAP_OBJ_REP *lhs_hor = ho->Entry_chi();
  if (rhs != NULL &&
      (rhs_hor = _hva->Find_cr_hor(sr, rhs)) != NULL) {
    vor->Set_hor(rhs_hor);
    //FIELD_OBJ_REP* fl = CXX_NEW(FIELD_OBJ_REP(rhs_hor, FLD_K_ID, 
    //                                          Vsa()->Cr_fldid(lhs), Vsa()->Cr_ofst(lhs)),
    //                            Vsa()->Mem_pool());
    //fl->Set_Next(lhs_hor->Flist());
    //lhs_hor->Set_flist(fl);
  }
}

// Process_sr<OPR_ISTORE, TRUE>: forward pass to create VSYM for ISTORE
template<> void
HVA_VO_CREATION::Process_sr<OPR_ISTORE, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_ISTORE, ("invalid istore sr"));
  Process_istore<TRUE>(sr);
}

// Process_sr<OPR_MSTORE, TRUE>: forward pass to create VSYM for MSTORE
template<> void
HVA_VO_CREATION::Process_sr<OPR_MSTORE, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_MSTORE, ("invalid istore sr"));
  // process mstore size
  Process_coderep<VSYM_OBJ_REP*>(sr, sr->Lhs()->Mstore_size(), FALSE);
  // process istore
  Process_istore<TRUE>(sr);

  STMT_HOR_SET_HELPER ho_set(_pending_stmts, _hva->Local_pool(), _hva, sr);
  // handle vor on lhs
  CODEREP *base = sr->Lhs()->Istr_base();
  HEAP_OBJ_REP *hor = Vsa()->Cr_2_heap_obj(base);
  if (hor != NULL && _hva->Visit_heap_obj(hor)) {
    ho_set.Add(hor, base, HO_MOD);
  }
  // handle vor on rhs
  if (sr->Rhs()->Kind() == CK_IVAR) {
    base = sr->Rhs()->Ilod_base();
    hor = Vsa()->Cr_2_heap_obj(base);
    if (hor != NULL && _hva->Visit_heap_obj(hor)) {
      ho_set.Add(hor, base, HO_REF);
    }
  }
  else {
    Is_True(sr->Rhs()->Kind() == CK_CONST, ("TODP: var or op"));
  }
}

// Process_sr<OPR_OPT_CHI, TRUE>: forward pass to create vsym for OPT_CHI
template<> void
HVA_VO_CREATION::Process_sr<OPR_OPT_CHI, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_OPT_CHI, ("invalid entry_chi sr"));
  // Nothing to do? remove this function later
}

// Process_sr<OPR_CALL, TRUE>: forward pass to create vsym for CALL
template<> void
HVA_VO_CREATION::Process_sr<OPR_CALL, TRUE>(STMTREP* sr)
{
  Is_True(sr->Opr() == OPR_CALL || sr->Opr() == OPR_ICALL, ("not a call"));
  TY_IDX proto_ty = sr->Opr() == OPR_CALL ? ST_type(sr->St())
                                          : sr->Ty();
  Is_True(proto_ty != TY_IDX_ZERO && TY_kind(proto_ty) == KIND_FUNCTION,
          ("not function type"));
  TYLIST_IDX tylist = TY_parms(proto_ty);

  // process rhs
  CODEREP *rhs = sr->Rhs();
  Is_True(rhs->Kind() == CK_OP && OPERATOR_is_call(rhs->Opr()),
          ("bad rhs"));
  INT32 parm_cnt = sr->Opr() == OPR_CALL ? rhs->Kid_count()
                                         : rhs->Kid_count() - 1;
  for (INT32 i = 0; i < parm_cnt; ++i) {
    TY_IDX parm_ty = TYLIST_ty(tylist);
    if (parm_ty != TY_IDX_ZERO) {
      ++ tylist;
    }
    CODEREP *parm = rhs->Opnd(i);
    Is_True(parm->Kind() == CK_IVAR && parm->Opr() == OPR_PARM,
            ("bad parm"));
    Process_param(sr, parm, parm_ty);
  }
  if (sr->Opr() == OPR_ICALL) {
    // process icall target
    Process_coderep<VSYM_OBJ_REP *>(sr, rhs->Opnd(parm_cnt), 0);
  }
  // process call
  Process_call<TRUE>(sr);
}

// Process_sr<OPR_INTRINSIC_CALL, TRUE>: forward pass for INTRINSIC_CALL
template<> void
HVA_VO_CREATION::Process_sr<OPR_INTRINSIC_CALL, TRUE>(STMTREP* sr)
{
  Is_True(sr->Opr() == OPR_INTRINSIC_CALL, ("not a call"));
  // process rhs
  CODEREP *rhs = sr->Rhs();
  Process_coderep<VSYM_OBJ_REP *>(sr, rhs, 0);
  // process call
  Process_call<TRUE>(sr);
}

// Process_sr<OPR_RETURN, TRUE>: forward pass for RETURN
template<> void
HVA_VO_CREATION::Process_sr<OPR_RETURN, TRUE>(STMTREP* sr)
{
  Is_True(sr->Opr() == OPR_RETURN, ("not return stmt"));

  // STMT_HOR_SET for return stmt
  STMT_HOR_SET_HELPER ho_set(_pending_stmts, _hva->Local_pool(), _hva, sr);

  // check hor on return value
  STMTREP* prev = sr->Prev();
  while (prev != NULL && OPERATOR_is_scalar_store(prev->Opr()) &&
         Opt_stab()->Aux_stab_entry(prev->Lhs()->Aux_id())->Is_dedicated_preg()) {
    HEAP_OBJ_REP* hor = _hva->Find_cr_hor(prev, prev->Lhs());
    if (hor != NULL && !Vsa()->Is_special_hor(hor)) {
      ho_set.Add(hor, prev->Lhs(), HO_REF);
    }
    prev = prev->Prev();
  }

  // check var mu on return stmt
  MU_LIST *var_mu_list = sr->Mu_list();
  if (var_mu_list != NULL && !var_mu_list->Is_Empty()) {
    MU_NODE *mu;
    MU_LIST_ITER mu_iter;
    FOR_ALL_NODE(mu, mu_iter, Init(var_mu_list)) {
      // ignore special vsym
      if (mu->Aux_id() == Opt_stab()->Default_vsym() ||
          mu->Aux_id() == Opt_stab()->Return_vsym())
        continue;
      AUX_STAB_ENTRY *aux_entry = Opt_stab()->Aux_stab_entry(mu->Aux_id());
      // no vor on reg/auto
      if (aux_entry->St() == NULL ||
          ST_sclass(aux_entry->St()) == SCLASS_REG ||
          ST_sclass(aux_entry->St()) == SCLASS_AUTO)
        continue;
      // no vor for non-pointer type formal
      if ((ST_sclass(aux_entry->St()) == SCLASS_FORMAL ||
           ST_sclass(aux_entry->St()) == SCLASS_FORMAL_REF) &&
          TY_kind(aux_entry->Ty()) != KIND_POINTER)
        continue;
      HEAP_OBJ_REP *hor = NULL;
      if (aux_entry->Is_global()) {
        CODEREP *opnd = Find_ilod_base(mu->OPND());
        hor = opnd ? _hva->Find_cr_hor(sr, opnd) : NULL;
      } else {
        hor = Vsa()->Cr_2_heap_obj(mu->OPND());
      }
      if (hor != NULL && !Vsa()->Is_special_hor(hor)) {
        ho_set.Add(hor, mu->OPND(), HO_REF);
      }
    }
  }
  // create/update hor for entry chi ho
  CHI_NODE *chi;
  CHI_LIST_ITER chi_iter;
  STMTREP *entry_stmt = _vsa->Get_entry_chi_stmt();
  Is_True(entry_stmt && entry_stmt->Opr() == OPR_OPT_CHI,
          ("bad entry stmt"));
  FOR_ALL_NODE(chi, chi_iter, Init(entry_stmt->Chi_list())) {
    if (!chi->Live())
      continue;
    AUX_STAB_ENTRY *aux_entry = Opt_stab()->Aux_stab_entry(chi->Aux_id());
    // no vor on auto
    if (aux_entry->St() == NULL ||
        ST_sclass(aux_entry->St()) == SCLASS_AUTO)
        continue;
    CODEREP *res = chi->RESULT();
    HEAP_OBJ_REP *hor = Vsa()->Cr_2_heap_obj(res);
    if (hor != NULL) {
      ho_set.Add(hor, res, HO_REF);
    }
  }

  // check hor mu
  MU_LIST *hor_mu_list = Vsa()->Stmt_hor_mu(sr);
  if (hor_mu_list != NULL && !hor_mu_list->Is_Empty()) {
    MU_NODE *hor_mu;
    MU_LIST_ITER hor_mu_iter;
    FOR_ALL_NODE(hor_mu, hor_mu_iter, Init(hor_mu_list)) {
      CHOR *chor = (CHOR*)hor_mu->OPND();
      HEAP_OBJ_REP *hor = chor->first;
      if (hor!= NULL && _hva->Visit_heap_obj(hor)) {
        ho_set.Add(hor, chor->second, HO_REF);
      }
    }
  }
  // others, say escaped hor, vor on return?
}

// ============================================================================
// HVA_VO_CREATION::Process_pending_call
// iterate pending stmt vor mu and chi list to make sure all sub hor been added
// to hor_set.
// The hor_set is initially added in Process_param, but the vor rhs_hor
// may not updated when forward proessing the call stmt
// ============================================================================
void
HVA_VO_CREATION::Process_pending_calls()
{
  if (!_pending_stmts)
    return;
  if (!_hva->Has_vo_created()) {
    return;
  }

  PENDING_STMT_VEC::iterator cend = _pending_stmts->end();
  for (PENDING_STMT_VEC::iterator cit = _pending_stmts->begin();
        cit != cend; ++cit) {
    STMTREP *stmt = (*cit)->Stmtrep();
    HO_MOD_REF_MAP *set = (*cit)->Hor_set();
    Is_True(stmt && set, ("stmt or set is null"));
    if ((stmt == NULL) || (set == NULL) || !OPERATOR_is_call(stmt->Opr())) {
      continue;
    }
    MU_LIST_ITER mu_iter;
    MU_NODE *mnode;
    FOR_ALL_NODE( mnode, mu_iter, Init(_hva->Vsa()->Stmt_vor_mu(stmt))) {
      CVOR *cvor = (CVOR*)mnode->OPND();
      VSYM_OBJ_REP *vor = cvor->first;
      CODEREP *cr = cvor->second;
      HEAP_OBJ_REP *base_hor = vor->Vsym_obj()->Base_hor();
      HO_MOD_REF_MAP::iterator hit = set->find(base_hor);
      if (hit != set->end()) {
        HEAP_OBJ_REP *rhs_hor = vor->Hor();
        if (rhs_hor && !_hva->Vsa()->Is_special_hor(rhs_hor) &&
            set->find(rhs_hor) == set->end()) {
          (*cit)->Add(rhs_hor, cr, HO_REF);
        }
      }
    }

    CHI_LIST_ITER chi_iter;
    CHI_NODE     *cnode;
    FOR_ALL_NODE( cnode, chi_iter, Init(_hva->Vsa()->Stmt_vor_chi(stmt))) {
      CVOR *cvor = (CVOR*)cnode->RESULT();
      VSYM_OBJ_REP *vor = cvor->first;
      CODEREP *cr = cvor->second;
      HEAP_OBJ_REP *base_hor = vor->Vsym_obj()->Base_hor();
      HO_MOD_REF_MAP::iterator hit = set->find(base_hor);
      if (hit != set->end()) {
        HEAP_OBJ_REP *rhs_hor = vor->Hor();
        if (rhs_hor && !_hva->Vsa()->Is_special_hor(rhs_hor) &&
            set->find(rhs_hor) == set->end()) {
          (*cit)->Add(rhs_hor, cr, HO_MOD);
        }
      }
    }
  }
}

void
HVA_VO_CREATION::Process_rets()
{
  STMTR_VECTOR::const_iterator it;
  for (it = Dna()->Rets_list()->begin(); it != Dna()->Rets_list()->end(); it++) {
    STMTREP *ret_stmt = *it;
    Process_sr<OPR_RETURN, TRUE>(ret_stmt);
  }
}

// initialize vsym_obj creation
void
HVA_VO_CREATION::Initialize() {
}

// finalize vsym_obj creation, create vor mu/chi for call and return
void
HVA_VO_CREATION::Finalize() {
  // create vor mu/chi for calls
  Is_True(_pending_stmts, ("pending calls vector is null"));

  // post process hor set on pending stmt
  Process_pending_calls();
  // postpone return processing, waiting for all hor been created in current iteration
  Process_rets();

  // TODO: should vo_updated be processed here?
  if (!_hva->Has_vo_created()) {
    Is_Trace(Tracing(), (TFile, "VOC[%d]: ---------------------------------------------------------------\n", _hva->Round()));
    Is_Trace(Tracing(), (TFile, "VOC[%d]: NO VO created\n", _hva->Round()));
    if (_hva->Has_vo_updated()) {
      Is_Trace_cmd(Tracing(),
                   Dump_ptr_set(TFile, " +VO updated:\n", _hva->Vo_updated()));
    }
    else {
      Is_Trace(Tracing(), (TFile, "VOC[%d]: NO VO updated\n", _hva->Round()));
    }
    Is_Trace(Tracing(), (TFile, "VOC[%d]: ---------------------------------------------------------------\n", _hva->Round()));
    return;
  }

#if 0
  // create vor chi for entry chi
  STMTREP *entry = Vsa()->Get_entry_chi_stmt();
  Is_True(entry && entry->Opr() == OPR_OPT_CHI,
          ("invalid entry chi stmt"));
  STMT_HOR_SET ent_set(entry, _hva->Local_pool());
  CHI_LIST *entry_chi = Vsa()->Stmt_hor_chi(entry);
  if (entry_chi != NULL) {
    CHI_NODE *chi;
    CHI_LIST_ITER chi_iter;
    FOR_ALL_NODE(chi, chi_iter, Init(entry_chi)) {
      CHOR *copnd = (CHOR *)chi->OPND();
      CHOR *cres = (CHOR*)chi->RESULT();
      HEAP_OBJ_REP *hor = cres->first;
      CODEREP *cr = cres->second;
      Is_True(hor->Heap_obj() == copnd->first->Heap_obj(),
              ("Heap obj mismatch"));
      Is_True(cr && cr == copnd->second,
              ("cr mismatch"));
      ent_set.Add(hor, cr, HO_REF);
    }
  }
#endif

  VSYM_OBJ *vobj;
  HASH_SET_ITER<VO_PTR_SET, VSYM_OBJ*> vo_iter;
  // traverse all vsym obj
  PENDING_STMT_VEC::iterator cend = _pending_stmts->end();
  FOR_ALL_NODE(vobj, vo_iter, Init(&_hva->Vo_created())) {
    HEAP_OBJ_REP *base_hor = vobj->Base_hor();
    VSYM_OBJ_REP *vor = vobj->Entry_chi();
    VSYM_FLD_REP vfr = vobj->Fld_rep();
    if (base_hor->Field_any()) {
      // append vor chi for aliased vo/vor
      HOR_VO_LIST_ITER iter(base_hor);
      VSYM_OBJ *alias_vo;
      FOR_ALL_NODE(alias_vo, iter, Init()) {
        if (alias_vo == vobj || !vfr.Aliased(alias_vo->Fld_rep())) {
          continue;
        }
        VSYM_OBJ_REP *alias_vor = alias_vo->Entry_chi();
        BOOL visit_vor = _hva->Visit_vsym_obj(alias_vor);
        Is_True(alias_vor != NULL, ("no entry chi"));
        alias_vor = alias_vor->Next();
        while (alias_vor) {
          if (alias_vor->Attr() == ROR_DEF_BY_COPY ||
              alias_vor->Attr() == ROR_DEF_BY_ISTORE) {
            STMTREP *stmt = alias_vor->Stmt_def();
            Is_True(stmt && OPERATOR_is_store(stmt->Opr()),
                    ("not store"));
            CHI_NODE *chi = _hva->Append_stmt_vor_chi(stmt->Lhs(), stmt, vor, Defbb_pool());
            // mark the alias store as may def
            if (chi) {
              VSYM_OBJ_REP *res_vor = ((CVOR*)chi->RESULT())->first;
              res_vor->Set_vsym_attr(ROR_VSYM_MAY_DEF);
            }
            _hva->Set_visit_later(stmt);
            if (visit_vor == FALSE) {
              visit_vor = TRUE;
              _hva->Set_vo_updated(alias_vo);
            }

            Is_Trace(Tracing(), (TFile, "VOC[%d]: Create chi for vor ", _hva->Round()));
            Is_Trace_cmd(Tracing(), vor->Print(TFile));
            Is_Trace(Tracing(), (TFile, " for alias vor "));
            Is_Trace_cmd(Tracing(), alias_vor->Print(TFile));
            Is_Trace(Tracing(), (TFile, " based on hor "));
            Is_Trace_cmd(Tracing(), base_hor->Print(TFile));
            Is_Trace(Tracing(), (TFile, " on lhs cr%d of %s sr%d.\n",
                                 stmt->Lhs()->Coderep_id(),
                                 OPERATOR_name(stmt->Opr()) + 4,
                                 stmt->Stmtrep_id()));
          }
          alias_vor = alias_vor->Next();
        }
      }
    }

    // if base_hor is created from VARPHI/VORPHI, create
    // related VOR phi which has attr of DEF_BY_HORPHI
    if (base_hor->Attr() == ROR_DEF_BY_VARPHI ||
        base_hor->Attr() == ROR_DEF_BY_VORPHI)
      _hva->Create_vsym_phi_for_hor_phi(vobj, base_hor);

    HEAP_OBJ *ho = base_hor->Heap_obj();
#if 0
    // create stmt vor chi for entry chi
    HO_MOD_REF_MAP::iterator ent_it = ent_set.Ho_set()->find(ho);
    if (ent_it != ent_set.Ho_set()->end()) {
      CODEREP *cr = ent_it->second.first;
      _hva->Append_stmt_vor_chi(cr, entry, vor, Defbb_pool());
    }
#endif

    // traverse all pending calls
    for (PENDING_STMT_VEC::iterator cit = _pending_stmts->begin();
         cit != cend; ++cit) {
      STMTREP *stmt = (*cit)->Stmtrep();
      HO_MOD_REF_MAP *set = (*cit)->Hor_set();
      Is_True(stmt && set, ("stmt or set is null"));
      HO_MOD_REF_MAP::iterator hit = set->find(base_hor);
      if (hit != set->end()) {
        CODEREP  *cr = hit->second.first;
        UINT      mr = hit->second.second;
        if ((mr & HO_REF) || ((mr & ANY_VO_REF) && vfr.Is_any()))
          _hva->Append_stmt_vor_mu(cr, stmt, vor);
        if ((mr & HO_MOD) || ((mr & ANY_VO_MOD) && vfr.Is_any()))
          _hva->Append_stmt_vor_chi(cr, stmt, vor, Defbb_pool());

        Is_Trace(Tracing(),
                 (TFile, "VOC[%d]: Create %s for vor ", _hva->Round(),
                  mr == (HO_REF | HO_MOD) ? "mu/chi"
                      : mr == HO_REF ? "mu"
                          : mr == HO_MOD ? "chi" : "-err-"));
        Is_Trace_cmd(Tracing(), vor->Print(TFile));
        Is_Trace(Tracing(), (TFile, " on cr%d of %s sr%d.\n",
                             cr->Coderep_id(),
                             OPERATOR_name(stmt->Opr()) + 4,
                             stmt->Stmtrep_id()));
      }
    }
  }

  // dump vo created or updated in this iteration
#ifdef Is_True_On
  Vsa()->Verify_heap_obj_stack();
#endif
  Is_Trace(Tracing(), (TFile, "VOC[%d]: ---------------------------------------------------------------\n", _hva->Round()));
  Is_Trace(Tracing(), (TFile, "VOC[%d]: FINISH HVA_VO_CREATION.\n", _hva->Round()));
  Is_Trace_cmd(Tracing(),
               Dump_ptr_set(TFile, "+vsym obj created:\n", _hva->Vo_created()));
  Is_Trace_cmd(Tracing(),
               Dump_ptr_set(TFile, "+vsym obj updated:\n", _hva->Vo_updated()));
  Is_Trace(Tracing(), (TFile, "VOC[%d]: ---------------------------------------------------------------\n", _hva->Round()));

}  // HVA_VO_CREATION::Finalize

// ============================================================================
// HVA_VO_RENAMING
//
// Traverse DOM tree and rename VSYM
// ============================================================================
class HVA_VO_RENAMING : public CFG_VISITOR_BASE<HVA_VO_RENAMING> {
public:
  // set visit flag
  enum {
    // visit LDA, CONST, VAR, IVAR and OP CODEREP
    CR_VFLAG = V_LDA | V_CONST | V_VAR | V_IVAR | V_OP,
    // visit store and call STMTREP
    SR_VFLAG = V_ANY_STORE | V_ANY_CALL | V_ANY_BRANCH | V_OPT_CHI | V_RETURN | V_ASM_STMT,
  };

  // constructor
  HVA_VO_RENAMING(HEAP_VSYM_ANALYSIS *hva)
   : CFG_VISITOR_BASE(hva, this) { }

private:
  // process call
  template<BOOL _FWD>
  void Process_call(STMTREP *sr);

  // process istore
  template<BOOL _FWD>
  void Process_istore(STMTREP *sr, BOOL handle_rhs);

public:
  // Enter_bb: before enter the bb, rename the vor phi result
  void Enter_bb(BB_NODE *bb) {
    Is_True(bb != NULL, ("invalid bb"));

    PHI_LIST* phi_list = Vsa()->Bb_vo_philist(bb);
    if (!phi_list)
      return;
    PHI_NODE     *phi;
    PHI_LIST_ITER phi_iter;
    FOR_ALL_ELEM (phi, phi_iter, Init(phi_list)) {
      VSYM_OBJ_REP *vor = (VSYM_OBJ_REP*)phi->RESULT();
      Is_True(vor, ("invalid vor phi result"));
      if (!_hva->Visit_vsym_obj(vor))
        continue;
      IDTYPE version = _hva->Gen_name(vor, NULL);
      //vor->Set_phi_def(phi);
      Is_True(vor->Phi_def() == phi, ("phi def mismatch"));
      Is_Trace(Tracing(), (TFile, "VOR[%d]: push vor phi result ", _hva->Round()));
      Is_Trace_cmd(Tracing(), vor->Print(TFile));
      Is_Trace(Tracing(), (TFile, " in BB%d.\n", bb->Id()));
    }
  }

  // Enter_dom_bb: before enter the dom bb, rename the vor phi opnd
  void Enter_dom_bb(BB_NODE* bb)
  {
    BB_NODE*     succ;
    BB_LIST_ITER bb_iter;
    FOR_ALL_ELEM (succ, bb_iter, Init(bb->Succ())) {
      PHI_LIST* phi_list = Vsa()->Bb_vo_philist(succ);
      if (!phi_list)
        continue;
      INT pos = succ->Pred()->Pos(bb);
      Is_True(pos != -1, ("invalid pos %d", pos));
      PHI_NODE     *phi;
      PHI_LIST_ITER phi_iter;
      FOR_ALL_ELEM (phi, phi_iter, Init(phi_list)) {
        VSYM_OBJ_REP* opnd = Phi_opnd_mismatch(phi)
                               ? (VSYM_OBJ_REP *) phi->OPND(pos)
                               : (VSYM_OBJ_REP *) phi->RESULT();
        Is_True(opnd != NULL, ("found null vor phi opnd"));
        Is_True(Phi_opnd_mismatch(phi) ||
                opnd->Vsym_obj() == ((VSYM_OBJ_REP*)phi->RESULT())->Vsym_obj(),
                ("vo mismatch"));

        // Set rhs hor for phi results
        if (!Phi_opnd_mismatch(phi)) {
          VSYM_OBJ_REP *real_opnd = (VSYM_OBJ_REP *) phi->OPND(pos);
          if (real_opnd && !Vsa()->Is_special_vor(real_opnd))
          {
            VSYM_OBJ_REP *res = (VSYM_OBJ_REP*)phi->RESULT();
            if (!res->Hor() && real_opnd->Hor() && !Vsa()->Is_special_hor(real_opnd->Hor())) {
              res->Set_hor(real_opnd->Hor());
            }
          }
        }
        if (!_hva->Visit_vsym_obj(opnd))
          continue;

        VSYM_OBJ_REP *tos = opnd->Vsym_obj()->Top_of_stack();
        Is_True(((tos->Attr() == ROR_DEF_BY_PHI ||
                  tos->Attr() == ROR_DEF_BY_HORPHI) &&
                 tos->Phi_def() != NULL) ||
                tos->Stmt_def() != NULL ||
                tos->Is_entry_chi(),
                ("vor not defined"));
        phi->Set_opnd(pos, (CODEREP*)tos);
        Is_Trace(Tracing(),
                 (TFile, "VOR[%d]: update opnd %d for vor ",
                         _hva->Round(), pos));
        Is_Trace_cmd(Tracing(), opnd->Print(TFile));
        Is_Trace(Tracing(), (TFile, " to vor "));
        Is_Trace_cmd(Tracing(), tos->Print(TFile));
        Is_Trace(Tracing(), (TFile, " from BB%d to BB%d.\n",
                                    bb->Id(), succ->Id()));
      }
    }
  }

  // Exit_bb: when exit the bb, pop the vor phi result
  void Exit_bb(BB_NODE *bb) {
    Is_True(bb != NULL, ("invalid bb"));

    PHI_LIST* phi_list = Vsa()->Bb_vo_philist(bb);
    if (!phi_list)
      return;
    PHI_NODE     *phi;
    PHI_LIST_ITER phi_iter;
    FOR_ALL_ELEM (phi, phi_iter, Init(phi_list)) {
      VSYM_OBJ_REP *vor = (VSYM_OBJ_REP*)phi->RESULT();
      if (!_hva->Visit_vsym_obj(vor))
        continue;
      Is_True(vor == vor->Vsym_obj()->Top_of_stack(), ("stack mismatch"));
      vor->Vsym_obj()->Pop();
      Is_Trace(Tracing(), (TFile, "VOR[%d]: pop vor phi result ", _hva->Round()));
      Is_Trace_cmd(Tracing(), vor->Print(TFile));
      Is_Trace(Tracing(), (TFile, " in BB%d.\n", bb->Id()));
    }
  }

  // Process_stmt_fwd: forward process the stmtrep
  void Process_stmt_fwd(STMTREP *sr);

  // Process_stmt_rev: backward process the stmtrep
  void Process_stmt_rev(STMTREP* sr);

  // process rhs of the stmtrep
  template<OPERATOR opr, BOOL _FWD>
  void Process_sr(STMTREP* sr) {
    Is_True(sr->Lhs() == NULL, ("TODO: %s", OPERATOR_name(sr->Opr()) + 4));
    if (sr->Rhs())
      Process_coderep<void>(sr, sr->Rhs(), FALSE);
  }

  // process coderep
  template<CODEKIND kind>
  void Process_cr(STMTREP* sr, CODEREP* cr, UINT flag) { }

  // perform VSYM_OBJ_REP renaming phase
  void Perform() {
    DOM_WALKER<HVA_VO_RENAMING> walker(_comp_unit->Cfg(), this);
    walker.Perform();
  }

  // initialize VSYM_OBJ_REP renaming phase
  void Initialize();

  // finalize VSYM_OBJ_REP renaming phase
  void Finalize();

};  // HVA_VO_RENAMING

// Process_call<TRUE>: forward pass to rename vsym for call
template<> void
HVA_VO_RENAMING::Process_call<TRUE>(STMTREP* sr)
{
  Is_True(sr && OPERATOR_is_call(sr->Opr()), ("invalid call sr"));
  // vor mu and chi are handled in Process_stmt_fwd()
  // only rename the rhs
  Process_coderep<void>(sr, sr->Rhs(), FALSE);
}

// Process_call<FALSE>: reverse pass to rename vsym for call
template<> void
HVA_VO_RENAMING::Process_call<FALSE>(STMTREP* sr)
{
  Is_True(sr && OPERATOR_is_call(sr->Opr()), ("invalid call sr"));
  // vor mu and chi are handled in Process_stmt_rev()
}

// Process_istore<TRUE>: forward pass to rename vsym for istore
template<> void
HVA_VO_RENAMING::Process_istore<TRUE>(STMTREP* sr, BOOL handle_rhs)
{
  Is_True(sr &&
          (OPERATOR_is_scalar_istore(sr->Opr()) ||
           sr->Opr() == OPR_MSTORE), ("invalid istore sr"));

  // rename rhs
  if (handle_rhs) {
    CODEREP *rhs = sr->Rhs();
    Process_coderep<void>(sr, rhs, FALSE);
  }

  // rename lhs
  CODEREP *lhs = sr->Lhs();
  CODEREP *base = lhs->Istr_base();
  Process_coderep<void>(sr, base, FALSE);

  VSYM_OBJ_REP* vor = Vsa()->Cr_2_vor(lhs);
  if (vor == NULL) {
    // check base hor
    CODEREP *base_cr = Find_ilod_base(base);
    HEAP_OBJ_REP *hor = base_cr ? Vsa()->Find_cr_heap_obj(base_cr)
                                : Vsa()->Null_hor();
    if (Vsa()->Is_special_hor(hor)) {
      Vsa()->Enter_cr_vor_map(lhs, Vsa()->Null_vor());
      Is_Trace(Tracing(), (TFile, "VOR[%d]: Create vor ", _hva->Round()));
      Is_Trace_cmd(Tracing(), vor->Print(TFile));
      Is_Trace(Tracing(), (TFile, " on hor "));
      Is_Trace_cmd(Tracing(), hor->Print(TFile));
      Is_Trace(Tracing(),
               (TFile, " to IVAR cr%d:\n", lhs->Coderep_id()));
      Is_Trace_cmd(Tracing(), lhs->Print(2, TFile));
      return;
    }
    // not found vor, so base shouldn't have a hor, or the
    // hor is created just now
    VSYM_OBJ_REP *vor;
    Is_True(hor == NULL || _hva->Visit_heap_obj(hor) ||
            base_cr->Kind() == CK_VAR ||
            (base_cr->Kind() == CK_IVAR &&
             ((vor = Vsa()->Cr_2_vor(base_cr)) == NULL ||
              _hva->Visit_vsym_obj(vor))), ("no vor found"));
    // the sr should be visited next time
    _hva->Set_visit_next(sr);
  }
  else if (_hva->Visit_vsym_obj(vor)) {
    // TODO: if any doesn't overwrite the vor being visited, check prev vor
    //if (vor->Vsym_obj()->Fld_rep().Is_any())
    //  vor->Set_prev(vor->Vsym_obj()->Top_of_stack());
    _hva->Gen_name(vor, sr);
    Is_Trace(Tracing(), (TFile, "VOR[%d]: push vor ", _hva->Round()));
    Is_Trace_cmd(Tracing(), vor->Print(TFile));
    Is_Trace(Tracing(),
             (TFile, " to sr%d %s.\n",
                     sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4));
#if 0
    HEAP_OBJ_REP* rhs_hor = Vsa()->Find_stmt_hor_mu(sr, sr->Lhs());
    if (rhs_hor != NULL) {
      vor->Set_hor(rhs_hor);
    }
#endif
  }
}

// Process_sr<OPR_ISTORE, FALSE>: reverse pass to rename vsym for ISTORE
template<> void
HVA_VO_RENAMING::Process_istore<FALSE>(STMTREP* sr, BOOL handle_rhs)
{
  Is_True(sr &&
          (OPERATOR_is_scalar_istore(sr->Opr()) ||
           sr->Opr() == OPR_MSTORE), ("invalid istore sr"));

  VSYM_OBJ_REP* vor = Vsa()->Cr_2_vor(sr->Lhs());
  if (vor != NULL &&
      !Vsa()->Is_special_vor(vor) &&
      vor->Vsym_obj()->Top_match_sr(sr)) {
    vor->Vsym_obj()->Pop();
    Is_Trace(Tracing(), (TFile, "VOR[%d]: pop vor istore lhs ", _hva->Round()));
    Is_Trace_cmd(Tracing(), vor->Print(TFile));
    Is_Trace(Tracing(),
             (TFile, " from lhs cr%d of sr%d %s.\n",
                     sr->Lhs()->Coderep_id(), sr->Stmtrep_id(),
                     OPERATOR_name(sr->Opr()) + 4));
  }
}

// Process_stmt_fwd: forward process the stmtrep
void
HVA_VO_RENAMING::Process_stmt_fwd(STMTREP* sr)
{
  // Process stmt vo mu list
  MU_LIST *mu_list = Vsa()->Stmt_vor_mu(sr);
  if (mu_list) {
    typedef hash_map<HEAP_OBJ_REP*, CODEREP*,
                     HASHER<HEAP_OBJ_REP>, std::equal_to<HEAP_OBJ_REP*> > HOR_MAP;
    HOR_MAP      hor_map;
    MU_NODE     *mnode;
    MU_LIST_ITER mu_iter;
    FOR_ALL_NODE (mnode, mu_iter, Init(Vsa()->Stmt_vor_mu(sr))) {
      CVOR *cvor = (CVOR*)mnode->OPND();
      VSYM_OBJ_REP *vor = cvor->first;
      if (!_hva->Visit_vsym_obj(vor))
        continue;
      VSYM_OBJ_REP *tos = vor->Vsym_obj()->Top_of_stack();
      if (vor != tos) {
        HEAP_OBJ_REP *tos_hor = tos->Hor();
        if (tos_hor && !Vsa()->Is_special_hor(tos_hor) && !vor->Hor()) {
          hor_map[tos_hor] = cvor->second;
        }
        cvor->first = tos;
        Is_Trace(Tracing(), (TFile, "VOR[%d]: set mu opnd ",  _hva->Round()));
        Is_Trace_cmd(Tracing(), tos->Print(TFile));
        Is_Trace(Tracing(), (TFile, " in sr%d %s.\n",
                             sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4));
      }
    }
    // append newly updated rhs hor's fld_vor to vor mu list
    HOR_MAP::iterator hor_iter;
    for (hor_iter = hor_map.begin(); hor_iter != hor_map.end(); hor_iter++) {
      HEAP_OBJ_REP *hor = hor_iter->first;
      HOR_VO_LIST_ITER iter(hor);
      VSYM_OBJ *fld_vo;
      FOR_ALL_NODE(fld_vo, iter, Init()) {
        if (!_hva->Find_stmt_vor_mu(sr, fld_vo)) {
          VSYM_OBJ_REP *fld_vor = fld_vo->Top_of_stack();
          if (_hva->Visit_vsym_obj(fld_vor)) {
            _hva->Append_stmt_vor_mu(hor_iter->second ,sr, fld_vor);
          }
        }
      }
    }
  }

  // Process stmt
  // if stmt is marked to be visited, visit the stmt include the rhs. otherwise
  // only visit istore/mstore because it may generate new version for VO
  Process_stmtrep<TRUE>(sr);
  if ((OPERATOR_is_scalar_istore(sr->Opr()) ||
       sr->Opr() == OPR_MSTORE) && !Visit_stmt(sr))
    Process_istore<TRUE>(sr, FALSE);

  // Process stmt vo chi list
  CHI_LIST *chi_list = Vsa()->Stmt_vor_chi(sr);
  if (chi_list) {
    CHI_NODE     *cnode;
    CHI_LIST_ITER chi_iter;
    FOR_ALL_NODE (cnode, chi_iter, Init(chi_list)) {
      CVOR* cvor = (CVOR*)cnode->OPND();
      VSYM_OBJ_REP* vor = cvor->first;
      if (!_hva->Visit_vsym_obj(vor))
        continue;
      VSYM_OBJ_REP* tos = vor->Vsym_obj()->Top_of_stack();
      if (vor != tos) {
        cvor->first = tos;
        Is_Trace(Tracing(), (TFile, "VOR[%d]: set chi opnd ",  _hva->Round()));
        Is_Trace_cmd(Tracing(), tos->Print(TFile));
        Is_Trace(Tracing(), (TFile, " in sr%d %s.\n",
                             sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4));
      }
      cvor = (CVOR*)cnode->RESULT();
      vor = cvor->first;
      IDTYPE version = _hva->Gen_name(vor, sr);
      // propagate rhs hor from opnd vor
      HEAP_OBJ_REP *opnd_rhor = tos->Hor();
      if (opnd_rhor && !Vsa()->Is_special_hor(opnd_rhor) && !vor->Hor()) {
        vor->Set_hor(opnd_rhor);
      }
      Is_Trace(Tracing(), (TFile, "VOR[%d]: push chi result ",  _hva->Round()));
      Is_Trace_cmd(Tracing(), vor->Print(TFile));
      Is_Trace(Tracing(), (TFile, " in sr%d %s.\n",
                           sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4));
    }
  }
}

// Process_stmt_rev: backward process the stmtrep
void
HVA_VO_RENAMING::Process_stmt_rev(STMTREP* sr)
{
  // Process stmt vo chi list
  CHI_LIST *chi_list = Vsa()->Stmt_vor_chi(sr);
  if (chi_list) {
    CHI_NODE     *cnode;
    CHI_LIST_ITER chi_iter;
    FOR_ALL_NODE (cnode, chi_iter, Init(Vsa()->Stmt_vor_chi(sr))) {
      CVOR* cvor = (CVOR*)cnode->RESULT();
      VSYM_OBJ_REP* vor = cvor->first;
      Is_True(!Vsa()->Is_special_vor(vor), ("TODO: special vor"));
      if (vor->Vsym_obj()->Top_match_sr(sr)) {
        vor->Vsym_obj()->Pop();
        Is_Trace(Tracing(), (TFile, "VOR[%d]: pop chi result ",  _hva->Round()));
        Is_Trace_cmd(Tracing(), vor->Print(TFile));
        Is_Trace(Tracing(), (TFile, " in sr%d %s.\n",
                             sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4));
      }
    }
  }

  // Process stmt
  // if stmt is marked to be visited, visit the stmt include the rhs. otherwise
  // only visit istore/mstore because it may generate new version for VO
  Process_stmtrep<FALSE>(sr);
  if ((OPERATOR_is_scalar_istore(sr->Opr()) ||
       sr->Opr() == OPR_MSTORE) && !Visit_stmt(sr))
    Process_istore<FALSE>(sr, FALSE);

  // nothing to do with stmt vor mu
}

// Process_cr<CK_VAR>: rename vsym on IVAR
template<> void
HVA_VO_RENAMING::Process_cr<CK_IVAR>(STMTREP* sr, CODEREP* cr, UINT flag)
{
  Is_True(sr && cr && cr->Kind() == CK_IVAR, ("invalid ivar cr"));

  if (cr->Opr() == OPR_PARM) {
    return Process_coderep<void>(sr, cr->Ilod_base(), flag);
  }

  CODEREP* base = (cr == sr->Lhs()) ? cr->Istr_base() : cr->Ilod_base();
  Process_coderep<void>(sr, base, flag);

  if (cr->Opr() == OPR_MLOAD)
    Process_coderep<void>(sr, cr->Mload_size(), flag);

  // rename VOR for ICALL vptr/vtable?
  // if we don't need vor for ICALL vptr/vtable, the last kid of ICALL shouldn't visit
  //if (!Vsa()->Is_ivar_need_vsym(cr, sr))
  //  return;

  VSYM_OBJ_REP* vor = Vsa()->Cr_2_vor(cr);
  if (vor == NULL) {
    // check base hor
    CODEREP *base_cr = Find_ilod_base(base);
    HEAP_OBJ_REP *hor = base_cr ? Vsa()->Find_cr_heap_obj(base_cr)
                                : Vsa()->Null_hor();
    if (Vsa()->Is_special_hor(hor)) {
      vor = Vsa()->Null_vor();
      Vsa()->Enter_cr_vor_map(cr, vor);
      Is_Trace(Tracing(), (TFile, "VOR[%d]: Create vor ", _hva->Round()));
      Is_Trace_cmd(Tracing(), vor->Print(TFile));
      Is_Trace(Tracing(), (TFile, " on hor "));
      Is_Trace_cmd(Tracing(), hor->Print(TFile));
      Is_Trace(Tracing(),
               (TFile, " to IVAR cr%d:\n", cr->Coderep_id()));
      Is_Trace_cmd(Tracing(), cr->Print(2, TFile));
      return;
    }
    // not found vor, so base shouldn't have a hor, or the
    // hor is created just now
    VSYM_OBJ_REP *vor;
    Is_True(hor == NULL || _hva->Visit_heap_obj(hor) ||
            (base_cr->Kind() == CK_IVAR &&
             ((vor = Vsa()->Cr_2_vor(base_cr)) == NULL ||
              _hva->Visit_vsym_obj(vor))), ("no vor found"));
  }
  else if (_hva->Visit_vsym_obj(vor)) {
    VSYM_OBJ_REP *cur_vor = vor->Vsym_obj()->Top_of_stack();
    if (vor != cur_vor) {
      Vsa()->Enter_cr_vor_map(cr, cur_vor);
      if (cur_vor->Hor()) {
        // replace existing heap_obj on cr
        Vsa()->Enter_cr_heap_obj_map(cr, cur_vor->Hor(), TRUE);
      }
    }

    // Add alias vor mu if version mismatch
    //   ho3 = phi(ho1, ho2)
    //   vo1v3(ho1) = phi(vo1v1, vo1v2)
    //   vo3v2(ho3) = phi(vo1v3, vo2v1)
    //   chi(vo3v3, vo1v4)
    // before:
    //   call(vo3v3)
    // after: add mu for vo1v4
    //   mu(vo1v4)
    //   call(vo3v3)
    if (cur_vor->Attr() == ROR_DEF_BY_HORPHI) {
      HEAP_OBJ_REP *base_hor = cur_vor->Vsym_obj()->Base_hor();
      HOR_LIST *ulist = base_hor->Ulist();
      if (ulist) {
        HEAP_OBJ_REP *cur_hor;
        HOR_LIST_ITER hor_list_iter;
        BB_NODE *bb  = cur_vor->Phi_def()->Bb();
        FOR_ALL_NODE(cur_hor, hor_list_iter, Init(ulist)) {
          Is_True(!Vsa()->Is_special_hor(cur_hor), ("special hor"));
          VSYM_OBJ *alias_vo = Vsa()->Find(cur_hor, cur_vor->Vsym_obj()->Fld_rep_ptr());
          if (alias_vo && _hva->Visit_vsym_obj(alias_vo->Top_of_stack())) {
            PHI_NODE *phi_node = _hva->Search_phi_node(_hva->Vo_phi_cache(),
                                                       Vsa()->Bb_vo_philist(bb),
                                                       bb, alias_vo);
            if (phi_node) {
              VSYM_OBJ_REP *alias_vor = (VSYM_OBJ_REP*) phi_node->RESULT();
              VSYM_OBJ_REP *alias_cur_vor = alias_vo->Top_of_stack();
              if (alias_vor != alias_cur_vor) {
                _hva->Append_stmt_vor_mu(cr, sr, alias_cur_vor);
              }
            }
          }
        }
      }
    }
  }
}

// Process_cr<CK_OP>: rename vsym on OP
template<> void
HVA_VO_RENAMING::Process_cr<CK_OP>(STMTREP* sr, CODEREP* cr, UINT flag)
{
  Is_True(sr && cr && cr->Kind() == CK_OP, ("invalid op cr"));

  for (INT32 i = 0; i < cr->Kid_count(); ++i) {
    Process_coderep<void>(sr, cr->Opnd(i), flag);
  }
}

// Process_sr<OPR_STID, TRUE>: forward pass to rename vsym for STID
template<> void
HVA_VO_RENAMING::Process_sr<OPR_STID, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_STID, ("invalid stid sr"));

  // rename rhs
  Process_coderep<void>(sr, sr->Rhs(), 0);

  VSYM_OBJ_REP* vor = Vsa()->Cr_2_vor(sr->Lhs());
  if (vor != NULL && _hva->Visit_vsym_obj(vor)) {
    _hva->Gen_name(vor, sr);
    Is_Trace(Tracing(), (TFile, "VOR[%d]: push vor ", _hva->Round()));
    Is_Trace_cmd(Tracing(), vor->Print(TFile));
    Is_Trace(Tracing(),
             (TFile, " to sr%d %s.\n",
                     sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4));
#if 0
    HEAP_OBJ_REP* hor = Vsa()->Find_stmt_hor_mu(sr, sr->Lhs());
    if (hor != NULL) {
      vor->Set_hor(hor);
    }
#endif
  }
  // add any vor mu for tag propagation
  if (VSA_Enable_TAG && !VSA_Enable_TAG_OLD) {
    HEAP_OBJ_REP *rhs_hor = Vsa()->Cr_2_heap_obj(sr->Rhs());
    if (rhs_hor && !Vsa()->Is_special_hor(rhs_hor)) {
      HOR_VO_LIST_ITER iter(rhs_hor);
      VSYM_OBJ *vo;
      FOR_ALL_NODE(vo, iter, Init()) {
        if (vo->Fld_rep().Is_any()) {
          VSYM_OBJ_REP *any_vor = vo->Top_of_stack();
          if (_hva->Visit_vsym_obj(any_vor)) {
            _hva->Append_stmt_vor_mu(sr->Rhs(), sr, any_vor);
          }
        }
      }
    }
  }
}

// Process_sr<OPR_STID, FALSE>: reverse pass to rename vsym for STID
template<> void
HVA_VO_RENAMING::Process_sr<OPR_STID, FALSE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_STID, ("invalid stid sr"));

  VSYM_OBJ_REP* vor = Vsa()->Cr_2_vor(sr->Lhs());
  if (vor != NULL &&
      _hva->Visit_vsym_obj(vor) &&
      vor->Vsym_obj()->Top_match_sr(sr)) {
    vor->Vsym_obj()->Pop();
    Is_Trace(Tracing(), (TFile, "VOR[%d]: pop vor ", _hva->Round()));
    Is_Trace_cmd(Tracing(), vor->Print(TFile));
    Is_Trace(Tracing(),
             (TFile, " to sr%d %s.\n",
                     sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4));
  }
}

// Process_sr<OPR_ISTORE, TRUE>: forward pass to rename vsym for ISTORE
template<> void
HVA_VO_RENAMING::Process_sr<OPR_ISTORE, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_ISTORE, ("invalid istore sr"));
  Process_istore<TRUE>(sr, TRUE);
}

// Process_sr<OPR_ISTORE, FALSE>: reverse pass to rename vsym for ISTORE
template<> void
HVA_VO_RENAMING::Process_sr<OPR_ISTORE, FALSE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_ISTORE, ("invalid istore sr"));
  Process_istore<FALSE>(sr, FALSE);
}

// Process_sr<OPR_MSTORE, TRUE>: forward pass to rename vsym for MSTORE
template<> void
HVA_VO_RENAMING::Process_sr<OPR_MSTORE, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_MSTORE, ("invalid mstore sr"));
  // process mstore size
  Process_coderep<void>(sr, sr->Lhs()->Mstore_size(), FALSE);
  // process istore
  Process_istore<TRUE>(sr, TRUE);
}

// Process_sr<OPR_MSTORE, FALSE>: reverse pass to rename vsym for MSTORE
template<> void
HVA_VO_RENAMING::Process_sr<OPR_MSTORE, FALSE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_MSTORE, ("invalid mstore sr"));
  Process_istore<FALSE>(sr, FALSE);
}

// Process_sr<OPR_OPT_CHI, TRUE>: forward pass to rename vsym for OPT_CHI
template<> void
HVA_VO_RENAMING::Process_sr<OPR_OPT_CHI, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_OPT_CHI, ("invalid entry_chi sr"));
  // Do nothing, already handled in Process_stmtrep<TRUE>() for mu list
  //Vsa()->Rename_entry_vsym_chi(sr, Defbb_pool(), TRUE);
}

// Process_sr<OPR_OPT_CHI, FALSE>: reverse pass to rename vsym for OPT_CHI
template<> void
HVA_VO_RENAMING::Process_sr<OPR_OPT_CHI, FALSE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_OPT_CHI, ("invalid entry_chi sr"));
  // Do nothing, already handled in Process_stmtrep<TRUE>() for mu list
  //Vsa()->Rename_entry_vsym_chi(sr, Defbb_pool(), FALSE);
}

// Process_sr<OPR_CALL, TRUE>: forward pass to rename vsym for CALL
template<> void
HVA_VO_RENAMING::Process_sr<OPR_CALL, TRUE>(STMTREP* sr)
{
  Is_True(sr->Opr() == OPR_CALL || sr->Opr() == OPR_ICALL, ("not a call"));
  Process_call<TRUE>(sr);
}

// Process_sr<OPR_CALL, FALSE>: reverse pass to rename vsym for CALL
template<> void
HVA_VO_RENAMING::Process_sr<OPR_CALL, FALSE>(STMTREP* sr)
{
  Is_True(sr->Opr() == OPR_CALL || sr->Opr() == OPR_ICALL, ("not a call"));
  Process_call<FALSE>(sr);
}

// Process_sr<OPR_INTRINSIC_CALL, TRUE>: forward pass to rename vsym for intrn_call
template<> void
HVA_VO_RENAMING::Process_sr<OPR_INTRINSIC_CALL, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_INTRINSIC_CALL, ("invalid intrn_call sr"));
  Process_call<TRUE>(sr);
}

// Process_sr<OPR_INTRINSIC_CALL, FALSE>: reverse pass to rename vsym for intrn_call
template<> void
HVA_VO_RENAMING::Process_sr<OPR_INTRINSIC_CALL, FALSE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_INTRINSIC_CALL, ("invalid intrn_call sr"));
  Process_call<FALSE>(sr);
}

// Process_sr<OPR_RETURN, TRUE>: forward pass to rename vsym for RETURN
template<> void
HVA_VO_RENAMING::Process_sr<OPR_RETURN, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_RETURN, ("invalid return sr"));
  // add vor mu for vor based on rhs hor and rhs ulist
  MU_LIST *mu_list = Vsa()->Stmt_vor_mu(sr);
  if (mu_list) {
    MU_NODE     *mnode;
    MU_LIST_ITER mu_iter;
    FOR_ALL_NODE (mnode, mu_iter, Init(Vsa()->Stmt_vor_mu(sr))) {
      CVOR *cvor = (CVOR*)mnode->OPND();
      VSYM_OBJ_REP *vor = cvor->first;
      HEAP_OBJ_REP *rhs_hor = vor->Hor();
      if (rhs_hor && !_hva->Vsa()->Is_special_hor(rhs_hor)) {
        HOR_VO_LIST_ITER iter(rhs_hor);
        VSYM_OBJ *fld_vo;
        FOR_ALL_NODE(fld_vo, iter, Init()) {
          // find current version
          VSYM_OBJ_REP *fld_vor = fld_vo->Top_of_stack();
          if (_hva->Visit_vsym_obj(fld_vor)) {
            _hva->Append_stmt_vor_mu(cvor->second, sr, fld_vor);
          }
        }
        HOR_LIST *ulist = rhs_hor->Ulist();
        if (ulist) {
          HEAP_OBJ_REP *cur_hor;
          HOR_LIST_ITER hor_list_iter;
          FOR_ALL_NODE(cur_hor, hor_list_iter, Init(ulist)) {
            if (!_hva->Vsa()->Is_special_hor(cur_hor)) {
              HOR_VO_LIST_ITER iter(cur_hor);
              VSYM_OBJ *fld_vo;
              FOR_ALL_NODE(fld_vo, iter, Init()) {
                // find current version
                VSYM_OBJ_REP *fld_vor = fld_vo->Top_of_stack();
                if (_hva->Visit_vsym_obj(fld_vor)) {
                  _hva->Append_stmt_vor_mu(cvor->second, sr, fld_vor);
                }
              }
            }
          }
        }
      }
    }
  }
}

// initialize VSYM_OBJ_REP renaming phase
void
HVA_VO_RENAMING::Initialize() {
  // place vsym_obj phi
  VSA::PHILIST_MAP *bb_ro_philist = _vsa->Bb_vo_philist();
  VSYM_OBJ         *vsym_obj;
  VSYM_OBJ_REP     *vor;
  HASH_SET_ITER<VO_PTR_SET, VSYM_OBJ*> iter;
  if (_hva->Has_vo_updated()) {
    // insert or update phi for heap_obj updated
    _vsa->Place_ro_phi_node(bb_ro_philist, vsym_obj, &_hva->Vo_updated(), &iter, vor, TRUE, _hva->Vo_phi_cache());
  }
  if (_hva->Has_vo_created()) {
    // insert phi for vsym_obj created
    // has to set last param TRUE here because for VO created for horphi, the
    // phi node has been added
    _vsa->Place_ro_phi_node(bb_ro_philist, vsym_obj, &_hva->Vo_created(), &iter, vor, TRUE, _hva->Vo_phi_cache());
  }
}

void
HVA_VO_RENAMING::Finalize() {
  // verify renaming stack
#ifdef Is_True_On
  Vsa()->Verify_heap_obj_stack();
#endif
  Is_Trace(Tracing(), (TFile, "VOR[%d]: ---------------------------------------------------------------\n", _hva->Round()));
  Is_Trace(Tracing(), (TFile, "VOR[%d]: FINISH HVA_VO_RENAMING.\n", _hva->Round()));
  //Is_Trace_cmd(Tracing(), Ipsa()->Print_fld_name_map(TFile));
  //Is_Trace_cmd(Tracing(), Vsa()->Print_hor(TFile));
  Is_Trace(Tracing(), (TFile, "VOR[%d]: ---------------------------------------------------------------\n", _hva->Round()));
  Is_Trace_cmd(Tracing(), Vsa()->Print_hor(TFile));
}


// ============================================================================
// HEAP_VSYM_ANALYSIS::Find_cr_hor
// find hor on cr
// ============================================================================
HEAP_OBJ_REP *
HEAP_VSYM_ANALYSIS::Find_cr_hor(STMTREP *sr, CODEREP *cr) {
  HEAP_OBJ_REP *hor = Cr_2_heap_obj(cr);
  if (hor != NULL) {
    return hor;
  }
  else if (cr->Kind() == CK_CONST || cr->Kind() == CK_RCONST) {
    return Vsa()->Null_hor();
  }
  else if (cr->Kind() == CK_LDA) {
    hor = Create_heap_obj_for_lda(cr);
    Enter_cr_heap_obj_map(cr, hor, FALSE);
    if (hor->Stmt_def() == NULL)
      hor->Set_stmt_def(sr, Dna());
    return hor;
  }
  else {
    // find by HOR_FINDER
    HOR_FINDER finder(this, &_hor_cache);
    return finder.Find_cr_hor(cr);
  }
}

// ============================================================================
// HEAP_VSYM_ANALYSIS::Perform_with_delayed_ho_renaming
// main driver for HEAP_VSYM_ANALYSIS::Perform_with_delayed_ho_renaming
// ============================================================================
void
HEAP_VSYM_ANALYSIS::Perform_with_delayed_ho_renaming(INT max_iter) {
  // create phi_list for BBs with multiple preds
  for (BB_NODE *bb = Cfg()->First_bb(); bb; bb = bb->Next()) {
    if (bb->Pred() && bb->Pred()->Multiple_bbs()) {
      Vsa()->Enter_bb_ho_philist(bb, CXX_NEW(PHI_LIST(bb), Vsa()->Mem_pool()));
      Vsa()->Enter_bb_vo_philist(bb, CXX_NEW(PHI_LIST(bb), Vsa()->Mem_pool()));
      ID_PHI_MAP *vo_phi_cache = CXX_NEW(ID_PHI_MAP(PHI_TABLE_SIZE,
                                                    __gnu_cxx::hash<IDTYPE>(),
                                                    std::equal_to<IDTYPE>(),
                                                     mempool_allocator<ID_PHI_PAIR>(Hva_pool())),
                                                    Hva_pool());

      ID_PHI_MAP *ho_phi_cache = CXX_NEW(ID_PHI_MAP(PHI_TABLE_SIZE,
                                                    __gnu_cxx::hash<IDTYPE>(),
                                                    std::equal_to<IDTYPE>(),
                                                     mempool_allocator<ID_PHI_PAIR>(Hva_pool())),
                                                    Hva_pool());
      _vo_phi_cache[bb->Id()] = vo_phi_cache;
      _ho_phi_cache[bb->Id()] = ho_phi_cache;
    }
  }

  do {
    // begin iteration
    Begin_iteration();
    // process call and lda in first round
    if (Round() == 0)
      Process_call_and_lda_for_vsym();

    // create vsym_obj
    Perform_single_step<HVA_VO_CREATION>();
    // rename vsym_obj_rep
    Perform_single_step<HVA_VO_RENAMING>();

    // end iteration
    End_iteration();

    if (!Need_next_round())
      break;

  } while (Incr_round() <= max_iter);

  // call _hor_cache's Begin_iteration
  OPT_POOL_Push(&_local_pool, VSA_NHV_TRACE_FLAG);
  _hor_cache.Begin_iteration();

  // process call for heap_obj renaming
  Process_call_for_heap();
  // rename heap_obj_rep
  Perform_single_step<HVA_HO_RENAMING>();

  // poop local pool
  OPT_POOL_Pop(&_local_pool, VSA_NHV_TRACE_FLAG);

#ifdef Is_True_On
  Verify();
#endif

  VSA_STATS_inc_n(hva_round, Round());
  Is_True(Round() <= max_iter, ("infinite loop?"));
}

BOOL
HEAP_VSYM_ANALYSIS::Verify()
{
  // check phi cache
  BOOL ret = TRUE;
  for (BB_NODE *bb = Cfg()->First_bb(); bb; bb = bb->Next()) {
    if (bb->Pred() && bb->Pred()->Multiple_bbs()) {
      ID_PHI_MAP *vo_phi_cache = Vo_phi_cache()->at(bb->Id());
      PHI_LIST *vo_phi_list = Vsa()->Bb_vo_philist(bb);
      if (vo_phi_list) {
        if (!vo_phi_cache || vo_phi_list->Len() != vo_phi_cache->size()) {
          FmtAssert(FALSE, ("mismatched vo phi cache in bb%d, real%d, cached%ld",
                            bb->Id(), vo_phi_list->Len(), vo_phi_cache->size()));
          ret = FALSE;
        }
      }
      ID_PHI_MAP *ho_phi_cache = Ho_phi_cache()->at(bb->Id());
      PHI_LIST *ho_phi_list = Vsa()->Bb_ho_philist(bb);
      if (ho_phi_list) {
        if (!ho_phi_cache || ho_phi_list->Len() != ho_phi_cache->size()) {
          FmtAssert(FALSE, ("mismatched ho phi cache in bb%d: real%d, cache%ld\n",
                            bb->Id(), ho_phi_list->Len(), ho_phi_cache->size()));
          ret = FALSE;
        }
      }
    }
  }
  return ret;
}
