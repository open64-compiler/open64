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
// opt_vsa_cprop.cxx
//
// VSA inter-procedure constant & flag propagation
//
// ==================================================================

#include "defs.h"
#include "vsa_defs.h"
#include "opt_bb.h"
#include "bb_node_set.h"
#include "opt_main.h"
#include "opt_htable.h"
#include "opt_vsa.h"
#include "opt_cr_util.h"
#include "opt_vsa_scc.h"
#include "vsa_annot.h"
#include "opt_vsa_hprop.h"
#include "opt_vsa_rbc.h"
#include "intrn_info.h"

// TODO:
// 1. constant propagation after value range analysis:
//    if (x == 5) {
//      ... <- x;        // replace x with 5
// 2. integration with assume:
//    assume_eq(x, 5);
//    ... <- x;          // replace x with 5? isthis really doable??
// 3. inter-procedure constant propagation:
//    void foo(int x);
//    void bar() {
//      ...
//      foo(5);          // in foo(), replace x with 5

// ==================================================================
// implementing of SCC for vor flag propagation
// with _N = NODEREP* and _G = COMP_UNIT *
// ==================================================================

// put TSCC in an anonymous namespace
namespace {

// TSCC<CODEREP *, COMP_UNIT *>::Print
// dump the scc
template<> void
TSCC<CODEREP *, COMP_UNIT *>::Print(FILE *fp, COMP_UNIT *cu) const
{
  fprintf(fp, "%sTSCC with %d entries:%s", SBar, Id(), (Id()==0)?"\n":" ");
  if (Id() != 0)
    Node(0)->Print(0, fp);

  for (INT i = 0; i < Id(); ++i) {
    fprintf(fp, "#%d:\t Ids: cr%d\tLow: %d\tFlags:",
            i, Node(i)->Coderep_id(), Low(i));
    VANT_UTIL::Dump(fp, Node(i)->Vsa_annot());
    fprintf(fp, "\n");
  }
} // TSCC::Print

// TSCC<CODEREP *, COMP_UNIT *>::Update
// update coderep in the scc
template<> void
TSCC<CODEREP *, COMP_UNIT *>::Update(COMP_UNIT *cu) const
{
  if (Id() == 0)
    return;  // there is nothing to be done

  Is_Trace(Tracing(),
           (TFile, " -- before unification on var for:\n"));
  Is_Trace_cmd(Tracing(), Print(TFile, cu));

  for (INT i=0; i < Id(); ++i) {
    if (Low(i) == i) {  // low_link candidate
      // unify the attribute by merging into the low_link
      CODEREP *low_link = Node(i);
      CODEREP *peer;

      V_ANNOT v = low_link->Vsa_annot();
      for (INT j = i+1; (j < Id()) && (Low(j) == i); ++j) {
        peer = Node(j);
        v = VANT_UTIL::Join(v, peer->Vsa_annot());
      } // unify loop
      low_link->Set_vsa_annot(v);

      // populate same attribute set from low_link to the rest of scc peers
      for (INT j = i+1; (j < Id()) && (Low(j) == i); ++j) {
        peer = Node(j);
        peer->Set_vsa_annot(v);
      } // populate loop

    } // low_link candidate
  }
  Is_Trace(Tracing(),
           (TFile, " -- after unification on var for:\n"));
  Is_Trace_cmd(Tracing(), Print(TFile, cu));
} // TSCC::Update

// ==================================================================
// implementing of SCC for vor flag propagation
// with _N = VSYM_OBJ_REP* and _G = VSA *
// ==================================================================

// TSCC<VSYM_OBJ_REP *, VSA *>::Print
// dump the scc
template<> void
TSCC<VSYM_OBJ_REP *, VSA *>::Print(FILE *fp, VSA *vsa) const
{
  fprintf(fp, "%sTSCC with %d entries:%s", SBar, Id(), (Id()==0)?"\n":" ");
  if (Id() != 0) {
    Node(0)->Print(fp);
    fprintf(fp, "\n");
  }

  for (INT i=0; i < Id(); ++i) {
    fprintf(fp, "#%d:\t Ids: vor%dv%d\tLow: %d\tFlags:",
            i, Node(i)->Vsym_obj()->Id(), Node(i)->Version(), Low(i));
    VANT_UTIL::Dump(fp, Node(i)->Vsa_annot());
    fprintf(fp, "\n");
  }
} // TSCC::Print

// TSCC<VSYM_OBJ_REP *, VSA*>::Update
// update all VSYM_OBJ_REP in the scc
template<> void
TSCC<VSYM_OBJ_REP *, VSA*>::Update(VSA *vsa) const
{
  if (Id() == 0)
    return;  // there is nothing to be done

  Is_Trace(Tracing(),
           (TFile, " -- before unification on vor for:\n"));
  Is_Trace_cmd(Tracing(), Print(TFile, vsa));

  for (INT i=0; i < Id(); ++i) {
    if (Low(i) == i) {  // low_link candidate
      // unify the attribute by merging into the low_link
      VSYM_OBJ_REP *low_link = Node(i);
      VSYM_OBJ_REP *peer;

      V_ANNOT v = low_link->Vsa_annot();
      for (INT j = i+1; (j < Id()) && (Low(j) == i); ++j) {
        peer = Node(j);
        v = VANT_UTIL::Join(v, peer->Vsa_annot());
      } // unify loop
      low_link->Set_vsa_annot(v);

      // populate same attribute set from low_link to the rest of scc peers
      for (INT j = i+1; (j < Id()) && (Low(j) == i); ++j) {
        peer = Node(j);
        peer->Set_vsa_annot(v);
      } // populate loop

    } // low_link candidate
  }
  Is_Trace(Tracing(),
           (TFile, " -- after unification on vor for:\n"));
  Is_Trace_cmd(Tracing(), Print(TFile, vsa));
} // TSCC::Update

} // end of anonymous namespace

// ==================================================================
// helpere function to get and set annotation
// ==================================================================

// Set_annot
// set annotation to var (CK_VAR) or vor (CK_IVAR)
static void
Set_annot(VSA *vsa, CODEREP *cr, V_ANNOT v) {
  VSYM_OBJ_REP *vor;
  if (cr->Kind() == CK_VAR) {
    cr->Set_vsa_annot(v);
  }
  else if (vsa && cr->Kind() == CK_IVAR &&
           (vor = vsa->Cr_2_vor(cr)) != NULL) {
    vor->Set_vsa_annot(v);
  }
} // Set_annot

// Set_annot
// set annotation to each var or vor in the map
template<typename MAP> static void
Set_annot(const MAP& map, ANT_KIND kind) {
  typename MAP::const_iterator end = map.end();
  for (typename MAP::const_iterator it = map.begin();
       it != end; ++it) {
    V_ANNOT annot = it->second->Vsa_annot();
    annot = VANT_UTIL::Set(annot, kind, ANT_YES);
    it->second->Set_vsa_annot(annot);
  }
} // Set_annot

// Merge_parm_out
// Join existing annotation with new annotation on vor or vor
static void
Merge_parmout_rev(VSA *vsa, CODEREP *cr, V_ANNOT v) {
  VSYM_OBJ_REP *vor;
  if (cr->Kind() == CK_VAR) {
    cr->Set_vsa_annot(VANT_UTIL::Merge_parmout_rev(cr->Vsa_annot(), v));
  }
  else if (vsa && cr->Kind() == CK_IVAR &&
           (vor = vsa->Cr_2_vor(cr)) != NULL) {
    vor->Set_vsa_annot(VANT_UTIL::Merge_parmout_rev(vor->Vsa_annot(), v));
  }
} // Merge_parmout_rev

// Get_annot
// return annotation on var (CK_VAR) or vor(CK_IVAR)
static V_ANNOT
Get_annot(VSA *vsa, CODEREP *cr) {
  VSYM_OBJ_REP *vor;
  if (cr->Kind() == CK_VAR) {
    return cr->Vsa_annot();
  }
  else if (vsa && cr->Kind() == CK_IVAR &&
           (vor = vsa->Cr_2_vor(cr)) != NULL) {
    return vor->Vsa_annot();
  }
  return VANT_UTIL::Empty();
} // Get_annot

// ==================================================================
// LOCAL_CPROP
//
// forward propagate constant and flags in single function
// - forward traverse the DOM tree and BB statments
// - propagate flags on caller's argument to this callee's formal at
//   beginning
// - propagate flags on callee's return value to this callstmt's
//   return value during visiting callstmt
// - propagate flags on callee's last assignment on output parameter
//   to this callstmt's output parameter during visiting callstmt
// ==================================================================
class LOCAL_CPROP {
  typedef TSCC<CODEREP *, COMP_UNIT *> VAR_SCC;
  typedef TSCC<VSYM_OBJ_REP *, VSA *> VOR_SCC;

private:
  IPSA        *_ipsa;        // IPSA to propagate flags for calls
  COMP_UNIT   *_comp_unit;   // CU to be propagated
  BOOL         _hva;         // prop over ho/vo?
  BOOL         _trace;       // is trace on
  CXX_MEM_POOL _loc_pool;    // temporary local pool

private:
  BOOL      Tracing() const { return _trace; }
  VSA      *Vsa() const     { return _comp_unit->Vsa(); }
  MEM_POOL *Loc_pool()      { return _loc_pool(); }

private:
  // handle intrinsic op
  V_ANNOT Process_intrn_op(CODEREP *var, V_ANNOT v, STMTREP *stmt);
  // handle intrinsic call
  V_ANNOT Process_intrn_call(INTRINSIC intrn, STMTREP *stmt);
  // handle call
  V_ANNOT Process_call(STMTREP *stmt);
  // handle indirect call
  V_ANNOT Process_icall(STMTREP *stmt);
  // handle call with ipsa rna
  V_ANNOT Process_rna(RNA_NODE *rna);

  // propagate flags for var within the SCC
  V_ANNOT Propagate_vardef(CODEREP *var, VAR_SCC *scc, hash_set<UINT32> &visited);
  // propagate flags for vor within the SCC
  V_ANNOT Propagate_vordef(VSYM_OBJ_REP *vor, VOR_SCC *scc, hash_set<UINT32> &visited);
  // propagate flags for coderep used in stmt
  V_ANNOT Propagate_expr(CODEREP *expr, V_ANNOT v, STMTREP *stmt);
  // propagate flags for stmt
  void    Propagate_stmt(STMTREP *stmt, BB_NODE *curbb);
  // propagate flags for bb
  void    Propagate_bb(BB_NODE *bb);
  // propagate flags from clby's arguments to formals
  void    Propagate_entry();
  // propagate flags from rbc dna
  void    Propagate_rbc_dna_flags();
  // propagate flags to stmt vor chi
  void    Propagate_vor_chi(VSA *vsa, STMTREP *stmt, CODEREP *cr, V_ANNOT v);
  // propagate value objects for call
  void    Propagate_values_for_call(STMTREP *stmt);

public:
  // constructor
  LOCAL_CPROP(IPSA *ipsa, COMP_UNIT *cu, BOOL hva)
    : _ipsa(ipsa), _comp_unit(cu), _hva(hva),
    _loc_pool("LOCAL_CPROP local pool", FALSE) {
    _trace = Get_Trace(TP_VSA, VSA_PROP_TRACE_FLAG);
  }

  // do propagation
  void Do_prop() {
    Is_Trace(Tracing(),
             (TFile, "============================================================\n"));
    Is_Trace(Tracing(),
             (TFile, "Constant prop tracing for %s %s\n", _comp_unit->Dna()->Fname(),
                     _hva ? "with ho/vo" : ""));

    // for heap_obj annotation propagation
    HOA_PROP heap_prop(_ipsa, _comp_unit);

    // propagate rbc
    Propagate_rbc_dna_flags();

    // propagate clby's flags to formal
    Propagate_entry();

    if (VSA_HOA_Prop && _hva) {
      // propagate clby's heap_obj annotation to formal in
      heap_prop.Propagate_ho_annot_on_entry();
    }

    // traverse the DOM tree recursively
    // TODO: change to non-recursive traversal
    Propagate_bb(_comp_unit->Cfg()->Entry_bb());

    if (VSA_HOA_Prop && _hva) {
      // propagate return value/output param's heap_obj annotation
      heap_prop.Calculate_ho_annot_on_return();

      // dump flag after propagation
      Is_Trace(Get_Trace(TP_VSA, VSA_HPROP_TRACE_FLAG),
               (TFile, "============================================================\n"));
      Is_Trace(Get_Trace(TP_VSA, VSA_HPROP_TRACE_FLAG),
               (TFile, "DUMP after forward hoa prop for %s\n", _comp_unit->Dna()->Fname()));
      Is_Trace_cmd(Get_Trace(TP_VSA, VSA_HPROP_TRACE_FLAG),
                   HOA_PROP::Print_dna(_comp_unit->Dna(), TFile));
      Is_Trace(Get_Trace(TP_VSA, VSA_HPROP_TRACE_FLAG),
               (TFile, "============================================================\n"));
    }

    // dump flag after propagation
    Is_Trace(Get_Trace(TP_VSA, VSA_PROP_DUMP_FLAG),
             (TFile, "============================================================\n"));
    Is_Trace(Get_Trace(TP_VSA, VSA_PROP_DUMP_FLAG),
             (TFile, "DUMP after constant prop for %s %s\n", _comp_unit->Dna()->Fname(),
                     _hva ? "with ho/vo" : ""));
    Is_Trace_cmd(Get_Trace(TP_VSA, VSA_PROP_DUMP_FLAG), Print(TFile));
    Is_Trace(Get_Trace(TP_VSA, VSA_PROP_DUMP_FLAG),
             (TFile, "============================================================\n"));
  }   // Do_prop

  // print the annotation
  void Print(FILE *fp) const;

};  // LOCAL_CPROP

// LOCAL_CPROP::Process_intrn_op
// process intrinsic op to figure out its flags
V_ANNOT
LOCAL_CPROP::Process_intrn_op(CODEREP *op, V_ANNOT v, STMTREP *stmt)
{
  Is_True(op->Kind() == CK_OP && op->Opr() == OPR_INTRINSIC_OP,
          ("not intrinsic op"));
  // set flags according to intrinsic
  // TODO: which intrinsic op?

  // handle intrinsic parameters
  for (INT i = 0; i < op->Kid_count(); ++i) {
    CODEREP *parm = op->Opnd(i);
    Is_True(parm && parm->Kind() == CK_IVAR &&
            parm->Opr() == OPR_PARM, ("bad parm"));
    Propagate_expr(parm->Ilod_base(), VANT_UTIL::Empty(), stmt);
  }

  return v;
} // LOCAL_CPROP::Process_intrn_op

// LOCAL_CPROP::Process_intrn_call
// process intrinsic call to figure out its flags
V_ANNOT
LOCAL_CPROP::Process_intrn_call(INTRINSIC intrn, STMTREP *stmt)
{
  Is_True(intrn != INTRINSIC_NONE &&
          (stmt->Opr() == OPR_CALL ||
           stmt->Opr() == OPR_INTRINSIC_CALL),
          ("not intrinsic call"));
  // set flags according to intrinsic
  // TODO: other intrinsics?
  if (intrn == INTRN_MEMCPY ||
      intrn == INTRN_STRCPY ||
      intrn == INTRN_STRNCPY) {
    Is_True(stmt->Rhs()->Kid_count() >= 2,
            ("bad rhs"));
    CODEREP *src = stmt->Rhs()->Opnd(1);
    CODEREP *rbase = _comp_unit->Analyze_base_info(stmt, src, FALSE);
    V_ANNOT src_v = VANT_UTIL::Empty();
    if (rbase) {
      if (rbase->Kind() == CK_LDA) {
        // set vor annot
        src_v = VANT_UTIL::Init(ANT_VCONST, ANT_YES);
      }
      else {
        // only keep the vor annot
        src_v = VANT_UTIL::Vor_annot(Get_annot(Vsa(), rbase));
      }
      CODEREP *dst = stmt->Rhs()->Opnd(0);
      CODEREP *lbase = _comp_unit->Analyze_base_info(stmt, dst, FALSE);
      if (lbase) {
        Set_annot(Vsa(), lbase, src_v);
      }
    }
  }
  else if (intrn == INTRN_MEMSET) {
    Is_True(stmt->Rhs()->Kid_count() == 3,
            ("bad rhs"));
    CODEREP *val = stmt->Rhs()->Opnd(1);
    V_ANNOT val_v = VANT_UTIL::Empty();
    if (val->Kind() == CK_CONST) {
      // set const and 0 flag
      val_v = VANT_UTIL::Init(ANT_CONST, ANT_YES);
      if (val->Const_val() == 0)
        val_v = VANT_UTIL::Set(val_v, ANT_ZERO, ANT_YES);
    }
    else if (val->Kind() == CK_VAR) {
      // TODO: should other flags copied?
      V_ANNOT tmp_v = val->Vsa_annot();
      val_v = VANT_UTIL::Copy2(val_v, tmp_v, ANT_CONST, ANT_ZERO);
    }

    CODEREP *dst = stmt->Rhs()->Opnd(0);
    CODEREP *base = _comp_unit->Analyze_base_info(stmt, dst, FALSE);
    if (base) {
      // convert var flag to vor flag
      val_v = VANT_UTIL::Var_2_vor(val_v);
      Set_annot(Vsa(), base, val_v);
    }
  }

  // handle intrinsic call parameters
  CODEREP *rhs = stmt->Rhs();
  Is_True(rhs && rhs->Kind() == CK_OP &&
          (rhs->Opr() == OPR_CALL || rhs->Opr() == OPR_INTRINSIC_CALL),
          ("bad rhs"));
  for (INT i = 0; i < rhs->Kid_count(); ++i) {
    CODEREP *parm = rhs->Opnd(i);
    Is_True(parm && parm->Kind() == CK_IVAR &&
            parm->Opr() == OPR_PARM, ("bad parm"));
    Propagate_expr(parm->Ilod_base(), VANT_UTIL::Empty(), stmt);
  }

  return VANT_UTIL::Empty();
} // LOCAL_CPROP::Process_intrn_call

// LOCAL_CPROP::Process_call
// process call to propagate flags by call name or IPSA side-effect
V_ANNOT
LOCAL_CPROP::Process_call(STMTREP *stmt)
{
  Is_True(stmt->Opr() == OPR_CALL && stmt->St() != NULL,
          ("bad call"));
  BOOL is_alloc = stmt->Callee_returns_new_heap_memory();
  BOOL is_free = stmt->Callee_frees_heap_memory();
  BOOL is_realloc = is_alloc && is_free;
  BOOL is_init = !Is_alloc_not_initialized(stmt);
  if (is_alloc) {
    CODEREP *ret = _comp_unit->Find_return_value(stmt);
    if (ret != NULL &&
        (ret = _comp_unit->Analyze_base_info(stmt, ret, FALSE)) != NULL) {
      V_ANNOT v = VANT_UTIL::Init(ANT_MALLOC, ANT_YES);
      V_ANNOT vor_v = VANT_UTIL::Init(ANT_MALLOC, ANT_YES);
      v = VANT_UTIL::Set(v, ANT_RETVAL, ANT_YES);
      v = VANT_UTIL::Set(v, ANT_WRITE, ANT_YES);
      if (is_realloc) {
        v = VANT_UTIL::Set(v, ANT_REALLOC, ANT_YES);
        v = VANT_UTIL::Set(v, ANT_VWRITE, ANT_MAYBE);
        vor_v = VANT_UTIL::Set(vor_v, ANT_REALLOC, ANT_YES);
        vor_v = VANT_UTIL::Set(vor_v, ANT_WRITE, ANT_MAYBE);
      } else if (is_init) {
        v = VANT_UTIL::Set(v, ANT_VZERO, ANT_YES);
        v = VANT_UTIL::Set(v, ANT_VWRITE, ANT_YES);
        vor_v = VANT_UTIL::Set(vor_v, ANT_WRITE, ANT_YES);
        vor_v = VANT_UTIL::Set(vor_v, ANT_ZERO, ANT_YES);
      } else {
        v = VANT_UTIL::Set(v, ANT_VWRITE, ANT_NO);
        vor_v = VANT_UTIL::Set(vor_v, ANT_WRITE, ANT_NO);
      }
      Set_annot(Vsa(), ret, v);
      Propagate_vor_chi(Vsa(), stmt, ret, vor_v);
    }
  }
  if (is_free) {
    Is_True(stmt->Rhs()->Kid_count() >= 1 &&
            stmt->Rhs()->Opnd(0)->Kind() == CK_IVAR &&
            stmt->Rhs()->Opnd(0)->Opr() == OPR_PARM,
           ("bad free call"));
    CODEREP *parm = stmt->Rhs()->Opnd(0)->Ilod_base();
    Is_True(parm != NULL, ("bad parm"));
    parm = _comp_unit->Analyze_base_info(stmt, parm, FALSE);
    if (parm != NULL) {
      V_ANNOT v = VANT_UTIL::Init(ANT_FREE, ANT_YES);
      V_ANNOT vor_v = v;
      v = VANT_UTIL::Set(v, ANT_ACTUAL, ANT_YES);
      v = VANT_UTIL::Set(v, ANT_VREAD, ANT_YES);
      v = VANT_UTIL::Set(v, ANT_VWRITE, ANT_YES);
      vor_v = VANT_UTIL::Set(v, ANT_READ, ANT_YES);
      vor_v = VANT_UTIL::Set(v, ANT_WRITE, ANT_YES);
      if (is_realloc)
        v = VANT_UTIL::Set(v, ANT_REALLOC, ANT_YES);
      Set_annot(_comp_unit->Vsa(), parm, v);
      Propagate_vor_chi(Vsa(), stmt, parm, vor_v);
      // proagate VWRITE flag to base
      if (parm->Kind() == CK_IVAR) {
        CODEREP *base_cr = Find_ilod_base(parm->Ilod_base());
        while (base_cr != NULL &&
               (base_cr->Kind() == CK_VAR || base_cr->Kind() == CK_IVAR)) {
          v = VANT_UTIL::Set(Get_annot(Vsa(), base_cr), ANT_VWRITE, ANT_YES);
          v = VANT_UTIL::Set(v, ANT_VFREE, ANT_YES);
          Set_annot(Vsa(), base_cr, v);
          base_cr = (base_cr->Kind() == CK_IVAR)
                      ? Find_ilod_base(base_cr->Ilod_base()) : NULL;
        }
      }
      RNA_NODE *rna = Vsa()->Sr_2_rna(stmt);
      Is_True(!rna || rna->Arg_cnt() >= 1, ("free rna arg < 1"));
      if (rna && rna->Arg_cnt() >= 1) {
        rna->Set_arg_flag(1, REF_ISTORE);
      }
    }
  }

  // handle call parameters
  CODEREP *rhs = stmt->Rhs();
  Is_True(rhs && rhs->Kind() == CK_OP && rhs->Opr() == OPR_CALL,
          ("bad rhs"));
  for (INT i = 0; i < rhs->Kid_count(); ++i) {
    CODEREP *parm = rhs->Opnd(i);
    Is_True(parm && parm->Kind() == CK_IVAR &&
            parm->Opr() == OPR_PARM, ("bad parm"));
    V_ANNOT parm_v = VANT_UTIL::Init(ANT_ACTUAL, ANT_YES);
    Propagate_expr(parm->Ilod_base(), parm_v, stmt);
  }

  // propagate from callee
  RNA_NODE *rna = Vsa()->Sr_2_rna(stmt);
  if (rna == NULL)
    return VANT_UTIL::Empty();

  return Process_rna(rna);
} // LOCAL_CPROP::Process_call

// LOCAL_CPROP::Process_icall
// process indirect call to propagate flags by IPSA side-effect
V_ANNOT
LOCAL_CPROP::Process_icall(STMTREP *stmt)
{
  // handle icall parameters
  CODEREP *rhs = stmt->Rhs();
  Is_True(rhs && rhs->Kind() == CK_OP && rhs->Opr() == OPR_ICALL &&
          rhs->Kid_count() > 0,
          ("bad rhs"));
  if (rhs->Kid_count() > 0) {
    INT i;
    for (i = 0; i < rhs->Kid_count() - 1; ++i) {
      CODEREP *parm = rhs->Opnd(i);
      Is_True(parm && parm->Kind() == CK_IVAR &&
              parm->Opr() == OPR_PARM, ("bad parm"));
      V_ANNOT parm_v = VANT_UTIL::Init(ANT_ACTUAL, ANT_YES);
      Propagate_expr(parm->Ilod_base(), parm_v, stmt);
    }
    V_ANNOT tgt_v = VANT_UTIL::Init(ANT_READ, ANT_YES);
    tgt_v = VANT_UTIL::Set(tgt_v, ANT_VREAD, ANT_YES);
    CODEREP *tgt_base = Find_ilod_base(rhs->Opnd(i));
    if (tgt_base)
      Propagate_expr(tgt_base, tgt_v, stmt);
  }

  // propagate from callee
  RNA_NODE *rna = Vsa()->Sr_2_rna(stmt);
  if (rna == NULL)
    return VANT_UTIL::Empty();

  return Process_rna(rna);
} // LOCAL_CPROP::Process_icall

// LOCAL_CPROP::Process_rna
// process rna to propagate flags by IPSA side-effect
V_ANNOT
LOCAL_CPROP::Process_rna(RNA_NODE *rna)
{
  Is_True(rna != NULL, ("bad rna"));

  if (_ipsa == NULL)
    return VANT_UTIL::Empty();

  // propagate DNA's CALLED_IN_LOOP flag to RNA
  BOOL in_loop = rna->Is_flag_set(RNA_CALLED_IN_LOOP);
  if (in_loop == FALSE) {
    in_loop = _comp_unit->Dna()->Is_set(DNA_CALLED_IN_LOOP);
    if (in_loop)
      rna->Set_flag(RNA_CALLED_IN_LOOP);
  }

  // get return value cr and flag
  STMTREP *sr = rna->Callstmt();
  CODEREP *retval = _comp_unit->Find_return_value(sr);
  Is_True(retval == NULL || retval->Kind() == CK_VAR ||
          retval->Kind() == CK_IVAR, ("retval not var"));
  V_ANNOT retv_v = retval ? Get_annot(Vsa(), retval)
                          : VANT_UTIL::Empty();

  // only propagate to callee for direct call or after icall resolved
  BOOL prop_to_callee = (sr->Opr() != OPR_ICALL) || _hva;
  UINT32 thrd_flag = _comp_unit->Dna()->Thread_flags();

  // process all callees
  for(CALLEE_VECTOR::const_iterator it = rna->Callee_list().begin();
      it != rna->Callee_list().end(); ++it) {
    DNA_NODE* callee = _ipsa->Get_dna(it->Callee());
    Is_True(callee != NULL, ("bad callee"));
    if (callee->Non_functional())
      continue;
    COMP_UNIT *callee_cu = callee->Comp_unit();

    if (prop_to_callee) {
      // propagate RNA's CALLED_IN_LOOP flag to callee
      if (in_loop && !callee->Is_set(DNA_CALLED_IN_LOOP))
        callee->Set_flag(DNA_CALLED_IN_LOOP);
      // propagate DNA's THREAD flags to callee
      if (thrd_flag != 0) {
        callee->Set_flag(thrd_flag);
        Is_Trace(Get_Trace(TP_VSA, VSA_REG_TRACE_FLAG),
                 (TFile, "REGISTER: prop flag from %s:%c to %s:%c at line %d:%s%s%s%s.\n",
                         _comp_unit->Dna()->Fname(),
                         _comp_unit->Dna()->Exec_in_parallel() ? 'Y' : 'N',
                         callee->Fname(),
                         callee->Exec_in_parallel() ? 'Y' : 'N',
                         Srcpos_To_Line(rna->Linenum()),
                         (thrd_flag & DNA_IN_MAIN_THREAD) ? " MAIN" : "",
                         (thrd_flag & DNA_IN_HELP_THREAD) ? " HELP" : "",
                         (thrd_flag & DNA_IN_WORK_THREAD) ? " WORK" : "",
                         (thrd_flag & DNA_IN_ISR) ? " ISR" : ""));
      }
    }

    // handle retv for return value and output parameter (argument)
    for (INT i = VAR_INIT_ID; i < callee->Retv_list()->size(); ++i) {
      PDV_NODE* pdv = (*callee->Retv_list())[i];
      STMTREP *stmt = pdv->Stmt();
      if (pdv->Kind() == BY_RETURNSTMT && pdv->Oparam() == 0) {
        Is_True(stmt != NULL && stmt->Opr() == OPR_STID, ("bad stmt"));
        Is_True(stmt->Next() != NULL && stmt->Next()->Opr() == OPR_RETURN,
                ("bad return stmt"));
        if (retval) {
          V_ANNOT rhs_v = Get_cr_annot(callee_cu->Vsa(), stmt->Rhs());
          retv_v = VANT_UTIL::Merge_retv(retv_v, rhs_v);
        }
      }
      else if (pdv->Oparam() > 0 && pdv->Oparam() <= rna->Arg_cnt()) {
        Is_True(pdv->Oparam() >= VAR_INIT_ID &&
                pdv->Oparam() <= callee->Parm_list()->size(),
                ("bad pdv parm index"));

        // how to handle this? in another call
        if (OPERATOR_is_call(stmt->Opr()))
          continue;
        CODEREP* arg = rna->Get_arg(pdv->Oparam());
        Is_True(arg != NULL,
                ("can not get actual for %d", pdv->Oparam()));
        if (arg != NULL &&
            (arg = _comp_unit->Analyze_base_info(rna->Callstmt(),
                                                 arg, FALSE)) != NULL) {
          Is_True(stmt->Opr() == OPR_ISTORE || stmt->Opr() == OPR_MSTORE,
                  ("bad pdv stmt opr"));
          Is_True(arg->Kind() == CK_LDA || arg->Kind() == CK_VAR ||
                  arg->Kind() == CK_IVAR, ("bad arg"));
          VSYM_OBJ_REP *vor;
          V_ANNOT parm_v = VANT_UTIL::Empty();
          if (callee_cu->Vsa() &&
              (vor = callee_cu->Vsa()->Cr_2_vor(stmt->Lhs())) != NULL)
            parm_v = vor->Vsa_annot();
          if (parm_v == VANT_UTIL::Empty())
            parm_v = Get_cr_annot(callee_cu->Vsa(), stmt->Rhs());
          if (parm_v == VANT_UTIL::Empty())
            continue;

          if (arg->Kind() == CK_LDA) {
            // copy parm_v to chi result
            // callsite: foo(&a);
            // callee:   foo(int *p) { *p = blah; }
            // ==>
            // a = blah
            VSYM_FLD_REP vfr(FLD_K_ID, stmt->Lhs()->I_field_id(), stmt->Lhs()->Offset());
            CHI_NODE *chi = Vsa()->Find_stmt_var_chi(rna->Callstmt(),
                                                     arg->Lda_base_st(),
                                                     &vfr);
            if (chi != NULL) {
              Merge_parmout_rev(Vsa(), chi->RESULT(), parm_v);
            }
          }
          else {
            // copy parm_v's var to p's vor
            // callsite: foo(a);
            // callee: foo(int *p) { *p = blah; }
            // ==>
            // *p = blah
            Merge_parmout_rev(Vsa(), arg, VANT_UTIL::Var_2_vor(parm_v));
          }
        }
      }
    }
  }

  // annotate to call's return value
  if (retval)
    Set_annot(Vsa(), retval, retv_v);

  // propagate heap_obj size on rna
  if (VSA_HOA_Prop && _hva) {
    HOA_PROP heap_prop(_ipsa, _comp_unit);
    heap_prop.Calculate_ho_annot_on_rna(rna);
    heap_prop.Propagate_ho_annot_from_rna(rna);
  }

  return retv_v;
} // LOCAL_CPROP::Process_rna

// LOCAL_CPROP::Propagate_vardef
// propagate flags for var within the SCC
V_ANNOT
LOCAL_CPROP::Propagate_vardef(CODEREP *var, VAR_SCC *scc, hash_set<UINT32> &visited)
{
  Is_True(var && var->Kind() == CK_VAR, ("not var cr"));
  // check if var is constant initialized
  if (Vsa()->Udt_const_init_scalar(var, var->Aux_id())) {
    V_ANNOT v = VANT_UTIL::Init(ANT_WRITE, ANT_YES);
    v = VANT_UTIL::Set(v, ANT_CONST, ANT_YES);
    var->Set_vsa_annot(v);
    return v;
  }

  if (var->Is_flag_set(CF_DEF_BY_CHI)) {
    ST *st = _comp_unit->Opt_stab()->Aux_stab_entry(var->Aux_id())->St();
    V_ANNOT v = var->Vsa_annot();
    if (var->Def_at_entry() && st != NULL) {
      switch (ST_class(st)) {
      case SCLASS_AUTO:
        v = VANT_UTIL::Set(v, ANT_WRITE, ANT_NO);
        break;
      case SCLASS_FORMAL:
      case SCLASS_FORMAL_REF:
        v = VANT_UTIL::Set(v, ANT_WRITE, ANT_MAYBE);
        break;
      case SCLASS_PSTATIC:
      case SCLASS_FSTATIC:
      case SCLASS_DGLOBAL:
      case SCLASS_UGLOBAL:
      case SCLASS_COMMON:
        v = VANT_UTIL::Set(v, ANT_WRITE, ANT_YES);
        // TODO: check INITO for ZERO flag
        v = VANT_UTIL::Set(v, ANT_ZERO, ANT_MAYBE);
        break;
      default:
        Is_True(FALSE, ("wrong sclass %s", Sclass_Name(ST_class(st))));
        break;
      }
    }

    STMTREP *defstmt = var->Defstmt();
    if (OPERATOR_is_call(defstmt->Opr())) {
      // check IPSA
    }
    else {
      // alias write
    }
  }

  if (var->Is_flag_set(CF_DEF_BY_PHI)) {
    PHI_NODE *phi = var->Defphi();
    Is_True(phi && phi->RESULT() == var, ("bad phi def"));
    V_ANNOT res_v = var->Vsa_annot();
    if (visited.find(phi->Bb()->Id()) != visited.end())
      return res_v;
    visited.insert(phi->Bb()->Id());

    CODEREP *opnd;
    PHI_OPND_ITER iter(phi);
    FOR_ALL_ELEM(opnd, iter, Init()) {
      V_ANNOT opnd_v = Propagate_vardef(opnd, scc, visited);
      if (!scc->Defer(opnd)) {
        res_v = VANT_UTIL::Join(res_v, opnd_v);
      }
    }
    var->Set_vsa_annot(res_v);
    scc->Update(_comp_unit);
    return var->Vsa_annot();
  }

  // check def stmt?
  return var->Vsa_annot();
} // LOCAL_CPROP::Propagate_vardef

// LOCAL_CPROP::Propagate_vordef
// propagate flags for vor within the SCC
V_ANNOT
LOCAL_CPROP::Propagate_vordef(VSYM_OBJ_REP *vor, VOR_SCC *scc, hash_set<UINT32> &visited)
{
  Is_True(vor, ("bad vor"));
  if (vor->Attr() == ROR_DEF_BY_CHI) {
    STMTREP *def = vor->Stmt_def();
    if (def && def->Opr() == OPR_OPT_CHI) {
      // TODO: vor defined by entry chii
    }
    else if (def && OPERATOR_is_call(def->Opr())) {
      // TODO: ipsa
    }
    else if (def) {
      // chi def
    }
    return vor->Vsa_annot();
  }

  if (vor->Attr() == ROR_DEF_BY_PHI) {
    PHI_NODE *phi = vor->Phi_def();
    Is_True(phi && phi->RESULT() == (CODEREP*)vor, ("bad vor"));
    V_ANNOT res_v = vor->Vsa_annot();
    if (visited.find(phi->Bb()->Id()) != visited.end())
      return res_v;
    visited.insert(phi->Bb()->Id());

    for (INT i = 0; i < phi->Size(); ++i) {
      VSYM_OBJ_REP *opnd = (VSYM_OBJ_REP*)phi->OPND(i);
      V_ANNOT opnd_v = Propagate_vordef(opnd, scc, visited);
      if (!scc->Defer(opnd)) {
        res_v = VANT_UTIL::Join(res_v, opnd_v);
      }
    }
    vor->Set_vsa_annot(res_v);
    scc->Update(Vsa());
    return vor->Vsa_annot();
  }

  // check ISTORE def?
  return vor->Vsa_annot();
} // LOCAL_CPROP::Propagate_vordef

// LOCAL_CPROP::Propagate_expr
// propagate flags for coderep used in stmt
V_ANNOT
LOCAL_CPROP::Propagate_expr(CODEREP *expr, V_ANNOT v, STMTREP *stmt)
{
  CODEREP *base;
  VSYM_OBJ_REP *vor;
  V_ANNOT lhs_v = VANT_UTIL::Empty();
  V_ANNOT rhs_v = VANT_UTIL::Empty();
  switch (expr->Kind()) {
  case CK_LDA:
    return VANT_UTIL::Set(v, ANT_CONST, ANT_YES);

  case CK_CONST:
    if (expr->Const_val() == 0)
      v = VANT_UTIL::Set(v, ANT_ZERO, ANT_YES);
    return VANT_UTIL::Set(v, ANT_CONST, ANT_YES);

  case CK_RCONST:
    if (expr->Const_fval() == 0.)
      v = VANT_UTIL::Set(v, ANT_ZERO, ANT_YES);
    return VANT_UTIL::Set(v, ANT_CONST, ANT_YES);

  case CK_VAR:
    v = VANT_UTIL::Or(expr->Vsa_annot(), v);
    v = VANT_UTIL::Set(v, ANT_READ, ANT_YES);
    expr->Set_vsa_annot(v);
    return v;
  case CK_IVAR:
    Is_True(expr->Opr() != OPR_PARM, ("no parm here"));
    Is_True(expr->Ilod_base() != NULL, ("no istore here"));
    // annotate vor READ flag
    v = VANT_UTIL::Set(v, ANT_READ, ANT_YES);
    if ((vor = Vsa()->Cr_2_vor(expr)) != NULL) {
      v = VANT_UTIL::Or(vor->Vsa_annot(), v);
      vor->Set_vsa_annot(v);
    }
    // annotate base VREAD flag
    base = _comp_unit->Analyze_base_info(stmt,
                                         expr->Ilod_base(),
                                         FALSE);
    if (base) {
      V_ANNOT base_v = VANT_UTIL::Var_2_vor(v);
      base_v = VANT_UTIL::Set(base_v, ANT_VREAD, ANT_YES);
      //Set_annot(Vsa(), base, base_v);
      // propagate ilod base flag
      CODEREP *base_cr = Find_ilod_base(expr->Ilod_base());
      if (base_cr)
        Propagate_expr(base_cr, base_v, stmt);
    }
    // propagate mload size
    if (expr->Opr() == OPR_MLOAD) {
      Propagate_expr(expr->Mload_size(), VANT_UTIL::Empty(), stmt);
    }
    return v;
    
  case CK_OP:
    switch (expr->Opr()) {
    case OPR_CVT:
    case OPR_CVTL:
    case OPR_TRUNC:
    case OPR_FLOOR:
      return Propagate_expr(expr->Opnd(0), v, stmt);

    // lb, ub
    case OPR_GE:
    case OPR_GT:
    case OPR_LE:
    case OPR_LT:
      lhs_v = Propagate_expr(expr->Opnd(0), VANT_UTIL::Empty(), stmt);
      rhs_v = Propagate_expr(expr->Opnd(1), VANT_UTIL::Empty(), stmt);
      {
        // handle lb, ub
        CODEREP_MAP lhs_var, rhs_var;
        VOR_MAP lhs_vor, rhs_vor;
        CR_UTIL(_comp_unit).Analyze_cmp_cr(expr, lhs_var, lhs_vor, rhs_var, rhs_vor);
        BOOL is_greater = (expr->Opr() == OPR_GE || expr->Opr() == OPR_GT);
        ANT_KIND lhs_ant = is_greater ? ANT_LB : ANT_UB;
        ANT_KIND rhs_ant = is_greater ? ANT_UB : ANT_LB;
        if (lhs_var.size() > 0)
          Set_annot(lhs_var, lhs_ant);
        if (lhs_vor.size() > 0)
          Set_annot(lhs_vor, lhs_ant);
        if (rhs_var.size() > 0)
          Set_annot(rhs_var, rhs_ant);
        if (rhs_vor.size() > 0)
          Set_annot(rhs_vor, rhs_ant);
        
      }
      return VANT_UTIL::And(lhs_v, rhs_v);

    // zero ptr
    case OPR_ADD:
    case OPR_MPY:
      lhs_v = Propagate_expr(expr->Opnd(0), v, stmt);
      rhs_v = Propagate_expr(expr->Opnd(1), v, stmt);
      return VANT_UTIL::Or(lhs_v, rhs_v);

    case OPR_SUB:
      // TODO: opnd(1)
      return Propagate_expr(expr->Opnd(0), v, stmt);

    // divisor, zero
    case OPR_DIV:
    case OPR_MOD:
    case OPR_REM:
    case OPR_DIVREM:
      lhs_v = Propagate_expr(expr->Opnd(0), v, stmt);
      rhs_v = VANT_UTIL::Set(v, ANT_DIV, ANT_YES);
      rhs_v = Propagate_expr(expr->Opnd(1), rhs_v, stmt);
      return VANT_UTIL::Or(lhs_v, rhs_v);

    case OPR_INTRINSIC_OP:
      return Process_intrn_op(expr, v, stmt);
    case OPR_ALLOCA:
      return VANT_UTIL::Set(v, ANT_VWRITE, ANT_YES);

    // TODO: other kind of Opr
    // don't handle call here. Call is propresses in Process_call()/etc
    default:
      break;
    }
    break;
  default:
    Is_True(FALSE, ("wrong cr kind"));
  }
  return VANT_UTIL::Empty();
} // LOCAL_CPROP::Propagate_expr

// LOCAL_CPROP::Propagate_stmt
// propagate flags for stmt
void
LOCAL_CPROP::Propagate_stmt(STMTREP *stmt, BB_NODE *curbb)
{
  if (stmt->Op() == OPC_XPRAGMA && // should move the inline body check here
      WN_pragma(stmt->Orig_wn()) == WN_PRAGMA_COPYIN_BOUND)
    return;

  // nothing to propagate
  if (stmt->Rhs() == NULL && !OPERATOR_is_call(stmt->Opr()))
    return;

  Is_Trace(Tracing(),
           (TFile, " -- before stmt propagation:\n"));
  Is_Trace_cmd(Tracing(), Vsa()->Print_sr(stmt, TFile));

  V_ANNOT rhs_v = VANT_UTIL::Empty();
  if (stmt->Rhs())
    rhs_v = Propagate_expr(stmt->Rhs(), VANT_UTIL::Empty(), stmt);

  CODEREP *base;
  VSYM_OBJ_REP *vor;
  INTRINSIC iopc;
  switch (stmt->Opr()) {
  case OPR_STID:
    // copy rhs annot to lhs
    rhs_v = VANT_UTIL::Set(rhs_v, ANT_WRITE, ANT_YES);
    stmt->Lhs()->Set_vsa_annot(rhs_v);
    break;

  case OPR_ISTORE:
  case OPR_ISTBITS:
  case OPR_MSTORE:
  {
    // handle Istr_base()
    Propagate_expr(stmt->Lhs()->Istr_base(), VANT_UTIL::Empty(), stmt);
    // annotate rhs|WRITE flag to lhs vor
    if ((vor = Vsa()->Cr_2_vor(stmt->Lhs())) != NULL) {
      rhs_v = VANT_UTIL::Set(rhs_v, ANT_WRITE, ANT_YES);
      vor->Set_vsa_annot(rhs_v);
    }
    // annotate lhs base VWRITE flag
    base = _comp_unit->Analyze_base_info(stmt,
                                         stmt->Lhs()->Istr_base(),
                                         FALSE);
    V_ANNOT base_v = VANT_UTIL::Var_2_vor(rhs_v);
    base_v = VANT_UTIL::Set(base_v, ANT_VWRITE, ANT_YES);
    while (base) {
      V_ANNOT merge_v = VANT_UTIL::Or(base_v, Get_annot(Vsa(), base));
      Set_annot(Vsa(), base, merge_v);
      if (base->Kind() == CK_IVAR) {
        base = base->Ilod_base();
      } else {
        break;
      }
    }
    // propagate mstore size
    if (stmt->Opr() == OPR_MSTORE) {
      Propagate_expr(stmt->Lhs()->Mstore_size(), VANT_UTIL::Empty(), stmt);
    }
    break;
  }
  case OPR_INTRINSIC_CALL:
  case OPR_CALL:
    Propagate_values_for_call(stmt);
    iopc = Get_call_intrinsic(stmt);
    if (iopc != INTRINSIC_NONE)
      Process_intrn_call(iopc, stmt);
    else
      Process_call(stmt);
    break;
  case OPR_ICALL:
    Propagate_values_for_call(stmt);
    Process_icall(stmt);
    break;

  case OPR_PRAGMA:
  case OPR_XPRAGMA:
  case OPR_DEALLOCA:
  case OPR_GOTO:
  case OPR_AGOTO:
  case OPR_COMPGOTO:
  case OPR_TRUEBR:
  case OPR_FALSEBR:
  case OPR_LABEL:
  case OPR_OPT_CHI:
  case OPR_PREFETCH:
  case OPR_RETURN:
  case OPR_EVAL:
  case OPR_ASM_STMT:
  case OPR_FORWARD_BARRIER:
  case OPR_BACKWARD_BARRIER:
    break;
  default:
    Is_True(FALSE, ("TODO: handle %s\n", OPERATOR_name(stmt->Opr()) + 4));
    break;
  }

  Is_Trace(Tracing(),
           (TFile, " -- after stmt propagation:\n"));
  Is_Trace_cmd(Tracing(), Vsa()->Print_sr(stmt, TFile));
} // LOCAL_CPROP::Propagate_stmt

// LOCAL_CPROP::Propagate_bb
// propagate flags on BB and it's dom bbs
void
LOCAL_CPROP::Propagate_bb(BB_NODE *bb)
{
  PHI_NODE *phi;
  PHI_LIST_ITER phi_iter;

  Is_Trace(Tracing(),
           (TFile, "---- propagation in BB%d:\n", bb->Id()));

  // processing var phi nodes
  OPT_STAB *stab = _comp_unit->Opt_stab();
  FOR_ALL_ELEM(phi, phi_iter, Init(bb->Phi_list())) {
    if (!phi->Live())
      continue;
    if (phi->Aux_id() == stab->Default_vsym() ||
        phi->Aux_id() == stab->Return_vsym())
      continue;

    CODEREP *opnd;
    PHI_OPND_ITER iter(phi);
    hash_set<IDTYPE> visited;
    CODEREP *res = phi->RESULT();
    V_ANNOT res_v = res->Vsa_annot();
    VAR_SCC scc(res, _comp_unit, _trace);
    FOR_ALL_ELEM(opnd, iter, Init()) {
      V_ANNOT opnd_v = Propagate_vardef(opnd, &scc, visited);
      if (!scc.Defer(opnd)) {
        res_v = VANT_UTIL::Join(res_v, opnd_v);
      }
    }
    res->Set_vsa_annot(res_v);
    scc.Update(_comp_unit);
  }

  // processing vor phi nodes
  VSA *vsa = Vsa();
  FOR_ALL_ELEM (phi, phi_iter, Init(vsa->Bb_vo_philist(bb))) {
    VSYM_OBJ_REP *res = (VSYM_OBJ_REP*)phi->RESULT();
    V_ANNOT res_v = res->Vsa_annot();
    hash_set<IDTYPE> visited;
    VOR_SCC scc(res, vsa, _trace);
    for (INT i = 0; i < phi->Size(); ++i) {
      VSYM_OBJ_REP *opnd = (VSYM_OBJ_REP *)phi->OPND(i);
      V_ANNOT opnd_v = Propagate_vordef(opnd, &scc, visited);
      if (!scc.Defer(opnd)) {
        res_v = VANT_UTIL::Join(res_v, opnd_v);
      }
    }
    res->Set_vsa_annot(res_v);
    scc.Update(vsa);
  }

  // processing stmt list
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {
    // process stmt
    Propagate_stmt(stmt, bb);

#if 0
    CHI_NODE *chi;
    CHI_LIST_ITER chi_iter;
    // process var chi on stmt
    FOR_ALL_NODE(chi, chi_iter, Init(stmt->Chi_list())) {
      // Do nothing? or propagate flag from OPND to RESULT?
    }
    // process vor chi on stmt
    FOR_ALL_NODE(chi, chi_iter, Init(vsa->Stmt_vor_chi(stmt))) {
      CVOR *cvor = (CVOR*)cnode->OPND();
      // Do noting? or propagate flag from OPND to RESULT?
    }
#endif
  }

  // processing dom bb
  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb->Dom_bbs()))
    Propagate_bb(dom_bb);

} // LOCAL_CPROP::Propagate_bb

void
LOCAL_CPROP::Propagate_rbc_dna_flags()
{
  DNA_NODE *dna = _comp_unit->Dna();
  DNODE_VECTOR *rbc_nodes = _comp_unit->Vsa()->Ipsa()->Rbc()->Get_rbc_nodes(dna);
  UINT32 prop_flags = 0;
  if (rbc_nodes) {
    DNODE_VECTOR::const_iterator rbc_iter = rbc_nodes->begin();
    for (; rbc_iter != rbc_nodes->end(); rbc_iter++) {
      DNA_NODE *rbc_callee = *rbc_iter;
      if (rbc_callee != NULL) {
        prop_flags |= rbc_callee->Flags();
      }
    }
  }
  UINT32 prop_mask = DNA_IN_HELP_THREAD | DNA_IN_WORK_THREAD | DNA_IN_ISR;
  prop_flags = prop_flags & prop_mask;
  if (prop_flags) {
    dna->Set_flag(prop_flags);
  }
}

// LOCAL_CPROP::Propagate_entry
// propagate clby's args on argument to formal
void
LOCAL_CPROP::Propagate_entry()
{
  DNA_NODE *dna = _comp_unit->Dna();
  for (INT parm = VAR_INIT_ID;
       parm < dna->Parm_list()->size(); parm++) {
    CODEREP *parm_cr = dna->Get_param_cr(parm);
    if (parm_cr == NULL)
      continue;
    Is_True(parm_cr->Kind() == CK_VAR, ("parm is not var"));
    V_ANNOT annot = VANT_UTIL::Empty();  // should be parm_cr->Vsa_annot()?
    for (INT clby = VAR_INIT_ID;
         clby < dna->Clby_list()->size(); clby++) {
      RNA_NODE *rna = (*dna->Clby_list())[clby];
      if (parm > rna->Arg_cnt()) {
        continue;
      }
      CODEREP *arg = rna->Get_arg(parm);
      DNA_NODE *caller = _ipsa->Get_dna(rna->Caller_idx());
      V_ANNOT arg_annot = Get_cr_annot(caller->Comp_unit()->Vsa(), arg);
      annot = VANT_UTIL::Merge_parm(annot, arg_annot);
    }
    parm_cr->Set_vsa_annot(annot);
  }
}

// LOCAL_CPROP::Propagate_vor_chi
// propagate annot to vor chi
void
LOCAL_CPROP::Propagate_vor_chi(VSA *vsa, STMTREP *stmt, CODEREP *cr, V_ANNOT v)
{
  CHI_LIST *chi_list = vsa->Stmt_vor_chi(stmt);
  if (chi_list && !chi_list->Is_Empty()) {
    CHI_NODE *cnode;
    CHI_LIST_ITER chi_iter;
    FOR_ALL_NODE(cnode, chi_iter, Init(chi_list)) {
      CVOR *cvor = (CVOR*)cnode->RESULT();
      if (cvor->second == cr) {
        VSYM_OBJ_REP *vor = (VSYM_OBJ_REP*)cvor->first;
        V_ANNOT merge_v = VANT_UTIL::Or(vor->Vsa_annot(), v);
        vor->Set_vsa_annot(merge_v);
      }
    }
  }
}

void
LOCAL_CPROP::Propagate_values_for_call(STMTREP *stmt)
{
  // do propagate after ho/vo been created
  if (!_hva || !VSA_Prop_Values ) {
    return;
  }
  Is_True_Ret(stmt && OPERATOR_is_call(stmt->Opr()), ("LOCAL_CPROP: not call"));
  RNA_NODE *rna = Vsa()->Sr_2_rna(stmt);
  if (rna->Is_flag_set(RBC_SE_CONTAINER_OP)) {
    Vsa()->Ipsa()->Rbc()->Eval__mvsa_container_op(Vsa()->Dna(), rna, Loc_pool(), NULL, TRUE);
  }
}

// LOCAL_CPROP::Print
void
LOCAL_CPROP::Print(FILE *fp) const
{
  Vsa()->Print_obj("var/vor flag after constant prop", fp);
} // LOCAL_CPROP::Print

// ==================================================================
// IPSA_CPROP
//
// propagate constant and flags among functions
// ==================================================================
class IPSA_CPROP {
private:
  IPSA *_ipsa;      // pointere to IPSA
  BOOL  _hva;       // prop over ho/vo?

private:
  // do propagation in single function
  void Local_prop(DNA_NODE *dna) {
    LOCAL_CPROP prop(_ipsa, dna->Comp_unit(), _hva);
    prop.Do_prop();
  } // Local_prop

public:
  // constructor
  IPSA_CPROP(IPSA *ipsa, BOOL hva) : _ipsa(ipsa), _hva(hva) { }

  // do single function propagation
  void Do_prop(DNA_NODE *func) {
    Local_prop(func);
  } // Do_prop

  // do whole-program propagation
  void Do_prop() {
    DNODE_ITER<DNA_TRAV_BOTTOM_UP_ORDER> iter(_ipsa);
    for (; !iter.Is_end(); iter.Next()) {
      DNA_NODE *func = iter.Current();
      if (func->Non_functional())
        continue;
      Do_prop(func);
    }
  } // Do_prop

};  // IPSA_CPROP


// ==================================================================
// LOCAL_SPROP
//
// backward propagate side effect and flags in single function
// - backward traverse the DOM tree and BB statements
// - propagate flags on callstmt's return value to this callee's
//   return value at beginning
// - propagate flags on callstmt's output parameter to this callee's
//   last assignment on output parameter at beginning
// - propagate flags on callee's formal to this callstmt's argument
//   during visiting callstmt
// ==================================================================
class LOCAL_SPROP {
  typedef TSCC<CODEREP *, COMP_UNIT *> VAR_SCC;
  typedef TSCC<VSYM_OBJ_REP *, VSA *> VOR_SCC;

private:
  IPSA      *_ipsa;        // IPSA to propagate flags for calls
  COMP_UNIT *_comp_unit;   // CU to be propagated
  BOOL       _hva;         // prop over ho/vo
  BOOL       _trace;       // is trace on

private:
  BOOL    Tracing() const { return _trace; }
  VSA    *Vsa() const     { return _comp_unit->Vsa(); }

private:
  // handle intrinsic op
  void    Process_intrn_op(CODEREP *var, V_ANNOT v, STMTREP *stmt);
  // handle intrinsic call
  void    Process_intrn_call(INTRINSIC intrn, STMTREP *stmt);
  // handle call
  void    Process_call(STMTREP *stmt);
  // handle indirect call
  void    Process_icall(STMTREP *stmt);
  // handle call with ipsa rna
  void    Process_rna(RNA_NODE *rna);

  // propagate flags for var within the SCC
  V_ANNOT Propagate_vardef(CODEREP *var, VAR_SCC *scc, hash_set<UINT32> &visited);
  // propagate flags for vor within the SCC
  V_ANNOT Propagate_vordef(VSYM_OBJ_REP *vor, VOR_SCC *scc, hash_set<UINT32> &visited);
  // propagate flags for coderep used in stmt
  V_ANNOT Propagate_expr(CODEREP *expr, V_ANNOT v, STMTREP *stmt);
  // propagate flags for stmt
  void    Propagate_stmt(STMTREP *stmt, BB_NODE *curbb);
  // propagate flags for bb
  void    Propagate_bb(BB_NODE *bb);
  // propagate return value/output parameter in clby list to return stmt
  void    Propagate_exit();

public:
  // constructor
  LOCAL_SPROP(IPSA *ipsa, COMP_UNIT *cu, BOOL hva)
    : _ipsa(ipsa), _comp_unit(cu), _hva(hva) {
    _trace = Get_Trace(TP_VSA, VSA_PROP_TRACE_FLAG);
  } // LOCAL_SPROP

  // do propagation
  void Do_prop() {
    Is_Trace(Tracing(),
             (TFile, "============================================================\n"));
    Is_Trace(Tracing(),
             (TFile, "Side-effect prop tracing for %s\n",
              _comp_unit->Dna()->Fname()));
    // for heap_obj annotation propagation
    HOA_PROP heap_prop(_ipsa, _comp_unit);

    if (VSA_HOA_Prop && _hva) {
      // propagate clby's heap_obj annotation to formal in
      heap_prop.Propagate_ho_annot_on_entry();
    }

    // propagate return value/output parameter in clby list to return stmt
    Propagate_exit();

    // post-order traverse the DOM tree recursively
    // TODO: change to non-recursive traversal
    Propagate_bb(_comp_unit->Cfg()->Entry_bb());

    // dump flag after propagation
    if (VSA_HOA_Prop && _hva) {
      // propagate return value/output param's heap_obj annotation
      heap_prop.Calculate_ho_annot_on_return();

      Is_Trace(Get_Trace(TP_VSA, VSA_HPROP_TRACE_FLAG),
               (TFile, "============================================================\n"));
      Is_Trace(Get_Trace(TP_VSA, VSA_HPROP_TRACE_FLAG),
               (TFile, "DUMP after backward hoa prop for %s\n", _comp_unit->Dna()->Fname()));
      Is_Trace_cmd(Get_Trace(TP_VSA, VSA_HPROP_TRACE_FLAG),
                   HOA_PROP::Print_dna(_comp_unit->Dna(), TFile));
      Is_Trace(Get_Trace(TP_VSA, VSA_HPROP_TRACE_FLAG),
               (TFile, "============================================================\n"));
    }

    // dump flag after propagation
    Is_Trace(Get_Trace(TP_VSA, VSA_PROP_DUMP_FLAG),
             (TFile, "============================================================\n"));
    Is_Trace(Get_Trace(TP_VSA, VSA_PROP_DUMP_FLAG),
             (TFile, "DUMP after side-effect prop for %s\n", _comp_unit->Dna()->Fname()));
    Is_Trace_cmd(Get_Trace(TP_VSA, VSA_PROP_DUMP_FLAG), Print(TFile));
    Is_Trace(Get_Trace(TP_VSA, VSA_PROP_DUMP_FLAG),
             (TFile, "============================================================\n"));
  } // Do_prop

  // print the annotation
  void Print(FILE *fp) const;

}; // LOCAL_SPROP

// LOCAL_SPROP::Process_intrn_op
// process intrinsic op to backward prooagate flags on parameter
void
LOCAL_SPROP::Process_intrn_op(CODEREP *op, V_ANNOT v, STMTREP *stmt)
{
  Is_True(op->Kind() == CK_OP && op->Opr() == OPR_INTRINSIC_OP,
          ("not intrinsic op"));
  // set flags on parameters according to intrinsic op semantics
  // TODO: which intrinsic op? or already done in LOCAL_CPROP?
} // LOCAL_SPROP::Process_intrn_op

// LOCAL_SPROP::Process_intrn_call
// process intrinsic call to backward prooagate flags on parameter
void
LOCAL_SPROP::Process_intrn_call(INTRINSIC intrn, STMTREP *stmt)
{
  Is_True(intrn != INTRINSIC_NONE &&
          (stmt->Opr() == OPR_CALL ||
           stmt->Opr() == OPR_INTRINSIC_CALL),
          ("not intrinsic call"));
  RNA_NODE *rna = Vsa()->Sr_2_rna(stmt);
  if (rna == NULL) {
    return;
  }
  // connect intrinsic rna with rbc nodes
  if (_hva == FALSE) {
    const char *callee_name = INTRINSIC_name(stmt->Rhs()->Intrinsic());
    if (callee_name != NULL && rna->Callee_cnt() == 0) {
      IDTYPE rbc_idx = _ipsa->Rbc()->Get_dna_idx_by_name((char*)callee_name);
      // as it is an intrinsic call, the callee here should be a RBC node
      DNA_NODE *callee = _ipsa->Get_dna(rbc_idx);
      if (callee != NULL) {
        // fetch its rbc nodes list
        DNODE_VECTOR *rbc_nodes = _ipsa->Rbc()->Get_rbc_nodes(callee);
        if (rbc_nodes != NULL) {
          DNODE_VECTOR::const_iterator iter = rbc_nodes->begin();
          for (; iter != rbc_nodes->end(); iter++) {
            DNA_NODE *rbc_callee = *iter;
            if (rbc_callee == NULL) {
              continue;
            }
            rna->Add_callee(rbc_callee->Dna_idx(), ICALL_VAR_TARGET);
            rna->Set_flag(RNA_HAS_RBC_NODES);
            rbc_callee->Inc_ref_cnt(rna);
          }
        } // rbc_nodes != NULL
      }
    }
  }
  Process_rna(rna);
  // set flags on parameters according to intrinsic call semantics
  // TODO: which intrinsic op? or already done in LOCAL_CPROP?
} // LOCAL_SPROP::Process_intrn_call

// LOCAL_SPROP::Process_call
// process call to backward prooagate flags on parameter
void
LOCAL_SPROP::Process_call(STMTREP *stmt)
{
  Is_True(stmt->Opr() == OPR_CALL && stmt->St() != NULL,
          ("bad call"));
  // set flags on parameters according to call's ST
  // TODO: which ST? or already done in LOCAL_CPROP?

  RNA_NODE *rna = Vsa()->Sr_2_rna(stmt);
  if (rna != NULL)
    Process_rna(rna);

} // LOCAL_SPROP::Process_call

// LOCAL_SPROP::Process_icall
// process indirect call to backward prooagate flags on parameter
void
LOCAL_SPROP::Process_icall(STMTREP *stmt)
{
  Is_True(stmt->Opr() == OPR_ICALL,
          ("bad icall"));

  RNA_NODE *rna = Vsa()->Sr_2_rna(stmt);
  if (rna != NULL)
    Process_rna(rna);

} // LOCAL_SPROP::Process_icall

// LOCAL_SPROP::Process_rna
// process rna to backward prooagate flags from callee to caller
void
LOCAL_SPROP::Process_rna(RNA_NODE *rna)
{
  Is_True(rna != NULL, ("bad rna"));

  if (_ipsa == NULL)
    return;

  rna->Prop_rbc_flags(_ipsa);

  STMTREP *sr = rna->Callstmt();
  Is_True(sr && OPERATOR_is_call(sr->Opr()), ("bad rna callstmt"));

  for(IDTYPE i = VAR_INIT_ID; i <= rna->Arg_cnt(); i++) {
    CODEREP* arg = rna->Get_arg(i);
    Is_True(arg != NULL,
            ("can not get actual %d.", i));

    V_ANNOT annot = VANT_UTIL::Empty();
    // process all callees
    for(CALLEE_VECTOR::const_iterator it = rna->Callee_list().begin();
        it != rna->Callee_list().end(); ++it) {
      DNA_NODE* callee = _ipsa->Get_dna(it->Callee());
      Is_True(callee != NULL, ("bad callee"));
      if (callee->Non_functional()) {
        continue;
      }

      if (i > callee->Parm_list()->size())
        continue;

      CODEREP *parm_cr = callee->Get_param_cr(i);
      if (parm_cr == NULL)
        continue;
      Is_True(parm_cr->Kind() == CK_VAR, ("parm is not var"));

      V_ANNOT parm_v = parm_cr->Vsa_annot();
      annot = VANT_UTIL::Merge_parmin_rev(annot, parm_v);
    }
    // if no callee, and passed by LDA, mark with VWRITE maybe
    if (arg->Kind() == CK_LDA && arg->Lda_base_st() && rna->Callee_cnt() == 0) {
      VSYM_FLD_REP zero_fld(FLD_K_ID, 0, 0);
      CHI_NODE *chi = Vsa()->Find_stmt_var_chi(sr, arg->Lda_base_st(), &zero_fld);
      if (chi) {
        annot = VANT_UTIL::Set(annot, ANT_VWRITE, ANT_MAYBE);
      }
    }

    // get rna flag from rbc model
    UINT32 rbc_parm_flags = _ipsa->Rna_get_rbc_parm_flag(rna, i);
    rbc_parm_flags |= rna->Get_arg_flags(i);
    // propagate to parm annoation to RNA
    if (VANT_UTIL::Get(annot, ANT_VREAD) != ANT_NO) {
      rna->Set_arg_flag(i, REF_ILOAD);
      Is_Trace(Tracing(),
               (TFile, " -- set call sr%d %s arg %d ILOAD flag.\n",
                       sr->Stmtrep_id(),
                       sr->Opr() == OPR_CALL ? ST_name(sr->St()) : "-nil-", i));
    } else if (rbc_parm_flags & REF_ILOAD) {
      annot = VANT_UTIL::Set(annot, ANT_VREAD, ANT_YES);
      rna->Set_arg_flag(i, REF_ILOAD);
      Is_Trace(Tracing(),
               (TFile, " -- set call sr%d %s arg %d ILOAD flag by rbc model\n",
                       sr->Stmtrep_id(),
                       sr->Opr() == OPR_CALL ? ST_name(sr->St()) : "-nil-", i));
    }
    if (VANT_UTIL::Get(annot, ANT_VWRITE) != ANT_NO) {
      rna->Set_arg_flag(i, REF_ISTORE);
      Is_Trace(Tracing(),
               (TFile, " -- set call sr%d %s arg %d ISTORE flag.\n",
                       sr->Stmtrep_id(),
                       sr->Opr() == OPR_CALL ? ST_name(sr->St()) : "-nil-", i));
    } else if (rbc_parm_flags & REF_ISTORE) {
      annot = VANT_UTIL::Set(annot, ANT_VWRITE, ANT_YES);
      rna->Set_arg_flag(i, REF_ISTORE);
      Is_Trace(Tracing(),
               (TFile, " -- set call sr%d %s arg %d ISTORE flag by rbc model.\n",
                       sr->Stmtrep_id(),
                       sr->Opr() == OPR_CALL ? ST_name(sr->St()) : "-nil-", i));
    }


    // propagate the free flag to dna parm
    ANT_STAT stat = VANT_UTIL::Get(annot, ANT_FREE);
    ANT_STAT vstat = VANT_UTIL::Get(annot, ANT_VFREE);
    if (stat != ANT_NO) {
      rna->Set_arg_flag(i, REF_FREED);
      BOOL   is_ptr;
      IDTYPE_SET visited_set;
      DNA_NODE *dna = Vsa()->Dna();
      IDTYPE arg_param = dna->Find_param_references(arg, &is_ptr, IN_NONE, visited_set);
      if (arg->Kind() == CK_VAR && arg_param != INVALID_VAR_IDX) {
        dna->Set_deallocate(stat == ANT_MAYBE ? FALSE : TRUE);
        dna->Set_parm_flag(arg_param, REF_FREED);
      }
    }
    // propagate vfree from callee parm to caller lda mu cr
    if (arg->Kind() == CK_LDA && vstat != ANT_NO) {
      VSYM_FLD_REP zero_fld(FLD_K_ID, 0, 0);
      MU_NODE *mu = Vsa()->Find_stmt_var_mu(sr, arg->Lda_base_st(), &zero_fld);
      CODEREP *mu_cr = mu ? mu->OPND() : NULL;
      if (mu_cr) {
        V_ANNOT mu_annot = VANT_UTIL::Set(Get_annot(Vsa(), mu_cr), ANT_FREE, vstat);
        Set_annot(Vsa(), mu_cr, mu_annot);
      }
    }
    // propagate to argument
    annot = Propagate_expr(arg, annot, sr);
  }

  // propagate heap_obj annotation from callee to rna
  if (VSA_HOA_Prop && _hva) {
    HOA_PROP heap_prop(_ipsa, _comp_unit);
    heap_prop.Calculate_ho_annot_on_rna(rna);
    heap_prop.Propagate_ho_annot_from_rna(rna);
  }

} // LOCAL_SPROP::Process_rna

// LOCAL_SPROP::Propagate_vardef
// backward propagate flags for var within the SCC
V_ANNOT
LOCAL_SPROP::Propagate_vardef(CODEREP *var, VAR_SCC *scc, hash_set<UINT32> &visited)
{
  Is_True(var && var->Kind() == CK_VAR, ("not var cr"));
  if (var->Is_flag_set(CF_DEF_BY_CHI)) {
    STMTREP *def = var->Defstmt();
    Is_True(def, ("vor defstmt is null"));
    if (def && def->Opr() == OPR_OPT_CHI) {
      // TODO: propagate to DNA arg list
    }
    else if (def && OPERATOR_is_call(def->Opr())) {
      // TODO: propagate to RNA parm list
    }
    else if (def) {
      // TODO: chi def
    }
    return var->Vsa_annot();
  }

  if (var->Is_flag_set(CF_DEF_BY_PHI)) {
    PHI_NODE *phi = var->Defphi();
    Is_True(phi && phi->RESULT() == var, ("bad phi def"));
    V_ANNOT res_v = var->Vsa_annot();
    if (visited.find(phi->Bb()->Id()) != visited.end())
      return res_v;
    visited.insert(phi->Bb()->Id());

    CODEREP *opnd;
    PHI_OPND_ITER iter(phi);
    FOR_ALL_ELEM(opnd, iter, Init()) {
      V_ANNOT prop_v = VANT_UTIL::Bwd_prop_annot(res_v);
      V_ANNOT opnd_v = VANT_UTIL::Or(opnd->Vsa_annot(), prop_v);
      opnd->Set_vsa_annot(opnd_v);
      opnd_v = Propagate_vardef(opnd, scc, visited);
      if (!scc->Defer(opnd)) {
        res_v = VANT_UTIL::Join(res_v, opnd_v);
      }
    }
    var->Set_vsa_annot(res_v);
    scc->Update(_comp_unit);
    return var->Vsa_annot();
  }

  return var->Vsa_annot();
} // LOCAL_SPROP::Propagate_vardef

// LOCAL_SPROP::Propagate_vordef
// backward propagate flags for vor within the SCC
V_ANNOT
LOCAL_SPROP::Propagate_vordef(VSYM_OBJ_REP *vor, VOR_SCC *scc, hash_set<UINT32> &visited)
{
  Is_True(vor, ("bad vor"));
  if (vor->Attr() == ROR_DEF_BY_CHI) {
    STMTREP *def = vor->Stmt_def();
    if (def && def->Opr() == OPR_OPT_CHI) {
      // TODO: propagate to DNA arg list
    }
    else if (def && OPERATOR_is_call(def->Opr())) {
      // TODO: propagate to RNA parm list
    }
    else if (def) {
      // TODO: chi def
    }
    return vor->Vsa_annot();
  }

  if (vor->Attr() == ROR_DEF_BY_PHI) {
    PHI_NODE *phi = vor->Phi_def();
    Is_True(phi && phi->RESULT() == (CODEREP*)vor, ("bad vor"));
    V_ANNOT res_v = vor->Vsa_annot();
    if (visited.find(phi->Bb()->Id()) != visited.end())
      return res_v;
    visited.insert(phi->Bb()->Id());

    for (INT i = 0; i < phi->Size(); ++i) {
      VSYM_OBJ_REP *opnd = (VSYM_OBJ_REP*)phi->OPND(i);
      V_ANNOT prop_v = VANT_UTIL::Bwd_prop_annot(res_v);
      V_ANNOT opnd_v = VANT_UTIL::Or(opnd->Vsa_annot(), prop_v);
      opnd->Set_vsa_annot(opnd_v);
      opnd_v = Propagate_vordef(opnd, scc, visited);
      if (!scc->Defer(opnd)) {
        res_v = VANT_UTIL::Join(res_v, opnd_v);
      }
    }
    vor->Set_vsa_annot(res_v);
    scc->Update(Vsa());
    return vor->Vsa_annot();
  }

  return vor->Vsa_annot();
} // LOCAL_SPROP::Propagate_vordef

// LOCAL_SPROP::Propagate_expr
// backward propagate flags for coderep used in stmt
V_ANNOT
LOCAL_SPROP::Propagate_expr(CODEREP *expr, V_ANNOT v, STMTREP *stmt)
{
  Is_True(expr, ("expr is NULL"));

  VSYM_OBJ_REP *vor;
  CODEREP *base_cr;
  ANT_STAT stat;
  switch (expr->Kind()) {
  case CK_LDA:
  case CK_CONST:
  case CK_RCONST:
    break;

  case CK_VAR:
    v = VANT_UTIL::Or(expr->Vsa_annot(), v);
    if (TY_kind(expr->object_ty()) != KIND_POINTER)
      v = VANT_UTIL::Remove_ptr_annot(v);
    expr->Set_vsa_annot(v);
    break;

  case CK_IVAR:
    Is_True(expr->Opr() != OPR_PARM, ("no parm here"));
    Is_True(expr->Ilod_base() != NULL, ("no istore here"));
    if ((vor = Vsa()->Cr_2_vor(expr)) != NULL) {
      V_ANNOT vor_v = VANT_UTIL::Or(vor->Vsa_annot(), v);
      if (TY_kind(expr->object_ty()) != KIND_POINTER)
        vor_v = VANT_UTIL::Remove_ptr_annot(vor_v);
      vor->Set_vsa_annot(vor_v);
    }
    {
      // propagate VWRITE/FREE flag to base if lhs has VWRITE flag
      ANT_STAT vwrite_stat = VANT_UTIL::Get(v, ANT_VWRITE);
      ANT_STAT free_stat = VANT_UTIL::Get(v, ANT_FREE);
      CODEREP *base_cr = Find_ilod_base(expr->Ilod_base());
      while (base_cr != NULL &&
            (base_cr->Kind() == CK_VAR || base_cr->Kind() == CK_IVAR)) {
        V_ANNOT base_annot = Get_annot(Vsa(), base_cr);
        if (vwrite_stat != ANT_NO)
          base_annot = VANT_UTIL::Set(base_annot, ANT_VWRITE, vwrite_stat);
        if (free_stat != ANT_NO)
          base_annot = VANT_UTIL::Set(base_annot, ANT_VFREE, free_stat);
        Set_annot(Vsa(), base_cr, base_annot);
        base_cr = (base_cr->Kind() == CK_IVAR)
                    ? Find_ilod_base(base_cr->Ilod_base()) : NULL;
      }
    }
    break;

  case CK_OP:
    switch (expr->Opr()) {
    case OPR_CVT:
    case OPR_CVTL:
    case OPR_TRUNC:
    case OPR_FLOOR:
      return Propagate_expr(expr->Opnd(0), VANT_UTIL::Remove_ptr_annot(v), stmt);

    case OPR_ADD:
      Propagate_expr(expr->Opnd(0), v, stmt);
      Propagate_expr(expr->Opnd(1), v, stmt);
      break;
    case OPR_MPY:
      Propagate_expr(expr->Opnd(0), VANT_UTIL::Remove_ptr_annot(v), stmt);
      Propagate_expr(expr->Opnd(1), VANT_UTIL::Remove_ptr_annot(v), stmt);
      break;

    case OPR_SUB:
    case OPR_DIV:
    case OPR_MOD:
    case OPR_REM:
    case OPR_DIVREM:
      // TODO: opnd(1)
      return Propagate_expr(expr->Opnd(0), VANT_UTIL::Remove_ptr_annot(v), stmt);

    default:
      break;
    }
    break;

  default:
    Is_True(FALSE, ("wrong cr kind"));
  }

  return v;
} // LOCAL_SPROP::Propagate_expr

// LOCAL_SPROP::Propagate_stmt
// backward propagate flags for stmt
void
LOCAL_SPROP::Propagate_stmt(STMTREP *stmt, BB_NODE *curbb)
{
  if (stmt->Op() == OPC_XPRAGMA && // should move the inline body check here
      WN_pragma(stmt->Orig_wn()) == WN_PRAGMA_COPYIN_BOUND)
    return;

  // nothing to propagate
  if (stmt->Rhs() == NULL && !OPERATOR_is_call(stmt->Opr()))
    return;

  Is_Trace(Tracing(),
           (TFile, " -- before stmt propagation:\n"));
  Is_Trace_cmd(Tracing(), Vsa()->Print_sr(stmt, TFile));

  V_ANNOT lhs_v = VANT_UTIL::Empty();
  VSYM_OBJ_REP *vor;
  INTRINSIC iopc;
  switch (stmt->Opr()) {
  case OPR_STID:
    lhs_v = stmt->Lhs()->Vsa_annot();
    break;

  case OPR_ISTORE:
  case OPR_ISTBITS:
  case OPR_MSTORE:
    if ((vor = Vsa()->Cr_2_vor(stmt->Lhs())) != NULL) {
      lhs_v = vor->Vsa_annot();
    }
    break;

  case OPR_INTRINSIC_CALL:
  case OPR_CALL:
    iopc = Get_call_intrinsic(stmt);
    if (iopc != INTRINSIC_NONE)
      Process_intrn_call(iopc, stmt);
    else
      Process_call(stmt);
    break;

  case OPR_ICALL:
    Process_icall(stmt);
    break;

  default:
    break;
  }

  if (lhs_v != VANT_UTIL::Empty() && stmt->Rhs()) {
    Propagate_expr(stmt->Rhs(), VANT_UTIL::Bwd_prop_annot(lhs_v), stmt);
  }

  Is_Trace(Tracing(),
           (TFile, " -- after stmt propagation:\n"));
  Is_Trace_cmd(Tracing(), Vsa()->Print_sr(stmt, TFile));
} // LOCAL_SPROP::Propagate_stmt

// LOCAL_SPROP::Propagate_bb(BB_NODE *bb)
// backward propagate flags on BB and it's pdom bbs
void
LOCAL_SPROP::Propagate_bb(BB_NODE *bb)
{
  // processing dom bb at first
  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb->Dom_bbs()))
    Propagate_bb(dom_bb);

  Is_Trace(Tracing(),
           (TFile, "---- propagation in BB%d:\n", bb->Id()));

  // backward processing stmt list
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE_REVERSE(stmt, stmt_iter, Init()) {
    // process stmt
    Propagate_stmt(stmt, bb);
  }

  // processing phi nodes
  PHI_NODE *phi;
  PHI_LIST_ITER phi_iter;
  OPT_STAB *stab = _comp_unit->Opt_stab();
  FOR_ALL_ELEM(phi, phi_iter, Init(bb->Phi_list())) {
    if (!phi->Live())
      continue;
    if (phi->Aux_id() == stab->Default_vsym() ||
        phi->Aux_id() == stab->Return_vsym())
      continue;

    CODEREP *opnd;
    PHI_OPND_ITER iter(phi);
    hash_set<IDTYPE> visited;
    CODEREP *res = phi->RESULT();
    V_ANNOT res_v = res->Vsa_annot();
    VAR_SCC scc(res, _comp_unit, _trace);
    FOR_ALL_ELEM(opnd, iter, Init()) {
      V_ANNOT prop_v = VANT_UTIL::Bwd_prop_annot(res_v);
      V_ANNOT opnd_v = VANT_UTIL::Or(opnd->Vsa_annot(), prop_v);
      opnd->Set_vsa_annot(opnd_v);
      opnd_v = Propagate_vardef(opnd, &scc, visited);
      if (!scc.Defer(opnd)) {
        res_v = VANT_UTIL::Join(res_v, opnd_v);
      }
    }
    res->Set_vsa_annot(res_v);
    scc.Update(_comp_unit);
  }

  // processing vor phi nodes
  VSA *vsa = Vsa();
  FOR_ALL_ELEM (phi, phi_iter, Init(vsa->Bb_vo_philist(bb))) {
    VSYM_OBJ_REP *res = (VSYM_OBJ_REP*)phi->RESULT();
    V_ANNOT res_v = res->Vsa_annot();
    hash_set<IDTYPE> visited;
    VOR_SCC scc(res, vsa, _trace);
    for (INT i = 0; i < phi->Size(); ++i) {
      VSYM_OBJ_REP *opnd = (VSYM_OBJ_REP *)phi->OPND(i);
      V_ANNOT prop_v = VANT_UTIL::Bwd_prop_annot(res_v);
      V_ANNOT opnd_v = VANT_UTIL::Or(opnd->Vsa_annot(), prop_v);
      opnd->Set_vsa_annot(opnd_v);
      opnd_v = Propagate_vordef(opnd, &scc, visited);
      if (!scc.Defer(opnd)) {
        res_v = VANT_UTIL::Join(res_v, opnd_v);
      }
      res->Set_vsa_annot(res_v);
      scc.Update(vsa);
    }
  }
} // LOCAL_SPROP::Propagate_bb

// propagate flags on clby's return value or output param to return stmt
// LOCAL_SPROP::Propagate_exit()
void
LOCAL_SPROP::Propagate_exit()
{
  if (_ipsa == NULL)
    return;

  DNA_NODE *dna = _comp_unit->Dna();
  // check all return stmt
  for (INT i = VAR_INIT_ID;
       i < dna->Retv_list()->size(); ++i) {
    PDV_NODE* pdv = (*dna->Retv_list())[i];
    STMTREP *stmt = pdv->Stmt();

    // TODO: BY_FREEIPARM, BY_GLOBALVAR, BY_FREEGLOBL
    if (pdv->Kind() != BY_RETURNSTMT &&
        pdv->Kind() != BY_PARAMETER)
      continue;

    Is_True(stmt &&
            stmt->Lhs() &&
            (stmt->Lhs()->Kind() == CK_VAR ||
             stmt->Lhs()->Kind() == CK_IVAR), ("not var or ivar"));

    BOOL is_return = (pdv->Kind() == BY_RETURNSTMT && pdv->Oparam() == 0);
    Is_True(!is_return ||
            (stmt != NULL && stmt->Opr() == OPR_STID &&
             stmt->Next() != NULL && stmt->Next()->Opr() == OPR_RETURN),
            ("bad return value"));
    BOOL is_outparm = pdv->Oparam() > 0;
    Is_True(!is_outparm ||
            (pdv->Oparam() >= VAR_INIT_ID &&
             pdv->Oparam() <= dna->Parm_list()->size()),
            ("bad output parm"));
    Is_True(is_return || is_outparm, ("not retval or outparm"));

    V_ANNOT annot = VANT_UTIL::Empty();
    // check all callers
    for (INT clby = VAR_INIT_ID;
         clby < dna->Clby_list()->size(); clby++) {
      RNA_NODE *rna = (*dna->Clby_list())[clby];

      if (pdv->Oparam() > rna->Arg_cnt())
        continue;

      STMTREP *sr = rna->Callstmt();
      Is_True(sr && OPERATOR_is_call(sr->Opr()), ("bad callstmt"));
      DNA_NODE *caller = _ipsa->Get_dna(rna->Caller_idx());
      COMP_UNIT *caller_cu = caller->Comp_unit();
      CODEREP *actual = is_return ? caller_cu->Find_return_value(sr)
                                  : rna->Get_arg(pdv->Oparam());
      Is_True(is_return || actual != NULL,
              ("not find actual retv or oparm"));
      if (actual == NULL)
        continue;
      V_ANNOT actual_v = Get_cr_annot(caller_cu->Vsa(), actual);
      annot = is_return
                       ? VANT_UTIL::Merge_retv_rev(annot, actual_v)
                       : VANT_UTIL::Merge_parmout_rev(annot, actual_v);
    }
    if (annot != VANT_UTIL::Empty()) {
      // set annot on rhs and propagate_expr do the rest
      Set_annot(Vsa(), stmt->Lhs(), annot);
    }
  }
}


// LOCAL_SPROP::Print
void
LOCAL_SPROP::Print(FILE *fp) const
{
  Vsa()->Print_obj("var/vor flag after side effect prop", fp);
} // LOCAL_SPROP::Print


// ==================================================================
// IPSA_SPROP
//
// propagate side effect flags among functions
// ==================================================================
class IPSA_SPROP {
private:
  IPSA        *_ipsa;          // pointere to IPSA
  BOOL         _hva;           // prop over ho/vo?

private:
  // do propagation in single function
  void Local_prop(DNA_NODE *dna) {
    LOCAL_SPROP prop(_ipsa, dna->Comp_unit(), _hva);
    prop.Do_prop();
  } // Local_prop

public:
  // constructor
  IPSA_SPROP(IPSA *ipsa, BOOL hva) : _ipsa(ipsa), _hva(hva) { }

  // do single function propagation
  void Do_prop(DNA_NODE *func) {
    Local_prop(func);
  } // Do_prop

  // do whole-program propagation
  void Do_prop() {
    DNODE_ITER<DNA_TRAV_BOTTOM_UP_ORDER> iter(_ipsa);
    for (; !iter.Is_end(); iter.Next()) {
      DNA_NODE *func = iter.Current();
      if (func->Non_functional())
        continue;
      Do_prop(func);
    }
  } // Do_prop

};  // IPSA_SPROP

// ==================================================================
// IPSA::Perform_constant_prop
//
// Perform inter-procedure constant propagation
// ==================================================================
void
IPSA::Perform_constant_prop(DNA_NODE *dna, BOOL hva) {
  IPSA_CPROP cprop(this, hva);
  cprop.Do_prop(dna);
}

// ==================================================================
// IPSA::Perform_side_effect_prop
//
// Perform inter-procedure side effect propagation
// ==================================================================
void
IPSA::Perform_side_effect_prop(DNA_NODE *dna, BOOL hva) {
  IPSA_SPROP sprop(this, hva);
  sprop.Do_prop(dna);
}
