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
#include "vsa_defs.h"
#include "opt_bb.h"
#include "bb_node_set.h"
#include "opt_main.h"
#include "opt_htable.h"
#include "opt_vsa.h"
#include "opt_vsa_du.h"
#include "opt_cr_util.h"
#include <stack>
#include <vector>
#include <ext/hash_map>

// ==================================================================
// DNA_DU_BUILDER
// ==================================================================

// DNA_DU_BUILDER
// traverse IR to build the D-U info
class DNA_DU_BUILDER {
  // DU_STACK, element is the DU index
  typedef std::stack<UINT32>    DU_STACK;
  // DU_STACK_VEC for all var or vor
  typedef std::vector<DU_STACK> DU_STACK_VEC;
  // USE_MAP: key is var or vor ID, value is combine of CODEREP* or
  // VSYM_OBJ_REP* (high 61-bit) and USE_KIND (low 3-bit)
  typedef __gnu_cxx::hash_map<UINT32, UINT64> USE_MAP;
  // only add phi use once like v2 = phi(v0, v1, v1)
  typedef __gnu_cxx::hash_set<uint64_t> PHI_SET;

private:
  DU_INFO_CONTAINER &_du_info;        // where the D-U is stored
  COMP_UNIT         *_comp_unit;      // comp_unit
  DU_STACK_VEC       _var_stack;      // rename stack for all var
  DU_STACK_VEC       _vor_stack;      // rename stack for all vora
  PHI_SET            _phi_set;        // only add phi use once
  UINT32             _first_vo;       // first vo ID
  BOOL               _trace;          // is trace on

private:
  // allocate a new entry in container
  std::pair<UINT32, DU_INFO*> New_entry() {
    UINT32 idx = _du_info.Allocate(sizeof(DU_INFO));
    Is_True(idx != NULL_ID, ("bad idx"));
    return std::make_pair(idx, _du_info.Get_ptr<DU_INFO>(idx));
  }

  // get the DU_INFO pointer from index
  DU_INFO *Get_entry(UINT32 idx) {
    Is_True(idx != NULL_ID, ("bad idx"));
    return _du_info.Get_ptr<DU_INFO>(idx);
  }

  // get ID from CODEREP
  UINT32 Id(CODEREP *cr) const {
    Is_True(cr->Kind() == CK_VAR, ("bad kind"));
    Is_True(cr->Aux_id() >= 0 && cr->Aux_id() < _var_stack.size(),
            ("bad aux id"));
    return cr->Aux_id();
  }

  // get ID from VSYM_OBJ_REP
  UINT32 Id(VSYM_OBJ_REP *vor) const {
    UINT32 id = vor->Vsym_obj()->Id() - _first_vo;
    Is_True(id >= 0 && id < _vor_stack.size(),
            ("bad vor id"));
    return id;
  }

  // get stack for CODEREP
  DU_STACK &Stack(CODEREP *cr) {
    return _var_stack[Id(cr)];
  }

  // get stack for VSYM_OBJ_REP
  DU_STACK &Stack(VSYM_OBJ_REP *vor) {
    return _vor_stack[Id(vor)];
  }

  // check if phi use of `id' has been added
  BOOL   Has_added(UINT32 phi, UINT32 id) {
    uint64_t key = ((UINT64)phi << 32) | id;
    if (_phi_set.find(key) != _phi_set.end())
      return TRUE;
    _phi_set.insert(key);
    return FALSE;
  }

  // get var/vor from CODEREP
  void   Analyze_cr(CODEREP *cr, USE_MAP& var_map, USE_MAP& vor_map) const;

  // get name of CODEREP
  const char *Name(CODEREP *cr, char *buf, UINT len) const {
    snprintf(buf, len, "sym%dv%d cr%d",
             cr->Aux_id(), cr->Version(), cr->Coderep_id());
    return buf;
  }

  const char *Name(VSYM_OBJ_REP *vor, char *buf, UINT len) const {
    snprintf(buf, len, "vor%dv%d",
             vor->Vsym_obj()->Id(), vor->Version());
    return buf;
  }

private:
  // add use in sr to D-U chain and push to stack
  void Append_use(BB_NODE *bb, UINT32 top, UINT32 idx, DU_INFO *info) {
    Is_True(Get_entry(idx) == info, ("idx info mismatch"));
    // if top is NULL, do only initialize the fields
    if (top == NULL_ID) {
      info->Set_next(NULL_ID);
      info->Set_child(NULL_ID);
      return;
    }

    // append to existing chain
    DU_INFO *prev = Get_entry(top);
    Is_True(prev && !prev->Is_null(), ("prev is null"));
    // force adding to child chain is prev is USE_BY_PHI?
    //Is_True(prev->Kind() != USE_BY_PHI, ("used by phi"));
    BB_NODE *prev_bb = prev->Bb();
    if (bb->Postdominates(prev_bb)) {
      // no control dep from prev to bb, append to next chain
      info->Set_child(NULL_ID);
      info->Set_next(prev->Next());
      prev->Set_next(idx);
    }
    else {
      // has control dep from prev to bb, append to child chain
      info->Set_next(NULL_ID);
      info->Set_child(prev->Child());
      prev->Set_child(idx);
    }

    Is_Trace(_trace,
             (TFile, " -- BB%d %s BB%d. set %d[+%d,-%d] %s of %d[+%d,-%d]\n",
              bb->Id(),
              bb->Postdominates(prev_bb) ? "pdom" : "not pdom",
              prev_bb->Id(),
              idx, info->Child(), info->Next(),
              bb->Postdominates(prev_bb) ? "next" : "child",
              top, prev->Child(), prev->Next()));
  }

  // add use for STMTREP
  UINT32 Add_use(STMTREP *sr, UINT32 prev, USE_KIND kind) {
    // create DU_INFO node
    std::pair<UINT32, DU_INFO*> entry = New_entry();
    entry.second->Set_use(sr, kind);
    Append_use(sr->Bb(), prev, entry.first, entry.second);
    return entry.first;
  }

  // add use for var (CODEREP)
  template<typename _N>
  void Add_use(STMTREP *sr, _N *v, USE_KIND kind) {
    DU_STACK &stack = Stack(v);

    UINT32 aux = Id(v);
    char name_buf[32];
    Is_Trace(_trace,
             (TFile, " -- add sr%d %s use for %s. stack top: %d.\n",
              sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4,
              Name(v, name_buf, sizeof(name_buf)),
              stack.empty() ? -1 : stack.top()));

    UINT32 prev = stack.empty() ? v->Use_chain() : stack.top();
    UINT32 id = Add_use(sr, prev, kind);
    stack.push(id);
    Is_Trace(_trace,
             (TFile, " -- push %d to stack[%d]\n", stack.top(), aux));

    if (v->Use_chain() == NULL_ID) {
      v->Set_use_chain(id);
      Is_Trace(_trace,
               (TFile, "++ set %s du=%d.\n",
                Name(v, name_buf, sizeof(name_buf)), id));
    }
  }

  // add use for PHI_NODE
  UINT32 Add_use(BB_NODE *bb, UINT32 prev, PHI_NODE *phi) {
    // create DU_INFO node
    std::pair<UINT32, DU_INFO*> entry = New_entry();
    entry.second->Set_use(phi);
    Append_use(bb, prev, entry.first, entry.second);
    return entry.first;
  }

  // add use for var PHI_NODE
  template<typename _N>
  void Add_use(PHI_NODE *phi, _N *v, BB_NODE *bb) {
    AUX_ID aux = Id(v);
    char name_buf[32];
    // check if phi use already added
    if (Has_added(phi->Bb()->Id(), aux) == TRUE) {
      Is_Trace(_trace,
               (TFile, " -- ignore phi in BB%d for %s. already added.\n",
                phi->Bb()->Id(), Name(v, name_buf, sizeof(name_buf))));
      return;
    }
    DU_STACK &stack = Stack(v);

    Is_Trace(_trace,
             (TFile, " -- add phi in BB%d for %s. stack top: %d.\n",
              phi->Bb()->Id(),
              Name(v, name_buf, sizeof(name_buf)),
              stack.empty() ? -1 : stack.top()));

    UINT32 prev = stack.empty() ? v->Use_chain() : stack.top();
    UINT32 id = Add_use(bb, prev, phi);
    // don't push phi use to stack because it's out of dom scope
    if (v->Use_chain() == NULL_ID) {
      v->Set_use_chain(id);
      Is_Trace(_trace,
               (TFile, "++ set %s du=%d.\n",
                Name(v, name_buf, sizeof(name_buf)), id));
    }
  }

private:
  // dump var/vor used in CODEREP
  template<typename _N>
  void Dump_use_map(const USE_MAP& map, const char *prefix, FILE *fp) const {
    BOOL need_comma = FALSE;
    USE_MAP::const_iterator end = map.end();
    for (USE_MAP::const_iterator it = map.begin();
         it != end; ++it) {
      // output a comma to seperate items
      if (need_comma)
        fprintf(fp, ", ");
      else
        need_comma = TRUE;

      // output var/vor
      _N *val = (_N *)(it->second & USE_PTR_MASK);
      USE_KIND kind = (USE_KIND)(it->second & USE_KIND_MASK);
      fprintf(fp, "%s%dv%d:%s%s%s%s",
              prefix, Id(val), val->Version(),
              (kind == USE_BY_PHI) ? "phi"  : "",
              (kind & USE_BY_STMT) ? "stmt" : "",
              (kind & USE_BY_CHI)  ? "chi"  : "",
              (kind & USE_BY_MU)   ? "mu"   : "");
    }
    fprintf(fp, "\n");
  }

public:
  // initialize DNA_DU_BUILDER
  void Initialize();
  // finalize DNA_DU_BUILDER
  void Finalize();
  // traverse stmtrep
  template<bool fwd>
  void Traverse_stmt(STMTREP *sr);
  // traverse succ's phi list
  void Traverse_succ_phi(BB_NODE *bb);
  // traverse basic block
  void Traverse(BB_NODE *bb);

public:
  // constructor
  DNA_DU_BUILDER(DU_INFO_CONTAINER &du, COMP_UNIT *cu)
    : _du_info(du), _comp_unit(cu), _first_vo(0) {
    _trace = Get_Trace(TP_VSA, VSA_DU_TRACE_FLAG);
  }

  // main entry to build D-U info
  void Build();

};  // DNA_DU_BUILDER

// DNA_DU_BUILDER::Analyze_cr
// analyze cr and put all var and vor into map
void
DNA_DU_BUILDER::Analyze_cr(CODEREP *cr, USE_MAP& var_map, USE_MAP& vor_map) const
{
  VSYM_OBJ_REP *vor;
  CODEREP *base;
  switch(cr->Kind()) {
  case CK_LDA:
  case CK_CONST:
  case CK_RCONST:
    break;  // do nothing
  case CK_VAR:
    Is_True(((UINT64)cr & USE_KIND_MASK) == 0, ("cr is not aligned"));
    if (!cr->Is_var_volatile())
      var_map.insert(std::make_pair(Id(cr), (UINT64)cr | USE_BY_STMT));
    break;
  case CK_IVAR:
    if (cr->Opr() == OPR_PARM) {
      Analyze_cr(cr->Ilod_base(), var_map, vor_map);
      return;
    }
    vor = _comp_unit->Vsa()->Cr_2_vor(cr);
    if (vor) {
      Is_True(((UINT64)vor & USE_KIND_MASK) == 0, ("vor is not aligned"));
      vor_map.insert(std::make_pair(Id(vor), (UINT64)vor | USE_BY_STMT));
    }
    base = cr->Ilod_base() ? cr->Ilod_base() : cr->Istr_base();
    Analyze_cr(base, var_map, vor_map);
    if (cr->Opr() == OPR_MLOAD) {
      Analyze_cr(cr->Mstore_size() ? cr->Mstore_size() : cr->Mload_size(),
                 var_map, vor_map);
    }
    else if (cr->Opr() == OPR_ILOADX) {
      Analyze_cr(cr->Index(), var_map, vor_map);
    }
    break;
  case CK_OP:
    for (INT i = 0; i < cr->Kid_count(); ++i) {
      Analyze_cr(cr->Opnd(i), var_map, vor_map);
    }
    break;
  default:
    Is_True(FALSE, ("Unknown cr kind %d", cr->Kind()));
  }
}

// DNA_DU_BUILDER::Initialize
// initialize DNA_DU_BUILDER
void
DNA_DU_BUILDER::Initialize()
{
  // initialize stack for variable
  _var_stack.resize(_comp_unit->Opt_stab()->Lastidx() + 1);
  // initialize stack for vsym
  UINT32 vo_count = _comp_unit->Vsa()->Count_vo_list();
  if (vo_count > 0) {
    _vor_stack.resize(_comp_unit->Vsa()->Count_vo_list());
    _first_vo = _comp_unit->Vsa()->Vsym_obj_list()->Head()->Id();
  }
}

// DNA_DU_BUILDER::Finalize
// finalize DNA_DU_BUILDER
void
DNA_DU_BUILDER::Finalize()
{
  // do nothing
}

// DNA_DU_BUILDER::Traverse_stmt
// Traverse STMTREP to collect D-U
template<bool fwd> void
DNA_DU_BUILDER::Traverse_stmt(STMTREP *sr)
{
  USE_MAP var_map;    // map for variable
  USE_MAP vor_map;    // map for vsym

  OPERATOR opr = sr->Opr();
  if (opr == OPR_INTRINSIC_CALL) {
    // TODO: handle assume
  }

  // analyze rhs
  if (sr->Rhs())
    Analyze_cr(sr->Rhs(), var_map, vor_map);

  // analyze rhs
  if (opr != OPR_STID && opr != OPR_STBITS && sr->Lhs())
    Analyze_cr(sr->Lhs(), var_map, vor_map);

  AUX_ID def_vsym = _comp_unit->Opt_stab()->Default_vsym();
  AUX_ID ret_vsym = _comp_unit->Opt_stab()->Return_vsym();
  // analyze var mu/chi
  {
    // analyze var mu
    MU_NODE *mnode;
    MU_LIST_ITER mu_iter;
    FOR_ALL_NODE(mnode, mu_iter, Init(sr->Mu_list())) {
      if (mnode->Aux_id() != def_vsym &&
          mnode->Aux_id() != ret_vsym &&
          !mnode->OPND()->Is_flag_set(CF_IS_ZERO_VERSION) &&
          !mnode->OPND()->Is_var_volatile()) {
        Is_True(((UINT64)mnode->OPND() & USE_KIND_MASK) == 0,
                ("cr is not aligned"));
        UINT64 &val = var_map[mnode->Aux_id()];
        val = (val == 0) ? ((UINT64)mnode->OPND() | USE_BY_MU)
                         : (val | USE_BY_MU);
      }
    }

    // analyze var chi
    CHI_NODE *cnode;
    CHI_LIST_ITER chi_iter;
    FOR_ALL_NODE(cnode, chi_iter, Init(sr->Chi_list())) {
      if (cnode->Live() &&
          cnode->Aux_id() != def_vsym &&
          cnode->Aux_id() != ret_vsym &&
          !cnode->OPND()->Is_flag_set(CF_IS_ZERO_VERSION) &&
          !cnode->OPND()->Is_var_volatile()) {
        Is_True(((UINT64)cnode->OPND() & USE_KIND_MASK) == 0,
                ("cr is not aligned"));
        UINT64 &val = var_map[cnode->Aux_id()];
        val = (val == 0) ? ((UINT64)cnode->OPND() | USE_BY_MU)
                         : (val | USE_BY_MU);
      }
    }
  }

  // analyze vor mu/chi
  VSA *vsa = _comp_unit->Vsa();
  VSYM_OBJ_REP *null_vor = vsa->Null_vor();
  {
    // analyze vor mu
    MU_NODE *mnode;
    MU_LIST_ITER mu_iter;
    FOR_ALL_NODE(mnode, mu_iter, Init(vsa->Stmt_vor_mu(sr))) {
      CVOR *cvor = (CVOR*)mnode->OPND();
      if (cvor->first != null_vor) {
        Is_True(((UINT64)cvor->first & USE_KIND_MASK) == 0,
               ("vor is not aligned"));
        UINT64 &val = vor_map[Id(cvor->first)];
        val = (val == 0) ? ((UINT64)cvor->first | USE_BY_MU)
                         : (val | USE_BY_MU);
      }
    }

    // analyze vor chi
    CHI_NODE *cnode;
    CHI_LIST_ITER chi_iter;
    FOR_ALL_NODE(cnode, chi_iter, Init(vsa->Stmt_vor_chi(sr))) {
      CVOR* cvor = (CVOR*)cnode->OPND();
      if (cvor->first != null_vor) {
        Is_True(((UINT64)cvor->first & USE_KIND_MASK) == 0,
               ("vor is not aligned"));
        UINT64 &val = vor_map[Id(cvor->first)];
        val = (val == 0) ? ((UINT64)cvor->first | USE_BY_CHI)
                         : (val | USE_BY_CHI);
      }
    }
  }

  Is_Trace(_trace,
           (TFile, " == analyze sr%d:\n", sr->Stmtrep_id()));
  Is_Trace_cmd(_trace, vsa->Print_sr(sr, TFile));
  Is_Trace(_trace,
           (TFile, "  + vars used: "));
  Is_Trace_cmd(_trace, Dump_use_map<CODEREP>(var_map, "sym", TFile));
  Is_Trace(_trace,
           (TFile, "  + vors used: "));
  Is_Trace_cmd(_trace, Dump_use_map<VSYM_OBJ_REP>(vor_map, "vor", TFile));

  // traverse USE_MAP to create D-U
  {
    // traverse var map to add var's D-U
    USE_MAP::iterator end = var_map.end();
    for (USE_MAP::iterator it = var_map.begin();
         it != end; ++it) {
      if (fwd) {
        CODEREP *var = (CODEREP*)(it->second & USE_PTR_MASK);
        USE_KIND kind = (USE_KIND)(it->second & USE_KIND_MASK);
        Add_use(sr, var, kind);
      }
      else {
        DU_STACK &stack = _var_stack[it->first];
        Is_True(!stack.empty(), ("var stack is empty"));
        const DU_INFO *info = Get_entry(stack.top());
        Is_True(info && info->Kind() != USE_BY_PHI && info->Get_stmt() == sr,
                ("var stack stmt mismatch"));

        Is_Trace(_trace,
                 (TFile, " -- pop %d from var stack[%d]\n", stack.top(), it->first));
        stack.pop();
      }
    }
  }

  {
    // traverse vor map to add vor's D-U
    USE_MAP::iterator end = vor_map.end();
    for (USE_MAP::iterator it = vor_map.begin();
         it != end; ++it) {
      if (fwd) {
        VSYM_OBJ_REP *vor = (VSYM_OBJ_REP*)(it->second & USE_PTR_MASK);
        USE_KIND kind = (USE_KIND)(it->second & USE_KIND_MASK);
        Add_use(sr, vor, kind);
      }
      else {
        DU_STACK &stack = _vor_stack[it->first];
        Is_True(!stack.empty(), ("vor stack is empty"));
        const DU_INFO *info = Get_entry(stack.top());
        Is_True(info && info->Kind() != USE_BY_PHI && info->Get_stmt() == sr,
                ("vor stack stmt mismatch"));

        Is_Trace(_trace,
                 (TFile, " -- pop %d from vor stack[%d]\n", stack.top(), it->first));
        stack.pop();
      }
    }
  }
}

// DNA_DU_BUILDER::Traverse_succ_phi
// Traver bb's successors' phi list and append phi opnd to use chain
void
DNA_DU_BUILDER::Traverse_succ_phi(BB_NODE *bb)
{
  BB_NODE *succ;
  BB_LIST_ITER bb_iter;
  VSA *vsa = _comp_unit->Vsa();
  FOR_ALL_ELEM (succ, bb_iter, Init(bb->Succ())) {
    Is_Trace(_trace,
             (TFile, " == analyze successor BB%d:\n", succ->Id()));

    PHI_NODE *phi;
    PHI_LIST_ITER phi_iter;
    INT32 pos = succ->Pred()->Pos(bb);
    // var phi list
    FOR_ALL_ELEM (phi, phi_iter, Init(succ->Phi_list())) {
      if (phi->Live()) {
        Add_use(phi, phi->OPND(pos), bb);
      }
    }

    // vor phi list
    FOR_ALL_NODE (phi, phi_iter, Init(vsa->Bb_vo_philist(succ))) {
      VSYM_OBJ_REP *vor = (VSYM_OBJ_REP *)phi->OPND(pos);
      Add_use(phi, vor, bb);
    }
  }
}

// DNA_DU_BUILDER::Traverse
// Traverse BB to collect D-U. Push current use to stack in forward pass and
// pop use from stack in backword pass to set the hierarchy between uses
void
DNA_DU_BUILDER::Traverse(BB_NODE *bb)
{
  // no need to process phi
  Is_Trace(_trace,
           (TFile, "== analyze BB%d:\n", bb->Id()));

  // forward processing stmt
  STMTREP_ITER fwd_iter(bb->Stmtlist());
  STMTREP *sr;
  FOR_ALL_NODE(sr, fwd_iter, Init()) {
    Traverse_stmt<true>(sr);
  }

  // process phi opnd used in successor
  Traverse_succ_phi(bb);

  // process dom bb
  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb->Dom_bbs())) {
    Traverse(dom_bb);
  }

  // backward processing stmt
  STMTREP_ITER rev_iter(bb->Stmtlist());
  FOR_ALL_NODE_REVERSE(sr, rev_iter, Init()) {
    Traverse_stmt<false>(sr);
  }
}

// DNA_DU_BUILDER::Build
// driver to build D-U
void
DNA_DU_BUILDER::Build()
{
  Is_True(_du_info.Empty(), ("du_info not empty"));
  Is_True(_comp_unit && _comp_unit->Vsa() && _comp_unit->Dna(),
          ("bad comp_unit"));

  // initialize
  Initialize();

  // traverse dom tree
  Traverse(_comp_unit->Cfg()->Entry_bb());

#ifdef Is_True_On
  // verify the var/vor stack
  for (DU_STACK_VEC::const_iterator it = _var_stack.begin();
       it != _var_stack.end(); ++it) {
    Is_True(it->empty(), ("stack not empty"));
  }
  for (DU_STACK_VEC::const_iterator it = _vor_stack.begin();
       it != _vor_stack.end(); ++it) {
    Is_True(it->empty(), ("stack not empty"));
  }
#endif

  // finalize
  Finalize();
}

// ==================================================================
// DU_INFO
// ==================================================================

// DU_INFO::Dump_use
// dump use info without next/child ID
void
DU_INFO::Dump_use(FILE* fp) const
{
  if (Is_null()) {
    fprintf(fp, "-nil-\n");
    return;
  }

  if (Kind() == USE_BY_PHI) {
    fprintf(fp, "use by phi %p in bb%d\n", Get_phi(), Get_phi()->Bb()->Id());
  }
  else if (Kind() <= USE_KIND_MAX) {
    BOOL in_stmt = (Kind() & USE_BY_STMT) == USE_BY_STMT;
    BOOL in_chi = (Kind() & USE_BY_CHI) == USE_BY_CHI;
    BOOL in_mu = (Kind() & USE_BY_MU) == USE_BY_MU;
    fprintf(fp, "use by%s%s%s %p sr%d\n",
            in_stmt ? " stmt" : "",
            in_chi ? " chi" : "",
            in_mu ? " mu" : "",
            Get_stmt(), Get_stmt()->Stmtrep_id());
  }
  else {
    fprintf(fp, "-err%d-\n", Kind());
  }
}

// DU_INFO::Dump
// dump use info include next/child ID
void
DU_INFO::Dump(FILE* fp) const
{
  if (Is_null()) {
    fprintf(fp, "-nil-\n");
    return;
  }

  Dump_use(fp);
  fprintf(fp, " ->%d +>%d\n", Next(), Child());
}

// ==================================================================
// DNA_DU_INFO
// ==================================================================

// DNA_DU_INFO::Build
// build the D-U info
void
DNA_DU_INFO::Build()
{
  Is_True(_du_info.Empty(), ("D-U already built"));
  Is_Trace(Get_Trace(TP_VSA, VSA_DU_TRACE_FLAG),
           (TFile, "++++ Build D-U for %s\n", _comp_unit->Dna()->Fname()));

  DNA_DU_BUILDER bldr(_du_info, _comp_unit);
  bldr.Build();

  Is_Trace(Get_Trace(TP_VSA, VSA_DU_DUMP_FLAG),
           (TFile, "++++ D-U dump for %s:\n", _comp_unit->Dna()->Fname()));
  Is_Trace_cmd(Get_Trace(TP_VSA, VSA_DU_DUMP_FLAG), Dump(TFile));
}

// DNA_DU_INFO::Dump
// dump single D-U entry
void
DNA_DU_INFO::Dump(FILE *fp, UINT32 du, UINT32 level) const
{
  const DU_INFO* info = Get_use(du);
  if (info == NULL) {
    fprintf(fp, "-nil-\n");
    return;
  }

  UINT32 i;
  for (i = 0; i < level; ++i) {
    fprintf(fp, " |");
  }
  fprintf(fp, " +%d[c:%d,n:%d] ", du, info->Child(), info->Next());

  info->Dump_use(fp);

  UINT32 child = info->Child();
  if (child != NULL_ID)
    Dump(fp, child, level + 1);

  UINT32 next = info->Next();
  if (next != NULL_ID)
    Dump(fp, next, level);
}

// DNA_DU_INFO::Dump
// dump all D-U entry
void
DNA_DU_INFO::Dump(FILE *fp) const
{
  AUX_ID i;
  AUX_STAB_ITER aux_stab_iter(_comp_unit->Opt_stab());
  FOR_ALL_NODE(i, aux_stab_iter, Init()) {
    AUX_STAB_ENTRY *aux = _comp_unit->Opt_stab()->Aux_stab_entry(i);
    if (i == _comp_unit->Opt_stab()->Default_vsym()) {
      fprintf(fp, "==== sym%d: default vsym\n", i);
      continue;
    }
    if (i == _comp_unit->Opt_stab()->Return_vsym()) {
      fprintf(fp, "==== sym%d: return vsym\n", i);
      continue;
    }
      
    fprintf(fp, "==== sym%d: %s\n",
            i, aux->St() == NULL ? "-nil-" : ST_name(aux->St()));
    CODEREP *cr;
    CODEREP_ITER cr_iter;
    FOR_ALL_NODE(cr, cr_iter, Init(aux->Cr_list())) {
      if (cr->Is_flag_set(CF_IS_ZERO_VERSION)) {
        fprintf(fp, " cr%d v%d: zero version. du=%d\n",
                cr->Coderep_id(), cr->Version(), cr->Use_chain());
        continue;
      }
      if (cr->Is_var_volatile()) {
        fprintf(fp, " cr%d v%d: volatile. du=%d\n",
                cr->Coderep_id(), cr->Version(), cr->Use_chain());
        continue;
      }
      const char* def_by = cr->Is_flag_set(CF_DEF_BY_PHI)
                             ? "phi in BB"
                             : (cr->Is_flag_set(CF_DEF_BY_CHI)
                                  ? "chi sr"
                                  : "stmt sr");
      UINT id = cr->Is_flag_set(CF_DEF_BY_PHI)
                             ? cr->Defphi()->Bb()->Id()
                             : (cr->Defstmt()
                                  ? cr->Defstmt()->Stmtrep_id()
                                  : -1);
                             
      fprintf(fp, " cr%d v%d: def by %s%d\n",
              cr->Coderep_id(), cr->Version(), def_by, id);
      if (cr->Use_chain())
        Dump(fp, cr->Use_chain(), 1);
      else
        fprintf(fp, " +no use\n");
    }
  }

  // TODO: dump vor
}

// ==================================================================
// IPSA_DU_MANAGER
// ==================================================================

// IPSA_DU_MANAGER::_du_mgr
// unique instance of the IPSA_DU_MANAGER
IPSA_DU_MANAGER IPSA_DU_MANAGER::_du_mgr;

// IPSA_DU_MANAGER::Check_watermark
// check memory used by D-U and do GC if it exceeds high watermark
// then do GC to low watermark
void
IPSA_DU_MANAGER::Check_watermark()
{
  Is_True(_head != NULL && _head->_prev != NULL, ("bad head"));
  Is_True(Total_size() < VSA_Du_Mem_Low * VSA_DU_MEM_UNIT,
          ("lower than low watermark"));

  // start from _head's prev (circular list)
  DNA_DU_INFO *last = _head->_prev;
  DNA_DU_INFO *item = last;
  UINT64 cur_freed = 0;
  UINT64 to_free = Total_size() - VSA_Du_Mem_Low * VSA_DU_MEM_UNIT;
  while (item != NULL && !item->Is_locked() &&
         cur_freed < to_free) {
    cur_freed += item->Total_size();
    DNA_DU_INFO *temp = item;
    item = item->_prev;
    // remove from map
    _map.erase(temp->Dna_idx());
    // for tracing
    const char* fname = temp->Fname();
    UINT64 size = temp->Total_size();
    BOOL is_head = (temp == _head);
    // delete D-U object
    delete temp;
    Is_Trace(Get_Trace(TP_VSA, VSA_DU_MGR_FLAG),
             (TFile, "----DU MGR free DU %s %lld %lld\n",
              fname, size, Total_size()));
    if (is_head) {
      _head = NULL;
      break;
    }
  }
  // connect item with head if head isn't removed
  if (_head) {
    _head->_prev = item;
    item->_next = _head;
  }

  Is_Trace_cmd(Get_Trace(TP_VSA, VSA_DU_MGR_FLAG), Dump(TFile));
  Is_True(_head == NULL || cur_freed >= to_free,
          ("Error: failed to free D-U info, too many locked?"));
}

// IPSA_DU_MANAGER::Dump
// dump all DNA D-U instances managed by the manager
void
IPSA_DU_MANAGER::Dump(FILE *fp) const
{
  fprintf(fp, "++++ DU MGR dump:\n");
  if (_head == NULL) {
    fprintf(fp, " + head is null. map %ld total=%lld\n",
            _map.size(), Total_size());
    return;
  }
  DNA_DU_INFO *ptr = _head;
  UINT32 count = 0;
  UINT64 total_size = 0;
  while (ptr != NULL) {
    ++ count;
    total_size += ptr->Total_size();
    DNA_DU_MAP::const_iterator it = _map.find(ptr->Dna_idx());
    fprintf(fp, " + DU for %s %lld. map[%d] %s\n",
            ptr->Fname(), ptr->Total_size(), ptr->Dna_idx(),
            (it != _map.end() && it->second == ptr) ? "match" : "mismatch");
    ptr = ptr->_next;
    if (ptr == _head)
      break;
  }
  fprintf(fp, "++ count=%d. total=%lld. map %ld. total %lld.\n",
          count, total_size, _map.size(), Total_size());
}

