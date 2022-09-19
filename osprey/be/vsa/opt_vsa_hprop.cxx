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
//
// Module: opt_vsa_hprop.cpp
//
// ====================================================================
//

#include "opt_vsa_hprop.h"
#include "cxx_memory.h"
#include "opt_vsa.h"
#include "vsa_annot.h"

// ====================================================================
//
// HOA_VALUE::Print
//
// ====================================================================
void
HOA_VALUE::Print(INT ident, FILE *fp) const
{
  fprintf(fp, "%*sval=%lx, fld=%d size=%d parm=%d fld=%d malloced=%c freed=%c\n",
              ident, "",
              Value(),
              Hoa_field_id(),
              Hoa_const_size(),
              Hoa_size_param_id(),
              Hoa_size_field_id(),
              Hoa_malloced() ? 'Y' : 'N',
              Hoa_freed() ? 'Y' : 'N');
}

void
HOA_VALUE::Print() const
{
  Print(0, stdout);
}

// ====================================================================
//
// HOA_HANDLE::Print
//
// ====================================================================
void
HOA_HANDLE::Print(INT ident, FILE *fp) const
{
  fprintf(fp, "handle: 0x%lx\n", Value());
  for (UINT i = 0; i < Hoa_count(); ++i) {
    HOA_VALUE val = Hoa_value(i);
    val.Print(ident + 1, fp);
  }
}

void
HOA_HANDLE::Print() const
{
  Print(0, stdout);
}

// ====================================================================
//
// HOA_BUILDER
// build HOA_VALUE/HOA_STC_ARRAY/HOA_DYN_ARRAY during
// propagation heap_obj annotation
//
// ====================================================================
class HOA_BUILDER {
private:
  // hash function for HEAP_OBJ_REP* and VSYM_OBJ_REP*
  template<typename _T>
  struct HASHER {
    size_t operator() (const _T* ptr) const { return (size_t)ptr; }
  };
  // HEAP_OBJ_REP -> FLD_ID map
  typedef pair<HEAP_OBJ_REP*, UINT> HOR_FLD_PAIR;
  typedef hash_map<HEAP_OBJ_REP*, UINT,
                   HASHER<HEAP_OBJ_REP>,
                   std::equal_to<HEAP_OBJ_REP*>,
                   mempool_allocator<HOR_FLD_PAIR> > HOR_FLD_MAP;
  // VSYM_OBJ_REP -> FLD_ID map
  typedef pair<VSYM_OBJ_REP*, UINT> VOR_FLD_PAIR;
  typedef hash_map<VSYM_OBJ_REP*, UINT,
                   HASHER<VSYM_OBJ_REP>,
                   std::equal_to<VSYM_OBJ_REP*>,
                   mempool_allocator<VOR_FLD_PAIR> > VOR_FLD_MAP;
  // HOA_VALUE vector
  typedef mempool_allocator<HOA_VALUE> HOA_VALUE_ALLOCATOR;
  typedef std::vector<HOA_VALUE, HOA_VALUE_ALLOCATOR> HOA_VALUE_VEC;

  // <HO, STAT> map
  typedef pair<UINT32, V_ANNOT> HO_ANNOT_PAIR;
  typedef hash_map<UINT32, V_ANNOT,
                   __gnu_cxx::hash<UINT32>,
                   std::equal_to<UINT32>,
                   mempool_allocator<HO_ANNOT_PAIR> > HO_ANNOT_MAP;

  // <VO, VOR> map
  typedef pair<UINT32, VSYM_OBJ_REP*> VO_VOR_PAIR;
  typedef hash_map<UINT32, VSYM_OBJ_REP*,
                   __gnu_cxx::hash<UINT32>,
                   std::equal_to<UINT32>,
                   mempool_allocator<VO_VOR_PAIR> > VO_VOR_MAP;

  HOA_VALUE_VEC *_vec;     // save HOA_VALUE in vector
  HOA_PROP      *_prop;    // heap_obj annotation propagator
  BOOL           _trace;

  // add new hoa into vector
  void Merge_value(HOA_VALUE val) {
    UINT ho = val.Hoa_field_id();
    HOA_VALUE_VEC::iterator end = _vec->end();
    for (HOA_VALUE_VEC::iterator it = _vec->begin();
         it != end; ++it) {
      if (it->Hoa_field_id() == ho) {
         *it = it->Merge(val);
         return;
      }
    }
    _vec->push_back(val);
  }

  // check if var is defined by value
  BOOL Is_cr_def_by(CODEREP *var, CODEREP *value);

  // get heap_obj state from hor
  V_ANNOT Hor_stat(HEAP_OBJ_REP *hor, const HO_ANNOT_MAP *map) const;

  // find vo/hor based on hor
  void Find_sub_vo_hor(HEAP_OBJ_REP *hor, STMTREP *sr,
                       VOR_FLD_MAP *vo_map,
                       HOR_FLD_MAP *ho_map);

  // find heap_obj annotation from input hor
  BOOL Find_sub_heap_obj_annot(UINT idx, HEAP_OBJ_REP *hor, STMTREP *sr,
                               const VNODE_VECTOR *vec,
                               const VOR_FLD_MAP *vo_map,
                               BOOL formal);

  // build <vo, vor> map on stmt
  template<bool chi>
  void Build_vo_map(VSA *vsa, STMTREP *sr, HEAP_OBJ_REP *base, VO_VOR_MAP &vo_map);

  // search vsym based on ho and calculate sub ho annotation
  void Find_local_sub_ho_annot(HEAP_OBJ_REP *hor, VSA *vsa,
                               const VO_VOR_MAP &vo_map, const HO_ANNOT_MAP &ho_map);

public:
  // constructor
  HOA_BUILDER(HOA_PROP *prop) : _vec(NULL), _prop(prop) {
    _vec = CXX_NEW(HOA_VALUE_VEC(HOA_VALUE_ALLOCATOR(prop->Loc_pool())),
                   prop->Loc_pool());
    Is_True(_vec != NULL, ("out of memory?"));
    _trace = Get_Trace(TP_VSA, VSA_HPROP_TRACE_FLAG);
  }

  // return hoa count
  UINT Heap_obj_annot_count() const {
    return _vec->size();
  }

  // merge existing heap_obj annotation
  void Merge_heap_obj_annot(HO_ANNOT hoa);

  // find heap_obj annotation from input hor
  BOOL Find_heap_obj_annot(UINT idx, HEAP_OBJ_REP *hor, STMTREP *sr,
                           const VNODE_VECTOR *vec,
                           BOOL formal);

  // calculate local ho annotation for function
  void Calculate_local_ho_annot(DNA_NODE *dna);

  // export value to memory allocated from global pool and return the rep
  HO_ANNOT Export_heap_obj_annot() const;

}; // HOA_BUILDER


// HOA_BUILDER::Is_cr_def_by
// check if var is defined by value
BOOL
HOA_BUILDER::Is_cr_def_by(CODEREP *var, CODEREP *value)
{
  if (var->Kind() == CK_LDA ||
      var->Kind() == CK_RCONST) {
    return FALSE;
  }

  // const
  if (var->Kind() == CK_CONST) {
    // assume value is propagated with constant value
    if (value->Kind() == CK_CONST &&
        var->Const_val() == value->Const_val())
      return TRUE;
    return FALSE;
  }

  // var
  if (var->Kind() == CK_VAR) {
    if (var->Is_flag_set(CF_DEF_BY_PHI) ||
        var->Is_flag_set(CF_DEF_BY_CHI) ||
        var->Is_flag_set(CF_IS_ZERO_VERSION) ||
        var->Is_var_volatile()) {
      return FALSE;
    }
    STMTREP *def = var->Defstmt();
    Is_True(def != NULL, ("no def for var"));
    return Is_cr_def_by(def->Rhs(), value);
  }

  // ivar
  if (var->Kind() == CK_IVAR) {
    VSYM_OBJ_REP *val_vor = _prop->Vsa()->Cr_2_vor(var);
    Is_True(val_vor != NULL, ("not find vor for IVAR"));
    if (val_vor != NULL &&
        val_vor == _prop->Vsa()->Cr_2_vor(value))
      return TRUE;
    if (val_vor != NULL &&
        (val_vor->Attr() == ROR_DEF_BY_COPY ||
         val_vor->Attr() == ROR_DEF_BY_ISTORE)) {
      STMTREP *def = val_vor->Stmt_def();
      Is_True(def != NULL, ("no def for vor"));
      return Is_cr_def_by(def->Rhs(), value);
    }
    return FALSE;
  }

  return FALSE;
}

// HOA_BUILDER::Hor_stat
// get heap_obj state from hor
V_ANNOT
HOA_BUILDER::Hor_stat(HEAP_OBJ_REP *hor, const HO_ANNOT_MAP* map) const
{
  Is_True(hor != NULL &&
          hor->Attr() != ROR_DEF_BY_NONE &&
          hor->Attr() != ROR_DEF_BY_NULL, ("invalid hor"));

  switch (hor->Attr()) {
  case ROR_DEF_BY_ALLOC:
    return VANT_UTIL::Init(ANT_MALLOC, ANT_YES);
  case ROR_DEF_BY_FREE:
    return VANT_UTIL::Init(ANT_FREE, ANT_YES);
  case ROR_DEF_BY_VARPHI:
  case ROR_DEF_BY_VORPHI:
    if (map != NULL)
      break;
    // fall through
  case ROR_DEF_BY_LDA:  // TODO: should model LDA?
  default:
    return VANT_UTIL::Empty();
  }

  HOR_LIST *ulist = hor->Ulist();
  if (ulist == NULL)
    return VANT_UTIL::Empty();

  V_ANNOT ret = VANT_UTIL::Empty();
  HOR_LIST_ITER iter;
  FOR_ALL_NODE(hor, iter, Init(ulist)) {
    Is_True(hor != NULL &&
            hor->Attr() != ROR_DEF_BY_NONE &&
            hor->Attr() != ROR_DEF_BY_NULL, ("invalid hor"));
    HO_ANNOT_MAP::const_iterator it = map->find(hor->Heap_obj()->Id());
    if (it != map->end()) {
      V_ANNOT stat = it->second;
      if (stat != VANT_UTIL::Empty()) {
        ret = VANT_UTIL::Or(ret, VANT_UTIL::Yes_to_maybe(stat));
      }
    }
  }
  return ret;
}

// HOA_BUILDER::Build_vo_map
// build <vo, vor> map on stmt using chi list
template<> void
HOA_BUILDER::Build_vo_map<true>(VSA *vsa, STMTREP *sr, HEAP_OBJ_REP *base, VO_VOR_MAP &vo_map)
{
  CHI_LIST *chi_list = vsa->Stmt_vor_chi(sr);
  if (chi_list && !chi_list->Is_Empty()) {
    CHI_NODE *cnode;
    CHI_LIST_ITER chi_iter;
    FOR_ALL_NODE(cnode, chi_iter, Init(chi_list)) {
      CVOR *cvor = (CVOR*)cnode->RESULT();
      VSYM_OBJ_REP *vor = cvor->first;
      Is_True(vor != NULL && !vsa->Is_special_vor(vor),
              ("special vor on stmt"));
      if (vor->Hor() == NULL)
        continue;
      if (base != NULL && vor->Vsym_obj()->Base_hor() != base)
        continue;
      Is_Trace(_trace,
               (TFile, "[HPROP] stmt %d %s defs vo%dv%d\n",
                       sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4,
                       vor->Vsym_obj()->Id(), vor->Version()));
      vo_map[vor->Vsym_obj()->Id()] = vor;
    }
  }
}

// HOA_BUILDER::Build_vo_map
// build <vo, vor> map on stmt using mu list
template<> void
HOA_BUILDER::Build_vo_map<false>(VSA *vsa, STMTREP *sr, HEAP_OBJ_REP *base, VO_VOR_MAP &vo_map)
{
  MU_LIST *mu_list = vsa->Stmt_vor_mu(sr);
  if (mu_list && !mu_list->Is_Empty()) {
    MU_NODE *mnode;
    MU_LIST_ITER mu_iter;
    FOR_ALL_NODE(mnode, mu_iter, Init(mu_list)) {
      CVOR *cvor = (CVOR*)mnode->OPND();
      VSYM_OBJ_REP *vor = cvor->first;
      Is_True(vor != NULL && !vsa->Is_special_vor(vor),
              ("special vor on stmt"));
      if (vor->Hor() == NULL)
        continue;
      if (base != NULL && vor->Vsym_obj()->Base_hor() != base)
        continue;
      Is_Trace(_trace,
               (TFile, "[HPROP] stmt %d %s uses vo%dv%d\n",
                       sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4,
                       vor->Vsym_obj()->Id(), vor->Version()));
      vo_map[vor->Vsym_obj()->Id()] = vor;
    }
  }
}

// HOA_BUILDER::Find_local_sub_ho_annot
// search vsym based on ho and calculate sub ho annotation
void
HOA_BUILDER::Find_local_sub_ho_annot(HEAP_OBJ_REP *hor, VSA *vsa,
                                     const VO_VOR_MAP &vo_map, const HO_ANNOT_MAP &ho_map)
{
  // search 1 level of field
  HOR_VO_LIST_ITER iter(hor);
  VSYM_OBJ *vo;
  FOR_ALL_NODE(vo, iter, Init()) {
    VSYM_FLD_REP vfr = vo->Fld_rep();
    // ignore ANY field so far
    if (!vfr.Is_uniq_id()) {
      Is_Trace(_trace,
               (TFile, "[HPROP] ignore ho%dv%d field ",
                       hor->Heap_obj()->Id(), hor->Version()));
      Is_Trace_cmd(_trace, vfr.Print(TFile));
      Is_Trace(_trace,
               (TFile, " is not uniq id\n"));
      continue;
    }
    // check vor/hor on entry
    VO_VOR_MAP::const_iterator it = vo_map.find(vo->Id());
    if (it == vo_map.end()) {
      Is_Trace(_trace,
               (TFile, "[HPROP] ignore ho%dv%d field ",
                       hor->Heap_obj()->Id(), hor->Version()));
      Is_Trace_cmd(_trace, vfr.Print(TFile));
      Is_Trace(_trace,
               (TFile, " vor not found on return\n"));
      continue;
    }
    VSYM_OBJ_REP *vor = it->second;
    Is_True(vor != NULL && !vsa->Is_special_vor(vor),
            ("not find parm vor on entry"));
    HEAP_OBJ_REP *hor = vor->Hor();
    Is_True(hor != NULL && !vsa->Is_special_hor(hor),
            ("not find parm hor on entry"));
    HO_ANNOT_MAP::const_iterator hstat = ho_map.find(hor->Heap_obj()->Id());
    V_ANNOT ant = hstat != ho_map.end() ? hstat->second : VANT_UTIL::Empty();
    Is_Trace(_trace,
             (TFile, "[HPROP] sub_ho fld%d vo%dv%d with ho%dv%d:%s%s\n",
                     vfr.Fld_id(), vo->Id(), vor->Version(),
                     hor->Heap_obj()->Id(), hor->Version(),
                     VANT_UTIL::Get(ant, ANT_MALLOC) != 0 ? " malloc" : "",
                     VANT_UTIL::Get(ant, ANT_FREE) != 0 ? " free" : ""));

    // hstat may be malloc/free, create annotation here
    if (ant != VANT_UTIL::Empty()) {
      HOA_VALUE val = HOA_VALUE::Make_value(vfr.Fld_id(),
                                            HOA_VALUE::TOP_VAL,
                                            HOA_VALUE::TOP_VAL,
                                            HOA_VALUE::TOP_VAL,
                                            VANT_UTIL::Get(ant, ANT_MALLOC) != 0,
                                            VANT_UTIL::Get(ant, ANT_FREE) != 0);
      _vec->push_back(val);
    }
  }
}

// HOA_BUILDER::Find_sub_vo_hor
// find vo/hor in sub field of hor
void
HOA_BUILDER::Find_sub_vo_hor(HEAP_OBJ_REP *hor, STMTREP *sr,
                             VOR_FLD_MAP *vo_map,
                             HOR_FLD_MAP *ho_map)
{
  HOR_VO_LIST_ITER iter(hor);
  VSYM_OBJ *fld_vo;
  VSA *vsa = _prop->Vsa();
  FOR_ALL_NODE(fld_vo, iter, Init()) {
    // ignore any
    if (fld_vo->Fld_rep().Is_any())
      continue;

    // find current version
    VSYM_OBJ_REP *fld_vor = vsa->Find_stmt_cur_vor(sr, fld_vo);
    if (fld_vor == NULL) {
      Is_Trace(_trace,
               (TFile, "[HPROP] sub_vo: failed to find cur vor for vo%d on sr%d. fld: ",
                       fld_vo->Id(), sr->Stmtrep_id()));
      Is_Trace_cmd(_trace, fld_vo->Fld_rep().Print(TFile));
      Is_Trace(_trace, (TFile, "\n"));
      continue;
    }

    // skip vor not defined by STID/ISTORE
    if (fld_vor->Attr() != ROR_DEF_BY_COPY &&
        fld_vor->Attr() != ROR_DEF_BY_ISTORE) {
      Is_Trace(_trace,
               (TFile, "[HPROP] sub_vo: vo%dv%d on sr%d is not def by istore. fld: ",
                       fld_vo->Id(), fld_vor->Version(), sr->Stmtrep_id()));
      Is_Trace_cmd(_trace, fld_vo->Fld_rep().Print(TFile));
      Is_Trace(_trace, (TFile, "\n"));
      continue;
    }

    UINT fld_id = fld_vo->Fld_rep().Fld_id();
    // add to vo_map
    vo_map->insert(std::make_pair(fld_vor, fld_id));

    // add to ho_map
    HEAP_OBJ_REP *fld_hor = fld_vor->Hor();
    if (fld_hor != NULL &&
        fld_hor->Attr() != ROR_DEF_BY_PHI &&
        fld_hor->Attr() != ROR_DEF_BY_VARPHI &&
        fld_hor->Attr() != ROR_DEF_BY_VORPHI) {
      ho_map->insert(std::make_pair(fld_hor, fld_id));
    }
  }
}

// HOA_BUILDER::Find_sub_heap_obj_annot
// find heap_obj annotation from input hor
BOOL
HOA_BUILDER::Find_sub_heap_obj_annot(UINT idx, HEAP_OBJ_REP *hor, STMTREP *sr,
                                     const VNODE_VECTOR *vec,
                                     const VOR_FLD_MAP *vo_map,
                                     BOOL formal)
{
  if (hor->Attr() == ROR_DEF_BY_PHI ||
      hor->Attr() == ROR_DEF_BY_VARPHI ||
      hor->Attr() == ROR_DEF_BY_VORPHI) {
    // no Byte_size cr foor hor defined by phi
    return FALSE;
  }

  if (hor->Attr() == ROR_DEF_BY_FREE) {
    // freed, size doesn't matter. only set freed flag.
    HOA_VALUE val = HOA_VALUE::Make_value(idx,
                                          HOA_VALUE::TOP_VAL,
                                          HOA_VALUE::TOP_VAL,
                                          HOA_VALUE::TOP_VAL,
                                          FALSE,
                                          TRUE);
    _vec->push_back(val);
    return TRUE;
  }

  CODEREP *sz_cr = hor->Heap_obj()->Byte_size();
  if (hor->Attr() == ROR_DEF_BY_LDA) {
    CODEREP *ho_cr = hor->Heap_obj()->Ho_cr();
    Is_True(ho_cr != NULL && ho_cr->Kind() == CK_LDA,
            ("ho cr is not LDA"));
    sz_cr = Alloc_stack_cr(0);
    sz_cr->Init_const(MTYPE_U8, ST_size(ho_cr->Lda_base_st()));
  }
  else {
    sz_cr = hor->Heap_obj()->Byte_size();
    if (sz_cr == NULL) {
      Is_Trace(_trace,
               (TFile, "[HPROP] hoa: failed to find ho%d size.\n",
                       hor->Heap_obj()->Id()));
      return FALSE;
    }
  }
  Is_True(sz_cr != NULL, ("no size cr for hor"));

  // ignore CVT
  while (sz_cr->Kind() == CK_OP &&
         sz_cr->Opr() == OPR_CVT)
    sz_cr = sz_cr->Opnd(0);

  if (sz_cr->Kind() == CK_OP) {
    Is_Trace(_trace,
             (TFile, "[HPROP] hoa: TODO ho%d size is CK_OP:\n",
                     hor->Heap_obj()->Id()));
    Is_Trace_cmd(_trace, sz_cr->Print(TFile));
    return FALSE;
  }
  Is_True(sz_cr->Kind() != CK_LDA && sz_cr->Kind() != CK_RCONST,
          ("size is LDA or RCONST"));

  BOOL valid = FALSE;
  // const value
  UINT cst = HOA_VALUE::TOP_VAL;
  if (sz_cr->Kind() == CK_CONST) {
    cst = sz_cr->Const_val();
    valid = TRUE;
    Is_Trace(_trace,
             (TFile, "[HPROP] hoa: ho%d size is constant %d.\n",
                     hor->Heap_obj()->Id(), cst));
  }
  VSYM_OBJ_REP *sz_vor = _prop->Vsa()->Cr_2_vor(sz_cr);
  Is_True(sz_cr->Kind() != CK_IVAR || sz_vor != NULL,
          ("vor not find for IVAR sz_cr"));

  // param
  UINT parm = HOA_VALUE::TOP_VAL;
  for (UINT i = VAR_INIT_ID; i < vec->size(); ++i) {
    CODEREP* parm_cr = (*vec)[i]->Cr();
    if (parm_cr == NULL)
      continue;
    Is_True(parm_cr->Kind() == CK_VAR || formal == FALSE,
            ("bad parm cr kind"));
    // same cr
    if (parm_cr == sz_cr) {
      parm = i;
      if (valid == FALSE)
        valid = TRUE;
      Is_Trace(_trace,
               (TFile, "[HPROP] hoa: ho%d size is param %d. same cr.\n",
                       hor->Heap_obj()->Id(), parm));
      break;
    }
    // check U-D of actual to see if the value is same as sz_cr
    if (formal == FALSE &&
        (Is_cr_def_by(parm_cr, sz_cr) == TRUE ||
         Is_cr_def_by(sz_cr, parm_cr) == TRUE)) {
      parm = i;
      if (valid == FALSE)
        valid = TRUE;
      Is_Trace(_trace,
               (TFile, "[HPROP] hoa: ho%d size is param %d. parm def by sz cr.\n",
                       hor->Heap_obj()->Id(), parm));
      break;
    }
  }

  // field, only available when hor is not on formal
  UINT fld = HOA_VALUE::TOP_VAL;
  if (vo_map != NULL) {
    VOR_FLD_MAP::const_iterator end = vo_map->end();
    for (VOR_FLD_MAP::const_iterator it = vo_map->begin();
         it != end; ++it) {
      VSYM_OBJ_REP *fld_vor = (VSYM_OBJ_REP *)it->first;

      // check if rhs matches
      STMTREP *def = fld_vor->Stmt_def();
      Is_True(def != NULL && OPERATOR_is_store(def->Opr()),
              ("def stmt is NULL"));
      CODEREP *rhs = def->Rhs();
      if (Is_cr_def_by(rhs, sz_cr) == TRUE ||
          Is_cr_def_by(sz_cr, rhs) == TRUE) {
        fld = it->second;
        if (valid == FALSE)
          valid = TRUE;
        Is_Trace(_trace,
                 (TFile, "[HPROP] hoa: ho%d size is fld %d on vo%dv%d def by sr%d.\n",
                         hor->Heap_obj()->Id(), fld,
                         fld_vor->Vsym_obj()->Id(), fld_vor->Version(),
                         sr->Stmtrep_id()));
        break;
      }
    }
  }

  if (valid == FALSE)
    return FALSE;

  HOA_VALUE val = HOA_VALUE::Make_value(idx, cst, parm, fld,
                                        hor->Attr() == ROR_DEF_BY_ALLOC, FALSE);
  _vec->push_back(val);
  return TRUE;
}

// HOA_BUILDER::Merge_heap_obj_annot
// merge existing heap_obj annotation
void
HOA_BUILDER::Merge_heap_obj_annot(HO_ANNOT hoa)
{
  HOA_HANDLE handle(hoa);

  UINT cnt = handle.Hoa_count();
  for (UINT i = 0; i < cnt; ++i) {
    HOA_VALUE val = handle.Hoa_value(i);
    if (val.Is_valid())
      Merge_value(val);
  }
}

// HOA_BUILDER::Find_heap_obj_annot
// find heap_obj annotation from input hor and its field hor
BOOL
HOA_BUILDER::Find_heap_obj_annot(UINT idx, HEAP_OBJ_REP *hor, STMTREP *sr,
                                 const VNODE_VECTOR *vec,
                                 BOOL formal)
{
  MEM_POOL* lpool = _prop->Loc_pool();
  // find out all vo and sub hor
  VOR_FLD_MAP *vo_map = CXX_NEW(VOR_FLD_MAP(3, HASHER<VSYM_OBJ_REP>(),
                                            std::equal_to<VSYM_OBJ_REP*>(),
                                            mempool_allocator<VOR_FLD_PAIR>(lpool)),
                                lpool);
  HOR_FLD_MAP *ho_map = CXX_NEW(HOR_FLD_MAP(3, HASHER<HEAP_OBJ_REP>(),
                                            std::equal_to<HEAP_OBJ_REP*>(),
                                            mempool_allocator<VOR_FLD_PAIR>(lpool)),
                                lpool);
  Find_sub_vo_hor(hor, sr, vo_map, ho_map);

  // handle hor
  BOOL valid = Find_sub_heap_obj_annot(idx, hor, sr,
                                       vec, vo_map, formal);

  // handle sub hor
  HOR_FLD_MAP::iterator end = ho_map->end();
  for (HOR_FLD_MAP::iterator it = ho_map->begin();
       it != end; ++it) {
    HEAP_OBJ_REP *fld_hor = it->first;
    valid |= Find_sub_heap_obj_annot(it->second, fld_hor, sr,
                                     vec, vo_map, formal);
  }
  return valid;
}

// HOA_BUILDER::Calculate_local_ho_annot
// calculate local heap_obj annot for malloc on output param/return value
// and free on input parameter
void
HOA_BUILDER::Calculate_local_ho_annot(DNA_NODE *dna)
{
  Is_True(dna != NULL && !dna->Non_functional(),
          ("non functional dna"));
  VSA *vsa = dna->Comp_unit()->Vsa();
  Is_True(vsa != NULL, ("invalid vsa"));

  // To check: probably need a context switch
  // CONTEXT_SWITCH ctx(dna);

  // setup hash map <ho, stat> on return stmts
  HO_ANNOT_MAP ho_map;
  STMTR_VECTOR::const_iterator it;
  for (it = dna->Rets_list()->begin(); it != dna->Rets_list()->end(); ++it) {
    STMTREP *stmt = *it;
    MU_LIST *mu_list = vsa->Stmt_hor_mu(stmt);
    if (mu_list && !mu_list->Is_Empty()) {
      MU_NODE *mnode;
      MU_LIST_ITER mu_iter;
      FOR_ALL_NODE(mnode, mu_iter, Init(mu_list)) {
        CHOR *chor = (CHOR*)mnode->OPND();
        HEAP_OBJ_REP* hor = chor->first;
        Is_True(hor != NULL && !vsa->Is_special_hor(hor),
                ("special hor on return"));
        V_ANNOT stat = Hor_stat(hor, NULL);
        Is_Trace(_trace,
                 (TFile, "[HPROP] return stmt %d ho%dv%d:%s%s\n",
                         stmt->Stmtrep_id(),
                         hor->Heap_obj()->Id(), hor->Version(),
                         VANT_UTIL::Get(stat, ANT_MALLOC) != 0 ? " malloc" : "",
                         VANT_UTIL::Get(stat, ANT_FREE) != 0 ? " free" : ""));
        if (stat != VANT_UTIL::Empty()) {
          V_ANNOT& ho_stat = ho_map[hor->Heap_obj()->Id()];
          ho_stat = VANT_UTIL::Join(ho_stat, stat);
        }
      }
    }
  }

  STMTREP *entry = vsa->Get_entry_chi_stmt();
  Is_True(entry != NULL && entry->Opr() == OPR_OPT_CHI,
          ("invalid entry stmt"));
  // setup hash_map <vo, vor> on entry chi
  VO_VOR_MAP vo_map;
  Build_vo_map<true>(vsa, entry, NULL, vo_map);

  INT i;
  // collect heap_obj on parameter
  for (i = VAR_INIT_ID; i < dna->Parm_list()->size(); ++i) {
    VAR_NODE *parm_node = (*dna->Parm_list())[i];
    Is_True(parm_node != NULL, ("invalid parm_node"));
    CODEREP *parm_cr = parm_node->Cr();
    if (parm_cr == NULL)
      continue;
    HEAP_OBJ_REP *hor = vsa->Cr_2_heap_obj(parm_cr);
    if (hor == NULL)
      continue;
    Is_True(!vsa->Is_special_hor(hor), ("invalid special hor"));

    HO_ANNOT_MAP::iterator it = ho_map.find(hor->Heap_obj()->Id());
    V_ANNOT stat = it != ho_map.end() ? it->second : VANT_UTIL::Empty();

    Is_Trace(_trace,
             (TFile, "[HPROP] find sub hor for parm %d cr%d ho%dv%d:%s%s\n",
                     i, parm_cr->Coderep_id(),
                     hor->Heap_obj()->Id(), hor->Version(),
                     VANT_UTIL::Get(stat, ANT_MALLOC) != 0 ? " malloc" : "",
                     VANT_UTIL::Get(stat, ANT_FREE) != 0 ? " free" : ""));
    if (stat != VANT_UTIL::Empty()) {
      HOA_VALUE val = HOA_VALUE::Make_value(0,
                                            HOA_VALUE::TOP_VAL,
                                            HOA_VALUE::TOP_VAL,
                                            HOA_VALUE::TOP_VAL,
                                            VANT_UTIL::Get(stat, ANT_MALLOC) != 0,
                                            VANT_UTIL::Get(stat, ANT_FREE) != 0);
      _vec->push_back(val);
    }

    // search 1 level of field
    Find_local_sub_ho_annot(hor, vsa, vo_map, ho_map);

    // export annot for current param
    if (_vec->size() > 0) {
      HO_ANNOT annot = Export_heap_obj_annot();
      Is_True(parm_node->Hoa_out() == 0, ("hoa out has set"));
      parm_node->Set_hoa_out(annot);
      Is_Trace(_trace,
               (TFile, "[HPROP] set parm %d out annot ", i));
      Is_Trace_cmd(_trace, HOA_HANDLE(annot).Print(0, TFile));
      _vec->clear();
    }
  }

  // collect heap_obj on return value
  std::vector<HO_ANNOT> ret_annot;
  for (i = VAR_INIT_ID; i < dna->Retv_list()->size(); ++i) {
    PDV_NODE* pdv = (*dna->Retv_list())[i];
    if (pdv->Kind() != BY_RETURNSTMT)
      continue;
    STMTREP *stmt = pdv->Stmt();
    Is_True(stmt && stmt->Opr() == OPR_STID &&
            stmt->Lhs()->Kind() == CK_VAR, ("not stid to var"));
    Is_True(stmt->Next() && stmt->Next()->Opr() == OPR_RETURN,
            ("next is not return"));

    HEAP_OBJ_REP *hor = vsa->Cr_2_heap_obj(stmt->Lhs());
    if (hor == NULL)
      continue;  // or break?
    if (vsa->Is_special_hor(hor))
      continue;

    V_ANNOT stat = Hor_stat(hor, NULL);
    if (stat != VANT_UTIL::Empty()) {
      // return value is from malloc
      HOA_VALUE val = HOA_VALUE::Make_value(0,
                                            HOA_VALUE::TOP_VAL,
                                            HOA_VALUE::TOP_VAL,
                                            HOA_VALUE::TOP_VAL,
                                            VANT_UTIL::Get(stat, ANT_MALLOC) != 0,
                                            VANT_UTIL::Get(stat, ANT_FREE) != 0);
      _vec->push_back(val);
    }
    // rebuild vo_map on return stmt
    vo_map.clear();
    STMTREP *ret_stmt = stmt->Next();
    Build_vo_map<false>(vsa, ret_stmt, hor, vo_map);

    // search 1 level of field
    Find_local_sub_ho_annot(hor, vsa, vo_map, ho_map);

    // export annot for return value
    if (_vec->size() > 0) {
      HO_ANNOT annot = Export_heap_obj_annot();
      ret_annot.push_back(annot);
      Is_True(pdv->Hoa() == 0, ("pdv hoa has set"));
      pdv->Set_hoa(annot);
      Is_Trace(_trace,
               (TFile, "[HPROP] set ret pdv %d annot ", i));
      Is_Trace_cmd(_trace, HOA_HANDLE(annot).Print(0, TFile));
      _vec->clear();
    }
  }

  // add annot on PDV to first item in DNA's parm list
  if (ret_annot.size() == 0)
    return;
  VAR_NODE *ret_node = (*dna->Parm_list())[0];
  if (ret_node == NULL) {
    ret_node = CXX_NEW(VAR_NODE(0, NULL), _prop->Mem_pool());
    (*dna->Parm_list())[0] = ret_node;
  }
  Is_True(ret_node != NULL, ("parm 0 is not initialized"));
  HO_ANNOT annot = ret_node->Hoa_out();
  if (annot == 0 && ret_annot.size() == 1) {
    annot = ret_annot[0];
  }
  else {
    Is_True(_vec && _vec->size() == 0, ("_vec not cleared"));
    if (annot != 0) {
      Merge_heap_obj_annot(annot);
    }
    for (INT i = 0; i < ret_annot.size(); ++i) {
      Merge_heap_obj_annot(ret_annot[i]);
    }
    annot = Export_heap_obj_annot();
  }
  ret_node->Set_hoa_out(annot);
}

// HOA_BUILDER::Export_heap_obj_annot
// export value to memory allocated from global pool and return the rep
HO_ANNOT
HOA_BUILDER::Export_heap_obj_annot() const
{
  UINT vec_sz = _vec->size();
  if (vec_sz == HOA_HANDLE::HOA_SZ_ZERO) {
    return HOA_VALUE::Top_value();
  }
  if (vec_sz == HOA_HANDLE::HOA_SZ_VALUE) {
    return _vec->at(0).Value();
  }
  UINT sz = vec_sz * sizeof(HOA_VALUE);
  const HOA_VALUE *data = (const HOA_VALUE*)&_vec->front();
  char *buf;
  if (vec_sz > HOA_HANDLE::HOA_SZ_ARRAY_MAX) {
    buf = (char *)MEM_POOL_Alloc(_prop->Mem_pool(), sz + sizeof(uint64_t));
    *(uint64_t *)buf = vec_sz;
    memcpy(buf + sizeof(uint64_t), data, sz);
    VSA_STATS_inc_n(hoa, vec_sz + 1);
  }
  else {
    buf = (char *)MEM_POOL_Alloc(_prop->Mem_pool(), sz);
    memcpy(buf, data, sz);
    VSA_STATS_inc_n(hoa, vec_sz);
  }
  return HOA_HANDLE(buf, vec_sz).Value();
}


// ====================================================================
//
// HOA_PROP
// implementation for heap_obj annotation propagation
//
// ====================================================================

// HOA_PROP::Propagate_hoa_to_cr
// propagate heap_obj annotation to HEAP_OBJ_REP on cr
void
HOA_PROP::Propagate_hoa_to_cr(RNA_NODE *rna, INT i, CODEREP *cr, HO_ANNOT annot)
{
  Is_True(cr != NULL && rna != NULL, ("bad cr or rna"));

  if (cr->Kind() == CK_CONST || cr->Kind() == CK_RCONST)
    return;

  HOA_HANDLE handle(annot);
  if (handle.Hoa_count() <= 1 || !handle.Hoa_value(0).Is_valid()) {
    Is_Trace(_trace,
             (TFile, "[HPROP] rna-out: no valid hoa\n"));
    return;
  }
  // only match first level, TODO: match sub-levels
  HOA_VALUE val = handle.Find_hoa(0);
  if (!val.Is_valid()) {
    Is_Trace(_trace,
             (TFile, "[HPROP] rna-out: TODO: not find hoa for field 0.\n"));
    return;
  }

  // find base pointer for CK_OP
  if (cr->Kind() == CK_OP) {
    cr = Find_ilod_base(cr);
    if (cr == NULL) {
      Is_Trace(_trace,
               (TFile, "[HPROP] rna-out: failed to find ilod base.\n"));
      return;
    }
  }

  STMTREP *sr = rna->Callstmt();
  Is_True(sr != NULL && OPERATOR_is_call(sr->Opr()), ("bad rna callstmt"));

  BOOL is_lda = FALSE;
  if (cr->Kind() == CK_LDA) {
    Is_True_Ret(sr->Chi_list() != NULL, ("var chi list is NULL"));
    CHI_NODE *chi = sr->Chi_list()->Search_chi_node(cr->Lda_aux_id());
    if (chi == NULL || !chi->Live()) {
      Is_Trace(_trace,
               (TFile, "[HPROP] rna-out: TODO: no chi node for LDA.\n"));
      return;
    }
    cr = chi->RESULT();
    is_lda = TRUE;
    Is_Trace(_trace,
             (TFile, "[HPROP] rna-out: TODO: handle LDA.\n"));
    return;
  }

  if (cr->Kind() == CK_IVAR) {
    Is_Trace(_trace,
             (TFile, "[HPROP] rna-out: TODO: find vor chi node from stmt.\n"));
    return;
  }

  HEAP_OBJ_REP *hor = Vsa()->Cr_2_heap_obj(cr);
  if (hor != NULL) {
    UINT parm = val.Hoa_size_param_id();
    UINT fld = val.Hoa_size_field_id();
    UINT cst_size = val.Hoa_const_size();
    CODEREP *sz_cr = NULL;
    if (parm > 0 && parm != HOA_VALUE::PARM_MAX) {
      Is_True(parm < rna->Arg_list()->size(), ("parm out of bound"));
      sz_cr = (*rna->Arg_list())[parm]->Cr();
      Is_True(sz_cr != NULL, ("not find actual cr"));
    }
    else if (fld > 0 && fld != HOA_VALUE::FIELD_MAX) {
      Is_Trace(_trace,
               (TFile, "[HPROP] rna-out: TODO: field.\n"));
    }
    else if (cst_size > 0 && cst_size != HOA_VALUE::CONST_MAX) {
      sz_cr = Comp_unit()->Htable()->Add_const(MTYPE_I8, cst_size);
    }

    if (sz_cr != NULL) {
      Is_Trace(_trace,
               (TFile, "[HPROP] rna-out: set retv ho%dv%d on cr%d size from cr%d to cr%d.\n",
                       hor->Heap_obj()->Id(), hor->Version(),
                       cr->Coderep_id(),
                       hor->Heap_obj()->Byte_size() ? hor->Heap_obj()->Byte_size()->Coderep_id()
                                                    : 0,
                       sz_cr->Coderep_id()));
      Is_Trace_cmd(_trace && hor->Heap_obj()->Byte_size(),
                   hor->Heap_obj()->Byte_size()->Print(TFile));
      Is_Trace_cmd(_trace,
                   sz_cr->Print(TFile));
      Is_True(hor->Heap_obj()->Byte_size() == NULL ||
              hor->Heap_obj()->Byte_size() == sz_cr ||
              (hor->Heap_obj()->Byte_size()->Kind() == CK_CONST &&
               sz_cr->Kind() == CK_CONST &&
               hor->Heap_obj()->Byte_size()->Const_val() == sz_cr->Const_val()),
              ("ret hor size already set"));
      hor->Heap_obj()->Set_byte_size(sz_cr);
    }
    else {
      Is_Trace(_trace,
               (TFile, "[HPROP] rna-out: no parm/fld/const size found for ho%dv%d on cr%d?\n",
                       hor->Heap_obj()->Id(), hor->Version(),
                       cr->Coderep_id()));
    }
  }
  else {
    Is_Trace(_trace,
             (TFile, "[HPROP] rna-out: TODO: should create HEAP_OBJ_REP for cr%d?\n",
                     cr->Coderep_id()));
  }
}

// HOA_PROP::Find_hoa_on_stmt_chi
// Find heap_obj annotation on stmt chi
pair<HO_ANNOT, BOOL>
HOA_PROP::Find_hoa_on_stmt(CODEREP *cr, HO_ANNOT out, STMTREP *def)
{
  Is_True(cr != NULL, ("invalid cr"));
  Is_True(def != NULL, ("def is null"));
  if (def->Opr() != OPR_OPT_CHI && !OPERATOR_is_call(def->Opr())) {
    Is_Trace(_trace,
             (TFile, "[HPROP] TODO: hoa for cr%d on sr%d %s at line %d.\n",
                     cr->Coderep_id(),
                     def->Stmtrep_id(),
                     OPERATOR_name(def->Opr()) + 4,
                     Srcpos_To_Line(def->Linenum())));
    return pair<HO_ANNOT, BOOL>(0, FALSE);
  }

  if (OPERATOR_is_call(def->Opr())) {
    RNA_NODE *rna = Vsa()->Sr_2_rna(def);
    Is_True_Ret(rna != NULL, ("not find rna"), pair<HO_ANNOT, BOOL>(0, FALSE));

    AUX_STAB_ENTRY* aux = cr->Kind() == CK_VAR
                            ? Opt_stab()->Aux_stab_entry(cr->Aux_id())
                            : NULL;
    IDTYPE arg_id = INVALID_VAR_IDX;
    BOOL   is_lda = FALSE;
    if (aux && aux->Is_return_preg()) {
      arg_id = 0;
    }
    else {
      pair<IDTYPE, BOOL> arg = rna->Get_arg(cr, Vsa());
      if (arg.first == INVALID_VAR_IDX) {
        return pair<HO_ANNOT, BOOL>(0, FALSE);
      }
      arg_id = arg.first;
      is_lda = arg.second;
    }
    Is_True(arg_id >= 0 && arg_id < rna->Arg_list()->size(),
            ("arg_id out of bound"));
    HO_ANNOT hoa = rna->Arg_list()->at(arg_id)->Hoa_out();
    return pair<HO_ANNOT, BOOL>(hoa, is_lda);
  }
  else if (def->Opr() == OPR_OPT_CHI) {
    IDTYPE parm_id = Dna()->Is_param(cr);
    if (parm_id != INVALID_VAR_IDX) {
      VAR_NODE *parm_node = (*Dna()->Parm_list())[parm_id];
      Is_True(parm_node != NULL, ("invalid parm_node"));
      if (out != 0) {
        HO_ANNOT hoa_out = parm_node->Hoa_out();
        if (hoa_out == 0) {
          hoa_out = out;
        }
        else {
          Is_Trace(_trace,
                   (TFile, "[HPROP] before-out: parm %d hoa:\n", parm_id));
          Is_Trace_cmd(_trace, HOA_HANDLE(hoa_out).Print(0, TFile));

          HOA_BUILDER bldr(this);
          bldr.Merge_heap_obj_annot(out);
          bldr.Merge_heap_obj_annot(hoa_out);
          hoa_out = bldr.Export_heap_obj_annot();
        }
        parm_node->Set_hoa_out(out);

        Is_Trace(_trace,
                 (TFile, "[HPROP] after-out: parm %d hoa:\n", parm_id));
        Is_Trace_cmd(_trace, HOA_HANDLE(out).Print(0, TFile));
      }
      HO_ANNOT hoa = parm_node->Hoa_in();
      return pair<HO_ANNOT, BOOL>(hoa, FALSE);
    }
  }
  else {
    Is_Trace(_trace,
             (TFile, "TODO: chi def by sr%d %s at line %d.\n",
                     def->Stmtrep_id(),
                     OPERATOR_name(def->Opr()) + 4,
                     Srcpos_To_Line(def->Linenum())));
  }
  return pair<HO_ANNOT, BOOL>(0, FALSE);
}

// HOA_PROP::Find_hoa_on_cr
// Find heap_obj annotation on generic cr
HO_ANNOT
HOA_PROP::Find_hoa_on_cr(CODEREP *cr, HO_ANNOT out, hash_set<IDTYPE>& visited)
{
  Is_True(cr != NULL, ("cr is null"));
  VSYM_OBJ_REP *vor;
  switch (cr->Kind()) {
  case CK_LDA:
  case CK_CONST:
  case CK_RCONST:
    return 0;

  case CK_VAR:
    return Find_hoa_on_var(cr, out, visited);

  case CK_IVAR:
    Is_True(cr->Opr() != OPR_PARM, ("invalid parm opr"));
    vor = Vsa()->Cr_2_vor(cr);
    return vor ? Find_hoa_on_vor(vor, out, visited) : 0;

  case CK_OP:
    cr = Find_ilod_base(cr);
    return cr ? Find_hoa_on_cr(cr, out, visited) : 0;

  default:
    Is_True(FALSE, ("bad cr kind"));
  }
  return 0;
}

// HOA_PROP::Find_hoa_on_var
// Find heap_obj annotation from var's U-D
HO_ANNOT
HOA_PROP::Find_hoa_on_var(CODEREP *cr, HO_ANNOT out, hash_set<IDTYPE>& visited)
{
  Is_True(cr != NULL && cr->Kind() == CK_VAR, ("not var cr"));
  // No U-D for def/ret vsym, zero version and volatile
  if (cr->Aux_id() == Opt_stab()->Default_vsym() ||
      cr->Aux_id() == Opt_stab()->Return_vsym() ||
      cr->Is_flag_set(CF_IS_ZERO_VERSION) ||
      cr->Is_var_volatile())
    return 0;

  if (cr->Is_flag_set(CF_DEF_BY_PHI)) {
    PHI_NODE *phi = cr->Defphi();
    Is_True(phi != NULL && phi->Live(), ("invalid defphi"));
    if (visited.find(phi->Bb()->Id()) != visited.end())
      return 0;
    visited.insert(phi->Bb()->Id());

    std::vector<HO_ANNOT> annot;
    for (INT i = 0; i < phi->Size(); ++i) {
      CODEREP *opnd_cr = phi->OPND(i);
      Is_True(opnd_cr != NULL, ("invalid phi opnd"));
      HO_ANNOT opnd_hoa = Find_hoa_on_var(opnd_cr, out, visited);
      if (opnd_hoa != 0)
        annot.push_back(opnd_hoa);
    }
    if (annot.size() == 0) {
      return 0;
    }
    else if (annot.size() == 1) {
      return annot[0];
    }
    else {
      HOA_BUILDER bldr(this);
      for (INT i = 0; i < annot.size(); ++i) {
        bldr.Merge_heap_obj_annot(annot[i]);
      }
      // TODO: export to a temporary memory?
      return bldr.Export_heap_obj_annot();
    }
  }
  else if (cr->Is_flag_set(CF_DEF_BY_CHI)) {
    STMTREP *def = cr->Get_defstmt();
    Is_True(def, ("bad defchi"));
    pair<HO_ANNOT, BOOL> hoa = Find_hoa_on_stmt(cr, out, def);
    if (hoa.second == TRUE) {
      // TODO: is_lda
      Is_Trace(_trace,
               (TFile, "[HPROP] TODO: hoa on LDA cr%d on sr%d %s line %d.\n",
                       cr->Coderep_id(),
                       def->Stmtrep_id(), OPERATOR_name(def->Opr()) + 4,
                       Srcpos_To_Line(def->Linenum())));
      return 0;
    }
    return hoa.first;
  }
  else {
    STMTREP *def = cr->Get_defstmt();
    Is_True(def && def->Lhs() == cr && def->Rhs(), ("bad defstmt"));
    return Find_hoa_on_cr(def->Rhs(), out, visited);
  }
  return 0;
}

// HOA_PROP::Find_hoa_on_vor
// Find heap_obj annotation from vor's U-D
HO_ANNOT
HOA_PROP::Find_hoa_on_vor(VSYM_OBJ_REP *vor, HO_ANNOT out, hash_set<IDTYPE>& visited)
{
  Is_True(vor != NULL, ("invalid vor"));

  switch (vor->Attr()) {
  case ROR_DEF_BY_PHI:
  case ROR_DEF_BY_HORPHI:
    {
      PHI_NODE *phi = vor->Phi_def();
      Is_True(phi != NULL, ("bad vor phidef"));
      if (visited.find(phi->Bb()->Id()) != visited.end())
        return 0;
      visited.insert(phi->Bb()->Id());

      std::vector<HO_ANNOT> annot;
      for (INT i = 0; i < phi->Size(); ++i) {
        VSYM_OBJ_REP *opnd_vor = (VSYM_OBJ_REP*)phi->OPND(i);
        Is_True(opnd_vor != NULL, ("invalid vor"));
        HO_ANNOT opnd_hoa = Find_hoa_on_vor(opnd_vor, out, visited);
        if (opnd_hoa != 0)
          annot.push_back(opnd_hoa);
      }
      if (annot.size() == 0) {
        return 0;
      }
      else if (annot.size() == 1) {
        return annot[0];
      }
      else {
        HOA_BUILDER bldr(this);
        for (INT i = 0; i < annot.size(); ++i) {
          bldr.Merge_heap_obj_annot(annot[i]);
        }
        // TODO: export to a temporary memory?
        return bldr.Export_heap_obj_annot();
      }
    }

  case ROR_DEF_BY_CHI:
    {
      STMTREP *def = vor->Stmt_def();
      if (def == NULL)
        return 0;
      CODEREP *base = Vsa()->Find_vor_chi_cr(def, vor);
      if (base == NULL)
        return 0;
      pair<HO_ANNOT, BOOL> hoa = Find_hoa_on_stmt(base, out, def);
      if (hoa.first == 0)
        return 0;
      // TODO: match hoa with vor vfr to adjust hoa
      return 0;
    }

  case ROR_DEF_BY_COPY:
  case ROR_DEF_BY_ISTORE:
    Is_True(vor->Stmt_def() && vor->Stmt_def()->Rhs(), ("bad defstmt"));
    return Find_hoa_on_cr(vor->Stmt_def()->Rhs(), out, visited);

  default:
    break;
  }
  return 0;
}

// HOA_PROP::Calculate_local_ho_annot
// calculate local heap_obj annot for malloc on output param/return valu
// and free on input parameter
void
HOA_PROP::Calculate_local_ho_annot()
{
  HOA_BUILDER bldr(this);
  bldr.Calculate_local_ho_annot(_comp_unit->Dna());
}

// HOA_PROP::Propagate_ho_annot_on_entry
// propagate clby's actual heap_size_annot to formals
void
HOA_PROP::Propagate_ho_annot_on_entry()
{
  DNA_NODE *dna = _comp_unit->Dna();
  VSA *vsa = _comp_unit->Vsa();
  for (INT parm = VAR_INIT_ID;
       parm < dna->Parm_list()->size(); parm++) {
    VAR_NODE *parm_node = (*dna->Parm_list())[parm];
    Is_True(parm_node != NULL, ("invalid parm_node"));
    CODEREP *parm_cr = parm_node->Cr();
    if (parm_cr == NULL)
      continue;
    HEAP_OBJ_REP *hor = vsa->Cr_2_heap_obj(parm_cr);
    if (hor == NULL)
      continue;

    HOA_BUILDER bldr(this);
    for (INT clby = VAR_INIT_ID;
         clby < dna->Clby_list()->size(); clby++) {
      RNA_NODE *rna = (*dna->Clby_list())[clby];
      if (parm > rna->Arg_cnt())
        continue;

      VAR_NODE *arg = rna->Arg_list()->at(parm);
      bldr.Merge_heap_obj_annot(arg->Hoa_in());
    }

    if (parm_node->Hoa_in() != 0) {
      bldr.Merge_heap_obj_annot(parm_node->Hoa_in());
    }
    HO_ANNOT annot = bldr.Export_heap_obj_annot();
    parm_node->Set_hoa_in(annot);
    Is_Trace(_trace,
             (TFile, "[HPROP] entry: %s set formal %d with hoa: ",
                     dna->Fname(), parm));
    Is_Trace_cmd(_trace, HOA_HANDLE(annot).Print(0, TFile));
  }
}

// HOA_PROP::Propagate_ho_annot_from_rna
// propagate hea_obj annot on callee's DNA_NODE's return to actual
void
HOA_PROP::Propagate_ho_annot_from_rna(RNA_NODE *rna)
{
  STMTREP *sr = rna->Callstmt();
  CODEREP *retval = _comp_unit->Find_return_value(sr);
  Is_True(retval == NULL || retval->Kind() == CK_VAR ||
          retval->Kind() == CK_IVAR, ("retval not var"));
  INT arg_cnt = sr->Opr() == OPR_ICALL ? sr->Rhs()->Kid_count() - 1
                                       : sr->Rhs()->Kid_count();
  // 0 for retval, [1, arg_cnt] for params
  for (IDTYPE i = 0; i <= arg_cnt; i++) {
    if (i == 0) {
      if (retval == NULL)
        continue;
    }
    else {
      // only merge parm with pointer type
      CODEREP *parm = sr->Rhs()->Opnd(i - 1);
      Is_True(parm && parm->Kind() == CK_IVAR && parm->Opr() == OPR_PARM,
              ("invalid parm"));
      if (TY_kind(parm->Ilod_ty()) != KIND_POINTER) {
        continue;
      }
    }

    HOA_BUILDER bldr(this);
    for(CALLEE_VECTOR::const_iterator it = rna->Callee_list().begin();
        it != rna->Callee_list().end(); ++it) {
      DNA_NODE* callee = _ipsa->Get_dna(it->Callee());
      Is_True(callee != NULL, ("bad callee"));
      if (callee->Non_functional())
        continue;

      if (i >= callee->Parm_list()->size())
        continue;

      VAR_NODE *callee_parm = (*callee->Parm_list())[i];
      if (callee_parm != NULL)
        bldr.Merge_heap_obj_annot(callee_parm->Hoa_out());
    }

    VAR_NODE *caller_act = (*rna->Arg_list())[i];
    Is_True(caller_act != NULL, ("invalid caller actual node"));
    if (caller_act->Hoa_out() != 0) {
      bldr.Merge_heap_obj_annot(caller_act->Hoa_out());
    }
    HO_ANNOT annot = bldr.Export_heap_obj_annot();
    if (annot == HOA_VALUE::Top_value())
      continue;

    caller_act->Set_hoa_out(annot);
    Is_Trace(_trace,
             (TFile, "[HPROP] rna-out: set %s %d sr%d %s at line %d with hoa: ",
                     (i > 0) ? "output parm" : "retval",
                     i,
                     sr->Stmtrep_id(),
                     sr->Opr() == OPR_CALL ? ST_name(sr->St()) : "icall",
                     Srcpos_To_Line(sr->Linenum())));
    Is_Trace_cmd(_trace, HOA_HANDLE(annot).Print(0, TFile));

    // propagate heap_obj annotation to HEAP_OBJ_REP on cr
    CODEREP *cr = (i == 0) ? retval : caller_act->Cr();
    Is_True(cr != NULL, ("actual param or return value is null"));
    if (cr) {
      Propagate_hoa_to_cr(rna, i, cr, annot);
    }
  }
}

// HOA_PROP::Calculate_ho_annot_on_return
// propagate heap_obj size info to DNA's PDV list
void
HOA_PROP::Calculate_ho_annot_on_return()
{
  DNA_NODE *dna = _comp_unit->Dna();
  INT retv_cnt = dna->Retv_list()->size();
  if (retv_cnt == 0)
    return;

  VSA *vsa = _comp_unit->Vsa();

  // to merge annotation to formal
  INT parm_cnt = dna->Parm_list()->size();
  HOA_BUILDER *parm_bldr = (HOA_BUILDER *)MEM_POOL_Alloc(Loc_pool(),
                             sizeof(HOA_BUILDER) * parm_cnt);
  for (INT i = 0; i < parm_cnt; ++i) {
    new (&parm_bldr[i]) HOA_BUILDER(this);
  }

  // TODO: this code deosn't work for more than 1 level heap_obj, should still start
  // from vor mu on return stmt
  for (INT i = VAR_INIT_ID; i < retv_cnt; ++i) {
    PDV_NODE* pdv = (*dna->Retv_list())[i];
    STMTREP *stmt = pdv->Stmt();

    // TODO: BY_FREEIPARM, BY_GLOBALVAR, BY_FREEGLOBL
    if (pdv->Kind() != BY_RETURNSTMT &&
        pdv->Kind() != BY_PARAMETER)
      continue;

    Is_True(stmt &&
            stmt->Lhs() && stmt->Rhs() &&
            (stmt->Lhs()->Kind() == CK_VAR ||
             stmt->Lhs()->Kind() == CK_IVAR), ("not var or ivar"));

    hash_set<IDTYPE> visited;
    HO_ANNOT hoa = Find_hoa_on_cr(stmt->Rhs(), 0, visited);
    if (hoa == 0) {
      Is_Trace(_trace,
               (TFile, "[HPROP] exit: %s failed to set hoa on %s %d sr%d at line %d.\n",
                       dna->Fname(),
                       pdv->Oparam() ? "outparm" : "retval",
                       pdv->Oparam(),
                       stmt->Stmtrep_id(),
                       Srcpos_To_Line(stmt->Linenum())));
      continue;
    }

    HO_ANNOT pdv_hoa = pdv->Hoa();
    if (pdv_hoa != 0) {
      // trace pdv hoa before merge
      Is_Trace(_trace,
               (TFile, "[HPROP] before-return: %s %s %d sr%d at line %d hoa: ",
                       dna->Fname(),
                       pdv->Oparam() ? "outparm" : "retval",
                       pdv->Oparam(),
                       stmt->Stmtrep_id(),
                       Srcpos_To_Line(stmt->Linenum())));
      Is_Trace_cmd(_trace, HOA_HANDLE(pdv_hoa).Print(0, TFile));

      HOA_BUILDER bldr(this);
      bldr.Merge_heap_obj_annot(pdv_hoa);
      bldr.Merge_heap_obj_annot(hoa);
      pdv_hoa = bldr.Export_heap_obj_annot();
    }
    else {
      pdv_hoa = hoa;
    }
    pdv->Set_hoa(pdv_hoa);
    // trace pdv hoa after merge
    Is_Trace(_trace,
             (TFile, "[HPROP] exit: %s set %s %d sr%d at line %d with hoa: ",
                     dna->Fname(),
                     pdv->Oparam() ? "outparm" : "retval",
                     pdv->Oparam(),
                     stmt->Stmtrep_id(),
                     Srcpos_To_Line(stmt->Linenum())));
    Is_Trace_cmd(_trace, HOA_HANDLE(pdv_hoa).Print(0, TFile));

    UINT parm_idx = pdv->Oparam();
    parm_bldr[parm_idx].Merge_heap_obj_annot(pdv_hoa);
  }

  // set to retval/output parm hor_out
  for (INT i = 0; i < dna->Parm_list()->size(); ++i) {
    if (parm_bldr[i].Heap_obj_annot_count() > 0) {
      HO_ANNOT annot = parm_bldr[i].Export_heap_obj_annot();
      VAR_NODE *parm_node = (*dna->Parm_list())[i];
      Is_True(parm_node != NULL, ("invalid parm_node"));
      parm_node->Set_hoa_out(annot);
      Is_Trace(_trace,
               (TFile, "[HPROP] exit: %s set %s %d out with hoa: ",
                       dna->Fname(),
                       (i > 0) ? "outparm" : "retval",
                       i));
      Is_Trace_cmd(_trace, HOA_HANDLE(annot).Print(0, TFile));
    }
  }
}

// HOA_PROP::Calculate_ho_annot_on_rna
// calculate heap_obj size on actual and save to RNA_NODE's Arg_list()
void
HOA_PROP::Calculate_ho_annot_on_rna(RNA_NODE *rna)
{
  STMTREP *sr = rna->Callstmt();
  Is_True(sr && OPERATOR_is_call(sr->Opr()), ("bad rna callstmt"));
  VSA *vsa = _comp_unit->Vsa();

  INT arg_cnt = sr->Opr() == OPR_ICALL ? sr->Rhs()->Kid_count() - 1
                                       : sr->Rhs()->Kid_count();

  for (INT i = 0; i < arg_cnt; ++i) {
    CODEREP* parm = sr->Rhs()->Opnd(i);
    Is_True(parm && parm->Kind() == CK_IVAR && parm->Opr() == OPR_PARM,
            ("invalid parm"));
    if (TY_kind(parm->Ilod_ty()) != KIND_POINTER) {
      continue;
    }

    CODEREP* base = parm->Ilod_base();
    base = Find_ilod_base(parm->Ilod_base());
    if (base == NULL) {
      continue;
    }

    VAR_NODE *arg = rna->Arg_list()->at(i + 1);
    Is_True(arg != NULL, ("arg %d is null", i + 1));
    // TODO: handle LDA
    BOOL is_lda = base->Kind() == CK_LDA;

    hash_set<IDTYPE> visited;
    HO_ANNOT hoa = Find_hoa_on_cr(base, arg->Hoa_out(), visited);
    if (hoa == 0) {
      Is_Trace(_trace,
               (TFile, "[HPROP] rna-call: no hoa on arg %d of sr%d %s at line %d\n",
                       i,
                       sr->Stmtrep_id(),
                       sr->Opr() == OPR_CALL ? ST_name(sr->St()) : "icall",
                       Srcpos_To_Line(sr->Linenum())));
      continue;
    }


    HO_ANNOT arg_hoa = arg->Hoa_in();
    if (arg_hoa != 0) {
      Is_Trace(_trace,
               (TFile, "[HPROP] before-call: actual %d sr%d %s at line %d with hoa: ",
                       i,
                       sr->Stmtrep_id(),
                       sr->Opr() == OPR_CALL ? ST_name(sr->St()) : "icall",
                       Srcpos_To_Line(sr->Linenum())));
      Is_Trace_cmd(_trace, HOA_HANDLE(arg_hoa).Print(0, TFile));
      HOA_BUILDER bldr(this);
      bldr.Merge_heap_obj_annot(arg_hoa);
      bldr.Merge_heap_obj_annot(hoa);
      arg_hoa = bldr.Export_heap_obj_annot();
    }
    else {
      arg_hoa = hoa;
    }
    arg->Set_hoa_in(arg_hoa);
    Is_Trace(_trace,
             (TFile, "[HPROP] rna-call: set actual %d sr%d %s at line %d with hoa: ",
                     i,
                     sr->Stmtrep_id(),
                     sr->Opr() == OPR_CALL ? ST_name(sr->St()) : "icall",
                     Srcpos_To_Line(sr->Linenum())));
    Is_Trace_cmd(_trace, HOA_HANDLE(arg_hoa).Print(0, TFile));
  }
}

// HOA_PROP::Print_hoa
// Print
void
HOA_PROP::Print_hoa(HO_ANNOT annot, FILE *fp)
{
  HOA_HANDLE(annot).Print(0, fp);
}

// HOA_PROP::Print_hoa
// Print in debugger
void
HOA_PROP::Print_hoa(HO_ANNOT annot)
{
  HOA_HANDLE(annot).Print(0, stdout);
}

// HOA_PROP::Print_dna
// Print dna's hoa
void
HOA_PROP::Print_dna(DNA_NODE *dna, FILE *fp)
{
  INT i;
  fprintf(fp, "============================================================\n");
  fprintf(fp, "Heap_obj annotation for %s\n", dna->Fname());
  fprintf(fp, "- Formals:\n");
  // DNA param in/out
  for (i = VAR_INIT_ID; i < dna->Parm_list()->size(); ++i) {
    const VAR_NODE *parm_node = (*dna->Parm_list())[i];
    Is_True(parm_node != NULL, ("invalid parm_node"));
    INT parm_cr = parm_node->Cr() ? parm_node->Cr()->Coderep_id() : 0;
    if (parm_node->Hoa_in() == 0 && parm_node->Hoa_out() == 0) {
      fprintf(fp, "  [%d]: cr%d no hoa\n", i, parm_cr);
      continue;
    }
    if (parm_node->Hoa_in() != 0) {
      fprintf(fp, "  [%d]: cr%d in: ", i, parm_cr);
      Print_hoa(parm_node->Hoa_in(), fp);
    }
    if (parm_node->Hoa_out() != 0) {
      fprintf(fp, "  [%d]: cr%d out: ", i, parm_cr);
      Print_hoa(parm_node->Hoa_out(), fp);
    }
  }

  // DNA return
  fprintf(fp, "- Return values:\n");
  for (i = VAR_INIT_ID; i < dna->Retv_list()->size(); ++i) {
    const PDV_NODE* pdv = (*dna->Retv_list())[i];
    Is_True(pdv != NULL, ("invalid pdv node"));
    if (pdv->Kind() != BY_RETURNSTMT)
      continue;
    STMTREP *stmt = pdv->Stmt();
    Is_True(stmt != NULL, ("invalid pdv_stmt"));
    if (pdv->Hoa() == 0) {
      fprintf(fp, "  [%d]: sr%d %s %d no hoa\n",
                  i, stmt->Stmtrep_id(),
                  OPERATOR_name(stmt->Opr()) + 4,
                  Srcpos_To_Line(stmt->Linenum()));
      continue;
    }
    fprintf(fp, "  [%d]: sr%d %s %d return: ",
                i, stmt->Stmtrep_id(),
                OPERATOR_name(stmt->Opr()) + 4,
                Srcpos_To_Line(stmt->Linenum()));
    Print_hoa(pdv->Hoa(), fp);
  }

  // RNA actual in/out
  fprintf(fp, "- Call lists:\n");
  for (i = VAR_INIT_ID; i < dna->Call_list()->size(); ++i) {
    RNA_NODE *rna = (*dna->Call_list())[i];
    Is_True(rna && rna->Callstmt(), ("rna or callstmt invalid"));
    STMTREP *stmt = rna->Callstmt();
    fprintf(fp, "  + Call sr%d %s %d:\n",
                stmt->Stmtrep_id(), OPERATOR_name(stmt->Opr()) + 4,
                Srcpos_To_Line(stmt->Linenum()));
    INT j;
    for (j = VAR_INIT_ID; j < rna->Arg_list()->size(); ++j) {
      const VAR_NODE *arg_node = rna->Arg_list()->at(j);
      Is_True(arg_node != NULL, ("invalid arg_node"));
      INT arg_cr = arg_node->Cr() ? arg_node->Cr()->Coderep_id() : 0;
      if (arg_node->Hoa_in() == 0 && arg_node->Hoa_out() == 0) {
        fprintf(fp, "   [%d]: cr%d no hoa\n", i, arg_cr);
        continue;
      }
      if (arg_node->Hoa_in() != 0) {
        fprintf(fp, "   [%d]: cr%d in: ", i, arg_cr);
        Print_hoa(arg_node->Hoa_in(), fp);
      }
      if (arg_node->Hoa_out() != 0) {
        fprintf(fp, "   [%d]: cr%d out: ", i, arg_cr);
        Print_hoa(arg_node->Hoa_out(), fp);
      }
    }
  }
  // RNA return
}

// HOA_PROP::Print_dna
// Print dna's hoa for debugger
void
HOA_PROP::Print_dna(DNA_NODE *dna)
{
  Print_dna(dna, stdout);
}

// ====================================================================
//
// DNA_NODE::Calculate_local_ho_annot
//
// ====================================================================
void
DNA_NODE::Calculate_local_ho_annot(IPSA *ipsa)
{
  HOA_PROP heap_prop(ipsa, Comp_unit());
  // collect local heap_obj annotation
  heap_prop.Calculate_local_ho_annot();
  // dump flag after local hoa is calculated
  Is_Trace(Get_Trace(TP_VSA, VSA_HPROP_TRACE_FLAG),
           (TFile, "============================================================\n"));
  Is_Trace(Get_Trace(TP_VSA, VSA_HPROP_TRACE_FLAG),
           (TFile, "DUMP after local hoa calculation for %s\n", Fname()));
  Is_Trace_cmd(Get_Trace(TP_VSA, VSA_HPROP_TRACE_FLAG),
               HOA_PROP::Print_dna(this, TFile));
  Is_Trace(Get_Trace(TP_VSA, VSA_HPROP_TRACE_FLAG),
           (TFile, "============================================================\n"));
}

