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
#include "opt_vsa_util.h"
#include "cxx_memory.h"
#include "report.h"
#include "erbe.h"
#include <ext/hash_set>
#include "opt_vsa_rbc.h"
#include "config_vsa.h"
#include "rbc_base.h"

// =============================================================================
//
// TB_LIST::Find returns the tag_base that has the same name
//
// =============================================================================
TAG_BASE*
TB_LIST::Find(STRING tag_name)
{
  TAG_BASE    *tag_base;
  TB_LIST_ITER tb_list_iter;
  FOR_ALL_ELEM (tag_base, tb_list_iter, Init(this)) {
    if (strcmp(tag_base->Tag_name(), tag_name) == 0) {
      return tag_base;
    }
  }
  return NULL;
}

// =============================================================================
//
// TB_LIST::Count returns the number of elements in this TB_LIST
//
// =============================================================================
INT
TB_LIST::Count(void)
{
  TAG_BASE    *tag_base;
  TB_LIST_ITER tb_list_iter;
  INT count = 0;
  FOR_ALL_ELEM (tag_base, tb_list_iter, Init(this)) {
    ++count;
  }
  return count;
}

// =============================================================================
//
// TB_LIST::Real_use_cnt returns the number of tag_base that been used in user 
// source code
//
// =============================================================================
INT
TB_LIST::Real_use_cnt(void)
{
  TAG_BASE    *tag_base;
  TB_LIST_ITER tb_list_iter;
  INT count = 0;
  FOR_ALL_ELEM (tag_base, tb_list_iter, Init(this)) {
    if (tag_base->Is_set_flag(RSC_TAG_USED))
      ++count;
  }
  return count;
}

// =============================================================================
//
// Print function for tracing Fsm Base Object lifecycle
//
// =============================================================================
void
TB_LIST::Print(FILE *fp)
{
  TAG_BASE    *fsm_base;
  TB_LIST_ITER tb_list_iter;
  FOR_ALL_ELEM (fsm_base, tb_list_iter, Init(this)) {
    fsm_base->Print(fp);
  }
}

#if 0
// =============================================================================
//
// TO_LIST::Find returns the tag_obj that has the same name
//
// =============================================================================
TAG_OBJ*
TO_LIST::Find(void *tagobjrp)
{
  TAG_OBJ     *tag_obj;
  TO_LIST_ITER to_list_iter;
  FOR_ALL_ELEM (tag_obj, to_list_iter, Init(this)) {
    if ((void*)tag_obj->Coderep() == tagobjrp) {
      return tag_obj;
    }
  }
  return NULL;
}


// =============================================================================
//
// TO_LIST::Count returns the number of elements in this TO_LIST
//
// =============================================================================
INT
TO_LIST::Count(void)
{
  TAG_OBJ     *tag_obj;
  TO_LIST_ITER to_list_iter;
  INT count = 0;
  FOR_ALL_ELEM (tag_obj, to_list_iter, Init(this)) {
    ++count;
  }
  return count;
}


// =============================================================================
//
// Print function for tracing Tag Object lifecycle
//
// =============================================================================
void
TO_LIST::Print(FILE *fp)
{
  TAG_OBJ     *tag_obj;
  TO_LIST_ITER to_list_iter;
  BOOL first = TRUE;
  FOR_ALL_ELEM (tag_obj, to_list_iter, Init(this)) {
    if (first) {
      fprintf(fp, "[");
      first = FALSE;
    } else {
      fprintf(fp, ":");
    }
    tag_obj->Print(fp);
  }
  if (!first)
    fprintf(fp, "]");
}
#endif

// =============================================================================
//
// TOR_LIST_OLD::Count, count the tor list
//
// =============================================================================
INT
TOR_LIST_OLD::Count(void)
{
  TAG_OBJ_REP *tor;
  TOR_LIST_OLD_ITER tor_list_iter;
  INT count = 0;
  FOR_ALL_ELEM(tor, tor_list_iter, Init(this)) {
    ++count;
  }
  return count;
}

// =============================================================================
//
// TOR_LIST_OLD::Find, find tor in list
//   if founded when there is same tor or tor that have same tag id
//
// =============================================================================
TAG_OBJ_REP *
TOR_LIST_OLD::Find(TAG_OBJ_REP *tor)
{
  TAG_OBJ_REP *cur_tor;
  TOR_LIST_OLD_ITER iter;
  FOR_ALL_ELEM(cur_tor, iter, Init(this)) {
    if (cur_tor == tor) {
      return cur_tor;
    } else if (tor->Tag_base()->Id() == cur_tor->Tag_base()->Id()) {
      return cur_tor;
    }
  }
  return NULL;
}

// =============================================================================
//
// TOR_LIST_OLD::Find, find tor in list, find by tag id
//
// =============================================================================
TAG_OBJ_REP *
TOR_LIST_OLD::Find(IDTYPE tag_id)
{
  TAG_OBJ_REP *cur_tor;
  TOR_LIST_OLD_ITER iter;
  FOR_ALL_ELEM(cur_tor, iter, Init(this)) {
    if (tag_id == cur_tor->Tag_base()->Id()) {
      return cur_tor;
    }
  }
  return NULL;
}

// =============================================================================
//
// TOR_LIST_OLD::Print, print the tor list
//
// =============================================================================
void
TOR_LIST_OLD::Print(FILE *fp, BOOL detail)
{
  TAG_OBJ_REP *tor;
  TOR_LIST_OLD_ITER tor_list_iter;
  BOOL first = TRUE;
  FOR_ALL_ELEM(tor, tor_list_iter, Init(this)) {
    if (first) {
      fprintf(fp, "[");
      first = FALSE;
    } else {
      fprintf(fp, "|");
    }
    tor->Print(fp, detail);
  }
  if (!first)
    fprintf(fp, "]");
}

// =============================================================================
//
// TAG_OBJ_REP::Push_phi, push phi opnd into phi_list
//
// =============================================================================
BOOL
TAG_OBJ_REP::Push_phi(TAG_OBJ_REP *tor, MEM_POOL *pool)
{
  Is_True(_def_attr == TO_DEF_BY_PHI, ("Def attr is not def by phi, def attr : %d", _def_attr));

  if (!_phi_list) {
    _phi_list = CXX_NEW(TOR_VEC(mempool_allocator<TAG_OBJ_REP*>(pool)), pool);
    _phi_list->push_back(tor);
    return TRUE;
  } else {
    for(int i = 0; i < _phi_list->size(); i++) {
      TAG_OBJ_REP *cur_tor = _phi_list->at(i);
      if (cur_tor == tor) {
        return FALSE;
      }
    }
    _phi_list->push_back(tor);
  }
  return TRUE;
}

// =============================================================================
//
// TAG_OBJ_REP::Def_attr_str, string value for tag def attribute
//
// =============================================================================
const char*
TAG_OBJ_REP::Def_attr_str() const
{
  switch(_def_attr) {
    case TO_DEF_BY_DEFAULT:
      return "default";
    break;
    case TO_DEF_BY_PHI:
      return "phi";
    break;
    case TO_DEF_BY_CHI:
      return "chi";
    break;
    case TO_DEF_BY_SE:
      return "eval";
    break;
    case TO_DEF_BY_COPY:
      return "copy";
    break;
    case TO_DEF_BY_CREATE:
      return "create";
    break;
    case TO_DEF_BY_ENTRY_CHI:
      return "e-chi";
    break;
    case TO_DEF_BY_TAG_ATTR:
      return "d-attr";
    break;
    default:
      return "invalid";
  }
}

// =============================================================================
//
// TAG_OBJ_REP::Print, Dump TAG_OBJ_REP
//
// =============================================================================
void
TAG_OBJ_REP::Print(FILE *fp, BOOL detail) const
{
  fprintf(fp, "tor%d(", _id);
  switch(Obj_kind()) {
  case TAG_KIND_VAR:
    fprintf(fp, "cr%d", Coderep()->Coderep_id());
    break;
  case TAG_KIND_HOR:
    fprintf(fp, "ho%dv%d",
            Heapobjrep()->Heap_obj() ? Heapobjrep()->Heap_obj()->Id() : -1,
            Heapobjrep()->Version());
    break;
  case TAG_KIND_VOR:
    fprintf(fp, "vo%dv%d", Vsymobjrep()->Vsym_obj()->Id(),
            Vsymobjrep()->Version());
    break;
  default:
    Is_True(FALSE, ("unknown tag_obj_rep kind %d", Obj_kind()));
    break;
  }
  fprintf(fp, ")(%s,attr:", _tag_base->Tag_name());
  BOOL first = TRUE;
  if(_tag_attrs.all()) {
    fprintf(fp,"%d-%d", TAG_START_ID,TAG_ATTR_DEFAULT_NUM);
  } else {
    for (INT32 i = TAG_START_ID; i < _tag_attrs.size(); i++) {
      if (_tag_attrs[i]) {
        if (!first)
          fprintf(fp, ",");
        fprintf(fp, "%d", i);
        first = FALSE;
      }
    }
  }
  fprintf(fp, ")");
  const char *def_str = Def_attr_str();
  fprintf(fp, "(%s)", def_str);
  if(detail) {
    if (Def_attr() == TO_DEF_BY_COPY || Def_attr() == TO_DEF_BY_TAG_ATTR) {
      TAG_OBJ_REP *deftor = Deftor();
      if (deftor != NULL) {
        fprintf(fp, "->tor%d", deftor->Id());
      }
    } else if (Def_attr() == TO_DEF_BY_PHI && Phi_list()) {
      fprintf(fp, "->");
      for (int idx = 0; idx < Phi_list()->size(); idx++) {
        fprintf(fp, "tor%d ", Phi_list()->at(idx)->Id());
      }
    }
  }
}

// =============================================================================
//
// TAG_OBJ_REP::Set_const_side_effect, set tag and attribute const side effect
//
// =============================================================================
void
TAG_OBJ_REP::Set_const_side_effect(RBC_ENGINE::TAG_DEF_VAL tag_def_val)
{
  Set_def_attr(TO_DEF_BY_CREATE);
  // if tag is unset, we set all attribute to 1
  // i.e.: tainted unset, we treate as tainted data are sanitized
  if (tag_def_val == RBC_ENGINE::TAG_UNSET) {
    Set_tag_attrs();
  } else if (tag_def_val == RBC_ENGINE::TAG_SET) {
    TAG_ATTRS &attrs = Get_tag_attrs();
    attrs.reset();
  }
}

// =============================================================================
//
// VSA::Find_tag_base, cannot be defined in the header file
//
// =============================================================================
TAG_BASE*
VSA::Find_tag_base( STRING tag_name ) const
{
  return Ipsa()->Rbc()->Find_tag_base(tag_name);
}

// =============================================================================
//
// VSA::Find_tor_list_from_cr, search from mu list
//   search order: vor, hor, cr
//
// =============================================================================
TOR_LIST_OLD*
VSA::Find_tor_list_from_cr(STMTREP *sr, CODEREP *cr, TAGOKIND &kind, BOOL mu) const
{
  Is_True_Ret(cr, ("Cr is NULL."), NULL);
  if (cr->Kind() == CK_IVAR && cr->Opr() == OPR_PARM)
    cr = cr->Ilod_base();
  if (cr->Kind() == CK_OP && cr->Opr() == OPR_CVT)
    cr = cr->Opnd(0);
  TOR_LIST_OLD *tor_list = NULL;
  // find vor from cr_2_vor map
  VSYM_OBJ_REP *vor = Cr_2_vor(cr);
  if (!vor && sr) {
    VS_FLD_KIND vf_kind = Get_vfr_kind(cr);
    VSYM_FLD_REP vsym_fld_rep(vf_kind, 0, 0);
    if (mu) {
      HEAP_OBJ_REP *hor = Cr_2_heap_obj(cr);
      if (hor) {
        vor = Find_hor_mu_vor(sr, hor, &vsym_fld_rep, cr);
      }
    }
    else
      vor = Find_vor_chi_vor(sr, cr, &vsym_fld_rep);
  }
  if (vor && vor->Tor_list<TOR_LIST_OLD>()) {
    tor_list = vor->Tor_list<TOR_LIST_OLD>();
    if (tor_list) {
      kind = TAG_KIND_VOR;
      return tor_list;
    }
  }
  HEAP_OBJ_REP *hor = Cr_2_heap_obj(cr);
  if (hor && hor->Tor_list()) {
    tor_list = hor->Tor_list();
    if (tor_list) {
      kind = TAG_KIND_HOR;
      return tor_list;
    }
  }
  if (cr->Kind() == CK_IVAR && cr->Opr() == OPR_PARM) {
    cr = cr->Ilod_base();
  }
  tor_list = Cr_2_tor_list<TOR_LIST_OLD>(cr);
  if (tor_list) {
    kind = TAG_KIND_VAR;
    return tor_list;
  }
  if(cr->Kind() == CK_IVAR) {
    CODEREP *base = (cr == sr->Lhs()) ? cr->Istr_base() : cr->Ilod_base();
    CODEREP *base_cr = Find_ilod_base(base);
    if(base_cr != NULL) {
      return Find_tor_list_from_cr(sr, base_cr, kind, mu);
    }
  }
  return NULL;
}

// =============================================================================
//
// VSA::Find_tgt_and_merge, find the target, and merge target with source
//
// =============================================================================
BOOL
VSA::Find_tgt_and_merge(STMTREP *sr, CODEREP *cr, TOR_LIST_OLD *src)
{
  BOOL ret = FALSE;
  VSYM_OBJ_REP *vor = Cr_2_vor(cr);
  if (!vor) {
    VS_FLD_KIND vf_kind = Get_vfr_kind(cr);
    VSYM_FLD_REP vsym_fld_rep(vf_kind, 0, 0);
    vor = Find_vor_chi_vor(sr, cr, &vsym_fld_rep);
  }
  if (vor) {
    if (!vor->Tor_list<TOR_LIST_OLD>())
      Enter_ror_tor_list(vor, CXX_NEW(TOR_LIST_OLD, Mem_pool()));
    ret = Merge_tor_list(sr, vor, vor->Tor_list<TOR_LIST_OLD>(), src);
  } else {
    if (!Cr_2_tor_list<TOR_LIST_OLD>(cr))
      Enter_ror_tor_list(cr, CXX_NEW(TOR_LIST_OLD, Mem_pool()));
    ret = Merge_tor_list(sr, cr, Cr_2_tor_list<TOR_LIST_OLD>(cr), src);
  }
  return ret;
}

// =============================================================================
//
// VSA::Create_tor_with_def_attr, create tor with tag def attr
//    different kind of def attr need to set different field
//
// =============================================================================
template<typename TAGOBJRP> TAG_OBJ_REP*
VSA::Create_tor_with_def_attr(TOR_DEF_ATTR def_attr, TAG_OBJ_REP *src_tor, TAGOBJRP tagobjr, STMTREP *defstmt, IDTYPE tag_attr_id)
{
  TAG_OBJ_REP *nw_tor = Allocate_tag_obj(src_tor->Tag_base(), tagobjr, defstmt);
  nw_tor->Set_def_attr(def_attr);
  if (def_attr == TO_DEF_BY_COPY) {
    nw_tor->Set_deftor(src_tor);
    nw_tor->Clone_tag_attrs(src_tor->Get_tag_attrs());
  } else if (def_attr == TO_DEF_BY_TAG_ATTR) {
    nw_tor->Set_deftor(src_tor);
    nw_tor->Set_tag_attr(tag_attr_id);
  } else if (def_attr == TO_DEF_BY_SE || def_attr == TO_DEF_BY_CHI) {
    // do nothing
  } else if (def_attr == TO_DEF_BY_PHI) {
    nw_tor->Push_phi(src_tor, Mem_pool());
  } else {
    Is_True(FALSE, ("Don't support this def attr, def attr : %d", def_attr));
  }
  return nw_tor;
}

// =============================================================================
//
// VSA::Create_tor_with_def_attr, create tor with tag def attr
//    different kind of def attr need to set different field
//
// =============================================================================
template<typename TAGOBJRP> BOOL
VSA::Update_tor_with_def_attr(TOR_DEF_ATTR def_attr, TAG_OBJ_REP *tgt_tor, TAG_OBJ_REP *src_tor,
                                TAGOBJRP tagobjr, STMTREP *defstmt, IDTYPE tag_attr_id)
{
  Is_True(tgt_tor != NULL, ("Target tor is NULL."));
  if (tgt_tor == src_tor) {
    // same tor, do nothing
    return FALSE;
  }
  // onced set those 3 attr, can't be changed to another attribute
  if (tgt_tor->Def_attr() == TO_DEF_BY_SE) {
    // do nothing, can't override tag that is created by se
    return FALSE;
  } else if (tgt_tor->Def_attr() == TO_DEF_BY_CREATE) {
    // do nothing, can't override tag that is created by create
    return FALSE;
  } else if (tgt_tor->Def_attr() == TO_DEF_BY_TAG_ATTR) {
    if (!tgt_tor->Is_set_tag_attr(tag_attr_id)) {
      tgt_tor->Set_def_attr(def_attr);
      return TRUE;
    }
    return FALSE;
  } else if (tgt_tor->Def_attr() == TO_DEF_BY_PHI) {
    return tgt_tor->Push_phi(src_tor, Mem_pool());
  }
  Is_Trace(Tracing(), (TFile, "VSA::Update_tor_with_def_attr: "));
  Is_Trace_cmd(Tracing(), tgt_tor->Print(TFile));
  BOOL changed = FALSE;
  if (def_attr == TO_DEF_BY_SE) {
    if (tgt_tor->Def_attr() != TO_DEF_BY_SE) {
      tgt_tor->Set_def_attr(def_attr);
      tgt_tor->Set_deftor(NULL);
      changed = TRUE;
    }
  } else if (def_attr == TO_DEF_BY_CREATE) {
    if (tgt_tor->Def_attr() != TO_DEF_BY_CREATE) {
      tgt_tor->Set_def_attr(def_attr);
      changed = TRUE;
    }
  } else if (def_attr == TO_DEF_BY_TAG_ATTR) {
    if (tgt_tor->Def_attr() != def_attr) {
      tgt_tor->Set_def_attr(def_attr);
      tgt_tor->Set_deftor(src_tor);
      tgt_tor->Set_tag_attr(tag_attr_id);
      changed = TRUE;
    } else if (tgt_tor->Deftor() != src_tor) {
      tgt_tor->Set_deftor(src_tor);
      tgt_tor->Set_tag_attr(tag_attr_id);
      changed = TRUE;
    } else if (!tgt_tor->Is_set_tag_attr(tag_attr_id)) {
      tgt_tor->Set_tag_attr(tag_attr_id);
      changed = TRUE;
    }
  } else if (def_attr == TO_DEF_BY_PHI) {
    if (tgt_tor->Def_attr() != def_attr) {
      TAG_OBJ_REP *deftor = (tgt_tor->Def_attr() == TO_DEF_BY_COPY ||
                             tgt_tor->Def_attr() == TO_DEF_BY_TAG_ATTR) ?
                             tgt_tor->Deftor() : NULL;
      tgt_tor->Set_deftor(NULL);
      tgt_tor->Set_def_attr(def_attr);
      tgt_tor->Push_phi(src_tor, Mem_pool());
      // push old deftor to phi list
      if(deftor) {
        tgt_tor->Push_phi(deftor, Mem_pool());
      }
      changed = TRUE;
    } else {
      changed = tgt_tor->Push_phi(src_tor, Mem_pool());
    }
  }
  else if (def_attr == TO_DEF_BY_COPY) {
    // It's quite possible that updating tgt_tor's def_tor to src_tor
    // will lead to TO_DEF_BY_COPY recursion due to ho tor & cr tor handling issue,
    // which will cause infinite loop in check phase.
    // Need review ho tor & cr tor design later.
    TAG_OBJ_REP *src_deftor = (src_tor->Def_attr() == TO_DEF_BY_COPY ||
                               src_tor->Def_attr() == TO_DEF_BY_TAG_ATTR) ?
      src_tor->Deftor() : NULL;
    BOOL recursion = FALSE;
    while (src_deftor != NULL) {
      if (src_deftor == tgt_tor) {
        recursion = TRUE;
        break;
      }
      src_deftor = (src_deftor->Def_attr() == TO_DEF_BY_COPY ||
                    src_deftor->Def_attr() == TO_DEF_BY_TAG_ATTR) ?
        src_deftor->Deftor() : NULL;
    }
    if (!recursion) {
      if (tgt_tor->Def_attr() != def_attr) {
        tgt_tor->Set_def_attr(def_attr);
        tgt_tor->Set_deftor(src_tor);
        changed = TRUE;
      } else if (tgt_tor->Deftor() != src_tor) {
        tgt_tor->Set_deftor(src_tor);
        changed = TRUE;
      }
    }
  }
  if (changed) {
    Is_Trace(Tracing(), (TFile, " => "));
    Is_Trace_cmd(Tracing(), tgt_tor->Print(TFile));
    Is_Trace(Tracing(), (TFile, "\n"));
  }
  else {
    Is_Trace(Tracing(), (TFile, " no changed\n"));
  }
  return changed;
}

static inline BOOL
Check_binding(TOR_DEF_ATTR def_attr, TAG_OBJ_REP *tgt, TAG_OBJ_REP *src)
{
  Is_True_Ret(tgt != NULL, ("Target tor is NULL."), FALSE);
  if (def_attr == TO_DEF_BY_COPY) {
    return tgt->Def_attr() != def_attr || tgt->Deftor() != src;
  } else if (def_attr == TO_DEF_BY_TAG_ATTR) {
    return tgt->Def_attr() != def_attr || tgt->Deftor() != src;
  } else if (def_attr == TO_DEF_BY_SE || def_attr == TO_DEF_BY_CHI) {
    return tgt->Def_attr() != def_attr;
  } else {
    Is_True_Ret(FALSE, ("Did not support this def attr, def attr : %d", def_attr), FALSE);
  }
  return FALSE;
}

// =============================================================================
//
// VSA::Bind_tor_list_to_cr, bind to cr list
//    TODO: need a new version cr here, if don't have new version cr
//          we just cover the original cr now
//
// =============================================================================
BOOL
VSA::Bind_tor_list_to_cr(STMTREP *sr, CODEREP *cr, TOR_LIST_OLD *src_tor_list,
                         TOR_DEF_ATTR attr, TAG_BASE *tag_base, IDTYPE attr_id)
{
  Is_True_Ret(cr, ("Cr is NULL."), FALSE);
  BOOL ret = FALSE;
  if (!src_tor_list)
    return ret;

  // TODO: confirm if LDA is handled correctly
  //Is_True_Ret(cr->Kind() == CK_VAR || cr->Kind() == CK_IVAR,
  //            ("not VAR or IVAR"), FALSE);

  // try vor at first
  if (Cr_2_vor(cr) != NULL || cr->Kind() != CK_VAR)
    ret = Bind_tor_list_to_vor(sr, cr, src_tor_list, attr, tag_base, attr_id);

  // don't bind tor list to IVAR cr since IVAR cr doesn't have U-D
  if (cr->Kind() != CK_VAR)
    return ret;

  TAG_OBJ_REP *tor;
  TOR_LIST_OLD_ITER iter;
  FOR_ALL_ELEM(tor, iter, Init(src_tor_list)) {
    // filter tor with same tag base
    if (tag_base && tor->Tag_base() != tag_base)
      continue;
    TAG_OBJ_REP *tgt_tor = NULL;
    TAG_OBJ_REP *nw_tor = NULL;
    TOR_LIST_OLD *tor_list = Ror_2_tor_list<CODEREP*, TOR_LIST_OLD>(cr);
    if (tor_list) {
      tgt_tor = tor_list->Find(tor);
    }
    if (tgt_tor) {
      BOOL bind = Check_binding(attr, tgt_tor, tor);
      if (bind) {
        Update_tor_with_def_attr(attr, tgt_tor, tor, cr, sr, attr_id);
      } // bind
    } else {
      TAG_OBJ_REP *nw_tor = Create_tor_with_def_attr(attr, tor, cr, sr, attr_id);
      Enter_tor_ror(cr, nw_tor);
      ret = TRUE;
    } // tgt_tor
  }
  return ret;
}

// =============================================================================
//
// VSA::Bind_tor_list_to_vor, bind to vor chi list
//
// =============================================================================
BOOL
VSA::Bind_tor_list_to_vor(STMTREP *sr, CODEREP *cr, TOR_LIST_OLD *src_tor_list,
                          TOR_DEF_ATTR attr, TAG_BASE *tag_base, IDTYPE attr_id)
{
  Is_True(src_tor_list != NULL, ("src tor list is null"));

  VSYM_OBJ_REP *vor = Cr_2_vor(cr);
  if (!vor) {
    VS_FLD_KIND vf_kind = Get_vfr_kind(cr);
    VSYM_FLD_REP vsym_fld_rep(vf_kind, 0, 0);
    vor = Find_vor_chi_vor(sr, cr, &vsym_fld_rep);
  }
  if (vor && !Is_special_vor(vor)) {
    HEAP_OBJ_REP *base_hor = vor->Vsym_obj()->Base_hor();
    TAG_OBJ_REP *tor;
    TOR_LIST_OLD_ITER iter;
    FOR_ALL_ELEM(tor, iter, Init(src_tor_list)) {
      // filter tor with same tag base
      if (tag_base && tor->Tag_base() != tag_base)
        continue;
      TAG_OBJ_REP *tgt_tor = NULL;
      TOR_LIST_OLD *tor_list = Ror_2_tor_list<VSYM_OBJ_REP*, TOR_LIST_OLD>(vor);
      if (tor_list) {
        tgt_tor = tor_list->Find(tor);
      }
      if (tgt_tor) {
        BOOL bind = Check_binding(attr, tgt_tor, tor);
        if (bind) {
          Update_tor_with_def_attr(attr, tgt_tor, tor, vor, sr, attr_id);
          TOR_LIST_OLD *hor_tor_list = Ror_2_tor_list<HEAP_OBJ_REP*, TOR_LIST_OLD>(base_hor);
          if (hor_tor_list) {
            TAG_OBJ_REP *hor_tor = hor_tor_list->Find(tor);
            if (hor_tor) {
              Update_tor_with_def_attr(attr, tgt_tor, tor, base_hor, sr, attr_id);
            }
          }
        } // bind
      } else {
        TAG_OBJ_REP *nw_vor_tor = Create_tor_with_def_attr(attr, tor, vor, sr, attr_id);
        Enter_tor_ror(vor, nw_vor_tor);
        TAG_OBJ_REP *nw_hor_tor = Create_tor_with_def_attr(attr, tor, base_hor, sr, attr_id);
        Enter_tor_ror(base_hor, nw_hor_tor);
        nw_hor_tor->Set_hor_cr(cr);
      } // tgt_ror
    } // for all src_tor_list
    return TRUE;
  } // if(vor)
  return FALSE;
}

// =============================================================================
//
// VSA::Propagate_tag_insts_stmt propagate tag in stmt level
//
// =============================================================================
void
VSA::Propagate_tag_insts_stmt(STMTREP *stmt, BB_NODE *bb, MEM_POOL *def_bbs_pool)
{
  OPERATOR opr = stmt->Opr();
  
  TOR_LIST_OLD *tor_list = NULL;
  if (stmt->Rhs()) {
    TAGOKIND tag_kind;
    tor_list = Find_tor_list_from_cr(stmt, stmt->Rhs(), tag_kind);
  }
  switch(opr) {
  case OPR_STID: {
    if (tor_list)
      Bind_tor_list_to_cr(stmt, stmt->Lhs(), tor_list, TO_DEF_BY_COPY);
    if (VSA_Enable_Chi_Prop_Tag) {
      CODEREP *lhs = stmt->Lhs();
      AUX_ID lhs_st_idx = lhs->Aux_id();
      TOR_LIST_OLD *lhs_tor_list = Ror_2_tor_list<CODEREP*, TOR_LIST_OLD>(lhs);
      CHI_NODE *cnode;
      CHI_LIST_ITER chi_iter;
      FOR_ALL_NODE(cnode, chi_iter, Init(stmt->Chi_list())) {
        if (!cnode->Live())
          continue;
        AUX_ID chi_st_idx = cnode->Aux_id();
        CODEREP *opnd = cnode->OPND();
        TOR_LIST_OLD *opnd_tor_list = Ror_2_tor_list<CODEREP*, TOR_LIST_OLD>(opnd);
        CODEREP *res = cnode->RESULT();
        TOR_LIST_OLD *res_tor_list = NULL;
        TAG_OBJ_REP *tor;
        TOR_LIST_OLD_ITER iter;
        // 1. propagate tor from chi opnd to chi result,
        //    only when chi result does not have a tor of the same base
        FOR_ALL_ELEM(tor, iter, Init(opnd_tor_list)) {
          res_tor_list = Ror_2_tor_list<CODEREP*, TOR_LIST_OLD>(res);
          TAG_OBJ_REP *orig_res_tor = NULL;
          if (res_tor_list != NULL)
            orig_res_tor = res_tor_list->Find(tor);
          if (orig_res_tor == NULL) {
            TAG_OBJ_REP *chi_tor = Create_tor_with_def_attr(TO_DEF_BY_COPY, tor, res, stmt, TAG_INVALID_ID);
            Enter_tor_ror(res, chi_tor);
            Is_Trace(Tracing(), (TFile, "Create CHI RESULT(cr%d) : ", res->Coderep_id()));
            Is_Trace_cmd(Tracing(), chi_tor->Print(TFile, TRUE));
            Is_Trace(Tracing(), (TFile, " from CHI OPND(cr%d) : ", opnd->Coderep_id()));
            Is_Trace_cmd(Tracing(), tor->Print(TFile, TRUE));
            Is_Trace(Tracing(), (TFile, "\n"));
          }
        }
        // 2. update/create chi result tor with lhs tor
        AUX_ID cur_idx = Opt_stab()->St_group(lhs_st_idx);
        while (cur_idx != 0 && cur_idx != lhs_st_idx) {
          if (cur_idx == chi_st_idx) {
            FOR_ALL_ELEM(tor, iter, Init(lhs_tor_list)) {
              res_tor_list = Ror_2_tor_list<CODEREP*, TOR_LIST_OLD>(res);
              TAG_OBJ_REP *orig_res_tor = NULL;
              if (res_tor_list != NULL)
                orig_res_tor = res_tor_list->Find(tor);
              if (orig_res_tor == NULL) {
                TAG_OBJ_REP *chi_tor = Create_tor_with_def_attr(TO_DEF_BY_COPY, tor, res, stmt, TAG_INVALID_ID);
                Enter_tor_ror(res, chi_tor);
                Is_Trace(Tracing(), (TFile, "Create CHI RESULT(cr%d) : ", res->Coderep_id()));
                Is_Trace_cmd(Tracing(), chi_tor->Print(TFile, TRUE));
                Is_Trace(Tracing(), (TFile, " from assign lhs(cr%d) : ", lhs->Coderep_id()));
                Is_Trace_cmd(Tracing(), tor->Print(TFile, TRUE));
                Is_Trace(Tracing(), (TFile, "\n"));
              }
              else {
                Update_tor_with_def_attr(TO_DEF_BY_COPY, orig_res_tor, tor, res, stmt, TAG_INVALID_ID);
              }
            }
          }
          cur_idx = Opt_stab()->St_group(cur_idx);
        } // while cur_idx
      } // FOR_ALL cnode
    }
    return;
  }
  case OPR_ISTORE: {
    if (tor_list) {
      Bind_tor_list_to_cr(stmt, stmt->Lhs(), tor_list, TO_DEF_BY_COPY);
    }
    return;
  }
  default:
    break;
  }
  if (!OPERATOR_is_call(opr)) {
    if (tor_list && stmt->Lhs()) {
      Bind_tor_list_to_cr(stmt, stmt->Lhs(), tor_list, TO_DEF_BY_COPY);
    }
    return;
  }
  // propagate tag for intrinsic, check_cast etc...
  if (opr == OPR_INTRINSIC_CALL) {
    INTRINSIC intrn = stmt->Rhs()->Intrinsic();
    if (intrn == INTRN_CHECK_CAST) {
      TAGOKIND tag_kind;
      TOR_LIST_OLD *tor_list = Find_tor_list_from_cr(stmt, stmt->Rhs()->Opnd(1)->Ilod_base(), tag_kind);
      if (tor_list) {
        CODEREP *ret_cr = Comp_unit()->Find_return_value(stmt);
        // ret value may be optimized out
        if(ret_cr) {
          Bind_tor_list_to_cr(stmt, ret_cr, tor_list, TO_DEF_BY_COPY);
          // propagate tag from tor_list to vor chi based on ret_cr
          if (Cr_2_vor(ret_cr) == NULL) {
            Bind_tor_list_to_vor(stmt, ret_cr, tor_list, TO_DEF_BY_COPY);
          }
        }
      }
    }
  }
  // handle call
  {
    RNA_NODE *rna = Dna()->Get_callsite_rna(stmt);
    if (rna->Is_flag_set(RBC_SE_TAG_OP)) {
      Ipsa()->Rbc()->Eval__mvsa_tag_op(Dna(), rna, def_bbs_pool);
    } else {
      // default propagation
      const CALLEE_VECTOR& callee_list = rna->Callee_list();
      for (CALLEE_VECTOR::const_iterator iter = callee_list.begin(); iter != callee_list.end(); iter++) {
        DNA_NODE *callee = Ipsa()->Get_dna(iter->Callee());
        if (callee == NULL || callee->Non_functional()) {
          continue;
        }
        // propagate tag by callee side-effect
        // [ step 1]: propgate return value
        CODEREP *ret_cr = Comp_unit()->Find_return_value(stmt);
        if(ret_cr) {
          INT32 ofst = ret_cr->Offset();
          for (INT i = VAR_INIT_ID; i < callee->Retv_list()->size(); ++ i) {
            PDV_NODE *pdv = (*callee->Retv_list())[i];
            STMTREP* sr = pdv->Stmt();
            if (pdv->Kind() == BY_RETURNSTMT && pdv->Oparam() == 0 &&
                sr->Lhs()->Offset() == ofst) {
              Is_True(sr->Opr() == OPR_STID, ("bad stmt"));
              Is_True(sr->Next() != NULL && sr->Next()->Opr() == OPR_RETURN,
                      ("bad return stmt"));
              CODEREP *callee_ret = sr->Lhs();
              TAGOKIND tag_kind;
              TOR_LIST_OLD *tor_list = callee->Comp_unit()->Vsa()->Find_tor_list_from_cr(sr, callee_ret, tag_kind);
              if(tor_list) {
                Bind_tor_list_to_cr(stmt, ret_cr, tor_list, TO_DEF_BY_CHI);
              }
            }
          } // for(retv_list)
        } // if(ret_cr)
        // [ step 2]: propagate output parm
        // [ step 3]: propagate global
      }
    }
    // propagate call, don't have callee; if have return, propagate to return value
    // otherwise, if first arg is pointer type, then propagate to first arg
    CODEREP *cr = stmt->Rhs();
    if (opr != OPR_INTRINSIC_CALL && rna->Callee_cnt() == 0 && VSA_Enable_Prop_Tag) {
      CODEREP *ret_cr = Comp_unit()->Find_return_value(stmt);
      // bind to return
      if (ret_cr) {
        for (INT i = 0; i < cr->Kid_count(); i++) {
          TAGOKIND tag_kind;
          TOR_LIST_OLD *tor_list = Find_tor_list_from_cr(stmt, cr->Opnd(i), tag_kind);
          if (tor_list) {
            Bind_tor_list_to_cr(stmt, ret_cr, tor_list, TO_DEF_BY_COPY);
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
            TAGOKIND tag_kind;
            TOR_LIST_OLD *tor_list = Find_tor_list_from_cr(stmt, cr->Opnd(i), tag_kind);
            if (tor_list) {
              Bind_tor_list_to_cr(stmt, first_arg, tor_list, TO_DEF_BY_COPY);
              break;
            }
          }
        }
      }
    }
  }
}

// =============================================================================
//
// VSA::Merge_tor_list, merge two list, then bind to target
//
// =============================================================================
template<typename RSCOBJP> BOOL
VSA::Merge_tor_list(STMTREP *sr, RSCOBJP ror, TOR_LIST_OLD *dst, TOR_LIST_OLD *src, BOOL for_phi)
{
  Is_True_Ret(dst, ("Dst to list is NULL."), FALSE);
  Is_True_Ret(src, ("Src to list is NULL."), FALSE);
  Is_Trace(Tracing(), (TFile, "Merge tor for sr(%d): Src:", sr ? sr->Stmtrep_id() : -1));
  Is_Trace_cmd(Tracing(), src->Print(TFile, TRUE));
  Is_Trace(Tracing(), (TFile, " -> Dst:"));
  Is_Trace_cmd(Tracing(), dst->Print(TFile, TRUE));
  Is_Trace(Tracing(), (TFile, "\n"));
  if(src == dst) {
    return FALSE;
  }
  BOOL changed = FALSE;
  TAG_OBJ_REP *src_tor;
  TOR_LIST_OLD_ITER src_iter;
  TAG_OBJ_REP *dst_tor;
  TOR_LIST_OLD_ITER dst_iter;
  // dst list is empty, just copy from src list
  if (dst->Len() == 0) {
    FOR_ALL_ELEM(src_tor, src_iter, Init(src)) {
      TAG_OBJ_REP *nw_tor = Create_tor_with_def_attr(TO_DEF_BY_PHI, src_tor, ror, sr);
      dst->Append(nw_tor);
    }
    changed = TRUE;
  } else {
    FOR_ALL_ELEM(src_tor, src_iter, Init(src)) {
      TAG_OBJ_REP *tgt_tor = dst->Find(src_tor);
      if (!tgt_tor) {
        TAG_OBJ_REP *tgt_tor = Create_tor_with_def_attr(TO_DEF_BY_PHI, src_tor, ror, sr);
        dst->Append(tgt_tor);
        changed = TRUE;
      } else {
        changed = Update_tor_with_def_attr(TO_DEF_BY_PHI, tgt_tor, src_tor, ror, sr);
      }
    }
  }
  Is_Trace(Tracing(), (TFile, "Merged Result:"));
  Is_Trace_cmd(Tracing(), dst->Print(TFile, TRUE));
  Is_Trace(Tracing(), (TFile, "\n\n"));
  return changed;
}

// =============================================================================
//
// VSA::Merge_bb_to_phi, merge phi node
//
// =============================================================================
BOOL
VSA::Merge_bb_to_phi(BB_NODE *bb)
{
  PHI_LIST_ITER phi_iter;
  PHI_NODE *phi;
  BOOL merge = FALSE;

  // merge to from cr
  FOR_ALL_NODE(phi, phi_iter, Init(bb->Phi_list())) {
    if (!phi->Live() || !phi->Res_is_cr())
      continue;
    TOR_LIST_OLD *result_tor_list = Cr_2_tor_list<TOR_LIST_OLD>(phi->RESULT());
    for (INT i = 0; i < phi->Size(); i++) {
      TAGOKIND tag_kind;
      TOR_LIST_OLD *opnd_tor_list = Find_tor_list_from_cr(NULL, phi->OPND(i), tag_kind);
      if (opnd_tor_list) {
        Is_Trace(Tracing(), (TFile, "--Merge with phi opnd[%d]\n", i));
        Is_Trace_cmd(Tracing(), phi->Print(TFile));
        if (!result_tor_list) {
          result_tor_list = CXX_NEW(TOR_LIST_OLD, Mem_pool());
          Enter_ror_tor_list(phi->RESULT(), result_tor_list);
        }
        if(opnd_tor_list != result_tor_list) {
          merge = Merge_tor_list(NULL, phi->RESULT(), result_tor_list, opnd_tor_list, TRUE) | merge;
        }
      }
    }
  }
  // merge to from hor
  FOR_ALL_NODE(phi, phi_iter, Init(Bb_ho_philist(bb))) {
    TOR_LIST_OLD *result_tor_list = ((HEAP_OBJ_REP *) phi->RESULT())->Tor_list();
    for (INT i = 0; i < phi->Size(); i++) {
      HEAP_OBJ_REP *opnd_hor = (HEAP_OBJ_REP *) phi->OPND(i);
      if (opnd_hor->Tor_list()) {
        Is_Trace(Tracing(), (TFile, "--Merge with phi ho opnd[%d]\n", i));
        Is_Trace_cmd(Tracing(), Print_hor_phi(phi, TFile));
        if (!result_tor_list) {
          result_tor_list = CXX_NEW(TOR_LIST_OLD, Mem_pool());
          Enter_ror_tor_list((HEAP_OBJ_REP *) phi->RESULT(), result_tor_list);
        }
        if(opnd_hor->Tor_list() != result_tor_list) {
          merge = Merge_tor_list(NULL, (HEAP_OBJ_REP *) phi->RESULT(), result_tor_list, opnd_hor->Tor_list(), TRUE) | merge;
        }
      }
    }
  }
  // merge to from vor
  FOR_ALL_NODE(phi, phi_iter, Init(Bb_vo_philist(bb))) {
    TOR_LIST_OLD *result_tor_list = ((VSYM_OBJ_REP *) phi->RESULT())->Tor_list<TOR_LIST_OLD>();
    for (INT i = 0; i < phi->Size(); i++) {
      VSYM_OBJ_REP *opnd_vor = (VSYM_OBJ_REP *) phi->OPND(i);
      if (opnd_vor->Tor_list<TOR_LIST_OLD>()) {
        Is_Trace(Tracing(), (TFile, "--Merge with phi vo opnd[%d]\n", i));
        Is_Trace_cmd(Tracing(), Print_vor_phi(phi, TFile));
        if (!result_tor_list) {
          result_tor_list = CXX_NEW(TOR_LIST_OLD, Mem_pool());
          Enter_ror_tor_list((VSYM_OBJ_REP *) phi->RESULT(), result_tor_list);
        }
        if(opnd_vor->Tor_list<TOR_LIST_OLD>() != result_tor_list) {
          merge = Merge_tor_list(NULL, (VSYM_OBJ_REP *) phi->RESULT(), result_tor_list, opnd_vor->Tor_list<TOR_LIST_OLD>(), TRUE) | merge;
        }
      }
    }
  }
  return merge;
}

// =============================================================================
//
// VSA::Propagate_tag_insts, create tag when needed, then do forward propagation
//
// =============================================================================
void
VSA::Propagate_tag_insts(BB_NODE *bb, MEM_POOL *def_bbs_pool, BOOL &changed)
{
  Is_Trace(Tracing(),
           (TFile, "%sVSA::Propagate_tag_insts BB=%d\n", SBar, bb->Id()));
  // merge phi tag obj first
  if (Merge_bb_to_phi(bb))
    changed = TRUE;

  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {
    Propagate_tag_insts_stmt(stmt, bb, def_bbs_pool);
  }

  // identify tag for donminated bbs
  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb->Dom_bbs())) {
    Propagate_tag_insts(dom_bb, def_bbs_pool, changed);
  }
}

// =============================================================================
//
// VSA::Create_entry_tor_chi, create entry tor, for entry chi vor, cr
//
// =============================================================================
void
VSA::Create_entrychi_tor()
{
  STMTREP *stmt = Get_entry_chi_stmt();
  Is_True_Ret(stmt && stmt->Opr() == OPR_OPT_CHI, ("Can't find entry chi in entry bb."));
  TB_LIST *tag_base_list = Ipsa()->Rbc()->Tag_base_list();
  CHI_NODE *cnode;
  CHI_LIST_ITER chi_iter;
  // create entry tag chi for vor
  FOR_ALL_NODE(cnode, chi_iter, Init(Stmt_vor_chi(stmt))) {
    CVOR *cvor = (CVOR *) cnode->RESULT();
    VSYM_OBJ_REP *vor = cvor->first;
    CODEREP *cr = cvor->second;
    BOOL is_const = FALSE;
    AUX_STAB_ENTRY* aux = Opt_stab()->Aux_stab_entry(cr->Aux_id());
    ST *st = aux->St();
    if(st && ST_is_const_var(st)) {
      is_const = TRUE;
    }
    if (vor->Attr() == ROR_DEF_BY_CHI) {
      TB_LIST_ITER tb_list_iter;
      TAG_BASE *tb;
      FOR_ALL_ELEM(tb, tb_list_iter, Init(tag_base_list)) {
        if (!tb->Is_set_flag(RSC_TAG_USED)) {
          continue;
        }
        RBC_ENGINE::TAG_DEF_VAL tag_def_val = RBC_ENGINE::TAG_KEEP;
        if (is_const) {
          TAG_CONF_INFO *info = Rbc()->Get_tag_conf_info(tb->Id());
          tag_def_val = info ? info->Cst_val() : RBC_ENGINE::TAG_KEEP;
          if (tag_def_val == RBC_ENGINE::TAG_KEEP) {
            continue;
          }
        }
        TAG_OBJ_REP *tor = Allocate_tag_obj(tb, vor, stmt);
        if(is_const) {
          tor->Set_const_side_effect(tag_def_val);
        } else {
          tor->Set_def_attr(TO_DEF_BY_ENTRY_CHI);
        }
        Enter_tor_ror(vor, tor);
      }
    }
  }
  // create entry tag chi for hor and cr
  FOR_ALL_NODE(cnode, chi_iter, Init(stmt->Chi_list())) {
    if (!cnode->Live())
      continue;
    BOOL is_const = FALSE;
    AUX_STAB_ENTRY* aux = Opt_stab()->Aux_stab_entry(cnode->Aux_id());
    ST *st = aux->St();
    if(st && ST_is_const_var(st)) {
      is_const = TRUE;
    }
    CODEREP *cr = cnode->RESULT();
    IDTYPE parm_idx = Dna()->Is_param(cr);
    TAG_BASE *tb;
    TB_LIST_ITER tb_list_iter;
    FOR_ALL_ELEM(tb, tb_list_iter, Init(tag_base_list)) {
      if (!tb->Is_set_flag(RSC_TAG_USED)) {
        continue;
      }
      RBC_ENGINE::TAG_DEF_VAL tag_def_val = RBC_ENGINE::TAG_KEEP;
      if (is_const) {
        TAG_CONF_INFO *info = Rbc()->Get_tag_conf_info(tb->Id());
        tag_def_val = info ? info->Cst_val() : RBC_ENGINE::TAG_KEEP;
        if (tag_def_val == RBC_ENGINE::TAG_KEEP) {
          continue;
        }
      }
      TAG_OBJ_REP *tor = Allocate_tag_obj(tb, cr, stmt);
      HEAP_OBJ_REP *hor = Cr_2_heap_obj(cr);
      TAG_OBJ_REP *hor_tor = (hor && !Is_special_hor(hor))
                               ? Allocate_tag_obj(tb, hor, stmt) : NULL;
      // mark all attr set for const, treat them as sanitized
      if(is_const) {
        tor->Set_const_side_effect(tag_def_val);
        if(hor_tor) {
          hor_tor->Set_const_side_effect(tag_def_val);
          hor_tor->Set_hor_cr(cr);
          Enter_tor_ror(hor, hor_tor);
        }
      } else {
        TOR_DEF_ATTR def_attr = TO_DEF_BY_ENTRY_CHI;
        if(parm_idx != INVALID_VAR_IDX &&
           Dna()->Is_set_parm_tag(parm_idx, tb->Id())) {
          def_attr = TO_DEF_BY_CREATE;
        }
        tor->Set_def_attr(def_attr);
        if(hor_tor) {
          hor_tor->Set_def_attr(def_attr);
          hor_tor->Set_hor_cr(cr);
          Enter_tor_ror(hor, hor_tor);
        }
      }
      Enter_tor_ror(cr, tor);
    }
  }
}

void
VSA::Create_tag_obj_rep(BB_NODE *bb, MEM_POOL *def_bbs_pool)
{
  // create tag
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {
    if (!OPERATOR_is_call(stmt->Opr()))
      continue;
    // handle call here
    STMTREP *call_stmt = stmt;
    RNA_NODE *rna = Dna()->Get_callsite_rna(call_stmt);
    DNA_NODE *callee = NULL;
    CALLEE_VECTOR::const_iterator iter;
    BOOL found = FALSE;
    for (iter = rna->Callee_list().begin(); iter != rna->Callee_list().end(); ++iter) {
      callee = Ipsa()->Get_dna(iter->Callee());
      if (!callee)
        continue;
      DNODE_VECTOR *rbc_nodes = Rbc()->Get_rbc_nodes(callee);
      if (rbc_nodes == NULL)
        continue;
      DNODE_VECTOR::const_iterator rbc_iter = rbc_nodes->begin();
      for (; rbc_iter != rbc_nodes->end(); rbc_iter++) {
        DNA_NODE *rbc_callee = *rbc_iter;
        if (callee->Is_set_rbc_flag(DNA_RBC_TAG_CREATE) || rbc_callee->Is_set_rbc_flag(DNA_RBC_TAG_CREATE)) {
          found = TRUE;
          Is_Trace(Tracing(), (TFile, "Find callee related with rbc tag, callee : %s.\n", callee->Fname()));
          // dna set ret tags
          if (callee->Get_ret_tag_node()->Is_set_any_tags()) {
            std::vector<IDTYPE> tag_id_vec;
            TAG_NODE *tn = callee->Get_ret_tag_node();
            tn->Collect_all_tags(tag_id_vec);
            // create tag obj for return
            CODEREP *cr = Comp_unit()->Find_return_value(call_stmt);
            if (cr) {
              std::vector<IDTYPE>::const_iterator iter;
              for (iter = tag_id_vec.begin(); iter != tag_id_vec.end(); ++iter) {
                IDTYPE tag_id = *iter;
                TAG_BASE *tb = Rbc()->Find_tag_base(tag_id);
                Is_True(tb != NULL, ("tag base is null."));
                VSYM_OBJ_REP *vor = Cr_2_vor(cr);
                if (!vor) {
                  VS_FLD_KIND vfr_kind = Get_vfr_kind(cr);
                  VSYM_FLD_REP vsym_fld_rep(vfr_kind, 0, 0);
                  vor = Find_vor_chi_vor(call_stmt, cr, &vsym_fld_rep);
                }
                if (vor) {
                  TAG_OBJ_REP *vor_tor = Allocate_tag_obj(tb, vor, call_stmt);
                  vor_tor->Set_def_attr(TO_DEF_BY_CREATE);
                  Enter_tor_ror(vor, vor_tor);
                  HEAP_OBJ_REP *base_hor = vor->Vsym_obj()->Base_hor();
                  TAG_OBJ_REP *hor_tor = Allocate_tag_obj(tb, base_hor, call_stmt);
                  hor_tor->Set_def_attr(TO_DEF_BY_CREATE);
                  Enter_tor_ror(base_hor, hor_tor);
                } else {
                  TAG_OBJ_REP *tor = Allocate_tag_obj(tb, cr, call_stmt);
                  tor->Set_def_attr(TO_DEF_BY_CREATE);
                  Enter_tor_ror(cr, tor);
                }
              }
            }
          }
          // create tag for parameter
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
              std::vector<IDTYPE>::const_iterator iter;
              for (iter = tag_id_vec.begin(); iter != tag_id_vec.end(); ++iter) {
                IDTYPE tag_id = *iter;
                TAG_BASE *tb = Rbc()->Find_tag_base(tag_id);
                Is_True(tb != NULL, ("tag base is null."));
                VSYM_OBJ_REP *vor = Cr_2_vor(cr);
                if (!vor) {
                  VS_FLD_KIND vfr_kind = Get_vfr_kind(cr);
                  VSYM_FLD_REP vsym_fld_rep(vfr_kind, 0, 0);
                  vor = Find_vor_chi_vor(call_stmt, cr, &vsym_fld_rep);
                }
                if (vor) {
                  TAG_OBJ_REP *tor = Allocate_tag_obj(tb, vor, call_stmt);
                  tor->Set_def_attr(TO_DEF_BY_CREATE);
                  Enter_tor_ror(vor, tor);
                  HEAP_OBJ_REP *base_hor = vor->Vsym_obj()->Base_hor();
                  TAG_OBJ_REP *hor_tor = Allocate_tag_obj(tb, base_hor, call_stmt);
                  hor_tor->Set_def_attr(TO_DEF_BY_CREATE);
                  Enter_tor_ror(base_hor, hor_tor);
                } else {
                  TAG_OBJ_REP *tor = Allocate_tag_obj(tb, cr, call_stmt);
                  tor->Set_def_attr(TO_DEF_BY_CREATE);
                  Enter_tor_ror(cr, tor);
                }
                // merge tags of elements/fields to base
                // with changes above on finding iload base, the code below seems useless now
                CODEREP *base_cr = Find_ilod_base(cr);
                if (base_cr != NULL && base_cr != cr) {
                  TAGOKIND tag_kind;
                  TOR_LIST_OLD *tor_list = Find_tor_list_from_cr(call_stmt, cr, tag_kind);
                  if (tor_list != NULL) {
                    BOOL merged = Find_tgt_and_merge(call_stmt, base_cr, tor_list);
                  }
                }
              }
            }
          }
          // if found one, ignore others callee
          break;
        }
      }
      if (found)
        break;
    }
  }

  // create tag for donminated bbs
  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb->Dom_bbs())) {
    Create_tag_obj_rep(dom_bb, def_bbs_pool);
  }
}

// =============================================================================
//
// VSA::Perform_tag_analysis, create tag, forward propagate tag
//
// =============================================================================
void
VSA::Perform_tag_analysis_old(CFG *cfg, MEM_POOL *def_bbs_pool)
{
  if (!VSA_Enable_TAG) return;  // turn off TAG for triaging aid

  if (Dna()->Non_functional())
    return;
  BB_NODE *bb;
  CFG_ITER cfg_iter;

  // Precondition: Init_DNA phase has created TAG according to user defined rule

  // TAG insts associate with function calls assocate with TAG notion
  // Collect def_bbs in at the same time

  Is_Trace(Tracing(),
           (TFile, "%sVSA::Perform_tag_analysis: perform TAG for %s\n%s",
            SBar, Dna()->Fname(), SBar));
  // create tag
  Create_tag_obj_rep(cfg->Entry_bb(), def_bbs_pool);
  Create_entrychi_tor();
  // propagate tag
  BOOL changed;
  Propagate_tag_insts(cfg->Entry_bb(), def_bbs_pool, changed);
  // TODO: need to change the phi propagation, use scc
  INT i = 0;
  do {
    changed = FALSE;
    Propagate_tag_insts(cfg->Entry_bb(), def_bbs_pool, changed);
  } while (changed && (i++ < 16));

  Is_Trace(Tracing(), (TFile, "tor cnt:%d\n", Last_tor_id()));
  Is_Trace(Tracing(),
          (TFile, "%sAfter VSA::Perform_tag_analysis: Dump tag object\n%s", SBar, SBar));
  // dump IR with to
  Is_Trace_cmd(Tracing(), Print_obj("Tag Object", TFile));
}
