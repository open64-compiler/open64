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
// opt_vsa_tag_prop.h
//
// VSA inter-procedure tag propagation
//
// ============================================================================
typedef enum pending_status
{
  PENDING_UNKNOWN,
  PENDING_YES,
  PENDING_NO,
} PENDING_STATUS;

class TAG_PROP {
  friend class BIND_TOR_LIST_HELPER_BASE;
  template<TOR_DEF_ATTR attr> friend class  BIND_TOR_LIST_HELPER;

  class TLIST_DEF_INFO {
  private:
    TAGOKIND          _obj_kind;
    TOR_LIST         *_tor_list;
    BB_NODE          *_def_bb;
    union {
      CODEREP        *_def_cr;
      VSYM_OBJ_REP   *_def_vor;
    };
    INT32             _round; // last update round
  public:
    TLIST_DEF_INFO(TOR_LIST *list, CODEREP *cr, BB_NODE *def_bb, INT32 round)
      : _obj_kind(TAG_KIND_VAR), _tor_list(list),
        _def_bb(def_bb), _def_cr(cr), _round(round)   {}
    TLIST_DEF_INFO(TOR_LIST *list, VSYM_OBJ_REP *vor, BB_NODE *def_bb, INT32 round)
      : _obj_kind(TAG_KIND_VOR), _tor_list(list),
        _def_bb(def_bb), _def_vor(vor), _round(round) {}
    TOR_LIST     *Tor_list()          { return _tor_list;   }
    BB_NODE      *Def_bb()            { return _def_bb;     }
    CODEREP      *Def_cr()            { return _def_cr;     }
    VSYM_OBJ_REP *Def_vor()           { return _def_vor;    }
    TAGOKIND      Def_kind()          { return _obj_kind;   }
    INT32         Round()             { return _round;      }
    void          Set_round(INT32 r)  { _round = r;         }
    void Print(FILE *fp)
    {
      if (_obj_kind == TAG_KIND_VAR) {
        fprintf(fp, "cr%d", _def_cr->Coderep_id());
      } else if (_obj_kind == TAG_KIND_VOR) {
        fprintf(fp, "vo%dv%d", _def_vor->Vsym_obj()->Id(), (mINT16)_def_vor->Version());
      } else {
        Is_True(FALSE, ("invalid obj_kind %d", _obj_kind));
      }
      _tor_list->Print(fp, TRUE);
    }
  };

  class PENDING_OP {
  private:
    TOR_DEF_ATTR   _attr;          // pending operator
    IDTYPE         _attr_id;       // for processing attr TO_DEF_BY_TAG_ATTR,
                                   // processing tor with _attr_id only
    CODEREP       *_tgt;           // pending target
    CODEREP       *_src1;          // pending source1
    CODEREP       *_src2;          // pending source2
    STMTREP       *_sr;            // pending statement
    TAG_BASE      *_tag_base;      // for processing attr TO_DEF_BY_TAG_ATTR,
                                   // processing tor with _tag_base only
  public:
    PENDING_OP(TOR_DEF_ATTR attr, STMTREP *sr, CODEREP *tgt, CODEREP *src1, CODEREP *src2 = NULL,
               TAG_BASE *tag_base = NULL, IDTYPE attr_id = TAG_INVALID_ID)
      : _attr(attr), _sr(sr), _tgt(tgt), _src1(src1), _src2(src2),
        _tag_base(tag_base), _attr_id(attr_id) {}
    TOR_DEF_ATTR  Attr()     { return _attr;     }
    CODEREP      *Src()      { return _src1;     }
    CODEREP      *Src1()     { return _src1;     }
    CODEREP      *Src2()     { return _src2;     }
    CODEREP      *Tgt()      { return _tgt;      }
    STMTREP      *Sr()       { return _sr;       }
    TAG_BASE     *Tag_base() { return _tag_base; }
    IDTYPE        Attr_id()  { return _attr_id;  }
  };

  typedef mempool_allocator<TLIST_DEF_INFO*>             DEF_INFO_ALLOCATOR;
  typedef vector<TLIST_DEF_INFO*, DEF_INFO_ALLOCATOR>    TORS_DEF_INFO;
  typedef ID_MAP<TOR_LIST*, IDTYPE>                      TOR_LIST_MAP2;
  typedef pair<INTPTR, INT32>                            TOR_LIST_IDX_PAIR;
  typedef mempool_allocator<TOR_LIST_IDX_PAIR>           TOR_LIST_IDX_ALLOCATOR;
  typedef hash_map<INTPTR, INT32,
                   __gnu_cxx::hash<INTPTR>, 
                   std::equal_to<INTPTR>,
                   TOR_LIST_IDX_ALLOCATOR>               TOR_LIST_IDX_MAP;
  typedef std::pair<TAG_OBJ_REP *, TOR_LIST *>           TOR_OWNER;
  typedef mempool_allocator<TOR_OWNER>                   TOR_OWNER_ALLOCATOR;
  typedef vector<TOR_OWNER, TOR_OWNER_ALLOCATOR>         TOR_OWNERS;
  typedef vector<PENDING_OP*, 
                 mempool_allocator<PENDING_OP*> >        PENDING_OPS;
  typedef vector<PHI_NODE *,
                 mempool_allocator<PHI_NODE*> >          PENDING_PHIS;

  typedef pair<INTPTR, PENDING_STATUS>                   PENDING_CACHE_PAIR;
  typedef mempool_allocator<PENDING_CACHE_PAIR>          PENDING_CACHE_ALLOCATOR;
  typedef hash_map<INTPTR, PENDING_STATUS,
                   __gnu_cxx::hash<INTPTR>, 
                   std::equal_to<INTPTR>,
                   TOR_LIST_IDX_ALLOCATOR>               PENDING_CACHE;
private:
  CXX_MEM_POOL        _loc_pool;        // temporary local pool
  IPSA               *_ipsa;            // IPSA
  COMP_UNIT          *_comp_unit;       // compile unit to be propagated
  BOOL                _trace;           // trace on
  IDTYPE              _last_tor_id;     // track the TAG_OBJ_REP id
  PENDING_OPS         _pending_ops;     // track pending tag ops
  PENDING_PHIS        _pending_phis;    // track pending phi
  PENDING_CACHE       _pending_cache;   // track pending cache
  TORS_DEF_INFO       _tors_definfo;    // track TOR_LIST def info
  TOR_LIST_IDX_MAP    _tors_idx_map;    // map tor_list -> idx
  TOR_OWNERS          _tor_owners;      // track tor's owner
  INT32               _round;           // iteration round

private:
  BOOL        Tracing() const   { return _trace;                 }
  COMP_UNIT  *Comp_unit() const { return _comp_unit;             }
  VSA        *Vsa() const       { return _comp_unit->Vsa();      }
  DNA_NODE   *Dna() const       { return _comp_unit->Dna();      }
  CFG        *Cfg() const       { return _comp_unit->Cfg();      }
  OPT_STAB   *Opt_stab() const  { return _comp_unit->Opt_stab(); }
  IPSA       *Ipsa() const      { return Vsa()->Ipsa();          }
  RBC_BASE   *Rbc() const       { return Vsa()->Ipsa()->Rbc();   }
  MEM_POOL   *Loc_pool()        { return _loc_pool();            }
  MEM_POOL   *Mem_pool()        { return Vsa()->Mem_pool();      }
  INT32       Round()           { return _round;                 }
  void        Inc_round()       { _round++;                      }

  template<typename RSCOBJP>
  TAG_OBJ_REP  *Allocate_tag_obj(TOR_LIST *list, TAG_BASE *tag_base,
                                 RSCOBJP tagobjr,STMTREP *defstmt,
                                 BB_NODE *bb, TOR_DEF_ATTR def_attr) {
    Is_True_Ret(tag_base != NULL, ("Tag base is null."), NULL);
    if (list == NULL) {
      list = Allocate_tor_list(tagobjr, bb);
    } else {
      TAG_OBJ_REP *old_tor = list->Find(tag_base->Id());
      Is_True_Ret(old_tor == NULL, ("tor alloated twice for %s", tag_base->Tag_name()), old_tor);
    }
    Is_Trace(Tracing(), (TFile, "  @@@@@ Allocate_tag_obj to tor_list[%d]: ",
                         Tor_list_idx(list)));
    TAG_OBJ_REP *tor = CXX_NEW(TAG_OBJ_REP(tag_base, _last_tor_id++,
                                           tagobjr, defstmt),
                               Mem_pool());
    Enter_tor_owner(tor, list);
    tor->Set_def_attr(def_attr);
    list->Push_back(tor);
    Is_Trace_cmdn(Tracing(), tor->Print(TFile, TRUE), TFile);
    Set_tor_list_round(list);
    return tor;
  }

  template <typename RSCOBJP>
  TOR_LIST *Allocate_tor_list(RSCOBJP ror, BB_NODE *def_bb) {
    TOR_LIST *tor_list = Tor_list(ror);
    Is_True_Ret(!tor_list, ("tor list already created"), tor_list);
    Is_Trace(Tracing(),
             (TFile, "  $$$$$ Allocate_tor_list[%ld]:", _tors_definfo.size()));
    Is_Trace_cmdn(Tracing(), Print_rscobj(ror, TFile), TFile);
    tor_list = CXX_NEW(TOR_LIST(Mem_pool()), Mem_pool());
    Vsa()->Enter_ror_tor_list(ror, tor_list);
    Is_True_Ret(def_bb, ("Allocate_tor_list: null def bb"), tor_list);
    _tors_idx_map[(INTPTR)tor_list] = _tors_definfo.size();
    TLIST_DEF_INFO *def_info = CXX_NEW(TLIST_DEF_INFO(tor_list, ror, def_bb, Round()),
                                       Loc_pool());
    _tors_definfo.push_back(def_info);
    return tor_list;
  }

  template <typename RSCOBJP>
  TOR_LIST *Tor_list(RSCOBJP ror) {
    return Vsa()->Ror_2_tor_list<RSCOBJP, TOR_LIST>(ror);
  }

  template <typename RSCOBJP>
  void Print_rscobj(RSCOBJP ror, FILE *fp) {
    fprintf(fp, "vo%dv%d", ror->Vsym_obj()->Id(), (mINT16)ror->Version());
  }

  TOR_LIST *Tor_owner(TAG_OBJ_REP *tor) {
    Is_True_Ret(tor, ("Tor_owner: null tor"), NULL);
    return (tor->Id() < _tor_owners.size()) ? _tor_owners[tor->Id()].second : NULL;
  }

  TOR_LIST *Tor_owner(IDTYPE tor_id) {
    return (tor_id < _tor_owners.size()) ? _tor_owners[tor_id].second : NULL;
  }

  void       Enter_tor_owner(TAG_OBJ_REP *tor, TOR_LIST *owner) {
    TOR_LIST *tor_list = Tor_owner(tor);
    if (tor_list == NULL) {
      Is_True(_tor_owners.size()  == tor->Id(),
              ("Enter_tor_owner: wrong tor_owners order"));
      _tor_owners.push_back(make_pair(tor, owner));
    } else {
      Is_True(tor_list == owner,
              ("Enter_tor_owner: call twice for tor%d", tor->Id()));
    }
  }

  IDTYPE     Tor_list_idx(TOR_LIST *list) {
    Is_True_Ret(list, ("Tor_list_idx: null list"), UINT32_MAX);
    TOR_LIST_IDX_MAP::iterator idx_iter = _tors_idx_map.find((INTPTR)list);
    Is_True_Ret(idx_iter != _tors_idx_map.end(), ("Tor_list_idx not found"), UINT32_MAX);
    return idx_iter->second;
  }

  TLIST_DEF_INFO *Tor_list_def_info(IDTYPE idx) {
    Is_True_Ret(idx <_tors_definfo.size(), ("idx outof bound"), NULL);
    return _tors_definfo[idx];
  }

  TLIST_DEF_INFO *Tor_list_def_info(TOR_LIST *list) {
    IDTYPE idx = Tor_list_idx(list);
    return Tor_list_def_info(idx);
  }

  BOOL       Tor_list_updated(IDTYPE idx) {
    TLIST_DEF_INFO *def_info = Tor_list_def_info(idx);
    Is_True_Ret(def_info, ("null def info found for idx", idx), FALSE);
    INT32 def_round = def_info->Round();
    if (def_round == Round() || def_round == Round() - 1) {
      return TRUE;
    }
    return FALSE;
  }

  BOOL       Tor_list_updated(TOR_LIST *list) {
    IDTYPE idx = Tor_list_idx(list);
    return Tor_list_updated(idx);
  }

  void       Set_tor_list_round(TOR_LIST *list) {
    TLIST_DEF_INFO *def_info = Tor_list_def_info(list);
    Is_True_Ret(def_info, ("null def info found tor_list"));
    def_info->Set_round(Round());
  }

  void       Add_pending_phi(PHI_NODE *phi) {
     _pending_phis.push_back(phi);
  }

  template<typename RSCOBJ>
  void       Enter_pending_cache(RSCOBJ *obj, BOOL pending) {
    _pending_cache[(INTPTR)obj] = pending ? PENDING_YES : PENDING_NO;
  }
  template<typename RSCOBJ>
  PENDING_STATUS  Is_rsc_pending(RSCOBJ *obj) {
    PENDING_CACHE::iterator it = _pending_cache.find((INTPTR)obj);
    if (it != _pending_cache.end()) {
      return it->second;
    } else {
      return PENDING_UNKNOWN;
    }
  }
public:
  TAG_PROP(IPSA *ipsa, COMP_UNIT *cu)
    : _ipsa(ipsa), _comp_unit(cu), _last_tor_id(1),
      _loc_pool("LOCAL_CPROP local pool", FALSE),
      _tors_definfo(DEF_INFO_ALLOCATOR(Loc_pool())),
      _pending_ops(mempool_allocator<PENDING_OP*>(Loc_pool())),
      _pending_phis(mempool_allocator<PHI_NODE*>(Loc_pool())),
      _tors_idx_map(7, __gnu_cxx::hash<INTPTR>(), std::equal_to<INTPTR>(),
                    TOR_LIST_IDX_ALLOCATOR(Loc_pool())),
      _pending_cache(7, __gnu_cxx::hash<INTPTR>(), std::equal_to<INTPTR>(),
                     PENDING_CACHE_ALLOCATOR(Loc_pool())),
      _tor_owners(1, make_pair((TAG_OBJ_REP *)NULL, (TOR_LIST *)NULL),
                  TOR_OWNER_ALLOCATOR(Loc_pool())),
      _round(1) {
    _trace = Get_Trace(TP_VSA, VSA_TAG_PROP_TRACE_FLAG);
    Vsa()->Set_tag_prop(this);
  }
  ~TAG_PROP(void) {
    Vsa()->Set_tag_prop(NULL);
  }

  void Do_prop();
  void Propagate_tag_entry();
  void Propagate_tag_callee();
  void Propagate_tag_rbc_create(RNA_NODE *rna, DNA_NODE *callee);
  void Propagate_tag_bb(BB_NODE *bb);
  void Propagate_tag_for_phi(BB_NODE *bb);
  void Propagate_tag_stmt(STMTREP *sr, BB_NODE *bb);
  void Propagate_tag_rna(RNA_NODE *rna);
  void Propagate_tag_chi(STMTREP *sr);
  void Propagate_pending_ops();
  void Finalize();

  template<TOR_DEF_ATTR attr> void
  Bind_or_pending_tag(BB_NODE *bb, STMTREP *sr,CODEREP *tgt,
                      CODEREP *src1, CODEREP *src2 = NULL,
                      TAG_BASE *tag_base = NULL, IDTYPE attr_id = TAG_INVALID_ID);

  template<TOR_DEF_ATTR attr> void
  Bind_tor_list_to_cr(BB_NODE *bb, STMTREP *sr, CODEREP *cr,
                      TOR_LIST *src_list, TOR_LIST *src_list2 = NULL,
                      TAG_BASE *tag_base = NULL, IDTYPE attr_id = TAG_INVALID_ID);

  template<typename RSCOBJ, TOR_DEF_ATTR attr> void
  Bind_tor_list(BB_NODE *bb, STMTREP *sr, RSCOBJ ror, TOR_LIST *src);

  TOR_LIST *Find_or_pending_stmt(STMTREP *sr, CODEREP *cr, BOOL &pending);
  BOOL      Is_cr_pending(CODEREP *cr, STMTREP *sr, hash_set<IDTYPE> &visited);
  BOOL      Is_vor_pending(VSYM_OBJ_REP *vor, hash_set<IDTYPE> &visited);
  void      Add_pending_op(TOR_DEF_ATTR attr,STMTREP *sr,  CODEREP *tgt,
                           CODEREP *src1, CODEREP *src2 = NULL,
                           TAG_BASE *tag_base = NULL, IDTYPE attr_id = TAG_INVALID_ID);

  void      Print(FILE *fp) const;
};

class BIND_TOR_LIST_HELPER_BASE
{
private:
  TAG_PROP     *_tag_prop;
  BB_NODE      *_bb;
  STMTREP      *_sr;
  CODEREP      *_tgt;
  TOR_LIST     *_src_list;
public:
  BIND_TOR_LIST_HELPER_BASE(TAG_PROP *prop, BB_NODE *bb, STMTREP *sr,
                            CODEREP *tgt, TOR_LIST *src_list)
    : _tag_prop(prop), _bb(bb), _sr(sr), _tgt(tgt), _src_list(src_list) {
  }
  TAG_PROP     *Tag_prop()    { return _tag_prop;            }
  BOOL          Tracing()     { return _tag_prop->Tracing(); }
  BB_NODE      *Bb()          { return _bb;                  }
  STMTREP      *Sr()          { return _sr;                  }
  CODEREP      *Tgt()         { return _tgt;                 }
  TOR_LIST     *Src_list()    { return _src_list;            }
  VSA          *Vsa()         { return _tag_prop->Vsa();     }

  template<typename HELPER>
  void Bind_base(HELPER *helper) {
    VSYM_OBJ_REP *tgt_vor = Tag_prop()->Vsa()->Find_vor_with_tag(Sr(), Tgt(), FALSE);
    TOR_LIST *tgt_list = tgt_vor ? Tag_prop()->Tor_list(tgt_vor) :
                                   Tag_prop()->Tor_list(Tgt());
    if (Src_list() == tgt_list) {
      return;
    }
    if (tgt_vor) {
      Bind_tor_list(helper, tgt_vor, Src_list());
    } else {
      Bind_tor_list(helper, Tgt(), Src_list());
    }
    // binds to vor chi was same base cr
    // NOTE: this propagate is downward from base to field, consider add extra info
    //       to mark the ambigous propagation in future
    if (Sr() && Vsa()->Stmt_vor_chi(Sr()) && 
        (!tgt_vor || tgt_vor->Vsym_obj()->Fld_rep_ptr()->Is_any())) {
      CHI_LIST_ITER chi_iter;
      CHI_NODE     *cnode;
      FOR_ALL_NODE(cnode, chi_iter, Init(Vsa()->Stmt_vor_chi(Sr()))) {
        CVOR *cvor = (CVOR*)cnode->RESULT();
        if (cvor->second == Tgt() && cvor->first != tgt_vor) {
          Bind_tor_list(helper, cvor->first, Src_list());
        }
      }
    }
  }

  template<typename RSCOBJ, typename HELPER> void
  Bind_tor_list(HELPER *helper, RSCOBJ ror, TOR_LIST *src) {
    Is_True_Ret(src, ("Src tor list is NULL."));
    for (int i = 0; i < src->Size(); i++) {
      helper->Bind_tor(ror, (*src)[i]);
    }
  }
};

template<TOR_DEF_ATTR attr>
class BIND_TOR_LIST_HELPER : public BIND_TOR_LIST_HELPER_BASE
{
public:
  BIND_TOR_LIST_HELPER(TAG_PROP *prop, BB_NODE *bb, STMTREP *sr,
                       CODEREP *tgt, TOR_LIST *src_list1,
                       TOR_LIST *src_list2 = NULL,
                       TAG_BASE *tag_base = NULL,
                       IDTYPE attr_id = TAG_INVALID_ID) :
    BIND_TOR_LIST_HELPER_BASE(prop, bb, sr, tgt, src_list1) {}
};

template<>
class BIND_TOR_LIST_HELPER<TO_DEF_BY_PHI> : public BIND_TOR_LIST_HELPER_BASE
{
public:
  BIND_TOR_LIST_HELPER(TAG_PROP *prop, BB_NODE *bb, STMTREP *sr,
                       CODEREP *tgt, TOR_LIST *src_list1,
                       TOR_LIST *src_list2 = NULL,
                       TAG_BASE *tag_base = NULL,
                       IDTYPE attr_id = TAG_INVALID_ID) :
    BIND_TOR_LIST_HELPER_BASE(prop, bb, sr, tgt, src_list1) {}

  void Bind() {
    Bind_base(this);
  }

  template<typename RSCOBJ> void 
  Bind_tor(RSCOBJ ror, TAG_OBJ_REP *src_tor) {
    Is_Trace(Tracing(),
             (TFile, "[sr%d]: Merge tag \"%s\" to ", Sr() ? Sr()->Stmtrep_id():-1,
              src_tor->Tag_base()->Tag_name()));
    Is_Trace_cmd(Tracing(), Tag_prop()->Print_rscobj(ror, TFile));
    Is_Trace(Tracing(), (TFile, " from "));
    Is_Trace_cmdn(Tracing(), src_tor->Print(TFile, FALSE), TFile);
    TOR_LIST *dst = Tag_prop()->Tor_list(ror);
    if (dst == NULL) {
      dst = Tag_prop()->Allocate_tor_list(ror, Bb());
    }
    INT32 pos = dst->Find_pos(src_tor);
    TAG_OBJ_REP *tgt_tor = pos == -1 ? NULL : (*dst)[pos];
#ifndef EXPAND_PHI_TOR
    if (tgt_tor == NULL) {
      tgt_tor = Tag_prop()->Allocate_tag_obj(
        dst, src_tor->Tag_base(), ror, Sr(), Bb(), TO_DEF_BY_PHI);
    }
    if (tgt_tor != src_tor) {
      if (tgt_tor->Def_attr() != TO_DEF_BY_PHI) {
          // delete old entry
          dst->Remove(pos);
          TAG_OBJ_REP *merge_tor = Tag_prop()->Allocate_tag_obj(
            dst, tgt_tor->Tag_base(), ror, Sr(), Bb(), TO_DEF_BY_PHI);
          merge_tor->Push_phi(tgt_tor, Tag_prop()->Mem_pool());
          tgt_tor = merge_tor;
      }
      tgt_tor->Push_phi(src_tor, Tag_prop()->Mem_pool());
    }
#else
    if (tgt_tor == NULL) {
      dst->Push_back(src_tor);
    }
    else if (tgt_tor != src_tor) {
      TOR_LIST *owner_list = Tag_prop()->Tor_owner(tgt_tor);
      Is_True(owner_list, ("tor%d owner not set", tgt_tor->Id()));
      if (owner_list == dst) {
        if (tgt_tor->Def_attr() != TO_DEF_BY_PHI) {
          // delete old entry
          dst->Remove(pos);
          TAG_OBJ_REP *merge_tor = Tag_prop()->Allocate_tag_obj(
            dst, tgt_tor->Tag_base(), ror, Sr(), Bb(), TO_DEF_BY_PHI);
          Push_phi(Sr(), dst, merge_tor, tgt_tor, Tag_prop()->Mem_pool());
          tgt_tor = merge_tor;
        }

        Push_phi(Sr(), dst, tgt_tor, src_tor, Tag_prop()->Mem_pool());
      } else {
        // delete old entry
        dst->Remove(pos);
        TAG_OBJ_REP *merge_tor = Tag_prop()->Allocate_tag_obj(
          dst, tgt_tor->Tag_base(), ror, Sr(), Bb, TO_DEF_BY_PHI);
        Push_phi(Sr(), dst, merge_tor, tgt_tor, Tag_prop()->Mem_pool());
        Push_phi(Sr(), dst, merge_tor, src_tor, Tag_prop()->Mem_pool());
      }
    }
#endif
    Is_Trace(Tracing(), (TFile, "  Result: "));
    Is_Trace_cmd(Tracing(), Tag_prop()->Print_rscobj(ror, TFile));
    Is_Trace_cmdn(Tracing(), dst->Print(TFile), TFile);
  }
#ifdef EXPAND_PHI_TOR
  void Push_phi(STMTREP *sr, TOR_LIST *tgt_list, TAG_OBJ_REP *tgt_tor,
                TAG_OBJ_REP *src_tor, MEM_POOL *pool) {
    if (src_tor == tgt_tor) {
      return;
    }
    // if src_tor has def stmt, phi is coming from or_tag, 
    // do not expand the source tor as we need to trace the path info
    // for the def stmt
    if (src_tor->Def_attr() == TO_DEF_BY_PHI && !src_tor->Defstmt()) {
      for (int i = 0; i < src_tor->Phi_list()->size(); i++) {
        Push_phi(sr, tgt_list, tgt_tor, src_tor->Phi_list()->at(i), pool);
      }
    } else {
      if (tgt_tor->Push_phi(src_tor, pool)) {
        // if sr is null, then tgt_list is coming from bb phi result
        // mark tgt_list updated, otherwise no need to set updated,
        // as the tor opnd won't be expanded
        if (sr == NULL) {
          Tag_prop()->Set_tor_list_round(tgt_list);
        }
        Is_Trace(Tracing(),
                  (TFile, "  &&&&& Push phi tor%d to tor%d\n",
                  src_tor->Id(), tgt_tor->Id()));
      }
    }
  }
#endif
};

template<>
class BIND_TOR_LIST_HELPER<TO_DEF_BY_COPY> : public BIND_TOR_LIST_HELPER_BASE
{
public:
  BIND_TOR_LIST_HELPER(TAG_PROP *prop, BB_NODE *bb, STMTREP *sr,
                       CODEREP *tgt, TOR_LIST *src_list1,
                       TOR_LIST *src_list2 = NULL,
                       TAG_BASE *tag_base = NULL,
                       IDTYPE attr_id = TAG_INVALID_ID) :
    BIND_TOR_LIST_HELPER_BASE(prop, bb, sr, tgt, src_list1) {}

  void Bind() {
    Bind_base(this);
  }
  template<typename RSCOBJ> void 
  Bind_tor(RSCOBJ ror, TAG_OBJ_REP *src_tor) {
    Is_Trace(Tracing(), (TFile, "[sr%d]: Copy tag to ", Sr()->Stmtrep_id()));
    Is_Trace_cmd(Tracing(), Tag_prop()->Print_rscobj(ror, TFile));
    Is_Trace(Tracing(), (TFile, " from "));
    Is_Trace_cmdn(Tracing(), src_tor->Print(TFile, FALSE), TFile);
    TOR_LIST *dst = Tag_prop()->Tor_list(ror);
    if (dst == NULL) {
      dst = Tag_prop()->Allocate_tor_list(ror, Bb());
    }
    TAG_OBJ_REP *tgt_tor = dst->Find(src_tor);
    if (tgt_tor == NULL) {
      tgt_tor = Tag_prop()->Allocate_tag_obj(dst, src_tor->Tag_base(),
                                             ror, Sr(), Bb(), TO_DEF_BY_COPY);
      tgt_tor->Set_deftor(src_tor);
      tgt_tor->Clone_tag_attrs(src_tor->Get_tag_attrs());
    } else if (!tgt_tor->Has_deftor() || tgt_tor->Deftor() != src_tor) {
      Is_True(Tag_prop()->Tor_owner(tgt_tor) == dst, ("tor_list not match"));
      Is_Trace(Tracing(),
               (TFile, "WARN: same tag_base but different tor added to sr%d,"
                "use latter\n", Sr()->Stmtrep_id()));
      tgt_tor->Set_def_attr(TO_DEF_BY_COPY);
      tgt_tor->Set_deftor(src_tor);
      tgt_tor->Clone_tag_attrs(src_tor->Get_tag_attrs());
    } else {
      // same tor, do nothing
    }
  }
};

template<>
class BIND_TOR_LIST_HELPER<TO_DEF_BY_CHI> : public BIND_TOR_LIST_HELPER_BASE
{
public:
  BIND_TOR_LIST_HELPER(TAG_PROP *prop, BB_NODE *bb, STMTREP *sr,
                       CODEREP *tgt, TOR_LIST *src_list1,
                       TOR_LIST *src_list2 = NULL,
                       TAG_BASE *tag_base = NULL,
                       IDTYPE attr_id = TAG_INVALID_ID) :
    BIND_TOR_LIST_HELPER_BASE(prop, bb, sr, tgt, src_list1) {}

  void Bind() {
    Bind_base(this);
  }

  template<typename RSCOBJ> void 
  Bind_tor(RSCOBJ ror, TAG_OBJ_REP *src_tor) {
    TOR_LIST *dst = Tag_prop()->Tor_list(ror);
    if (dst == NULL) {
      dst = Tag_prop()->Allocate_tor_list(ror, Bb());
    }
    TAG_OBJ_REP *tgt_tor = dst->Find(src_tor);
    if (tgt_tor == NULL) {
      Tag_prop()->Allocate_tag_obj(dst, src_tor->Tag_base(), ror, Sr(), Bb(), TO_DEF_BY_CHI);
    } else {
      Is_True(tgt_tor->Def_attr() == TO_DEF_BY_CHI,
              ("Bind_tor:already bind with attr%d", tgt_tor->Def_attr()));
    }
  }
};

template<>
class BIND_TOR_LIST_HELPER<TO_DEF_BY_SE> : public BIND_TOR_LIST_HELPER_BASE
{
public:
  BIND_TOR_LIST_HELPER(TAG_PROP *prop, BB_NODE *bb, STMTREP *sr,
                       CODEREP *tgt, TOR_LIST *src_list1,
                       TOR_LIST *src_list2 = NULL,
                       TAG_BASE *tag_base = NULL,
                       IDTYPE attr_id = TAG_INVALID_ID) :
    BIND_TOR_LIST_HELPER_BASE(prop, bb, sr, tgt, src_list1) {}

  void Bind() {
    Bind_base(this);
  }

  template<typename RSCOBJ> void 
  Bind_tor(RSCOBJ ror, TAG_OBJ_REP *src_tor) {
    TOR_LIST *dst = Tag_prop()->Tor_list(ror);
    if (dst == NULL) {
      dst = Tag_prop()->Allocate_tor_list(ror, Bb());
    }
    TAG_OBJ_REP *tgt_tor = dst->Find(src_tor);
    if (tgt_tor == NULL) {
      Tag_prop()->Allocate_tag_obj(dst, src_tor->Tag_base(), ror, Sr(), Bb(), TO_DEF_BY_SE);
    } else {
      Is_True(tgt_tor->Def_attr() == TO_DEF_BY_SE,
              ("Bind_tor:already bind with attr%d", tgt_tor->Def_attr()));
    }
  }
};

template<>
class BIND_TOR_LIST_HELPER<TO_DEF_BY_TAG_ATTR> : public BIND_TOR_LIST_HELPER_BASE
{
private:
  TAG_BASE *_tag_base;
  IDTYPE    _attr_id;
public:
  BIND_TOR_LIST_HELPER(TAG_PROP *prop, BB_NODE *bb, STMTREP *sr,
                       CODEREP *tgt, TOR_LIST *src_list1, TOR_LIST *src_list2,
                       TAG_BASE *tag_base, IDTYPE attr_id) :
    BIND_TOR_LIST_HELPER_BASE(prop, bb, sr, tgt, src_list1),
    _tag_base(tag_base), _attr_id(attr_id) {}

  void Bind() {
    Is_True_Ret(_tag_base, ("Bind<TO_DEF_BY_TAG_ATTR>: tag_base is NULL"));
    Is_True_Ret(_attr_id != TAG_INVALID_ID, ("Bind<TO_DEF_BY_TAG_ATTR>: invalid attr_id"));
    TOR_LIST *src_list = Src_list();
    if (src_list == NULL || src_list->Empty()) {
      return;
    }
    VSYM_OBJ_REP *tgt_vor = Tag_prop()->Vsa()->Find_vor_with_tag(Sr(), Tgt(), FALSE);
    for (int i = 0; i < src_list->Size(); i++) {
      TAG_OBJ_REP *src_tor = (*src_list)[i];
      if (src_tor->Tag_base()->Id() == _tag_base->Id()) {
        if (tgt_vor) {
          Bind_tor(tgt_vor, src_tor);
        } else {
          Bind_tor(Tgt(), src_tor);
        }
      } else {
        // for other tag_base, copy from source
        BIND_TOR_LIST_HELPER<TO_DEF_BY_COPY> helper(Tag_prop(), Bb(), Sr(), Tgt(), src_list);
        if (tgt_vor) {
          helper.Bind_tor(tgt_vor, src_tor);
        } else {
          helper.Bind_tor(Tgt(), src_tor);
        }
      }
    }
  }

  template<typename RSCOBJ> void 
  Bind_tor(RSCOBJ ror, TAG_OBJ_REP *src_tor) {
    TOR_LIST *dst = Tag_prop()->Tor_list(ror);
    if (dst == NULL) {
      dst = Tag_prop()->Allocate_tor_list(ror, Bb());
    }
    TAG_OBJ_REP *tgt_tor = dst->Find(src_tor);
    if (tgt_tor == NULL) {
      tgt_tor = Tag_prop()->Allocate_tag_obj(dst, src_tor->Tag_base(), ror, Sr(), Bb(), TO_DEF_BY_TAG_ATTR);
      tgt_tor->Set_deftor(src_tor);
      tgt_tor->Set_tag_attr(_attr_id);
    } else {
      Is_True(tgt_tor->Def_attr() == TO_DEF_BY_TAG_ATTR &&
              tgt_tor->Deftor() == src_tor &&
              tgt_tor->Is_set_tag_attr(_attr_id),
              ("Bind_tor:already bind with attr%d", tgt_tor->Def_attr()));
    }
  }
};

template<>
class BIND_TOR_LIST_HELPER<TO_DEF_BY_OR> : public BIND_TOR_LIST_HELPER_BASE
{
private:
  TOR_LIST *_src_list2;
public:
  BIND_TOR_LIST_HELPER(TAG_PROP *prop, BB_NODE *bb, STMTREP *sr,
                       CODEREP *tgt, TOR_LIST *src_list1,
                       TOR_LIST *src_list2,
                       TAG_BASE *tag_base = NULL,
                       IDTYPE attr_id = TAG_INVALID_ID) :
    BIND_TOR_LIST_HELPER_BASE(prop, bb, sr, tgt, src_list1),
    _src_list2(src_list2) {}

  TOR_LIST *Src_list2()   { return _src_list2; }

  void Bind() {
    TOR_LIST *src_list1 = Src_list();
    TOR_LIST *src_list2 = Src_list2();
    if (src_list1 == NULL && src_list2 == NULL) {
      return;
    }
    VSYM_OBJ_REP *tgt_vor = Tag_prop()->Vsa()->Find_vor_with_tag(Sr(), Tgt(), FALSE);
    if (tgt_vor) {
      if (src_list1) {
        Bind_tor_list(this, tgt_vor, src_list1);
      }
      if (src_list2) {
        Bind_tor_list(this, tgt_vor, src_list2);
      }
    } else {
      if (src_list1) {
        Bind_tor_list(this, Tgt(), src_list1);
      }
      if (src_list2) {
        Bind_tor_list(this, Tgt(), src_list2);
      }
    }
  }

  template<typename RSCOBJ> void 
  Bind_tor(RSCOBJ ror, TAG_OBJ_REP *src_tor) {
     Is_Trace(Tracing(),
             (TFile, "[sr%d]: Merge tag \"%s\" to ", Sr() ? Sr()->Stmtrep_id():-1,
              src_tor->Tag_base()->Tag_name()));
    Is_Trace_cmd(Tracing(), Tag_prop()->Print_rscobj(ror, TFile));
    Is_Trace(Tracing(), (TFile, " from "));
    Is_Trace_cmdn(Tracing(), src_tor->Print(TFile, FALSE), TFile);
    TOR_LIST *dst = Tag_prop()->Tor_list(ror);
    if (dst == NULL) {
      dst = Tag_prop()->Allocate_tor_list(ror, Bb());
    }
    INT32 pos = dst->Find_pos(src_tor);
    TAG_OBJ_REP *tgt_tor = pos == -1 ? NULL : (*dst)[pos];
    if (tgt_tor == NULL) {
      tgt_tor = Tag_prop()->Allocate_tag_obj(
        dst, src_tor->Tag_base(), ror, Sr(), Bb(), TO_DEF_BY_PHI);
    }
    if (tgt_tor != src_tor) {
      if (tgt_tor->Def_attr() != TO_DEF_BY_PHI) {
          // delete old entry
          dst->Remove(pos);
          TAG_OBJ_REP *merge_tor = Tag_prop()->Allocate_tag_obj(
            dst, tgt_tor->Tag_base(), ror, Sr(), Bb(), TO_DEF_BY_PHI);
          merge_tor->Push_phi(tgt_tor, Tag_prop()->Mem_pool());
          tgt_tor = merge_tor;
      }
      tgt_tor->Push_phi(src_tor, Tag_prop()->Mem_pool());
    }
    Is_Trace(Tracing(), (TFile, "  Result: "));
    Is_Trace_cmd(Tracing(), Tag_prop()->Print_rscobj(ror, TFile));
    Is_Trace_cmdn(Tracing(), dst->Print(TFile), TFile);
  }
};

template <> inline void
TAG_PROP::Print_rscobj<CODEREP *>(CODEREP *ror, FILE *fp) {
  fprintf(fp, "cr%d", ror->Coderep_id());
}

template<TOR_DEF_ATTR attr> void
TAG_PROP::Bind_tor_list_to_cr(BB_NODE *bb, STMTREP *sr, CODEREP *tgt,
                              TOR_LIST *src_list1, TOR_LIST *src_list2,
                              TAG_BASE *tag_base, IDTYPE attr_id)
{
  Is_True_Ret(tgt, ("Bind_tor_list_to_cr: tgt is NULL."));
  if ((src_list1 == NULL || src_list1->Empty()) &&
      (src_list2 == NULL || src_list2->Empty()))
    return;
  BIND_TOR_LIST_HELPER<attr> helper(this, bb, sr, tgt, 
                                    src_list1, src_list2, tag_base, attr_id);
  helper.Bind();
}


template<TOR_DEF_ATTR attr> void
TAG_PROP::Bind_or_pending_tag(BB_NODE *bb, STMTREP *sr, CODEREP *tgt,
                              CODEREP *src1, CODEREP *src2,
                              TAG_BASE *tag_base, IDTYPE attr_id)
{
  Is_True_Ret(tgt, ("Bind_or_pending_tag: tgt is NULL."));
  Is_True_Ret(src1, ("Bind_or_pending_tag: src1 is NULL"));
  Is_Trace(Tracing(), (TFile, "\n[sr%d]:\n", sr->Stmtrep_id()));
  Is_Trace_cmd(Tracing(), Vsa()->Print_sr(sr, TFile));
  BOOL pending = FALSE;
  TOR_LIST *src_list1 = Find_or_pending_stmt(sr, src1, pending);
  if (src_list1) {
    Bind_tor_list_to_cr<attr>(bb, sr, tgt, src_list1, NULL, tag_base, attr_id);
  }
  if (pending) {
    Add_pending_op(attr, sr, tgt, src1, NULL, tag_base, attr_id);
  }
}

template<> void
TAG_PROP::Bind_or_pending_tag<TO_DEF_BY_OR>(BB_NODE *bb, STMTREP *sr, CODEREP *tgt,
                                            CODEREP *src1, CODEREP *src2,
                                            TAG_BASE *tag_base, IDTYPE attr_id)
{
  Is_True_Ret(tgt, ("Bind_or_pending_tag: cr is NULL."));
  Is_True_Ret(src1, ("Bind_or_pending_tag: src1 is NULL"));
  Is_True_Ret(src2, ("Bind_or_pending_tag: src2 is NULL"));
  Is_Trace(Tracing(), (TFile, "\n[sr%d]:\n", sr->Stmtrep_id()));
  Is_Trace_cmd(Tracing(), Vsa()->Print_sr(sr, TFile));
  BOOL pending1 = FALSE;
  BOOL pending2 = FALSE;
  TOR_LIST *src_list1 = Find_or_pending_stmt(sr, src1, pending1);
  TOR_LIST *src_list2 = Find_or_pending_stmt(sr, src2, pending2);
  Bind_tor_list_to_cr<TO_DEF_BY_OR>(bb, sr, tgt, src_list1, src_list2, NULL, TAG_INVALID_ID);
  if (pending1 || pending2) {
    Add_pending_op(TO_DEF_BY_OR, sr, tgt, src1, src2);
  }
}

template<typename RSCOBJ, TOR_DEF_ATTR attr> void
TAG_PROP::Bind_tor_list(BB_NODE *bb, STMTREP *sr, RSCOBJ ror, TOR_LIST *src)
{
  if (src == Tor_list(ror)) {
    return;
  }
  BIND_TOR_LIST_HELPER<attr> helper(this, bb, sr, NULL, src);
  helper.Bind_tor_list(&helper, ror, src);
}
