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
// Module: opt_vsa.h
//
// ====================================================================
//


#ifndef opt_vsa_INCLUDED
#define opt_vsa_INCLUDED        "opt_vsa.h"

#ifdef _KEEP_RCS_ID
static char *opt_vsarcs_id =         opt_vsa_INCLUDED"$Revision$";
#endif /* _KEEP_RCS_ID */

#include "opt_dna.h"
#include "opt_etable.h"
#include "opt_htable.h"
#include "opt_mu_chi.h"
#include "opt_vsa_ssa.h"
#include "config_vsa.h"
#include "report.h"  // for vsa report
#include <ext/hash_set>
#include "symtab_access_global.h"
using __gnu_cxx::hash_set;
#include "opt_vsa_meta.h"
#include "opt_vsa_var_def.h"
#include <regex.h>

enum EVAL_ACTION {
  MIN_VALUE,         // eval minimal value, like dest buff size, etc
  MAX_VALUE,         // eval maximal value, like src buf size, copy length, etc
};

#include "opt_vsa_rsc.h"
#include "opt_vsa_jni.h"
#include "opt_vsa_path.h"
#include "opt_vsa_eh.h"
#include "opt_vsa_vsym_tracker.h"

// forward declaration
class CFG;
class CODEMAP;
class DNA_NODE;
class EXP_OCCURS;
class JNI_ANNOT;
class EH_PATH;
class STMTERP;
class TOR_LIST_OLD;
class TRAV_CONTEXT;
class VAR_DEF_HELPER;
class TAG_PROP;

typedef std::vector<SRCPOS_NODE> SRCPOS_NODES;
typedef std::pair<DNA_NODE *, IDTYPE> DNA_CR_TYPE;
typedef vector<DNA_CR_TYPE> DEF_TYS;

extern void Rename_CODEMAP(COMP_UNIT *);
 
enum VAL_RANGE_RESULT {
  VAL_OOR = 0,
  VAL_May_OOR = 1,
  VAL_INR = 2,
  VAL_UNK_R = 3,
};


class SCC_NODE {
private:
    CODEREP       *_id;                   // maps id in stack to CODEREP for a variable
    INT            _low;                  // low-link of this in indexing into _ids
    BOOL           _on_stack;             // shows whether CR is on solver stack

  SCC_NODE(void);                         // REQUIRED UNDEFINED UNWANTED methods
  SCC_NODE(const SCC_NODE &);             // REQUIRED UNDEFINED UNWANTED methods
  SCC_NODE& operator = (const SCC_NODE&); // REQUIRED UNDEFINED UNWANTED methods

public:
  SCC_NODE(CODEREP *id, INT low, BOOL on_stack) :
    _id(id), _low(low), _on_stack(on_stack) {};

  CODEREP    *Id()        const          { return _id; }
  INT         Low()       const          { return _low; }
  BOOL        On_stack()  const          { return _on_stack; }

  void        Set_id(CODEREP *id)        { _id       = id; }
  void        Set_low(INT low)           { _low      = low; }
  void        Set_on_stack(BOOL on_stack){ _on_stack = on_stack; }

};

// =============================================================================
//
// SCC: Tarjan's Strongly Connected Component for the SSA graph for a variable
//      It's used to keep track of SSA names w/ circular dependency through phi
//      Each SCC is treated as a super phi node which shares the same attribute
//      in their phi_result
//
// =============================================================================
class SCC {
  typedef mempool_allocator<SCC_NODE*> SN_ALLOCATOR;
  typedef vector<SCC_NODE*, SN_ALLOCATOR> SCC_NODE_VEC;

private:
  INT              _id;          // last entry in the vector below
  INT              _scc_ecnt;    // number of elements in the scc, 1 as minimum
  SCC_NODE_VEC    *_scc_vec;     // actual content of SCC data
  BOOL             _tracing;     // depends on VSA_DUMP_FLAG
  STACK<CODEREP*> *_stack;       // solver stack
  MEM_POOL        *_mem_pool;

  SCC(void);                     // REQUIRED UNDEFINED UNWANTED methods
  SCC(const SCC&);               // REQUIRED UNDEFINED UNWANTED methods
  SCC& operator = (const SCC&);  // REQUIRED UNDEFINED UNWANTED methods

  INT       Id(void) const       { return _id; }
  INT       Scc_ecnt(void) const { return _scc_ecnt; }
  void      Inc_scc_ecnt(void)   { ++_scc_ecnt; }
  INT       Low(INT i) const     { return (*_scc_vec)[i]->Low(); }
  void      Set_low(INT i, INT v){ (*_scc_vec)[i]->Set_low(v); }
  CODEREP  *Ids(INT i) const     { return (*_scc_vec)[i]->Id(); }
  BOOL      Onstack(INT i) const { return (*_scc_vec)[i]->On_stack(); }
  void      Dfs(CODEREP *x, VSA *vsa, hash_set<IDTYPE>&);
  void      Push(CODEREP *x)     {
                                   SCC_NODE * node = CXX_NEW(SCC_NODE(x, _id, TRUE), _mem_pool);
                                   _scc_vec->push_back(node);
                                   _id++;
                                   _stack->Push(x);
                                 }
  INT       Lookup(const CODEREP *x)const{
                                   for (INT i = 0; i < _scc_vec->size(); ++i) {
                                     if (Ids(i) == x)
                                       return i;
                                   }
                                   return -1;  // could not find x!
                                 }

public:
  SCC(CODEREP *x, VSA *vsa, MEM_POOL *mem_pool):
    _id(0), _scc_ecnt(1), _mem_pool(mem_pool)
  {
    OPT_POOL_Push(mem_pool, VSA_DUMP_FLAG);
    _scc_vec = CXX_NEW(SCC_NODE_VEC(SCC_NODE_VEC::allocator_type(mem_pool)), mem_pool);
    _stack = CXX_NEW(STACK<CODEREP*>(mem_pool), mem_pool);
    _tracing = Get_Trace(TP_WOPT2, VSA_DUMP_FLAG);
    hash_set<IDTYPE> visited_bb;
    Dfs(x, vsa, visited_bb);
    // Is_Trace_cmd(Tracing(), Print(TFile));
  }
  ~SCC(void) { OPT_POOL_Pop(_mem_pool, VSA_DUMP_FLAG); }

  BOOL      Tracing(void) const  { return _tracing; }
  BOOL      Defer(CODEREP *x) const;
  void      Update(void) const;
  void      Print(FILE *) const;

};

// ====================================================================
//
// put it here to expedite the filter development
//
// ====================================================================
typedef enum _vsa_filter_kind {
  VK_UNKNOWN    = 0,
  VK_IDENT_ASGN = 1,
  VK_INITED_GLB = 2,
} VSA_FILTER_KIND;
  
typedef mempool_allocator<WN*> WNP_ALLOCATOR;
typedef vector<WN*, WNP_ALLOCATOR> WNP_VECTOR;

class VSA_FILTER {

private:
  VSA_FILTER_KIND  _kind;
  union {
    WNP_VECTOR    *_wnp_tab;
  } _fil;
  MEM_POOL        *_mem_pool;

  VSA_FILTER(void);                           // REQUIRED UNDEFINED UNWANTED methods
  VSA_FILTER(const VSA_FILTER&);              // REQUIRED UNDEFINED UNWANTED methods
  VSA_FILTER& operator = (const VSA_FILTER&); // REQUIRED UNDEFINED UNWANTED methods

public:
  VSA_FILTER(VSA_FILTER_KIND kind, MEM_POOL *mem_pool) :
    _kind(kind),
    _mem_pool(mem_pool)
  { }
  ~VSA_FILTER() { }

  WNP_VECTOR *Wnp_tab(void) const             { return _fil._wnp_tab; } 
  void        Set_wnp_tab(WNP_VECTOR *wnpv)   { _fil._wnp_tab = wnpv; }
  void        Append_wnp(WN *wnp)             { _fil._wnp_tab->push_back(wnp); }
  BOOL        Is_ident_asgn(COMP_UNIT *cu, const WN *wn) const;
  void        Print(FILE *fp) const;
}; // end of VSA_FILTER


class VSA_FILT_HANDLER {

private:
  VSA_FILTER  _filter_tabs;                  // will expand to vector for more filters
  WNP_VECTOR  _ident_asgn_filt;              // filter for identity assignments
  MEM_POOL   *_mem_pool;

  VSA_FILT_HANDLER(void);                                 // REQUIRED UNDEFINED UNWANTED methods
  VSA_FILT_HANDLER(const VSA_FILT_HANDLER&);              // REQUIRED UNDEFINED UNWANTED methods
  VSA_FILT_HANDLER& operator = (const VSA_FILT_HANDLER&); // REQUIRED UNDEFINED UNWANTED methods
  VSA_FILTER *Filter_tabs(void)              { return &_filter_tabs; }

public:
  VSA_FILT_HANDLER(MEM_POOL *mem_pool) :
    _mem_pool(mem_pool),
    _filter_tabs(VK_IDENT_ASGN,_mem_pool),
    _ident_asgn_filt(0, (WN*)NULL, WNP_VECTOR::allocator_type(mem_pool))
  {
    _filter_tabs.Set_wnp_tab(&_ident_asgn_filt);
  }

  ~VSA_FILT_HANDLER() { }

  BOOL        Is_ident_asgn(COMP_UNIT *cu, const WN *wn) { return _filter_tabs.Is_ident_asgn(cu, wn); }
  void        Append_ident_asgn(WN *asgn)
  {
    Is_True(this, ("call VSA_FILT_HANDLER::Append_ident_asgn with NULL pointer"));
    Filter_tabs()->Append_wnp(asgn);
  }
}; // end of VSA_FILT_HANDLER


//==============================================================================
// Rule based checker: rule id definition
//==============================================================================
#include "rbc_rule_defs.h"

//==============================================================================
// VSA stands for Vulnerability Static Analysis
// The VSA class is the management object for this functionality, following the
// Mongoose's WOPT project. 
//==============================================================================

typedef std::pair<HEAP_OBJ_REP*, IDTYPE> HOR_PAIR;
typedef std::pair<VSYM_OBJ_REP*, IDTYPE> VOR_PAIR;
typedef std::pair<FSM_OBJ_REP*, IDTYPE> FOR_PAIR;

typedef pair<FILE_ST_IDX, AUX_ID>          FST_AUX_PAIR;
typedef mempool_allocator<FST_AUX_PAIR>    FST_AUX_ALLOCATOR;
typedef hash_map<FILE_ST_IDX, AUX_ID, __gnu_cxx::hash<FILE_ST_IDX>,
                 std::equal_to<FILE_ST_IDX>, FST_AUX_ALLOCATOR> FST_AUX_MAP;

typedef pair<FILE_ST_IDX, FILE_ST_IDX>     FST_FST_PAIR;
typedef mempool_allocator<FST_FST_PAIR>    FST_FST_ALLOCATOR;
typedef hash_map<FILE_ST_IDX, FILE_ST_IDX, __gnu_cxx::hash<FILE_ST_IDX>,
                 std::equal_to<FILE_ST_IDX>, FST_FST_ALLOCATOR> FST_FST_MAP;

typedef mempool_allocator<AUX_ID>          AUX_ALLOCATOR;
typedef hash_set<AUX_ID, __gnu_cxx::hash<AUX_ID>,
                 std::equal_to<AUX_ID>, AUX_ALLOCATOR>          AUX_SET;

typedef pair<IDTYPE, AUX_SET*>             AUX_SET_PAIR;
typedef mempool_allocator<AUX_SET_PAIR>    AUX_SET_ALLOCATOR;
typedef hash_map<IDTYPE, AUX_SET*, __gnu_cxx::hash<IDTYPE>,
                 std::equal_to<IDTYPE>, AUX_SET_ALLOCATOR>      AUX_SET_MAP;

typedef pair<AUX_ID, BB_LIST*>             AUX_DEF_PAIR;
typedef mempool_allocator<AUX_DEF_PAIR>    AUX_DEF_ALLOCATOR;
typedef hash_map<AUX_ID, BB_LIST*, __gnu_cxx::hash<AUX_ID>,
                 std::equal_to<AUX_ID>, AUX_DEF_ALLOCATOR>      AUX_DEF_MAP;

typedef std::pair<IDTYPE, PHI_NODE*>                            ID_PHI_PAIR;
typedef hash_map<IDTYPE, PHI_NODE*,
                 __gnu_cxx::hash<IDTYPE>,
                 std::equal_to<IDTYPE>,
                 mempool_allocator<ID_PHI_PAIR> >               ID_PHI_MAP;
typedef std::vector<ID_PHI_MAP*>                                PHI_CACHE;

typedef struct
{
  size_t operator() (VSYM_FLD_REP *k) const { return (size_t)k; }
} hash_vfr;

typedef struct {
  size_t operator() (CODEREP *cr) const { return (size_t)cr; }
} hash_coderep;

typedef pair<VSYM_FLD_REP*, VFR_CANDS*>                         VFR_CAND_PAIR;
typedef mempool_allocator<VFR_CAND_PAIR>                        VFR_CAND_ALLOCATOR;
typedef hash_map<VSYM_FLD_REP*, VFR_CANDS*, 
                 hash_vfr, std::equal_to<VSYM_FLD_REP*>, 
                 VFR_CAND_ALLOCATOR>                            VFR_CAND_MAP;

typedef pair<INTPTR, FOR_ARRAY*>                                VOR_FORARR_PAIR;
typedef mempool_allocator<VOR_FORARR_PAIR>                      VOR_FORARR_ALLOCATOR;
typedef hash_map<INTPTR, FOR_ARRAY*,
                 __gnu_cxx::hash<INTPTR>,
                 std::equal_to<INTPTR>,
                 VOR_FORARR_ALLOCATOR>                          VOR_FORARR_MAP;

typedef pair<IDTYPE, IDTYPE>                                    HEAP_OBJ_PAIR;
typedef mempool_allocator<HEAP_OBJ_PAIR>                        HEAP_OBJ_ALLOCATOR;
typedef hash_map<IDTYPE, IDTYPE,
                 __gnu_cxx::hash<IDTYPE>,
                 __gnu_cxx::equal_to<IDTYPE>,
                 HEAP_OBJ_ALLOCATOR>                            HEAP_OBJ_MAP;
typedef hash_set<CODEREP*,
                 hash_coderep, std::equal_to<CODEREP*>,
                 mempool_allocator<HEAP_OBJ*> >                 CODEREP_SET;

using idmap::ID_MAP_ITER;

typedef pair<INTPTR, INTPTR>                                    VOR_VALUE_PAIR;
typedef mempool_allocator<VOR_VALUE_PAIR>                       VOR_VALUE_ALLOCATOR;
typedef hash_map<INTPTR, INTPTR,
                 __gnu_cxx::hash<INTPTR>,
                 std::equal_to<INTPTR>,
                 VOR_VALUE_ALLOCATOR>                           VOR_VALUE_MAP;

class VSA {
  friend class CR_UTIL;
  friend class CDA_BUILDER;
  friend class VALUE_GRAPH;
  friend class LOCAL_CPROP;
  friend class LOCAL_SPROP;
  friend class HOA_PROP;
  friend class HOA_BUILDER;
  friend class HOR_FINDER;
  friend class DNA_DU_BUILDER;
  friend class DNA_NODE;
  friend class VSA_OOB_CHECKER;
  friend class NPD_CHECKER;
  friend class UIV_CHECKER;
  friend class HEAP_CHECKER;
  friend class VAR_DEF_TRAV;
  friend class TAG_CHECKER;
  friend class ICALL_TARGET_FINDER;
  template<CHECKER_STATUS _STATE> friend class GLOBAL_VAR_HELPER;
  friend class HEAP_VSYM_ANALYSIS;
  friend class HEAP_OBJ_CREATION;
  friend class HEAP_OBJ_RENAMING;
  friend class VSYM_OBJ_CREATION;
  friend class VSYM_OBJ_RENAMING;
  friend class HVA_HO_CREATION;
  friend class HVA_HO_RENAMING;
  friend class HVA_VO_CREATION;
  friend class HVA_VO_RENAMING;
  friend class TAG_PROP;
  friend class BIND_TOR_LIST_HELPER_BASE;
  template<TOR_DEF_ATTR attr> friend class BIND_TOR_LIST_HELPER;
  friend class UNI_PARAMETER_HELPER;
  friend class UNI_GLOBAL_VAR_HELPER;
  friend class UNI_OUTPUT_VAR_HELPER;
  friend class UNI_RETURN_VAR_HELPER;
  friend class VSYM_PARAMETER_HELPER;
  friend class VSYM_GLOBAL_VAR_HELPER;
  friend class VSYM_RETURN_VAR_HELPER;
  friend class VSYM_OUTPUT_VAR_HELPER;
  friend class VSYM_TRACKER;
  friend class TRAV_CONTEXT;
  friend class RBC_CONTEXT;
  friend class RBC_BASE;
  friend class JNI_CHECKER_HELPER;
  friend class IPSA;
  friend class VRA;
  friend class SRCPOS_HANDLE;
  friend class RNA_NODE;
  template<typename _CHECKER> friend class UD_TRAVELER;
  template<typename _CHECKER> friend class CHECKER_TRAVELER;
  friend class CONTAINER_UD_HELPER;
  friend struct STMT_HOR_SET;

private:
  typedef ID_MAP<EXP_OCCURS*, IDTYPE> EXPOCC_MAP;
  typedef ID_MAP<PHI_LIST*, IDTYPE> PHILIST_MAP;
  typedef ID_MAP<RNA_NODE*, IDTYPE> RNANODE_MAP;
  typedef ID_MAP_ITER<RNA_NODE*, IDTYPE> RNANODE_MAP_ITER;
  typedef ID_MAP<HEAP_OBJ_REP*, IDTYPE> HOBJREP_MAP;
  typedef ID_MAP<VSYM_OBJ_REP*, IDTYPE> VOBJREP_MAP;
  typedef ID_MAP<MU_LIST*, IDTYPE>  MULIST_MAP;
  typedef ID_MAP<CHI_LIST*, IDTYPE> CHILIST_MAP;
  typedef ID_MAP<HOR_ARRAY*, IDTYPE> HORARR_MAP;
  typedef ID_MAP<FOR_ARRAY*, IDTYPE> FORARR_MAP;
  typedef ID_MAP<CODEREP*, IDTYPE> CODEREP_MAP;
  typedef ID_MAP<UINT32, IDTYPE> UINT32_MAP;
  typedef ID_MAP<INTPTR, IDTYPE> TOR_LIST_MAP;

  COMP_UNIT    *_cu;                    // cu that VSA analyzes currently
  OPT_STAB     *_opt_stab;              // the optimizer symtab
  CFG          *_cfg;                   // the control flow graph
  CODEMAP      *_htable;                   // the hash table
  IPSA         *_ipsa_mgr;                 // IPSA context and methods
  HO_LIST      *_heap_obj_list;            // the list of heap obj identified
  VO_LIST      *_vsym_obj_list;            // the list of vsym obj identified
  FO_LIST      *_fsm_obj_list;             // the list of fsm obj identified
  TOR_LIST_OLD *_tag_obj_rep_list;         // the list of tag obj identified (not used, to be deleted)
  MEM_POOL     *_mem_pool;                 // the permenant pool for new nodes
  MEM_POOL     *_loc_pool;                 // the temporary pool for VSA phase
  STMTREP_LIST *_pending_ref;              // reference to pointer
  HEAP_OBJ_REP *_null_hor;                 // represent (void *)NULL
  VSYM_OBJ_REP *_null_vor;                 // represent *(void *)NULL
  HEAP_OBJ_MAP **_ignore_ho_map;           // ho map which should be ignored by MSF check at BB
                                           // say: p = malloc(...);
                                           //      if (p == 0)  return; <-- HO on p should be ignored
  MEM_POOL     *_ignore_ho_map_mp;         // mempool for _ignore_ho_map
  BOOL          _disabled;                 // disabled vsa currently
  BOOL          _tracing;                  // depends on VSA_DUMP_FLAG
  BOOL          _past_ret_reg_def;         // true if currently working in a region between
                                           // a def of a dedicated return preg and RETURN
  IDTYPE        _last_heap_obj_id;         // track the heap_obj
  IDTYPE        _last_vsym_obj_id;         // track the vsym_obj
  IDTYPE        _last_tor_id;              // track the TAG_OBJ_REP id for old tag impl, to be deleted 

  EXPOCC_MAP    _cr_2_expocc;              // table mapping cr_id to exp_occurs
  PHILIST_MAP   _bb_ho_philist;            // add heap_obj philist to BB_NODE
  RNANODE_MAP   _sr_2_rna_map;             // map sr_id -> it's value
  HOBJREP_MAP   _cr_2_heap_obj_map;        // map cr_id -> heap_obj_rep
  HOBJREP_MAP   _cr_2_heap_obj_refmap;     // map cr_id -> heap_obj_rep reference
  VOBJREP_MAP   _cr_2_vor_map;             // map cr_id -> vsym_obj_rep
  FORARR_MAP    _sr_2_for_array_map;       // map sr_id -> fsm_obj_rep array, for FSM transits
  FORARR_MAP    _cr_2_for_array_map;       // map cr_id -> fsm_obj_rep array, for FSM keys
  VOR_FORARR_MAP _vor_2_for_array_map;     // map vor -> fsm_obj_rep array, for FSM keys

  MULIST_MAP    _stmt_hor_mu_map;          // map stmt_id -> mu_list w/ hor
  CHILIST_MAP   _stmt_hor_chi_map;         // map stmt_id -> chi_list w/ hor
  MULIST_MAP    _stmt_vor_mu_map;          // map stmt_id -> mu_list w/ vor
  CHILIST_MAP   _stmt_vor_chi_map;         // map stmt_id -> chi_list w/ vor
  MULIST_MAP    _stmt_fsm_mu_map;          // map stmt_id -> mu_list w/ mixed fsm
  CHILIST_MAP   _stmt_fsm_chi_map;         // map stmt_id -> chi_list w/ mixed fsm
  PHILIST_MAP   _bb_vo_philist;            // add vsym_obj philist to BB_NODE
  PHILIST_MAP   _bb_fo_philist;            // add fsm_obj philist to BB_NODE
  HORARR_MAP    _ret_2_hor_array_map;      // map ret bb to hor array
  BB_LIST      *_call_bb_list;             // list of call bbs where pointer escapes
  VFR_CAND_MAP  _vfr_cand_map;             // map vfr -> all candidates
  VR_MAP        _value_range_map;          // cr_id -> value range map
 
  VSA_FILT_HANDLER _filt_handler;          // serve the error message filter

  TOR_LIST_MAP   _cr_2_tor_list_map;       // map cr_id -> tor_list
  TAG_PROP      *_tag_prop;                // local tag propagator
  CODEREP_SET   *_ignore_cr_set;           // for reducing output file size, some checkers may ignore part of issues
  VOR_VALUE_MAP  _vor_2_value_objs;        // map vor -> value_objs

  VSA(void);                               // REQUIRED UNDEFINED UNWANTED methods
  VSA(const VSA&);                         // REQUIRED UNDEFINED UNWANTED methods
  VSA& operator = (const VSA&);            // REQUIRED UNDEFINED UNWANTED methods

  COMP_UNIT    *Comp_unit(void) const      { return _cu; }
  CFG          *Cfg(void) const            { return _cfg; }
  CODEMAP      *Htable(void)const          { return _htable; }
  EH_TABLE     *EH_table(void) const       { return Comp_unit()->EH_table(); }
  IPSA         *Ipsa(void) const
  {
    Is_True(_ipsa_mgr != NULL, ("_ipsa_mgr is NULL"));
    return _ipsa_mgr;
  }
  HO_LIST      *Heap_obj_list(void)const   { return _heap_obj_list; }
  HEAP_OBJ     *Find(IDTYPE symid, BOOL is_lda) const
                                           { return _heap_obj_list->Find(symid, is_lda); }
  HEAP_OBJ     *Find(AUX_STAB_ENTRY *s, BOOL is_lda)
                                           { return _heap_obj_list->Find(this, s, is_lda); }
  VO_LIST      *Vsym_obj_list(void)const   { return _vsym_obj_list; }
  VSYM_OBJ     *Find(HEAP_OBJ_REP *hor, VSYM_FLD_REP* vfr) const
                                           { Is_True(_vsym_obj_list->Find(hor, vfr) == hor->Find(vfr), ("bad hor vo list"));
                                             return hor->Find(vfr); }
  FO_LIST      *Fsm_obj_list(void)const    { return _fsm_obj_list; }
  FSM_OBJ      *Find(STRING name)const     { return _fsm_obj_list->Find(name); }

  TOR_LIST_OLD *Tag_obj_rep_list(void)const    { return _tag_obj_rep_list; }
  TAG_OBJ_REP  *Find_tag(TAG_OBJ_REP*tor)const { return _tag_obj_rep_list->Find(tor); }

  PHILIST_MAP  *Bb_ho_philist(void)        { return &_bb_ho_philist; }
  PHILIST_MAP  *Bb_vo_philist(void)        { return &_bb_vo_philist; }
  PHILIST_MAP  *Bb_fo_philist(void)        { return &_bb_fo_philist; }
  HORARR_MAP   *Bb_horarr_map(void)        { return & _ret_2_hor_array_map; }

  char         *Cur_pu_name(void)const     { return Dna()->Fname(); }
  DNA_NODE     *Dna(void) const            { return Comp_unit()->Dna(); }
  TAG_PROP     *Tag_prop(void) const       { return _tag_prop;          }
  void          Set_tag_prop(TAG_PROP *p)  { _tag_prop = p;             }
  IDTYPE        Last_tor_id()              { return _last_tor_id;       }

  // heap object based analysis functions
  void          Merge(HEAP_OBJ *to, HEAP_OBJ *from);
  void          Merge(HEAP_OBJ *heap_obj, HEAP_OBJ_REP *hor);

  // heap_obj_rep creation
  void          Handle_call(STMTREP *call_stmt, BB_NODE *bb, MEM_POOL *pool);
  void          Handle_call_sideffect(STMTREP *call_stmt);
  void          Prepend_ref(STMTREP *sr){
    Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
             (TFile, "<<<<<<<< VSA::Prepend_ref sr%d >>>>>>>>\n", sr->Stmtrep_id()));
    Is_True(sr->Opr() == OPR_CALL, ("VSA::Prepend_ref is called for SR that is not OPR_CALL"));
    _pending_ref = _pending_ref->Prepend(sr, Loc_pool());
  }
  void          Update_reaching_def(CODEREP *cr, HEAP_OBJ_REP *hor, MEM_POOL *def_bbs_pool, hash_set<IDTYPE>&);
  HEAP_OBJ_REP *Find_real_phi_def(CODEREP *x, hash_set<IDTYPE>&);
  void          Update_pending_ref(MEM_POOL *def_bbs_pool);

  // heap object rep rename phase
  HEAP_OBJ_REP *Gen_phi(PHI_NODE *phi, HEAP_OBJ *heap_obj);
  void          Place_ho_phi_node(void);
  void          Perform_hor_unification(BB_NODE *bb);                // unify hor for a pointer
  BOOL          Hor_same_ulist(HEAP_OBJ_REP *a, HEAP_OBJ_REP *b) const;
  void          Update_ulist_w_ho_rename(HEAP_OBJ_REP *ho, HEAP_OBJ_REP *prev, STMTREP *stmt, ROR_ATTR attr);
  void          Update_ulist_w_free_rename(HEAP_OBJ_REP *ho, HEAP_OBJ_REP *prev, STMTREP *call_stmt)
                                       { Update_ulist_w_ho_rename(ho, prev, call_stmt, ROR_DEF_BY_FREE); }
  void          Update_ulist_w_ho_rename_rev(HEAP_OBJ_REP *hor, STMTREP *call_stmt);
  void          Update_ulist_w_free_rename_rev(HEAP_OBJ_REP *hor, STMTREP *call_stmt)
                                       { Update_ulist_w_ho_rename_rev(hor, call_stmt); }
  void          Generate_ho_exit_mu(BB_NODE *bb);
  void          Rename_call(STMTREP *call_stmt);
  void          Rename_hochi(STMTREP *stmt, BOOL fwd);
  void          Rename_horef(CODEREP *cr, STMTREP *enclosing_stmt);
  void          Rename(BB_NODE *bb);
  void          Verify_heap_obj_stack(void);
    
  VSYM_OBJ_REP *Gen_phi(PHI_NODE *phi, VSYM_OBJ *vsym_obj);
  void          Place_vo_phi_node(void);
  void          Find_coderep_in_cd_bbs(BB_NODE*, hash_map<CODEREP*, STMTREP*>&) const;
  void          Find_coderep_in_last_stmtrep(BB_NODE*, hash_set<CODEREP*>&) const;
  BOOL          Is_conditional_free(STMTREP *stmt) const;
  HEAP_OBJ_MAP* Find_ignore_ho_ids(BB_NODE*);
  void          Rename_vocall(STMTREP *call_stmt, MEM_POOL *def_bbs_pool);
  void          Rename_voref(CODEREP *cr);
  void          Rename_vsym(BB_NODE *bb, MEM_POOL *def_bbs_pool);
  void          Verify_vsym_obj_stack(void);

  // vsym_obj_rep creation and rename phase
  template<typename T>
  void          Add_field_heap_obj(HEAP_OBJ_REP *hor, hash_map<uintptr_t, T>& ho_map,
                                   const T& value);
  MU_NODE      *Create_stmt_vsym_mu(CODEREP *cr, STMTREP *stmt, VSYM_OBJ_REP *vor);
  CHI_NODE     *Create_stmt_vsym_chi(CODEREP *cr, STMTREP *stmt, VSYM_OBJ_REP *vor,
                                     MEM_POOL *pool);
  void          Rename_entry_vsym_chi(STMTREP *stmt, MEM_POOL *pool, BOOL fwd);
  void          Create_stmt_vsym_mu_chi(CODEREP *cr, STMTREP* stmt, BOOL chi, BOOL mu,
                                        MEM_POOL *pool);
  void          Create_call_vsym_mu_chi(STMTREP *stmt, MEM_POOL *pool);
  void          Create_return_vsym_mu(STMTREP *stmt, MEM_POOL *pool);
  void          Create_refn_vsym_mu(CODEREP *cr, STMTREP *stmt, HEAP_OBJ_REP *hor,
                                    IDTYPE ifldid, mINT32 ofst, MEM_POOL *pool, VS_FLD_KIND fld_k = FLD_K_ID);
  void          Create_refn_vsym_chi(CODEREP *cr, STMTREP *stmt,  HEAP_OBJ_REP *hor,
                                     IDTYPE ifldid, mINT32 ofst, MEM_POOL *pool, VS_FLD_KIND fld_k = FLD_K_ID);
  void          Create_refn_vsym(CODEREP *cr, STMTREP *stmt, MEM_POOL *def_bbs_pool,
                                 BOOL handle_call = FALSE);
  
  // fsm_obj_rep creation and rename phase
  void          Identify_fsm_insts(BB_NODE *bb, MEM_POOL *def_bbs_pool);
  void          Identify_fsm_insts_stmt(STMTREP *stmt, BB_NODE *bb, MEM_POOL *def_bbs_pool);
  FSM_MATCH_KIND Is_action_match(STMTREP *sr, const char* ft_type, const char* ft_value);
  void          Add_new_for(STMTREP *sr, BB_NODE *bb, MEM_POOL *def_bbs_pool,
                            STRING fsm_name, const char *action, DNA_NODE *callee = NULL);
  void          Place_fo_phi_node(void);
  FSM_OBJ_REP  *Gen_phi(PHI_NODE *phi, FSM_OBJ *fsm_obj);
  void          Print_fo_list(FILE *fp) const { _fsm_obj_list->Print(fp); }
  
  // fsm helper function
  BOOL          Is_candidate_action(STMTREP *call, const char *action1, const char *action2); 

  // tag_ob_rep creation and rename phase
  void          Propagate_tag_insts(BB_NODE *bb, MEM_POOL *def_bbs_pool, BOOL &changed);
  void          Propagate_tag_insts_stmt(STMTREP *stmt, BB_NODE *bb, MEM_POOL *def_bbs_pool);
  void          Print_to_list(FILE *fp) const { _tag_obj_rep_list->Print(fp); }

  // ipsa work
  CODEREP      *Find_actual_arg(STMTREP *call_stmt) const;

  VAL_RANGE_RESULT Check_val_range(CODEREP *cr, BB_NODE *bb, INT32 ubd, INT32 lbd);
  VAL_RANGE_RESULT Check_val_range(CODEREP *cr, BB_NODE *bb);
  VAL_RANGE_RESULT Check_var_zero(CODEREP *cr, BB_NODE *bb, BOOL& is_vra);
  VAL_RANGE_RESULT Check_expr_zero(CODEREP *cr, BB_NODE *bb, BOOL& is_vra);
  // check if expr at bb is in range of [lb, lb + size)
  VAL_RANGE_RESULT Check_expr_in_range(CODEREP *expr, BB_NODE* bb, CODEREP *lb, CODEREP *size);
  // check if expr at bb is out of range of [lb, lb + size)
  VAL_RANGE_RESULT Check_expr_out_range(CODEREP *expr, BB_NODE* bb, CODEREP *lb, CODEREP *size);

  // Do not call these functions directly
  BOOL          Is_path_possible_local(CODEREP* val, CODEREP* cmp, BB_NODE* bb, const PATH_SELECTED& path) const;
  BOOL          Is_path_possible_from_caller(RNA_NODE* rna, IDTYPE param, CODEREP* cmp,
                                             CALL_STACK& cs, VSYM_TRACKER* tracker, const PATH_SELECTED& path) const;
  BOOL          Is_path_possible_from_caller(IDTYPE param, CODEREP* cmp, CALL_STACK& cs, VSYM_TRACKER* tracker,
                                             const PATH_SELECTED& path) const;
  BOOL          Is_path_possible_from_callee(RNA_NODE* rna, IDTYPE param, CODEREP* cmp, CALL_STACK& cs,
                                             VSYM_TRACKER* tracker, const PATH_SELECTED& path) const;
  BOOL          Is_path_possible_var(CODEREP* var, BB_NODE* bb, CODEREP* cmp_expr, CALL_STACK& cs, VSYM_TRACKER* tracker,
                                 const PATH_SELECTED& path) const;
  BOOL          Is_path_possible_ivar(CODEREP* var, BB_NODE* bb, CODEREP* cmp_expr, CALL_STACK& cs, VSYM_TRACKER* tracker,
                                 const PATH_SELECTED& path) const;
  BOOL          Is_path_possible(CODEREP* var, BB_NODE* bb, CODEREP* cmp_expr, CALL_STACK& cs, VSYM_TRACKER* tracker,
                                 const PATH_SELECTED& path) const;
  BOOL          Is_path_possible(BB_NODE* pred, BB_NODE* succ, CALL_STACK& cs, const PATH_SELECTED& path) const;
  // call these functions instead of above
  BOOL          Is_path_possible(BB_NODE* use_bb, PHI_NODE* phi, INT opnd) const;
  BOOL          Is_path_possible(STMTREP* sr, CALL_STACK& cs, const SRCPOS_HANDLE* sp_h) const;

  HEAP_OBJ_REP *Allocate_heap_obj(HEAP_OBJ *ho, HEAP_OBJ_REP *prev)
  { return CXX_NEW(HEAP_OBJ_REP( ho, prev ), Mem_pool());  }

  HEAP_OBJ_REP *Allocate_heap_obj(CODEREP *cr, BB_NODE *bb)
  {
    HEAP_OBJ *heap_obj;
    IDTYPE id = (_ipsa_mgr)?_ipsa_mgr->New_heapobj_id():_last_heap_obj_id++;
    heap_obj = CXX_NEW(HEAP_OBJ(id, cr, Mem_pool()), Mem_pool());
    VSA_STATS_inc(ho);
    _heap_obj_list->Append(heap_obj);
    HEAP_OBJ_REP *entry_chi = Allocate_heap_obj(heap_obj, NULL); // entry_chi version
    entry_chi->Set_version_entry_chi();
    entry_chi->Set_srcpos_node((STMTREP*)NULL, NULL, PATHINFO_NONE);
    entry_chi->Set_attr(ROR_DEF_BY_CHI);
    heap_obj->Set_stack(CXX_NEW( STACK<HOR_PAIR>(Mem_pool()), Mem_pool() ));
    heap_obj->Push(entry_chi, 0);    // push entry chi to stack
    heap_obj->Set_entry_chi(entry_chi);
    return entry_chi;
  }

  HEAP_OBJ_REP *Allocate_heap_obj(CODEREP *cr, BB_NODE *bb, MEM_POOL *def_bbs_pool)
  {
    HEAP_OBJ_REP *entry_chi = Allocate_heap_obj(cr, bb);
    if (def_bbs_pool == NULL)
      return entry_chi;
    HEAP_OBJ     *heap_obj = entry_chi->Heap_obj();
    // for real occurrence, we need to append def bbs and return an hor
    heap_obj->Prepend_def_bbs(bb, def_bbs_pool);
    return CXX_NEW(HEAP_OBJ_REP( heap_obj, NULL ), Mem_pool());
  }
  HEAP_OBJ_REP *Clone_heap_obj(HEAP_OBJ_REP *hor,  BB_NODE *bb, MEM_POOL *def_bbs_pool)
  {
    Is_True(hor != Null_hor(), ("try to clone null vor"));
    hor->Heap_obj()->Prepend_def_bbs(bb, def_bbs_pool);
    HEAP_OBJ_REP *nw_hor = CXX_NEW(HEAP_OBJ_REP( hor->Heap_obj(), hor ), Mem_pool());
    return nw_hor;
  }

  VSYM_OBJ_REP *Allocate_vsym_obj(VSYM_OBJ *vo) { return CXX_NEW(VSYM_OBJ_REP(vo), Mem_pool()); }
  VSYM_OBJ_REP *Allocate_vsym_obj(BB_NODE *bb, HEAP_OBJ_REP *hor,
                                  VSYM_FLD_REP *fld_rep, MEM_POOL *def_bbs_pool)
  {
    if (!VSA_New_HVA) {
      if (hor == Null_hor())
        return Null_vor();
      if (hor->Ulist() != NULL) {
        Is_True(hor->Ulist()->Head(), ("ulist head is NULL"));
        hor = hor->Ulist()->Head()->Hor();
      }
    }
    else {
      Is_True(hor != Null_hor(), ("try to create vor for null hor"));
    }
    VSYM_OBJ *vsym_obj = Find(hor, fld_rep);
    if (vsym_obj == NULL) { 
      vsym_obj = CXX_NEW(VSYM_OBJ((_ipsa_mgr)?_ipsa_mgr->New_vsymobj_id():_last_vsym_obj_id++, *fld_rep, Mem_pool()), Mem_pool());
      VSA_STATS_inc(vo);
      vsym_obj->Set_kind(RSC_KIND_VSYM);
      vsym_obj->Set_base_hor(hor);
      if (hor->Vsym_obj()) {
        // append after hor's first vsym_obj
        if (_vsym_obj_list->Tail() == hor->Vsym_obj())
          _vsym_obj_list->Append(vsym_obj);
        else
          hor->Vsym_obj()->Insert_After(vsym_obj);
      }
      else {
        hor->Set_vsym_obj(vsym_obj);
        // append to _vsym_obj_list tail
        _vsym_obj_list->Append(vsym_obj);
      }
      vsym_obj->Set_stack(CXX_NEW( STACK<VOR_PAIR>(Mem_pool()), Mem_pool() ));
      VSYM_OBJ_REP *entry_chi = Allocate_vsym_obj(vsym_obj); // entry_chi version
      entry_chi->Set_version_entry_chi();
      entry_chi->Set_srcpos_node((STMTREP*)NULL, NULL, PATHINFO_CHI);
      entry_chi->Set_attr(ROR_DEF_BY_CHI);
      vsym_obj->Push(entry_chi, 0);    // push entry chi to stack
      vsym_obj->Set_entry_chi(entry_chi);
    }

    if (bb)
      vsym_obj->Prepend_def_bbs(bb, def_bbs_pool);   // prepend only if bb is defined
    else
      return vsym_obj->Entry_chi(); // not a def, return its entry chi

    VSYM_OBJ_REP *vor = Allocate_vsym_obj(vsym_obj);
    return vor;
  }
  VSYM_OBJ_REP *Allocate_vsym_obj( BB_NODE *bb, HEAP_OBJ_REP *hor, IDTYPE ifldid, mINT32 ofst,
                                   MEM_POOL *def_bbs_pool, VS_FLD_KIND kind = FLD_K_ID)
  {
    if (!VSA_New_HVA) {
      if (hor == Null_hor())
        return Null_vor();
      if (hor->Ulist() != NULL) {
        Is_True(hor->Ulist()->Head(), ("ulist head is NULL"));
        hor = hor->Ulist()->Head()->Hor();
      }
    }
    else {
      Is_True(hor != Null_hor(), ("try to create vor for null hor"));
    }
    if (kind == FLD_K_ID) {
      ifldid = Synthesize_fldid(hor, ifldid, ofst);
    }
    VSYM_FLD_REP fld_rep = VSYM_FLD_REP(kind, ifldid, ofst);
    return Allocate_vsym_obj(bb, hor, &fld_rep, def_bbs_pool);
  }

  VSYM_OBJ_REP *Clone_vsym_obj(VSYM_OBJ_REP *vor, BB_NODE *bb, MEM_POOL *def_bbs_pool)
  {
    Is_True(vor != Null_vor(), ("try to clone null vor"));
    vor->Vsym_obj()->Prepend_def_bbs(bb, def_bbs_pool);
    return CXX_NEW(VSYM_OBJ_REP(vor->Vsym_obj()), Mem_pool());
  }


  FSM_OBJ_REP  *Allocate_fsm_obj(FSM_OBJ *fo)
  { return CXX_NEW(FSM_OBJ_REP( fo, NULL, Mem_pool() ), Mem_pool());  }

  // this function is specific to VSA since the bb won't be valid at IPSA level
  FSM_OBJ_REP  *Allocate_fsm_obj( BB_NODE *bb, STRING fsm_name, MEM_POOL *def_bbs_pool);

  // this function is specific to VSA since the bb won't be valid at IPSA level
  FSM_OBJ_REP  *Clone_fsm_obj(FSM_OBJ_REP *fsmor,  BB_NODE *bb, MEM_POOL *def_bbs_pool)
  {
    fsmor->Fsm_obj()->Prepend_def_bbs(bb, def_bbs_pool);
    return CXX_NEW(FSM_OBJ_REP( fsmor->Fsm_obj(), NULL, Mem_pool() ), Mem_pool());
  }

  // Map functions ...
  void          Enter_bb_ho_philist(BB_NODE *bb, PHI_LIST *pl)
  {
    /*if (_bb_ho_philist.Lookup(bb->Id()) == NULL)*/ _bb_ho_philist.Insert(bb->Id(), pl);
  }
  PHI_LIST     *Bb_ho_philist(BB_NODE *bb) const
                                           { return _bb_ho_philist.Lookup(bb->Id());}
  PHI_NODE     *Search_phi_node(PHI_LIST *list, HEAP_OBJ *ho);

  void          Enter_bb_vo_philist(BB_NODE *bb, PHI_LIST *pl)
                                           { _bb_vo_philist.Insert(bb->Id(), pl);  }
  PHI_LIST     *Bb_vo_philist(BB_NODE *bb) const
                                           { return _bb_vo_philist.Lookup(bb->Id());}
  PHI_NODE     *Search_phi_node(PHI_LIST *list, VSYM_OBJ *vo);

  void          Enter_bb_fo_philist(BB_NODE *bb, PHI_LIST *pl)
                                           { _bb_fo_philist.Insert(bb->Id(), pl); }
  PHI_LIST     *Bb_fo_philist(BB_NODE *bb) const
                                           { return _bb_fo_philist.Lookup(bb->Id());}

  RNANODE_MAP  &Sr_2_rna_map(void)         { return _sr_2_rna_map; }

  void          Setup_sr_rna_map(DNA_NODE *dna);

  void          Enter_sr_rna_map(STMTREP *sr, RNA_NODE *rna)
  {
    _sr_2_rna_map.Insert((IDTYPE) sr->Stmtrep_id(), rna);
    Is_Trace(Tracing(), (TFile, ">>>>>>>> VSA::Enter_sr_rna_map: sr%d :: rna(%d)\n",
                      sr->Stmtrep_id(), rna->Rna_idx()));
  }

  HOBJREP_MAP  &Cr_2_heap_obj_map(void)    { return _cr_2_heap_obj_map; }

  HEAPSTATE     Get_heap_obj_state(HEAP_OBJ_REP *hor) const;

  void          Enter_cr_heap_obj_map(CODEREP *cr, HEAP_OBJ_REP *heap_obj, BOOL replace = FALSE)
  {
    HEAP_OBJ_REP *old_hor = Cr_2_heap_obj(cr);
#ifdef Is_True_On
    Is_True(replace == TRUE ||
            (old_hor == NULL || old_hor->Heap_obj() == heap_obj->Heap_obj()),
            ("cr id = %d, map to different Heap_obj, pre ho_id = %d, curr ho_id = %d",
             cr->Coderep_id(), old_hor->Heap_obj()->Id(), heap_obj->Heap_obj()->Id())); 
#endif
    if (old_hor != NULL)
      _cr_2_heap_obj_map.Delete(cr->Coderep_id());
    _cr_2_heap_obj_map.Insert(cr->Coderep_id(), heap_obj);

    Is_Trace(Tracing(),
             (TFile, ">>>>>>>> VSA::Enter_cr_heap_obj_map: cr%d :: ", cr->Coderep_id()));
    Is_Trace_cmd(Tracing(), heap_obj->Print(TFile));
    Is_Trace(Tracing(),(TFile, "\n"));
  }

  HEAP_OBJ_REP *Cr_2_heap_obj(CODEREP *cr) const
                                           { return _cr_2_heap_obj_map.Lookup(cr->Coderep_id()); }
  HEAP_OBJ_REP *Find_cr_heap_obj(CODEREP *cr);

  HOBJREP_MAP  &Cr_2_heap_obj_refmap(void)
                                           { return _cr_2_heap_obj_refmap; }

  void          Enter_cr_heap_obj_refmap(CODEREP *cr, HEAP_OBJ_REP *heap_obj)
  {
    _cr_2_heap_obj_refmap.Insert(cr->Coderep_id(), heap_obj);
    Is_Trace(Tracing(),
             (TFile, ">>>>>>>> VSA::Enter_cr_heap_obj_refmap: cr%d :: ", cr->Coderep_id()));
    Is_Trace_cmd(Tracing(), heap_obj->Print(TFile));
    Is_Trace(Tracing(),(TFile, "\n"));
  }

  HEAP_OBJ_REP *Cr_2_heap_obj_ref(CODEREP *cr) const
                                           { return _cr_2_heap_obj_refmap.Lookup(cr->Coderep_id()); }

  // HOR/VOR mu/chi list
  void          Enter_stmt_hor_mu_map(STMTREP *stmt, MU_LIST *ml)
                                           { _stmt_hor_mu_map.Insert(stmt->Stmtrep_id(), ml); }
  MU_LIST      *Stmt_hor_mu(STMTREP *stmt) const {return _stmt_hor_mu_map.Lookup(stmt->Stmtrep_id());}
  MU_NODE      *Find_stmt_hor_mu(STMTREP *stmt, HEAP_OBJ *ho);
  HEAP_OBJ_REP *Find_stmt_hor_mu(STMTREP *stmt, CODEREP *cr);
  CODEREP      *Find_hor_chi_cr(STMTREP *stmt, HEAP_OBJ_REP *hor) const;
  void          Append_stmt_hor_mu(STMTREP *stmt, HEAP_OBJ_REP *hor, CODEREP *cr);
  void          Update_stmt_hor_mu(STMTREP *stmt, HEAP_OBJ_REP *hor, CODEREP *cr, BOOL append);

  void          Enter_stmt_hor_chi_map(STMTREP *stmt, CHI_LIST *ml)
                                           { _stmt_hor_chi_map.Insert(stmt->Stmtrep_id(), ml); }
  CHI_LIST     *Stmt_hor_chi(STMTREP *stmt) const {return _stmt_hor_chi_map.Lookup(stmt->Stmtrep_id());}
  CHI_NODE     *Find_stmt_hor_chi(STMTREP *stmt, HEAP_OBJ *ho);
  HEAP_OBJ_REP *Find_stmt_hor_chi(STMTREP *stmt, CODEREP *cr);
  void          Append_stmt_hor_chi(STMTREP *stmt, HEAP_OBJ_REP *res,
                                    HEAP_OBJ_REP *opnd, CODEREP *cr);
  HEAP_OBJ_REP *Append_stmt_hor_chi(STMTREP *stmt, HEAP_OBJ_REP *hor, CODEREP *cr,
                                    ROR_ATTR attr, MEM_POOL *pool);
  HEAP_OBJ_REP *Update_stmt_hor_chi(STMTREP *stmt, HEAP_OBJ_REP *opnd, CODEREP *cr);


  VOBJREP_MAP  &Cr_2_vor_map(void)         { return _cr_2_vor_map; }

  void          Enter_cr_vor_map(CODEREP *cr, VSYM_OBJ_REP *vsym_obj)
  {
    INT32 cr_id = cr->Coderep_id();
    // Should delete old vor first - ID_MAP Resize may cause new map use old vor
    if (_cr_2_vor_map.Lookup(cr_id) != NULL) {
      _cr_2_vor_map.Delete(cr_id);
    }
    _cr_2_vor_map.Insert(cr_id, vsym_obj);

    Is_Trace(Tracing(),
             (TFile, ">>>>>>>> VSA::Enter_cr_vor_map: cr%d :: ", cr_id));
    Is_Trace_cmd(Tracing(), vsym_obj->Print(TFile));
    Is_Trace(Tracing(),(TFile, "\n"));
  }

  FOR_ARRAY    *Sr_2_for_array(STMTREP *sr) const { return _sr_2_for_array_map.Lookup(sr->Stmtrep_id()); }
  FORARR_MAP   &Sr_2_for_array_map(void)          { return _sr_2_for_array_map; }

  void          Enter_sr_for_array_map(STMTREP *sr, FSM_OBJ_REP *fsm_obj)
  {
    INT32 sr_id = sr->Stmtrep_id();
    FOR_ARRAY *for_array = _sr_2_for_array_map.Lookup(sr_id);
    if (for_array == NULL) {
      for_array = CXX_NEW(FOR_ARRAY(mempool_allocator<FSM_OBJ_REP*>(Mem_pool())), Mem_pool());
      _sr_2_for_array_map.Insert(sr_id, for_array);
    }
    Is_Trace(Tracing(), (TFile, ">>>>>>>> VSA::Enter_sr_for_array_map: sr%d : size(%ld) : 0x%llx : ",
                         sr_id, for_array->size(), (UINT64)fsm_obj));
    Is_Trace_cmd(Tracing(), fsm_obj->Print(TFile));
    Is_Trace(Tracing(), (TFile, "\n"));
    for_array->push_back(fsm_obj);
  }

  FOR_ARRAY    *Cr_2_for_array(CODEREP *cr) const { return _cr_2_for_array_map.Lookup(cr->Coderep_id()); }
  FORARR_MAP   &Cr_2_for_array_map(void)          { return _cr_2_for_array_map; }

  void          Enter_cr_for_array_map(CODEREP *cr, FSM_OBJ_REP *fsm_obj)
  {
    INT32 cr_id = cr->Coderep_id();
    FOR_ARRAY *for_array = _cr_2_for_array_map.Lookup(cr_id);
    if (for_array == NULL) {
      for_array = CXX_NEW(FOR_ARRAY(mempool_allocator<FSM_OBJ_REP*>(Mem_pool())), Mem_pool());
      _cr_2_for_array_map.Insert(cr_id, for_array);
    }
    Is_Trace(Tracing(), (TFile, ">>>>>>>> VSA::Enter_cr_for_array_map: cr%d : size(%ld) : 0x%llx : ",
                         cr_id, for_array->size(), (UINT64)fsm_obj));
    Is_Trace_cmd(Tracing(), fsm_obj->Print(TFile));
    for (INT fa_idx = 0; fa_idx < for_array->size(); fa_idx++) {
      FSM_OBJ_REP *item = (*for_array)[fa_idx];
      if (item == fsm_obj) {
        Is_Trace(Tracing(), (TFile, " already exists!\n"));
        return;
      }
    }
    Is_Trace(Tracing(), (TFile, "\n"));
    for_array->push_back(fsm_obj);
    VSYM_OBJ_REP *vor = Cr_2_vor(cr);
    if (vor != NULL)
      Enter_vor_for_array_map(vor, fsm_obj);
  }

  FOR_ARRAY    *Vor_2_for_array(VSYM_OBJ_REP *vor) const
  {
    VOR_FORARR_MAP::const_iterator iter = _vor_2_for_array_map.find((INTPTR)vor);
    if (iter != _vor_2_for_array_map.end())
      return iter->second;
    else
      return NULL;
  }
  VOR_FORARR_MAP &Vor_2_for_array_map(void)       { return _vor_2_for_array_map; }

  void          Enter_vor_for_array_map(VSYM_OBJ_REP *vor, FSM_OBJ_REP *fsm_obj)
  {
    FOR_ARRAY *for_array = NULL;
    VOR_FORARR_MAP::const_iterator iter = _vor_2_for_array_map.find((INTPTR)vor);
    if (iter != _vor_2_for_array_map.end())
      for_array = iter->second;
    if (for_array == NULL) {
      for_array = CXX_NEW(FOR_ARRAY(mempool_allocator<FSM_OBJ_REP*>(Mem_pool())), Mem_pool());
      _vor_2_for_array_map[(INTPTR)vor] = for_array;
    }
    Is_Trace(Tracing(), (TFile, ">>>>>>>> VSA::Enter_vor_for_array_map: "));
    Is_Trace_cmd(Tracing(), vor->Print(TFile));
    Is_Trace(Tracing(), (TFile, " : size(%ld) : 0x%llx : ", for_array->size(), (UINT64)fsm_obj));
    Is_Trace_cmd(Tracing(), fsm_obj->Print(TFile));
    for (INT fa_idx = 0; fa_idx < for_array->size(); fa_idx++) {
      FSM_OBJ_REP *item = (*for_array)[fa_idx];
      if (item == fsm_obj) {
        Is_Trace(Tracing(), (TFile, " already exists!\n"));
        return;
      }
    }
    Is_Trace(Tracing(), (TFile, "\n"));
    for_array->push_back(fsm_obj);
  }

  VALUE_RANGE_VEC *Cr_2_vr(IDTYPE cr_id) { return _value_range_map[cr_id]; }
  void          Enter_cr_vr_map(IDTYPE cr_id, INT64 lower, INT64 upper)
  {
    VALUE_RANGE_VEC* tmp = _value_range_map[cr_id];
    if (tmp == NULL) {
      tmp = CXX_NEW(VALUE_RANGE_VEC(VALUE_RANGE_VEC::allocator_type(_mem_pool)), _mem_pool);
      _value_range_map[cr_id] = tmp;
    }
    VALUE_RANGE* vr = CXX_NEW(VALUE_RANGE(lower, upper), _mem_pool);
    tmp->push_back(vr);
    Is_Trace(Tracing(), (TFile, "RBC: add value range: cr(%d) => [%lld, %lld)\n",
                         cr_id, lower, upper));
  }

  VFR_CANDS    *Vfr_2_cand_map(VSYM_FLD_REP *vfr) const
  { 
    VFR_CAND_MAP::const_iterator it = _vfr_cand_map.find(vfr);
    if(it == _vfr_cand_map.end()) {
      return NULL;
    } else {
      return  it->second;
    }
  }

  void          Enter_vfr_cand_map(VSYM_FLD_REP *vfr, char *cls_name, char *fld_name, IDTYPE field_id)
  {
    VFR_CANDS *cands = NULL;
    VFR_CAND_MAP::const_iterator it = _vfr_cand_map.find(vfr);
    if(it == _vfr_cand_map.end()) {
      cands = CXX_NEW(vector<VFR_CAND *>(), Mem_pool());
    } else {
      cands = it->second;
    }
    char *dup_cls_name = (char*)MEM_POOL_Alloc(Mem_pool(), strlen(cls_name) + 1);
    char *dup_fld_name = (char*)MEM_POOL_Alloc(Mem_pool(), strlen(fld_name) + 1);
    strcpy(dup_cls_name, cls_name);
    strcpy(dup_fld_name, fld_name);
    VFR_CAND *cand = CXX_NEW(VFR_CAND(dup_cls_name, dup_fld_name, field_id), Mem_pool());
    cands->push_back(cand);
    _vfr_cand_map[vfr] = cands;
  }

  // _stmt_vor_mu_map related access functions
  void          Enter_stmt_vor_mu_map(STMTREP *stmt, MU_LIST *ml)
                                           { _stmt_vor_mu_map.Insert(stmt->Stmtrep_id(), ml);  }
  MU_LIST      *Stmt_vor_mu(STMTREP *stmt) const {return _stmt_vor_mu_map.Lookup(stmt->Stmtrep_id());}
  CODEREP      *Find_vor_mu_cr(STMTREP *stmt, VSYM_OBJ_REP *vor) const;
  VSYM_OBJ_REP *Find_vor_mu_vor(STMTREP *stmt, CODEREP *cr, VSYM_FLD_REP* fld) const;
  VSYM_OBJ_REP *Find_vor_mu_vor(STMTREP *stmt, CODEREP *cr, IDTYPE fldid, mINT32 ofst) const;
  VSYM_OBJ_REP *Find_hor_mu_vor(STMTREP *stmt, HEAP_OBJ_REP* hor, VSYM_FLD_REP* fld, CODEREP *cr) const;
  VSYM_OBJ_REP *Find_hor_mu_vor_any(STMTREP *stmt, HEAP_OBJ_REP* hor,
                                    VSYM_FLD_REP* fld, CODEREP *cr, BOOL match_any) const;
  MU_NODE      *Find_vor_mu(STMTREP *stmt, UINT32 file_idx, ST_IDX st_idx, VSYM_FLD_REP* fld) const;
  MU_NODE      *Find_vor_mu(STMTREP *stmt, VSYM_OBJ *vo) const;

  // _stmt_vor_chi_map related access functions
  void          Enter_stmt_vor_chi_map(STMTREP *stmt, CHI_LIST *ml)
                                           { _stmt_vor_chi_map.Insert(stmt->Stmtrep_id(), ml);  }
  CHI_LIST     *Stmt_vor_chi(STMTREP *stmt) const {return _stmt_vor_chi_map.Lookup(stmt->Stmtrep_id());}
  CODEREP      *Find_vor_chi_cr(STMTREP *stmt, VSYM_OBJ_REP *vor) const;
  VSYM_OBJ_REP *Find_vor_chi_vor(STMTREP *stmt, CODEREP *cr, VSYM_FLD_REP* fld) const;
  VSYM_OBJ_REP *Find_hor_chi_vor(STMTREP *stmt, HEAP_OBJ_REP* hor, VSYM_FLD_REP* fld, CODEREP *cr) const;
  VSYM_OBJ_REP *Find_hor_chi_vor_any(STMTREP *stmt, HEAP_OBJ_REP* hor,
                                     VSYM_FLD_REP* fld, CODEREP *cr, BOOL match_any) const;
  CHI_NODE     *Find_vor_chi(STMTREP *stmt, UINT32 file_idx, ST_IDX st_idx, VSYM_FLD_REP* fld) const;
  CHI_NODE     *Find_vor_chi(STMTREP *stmt, CODEREP *cr, VSYM_FLD_REP *fld) const;
  CHI_NODE     *Find_vor_chi(STMTREP *stmt, VSYM_OBJ *vo) const;
  CVOR         *Find_vor_chi_opnd(STMTREP *stmt, VSYM_OBJ_REP *vor) const;

  HEAP_OBJ_REP *Find_stmt_cur_hor(STMTREP *stmt, HEAP_OBJ *ho) const;
  CODEREP      *Find_stmt_chi_var(STMTREP *stmt, HEAP_OBJ *ho) const;
  VSYM_OBJ_REP *Find_stmt_cur_vor(STMTREP *stmt, VSYM_OBJ *vo) const;

  // _stmt_fsm_mu_map related access functions
  void          Enter_stmt_fsm_mu_map(STMTREP *stmt, MU_LIST *ml)
                                           { _stmt_fsm_mu_map.Insert(stmt->Stmtrep_id(), ml); }
  MU_LIST      *Stmt_fsm_mu(STMTREP *stmt) const { return _stmt_fsm_mu_map.Lookup(stmt->Stmtrep_id()); }

  template <typename RSCOBJP>
  void Enter_fsm_mu_ror(STMTREP *stmt, RSCOBJP ror) {
    MU_LIST *mu_list;
    mu_list = Stmt_fsm_mu(stmt);
    if (mu_list == NULL) {
      mu_list = CXX_NEW(MU_LIST, Mem_pool());
      Enter_stmt_fsm_mu_map(stmt, mu_list);
    }

    MU_NODE *mnode;
    if (! mu_list->Is_Empty()) {
      MU_LIST_ITER mu_iter;
      FOR_ALL_NODE(mnode, mu_iter, Init(mu_list)) {
        RSCOBJP mror = (RSCOBJP)mnode->OPND();
        if (mror == ror) 
          return ; // ror already exist in the list
      }
    }

    // append hor to the mu_list
    mnode = CXX_NEW(MU_NODE, Mem_pool());
    mnode->Set_OPND((CODEREP*)ror, FALSE);
    mu_list->Append(mnode);
  }
  void          Enter_fsm_mu_hor(STMTREP *stmt, HEAP_OBJ_REP *hor) { Enter_fsm_mu_ror(stmt, hor); }
  void          Enter_fsm_mu_fo(STMTREP *stmt, FSM_OBJ_REP *fo) { Enter_fsm_mu_ror(stmt, fo); }

  template <typename RSCOBJP>
  RSCOBJP Find_fsm_mu_ror(STMTREP *stmt, RSCOBJP ror) {
    MU_LIST *mu_list;
    mu_list = Stmt_fsm_mu(stmt);
    if (mu_list == NULL || mu_list->Is_Empty())
      return NULL;  // cannot find it if the Stmt_fsm_mu does not exist

    MU_NODE *mnode;
    MU_LIST_ITER mu_iter;
    FOR_ALL_NODE(mnode, mu_iter, Init(mu_list)) {
      RSCOBJP mror = (RSCOBJP)mnode->OPND();
      if (mror == ror) 
        return ror; // hor already exist in the list

      if (mror->Rsc_obj()->Kind() != ror->Rsc_obj()->Kind()) continue;
      if (mror->Rsc_obj() == ror->Rsc_obj())
        return mror;
    }
    return NULL;
  }
  HEAP_OBJ_REP *Find_fsm_mu_hor(STMTREP *stmt, HEAP_OBJ_REP *hor) { return Find_fsm_mu_ror(stmt, hor); }
  FSM_OBJ_REP  *Find_fsm_mu_fo(STMTREP *stmt, FSM_OBJ_REP *fo) { return Find_fsm_mu_ror(stmt, fo); }

  // _stmt_fsm_chi_map related access functions
  void          Enter_stmt_fsm_chi_map(STMTREP *stmt, CHI_LIST *cl)
                                           { _stmt_fsm_chi_map.Insert(stmt->Stmtrep_id(), cl);  }
  CHI_LIST     *Stmt_fsm_chi(STMTREP *stmt) const {return _stmt_fsm_chi_map.Lookup(stmt->Stmtrep_id());}

  template <typename RSCOBJP>
  void Enter_fsm_chi_ror(STMTREP *stmt, RSCOBJP ror) {
    CHI_LIST *chi_list;
    chi_list = Stmt_fsm_chi(stmt);
    if (chi_list == NULL) {
      chi_list = CXX_NEW(CHI_LIST, Mem_pool());
      Enter_stmt_fsm_chi_map(stmt, chi_list);
    }

    CHI_NODE *cnode;
    if (! chi_list->Is_Empty()) {
      CHI_LIST_ITER chi_iter;
      FOR_ALL_NODE(cnode, chi_iter, Init(chi_list)) {
        RSCOBJP cror = (RSCOBJP)cnode->OPND();
        if (cror == ror) 
          return ; // ror already exist in the list
      }
    }

    // append hor to the chi_list
    cnode = CXX_NEW(CHI_NODE, Mem_pool());
    cnode->Set_OPND((CODEREP*)ror, FALSE);
    cnode->Set_RESULT((CODEREP *)ror);
    cnode->Set_aux_id(ror->Rsc_obj()->Id());
    chi_list->Append(cnode);
  }
  void          Enter_fsm_chi_hor(STMTREP *stmt, HEAP_OBJ_REP *hor) { Enter_fsm_chi_ror(stmt, hor); }
  void          Enter_fsm_chi_for(STMTREP *stmt, FSM_OBJ_REP *fo)   { Enter_fsm_chi_ror(stmt, fo); }

  // Tag related APIs
  TAG_BASE     *Find_tag_base( STRING tag_name) const;
  template <typename T>
  T            *Cr_2_tor_list(CODEREP *cr);
  template <typename T>
  T            *Cr_2_tor_list(CODEREP *cr) const;

  template <typename RSCOBJP, typename T>
  void          Enter_ror_tor_list(RSCOBJP ror, T *tor_list)    {
    Is_True_Ret(ror, ("Ror is NULL."));
    Is_True_Ret(tor_list, ("Tag obj rep list is NULL."));

    ror->Set_tor_list(tor_list);
  }

  template <typename RSCOBJP, typename T>
  T            *Ror_2_tor_list(RSCOBJP ror)
  {
    return ror->template Tor_list<T>();
  }

  CODEREP      *Find_cr_with_tag(CODEREP *cr) const;
  VSYM_OBJ_REP *Find_vor_with_tag(STMTREP *sr, CODEREP *cr, BOOL mu) const;
  TOR_LIST     *Find_tor_list_from_cr(STMTREP *sr, CODEREP *cr, BOOL mu = TRUE) const;

  // Old Tag Api - to be deleted
  template <class TAGOBJRP>
  TAG_OBJ_REP  *Allocate_tag_obj(TAG_BASE *tag_base, TAGOBJRP tagobjr, STMTREP *defstmt)    {
    Is_True(tag_base != NULL, ("Tag base is null."));
    Is_Trace(Tracing(), (TFile, "VSA::Allocate_tag_obj for %s, ror : ", tag_base->Tag_name()));
    Is_Trace_cmdn(Tracing(), tagobjr->Print(TFile), TFile);
    TAG_OBJ_REP *tor = CXX_NEW(TAG_OBJ_REP(tag_base, _last_tor_id++, tagobjr, defstmt), Mem_pool());
    return tor;
  }
  template<typename TAGOBJRP>
  TAG_OBJ_REP  *Create_tor_with_def_attr(TOR_DEF_ATTR def_attr, TAG_OBJ_REP *src_tor, TAGOBJRP tagobjr,
                                        STMTREP *defstmt = NULL, IDTYPE attr_id = TAG_INVALID_ID);
  template<typename TAGOBJRP>
  BOOL          Update_tor_with_def_attr(TOR_DEF_ATTR def_attr, TAG_OBJ_REP *tgt_tor, TAG_OBJ_REP *src_tor, 
                                         TAGOBJRP tagobjr, STMTREP *defstmt = NULL, IDTYPE attr_id = TAG_INVALID_ID);

  template <typename RSCOBJP>
  void          Enter_tor_ror(RSCOBJP ror, TAG_OBJ_REP *tor)
  {
    Is_True_Ret(ror, ("Ror is NULL."));
    Is_True_Ret(tor, ("Tag obj rep is NULL."));

    TOR_LIST_OLD *tor_list = Ror_2_tor_list<RSCOBJP, TOR_LIST_OLD>(ror);
    if (!tor_list) {
      tor_list = CXX_NEW(TOR_LIST_OLD, Mem_pool());
      ror->Set_tor_list(tor_list);
      tor_list->Append(tor);
    } else {
      tor_list->Append(tor);
    }
  }

  TOR_LIST_OLD *Find_tor_list_from_cr(STMTREP *sr, CODEREP *cr, TAGOKIND &kind, BOOL mu = TRUE) const;
  BOOL          Bind_tor_list_to_cr(STMTREP *sr, CODEREP *cr, TOR_LIST_OLD *tor_list,
                                    TOR_DEF_ATTR attr,
                                    TAG_BASE *tag_base = NULL,
                                    IDTYPE attr_id = TAG_INVALID_ID);
  BOOL          Bind_tor_list_to_vor(STMTREP *sr, CODEREP *cr, TOR_LIST_OLD *tor_list,
                                     TOR_DEF_ATTR attr,
                                     TAG_BASE *tag_base = NULL,
                                     IDTYPE attr_id = TAG_INVALID_ID);
  void          Copy_tor_from_hor_to_hor(HEAP_OBJ_REP *dst_hor, HEAP_OBJ_REP *src_hor)
  {
    // [FIXME]: need to copy tor if dst is def by phi? may be need a merge
    // if(dst_hor->Attr() == ROR_DEF_BY_PHI)
    //  return;
    Is_True_Ret(dst_hor != NULL, ("Destination hor is NULL."));
    if (src_hor && src_hor != dst_hor && src_hor->Tor_list()) {
      Is_Trace(Tracing(), (TFile, "Copy tag obj from  ho%dv%d to ho%dv%d.\n",
      src_hor->Heap_obj()->Id(), src_hor->Version(), dst_hor->Heap_obj()->Id(), dst_hor->Version()));
      TAG_OBJ_REP *tor;
      TOR_LIST_OLD_ITER iter;
      FOR_ALL_ELEM(tor, iter, Init(src_hor->Tor_list())) {
        TAG_OBJ_REP *cloned_tor = Create_tor_with_def_attr(TO_DEF_BY_COPY, tor, dst_hor, NULL);
        Enter_tor_ror(dst_hor, cloned_tor);
      }
    }
  }
  BOOL          Merge_bb_to_phi(BB_NODE *bb);
  template<typename RSCOBJP>
  BOOL          Merge_tor_list(STMTREP *sr, RSCOBJP ror, TOR_LIST_OLD *dst, TOR_LIST_OLD *src, BOOL for_phi = FALSE);
  BOOL          Find_tgt_and_merge(STMTREP *sr, CODEREP *cr, TOR_LIST_OLD *src);
  void          Create_entrychi_tor();
  void          Create_tag_obj_rep(BB_NODE *bb, MEM_POOL *def_bbs_pool);

  // value objects interfaces
  // create value objects for vor
  DEF_OBJS      *Create_value_objs(VSYM_OBJ_REP *vor, VALUE_TYPE type, INT32 def_size = DEF_VALUE_OBJ_SIZE)
  {
    Is_True_Ret(!Vor_2_value_objs(vor), ("value objs already created for vor"), NULL);
    VSA_STATS_inc(value_objs);
    DEF_OBJS *value_objs = CXX_NEW(DEF_OBJS(UNK_TYPE, _mem_pool, def_size), _mem_pool);
    _vor_2_value_objs[(INTPTR(vor))] = (INTPTR)value_objs;
    value_objs->Set_type(type);
    return value_objs;
  }

  DEF_OBJS      *Clone_value_objs(VSYM_OBJ_REP *to, VSYM_OBJ_REP *from)
  {
    Is_True_Ret(from && to && from != to, ("Clone same vor's value objs"), NULL);
    Is_True_Ret(from != to && !Vor_2_value_objs(to), ("value objs already created for vor"), NULL);
    DEF_OBJS *from_objs = (DEF_OBJS*)Vor_2_value_objs(from);
    if (from_objs) {
      DEF_OBJS *to_objs = from_objs->Clone(_mem_pool);
      VSA_STATS_inc(value_objs);
      _vor_2_value_objs[(INTPTR)to] = (INTPTR)to_objs;
      return to_objs;
    } else {
      return NULL;
    }
  }

  // add new value object to vsym object rep
  // @return TRUE for success, FALSE for fail
  template<typename T>
  BOOL          Add_value_obj(VSYM_OBJ_REP *vor, T *value)
  {
    // TO BE IMPLEMENTED
    return TRUE;
  }
  
  LIST_ENTRY  *Get_list_entry(VSYM_OBJ_REP *vor, CODEREP *idx_cr)
  {
    if (idx_cr->Kind() != CK_CONST) {
      return NULL;
    }
    INT64 idx = idx_cr->Const_val();
    Is_True_Ret(idx >= 0 && idx < INT32_MAX, ("invalid outof range"), NULL);
    LIST_OBJS *list_objs = (LIST_OBJS *) Vor_2_value_objs(vor);
    if (list_objs) {
      Is_True_Ret(list_objs->Type() == LIST_TYPE, ("invalid type"), NULL);
      return list_objs->Value_obj(idx);
    }
    return NULL;
  }

  LIST_ENTRY   *Get_list_back(VSYM_OBJ_REP *vor)
  {
    LIST_OBJS *list_objs = (LIST_OBJS *) Vor_2_value_objs(vor);
    if (list_objs) {
      Is_True_Ret(list_objs->Type() == LIST_TYPE, ("invalid type"), NULL);
      return list_objs->Back();
    }
    return NULL;
  }

  DEF_OBJS     *Vor_2_value_objs(VSYM_OBJ_REP *vor) const
  {
    VOR_VALUE_MAP::const_iterator it = _vor_2_value_objs.find((INTPTR)vor);
    if (it != _vor_2_value_objs.end()) {
      return (DEF_OBJS *)(it->second);
    } else {
      return (DEF_OBJS *)NULL;
    }
  }
  // get vsym object rep's current value objects size
  INT32         Value_obj_size(VSYM_OBJ_REP *vor) const
  {
    // TO BE IMPLEMENTED
    return 0;
  }

private:
  template <typename RSCOBJP>
  RSCOBJP Find_fsm_chi_ror(STMTREP *stmt, RSCOBJP ror) {
    CHI_LIST *chi_list;
    chi_list = Stmt_fsm_chi(stmt);
    if (chi_list == NULL || chi_list->Is_Empty())
      return NULL;  // cannot find it if the Stmt_fsm_chi does not exist

    CHI_NODE *cnode;
    CHI_LIST_ITER chi_iter;
    FOR_ALL_NODE(cnode, chi_iter, Init(chi_list)) {
      RSCOBJP cror = (RSCOBJP)cnode->OPND();
      if (cror == ror) 
        return ror; // found ror in the list

      if (cror->Rsc_obj()->Kind() != ror->Rsc_obj()->Kind()) continue;
      if (cror->Rsc_obj() == ror->Rsc_obj())
        return cror;
    }
    return NULL;
  }
  HEAP_OBJ_REP *Find_fsm_chi_hor(STMTREP *stmt, HEAP_OBJ_REP *hor) { return Find_fsm_chi_ror(stmt, hor); }
  FSM_OBJ_REP  *Find_fsm_chi_fo(STMTREP *stmt, FSM_OBJ_REP *fo) { return Find_fsm_chi_ror(stmt, fo); }

  // can add a field_id parameter below
  ST*           Find_or_create_local_st(FST_FST_MAP* fst_map, UINT32 file_idx, ST_IDX st_idx, BOOL& create);
  MU_NODE      *Find_stmt_var_mu(STMTREP *stmt, UINT32 file_idx, ST_IDX st_idx, VSYM_FLD_REP* fld) const;
  MU_NODE      *Find_stmt_var_mu(STMTREP *stmt, ST* st, VSYM_FLD_REP* fld) const;
  CHI_NODE     *Find_stmt_var_chi(STMTREP *stmt, UINT32 file_idx, ST_IDX st_idx, VSYM_FLD_REP* fld) const;
  CHI_NODE     *Find_stmt_var_chi(STMTREP *stmt, ST* st, VSYM_FLD_REP* fld) const;
  CHI_NODE     *Find_stmt_var_chi(STMTREP *STMT, CODEREP *opnd) const;
  void          Update_stmt_var_mu(STMTREP *stmt, MU_NODE *mu, AUX_ID aux);
  void          Update_stmt_var_chi(STMTREP *stmt, CHI_NODE *chi, AUX_ID aux);
  void          Update_bb_var_phi(BB_NODE *bb, PHI_NODE *phi, AUX_ID aux);
  void          Update_stmt_var_mu(STMTREP *stmt, const AUX_DEF_MAP *to_add, const AUX_DEF_MAP *to_upd, const AUX_SET* aux_set);
  void          Update_stmt_var_chi(STMTREP *stmt, const AUX_DEF_MAP *to_add, const AUX_DEF_MAP *to_upd, const AUX_SET* aux_set);
  void          Update_bb_var_phi(BB_NODE *bb, const AUX_DEF_MAP *to_add, const AUX_DEF_MAP *to_upd);
  void          Update_bb_succ_var_phi(BB_NODE *bb, const AUX_DEF_MAP *to_add, const AUX_DEF_MAP *to_upd);

  BOOL          Get_alloc_path(vector<SRCPOS_NODE> *nodes, HEAP_OBJ_REP* hor, CODEREP* cr, hash_set<IDTYPE>&);
  void          Compose_dbf_path(SRCPOS_HANDLE *srcpos_h, HEAP_OBJ_REP* hor, STMTREP* call);
  void          Compose_rvsa_path(SRCPOS_HANDLE *srcpos_h, CODEREP* cr);
  // Classify error procedures and their supporting function
  void          Classify_dsym_error(AUX_STAB_ENTRY *sym, USRCPOS spos);
  void          Classify_asm_npd_error(STMTREP *stmt, BB_NODE *curbb);

private:
  // Generate CD path when the use of a variable is far from the function entry
  void          Append_cd_linenum(CODEREP *x, BB_NODE *bb, SRCPOS_HANDLE *srcpos_h, hash_set<IDTYPE>&); 
  SRCPOS        Get_bb_linenum(BB_NODE *bb);

  // Traverse CODEMAP for any use of uninitalized variable
  void          Classify_uiv_error(STMTREP *stmt, BB_NODE *curbb); // for statement
  void          Classify_uiv_error(CODEREP *expr, BB_NODE *curbb, STMTREP *stmt, ILODSTORBASE kind, STPATH* stpath=NULL);
  void          Classify_uiv_error(CODEREP *var, BB_NODE *curbb,
                                   CALL_STACK& cs, SRCPOS_HANDLE *sp_h, ILODSTORBASE kind, hash_set<IDTYPE> &visited);
  void          Classify_uiv_error(CODEREP *ilod, BB_NODE *curbb, STMTREP *stmt, VSYM_OBJ_REP *vor,
                                   CALL_STACK& cs, SRCPOS_HANDLE *sp_h, ILODSTORBASE kind, hash_set<IDTYPE> &visited);
  void          Classify_asm_uiv_error(STMTREP *stmt, BB_NODE *curbb);  // for ASM_STMT

  INT32         Page_size(void)                { return 256;   /* to be set up by configuration */ }
  BOOL          Is_invalid_address(INT32 val)  { return (val < Page_size() && val > -Page_size()); }

  // Propagate the Is_var_defined attribute and Zero value attribute
  void          Propagate_vardef(STMTREP *stmt, BB_NODE *curbb);
  CODEREP      *Propagate_vardef(CODEREP *expr, BB_NODE *curbb, STMTREP *stmt, UINT32 *flags);
  CODEREP      *Propagate_vardef(CODEREP *var, SCC *scc, hash_set<IDTYPE>&);

  // EH path traversal functions
  void          Check_eh_paths_for_vul(CODEREP *x, BB_NODE* curbb, STMTREP *opt_chi,
                                       CALL_STACK& cs, SRCPOS_HANDLE *sp_h, ILODSTORBASE kind, hash_set<IDTYPE> &visited);
  void          Check_intrn_call_for_vul(CODEREP *x, BB_NODE* cur_bb, STMTREP* stmt,
                                         CALL_STACK& cs, SRCPOS_HANDLE* sp_h, ILODSTORBASE kind, hash_set<IDTYPE> &visited);

  // IPSA_insession() vsa functions
  void          Check_callers_argument_for_uiv(CODEREP *x, CALL_STACK& cs, SRCPOS_HANDLE *sp_h,
                                               ILODSTORBASE kind, VSYM_TRACKER *tracker = NULL);
  void          Check_callers_global_for_uiv(ST_IDX st, CALL_STACK& cs, SRCPOS_HANDLE *sp_h,
                                             ILODSTORBASE kind);
  void          Check_callee_return_value_for_xfa(CODEREP *x, CALL_STACK& cs, SRCPOS_HANDLE *sp_h,
                                                  ILODSTORBASE kind);
  void          Check_callee_istore_for_uiv(BB_NODE *bb, IDTYPE arg, CODEREP *chi_opnd, CALL_STACK &cs,
                                            SRCPOS_HANDLE *sp_h, ILODSTORBASE kind, hash_set<IDTYPE> &visited);

  // UIV analysis based on recursive descend process
  BOOL          Load_from_subset_memory_area(STMTREP *defstmt, CODEREP *x);

  void          Mark_var_escaped(CODEREP* x, hash_set<IDTYPE> &visited);
  CODEREP      *Udt_ivar(CODEREP *x);
  CODEREP      *Udt_var(CODEREP *x, BB_NODE *curbb, SRCPOS spos);
  CODEREP      *Udt_cr(CODEREP *x, BB_NODE *curbb, STMTREP *stmt);
  void          Udt_stmt(STMTREP *stmt, BB_NODE *bb, MEM_POOL *def_bbs_pool); 

  CODEREP      *Udt_const_init_scalar(CODEREP *x, AUX_ID var_aux_id);

private:
  // Framework to traverse CODEMAP with Heap object setup
  CODEREP      *Vsa_var(CODEREP *x);
  CODEREP      *Vsa_ivar(CODEREP *x);
  CODEREP      *Vsa_cr(CODEREP *x, STMTREP *sr, MEM_POOL *def_bbs_pool, BOOL in_array);
  void          Vsa_stmt(STMTREP *stmt, BB_NODE *bb, const AUX_DEF_MAP* to_add,
                         const AUX_DEF_MAP* to_upd, const AUX_SET_MAP* aux_set, MEM_POOL *def_bbs_pool);
  BOOL          Identical_phi_opnd(PHI_NODE *phi, BB_NODE *bb);
  BOOL          ST_initialized_zero(ST* st, INT ofst);

  // Error report utilities
  char         *Sym_name(IDTYPE id) const;
  BOOL          Is_legit_error(IDTYPE id) const;

  BOOL          Is_ivar_need_vsym(CODEREP *cr, STMTREP *stmt);

  // Print function support
  static void   Print_srcpos_nodes(SRCPOS_NODES &nodes, FILE* fp = stdout);

  // Private interfaces for error checking
  void          Classify_dbf_error(CODEREP *arg, STMTREP *stmt, HEAP_OBJ_REP *hor, hash_set<IDTYPE>&);
 // missing free
  void          Check_var_escaped(BB_NODE *bb, STMTREP *sr, CODEREP *res, CODEREP *opnd, BOOL fwd, BOOL escaped);
  void          Check_heap_obj_escaped(BB_NODE *bb, STMTREP *sr, CODEREP *cr, BOOL fwd, BOOL escaped);
  void          Check_heap_obj_escaped(BB_NODE *bb, STMTREP *sr, BOOL fwd);
  void          Classify_msf_error(BB_NODE *bb, HEAP_OBJ_REP* hor, SRCPOS_HANDLE *srcpos_h, hash_set<IDTYPE>&, ISSUE_CERTAINTY ic=IC_DEFINITELY);
  void          Classify_msf_error(AUX_ID sym, BB_NODE *bb, SRCPOS_HANDLE *srcpos_h, ISSUE_CERTAINTY ic) const;
  void          Classify_uaf_error(BB_NODE *bb, STMTREP *sr);
  void          Classify_uaf_error(BB_NODE *bb, STMTREP *sr, CODEREP *cr);
  void          Classify_uaf_error(HEAP_OBJ_REP *hor, CODEREP *x, BB_NODE *bb, SRCPOS_HANDLE *, hash_set<IDTYPE>&) const;
  void          Classify_uaf_error(CODEREP *x, BB_NODE *bb, SRCPOS_HANDLE *srcpos_h) const;

  void          Check_throw_catched_or_throws(vector<const char*>& valid_throw_names, 
                                              STMTREP *stmt, DNA_NODE *dna,
                                              SRCPOS_HANDLE *sp_h, hash_set<IDTYPE> *visited_funs,
                                              INT &cur_level);
  BOOL          Check_throw_catched(vector<const char*>& valid_throw_names, EH_PATH *path);
  BOOL          Check_fun_throws(DNA_NODE *dna, vector<const char*>& throw_list);
  void          Add_base_throw_types(vector<const char*>& valid_throw_names, TY_IDX throw_ty);

  // devirtualization related private functions
  BOOL          Find_icall_defs(CODEREP* cr, BB_NODE* bb, VSYM_OBJ_REP* vor,
                                CALL_STACK&, ICALL_TARGET_VECTOR&, hash_set<IDTYPE>&);
  BOOL          Find_callers_argument_for_icall_defs(CODEREP *base, VSYM_FLD_REP* vfr, ILODSTORBASE kind,
                                                     CALL_STACK&, ICALL_TARGET_VECTOR&);
  BOOL          Find_callees_side_effects_for_icall_defs(CODEREP *base, VSYM_FLD_REP* vfr, ILODSTORBASE kind,
                                                         CALL_STACK&, ICALL_TARGET_VECTOR&);
  BOOL          Find_vfunc_from_creation(STMTREP*, CODEREP*, INT32 ofst, ICALL_TARGET_VECTOR&);
  BOOL          Find_vfunc_from_type(TY_IDX ty, INT32 ofst, ICALL_TARGET_VECTOR&);

private:
  // dump IR with heap object information
  void          Print_chor(CHOR* chor, FILE *fp) const;
  void          Print_cvor(CVOR* cvor, FILE *fp) const;
  void          Print_vor_phi(PHI_NODE* phi, FILE *fp) const;
  void          Print_hor_phi(PHI_NODE* phi, FILE* fp, BOOL bb_header = FALSE) const;
  template <class RSCOR>
  void          Print_rscor_phi(PHI_NODE* phi, RSCOR *res, FILE *fp) const {
    fprintf(fp, "   ");
    res->Print(fp);
    fprintf(fp, " <- phi(");
    for (INT i = 0; i < phi->Size(); ++i)  {
      if (i != 0)
        fprintf(fp, ", ");
      RSCOR* opnd = (RSCOR *)phi->OPND(i);
      if (opnd)
        opnd->Print(fp);
      else
        fprintf(fp, "-nil-");
    }
    fprintf(fp, ")\n");
  }

  void          Print_cr(CODEREP *cr, INT indent, FILE *fp) const;
  void          Print_cr(CODEREP *cr, FILE *fp) const { Print_cr(cr, 0, fp); }
  void          Print_sr(STMTREP *stmt, FILE *fp) const;
  void          Print_bb_hor_list(BB_NODE *bb, FILE *fp) const;
  void          Print_bb_vor_list(BB_NODE *bb, FILE *fp) const;
  void          Print_bb_for_list(BB_NODE *bb, FILE *fp) const;
  void          Print_bb(BB_NODE *bb, FILE *fp) const;
  char         *Get_st_name_from_cr(CODEREP *cr, BOOL demangle = TRUE) const;

  INT           Count_ho_list(void) const    { return _heap_obj_list->Count(); }
  INT           Count_vo_list(void) const    { return _vsym_obj_list->Count(); }
  INT           Count_fo_list(void) const    { return _fsm_obj_list->Count(); }
  INT           Count_tor_list(void) const   { return _tag_obj_rep_list->Count(); }
  BOOL          Is_root_entry(void) const    { return Dna()->Is_root_entry(); }
  CODEREP      *Create_intconst(INT64 val);
  AUX_ID        Cr_aux_id(CODEREP *cr) const;
  mINT32        Cr_ofst(CODEREP *cr) const;
  IDTYPE        Cr_fldid(CODEREP *cr) const;
  VSYM_FLD_REP  Cr_vfr(CODEREP *cr) const;
  VSYM_FLD_REP  Base_vfr(CODEREP *cr) const;
  IDTYPE        Synthesize_fldid(HEAP_OBJ_REP *hor, IDTYPE original, mINT32 ofst) const;
  HEAP_OBJ_REP *Corelate_hor(HEAP_OBJ_REP *basehor, IDTYPE bauxid,
                             HEAP_OBJ_REP *newhor, IDTYPE nauxid);
  HEAP_OBJ_REP *Create_alloca_rscobj(CODEREP *x, STMTREP *sr);
  HEAP_OBJ_REP *Create_aux_rscobj(CODEREP *x, STMTREP *sr, BOOL merge_fld_base);
  CODEREP      *Create_lda_rscobj(CODEREP *x, STMTREP *sr);
  void          Create_aggregate_heapobj(STMTREP *stmt);

  BOOL          Is_same_symbol(CODEREP* cr1, CODEREP* cr2) const;
  BOOL          Is_strlen_of(CODEREP* str, CODEREP* len, BOOL is_wchar) const;
  BOOL          Is_var_tainted(CODEREP* cr) const;
  BOOL          Is_vor_tainted(CODEREP* cr, VSYM_OBJ_REP* vor) const;

public:
  VSA( COMP_UNIT *cu, IPSA *ipsa, MEM_POOL* gpool, MEM_POOL* lpool);
  ~VSA(void) {}

  OPT_STAB     *Opt_stab(void) const         { return _opt_stab; }
  MEM_POOL     *Loc_pool(void) const         { return _loc_pool; }
  MEM_POOL     *Mem_pool(void)const          { return _mem_pool; }
  BOOL          Tracing(void) const          { return _tracing; }
  BOOL          Disabled(void) const         { return _disabled; }
  void          Set_disabled(void)           { _disabled = TRUE; }
  void          Reset_disabled(void)         { _disabled = FALSE; }
  HEAP_OBJ_REP *Default_hor(void) const      { return _ipsa_mgr->Default_hor(); }
  VSYM_OBJ_REP *Default_vor(void) const      { return _ipsa_mgr->Default_vor(); }
  HEAP_OBJ_REP *Null_hor(void) const         { return _ipsa_mgr->Null_hor(); }
  VSYM_OBJ_REP *Null_vor(void) const         { return _ipsa_mgr->Null_vor(); }
  BOOL          Is_special_hor(HEAP_OBJ_REP *hor)
                                             { return (hor == Default_hor()) ||
                                                      (hor == Null_hor()); }
  BOOL          Is_special_vor(VSYM_OBJ_REP *vor) const
                                             { return (vor == Default_vor()) ||
                                                      (vor == Null_vor()); }
  IDTYPE        Last_heapobj_id(void) const  { return _ipsa_mgr ? _ipsa_mgr->Last_heapobj_id()
                                                                : _last_heap_obj_id; }
  IDTYPE        Last_vsymobj_id(void) const  { return _ipsa_mgr ? _ipsa_mgr->Last_vsymobj_id()
                                                                : _last_vsym_obj_id; }
  VSYM_OBJ_REP *Cr_2_vor(CODEREP *cr) const
                                             { return _cr_2_vor_map.Lookup(cr->Coderep_id()); }

  RNA_NODE     *Sr_2_rna(STMTREP *sr) const
                                             { return _sr_2_rna_map.Lookup(sr->Stmtrep_id()); }

  RBC_BASE     *Rbc(void) const              { return Ipsa()->Rbc(); }

  BOOL          Past_ret_reg_def(void) const { return _past_ret_reg_def; }
  void          Set_past_ret_reg_def(void)   { _past_ret_reg_def = TRUE; }
  void          Reset_past_ret_reg_def(void) { _past_ret_reg_def = FALSE; }
  VS_FLD_KIND   Get_vfr_kind(CODEREP *cr) const;

  void          Enter_cr_occ_map(CODEREP *cr, EXP_OCCURS *expocc)
  {
    INT32 cr_id = cr->Coderep_id();
    if (_cr_2_expocc.Lookup(cr_id) == NULL) _cr_2_expocc.Insert(cr_id, expocc);
  }
  HEAPSTATE     Get_heap_obj_state(STMTREP *stmt) const {
                  return Get_heap_obj_state(Cr_2_heap_obj(stmt->Rhs())); }
  EXP_OCCURS   *Cr_2_EXPOCC(CODEREP *cr)const{return _cr_2_expocc.Lookup(cr->Coderep_id());}

  void          Append_ident_asgn(WN *asgn)  { _filt_handler.Append_ident_asgn(asgn); }
  BOOL          Is_ident_asgn(const WN *wn)  { return _filt_handler.Is_ident_asgn(Comp_unit(), wn); }
  CODEREP      *Get_heapobj_length(HEAP_OBJ_REP* ho, BB_NODE* bb, hash_set<IDTYPE>& visited, CALL_STACK& cs, EVAL_ACTION act) const;
  std::pair<CODEREP*, RNA_NODE*>
                Get_object_length(CODEREP* cr, STMTREP* use, hash_set<IDTYPE>& visited, CALL_STACK& cs, EVAL_ACTION act) const;
  std::pair<UINT, RNA_NODE*>
                Get_object_length_xfa(CODEREP* cr, hash_set<IDTYPE>& visited, CALL_STACK& cs, EVAL_ACTION act) const;
  std::pair<CODEREP*, RNA_NODE*>
  Get_object_length(CODEREP* cr, STMTREP* sr, EVAL_ACTION act) const
  {
    hash_set<IDTYPE> visited;
    CALL_STACK cs;
    if (Ipsa()) Ipsa()->Begin_trav_counter();
    std::pair<CODEREP*, RNA_NODE*> ret = Get_object_length(cr, sr, visited, cs, act);
    if (Ipsa()) Ipsa()->End_trav_counter();
    return ret;
  }

  std::pair<UINT, RNA_NODE*>
               Get_string_length_xfa(CODEREP* cr, hash_set<IDTYPE>& visited,
                                     CALL_STACK& cs, VSYM_TRACKER* tracker, EVAL_ACTION act) const;
  std::pair<CODEREP*, RNA_NODE*>
               Get_string_length(CODEREP* cr, STMTREP* sr, hash_set<IDTYPE>& visited,
                                 CALL_STACK& cs, VSYM_TRACKER* tracker, EVAL_ACTION act) const;
  std::pair<CODEREP*, RNA_NODE*>
  Get_string_length_from_object(CODEREP* cr, STMTREP* sr, hash_set<IDTYPE>& visited,
                                CALL_STACK& cs, EVAL_ACTION act) const;
  std::pair<CODEREP*, RNA_NODE*>
  Get_string_length(CODEREP* cr, STMTREP* sr, EVAL_ACTION act) const
  {
    hash_set<IDTYPE> visited;
    CALL_STACK cs;
    VSYM_TRACKER tracker;
    if (Ipsa()) Ipsa()->Begin_trav_counter();
    std::pair<CODEREP*, RNA_NODE*> sz = Get_string_length(cr, sr, visited, cs, &tracker, act);
    if (Ipsa()) Ipsa()->End_trav_counter();
    return sz;
  }

  std::pair<UINT, RNA_NODE*>
               Eval_size_info_xfa(CODEREP* cr, VSYM_OBJ_REP* vor, hash_set<IDTYPE>& visited, CALL_STACK& cs,
                                  EVAL_ACTION act) const;
  std::pair<CODEREP*, RNA_NODE*>
               Eval_size_info(CODEREP* cr, VSYM_OBJ_REP* vor, STMTREP* sr,
                              hash_set<IDTYPE>& visited, CALL_STACK& cs, EVAL_ACTION act) const;
  std::pair<CODEREP*, RNA_NODE*>
               Eval_size_info(CODEREP* cr, STMTREP* sr, hash_set<IDTYPE>& visited, CALL_STACK& cs, EVAL_ACTION act) const;
  std::pair<CODEREP*, RNA_NODE*>
  Eval_size_info(CODEREP* cr, STMTREP* sr, EVAL_ACTION act) const
  {
    hash_set<IDTYPE> visited;
    CALL_STACK cs;
    if (Ipsa()) Ipsa()->Begin_trav_counter();
    std::pair<CODEREP*, RNA_NODE*> sz = Eval_size_info(cr, sr, visited, cs, act);
    if (Ipsa()) Ipsa()->End_trav_counter();
    return sz;
  }

  TY_IDX        Find_preg_type(CODEREP *cr, hash_set<IDTYPE>& visited) const;

  BOOL         Is_value_tainted(CODEREP* cr) const;

  BOOL         Find_def_cr_rec(CODEREP* cr, DNA_NODE* dna, const PATH_SELECTED& paths,
                               CODEREP*& ret, DNA_NODE*& def, IDTYPE& bb, hash_set<IDTYPE>& visited) const;
  BOOL         Find_def_cr_in_dna(CODEREP* cr, DNA_NODE* dna, const PATH_SELECTED& paths,
                                  CODEREP*& ret, DNA_NODE*& def, IDTYPE& bb) const
  {
    hash_set<IDTYPE> visited;
    BOOL found = Find_def_cr_rec(cr, dna, paths, ret, def, bb, visited);
    return found;
  }

public:

  HEAPSTATE     Callee_returns_new_heap_memory(STMTREP *call, BOOL chkhor=FALSE) const;
  CODEREP      *Callee_frees_heap_memory(STMTREP *call, BOOL *maybe = NULL) const;
  BOOL          Callee_may_taint_arg(STMTREP *call, CODEREP *cr, IDTYPE *which_arg) const;
  CODEREP      *Vsa_malloc_size(STMTREP* call, CODEREP *ret_cr, COMP_UNIT *cu, HEAPSIZE *hsz = NULL);
  CODEREP      *Callee_create_new_thread(STMTREP *sr);
  CODEREP      *Callee_register_signal_handler(STMTREP *sr);
  STMTREP      *Get_entry_chi_stmt() const;
  AUX_ID        Find_or_create_local_aux(FST_AUX_MAP* aux_map, FST_FST_MAP* fst_map,
                                         UINT32 file_idx, ST_IDX st_idx, BOOL& create);
  void          Collect_stmt_chi_globals(UINT32 file_idx, STMTREP* stmt, FST_AUX_MAP* globals);
  void          Build_local_fst_map(FST_FST_MAP* map);
  void          Build_local_aux_map(FST_AUX_MAP* map);
  BOOL          Is_stmt_dominates(STMTREP *sr1, STMTREP *sr2);
  BOOL          Is_stmt_reaches(STMTREP *sr1, STMTREP *sr2);

  // VSA functionalities exposed to IPSA::Link and COMP_UNIT::Do_vsa for data modeling
  void          Vsa_bb(BB_NODE *bb, const AUX_DEF_MAP* to_add, const AUX_DEF_MAP* to_upd,
                       const AUX_SET_MAP* aux_set, MEM_POOL *def_bbs_pool);       // heap_obj setup
  //void     Collect_callee_globals(FST_AUX_MAP& aux_map, FST_FST_MAP& fst_map,
  //                                AUX_DEF_MAP& to_add, AUX_DEF_MAP& to_upd,
  //                                UINT32 adjust, MEM_POOL* pool);
    // VSA functionalities exposed to IPSA::Link and COMP_UNIT::Do_vsa for data modeling
  void          Place_global_phi(const AUX_DEF_MAP* to_add, const AUX_DEF_MAP* to_upd, MEM_POOL* pool);
  void          Propagate_vardef(BB_NODE *bb);                           // attribute propagation
  void          Perform_heap_analysis(CFG *cfg, MEM_POOL *def_bbs_pool); // dbf analysis
  void          Perform_vsym_analysis(CFG *cfg, MEM_POOL *def_bbs_pool); // vsym analysis
  void          Perform_fsm_analysis(CFG *cfg, MEM_POOL *def_bbs_pool);  // fsm analysis
  void          Perform_heap_vsym_analysis(CFG* cfg, MEM_POOL* def_bbs_pool, INT max_iter);
  void          Perform_tag_analysis_old(CFG *cfg, MEM_POOL *def_bbs_pool);  // tag analysis
  void          Perform_tag_analysis();  // tag analysis

  void          Create_entrychi_hor(CODEREP *cr, STMTREP *stmt, MEM_POOL *pool);
  void          Create_refn_vsym_mu(CODEREP *cr, STMTREP *stmt, MEM_POOL *pool) {
                HEAP_OBJ *ho = _heap_obj_list->Find(cr->Aux_id(), FALSE);
                if (ho == NULL)
                  Create_refn_vsym_mu(cr, stmt, Allocate_heap_obj(cr, stmt->Bb(), pool),
                                      Cr_fldid(cr), Cr_ofst(cr), pool);
                else
                  Create_refn_vsym_mu(cr, stmt, Allocate_heap_obj(ho, NULL),
                                      Cr_fldid(cr), Cr_ofst(cr), pool);
                }
  void          Create_refn_vsym_chi(CODEREP *cr, STMTREP *stmt, MEM_POOL *pool)
      {         Create_refn_vsym_chi(cr, stmt, Allocate_heap_obj(cr, stmt->Bb(), pool),
                                     Cr_fldid(cr), Cr_ofst(cr), pool); }
  void          Create_refn_vsym(BB_NODE *bb, MEM_POOL *def_bbs_pool);


  CODEREP      *Add_const(MTYPE type, INT64 v) const {
                  return  Htable()->Add_const(type, v);
                }
  void          Check_printf(DNA_NODE* caller, RNA_NODE* callsite,  // check printf format
                             INT buf_arg, INT len_arg, INT fmt_arg, INT v_arg);
  void          Check_scanf(DNA_NODE* caller, RNA_NODE* callsite,   // check scanf format
                            INT fmt_arg, INT v_arg);

public:
  // VSA functionalities exposed to COMP_UNIT::Do_vsa for error reporting
  void          Scan_uiv_error(BB_NODE *bb);                       // uiv analysis only
  void          Scan_npd_error(BB_NODE *bb, MEM_POOL *def_bbs_pool); // npd analysis
  void          Scan_npd_new();                                      // new NPD analysis
  void          Scan_npd_new(CODEREP *cr, STMTREP *sr);              // new NPD analysis for given cr
  void          Scan_uiv_new();
  void          Var_def_trav_helper(VAR_DEF_HELPER *helper, CHECK_OBJ &obj) const;
  void          Classify_dbf_error(BB_NODE *bb);
  BOOL          BB_has_real_stmtrep(BB_NODE *bb) const;
  BOOL          Is_bb_loop_inverted(BB_NODE *pred, BB_NODE *succ) const;
  BOOL          Can_swap_condition(BB_NODE *pred, BB_NODE *succ) const;
  void          Check_redundant_condition(BB_NODE *bb, CODEREP *cond, SRCPOS_HANDLE* sp_h, hash_set<IDTYPE>& visited);
  BOOL          Is_var_overflow_candidate(CODEREP *cr, SRCPOS_HANDLE *sp_h, hash_set<IDTYPE>& visited) const;
  BOOL          Is_overflow_checked(const hash_set<CODEREP*> &crs,
                                    BB_NODE *def, BB_NODE *use, hash_set<IDTYPE>& visited) const;
  BOOL          Scan_integer_overflow(UINT32 tag, CODEREP *arith, CODEREP *cr, STMTREP *sr);
  BOOL          Scan_pointer_for_misra(CODEREP *cr, STMTREP *sr);
  void          Scan_abs_for_misra(CODEREP *cr, STMTREP *sr);
  BOOL          Check_var_value(STMTREP *sr, CODEREP *cr, BB_NODE *bb, INT64 lower_bound);
  void          Scan_rule_based_error(BB_NODE *bb);
  void          Scan_eh_error(BB_NODE *bb);
  void          Classify_eh_error(BB_NODE *bb);
  void          Builtin_certj_obj11();
  void          Check_catch_bb_ignored_exception(BB_NODE *bb, SRCPOS_HANDLE *sp_h, regex_t **reg_exp_arr, INT32 arr_len, STMTREP **source_sr, CODEREP **source_cr, STMTREP **sink_sr, CODEREP **sink_cr, BOOL &ignored);
  void          Builtin_certj_err00();
  BOOL          Check_parent(CLASS_INFO *info, const char *ty_name);
  STMTREP      *Get_stmtrep_for_catch(EH_PATH *path, const char *name);
  void          Builtin_certcpp_err54();
  void          Builtin_certj_err08(STMTREP *stmt);
  void          Builtin_certj_tnh(STMTREP *stmt);
  void          Check_heap_new();
  void          Check_uaf(CODEREP *cr, STMTREP *sr);
  void          Check_mload_oob(CODEREP* cr, STMTREP* sr);
  SRCPOS_HANDLE*Find_global_use_bb(BB_NODE *bb, CODEREP *cr);
  void          Dump_call_path(DNA_NODE *dna, SRCPOS_HANDLE *srcpos_h, hash_set<IDTYPE> &visited);
  void          Dump_global_access_path(void);

  void          Initialize_ignore_ho_ids(MEM_POOL* mp);
  void          Finalize_ignore_ho_ids(MEM_POOL* mp);
  CODEREP_SET  *Ignore_cr_set()                 { return _ignore_cr_set; }
  void          Initialize_ignore_cr_set(MEM_POOL *mp);
  void          Finalize_ignore_cr_set()        { _ignore_cr_set = NULL; }

  // Classify DBZ/NPD error, can be called from IPSA for cross function analysis
  void          Classify_vul_error(CODEREP* x, BB_NODE* cur_bb, STMTREP* stmt,
                                   CALL_STACK& cs, SRCPOS_HANDLE* sp_h, ILODSTORBASE kind,
                                   hash_set<IDTYPE>&);
  void          Classify_vul_error(CODEREP* x, BB_NODE* cur_bb, STMTREP* stmt, VSYM_OBJ_REP* vor,
                                   CALL_STACK& cs, SRCPOS_HANDLE* sp_h, ILODSTORBASE kind,
                                   hash_set<IDTYPE>& visited, BOOL back_edge);
  BOOL          Report_vul_error(CODEREP* x, SRCPOS_HANDLE* sp_h, ILODSTORBASE kind,
                                 ISSUE_CERTAINTY ic);

  // Report uiv error when Classify_uiv_error for var detects error
  void          Report_uiv_error(CODEREP *var, SRCPOS_HANDLE *srcpos_h, BB_NODE *bb=NULL) const;
  HEAP_OBJ_REP *Find_or_create_hor(CODEREP *x, BB_NODE *bb, MEM_POOL *def_bbs_pool);

  // Only interfaces for VSA to report an error
  BOOL          Report_vsa_error(CODEREP *var, AUX_ID auxid, UINT32 anat, ISSUE_CERTAINTY ic,
                                 SRCPOS_HANDLE *srcpos_h) const;
  BOOL          Report_vsa_error(CODEREP *var, const char*, UINT32 anat, ISSUE_CERTAINTY ic,
                                 SRCPOS_HANDLE *srcpos_h) const;
  BOOL          Report_vsa_error(CODEREP *var, const char*, const char *anat, INT32 fix_cost,
                                 ISSUE_CERTAINTY ic, SRCPOS_HANDLE *srcpos_h) const;
  BOOL          Report_rvsa_info(CODEREP* x, AUX_ID auxid, UINT32 anat, ISSUE_CERTAINTY ic,
                                 SRCPOS_HANDLE *srcpos_h) const;

  BOOL          Report_xsca_error(CODEREP *var, const char*, const char *anat,
                                  SRCPOS_HANDLE *srcpos_h) const;
  // out-of-bound
  void          Perform_aofb_analysis();
  void          Perform_aob_analysis(COMP_UNIT *cu);
  void          Classify_aob_error(CODEREP *x, BB_NODE* cur_bb, STMTREP* stmt,
                                   CALL_STACK& cs, SRCPOS_HANDLE* sp_h,
                                   VSA_ADDRESS_INFO *info, std::vector<bool>& visited,
                                   VSYM_TRACKER *tracker = NULL);
  void          Classify_aob_error(CODEREP *x, ISSUE_CERTAINTY ic, SRCPOS_HANDLE *srcpos_h);
  void          Report_aob_if_no_size(CODEREP *arg, STMTREP *stmt);

  void          Classify_msf_error(BB_NODE *bb);
  IDTYPE        Put_isugrp_id(const char *s)    { return Ipsa()->Put_isugrp_id(s); }
public:
  // dump IR with heap object information
  void          Print_vo(VSYM_OBJ* vo, FILE *fp) const;
  void          Print_hor(FILE *fp) const;
  void          Print_ho_list(FILE *fp) const{ _heap_obj_list->Print(fp); }
  void          Print_vo_list(FILE *fp) const{ _vsym_obj_list->Print(this, fp); }
  void          Print_sr_2_rna_map(FILE *fp);
  void          Print_obj(const char *obj_title, FILE *fp) const;

  // for devirtualization and icall promotion
  BOOL          Find_icall_defs_new(STMTREP* sr, ICALL_TARGET_VECTOR&);
  BOOL          Find_icall_defs(CODEREP* cr, BB_NODE* bb,
                                CALL_STACK&, ICALL_TARGET_VECTOR&, hash_set<IDTYPE>&);
  BOOL          Find_vfunc(CODEREP* cr, BB_NODE* bb, INT32 ofst, ICALL_TARGET_VECTOR&, hash_set<IDTYPE>&, CALL_STACK &);

  // for register function related
  BOOL          Find_registered_function(STMTREP *sr, CODEREP *cr, ICALL_TARGET_VECTOR& targets);

  // for unkown fld
  VS_FLD_KIND   Find_fld_name(RNA_NODE *rna, UINT &fld_id);
  void          Find_cr_def_types(STMTREP *stmt, CODEREP *cr, DEF_TYS &def_types, VAR_DEF_KIND kind = FOR_GENERAL) const;
  IDTYPE        Create_or_get_fld_id(VS_FLD_KIND fld_kind, vector<const char *> &fld_names, UINT32 cr_id);
  BOOL          Vsym_match(STMTREP *stmt, CODEREP *cr, VSYM_FLD_REP *vfr1, VSYM_FLD_REP *vfr2) const;
  BOOL          Eval_vsym_match(STMTREP *stmt, CODEREP *cr, VSYM_FLD_REP *vfr1, VSYM_FLD_REP *vfr2) const;
  void          Resolve_vsym_fld_name(STMTREP *stmt, CODEREP *obj_cr, CODEREP *fld_cr);

  // check if cr is in a loop and access whole base heap_obj
  INT           Vor_access_whole_ho(STMTREP *sr, CODEREP *cr, VSYM_OBJ_REP *vor);


// =============================================================================
//
//  VSA::Set_phi_flag set different phi flag for HOR and VOR
//
// =============================================================================
  template <typename REPP> void
  Set_phi_flag(PHI_NODE *phi)                { }

// =============================================================================
//
//  VSA::Place_ro_phi_node places phi node for RSC_OBJs
//
// =============================================================================
  template <typename RSCOBJP, typename RSCOBJREPP,
            typename ROLISTP, typename ROLIST_ITER>
  void Place_ro_phi_node(PHILIST_MAP *bb_ro_philist,
                         RSCOBJP rsc_obj, ROLISTP rsc_obj_list,
                         ROLIST_ITER *ro_list_iter, RSCOBJREPP rsc_obj_rep,
                         BOOL check_before_insert = FALSE,
                         PHI_CACHE *phi_cache = NULL) {
    
    BB_LIST_ITER      bb_list_iter;
    BB_LIST_CONTAINER worklist;
    BB_NODE_SET_ITER  bns_iter;
    BB_NODE          *bbx, *bby;
    BS_ELT            bbs = Cfg()->Total_bb_count();
    MEM_POOL          bbset_pool;

    OPT_POOL_Initialize(&bbset_pool, "SSA bb set pool", FALSE, SSA_DUMP_FLAG);
    OPT_POOL_Push(&bbset_pool, SSA_DUMP_FLAG);

    BB_NODE_SET inserted(bbs, Cfg(), &bbset_pool, BBNS_EMPTY);
    BB_NODE_SET everonlist(bbs, Cfg(), &bbset_pool, BBNS_EMPTY);

    //  Iterate through the heap object list.
    FOR_ALL_NODE(rsc_obj, (*ro_list_iter), Init(rsc_obj_list)) {

      inserted.ClearD();
      everonlist.ClearD();
      worklist.Clear();

      //  Iterate through the BBs that rsc_obj is defined.
      FOR_ALL_ELEM (bbx, bb_list_iter, Init(rsc_obj->Def_bbs())) {
        if (everonlist.MemberP(bbx) == FALSE){
          everonlist.Union1D(bbx);
          worklist.Append(bbx, &bbset_pool);
        }
      }
      //  Add call bbs that rsc_obj may escape from these call sites
      // FOR_ALL_ELEM (bbx, bb_list_iter, Init(_call_bb_list)) {
      //   if (everonlist.MemberP(bbx) == FALSE){
      //     everonlist.Union1D(bbx);
      //     worklist.Append(bbx, &bbset_pool);
      //   }
      // }

      //  Go through the work list.
      while (bbx = worklist.Remove_head(&bbset_pool)) {
        //  Go through the dominator frontier of bbx.
        FOR_ALL_ELEM (bby, bns_iter, Init(bbx->Dom_frontier())) {
          if (inserted.MemberP(bby) == FALSE) {
            PHI_LIST *phi_list = (PHI_LIST *)(*bb_ro_philist).Lookup(bby->Id());
            if (phi_list == NULL) Cfg()->Print(stderr);
            Is_True(phi_list != NULL, ("VSA:Place_ho_phi, null philist in BB%d, a Dom_frontier of BB%d", bby->Id(), bbx->Id()));
            ID_PHI_MAP *phi_rsc_cache = NULL;
            if (check_before_insert) {
              // if checked by phi_cache, search from cache instead, faster solution
              if (phi_cache) {
                Is_True(phi_cache->size() > bby->Id(), ("phi_cache is wrong"));
                phi_rsc_cache = phi_cache->at(bby->Id());
                Is_True(phi_rsc_cache, ("phi cache is not set up"));
                if (phi_rsc_cache && phi_rsc_cache->find(rsc_obj->Id()) != phi_rsc_cache->end()) {
                  continue;
                }
              } else if (phi_list->Search_phi_node(rsc_obj->Id()) != NULL) {
                continue;
              }
            }
            PHI_NODE *phi = phi_list->New_phi_node(rsc_obj->Id(), Mem_pool(), bby);
            if (phi_rsc_cache) {
              phi_rsc_cache->insert(std::make_pair(rsc_obj->Id(), phi));
            }
            phi->Set_live();
            Set_phi_flag<RSCOBJREPP>(phi);
            Is_Trace(Tracing(), (TFile, "######## VSA::Place rsc_obj PHI in BB%d as DF of BB%d: ", bby->Id(), bbx->Id()));
            rsc_obj_rep = Gen_phi(phi, rsc_obj);
            rsc_obj_rep->Set_srcpos_node(bby, Comp_unit()->Dna(), PATHINFO_PHI);

            inserted.Union1D(bby);
            if (everonlist.MemberP(bby) == FALSE) {
              everonlist.Union1D(bby);
              worklist.Append(bby, &bbset_pool);
            }
          }
        }
      }
    }

    OPT_POOL_Pop(&bbset_pool, SSA_DUMP_FLAG);
    OPT_POOL_Delete(&bbset_pool, SSA_DUMP_FLAG);
  }

}; // end of class VSA

template<> inline __attribute__((always_inline)) void
VSA::Set_phi_flag<HEAP_OBJ_REP*>(PHI_NODE *phi) { VSA_PHI_NODE(phi).Set_res_is_hor(); }
template<> inline __attribute__((always_inline)) void
VSA::Set_phi_flag<VSYM_OBJ_REP*>(PHI_NODE *phi) { VSA_PHI_NODE(phi).Set_res_is_vor(); }


template <> inline __attribute__((always_inline)) TOR_LIST_OLD*
VSA::Cr_2_tor_list<TOR_LIST_OLD>(CODEREP *cr)
{
  Is_True_Ret(VSA_Enable_TAG_OLD, ("VSA_Enable_TAG_OLD is off but enter old TOR_LIST_OLD"), NULL);
  Is_True_Ret(cr, ("Cr is NULL."), NULL);
  INTPTR tor_list = _cr_2_tor_list_map.Lookup(cr->Coderep_id());
  return (TOR_LIST_OLD *)tor_list;
}
template <> inline __attribute__((always_inline)) TOR_LIST*
VSA::Cr_2_tor_list<TOR_LIST>(CODEREP *cr)
{
  Is_True_Ret(!VSA_Enable_TAG_OLD, ("VSA_Enable_TAG_OLD is on but enter new TOR_LIST_OLD"), NULL);
  Is_True_Ret(cr, ("Cr is NULL."), NULL);
  INTPTR tor_list = _cr_2_tor_list_map.Lookup(cr->Coderep_id());
  return (TOR_LIST *)tor_list;
}

template <> inline __attribute__((always_inline)) TOR_LIST_OLD*
VSA::Cr_2_tor_list<TOR_LIST_OLD>(CODEREP *cr) const
{
  Is_True_Ret(VSA_Enable_TAG_OLD, ("VSA_Enable_TAG_OLD is off but enter old TOR_LIST_OLD"), NULL);
  Is_True_Ret(cr, ("Cr is NULL."), NULL);
  INTPTR tor_list = _cr_2_tor_list_map.Lookup(cr->Coderep_id());
  return (TOR_LIST_OLD *)tor_list;
}
template <> inline __attribute__((always_inline)) TOR_LIST*
VSA::Cr_2_tor_list<TOR_LIST>(CODEREP *cr) const
{
  Is_True_Ret(!VSA_Enable_TAG_OLD, ("VSA_Enable_TAG_OLD is on but enter new TOR_LIST_OLD"), NULL);
  Is_True_Ret(cr, ("Cr is NULL."), NULL);
  INTPTR tor_list = _cr_2_tor_list_map.Lookup(cr->Coderep_id());
  return (TOR_LIST *)tor_list;
}

template <> inline __attribute__((always_inline)) TOR_LIST_OLD *
VSA::Ror_2_tor_list<CODEREP*, TOR_LIST_OLD>(CODEREP *cr)
{
  return Cr_2_tor_list<TOR_LIST_OLD>(cr);
}

template <> inline __attribute__((always_inline)) TOR_LIST *
VSA::Ror_2_tor_list<CODEREP*, TOR_LIST>(CODEREP *cr)
{
  return Cr_2_tor_list<TOR_LIST>(cr);
}

template <> inline __attribute__((always_inline)) TOR_LIST_OLD *
VSA::Ror_2_tor_list<HEAP_OBJ_REP*, TOR_LIST_OLD>(HEAP_OBJ_REP *hor)
{
  return hor->Tor_list();
}

template <> inline __attribute__((always_inline)) void
VSA::Enter_ror_tor_list<CODEREP *, TOR_LIST_OLD>(CODEREP *cr, TOR_LIST_OLD *tor_list)
{
  Is_True_Ret(VSA_Enable_TAG_OLD, ("VSA_Enable_TAG_OLD is off but enter old TOR_LIST_OLD"));
  Is_True_Ret(!Cr_2_tor_list<TOR_LIST_OLD>(cr), ("multiple tor_list entered for cr%d", cr->Coderep_id()));
  _cr_2_tor_list_map.Insert(cr->Coderep_id(), (INTPTR)tor_list);
}

template <> inline __attribute__((always_inline)) void
VSA::Enter_ror_tor_list<CODEREP *, TOR_LIST>(CODEREP *cr, TOR_LIST *tor_list)
{
  Is_True_Ret(!VSA_Enable_TAG_OLD, ("VSA_Enable_TAG_OLD is on but enter new TOR_LIST_OLD"));
  Is_True_Ret(!Cr_2_tor_list<TOR_LIST>(cr), ("multiple tor_list entered for cr%d", cr->Coderep_id()));
  _cr_2_tor_list_map.Insert(cr->Coderep_id(), (INTPTR)tor_list);
}

template <> inline __attribute__((always_inline)) void
VSA::Enter_tor_ror<CODEREP *>(CODEREP *cr, TAG_OBJ_REP *tor)
{
  TOR_LIST_OLD *tor_list = Cr_2_tor_list<TOR_LIST_OLD>(cr);
  if (!tor_list) {
    tor_list = CXX_NEW(TOR_LIST_OLD, Mem_pool());
    _cr_2_tor_list_map.Insert(cr->Coderep_id(), (INTPTR)tor_list);
    tor_list->Append(tor);
  } else {
    tor_list->Append(tor);
  }
}

#endif  // opt_vsa_INCLUDE
