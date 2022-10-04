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

// =============================================================================
// =============================================================================
//
// Module: opt_dna.cxx
//
// =============================================================================
//
// Description:
//
// Vulnerability Static Analysis
//
// =============================================================================
// =============================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#include "defs.h"
#include "dwarf_DST.h"
#include "dwarf_DST_mem.h" // DST_TYPE
#include "errors.h"
#include "erglob.h"
#include "glob.h"       // for Cur_PU_Name
#include "mempool.h"
#include "pu_info.h"    // for Current_PU_Info
#include "tracing.h"    /* for TFile */
#include "stab.h"
#include "irbdata.h"
#include "cxx_memory.h"
#include "be_symtab.h"
#include "erbe.h"

#include "opt_defs.h"
#include "opt_config.h"
#include "opt_cfg.h"
#include "opt_dbg.h"
#include "opt_estr.h"
#include "opt_etable.h"
#include "opt_main.h"
#include "bb_node_set.h"
#include "opt_util.h"
#include "opt_vsa_util.h"
#include "opt_mu_chi.h"
#include "opt_ssa.h"
#include "opt_sym.h"
#include "opt_alias_rule.h"
#include "opt_cvtl_rule.h"
#include "opt_lftr2.h"
#include "opt_vsa.h"
#include "opt_dna.h"
#include "opt_vsa_eh.h"
#include "whirl_file_mgr.h"   // for cross file analysis (cfa)
#include "opt_addr_util.h"
#include "config_vsa.h"
#include "cxx_class_hierarchy_bldr.h"
#include "j_class_hierarchy_bldr.h"
#include <queue>
#include "opt_vsa_rbc.h"
#include "builtin_rule_defs.h"
#include "pro_core.h"
#include "intrn_info.h"
#include "whirl_file_ctx.h"
#include "ir_bwrite.h"
#include "file_util.h"
#include "opt_main.h"
#include "opt_rvi.h"
#include "timing.h"

extern COMP_UNIT *g_comp_unit;

// =============================================================================
//
// Print function for tracing DNA node
//
// =============================================================================
DNA_NODE::~DNA_NODE(void)
{
  INT i;
  for (i = VAR_INIT_ID; i < _call_list.size(); ++i)
    CXX_DELETE(_call_list[i], Mem_pool());

  for (i = VAR_INIT_ID; i < _parm_list.size(); ++i)
    CXX_DELETE(_parm_list[i], Mem_pool());
  
  for (i = VAR_INIT_ID; i < _glob_list.size(); ++i)
    CXX_DELETE(_glob_list[i], Mem_pool());

  for (i = PDV_INIT_ID; i < _retv_list.size(); ++i)
    CXX_DELETE(_retv_list[i], Mem_pool());

  
  for (i = PDV_INIT_ID; i < _rgvl_list.size(); ++i)
    CXX_DELETE(_rgvl_list[i], Mem_pool());

  for (i = VAR_INIT_ID; i < _clby_list.size(); ++i)
    CXX_DELETE(_clby_list[i], Mem_pool());
}

// =============================================================================
//
// VAR_NODE::Ipreg
// return dedicated register for this param if exists
//
// =============================================================================
CODEREP *
VAR_NODE::Ipreg(void) const
{
  if (_cr == NULL ||
      _cr->Is_flag_set(CF_DEF_BY_PHI) ||
      _cr->Is_flag_set(CF_DEF_BY_CHI))
    return NULL;

  STMTREP *def = _cr->Defstmt();
  if (def == NULL || def->Opr() != OPR_STID)
    return NULL;

  Is_True(def->Lhs() == _cr, ("stid lhs mismatch"));
  CODEREP *rhs = def->Rhs();
  while (rhs->Kind() == CK_OP &&
         (rhs->Opr() == OPR_CVT ||
          rhs->Opr() == OPR_CVTL))
    rhs = rhs->Opnd(0);

  // TODO: pass in Comp_unit and check rhs is dedicated preg
  Is_True(rhs->Kind() == CK_VAR &&
          rhs->Is_flag_set(CF_DEF_BY_CHI) &&
          rhs->Def_at_entry(),
          ("bad param stid rhs"));

  return rhs;
}

// =============================================================================
//
// Update_flags for PDV_NODE
//
// =============================================================================
UINT32
PDV_NODE::Update_flags(DNA_NODE* dna)
{
  UINT32 ret = 0;
  CODEREP* rhs = Stmt()->Rhs();
  if (rhs->Kind() == CK_CONST && rhs->Const_val() == 0) {
    ret = Set_flag(ARG_VALUE_IVAD);
  }
  if (rhs->Kind() == CK_VAR) {
    IDTYPE param = dna->Is_param(rhs);
    if (param != INVALID_VAR_IDX) {
      // copy param flag, probably not correct
      ret = Set_flag(dna->Parm_flags(param));
    }
    ret |= Copy_flag_from_cr(rhs);
  }
  return ret;
}

// =============================================================================
//
// Update_flags from CODEREP to PDV_NODE
//
// =============================================================================
BOOL
PDV_NODE::Copy_flag_from_cr(CODEREP* cr)
{
  UINT32 old_flag = Flags();
  if (cr->Value_def())
    Set_flag(ARG_VALUE_UDEF);
  if (cr->Value_maydef())
    Set_flag(ARG_VALUE_MAYD);
  if (cr->Value_invalid_addr())
    Set_flag(ARG_VALUE_IVAD);
  if (cr->Value_maydangling())
    Set_flag(ARG_VALUE_MDANG);
  if (cr->Value_malloc())
    Set_flag(REF_MALLOCED);

  return old_flag != Flags();
}

// =============================================================================
//
// Update_flags from PDV_NODE to CODEREP
//
// =============================================================================
BOOL
PDV_NODE::Copy_flag_to_cr(CODEREP* cr) const
{
  UINT32 old_flag = cr->Isvar_flags();
  cr->Reset_value_defmaydef();
  if ((_flags & ARG_VALUE_UDEF) == ARG_VALUE_UDEF)
    cr->Set_value_def();
  if ((_flags & ARG_VALUE_MAYD) == ARG_VALUE_MAYD)
    cr->Set_value_maydef();

  cr->Reset_value_invalidmaydangling();
  if ((_flags & ARG_VALUE_IVAD) == ARG_VALUE_IVAD)
    cr->Set_value_invalid_addr();
  if ((_flags & ARG_VALUE_MDANG) == ARG_VALUE_MDANG)
    cr->Set_value_maydangling();

  if ((_flags & REF_MALLOCED) == REF_MALLOCED)
    cr->Set_value_malloc();

  return old_flag != cr->Isvar_flags();
}

// =============================================================================
//
// Print function for PDV_NODE
//
// =============================================================================
void
PDV_NODE::Print(FILE *fp) const
{
  const char* return_method = "unknown";
  if (Kind() & BY_RETURNSTMT) return_method = "return statement";
  else if (Kind() & BY_PARAMETER) return_method = "reference paramenter";
  else if (Kind() & BY_GLOBALVAR) return_method = "global variable";
  else if (Kind() & BY_FREEIPARM) return_method = "free parm or *parm";
  else if (Kind() & BY_FREEGLOBL) return_method = "free global or *global";

  fprintf(fp, "(predicate: %s | value: cr%d | return by: %s | Oparam id: %d | Flags: 0x%x),\n",
          Predicate() ? "conditional" : "unconditional",
          Stmt()->Rhs()->Coderep_id(), return_method, Oparam(), Flags());
}

// =============================================================================
//
// Print function for tracing DNA node
//
// =============================================================================
void
DNA_NODE::Print(BOOL print_callsite_stmt, FILE *fp) const
{
  CONTEXT_SWITCH context(this);
  Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG), (TFile, "%s \n",SBar));
  char* fname = Vsa_demangle(ST_name(St()));
  fprintf(fp, "DNA_NODE(ID:%d,%s): %s, File_id: %d, Pu_id: %d, Flag: 0x%x, RBC_flag: 0x%x, Stk_size: %d, # callsites: %d\n",
          Dna_idx(), Non_functional() ? "rule" : "user", fname, File_idx(), Pu_idx(),
          Flags(), Rbc_flags(), Stack_size(), Callsite_cnt());
  // ATTENTION: the memory was allocated in the Vsa_demangle, do not allocate memory by yourself
  if(fname)
    free(fname);
  INT i;
  if (_fsm_list != NULL && _fsm_list->size() > 0) {
    fprintf(fp, "  FSMs binded:");
    for (i = 0; i < _fsm_list->size(); ++i) {
      fprintf(fp, "  %s", (*_fsm_list)[i]);
    }
    fprintf(fp, "\n");
  }

  if (_parm_list.size() > VAR_INIT_ID) {
    fprintf(fp, "  Parm List:\n");
    for (i = VAR_INIT_ID; i < _parm_list.size(); ++i) {
      fprintf(fp, "    Parm%d: {%s(0x%x), cr%d, ipreg%d}", i,
              ST_name((_parm_list[i])->St_idx()), (_parm_list[i])->St_idx(),
              ((_parm_list[i])->Cr() == NULL)?0:(_parm_list[i])->Cr()->Coderep_id(),
              ((_parm_list[i])->Ipreg() == NULL)?0:(_parm_list[i])->Ipreg()->Coderep_id());
      if ((_parm_list[i]->Flags() & REF_ILOAD) != 0) fprintf(fp, " ref_iload");
      if ((_parm_list[i]->Flags() & REF_ISTORE) != 0) fprintf(fp, " ref_istore");
      if ((_parm_list[i]->Flags() & REF_COPIED) != 0) fprintf(fp, " ref_copied");
      if ((_parm_list[i]->Flags() & REF_MALLOCED) != 0) fprintf(fp, " ref_malloced");
      if ((_parm_list[i]->Flags() & REF_FREED) != 0) fprintf(fp, " ref_freed");
      fprintf(fp, "\n");
    }
  }

  if (_glob_list.size() > VAR_INIT_ID) {
    fprintf(fp, "  Referenced Global Variable List:\n");
    for (i = VAR_INIT_ID; i < _glob_list.size(); ++i) {
      fprintf(fp, "    global%d: {%s(%d), cr%d},", i,
              ST_name((_glob_list[i])->St_idx()), (_glob_list[i])->St_idx(),
              ((_glob_list[i])->Cr() == NULL)?0:(_glob_list[i])->Cr()->Coderep_id());
      if ((_glob_list[i]->Flags() & REF_GLOBREF) != 0) fprintf(fp, " ref_globref");
      if ((_glob_list[i]->Flags() & REF_GLOBCHG) != 0) fprintf(fp, " ref_globchg");
      if ((_glob_list[i]->Flags() & REF_FREED) != 0) fprintf(fp, " ref_freed");
      fprintf(fp, "\n");
    }
  }

  if (_retv_list.size() > PDV_INIT_ID) {
    fprintf(fp, "  Return Value List:\n");
    for (i = PDV_INIT_ID; i < _retv_list.size(); ++i) {
      _retv_list[i]->Print(fp);
    }
  }

  if (_rgvl_list.size() > PDV_INIT_ID) {
    fprintf(fp, "  Return Global Value List:\n");
    for (i = PDV_INIT_ID; i < _rgvl_list.size(); ++i) {
      _rgvl_list[i]->Print(fp);
    }
  }

  if (_call_list.size() > VAR_INIT_ID) {
    g_comp_unit = _comp_unit;
    fprintf(fp, "  Call Sites List:\n");
    for (i = VAR_INIT_ID; i < _call_list.size(); ++i) {
      _call_list[i]->Print(print_callsite_stmt, fp);
    }
  }

  if (_clby_list.size() > VAR_INIT_ID) {
    g_comp_unit = _comp_unit;
    fprintf(fp, "  Caller Sites List:\n");
    for (i = VAR_INIT_ID; i < _clby_list.size(); ++i) {
      _clby_list[i]->Print(FALSE, fp);
    }
  }
}


// =============================================================================
// FPTR_ASSGN_INFO::Analyze()
// Ananlyze stmtrep to fill the global function pointer assignment info
// =============================================================================
ST *
FPTR_ASSGN_INFO::Analyze(IPSA *ipsa, DNA_NODE *dna, STMTREP *sr)
{
  Is_True(ipsa != NULL && dna != NULL && sr != NULL &&
          !dna->Non_functional(), ("bad params"));
  if (sr->Opr() != OPR_STID)
    return NULL;  // TODO: handle ISTORE later

  CODEREP *lhs = sr->Lhs();
  Is_True(lhs != NULL && lhs->Kind() == CK_VAR, ("bad lhs"));
  TY_IDX ty = sr->Lhs()->object_ty();
  if (TY_kind(ty) !=  KIND_POINTER ||
      TY_kind(TY_pointed(ty)) != KIND_FUNCTION)
    return NULL;

  OPT_STAB *stab = dna->Comp_unit()->Opt_stab();
  Is_True(stab != NULL, ("invalid stab"));
  ST *st = stab->Aux_stab_entry(lhs->Aux_id())->St();
  if (st == NULL || ST_level(st) != GLOBAL_SYMTAB)
    return NULL;

  _dna_idx = dna->Dna_idx();
  _offset = lhs->Offset();
  _assign = sr;

  return st;
}

// =============================================================================
// FPTR_ASSGN_INFO::Callee()
// Get callee of the assignment if rhs is LDA
// =============================================================================
DNA_NODE *
FPTR_ASSGN_INFO::Callee(IPSA *ipsa) const
{
  Is_True(_assign != NULL, ("bad assign"));
  CODEREP *rhs = _assign->Rhs();
  if (rhs->Kind() == CK_LDA) {
    ST *lda_st = rhs->Lda_base_st();
    Is_True_Ret(lda_st && ST_class(lda_st) == CLASS_FUNC,
            ("not function callee_st"), NULL);
    DNA_NODE *caller = ipsa->Get_dna(_dna_idx);
    Is_True(caller != NULL, ("bad caller"));
    return ipsa->Get_dna(caller->File_idx(), lda_st);
  }
  return NULL;
}

// =============================================================================
//
// add a new assign from src->tgt
//
// =============================================================================
void
ASSIGN_INFO::Enter_assign(IDTYPE tgt, IDTYPE src)
{
  for(int tgt_idx = 0; tgt_idx < _assign_info.size(); tgt_idx++) {
    ASSIGN_ITEM* item = _assign_info[tgt_idx];
    if(item->_tgt == tgt) {
      if(!item->Exist_src(src)) {
        item->_srcs.push_back(src);
        return;
      }
    }
  }
  ASSIGN_ITEM *item = CXX_NEW(ASSIGN_ITEM(tgt, src), _pool);
  _assign_info.push_back(item);
}


// =============================================================================
//
// Get tgt's assign souces
//
// =============================================================================
SRC_VECTOR *
ASSIGN_INFO::Get_assign_src(IDTYPE tgt) 
{
  for(int tgt_idx = 0; tgt_idx < _assign_info.size(); tgt_idx++) {
    ASSIGN_ITEM *item = _assign_info[tgt_idx];
    if(item->_tgt == tgt) {
      return &item->_srcs;
    }
  }
  return NULL;
}


// =============================================================================
//
// Print ASSIGN_INFO
//
// =============================================================================
void
ASSIGN_INFO::Print(FILE *fp)
{
  fprintf(fp, "ASSIGN INFO Dump:\n");
  for(int tgt_idx = 0; tgt_idx < _assign_info.size(); tgt_idx++) {
    ASSIGN_ITEM* item = _assign_info[tgt_idx];
    fprintf(fp, "tgt: %d, srcs:", item->_tgt);
    for(int src_idx = 0; src_idx < item->_srcs.size(); src_idx++) {
      fprintf(fp, "%d ", item->_srcs[src_idx]);
    }
  }
}

// =============================================================================
//
// IPSA::IPSA
//
// =============================================================================
IPSA::IPSA():
  _mem_pool("IPSA_global_pool", FALSE),
  _loc_pool("IPSA_local_pool", FALSE),
  _st(DNA_INIT_IDX, _mem_pool()),
  _dnaid_to_dnanode(DNA_INIT_IDX, (DNA_NODE*)NULL, DNODE_VECTOR::allocator_type(_mem_pool())),
  _rnaid_to_rnanode(RNA_INIT_IDX, (RNA_NODE*)NULL, RNODE_VECTOR::allocator_type(_mem_pool())),
  _pre_order_dnode(DNODE_VECTOR::allocator_type(_mem_pool())),
  _post_order_dnode(DNODE_VECTOR::allocator_type(_mem_pool())),
  _fst_fptr_map(3, __gnu_cxx::hash<uint64_t>(), std::equal_to<uint64_t>(), FST_FPTR_ALLOCATOR(_mem_pool())),
  _shutdown_hooks(3, __gnu_cxx::hash<UINT32>(), __gnu_cxx::equal_to<UINT32>(),DNA_IDX_SET::allocator_type(_mem_pool())),
  _rbc_assign_infos(7, NULL, _mem_pool(), FALSE),
  _fld_name_id_map(7, __gnu_cxx::hash<const char *>(), streq(), FLD_NAME_ALLCATOR(_mem_pool())),
  _isu_grp_map(7, __gnu_cxx::hash<const char *>(), streq(), SI_ALLCATOR(_mem_pool()))
{
  _phase = DNA_CREATION;
  _last_heap_obj_id = 0; _last_vsym_obj_id = 0; _last_fld_name_id = 0;
  _last_isu_grp_id = 0;
  _trav_counter = RNA_INIT_IDX;
  _trav_counter_is_set = FALSE;
  _glob_cha = CXX_NEW(CLASS_HIERARCHY(_mem_pool(), _mem_pool()), _mem_pool());
  _default_hor = Create_special_hor(TRUE);
  _default_vor = Create_special_vor(_default_hor, TRUE);
  _null_hor = Create_special_hor(FALSE);
  _null_vor = Create_special_vor(_null_hor, FALSE);
  _pu_counter = 0;
  _rbc = NULL;
  _rbc_assign_infos.Init();
  // if you want to get the intrinsic id, please dump entire intrinsic table
  // must be dumped before vsa, prevent assertion from the following vsa phases
  if (VSA_Dump_Intrn_Table && Get_Trace(TP_WOPT2, VSA_DUMP_FLAG)) {
    Print_intrn_table(TFile);
  }
}

IDTYPE
IPSA::Get_isugrp_id(const char * str)
{
  STR_MAP::iterator it = _isu_grp_map.find(str);
  if (it == _isu_grp_map.end()) {
    return 0;
  } else {
    return it->second;
  }
}


IDTYPE
IPSA::Put_isugrp_id(const char *str)
{
  IDTYPE rid = Get_isugrp_id(str);
  if (rid == 0) {
    // does not exist before, therefore call put
    rid = New_isugrp_id();
    // make a copy of str in global pool for full ownership of str
    str = Clone_string((char*)str);
    _isu_grp_map[str] = rid;
  }

  return rid;
}


// =============================================================================
//
// Create special heap_obj_rep
//
// =============================================================================
HEAP_OBJ_REP *
IPSA::Create_special_hor(BOOL def_hor)
{
  HEAP_OBJ *heap_obj = CXX_NEW(HEAP_OBJ(New_heapobj_id(), NULL, Mem_pool()),
                               Mem_pool());
  heap_obj->Set_stack(NULL);
  HEAP_OBJ_REP *entry_chi = CXX_NEW(HEAP_OBJ_REP(heap_obj, NULL),
                                    Mem_pool());
  entry_chi->Set_version_entry_chi();
  entry_chi->Set_srcpos_node((STMTREP*)NULL, NULL, PATHINFO_NONE);
  if (def_hor == FALSE)
    entry_chi->Set_attr(ROR_DEF_BY_NULL);
  if (!VSA_New_HVA) {
    heap_obj->Set_stack(CXX_NEW(STACK<HOR_PAIR>(Mem_pool()), Mem_pool()));
    heap_obj->Push(entry_chi, 0);    // push entry chi to stack
  }
  heap_obj->Set_entry_chi(entry_chi);
  return entry_chi;
}

// =============================================================================
//
// Create special vsym_obj_rep
//
// =============================================================================
VSYM_OBJ_REP *
IPSA::Create_special_vor(HEAP_OBJ_REP *base_hor, BOOL def_vor)
{
  VSYM_FLD_REP vfr(FLD_K_ANY, 0, 0);
  VSYM_OBJ *vsym_obj = CXX_NEW(VSYM_OBJ(New_vsymobj_id(), vfr, Mem_pool()),
                               Mem_pool());
  vsym_obj->Set_kind(RSC_KIND_VSYM);
  vsym_obj->Set_base_hor(base_hor);
  vsym_obj->Set_stack(NULL);
  base_hor->Set_vsym_obj(vsym_obj);
  VSYM_OBJ_REP *entry_chi = CXX_NEW(VSYM_OBJ_REP(vsym_obj),
                                    Mem_pool());
  entry_chi->Set_version_entry_chi();
  entry_chi->Set_srcpos_node((STMTREP*)NULL, NULL, PATHINFO_CHI);
  entry_chi->Set_hor(base_hor);
  if (def_vor == FALSE)
    entry_chi->Set_attr(ROR_DEF_BY_NULL);
  if (!VSA_New_HVA) {
    vsym_obj->Set_stack(CXX_NEW(STACK<VOR_PAIR>(Mem_pool()), Mem_pool()));
    vsym_obj->Push(entry_chi, 0);    // push entry chi to stack
  }
  vsym_obj->Set_entry_chi(entry_chi);
  return entry_chi;
}

// =============================================================================
//
// Print function for tracing DNODE_VECTOR
//
// =============================================================================
void
IPSA::Print_dnode_vector(const DNODE_VECTOR& vec, FILE* fp) const
{
  DNODE_VECTOR::const_iterator iter  = vec.begin();
  DNODE_VECTOR::const_iterator end = vec.end();
  for ( ; iter != end; ++iter ) {
    fprintf(fp, " [%4d] %s\n", (*iter)->Dna_idx(), (*iter)->Fname());
  }
}

// =============================================================================
//
// Print function for tracing DNA node
//
// =============================================================================
void
IPSA::Print(FILE *fp) const
{
  INT i;

  fprintf(fp, "%s\nIPSA DNA List\n", DBar);
  if (_dnaid_to_dnanode.size() > DNA_INIT_IDX) {
    for (i = DNA_INIT_IDX; i < _dnaid_to_dnanode.size(); ++i) {
      _dnaid_to_dnanode[i]->Print(FALSE, fp);
    }
  }

  fprintf(fp, "%s\nIPSA RNA List\n", DBar);
  if (_rnaid_to_rnanode.size() > RNA_INIT_IDX) {
    for (i = RNA_INIT_IDX; i < _rnaid_to_rnanode.size(); ++i) {
      _rnaid_to_rnanode[i]->Print(FALSE, fp);
    }
  }
}

// =============================================================================
//
// Print function for different traversal order
//
// =============================================================================
template<DNA_TRAVERSAL_ORDER _Order>
void
IPSA::Print(FILE *fp)
{
  if (_Order == DNA_TRAV_PRE_ORDER)
    fprintf(fp, "....IPSA::Print Dumping PRE ORDER DNA_NODE after rebuild ....\n");
  else if (_Order == DNA_TRAV_POST_ORDER)
    fprintf(fp, "....IPSA::Print Dumping POST ORDER DNA_NODE after rebuild ....\n");
  else if (_Order == DNA_TRAV_TOP_DOWN_ORDER)
    fprintf(fp, "....IPSA::Print Dumping TOP DOWN ORDER DNA_NODE ....\n");
  else
    return;
    
  for(DNODE_ITER<_Order> iter(this);
      !iter.Is_end(); iter.Next()) {
    DNA_NODE* func = iter.Current();
    CONTEXT_SWITCH context(func);
    char* fname = Vsa_demangle(func->Fname());
    fprintf(fp, "D%d: %s callsite:%d, call-by:%d\n", func->Dna_idx(), fname,
                func->Callsite_cnt(), func->Clby_list_cnt() - 1);
    // ATTENTION: the memory was allocated in the Vsa_demangle, do not allocate memory by yourself
    if(fname)
      free(fname);
    for (int i = VAR_INIT_ID; i < func->Call_list()->size(); i++) {
      RNA_NODE* rna = (*func->Call_list())[i];
      if(rna->Callee_st()) {
        fname = Vsa_demangle(ST_name(rna->Callee_st()));
        fprintf(fp, "   R%d: L:%d, %s\n", rna->Rna_idx(), Srcpos_To_Line(rna->Linenum()), fname);
        // ATTENTION: the memory was allocated in the Vsa_demangle, do not allocate memory by yourself
        if (fname)
          free(fname);
      }
      else if(rna->Is_indirect_call()) {
        fprintf(fp, "  IR%d: L:%d", rna->Rna_idx(), Srcpos_To_Line(rna->Linenum()));
        for(CALLEE_VECTOR::const_iterator iter = rna->Callee_list().begin();
            iter != rna->Callee_list().end(); iter++) {
          DNA_NODE* callee = Get_dna(iter->Callee());
          fname = Vsa_demangle(callee->Fname());
          fprintf(fp, ", %s", fname);
          if (callee->Non_functional())
            fprintf(fp, " <rbc>");
          // ATTENTION: the memory was allocated in the Vsa_demangle, do not allocate memory by yourself
          if(fname)
            free(fname);
        }
        fprintf(fp, "\n");
      }
    }
  }
  fprintf(fp, "....IPSA::Print End Dumping DNA_NODE ....\n");
}

// =============================================================================
//
// Set CLASS_INFO according to function class name
//
// =============================================================================
void
DNA_NODE::Set_cls_info(IPSA *ipsa)
{
  STRING_BUFFER buf(strlen(Fname()) + 1);
  const char * cls_name = CLASS_HIERARCHY::Extract_class_name(Fname(), &buf);
  if(cls_name == NULL) {
    return;
  }
  CLASS_INFO *info = ipsa->Glob_cha()->Get_class_info(cls_name);
  if(info != NULL) {
    _cls_info = info;
  }
}

// =============================================================================
// DNA_NODE::Get_eh_throw_type(STMTREP* call)
// @parm ty: output value, if not null set eh type idx
// @return: return 0 if not found, otherwise the (STR_IDX) name of the type to the thrown object
// =============================================================================
STR_IDX
DNA_NODE::Get_eh_throw_type(STMTREP* call, TY_IDX *throw_ty) const
{
  if (call->Opr() == OPR_CALL) {
    if (strcmp(ST_name(call->St()), "__cxa_throw") == 0) {
      // C++ throw
      Is_True(call->Rhs()->Kid_count() == 3 &&
              call->Rhs()->Opnd(1)->Opr() == OPR_PARM,
              ("invalud c++ throw"));
      CODEREP* ti = call->Rhs()->Opnd(1)->Ilod_base();
      Is_True(ti->Kind() == CK_LDA,
              ("invalid c++ throw parameter"));
      return ST_name_idx(ti->Lda_base_st());
    }
    // TODO: handle __cxa_rethrow
    // else if (strcmp(ST_name(call->St()), "__cxa_rethrow") == 0)
    else if (strcmp(ST_name(call->St()), "_Jv_Throw") == 0) {
      // Java throw
      Is_True(call->Rhs()->Kid_count() == 1 &&
              call->Rhs()->Opnd(0)->Opr() == OPR_PARM,
              ("invalid java throw"));
      if (call->Rhs()->Kid_count() != 1 ||
          call->Rhs()->Opnd(0)->Opr() != OPR_PARM) return 0; // Is_True
      CODEREP* obj = call->Rhs()->Opnd(0)->Ilod_base();
      if (obj->Kind() == CK_CONST) {
        // Java Case: 'throw null;'
        // When throwing null, we return 0 to indicate
        // that we could not find the according EH-type symbol
        // Semantically, this means the thrown object is simply null.
        return 0;
      }
      Is_True(obj->Kind() == CK_VAR || obj->Kind() == CK_IVAR,
              ("invalid java throw parameter, kind : %d", obj->Kind()));
      if (!(obj->Kind() == CK_VAR || obj->Kind() == CK_IVAR)) return 0; //
      TY_IDX obj_ty = obj->object_ty();
      if (TY_kind(obj_ty) != KIND_POINTER ||
          TY_kind(TY_pointed(obj_ty)) != KIND_STRUCT) {
        STPATH* stp = Get_stpath(call, call->Rhs()->Opnd(0)->Ilod_base());
        Is_True(stp != NULL, ("no cprop info for throw parameter"));
        if (stp == NULL) return 0; // Is_True
        obj_ty = ST_type(stp->St_idx());
      }
      Is_True(TY_kind(obj_ty) == KIND_POINTER,
              ("invalid java throw object ref type"));
      if (TY_kind(obj_ty) != KIND_POINTER) return 0; // Is_True
      TY_IDX obj_ty_ptd = TY_pointed(obj_ty);
      Is_True(TY_kind(obj_ty_ptd) == KIND_STRUCT,
              ("invalid java throw object type"));
      if (TY_kind(obj_ty_ptd) != KIND_STRUCT) return 0; // Is_True
      if(throw_ty != NULL) {
        *throw_ty = obj_ty_ptd;
      }
      return TY_name_idx(obj_ty_ptd);
    }
  }
  else if (call->Opr() == OPR_INTRINSIC_CALL) {
    Is_True(call->Rhs()->Intrinsic() != INTRN_THROW,
            ("throw should not be intrinsic"));
  }
  return 0;
}

// =============================================================================
//
// DNA_NODE::Check_caller_arg_val_attr, checks attribute of the argument as
//      specified by which_arg and report the error if it is conclusive,
//      otherwise traverse up the call chain recursively.
//
// =============================================================================
void
DNA_NODE::Check_caller_arg_val_attr(IPSA *ipsa, IDTYPE which_arg, RNA_NODE* rna,
                                    CALL_STACK& cs, SRCPOS_HANDLE *sp_h,
                                    ILODSTORBASE kind, VSYM_TRACKER *tracker)
{
  CONTEXT_SWITCH context(this);
  DNA_NODE *initializer_cxt = sp_h->Context();

  STMTREP* stmt = rna->Callstmt();
  sp_h->Append_data(stmt, this, PATHINFO_DNA_CALLSITE);

  // ignore UIV for function call from stl clang front end for now
  if (OPERATOR_is_call(stmt->Opr()) && OPERATOR_has_sym(stmt->Opr()) &&
    Vsa_check_regex_sym_ignore(ST_name(stmt->St())))
  return;

  if (which_arg > rna->Arg_cnt())
    return; // possible if callsite doesn't pass enough parameters

  CODEREP *x = rna->Get_arg(which_arg);
  Is_True(x != NULL, ("not find the actual, wrong caller?"));
#if 0  // handles UIV by vsym Shin 2019-07-21
  VSA *vsa = Comp_unit()->Vsa();
  HEAP_OBJ_REP *hor = vsa->Cr_2_heap_obj(x);
  if (hor && tracker) {
    VSYM_FLD_REP *vfr = tracker->Fld_rep();
    VSYM_OBJ_REP *vor = vsa->Find_hor_mu_vor(stmt, hor, vfr, x);

    if (vor == NULL) {
      VSYM_FLD_REP zero_fld(FLD_K_ID, 0, 0);
      vor = vsa->Find_hor_mu_vor(stmt, hor, &zero_fld, x);
    }
    if (vor != NULL) {
      vor = tracker->Compress(vsa, hor, vfr, vor, stmt, x);
    }
  }
#endif
  UINT32   arg_flags = rna->Get_arg_flags(which_arg);
#if 0
  // report error for callee's *p, this *p is UIV if on the RHS
  Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
           (TFile, "Check_caller_arg_val_attr:: Caller (%s) var attr evaluated  >>", ST_name(St())));
  Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG), x->Print(0, TFile));
#endif

  if ((kind & FOR_DIVISOR) != 0) {
    // uncomment this once the flag propagation is reliable
    //if ((arg_flags & ARG_VALUE_IVAD) == ARG_VALUE_IVAD ||
    //    (arg_flags & ARG_VALUE_MDANG) == ARG_VALUE_MDANG) {
    // report DBZ error

    // adjust CR for LDA if it's VSYM_BASE
    if ((kind & FOR_VSYM_BASE) != 0 &&
        x->Kind() == CK_LDA) {
      Is_True(stmt->Chi_list() != NULL,
              ("No chi list for call stmt"));
      MU_NODE* mu = stmt->Mu_list()->Search_mu_node(x->Lda_aux_id());
      if (mu == NULL) {
        Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
                 (TFile, "Check_caller_arg_val_attr: not find mu for LDA sym%d cr%d in stmt ",
                         x->Lda_aux_id(), x->Coderep_id()));
        Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
                     stmt->Print(TFile));
        return;
      }
      Is_True(mu != NULL,
              ("No chi for LDA parameter"));
      x = mu->OPND();
      kind = (ILODSTORBASE)(kind & (~FOR_VSYM_BASE));  // remove VSYM_BASE flag
    }
    hash_set<IDTYPE> visited_bb;
    Comp_unit()->Vsa()->Classify_vul_error(x, stmt->Bb(), stmt,
                                           cs, sp_h, kind, visited_bb);
    //}
    return;
  }

  if ((arg_flags & ARG_VALUE_UDEF) == ARG_VALUE_UDEF) {
    CODEREP* root_x = sp_h->Root_x();
    sp_h->Set_msgid("UIV.1");
    if (root_x->Kind() == CK_VAR) {
      CONTEXT_SWITCH context(sp_h->Context());
      initializer_cxt->Comp_unit()->Vsa()->Report_vsa_error(
        root_x, root_x->Aux_id(), UIV, IC_DEFINITELY, sp_h);
    }
    else if (x->Kind() == CK_VAR) {
      Comp_unit()->Vsa()->Report_vsa_error(
        root_x, x->Aux_id(), UIV, IC_DEFINITELY, sp_h);
    }
  }
  else if(VSA_Rvsa && (arg_flags & ARG_VALUE_MAYD) == 0 ) {
    CODEREP* root_x = sp_h->Root_x();
    sp_h->Set_msgid("UIV.1");
    if (root_x->Kind() == CK_VAR) {
      CONTEXT_SWITCH context(sp_h->Context());
      initializer_cxt->Comp_unit()->Vsa()->Report_vsa_error(
        root_x, root_x->Aux_id(), UIV, IC_MAYBE, sp_h);
    }
    else if (x->Kind() == CK_VAR) {
      Comp_unit()->Vsa()->Report_vsa_error(
        root_x, x->Aux_id(), UIV, IC_MAYBE, sp_h);
    }
  }
  // the following section should be removed once the new NPD is finalized
  if ((kind & (FOR_ILOD_BASE | FOR_ISTOR_BASE)) != 0 &&
      (kind & FOR_NPD) != 0 ) {
    // uncomment this once the flag progation is reliable
    //if ((arg_flags & ARG_VALUE_IVAD) == ARG_VALUE_IVAD ||
    //    (arg_flags & ARG_VALUE_MDANG) == ARG_VALUE_MDANG) {
    // report error for NPD
    // if(!Comp_unit()->Vsa()->Visit_counter_is_set()) {

    // Adjust CR for LDA if it's VSYM_BASE
    if ((kind & FOR_VSYM_BASE) != 0 &&
        x->Kind() == CK_LDA) {
      Is_True(stmt->Chi_list() != NULL,
              ("No chi list for call stmt"));
      MU_NODE* mu = stmt->Mu_list()->Search_mu_node(x->Lda_aux_id());
      if (mu == NULL)
        return;
      Is_True(mu != NULL,
              ("No chi for LDA parameter"));
      x = mu->OPND();
      kind = (ILODSTORBASE)(kind & (~FOR_VSYM_BASE));  // remove VSYM_BASE flag
    }

    hash_set<IDTYPE> visited_bb;
    Comp_unit()->Vsa()->Classify_vul_error(x, stmt->Bb(), stmt,
                                           cs, sp_h, kind, visited_bb);

      // }
    //}
    return;
  }

  // return if x not entry_chi, or recurse up call chain, fork!!!
  if (x->Kind() == CK_VAR &&
      (arg_flags & ARG_VALUE_MAYD) == ARG_VALUE_MAYD) {
    IDTYPE which_arg = Is_param(x); // WORK IN PROGRESS
    if (which_arg != INVALID_VAR_IDX && this->Is_root() && kind == FOR_ILOD_REF) {
      if ( !VSA_Param_Uiv )  // if -VSA:param_uiv=off
        return;
      if ( !Report_parm_uiv()) // if RBC rule did not set DNA_RT_PARM_UIV for this DNA
        return;

      sp_h->Set_msgid("UIV.1");
      {
        CONTEXT_SWITCH context(sp_h->Context());
        CODEREP *root_x = Find_base_pointer_load(sp_h->Root_x());
        if (root_x->Kind() == CK_VAR) {
          initializer_cxt->Comp_unit()->Vsa()->Report_vsa_error(
            sp_h->Root_x(), root_x->Aux_id(), UIV, IC_MAYBE, sp_h);
          return;
        }
      }
      if (x->Kind() == CK_VAR) {
        Comp_unit()->Vsa()->Report_vsa_error(
          sp_h->Root_x(), x->Aux_id(), UIV, IC_MAYBE, sp_h);
      }
    }
    else {
      return Check_callers_argument_for_uiv(ipsa, x, cs, sp_h, kind);
    }
  }

  if (kind == FOR_ILOD_REF &&
      ((arg_flags & LDA_VAL_REFED) == LDA_VAL_REFED) &&
      ((arg_flags & ARG_LDA_PT_UDEF) == ARG_LDA_PT_UDEF)) {
    // The caller passed down an unitialized memory, the call of this function
    // need this information to do additional checking via the refined vsym
    // model, which will analyze rna->Callee() context if the mu of the
    // callsite indeed reference to the vsym that is defined at the function
    // entry
    sp_h->Set_msgid("UIV.1");
    {
      CONTEXT_SWITCH context(sp_h->Context());
      CODEREP *root_x = Find_base_pointer_load(sp_h->Root_x());
      if (root_x->Kind() == CK_VAR) {
        initializer_cxt->Comp_unit()->Vsa()->Report_vsa_error(
          sp_h->Root_x(), root_x->Aux_id(), UIV, IC_DEFINITELY, sp_h);
        return;
      }
    }
    if (x->Kind() == CK_VAR) {
      Comp_unit()->Vsa()->Report_vsa_error(
        sp_h->Root_x(), x->Aux_id(), UIV, IC_DEFINITELY, sp_h);
    }
  }
  // check uninitialized vsym passed through the call
  Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
           (TFile, "Check_caller_arg_val_attr:: Caller pass uninit vsym check \n"));
  
}


// =============================================================================
//
// DNA_NODE::Is_param, given a CODEREP node, find the sequence number of the
//           formal parameter within the _parm_list.  Retrun INVALID_VAR_IDX
//           if it is not on the _parm_list.
//
// =============================================================================
IDTYPE
DNA_NODE::Is_param(CODEREP *x) const
{
  Is_True(x->Kind() == CK_VAR, ("DNA_NODE::Is_param must take CK_VAR as input"));
  AUX_STAB_ENTRY *sym = Comp_unit()->Opt_stab()->Aux_stab_entry(x->Aux_id());
  ST *st  = sym->St();
  if (!st)
    return INVALID_VAR_IDX;
  BOOL is_preg = (ST_sclass(st) == SCLASS_REG);
  for (INT i = VAR_INIT_ID; i < _parm_list.size(); ++i) {
    if (is_preg && _parm_list[i]->Ipreg() == x)
      return (IDTYPE) i;
    if (_parm_list[i]->St_idx() == st->st_idx)
      return (IDTYPE) i;      // match symbol idx
  }
  return INVALID_VAR_IDX;
}

// =============================================================================
//
// DNA_NODE::Is_global, given a CODEREP node, find the sequence number of the
//           within the _glob_list.  Retrun INVALID_VAR_IDX if it is not there
//
// =============================================================================
IDTYPE
DNA_NODE::Is_global(CODEREP *x) const
{
  Is_True(x->Kind() == CK_VAR, ("DNA_NODE::Is_global must take CK_VAR as input"));
  AUX_STAB_ENTRY *sym = Comp_unit()->Opt_stab()->Aux_stab_entry(x->Aux_id());
  ST *st  = sym->St();
  if (!st || ST_sclass(st) == SCLASS_REG)
    return INVALID_VAR_IDX;

  for (INT i = VAR_INIT_ID; i < _glob_list.size(); ++i) {
    if (_glob_list[i]->St_idx() == st->st_idx)
      return (IDTYPE) i;      // match symbol idx
  }
  return INVALID_VAR_IDX;
}


// =============================================================================
//
// DNA_NODE::Check_coderep_attr, checks attribute of the coderep which
//      supposedly to be the return value so far and report the error
//      following U-D according to ILODSTORBASE kind.
//
// =============================================================================
void
DNA_NODE::Check_coderep_attr(IPSA *ipsa, CODEREP *x, STMTREP *stmt,
                             CALL_STACK& cs, SRCPOS_HANDLE *sp_h, ILODSTORBASE kind)
{
  if ((kind & FOR_DIVISOR) != 0 ||
      (kind & (FOR_ILOD_BASE | FOR_ISTOR_BASE)) != 0) {
    hash_set<IDTYPE> visited_bb;
    Comp_unit()->Vsa()->Classify_vul_error(x, stmt->Bb(), stmt,
                                           cs, sp_h, kind, visited_bb);
  }
}

// =============================================================================
// DNA_NODE::Check_global_val_for_uiv, checks if global variable attr and report
//      error accroding to kind
// =============================================================================
void
DNA_NODE::Check_global_val_attr(IPSA *ipsa, UINT32 file_idx, ST_IDX st_idx, RNA_NODE* rna,
                                CALL_STACK& cs, SRCPOS_HANDLE *sp_h, ILODSTORBASE kind)
{
  // switch into caller's context
  CONTEXT_SWITCH context(this);
  STMTREP* stmt = rna->Callstmt();
  sp_h->Append_data(stmt, this, PATHINFO_DNA_CALLSITE);

  CHI_NODE* cnode;
  CHI_LIST_ITER chi_iter;
  WHIRL_FILE_MANAGER* mgr = WHIRL_FILE_MANAGER::Get();
  BOOL found_chi = FALSE;
  FOR_ALL_NODE(cnode, chi_iter, Init(stmt->Chi_list())) {
    if (!cnode->Live())
      continue;
    if (!Is_aux_global(cnode->Aux_id()))
      continue;

    AUX_STAB_ENTRY* chi_aux = Comp_unit()->Opt_stab()->Aux_stab_entry(cnode->Aux_id());
    UINT32 clr_file = File_idx();
    UINT32 clr_st = ST_st_idx(chi_aux->St());
    if (clr_file != 0 &&
        ST_sclass(chi_aux->St()) == SCLASS_EXTERN) {
      Is_True(mgr != NULL, ("not in xfa mode"));
      mgr->Resolve(clr_file, clr_st, clr_file, clr_st);
    }

    if (clr_file == file_idx && st_idx == clr_st) {
      if (found_chi == FALSE)
        found_chi = TRUE;
      Check_coderep_attr(ipsa, cnode->OPND(), stmt, cs, sp_h, kind);
    }
  }

  // global var is not referenced in this function
  if (found_chi == FALSE) {
    Check_callers_global_for_uiv(ipsa, file_idx, st_idx, cs, sp_h, kind);
  }
}

// =============================================================================
//
// Check_callers_argument_for_uiv, iterate through current function's
//      caller and examine the matching argument and it's initialization state.
//      It report error with the appropriate path across procedure boundary.
//      Although the caller will likely report UIV error for the argument list,
//      this function will tell the developer more precise information how that
//      unitialized variable get referenced in the callee.
//
// =============================================================================
void
DNA_NODE::Check_callers_argument_for_uiv(IPSA *ipsa, CODEREP *x, CALL_STACK& cs,
                                         SRCPOS_HANDLE *sp_h, ILODSTORBASE kind,
                                         VSYM_TRACKER *tracker)
{
  if (x->Kind() != CK_VAR) return;
  IDTYPE which_arg = Is_param(x);
  if (which_arg == INVALID_VAR_IDX) return;

#if 0
  Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
           (TFile, "Check_callers_argument_for_uiv:: ( %s )for variable >>>", ST_name(St())));
  Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG), x->Print(0, TFile));
#endif

  if (_clby_list.size() > VAR_INIT_ID) {
    RNA_NODE* call_site;
    SRCPOS_TREENODE *cur_treenode;
    if (cs.empty()) {
      call_site = NULL;
      // fork the srcpos_h before traverse the caller chain
      sp_h->Add_children(_clby_list.size() - 1);
      cur_treenode = sp_h->Cur_node();
    }
    else {
      call_site = cs.top();
      cs.pop();
    }

    sp_h->Path()->Push_mark(Comp_unit());

    for (INT i = VAR_INIT_ID; i < _clby_list.size(); ++i) {
      RNA_NODE *rna = _clby_list[i];

      // skip callsites which isn't previous call site
      if (call_site != NULL && call_site != rna)
        continue;

      DNA_NODE *caller = ipsa->Get_dna(rna->Caller_idx());
      if (caller == NULL || caller->Non_functional())
        continue;

       // skip the rna if it has already visited
      if (ipsa->Traved(rna))
        continue;
      ipsa->Set_trav(rna);

      // set current srcpos_h current node
      if (call_site == NULL)
        sp_h->Set_cur_node(cur_treenode, i-1);

      sp_h->Path()->Push_mark(caller->Comp_unit());
      sp_h->Path()->Add_bb(rna->Callstmt()->Bb());
      cs.push(rna);
      INT size = cs.size();
      // check if the argument is initialized by the caller appropriately
      // initiate the argument value attribute probing
      if (tracker) {
        VSYM_TRACKER n_tracker(*tracker); // could the copy constructor copy the stack?
        caller->Check_caller_arg_val_attr(ipsa, which_arg, rna, cs, sp_h, kind, &n_tracker);
      }
      else {
        caller->Check_caller_arg_val_attr(ipsa, which_arg, rna, cs, sp_h, kind);
      }
      Is_True(cs.size() == size && cs.top() == rna, ("call stack corrupted"));
      cs.pop();

      sp_h->Path()->Pop_mark(caller->Comp_unit(), TRUE);
      if (call_site != NULL)
        break;
    }

    sp_h->Path()->Pop_mark(Comp_unit(), TRUE);

    if (call_site != NULL)
      cs.push(call_site);
  }
}

// =============================================================================
//
// Check_callers_global_for_uiv, iterate through current function's
//      caller and examine the matching globals and it's initialization state.
//      It report error with the appropriate path across procedure boundary.
// =============================================================================
void
DNA_NODE::Check_callers_global_for_uiv(IPSA *ipsa, UINT32 file_idx, ST_IDX st,
                                       CALL_STACK& cs, SRCPOS_HANDLE *sp_h, ILODSTORBASE kind)
{
  // still in callee's context
  WHIRL_FILE_MANAGER* mgr = WHIRL_FILE_MANAGER::Get();

  if (_clby_list.size() > VAR_INIT_ID) {
    RNA_NODE* call_site;
    SRCPOS_TREENODE *cur_treenode;
    if (cs.empty()) {
      call_site = NULL;
      // fork the srcpos_h before traverse the caller chain
      sp_h->Add_children(_clby_list.size() - 1);
      cur_treenode = sp_h->Cur_node();
    }
    else {
      call_site = cs.top();
      cs.pop();
    }

    sp_h->Path()->Push_mark(Comp_unit());

    for (INT i = VAR_INIT_ID; i < _clby_list.size(); ++i) {
      RNA_NODE *rna = _clby_list[i];

      // skip callsites which isn't previous call site
      if (call_site != NULL && call_site != rna)
        continue;

      DNA_NODE *caller = ipsa->Get_dna(rna->Caller_idx());
      if (caller == NULL || caller->Non_functional())
        continue;

      // skip the rna if it has already visited
      if (ipsa->Traved(rna))
        continue;
      ipsa->Set_trav(rna);

      // set current srcpos_h current node
      if (call_site == NULL)
        sp_h->Set_cur_node(cur_treenode, i-1);

      sp_h->Path()->Push_mark(caller->Comp_unit());
      sp_h->Path()->Add_bb(rna->Callstmt()->Bb());
      cs.push(rna);
      INT size = cs.size();
      // check if the globals is initialized by the caller appropriately
      caller->Check_global_val_attr(ipsa, file_idx, st, rna, cs, sp_h, kind);
      Is_True(cs.size() == size && cs.top() == rna, ("call stack corrupted"));
      cs.pop();
      sp_h->Path()->Pop_mark(caller->Comp_unit(), TRUE);

      if (call_site != NULL)
        break;
    }

    sp_h->Path()->Pop_mark(Comp_unit(), TRUE);

    if (call_site != NULL)
      cs.push(call_site);
  }
}

// =============================================================================
//
// Check_istore_for_uiv
//      Check if lda parameter is istored on all path to return statement
// =============================================================================
void
DNA_NODE::Check_istore_for_uiv(IPSA *ipsa, BB_NODE *curbb, IDTYPE arg, CODEREP *chi_opnd, CALL_STACK &cs,
                               SRCPOS_HANDLE *sp_h, ILODSTORBASE kind, hash_set<IDTYPE> &visited)
{
  Is_True(!cs.empty(), ("no call stack found"));

  CODEREP *cr = Get_param_cr(arg);
  Is_True_Ret(cr != NULL, ("not find the cr"));
  VSA *vsa = Comp_unit()->Vsa();
  STMTREP *entry_chi = vsa->Get_entry_chi_stmt();
  Is_True_Ret(entry_chi != NULL, ("not find entry chi"));
  VSYM_FLD_REP fld(FLD_K_ID, 0, 0); // fld id may not zero?
  VSYM_OBJ_REP *vor = vsa->Find_vor_chi_vor(entry_chi, cr, &fld);
  if (vor == NULL)
    return;
  HEAP_OBJ_REP *hor = vor->Vsym_obj()->Base_hor();
  Is_True_Ret(hor != NULL, ("not find base hor"));

  RNA_NODE *rna = cs.top();
  cs.pop();  // pop the current rna when we go back
  INT size = cs.size();
  STMTREP *call_stmt = rna->Callstmt();
  DNA_NODE *caller_dna = ipsa->Get_dna(rna->Caller_idx());
  VSA *caller_vsa = caller_dna->Comp_unit()->Vsa();
  CODEREP *ret1 = caller_dna->Comp_unit()->Find_return_value(call_stmt);
  CODEREP *ret2 = NULL;
  VRA *caller_vra = caller_dna->Comp_unit()->Vra();
  if (ret1 != NULL) {
    STMTREP *next = call_stmt->Next();
    if (next->Opr() == OPR_STID &&
        (next->Rhs() == ret1 ||
         (next->Rhs()->Kind() == CK_OP &&
          (next->Rhs()->Opr() == OPR_CVT || next->Rhs()->Opr() == OPR_CVTL) &&
          next->Rhs()->Opnd(0) == ret1))) {
      ret2 = next->Lhs();
    }
  }

  SRCPOS_TREENODE* cur_node = sp_h->Add_children(Rets_list()->size());
  sp_h->Path()->Push_mark(Comp_unit());

  INT i = 0;
  VRA *vra = Comp_unit()->Vra();
  PATH_SELECTED paths;
  STMTR_VECTOR::const_iterator it;
  for (it = Rets_list()->begin(); it != Rets_list()->end(); ++it) {
    STMTREP* stmt = *it;
    if (stmt->Bb()->Unreachable())
      continue;
    if (vra->Var_cmp_val<OPR_EQ>(cr, stmt->Bb()->Id(), (INT64)0, paths) == VA_YES)
      continue;
    VSYM_OBJ_REP *rvor = vsa->Find_hor_mu_vor(stmt, hor, &fld, NULL);
    if (rvor == NULL || rvor != vor)
      continue;
    // check return value matches with caller's control dependency
    CODEREP *val = NULL;
    if (ret1 != NULL) {
      STMTREP *prev = stmt->Prev();
      if (prev && OPERATOR_is_scalar_store(prev->Opr()) &&
          Comp_unit()->Opt_stab()->Aux_stab_entry(prev->Lhs()->Aux_id())->Is_dedicated_preg()) {
        CODEREP *rhs = prev->Rhs();
        IDTYPE param;
        if (rhs->Kind() == CK_CONST)
          val = rhs;
        else if (rhs->Kind() == CK_VAR &&
                 (param = Is_param(rhs)) != INVALID_VAR_IDX)
          val = rna->Get_arg(param);
        else
          sp_h->Set_flag(SRCPOS_FLAG_MAYBE);
      }
    }
    if (val != NULL) {
      VRA_RESULT res1 = VA_UNKNOWN, res2 = VA_UNKNOWN;
      if (ret2 != NULL &&
          (res2 = caller_vra->Is_path_possible(curbb, ret2, val)) == VA_NO)
        continue;
      if (ret1 != NULL &&
          (res1 = caller_vra->Is_path_possible(curbb, ret1, val)) == VA_NO)
        continue;
      if (res1 != VA_YES && res2 != VA_YES)
        sp_h->Set_flag(SRCPOS_FLAG_MAYBE);
    }

    sp_h->Set_cur_node(cur_node, i++);
    // there is no istore on arg if rvor == vor
    sp_h->Append_data(stmt, this, PATHINFO_DNA_CALLRETURN);
    sp_h->Path()->Add_bb(stmt->Bb());
    sp_h->Append_data(entry_chi, this, PATHINFO_PARM);
    sp_h->Path()->Add_bb(entry_chi->Bb());
    {
      CONTEXT_SWITCH context(caller_dna);
      sp_h->Path()->Push_mark(caller_dna->Comp_unit());
      sp_h->Path()->Add_bb(call_stmt->Bb());
      caller_vsa->Classify_uiv_error(chi_opnd, call_stmt->Bb(), cs,
                                     sp_h, kind, visited);
      sp_h->Path()->Pop_mark(caller_dna->Comp_unit(), TRUE);
    }
    sp_h->Path()->Pop_mark(Comp_unit(), FALSE);
  }
  sp_h->Path()->Pop_mark(Comp_unit(), TRUE);
  Is_True(cs.size() == size, ("call stack corrupted"));
  cs.push(rna);  // add rna back to maintain the call stack
}

// =============================================================================
//
// Check_callee_side_effects_for_xfa, iterate through callee's return values
//      and examine the matching return coderep.
//      It report error with the appropriate path across procedure boundary
//      according to ILODSTORBASE.
//
// =============================================================================
void
DNA_NODE::Check_callee_side_effects_for_xfa(IPSA *ipsa, CODEREP *x,
                                            CALL_STACK& cs, SRCPOS_HANDLE *sp_h, ILODSTORBASE kind)
{
  Is_True(!cs.empty(), ("no call stack found"));
  RNA_NODE* rna = cs.top();
  // 'this' is callee
  Is_True(rna->Has_callee(Dna_idx()),
          ("callee index mismatch"));
  Is_True(x->Kind() == CK_VAR,
          ("Only var is allowed"));

  // skip the rna if it has already visited
  if (ipsa->Traved(rna))
    return;
  ipsa->Set_trav(rna);

  std::vector<STMTREP*> stmts;
  stmts.reserve(_retv_list.size() + _rgvl_list.size() / 4);
  Collect_callee_return_value_for_xfa(ipsa, x, rna, stmts);
  Collect_callee_global_value_for_xfa(ipsa, x, rna, stmts);

  if (stmts.size() == 0)
    return; // not found? chi not clean-up after populate callee's side effect

  Is_True(stmts.size() > 0,
          ("no return value found at callsite"));

  sp_h->Add_children(stmts.size());
  SRCPOS_TREENODE *cur_node = sp_h->Cur_node();

  // in callee's context
  CONTEXT_SWITCH context(this);
  sp_h->Path()->Push_mark(Comp_unit());
  for (INT i = 0; i < stmts.size(); ++i) {
    STMTREP* stmt = stmts[i];
    sp_h->Set_cur_node(cur_node, i);
    sp_h->Append_data(stmt, this, PATHINFO_COPY);
    sp_h->Path()->Add_bb(stmt->Bb());
    CODEREP *param;
    if (stmt->Rhs()->Kind() == CK_CONST &&
        Is_set(DNA_CHECK_NOT_ZERO) &&
        (param = Branch_stmt_4_bb(stmt->Bb(), TRUE).second) != NULL) {
      // rhs is INTCONST and DNA is set witl flag DNA_CHECK_NOT_ZERO
      // continue in caller to check param
      Check_callers_argument_for_uiv(ipsa, param, cs, sp_h, kind);
    }
    else {
      Check_coderep_attr(ipsa, stmt->Rhs(), stmt, cs, sp_h, kind);
    }
    sp_h->Path()->Pop_mark(Comp_unit(), FALSE);
  }
  sp_h->Path()->Pop_mark(Comp_unit(), TRUE);
}

// =============================================================================
//
// Collect_callee_return_value_for_xfa, iterate through callee's return value
//      list and output parameters, add stmt to vector if it modifies coderep x
//
// =============================================================================
void
DNA_NODE::Collect_callee_return_value_for_xfa(IPSA *ipsa, CODEREP *x, RNA_NODE* rna,
                                              vector<STMTREP*>& stmts)
{
  // 'this' is callee
  Is_True(rna->Has_callee(Dna_idx()),
          ("callee index mismatch"));
  Is_True(x->Kind() == CK_VAR,
          ("Only var is allowed"));

  // search retv at first
  if (_retv_list.size() > 0) {
    // still in caller's context
    DNA_NODE* caller = ipsa->Get_dna(rna->Caller_idx());

    AUX_STAB_ENTRY* aux = caller->Comp_unit()->Opt_stab()->Aux_stab_entry(x->Aux_id());

    for (INT i = VAR_INIT_ID; i < _retv_list.size(); ++i) {
      PDV_NODE* pdv = _retv_list[i];
      if (pdv->Oparam() == 0) {
        if (aux->Is_return_preg() &&
            pdv->Stmt()->Lhs()->Offset() == x->Offset()) {
          Is_True(pdv->Stmt()->Opr() == OPR_STID,
                  ("Invalid return value opr"));
          stmts.push_back(pdv->Stmt());
        }
      }
      else {
        Is_True(pdv->Oparam() >= VAR_INIT_ID &&
                pdv->Oparam() <= Parm_list()->size(),
                ("Invalid pdv param index in formal"));
        Is_True(pdv->Oparam() <= rna->Arg_cnt(),
                ("Invalid pdv param index in actual"));
        CODEREP* arg = rna->Get_arg(pdv->Oparam());
        Is_True(arg != NULL,
                ("can not get actual for %d", pdv->Oparam()));
        if (arg != NULL &&
            arg->Kind() == CK_LDA &&
            arg->Lda_aux_id() == x->Aux_id()) {
           // ignore CALL because it's in another context
           if (OPERATOR_is_call(pdv->Stmt()->Opr()))
             continue;
           Is_True(pdv->Stmt()->Opr() == OPR_ISTORE || pdv->Stmt()->Opr() == OPR_MSTORE,
                   ("Invalid output value opr"));
           // TODO: Istr_base may be OP/IVAR/etc
           CODEREP* base = pdv->Stmt()->Lhs()->Istr_base();
           if (base->Kind() != CK_VAR)
             continue;
           Is_True(base->Aux_id() == _parm_list[pdv->Oparam()]->Cr()->Aux_id(),
                   ("Invalid output value lhs sym"));
           stmts.push_back(pdv->Stmt());
        }
      }
    }
  }
}

// =============================================================================
//
// Collect_callee_global_value_for_xfa, iterate through callee's return global
//      values list and add stmt to vector if it modifies coderep x
//
// =============================================================================
void
DNA_NODE::Collect_callee_global_value_for_xfa(IPSA *ipsa, CODEREP *x, RNA_NODE* rna,
                                              vector<STMTREP*>& stmts)
{
  // 'this' is callee
  Is_True(rna->Has_callee(Dna_idx()),
          ("callee index mismatch"));
  Is_True(x->Kind() == CK_VAR,
          ("Only var is allowed"));

  // still in caller's context
  DNA_NODE* caller = ipsa->Get_dna(rna->Caller_idx());

  if (_rgvl_list.size() > 0 && caller->Is_aux_global(x->Aux_id())) {

    AUX_STAB_ENTRY* aux = caller->Comp_unit()->Opt_stab()->Aux_stab_entry(x->Aux_id());
    UINT32 file_idx = caller->File_idx();
    UINT32 st_idx = ST_st_idx(aux->St());

    WHIRL_FILE_MANAGER* mgr = WHIRL_FILE_MANAGER::Get();
    if (file_idx != File_idx() &&
        ST_sclass(aux->St()) == SCLASS_EXTERN) {
      Is_True(mgr != NULL, ("not in xfa mode"));
      mgr->Resolve(file_idx, st_idx, file_idx, st_idx);
    }

    for (INT i = VAR_INIT_ID; i < _rgvl_list.size(); ++i) {

      PDV_NODE* pdv = _rgvl_list[i];
      if (!pdv->Is_value())  // ignore resource PDV_NODE
        continue;
      Is_True(pdv->Stmt() != NULL && pdv->Stmt()->Opr() == OPR_STID,
              ("invalid assignment to global"));
      CODEREP* lhs = pdv->Stmt()->Lhs();
      Is_True(Is_aux_global(lhs->Aux_id()),
              ("invalid aux in return global value list"));

      AUX_STAB_ENTRY* lhs_aux = Comp_unit()->Opt_stab()->Aux_stab_entry(lhs->Aux_id());
      UINT32 pdv_file = File_idx();
      UINT32 pdv_st = ST_st_idx(lhs_aux->St());

      if (caller->File_idx() != pdv_file &&
          ST_sclass(lhs_aux->St()) == SCLASS_EXTERN) {
        Is_True(mgr != NULL, ("not in xfa mode"));
        mgr->Resolve(pdv_file, pdv_st, pdv_file, pdv_st);
      }
      if (pdv_file == file_idx && pdv_st == st_idx)
        stmts.push_back(pdv->Stmt());
    }
  }
}

// =============================================================================
//
// Check_caller_argument_for_aob: match the argument in ONE caller and traverse
//      the U-D, including CK_VAR and CK_IVAR to report error with the
//      appropriate path across procedure boundary.
//
// =============================================================================
void
DNA_NODE::Check_caller_argument_for_aob(IPSA *ipsa, IDTYPE arg, RNA_NODE* rna,
                                        CALL_STACK& cs, SRCPOS_HANDLE *sp_h,
                                        VSA_ADDRESS_INFO *info,
                                        VSYM_TRACKER *tracker)
{
  // switch into caller's context
  CONTEXT_SWITCH context(this);
  STMTREP* stmt = rna->Callstmt();
  sp_h->Append_data(stmt, this, PATHINFO_DNA_CALLSITE);

  if (arg > rna->Arg_cnt())
    return;

  CODEREP* cr = rna->Get_arg(arg);
  Is_True(cr != NULL, ("bad argument"));
  VSA* vsa = Comp_unit()->Vsa();

  if (cr->Kind() == CK_LDA) {
    Is_Trace(Tracing(), (TFile, "TODO: hit LDA. check LDA st directly.\n"));
    if (tracker != NULL) {
      if (tracker->Size() > 1)  // TODO: more than 1 level ILOAD with LDA
        return;
      MU_NODE* mu = vsa->Find_stmt_var_mu(stmt, cr->Lda_base_st(), tracker->Fld_rep());
      if (mu == NULL)
        return;
      cr = mu->OPND();
    }
    std::vector<bool> visited;
    visited.resize(Comp_unit()->Cfg()->Total_bb_count());
    Comp_unit()->Vsa()->Classify_aob_error(cr, stmt->Bb(), stmt,
                                           cs, sp_h, info, visited);
    return;
  }
  HEAP_OBJ_REP* hor = vsa->Cr_2_heap_obj(cr);
  if (hor == NULL || tracker == NULL) {
    // follow the var U-D
    Is_Trace(Tracing(), (TFile, "no heap obj found for cr%d, switch back to VAR-UD\n", cr->Coderep_id()));
    std::vector<bool> visited;
    visited.resize(Comp_unit()->Cfg()->Total_bb_count());
    Comp_unit()->Vsa()->Classify_aob_error(cr, stmt->Bb(), stmt,
                                           cs, sp_h, info, visited, tracker);
    return;
  }
  if (! VSA_Model_Lda()) return;
  // first try, match the field id
  VSYM_FLD_REP* vfr = tracker->Fld_rep();
  VSYM_OBJ_REP* vor = vsa->Find_hor_mu_vor(stmt, hor, vfr, cr);
  Is_True(vor == NULL || vor->Vsym_obj()->Fld_rep().Match(vfr), ("fld mismatch"));
  // second try, match the field id 0
  if (vor == NULL) {
    VSYM_FLD_REP zero_fld(FLD_K_ID, 0, 0);
    vor = vsa->Find_hor_mu_vor(stmt, hor, &zero_fld, cr);
  }

  if (vor == NULL) {
    // follow the var U-D
    Is_Trace(Tracing(), (TFile, "no vsym obj found for cr%d with vfr: ", cr->Coderep_id()));
    Is_Trace_cmd(Tracing(), vfr->Print(vsa->Ipsa(), TFile));
    hor = vsa->Cr_2_heap_obj(cr);
    if (hor != NULL)
      hor = vsa->Find_stmt_cur_hor(stmt, hor->Heap_obj()); 
    std::vector<bool> visited;
    visited.resize(Comp_unit()->Cfg()->Total_bb_count());
    Comp_unit()->Vsa()->Classify_aob_error(cr, stmt->Bb(), stmt,
                                           cs, sp_h, info, visited, tracker);
  }
  else {
    vor = tracker->Compress_old(vsa, hor, vfr, vor, stmt, cr);

    // The VSYM resolution connect up the caller's LDA node as defined in cr
    // Should the defstmt of the vor be picked up in Classify_aob_error?
    STMTREP *def = (vor->Attr() != ROR_DEF_BY_NONE &&
                    vor->Attr() != ROR_DEF_BY_PHI &&
                    vor->Attr() != ROR_DEF_BY_HORPHI &&
                    vor->Attr() != ROR_DEF_BY_IPARM) ? vor->Stmt_def():NULL;

    std::vector<bool> visited;
    visited.resize(Comp_unit()->Cfg()->Total_bb_count());
    if (def && (def->Opr() != OPR_OPT_CHI)) {
      if (vor->Attr() == ROR_DEF_BY_ISTORE)
        tracker->Pop();
      if (tracker->Empty())
        tracker = NULL;
      sp_h->Append_stpath(def, def->Rhs(), Comp_unit()->Dna(), FALSE);
      Comp_unit()->Vsa()->Classify_aob_error(def->Rhs(), def->Bb(), def,
                                             cs, sp_h, info, visited, tracker);
    }
    else
      Comp_unit()->Vsa()->Classify_aob_error(cr, stmt->Bb(), stmt,
                                             cs, sp_h, info, visited, tracker);
  }
}

// =============================================================================
//
// Check_caller_global_for_aob: match the global in ONW caller and traverse
//      the U-D, including CK_VAR and CK_IVAR to report error with the
//      appropriate path across procedure boundary.
//
// =============================================================================
void
DNA_NODE::Check_caller_global_for_aob(IPSA *ipsa, UINT32 file_idx, ST_IDX st_idx,
                                      INT32 offset, RNA_NODE* rna, CALL_STACK& cs,
                                      SRCPOS_HANDLE *sp_h, VSA_ADDRESS_INFO *info,
                                      VSYM_TRACKER *Tracker)
{
  // switch into caller's context
  CONTEXT_SWITCH context(this);
  STMTREP* stmt = rna->Callstmt();
  sp_h->Append_data(stmt, this, PATHINFO_DNA_CALLSITE);

  CHI_NODE* cnode;
  CHI_LIST_ITER chi_iter;
  WHIRL_FILE_MANAGER* mgr = WHIRL_FILE_MANAGER::Get();
  FOR_ALL_NODE(cnode, chi_iter, Init(stmt->Chi_list())) {
    if (!cnode->Live())
      continue;
    if (!Is_aux_global(cnode->Aux_id()))
      continue;

    AUX_STAB_ENTRY* chi_aux =
      Comp_unit()->Opt_stab()->Aux_stab_entry(cnode->Aux_id());
    UINT32 clr_file = File_idx();
    UINT32 clr_st = ST_st_idx(chi_aux->St());

    if (file_idx != 0 &&
        ST_sclass(chi_aux->St()) == SCLASS_EXTERN) {
      Is_True(mgr != NULL, ("not in xfa mode"));
      mgr->Resolve(clr_file, clr_st, clr_file, clr_st);
    }

    if (clr_file == file_idx &&
        st_idx == clr_st &&
        cnode->OPND()->Offset() == offset) {
      std::vector<bool> visited;
      visited.resize(Comp_unit()->Cfg()->Total_bb_count());
      Comp_unit()->Vsa()->Classify_aob_error(cnode->OPND(), stmt->Bb(), stmt,
                                             cs, sp_h, info, visited);
      break;
    }
  }
}

// =============================================================================
//
// Check_callers_argument_for_aob: iterate through current function's
//      caller and examine the matching argument.  The caller seeks VSYM
//      resolution if it pass down non-NULL 'tracker' parameter.
//      It report error with the appropriate path across procedure boundary.
//
// =============================================================================
void
DNA_NODE::Check_callers_argument_for_aob(IPSA *ipsa, CODEREP *x,
                                         CALL_STACK& cs, SRCPOS_HANDLE *sp_h,
                                         VSA_ADDRESS_INFO *info,
                                         VSYM_TRACKER *tracker)
{
  if (x->Kind() != CK_VAR) return;
  IDTYPE which_arg = Is_param(x);
  if (which_arg == INVALID_VAR_IDX)
    return;

  if (_clby_list.size() > VAR_INIT_ID) {
    RNA_NODE *call_site;
    SRCPOS_TREENODE *cur_treenode;
    if (cs.empty()) {
      call_site = NULL;
      // fork the srcpos_h before traverse the caller chain
      sp_h->Add_children(_clby_list.size() - 1);
      cur_treenode = sp_h->Cur_node();
    }
    else {
      call_site = cs.top();
      cs.pop();
    }

    sp_h->Path()->Push_mark(Comp_unit());

    for (INT i = VAR_INIT_ID; i < _clby_list.size(); ++i) {
      RNA_NODE *rna = _clby_list[i];

      // skip callsites which isn't previous call site
      if (call_site != NULL && call_site != rna)
        continue;

      DNA_NODE *caller = ipsa->Get_dna(rna->Caller_idx());
      if (caller == NULL || caller->Non_functional())
        continue;

      // skip the rna if it has already visited
      if (ipsa->Traved(rna))
        continue;
      ipsa->Set_trav(rna);

      // set current srcpos_h current node
      if (call_site == NULL)
        sp_h->Set_cur_node(cur_treenode, i-1);

      sp_h->Path()->Push_mark(caller->Comp_unit());
      sp_h->Path()->Add_bb(rna->Callstmt()->Bb());
      cs.push(rna);
      INT size = cs.size();
      VSA_ADDRESS_INFO n_info(info);
      if (tracker) {
        VSYM_TRACKER n_tracker(*tracker); // could the copy constructor copy the stack?
        caller->Check_caller_argument_for_aob(ipsa, which_arg, rna, cs,
                                              sp_h, &n_info, &n_tracker);
      }
      else
        caller->Check_caller_argument_for_aob(ipsa, which_arg, rna, cs,
                                              sp_h, &n_info, NULL);
      Is_True(cs.size() == size && cs.top() == rna, ("call stack corrupted"));
      cs.pop();
      sp_h->Path()->Pop_mark(caller->Comp_unit(), TRUE);

      if (call_site != NULL)
        break;
    }

    sp_h->Path()->Pop_mark(Comp_unit(), TRUE);

    if (call_site != NULL)
      cs.push(call_site);
  }
}

// =============================================================================
// Check_callers_global_for_aob: iterate through current function's
//      caller and examine the matching global variables.
//      It report error with the appropriate path across procedure boundary.
// =============================================================================
void
DNA_NODE::Check_callers_global_for_aob(IPSA *ipsa, UINT32 file_idx, ST_IDX st,
                                       INT32 offset, CALL_STACK& cs,
                                       SRCPOS_HANDLE *sp_h,
                                       VSA_ADDRESS_INFO *info,
                                       VSYM_TRACKER *tracker)
{
  // still in callee's context
  WHIRL_FILE_MANAGER* mgr = WHIRL_FILE_MANAGER::Get();

  if (_clby_list.size() > VAR_INIT_ID) {
    RNA_NODE* call_site;
    SRCPOS_TREENODE *cur_treenode;
    if (cs.empty()) {
      call_site = NULL;
      // fork the srcpos_h before traverse the caller chain
      sp_h->Add_children(_clby_list.size() - 1);
      cur_treenode = sp_h->Cur_node();
    }
    else {
      call_site = cs.top();
      cs.pop();
    }

    sp_h->Path()->Push_mark(Comp_unit());

    for (INT i = VAR_INIT_ID; i < _clby_list.size(); ++i) {
      RNA_NODE *rna = _clby_list[i];

      // skip callsites which isn't previous call site
      if (call_site != NULL && call_site != rna)
        continue;

      DNA_NODE *caller = ipsa->Get_dna(rna->Caller_idx());
      if (caller == NULL || caller->Non_functional())
        continue;

      // skip the rna if it has already visited
      if (ipsa->Traved(rna))
        continue;
      ipsa->Set_trav(rna);

      // set current srcpos_h current node
      if (call_site == NULL)
        sp_h->Set_cur_node(cur_treenode, i-1);

      sp_h->Path()->Push_mark(caller->Comp_unit());
      sp_h->Path()->Add_bb(rna->Callstmt()->Bb());
      cs.push(rna);
      INT size = cs.size();
      VSA_ADDRESS_INFO n_info(info);
      if (tracker) {
        VSYM_TRACKER n_tracker(*tracker); // could the copy constructor copy the stack?
        caller->Check_caller_global_for_aob(ipsa, file_idx, st, offset, rna, cs, sp_h, &n_info, &n_tracker);
      }
      else
        caller->Check_caller_global_for_aob(ipsa, file_idx, st, offset, rna, cs, sp_h, &n_info, NULL);
      Is_True(cs.size() == size && cs.top() == rna, ("call stack corrupted"));
      cs.pop();
      sp_h->Path()->Pop_mark(caller->Comp_unit(), TRUE);

      if (call_site != NULL)
        break;
    }

    sp_h->Path()->Pop_mark(Comp_unit(), TRUE);

    if (call_site != NULL)
      cs.push(call_site);
  }
}

// =============================================================================
// Check_callee_side_effects_for_aob: iterate through callee's return values
//      and examine the matching return coderep.
//      It report error with the appropriate path across procedure boundary.
// =============================================================================
void
DNA_NODE::Check_callee_side_effects_for_aob(IPSA *ipsa, CODEREP *x, CALL_STACK& cs,
                                            SRCPOS_HANDLE *sp_h, VSA_ADDRESS_INFO *info, VSYM_TRACKER *tracker)
{
  Is_True(!cs.empty(), ("no call stack found"));
  RNA_NODE* rna = cs.top();
  // 'this' is callee
  Is_True(rna->Has_callee(Dna_idx()),
          ("callee index mismatch"));
  Is_True(x->Kind() == CK_VAR,
          ("Only var is allowed"));

  // skip the rna if it has already visited
  if (ipsa->Traved(rna))
    return;
  ipsa->Set_trav(rna);

  std::vector<STMTREP*> stmts;
  stmts.reserve(_retv_list.size() + _rgvl_list.size() / 4);
  Collect_callee_return_value_for_xfa(ipsa, x, rna, stmts);
  Collect_callee_global_value_for_xfa(ipsa, x, rna, stmts);

  if (stmts.size() == 0)
    return; // not found? chi not clean-up after populate callee's side effect

  Is_True(stmts.size() > 0,
          ("no return value found at callsite"));

  sp_h->Add_children(stmts.size());
  SRCPOS_TREENODE *cur_node = sp_h->Cur_node();

  // in callee's context
  CONTEXT_SWITCH context(this);
  sp_h->Path()->Push_mark(Comp_unit());

  for (INT i = 0; i < stmts.size(); ++i) {
    STMTREP* stmt = stmts[i];
    sp_h->Set_cur_node(cur_node, i);
    sp_h->Append_data(stmt, this, PATHINFO_COPY);
    VSA_ADDRESS_INFO n_info(info);
    std::vector<bool> visited;
    visited.resize(Comp_unit()->Cfg()->Total_bb_count());
    sp_h->Path()->Add_bb(stmt->Bb());
    if (tracker) {
      VSYM_TRACKER n_tracker(*tracker);
      Comp_unit()->Vsa()->Classify_aob_error(stmt->Rhs(), stmt->Bb(), stmt, cs,
                                             sp_h, &n_info, visited, &n_tracker);
    }
    else
      Comp_unit()->Vsa()->Classify_aob_error(stmt->Rhs(), stmt->Bb(), stmt, cs,
                                             sp_h, &n_info, visited, NULL);
    sp_h->Path()->Pop_mark(Comp_unit(), FALSE);
  }
  sp_h->Path()->Pop_mark(Comp_unit(), TRUE);
}

// =============================================================================
// Check_clinit_side_effects_for_aob: iterate through clinit method's return 
//      global values and examine the matching return coderep.
// =============================================================================
void
DNA_NODE::Check_clinit_side_effects_for_aob(IPSA *ipsa, UINT32 file_idx, ST_IDX st_idx,
                                            CALL_STACK& cs, SRCPOS_HANDLE *sp_h, VSA_ADDRESS_INFO *info)
{
  std::vector<STMTREP*> stmts;
  stmts.reserve(_rgvl_list.size());
  
  WHIRL_FILE_MANAGER* mgr = WHIRL_FILE_MANAGER::Get();
  for (INT i = VAR_INIT_ID; i < _rgvl_list.size(); ++i) {
    PDV_NODE* pdv = _rgvl_list[i];
    if (!pdv->Is_value())  // ignore resource PDV_NODE
      continue;
    Is_True(pdv->Stmt() != NULL && pdv->Stmt()->Opr() == OPR_STID,
            ("invalid assignment to global"));
    CODEREP* lhs = pdv->Stmt()->Lhs();
    Is_True(Is_aux_global(lhs->Aux_id()),
            ("invalid aux in return global value list"));

    AUX_STAB_ENTRY* lhs_aux = Comp_unit()->Opt_stab()->Aux_stab_entry(lhs->Aux_id());
    UINT32 pdv_file = File_idx();
    UINT32 pdv_st = ST_st_idx(lhs_aux->St());

    if (file_idx != pdv_file &&
        ST_sclass(lhs_aux->St()) == SCLASS_EXTERN) {
      Is_True(mgr != NULL, ("not in xfa mode"));
      mgr->Resolve(pdv_file, pdv_st, pdv_file, pdv_st);
    }
    if (pdv_file == file_idx && pdv_st == st_idx)
      stmts.push_back(pdv->Stmt());
  }

  if (stmts.size() == 0)
    return; // not found? chi not clean-up after populate callee's side effect

  Is_True(stmts.size() > 0,
          ("no return value found at callsite"));

  sp_h->Add_children(stmts.size());
  SRCPOS_TREENODE *cur_node = sp_h->Cur_node();

  // in callee's context
  CONTEXT_SWITCH context(this);
  sp_h->Path()->Push_mark(Comp_unit());

  for (INT i = 0; i < stmts.size(); ++i) {
    STMTREP* stmt = stmts[i];
    sp_h->Set_cur_node(cur_node, i);
    sp_h->Append_data(stmt, this, PATHINFO_COPY);
    VSA_ADDRESS_INFO n_info(info);
    std::vector<bool> visited;
    visited.resize(Comp_unit()->Cfg()->Total_bb_count());
    sp_h->Path()->Add_bb(stmt->Bb());
    Comp_unit()->Vsa()->Classify_aob_error(stmt->Rhs(), stmt->Bb(), stmt, cs,
                                           sp_h, &n_info, visited);
    sp_h->Path()->Pop_mark(Comp_unit(), FALSE);
  }
  sp_h->Path()->Pop_mark(Comp_unit(), TRUE);
}

// =============================================================================
// Is_path_possible_in_callee
//   check if path possible in callee by comparison return value with
//   comparison expression in callee
// =============================================================================
BOOL
DNA_NODE::Is_path_possible_in_callee(IPSA *ipsa, RNA_NODE* rna, IDTYPE param, CODEREP* cmp,
                                     CALL_STACK& cs, VSYM_TRACKER* tracker, const PATH_SELECTED& path)
{
  Is_True(param == 0, ("only return value handled"));
  Is_True(cs.top() == rna, ("call stack corrupted"));

  VSA* vsa = Comp_unit()->Vsa();
  CONTEXT_SWITCH ctx(this);

  for (INT i = VAR_INIT_ID; i < _retv_list.size(); ++i) {
    PDV_NODE* pdv = _retv_list[i];
    if (pdv->Oparam() == 0 &&
        pdv->Stmt() != NULL &&
        pdv->Stmt()->Opr() == OPR_STID) {
      if (vsa->Is_path_possible(pdv->Stmt()->Rhs(), pdv->Stmt()->Bb(), cmp, cs, tracker, path))
        return TRUE;
    }
  }
  return FALSE;
}

// =============================================================================
//
// Update_parm_flags
//   update parameter flags according to all actual parameter in
//   RNA node in _clby_list.
//
//   NOTE: This function is not appropriate to call if there are more than one
//         elements in clby_list in this function.  This call will ruin the
//         context sensitive objective of xvsa.  -Shin 6/18/2019
//
// =============================================================================
BOOL
DNA_NODE::Update_parm_flags(IPSA* ipsa)
{
  if (Clby_list()->size() > (VAR_INIT_ID + 1)) return FALSE;
  BOOL changed = FALSE;
  INT i, j;
  for (i = VAR_INIT_ID; i < _parm_list.size(); ++i) {
    UINT32 old_flag = Parm_flags(i);
    UINT32 new_flag = old_flag;
    for (j = VAR_INIT_ID; j < _clby_list.size(); ++j) {
      RNA_NODE *rna = _clby_list[j];
      if (i > rna->Arg_cnt())
        continue;
      new_flag = Set_parm_flag(i, rna->Get_arg_flags(i));
    }
    if (old_flag != new_flag && changed == FALSE)
      changed = TRUE;
  }
  return changed;
}

// =============================================================================
//
// Update_retv_flags
//   update return value flags according to all PDV_NODE in retv and update
//   RNA node in _clby_list
//
// =============================================================================
BOOL
DNA_NODE::Update_retv_flags(IPSA* ipsa)
{
  BOOL changed = FALSE;
  INT i, j;
  AUX_STAB_ENTRY* retv = NULL;
  for (i = PDV_INIT_ID; i < _retv_list.size(); ++i) {
    PDV_NODE* pdv = _retv_list[i];
    pdv->Update_flags(this);
  }

  for (i = VAR_INIT_ID; i < _clby_list.size(); ++i) {
    RNA_NODE *rna = _clby_list[i];
    DNA_NODE* caller = ipsa->Get_dna(rna->Caller_idx());
    CONTEXT_SWITCH context(caller);
    INT j;
    for (j = PDV_INIT_ID; j < _retv_list.size(); ++j) {
      PDV_NODE* pdv = _retv_list[j];
      BOOL ret = rna->Update_retv_flags(caller, pdv);
      if (changed == FALSE && ret == TRUE)
        changed = TRUE;
    }
  }
  return changed;
}

// =============================================================================
// Update_eh_types
//   Update caller's eh types according to callee's eh types
// =============================================================================
BOOL
DNA_NODE::Update_eh_types(IPSA* ipsa)
{
  EH_TABLE* eh_table = Comp_unit()->EH_table();
  Is_True(eh_table != NULL, ("EH table is NULL"));

  EH_TYPE_VECTOR* ty = eh_table->Type_list();
  if (ty->size() == 0)
    return FALSE;
  BOOL changed = FALSE;
  INT i;
  for (i = VAR_INIT_ID; i < _clby_list.size(); ++i) {
    RNA_NODE *rna = _clby_list[i];
    if (rna->Is_back_edge())
      continue;
    DNA_NODE* caller = ipsa->Get_dna(rna->Caller_idx());
    EH_TABLE* caller_eh = caller->Comp_unit()->EH_table();
    EH_TYPE_VECTOR::iterator iter;
    for (iter = ty->begin(); iter != ty->end(); ++iter) {
      BOOL ret = caller_eh->Add_eh_type(iter->first, iter->second);
      if (ret == TRUE && changed == FALSE)
        changed = TRUE;
    }
  }
  return changed;
}

// =============================================================================
//
// Create_arg_lists create argument list for the RNA node.  This routine depend
//   on the attributes are collected by VSA::propagate_vardef.  With attributes
//   propagated, we implicitely perform Top_down_propagation.  We takes care of
//   the following value n attributes
//   1. unique pointer parameters, includes LDA or malloced heap objects
//   2. escaped pointer parameters
//   3. tainted pointer parameters
//   4. specific value parameters such as zero value
//
// =============================================================================
void
DNA_NODE::Create_arg_lists(IPSA *ipsa)
{
  if (_call_list.size() > VAR_INIT_ID) {
    for (INT i = VAR_INIT_ID; i < _call_list.size(); ++i) {
      RNA_NODE *rna = _call_list[i];
      //if (callee == NULL) continue;  // API MODEL POC HACK, to remove Uniq_callee later

      STMTREP  *stmt = rna->Callstmt();
      rna->Collect_arg_list(stmt->Rhs(), ipsa);
      Is_True((*rna->Arg_list())[0] != NULL, ("_arg_list[0] for retval is NULL"));

      CALLEE_VECTOR::const_iterator callee_iter;
      // if there are more than one callee, find the callee that has the longgest parm list
      for (callee_iter = rna->Callee_list().begin(); callee_iter != rna->Callee_list().end(); ++callee_iter) {
        DNA_NODE *callee = ipsa->Get_dna(callee_iter->Callee());
        if (!callee || callee->Non_functional())
          continue;
        if (VSA_Fam) {
          BOOL match = TRUE;
          // direct call
          if (rna->Callstmt()->Opr() == OPR_CALL) {
            if (rna->Callee_st() && TY_is_varargs(ST_type(rna->Callee_st()))) {
              Is_True(rna->Arg_cnt() >= callee->Parm_list()->size() - 1,
                      ("vararg callsite param less than caller param"));
            } else if (rna->Arg_cnt() != callee->Parm_list()->size() - 1) {
              match = FALSE;
            }
          }
          // icall
          else if (rna->Callstmt()->Opr() == OPR_ICALL) {
            CONTEXT_SWITCH context(callee);
            STMTREP *call_sr = rna->Callstmt();
            // use callee's PU symbol to check varargs attribute
            if (callee->St() && TY_is_varargs(ST_type(callee->St()))) {
              Is_True(rna->Arg_cnt() >= callee->Parm_list()->size() - 1,
                      ("vararg callsite param less than caller param"));
            } else if (PU_java_lang(Get_Current_PU())) {
              CODEREP *last_parm_cr = call_sr->Rhs()->Opnd(call_sr->Rhs()->Kid_count() - 1);
              // the last cr is intrinsic
              if (last_parm_cr->Kind() == CK_OP && last_parm_cr->Opr() == OPR_INTRINSIC_OP) {
                // last parm intrinsic has 2 kids, we pushed all of them to arg list, but callee don't have those parm
                if (rna->Arg_cnt() - 2 != callee->Parm_list()->size() - 1) {
                  match = FALSE;
                }
              }
            }
            // for c and c++, it is possible that actual argcnt is 1 bigger for indirect call as function pointer
            // and it is also possible that actual argcnt is equal to the formal argcnt, TO look into this case later
            else if (PU_c_lang(Get_Current_PU()) || PU_cxx_lang(Get_Current_PU())) {
              INT actual_argcnt = rna->Arg_cnt();
              INT callee_argcnt = callee->Parm_list()->size() - 1;
              if (actual_argcnt != callee_argcnt &&
                  actual_argcnt != callee_argcnt + 1) {
                match = FALSE;
              }
            } else {
              Is_True(FALSE, ("Do not support check FAM for lang: ", Get_Current_PU().src_lang));
            }
          }
          if (!match) {
            SRCPOS_HANDLE srcpos_h(NULL, stmt, this, Comp_unit()->Loc_pool());
            char* fname = Vsa_demangle(callee->Fname());
#if 0
            srcpos_h.Add_message("parameter mismatch %s %s",
                                _call_list.size() > VAR_INIT_ID ? "if callee is" : "in calling",
                                fname ? fname : callee->Fname());
#endif
            srcpos_h.Set_msgid("FAM.1");
            if (rna->Callee_st()) {
              SRCPOS declare_loc = ST_Srcpos(*rna->Callee_st());
              if (declare_loc != 0 && declare_loc != stmt->Linenum())
                srcpos_h.Append_data(rna->Callee_st(), NULL, this, PATHINFO_ST_DECLARE);
            }
            if (callee->St()) {
              SRCPOS callee_loc = ST_Srcpos(*callee->St());
              srcpos_h.Append_data(callee->St(), NULL, callee, PATHINFO_ST_DECLARE);
            }
            Comp_unit()->Vsa()->Report_vsa_error(NULL, fname, FAM, IC_DEFINITELY, &srcpos_h);
            // ATTENTION: the memory was allocated in the Vsa_demangle, do not allocate memory by yourself
            if (fname)
              free(fname);
          }
        }
        INT arg_cnt = callee->Parm_list()->size() - 1 > rna->Arg_cnt() ? rna->Arg_cnt() : callee->Parm_list()->size() - 1;
        for (INT j = VAR_INIT_ID; j <= arg_cnt; ++j) {
          // merge the callee parm_flags into arg_flags for easier access from VSA
          UINT32 arg_flags = rna->Set_arg_flag(j, callee->Parm_flags(j)|ARG_REF_MERGED);
          if ((arg_flags & ARG_LDA) == ARG_LDA) {
            // check if the var is initilized, if LDA of such var is in the argument list
            ST_IDX        arg_stidx = rna->Get_arg_stidx(j);
            MU_LIST_ITER  mu_iter;
            MU_NODE      *mnode;
            FOR_ALL_NODE( mnode, mu_iter, Init(stmt->Mu_list())) {
              if (mnode->Aux_id() == arg_stidx) {
                CODEREP *x = mnode->OPND();
                // we only check un-initialized variable for UIV purpose
                if (x->Is_flag_set((CR_FLAG)(CF_DEF_BY_CHI)) &&
                    x->Def_at_entry() &&
                    ST_sclass(Comp_unit()->Opt_stab()->Aux_stab_entry(arg_stidx)->St()) == SCLASS_AUTO) {
                  // the above test is incomplete, comment out the next line for now
                  arg_flags = rna->Set_arg_flag(j, ARG_LDA_PT_UDEF);
                } // defined by entry_chi
              }
              // else if defined by PHI
            } // mu_iter
          } // check if the variable pointed to by LDA node is initialized
        } // merge flags for the argument list of the caller
      } // for each callee of this callsite RNA
    } // for each callsite of this DNA
  }
}

// =============================================================================
// Propagate_arg_and_ret_tags
//   propagate tags from callee's dna to rna
// =============================================================================
void
DNA_NODE::Propagate_arg_and_ret_tags()
{
  if (_clby_list.size() > VAR_INIT_ID) {
    for (INT i = VAR_INIT_ID; i < _clby_list.size(); ++i) {
      RNA_NODE *rna = _clby_list[i];
      INT len = rna->Arg_list()->size() < _parm_list.size() ? rna->Arg_list()->size() : _parm_list.size();
      for (INT j = VAR_INIT_ID; j < len; j++) {
        rna->Merge_arg_tag_node(j, _parm_list[j]->Get_tag_node());
      }
      rna->Merge_ret_tag_node(Get_ret_tag_node());
    }
  }
}

// =============================================================================
// Merge_flags_from_callee_to_rna
//   merge flags from rna's callee to rna, some rbc rule will set flag to DNA,
//   so we should merge flags after all dna's flags are setted
// =============================================================================
void
DNA_NODE::Merge_flags_from_callee_to_rna(IPSA *ipsa)
{
  if (_call_list.size() > VAR_INIT_ID) {
    for (INT i = VAR_INIT_ID; i < _call_list.size(); ++i) {
      RNA_NODE *rna = _call_list[i];
      CALLEE_VECTOR::const_iterator callee_iter;
      for (callee_iter = rna->Callee_list().begin(); callee_iter != rna->Callee_list().end(); ++callee_iter) {
        DNA_NODE *callee = ipsa->Get_dna(callee_iter->Callee());
        INT arg_cnt = callee->Parm_list()->size() - 1;
        if (arg_cnt > rna->Arg_cnt())
          arg_cnt = rna->Arg_cnt();
        for (INT j = VAR_INIT_ID; j <= arg_cnt; ++j) {
          rna->Set_arg_flag(j, callee->Parm_flags(j) | ARG_REF_MERGED);
        }
      }
    }
  }
}

// =============================================================================
//
// Update_arg_crs: rna arg coderep may change after Rename_codemap, update arg
//                 crs if changed
//
// =============================================================================
void DNA_NODE::Update_arg_crs()
{
  if (_call_list.size() > VAR_INIT_ID) {
    Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
             (TFile, "Update arg crs for fun:%s\n", Fname()));
    for (INT i = VAR_INIT_ID; i < _call_list.size(); ++i) {
      RNA_NODE *rna = _call_list[i];
      CODEREP *rhs = rna->Callstmt()->Rhs();
      for(INT32 j = 0; j < rhs->Kid_count(); ++j) {
        int arg_idx = j + 1;
        rna->Update_arg_crs(arg_idx, rhs->Opnd(j));
      }
    }
  }
}

// =============================================================================
//
// Is_param_copyin: after the mid-whirl lowering, the formal parameter are
//                  assigned with dedicated register on the rhs of STID stmt
//                  in the next bb of the entry_bb.
//
// =============================================================================
BOOL
DNA_NODE::Is_param_copyin(STMTREP *stmt) const
{
  if (stmt->Opr() != OPR_STID)
    return FALSE;
  CODEREP *rhs = stmt->Rhs();
  while(rhs->Kind() == CK_OP && (rhs->Opr() == OPR_CVT || rhs->Opr() == OPR_CVTL))
    rhs = rhs->Opnd(0);
  if (rhs->Kind() == CK_VAR &&
      rhs->Is_flag_set((CR_FLAG)(CF_DEF_BY_CHI)) &&
      rhs->Def_at_entry() &&
      Comp_unit()->Opt_stab()->Aux_stab_entry(rhs->Aux_id())->Is_dedicated_preg()){
    IDTYPE maybe_param = Is_param(stmt->Lhs());
    return (maybe_param != INVALID_VAR_IDX);
  }
  return FALSE;
}

IDTYPE
DNA_NODE::Is_param_copyin(CODEREP *lhs, CODEREP *rhs)
{
  while(rhs->Kind() == CK_OP && (rhs->Opr() == OPR_CVT || rhs->Opr() == OPR_CVTL))
    rhs = rhs->Opnd(0);
  if (rhs->Kind() == CK_VAR &&
      rhs->Is_flag_set((CR_FLAG)(CF_DEF_BY_CHI)) &&
      rhs->Def_at_entry() &&
      Comp_unit()->Opt_stab()->Aux_stab_entry(rhs->Aux_id())->Is_dedicated_preg()){
    // update the CR of parm_list entry if rhs is input register, which def_at_entry
    IDTYPE maybe_param = Is_param(lhs);
    if (maybe_param != INVALID_VAR_IDX) {
      _parm_list[maybe_param]->Set_cr(lhs);
      return maybe_param;
    }
  }
  return INVALID_VAR_IDX;
}

// =============================================================================
// Is_va_arg_last_param
// Check if func is va_arg and st is the last known parameter
//
// =============================================================================
BOOL
DNA_NODE::Is_va_arg_last_param(ST* st) const
{
  if (ST_sclass(st) != SCLASS_FORMAL)
    return FALSE;
  if (!TY_is_varargs(ST_type(St())))
    return FALSE;
  if (_parm_list.size() <= VAR_INIT_ID)
    return TRUE;  // no formal param: foo(...)
  VAR_NODE* last = _parm_list.back();
  Is_True(last != NULL, ("last param is NULL"));
  if (last && last->St_idx() == ST_st_idx(st))
    return TRUE;
  return FALSE;
}

// =============================================================================
//
// Collect parm_list: formal parameters from WN tree; the stid is critical for
//                    the symbol resolution later
//
// =============================================================================
void
DNA_NODE::Collect_parm_list(WN *wn)
{
  WN *stmt;
  INT i;

  if (wn == NULL)
    return;

  if (WN_operator(wn) == OPR_FUNC_ENTRY ||
      WN_operator(wn) == OPR_ALTENTRY) {
    for (i = 0; i < WN_kid_count(wn); i++) {
      WN *kid = WN_kid(wn,i);
      if (kid !=NULL && WN_operator(kid) == OPR_IDNAME) {

        // enter the parameter list
        ST_IDX stid = WN_st_idx(kid);
        VAR_NODE *vnd = CXX_NEW(VAR_NODE(stid, NULL), Mem_pool());
        _parm_list.push_back(vnd);
      }
      else {
        Collect_parm_list(kid);
      }
    }
  }

  if (WN_operator(wn) == OPR_BLOCK) 
    for (stmt = WN_first(wn); stmt != NULL; stmt = WN_next(stmt))  
      Collect_parm_list(stmt);
  else if ( !OPERATOR_is_black_box( WN_operator(wn) ) ) {
    for (i = 0; i < WN_kid_count(wn); i++) 
      Collect_parm_list(WN_kid(wn,i));
  }
}

// =============================================================================
//
// Calc_stack_size()
//    Calculate static stack size for this function
//
// =============================================================================
UINT32
DNA_NODE::Calc_stack_size() const
{
  CONTEXT_SWITCH context(this);
  UINT32 size = 0;
  UINT32 ptr_size = MTYPE_size_min(Pointer_type) >> 3;

  // symbols in current symtab
  ST* st;
  INT i;
  FOREACH_SYMBOL (CURRENT_SYMTAB, st, i) {
    if (ST_class(st) != CLASS_VAR)
      continue;
    // treat parameters are passed by memory or by register at first
    // then stored on stack
    // pay attention, frame pointer & return address are also FORMALs
    // in CURRENT_SYMTAB
    if (ST_sclass(st) != SCLASS_AUTO &&
        ST_sclass(st) != SCLASS_FORMAL &&
        ST_sclass(st) != SCLASS_FORMAL_REF)
      continue;
    // ignore ST starts with ".result_decl_" which is created from
    // GCC RESULT_DECL but not used in current function
    if (strncmp(ST_name(st), ".result_decl_", 13) == 0)
      continue;

    size += ST_size(st);
  }

  // align to pointer size
  return (size + ptr_size - 1) & ~(ptr_size - 1);
}

// =============================================================================
//
// Find_referenced_vars_at_entry marks which parameter is used within the call
//     this part of the work will also include the list of touched heap_objects.
//
// =============================================================================
void
DNA_NODE::Find_referenced_vars_at_entry(COMP_UNIT *cu, BB_NODE *bb)
{
  if (bb->Kind() != BB_ENTRY)
    return;
  STMTREP *stmt = bb->First_stmtrep();
  Is_True(bb->First_stmtrep()->Op() == OPC_OPT_CHI,
          ("Find_uninit_locals_for_entry: cannot find entry chi"));

  const STRING   pu_name = Cur_PU_Name;
  CHI_NODE      *cnode;
  CHI_LIST_ITER  chi_iter;
  FOR_ALL_NODE(cnode, chi_iter, Init(bb->First_stmtrep()->Chi_list())) {
    if (! cnode->Live())
      continue;
    if (cnode->RESULT()->Is_flag_set(CF_IS_ZERO_VERSION))
      continue;
    AUX_STAB_ENTRY *sym = cu->Opt_stab()->Aux_stab_entry(cnode->Aux_id());
    ST             *st  = sym->St();

    if (st == NULL)
      continue;
    if (! sym->Is_real_var())
      continue;
    if (ST_is_temp_var(st) && ST_sclass(st) == SCLASS_AUTO)
      continue;
    if (sym->Is_volatile())
      continue;
    if (sym->Mp_shared())
      continue;
    if (sym->Has_nested_ref())
      continue;
    if (! sym->Points_to()->Local()) {
      ; // continue;  handle global pointers references
    }
    //if (! sym->Points_to()->No_alias()) {
    //continue;
    //}
    if (sym->Points_to()->F_param()) {
      continue;
    }

    ST_IDX    stid = st->st_idx;
    VAR_NODE *vnd;
    switch (ST_sclass(st)) {
    case SCLASS_AUTO:
      break;

    case SCLASS_FORMAL:
    case SCLASS_FORMAL_REF:
    {
      // split the handling for Formal to catch those not referenced formals
      // when the formal is not referenced at the entry, it is redefined inside PU
      for (INT i = VAR_INIT_ID; i < _parm_list.size(); ++i) {
        if ((_parm_list[i])->St_idx() == stid) {
          _parm_list[i]->Set_cr(cnode->RESULT());
          continue;
        }
      }
    }
    break;
    case SCLASS_FSTATIC:
    case SCLASS_UGLOBAL:
    case SCLASS_DGLOBAL:
    case SCLASS_COMMON:
    case SCLASS_EXTERN:
    {
      vnd = CXX_NEW(VAR_NODE(stid, cnode->RESULT()), Mem_pool());
      vnd->Set_flag(REF_GLOBREF);
      _glob_list.push_back(vnd);
    }
    break;
    default:
      break;
    }
  }
}

// =============================================================================
//
//
// =============================================================================
void
DNA_NODE::Create_refn_4_entrychi(COMP_UNIT *cu, STMTREP *entry_chi, MEM_POOL *pool)
{
  Is_True(entry_chi->Opr() == OPR_OPT_CHI,
          ("Create_refn_4_entrychi: wrong entry_chi statement"));

  if (_parm_list.size() > VAR_INIT_ID) {
    INT i;
    for (i = VAR_INIT_ID; i < _parm_list.size(); ++i) {
      if ((_parm_list[i]->Flags() & REF_ILOAD ||
           _parm_list[i]->Flags() & REF_ISTORE ||
           _parm_list[i]->Flags() & REF_PTRPASSED) != 0) {
        CODEREP *cr = _parm_list[i]->Cr();
        if (cr != NULL)
          cu->Vsa()->Create_entrychi_hor(cr, entry_chi, pool);
      }
    }
  }
}


// =============================================================================
//
//
// =============================================================================
#if 0
void
DNA_NODE::Create_refn_4_exitmu(COMP_UNIT *cu, STMTREP *retstmt, MEM_POOL *pool)
{
  Is_True(retstmt->Opr() == OPR_RETURN ||
          retstmt->Opr() == OPR_RETURN_VAL,          
          ("Create_refn_4_exitmu: wrong return statement"));

  if (_parm_list.size() > VAR_INIT_ID) {
    INT i;
    for (i = VAR_INIT_ID; i < _parm_list.size(); ++i) {

      if ((_parm_list[i]->Flags() & REF_ISTORE) != 0) {
        CODEREP *cr = _parm_list[i]->Cr();
        cu->Vsa()->Create_refn_vsym_mu(cr, retstmt, pool);
      }
    }
  }
}
#endif

// =============================================================================
//
// Find_param_references from the expression and update the _parm_list with the
//           appropriate flags as defined in the header file
//
// =============================================================================
IDTYPE
DNA_NODE::Find_param_references(CODEREP *x, BOOL *is_ptr, ACTION act, IDTYPE_SET &visited_set, BOOL deeply)
{
  CONTEXT_SWITCH context(this);
  IDTYPE retv = INVALID_VAR_IDX;
  CODEREP *kid;
  STMTREP *defstmt;

  switch (x->Kind()) {
  case CK_LDA: 
  case CK_CONST: 
  case CK_RCONST:
    break;
  case CK_VAR:
    if (deeply) {
      defstmt = x->Defstmt();
      if (defstmt && defstmt->Opr() == OPR_STID) {
        return Find_param_references(defstmt->Rhs(), is_ptr, act, visited_set, deeply);
      } else if (x->Is_flag_set(CF_DEF_BY_PHI)) {
        PHI_NODE *phi = x->Defphi();
        if (visited_set.find(phi->Bb()->Id()) == visited_set.end()) {
          visited_set.insert(phi->Bb()->Id());
        } else {
          return retv;
        }
        BB_NODE* bb_pred;
        BB_LIST_ITER bb_iter;
        INT i = 0;
        FOR_ALL_ELEM(bb_pred, bb_iter, Init(phi->Bb()->Pred())) {
          CODEREP* opnd = phi->OPND(i);
          retv = Find_param_references(opnd, is_ptr, act, visited_set, deeply);
          if (retv != INVALID_VAR_IDX) {
            return retv;
          }
          i++;
        }
      }
    }
    *is_ptr = (Ty_Table[x->object_ty()].kind == KIND_POINTER);
    retv = Is_param(x);
    
    if (retv != INVALID_VAR_IDX) {
      switch (act) {
      case IN_PARM:
        _parm_list[retv]->Set_flag(REF_PTRPASSED);
        break;
      case IN_ILOAD_BASE:
        _parm_list[retv]->Set_flag(REF_ILOAD);
        break;
      case IN_ISTORE_BASE:
        _parm_list[retv]->Set_flag(REF_ISTORE);
        break;
      default:
        ;
      }
    }
    return retv;
  case CK_IVAR:
    if (x->Opr() == OPR_PARM)
      retv = Find_param_references(x->Ilod_base(), is_ptr, IN_PARM, visited_set, deeply);
    else {
      Find_param_references(x->Ilod_base(), is_ptr, IN_ILOAD_BASE, visited_set, deeply);
      retv = INVALID_VAR_IDX;
    }
    break;
  case CK_OP:
    for (INT32 i = 0; i < x->Kid_count(); i++) {
      // return immediately if we find the param reference
      CODEREP *opnd = x->Opnd(i);
      IDTYPE parm_num = Find_param_references(x->Opnd(i), is_ptr, IN_NONE, visited_set, deeply);
      if (opnd->Kind() == CK_VAR && parm_num != INVALID_VAR_IDX) {
        retv = parm_num;
        if (*is_ptr) {
          if (act == IN_ILOAD_BASE)
            _parm_list[retv]->Set_flag(REF_ILOAD);
          else if (act == IN_ISTORE_BASE)
            _parm_list[retv]->Set_flag(REF_ISTORE);
        }
      }
    }
    break;
  default:
    break;
  }
  return retv;
}

// =============================================================================
//
// Find_glob_references from the expression and update the _glob_list with the
//           appropriate flags as defined in the header file
//
// =============================================================================
IDTYPE
DNA_NODE::Find_glob_references(CODEREP *x, BOOL *is_ptr, ACTION act)
{
  CONTEXT_SWITCH context(this);
  IDTYPE retv = INVALID_VAR_IDX;
  CODEREP *kid;

  switch (x->Kind()) {
  case CK_LDA: 
  case CK_CONST: 
  case CK_RCONST:
    break;
  case CK_VAR: 
    *is_ptr = (Ty_Table[x->object_ty()].kind == KIND_POINTER);
    retv = Is_global(x);
    if (retv != INVALID_VAR_IDX) {
      switch (act) {
      case IN_PARM:
        Is_True(FALSE, ("DNA_NODE::Find_glob_reference called for IN_PARM act"));
        break;
      case IN_ILOAD_BASE:
        _glob_list[retv]->Set_flag(REF_ILOAD);
        break;
      case IN_ISTORE_BASE:
        _glob_list[retv]->Set_flag(REF_ISTORE);
        break;
      default:
        ;
      }
    }
    return retv;
  case CK_IVAR:
    if (x->Opr() == OPR_PARM)
      retv = Find_glob_references(x->Ilod_base(), is_ptr, IN_PARM);
    else 
      retv = Find_glob_references(x->Ilod_base(), is_ptr, IN_ILOAD_BASE);
    break;
  case CK_OP:
    for (INT32 i = 0; i < x->Kid_count(); i++) {
      // return immediately if we find the param reference
      CODEREP *opnd = x->Opnd(i);
      IDTYPE glob_num = Find_glob_references(x->Opnd(i), is_ptr, IN_NONE);
      if (opnd->Kind() == CK_VAR && glob_num != INVALID_VAR_IDX) {
        retv = glob_num;
        if (*is_ptr) {
          if (act == IN_ILOAD_BASE)
            _glob_list[retv]->Set_flag(REF_ILOAD);
          else if (act == IN_ISTORE_BASE)
            _glob_list[retv]->Set_flag(REF_ISTORE);
        }
      }
    }
    break;
  default:
    break;
  }
  return retv;
}

// =============================================================================
//
// Is_simple_expression: return the node which has var as i=its parameter
//                 returns NULL if the expresson is more than a LDID node
//
// =============================================================================
CODEREP *
DNA_NODE::Is_simple_expression(CODEREP *x, INT64 *adjuster, BOOL deeply) const
{
  switch (x->Kind()) {
  case CK_VAR:
    if (deeply) {
      STMTREP *defstmt = x->Defstmt();
      if (defstmt && defstmt->Opr() == OPR_STID)
        return Is_simple_expression(defstmt->Rhs(), adjuster, deeply);
      else
        return x;
    }
    else
      return x;
  case CK_IVAR:
    return Is_simple_expression(x->Ilod_base(), adjuster, deeply);
  case CK_OP:
  {
    const OPERATOR opr = x->Opr();
    if (opr == OPR_CVT || opr == OPR_CVTL)
      return Is_simple_expression(x->Opnd(0), adjuster, deeply);
    else if (opr == OPR_ADD || opr == OPR_SUB) {
      CODEREP *retv = Is_simple_expression(x->Opnd(0), adjuster, deeply);
      if (retv == NULL) return NULL;

      CODEREP *co = x->Opnd(1);
      if (co->Kind() != CK_CONST) return NULL;
      *adjuster = co->Const_val();
      if (opr == OPR_SUB) *adjuster *= -1;

      return retv;
    }
    else if (opr == OPR_CALL) {
      // to handle Call to free as side effect
      return Is_simple_expression(x->Opnd(0), adjuster, TRUE);
    }
    else
      return NULL;
  }
  break;
  default:
    return NULL;
  }
}


// =============================================================================
//
// RNA_NODE::Symbolic_eval_predicate evaluate the return expression coderep x 
//           will be executed given the set of actual argument passed from caller
//
// =============================================================================
BOOL
DNA_NODE::Symbolic_eval_predicate(RNA_NODE *rna, CODEREP *x, BB_NODE *exit_bb)
{
  // 1. Traverse the CD bb of the containing BB of expression x
  STMTREP *branch_stmt = Branch_stmt_4_bb(exit_bb).first;

  // 2. Pick up the conditional branch of the CD bb
  CODEREP *cond_exp    = (branch_stmt)? branch_stmt->Rhs():NULL;
  if (cond_exp == NULL) return FALSE;
 
  // 3. Perform symbolic evaluation of the conditional expression
  CODEREP *lhs_val;
  CODEREP *rhs_val;
  switch (cond_exp->Kind()) {
  case CK_CONST:
  case CK_RCONST:
    break;
  case CK_VAR:
    break;
  case CK_IVAR:
    break;
  case CK_OP: {
    switch (cond_exp->Opr()) {
    case OPR_GT:
    case OPR_GE:
    case OPR_LT:
    case OPR_LE:
      break;
    default:
      break;
    }
  }
  default:
    break;
  }

  // 4. Apply the true or false branch logic
  return FALSE;
}

// =============================================================================
//
// DNA_NODE::Eval_upward_value_attribute, it analyzes the return value
//     if it is defined by an input parameter and return the sequence number of
//     parameter within the parm_list as well as its constant adjustment.
//     Besides value returned through return statement or pointer parameters,
// Todo: 1. return pointer received from malloc calls
//       2. freed pointer parameters, notify caller which parameter get called
// Note: we make context switch to the callee to enable us perform CODEREP level
//     evaluation.  Therefore, do not try to manipulate the caller side CODEREP.
//
// =============================================================================
IDTYPE
DNA_NODE::Eval_upward_value_attribute(RNA_NODE *rna, IDTYPE *oparam, INT64 *adjuster)
{
  // This function has been evaluated before
  if (Is_set(DNA_PARAM_NO_PASSTHRU)) return INVALID_VAR_IDX;

  INT retv_count = _retv_list.size() - 1;
  if (retv_count == 0) return No_passthru();

  CONTEXT_SWITCH context(this);
  CODEREP *x, *ldid_node;
  ST      *st;
  IDTYPE   retv;
  
  for (INT i = PDV_INIT_ID; i < _retv_list.size(); ++i) {

    x = _retv_list[i]->Stmt()->Rhs();
    if (_retv_list[i]->Oparam() != INVALID_VAR_IDX)
      *oparam = _retv_list[i]->Oparam();

    if ((ldid_node = Is_simple_expression(x, adjuster, FALSE)) == NULL) continue;
    if ((retv = Is_param(ldid_node)) == INVALID_VAR_IDX) continue;
    if (! Is_parm_original(retv)) continue;
    if ((_parm_list[retv]->Flags() & REF_FREED) != 0 &&  // caller has already handled
         Deallocate()) continue;
    if (_retv_list[i]->Predicate() != NULL) {  // return conditionally
      // Symbolically Evaluate the predicate expression in the callee for simple cases
      BOOL will_return = Symbolic_eval_predicate(rna, x, _retv_list[i]->Predicate());
      if (! will_return) continue;
    }

    return retv;

  }
  return No_passthru();
}


// =============================================================================
//
// DNA_NODE::Collect_global_ref_info for current DNA
//
// =============================================================================
void 
DNA_NODE::Collect_global_ref_info(GLOB_REF_MGR *glob_mgr, MEM_POOL *pool)
{
  Is_True_Ret(glob_mgr, ("null ref map"));
  CONTEXT_SWITCH ctx(this);
  BB_NODE *entry_bb = Comp_unit()->Vsa()->Cfg()->Entry_bb();
  GLOB_REF_LIST *ref_list = 
      CXX_NEW(GLOB_REF_LIST(GLOB_REF_LIST::allocator_type(pool)), pool);
  glob_mgr->Enter_global_info(this, ref_list);
  INT stack_size = 0;
  Collect_global_ref_info(entry_bb, glob_mgr, ref_list, &stack_size, pool);
}

// =============================================================================
//
// DNA_NODE::Collect_global_ref_info for given bb
//
// =============================================================================
void
DNA_NODE::Collect_global_ref_info(BB_NODE *bb, GLOB_REF_MGR *glob_mgr, 
                                  GLOB_REF_LIST *ref_list, INT *stack_size, MEM_POOL *pool)
{
  if (*stack_size > VSA_Checker_Max_Frame) {
    return;
  }
  *stack_size += 1;
  CONTEXT_SWITCH ctx(this);
  VSA *vsa = Comp_unit()->Vsa();
  STMTREP *sr;
  STMTREP_ITER iter(bb->Stmtlist());
  FOR_ALL_NODE(sr, iter, Init()) {
    CODEREP *rhs = sr->Rhs();
    CODEREP *lhs = sr->Lhs();
    if(rhs) {
      Collect_global_ref_info(sr, rhs, ref_list, stack_size, pool);
    }
    if(lhs) {
      Collect_global_ref_info(sr, lhs, ref_list, stack_size, pool);
    }
    if(OPERATOR_is_call(sr->Opr())) {
      RNA_NODE *rna = vsa->Sr_2_rna(sr);
      if(rna == NULL) {
        continue;
      }
      for(CALLEE_VECTOR::const_iterator it = rna->Callee_list().begin();
          it != rna->Callee_list().end(); it++) {
        DNA_NODE* callee = vsa->Ipsa()->Get_dna(it->Callee());
        if(callee && !callee->Non_functional()) {
          CONTEXT_SWITCH callee_ctx(callee);
          BB_NODE *entry_bb = callee->Comp_unit()->Vsa()->Cfg()->Entry_bb();
          if(!glob_mgr->Get_ref_list(callee)) {
            GLOB_REF_LIST *callee_ref_list = 
              CXX_NEW(GLOB_REF_LIST(GLOB_REF_LIST::allocator_type(pool)), pool);
            glob_mgr->Enter_global_info(callee, callee_ref_list);
            callee->Collect_global_ref_info(entry_bb, glob_mgr,
                                            callee_ref_list, stack_size, pool);
          }
          // add callee ref info
          ST *call_st = callee->St();
          GLOB_REF_INFO *ref_info = 
            CXX_NEW(GLOB_REF_INFO(GLOB_REF_INFO::REF_BY_CALL, 
                                  call_st ? ST_st_idx(call_st) : ST_IDX_ZERO,
                                  sr, this, callee), pool);
          ref_list->push_back(ref_info);
        } // end of functional callee
      } // end of iterate callee list
    } // end of OPERATOR_is_call
  }

  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb->Dom_bbs())) {
    Collect_global_ref_info(dom_bb, glob_mgr, ref_list, stack_size, pool);
  }
}

// =============================================================================
//
// DNA_NODE::Collect_global_ref_info for given cr
//
// =============================================================================
void
DNA_NODE::Collect_global_ref_info(STMTREP *sr, CODEREP *cr, 
                                  GLOB_REF_LIST *ref_list, INT *stack_size, MEM_POOL *pool)
{
  Is_True_Ret(cr != NULL, ("invalid cr"));
  if (*stack_size > VSA_Checker_Max_Frame) {
    return;
  }
  *stack_size += 1;
  CONTEXT_SWITCH ctx(this);
  VSA *vsa = Comp_unit()->Vsa();
  switch(cr->Kind()) {
    case CK_VAR:
    {
      AUX_STAB_ENTRY *sym = vsa->Opt_stab()->Aux_stab_entry(cr->Aux_id());
      ST *st = sym->St();
      if(st && sym->Is_global() && !ST_is_class_const_data(st)) {
        DNA_NODE *init_dna = NULL;
        if(PU_java_lang(Get_Current_PU())) {
          init_dna = vsa->Ipsa()->Get_global_clinit_dna(File_idx(), ST_st_idx(st));
        }
        GLOB_REF_INFO::REF_TYPE type = GLOB_REF_INFO::REF_BY_VAR_MU;
        if(sr->Opr() == OPR_STID && cr == sr->Lhs()) {
          type = GLOB_REF_INFO::REF_BY_VAR_CHI;
        }
        GLOB_REF_INFO *info = 
          CXX_NEW(GLOB_REF_INFO(type, ST_st_idx(st),
                                sr, this, init_dna), pool);
        ref_list->push_back(info);
      }
    }
    break;
    case CK_IVAR:
    {
      BOOL is_store = OPERATOR_is_store(sr->Opr()) && cr == sr->Lhs();
      Collect_global_ref_info(sr, is_store ? cr->Istr_base() : cr->Ilod_base(), ref_list, stack_size, pool);
    }
    break;
    case CK_OP:
      for(INT i = 0; i < cr->Kid_count(); ++i) {
        Collect_global_ref_info(sr, cr->Opnd(i), ref_list, stack_size, pool);
      }
    default:
    break;
  }
}

// =============================================================================
//
// DNA_NODE::Get_global_retv, return global return PDV_NODE with given symbol
//
// =============================================================================
PDV_NODE *
DNA_NODE::Get_global_retv(ST_IDX stidx)
{
  PNODE_VECTOR *rgv_list = Rgvl_list();
  for(INT i = PDV_INIT_ID; i < rgv_list->size(); i++) {
    PDV_NODE *pdv = (*rgv_list)[i];
    STMTREP *stmt = pdv->Stmt();
    CODEREP *lhs = stmt->Lhs();
    if(lhs && lhs->Kind() == CK_VAR) {
      AUX_STAB_ENTRY *sym = Comp_unit()->Opt_stab()->Aux_stab_entry(lhs->Aux_id());
      ST *st = sym->St();
      if(st && ST_st_idx(st) == stidx) {
        return pdv;
      }
    }
  }
  return NULL;
}

// =============================================================================
//
// DNA_NODE::Get_callsite_rna, given the call statement,
//
// =============================================================================
RNA_NODE*
DNA_NODE::Get_callsite_rna(STMTREP *stmt)
{
  RNA_NODE *rna;
  RNA_NODE *retv = NULL;
  if (_call_list.size() > VAR_INIT_ID) {
    for (INT i = VAR_INIT_ID; i < _call_list.size(); ++i) {
      rna = _call_list[i];
      if (rna->Callstmt() == stmt) {
        retv = rna;
        break;
      }
    }
  }
  return retv;
}

// an overloaded function for Get_callsite_rna(STMTREP *)
RNA_NODE*
DNA_NODE::Get_callsite_rna(CODEREP *call)
{
  RNA_NODE *rna;
  RNA_NODE *retv = NULL;
  if (_call_list.size() > VAR_INIT_ID) {
    for (INT i = VAR_INIT_ID; i < _call_list.size(); ++i) {
      rna = _call_list[i];
      if (rna->Callstmt()->Rhs() == call) {
        retv = rna;
        break;
      }
    }
  }
  return retv;
}


// =============================================================================
//
// Is_func_of_iparm: Is the expression x composing of input parameter +/-
//                   a constant.
//
// =============================================================================
CODEREP*
DNA_NODE::Is_func_of_iparm(CODEREP *x, BOOL var_only)
{
  switch (x->Kind()) {
  case CK_LDA:
  case CK_CONST:
  case CK_RCONST:
    return var_only ? NULL : x;
  case CK_VAR:
    return Is_param(x) ? x : NULL;
  case CK_IVAR:
    return var_only ? NULL : Is_func_of_iparm(x->Ilod_base(), FALSE);
  case CK_OP:
    for (INT32 i = 0; i < x->Kid_count(); i++) {
      CODEREP *cr = Is_func_of_iparm(x->Opnd(i), var_only);
      if (cr)
        return cr;
    }
    return NULL;
  default:
    return NULL;
  }
}

// =============================================================================
//
// Branch_stmt_4_bb: Fetch the branch statement in the Control Dependence of
// the bb.  Only return the branch stmt that contains only input parameter(s)
//
// =============================================================================
pair<STMTREP*, CODEREP*>
DNA_NODE::Branch_stmt_4_bb(BB_NODE *bb, BOOL var_only)
{
  //Is_True(bb->Kind() == BB_EXIT, ("IPSA::Branch_stmt_4_bb called for non-exit block"));

  // Iterate through the CD set since there might be multiple CD for this BB
  BB_NODE *rcfgbb;
  BB_NODE_SET_ITER rcfg_iter;
  FOR_ALL_ELEM( rcfgbb, rcfg_iter, Init( bb->Rcfg_dom_frontier() ) ) {
    STMTREP *branch_stmt = rcfgbb->Branch_stmtrep();
    if (!branch_stmt || !branch_stmt->Rhs()) continue;
    CODEREP *cond_exp = Is_func_of_iparm(branch_stmt->Rhs(), var_only);
    if (cond_exp)
      return make_pair(branch_stmt, cond_exp);
  }
  return make_pair((STMTREP*)NULL, (CODEREP*)NULL);
}

// =============================================================================
//
// Save_context: work together with Restore_context to enable context switch
//     in IPSA call graph traversal
//
// =============================================================================
void
DNA_NODE::Save_context(void)
{
  // save PU context
  _context.Save_context();
  _file_idx        = File_Index;
  _comp_unit       = g_comp_unit;
}                             

// =============================================================================
//
// Restore_context: work together with Save_context to enable context switch
//     in IPSA call graph traversal
//
// =============================================================================
void
DNA_NODE::Restore_context (void) const
{
  extern CODEMAP*  Initialize_CR_simp(CODEMAP*); // or we include opt_fold.h

  // restore file context if necessary
  if (_file_idx != File_Index && _file_idx != INVALID_DNA_FILE_IDX) {
    WHIRL_FILE_MANAGER *mgr = WHIRL_FILE_MANAGER::Get();
    mgr->Set_file_context(_file_idx);
    File_Index = _file_idx;
    if (File_Index <= mgr->Get_file_count())
      Set_Error_Source(mgr->Get_file(File_Index).File_name());
  }

  // restore PU context
  _context.Restore_context();
  g_comp_unit      = _comp_unit;
  g_opt_stab       = _comp_unit->Opt_stab();
  Initialize_CR_simp(_comp_unit->Htable());
}


// =============================================================================
//
// DNA_GLOB_REF_MAP::Get_ref_chi_list: returns global store stmt list with given st
//
// =============================================================================
void
GLOB_REF_MGR::Get_ref_chi_stmts(DNA_NODE *dna, ST_IDX global_st, vector<STMTREP*> &list)
{
  GLOB_REF_LIST *ref_list = Get_ref_list(dna);
  if(ref_list == NULL) {
    return;
  }
  for(GLOB_REF_LIST::iterator ref_iter = ref_list->begin();
      ref_iter != ref_list->end(); ref_iter++) {
    GLOB_REF_INFO *info = *ref_iter;
    if(info->Is_ref_by_chi() && info->Ref_st() == global_st) {
      list.push_back(info->Ref_sr());
    }
  }
}

// =============================================================================
//
// DNA_GLOB_REF_MAP::Print: dump all global ref info
//
// =============================================================================
void
GLOB_REF_MGR::Print(IPSA *ipsa, FILE *fp) {
  fprintf(fp, "Dump of Global Ref Map:\n");
  fprintf(fp, "%s", SBar);
  for(DNA_GLOB_REF_ITER dna_iter = _ref_map.begin();
      dna_iter != _ref_map.end(); dna_iter++) {
    IDTYPE dna_idx = dna_iter->first;
    DNA_NODE *dna = ipsa->Get_dna(dna_idx);
    GLOB_REF_LIST *ref_list = dna_iter->second;
    fprintf(fp, "GLOB for fun %s\n", dna->Fname());
    if(ref_list != NULL) {
      for(GLOB_REF_LIST::iterator ref_iter = ref_list->begin();
          ref_iter != ref_list->end(); ref_iter++) {
        fprintf(fp, "  ");
        (*ref_iter)->Print(fp);
      }
    }
  }
  fprintf(fp, "%s", SBar);
}

// =============================================================================
//
// RNA_NODE::Enter_arg_list: counter to the parm_list get created for DNA,
//      the actual argument list is maintained in the RNA, for the context
//      sensitive static analysis algorithms.
//
// =============================================================================
void
RNA_NODE::Enter_arg_list(CODEREP *cr, IPSA *ipsa)
{
  VAR_NODE *vnd;

  switch (cr->Kind()) {
  case CK_CONST:
    vnd = CXX_NEW(VAR_NODE(ILLEGAL_AUX_ID, cr), ipsa->Mem_pool());
    if (cr->Const_val() == 0)
      vnd->Set_flag(ARG_VALUE_IVAD); 
    break;
  case CK_LDA:
    vnd = CXX_NEW(VAR_NODE(cr->Lda_aux_id(), cr), ipsa->Mem_pool());
    vnd->Set_flag(ARG_LDA | ARG_PTR | ARG_PTR_UNIQ); 
    break;
  case CK_VAR:
    vnd = CXX_NEW(VAR_NODE(cr->Aux_id(), cr), ipsa->Mem_pool());
    if (TY_KIND(cr->Lod_ty()) == KIND_POINTER)
      vnd->Set_flag(ARG_PTR);
    // if this var is an input parameter, we gather additional attributes
    // to this RNA node.
    if (cr->Value_invalid_addr())
      vnd->Set_flag(ARG_VALUE_IVAD);
    else if (cr->Value_maydangling())
      vnd->Set_flag(ARG_VALUE_MDANG);
    else if (cr->Value_maydef())
      vnd->Set_flag(ARG_VALUE_MAYD);
    else if (cr->Value_not_def())
      vnd->Set_flag(ARG_VALUE_UDEF);
    break;
  case CK_IVAR:
    vnd = CXX_NEW(VAR_NODE(ILLEGAL_AUX_ID, cr), ipsa->Mem_pool());
    if (TY_KIND(cr->Ilod_ty()) == KIND_POINTER)
      vnd->Set_flag(ARG_PTR); 
    break;
  default:
    vnd = CXX_NEW(VAR_NODE(ILLEGAL_AUX_ID, cr), ipsa->Mem_pool());
    break;
  }
  _arg_list.push_back(vnd);
}

// =============================================================================
// RNA_NODE::Update_retv_flags
//   update callsite chi nodes according to PDV_NODE
// =============================================================================
BOOL
RNA_NODE::Update_retv_flags(DNA_NODE* caller, PDV_NODE* pdv)
{
  AUX_ID arg_aux = 0;
  if (pdv->Oparam() != 0) {
    CODEREP* arg = Get_arg(pdv->Oparam());
    if (arg && arg->Kind() == CK_LDA) {
      arg_aux = arg->Lda_aux_id();
    }
    else {
      // TODO: vsym side effect
    }
  }
  CHI_NODE* cnode;
  CHI_LIST_ITER chi_iter;
  FOR_ALL_NODE(cnode, chi_iter, Init(Chi_list())) {
    if (!cnode->Live())
      continue;
    AUX_STAB_ENTRY* chi_sym = caller->Comp_unit()->Opt_stab()->Aux_stab_entry(cnode->Aux_id());
    if ((chi_sym->Is_return_preg() && pdv->Oparam() == 0 &&
         cnode->RESULT()->Offset() == pdv->Stmt()->Lhs()->Offset()) ||
        (cnode->Aux_id() == arg_aux)) {
      return pdv->Copy_flag_to_cr(cnode->RESULT());
    }
  }
  return FALSE;
}

IDTYPE
RNA_NODE::Get_arg_with_aux(AUX_ID aux, OPT_STAB *stab)
{
  AUX_STAB_ENTRY *sym = stab->Aux_stab_entry(aux);
  INT64 sym_begin = sym->Base_byte_ofst();
  INT64 sym_end = sym->Base_byte_ofst() + sym->Byte_size();
  for (INT i = VAR_INIT_ID; i < _arg_list.size(); ++i) {
    if ((_arg_list[i]->Flags() & ARG_LDA) != ARG_LDA)
      continue;
    Is_True(_arg_list[i]->Cr() != NULL &&
            _arg_list[i]->Cr()->Kind() == CK_LDA &&
            _arg_list[i]->Cr()->Lda_aux_id() == _arg_list[i]->St_idx(),
            ("invalid lda param"));
    AUX_STAB_ENTRY *arg = stab->Aux_stab_entry(_arg_list[i]->Cr()->Lda_aux_id());
    if (sym->Base() == arg->Base() &&
        sym_begin >= arg->Base_byte_ofst() &&
        sym_end <= (arg->Base_byte_ofst() + arg->Byte_size()))
      return i;
  }
  return INVALID_VAR_IDX;
}

IDTYPE
RNA_NODE::Get_arg_with_lda(ST_IDX stid)
{
  INT i;
  for (i = VAR_INIT_ID; i < _arg_list.size(); ++i) {
    if ((_arg_list[i]->St_idx() == stid) && _arg_list[i]->Flags() & ARG_LDA)
      return i;
  }
  return INVALID_VAR_IDX;
}

IDTYPE
RNA_NODE::Get_arg_with_cr(CODEREP *cr)
{
  INT i;
  for (i = VAR_INIT_ID; i < _arg_list.size(); ++i) {
    if ((_arg_list[i]->Cr() == cr))
      return i;
  }
  return INVALID_VAR_IDX;
}

pair<IDTYPE, BOOL>
RNA_NODE::Get_arg(CODEREP *cr, const VSA *vsa)
{
  INT i;
  INT lda_cr = INVALID_VAR_IDX;
  AUX_ID aux = cr->Kind() == CK_VAR ? cr->Aux_id() : ILLEGAL_AUX_ID;
  // get base aux_id by iterate st_group
  hash_set<AUX_ID> base_set;
  if (vsa && aux != ILLEGAL_AUX_ID) {
    OPT_STAB *opt_stab = vsa->Opt_stab();
    CONTEXT_SWITCH(vsa->Dna());
    AUX_ID base_aux = aux;
    do {
      base_aux = opt_stab->St_group(base_aux);
      if (!opt_stab->Aux_stab_entry(base_aux)->Is_real_var()) {
        base_set.insert(base_aux);
      }
    } while (base_aux != aux && base_aux != 0);
  }
  for (i = VAR_INIT_ID; i < _arg_list.size(); ++i) {
    if (_arg_list[i]->Cr() == cr)
      return make_pair(i, (_arg_list[i]->Flags() & ARG_LDA) == ARG_LDA);
    if (_arg_list[i]->Cr()->Kind() == CK_LDA &&
        (_arg_list[i]->Cr()->Lda_aux_id() == aux ||
         base_set.find(_arg_list[i]->Cr()->Lda_aux_id()) != base_set.end()))
      lda_cr = i;
  }
  if (lda_cr != INVALID_VAR_IDX)
    return make_pair(lda_cr, TRUE);
  else
    return make_pair(INVALID_VAR_IDX, FALSE);
}

void
RNA_NODE::Collect_arg_list(CODEREP *cr, IPSA *ipsa)
{
  ST_IDX    stid;
  VAR_NODE *vnd;
  switch (cr->Kind()) {
  case CK_IVAR:
    if (cr->Opr() == OPR_PARM) {
      Enter_arg_list(cr->Ilod_base(), ipsa);
    }
    else
      Enter_arg_list(cr, ipsa);
    break;
  case CK_OP:
    for (INT32 i=0; i<cr->Kid_count(); i++) {
      Collect_arg_list(cr->Opnd(i), ipsa);
    }
    break;
  default:
    break;
  }
}

void
RNA_NODE::Update_arg_crs(INT32 &arg_idx, CODEREP *cr)
{
  switch (cr->Kind()) {
    case CK_IVAR:
      if (cr->Opr() == OPR_PARM) {
        cr = cr->Ilod_base();
      }
      if(Get_arg(arg_idx) != cr) {
        VAR_NODE *vnode = Arg_list()->at(arg_idx);
        Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG), 
                 (TFile, "Update arg cr%d to cr%d", 
                  vnode->Cr()->Coderep_id(),
                  cr->Coderep_id()));
        vnode->Set_cr(cr);
      }
      arg_idx++;
      break;
    case CK_OP:
      for (INT32 i=0; i<cr->Kid_count(); i++) {
        Update_arg_crs(arg_idx, cr->Opnd(i));
      }
      break;
    default:
      break;
  }
}


// =============================================================================
//
// IPSA::Callee_return_an_arg_passthrough, this function is a method for
//     context aware analysis.  RNA's callsite specific, we may cache the query
//     result from callee in the RNA node.
//
// =============================================================================
CODEREP*
RNA_NODE::Get_arg(IDTYPE which_arg) const
{
  if (which_arg != INVALID_VAR_IDX) {
    if (which_arg < _arg_list.size())
      return _arg_list[which_arg]->Cr();
    else
      return NULL;
  }
  return NULL;
}

// =============================================================================
//
// RNA_NODE::Trim(IPSA *ipsa)
//
// =============================================================================
void
RNA_NODE::Trim(IPSA *ipsa)
{
  CALLEE_VECTOR::iterator iter;
  for (iter = _callee_list.begin(); iter != _callee_list.end(); /* */) {
    if (iter->Flags() & (ICALL_VAR_TARGET | ICALL_VSYM_TARGET)) {
      ++iter;
    }
    else {
      IDTYPE callee_idx = iter->Callee();
      DNA_NODE* callee = ipsa->Get_dna(callee_idx);
      // remove from callee's _clby_list
      callee->Dec_ref_cnt(this);
      // remove from _callee_list
      iter = _callee_list.erase(iter);
      Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
               (TFile, "RNA_NODE::Trim: Removing %s[%d] -> %s[%d]\n",
                       ipsa->Get_dna(Caller_idx())->Fname(), Caller_idx(),
                       callee->Fname(), callee->Dna_idx()));
    }
  }
}

// =============================================================================
//
// IPSA::Build_cha_begin: Tasks perfromed before build class hierarchy
// 1. Build vtable name set: for minimize loading of vtable initos
// 2. MEM_POOL push
//
// =============================================================================
void
IPSA::Build_cha_begin()
{
  Start_Timer(T_BUILD_CHA);
  SET_OPT_PHASE("Build class hierarchy");
  OPT_POOL_Push(Loc_pool(), VSA_DUMP_FLAG);
  if (!VSA_Load_All_Sym) {
    Build_vtable_name_set(Loc_pool());
  }
}

// =============================================================================
//
// IPSA::Build_cha_end: Tasks performed after build class hierarchy
// 1. Connect class hierarchy
// 2. clean up memory pool
// 3. memory trace
//
// =============================================================================
void
IPSA::Build_cha_end()
{
  _glob_cha->Connect_classes();

  Is_Trace_cmd(Get_Trace(TP_VSA, VSA_CHA_DUMP_FLAG), _glob_cha->Print(TFile));
  OPT_POOL_Pop(Loc_pool(), VSA_DUMP_FLAG);
  if (Get_Trace (TKIND_ALLOC, TP_MISC)) {
    fprintf(TFile, "======== MEM TRACE after Build class hierarchy ========\n");
    MEM_Trace();
  }
  Stop_Timer(T_BUILD_CHA);
}

// =============================================================================
//
// IPSA::Build_and_merge_cha create class hierarchy on current file and merge
// with global class hierarchy
// create class hierarchy based on language type
// JAVA - create JAVA_CLASS_HIERARCHY_BUILDER
// CXX  - create CXX_CLASS_HIERARCHY_BUILDER
//
// =============================================================================
void
IPSA::Build_and_merge_cha()
{
  // do not add rule file to class hierarchy
  if(FILE_INFO_is_rbc(File_info))
    return;

  CHA *cha = NULL;
  MEM_POOL *lpool = Loc_pool();
  DST_INFO *   cu_info;
  DST_IDX cu_idx = DST_get_compile_unit ();
  cu_info = DST_INFO_IDX_TO_PTR (cu_idx);
  DST_language lang = DST_COMPILE_UNIT_language( DST_ATTR_IDX_TO_PTR(
    DST_INFO_attributes(cu_info), DST_COMPILE_UNIT));
  switch (lang) {
    case DW_LANG_C_plus_plus:
      cha = CXX_NEW(CXX_CLASS_HIERARCHY_BUILDER(Mem_pool(), lpool),lpool);
      break;
    case DW_LANG_Java:
      cha = CXX_NEW(JAVA_CLASS_HIERARCHY_BUILDER(Mem_pool(), lpool), lpool);
      break;
    case DW_LANG_C89:
      break;
    default:
      Is_True_Ret(FALSE, ("not supported pu lang\n"));
      break;
  }
  if(cha != NULL) {
    _glob_cha->Merge(cha);
  }
}

// =============================================================================
//
// IPSA::Eval_callee_side_effect_return_value returns the argument that is returned
//       directly either through the return statement or an output parameter.
//       The return value of this function is the input argument while oparam
//       is the argument sequence number of the output parameter/argument.
//       The third parameter is to add the adjustment to the returned CODEREP,
//       such as the offset of a pointer if the return value is a pointer type.
//
// =============================================================================
CODEREP*
IPSA::Eval_callee_side_effect_return_value(RNA_NODE *rna, IDTYPE *oparam, INT64 *adjuster)
{
  DNA_NODE *callee = Get_dna(rna->Uniq_callee());

  if (callee == NULL) return NULL;

  if (callee->Is_set(DNA_PARAM_NO_PASSTHRU) ) return NULL;
  IDTYPE which_arg = callee->Eval_upward_value_attribute( rna, oparam, adjuster );
  return rna->Get_arg(which_arg);
}  

// =============================================================================
//
// New_rna: create the rna node and append to the vector of rnaid to rnanode map
//
// =============================================================================
RNA_NODE*
IPSA:: New_rna(DNA_NODE *dna, STMTREP *stmt)
{
  Is_True( this != NULL, ("Illegal call to IPSA::New_rna(), IPSA not initialized"));
  IDTYPE id = _rnaid_to_rnanode.size();
  RNA_NODE *retv = CXX_NEW(RNA_NODE(id, dna->Dna_idx(), stmt, Mem_pool()), Mem_pool());
  Is_True(retv && stmt, ("rna or callstmt invalid"));
  retv->Set_rbc_op(Rbc(), dna);
  VSA *vsa = dna->Comp_unit()->Vsa();
  Is_True(vsa, ("vsa not created"));
  if (vsa) {
    vsa->Enter_sr_rna_map(stmt, retv);
  }
  dna->Add_call(retv);  // add rna to dna call list
  _rnaid_to_rnanode.push_back(retv);
  return retv;
}

// =============================================================================
// IPSA::Convert_icall_to_call()
// =============================================================================
void
IPSA::Convert_icall_to_call(DNA_NODE *dna, STMTREP* stmt, ST* call_st)
{
  Is_True(stmt != NULL && stmt->Opr() == OPR_ICALL, ("not icall"));
  Is_True(call_st != NULL && ST_class(call_st) == CLASS_FUNC, ("not function"));

  Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
           (TFile, "IPSA::Convert_icall_to_call: before promotion:\n"));
  Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG), stmt->Print(TFile));

  OPCODE opc = OPCODE_make_op(OPR_CALL, stmt->Rtype(), stmt->Desc());
  CODEREP* old_rhs = stmt->Rhs();
  int old_kid_count = old_rhs->Kid_count();
  CODEREP *cr = Alloc_stack_cr(old_kid_count - 1 + IVAR_EXTRA_NODE_CNT);

  cr->Init_op(opc, old_kid_count - 1);
  for (int i = 0; i < old_kid_count - 1; i++) {
    cr->Set_opnd(i, old_rhs->Opnd(i));
  }

  CODEREP *new_rhs = dna->Comp_unit()->Htable()->Hash_Op(cr);
  stmt->Init(stmt->Lhs(), new_rhs, opc);
  stmt->Set_st(call_st);
  if (stmt->Call_flags() & WN_CALL_IS_VIRTUAL)
    stmt->Set_call_flags(stmt->Call_flags() & ~WN_CALL_IS_VIRTUAL); // remove virtual flag

  old_rhs->Opnd(old_kid_count - 1)->DecUsecnt();
  old_rhs->DecUsecnt();
  new_rhs->IncUsecnt();

  Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
           (TFile, "IPSA::Convert_icall_to_call: after promotion:\n"));
  Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG), stmt->Print(TFile));
}

ST*
IPSA::Find_or_create_fun_st(UINT32 file_idx, ST_IDX st_idx, UINT32 ref_file_idx, TY_IDX ty)
{
  if (file_idx == ref_file_idx) {
    return ST_ptr(st_idx);
  }
  ST* ref_st = St_ptr(file_idx, st_idx);
  // create new PU and extern st
  PU_IDX pu_idx;
  PU&    pu = New_PU (pu_idx);
  ST* st = New_ST(GLOBAL_SYMTAB);
  PU_Init (pu, ty, CURRENT_SYMTAB);
  ST_Init(st, Save_Str(ST_name(file_idx, ref_st)), ST_class(ref_st),
          SCLASS_EXTERN, EXPORT_PREEMPTIBLE, (TY_IDX)pu_idx);

  WHIRL_FILE_MANAGER* mgr = WHIRL_FILE_MANAGER::Get();
  Is_True(mgr != NULL, ("not in xfa mode"));
  mgr->Add_symbol(ref_file_idx, ST_st_idx(st), file_idx, st_idx);
  return st;
}

TY_IDX
IPSA::Find_icall_ty_from_ivar_mu(DNA_NODE *dna, CODEREP *opnd,
                                 AUX_ID aux, hash_set<IDTYPE> &visited_bb)
{
  Is_True(opnd->Kind() == CK_VAR, ("bad opnd cr"));
  if (opnd->Is_flag_set(CF_DEF_BY_PHI)) {
    PHI_NODE *phi = opnd->Defphi();
    if (visited_bb.find(phi->Bb()->Id()) != visited_bb.end())
      return TY_IDX_ZERO;
    visited_bb.insert(phi->Bb()->Id());
    PHI_OPND_ITER phi_opnd_iter(phi);
    TY_IDX ret_ty = TY_IDX_ZERO;
    // search all opnd
    FOR_ALL_ELEM(opnd, phi_opnd_iter, Init()) {
      TY_IDX ty = Find_icall_ty_from_ivar_mu(dna, opnd, aux, visited_bb);
      if (ty == TY_IDX_ZERO || ty == ret_ty)
        continue;
      // if mismatch, return TY_IDX_ZERO
      if (ret_ty != TY_IDX_ZERO)
        break;
      ret_ty = ty;
    }
    return ret_ty;
  }
  else if (opnd->Is_flag_set(CF_DEF_BY_CHI)) {
    STMTREP *def = opnd->Defstmt();
    Is_True(def != NULL, ("bad def stmt"));
    if (OPERATOR_is_call(def->Opr())) {
      if ((def->Call_flags() & WN_CALL_IS_CONSTRUCTOR)) {
        Is_True(def->Rhs()->Kid_count() >= 1 &&
                def->Rhs()->Opnd(0)->Kind() == CK_IVAR &&
                def->Rhs()->Opnd(0)->Opr() == OPR_PARM, ("bad parm opnd"));
        if (def->Rhs()->Opnd(0)->Ilod_base()->Kind() == CK_LDA &&
            def->Rhs()->Opnd(0)->Ilod_base()->Lda_aux_id() == aux) {
          return def->Rhs()->Opnd(0)->Ilod_ty();
        }
      }
    }
    if (def->Opr() == OPR_OPT_CHI) {
      //Is_True(FALSE, ("TODO: not find def of vtable"));
      return TY_IDX_ZERO;
    }
    Is_True(opnd->Defchi() != NULL &&
            opnd->Defchi()->Live() &&
            opnd->Defchi()->RESULT() == opnd, ("bad def chi"));
    return Find_icall_ty_from_ivar_mu(dna, opnd->Defchi()->OPND(), aux, visited_bb);
  }
  else {
    STMTREP *def = opnd->Defstmt();
    if (def->Opr() == OPR_STID) {
      opnd = def->Rhs();
      if (opnd->Kind() == CK_LDA) {
      }
      else if (opnd->Kind() == CK_VAR) {
      }
      Is_True(FALSE, ("TODO: handle other opnd"));
      return TY_IDX_ZERO;
    }
    else {
      Is_True(FALSE, ("TODO: handle opr %s", OPERATOR_name(def->Opr()) + 4));
      return TY_IDX_ZERO;
    }
  }
}

// =============================================================================
//
// IPSA::Do_devirtualization() 
//
// =============================================================================
void
IPSA::Do_devirtualization(DNA_NODE* dna, STMTREP* stmt)
{
  Is_True(stmt->Call_flags() & WN_CALL_IS_VIRTUAL, ("Not a virtual call"));
  Is_True(stmt->Opr() == OPR_ICALL, ("Virtual call should be icall"));
  ST* call_st = NULL;  // st for function in vtable
  CODEREP *rhs = stmt->Rhs();
  CODEREP *icall_tg = rhs->Opnd(rhs->Kid_count() - 1);

  if (icall_tg->Kind() == CK_OP &&
      icall_tg->Opr() == OPR_INTRINSIC_OP &&
      icall_tg->Intrinsic() == INTRN_LOOKUP_VIRT_FUNC &&
      icall_tg->Kid_count() >= 2) {
    BOOL valid = icall_tg->Opnd(0)->Kind() == CK_IVAR &&
                 icall_tg->Opnd(0)->Op() == OPC_U8PARM &&
                 icall_tg->Opnd(1)->Kind() == CK_IVAR &&
                 icall_tg->Opnd(1)->Op() == OPC_U8PARM;

    Is_True(valid, ("Devirt: invalid operand structure for LOOKUP_VIRT_FUNC, "
                    "[1] = %d, [2] = %d",
                    icall_tg->Opnd(0)->Kind(),
                    icall_tg->Opnd(1)->Kind()));

    // Use the real target for icall_tg.
    if (valid) {
      icall_tg = icall_tg->Opnd(0)->Ilod_base();
    }
  }

  if (icall_tg->Kind() == CK_VAR) {
    // vtable is propagated (C++)
    ST* st = dna->Comp_unit()->Opt_stab()->Aux_stab_entry(icall_tg->Aux_id())->St();
    // If st is V-table's symbol (_ZTV*******) in Symbole_Table
    if (st && ST_is_vtable(st)) {
      char *cls_name = TY_name(ST_vtable_ty_idx(st));
      INT32 ofst = icall_tg->Offset();
      CLASS_HIERARCHY *cha = Glob_cha();
      VIRFUNC_INFO *vi = cha->Get_vtable_entry(cls_name, ST_name(st), ofst);
      if (vi != NULL) {
        call_st = Find_or_create_fun_st(vi->_file_idx, vi->_fun_st, dna->File_idx(), stmt->Ty());
        Is_True(ST_class(call_st) == CLASS_FUNC, ("virfunc is not CLASS_FUNC"));
        Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
                  (TFile, "Res vir fun: %s:%s(%d)\n", cls_name, ST_name(st), ofst));
      } else {
        Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
                  (TFile, "Unable to find vir fun: %s:%s(%d)\n",
                  cls_name, ST_name(st), ofst));
        return;
      }
    }
    else {
      Is_True(FALSE, ("st is not vtable"));
      return;
    }
  }
  else {
    Is_True(icall_tg->Kind() == CK_IVAR, ("IPSA::Do_devirtualization: icall_tg 's kind is not CK_IVAR, kind: %d", icall_tg->Kind()));
    CODEREP *icall_base = icall_tg->Ilod_base();
    if (PU_java_lang(Get_Current_PU()) && icall_base->Kind() == CK_VAR) {
      // vtable is propagated (JAVA)
      ST* st = dna->Comp_unit()->Opt_stab()->Aux_stab_entry(icall_base->Aux_id())->St();
      if (st && ST_is_class_symbol(st)) {
        // doesn't work with current java FE
        return;
        ST* vst = Get_vtab_entry(st, icall_base->Offset(), FALSE);
        if(vst) {
          Is_True(ST_is_vtable(vst), ("failed to get class entry"));
          call_st = Get_vtab_entry(vst, icall_tg->Offset(), FALSE);
        } else {
          Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
                   (TFile, "IPSA::Do_devirtualization symbol(v-table) "
                           "%s has INITO but failed to get its #%d entry,"
                           " bail out \n", ST_name(st), icall_base->Offset()));
        }
      }
      else {
        Is_True(FALSE, ("st is not class symbol"));
        return;
      }
    }
    else {
      // C++/C Routine.
      CODEREP *this_ptr = rhs->Opnd(0)->Ilod_base();
      if (this_ptr->Kind() == CK_LDA) {
        TY_IDX ty = TY_IDX_ZERO;
        MU_NODE *ivar_mu = icall_tg->Ivar_mu_node();
        if (ivar_mu != NULL) {
          // check constructor by follow ivar_mu U-D
          hash_set<IDTYPE> visited_bb;
          visited_bb.insert(stmt->Bb()->Id());
          ty = Find_icall_ty_from_ivar_mu(dna, ivar_mu->OPND(),
                                          this_ptr->Lda_aux_id(), visited_bb);
          Is_True(ty == TY_IDX_ZERO || TY_kind(ty) == KIND_POINTER,
                  ("lda ty is not pointer"));
        }
        if (ty == TY_IDX_ZERO) {
          // check this
          ty = rhs->Opnd(0)->Ilod_base()->Lda_ty();
          Is_True(TY_kind(ty) == KIND_POINTER, ("lda ty is not pointer"));
          if (TY_kind(TY_pointed(ty)) != KIND_STRUCT) {
            // object is constructed by placement new, try parm ty
            ty = rhs->Opnd(0)->Ilod_ty();
            Is_True(TY_kind(ty) == KIND_POINTER, ("lda ty is not pointer"));
            CLASS_HIERARCHY *cha = Glob_cha();
            CLASS_INFO *ci = cha->Get_class_info(TY_name(TY_pointed(ty)));
            if (ci != NULL && ci->Get_children()->size() > 0) {
              Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
                       (TFile, "IPSA::Do_devirtualization %s not leaf class.\n",
                               TY_name(TY_pointed(ty))));
              return;
            }
          }
        }
        TY_IDX pty = TY_pointed(ty);
        Is_True(TY_kind(pty) == KIND_STRUCT, ("pty is not struct"));
        ST_IDX sti = TY_vtable(pty);
        if (sti == ST_IDX_ZERO) {
          Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
                   (TFile, "IPSA::Do_devirtualization no v-table for %s.\n",
                           TY_name(pty)));
          return;
        }
        ST* vst = ST_ptr(sti);
        Is_True(vst != NULL && ST_is_vtable(vst), ("failed to get vtable st"));

        if (ST_sclass(vst) == SCLASS_EXTERN) {
          // and output message for bailing
          Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
                   (TFile, "IPSA::Do_devirtualization symbol(v-table) "
                           "%s is declared EXTERN, bail out \n", ST_name(vst)));
          return;
        }
        if (Find_INITO_For_Symbol(vst) == 0) {
          // Bail out : Cannot find v-table's INITO.
          Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
                   (TFile, "IPSA::Do_devirtualization symbol(v-table) "
                           "%s is not EXTERN, \n but sclass = %d, and "
                           "has no INITO, bail out \n",
                           ST_name(vst), ST_sclass(vst)));
          return;
        }
        // TODO: The following line may trigger ASSERTION when
        // icall_tg->Offset() is not found.

        // To be improved
        INT ofst = icall_tg->Offset();
        // skip virtual base offset, offset_to_top and rtti
        ofst += PU_java_lang(Get_Current_PU())
                ? (Is_Target_32bit()) ? 8 : 16
                : Get_base_offset_from_vtable(vst);
        call_st = Get_vtab_entry(vst, ofst, FALSE);
        Is_True(call_st != NULL,
                ("IPSA::Do_devirtualization, cannot locate function in V-Table(%0#x<%d>)\n", call_st));
      }
      else if (this_ptr->Kind() == CK_VAR && icall_tg->Offset() >= 0) {
        // unable to get the fun symbol when offset < 0
        // it may happen when call interface default implementation
        // do a simple U-D traversal here to find single target
        while (!this_ptr->Is_flag_set(CF_DEF_BY_PHI) &&
               !this_ptr->Is_flag_set(CF_DEF_BY_CHI)) {
          ID_ST_MAP::iterator iter = dna->Vtbl_map()->find(this_ptr->Coderep_id());
          if (iter != dna->Vtbl_map()->end()) {
            ST* vst = ST_ptr(iter->second);
            Is_True(vst != NULL && ST_is_vtable(vst), ("failed to get vtable st"));
            // To be improved
            INT ofst = icall_tg->Offset();
            // skip virtual base offset, offset_to_top and rtti
            ofst += PU_java_lang(Get_Current_PU())
                    ? (Is_Target_32bit()) ? 8 : 16
                    : Get_base_offset_from_vtable(vst);
            call_st = Get_vtab_entry(vst, ofst, FALSE);
            break;
          }
          STMTREP* sr = this_ptr->Defstmt();
          CODEREP* rhs = sr->Rhs();
          if (sr->Opr() == OPR_INTRINSIC_CALL &&
              (rhs->Intrinsic() == INTRN_ALLOC_OBJ ||
               rhs->Intrinsic() == INTRN_NEW_PRIM_ARR ||
               rhs->Intrinsic() == INTRN_NEW_OBJ_ARR ||
               rhs->Intrinsic() == INTRN_NEW_MULTI_ARR)) {
            Is_True(rhs->Opnd(0)->Ilod_base()->Kind() == CK_LDA, ("kid0 not LDA"));
            ST* cls_st = rhs->Opnd(0)->Ilod_base()->Lda_base_st();
            Is_True(cls_st != NULL && ST_is_class_symbol(cls_st), ("kid0 not class st"));
            ST* vst = Get_vtab_st_from_class(cls_st);
            Is_True(vst != NULL && ST_is_vtable(vst),
                    ("failed to get vtable st"));
            call_st = Get_vtab_entry(vst, icall_tg->Offset(), FALSE);
            break;
          }
          if (sr->Opr() != OPR_STID || sr->Rhs()->Kind() != CK_VAR)
            break;
          this_ptr = sr->Rhs();
        }
      }
    }
  }

  if (call_st != NULL) {
    Is_True(ST_class(call_st) == CLASS_FUNC, ("st is not func"));
    Convert_icall_to_call(dna, stmt, call_st);
  } else {
    Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
             (TFile, "IPSA::Do_devirtualization failed to get vtable for stmt: \n"));
    
    Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG), stmt->Print(TFile));
  }

#if 0
  else {
    Is_True( icall_tg->Kind() == CK_IVAR , ("Wrong icall target kind"));
    INT32 offset = icall_tg->Offset();
    CODEREP * base = icall_tg->Ilod_base();
    TY_IDX ty;
    if( base->Kind() == CK_IVAR)
      ty = base->Ilod_ty();
    else
      ty = base->Lod_ty();
    CLASS_HIERARCHY* hbuilder = Glob_cha(dna);
    VIRFUNC_INFO_VEC *cand_calls = hbuilder->Find_candidate_functions(ty, offset);
    if(cand_calls && cand_calls->size() == 1) {
      // May need to do devirtualization here,
      // But there would be 
    }
    else if (!cand_calls || cand_calls->size() == 0) {
      // TODO: need to report VTable out of range vulnerable issue here
    }
  }
#endif
}

// =============================================================================
// IPSA::Do_icall_promotion()
// =============================================================================
void
IPSA::Do_icall_promotion(DNA_NODE* dna, STMTREP* stmt)
{
  Is_True(stmt->Opr() == OPR_ICALL, ("not icall"));

  ST* call_st = NULL;  // st for function in vtable
  CODEREP *rhs = stmt->Rhs();
  CODEREP *icall_tg = rhs->Opnd(rhs->Kid_count() - 1);

  if (icall_tg->Kind() == CK_LDA) {
    call_st = icall_tg->Lda_base_st();
  }
  else {
    // TODO: local U-D traversal here? seems not necessary because COPY-PROP
  }

  if (call_st != NULL) {
    Convert_icall_to_call(dna, stmt, call_st);
  }

}

// =============================================================================
//
// Find_calls_and_side_effects: creates the callsite RNA for calls
//     it also create exit_mu, the counter part of Find_referenced_vars_at_entry
//     this part of the work will also include the list of touched heap_objects.
//
// =============================================================================
void
IPSA::Find_calls_and_side_effects(DNA_NODE *dna, BB_NODE *bb, STMTREP_STACK *oparam)
{
  //  Iterate through each statement
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP     *stmt;
  STMTREP     *prev_stmt = NULL;
  IDTYPE       which_param;
  UINT32       ret_cnt = 0;
  BOOL         is_ptr;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {
    if (stmt->Rhs() != NULL) {
      IDTYPE_SET visited_set;
      which_param = dna->Find_param_references(stmt->Rhs(), &is_ptr, IN_NONE, visited_set);
    }

    RNA_NODE *rna = NULL;
    OPERATOR opr = stmt->Opr();
    switch (opr) {
    case OPR_STID: {
      if (which_param == INVALID_VAR_IDX)
        which_param = dna->Is_param_copyin(stmt->Lhs(), stmt->Rhs());
      else if (stmt->Rhs()->Kind() == CK_VAR) {
        // process copy, if the rhs is a reference parameter
        if (! dna->Is_parm_original(which_param)) // parm redefined
          break;
        if (stmt->Next() == NULL ||
            !(stmt->Next()->Opr() == OPR_RETURN && dna->Comp_unit()->
              Opt_stab()->Aux_stab_entry(stmt->Lhs()->Aux_id())->Is_dedicated_preg())) {
          dna->Set_parm_flag(which_param, REF_COPIED);
        }
      }
      AUX_ID lhs_aux = stmt->Lhs()->Aux_id();
      if (dna->Is_aux_global(lhs_aux)) {
        Is_True(dna->Parm_list()->size() + lhs_aux < oparam->size(),
                ("aux id out of bound"));
        (*oparam)[dna->Parm_list()->size() + lhs_aux].push(stmt);

        // check if stmt is assignment to global function pointer
        Enter_global_fptr_assign(dna, stmt);
      }
    }  // OPR_STID
    break;
    case OPR_ISTORE: {
      CODEREP *base = stmt->Lhs()->Istr_base();
      TY_IDX istr_ty = stmt->Lhs()->Ilod_ty();
      if (TY_kind(istr_ty) == KIND_STRUCT && TY_vtable(istr_ty) != ST_IDX_ZERO &&
          stmt->Lhs()->Offset() == 0) {
        // this is a store to vptr
        CODEREP* p = Find_base_pointer_load(stmt->Rhs());
        if (p != NULL && p->Kind() == CK_LDA) {
          ST* st = p->Lda_base_st();
          if (ST_is_vtable(st)) {
            ST_IDX& vtbl = (*dna->Vtbl_map())[base->Coderep_id()];
            vtbl = ST_st_idx(st);  // overwrite the previous one if exists
          }
        }
      }
    } // OPR_ISTORE
    // fall thrw
    case OPR_MSTORE: {                                  
      CODEREP *base = stmt->Lhs()->Istr_base();
      IDTYPE_SET visited_set;
      IDTYPE   lhs_ref_param = dna->Find_param_references(base, &is_ptr, IN_ISTORE_BASE, visited_set);
      if (lhs_ref_param != INVALID_VAR_IDX &&
          base->Kind() != CK_IVAR &&
          dna->Is_parm_original(lhs_ref_param)) {
        Is_True(lhs_ref_param > 0 && lhs_ref_param < dna->Parm_list()->size(),
                ("lhs_ref_param out of range"));
        // it's a *param = ... or param[index] = ...
        // add to stack
        dna->Set_parm_flag(lhs_ref_param, REF_ISTORE);
        (*oparam)[lhs_ref_param].push(stmt);
      }
    } // OPR_MSTORE
    break;
    case OPR_RETURN:
    case OPR_RETURN_VAL:  {
      // do not add return stmt to return list if prev_stmt a call
      // with no-return flag
      if(prev_stmt != NULL && prev_stmt->Opr() == OPR_CALL) {
        ST *st = prev_stmt->St();
        if (PU_has_attr_noreturn((*Pu_Table_ptr)[ST_pu(st)])) {
          break;
        }
      }
      // Add to rets (return statememt) list
      dna->Rets_list()->push_back(stmt);
      // Create PDV_NODE for return value
      if (prev_stmt != NULL) {
        if (OPERATOR_is_scalar_store(prev_stmt->Opr()) && dna->Comp_unit()->
            Opt_stab()->Aux_stab_entry(prev_stmt->Lhs()->Aux_id())->Is_dedicated_preg()) {

          STMTREP *pdv_pred = dna->Branch_stmt_4_bb(bb).first;
          if (pdv_pred != NULL &&
              (pdv_pred->Opr() == OPR_TRUEBR || pdv_pred->Opr() == OPR_FALSEBR)) {
            Is_True(pdv_pred->Bb()->Next() != NULL, ("no fall through for truebr/falsebr"));
            // check the comparison condition
            CODEREP *cmp_var = NULL, *cmp_val = NULL;
            OPERATOR cmp_opr;
            if (pdv_pred->Rhs()->Kind() == CK_VAR) {
              // handle if (var)
              cmp_var = pdv_pred->Rhs();
              cmp_opr = OPR_NE;
              cmp_val = NULL;
            }
            else if (pdv_pred->Rhs()->Kind() == CK_OP &&
                     pdv_pred->Rhs()->Opnd(0)->Kind() == CK_VAR) {
              // handle if (var cmp val)
              cmp_var = pdv_pred->Rhs()->Opnd(0);
              cmp_opr = pdv_pred->Rhs()->Opr();
              cmp_val = pdv_pred->Rhs()->Opnd(1);
            }
            // check if pdv_pred is if (arg1 == 0) or if (arg1 != 0)
            if (cmp_var != NULL &&
                dna->Is_param(cmp_var) == 1 &&
                (cmp_opr == OPR_NE || cmp_opr == OPR_EQ) &&
                (cmp_val == NULL ||
                 (cmp_val->Kind() == CK_CONST && cmp_val->Const_val() == 0))) {
              // Now we have if (arg == 0) or if (arg != 0)
              BOOL fall_thr = pdv_pred->Bb()->Next() == bb ||
                              pdv_pred->Bb()->Next()->Dominates(bb);
              BOOL ret_zero = (prev_stmt->Rhs()->Kind() == CK_CONST &&
                               prev_stmt->Rhs()->Const_val() == 0);
              // adjust cmp_opr according to fall_thr and TRUEBR/FALSEBR
              if ((pdv_pred->Opr() == OPR_TRUEBR && fall_thr) ||
                  (pdv_pred->Opr() == OPR_FALSEBR && !fall_thr))
                cmp_opr = (cmp_opr == OPR_NE) ? OPR_EQ : OPR_NE;
              if ((cmp_opr == OPR_NE && !ret_zero) ||
                  (cmp_opr == OPR_EQ && ret_zero))
                dna->Set_flag(DNA_CHECK_NOT_ZERO);
              else
                dna->Set_flag(DNA_CHECK_ZERO);
            }
          }
          // The Rhs() is the operand of return value in source
          PDV_NODE *pnd =
            CXX_NEW(PDV_NODE((pdv_pred)? bb : NULL,
                             prev_stmt, BY_RETURNSTMT, INVALID_VAR_IDX),
                    dna->Comp_unit()->Mem_pool());
          pnd->Update_flags(dna);
          dna->Retv_list()->push_back(pnd);
          ++ret_cnt;
        }
      }

      // Create PDV_NODE for outout param
      INT i;
      for (i = 0; i < oparam->size(); ++i) {
        std::stack<STMTREP*>& s = (*oparam)[i];
        if (!s.empty()) {
          STMTREP* stmt = s.top();
          if (stmt == NULL)  // already added to retv/rgvl
            continue;
          Is_True(stmt != NULL &&
                  (stmt->Opr() == OPR_ISTORE || stmt->Opr() == OPR_MSTORE ||
                   stmt->Opr() == OPR_STID || stmt->Opr() == OPR_CALL),
                  ("invalid stmtrep in oparam stack"));

          PDV_NODE *pnd;
          if (i < dna->Parm_list()->size()) {
            Is_True(stmt->Opr() == OPR_ISTORE || stmt->Opr() == OPR_MSTORE ||
                    stmt->Opr() == OPR_CALL,
                    ("invalid stmtrep in oparam stack"));
            if (stmt->Opr() == OPR_ISTORE || stmt->Opr() == OPR_MSTORE)
              pnd = CXX_NEW(PDV_NODE(NULL, stmt, BY_PARAMETER, i),
                            dna->Comp_unit()->Mem_pool());
            else if (stmt->Opr() == OPR_CALL)
              pnd = CXX_NEW(PDV_NODE(NULL, stmt, BY_FREEIPARM, i),
                            dna->Comp_unit()->Mem_pool());
            dna->Retv_list()->push_back(pnd);
          }
          else {
            Is_True(stmt->Opr() == OPR_STID ||
                    stmt->Opr() == OPR_CALL,
                    ("invalid stmtrep in oparam stack"));
            if (stmt->Opr() == OPR_STID)
              pnd = CXX_NEW(PDV_NODE(NULL, stmt, BY_GLOBALVAR, 0),
                            dna->Comp_unit()->Mem_pool());
            else if (stmt->Opr() == OPR_CALL)
              pnd = CXX_NEW(PDV_NODE(NULL, stmt, BY_FREEGLOBL, 0),
                            dna->Comp_unit()->Mem_pool());
            dna->Rgvl_list()->push_back(pnd);
          }
          pnd->Update_flags(dna);
          // pop-up this stmt and push NULL so that it's only added once
          s.pop();
          s.push(NULL);
        }
      }
      // TODO: process exit_mu list 
      MU_LIST_ITER mu_iter;
      MU_NODE *mnode;
      FOR_ALL_NODE( mnode, mu_iter, Init(stmt->Mu_list())) {
      }
    } // OPR_RETURN , OPR_RETURN_VAL
    break;
    default: {
      if ( OPERATOR_is_call(opr)) {
        ST *st = NULL;
        if(stmt->Call_flags() & WN_CALL_IS_VIRTUAL) {
          Do_devirtualization(dna, stmt);
          opr = stmt->Opr(); // reset opr
        }
        else if (stmt->Opr() == OPR_ICALL) {
          Do_icall_promotion(dna, stmt);
          opr = stmt->Opr();
        }
        
        if (OPERATOR_has_sym(opr)) {
          st = stmt->St();
        }
        rna = New_rna( dna, stmt );

        if (st) {
          // set DNA side effect flags for all RBC builtin functions
          Rbc()->Process_rbc_builtin(this, dna, rna, st);

          // mark functions with malloc/free attribute
          if (stmt->Callee_frees_heap_memory()) {
            //CODEREP *actual_arg = stmt->Rhs()->Find_actual_arg();
            CODEREP *actual_arg = Vsa_free_ptr_opnd(stmt);
            IDTYPE_SET visited_set;
            IDTYPE   arg_param = dna->Find_param_references(actual_arg, &is_ptr, IN_NONE, visited_set, TRUE);
            // check if bb has control dependency
            BOOL maybe = bb->Rcfg_dom_frontier() != NULL && !bb->Rcfg_dom_frontier()->EmptyP();
            if (actual_arg->Kind() == CK_VAR && arg_param != INVALID_VAR_IDX) {
              dna->Set_deallocate(maybe);
              dna->Set_parm_flag(arg_param, REF_FREED);
              (*oparam)[arg_param].push(stmt); // add free call into stmt list
            }
            // the actual_arg might be a global variable
            IDTYPE   arg_glob   = dna->Find_glob_references(actual_arg, &is_ptr, IN_NONE);
            if (arg_glob != INVALID_VAR_IDX) {
              dna->Set_deallocate(maybe);
              dna->Set_glob_flag(arg_glob, REF_FREED);
              (*oparam)[arg_glob].push(stmt);  // add Free call into stmt list
            }
          }
          if (stmt->Callee_returns_new_heap_memory()) {
            dna->Set_flag(DNA_MEMORY_ALLOCATION);
            CODEREP* retv = dna->Comp_unit()->Find_return_value(stmt);
            if (retv != NULL)
              retv->Set_value_malloc();
          }
          if (PU_is_constructor(Pu_Table[ST_pu(st)])) {
            CODEREP* cr = stmt->Rhs()->Opnd(0)->Ilod_base();
            // TODO: for cr = this+ofst, get the field type
            if(cr->Kind() == CK_VAR || cr->Kind() == CK_LDA) {
              TY_IDX this_ty = cr->Kind() == CK_LDA ? cr->Lda_ty() : cr->object_ty();
              if (TY_kind(this_ty) != KIND_POINTER ||
                  TY_kind(TY_pointed(this_ty)) != KIND_STRUCT) // get TY from opnd(0) which is `this'
                this_ty = stmt->Rhs()->Opnd(0)->Ilod_ty();
              Is_True(TY_kind(this_ty) == KIND_POINTER, ("this not pointer"));
              TY_IDX cls_ty = TY_pointed(this_ty);
              if (TY_kind(cls_ty) == KIND_ARRAY)
                cls_ty = TY_etype(cls_ty);
              Is_True(TY_kind(cls_ty) == KIND_STRUCT, ("not struct"));
              ST_IDX stv = TY_vtable(cls_ty);
              if (stv != ST_IDX_ZERO) {
                Is_True(ST_is_vtable(ST_ptr(stv)), ("not vtable"));
                if (cr->Kind() == CK_LDA) {
                  CHI_NODE* chi = stmt->Chi_list()->Search_chi_node(cr->Lda_aux_id());
                  if (chi != NULL && chi->Live()) {
                    cr = chi->RESULT();
                  }
                  else {
                    MU_NODE* mu = stmt->Mu_list()->Search_mu_node(cr->Lda_aux_id());
                    Is_True(mu != NULL, ("can not find mu for LDA"));
                    cr = mu->OPND();
                  }
                }
                (*dna->Vtbl_map())[cr->Coderep_id()] = stv;
              }
            }
          }
          if(VSA_EH()) {
            STR_IDX eh_type = dna->Get_eh_throw_type(stmt);
            // We allow eh_type to be zero in case that we cannot find the
            // according thrown object (or if it's 'throw null'), in which case we skip.
            if (eh_type != 0) {
              dna->EH_table()->Add_eh_type(dna->File_idx(), eh_type);
            }
          }
        } // process call with symbol
      }
      break;
    } // default
    } // switch (opr)
    prev_stmt = stmt;
  } // for each statement
  if (ret_cnt == 1) dna->Set_flag(DNA_SINGLE_RETURN);

  BB_NODE      *dom_bb;
  BB_LIST_ITER  dom_bb_iter;
  FOR_ALL_ELEM (dom_bb, dom_bb_iter, Init(bb->Dom_bbs())) {
    Find_calls_and_side_effects(dna, dom_bb, oparam);  /* child */
  }

  // pop up STMTREP from oparam stack
  FOR_ALL_NODE_REVERSE(stmt, stmt_iter, Init()) {
    OPERATOR opr = stmt->Opr();
    switch (opr) {
    case OPR_STID:
      {
        AUX_ID lhs_aux = stmt->Lhs()->Aux_id();
        if (dna->Is_aux_global(lhs_aux)) {
          Is_True(dna->Parm_list()->size() + lhs_aux < oparam->size(),
                  ("aux id out of bound"));
          std::stack<STMTREP*>& s = (*oparam)[dna->Parm_list()->size() + lhs_aux];
          if (!s.empty()) {
            Is_True(s.top() == stmt || s.top() == NULL, ("stack corrupted"));
            s.pop();
          }
        }
      }
      break;
    case OPR_ISTORE:
    case OPR_MSTORE:
      {
        CODEREP *base = stmt->Lhs()->Istr_base();
        IDTYPE_SET visited_set;
        IDTYPE   lhs_ref_param = dna->Find_param_references(base, &is_ptr, IN_ISTORE_BASE, visited_set);
        if (lhs_ref_param != INVALID_VAR_IDX &&
            base->Kind() != CK_IVAR &&
            dna->Is_parm_original(lhs_ref_param)) {
          Is_True(lhs_ref_param > 0 && lhs_ref_param < dna->Parm_list()->size(),
                  ("lhs_ref_param out of range"));
          std::stack<STMTREP*>& s = (*oparam)[lhs_ref_param];
          if (!s.empty()) {
            Is_True(s.top() == stmt || s.top() == NULL, ("stack corrupted"));
            s.pop();
          }
        }
      }
      break;
    default:
      if ( OPERATOR_is_call(opr) && OPERATOR_has_sym(opr) &&
           stmt->St() && stmt->Callee_frees_heap_memory() ) {
        CODEREP *actual_arg = Vsa_free_ptr_opnd(stmt);
        IDTYPE_SET visited_set;
        IDTYPE   arg_param  = dna->Find_param_references(actual_arg, &is_ptr, IN_NONE, visited_set, TRUE);
        IDTYPE   arg_glob   = dna->Find_glob_references(actual_arg, &is_ptr, IN_NONE);
        IDTYPE   idx = (actual_arg->Kind() == CK_VAR && arg_param != INVALID_VAR_IDX) ?
                         arg_param : arg_glob;
        if (idx != INVALID_VAR_IDX) {
          std::stack<STMTREP*>& s = (*oparam)[idx];
          if (!s.empty()) {
            Is_True(s.top() == stmt || s.top() == NULL, ("stack corrupted"));
            s.pop();
          }
        }
      }
      break;
    }
  }
}

void
IPSA::Enter_function_dna(UINT32 file_idx, ST* st, IDTYPE id)
{
  _st.Put(file_idx, st, id);
}

// =============================================================================
// IPSA::Enter_global_fptr_assign()
// Keep record on assignment on global function pointer assignment for
// icall resolution
// =============================================================================
void
IPSA::Enter_global_fptr_assign(DNA_NODE *dna, STMTREP *sr)
{
  Is_True(dna != NULL && !dna->Non_functional(), ("bad dna"));
  Is_True(sr != NULL && OPERATOR_is_store(sr->Opr()), ("bad stmt"));

  FPTR_ASSGN_INFO info;
  ST *st = info.Analyze(this, dna, sr);
  if (st == NULL)
    return;
  Is_True(ST_level(st) == GLOBAL_SYMTAB, ("not global st"));

  UINT32 file_idx = dna->File_idx();
  WHIRL_FILE_MANAGER *mgr = WHIRL_FILE_MANAGER::Get();
  Is_True(file_idx == 0 || mgr != NULL, ("invalid xfa mode"));

  ST_IDX st_idx = ST_st_idx(st);
  if (mgr && ST_sclass(st) == SCLASS_EXTERN) {
    mgr->Resolve(file_idx, st_idx, file_idx, st_idx);
  }

  // add to map
  uint64_t key = ((uint64_t)file_idx << 32) | st_idx;
  FPTR_VECTOR * &vec = _fst_fptr_map[key];
  if (vec == NULL) {
    vec = CXX_NEW(FPTR_VECTOR(FPTR_ALLOCATOR(_mem_pool())),
                  _mem_pool());
    Is_True(vec != NULL, ("ipsa global fptr out of memory"));
  }
  vec->push_back(info);

  Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
           (TFile, "Enter_global_fptr_assign: add %s sr%d line %d to assign to %s ofst %d.\n",
                   dna->Fname(),
                   sr->Stmtrep_id(), Srcpos_To_Line(sr->Linenum()),
                   ST_name(file_idx, st_idx), info.Offset()));

}

// =============================================================================
//
// New_dna: create the dna node and append to the vector of dnaid to dnanode map
//          This function is called in PREOPT just before copy propagation, just
//          to create _stpath_map and _inlcxt_map
//
// =============================================================================
DNA_NODE*
IPSA::New_dna(COMP_UNIT *cu)
{

  Is_True( this != NULL, ("Illegal call to IPSA::New_dna(), IPSA not initialized"));
  WN         *wn    = cu->Input_tree();
  ST         *st    = WN_st(wn);
  IDTYPE      pu    = ST_pu(st);

  WHIRL_FILE_MANAGER* mgr = WHIRL_FILE_MANAGER::Get();
  if (mgr && File_Index <= mgr->Get_file_count())
    Set_Error_Source(mgr->Get_file(File_Index).File_name());

  IDTYPE      id    = _dnaid_to_dnanode.size();
  if (id != DNA_INIT_IDX && _dnaid_to_dnanode[id-1]->St() == st) {
    // avoid New_dna twice for one PU
    return _dnaid_to_dnanode[id];
  }

  Enter_function_dna(File_Index, st, id);

  DNA_NODE *retv = CXX_NEW(DNA_NODE(id, pu, st, ST_name_idx(st), cu, Mem_pool()), Mem_pool());
  _dnaid_to_dnanode.push_back(retv);
  retv->Set_file_idx(File_Index);
  cu->Set_dna(retv);
  retv->Set_cls_info(this);

  Rbc()->Insert_fname_dna_idx(retv->Fname(), retv->Dna_idx());
  Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
           (TFile, "%sIPSA::New_dna (func_name: %s, DNA id: %d) \n",SBar, retv->Fname(), id));
  // set phase to DNA_INIT
  _phase = DNA_INIT;
  return retv;
}

// =============================================================================
//
// IPSA::Init_dna: This function is call after MAINOPT, Node that this cu is a
//                 different one compare to the one this DNA_NODE is created
//
// =============================================================================

DNA_NODE*
IPSA::Init_dna(COMP_UNIT* cu)
{
  Is_True( this != NULL, ("Illegal call to IPSA::Init_dna(), IPSA not initialized"));
  WN         *wn    = cu->Input_tree();
  ST         *st    = WN_st(wn);
  IDTYPE      id    = _dnaid_to_dnanode.size();
  DNA_NODE   *dna   = _dnaid_to_dnanode[id -1];

  Is_True(id != DNA_INIT_IDX && dna->St() == st, ("DNA node is not created yet"));

  WHIRL_FILE_MANAGER* mgr = WHIRL_FILE_MANAGER::Get();
  if (mgr && File_Index <= mgr->Get_file_count())
    Set_Error_Source(mgr->Get_file(dna->File_idx()).File_name());

  // the old cu in dna is for PREOPT
  // now reset to cu for MAINOPT
  dna->Set_comp_unit(cu);
  dna->Collect_parm_list(cu->Input_tree());
  if (cu->Vsa() == NULL) {
    CXX_NEW(VSA(cu, this, Mem_pool(), Loc_pool()), Mem_pool());
  }
  // if cu has return value, use _parm_list[0] for return value annotations
  TY_IDX ret_ty = TY_ret_type(ST_pu_type(st));
  if (TY_mtype(ret_ty) != MTYPE_V)
    (*dna->Parm_list())[0] = CXX_NEW(VAR_NODE(ILLEGAL_AUX_ID, NULL), Mem_pool());

  // initialize eh table
  if (VSA_EH)
    cu->Build_eh_table();

  // go through entry chi to created reference list of this PU
  // which includes parm_list, glob_list, stat_list which maps stid to CODEREP
  if (cu->Cfg()->Fake_entry_bb() == NULL) {
    dna->Find_referenced_vars_at_entry(cu, cu->Cfg()->Entry_bb());
  }
  else {
    BB_NODE *entry_bb;
    BB_LIST_ITER bb_iter;
    FOR_ALL_ELEM (entry_bb, bb_iter, Init(cu->Cfg()->Fake_entry_bb()->Succ())) {
      dna->Find_referenced_vars_at_entry(cu, entry_bb);
    }
  }

  // process calls, to create call graph within file/object; collect Rhs() of a
  // return statement
  // oparam stack: first N for output param and the rest for global/static
  STMTREP_STACK oparam(dna->Parm_list()->size() +
                       dna->Comp_unit()->Opt_stab()->Lastidx() + 1);
  Find_calls_and_side_effects(dna, cu->Cfg()->Entry_bb(), &oparam);

#ifdef Is_True_On
  for (INT tmp = 0; tmp < oparam.size(); ++ tmp) {
    Is_True(oparam[tmp].empty(), ("stack corrupted"));
  }
#endif

  cu->Set_dna(dna);
  
  Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
           (TFile, "%sIPSA::Init_dna (func_name: %s, DNA id: %d) \n",SBar, ST_name(st), dna->Dna_idx()));
  Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG), dna->Print(TRUE, TFile));
  Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG), (TFile, "%s",SBar));

  // set phase back to DNA_CREATION
  _phase = DNA_CREATION;
  return dna;
}

// =============================================================================
//
// Prop_rbc_flags propogate callee rbc flag to rna flag
//
// =============================================================================
void
RNA_NODE::Prop_rbc_flags(IPSA *ipsa)
{
  UINT32 flags = 0;
  STMTREP *sr = Callstmt();
  CALLEE_VECTOR::const_iterator end = Callee_list().end();
  for (CALLEE_VECTOR::const_iterator it = Callee_list().begin();
       it != end; ++it) {
    DNA_NODE *callee = ipsa->Get_dna(it->Callee());
    if (callee == NULL)
      continue;
    DNODE_VECTOR *rbc_nodes = ipsa->Rbc()->Get_rbc_nodes(callee);
    if (rbc_nodes == NULL)
      continue;
    DNODE_VECTOR::const_iterator iter = rbc_nodes->begin();
    for (; iter != rbc_nodes->end(); iter++) {
      DNA_NODE *rbc_callee = *iter;
      if (callee->Is_set_rbc_flag(DNA_RBC_TAG_OP)) {
        flags |= RBC_SE_TAG_OP;
      }
      if (callee->Is_set_rbc_flag(DNA_RBC_MODEL)) {
        flags |= RBC_SE_MODEL;
      }
      if (callee->Is_set_rbc_flag(DNA_RBC_ASSERT)) {
        flags |= RBC_SE_ASSERT;
      }
    }
  }
  Set_flag(flags);
}
// =============================================================================
//
// Set_rbc_op set rbc_op by function name
//
// =============================================================================
void
RNA_NODE::Set_rbc_op(RBC_BASE *rbc, DNA_NODE *dna)
{
  CONTEXT_SWITCH ctx(dna);
  STMTREP *stmt = Callstmt();
  if (stmt) {
    if (stmt && stmt->Opr() == OPR_CALL) {
      ST *st = stmt->St();
      if (st) {
        const char *st_name = ST_name(st);
        RBC_OP op = rbc->Get_rbc_op(st_name);
        if (op != RBC_OP_NONE) {
          Set_rbc_op(op);
        }
      }
    }
  }
}

// =============================================================================
//
// Print function for tracing RNA node
//
// =============================================================================
void
RNA_NODE::Print(BOOL print_call_stmt, FILE *fp) const
{
  Is_True(g_ipsa_manager != NULL, ("Not in ipsa session"));
  DNA_NODE* dna = g_ipsa_manager->Get_dna(Caller_idx());
  CONTEXT_SWITCH context(dna);

  Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG), (TFile, "%s",SBar));
  if (Callee_st()) {
    char* callee_name = Vsa_demangle(ST_name(Callee_st()));
    fprintf(fp, "RNA_NODE(ID:%d) File_id: %d, Caller: %s(%d), Callee name:%s, Callee_id:%d Linenum:%d, RNA_FLAGS:0x%x\n",
            Rna_idx(), dna->File_idx(), dna->Fname(), Caller_idx(), callee_name, Uniq_callee(), Srcpos_To_Line(Linenum()), _flags);
    // ATTENTION: the memory was allocated in the Vsa_demangle, do not allocate memory by yourself
    if(callee_name)
      free(callee_name);
  }
  else {
    fprintf(fp, "RNA_NODE(ID:%d) File_id: %d, Caller: %s(%d), Callee name: NULL, Linenum:%d, RNA_FLAGS:0x%x\n",
            Rna_idx(), dna->File_idx(), dna->Fname(), Caller_idx(), Srcpos_To_Line(Linenum()), _flags);
    if(Is_indirect_call()) {
      fprintf(fp, "Possible callees : \n");
      for(CALLEE_VECTOR::const_iterator iter = _callee_list.begin(); iter != _callee_list.end(); iter++) {
        DNA_NODE* callee  = g_ipsa_manager->Get_dna(iter->Callee());
        char* fname = Vsa_demangle(callee->Fname());
        fprintf(fp, "Callee_name:%s, Callee_id:%d, Flags:0x%x\n",
                    fname, callee->Dna_idx(), iter->Flags());
        // ATTENTION: the memory was allocated in the Vsa_demangle, do not allocate memory by yourself
        if(fname)
          free(fname);
      }
    }
  }
  RBC_OP_SET op_set;
  RBC_OP_SET *rbc_ops = g_ipsa_manager->Rna_get_rbc_ops((RNA_NODE *)this, op_set);
  if (rbc_ops) {
    fprintf(fp, "  RBC ops:");
    RBC_OP_SET::iterator iter;
    for (iter = rbc_ops->begin(); iter != rbc_ops->end(); iter++) {
      fprintf(fp, "%s ", g_ipsa_manager->Rbc()->Rbc_op_name((RBC_OP)*iter));
    }
    fprintf(fp, "\n");
  }

  if (_arg_list.size() > VAR_INIT_ID) {
    fprintf(fp, "  Argument List:");
    for (INT i = VAR_INIT_ID; i < _arg_list.size(); ++i) {
      fprintf(fp, " Arg%d: cr%d ", i,
              ((_arg_list[i])->Cr() == NULL)?0:(_arg_list[i])->Cr()->Coderep_id());
      if ((_arg_list[i]->Flags() & ARG_LDA) != 0) fprintf(fp, "/arg_lda");
      if ((_arg_list[i]->Flags() & ARG_LDA_PT_UDEF) != 0) fprintf(fp, "/arg_lda_pt_udef");
      if ((_arg_list[i]->Flags() & ARG_PTR) != 0) fprintf(fp, "/arg_ptr");
      if ((_arg_list[i]->Flags() & ARG_PTR_PT_UDEF) != 0) fprintf(fp, "/arg_ptr_pt_udef");
      if ((_arg_list[i]->Flags() & ARG_VALUE_IVAD) != 0) fprintf(fp, "/arg_value_invalid_addr");
      if ((_arg_list[i]->Flags() & ARG_VALUE_MDANG) != 0) fprintf(fp, "/arg_value_maydangling");
      if ((_arg_list[i]->Flags() & ARG_VALUE_UDEF) != 0) fprintf(fp, "/arg_value_udef");
      if ((_arg_list[i]->Flags() & ARG_VALUE_MAYD) != 0) fprintf(fp, "/arg_value_mayd");
      if ((_arg_list[i]->Flags() & REF_ILOAD) != 0) fprintf(fp, "/ref_iload");
      if ((_arg_list[i]->Flags() & REF_ISTORE) != 0) fprintf(fp, "/ref_istore");
      if ((_arg_list[i]->Flags() & REF_COPIED) != 0) fprintf(fp, "/ref_copied");
      if ((_arg_list[i]->Flags() & REF_PTRPASSED) != 0) fprintf(fp, "/ref_ptrpassed");
      if ((_arg_list[i]->Flags() & REF_RETURN) != 0) fprintf(fp, "/ref_return");
      if ((_arg_list[i]->Flags() & REF_MALLOCED) != 0) fprintf(fp, "/ref_malloced");
      if ((_arg_list[i]->Flags() & REF_FREED) != 0) fprintf(fp, "/ref_freed");
    }
    fprintf(fp, "\n");
  }

  if (print_call_stmt) _callstmt->Print(fp);
}



// =============================================================================
//
// IPSA::~IPSA needs to free up all DNA and RNA nodes kept in the IPSA class
//
// =============================================================================
IPSA::~IPSA()
{
  for (INT i = DNA_INIT_IDX; i < _dnaid_to_dnanode.size(); ++i)
    CXX_DELETE(_dnaid_to_dnanode[i], Mem_pool());
  
  for (INT i = RNA_INIT_IDX; i < _rnaid_to_rnanode.size(); ++i)
    CXX_DELETE(_rnaid_to_rnanode[i], Mem_pool());
}

// =============================================================================
//
// IPSA_ST::IPSA_ST initialize file st vector
//
// =============================================================================
IPSA_ST::IPSA_ST(UINT32 min_size, MEM_POOL *mem_pool)
  : _tab(1, NULL, ST_ALLOCATOR(mem_pool)),
    _mpool(mem_pool)
{
  WHIRL_FILE_MANAGER *mgr = WHIRL_FILE_MANAGER::Get();
  if (mgr == NULL) {
    // cross file analysis off
    _tab[0] = CXX_NEW(ST_MAP(min_size, __gnu_cxx::hash<ST_IDX>(), std::equal_to<ST_IDX>(), HT_ALLOCATOR(mem_pool)), mem_pool);
    return;
  }
  const UINT32 fcnt = mgr->Get_file_count() + 1;
  _tab.resize(fcnt);
  _tab[0] = NULL;    // reserive _tab[0]
  for (UINT32 i = 1; i < fcnt; ++i) {
    _tab[i] = CXX_NEW(ST_MAP(min_size, __gnu_cxx::hash<ST_IDX>(), std::equal_to<ST_IDX>(), HT_ALLOCATOR(mem_pool)), mem_pool);
  }
}

// =============================================================================
//
// IPSA_ST::~IPSA_ST needs to free up all resources it owns
//
// =============================================================================
IPSA_ST::~IPSA_ST()
{
  for (UINT i = 0; i < _tab.size(); ++i) {
    CXX_DELETE(_tab[i], _mpool);
  }
  _tab.clear();
}

// =============================================================================
//
// IPSA_ST::Put (file_idx, st_idx) -> dna_id into map
//
// =============================================================================
void
IPSA_ST::Put(UINT32 file_idx, ST* st, IDTYPE dna_id)
{
  Is_True(file_idx < _tab.size(), ("file index %d out of bound", file_idx));
  ST_MAP *map = _tab[file_idx];
  Is_True(map != NULL, ("map for file index %d not initialized", file_idx));
  (*map).insert(ST_MAPPING(ST_index(st), dna_id));
}

// =============================================================================
//
// IPSA_ST::Get return dna_id from (file_idx, st_idx) -> dna_id into map
//
// =============================================================================
const IDTYPE
IPSA_ST::Get(UINT32 file_idx, ST* st)
{
  UINT32 st_idx = ST_st_idx(st);
  if (file_idx != 0 && ST_sclass(st) == SCLASS_EXTERN) {
    // resolve by linker
    if (WHIRL_FILE_MANAGER::Get()->Resolve(file_idx, st_idx, file_idx, st_idx) == FALSE)
      return INVALID_RNA_PU_IDX;
  }
  Is_True(file_idx < _tab.size(), ("file index %d out of bound", file_idx));
  ST_MAP *map = _tab[file_idx];
  Is_True(map != NULL, ("map for file index %d not initialized", file_idx));
  IDTYPE retv = (*map)[ST_IDX_index(st_idx)];
  return retv;
}

// =============================================================================
//
// IPSA_ST::Get return dna_id from (file_idx, st_idx) -> dna_id into map
// for an export_preemptible function
//
// =============================================================================
const IDTYPE
IPSA_ST::Get_xpreempt(UINT32 file_idx, ST_IDX st_idx, ST_SCLASS sclass)
{
  if (file_idx != 0 && sclass == SCLASS_EXTERN) {
    // resolve by linker
    if (WHIRL_FILE_MANAGER::Get()->Resolve(file_idx, st_idx, file_idx, st_idx) == FALSE)
      return INVALID_RNA_PU_IDX;
  }
  Is_True(file_idx < _tab.size(), ("file index %d out of bound", file_idx));
  ST_MAP *map = _tab[file_idx];
  Is_True(map != NULL, ("map for file index %d not initialized", file_idx));
  IDTYPE retv = (*map)[ST_IDX_index(st_idx)];
  return retv;
}

// =============================================================================
//
// IPSA::Collect_ty_kind
//
// =============================================================================
void
IPSA::Collect_ty_kind(const STMTREP* sr, vector<TY_KIND>& rna_ty)
{
  Is_True(OPERATOR_is_call(sr->Opr()), ("not call stmt"));
  CODEREP* call = sr->Rhs();
  INT kid_count = (sr->Opr() == OPR_ICALL) ? call->Kid_count() - 1:
                                             call->Kid_count();
  for (INT i = 0; i < kid_count; ++i) {
    CODEREP* cr = call->Opnd(i);
    Is_True(cr->Kind() == CK_IVAR && cr->Opr() == OPR_PARM, ("not parm"));
    cr = cr->Ilod_base();
    TY_IDX ty = cr->object_ty();
    if (ty == TY_IDX_ZERO)
      ty = MTYPE_To_TY(cr->Dtyp());
    rna_ty.push_back(TY_kind(ty));
  }
}

void
IPSA::Collect_ty_kind(const VNODE_VECTOR* vec, vector<TY_KIND>& rna_ty)
{
  VNODE_VECTOR::const_iterator end = vec->end();
  for (INT i = VAR_INIT_ID; i < vec->size(); ++i) {
    VAR_NODE* var = (*vec)[i];
    if (var->St_idx() != ST_IDX_ZERO) {
      TY_IDX ty = ST_type(var->St_idx());
      rna_ty.push_back(TY_kind(ty));
    }
    else if (var->Cr() != NULL) {
      TY_IDX ty = var->Cr()->object_ty();
      if (ty == TY_IDX_ZERO)
        ty = MTYPE_To_TY(var->Cr()->Dtyp());
      rna_ty.push_back(TY_kind(ty));
    }
    else {
      Is_True(FALSE, ("var node st and cr are both null"));
      rna_ty.push_back(KIND_INVALID);
    }
  }
}
// =============================================================================
//
// IPSA::Pu_type_is_equivarent
// 
// =============================================================================
void dump_ty(TY_IDX);
BOOL
IPSA::Pu_match_type(DNA_NODE *pu, const vector<TY_KIND>& call_ty, TY_IDX tyidx)
{
  Is_True(TY_kind(tyidx) == KIND_FUNCTION, ("ty should be a function type"));
  int fid = pu->File_idx();

  // if in the same file context, just compare the type idx
  if(fid == File_Index)   
    return ST_type(pu->St()) == tyidx; 
  {
    CONTEXT_SWITCH contex(pu);
    vector<TY_KIND> callee_ty;
    Collect_ty_kind(pu->Parm_list(), callee_ty);
    if (call_ty.size() != callee_ty.size())
      return FALSE;
    for (INT i = 0; i < call_ty.size(); ++i) {
      if (call_ty[i] != callee_ty[i])
        return FALSE;
    }
    return TRUE;
  }
#if 0
  // PU_prototype isn't reliable
  else {
    // FIXME: if arg type is pointer type, should find pointeed type name idx, compare pointer has no signification
    // 1. get the type vector for ty in current conext
    vector<int> vty1;
    TY &ty = Ty_Table[tyidx];
    TYLIST_IDX idx = ty.Tylist();
    TY_IDX ty_idx = Tylist_Table[idx];
    while(ty_idx != 0 ) {
      vty1.push_back(TY_kind(ty_idx));
      idx++;
      ty_idx = Tylist_Table[idx];
    }
    // 2. switch to contex of pu
    vector<int> vty2;
    CONTEXT_SWITCH contex(pu);
    TY &ty2 = Ty_Table[ST_type(pu->St())];
    idx = ty2.Tylist();
    ty_idx = Tylist_Table[idx];
    while(ty_idx != 0 ) {
      vty2.push_back(TY_kind(ty_idx));
      idx++;
      ty_idx = Tylist_Table[idx];
    }
    if(vty1.size() != vty2.size())
      return FALSE;

    for(int i=0; i < vty1.size(); i++)
      if(vty1[i] != vty2[i])
        return FALSE;
    return TRUE;
  }
#endif
}

// =============================================================================
//
// Connect_indirect_call(DNA_NODE*, RNA_NODE*, DNODE_VECTOR& addr_taken_funs)
//
// =============================================================================
void
IPSA::Connect_indirect_call(DNA_NODE* caller, RNA_NODE* rna, DNODE_VECTOR& addr_taken_funs)
{
  Is_True(rna->Is_indirect_call() && !rna->Is_virtual_call(), ("bad icall"));
  vector<TY_KIND> rna_ty;
  STMTREP* stmt = rna->Callstmt();
  Is_True(stmt && stmt->Opr() == OPR_ICALL, ("not icall"));
  INT icall_tgt_idx = stmt->Rhs()->Kid_count() - 1;
  CODEREP *icall_tgt = stmt->Rhs()->Opnd(icall_tgt_idx);
  if (icall_tgt->Kind() == CK_VAR &&
      caller->Is_aux_global(icall_tgt->Aux_id())) {
    OPT_STAB *stab = caller->Comp_unit()->Opt_stab();
    Is_True(stab != NULL, ("stab is null"));
    ST *st = stab->Aux_stab_entry(icall_tgt->Aux_id())->St();
    Is_True(st != NULL && ST_level(st) == GLOBAL_SYMTAB, ("invalid st"));

    UINT32 file_idx = caller->File_idx();
    WHIRL_FILE_MANAGER *mgr = WHIRL_FILE_MANAGER::Get();
    Is_True(file_idx == 0 || mgr != NULL, ("invalid xfa mode"));

    ST_IDX st_idx = ST_st_idx(st);
    if (mgr && ST_sclass(st) == SCLASS_EXTERN) {
      mgr->Resolve(file_idx, st_idx, file_idx, st_idx);
    }

    // check _fst_fptr_map
    uint64_t key = ((uint64_t)file_idx << 32) | st_idx;
    FST_FPTR_MAP::iterator it = _fst_fptr_map.find(key);
    if (it != _fst_fptr_map.end()) {
      INT32 ofst = icall_tgt->Offset();
      FPTR_VECTOR *vec = it->second;
      Is_True(vec != NULL, ("invalid null pointer in map"));
      for (FPTR_VECTOR::iterator finfo = vec->begin();
           finfo != vec->end(); ++finfo) {
        DNA_NODE *callee = NULL;
        if (ofst == finfo->Offset() &&
            (callee = finfo->Callee(this)) != NULL) {
          Is_True(!callee->Non_functional(), ("invalid callee"));
          rna->Add_callee(callee->Dna_idx(), ICALL_VAR_TARGET);
          callee->Inc_ref_cnt(rna);
          Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
                   (TFile, "Connect_indirect_call: Adding %s[%d] -> %s[%d] on sr%d line %d by global fptr assign\n",
                           caller->Fname(), caller->Dna_idx(),
                           callee->Fname(), callee->Dna_idx(),
                           stmt->Stmtrep_id(), Srcpos_To_Line(stmt->Linenum())));
        }
      }
      return;
    }
  }

  Collect_ty_kind(stmt, rna_ty);
  for (DNODE_VECTOR::iterator iter = addr_taken_funs.begin();
       iter != addr_taken_funs.end(); ++ iter) {
    DNA_NODE *callee = *iter;
    // do not add candidates for different language
    PU *caller_pu = Pu_ptr(caller->File_idx(), caller->Pu_idx());
    PU *callee_pu = Pu_ptr(callee->File_idx(), callee->Pu_idx());
    if(PU_src_lang(*caller_pu) != PU_src_lang(*callee_pu)) {
      continue;
    }
    if (PU_java_lang(*caller_pu))
    {
      // TODO: do demangle here, compare method name and arg list type
    }
    if (Pu_match_type(callee, rna_ty, stmt->Ty())) {
      rna->Add_callee(callee->Dna_idx(), ICALL_TYPE_TARGET);
      callee->Inc_ref_cnt(rna);
      Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
               (TFile, "Connect_indirect_call: Adding %s[%d] -> %s[%d] on sr%d line %d by type matching\n",
                       caller->Fname(), caller->Dna_idx(),
                       callee->Fname(), callee->Dna_idx(),
                       stmt->Stmtrep_id(), Srcpos_To_Line(stmt->Linenum())));
    }
  }
}

// =============================================================================
//
//  Get the virtual call offset & type name
//  For Java : from ICALL's last param if applicable.
//  For C/C++: from base pointer & last param
//
// =============================================================================
BOOL
IPSA::Get_virtual_call_ty_and_ofst(CODEREP *callcr, TY_IDX *p_ty_idx,
                                        INT32 *p_offset) {
  // This callee_name indicates the possibility of using the new offset.
  Is_True(callcr != NULL &&
          callcr->Kind() == CK_OP && callcr->Opr() == OPR_ICALL &&
          p_offset != NULL &&
          p_ty_idx != NULL, ("Invalid parameters, cannot write to those locations"));

  CLASS_HIERARCHY* hierarchy = Glob_cha();
  // Get called object type
  TY_IDX obj_ty = callcr->Opnd(0)->Ilod_base()->object_ty();

  // cr may be propagated to the icall's first param, look for the type from parm node.
  obj_ty = (obj_ty == TY_IDX_ZERO || TY_kind(obj_ty) != KIND_POINTER) ?
           callcr->Opnd(0)->object_ty() : obj_ty;

  if(obj_ty == TY_IDX_ZERO || TY_kind(obj_ty) != KIND_POINTER) {
    Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
             (TFile, "-DEVIRT: icall object type[%s] not pointer\n", obj_ty ? TY_name(obj_ty) : "null"));
    return FALSE;
  }

  // Extract the class type from pointer type
  obj_ty = TY_pointed(obj_ty);

  // for array class type, use java.lang.object type for devirtualization
  if (TY_is_array_class(obj_ty)) {
    UINT cur_field_id = 0;
    FLD_HANDLE fld = FLD_get_to_field(obj_ty, 2, cur_field_id);
    Is_True(!fld.Is_Null(), ("wrong fld at idx 2"));
    if (!fld.Is_Null()) {
      Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG), 
               (TFile, "-DEVIRT *Switch to Object type for array class type for %s\n",
                TY_name(obj_ty)));
      obj_ty = FLD_type(fld);
    }
  }

  if(TY_kind(obj_ty) != KIND_STRUCT) {
    Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
             (TFile, "-DEVIRT: icall object type %s is not struct\n", TY_name(obj_ty)));
    return FALSE;
  }

  *p_ty_idx = obj_ty;

  CODEREP *ild = callcr->Opnd(callcr->Kid_count() -1);
  // With devirtIndicator(LOOKUP_VIRT_FUNC), it's CK_OP,
  // Without devirtIndicator, it's CK_IVAR
  if (ild->Kind() != CK_IVAR) {
    if (ild->Kind() == CK_OP && Is_valid_lookup_virt_op(ild)) {
      // Get callee offset
      const char *callee_name = NULL;
      callee_name = Get_lookup_virt_mangled_function_name(ild);
      ild = Get_lookup_virt_original_target_info(ild);
      // Use the function name to find candidate callees, including type & offset.
      INT32 ofst_searched = hierarchy->Find_function_offset(callee_name, Loc_pool());
      if (ofst_searched == INVALID_VTABLE_OFFSET) {
        // This is allowed to happen due to the fact that we do not load
        // all the rt.o's symbols in and some could be missing.
        // Use original offset in this case, there is nothing gain
        // if we resolve this correctly.
        Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
                 (TFile, "-DEVIRT, Failed to resolve class info for callee %s \n", callee_name));
        return FALSE;
      } else {
        if (ofst_searched < 0) {
          ofst_searched = -ofst_searched; // In case of interface func
        }
        Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
                 (TFile, "-DEVIRT, Found offset for callee = %d \n", ofst_searched));
        // Checking if we are reassigning a different
        INT32 ofst_fe_generated = ild->Offset();
        if (ofst_fe_generated != ofst_searched) {
          // This would indicate that the offset given from JFE is inconsistent with what
          // we find from V-Table, likely the situation where the V-Table and ICALL is
          // generated against two incompatible classes, thus the ICALL's offset would
          // be different
          Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
                   (TFile, "-DEVIRTJAVA, Reassign offset = %d from CHA for %s,"
                    "old JFE give %d.", ofst_searched, callee_name, ofst_fe_generated));
        }
        // Return the results;
        if (p_offset)
          *p_offset = ofst_searched;
        return TRUE;
      }
    } else {
      // this is possible because the vtable is missing, (C/C++ cases)
      Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
               (TFile, "-DEVIRT  Assumption not met, found an LDID on ICall's func pointer, maybe v-table missing? kind = %d\n",
                (ild->Print((FILE*) TFile), ild->Kind()) ));
      return FALSE;
    }
  }
  INT32 offset = ild->Offset();
  CODEREP *base = ild->Ilod_base(); // The ILOAD in the middle
  TY_IDX ty;
  if (base->Kind() != CK_IVAR) {
    // should be resolved on IPSA::Do_devirtualization()
    // U-D would be simple enough to be resolved there, or
    // maybe the def is by param, and thus the de-virt is impossible.
    Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
             (TFile,
              "Assumption not met, ICALL's last param is not ILOAD "
              "even after LOOKUP_VIR_FUNC extraction, kind = %d \n",
              (base->Print((FILE*) TFile), base->Kind()) ));
    return FALSE;
  }
  else {
    // C++ way of handling ivar
    Is_True(base->Kind() == CK_IVAR, ("not ivar for vptr"));
    CODEREP *base_ptr = base->Ilod_base();
    // stid ptr 0
    //       ldid ptr
    //     iload 0
    //   iload offset
    // icall
    if (base_ptr->Kind() == CK_CONST) {
      return FALSE;
    }
    ty = base->Ilod_base()->object_ty();
    if (TY_kind(ty) != KIND_POINTER ||
        TY_kind(TY_pointed(ty)) != KIND_STRUCT) // get TY from opnd(0) which is `this'
      ty = callcr->Opnd(0)->Ilod_ty();
    //Is_True(ty == callcr->Opnd(0)->object_ty(),
    //        ("ty of this and vtable mismatch"));
    Is_True(TY_kind(ty) == KIND_POINTER, ("not pointer type"));
    ty = TY_pointed(ty);
    // TODO: do local type U-D to find a class type ?
    if (TY_kind(ty) != KIND_STRUCT) {
      Is_True(FALSE, ("Iload base type not handled, type name : %s, type kind : %d", TY_name(ty), TY_kind(ty)));
      return FALSE;
    }
  }

  // Assign to return variables.
  if (p_offset)
    *p_offset = offset;
  return TRUE;
}

// =============================================================================
//
// Connect_virtual_call(DNA_NODE* caller, RNA_NODE* rna)
// This is to find all the candidates to a call stmt inisde the caller
//
// =============================================================================
void
IPSA::Connect_virtual_call(DNA_NODE* caller, RNA_NODE* rna)
{
  Is_True(rna->Is_virtual_call(), ("bad call"));
  CLASS_HIERARCHY* hierarchy = Glob_cha();
  STMTREP* stmt = rna->Callstmt();
  CODEREP *callcr = stmt->Rhs();
  TY_IDX ty_idx = TY_IDX_ZERO;
  INT32 offset = 0;
  Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
           (TFile, "-DEVIRT Connecting virtual call, caller = %s\n",
             caller->Fname()));
  if (callcr->Kid_count() > 0 && Is_valid_lookup_virt_op(callcr->Opnd(callcr->Kid_count() -1))) {
    Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
             (TFile, "-DEVIRT Connecting virtual call, front-end marked callee = %s \n",
               Get_lookup_virt_mangled_function_name(callcr->Opnd(callcr->Kid_count() -1))));
  }
  // Get the ty_name and offset from icall, differ in C/C++ and Java
  // Please refer to the details in the design document.
  if (!Get_virtual_call_ty_and_ofst(callcr, &ty_idx, &offset)){
    // This is due to missing V-Table or CK_CONST in base pointer.
    Is_Trace (Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
              (TFile, "-DEVIRT Connect_virtual_call: get ty and offset failed. for func above = %d\n",
               (callcr->Print(TFile), 1)));
    return;
  }
  Is_True(ty_idx != TY_IDX_ZERO, ("ty idx is 0"));
  if (ty_idx == TY_IDX_ZERO) {
    Is_Trace (Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
              (TFile, "-DEVIRT Connect_virtual_call: get ty failed, TY_IDX_ZERO returned for func above = %d\n",
               (callcr->Print(TFile), 1)));
    return;
  }
  Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
           (TFile, "-DEVIRT Connecting virtual call, callee ty_name = %s, offset = %d \n",
            TY_name(ty_idx), offset));
  // if rna uses the same `this' as dna, caller and callee must be on the same
  // vtable. we get the caller_ofst and compare the function name on the same
  // offset on callee's vtable. callee is added only when the function name match
  INT caller_ofst = INVALID_VTABLE_OFFSET;
  CODEREP *first_parm = callcr->Opnd(0)->Ilod_base();
  if (first_parm->Kind() == CK_VAR &&
      caller->Is_param(first_parm) == 1) { // We only set caller_ofst when it's a 'this' pointer
    caller_ofst = hierarchy->Find_function_offset(caller->Fname(), Loc_pool()); // will return INVALID_VTABLE_OFFSET if not found.
  }

  const char *ty_name = TY_name(ty_idx);
  CAND_CALL_ITER iter(hierarchy, ty_name, offset, TRUE);
  for (; !iter.Is_end(); iter.Next()) {
    ST_IDX st_idx = iter.Curr_cand_st();
    IDTYPE file_idx = iter.Curr_file_idx();
    ST_SCLASS sclass = ST_sclass(St_ptr(file_idx, st_idx));
    ST_IDX vtbl_idx = iter.Curr_cand_vtable();
    Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
             (TFile, "-DEVIRT candidate name = %s \n", ST_name(file_idx, st_idx)));
    if (ST_java_abstract(St_ptr(file_idx, st_idx))) {
      Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
               (TFile, "-DEVIRT Connect_virtual_call: Found an abstract function = %0#x. \n",
                st_idx));
    }
    IDTYPE idx = _st.Get_xpreempt(file_idx, st_idx, sclass);
    if (idx == INVALID_RNA_PU_IDX) {
      Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
               (TFile, "-DEVIRT use rbc func name map to find dna idx for func %s, %d\n",
                ST_name(file_idx, st_idx), idx));
      idx = Rbc()->Get_dna_idx_by_name(ST_name(file_idx, st_idx));
    }
    if (idx != INVALID_RNA_PU_IDX) {
      DNA_NODE *callee = Get_dna(idx);
      //
      // Case
      // A            { foo() { this->bar();}  bar() { ... } } // this->bar should NOT have B::bar();
      // B : public A { foo() { this->bar();}  bar() { ... } } // 

      // Another Case
      // A            { foo() { this->bar();}  bar() { ... } }
      // B : public A { foo() { this->bar();} }                // this->bar should have A::bar();
      //
      if (caller_ofst != INVALID_VTABLE_OFFSET) { // this also means that this is a 'this' pointer
        // check if caller and callee are on the same vtable
        ST_IDX vtbl_idx = iter.Curr_cand_vtable();
        Is_True(vtbl_idx && ST_is_vtable(St_ptr(file_idx, vtbl_idx)),
                ("st is not vtable"));
        TY_IDX callee_class_ty = ST_vtable_ty_idx(*St_ptr(file_idx, vtbl_idx));
        Is_True(callee_class_ty != TY_IDX_ZERO, ("callee ty is invalid"));
        const char *callee_class_name = TY_name(file_idx, callee_class_ty);
        VIRFUNC_INFO *callee_class_vfuninfo_at_caller_offset = hierarchy->Get_vtable_entry(callee_class_name, caller_ofst);
        // ignore class hierarchy issue if caller_vfunc is NULL
        if (callee_class_vfuninfo_at_caller_offset == NULL) {
          Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
                   (TFile, "-DEVIRT Cannot find function at caller's offset in callee's class (%s) vtable,"
                    " on a this->xxx() call \n", callee_class_name));
          continue;
        }
        const char *callee_class_vfunc_name_at_caller_offset = ST_name(file_idx, callee_class_vfuninfo_at_caller_offset->Fun_st());
        if (strcmp(caller->Fname(), callee_class_vfunc_name_at_caller_offset) != 0) {
          Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
                   (TFile, "-DEVIRT Skipping overriden function %s at caller, impossible candidate\n",
                    callee_class_vfunc_name_at_caller_offset));
          continue;
        }
      }
      
      rna->Add_callee(callee->Dna_idx(), ICALL_DEVIRT_TARGET);
      callee->Inc_ref_cnt(rna);
      Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
               (TFile, "-DEVIRT Connect_virtual_call: Adding %s[%d] -> %s[%d]\n",
                caller->Fname(), caller->Dna_idx(),
                callee->Fname(), callee->Dna_idx()));
    } else {
      Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
               (TFile, "-DEVIRT Connect_virtual_call: Failed Adding [no dna] %s[%d] -> %s\n",
                caller->Fname(), caller->Dna_idx(),
                ST_name(file_idx, st_idx)));
    }
  } // end for
}

void
IPSA::Connect_interface_call(DNA_NODE *caller, RNA_NODE *rna)
{
  Is_True(rna->Is_interface_call(), ("bad call"));
  CODEREP *call_cr = rna->Callstmt()->Rhs();
  INT32 offset = 0;
  TY_IDX class_ty = TY_IDX_ZERO;
  if(!Get_intrfc_call_info(call_cr, offset, class_ty)) {
    Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG), 
             (TFile, "Connect_interface_call: unable to get interface info"));
    return;
  }
  CLASS_HIERARCHY* hierarchy = Glob_cha();
  CAND_CALL_ITER iter(hierarchy, TY_name(class_ty), offset, FALSE);
  for (; !iter.Is_end(); iter.Next()) {
    ST_IDX stidx = iter.Curr_cand_st();
    IDTYPE file_idx = iter.Curr_file_idx();
    ST_SCLASS sclass = ST_sclass(St_ptr(file_idx, stidx));
    IDTYPE idx = _st.Get_xpreempt(file_idx, stidx, sclass);
    if(idx != INVALID_RNA_PU_IDX) {
      DNA_NODE *callee = Get_dna(idx);
      rna->Add_callee(callee->Dna_idx(), ICALL_DEVIRT_TARGET);
      callee->Inc_ref_cnt(rna);
      Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
               (TFile, "Connect_interface_call: Adding %s[%d] -> %s[%d]\n",
                       caller->Fname(), caller->Dna_idx(),
                       callee->Fname(), callee->Dna_idx()));
    }
  }
}

BOOL
IPSA::Verify_found_virtual(RNA_NODE* call_site, DNA_NODE* callee) {
  Is_True(call_site->Is_indirect_call(), ("Call site is not indirect call."));
  STMTREP *stmt = call_site->Callstmt();
  INT n = stmt->Rhs()->Kid_count();
  CODEREP* cr = stmt->Rhs()->Opnd(n-1);
  TY_IDX clazz_type = TY_IDX_ZERO;
  INT32 offset = 0;
  CLASS_HIERARCHY* hierarchy = Glob_cha();
  // verify java de-virtualization
  if ((PU_src_lang(Get_Current_PU()) & PU_JAVA_LANG) && call_site->Is_virtual_call()) {
    // virtual call
    // Possible verification
    // 1. Verify type match, base pointer and callee function names's mangled base class.
    // 2. Verify type match, ICALL's invocation method type and callee function class
    // 3. Verify callee inside V-Table / candidates
    // 4. Verify callee inside method table / ...
    // ..... To be added .....
    if (Is_valid_lookup_virt_op(cr)) {
      // New Java way
      if (!Get_virtual_call_ty_and_ofst(stmt->Rhs(), &clazz_type, &offset)) {
        // This is due to missing V-Table or CK_CONST in base pointer.
          Is_True(FALSE, ("Verify_found_virtual cannot get clazz name "
                          "from callcr = %s\n", cr->Print_str(TRUE)));
        return FALSE;
      }
    } else if (cr->Kind() == CK_IVAR &&
               cr->Ilod_base()->Kind() == CK_IVAR) {
      // old Java way
      if ((call_site->Arg_list()->size() - 1) != callee->Parm_list()->size()) {
        Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
                 (TFile, "-DEVIRT resolved failed, call site arg num: %ld, callee %s parm size : %ld\n", call_site->Arg_list()->size(), callee->Fname(), callee->Parm_list()->size()));
        return FALSE;
      }
      int offset = cr->Offset();
      CODEREP *cr_ldid = cr->Ilod_base()->Ilod_base();
      TY_IDX ty = cr_ldid->object_ty();
      clazz_type = TY_pointed(ty);
    }
    if (clazz_type == TY_IDX_ZERO || offset <= 0) {
      Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
               (TFile, "-DEVIRT error, cannot get virtual call clazz_name or"
                "offset, clazz_type = %d, clazz_name = %s, offset = %d\n",
                clazz_type, clazz_type ? TY_name(clazz_type) : "n/a", offset));
      return TRUE;
    }
    CLASS_INFO *clazz_info = hierarchy->Get_class_info(TY_name(clazz_type));
    if (clazz_info != NULL) {
      VIRFUNC_INFO *func_info = clazz_info->Get_vtable_entry(offset);
      STRING right_st_name = callee->Fname();
      if (func_info != NULL) {
        STRING left_st_name = ST_name(func_info->_file_idx, func_info->_fun_st);
        if (strcmp(left_st_name, right_st_name) == 0) {
          return TRUE;
        }
        // TODO demangle func name, and compare
      } else {
        // TODO get method name from method table, and compare
      }
    }
  }
  return TRUE;
}

// =============================================================================
//
// IPSA::Resolved_indirect_and_virtual_call
//       Objective: Verify the correctness of devirtualization.
//       What: Find the definition (new statement in Java) of the
//             base pointer of ICALL stmt to check if:
//              the type of the candidate callees (previously found)
//              are a match to
//              the ones of the definition (new stmt)
//       How: By using traversing U-D chain to find the definition and
//              then use class hierarchy to verify the two type's relations.
//              see opt_vsa_icall.cxx Find_icall_defs_new();
//
// =============================================================================
void
IPSA::Resolved_indirect_and_virtual_call(DNA_NODE *func, DNODE_VECTOR& addr_taken_funs) {
  for (INT i = RNA_INIT_IDX; i < func->Call_list()->size(); i++) {
    RNA_NODE *call_site = (*func->Call_list())[i];
    // This indicates that we have located the definition(s) of the icall's base pointer. (new stmt)
    // We could then use them to verify the candidate callees.
    BOOL definition_found = FALSE;
    UINT32 devir_flag = ICALL_VAR_TARGET;
    if (!call_site->Is_indirect_call()) {
      continue;
    }
    STMTREP* stmt = call_site->Callstmt();
    INT n = stmt->Rhs()->Kid_count();
    CODEREP* cr = stmt->Rhs()->Opnd(n-1);

    Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
              (TFile, "DEVIRT the following ICALL:\n"));
    Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
                  func->Comp_unit()->Vsa()->Print_sr(stmt, TFile));

    ICALL_TARGET_VECTOR defs;
    BOOL always_trim = FALSE;
    if (call_site->Is_virtual_call()) {
      // for the vcall pattern
      //    LDID U8 U8 sym4v4 0 ty=4103  <u=3 cr24//  flags:0x8 b=-1 #a
      //   U8PARM ty=4103  <u=0 cr27//  flags:0x0 b=-1 mu<//
      //     LDID U8 U8 sym4v4 0 ty=4103  <u=3 cr24//  flags:0x8 b=-1 #a
      //    U8U8ILOAD 0 ty=3c03  <u=0 cr28//  flags:0x0 b=-1 mu<9/cr26//
      //   U8U8ILOAD 0 ty=4b03  <u=0 cr29//  flags:0x0 b=-1 mu<9/cr26//
      //  VICALL <u=0 cr30//  isop_flags:0x0 flags:0x1 b=-1
      if (PU_java_lang(Get_Current_PU()) && call_site->Callee_list().size() <= 1) {
        // skip verification if no candidate found or only 1 found.
        // call_site->Callee_list().size() == 0
        //    indicates failed devirtualization   --> skip verification
        // call_site->Callee_list().size() == 1
        //    gives us only one option left, nothing we can do if it's wrong
        //    thus, we could assume it's correct and
        //    use the existing candidate onwards.  --> skip verification
        Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
                  (TFile, "DEVIRT: only one candidate found, skip verification\n"));
        definition_found = FALSE;
      } else if ((cr->Kind() == CK_OP && Is_valid_lookup_virt_op(cr)) ||  // new java way
                  (cr->Kind() == CK_IVAR && cr->Ilod_base()->Kind() == CK_IVAR)) { // c++ or old java way
        // c++ behavior(with Experimental = true) or Java behavior
        definition_found = func->Comp_unit()->Vsa()->Find_icall_defs_new(stmt, defs);
      } else if (cr->Kind() == CK_VAR && PU_cxx_lang(Get_Current_PU())) {
        // Trying to resolve by vtable + offset from global class hierarchy
        ST* st = func->Comp_unit()->Opt_stab()->Aux_stab_entry(cr->Aux_id())->St();
        if (st && ST_is_vtable(st)) {
          char *cls_name = TY_name(ST_vtable_ty_idx(st));
          INT32 ofst = cr->Offset();
          CLASS_HIERARCHY *cha = Glob_cha();
          VIRFUNC_INFO *vi = cha->Get_vtable_entry(cls_name, ST_name(st), ofst);
          if (vi != NULL) {
            ST *fun_st = St_ptr(vi->_file_idx, vi->_fun_st);
            Is_True(ST_class(fun_st) == CLASS_FUNC, ("virfunc is not CLASS_FUNC"));
            defs.push_back(std::make_pair(vi->_file_idx, vi->_fun_st));
            Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG), 
                      (TFile, "Res vir fun: %s:%s(%d)\n", cls_name, ST_name(st), ofst));
            definition_found = TRUE;
          } else {
            Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
                      (TFile, "Unable to find vir fun: %s:%s(%d)\n",
                      cls_name, ST_name(st), ofst));
          }
        }
      }
    } // call_site->Is_virtual_call()
    else if (call_site->Is_interface_call()) {
      definition_found = func->Comp_unit()->Vsa()->Find_icall_defs_new(stmt, defs);
      devir_flag =  devir_flag | ICALL_DEVIRT_TARGET;
    } // callsite is interface_call
    else {
      definition_found = func->Comp_unit()->Vsa()->Find_icall_defs_new(stmt, defs);
      devir_flag =  devir_flag | ICALL_DEVIRT_TARGET;
      // always trim candidates for indirect call (not virturl/interface call)
      // to reduce invalid candidates
      always_trim = TRUE;
    } // call_site is not virutal or interface call

    Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
              (TFile, " - Resolved: %s\n", definition_found ? "yes" : "no"));
    Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
              (TFile, " - Num of target: %ld\n", defs.size()));

    // remove old target at first, do this temporary. probably we should do this after all icall processed
    if (always_trim) {
      call_site->Trim(this);
    }
    // Is_True(defs.size() > 0, ("inconsistent definition_found's value "));
    if (definition_found && defs.size() > 0)
    {
      call_site->Trim(this);
      call_site->Set_flag(RNA_ICALL_RESOLVED);
      for (ICALL_TARGET_VECTOR::const_iterator iter = defs.begin(); iter != defs.end(); iter++) {
        ST *fun_st = St_ptr(iter->first, iter->second);
        IDTYPE idx = _st.Get(iter->first, fun_st);
        // if there don't exist a callee dna, find rbc node that this method overrided then apply
        if (idx < DNA_INIT_IDX) {
          idx = Rbc()->Get_dna_idx_by_name(ST_name(iter->first, fun_st));
        }
        if (idx >= DNA_INIT_IDX) {
          DNA_NODE *callee = _dnaid_to_dnanode[idx];
          // TO do: check for all kind of calls
          BOOL verify = Verify_found_virtual(call_site, callee);
          Is_True(verify, ("Call site does not match callee, false de-virtualization, sr%d, dna %s.",
            call_site->Callstmt()->Stmtrep_id(), callee->Fname()));
          if (verify) {
            call_site->Add_callee(callee->Dna_idx(), devir_flag);
            callee->Inc_ref_cnt(call_site);
            Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
                      (TFile, "IPSA::Resolved_indirect_and_virtual_call: Adding call %d -> %d \n",
                      call_site->Caller_idx(), callee->Dna_idx()));
          }
        }
        else {
          if (call_site->Is_virtual_call() || call_site->Is_interface_call()) {
            Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
                      (TFile, " - Fail: not find DNA for %s\n", ST_name(iter->first, iter->second)));
          } else {
            // TODO:: add an external call
          }
        }
      }
    } // if definition_found
    else {
      Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
                (TFile, " -DEVIRT : Resolved_indirect_and_virtual_call :"
                "cannot resolve the definition of base pointer (new stmt) for icall for %d func.\n",
                (call_site->Print((FILE*) TFile), 1)));
      if (VSA_Check_Devirt) {
        Is_True(FALSE, ("Can't find icall."));
      }
    }
  } // for
}

// =============================================================================
// IPSA::Resolved_register_call
// Process register related calls: thread_create(bind thread work function)/signal
// (bind signal handler function)
// Find the rna, follow the U-D of function pointer to find the entry and mark
// =============================================================================
void
IPSA::Resolved_register_call(DNA_NODE *func, DNODE_VECTOR& addr_taken_funs)
{
  COMP_UNIT *cu = func->Comp_unit();
  VSA *vsa = cu->Vsa();
  Is_True(vsa != NULL, ("vsa not initialized"));

  for (INT i = RNA_INIT_IDX; i < func->Call_list()->size(); i++) {
    RNA_NODE *rna = (*func->Call_list())[i];
    Is_True(rna != NULL && rna->Callstmt() != NULL, ("bad rna"));
    STMTREP *sr = rna->Callstmt();
    CODEREP *thread_fn = vsa->Callee_create_new_thread(sr);
    CODEREP *signal_fn = thread_fn ? NULL : vsa->Callee_register_signal_handler(sr);
    CODEREP *cr = thread_fn ? thread_fn : signal_fn;
    if (cr == NULL || cr->Kind() == CK_CONST || cr->Kind() == CK_RCONST)
      continue;

    if (cr->Kind() == CK_LDA) {
      // handle LDA
      Is_Trace(Get_Trace(TP_VSA, VSA_REG_TRACE_FLAG),
               (TFile, "REGISTER: find register entry LDA %s in %s line %d.\n",
                        ST_name(cr->Lda_base_st()),
                        func->Fname(), Srcpos_To_Line(rna->Linenum())));
      IDTYPE idx = _st.Get(func->File_idx(), cr->Lda_base_st());
      if (idx >= DNA_INIT_IDX) {
        if (thread_fn) {
          BOOL many = cu->Is_stmt_in_loop(sr);
          Mark_thread_entry(idx, many);
        } else {
          Is_True(signal_fn, ("null signal handle function pointer"));
          Mark_isr_entry(idx);
        }
      }
      else {
        Is_Trace(Get_Trace(TP_VSA, VSA_REG_TRACE_FLAG),
                 (TFile, "REGISTER: not find register entry %s DNA in %s line %d.\n",
                         ST_name(func->File_idx(), cr->Lda_base_st()),
                         func->Fname(), Srcpos_To_Line(rna->Linenum())));
      }
      continue;
    }

    Is_True(cr->Kind() == CK_VAR ||
            (cr->Kind() == CK_IVAR && cr->Opr() != OPR_PARM), ("bad thr func ptr"));

    ICALL_TARGET_VECTOR defs;
    if (vsa->Find_registered_function(sr, cr, defs) == TRUE) {
      BOOL many = cu->Is_stmt_in_loop(sr);
      // handle defs in ICALL_TARGET_VECTOR
      for (ICALL_TARGET_VECTOR::const_iterator iter = defs.begin();
           iter != defs.end(); ++iter) {
        ST *fun_st = St_ptr(iter->first, iter->second);
        Is_Trace(Get_Trace(TP_VSA, VSA_REG_TRACE_FLAG),
                 (TFile, "REGISTER: find register entry U-D %s in %s line %d.\n",
                          ST_name(iter->first, fun_st),
                          func->Fname(), Srcpos_To_Line(rna->Linenum())));
        IDTYPE idx = _st.Get(iter->first, fun_st);
        if (idx >= DNA_INIT_IDX) {
          if (thread_fn) {
            Mark_thread_entry(idx, many);
          } else {
            Is_True(signal_fn, ("null signal handle function pointer"));
            Mark_isr_entry(idx);
          }
        }
        else {
          Is_Trace(Get_Trace(TP_VSA, VSA_REG_TRACE_FLAG),
                   (TFile, "REGISTER: not find register entry %s DNA in %s line %d.\n",
                           ST_name(iter->first, fun_st),
                           func->Fname(), Srcpos_To_Line(rna->Linenum())));
        }
      }
    }
    else {
      Is_Trace(Get_Trace(TP_VSA, VSA_REG_TRACE_FLAG),
               (TFile, "REGISTER: not find register entry by U-D in %s line %d.\n",
                       func->Fname(), Srcpos_To_Line(rna->Linenum())));
    }
  }
}

// =============================================================================
// IPSA::Mark_thread_entry
// Find DNA from file_idx and func ST and mark it to be thread entry
// =============================================================================
void
IPSA::Mark_thread_entry(UINT32 dna_idx, BOOL many)
{
  Is_True_Ret(dna_idx >= DNA_INIT_IDX && dna_idx < Dna_count(),
              ("dna idx out of bound"));
  DNA_NODE *dna = Get_dna(dna_idx);
  dna->Set_rbc_flag(DNA_THREAD_ENTRY);
  dna->Set_flag(many ? DNA_IN_WORK_THREAD : DNA_IN_HELP_THREAD);
  Is_Trace(Get_Trace(TP_VSA, VSA_REG_TRACE_FLAG),
           (TFile, "THREAD: set %d %s %s thread entry.\n",
                   dna_idx, dna->Fname(),
                   many ? "worker" : "helper"));
}

// =============================================================================
// IPSA::Mark_isr_entry
// =============================================================================
void
IPSA::Mark_isr_entry(UINT32 dna_idx)
{
  Is_True_Ret(dna_idx >= DNA_INIT_IDX && dna_idx < Dna_count(),
              ("dna idx out of bound"));
  DNA_NODE *dna = Get_dna(dna_idx);
  dna->Set_rbc_flag(DNA_ISR_ENTRY);
  dna->Set_flag(DNA_IN_ISR);
  Is_Trace(Get_Trace(TP_VSA, VSA_REG_TRACE_FLAG),
           (TFile, "SIGNAL: set %d %s handler entry.\n",
                   dna_idx, dna->Fname()));
}

void
IPSA::Identify_vsym_field_info(DNA_NODE *func)
{
  for(INT i = RNA_INIT_IDX; i < func->Call_list()->size(); i++) {
    RNA_NODE *call_site = (*func->Call_list())[i];
    VSA *vsa = func->Comp_unit()->Vsa();
    if(Is_jni_call(call_site)) {
      CODEREP *obj_cr = NULL;
      CODEREP *fld_cr = NULL;
      for (int i = VAR_INIT_ID; i <= call_site->Arg_cnt(); ++i) {
        if(call_site->Is_set_arg_flag(i, REF_BASE)) {
          Is_True(!obj_cr, ("duplicate REF_BASE flag"));
          obj_cr = obj_cr ? NULL : call_site->Get_arg(i);
        } else if(call_site->Is_set_arg_flag(i, REF_FLDNM)) {
          Is_True(!fld_cr, ("duplicate REF_BASE flag"));
          fld_cr = fld_cr ? NULL : call_site->Get_arg(i);
        }
      }
      if(obj_cr != NULL && fld_cr != NULL) {
        vsa->Resolve_vsym_fld_name(call_site->Callstmt(), obj_cr, fld_cr);
      }
    }
  }
}


// =============================================================================
// Enter_rbc_assign
// record implicit assign info on param/ret value
// =============================================================================
void
IPSA::Enter_rbc_assign(IDTYPE rna_idx, IDTYPE tgt, IDTYPE src)
{
  ASSIGN_INFO *info = Rna_2_assign_info(rna_idx);
  if(info == NULL) {
    info = CXX_NEW(ASSIGN_INFO(Mem_pool()), Mem_pool());
  }
  info->Enter_assign(tgt, src);
  _rbc_assign_infos.Insert(rna_idx, info);
}

// =============================================================================
// Get_assign_src
// 
// =============================================================================
SRC_VECTOR *
IPSA::Get_assign_src(RNA_NODE *rna, CODEREP *tgt)
{
  ASSIGN_INFO *info = Rna_2_assign_info(rna->Rna_idx());
  if(!info) {
    return NULL;
  }
  IDTYPE tgt_idx = rna->Get_arg_with_cr(tgt);
  // tgt canbe parameter or ret value, if not parm or ret return null
  if(tgt_idx == INVALID_VAR_IDX) {
    DNA_NODE *caller = Get_dna(rna->Caller_idx());
    if(tgt != caller->Comp_unit()->Find_return_value(rna->Callstmt())) {
      return NULL;
    }
  }
  return info->Get_assign_src(tgt_idx);
}

// =============================================================================
// class IPSA_CONTEXT_FOR_GLOBAL
// manage global symbols accessed by all DNA
// =============================================================================

class IPSA_CONTEXT_FOR_GLOBAL {
private:
  struct DNA_CONTEXT_FOR_GLOBAL {
  public:
    FST_AUX_MAP  *_fst_use_map; // global <file, st> to local aux used in func
    FST_AUX_MAP  *_fst_aux_map; // map global <file, st> to local aux
    FST_FST_MAP  *_fst_fst_map; // map global <file, st> to local <file, st>
    AUX_DEF_MAP  *_aux_to_add;  // aux refed in callee but not in caller
    AUX_DEF_MAP  *_aux_to_upd;  // aux refed in callee and also in caller
    AUX_SET_MAP  *_aux_set_map; // aux refed at each callsite
    UINT32        _orig_lastidx;// orginal aux lastidx

  public:
    FST_AUX_MAP  *Fst_use_map() const { return _fst_use_map; }
    FST_AUX_MAP  *Fst_aux_map() const { return _fst_aux_map; }
    FST_FST_MAP  *Fst_fst_map() const { return _fst_fst_map; }
    AUX_DEF_MAP  *Aux_to_add() const  { return _aux_to_add;  }
    AUX_DEF_MAP  *Aux_to_upd() const  { return _aux_to_upd;  }
    AUX_SET_MAP  *Aux_set_map() const { return _aux_set_map; }
    UINT32        Orig_lastidx() const{ return _orig_lastidx;}
    void          Print(FILE* fp) const;
  };

  MEM_POOL                _gpool;
  MEM_POOL                _lpool;
  DNA_CONTEXT_FOR_GLOBAL *_dcg_vector;
  IPSA                   *_ipsa;
  UINT32                  _dcg_size;

public:
  IPSA_CONTEXT_FOR_GLOBAL(IPSA* ipsa) : _ipsa(ipsa), _dcg_size(ipsa->Dna_count())
  {
    OPT_POOL_Initialize(&_gpool, "IPSA context global pool", FALSE, VSA_DUMP_FLAG);
    OPT_POOL_Push(&_gpool, VSA_DUMP_FLAG);
    _dcg_vector = TYPE_OPT_POOL_ALLOC_N(DNA_CONTEXT_FOR_GLOBAL, &_gpool,
                                        _dcg_size, VSA_DUMP_FLAG);
    Is_True(_dcg_vector != NULL, ("failed to create dcg vector"));
    BZERO(_dcg_vector, sizeof(DNA_CONTEXT_FOR_GLOBAL) * _dcg_size);

    OPT_POOL_Initialize(&_lpool, "IPSA context local pool", FALSE, VSA_DUMP_FLAG);
  }

  ~IPSA_CONTEXT_FOR_GLOBAL()
  {
    Is_True(_dcg_vector == NULL, ("dcg vector not freed"));
  }

  void Destroy()
  {
    _dcg_vector = NULL;

    OPT_POOL_Delete(&_lpool, VSA_DUMP_FLAG);

    OPT_POOL_Pop(&_gpool, VSA_DUMP_FLAG);
    OPT_POOL_Delete(&_gpool, VSA_DUMP_FLAG);
  }

public:
  BOOL         Tracing() const
  {
    return Get_Trace(TP_WOPT2, VSA_DUMP_FLAG);
  }

  FST_AUX_MAP* Fst_use_map(DNA_NODE* dna) const
  {
    Is_True(dna->Dna_idx() < _dcg_size, ("dna idx oob"));
    return _dcg_vector[dna->Dna_idx()].Fst_use_map();
  }

  FST_AUX_MAP* Fst_aux_map(DNA_NODE* dna) const
  {
    Is_True(dna->Dna_idx() < _dcg_size, ("dna idx oob"));
    return _dcg_vector[dna->Dna_idx()].Fst_aux_map();
  }

  FST_FST_MAP* Fst_fst_map(DNA_NODE* dna) const
  {
    Is_True(dna->Dna_idx() < _dcg_size, ("dna idx oob"));
    return _dcg_vector[dna->Dna_idx()].Fst_fst_map();
  }

  AUX_DEF_MAP* Aux_to_add(DNA_NODE* dna) const
  {
    Is_True(dna->Dna_idx() < _dcg_size, ("dna idx oob"));
    return _dcg_vector[dna->Dna_idx()].Aux_to_add();
  }

  AUX_DEF_MAP* Aux_to_upd(DNA_NODE* dna) const
  {
    Is_True(dna->Dna_idx() < _dcg_size, ("dna idx oob"));
    return _dcg_vector[dna->Dna_idx()].Aux_to_upd();
  }

  AUX_SET_MAP* Aux_set_map(DNA_NODE* dna) const
  {
    Is_True(dna->Dna_idx() < _dcg_size, ("dna idx oob"));
    return _dcg_vector[dna->Dna_idx()].Aux_set_map();
  }

  UINT32 Orig_lastidx(DNA_NODE* dna) const
  {
    Is_True(dna->Dna_idx() < _dcg_size, ("dna idx oob"));
    return _dcg_vector[dna->Dna_idx()].Orig_lastidx();
  }

  void Collect_callee_globals(DNA_NODE* dna, const DNA_CONTEXT_FOR_GLOBAL& dcg);

  void Collect_dna_context(DNA_NODE* dna)
  {
    Is_True(dna->Dna_idx() < _dcg_size, ("dna idx oob"));
    DNA_CONTEXT_FOR_GLOBAL& dcg = _dcg_vector[dna->Dna_idx()];
    Is_True(dcg.Fst_use_map() == NULL && dcg.Fst_aux_map() == NULL &&
            dcg.Orig_lastidx() == 0,
            ("context already collected"));
    dcg._fst_use_map = CXX_NEW(FST_AUX_MAP(41,
                                           __gnu_cxx::hash<FILE_ST_IDX>(),
                                           std::equal_to<FILE_ST_IDX>(),
                                           FST_AUX_ALLOCATOR(&_gpool)),
                               &_gpool);
    // collect fst_use_map
    VSA* vsa = dna->Comp_unit()->Vsa();
    STMTREP* entry = vsa->Get_entry_chi_stmt();
    Is_True(entry != NULL && entry->Opr() == OPR_OPT_CHI, ("bad entry chi"));
    vsa->Collect_stmt_chi_globals(dna->File_idx(), entry, dcg.Fst_use_map());
    dcg._orig_lastidx = vsa->Opt_stab()->Lastidx();

    // push mempool
    OPT_POOL_Push(&_lpool, VSA_DUMP_FLAG);
    dcg._fst_aux_map = CXX_NEW(FST_AUX_MAP(41,
                                           __gnu_cxx::hash<FILE_ST_IDX>(),
                                           std::equal_to<FILE_ST_IDX>(),
                                           FST_AUX_ALLOCATOR(&_lpool)),
                               &_lpool);
    dcg._fst_fst_map = CXX_NEW(FST_FST_MAP(41,
                                           __gnu_cxx::hash<FILE_ST_IDX>(),
                                           std::equal_to<FILE_ST_IDX>(),
                                           FST_FST_ALLOCATOR(&_lpool)),
                               &_lpool);
    dcg._aux_to_add = CXX_NEW(AUX_DEF_MAP(41,
                                          __gnu_cxx::hash<AUX_ID>(),
                                          std::equal_to<AUX_ID>(),
                                          AUX_DEF_ALLOCATOR(&_lpool)),
                              &_lpool);
    dcg._aux_to_upd = CXX_NEW(AUX_DEF_MAP(41,
                                          __gnu_cxx::hash<AUX_ID>(),
                                          std::equal_to<AUX_ID>(),
                                          AUX_DEF_ALLOCATOR(&_lpool)),
                              &_lpool);
    dcg._aux_set_map = CXX_NEW(AUX_SET_MAP(41,
                                           __gnu_cxx::hash<AUX_ID>(),
                                           std::equal_to<AUX_ID>(),
                                           AUX_SET_ALLOCATOR(&_lpool)),
                                &_lpool);
    // collect fst_fst_map, fst_aux_map
    vsa->Build_local_fst_map(dcg.Fst_fst_map());
    vsa->Build_local_aux_map(dcg.Fst_aux_map());

    Collect_callee_globals(dna, dcg);

    Is_Trace(Tracing(), (TFile, "GLOBAL CONTEXT for %s:%d\n",
                                dna->Fname(), dna->Dna_idx()));
    Is_Trace_cmd(Tracing(), dcg.Print(TFile));
  }

  void Destroy_dna_context(DNA_NODE* dna)
  {
    Is_True(dna->Dna_idx() < _dcg_size, ("dna idx oob"));
    DNA_CONTEXT_FOR_GLOBAL& dcg = _dcg_vector[dna->Dna_idx()];
    Is_True(dcg.Fst_use_map() != NULL && dcg.Fst_aux_map() != NULL,
            ("context not collected"));
    dcg._fst_aux_map = NULL;
    dcg._fst_fst_map = NULL;
    dcg._aux_to_add = NULL;
    dcg._aux_to_upd = NULL;
    dcg._aux_set_map = NULL;
    OPT_POOL_Pop(&_lpool, VSA_DUMP_FLAG);
  }

};

void
IPSA_CONTEXT_FOR_GLOBAL::DNA_CONTEXT_FOR_GLOBAL::Print(FILE* fp) const
{
  fprintf(fp, "+ GLOBAL VARIABLE used: %s\n", _fst_use_map ? "" : "-nil-");
  if (_fst_use_map != NULL) {
    for (FST_AUX_MAP::const_iterator iter = _fst_use_map->begin();
         iter != _fst_use_map->end(); ++ iter) {
      UINT32 file_idx = FST_file_idx(iter->first);
      ST_IDX st_idx = FST_st_idx(iter->first);
      AUX_ID aux = iter->second;
      fprintf(fp, " - %s:%d:0x%x --> sym%d\n",
                  ST_name(file_idx, st_idx), file_idx, st_idx, aux);
    }
  }

  fprintf(fp, "+ GLOBAL VARIABLE - AUX map: %s\n", _fst_aux_map ? "" : "-nil-");
  if (_fst_aux_map != NULL) {
    for (FST_AUX_MAP::const_iterator iter = _fst_aux_map->begin();
         iter != _fst_aux_map->end(); ++ iter) {
      UINT32 file_idx = FST_file_idx(iter->first);
      ST_IDX st_idx = FST_st_idx(iter->first);
      AUX_ID aux = iter->second;
      fprintf(fp, " - %s:%d:0x%x --> sym%d\n",
                  ST_name(file_idx, st_idx), file_idx, st_idx, aux);
    }
  }

  fprintf(fp, "+ GLOBAL VARIABLE - LOCAL VARIABLE map: %s\n", _fst_fst_map ? "" : "-nil-");
  if (_fst_fst_map != NULL) {
    for (FST_FST_MAP::const_iterator iter = _fst_fst_map->begin();
         iter != _fst_fst_map->end(); ++ iter) {
      UINT32 g_file_idx = FST_file_idx(iter->first);
      ST_IDX g_st_idx = FST_st_idx(iter->first);
      UINT32 l_file_idx = FST_file_idx(iter->second);
      ST_IDX l_st_idx = FST_st_idx(iter->second);
      fprintf(fp, " - %s:%d:0x%x --> %s:%d:0x%x\n",
                  ST_name(g_file_idx, g_st_idx), g_file_idx, g_st_idx,
                  ST_name(l_file_idx, l_st_idx), l_file_idx, l_st_idx);
    }
  }

  fprintf(fp, "+ AUX to be added: %s\n", _aux_to_add ? "" : "-nil-");
  if (_aux_to_add != NULL) {
    for (AUX_DEF_MAP::const_iterator iter = _aux_to_add->begin();
         iter != _aux_to_add->end(); ++ iter) {
      AUX_ID aux = iter->first;
      BB_LIST* list = iter->second;
      fprintf(fp, " - sym%d -->", aux);
      BB_NODE* bb;
      BB_LIST_ITER bb_iter;
      FOR_ALL_ELEM (bb, bb_iter, Init(list)) {
        fprintf(fp, " BB%d", bb->Id());
      }
      fprintf(fp, "\n");
    }
  }

  fprintf(fp, "+ AUX to be updated: %s\n", _aux_to_upd ? "" : "-nil-");
  if (_aux_to_upd != NULL) {
    for (AUX_DEF_MAP::const_iterator iter = _aux_to_upd->begin();
         iter != _aux_to_upd->end(); ++ iter) {
      AUX_ID aux = iter->first;
      BB_LIST* list = iter->second;
      fprintf(fp, " - sym%d -->", aux);
      BB_NODE* bb;
      BB_LIST_ITER bb_iter;
      FOR_ALL_ELEM (bb, bb_iter, Init(list)) {
        fprintf(fp, " BB%d", bb->Id());
      }
      fprintf(fp, "\n");
    }
  }

  fprintf(fp, "+ AUX updated at each callsite: %s\n", _aux_set_map ? "" : "-nil-");
  if (_aux_set_map != NULL) {
    for (AUX_SET_MAP::const_iterator iter = _aux_set_map->begin();
         iter != _aux_set_map->end(); ++ iter) {
      IDTYPE sr = iter->first;
      AUX_SET* aux = iter->second;
      fprintf(fp, " - sr%d -->%s", sr, _aux_set_map ? "" : " -nil-\n");
      if (aux != NULL) {
        for (AUX_SET::const_iterator iter = aux->begin();
             iter != aux->end(); ++ iter) {
          fprintf(fp, " sym%d", *iter);
        }
        fprintf(fp, "\n");
      }
    }
  }
}

void
IPSA_CONTEXT_FOR_GLOBAL::Collect_callee_globals(DNA_NODE* dna, const DNA_CONTEXT_FOR_GLOBAL& dcg)
{
  WHIRL_FILE_MANAGER* mgr = WHIRL_FILE_MANAGER::Get();
  VSA* vsa = dna->Comp_unit()->Vsa();

  FST_AUX_MAP* caller_use = dcg.Fst_use_map();
  FST_AUX_MAP* caller_aux = dcg.Fst_aux_map();
  FST_FST_MAP* caller_fst = dcg.Fst_fst_map();

  AUX_SET_MAP* cs_aux_map = dcg.Aux_set_map();
  Is_True(cs_aux_map != NULL, ("callsite aux list is null"));

  INT32 last_idx = vsa->Opt_stab()->Lastidx();

  for (INT i = IPSA::RNA_INIT_IDX; i < dna->Call_list()->size(); ++i) {
    RNA_NODE* rna = (*dna->Call_list())[i];
    STMTREP* stmt = rna->Callstmt();
    if (stmt->Opr() != OPR_CALL && stmt->Opr() != OPR_ICALL)
      continue;

    Is_True(cs_aux_map->find(stmt->Stmtrep_id()) == cs_aux_map->end(),
            ("callsite aux set is NULL"));
    AUX_SET* callsite_aux = NULL;

    // check if globals used in parameter
    INT parm_count = stmt->Opr() == OPR_ICALL ? rna->Arg_list()->size() - 1 :
                                                rna->Arg_list()->size();
    for (INT parm = VAR_INIT_ID; parm < parm_count; parm++) {
      VAR_NODE *arg = rna->Arg_list()->at(parm);
      if ((arg->Flags() & (REF_ILOAD | REF_ISTORE)) == 0)
        continue;
      CODEREP* cr = arg->Cr();
      if (cr->Kind() != CK_LDA && cr->Kind() != CK_VAR)
        continue;   // Probably need to check OP and IVAR
      if (!Is_struct_ptr(cr))
        continue;
      AUX_ID aux_id = cr->Kind() == CK_LDA ? cr->Lda_aux_id() : cr->Aux_id();
      AUX_STAB_ENTRY* entry = vsa->Opt_stab()->Aux_stab_entry(aux_id);
      if (!entry->Is_global())
        continue;
      if (entry->Is_volatile())
        continue;
      if (callsite_aux == NULL) {
        callsite_aux = CXX_NEW(AUX_SET(41,
                                       __gnu_cxx::hash<AUX_ID>(),
                                       std::equal_to<AUX_ID>(),
                                       AUX_DEF_ALLOCATOR(&_lpool)),
                               &_lpool);
        cs_aux_map->insert(make_pair(stmt->Stmtrep_id(), callsite_aux));
      }
      callsite_aux->insert(aux_id);
      AUX_DEF_MAP* to_upd = dcg.Aux_to_upd();
      BB_LIST* &bbs = (*to_upd)[aux_id];
      if (bbs == NULL || bbs->Node()->Id() != stmt->Bb()->Id())
        bbs = bbs->Prepend(stmt->Bb(), &_lpool);
      // if no chi, insert a chi
      CHI_NODE* chi = stmt->Chi_list()->Search_chi_node(aux_id);
      if (chi == NULL) {
        chi = stmt->Chi_list()->New_chi_node(aux_id, dna->Comp_unit()->Mem_pool());
        CODEREP* zero_cr = entry->Zero_cr();
        Is_True(zero_cr != NULL, ("no zero cr"));
        chi->Set_OPND(zero_cr);
        chi->Set_RESULT(zero_cr);
      }
    }

    // check if callee update globals
    for(CALLEE_VECTOR::const_iterator iter = rna->Callee_list().begin();
        iter != rna->Callee_list().end(); iter++) {
      if (iter->Flags() & RNA_BACK_EDGE)
        continue;  // ignore back edge???
      DNA_NODE* callee = _ipsa->Get_dna(iter->Callee());
      if (callee == NULL || callee->Non_functional())
        continue;  // ignore RBC model function
      FST_AUX_MAP* callee_aux = Fst_use_map(callee);
      Is_True(callee_aux != NULL, ("callee globals is empty"));

      for (FST_AUX_MAP::iterator i = callee_aux->begin();
           i != callee_aux->end(); ++ i) {
        if (callsite_aux == NULL) {
          callsite_aux = CXX_NEW(AUX_SET(41,
                                         __gnu_cxx::hash<AUX_ID>(),
                                         std::equal_to<AUX_ID>(),
                                         AUX_DEF_ALLOCATOR(&_lpool)),
                                 &_lpool);
          cs_aux_map->insert(make_pair(stmt->Stmtrep_id(), callsite_aux));
        }

        FILE_ST_IDX fst = i->first;
        FST_AUX_MAP::iterator j = caller_aux->find(fst);
        if (j != caller_aux->end()) {
          callsite_aux->insert(j->second);
          AUX_DEF_MAP* to_upd = j->second > last_idx ? dcg.Aux_to_add() : dcg.Aux_to_upd();
          BB_LIST* &bbs = (*to_upd)[j->second];
          if (bbs == NULL || bbs->Node()->Id() != stmt->Bb()->Id())
            bbs = bbs->Prepend(stmt->Bb(), &_lpool);
        }
        else {
          BOOL create = TRUE;
          UINT32 def_file = FST_file_idx(fst);
          ST_IDX def_st = FST_st_idx(fst);
          AUX_ID local = vsa->Find_or_create_local_aux(caller_aux, caller_fst, def_file, def_st, create);
          if (create) {
            Is_Trace(Tracing(),
                     (TFile, "Collect_callee_globals: create aux %d in %s:%d:%d for aux %d st %s in %s:%d:%d.\n",
                             local,
                             dna->Fname(), dna->Dna_idx(), dna->File_idx(),
                             i->second, ST_name(def_file, def_st),
                             callee->Fname(), callee->Dna_idx(), callee->File_idx()));
            // add to caller's Fst_use_map
            (*caller_use)[fst] = local;
          }
          callsite_aux->insert(local);
          AUX_DEF_MAP* map = local > last_idx ? dcg.Aux_to_add() : dcg.Aux_to_upd();
          BB_LIST* &bbs = (*map)[local];
          if (bbs == NULL || bbs->Node()->Id() != stmt->Bb()->Id())
            bbs = bbs->Prepend(stmt->Bb(), &_lpool);
        }
      }
    }
  }
}

#include "../../include/gnu/demangle.h"
extern "C" char *cplus_demangle (const char *, int);
#include <stdlib.h>

void
IPSA::Build_vtable_name_set(MEM_POOL *pool)
{
  JAVA_CLASS_HIERARCHY_HELPER::Init(pool);
  NAME_SET *rbc_class_sym_set = JAVA_CLASS_HIERARCHY_HELPER::Get_class_sym_set();
  NAME_SET *rbc_vtable_sym_set = JAVA_CLASS_HIERARCHY_HELPER::Get_vtable_sym_set();
  for (INT i = DNA_INIT_IDX; i < _dnaid_to_dnanode.size(); ++i) {
    DNA_NODE *dna = _dnaid_to_dnanode[i];
    CONTEXT_SWITCH context(dna);
    // ingore all functional dna
    if (!dna->Non_functional()) {
      continue;
    }
    // only handle java language for now
    if (!(PU_src_lang(*Pu_ptr(dna->File_idx(), dna->Pu_idx())) & PU_JAVA_LANG)) {
      continue;
    }
    // non-static function required vtable
    if (dna->Parm_list()->size() <= 1) {
      continue;
    }
    VAR_NODE *func_parm = (*dna->Parm_list())[1];
    ST *func_parm_st = ST_ptr(func_parm->St_idx());
    Is_True(func_parm_st != NULL, ("DNA first parm st is NULL, DNA : %s", dna->Fname()));
    if (!ST_is_this_ptr(func_parm_st)) {
      continue;
    }
    char *fname = dna->Fname();
    char *demangled_name = cplus_demangle(fname, 0);
    if (demangled_name == NULL) {
      return;
    }
    int len = strlen(fname) + 8;
    char *vtable_sym_name = (char *) MEM_POOL_Alloc(pool, len);
    char *class_sym_name = (char *) MEM_POOL_Alloc(pool, len);
    memset(vtable_sym_name, 0, len);
    memset(class_sym_name, 0, len);
    // find "::" in demangled name, CLASS_NAME::METHOD_NAME
    int d_len = strlen(demangled_name);
    int colon = d_len - 1;
    for (; colon >= 1; colon--) {
      if (demangled_name[colon] == ':') {
        if (demangled_name[colon-1] == ':') {
          ++colon;
          break;
        }
      }
    }
    if (colon > 1 && colon != d_len - 1) {
      char *sub_str = (char *) MEM_POOL_Alloc(pool, d_len - colon + 5);
      sprintf(sub_str, "%d", d_len - colon);
      strcat(sub_str, demangled_name + colon);
      char *func_name = strstr(fname, sub_str);
      if (func_name) {
        memcpy(class_sym_name, fname, func_name - fname);
        strcat(class_sym_name, "6class$E");
        strcat(vtable_sym_name, "_ZTV");
        memcpy(vtable_sym_name + 4, fname + 2, func_name - fname - 2);
        strcat(vtable_sym_name, "E");
        rbc_class_sym_set->insert(class_sym_name);
        rbc_vtable_sym_set->insert(vtable_sym_name);
      }
    }
    // ATTENTION: the memory was allocated in the Vsa_demangle, do not allocate memory by yourself
    free(demangled_name);
  }
}

// =============================================================================
//
// IPSA::Link, perform linker function
//      Output: Root nodes can be idenfied if (DNA_NODE::_ref_cnt == 0)
//              All visible functions will be resolved;
//      TODO:   unresolved symbol list, could be created during this time
//
// =============================================================================
void
IPSA::Link(void)
{
  if (Show_Progress) fprintf(stderr, "linking ........\n");

  SET_OPT_PHASE("Collect Address Taken");
  // collect addr_taken functions to a vector
  hash_set<IDTYPE> visited_files;
  DNODE_VECTOR addr_taken_funs; 
  for (INT i = DNA_INIT_IDX; i < _dnaid_to_dnanode.size(); ++i) {
    DNA_NODE *func = _dnaid_to_dnanode[i];
    Is_True(func->Caller_cnt() == 0, ("func already has clby"));
    if(ST_addr_saved(func->St()) || ST_addr_passed(func->St())) {
      func->Set_flag(DNA_ADDR_TAKEN);
      addr_taken_funs.push_back(func);
      Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
               (TFile, "Add func %s(%d) to address taken vector\n",
                       func->Fname(), func->Dna_idx()));
    }
  }

  SET_OPT_PHASE("Perform Symbol Resolution");
  Rbc()->Build_rbc_func_name_map(this);
  // 1. Perform symbol resolution for all call sites
  for (INT i = DNA_INIT_IDX; i < _dnaid_to_dnanode.size(); ++i) {
    DNA_NODE *func = _dnaid_to_dnanode[i];
    Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
             (TFile, "%sIPSA::Analyze, resolve callee for function: %s\n%s",SBar,func->Fname(),SBar));
    // this context switch is useful for
    // 1. printf in Show_Progress to get the ST_name()
    // 2. create class hierarchy
    CONTEXT_SWITCH context(func);
    for (INT j = RNA_INIT_IDX; j < func->Call_list()->size(); ++j) {
      // resolve callee_idx
      RNA_NODE *call_site = (*func->Call_list())[j];
      STMTREP *call_stmt = call_site->Callstmt();
      Is_True(call_stmt != NULL, ("invalid callstmt"));
      BOOL called_in_loop = func->Comp_unit()->Is_stmt_in_loop(call_stmt);
      if (called_in_loop)
        call_site->Set_flag(RNA_CALLED_IN_LOOP);

      if (call_stmt->Opr() == OPR_CALL) {
        ST* callee_st = call_stmt->St();
        Is_True(callee_st != NULL, ("no st for call"));
        IDTYPE callee_idx = _st.Get(func->File_idx(), callee_st);
        // try to bind to rule dna if there's no source dna
        if (callee_idx == INVALID_RNA_PU_IDX)
          callee_idx = Rbc()->Get_dna_idx_by_name(ST_name(callee_st));
        if (callee_idx != INVALID_RNA_PU_IDX) {
          Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG), (TFile, "Connect RNA:%4d and DNA:%4d, call stmt: %d.\n", call_site->Rna_idx(), callee_idx, call_stmt->Stmtrep_id()));
          call_site->Set_callee_idx(callee_idx);
          DNA_NODE *callee = _dnaid_to_dnanode[callee_idx];
          callee->Inc_ref_cnt(call_site);
          if (called_in_loop)
            callee->Set_flag(DNA_CALLED_IN_LOOP);
        }
        else {
          if (Show_Progress) {
            char *fname = ST_name(call_site->Callee_st());
            if (!strstr(fname, "RBC_ENGINE")) {
              fprintf(stderr,"linker got external reference %s\n", fname);
            }
          }
        }
      }
      else if (VSA_Devirt_Aggr && call_stmt->Opr() == OPR_ICALL) {
        Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG), (TFile, "Connect virtual call, call stmt:\n"));
        Is_Trace_cmdn(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG), call_stmt->Print(TFile), TFile);
        // add all possible icall target to rna callee list
        if ((call_stmt->Call_flags() & WN_CALL_IS_VIRTUAL) != 0)
          Connect_virtual_call(func, call_site);
        else if((call_stmt->Call_flags() & WN_CALL_IS_INTERFACE) != 0) {
          Connect_interface_call(func, call_site);
        } else
          Connect_indirect_call(func, call_site, addr_taken_funs);
        if(PU_java_lang(Get_Current_PU()) && 
           call_site->Callee_cnt() >= VSA_Connect_Icall_Threshold) {
          Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
                   (TFile, "Warn: %s icall candidates exceed max limit %d, trim all cands\n",
                    func->Fname(), VSA_Connect_Icall_Threshold));
          call_site->Trim(this);
        }
      }
    }

    Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG), func->Print(FALSE, TFile));
  }

  SET_OPT_PHASE("Build Topological Order");
  // 1. Build the topological order vector,
  // 2. set DNA_IN_RECURSION for functions.
  Build_dfs_vector(TRUE);
  Build_top_vector(TRUE);
  if (Get_Trace (TKIND_ALLOC, TP_MISC)) {
    fprintf(TFile, "======== MEM TRACE after Build Topological Order ========\n");
    MEM_Trace();
  }

  // link dna & rule for RBC
  SET_OPT_PHASE("Link DNA for RBC");
  Rbc()->Link_dna_for_rbc(this);
  SET_OPT_PHASE("Link Rule for RBC");
  Rbc()->Link_rule_for_rbc(this);
  if (Get_Trace (TKIND_ALLOC, TP_MISC)) {
    fprintf(TFile, "======== MEM TRACE after Link DNA and Rule for RBC ========\n");
    MEM_Trace();
  }

  SET_OPT_PHASE("Top-down Traversal");
  _real_pu_count = 0;
  // top-down traverse CG to update parm flags and do the first propagation
  for (DNODE_ITER<DNA_TRAV_TOP_DOWN_ORDER> iter(this); !iter.Is_end(); iter.Next()) {
    DNA_NODE *func = iter.Current();
    {
      VSA *vsa = func->Comp_unit()->Vsa();
      Is_True(vsa, ("vsa should be created in Init_dna phase"));
      if (vsa == NULL) {
        vsa = CXX_NEW(VSA(func->Comp_unit(), this, Mem_pool(), Loc_pool()), Mem_pool());
        vsa->Setup_sr_rna_map(func);
      }

      // TODO: move this before vsa creation after checking sec01 & sec02
      if (func->Non_functional())
        continue;
      ++ _real_pu_count;

      CONTEXT_SWITCH context(func);
      {
        MEM_POOL def_bbs_pool;
        CFG* cfg = func->Comp_unit()->Cfg();
        OPT_POOL_Initialize(&def_bbs_pool, "VSA defs bb pool", FALSE, VSA_DUMP_FLAG);
        OPT_POOL_Push(&def_bbs_pool, SSA_DUMP_FLAG);
        Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
                 (TFile, "%sIPSA::Analyze, Propagate_vardef for function: %s\n%s",
                  DBar, func->Fname(), DBar));
        func->Comp_unit()->Opt_stab()->New_coderep(&def_bbs_pool);
        func->Comp_unit()->Opt_stab()->Clear_coderep();
        vsa->Propagate_vardef(cfg->Entry_bb());
        Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
                 (TFile, "%sIPSA::Analyze, Create Arg Lists for %s()\n%s ",SBar,func->Fname(),SBar));
        Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG), func->Print(TRUE, TFile));
        Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG), (TFile, "%s",SBar));
        func->Create_arg_lists(this);
        if (VSA_New_Cprop) {
          Perform_constant_prop(func, FALSE);
        }
        OPT_POOL_Pop(&def_bbs_pool, SSA_DUMP_FLAG);

        // top-down prop param flags
        func->Update_parm_flags(this);
        OPT_POOL_Delete(&def_bbs_pool, SSA_DUMP_FLAG);
      }
    }
  }
  if (Get_Trace (TKIND_ALLOC, TP_MISC)) {
    fprintf(TFile, "======== MEM TRACE after Top-down Traversal ========\n");
    MEM_Trace();
  }

  if(Get_Trace(TP_WOPT2, IPSA_DUMP_CG)) {
    fprintf(TFile, "****CG before resolved indirect and virtual calls\n");
    Print<DNA_TRAV_TOP_DOWN_ORDER>(TFile);
  }

  SET_OPT_PHASE("Bottom-up Traversal");
  // bottom-up traverse CG to create/update arg list, perform HO/VO/VRA analysis and do devirtualization
  IPSA_CONTEXT_FOR_GLOBAL ipsa_ctx_g(this);
  _pu_counter = 0;
  for (DNODE_ITER<DNA_TRAV_BOTTOM_UP_ORDER> iter(this); !iter.Is_end(); iter.Next()) {
    DNA_NODE *func = iter.Current();
    if (func->Non_functional())
      continue;
    {
      // if (func->Decl_FSM_only()) continue;

      CONTEXT_SWITCH context(func);

      if (Show_Progress) fprintf(stderr, "Bottom Up Analysis %s(%d)\n", func->Fname(), _pu_counter);
      if (VSA_New_Cprop) {
        Perform_side_effect_prop(func, FALSE);
      }

      Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
               (TFile, "%sIPSA::Bottom up Analysis %s(%d)\n%s", DBar, func->Fname(), _pu_counter, DBar));
      if (Get_Trace(TP_WOPT2, VSA_DUMP_FLAG)) {
        fprintf(TFile, "%sDump Heap object list\n%s", SBar, SBar);
        func->Comp_unit()->Vsa()->Print_hor(TFile);
      }
      _pu_counter++;
      {
        MEM_POOL def_bbs_pool;
        OPT_POOL_Initialize(&def_bbs_pool, "VSA defs bb pool", FALSE, VSA_DUMP_FLAG);
        OPT_POOL_Push(&def_bbs_pool, SSA_DUMP_FLAG);

        CFG* cfg = func->Comp_unit()->Cfg();
        VSA *vsa = func->Comp_unit()->Vsa();

        AUX_DEF_MAP* to_add = NULL;
        AUX_DEF_MAP* to_upd = NULL;
        AUX_SET_MAP* aux_set = NULL;
        UINT32 adjust = 0;
        if (VSA_Global_Muchi) {
          ipsa_ctx_g.Collect_dna_context(func);
          to_add = ipsa_ctx_g.Aux_to_add(func);
          to_upd = ipsa_ctx_g.Aux_to_upd(func);
          aux_set = ipsa_ctx_g.Aux_set_map(func);
          adjust = ipsa_ctx_g.Orig_lastidx(func);
          if (to_add->size() > 0 || to_upd->size() > 0)
            vsa->Place_global_phi(to_add, to_upd, &def_bbs_pool);
        }

        func->Comp_unit()->Opt_stab()->New_coderep(&def_bbs_pool);
        func->Comp_unit()->Opt_stab()->Clear_coderep();
        if (!VSA_New_HVA) {
          vsa->Vsa_bb(cfg->Entry_bb(), to_add, to_upd, aux_set, &def_bbs_pool);
        }
        Rename_codemap(func->Comp_unit());
        // the coderep may changed in Rename_codemap, need to update
        // RNA arg_list coderep
        func->Update_arg_crs();
        func->Propagate_arg_and_ret_tags();

        if (VSA_Global_Muchi)
          ipsa_ctx_g.Destroy_dna_context(func);

        if (!VSA_New_HVA) {
          vsa->Perform_heap_analysis(cfg, &def_bbs_pool);
          vsa->Create_refn_vsym(cfg->Entry_bb(), &def_bbs_pool);
          vsa->Perform_vsym_analysis(cfg, &def_bbs_pool);
        }
        else {
          vsa->Perform_heap_vsym_analysis(cfg, &def_bbs_pool, VSA_New_HVA_Round);
        }

        Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
                     VSA_STATS_Print_delta(func->Fname(), TFile));

        OPT_POOL_Pop(&def_bbs_pool, SSA_DUMP_FLAG);
        OPT_POOL_Delete(&def_bbs_pool, SSA_DUMP_FLAG);
      }

      if (VSA_EH)
        func->Update_eh_types(this);
      if (VSA_Vra)
        func->Comp_unit()->Do_vra(this); // do value range analysis here
      if (VSA_Cda)
        func->Comp_unit()->Do_cda(this); // do control dependency analysis here
      if (VSA_HOA_Prop)
        func->Calculate_local_ho_annot(this);  // collect local heap_obj annotation

      if (Show_Progress_Percent && _pu_counter < _real_pu_count)
        Display_Progress(PS_LINK_P1_S + _pu_counter * PS_LINK_P1_C / _real_pu_count, FALSE);
    }
  }

  Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
               VSA_STATS_Print(TFile));
  Is_Trace(Get_Trace(TKIND_ALLOC, TP_MISC),
           (TFile, "======== MEM TRACE after Bottom-up Traversal ========\n"));
  Is_Trace_cmd(Get_Trace(TKIND_ALLOC, TP_MISC), MEM_Trace());

  // destroy IPSA_CONTEXT_FOR_GLOBAL
  ipsa_ctx_g.Destroy();

  SET_OPT_PHASE("Bottom-up Retv prop");
  MEM_POOL tag_local_pool;
  OPT_POOL_Initialize(&tag_local_pool, "VSA tag defs bb pool", FALSE, VSA_DUMP_FLAG);
  _pu_counter = 0;
  // bottom-up prop retv flags
  for (DNODE_ITER<DNA_TRAV_BOTTOM_UP_ORDER> iter(this); !iter.Is_end(); iter.Next()) {
    DNA_NODE *func = iter.Current();
    if (func->Non_functional())
      continue;
    ++ _pu_counter;
    CONTEXT_SWITCH context(func);
    func->Update_retv_flags(this);
    if (VSA_New_Cprop) {
      Perform_side_effect_prop(func, TRUE);
    }
    Resolved_indirect_and_virtual_call(func, addr_taken_funs);
    Resolved_register_call(func, addr_taken_funs);
    func->Merge_flags_from_callee_to_rna(this);
    if (VSA_Builtin_Jni()) {
      Identify_vsym_field_info(func);
    }
    // Perform tag analsyis
    Start_Timer(T_TAG_PROP);
    if (VSA_Enable_TAG_OLD) {
      OPT_POOL_Push(&tag_local_pool, SSA_DUMP_FLAG);
      func->Comp_unit()->Vsa()->Perform_tag_analysis_old(func->Comp_unit()->Cfg(), &tag_local_pool);
      OPT_POOL_Pop(&tag_local_pool, SSA_DUMP_FLAG);
    }
    else {
      func->Comp_unit()->Vsa()->Perform_tag_analysis();
    }
    Stop_Timer(T_TAG_PROP);

    if (Show_Progress_Percent && _pu_counter < _real_pu_count)
      Display_Progress(PS_LINK_P2_S + _pu_counter * PS_LINK_P2_C / _real_pu_count, FALSE);
  }
  OPT_POOL_Delete(&tag_local_pool, SSA_DUMP_FLAG);
  if (Get_Trace (TKIND_ALLOC, TP_MISC)) {
    fprintf(TFile, "======== MEM TRACE after Bottom-up Retv Prop ========\n");
    MEM_Trace();
  }

  // top-down prop constant/heap_obj annot
  SET_OPT_PHASE("Top-down constant/hoa prop");
  _pu_counter = 0;
  for (DNODE_ITER<DNA_TRAV_TOP_DOWN_ORDER> iter(this); !iter.Is_end(); iter.Next()) {
    DNA_NODE *func = iter.Current();
    if (func->Non_functional())
      continue;
    ++ _pu_counter;
    CONTEXT_SWITCH context(func);
    OPT_POOL_Push(Loc_pool(), VSA_DUMP_FLAG);
    if (VSA_New_Cprop) {
      Perform_constant_prop(func, TRUE);
    }
    OPT_POOL_Pop(Loc_pool(), VSA_DUMP_FLAG);
  }
  Is_Trace_cmd(Get_Trace(TP_VSA, VSA_PROP_DUMP_FLAG),
               VSA_STATS_Print(TFile));
  if (Get_Trace (TKIND_ALLOC, TP_MISC)) {
    fprintf(TFile, "======== MEM TRACE after Top-down constant/hoa prop ========\n");
    MEM_Trace();
  }

  // Perform_fsm_analysis performed after first bottom-up trav finished
  // rbc eval in identify FSM need cross function
  // fsm condition need tag created first
  // [FIXME] shall we remove condition eval for fsm? discuss with long
  _pu_counter = 0;
  for (DNODE_ITER<DNA_TRAV_BOTTOM_UP_ORDER> iter(this); !iter.Is_end(); iter.Next()) {
    DNA_NODE *func = iter.Current();
    if (func->Non_functional())
      continue;
    ++ _pu_counter;
    CONTEXT_SWITCH context(func);
    MEM_POOL def_bbs_pool;
    OPT_POOL_Initialize(&def_bbs_pool, "VSA fsm defs bb pool", FALSE, VSA_DUMP_FLAG);
    OPT_POOL_Push(&def_bbs_pool, SSA_DUMP_FLAG);
    func->Comp_unit()->Vsa()->Perform_fsm_analysis(func->Comp_unit()->Cfg(), &def_bbs_pool);
    OPT_POOL_Pop(&def_bbs_pool, SSA_DUMP_FLAG);

    OPT_POOL_Delete(&def_bbs_pool, SSA_DUMP_FLAG);
    if (Show_Progress_Percent && _pu_counter < _real_pu_count)
      Display_Progress(PS_LINK_P3_S + _pu_counter * PS_LINK_P3_C / _real_pu_count, FALSE);
  }
  if (Get_Trace (TKIND_ALLOC, TP_MISC)) {
    fprintf(TFile, "======== MEM TRACE after Bottom-up FSM analysis ========\n");
    MEM_Trace();
  }

  SET_OPT_PHASE("Rebuild iterator");
  // rebuild iterator
  Build_dfs_vector(TRUE);
  Build_top_vector(TRUE);
  if (Get_Trace (TKIND_ALLOC, TP_MISC)) {
    fprintf(TFile, "======== MEM TRACE after Rebuild Iterator ========\n");
    MEM_Trace();
  }

  if(Get_Trace(TP_WOPT2, IPSA_DUMP_CG)) {
    fprintf(TFile, "\n****CG after resolved indirect and virtual calls\n");
    Print<DNA_TRAV_TOP_DOWN_ORDER>(TFile);
  }

}

// =============================================================================
//
// IPSA::Get_global, to find the LDID node inside the mu_list of the statement
//                   stated in the parameter list.
//             NOTE: returns LDID node w/i the context of dna, the 1st parameter
//                   stidx is the index to the global symbol table
//                   stmt is the call statement that is analyzed 
//
// =============================================================================
CODEREP*
IPSA::Get_global(DNA_NODE *dna, UINT32 callee_fid, ST_IDX stidx, STMTREP *call_stmt)
{
  CONTEXT_SWITCH context(dna);
  OPERATOR opr = call_stmt->Opr();
  if (opr != OPR_CALL || ! OPERATOR_has_sym(opr)) return NULL;

  MU_LIST_ITER mu_iter;
  MU_NODE *mnode;
  MU_LIST *mu_list = call_stmt->Mu_list();

  WHIRL_FILE_MANAGER* mgr = WHIRL_FILE_MANAGER::Get();
  FOR_ALL_NODE (mnode, mu_iter, Init(mu_list)) {
    CODEREP *ref = mnode->OPND();
    AUX_STAB_ENTRY *sym = dna->Comp_unit()->Opt_stab()->Aux_stab_entry(ref->Aux_id());
    ST *st = sym->St();
    if (st == NULL)
      continue;
    if (callee_fid == dna->File_idx() && stidx == ST_st_idx(st))
      return ref;
    else if (mgr &&
             strcmp(ST_name(callee_fid, stidx), ST_name(st)) == 0)
      return ref;
  }
  return NULL;
}


// =============================================================================
//
// IPSA::Callee_frees_heap_memory returns the argument that is freed
//   Limitation: it returns only the 1st argument freed, 08/17/2018 -Shin
//   The return value could be the CR of the actual argument or the global
//   variable that is freed by the callee
//
// =============================================================================
CODEREP*
IPSA::Callee_frees_heap_memory(DNA_NODE *dna, STMTREP *stmt, BOOL *maybe)
{
  RNA_NODE *rna = dna->Get_callsite_rna(stmt);
  DNA_NODE *callee = Get_dna(rna->Uniq_callee());

  if (callee == NULL) return NULL;

  if (callee->Deallocate()) {
    IDTYPE i;
    // set maybe flag
    if (maybe != NULL)
      *maybe = callee->May_deallocate() ? TRUE : FALSE;
    // find callee's formal parameter that gets freed in the function
    i = callee->Find_first_param_freed();
    // get the argument of the call statement that has the same seqquence order
    if (i != INVALID_VAR_IDX)
      return rna->Get_arg(i);

    // none of the callee's parameter get free, maybe global get freed
    i = callee->Find_first_glob_freed();
    if (i != INVALID_VAR_IDX) {
      // check any of the mu list entry is the freed global symbol
      // return the LDID node of global variable if found
      // TODO: otherwise, we need to propagate upward, pending global symbol
      CODEREP *retv = Get_global(dna, callee->File_idx(), callee->Get_glob_stidx(i), stmt);
      return retv;
    }
  }
  return NULL;
}

// =============================================================================
//
// DNA_NODE::Return_heapobj check the retv_list for the returning value if it
//           carries value returns from intrinsic function as it is marked
//           during Init_DNA phase
//
// =============================================================================
HEAPSTATE
DNA_NODE::Return_heapobj(HEAPINQ chkhor, HEAP_OBJ_REP **retv, HEAPSIZE *hsz)
{
  CONTEXT_SWITCH context(this);
  PNODE_VECTOR  *retv_list = Retv_list();
  HEAPSTATE      rv = HS_NONE;

  if ((*retv_list).size() > PDV_INIT_ID) {
    for (INT i = PDV_INIT_ID; i < (*retv_list).size(); ++i) {
      PDV_NODE *pdv = (*retv_list)[i];
      if ((pdv->Kind() & BY_RETURNSTMT) == 0)
        continue;
      STMTREP *stmt = pdv->Stmt();
      if (! chkhor)
        return (stmt->Rhs()->Value_malloc() != 0)?HS_GOOD:HS_NONE;
      else {
        VSA *vsa = Comp_unit()->Vsa();
        if (retv == NULL)
          return vsa->Get_heap_obj_state(stmt);
        else {
          *retv = vsa->Cr_2_heap_obj(stmt->Rhs());
          return vsa->Get_heap_obj_state(*retv);
        }
      }
    }
  }

  return rv;
}

// =============================================================================
//
// IPSA::Callee_allocate_heap_memory wrapper for checking user defined function
//                       returns heap memory
//
// =============================================================================
HEAPSTATE
IPSA::Callee_allocate_heap_memory(DNA_NODE *dna, STMTREP *stmt, BOOL chkhor)
{
  RNA_NODE *rna = dna->Get_callsite_rna(stmt);
  DNA_NODE *callee = Get_dna(rna->Uniq_callee());

  if (callee == NULL) return HS_NONE;
  if (! callee->Is_set(DNA_MEMORY_ALLOCATION)) return HS_NONE;
  // even with DAN_MEMORY_ALLOCATION flag set, we check if the RHS of the
  // return statement
  return callee->Return_heapobj((chkhor)?HINQ_STATE:HINQ_NONE, NULL);
}

// =============================================================================
//
// IPSA::Callee_returns_heapobj_size returns the size or -1 if size not unknown.
//
// =============================================================================
INT64
IPSA::Callee_returns_heapobj_size(DNA_NODE *dna, STMTREP *stmt, HEAPSIZE *hsz)
{
  RNA_NODE *rna = dna->Get_callsite_rna(stmt);
  DNA_NODE *callee = Get_dna(rna->Uniq_callee());

  if (callee == NULL){ if (hsz) *hsz = HSZ_UNKNOWN; return -1; }
  if (! callee->Is_set(DNA_MEMORY_ALLOCATION)){ if (hsz) *hsz = HSZ_UNKNOWN; return -1; }

  HEAP_OBJ_REP *retv = NULL;
  callee->Return_heapobj(HINQ_HOR, &retv, hsz);
  if (retv != NULL) {
    CODEREP *size_cr = retv->Heap_obj()->Byte_size();
    if (size_cr && size_cr->Kind() == CK_CONST) {
      // *hsz is set by the call to Return_heapobj
      return size_cr->Const_val();
    }
    else {
      if (hsz) *hsz = HSZ_UNKNOWN; return -1;
    }
  }
  if (hsz) *hsz = HSZ_UNKNOWN; return -1;
}


// =============================================================================
//
// IPSA::Recover_call_stmt finds the stmtrep based on the coderep of call
//
// =============================================================================
STMTREP*
IPSA::Recover_call_stmt(DNA_NODE *dna, CODEREP *call)
{
  RNA_NODE *rna = dna->Get_callsite_rna(call);
  return (rna)? rna->Callstmt() : NULL;
}
  
// =============================================================================
//
// IPSA::Eval_callsite_arg_list, establish the argument list of the call.
//
// =============================================================================
RNA_NODE*
IPSA::Eval_callsite_arg_list(DNA_NODE *caller, STMTREP *stmt)
{
  OPERATOR opr = stmt->Opr();
  Is_True( OPERATOR_is_call(opr),
           ("IPSA::Eval_callsite_arg_list invoked for non_call"));

  RNA_NODE     *rna = caller->Get_callsite_rna(stmt);
  DNA_NODE     *callee = Get_dna(rna->Uniq_callee());
  MU_LIST_ITER  mu_iter;
  MU_NODE      *mnode;
  MU_NODE      *pmnode;
  CHI_LIST_ITER chi_iter;
  CHI_NODE     *cnode;
  CHI_NODE     *pcnode;

  if (callee == NULL) {
    if (stmt->Callee_returns_new_heap_memory() ||
        stmt->Callee_frees_heap_memory()) {
      // clear any mu chi entry that is not preg or return vsym
      // TODO: we shall create side effect summary for such functions

      pcnode = NULL;
      FOR_ALL_NODE( cnode, chi_iter, Init(stmt->Chi_list())) {
        if (cnode->Live() &&
            cnode->RESULT()->Is_flag_set(CF_IS_ZERO_VERSION) &&
            ! caller->Is_aux_global(cnode->Aux_id()) && // keep the global
            cnode->Aux_id() != caller->Comp_unit()->Opt_stab()->Return_vsym() &&
            cnode->Aux_id() != caller->Comp_unit()->Opt_stab()->Default_vsym()) {
          stmt->Chi_list()->Remove(pcnode, cnode);
        }
        pcnode = cnode;
      }
    }
    return NULL;
  }

  // moved to link phase, rna->Collect_arg_list(stmt->Rhs(), Mem_pool());

  Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG), (TFile, "IPSA::Eval_callsite_arg_list\n"));
  Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG), rna->Print(TRUE, TFile));

  // Update the CODEMAP with side effect about the argument passed into call
  INT i;
  for (i = VAR_INIT_ID; i <= rna->Arg_cnt(); ++i) {
    // TODO: model how callee handles va_args
    if (i >= callee->Parm_list()->size())
      break;
    // merge the callee parm_flags into arg_flags for easier access from VSA
    UINT32 arg_flags = rna->Set_arg_flag(i, callee->Parm_flags(i)|ARG_REF_MERGED);
    // UINT32 arg_flags = rna->Get_arg_flags(i);
    AUX_ID arg_aux = rna->Get_arg_stidx(i);
    CODEREP *arg = rna->Get_arg(i);
    if (!Is_struct_ptr(arg) ||
        (arg_flags & (REF_ILOAD | REF_ISTORE)) == 0) {
      if ((arg_flags & LDA_VAL_REFED) != LDA_VAL_REFED) {
        // this arg is not referenced, remove it from mu_list
        pmnode = NULL;
        FOR_ALL_NODE( mnode, mu_iter, Init(stmt->Mu_list())) {
          if (mnode->Aux_id() == arg_aux) {
            stmt->Mu_list()->Remove(pmnode, mnode);
          }
          pmnode = mnode;
        }
      }
      if ((arg_flags & LDA_VAL_MODED) != LDA_VAL_MODED) {
        // this arg is not modified, remove it from chi_list
        pcnode = NULL;
        FOR_ALL_NODE( cnode, chi_iter, Init(stmt->Chi_list())) {
          if (cnode->Aux_id() == arg_aux) {
            stmt->Chi_list()->Remove(pcnode, cnode);
          }
          pcnode = cnode;
        }
      }
    }
    if ((arg_flags & REF_ISTORE)){
      // this arg is modified, we shall update the parm if the arg is a parm of
      // the caller DNA_NODE

      if (arg->Kind() == CK_VAR) {
        BOOL is_ptr = FALSE;
        IDTYPE_SET visited_set;
        IDTYPE ref_param = caller->Find_param_references(arg, &is_ptr, IN_ISTORE_BASE, visited_set);
        if (ref_param != INVALID_VAR_IDX && caller->Is_parm_original(ref_param))
          caller->Set_parm_flag(ref_param, REF_ISTORE);
      }
    }
  }

  return rna;
}

// =============================================================================
//
// IPSA::Analysis_driver, depth first traversal in the call graph and perform
//     the static analysis.  It enables the main VSA function to handle
//     context sensitivity.
//
// =============================================================================
#if 0
void
IPSA::Analysis_driver(DNA_NODE *func,  RNA_NODE *caller_instance)
{
  if (Visited(func)) return;
  Set_visit(func);

  CONTEXT_SWITCH context(func);

  // 1. Perform top-down alias info progagation
  //    which is embedded inside the Collect_arg_list.

  for (INT j = VAR_INIT_ID; j < func->Call_list()->size(); ++j) {
    // 2. Visit its call_sites first
    RNA_NODE *call_site = (*func->Call_list())[j];
    if (! Visited(call_site) ) {
      Set_visit(call_site);

      IDTYPE    callee_idx = call_site->Callee_idx();
      if (callee_idx != INVALID_RNA_PU_IDX) {
        DNA_NODE *callee = _dnaid_to_dnanode[callee_idx];
        Analysis_driver(callee, call_site);

        // Do the context sensitive analysis with (caller:func, callee)
        // TODO TODO TODO
      }
    }
  }

  // 3. Perform the SA and Report errors for THIS func also propagate side effect up
  if (Show_Progress) fprintf(stderr, "Analyzing %s(%d)\n", func->Fname(), _pu_counter);

  Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
           (TFile, "%sIPSA::Analyzing %s(%d)\n%s", DBar, func->Fname(), _pu_counter, DBar));
  _pu_counter++;

  func->Comp_unit()->Do_vsa(this);
}

#endif


RBC_BASE*
IPSA::Rbc(void)
{
  if (_rbc == NULL) {
    _rbc = CXX_NEW(RBC_BASE(Mem_pool(), Loc_pool()), Mem_pool());
  }
  return _rbc;
}


// =============================================================================
//
// IPSA::Fld_name_2_id, mapping field name to a uniq id
//
// =============================================================================
IDTYPE
IPSA::Fld_name_2_id(STRING fld_name)
{
  FLD_NAME_MAP::iterator iter = _fld_name_id_map.find(fld_name);
  if(iter != _fld_name_id_map.end()) {
    return iter->second;
  } else {
    return FLD_ID_INVALID;
  }
}


// =============================================================================
//
// IPSA::Fld_id_2_name, mapping id to field name
//
// =============================================================================
const STRING
IPSA::Fld_id_2_name(IDTYPE id)
{
  if(id >= _last_fld_name_id) {
    Is_True(false, ("invalid FLD name id"));
    return NULL;
  }
  FLD_NAME_MAP::iterator iter;
  FLD_NAME_MAP::iterator itEnd = _fld_name_id_map.end();
  for(iter=_fld_name_id_map.begin(); iter != itEnd; iter++) {
    if(iter->second == id) {
      return iter->first;
    }
  }
  Is_True(false, ("unable to find id"));
  return NULL;
}


// =============================================================================
//
// IPSA::Enter_fld_name, add fld name to map with a new id
//
// =============================================================================
IDTYPE
IPSA::Enter_fld_name(STRING fld_name)
{
  int len = strlen(fld_name);
  STRING new_fld_name = (STRING)MEM_POOL_Alloc(Mem_pool(), len + 1);
  strcpy(new_fld_name, fld_name);
  new_fld_name[len] = '\0';
  IDTYPE new_id = New_fldname_id();
  _fld_name_id_map[new_fld_name] = new_id;
  return new_id;
}


// =============================================================================
//
// IPSA::Print_fld_name_map
//
// =============================================================================
void
IPSA::Print_fld_name_map(FILE* fp)
{
  FLD_NAME_MAP::iterator iter;
  fprintf(fp, "%sField id name map:\n%s", DBar, DBar);
  for(iter=_fld_name_id_map.begin(); iter != _fld_name_id_map.end(); iter++) {
    fprintf(fp, "%d->[%s]\n", iter->second, iter->first);
  }
}

// =============================================================================
//
// IPSA::Analyze: it performs Link process, which will be converged with the LD
//     function later, then process root functions by calling Bottom_up_analysis
//
// =============================================================================
void
IPSA::Analyze(void)
{
  Set_Error_Phase("IPSA Analyze");
  if (Get_Trace (TKIND_ALLOC, TP_MISC)) {
    // Sub_Mem_Tracing_Enabled(TRUE);
    fprintf(TFile, "======== MEM TRACE before IPSA::Analyze ========\n");
    MEM_Trace();
  }

  SET_OPT_PHASE("IPSA Preparation");
  Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG), (TFile, "IPSA::Analyze\n"));
  // if no source file is provided, skip analyze on rule files and warn.
  WHIRL_FILE_MANAGER *mgr = WHIRL_FILE_MANAGER::Get();
  if (mgr == NULL) {
    if (FILE_INFO_is_rbc(File_info)) {
      ErrMsg ( EC_No_Sources );
      return;
    }
  }
  else {
    BOOL has_source = FALSE;
    for (int i = 1; i <= mgr->Get_file_count(); i++) {
      WHIRL_FILE_INFO& fi = mgr->Get_file(i);
      if ((fi.File_type() == file_whirl ||
           fi.File_type() == file_ar_whirl) &&
          !FILE_INFO_is_rbc(fi.Whirl_file_info())) {
        has_source = TRUE;
        break;
      }
      Set_Error_Source(fi.File_name());
    }
    if (!has_source) {
      ErrMsg ( EC_No_Sources );
      return;
    }
  }

  Link();

  if (VSA_VCG_Cg_Fname != NULL) {
    Print_cg_vcg(VSA_VCG_Cg_Fname);
  }

  if (Get_Trace (TKIND_ALLOC, TP_MISC)) {
    fprintf(TFile, "======== MEM TRACE after IPSA::Link ========\n");
    MEM_Trace();
  }

  // checks that are not bound to specific functions, should be done globally
  // like builtin recursion check, checks on all execution paths
  SET_OPT_PHASE("Builtin & Exec Path Analysis");
  if (!Rbc()->Is_builtin_check(RBCT_NONE)) {
    MEM_POOL msg_out_pool;
    OPT_POOL_Initialize(&msg_out_pool, "Builtin & Exec Path Analyze pool", FALSE, VSA_DUMP_FLAG);
    OPT_POOL_Push(&msg_out_pool, VSA_DUMP_FLAG);

    if (Rbc()->Is_builtin_check(RBCT_RECURSION)) {
      Rbc()->Builtin_recursion_check(this, &msg_out_pool);
    }

    for (DNODE_ITER<DNA_TRAV_PRE_ORDER> iter(this); !iter.Is_end(); iter.Next()) {
      DNA_NODE *func = iter.Current();
      if (!func->Is_set_rbc_flag(DNA_RBC_RULE_CFG))
        continue;

      RNODE_VECTOR *rna_list = func->Call_list();
      for (INT i = VAR_INIT_ID; i < rna_list->size(); i++) {
        RNA_NODE *rna = (*rna_list)[i];
        if (rna->Is_flag_set(RBC_SE_EXEC_PATH)) {
          CONTEXT_SWITCH context(func);
          Rbc()->Process_rbc_exec_path(this, func, rna, &msg_out_pool);
        }
      }
    }

    OPT_POOL_Pop(&msg_out_pool, VSA_DUMP_FLAG);
    OPT_POOL_Delete(&msg_out_pool, VSA_DUMP_FLAG);
  }

  if (Get_Trace (TKIND_ALLOC, TP_MISC)) {
    fprintf(TFile, "======== MEM TRACE after Builtin & Exec Path Analysis ========\n");
    MEM_Trace();
  }

  if(VSA_Builtin_CertC) {
    SET_OPT_PHASE("Global Builtin CertC Analysis");
    Rbc()->Global_builtin_certc_check(this);
    if (Get_Trace (TKIND_ALLOC, TP_MISC)) {
      fprintf(TFile, "======== MEM TRACE after Global Builtin CertC Analysis ========\n");
      MEM_Trace();
    }
  }

  if (VSA_Builtin_CertJ) {
    SET_OPT_PHASE("Global Builtin CertJ Analysis");
    Rbc()->Global_builtin_certj_check(this);
    if (Get_Trace (TKIND_ALLOC, TP_MISC)) {
      fprintf(TFile, "======== MEM TRACE after Global Builtin CertJ Analysis ========\n");
      MEM_Trace();
    }
  }

  // evaluate & check FSMs
  if (VSA_Enable_FSM()) {
    SET_OPT_PHASE("Evaluate & Check FSMs");
    Rbc()->Evaluate_and_check_FSMs(this);

    if (Get_Trace (TKIND_ALLOC, TP_MISC)) {
      fprintf(TFile, "======== MEM TRACE after Evaluate & check FSMs ========\n");
      MEM_Trace();
    }
  }

  // bottom up traversal for each independent call tree
  _pu_counter = 0;
  // SET_OPT_PHASE("Builtin Analyze");

  for(DNODE_ITER<DNA_TRAV_POST_ORDER> iter(this); !iter.Is_end(); iter.Next()) {
    DNA_NODE *func = iter.Current();
    if (func->Non_functional()) continue;

    if (!VSA_Enable_Lib_Check) {
      char *fname = func->Fname();
      BOOL skip = Is_lib_func(fname);
      if ((PU_c_lang(*func->Pu()) || PU_cxx_lang(*func->Pu())) && skip)
        continue;
    }

    CONTEXT_SWITCH context(func);
    if (Show_Progress) fprintf(stderr, "Analyzing %s(%d)\n", func->Fname(), _pu_counter);

    Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
             (TFile, "%sIPSA::Analyzing %s(%d)\n%s", DBar, func->Fname(), _pu_counter, DBar));
    _pu_counter++;
    func->Comp_unit()->Do_vsa(this);

    if (Show_Progress_Percent)
      Display_Progress(PS_ANALYZE_S + _pu_counter * PS_ANALYZE_C / _real_pu_count, FALSE);
  }
  SET_OPT_PHASE("Finish");

  // flush issues at first
  VSA_ISSUE_WRITTER* issue_writter = VSA_ISSUE_WRITTER::Get_writter(RFile);
  issue_writter->Flush();

  if (Get_Trace (TKIND_ALLOC, TP_MISC)) {
    fprintf(TFile, "======== MEM TRACE after Builtin Analyze ========\n");
    MEM_Trace();
  }
  REPORT_STATISTICS();

  if (Show_Progress_Percent)
    Display_Progress(PS_ANALYZE_S + PS_ANALYZE_C, TRUE);

#if 0
  New_visit_counter();
  for (INT i = DNA_INIT_IDX; i < _dnaid_to_dnanode.size(); ++i) {
    DNA_NODE *func = _dnaid_to_dnanode[i];
    // Analyze root functions
    if (func->Is_root()) {
      // Perform analysis
      Analysis_driver(func, NULL);
    }
  }
#endif

}

// =============================================================================
//
// IPSA::Emit: Emit whirl with IPSA instrumentation
//
// =============================================================================
void
IPSA::Emit(char *workdir)
{
  hash_set<IDTYPE> visited_files;
  IDTYPE curr_file = INVALID_FILE_IDX;
  Output_File *curr_ir_output = NULL;

  for (INT i = DNA_INIT_IDX; i < Dna_count(); ++i) {
    DNA_NODE *func = _dnaid_to_dnanode[i];
    if(func->Non_functional()) {
      continue;
    }

    if (Show_Progress) fprintf(stderr, "Emitting %s(%d)\n", func->Fname(), func->Dna_idx());

    CONTEXT_SWITCH context(func);
    if(curr_file != func->File_idx()) {
      if(visited_files.find(func->File_idx()) != visited_files.end()) {
        Is_True(FALSE, ("IPSA::Emit - File %s already emitted", Src_File_Name));
        continue;
      }

      visited_files.insert(func->File_idx());
      char *output_ir_name = NULL;
      if(workdir != NULL) {
        char * output_ir_name = (char *) malloc(strlen(workdir) + strlen(Ipsa_File_Name) + 2);
        sprintf(output_ir_name, "%s/%s", workdir, Ipsa_File_Name);
        curr_ir_output = Open_Output_Info(output_ir_name);
        free(output_ir_name);
      } else {
        curr_ir_output = Open_Output_Info(Ipsa_File_Name);
      }
      curr_file = func->File_idx();

    }

    // write current pu tree
    COMP_UNIT *cu = func->Comp_unit();
    RVI rvi(WOPT_Enable_RVI, cu->Opt_stab(),
      WOPT_Enable_RVI ? cu->Pre_rvi_hooks()->Nbits() : 0,
      cu->Alias_mgr());
    WN *tree = cu->Emit_ML_WHIRL(&rvi);
    Set_PU_Info_tree_ptr(Current_PU_Info, tree);
    Write_PU_Info(Current_PU_Info);

    // write global info for file's last dna, dna created based-on file sequence
    DNA_NODE *next_func = NULL;
    if(i < Dna_count() - 1) {
      next_func = _dnaid_to_dnanode[i+1];
    }
    if(!next_func || next_func->File_idx() != curr_file) {
      // remove PU if already defined in other files
      // only traverse top-level PUs, assume nested pus should be defined in
      // same file with parent
      PU_Info *prev_pu = NULL;
      PU_Info *new_pu_tree = (PU_Info*) Global_PU_Tree;
      for (PU_Info *current_pu = new_pu_tree;
           current_pu != NULL;
           current_pu = PU_Info_next(current_pu)) {
        ST_IDX proc_sym = current_pu->proc_sym;
        DNA_NODE *proc_dna = Get_dna(curr_file, proc_sym);
        if(!proc_dna || proc_dna->File_idx() != curr_file) {
          if(prev_pu) {
            PU_Info_next(prev_pu) = PU_Info_next(current_pu);
          } else {
            new_pu_tree = PU_Info_next(current_pu);
          }
        } else {
          prev_pu = current_pu;
        }
      }
      Write_Global_Info((PU_Info*)new_pu_tree);
      Close_Output_Info();
    }
  }
}

BOOL
IPSA::Is_jni_call(RNA_NODE *rna)
{
  CALLEE_VECTOR::const_iterator callee_iter;
  for (callee_iter = rna->Callee_list().begin(); callee_iter != rna->Callee_list().end(); ++callee_iter) {
    DNA_NODE *callee = Get_dna(callee_iter->Callee());
    if (!callee || !callee->Non_functional())
      continue;
    if (callee->Is_set_rbc_flag(DNA_JNI_MODEL))
      return TRUE;
  }
  return FALSE;
}

UINT32
IPSA::Rna_get_rbc_parm_flag(RNA_NODE *rna, IDTYPE parm_idx)
{
  if (rna == NULL)
    return REF_NONE;

  STMTREP *call_stmt = rna->Callstmt();
  if (call_stmt == NULL)
    return REF_NONE;

  UINT32 parm_flags = REF_NONE;
  if (call_stmt->Opr() == OPR_INTRINSIC_CALL) {
    DNA_NODE *caller = Get_dna(rna->Caller_idx());
    if (caller == NULL || caller->Non_functional())
      return REF_NONE;
    CONTEXT_SWITCH ctx(caller);
    const char *callee_name = INTRINSIC_name(call_stmt->Rhs()->Intrinsic());
    if (callee_name != NULL) {
      IDTYPE rbc_idx = Rbc()->Get_dna_idx_by_name((char*)callee_name);
      DNA_NODE *callee = Get_dna(rbc_idx);
      if (callee != NULL) {
        DNODE_VECTOR *rbc_nodes = Rbc()->Get_rbc_nodes(callee);
        if (rbc_nodes != NULL) {
          DNODE_VECTOR::const_iterator rbc_iter = rbc_nodes->begin();
          for (; rbc_iter != rbc_nodes->end(); rbc_iter++) {
            DNA_NODE *rbc_callee = *rbc_iter;
            if (rbc_callee != NULL &&
                parm_idx < rbc_callee->Parm_list()->size()) {
              parm_flags |= rbc_callee->Parm_flags(parm_idx);
            }
          }
        }
      }
    }
  } else {
    const CALLEE_VECTOR& callee_list = rna->Callee_list();
    CALLEE_VECTOR::const_iterator iter;
    DNA_NODE *callee = NULL;
    for (iter = callee_list.begin(); iter != callee_list.end(); ++iter) {
      callee = Get_dna(iter->Callee());
      if (callee) {
        DNODE_VECTOR *rbc_nodes = Rbc()->Get_rbc_nodes(callee);
        if (rbc_nodes == NULL)
          continue;
        DNODE_VECTOR::const_iterator rbc_iter = rbc_nodes->begin();
        for (; rbc_iter != rbc_nodes->end(); rbc_iter++) {
          DNA_NODE *rbc_callee = *rbc_iter;
          if (rbc_callee != NULL &&
              parm_idx < rbc_callee->Parm_list()->size()) {
            parm_flags |= rbc_callee->Parm_flags(parm_idx);
          }
        }
      }
    }
  }
  return parm_flags;
}

BOOL
IPSA::Rna_has_rbc_parm_flag(RNA_NODE *rna, IDTYPE parm_idx, UINT32 flags)
{
  UINT32 parm_flags = Rna_get_rbc_parm_flag(rna, parm_idx);
  if (parm_flags & flags) {
    return TRUE;
  }
  return FALSE;
}

RBC_OP_SET *
IPSA::Rna_get_rbc_ops(RNA_NODE *rna, RBC_OP_SET &op_set)
{
  if (!rna->Is_flag_set(RNA_HAS_RBC_NODES)) {
    return NULL;
  }
  const CALLEE_VECTOR& callee_list = rna->Callee_list();
  if (callee_list.size() == 1) {
    return Rbc()->Get_rbc_ops(Get_dna(rna->Uniq_callee()));
  }
  CALLEE_VECTOR::const_iterator iter;
  DNA_NODE *callee = NULL;
  for (iter = callee_list.begin(); iter != callee_list.end(); ++iter) {
    callee = Get_dna(iter->Callee());
    if (callee) {
      RBC_OP_SET *rbc_ops = Rbc()->Get_rbc_ops(callee);
      if (rbc_ops) {
        op_set.insert(rbc_ops->begin(), rbc_ops->end());
      }
    }
  }
  return &op_set;
}

BOOL
IPSA::Rna_has_rbc_ops(RNA_NODE *rna, RBC_OP *ops, int ops_cnt)
{
  RBC_OP_SET op_set;
  RBC_OP_SET *res_ops = Rna_get_rbc_ops(rna, op_set);
  if (res_ops) {
    for(int idx = 0; idx < ops_cnt; idx++) {
      if (res_ops->find(ops[idx]) != res_ops->end()) {
        return TRUE;
      }
    }
  }
  return FALSE;
}

BOOL
IPSA::Rna_has_rbc_op(RNA_NODE *rna, RBC_OP op)
{
  RBC_OP_SET op_set;
  RBC_OP_SET *res_ops = Rna_get_rbc_ops(rna, op_set);
  if (res_ops && res_ops->find(op) != res_ops->end()) {
    return TRUE;
  }
  return FALSE;
}

DNA_NODE*
IPSA::Get_global_clinit_dna(UINT32 file_idx, ST_IDX stidx)
{
  ST *st = St_ptr(file_idx, stidx);
  Is_True(st, ("Null global st"));
  if(st) {
    ST_SCLASS sc = ST_sclass(st);
    BOOL is_global = (sc == SCLASS_FSTATIC ||
                      sc == SCLASS_UGLOBAL ||
                      sc == SCLASS_DGLOBAL ||
                      sc == SCLASS_COMMON ||
                      sc == SCLASS_EXTERN) ? TRUE : FALSE;
    Is_True(is_global, ("not a global variable"));
    if(is_global) {
      const char *glob_name = ST_name(file_idx, stidx);
      UINT32 def_file = INVALID_FILE_IDX;
      ST_IDX def_st = 0;
      BOOL found = Glob_cha()->Get_clinit_by_glob(glob_name, def_file, def_st);
      if(found && def_file && def_st) {
        IDTYPE idx = _st.Get_xpreempt(def_file, def_st, ST_sclass(St_ptr(def_file, def_st)));
        if(idx != INVALID_RNA_PU_IDX) {
          return Get_dna(idx);
        }
      }
    }
  }
  return NULL;
}

// =============================================================================
//
// IPSA::Verify: this function serves the purpose of debugging memory management
//     and context switch issues.  The current shape is not final/!!!
//
// =============================================================================
void
IPSA::Verify(void)
{
  for( DNODE_ITER<DNA_TRAV_PRE_ORDER> iter(this); !iter.Is_end(); iter.Next()) {
    DNA_NODE *func = iter.Current();
    g_comp_unit = func->Comp_unit();
    func->Restore_context();
    func->Comp_unit()->Cfg()->Print(TFile);
  }
}

// =============================================================================
//
// IPSA::Print_cr: context switch version of coderep print
//
// =============================================================================
void
IPSA::Print_cr(DNA_NODE *dna, CODEREP* cr, FILE* f)
{
  if(dna) {
    CONTEXT_SWITCH c(dna);
    cr->Print(f);
  }
}

// =============================================================================
//
// IPSA::Print_sr: context switch version of stmtrep print
//
// =============================================================================
void
IPSA::Print_sr(DNA_NODE *dna, STMTREP* sr, FILE* f)
{
  if(dna) {
    CONTEXT_SWITCH c(dna);
    sr->Print(f);
  }
}

// =============================================================================
//
// IPSA::Print_cfg: context switch version of cfg print
//
// =============================================================================
void
IPSA::Print_cfg(COMP_UNIT* cu, FILE* f)
{
  if(cu && cu->Dna()) {
    CONTEXT_SWITCH c(cu->Dna());
    cu->Cfg()->Print(f);
  }
}

// =============================================================================
//
// IPSA::Print_intrn_table: print the entire entrinsic table
//
// =============================================================================
void
IPSA::Print_intrn_table(FILE *fp)
{
  fprintf(fp, "======== Dump intrinsic table ========\n");
  INT32 id;
  for (id = INTRINSIC_FIRST; id < INTRINSIC_LAST; ++id) {
    Print_intrn_entry(fp, (INTRINSIC) id);
  }
  fprintf(fp, "======== Dump intrinsic table end ========\n");
}

// =============================================================================
//
// STPATH::New_instance, this function is used to create an instance of type
// STPATH, as this class has variable length member, we disable any public
// constructor
//
// =============================================================================
STPATH*
STPATH::New_instance(STPATH * sp, MEM_POOL* pool)
{
  STPATH* new_sp = New_instance(sp->St_idx(), sp->Field_id(), sp->Path_size(), pool);
  for(int i=0; i < sp->Path_size(); i++)
    new_sp->Add_path(sp->Path(i), i);
  return new_sp;
}

// =============================================================================
//
// STPATH::Print, print STPATH with well format
//
// =============================================================================
void
STPATH::Print(FILE *fp)
{
  fprintf(fp, "(%d)%s[", St_idx(), St_name());
  if(Field_id() > 0) {
    fprintf(fp, "+%d", Field_id());
  }
  for (int i=0; i < Path_size();) {
    fprintf(fp, "%d:%d", SRCPOS_filenum(Path(i)), SRCPOS_linenum(Path(i)));
    i++;
    if(i< Path_size())
      fprintf(fp, ",");
  }
  fprintf(fp, "]");
}

const char *
STPATH::Find_cr_stname(STRING_BUFFER *buf, OPT_STAB *opt_stab, CODEREP *cr, STMTREP *sr)
{
  if (cr->Kind() == CK_VAR) {
    AUX_STAB_ENTRY *sym = opt_stab->Aux_stab_entry(cr->Aux_id());
    char *st_name = Vsa_sym_name(sym);
    const char *new_name = NULL;
    buf->Append_stname(st_name);
    if(cr->Field_id() > 0) {
      new_name = SRCPOS_HANDLE::Gen_fld_stname(buf, NULL, cr->Lod_ty(), cr->Field_id());
    }
    return buf->To_string();
  } else if(cr->Kind() == CK_IVAR) {
    CODEREP *base = cr->Ilod_base() ? cr->Ilod_base() : cr->Istr_base();
    Is_True(base, ("base is null for Ivar"));
    UINT fld_id = cr->I_field_id();
    TY_IDX ilod_ty = cr->Ilod_ty();
    if (fld_id == 0) {
      if (PU_src_lang(Get_Current_PU()) & (PU_C_LANG | PU_CXX_LANG))
        buf->Append('*');
    }
    else if (!Is_Structure_Type(ilod_ty))
      buf->Append('(');
    const char *base_name = Find_cr_stname(buf, opt_stab, base, sr);
    if(base_name == NULL)
      return NULL;
    if(Is_Structure_Type(ilod_ty) && fld_id > 0) {
      const char* fld_name = SRCPOS_HANDLE::Gen_fld_stname(buf, NULL, cr->Ilod_base_ty(), fld_id);
      return buf->To_string();
    } else {
      if (fld_id > 0)
        buf->Format("+%d)", fld_id);
      return buf->To_string();
    }
  }
  return NULL;
}

// =============================================================================
//
// Dump_stpath: print STPATH with well format
//
// =============================================================================
void
Dump_stpath(const WN* wn, FILE* fp)
{
  DNA_NODE *cur_dna = Get_cur_dna();
  if(cur_dna) {
    STPATH* stpath = cur_dna->Get_stpath(wn);
    if(stpath != NULL) {
      fprintf(fp, " #STPATH: ");
      stpath->Print(fp);
    }
  }
}

void
DNA_NODE::Dump_all_stpath(FILE *fp)
{
  STPATH_MAP::iterator iter = _stpath_map.begin();
  for (; iter != _stpath_map.end(); ++iter) {
    UINT64 key = iter->first;
    fprintf(fp, "Key: sr(%d)cr(%d); ", (INT32) (key >> 32), (INT32) (key & 0xffffffff));
    iter->second->Print(fp);
    fprintf(fp, "\n");
  }
}

void
DNA_NODE::Dump_stpath(const STMTREP *stmt, const CODEREP* cr, FILE* fp)
{
  STPATH* stpath = Get_stpath(stmt, cr);
  if(stpath != NULL) {
    fprintf(fp, " #STPATH: ");
    stpath->Print(fp);
  }
}

// =============================================================================
//
// DNA_NODE::Map_st_node, MAP ST  to WN/STMTREP/CODEREP where the ST is deleted or
// replaced by other ST but it should be tracked for reporting
//
// =============================================================================
void
DNA_NODE::Map_st_node(STMTREP *sr, ST_IDX st)
{
  Is_True(IPSA_insession(), ("IPSA feature kicked in without IPSA"));
  SRCR_ID key = Gen_srcr_id(sr, NULL);
  STPATH_MAP::const_iterator iter = _stpath_map.find(key);
  
  if (iter == _stpath_map.end()) {
    _stpath_map[key] =  STPATH::New_instance(st, 0, 0, Mem_pool());
  }
  else if (!Vsa_check_sym_ignore(ST_name(st))) {
    // replace with the new name
    STPATH* old_entry = iter->second;
    if (Vsa_check_sym_ignore(ST_name(old_entry->St_idx())))
      old_entry->Set_idx(st);
  }
}

void
DNA_NODE::Map_st_node(WN *wn, ST_IDX st)
{
  Is_True(IPSA_insession(), ("IPSA feature kicked in without IPSA"));

  STPATH_MAP::iterator iter = _wn_stpath_map.find((SRCR_ID)wn); 
  if (iter == _wn_stpath_map.end()) {
    STPATH* new_entry = STPATH::New_instance(st, 0, 0, Mem_pool());
    _wn_stpath_map[(SRCR_ID)wn] = new_entry;
  }
  else if (!Vsa_check_sym_ignore(ST_name(st))) {
    STPATH* old_entry = iter->second;
    // replace with the new name
    old_entry->Set_idx(st);
  }
}

// =============================================================================
//
// DNA_NODE::Get_node_st, Get the ST annotated on the node
//
// =============================================================================
ST_IDX
DNA_NODE::Get_node_st(STMTREP *sr, CODEREP *cr)
{
  Is_True(IPSA_insession(), ("IPSA feature kicked in without IPSA"));

  SRCR_ID key = Gen_srcr_id(sr, cr);
  STPATH_MAP::iterator iter = _stpath_map.find(key);
  if (iter != _stpath_map.end())
    return iter->second->St_idx();
  return ST_IDX_ZERO;
}

static inline BOOL
Extract_st_info_from_cr(OPT_STAB *opt_stab, CODEREP *cr, STMTREP *sr, ST_IDX *st_idx_p, UINT16 *field_id_p, char **st_name_p, MEM_POOL *pool)
{
  if (cr->Kind() == CK_VAR) {
    *st_idx_p = ST_st_idx(opt_stab->Aux_stab_entry(cr->Aux_id())->St());
    *field_id_p = (UINT16) cr->Field_id();
  } else if (cr->Kind() == CK_IVAR) {
    CODEREP *base = cr->Ilod_base() ? cr->Ilod_base() : cr->Istr_base();
    if (base->Kind() != CK_VAR) {
      return FALSE;
    }
    *st_idx_p = ST_st_idx(opt_stab->Aux_stab_entry(base->Aux_id())->St());
    *field_id_p = (UINT16) cr->I_field_id();
  } else {
    return FALSE;
  }
  char *buf = (char*) MEM_POOL_Alloc(pool, VSA_VAR_NAME_MAX_LEN);
  STRING_BUFFER sbuf(buf, VSA_VAR_NAME_MAX_LEN);
  char *st_name = (char *) STPATH::Find_cr_stname(&sbuf, opt_stab, cr, sr);
  if (st_name == NULL) {
    char *name = ST_name(*st_idx_p);
    memcpy(buf, name, strlen(name) + 1);
  }
  *st_name_p = buf;
  return TRUE;
}

void
DNA_NODE::Map_stpath(OPT_STAB *opt_stab, CODEREP* cr_old, STMTREP* sr, CODEREP *cr_new)
{
  Is_True(IPSA_insession(), ("IPSA feature kicked in without IPSA"));
  // first check if already map the node
  // find out the old_entry

  // TODO: we may need to consider if
  // node = Gen_srcr_id(sr, cr_old) is exist.
  // attach it to the current PATH?
  
  SRCR_ID key = Gen_srcr_id(sr, cr_new);
  STPATH_MAP::iterator iter = _stpath_map.find(key);
  if (iter != _stpath_map.end())
      return; // this node has already been maped. 
  ST_IDX st_idx;
  UINT16 field_id;
  char *st_name = NULL;
  // find the cr old in map, if exist, use cr old's info
  SRCR_ID key_old = Gen_srcr_id(sr, cr_old);
  STPATH_MAP::iterator iter_old = _stpath_map.find(key_old);
  // use cr old symbol first, if cr old symbol is not temporary
  if (iter_old != _stpath_map.end()) {
    if (!SRCPOS_HANDLE::Is_temp_var(iter_old->second->St_name())) {
      st_idx = iter_old->second->St_idx();
      field_id = iter_old->second->Field_id();
      st_name = (char *) iter_old->second->St_name();
    }
  }
  if (st_name == NULL) {
    BOOL extracted = Extract_st_info_from_cr(opt_stab, cr_old, sr, &st_idx, &field_id, &st_name, Mem_pool());
    if (!extracted)
      return;
  }
  // first generate the path
  vector<SRCPOS_NODE> path;
  STMTREP *defstmt = cr_old->Get_defstmt();
  SRCPOS_NODE last_srcpos_node;
  BOOL is_first_def = TRUE;

  BOOL is_temp_st = SRCPOS_HANDLE::Is_temp_var(st_name);
  while ( defstmt) {
    if(!OPERATOR_is_scalar_store(defstmt->Opr()) && !OPERATOR_is_scalar_istore(defstmt->Opr()))
      break;
    if(is_first_def) {
      SRCPOS_NODE srcpos_node(this);;
      if (_comp_unit->Phase() == MAINOPT_PHASE) {
        srcpos_node.Set_stmt(defstmt, PATHINFO_COPY);
      }
      else {
        // preopt, INLCXT/STMT is freed in VSA phase. copy local cxt to global
        // defstmt's SRCPOS is in cxt and BB's inlcxt is cxt's parent
        INLCXT local_cxt(defstmt->Bb()->Inlinecxt(), defstmt->Linenum(), 0, FALSE);
        INLCXT* global_cxt = Copy_inlcxt_node(&local_cxt);
        defstmt->Bb()->Set_inlinecxt(global_cxt->Parent());
        srcpos_node.Set_inlcxt(global_cxt, PATHINFO_COPY_PROP);
      }
      if(!srcpos_node.equal(last_srcpos_node)) {
        path.push_back(srcpos_node);
        last_srcpos_node = srcpos_node;
      }
    }
    SRCR_ID def_key = Gen_srcr_id(defstmt, defstmt->Rhs());
    STPATH_MAP::iterator iter_def = _stpath_map.find(def_key);

    if(iter_def != _stpath_map.end()) {
      STPATH* spath = iter_def->second;
      ST_IDX orig_st = spath->St_idx();
      BOOL is_temp_orig = SRCPOS_HANDLE::Is_temp_var(spath->St_name());
      // current is temp, prev is not temp, use prev update current
      if(is_temp_st && !is_temp_orig) {
        st_idx = spath->St_idx();
        field_id = spath->Field_id();
        st_name = (char *) spath->St_name();
      }
      // current is not temp, prev is temp, use current update prev
      else if(!is_temp_st && is_temp_orig) {
        spath->Update_sym(st_idx, field_id, st_name);
      }
      if(is_first_def) {
        for(int i =0; i < spath->Path_size(); i++)
          path.push_back(spath->Path(i));
      }
      is_first_def = FALSE;
    } else {
      // prop current st info back to its define stmt
      if(!is_temp_st && !sr->Bb()->Inlinecxt()) {
        CODEREP *defcr = defstmt->Rhs();
        STPATH *def_entry = STPATH::New_instance(st_idx, field_id, st_name, 0, Mem_pool());
        _stpath_map[def_key] = def_entry;
        Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
                (TFile, "[STPATH Trace] COPYPROP::Vsa_annotate_srcr: prop back to SR %d, cr %d, with ST %s\n",
                  defstmt->Stmtrep_id(), defcr->Coderep_id(), st_name));
      }
    }
    defstmt = defstmt->Rhs()->Get_defstmt();
  }
  STPATH *entry = STPATH::New_instance(st_idx, field_id, st_name, path.size(), Mem_pool());
  for (int i=0; i < path.size(); i++) {
    entry->Add_path(path[i], i);
  }
  _stpath_map[key] = entry;
}

// =============================================================================
//
// DNA_NODE::Update_stpath, Move stpath from _stpath_map to _wn_stpath_map
//                          Used in PREOPT emitter
//
// =============================================================================
BOOL 
DNA_NODE::Update_stpath(STMTREP* sr, CODEREP* cr, WN* wn)
{
  Is_True(IPSA_insession(), ("IPSA feature kicked in without IPSA"));
  SRCR_ID key = Gen_srcr_id(sr, cr);
  
  STPATH_MAP::iterator iter = _stpath_map.find(key);
  
  if (iter != _stpath_map.end()) {
    STPATH* stpath = iter->second;
    STPATH_MAP::iterator iter0 = _wn_stpath_map.find((SRCR_ID)wn);
      
    if(iter0 != _wn_stpath_map.end()) {
      STPATH* stpath0 = iter0->second;
      stpath0->Dec_use_cnt();
      _wn_stpath_map.erase(iter0);
    }
    _stpath_map.erase(iter);
    _wn_stpath_map[(SRCR_ID)wn] = stpath;
    return TRUE;
  }
  return FALSE;
}

// =============================================================================
//
// DNA_NODE::Update_stpath, Move stpath from _wn_stpath_map to _stpath_map
//                          Used in MAINOPT rename phase 
//
// =============================================================================
BOOL
DNA_NODE::Update_stpath(WN* wn, STMTREP* sr, CODEREP* cr)
{
  Is_True(IPSA_insession(), ("IPSA feature kicked in without IPSA"));
  SRCR_ID key = Gen_srcr_id(sr, cr);
  
  STPATH_MAP::iterator iter_wn = _wn_stpath_map.find((SRCR_ID)wn);
  if (iter_wn != _wn_stpath_map.end()) {
    STPATH* stpath = iter_wn->second;
    _wn_stpath_map.erase(iter_wn);
    
    STPATH_MAP::iterator iter = _stpath_map.find(key);
    if(iter == _stpath_map.end()) {
      _stpath_map[key] = stpath;
    }
    else if (stpath != iter->second){
      // the sr cr has already attach a STPATH
      // select priorty:
      // 1: pick non-temp variable name stpath
      // 2: pick longer path
      STPATH* stpath2 = iter->second;
      BOOL temp_stpath1 = SRCPOS_HANDLE::Is_temp_var(stpath->St_idx());
      BOOL temp_stpath2 = SRCPOS_HANDLE::Is_temp_var(stpath2->St_idx());
      BOOL use_stpath1 = FALSE;
      if ((temp_stpath1 && temp_stpath2) || (!temp_stpath1 && !temp_stpath2)) {
        if(stpath->Path_size() > stpath2->Path_size()) {
          use_stpath1 = TRUE;
        }
      } else if (temp_stpath2) {
        use_stpath1 = TRUE;
      }
      if(use_stpath1) {
        _stpath_map.erase(iter);
        stpath2->Dec_use_cnt();
        if(stpath2->Use_cnt() == 0)
          CXX_DELETE(stpath2, Mem_pool());
        _stpath_map[key] = stpath;
      } else {
        stpath->Dec_use_cnt();
        if(stpath->Use_cnt() == 0)
          CXX_DELETE(stpath, Mem_pool());
      }
    }
    return TRUE;
  }
  return FALSE;
}

// =============================================================================
//
// DNA_NODE::Get_stpath, Get the ST annotated on the node
//
// =============================================================================
STPATH*
DNA_NODE::Get_stpath(const STMTREP* sr, const CODEREP* cr) const
{
  SRCR_ID node = Gen_srcr_id(sr, cr);
  Is_True(IPSA_insession(), ("IPSA feature kicked in without IPSA"));
  STPATH_MAP::const_iterator iter = _stpath_map.find(node);
  if(iter != _stpath_map.end())
    return iter->second;
  else
    return NULL;
}

STPATH*
DNA_NODE::Get_stpath(IDTYPE srid, IDTYPE crid) const
{
  SRCR_ID node = Gen_srcr_id(srid, crid);
  Is_True(IPSA_insession(), ("IPSA feature kicked in without IPSA"));
  STPATH_MAP::const_iterator iter = _stpath_map.find(node);
  if(iter != _stpath_map.end())
    return iter->second;
  else
    return NULL;
}

// =============================================================================
//
// DNA_NODE::Get_stpath, Get the STPATH annotated on the WN
//
// =============================================================================
STPATH*
DNA_NODE::Get_stpath(const WN* wn) const
{
  Is_True(IPSA_insession(), ("IPSA feature kicked in without IPSA"));
  STPATH_MAP::const_iterator iter = _wn_stpath_map.find((SRCR_ID)wn);
  if(iter != _wn_stpath_map.end())
    return iter->second;
  else
    return NULL;
}

// =============================================================================
//
// DNA_NODE::Get_stpath, make a duplication of STPATH
//
// =============================================================================
BOOL
DNA_NODE::Dup_stpath(const WN *src_wn, const WN* dst_wn)
{
  Is_True(IPSA_insession(), ("IPSA feature kicked in without IPSA"));
  STPATH_MAP::iterator iter = _wn_stpath_map.find((SRCR_ID)src_wn);
  // src and dst map to the same stpath
  if(iter != _wn_stpath_map.end()) {
    STPATH* stpath = iter->second;
    stpath->Inc_use_cnt();
    _wn_stpath_map[(SRCR_ID)dst_wn] = stpath;
    if(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG)){
      fprintf(TFile, "[STPATH Trace] Dup_st_node on wn %p to wn %p\n", src_wn, dst_wn);
      fdump_tree_no_st(TFile, (WN *)src_wn);
    }
    return TRUE;
  }
  return FALSE;
}

// =============================================================================
//
// DNA_NODE::Remove_dead_stpath, This function is called after PREOPT emit
// it will remove all the STPATH node that not has Is_map_wn set. 
//
// =============================================================================
void
DNA_NODE::Remove_dead_stpath(void)
{
  Is_True(IPSA_insession(), ("IPSA feature kicked in without IPSA"));

  STPATH_MAP::iterator iter = _stpath_map.begin();
  while(iter != _stpath_map.end()) {
    STPATH *stpath = iter->second;
    stpath->Dec_use_cnt();
    if(stpath->Use_cnt() == 0){
      Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG), 
               (TFile, "[STPATH Trace] remove dead STPATH on SR:%ld, cr %ld \n",
               (iter->first)>>32, (iter->first) & 0xffffffff));
      CXX_DELETE(stpath, Mem_pool());
    }
    _stpath_map.erase(iter);
    iter = _stpath_map.begin();
  }
}

// =============================================================================
//
// DNA_NODE::Prop_stpath
//       Update STPATH for following situation
//           cr1     <-- attached STPATH with key <cr1, defstmt0>
//          new_cr
//         defstmt
//         ....
//           old_cr
//         stmt1
//       
//         old_cr replace with new_cr
//         attached STPATH0 with key <cr1, stmt1>
// =============================================================================
void
DNA_NODE::Prop_stpath(STMTREP* defstmt, STMTREP* stmt1, CODEREP * cr)
{
  STPATH_MAP::iterator iter0;
  STPATH_MAP::iterator iter1;
  SRCR_ID id0, id1;

  id0 = Gen_srcr_id(defstmt, cr);
  id1 = Gen_srcr_id(stmt1, cr);
  iter0 = _stpath_map.find(id0);
  if(iter0 != _stpath_map.end()) {
    STPATH* stpath = iter0->second; 
    iter1 = _stpath_map.find(id1);
    if(iter1 == _stpath_map.end()) {
      Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
               (TFile, "[STPATH Trace] Prop STPATH %p, from stmt %d, to stmt %d for CR %d with ST %s\n",
                stpath, defstmt->Stmtrep_id(), stmt1->Stmtrep_id(), cr->Coderep_id(),
                stpath->St_name()));
      stpath->Inc_use_cnt();
      _stpath_map[id1] = stpath;
    }
  }

  switch (cr->Kind()) {
  case CK_LDA:
  case CK_CONST:
  case CK_RCONST:
  case CK_VAR:
    break;
  case CK_IVAR:
    Prop_stpath(defstmt, stmt1, cr->Ilod_base());
    break;
  case CK_OP:
    for(int i = 0; i < cr->Kid_count(); i++)
      Prop_stpath(defstmt, stmt1, cr->Opnd(i));
    break;
  default:
    break;
  }
}


static const char* path_info_msg[] = {
  "No message",                       //0,  PATHINFO_NONE,
  "Defined by copy",                  //1,  PATHINFO_COPY,
  "Function call",                    //2,  PATHINFO_DNA_CALLSITE,
  "Vulnerable spot",                  //3,  PATHINFO_VUL_SPOT,
  "Symbol declare line",              //4,  PATHINFO_ST_DECLARE,
  "Defined by call site effect",      //5,  PATHINFO_CALL_CHI,
  "May defined by this statement",    //6,  PATHINFO_CHI,
  "Phi BB",                           //7,  PATHINFO_PHI,
  "Defined by istore",                //8,  PATHINFO_ISTORE,
  "Defined by alloc/new",             //9,  PATHINFO_ALLOC,
  "Free memory",                      //10, PATHINFO_FREE,
  "The control dependency BB",        //11, PATHINFO_CD_BB,
  "Function exit",                    //12, PATHINFO_FUN_EXIT,
  "Function inline begin",            //13, PATHINFO_INL_BEGIN,
  "Function inline end",              //14, PATHINFO_INL_END,
  "Pass by parameter",                //15, PATHINFO_PARM,
  "Rule base check spot",             //16, PATHINFO_RBC,
  "Jump from here",                   //17, PATHINFO_BRANCH,
  "Function return value",            //18, PATHINFO_DNA_CALLRETURN,
  "Throws exception",                 //19, PATHINFO_EH_THROW,
  "Catches exception",                //20, PATHINFO_EH_CATCH,
  "Related operation identified",     //21, PATHINFO_TRANSIT,
  "Defined by &(variable)",           //22, PATHINFO_LDA,
  "Condition evaluates to true",      //23, PATHINFO_COND_TRUE,
  "Condition evaluates to false",     //24, PATHINFO_COND_FALSE,
  "Then block is taken",              //25, PATHINFO_THEN_TAKEN.
  "Else block is taken",              //26, PATHINFO_ELSE_TAKEN
  "Value propagated",                 //27, PATHINFO_COPY_PROP,
  "Dummy path",                       //28, PATHINFO_DUMMY,
  "Message max"                       //29, PATHINFO_MAX
};

SRCPOS
SRCPOS_NODE::Spos() const
{
  if (Is_srcpos()) {
    return _u1._srcpos;
  }
  else if (Is_st()) {
    SRCPOS spos = ST_Srcpos(*_u1._st);
    if (spos != 0)
      return spos;
    if (Dna())
      return Dna()->Comp_unit()->Cfg()->Entry_spos();
    return 0;
  }
  Is_True(_u1._stmt != NULL, ("bb or stmt is not set"));
  if (Is_bb_first_linenum()) {
    // use first stmtrep srcpos in phi/then/else bb
    return Get_bb_first_linenum(_u1._bb);
  }
  else if (Is_bb_last_linenum()) {
    // usr last stmtrep srcpos in branch bb
    return Get_bb_last_linenum(_u1._bb);
  }
  else if (Is_inlcxt()) {
    return _u1._inlcxt->Inlcxt_line_num();
  }
  else {
    Is_True(Is_stmt(), ("invalid info"));
    return _u1._stmt->Linenum();
  }
}

INLCXT *
SRCPOS_NODE::Inlcxt() const
{
  if (Is_srcpos() || Is_st())
    return NULL;
  Is_True(_u1._stmt != NULL, ("bb or stmt is not set"));
  if (Is_bb())
    return _u1._bb->Inlinecxt();
  else if (Is_inlcxt())
    return _u1._inlcxt->Parent();
  else
    return _u1._stmt->Bb()->Inlinecxt();
}

const char*
SRCPOS_NODE::Fname() const
{
  INLCXT* cxt = Inlcxt();
  if (cxt != NULL)
    return ST_name(_dna->File_idx(), cxt->Inlcxt_call_st());
  return _dna->Fname();
}

const char*
SRCPOS_NODE::Vname() const
{
  ST *st = NULL;
  if (Is_srcpos())
    return NULL;
  if (Is_st()) {
    Is_True(_dna != NULL, ("invalid dna"));
    st = St();
  }
  else if (Is_stmt()) {
    Is_True(_dna != NULL, ("invalid dna"));
    if (Stmt()->Opr() == OPR_STID) {
      AUX_ID aux = Stmt()->Lhs()->Aux_id();
      AUX_STAB_ENTRY *aux_entry = Dna()->Comp_unit()->Opt_stab()->Aux_stab_entry(aux);
      if (aux_entry)
        st = aux_entry->St();
    }
  }
  if (st != NULL) {
    const char *vname = Str_ptr(_dna->File_idx(), ST_name_idx(st));
    if (!Vsa_check_sym_ignore(vname))
      return vname;
  }
  return NULL;
}

void
IPSA::Build_dfs_vector(BOOL set_fun_attr)
{
  if(!_post_order_dnode.empty())
    _post_order_dnode.clear();
  if(!_pre_order_dnode.empty())
    _pre_order_dnode.clear();
  
  INT node_size = _dnaid_to_dnanode.size(); 

  mUINT8 *visited = new mUINT8[node_size];
  BZERO(visited, sizeof(mUINT8) * node_size);
  visited[0] = 2; // the first idx is invalid

  UINT32 *indegree = new UINT32[node_size];
  BZERO(indegree, sizeof(UINT32) * node_size);

  for (INT i = IPSA::DNA_INIT_IDX; i < node_size; ++i ) {
    DNA_NODE *func = _dnaid_to_dnanode[i]; 
    if (func->Is_root()) {
      if (Get_Trace(TP_WOPT2, VSA_DUMP_FLAG)) {
        fprintf(TFile, "DNODE_ITER::Build_dfs_vector: Begin with root node %d\n", i);
      }
          
      Is_True(!visited[i], ("Root node is already visited!"));
      Dfs_walk(i, visited, set_fun_attr);
    }
  }

  if(_pre_order_dnode.size() < node_size -1) {
    // there are some recursive circle outside of root graph, add them
    for (INT i = IPSA::DNA_INIT_IDX; i < node_size; ++i ) {
      if (!visited[i]) {
        if (Get_Trace(TP_WOPT2, VSA_DUMP_FLAG)) {
          fprintf(TFile, "DNODE_ITER::Build_dfs_vector: Begin with no root node %d\n", i);
        }
        Dfs_walk(i, visited, set_fun_attr);
      }
    }
  }

  Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
           (TFile, ("DFS pre order:\n")));
  Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
               Print_dnode_vector(_pre_order_dnode, TFile));
  Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
           (TFile, ("DFS post order:\n")));
  Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
               Print_dnode_vector(_post_order_dnode, TFile));
   
  delete[] visited;
  visited = NULL; 
}

void
IPSA::Build_top_vector(BOOL set_fun_attr)
{
  if (!_top_order_dnode.empty())
    _top_order_dnode.clear();

  INT node_size = _dnaid_to_dnanode.size();
  UINT32 *indegree = new UINT32[node_size];
  BZERO(indegree, sizeof(UINT32) * node_size);

  std::queue<DNA_NODE*> queue;

#if 0
  for (INT i = IPSA::DNA_INIT_IDX; i < node_size; ++i ) {
    DNA_NODE *func = _dnaid_to_dnanode[i];
    UINT32 my_degree = 0;
    // traverse all caller to calculate indegree
    for (CALLER_ITER iter(this, func); !iter.Is_end(); iter.Next()) {
      if (!iter.Current_callsite()->Is_back_edge())
        ++ my_degree;
    }
    indegree[i] = my_degree;
    if (indegree[i] == 0)
      queue.push(func);
  }
#else
  for (INT i = IPSA::DNA_INIT_IDX; i < node_size; ++i ) {
    DNA_NODE *func = _dnaid_to_dnanode[i];
    for (CALLEE_ITER iter(this, func); !iter.Is_end(); iter.Next()) {
      if (!iter.Current_callee()->Is_back_edge())
        ++ indegree[iter.Current_callee()->Callee()];
    }
  }

  for (INT i = IPSA::DNA_INIT_IDX; i < node_size; ++i ) {
    if (indegree[i] == 0)
      queue.push(_dnaid_to_dnanode[i]);
  }
#endif

  while (!queue.empty()) {
    DNA_NODE *func = queue.front();
    _top_order_dnode.push_back(func);
    queue.pop();
    Is_True(indegree[func->Dna_idx()] == 0,
            ("dna in queue indegree is not 0"));
    // traverse all callee to update indegree
    for (CALLEE_ITER iter(this, func); !iter.Is_end(); iter.Next()) {
      if (!iter.Current_callee()->Is_back_edge()) {
        DNA_NODE* callee = iter.Current();
        if ((--indegree[iter.Current()->Dna_idx()]) == 0)
          queue.push(callee);
      }
    }
  }

  Is_True(_top_order_dnode.size() == node_size - 1,
          ("Not all nodes are added"));

  Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
           (TFile, ("Toplogical order:\n")));
  Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
               Print_dnode_vector(_top_order_dnode, TFile));

  delete[] indegree;
}

void
IPSA::Dfs_walk(INT node, mUINT8* visited, BOOL set_fun_attr)
{
  visited[node] = 1;
  DNA_NODE *func = _dnaid_to_dnanode[node];
  
  _pre_order_dnode.push_back(_dnaid_to_dnanode[node]);

  
  for (CALLEE_ITER iter(this, func); !iter.Is_end(); iter.Next()) {
    //RNA_NODE * call_site = (*func->Call_list())[i];
    DNA_NODE *callee = iter.Current();
    INT callee_idx = callee ? callee->Dna_idx() : 0;
    if( set_fun_attr) {
      // visited[callee_idx] == 2 means the callee_idx and all it's successor
      // is visited. so 1 means this is a back edge.
      INT flag = 0;
      if (visited[callee_idx] == 1) {
        flag = DNA_IN_RECURSION;
        iter.Current_callee()->Update_flags(RNA_BACK_EDGE);
      }
      if(flag) {
       
        if(flag & DNA_IN_RECURSION)
          iter.Current_callsite()->Set_is_back_edge();
       
        for (DNODE_VECTOR::reverse_iterator iter = _pre_order_dnode.rbegin();
             iter != _pre_order_dnode.rend(); iter++ ) {
          DNA_NODE *dn = *iter;
          if(visited[dn->Dna_idx()] == 1 ) {
            if (Get_Trace(TP_WOPT2, VSA_DUMP_FLAG))
              fprintf(TFile, "DNODE_ITER::Dfs_walk: Setting flag 0x%x for dn %d, %s\n",
                      flag, dn->Dna_idx(), dn->Fname());
              dn->Set_flag(flag);
          }

          if(dn->Dna_idx() == callee_idx &&
             (flag & DNA_IN_RECURSION))
          {
            // This is the entry of recursion
            dn->Set_flag(DNA_RECURSION_ENTRY);
            // from now on, remove DNA_IN_RECURSION flag
            flag &= ~DNA_IN_RECURSION;  
          }
        }
      }
    }
    if (!visited[callee_idx])
      Dfs_walk(callee_idx, visited, set_fun_attr);
  }
  
  _post_order_dnode.push_back(_dnaid_to_dnanode[node]);
  visited[node] = 2; 
}

void
CALLEE_ITER::Next(void)
{
  Is_True(_cur_call_site_idx < _func->_call_list.size(), ("CALLEE_ITER Out of Range"));
  RNA_NODE* call_site = _func->Callsite(_cur_call_site_idx);
  if(_cur_indirect_idx < call_site->Callee_cnt() - 1 )
    _cur_indirect_idx++;
  else {
    _cur_indirect_idx = 0;
    _cur_call_site_idx++;
  }
  while(_cur_call_site_idx < _func->Call_site_cnt() &&
        (call_site = _func->Callsite(_cur_call_site_idx)) &&
        call_site->Callee_cnt() == 0)
    // filter out indirect call which has no callee candidate
    _cur_call_site_idx++;
}
