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
#include "cxx_memory.h"
#include "report.h"
#include "erbe.h"
#include "opt_dna.h"
#include "opt_vsa.h"
#include "opt_vsa_eh.h"
#include "config_vsa.h"
#include "opt_vsa_rbc.h"
#include "java_defs.h"
#include "class_hierarchy.h"
#include "j_class_hierarchy_bldr.h"

// ----------------------------------------------------------------------------
// EH_TABLE_BUILDER
//   Build the EH table by traversing all BBs and Calls
// ----------------------------------------------------------------------------
class EH_TABLE_BUILDER
{
private:
  EH_TABLE *_table;
  CFG      *_cfg;
  std::vector<BB_REGION*> _ri_stack;  // use vector instead of stack
                                      // for better traversal

private:
  BOOL Is_in_try_region();
  void Connect_node(STMTREP* call, STMTREP* opt_chi, BB_REGION* rinfo);
  void Process_call(STMTREP* call);
  void Process_region_start(BB_NODE* bb, BOOL fwd);
  void Process_bb(BB_NODE* bb);

public:
  EH_TABLE_BUILDER(EH_TABLE* t)
   : _table(t), _cfg(t->Comp_unit()->Cfg()) { }

  void Build();
};

// ----------------------------------------------------------------------------
// EH_TABLE_BUILDER::Is_in_try_region
//   Check if current BB/stmt is in try region
// ----------------------------------------------------------------------------
BOOL
EH_TABLE_BUILDER::Is_in_try_region()
{
  std::vector<BB_REGION*>::reverse_iterator it;
  for (it = _ri_stack.rbegin(); it != _ri_stack.rend(); ++it) {
    BB_REGION* ri = *it;
    Is_True(ri->Rid() != NULL, ("no RID"));
    if (RID_TYPE_try(ri->Rid()))
      return TRUE;
    if (RID_TYPE_cleanup(ri->Rid()))
      return FALSE;
    Is_True(FALSE,
            ("TODO: handle region type 0x%x", RID_type(ri->Rid())));
    // No idea about exc_spec, mask, guard, null_cleanup
  }
  return FALSE;
}

// ----------------------------------------------------------------------------
// EH_TABLE_BUILDER::Connect_node
//   Connect the call stmt with its handler and BB_REGION is annotated
// ----------------------------------------------------------------------------
void
EH_TABLE_BUILDER::Connect_node(STMTREP* call, STMTREP* opt_chi, BB_REGION* rinfo)
{
  _table->Create_eh_path(call, opt_chi, rinfo);
}

// ----------------------------------------------------------------------------
// EH_TABLE_BUILDER::Process_call
//   Process call stmt and find out its handler
// ----------------------------------------------------------------------------
void
EH_TABLE_BUILDER::Process_call(STMTREP* call)
{
  Is_True(Is_in_try_region(), ("call stmt is not in try region"));

  // do not handle intrinsic call here
  if (call->Opr() == OPR_INTRINSIC_CALL)
    return;

  // do not count some EH runtime calls
  if (OPERATOR_has_sym(call->Opr()) &&
      (Is_EH_rt_call(ST_name(call->St())) ||
       PU_nothrow(Pu_Table[ST_pu(call->St())])))
    return;

  // find all handlers
  // add call with its handlers/exc object types to table
  BB_REGION* ri = _ri_stack.back();
  Is_True(ri->Rid() != NULL, ("no RID"));
  if (ri->Rid() == NULL)
    return;
  if (!RID_TYPE_try(ri->Rid())) {
    Is_True(RID_TYPE_cleanup(ri->Rid()),
            ("TODO: handle exc_spec, mask, guard, null_cleanup"));
    return;
  }

  WN* r_prags = ri->Region_pragma_list();
  Is_True(r_prags != NULL && WN_operator(r_prags) == OPR_BLOCK,
          ("no region pragmas found"));
#if 0
  // call is in a try block without another level of region
  // bad IR?
  Is_True(WN_first(r_prags) == NULL,
          ("should not add region info for try directly"));
#endif
  // connect with clean-ups
  INITO_IDX iidx = ri->Ereg_supp();
  INITV_IDX iv_idx = INITO_val(iidx);
  if (iv_idx != 0 && INITV_kind(iv_idx) == INITVKIND_BLOCK) {
    iv_idx = INITV_blk(iv_idx);
    if (INITV_kind(iv_idx) == INITVKIND_LABEL) {
      INT32 label = INITV_lab(iv_idx);
      BB_NODE* handler = _cfg->Get_bb_from_label(label);
      Is_True(handler != NULL, ("not find bb for L%d", label));
      STMTREP* opt_chi = handler->First_stmtrep();
      Is_True(opt_chi != NULL && opt_chi->Opr() == OPR_OPT_CHI,
              ("invalid entry chi stmtrep"));
      Connect_node(call, opt_chi, ri);
    }
  }
}

// ----------------------------------------------------------------------------
// EH_TABLE_BUILDER::Process_region_start
//   Process REGIONSTART bb to keep track of BB_REGION information
// ----------------------------------------------------------------------------
void
EH_TABLE_BUILDER::Process_region_start(BB_NODE* bb, BOOL fwd)
{
  Is_True(bb->Kind() == BB_REGIONSTART,
          ("not region start"));
  BB_REGION* rinfo = bb->Regioninfo();
  Is_True(rinfo != NULL && rinfo->Rid(),
          ("Region info is NULL"));
  if (rinfo->Rid() && RID_TYPE_eh(rinfo->Rid())) {
    if (fwd) {
      _ri_stack.push_back(rinfo);
    }
    else {
      Is_True(rinfo == _ri_stack.back(),
              ("region info stack top mismatch"));
      _ri_stack.pop_back();
    }
  }
}

// ----------------------------------------------------------------------------
// EH_TABLE_BUILDER::Process_bb
//   Traverse BB on DOM tree
// ----------------------------------------------------------------------------
void
EH_TABLE_BUILDER::Process_bb(BB_NODE* bb)
{
  if (bb->Kind() == BB_REGIONSTART) {
    Process_region_start(bb, TRUE);
  }

  if (Is_in_try_region()) {
    STMTREP *stmt;
    STMTREP_ITER stmt_iter(bb->Stmtlist());
    FOR_ALL_NODE (stmt, stmt_iter, Init()) {
      if (OPERATOR_is_call(stmt->Opr()))
        Process_call(stmt);
    }
  }

  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM (dom_bb, dom_bb_iter, Init(bb->Dom_bbs())) {
    Process_bb(dom_bb);
  }

  if (bb->Kind() == BB_REGIONSTART) {
    Process_region_start(bb, FALSE);
  }
}

// ----------------------------------------------------------------------------
// EH_TABLE_BUILDER::Build
//   Build the EH table
// ----------------------------------------------------------------------------
void
EH_TABLE_BUILDER::Build()
{
  Process_bb(_cfg->Fake_entry_bb());
}

// ----------------------------------------------------------------------------
// COMP_UNIT::Build_eh_table
//   Build EH table for current function
// ----------------------------------------------------------------------------
void
COMP_UNIT::Build_eh_table()
{
  Is_True(_eh_table == NULL,
          ("EH table already built"));

  if (!VSA_EH)
    return;

  // always construct this even if for C function
  // because it may call C++ and be called by C++
  _eh_table = CXX_NEW(EH_TABLE(this), this->Mem_pool());

  // no fake entry, means no EH handlers
  if (Cfg()->Fake_entry_bb()) {
    EH_TABLE_BUILDER bldr(_eh_table);
    bldr.Build();
  }

  Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
           (TFile, "EH table dump: >>>>>>>>\n"));
  Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
               _eh_table->Print(TFile));
}

// ----------------------------------------------------------------------------
// EH_PATH::Find_catch_symbol_and_body
//   Find the real catch symbol and it's catch body
//   BB_HANDLER's succ: BB_STORE_EH_PTR
//   BB_STORE_EH_PTR's succ: BB_FILTER
//   BB_FILTER's succ: BB_FILTER, BB_FILTER, OTHER_BB
// ----------------------------------------------------------------------------
void
EH_PATH::Find_catch_symbol_and_body(STMTREP *handler, EH_TYPE_VECTOR *filter_info, COMP_UNIT *cu)
{
  BB_NODE *bb_handler = handler->Bb();
  BB_LIST *dom_bb_list = bb_handler->Dom_bbs();
  Is_True_Ret(dom_bb_list->Len() == 1,
    ("Handler bb's succssor number is not 1, dom bb list len: %d", dom_bb_list->Len()));
  hash_set<IDTYPE> visited_bb;
  BB_NODE *bb_store_eh_ptr = dom_bb_list->Node();
  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb_store_eh_ptr->Dom_bbs())) {
    Find_catch_symbol_and_body(dom_bb, filter_info, cu, visited_bb);
  }
}

// ----------------------------------------------------------------------------
// EH_PATH::Find_catch_symbol_and_body
//   Find the real catch symbol and it's catch body
// ----------------------------------------------------------------------------
void
EH_PATH::Find_catch_symbol_and_body(BB_NODE *bb, EH_TYPE_VECTOR *filter_info, COMP_UNIT *cu, hash_set<IDTYPE> &visited_bb)
{
  visited_bb.insert(bb->Id());
  STMTREP *stmt;
  STMTREP_ITER iter(bb->Stmtlist());
  BOOL traverse_succ = FALSE;
  FOR_ALL_NODE(stmt, iter, Init()) {
    // detect the bb is catch block or not
    if (stmt->Opr() == OPR_TRUEBR || stmt->Opr() == OPR_FALSEBR) {
      CODEREP *cr_op = stmt->Rhs();
      if (cr_op->Kind() == CK_OP && (cr_op->Opr() == OPR_EQ || cr_op->Opr() == OPR_NE)) {
        CODEREP *cr0 = cr_op->Opnd(0);
        CODEREP *cr1 = cr_op->Opnd(1);
        //    LDID __Exc_Filter__
        //    LDC CONST
        //  OPR_TRUEBR/OPR_FALSEBR
        if (cr0->Kind() == CK_VAR && cr1->Kind() == CK_CONST) {
          AUX_STAB_ENTRY *aux = cu->Opt_stab()->Aux_stab_entry(cr0->Aux_id());
          ST *st = aux->St();
          if (strcmp(ST_name(st), "__Exc_Filter__") == 0) {
            INT64 filter_idx = cr1->Const_val();
            Is_True_Ret(0 < filter_idx && filter_idx <= filter_info->size(),
              ("Filter index out of bound, filter index: %d", filter_idx));
            OPERATOR sr_opr = stmt->Opr();
            OPERATOR cr_opr = cr_op->Opr();
            INT32 label_number = stmt->Label_number();
            IDTYPE bb_id = cu->Cfg()->Get_bb_from_label(label_number)->Id();
            if ((sr_opr == OPR_TRUEBR && cr_opr == OPR_NE) || (sr_opr == OPR_FALSEBR && cr_opr == OPR_EQ)) {
              Is_True_Ret(bb->Succ() != NULL && bb->Succ()->Len() == 2, ("This bb has more than 2 successors, bb: %d", bb->Id()));
              BB_NODE *succ_bb;
              BB_LIST_ITER succ_bb_iter;
              FOR_ALL_ELEM(succ_bb, succ_bb_iter, Init(bb->Succ())) {
                if (succ_bb->Id() != bb_id) {
                  bb_id = succ_bb->Id();
                  break;
                }
              }
            }
            Fs_bb_vector()->push_back(std::make_pair(filter_info->at(filter_idx - 1), bb_id));
            traverse_succ = TRUE;
            visited_bb.insert(bb_id);
          }
        }
      }
    } else if (stmt->Opr() == OPR_LABEL) {
      traverse_succ = TRUE;
    } else if (stmt->Opr() == OPR_GOTO) {
      traverse_succ = TRUE;
    } else {
      traverse_succ = FALSE;
    }
    if (!traverse_succ) {
      return;
    }
  }
  if (!traverse_succ) {
    return;
  }
  BB_NODE *succ_bb;
  BB_LIST_ITER succ_bb_iter;
  FOR_ALL_ELEM(succ_bb, succ_bb_iter, Init(bb->Succ())) {
    // this dom is catch block, just ignored
    if (visited_bb.find(succ_bb->Id()) != visited_bb.end()) {
      continue;
    }
    Find_catch_symbol_and_body(succ_bb, filter_info, cu, visited_bb);
  }
}

// ----------------------------------------------------------------------------
// EH_PATH::Print
//   Print EH_PATH
// ----------------------------------------------------------------------------
void
EH_PATH::Print(FILE* fp) const
{
  fprintf(fp, ">>> From call:\n");
  Throw_stmt()->Print(fp);
  fprintf(fp, ">>> To handler in BB%d flags=0x%x _mask=0x%x\n",
          Handler_stmt()->Bb()->Id(), _flag, _mask);
  if (PU_java_lang(Get_Current_PU())) {
    fprintf(fp, ">>> Java handler detail:");
    FS_BB_VECTOR::const_iterator iter;
    for (iter = _fs_bb_vec.begin(); iter != _fs_bb_vec.end(); ++iter) {
      if (iter->second == 0) {
        continue;
      }
      FS_PAIR t = iter->first;
      fprintf(fp, " type(%s)->bb(%d)", t.second > 0 ? STR_idx_str(t.first, t.second) : "(nil)", iter->second);
    }
    fprintf(fp, "\n");
  }
}

// ----------------------------------------------------------------------------
// EH_TABLE::Type_info(INT filter) const
// ----------------------------------------------------------------------------
void
EH_TABLE::Init_eh_info()
{
  INITO_IDX misc = PU_misc_info(Pu_Table[ST_pu(WN_st(_cu->Input_tree()))]);
  if (misc == INITO_IDX_ZERO)
    return;

  if (ST_sclass(INITO_st(misc)) != SCLASS_EH_REGION_SUPP)
    return;

  // typeinfo table
  INITV_IDX ty_tab = INITV_next(INITV_next(INITO_val(misc)));
  INITO_IDX idx = TCON_uval (INITV_tc_val(ty_tab));
  if (idx) {
    INITO* ino = &Inito_Table[idx];
    ST* st = INITO_st(ino);
    Is_True(!strcmp(ST_name (*st), "__TYPEINFO_TABLE__") &&
            (ST_sclass(st) == SCLASS_EH_REGION_SUPP),
            ("Unexpected ST in PU"));
    INITV_IDX blk = INITO_val(*ino);
    _typeinfo.push_back(std::make_pair(0, 0));  // reserve entry 0
    do {
      INITV_IDX st_entry = INITV_blk(blk);
      ST_IDX st_idx = 0;
      int filter = TCON_ival (INITV_tc_val (INITV_next (st_entry)));
      Is_True(_typeinfo.size() == filter,
              ("wrong order in typeinfo table"));
      // for java, EH type table saved the type pointer symbol name, should to find type pointer
      // and then find pointed type name, and then get the exception name
      if (INITV_kind (st_entry) != INITVKIND_ZERO) {
        st_idx = TCON_uval(INITV_tc_val (st_entry));
        if (PU_src_lang(Pu_Table[ST_pu(WN_st(_cu->Input_tree()))]) & PU_JAVA_LANG) {
          TY_IDX ty_idx = ST_type(st_idx);
          Is_True(TY_kind(ty_idx) == KIND_POINTER, 
            ("EH st type is not pointer type, st : %s, type : %s", ST_name(st_idx), TY_name(ty_idx)));
          _typeinfo.push_back(std::make_pair(File_Index, TY_name_idx(TY_pointed(ty_idx))));
          _filter_info.push_back(std::make_pair(File_Index, TY_name_idx(TY_pointed(ty_idx))));
        } else {
          _typeinfo.push_back(std::make_pair(File_Index, ST_name_idx(ST_ptr(st_idx))));
          _filter_info.push_back(std::make_pair(File_Index, ST_name_idx(ST_ptr(st_idx))));
        }
      }
      else {
        _typeinfo.push_back(std::make_pair(0, 0));
        _filter_info.push_back(std::make_pair(0, 0));
      }
    } while (INITV_next(blk) && (blk=INITV_next(blk)));
  }

  // eh spec table
  INITV_IDX eh_spec = INITV_next(ty_tab);
  idx = TCON_uval(INITV_tc_val(eh_spec));
  if (idx) {
    INITO* ino = &Inito_Table[idx];
    ST* st = INITO_st(ino);
    Is_True(!strcmp(ST_name (*st), "__EH_SPEC_TABLE__") &&
            (ST_sclass(st) == SCLASS_EH_REGION_SUPP),
            ("Unexpected ST in PU"));
    INITV_IDX blk = INITO_val(*ino);
    INITV_IDX st_entry = INITV_blk(blk);
    while (st_entry != INITO_IDX_ZERO) {
      ST_IDX st_idx = 0;
      if (INITV_kind (st_entry) != INITVKIND_ZERO) {
        st_idx = TCON_uval(INITV_tc_val (st_entry));
        _eh_spec.push_back(std::make_pair(File_Index, ST_name_idx(ST_ptr(st_idx))));
      }
      st_entry = INITV_next(st_entry);
    }
  }
}

// ----------------------------------------------------------------------------
// EH_TABLE::Type_info(INT filter) const
// ----------------------------------------------------------------------------
FS_PAIR
EH_TABLE::Type_info(INT filter) const
{
  Is_True(filter > 0 && filter < _typeinfo.size(),
          ("filter out of range"));
  return _typeinfo[filter];
}

// ----------------------------------------------------------------------------
// EH_TABLE::Is_eh_type_match(const char* eh_type, INT filter) const
// ----------------------------------------------------------------------------
BOOL
EH_TABLE::Is_eh_type_match(uint32_t file_idx, STR_IDX eh_type, INT filter) const
{
  if (filter != -1) {
    FS_PAIR ti = Type_info(filter);
    return file_idx == ti.first && eh_type == ti.second; // TODO: check class hierarchy
  }
  else {
    EH_TYPE_VECTOR::const_iterator it;
    for (it = _typeinfo.begin(); it != _typeinfo.end(); ++it) {
      if (it->first == 0)
        continue;
      if (file_idx == it->first && eh_type == it->second) // TODO: check class hierarchy
        return FALSE;
    }
    return TRUE;
  }
}

// ----------------------------------------------------------------------------
// EH_TABLE::Get_type_filter(const char* eh_type) const
// ----------------------------------------------------------------------------
INT
EH_TABLE::Get_type_filter(uint32_t file_idx, STR_IDX eh_type) const
{
  INT max = _typeinfo.size();
  for (INT i = 1; i < max; ++i) {
    FS_PAIR ti = _typeinfo[i];
    if (file_idx == ti.first && eh_type == ti.second)
      return i;
  }
  return -1;
}

// ----------------------------------------------------------------------------
// EH_TABLE::Add_eh_type(const char* ti)
//   Add new eh type
// ----------------------------------------------------------------------------
BOOL
EH_TABLE::Add_eh_type(uint32_t file_idx, STR_IDX ti)
{
  if(ti == 0) {
    return FALSE;
  }
  EH_TYPE_VECTOR::const_iterator it;
  for (it = _typeinfo.begin(); it != _typeinfo.end(); ++it) {
    FS_PAIR t = *it;
    if (t.first == 0)
      continue;
    if (file_idx == t.first && ti == t.second)
      return FALSE;
  }
  _typeinfo.push_back(std::make_pair(file_idx, ti));
  return TRUE;
}

// ----------------------------------------------------------------------------
// EH_TABLE::Throw_eh_type(const char* ti) const
//   Check if function throw exception with given type
// ----------------------------------------------------------------------------
BOOL
EH_TABLE::Throw_eh_type(uint32_t file_idx, STR_IDX ti) const
{
  EH_TYPE_VECTOR::const_iterator it;
  for (it = _typeinfo.begin(); it != _typeinfo.end(); ++it) {
    FS_PAIR t = *it;
    if (t.first == 0)
      continue;
    if (file_idx == t.first && ti == t.second)
      return TRUE;
  }
  return FALSE;
}

// ----------------------------------------------------------------------------
// EH_TABLE::Print
//   Print EH table
// ----------------------------------------------------------------------------
void
EH_TABLE::Print(FILE* fp) const
{
  fprintf(fp, "EH TABLE for %s\n",
              ST_name(WN_st(Comp_unit()->Input_tree())));

  fprintf(fp, "============================\n");
  EH_PATH_VECTOR::const_iterator it;
  for (it = _paths.begin(); it != _paths.end(); ++it) {
    (*it)->Print(fp);
    Print_region((*it)->Region_info(), fp);
    fprintf(fp, "----------------------------\n");
  }

  fprintf(fp, " + Type info:");
  EH_TYPE_VECTOR::const_iterator ty;
  for (ty = _typeinfo.begin(); ty != _typeinfo.end(); ++ty) {
    FS_PAIR t = *ty;
    fprintf(fp, " %s", t.second > 0 ? STR_idx_str(t.first, t.second) : "(nil)");
  }

  fprintf(fp, " + EH spec:");
  EH_TYPE_VECTOR::const_iterator es;
  for (es = _eh_spec.begin(); es != _eh_spec.end(); ++es) {
    FS_PAIR t = *es;
    if (t.first == 0)
      continue;
    fprintf(fp, "%s ", t.second > 0 ? STR_idx_str(t.first, t.second) : "(nil)");
  }
  fprintf(fp, "\n============================\n");
}

// ----------------------------------------------------------------------------
// EH_TABLE::Print_region
//   Print BB_REGION
// ----------------------------------------------------------------------------
void
EH_TABLE::Print_region(BB_REGION* region, FILE* fp) const
{
  fprintf(fp, "region=%p parent=%p start=BB%d end=BB%d n_exit=%d esupp=%d\n",
              region, region->Parent(),
              region->Region_start()->Id(), region->Region_end()->Id(),
              region->Region_num_exits(), region->Ereg_supp());

  // print rid
  RID* rid = region->Rid();
  if (rid != NULL)
    fprintf(fp, " - rid=%p id=%d type=0x%x depth=%d flag=0x%x n_exit=%d parent=%p kid=%p next=%p\n",
                rid, rid->id, rid->rid_type, rid->depth, rid->flags, rid->num_exits,
                rid->parent, rid->first_kid, rid->next);

  // print exits
  WN* r_exits = region->Region_exit_list();
  if (r_exits != NULL && WN_first(r_exits) != NULL) {
    fprintf(fp, " + exits:\n");
    for (WN* kid = WN_first(r_exits); kid != NULL; kid = WN_next(kid)) {
      OPERATOR opr = WN_operator(kid);
      fprintf(fp, "   %s L%d\n",
              OPERATOR_name(opr) + 4,
              OPERATOR_has_label(opr) ? WN_label_number(kid) : 0);
              
    }
  }

  // print pragma list
  WN* r_prags = region->Region_pragma_list();
  if (r_prags != NULL && WN_first(r_prags) != NULL) {
    fprintf(fp, " + pragmas:\n");
    for (WN* kid = WN_first(r_prags); kid != NULL; kid = WN_next(kid)) {
      OPERATOR opr = WN_operator(kid);
      fprintf(fp, "   %s L%d\n",
              OPERATOR_name(opr) + 4,
              OPERATOR_has_label(opr) ? WN_label_number(kid) : 0);
    }          
  }

  // print ereg_supp
  INITV_IDX iv_idx = INITO_val(region->Ereg_supp());
  if (iv_idx != 0 && INITV_kind(iv_idx) == INITVKIND_BLOCK) {
    iv_idx = INITV_blk(iv_idx);
    if (INITV_kind(iv_idx) == INITVKIND_LABEL) {
      INT32 label = INITV_lab(iv_idx);
      BB_NODE* bb = Comp_unit()->Cfg()->Get_bb_from_label(label);
      fprintf(fp, " - handler: BB%d (Lab%d)\n",
                  bb->Id(), label);
    }
    INITV_IDX type;
    for (type = INITV_next(iv_idx); type; type = INITV_next(type)) {
      if (INITV_kind (type) != INITVKIND_VAL)
        continue;
      int tcval = TCON_uval(INITV_tc_val(type));
      if (tcval > 0)
        fprintf(fp, " - type: %s\n", ST_name((ST_IDX)tcval));
      else
        fprintf(fp, " - type: (%d)\n", tcval);
    }
  }
}

#include "opt_vsa_report.h"

static const char *can_not_catch_exception[] = {
  "java.lang.NullPointerException",
  "java.lang.RuntimeException",
  "java.lang.Exception",
};

// =============================================================================
//
// VSA::Scan_eh_error entry point for exception handling check
//
// =============================================================================
void
VSA::Scan_eh_error(BB_NODE *bb)
{
  if(!VSA_EH)
    return;
  if(!PU_java_lang(Get_Current_PU())) {
    Builtin_certcpp_err54();
    return;
  }
  Builtin_certj_obj11();
  Builtin_certj_err00();
  Classify_eh_error(bb);
}

// =============================================================================
//
// VSA::Classify_eh_error traverse bb, check bb or stmt level vulnerability
//
// =============================================================================
void
VSA::Classify_eh_error(BB_NODE *bb)
{
  // iterate through each statement in this bb
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {
    Builtin_certj_err08(stmt);
    Builtin_certj_tnh(stmt);
  }

  // do the same for its dominated nodes
  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb->Dom_bbs()))
    Classify_eh_error(dom_bb);
}

// =============================================================================
//
// VSA::Builtin_certj_obj11 check cert-j obj11
//    OBJ11-J. Be wary of letting constructors throw exceptions
//
// =============================================================================
void
VSA::Builtin_certj_obj11()
{
  // CERT OBJ11-J check
  // if this method is constructor, any throw not handle in current function is forbidden
  if (Dna()->Is_set(DNA_IS_CONSTRUCTOR) && EH_table()->Spec_list()->size() > 0) {
    STMTREP *sr = Get_entry_chi_stmt();
    SRCPOS_HANDLE srcpos_h(NULL, sr, Dna(), Loc_pool(), this);
    //srcpos_h.Add_message("Throws %d Exception in constructor.", EH_table()->Spec_list()->size());
    srcpos_h.Set_msgid("OBJ11J.1");
    Rbc()->Report_rbc_error(this, sr, "OBJ11-J", IC_DEFINITELY, &srcpos_h);
  }
}

// =============================================================================
//
// VSA::Check_catch_bb_ignored_exception
//    used by Buildin_certj_err00, check catch block is empty or
//       just print the stack trace
//
// =============================================================================
void
VSA::Check_catch_bb_ignored_exception(BB_NODE *bb, SRCPOS_HANDLE *sp_h, regex_t **reg_exp_arr,
    INT32 arr_len, STMTREP **source_sr, CODEREP **source_cr, STMTREP **sink_sr, CODEREP **sink_cr, BOOL &ignored)
{
  STMTREP *stmt;
  STMTREP *prev_stmt = NULL;
  STMTREP_ITER iter(bb->Stmtlist());
  FOR_ALL_NODE(stmt, iter, Init()) {
    switch (stmt->Opr()) {
    case OPR_LABEL:
    {
      if (*source_sr != NULL) {
        *source_sr = stmt;
      }
      continue;
    }
    break;
    case OPR_STID:
    {
      BOOL found_stname = FALSE;
      CODEREP *rhs = stmt->Rhs();
      if (rhs->Kind() == CK_IVAR && rhs->Ilod_base()->Kind() == CK_VAR) {
        AUX_STAB_ENTRY *aux = Comp_unit()->Opt_stab()->Aux_stab_entry(rhs->Ilod_base()->Aux_id());
        ST *st = aux->St();
        if (strcmp(ST_name(st), "__Exc_Ptr__") == 0) {
          found_stname = TRUE;
          if (stmt->Lhs()->Kind() == CK_VAR) {
            if (*source_sr == NULL || (*source_sr)->Opr() == OPR_LABEL) {
              *source_sr = stmt;
              *source_cr = stmt->Lhs();
            }
          }
        }
      }
      if (!found_stname) {
        // found store stmt that do not store exception pointer, treat the store stmt as handler
        ignored = FALSE;
      }
    }
    break;
    case OPR_ICALL:
    {
      CODEREP *callcr = stmt->Rhs();
      const char *mangled_name = Get_lookup_virt_mangled_function_name(callcr->Opnd(callcr->Kid_count() -1));
      if (!mangled_name) {
        ignored = FALSE;
      } else {
        for (INT32 i = 0; i < arr_len; i++) {
          regex_t *reg_exp = reg_exp_arr[i];
          regmatch_t dummy[1];
          INT reti = (INT) regexec(reg_exp, mangled_name, 1, dummy, 0);
          // not match, found handler
          if (reti == REG_NOMATCH) {
            ignored = FALSE;
            break;
          } else {
            if (*sink_sr == NULL) {
              *sink_sr = stmt;
              *sink_cr = stmt->Rhs()->Opnd(0);
            }
          }
        }
      }
    }
    break;
    case OPR_RETURN_VAL:
    case OPR_RETURN:
    {
      // if the catch block returned, we treat return stmt as handler
      if (prev_stmt != NULL) {
        if (prev_stmt->Opr() == OPR_STID && prev_stmt->Lhs()->Kind() == CK_VAR) {
          AUX_STAB_ENTRY *aux = Comp_unit()->Opt_stab()->Aux_stab_entry(prev_stmt->Lhs()->Aux_id());
          if (aux->Is_preg()) {
            ignored = FALSE;
          }
        }
      }
    }
    break;
    case OPR_GOTO:
    {
      // do nothing
    }
    break;
    default:
    {
      // found handler
      ignored = FALSE;
    }
    break;
    }
    if (!ignored) {
      // handle stmt for exception founded, need not to traverse the doms
      return;
    }
    prev_stmt = stmt;
  }
  if (!ignored) {
    // handle stmt for exception founded, need not to traverse the doms
    return;
  }
  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb->Dom_bbs())) {
    Check_catch_bb_ignored_exception(dom_bb, sp_h, reg_exp_arr, arr_len, source_sr, source_cr, sink_sr, sink_cr, ignored);
    if (!ignored) {
      return;
    }
  }
}

static const char *err00_ignored_method_pattern[] = {
  "15printStackTrace",
};

// =============================================================================
//
// VSA::Builtin_certj_err00 check cert-j err00
//    ERR00-J. Do not suppress or ignore checked exceptions
//
// =============================================================================
void
VSA::Builtin_certj_err00()
{
  DNA_NODE *dna = Dna();
  JAVA_AUX_INFO *java_aux_info = Ipsa()->Glob_cha()->Get_java_aux_info(dna->Fname());
  if (java_aux_info != NULL) {
    JAVA_METH_INFO *java_meth_info = java_aux_info->Get_java_meth_info(dna->Fname());
    if (java_meth_info != NULL) {
      UINT32 flags = java_meth_info->Get_flag();
      if (flags & ACC_SYNTHETIC) {
        Is_Trace(Tracing(), (TFile,
          "VSA::Builtin_certj_err00: ignore ACC_SYNTHETIC method, method name: %s\n", dna->Fname()));
        return;
      }
      const char *meth_name = java_meth_info->Get_name();
      if (strcmp(meth_name, "<clinit>") == 0) {
        Is_Trace(Tracing(), (TFile,
          "VSA::Builtin_certj_err00: ignore <clinit> method, method name: %s\n", dna->Fname()));
        return;
      }
    }
  }
  INT32 arr_len = sizeof(err00_ignored_method_pattern) / sizeof(const char *);
  regex_t **reg_exp_arr = Compile_pattern_arr(err00_ignored_method_pattern, arr_len, FALSE, Loc_pool());
  Is_True_Ret(reg_exp_arr != NULL, ("Wrong regular expression pattern in err00_ignored_method_pattern array."));
  EH_TABLE *eh_table = Comp_unit()->EH_table();
  EH_PATH_VECTOR *path_list = eh_table->Path_list();
  EH_PATH_VECTOR::const_iterator iter;
  hash_set<IDTYPE> visited_bb;
  for (iter = path_list->begin(); iter != path_list->end(); ++iter) {
    EH_PATH *path = *iter;
    FS_BB_VECTOR *fs_bb_vec = path->Fs_bb_vector();
    FS_BB_VECTOR::const_iterator fs_iter;
    for (fs_iter = fs_bb_vec->begin(); fs_iter != fs_bb_vec->end(); ++fs_iter) {
      if (visited_bb.find(fs_iter->second) != visited_bb.end()) {
        continue;
      }
      IDTYPE bb_id = fs_iter->second;
      visited_bb.insert(bb_id);
      BB_NODE *bb_catch = Cfg()->Get_bb(bb_id);
      SRCPOS_HANDLE sp_h(Dna(), Loc_pool());
      BOOL ignored = TRUE;
      STMTREP *source_sr = NULL, *sink_sr = NULL;
      CODEREP *source_cr = NULL, *sink_cr = NULL;
      Check_catch_bb_ignored_exception(bb_catch, &sp_h, reg_exp_arr, arr_len, &source_sr, &source_cr, &sink_sr, &sink_cr, ignored);
      Is_Trace(Tracing(), (TFile,
        "VSA::Builtin_certj_err00: check catch bb: %d, ignored: %d.\n", bb_catch->Id(), ignored));
      // can't find useful handler stmt, report error
      if (ignored) {
        // push sink first
        if (sink_sr != NULL) {
          if (sink_cr != NULL) {
            sp_h.Append_data(sink_sr, sink_cr, Dna(), PATHINFO_VUL_SPOT);
          } else {
            sp_h.Append_data(sink_sr, Dna(), PATHINFO_VUL_SPOT);
          }
        }
        // push source
        STMTREP *rbc_sr = source_sr;
        if (rbc_sr == NULL || rbc_sr->Linenum() == 0) {
          rbc_sr = path->Handler_stmt();
        }
        // can't find valid rbc stmt
        if (rbc_sr == NULL || rbc_sr->Linenum() == 0) {
          continue;
        }
        sp_h.Append_data(source_sr, Dna(), PATHINFO_RBC);
        // push symbol
        if (source_sr != NULL) {
          if (source_cr != NULL) {
            Is_True(source_cr->Kind() == CK_VAR,
              ("Source coderep's kind is not CK_VAR, kind: %d, cr: cr%d.", source_cr->Kind(), source_cr->Coderep_id()));
            sp_h.Set_orig_stname(sp_h.Find_orig_stname(source_cr, source_sr, Dna()));
            AUX_STAB_ENTRY *aux_stab = Comp_unit()->Opt_stab()->Aux_stab_entry(source_cr->Aux_id());
            ST *st = aux_stab->St();
            sp_h.Append_data(st, source_sr->Bb(), Dna(), PATHINFO_ST_DECLARE);
          }
        }
        Rbc()->Report_rbc_error(this, rbc_sr, "ERR00-J", IC_DEFINITELY, &sp_h);
      }
    }
  }
}

// =============================================================================
//
// VSA::Get_stmtrep_for_catch
//    used by Builtin_certcpp_err54, get catch STMTREP from input catched st
//
// =============================================================================
STMTREP *
VSA::Get_stmtrep_for_catch(EH_PATH *path, const char *name)
{
  hash_set<IDTYPE> visited_bb;
  FS_BB_VECTOR *fs_bb_vec = path->Fs_bb_vector();
  FS_BB_VECTOR::const_iterator fs_iter;
  for (fs_iter = fs_bb_vec->begin(); fs_iter != fs_bb_vec->end(); ++fs_iter) {
    if (visited_bb.find(fs_iter->second) != visited_bb.end())
      continue;
    IDTYPE bb_id = fs_iter->second;
    visited_bb.insert(bb_id);
    BB_NODE *bb_catch = Cfg()->Get_bb(bb_id);
    char *st_name = STR_idx_str(fs_iter->first.first, fs_iter->first.second);
    if (!strcmp(st_name, name)) {
      return bb_catch->Last_stmtrep();
    }
  }
  return NULL;
}

// =============================================================================
//
// VSA::Check_parent
//    used by Builtin_certcpp_err54
//    check if ty_name is the type name of one of class node's parent
//
// =============================================================================
BOOL
VSA::Check_parent(CLASS_INFO *info, const char *ty_name)
{
  C_STR_VEC *parents = info->Get_parents();
  C_STR_VEC_ITER iter;
  if (parents) {
    for (iter = parents->begin(); iter != parents->end(); iter++) {
      if (strcmp(*iter, ty_name) == 0)
        return TRUE;
      CLASS_INFO *parent_info = Ipsa()->Glob_cha()->Get_class_info(*iter);
      if (parent_info) {
        if (Check_parent(parent_info, ty_name))
         return TRUE;
      }
    }
  }
  return FALSE;
}

// =============================================================================
//
// VSA::Builtin_certcpp_err54 check cert-cpp err54
//    ERR54-CPP. Catch handlers should order their parameter types
//               from most derived to least derived
//
// =============================================================================
void
VSA::Builtin_certcpp_err54()
{
  hash_set<uintptr_t> handler_set;
  EH_TABLE *eh_table = Comp_unit()->EH_table();
  Is_True(eh_table != NULL, ("EH table is NULL"));

  EH_PATH_VECTOR *path_list = eh_table->Path_list();
  EH_PATH_VECTOR::const_iterator path;
  for (path = path_list->begin(); path != path_list->end(); ++path) {
    STMTREP *cur_handler = (*path)->Handler_stmt();
    if (handler_set.find((uintptr_t)cur_handler) != handler_set.end())
      continue;
    handler_set.insert((uintptr_t)cur_handler);

    BB_REGION *region = (*path)->Region_info();
    INITV_IDX iv_idx = INITO_val(region->Ereg_supp());
    if (iv_idx != 0 && INITV_kind(iv_idx) == INITVKIND_BLOCK) {
      iv_idx = INITV_blk(iv_idx);
      INITV_IDX type;
      CLASS_SET *children = NULL;
      vector<pair<ST *, bool> > rtti_st_list;
      for (type = INITV_next(iv_idx); type; type = INITV_next(type)) {
        if (INITV_kind(type) != INITVKIND_VAL)
          continue;
        int tcval = TCON_uval(INITV_tc_val(type));
        if (tcval > 0) {
          ST *rtti_st = ST_ptr((ST_IDX)tcval);
          // ignore fundamental type info
          if (!strcmp("__fundamental_type_info_pseudo", TY_name(ST_type(rtti_st))))
            continue;
          if (!strcmp("__pointer_type_info_pseudo", TY_name(ST_type(rtti_st))) &&
              !ST_is_rtti(rtti_st))
            continue;
          Is_True(rtti_st && ST_is_rtti(rtti_st), ("st is not rtti"));
          TY_IDX class_ty = ST_vtable_ty_idx(rtti_st);
          Is_True(TY_kind(class_ty) == KIND_STRUCT ||
                  (TY_kind(class_ty) == KIND_POINTER &&
                   TY_kind(TY_pointed(class_ty)) == KIND_STRUCT),
                  ("invalid class ty for rtti"));
          bool class_ty_is_ptr = FALSE; // TRUE if class ty is a pointer type
          if (TY_kind(class_ty) == KIND_POINTER) {
            class_ty_is_ptr = TRUE;
            class_ty = TY_pointed(class_ty);
          }
          CLASS_INFO *clazz_info = Ipsa()->Glob_cha()->Get_class_info(TY_name(class_ty));
          if (clazz_info != NULL) {
            for (int i = 0; i < rtti_st_list.size(); ++i) {
              if (rtti_st_list[i].second != class_ty_is_ptr)
                continue;
              TY_IDX check_ty = ST_vtable_ty_idx(rtti_st_list[i].first);
              if (class_ty_is_ptr)
                check_ty = TY_pointed(check_ty);
              if (Check_parent(clazz_info, TY_name(check_ty))) {
                STMTREP *sr = Get_stmtrep_for_catch(*path, ST_name(rtti_st));
                STMTREP *catch_sr = Get_stmtrep_for_catch(*path, ST_name(rtti_st_list[i].first));
                if (sr == NULL || catch_sr == NULL)
                  continue;
                Is_True(sr != NULL, ("invalid stmtrep for catch"));
                SRCPOS_HANDLE srcpos_h(NULL, sr, Dna(), Loc_pool(), this);
                srcpos_h.Set_msgid("ERR54CPP.1");
                srcpos_h.Append_data(catch_sr, Dna(), PATHINFO_EH_CATCH);
                Rbc()->Report_rbc_error(this, sr, "ERR54-CPP",
                                        IC_DEFINITELY, &srcpos_h);
              }
            }
          }
          rtti_st_list.push_back(std::make_pair(rtti_st, class_ty_is_ptr));
        }
      }
    }
  }
}

// =============================================================================
//
// VSA::Builtin_certj_err08 check cert-j err08
//    ERR08-J. Do not catch NullPointerException or any of its ancestors
//
// =============================================================================
void
VSA::Builtin_certj_err08(STMTREP *stmt)
{
  if (stmt == NULL || stmt->Opr() != OPR_STID) {
    return;
  }
  /*
       LDID __Exc_Ptr__
      ILOAD -8
    STID eh_symbol
    
    the pattern shows how to store exception symbol pointer into catch symbol
  */
  if (stmt->Rhs()->Kind() == CK_IVAR && stmt->Rhs()->Ilod_base() && stmt->Rhs()->Ilod_base()->Kind() == CK_VAR) {
    CODEREP *base = stmt->Rhs()->Ilod_base();
    AUX_STAB_ENTRY *exc_ptr_sym = Comp_unit()->Opt_stab()->Aux_stab_entry(base->Aux_id());
    ST *exc_ptr_st = exc_ptr_sym->St();
    // find catch
    if (strcmp(ST_name(exc_ptr_st), "__Exc_Ptr__") == 0) {
      Is_True(stmt->Lhs()->Kind() == CK_VAR,
        ("Store catch symbol error, the kind of lhs is not CK_VAR, kind : %d", stmt->Lhs()->Kind()));
      CODEREP *cr = stmt->Lhs();
      AUX_STAB_ENTRY *ex_sym = Comp_unit()->Opt_stab()->Aux_stab_entry(cr->Aux_id());
      ST *ex_st = ex_sym->St();
      // maybe the catch symbol was converted to p_reg
      if (TY_kind(ST_type(ex_st)) == KIND_POINTER) {
        char *ty_name = TY_name(TY_pointed(ST_type(ex_st)));
        for (int i = 0; i < sizeof(can_not_catch_exception) / sizeof(const char *); i++) {
          if (strcmp(can_not_catch_exception[i], ty_name) != 0) {
            continue;
          }
          SRCPOS_HANDLE srcpos_h(NULL, stmt, Dna(), Loc_pool(), this);
          srcpos_h.Set_msgid("ERR08J.1");
          const char *orig_name = srcpos_h.Find_orig_stname(cr, stmt, Dna());
          srcpos_h.Set_orig_stname(orig_name);
          Rbc()->Report_rbc_error(this, stmt, "ERR08-J", IC_DEFINITELY, &srcpos_h);
          break;
        }
      } else {
        Is_Trace(Tracing(), (TFile, "VSA::Classify_eh_error: Can't get the original exception type, type name: %s\n", TY_name(ST_type(ex_st))));
      }
    }
  }
}

// =============================================================================
//
// VSA::Builtin_certj_tnh check rule: throw not handling
//    it's hard to mapping this rule to CERT-J
//
// =============================================================================
void
VSA::Builtin_certj_tnh(STMTREP *stmt)
{
  OPERATOR opr = stmt->Opr();
  if(opr == OPR_CALL &&
     OPERATOR_has_sym(opr) &&
     !strcmp(ST_name(stmt->St()), "_Jv_Throw")) {
    TY_IDX throw_ty = 0;
    STR_IDX throw_ty_name_idx = Comp_unit()->Dna()->Get_eh_throw_type(stmt, &throw_ty);
    // Removed assertion: if throwing null, there would be no valid TY for us,
    // which is still Java compliant code, thus no assertion needed.
    // We only handle the case when the thrown object is not null (TY non null)
    if(throw_ty) {
      SRCPOS_HANDLE srcpos_h(stmt->Rhs()->Opnd(0)->Ilod_base(), stmt,
                             Comp_unit()->Dna(), Loc_pool(), this);
      char *throw_ty_name = &Str_Table[throw_ty_name_idx];
      srcpos_h.Set_orig_stname(throw_ty_name);
      hash_set<IDTYPE> visited_funs;
      // don't check throw java.lang.Throwable, java EH will throw this type when there are finally block
      if (strcmp(throw_ty_name, "java.lang.Throwable") != 0) {
        vector<const char*> valid_throw_names;
        INT cur_level = 0;
        valid_throw_names.push_back(throw_ty_name);
        Add_base_throw_types(valid_throw_names, throw_ty);
        Check_throw_catched_or_throws(valid_throw_names, stmt, 
                                      Comp_unit()->Dna(), &srcpos_h, &visited_funs,cur_level);
      }
    }
  }
}

void
VSA::Check_throw_catched_or_throws(vector<const char*>& valid_throw_names,
                                   STMTREP *stmt, DNA_NODE *dna, 
                                   SRCPOS_HANDLE *sp_h, hash_set<IDTYPE>* visited_funs, INT &cur_level)
{
  // limit traverse level, only check one callby level by default
  if(cur_level > VSA_EH_LEVEL) {
    return;
  }
  // skip visited functions
  if(visited_funs->find(dna->Dna_idx()) == visited_funs->end()) {
    visited_funs->insert(dna->Dna_idx());
  } else {
    return;
  }
  Is_True(valid_throw_names.size() >= 1, ("SCAN EH ERROR: invalid throw names"));

  const char* throw_ty_name = valid_throw_names[0];
  BOOL fun_throws = Check_fun_throws(dna, valid_throw_names);
  EH_TABLE *eh_table = dna->Comp_unit()->EH_table();
  Is_True(eh_table != NULL, ("EH table is NULL"));
  EH_PATH *path = eh_table->Find_handler(stmt);
  BOOL catched = FALSE;
  if(path) {
    // TODO: check if handler catched the throwed type
    if(!Check_throw_catched(valid_throw_names, path)) {
      if (dna->Is_set(DNA_IS_CONSTRUCTOR)) {
        //sp_h->Add_message("Exception %s is not caught in constructor.", throw_ty_name);
        sp_h->Set_msgid("OBJ11J.1");
        Rbc()->Report_rbc_error(this, stmt, "OBJ11-J", IC_DEFINITELY, sp_h);
        return;
      }
      if(VSA_Tnh && !fun_throws) {
        //sp_h->Add_message("Exception %s is not caught.", throw_ty_name);
        sp_h->Set_msgid("TNHX.1");
        Rbc()->Report_rbc_error(this, stmt, "TNH", IC_DEFINITELY, sp_h);
      }
    } else {
      catched = TRUE;
    }
  } else {
    if (dna->Is_set(DNA_IS_CONSTRUCTOR)) {
      //sp_h->Add_message("Exception %s is not caught in constructor.", throw_ty_name);
      sp_h->Set_msgid("OBJ11J.1");
      Rbc()->Report_rbc_error(this, stmt, "OBJ11-J", IC_DEFINITELY, sp_h);
      return;
    }
    if(VSA_Tnh && !fun_throws) {
      //sp_h->Add_message("Exception %s is not in function throws list.", throw_ty_name);
      sp_h->Set_msgid("TNHX.1");
      Rbc()->Report_rbc_error(this, stmt, "TNH", IC_DEFINITELY, sp_h);
    }
  }

  if(!catched) {
    // check caller catched the Exception or throw out
    cur_level++;
    for (INT i = VAR_INIT_ID; i < dna->Clby_list()->size(); i++)
    {
      RNA_NODE *call_rna = (*dna->Clby_list())[i];
      DNA_NODE *caller = Ipsa()->Get_dna(call_rna->Caller_idx());
      if (caller->Non_functional()) {
        Is_Trace(Tracing(), (TFile, "VSA:SCAN_EH_ERROR: skip caller %s --> callee %s. caller is from rbc model\n",
                                    caller->Fname(), dna->Fname()));
        continue;
      }
      STMTREP *call_stmt = call_rna->Callstmt();
      CONTEXT_SWITCH context(caller);
      SRCPOS_HANDLE *srcpos_h = sp_h->Clone();
      srcpos_h->Append_data(call_stmt, caller,PATHINFO_DNA_CALLSITE);
      caller->Comp_unit()->Vsa()->Check_throw_catched_or_throws(valid_throw_names,
        call_stmt, caller, srcpos_h, visited_funs, cur_level);
    }
  }
}

BOOL
VSA::Check_throw_catched(vector<const char *>& valid_throw_names, EH_PATH *path)
{
  BB_REGION *region = path->Region_info();
  INITV_IDX iv_idx = INITO_val(region->Ereg_supp());
  if (iv_idx != 0 && INITV_kind(iv_idx) == INITVKIND_BLOCK) {
    iv_idx = INITV_blk(iv_idx);
    INITV_IDX type;
    for (type = INITV_next(iv_idx); type; type = INITV_next(type)) {
      if (INITV_kind (type) != INITVKIND_VAL)
        continue;
      int tcval = TCON_uval(INITV_tc_val(type));
      if (tcval > 0) {
        ST_IDX catch_st = (ST_IDX)tcval;
        TY_IDX ty = ST_type(catch_st);
        Is_True(TY_kind(ty) == KIND_POINTER, ("catch st type is not pointer"));
        if (TY_kind(ty) != KIND_POINTER)
          return FALSE;
        char* catch_ty_name = TY_name(TY_pointed(ty));
        for(int idx = 0; idx < valid_throw_names.size(); idx++) {
          if(strcmp(valid_throw_names[idx], catch_ty_name) == 0) {
            return TRUE;
          }
        }
      }
    }
  }
  return FALSE;
}

BOOL
VSA::Check_fun_throws(DNA_NODE *dna, vector<const char *>& throw_list)
{
  Is_True_Ret(dna != NULL, ("dna is null"), FALSE);
  EH_TABLE *eh_table = dna->Comp_unit()->EH_table();
  Is_True_Ret(eh_table != NULL, ("EH table is NULL"), FALSE);
  EH_TYPE_VECTOR *spec_list = eh_table->Spec_list();
  for (int i = 0; i < spec_list->size(); i++) {
    FS_PAIR spec_t = (*spec_list)[i];
    if (spec_t.first == 0)
      continue;
    char *st_name = STR_idx_str(spec_t.first, spec_t.second);
    for (int j = 0; j < throw_list.size(); j++) {
      if (strcmp(throw_list[j], st_name) == 0) {
        return TRUE;
      }
    }
  }
  return FALSE;
}

void
VSA::Add_base_throw_types(vector<const char*>& valid_throw_names, TY_IDX throw_ty)
{
  const char *throw_ty_name = TY_name(throw_ty);
  // search from type table
  TY_IDX curr_throw_ty = throw_ty;
  while(Is_Structure_Type(curr_throw_ty)) {
    UINT curr_id = 0;
    UINT base_fld_id = 2;  // java base fld
    FLD_HANDLE fld_handle = FLD_get_to_field(curr_throw_ty, base_fld_id, curr_id);
    if(!fld_handle.Is_Null()) {
      curr_throw_ty = FLD_type(fld_handle);
      const char *base_name = TY_name(curr_throw_ty);
      if(strcmp(base_name, "java.lang.Object")) {
        valid_throw_names.push_back(base_name);
        Is_Trace(Tracing(), (TFile, "VSA:SCAN_EH_ERROR: %s add base throw name %s\n",
                             throw_ty_name, base_name));
      } else {
        break;
      }
    } else {
      break;
    }
  }
}
