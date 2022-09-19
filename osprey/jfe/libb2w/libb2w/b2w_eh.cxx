/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

#include "b2w_common.h"
#include "b2w_handler.h"
#include <map>
using namespace std;

enum {
  TRY_REGION,
  CATCH_REGION,
  UNWIND_REGION
};

class B2W_EH_Info {
  public:
  LABEL_IDX cmp_idx;
  LABEL_IDX handler_begin_idx;
  LABEL_IDX* real_handler_idx;
  LABEL_IDX unwind_begin_idx;
  WN *  unwind_handler;
  INT   try_idx;

  B2W_EH_Info() {
    cmp_idx = 0;
    handler_begin_idx = 0;
    real_handler_idx = 0;
    unwind_begin_idx = 0;
    unwind_handler = 0;
  }

  B2W_EH_Info(B2W_EH_Info info, INT cursor) {
    cmp_idx = info.cmp_idx;
    handler_begin_idx = info.handler_begin_idx;
    real_handler_idx = info.real_handler_idx;
    unwind_begin_idx = info.unwind_begin_idx;
    unwind_handler = info.unwind_handler;
    try_idx = cursor;
  }

  void init_for_try(UINT32 real_handler_cnt, UINT32 try_idx) {
    // generate the label for future use
    New_LABEL(CURRENT_SYMTAB, cmp_idx);
    New_LABEL(CURRENT_SYMTAB, handler_begin_idx);
    Set_LABEL_KIND (Label_Table[handler_begin_idx], LKIND_BEGIN_HANDLER);
    if(real_handler_idx) {
      delete real_handler_idx;
    }
    real_handler_idx = new LABEL_IDX [real_handler_cnt];
    memset(real_handler_idx,0, real_handler_cnt);
  }

  void init() {
    unwind_begin_idx = 0;
    unwind_handler = 0;
    cmp_idx = 0;
    handler_begin_idx = 0;
    if(real_handler_idx) {
      delete real_handler_idx;
    }
    real_handler_idx = 0;
  }
};

class TYPE_FILTER_ENTRY {
  public:
  ST_IDX  st;	   // typeinfo
  int     filter;	 // action record filter
  friend bool operator== (const TYPE_FILTER_ENTRY&, const TYPE_FILTER_ENTRY&);
};

inline bool operator==(const TYPE_FILTER_ENTRY& x, const TYPE_FILTER_ENTRY& y) {
	return x.st == y.st;
}


//==============================================================================
// Globals to store EH related info
//==============================================================================
// the list to store exception type symbol/filter
static vector<TYPE_FILTER_ENTRY>	type_filter_vector;

// store label realted info
static B2W_EH_Info eh_info;

// mapping handler to label
static map<WN*, B2W_EH_Info> handler_eh_info_map;

// store the WNs that need future fixup
// handler may need parent handler to catch more exceptions, the label is not
// known yet, make a fake label first, and fix up later when parent EH is generated
static map<WN*, WN*> wn_label_need_fixup_map;

// Try IDX is only internally used for java fe. (Auto-Incremental)
static map<INT, B2W_EH_Info *> try_idx_eh_info_map;

// unwind function symbol;
static ST * unwind_fun_st;

class Target_Misc_Info {
public:
    LABEL_IDX label;
    INT created;
    WN *created_label;
    Target_Misc_Info(){
      label = 0;
      created = 0;
      created_label = NULLPTR;
    }
    Target_Misc_Info(LABEL_IDX lab) {
      label = lab;
      created_label = NULLPTR;
      created = 0;
    }
};

static map<WN*, Target_Misc_Info *> target_label_idx_map;

// is EH entry been setup
static bool entry_setup = FALSE;

//==============================================================================
// insert wn before pos
// @param  WN*     insert wn
// @param  WN*     position of the insert place
//==============================================================================
void EH_insert_block_before(WN *insert, WN *pos) {
  WN *parentBlk = B2W_CONTEXT::Get_wn_parent(pos, TRUE);
  B2W_insert_block_before(parentBlk, insert, pos);
}

//==============================================================================
// insert wn after pos
// @param  WN*     insert wn
// @param  WN*     insert position
//==============================================================================
void EH_insert_block_after(WN *insert, WN *pos) {
  WN *parentBlk = B2W_CONTEXT::Get_wn_parent(pos, TRUE);
  B2W_insert_block_after(parentBlk, insert, pos);
}

//==============================================================================
// Build eh region
// @param  WN*     region body
// @param  WN*     region type: try, unwind
//==============================================================================
WN * B2W_build_eh_region(WN *region_body, int region_type) {
  INITV_IDX blk = New_INITV();

  if(region_type == TRY_REGION) {
    INITV_Init_Label(blk, eh_info.cmp_idx, 1);
  } else if(region_type == UNWIND_REGION) {
    INITV_IDX iv = New_INITV();
    INITV_Set_ZERO(Initv_Table[iv], MTYPE_U4, 1);
    INITV_IDX initv_label = New_INITV();
	  INITV_Set_ZERO (Initv_Table[initv_label], MTYPE_U4, 1);
    INITV_Init_Block(blk, initv_label);
    Set_INITV_next(initv_label, iv);
  }

  TY_IDX ty = MTYPE_TO_TY_array[MTYPE_U4];
  ST *ereg = Gen_Temp_Named_Symbol(ty, "dummy1", CLASS_VAR, SCLASS_EH_REGION_SUPP);
  Set_ST_is_initialized(*ereg);
  Set_ST_is_not_used(*ereg);
  INITO_IDX ereg_supp = New_INITO(ST_st_idx(ereg), blk);

  WN *region_pragmas = WN_CreateBlock();

  if(region_type == TRY_REGION)
  {
    B2W_insert_block_last(region_pragmas, WN_CreateGoto(eh_info.handler_begin_idx));
  }
  
  WN *region = B2W_Create_Region(REGION_KIND_EH, region_body, region_pragmas,
                                 WN_CreateBlock(), ereg_supp);
  Set_PU_has_region(Get_Current_PU());
  Set_PU_has_exc_scopes(Get_Current_PU());
  return region;
}

//==============================================================================
// Build eh for try with no goto in the range (be don't allow goto to outer region)
// @param  WN*     try begin wn
// @param  WN*     try end wn
//==============================================================================
void B2W_build_try_no_goto(WN *begin_wn, WN *end_wn, WN *new_block) {

  WN *real_begin_wn = begin_wn;
  WN *real_end_wn = end_wn;
  WN *begin_parent = NULL;
  WN *end_parent = NULL;

  // find common parent
  begin_parent = B2W_CONTEXT::Get_wn_parent(begin_wn, TRUE);
  while(begin_parent) {
    real_end_wn = end_wn;
    end_parent = B2W_CONTEXT::Get_wn_parent(end_wn, TRUE);
    while(begin_parent != end_parent && end_parent != NULL) {
      real_end_wn = end_parent;
      end_parent = B2W_CONTEXT::Get_wn_parent(end_parent, FALSE);
    }
    if(begin_parent != end_parent) {
      real_begin_wn = begin_parent;
      begin_parent = B2W_CONTEXT::Get_wn_parent(begin_parent, FALSE);
    } else {
      break;
    }
  }
  
  Is_Valid(real_end_wn, ("try end wn must be valid"));
  Is_Valid(begin_parent == end_parent, ("begin and end parent should be same"));

  WN *begin_prev = WN_prev(real_begin_wn);
  WN *end_next = WN_next(real_end_wn);
  WN *parentBlk = begin_parent;

  if(Tracing() && B2W_LOG(B2W_LVL_EH)) {

    Is_Trace_Ex(TRUE, (TFile, "[Try begin wn: 0x%llx:]\n", (UINT64)begin_wn));
    Is_Trace_Ex(TRUE, (TFile, "------------------------------------------------\n"));
    //fdump_tree(TFile, begin_wn);

    Is_Trace_Ex(TRUE, (TFile, "------------------------------------------------\n"));
    Is_Trace_Ex(TRUE, (TFile, "[Try end wn: 0x%llx:]\n", (UINT64)end_wn));

    Is_Trace_Ex(TRUE, (TFile, "------------------------------------------------\n"));
    //fdump_tree(TFile, end_wn);
    Is_Trace_Ex(TRUE, (TFile, "------------------------------------------------\n"));

    Is_Trace_Ex(TRUE, (TFile, "[WN Tree before try catch:]\n"));

    Is_Trace_Ex(TRUE, (TFile, "------------------------------------------------\n"));
    //fdump_tree(TFile, parentBlk);
    Is_Trace_Ex(TRUE, (TFile, "------------------------------------------------\n"));

  }

  // move the WN from [begin_wn, end_wn) to try block
  WN_first(new_block) = real_begin_wn;
  WN_last(new_block) = real_end_wn;
  WN_prev(real_begin_wn) = NULL;
  WN_next(real_end_wn) = NULL;
  WN_Set_Linenum(new_block, WN_Get_Linenum(real_begin_wn));

  // set up the parent map 
  WN *try_element = real_begin_wn;
  while(try_element != end_next && try_element != NULL) {
    B2W_CONTEXT::Set_wn_parent(try_element, new_block);
    try_element = WN_next(try_element);
  }

  Is_Valid(B2W_CONTEXT::Get_wn_parent(real_end_wn, FALSE) == new_block, ("end_prev parent should be try block"));

  // generate region for try block
  WN *region = B2W_build_eh_region(new_block, TRY_REGION);

  // insert the region to parent block
  if(begin_prev == NULL) {
    WN_first(parentBlk) = region;
  } else {
    WN_next(begin_prev) = region;
  }
  WN_prev(region) = begin_prev;
  WN_next(region) = end_next;
  if(end_next == NULL) {
    WN_last(parentBlk) = region;
  } else {
    WN_prev(end_next) = region;
  }

  B2W_CONTEXT::Set_wn_parent(region, parentBlk);

  if(Tracing() && B2W_LOG(B2W_LVL_EH)) {

    Is_Trace_Ex(TRUE, (TFile, "[WN Tree after build try :]\n"));
    Is_Trace_Ex(TRUE, (TFile, "------------------------------------------------\n"));
    //fdump_tree(TFile, parentBlk);
    Is_Trace_Ex(TRUE, (TFile, "------------------------------------------------\n"));
  }
}

//==============================================================================
// Build eh region
// @param  WN*     region body
// @param  WN*     region type: try, unwind
//==============================================================================
WN * B2W_build_eh_region(WN *body, WN *pragmas, WN *exits, INITO_IDX region_supplement) {
  WN *region = B2W_Create_Region(REGION_KIND_EH, body, pragmas,
                                 exits, region_supplement);
  Set_PU_has_region(Get_Current_PU());
  Set_PU_has_exc_scopes(Get_Current_PU());
  if(Tracing() && B2W_LOG(B2W_LVL_EH)) {
    Is_Trace_Ex(TRUE, (TFile, "[B2W_build_eh_region] WN Tree region:]\n"));
    Is_Trace_Ex(TRUE, (TFile, "------------------------------------------------\n"));
    //fdump_tree(TFile, region);
    Is_Trace_Ex(TRUE, (TFile, "------------------------------------------------\n"));
  }
  return region;
}


//==============================================================================
// traverse tree to find the case goto stmt, build try region without goto
// @param  WN*     try begin wn
// @param  WN*     try end wn
// @param  WN*     current traverse node
// @return bool    reach end wn?
//==============================================================================
bool B2W_split_casegoto(WN **begin_wn, WN *end_wn, WN *curr_wn) {
  Is_Valid(curr_wn, ("current wn is NULL"));
  
  if(curr_wn == end_wn) {
    return TRUE;
  } else if(*begin_wn == end_wn) {
    return TRUE;
  }

  if(WN_opcode(curr_wn) == OPC_SWITCH) {
    if(Tracing() && B2W_LOG(B2W_LVL_EH)) {
      Is_Trace_Ex(TRUE, (TFile, "\n//found SWITCH in a try region :\n"));
      Is_Trace_Ex(TRUE, (TFile, "------------------------------------------------\n"));
      //fdump_tree(TFile, curr_wn);
      Is_Trace_Ex(TRUE, (TFile, "------------------------------------------------\n"));
    }
    WN *next_wn = WN_next(curr_wn);

    Is_Valid(next_wn, ("next wn is NULL"));
    if(*begin_wn == curr_wn) {
      *begin_wn = next_wn;
    } else {
      WN *prev_wn = WN_prev(curr_wn);

      Is_Valid(prev_wn, ("prev wn is NULL"));
      Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_EH),(TFile, "//skip SWITCH, gen region from begin_wn to prev_wn\n"));

      B2W_build_try_no_goto(*begin_wn, prev_wn, WN_CreateBlock());
      *begin_wn = next_wn;
    }
  } else if (WN_opcode(curr_wn) == OPC_BLOCK) {
    WN *kid = WN_first (curr_wn);
    while (kid) {
      if(B2W_split_casegoto(begin_wn, end_wn, kid)) {
        return TRUE;
      } else {
        kid = WN_next(kid);
      }
    }
  } else {
    for (int i=0; i < WN_kid_count(curr_wn); i++) {
      WN *kid = WN_kid(curr_wn, i);
      B2W_CONTEXT::Set_wn_parent(kid, curr_wn);
      if(B2W_split_casegoto(begin_wn, end_wn, kid)) {
        return TRUE;
      }
    }
  }
  return FALSE;
}


//==============================================================================
// Build Try Region
// @param  WN*     try begin wn
// @param  WN*     try end wn
//==============================================================================
void B2W_build_try(WN *begin_wn, WN *end_wn) {

  if(Tracing() && B2W_LOG(B2W_LVL_EH)) {
    Is_Trace_Ex(TRUE, (TFile, "================================================\n"));
    Is_Trace_Ex(TRUE, (TFile, "           START Build TRY\n"));
    Is_Trace_Ex(TRUE, (TFile, "[Input try begin wn : 0x%llx]\n", (UINT64)begin_wn));

    Is_Trace_Ex(TRUE, (TFile, "------------------------------------------------\n"));
    //fdump_tree(TFile, begin_wn);

    Is_Trace_Ex(TRUE, (TFile, "------------------------------------------------\n"));
    Is_Trace_Ex(TRUE, (TFile, "[Input try end wn : 0x%llx]\n", (UINT64)end_wn));

    Is_Trace_Ex(TRUE, (TFile, "------------------------------------------------\n"));
    //fdump_tree(TFile, end_wn);
    Is_Trace_Ex(TRUE, (TFile, "------------------------------------------------\n"));
  }

  // try range can be composed by 1-N wn,  soot provide the range in two ways
  // 1) [begin_wn, end_wn), end_wn was not included
  // 2) [begin_wn, end_wn], end_wn is the same as begin_wn
  // adjust the 1) to [begin_wn, end_wn_prev] to make the range enclosed
  // the end previous may be the first node of a block/region,
  // find the parent block's previous as the end_wn
  if(begin_wn != end_wn) {
    while(!WN_prev(end_wn)) {
      end_wn = B2W_CONTEXT::Get_wn_parent(end_wn, TRUE);
    }
    end_wn = WN_prev(end_wn);
    // for IR like
    // BLOCK  // end_wn
    //   ..
    //   begin_wn
    // END_BLOCK
    // end_wn is parent block of begin_wn
    // set end_wn to begin_wn
    WN *begin_parent = B2W_CONTEXT::Get_wn_parent(begin_wn, FALSE);
    while(begin_parent) {
      if(end_wn == begin_parent) {
        end_wn = begin_wn;
      }
      begin_parent = B2W_CONTEXT::Get_wn_parent(begin_parent, FALSE);
    }
  }
  Is_Valid(end_wn, ("try end wn must be valid"));

  // BE does not allow goto label jmp out of region, but Java EH with finally
  // will break orignal region switch goto label target into several different regions
  // to fix it, we should move the casegoto outof region, for easy implementation
  // now we move the whole swith out of region (case: cfr/FinallyTest1.java)
  WN *curr_wn = begin_wn;
  WN *new_begin = begin_wn;
  bool reach_end = FALSE;
  while(curr_wn != NULL) {
    reach_end = B2W_split_casegoto(&new_begin, end_wn, curr_wn);
    if(reach_end) {
      break;
    }
    while(!WN_next(curr_wn)) {
      curr_wn = B2W_CONTEXT::Get_wn_parent(curr_wn, TRUE);
    }
    curr_wn = WN_next(curr_wn);
  }
  Is_Valid(reach_end, ("should reach end_wn"));
  Is_Valid(curr_wn != NULL, ("curr wn should not be NULL"));

  B2W_build_try_no_goto(new_begin, end_wn, WN_CreateBlock());
  
  if(Tracing() && B2W_LOG(B2W_LVL_EH)) {
    Is_Trace_Ex(TRUE, (TFile, "            END Build TRY\n"));
    Is_Trace_Ex(TRUE, (TFile, "================================================\n"));
  }
}

//==============================================================================
// Build filter compare for try block
// @param  WN*         the insert position
// @param  ST_IDX      handler symbol idx
// @param  handle_idx  the idx of current handler
// @param  is_throw    is throw to upper stack
//==============================================================================
WN * B2W_build_filter_cmp(WN *cmp_insert_pos, ST_IDX handler_st,
                          int handle_idx, bool is_throw, WN *target_wn) {
  Is_Valid(cmp_insert_pos, ("filter compare should be append to a valid WN"));

  // generate labels before compare exception
  LABEL_IDX start_handler = eh_info.handler_begin_idx;
  Is_Valid(start_handler, ("start handler label should be created first"));
  Is_True (LABEL_kind (Label_Table[start_handler]) ==
           LKIND_BEGIN_HANDLER, ("Wrong label kind, expecting handler_begin"));

  if(handle_idx == 0) {

    WN *first_catch_begin = cmp_insert_pos;
    if(target_label_idx_map.find(target_wn) != target_label_idx_map.end()) {
      Is_Valid(target_label_idx_map[target_wn]->created_label != 0, ("[B2W_build_filter_label] label must be already set. \n"));
      first_catch_begin = target_label_idx_map[target_wn]->created_label;
    }

    WN * cmp_wn = WN_CreateLabel ((ST_IDX) 0, start_handler, 0, NULL);
    WN_Set_Label_Is_Handler_Begin (cmp_wn);
    EH_insert_block_before(cmp_wn, first_catch_begin);
    WN_linenum(cmp_wn) = WN_linenum(first_catch_begin);

    Is_Valid(eh_info.cmp_idx, ("eh_info cmp_idx should be created first"));
    WN * actual_cmp = WN_CreateLabel ((ST_IDX) 0, eh_info.cmp_idx, 0, NULL);
    EH_insert_block_before(actual_cmp, first_catch_begin);
    WN_linenum(actual_cmp) = WN_linenum(first_catch_begin);
    cmp_insert_pos = actual_cmp;
  }

  LABEL_IDX target_label_idx = 0;
  if(target_label_idx_map.find(target_wn) != target_label_idx_map.end()){
    target_label_idx = target_label_idx_map[target_wn]->label;
    eh_info.real_handler_idx[handle_idx] = target_label_idx;
  } else {
    New_LABEL(CURRENT_SYMTAB, target_label_idx);
    eh_info.real_handler_idx[handle_idx] = target_label_idx;
    target_label_idx_map[target_wn] = new Target_Misc_Info(target_label_idx);
  }

  // generate compare
  // if the exception is to throw upper stack, no need compare exception type
  if(!is_throw) {
    TYPE_FILTER_ENTRY entry;
    entry.st = handler_st;
    entry.filter = 0;
    vector<TYPE_FILTER_ENTRY>::iterator f = find(type_filter_vector.begin(), type_filter_vector.end(), entry);
    if (f == type_filter_vector.end()) {
      entry.filter = (INT) type_filter_vector.size() + 1;
      type_filter_vector.push_back(entry);
    }
    else {
      entry.filter = f->filter;
    }

    ST_IDX filter_param = TCON_uval (INITV_tc_val (INITV_next (INITO_val (PU_misc_info(Get_Current_PU())))));
    const TYPE_ID mtype = TARGET_64BIT ? MTYPE_U8 : MTYPE_U4;

    WN * wn_ldid = WN_Ldid (mtype, 0, &St_Table[filter_param], MTYPE_TO_TY_array[mtype]);
    WN * goto_wn = WN_CreateGoto (target_label_idx);
    WN_next (goto_wn) = WN_prev (goto_wn) = NULL;
    WN_linenum(goto_wn) = WN_linenum(cmp_insert_pos);

    WN * if_then = WN_CreateBlock ();
    WN_first (if_then) = WN_last (if_then) = goto_wn;

    WN * if_else = WN_CreateBlock ();
    WN * cmp_value = WN_Intconst (mtype, entry.filter); // filter
    WN * cond = WN_Create (OPR_EQ, WN_rtype (wn_ldid), mtype, 2);
    WN_kid0 (cond) = wn_ldid;
    WN_kid1 (cond) = cmp_value;

    WN * if_blk = WN_CreateIf (cond, if_then, if_else);
    EH_insert_block_after(if_blk, cmp_insert_pos);
    WN_linenum(if_blk) = WN_linenum(cmp_insert_pos);
    cmp_insert_pos = if_blk;
  }
  return cmp_insert_pos;
}

//==============================================================================
// Build unwind resume
// @param  WN*     the first throw handler WN
//==============================================================================
void B2W_build_unwind(WN *catch_begin) {
  // generate unwind begin label
  LABEL_IDX unwind_begin_idx = 0;
  New_LABEL(CURRENT_SYMTAB, unwind_begin_idx);
  eh_info.unwind_begin_idx = unwind_begin_idx;
  WN *unwind_begin = WN_CreateLabel((ST_IDX)0, unwind_begin_idx, 0, NULL);
  WN_linenum(unwind_begin) = WN_linenum(catch_begin);
  EH_insert_block_after(unwind_begin, catch_begin);

  // generate unwind resume
  ST_IDX exc_ptr_param = TCON_uval (INITV_tc_val (INITO_val (PU_misc_info(Get_Current_PU()))));
  ST exc_st = St_Table[exc_ptr_param];
  WN* parm_node = WN_Ldid (Pointer_Mtype, 0, &exc_st, ST_type (exc_st));

  TY_IDX idx;
  TY &ptr_ty = New_TY (idx);
  TY_Init (ptr_ty, Pointer_Size, KIND_POINTER, Pointer_Mtype, Save_Str ("anon_ptr."));
  ptr_ty.Set_pointed (ST_type(exc_st));

  WN * arg0 = WN_CreateParm (Pointer_Mtype, parm_node, idx, WN_PARM_BY_VALUE);
  if(unwind_fun_st == 0) {
    unwind_fun_st = New_ST (GLOBAL_SYMTAB);
    PU_IDX pu_idx;
    PU & pu = New_PU (pu_idx);

    TY_IDX puTypeIdx = 0;
    TY &ty = New_TY (puTypeIdx);
    TY_Init (ty, 0, KIND_FUNCTION, MTYPE_UNKNOWN, 0);
    TYLIST_IDX tylist_idx;
    Set_TYLIST_type (New_TYLIST (tylist_idx), Void_Type);
    Set_TY_tylist (ty, tylist_idx);
    Set_TYLIST_type (New_TYLIST (tylist_idx), 0);
    PU_Init (pu, puTypeIdx, GLOBAL_SYMTAB + 1);
    ST_Init (unwind_fun_st, Save_Str("_Unwind_Resume"),
            CLASS_FUNC, SCLASS_EXTERN, EXPORT_PREEMPTIBLE, TY_IDX(pu_idx));
  }
  WN * call_wn = WN_Create (OPR_CALL, Pointer_Mtype, MTYPE_V, 1);
  WN_kid0 (call_wn) = arg0;
  WN_st_idx (call_wn) = ST_st_idx (unwind_fun_st);
  WN_Set_Call_Never_Return (call_wn);
  WN_linenum(call_wn) = WN_linenum(catch_begin);
#if 0
  // do not generate region for unwind
  WN *regin_body = WN_CreateBlock();
  B2W_insert_block_last(regin_body, call_wn);
  WN *region = B2W_build_eh_region(regin_body, UNWIND_REGION);
  EH_insert_block_after(region, unwind_begin);
#endif
  EH_insert_block_after(call_wn, unwind_begin);

}

//==============================================================================
// Build catch
// @param  WN*      start of catch wn
// @param  int      the current handler idx
// @param  bool     is throw to upper stack
//==============================================================================
void B2W_build_catch(WN* handler, int catchIdx, bool is_throw) {
  // if the catch is throw to upper stack, no need to generate compare to jmp to real catch
  // the label generate in B2W_build_catch is no needed
  //if(!is_throw) {
  // create catch start label
  Is_Valid(eh_info.real_handler_idx[catchIdx], ("should have a real handler label"));
  Is_Valid(target_label_idx_map.find(handler) != target_label_idx_map.end(), ("label should be inside target_label_idx_map"));
  if(!target_label_idx_map[handler]->created) {
    WN *catch_start = WN_CreateLabel((ST_IDX) 0, eh_info.real_handler_idx[catchIdx], 0, NULL);
    target_label_idx_map[handler]->created = 1;
    target_label_idx_map[handler]->created_label = catch_start;
    WN_linenum(catch_start) = WN_linenum(handler);
    EH_insert_block_before(catch_start, handler);
  }
  //}
}


//==============================================================================
// Build handlers
// @param  WN*       parent handler
// @param  WN**      all handlers list
// @param  ST_IDX*   all handler st idx
// @param  len       number of handlers
//==============================================================================
void B2W_build_handlers(WN* parent_handler, WN **handlers, ST_IDX * handler_sts, int len) {
  WN *cmp_insert_pos = handlers[0];
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_EH), (TFile, "[B2W_build_handlers]. cmp_insert_pos "
                                                        "= %#0llx \n", (UINT64) cmp_insert_pos));
  int last_throw_idx = -1;
#if 0
  // first loop, find the last throw, generate unwind region
  for (int handler_idx1 = len - 1; handler_idx1 >= 0; handler_idx1--) {
    B2W_get_stptr(handler_sts[handler_idx1]);
    TY_IDX handler_ty = ST_type(handler_sts[handler_idx1]);
    Is_Valid(TY_kind(handler_ty) == KIND_POINTER, ("handler type should be a pointer"));
    char * tyname = TY_name(TY_pointed(handler_ty));
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_EH), (TFile, "[B2W_build_handlers] handling ST:%s = %#0llx \n",
                                                          (CHPTR) ST_name(handler_sts[handler_idx1]),
                                                          (UINT64) handler_idx1));
    // for same range try with several catch, the catch may conatins two throwable
    // exception, for ex:
    //   [begin1, end1] -> Throwable (user specified throwable)
    //   [begin1, end1] -> any (default unwind)
    // the second entry's exception type also coverted to Throwable by Soot, thus
    // we cannot distinguish it's user specified throw or default throw
    // below code assumes the last entry with Throawable type is the unwind
    // if this is wrong, then we need to modify soot code
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_EH), (TFile, " %s %d : comparing %s with %s \n", __FILE__, __LINE__,
      tyname, "java.lang.Throwable"));
     if(strcmp(tyname, "java.lang.Throwable") == 0) {
      // Is_Valid(last_throw_idx == -1, ("should only have one thow for each try"));
      last_throw_idx = handler_idx1;
      WN *throw_handler = handlers[last_throw_idx];
      // we only need one unwind region for each try, create it for first catch,
      // all other catch create a goto WN to jump to the last unwind region
      if(eh_info.unwind_begin_idx == 0 || eh_info.unwind_handler != throw_handler) {
        // B2W_build_unwind(throw_handler);
        eh_info.unwind_handler = throw_handler;
        Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_EH), (TFile, "[B2W_build_handlers]. setting  "
                                                              "%#0llx as unwind \n", (UINT64) throw_handler));
      }
      break;
     }
  }
#endif

  for (int handler_idx2 = 0; handler_idx2 < len; handler_idx2 ++) {
    WN *handler = handlers[handler_idx2];
    TY_IDX handler_ty = ST_type(handler_sts[handler_idx2]);
    char * tyname = TY_name(TY_pointed(handler_ty));
    bool is_throw = (strcmp(tyname, "java.lang.Throwable") == 0);
    if (is_throw && last_throw_idx == -1) {
      last_throw_idx = handler_idx2;
    }
    cmp_insert_pos = B2W_build_filter_cmp(cmp_insert_pos, handler_sts[handler_idx2],
                                        handler_idx2, is_throw, handler);
    B2W_build_catch(handler, handler_idx2, is_throw);

    // set the mapping for handler start label
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_EH), (TFile, "[B2W_build_handlers]. setting handler "
                                                          "%#0llx has label (%#0x) \n", (UINT64) handler, eh_info.cmp_idx));
    handler_eh_info_map[handler] = eh_info;
  }
  // if try have parent handler, should goto parent filter label, and don't generate unwind
  // if exception is java.lang.Throwable, this catch is finally, need to exec finally body, don't generate unwind, finally body have unwind stmt
  if (last_throw_idx == -1 && !parent_handler) {
    B2W_build_unwind(cmp_insert_pos);
    eh_info.unwind_handler = cmp_insert_pos;
  }
  if(last_throw_idx != -1){
    // Finally exists
    WN *goto_unwind = WN_CreateGoto(eh_info.real_handler_idx[last_throw_idx]);
    EH_insert_block_after(goto_unwind, cmp_insert_pos);
  } else {
    // Finally does not exist
    LABEL_IDX parent_label = 0;
    bool need_fixup = false;
    // if have parent handler, goto parent filter label, don't goto parent handler label
    // otherwise will duplicate generate handler related assembly, cause null pointer exception
    if (parent_handler) {
      map<WN *, B2W_EH_Info>::iterator iter = handler_eh_info_map.find(parent_handler);
      if (iter == handler_eh_info_map.end()) {
        need_fixup = true;
        parent_label = 999; // assign a temp value
      } else {
        parent_label = (iter->second).cmp_idx;
      }
    }
    parent_label = parent_label ? parent_label : eh_info.unwind_begin_idx;
    if(parent_label) {
      // create goto parent label for last filter compare
      // for any not handled exception, goto parent catch to search for exceptions or
      // goto  unwind resume to search from callsite
      WN *goto_unwind = WN_CreateGoto(parent_label);
      EH_insert_block_after(goto_unwind, cmp_insert_pos);
      WN_linenum(goto_unwind) = WN_linenum(cmp_insert_pos);
      if(need_fixup) {
        wn_label_need_fixup_map[goto_unwind] = parent_handler;
      }
    }
  }

  // get top parent Block
  WN *parentBlk = handlers[0];
  while(B2W_CONTEXT::Get_wn_parent(parentBlk, FALSE) != NULL) {
    parentBlk = B2W_CONTEXT::Get_wn_parent(parentBlk, FALSE);
  }
  if(Tracing() && B2W_LOG(B2W_LVL_EH)) {
    Is_Trace_Ex(TRUE, (TFile, "[WN Tree after handler:]\n"));
    Is_Trace_Ex(TRUE, (TFile, "------------------------------------------------\n"));
    //fdump_tree(TFile, parentBlk);
    Is_Trace_Ex(TRUE, (TFile, "------------------------------------------------\n"));
  }
}


//==============================================================================
// Build try catch block
// @param  WN*     try begin whirl node
// @param  WN*     try end whirl node
// @param  WN*     handler's whirl node
//==============================================================================
INT B2W_build_try_catch(WN *begin_wn, WN *end_wn, WN *parent_handler, WN **handlers, ST_IDX* handler_sts, INT32 len) {
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_EH), (TFile, "[B2W_build_try_catch]. begin %#0llx, "
    "end %#0llx, parent %#0llx, handlers: (%d)",
    (UINT64) begin_wn, (UINT64) end_wn, (UINT64) parent_handler, len));

  Is_Valid(begin_wn, ("begin wn should not be null"));
  Is_Valid(end_wn, ("end wn should not be null"));


  UINT32 cursor = (UINT32) try_idx_eh_info_map.size();

  eh_info.init_for_try(len, cursor);

  try_idx_eh_info_map[cursor] = new B2W_EH_Info(eh_info, cursor);

  B2W_build_try(begin_wn, end_wn);

  B2W_build_handlers(parent_handler, handlers, handler_sts, len);

  return cursor;
}

ST *B2W_get_eh_spec_st(int eh_spec_size) {
  ARB_HANDLE arb = New_ARB();
  ARB_Init (arb, 0, eh_spec_size-1, sizeof(ST_IDX));
  Set_ARB_flags (arb, ARB_flags(arb) | ARB_FIRST_DIMEN | ARB_LAST_DIMEN);
  STR_IDX str = Save_Str ("__EH_SPEC_TABLE__");
  TY_IDX ty;
  TY_Init (New_TY(ty), eh_spec_size*sizeof(ST_IDX), KIND_ARRAY, MTYPE_UNKNOWN, str);
  Set_TY_arb (ty, arb);
  Set_TY_etype (ty, MTYPE_TO_TY_array[MTYPE_U4]);
  ST * etable = New_ST (CURRENT_SYMTAB);
  ST_Init (etable, str, CLASS_VAR, SCLASS_EH_REGION_SUPP, EXPORT_LOCAL, ty);
  Set_ST_is_initialized (*etable);
	Set_ST_one_per_pu (etable);
  return etable;
}

//==============================================================================
// Setup EH array for:
// exc_ptr ST_IDX, filter ST_IDX, typeinfo INITO_IDX, eh_spec INITO_IDX
//==============================================================================
void B2W_setup_entry_for_eh (ST_IDX *eh_throws, int len) {
  const int lbnd = 0;
  const int hbnd = 3;

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_EH), (TFile, "[B2W_setup_entry_for_eh] \n"));

  ARB_HANDLE arb = New_ARB();
  ARB_Init (arb, lbnd, hbnd, 4);
  Set_ARB_flags (arb, ARB_flags(arb) | ARB_FIRST_DIMEN | ARB_LAST_DIMEN);
  STR_IDX str = Save_Str ("__EH_INFO_PER_PU__");
  TY_IDX ty;
  TY_Init (New_TY(ty), (hbnd+1) * 4 , KIND_ARRAY, MTYPE_M, str); /// put attention here
  Set_TY_arb (ty, arb);
  Set_TY_etype (ty, MTYPE_TO_TY_array[MTYPE_U4]);
  ST * etable = New_ST (CURRENT_SYMTAB);
  ST_Init (etable, str, CLASS_VAR, SCLASS_EH_REGION_SUPP, EXPORT_LOCAL, ty);
  Set_ST_is_initialized (*etable);
  Set_ST_one_per_pu (etable);

  ST  * exc_ptr_st = New_ST (CURRENT_SYMTAB);
  ST_Init (exc_ptr_st, Save_Str ("__Exc_Ptr__"), CLASS_VAR, SCLASS_AUTO,
    EXPORT_LOCAL, MTYPE_To_TY(Pointer_Mtype));
  Set_ST_one_per_pu (exc_ptr_st);
  
  INITV_IDX exc_ptr_iv = New_INITV();
  INITV_Set_VAL (Initv_Table[exc_ptr_iv], Enter_tcon (Host_To_Targ (MTYPE_U4,
                              ST_st_idx (exc_ptr_st))), 1);

  ST  * filter_st = New_ST (CURRENT_SYMTAB);

  ST_Init (filter_st, Save_Str ("__Exc_Filter__"), CLASS_VAR, SCLASS_AUTO,
                  EXPORT_LOCAL, MTYPE_To_TY(TARGET_64BIT ? MTYPE_U8 : MTYPE_U4));

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_EH), (TFile, "[__EH_INFO_PER_PU__] : [%#0x] \n", (UINT) etable->st_idx));
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_EH), (TFile, "[__Exc_Ptr__] : [%#0x] \n", exc_ptr_st->st_idx));
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_EH), (TFile, "[__Exc_Filter__] : [%#0x] \n", filter_st->st_idx));

  Set_ST_one_per_pu (filter_st);
  
  INITV_IDX filter_iv = New_INITV();

  INITV_Set_VAL (Initv_Table[filter_iv], Enter_tcon (Host_To_Targ (MTYPE_U4,
                              ST_st_idx (filter_st))), 1);
  Set_INITV_next (exc_ptr_iv, filter_iv);

  // this will be filled in later if there are type-filter entries
  INITV_IDX tinfo = New_INITV();
  INITV_Set_VAL (Initv_Table[tinfo], Enter_tcon (Host_To_Targ (MTYPE_U4,
                              0)), 1);
  Set_INITV_next (filter_iv, tinfo);



  ST *eh_spec_st = NULL;
  INITV_IDX eh_spec = New_INITV();
  INITO_IDX eh_spec_inito = 0;
  
  
  if (len > 0) {
    eh_spec_st = B2W_get_eh_spec_st(len);
    I64VEC eh_spec_init_list;
    eh_spec_init_list.push_back(B2W_INIT_MASK);
    eh_spec_init_list.push_back(B2W_INIT_START_BLK);
    for (int i = 0; i < len; i++) {
      eh_spec_init_list.push_back(B2W_INIT_TCON_U4_MASK);
      eh_spec_init_list.push_back(eh_throws[i]);
    }
    eh_spec_init_list.push_back(B2W_INIT_MASK);
    eh_spec_init_list.push_back(B2W_INIT_END_BLK);
    eh_spec_inito = B2W_initialize_with_array(eh_spec_init_list, ST_st_idx(eh_spec_st));
  }
  INITV_Set_VAL (Initv_Table[eh_spec], Enter_tcon (Host_To_Targ (MTYPE_U4,
                                eh_spec_inito)), 1);
  Set_INITV_next (tinfo, eh_spec);
  
 /*
  len = 0;
  I64VEC init_list;
  init_list.push_back(B2W_INIT_TCON_U4_MASK);
  init_list.push_back(ST_st_idx(exc_ptr_st));
  init_list.push_back(B2W_INIT_TCON_U4_MASK);
  init_list.push_back(ST_st_idx(filter_st));
  init_list.push_back(B2W_INIT_U4_CST_MASK);
  init_list.push_back(0);
  if (len > 0) {
    init_list.push_back(B2W_INIT_TCON_U4_MASK);
    init_list.push_back(ST_st_idx(eh_spec));
  } else {
    init_list.push_back(B2W_INIT_U4_CST_MASK);
    init_list.push_back(0);
  }
  INITV_IDX exc_ptr_iv = B2W_initialize_with_array(init_list, ST_st_idx(etable));
  */
  Set_PU_misc_info (Get_Current_PU(),
                    New_INITO (ST_st_idx (etable), exc_ptr_iv));
  entry_setup = TRUE;
}


//==============================================================================
// Build EH type info
// create type info symbol (__TYPEINFO_TABLE__ ), type, inito
//==============================================================================
void B2W_build_eh_type_info() {
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_EH), (TFile, "[B2W_build_eh_type_info] \n"));
  INITV_IDX blk, start;
  INITO_IDX id;

  for (int i=0; i<type_filter_vector.size(); ++i)
  {
    // generate INITV for filter symbol, filter value
    INITV_IDX st = New_INITV();
    if (type_filter_vector[i].st)
      INITV_Set_VAL (Initv_Table[st],
                     Enter_tcon (Host_To_Targ (MTYPE_U4,type_filter_vector[i].st)), 1);
    else
      INITV_Set_ZERO (Initv_Table[st], MTYPE_U4, 1);

    INITV_IDX filter = New_INITV();
    INITV_Set_VAL (Initv_Table[filter],
                    Enter_tcon (Host_To_Targ (MTYPE_U4, type_filter_vector[i].filter)), 1);
    Set_INITV_next (st, filter);

    if (i == 0)
    {
      blk = start = New_INITV();
      INITV_Init_Block (blk, st);
    }
    else
    {
      INITV_IDX next_blk = New_INITV();
      INITV_Init_Block (next_blk, st);
      Set_INITV_next (blk, next_blk);
      blk = next_blk;
    }
  }
    // generate type info table symbol, and its inito/initv info
  if (type_filter_vector.size() > 0)
  {
    // generate type info symbol
    ARB_HANDLE arb = New_ARB();
    ARB_Init (arb, 0, type_filter_vector.size()-1, sizeof(TYPE_FILTER_ENTRY));
    Set_ARB_flags (arb, ARB_flags(arb) | ARB_FIRST_DIMEN | ARB_LAST_DIMEN);
    STR_IDX str = Save_Str ("__TYPEINFO_TABLE__");
    FLD_HANDLE fld1 = New_FLD ();
    FLD_Init (fld1, Save_Str ("st"),
                            MTYPE_TO_TY_array[MTYPE_U4], 0);
    FLD_HANDLE fld2 = New_FLD ();
    FLD_Init (fld2, Save_Str ("filter"),
                            MTYPE_TO_TY_array[MTYPE_U4], 4);
    Set_FLD_flags (fld2, FLD_LAST_FIELD);

    TY_IDX struct_ty;
    TY_Init (New_TY(struct_ty), sizeof(TYPE_FILTER_ENTRY), KIND_STRUCT,
                            MTYPE_M, Save_Str ("__TYPEINFO_ENTRY__"));
    Set_TY_fld (struct_ty, fld1);
    TY_IDX ty;
    TY_Init (New_TY(ty), type_filter_vector.size()*sizeof(TYPE_FILTER_ENTRY), KIND_ARRAY, MTYPE_M, str);
    Set_TY_arb (ty, arb);
    Set_TY_etype (ty, struct_ty);
    ST * typeinfo = New_ST (CURRENT_SYMTAB);
    ST_Init (typeinfo, str, CLASS_VAR, SCLASS_EH_REGION_SUPP, EXPORT_LOCAL, ty);
    Set_ST_is_initialized (*typeinfo);
    Set_ST_one_per_pu (typeinfo);

    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_EH), (TFile, "[TYPEINFO<%s>] : [%#0x] \n", (CHPTR) (&Str_Table[str]), typeinfo->st_idx));

    id = New_INITO (ST_st_idx(typeinfo), start);
    // Store the inito_idx in the PU
    // 1. exc_ptr 2. filter : Set 3rd entry with inito_idx
    INITV_IDX index = INITV_next (INITV_next (INITO_val (
                                  PU_misc_info (Get_Current_PU()))));
    // INITV_Set_VAL resets the next field, so back it up
    // and set it again.
    INITV_IDX bkup = INITV_next (index);
    INITV_Set_VAL (Initv_Table[index], 
    Enter_tcon (Host_To_Targ (MTYPE_U4, id)), 1);
    Set_INITV_next (index, bkup);
  }
  // eh spec function end init deleted
}


//==============================================================================
// fixup goto labels
// for nesting try, inner try need fixup the goto lable to parent try
//==============================================================================
void B2W_fixup_goto_label() {
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_EH), (TFile, "[B2W_fixup_goto_label] \n"));
  map<WN*, WN*>::iterator fixupIter = wn_label_need_fixup_map.begin();
  while(fixupIter != wn_label_need_fixup_map.end()) {
    WN *gotoWN = fixupIter->first;
    WN *parentHandler = fixupIter->second;
    Is_Valid(handler_eh_info_map.find(parentHandler) != handler_eh_info_map.end(), ("cannot find parent handler label idx"));
    LABEL_IDX label_idx = handler_eh_info_map[parentHandler].cmp_idx;
    WN_label_number(gotoWN) = label_idx;
    fixupIter++;
  }
}

//==============================================================================
// Build EH begin
// initialize global area, setup eh entries
//==============================================================================
void B2W_build_eh_begin() {

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_EH), (TFile, "[B2W_build_eh_begin] \n"));
  type_filter_vector.clear();
  handler_eh_info_map.clear();
  wn_label_need_fixup_map.clear();
  eh_info.init();
}

//==============================================================================
// Build EH end
// fixup the labels, generate type info, clean global area
//==============================================================================
void B2W_build_eh_end() {

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_EH), (TFile, "[B2W_build_eh_end] \n"));
  B2W_fixup_goto_label();
  B2W_build_eh_type_info();

  type_filter_vector.clear();
  handler_eh_info_map.clear();
  wn_label_need_fixup_map.clear();

  map<INT, B2W_EH_Info *>::iterator it = try_idx_eh_info_map.begin();
  for (; it != try_idx_eh_info_map.end(); it++){
    B2W_EH_Info *info = it->second;
    delete info;
  }
  try_idx_eh_info_map.clear();
  map<WN*, Target_Misc_Info *>::iterator mit = target_label_idx_map.begin();
  for (; mit != target_label_idx_map.end(); mit++){
    Target_Misc_Info *misc = mit->second;
    delete misc;
  }
  target_label_idx_map.clear();
  entry_setup = false;
  eh_info.init();
}


//==============================================================================
// Return EXEC_PTR symbol index
//==============================================================================
ST_IDX B2W_get_exec_ptr() {
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_EH), (TFile, "[B2W_get_exec_ptr] \n"));
  /*
  if(!entry_setup) {
    B2W_setup_entry_for_eh();
  }
  */
  Is_True(entry_setup, ("Entry symbol did not set up."));
  ST_IDX exc_ptr = TCON_uval (INITV_tc_val (INITO_val (PU_misc_info(Get_Current_PU()))));
  return exc_ptr;
}

//==============================================================================
// Setting up new region_supp symbol(new_st)
// and initializes the new_st,
// then bind to region_wn;
//==============================================================================
void B2W_set_eh_inito(WN *region_wn, LABEL_IDX comparator_idx, ST_IDX supp_symbol, ST_IDX *symbols, UINT symbols_len) {
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_EH), (TFile, "[B2W_set_eh_inito] \n"));
  I64VEC init_list;
  Is_Valid((symbols_len < 1 ? 8 : ((symbols_len) * 4 + 4)) == TY_size(*B2W_get_typtr(ST_type(B2W_get_stptr(supp_symbol)))),
    ("incorrect supp_symbol_size"));

  if(symbols_len != 0) {
    LABEL_IDX label = comparator_idx;
    init_list.push_back(B2W_INIT_MASK);
    init_list.push_back(B2W_INIT_START_BLK);
    init_list.push_back(B2W_INIT_LABEL_MASK);
    init_list.push_back(label);
    for(UINT i = 0; i < symbols_len; i++) {
      // Assert that symbol is valid.
      if(symbols[i] != 0) B2W_get_stptr(symbols[i]);
      init_list.push_back(B2W_INIT_TCON_U4_MASK);
      init_list.push_back(symbols[i]);
      // FIXME: symbols[i] maybe zero.
      //Is_Trace_Ex(!is_found && Tracing() && B2W_LOG(B2W_LVL_EH), (TFile, "[B2W_set_eh_inito] cannot locate filter in type_filter_vec for ref_sym<%s> %#0x \n",
      //  ST_name(symbols[i]), symbols[i]));
    }
    init_list.push_back(B2W_INIT_MASK);
    init_list.push_back(B2W_INIT_END_BLK);
    WN_ereg_supp(region_wn) = B2W_initialize_with_array(init_list, supp_symbol);
  } else {
    init_list.push_back(B2W_INIT_MASK);
    init_list.push_back(B2W_INIT_START_BLK);
    init_list.push_back(B2W_INIT_U4_CST_MASK);
    init_list.push_back(0);
    init_list.push_back(B2W_INIT_U4_CST_MASK);
    init_list.push_back(0);
    init_list.push_back(B2W_INIT_MASK);
    init_list.push_back(B2W_INIT_END_BLK);
    WN_ereg_supp(region_wn) = B2W_initialize_with_array(init_list, supp_symbol);
  }
}


INT64 B2W_get_try_comparator (INT try_idx) {

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_EH), (TFile,
    "[B2W_get_try_comparator] try_idx %#0x : \n", (INT) try_idx));

  Is_True(try_idx_eh_info_map.size() > try_idx,
          ("[B2W_get_try_comparator] try idx must be within the length of try_idx_eh_info_map[0, %ld]", try_idx_eh_info_map.size()));

  B2W_EH_Info  *info = try_idx_eh_info_map[try_idx];

  Is_True(info != NULLPTR,    ("[B2W_get_try_comparator] eh_info <%#0x> is null.", info));
  Is_True(info->handler_begin_idx != 0, ("[B2W_get_try_comparator] eh_info <%#0x> has no comparator.", info));

  return info->handler_begin_idx;
}

/*============================================================================
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniInitEHSym
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_jniInitEHSym
  (JNIEnv *env, jclass, jlongArray eh_throws_sym) {
  jsize len = env->GetArrayLength(eh_throws_sym);
  jlong *syms = env->GetLongArrayElements(eh_throws_sym, 0);
  ST_IDX *symIdxs = new ST_IDX[len];
  for (int i=0; i<len; i++) {
    symIdxs[i] = (ST_IDX) syms[i];
  }
  B2W_setup_entry_for_eh(symIdxs, len);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniGetExecPtr
 * Signature: ()J
 ============================================================================*/
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniGetExecPtr
  (JNIEnv *, jclass) {
    return (jlong)B2W_get_exec_ptr();
}
/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniBuildEHBegin
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_jniBuildEHBegin
  (JNIEnv *, jclass) {
    B2W_build_eh_begin();
}

/*============================================================================
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniBuildTryCatch
 * Signature: (JJsJ[J[J)V
 ============================================================================*/
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniBuildTryCatch
  (JNIEnv *env, jclass, jlong begin_wn, jlong end_wn, jlong parent_handler, jlongArray wns, jlongArray sts) {

  jsize len1 = env->GetArrayLength(wns);
  jsize len2 = env->GetArrayLength(sts);
  Is_Valid(len1 == len2, ("handler wn cnt should be same as hanlder symbol cnt"));

  jlong * handlers = env->GetLongArrayElements(wns, 0);
  jlong * handler_sts = env->GetLongArrayElements(sts, 0);
  ST_IDX *symIdxs = new ST_IDX[len2];
  for (int i=0; i<len2; i++) {
    symIdxs[i] = (ST_IDX)handler_sts[i];
  }
  INT try_idx = B2W_build_try_catch((WN *)begin_wn, (WN *)end_wn, (WN *)parent_handler, (WN **)handlers, symIdxs, len1);
  delete[] symIdxs;
  return try_idx;
}

/*============================================================================
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniBuildEHEnd
 * Signature: ()V
 ============================================================================*/
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_jniBuildEHEnd
  (JNIEnv *, jclass) {
  B2W_build_eh_end();
}


/*=============================================================================
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniBuildEHRegion
 * Signature: (JJJJ)J
 ============================================================================*/
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniBuildEHRegion
  (JNIEnv *, jclass, jlong body, jlong exit, jlong pragma, jlong supp_inito){
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_EH), (TFile,
    "[jniBuildEHRegion] body:%#0lx, exit:%#0lx, pragma:%#0lx, supp_inito:%#0lx \n",
    body, exit, pragma, supp_inito));
  return (jlong) B2W_build_eh_region(
    (WN *) body, (WN *) pragma, (WN *) exit, (INITO_IDX) supp_inito);
}

/*=============================================================================
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniSetupRegionInito
 * Signature: (JJJ[J)V
 ============================================================================*/
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_jniSetupRegionInito
  (JNIEnv *env, jclass, jlong region_wn, jlong handler_idx, jlong new_st, jlongArray sym_list){
  jsize len = env->GetArrayLength(sym_list);
  jlong *idx_long_list = env->GetLongArrayElements(sym_list, 0);
  ST_IDX *symbols;
  if(len == 0) {
    symbols = NULLPTR;
  } else {
    symbols = new ST_IDX[len + 1];
    for (UINT i = 0; i < len; i++) {
      symbols[i] = (ST_IDX) idx_long_list[i];
    }
  }
  B2W_set_eh_inito((WN*) region_wn, (LABEL_IDX) handler_idx, (ST_IDX) new_st, symbols, (UINT) len);
  delete symbols;
}


/* ============================================================================
 * Class:     io_xcalibyte_BGenDriver
 * Method:    getTryComparator
 * Signature: (J)J
 ============================================================================*/
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_getTryComparator
  (JNIEnv *, jclass, jlong try_idx) {
  return (jlong) B2W_get_try_comparator(try_idx);
}