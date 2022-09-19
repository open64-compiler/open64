/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

#include "b2w_common.h"
#include "b2w_handler.h"
#include <symtab_compatible.h>
#include "dwarf_DST_producer.h"
#include <map>

//typedef     std::stack<WN_STMT>   STMTSTK;
//STMTSTK     whirl_stmt_stack;
// a map to store the mapping of wn -> parent block
using std::map;
using std::vector;

// map the region body first wn to region 
// static map<WN*, WN*> wn_region_map;

/******************************************************************************
 * B2W_map_tab_create
 * mirroring for WN_MAP_TAB_Create
 *****************************************************************************/
WN *B2W_create_block(){
    WN *pWN = WN_CreateBlock();
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE),
             (TFile, "[B2W_create_block] -> 0x%llx \n", (UINT64) pWN));
    return pWN;
}


/******************************************************************************
 * B2W_Create_Region
 * mirroring for WN_CreateRegion
 *****************************************************************************/
WN *B2W_Create_Region(REGION_KIND region_kind, WN* region_body, WN* region_pragmas,
                      WN *region_exit, INITO_IDX inito_idx) {
  WN *region = WN_CreateRegion(region_kind, region_body, region_pragmas, region_exit,
                         New_Region_Id(), inito_idx);
  
  B2W_CONTEXT::Set_wn_parent_direct(region_body, region);
  WN_Set_Linenum(region, WN_Get_Linenum(region_body));
  return region;
}

/******************************************************************************
 * B2W_map_tab_create
 * mirroring for WN_MAP_TAB_Create
 *****************************************************************************/
MTABPTR B2W_map_tab_create(void * pool){
    Is_Valid(pool != NULLPTR, ("%s\n","**** Error in B2W_map_tab_create : %s",
                          "Null Pool Pointer"));
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE), (TFile, "%s0x%llx\n",
        "[B2W_map_tab_create] create map in pool@", (UINT64) pool));
    return WN_MAP_TAB_Create((mem_pool *) pool);
}

/******************************************************************************
 * B2W_int_const
 * mirroring for WN_Int_Const
 *****************************************************************************/
WN * B2W_int_const(UINT mtype, UINT64 const_value){
    WN * wn = WN_Intconst(mtype, const_value);
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_WN_MINOR),
             (TFile, "[B2W_int_const] const(type of %u) = 0x%llx @ 0x%llx\n",
             (UINT) mtype, const_value, (UINT64)wn));
    return wn;
}

/******************************************************************************
 * B2W_float_const
 * mirroring for WN_Float_Const
 *****************************************************************************/
WN * B2W_float_const(UINT mtype, double const_value){
    WN * wn = WN_Floatconst(mtype, const_value);
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_WN_MINOR),
             (TFile, "[B2W_float_const] const(type of %u) = 0x%llx @ 0x%llx\n",
                 (UINT) mtype, (UINT64) const_value, (UINT64)wn));

    return wn;
}

/******************************************************************************
 * B2W_string_const
 * mirroring for WN_LdaString
 *****************************************************************************/
WN * B2W_lda_string(const char *str) {
  WN *wn = WN_LdaString(str, 0, strlen(str));
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_WN_MINOR),
              (TFile, "[B2W_lda_string] 0x%llx\n", (UINT64) wn));
  return wn;
}


/******************************************************************************
 * B2W_set_line_num
 * @param line: line number
 * mirroring for WN_Set_Linenum
 *****************************************************************************/
void B2W_set_line_num(UINT64 wn_ptr, UINT line){
    Is_Valid(wn_ptr != 0, ("[B2W_set_line_num][ERROR] wn : Null Pointer "
                          "-- value : %u \n", line));
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_WN_MINOR),
      (TFile, "[B2W_set_line_num] wn : %llu, val : %u \n",
        wn_ptr, line));
    SRCPOS srcpos;
    SRCPOS_clear(srcpos);
    SRCPOS_filenum(srcpos) = B2W_CONTEXT::Get_file_num();
    SRCPOS_linenum(srcpos) = line;

    WN * wn = (WN *) wn_ptr;
    if(OPERATOR_is_stmt(WN_operator(wn))) {
      WN_Set_Linenum(wn, srcpos);
    } else if (WN_operator(wn) == OPR_IF || WN_operator(wn) == OPR_GOTO ||
      WN_operator(wn) == OPR_BLOCK || WN_operator(wn) == OPR_FUNC_ENTRY ||
      WN_operator(wn) == OPR_CASEGOTO || WN_operator(wn) == OPR_COMPGOTO ||
      WN_operator(wn) == OPR_DO_LOOP || WN_operator(wn) == OPR_DO_WHILE ||
      WN_operator(wn) == OPR_GOTO_OUTER_BLOCK || WN_operator(wn) == OPR_IDNAME ||
      WN_operator(wn) == OPR_LOOP_INFO || WN_operator(wn) == OPR_OPTPARM ||
      WN_operator(wn) == OPR_PRAGMA || WN_operator(wn) == OPR_REGION ||
      WN_operator(wn) == OPR_SELECT || WN_operator(wn) == OPR_SWITCH ||
      WN_operator(wn) == OPR_TRAP || WN_operator(wn) == OPR_TRUNC ||
      WN_operator(wn) == OPR_WHERE || WN_operator(wn) == OPR_WHILE_DO ||
      WN_operator(wn) == OPR_XGOTO || WN_operator(wn) == OPR_XPRAGMA) {
      WN_Set_Linenum(wn, srcpos);
    } else {
       Is_Trace_Ex(TRUE, (stderr, "\n \n -----  BEGIN: %lld  ----- \n", (INT64) wn));
       fdump_tree(stderr, wn);
       fflush(stderr);
       Is_Trace_Ex(TRUE, (stderr, " -----  END:   %lld ----- \n", (INT64) wn));
       Is_Valid(FALSE, ("Redundant adding linenum to the wn, opr = %d", WN_operator(wn)));
    }
}


/******************************************************************************
 * B2W_get_line_num
 * @param wn
 * mirroring for WN_Get_Linenum
 *****************************************************************************/
UINT B2W_get_line_num(WN *wn) {
  Is_Valid(wn != 0, ("[B2W_get_line_num][ERROR] wn : Null Pointer\n"));
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_WN_MINOR),
              (TFile, "[B2W_get_line_num] wn :%p\n", wn));
  SRCPOS srcpos = WN_Get_Linenum(wn);
  return SRCPOS_linenum(srcpos);
}

/******************************************************************************
 * B2W_get_body_from_entry
 * mirroring for WN_func_body
 *****************************************************************************/
WN * B2W_get_body_from_entry(WN *pWN) {
  /*
  WN_entry_name(wn) = name;
  WN_func_body(wn) = body;
  WN_func_pragmas(wn) = WN_block_element(pragmas);
  WN_func_varrefs(wn) = WN_block_element(varrefs);
   */
  return WN_func_body(pWN);
}

/******************************************************************************
 * B2W_start_function
 * Initializing Entry, Block...
 * @return WN * entry
 *****************************************************************************/
UINT64 B2W_start_function(ST_IDX func_st, UINT num_args, UINT scope, INT line){
  B2W_CONTEXT::Clear_wn_parent_map();

  // Function Decl Test
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_DATA), (TFile, "%s[B2W_start_function]\n"
          "st : (%s)%u, num_args: %u, scope: %u, line: %d\n%s",
          DBar, ST_name(func_st), func_st, num_args, scope, line, DBar));

  Is_Valid(CURRENT_SYMTAB == 2,
          ("%s%d \n", "[B2W_start_function] error because CURRENT_SYMTAB "
                      "require 2, given ", scope));

  Is_Valid(scope == 2, ("%s%d \n", "[B2W_start_function] error because "
                       "scope require 2, given ", scope));

  UINT64 vla_block_wn = (UINT64) B2W_create_block();
  ST_IDX  st          = func_st;
  ST     *func_st_ptr = B2W_get_stptr(st);
  TY_IDX  func_type   = ST_pu_type(st);
  TY_IDX  ret_ty_idx  = TY_ret_type(func_type);
  WN *    body        = NULLPTR;
  WN *    wn          = NULLPTR;
  WN *    entry_wn    = NULLPTR;
  Set_ST_sclass(func_st_ptr, SCLASS_TEXT);
  Scope_tab[scope].st = func_st_ptr;
  body                = WN_CreateBlock();
  entry_wn            = WN_CreateEntry(num_args, st, body, NULLPTR, NULLPTR);

  B2W_set_line_num((UINT64) entry_wn, (UINT) line);
  B2W_set_line_num((UINT64) body, (UINT) line);
  B2W_set_line_num(vla_block_wn, line);

  // from 1..nkids=num_args, create IDNAME args for OPR_FUNC_ENTRY
  INT i = 0;

  PU_Info *pu_info    = TYPE_MEM_POOL_ALLOC(PU_Info, Malloc_Mem_Pool);

  PU_Info_init(pu_info);

  Set_PU_Info_tree_ptr (pu_info, entry_wn);
  PU_Info_maptab (pu_info)        = Current_Map_Tab;
  PU_Info_proc_sym (pu_info)      = st;

  Set_PU_Info_state(pu_info, WT_SYMTAB,   Subsect_InMem);
  Set_PU_Info_state(pu_info, WT_TREE,     Subsect_InMem);
  Set_PU_Info_state(pu_info, WT_PROC_SYM, Subsect_InMem);
  Set_PU_Info_flags(pu_info, PU_IS_COMPILER_GENERATED);

  PU& pu = Pu_Table[ST_pu (St_Table [PU_Info_proc_sym (pu_info)])];
  if (strcmp (ST_name (func_st), "main") == 0) {
    Set_PU_is_mainpu (pu);
    Set_PU_no_inline (pu);
  } else {
    Set_PU_is_inline_function (pu);
  }

  if (B2W_CONTEXT::Get_pu_info_tab(CURRENT_SYMTAB) != NULL) {
    //LEVEL exist, set to next
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_VERBOSE),
      (TFile, "[B2W_start_function] Adding pu to Cur->next \n"));
    PU_Info_next (B2W_CONTEXT::Get_pu_info_tab (CURRENT_SYMTAB)) = pu_info;
  } else if (CURRENT_SYMTAB == GLOBAL_SYMTAB + 1) {
      // level not exist, create level;
      // CURRENT=2, GLOBAL=1, New_Function
      B2W_CONTEXT::Set_pu_root(pu_info);
      Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_VERBOSE),
        (TFile, "[B2W_start_function] Adding pu to pu_root \n"));
  } else {
      PU_Info_child (B2W_CONTEXT::Get_pu_info_tab(CURRENT_SYMTAB - 1)) = pu_info;
      Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_VERBOSE),
        (TFile, "[B2W_start_function]Adding pu to pu_root->child \n"));
  }

  B2W_CONTEXT::Set_pu_info_tab (CURRENT_SYMTAB, pu_info);

  B2W_CONTEXT::Push_pu_stmt (entry_wn);
  B2W_CONTEXT::Push_pu_stmt (body);

  wn = WN_CreatePragma (WN_PRAGMA_PREAMBLE_END, (ST_IDX) NULL, 0, 0);
  B2W_set_line_num((UINT64) wn, (UINT) line);

  B2W_insert_block_last(body, wn);
  B2W_insert_block_last(body, (WN *) vla_block_wn);

  // TODO: VARARGS
  if (FALSE) {    // for global destructor

      INITV_IDX initv = New_INITV ();
      INITV_Init_Symoff (initv, func_st_ptr, 0, 1);
      ST *init_st = New_ST (GLOBAL_SYMTAB);
      ST_Init (init_st, Save_Str2i ("__dtors", "_", 1), //++__dtors -> 1
               CLASS_VAR, SCLASS_FSTATIC,
               EXPORT_LOCAL, Make_Pointer_Type (ST_pu_type (func_st), FALSE));
      Set_ST_is_initialized (init_st);
      INITO_IDX inito = New_INITO (init_st, initv);
      ST_ATTR_IDX st_attr_idx;
      ST_ATTR&    st_attr = New_ST_ATTR (GLOBAL_SYMTAB, st_attr_idx);
      ST_ATTR_Init (st_attr, ST_st_idx (init_st), ST_ATTR_SECTION_NAME,
                    Save_Str (".dtors"));
      Set_PU_no_inline (Pu_Table [ST_pu (func_st_ptr)]);
      Set_PU_no_delete (Pu_Table [ST_pu (func_st_ptr)]);
      Set_ST_addr_saved (func_st_ptr);
  }

  if (FALSE) { // Constructor

      INITV_IDX initv = New_INITV();
      INITV_Init_Symoff(initv, func_st_ptr, 0, 1);
      Set_ST_addr_saved(func_st_ptr);
      ST *init_st = New_ST(GLOBAL_SYMTAB);
      ST_Init(init_st, Save_Str2i("__ctors", "_", 1), //__ctors++
              CLASS_VAR, SCLASS_FSTATIC,
              EXPORT_LOCAL, Make_Pointer_Type(ST_pu_type(func_st), FALSE));
      Set_ST_is_initialized(init_st);
      INITO_IDX inito = New_INITO(init_st, initv);
      ST_ATTR_IDX st_attr_idx;
      ST_ATTR &st_attr = New_ST_ATTR(GLOBAL_SYMTAB, st_attr_idx);
      ST_ATTR_Init(st_attr, ST_st_idx(init_st), ST_ATTR_SECTION_NAME,
                   Save_Str(".ctors"));
      Set_PU_no_inline(Pu_Table[ST_pu(func_st_ptr)]);
      Set_PU_no_delete(Pu_Table[ST_pu(func_st_ptr)]);
  }

  B2W_CONTEXT::Push_current_entry_wn(wn);

  SRCPOS srcpos = 0;
  SRCPOS_column(srcpos) = 1;
  SRCPOS_filenum(srcpos) = B2W_CONTEXT::Get_file_num();
  SRCPOS_linenum(srcpos) = line <= 0 ? 1 : line; // FIXME: is this correct line num ? need testcase.
  B2W_dst_build_subprogram(func_st, pu_info, srcpos);

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_INFO), (TFile, "[B2W_start_function] .. "
                                "entry_wn: 0x%llx, body_wn: 0x%llx \n",
                                (UINT64) entry_wn, (UINT64) body));
    return (UINT64) entry_wn;
}

/******************************************************************************
 * B2W_finish_function
 * @return WN * entry
 *****************************************************************************/

void B2W_finish_function(UINT line){

    Is_Valid(CURRENT_SYMTAB == 2,
            ("[ERROR][B2W_finish_function] current_symtab misplaced"));

    PU_Info *pu_info = B2W_CONTEXT::Get_pu_info_tab(CURRENT_SYMTAB);
    // Insert a RETURN if it does not exist
    WN * block = B2W_CONTEXT::Pu_last_stmt();
    WN * wn = WN_last (block);
    if (wn == NULLPTR ||
        (WN_operator (wn) != OPR_RETURN &&
         WN_operator (wn) != OPR_RETURN_VAL)) {
        WN *returnWN = WN_CreateReturn();
        B2W_set_line_num((UINT64)returnWN, line);
        B2W_insert_block_last(block, returnWN);
    }

    // write out all the PU information
    B2W_CONTEXT::Pop_pu_stmt();
    B2W_CONTEXT::Pop_pu_stmt();

    // deallocate the old map table
    if (Current_Map_Tab) {
        WN_MAP_TAB_Delete(Current_Map_Tab);
        Current_Map_Tab = NULLPTR;
    }

    Write_PU_Info (pu_info);

    B2W_CONTEXT::Set_pu_info_tab(CURRENT_SYMTAB + 1, NULLPTR);


//    if (Return_Address_ST [CURRENT_SYMTAB]) {
//        Set_PU_has_return_address (Get_Current_PU ());
//        Set_PU_no_inline (Get_Current_PU ());
//        Return_Address_ST [CURRENT_SYMTAB] = NULL;
//    }

    B2W_CONTEXT::Pop_current_entry_wn();

    Delete_Scope (CURRENT_SYMTAB);
    --CURRENT_SYMTAB;

    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_INFO),
            (TFile, "%s[B2W_finish_function] .. "
            "func_pu->sym->name : %s ... finished \n%s\n\n\n\n\n",
            DBar, ST_name(pu_info->proc_sym), DBar));
}


DST_IDX B2W_dst_build_subprogram(ST_IDX func_st, PU_Info * pu_info, SRCPOS srcpos) {

  USRCPOS        src;
  DST_inline     inlin  = 0;
  USRCPOS_srcpos(src)   = srcpos;
  DST_INFO_IDX   dst    = DST_INVALID_INIT;
  DST_INFO_IDX   ret    = DST_INVALID_IDX;
  CHPTR          link   = ST_name(func_st);
  ST_IDX         fstidx = func_st;
  DST_IDX        scope  = B2W_CONTEXT::Get_comp_unit_dst();
  dst                   = DST_mk_subprogram(
    src,			// srcpos
    ST_name (func_st),
    ret,                    // return type
    DST_INVALID_IDX,        // Index to alias for weak is set later
    func_st,                 // index to fe routine for st_idx
    inlin,                  // applies to C++
    DW_VIRTUALITY_none,     // applies to C++
    0,                      // vtable_elem_location
    FALSE,                  // is_declaration
    FALSE,                  // is_artificial
    FALSE,                  //
    ! ST_is_export_local(B2W_get_stptr(func_st)) );  // is_external

  DST_RESET_assoc_fe(DST_INFO_flag(DST_INFO_IDX_TO_PTR(dst)));
  DST_append_child(scope, dst);
  // Below is for member function
  //  if (class_func_found_member) {
  //    DST_add_specification_to_subprogram(dst, class_func_idx);
  //  }
  DST_add_linkage_name_to_subprogram(dst, link);
  PU_Info_pu_dst(pu_info) = dst;
}

/******************************************************************************
 * B2W_if Create OPERATOR : NOT_EQUAL(!=)
 * @return WN * node
 *****************************************************************************/
WN * B2W_if (WN * test, WN * then_block, WN * else_block){
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_DEBUG), (TFile, "[B2W_if] test : 0x%llx, then : 0x%llx, else:0x%llx \n",
    (UINT64) test, (UINT64) then_block, (UINT64) else_block));
  return (WN *) WN_CreateIf(test, then_block, else_block);
}

/******************************************************************************
 * B2W_trueBr Create TRUEBR if cmp_wn evaluate to non zero goto label, otherwise
 * control flows to next statement
 * @return WN * node
 *****************************************************************************/
WN * B2W_trueBr (int label_idx, WN *cmp_wn){
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_DEBUG), (TFile, "[B2W_trueBr] label : %d, cmpWN : 0x%llx\n",
    label_idx, (UINT64) cmp_wn));
  return (WN *) WN_CreateTruebr(label_idx, cmp_wn);
}

/******************************************************************************
 * B2W_trueBr Create TRUEBR if cmp_wn evaluate to zero goto label, otherwise
 * control flows to next statement
 * @return WN * node
 *****************************************************************************/
WN * B2W_falseBr (int label_idx, WN *cmp_wn){
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_DEBUG), (TFile, "[B2W_falseBr] label : %d, cmpWN : 0x%llx\n",
    label_idx, (UINT64) cmp_wn));
  return (WN *) WN_CreateFalsebr(label_idx, cmp_wn);
}

/******************************************************************************
 * B2W_insert_block_last , copy of WGEN_Stmt_Append
 * @param block WN * block
 * @param stmt  WN * stmt to insert to block
 *****************************************************************************/
void B2W_insert_block_last (WN * block, WN * stmt){

  Is_True ((WN_operator(block) == OPR_BLOCK),
      ("[B2W_insert_block_last] NOT A VALID BLOCK WN\n"));

  B2W_CONTEXT::Set_wn_parent(stmt, block);

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_INSERT), (TFile, "  [B2W_insert_block_last] stmt(0x%llx)->block(0x%llx)"
    , (UINT64) stmt, (UINT64) block));

  //if( WN_operator(block) == OPR_BLOCK && WN_first(block) == NULLPTR)
  //  return;

  // if (TRUE) {
  //   //srcpos
  //   UINT dst_id = B2W_get_file_dst_info((CHPTR) "MySampleInputFile.class",
  //                         B2W_get_dir_dst_info((CHPTR) "/MySampleInputDir"));
  //   INT64 linenum = ((UINT64) dst_id << 32 ) + 0;
  //   WN_Set_Linenum ( stmt, linenum );
  //   if (WN_operator(stmt) == OPR_BLOCK && WN_first(stmt) != NULLPTR) {
  //     WN_Set_Linenum(WN_first(stmt), linenum);
  //   }
  // }

  WN_INSERT_BlockLast(block, stmt);
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_INSERT), (TFile, ",now_count: %d \n", WN_kid_count(block)));

}

/******************************************************************************
 * B2W_insert_block_first
 * @param block WN * block
 * @param stmt  WN * stmt to insert to block
 *****************************************************************************/
void B2W_insert_block_first (WN * block, WN * stmt){

  B2W_CONTEXT::Set_wn_parent(stmt, block);

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_INSERT), (TFile, "  [B2W_insert_block_first] stmt(0x%llx)->block(0x%llx)\n"
    , (UINT64) stmt, (UINT64) block));

  WN_INSERT_BlockFirst(block, stmt);
}

/******************************************************************************
 * B2W_insert_block_before
 * @param block      WN * block
 * @param stmt       WN * stmt to insert to block
 * @param target_pos WN * insert stmt before target_pos
 *****************************************************************************/
void B2W_insert_block_before (WN * block, WN * stmt, WN * target_pos){

  B2W_CONTEXT::Set_wn_parent(stmt, block);
  
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_INSERT), (TFile, "  [B2W_insert_block_before] stmt(0x%llx)->block(0x%llx)-> before(0x%llx)\n"
    , (UINT64) stmt, (UINT64) block, (UINT64)target_pos));

  WN_INSERT_BlockBefore(block, target_pos, stmt);
}

/******************************************************************************
 * B2W_insert_block_after
 * @param block      WN * block
 * @param stmt       WN * stmt to insert to block
 * @param target_pos WN * insert stmt after target_pos
 *****************************************************************************/
void B2W_insert_block_after (WN * block, WN * stmt, WN * target_pos){

  B2W_CONTEXT::Set_wn_parent(stmt, block);
  
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_INSERT), (TFile, "  [B2W_insert_block_after] stmt(0x%llx)->block(0x%llx)-> after(0x%llx)\n"
    , (UINT64) stmt, (UINT64) block, (UINT64)target_pos));
  WN_INSERT_BlockAfter(block, target_pos, stmt);
}


/******************************************************************************
 * B2W_LDA
 * @return WN *
 *****************************************************************************/
WN * B2W_lda (TYPE_ID desc, WN_OFFSET offset, ST_IDX sym)
{
  B2W_get_stptr(sym);
  WN * wn = WN_Lda(desc, offset, ST_ptr(sym), 0);
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_WN_MIDDLE),
              (TFile, "[B2W_lda] sym : %s (%u) -> wn @ 0x%llx \n", ST_name(sym), sym, (UINT64) wn));
  return wn;
}

/******************************************************************************
 * B2W_LDID
 * @return WN *
 *****************************************************************************/
WN * B2W_ldid (TYPE_ID desc, WN_OFFSET offset, ST_IDX sym,
                 TY_IDX ty, UINT field_id){

  B2W_get_stptr(sym);
  B2W_get_typtr(ty);

  Is_Valid(ty != 0, ("[B2W_ldid] ty is zero"));
  WN * wn = WN_Ldid(desc, offset, sym, ty, field_id);

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_WN_MIDDLE),
      (TFile, "[B2W_ldid] sym : %s (%u) -> wn @ 0x%llx \n",
          ST_name(sym), sym, (UINT64) wn));

  return wn;
}

//=============================================================================
// WN_STID
//=============================================================================
WN * B2W_stid (TYPE_ID desc, WN_OFFSET offset, ST_IDX sym,
                 TY_IDX ty, WN * val, UINT field_id){

  B2W_get_stptr(sym);
  B2W_get_typtr(ty);

  Is_Valid(val != NULLPTR, ("[B2W_stid] val is null pointer"));
  WN * wn = WN_Stid(desc, offset, &St_Table[sym], ty, val, field_id);

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_WN_MIDDLE),
           (TFile, "[B2W_stid] val(0x%llx) -> sym : %s (%u),  wn @ 0x%llx\n",
               (UINT64) val, ST_name(sym), sym, (UINT64) wn));
  return wn;
}

//=============================================================================
// WN_STORE
//=============================================================================
WN *B2W_istore(TYPE_ID desc, WN_OFFSET offset, TY_IDX ty_idx, WN *addr, WN *value, FLD_IDX field_id)
{
  B2W_get_typtr(ty_idx);
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_WN_IMP), (TFile,
                            "[B2W_istore] desc: %d, ty_idx: %u, addr(0x%llx)+offset(%d)-> wn(0x%llx), field_id: 0x%llx\n",
                            desc, ty_idx, (UINT64) addr, offset, (UINT64) value, (UINT64) field_id));
  WN *ret = WN_Istore(desc, offset, ty_idx, addr, value, field_id);
  return ret;
}

WN *B2W_Iload(TYPE_ID desc, WN_OFFSET offset, TY_IDX ty_idx, WN *addr,
             UINT field_id) {
  B2W_get_typtr(ty_idx);
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_WN_IMP), (TFile,
                            "[B2W_Iload] desc: %d, ty_idx: %u, addr(0x%llx)+offset(%d), field_id: 0x%llx\n",
                            desc, ty_idx,  (UINT64) addr, offset, (UINT64) field_id));
  WN *ret = WN_Iload(desc, offset, ty_idx, addr, field_id);
  return ret;
}

WN * B2W_exp2(OPERATOR opr, TYPE_ID rtype, TYPE_ID desc, WN *kid0, WN *kid1) {
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_WN_MINOR),
           (TFile, "[B2W_create_exp2] entry \n"));
  return WN_CreateExp2(opr, rtype, desc, kid0, kid1);
}

WN * B2W_unary(OPERATOR opr, TYPE_ID rtype, WN *lhs) {
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_WN_MINOR),
           (TFile, "[B2W_unary] opr = %u , rtype = %u, lhs = 0x%llx\n",
            opr, rtype, (UINT64) lhs));
  return WN_Unary(opr, rtype, lhs);
}

WN * B2W_binary(OPERATOR opr, TYPE_ID rtype, WN *lhs, WN *rhs) {
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_WN_MINOR),
           (TFile, "[B2W_binary] opr = %u , rtype = %u, kid0 = 0x%llx, kid1 = 0x%llx\n",
               opr, rtype, (UINT64) lhs, (UINT64) rhs));
  return WN_Binary(opr, rtype, lhs, rhs);
}

WN * B2W_ternary(OPERATOR opr, TYPE_ID rtype, WN *kid0, WN *kid1, WN *kid2) {
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_WN_MINOR),
           (TFile, "[B2W_ternary] opr = %u , rtype = %u, kid0 = 0x%llx, "
            "kid1 = 0x%llx, kid2 = 0x%llx\n",
            opr, rtype, (UINT64) kid0, (UINT64) kid1, (UINT64) kid2));
  return WN_Ternary(opr, rtype, kid0, kid1, kid2);
}





//=============================================================================
// Unary Operator
//=============================================================================
WN* B2W_ilda(OPERATOR opr, TYPE_ID rtype, TYPE_ID desc, WN_OFFSET offset, TY_IDX ty)
{
  B2W_get_typtr(ty);
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_WN_MINOR),
              (TFile, "[B2W_ilda] opr = %u, rtype = %u, desc = %u, offset = %u, ty = %u\n",
               opr, rtype, desc, offset, ty));
  return WN_CreateIlda(opr, rtype, desc, offset, ty);
}
// WN* B2W_strctfld(TYPE_ID ret_mtype, WN * kid0)
// {
//     Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_WN_MINOR),
//                 (TFile, "[B2W_strctfld] rtype = %u, kid0 = 0x%llx\n",
//                  rtype, (UINT64) kid0));
//     return WN_Neg(ret_mtype, kid0);
// }
WN* B2W_extract_bits(TYPE_ID rtype, TYPE_ID desc, WN * kid0)
{
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_WN_MINOR),
                (TFile, "[B2W_extract_bits] rtype = %u, kid0 = 0x%llx\n",
                 rtype, (UINT64) kid0));
    return WN_CreateExp1(OPR_EXTRACT_BITS, rtype, desc, kid0);
}
WN* B2W_parm(TYPE_ID rtype, WN * kid0, TY_IDX ty, UINT32 flag)
{
  B2W_get_typtr(ty);
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_WN_MINOR),
              (TFile, "[B2W_parm] rtype = %u, kid0 = 0x%llx\n",
               rtype, (UINT64) kid0));
  return WN_CreateParm(rtype, kid0, ty, flag);
}
WN* B2W_asm_input(char* constraint_string, UINT32 opnd_num, WN *value)
{
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_WN_MINOR),
              (TFile, "[B2W_asm_input] constraint_string = %s, opnd_num = %u, value = 0x%llx\n",
               constraint_string, opnd_num, (UINT64) value));
  return WN_CreateAsm_Input(constraint_string, opnd_num, value);
}
WN* B2W_alloca(WN *size)
{
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_WN_MINOR),
              (TFile, "[B2W_alloca] size = 0x%llx\n", (UINT64) size));
  return WN_CreateAlloca(size);
}
//=============================================================================
// Binary Operator
//=============================================================================

WN *B2W_relational(OPERATOR opr, TYPE_ID rtype, WN *l, WN *r) {
    return WN_CreateExp2(opr, Boolean_type, rtype, l, r);
}

WN* B2W_pair(UINT rtype, WN *lhs, WN *rhs)
{
    return B2W_binary(OPR_PAIR, rtype, lhs, rhs);
}
WN* B2W_comma(TYPE_ID rtype, TYPE_ID desc_mtype, WN * kid0, WN *kid1)
{
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_WN_MIDDLE),
                (TFile, "[B2W_comma] rtype = %u, desc = %u, kid0 = 0x%llx, kid1 = 0x%llx\n",
                 rtype, desc_mtype, (UINT64) kid0, (UINT64) kid1));
    return WN_CreateComma(OPR_COMMA, Mtype_comparison(rtype), desc_mtype, kid0, kid1);
}
WN* B2W_rcomma(TYPE_ID rtype, TYPE_ID desc_mtype, WN * kid0, WN * kid1)
{
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_WN_MIDDLE),
                (TFile, "[B2W_rcomma] rtype = %u, desc = %u, kid0 = 0x%llx, kid1 = 0x%llx\n",
                 rtype, desc_mtype, (UINT64) kid0, (UINT64) kid1));
    return WN_CreateRcomma(OPR_RCOMMA, rtype, desc_mtype, kid0, kid1);
}

WN * B2W_create_while_do (WN *test, WN *body){
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_WN_MIDDLE),
                (TFile, "[Creating While-Do] \n"));
    return WN_CreateWhileDo(test, body);
}

WN * B2W_create_do_while (WN *test, WN *body){
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_WN_MIDDLE),
                (TFile, "[Creating Do-While] \n"));
    return WN_CreateDoWhile(test, body);
}
WN * B2W_create_do      (WN *index, WN *start, WN *l_end /* l_end = end */, WN *step,
                                    WN *body, WN *loop_info){
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_WN_MIDDLE),
                (TFile, "[Creating DO] \n"));
    return WN_CreateDO(index, start, l_end, step, body,
                       loop_info);
}



WN * B2W_icall(TYPE_ID rtype, TYPE_ID desc, INT32 n, TY_IDX ty){
  B2W_get_typtr(ty);
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_WN_IMP),
                (TFile, "[B2W_icall] \n  n= TY_IDX = %u\n", ty ));
  return WN_Icall(rtype, desc, n, ty);
}

WN * B2W_call(TYPE_ID type, TYPE_ID desc, INT32 n, ST_IDX s){
  B2W_get_stptr(s);
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_WN_IMP),
                (TFile, "  [B2W_call] ST_IDX = %u\n", s));

  return WN_Call(type, desc, n, s);
}



WN * B2W_int_type_conversion( WN *wn, TYPE_ID to_type ){
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_WN_MINOR),
                (TFile, "[B2W_int_type_conversion] ----(INT) Converting from %x to %x ---- \n",
                  WN_ty(wn), MTYPE_To_TY(to_type)));
    return WN_Int_Type_Conversion(wn, to_type);
}
WN * B2W_float_type_conversion( WN *wn, TYPE_ID to_type ){
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_WN_MINOR),
                (TFile, "[B2W_float_type_conversion] ----(Float) Converting from %x to %x ---- \n",
                  WN_ty(wn), MTYPE_To_TY(to_type)));
    return WN_Float_Type_Conversion(wn, to_type);
}

WN * B2W_type_conversion( WN *wn, TYPE_ID to_type ){
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_WN_MINOR),
                (TFile, "[B2W_type_conversion] ----(Type) Converting from %x to %x ---- \n",
                  WN_ty(wn), MTYPE_To_TY(to_type)));
    return WN_Type_Conversion(wn, to_type);
}


WN       *B2W_create_label    (INT32 label_number,
                               UINT32 label_flag, WN *loop_info){
    return WN_CreateLabel (label_number, label_flag, loop_info);
}

WN       *B2W_create_goto     (INT32 label_number){
    return WN_CreateGoto (label_number);
}

WN       *B2W_create_return(void){
    return WN_CreateReturn();
}

WN       *B2W_create_return_val(OPERATOR opr, TYPE_ID rtype, TYPE_ID desc, WN *val){
    return WN_CreateReturn_Val (opr, rtype, desc, val);
}

WN       *B2W_create_compgoto(INT32 num_entries, WN *value,
                      WN *block, WN *deflt, INT32 last_label){
    return  WN_CreateCompgoto(num_entries, value,
                              block, deflt, last_label);
}

WN       *B2W_create_switch(INT32 num_entries, WN *value, WN *block, WN *deflt,
                            INT32 last_label){
    return  WN_CreateSwitch(num_entries, value, block, deflt, last_label);
}


/// ===========================================================================
/// B2W_get_ty_from_wn
/// \param wn_ptr (WN *)
/// \return TY_IDX
/// ===========================================================================
TY_IDX B2W_get_ty_from_wn  (WN * wn){
  Is_Valid(WN_operator(wn) == OPR_ILOAD
      || WN_operator(wn) == OPR_MLOAD
      || WN_operator(wn) == OPR_ILOADX
      || WN_operator(wn) == OPR_LDID
      || WN_operator(wn) == OPR_ICALL
      || WN_operator(wn) == OPR_ISTORE
      || WN_operator(wn) == OPR_MSTORE
      || WN_operator(wn) == OPR_ISTOREX
      || WN_operator(wn) == OPR_STID
      || WN_operator(wn) == OPR_TAS
      || WN_operator(wn) == OPR_IO_ITEM
      || WN_operator(wn) == OPR_LDA,
      ("[B2W_get_ty_from_wn] OPERATOR: %s Not a WN for OPR_ILOAD, OPR_MLOAD, OPR_ILOADX, OPR_LDID, OPR_ICALL, OPR_ISTORE,OPR_MSTORE,OPR_ISTOREX,OPR_STID,OPR_TAS, OPR_IO_ITEM, OPR_LDA", OPERATOR_name(WN_operator(wn))));

  TY_IDX ty = WN_ty(wn);
  Is_Valid(ty > 0, ("[B2W_get_ty_from_wn] invalid ty, please check how this wn is created"));

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_VERBOSE),
      (TFile, "[B2W_get_ty_from_wn] wn(0x%llx) -> ty = %u",
          (UINT64) wn, (UINT) ty));
  return ty;
}

/******************************************************************************
 * B2W_array
 * @return the address of array element
 *****************************************************************************/
WN* B2W_array(INT64 base, INT len, INT64 *dims, TY_IDX ty_idx)
{
    Is_Valid(len > 0 && len % 2 == 0, ("[B2W_array] dims error, the size must be odd"));
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE),
                (TFile, "[B2W_array] base = %lld, len = %d, dims = %lld", base, (INT) len, (INT64) dims));
    WN *base_wn = (WN*) base;
    WN *arr_wn = WN_Create(OPR_ARRAY, Pointer_Mtype, MTYPE_V, (mUINT16)(len + 1));
    WN_element_size(arr_wn) = TY_size(ty_idx);
    WN_kid(arr_wn, 0) = base_wn;
    INT part = len / 2;
    for (INT i = 0; i < part; ++i)
        WN_kid(arr_wn, i + 1) = (WN *) dims[i];
    for (INT i = part; i < len; ++i)
        WN_kid(arr_wn, i + 1) = (WN *) dims[i];
    return arr_wn;
}

/******************************************************************************
 * B2W_set_call_falgs
 *  set the wn call falg 
 *****************************************************************************/
void B2W_set_call_flags(WN *wn, int flag) {
  if(flag & WN_CALL_DOES_MEM_ALLOC) {
    WN_Set_Call_Does_Mem_Alloc(wn);
  }
  if(flag & WN_CALL_IS_INTERFACE) {
    WN_Set_Call_Is_Interface(wn);
  }
  if(flag & WN_CALL_IS_VIRTUAL) {
    WN_Set_Call_Is_Virtual(wn);
  }
}

/******************************************************************************
 * Class:     io_xcalibyte_BGenDriver
 * Method:    getStPtr
 * Signature: (J)J
 ******************************************************************************/
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_getStPtr
        (JNIEnv *, jclass, jlong st_idx){
    return (jlong) B2W_get_stptr((ST_IDX) st_idx);
}

/******************************************************************************
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniCreateBlock
 * Signature: ()J
 *****************************************************************************/
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniCreateBlock
  (JNIEnv *, jclass){
    return (jlong) B2W_create_block();
}

/******************************************************************************
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniMapTabCreate
 * Signature: (mempool * J)J
 *****************************************************************************/
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniMapTabCreate
            (JNIEnv *, jclass, jlong pool){
    return (jlong) B2W_map_tab_create((void *) pool);
}

/******************************************************************************
 * Class:     io_xcalibyte_BGenDriver
 * Method:    startFunction
 * Signature: (JJI)J
 ******************************************************************************/
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_startFunction
(JNIEnv *, jclass, jlong func_st, jlong num_args, jint scope, jint line){
    return (jlong) B2W_start_function(
        (ST_IDX) func_st, (UINT) num_args, (UINT) scope, line);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    finishFunction
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_finishFunction
  (JNIEnv *, jclass, jint line) {
    B2W_finish_function((UINT) line);
}


/******************************************************************************
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniPoolInitialize
 * Signature: ()J
 ******************************************************************************/
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniPoolInitialize
        (JNIEnv *, jclass){
    return (jlong) B2W_pool_initialize();
}

/******************************************************************************
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniNewScope
 * Signature: (J)J
 ******************************************************************************/
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniNewScope
        (JNIEnv *, jclass, jlong ptr){
    return (jlong) B2W_new_scope((MPPTR) ptr);
}

/******************************************************************************
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniSetLineNum
 * Signature: (JJ)V
 ******************************************************************************/
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_jniSetLineNum
        (JNIEnv *, jclass, jlong wn_ptr, jlong line){
    //if(line < 0) return;
    //Is_True(line >= 0, ("Line number for %#0x cannot be %lld ! \n", (fdump_tree(stderr, (WN*) wn_ptr), wn_ptr), line));
    B2W_set_line_num((UINT64) wn_ptr, (UINT) line);
}

/******************************************************************************
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniGetLineNum
 * Signature: (J)I
 ******************************************************************************/
JNIEXPORT jint JNICALL Java_io_xcalibyte_BGenDriver_jniGetLineNum
  (JNIEnv *, jclass, jlong wn_ptr)
{
    return (jint) B2W_get_line_num((WN*)wn_ptr);
}

/******************************************************************************
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniSetCurrentSrcFile
 * Signature: (Ljava/lang/String;I)V
 ******************************************************************************/
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_jniSetCurrentSrcFile
  (JNIEnv *env_, jclass, jstring name_, jint file_system_flag){
  CCHPTR name_utf8 = env_->GetStringUTFChars(name_, JNI_FALSE);
  Is_True(file_system_flag == FILE_SYSTEM_UNIX || file_system_flag == FILE_SYSTEM_WINDOWS,
          ("Unknown file_system flag = %d", file_system_flag));
  B2W_CONTEXT::Set_file(name_utf8, (FILE_SYSTEM) file_system_flag);
  env_->ReleaseStringUTFChars(name_, name_utf8);
  return;
}

/******************************************************************************
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniCreateIf
 * Signature: (JJJ)J
 ******************************************************************************/
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniCreateIf
  (JNIEnv *, jclass, jlong test, jlong then_block, jlong else_block){
    return (jlong) B2W_if((WN *) test, (WN *) then_block, (WN *) else_block);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniCreateTrueBr
 * Signature: (JJ)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniCreateTrueBr
  (JNIEnv *, jclass, jlong label_idx, jlong cmp_wn) {
  return (jlong) B2W_trueBr((int) label_idx, (WN *) cmp_wn);
}


/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniCreateFalseBr
 * Signature: (JJ)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniCreateFalseBr
  (JNIEnv *, jclass, jlong label_idx, jlong cmp_wn) {
  return (jlong) B2W_falseBr((int) label_idx, (WN *) cmp_wn);
}


/******************************************************************************
 * B2W_LDA
 * @return WN *
 *****************************************************************************/
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniLDA
  (JNIEnv *, jclass, jint desc, jlong offset, jlong sym)
{
    return (jlong )B2W_lda((TYPE_ID) desc, (WN_OFFSET)offset, (ST_IDX) sym);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniLDID
 * Signature: (IJJJ)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniLDID
  (JNIEnv *, jclass, jint desc, jlong offset, jlong sym, jlong ty){

    return (jlong )B2W_ldid((TYPE_ID) desc, (WN_OFFSET)offset, (ST_IDX) sym, (TY_IDX) ty, 0);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniLDIDField
 * Signature: (IJJJJ)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniLDIDField
  (JNIEnv *, jclass, jint desc, jlong offset, jlong sym, jlong ty, jlong field_id){
    return (jlong )B2W_ldid((TYPE_ID) desc, (WN_OFFSET)offset, (ST_IDX) sym, (TY_IDX) ty, (UINT) field_id);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniSTID
 * Signature: (IJJJJ)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniSTID
  (JNIEnv *, jclass, jint desc, jlong offset, jlong sym, jlong ty, jlong val){
    return (jlong )B2W_stid((TYPE_ID) desc, (WN_OFFSET)offset, (ST_IDX) sym, (TY_IDX) ty, (WN *) val,0);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniSTIDField
 * Signature: (IJJJJJ)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniSTIDField
  (JNIEnv *, jclass, jint desc, jlong offset, jlong sym, jlong ty, jlong val, jlong field_id){
  return (jlong )B2W_stid((TYPE_ID) desc, (WN_OFFSET)offset, (ST_IDX) sym, (TY_IDX) ty, (WN *) val,field_id);
}

/******************************************************************************
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniInsertBlockFirst
 * Signature: (JJ)V
 ******************************************************************************/
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_jniInsertBlockFirst
  (JNIEnv *, jclass, jlong block, jlong stmt){
  B2W_insert_block_first ((WN *) block, (WN *) stmt);
}

/******************************************************************************
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniInsertBlockLast
 * Signature: (JJ)V
 ******************************************************************************/
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_jniInsertBlockLast
  (JNIEnv *, jclass, jlong block, jlong stmt){
  B2W_insert_block_last ((WN *) block, (WN *) stmt);
}

/******************************************************************************
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniInsertBlockBefore
 * Signature: (JJJ)V
 ******************************************************************************/
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_jniInsertBlockBefore
  (JNIEnv *, jclass, jlong block, jlong stmt, jlong pos){
  B2W_insert_block_before ((WN *) block, (WN *) stmt, (WN *) pos);
}

/******************************************************************************
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniInsertBlockAfter
 * Signature: (JJJ)V
 ******************************************************************************/
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_jniInsertBlockAfter
  (JNIEnv *, jclass, jlong block, jlong stmt, jlong pos){
  B2W_insert_block_after ((WN *) block, (WN *) stmt, (WN *) pos);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniGetParentBlock
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniGetParentBlock
  (JNIEnv *, jclass, jlong stmt) {

  WN *parent = B2W_CONTEXT::Get_wn_parent((WN*)stmt, TRUE);
  if(Tracing() && B2W_LOG(B2W_LVL_EH)) {
    fprintf(TFile, " ----------parent(%#0llx) of stmt(%#0llx)--------------",
      (UINT64) parent, (UINT64) stmt);
    fdump_tree(TFile, parent);
    fflush(TFile);
  }
  return (jlong) parent;
}
/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    wnGetBodyFromEntry
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_getBodyFromEntry
(JNIEnv *, jclass, jlong entry){
  return (jlong) B2W_get_body_from_entry((WN *) entry);
}



/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    getTyFromWN
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_getTyFromWN
    (JNIEnv *, jclass, jlong wn){
    return (jlong) B2W_get_ty_from_wn((WN *) wn);
}

/*
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniCreateExp2
  (JNIEnv *, jclass, jint opr, jlong ret_mtype, jlong type_idx, jlong kid0, jlong kid1) {
    return (jlong) B2W_create_exp2((OPERATOR)opr, (UINT)ret_mtype, (UINT)type_idx, (WN *)kid0, (WN *)kid1);
}*/




/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniCreateUnary
 * Signature: (IJJ)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniCreateUnary
    (JNIEnv *, jclass, jint opr, jlong ret_mtype, jlong lhs){
    return (jlong) B2W_unary((OPERATOR) opr, (UINT) ret_mtype, (WN *) lhs);
}


/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniCreateBinary
 * Signature: (IJJJ)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniCreateBinary
    (JNIEnv *, jclass, jint opr, jlong ret_mtype, jlong lhs, jlong rhs){
    return (jlong) B2W_binary((OPERATOR) opr, (UINT) ret_mtype, (WN *) lhs,
                              (WN *) rhs);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniCreateTernary
 * Signature: (IJJJJ)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniCreateTernary
    (JNIEnv *, jclass, jint opr, jlong ret_mtype, jlong kid0, jlong kid1, jlong kid2){
    return (jlong) B2W_ternary((OPERATOR) opr, (UINT) ret_mtype, (WN *) kid0,
                               (WN *) kid1, (WN *) kid2);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniIntConst
 * Signature: (IJ)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniIntConst
    (JNIEnv *, jclass, jint mtype, jlong val){

    return (jlong) B2W_int_const((UINT) mtype, (UINT64) val);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniFloatConst
 * Signature: (IJ)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniFloatConst
    (JNIEnv *, jclass, jint mtype, jdouble val) {

    return (jlong) B2W_float_const((UINT) mtype, (double) val);

}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniStringConst
 * Signature: (Ljava/lang/String;)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniLDAString
  (JNIEnv *env, jclass, jstring name)
{
    const char *str = B2W_verify_ascii_string(env, name);
    jlong ret = (jlong) B2W_lda_string(str);
    env->ReleaseStringUTFChars(name, str);
    return ret;
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    createWhileDo
 * Signature: (JJ)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_createWhileDo
    (JNIEnv *, jclass, jlong test, jlong body){
    return (jlong) B2W_create_while_do((WN *) test, (WN *) body);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    createDoWhile
 * Signature: (JJ)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_createDoWhile
    (JNIEnv *, jclass, jlong test, jlong body){
    return (jlong) B2W_create_do_while((WN *)test, (WN *) body);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    createDO
 * Signature: (JJJJJJ)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_createDO
    (JNIEnv *, jclass, jlong index, jlong start, jlong end, jlong step, jlong body, jlong loop_info){
    return (jlong) B2W_create_do((WN *) index, (WN *) start, (WN *) end,
                                 (WN *) step, (WN *) body, (WN *) loop_info);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    funcICall
 * Signature: (JJIJ)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_funcICall
  (JNIEnv *, jclass, jlong rtype, jlong desc, jint n, jlong ty) {
    return (jlong) B2W_icall((TYPE_ID) rtype, (TYPE_ID) desc, n, (TY_IDX) ty);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    funcCall
 * Signature: (JJIJ)V
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_funcCall
    (JNIEnv *, jclass, jlong type, jlong desc, jint n, jlong s){
    return (jlong) B2W_call((TYPE_ID) type, (TYPE_ID) desc,  n, (ST_IDX) s);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    intTypeConversion
 * Signature: (JB)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_intTypeConversion
    (JNIEnv *, jclass, jlong wn, jbyte to_type){
    return (jlong) B2W_int_type_conversion((WN *) wn, (TYPE_ID) to_type);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    floatTypeConversion
 * Signature: (JB)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_floatTypeConversion
    (JNIEnv *, jclass, jlong wn, jbyte to_type){
    return (jlong) B2W_float_type_conversion((WN *) wn, (TYPE_ID) to_type);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    typeConversion
 * Signature: (JB)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_typeConversion
    (JNIEnv *, jclass, jlong wn, jbyte to_type){
    return (jlong) B2W_type_conversion((WN *) wn, (TYPE_ID) to_type);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniIlda
 * Signature: (IIIIJ)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniIlda
  (JNIEnv *, jclass, jint opr, jint rtype, jint desc, jint offset, jlong ty)
{
    return (long) B2W_ilda((OPERATOR) opr, (TYPE_ID) rtype, (TYPE_ID) desc, (WN_OFFSET) offset, (TY_IDX) ty);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniExtractBits
 * Signature: (IIJ)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniExtractBits
  (JNIEnv *, jclass, jint rtype, jint desc, jlong kid0)
{
    return (long) B2W_extract_bits((TYPE_ID) rtype, (TYPE_ID) desc, (WN*) kid0);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniParm
 * Signature: (IJJI)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniParm
  (JNIEnv *, jclass, jint rtype, jlong kid0, jlong ty, jint flag)
{
    return (long) B2W_parm((TYPE_ID) rtype, (WN*) kid0, (TY_IDX) ty, (UINT32) flag);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniAsmInput
 * Signature: (IJI)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniAsmInput
  (JNIEnv *env, jclass, jstring name, jint opnd_num, jlong kid0)
{
    const char *name_utf8 = B2W_verify_ascii_string(env, name);
    jlong ret = (jlong) B2W_asm_input(const_cast<char*>(name_utf8), (UINT32) opnd_num, (WN *) kid0);
    env->ReleaseStringUTFChars(name, name_utf8);
    return ret;
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniAlloca
 * Signature: (IJ)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniAlloca
  (JNIEnv *, jclass, jlong kid0)
{
   return (jlong) B2W_alloca ((WN *) kid0);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniComma
 * Signature: (IJJJ)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniComma
  (JNIEnv *, jclass, jint rtype, jint desc, jlong kid0, jlong kid1)
{
    return (jlong) B2W_comma((TYPE_ID) rtype, (TYPE_ID) desc, (WN*) kid0, (WN*) kid1);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniRcomma
 * Signature: (IJJJ)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniRcomma
  (JNIEnv *, jclass, jint rtype, jint desc, jlong kid0, jlong kid1)
{
    return (jlong) B2W_rcomma((TYPE_ID) rtype, (TYPE_ID) desc, (WN*) kid0, (WN*) kid1);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniCreateReturn
 * Signature: ()J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniCreateReturn
    (JNIEnv *, jclass){
    return (jlong) B2W_create_return();
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniCreateLabel
 * Signature: (IJJ)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniCreateLabel
    (JNIEnv *, jclass, jint label_number, jlong label_flag, jlong loop_info){
    return (jlong) B2W_create_label(label_number, (UINT32) label_flag, (WN *)loop_info);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniRelational
 * Signature: (IIJJ)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniRelational
    (JNIEnv *, jclass, jint opr, jint rtype, jlong lhs, jlong rhs){
    return (jlong) B2W_relational((OPERATOR) opr, (TYPE_ID) rtype, (WN *) lhs, (WN *) rhs);
}
/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniLabel
 * Signature: ()J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniLabel
  (JNIEnv *, jclass)
{
    LABEL_IDX label_idx;
    New_LABEL (CURRENT_SYMTAB, label_idx);
    WN *wn = WN_Create(OPC_LABEL, 0);
    WN_label_number(wn) = label_idx;
    return (jlong) wn;
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniCreateGoto
 * Signature: (JI)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniCreateGoto
  (JNIEnv *, jclass, jlong wn_label, jint linenum)
{
    WN *wn = WN_Create(OPC_GOTO,0);
    WN_label_number(wn) = WN_label_number((WN*) wn_label);
    B2W_set_line_num((UINT64)wn, linenum);
    return (jlong) wn;
}

JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_getLabelNum
  (JNIEnv *, jclass, jlong label_wn) {
      return (jlong) WN_label_number((WN *) label_wn);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniCreateSwitch
 * Signature: (IJJJJ)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniCreateSwitch
  (JNIEnv *, jclass, jint num_entries, jlong value, jlong block, jlong deflt, jlong last_label_number) {
      return (jlong) B2W_create_switch((INT32) num_entries, (WN *) value, (WN *) block, (WN *) deflt, (INT32) last_label_number);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniCreateCasegoto
 * Signature: (JJ)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniCreateCasegoto
  (JNIEnv *, jclass, jlong case_value, jlong case_label_number) {
      return (jlong) WN_CreateCasegoto((INT64) case_value, (INT32) case_label_number);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniCall
 * Signature: (IIIJ)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniCall
  (JNIEnv *, jclass, jint rtype, jint desc, jint n, jlong st_idx) {
      return (jlong) B2W_call((TYPE_ID) rtype, (TYPE_ID) desc, (INT32) n, (ST_IDX) st_idx);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniSetWNKid
 * Signature: (JIJ)V
 */
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_jniSetWNKid
  (JNIEnv *, jclass, jlong wn, jint idx, jlong kid)
{
    WN_kid((WN*) wn, idx) = (WN*) kid;
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniSetCallFlag
 * Signature: (JI)V
 */
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_jniSetCallFlag
  (JNIEnv *, jclass, jlong wn, jint flag) {
    B2W_set_call_flags((WN*)wn, (int)flag);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniCreateReturnVal
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniCreateReturnVal
  (JNIEnv *, jclass, jlong val)
{
    WN *val_wn = (WN*) val;
    return (jlong) WN_CreateReturn_Val(OPR_RETURN_VAL, WN_rtype(val_wn), MTYPE_V, val_wn);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniOpcodeMakeOp
 * Signature: (III)I
 */
JNIEXPORT jint JNICALL Java_io_xcalibyte_BGenDriver_jniOpcodeMakeOp
  (JNIEnv *, jclass, jint opr, jint mtype, jint desc) {
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE),
        (TFile, "[Java_io_xcalibyte_BGenDriver_jniOpcodeMakeOp] opr = %d, mtype = %d, desc = %d\n",
            opr, mtype, desc));
    return (jint) OPCODE_make_op((OPERATOR) opr, (TYPE_ID) mtype, (TYPE_ID) desc);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniCreateIntrinsic
 * Signature: (III[J)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniCreateIntrinsic
  (JNIEnv *env_, jclass, jint opc, jint iopc, jint kid_num, jlongArray kids) {
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE),
        (TFile, "[Java_io_xcalibyte_BGenDriver_jniCreateIntrinsic] opc = %d, iopc = %d, kid_num = %d\n",
            opc, iopc, kid_num));
    WN *ikids[kid_num];
    jboolean isCopy = TRUE;
    jlong *kid_idx = env_->GetLongArrayElements(kids, &isCopy);
    for(int i = 0; i< kid_num; i++) {
      ikids[i] = (WN *)kid_idx[i];
    }
    env_->ReleaseLongArrayElements(kids, kid_idx, 0);
    return (jlong) WN_Create_Intrinsic ((OPCODE) opc, (INTRINSIC) iopc, (INT) kid_num, ikids);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniArray
 * Signature: (J[J)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniArray
  (JNIEnv *env, jclass, jlong base, jlongArray dims, jlong ty_idx)
{
    UINT     len    = env->GetArrayLength(dims);
    INT64   *arr    = new INT64[len];
    jlong   *body   = env->GetLongArrayElements(dims, 0);
    for(UINT i = 0; i < len; i++){
      arr[i] = body[i];
    }
    jlong    retval = (jlong) B2W_array((INT64) base, (INT) len, arr, (TY_IDX) ty_idx);
    delete[] arr;
    return   retval;
}
/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniIStore
 * Signature: (IJJJJJ)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniIStore
  (JNIEnv *, jclass, jint desc, jlong offset, jlong ty_idx, jlong addr_wn, jlong value, jlong field_id)
{
    return (jlong) B2W_istore((TYPE_ID) desc, (WN_OFFSET) offset, (TY_IDX) ty_idx, (WN*) addr_wn, (WN*) value, (FLD_IDX) field_id);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniILoad
 * Signature: (IJJJJ)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniILoad
  (JNIEnv *, jclass, jint desc, jlong offset, jlong ty_idx, jlong addr_wn, jlong field_id) {
    return (jlong) B2W_Iload((TYPE_ID) desc, (WN_OFFSET) offset, (TY_IDX) ty_idx, (WN *) addr_wn, (UINT) field_id);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniCreateRegion
 * Signature: (IJJJI)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniCreateRegion
  (JNIEnv *, jclass, jint region_kind, jlong body_blk, jlong pragma_blk, jlong exit_blk, jint inito_idx) {
  return (jlong) B2W_Create_Region((REGION_KIND)region_kind, (WN *) body_blk, (WN *) pragma_blk, (WN *) exit_blk, (INITO_IDX) inito_idx);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    printWN
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_printWN
  (JNIEnv *, jclass, jlong wn) {
  fdump_tree(TFile, (WN*)wn);
  fflush(TFile);
}
