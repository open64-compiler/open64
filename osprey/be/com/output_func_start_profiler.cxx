/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it would be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * Further, this software is distributed without any warranty that it is
 * free of the rightful claim of any third person regarding infringement 
 * or the like.  Any license provided herein, whether implied or 
 * otherwise, applies only to this software file.  Patent licenses, if 
 * any, provided herein do not apply to combinations of this program with 
 * other software, or any other product whatsoever.  
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write the Free Software Foundation, Inc., 59
 * Temple Place - Suite 330, Boston MA 02111-1307, USA.
 */


#include "output_func_start_profiler.h"
#include "sections.h"
#include "stblock.h"

#define ROUNDUP(val,align)       ( (-(INT64)align) & (INT64)(val+align-1) )
static ST* Get_Section_ST(SECTION_IDX sec, UINT align, ST_SCLASS sclass);

extern WN * Gen_Call( const char *name, TYPE_ID rtype = MTYPE_V );
extern WN * Gen_Call( const char *name, WN *arg1, TYPE_ID rtype = MTYPE_V);
extern WN * Gen_Call( const char *name, WN *arg1, WN *arg2, TYPE_ID rtype = MTYPE_V  );
extern WN * Gen_Call( const char *name, WN *arg1, WN *arg2, WN *arg3, TYPE_ID rtype= MTYPE_V);
extern WN * Gen_Call( const char *name, WN *arg1, WN *arg2, WN *arg3, WN *arg4, TYPE_ID rtype = MTYPE_V);
extern WN * Gen_Call( const char *name, WN *arg1, WN *arg2, WN *arg3, WN *arg4, WN *arg5, TYPE_ID rtype = MTYPE_V );
extern WN * Gen_Call( const char *name, WN *arg1, WN *arg2, WN *arg3, WN *arg4, WN *arg5, WN *arg6, TYPE_ID rtype = MTYPE_V );


const char* OUTPUT_FUNC_START_PROFILER::_prefix = "_GLOBAL__GCOV_";
#ifdef GCC_303
const char* OUTPUT_FUNC_START_PROFILER::_init_proc = "__gcov_init";
#else
const char* OUTPUT_FUNC_START_PROFILER::_init_proc = "__bb_init_func";
#endif
const char* OUTPUT_FUNC_START_PROFILER::_lpbx_0 = "LPBX0";

OUTPUT_FUNC_START_PROFILER Output_Func_Start_Profiler;

OUTPUT_FUNC_START_PROFILER::OUTPUT_FUNC_START_PROFILER(const char* src_file_name, PU_Info** pu_tree_p)
    : _file_name(src_file_name), 
      _pu_tree_p(pu_tree_p),
      _func_st(NULL),
      _func_entry(NULL),
      _func_body(NULL),
      _pu_info(NULL), 
      _func_level(0), 
      _func_name(NULL) {}

char*
OUTPUT_FUNC_START_PROFILER::Construct_Func_Name(const char *name)
{
    char* func_name = TYPE_MEM_POOL_ALLOC_N(char, &_mem_pool, strlen(name) + strlen(_file_name) + 1);
    sprintf(func_name, "%s%s", name, _file_name);
    for(INT i = 0; func_name[i]; i++){
        if( !(    func_name[i] == '_' 
              || ('0' <= func_name[i] && func_name[i] <= '9') 
              || ('A' <= func_name[i] && func_name[i] <= 'Z') 
              || ('a' <= func_name[i] && func_name[i] <= 'z')) ) 
            func_name[i] = '_';
    }
    return func_name;
}

void 
OUTPUT_FUNC_START_PROFILER::Generate_Func_Start_Profiler_PU(void)
{
    if(*_pu_tree_p == NULL)
        return; 

    // create new type for global naming function
    TY_IDX func_ty_idx;
    TY& func_ty = New_TY(func_ty_idx);
    TY_Init(func_ty, 0, KIND_FUNCTION, MTYPE_UNKNOWN, STR_IDX_ZERO);
 
    // create return type and parm list 
    TYLIST_IDX parm_idx;
    TYLIST& parm_list = New_TYLIST(parm_idx);
    Set_TY_tylist(func_ty, parm_idx);
    Set_TYLIST_type(parm_list, Be_Type_Tbl(MTYPE_V));  // return type
    Set_TYLIST_type(New_TYLIST(parm_idx),  MTYPE_To_TY(MTYPE_V));  // end of parm list

    // generate a name for global naming function 
    _func_name = Construct_Func_Name(_prefix);

    // creat a new PU 
    PU_IDX pu_idx;
    PU& new_pu = New_PU(pu_idx);
    PU_Init(new_pu, func_ty_idx, GLOBAL_SYMTAB + 1);

    Set_PU_no_inline(new_pu);

    // creat a new ST for this PU 
    _func_st = New_ST(GLOBAL_SYMTAB); 
    ST_Init(_func_st, 
            Save_Str(_func_name),
            CLASS_FUNC,
            SCLASS_TEXT,
            EXPORT_INTERNAL,
            pu_idx);  
    Allocate_Object(_func_st);

    // creat local symbol table for global naming function
    New_Scope(GLOBAL_SYMTAB + 1, &_mem_pool, TRUE);    
    _func_level = CURRENT_SYMTAB;
    Scope_tab[_func_level].st = _func_st;
    Set_PU_lexical_level(new_pu, _func_level);

    // create WHIRL tree 
    _func_body = WN_CreateBlock();
    _func_entry = WN_CreateEntry(0, _func_st, 
                                    _func_body, 
                                    WN_CreateBlock(),
                                    WN_CreateBlock());
  
    // create PU_Info for this function
    _pu_info = CXX_NEW(PU_Info, &_mem_pool);
    PU_Info_init(_pu_info);

    Set_PU_Info_tree_ptr(_pu_info, _func_entry);
    Set_PU_Info_state(_pu_info, WT_SYMTAB, Subsect_InMem);
    Set_PU_Info_state(_pu_info, WT_TREE, Subsect_InMem);
    Set_PU_Info_state(_pu_info, WT_PROC_SYM, Subsect_InMem);
    Set_PU_Info_flags(_pu_info, PU_IS_COMPILER_GENERATED | PU_IS_PROFILER);

    // set the symbol idx
    PU_Info_proc_sym(_pu_info) = ST_st_idx(_func_st);
    
    // create a wn map table
    PU_Info_maptab(_pu_info) = WN_MAP_TAB_Create(&_mem_pool);

    PU_Info_pu_dst (_pu_info) = DST_INVALID_IDX;
    PU_Info_cu_dst (_pu_info) = DST_INVALID_IDX;    

    // save local symbol table
    Set_PU_Info_symtab_ptr(_pu_info, NULL);
    Save_Local_Symtab(_func_level, _pu_info);

    // set language type. *_pu_tree_p point to the first pu.
    if(PU_c_lang(PU_Info_pu(*_pu_tree_p)))
        Set_PU_c_lang(new_pu);
    if(PU_cxx_lang(PU_Info_pu(*_pu_tree_p)))
        Set_PU_cxx_lang(new_pu);
    if(PU_f77_lang(PU_Info_pu(*_pu_tree_p)))
        Set_PU_f77_lang(new_pu);
    if(PU_f90_lang(PU_Info_pu(*_pu_tree_p)))
        Set_PU_f90_lang(new_pu);
/*
    PU_Info* tmp_pu_tree = *_pu_tree_p;
    while (PU_Info_next(tmp_pu_tree))
        tmp_pu_tree = PU_Info_next(tmp_pu_tree);

    PU_Info_next(tmp_pu_tree) = _pu_info;
    PU_Info_next(_pu_info) = 0;
*/
    PU_Info_next(_pu_info) = *_pu_tree_p;
    *_pu_tree_p = _pu_info;

    /* ctor_section_asm_out_constructor */ 
    TY_IDX func_ptr_ty_idx = Make_Pointer_Type(func_ty_idx);
    ST* _ctor_st = New_ST(GLOBAL_SYMTAB);
    ST_Init(_ctor_st, Save_Str(Construct_Func_Name("__ctors_")),
            CLASS_VAR, SCLASS_FSTATIC, EXPORT_LOCAL, func_ptr_ty_idx);
    Set_ST_is_initialized(_ctor_st);
    Clear_ST_gprel(_ctor_st);
    //Allocate_Object(_ctor_st); 

    ST *newblk = NULL;
    ST *st;
    INT i;
    FOREACH_SYMBOL (GLOBAL_SYMTAB,st,i) {
      if (ST_class(st) != CLASS_BLOCK) continue;
      if (STB_section_idx(st) == _SEC_DATA && strcmp(ST_name(st), ".ctors") ==0){
        newblk = st;
        break;
      }
    }
    if (newblk == NULL) {
      ST *blk = Get_Section_ST(_SEC_DATA, 0, SCLASS_UNKNOWN);
      newblk = Copy_ST_Block(blk);
//Bug# 1238
      Set_STB_size(newblk, 0);
      Set_ST_name_idx(newblk, Save_Str(".ctors"));
    }
    Set_ST_base(_ctor_st, newblk);

    INT64 old_offset;
    INT64 size = 8;
    old_offset = STB_size(newblk);
    Set_ST_ofst(_ctor_st, ROUNDUP(old_offset, 8));
    Set_STB_size(newblk, ROUNDUP(ST_ofst(_ctor_st) + size, 8));

    INITO_IDX inito_ctor = New_INITO(_ctor_st);
    INITV_IDX initv_ctor;
    initv_ctor = New_INITV();
    INITV_Init_Symoff(initv_ctor, _func_st, 0);
    Append_INITV(initv_ctor, inito_ctor, INITV_IDX_ZERO);

    return;
}


void 
OUTPUT_FUNC_START_PROFILER::Fill_In_Func_Body(void)
{

    // create new type for gcov_init function
    TY_IDX func_ty_idx;
    TY& func_ty = New_TY(func_ty_idx);
    TY_Init(func_ty, 0, KIND_FUNCTION, MTYPE_UNKNOWN, TY_IDX_ZERO);
 
    // create return type and parm list 
    TYLIST_IDX parm_idx;
    TYLIST& parm_list = New_TYLIST(parm_idx);
    Set_TY_tylist(func_ty, parm_idx);
    Set_TYLIST_type(parm_list, Be_Type_Tbl(MTYPE_V));  // return type
    Set_TYLIST_type(New_TYLIST(parm_idx),  MTYPE_To_TY(MTYPE_V));  // end of parm list

    // creat a new PU 
    PU_IDX pu_idx;
    PU& new_pu = New_PU(pu_idx);
    PU_Init(new_pu, func_ty_idx, GLOBAL_SYMTAB + 1);

    Set_PU_no_inline(new_pu);

    // creat a new ST for this PU 
    _func_st = New_ST(GLOBAL_SYMTAB); 
    ST_Init(_func_st, 
            Save_Str(_init_proc),
            CLASS_FUNC,
            SCLASS_TEXT,
            EXPORT_PROTECTED,
            pu_idx);  
#ifdef GCC_303
    UINT32 magic_size = 8;
    UINT32 zero_word_size = 8;
    UINT32 filename_size = 8;
    UINT32 counts_size = 8;
    UINT32 ncounts_size = 8;
    UINT32 next_size = 8;
    UINT32 sizeofbb_size = 8;
    UINT32 bb_function_info_size = 8;
    UINT32 size = zero_word_size + filename_size + counts_size + ncounts_size + next_size + sizeofbb_size + bb_function_info_size;
#else
    UINT32 size;
#if defined(TARG_X8664) || defined(TARG_LOONGSON)
    if (Is_Target_32bit())
      size = 7*4;
    else
#endif // TARG_X8664
      size = 7*8;
#endif

    TY_IDX tyi;
    TY& ty = New_TY(tyi);
    TY_Init(ty, size, KIND_STRUCT, MTYPE_M,
          STR_IDX_ZERO);
#if defined(TARG_X8664) || defined(TARG_LOONGSON)
    Set_TY_align(tyi, Is_Target_32bit()? 4 : 32);
#else
    Set_TY_align(tyi, 32);
#endif
    _lpbx_st = New_ST(GLOBAL_SYMTAB);
    ST_Init(_lpbx_st, Save_Str(Construct_Func_Name(_lpbx_0)),
            CLASS_VAR, SCLASS_PSTATIC, EXPORT_PREEMPTIBLE, tyi);
    Set_ST_is_initialized(_lpbx_st);
    Set_ST_is_not_used(_lpbx_st);
#if defined(TARG_X8664) || defined(TARG_LOONGSON)
    WN* parm_var_addr =
      WN_CreateLda (OPCODE_make_op(OPR_LDA, Pointer_type, MTYPE_V),
                    0,
                    Make_Pointer_Type(Is_Target_32bit() ?
                                        MTYPE_To_TY(MTYPE_I4) :
                                        MTYPE_To_TY(MTYPE_I8),
                                      FALSE),
                    _lpbx_st);
#else
    WN* parm_var_addr =
      WN_CreateLda (OPCODE_make_op(OPR_LDA, Pointer_type, MTYPE_V),
                    0,
                    Make_Pointer_Type(MTYPE_I8, FALSE),
                    _lpbx_st);
#endif
    WN* call_stmt;
    call_stmt = Gen_Call(_init_proc,
                            parm_var_addr
                                 );
    WN_INSERT_BlockBefore(_func_body, NULL, call_stmt);

    WN* ret_stmt = WN_CreateReturn();
    WN_INSERT_BlockBefore(_func_body, NULL, ret_stmt);
    
    return;
}

static ST*
Get_Section_ST(SECTION_IDX sec, UINT align, ST_SCLASS sclass)
{
  if (SEC_block(sec) == NULL) {
    ST *new_blk = New_ST_Block (Save_Str(SEC_name(sec)),
        TRUE/*is_global*/, sclass, align, 0);
    Set_STB_section_idx(new_blk, sec);
    SEC_block(sec) = new_blk;
    Set_STB_section(new_blk);
    Set_STB_root_base(new_blk);
    if (SEC_is_gprel(sec)) {
        Set_STB_is_basereg(new_blk);
        Set_ST_gprel(new_blk);
    }
    if (SEC_is_merge(sec))
        Set_STB_merge(new_blk);
    if (SEC_is_exec(sec))
        Set_STB_exec(new_blk);
    if (SEC_is_nobits(sec))
        Set_STB_nobits(new_blk);
    Enter_ST(new_blk);
  }
  return SEC_block(sec);
}

