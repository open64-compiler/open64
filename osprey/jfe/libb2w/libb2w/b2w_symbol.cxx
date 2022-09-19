/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

#include "b2w_common.h"
#include "b2w_handler.h"
#include <string>
#include <unordered_map>


#define SOOT_LOCAL_PREFIX ".stack"
/*============================================================================
 * B2W_create_var_symbol
 * creating a var symbol for
 * @param name      var symbol's name
 * @param typeIdx   var's typeIdx, primitive/struct/array allowed
 * @param sclass    storage class, see the enum ST_SCLASS
 * @param eclass    export level, see the enum ST_EXPORT
 * @param level     lexical level (global var = 1, local val/parameter = 2)
 * @return ST_IDX   created symbol_idx
 *===========================================================================*/
ST_IDX 	B2W_create_var_symbol(const char * name, INT64 typeIdx, INT32 sclass,
			      INT32 eclass, INT32 level){

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE),
   (TFile, "[B2W_create_var_symbol] name = %s, "
    "typeidx = %lld, sclass = %d, eclass = %d, "
    "level = %d ", name, typeIdx, sclass, eclass, level));

  B2W_get_typtr((TY_IDX) typeIdx);

  TY_KIND kind = TY_kind((TY_IDX) typeIdx);
  Is_Valid(kind == KIND_ARRAY || kind == KIND_SCALAR || kind == KIND_VOID ||
    kind == KIND_POINTER || kind == KIND_STRUCT,
    ("[B2W_create_var_symbol] wrong typeIdx : 0x%x, "
     "only void/scalar/pointer/struct/array allowed", typeIdx));

  if(eclass == EXPORT_LOCAL) {
    if(level != CURRENT_SYMTAB && CURRENT_SYMTAB == 2){
      Is_Valid(FALSE, ("[ERROR][B2W_create_var_symbol] e=local, "
                      "level must be 2\n"));
    }
  if (sclass != SCLASS_AUTO && SCLASS_FORMAL != sclass && sclass != SCLASS_EH_REGION_SUPP) {
    Is_Valid(FALSE, ("[ERROR][B2W_create_var_symbol] e=local, "
                    "sclass must be 2 or 1\n"));
    }
  } else if(eclass == EXPORT_PREEMPTIBLE || eclass == EXPORT_PROTECTED){
    Is_Valid(level == GLOBAL_SYMTAB, ("[ERROR][B2W_create_var_symbol] e=global, "
        "level must be 1\n"));
  } else {
    Is_Valid(FALSE , ("[ERROR][B2W_create_var_symbol] wrong eclass"));
  }

  TY_IDX  idx  = (TY_IDX) typeIdx;
  ST *st = New_ST ((SYMTAB_IDX) level);

  ST_Init(st, Save_Str (name), CLASS_VAR,
          (ST_SCLASS) sclass,
          (ST_EXPORT) eclass, idx);

  if(sclass ==  SCLASS_DGLOBAL)
    Set_ST_is_initialized (st);

  enum ST_TLS_MODEL tls_model = TLS_NONE;
  Set_ST_tls_model(st, tls_model);

  /* the line number is meaningless
  SRCPOS srcpos;
  SRCPOS_clear(srcpos);
  
  SRCPOS_filenum(srcpos) = B2W_CONTEXT::Get_file_num();
  SRCPOS_linenum(srcpos) = 1;
  
  Set_ST_Srcpos (*st, srcpos);
  */
  ST_IDX result = ST_st_idx((const ST *) st);

  if(name != NULL && strncmp(name, SOOT_LOCAL_PREFIX, strlen(SOOT_LOCAL_PREFIX)) == 0) {
    Set_ST_is_temp_var(st);
  }

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE), (TFile, " -> %u \n",  result));

  return result;
}

/*=============================================================================
 * get the symbol for a mtype's preg (already created during intialization)
 * @param mtype
 * @return
 =============================================================================*/
ST_IDX B2W_create_preg_st(INT32 mtype) {
  Is_Valid((mtype >= Int_Preg_Min_Offset &&
           mtype < Last_Dedicated_Preg_Offset),
          ("mtype not in valid range"));
  ST *st = MTYPE_To_PREG(mtype);
  ST_IDX result = ST_st_idx(st);

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE),
              (TFile, "[B2W_create_preg_st] entry : mtype =%d , st_idx =%d", mtype, result));
  return result;
}

//=============================================================================
// B2W_create_preg
// create a preg to be used for return_val / parms / middle vars
//=============================================================================
PREG_NUM B2W_create_preg(INT32 mtype, const char *name) {
  Is_Valid((mtype >= Int_Preg_Min_Offset &&
           mtype < Last_Dedicated_Preg_Offset),
          ("mtype not in valid range"));
  PREG_NUM preg = Create_Preg(mtype, name);
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE),
              (TFile, "[B2W_create_preg] entry : mtype =%d , preg =%d", mtype, preg));
  return preg;
}

//=============================================================================
// B2W_create_method_symbol
//=============================================================================
ST_IDX 	B2W_create_method_symbol(const char * name, INT64 func_type_idx,
				 INT32 sclass, INT32 eclass, INT32 level){
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE),
           (TFile, "[B2W_create_method_symbol] <%s>, ", name));

  TY      *ty_ptr = B2W_get_typtr(func_type_idx);

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE),
           (TFile, "ty = %#x, sclass = %d eclass = %d level = %d\n" ,
             (UINT) func_type_idx, sclass, eclass, level));

  Is_Valid((ST_SCLASS) sclass == SCLASS_EXTERN || (ST_SCLASS) sclass == SCLASS_TEXT,
          ("*** [B2W_create_method_symbol] bad SCLASS : %d", sclass));

  Is_Valid((ST_EXPORT) eclass == EXPORT_PREEMPTIBLE || (ST_EXPORT) eclass == EXPORT_LOCAL,
          ("*** [B2W_create_method_symbol] bad ECLASS : %d", sclass));

  Is_Valid(level == 1 || level == 2 /*&& Scope_tab[2].st_tab != NULL*/,
  ("*** [B2W_create_method_symbol] bad level, "
   "require 1 or 2, given : %d", level));

  B2W_get_typtr((TY_IDX) func_type_idx);

  Is_Valid(func_type_idx != (INT64) NULL &&
      TY_kind((TY_IDX) func_type_idx) == KIND_FUNCTION,
  ("*** [B2W_create_method_symbol] "
   "bad func_type_idx : %#x", (UINT) func_type_idx));

  TY_IDX idx 	= (TY_IDX) func_type_idx;
  ST     *st   	= New_ST (level);
  PU_IDX  pu_idx  = 0;
  PU     *pu 	= &(New_PU (pu_idx));
  TYLIST *tylist	= NULLPTR;

  PU_Init (*pu, idx, level + 1);

  // TODO: Set_PU_is_constructor (pu);
  // TODO: Set_ST_is_pure_vfunc (st);

  if(sclass == SCLASS_TEXT) {

    ST_Init (st, Save_Str (name), CLASS_FUNC,
             (ST_SCLASS) SCLASS_EXTERN,
             (ST_EXPORT) eclass, TY_IDX (pu_idx));

    Set_ST_sclass(st, SCLASS_TEXT);

  }else{
    ST_Init (st, Save_Str (name), CLASS_FUNC,
             (ST_SCLASS) sclass,
             (ST_EXPORT) eclass, TY_IDX (pu_idx));
  }

  //Set_PU_lexical_level(Pu_Table[ST_pu(st)], CURRENT_SYMTAB);
  Set_PU_cxx_lang(Pu_Table[ST_pu(st)]);

  //Set_PU_java_lang(Pu_Table[ST_pu(st)]);

  /**
   * case GS_VISIBILITY_DEFAULT:
    export_class = EXPORT_PREEMPTIBLE;
    break;
  case GS_VISIBILITY_PROTECTED:
    export_class = EXPORT_PROTECTED;
    break;
  case GS_VISIBILITY_HIDDEN:
    export_class = EXPORT_HIDDEN;
    break;
  case GS_VISIBILITY_INTERNAL:
    export_class = EXPORT_INTERNAL;
    break;
   * */

  ST_IDX result      = ST_st_idx((const ST *) st);

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE), (TFile, "[B2W_create_method_symbol] result : <%s> %u \n",  name, result));

  return result;
}

//=============================================================================
// B2W_create_extern_function_symbol
//=============================================================================
ST_IDX 	B2W_create_extern_function_symbol(const char * name, INT64 ty_idx)
{
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE), (TFile,
      "[B2W_create_extern_function_symbol] entry : name = %s, ty_idx = %lld :",
                         name, ty_idx));
    ST     *st     	= New_ST(GLOBAL_SYMTAB);
    PU_IDX  pu_idx  = 0;
    PU     *pu 	    = &(New_PU (pu_idx));
    PU_Init (*pu, (TY_IDX) ty_idx, GLOBAL_SYMTAB + 1);

    ST_Init (st, Save_Str (name), CLASS_FUNC, SCLASS_EXTERN, EXPORT_PREEMPTIBLE, TY_IDX(pu_idx));
    ST_IDX result   = ST_st_idx((const ST *) st);
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE), (TFile, ": -> %u \n",  result));
    return result;
}

/*=============================================================================
 * Binding params to a function's symbol
 * @param args      vector<ST_IDX> for function's parameters' symbols
 * @param func_st   function's symbol
 * @param entry     entry WHIRL node
 =============================================================================*/
void  B2W_bind_func_params(U32VEC &args, ST_IDX func_st, WN *entry){

  INT    i            = 0;
  ST    *st_temp      = NULLPTR;

  B2W_get_stptr(func_st);

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_DEBUG), (TFile,
    "[B2W_bind_func_params]  entry: %llu, %s(%u) \n", (UINT64) entry,
    ST_name(func_st), func_st));

  // Create the fake first param.
  if (TY_return_in_mem(ST_pu_type(func_st))) {
    st_temp          = New_ST ();
    ST_Init (st_temp, Save_Str2i(".arg", "", 0), CLASS_VAR, SCLASS_FORMAL,
             EXPORT_LOCAL, Make_Pointer_Type(ST_pu_type(st_temp), FALSE));
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_DEBUG), (TFile, ".arg_%d", i));
    Set_ST_is_value_parm(st_temp);
    WN_kid(entry, i) = WN_CreateIdname ( 0, ST_st_idx(st_temp) );
    ++i;
  }

  U32VECI args_iter = args.begin();

  for(; args_iter  != args.end(); args_iter++){
    ST * stx        = B2W_get_stptr(*args_iter);
    Set_ST_sclass(stx, SCLASS_FORMAL);
    // Set_ST_promote_parm (stx);    // causing demotion error
    WN * wnkid      = WN_CreateIdname ( 0, ST_st_idx(stx) );
    WN_kid(entry,i) = wnkid;
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_DEBUG), (TFile,
      "[B2W_bind_func_params]  parm::%s, wnkid:%llu \n", ST_name(stx), (UINT64) wnkid));
    ++i;
  }

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_DEBUG), (TFile, "\n"));
}


/*=============================================================================
 * set vtable st for class type and set class type for vtable st
 ============================================================================*/
void B2W_set_vtable_relation (ST_IDX vtable_sym, TY_IDX class_ty_idx) {
  ST *st = &St_Table[vtable_sym];
  TY *class_ty = &Ty_Table[class_ty_idx];
  Set_ST_is_vtable(st);
  Set_ST_vtable_ty_idx(st, class_ty_idx);
  class_ty->Set_vtable(vtable_sym);
}

/*=============================================================================
 * set class symbol st for class type and set class type for class symbol
 ============================================================================*/
void B2W_set_class_symbol (ST_IDX vtable_sym, TY_IDX class_ty_idx) {
  ST *st = &St_Table[vtable_sym];
  TY *class_ty = &Ty_Table[class_ty_idx];
  Set_ST_is_class_symbol(st);
  Set_ST_vtable_ty_idx(st, class_ty_idx);
}


/*=============================================================================
 * set class type's copy constructor function symbol
 ============================================================================*/
void B2W_set_class_copy_ctor(TY_IDX class_ty_idx, ST_IDX copy_ctor_sym) {
  TY *class_ty = &Ty_Table[class_ty_idx];
  class_ty->Set_copy_constructor(copy_ctor_sym);
}

/*=============================================================================
 * Get ty_idx from symbol's idx
 * @param st_idx
 * @return TY_IDX
 ============================================================================*/
TY_IDX   B2W_get_ty_from_st(ST_IDX st_idx){
  B2W_get_stptr(st_idx);
  TY_IDX ty = ST_type(st_idx);
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_ALL_EXEC),
           (TFile, "[B2W_get_ty_from_st] st(%u) -> ty = %u\n",
               (UINT) st_idx, (UINT) ty));
  return ty;
}

/*=============================================================================
 * ST_IDX -> ST *
 ============================================================================*/
ST * B2W_get_stptr(ST_IDX st_idx){

  SYMTAB_IDX  st_level = Extract_level8(st_idx, (SYMTAB_IDX *) 0);
  UINT        st_index = Extract_index24(st_idx);

  Is_Valid(st_level == 1 || st_level == 2,
    ("[B2W_get_st_ptr] symbol idx (symtab_level) not 1 or 2, "
     "but level:%d, index:%u, ST_IDX :%u \n", st_level, st_index, st_idx));

  Is_Valid(ST_Table_Size(st_level) > st_index,
    ("[B2W_get_st_ptr] symbol's index exceed it's level's symtab's size,"
     "st_level:%u, st_index:%u, level_size:%u, ST_IDX:%u \n",
     st_level, st_index, ST_Table_Size(st_level), st_idx));

	ST         *st       = &St_Table[st_idx];

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_ALL_EXEC),
    (TFile, "[B2W_get_stptr] st: %u, ptr: %llu\n",
      st_idx, (UINT64) st));
	return 	st;
}

/*=============================================================================
 * ST_IDX initialize a class st.
 ============================================================================*/
void B2W_initialize_class_symbol(ST_IDX st) {

    ST      *st_ptr     = B2W_get_stptr(st);
    if(!ST_is_initialized(st_ptr)){
      Set_ST_is_initialized(st_ptr);
    }
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE),
      (TFile, "[B2W_initilize_class_symbol] st : %u \n", st));
}

/*============================================================================*
 * B2W_initialize_set_st
 * set symbol to become DGLOBAL, and initialized
 *============================================================================*/
void B2W_initialize_set_st(ST * st) {
  if (ST_sclass(st) == SCLASS_UGLOBAL ||
      ST_sclass(st) == SCLASS_EXTERN  ||
      ST_sclass(st) == SCLASS_COMMON) {
    Set_ST_sclass(*st, SCLASS_DGLOBAL);
  }

  if(!ST_is_initialized(st)) {
    Set_ST_is_initialized(st);
  }
}

/*============================================================================*
 * Set Symbol's SrcPosition
 * @param INT64 st_idx
 * @param INT64 line_in_current_file,  if need to change file,
 *              use B2W_CONTEXT::Set_file
 *============================================================================*/
void B2W_set_st_srcpos(INT64 idx, INT64 line_in_current_file) {
  Is_Valid(B2W_CONTEXT::Get_file_num() >= 1, ("Must have at least one file to set position in."));
  SRCPOS srcpos;
  SRCPOS_clear(srcpos);
  SRCPOS_filenum(srcpos) = B2W_CONTEXT::Get_file_num();
  SRCPOS_linenum(srcpos) = line_in_current_file;
  Set_ST_Srcpos(*B2W_get_stptr(idx), srcpos);
}

/*============================================================================*
 *
 *============================================================================*/
void B2W_set_misc_flags(INT64 idx, INT64 flag_kind, INT64 flag) {
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_VERBOSE), (TFile, "[B2W_set_misc_flags] setting idx<%lld>, kind<%lld> => val<%lld>",
    idx, flag_kind, flag));
  switch(flag_kind){
    case B2W_FLAG_KIND_STFLAG:
      B2W_get_stptr((ST_IDX) idx)->flags |= flag;
      break;

    case B2W_FLAG_KIND_CLEAR_STFLAG:
      B2W_get_stptr((ST_IDX) idx)->flags &= ~flag;
      break;

    case B2W_FLAG_KIND_ST_EXT_FLAG:
      B2W_get_stptr((ST_IDX) idx)->flags_ext |= flag;
      break;

    case B2W_FLAG_KIND_CLEAR_ST_EXT_FLAG:
      B2W_get_stptr((ST_IDX) idx)->flags_ext &= ~flag;
      break;

    case B2W_FLAG_KIND_SCLASS:
      Set_ST_sclass(B2W_get_stptr((ST_IDX) idx), (ST_SCLASS) flag);
      break;

    case B2W_FLAG_KIND_ST_TY:
      Set_ST_type(B2W_get_stptr((ST_IDX) idx), (TY_IDX) flag);
      break;

    case B2W_FLAG_KIND_STB:
      Set_STB_flags(B2W_get_stptr((TY_IDX) idx), (ST_SCLASS) flag);
      break;

    case B2W_FLAG_KIND_EXPORT:
      Set_ST_export(B2W_get_stptr((ST_IDX) idx), (ST_EXPORT) flag);
      break;

    case B2W_FLAG_KIND_TY_FLAGS:
      Set_TY_flags(*B2W_get_typtr((TY_IDX) idx), (UINT16) flag);
      break;

    case B2W_FLAG_KIND_SYM_SRCPOS:
      B2W_set_st_srcpos(idx, flag);
      break;

    default:
        Is_Valid(FALSE, ("[B2W_set_symbol_flag] cannot determine flag_kind:<%d>, flag<%d> \n", flag_kind, flag));
  }
}

/*============================================================================*
 * B2W_aggregate_int
 * initializing a initv with int
 *============================================================================*/
void B2W_aggregate_int(INITO_IDX _inito, INT64 val, INT size, BOOL is_root) {

  TYPE_ID     mtype        = 0;
  INITV_IDX   inv          = New_INITV();

  if (size == 1) mtype = MTYPE_I1;
  else if (size == 2) mtype = MTYPE_I2;
  else if (size == 4) mtype = MTYPE_I4;
  else if (size == 8) mtype = MTYPE_I8;
  else {
    Is_Valid(FALSE, ("[B2W_aggregate_int] cannot determine size(not 1,2,4,8),"
                    " inito(%d), size = %u, val: %u \n", _inito, size, val));
    return;
  }

  INITV_Init_Integer(inv, mtype, val, 1);
  if (B2W_CONTEXT::Get_last_initv() != 0) {
    Set_INITV_next(B2W_CONTEXT::Get_last_initv(), inv);
  } else if (is_root) {
    Set_INITO_val(_inito, inv);
  }
  B2W_CONTEXT::Set_last_initv(inv);
}

/*============================================================================*
 * B2W_aggregate_int
 * initializing a initv with int
 *============================================================================*/
void B2W_aggregate_uint(INITO_IDX _inito, UINT64 val, INT size, BOOL is_root) {

  TYPE_ID     mtype        = 0;
  INITV_IDX   inv          = New_INITV();

  if (size == 1) mtype = MTYPE_U1;
  else if (size == 2) mtype = MTYPE_U2;
  else if (size == 4) mtype = MTYPE_U4;
  else if (size == 8) mtype = MTYPE_U8;
  else {
      Is_Valid(FALSE, ("[B2W_aggregate_uint] cannot determine size(not 1,2,4,8),"
                      " inito(%d), size = %u, val: %u \n", _inito, size, val));
    return;
  }

  INITV_Init_Integer(inv, mtype, val, 1);
  if (B2W_CONTEXT::Get_last_initv() != 0) {
    Set_INITV_next(B2W_CONTEXT::Get_last_initv(), inv);
  } else if(is_root) {
    Set_INITO_val(_inito, inv);
  }
  B2W_CONTEXT::Set_last_initv(inv);
}

/*============================================================================*
 * B2W_aggregate_tcon_int
 * initializing a initv with tcon
 *============================================================================*/
void B2W_aggregate_tcon_int(INITO_IDX _inito, INT64 val, INT size, BOOL is_root) {

  TYPE_ID     mtype        = 0;
  INITV_IDX   inv          = New_INITV();

  if (size == 1) mtype = MTYPE_U1;
  else if (size == 2) mtype = MTYPE_U2;
  else if (size == 4) mtype = MTYPE_U4;
  else if (size == 8) mtype = MTYPE_U8;
  else {
    Is_Valid(FALSE, ("[B2W_aggregate_tcon_int] cannot determine size(not 1,2,4,8),"
                     " inito(%d), size = %u, val: %u \n", _inito, size, val));
    return;
  }

  TCON_IDX tcon = Enter_tcon (Host_To_Targ (mtype, val));
  INITV_Set_VAL (Initv_Table[inv], tcon, 1);
  if (B2W_CONTEXT::Get_last_initv() != 0) {
    Set_INITV_next(B2W_CONTEXT::Get_last_initv(), inv);
  } else if(is_root) {
    Set_INITO_val(_inito, inv);
  }
  B2W_CONTEXT::Set_last_initv(inv);
}

/*============================================================================*
 * B2W_aggregate_symbol
 * initializing initv with the address to a symbol
 *============================================================================*/
void B2W_aggregate_symbol (INITO_IDX _inito, ST *st, WN_OFFSET ofst, BOOL is_root) {

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE),
    (TFile, "[B2W_aggragate_symbol] inito:%u, st_ptr:%llu, ofst:%u\n",
                          _inito, (UINT64) st, ofst));

  if (_inito == 0)     return;

  INITV_IDX inv      = New_INITV();
  INITV_Init_Symoff (inv, st, ofst);

  if (st)  Set_ST_addr_saved (st);

  if (B2W_CONTEXT::Get_last_initv() != 0)
    Set_INITV_next(B2W_CONTEXT::Get_last_initv(), inv);
  else if (is_root)
    Set_INITO_val(_inito, inv);

  B2W_CONTEXT::Set_last_initv(inv);
}

/*============================================================================*
 * B2W_aggregate_address
 * initializing address for a st to bind to initv
 *============================================================================*/
void B2W_aggregate_address(INITO_IDX _inito, ST *st, INT ofst, BOOL is_root) {
  B2W_aggregate_symbol(_inito, st, ofst, is_root);
  if(TRUE)  Set_ST_initv_in_other_st (st);
}

/*============================================================================*
 * B2W_aggregate_block
 * initializing block (require)
 * 1. if is_root and last_initv is zero, bind initv to intio
 * 2. if last_initv not zero, set the new block to last_initv's next.
 * 3. if last_initv is  zero, and not_root, do nothing (for second dimension block)
 *============================================================================*/
void B2W_aggregate_block(INITO_IDX _inito, BOOL is_root) {
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_VERBOSE),
    (TFile, "[B2W_aggragate_block] -- inito:%u\n ", _inito));
  if (_inito == 0) return;
  INITV_IDX inv_blk = New_INITV();
  if (B2W_CONTEXT::Get_last_initv() != 0) {
    Set_INITV_next(B2W_CONTEXT::Get_last_initv(), inv_blk);
  } else if(is_root) {
    Set_INITO_val(_inito, inv_blk);
  }
  INITV_Init_Block(inv_blk, INITV_Next_Idx());
  B2W_CONTEXT::Set_last_initv(inv_blk);
}

/*============================================================================*
 * B2W_aggregate_pad
 * initializing padding
 *============================================================================*/
void B2W_aggregate_pad(INITO_IDX _inito, UINT64 size, BOOL is_root) {
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_VERBOSE),
              (TFile, "[B2W_aggregate_pad] -- inito: %u, size:%llu, is_root:%d \n",
                      _inito, size, (INT) is_root));

  if (_inito == 0)     return;
  INITV_IDX    inv     = New_INITV();

  INITV_Init_Pad(inv, size);

  if (B2W_CONTEXT::Get_last_initv() != 0)
    Set_INITV_next(B2W_CONTEXT::Get_last_initv(), inv);

  else if (is_root)
    Set_INITO_val(_inito, inv);

  B2W_CONTEXT::Set_last_initv(inv);
}

/*============================================================================*
 * B2W_aggregate_real
 * initializing a real num(float, double), internal use only
 *============================================================================*/
void B2W_aggregate_real(INITO_IDX _inito, FLOAT8 fval, UINT size, BOOL is_root) {

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_VERBOSE),
    (TFile, "[B2W_aggragate_real] -- inito: %u, fval:%lf, size:%u, is_root:%d \n",
      _inito, fval, size, (INT) is_root));

  INITV_IDX      inv           = New_INITV();
  TCON           tc;

  if (_inito == 0) return;

  switch (size) {
    case 4:
      tc = Host_To_Targ_Float_4 (MTYPE_F4, (float) fval);
      break;
    case 8:
      tc = Host_To_Targ_Float (MTYPE_F8, fval);
      break;
      // TODO: Qfloat (Qfloat (F16) also supported as valid initializable
    default:
      Is_Valid(FALSE, ("[B2W_aggregate_real] unexpected size\n"));
      // In case in release mode:
      return;
  }

  INITV_Set_VAL (Initv_Table[inv], Enter_tcon(tc), 1);
  if (B2W_CONTEXT::Get_last_initv() != 0)
    Set_INITV_next(B2W_CONTEXT::Get_last_initv(), inv);
  else if (is_root)
    Set_INITO_val(_inito, inv);
  B2W_CONTEXT::Set_last_initv(inv);
}

/*============================================================================*
 * B2W_aggregate_string
 * initializing a string, internal use only
 *============================================================================*/
void B2W_aggregate_string(INITO_IDX _inito, CCHPTR str, INT size, BOOL is_root)
{

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_VERBOSE),
              (TFile, "[B2W_aggregate_string] -- inito: %u, str:%s, size:%u, is_root:%d \n",
                _inito, str, size, (INT) is_root));

  if (_inito == 0)      return;
  INITV_IDX  inv       = New_INITV();
  INITV_Init_String (inv, (CHPTR) str, size);

  if (B2W_CONTEXT::Get_last_initv() != 0)
    Set_INITV_next(B2W_CONTEXT::Get_last_initv(), inv);
  else if (is_root)
    Set_INITO_val(_inito, inv);

  B2W_CONTEXT::Set_last_initv(inv);

}

/*============================================================================*
 * B2W_aggregate_label
 * initializing a label, internal use only, external use see B2w_initialize_with_array
 *============================================================================*/
void B2W_aggregate_label(INITO_IDX _inito, LABEL_IDX label, UINT32 repeat,
                         UINT32 mtype, UINT32 flags, BOOL is_root)
{
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_VERBOSE),
              (TFile, "[B2W_aggregate_label] -- inito: %u, label:%u, repeat:%u, is_root:%d \n",
                _inito, label, repeat, (INT) is_root));

  if (_inito == 0)      return;
  INITV_IDX  inv       = New_INITV();
  INITV_Init_Label (inv, label, repeat, flags, mtype);

  if (B2W_CONTEXT::Get_last_initv() != 0)
    Set_INITV_next(B2W_CONTEXT::Get_last_initv(), inv);
  else if (is_root)
    Set_INITO_val(_inito, inv);

  B2W_CONTEXT::Set_last_initv(inv);

}

/*============================================================================*
 * B2W_aggregate_array
 * initializing the array
 *============================================================================*/
void B2W_aggregate_array(INITO_IDX _inito, ST *st, U32VEC &stList, UINT64 *ofst,
                         BOOL is_root) {

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_VERBOSE),
    (TFile, "[B2W_aggregate_array] -- inito: %u, size:%lu, ofst:%llu, is_root:%d \n",
      _inito, stList.size(), *ofst, (INT) is_root));

  U32VECI it  = stList.begin();
  for(; it != stList.end(); it++){
    *ofst   +=   8;
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_VERBOSE), (TFile, "[B2W_aggregate_array] one_sym: %s(%d) \n",ST_name(*st), *it));
    if(is_root && it == stList.begin()){
      B2W_aggregate_address(_inito, B2W_get_stptr(*it), (INT) *ofst, TRUE);   //Add FUNC_ADDR
    }else{
      B2W_aggregate_address(_inito, B2W_get_stptr(*it), (INT) *ofst, FALSE);   //Add FUNC_ADDR
    }
  }
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_VERBOSE), (TFile, "\n\n"));
}

/*============================================================================*
 * B2W_aggregate_construct
 * creating construct for ST
 *============================================================================*/
void B2W_aggregate_construct(ST * st, TY_IDX class_ty, U32VEC &st_list, BOOL is_root){

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE), (TFile,
    "[B2W_aggregate_construct] st: %llu, class_ty: %u \n", (UINT64) st, class_ty));

  INITO_IDX     _inito    = New_INITO(st);
  TY_IDX ty               = ST_type(st);
  TY_IDX ety              = TY_etype(ty);
  UINT esize              = (UINT) TY_size(ety);

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_DEBUG)
    , (TFile, "[B2W_aggregate_construct] ty:%u, ety:<%s>(%u), esize:%u \n",
    ty, TY_name(ety), ety, esize));

  INITV_IDX last_initv_save;

  B2W_initialize_set_st(st);

  B2W_aggregate_block(_inito, is_root);
  last_initv_save = B2W_CONTEXT::Get_last_initv();
  B2W_CONTEXT::Set_last_initv(0);

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_VERBOSE),
    (TFile, "[B2W_aggregate_construct] after initv set. \n"));

  UINT64 ofst = 0;
  B2W_aggregate_int(_inito, 0, 8, FALSE);    //Add NOP
  ofst += 8;
  UINT64 pad = 0;

  if (esize == 0) {
    Is_Valid(FALSE,("[B2W_aggregate_construct] having a vtable with 0 size elements.\n"));
    return;
  } else {
    B2W_aggregate_array(_inito, st, st_list, &ofst, FALSE);
  }

  pad = TY_size(ty) - ofst;
  if (pad > 0) {
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_DATA),
      (TFile, "[B2W_aggregate_construct] padding occured, ty_size: %llu, "
              "ofst:%llu \n", TY_size(ty), ofst));

    Is_Valid(FALSE, ("[B2W_aggregate_pad] Unexpected Padding in array occured, "
                    "please check whether TY_size(ty) == num * TY_size(etype), "
                    "diff = %u\n", pad));

    B2W_aggregate_pad(_inito, pad, FALSE);
  }

  B2W_CONTEXT::Set_last_initv(last_initv_save);

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_VERBOSE),
    (TFile, "[B2W_aggregate_construct] finished \n"));

}

void B2W_initialize_symbol_fail(ST_IDX symbol, I64VEC &value) {
  Is_Trace_Ex(Tracing(), (stderr, "[Symbol]\n"));
  B2W_fail_symbol(symbol, 0);
  Is_Trace_Ex(Tracing(), (stderr, "\n[Type]\n"));
  B2W_fail_type(ST_type(symbol), 0);
  Is_Trace_Ex(Tracing(), (stderr, "\n[Init-Array]\n("));
  for(INT i = 0; i < value.size() ; i++){
    Is_Trace_Ex(Tracing(), (stderr, "%llu%s", value[i], (i == value.size()-1)?")":","));
  }
  Is_Trace_Ex(Tracing(), (stderr, "\n"));
}

/***
 * Use an array to create INITO for symbol
 * array should be :
 *
 * For Class / Array:
 * MASK
 * START_BLOCK
 * ...
 * MASK
 * END_BLOCK
 *
 *
 * for int:
 * INT_CST_MASK
 * 12345 (int data)
 *
 * for vtable:
 * MASK
 * START_BLOCK
 * ...
 * SYM_MASK
 * (foo) (function's st_idx)
 * ...
 * MASK
 * END_BLOCK
 *
 * @param value
 * @param symbol
 */
INITO_IDX      B2W_initialize_with_array(I64VEC &value, ST_IDX symbol) {
  ST           *st        = B2W_get_stptr(symbol);
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE),
              (TFile, "[B2W_initialize_with_array] entry, symbol: <%s>%u, "
                      "vec->size: <%ld> \n", ST_name(symbol), symbol, value.size()));
  B2W_CONTEXT::Set_last_initv(0);
  Set_ST_is_const_var (st);

  INITO_IDX     _inito    = New_INITO(symbol);
  INT           pos       = 0;
  INT64         first,    second;
  UINT          i_size    = 0;
  UINT          r_size    = TY_size(ST_type(symbol));
  U32VEC        stack_save_last_initv;
  BOOL          is_root   = TRUE;
  INT           ofst_sym  = 0;

  Is_Valid(r_size > 0, ("[B2W_initialize_with_array] cannot initialize because symbol(%s<%d>), type(%s<%d>) has size == 0",
    (B2W_initialize_symbol_fail(symbol, value), ST_name(symbol)), symbol, TY_name(ST_type(symbol)), ST_type(symbol), r_size));

  Is_Valid(value.size() % 2 == 0,
     ("[B2W_initialize_with_array] wrongly formatted array, MASK-DATA not paired, size=%d \n",
     (B2W_initialize_symbol_fail(symbol, value), value.size())));

  for(pos = 0; pos < value.size() ; pos += 2){
    first                 = value[pos];
    second                = value[pos + 1];

    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_VERBOSE),
                (TFile, "[B2W_initialize_with_array] pos : %d,  first : %llu, "
                        "second: %llu \n", pos, first, second));

    if(first == B2W_INIT_MASK) {
      if (second == B2W_INIT_START_BLK) {
        B2W_initialize_set_st(st);
        B2W_aggregate_block(_inito, is_root);
        stack_save_last_initv.push_back(B2W_CONTEXT::Get_last_initv());
        B2W_CONTEXT::Set_last_initv(0);
      } else if (second == B2W_INIT_END_BLK) {
          Is_Valid(stack_save_last_initv.size() > 0,
                  ("%s", (B2W_initialize_symbol_fail(symbol, value),
                    "[B2W_initialize_with_array] end block without previous start block")));
        B2W_CONTEXT::Set_last_initv((INITV_IDX) stack_save_last_initv[stack_save_last_initv.size() - 1]);
        stack_save_last_initv.pop_back();
      }
    }else if(first == B2W_INIT_OFST_MASK) {
      ofst_sym = (INT) second;
    }else if(first == B2W_INIT_TCON_U4_MASK) {
      i_size          += 4;
      B2W_aggregate_tcon_int(_inito, second, 4, is_root);
    } else if(first == B2W_INIT_TCON_U8_MASK) {
      i_size          += 8;
      B2W_aggregate_tcon_int(_inito, second, 8, is_root);
    } else if(first == B2W_INIT_LABEL_MASK){
      i_size          += 4;
      LABEL_IDX lab =  (LABEL_IDX) second;
      Set_LABEL_addr_saved(lab);
      B2W_aggregate_label(_inito, lab, 1, 0, 0, is_root);
    }else if(first == B2W_INIT_SYM_MASK){
      i_size          += B2W_CONTEXT::Get_target_ptr_size();
      ST_IDX st_idx   =  (ST_IDX) second;
      ST    *tempst   = B2W_get_stptr(st_idx);
      Set_ST_addr_saved(*tempst);
      Set_ST_initv_in_other_st(tempst);
      B2W_aggregate_symbol(_inito, tempst, ofst_sym, is_root);
      ofst_sym        = 0;
    }else if(first == B2W_INIT_I1_CST_MASK){
      i_size          += 1;
      B2W_aggregate_int(_inito, (INT64) second, 1, is_root);
    }else if(first == B2W_INIT_I2_CST_MASK){
      i_size          += 2;
      B2W_aggregate_int(_inito, (INT64) second, 2, is_root);
    }else if(first == B2W_INIT_I4_CST_MASK){
      i_size          += 4;
      B2W_aggregate_int(_inito, (INT64) second, 4, is_root);
    }else if(first == B2W_INIT_I8_CST_MASK){
      i_size          += 8;
      B2W_aggregate_int(_inito, (INT64) second, 8, is_root);
    }else if(first == B2W_INIT_U1_CST_MASK){
      i_size          += 1;
      B2W_aggregate_uint(_inito, (UINT64) second, 1, is_root);
    }else if(first == B2W_INIT_U2_CST_MASK){
      i_size          += 2;
      B2W_aggregate_uint(_inito, (UINT64) second, 2, is_root);
    }else if(first == B2W_INIT_U4_CST_MASK){
      i_size          += 4;
      B2W_aggregate_uint(_inito, (UINT64) second, 4, is_root);
    }else if(first == B2W_INIT_U8_CST_MASK){
      i_size          += 8;
      B2W_aggregate_uint(_inito, (UINT64) second, 8, is_root);
    }else if(first == B2W_INIT_F4_CST_MASK){
      i_size          += 4;
      B2W_INIT_DATA m;
      m.uintData      = second;
      B2W_aggregate_real(_inito, m.floatData, 4, is_root);
    }else if(first == B2W_INIT_F8_CST_MASK) {
      i_size          += 8;
      B2W_INIT_DATA m;
      m.uintData = second;
      B2W_aggregate_real(_inito, m.doubleData, 8, is_root);
    }else if(first == B2W_INIT_STR_CST_MASK) {
      Is_Valid(B2W_CONTEXT::Get_temporaries(second) != NULLPTR,
                  ("[B2W_initialize_with_array] string constant not in temporaries table "
                   "(did not register as CHPTR), temps[%llu]",
                   (B2W_initialize_symbol_fail(symbol, value), second)));
      CHU64P *str  = (CHU64P *) B2W_CONTEXT::Get_temporaries(second);
      Is_Valid(str->second > 0 && str->second < INT_MAX, ("[B2W_initialize_with_array] string constant length error:%d\n", str->second));
      INT size = (INT) str->second;
      i_size += size;
      B2W_aggregate_string(_inito, (CCHPTR) str->first, size, is_root);
      // init string need to append 0, but not calculated into length
      // i_size -= 1;
    }else if(first == B2W_INIT_PAD_MASK){
      i_size += second;
      B2W_aggregate_pad(_inito, (UINT64) second, is_root);
    }else{
      Is_Valid(FALSE, ("[B2W_initialize_with_array] cannot interpret MASK <= %d \n",
        (B2W_initialize_symbol_fail(symbol, value), first)));
    }
    is_root           = FALSE;
  }

  if(r_size != i_size){
    Is_True (FALSE, ("[B2W_initialize_with_array] size mismatch : (actual) %d != %d "
             "(required) of [symbol(%s<%d>), type(%s<%d>)]",
    (B2W_initialize_symbol_fail(symbol, value), i_size), r_size,
    ST_name(symbol), symbol, TY_name(ST_type(symbol)), ST_type(symbol)));
  }

  return _inito;
}

/*=============================================================================
 * Class:     io_xcalibyte_BGenDriver
 * Method:    createVarSymbol
 * Signature: (Ljava/lang/String;JIII)J
/============================================================================*/
jlong JNICALL Java_io_xcalibyte_BGenDriver_createVarSymbol
(JNIEnv * env, jclass class_, jstring name, jlong typeidx, jint sclass,
		jint eclass, jint level){
  const char *name_utf8 = B2W_verify_ascii_string(env, name);
  UINT32 result = B2W_create_var_symbol(name_utf8, typeidx, sclass,
              eclass, level);
        env->ReleaseStringUTFChars(name, name_utf8);
  return (jlong)result;
}

/*=============================================================================
 * Class:     io_xcalibyte_BGenDriver
 * Method:    createPregSymbol
 * Signature: (I)J
/============================================================================*/
jlong JNICALL Java_io_xcalibyte_BGenDriver_createPregSymbol
    (JNIEnv * env, jclass class_, jint mtype) {
  UINT32 result = B2W_create_preg_st(mtype);
  return result;
}

/*=============================================================================
 * Class:     io_xcalibyte_BGenDriver
 * Method:    createPreg
 * Signature: (ILjava/lang/String;)J
/============================================================================*/
jlong JNICALL Java_io_xcalibyte_BGenDriver_createPreg
    (JNIEnv * env, jclass class_, jint mtype, jstring name){
  INT32 result;
  if(name) {
    const char *name_utf8 = B2W_verify_ascii_string(env, name);
    result = B2W_create_preg(mtype, name_utf8);
          env->ReleaseStringUTFChars(name, name_utf8);
  }
  else {
    result = B2W_create_preg(mtype, NULL);
  }
  return (jlong)result;
}

/*=============================================================================
 * Class:     io_xcalibyte_BGenDriver
 * Method:    createFunctionSymbol
 * Signature: (Ljava/lang/String;IIIIJI)J
 ============================================================================*/
jlong JNICALL Java_io_xcalibyte_BGenDriver_createFunctionSymbol
(JNIEnv * env, jclass class_, jstring name, jlong typeidx, jint sclass,
	jint eclass, jint level){

  const char *name_utf8 = B2W_verify_ascii_string(env, name);
  UINT32 result = B2W_create_method_symbol(name_utf8, typeidx, sclass, eclass, level);
  env->ReleaseStringUTFChars(name, name_utf8);
  return (jlong)result;

}

/*=============================================================================
 * Class:     io_xcalibyte_BGenDriver
 * Method:    createExternFunctionSymbol
 * Signature: (Ljava/lang/String;J)J
 *============================================================================*/
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_createExternFunctionSymbol
  (JNIEnv *env, jclass, jstring name, jlong ty_idx)
{
  const char *name_utf8 = B2W_verify_ascii_string(env, name);
  UINT32 result = B2W_create_extern_function_symbol(name_utf8, ty_idx);
  env->ReleaseStringUTFChars(name, name_utf8);
  return (jlong)result;
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    setVTable
 * Signature: (JJ)V
 */
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_setVTable
  (JNIEnv *, jclass, jlong vtable_sym, jlong class_ty) {
  B2W_set_vtable_relation((ST_IDX)vtable_sym, (TY_IDX) class_ty);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    setClassSymbol
 * Signature: (JJ)V
 */
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_setClassSymbol
  (JNIEnv *, jclass, jlong class_sym, jlong class_ty) {
  B2W_set_class_symbol((ST_IDX) class_sym, (TY_IDX) class_ty);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    setClassCopyCtor
 * Signature: (JJ)V
 */
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_setClassCopyCtor
  (JNIEnv *, jclass, jlong class_ty, jlong copy_ctor_sym) {
  B2W_set_class_copy_ctor((TY_IDX)class_ty, (ST_IDX) copy_ctor_sym);
}

/*============================================================================*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    getTyFromST
 * Signature: (J)J
 *============================================================================*/
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_getTyFromST
    (JNIEnv *, jclass, jlong st_idx){
  return (jlong) B2W_get_ty_from_st((UINT) st_idx);
}

/*============================================================================*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    getReturnValPreg
 * Signature: ()J
 *============================================================================*/
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_getReturnValPreg
  (JNIEnv *, jclass)
{
  return (jlong) ST_st_idx(Return_Val_Preg);
}

/*============================================================================*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    bindFunctionParams
 * Signature: ([JJJ)V
 *============================================================================*/
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_bindFunctionParams
  (JNIEnv * env_, jclass, jlongArray args, jlong func, jlong entry){

  jlong   *st_id_arr      = NULLPTR;
  jboolean isCopy         = TRUE;
  jint     array_length   = 0;
  U32VEC   args_vec;
  st_id_arr               = env_->GetLongArrayElements(args, &isCopy);
  array_length            = env_->GetArrayLength(args);

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_VERBOSE),
    (TFile, "%s%d%s\n","[Java_io_xcalibyte_BGenDriver_bindFunctionParams]"
   " args : [ ", array_length, "]"));

  for(int i = 0; i < array_length; i++){
    args_vec.push_back((UINT32) st_id_arr[i]);
  }

  env_->ReleaseLongArrayElements(args, st_id_arr, 0);
  return B2W_bind_func_params(args_vec, (ST_IDX) func, (WN *) entry);
}

/*============================================================================*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    initializeWithArray
 * Signature: ([JJ)V
 *============================================================================*/
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_initializeWithArray
  (JNIEnv *env_, jclass, jlongArray arr, jlong st_idx){
  jlong   *st_id_arr      = NULLPTR;
  jboolean isCopy         = TRUE;
  jint     array_length   = 0;
  I64VEC   maskedvec;
  st_id_arr               = env_->GetLongArrayElements(arr, &isCopy);
  array_length            = env_->GetArrayLength(arr);

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_DATA), (TFile, "%s%d%s\n",
    "[Java_io_xcalibyte_BGenDriver_initializeWithArray], "
    " args : [ ", array_length, "]"));

  for(int i = 0; i < array_length; i++){
    maskedvec.push_back((INT64) st_id_arr[i]);
  }

  env_->ReleaseLongArrayElements(arr,  st_id_arr, 0);
  return (jlong) B2W_initialize_with_array(maskedvec, (ST_IDX)   st_idx);

}

/*=============================================================================
 * Class:     io_xcalibyte_BGenDriver
 * Method:    setMiscFlags
 * Signature: (JJJ)V
 ============================================================================*/
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_setMiscFlags
  (JNIEnv *, jclass, jlong idx, jlong flag_kind, jlong flag){
  B2W_set_misc_flags(idx, flag_kind, flag);
}


/*============================================================================*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    setPuLangCXX
 * Signature: (J)V
 ============================================================================**/
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_setPuLangCXX
  (JNIEnv *, jclass, jlong func_sym){
  B2W_set_pu_cxx_lang(func_sym);
}

/*============================================================================*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    setPuLangC
 * Signature: (J)V
 ============================================================================**/
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_setPuLangC
  (JNIEnv *, jclass, jlong func_sym){
  B2W_set_pu_c_lang(func_sym);
}

/*============================================================================*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    setPuLangJava
 * Signature: (J)V
 ============================================================================**/
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_setPuLangJava
  (JNIEnv *, jclass, jlong func_sym){
  B2W_set_pu_java_lang(func_sym);
}

/*============================================================================*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    setPuConstructor
 * Signature: (JJ)V
 ============================================================================**/
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_setPuConstructor
  (JNIEnv *, jclass, jlong func_sym, jlong class_ty){
  B2W_set_pu_constructor(func_sym);
}

/*============================================================================*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    setPuNoReturn
 * Signature: (J)V
 ============================================================================**/
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_setPuNoReturn
  (JNIEnv *, jclass, jlong func_sym) {
  B2W_set_pu_no_return(func_sym);
}

/*============================================================================*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    setCurrPuRbc
 * Signature: ()V
 ============================================================================**/
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_setCurrPuRbc
  (JNIEnv *, jclass){
  B2W_set_curr_pu_rbc();
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    setPuIsMainPu
 * Signature: (J)V
 */
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_setPuIsMainPu
  (JNIEnv *, jclass, jlong func_sym) {
  B2W_set_pu_is_mainpu(func_sym);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    clearPuIsInlie
 * Signature: (J)V
 */
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_clearPuIsInlie
  (JNIEnv *, jclass, jlong func_sym) {
  B2W_clear_pu_is_inline(func_sym);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    setPuNoInline
 * Signature: (J)V
 */
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_setPuNoInline
  (JNIEnv *, jclass, jlong func_sym) {
  B2W_set_pu_no_inline(func_sym);
}

/*============================================================================*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    saveAsTempByteArray
 * Signature: ([B)J
 *============================================================================*/
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_saveAsTempByteArray
  (JNIEnv *env_, jclass, jbyteArray arr) {
  jbyte   *data_array = NULLPTR;
  jboolean isCopy     = TRUE;
  data_array          = env_->GetByteArrayElements(arr, &isCopy);
  INT32 array_length  = env_->GetArrayLength(arr);
  CHPTR  temp_arr         = (CHPTR) malloc(array_length);
  Is_Valid(temp_arr != NULLPTR, ("[BGenDriver_saveAsTempByteArray] Memory Allocation failure, str is NULLPTR"));
  Is_Valid(array_length >= 0 && array_length < INT_MAX,
           ("[BGenDriver_saveAsTempByteArray] Utf-8 String length out of bound : %llu", array_length));
  INT            cursor       = 0;
  memcpy(temp_arr, data_array, array_length);
  CHU64P        *datapair     = new CHU64P(temp_arr, array_length);
  UINT64         temp_id      = B2W_CONTEXT::Set_temporaries((void *) datapair);
  if(B2W_LOG(B2W_LVL_VERBOSE)) {
    for (INT64 i = 0; i < array_length; i++) {
      Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE), (TFile, "%d,", temp_arr[i]));
    }
  }
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE), (TFile, "\n"));
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE), (TFile, "[BGenDriver_saveAsTempByteArray] saved : len<%d>\n", array_length));
  return (jlong) temp_id;
}

/*============================================================================*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    saveAsTempString
 * Signature: (Ljava/lang/String;)J
 *============================================================================*/
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_saveAsTempString
    (JNIEnv *env, jclass, jstring str_to_save){
  CCHPTR         name_utf8    = B2W_verify_utf8_string(env, str_to_save);
  UINT64         size_of_str  = (UINT64) env->GetStringUTFLength(str_to_save);
  CHPTR          str          = (CHPTR) malloc(size_of_str + 1);
  Is_Valid(str != NULLPTR, ("[BGenDriver_saveAsTempString] Memory Allocation failure, str is NULLPTR"));
  Is_Valid(size_of_str >= 0 && size_of_str < INT_MAX,
    ("[BGenDriver_saveAsTempString] Utf-8 String length out of bound : %llu", size_of_str));
  INT            cursor       = 0;
  for(UINT i = 0; i < size_of_str; i++){
    str[cursor++] = name_utf8[i];
  }
  str[cursor] = 0;          //Pending 0;
  env->ReleaseStringUTFChars(str_to_save, name_utf8);

  CHU64P        *datapair     = new CHU64P(str, cursor + 1);
  UINT64         temp_id      = B2W_CONTEXT::Set_temporaries((void *) datapair);
  if(B2W_LOG(B2W_LVL_VERBOSE)) {
    for (INT64 i = 0; i < size_of_str; i++) {
      Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE), (TFile, "%d,", str[i]));
    }
  }
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE), (TFile, "\n"));
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE), (TFile, "[BGenDriver_saveAsTempString] saved : len<%llu>\n", size_of_str));
  return (jlong) temp_id;
}


/*============================================================================*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    printSymbolInfo
 * Signature: (J)V
 *============================================================================*/
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_printSymbolInfo
  (JNIEnv *, jclass, jlong st_idx){
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_VERBOSE), (TFile, "[BGenDriver_printSymbolInfo] st_idx:%u \n", (ST_IDX) st_idx));
  B2W_get_stptr((ST_IDX) st_idx)->Print(TFile);
  fflush(TFile);
}

/*============================================================================*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    getStringUTF8Length
 * Signature: (Ljava/lang/String;)I
 *============================================================================*/
JNIEXPORT jint JNICALL Java_io_xcalibyte_BGenDriver_getStringUTF8Length
  (JNIEnv * env, jclass, jstring str){
  UINT64         size_of_str  = (UINT64) env->GetStringUTFLength(str);
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE), (TFile, "[GetStringUTFLength] len<%u> \n", (ST_IDX) size_of_str));
  return (jint) size_of_str;
}


/*=============================================================================
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniNewInito
 * Signature: (J)J
 =============================================================================*/
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniNewInito
  (JNIEnv *, jclass, jlong sym_idx) {
  INITO_IDX inito_ = New_INITO(sym_idx);
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE), (TFile, "[jniNewInito] sym<%u> \n", (ST_IDX) sym_idx));
  return (jlong) inito_;
}
