/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

/****************************************************************************
 **   
 **  File: B2w_handler.h
 **  A common decl for handlers inside B2w.
 ** 
 ** Call Graph:
 ** Java -> JNI -> Handler(*) -> WHIRL API
 *****************************************************************************/

#ifndef OSPREY_B2W_HANDLER_H_
#define OSPREY_B2W_HANDLER_H_

#include "b2w_common.h"

//-----------------------------------------------------------------------------
// JNI Related Utilities
//-----------------------------------------------------------------------------

extern MPPTR     B2W_pool_initialize();                         //MAP_POOL_Initialize
extern void      B2W_mem_pool_push  (void * pool);              //MEM_POOL_Push
extern void      B2W_mem_pool_pop   (void * pool);              //MEM_POOL_Pop
extern CCHPTR    B2W_verify_ascii_string (JNIEnv *env, jstring val); //check if ascii
extern CCHPTR    B2W_verify_utf8_string  (JNIEnv *env, jstring val);
//-----------------------------------------------------------------------------
// WN API  Handler
//-----------------------------------------------------------------------------
  
extern MTABPTR   B2W_map_tab_create (void *);
extern void      B2W_delete_map     (void * Current_Map_Tab);
extern void      B2W_set_line_num   (UINT64 wn_ptr, UINT line_num);
extern WN       *B2W_create_block   ();
extern WN       *B2W_Create_Region(REGION_KIND region_kind, WN* region_body,
                                   WN* region_pragmas,
                                   WN *region_exit, INITO_IDX inito_idx);
extern TY_IDX    B2W_get_ty_from_wn (WN * wn);
extern WN       *B2W_int_const      (UINT mtype, UINT64 const_value);
extern WN       *B2W_float_const    (UINT mtype, double const_value);

extern WN       *B2W_icall(TYPE_ID rtype, TYPE_ID desc, INT32 n, TY_IDX ty);
extern WN       *B2W_call(TYPE_ID type, TYPE_ID desc, INT32 n, ST_IDX s);

extern WN       *B2W_int_type_conversion( WN *wn, TYPE_ID to_type );
extern WN       *B2W_float_type_conversion( WN *wn, TYPE_ID to_type );
//-----------------------------------------------------------------------------
// Function API Handler
//-----------------------------------------------------------------------------
extern UINT64    B2W_start_function (ST_IDX func_st, UINT num_args, UINT scope, int line);
extern void      B2W_finish_function(UINT lastLine);

//-----------------------------------------------------------------------------
// Operator API Handler
//-----------------------------------------------------------------------------

extern WN       *B2W_if             (WN * test, WN * then_block,
                                     WN * else_block);
extern WN       *B2W_ldid           (TYPE_ID mtype, WN_OFFSET offset, ST_IDX sym,
                                     TY_IDX ret_ty_idx, UINT field_id);
extern WN       *B2W_ilda           (OPERATOR opr, TYPE_ID rtype, TYPE_ID desc, WN_OFFSET offset, TY_IDX ty);
// extern WN       *B2W_strctfld       (TYPE_ID rtype, WN * kid0);
extern WN       *B2W_extract_bits   (TYPE_ID rtype, TYPE_ID desc, WN * kid0);
extern WN       *B2W_parm           (TYPE_ID rtype, WN * kid0, TY_IDX ty, UINT32 flag);
extern WN       *B2W_asm_input      (char constraint_string, UINT32 opnd_num, WN * value);
extern WN       *B2W_alloca         (TYPE_ID rtype, WN * kid0);
extern WN       *B2W_comma          (TYPE_ID rtype, TYPE_ID desc, WN * lhs, WN * rhs);
extern WN       *B2W_rcomma         (TYPE_ID rtype, TYPE_ID desc, WN * lhs, WN * rhs);

extern WN       *B2W_exp2           (OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
                                     WN *kid0, WN *kid1);
extern WN       *B2W_stid           (TYPE_ID desc, WN_OFFSET offset, ST_IDX sym,
                                     TY_IDX ty, WN * val, UINT field_id);
extern WN       *B2W_unary          (OPERATOR opr, TYPE_ID rtype,
                                     WN * lhs);
extern WN       *B2W_binary         (OPERATOR opr, TYPE_ID rtype,
                                     WN * lhs, WN * rhs);
extern WN       *B2W_ternary        (OPERATOR opr, TYPE_ID rtype,
                                     WN * kid0, WN * kid1, WN * kid2);

extern WN       *B2W_relational     (OPERATOR opr, TYPE_ID rtype,
                                     WN *l, WN *r);

//-----------------------------------------------------------------------------
// Stmt API Handler
//-----------------------------------------------------------------------------

extern void      B2W_insert_block_last      (WN * block, WN * stmt);
extern void      B2W_insert_block_first     (WN * block, WN * stmt);
extern void      B2W_insert_block_before    (WN * block, WN * stmt, WN * pos);
extern void      B2W_insert_block_after     (WN * block, WN * stmt, WN * pos);
extern WN       *B2W_get_body_from_entry    (WN * entry);

//-----------------------------------------------------------------------------
// Flow Control API Handler
//-----------------------------------------------------------------------------
extern WN       *B2W_create_while_do (WN *test, WN *body);
extern WN       *B2W_create_do_while (WN *test, WN *body) ;
extern WN       *B2W_create_do       (WN *index, WN *start, WN *l_end, WN *step,
                                      WN *body, WN *loop_info);

extern WN       *B2W_create_label    (INT32 label_number,
                                      UINT32 label_flag, WN *loop_info);

extern WN       *B2W_create_goto     (INT32 label_number);


extern WN       *B2W_create_return   (void);

extern WN       *B2W_create_return_val
                                     (OPERATOR opr, TYPE_ID rtype,
                                      TYPE_ID desc, WN *val);

extern WN       *B2W_create_compgoto (INT32 num_entries, WN *value,
                                      WN *block, WN *deflt, INT32 last_label);

extern WN       *B2W_create_switch   (INT32 num_entries, WN *value, WN *block, WN *deflt,
                                      INT32 last_label);
//-----------------------------------------------------------------------------
// FLD API Handler
//-----------------------------------------------------------------------------
extern FLD_IDX   B2W_get_current_field_idx ();
extern FLD_IDX   B2W_allocate_spot_in_class(TY_IDX class_ty, CCHPTR fld_name, UINT offset);
extern FLD_IDX   B2W_set_fld_as_pointer    (TY_IDX class_ty, FLD_IDX fld);
extern TY_IDX    B2W_make_pointer_type     (TY_IDX ty);
extern FLD_IDX   B2W_set_fld_type          (TY_IDX class_ty,  FLD_IDX fld_idx,
                                            TY_IDX typeidx,  UINT64 offset);
extern void      B2W_set_fld_baseclass     (FLD_IDX fld_idx);
extern void      B2W_finalize_fields       (TY_IDX ty, FLD_IDX first_field_idx, FLD_IDX last_field_idx);
extern void      B2W_traverse_struct_size  (TY_IDX ty_idx, UINT *size, INT alignment);


//-----------------------------------------------------------------------------
// ST API Handler
//-----------------------------------------------------------------------------

extern ST_IDX    B2W_create_var_symbol      (const char *  name,
                                             INT64 typeIdx,
                                             INT32 sclass,
                                             INT32 eclass,
                                             INT32 level); //  ST_Init for Var

extern ST_IDX    B2W_create_method_symbol   (const char * name,
                                             INT64 typeIdx, INT32 sclass,
                                             INT32 eclass,  INT32 level);



//=============================
// ST, TY attributes
//=============================
extern ST       *B2W_get_stptr              (ST_IDX st_idx);
extern TY_IDX    B2W_get_ty_from_st         (ST_IDX st_idx);
extern TY       *B2W_get_typtr              (TY_IDX idx);
extern void      B2W_set_pu_cxx_lang        (ST_IDX func_st);
extern void      B2W_set_pu_c_lang          (ST_IDX func_st);
extern void      B2W_set_pu_java_lang       (ST_IDX func_st);
extern void      B2W_set_pu_constructor     (ST_IDX func_st);
extern void      B2W_set_pu_no_return       (ST_IDX func_symbol);
extern void      B2W_set_curr_pu_rbc        ();
extern void      B2W_set_pu_is_mainpu       (ST_IDX func_symbol);
extern void      B2W_clear_pu_is_inline     (ST_IDX func_symbol);
extern void      B2W_set_pu_no_inline       (ST_IDX func_symbol);

//=============================
// Externally Usable
//=============================
extern void      B2W_bind_func_params       (U32VEC &args, ST_IDX class_st,
                                             WN *entry);
extern INITO_IDX B2W_initialize_with_array  (I64VEC &value, ST_IDX symbol);
extern void      B2W_build_eh_begin         ();
extern void      B2W_build_eh_end           ();
extern void      B2W_set_eh_inito           (WN *region_wn, LABEL_IDX comparator_idx, ST_IDX supp_symbol, ST_IDX *symbols,
                                             UINT symbols_len);
extern INT       B2W_build_try_catch        (WN *begin_wn, WN *end_wn, WN *parent_handler, WN **handlers,
                                             ST_IDX* handler_sts, int len);
extern void      B2W_setup_entry_for_eh     (ST_IDX *eh_throws, int len);
extern INT64     B2W_get_try_comparator     (INT try_idx);

//=============================
// Internal use Initialization
//=============================
extern void      B2W_initialize_set_st      (ST * st);
extern void      B2W_aggregate_array        (INITO_IDX _inito, ST *st,
                                             U32VEC &st_list, UINT64 *ofst, BOOL is_root);
extern void      B2W_aggregate_int          (INITO_IDX _inito, INT64 val,  INT size, BOOL is_root);
extern void      B2W_aggregate_uint         (INITO_IDX _inito, UINT64 val, INT size, BOOL is_root);
extern void      B2W_aggregate_symbol       (INITO_IDX _inito, ST *st, WN_OFFSET ofst, BOOL is_root);
extern void      B2W_aggregate_address      (INITO_IDX _inito, ST *st, INT ofst, BOOL is_root);
extern void      B2W_aggregate_block        (INITO_IDX _inito, BOOL is_root);
extern void      B2W_aggregate_pad          (INITO_IDX _inito, UINT64 ofst, BOOL is_root);
extern void      B2W_aggregate_string       (INITO_IDX _inito, CCHPTR str, INT size, BOOL is_root);
extern void      B2W_aggregate_real         (INITO_IDX _inito, FLOAT8 fval, UINT size, BOOL is_root);
extern void      B2W_aggregate_label        (INITO_IDX _inito, LABEL_IDX label, UINT32 repeat,
                                             UINT32 mtype, UINT32 flags, BOOL is_root);
extern void      B2W_aggregate_tcon_int     (INITO_IDX _inito, INT64 val, INT size, BOOL is_root);
//-----------------------------------------------------------------------------
// TY API Handler
//-----------------------------------------------------------------------------

extern TY_IDX    B2W_get_primitive_type     (UINT kind);          // MTYPE -> Idx
extern TY_IDX    B2W_create_class_type      (const char *name, INT32 size,
                                             INT32 alignWidth);
extern TY_IDX    B2W_create_func_type       (UINT32 ret_idx, U32VEC arg_idx,
                                             UINT align);
extern TY_IDX    B2W_set_ty_is_const        (TY_IDX idx);
extern TY_IDX    B2W_set_ty_is_volatile     (TY_IDX idx);

extern TY_IDX    B2W_create_array_type        (UINT size, TY_IDX elm_ty);
extern TY_IDX    B2W_create_common_array_type (UINT size, TY_IDX elm_ty, INT align,
                                               WN * ubound, WN * block, WN* sizewn); // for vla
extern void      B2W_set_arb_upper_bound      (TY_IDX arr_ptr, WN *size_wn);
extern TY_IDX    B2W_create_ref_type          (CCHPTR name_utf8, TY_IDX pointedIdx_);

//-----------------------------------------------------------------------------
// Scope Handler
//-----------------------------------------------------------------------------
extern UINT32    B2W_new_scope              (MPPTR pool_ptr);
extern UINT      B2W_get_file_dst_info      (CHPTR name, UINT dir);
extern UINT      B2W_get_dir_dst_info       (CHPTR name);
extern void      B2W_set_st_srcpos          (INT64 idx, INT64 line_in_current_file);
extern void      B2W_print_backtrace        ();
extern DST_IDX   B2W_dst_build_subprogram   (ST_IDX func_st, PU_Info * pu_info, SRCPOS srcpos);
#endif
