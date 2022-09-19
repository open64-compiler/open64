/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

#include <stdio.h>
#include <iostream>
#include "b2w_handler.h"
#include "b2w_common.h"

extern void B2W_file_close();
extern void B2W_file_finish ();
extern void B2W_finish () ;


extern void B2W_init(INT);
extern void B2W_file_open_ir(CHPTR);
extern void B2W_file_init_dst(CHPTR);

static UINT func_type;
void my(){

  // =========================================
  // InitO Tests
  // =========================================
  I64VEC    zti_table;
  CCHPTR    str_val   = (new std::string("SOMEWHAT-UTF8-STRING"))->c_str();

  TY_IDX ptr_to_func        = B2W_make_pointer_type(func_type);
  TY_IDX arr_to_ptr_to_func = B2W_create_array_type(2 + 1, ptr_to_func);

  ST_IDX  cxx_abi_ty         = B2W_set_ty_is_const(arr_to_ptr_to_func);
  ST_IDX  cxx_abi_st         = B2W_create_var_symbol(
    "_ZTVN10__cxxabiv121__vmi_class_type_infoE",
    cxx_abi_ty, SCLASS_EXTERN, EXPORT_PREEMPTIBLE, 1);


  zti_table.push_back(B2W_INIT_MASK);
  zti_table.push_back(B2W_INIT_START_BLK);
  zti_table.push_back(B2W_INIT_MASK);
  zti_table.push_back(B2W_INIT_START_BLK);
  zti_table.push_back(B2W_INIT_OFST_MASK);
  zti_table.push_back(2);
  zti_table.push_back(B2W_INIT_SYM_MASK);
  zti_table.push_back(cxx_abi_st);
  zti_table.push_back(B2W_INIT_I8_CST_MASK);
  zti_table.push_back(-1);
  zti_table.push_back(B2W_INIT_MASK);
  zti_table.push_back(B2W_INIT_END_BLK);
  zti_table.push_back(B2W_INIT_MASK);
  zti_table.push_back(B2W_INIT_END_BLK);

  std::string sta;
  sta = "1simpleClass_50";
  TY_IDX  zts_ty             = B2W_create_array_type((UINT) (sta.size() + 1),
                                                     B2W_get_primitive_type(MTYPE_I1));
  ST_IDX  zts_st             = B2W_create_var_symbol("_ZTS14simpleClass_50", zts_ty,
                                                     SCLASS_UGLOBAL, EXPORT_PREEMPTIBLE, 1);
  //st_used_as_initialization
  TY_IDX  rand_str_ty        = B2W_create_array_type(sta.size() + 1, B2W_get_primitive_type(MTYPE_I1));
  ST_IDX  rand_str_st        = B2W_create_var_symbol("Random_String_Test",
                                                     B2W_set_ty_is_const(rand_str_ty),
                                                     SCLASS_UGLOBAL, EXPORT_PREEMPTIBLE, 1); //vtable

  I64VEC rand_str_table;
  rand_str_table.push_back(B2W_INIT_STR_CST_MASK);
  CCHPTR simple_ascii_string = std::string("abcdeabcdeabcde").c_str();
  rand_str_table.push_back(B2W_CONTEXT::Set_temporaries(
    (void*) new CHU64P(
      (CHPTR) simple_ascii_string,
      strlen(simple_ascii_string) + 1)));
  B2W_initialize_with_array(rand_str_table, rand_str_st);
  B2W_initialize_with_array(zti_table, zts_st);


  ST_IDX vtable_st          = B2W_create_var_symbol("_ZTV14simpleClass_50",
                                                    B2W_set_ty_is_const(arr_to_ptr_to_func),
                                                    SCLASS_EXTERN, EXPORT_PREEMPTIBLE, 1); //vtable

}


void basic_class_type_test(TY_IDX  &ty_info_ty) {

  TY_IDX void_ty = B2W_get_primitive_type(MTYPE_V);
  vector<FLD_IDX>       flds;
  ty_info_ty         = B2W_create_class_type("__type_ime_test1", 16, 8);
  flds.push_back(B2W_allocate_spot_in_class(ty_info_ty, "typeVtable", 0));
  flds.push_back(B2W_allocate_spot_in_class(ty_info_ty, "shorVarA", 0));
  flds.push_back(B2W_allocate_spot_in_class(ty_info_ty, "typeName", 0));
  B2W_set_fld_type(ty_info_ty, flds[0], B2W_make_pointer_type(void_ty), 0);
  B2W_set_fld_type(ty_info_ty, flds[1], B2W_get_primitive_type(MTYPE_I2), 0);
  B2W_set_fld_type(ty_info_ty, flds[2], B2W_make_pointer_type(void_ty), 0);
  B2W_finalize_fields(ty_info_ty, flds.front(), flds.back());
  flds.clear();

  ty_info_ty         = B2W_create_class_type("__type_ime_test2", 16, 8);
  flds.push_back(B2W_allocate_spot_in_class(ty_info_ty, "typeVtable", 0));
  flds.push_back(B2W_allocate_spot_in_class(ty_info_ty, "shorVarA", 0));
  flds.push_back(B2W_allocate_spot_in_class(ty_info_ty, "intVarA", 0));
  flds.push_back(B2W_allocate_spot_in_class(ty_info_ty, "typeName", 0));
  B2W_set_fld_type(ty_info_ty, flds[0], B2W_make_pointer_type(void_ty), 0);
  B2W_set_fld_type(ty_info_ty, flds[1], B2W_get_primitive_type(MTYPE_I2), 0);
  B2W_set_fld_type(ty_info_ty, flds[2], B2W_get_primitive_type(MTYPE_I4), 0);
  B2W_set_fld_type(ty_info_ty, flds[3], B2W_make_pointer_type(void_ty), 0);
  B2W_finalize_fields(ty_info_ty, flds.front(), flds.back());
  flds.clear();

  ty_info_ty                 = B2W_create_class_type("__type_info_pseudo", 16, 8);
  flds.push_back(B2W_allocate_spot_in_class(ty_info_ty, "typeVtable", 0));
  flds.push_back(B2W_allocate_spot_in_class(ty_info_ty, "typeName", 0));
  B2W_set_fld_type(ty_info_ty, flds[0], B2W_make_pointer_type(void_ty), 0);
  B2W_set_fld_type(ty_info_ty, flds[1], B2W_make_pointer_type(void_ty), 0);
  B2W_finalize_fields(ty_info_ty, flds.front(), flds.back());

  TY_IDX  class_ty_info_ty   = B2W_create_class_type("__class_type_info_pseudo", 16, 8);
  FLD_IDX fld_pseudo         = B2W_allocate_spot_in_class(class_ty_info_ty, ".typeinfo", 0);
  B2W_set_fld_type(class_ty_info_ty, fld_pseudo, ty_info_ty, 0);
  B2W_finalize_fields(class_ty_info_ty, fld_pseudo, fld_pseudo);


  vector<FLD_IDX> kps;
  TY_IDX tpinfo                 = B2W_create_class_type("testOf44", 16, 2);
  kps.push_back(B2W_allocate_spot_in_class(tpinfo, "u2ax", 0));
  kps.push_back(B2W_allocate_spot_in_class(tpinfo, "u2bx", 0));
  kps.push_back(B2W_allocate_spot_in_class(tpinfo, "arrcx", 0));
  B2W_set_fld_type(tpinfo, kps[0], B2W_get_primitive_type(MTYPE_U2), 0);
  B2W_set_fld_type(tpinfo, kps[1], B2W_get_primitive_type(MTYPE_U2), 0);
  B2W_set_fld_type(tpinfo, kps[2], B2W_create_array_type(39, B2W_get_primitive_type(MTYPE_I1)), 0);
  B2W_finalize_fields(tpinfo, kps.front(), kps.back());
  (TY_size(tpinfo) == 44, ("[finalize field error!, expected : 44, given : %d ]", TY_size(tpinfo)));




  ST_IDX st = B2W_create_var_symbol("myVar0", B2W_get_primitive_type(4),
                                    SCLASS_UGLOBAL, EXPORT_PREEMPTIBLE, GLOBAL_SYMTAB);

  I64VEC st_init;
  st_init.push_back(B2W_INIT_I4_CST_MASK);
  st_init.push_back(12345);
  B2W_initialize_with_array(st_init, st);

  ST_IDX st_real = B2W_create_var_symbol("myVar_F4", B2W_get_primitive_type(MTYPE_F4),
                                         SCLASS_UGLOBAL, EXPORT_PREEMPTIBLE, GLOBAL_SYMTAB);
  union {
      INT64 q;
      double t;
      float  f;
  } me;
  me.f = 6.66666;
  st_init.clear();
  st_init.push_back(B2W_INIT_F4_CST_MASK);
  st_init.push_back(me.q);
  B2W_initialize_with_array(st_init, st_real);


  ST_IDX st_real8 = B2W_create_var_symbol("myVar_F8", B2W_get_primitive_type(MTYPE_F8),
                                          SCLASS_UGLOBAL, EXPORT_PREEMPTIBLE, GLOBAL_SYMTAB);
  I64VEC st_some;
  union {
      INT64 q;
      double t;
  } me2;
  me2.t = 6.66666829292929292992929;
  st_some.push_back(B2W_INIT_F8_CST_MASK);
  st_some.push_back(me2.q);
  B2W_initialize_with_array(st_some, st_real8);

}


TY_IDX get_simple_class_type() {
// ================ Class Type ================
  TY_IDX class_type = B2W_create_class_type("simpleClass_50_class", 1, B2W_CONTEXT::Get_default_align());
  TY_IDX   int_type = B2W_get_primitive_type(4);

  INT32    next_id  = 1;
  FLD_IDX  startp   = B2W_get_current_field_idx();

  FLD_IDX  fld0 = B2W_allocate_spot_in_class(class_type, ".vptr_simpleClass_50", 0);
  next_id  ++;

  FLD_IDX  fld1 = B2W_allocate_spot_in_class(class_type, "myfirstIntField", 0);
  next_id  ++;

  FLD_IDX  fld2 = B2W_allocate_spot_in_class(class_type, "mySecondIntField", 0);
  next_id  ++;

  B2W_set_fld_as_pointer(class_type, startp);
  // calc class_type's size

  // Expand field - classes (which are used pointers) now

  // 1. Expand VTable
  TY_IDX ptr_to_func        = B2W_make_pointer_type(func_type);
  TY_IDX arr_to_ptr_to_func = B2W_create_array_type(2 + 1, ptr_to_func);
  // 2 + funcs.length
  // vtable_symbol size = func_list.length + 1(NOP)
  TY_IDX ptr_to_ptr_to_func = B2W_make_pointer_type(ptr_to_func);  // vptr
  // Create Symbol (func*)[] , vptr func**;

  B2W_set_fld_type(class_type, fld0, ptr_to_ptr_to_func, 0);  //vtable
  B2W_set_fld_type(class_type, fld1, ptr_to_ptr_to_func, 0);            // int member field
  B2W_set_fld_type(class_type, fld2, int_type, 0);            // int member field
  next_id  ++;

  B2W_finalize_fields(class_type, fld0, fld2);
  return class_type;
}



void wn_testcall(TY_IDX tyy_path, WN * body, ST_IDX func_st) {

  ST_IDX st = B2W_create_var_symbol(".test-callee", tyy_path, SCLASS_UGLOBAL, EXPORT_PREEMPTIBLE, 1);
  // ================ Assign Test ================
  WN *const_6_6 =B2W_float_const(MTYPE_F8, (double) 6.665465);
  WN *const_4_4 =B2W_float_const(MTYPE_F8, (double) 4.425156);

  WN *const_10 = B2W_int_const(MTYPE_I4, (UINT) 10);
  WN *const_11 = B2W_int_const(MTYPE_I4, (UINT) 11);
  WN *wAdd     = B2W_binary(OPR_ADD, MTYPE_I4, const_10, const_11);
  TY_IDX ty_i4 = B2W_get_primitive_type(4);
  WN *assign   = B2W_stid(MTYPE_I4, 0, st, ty_i4, wAdd, 0);
  WN *wAdd2    = B2W_binary(OPR_ADD, MTYPE_F8, const_6_6, const_4_4);
  TY_IDX ty_f8 = MTYPE_To_TY(MTYPE_F8);
  WN *assign2  = B2W_stid(MTYPE_F8, 0, st, ty_f8, wAdd2, 0);

  printf("%llu\n", (UINT64) wAdd);

  WN * wn = B2W_int_const(MTYPE_I4, 10);

  B2W_insert_block_last(body, assign);
  WN *region_ptr = B2W_Create_Region(REGION_KIND_EH, B2W_create_block(), B2W_create_block(), B2W_create_block(), 0);
  B2W_insert_block_last(body, region_ptr);
  B2W_insert_block_last(body, assign2);

  WN * const_12 = B2W_int_const(MTYPE_I4, (UINT) 12);
  ST_IDX myvar = B2W_create_var_symbol("myvar", B2W_get_primitive_type(MTYPE_I4), 8, 5, 1);
  WN * test_cond        = WN_NE(MTYPE_I4, const_12, B2W_ldid(MTYPE_I4, 0, myvar, B2W_get_primitive_type(MTYPE_I4), 0));
  WN * while_loop_block = B2W_create_block();
  WN * while_ = B2W_create_do_while(test_cond,while_loop_block);
  B2W_insert_block_last(body, while_);

  WN *long_var   = B2W_int_const(MTYPE_I8, 23333333);
  UINT long_varr = 233333;
  ST_IDX st_c = B2W_create_var_symbol("jc",
                                      B2W_get_primitive_type(MTYPE_I8), 1, 0, 2);

  WN *double_var = B2W_float_const(MTYPE_F8, 2.3333);

  TY_IDX class_type = get_simple_class_type();

  // ================ Class Symbol ================
  ST_IDX obj = B2W_create_var_symbol("myInstanceOfAClass", class_type, SCLASS_UGLOBAL,
                                     EXPORT_PREEMPTIBLE, 1);

  WN    *long_var1      = B2W_int_const(MTYPE_I4, 17878663259);
  WN    *double_var1    = B2W_float_const(MTYPE_F8, 2.3333333);

  TY_IDX class_ptr_type = B2W_make_pointer_type(class_type);
  ST_IDX pointer_to_obj = B2W_create_var_symbol("myPointerOfAClass", class_ptr_type,
                                                SCLASS_UGLOBAL, EXPORT_PREEMPTIBLE, 1);
  WN *lda_wn = WN_Lda(Pointer_Mtype, 0, B2W_get_stptr(obj), 0);

  WN   *st_pointer   = B2W_stid(Pointer_Mtype, 0, pointer_to_obj, B2W_get_ty_from_st(pointer_to_obj), lda_wn, 0);
  B2W_insert_block_last(body, st_pointer);
  WN   *ldid_pointer = B2W_ldid(Pointer_Mtype, 0, pointer_to_obj, B2W_get_ty_from_st(pointer_to_obj), 0);
  WN   *store_field  = WN_Istore(MTYPE_I4, 0, class_ptr_type, ldid_pointer, long_var1, 1);
  B2W_insert_block_last(body, store_field);
  WN   *load_field   = WN_Iload(MTYPE_I4, 0, class_type, ldid_pointer, 2);
  ST_IDX store_var_idx = B2W_create_var_symbol("store_field", B2W_get_primitive_type(MTYPE_I4), 8, 5, 1);
  WN   *stid_field   = B2W_stid(MTYPE_I4, 0, store_var_idx, B2W_get_ty_from_st(store_var_idx), load_field, 0);
  B2W_insert_block_last(body, stid_field);


  //B2W_int_type_conversion(long_var, MTYPE_I4);
  B2W_float_type_conversion(double_var, MTYPE_F4);

  //WN * create_do = B2W_create_do();

  WN    *wn_     = WN_Create(OPC_LABEL, 0);
  WN    *label   = B2W_create_label(WN_label_number(wn_), WN_label_flag(wn_), (WN *) 0);
  WN    *go_to   = B2W_create_goto(WN_label_number(wn_));
  B2W_insert_block_last(body, label);
  B2W_insert_block_last(body, go_to);

  WN    *resolve = B2W_stid(MTYPE_I4, 0, store_var_idx, B2W_get_ty_from_st(store_var_idx), WN_Intconst(MTYPE_I4, 409), 0);
  B2W_insert_block_last(body, resolve);

  WN    *return_ = B2W_create_return();
  B2W_insert_block_last(body, return_);


  //WN * func_call = B2W_call(MTYPE_I4, MTYPE_I4, 0, func_st);
//B2W_insert_block_last(body, func_call);

  //ST_IDX myvar = B2W_create_var_symbol("myvar", B2W_get_primitive_type(MTYPE_I4, 0), 8, 5, 1);


  /*WN * const_100 = B2W_int_const(MTYPE_I4, (UINT) 100);


  ST_IDX    i    = B2W_create_var_symbol("i", B2W_get_primitive_type(MTYPE_I4, 0), SCLASS_AUTO, EXPORT_LOCAL, 2);

  UINT ex = 1;


  B2W_initialize_var_symbol(i, IV_INT_CST, (void *)&ex);



  WN * test_cond        = B2W_le(MTYPE_I4, B2W_ldid(MTYPE_I4, 0, i, B2W_get_primitive_type(MTYPE_I4, 0), 0),
                                 const_100);

  WN * while_loop_block = B2W_create_block();

  WN * one              = B2W_int_const(MTYPE_I4, 1);

  WN * add1        = B2W_add(MTYPE_I4,
                                  B2W_ldid(MTYPE_I4, 0, myvar, B2W_get_primitive_type(MTYPE_I4, 0), 0),
                                  one);

  WN * add_myvar   = B2W_stid(MTYPE_I4, 0, myvar, B2W_get_primitive_type(MTYPE_I4, 0), add1, 0);
*/

  TY_IDX supp_idx    = B2W_create_class_type("dummy_eh_supp_ty_2", 8, 4);
  vector<FLD_IDX>      fields_of_eh_supp;
  fields_of_eh_supp.push_back(B2W_allocate_spot_in_class(supp_idx, "u4_simple", 0));
  fields_of_eh_supp.push_back(B2W_allocate_spot_in_class(supp_idx, "u4_simple", 0));
  B2W_set_fld_type    (supp_idx, fields_of_eh_supp[0], B2W_get_primitive_type(MTYPE_U4), 0);
  B2W_set_fld_type    (supp_idx, fields_of_eh_supp[1], B2W_get_primitive_type(MTYPE_U4), 0);
  B2W_finalize_fields (supp_idx, fields_of_eh_supp.front(), fields_of_eh_supp.back());
  ST_IDX supp_sym    = B2W_create_var_symbol("dummy_eh_supp", supp_idx, SCLASS_EH_REGION_SUPP, EXPORT_LOCAL, 2);
  //WN * return_     = B2W_create_return();

  WN * func_call = B2W_call(MTYPE_I4, MTYPE_I4, 0, func_st);
  B2W_insert_block_last(body, func_call);
  B2W_build_eh_begin();

  WN     **handlers  = new WN *[1];
  ST_IDX  *handle_st = new ST_IDX[1];
  handle_st[0]       = pointer_to_obj;
  handlers [0]       = resolve;
  INT      try_idx   = B2W_build_try_catch(assign, assign2, NULLPTR, handlers, handle_st, 1);
  INT64    label_t   = B2W_get_try_comparator(try_idx);
  B2W_set_eh_inito (region_ptr, (LABEL_IDX) label_t, supp_sym, NULLPTR, 0);
  B2W_finish_function (40000);
}

void Check_get_folder_name(std::string orig, std::string destfn, FILE_SYSTEM file_sys) {
  std::string act = B2W_CONTEXT::Get_file_folder_name(orig, file_sys);
  Is_Valid(act == destfn, ("Folder Failed on org = %s, dest = %s, res = %s", orig.c_str(), destfn.c_str(), act.c_str()));
}

void Check_get_file_name(std::string orig, std::string destfn, FILE_SYSTEM file_sys) {
  std::string act = B2W_CONTEXT::Get_file_base_name(orig, file_sys);
  Is_Valid(act == destfn, ("File check failed on org = %s, dest = %s, res = %s", orig.c_str(), destfn.c_str(), act.c_str()));
}

int main(){

  const char *p        = "MyOutput.B";
  Tracing_Enabled      = true;

  B2W_init(64);
  B2W_CONTEXT::Set_log_level(0xffffffff);
  
  B2W_file_open_ir((CHPTR) p);
  B2W_file_init_dst((CHPTR) "MyOutput.class");

  printf("%s %s", DBar, "B2W_get_primitive_type : \n");
  //-----------------------------------------------
  for(int i = 1; i<8; i++){
      printf("%u -> %u, ",i, B2W_get_primitive_type((UINT) i));
  }


  TY_IDX   int_type = B2W_get_primitive_type(4);
  TY_IDX array_type = B2W_create_array_type(10, int_type);
  array_type = B2W_create_array_type(11, int_type);
  array_type = B2W_create_array_type(12, int_type);
  array_type = B2W_create_array_type(13, int_type);
  array_type = B2W_create_array_type(14, int_type);
  array_type = B2W_create_array_type(15, int_type);

  B2W_create_common_array_type(0, int_type, DEFAULT_ALIGN, 0, 0, 0);

  TY_IDX void_ty    = B2W_get_primitive_type(10);

  // =================  Function Test  ====================
  U32VEC  args_ty_list;
  args_ty_list.push_back(int_type);
  args_ty_list.push_back(int_type);
  args_ty_list.push_back(int_type);
  args_ty_list.pop_back();

  // Creating a level 2 Pool
  MPPTR pool = (MPPTR) B2W_pool_initialize();
  B2W_map_tab_create(pool);
  UINT scope = B2W_new_scope(pool);

  // Generating the function symbol for level 1
  func_type = B2W_create_func_type(void_ty, args_ty_list, B2W_CONTEXT::Get_align_width());
  ST_IDX  func_st      = B2W_create_method_symbol("(V)Simple.main(V)", func_type,
                             SCLASS_TEXT, EXPORT_PREEMPTIBLE, GLOBAL_SYMTAB);

  get_simple_class_type();

  Is_Valid(CURRENT_SYMTAB == 2, ("[ERROR] b2w_test current_symtab misplaced"));


  WN * entry = (WN *) B2W_start_function(func_st, 0, scope, 0);

  B2W_setup_entry_for_eh(NULL, 0);

  // Adding parameters to entry
  ST_IDX  int_parm1    = B2W_create_var_symbol("intparm1", int_type,
                                               SCLASS_FORMAL, EXPORT_LOCAL, 2);
  ST_IDX  int_parm2    = B2W_create_var_symbol("intparm2", int_type,
                                               SCLASS_FORMAL, EXPORT_LOCAL, 2);

  Check_get_folder_name("/a/b/c/d.java", "/a/b/c", FILE_SYSTEM_UNIX);
  Check_get_folder_name("/a/b/c/.java", "/a/b/c", FILE_SYSTEM_UNIX);
  Check_get_folder_name("/a/b/c//e.java", "/a/b/c/", FILE_SYSTEM_UNIX);
  Check_get_folder_name("/java", "/", FILE_SYSTEM_UNIX);
  Check_get_folder_name("m.java", Get_Current_Working_Directory(), FILE_SYSTEM_UNIX);
  Check_get_folder_name("a/b/c/d.java", "a/b/c", FILE_SYSTEM_UNIX);

  Check_get_folder_name("C:\\oneload\\aone.c", "C:\\oneload", FILE_SYSTEM_WINDOWS);
  Check_get_folder_name("C:\\oneload\\aone.java", "C:\\oneload", FILE_SYSTEM_WINDOWS);
  Check_get_folder_name("\\\\.\\System\\a\\b\\c.java", "\\\\.\\System\\a\\b", FILE_SYSTEM_WINDOWS);
  Check_get_folder_name("\\\\?\\System\\a\\b\\c.java", "\\\\?\\System\\a\\b", FILE_SYSTEM_WINDOWS);
  Check_get_folder_name("\\Program Files\\a.java", "\\Program Files", FILE_SYSTEM_WINDOWS);
  Check_get_folder_name("\\\\ab", "\\\\", FILE_SYSTEM_WINDOWS);
  Check_get_folder_name("\\\\a\\b.java", "\\\\a", FILE_SYSTEM_WINDOWS);
  Check_get_folder_name("\\a\\b.java", "\\a", FILE_SYSTEM_WINDOWS);

  Check_get_file_name("/a/b/c/d.java", "d.java", FILE_SYSTEM_UNIX);
  Check_get_file_name("/a/b/c/.java", ".java", FILE_SYSTEM_UNIX);
  Check_get_file_name("/a/b/c//e.java", "e.java", FILE_SYSTEM_UNIX);
  Check_get_file_name("/java", "java", FILE_SYSTEM_UNIX);
  Check_get_file_name("m.java", "m.java", FILE_SYSTEM_UNIX);
  Check_get_file_name("a/b/c/d.java", "d.java", FILE_SYSTEM_UNIX);

  Check_get_file_name("C:\\oneload\\aone.c", "aone.c", FILE_SYSTEM_WINDOWS);
  Check_get_file_name("C:\\oneload\\aone.java", "aone.java", FILE_SYSTEM_WINDOWS);
  Check_get_file_name("\\\\.\\System\\a\\b\\c.java", "c.java", FILE_SYSTEM_WINDOWS);
  Check_get_file_name("\\\\?\\System\\a\\b\\c.java", "c.java", FILE_SYSTEM_WINDOWS);
  Check_get_file_name("\\Program Files\\a.java", "a.java", FILE_SYSTEM_WINDOWS);
  Check_get_file_name("\\\\ab", "ab", FILE_SYSTEM_WINDOWS);
  Check_get_file_name("\\\\a\\b.java", "b.java", FILE_SYSTEM_WINDOWS);
  Check_get_file_name("\\a\\b.java", "b.java", FILE_SYSTEM_WINDOWS);


  B2W_CONTEXT::Set_file("oneload.kt", FILE_SYSTEM_UNIX);
  B2W_CONTEXT::Set_file("C:\\oneload\\aone.c", FILE_SYSTEM_WINDOWS);
  B2W_CONTEXT::Set_file("C:\\oneload\\aone.java", FILE_SYSTEM_WINDOWS);
  B2W_CONTEXT::Set_file("\\\\.\\System\\a\\b\\c.java", FILE_SYSTEM_WINDOWS);
  B2W_CONTEXT::Set_file("\\\\?\\System\\a\\b\\c.java", FILE_SYSTEM_WINDOWS);
  B2W_CONTEXT::Set_file("\\Program Files\\a.java", FILE_SYSTEM_WINDOWS);
  B2W_CONTEXT::Set_file("/usr/b/c/a.java", FILE_SYSTEM_UNIX);
  B2W_set_st_srcpos(int_parm1, 4);

  U32VEC  int_args_vec = {int_parm1, int_parm2};
  B2W_bind_func_params(int_args_vec, func_st, entry);

  INT64 arrayTy = B2W_create_array_type (10, B2W_get_primitive_type(MTYPE_I4)); 
  B2W_create_var_symbol((CCHPTR) "myArray", arrayTy, 8, 5, 1);

  TY_IDX  ty_info_ty;
  basic_class_type_test(ty_info_ty);
  wn_testcall(ty_info_ty, B2W_get_body_from_entry(entry), func_st);
  my();

  printf(" -- ***** success ***** --\n ");


  B2W_file_close();
  B2W_file_finish();
  B2W_finish();

  printf("%s ######## finish all. ########\n%s\n", DBar, DBar);
}

