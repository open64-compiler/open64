/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

//-*-c++-*-
// =============================================================================
// =============================================================================
//
// Module: opt_vsa.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: $
// $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/config_vsa.cxx,v $
//
// Revision history:
//  Spring 2018 - Original Version
//
// =============================================================================
//
// Description:
//
//  Configure the -VSA group (included in config.cxx).
//
// =============================================================================
// =============================================================================
/* This file is included in config.c, so it doesn't need its own set of
 * standard includes -- only the following:
 */
#include "config_vsa.h"


/* ====================================================================
 *
 * Global flag variables which reflect the -VSA group options.
 *
 * ====================================================================
 */

BOOL VSA_Cda = TRUE;
BOOL VSA_Vra = TRUE;
BOOL VSA_Uiv = TRUE;
BOOL VSA_Aob = TRUE;
BOOL VSA_Aofb = FALSE;
BOOL VSA_Npd = TRUE;
BOOL VSA_Rbc = TRUE;
BOOL VSA_Ubf = TRUE;
BOOL VSA_Uaf = TRUE;
BOOL VSA_Msf = TRUE;
BOOL VSA_Dbf = TRUE;
BOOL VSA_Dbz = TRUE;
BOOL VSA_Ddv = TRUE;
BOOL VSA_Ral = TRUE;
BOOL VSA_Udr = TRUE;
BOOL VSA_Fam = TRUE;
BOOL VSA_Fmt = TRUE;
BOOL VSA_Ecb = TRUE;
BOOL VSA_Tnh = FALSE;   // Turn off THN by default
BOOL VSA_Ddc = TRUE;
BOOL VSA_Rcd = TRUE;
BOOL VSA_Scb = TRUE;
BOOL VSA_Sse = TRUE;
BOOL VSA_Uic = TRUE;
BOOL VSA_Muchi = TRUE;
BOOL VSA_Not_Muchi = TRUE;
BOOL VSA_Alias_Intent = TRUE;
BOOL VSA_Alias_Local_Lda = FALSE;
BOOL VSA_Vra_Mayzero = FALSE;
BOOL VSA_Vra_Ivar = TRUE;
BOOL VSA_Param_Uiv = FALSE;
BOOL VSA_Extern_Uiv = FALSE;
BOOL VSA_Prop_Vardef = TRUE;
BOOL VSA_Vsym_Memcall = TRUE;
BOOL VSA_Vsym_Uiv = TRUE;
BOOL VSA_Vsym_Npd = TRUE;
BOOL VSA_NPD_Nullbase = TRUE;
BOOL VSA_Udr_Null = FALSE;
BOOL VSA_MSF_Addr_Pass = FALSE;
BOOL VSA_Global_Var_Zero = FALSE;
BOOL VSA_EH = TRUE;
INT  VSA_EH_Spec_Default = 0;
INT  VSA_EH_LEVEL = 1;
BOOL VSA_Enable_DCE = FALSE;
BOOL VSA_Devirt_Aggr = TRUE;
INT  VSA_Devirt_Aggr_Threshold = 2;
INT  VSA_Connect_Icall_Threshold = 32;
BOOL VSA_HOA_Prop = FALSE;
BOOL VSA_Hor = TRUE;
BOOL VSA_Hor_Unification = TRUE;
BOOL VSA_Xsca = TRUE;
BOOL VSA_Rvsa = FALSE;
BOOL VSA_Customized_Rule = FALSE;
BOOL VSA_Xcalibyte_Rule = TRUE;
BOOL VSA_Multi_Thread_Rule = FALSE;
BOOL VSA_Global_Muchi = FALSE;
BOOL VSA_Global_Fld_Muchi = FALSE;
BOOL VSA_Global_Vor_Muchi = FALSE;
BOOL VSA_Global_Fld_Vor_Muchi = FALSE;
BOOL VSA_Src_Tainted = TRUE;
BOOL VSA_Tainted_Int32C = FALSE;
BOOL VSA_ST_Modified = TRUE;
BOOL VSA_HVA_Delay_Ho_Rename = TRUE;
BOOL VSA_New_HVA = TRUE;
INT  VSA_New_HVA_Round = 16;
BOOL VSA_New_HVA_Compat = TRUE;
BOOL VSA_New_HVA_Unification = TRUE;
BOOL VSA_New_Heap_Checker = TRUE;
BOOL VSA_Warn_On_Global = FALSE;
BOOL VSA_Warn_On_Param = FALSE;
BOOL VSA_Warn_On_Retval = FALSE;
BOOL VSA_Warn_On_This = FALSE;
BOOL VSA_Enable_Lib_Check = FALSE;
BOOL VSA_Demo = FALSE;
BOOL VSA_Cd_Filter = TRUE;
BOOL VSA_Experimental = FALSE;
BOOL VSA_New_Npd_Checker = TRUE;
BOOL VSA_New_Uiv_Checker = TRUE;
BOOL VSA_New_Cprop = TRUE;
BOOL VSA_Prop_Values = FALSE;
BOOL VSA_New_Cselect_Lower = TRUE;
BOOL VSA_Value_Graph = TRUE;
BOOL VSA_Value_Graph_Lazy = TRUE;
BOOL VSA_Check_Main_Entry = TRUE;
BOOL VSA_Java_Tmp = TRUE;
BOOL VSA_Check_Devirt = FALSE;
BOOL VSA_Model_Lda = TRUE;
BOOL VSA_Maybe_Report = TRUE;
BOOL VSA_Ignore_Dup = FALSE;
INT  VSA_Compiletime_Triage = 0;
STRING VSA_Authen_Token = NULL;
STRING VSA_Authen_Server = NULL;

BOOL VSA_Single_Report = TRUE;
#ifdef Is_True_On
BOOL VSA_Output_Json = FALSE;
BOOL VSA_Output_Cbor = FALSE;
#else
BOOL VSA_Output_Json = FALSE;
BOOL VSA_Output_Cbor = FALSE;
#endif
BOOL VSA_Output_Msgid = FALSE;
INT  VSA_Json_Version = 1;
INT  VSA_Json_Path_Max = 30;
INT  VSA_Path_Head_Max = 16;
INT  VSA_Path_Tail_Max = 24;
UINT VSA_Du_Mem_High = 512;    // 512B for high watermark for D-U memory
UINT VSA_Du_Mem_Low  =  64;    // 64MB for low watermark for D-U memory
BOOL VSA_Emit_Whirl = TRUE;
BOOL VSA_CXX_Intrn = FALSE;
BOOL VSA_Checker_SE = FALSE;
BOOL VSA_Builtin_Jni = FALSE;
BOOL VSA_Builtin_Recursion = FALSE;
BOOL VSA_Builtin_CertC = FALSE;
BOOL VSA_Builtin_CertJ = FALSE;
BOOL VSA_RBC_Exception = TRUE;
BOOL VSA_Enable_FSM = TRUE;
BOOL VSA_Enable_TAG_OLD = FALSE;
BOOL VSA_Enable_TAG = TRUE;
BOOL VSA_Enable_Prop_Tag = TRUE;
BOOL VSA_Enable_Chi_Prop_Tag = FALSE;
BOOL VSA_Enable_May_Tag = TRUE;
BOOL VSA_Enable_Taint_Model = TRUE;
BOOL VSA_Ignore_Asm = TRUE;
BOOL VSA_Check_RT_version = TRUE;
BOOL VSA_Authen_Enabled = FALSE;
// load all symbols from vtable files
BOOL VSA_Load_All_Sym = TRUE;
#ifdef Is_True_On
BOOL VSA_Authen_Skip_Patch = FALSE;
#endif
BOOL VSA_TCheck = TRUE;
BOOL VSA_OReload = TRUE;
BOOL VSA_Dump_Intrn_Table = FALSE;
OPTION_LIST *VSA_Reg_Map = NULL;
BOOL VSA_Enable_Java_Anno = TRUE;
BOOL VSA_Filter_Include = TRUE;
BOOL VSA_Alert_Huge_Path = FALSE;
STRING VSA_Magic_Id = NULL;
INT  VSA_Issue_Id = 0;

// VSA checker options
BOOL VSA_Checker_Aggr = FALSE;
BOOL VSA_Checker_Vfr_Exact_Match = FALSE;
INT  VSA_Checker_Max_Callby = 32;
INT  VSA_Checker_Max_Callee = 32;
INT  VSA_Checker_Max_Frame  = 128;
INT  VSA_Checker_Max_Srcpos_Child = 2048;
INT  VSA_Checker_Max_Srcpos_Data = 100;
INT  VSA_Checker_Max_Path = 32;
UINT VSA_Call_Stack_Size_Max = 0;
UINT VSA_Call_Stack_Level_Max = 0;
INT  VSA_Issue_Certainty_Maybe = FALSE;

STRING VSA_VCG_Cg_Fname = NULL;

INT  VSA_Target_Env = VSA_ENV_APP;

/* List of global variables to turn on and off various optimizations */

/* ====================================================================
 *
 * Descriptor for the -VSA option group.
 *
 * ====================================================================
 */
static OPTION_DESC Options_VSA[] = {
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "uiv_uninit_var",          "uiv",
    FALSE, 0, 0, &VSA_Uiv,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "aob_array_out_of_bound",  "aob",
    FALSE, 0, 0, &VSA_Aob,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "aofb_out_of_fixed_bound", "aofb",
    FALSE, 0, 0, &VSA_Aofb,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "npd_null_ptr_deref",      "npd",
    FALSE, 0, 0, &VSA_Npd,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "rbc_rule_based_check",    "rbc",
    FALSE, 0, 0, &VSA_Rbc,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "ubf_unbalanced_free",     "ubf",
    FALSE, 0, 0, &VSA_Ubf,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "uaf_use_after_free",      "uaf",
    FALSE, 0, 0, &VSA_Uaf,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "msf_missing_free",        "msf",
    FALSE, 0, 0, &VSA_Msf,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "dbz_division_by_zero",    "dbz",
    FALSE, 0, 0, &VSA_Dbz,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "ddv_dead_assign_val",     "ddv",
    FALSE, 0, 0, &VSA_Ddv,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "ral_return_addr_lcl",     "ral",
    FALSE, 0, 0, &VSA_Ral,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "dbf_double_free",         "dbf",
    FALSE, 0, 0, &VSA_Dbf,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "udr_use_dangling_ref",    "udr",
    FALSE, 0, 0, &VSA_Udr,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "fam_formal_actual_mismatch", "fam",
    FALSE, 0, 0, &VSA_Fam,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "fmt_format_string",       "fmt",
    FALSE, 0, 0, &VSA_Fmt,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "ecb_empty_catch_block",   "ecb",
    FALSE, 0, 0, &VSA_Ecb,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "tnh_throw_no_handle",     "tnh",
    FALSE, 0, 0, &VSA_Tnh,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "ddc_dead_code",           "ddc",
    FALSE, 0, 0, &VSA_Ddc,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "rcd_redundant_ctrldep",   "rcd",
    FALSE, 0, 0, &VSA_Rcd,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "scb_same_code_block",     "scb",
    FALSE, 0, 0, &VSA_Scb,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "sse_same_sub_expression", "sse",
    FALSE, 0, 0, &VSA_Sse,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "uic_uninit_in_ctor",      "uic",
    FALSE, 0, 0, &VSA_Uic,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "cda_control_dependency",  "cda",
    FALSE, 0, 0, &VSA_Cda,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "vra_value_range",         "vra",
    FALSE, 0, 0, &VSA_Vra,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "rvsa_reverse_vsa",        "rvsa",
    FALSE, 0, 0, &VSA_Rvsa, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "xsca",                    "",
    FALSE, 0, 0, &VSA_Xsca, NULL },
  { OVK_BOOL,  OV_INTERNAL, FALSE, "cusr_customized_rule",     "cusr",
    FALSE, 0, 0, &VSA_Customized_Rule, NULL },
  { OVK_BOOL,  OV_INTERNAL, FALSE, "xcbr_xcalibyte_rule",      "xcbr",
    FALSE, 0, 0, &VSA_Xcalibyte_Rule, NULL },
  { OVK_BOOL,  OV_INTERNAL, FALSE, "mtr_multithread_rule",        "mtr",
    FALSE, 0, 0, &VSA_Multi_Thread_Rule, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "hoa_prop",                "hoa",
    FALSE, 0, 0, &VSA_HOA_Prop, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "hor_heap_object",         "hor",
    FALSE, 0, 0, &VSA_Hor,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "hor_unification",         "hor_uni",
    FALSE, 0, 0, &VSA_Hor_Unification,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "muchi_gbl_ref_at_entry",  "muchi",
    FALSE, 0, 0, &VSA_Muchi,NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "nmuchi_gbl_ref_at_entry", "nmuchi",
    FALSE, 0, 0, &VSA_Not_Muchi,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "alias_intent",            "",
    FALSE, 0, 0, &VSA_Alias_Intent, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "alias_local_lda",         "",
    FALSE, 0, 0, &VSA_Alias_Local_Lda, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "vma_vra_may_zero",        "vma",
    FALSE, 0, 0, &VSA_Vra_Mayzero,NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "vra_ivar",                "",
    FALSE, 0, 0, &VSA_Vra_Ivar,   NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "param_uiv",               "",
    FALSE, 0, 0, &VSA_Param_Uiv,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "extern_uiv",              "",
    FALSE, 0, 0, &VSA_Extern_Uiv,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "prop_vardef",             "",
    FALSE, 0, 0, &VSA_Prop_Vardef,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "vsym_uiv",                "",
    FALSE, 0, 0, &VSA_Vsym_Uiv,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "vsym_npd",                "",
    FALSE, 0, 0, &VSA_Vsym_Npd,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "npd_nullbase",            "",
    FALSE, 0, 0, &VSA_NPD_Nullbase,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "udr_null",                "",
    FALSE, 0, 0, &VSA_Udr_Null, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "msf_addr_pass",           "",
    FALSE, 0, 0, &VSA_MSF_Addr_Pass, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "memcall_vsym",            "",
    FALSE, 0, 0, &VSA_Vsym_Memcall,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "zero_global_variable",    "zero_global",
    FALSE, 0, 0, &VSA_Global_Var_Zero,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "eh",                      "eh",
    FALSE, 0, 0, &VSA_EH,   NULL },
  { OVK_INT32,	OV_INTERNAL,	FALSE, "eh_spec",                 "eh_spec",
    0,  0, 1,    &VSA_EH_Spec_Default,  NULL },
  { OVK_INT32,	OV_INTERNAL,	FALSE, "eh_level",                "eh_level",
    1,  0, 100,  &VSA_EH_LEVEL,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "devirt_aggr",             "",
    FALSE, 0, 0, &VSA_Devirt_Aggr,NULL },
  { OVK_INT32,	OV_INTERNAL,	FALSE, "devirt_aggr_threshold",   "",
    2,  1, 20,   &VSA_Devirt_Aggr_Threshold, NULL },
  { OVK_INT32,	OV_INTERNAL,	FALSE, "icall_threshold",         "",
    1,  1, 1024, &VSA_Connect_Icall_Threshold, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "gmc_global_mu_chi",       "gmc",
    FALSE, 0, 0, &VSA_Global_Muchi, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "gfmc_global_field_mu_chi","gfmc",
    FALSE, 0, 0, &VSA_Global_Fld_Muchi, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "gvmc_global_vor_mu_chi",  "gvmc",
    FALSE, 0, 0, &VSA_Global_Vor_Muchi, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "gfvmc_global_field_vor_mu_chi", "gfvmc",
    FALSE, 0, 0, &VSA_Global_Fld_Vor_Muchi, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "src_tainted",             "",
    FALSE, 0, 0, &VSA_Src_Tainted, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "tainted_int32c",          "",
    FALSE, 0, 0, &VSA_Tainted_Int32C, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "stm0_st_modified",        "stm0",
    FALSE, 0, 0, &VSA_ST_Modified, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "delay_hor",               "",
    FALSE, 0, 0, &VSA_HVA_Delay_Ho_Rename, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "nhv",                     "",
    FALSE, 0, 0, &VSA_New_HVA, NULL },
  { OVK_INT32,	OV_INTERNAL,	FALSE, "nhvr_round",              "nhvr",
    12, 4, 32,   &VSA_New_HVA_Round,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "nhvc_compat",             "nhvc",
    FALSE, 0, 0, &VSA_New_HVA_Compat, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "nhvu_unification",        "nhvu",
    FALSE, 0, 0, &VSA_New_HVA_Unification, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "nhc_new_heap_checker",    "nhc",
    FALSE, 0, 0, &VSA_New_Heap_Checker, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "warn_global",             "",
    FALSE, 0, 0, &VSA_Warn_On_Global, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "warn_param",              "",
    FALSE, 0, 0, &VSA_Warn_On_Param, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "warn_retval",             "",
    FALSE, 0, 0, &VSA_Warn_On_Retval, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "warn_this",               "",
    FALSE, 0, 0, &VSA_Warn_On_This, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "lib_check",               "",
    FALSE, 0, 0, &VSA_Enable_Lib_Check, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "demo",                    "",
    FALSE, 0, 0, &VSA_Demo, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "cd_filter",               "",
    FALSE, 0, 0, &VSA_Cd_Filter, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "experimental",            "exp",
    FALSE, 0, 0, &VSA_Experimental, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "new_npd_checker",         "new_npd",
    FALSE, 0, 0, &VSA_New_Npd_Checker, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "new_uiv_checker",         "new_uiv",
    FALSE, 0, 0, &VSA_New_Uiv_Checker, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "ncp_new_cprop",           "ncp",
    FALSE, 0, 0, &VSA_New_Cprop, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "prop_value",              "prop_value",
    FALSE, 0, 0, &VSA_Prop_Values, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "cselect_new_lower",       "cselect",
    FALSE, 0, 0, &VSA_New_Cselect_Lower, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "value_graph",             "",
    FALSE, 0, 0, &VSA_Value_Graph, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "vg_lazy",                 "",
    FALSE, 0, 0, &VSA_Value_Graph_Lazy, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "check_main_entry",        "check_main",
    FALSE, 0, 0, &VSA_Check_Main_Entry, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "java_tmp_opt_for_test",   "java_tmp",
    FALSE, 0, 0, &VSA_Java_Tmp, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "check_devirt",            "check_devirt",
    FALSE, 0, 0, &VSA_Check_Devirt, NULL },
  { OVK_INT32,	OV_INTERNAL,	FALSE, "cmptime",                 "cmpt",
    16, 1, 40,  &VSA_Compiletime_Triage,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "model_lda",               "",
    FALSE, 0, 0, &VSA_Model_Lda, NULL },
  { OVK_BOOL,   OV_INTERNAL,    FALSE, "maybe_issue_report",      "maybe",
    FALSE, 0, 0, &VSA_Maybe_Report, NULL },
  { OVK_NAME,	OV_INTERNAL,	FALSE, "magic",	                  "",
    0, 0, 0,	&VSA_Magic_Id,	NULL },
  { OVK_BOOL,   OV_INTERNAL,    FALSE, "ignore_duplicate_result", "ignore_dup",
    FALSE, 0, 0, &VSA_Ignore_Dup, NULL },
  { OVK_NAME,	OV_INTERNAL,	FALSE, "token",	                  "",
    0, 0, 0,	&VSA_Authen_Token,	NULL },
  { OVK_NAME,	OV_INTERNAL,	FALSE, "server",                  "",
    0, 0, 0,	&VSA_Authen_Server,	NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "sr_single_report",        "sr",
    FALSE, 0, 0, &VSA_Single_Report,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "json_output_json",        "json",
    FALSE, 0, 0, &VSA_Output_Json,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "cbor_output_cbor",        "cbor",
    FALSE, 0, 0, &VSA_Output_Cbor,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "msgid_output_msgid",      "msgid",
    FALSE, 0, 0, &VSA_Output_Msgid, NULL },
  { OVK_INT32,	OV_INTERNAL,	FALSE, "jver_json_version",       "jver",
    1, 0, 999,  &VSA_Json_Version,  NULL },
  { OVK_INT32,	OV_INTERNAL,	FALSE, "jpath_json_path",         "jpath",
    30, 0, 200,  &VSA_Json_Path_Max,  NULL },
  { OVK_INT32,	OV_INTERNAL,	FALSE, "path_head",               "",
    16, 1, 4000,  &VSA_Path_Head_Max,  NULL },
  { OVK_INT32,	OV_INTERNAL,	FALSE, "path_tail",               "",
    24, 1, 8000,  &VSA_Path_Tail_Max,  NULL },
  { OVK_INT32,	OV_INTERNAL,	FALSE, "dumx_du_mem_high",        "dumx",
    512, 1, 16384,  &VSA_Du_Mem_High,  NULL },
  { OVK_INT32,	OV_INTERNAL,	FALSE, "dums_du_mem_low",         "dums",
    64, 1, 4096,  &VSA_Du_Mem_Low,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "emit_whirl",              "emit",
    FALSE, 0, 0, &VSA_Emit_Whirl,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "jni",                     "",
    FALSE, 0, 0, &VSA_Builtin_Jni, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "cxx_intrn",               "",
    FALSE, 0, 0, &VSA_CXX_Intrn, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "checker_se",              "",
    FALSE, 0, 0, &VSA_Checker_SE, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "check_recursion",         "",
    FALSE, 0, 0, &VSA_Builtin_Recursion, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "certc",                   "",
    FALSE, 0, 0, &VSA_Builtin_CertC, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "certj",                   "",
    FALSE, 0, 0, &VSA_Builtin_CertJ, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "rexc",                    "",
    FALSE, 0, 0, &VSA_RBC_Exception, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "fsm",                     "",
    FALSE, 0, 0, &VSA_Enable_FSM, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "old_tag",                 "",
    FALSE, 0, 0, &VSA_Enable_TAG_OLD, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "tag",                     "",
    FALSE, 0, 0, &VSA_Enable_TAG, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "prop_tag",                "",
    FALSE, 0, 0, &VSA_Enable_Prop_Tag, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "chi_prop_tag",            "",
    FALSE, 0, 0, &VSA_Enable_Chi_Prop_Tag, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "may_tag",                 "",
    FALSE, 0, 0, &VSA_Enable_May_Tag, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "taint_model",             "",
    FALSE, 0, 0, &VSA_Enable_Taint_Model, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "ignore_asm",              "",
    FALSE, 0, 0, &VSA_Ignore_Asm, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "check_rt_version",        "",
    FALSE, 0, 0, &VSA_Check_RT_version, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "authen",                  "",
    FALSE, 0, 0, &VSA_Authen_Enabled, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "load_all_sym",            "",
    FALSE, 0, 0, &VSA_Load_All_Sym, NULL },
#ifdef Is_True_On
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "nopatch",                 "",
    FALSE, 0, 0, &VSA_Authen_Skip_Patch, NULL },
#endif
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "validtime",               "",
    TRUE, 0, 0, &VSA_TCheck, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "optseed",                 "",
    TRUE, 0, 0, &VSA_OReload, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "dump_intrn",              "",
    TRUE, 0, 0, &VSA_Dump_Intrn_Table, NULL },
  { OVK_LIST,	OV_INTERNAL,	FALSE, "regmap",                  "",
    0, 0, 0, &VSA_Reg_Map, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "java_anno",               "",
    FALSE, 0, 0, &VSA_Enable_Java_Anno, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "filterincl",              "",
    FALSE, 0, 0, &VSA_Filter_Include, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "alertpath",               "",
    FALSE, 0, 0, &VSA_Alert_Huge_Path, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "checker_aggr",            "",
    TRUE, 0, 0, &VSA_Checker_Aggr, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "vfr_exact_match",         "vfr_exact",
    FALSE, 0, 0, &VSA_Checker_Vfr_Exact_Match, NULL },
  { OVK_INT32,	OV_INTERNAL,	FALSE, "chk_max_callby",        "",
    32, -1024, 1024, &VSA_Checker_Max_Callby, NULL },
  { OVK_INT32,	OV_INTERNAL,	FALSE, "chk_max_callee",        "",
    32, -1024, 1024, &VSA_Checker_Max_Callee, NULL },
  { OVK_INT32,	OV_INTERNAL,	FALSE, "chk_max_frame",         "",
    128, -1024, 1024, &VSA_Checker_Max_Frame, NULL },
  { OVK_INT32,	OV_INTERNAL,	FALSE, "chk_max_spos_chld",     "",
    3000, -1024, 409600, &VSA_Checker_Max_Srcpos_Child, NULL },
  { OVK_INT32,	OV_INTERNAL,	FALSE, "chk_max_spos_data",     "",
    128, -1024, 1024, &VSA_Checker_Max_Srcpos_Data, NULL },
  { OVK_INT32,	OV_INTERNAL,	FALSE, "chk_max_path",          "",
    32, -1024, 1024, &VSA_Checker_Max_Path, NULL },
  { OVK_INT32,	OV_INTERNAL,	FALSE, "css_max",               "",
    0, 0, 0xFFFF, &VSA_Call_Stack_Size_Max, NULL },
  { OVK_INT32,	OV_INTERNAL,	FALSE, "csl_max",               "",
    0, 0, 0xFFFF, &VSA_Call_Stack_Level_Max, NULL },

  { OVK_INT32,	OV_INTERNAL,	FALSE, "icm",                   "",
    1, 0, 2, &VSA_Issue_Certainty_Maybe, NULL },

  { OVK_NAME,	OV_INTERNAL,	FALSE, "vcg_cg",                "",
    0, 0, 0,	&VSA_VCG_Cg_Fname, NULL },

  { OVK_INT32,	OV_INTERNAL,	FALSE, "env",                   "",
    0, 0, 0xFFFF, &VSA_Target_Env, NULL },

    { OVK_COUNT }		/* List terminator -- must be last */
};
