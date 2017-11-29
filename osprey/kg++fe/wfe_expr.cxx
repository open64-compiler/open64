/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright (C) 2007, 2008 Pathscale, LLC.  All Rights Reserved.
 */

/*
 * Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.
 */

/* 
   Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
   File modified October 9, 2003 by PathScale, Inc. to update Open64 C/C++ 
   front-ends to GNU 3.3.1 release.
 */


/* 
   Copyright (C) 2002 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


#include <values.h>
#include "defs.h"
#include "glob.h"
#include "config.h"
#if defined(TARG_X8664) || defined(TARG_LOONGSON)
#include "config_opt.h"
#endif
#ifdef TARG_SL
#include <cmplrs/rcodes.h>
#endif
#include "wn.h"
#include "wn_util.h"
#include "targ_sim.h"
#include "const.h"
#include "c_int_model.h"

extern "C" {
#include "gnu_config.h"
}
#include "gnu/system.h"

extern "C" {
#include "gnu/flags.h"
#include "gnu/machmode.h"
}

extern "C" {
#include "gnu/system.h"
#include "gnu/tree.h"
extern void warning (char*,...);	// from gnu
#ifdef GPLUSPLUS_FE
#include "gnu/cp/cp-tree.h"
#endif /* GPLUSPLUS_FE */
#ifdef KEY
#include "real.h"
#endif // KEY
}

#include "ir_reader.h"
#include "wfe_misc.h"
#include "wfe_dst.h"
#include "tree_symtab.h"
#include "wfe_decl.h"
#include "wfe_expr.h"
#include "wfe_stmt.h"
#include "tree_cmp.h"

// #define WFE_DEBUG
#ifdef KEY
extern void WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_ID, ST *);
#endif // KEY

LABEL_IDX loop_expr_exit_label = 0; // exit label for LOOP_EXPRs

struct operator_from_tree_t {
  int      tree_code;
  char*    name;
  char     code;
  int      nkids;
  OPERATOR opr;
} Operator_From_Tree [] = {
  ERROR_MARK,              "error_mark",              'x', 0,  OPERATOR_UNKNOWN,
  IDENTIFIER_NODE,         "identifier_node",         'x', -1, OPERATOR_UNKNOWN,
  OP_IDENTIFIER,           "op_identifier",           'x', 2,  OPERATOR_UNKNOWN,
  TREE_LIST,               "tree_list",               'x', 2,  OPERATOR_UNKNOWN,
  TREE_VEC,                "tree_vec",                'x', 2,  OPERATOR_UNKNOWN,
  BLOCK,                   "block",                   'b', 0,  OPERATOR_UNKNOWN,
  VOID_TYPE,               "void_type",               't', 0,  OPERATOR_UNKNOWN,
  INTEGER_TYPE,            "integer_type",            't', 0,  OPERATOR_UNKNOWN,
  REAL_TYPE,               "real_type",               't', 0,  OPERATOR_UNKNOWN,
  COMPLEX_TYPE,            "complex_type",            't', 0,  OPERATOR_UNKNOWN,
  VECTOR_TYPE,             "vector_type",             't', 0,  OPERATOR_UNKNOWN,
  ENUMERAL_TYPE,           "enumeral_type",           't', 0,  OPERATOR_UNKNOWN,
  BOOLEAN_TYPE,            "boolean_type",            't', 0,  OPERATOR_UNKNOWN,
  CHAR_TYPE,               "char_type",               't', 0,  OPERATOR_UNKNOWN,
  POINTER_TYPE,            "pointer_type",            't', 0,  OPERATOR_UNKNOWN,
  OFFSET_TYPE,             "offset_type",             't', 0,  OPERATOR_UNKNOWN,
  REFERENCE_TYPE,          "reference_type",          't', 0,  OPERATOR_UNKNOWN,
  METHOD_TYPE,             "method_type",             't', 0,  OPERATOR_UNKNOWN,
  FILE_TYPE,               "file_type",               't', 0,  OPERATOR_UNKNOWN,
  ARRAY_TYPE,              "array_type",              't', 0,  OPERATOR_UNKNOWN,
  SET_TYPE,                "set_type",                't', 0,  OPERATOR_UNKNOWN,
  RECORD_TYPE,             "record_type",             't', 0,  OPERATOR_UNKNOWN,
  UNION_TYPE,              "union_type",              't', 0,  OPERATOR_UNKNOWN,
  QUAL_UNION_TYPE,         "qual_union_type",         't', 0,  OPERATOR_UNKNOWN,
  FUNCTION_TYPE,           "function_type",           't', 0,  OPERATOR_UNKNOWN,
  LANG_TYPE,               "lang_type",               't', 0,  OPERATOR_UNKNOWN,
  INTEGER_CST,             "integer_cst",             'c', 2,  OPERATOR_UNKNOWN,
  REAL_CST,                "real_cst",                'c', 3,  OPERATOR_UNKNOWN,
  COMPLEX_CST,             "complex_cst",             'c', 3,  OPERATOR_UNKNOWN,
  VECTOR_CST,              "vector_cst",              'c', 3,  OPERATOR_UNKNOWN,
  STRING_CST,              "string_cst",              'c', 3,  OPERATOR_UNKNOWN,
  FUNCTION_DECL,           "function_decl",           'd', 0,  OPERATOR_UNKNOWN,
  LABEL_DECL,              "label_decl",              'd', 0,  OPERATOR_UNKNOWN,
  CONST_DECL,              "const_decl",              'd', 0,  OPERATOR_UNKNOWN,
  TYPE_DECL,               "type_decl",               'd', 0,  OPERATOR_UNKNOWN,
  VAR_DECL,                "var_decl",                'd', 0,  OPERATOR_UNKNOWN,
  PARM_DECL,               "parm_decl",               'd', 0,  OPERATOR_UNKNOWN,
  RESULT_DECL,             "result_decl",             'd', 0,  OPERATOR_UNKNOWN,
  FIELD_DECL,              "field_decl",              'd', 0,  OPERATOR_UNKNOWN,
  NAMESPACE_DECL,          "namespace_decl",          'd', 0,  OPERATOR_UNKNOWN,
  COMPONENT_REF,           "component_ref",           'r', 2,  OPERATOR_UNKNOWN,
  BIT_FIELD_REF,           "bit_field_ref",           'r', 3,  OPERATOR_UNKNOWN,
  INDIRECT_REF,            "indirect_ref",            'r', 1,  OPERATOR_UNKNOWN,
  BUFFER_REF,              "buffer_ref",              'r', 1,  OPERATOR_UNKNOWN,
  ARRAY_REF,               "array_ref",               'r', 2,  OPERATOR_UNKNOWN,
  ARRAY_RANGE_REF,         "array_range_ref",         'r', 2,  OPERATOR_UNKNOWN,
  VTABLE_REF,              "vtable_ref",              'r', 3,  OPERATOR_UNKNOWN,
  CONSTRUCTOR,             "constructor",             'e', 2,  OPERATOR_UNKNOWN,
  COMPOUND_EXPR,           "compound_expr",           'e', 2,  OPERATOR_UNKNOWN,
  MODIFY_EXPR,             "modify_expr",             'e', 2,  OPERATOR_UNKNOWN,
  INIT_EXPR,               "init_expr",               'e', 2,  OPERATOR_UNKNOWN,
  TARGET_EXPR,             "target_expr",             'e', 4,  OPERATOR_UNKNOWN,
  COND_EXPR,               "cond_expr",               'e', 3,  OPERATOR_UNKNOWN,
  BIND_EXPR,               "bind_expr",               'e', 3,  OPERATOR_UNKNOWN,
  CALL_EXPR,               "call_expr",               'e', 3,  OPERATOR_UNKNOWN,
  METHOD_CALL_EXPR,        "method_call_expr",        'e', 4,  OPERATOR_UNKNOWN,
  WITH_CLEANUP_EXPR,       "with_cleanup_expr",       'e', 3,  OPERATOR_UNKNOWN,
  CLEANUP_POINT_EXPR,      "cleanup_point_expr",      'e', 1,  OPERATOR_UNKNOWN,
  PLACEHOLDER_EXPR,        "placeholder_expr",        'x', 0,  OPERATOR_UNKNOWN,
  WITH_RECORD_EXPR,        "with_record_expr",        'e', 2,  OPERATOR_UNKNOWN,
  PLUS_EXPR,               "plus_expr",               '2', 2,  OPR_ADD,
  MINUS_EXPR,              "minus_expr",              '2', 2,  OPR_SUB,
  MULT_EXPR,               "mult_expr",               '2', 2,  OPR_MPY,
  TRUNC_DIV_EXPR,          "trunc_div_expr",          '2', 2,  OPR_DIV,
  CEIL_DIV_EXPR,           "ceil_div_expr",           '2', 2,  OPR_DIV,
  FLOOR_DIV_EXPR,          "floor_div_expr",          '2', 2,  OPERATOR_UNKNOWN,
  ROUND_DIV_EXPR,          "round_div_expr",          '2', 2,  OPERATOR_UNKNOWN,
  TRUNC_MOD_EXPR,          "trunc_mod_expr",          '2', 2,  OPR_REM,
  CEIL_MOD_EXPR,           "ceil_mod_expr",           '2', 2,  OPERATOR_UNKNOWN,
  FLOOR_MOD_EXPR,          "floor_mod_expr",          '2', 2,  OPERATOR_UNKNOWN,
  ROUND_MOD_EXPR,          "round_mod_expr",          '2', 2,  OPERATOR_UNKNOWN,
  RDIV_EXPR,               "rdiv_expr",               '2', 2,  OPR_DIV,
  EXACT_DIV_EXPR,          "exact_div_expr",          '2', 2,  OPR_DIV,
  FIX_TRUNC_EXPR,          "fix_trunc_expr",          '1', 1,  OPERATOR_UNKNOWN,
  FIX_CEIL_EXPR,           "fix_ceil_expr",           '1', 1,  OPERATOR_UNKNOWN,
  FIX_FLOOR_EXPR,          "fix_floor_expr",          '1', 1,  OPERATOR_UNKNOWN,
  FIX_ROUND_EXPR,          "fix_round_expr",          '1', 1,  OPERATOR_UNKNOWN,
  FLOAT_EXPR,              "float_expr",              '1', 1,  OPERATOR_UNKNOWN,
  EXPON_EXPR,              "expon_expr",              '2', 2,  OPERATOR_UNKNOWN,
  NEGATE_EXPR,             "negate_expr",             '1', 1,  OPR_NEG,
  MIN_EXPR,                "min_expr",                '2', 2,  OPR_MIN,
  MAX_EXPR,                "max_expr",                '2', 2,  OPR_MAX,
  ABS_EXPR,                "abs_expr",                '1', 1,  OPR_ABS,
  FFS_EXPR,                "ffs_expr",                '1', 1,  OPERATOR_UNKNOWN,
  LSHIFT_EXPR,             "lshift_expr",             '2', 2,  OPR_SHL,
  RSHIFT_EXPR,             "rshift_expr",             '2', 2,  OPERATOR_UNKNOWN,
  LROTATE_EXPR,            "lrotate_expr",            '2', 2,  OPR_RROTATE,
  RROTATE_EXPR,            "rrotate_expr",            '2', 2,  OPR_RROTATE,
  BIT_IOR_EXPR,            "bit_ior_expr",            '2', 2,  OPR_BIOR,
  BIT_XOR_EXPR,            "bit_xor_expr",            '2', 2,  OPR_BXOR,
  BIT_AND_EXPR,            "bit_and_expr",            '2', 2,  OPR_BAND,
  BIT_ANDTC_EXPR,          "bit_andtc_expr",          '2', 2,  OPERATOR_UNKNOWN,
  BIT_NOT_EXPR,            "bit_not_expr",            '1', 1,  OPR_BNOT,
  TRUTH_ANDIF_EXPR,        "truth_andif_expr",        'e', 2,  OPR_CAND,
  TRUTH_ORIF_EXPR,         "truth_orif_expr",         'e', 2,  OPR_CIOR,
  TRUTH_AND_EXPR,          "truth_and_expr",          'e', 2,  OPR_BAND,
  TRUTH_OR_EXPR,           "truth_or_expr",           'e', 2,  OPR_BIOR,
  TRUTH_XOR_EXPR,          "truth_xor_expr",          'e', 2,  OPR_BXOR,
  TRUTH_NOT_EXPR,          "truth_not_expr",          'e', 1,  OPR_LNOT,
  LT_EXPR,                 "lt_expr",                 '<', 2,  OPR_LT,
  LE_EXPR,                 "le_expr",                 '<', 2,  OPR_LE,
  GT_EXPR,                 "gt_expr",                 '<', 2,  OPR_GT,
  GE_EXPR,                 "ge_expr",                 '<', 2,  OPR_GE,
  EQ_EXPR,                 "eq_expr",                 '<', 2,  OPR_EQ,
  NE_EXPR,                 "ne_expr",                 '<', 2,  OPR_NE,
  UNORDERED_EXPR,          "unordered_expr",          '<', 2,  OPERATOR_UNKNOWN,
  ORDERED_EXPR,            "ordered_expr",            '<', 2,  OPERATOR_UNKNOWN,
  UNLT_EXPR,               "unlt_expr",               '<', 2,  OPERATOR_UNKNOWN,
  UNLE_EXPR,               "unle_expr",               '<', 2,  OPERATOR_UNKNOWN,
  UNGT_EXPR,               "ungt_expr",               '<', 2,  OPERATOR_UNKNOWN,
  UNGE_EXPR,               "unge_expr",               '<', 2,  OPERATOR_UNKNOWN,
  UNEQ_EXPR,               "uneq_expr",               '<', 2,  OPERATOR_UNKNOWN,
  IN_EXPR,                 "in_expr",                 '2', 2,  OPERATOR_UNKNOWN,
  SET_LE_EXPR,             "set_le_expr",             '<', 2,  OPERATOR_UNKNOWN,
  CARD_EXPR,               "card_expr",               '1', 1,  OPERATOR_UNKNOWN,
  RANGE_EXPR,              "range_expr",              '2', 2,  OPERATOR_UNKNOWN,
  CONVERT_EXPR,            "convert_expr",            '1', 1,  OPERATOR_UNKNOWN,
  NOP_EXPR,                "nop_expr",                '1', 1,  OPERATOR_UNKNOWN,
  NON_LVALUE_EXPR,         "non_lvalue_expr",         '1', 1,  OPERATOR_UNKNOWN,
  VIEW_CONVERT_EXPR,       "view_convert_expr",       '1', 1,  OPERATOR_UNKNOWN,
  SAVE_EXPR,               "save_expr",               'e', 3,  OPERATOR_UNKNOWN,
  UNSAVE_EXPR,             "unsave_expr",             'e', 1,  OPERATOR_UNKNOWN,
  RTL_EXPR,                "rtl_expr",                'e', 2,  OPERATOR_UNKNOWN,
  ADDR_EXPR,               "addr_expr",               'e', 1,  OPERATOR_UNKNOWN,
  REFERENCE_EXPR,          "reference_expr",          'e', 1,  OPERATOR_UNKNOWN,
  ENTRY_VALUE_EXPR,        "entry_value_expr",        'e', 1,  OPERATOR_UNKNOWN,
  FDESC_EXPR,              "fdesc_expr",              'e', 2,  OPERATOR_UNKNOWN,
  COMPLEX_EXPR,            "complex_expr",            '2', 2,  OPR_PAIR,
  CONJ_EXPR,               "conj_expr",               '1', 1,  OPERATOR_UNKNOWN,
  REALPART_EXPR,           "realpart_expr",           '1', 1,  OPR_FIRSTPART,
  IMAGPART_EXPR,           "imagpart_expr",           '1', 1,  OPR_SECONDPART,
  PREDECREMENT_EXPR,       "predecrement_expr",       'e', 2,  OPR_SUB,
  PREINCREMENT_EXPR,       "preincrement_expr",       'e', 2,  OPR_ADD,
  POSTDECREMENT_EXPR,      "postdecrement_expr",      'e', 2,  OPR_SUB,
  POSTINCREMENT_EXPR,      "postincrement_expr",      'e', 2,  OPR_ADD,
  VA_ARG_EXPR,             "va_arg_expr",             'e', 1,  OPERATOR_UNKNOWN,
  TRY_CATCH_EXPR,          "try_catch_expr",          'e', 2,  OPERATOR_UNKNOWN,
  TRY_FINALLY_EXPR,        "try_finally",             'e', 2,  OPERATOR_UNKNOWN,
  GOTO_SUBROUTINE_EXPR,    "goto_subroutine",         'e', 2,  OPERATOR_UNKNOWN,
  POPDHC_EXPR,             "popdhc_expr",             's', 0,  OPERATOR_UNKNOWN,
  POPDCC_EXPR,             "popdcc_expr",             's', 0,  OPERATOR_UNKNOWN,
  LABEL_EXPR,              "label_expr",              's', 1,  OPERATOR_UNKNOWN,
  GOTO_EXPR,               "goto_expr",               's', 1,  OPERATOR_UNKNOWN,
  RETURN_EXPR,             "return_expr",             's', 1,  OPERATOR_UNKNOWN,
  EXIT_EXPR,               "exit_expr",               's', 1,  OPERATOR_UNKNOWN,
  LOOP_EXPR,               "loop_expr",               's', 1,  OPERATOR_UNKNOWN,
  LABELED_BLOCK_EXPR,      "labeled_block_expr",      'e', 2,  OPERATOR_UNKNOWN,
  EXIT_BLOCK_EXPR,         "exit_block_expr",         'e', 2,  OPERATOR_UNKNOWN,
  EXPR_WITH_FILE_LOCATION, "expr_with_file_location", 'e', 3,  OPERATOR_UNKNOWN,
  SWITCH_EXPR,             "switch_expr",             'e', 2,  OPERATOR_UNKNOWN,
  EXC_PTR_EXPR,            "exc_ptr_expr",            'e', 0,  OPERATOR_UNKNOWN,
  LAST_AND_UNUSED_TREE_CODE,"last_and_unused_tree_code",0, 0,  OPERATOR_UNKNOWN,

  SRCLOC,                  "srcloc",                  'x', 2,  OPERATOR_UNKNOWN,
  SIZEOF_EXPR,             "sizeof_expr",             '1', 1,  OPERATOR_UNKNOWN,
  ARROW_EXPR,              "arrow_expr",              'e', 1,  OPERATOR_UNKNOWN,
  ALIGNOF_EXPR,            "alignof_expr",            '1', 1,  OPERATOR_UNKNOWN,
  EXPR_STMT,               "expr_stmt",               'e', 1,  OPERATOR_UNKNOWN,
  COMPOUND_STMT,           "compound_stmt",           'e', 1,  OPERATOR_UNKNOWN,
  DECL_STMT,               "decl_stmt",               'e', 1,  OPERATOR_UNKNOWN,
  IF_STMT,                 "if_stmt",                 'e', 3,  OPERATOR_UNKNOWN,
  FOR_STMT,                "for_stmt",                'e', 4,  OPERATOR_UNKNOWN,
  WHILE_STMT,              "while_stmt",              'e', 2,  OPERATOR_UNKNOWN,
  DO_STMT,                 "do_stmt",                 'e', 2,  OPERATOR_UNKNOWN,
  RETURN_STMT,             "return_stmt",             'e', 1,  OPERATOR_UNKNOWN,
  BREAK_STMT,              "break_stmt",              'e', 0,  OPERATOR_UNKNOWN,
  CONTINUE_STMT,           "continue_stmt",           'e', 0,  OPERATOR_UNKNOWN,
  SWITCH_STMT,             "switch_stmt",             'e', 2,  OPERATOR_UNKNOWN,
  GOTO_STMT,               "goto_stmt",               'e', 1,  OPERATOR_UNKNOWN,
  LABEL_STMT,              "label_stmt",              'e', 1,  OPERATOR_UNKNOWN,
  ASM_STMT,                "asm_stmt",                'e', 5,  OPERATOR_UNKNOWN,
  SCOPE_STMT,              "scope_stmt",              'e', 1,  OPERATOR_UNKNOWN,
  FILE_STMT,               "file_stmt",               'e', 1,  OPERATOR_UNKNOWN,
  CASE_LABEL,              "case_label",              'e', 2,  OPERATOR_UNKNOWN,
  STMT_EXPR,               "stmt_expr",               'e', 1,  OPERATOR_UNKNOWN,
  COMPOUND_LITERAL_EXPR,   "compound_literal_expr",   'e', 1,  OPERATOR_UNKNOWN,
  CLEANUP_STMT,            "cleanup_stmt",            'e', 2,  OPERATOR_UNKNOWN,
#ifdef KEY
  OMP_MARKER_STMT,         "omp_marker_stmt",         'e', 0,  OPERATOR_UNKNOWN,
#endif // KEY
  FREQ_HINT_STMT,               "freq_hint_stmt",              'e', 1,   OPERATOR_UNKNOWN,   
  LAST_C_TREE_CODE,        "last_c_tree_code",          0, 0,  OPERATOR_UNKNOWN,

#ifdef GPLUSPLUS_FE
  OFFSET_REF,              "offset_ref",              'r', 2,  OPERATOR_UNKNOWN,
  PTRMEM_CST,              "ptrmem_cst",              'c', 2,  OPERATOR_UNKNOWN,
  NEW_EXPR,                "nw_expr",                 'e', 3,  OPERATOR_UNKNOWN,
  VEC_NEW_EXPR,            "vec_nw_expr",             'e', 3,  OPERATOR_UNKNOWN,
  DELETE_EXPR,             "dl_expr",                 'e', 2,  OPERATOR_UNKNOWN,
  VEC_DELETE_EXPR,         "vec_dl_expr",             'e', 2,  OPERATOR_UNKNOWN,
  SCOPE_REF,               "scope_ref",               'r', 2,  OPERATOR_UNKNOWN,
  MEMBER_REF,              "member_ref",              'r', 2,  OPERATOR_UNKNOWN,
  TYPE_EXPR,               "type_expr",               'e', 1,  OPERATOR_UNKNOWN,
  AGGR_INIT_EXPR,          "aggr_init_expr",          'e', 3,  OPERATOR_UNKNOWN,
  THROW_EXPR,              "throw_expr",              'e', 1,  OPERATOR_UNKNOWN,
  EMPTY_CLASS_EXPR,        "empty_class_expr",        'e', 0,  OPERATOR_UNKNOWN,
#ifdef KEY
  BASELINK,		   "baselink",		      'e', 3,  OPERATOR_UNKNOWN,
#endif // KEY
  TEMPLATE_DECL,           "template_decl",           'd', 0,  OPERATOR_UNKNOWN,
  TEMPLATE_PARM_INDEX,     "template_parm_index",     'x', 0,  OPERATOR_UNKNOWN,
  TEMPLATE_TYPE_PARM,      "template_type_parm",      't', 0,  OPERATOR_UNKNOWN,
  TEMPLATE_TEMPLATE_PARM,  "template_template_parm",  't', 0,  OPERATOR_UNKNOWN,
  BOUND_TEMPLATE_TEMPLATE_PARM, "bound_template_template_parm", 't', 0, OPERATOR_UNKNOWN,
  TYPENAME_TYPE,           "typename_type",           't', 0,  OPERATOR_UNKNOWN,
  UNBOUND_CLASS_TEMPLATE,  "unbound_class_template",  't', 0,  OPERATOR_UNKNOWN,
  TYPEOF_TYPE,             "typeof_type",             't', 0,  OPERATOR_UNKNOWN,
  USING_DECL,              "using_decl",              'd', 0,  OPERATOR_UNKNOWN,
  USING_STMT,              "using_directive",         'e', 1,  OPERATOR_UNKNOWN,
#ifdef KEY
  DEFAULT_ARG,             "default_arg",             'x', 2,  OPERATOR_UNKNOWN,
#else
  DEFAULT_ARG,             "default_arg",             'c', 2,  OPERATOR_UNKNOWN,
#endif // KEY
  TEMPLATE_ID_EXPR,        "template_id_expr",        'e', 2,  OPERATOR_UNKNOWN,
#ifndef KEY	// CPLUS_BINDING does not exist
  CPLUS_BINDING,           "binding",                 'x', 2,  OPERATOR_UNKNOWN,
#endif // !KEY
  OVERLOAD,                "overload",                'x', 1,  OPERATOR_UNKNOWN,
  WRAPPER,                 "wrapper",                 'x', 1,  OPERATOR_UNKNOWN,
  LOOKUP_EXPR,             "lookup_expr",             'e', 1,  OPERATOR_UNKNOWN,
  MODOP_EXPR,              "modop_expr",              'e', 3,  OPERATOR_UNKNOWN,
  CAST_EXPR,               "cast_expr",               '1', 1,  OPERATOR_UNKNOWN,
  REINTERPRET_CAST_EXPR,   "reinterpret_cast_expr",   '1', 1,  OPERATOR_UNKNOWN,
  CONST_CAST_EXPR,         "const_cast_expr",         '1', 1,  OPERATOR_UNKNOWN,
  STATIC_CAST_EXPR,        "static_cast_expr",        '1', 1,  OPERATOR_UNKNOWN,
  DYNAMIC_CAST_EXPR,       "dynamic_cast_expr",       '1', 1,  OPERATOR_UNKNOWN,
  DOTSTAR_EXPR,            "dotstar_expr",            'e', 2,  OPERATOR_UNKNOWN,
  TYPEID_EXPR,             "typeid_expr",             'e', 1,  OPERATOR_UNKNOWN,
  PSEUDO_DTOR_EXPR,        "pseudo_dtor_expr",        'e', 3,  OPERATOR_UNKNOWN,
#ifndef KEY	// SUBOBJECT, CTOR_STMT removed
  SUBOBJECT,               "subobject",               'e', 1,  OPERATOR_UNKNOWN,
  CTOR_STMT,               "ctor_stmt",               'e', 0,  OPERATOR_UNKNOWN,
  CTOR_INITIALIZER,        "ctor_initializer",        'e', 2,  OPERATOR_UNKNOWN,
#else
  CTOR_INITIALIZER,        "ctor_initializer",        'e', 1,  OPERATOR_UNKNOWN,
#endif // !KEY
  RETURN_INIT,             "return_init",             'e', 2,  OPERATOR_UNKNOWN,
  TRY_BLOCK,               "try_block",               'e', 2,  OPERATOR_UNKNOWN,
  EH_SPEC_BLOCK,           "eh_spec_block",           'e', 2,  OPERATOR_UNKNOWN,
  HANDLER,                 "handler",                 'e', 2,  OPERATOR_UNKNOWN,
  MUST_NOT_THROW_EXPR,     "must_not_throw_expr",     'e', 1,  OPERATOR_UNKNOWN,
  TAG_DEFN,                "tag_defn",                'e', 0,  OPERATOR_UNKNOWN,
  IDENTITY_CONV,           "identity_conv",           'e', 1,  OPERATOR_UNKNOWN,
  LVALUE_CONV,             "lvalue_conv",             'e', 1,  OPERATOR_UNKNOWN,
  QUAL_CONV,               "qual_conv",               'e', 1,  OPERATOR_UNKNOWN,
  STD_CONV,                "std_conv",                'e', 1,  OPERATOR_UNKNOWN,
  PTR_CONV,                "ptr_conv",                'e', 1,  OPERATOR_UNKNOWN,
  PMEM_CONV,               "pmem_conv",               'e', 1,  OPERATOR_UNKNOWN,
  BASE_CONV,               "base_conv",               'e', 1,  OPERATOR_UNKNOWN,
  REF_BIND,                "ref_bind",                'e', 1,  OPERATOR_UNKNOWN,
  USER_CONV,               "user_conv",               'e', 2,  OPERATOR_UNKNOWN,
  AMBIG_CONV,              "ambig_conv",              'e', 1,  OPERATOR_UNKNOWN,
  RVALUE_CONV,             "rvalue_conv",             'e', 1,  OPERATOR_UNKNOWN,
  LAST_CPLUS_TREE_CODE,    "last_cplus_tree_code",     0,  0,  OPERATOR_UNKNOWN
#endif /* GPLUSPLUSFE */
};

#ifdef TARG_SL
/*
  *  some side-effect intrinsic op need to be extended 
  */
typedef enum EXTEND_PARM_POS {
 PZERO,   // no extend
 P0,
 P1,
 P2,
 P3,
 P4,
 P0_P2,
 P1_P3,
 P2_P4,	
 P3_P4,
}EXTEND_PARM_POS;

typedef struct intrinsicop_attr_extended {
  INTRINSIC id;
  BOOL need_extend; //
  int extend_kid;
  EXTEND_PARM_POS pos;
  INTRINSIC aux_id;   // extend intrinsic opr
} INTRN_ATTR_EXTEND; 

#define INTRN_EATTR_LAST 27	
static INTRN_ATTR_EXTEND intrn_eattr[INTRN_EATTR_LAST] = {
  INTRN_C2_LD_V,  FALSE, 1, P1,INTRN_C3_PTR,
  INTRN_C2_LD_G,    FALSE, 1, P0,INTRN_C3_PTR,
  INTRN_C2_LD_V2G,   FALSE, 1, P0,INTRN_C3_PTR,
  INTRN_C2_LD_C_IMM,     FALSE, 1, P1,INTRN_C3_PTR,
  INTRN_C2_LD_V_IMM,     FALSE, 1, P4,INTRN_C3_PTR,
  INTRN_C2_LD_G_IMM,     FALSE, 1, P2,INTRN_C3_PTR,
  INTRN_C2_LD_V2G_IMM,  FALSE, 1, P2,INTRN_C3_PTR,
  INTRN_C2_ST_V,    FALSE, 1, P1,INTRN_C3_PTR,
  INTRN_C2_ST_G,   FALSE, 1, P1,INTRN_C3_PTR,
  INTRN_C2_ST_G2V,  FALSE, 1, P1,INTRN_C3_PTR,
  INTRN_C2_ST_C_IMM,   FALSE, 1, P1,INTRN_C3_PTR,
  INTRN_C2_ST_V_IMM,   FALSE, 1, P3,INTRN_C3_PTR,
  INTRN_C2_ST_G_IMM,   FALSE, 1, P2,INTRN_C3_PTR,
  INTRN_C2_ST_G2V_IMM,  FALSE, 1, P2,INTRN_C3_PTR,
  // new C3
  INTRN_C3DMAC_A, TRUE, 2, P2_P4, INTRN_C3_PTR,
  INTRN_C3DMULA_A, TRUE, 1, P2, INTRN_C3_PTR,
  INTRN_C3LD, TRUE, 1, P0, INTRN_C3_PTR,
  INTRN_C3ST, TRUE, 1, P1, INTRN_C3_PTR,
  INTRN_C3MAC_A, TRUE, 2, P2_P4, INTRN_C3_PTR,
  INTRN_C3MAC_AR, TRUE, 1, P3, INTRN_C3_PTR,
  INTRN_C3MULA_A, TRUE, 1, P2_P4, INTRN_C3_PTR,
  INTRN_C3MULA_AR, TRUE, 1, P3, INTRN_C3_PTR,
  INTRN_C3SAADD_A, TRUE, 2, P0_P2, INTRN_C3_PTR,
  INTRN_C3SAADDH_A, TRUE, 2, P0_P2, INTRN_C3_PTR,
  INTRN_C3SADDA_A, TRUE, 1, P2, INTRN_C3_PTR,
  INTRN_C3SAMULH_A, TRUE, 2, P0_P2, INTRN_C3_PTR,
  INTRN_C3_SET_CIRCBUF, FALSE, 2, P3_P4, INTRN_C3_PTR,
};

static BOOL intrinsic_op_need_extend (INTRINSIC id) {

  INTRN_ATTR_EXTEND *p = &intrn_eattr[0];
  int i=0;
  while (p && (i<INTRN_EATTR_LAST)) {
  	if (p->id == id ) {
		return p->need_extend;
  	}
	i++;
	p++;
  }
  return FALSE;
  
}

static BOOL intrinsic_need_deref (INTRINSIC id) {

  INTRN_ATTR_EXTEND *p = &intrn_eattr[0];
  int i=0; 
  while (p && (i<INTRN_EATTR_LAST)) {
        if (p->id == id ) {
                return TRUE;
        }
        i++;
        p++;
  }
  return FALSE;

}

static INTRN_ATTR_EXTEND *Get_intrinsic_op_Eattr (INTRINSIC id) {
  INTRN_ATTR_EXTEND *p = &intrn_eattr[0];
  int i =0;
  while (p && (i<INTRN_EATTR_LAST)) {
  	if (p->id == id ) {
		return p;
  	}
	p++;
	i++;
  }
  return NULL; 	
}

static void WN_Set_Deref_If_Needed(WN *wn) {
	INTRINSIC intrn=WN_intrinsic(wn);
        if(intrinsic_need_deref(intrn)){
                INTRN_ATTR_EXTEND *p=Get_intrinsic_op_Eattr(intrn);

                switch (p->pos) {
                        case P0:
                                WN_Set_Parm_Dereference(WN_kid(wn,0));  break;
                        case P1:
                                WN_Set_Parm_Dereference(WN_kid(wn,1));  break;
                        case P2:
                                WN_Set_Parm_Dereference(WN_kid(wn,2));  break;
                        case P3:
                                WN_Set_Parm_Dereference(WN_kid(wn,3));  break;
			case P4:
				WN_Set_Parm_Dereference(WN_kid(wn,4));  break;
                        case P0_P2:
                                WN_Set_Parm_Dereference(WN_kid(wn,0)); WN_Set_Parm_Dereference(WN_kid(wn,2)); break;
                        case P1_P3:
                                WN_Set_Parm_Dereference(WN_kid(wn,1)); WN_Set_Parm_Dereference(WN_kid(wn,3)); break;
                        case P2_P4:
                                WN_Set_Parm_Dereference(WN_kid(wn,2)); WN_Set_Parm_Dereference(WN_kid(wn,4)); break;
                        case P3_P4:
                                WN_Set_Parm_Dereference(WN_kid(wn,3)); WN_Set_Parm_Dereference(WN_kid(wn,4)); break;
                        default:
                                Is_True(0, ("intrinsic has no extended attribution"));
                }
        }
	return;
}

static int intrinsic_op_extend_kid (int index) {
   return intrn_eattr[index].extend_kid;	
}

static EXTEND_PARM_POS intrinsic_op_parm_pos (int index) {
   return intrn_eattr[index].pos;	
}

static BOOL WN_Need_Append_Intrinsic(WN *rhs) {
   OPERATOR opr=WN_operator(rhs);
   if (opr == OPR_INTRINSIC_OP || opr == OPR_INTRINSIC_CALL) {
  	INTRINSIC inid = WN_intrinsic(rhs);
	if (intrinsic_op_need_extend(inid)) {
		return TRUE;
	}
  } else  if (opr == OPR_CVTL) {
   	WN *tmp = WN_kid0(rhs);
	OPERATOR kid_opr=WN_operator(tmp);
	if (kid_opr == OPR_INTRINSIC_OP || kid_opr == OPR_INTRINSIC_CALL) {
  	  INTRINSIC inid = WN_intrinsic(tmp);
	  if (intrinsic_op_need_extend(inid)) {
		return TRUE;
	  }
	}
  } 
  return FALSE;
}
/*
 *  given a side effect intrinsic op:  sum = intrinsic_c3_mac_a(sum, p, 1, q, 1, 0) which doing sum=; p++; q++ 
 *  we need extend it to
 *   sum = intrinsic_c3_mac_a(sum, p, 1, q, 1, 0)
 *   p = intrinsic_c3_ptr(p, sum);  <-slave intrinsic op
 *   q = intrinsic_c3_ptr(q, sum);
*/
static void WFE_Stmt_Append_Extend_Intrinsic(WN *wn, WN *master_variable, SRCPOS src) {
   WN *kid1s[5];
   WN *op1;
   int aux_kid = -1;  // parameter numbers of slave intrinsic op
   int extend_num = -1;
   int pos[5]= {-1, -1, -1, -1, -1};
  
   WN *tmp_wn;
   INTRN_ATTR_EXTEND *p ;
   if (WN_operator(WN_kid0(wn)) == OPR_INTRINSIC_OP) {
     p = Get_intrinsic_op_Eattr(WN_intrinsic(WN_kid0(wn)));
     tmp_wn = wn;
   } else if (WN_operator(WN_kid0(wn)) == OPR_CVTL && WN_operator(WN_kid0(WN_kid0(wn))) == OPR_INTRINSIC_OP ) {
     p = Get_intrinsic_op_Eattr(WN_intrinsic(WN_kid0(WN_kid0(wn))));
     tmp_wn = WN_kid0(wn);
   } else if (WN_operator(wn) == OPR_INTRINSIC_CALL) {
     p = Get_intrinsic_op_Eattr(WN_intrinsic(wn));
     tmp_wn = wn;
   }
 
   if (p) {
   	extend_num = p->extend_kid;
       switch (p->aux_id) {
	   case INTRN_C3_PTR:  aux_kid = 2; break;
	   default:  
	   	Is_True(0, ("unsupport internal intrinsic op"));
       }
       switch (p->pos) {
	    case P0:
                pos[0] = 0; break;
	    case P1:
                pos[0] = 1; break;
	    case P2:
	   	pos[0] = 2;  break;
	    case P3:
		pos[0] = 3;  break;	
	    case P0_P2: 
	   	pos[0] = 0; pos[1] = 2; break;
	    case P1_P3:
	   	pos[0] = 1; pos[1] = 3; break;
	    case P2_P4:
	   	pos[0] = 2; pos[1] = 4; break;
	    case P3_P4:
	   	pos[0] = 3; pos[1] = 4; break;
	    default:
	   	Is_True(0, ("intrinsic has no extended attribution"));
       }
   } else {
     Is_True(0, ("intrinsic has no extended attribution"));
   }
   
   TY_IDX  ti2 = WN_ty(master_variable);
   TYPE_ID tm2 = WN_rtype(master_variable);
   master_variable = WN_CreateParm (Mtype_comparison (tm2), master_variable,
					  ti2, WN_PARM_BY_VALUE);
   kid1s[0]= master_variable;
   for (int i =0; i < extend_num; i++) {
     WN *op1;
     if (WN_operator(wn) == OPR_INTRINSIC_CALL) {
       op1 = WN_kid0(WN_kid(tmp_wn, pos[i]));
     } else {
       op1 = WN_kid0(WN_kid(WN_kid0(tmp_wn), pos[i]));
     }
     
     ST *st1;
     if (WN_has_sym(op1)) {
       st1 = WN_st(op1);
     } else {
       // parameter is an expression, don't extend it
       continue;
     }
     TY_IDX  ti1 = WN_ty(op1);
     TYPE_ID tm1 = WN_rtype(master_variable);
     op1 = WN_CreateParm (Mtype_comparison (tm1), op1,
			  ti1, WN_PARM_BY_VALUE);
     kid1s[1]= op1;
     WN *app1 = WN_Create_Intrinsic(OPR_INTRINSIC_OP,TY_mtype(ST_type(st1)), MTYPE_V, p->aux_id, aux_kid, kid1s);   
     WN *stmt1 = WN_Stid(TY_mtype(ST_type(st1)), ST_ofst(st1), st1, ST_type(st1), app1, 0);   
     WFE_Stmt_Append(stmt1, src);
  }
}

#endif


#ifdef KEY
static bool WFE_Call_Returns_Ptr_To_Member_Func (tree exp);

static WN *WFE_Expand_Ptr_To_Member_Func_Call_Expr (tree exp,
	     TY_IDX nop_ty_idx, TYPE_ID rtype, TYPE_ID desc,
	     WN_OFFSET offset = 0, UINT field_id = 0);

// The words in 'buf' are in target order. Convert them to host order
// in place. 'buf' is a two word array.
void
WFE_Convert_To_Host_Order (long *buf)
{
  if (!Same_Byte_Sex)
    {
      int t = buf[0];
      buf[0] = buf[1];
      buf[1] = t;
    }
}

// Add guard variable GUARD_VAR to a conditional expression that may or may
// not be evaluated, such as x and y in "foo ? x : y", or y in "if (x && y)".
// Transform the code to:
//   guard_var=0
//   foo ? (guard_var=1, x) : y		// assuming VALUE_WN is x
// and:
//   guard_var=0
//   if (x && (guard_var=1, y))
static void
WFE_add_guard_var (tree guard_var, WN *value_wn)
{
  WN *stid, *comma;

  // Set the guard variable to 0 before the condition is evaluated.
  WN *zero_wn = WN_Intconst(MTYPE_I4, 0);
  stid = WN_Stid(MTYPE_I4, 0, Get_ST(guard_var), MTYPE_To_TY(MTYPE_I4),
		 zero_wn, 0);
  WFE_Stmt_Append(stid, Get_Srcpos());

  // Set the guard variable to 1 while evaluating the value of the conditional
  // expression.
  WN *one_wn = WN_Intconst(MTYPE_I4, 1);
  stid = WN_Stid(MTYPE_I4, 0, Get_ST(guard_var), MTYPE_To_TY(MTYPE_I4),
		 one_wn, 0);
  if (WN_operator(value_wn) == OPR_COMMA) {
    comma = value_wn;
  } else if (WN_operator(WN_kid0(value_wn)) == OPR_COMMA) {
    comma = WN_kid0(value_wn);
  } else {
    // Create a comma.
    WN *wn0 = WN_CreateBlock();
    WN *wn1 = WN_kid0(value_wn);
    WN_Set_Linenum (wn0, Get_Srcpos());
    comma = WN_CreateComma (OPR_COMMA, WN_rtype(wn1), MTYPE_V, wn0, wn1);
    WN_kid0(value_wn) = comma;
  }
  WN *wn = WN_kid0(comma);
  FmtAssert(WN_operator(wn) == OPR_BLOCK,
    ("WFE_add_guard_var: unexpected WN operator"));
  WN_INSERT_BlockFirst(wn, stid);
}
#endif

// check whether the WHIRL operator has subsumed cvtl in its semantics
// (intended only for integer operations)
bool
Has_Subsumed_Cvtl(OPERATOR opr)
{
  if (OPERATOR_is_load(opr) || OPERATOR_is_leaf(opr))
    return TRUE;
  if (opr == OPR_CVT || opr == OPR_CVTL || opr == OPR_TAS)
    return TRUE;
  if (opr == OPR_EQ || opr == OPR_NE ||
      opr == OPR_GE || opr == OPR_GT ||
      opr == OPR_LE || opr == OPR_LT ||
      opr == OPR_LNOT || opr == OPR_LAND || opr == OPR_LIOR ||
      opr == OPR_CAND || opr == OPR_CIOR)
    return TRUE;
  return FALSE;
}

// Round up an object size to the size it would require in the parameter
// area on the stack.  This is defined to be the difference between its
// start address and the lowest possible starting address of the next parameter.
inline UINT64 Parameter_Size(UINT64 sz)
{
#   if WORDS_BIG_ENDIAN
	return sz;
#   else
	return (sz + UNITS_PER_WORD - 1) & ~(UNITS_PER_WORD - 1);
#   endif
}

inline TYPE_ID
Widen_Mtype (TYPE_ID t)
{
  if (MTYPE_is_m(t))
    return t;
  if (MTYPE_is_void(t) || t == MTYPE_BS) {
    Fail_FmtAssertion ("Widen_Mtype: for MTYPE_V or MTYPE_BS");
    return t;
  }
  if (MTYPE_byte_size(t) >= 4)
    return t;
  return Mtype_TransferSize(MTYPE_I4, t);
}

// Traverse the tree to see if the address of a variable is being taken

void
WFE_Set_ST_Addr_Saved (WN *wn)
{
  OPERATOR  Operator;
  ST       *st;

  Operator = WN_operator (wn);

  switch ( Operator ) {

    case OPR_LDA:
    case OPR_LDMA:

      st = WN_st (wn);

      if (ST_class(st) == CLASS_VAR || ST_class(st) == CLASS_FUNC)
        Set_ST_addr_saved (st);
      break;

    case OPR_ARRAY:

      WFE_Set_ST_Addr_Saved (WN_kid0 (wn));
      break;

    case OPR_LDID:

      st = WN_st (wn);
      if (ST_pt_to_unique_mem (st))
        Clear_ST_pt_to_unique_mem (st);
      break;

    case OPR_CONST:
    case OPR_ILOAD:
    case OPR_MLOAD:
    case OPR_INTCONST:
    case OPR_INTRINSIC_OP:
    case OPR_CALL:
    case OPR_EQ:
    case OPR_NE:
    case OPR_GT:
    case OPR_GE:
    case OPR_LT:
    case OPR_LE:
    case OPR_ALLOCA:
      break;

    case OPR_EVAL:
    case OPR_TAS:
    case OPR_CVT:
    case OPR_CVTL:
    case OPR_NEG:
    case OPR_ABS:
    case OPR_SQRT:
    case OPR_REALPART:
    case OPR_IMAGPART:
    case OPR_PAREN:
    case OPR_RND:
    case OPR_TRUNC:
    case OPR_CEIL:
    case OPR_FLOOR:
    case OPR_BNOT:
    case OPR_LNOT:
    case OPR_LOWPART:
    case OPR_HIGHPART:
    case OPR_MINPART:
    case OPR_MAXPART:
    case OPR_RECIP:
    case OPR_RSQRT:
    case OPR_PARM:
    case OPR_EXTRACT_BITS:

      WFE_Set_ST_Addr_Saved (WN_kid0(wn));
      break;

    case OPR_CSELECT:

      WFE_Set_ST_Addr_Saved (WN_kid1(wn));
      WFE_Set_ST_Addr_Saved (WN_kid2(wn));
      break;

    case OPR_SELECT:
    case OPR_ADD:
    case OPR_SUB:
    case OPR_MPY:
    case OPR_DIV:
    case OPR_MOD:
    case OPR_REM:
    case OPR_DIVREM:
    case OPR_MAX:
    case OPR_MIN:
    case OPR_MINMAX:
    case OPR_BAND:
    case OPR_BIOR:
    case OPR_BXOR:
    case OPR_BNOR:
    case OPR_LAND:
    case OPR_LIOR:
    case OPR_SHL:
    case OPR_ASHR:
    case OPR_LSHR:
    case OPR_COMPLEX:
    case OPR_HIGHMPY:
    case OPR_RROTATE:
    case OPR_COMPOSE_BITS:

      WFE_Set_ST_Addr_Saved (WN_kid0(wn));
      WFE_Set_ST_Addr_Saved (WN_kid1(wn));
      break;

    case OPR_CAND:
    case OPR_CIOR:

      break;

    case OPR_COMMA:

      WFE_Set_ST_Addr_Saved (WN_kid1(wn));
      break;

    case OPR_RCOMMA:

      WFE_Set_ST_Addr_Saved (WN_kid0(wn));
      break;

    default:

      DevWarn ("WFE_Set_ST_Addr_Saved not implemented");
  }
} /* WFE_Set_ST_Addr_Saved */

#ifndef GPLUSPLUS_FE
typedef struct wfe_bind_expr_t {
  tree  rtl_expr;
  WN   *block;
} WFE_BIND_EXPR;

WFE_BIND_EXPR *wfe_bind_expr_stack       = NULL;
INT32          wfe_bind_expr_stack_last  = -1;
INT32          wfe_bind_expr_stack_max   = 0;

void
WFE_Expand_Start_Stmt_Expr (tree t)
{
  WN *block = WN_CreateBlock ();
  WFE_Stmt_Push (block, wfe_stmk_comma, Get_Srcpos ());
} /* WFE_Start_Stmt_Expr */

void
WFE_Expand_End_Stmt_Expr (tree t)
{
  WN *block = WFE_Stmt_Pop (wfe_stmk_comma);
  ++wfe_bind_expr_stack_last;
  if (wfe_bind_expr_stack_last == wfe_bind_expr_stack_max) {
    if (wfe_bind_expr_stack == NULL) {
      wfe_bind_expr_stack_max = 32;
      wfe_bind_expr_stack     =
        (WFE_BIND_EXPR *) malloc (wfe_bind_expr_stack_max *
                                  sizeof (WFE_BIND_EXPR));
    }
    else {
      wfe_bind_expr_stack_max = wfe_bind_expr_stack_max +
                                (wfe_bind_expr_stack_max >> 1);
      wfe_bind_expr_stack     =
        (WFE_BIND_EXPR *) realloc (wfe_bind_expr_stack,
                                   wfe_bind_expr_stack_max *
                                   sizeof (WFE_BIND_EXPR));
    }
  }
  wfe_bind_expr_stack [wfe_bind_expr_stack_last].rtl_expr = t;
  wfe_bind_expr_stack [wfe_bind_expr_stack_last].block    = block;
} /* WFE_End_Stmt_Expr */
#endif /* GPLUSPLUS_FE */

typedef struct wfe_save_expr_t {
  tree  exp;
  ST   *st;
#ifdef KEY
  INT32  level;	// to identify which cleanup the save expr belongs to
#endif
} WFE_SAVE_EXPR;

WFE_SAVE_EXPR *wfe_save_expr_stack      = NULL;
INT32          wfe_save_expr_stack_last = -1;
INT32          wfe_save_expr_stack_max  = 0;

#ifdef KEY
INT32 wfe_save_expr_level;	// identify the current cleanup
INT32 wfe_last_save_expr_level;	// the last cleanup level used
#endif

static WN*
WFE_Save_Expr (tree save_exp,
               bool need_result,
               TY_IDX nop_ty_idx,
               TY_IDX component_ty_idx,
               INT64 component_offset,
               UINT16 field_id)
{
  INT32     i;
  tree      exp     = TREE_OPERAND (save_exp, 0);
  TY_IDX    ty_idx  = Get_TY (TREE_TYPE (exp));
  TYPE_ID   mtype   = TY_mtype (ty_idx);
  ST       *st;
  WN       *wn;
  bool     found = false;  

  for (i = wfe_save_expr_stack_last; i >= 0; i--) {
#ifndef KEY
    if (wfe_save_expr_stack [i].exp == exp) {
#else
    if (wfe_save_expr_stack [i].exp == save_exp &&
    	wfe_save_expr_stack [i].level == wfe_save_expr_level) {
#endif
      st = wfe_save_expr_stack [i].st;
      FmtAssert (st != 0,
                 ("WFE_Save_Expr: st not yet assigned"));
      found = true;
      break;
    }
  }
  
  if (!found) {
    i = ++wfe_save_expr_stack_last;
    if (i == wfe_save_expr_stack_max) {
      if (wfe_save_expr_stack == NULL) {
        wfe_save_expr_stack_max = 32;
        wfe_save_expr_stack     =
          (WFE_SAVE_EXPR *) malloc (wfe_save_expr_stack_max *
                                    sizeof (WFE_SAVE_EXPR));
      }
      else {
        wfe_save_expr_stack_max = wfe_save_expr_stack_max +
                                  (wfe_save_expr_stack_max >> 1);
        wfe_save_expr_stack     =
          (WFE_SAVE_EXPR *) realloc (wfe_save_expr_stack,
                                     wfe_save_expr_stack_max *
                                     sizeof (WFE_SAVE_EXPR));
      }
    }
#ifndef KEY
    wfe_save_expr_stack [i].exp = exp;
#else
    wfe_save_expr_stack [i].exp = save_exp;
    wfe_save_expr_stack [i].level = wfe_save_expr_level;
#endif
    wfe_save_expr_stack [i].st  = 0;
#ifdef KEY
    // If exp is a CALL_EXPR that returns a ptr-to-member-function, then call
    // WFE_Expand_Ptr_To_Member_Func_Call_Expr to expand it.  Otherwise, call
    // WFE_Expand_Expr to do regular expansion.  Bug 3400.
    if (WFE_Call_Returns_Ptr_To_Member_Func(exp)) {
      TYPE_ID desc = TY_mtype(Get_TY(TREE_TYPE(exp)));
      wn = WFE_Expand_Ptr_To_Member_Func_Call_Expr(exp, nop_ty_idx,
						   Widen_Mtype(desc), desc);
    } else
#endif
    wn = WFE_Expand_Expr (exp);

    st = Gen_Temp_Symbol (ty_idx, "__save_expr");
#ifdef KEY
    WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, st);
#endif
    WFE_Set_ST_Addr_Saved (wn);
    wn = WN_Stid (mtype, 0, st, ty_idx, wn);
    WFE_Stmt_Append (wn, Get_Srcpos());
    wfe_save_expr_stack [i].st = st;
  }

  if (component_ty_idx == 0)
    wn = WN_Ldid (mtype, 0, st, ty_idx);
  else {
    TYPE_ID desc  = TY_mtype(component_ty_idx);
    TYPE_ID rtype = Widen_Mtype(desc);
    wn = WN_CreateLdid(OPR_LDID, rtype, desc, component_offset, st,
		     field_id? ty_idx : component_ty_idx, field_id);  
  }
  return wn;
} /* WFE_Save_Expr */

/* process the tree doing array indicing and return the WN that performs
 * the address computation; ty_idx returns the high-level array type if it
 * is a DECL node, and the element type if it is an ARRAY_REF node.
 */
static WN *
WFE_Array_Expr(tree exp, 
	       TY_IDX *ty_idx, 
	       TY_IDX component_ty_idx,
	       INT64 component_offset,
	       UINT32 field_id)
{
  WN *wn;
  enum tree_code code = TREE_CODE (exp);
  if (code == COMPONENT_REF) {
    TY_IDX ty_idx0;
    tree arg0 = TREE_OPERAND(exp, 0); 
    tree arg1 = TREE_OPERAND(exp, 1); 
    if (component_ty_idx == 0)
      ty_idx0 = Get_TY(TREE_TYPE(exp));
    else ty_idx0 = component_ty_idx;
#ifdef KEY // bug 10728
    if (TREE_THIS_VOLATILE(exp))
      Set_TY_is_volatile(ty_idx0);
#endif
    Is_True(! DECL_BIT_FIELD(arg1),
	    ("WFE_Array_Expr: address arithmetic cannot use bitfield addresses"));
    INT64 ofst = (BITSPERBYTE * Get_Integer_Value(DECL_FIELD_OFFSET(arg1)) +
				Get_Integer_Value(DECL_FIELD_BIT_OFFSET(arg1)))
			      / BITSPERBYTE;
#ifdef KEY
    // OSP_7, MODIFY_EXPR in ARRAY_REF
    // Refer GCC 4.0.2: gcc.c-torture/compile/struct-non-lval-3.c
    // We only handle this case so far:
    // (p = q).x[index]
    // the lhs of modify_expr is var_decl, not an expression
    // ARRAY_REF
    //     |---> MODIFY_EXPT
    if (TREE_CODE(arg0) == MODIFY_EXPR) {
      WFE_Expand_Expr(arg0);
      tree lhs = TREE_OPERAND(arg0, 0);
      Is_True (lhs != NULL && 
	       (TREE_CODE(lhs) == VAR_DECL || TREE_CODE(lhs) == INDIRECT_REF),
		      ("Unsupported lhs for `(p=q).x[n]'"));
      arg0 = lhs;
    }
#endif

#ifdef KEY // bug 9725: If the field is an array of struct, it is considered
           // a single field.
    return WFE_Array_Expr(arg0, ty_idx, ty_idx0, ofst + component_offset,
			  DECL_FIELD_ID(arg1));
#else
    return WFE_Array_Expr(arg0, ty_idx, ty_idx0, ofst + component_offset,
			  field_id + DECL_FIELD_ID(arg1));
#endif
  }
  else if (code == VAR_DECL || code == PARM_DECL) {
    ST *st = Get_ST (exp);
    ST *base_st = ST_base (st);
    // for VLAs the instead of using the ST use its base st
    // also for the time being do not support VLAs within structs
    if (st != base_st) {
      FmtAssert (component_ty_idx == 0,
                 ("Variable Length Arrays within struct not currently implemented"));
      wn = WN_Ldid (Pointer_Mtype, 0, base_st, ST_type (base_st));
    }
    else
      wn = WN_Lda (Pointer_Mtype, ST_ofst(st)+component_offset, st, field_id);
    if (component_ty_idx == 0)
      *ty_idx = ST_type(st);
    else {
      *ty_idx = component_ty_idx;
      if (TY_align(ST_type(st)) < TY_align(component_ty_idx))
	Set_TY_align(*ty_idx, TY_align(ST_type(st)));//pick more stringent align
    }
    Is_True(TY_kind(*ty_idx) == KIND_ARRAY,
	    ("WFE_Array_Expr: ARRAY_REF base not of type KIND_ARRAY"));
    return wn;
  }
  else if (code == CONSTRUCTOR) {
    ST *st = WFE_Generate_Temp_For_Initialized_Aggregate (exp, "");
    wn = WN_Lda (Pointer_Mtype, ST_ofst(st)+component_offset, st, field_id);
    if (component_ty_idx == 0)
      *ty_idx = ST_type(st);
    else *ty_idx = component_ty_idx;
    Is_True(TY_kind(*ty_idx) == KIND_ARRAY,
	    ("WFE_Array_Expr: ARRAY_REF base not of type KIND_ARRAY"));
    return wn;
  }
  else if (code == STRING_CST) {
    wn = WFE_Expand_Expr(exp);
    *ty_idx = ST_type (TREE_STRING_ST (exp));
    return wn;
  }
  else if (code == INDIRECT_REF) {
    wn = WFE_Expand_Expr(TREE_OPERAND (exp, 0));
    if (component_ty_idx == 0)
      *ty_idx = Get_TY (TREE_TYPE(exp));
    else {
      *ty_idx = component_ty_idx;
      INT node_align = TYPE_ALIGN(TREE_TYPE(exp)) / BITSPERBYTE;
      if (node_align < TY_align(component_ty_idx))
	Set_TY_align(*ty_idx, node_align);//pick more stringent align
    }
    if (component_offset != 0) { // TODO: use ILDA instead
      WN *wn0 = WN_Intconst(MTYPE_I4, component_offset);
      wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn, wn0);
    }
    return wn;
  }
  else if (code == CALL_EXPR) {
    wn = WFE_Expand_Expr(exp);
    FmtAssert (WN_opcode (wn) == OPC_MCOMMA,
               ("opcode other than OPC_MCOMMA for call underneath ARRAY_REF"));
    WFE_Stmt_Append (WN_kid0 (wn), Get_Srcpos ());
    ST *st = WN_st (WN_kid1 (wn));
    WN_Delete (WN_kid1 (wn));
    WN_Delete (wn);
    wn = WN_Lda (Pointer_Mtype, ST_ofst(st)+component_offset, st, field_id);
    if (component_ty_idx == 0)
      *ty_idx = ST_type(st);
    else {
      *ty_idx = component_ty_idx;
      if (TY_align(ST_type(st)) < TY_align(component_ty_idx))
	Set_TY_align(*ty_idx, TY_align(ST_type(st)));//pick more stringent align
    }
    Is_True(TY_kind(*ty_idx) == KIND_ARRAY,
	    ("WFE_Array_Expr: ARRAY_REF base not of type KIND_ARRAY"));
    return wn;
  }
  else if (code == ARRAY_REF) { // recursive call
    WN *wn0, *wn1, *wn2;
    TY_IDX ty_idx0;
#ifdef KEY  // Bug 5831.
    wn0 = WFE_Array_Expr(TREE_OPERAND (exp, 0), &ty_idx0, 0,
			 component_offset, field_id);
#else
    wn0 = WFE_Array_Expr(TREE_OPERAND (exp, 0), &ty_idx0, component_ty_idx,
			 component_offset, field_id);
#endif
    Is_True(TY_kind(ty_idx0) == KIND_ARRAY,
	    ("WFE_Array_Expr: arg 0 of ARRAY_REF not of type KIND_ARRAY"));
    ARB_HANDLE arb = TY_arb(ty_idx0);
    if (ARB_dimension(arb) == 1 && 
	ARB_first_dimen(arb) && ARB_last_dimen(arb) &&
	ARB_const_lbnd(arb)) {
      if (ARB_const_ubnd(arb))
        wn1 = WN_Intconst(MTYPE_I4, ARB_ubnd_val(arb) - ARB_lbnd_val(arb) + 1);
#ifdef KEY
      // Variable upper bound.  Bug 4692.
      else if (ARB_ubnd_var(arb)) {
        ST *ubnd_st = &St_Table[ARB_ubnd_var(arb)];
	wn1 = WN_Binary(OPR_SUB, MTYPE_I4,
			WN_Ldid(MTYPE_I4, 0, ubnd_st, ST_type(ubnd_st)),
			WN_Intconst(MTYPE_I4, ARB_lbnd_val(arb) - 1));
      }
#endif
      else
        wn1 = WN_Intconst(MTYPE_I4, 0);
      wn2 = WFE_Expand_Expr (TREE_OPERAND (exp, 1));
#ifdef TARG_X8664 // bug 11705
      // when a 32-bit integer is stored in a 64-bit register,
      // the high-order 32 bits are zero-extended for x8664
      if (WN_operator(wn2) == OPR_SUB)
        WN_set_rtype(wn2, Mtype_TransferSign(MTYPE_I4, WN_rtype(wn2)));
#endif
#ifdef KEY // bug 14871, OSP_455
      if (TARGET_64BIT && OPCODE_is_load(WN_opcode(wn2)))
        WN_set_rtype(wn2, Mtype_TransferSize(MTYPE_U8, WN_rtype(wn2)));
#endif
#ifdef KEY
      // Expand the current dimension by growing the array just expanded.  Bug
      // 4692.
      if (TREE_CODE(TREE_OPERAND(exp, 0)) == ARRAY_REF) {
        Is_True(WN_operator(wn0) == OPR_ARRAY,
		("WFE_Array_Expr: ARRAY_REF not translated to OPR_ARRAY"));
	int old_kid_count = WN_kid_count(wn0);
	int new_kid_count = old_kid_count + 2;
	wn = WN_Create(OPR_ARRAY, Pointer_Mtype, MTYPE_V, new_kid_count);
	for (int kid = 0; kid < (old_kid_count >> 1); kid++) {
	  WN_kid(wn, kid + 1) = WN_kid(wn0, kid + 1);
	  WN_kid(wn, (new_kid_count >> 1) + kid + 1) =
	    WN_kid(wn0, (old_kid_count >> 1) + kid + 1);
	}
	WN_kid(wn, 0) = WN_kid(wn0, 0);
	WN_kid(wn, new_kid_count >> 1) = wn1;
	WN_kid(wn, new_kid_count - 1) = wn2;
	WN_Delete(wn0);
      } else
#endif
      wn = WN_Ternary(OPR_ARRAY, Pointer_Mtype, wn0, wn1, wn2);

      WN_element_size(wn) = TY_size(Get_TY (TREE_TYPE(exp)));
    }
    else Is_True(FALSE,
		 ("WFE_Array_Expr: only const-bounds 1-dimension arrays handled now"));
    if (component_ty_idx == 0) {
      *ty_idx = TY_etype(ty_idx0);
      if (TY_align(ty_idx0) < TY_align(*ty_idx))
	Set_TY_align(*ty_idx, TY_align(ty_idx0));// pick more stringent align
#ifdef KEY // bug 10728
      if (TREE_THIS_VOLATILE(exp))
	Set_TY_is_volatile(*ty_idx);
#endif
    }
    else *ty_idx = component_ty_idx;
    return wn;
  }
#ifdef KEY
  else if (code == COMPOUND_LITERAL_EXPR) {
    tree arg0 = DECL_INITIAL (TREE_OPERAND (TREE_OPERAND (exp, 0), 0));
    ST *st = WFE_Generate_Temp_For_Initialized_Aggregate (arg0, "");
    wn = WN_Lda (Pointer_Mtype, ST_ofst(st), st);
    *ty_idx = component_ty_idx == 0 ? ST_type(st) : component_ty_idx;
    return wn;
  } else if (code == TARGET_EXPR) {
    wn = WFE_Expand_Expr(exp);
    Is_True(WN_operator(wn) == OPR_LDID,
	    ("WFE_Array_Expr: OPR_LDID not found"));
    ST *st = WN_st(wn);
    wn = WN_Lda (Pointer_Mtype, ST_ofst(st)+component_offset, st, field_id);
    if (component_ty_idx == 0)
      *ty_idx = ST_type(st);
    else {
      *ty_idx = component_ty_idx;
      if (TY_align(ST_type(st)) < TY_align(component_ty_idx))
	Set_TY_align(*ty_idx, TY_align(ST_type(st)));//pick more stringent align
    }
    Is_True(TY_kind(*ty_idx) == KIND_ARRAY,
	    ("WFE_Array_Expr: ARRAY_REF base not of type KIND_ARRAY"));
    return wn;
  }
#endif /* KEY */
  else {
    Is_True(FALSE,
	    ("WFE_Array_Expr: unsupported node for base of ARRAY_REF"));
    return NULL;
  }
}

/* access to different vbuf array need to adjust offset. Following function 
 * used as one general interface to adjust kinds of cases. It is far away from
 * finished. 
 */
#if defined(TARG_SL)
/* decide if the wn tree has LDA node and if the node has vbuf symbol. if so, we
 * need traverse the tree and return the ST symbol. 
 */ 
ST*
WN_Include_Vbuf_Symbol( WN* wn, BOOL need_initialize) {
  WN* tmp = wn;
  static ST* st = NULL;
  if(need_initialize) {
    st = NULL;
    need_initialize=FALSE;
  }
  for( INT i = 0; i < WN_kid_count(wn); i++) {
    if(WN_operator(WN_kid(wn,i)) != OPR_LDA) {
      st = WN_Include_Vbuf_Symbol(WN_kid(wn,i), need_initialize);
    }
    else {
      if(WN_has_sym(WN_kid(wn,i)) && ST_in_vbuf(WN_st(WN_kid(wn,i)))) {
	return WN_st(WN_kid(wn,i));
      }
    }
  }
  return st; 
}

/* do actual adjustment work, this function only needed for v2buf/v4buf */ 
 
void
WN_Adjust_Vbuf_Ofst(WN* wn, ST* st){
  INT shft_num = ST_in_v2buf(st) ? 1 : (ST_in_v4buf(st) ? 2 : 0);
  /* OPR_INTCONST && OPR_LDA has kid_count is zero */ 
  if(WN_operator(wn) == OPR_LDA) {
    Is_True(WN_has_sym(wn), ("this symbol don't has base address symbole"));
    Is_True(ST_in_vbuf(st), ("this symbol for adjusting offset is not vbuf variable"));
    /* __v2buf char array[5][4][16]
     * array[3][2][8] = value;
     * lda_offset equals 3*4*16+2*16+8 
     * the actual offset we need is (3*4*16 + 2*16)*2 + 8
     * so we need do seperate the lower dimension from higher dimension using formula:
     * 
     *       lda_offset = (((lda_offset/16)*16)<<shft_num) + lda_offset % 16;
     */
    // testing only adjust lda_offset in cgemit r_apply_l_const
    if(!WN_vbuf_ofst_adjusted(wn)) {
      WN_lda_offset(wn) = (((WN_lda_offset(wn) / 16) * 16) << shft_num ) \
	+ (WN_lda_offset(wn) % 16);
      WN_Set_vbuf_ofst_adjusted(wn, TRUE);
    }
    return;
  }
  else if(WN_operator(wn) == OPR_INTCONST) {
    if(WN_vbuf_ofst_adjusted(wn)) return;
    else {
      WN_const_val(wn) <<= shft_num;
      WN_Set_vbuf_ofst_adjusted(wn, TRUE);
    }
    return;
  }
  else if(WN_kid_count(wn)) {
    for( INT i = 0; (i < WN_kid_count(wn) || !WN_kid_count(wn)); i++) {
      /* if there is sub-tree which has one type of above four, all addend and ofst 
       * in sub-tree must have been adjusted again, so we don't adjust twice. 
       */              
      
      if(WN_operator(wn) == OPR_STID ||
	 WN_operator(wn) == OPR_LDID ||
	 WN_operator(wn) == OPR_ISTORE || 
	 WN_operator(wn) == OPR_ILOAD)
	continue;
      /* For operator array, kid1 means the size of the dimension 
       * we don't adjust this const value
       */
      else if(WN_operator(wn) == OPR_ARRAY && i == 1) {
	continue;
      }
/* __v2buf char array[3][2][16]; 
 * ptr_2_v2buf = array[i][j]
 * ptr_2_v2buf = array + (i*2+j)*16*2
 *   U4LDA 0 <1,31,array> T<60,anon_ptr.,4>
 *     U4U4LDID 0 <2,2,j> T<8,.predef_U4,4>
 *      U4U4LDID 0 <2,1,i> T<8,.predef_U4,4>
 *      I4INTCONST 2 (0x2)  <=== don't adjust this const 
 *     U4MPY
 *    U4ADD
 *    U4INTCONST 32 (0x20)  <== only adjust this const 
 *   U4MPY
 *  U4ADD
 * U4STID 0 <1,35,ptr2> T<55,anon_ptr.,4>
 */
      else if(WN_operator(wn) == OPR_MPY)
        {
          continue;
        }
/*__v4buf char b4_mv[16]
 * 
 *    I4I4LDID 0 <2,9,i0> T<4,.predef_I4,4>
 *      I4I4LDID 0 <2,10,j0> T<4,.predef_I4,4>
 *      I4INTCONST 2 (0x2)   <==== don't adjust this constant
 *     I4SHL
 *    I4ADD
 *   U4I4CVT
 *  U4LDA 0 <1,31,b4_mv> T<57,anon_ptr.,4>
 * U4ADD
 */
      else if(WN_operator(wn) == OPR_SHL) {
           continue;
	}
      else {
	WN_Adjust_Vbuf_Ofst(WN_kid(wn,i), st);
      }
    }
    return;
  }
  return;
}


WN* 
Shft_Vbuf_Array_Ofst(WN* wn, INT shft_num = 0) {
  if(WN_operator(wn) == OPR_INTCONST) {
    WN_const_val(wn) <<= shft_num;
  }
  else if(WN_operator(wn) == OPR_LDA) {
    WN_lda_offset(wn) <<= shft_num;
  } 
  return wn;
}

WN* 
Adjust_Vbuf_Array_Ofst(WN* wn) {
  Is_True((WN_operator(wn) == OPR_STID ||
          WN_operator(wn) == OPR_LDID || 
          WN_operator(wn) == OPR_ISTORE || 
          WN_operator(wn) == OPR_ILOAD ||
          WN_operator(wn) == OPR_PARM) , 
          ( " invalid operator for adjusting vbuf ofst"));
  ST* st = WN_Include_Vbuf_Symbol(wn, TRUE); 
  /* compiler only adjust v2buf and v4buf variable */
  if(!st || ! (ST_in_v2buf(st) || ST_in_v4buf(st))) return wn;

       /* case 1:
        *    __v2buf char array[4][10][16];
        *    __v2buf char * ptr;
        *    ptr = array[4][6];
        */
  for( INT i = 0; i < WN_kid_count(wn); i++) {
    WN_Adjust_Vbuf_Ofst(WN_kid(wn,i), st);
  }
  return wn;
}
#endif 
#ifdef TARG_SL
/* Same_Var: 
 *      To check whether tree (rhs) contains the reference
 *      to the variable with the same name as var_name
 */
static BOOL sameness;
static BOOL Same_Var( char* var_name, tree rhs )
{
  if (!rhs) {
     DevWarn("Same_Var::rhs is NULL");
     return FALSE;
  }

  /* put the constant tree nodes here.
   */
  if( TREE_CODE_CLASS(TREE_CODE(rhs)) == 'c' )
    return FALSE;

  /* I dont want to handle component reference, it's
   * too complicated.
   * //TODO
   */
  if( TREE_CODE(rhs) == COMPONENT_REF )
    return FALSE;

  BOOL tempsame = FALSE;
  /* check if the names are the same.
   * I'm very sorry, since there is no traversing function for
   * gcc AST tree. So this code segment is very ugly
   */
  switch( TREE_CODE(rhs) ) {
    case VAR_DECL :
      if( DECL_NAME(rhs) && !strcmp(var_name,IDENTIFIER_POINTER(DECL_NAME(rhs))))
        return TRUE;
      else
        return FALSE;
    case PARM_DECL:
      if( DECL_NAME(rhs) && !strcmp(var_name,IDENTIFIER_POINTER(DECL_NAME(rhs))))
        return TRUE;
      else if( TREE_CHAIN(rhs) ) 
          tempsame |= Same_Var( var_name, TREE_CHAIN(rhs) );
      else
        return FALSE;
      break;
    case FUNCTION_DECL:
      tempsame |= Same_Var( var_name, DECL_ARGUMENTS(rhs) );
      break;
    case INDIRECT_REF:
      /* Like the treatment of l.h.s, we only treat the simple case,
       * where *p with p being a parm_decl or var_decl
       */
      if( TREE_CODE(TREE_OPERAND(rhs,0))==PARM_DECL ||
          TREE_CODE(TREE_OPERAND(rhs,0))==VAR_DECL ) {
        tempsame |= Same_Var( var_name, TREE_OPERAND(rhs,0) );
      } else
        return FALSE;
      break;
    default:
      for (int i=0; i < TREE_CODE_LENGTH(TREE_CODE(rhs)); i++) {
        if( TREE_OPERAND(rhs,i) )
          tempsame |= Same_Var( var_name, TREE_OPERAND(rhs,i) );
      }
      break; 

  }
  /* I dont know how to get all the kids of a tree node,
   * but I use the common case : each node has only two kids
   */
 
  return tempsame;
} 
#endif

#ifdef TARG_SL
/* For case: *p++(or --) op *p ... */
static BOOL Is_Special_Case (WN* wn)
{
  WN * body;
  WN * last;

  FmtAssert(WN_operator(wn) == OPR_ISTORE, ("WGEN_Stmt_Add: FYI"));

  body = WFE_Stmt_Top ();

  if (body) {

/* Here is just a simple match.
 * 
 * wn:   (*p)
 *    .....
 *   U4U4LDID 72 <1,4,.preg_U4> T<47,anon_ptr.,4> # <preg>
 *  I4ISTORE 0im:0 T<47,anon_ptr.,4>
 *
 * last: (p++)
 *    U4U4LDID 72 <1,4,.preg_U4> T<47,anon_ptr.,4> # <preg>
 *    U4INTCONST 4 (0x4)
 *   U4ADD
 *  U4STID 0 <2,1,p> T<47,anon_ptr.,4>
 */

    last = WN_last(body);
    if ((WN_operator(last) == OPR_STID )
       && ((WN_operator(WN_kid0(last)) == OPR_ADD) 
         || (WN_operator(WN_kid0(last)) == OPR_SUB)) 
       && (WN_Equiv(WN_kid0(WN_kid0(last)) ,WN_kid1(wn))))    
      return TRUE;
    else
      return FALSE;
  }
} 
#endif


/* rhs_wn is the WN representing the rhs of a MODIFY_EXPR node; this
 * routine processes the lhs of the node and generate the appropriate
 * form of store.
 *
 * In special cases where the RHS of the store is unknown but the
 * statement being expanded is nonetheless semantically a store,
 * rhs_wn can be NULL. This happens, for example, for each output
 * operand of an asm statement. When rhs_wn is NULL, we manufacture an
 * RHS that is an LDID of a PREG specified by rhs_preg_num (generally
 * a negative-numbered PREG). If rhs_st is non-NULL, rhs_preg_num is
 * ignored.  assign_code tells if it is {PRE,POST}{IN,DE}CREMENT_EXPR.
 * Ordinarily, it is MODIFY_EXPR.
 */
WN *
WFE_Lhs_Of_Modify_Expr(tree_code assign_code,
		       tree lhs, 
#ifdef TARG_SL
                       /* To make consistent with gcc on some
                        * undefined behavior: *p++=...
                        */
                       tree rhs,
#endif
		       bool need_result,
		       TY_IDX component_ty_idx, 
		       INT64 component_offset,
		       UINT32 field_id,
		       bool is_bit_field,
		       WN *rhs_wn,
		       PREG_NUM rhs_preg_num,
		       bool is_realpart,
		       bool is_imagpart)
{
  WN *wn = NULL;
  ST *st;
  bool result_in_temp = FALSE;
  ST *result_preg_st;
  PREG_NUM result_preg;
  PREG_NUM lhs_preg_num = 0;
  enum tree_code code = TREE_CODE (lhs);
  BOOL volt = FALSE;
#ifdef TARG_SL
  BOOL need_append = FALSE;
#endif
  if (rhs_wn != NULL) {
    WFE_Set_ST_Addr_Saved (rhs_wn);
#ifdef TARG_SL	
    need_append = WN_Need_Append_Intrinsic(rhs_wn);	
#endif
  }

  switch (code) {
  case COMPONENT_REF:
    {
      INT64 ofst;
      TY_IDX ty_idx0;

      tree arg0 = TREE_OPERAND(lhs, 0);
      tree arg1 = TREE_OPERAND(lhs, 1);
#ifdef GPLUSPLUS_FE
      // for g++ ensure that the WHIRL type for the enclosing structure has been
      // created in order to set the field id to a non zero value
      (void) Get_TY (TREE_TYPE (arg0));
#endif /* GPLUSPLUS_FE */
      if (component_ty_idx == 0)
        ty_idx0 = Get_TY(TREE_TYPE(lhs));
      else ty_idx0 = component_ty_idx;
      if (DECL_BIT_FIELD(arg1)) 
        is_bit_field = TRUE;
      if (! is_bit_field)
        ofst = (BITSPERBYTE * Get_Integer_Value(DECL_FIELD_OFFSET(arg1)) +
			      Get_Integer_Value(DECL_FIELD_BIT_OFFSET(arg1)))
			    / BITSPERBYTE;
      else ofst = 0;
#ifdef KEY    // bug 10422: check if the field is volatile
      if (TREE_THIS_VOLATILE(arg1)) {
	Set_TY_is_volatile(ty_idx0);
	volt = TRUE;
      }
#endif
#ifdef KEY
      FmtAssert (DECL_FIELD_ID(arg1) != 0,
                 ("WFE_Lhs_Of_Modify_Expr: DECL_FIELD_ID used but not set"));
#endif
#ifdef TARG_SL
      wn = WFE_Lhs_Of_Modify_Expr(assign_code, arg0, NULL, need_result, ty_idx0, 
#else
      wn = WFE_Lhs_Of_Modify_Expr(assign_code, arg0, need_result, ty_idx0, 
#endif
				  ofst+component_offset,
			          field_id + DECL_FIELD_ID(arg1), is_bit_field, 
				  rhs_wn, rhs_preg_num, is_realpart,
				  is_imagpart);
    }
    return wn;

  case REALPART_EXPR:
    {
      tree arg0 = TREE_OPERAND(lhs, 0);
      TY_IDX ty_idx0 = Get_TY(TREE_TYPE(arg0));
#ifdef TARG_SL
      wn = WFE_Lhs_Of_Modify_Expr(assign_code, arg0, NULL, need_result, ty_idx0,
#else
      wn = WFE_Lhs_Of_Modify_Expr(assign_code, arg0, need_result, ty_idx0,
#endif
				  component_offset, field_id, is_bit_field,
				  rhs_wn, rhs_preg_num, TRUE, FALSE);
    }
    return wn;

  case IMAGPART_EXPR:
    {
      tree arg0 = TREE_OPERAND(lhs, 0);
      TY_IDX ty_idx0 = Get_TY(TREE_TYPE(arg0));
#ifdef TARG_SL
      wn = WFE_Lhs_Of_Modify_Expr(assign_code, arg0, NULL, need_result, ty_idx0,
#else
      wn = WFE_Lhs_Of_Modify_Expr(assign_code, arg0, need_result, ty_idx0,
#endif
				  component_offset, field_id, is_bit_field,
				  rhs_wn, rhs_preg_num, FALSE, TRUE);
    }
    return wn;

#ifdef KEY
  case TARGET_EXPR:	// bug 6907
    {
      WN *wn = WFE_Expand_Expr(lhs);
      Is_True(WN_operator(wn) == OPR_LDID,
	      ("WFE_Lhs_Of_Modify_Expr: wrong operator from TARGET_EXPR"));
      st = WN_st(wn);
    }
    // fall through
#endif

  case PARM_DECL:
  case VAR_DECL:
  case RESULT_DECL:
    {
      TY_IDX hi_ty_idx = Get_TY(TREE_TYPE(lhs)); // type associated with field_id
      if (TREE_THIS_VOLATILE(lhs)) {
        Set_TY_is_volatile(hi_ty_idx);
        volt = TRUE;
      }
      TY_IDX desc_ty_idx = component_ty_idx;
      if (desc_ty_idx == 0)
        desc_ty_idx = hi_ty_idx;
      if (TY_is_volatile(desc_ty_idx)) {
        Clear_TY_is_volatile(desc_ty_idx);
        volt = TRUE;
      }

#ifdef KEY
      if (code != TARGET_EXPR)
#endif
      st = Get_ST (lhs);

      if (ST_assigned_to_dedicated_preg (st)) {
        Set_TY_is_volatile(hi_ty_idx);
        volt = TRUE;
      }
      Is_True(! is_bit_field || field_id <= MAX_FIELD_ID,
	      ("WFE_Lhs_Of_Modify_Expr: field id for bit-field exceeds limit"));

      TYPE_ID rtype = Widen_Mtype(TY_mtype(desc_ty_idx));
      TYPE_ID desc = is_bit_field ? MTYPE_BS : TY_mtype(desc_ty_idx);

      if (rhs_wn == NULL) {
        // Manufacture a negative-PREG RHS for the STID we are about to
        // generate. This feature is used in preparing WHIRL ASM
        // statements.
        // TODO: How to support a bit-field output of non-integral
        // number of bytes?
        if (rtype == MTYPE_M && desc == MTYPE_M) {
#ifndef KEY
          FmtAssert(TY_size(desc_ty_idx) == MTYPE_byte_size(Def_Int_Mtype),
                    ("Size of ASM struct opnd must be equal to register size"));
          desc = rtype = Def_Int_Mtype;
          desc_ty_idx = hi_ty_idx = MTYPE_To_TY(Def_Int_Mtype);
#else
	  // Handle asm like:
	  //        asm("cfc1 %0,$31":"=r"(*s));
	  // where, s is a pointer to a structure.
	  if (rtype == MTYPE_M && desc == MTYPE_M) {
	    if (TY_size(desc_ty_idx) == MTYPE_byte_size(Def_Int_Mtype)) {
	      desc = rtype = Def_Int_Mtype;
	      desc_ty_idx = hi_ty_idx = MTYPE_To_TY(Def_Int_Mtype);
	    } else {
	      desc = rtype = MTYPE_I4;
	      desc_ty_idx = hi_ty_idx = MTYPE_To_TY(MTYPE_I4);
	    }
	  }
#endif
        }
        ST *rhs_st = MTYPE_To_PREG(desc);
        rhs_wn = WN_CreateLdid (OPR_LDID, rtype,
			        desc, rhs_preg_num, rhs_st,
			        desc_ty_idx, 0);
      }
      else {
        WN *result_wn;	// the result wn to be returned
        if (assign_code == MODIFY_EXPR) {
	  if (is_realpart)
	    rhs_wn = WN_Binary(OPR_COMPLEX, rtype,
			       rhs_wn,
			       WN_Unary(OPR_IMAGPART,
				        Mtype_complex_to_real (rtype),
				        WN_CreateLdid(OPR_LDID, rtype, desc,
						      ST_ofst(st) + component_offset,
						      st, hi_ty_idx, field_id)));
	  else
	  if (is_imagpart)
	    rhs_wn = WN_Binary(OPR_COMPLEX, rtype,
			       WN_Unary(OPR_REALPART,
				        Mtype_complex_to_real (rtype),
				        WN_CreateLdid(OPR_LDID, rtype, desc,
						      ST_ofst(st) + component_offset,
						      st, hi_ty_idx, field_id)),
			       rhs_wn);
        }
        else {
	  if (is_realpart)
	    rhs_wn = WN_Binary(OPR_COMPLEX, rtype,
			       rhs_wn,
			       WN_Floatconst (Mtype_complex_to_real (rtype), 0.0));
	  else
	  if (is_imagpart)
	    rhs_wn = WN_Binary(OPR_COMPLEX, rtype,
			       WN_Floatconst (Mtype_complex_to_real (rtype), 0.0),
			       rhs_wn);
        }

        if (assign_code == PREINCREMENT_EXPR ||
	    assign_code == PREDECREMENT_EXPR) {
          wn = WN_CreateLdid (OPR_LDID, rtype, desc, 
			      ST_ofst(st) + component_offset,
			      st, hi_ty_idx, field_id);
          rhs_wn = WN_Binary(Operator_From_Tree [assign_code].opr,
		             rtype, wn, rhs_wn);
	  result_wn = rhs_wn;
        }
        else if (assign_code == POSTINCREMENT_EXPR ||
	         assign_code == POSTDECREMENT_EXPR) {
          result_wn = WN_CreateLdid (OPR_LDID, rtype, desc, 
				     ST_ofst(st) + component_offset,
				     st, hi_ty_idx, field_id);
        }
        else result_wn = rhs_wn;

	// OSP_382, do not store MTYPE_M into temp
        if (need_result && rtype != MTYPE_M &&
	    (volt ||
	     assign_code == POSTINCREMENT_EXPR ||
	     assign_code == POSTDECREMENT_EXPR)) { // save result in a preg
          result_in_temp = TRUE;
          result_preg_st = MTYPE_To_PREG(rtype);
          result_preg = Create_Preg(rtype, NULL);
          wn = WN_Stid(rtype, result_preg, result_preg_st, desc_ty_idx,
		       result_wn, 0);
          WFE_Stmt_Append (wn, Get_Srcpos());
          result_wn = WN_Ldid(rtype, result_preg, result_preg_st, desc_ty_idx,
			      0);
        }

        if (assign_code == POSTINCREMENT_EXPR ||
	    assign_code == POSTDECREMENT_EXPR) {
          rhs_wn = WN_Binary(Operator_From_Tree [assign_code].opr,
		             rtype, result_wn, rhs_wn);
        }
        else rhs_wn = result_wn;

        // rhs_wn is now always the right-hand-side of the assignment
      }

      // the assignment
      if (!WFE_Keep_Zero_Length_Structs &&
          desc == MTYPE_M               &&
          TY_size (hi_ty_idx) == 0) {
        // ignore zero length structs
      }
      else {
#ifdef KEY    // bug 10422: check if the field is volatile
	if (volt) 
	  Set_TY_is_volatile(hi_ty_idx);
#endif
        wn = WN_Stid (desc, ST_ofst(st) + component_offset + lhs_preg_num, st,
		      hi_ty_idx, rhs_wn, field_id);
#if defined(TARG_SL)
        wn = Adjust_Vbuf_Array_Ofst(wn);
#endif 
        WFE_Stmt_Append(wn, Get_Srcpos());
#if defined(TARG_SL)
      if (need_append) {
	WN *ldid_wn;
        if (! result_in_temp)
           ldid_wn =  WN_CreateLdid(OPR_LDID, rtype, desc,
                           ST_ofst(st) + component_offset, st, hi_ty_idx,
                           field_id);
 
	else 
           ldid_wn =  WN_Ldid(rtype, result_preg, result_preg_st, desc_ty_idx, 0);
 	
        WFE_Stmt_Append_Extend_Intrinsic(wn, ldid_wn, Get_Srcpos());
      }
#endif
      }
      if (need_result) {
        if (! result_in_temp)
          wn = WN_CreateLdid(OPR_LDID, rtype, desc, 
			     ST_ofst(st) + component_offset, st, hi_ty_idx,
			     field_id);
        else wn = WN_Ldid(rtype, result_preg, result_preg_st, desc_ty_idx, 0);
        if (is_realpart)
	  wn = WN_Unary (OPR_REALPART, Mtype_complex_to_real (rtype), wn);
        else
        if (is_imagpart)
	  wn = WN_Unary (OPR_IMAGPART, Mtype_complex_to_real (rtype), wn);
      }
      else wn = NULL;
    }
    break;

  case INDIRECT_REF:
    {
      TY_IDX hi_ty_idx = Get_TY(TREE_TYPE(lhs));
      if (TREE_THIS_VOLATILE(lhs)) {
        Set_TY_is_volatile(hi_ty_idx);
        volt = TRUE;
      }
      tree op = TREE_OPERAND(lhs, 0);
      WN *addr_wn = WFE_Expand_Expr (TREE_OPERAND (lhs, 0));
      TY_IDX desc_ty_idx = component_ty_idx;
      if (desc_ty_idx == 0)
        desc_ty_idx = hi_ty_idx;
      if (TY_is_volatile(desc_ty_idx)) {
        Clear_TY_is_volatile(desc_ty_idx);
        volt = TRUE;
      }
      Is_True(! is_bit_field || field_id <= MAX_FIELD_ID,
	      ("WFE_Lhs_Of_Modify_Expr: field id for bit-field exceeds limit"));
      if (WN_has_side_effects(addr_wn) &&
	  (need_result || 
	   assign_code == PREINCREMENT_EXPR ||
	   assign_code == PREDECREMENT_EXPR ||
	   assign_code == POSTINCREMENT_EXPR ||
	   assign_code == POSTDECREMENT_EXPR)) {
        ST       *preg_st;
        PREG_NUM  preg;
        TY_IDX    address_ty_idx = Get_TY (TREE_TYPE (TREE_OPERAND (lhs, 0)));
#ifdef KEY
        //Bug 8738: PREG should NOT be VOLATILE in whirl
        if (TY_is_volatile(address_ty_idx)) {
           Clear_TY_is_volatile(address_ty_idx);
           volt = TRUE;
        }
#endif
        preg_st = MTYPE_To_PREG(Pointer_Mtype);
        preg    = Create_Preg (Pointer_Mtype, NULL);
        wn      = WN_Stid (Pointer_Mtype, preg, preg_st, address_ty_idx, addr_wn);
        WFE_Set_ST_Addr_Saved (addr_wn);
        WFE_Stmt_Append (wn, Get_Srcpos());
        addr_wn = WN_Ldid (Pointer_Mtype, preg, preg_st, address_ty_idx);
      }

      TYPE_ID rtype = Widen_Mtype(TY_mtype(desc_ty_idx));
      TYPE_ID desc = is_bit_field ? MTYPE_BS : TY_mtype(desc_ty_idx);

      if (rhs_wn == NULL) {
        // Manufacture a negative-PREG RHS for the ISTORE we are about to
        // generate. This feature is used in preparing WHIRL ASM
        // statements.
        ST *rhs_st;
        // TODO: How to support a bit-field output of non-integral
        // number of bytes?
        rhs_st = MTYPE_To_PREG(desc);
        // Types are likely to be wrong in the following
        rhs_wn = WN_CreateLdid (OPR_LDID, rtype, desc, rhs_preg_num, rhs_st,
			        desc_ty_idx, 0);
#ifdef KEY
      // Bug 8056: Need to preserve the semantics on the preg if it's size
      // is less than 4 bytes.
      if (MTYPE_byte_size(desc) < 4) {
                                                                                                                                                            
         rhs_wn = WN_CreateCvtl(!MTYPE_signed(desc) ? OPC_U4CVTL : OPC_I4CVTL,
                                MTYPE_bit_size(desc),
                                rhs_wn);
                                                                                                                                                            
      }
#endif
      }
      else {
        WN *result_wn;	// the result wn to be returned

        if (assign_code == MODIFY_EXPR) {
	  if (is_realpart)
	    rhs_wn = WN_Binary(OPR_COMPLEX, rtype,
			       rhs_wn,
			       WN_Unary(OPR_IMAGPART,
				        Mtype_complex_to_real (rtype),
				        WN_CreateIload(OPR_ILOAD, rtype, desc,
						       component_offset,
						       field_id != 0 ? hi_ty_idx : desc_ty_idx,
						       Make_Pointer_Type(hi_ty_idx, FALSE),
						       WN_COPY_Tree (addr_wn),
						       field_id)));
	  else
	  if (is_imagpart)
	    rhs_wn = WN_Binary(OPR_COMPLEX, rtype,
			       WN_Unary(OPR_REALPART,
				        Mtype_complex_to_real (rtype),
				        WN_CreateIload(OPR_ILOAD, rtype, desc,
						       component_offset,
						       field_id != 0 ? hi_ty_idx : desc_ty_idx,
						       Make_Pointer_Type(hi_ty_idx, FALSE),
						       WN_COPY_Tree (addr_wn),
						       field_id)),
			       rhs_wn);
        }
        else {
	  if (is_realpart)
	    rhs_wn = WN_Binary(OPR_COMPLEX, rtype,
			       rhs_wn,
			       WN_Floatconst (Mtype_complex_to_real (rtype), 0.0));
	  else
	  if (is_imagpart)
	    rhs_wn = WN_Binary(OPR_COMPLEX, rtype,
			       WN_Floatconst(Mtype_complex_to_real(rtype), 0.0),
			       rhs_wn);
        }

        if (assign_code == PREINCREMENT_EXPR ||
	    assign_code == PREDECREMENT_EXPR) {
          wn = WN_CreateIload (OPR_ILOAD, rtype, desc, component_offset,
			       field_id != 0 ? hi_ty_idx : desc_ty_idx,
			       Make_Pointer_Type(hi_ty_idx, FALSE),
			       WN_COPY_Tree (addr_wn),
			       field_id);
          rhs_wn = WN_Binary(Operator_From_Tree [assign_code].opr,
                             rtype, wn, rhs_wn);
          result_wn = rhs_wn;
        }
        else if (assign_code == POSTINCREMENT_EXPR ||
	         assign_code == POSTDECREMENT_EXPR) {
	  result_wn = WN_CreateIload (OPR_ILOAD, rtype, desc, component_offset,
				      field_id != 0 ? hi_ty_idx : desc_ty_idx,
				      Make_Pointer_Type(hi_ty_idx, FALSE),
				      WN_COPY_Tree (addr_wn),
				      field_id);
        }
        else result_wn = rhs_wn;

	// OSP_382, do not store MTYPE_M into temp
        if (need_result && rtype != MTYPE_M &&
	    (volt ||
             assign_code == POSTINCREMENT_EXPR ||
             assign_code == POSTDECREMENT_EXPR)) { // save result in a preg
	  result_in_temp = TRUE;
          result_preg_st = MTYPE_To_PREG(rtype);
          result_preg = Create_Preg(rtype, NULL);
          wn = WN_Stid(rtype, result_preg, result_preg_st, desc_ty_idx,
		       result_wn, 0);
          WFE_Stmt_Append (wn, Get_Srcpos());;
          result_wn = WN_Ldid(rtype, result_preg, result_preg_st, desc_ty_idx,
			      0);
        }

        if (assign_code == POSTINCREMENT_EXPR ||
	    assign_code == POSTDECREMENT_EXPR) {
          rhs_wn = WN_Binary(Operator_From_Tree [assign_code].opr,
                             rtype, result_wn, rhs_wn);
        }
        else rhs_wn = result_wn;

        // rhs_wn is now always the right-hand-side of the assignment
      }

      // the assignment
      if (!WFE_Keep_Zero_Length_Structs &&
          desc == MTYPE_M               &&
          TY_size (hi_ty_idx) == 0) {
        // ignore zero length structs
        if (WN_has_side_effects (addr_wn)) {
	  wn = WN_CreateEval (addr_wn);
	  WFE_Stmt_Append (wn, Get_Srcpos());
        }
        wn = NULL;
      }
      else {
#ifdef KEY
	// The store target could be an INDIRECT_REF that kg++fe added to make
	// the store write to the area pointed to by the fake first param.  If
	// so, check that copying the object does not involve a copy
	// constructor.  kg++fe cannot call the copy constructor whenever it
	// wants because g++ might not have generated the copy constructor
	// definition, since the
	// copy constructor was never called.  Furthermore, copying the object
	// implies we need to destroy the source object, but g++ might not have
	// generated the destructor definition for the same reason.
        //
	// This checking is only necessary when we are copying through the fake
	// param.  Normally g++ calls the copy constructor explicitly if an
	// object requires it.  In such a case, the copy constructor is always
	// defined.
        tree addr = TREE_OPERAND(lhs, 0);
        WN *first_formal = WN_formal(Current_Entry_WN(), 0);
        if (TY_return_in_mem(hi_ty_idx) &&
	    field_id == 0 &&
	    // See if it is an indirect ref of the fake first parm.
	    // bug fix for OSP_314
	    //
	    first_formal != NULL && WN_operator(first_formal) != OPR_BLOCK &&
	    TREE_CODE(addr) == VAR_DECL &&
	    DECL_ST(addr) == WN_st(first_formal)) {
	  FmtAssert(TY_mtype(hi_ty_idx) == MTYPE_M,
		    ("WFE_Lhs_Of_Modify_Expr: return_in_mem type not MTYPE_M"));
	  tree ptr_type = TREE_TYPE(TREE_OPERAND(lhs, 0));
	  tree type = TREE_TYPE(ptr_type);
	  FmtAssert(TREE_CODE(ptr_type) == POINTER_TYPE,
	    ("WFE_Lhs_Of_Modify_Expr: INDIRECT_REF opnd0 is not POINTER_TYPE"));
	  FmtAssert(component_offset == 0,
		    ("WFE_Lhs_Of_Modify_Expr: component_offset nonzero"));
	  TY_IDX tidx = Get_TY(ptr_type);
	  // Check object has no copy constructor.
	  FmtAssert(!WFE_has_copy_constructor(type),
	      ("WFE_Lhs_Of_Modify_Expr: object needs copy constructor"));
        }
#endif
        wn = WN_CreateIstore(OPR_ISTORE, MTYPE_V, desc, component_offset, 
			     Make_Pointer_Type (hi_ty_idx, FALSE),
			     rhs_wn, addr_wn, field_id);
#ifdef TARG_SL
      /* so far I only handle *p++=... cases, change this case to 
       *   *p = ... ;
       *   p++;
       * This is to make our compiler consistent with gcc. So far,
       * only POST(INC/DEC) differs from gcc.
       */
      tree post_inc_dec = TREE_OPERAND(lhs, 0);
      if(((TREE_CODE(post_inc_dec) == POSTINCREMENT_EXPR) ||
          (TREE_CODE(post_inc_dec) == POSTDECREMENT_EXPR)) && Is_Special_Case(wn))
        WFE_Stmt_Prepend_Last(wn, Get_Srcpos());
      else
#endif
        WFE_Stmt_Append(wn, Get_Srcpos());

#if defined(TARG_SL)
      if (need_append) {
         WN *ldid_wn;
         if (! result_in_temp)
          ldid_wn = WN_CreateIload(OPR_ILOAD, rtype, desc, component_offset,
			      field_id != 0 ? hi_ty_idx : desc_ty_idx,
			      Make_Pointer_Type (hi_ty_idx, FALSE),
			      WN_COPY_Tree (addr_wn),
			      field_id);
	else 
          ldid_wn = WN_Ldid(rtype, result_preg, result_preg_st, desc_ty_idx, 0); 
	
        WFE_Stmt_Append_Extend_Intrinsic(wn, ldid_wn, Get_Srcpos());
      }
#endif

        if (need_result) {
	  if (! result_in_temp)
            wn = WN_CreateIload(OPR_ILOAD, rtype, desc, component_offset,
			        field_id != 0 ? hi_ty_idx : desc_ty_idx,
			        Make_Pointer_Type (hi_ty_idx, FALSE),
			        WN_COPY_Tree (addr_wn),
			        field_id);
	  else wn = WN_Ldid(rtype, result_preg, result_preg_st, desc_ty_idx, 0);
	  if (is_realpart)
	    wn = WN_Unary (OPR_REALPART, Mtype_complex_to_real (rtype), wn);
	  else
	  if (is_imagpart)
	    wn = WN_Unary (OPR_IMAGPART, Mtype_complex_to_real (rtype), wn);
        }
        else wn = NULL;
      }
    }
    break;

  case ARRAY_REF:
    {
      TY_IDX elem_ty_idx;
      // generate the WHIRL array node
      WN *addr_wn = WFE_Array_Expr(lhs, &elem_ty_idx, 0, 0, 0);
      if (TY_is_volatile(elem_ty_idx))
        volt = TRUE;
      TY_IDX desc_ty_idx = component_ty_idx;
      if (desc_ty_idx == 0)
        desc_ty_idx = Get_TY (TREE_TYPE(lhs));
      if (TY_is_volatile(desc_ty_idx)) {
        Clear_TY_is_volatile(desc_ty_idx);
        volt = TRUE;
      }
      Is_True(! is_bit_field || field_id <= MAX_FIELD_ID,
	      ("WFE_Lhs_Of_Modify_Expr: field id for bit-field exceeds limit"));
      if (WN_has_side_effects(addr_wn) &&
          (need_result ||
           assign_code == PREINCREMENT_EXPR ||
           assign_code == PREDECREMENT_EXPR ||
           assign_code == POSTINCREMENT_EXPR ||
	   assign_code == POSTDECREMENT_EXPR)) {
        ST       *preg_st;
        PREG_NUM  preg;
        TY_IDX    address_ty_idx = Make_Pointer_Type(elem_ty_idx, FALSE);
        preg_st = MTYPE_To_PREG(Pointer_Mtype);
        preg    = Create_Preg (Pointer_Mtype, NULL);
        wn      = WN_Stid (Pointer_Mtype, preg, preg_st, address_ty_idx, addr_wn);
        WFE_Set_ST_Addr_Saved (addr_wn);
        WFE_Stmt_Append (wn, Get_Srcpos());
        addr_wn = WN_Ldid (Pointer_Mtype, preg, preg_st, address_ty_idx);
      }

      TYPE_ID rtype = Widen_Mtype(TY_mtype(desc_ty_idx));
      TYPE_ID desc = is_bit_field ? MTYPE_BS : TY_mtype(desc_ty_idx);

      if (rhs_wn == NULL) {
        // Manufacture a negative-PREG RHS for the ISTORE we are about to
        // generate. This feature is used in preparing WHIRL ASM
        // statements.
        ST *rhs_st;
        // TODO: How to support a bit-field output of non-integral
        // number of bytes?
        rhs_st = MTYPE_To_PREG(desc);
        rhs_wn = WN_CreateLdid (OPR_LDID, rtype, desc, rhs_preg_num, rhs_st,
			        desc_ty_idx, 0);
      }
      else {
        WN *result_wn;    // the result wn to be returned

        if (assign_code == MODIFY_EXPR) {
	  if (is_realpart)
	    rhs_wn = WN_Binary(OPR_COMPLEX, rtype,
			       rhs_wn,
			       WN_Unary(OPR_IMAGPART,
				        Mtype_complex_to_real (rtype),
				        WN_CreateIload(OPR_ILOAD, rtype, desc,
						       component_offset,
						       field_id != 0 ? elem_ty_idx : desc_ty_idx,
						       Make_Pointer_Type(elem_ty_idx, FALSE),
						       WN_COPY_Tree (addr_wn),
						       field_id)));
	  else
	  if (is_imagpart)
	    rhs_wn = WN_Binary(OPR_COMPLEX, rtype,
			       WN_Unary(OPR_REALPART,
				        Mtype_complex_to_real (rtype),
				        WN_CreateIload(OPR_ILOAD, rtype, desc,
						       component_offset,
						       field_id != 0 ? elem_ty_idx : desc_ty_idx,
						       Make_Pointer_Type(elem_ty_idx, FALSE),
						       WN_COPY_Tree (addr_wn),
						       field_id)),
			       rhs_wn);
        }
        else {
	  if (is_realpart)
	    rhs_wn = WN_Binary(OPR_COMPLEX, rtype,
			       rhs_wn,
			       WN_Floatconst (Mtype_complex_to_real (rtype), 0.0));
	  else
	  if (is_imagpart)
	    rhs_wn = WN_Binary(OPR_COMPLEX, rtype,
			       WN_Floatconst(Mtype_complex_to_real(rtype), 0.0),
			       rhs_wn);
        }

        if (assign_code == PREINCREMENT_EXPR ||
            assign_code == PREDECREMENT_EXPR) {
          wn = WN_CreateIload (OPR_ILOAD, rtype, desc, component_offset,
                               field_id != 0 ? elem_ty_idx : desc_ty_idx,
                               Make_Pointer_Type(elem_ty_idx, FALSE),
                               WN_COPY_Tree (addr_wn),
                               field_id);
          rhs_wn = WN_Binary(Operator_From_Tree [assign_code].opr,
                             rtype, wn, rhs_wn);
	  result_wn = rhs_wn;
        }
        else if (assign_code == POSTINCREMENT_EXPR ||
	         assign_code == POSTDECREMENT_EXPR) {
          result_wn = WN_CreateIload (OPR_ILOAD, rtype, desc, component_offset,
				      field_id != 0 ? elem_ty_idx : desc_ty_idx,
				      Make_Pointer_Type(elem_ty_idx, FALSE),
				      WN_COPY_Tree (addr_wn),
				      field_id);
        }
        else result_wn = rhs_wn;

	// OSP_382, do not store MTYPE_M into temp
        if (need_result && rtype != MTYPE_M &&
	    (volt ||
             assign_code == POSTINCREMENT_EXPR ||
	     assign_code == POSTDECREMENT_EXPR)) { // save result in a preg
          result_in_temp = TRUE;
          result_preg_st = MTYPE_To_PREG(rtype);
          result_preg = Create_Preg(rtype, NULL);
          wn = WN_Stid(rtype, result_preg, result_preg_st, desc_ty_idx,
		       result_wn, 0);
          WFE_Stmt_Append (wn, Get_Srcpos());;
          result_wn = WN_Ldid(rtype, result_preg, result_preg_st, desc_ty_idx,
			      0);
        }

        if (assign_code == POSTINCREMENT_EXPR ||
            assign_code == POSTDECREMENT_EXPR) {
          rhs_wn = WN_Binary(Operator_From_Tree [assign_code].opr,
                             rtype, result_wn, rhs_wn);
        }
        else rhs_wn = result_wn;

        // rhs_wn is now always the right-hand-side of the assignment
      }

      // the assignment
      if (!WFE_Keep_Zero_Length_Structs &&
          desc == MTYPE_M               &&
          TY_size (elem_ty_idx) == 0) {
        // ignore zero length structs
        if (WN_has_side_effects (addr_wn)) {
          wn = WN_CreateEval (addr_wn);
          WFE_Stmt_Append (wn, Get_Srcpos());
        }
        wn = NULL;
      }
      else {
        wn = WN_CreateIstore(OPR_ISTORE, MTYPE_V, desc, component_offset, 
			     Make_Pointer_Type(elem_ty_idx, FALSE), rhs_wn,
			     addr_wn, field_id);
        WFE_Stmt_Append(wn, Get_Srcpos());
#ifdef TARG_SL
      if (need_append) {
        WN *iload;
        if (!result_in_temp)  
            iload = WN_CreateIload (OPR_ILOAD, rtype, desc, component_offset,
                                    field_id != 0 ? elem_ty_idx : desc_ty_idx,
                                    Make_Pointer_Type (elem_ty_idx, FALSE),
                                    WN_COPY_Tree (addr_wn),
                                    field_id);
        else 
            iload = WN_Ldid(rtype, result_preg, result_preg_st, desc_ty_idx, 0);
        WFE_Stmt_Append_Extend_Intrinsic(wn, iload, Get_Srcpos()); 
      }
#endif
        if (need_result) {
          if (! result_in_temp)
	    wn = WN_CreateIload (OPR_ILOAD, rtype, desc, component_offset,
			         field_id != 0 ? elem_ty_idx : desc_ty_idx,
                                 Make_Pointer_Type (elem_ty_idx, FALSE),
			         WN_COPY_Tree (addr_wn),
			         field_id);
	  else wn = WN_Ldid(rtype, result_preg, result_preg_st, desc_ty_idx, 0);
	  if (is_realpart)
	    wn = WN_Unary (OPR_REALPART, Mtype_complex_to_real (rtype), wn);
	  else
	  if (is_imagpart)
	    wn = WN_Unary (OPR_IMAGPART, Mtype_complex_to_real (rtype), wn);
        }
        else wn = NULL;
      }
    }
    break;
#ifdef KEY // bug 10073
  case MIN_EXPR:
  case MAX_EXPR:
    {
      tree arg0 = TREE_OPERAND(lhs, 0);
      tree arg1 = TREE_OPERAND(lhs, 1);

      WN *then_block = WN_CreateBlock ();
      WN *else_block = WN_CreateBlock ();

      WFE_Stmt_Push (then_block, wfe_stmk_if_then, Get_Srcpos());
#ifdef TARG_SL
      WN * wn1 = WFE_Lhs_Of_Modify_Expr (assign_code, arg0, NULL,
#else
      WN * wn1 = WFE_Lhs_Of_Modify_Expr (assign_code, arg0, 
#endif
				         TRUE,
                                         component_ty_idx, component_offset,
                                         field_id, is_bit_field,
                                         rhs_wn, rhs_preg_num, is_realpart,
                                         is_imagpart);
      WFE_Stmt_Pop (wfe_stmk_if_then);

      WFE_Stmt_Push (else_block, wfe_stmk_if_else, Get_Srcpos());
#ifdef TARG_SL
      WN * wn2 = WFE_Lhs_Of_Modify_Expr (assign_code, arg1, NULL,
#else
      WN * wn2 = WFE_Lhs_Of_Modify_Expr (assign_code, arg1,
#endif
					 TRUE,
                                         component_ty_idx, component_offset,
                                         field_id, is_bit_field,
                                         rhs_wn, rhs_preg_num, is_realpart,
                                         is_imagpart);
      WFE_Stmt_Pop (wfe_stmk_if_else);

      Is_True (wn1 && wn2,
               ("WFE_Lhs_Of_Modify_Expr: null operands of MIN/MAX_EXPR?"));
      WN * wn0 = WN_Relational (code == MIN_EXPR ? OPR_LE : OPR_GE,
                                Widen_Mtype(TY_mtype(Get_TY(TREE_TYPE(lhs)))),
                                wn1, wn2);
      WN *if_stmt = WN_CreateIf (wn0, then_block, else_block);
      WFE_Stmt_Append (if_stmt, Get_Srcpos());
    }
    break;
#endif

  default:
    Fail_FmtAssertion ("WFE_Lhs_Of_Modify_Expr: unhandled tree node in LHS of MODIFY_EXPR");
  }

  return wn;
}

/* ============================================================================
 *
 * WFE_Expand_Expr_With_Sequence_Point
 *
 * This routine is invoked instead of WN_Expand_Expr to handle the
 * following expression nodes
 *
 *   both operands of && and ||
 *   all three operands of conditional ?
 *   controlling expression of if
 *   controlling expression of switch
 *   controlling expression of while
 *   statement expression
 *
 * In order to generate WHIRL for an expression with side effects,
 * we would like to move operations such as calls, pre increment/decrement
 * into a comma operator, and operations such as post increment/decrement
 * into a rcomma operator.
 *
 * Sequence points related to function call and return are not handled
 * here as we cannot generate RCOMMA nodes in these cases.
 *
 * ============================================================================
 */

WN*
WFE_Expand_Expr_With_Sequence_Point (tree exp, TYPE_ID mtype, WN* target_wn)
{
  WN *wn;

  if (mtype == MTYPE_V)
#ifdef KEY
    wn = WFE_Expand_Expr (exp, FALSE, 0, 0, 0, 0, FALSE, FALSE, target_wn);
#else
    wn = WFE_Expand_Expr (exp, FALSE);
#endif

  else {

    WN *comma_block      = WN_CreateBlock ();

    WFE_Stmt_Push (comma_block, wfe_stmk_comma, Get_Srcpos ());
#ifdef KEY
    wn = WFE_Expand_Expr (exp, TRUE, 0, 0, 0, 0, FALSE, FALSE, target_wn);
#else
    wn = WFE_Expand_Expr (exp);
#endif

    // normalize bool expressions
    if (TREE_TYPE (exp) == boolean_type_node) {
      if (WN_operator (wn) == OPR_LDID ||
          WN_operator (wn) == OPR_ILOAD) {
        WN *zero = WN_Intconst (WN_rtype (wn), 0);
        wn = WN_Relational (OPR_NE, MTYPE_I4, wn, zero);
      }
    }

    WFE_Stmt_Pop (wfe_stmk_comma);
    if (WN_first (comma_block)) {
      if (wn)
	wn = WN_CreateComma (OPR_COMMA, Mtype_comparison (mtype), MTYPE_V,
			     comma_block, wn);
      else
	WFE_Stmt_Append (comma_block, Get_Srcpos());
    }
    else
      WN_Delete (comma_block);
  }

  return wn;
} /* WFE_Expand_Expr_With_Sequence_Point */

static void
emit_barrier (bool type, tree list, INT32 k)
{
  INT32  i;
  WN    *wn = WN_CreateBarrier (type, k);

  for (i = 0; i < k; i++) {
    tree exp = TREE_VALUE (list);
    ST *st   = Get_ST (exp);
    WN_kid (wn, i) = WN_Lda (Pointer_Mtype, 0, st,
                             Make_Pointer_Type (ST_type (st), FALSE));
    list = TREE_CHAIN (list);
  }

  WFE_Stmt_Append (wn, Get_Srcpos());
} /* emit_barrier */

static WN *
emit_builtin_lock_test_and_set (tree exp, INT32 k)
{
  WN        *wn;
  WN        *arg_wn;
  WN        *ikids [2];
  TYPE_ID    obj_mtype;
  TY_IDX     arg_ty_idx;
  TYPE_ID    arg_mtype;
  tree       list = TREE_OPERAND (exp, 1);
  OPCODE     opc;
  INTRINSIC  iopc;

  obj_mtype  = TY_mtype (TY_pointed (Get_TY(TREE_TYPE(TREE_VALUE(list)))));
  arg_ty_idx = Get_TY(TREE_TYPE(TREE_VALUE(list)));
  arg_mtype  = TY_mtype (arg_ty_idx);
  arg_wn     = WFE_Expand_Expr (TREE_VALUE (list));
  arg_wn     = WN_CreateParm (arg_mtype, arg_wn, arg_ty_idx, WN_PARM_BY_VALUE);
  ikids [0]  = arg_wn;
  list       = TREE_CHAIN (list);
  arg_ty_idx = Get_TY(TREE_TYPE(TREE_VALUE(list)));
  arg_mtype  = TY_mtype (arg_ty_idx);
  arg_wn     = WFE_Expand_Expr (TREE_VALUE (list));
  arg_wn     = WN_CreateParm (arg_mtype, arg_wn, arg_ty_idx, WN_PARM_BY_VALUE);
  ikids [1]  = arg_wn;
  list       = TREE_CHAIN (list);

  if (obj_mtype == MTYPE_I4) {
    opc  = OPC_I4INTRINSIC_CALL;
    iopc = INTRN_LOCK_TEST_AND_SET_I4;
  }
  else
  if (obj_mtype == MTYPE_U4) {
    opc  = OPC_U4INTRINSIC_CALL;
    iopc = INTRN_LOCK_TEST_AND_SET_I4;
  }
  else
  if (obj_mtype == MTYPE_I8) {
    opc  = OPC_I8INTRINSIC_CALL;
    iopc = INTRN_LOCK_TEST_AND_SET_I8;
  }
  else
  if (obj_mtype == MTYPE_U8) {
    opc  = OPC_U8INTRINSIC_CALL;
    iopc = INTRN_LOCK_TEST_AND_SET_I8;
  }
  else {
    Fail_FmtAssertion ("unknown object type in __builtin_lock_test_and_set");
    opc  = OPCODE_UNKNOWN;
    iopc = INTRINSIC_NONE;
  }

  wn = WN_Create_Intrinsic (opc, iopc, 2, ikids);
  WFE_Stmt_Append (wn, Get_Srcpos());

  ST       *preg_st = MTYPE_To_PREG(obj_mtype);
  TY_IDX    preg_ty_idx = Be_Type_Tbl(obj_mtype);
  PREG_NUM  preg = Create_Preg (obj_mtype, NULL);

  wn = WN_Ldid (obj_mtype, -1, Return_Val_Preg, preg_ty_idx);
  wn = WN_Stid (obj_mtype, preg, preg_st, preg_ty_idx, wn),
  WFE_Stmt_Append (wn, Get_Srcpos());

  emit_barrier (FALSE, list, k);

  wn = WN_Ldid (obj_mtype, preg, preg_st, preg_ty_idx);

  return wn;
} /* emit_builtin_lock_test_and_set */

static void
emit_builtin_lock_release (tree exp, INT32 k)
{
  WN        *wn;
  WN        *arg_wn;
  WN        *ikids [1];
  TYPE_ID    obj_mtype;
  TY_IDX     arg_ty_idx;
  TYPE_ID    arg_mtype;
  tree       list = TREE_OPERAND (exp, 1);
  OPCODE     opc;
  INTRINSIC  iopc;

  obj_mtype  = TY_mtype (TY_pointed (Get_TY(TREE_TYPE(TREE_VALUE(list)))));
  arg_ty_idx = Get_TY(TREE_TYPE(TREE_VALUE(list)));
  arg_mtype  = TY_mtype (arg_ty_idx);
  arg_wn     = WFE_Expand_Expr (TREE_VALUE (list));
  arg_wn     = WN_CreateParm (arg_mtype, arg_wn, arg_ty_idx, WN_PARM_BY_VALUE);
  ikids [0]  = arg_wn;
  list       = TREE_CHAIN (list);

  emit_barrier (TRUE, list, k);

  opc = OPC_VINTRINSIC_CALL;
  if (obj_mtype == MTYPE_I4)
    iopc = INTRN_LOCK_RELEASE_I4;
  else
  if (obj_mtype == MTYPE_U4)
    iopc = INTRN_LOCK_RELEASE_I4;
  else
  if (obj_mtype == MTYPE_I8)
    iopc = INTRN_LOCK_RELEASE_I8;
  else
  if (obj_mtype == MTYPE_U8)
    iopc = INTRN_LOCK_RELEASE_I8;
  else {
    Fail_FmtAssertion ("unknown object type in __builtin_lock_test_and_set");
    opc  = OPCODE_UNKNOWN;
    iopc = INTRINSIC_NONE;
  }

  wn = WN_Create_Intrinsic (opc, iopc, 1, ikids);
  WFE_Stmt_Append (wn, Get_Srcpos());
} /* emit_builtin_lock_release */

static WN *
emit_builtin_compare_and_swap (tree exp, INT32 k)
{
  WN        *wn;
  WN        *arg_wn;
  WN        *ikids [3];
  TYPE_ID    obj_mtype;
  TY_IDX     arg_ty_idx;
  TYPE_ID    arg_mtype;
  tree       list = TREE_OPERAND (exp, 1);
  OPCODE     opc;
  INTRINSIC  iopc;

  obj_mtype  = TY_mtype (TY_pointed (Get_TY(TREE_TYPE(TREE_VALUE(list)))));
  arg_ty_idx = Get_TY(TREE_TYPE(TREE_VALUE(list)));
  arg_mtype  = TY_mtype (arg_ty_idx);
  arg_wn     = WFE_Expand_Expr (TREE_VALUE (list));
  arg_wn     = WN_CreateParm (arg_mtype, arg_wn, arg_ty_idx, WN_PARM_BY_VALUE);
  ikids [0]  = arg_wn;
  list       = TREE_CHAIN (list);
  arg_ty_idx = Get_TY(TREE_TYPE(TREE_VALUE(list)));
  arg_mtype  = TY_mtype (arg_ty_idx);
  arg_wn     = WFE_Expand_Expr (TREE_VALUE (list));
  arg_wn     = WN_CreateParm (arg_mtype, arg_wn, arg_ty_idx, WN_PARM_BY_VALUE);
  ikids [1]  = arg_wn;
  list       = TREE_CHAIN (list);
  arg_ty_idx = Get_TY(TREE_TYPE(TREE_VALUE(list)));
  arg_mtype  = TY_mtype (arg_ty_idx);
  arg_wn     = WFE_Expand_Expr (TREE_VALUE (list));
  arg_wn     = WN_CreateParm (arg_mtype, arg_wn, arg_ty_idx, WN_PARM_BY_VALUE);
  ikids [2]  = arg_wn;
  list       = TREE_CHAIN (list);

  emit_barrier (TRUE, list, k);

  opc = OPC_I4INTRINSIC_CALL;
  if (obj_mtype == MTYPE_I4)
    iopc = INTRN_COMPARE_AND_SWAP_I4;
  else
  if (obj_mtype == MTYPE_U4)
    iopc = INTRN_COMPARE_AND_SWAP_I4;
  else
  if (obj_mtype == MTYPE_I8)
    iopc = INTRN_COMPARE_AND_SWAP_I8;
  else
  if (obj_mtype == MTYPE_U8)
    iopc = INTRN_COMPARE_AND_SWAP_I8;
  else {
    Fail_FmtAssertion ("unknown object type in __builtin_lock_test_and_set");
    opc  = OPCODE_UNKNOWN;
    iopc = INTRINSIC_NONE;
  }

  wn = WN_Create_Intrinsic (opc, iopc, 3, ikids);
  WFE_Stmt_Append (wn, Get_Srcpos());

  ST       *preg_st = MTYPE_To_PREG(MTYPE_I4);
  TY_IDX    preg_ty_idx = Be_Type_Tbl(MTYPE_I4);
  PREG_NUM  preg = Create_Preg (MTYPE_I4, NULL);

  wn = WN_Ldid (MTYPE_I4, -1, Return_Val_Preg, preg_ty_idx);
  wn = WN_Stid (MTYPE_I4, preg, preg_st, preg_ty_idx, wn),
  WFE_Stmt_Append (wn, Get_Srcpos());

  emit_barrier (FALSE, list, k);

  wn = WN_Ldid (MTYPE_I4, preg, preg_st, preg_ty_idx);

  return wn;
} /* emit_builtin_compare_and_swap */

static void
emit_builtin_synchronize (tree exp, INT32 k)
{
  WN *wn;
  tree list = TREE_OPERAND (exp, 1);
  emit_barrier (TRUE,  list, k);
  wn = WN_Create_Intrinsic (OPC_VINTRINSIC_CALL, INTRN_SYNCHRONIZE, 0, NULL);
  WFE_Stmt_Append (wn, Get_Srcpos());
  emit_barrier (FALSE, list, k);
} /* emit_builtin_synchronize */

static char *
get_string_pointer (WN *wn)
{
  char *ptr = NULL;

  if (WN_operator (wn) == OPR_LDA) {
    ST *st = WN_st (wn);
    if (ST_class (st) == CLASS_CONST) {
      TCON tcon = Tcon_Table [ST_tcon (st)];
      if (TCON_ty (tcon) == MTYPE_STRING)
        ptr = ((char *) Targ_String_Address (tcon)) + WN_offset (wn);
    }
  }

  return ptr;
} /* get_string_pointer */

#ifdef KEY
// Bug 12781: code taken from unemitted_tinfo_decl_p() in gnu/cp/rtti.c.
// Return 1 if t is a typeinfo, 0 otherwise.
int
tinfo_decl_p (tree t)
{
  if (/* It's a var decl */
      TREE_CODE (t) == VAR_DECL
      /* whos name points back to itself */
      && IDENTIFIER_GLOBAL_VALUE (DECL_NAME (t)) == t
      /* and whose type is a struct */
      && TREE_CODE (TREE_TYPE (t)) == RECORD_TYPE
      /* with a field */
      && TYPE_FIELDS (TREE_TYPE (t))
      /* which is our pseudo type info */
      && TREE_TYPE (TYPE_FIELDS (TREE_TYPE (t))) == ti_desc_type_node)
    return 1;
  return 0;
}
#endif

// Auxiliary function for WFE_Expand_Expr, return the address of
// a tree operand.  (Used for ADDR_EXPR.)
WN*
WFE_Address_Of(tree arg0)
{
  enum tree_code code0 = TREE_CODE (arg0);
  ST *st = 0;
  WN* wn = 0;
  WN* wn0;
  WN* wn1;
  TY_IDX ty_idx;

  switch (code0) {
  case VAR_DECL:
  case PARM_DECL:
  case FUNCTION_DECL:
#ifdef KEY
  case RESULT_DECL:	// bug 3878
#endif
    {
      st = Get_ST (arg0);
      ty_idx = ST_type (st);
#ifdef KEY
      // Arg0 is the virtual function table (vtable) for a class.  Initialize
      // the table.
      if (code0 == VAR_DECL) {
	if (DECL_INITIAL(arg0) &&
#ifdef PATHSCALE_MERGE
            (DECL_VIRTUAL_P(arg0) || tinfo_decl_p(arg0) /* bug 12781: typeinfo ? */) &&
#else
	    DECL_VIRTUAL_P(arg0) &&
#endif
	    !DECL_EXTERNAL(arg0)) {
	  tree init = DECL_INITIAL(arg0);
	  if (TREE_CODE(init) != ERROR_MARK) {
	    FmtAssert (TREE_CODE(init) == CONSTRUCTOR,
		       ("Unexpected initializer for virtual table"));
	    WFE_Initialize_Decl(arg0);
	  }
	}
      }
#endif
      // for VLAs, use the base_st instead of st
      if (code0 == VAR_DECL &&
          st != ST_base(st)) {
        FmtAssert (ST_ofst (st) == 0,
                   ("Variable Length Arrays within struct not currently implemented"));
        wn = WN_Ldid (Pointer_Mtype, 0, ST_base(st), ST_type(ST_base(st)));
      }
      else
        if (!WFE_Keep_Zero_Length_Structs &&
            code0 == PARM_DECL            &&
            TY_mtype (ty_idx) == MTYPE_M  &&
            TY_size (ty_idx) == 0) {
          // taking address of zero length struct passed as parameter
          DevWarn ("taking address of zero length struct %s at line %d",
                   ST_name (st), lineno);
          wn = WN_Intconst (Pointer_Mtype, 0);
        }
        else
          wn = WN_Lda (Pointer_Mtype, ST_ofst(st), st);
    }
    break;

  case INDIRECT_REF:
    wn = WFE_Expand_Expr (TREE_OPERAND(arg0, 0));
    break;

  case STRING_CST:
    {
      TCON tcon;
      tcon = Host_To_Targ_String (MTYPE_STRING,
                                  const_cast<char*>TREE_STRING_POINTER(arg0),
                                  TREE_STRING_LENGTH(arg0));
      ty_idx = Get_TY(TREE_TYPE(arg0));
      st = New_Const_Sym (Enter_tcon (tcon), ty_idx);
      wn = WN_Lda (Pointer_Mtype, ST_ofst(st), st);
      TREE_STRING_ST (arg0) = st;
    }
    break;

  case CONSTRUCTOR:
    {
      st = WFE_Generate_Temp_For_Initialized_Aggregate (arg0, "");
      wn = WN_Lda (Pointer_Mtype, ST_ofst(st), st);
    }
    break;

  case LABEL_DECL:
    {
      DevWarn ("taking address of a label at line %d", lineno);
      LABEL_IDX label_idx = WFE_Get_LABEL (arg0, FALSE);
      wn = WN_LdaLabel (Pointer_Mtype, label_idx);
      Set_LABEL_addr_saved (label_idx);
    }
    break;

  case TARGET_EXPR:
    {
      WFE_Expand_Expr (arg0);
      st = Get_ST (TREE_OPERAND(arg0, 0));
      wn = WN_Lda (Pointer_Mtype, ST_ofst(st), st);
    }
    break;

  case COMPOUND_EXPR:
    {
#ifdef KEY
      wn = WFE_Expand_Expr(arg0);
      if (WN_operator(wn) == OPR_CSELECT) {
	// If WN is a CSELECT, get the ST from CSELECT's kids.  Bug 8472.
	//
	// Handle case where CSELECT's kid1 and kid2 are LDID or COMMAs that
	// return the same ST:
	//     ...
	//     U8U8LDID 0 <2,21,anon954> T<1402,anon_ptr.,8>
	//    U8COMMA
	//    U8U8LDID 0 <2,21,anon954> T<1402,anon_ptr.,8>
	//   U8CSELECT
	//
	// Handle other cases as they arise.
	int i;
	ST *st1 = NULL, *st2 = NULL;
	for (i = 0; i < 2; i++) {
	  WN *kid = (i == 0) ? WN_kid1(wn) : WN_kid2(wn);
	  WN *comma_kid1;
	  ST **st_ptr = (i == 0) ? &st1 : &st2;
	  switch (WN_operator(kid)) {
	    case OPR_LDID:
	      *st_ptr = WN_st(kid);
	      break;
	    case OPR_COMMA:
	      comma_kid1 = WN_kid1(kid);
	      Is_True(WN_operator(comma_kid1) == OPR_LDID,
		      ("WFE_Address_Of: kid1 of COMMA is not LDID"));
	      *st_ptr = WN_st(comma_kid1);
	      break;
	    default:
	      FmtAssert(FALSE, ("WFE_Address_Of: CSELECT kid NYI"));
	  }
	}
	Is_True((st1 != NULL) && (st1 == st2),
		("WFE_Address_Of: CSELECT kids returns different STs"));
	st = st1;
      } else
	st = WN_st(wn);
#else
      st = WN_st (WFE_Expand_Expr (arg0));
#endif
      wn = WN_Lda (Pointer_Mtype,  ST_ofst (st), st);
    }
    break;

  case NOP_EXPR:
    {
      wn = WFE_Address_Of(TREE_OPERAND(arg0, 0));
    }
    break;

  case MIN_EXPR:
  case MAX_EXPR:
    {
      // &(a <? b) or &(a >? b)
      tree op0 = TREE_OPERAND(arg0, 0);
      tree op1 = TREE_OPERAND(arg0, 1);
      WN* a = WFE_Expand_Expr(op0);
      WN* b = WFE_Expand_Expr(op1);
      FmtAssert(!WN_has_side_effects(a) && !WN_has_side_effects(b),
                ("Addr of MIN/MAX_EXPR with side effects not yet supported"));

      FmtAssert(same_type_p(TREE_TYPE(op0), TREE_TYPE(op1)),
                ("Types of MIN/MAX_EXPR operands differ"));
      TY_IDX  ptr_ty    = Make_Pointer_Type (Get_TY(TREE_TYPE(op0)), FALSE);
      TYPE_ID ptr_mtype = TY_mtype(ptr_ty);
      TY_IDX  arg_ty    = Get_TY(TREE_TYPE(TREE_OPERAND(arg0, 0)));
      TYPE_ID arg_mtype = TY_mtype(arg_ty);

      WN* aptr = WFE_Address_Of(op0);
      WN* bptr = WFE_Address_Of(op1);
      wn = WN_Select(Widen_Mtype(ptr_mtype),
                     WN_Relational(code0 == MIN_EXPR ? OPR_LT : OPR_GT,
                                   Widen_Mtype(arg_mtype),
                                   a, b),
                     aptr, bptr);
      Set_PU_has_very_high_whirl (Get_Current_PU ());
    }
    break;

  case COMPONENT_REF:
    {
      wn = WFE_Expand_Expr (arg0);
      ty_idx = Get_TY(TREE_TYPE(arg0));
      if (WN_operator (wn) == OPR_LDID) {
        WN_set_operator (wn, OPR_LDA);
        WN_set_desc (wn, MTYPE_V);
        WN_set_rtype(wn, Pointer_Mtype);
        WN_set_ty (wn, Make_Pointer_Type(WN_ty(wn))); // bug 10098, bug 10352
     }
      else
      if (WN_operator (wn) == OPR_ILOAD) {
        wn0 = WN_kid0 (wn);
        wn1 = WN_Intconst (Pointer_Mtype, WN_offset (wn));
        wn  = WN_Binary (OPR_ADD, Pointer_Mtype, wn0, wn1);
      }
      else
        Fail_FmtAssertion ("WFE_Address_Of has unhandled %s",
                           Operator_From_Tree [code0].name);
    }
    break;

#ifdef KEY // bug 3228
  case ARRAY_REF:
    wn = WFE_Expand_Expr (arg0);
    if (WN_operator(wn) == OPR_ILOAD) // bug 10105
      wn = WN_kid0(wn);
    ty_idx = Get_TY(TREE_TYPE(arg0));
    break;
#endif

#ifdef KEY
    case COMPOUND_LITERAL_EXPR:
    {
      arg0 = DECL_INITIAL (TREE_OPERAND (TREE_OPERAND (arg0, 0), 0));
      st = WFE_Generate_Temp_For_Initialized_Aggregate (arg0, "");
      wn = WN_Lda (Pointer_Mtype, ST_ofst(st), st);
    }
    break;

    // bug 2399
    case SAVE_EXPR:
    {
      st = WN_st (WFE_Expand_Expr (arg0));
      wn = WN_Lda (Pointer_Mtype,  ST_ofst (st), st);
    }
    break;

    // bug 5532, 5609
    case REALPART_EXPR:
    {
      wn = WFE_Expand_Expr (TREE_OPERAND (arg0, 0));
      if (WN_operator (wn) == OPR_ILOAD)
        wn = WN_kid0 (wn);
      else if (WN_operator (wn) == OPR_LDID)
	wn = WN_Lda (Pointer_Mtype, WN_offset(wn), WN_st(wn));
      else Fail_FmtAssertion ("WFE_Address_Of: NYI for REALPART_EXPR");
    }
    break;

    case IMAGPART_EXPR:
    {
      wn = WFE_Expand_Expr (TREE_OPERAND (arg0, 0));
      if (WN_operator (wn) == OPR_ILOAD)
      {
        wn0 = WN_kid0 (wn);
	TYPE_ID imag_mtype;
	switch (WN_rtype (wn))
	{
	  case MTYPE_C4:
	    imag_mtype = MTYPE_F4;
	    break;
	  case MTYPE_C8:
	    imag_mtype = MTYPE_F8;
	    break;
	  case MTYPE_C10:
	    imag_mtype = MTYPE_F10;
	    break;
	  case MTYPE_CQ:
	    imag_mtype = MTYPE_FQ;
	    break;
	  default:
	    Fail_FmtAssertion ("WFE_Address_Of: Unexpected rtype in IMAGPART_EXPR");
	}
	INT ofst;
	if (imag_mtype == MTYPE_FQ)
	{
#ifdef TARG_X8664
	  if (Is_Target_32bit()) ofst = 12; else
#endif // TARG_X8664
	  ofst = 16;
	}
	else ofst = MTYPE_byte_size (imag_mtype);

	wn1 = WN_Intconst (Pointer_Mtype, ofst);
	wn  = WN_Binary (OPR_ADD, Pointer_Mtype, wn0, wn1);
      }
      else if (WN_operator (wn) == OPR_LDID)
	wn = WN_Lda (Pointer_Mtype, 
		     WN_offset(wn) + MTYPE_byte_size(WN_rtype(wn)) / 2,
		     WN_st(wn));
      else Fail_FmtAssertion ("WFE_Address_Of: NYI for IMAGPART_EXPR");
    }
    break;
#endif

  default:
    {
      Fail_FmtAssertion ("WFE_Address_Of: Unexpected operand %s",
                         Operator_From_Tree [code0].name);
    }
    break;
  }

  FmtAssert(wn != 0, ("WFE_Address_Of: null WHIRL tree for %s",
                      Operator_From_Tree [code0].name));
  return wn;
}

#ifdef TARG_X8664
/* expand a VA_ARG_EXPR node for scalar type according to X86-64 ABI and 
 * return the WHIRL node that represents the address to be dereferenced;
 * 'twice' is true is loading two consecutive parameters of the same type
 * because they belong to a struct; currently, twice is TRUE only if isfloat
 * is FALSE */
static WN *WFE_x8664_va_arg(WN *ap_wn, BOOL isfloat, TY_IDX ty_idx, BOOL twice)
{
  /* compare gp_offset with 48 or fp_offset with 176 */
  WN *wn0 = WN_Iload(MTYPE_I4, !isfloat ? 0 : 4, MTYPE_To_TY(MTYPE_I4), 
      		     WN_CopyNode(ap_wn));
  WN *wn1 = WN_Intconst(MTYPE_I4, (!isfloat ? 48 : 176) - (twice ? 8 : 0));
  WN *wn = WN_Relational(OPR_GE, MTYPE_I4, wn0, wn1);
  LABEL_IDX lab1;
  New_LABEL (CURRENT_SYMTAB, lab1);
  WN *lab1_wn = WN_CreateLabel ((ST_IDX) 0, lab1, 0, NULL);
  wn = WN_CreateTruebr(lab1, wn);
  WFE_Stmt_Append (wn, Get_Srcpos ());

  ST *arg_temp_st = Gen_Temp_Symbol(Make_Pointer_Type(ty_idx), ".va_arg");
  WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, arg_temp_st);
  /* compute reg_save_area+gp_offset/fp_offset and store to arg_temp_st */
  wn0 = WN_Iload(MTYPE_I4, !isfloat ? 0 : 4, MTYPE_To_TY(MTYPE_I4), 
      		 WN_CopyNode(ap_wn));
  wn1 = WN_Iload(Pointer_Mtype, 16, MTYPE_To_TY(Pointer_Mtype), 
		 WN_CopyNode(ap_wn));
  wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
  wn = WN_Stid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx), wn);
  WFE_Stmt_Append (wn, Get_Srcpos ());

  /* increment gp_offset by 8 or fp_offset by 16 */
  wn0 = WN_Iload(MTYPE_I4, !isfloat ? 0 : 4, MTYPE_To_TY(MTYPE_I4), 
      		 WN_CopyNode(ap_wn));
  wn1 = WN_Intconst(MTYPE_I4, (!isfloat ? 8 : 16) * ((INT)twice+1));
  wn = WN_Binary(OPR_ADD, MTYPE_I4, wn0, wn1);
  wn = WN_Istore(MTYPE_I4, !isfloat ? 0 : 4, 
		 Make_Pointer_Type(MTYPE_To_TY(MTYPE_I4)), 
		 WN_CopyNode(ap_wn), wn);
  WFE_Stmt_Append (wn, Get_Srcpos ());

  /* branch around next part */
  LABEL_IDX lab2;
  New_LABEL (CURRENT_SYMTAB, lab2);
  WN *lab2_wn = WN_CreateLabel ((ST_IDX) 0, lab2, 0, NULL);
  wn = WN_CreateGoto(lab2);
  WFE_Stmt_Append (wn, Get_Srcpos ());

  WFE_Stmt_Append (lab1_wn, Get_Srcpos ());

  /* store overflow_arg_area pointer to arg_temp_st */
  wn0 = WN_Iload(Pointer_Mtype, 8, Make_Pointer_Type(ty_idx), 
		 WN_CopyNode(ap_wn));
  wn = WN_Stid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx), wn0);
  WFE_Stmt_Append (wn, Get_Srcpos ());

  /* increment overflow_arg_area pointer by 8 */
  wn0 = WN_Iload(Pointer_Mtype, 8, MTYPE_To_TY(Pointer_Mtype), 
		 WN_CopyNode(ap_wn));
  wn1 = WN_Intconst(MTYPE_U8, twice ? 16 : 8);
  wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
  wn = WN_Istore(Pointer_Mtype, 8,Make_Pointer_Type(MTYPE_To_TY(Pointer_Mtype)),
		 WN_CopyNode(ap_wn), wn);
  WFE_Stmt_Append (wn, Get_Srcpos ());

  WFE_Stmt_Append (lab2_wn, Get_Srcpos ());

  return WN_Ldid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx));
}

/* expand a VA_ARG_EXPR node for struct type being passed in 2 different classes
 * of registers, according to X86-64 ABI and return the WHIRL node that 
 * represents the address to be dereferenced; this requires allocating a
 * temporary for assembling the struct if passed in registers; isfloat0 is 
 * for the first 8-byte and isfloat1 is for the second 8-byte  */
static WN *WFE_x8664_va_arg_2_mixed(WN *ap_wn, BOOL isfloat0, BOOL isfloat1, 
				    TY_IDX ty_idx)
{
  /* compare gp_offset with 48 */
  WN *wn0 = WN_Iload(MTYPE_I4, 0, MTYPE_To_TY(MTYPE_I4), WN_CopyNode(ap_wn));
  WN *wn1 = WN_Intconst(MTYPE_I4, 48);
  WN *wn = WN_Relational(OPR_GE, MTYPE_I4, wn0, wn1);
  LABEL_IDX lab1;
  New_LABEL (CURRENT_SYMTAB, lab1);
  WN *lab1_wn = WN_CreateLabel ((ST_IDX) 0, lab1, 0, NULL);
  wn = WN_CreateTruebr(lab1, wn);
  WFE_Stmt_Append (wn, Get_Srcpos ());
  /* compare fp_offset with 176 */
  wn0 = WN_Iload(MTYPE_I4, 4, MTYPE_To_TY(MTYPE_I4), WN_CopyNode(ap_wn));
  wn1 = WN_Intconst(MTYPE_I4, 176);
  wn = WN_Relational(OPR_GE, MTYPE_I4, wn0, wn1);
  wn = WN_CreateTruebr(lab1, wn);
  WFE_Stmt_Append (wn, Get_Srcpos ());

  /* allocate a temporary location to assemble the structure value */
  ST *struct_temp_st = Gen_Temp_Symbol(ty_idx, ".va_arg_struct");
  WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, struct_temp_st);

  /* compute reg_save_area+gp_offset and store dereferenced value to 
   * struct_temp_st */
  wn0 = WN_Iload(MTYPE_I4, 0, MTYPE_To_TY(MTYPE_I4), WN_CopyNode(ap_wn));
  wn1 = WN_Iload(Pointer_Mtype, 16, MTYPE_To_TY(Pointer_Mtype), 
		 WN_CopyNode(ap_wn));
  wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
  wn = WN_Iload(MTYPE_I8, 0, MTYPE_To_TY(MTYPE_I8), wn);
  wn = WN_Stid(MTYPE_I8, isfloat0 ? 8 : 0, struct_temp_st, 
	       MTYPE_To_TY(MTYPE_I8), wn);
  WFE_Stmt_Append (wn, Get_Srcpos ());
  /* compute reg_save_area+fp_offset and store dereferenced value to 
   * struct_temp_st */
  wn0 = WN_Iload(MTYPE_I4, 4, MTYPE_To_TY(MTYPE_I4), WN_CopyNode(ap_wn));
  wn1 = WN_Iload(Pointer_Mtype, 16, MTYPE_To_TY(Pointer_Mtype), 
		 WN_CopyNode(ap_wn));
  wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
  wn = WN_Iload(MTYPE_F8, 0, MTYPE_To_TY(MTYPE_F8), wn);
  wn = WN_Stid(MTYPE_F8, isfloat0 ? 0 : 8, struct_temp_st, 
	       MTYPE_To_TY(MTYPE_F8), wn);
  WFE_Stmt_Append (wn, Get_Srcpos ());

  /* increment gp_offset by 8 */
  wn0 = WN_Iload(MTYPE_I4, 0, MTYPE_To_TY(MTYPE_I4), WN_CopyNode(ap_wn));
  wn1 = WN_Intconst(MTYPE_I4, 8);
  wn = WN_Binary(OPR_ADD, MTYPE_I4, wn0, wn1);
  wn = WN_Istore(MTYPE_I4, 0, Make_Pointer_Type(MTYPE_To_TY(MTYPE_I4)), 
		 WN_CopyNode(ap_wn), wn);
  WFE_Stmt_Append (wn, Get_Srcpos ());
  /* increment fp_offset by 16 */
  wn0 = WN_Iload(MTYPE_I4, 4, MTYPE_To_TY(MTYPE_I4), WN_CopyNode(ap_wn));
  wn1 = WN_Intconst(MTYPE_I4, 16);
  wn = WN_Binary(OPR_ADD, MTYPE_I4, wn0, wn1);
  wn = WN_Istore(MTYPE_I4, 4, Make_Pointer_Type(MTYPE_To_TY(MTYPE_I4)), 
		 WN_CopyNode(ap_wn), wn);
  WFE_Stmt_Append (wn, Get_Srcpos ());

  /* put the address of struct_temp_st in arg_temp_st */
  ST *arg_temp_st = Gen_Temp_Symbol(Make_Pointer_Type(ty_idx), ".va_arg");
  WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, arg_temp_st);
  wn = WN_Lda(Pointer_Mtype, 0, struct_temp_st, 0);
  Set_ST_addr_saved(struct_temp_st);
  wn = WN_Stid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx), wn);
  WFE_Stmt_Append (wn, Get_Srcpos ());

  /* branch around next part */
  LABEL_IDX lab2;
  New_LABEL (CURRENT_SYMTAB, lab2);
  WN *lab2_wn = WN_CreateLabel ((ST_IDX) 0, lab2, 0, NULL);
  wn = WN_CreateGoto(lab2);
  WFE_Stmt_Append (wn, Get_Srcpos ());

  WFE_Stmt_Append (lab1_wn, Get_Srcpos ());

  /* store overflow_arg_area pointer to arg_temp_st */
  wn0 = WN_Iload(Pointer_Mtype, 8, Make_Pointer_Type(ty_idx), 
		 WN_CopyNode(ap_wn));
  wn = WN_Stid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx), wn0);
  WFE_Stmt_Append (wn, Get_Srcpos ());

  /* increment overflow_arg_area pointer by 16 */
  wn0 = WN_Iload(Pointer_Mtype, 8, MTYPE_To_TY(Pointer_Mtype), 
		 WN_CopyNode(ap_wn));
  wn1 = WN_Intconst(MTYPE_U8, 16);
  wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
  wn = WN_Istore(Pointer_Mtype, 8,Make_Pointer_Type(MTYPE_To_TY(Pointer_Mtype)),
		 WN_CopyNode(ap_wn), wn);
  WFE_Stmt_Append (wn, Get_Srcpos ());

  WFE_Stmt_Append (lab2_wn, Get_Srcpos ());

  return WN_Ldid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx));
}

/* expand a VA_ARG_EXPR node for struct type being passed in 2 float
 * registers, according to X86-64 ABI and return the WHIRL node that 
 * represents the address to be dereferenced; this requires allocating a
 * temporary for assembling the struct if passed in registers, because each
 * float register is saved into 128 bit locations */
static WN *WFE_x8664_va_arg_2_float(WN *ap_wn, TY_IDX ty_idx)
{
  LABEL_IDX lab1;
  New_LABEL (CURRENT_SYMTAB, lab1);
  WN *lab1_wn = WN_CreateLabel ((ST_IDX) 0, lab1, 0, NULL);
  /* compare fp_offset with 160 (176 - 16) */
  WN *wn0 = WN_Iload(MTYPE_I4, 4, MTYPE_To_TY(MTYPE_I4), WN_CopyNode(ap_wn));
  WN *wn1 = WN_Intconst(MTYPE_I4, 160);
  WN *wn = WN_Relational(OPR_GE, MTYPE_I4, wn0, wn1);
  wn = WN_CreateTruebr(lab1, wn);
  WFE_Stmt_Append (wn, Get_Srcpos ());

  /* allocate a temporary location to assemble the structure value */
  ST *struct_temp_st = Gen_Temp_Symbol(ty_idx, ".va_arg_struct");
  WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, struct_temp_st);

  /* compute reg_save_area+fp_offset and store 1st dereferenced value to 
   * struct_temp_st */
  wn0 = WN_Iload(MTYPE_I4, 4, MTYPE_To_TY(MTYPE_I4), WN_CopyNode(ap_wn));
  wn1 = WN_Iload(Pointer_Mtype, 16, MTYPE_To_TY(Pointer_Mtype), 
		 WN_CopyNode(ap_wn));
  wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
  wn = WN_Iload(MTYPE_F8, 0, MTYPE_To_TY(MTYPE_F8), wn);
  wn = WN_Stid(MTYPE_F8, 0, struct_temp_st, MTYPE_To_TY(MTYPE_F8), wn);
  WFE_Stmt_Append (wn, Get_Srcpos ());
  /* compute reg_save_area+fp_offset and store 2nd dereferenced value to 
   * struct_temp_st */
  wn0 = WN_Iload(MTYPE_I4, 4, MTYPE_To_TY(MTYPE_I4), WN_CopyNode(ap_wn));
  wn1 = WN_Iload(Pointer_Mtype, 16, MTYPE_To_TY(Pointer_Mtype), 
		 WN_CopyNode(ap_wn));
  wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
  wn = WN_Iload(MTYPE_F8, 16, MTYPE_To_TY(MTYPE_F8), wn);
  wn = WN_Stid(MTYPE_F8, 8, struct_temp_st, MTYPE_To_TY(MTYPE_F8), wn);
  WFE_Stmt_Append (wn, Get_Srcpos ());

  /* increment fp_offset by 32 */
  wn0 = WN_Iload(MTYPE_I4, 4, MTYPE_To_TY(MTYPE_I4), WN_CopyNode(ap_wn));
  wn1 = WN_Intconst(MTYPE_I4, 32);
  wn = WN_Binary(OPR_ADD, MTYPE_I4, wn0, wn1);
  wn = WN_Istore(MTYPE_I4, 4, Make_Pointer_Type(MTYPE_To_TY(MTYPE_I4)), 
		 WN_CopyNode(ap_wn), wn);
  WFE_Stmt_Append (wn, Get_Srcpos ());

  /* put the address of struct_temp_st in arg_temp_st */
  ST *arg_temp_st = Gen_Temp_Symbol(Make_Pointer_Type(ty_idx), ".va_arg");
  WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, arg_temp_st);
  wn = WN_Lda(Pointer_Mtype, 0, struct_temp_st, 0);
  Set_ST_addr_saved(struct_temp_st);
  wn = WN_Stid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx), wn);
  WFE_Stmt_Append (wn, Get_Srcpos ());

  /* branch around next part */
  LABEL_IDX lab2;
  New_LABEL (CURRENT_SYMTAB, lab2);
  WN *lab2_wn = WN_CreateLabel ((ST_IDX) 0, lab2, 0, NULL);
  wn = WN_CreateGoto(lab2);
  WFE_Stmt_Append (wn, Get_Srcpos ());

  WFE_Stmt_Append (lab1_wn, Get_Srcpos ());

  /* store overflow_arg_area pointer to arg_temp_st */
  wn0 = WN_Iload(Pointer_Mtype, 8, Make_Pointer_Type(ty_idx), 
		 WN_CopyNode(ap_wn));
  wn = WN_Stid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx), wn0);
  WFE_Stmt_Append (wn, Get_Srcpos ());

  /* increment overflow_arg_area pointer by 16 */
  wn0 = WN_Iload(Pointer_Mtype, 8, MTYPE_To_TY(Pointer_Mtype), 
		 WN_CopyNode(ap_wn));
  wn1 = WN_Intconst(MTYPE_U8, 16);
  wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
  wn = WN_Istore(Pointer_Mtype, 8,Make_Pointer_Type(MTYPE_To_TY(Pointer_Mtype)),
		 WN_CopyNode(ap_wn), wn);
  WFE_Stmt_Append (wn, Get_Srcpos ());

  WFE_Stmt_Append (lab2_wn, Get_Srcpos ());

  return WN_Ldid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx));
}
#endif

#ifdef KEY
static bool inside_eh_region = false;
// Setup an EH region, typically across a function call.
void
Setup_EH_Region (bool for_unwinding)
{
    WN * region_body;

    if (for_unwinding)
	region_body = WFE_Stmt_Pop (wfe_stmk_region_body);
    else
    	{
	    region_body = WFE_Stmt_Pop (wfe_stmk_call_region_body);
	    inside_eh_region = false;
	}
    INITV_IDX iv;
    LABEL_IDX pad = 0;

    if (!for_unwinding) pad = lookup_cleanups (iv);
    else
    {
	iv = New_INITV();
        INITV_Set_ZERO (Initv_Table[iv], MTYPE_U4, 1);
    }

    INITV_IDX initv_label = New_INITV();
    if (pad)
    	INITV_Init_Label (initv_label, pad, 1);
    else
	INITV_Set_ZERO (Initv_Table[initv_label], MTYPE_U4, 1);
    INITV_IDX blk = New_INITV();
    INITV_Init_Block (blk, initv_label);

    Set_INITV_next (initv_label, iv);

    TY_IDX ty = MTYPE_TO_TY_array[MTYPE_U4];
    ST * ereg = Gen_Temp_Named_Symbol (ty, "dummy1", CLASS_VAR,
				SCLASS_EH_REGION_SUPP);
    Set_ST_is_initialized (*ereg);
    Set_ST_is_not_used (*ereg);
    INITO_IDX ereg_supp = New_INITO (ST_st_idx(ereg), blk);

    WFE_Stmt_Append (WN_CreateRegion (REGION_KIND_EH, region_body,
      WN_CreateBlock(), WN_CreateBlock(), New_Region_Id(), ereg_supp), Get_Srcpos());
    Set_PU_has_region (Get_Current_PU());
    Set_PU_has_exc_scopes (Get_Current_PU());
    
    // The following code creat a new TY for the ST that is created 
    // above. Because in CG, we will get the size of the ST from its
    // TY, we should get its right size from the INITO attached with 
    // it, and write it into a new TY
    TY_IDX tyi;            
    TY& zty = New_TY (tyi);
    UINT inito_size = Get_INITO_Size(ereg_supp);
    TY_Init (zty, inito_size, KIND_STRUCT, MTYPE_M,ereg->u1.name_idx);
    Set_TY_align (tyi, 4);
    ST_Init (ereg, TY_name_idx (zty),
	     CLASS_VAR, SCLASS_EH_REGION_SUPP, EXPORT_LOCAL, tyi);
    Set_ST_is_initialized (ereg);
}
#endif // KEY

static TY_IDX
get_field_type (TY_IDX struct_type, UINT field_id)
{
  Is_True (TY_kind (struct_type) == KIND_STRUCT, ("expecting KIND_STRUCT"));
  UINT cur_field_id = 0;
  FLD_HANDLE fld = FLD_get_to_field (struct_type, field_id, cur_field_id);
  Is_True (! fld.Is_Null(), ("Invalid field id %d for type 0x%x",
                          field_id, struct_type));
  return FLD_type (fld);
}

#ifdef TARG_X8664
// Handle GNU x86 builtins
static WN *
WFE_target_builtins (tree exp, INTRINSIC * iopc, BOOL * intrinsic_op)
{
  WN * wn = NULL;
                                                                                
  // Assumption: we would be generating intrinsics for most of the builtins
  *intrinsic_op = TRUE;
                                                                                
  tree func = TREE_OPERAND (TREE_OPERAND (exp, 0), 0);
  Is_True (TREE_CODE (func) == FUNCTION_DECL && DECL_BUILT_IN (func) &&
           DECL_BUILT_IN_CLASS (func) == BUILT_IN_MD, ("Invalid tree node"));
                                                                                
  unsigned int ins_code = DECL_FUNCTION_CODE (func);
  TYPE_ID res_type = TY_mtype(Get_TY(TREE_TYPE(exp)));
  tree t_list = TREE_OPERAND (exp, 1);
  WN * arg0 = NULL, * arg1 = NULL;
  if (t_list)
  {
    // Assumption: every builtin has 2 kids: this will change
    arg0 = WFE_Expand_Expr (TREE_VALUE (t_list));
    if (TREE_CHAIN (t_list))
      arg1 = WFE_Expand_Expr (TREE_VALUE (TREE_CHAIN (t_list)));
  }
                                                                                
  switch (ins_code)
  {
    // Generate WN
    case IX86_BUILTIN_PADDB:
    case IX86_BUILTIN_PADDW:
    case IX86_BUILTIN_PADDD:
    case IX86_BUILTIN_ADDPD:
    case IX86_BUILTIN_PADDB128:
    case IX86_BUILTIN_PADDW128:
    case IX86_BUILTIN_PADDD128:
      wn = WN_Add (res_type, arg0, arg1);
      *intrinsic_op = FALSE;
      break;
    case IX86_BUILTIN_PSUBB:
    case IX86_BUILTIN_PSUBW:
    case IX86_BUILTIN_PSUBD:
    case IX86_BUILTIN_SUBPD:
    case IX86_BUILTIN_PSUBB128:
    case IX86_BUILTIN_PSUBW128:
    case IX86_BUILTIN_PSUBD128:
      wn = WN_Sub (res_type, arg0, arg1);
      *intrinsic_op = FALSE;
      break;
    case IX86_BUILTIN_PAND:
      wn = WN_Band (res_type, arg0, arg1);
      *intrinsic_op = FALSE;
      break;
    case IX86_BUILTIN_PANDN:
      wn = WN_Band (res_type, WN_Bnot (res_type, arg0), arg1);
      *intrinsic_op = FALSE;
      break;
    case IX86_BUILTIN_POR:
      wn = WN_Bior (res_type, arg0, arg1);
      *intrinsic_op = FALSE;
      break;
    case IX86_BUILTIN_PXOR:
      wn = WN_Bxor (res_type, arg0, arg1);
      *intrinsic_op = FALSE;
      break;
                                                                                
    // Generate intrinsics to be expanded in CG expand
    case IX86_BUILTIN_PADDSB:
      *iopc = INTRN_PADDSB;
      break;
    case IX86_BUILTIN_PADDSW:
      *iopc = INTRN_PADDSW;
      break;
    case IX86_BUILTIN_PSUBSB:
      *iopc = INTRN_PSUBSB;
      break;
    case IX86_BUILTIN_PSUBSW:
      *iopc = INTRN_PSUBSW;
      break;
    case IX86_BUILTIN_PADDUSB:
      *iopc = INTRN_PADDUSB;
      break;
    case IX86_BUILTIN_PADDUSW:
      *iopc = INTRN_PADDUSW;
      break;
    case IX86_BUILTIN_PSUBUSB:
      *iopc = INTRN_PSUBUSB;
      break;
    case IX86_BUILTIN_PSUBUSW:
      *iopc = INTRN_PSUBUSW;
      break;
    case IX86_BUILTIN_PMULLW:
      *iopc = INTRN_PMULLW;
      break;
    case IX86_BUILTIN_PMULHW:
      *iopc = INTRN_PMULHW;
      break;
    case IX86_BUILTIN_PCMPEQB:
      *iopc = INTRN_PCMPEQB;
      break;
    case IX86_BUILTIN_PCMPEQW:
      *iopc = INTRN_PCMPEQW;
      break;
    case IX86_BUILTIN_PCMPEQD:
      *iopc = INTRN_PCMPEQD;
      break;
    case IX86_BUILTIN_PCMPGTB:
      *iopc = INTRN_PCMPGTB;
      break;
    case IX86_BUILTIN_PCMPGTW:
      *iopc = INTRN_PCMPGTW;
      break;
    case IX86_BUILTIN_PCMPGTD:
      *iopc = INTRN_PCMPGTD;
      break;
    case IX86_BUILTIN_PUNPCKHBW:
      *iopc = INTRN_PUNPCKHBW;
      break;
    case IX86_BUILTIN_PUNPCKHWD:
      *iopc = INTRN_PUNPCKHWD;
      break;
    case IX86_BUILTIN_PUNPCKHDQ:
      *iopc = INTRN_PUNPCKHDQ;
      break;
    case IX86_BUILTIN_PUNPCKLBW:
      *iopc = INTRN_PUNPCKLBW;
      break;
    case IX86_BUILTIN_PUNPCKLWD:
      *iopc = INTRN_PUNPCKLWD;
      break;
    case IX86_BUILTIN_PUNPCKLDQ:
      *iopc = INTRN_PUNPCKLDQ;
      break;
    case IX86_BUILTIN_PACKSSWB:
      *iopc = INTRN_PACKSSWB;
      break;
    case IX86_BUILTIN_PACKSSDW:
      *iopc = INTRN_PACKSSDW;
      break;
    case IX86_BUILTIN_PACKUSWB:
      *iopc = INTRN_PACKUSWB;
      break;
    case IX86_BUILTIN_PMULHUW:
      *iopc = INTRN_PMULHUW;
      break;
    case IX86_BUILTIN_PAVGB:
      *iopc = INTRN_PAVGB;
      break;
    case IX86_BUILTIN_PAVGW:
      *iopc = INTRN_PAVGW;
      break;
    case IX86_BUILTIN_PSADBW:
      *iopc = INTRN_PSADBW;
      break;
    case IX86_BUILTIN_PMAXUB:
      *iopc = INTRN_PMAXUB;
      break;
    case IX86_BUILTIN_PMAXSW:
      *iopc = INTRN_PMAXSW;
      break;
    case IX86_BUILTIN_PMINUB:
      *iopc = INTRN_PMINUB;
      break;
    case IX86_BUILTIN_PMINSW:
      *iopc = INTRN_PMINSW;
      break;
    case IX86_BUILTIN_PEXTRW:
      {
        Is_True (TREE_CODE (TREE_VALUE (TREE_CHAIN (t_list))) == INTEGER_CST,
                 ("Immediate value required by pextrw"));
        UINT val = Get_Integer_Value (TREE_VALUE (TREE_CHAIN (t_list)));
        switch (val)
        {
          case 0:
            *iopc = INTRN_PEXTRW0;
            break;
          case 1:
            *iopc = INTRN_PEXTRW1;
            break;
          case 2:
            *iopc = INTRN_PEXTRW2;
            break;
          case 3:
            *iopc = INTRN_PEXTRW3;
            break;
          default:
            Fail_FmtAssertion ("Invalid imm value %d to pextrw", val);
        }
        TY_IDX arg_ty_idx = Get_TY(TREE_TYPE(TREE_VALUE(t_list)));
        TYPE_ID arg_mtype  = TY_mtype(arg_ty_idx);
        arg0     = WN_CreateParm (Mtype_comparison (arg_mtype), arg0,
                                  arg_ty_idx, WN_PARM_BY_VALUE);
        wn = WN_Create_Intrinsic (OPR_INTRINSIC_OP, MTYPE_U4, MTYPE_V,
                                      *iopc, 1, &arg0);
        break;
      }
    case IX86_BUILTIN_PINSRW:
      {
        Is_True (TREE_CODE (TREE_VALUE (TREE_CHAIN (TREE_CHAIN (t_list)))) == INTEGER_CST, ("Immediate value required by pinsrw"));
        UINT val = Get_Integer_Value (TREE_VALUE (TREE_CHAIN (TREE_CHAIN (t_list))));
        switch (val)
        {
          case 0:
            *iopc = INTRN_PINSRW0;
            break;
          case 1:
            *iopc = INTRN_PINSRW1;
            break;
          case 2:
            *iopc = INTRN_PINSRW2;
            break;
          case 3:
            *iopc = INTRN_PINSRW3;
            break;
          default:
            Fail_FmtAssertion ("Invalid imm value %d to pinsrw", val);
                                                                                
        }
        WN * args[2];
        for (int c=0; c<2; c++)
        {
            TY_IDX arg_ty_idx = Get_TY (TREE_TYPE (TREE_VALUE (t_list)));
            TYPE_ID arg_mtype = TY_mtype (arg_ty_idx);
            args[c] = WN_CreateParm (Mtype_comparison (arg_mtype), arg0,
                                        arg_ty_idx, WN_PARM_BY_VALUE);
            t_list = TREE_CHAIN (t_list);
            arg0 = arg1;
        }
                                                                                
        wn = WN_Create_Intrinsic (OPR_INTRINSIC_OP, MTYPE_M8I2, MTYPE_V,
                                  *iopc, 2, args);
        break;
      }
    case IX86_BUILTIN_PMOVMSKB:
      *iopc = INTRN_PMOVMSKB;
      break;
    case IX86_BUILTIN_ADDPS:
      *iopc = INTRN_ADDPS;
      break;
    case IX86_BUILTIN_SUBPS:
      *iopc = INTRN_SUBPS;
      break;
    case IX86_BUILTIN_MULPS:
      *iopc = INTRN_MULPS;
      break;
    case IX86_BUILTIN_DIVPS:
      *iopc = INTRN_DIVPS;
      break;
    case IX86_BUILTIN_ADDSS:
      *iopc = INTRN_ADDSS;
      break;
    case IX86_BUILTIN_SUBSS:
      *iopc = INTRN_SUBSS;
      break;
    case IX86_BUILTIN_MULSS:
      *iopc = INTRN_MULSS;
      break;
    case IX86_BUILTIN_DIVSS:
      *iopc = INTRN_DIVSS;
      break;
    case IX86_BUILTIN_CMPEQPS:
      *iopc = INTRN_CMPEQPS;
      break;
    case IX86_BUILTIN_CMPLTPS:
      *iopc = INTRN_CMPLTPS;
      break;
    case IX86_BUILTIN_CMPLEPS:
      *iopc = INTRN_CMPLEPS;
      break;
    case IX86_BUILTIN_CMPGTPS:
      *iopc = INTRN_CMPGTPS;
      break;
    case IX86_BUILTIN_CMPGEPS:
      *iopc = INTRN_CMPGEPS;
      break;
    case IX86_BUILTIN_CMPUNORDPS:
      *iopc = INTRN_CMPUNORDPS;
      break;
    case IX86_BUILTIN_CMPNEQPS:
      *iopc = INTRN_CMPNEQPS;
      break;
    case IX86_BUILTIN_CMPNLTPS:
      *iopc = INTRN_CMPNLTPS;
      break;
    case IX86_BUILTIN_CMPNLEPS:
      *iopc = INTRN_CMPNLEPS;
      break;
    case IX86_BUILTIN_CMPNGTPS:
      *iopc = INTRN_CMPNGTPS;
      break;
    case IX86_BUILTIN_CMPNGEPS:
      *iopc = INTRN_CMPNGEPS;
      break;
    case IX86_BUILTIN_CMPORDPS:
      *iopc = INTRN_CMPORDPS;
      break;
    case IX86_BUILTIN_CMPEQSS:
      *iopc = INTRN_CMPEQSS;
      break;
    case IX86_BUILTIN_CMPLTSS:
      *iopc = INTRN_CMPLTSS;
      break;
    case IX86_BUILTIN_CMPLESS:
      *iopc = INTRN_CMPLESS;
      break;
    case IX86_BUILTIN_CMPUNORDSS:
      *iopc = INTRN_CMPUNORDSS;
      break;
    case IX86_BUILTIN_CMPNEQSS:
      *iopc = INTRN_CMPNEQSS;
      break;
    case IX86_BUILTIN_CMPNLTSS:
      *iopc = INTRN_CMPNLTSS;
      break;
    case IX86_BUILTIN_CMPNLESS:
      *iopc = INTRN_CMPNLESS;
      break;
    case IX86_BUILTIN_CMPORDSS:
      *iopc = INTRN_CMPORDSS;
      break;
    case IX86_BUILTIN_MAXPS:
      *iopc = INTRN_MAXPS;
      break;
    case IX86_BUILTIN_MAXSS:
      *iopc = INTRN_MAXSS;
      break;
    case IX86_BUILTIN_MINPS:
      *iopc = INTRN_MINPS;
      break;
    case IX86_BUILTIN_MINSS:
      *iopc = INTRN_MINSS;
      break;
    case IX86_BUILTIN_ANDPS:
      *iopc = INTRN_ANDPS;
      break;
    case IX86_BUILTIN_ANDNPS:
      *iopc = INTRN_ANDNPS;
      break;
    case IX86_BUILTIN_ORPS:
      *iopc = INTRN_ORPS;
      break;
    case IX86_BUILTIN_XORPS:
      *iopc = INTRN_XORPS;
      break;
    case IX86_BUILTIN_MOVSS:
      *iopc = INTRN_MOVSS;
      break;
    case IX86_BUILTIN_MOVHLPS:
      *iopc = INTRN_MOVHLPS;
      break;
    case IX86_BUILTIN_MOVLHPS:
      *iopc = INTRN_MOVLHPS;
      break;
    case IX86_BUILTIN_UNPCKHPS:
      *iopc = INTRN_UNPCKHPS;
      break;
    case IX86_BUILTIN_UNPCKLPS:
      *iopc = INTRN_UNPCKLPS;
      break;
    case IX86_BUILTIN_RCPPS:
      *iopc = INTRN_RCPPS;
      break;
    case IX86_BUILTIN_RSQRTPS:
      *iopc = INTRN_RSQRTPS;
      break;
    case IX86_BUILTIN_SQRTPS:
      *iopc = INTRN_SQRTPS;
      break;
    case IX86_BUILTIN_RCPSS:
      *iopc = INTRN_RCPSS;
      break;
    case IX86_BUILTIN_RSQRTSS:
      *iopc = INTRN_RSQRTSS;
      break;
    case IX86_BUILTIN_SQRTSS:
      *iopc = INTRN_SQRTSS;
      break;
    case IX86_BUILTIN_SHUFPS:
      *iopc = INTRN_SHUFPS;
      break;
    case IX86_BUILTIN_EMMS:
      *iopc = INTRN_EMMS;
      *intrinsic_op = FALSE;
      break;
    case IX86_BUILTIN_LOADAPS:
      *iopc = INTRN_LOADAPS;
      break;
    case IX86_BUILTIN_STOREAPS:
      *iopc = INTRN_STOREAPS;
      *intrinsic_op = FALSE;
      break;
    case IX86_BUILTIN_PXOR128:
      wn = WN_Bxor (res_type, arg0, arg1);
      *intrinsic_op = FALSE;
      break;
    case IX86_BUILTIN_PSLLDQI128:
      *iopc = INTRN_PSLLDQ;
      break;
    case IX86_BUILTIN_PSRLDQI128:
      *iopc = INTRN_PSRLDQ;
      break;
    case IX86_BUILTIN_PSLLW128:
      *iopc = INTRN_PSLLW;
      break;
    case IX86_BUILTIN_PSLLD128:
      *iopc = INTRN_PSLLD;
      break;
    case IX86_BUILTIN_PSLLQ128:
      *iopc = INTRN_PSLLQ;
      break;
    case IX86_BUILTIN_PSRLW128:
      *iopc = INTRN_PSRLW;
      break;
    case IX86_BUILTIN_PSRLD128:
      *iopc = INTRN_PSRLD;
      break;
    case IX86_BUILTIN_PSRLQ128:
      *iopc = INTRN_PSRLQ;
      break;
    case IX86_BUILTIN_PSRAW128:
      *iopc = INTRN_PSRAW;
      break;
    case IX86_BUILTIN_PSRAD128:
      *iopc = INTRN_PSRAD;
      break;
    case IX86_BUILTIN_PSRAWI128:
      *iopc = INTRN_PSRAW;
      break;
    case IX86_BUILTIN_PSRADI128:
      *iopc = INTRN_PSRAD;
      break;
    case IX86_BUILTIN_PSLLWI128:
      *iopc = INTRN_PSLLW;
      break;
    case IX86_BUILTIN_PSLLDI128:
      *iopc = INTRN_PSLLD;
      break;
    case IX86_BUILTIN_PSLLQI128:
      *iopc = INTRN_PSLLQ;
      break;
    case IX86_BUILTIN_PSRLWI128:
      *iopc = INTRN_PSRLW;
      break;
    case IX86_BUILTIN_PSRLDI128:
      *iopc = INTRN_PSRLD;
      break;
    case IX86_BUILTIN_PSRLQI128:
      *iopc = INTRN_PSRLQ;
      break;
    case IX86_BUILTIN_MOVNTDQ:
      *iopc = INTRN_MOVNTDQ;
      *intrinsic_op = FALSE;
      break;
    case IX86_BUILTIN_LOADD:
      *iopc = INTRN_LOADD;
      break;
    case IX86_BUILTIN_MOVNTPS:
      *iopc = INTRN_MOVNTPS;
      *intrinsic_op = FALSE;
      break;
    case IX86_BUILTIN_SSE_ZERO:
      *iopc = INTRN_SSE_ZERO;
      *intrinsic_op = FALSE;
      break;
    case IX86_BUILTIN_CLRTI:
      *iopc = INTRN_CLRTI;
      *intrinsic_op = FALSE;
      break;
    case IX86_BUILTIN_PSHUFD:
      *iopc = INTRN_PSHUFD;
      break;
    case IX86_BUILTIN_LOADSS:
      *iopc = INTRN_LOADSS;
      break;
    case IX86_BUILTIN_DIVPD:
      wn = WN_Div (res_type, arg0, arg1);
      *intrinsic_op = FALSE;
      break;
    case IX86_BUILTIN_MULPD:
      wn = WN_Mpy (res_type, arg0, arg1);
      *intrinsic_op = FALSE;
      break;
    case IX86_BUILTIN_SQRTPD:
      wn = WN_Sqrt (res_type, arg0);
      *intrinsic_op = FALSE;
      break;
    case IX86_BUILTIN_MINPD:
      wn = WN_Binary (OPR_MIN, res_type, arg0, arg1);
      *intrinsic_op = FALSE;
      break;
    case IX86_BUILTIN_MAXPD:
      wn = WN_Binary (OPR_MAX, res_type, arg0, arg1);
      *intrinsic_op = FALSE;
      break;
    case IX86_BUILTIN_SHUFPD:
      *iopc = INTRN_SHUFPD;
      break;
    case IX86_BUILTIN_XORPD:
      *iopc = INTRN_XORPD;
      break;
    case IX86_BUILTIN_ANDPD:
      *iopc = INTRN_ANDPD;
      break;
    case IX86_BUILTIN_ORPD:
      *iopc = INTRN_ORPD;
      break;
    case IX86_BUILTIN_STORELPD:
      *iopc = INTRN_STORELPD;
      *intrinsic_op = FALSE;
      break;
    case IX86_BUILTIN_STOREHPD:
      *iopc = INTRN_STOREHPD;
      *intrinsic_op = FALSE;
      break;
    case IX86_BUILTIN_LOADLPD:
      *iopc = INTRN_LOADLPD;
      break;
    case IX86_BUILTIN_LOADHPD:
      *iopc = INTRN_LOADHPD;
      break;
    case IX86_BUILTIN_UNPCKLPD:
      *iopc = INTRN_UNPCKLPD;
      break;
    case IX86_BUILTIN_UNPCKHPD:
      *iopc = INTRN_UNPCKHPD;
      break;
    case IX86_BUILTIN_LFENCE:
      *iopc = INTRN_LFENCE;
      *intrinsic_op = FALSE;
      break;
    case IX86_BUILTIN_MFENCE:
      *iopc = INTRN_MFENCE;
      *intrinsic_op = FALSE;
      break;
    case IX86_BUILTIN_SFENCE:
      *iopc = INTRN_SFENCE;
      *intrinsic_op = FALSE;
      break;
    case IX86_BUILTIN_PSHUFW:
      *iopc = INTRN_PSHUFW;
      break;
    case IX86_BUILTIN_LOADDQA:
      *iopc = INTRN_LOADDQA;
      break;
    case IX86_BUILTIN_LOADDQU:
      *iopc = INTRN_LOADDQU;
      break;
    case IX86_BUILTIN_STOREDQA:
      *iopc = INTRN_STOREDQA;
      *intrinsic_op = FALSE;
      break;
    case IX86_BUILTIN_STOREDQU:
      *iopc = INTRN_STOREDQU;
      *intrinsic_op = FALSE;
      break;
    case IX86_BUILTIN_MOVNTI:
      *iopc = INTRN_MOVNTI;
      *intrinsic_op = FALSE;
      break;
                                                                                
    default:
      if (Opt_Level > 0)
      { // Don't assert in front-end. If used, backend will assert.
        *iopc = INTRN_UNIMP_PURE;
        if (res_type == MTYPE_V)
        {
          *iopc = INTRN_UNIMP;
          *intrinsic_op = FALSE;
        }
      }
      else
      {
        *intrinsic_op = FALSE;
        // For simplicity, generate a U8 constant, and then use a cvt
        // if necessary. If void result type, generate a placeholder eval.
        wn = WN_Intconst (MTYPE_U8, 0);
        if (res_type != MTYPE_U8 && res_type != MTYPE_V)
          wn = WN_Cvt (MTYPE_U8, res_type, wn);
      }
      break;
  }
                                                                                
  // The following instructions expect both arguments as FP (xmm), but
  // the 2nd argument type for the corresponding builtin is INT, so we
  // need to insert a CVT here.
  switch (ins_code)
  {
    case IX86_BUILTIN_PSRAWI128:
    case IX86_BUILTIN_PSRADI128:
    case IX86_BUILTIN_PSLLWI128:
    case IX86_BUILTIN_PSLLDI128:
    case IX86_BUILTIN_PSLLQI128:
    case IX86_BUILTIN_PSRLWI128:
    case IX86_BUILTIN_PSRLDI128:
    case IX86_BUILTIN_PSRLQI128:
      Is_True (wn == NULL, ("WFE_target_builtins: null WN expected"));
      WN * args[2];
      //for (int c=0; c<2; c++)
      {
        // 1st argument
        TY_IDX arg_ty_idx = Get_TY (TREE_TYPE (TREE_VALUE (t_list)));
        TYPE_ID arg_mtype = TY_mtype (arg_ty_idx);
        args[0] = WN_CreateParm (Mtype_comparison (arg_mtype), arg0,
                                 arg_ty_idx, WN_PARM_BY_VALUE);
                                                                                
        // 2nd argument
        arg1 = WN_Cvt (WN_rtype(arg1), MTYPE_V16I8, arg1);
        arg_ty_idx = MTYPE_TO_TY_array[WN_rtype (arg1)];
        arg_mtype = WN_rtype (arg1);
        args[1] = WN_CreateParm (Mtype_comparison (arg_mtype), arg1,
                                 arg_ty_idx, WN_PARM_BY_VALUE);
      }
                                                                                
      wn = WN_Create_Intrinsic (OPR_INTRINSIC_OP, res_type, MTYPE_V,
                                *iopc, 2, args);
      break;
  }
                                                                                
  return wn;
}
#endif // TARG_X8664

#ifdef TARG_SL
BOOL 
Has_LDA_Node(WN* tree) {
  BOOL has_lda = FALSE;
  for(INT i = 0; i < WN_kid_count(tree); i++) {
    has_lda |= Has_LDA_Node(WN_kid(tree, i));
    if(has_lda)
      break;         
  }
  if( WN_operator(tree) == OPR_LDA) {
    return TRUE;
  }
  return has_lda;
}

BOOL 
Mark_LDA_Vbuf_Offset(WN* tree, INTRINSIC iopc ) {

  BOOL has_v1buf_lda = FALSE; 
  static vector < WN* > processed; 

  if(find(processed.begin(), processed.end(), tree) != processed.end()) 
    return has_v1buf_lda;

  for(INT i = 0; i < WN_kid_count(tree); i++) {
    has_v1buf_lda |= Mark_LDA_Vbuf_Offset(WN_kid(tree, i), iopc);
  }

  if(WN_operator(tree) == OPR_LDA) {
    if( ST_in_vbuf(WN_st(tree)) && iopc == INTRN_VBUF_OFFSET) {
      WN_Set_is_internal_mem_ofst(tree, TRUE);
      Set_ST_is_vbuf_ofst(WN_st(tree));
      if(ST_in_v1buf(WN_st(tree))) 
        has_v1buf_lda = TRUE;
    }		  
    else if(ST_in_sbuf(WN_st(tree)) && iopc == INTRN_SBUF_OFFSET) {
      WN_Set_is_internal_mem_ofst(tree, TRUE);
      Set_ST_is_sbuf_ofst(WN_st(tree));
    }		  
  }

  processed.push_back(tree); 

  return has_v1buf_lda; 
}


BOOL 
May_Include_Vbuf_Offset(INTRINSIC iopc, WN* call) {
  switch(iopc) {
    case INTRN_C2_LD_V:
    case INTRN_C2_ST_V:
    case INTRN_C2_ST_G2V:
    case INTRN_C2_ST_G:
      return Has_LDA_Node(WN_kid0(WN_kid1(call)));
    case INTRN_C2_LD_V2G:
    case INTRN_C2_LD_G:
      return Has_LDA_Node(WN_kid0(WN_kid0(call)));
    default:
      return FALSE;
  }
}
#endif 

/* expand gnu expr tree into symtab & whirl */
WN *
WFE_Expand_Expr (tree exp, 
		 bool need_result,
		 TY_IDX nop_ty_idx, 
		 TY_IDX component_ty_idx, 
		 INT64 component_offset,
		 UINT16 field_id,
		 bool is_bit_field,
		 bool is_aggr_init_via_ctor
#ifdef KEY
		 , WN *target_wn
#endif
		 )
{
  FmtAssert(exp != NULL_TREE, ("WFE_Expand_Expr: null argument"));
  enum tree_code code = TREE_CODE (exp);
  WN *wn, *wn0, *wn1, *wn2;
  ST *st;
  TY_IDX ty_idx;
  TY_IDX desc_ty_idx;
  tree arg0, arg1, arg2;
#ifdef KEY
  static BOOL must_not_throw = FALSE;
#endif

  wn = NULL;

#ifdef WFE_DEBUG
  fprintf (stderr,
           "{( WFE_Expand_Expr: %s\n", Operator_From_Tree [code].name); // ")}"
#endif /* WFE_DEBUG */

  switch (code)
    {
    // leaves
    case ADDR_EXPR:
      wn = WFE_Address_Of(TREE_OPERAND(exp, 0));
      break;
      
    /*FDESC_EXPR:
     *Operand0 is a function constant; result is part N of a function 
     *descriptor of type ptr_mode. 
     *So we should get function constant and exprand it.
     */
    case FDESC_EXPR:
      {
	tree exp_operand = TREE_OPERAND(exp, 0);
	FmtAssert(TREE_CODE(exp_operand) == FUNCTION_DECL,("Unexpected Tree Code!!"));
	st = Get_ST (exp_operand);
	ty_idx = ST_type (st);
	wn = WN_Lda (Pointer_Mtype, ST_ofst(st), st);
      }
      break;
      
    case FUNCTION_DECL:
      {
	 st = Get_ST (exp);
	 ty_idx = ST_type (st);
	 wn = WN_Lda (Pointer_Mtype, ST_ofst(st), st);
      }
      break;

    case TREE_LIST: 
      {
	tree stmt;
	for (stmt = TREE_PURPOSE(exp); stmt; stmt = TREE_CHAIN(stmt))
	  WFE_Expand_Stmt (stmt);
	wn = WFE_Expand_Expr (TREE_VALUE(exp));
      }
      break;

    case DECL_STMT:
      {
        tree decl = DECL_STMT_DECL(exp);
	WFE_Expand_Decl (decl);
	wn = WFE_Expand_Expr (decl);
      }
      break;

    case BIND_EXPR:
#ifdef GPLUSPLUS_FE
      DevWarn ("Encountered BIND_EXPR at line %d", lineno);
      // ignore the first operand as it ia a list of temporary variables
      wn = WFE_Expand_Expr (TREE_OPERAND (exp, 1));
      break;
#else
      {
        INT32    i;
        WN      *block;
        TYPE_ID  mtype;
        tree     t;

	DevWarn ("Encountered BIND_EXPR at line %d", lineno);

        for (i = wfe_bind_expr_stack_last; i >= 0; --i) {

          if (wfe_bind_expr_stack [i].rtl_expr == TREE_OPERAND (exp, 1)) {

            block = wfe_bind_expr_stack [i].block;
            t     = wfe_bind_expr_stack [i].rtl_expr;
            wfe_bind_expr_stack [i] = wfe_bind_expr_stack [wfe_bind_expr_stack_last];
            --wfe_bind_expr_stack_last;
            break;
          }
        }

        FmtAssert (i >= 0,
                   ("BIND_EXPR: did not find tree"));
	ty_idx = Get_TY (TREE_TYPE(t));
        mtype  = TY_mtype (ty_idx);
	if (mtype == MTYPE_V) {
	  WFE_Stmt_Append (block, Get_Srcpos ());
          break;
	}
	else {
	  wn0 = block;
	  wn1 = WN_COPY_Tree (WN_last (wn0));
	  WN_DELETE_FromBlock (wn0, WN_last (wn0));
	  WFE_Stmt_Append (wn0, Get_Srcpos ());
	  if (nop_ty_idx == 0 && component_ty_idx == 0) {
	    wn = WN_kid0 (wn1);
            break;
	  }
          if (WN_operator (WN_kid0 (wn1)) == OPR_LDID)
            st = WN_st (WN_kid0 (wn1));
          else {
            st = Gen_Temp_Symbol (ty_idx, "__bind_expr");
#ifdef KEY
  	    WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, st);
#endif
            WFE_Set_ST_Addr_Saved (WN_kid0 (wn1));
            wn0 = WN_Stid (mtype, 0, st, ty_idx, WN_kid0 (wn1));
            WFE_Stmt_Append (wn0, Get_Srcpos ());
          }
	}
      }
      /*FALLTHRU*/
#endif /* GPLUSPLUS_FE */

    case TARGET_EXPR:
      {
	tree opnd0 = TREE_OPERAND(exp, 0);
	st = NULL;
	TY_IDX ty;
	TYPE_ID mtype;
#ifdef KEY
	// If we are supposed to put the result in target_wn, then give the
	// init target the same ST as target_wn.
	if (target_wn != NULL) {
	  if (WN_operator(target_wn) == OPR_LDA) {
	    FmtAssert(TREE_CODE(opnd0) != INDIRECT_REF,
		      ("WFE_Expand_Expr: write target mismtach"));
	    set_DECL_ST(opnd0, WN_st(target_wn));
	  } else if (WN_operator(target_wn) == OPR_LDID) {
	    // Change the target into an INDIRECT_REF only if we have not done
	    // so.
	    if (TREE_CODE(opnd0) == VAR_DECL) {
	      tree ptr_var = build_decl(VAR_DECL, NULL_TREE,
					build_pointer_type(TREE_TYPE(opnd0)));
	      TREE_SET_CODE(opnd0, INDIRECT_REF);
	      TREE_OPERAND(opnd0, 0) = ptr_var;
	      set_DECL_ST(ptr_var, WN_st(target_wn));
	    }
	  }
	}

	// We might have changed the VAR_DECL to an INDIRECT_REF, in which case
	// use the referenced symbol instead of creating a new one.
	if (TREE_CODE(opnd0) != INDIRECT_REF)
#endif
	{
	st    = Get_ST (TREE_OPERAND(exp, 0));
	ty    = ST_type(st);
	mtype = TY_mtype (ty);
	}
	/*
	 * Usually operand 1 of the target_expr will be an aggr_init_expr
	 * for which AGGR_INIT_VIA_CTOR_P holds.  Such a node has the
	 * annoying property that its first argument is not the expected
	 * argument to the constructor call.  Instead the argument whose
	 * address should be passed to the constructor appears as 
	 * operand 2.  The value in operand 2, however, is not always
 	 * right:  it is the original temporary var_decl from the
	 * target_expr.  What we really want is the current operand 0
	 * of the target_expr, which may have been changed (see INIT_EXPR).
	 * This is really ugly, but we can't help it because at the
	 * expression level we need to stay compatible with the current
	 * rtl generation.
	 * So we're going to replace the first argument of the aggr_init_expr
	 * with the var_decl from operand 0 of the target_expr, and pass
	 * is_aggr_init_via_ctor to WFE_Expand_Expr, so it can be dealt
	 * with by the AGGR_INIT_EXPR/CALL_EXPR code.
	 *
	 * If a target expression is initialized by a target expression,
	 * it ought not to have an associated cleanup, so we clear the
	 * cleanup in this case.
	 */
	tree t = TREE_OPERAND(exp, 1);
#ifdef KEY
	if (t == NULL_TREE) {
	  t = TREE_OPERAND(exp, 3);
	  FmtAssert(t != NULL_TREE,
		    ("WFE_Expand_Expr: no initializer found for TARGET_EXPR"));
	}
#endif
 	if (TREE_CODE(t) == TARGET_EXPR)
	  TREE_OPERAND(t, 2) = 0;
	if (TREE_CODE(t) == AGGR_INIT_EXPR && AGGR_INIT_VIA_CTOR_P(t)) {
	  tree args = TREE_OPERAND(t, 1);
	  TREE_VALUE(args) = TREE_OPERAND(exp, 0);
	  WFE_Expand_Expr (t, false, 0, 0, 0, 0, false, true);
	}
	else {
#ifdef KEY
	  // If opnd 0 of the TARGET_EXPR is an INDIRECT_REF, then tell
	  // WFE_Expand_Expr to put the result in the area addressed by the
	  // INDIRECT_REF.
          // OSP_397, initializing member of a class having copy-constructor
          // This fix is done by Pathscale. Thanks
          // If opnd0 is not DECL, then generate the addr of opnd 0
          // Otherwise, get the ST and get the addr of the st
          if (TREE_CODE(opnd0) == INDIRECT_REF) {
            if (TREE_CODE_CLASS(TREE_CODE(TREE_OPERAND(opnd0,0))) != 'd') {
              WN * target_wn = WFE_Address_Of (opnd0);
              WFE_Expand_Expr (t, TRUE /* for return_in_mem */,
                               0, 0, 0, 0, FALSE, FALSE, target_wn);
            }
            else {
	      ST *st = Get_ST(TREE_OPERAND(opnd0, 0));
	      WN *ldid_wn = WN_Ldid (Pointer_Mtype, 0, st, ST_type(st));
	      WN *result_wn = WFE_Expand_Expr (t, TRUE, 0, 0, 0, 0, FALSE, FALSE,
                                               ldid_wn);
	      // If the result of expanding t is not an indirect reference to the
	      // result area we want, then it means t has not copied the value
	      // into the result area.  Do the copy.
	      if (result_wn &&
                  !(WN_operator(result_wn) == OPR_ILOAD &&
		    WN_operator(WN_kid0(result_wn)) == OPR_LDID &&
		    WN_st(WN_kid0(result_wn)) == st)) {
	        WFE_Stmt_Append(WN_Istore(WN_rtype(result_wn), 0, ST_type(st),
                                          WN_CopyNode(ldid_wn), result_wn),
                                Get_Srcpos());
	      }
            }
	  }
	  // If the initializer returns the object in memory, then make sure
	  // the type doesn't require a copy constructor, since such types
	  // sometimes require one.
	  //
	  // Note when one stmt like "bool_expr ? Default Constructor : throw 0;"
	  // the TY_return_in_mem(Get_TY(TREE_TYPE(t)) return 0, so add another
	  // condition for this case.
	  else if (TY_return_in_mem(Get_TY(TREE_TYPE(t))) || 
		   ((TY_mtype (Get_TY(TREE_TYPE(t))) == MTYPE_M) &&
		    (TREE_CODE(t) == COND_EXPR))) {
	    if (TREE_CODE(t) == VAR_DECL ||
		TREE_CODE(t) == PARM_DECL) {
	      // The initializer is a var or parm.  We need to insert copy.
	      // First make sure type has no copy constructor.
	      WN *rhs_wn = WFE_Expand_Expr (t);
	      tree type = TREE_TYPE(TREE_OPERAND(exp, 0));
	      Is_True(!WFE_has_copy_constructor(type),
		      ("WFE_Expand_Expr: type require copy constructor"));
	      WFE_Stmt_Append(WN_Stid (mtype, ST_ofst(st), st, ty, rhs_wn),
			      Get_Srcpos());
	    } else {
	      // The initializer is an expression.  Try to expand it directly
	      // into the target.
	      WN *target_wn = WN_Lda (Pointer_Mtype, 0, st, 0);
	      WN *result_wn = WFE_Expand_Expr (t, TRUE, 0, 0, 0, 0, FALSE,
					       FALSE, target_wn);
	      // If expanding t did not write the result into the target as a
	      // side effect, then create the copy.
	      if (result_wn) {
		// There may be more cases where we need to store the result.
		// Need to find a better way to catch them all.
		if (WN_operator(result_wn) == OPR_ILOAD) {
		  WFE_Stmt_Append(WN_Stid (mtype, ST_ofst(st), st, ty,
				  result_wn), Get_Srcpos());
		} else if (WN_operator(result_wn) == OPR_CSELECT) {
		  WN *wn = WN_CreateEval(result_wn);
		  WFE_Stmt_Append(wn, Get_Srcpos());
		}
	      }
	    }
	  } else {
	    // Bug 7862: Set addr_saved flag if the initializer is an LDA.
	    WN * init = WFE_Expand_Expr (t);
	    if (WN_operator (init) == OPR_LDA)
	      WFE_Set_ST_Addr_Saved (init);
	    WFE_Stmt_Append(WN_Stid (mtype, ST_ofst(st), st, ty, init),
			    Get_Srcpos());
	  }
#else
	  WFE_Stmt_Append(WN_Stid (mtype, ST_ofst(st), st, ty,
		        	   WFE_Expand_Expr (t)),
			  Get_Srcpos());
#endif
	}
        if (TREE_OPERAND(exp, 2) 
#ifdef KEY
// We should not be emitting all cleanups
	&& TREE_LANG_FLAG_7 (exp)
#endif
	)
#ifdef KEY
          Push_Temp_Cleanup(TREE_OPERAND(exp, 2), true, CLEANUP_EH_ONLY (exp));
#else
          Push_Temp_Cleanup(TREE_OPERAND(exp, 2), true);
#endif

#ifdef KEY
	// If the target area was supplied by the caller, then return an ILOAD
	// of the target pointer.
        // OSP_397, fix by Pathscale.
        // If the opnd0 is a DECL, then get the ST and load from the st
        // Otherwise, expand opnd 0
        if (TREE_CODE(opnd0) == INDIRECT_REF) {
          if (TREE_CODE_CLASS(TREE_CODE(TREE_OPERAND(opnd0,0))) == 'd') {
            ST *st = Get_ST(TREE_OPERAND(opnd0, 0));
            TY_IDX ty_idx = Get_TY (TREE_TYPE(exp));
            WN *ldid_wn = WN_Ldid (Pointer_Mtype, 0, st, ST_type(st));
            wn = WN_Iload(TY_mtype(ty_idx), 0, ty_idx, ldid_wn);
            break;
          }
          else {
            wn = WFE_Expand_Expr(opnd0);
	    break;
          }
        }
#endif
      }

    case CONSTRUCTOR:
#ifdef KEY
      // In general, if the result is not needed and EXP has no side effects,
      // then there is no need to expand EXP, regardless of what EXP is.  This
      // is what gcc's expand_expr does.  However, doing so breaks the WHIRL
      // front-end, so limit this to CONSTRUCTOR for now.
      if (!need_result &&
	  !TREE_SIDE_EFFECTS(exp)) {
	return NULL;
      }
#endif

    case PARM_DECL: // for formal parms
    case VAR_DECL:
      {
	UINT xtra_BE_ofst = 0; 	// only needed for big-endian target
        PREG_NUM preg_num = 0;
	desc_ty_idx = component_ty_idx;
	TY_IDX hi_ty_idx = Get_TY (TREE_TYPE(exp));
	if (desc_ty_idx == 0)
	  desc_ty_idx = hi_ty_idx;

	if (! MTYPE_is_integral(TY_mtype(desc_ty_idx)))
	  ty_idx = desc_ty_idx;
	else {
	  ty_idx = nop_ty_idx;
	  if (ty_idx == 0) 
	    ty_idx = desc_ty_idx;
	}

	UINT cvtl_size = 0; // if non-zero, need to generate CVTL with this size
	if (! is_bit_field) {
	  if (TY_size(desc_ty_idx) > TY_size(ty_idx)) {
	    if (Target_Byte_Sex == BIG_ENDIAN)
	      xtra_BE_ofst = TY_size(desc_ty_idx) - TY_size(ty_idx);
	    cvtl_size = TY_size(ty_idx) * 8;
	    ty_idx = desc_ty_idx;
	  }
	}
	else {
	  if (TY_size(desc_ty_idx) > TY_size(ty_idx)) 
	    ty_idx = desc_ty_idx;
	}

        TYPE_ID rtype = Widen_Mtype(TY_mtype(ty_idx));
        TYPE_ID desc = TY_mtype(desc_ty_idx);
        if (MTYPE_is_integral(desc)) {
          if (MTYPE_signed(rtype) != MTYPE_signed(desc)) {
            if (MTYPE_size_min(rtype) > MTYPE_size_min(desc) ||
		is_bit_field)
              rtype = Mtype_TransferSign(desc, rtype);
            else desc = Mtype_TransferSign(rtype, desc);
          }
        }

	if (TREE_THIS_VOLATILE(exp))
	  Set_TY_is_volatile(ty_idx);

	if (code == PARM_DECL || code == VAR_DECL) {
	  st = Get_ST (exp);
          if (ST_assigned_to_dedicated_preg (st))
	    Set_TY_is_volatile(ty_idx);
        }
	else
	if (code == CONSTRUCTOR) {
	  DevWarn ("Encountered CONSTRUCTOR at line %d", lineno);
	  st = WFE_Generate_Temp_For_Initialized_Aggregate (exp, "");
	}

	Is_True(! is_bit_field || field_id <= MAX_FIELD_ID,
		("WFE_Expand_Expr: field id for bit-field exceeds limit"));

#ifdef TARG_X8664
        // The source may have different types of casting between same-sized
        // vector types, and between same-sized vector-nonvector types.
        if (MTYPE_is_vector (rtype) || MTYPE_is_vector (desc))
          desc = rtype;
#endif

	wn = WN_CreateLdid (OPR_LDID, rtype,
			    is_bit_field ? MTYPE_BS : desc,
			    ST_ofst(st)+component_offset+xtra_BE_ofst+preg_num, st,
			    field_id != 0 ? hi_ty_idx : ty_idx, field_id);
	if (cvtl_size != 0)
	  wn = WN_CreateCvtl(OPR_CVTL, rtype, MTYPE_V, cvtl_size, wn);
      }
      break;

    case CONST_DECL:
        wn = WFE_Expand_Expr(DECL_INITIAL(exp), need_result);
	break;

    case INTEGER_CST:
      {
	ty_idx = Get_TY (TREE_TYPE(exp));
	TYPE_ID mtyp = TY_mtype(ty_idx);
	mtyp = (mtyp == MTYPE_V) ? MTYPE_I4 : Widen_Mtype(mtyp);
	wn = WN_Intconst(mtyp, Get_Integer_Value(exp));
      }
      break;

    case PTRMEM_CST:
      {
	wn = WFE_Expand_Expr(cplus_expand_constant (exp),
			     need_result, nop_ty_idx, component_ty_idx,
			     component_offset, field_id);
      }
      break;

#ifdef KEY	// Use the code from kgccfe, which is newer and can handle
		// i386.
    case REAL_CST:
      {
	TCON tcon;
	ty_idx = Get_TY (TREE_TYPE(exp));
#if (defined(TARG_IA32) || defined(TARG_X8664)) && !defined(REAL_ARITHMETIC)
	tcon = Host_To_Targ_Float (TY_mtype (ty_idx), TREE_REAL_CST(exp));
#else
	REAL_VALUE_TYPE real = TREE_REAL_CST(exp);
#ifndef TARG_IA64
	int rval;

	long rbuf [4];
#ifdef KEY
	INT32 rbuf_w[4]; // this is needed when long is 64-bit
	INT32 i;
#endif
#endif
	switch (TY_mtype (ty_idx)) {
#ifdef TARG_IA64
	  case MTYPE_F4:
	    tcon = Host_To_Targ_Float_4 (MTYPE_F4,
		WFE_Convert_Internal_Real_to_IEEE_Single(real));
	    break;

	  case MTYPE_F8:
	    tcon = Host_To_Targ_Float (MTYPE_F8,
		WFE_Convert_Internal_Real_to_IEEE_Double(real));
	    break;

          case MTYPE_F10:
	    tcon = Host_To_Targ_Float_10 (MTYPE_F10,
		WFE_Convert_Internal_Real_to_IEEE_Double_Extended(real));
            break;

	  case MTYPE_FQ:
	    tcon = Host_To_Targ_Quad (WFE_Convert_Internal_Real_to_IEEE_Double_Extended(real));
	    break;	    

#else
	  case MTYPE_F4:
	    REAL_VALUE_TO_TARGET_SINGLE (real, rval);
	    tcon = Host_To_Targ_Float_4 (MTYPE_F4, *(float *) &rval);
	    break;
	  case MTYPE_F8:
	    REAL_VALUE_TO_TARGET_DOUBLE (real, rbuf);
#ifdef KEY
	    WFE_Convert_To_Host_Order(rbuf);
	    for (i = 0; i < 4; i++)
	      rbuf_w[i] = rbuf[i];
	    tcon = Host_To_Targ_Float (MTYPE_F8, *(double *) &rbuf_w);
#else
	    tcon = Host_To_Targ_Float (MTYPE_F8, *(double *) &rbuf);
#endif
	    break;
#if defined(TARG_IA32) || defined(TARG_X8664) || defined(TARG_LOONGSON) 
	  case MTYPE_FQ:
	    REAL_VALUE_TO_TARGET_LONG_DOUBLE (real, rbuf);
	    for (i = 0; i < 4; i++)
	      rbuf_w[i] = rbuf[i];
#ifdef TARG_LOONGSON
	    tcon = Host_To_Targ_Quad (*(QUAD_TYPE *) &rbuf_w);
#else
	    tcon = Host_To_Targ_Quad (*(long double *) &rbuf_w);
#endif
	    break;	    
#endif /* TARG_IA32 */
#endif
	  default:
	    FmtAssert(FALSE, ("WFE_Expand_Expr unexpected float size"));
	    break;
	}
#endif
	st = New_Const_Sym (Enter_tcon (tcon), ty_idx);
	wn = WN_CreateConst (OPR_CONST, TY_mtype (ty_idx), MTYPE_V, st);
      }
      break;

    case COMPLEX_CST:
      {
	TCON tcon;
	ty_idx = Get_TY (TREE_TYPE(exp));
#if (defined(TARG_IA32) || defined(TARG_X8664)) && !defined(REAL_ARITHMETIC)
        tcon = Host_To_Targ_Complex (TY_mtype (ty_idx),
				     TREE_REAL_CST(TREE_REALPART(exp)),
				     TREE_REAL_CST(TREE_IMAGPART(exp)));
#else
#ifdef TARG_IA64
	REAL_VALUE_TYPE real = TREE_REAL_CST(TREE_REALPART(exp));
	REAL_VALUE_TYPE imag = TREE_REAL_CST(TREE_IMAGPART(exp));

	switch (TY_mtype (ty_idx)) {
	  case MTYPE_C4:
	    tcon = Host_To_Targ_Complex_4 (MTYPE_C4,
		WFE_Convert_Internal_Real_to_IEEE_Single(real),
		WFE_Convert_Internal_Real_to_IEEE_Single(imag));
	    break;

	  case MTYPE_C8:
	    tcon = Host_To_Targ_Complex (MTYPE_C8,
		WFE_Convert_Internal_Real_to_IEEE_Double(real),
		WFE_Convert_Internal_Real_to_IEEE_Double(imag));
	    break;

	  case MTYPE_C10:
	    tcon = Host_To_Targ_Complex_10 (MTYPE_C10,
		WFE_Convert_Internal_Real_to_IEEE_Double_Extended(real),
		WFE_Convert_Internal_Real_to_IEEE_Double_Extended(imag));
	    break;

	  case MTYPE_CQ:
	    tcon = Host_To_Targ_Complex_Quad (
		WFE_Convert_Internal_Real_to_IEEE_Double_Extended(real),
		WFE_Convert_Internal_Real_to_IEEE_Double_Extended(imag));
	    break;
#else

	REAL_VALUE_TYPE real = TREE_REAL_CST(TREE_REALPART(exp));
	REAL_VALUE_TYPE imag = TREE_REAL_CST(TREE_IMAGPART(exp));
        int rval;
	int ival;
	long rbuf [4];
	long ibuf [4];
#ifdef KEY
	INT32 rbuf_w [4]; // this is needed when long is 64-bit
	INT32 ibuf_w [4]; // this is needed when long is 64-bit
	INT32 i;
#endif 
	switch (TY_mtype (ty_idx)) {
	  case MTYPE_C4:
	    REAL_VALUE_TO_TARGET_SINGLE (real, rval);
	    REAL_VALUE_TO_TARGET_SINGLE (imag, ival);
	    tcon = Host_To_Targ_Complex_4 (MTYPE_C4,
					   *(float *) &rval,
					   *(float *) &ival);
	    break;
	  case MTYPE_C8:
	    REAL_VALUE_TO_TARGET_DOUBLE (real, rbuf);
	    REAL_VALUE_TO_TARGET_DOUBLE (imag, ibuf);
#ifdef KEY
	    WFE_Convert_To_Host_Order(rbuf);
	    WFE_Convert_To_Host_Order(ibuf);
	    for (i = 0; i < 4; i++) {
	      rbuf_w[i] = rbuf[i];
	      ibuf_w[i] = ibuf[i];
	    }
	    tcon = Host_To_Targ_Complex (MTYPE_C8,
					 *(double *) &rbuf_w,
					 *(double *) &ibuf_w);
#else
	    tcon = Host_To_Targ_Complex (MTYPE_C8,
					 *(double *) &rbuf,
					 *(double *) &ibuf);
#endif
	    break;
#if defined(KEY) && !defined(TARG_LOONGSON)
	case MTYPE_CQ:
	    REAL_VALUE_TO_TARGET_LONG_DOUBLE (real, rbuf);
	    REAL_VALUE_TO_TARGET_LONG_DOUBLE (imag, ibuf);
	    WFE_Convert_To_Host_Order(rbuf);
	    WFE_Convert_To_Host_Order(ibuf);
	    for (i = 0; i < 4; i++) {
	      rbuf_w[i] = rbuf[i];
	      ibuf_w[i] = ibuf[i];
	    }
	    tcon = Host_To_Targ_Complex_Quad( *(long double *) &rbuf_w,
					      *(long double *) &ibuf_w );
	  break;
#endif
#endif
	  default:
	    FmtAssert(FALSE, ("WFE_Expand_Expr unexpected float size"));
	    break;
	}
#endif
	st = New_Const_Sym (Enter_tcon (tcon), ty_idx);
	wn = WN_CreateConst (OPR_CONST, TY_mtype (ty_idx), MTYPE_V, st);
      }
      break;

#else	// KEY

    case REAL_CST:
      {
	TCON tcon;
	ty_idx = Get_TY (TREE_TYPE(exp));
	tcon = Host_To_Targ_Float (TY_mtype (ty_idx), TREE_REAL_CST(exp));
	st = New_Const_Sym (Enter_tcon (tcon), ty_idx);
	wn = WN_CreateConst (OPR_CONST, TY_mtype (ty_idx), MTYPE_V, st);
      }
      break;

    case COMPLEX_CST:
      {
	TCON tcon;
	ty_idx = Get_TY (TREE_TYPE(exp));
	tcon = Host_To_Targ_Complex (TY_mtype (ty_idx),
				     TREE_REAL_CST(TREE_REALPART(exp)),
				     TREE_REAL_CST(TREE_IMAGPART(exp)));
	st = New_Const_Sym (Enter_tcon (tcon), ty_idx);
	wn = WN_CreateConst (OPR_CONST, TY_mtype (ty_idx), MTYPE_V, st);
      }
      break;
#endif	// KEY

    // this should occur only if string is a statement expression
    case STRING_CST:
      {
	TCON tcon;
	tcon = Host_To_Targ_String (MTYPE_STRING,
				    const_cast<char*>TREE_STRING_POINTER(exp),
				    TREE_STRING_LENGTH(exp));
	ty_idx = Get_TY(TREE_TYPE(exp));
	st = New_Const_Sym (Enter_tcon (tcon), ty_idx);
	wn = WN_Lda (Pointer_Mtype, ST_ofst(st), st);
	TREE_STRING_ST (exp) = st;
      }
      break;

    // unary ops
    case BIT_NOT_EXPR:
    case ABS_EXPR:
    case NEGATE_EXPR:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
      {
        wn0 = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
        wn  = WN_Unary (Operator_From_Tree [code].opr,
                        Widen_Mtype(TY_mtype(Get_TY(TREE_TYPE(exp)))), wn0);
#ifdef KEY // bug 2648
        TYPE_ID mtyp = TY_mtype(Get_TY(TREE_TYPE(exp)));
        if (mtyp != WN_rtype(wn))
          wn = WN_CreateCvtl (OPR_CVTL, WN_rtype(wn), MTYPE_V,
                              MTYPE_size_min(mtyp), wn);
#endif
      }
      break;

    case TRUTH_NOT_EXPR:
      wn0 = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
      wn1 = WN_Intconst (MTYPE_I4, 0);
      wn  = WN_Relational (OPR_EQ, MTYPE_I4, wn0, wn1);
      break;

    case CONJ_EXPR:
      {
	ty_idx = Get_TY (TREE_TYPE(exp));
        TYPE_ID complex_mtype = TY_mtype(ty_idx);
        TYPE_ID float_mtype   = Mtype_complex_to_real (complex_mtype);
        wn0 = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
	if (WN_has_side_effects (wn0)) {
	  ST       *preg_st;
	  PREG_NUM  preg;
	  preg_st = MTYPE_To_PREG(complex_mtype);
	  preg    = Create_Preg (complex_mtype, NULL);
	  wn0     = WN_Stid (complex_mtype, preg, preg_st, ty_idx, wn0);
	  WFE_Stmt_Append (wn0, Get_Srcpos());
	  wn0 = WN_Ldid (complex_mtype, preg, preg_st, ty_idx);
	}
#ifdef KEY
	// Fix bug 603
        wn = WN_Binary (OPR_COMPLEX, complex_mtype,
			WN_Unary (OPR_REALPART, float_mtype, wn0),
			WN_Unary (OPR_NEG, float_mtype,
				  WN_Unary (OPR_IMAGPART, float_mtype, wn0)));
#else
        wn = WN_Binary (OPR_COMPLEX, complex_mtype,
			WN_Unary (OPR_REALPART, float_mtype, wn0),
			WN_Unary (OPR_NEG, float_mtype,
				  WN_Unary (OPR_REALPART, float_mtype, wn0)));
#endif
      }
      break;

    case NOP_EXPR:
      {
	ty_idx = Get_TY (TREE_TYPE(exp));
        TYPE_ID mtyp = TY_mtype(ty_idx);
	// do not pass struct type down because will cause rtype of MTYPE_M
        wn = WFE_Expand_Expr (TREE_OPERAND (exp, 0), TRUE, 
			      (mtyp == MTYPE_M) ? 0 : ty_idx,
			       component_ty_idx, component_offset,
			       field_id, is_bit_field
#ifdef KEY
			       , FALSE, target_wn
#endif
			       );
	if (mtyp == MTYPE_V) 
	  break;
	if (mtyp == MTYPE_M) 
	  break;
	if (MTYPE_is_integral(mtyp) && MTYPE_is_integral(WN_rtype(wn))) {
	  // For 32-bit to 64-bit conversion, make the result have the same
	  // sign as the source.  Fix bug 480.
	  if (MTYPE_size_min(mtyp) == 64 &&
	      MTYPE_size_min(WN_rtype(wn)) == 32 &&
	      MTYPE_is_signed(mtyp) != MTYPE_is_signed(WN_rtype(wn))) {
	    mtyp = MTYPE_complement(mtyp);
	  }

	  if (MTYPE_size_min(mtyp) < MTYPE_size_min(WN_rtype(wn))) {
	    if (MTYPE_size_min(mtyp) != 32)
	      wn = WN_CreateCvtl(OPR_CVTL, Widen_Mtype(mtyp), MTYPE_V,
			         MTYPE_size_min(mtyp), wn);
	    else wn = WN_Cvt(WN_rtype(wn), mtyp, wn);
	  }
	  else {
	    TY_IDX ty_idx0 = Get_TY(TREE_TYPE(TREE_OPERAND (exp, 0)));
	    TYPE_ID mtyp0 = TY_mtype(ty_idx0);

	    if (MTYPE_size_min(mtyp) > MTYPE_size_min(mtyp0) &&
		! Has_Subsumed_Cvtl(WN_operator(wn)))
	      wn = WN_CreateCvtl(OPR_CVTL, Widen_Mtype(mtyp0), MTYPE_V,
				 MTYPE_size_min(mtyp0), wn);

	    if (MTYPE_size_min(mtyp) > MTYPE_size_min(WN_rtype(wn)))
	      wn = WN_Cvt(WN_rtype(wn), mtyp, wn);
	    else { // same size
	      if (mtyp != WN_rtype(wn)) 
	        wn = WN_Cvt(WN_rtype(wn), mtyp, wn);
	    }
	  }
	}
	else {
	  if (mtyp != WN_rtype(wn)) 
	    wn = WN_Cvt(WN_rtype(wn), mtyp, wn);
	}
      }
      break;

    case COMPONENT_REF:
      {
	INT64 ofst;
	arg0 = TREE_OPERAND (exp, 0);
	arg1 = TREE_OPERAND (exp, 1);
	// If this is an indirect of a nop_expr, we may need to fix the
	// type of the nop_expr:
	(void) Get_TY(TREE_TYPE(arg0));
   
	if (component_ty_idx == 0)
	  ty_idx = Get_TY (TREE_TYPE(exp));
	else ty_idx = component_ty_idx;
	if (DECL_BIT_FIELD(arg1)) 
	  is_bit_field = TRUE;

	if (! is_bit_field && 
	    component_ty_idx == 0) {  // only for top-level COMPONENT_REF
          // if size does not agree with ty_idx, fix ty_idx
          tree sizenode = DECL_SIZE(arg1);
          if (
#ifdef KEY
              sizenode && // bug 11726, in absence of size expression
#endif
              TREE_CODE(sizenode) == INTEGER_CST) {
	    TYPE_ID c_mtyp = TY_mtype(ty_idx);
	    INT32 bsize = Get_Integer_Value(sizenode);
	    if (MTYPE_size_min(c_mtyp) > bsize) {
	      FmtAssert(MTYPE_is_integral(c_mtyp), 
	        ("COMPONENT_REF: integer type expected at inconsistent field size"));
	      c_mtyp = Mtype_AlignmentClass(bsize >> 3, MTYPE_type_class(c_mtyp));
	      ty_idx = MTYPE_To_TY(c_mtyp);
	    }
	  }
        }

	if (! is_bit_field)
	  ofst = (BITSPERBYTE * Get_Integer_Value(DECL_FIELD_OFFSET(arg1)) +
			        Get_Integer_Value(DECL_FIELD_BIT_OFFSET(arg1)))
			      / BITSPERBYTE;
	else ofst = 0;
#ifdef KEY
	FmtAssert (DECL_FIELD_ID(arg1) != 0,
                   ("WFE_Expand_Expr: DECL_FIELD_ID used but not set"));

	// If arg0 is a CALL_EXPR that returns a ptr-to-member-function, then
	// call WFE_Expand_Ptr_To_Member_Func_Call_Expr to expand it.
	// Otherwise, call WFE_Expand_Expr to do regular expansion.
	// Bug 3400, 3427.
	if (WFE_Call_Returns_Ptr_To_Member_Func(arg0)) {
	  tree field0 = TYPE_FIELDS(TREE_TYPE(arg0));
	  // Get_TY(TREE_TYPE(field0)) is valid only if
	  // WFE_Call_Returns_Ptr_To_Member_Func(arg0)) is TRUE.  Bug 6022.
	  TYPE_ID desc = TY_mtype(Get_TY(TREE_TYPE(field0)));
	  wn = WFE_Expand_Ptr_To_Member_Func_Call_Expr (arg0, nop_ty_idx,
		  Pointer_Mtype, desc, component_offset,
		  field_id + DECL_FIELD_ID(arg1));
	} else
#endif
        wn = WFE_Expand_Expr (arg0, TRUE, nop_ty_idx, ty_idx, ofst+component_offset,
			      field_id + DECL_FIELD_ID(arg1), is_bit_field);

#ifdef KEY
	// For code such as (p->a = q->a).b, the gnu tree is:
	//   component_ref
	//     modify_expr
	//       indirect_ref
	//       indirect_ref
	// WFE_Expand_Expr will call WFE_Lhs_Of_Modify_Expr to expand the
	// modify_expr.  WFE_Lhs_Of_Modify_Expr will return an iload
	// corresponding to p->a.  Since we want p->a.b, recreate the iload
	// here.  Bug 3122 and 3210
	//
	// bug fix for OSP_118
	//
	if (TREE_CODE(arg0) == MODIFY_EXPR || TREE_CODE(arg0) == NON_LVALUE_EXPR) {
	  TYPE_ID rtype = Widen_Mtype(TY_mtype(ty_idx));
	  TYPE_ID desc = TY_mtype(ty_idx);
	  if (WN_operator(wn) == OPR_ILOAD) {
#ifdef PATHSCALE_MERGE
            wn = WN_CreateIload(OPR_ILOAD, rtype, desc,
			        WN_offset(wn) + ofst + component_offset, ty_idx,
			        WN_load_addr_ty(wn), WN_kid0(wn),
			        WN_field_id(wn)+field_id + DECL_FIELD_ID(arg1));
#else
            wn = WN_CreateIload(OPR_ILOAD, rtype, desc,
			        ofst + component_offset, ty_idx,
			        Make_Pointer_Type (ty_idx, FALSE), WN_kid0(wn),
			        field_id + DECL_FIELD_ID(arg1));
#endif
	  } 
	  else if (WN_operator(wn) == OPR_LDID) {
	    WN_set_rtype(wn, rtype);
	    WN_set_desc(wn, desc);
	    WN_offset(wn) = WN_offset(wn)+ofst+component_offset;
	    WN_set_ty(wn, ty_idx);
	    // bug fix for OSP_158 
	    // if (TY_kind(ty_idx) == KIND_SCALAR)
	    if (TY_kind(ty_idx) != KIND_STRUCT)
	      WN_set_field_id (wn, 0);
	    else
#ifdef PATHSCALE_MERGE
	      WN_set_field_id(wn, WN_field_id(wn)+field_id + DECL_FIELD_ID(arg1));
#else
	      WN_set_field_id(wn, field_id + DECL_FIELD_ID(arg1));
#endif
	  } 
	} 
	// bug 6122
	// Handle code like (x == 1 ? p->a : p->b).c
	else if (TREE_CODE(arg0) == COND_EXPR &&
		 WN_operator(wn) == OPR_CSELECT &&
		 WN_rtype(wn) == MTYPE_M)
	{
	  // kid1 and kid2 must be type M and must be of the same struct type
	  Is_True (WN_rtype (WN_kid1(wn)) == MTYPE_M, ("Unexpected type"));
	  // code adapted from vho
	  TY_IDX temp_ty_idx = WN_ty (WN_kid1 (wn));
	  // Get the struct type corresponding to the field
	  if (WN_field_id (WN_kid1 (wn)))
	    temp_ty_idx = get_field_type (temp_ty_idx,
	                                  WN_field_id (WN_kid1 (wn)));
	  // Store into temp symbol
	  ST * temp = Gen_Temp_Symbol (temp_ty_idx, ".mcselect_store");
	  wn = WN_Stid (MTYPE_M, 0, temp, temp_ty_idx, wn);
	  WFE_Stmt_Append (wn, Get_Srcpos());
	  // Load correct field from temp symbol
	  wn = WN_Ldid (TY_mtype (ty_idx), ofst + component_offset,
	                temp, temp_ty_idx, field_id + DECL_FIELD_ID(arg1));
	}
#endif
      }
      break;

    case INDIRECT_REF:
      {
	UINT xtra_BE_ofst = 0; 	// only needed for big-endian target
        wn0 = WFE_Expand_Expr (TREE_OPERAND (exp, 0));

	TY_IDX hi_ty_idx = Get_TY(TREE_TYPE(exp));

	desc_ty_idx = component_ty_idx;
	if (desc_ty_idx == 0)
	  desc_ty_idx = hi_ty_idx;

        if (! MTYPE_is_integral(TY_mtype(desc_ty_idx)))
	  ty_idx = desc_ty_idx;
	else {
	  ty_idx = nop_ty_idx;
	  if (ty_idx == 0) 
	    ty_idx = desc_ty_idx;
	}

	if (! is_bit_field) {
	  if (TY_size(desc_ty_idx) > TY_size(ty_idx)) {
	    if (Target_Byte_Sex == BIG_ENDIAN)
	      xtra_BE_ofst = TY_size(desc_ty_idx) - TY_size(ty_idx);
	    desc_ty_idx = ty_idx;
	  }
	}
	else {
	  if (TY_size(desc_ty_idx) > TY_size(ty_idx)) 
	    ty_idx = desc_ty_idx;
	}

	TYPE_ID rtype = Widen_Mtype(TY_mtype(ty_idx));
	TYPE_ID desc = TY_mtype(desc_ty_idx);
	if (MTYPE_is_integral(desc)) {
	  if (MTYPE_signed(rtype) != MTYPE_signed(desc)) {
	    if (MTYPE_size_min(rtype) > MTYPE_size_min(desc) ||
		is_bit_field)
	      rtype = Mtype_TransferSign(desc, rtype);
	    else desc = Mtype_TransferSign(rtype, desc);
	  }
	}

	if (TREE_THIS_VOLATILE(exp))
	  Set_TY_is_volatile(hi_ty_idx);

	Is_True(! is_bit_field || field_id <= MAX_FIELD_ID,
		("WFE_Expand_Expr: field id for bit-field exceeds limit"));

        if (!WFE_Keep_Zero_Length_Structs &&
            rtype == MTYPE_M              &&
            TY_size (hi_ty_idx) == 0) {
	  if (WN_has_side_effects (wn0)) {
	    wn = WN_CreateEval (wn0);
	    WFE_Stmt_Append (wn, Get_Srcpos());
	  }
	  wn = NULL;
        }
        else {
	  // special case indexing into a constant string
	  if (WN_operator (wn0) == OPR_LDA          &&
	      ST_class (WN_st (wn0)) == CLASS_CONST &&
	      is_bit_field == FALSE                 &&
	      field_id == 0) {
            st = WN_st (wn0);
	    TCON tcon = Tcon_Table [ST_tcon (st)];
	    if (TCON_ty (tcon) == MTYPE_STRING &&
                TY_size (Be_Type_Tbl (desc)) == 1) {
	      mUINT32 len = Targ_String_Length (tcon);
	      mUINT64 offset = component_offset + xtra_BE_ofst + WN_offset (wn0);
	      if (offset <= len    &&
		  desc == MTYPE_U1 &&
		  (rtype == MTYPE_U4 || rtype == MTYPE_U8)) {
		unsigned char *cp = (unsigned char *) Targ_String_Address (tcon);
		unsigned long long val = cp [offset];
		wn = WN_Intconst (rtype, val);
		break;
	      }
	      else
	      if (offset <= len    &&
		  desc == MTYPE_I1 &&
		  (rtype == MTYPE_I4 || rtype == MTYPE_I8)) {
		signed char *cp = (signed char *) Targ_String_Address (tcon);
		signed long long val = cp [offset];
		wn = WN_Intconst (rtype, val);
		break;
	      }
	    }
	  }
	  if (need_result)
	    wn = WN_CreateIload(OPR_ILOAD, rtype,
				is_bit_field ? MTYPE_BS : desc, 
				component_offset+xtra_BE_ofst,
				field_id != 0 ? hi_ty_idx : ty_idx, 
				Make_Pointer_Type (hi_ty_idx, FALSE),
				wn0, field_id);
	  else
	  if (WN_has_side_effects (wn0))
	    wn = wn0;
	}
      }
      break;

    case CONVERT_EXPR:
    case FLOAT_EXPR:
      {
        wn0 = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
	ty_idx = Get_TY (TREE_TYPE(exp));
	TYPE_ID mtyp = TY_mtype(ty_idx);
	if (mtyp == MTYPE_V)
	  wn = wn0;
	else {
	  mtyp = Widen_Mtype(TY_mtype(ty_idx));
	  if (mtyp == WN_rtype(wn0) || mtyp == MTYPE_V)
	    wn = wn0;
	  else {
#ifdef KEY // prevent zero extension when converting to 64-bit address type
	    if (TREE_CODE(TREE_TYPE(exp)) == POINTER_TYPE &&
		MTYPE_byte_size(FE_Pointer_Type_To_Mtype()) == 8) {
	      if (WN_operator(wn0) == OPR_CVT && WN_desc(wn0) == MTYPE_U4) {
		WN_set_desc(wn0, MTYPE_I4);
		wn = WN_Cvt(WN_rtype(wn0), mtyp, wn0);
	      }
	      else if (MTYPE_byte_size(WN_rtype(wn0) == 4))
		wn = WN_Cvt(MTYPE_I4, mtyp, wn0);
	      else wn = WN_Cvt(WN_rtype(wn0), mtyp, wn0);
	    }
	    else
#endif
	    wn = WN_Cvt(WN_rtype(wn0), mtyp, wn0);
	    // The following opcodes are not valid for MIPS
	    if (WN_opcode(wn) == OPC_I4U4CVT ||
	        WN_opcode(wn) == OPC_U4I4CVT ||
	        WN_opcode(wn) == OPC_I8U8CVT ||
	        WN_opcode(wn) == OPC_U8I8CVT) {
	      wn = WN_kid0 (wn);
	    }
	  }
	}
      }
      break;

    case FIX_TRUNC_EXPR:
      {
        wn0 = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
	ty_idx = Get_TY (TREE_TYPE(exp));
         TYPE_ID mtype = Widen_Mtype(TY_mtype(ty_idx));
         if(WN_operator(wn0) == OPR_CVT &&
	    MTYPE_is_integral(WN_desc(wn0)) &&
	    MTYPE_is_float(WN_rtype(wn0))){
           wn1 = WN_kid0(wn0);
           TYPE_ID kid_type = WN_rtype(wn1);
           if(mtype == kid_type){
             wn = wn1;
           }
           else{
             wn = WN_Cvt(WN_rtype(wn1), mtype, wn1);
           }
         }
         else
           wn = WN_Trunc(WN_rtype(wn0), mtype, wn0);

      }
      break;

#ifdef GPLUSPLUS_FE
    case EXPR_STMT:
      {
#ifdef KEY
        wn = WFE_Expand_Expr (EXPR_STMT_EXPR(exp), false, nop_ty_idx,
			    component_ty_idx, component_offset, field_id,
			    is_bit_field, is_aggr_init_via_ctor);
#else
	wn = WFE_Expand_Expr (EXPR_STMT_EXPR(exp), false);
#endif
      }
      break;

    case STMT_EXPR:
      {
#ifdef KEY
	bool write_to_target_wn = TRUE;

	// If we need to store the result in target_wn, then give result the
	// same ST as the ST in target_wn.  To do this, first find the
	// STMT_EXPR's result, which is returned by the last EXPR_STMT in the
	// COMPOUND_STMT.  Based on code in gnu/c-common.c:c_expand_expr.
	if (target_wn != NULL
	    && TREE_CODE (STMT_EXPR_STMT (exp)) == COMPOUND_STMT
	    && TREE_CODE (COMPOUND_BODY (STMT_EXPR_STMT (exp))) == SCOPE_STMT) {
	  tree expr = COMPOUND_BODY (STMT_EXPR_STMT (exp));
	  tree last = TREE_CHAIN (expr);

	  while (TREE_CHAIN (last)) {
	    expr = last;
	    last = TREE_CHAIN (last);
	  }

	  if (TREE_CODE (last) == SCOPE_STMT
	      && TREE_CODE (expr) == EXPR_STMT) {
	    if (TREE_CODE (EXPR_STMT_EXPR (expr)) == VAR_DECL) {
	      // If the last expression is a variable, then the variable is the
	      // returned value.
	      tree var_decl = EXPR_STMT_EXPR (expr);
	      ST *st = DECL_ST (var_decl);
	      if (st == NULL) {
		// Give the returned var_decl the same ST indicated by
		// target_wn.
		if (WN_operator(target_wn) == OPR_LDA) {
		  set_DECL_ST(var_decl, WN_st(target_wn));
		} else if (WN_operator(target_wn) == OPR_LDID) {
		  // target_wn is an ldid of the fake first parm.  Change the
		  // stmt_expr's var_decl to be an indirect_ref of the fake
		  // parm.
		  tree ptr_var =
		    build_decl(VAR_DECL, NULL_TREE,
			       build_pointer_type(TREE_TYPE(var_decl)));
		  TREE_SET_CODE(var_decl, INDIRECT_REF);
		  TREE_OPERAND(var_decl, 0) = ptr_var;
		  set_DECL_ST(ptr_var, WN_st(target_wn));
		} else {
		  FmtAssert(FALSE,
			 ("WFE_Expand_Expr: unexpected operator in target_wn"));
		}
	      } else {
		// The var_decl already has a ST assigned.  This should be the
		// same ST as the target_wn.
		FmtAssert(st == WN_st(target_wn),
			  ("WFE_Expand_Expr: STs are different"));
	      }
	      // Don't need target_wn anymore since the returned var_decl
	      // already has the target ST.
	      write_to_target_wn = FALSE;

	    } else if (TREE_CODE (EXPR_STMT_EXPR (expr)) == INDIRECT_REF) {
	      // The indirect_ref must have been a var_decl that was changed by
	      // kg++fe to an indirect_ref.  This means we are already writing
	      // to the target location.
	      write_to_target_wn = FALSE;
	    }
	  }
	}
	wn = WFE_Expand_Expr (TREE_OPERAND (exp, 0), need_result, nop_ty_idx,
			      component_ty_idx, component_offset, field_id,
			      is_bit_field, is_aggr_init_via_ctor,
			      write_to_target_wn ? target_wn : NULL);
#else
        wn = WFE_Expand_Expr (TREE_OPERAND (exp, 0), need_result);
#endif
      }
      break;

#ifndef KEY
    case SUBOBJECT:
      break;
#endif // !KEY

    case CLEANUP_POINT_EXPR: 
      {
        Push_Temp_Cleanup(exp, false);
        wn = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
        WN * cleanup_block = WN_CreateBlock ();
        WFE_Stmt_Push (cleanup_block, wfe_stmk_temp_cleanup, Get_Srcpos ());
        Do_Temp_Cleanups(exp);
        WFE_Stmt_Pop (wfe_stmk_temp_cleanup);
	if (wn && WN_has_side_effects (wn) && WN_first (cleanup_block)) {
	  DevWarn("CLEANUP_POINT_EXPR: expressson has side effects");
	  ty_idx = Get_TY (TREE_TYPE(exp));
	  st = Gen_Temp_Symbol (ty_idx, "__cleanup_point_expr");
#ifdef KEY
  	  WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, st);
#endif
	  TYPE_ID mtype = TY_mtype (ty_idx);
	  WFE_Set_ST_Addr_Saved (wn);
	  wn = WN_Stid (mtype, 0, st, ty_idx, wn);
	  WFE_Stmt_Append (wn, Get_Srcpos ());
	  wn = WN_Ldid (mtype, 0, st, ty_idx);
	}
	WFE_Stmt_Append (cleanup_block, Get_Srcpos ());
      }
      break;

    case THROW_EXPR:
      WFE_One_Stmt (TREE_OPERAND (exp, 0));
      Call_Throw();
      break;

    case TRY_CATCH_EXPR:
      DevWarn ("Encountered TRY_CATCH_EXPR at line %d:  ignored", lineno);
      break;

    case COMPOUND_STMT:
      {
	tree t = COMPOUND_BODY(exp);
	tree last_expr_stmt = 0;
	wn = NULL;
	while (t) {
	  if (TREE_CODE(t) == EXPR_STMT)
	    last_expr_stmt = t;
	  t = TREE_CHAIN(t);
	}

	t = COMPOUND_BODY(exp);

	while (t != last_expr_stmt) {
	  WFE_Expand_Stmt (t, target_wn);
	  t = TREE_CHAIN(t);
	}

	if (t) {
#ifdef KEY
	  wn =  WFE_Expand_Expr(t, need_result, nop_ty_idx, component_ty_idx,
				component_offset, field_id, is_bit_field,
				is_aggr_init_via_ctor, target_wn);
#else
	  wn =  WFE_Expand_Expr(t, need_result);
#endif
	  t = TREE_CHAIN(t);
	}

	while (t) {
	  WFE_Expand_Stmt(t, target_wn);
	  t = TREE_CHAIN(t);
	}

      }
      break;

    case EMPTY_CLASS_EXPR:
      DevWarn ("Encountered EMPTY_CLASS_EXPR at line %d\n", lineno);
      ty_idx = Get_TY (TREE_TYPE(exp));
      st = Gen_Temp_Symbol (ty_idx, "__empty_class_expr");
#ifdef KEY
      WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, st);
#endif
      wn = WN_Ldid (TY_mtype (ty_idx), 0, st, ty_idx);
      break;
#endif /* GLPUSPLUFE */

    // binary ops
    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case MAX_EXPR:
    case MIN_EXPR:
    case LSHIFT_EXPR:
    case BIT_AND_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case TRUNC_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case RDIV_EXPR:
    case EXACT_DIV_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
    case COMPLEX_EXPR:
    case CEIL_DIV_EXPR:
      {
#ifdef KEY
	TYPE_ID etype = TY_mtype(Get_TY(TREE_TYPE(exp)));
        wn0 = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
        wn1 = WFE_Expand_Expr (TREE_OPERAND (exp, 1));
        wn  = WN_Binary (Operator_From_Tree [code].opr,
                         Widen_Mtype(etype), wn0, wn1);

#if defined(TARG_SL)
        if(WN_operator(wn) == OPR_ADD &&
          (WN_operator(WN_kid0(wn)) == OPR_LDA ||
           WN_operator(WN_kid(wn, 1)) == OPR_LDA)) {

           if(WN_operator(WN_kid0(wn)) == OPR_LDA && WN_has_sym(WN_kid0(wn))) {
             ST* vbuf_sym = WN_st(WN_kid0(wn));
             if(ST_in_v2buf(vbuf_sym) || ST_in_v4buf(vbuf_sym)) {
               if(WN_operator(WN_kid1(wn)) == OPR_MPY) {
                 WN* tmp = WN_Intconst(MTYPE_U4,  ST_in_v2buf(vbuf_sym) ? 2 : 4); 
                 WN_Set_vbuf_ofst_adjusted(tmp, TRUE);
                 WN* new_wn = WN_Binary(OPR_MPY, MTYPE_I4, WN_kid1(wn), tmp);
                 WN_Set_vbuf_ofst_adjusted(new_wn, TRUE);
                 WN_kid1(wn) = new_wn;
               }
             }
           }
        }
#endif // TARG_SL	
	
	// bug 2649, 5503
	if ((MTYPE_is_integral(etype)) &&
	    (Widen_Mtype(etype) != etype) &&
	    (TY_size (Get_TY(TREE_TYPE(exp))) < 32) &&
	     (code == PLUS_EXPR || code == MINUS_EXPR || 
	     code == MULT_EXPR || code == LSHIFT_EXPR || 
	     code == BIT_XOR_EXPR || code == BIT_IOR_EXPR))
	  wn = WN_CreateCvtl(OPR_CVTL, Widen_Mtype(etype), MTYPE_V,
	                     TY_size (Get_TY(TREE_TYPE(exp))) * 8, wn);
#else
        wn0 = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
        wn1 = WFE_Expand_Expr (TREE_OPERAND (exp, 1));
        wn  = WN_Binary (Operator_From_Tree [code].opr,
                         Widen_Mtype(TY_mtype(Get_TY(TREE_TYPE(exp)))), wn0, wn1);
#endif
      }
      break;

    case LROTATE_EXPR:
      {
	ty_idx = Get_TY(TREE_TYPE(exp));
	TYPE_ID mtype = TY_mtype (ty_idx);
        wn0 = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
        wn1 = WFE_Expand_Expr (TREE_OPERAND (exp, 1));
	wn1 = WN_Binary (OPR_SUB, Widen_Mtype (mtype),
			 WN_Intconst (Widen_Mtype (mtype),
				      TY_size (ty_idx) * 8),
			 wn1);
	wn  = WN_Rrotate (TY_mtype(Get_TY(TREE_TYPE(exp))), wn0, wn1);
      }
      break;

    case RROTATE_EXPR:
      {
        wn0 = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
        wn1 = WFE_Expand_Expr (TREE_OPERAND (exp, 1));
	wn  = WN_Rrotate (TY_mtype(Get_TY(TREE_TYPE(exp))), wn0, wn1);
      }
      break;

    case RSHIFT_EXPR:
      {
	TYPE_ID mtyp = Widen_Mtype(TY_mtype(Get_TY(TREE_TYPE(exp))));
        wn0 = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
        wn1 = WFE_Expand_Expr (TREE_OPERAND (exp, 1));
        wn  = WN_Binary (MTYPE_signed(mtyp) ? OPR_ASHR : OPR_LSHR,
                         mtyp, wn0, wn1);
      }
      break;

    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
      {
#ifdef KEY
	// bug 2651: evaluate the 1st operand unconditionally
        wn0 = WFE_Expand_Expr (TREE_OPERAND (exp, 0));

	// Evaluate the second condition.  Add guard variable to the cleanup if
	// there is cleanup.
        WFE_Guard_Var_Push();
        wn1 = WFE_Expand_Expr_With_Sequence_Point (TREE_OPERAND (exp, 1),
						   Boolean_type);
        tree guard_var = WFE_Guard_Var_Pop();
	if (guard_var != NULL_TREE) {
	  WFE_add_guard_var(guard_var, wn1);
	}
#else
        wn0 = WFE_Expand_Expr_With_Sequence_Point (TREE_OPERAND (exp, 0),
						   Boolean_type);
        wn1 = WFE_Expand_Expr_With_Sequence_Point (TREE_OPERAND (exp, 1),
						   Boolean_type);
#endif
        wn  = WN_Binary (Operator_From_Tree [code].opr,
                         Boolean_type, wn0, wn1);
        if (Boolean_type != MTYPE_B &&
	    Widen_Mtype(TY_mtype(Get_TY(TREE_TYPE(exp)))) != Boolean_type)
	  wn = WN_Cvt (Boolean_type, Widen_Mtype(TY_mtype(Get_TY(TREE_TYPE(exp)))), wn);
      }
      break;

    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
      {
        wn0 = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
        wn1 = WFE_Expand_Expr (TREE_OPERAND (exp, 1));

	// check if conversion is needed
	ty_idx = Get_TY (TREE_TYPE(exp));
        TYPE_ID mtyp = TY_mtype(ty_idx);
	TY_IDX ty_idx0 = Get_TY(TREE_TYPE(TREE_OPERAND (exp, 0)));
	TYPE_ID mtyp0 = TY_mtype(ty_idx0);
	TY_IDX ty_idx1 = Get_TY(TREE_TYPE(TREE_OPERAND (exp, 1)));
	TYPE_ID mtyp1 = TY_mtype(ty_idx1);

	if (MTYPE_size_min(mtyp1) > MTYPE_size_min(mtyp0) &&
	    ! Has_Subsumed_Cvtl(WN_operator(wn0)))
	  wn0 = WN_CreateCvtl(OPR_CVTL, Widen_Mtype(mtyp0), MTYPE_V,
			      MTYPE_size_min(mtyp0), wn0);
	if (MTYPE_size_min(mtyp0) > MTYPE_size_min(mtyp1) &&
	    ! Has_Subsumed_Cvtl(WN_operator(wn1)))
	  wn1 = WN_CreateCvtl(OPR_CVTL, Widen_Mtype(mtyp1), MTYPE_V,
			      MTYPE_size_min(mtyp1), wn1);

        wn  = WN_Relational (Operator_From_Tree [code].opr,
			     Widen_Mtype(TY_mtype(Get_TY(TREE_TYPE(TREE_OPERAND(exp, 0))))),
			     wn0, wn1);
#if defined(TARG_SL)
// don't add conversion for float, it will cause to compiler to generate instruction dmtc1 
        if ((Widen_Mtype(mtyp0) != Boolean_type ) && !MTYPE_float(mtyp0))
          wn = WN_Cvt( Boolean_type, Widen_Mtype(mtyp0), wn);
#endif 
      }
      break;

    case COND_EXPR:
      {
        TY_IDX ty_idx1 = Get_TY (TREE_TYPE(TREE_OPERAND (exp, 1)));
        TY_IDX ty_idx2 = Get_TY (TREE_TYPE(TREE_OPERAND (exp, 2)));
	ty_idx = Get_TY (TREE_TYPE(exp));
	if(ty_idx != ty_idx1 && TY_mtype(ty_idx1) != MTYPE_V)
	  DevWarn("The type of COND_EXPR and its first kid mismatch!");
	if(ty_idx != ty_idx2 && TY_mtype(ty_idx2) != MTYPE_V)
	  DevWarn("The type of COND_EXPR and its second kid mismatch!"); 
#ifdef KEY // bug 2645
	wn0 = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
#else
	wn0 = WFE_Expand_Expr_With_Sequence_Point (TREE_OPERAND (exp, 0),
						   Boolean_type);
#endif
	// Due to a bug of handling ternary operators(i.e, COND_EXPR) in the GCC 3.3 front end,
	// WFE_Expand_Expr convert the ternary operator to an if/else statement.
	// 
	if (TY_mtype (ty_idx)  == MTYPE_V ||
            TY_mtype (ty_idx1) == MTYPE_V ||
            TY_mtype (ty_idx2) == MTYPE_V) {
	  WN *then_block = WN_CreateBlock ();
	  WN *else_block = WN_CreateBlock ();
	  WN *if_stmt    = WN_CreateIf (wn0, then_block, else_block);
	  WFE_Stmt_Append (if_stmt, Get_Srcpos());
	  WFE_Stmt_Push (then_block, wfe_stmk_if_then, Get_Srcpos());
	  wn1 = WFE_Expand_Expr (TREE_OPERAND (exp, 1), FALSE);
	  if (wn1) {
	    wn1 = WN_CreateEval (wn1);
	    WFE_Stmt_Append (wn1, Get_Srcpos());
	  }
	  WFE_Stmt_Pop (wfe_stmk_if_then);
	  WFE_Stmt_Push (else_block, wfe_stmk_if_else, Get_Srcpos());
	  wn2 = WFE_Expand_Expr (TREE_OPERAND (exp, 2), FALSE);
	  if (wn2) {
	    wn2 = WN_CreateEval (wn2);
	    WFE_Stmt_Append (wn2, Get_Srcpos());
	  }
	  WFE_Stmt_Pop (wfe_stmk_if_else);
        }
	else {
#ifdef KEY
	  // Prepare a guard variable for each part of the conditional, in case
	  // the conditional has a cleanup that is executed after the whole
	  // conditional expression is evaluated.  The guard variable ensures
	  // that a cleanup is executed only if its part of the conditional is
	  // executed.
	  WFE_Guard_Var_Push();
	  if(ty_idx != ty_idx1 &&
	     Ty_Table[ty_idx].kind == KIND_POINTER &&
	     Ty_Table[ty_idx1].kind == KIND_STRUCT) {
	    wn1 = WFE_Address_Of (TREE_OPERAND (exp, 1));
	  }
	  else{
	    wn1 = WFE_Expand_Expr_With_Sequence_Point (TREE_OPERAND (exp, 1),
						       TY_mtype (ty_idx),
						       target_wn);
	  }
	  
	  tree guard_var1 = WFE_Guard_Var_Pop();

	  WFE_Guard_Var_Push();
	  if(ty_idx != ty_idx2 &&
	     Ty_Table[ty_idx].kind == KIND_POINTER &&
	     Ty_Table[ty_idx2].kind == KIND_STRUCT) {
	    wn2 = WFE_Address_Of (TREE_OPERAND (exp, 2));
	  }
	  else{
	    wn2 = WFE_Expand_Expr_With_Sequence_Point (TREE_OPERAND (exp, 2),
						       TY_mtype (ty_idx),
						       target_wn);
	  }
	  tree guard_var2 = WFE_Guard_Var_Pop();

	  // Add guard variables if they are needed.
	  if (guard_var1 != NULL_TREE) {
	    WFE_add_guard_var(guard_var1, wn1);
	  }
	  if (guard_var2 != NULL_TREE) {
	    WFE_add_guard_var(guard_var2, wn2);
	  }
#else
	  wn1 = WFE_Expand_Expr_With_Sequence_Point (TREE_OPERAND (exp, 1),
						     TY_mtype (ty_idx));
	  wn2 = WFE_Expand_Expr_With_Sequence_Point (TREE_OPERAND (exp, 2),
						     TY_mtype (ty_idx));
#endif
	  wn  = WN_CreateExp3 (OPR_CSELECT, Mtype_comparison (TY_mtype (ty_idx)),
			   MTYPE_V, wn0, wn1, wn2);
	  Set_PU_has_very_high_whirl (Get_Current_PU ());
        }

        // bug fix for OSP_229
	// 
	FmtAssert ((wn != 0 || 
		   TY_mtype(ty_idx) == MTYPE_V ||
                   TY_mtype(ty_idx1) == MTYPE_V || 
		   TY_mtype(ty_idx2) == MTYPE_V),
		  ("WFE_Expand_Expr: NULL WHIRL tree for %s", 
		   Operator_From_Tree [code].name));
      }
      break;

    case INIT_EXPR:
#ifdef KEY
      // Put the result in the write target if there is a write target.
      if (target_wn != NULL &&
	  (TREE_CODE(TREE_OPERAND(exp, 0)) == VAR_DECL ||
	   TREE_CODE(TREE_OPERAND(exp, 0)) == RESULT_DECL ||
	   TREE_CODE(TREE_OPERAND(exp, 0)) == PARM_DECL)) {
	if (WN_operator(target_wn) == OPR_LDA) {
	  // target_wn is a LDA of a ST.  Give the result the same ST.
	  set_DECL_ST(TREE_OPERAND(exp, 0), WN_st(target_wn));
	} else if (WN_operator(target_wn) == OPR_LDID) {
	  // target_wn is a LDID of a ST, where ST points to the target
	  // location.  This only happens if ST is the fake first parm.  To
	  // have the INIT_EXPR write to the target area, change node X into an
	  // indirect ref of the ST, where X is the original target of the
	  // init_expr (var_decl/result_decl/parm_decl).  This will make all
	  // tree nodes that point to X now point to the indirect ref.
	  tree opnd0 = TREE_OPERAND(exp, 0);
	  tree ptr_var = build_decl(VAR_DECL, NULL_TREE,
				    build_pointer_type(TREE_TYPE(opnd0)));
	  TREE_SET_CODE(opnd0, INDIRECT_REF);
	  TREE_OPERAND(opnd0, 0) = ptr_var;
	  set_DECL_ST(ptr_var, WN_st(target_wn));
	}
      }

#ifdef NEW_INITIALIZER
      {
        tree opnd0 = TREE_OPERAND(exp, 0);
        tree opnd1 = TREE_OPERAND(exp, 1);
        if( TREE_CODE(opnd0) == INDIRECT_REF &&
            TREE_CODE(opnd1) == CONSTRUCTOR ) {
          WN* target = WFE_Address_Of(opnd0);
          ST* copy_st = WFE_Generate_Initialized_Aggregate(target, opnd1);
          ST* orig_st = WN_st(target);
          if ( ST_st_idx(copy_st) != ST_st_idx(orig_st) ) {
            // If the returned ST is not the original one,
            //   it means we create a new temp ST for initialization
            // One case is on x8664, the returned struct is converted into 
            //   the first FORMAL.
            // When initialize the FORMAL, we need to use a temp st.
            //   we copy the new ST into target here
            TY_IDX copy_ty = ST_type(copy_st);
            WN* ldid = WN_Ldid(TY_mtype(copy_ty), 0, copy_st, copy_ty);
            if ( WN_operator(target) == OPR_LDA ) {
              WFE_Stmt_Append(
                         WN_Stid (TY_mtype(copy_ty), WN_lda_offset(target),
                                  orig_st, ST_type(orig_st), ldid),
                         Get_Srcpos() );
            }
            else if ( WN_operator(target) == OPR_LDID ) {
              WFE_Stmt_Append(
                         WN_Istore(TY_mtype(copy_ty), 0,
                                   ST_type(orig_st), target, ldid),
                         Get_Srcpos() );
            }
            else {
              FmtAssert(FALSE, ("Bad operator for target, not LDA/LDID"));
            }
          }
          break;
        }
      }
#endif

      // fall through
#endif
    case MODIFY_EXPR:
      /*
       * When operand 1 of an init_expr or modify_expr is a target_expr,
       * then the temporary in the target_expr needs to be replaced by
       * operand 1 of the init_expr or modify_expr and the cleanup
       * (operand 2) of the target_expr needs to be zeroed out, since
       * no temporary will be generated so none should be destroyed.
       */
#ifdef KEY
       // Don't replace the temporary in the target_expr by the first operand
       // of the init_expr as the above comment says.  This is because we no
       // longer generate the MLDID-MSTID that copies the target_expr's
       // initialization result into the target_expr's init target (the
       // target_expr's temporary).  (Using MLDID-MSTID is incorrect when there
       // is a copy constructor.)  In the new scheme, the target_expr's
       // initializer writes directly into the target_expr's init target.
       // Doing the transformation in the above comment breaks this.  For
       // example:
       //
       //   init
       //     var_decl y  (node 1)
       //     target_expr
       //       var_decl x  (target_expr's init target, node 2)
       //       compound_expr (target_expr's initializer)
       //         call_expr
       //           addr_expr
       //           tree_list
       //             addr_expr  (arg 0)
       //               var_decl x
       //         var_decl x  (compound_expr returns x)
       //
       // After node 2 is replaced by node 1, the target_expr's init target
       // becomes y.  However, the target_expr's initializer still writes to x.
       // Without a MLDID-MSTID to copy x to y, the code is now incorrect.
       //
       // The solution is instead of replacing node 2 with node 1, we give both
       // x and y the same ST.  This follows the intention that x is just an
       // alias for y.
       //
       // The init's target can be an indirect ref.  (This occurs when we
       // changed the init target from a var_decl/result_decl/parm_decl to an
       // indirect ref based on a target_wn that is a LDID of a fake arg0 ST.)
       // In this case, replace the target_expr's var_decl x with the indirect
       // ref.  Change:
       //
       //   init
       //     indirect_ref    (node 1)
       //       var_decl y
       //     target_expr
       //       var_decl x    (node 2)
       //       initializer
       //
       // to:
       //
       //   init
       //     indirect_ref    (node 1)
       //       var_decl y
       //     target_expr
       //       indirect_ref  (node 2)
       //         var_decl y
       //       initializer
       //
       // After the transformation, target_expr's init target remains to be
       // node 2, but node 2 is renamed to be an indirect_ref node.  This will
       // cause all references inside the target_expr's initializer to
       // reference the location pointed to by the indirect_ref.

       // Handle case where initializer is a nop_expr (bug 3045):
       //   init
       //     indirect_ref
       //       var_decl y
       //     nop_expr
       //       target_expr
       //         var_decl x
       //         initializer
       //
       // Handle by expanding the nop_expr into the target location pointed to
       // by y.  Do this by setting target_wn to y before calling
       // WFE_Expand_Expr.  (This case was discovered after the scheme to
       // modify the init tree (described above) was implemented.  It seems the
       // correct approach is to simply set target_wn and then call
       // WFE_Expand_Expr to expand directly into target_wn, without modifying
       // the init tree.  Change to this scheme if more problems show up.)
       //
       // As below, only check for indirect_ref if the indirect_ref is created
       // by kg++fe to access through the fake arg0, in order to avoid
       // (indirect_ref (nop (var_decl))) which is generated by g++.
      {
	tree init_expr_opnd0 = TREE_OPERAND(exp, 0);
	if (TREE_CODE(TREE_OPERAND(exp, 1)) == NOP_EXPR &&
	    TREE_CODE(TREE_OPERAND(TREE_OPERAND(exp, 1), 0)) == TARGET_EXPR &&
	    TREE_CODE(init_expr_opnd0) == INDIRECT_REF &&
	    TREE_CODE(TREE_OPERAND(init_expr_opnd0, 0)) == VAR_DECL) {
	  tree t = TREE_OPERAND(exp, 1);
	  ST *st = Get_ST(TREE_OPERAND(init_expr_opnd0, 0));
	  WN *target_wn = WN_Ldid(Pointer_Mtype, 0, st, ST_type(st));
	  wn = WFE_Expand_Expr(t, TRUE, 0, 0, 0, 0, FALSE, FALSE, target_wn);
	  break;
	}
      }
#endif
      {
	tree t = NULL;
        if (TREE_CODE(TREE_OPERAND(exp, 1)) == TARGET_EXPR) {
	  t = TREE_OPERAND(exp, 1);
	  TREE_OPERAND(t, 2) = NULL_TREE;
        }
#ifdef KEY
        // Handle case where initializer is (nop (target ...)).  Bug 7792.
        else if (TREE_CODE(TREE_OPERAND(exp, 1)) == NOP_EXPR &&
                 TREE_CODE(TREE_OPERAND(TREE_OPERAND(exp, 1), 0))
		   == TARGET_EXPR) {
	  t = TREE_OPERAND(TREE_OPERAND(exp, 1), 0);
	  TREE_OPERAND(t, 2) = NULL_TREE;
        }
#endif

        if (t != NULL) {
#ifdef KEY
	  // Only check for indirect_ref if the indirect_ref is created by
	  // kg++fe to access through the fake arg0, in order to avoid
	  // (indirect_ref (nop (var_decl))) which is generated by g++.
	  tree init_expr_opnd0 = TREE_OPERAND(exp, 0);
          tree initializer = NULL;
	  if (TREE_CODE(init_expr_opnd0) == INDIRECT_REF &&
	      TREE_CODE(TREE_OPERAND(init_expr_opnd0, 0)) == VAR_DECL) {
	    tree target_expr_opnd0 = TREE_OPERAND(t, 0);
	    tree ptr_var =
	      build_decl(VAR_DECL, NULL_TREE,
		         TREE_TYPE(TREE_OPERAND(init_expr_opnd0, 0)));
	    TREE_SET_CODE(target_expr_opnd0, INDIRECT_REF);
	    TREE_OPERAND(target_expr_opnd0, 0) = ptr_var;
	    set_DECL_ST(ptr_var, DECL_ST(TREE_OPERAND(init_expr_opnd0, 0)));
	    wn = WFE_Expand_Expr(t);
	    break;
	  }
#ifdef PATHSCALE_MERGE
	  // The above is no longer true. INDIRECT_REFs generated by g++
	  // are also handled here for bug 12788. Handle:
	  //   init
	  //     indirect_ref
	  //       nop_expr
	  //         var_decl y
	  //     target_expr
	  //       var_decl x
	  //       initializer
	  else if (TREE_CODE(init_expr_opnd0) == INDIRECT_REF &&
	           TREE_CODE(TREE_OPERAND(init_expr_opnd0, 0)) == NOP_EXPR &&
	           TREE_CODE(TREE_OPERAND(TREE_OPERAND(init_expr_opnd0,0),0))
		                                           == VAR_DECL) {
	    tree target_expr_opnd0 = TREE_OPERAND(t, 0);
	    tree target_var = TREE_OPERAND(TREE_OPERAND(init_expr_opnd0, 0), 0);
	    tree ptr_var =
	      build_decl(VAR_DECL, NULL_TREE, TREE_TYPE(target_var));
	    TREE_SET_CODE(target_expr_opnd0, INDIRECT_REF);
	    TREE_OPERAND(target_expr_opnd0, 0) = ptr_var;
	    set_DECL_ST(ptr_var, DECL_ST(target_var));
	    wn = WFE_Expand_Expr(t);
	    break;
	  }
#endif // PATHSCALE_MERGE
#endif
	  if (TREE_CODE(TREE_OPERAND(exp, 0)) == VAR_DECL    ||
	      TREE_CODE(TREE_OPERAND(exp, 0)) == RESULT_DECL ||
	      TREE_CODE(TREE_OPERAND(exp, 0)) == PARM_DECL) {
#ifdef KEY
	    ST *st = Get_ST(TREE_OPERAND(exp, 0));
	    set_DECL_ST(TREE_OPERAND(t, 0), st);
#else
 	    TREE_OPERAND(t, 0) = TREE_OPERAND(exp, 0);
#endif
	    wn = WFE_Expand_Expr(t);
	    break;
	  }
          // OSP_397, initializing a class member having copy-constructor
          // fixed by Pathscale
          // Bug 13261: Handle INDIRECT_REF generated by g++, when its
          // operand is not a decl.
          else if (TREE_CODE(init_expr_opnd0) == INDIRECT_REF &&
                   ((initializer = TREE_OPERAND(t,1)) ||
                    (initializer = TREE_OPERAND(t,3))) &&
                   (TREE_CODE(initializer) == CALL_EXPR ||
                    TREE_CODE(initializer) == AGGR_INIT_EXPR)) {
            TREE_OPERAND(t,0) = init_expr_opnd0;
            wn = WFE_Expand_Expr(t);
            break;
          }
	  DevWarn ("INIT_EXPR/MODIFY_EXPR kid1 is TARGET_EXPR, kid0 is %s\n",
		   Operator_From_Tree [TREE_CODE(TREE_OPERAND(exp, 0))].name);
        }
      }

    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
      {
	if (TREE_CODE(TREE_OPERAND(exp, 1)) == ERROR_MARK)
	    break;
#ifdef KEY
	// If TREE_OPERAND(exp, 1) is a CALL_EXPR that returns a
	// ptr-to-member-function, then call
	// WFE_Expand_Ptr_To_Member_Func_Call_Expr to expand it.  Otherwise,
	// call WFE_Expand_Expr to do regular expansion.  Bug 4737.
	tree exp_opnd1 = TREE_OPERAND(exp, 1);
	if (WFE_Call_Returns_Ptr_To_Member_Func(exp_opnd1)) {
	  TYPE_ID desc = TY_mtype(Get_TY(TREE_TYPE(exp_opnd1)));
	  wn1 = WFE_Expand_Ptr_To_Member_Func_Call_Expr(exp_opnd1, 0,
						       Widen_Mtype(desc), desc);
        } else
#endif
        wn1 = WFE_Expand_Expr (TREE_OPERAND (exp, 1)); // r.h.s.

#ifdef TARG_SL
	wn  = WFE_Lhs_Of_Modify_Expr(code, TREE_OPERAND (exp, 0), TREE_OPERAND(exp,1), need_result, 
#else
	wn  = WFE_Lhs_Of_Modify_Expr(code, TREE_OPERAND (exp, 0), need_result, 
#endif
				     0, 0, 0, FALSE, wn1, 0, FALSE, FALSE);
      }
      break;

    // ternary ops

    case BIT_FIELD_REF:
      {
        wn = WFE_Expand_Expr (TREE_OPERAND (exp, 0), TRUE, nop_ty_idx, 
			      component_ty_idx, component_offset,
			      field_id, FALSE);
	ty_idx = Get_TY (TREE_TYPE(exp));
	TYPE_ID rtype = TY_mtype(ty_idx);
	UINT siz = TY_size(ty_idx);
	TYPE_ID desc;
	if (siz <= 8) {
	  if (MTYPE_signed(rtype))
	    desc = Mtype_AlignmentClass(siz, MTYPE_CLASS_INTEGER);
	  else desc = Mtype_AlignmentClass(siz, MTYPE_CLASS_UNSIGNED_INTEGER);
	  rtype = Widen_Mtype(desc);
	}
	else desc = rtype;
#ifdef KEY
	// bug 3074
        while (1) {
        if ((WN_operator(wn) == OPR_CVT)
            && (desc == rtype)
#ifdef TARG_SL
            /* for SL, we need to use ldub and etc to support
             * byte load, and we dont want to expand desc
             */
            && WN_operator(WN_kid0(wn)) != OPR_LDA
            && WN_operator(WN_kid0(wn)) != OPR_LDID
            && WN_operator(WN_kid0(wn)) != OPR_LDBITS
#endif
        )
            { // We do not need the CVT
                WN * del = wn;
                wn = WN_kid0 (wn);
                WN_Delete (del);
            }
        else break;
        }
#endif // KEY
	WN_set_rtype(wn, rtype);
	if (WN_desc(wn) != MTYPE_V)
	  WN_set_desc(wn, desc);
	INT bofst = Get_Integer_Value(TREE_OPERAND(exp, 2));
	INT bsiz =Get_Integer_Value(TREE_OPERAND(exp, 1));
	if ((bsiz & 7) == 0 &&	// field size multiple of bytes
	    MTYPE_size_min(desc) % bsiz == 0 && // accessed loc multiple of bsiz
	    bofst % bsiz == 0) {		// bofst multiple of bsiz
	  // not really a bit-field extraction!
	  if (WN_desc(wn) != MTYPE_V)
	    if (MTYPE_signed(rtype))
	      WN_set_desc(wn, Mtype_AlignmentClass(bsiz >> 3, MTYPE_CLASS_INTEGER));
	    else WN_set_desc(wn, Mtype_AlignmentClass(bsiz >> 3, MTYPE_CLASS_UNSIGNED_INTEGER));
	  WN_load_offset(wn) = WN_load_offset(wn) + (bofst >> 3);
	} else {
#ifdef KEY
          // bofst is ofst in bits from the base of the object.
          // Convert it to ofst from the beginning of the field, and update
          // the load offset using the proper alignment
          // The change is needed when we come here with bofst > base_type_size
          mUINT16 base_type_size = MTYPE_bit_size (desc);
          WN_load_offset(wn) += (bofst / base_type_size) * MTYPE_byte_size (desc);
          bofst = bofst % base_type_size;
#endif
	  if (WN_operator(wn) == OPR_LDID)
	    WN_set_operator(wn, OPR_LDBITS);
	  else WN_set_operator(wn, OPR_ILDBITS);
	  WN_set_bit_offset_size(wn, bofst, bsiz);
#ifdef KEY
	  TY_IDX ty = MTYPE_To_TY (WN_desc(wn));
	  WN_set_ty (wn, ty);
	  if (WN_operator(wn) == OPR_ILDBITS)
	    WN_set_load_addr_ty(wn, Make_Pointer_Type(ty));  // Bug 12394
	  break;
#endif
	}
	if (MTYPE_byte_size (WN_desc(wn)) != TY_size(WN_ty(wn)))
	  // the container is smaller than the entire struct
#ifdef KEY
	{
	  TY_IDX ty = MTYPE_To_TY (WN_desc(wn));
	  if ((TY_kind(Ty_Table[WN_ty(wn)]) == KIND_STRUCT)
              && (TY_kind(Ty_Table[ty]) != KIND_STRUCT))
	// if struct is being changed to a non-struct, the field-id
	// does not hold any more.
		WN_set_field_id (wn, 0);
	  WN_set_ty (wn, ty);
	  if (WN_operator(wn) == OPR_ILOAD || WN_operator(wn) == OPR_ILDBITS)
	    WN_set_load_addr_ty(wn, Make_Pointer_Type(ty));
	}
#else
	  WN_set_ty (wn, MTYPE_To_TY (WN_desc(wn)));
#endif
      }
      break;

    // n-ary ops

    case ARRAY_REF:
      {
	UINT xtra_BE_ofst = 0; 	// only needed for big-endian target
	TY_IDX elem_ty_idx;
	// generate the WHIRL array node
        wn0 = WFE_Array_Expr(exp, &elem_ty_idx, 0, 0, 0);

	// generate the iload node
	TY_IDX hi_ty_idx = Get_TY (TREE_TYPE(exp));
	desc_ty_idx = component_ty_idx;
	if (desc_ty_idx == 0)
          desc_ty_idx = hi_ty_idx;

        if (! MTYPE_is_integral(TY_mtype(desc_ty_idx)))
	  ty_idx = desc_ty_idx;
	else {
	  ty_idx = nop_ty_idx;
	  if (ty_idx == 0) 
	    ty_idx = desc_ty_idx;
	}

	if (! is_bit_field) {
	  if (TY_size(desc_ty_idx) > TY_size(ty_idx)) {
	    if (Target_Byte_Sex == BIG_ENDIAN)
	      xtra_BE_ofst = TY_size(desc_ty_idx) - TY_size(ty_idx);
	    desc_ty_idx = ty_idx;
	  }
	}
        else {
          if (TY_size(desc_ty_idx) > TY_size(ty_idx))
            ty_idx = desc_ty_idx;
        }

        TYPE_ID rtype = Widen_Mtype(TY_mtype(ty_idx));
        TYPE_ID desc = TY_mtype(desc_ty_idx);
        if (MTYPE_is_integral(desc)) {
          if (MTYPE_signed(rtype) != MTYPE_signed(desc)) {
            if (MTYPE_size_min(rtype) > MTYPE_size_min(desc) ||
		is_bit_field)
              rtype = Mtype_TransferSign(desc, rtype);
            else desc = Mtype_TransferSign(rtype, desc);
          }
        }

	Is_True(! is_bit_field || field_id <= MAX_FIELD_ID,
		("WFE_Expand_Expr: field id for bit-field exceeds limit"));
	wn = WN_CreateIload(OPR_ILOAD, rtype,
			    is_bit_field ? MTYPE_BS : desc, 
			    component_offset+xtra_BE_ofst,
			    field_id != 0 ? hi_ty_idx : ty_idx,
			    Make_Pointer_Type(elem_ty_idx, FALSE),
			    wn0, field_id);
      }
#if defined(TARG_SL)
     // following code is used to set if iload is a v1buf iload
     // set corresponding flag for automatic expand v1buf ld/st
     // in whirl2ops.cxx 
    if(Mark_LDA_Vbuf_Offset(wn, INTRN_VBUF_OFFSET)) 
      WN_Set_is_internal_mem_ofst(wn, TRUE);

    // following code used to handle assignment from one vbuf array value to 
    // another vbuf array value;
    // __v2buf array[2][3][16];
    // __v2buf array2[3][4][16];
    // array[0][1][2] = array2[2][3][4]
    //
    wn = Adjust_Vbuf_Array_Ofst(wn);
#endif       // TARG_SL

      break;

    case AGGR_INIT_EXPR:
    case CALL_EXPR:
      {
	INTRINSIC iopc = INTRINSIC_NONE;
	tree arglist = TREE_OPERAND (exp, 1);
        TYPE_ID ret_mtype;
        WN *call_wn;
        WN *arg_wn;
	TY_IDX  arg_ty_idx;
        TYPE_ID arg_mtype;
        INT num_args = 0;
	INT num_handlers = 0;
        INT i;
	tree list;
	arg0 = TREE_OPERAND (exp, 0);
	enum tree_code code0 = TREE_CODE (arg0);
	// KEY:  true if type must be returned in mem
	BOOL return_in_mem = FALSE;
#ifdef KEY
	ST *ret_st = NULL;		// return symbol
#endif
	for (list = TREE_OPERAND (exp, 1); list; list = TREE_CHAIN (list)) {
          arg_ty_idx = Get_TY(TREE_TYPE(TREE_VALUE(list)));
          if (!WFE_Keep_Zero_Length_Structs    &&
              TY_mtype (arg_ty_idx) == MTYPE_M &&
              TY_size (arg_ty_idx) == 0) {
            // zero length struct parameter
          }
          else
            num_args++;
        }
        ty_idx = Get_TY(TREE_TYPE(exp));
        if (need_result) {
          if (!WFE_Keep_Zero_Length_Structs  &&
              TY_mtype (ty_idx) == MTYPE_M   &&
              TY_size (ty_idx) == 0) {
            // zero length struct return
            ret_mtype = MTYPE_V;
          }
          else
            ret_mtype = TY_mtype (ty_idx);
#ifdef KEY
	  // If the type must be returned in memory, create a symbol and pass
	  // its address as the first param.
          if (TY_return_in_mem (ty_idx)) {
	    ret_mtype = MTYPE_V;
	    return_in_mem = TRUE;
            num_args++;		// first param is address of return symbol
	  }
#endif
        }
        else
          ret_mtype = MTYPE_V;
        st = NULL;
        if (code0 == ADDR_EXPR                  &&
            TREE_CODE (TREE_OPERAND (arg0, 0))) {
	  tree func = TREE_OPERAND (arg0, 0);
	  BOOL intrinsic_op = FALSE;
          BOOL whirl_generated = FALSE;

#ifdef KEY
	  // bug 8251: If we forcibly change the return type, we should
	  // generate a CVT.
	  TYPE_ID cvt_to = MTYPE_UNKNOWN;

	  if (DECL_ASSEMBLER_NAME(func))
	    TREE_SYMBOL_REFERENCED_BY_WHIRL(DECL_ASSEMBLER_NAME(func)) = 1;
#endif
          
	  if (DECL_BUILT_IN (func)) {
	    if (DECL_BUILT_IN_CLASS (func) != BUILT_IN_MD) {

            switch (DECL_FUNCTION_CODE (func)) {

	      case END_BUILTINS:
		break;

	      case BUILT_IN_STDARG_START:
#ifdef KEY
	      case BUILT_IN_VA_START:
#endif
	      {
#ifdef TARG_X8664
		if( TARGET_64BIT ){
		  iopc = INTRN_VA_START;
		  break;
		}
#endif
		arg1 = TREE_VALUE (arglist);
		arg2 = TREE_VALUE (TREE_CHAIN (arglist));
		WN *arg_wn = WFE_Expand_Expr (arg1);
		TY_IDX arg_ty_idx = Get_TY (TREE_TYPE (arg1));
		while (TREE_CODE (arg2) == NOP_EXPR
		       || TREE_CODE (arg2) == CONVERT_EXPR
		       || TREE_CODE (arg2) == NON_LVALUE_EXPR
		       || TREE_CODE (arg2) == INDIRECT_REF)
		  arg2 = TREE_OPERAND (arg2, 0);
		ST *st2 = Get_ST (arg2);
#if defined(TARG_X8664) || defined(TARG_SL) || defined(TARG_MIPS) || defined(TARG_LOONGSON)
		const int align = PARM_BOUNDARY / BITS_PER_UNIT;
		wn = WN_Lda (Pointer_Mtype, 
                             ((TY_size (ST_type (st2)) + align-1) & (-align)),
                             st2);
#else
		wn = WN_Lda (Pointer_Mtype, 
                             ((TY_size (ST_type (st2)) + 7) & (-8)),
                             st2);
#endif
		if (WN_operator (arg_wn) == OPR_LDA) {
			wn = WN_Stid (Pointer_Mtype, WN_offset (arg_wn),
				      WN_st (arg_wn), arg_ty_idx, wn);
		}
		else {
			wn = WN_CreateIstore (OPR_ISTORE, MTYPE_V,
					      Pointer_Mtype, 0, arg_ty_idx,
					      wn, arg_wn, 0);
		}

		WFE_Stmt_Append (wn, Get_Srcpos());
		whirl_generated = TRUE;
		wn = NULL;
		break;
	      }

	      case BUILT_IN_VA_COPY:
	      {
		arg1 = TREE_VALUE (arglist);
		arg2 = TREE_VALUE (TREE_CHAIN (arglist));
                TY_IDX arg_ty_idx = Get_TY (TREE_TYPE (arg1));


#ifdef KEY
		/* Under -m32, convert a __builtin_va_copy to an assignment if the
		   type of va_list is not array.
		   Also, the original code seems to only work for -m64, like other
		   va_XYZ code; under -m32, the source address is wrong.  (bug#2601)
		   (But even under -m64, the using of memcpy is unnecessary.)
		 */
#if defined(TARG_X8664) || defined(TARG_SL) || defined(TARG_MIPS)
		if( !TARGET_64BIT )
#endif
                {
		  FmtAssert( TREE_CODE(arglist) != ARRAY_TYPE,
			     ("unexpected array type for intrinsic 'va_copy'") );
		  WN* addr = WFE_Expand_Expr( arg1 );
		  WN* value = WFE_Expand_Expr( arg2 );
		  wn = WN_CreateIstore( OPR_ISTORE, MTYPE_V, Pointer_Mtype,
					0, arg_ty_idx, value, addr, 0 );

		  WFE_Stmt_Append( wn, Get_Srcpos() );
		  whirl_generated = TRUE;
		  wn = NULL;
		  break;
		}
#endif // KEY

		WN *dst  = WN_CreateParm (Pointer_Mtype, WFE_Expand_Expr (arg1),
					  arg_ty_idx, WN_PARM_BY_VALUE);
		WN *src  = WN_CreateParm (Pointer_Mtype, WFE_Expand_Expr (arg2),
					  arg_ty_idx, WN_PARM_BY_VALUE);
		WN *size = WN_CreateParm (MTYPE_I4,
					  WN_Intconst(MTYPE_I4,TY_size(TY_pointed(arg_ty_idx))),
					  Be_Type_Tbl(MTYPE_I4), WN_PARM_BY_VALUE);
		wn = WN_Create (OPR_INTRINSIC_CALL, ret_mtype, MTYPE_V, 3);
		WN_intrinsic (wn) = INTRN_MEMCPY;
		WN_kid0 (wn) = dst;
		WN_kid1 (wn) = src;
		WN_kid2 (wn) = size;
		WFE_Stmt_Append (wn, Get_Srcpos());
		whirl_generated = TRUE;
		wn = NULL;
		break;
	      }

	      case BUILT_IN_VA_END:
	      {
		arg1 = TREE_VALUE (arglist);
		wn = WN_CreateEval ( WFE_Expand_Expr (arg1) );
		WFE_Stmt_Append (wn, Get_Srcpos());
		whirl_generated = TRUE;
		wn = NULL;
		break;
	      }

	      case BUILT_IN_NEXT_ARG:
	      {
                tree last_parm = tree_last 
				   (DECL_ARGUMENTS (Current_Function_Decl()));
		while (TREE_CODE (last_parm) == NOP_EXPR
		       || TREE_CODE (last_parm) == CONVERT_EXPR
		       || TREE_CODE (last_parm) == NON_LVALUE_EXPR
		       || TREE_CODE (last_parm) == INDIRECT_REF)
		  last_parm = TREE_OPERAND (last_parm, 0);
		ST *st = Get_ST (last_parm);
		arg_wn = WN_Lda (Pointer_Mtype, ST_ofst(st), st);
		wn = WN_Binary (OPR_ADD, Pointer_Mtype, arg_wn,
				WN_Intconst (Pointer_Mtype,
					     Parameter_Size(ST_size(st))));
                whirl_generated = TRUE;
		break;
	      }

              case BUILT_IN_ALLOCA:
		Set_PU_has_alloca (Get_Current_PU ());
		Set_PU_has_user_alloca (Get_Current_PU ());
                arg_wn = WFE_Expand_Expr (TREE_VALUE (TREE_OPERAND (exp, 1)));
	        wn = WN_CreateAlloca (arg_wn);
                whirl_generated = TRUE;
                break;

              case BUILT_IN_MEMCPY:
		iopc = INTRN_MEMCPY;
                break;

              case BUILT_IN_MEMCMP:
		iopc = INTRN_MEMCMP;
                break;

              case BUILT_IN_MEMSET:
		iopc = INTRN_MEMSET;
                break;

              case BUILT_IN_STRCPY:
		iopc = INTRN_STRCPY;
                break;

              case BUILT_IN_STRCMP:
#ifdef GPLUSPLUS_FE
		iopc = INTRN_STRCMP;
#else
		if (arglist == 0
		    /* Arg could be non-pointer if user redeclared this fcn wrong.  */
		    || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != POINTER_TYPE
		    || TREE_CHAIN (arglist) == 0
		    || TREE_CODE (TREE_TYPE (TREE_VALUE (TREE_CHAIN (arglist)))) != POINTER_TYPE)
		  break;
		else {
		  arg1 = TREE_VALUE (arglist);
		  arg2 = TREE_VALUE (TREE_CHAIN (arglist));
		  tree len1 = c_strlen (arg1);
		  if (len1) {
		    tree len2 = c_strlen (arg2);
		    if (len2) {
		      char *ptr1 = get_string_pointer (WFE_Expand_Expr (arg1));
		      char *ptr2 = get_string_pointer (WFE_Expand_Expr (arg2));
		      if (ptr1 && ptr2) {
			wn = WN_Intconst (MTYPE_I4,
					  strcmp (ptr1, ptr2));
			whirl_generated = TRUE;
			break;
		      }
		    }
		  }
		  iopc = INTRN_STRCMP;
//		  intrinsic_op = TRUE;
		}
#endif /* GPLUSPLUS_FE */
                break;

              case BUILT_IN_STRLEN:
#ifdef GPLUSPLUS_FE
		iopc = INTRN_STRLEN;
#else
		if (arglist == 0
		/* Arg could be non-pointer if user redeclared this fcn wrong.  */
		   || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != POINTER_TYPE)
		  break;
		else {
		  tree src = TREE_VALUE (arglist);
		  tree len = c_strlen (src);
		  if (len) {
		    wn = WFE_Expand_Expr (len);
		    whirl_generated = TRUE;
		  }
		  else {
		    iopc = INTRN_STRLEN;
//		    intrinsic_op = TRUE;
		  }
		}
#endif /* GPLUSPLUS_FE */
                break;

#ifdef KEY
	    case BUILT_IN_FLOOR:
	      arg_wn = WFE_Expand_Expr (TREE_VALUE (TREE_OPERAND (exp, 1)));
#if defined TARG_MIPS
	      iopc = INTRN_FLOOR;
	      intrinsic_op = TRUE;
#else
	      wn = WN_CreateExp1 (OPR_FLOOR, ret_mtype , MTYPE_F8, arg_wn);
              whirl_generated = TRUE;
#endif	      
	      break;

	    case BUILT_IN_FLOORF: 
              arg_wn = WFE_Expand_Expr (TREE_VALUE (TREE_OPERAND (exp, 1)));
#if defined TARG_MIPS
	      iopc = INTRN_FLOORF;
	      intrinsic_op = TRUE;
#else
	      wn = WN_CreateExp1 (OPR_FLOOR, ret_mtype, MTYPE_F4, arg_wn);
              whirl_generated = TRUE;
#endif	      
	      break;

#ifndef TARG_MIPS
            case BUILT_IN_FLOORL:
              arg_wn = WFE_Expand_Expr (TREE_VALUE (TREE_OPERAND (exp, 1)));
#ifdef TARG_IA64
	      wn = WN_CreateExp1 (OPR_FLOOR, ret_mtype, MTYPE_F10, arg_wn);
#else
	      wn = WN_CreateExp1 (OPR_FLOOR, ret_mtype, MTYPE_FQ, arg_wn);
#endif // TARG_IA64	      
	      whirl_generated = TRUE;
              break;
#endif // ! TARG_MIPS
#endif // KEY

#ifdef KEY
	      case BUILT_IN_SQRT:
		if( flag_errno_math ){
		  break;
		}
#else
              case BUILT_IN_FSQRT:
#endif
                arg_wn = WFE_Expand_Expr (TREE_VALUE (TREE_OPERAND (exp, 1)));
                wn = WN_CreateExp1 (OPR_SQRT, ret_mtype, MTYPE_V, arg_wn);
                whirl_generated = TRUE;
                break;

              case BUILT_IN_SIN:
		intrinsic_op = TRUE;
#ifdef TARG_X8664
                if (!Force_IEEE_Comparisons)
                {
                  iopc = INTRN_SINL;
		  if (ret_mtype != MTYPE_FQ)
		  {
		    // generate a cvt to 'cvt_to'
		    cvt_to = ret_mtype;
		    ret_mtype = MTYPE_FQ;
		  }
                  break;
                }
#endif
#ifdef KEY
                if (ret_mtype == MTYPE_V) ret_mtype = MTYPE_F8;
#endif
		     if (ret_mtype == MTYPE_F4) iopc = INTRN_F4SIN;
                else if (ret_mtype == MTYPE_F8) iopc = INTRN_F8SIN;
                else Fail_FmtAssertion ("unexpected mtype for intrinsic 'sin'");
                break;

              case BUILT_IN_COS:
		intrinsic_op = TRUE;
#ifdef TARG_X8664
                if (!Force_IEEE_Comparisons)
                {
                  iopc = INTRN_COSL;
		  if (ret_mtype != MTYPE_FQ)
		  {
		    // generate a cvt to 'cvt_to'
		    cvt_to = ret_mtype;
		    ret_mtype = MTYPE_FQ;
		  }
                  break;
                }
#endif
#ifdef KEY
                if (ret_mtype == MTYPE_V) ret_mtype = MTYPE_F8;
#endif
		     if (ret_mtype == MTYPE_F4) iopc = INTRN_F4COS;
                else if (ret_mtype == MTYPE_F8) iopc = INTRN_F8COS;
                else Fail_FmtAssertion ("unexpected mtype for intrinsic 'cos'");
                break;

#ifdef KEY
              case BUILT_IN_EXP:
	      case BUILT_IN_EXPF:
		// bug 3390
		// If return type is void, generate an intrinsic assuming
		// double (so if it is without side-effects, optimizer can 
		// remove it)
		if (ret_mtype == MTYPE_V) ret_mtype = MTYPE_F8;

                if (ret_mtype == MTYPE_F4) iopc = INTRN_F4EXP;
                else if (ret_mtype == MTYPE_F8) iopc = INTRN_F8EXP;
                else Fail_FmtAssertion ("unexpected mtype for intrinsic 'exp'");
		intrinsic_op = TRUE;
                break;

	    case BUILT_IN_POW:

                // Bug 8195: If for whatever reason the pow(3) call is unused,
                // need_result will be false. Then, the value that this very
                // function assigns to ret_mtype for pow(3) is MTYPE_V. So,
                // just like we handle BUILT_IN_EXP above, we need to reassign
                // ret_mtype to MTYPE_F8.

                // Note that since pow[lf](3) are not builtin's (unlike the way
                // exp[lf]?(3)'s are), we only permit ret_mtype MTYPE_F8 here.

                if (ret_mtype == MTYPE_V) ret_mtype = MTYPE_F8;

	        FmtAssert(ret_mtype == MTYPE_F8, 
			  ("unexpected mtype for intrinsic 'pow'"));
		iopc = INTRN_F8EXPEXPR;
		intrinsic_op = TRUE;
		break;
#endif // KEY

              case BUILT_IN_CONSTANT_P:
              {
                tree arg = TREE_VALUE (TREE_OPERAND (exp, 1));
                STRIP_NOPS (arg);
                if (really_constant_p (arg)
                    || (TREE_CODE (arg) == ADDR_EXPR
                        && TREE_CODE (TREE_OPERAND (arg, 0)) == STRING_CST))
		{
                  wn = WN_Intconst (MTYPE_I4, 1);
		  whirl_generated = TRUE; // KEY
		}
#ifdef KEY
// If not yet compile-time constant, let the backend decide if it is 
// a constant
		else
		{
		  iopc = INTRN_CONSTANT_P;
                  if (ret_mtype == MTYPE_V) ret_mtype = MTYPE_I4;
		  intrinsic_op = TRUE;
		}
#else

                else
                  wn = WN_Intconst (MTYPE_I4, 0);
//                wn = WFE_Expand_Expr (TREE_VALUE (TREE_OPERAND (exp, 1)));
                whirl_generated = TRUE;
#endif // KEY
                break;
              }

              case BUILT_IN_RETURN_ADDRESS:
                i = Get_Integer_Value (TREE_VALUE (TREE_OPERAND (exp, 1)));
		if (i > 0) {
			// currently don't handle levels > 0,
			// which requires iterating thru call-stack
			// and finding $ra in fixed place.
			warning("non-zero levels not supported for builtin_return_address");
			wn = WN_Intconst(Pointer_Mtype, 0);
		}
		else {
			st = WFE_Get_Return_Address_ST (i);
			wn = WN_Ldid (Pointer_Mtype, 0, st, ST_type (st));
		}
                whirl_generated = TRUE;
		break;

#ifdef KEY
              case BUILT_IN_EXTRACT_RETURN_ADDR:
		list = TREE_OPERAND (exp, 1);
		wn   = WFE_Expand_Expr (TREE_VALUE (list));
                wn = WN_Binary (OPR_BAND, Pointer_Mtype, wn, 
				WN_Intconst(Pointer_Mtype, -2));
                whirl_generated = TRUE;
		break;

              case BUILT_IN_FRAME_ADDRESS:
		Set_PU_has_alloca(Get_Current_PU());
		iopc = MTYPE_byte_size(Pointer_Mtype) == 4 ?
		   	 INTRN_U4READFRAMEPOINTER : INTRN_U4READFRAMEPOINTER;
		intrinsic_op = TRUE;
		break;
	      case BUILT_IN_APPLY_ARGS:
		Set_PU_has_alloca(Get_Current_PU());
		iopc = INTRN_APPLY_ARGS;
		break;	
	      case BUILT_IN_APPLY:
		{
		  WN *load_wn, *sp_addr;

		  Set_PU_has_alloca(Get_Current_PU());

		  iopc = INTRN_APPLY;
		  call_wn = WN_Create (OPR_INTRINSIC_CALL, ret_mtype, MTYPE_V, 
				       num_args);
		  WN_intrinsic (call_wn) = iopc;
		  WN_Set_Linenum (call_wn, Get_Srcpos());
		  WN_Set_Call_Default_Flags (call_wn);
		  i = 0;
		  BOOL generate_mload = FALSE;
		  WN *kid1 = NULL;
		  WN *kid2 = NULL;
		  for (list = TREE_OPERAND (exp, 1);
		       list;
		       list = TREE_CHAIN (list)) {
		    arg_wn     = WFE_Expand_Expr (TREE_VALUE (list));
		    if (i == 1)
		      kid1 = arg_wn;
		    if (i == 2 && 
			WN_operator(arg_wn) != OPR_INTCONST) {
		      generate_mload = TRUE;
		      kid2 = arg_wn;
		    } else if (i == 2)
		      kid2 = arg_wn;
		    arg_ty_idx = Get_TY(TREE_TYPE(TREE_VALUE(list)));
		    arg_mtype  = TY_mtype(arg_ty_idx);
		    arg_wn = WN_CreateParm (Mtype_comparison (arg_mtype), 
					    arg_wn,
					    arg_ty_idx, WN_PARM_BY_VALUE);
		    WN_kid (call_wn, i++) = arg_wn;
		  }

		  // Store SP & Alloca
		  TY_IDX ty_idx = 
		    Make_Pointer_Type (Be_Type_Tbl (MTYPE_V), FALSE);
		  ST* alloca_st_0 = 
		    Gen_Temp_Symbol (ty_idx, 
				     "__builtin_apply_alloca0");
		  WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL,
						       alloca_st_0);
		  WN *alloca_0 = 
		    WN_CreateAlloca (WN_CreateIntconst (OPC_I4INTCONST, 0));
		  WN *alloca_kid0 = alloca_0;
		  alloca_kid0 = 
		    WN_Stid (Pointer_Mtype, 
			     0, alloca_st_0, ty_idx, alloca_kid0);
		  WFE_Stmt_Append (alloca_kid0, Get_Srcpos());
		  ST *alloca_st_1 = 
		    Gen_Temp_Symbol (ty_idx, 
				     "__builtin_apply_alloca1");
		  WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL,
		  				       alloca_st_1);
		  WN *alloca_1 = WN_CreateAlloca (kid2);
		  WN *alloca_kid1 = alloca_1;
		  alloca_kid1 = WN_Stid (Pointer_Mtype, 
					 0, alloca_st_1, ty_idx, alloca_kid1);
		  WFE_Stmt_Append (alloca_kid1, Get_Srcpos());

		  // The src is actually in 0(kid1)
		  kid1 = 
		    WN_CreateIload (OPR_ILOAD, MTYPE_I4, MTYPE_I4, 0,
				    MTYPE_To_TY(MTYPE_I4), 
				    Make_Pointer_Type(MTYPE_To_TY(MTYPE_U8)), 
				    kid1, 0);
		  load_wn = 
		    WN_CreateMload (0, 
				    Make_Pointer_Type(MTYPE_To_TY(MTYPE_U8)), 
				    kid1, kid2);
		  sp_addr = WN_LdidPreg(MTYPE_U4, 29); // $sp
		  WFE_Stmt_Append(WN_CreateMstore (0, 
			      Make_Pointer_Type(MTYPE_To_TY(MTYPE_U8)), 
						   load_wn,
						   sp_addr,
						   kid2),
				  Get_Srcpos());

		  WFE_Stmt_Append (call_wn, Get_Srcpos());

		  call_wn = WN_Create (OPR_ICALL, ret_mtype, MTYPE_V, 1);
		  WN_kid(call_wn, 0) = 
		    WFE_Expand_Expr (TREE_VALUE (TREE_OPERAND (exp, 1)));
		  WN_set_ty (call_wn, TY_pointed(Get_TY(
			    TREE_TYPE (TREE_VALUE(TREE_OPERAND (exp, 1))))));
		  WFE_Stmt_Append (call_wn, Get_Srcpos());		

		  TY_IDX tyi;
		  TY& ty = New_TY(tyi);
		  TY_Init(ty, 16, KIND_STRUCT, MTYPE_M,
			  Save_Str("__apply"));
		  Set_TY_align(tyi, 8);
		  ST *tmpst = New_ST(CURRENT_SYMTAB);
		  ST_Init(tmpst, TY_name_idx(ty),
			  CLASS_VAR, SCLASS_AUTO, EXPORT_LOCAL, tyi);
		  Set_ST_is_temp_var(tmpst);
		  WN *load, *store;
		  load = WN_LdidPreg(MTYPE_I8, 2); // $v0
		  store = WN_Stid(MTYPE_I8, 
				  (WN_OFFSET)0, tmpst, Spill_Int_Type, load);
		  WFE_Stmt_Append (store, Get_Srcpos());		
		  load = WN_LdidPreg(MTYPE_F8, 32); //$f0
		  store = WN_Stid(MTYPE_F8, 
				  (WN_OFFSET)8, tmpst, Spill_Int_Type, load);
		  WFE_Stmt_Append (store, Get_Srcpos());		
		  wn = WN_Lda (Pointer_Mtype, 0, tmpst, 
			       Make_Pointer_Type (ST_type(tmpst), FALSE));

		  // Dealloca/Restore SP
		  WN *dealloca_wn = WN_CreateDealloca (2);
		  WN_kid0 (dealloca_wn) = 
		    WN_Ldid (Pointer_Mtype, 
			     0, alloca_st_0, ST_type (alloca_st_0));
		  WN_kid1 (dealloca_wn) = 
		    WN_Ldid (Pointer_Mtype, 
			     0, alloca_st_1, ST_type (alloca_st_1));
		  WFE_Stmt_Append (dealloca_wn, Get_Srcpos());		
		  
		  whirl_generated = TRUE;
		  break;
		}
	      case BUILT_IN_RETURN:
		Set_PU_has_alloca(Get_Current_PU());
		iopc = INTRN_RETURN;
		break;	

                // Implement built-in versions of the ISO C99 floating point
                // comparison macros (that avoid raising exceptions for
                // unordered operands)
              case BUILT_IN_ISGREATER:
                iopc = INTRN_ISGREATER;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_ISGREATEREQUAL:
                iopc = INTRN_ISGREATEREQUAL;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_ISLESS:
                iopc = INTRN_ISLESS;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_ISLESSEQUAL:
                iopc = INTRN_ISLESSEQUAL;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_ISLESSGREATER:
                iopc = INTRN_ISLESSGREATER;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_ISUNORDERED:
                iopc = INTRN_ISUNORDERED;
                intrinsic_op = TRUE;
                break;

	      case BUILT_IN_EXPECT:
#ifdef KEY
                iopc = INTRN_EXPECT;
                intrinsic_op = TRUE;
                if (ret_mtype == MTYPE_V) ret_mtype = MTYPE_I4; // bug 12344
#else
                wn = WFE_Expand_Expr (TREE_VALUE (TREE_OPERAND (exp, 1)));
                whirl_generated = TRUE;
#endif
	        break;

              case BUILT_IN_FFS:
                iopc = INTRN_I4FFS;
                intrinsic_op = TRUE;
                if (ret_mtype == MTYPE_V)
                  ret_mtype = MTYPE_I4;
                break;

	      case BUILT_IN_EXTEND_POINTER:
		wn = WFE_Expand_Expr (TREE_VALUE (TREE_OPERAND (exp, 1)));
		whirl_generated = TRUE;
	        break;
	
	      case BUILT_IN_POPCOUNT:
	      case BUILT_IN_POPCOUNTL:
	      case BUILT_IN_POPCOUNTLL:
	        iopc = INTRN_POPCOUNT;
		intrinsic_op = TRUE;
		break;
	
	      case BUILT_IN_CTZ:
	      case BUILT_IN_CTZL:
	      case BUILT_IN_CTZLL:
	        iopc = INTRN_CTZ;
		intrinsic_op = TRUE;
		break;

	      case BUILT_IN_TRAP:
		call_wn = WN_Create (OPR_CALL, MTYPE_V, MTYPE_V, 0);
		st = Get_ST (TREE_OPERAND (arg0, 0));
		Set_ST_name_idx (st, Save_Str ("abort"));
		WN_st_idx (call_wn) = ST_st_idx (st);
		WN_Set_Linenum (call_wn, Get_Srcpos());
		WN_Set_Call_Default_Flags (call_wn);
		WFE_Stmt_Append (call_wn, Get_Srcpos());
		whirl_generated = TRUE;
		break;

	      case BUILT_IN_PREFETCH:
	        {
		  // prefetch address
		  tree pf_arg = TREE_OPERAND (exp, 1);
		  WN * pf_addr = WFE_Expand_Expr (TREE_VALUE (pf_arg));
		  // Note 2nd/3rd argument optional
		  // read/write access
		  pf_arg = TREE_CHAIN (pf_arg);
		  UINT32 pf_flag = 0;
		  int access = 0;
		  if (pf_arg && TREE_CODE (TREE_VALUE (pf_arg)) == INTEGER_CST)
		    access = Get_Integer_Value (TREE_VALUE (pf_arg));
		  if (access == 0)
		    PF_SET_READ (pf_flag);
		  else // should be 1 (write access)
		    PF_SET_WRITE (pf_flag);
		  // Ignore 3rd argument which gives a measure of temporal
		  // locality. LNO does analyze the temporal locality, but
		  // not sure what is a good way to encode it in PREFETCH.
		  PF_SET_MANUAL (pf_flag); // manual prefetch
		  WFE_Stmt_Append (WN_CreatePrefetch (0, pf_flag, pf_addr),
		                   Get_Srcpos());
		  whirl_generated = TRUE;
		}
		break;
#endif

#ifdef TARG_X8664
              case BUILT_IN_COSF:
              case BUILT_IN_COSL:
                if (!Force_IEEE_Comparisons)
                {
                  iopc = INTRN_COSL;
                  intrinsic_op = TRUE;
		  if (ret_mtype != MTYPE_FQ)
		  {
		    // generate a cvt to 'cvt_to'
		    cvt_to = ret_mtype;
		    ret_mtype = MTYPE_FQ;
		  }
                }
                break;
                                                                                
              case BUILT_IN_SINF:
              case BUILT_IN_SINL:
                if (!Force_IEEE_Comparisons)
                {
                  iopc = INTRN_SINL;
                  intrinsic_op = TRUE;
		  if (ret_mtype != MTYPE_FQ)
		  {
		    // generate a cvt to 'cvt_to'
		    cvt_to = ret_mtype;
		    ret_mtype = MTYPE_FQ;
		  }
                }
                break;
#endif // TARG_X8664

#ifdef KEY
#if !defined(TARG_SL)
              case BUILT_IN_TAN:
                // return type should only be F8 for tan
                if (ret_mtype == MTYPE_F8)
                {
                  iopc = INTRN_TAN;
                  intrinsic_op = TRUE;
                }
                break;
#endif
#endif
#if defined(TARG_SL)
           case BUILT_IN_PERIPHERAL_RW_BEGIN:
             iopc = INTRN_PERIPHERAL_RW_BEGIN; 
             break;
           case BUILT_IN_PERIPHERAL_RW_END:
             iopc = INTRN_PERIPHERAL_RW_END; 
             break;
	    case BUILT_IN_VBUF_OFFSET:
	      iopc = INTRN_VBUF_OFFSET;
	      intrinsic_op = TRUE;
	      break;
	    case BUILT_IN_VBUF_ABSOLUTE:
	      iopc = INTRN_VBUF_ABSOLUTE;
	      intrinsic_op = TRUE;
	      break;
	    case BUILT_IN_SBUF_OFFSET:
	      iopc = INTRN_SBUF_OFFSET;
	      intrinsic_op = TRUE;
	      break;
           case  BUILT_IN_CVT64_HIGH:
                iopc = INTRN_CVT64_HIGH;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_CVT64_LOW:
                iopc = INTRN_CVT64_LOW;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_CVT32:
                iopc = INTRN_CVT32;
                intrinsic_op = TRUE;
                break;
           case  BUILT_IN_LONGLONG_CVT64_HIGH:
                iopc = INTRN_LONGLONG_CVT64_HIGH;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_LONGLONG_CVT64_LOW:
                iopc = INTRN_LONGLONG_CVT64_LOW;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3_INIT_ACC:
                iopc = INTRN_C3_INIT_ACC;
                break;
           case BUILT_IN_C3_SAVE_ACC:
                iopc = INTRN_C3_SAVE_ACC;
                break;
           case BUILT_IN_C3_INIT_ADDR:
                iopc = INTRN_C3_INIT_ADDR;
                break;
           case BUILT_IN_C3_SAVE_ADDR:
                iopc = INTRN_C3_SAVE_ADDR;
                break;
           case BUILT_IN_C3_INIT_DACC:
                iopc = INTRN_C3_INIT_DACC;
                break;
           case BUILT_IN_C3_SAVE_DACC:
                iopc = INTRN_C3_SAVE_DACC;
                break;
           case BUILT_IN_C3_MVFS:
                iopc = INTRN_C3_MVFS;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_SET_ADDR:
                iopc = INTRN_C3_SET_ADDR;
                break;
           case BUILT_IN_SET_CIRCBUF:
                iopc = INTRN_C3_SET_CIRCBUF;
                break;
           case BUILT_IN_C3AADDA:
                iopc = INTRN_C3AADDA;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3BITR:
                iopc = INTRN_C3BITR;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3CS:
                iopc = INTRN_C3CS;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3DADD:
                iopc = INTRN_C3DADD;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3DMAC:
                iopc = INTRN_C3DMAC;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3DMACA:
                iopc = INTRN_C3DMAC_A;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3DMULA:
                iopc = INTRN_C3DMULA;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3DMULAA:
                iopc = INTRN_C3DMULA_A;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3DSHLLI:
                iopc = INTRN_C3DSHLL_I;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3FFE:
                iopc = INTRN_C3FFE;
                break;
           case BUILT_IN_C3LD:
                iopc = INTRN_C3LD;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3ST:
                iopc = INTRN_C3ST;
                break;
           case BUILT_IN_C3LEAD:
                iopc = INTRN_C3LEAD;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3MAC:
                iopc = INTRN_C3MAC;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3MACA:
                iopc = INTRN_C3MAC_A;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3MACAR:
                iopc = INTRN_C3MAC_AR;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3MACI:
                iopc = INTRN_C3MAC_I;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3MULA:
                iopc = INTRN_C3MULA;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3MULAA:
                iopc = INTRN_C3MULA_A;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3MULAAR:
                iopc = INTRN_C3MULA_AR;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3MULAI:
                iopc = INTRN_C3MULA_I;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3MULS:
                iopc = INTRN_C3MULS;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3MULUS:
                iopc = INTRN_C3MULUS;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3REVB:
                iopc = INTRN_C3REVB;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3ROUND:
                iopc = INTRN_C3ROUND;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3SAADDA:
                iopc = INTRN_C3SAADD_A;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3SAADDHA:
                iopc = INTRN_C3SAADDH_A;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3SAADDS:
                iopc = INTRN_C3SAADDS;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3SAADDSH:
                iopc = INTRN_C3SAADDSH;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3SADDA:
                iopc = INTRN_C3SADDA;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3SADDAA:
                iopc = INTRN_C3SADDA_A;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3SAMULHA:
                iopc = INTRN_C3SAMULH_A;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3SAMULSH:
                iopc = INTRN_C3SAMULSH;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3SHAV:
                iopc = INTRN_C3SHAV;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3SHLAFAI:
                iopc = INTRN_C3SHLAFA_I;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3SHLATAI:
                iopc = INTRN_C3SHLATA_I;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3SHLAI:
                iopc = INTRN_C3SHLA_I;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3SUBC:
                iopc = INTRN_C3SUBC;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C3NEGA:
                iopc = INTRN_C3NEGA;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_INIT_HI:
                iopc = INTRN_INIT_HI;
                break;
           case BUILT_IN_COPY_HI:
                iopc = INTRN_COPY_HI;
                intrinsic_op = TRUE;
                break;
           case BUILT_IN_C2_MVGR_R2G:
                iopc = INTRN_C2_MVGR_R2G;
	        break;
	   case BUILT_IN_C2_MVGR_G2R:
	        iopc = INTRN_C2_MVGR_G2R;
                break;
           case BUILT_IN_C2_MVGC_G2C:
                iopc = INTRN_C2_MVGC_G2C;	  
                break;
	   case BUILT_IN_C2_MVGC_C2G:
	        iopc = INTRN_C2_MVGC_C2G;
	        break;
           case BUILT_IN_C2_LD_V:
                iopc = INTRN_C2_LD_V;
                break;
           case BUILT_IN_C2_LD_G:
                iopc = INTRN_C2_LD_G;
                break;
           case BUILT_IN_C2_LD_S:
                iopc = INTRN_C2_LD_S;
                break;
           case  BUILT_IN_C2_ST_V: 
                iopc = INTRN_C2_ST_V;
                break;
	   case BUILT_IN_C2_ST_G:
	        iopc = INTRN_C2_ST_G;
	        break;
	   case BUILT_IN_C2_LD_G_IMM:
                iopc = INTRN_C2_LD_G_IMM;
                break;
	   case BUILT_IN_C2_LD_C_IMM:
                iopc = INTRN_C2_LD_C_IMM;
                break;
	   case BUILT_IN_C2_LD_V_IMM:
                iopc = INTRN_C2_LD_V_IMM;
                break;
	   case BUILT_IN_C2_ST_V_IMM:
                iopc = INTRN_C2_ST_V_IMM;
                break;
	   case BUILT_IN_C2_ST_C_IMM:
                iopc = INTRN_C2_ST_C_IMM;
                break;
	   case BUILT_IN_C2_ST_G_IMM:
                iopc = INTRN_C2_ST_G_IMM;
                break;
           case BUILT_IN_C2_VADDS:
                iopc = INTRN_C2_VADDS;
                break;
           case BUILT_IN_C2_VSUBS:
                iopc = INTRN_C2_VSUBS;
                break;
           case BUILT_IN_C2_VMUL:
                iopc = INTRN_C2_VMUL;
                break;
	   case BUILT_IN_C2_VNEG:
                iopc = INTRN_C2_VNEG;
                break;
           case BUILT_IN_C2_VSHFT:
                iopc = INTRN_C2_VSHFT;
                break;
           case BUILT_IN_C2_VCLP:
                iopc = INTRN_C2_VCLP;
                break;
           case BUILT_IN_C2_VCLG:
                iopc = INTRN_C2_VCLG;
                break;
           case BUILT_IN_C2_VCMOV:		
                iopc = INTRN_C2_VCMOV;
                break;
           case BUILT_IN_C2_LCZERO:		   	
                iopc = INTRN_C2_LCZERO;
                break;
           case BUILT_IN_C2_VRND:		
                iopc = INTRN_C2_VRND;
                break;
           case BUILT_IN_C2_VSPAS:		
                iopc = INTRN_C2_VSPAS;
                break;
           case BUILT_IN_C2_VSPEL:		
                iopc = INTRN_C2_VSPEL;
                break;
           case BUILT_IN_C2_VSPEL_MAC:		
                iopc = INTRN_C2_VSPEL_MAC;
                break;
           case BUILT_IN_C2_MMUL:		
                iopc = INTRN_C2_MMUL;
                break;
           case BUILT_IN_C2_VMOV:		
                iopc = INTRN_C2_VMOV;
                break;
           case BUILT_IN_C2_VCOPY:		
                iopc = INTRN_C2_VCOPY;
                break;
           case BUILT_IN_C2_VCMPR:		
                iopc = INTRN_C2_VCMPR;
                break;
           case BUILT_IN_C2_SAD:		
                iopc = INTRN_C2_SAD;
                break;
           case BUILT_IN_C2_SATD:		
                iopc = INTRN_C2_SATD;
                break;
           case BUILT_IN_C2_INTRA:	
                iopc = INTRN_C2_INTRA;
                break;
           case BUILT_IN_C2_MVSEL:		
                iopc = INTRN_C2_MVSEL;
                break;		
           case BUILT_IN_C2_BCST:		
                iopc = INTRN_C2_BCST;
                break;		
           case BUILT_IN_C2_VLCS:		
                iopc = INTRN_C2_VLCS;
                break;		
           case BUILT_IN_C2_VLCS_R:		
                iopc = INTRN_C2_VLCS_R;
                break;		
           case BUILT_IN_C2_ADDS:		
                iopc = INTRN_C2_ADDS;
                break;		
           case BUILT_IN_C2_ADDS_R:		
                iopc = INTRN_C2_ADDS_R;
                break;		
           case BUILT_IN_C2_SUBS:		
                iopc = INTRN_C2_SUBS;
                break;
           case BUILT_IN_C2_SUBS_R:		
                iopc = INTRN_C2_SUBS_R;
                break;
           case BUILT_IN_C2_MULS:		
                iopc = INTRN_C2_MULS;
                break;		
           case BUILT_IN_C2_MADS:		
                iopc = INTRN_C2_MADS;
                break;		
           case BUILT_IN_C2_SMADS:		
                iopc = INTRN_C2_SMADS;
                break;		
           case BUILT_IN_C2_CMOV:		
                iopc = INTRN_C2_CMOV;
                break;		
           case BUILT_IN_C2_MOV:		
                iopc = INTRN_C2_MOV;
                break;
           case BUILT_IN_C2_MOV_R:		
                iopc = INTRN_C2_MOV_R;
                break;
           case BUILT_IN_C2_CLP:		
                iopc = INTRN_C2_CLP;
                break;
           case BUILT_IN_C2_CHKRNG:		
                iopc = INTRN_C2_CHKRNG;
                break;
           case BUILT_IN_C2_SCOND:		
                iopc = INTRN_C2_SCOND;  
                break;		
           case BUILT_IN_C2_SCOND_R:		
                iopc = INTRN_C2_SCOND_R;  
                break;		
           case BUILT_IN_C2_SCOND_R_WB:		
                iopc = INTRN_C2_SCOND_R_WB;  
                break;		
           case BUILT_IN_C2_BOP:		
                iopc = INTRN_C2_BOP;
                break;		
           case BUILT_IN_C2_BDEP:		
                iopc = INTRN_C2_BDEP;
                break;		
           case BUILT_IN_C2_WRAP:		
                iopc = INTRN_C2_WRAP;
                break;		
           case BUILT_IN_C2_BXTR:		
                iopc = INTRN_C2_BXTR;
                break;		
           case BUILT_IN_C2_BXTRR48:		
                iopc = INTRN_C2_BXTRR48;
                break;		
           case BUILT_IN_C2_SUM4:		
                iopc = INTRN_C2_SUM4;
                break;		 
           case BUILT_IN_C2_SUM3_SADDR:		
                iopc = INTRN_C2_SUM3_SADDR;
                break;		 
           case BUILT_IN_C2_SUM4_R:		
                iopc = INTRN_C2_SUM4_R;
                break;		 
	   case BUILT_IN_C2_MED:
	        iopc = INTRN_C2_MED;
                break;
	   case BUILT_IN_C2_LD_V2G:
	        iopc = INTRN_C2_LD_V2G;
                break;
	   case BUILT_IN_C2_LD_V2G_IMM:
                iopc = INTRN_C2_LD_V2G_IMM;
                break;
	   case BUILT_IN_C2_ST_G2V:
                iopc = INTRN_C2_ST_G2V;
                break;
	   case BUILT_IN_C2_ST_G2V_IMM:
                iopc = INTRN_C2_ST_G2V_IMM;
                break;
	   case BUILT_IN_C2_MVGR_R2S:
                iopc = INTRN_C2_MVGR_R2S;
                break;
	   case BUILT_IN_C2_GSUMS:
                iopc = INTRN_C2_GSUMS;
                break;
           case BUILT_IN_C2_FORK:		
                iopc = INTRN_C2_FORK;
                break;		
           case BUILT_IN_C2_JOINT:		
                iopc = INTRN_C2_JOINT;
                break;
           case BUILT_IN_C2_CLZOB:
                iopc = INTRN_C2_CLZOB; 
                break;
           case BUILT_IN_C2_THCTRL:
                iopc = INTRN_C2_THCTRL; 
                break; 
#endif // TARG_SL


	      default:
		DevWarn ("Encountered BUILT_IN: %d at line %d\n",
			 DECL_FUNCTION_CODE (func), lineno);
		break;
            }
	  }
	  else
	    {
#ifdef TARG_X8664
	      wn = WFE_target_builtins (exp, &iopc, &intrinsic_op);
	      if (wn) break;
#else
	      Fail_FmtAssertion ("Target-specific builtins NYI");
#endif
	    }
	  }

          if (whirl_generated) {
            break;
          }

	  if (intrinsic_op) {
	    WN *ikids [16];
	    for (i = 0, list = TREE_OPERAND (exp, 1);
		 list;
		 i++, list = TREE_CHAIN (list)) {
              arg_wn     = WFE_Expand_Expr (TREE_VALUE (list));
	      arg_ty_idx = Get_TY(TREE_TYPE(TREE_VALUE(list)));
	      arg_mtype  = TY_mtype(arg_ty_idx);
              arg_wn     = WN_CreateParm (Mtype_comparison (arg_mtype), arg_wn,
					  arg_ty_idx, WN_PARM_BY_VALUE);
#ifdef TARG_SL
            if( iopc == INTRN_SBUF_OFFSET || iopc == INTRN_VBUF_OFFSET ||
                iopc == INTRN_VBUF_ABSOLUTE ) {
              Mark_LDA_Vbuf_Offset(ikids[0], iopc);
              Adjust_Vbuf_Array_Ofst(ikids[0]);
              wn = WN_kid0(ikids[0]); 
	     }
#endif 

	      ikids [i]  = arg_wn;
	    }
#if defined(TARG_SL)
            switch (ret_mtype) {
            case MTYPE_I1:
            case MTYPE_I2: ret_mtype = MTYPE_I4;   break;
            default: ;
            }
            wn = WN_Create_Intrinsic (OPR_INTRINSIC_OP, ret_mtype, MTYPE_V,
	 			      iopc, num_args, ikids);
	    //set deref flags for parameters if needed
	    WN_Set_Deref_If_Needed(wn);
#else 
	    wn = WN_Create_Intrinsic (OPR_INTRINSIC_OP, ret_mtype, MTYPE_V,
				      iopc, num_args, ikids);
#endif
#ifdef KEY
	    if (cvt_to != MTYPE_UNKNOWN) // bug 8251
              wn = WN_Cvt (ret_mtype, cvt_to, wn);
#endif
	    break;
	  }

	  if (iopc) {
            call_wn = WN_Create (OPR_INTRINSIC_CALL, ret_mtype, MTYPE_V, num_args);
	    WN_intrinsic (call_wn) = iopc;
	  }
	  else {
	    num_handlers = Current_Handler_Count();
            call_wn = WN_Create (OPR_CALL, ret_mtype, MTYPE_V,
                                 num_args + num_handlers);

            st = Get_ST (TREE_OPERAND (arg0, 0));
            WN_st_idx (call_wn) = ST_st_idx (st);
	  }
        }

        else {
          WN *wn_kid0, *wn_kid0_kid0;
          TY_IDX kid_ty_idx;
          UINT32 kid_field_id, kid_cur_fld;
          FLD_HANDLE kid_fld_handle;
	  num_args++;
	  num_handlers = Current_Handler_Count();
          call_wn = WN_Create (OPR_ICALL, ret_mtype, MTYPE_V,
			       num_args + num_handlers);
          wn_kid0 = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
          WN_kid(call_wn, num_args-1) = wn_kid0;
          WN_set_ty (call_wn, TY_pointed(Get_TY(TREE_TYPE(TREE_OPERAND(exp, 0)))));

          /* 
            Check if the indirect call is a call to a virtual function 
            First, get the TY of the address and the field ID 
            1) For IA64, virtual function call is 
                    LDID object 
                  LDID object                ILOAD vptr field 
                ILOAD vptr field    or     ADD offset 
              ICALL                      ICALL 
            2) For X86, virtual function call is 
                    LDID object 
                  ILOAD vptr field 
                ILOAD offset 
              ICALL 
          */ 
          kid_ty_idx = kid_field_id = 0; 
#ifdef TARG_IA64 
          switch (WN_operator(wn_kid0)) {
              case OPR_ILOAD :
                  // if offset == 0, ILOAD the vptr directly
                  kid_ty_idx = WN_ty(wn_kid0);
                  kid_field_id = WN_field_id(wn_kid0);
                  break;
              case OPR_ADD :
                  // if call the result by ADD, analysis the tree of ADD
                  wn_kid0_kid0 = WN_kid0(wn_kid0);
                  if (WN_operator_is(wn_kid0_kid0, OPR_ILOAD)) {
                      kid_ty_idx = WN_ty(wn_kid0_kid0);
                      kid_field_id = WN_field_id(wn_kid0_kid0);
                  }
                  break;
          }
#endif 
#ifdef TARG_X8664 
          if (WN_operator(wn_kid0) == OPR_ILOAD) { 
              WN *wn_kid0_kid0 = WN_kid0(wn_kid0); 
              if (WN_operator(wn_kid0_kid0) == OPR_ILOAD) { 
                  kid_ty_idx = WN_ty(wn_kid0_kid0); 
                  kid_field_id = WN_field_id(wn_kid0_kid0); 
              } 
          } 
#endif 

          if (kid_ty_idx > 0 && kid_field_id > 0) {
              // If the TY and the field ID of the address are found,
              // then check if the field of the TY is a virtual pointer
              kid_cur_fld = 0;
              kid_fld_handle = FLD_get_to_field(kid_ty_idx, kid_field_id, kid_cur_fld);
              if (!strncmp (&Str_Table[FLD_name_idx(kid_fld_handle)], "_vptr.", 6))
                  WN_Set_Call_Is_Virtual(call_wn);
          }	
        }

	WN_Set_Linenum (call_wn, Get_Srcpos());
	WN_Set_Call_Default_Flags (call_wn);

        if (st) {
          tree func = TREE_OPERAND (arg0, 0);
          if (DECL_INLINE (func)) {
            wfe_invoke_inliner = TRUE;
          }
          // check to see whehter it is non-placement new operator 
          if (num_args == 1 && 
              (DECL_NAME(func) == ansi_opname(NEW_EXPR) ||
               DECL_NAME(func) == ansi_opname(VEC_NEW_EXPR))) {
            Set_PU_has_attr_malloc (Pu_Table[ST_pu(st)]);
          } else if (DECL_IS_MALLOC (func)) {
            Set_PU_has_attr_malloc (Pu_Table[ST_pu(st)]);
          } else if (DECL_IS_PURE(func)) {
            Set_PU_has_attr_pure (Pu_Table[ST_pu(st)]);
          } else if (TREE_READONLY(func)) {
            Set_PU_is_pure (Pu_Table[ST_pu(st)]);
          }
        }

        i = 0;
#ifdef KEY
	// If the object must be returned through memory, create the fake first
	// param to pass the address of the return area.  Here we decide if an
	// object must be returned via memory based on high-level language
	// requirements, such as if the return type has a copy constructor.
	// Later on, the back-end will make the same decision but based on the
	// ABI.
	if (return_in_mem) {
	  FmtAssert (target_wn != NULL,
		     ("WFE_Expand_Expr: write target is NULL"));
          WN *arg_wn = WN_CreateParm (Pointer_Mtype, target_wn,
				      Make_Pointer_Type(ty_idx, FALSE),
				      WN_PARM_BY_VALUE);
          WN_kid (call_wn, i++) = arg_wn;
	  if (WN_operator(target_wn) == OPR_LDA) {
	    ST *st = WN_st(target_wn);
	    Set_ST_addr_passed(*st);
	  }
	}
#endif
	for (list = TREE_OPERAND (exp, 1);
	     list;
	     list = TREE_CHAIN (list)) {
	  if (i == 0 && is_aggr_init_via_ctor) {
	    ST * st = Get_ST(TREE_VALUE(list));
	    arg_wn = WN_Lda(Pointer_Mtype, ST_ofst(st), st);
	    arg_ty_idx = Get_TY(
			   build_pointer_type(TREE_TYPE(TREE_VALUE(list))));
	  }
	  else {
#ifdef KEY
	    if (TREE_CODE (TREE_VALUE (list)) == ADDR_EXPR &&
	    	TREE_CODE (TREE_OPERAND (TREE_VALUE (list), 0)) == TARGET_EXPR)
	    {
		tree targ = TREE_OPERAND (TREE_VALUE (list), 0);
            	WN *targ_wn = WFE_Expand_Expr(targ);
            	arg_wn     = WN_Lda(Pointer_Mtype, 0, WN_st(targ_wn), 0);
	    	arg_ty_idx = Make_Pointer_Type(Get_TY(TREE_TYPE(targ)));
	    }
	    else
	    {
#endif // KEY
            arg_wn     = WFE_Expand_Expr (TREE_VALUE (list));
	    arg_ty_idx = Get_TY(TREE_TYPE(TREE_VALUE(list)));
#ifdef KEY
	    }
#endif // KEY
#ifdef KEY // bug 11585
	    if (WN_operator(arg_wn) == OPR_LDA) {
	      ST *st = WN_st(arg_wn);
	      Set_ST_addr_passed(*st);
	      Set_ST_addr_saved(*st);
	    }
#endif // KEY
	  }

	  arg_mtype  = TY_mtype(arg_ty_idx);
          arg_wn = WN_CreateParm (Mtype_comparison (arg_mtype), arg_wn,
		    		  arg_ty_idx, WN_PARM_BY_VALUE);
#if defined(TARG_SL)
/*   need adjust this value 
 *        ||
 *        ||
 *       \\//
 *        \/
 *  U4LDA 16 <1,31,array> T<64,anon_ptr.,4>
 * U4PARM 2 T<55,anon_ptr.,4> #  by_value
*/
         arg_wn = Adjust_Vbuf_Array_Ofst(arg_wn);
#endif // TARG_SL
          WN_kid (call_wn, i++) = arg_wn;
        }

#ifdef ADD_HANDLER_INFO
	if (num_handlers) 
	  Add_Handler_Info (call_wn, i, num_handlers);
#endif

#ifdef KEY
	if (key_exceptions && !TREE_NOTHROW(exp) && 
// Call terminate() "when the destruction of an object during stack 
// unwinding (except.ctor) exits using an exception" [except.terminate]
// So we don't want to form a region in such cases.
//
// NOTE: It need not be a destructor call, e.g. if we inline the destructor,
// all functions inside cannot throw. We assume that it is cleanup code means
// it has to be a destructor, be its call or its body.
	    !(in_cleanup) && 
// If this expr is wrapped in a MUST_NOT_THROW_EXPR, then we must not
// insert this call in an eh region. Example is bug 10061. The standard
// (Section 15.5.1) requires calling terminate()
//   when the exception handling mechanism, after completing evaluation of
//   the expression to be thrown but before the exception is caught
//   (except.throw), calls a user function that exits via an uncaught
//   exception
// See build_throw() for GNU's handling of this situation.
	    !must_not_throw)
	{
	    if (!inside_eh_region)
	    { // check that we are not already in a region
            	WN * region_body = WN_CreateBlock();
		inside_eh_region = true;
            	WFE_Stmt_Push (region_body, wfe_stmk_call_region_body, Get_Srcpos());
	    }
	} else if (key_exceptions && inside_eh_region && opt_regions)
	{
	    // The above conditions dictate that this call MUST not be inside
	    // a region. So close the region.
	    // TODO: Is this only for opt_regions or in general?
	    if (Check_For_Call_Region ())
	    	Did_Not_Terminate_Region = FALSE;
	}
#endif // KEY

#if defined(TARG_SL)
/* automatic inserting intrinsic_vbuf_offset or intrinsic_sbuf_offset 
 * for lwc2 & swc2
 */
    	  if(May_Include_Vbuf_Offset(iopc, call_wn))
          {
     /*    I4INTCONST
      *   I4PARM
      * INTRINSIC_CALL
      * 
      * The 4th operand is scalar mode, if scalar==1, compiler 
      * insert intrinsic_sbuf_offset, otherwise insert 
      * intrinsic_vbuf_offset;
      */
           INTRINSIC id;
           WN* intrn_ofst_wn;
           UINT64 nth_child;
          if(iopc == INTRN_C2_LD_V2G || iopc == INTRN_C2_LD_G)   {
              nth_child = 0;
             id = ((iopc == INTRN_C2_LD_V2G) ? INTRN_VBUF_OFFSET : INTRN_SBUF_OFFSET);
          }
	   else {
                id = ((iopc == INTRN_C2_ST_G) ? INTRN_SBUF_OFFSET : INTRN_VBUF_OFFSET);
		  nth_child = 1;
	    }
           intrn_ofst_wn = WN_Create (OPR_INTRINSIC_OP, MTYPE_U4, MTYPE_V, 1);
	    WN_intrinsic (intrn_ofst_wn) = id;
           WN_kid0(intrn_ofst_wn) = WN_kid(call_wn, nth_child);
           Mark_LDA_Vbuf_Offset(WN_kid0(intrn_ofst_wn), id);
           TY_IDX ty_idx = Get_TY(ptr_type_node);
           WN* parm_wn = WN_CreateParm (Mtype_comparison (MTYPE_I4), intrn_ofst_wn,
				    ty_idx, WN_PARM_BY_VALUE); 
           WN_kid(call_wn, nth_child) = parm_wn;
		   
         }
	//set parameter dereference for generated wn
	WN_Set_Deref_If_Needed(call_wn);
#endif // TARG_SL


        if (ret_mtype == MTYPE_V
#ifdef KEY
	   // If the result is already put into the preferred symbol, then emit
	   // the call and we're done.
	   || return_in_mem
#endif
	   ) {
	  WFE_Stmt_Append (call_wn, Get_Srcpos());
#ifdef TARG_SL
          // c3_store, c3_fftst
          if ((WN_Need_Append_Intrinsic(call_wn))) {
            WFE_Stmt_Append_Extend_Intrinsic(call_wn, WN_kid0(WN_kid0(call_wn)), Get_Srcpos());
          }
#endif
        }

	else {
          wn0 = WN_CreateBlock ();
          WN_INSERT_BlockLast (wn0, call_wn);
#ifdef KEY
          // Preserve type information if available, in preference to
          // (void *).
          if (nop_ty_idx && TY_kind(ty_idx) == KIND_POINTER &&
              TY_mtype(TY_pointed(ty_idx)) == MTYPE_V) /* pointer to void */
            ty_idx = nop_ty_idx;
#endif
	  wn1 = WN_Ldid (ret_mtype, -1, Return_Val_Preg, ty_idx);

	  if (ret_mtype == MTYPE_M) { // copy the -1 preg to a temp area

	    TY_IDX ret_ty_idx = ty_idx;
#ifndef KEY
// bug 3735: the compiler cannot arbitrarily change the alignment of
// individual structures
	    if (Aggregate_Alignment > 0 &&
		Aggregate_Alignment > TY_align (ret_ty_idx))
	      Set_TY_align (ret_ty_idx, Aggregate_Alignment);
#endif // !KEY
            if (TY_align (ret_ty_idx) < MTYPE_align_best(Spill_Int_Mtype))
              Set_TY_align (ret_ty_idx, MTYPE_align_best(Spill_Int_Mtype));
	    ST *ret_st = Gen_Temp_Symbol(ret_ty_idx, 
		  st ? Index_To_Str(Save_Str2(".Mreturn.",
					      ST_name(ST_st_idx(st))))
		     : ".Mreturn.");
#ifdef KEY
	    WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, ret_st);
#endif

	    if (!return_in_mem) {
	      wn1 = WN_Stid (ret_mtype, 0, ret_st, ty_idx, wn1);
	      WN_INSERT_BlockLast (wn0, wn1);
	    }

	    // ritual for determining the right mtypes to be used in the LDID
            UINT xtra_BE_ofst = 0;  // only needed for big-endian target
            desc_ty_idx = component_ty_idx;
            if (desc_ty_idx == 0)
              desc_ty_idx = Get_TY (TREE_TYPE(exp));
              
            if (! MTYPE_is_integral(TY_mtype(desc_ty_idx)))
              ty_idx = desc_ty_idx;
            else { 
              ty_idx = nop_ty_idx;
              if (ty_idx == 0)
                ty_idx = desc_ty_idx;
            }

	    if (! is_bit_field) {
              if (TY_size(desc_ty_idx) > TY_size(ty_idx)) {
                if (Target_Byte_Sex == BIG_ENDIAN)
                  xtra_BE_ofst = TY_size(desc_ty_idx) - TY_size(ty_idx);
                desc_ty_idx = ty_idx;
	      }
            }
	    else {
	      if (TY_size(desc_ty_idx) > TY_size(ty_idx))
		ty_idx = desc_ty_idx;
	    }

	    TYPE_ID rtype = Widen_Mtype(TY_mtype(ty_idx));
	    TYPE_ID desc = TY_mtype(desc_ty_idx);
	    if (MTYPE_is_integral(desc)) {
	      if (MTYPE_signed(rtype) != MTYPE_signed(desc)) {
		if (MTYPE_size_min(rtype) > MTYPE_size_min(desc) ||
		    is_bit_field)
		  rtype = Mtype_TransferSign(desc, rtype);
		else desc = Mtype_TransferSign(rtype, desc);
	      }
	    }

            Is_True(! is_bit_field || field_id <= MAX_FIELD_ID,
                    ("WFE_Expand_Expr: field id for bit-field exceeds limit"));
 
	    wn1 = WN_CreateLdid(OPR_LDID, rtype,
			        is_bit_field ? MTYPE_BS : desc,
			        ST_ofst(ret_st)+component_offset+xtra_BE_ofst, 
				ret_st,
				(field_id != 0 && component_ty_idx != 0) ?
				Get_TY (TREE_TYPE(exp)) : ty_idx,
				field_id);
	  }

          wn  = WN_CreateComma (OPR_COMMA, WN_rtype (wn1), MTYPE_V,
                                wn0, wn1);
        }
      }
      break;

    case COMPOUND_EXPR:
      {
#ifdef KEY
	// If we are supposed to put the result in target_wn, then give the
	// result VAR_DECL the same ST as target_wn.
	tree opnd1 = TREE_OPERAND(exp, 1);
	if (TREE_CODE(opnd1) == VAR_DECL &&
	    target_wn != NULL) {
	  ST *st = DECL_ST(opnd1);
	  if (st == NULL) {
	    // Don't think we would see a LDID target_wn.
	    FmtAssert(WN_operator(target_wn) == OPR_LDA,
		      ("WFE_Expand_Expr: target_wn not LDA"));
	    set_DECL_ST(opnd1, WN_st(target_wn));
	  } else {
	    FmtAssert(st == WN_st(target_wn),
	       ("WFE_Expand_Expr: conflicting ST in COMPOUND_EXPR's VAR_DECL"));
	  }
	}
#endif
        wn = WFE_Expand_Expr (TREE_OPERAND (exp, 0), FALSE);
        if (wn && WN_has_side_effects(wn)) {
          wn = WN_CreateEval (wn);
          WFE_Stmt_Append (wn, Get_Srcpos ());
        }
        wn = WFE_Expand_Expr (TREE_OPERAND (exp, 1), need_result);
	// bug fix for OSP_161
	if (opt_regions && Did_Not_Terminate_Region == TRUE)
	{
	  Check_For_Call_Region ();
	  Did_Not_Terminate_Region = FALSE;
	}  
      }
      break;

    case NON_LVALUE_EXPR:
      {
#ifdef KEY
	// Pass field_id for bug 10339.
        wn = WFE_Expand_Expr (TREE_OPERAND (exp, 0), need_result, nop_ty_idx,
			      component_ty_idx, component_offset, field_id);
#else
        wn = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
#endif
      }
      break;

    case SAVE_EXPR:
      {
	DevWarn ("Encountered SAVE_EXPR at line %d", lineno);
        wn = WFE_Save_Expr (exp, need_result, nop_ty_idx,
			    component_ty_idx, component_offset, field_id);
      }
      break;

    case ERROR_MARK:
      // This is not necessarily an error:  return a constant 0.
      wn = WN_Intconst(MTYPE_I4, 0);
      break;

    case LOOP_EXPR:
      {
        DevWarn ("Encountered LOOP_EXPR at line %d\n", lineno);
        LABEL_IDX saved_loop_expr_exit_label = loop_expr_exit_label;
        loop_expr_exit_label = 0;
        tree body = LOOP_EXPR_BODY(exp);
        WN *loop_test = WN_Intconst (Boolean_type, 1);
        WN *loop_body = WN_CreateBlock ();
        if (body) {
          WFE_Stmt_Push (loop_body, wfe_stmk_while_body, Get_Srcpos());
          wn = WFE_Expand_Expr (body);
          WFE_Stmt_Pop (wfe_stmk_while_body);
        }
        WN *loop_stmt = WN_CreateWhileDo (loop_test, loop_body);
        WFE_Stmt_Append (loop_stmt, Get_Srcpos());
        if (loop_expr_exit_label)
          WFE_Stmt_Append (WN_CreateLabel ((ST_IDX) 0, loop_expr_exit_label, 0, NULL),
                           Get_Srcpos ());
        loop_expr_exit_label = saved_loop_expr_exit_label;
      }
      break;

    case EXIT_EXPR:
      {
        DevWarn ("Encountered EXIT_EXPR at line %d\n", lineno);
	WN *test = WFE_Expand_Expr (TREE_OPERAND(exp, 0));
        New_LABEL (CURRENT_SYMTAB, loop_expr_exit_label);
        WN *stmt = WN_CreateTruebr (loop_expr_exit_label, test);
        WFE_Stmt_Append (stmt, Get_Srcpos ());
      }
      break;

    case VA_ARG_EXPR:
      {
#ifdef TARG_X8664
	if( TARGET_64BIT ){
	  tree kid0 = TREE_OPERAND(exp, 0);
	  WN *ap_wn;
	  ap_wn = WFE_Expand_Expr(kid0);
	  if (WN_rtype(ap_wn) == MTYPE_M) {
	    if (OPCODE_is_leaf(WN_opcode(ap_wn)))
	      ap_wn = WN_Lda(Pointer_Mtype, WN_offset(ap_wn), WN_st(ap_wn), 0);
	    else {
	      Is_True(OPCODE_is_load(WN_opcode(ap_wn)),
		      ("WFE_Expand_Expr: unexpected VA_ARG_EXPR argument"));
	      if ( WN_offset(ap_wn) == 0 )
		ap_wn = WN_kid0(ap_wn);
	      else
		ap_wn = WN_Add(Pointer_Mtype, WN_kid0(ap_wn), WN_Intconst(Pointer_Mtype, WN_offset(ap_wn)));
	    }
	  }
	  TY_IDX ty_idx = Get_TY (TREE_TYPE(exp));
	  TYPE_ID mtype = Fix_TY_mtype(ty_idx);

	  if (mtype != MTYPE_FQ && mtype != MTYPE_M && !MTYPE_is_complex(mtype)) {
	    wn = WFE_x8664_va_arg(ap_wn, MTYPE_float(mtype), ty_idx, FALSE);
	    wn = WN_CreateIload(OPR_ILOAD, Widen_Mtype (mtype), mtype, 0,
				ty_idx, Make_Pointer_Type(ty_idx), wn);
	  }
	  else if (mtype == MTYPE_C4) {
	    wn = WFE_x8664_va_arg(ap_wn, MTYPE_float(mtype), ty_idx, FALSE);
	    wn = WN_CreateIload(OPR_ILOAD, MTYPE_M, MTYPE_M, 0, ty_idx,
				Make_Pointer_Type(ty_idx), wn);
	  }
	  else {
	    enum X86_64_PARM_CLASS classes[MAX_CLASSES];
	    INT n = Classify_Aggregate(ty_idx, classes);
	    if (n == 0) { /* can only pass in memory */
	      /* increment overflow_arg_area pointer by 8 */
	      INT delta = ((TY_size(ty_idx) + 7) / 8) * 8;
	      wn0 = WN_Iload(Pointer_Mtype, 8, MTYPE_To_TY(Pointer_Mtype), 
			     WN_CopyNode(ap_wn));
	      wn1 = WN_Intconst(MTYPE_U8, delta);
	      wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
	      wn = WN_Istore(Pointer_Mtype, 8,
			     Make_Pointer_Type(MTYPE_To_TY(Pointer_Mtype)),
			     WN_CopyNode(ap_wn), wn);
	      WFE_Stmt_Append (wn, Get_Srcpos ());
	      /* load pointer to overflow_arg_area */
	      wn = WN_Iload(Pointer_Mtype, 8, MTYPE_To_TY(Pointer_Mtype),
			    WN_CopyNode(ap_wn));
	      /* adjust with the amount just incremented */
	      wn1 = WN_Intconst(MTYPE_I8, -delta);
	      wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
	    }
	    else if (n == 1) {
	      wn = WFE_x8664_va_arg(ap_wn, classes[0] == X86_64_SSE_CLASS,
				    ty_idx, FALSE);
	    }
	    else if (n > 1) { /* must be == 2 */
	      if (classes[0] == classes[1]) {
		if (classes[0] == X86_64_INTEGER_CLASS)
		  wn = WFE_x8664_va_arg(ap_wn, classes[0] == X86_64_SSE_CLASS, 
					ty_idx, TRUE/*twice*/);
		else wn = WFE_x8664_va_arg_2_float(ap_wn, ty_idx);
	      }
	      else {
		wn = WFE_x8664_va_arg_2_mixed(ap_wn, 
					      classes[0] == X86_64_SSE_CLASS,
					      classes[1] == X86_64_SSE_CLASS, ty_idx);
	      }
	    }
	    
	    if( mtype == MTYPE_FQ )
	      wn = WN_CreateIload(OPR_ILOAD, Widen_Mtype (mtype), mtype, 0,
				  ty_idx, Make_Pointer_Type(ty_idx), wn);
	    else
	      wn = WN_CreateIload(OPR_ILOAD, MTYPE_M, MTYPE_M, 0, ty_idx, 
				  Make_Pointer_Type(ty_idx), wn);
	  }

	  break;
	} // end of TARGET_64BIT
#endif
        // code swiped from builtins.c (std_expand_builtin_va_arg)
	tree type = TREE_TYPE (exp);
	TY_IDX ty_idx = Get_TY (type);
	TYPE_ID mtype = TY_mtype (ty_idx);
	INT64 ty_align = TYPE_ALIGN (type) / BITSPERBYTE;

	INT64 align = PARM_BOUNDARY / BITS_PER_UNIT;
	INT64 ty_size = ((int_size_in_bytes (type) + align - 1) / align) * align;
	ty_align = ((ty_align + align - 1) / align) * align;

	/* Get AP.  */
#ifdef PATHSCALE_MERGE
	WN        *ap_load   = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
        TY_IDX     ap_ty_idx = Get_TY (TREE_TYPE (TREE_OPERAND (exp, 0)));
        WN        *ap_addr;
	TY_IDX	   ap_addr_ty;
        ST        *ap_st;
        WN_OFFSET  ap_offset;
        UINT32     ap_field_id = 0;

        if (WN_operator(ap_load) == OPR_LDID) {
	  ap_st     = WN_st (ap_load);
          ap_offset = WN_offset (ap_load);
        }
        else
        if (WN_operator(ap_load) == OPR_ILOAD) {
          ap_st     = NULL;
          ap_offset = WN_offset (ap_load);
          ap_field_id = WN_field_id(ap_load);
          ap_addr   = WN_COPY_Tree (WN_kid0 (ap_load));
	  ap_addr_ty = WN_load_addr_ty(ap_load);
          if (WN_has_side_effects (ap_addr))
            Fail_FmtAssertion ("VA_ARG_EXPR: ap address has side effects");
        }
        else
          Fail_FmtAssertion ("VA_ARG_EXPR: unknown operator for ap");
#else
	WN *ap = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
	if (WN_operator(ap) == OPR_ILOAD) {
	  STR_IDX str = Save_Str("__ap");
	  ST *st = New_ST(CURRENT_SYMTAB);
	  ST_Init(st, str, CLASS_VAR, SCLASS_AUTO, EXPORT_LOCAL,
		  MTYPE_To_TY(Pointer_Mtype));
	  Set_ST_is_temp_var(st);
	  WN *wn = WN_Stid(Pointer_Mtype, 0, st, ST_type(st), ap);
	  WFE_Stmt_Append (wn, Get_Srcpos ());

	  ap = WN_Ldid(Pointer_Mtype, 0, st, ST_type(st));
	}
	WN *ap_load = WN_COPY_Tree(ap);
	TY_IDX ap_ty_idx = Get_TY (TREE_TYPE (TREE_OPERAND (exp, 0)));
	st = WN_st (ap);
#endif

	wn = WN_COPY_Tree(ap_load);

#ifdef TARG_IA64
	/* Align AP for the next argument. */
	if (ty_align > align) {
		wn = WN_Binary (OPR_ADD, Pointer_Mtype, wn,
			WN_Intconst (Pointer_Mtype, ty_align - 1));
		wn = WN_Binary (OPR_BAND, Pointer_Mtype, wn,
			WN_Intconst (Pointer_Mtype, -ty_align));
	}
#endif
	/* Compute new value for AP.  */
	if (Target_Byte_Sex == BIG_ENDIAN) {
	  wn = WN_Binary (OPR_ADD, Pointer_Mtype, wn,
			  WN_Intconst (Pointer_Mtype, 3));
	  wn = WN_Binary (OPR_BAND, Pointer_Mtype, wn,
			  WN_Intconst (Pointer_Mtype, -8));
#ifdef TARG_X8664
	  INT64 adj;
	  adj = TREE_INT_CST_LOW (TYPE_SIZE (type)) / BITS_PER_UNIT;
	  if (ty_size > align)
	    adj = ty_size;
	  wn = WN_Binary (OPR_ADD, Pointer_Mtype, wn,
			  WN_Intconst (Pointer_Mtype, ty_size));
#endif
  } else
#if defined(TARG_SL)
    {
      wn = WN_Binary (OPR_ADD, Pointer_Mtype, WN_COPY_Tree (ap_load),
 		WN_Intconst (Pointer_Mtype, ty_size));
    }
#else
      wn = WN_Binary (OPR_ADD, Pointer_Mtype, wn,
              WN_Intconst (Pointer_Mtype, ty_size));
#endif

#ifdef TARG_X8664 // bug 12118: pad since under -m32, vector types are 8-byte aligned
	if (MTYPE_is_vector(mtype) && ! TARGET_64BIT) {
	  wn = WN_Add(Pointer_Mtype, wn, WN_Intconst(Pointer_Mtype, 7));
	  wn = WN_Div(Pointer_Mtype, wn, WN_Intconst(Pointer_Mtype, 8));
	  wn = WN_Mpy(Pointer_Mtype, wn, WN_Intconst(Pointer_Mtype, 8));
	}
#endif
#if defined(TARG_MIPS) || defined(TARG_LOONGSON) // bug 12945: pad since long doubles are 16-byte aligned
	if (mtype == MTYPE_FQ) {
	  wn = WN_Add(Pointer_Mtype, wn, WN_Intconst(Pointer_Mtype, 15));
	  wn = WN_Div(Pointer_Mtype, wn, WN_Intconst(Pointer_Mtype, 16));
	  wn = WN_Mpy(Pointer_Mtype, wn, WN_Intconst(Pointer_Mtype, 16));
	}
#endif

#ifdef PATHSCALE_MERGE
        if (ap_st)
	  wn = WN_Stid (Pointer_Mtype, ap_offset, ap_st, ap_ty_idx, wn);
        else {
          wn = WN_CreateIstore (OPR_ISTORE, MTYPE_V, Pointer_Mtype, ap_offset,
                                ap_addr_ty, wn, ap_addr, ap_field_id);
        }
#else
	wn = WN_Stid (Pointer_Mtype, 0, st, ST_type (st), wn);
#endif

        WFE_Stmt_Append (wn, Get_Srcpos ());

#ifdef KEY
	if (Target_Byte_Sex != Host_Byte_Sex)
          wn = WN_CreateIload (OPR_ILOAD, Widen_Mtype (mtype), mtype, 
			  ((MTYPE_size_min(mtype)==32)?4:0)-ty_size, 
                          ty_idx, Make_Pointer_Type (ty_idx, FALSE),
 			  ap_load);
        else
#endif
#ifdef PATHSCALE_MERGE
          wn = WN_CreateIload (OPR_ILOAD, Widen_Mtype (mtype), mtype, -ty_size,
			       ty_idx, Make_Pointer_Type (ty_idx, FALSE),
			       ap_load);
#else
          wn = WN_CreateIload (OPR_ILOAD, Widen_Mtype (mtype), mtype, -ty_size,
                              ty_idx, Make_Pointer_Type (ty_idx, FALSE),
                              WN_Ldid (Pointer_Mtype, 0, st, ST_type (st)));
#endif
      }
      break;

#ifdef KEY
    case EXPR_WITH_FILE_LOCATION:
      wn = WFE_Expand_Expr (EXPR_WFL_NODE (exp), need_result, nop_ty_idx,
			    component_ty_idx, component_offset, field_id,
			    is_bit_field, is_aggr_init_via_ctor,
			    target_wn);
      break;

    case EXC_PTR_EXPR:
    {
      if (key_exceptions)
      {
	ST_IDX exc_ptr_st = TCON_uval (INITV_tc_val (INITO_val (PU_misc_info (Get_Current_PU()))));
      	wn = WN_Ldid (Pointer_Mtype, 0, exc_ptr_st, Get_TY(TREE_TYPE(exp)));
      }
      else
      {
	// bug 12500: dummy exc_ptr_expr
      	if (!Dummy_Exc_Ptr_Expr)
          Dummy_Exc_Ptr_Expr = Gen_Temp_Symbol (Get_TY(TREE_TYPE(exp)),
                                                "__dummy_exc_ptr");
      	wn = WN_Ldid (Pointer_Mtype, 0, Dummy_Exc_Ptr_Expr, Get_TY(TREE_TYPE(exp)));
      }
    }
      break;

    case CLEANUP_STMT:
      DevWarn ("CLEANUP_STMT not implemented: at line %d\n", lineno);
      // TODO:  Return a dummy constant 0 for now.
      wn = WN_Intconst(MTYPE_I4, 0);
      break;

    case MUST_NOT_THROW_EXPR:
      // Call terminate if this expr throws
      must_not_throw = TRUE;
      wn = WFE_Expand_Expr (TREE_OPERAND (exp,0));
      must_not_throw = FALSE;
      break;

   case VECTOR_CST:
     {
       ST * init_st = Gen_Temp_Symbol (Get_TY(TREE_TYPE(exp)), "__vec_cst");
#ifdef NEW_INITIALIZER
       WN* target = WN_Lda(Pointer_Mtype, 0, init_st, 0);
       Traverse_Aggregate_Vector_Const (target, exp, 0, 0);
#else
       Traverse_Aggregate_Vector_Const (init_st, exp, 0, 0);
#endif
       TY_IDX ty = ST_type (init_st);
       TYPE_ID mtype = TY_mtype (ty);
       wn = WN_CreateLdid (OPR_LDID, mtype, mtype, 0, init_st, ty, 0);
       break;
     }
#endif /* KEY */

    default:
      {
        Fail_FmtAssertion ("WFE_Expand_Expr: not implemented %s",
                           Operator_From_Tree [code].name);
      }
      break;
    }

#ifdef WFE_DEBUG
  if (wn)
    fdump_tree (stderr, wn);

  fprintf (stderr, // "{("
           ")} WFE_Expand_Expr: %s\n", Operator_From_Tree [code].name);
#endif /* WFE_DEBUG */

  if (need_result)
    FmtAssert (wn != 0 || code == CALL_EXPR || code == BIND_EXPR ||
               code == COMPOUND_STMT ||
               code == STMT_EXPR     ||
               code == EXPR_STMT     ||	// KEY
               code == COMPOUND_EXPR ||
               code == INDIRECT_REF  ||
               code == COMPONENT_REF ||
               code == LOOP_EXPR     ||
               code == NOP_EXPR      ||
               code == THROW_EXPR    ||
	       code == MUST_NOT_THROW_EXPR ||
	       code == EXPR_WITH_FILE_LOCATION	||	// KEY
               ((code == COND_EXPR) &&
	        (TY_mtype(ty_idx) == MTYPE_V || TY_mtype(ty_idx) == MTYPE_M)),
	       ("WFE_Expand_Expr: NULL WHIRL tree for %s",
		Operator_From_Tree [code].name));

  return wn;
}


#ifdef KEY
// Like WFE_One_Stmt but don't reuse label indexes already allocated so far.
// This is necessary because the cleanup represented by the EXP tree can be
// expanded multiple times, and each expansion needs its own set of labels.
void
WFE_One_Stmt_Cleanup (tree exp)
{
  LABEL_IDX idx = WFE_unusable_label_idx;
  INT32 save_expr_level = wfe_save_expr_level;

  // Don't reuse label indexes that are allocated up to this point.
  WFE_unusable_label_idx = WFE_last_label_idx;

  // Make the saved expr's, if any, unique to this cleanup.
  wfe_save_expr_level = ++wfe_last_save_expr_level;
  
  WFE_One_Stmt(exp);
  WFE_unusable_label_idx = idx;
  wfe_save_expr_level = save_expr_level;
}
#endif


void WFE_One_Stmt (tree exp, WN* target_wn)
{
  WN *wn;
#ifndef KEY
  wfe_save_expr_stack_last = -1; // to minimize searches
#endif
  wn = WFE_Expand_Expr_With_Sequence_Point (exp, MTYPE_V, target_wn);
  if (wn) {
    for (;;) {
      if (WN_operator (wn) == OPR_COMMA) {
	WN *crwn = wn;
	if (WN_operator (WN_kid1 (wn)) == OPR_LDID                 &&
	    WN_st (WN_kid1 (wn)) == Return_Val_Preg                &&
	    (WN_operator (WN_last (WN_kid0 (wn))) == OPR_CALL   ||
	     WN_operator (WN_last (WN_kid0 (wn))) == OPR_ICALL)) {
	  WN_set_rtype (WN_last (WN_kid0 (wn)), MTYPE_V);
	  WFE_Stmt_Append (WN_kid0 (wn), Get_Srcpos ());
	  WN_Delete (crwn);
	  break;
	}
	else {
	  WFE_Stmt_Append (WN_kid0 (wn), Get_Srcpos ());
	  wn = WN_kid1 (wn);
	  WN_Delete (crwn);
	}
      }
      else {
	if (WN_has_side_effects (wn)) {
	  wn = WN_CreateEval (wn);
	  WFE_Stmt_Append (wn, Get_Srcpos ());
	}
	break;
      }
    }
  }
}

void WFE_Null_Return (void)
{
  WN *wn = WN_CreateReturn ();
  WFE_Stmt_Append (wn, Get_Srcpos());
}

UINT64
Get_Integer_Value (tree exp)
{
	FmtAssert (TREE_CODE(exp) == INTEGER_CST, 
		("Get_Integer_Value unexpected tree"));
#ifdef _LP64
	return TREE_INT_CST_LOW (exp);
#else
	UINT64 h = TREE_INT_CST_HIGH (exp);
	UINT64 l = TREE_INT_CST_LOW (exp);
#ifndef KEY
	l = l << 32 >> 32;	// zero-out high 32 bits
	h = h << 32;
	return (h | l);
#else
	// In the new gcc-3.2.2 both TREE_INT_CST_HIGH and
	// TREE_INT_CST_LOW are 64-bits wide.
	return (l);
#endif /* KEY */
#endif /* _LP64 */
}

void
WFE_Expr_Init (void)
{
  INT i;
  for (i = 0; i < LAST_CPLUS_TREE_CODE; i++)
    FmtAssert (Operator_From_Tree [i].tree_code == i,
               ("Operator_From_Tree[%d] incorrect, value = %d (last is %d)",
                i, Operator_From_Tree [i].tree_code, LAST_CPLUS_TREE_CODE));
}

char *
WFE_Tree_Node_Name (tree op)
{
  return Operator_From_Tree [TREE_CODE (op)].name;
}

#ifdef KEY
// g++ uses a record to hold a ptr-to-member-function.  Return TRUE iff EXP is
// a CALL_EXPR that returns a ptr-to-member-function and the ABI requires that
// such a record be returned in memory.
//
// Invoke WFE_Expand_Ptr_To_Member_Func_Call_Expr to expand such calls.  The
// routine creates a temp record for the ptr-to-member-function and invokes
// WFE_Expand_Expr to expand the return value there.
static bool
WFE_Call_Returns_Ptr_To_Member_Func (tree exp)
{
  TY_IDX exp_ty_idx = Get_TY(TREE_TYPE(exp));
  if (TREE_CODE(exp) == CALL_EXPR &&
      TYPE_PTRMEMFUNC_P(TREE_TYPE(exp)) &&
      TY_return_in_mem(exp_ty_idx)) {
    return TRUE;
  }
  return FALSE;
}

// See comment for WFE_Call_Returns_Ptr_To_Member_Func.
static WN*
WFE_Expand_Ptr_To_Member_Func_Call_Expr (tree exp, TY_IDX nop_ty_idx,
					 TYPE_ID rtype, TYPE_ID desc,
					 WN_OFFSET offset, UINT field_id)
{
  TY_IDX exp_ty_idx = Get_TY(TREE_TYPE(exp));
  WN *wn;
  ST *st = New_ST (CURRENT_SYMTAB);

  ST_Init(st, Save_Str("__ptr_to_mem_func"), CLASS_VAR, SCLASS_AUTO,
	  EXPORT_LOCAL, exp_ty_idx);
  WN *target_wn = WN_Lda(Pointer_Mtype, ST_ofst(st), st);
  WFE_Expand_Expr(exp, TRUE, nop_ty_idx, exp_ty_idx, 0, 0, FALSE, FALSE,
		  target_wn);
  wn = WN_CreateLdid(OPR_LDID, rtype, desc, ST_ofst(st) + offset,
		     st, exp_ty_idx, field_id);
  return wn;
}
#endif
