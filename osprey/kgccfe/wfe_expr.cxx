/*
 * Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/* 
   Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
   File modified June 20, 2003 by PathScale, Inc. to update Open64 C/C++ 
   front-ends to GNU 3.2.2 release.
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


#if defined(BUILD_OS_DARWIN)
#include <limits.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <values.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include "defs.h"
#include "glob.h"
#include "config.h"
#if defined(TARG_X8664) || defined(TARG_LOONGSON)
#include "config_opt.h"
#endif
#include "wn.h"
#include "wn_util.h"
#include "targ_sim.h"
#include "const.h"
#include "c_int_model.h"

// #include "gnu/MIPS/config.h"
#include "gnu_config.h"
#ifdef KEY 
// To get HW_WIDE_INT ifor flags.h */
#include "gnu/hwint.h"
#endif /* KEY */
#include "gnu/flags.h"
#include "gnu/system.h"

#ifdef KEY
extern "C" {
#include "gnu/machmode.h"
}
#else
#include "gnu/machmode.h"
#endif // KEY

extern "C" {
#include "gnu/tree.h"
extern void warning (char*,...);	// from gnu
extern tree c_strlen_exported (tree);
#include "gnu/function.h"
}

#ifdef GPLUSPLUS_FE
#include "gnu/cp/cp-tree.h"
#endif /* GPLUSPLUS_FE */

#include "ir_reader.h"
#include "tree_symtab.h"
#include "wfe_misc.h"
#ifdef KEY // get REAL_VALUE_TYPE
extern "C" {
#include "real.h"
}
#endif // KEY
#include "wfe_decl.h"
#include "wfe_expr.h"
#include "wfe_stmt.h"
#include <cmplrs/rcodes.h>
#include "config_asm.h"

#ifdef KEY
/* Codes of tree nodes */

#define DEFTREECODE(SYM, STRING, TYPE, NARGS)   SYM,

enum c_tree_code {
  C_DUMMY_TREE_CODE = LAST_AND_UNUSED_TREE_CODE,
#include "gnu/c-common.def"
  LAST_C_TREE_CODE
};
#undef DEFTREECODE
#endif /* KEY */
#include "tree_cmp.h"

// #define WFE_DEBUG

extern void dump_ty_idx (TY_IDX);
#ifdef KEY
extern void WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_ID, ST *);
#endif // KEY

extern "C" int get_expr_stmts_for_value (void);

extern PREG_NUM asm_neg_preg;

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
#ifdef KEY
  STRING_CST,              "string_cst",              'c', 4,  OPERATOR_UNKNOWN,
#else
  STRING_CST,              "string_cst",              'c', 3,  OPERATOR_UNKNOWN,
#endif // KEY
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
#ifdef KEY
  CALL_EXPR,               "call_expr",               'e', 2,  OPERATOR_UNKNOWN,
#else
  CALL_EXPR,               "call_expr",               'e', 3,  OPERATOR_UNKNOWN,
#endif // KEY
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
#ifdef KEY
  SWITCH_STMT,             "switch_stmt",             'e', 3,  OPERATOR_UNKNOWN,
#else
  SWITCH_STMT,             "switch_stmt",             'e', 2,  OPERATOR_UNKNOWN,
#endif // KEY
  GOTO_STMT,               "goto_stmt",               'e', 1,  OPERATOR_UNKNOWN,
  LABEL_STMT,              "label_stmt",              'e', 1,  OPERATOR_UNKNOWN,
  ASM_STMT,                "asm_stmt",                'e', 5,  OPERATOR_UNKNOWN,
  SCOPE_STMT,              "scope_stmt",              'e', 1,  OPERATOR_UNKNOWN,
  FILE_STMT,               "file_stmt",               'e', 1,  OPERATOR_UNKNOWN,
#ifdef KEY
  CASE_LABEL,              "case_label",              'e', 3,  OPERATOR_UNKNOWN,
#else
  CASE_LABEL,              "case_label",              'e', 2,  OPERATOR_UNKNOWN,
#endif // KEY
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
  DEFAULT_ARG,             "default_arg",             'c', 2,  OPERATOR_UNKNOWN,
  TEMPLATE_ID_EXPR,        "template_id_expr",        'e', 2,  OPERATOR_UNKNOWN,
  CPLUS_BINDING,           "binding",                 'x', 2,  OPERATOR_UNKNOWN,
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
  SUBOBJECT,               "subobject",               'e', 1,  OPERATOR_UNKNOWN,
  CTOR_STMT,               "ctor_stmt",               'e', 0,  OPERATOR_UNKNOWN,
  CTOR_INITIALIZER,        "ctor_initializer",        'e', 2,  OPERATOR_UNKNOWN,
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
 P0,   // no extend
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
  // C3 
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

BOOL WN_Need_Append_Intrinsic(WN *rhs) {
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
void WFE_Stmt_Append_Extend_Intrinsic(WN *wn, WN *master_variable, SRCPOS src) {
   WN *kid1s[5];
   WN *op1;
   int aux_kid = -1;  // parameter numbers of slave intrinsic op
   int extend_num = -1;
   int pos[5]= {-1, -1, -1, -1, -1};
   WN *tmp_wn; 
   INTRN_ATTR_EXTEND *p;

   if (WN_kid0(master_variable) && WN_operator(master_variable) == OPR_CVT) {
      master_variable = WN_kid0(master_variable);
   }
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
  
   if (WN_operator(master_variable) == OPR_INTCONST) {
     // c3.st/c3.fftst first variable could be immediate 
     Is_True(WN_opcode(master_variable) ==  OPC_I4INTCONST,("should be 32-bit immediate"));
     master_variable = WN_CreateParm(MTYPE_I4, master_variable, Be_Type_Tbl(MTYPE_I4), WN_PARM_BY_VALUE);
   } else {  
     TY_IDX  ti2 = WN_ty(master_variable);
     TYPE_ID tm2 = WN_rtype(master_variable);
     master_variable = WN_CreateParm (Mtype_comparison (tm2), master_variable,
                                      ti2, WN_PARM_BY_VALUE);
   }
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
     TYPE_ID tm1 = WN_rtype(op1);
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
// The words in 'buf' are in target order. Convert them to host order
// in place. 'buf' is a two word array.
void
WFE_Convert_To_Host_Order (long *buf)
{
  if (!Same_Byte_Sex)
    {
      long t = buf[0];
      buf[0] = buf[1];
      buf[1] = t;
    }
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

TYPE_ID
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

typedef struct wfe_save_expr_t {
  tree  exp;
  ST   *st;
} WFE_SAVE_EXPR;

WFE_SAVE_EXPR *wfe_save_expr_stack      = NULL;
INT32          wfe_save_expr_stack_last = -1;
INT32          wfe_save_expr_stack_max  = 0;

static WN*
WFE_Save_Expr (tree save_exp)
{
  INT32     i;
  tree      exp     = TREE_OPERAND (save_exp, 0);
  TY_IDX    ty_idx  = Get_TY (TREE_TYPE (exp));
  TYPE_ID   mtype   = TY_mtype (ty_idx);
  ST       *st;
  WN       *wn;

  for (i = wfe_save_expr_stack_last; i >= 0; i--) {
#ifndef KEY
    if (wfe_save_expr_stack [i].exp == exp) {
#else
    if (wfe_save_expr_stack [i].exp == save_exp) {
#endif
      st = wfe_save_expr_stack [i].st;
      FmtAssert (st != 0,
                 ("WFE_Save_Expr: st not yet assigned"));
      wn   = WN_Ldid (mtype, 0, st, ty_idx);
      return wn;
    }
  }
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
#endif
  wfe_save_expr_stack [i].st  = 0;
  wn = WFE_Expand_Expr (exp);
  st = Gen_Temp_Symbol (ty_idx, "__save_expr");
#ifdef KEY
  WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, st);
#endif // KEY
  WFE_Set_ST_Addr_Saved (wn);
  wn = WN_Stid (mtype, 0, st, ty_idx, wn);
  WFE_Stmt_Append (wn, Get_Srcpos());
  wfe_save_expr_stack [i].st = st;
  wn = WN_Ldid (mtype, 0, st, ty_idx);
  return wn;
} /* WFE_Save_Expr */
        
static void
WFE_Unsave_Expr (tree save_exp)
{
  INT32     i;
  tree      exp     = TREE_OPERAND (save_exp, 0);
  TY_IDX    ty_idx  = Get_TY (TREE_TYPE (exp));
  TYPE_ID   mtype   = TY_mtype (ty_idx);
  ST       *st;
  WN       *wn;

  for (i = wfe_save_expr_stack_last; i >= 0; i--) {
#ifndef KEY
    if (wfe_save_expr_stack [i].exp == exp) {
#else
    if (wfe_save_expr_stack [i].exp == save_exp) {
#endif
      wfe_save_expr_stack [i].exp == NULL;
      wfe_save_expr_stack [i].st  == NULL;
      break;
    }
  }
} /* WFE_Unsave_Expr */

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
      Is_True (lhs != NULL && TREE_CODE(lhs) == VAR_DECL,
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
    else {
      wn = WN_Lda (Pointer_Mtype, ST_ofst(st)+component_offset, st, field_id);
    }
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
  }
  else if (code == COND_EXPR) {
    // OSP_7, COND_EXPR in ARRAY_REF
    // struct s { char c[1]; };
    // struct s a, b, c;
    // (i ? b : c).c[0];
    // ARRAY_REF
    //     |---> COND_EXPR
    WN *wn1, *wn2;
    wn = WFE_Expand_Expr (TREE_OPERAND(exp, 0));
    wn1 = WFE_Array_Expr (TREE_OPERAND(exp, 1), ty_idx, component_ty_idx,
		          component_offset, field_id);
    wn2 = WFE_Array_Expr (TREE_OPERAND(exp, 2), ty_idx, component_ty_idx,
		          component_offset, field_id);
    Set_PU_has_very_high_whirl(Get_Current_PU());
    return WN_CreateExp3(OPR_CSELECT, WN_rtype(wn1), MTYPE_V, wn, wn1, wn2);
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
Mark_LDA_Vbuf_Offset(WN* tree, INTRINSIC iopc) {

  BOOL has_v1buf_lda = FALSE; 
  static vector < WN* > processed; 

  if(find(processed.begin(), processed.end(), tree) != processed.end()) 
    return has_v1buf_lda;
  
  for(INT i = 0; i < WN_kid_count(tree); i++) {
    has_v1buf_lda |= Mark_LDA_Vbuf_Offset(WN_kid(tree, i), iopc);
  }

  if(WN_operator(tree) == OPR_LDA) {
    if(ST_in_vbuf(WN_st(tree)) && iopc == INTRN_VBUF_OFFSET) {
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
  WN *wn;
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
  if (code == COMPONENT_REF) {
    INT64 ofst;
    TY_IDX ty_idx0;
    tree arg0 = TREE_OPERAND(lhs, 0);
    tree arg1 = TREE_OPERAND(lhs, 1);
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
#ifdef TARG_SL
    wn = WFE_Lhs_Of_Modify_Expr(assign_code, arg0, NULL, need_result, ty_idx0, 
#else
    wn = WFE_Lhs_Of_Modify_Expr(assign_code, arg0, need_result, ty_idx0, 
#endif
				ofst+component_offset,
			        field_id + DECL_FIELD_ID(arg1), is_bit_field, 
				rhs_wn, rhs_preg_num, is_realpart, is_imagpart);
    return wn;
  }

  if (code == REALPART_EXPR) {
    tree arg0 = TREE_OPERAND(lhs, 0);
    TY_IDX ty_idx0 = Get_TY(TREE_TYPE(arg0));
#ifdef TARG_SL
    wn = WFE_Lhs_Of_Modify_Expr(assign_code, arg0, NULL, need_result, ty_idx0,
#else
    wn = WFE_Lhs_Of_Modify_Expr(assign_code, arg0, need_result, ty_idx0,
#endif
				component_offset, field_id, is_bit_field,
				rhs_wn, rhs_preg_num, TRUE, FALSE);
    return wn;
  }

  if (code == IMAGPART_EXPR) {
    tree arg0 = TREE_OPERAND(lhs, 0);
    TY_IDX ty_idx0 = Get_TY(TREE_TYPE(arg0));
#ifdef TARG_SL
    wn = WFE_Lhs_Of_Modify_Expr(assign_code, arg0, NULL, need_result, ty_idx0,
#else
    wn = WFE_Lhs_Of_Modify_Expr(assign_code, arg0, need_result, ty_idx0,
#endif
				component_offset, field_id, is_bit_field,
				rhs_wn, rhs_preg_num, FALSE, TRUE);
    return wn;
  }

  if (code == PARM_DECL || code == VAR_DECL) {
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
        FmtAssert(TY_size(desc_ty_idx) == MTYPE_byte_size(Def_Int_Mtype),
                  ("Size of ASM struct opnd must be equal to register size"));
        desc = rtype = Def_Int_Mtype;
        desc_ty_idx = hi_ty_idx = MTYPE_To_TY(Def_Int_Mtype);
      }
      ST *rhs_st = MTYPE_To_PREG(desc);
      rhs_wn = WN_CreateLdid (OPR_LDID, rtype,
			      desc, rhs_preg_num, rhs_st,
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
        wn = WN_Stid(rtype, result_preg, result_preg_st, desc_ty_idx, result_wn,
		     0);
        WFE_Stmt_Append (wn, Get_Srcpos());
        result_wn = WN_Ldid(rtype, result_preg, result_preg_st, desc_ty_idx, 0);
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
  else if (code == INDIRECT_REF) {
    WN *addr_wn = WFE_Expand_Expr (TREE_OPERAND (lhs, 0));
    TY_IDX hi_ty_idx = Get_TY(TREE_TYPE(lhs));
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
      // Bug 8738 : volatile type should NOT be for preg
      if (TY_is_volatile(address_ty_idx)) {
       Clear_TY_is_volatile(address_ty_idx);
       volt = TRUE;
      }
#endif
      preg_st = MTYPE_To_PREG(Pointer_Mtype);
      preg    = Create_Preg (Pointer_Mtype, NULL);
      wn      = WN_Stid (Pointer_Mtype, preg, preg_st, address_ty_idx, addr_wn);
      WFE_Set_ST_Addr_Saved (addr_wn);
#ifdef KEY
      // Handle function calls for asm input-output constraints
      // see torture test 990130-1.c
      WN *body = WFE_Stmt_Top();
      if (body &&		// Do prepend only for asm's.  Bug 4732.
	  WN_last(body) &&
	  WN_operator(WN_last(body)) == OPR_ASM_STMT) {
        WFE_Stmt_Prepend_Last (wn, Get_Srcpos());
      } else
#endif /* KEY */
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
#ifdef KEY
      // Handle asm like:
      // 	asm("cfc1 %0,$31":"=r"(*s));
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
      rhs_st = MTYPE_To_PREG(desc);
      // Types are likely to be wrong in the following
      rhs_wn = WN_CreateLdid (OPR_LDID, rtype, desc, rhs_preg_num, rhs_st,
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
			     WN_Floatconst (Mtype_complex_to_real (rtype), 0.0),
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
        wn = WN_Stid(rtype, result_preg, result_preg_st, desc_ty_idx, result_wn,
		     0);
        WFE_Stmt_Append (wn, Get_Srcpos());;
        result_wn = WN_Ldid(rtype, result_preg, result_preg_st, desc_ty_idx, 0);
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
  else if (code == ARRAY_REF) {
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
			     WN_Floatconst (Mtype_complex_to_real (rtype), 0.0),
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
        wn = WN_Stid(rtype, result_preg, result_preg_st, desc_ty_idx, result_wn,
		     0);
        WFE_Stmt_Append (wn, Get_Srcpos());;
        result_wn = WN_Ldid(rtype, result_preg, result_preg_st, desc_ty_idx, 0);
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
      // following two line code is used to set if iload is a v1buf iload
      // set corresponding flag for automatic expand v1buf ld/st
      // in whirl2ops.cxx 
      if(Mark_LDA_Vbuf_Offset(addr_wn,  INTRN_VBUF_OFFSET)) 
        WN_Set_is_internal_mem_ofst(wn, TRUE);

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
#ifdef KEY
  else if (code == COMPOUND_LITERAL_EXPR) {
      tree var = TREE_OPERAND (TREE_OPERAND (lhs, 0), 0); 
      TY_IDX ty_idx0 = Get_TY(TREE_TYPE(var));
#ifdef TARG_SL
      wn = WFE_Lhs_Of_Modify_Expr (assign_code, var, NULL, need_result,
#else
      wn = WFE_Lhs_Of_Modify_Expr (assign_code, var, need_result,
#endif
      				ty_idx0, component_offset, field_id,
				is_bit_field, rhs_wn, rhs_preg_num,
				is_realpart, is_imagpart);
  }
  else if (code == RESULT_DECL) {
  // Seems RESULT_DECL can appear as lhs of MODIFY_EXPR (from gcc code)
  // Need to see how a RESULT_DECL looks like, and if it can reach us ever.
      Fail_FmtAssertion ("Implement RESULT_DECL as lhs of MODIFY_EXPR");
  }
#endif // KEY
  else Fail_FmtAssertion ("WFE_Lhs_Of_Modify_Expr: unhandled tree node in LHS of MODIFY_EXPR");
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
WFE_Expand_Expr_With_Sequence_Point (tree exp, TYPE_ID mtype)
{
  WN *wn;

  if (mtype == MTYPE_V)
    wn = WFE_Expand_Expr (exp, FALSE);

  else {

    WN *comma_block      = WN_CreateBlock ();

    WFE_Stmt_Push (comma_block, wfe_stmk_comma, Get_Srcpos ());
    wn = WFE_Expand_Expr (exp);
    WFE_Stmt_Pop (wfe_stmk_comma);
    if (WN_first (comma_block)) {
      if (wn)
	wn = WN_CreateComma (OPR_COMMA, Mtype_comparison (mtype), MTYPE_V,
			     comma_block, wn);
      else {
	DevWarn("WFE_Expand_Expr_With_Sequence_Point: no wn for COMMA");
	WFE_Stmt_Append (comma_block, Get_Srcpos());
      }
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

// sync not a pure call, so make this intrinsic_call not intrinsic_op
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

#ifdef TARG_NVISA
static void
emit_builtin_brkpt (void)
{
  WN *wn = WN_Create_Intrinsic (OPC_VINTRINSIC_CALL, INTRN_BRKPT, 0, NULL);
  WFE_Stmt_Append (wn, Get_Srcpos());
}
static void
emit_builtin_trap (void)
{
  WN *wn = WN_Create_Intrinsic (OPC_VINTRINSIC_CALL, INTRN_TRAP, 0, NULL);
  WFE_Stmt_Append (wn, Get_Srcpos());
}

static WN*
emit_builtin_clock (void)
{
  WN *wn = WN_Create_Intrinsic (OPC_I4INTRINSIC_CALL, INTRN_CLOCK, 0, NULL);
  WFE_Stmt_Append (wn, Get_Srcpos());

  ST       *preg_st = MTYPE_To_PREG(MTYPE_I4);
  TY_IDX    preg_ty_idx = Be_Type_Tbl(MTYPE_I4);
  PREG_NUM  preg = Create_Preg (MTYPE_I4, NULL);

  wn = WN_Ldid (MTYPE_I4, -1, Return_Val_Preg, preg_ty_idx);
  wn = WN_Stid (MTYPE_I4, preg, preg_st, preg_ty_idx, wn),
  WFE_Stmt_Append (wn, Get_Srcpos());

  wn = WN_Ldid (MTYPE_I4, preg, preg_st, preg_ty_idx);
  return wn;
}

static WN*
emit_builtin_atomic (tree exp, UINT nkids, TYPE_ID mtype, INTRINSIC iopc)
{
  WN        *wn;
  WN        *arg_wn;
  WN        *ikids [3];
  TY_IDX     arg_ty_idx;
  TYPE_ID    arg_mtype;
  tree       list = TREE_OPERAND (exp, 1);
  OPCODE     opc;

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
  if (nkids == 3) {
    list       = TREE_CHAIN (list);
    arg_ty_idx = Get_TY(TREE_TYPE(TREE_VALUE(list)));
    arg_mtype  = TY_mtype (arg_ty_idx);
    arg_wn     = WFE_Expand_Expr (TREE_VALUE (list));
    arg_wn     = WN_CreateParm (arg_mtype, arg_wn, arg_ty_idx, WN_PARM_BY_VALUE);
    ikids [2]  = arg_wn;
  }

  opc = OPCODE_make_op(OPR_INTRINSIC_CALL,mtype,MTYPE_V);
  wn = WN_Create_Intrinsic (opc, iopc, nkids, ikids);
  WFE_Stmt_Append (wn, Get_Srcpos());

  ST       *preg_st = MTYPE_To_PREG(mtype);
  TY_IDX    preg_ty_idx = ST_type(preg_st);
  PREG_NUM  preg = Create_Preg (mtype, NULL);

  wn = WN_Ldid (mtype, -1, Return_Val_Preg, preg_ty_idx);
  wn = WN_Stid (mtype, preg, preg_st, preg_ty_idx, wn),
  WFE_Stmt_Append (wn, Get_Srcpos());

  wn = WN_Ldid (mtype, preg, preg_st, preg_ty_idx);
  return wn;
}

static WN*
emit_builtin_vote (tree exp, INTRINSIC iopc)
{
  // NB: We are using MTYPE_I4 instead of MTYPE_B since
  // intrinsics do not allow the predicate type.
  OPCODE     opc = OPCODE_make_op (OPR_INTRINSIC_CALL, MTYPE_I4, MTYPE_V);
  tree       arg = TREE_VALUE (TREE_OPERAND (exp, 1));
  TY_IDX arg_ty_idx = Get_TY(TREE_TYPE (arg));
  TYPE_ID arg_mtype = TY_mtype (arg_ty_idx);
  WN *wn, *arg_wn, *ikids [1];

  arg_wn = WFE_Expand_Expr (arg);
  arg_wn = WN_CreateParm (arg_mtype, arg_wn, arg_ty_idx, WN_PARM_BY_VALUE);
  ikids [0] = arg_wn;

  wn = WN_Create_Intrinsic (opc, iopc, 1, ikids);
  WFE_Stmt_Append (wn, Get_Srcpos());

  ST       *preg_st = MTYPE_To_PREG(MTYPE_I4);
  TY_IDX    preg_ty_idx = ST_type(preg_st);
  PREG_NUM  preg = Create_Preg (MTYPE_I4, NULL);

  wn = WN_Ldid (MTYPE_I4, -1, Return_Val_Preg, preg_ty_idx);
  wn = WN_Stid (MTYPE_I4, preg, preg_st, preg_ty_idx, wn);
  WFE_Stmt_Append (wn, Get_Srcpos());

  wn = WN_Ldid (MTYPE_I4, preg, preg_st, preg_ty_idx);

  return wn;
}

// create block of asm output constraints;
// assume here that all are same type (happens to be true so far).
static WN*
create_asm_output_constraints (INT num_outputs, TYPE_ID mtype)
{
  WN *output_constraint_block = WN_CreateBlock ();
  UINT opnd_num;
  for (opnd_num = 0; opnd_num < num_outputs; ++opnd_num) {
        ST *preg_st = MTYPE_To_PREG(mtype);
        ST *constraint_st = New_ST(CURRENT_SYMTAB);
        ST_Init(constraint_st,
                Save_Str(MTYPE_is_float(mtype) ? "=f" : "=r"),
                CLASS_NAME, SCLASS_UNKNOWN, EXPORT_LOCAL, (TY_IDX) 0);

        WN *constraint_pragma =
          WN_CreatePragma (WN_PRAGMA_ASM_CONSTRAINT,
                           (ST_IDX) ST_st_idx(preg_st),
                           (INT32) ST_st_idx(constraint_st),
                           asm_neg_preg,
                           opnd_num);

        WN_INSERT_BlockAfter (output_constraint_block,
                              WN_last (output_constraint_block),
                              constraint_pragma);
        --asm_neg_preg;
  }
  return output_constraint_block;
}

// parameterized routine for textures:
// pass in ptx inst name, element type of result and coord
static WN*
emit_builtin_texfetch (tree exp, char *name, TYPE_ID rtype, TYPE_ID ctype)
{
  // If try to emit this as a regular call,
  // then need predefined registers for the vector return value,
  // which we don't really have.
  // So just make this be a black box and emit as asm statement.
  tree list = TREE_OPERAND (exp, 1);
  WN *texture_arg = WFE_Expand_Expr (TREE_VALUE (list));
  list       = TREE_CHAIN (list);
  WN *coord_arg = WFE_Expand_Expr (TREE_VALUE (list));

  char buf[80];
  strcpy(buf, name);
  strcat(buf, " {%0,%1,%2,%3},[%4,{%5,%6,%7,%8}];");
  WN *asm_wn = WN_CreateAsm_Stmt (7 /* 2 + #inputs */, buf);

  // no clobbered registers
  WN *clobber_block = WN_CreateBlock ();
  WN_kid0(asm_wn) = clobber_block;

  UINT opnd_num = 4;
  WN_kid1(asm_wn) = create_asm_output_constraints (opnd_num, rtype);

  // texture param
  WN_kid (asm_wn, 2) =
        WN_CreateAsm_Input ("m", opnd_num, texture_arg);
  ++opnd_num;

  // coord params (split into 4 regs)
  INT kidnum = 3;
  INT offset = 0;
  INT field_id = 1;
  for (; opnd_num <= 8; ++opnd_num) {
    WN *coordf = WN_CreateLdid (OPR_LDID, ctype, ctype,
        offset, WN_st(coord_arg), WN_ty(coord_arg), field_id);
    WN_kid (asm_wn, kidnum) =
        WN_CreateAsm_Input (
          (char*) ((ctype == MTYPE_F4) ? "f" : "r"),
          opnd_num, coordf);
    offset += 4;
    ++field_id;
    ++kidnum;
  }

  return asm_wn;
}

/* mov.b64 {r1,r2},fd1 */
static WN*
emit_builtin_double_to_int2 (tree exp)
{
  tree list = TREE_OPERAND (exp, 1);
  WN *double_arg = WFE_Expand_Expr (TREE_VALUE (list));

  WN *asm_wn = WN_CreateAsm_Stmt (3 /* 2 + #inputs */, "mov.b64 {%0,%1},%2;");

  // no clobbered registers
  WN *clobber_block = WN_CreateBlock ();
  WN_kid0(asm_wn) = clobber_block;

  UINT opnd_num = 2;
  WN_kid1(asm_wn) = create_asm_output_constraints (opnd_num, MTYPE_I4);

  // double param
  WN_kid (asm_wn, 2) =
        WN_CreateAsm_Input ("f", opnd_num, double_arg);
  ++opnd_num;

  return asm_wn;
}

/* mov.b64 fd1,{r1,r2} */
static WN*
emit_builtin_int2_to_double (tree exp)
{
  tree list = TREE_OPERAND (exp, 1);
  WN *int2_arg = WFE_Expand_Expr (TREE_VALUE (list));

  WN *asm_wn = WN_CreateAsm_Stmt (4 /* 2 + #inputs */, "mov.b64 %0,{%1,%2};");

  // no clobbered registers
  WN *clobber_block = WN_CreateBlock ();
  WN_kid0(asm_wn) = clobber_block;

  UINT opnd_num = 1;
  WN_kid1(asm_wn) = create_asm_output_constraints (opnd_num, MTYPE_F8);

  // int2 param (split into 2 regs)
  INT kidnum = 2;
  INT offset = 0;
  INT field_id = 1;
  for (; opnd_num <= 2; ++opnd_num) {
    WN *int2f = WN_CreateLdid (OPR_LDID, MTYPE_I4, MTYPE_I4,
        offset, WN_st(int2_arg), WN_ty(int2_arg), field_id);
    WN_kid (asm_wn, kidnum) =
        WN_CreateAsm_Input ("r", opnd_num, int2f);
    offset += 4;
    ++field_id;
    ++kidnum;
  }

  return asm_wn;
}
#endif

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

#ifdef KEY
  WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, arg_temp_st);
#endif
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

#ifdef KEY
  WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, struct_temp_st);
#endif
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
#ifdef KEY
  WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, arg_temp_st);
#endif
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

#ifdef KEY
  WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, struct_temp_st);
#endif
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
#ifdef KEY
  WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, arg_temp_st);
#endif
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
// bug 2813
// If we are expanding a stmt and we reach here through the expansion of
// STMT_EXPR, the stmt may have a tree-chain even if it is not a COMPOUND_STMT.
// Tree-chain for a COMPOUND_STMT is already handled. So handle it for the 
// other stmts (mimick expand_stmt (c-semantics.c))
// e.g.if GNU inlines a function containing "if (1) return 1;",
// The then-clause will have 2 stmts, one is a store
// of 1 into a temporary variable, the 2nd is a jmp. We will miss the jmp
// if we don't traverse the TREE_CHAIN.
static inline void
traverse_tree_chain (tree op1)
{
  if (TREE_CODE (op1) != COMPOUND_STMT && TREE_CHAIN (op1))
  {
    tree last = TREE_CHAIN (op1);
    while (last)
    {
      WFE_Expand_Expr (last, FALSE);
      last = TREE_CHAIN (last);
    }
  }
}

// bug 3180: Use a stack of struct nesting to handle nested 
// loops/switch/case in a STMT_EXPR
struct nesting * wfe_nesting_stack;
struct nesting * wfe_cond_stack;
struct nesting * wfe_loop_stack;
struct nesting * wfe_case_stack;
extern "C"
{
// malloc a structure
extern struct nesting * alloc_nesting (void);
// initialize fields in struct (1st parameter)
extern void construct_nesting ( struct nesting *,
				struct nesting *,
				struct nesting *,
				LABEL_IDX);
// mimic POPSTACK in gnu/stmt.c
extern void popstack (struct nesting *);
extern LABEL_IDX get_nesting_label (struct nesting *);
extern struct nesting * wfe_get_matching_scope (struct nesting *);

// process pragma statements, function definition in c-semantics.c
extern void process_omp_stmt (tree);
}

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
#endif // KEY

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

ST* ap_tmp;
ST* vararg_ofst;
static WN * CURRENT_PU_WN;

void Set_Current_PU_WN(WN * wn)
{
	CURRENT_PU_WN = wn;
}

WN* Get_Current_PU_WN()
{
	return CURRENT_PU_WN;
}

static bool isFindCalled = false;
TY_IDX
Find_Vararg_TY_IDX_PPC32() {
	static TY_IDX vararg_ty_idx;
	if (!isFindCalled) {
		isFindCalled = true;
	} else {
		return vararg_ty_idx;
	}
	STR_IDX name_idx = Save_Str("__va_list_tag");
	UINT idx = 0;
	TY_ITER iter = Ty_tab.begin();
	for (; iter != Ty_tab.end(); iter++, idx++) {
		if ((*iter).name_idx == name_idx) {
			vararg_ty_idx = make_TY_IDX(idx);
			return vararg_ty_idx;
		}
	}
	FmtAssert(FALSE, ("could not find __va_list_tag"));
}

inline void 
Check_AP_Tmp()
{
	if (ap_tmp == NULL) {
		ap_tmp = New_ST();
		ST_Init(ap_tmp, Save_Str("ap_tmp"), CLASS_VAR, SCLASS_AUTO, EXPORT_LOCAL, Make_Pointer_Type(Find_Vararg_TY_IDX_PPC32(), FALSE));
	}
}
inline void 
Check_Vararg_Ofst()
{
	if (vararg_ofst == NULL) {
		vararg_ofst = New_ST();
		ST_Init(vararg_ofst, Save_Str("va_cur_position"), CLASS_VAR, SCLASS_AUTO, EXPORT_LOCAL, MTYPE_To_TY(Pointer_Mtype));
	}
}

extern void WFE_Expand_Start_Cond_WN(WN* test,  int exitflag);

bool 
Vararg_Is_Simple_Type(TYPE_ID mtype)
{
	return mtype == MTYPE_I1 || mtype == MTYPE_I2 || mtype == MTYPE_I4 || mtype == MTYPE_I8
		|| mtype == MTYPE_U1 || mtype == MTYPE_U2 || mtype == MTYPE_U4 || mtype == MTYPE_U8
		|| mtype == MTYPE_F4 || mtype == MTYPE_F8 || mtype == MTYPE_A4;
}

TY_IDX
WFE_Load_Va_List_PPC32(tree ap)
{
	WN* ap_wn = WFE_Expand_Expr(ap);
	TY_IDX va_ty_index = Find_Vararg_TY_IDX_PPC32();
	if (WN_opcode(ap_wn) == OPC_MMLDID) {
		ST* ap_st = WN_st(ap_wn);
		Set_ST_addr_saved(ap_st);
		ap_wn = WN_Lda(Pointer_Mtype, ST_ofst(ap_st), ap_st);
	} else if (WN_opcode(ap_wn) == OPC_MMILOAD) { // for pointer
		ap_wn = WN_kid0(ap_wn);
	}
	ap_wn = 	WN_Ternary(OPR_ARRAY, Pointer_Mtype, 
			ap_wn, 
			Make_Integer_Const(MTYPE_I4, 1), 
			Make_Integer_Const(MTYPE_I4, 0));
	WN_element_size(ap_wn) = TY_size(va_ty_index);
	Check_AP_Tmp();
	WN* wn = WN_Stid(Pointer_Mtype, ST_ofst(ap_tmp), ap_tmp, Make_Pointer_Type(va_ty_index, FALSE), ap_wn);
	WFE_Stmt_Append (wn, Get_Srcpos());
	return va_ty_index;
}

WN* 
WFE_Expand_Vararg_Expr_PPC32(TY_IDX ty_idx, TY_IDX va_ty_index, FLD_HANDLE& reg_count, FLD_HANDLE& reg_save_area, FLD_HANDLE& overflow_area)
{
	TYPE_ID mtype = TY_mtype(ty_idx);
	UINT reg_count_field_id = MTYPE_is_float(mtype) ? 2 : 1;
	TYPE_ID	reg_count_rtype = Mtype_comparison(TY_mtype(FLD_type(reg_count)));
	if (mtype == MTYPE_I8 || mtype == MTYPE_U8 || mtype == MTYPE_A8) {
		WN* iwn = WN_Iload(TY_mtype(FLD_type(reg_count)), FLD_ofst(reg_count), va_ty_index, 
			WN_Ldid(Pointer_Mtype, ST_ofst(ap_tmp), ap_tmp, Make_Pointer_Type(va_ty_index, FALSE)),
			reg_count_field_id);
		iwn = WN_Band(reg_count_rtype, iwn, Make_Integer_Const(reg_count_rtype, 1));
		iwn = WN_Add(reg_count_rtype, iwn, 
			WN_Iload(TY_mtype(FLD_type(reg_count)), FLD_ofst(reg_count), va_ty_index, 
			WN_Ldid(Pointer_Mtype, ST_ofst(ap_tmp), ap_tmp, Make_Pointer_Type(va_ty_index, FALSE)),
			reg_count_field_id));
		iwn = WN_Istore(TY_mtype(FLD_type(reg_count)),  FLD_ofst(reg_count), Make_Pointer_Type(va_ty_index, FALSE),
			WN_Ldid(Pointer_Mtype, ST_ofst(ap_tmp), ap_tmp, Make_Pointer_Type(va_ty_index, FALSE)), iwn, reg_count_field_id);
		WFE_Stmt_Append (iwn, Get_Srcpos());
		
	}
	WN* cond = WN_LT(reg_count_rtype,  
		WN_Iload(TY_mtype(FLD_type(reg_count)), FLD_ofst(reg_count), va_ty_index, 
			WN_Ldid(Pointer_Mtype, ST_ofst(ap_tmp), ap_tmp, Make_Pointer_Type(va_ty_index, FALSE)),
			reg_count_field_id), 
		Make_Integer_Const(reg_count_rtype, 8));
	INT16 ty_size = (MTYPE_is_size_double(mtype) || mtype == MTYPE_F4) ? 8 : 4;
	WN *comma_block      = WN_CreateBlock ();
 	WFE_Stmt_Push (comma_block, wfe_stmk_comma, Get_Srcpos ());
	WFE_Stmt_Pop (wfe_stmk_comma);
	if (WN_first (comma_block)) {
		cond = WN_CreateComma (OPR_COMMA, Mtype_comparison (mtype), MTYPE_V,
		comma_block, cond);
	} else {
		WN_Delete (comma_block);
	}
	WFE_Expand_Start_Cond_WN(cond, 0);
	Check_Vararg_Ofst();
	WN* wn = WN_Iload(TY_mtype(FLD_type(reg_count)), FLD_ofst(reg_count), va_ty_index, 
			WN_Ldid(Pointer_Mtype, ST_ofst(ap_tmp), ap_tmp, Make_Pointer_Type(va_ty_index, FALSE)),
			reg_count_field_id);
	if (reg_count_rtype != Pointer_Mtype) {
		wn = WN_Cvt(reg_count_rtype, Pointer_Mtype, wn);
	}
	wn = WN_Shl(Pointer_Mtype, wn, Make_Integer_Const(MTYPE_I4, MTYPE_is_float(mtype) ? 3 : 2));
	WN* base = WN_Iload(TY_mtype(FLD_type(reg_save_area)), FLD_ofst(reg_save_area), va_ty_index, 
			WN_Ldid(Pointer_Mtype, ST_ofst(ap_tmp), ap_tmp, Make_Pointer_Type(va_ty_index, FALSE)),
			4);
	if (MTYPE_is_float(mtype)) {
		base = WN_Add(Pointer_Mtype, base, Make_Integer_Const(Pointer_Mtype, 32));
	}
	wn = WN_Add(Pointer_Mtype, wn, base);
	wn = WN_Stid(Pointer_Mtype, ST_ofst(vararg_ofst), vararg_ofst, Make_Pointer_Type(ty_idx, FALSE), wn);
	WFE_Stmt_Append (wn, Get_Srcpos());

	INT16  countinc = (mtype == MTYPE_I8 || mtype == MTYPE_U8) ? 2 : 1;
	wn = WN_Iload(TY_mtype(FLD_type(reg_count)), FLD_ofst(reg_count), va_ty_index, 
			WN_Ldid(Pointer_Mtype, ST_ofst(ap_tmp), ap_tmp, Make_Pointer_Type(va_ty_index, FALSE)),
			reg_count_field_id);
	wn = WN_Add(reg_count_rtype, wn, Make_Integer_Const(reg_count_rtype, countinc));
	wn = WN_Istore(TY_mtype(FLD_type(reg_count)),  FLD_ofst(reg_count), Make_Pointer_Type(va_ty_index, FALSE), 
			WN_Ldid(Pointer_Mtype, ST_ofst(ap_tmp), ap_tmp, Make_Pointer_Type(va_ty_index, FALSE)), wn,reg_count_field_id);
	WFE_Stmt_Append (wn, Get_Srcpos());
			
	WFE_Expand_Start_Else();

	wn = WN_Iload(TY_mtype(FLD_type(overflow_area)), FLD_ofst(overflow_area), va_ty_index, 
			WN_Ldid(Pointer_Mtype, ST_ofst(ap_tmp), ap_tmp, Make_Pointer_Type(va_ty_index, FALSE)), 
			3);
	wn = WN_Stid(Pointer_Mtype, ST_ofst(vararg_ofst), vararg_ofst, Make_Pointer_Type(ty_idx, FALSE), wn);
	WFE_Stmt_Append (wn, Get_Srcpos());

	wn = WN_Iload(TY_mtype(FLD_type(overflow_area)), FLD_ofst(overflow_area), va_ty_index, 
			WN_Ldid(Pointer_Mtype, ST_ofst(ap_tmp), ap_tmp, Make_Pointer_Type(va_ty_index, FALSE)), 
			3);
	wn = WN_Add(TY_mtype(FLD_type(overflow_area)), wn, Make_Integer_Const(TY_mtype(FLD_type(overflow_area)), ty_size));
	wn = WN_Istore(TY_mtype(FLD_type(overflow_area)),  FLD_ofst(overflow_area), Make_Pointer_Type(va_ty_index, FALSE), 
			WN_Ldid(Pointer_Mtype, ST_ofst(ap_tmp), ap_tmp, Make_Pointer_Type(va_ty_index, FALSE)), wn, 3);
	WFE_Stmt_Append (wn, Get_Srcpos());

	WFE_Expand_End_Cond ();

	
	if (Vararg_Is_Simple_Type(mtype)) {
		wn = WN_Iload(mtype, 0, ty_idx, 
			WN_Ldid(Pointer_Mtype, ST_ofst(vararg_ofst), vararg_ofst, Make_Pointer_Type(ty_idx, FALSE)));
	} else {
		wn = WN_Iload(Pointer_Mtype, 0, Make_Pointer_Type(ty_idx, FALSE), 
			WN_Ldid(Pointer_Mtype, ST_ofst(vararg_ofst), vararg_ofst, Make_Pointer_Type(ty_idx, FALSE)));
		wn = WN_Iload(mtype, 0,  ty_idx, wn);
	}
	return wn;
}


WN*
WFE_Expand_Vararg_Expr_PPC32(TY_IDX ty_idx, TY_IDX va_ty_index)
{
	FLD_HANDLE gpr = TY_fld(va_ty_index);
	FLD_HANDLE fpr = FLD_next(gpr);
	FLD_HANDLE overflow_area = FLD_next(fpr);
	FLD_HANDLE reg_save_area = FLD_next(overflow_area);
	TYPE_ID mtype = TY_mtype(ty_idx);
	switch (mtype)
	{	
    		case MTYPE_F4:
		case MTYPE_F8:
		{
			return WFE_Expand_Vararg_Expr_PPC32(ty_idx, va_ty_index, fpr, reg_save_area, overflow_area);
		}
		default:
			return WFE_Expand_Vararg_Expr_PPC32(ty_idx, va_ty_index, gpr, reg_save_area, overflow_area);
	}
	return NULL;
}

/* expand gnu expr tree into symtab & whirl */
WN *
WFE_Expand_Expr (tree exp, 
		 bool need_result,
		 TY_IDX nop_ty_idx, 
		 TY_IDX component_ty_idx, 
		 INT64 component_offset,
		 UINT16 field_id,
		 bool is_bit_field,
                 bool expect_boolean)
{
  enum tree_code code = TREE_CODE (exp);
  WN *wn, *wn0, *wn1, *wn2;
  ST *st;
  TY_IDX ty_idx;
  TY_IDX desc_ty_idx;
  tree arg0, arg1, arg2;

  wn = NULL;

#ifdef WFE_DEBUG
  fprintf (stderr,
           "{( WFE_Expand_Expr: %s\n", Operator_From_Tree [code].name); // ")}"
#endif /* WFE_DEBUG */

  switch (code)
    {
    // leaves
    case ADDR_EXPR:
      {
	arg0 = TREE_OPERAND (exp, 0);
	enum tree_code code0 = TREE_CODE (arg0);
	switch (code0) {
	  case VAR_DECL:
	  case PARM_DECL:
	  case FUNCTION_DECL:
	    {
	      st = Get_ST (arg0);
	      ty_idx = ST_type (st);
#ifdef KEY
	      // Taking the address of a nested function requires the use of
	      // a trampoline dynamically allocated on the stack
	      if (code0 == FUNCTION_DECL && 
		  PU_is_nested_func(Pu_Table[ST_pu(st)]))
		Set_PU_has_alloca(Get_Current_PU());
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

#ifdef KEY // bug 3228
	  case INDIRECT_REF:
	    wn = WFE_Expand_Expr (TREE_OPERAND(arg0, 0));
            break;
#endif

	  case STRING_CST:
	    {
              TCON tcon;
              tcon = Host_To_Targ_String (MTYPE_STRING,
                                          const_cast<char *>TREE_STRING_POINTER(arg0),
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
              FmtAssert (arg0->decl.symtab_idx == CURRENT_SYMTAB,
                         ("line %d: taking address of a label not defined in current function currently not implemented", lineno));
              wn = WN_LdaLabel (Pointer_Mtype, label_idx);
	      Set_LABEL_addr_saved (label_idx);
#ifdef KEY
	      // Bugs 1056 &  1227 - As a quality of implementation issue, we 
	      // should not prevent inlining of function explicitly marked 
	      // static inline just because a label therein had its address 
	      // taken. 
	      if ( ST_export (Get_Current_PU_ST()) != EXPORT_LOCAL)
#endif
#if defined(TARG_PPC32)
            {
              Clear_PU_must_inline(Get_Current_PU());
#endif
              Set_PU_no_inline (Get_Current_PU ());
#if defined(TARG_PPC32)
             }
#endif
            }
            break;

	  case COMPONENT_REF:
	    {
	      wn = WFE_Expand_Expr (arg0);
              ty_idx = Get_TY(TREE_TYPE(arg0));
	      if (WN_operator (wn) == OPR_LDID) {
		WN_set_operator (wn, OPR_LDA);
		WN_set_desc (wn, MTYPE_V);
                WN_set_rtype (wn, Pointer_Mtype);
                WN_set_ty (wn, Make_Pointer_Type(WN_ty(wn))); // bug 10098, bug 10352
	      }
	      else
	      if (WN_operator (wn) == OPR_ILOAD) {
		wn0 = WN_kid0 (wn);
		wn1 = WN_Intconst (Pointer_Mtype, WN_offset (wn));
	        wn  = WN_Binary (OPR_ADD, Pointer_Mtype, wn0, wn1);
	      }
	      else
	        Fail_FmtAssertion ("WFE_Expand_Expr: ADDR_EXPR has unhandled %s",
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
	    	tree oper = TREE_OPERAND (TREE_OPERAND (arg0, 0), 0);
		if (TREE_CODE (DECL_INITIAL(oper)) == CONSTRUCTOR)
		{
	            arg0 = DECL_INITIAL (oper);
		    st = WFE_Generate_Temp_For_Initialized_Aggregate (arg0, "");
		}
		else 
		{
		    arg0 = oper; // just in case someone expects the proper value from arg0
		    st = Get_ST (arg0);
		}
	        wn = WN_Lda (Pointer_Mtype, ST_ofst(st), st);
	    }
	    break;
	  
	  case REALPART_EXPR: // bug 10103
	    {
	      wn = WFE_Expand_Expr (TREE_OPERAND (arg0, 0));
	      if (WN_operator (wn) == OPR_ILOAD)
		wn = WN_kid0 (wn);
	      else if (WN_operator (wn) == OPR_LDID)
		wn = WN_Lda (Pointer_Mtype, WN_offset(wn), WN_st(wn));
	      else Fail_FmtAssertion ("WFE_Expand_Expr: NYI for REALPART_EXPR");
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
		case MTYPE_CQ:
		  imag_mtype = MTYPE_FQ;
		  break;
		default:
		  Fail_FmtAssertion ("WFE_Expand_Expr: Unexpected rtype in IMAGPART_EXPR");
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
	    else Fail_FmtAssertion ("WFE_Expand_Expr: NYI for IMAGPART_EXPR");
	  }
	  break;
#endif

#ifdef KEY // bug 11877
          case CALL_EXPR: {
            WN *comma = WFE_Expand_Expr (arg0);
            if (WN_operator(comma) != OPR_COMMA)
              Fail_FmtAssertion ("WFE_Expand_Expr: ADDR_EXPR of call returning void unhandled");
            wn = WN_kid1(comma);
            if (WN_operator(wn) != OPR_LDID)
              Fail_FmtAssertion ("WFE_Expand_Expr: ADDR_EXPR of call returning void unhandled");
            WN_set_operator (wn, OPR_LDA);
            WN_set_rtype (wn, Pointer_Mtype);
            WN_set_desc (wn, MTYPE_V);
            WN_set_ty(wn, Make_Pointer_Type(WN_ty(wn)));
            WN_set_rtype (comma, Pointer_Mtype);
            wn = comma;
            break;
          }

	  case MODIFY_EXPR : {
	    // OSP_7, MODIFY_EXPR in ADDR_EXPR
	    // (a = b).c
	    // ADDR_EXPR
	    //    |--> MODIFY_EXPR
	    WFE_Expand_Expr (arg0);
	    tree lhs = TREE_OPERAND(arg0, 0);
	    Is_True (lhs != NULL && TREE_CODE(lhs) == VAR_DECL,
			    ("Unsupported lhs for `(p=q).x'"));
	    st = Get_ST(TREE_OPERAND(arg0, 0));
	    wn = WN_Lda(Pointer_Mtype, ST_ofst(st), st);
	    break;
	  }
#endif

	  default:
	    {
	      Fail_FmtAssertion ("WFE_Expand_Expr: ADDR_EXPR has unhandled %s",
				 Operator_From_Tree [code0].name);
	    }
	    break;
	}
      }
      break;

    case FUNCTION_DECL:
      {
	 st = Get_ST (exp);
	 ty_idx = ST_type (st);
	 wn = WN_Lda (Pointer_Mtype, ST_ofst(st), st);
      }
      break;

    case BIND_EXPR:
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
          wn1 = WN_last (block);
          if (wn1 && WN_operator (wn1) == OPR_EVAL) {
            wn1 = WN_COPY_Tree (WN_kid0 (wn1));
            wn0 = block;
            WN_DELETE_FromBlock (wn0, WN_last (wn0));
	    WFE_Stmt_Append (wn0, Get_Srcpos ());
	    if (nop_ty_idx == 0 && component_ty_idx == 0) {
	      wn = wn1;
              break;
	    }
            if (WN_operator (wn1) == OPR_LDID)
              st = WN_st (wn1);
            else {
              st = Gen_Temp_Symbol (ty_idx, "__bind_expr");
#ifdef KEY
	      WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, st);
#endif // KEY
              WFE_Set_ST_Addr_Saved (wn1);
              wn0 = WN_Stid (mtype, 0, st, ty_idx, wn1);
              WFE_Stmt_Append (wn0, Get_Srcpos ());
            }
          }
          else {
	    WFE_Stmt_Append (block, Get_Srcpos ());
            break;
          }
	}
      }
      /*FALLTHRU*/

    case CONSTRUCTOR:
    case PARM_DECL: // for formal parms
    case VAR_DECL:
      {
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
          Is_True(ST_st_idx(st) != 0, ("got null st?"));
          Is_True(ST_st_idx(st) != -1, ("got -1 st?"));
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
			    ST_ofst(st)+component_offset+preg_num, st,
			    field_id != 0 ? hi_ty_idx : ty_idx, field_id);
	if (cvtl_size != 0)
	  wn = WN_CreateCvtl(OPR_CVTL, rtype, MTYPE_V, cvtl_size, wn);
      }
      break;

    case INTEGER_CST:
      {
	ty_idx = Get_TY (TREE_TYPE(exp));
	TYPE_ID mtyp = TY_mtype(ty_idx);
	mtyp = (mtyp == MTYPE_V) ? MTYPE_I4 : Widen_Mtype(mtyp);
	wn = WN_Intconst(mtyp, Get_Integer_Value(exp));
      }
      break;

    case REAL_CST:
      {
	TCON tcon;
	ty_idx = Get_TY (TREE_TYPE(exp));
#if (defined(TARG_IA32) || defined(TARG_X8664)) && !defined(REAL_ARITHMETIC)
	tcon = Host_To_Targ_Float (TY_mtype (ty_idx), TREE_REAL_CST(exp));
#else
	REAL_VALUE_TYPE real = TREE_REAL_CST(exp);

#ifdef TARG_IA64
	switch (TY_mtype (ty_idx)) {
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
      int rval;
      long rbuf [4];
#ifdef KEY
      INT32 rbuf_w[4]; // this is needed when long is 64-bit
      INT32 i;
#endif
      switch (TY_mtype (ty_idx)) {
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
#if defined(TARG_IA32) || defined(TARG_X8664) || defined(TARG_NVISA) || defined(TARG_LOONGSON)
        case MTYPE_FQ:
          REAL_VALUE_TO_TARGET_LONG_DOUBLE (real, rbuf);
          for (i = 0; i < 4; i++)
            rbuf_w[i] = rbuf[i];
#ifdef TARG_LOONGSON
	    tcon = Host_To_Targ_Quad (*(QUAD_TYPE *)&rbuf_w);
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
#ifdef KEY
	// Bug 949
	if (TREE_CODE(TREE_REALPART(exp)) != REAL_CST || 
	    TREE_CODE(TREE_IMAGPART(exp)) != REAL_CST) {
	  #ifdef PSC_TO_OPEN64
	  printf("opencc does not support complex integer data types (a gcc extension)\n");
	  #endif
	  exit(2);
	}
#endif
#if (defined(TARG_IA32) || defined(TARG_X8664)) && !defined(REAL_ARITHMETIC)
        tcon = Host_To_Targ_Complex (TY_mtype (ty_idx),
				     TREE_REAL_CST(TREE_REALPART(exp)),
				     TREE_REAL_CST(TREE_IMAGPART(exp)));
#else
	REAL_VALUE_TYPE real = TREE_REAL_CST(TREE_REALPART(exp));
	REAL_VALUE_TYPE imag = TREE_REAL_CST(TREE_IMAGPART(exp));
#ifdef TARG_IA64
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
                      *(float *) &rval, *(float *) &ival);
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
                            *(double *) &rbuf_w, *(double *) &ibuf_w);
#else
	   tcon = Host_To_Targ_Complex (MTYPE_C8,
                            *(double *) &rbuf, *(double *) &ibuf);
#endif
	   break;										       
#ifdef KEY
	  case MTYPE_CQ:
	    REAL_VALUE_TO_TARGET_LONG_DOUBLE (real, rbuf);
	    REAL_VALUE_TO_TARGET_LONG_DOUBLE (imag, ibuf);
	    WFE_Convert_To_Host_Order(rbuf);
	    WFE_Convert_To_Host_Order(ibuf);
	    for (i = 0; i < 4; i++) {
              rbuf_w[i] = rbuf[i];
	      ibuf_w[i] = ibuf[i];
	    }
#ifdef TARG_LOONGSON
            tcon = Host_To_Targ_Complex_Quad( *(QUAD_TYPE *)&rbuf_w, *(QUAD_TYPE *)&ibuf_w);
#else
            tcon = Host_To_Targ_Complex_Quad( *(long double *) &rbuf_w,
                                 *(long double *) &ibuf_w );
#endif
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

    // this should occur only if string is a statement expression
    case STRING_CST:
      {
	TCON tcon;
	tcon = Host_To_Targ_String (MTYPE_STRING,
				    const_cast<char *>TREE_STRING_POINTER(exp),
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
    case TRUTH_NOT_EXPR:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
      {
#ifdef TARG_SL
        wn0 = WFE_Expand_Expr (TREE_OPERAND (exp, 0), Get_TY(TREE_TYPE(exp)));
#else
        wn0 = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
#endif
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
        wn = WFE_Expand_Expr (TREE_OPERAND (exp, 0), 
#ifndef KEY
			      TRUE, 
#else
			      mtyp != MTYPE_V, // need_result
#endif
			      (mtyp == MTYPE_M) ? 0 : ty_idx);
#ifdef KEY
// Fix Bug# 10
// mtyp == MTYPE_V => wn == 0
	if ((mtyp == MTYPE_M) || (mtyp == MTYPE_V))
#else
	if (mtyp == MTYPE_M) 
#endif // KEY
	  break;
	if (MTYPE_is_integral(mtyp) && MTYPE_is_integral(WN_rtype(wn))) {
	  // For 32-bit to 64-bit conversion, make the result have the same
	  // sign as the source.  Fix bug 480.
#ifndef TARG_IA64
	  if (MTYPE_size_min(mtyp) == 64 &&
	      MTYPE_size_min(WN_rtype(wn)) == 32 &&
	      MTYPE_is_signed(mtyp) != MTYPE_is_signed(WN_rtype(wn))) {
	    mtyp = MTYPE_complement(mtyp);
	  }
#endif
	  if (MTYPE_size_min(mtyp) < MTYPE_size_min(WN_rtype(wn))) {
	    if (MTYPE_size_min(mtyp) != 32)
#ifdef TARG_NVISA // maybe should be all targets, but NVISA is stricter
            {
              // cvtl rtype should be same as child type
              // (else may get U4CVTL of U8 child)
              // but preserve sign of mtyp for cvtl
              TYPE_ID cvtl_mtype = Mtype_TransferSign(mtyp, WN_rtype(wn));
              wn = WN_CreateCvtl(OPR_CVTL, Widen_Mtype(cvtl_mtype), MTYPE_V,
                                 MTYPE_size_min(mtyp), wn);
              // if widened dest type is still smaller, do cvt of cvtl, e.g.
              // U4U8CVT ( U8CVTL 8 (U8ADD
              if (MTYPE_size_min(Widen_Mtype(mtyp))
                < MTYPE_size_min(cvtl_mtype))
              {
                wn = WN_Cvt(cvtl_mtype, Widen_Mtype(mtyp), wn);
              }
            }
#else
	      wn = WN_CreateCvtl(OPR_CVTL, Widen_Mtype(mtyp), MTYPE_V,
			         MTYPE_size_min(mtyp), wn);
#endif
	    else wn = WN_Cvt(WN_rtype(wn), mtyp, wn);
	  }
	  else {
	    TY_IDX ty_idx0 = Get_TY(TREE_TYPE(TREE_OPERAND (exp, 0)));
	    TYPE_ID mtyp0 = TY_mtype(ty_idx0);

	    if (MTYPE_size_min(mtyp) > MTYPE_size_min(mtyp0) &&
#ifndef KEY
		! Has_Subsumed_Cvtl(WN_operator(wn)))
#else
	        ! Has_Subsumed_Cvtl(WN_operator(wn)) &&
		  Widen_Mtype(mtyp0) != mtyp0)
#endif
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
	if (TREE_CODE(arg0) == MODIFY_EXPR) {
	  TYPE_ID rtype = Widen_Mtype(TY_mtype(ty_idx));
	  TYPE_ID desc = TY_mtype(ty_idx);
	  if (WN_operator(wn) == OPR_ILOAD) {
            wn = WN_CreateIload(OPR_ILOAD, rtype, desc,
			        WN_offset(wn) + ofst + component_offset, ty_idx,
			        WN_load_addr_ty(wn), WN_kid0(wn),
			        WN_field_id(wn)+field_id + DECL_FIELD_ID(arg1));
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
	      WN_set_field_id(wn, WN_field_id(wn)+field_id + DECL_FIELD_ID(arg1));
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

#ifdef KEY
        if (!need_result) {
          if (TREE_SIDE_EFFECTS (TREE_OPERAND (exp, 0))) {
	    WFE_Stmt_Append (WN_CreateEval (wn0), Get_Srcpos ());
          }
	  wn = NULL;
          break;
        }
#endif

	TY_IDX hi_ty_idx = Get_TY(TREE_TYPE(exp));

	desc_ty_idx = component_ty_idx;
	if (desc_ty_idx == 0)
	  desc_ty_idx = hi_ty_idx;

#ifdef KEY
        TY_IDX iload_ty_idx = desc_ty_idx;
#endif

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
	  wn = WN_CreateIload(OPR_ILOAD, rtype,
			      is_bit_field ? MTYPE_BS : desc, 
			      component_offset+xtra_BE_ofst,
#ifndef KEY
			      field_id != 0 ? hi_ty_idx : ty_idx, 
#else
			      field_id != 0 ? hi_ty_idx : iload_ty_idx, 
#endif
			      Make_Pointer_Type (hi_ty_idx, FALSE),
			      wn0, field_id);
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
#if defined(TARG_PPC32)
	    if (WN_opcode(wn) == OPC_I4U4CVT ||
	        WN_opcode(wn) == OPC_I8U8CVT) {
#else
	    if (WN_opcode(wn) == OPC_I4U4CVT ||
	        WN_opcode(wn) == OPC_U4I4CVT ||
	        WN_opcode(wn) == OPC_I8U8CVT ||
	        WN_opcode(wn) == OPC_U8I8CVT) {
#endif
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
	TYPE_ID mtyp = Widen_Mtype(TY_mtype(ty_idx));
	wn = WN_Trunc(WN_rtype(wn0), mtyp, wn0);
      }
      break;
      
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
	TYPE_ID etype = TY_mtype(Get_TY(TREE_TYPE(exp)));
        wn0 = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
        wn1 = WFE_Expand_Expr (TREE_OPERAND (exp, 1));
        wn  = WN_Binary (Operator_From_Tree [code].opr, Widen_Mtype(etype), wn0, wn1);

#if defined(TARG_SL)
        if(WN_operator(wn) == OPR_ADD &&
           (WN_operator(WN_kid0(wn)) == OPR_LDA ||
               WN_operator(WN_kid(wn, 1)) == OPR_LDA)) 
        {
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


	if ((MTYPE_is_integral(etype)) &&
	    (Widen_Mtype(etype) != etype) &&
	    (TY_size (Get_TY(TREE_TYPE(exp))) < 32) &&
#ifdef KEY // bug 2649
	    (code == PLUS_EXPR || code == MINUS_EXPR || 
	    code == MULT_EXPR || code == LSHIFT_EXPR || 
	    code == BIT_XOR_EXPR || code == BIT_IOR_EXPR)
#else
	    (code == PLUS_EXPR || code == MINUS_EXPR || code == MULT_EXPR)
#endif
	    )
	  wn = WN_CreateCvtl(OPR_CVTL, Widen_Mtype(etype), MTYPE_V,
			     TY_size (Get_TY(TREE_TYPE(exp))) * 8, wn);

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
#ifdef KEY // bug 2651
        wn0 = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
#else
        wn0 = WFE_Expand_Expr_With_Sequence_Point (TREE_OPERAND (exp, 0),
						   Boolean_type);
#endif // KEY
        wn1 = WFE_Expand_Expr_With_Sequence_Point (TREE_OPERAND (exp, 1),
						   Boolean_type);
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
#ifdef TARG_NVISA
        if (expect_boolean
          && (Mtype_TransferSign(Boolean_type, mtyp) != Boolean_type))
        {
          // force mtype for comparison to be boolean (see select comment).
          mtyp = Boolean_type;
        }
#endif
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

#ifdef TARG_NVISA
        // want result to be boolean,
        // and later cvt then goes from boolean to mtyp.
        wn = WN_CreateExp2(Operator_From_Tree [code].opr, Boolean_type,
                           Widen_Mtype(mtyp0), wn0, wn1);
#else
	wn = WN_CreateExp2(Operator_From_Tree [code].opr, Widen_Mtype(mtyp),
			   Widen_Mtype(mtyp0), wn0, wn1);
#endif
#ifdef KEY
#ifdef TARG_SL // 32bit target
      if ( (Widen_Mtype(mtyp) != Boolean_type) && 
	  	   (!MTYPE_is_size_double(mtyp)))	  	        
#else
        if (Widen_Mtype(mtyp) != Boolean_type)
#endif	
	  wn = WN_Cvt(Boolean_type, Widen_Mtype(mtyp), wn);
#endif
      }
      break;

    case COND_EXPR:
      {
	ty_idx = Get_TY (TREE_TYPE(exp));
#ifdef KEY // bug 2645
#ifdef TARG_NVISA
        // Want to tell comparison that we expect a boolean here,
        // else when result is long in m64, it uses I8 for condition
        // which triggers cvt and later causes us to miss mtype_b optimization.
        // This appears to be a gcc bug that have to work around,
        // as IMO condition type should be boolean not result type
        // (and it already is for other result types).
        wn0 = WFE_Expand_Expr (TREE_OPERAND (exp, 0), TRUE, 0,0,0,0,FALSE,
                TRUE /* expect_boolean */);
#else
	wn0 = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
#endif
#else
	wn0 = WFE_Expand_Expr_With_Sequence_Point (TREE_OPERAND (exp, 0),
						   Boolean_type);
#endif
	if (TY_mtype (ty_idx) == MTYPE_V) {
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
	  wn1 = WFE_Expand_Expr_With_Sequence_Point (TREE_OPERAND (exp, 1),
						     TY_mtype (ty_idx));
	  wn2 = WFE_Expand_Expr_With_Sequence_Point (TREE_OPERAND (exp, 2),
						     TY_mtype (ty_idx));
	  wn  = WN_CreateExp3 (OPR_CSELECT, Mtype_comparison (TY_mtype (ty_idx)),
			   MTYPE_V, wn0, wn1, wn2);
	  Set_PU_has_very_high_whirl (Get_Current_PU ());
        }
      }
      break;

    case MODIFY_EXPR:
    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
      {
        wn1 = WFE_Expand_Expr (TREE_OPERAND (exp, 1)); // r.h.s.
#ifdef TARG_SL
	wn  = WFE_Lhs_Of_Modify_Expr(code, TREE_OPERAND (exp, 0), TREE_OPERAND(exp,1), need_result, 
#else
	wn  = WFE_Lhs_Of_Modify_Expr(code, TREE_OPERAND (exp, 0), need_result, 
#endif
				     0, 0, 0, FALSE, wn1, 0, FALSE, FALSE);
#if defined(TARG_SL)
        Mark_LDA_Vbuf_Offset(wn1,  INTRN_VBUF_OFFSET); 
#endif 

      }
      break;

    // ternary ops

    case BIT_FIELD_REF:
      {
        wn = WFE_Expand_Expr (TREE_OPERAND (exp, 0), TRUE, nop_ty_idx, 
			      component_ty_idx, component_offset,
			      field_id, FALSE);
#ifdef PATHSCALE_MERGE
#ifdef Is_True_On
	{
	WN* tmp = wn;
	while (WN_operator (tmp) == OPR_CVTL || WN_operator (tmp) == OPR_CVT) {
	  tmp = WN_kid0(tmp);
	}
	Is_True (WN_operator(tmp) == OPR_LDID || 
	         WN_operator(tmp) == OPR_LDBITS ||
	         WN_operator(tmp) == OPR_ILOAD ||
                 // begin - bug fix for OSP_178
                 WN_operator(tmp) == OPR_BAND ||
                 // end - bug fix for OSP_178
	         WN_operator(tmp) == OPR_ILDBITS, 
		 ("Not expected operator"));
	}
#endif
	INT bofst = Get_Integer_Value(TREE_OPERAND(exp, 2));
	INT bsiz =Get_Integer_Value(TREE_OPERAND(exp, 1));
#endif
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
            && WN_operator(WN_kid0(wn)) != OPR_ILOAD 
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
#ifdef PATHSCALE_MERGE
#ifdef TARG_IA64
	// bug fix for OSP_157 && OSP_178 && OSP_225 
	// Besides, solve the unaligned memory access triggered in:
	// 400.perlbench in spec2k6 and 253.perlbmk in spec2k,
	// one of the concrete example: "U4U1ILOAD 2 sym" into "U4U4ILOAD 2 sym".
	// If one symbol is allocated with A bytes alignment, compiler must access
	// it with load/store B, where B <= A. 
	// Please NOTE that MTYPE_bit_size(MTYPE_M) == 0.
	// Moreover, we should NOT modify it if desc equals to MTYPE_V,
	// like CVTL, it's descriptor type must be set MTYPE_V,
	//
	if (!MTYPE_is_void (WN_desc(wn)) &&
	    (!MTYPE_is_integral (WN_desc(wn)) || MTYPE_bit_size (WN_desc(wn)) >= bsiz)) 
	{
	  WN_set_desc (wn, desc);
	}
#else
	if (WN_desc(wn) != MTYPE_V)
	  WN_set_desc(wn, desc);
#endif
#endif
	if ((bsiz & 7) == 0 &&	// field size multiple of bytes
	    MTYPE_size_min(desc) % bsiz == 0 && // accessed loc multiple of bsiz
	    bofst % bsiz == 0) {		// bofst multiple of bsiz
	  // not really a bit-field extraction!
#ifdef PATHSCALE_MERGE
#ifdef TARG_IA64
	  BOOL change_desc = FALSE;
	  
	  
	  if (MTYPE_is_void (WN_desc(wn))) {
	     /* it make not sense to change the desc and it is illegal to do that.
	      *  (e.g. WN is CVTL.) */
	  } else if (!MTYPE_is_integral (WN_desc(wn))) {
	     /* change the type to integral mandatorily to ease both 
	      * analysis and convertion etc.  */
	     change_desc = TRUE;
	  } else {
	     /* We otherwise convert, say, "I4I2LDID 2 sym" into "I4I4LDID 2 sym".
	      * There will be alignment issue. */
	     change_desc = (MTYPE_bit_size (WN_desc(wn)) >= bsiz);
	  }

	  if (change_desc)
#else
	  if (WN_desc(wn) != MTYPE_V)
#endif
#endif
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
#ifdef PATHSCALE_MERGE
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
#ifdef PATHSCALE_MERGE
	  if (WN_operator(wn) == OPR_ILOAD || WN_operator(wn) == OPR_ILDBITS)
	    WN_set_load_addr_ty(wn, Make_Pointer_Type(ty));
#endif
	}
#else
	  WN_set_ty (wn, MTYPE_To_TY (WN_desc(wn)));
#endif // KEY
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
        wn = Adjust_Vbuf_Array_Ofst(wn);
       
#endif       // TARG_SL

      break;

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
        INT i;
	tree list;
	arg0 = TREE_OPERAND (exp, 0);
	enum tree_code code0 = TREE_CODE (arg0);
	if (LANG_Ansi_Setjmp_On == FALSE) {
	  if (current_function_calls_setjmp)
	    Set_PU_calls_setjmp (Get_Current_PU ());
	  if (current_function_calls_longjmp)
	    Set_PU_calls_longjmp (Get_Current_PU ());
	}
	for (list = TREE_OPERAND (exp, 1); list; list = TREE_CHAIN (list)) {
	  if (TREE_CODE(TREE_VALUE(list)) == ERROR_MARK)
	    exit (RC_USER_ERROR);
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

        // If intrinsic or whirl_generated,
        // always need ret_mtype to find matching pattern;
        // if not used the optimizer will delete the intrinsic.
        // But for regular calls we use void return if not used.
        // Because there are lots of intrinsic and whirl cases,
        // we start by defining ret_mtype, then undefine later for calls.
        // Could now undo the checks for V ret_mtype that KEY added,
        // but leave them in for now (no harm).
        //if (need_result) {
	  if (!WFE_Keep_Zero_Length_Structs  &&
              TY_mtype (ty_idx) == MTYPE_M   &&
              TY_size (ty_idx) == 0) {
            // zero length struct return
            ret_mtype = MTYPE_V;
          }
          else
            ret_mtype = TY_mtype (ty_idx);
        //}
        //else
        //  ret_mtype = MTYPE_V;
        st = NULL;
        if (code0 == ADDR_EXPR                  &&
            TREE_CODE (TREE_OPERAND (arg0, 0))) {
	  tree func = TREE_OPERAND (arg0, 0);
	  BOOL intrinsic_op = FALSE;
          BOOL whirl_generated = FALSE;
          BOOL asm_generated = FALSE;
#ifdef KEY
	  // bug 8251: If we forcibly change the return type, we should
	  // generate a CVT.
	  TYPE_ID cvt_to = MTYPE_UNKNOWN;
#endif // KEY
          
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
#ifdef TARG_PPC32
		arg1 = TREE_VALUE (arglist);
		TY_IDX va_ty_index = WFE_Load_Va_List_PPC32(arg1);
		
		static int va_count;

		va_count++;
		int fix_int = 0, fix_float = 0, int_start_offset, float_start_offset;
		
		ST * pu = Get_Current_PU_ST();
		WN * pu_tree = CURRENT_PU_WN;
		
		arg2 = TREE_VALUE (TREE_CHAIN (arglist));
		while (TREE_CODE (arg2) == NOP_EXPR
		       || TREE_CODE (arg2) == CONVERT_EXPR
		       || TREE_CODE (arg2) == NON_LVALUE_EXPR
		       || TREE_CODE (arg2) == INDIRECT_REF)
		  arg2 = TREE_OPERAND (arg2, 0);
		
		ST * st_arg2 = Get_ST(arg2);
		TYPE_ID ty_arg2 = TY_mtype(ST_type(st_arg2));
		int va_arg_begin = -1;
		
		// Get fix formal integer/float parameter number
		for (int i = 0; i < WN_num_formals(pu_tree); i++) {
			ST * stf  = WN_st(WN_formal(pu_tree, i));
			TY_IDX ty = ST_type(stf);
			TYPE_ID tid = TY_mtype (ty);		  	  
			if (MTYPE_float(tid)) {
				fix_float++;
			}
			else if (MTYPE_is_integral(tid) && MTYPE_is_size_double(tid)) {	// longlong type
				fix_int += 2;
				if (i & 0x1) 
					fix_int++;
			}
			else {		// struct value, union value, and all others are treated as integer
				fix_int++;
			}
			if (st_arg2 == stf) {
				FmtAssert((va_arg_begin == -1), ("VA_START here"));
				if (MTYPE_float(ty_arg2)) {
					va_arg_begin = fix_float;
				}
				else {
					va_arg_begin = fix_int;
				}
				break;
			}    
		}
		FmtAssert((va_arg_begin != -1), ("va_start can not find 2nd parameter in formal parameter list"));
		FLD_HANDLE gpr = TY_fld(va_ty_index);
		wn = WN_Istore(TY_mtype(FLD_type(gpr)), FLD_ofst(gpr), Make_Pointer_Type(va_ty_index, FALSE), 
			WN_Ldid(Pointer_Mtype, ST_ofst(ap_tmp), ap_tmp, Make_Pointer_Type(va_ty_index, FALSE)), 
			Make_Integer_Const(MTYPE_I4, (fix_int>=8) ? 8 : fix_int), 1);
		WFE_Stmt_Append (wn, Get_Srcpos());
		
		FLD_HANDLE fpr = FLD_next(gpr);
		wn = WN_Istore(TY_mtype(FLD_type(fpr)), FLD_ofst(fpr), Make_Pointer_Type(va_ty_index, FALSE), 
			WN_Ldid(Pointer_Mtype, ST_ofst(ap_tmp), ap_tmp, Make_Pointer_Type(va_ty_index, FALSE)), 
			Make_Integer_Const(MTYPE_I4, (fix_float>=8) ? 8: fix_float), 2);
		WFE_Stmt_Append (wn, Get_Srcpos());
		
		FLD_HANDLE overflow_arg_area = FLD_next(fpr);
		INT32 overflow_start_ofst = 8;
		if (fix_int > 8) {
			overflow_start_ofst += MTYPE_byte_size(MTYPE_I4) * (fix_int - 8);
		}
		if (fix_float > 8) {
			overflow_start_ofst += MTYPE_byte_size(MTYPE_F8) * (fix_float - 8);
		}
		WN* ofstWN = WN_Add(Pointer_Mtype, WN_LdidPreg(Pointer_Mtype, 65), 
			Make_Integer_Const(MTYPE_I4, overflow_start_ofst));
		wn = WN_Istore(TY_mtype(FLD_type(overflow_arg_area)), FLD_ofst(overflow_arg_area), Make_Pointer_Type(va_ty_index, FALSE), 
			WN_Ldid(Pointer_Mtype, ST_ofst(ap_tmp), ap_tmp, Make_Pointer_Type(va_ty_index, FALSE)), ofstWN, 3);
		WFE_Stmt_Append (wn, Get_Srcpos());

		FLD_HANDLE reg_save_area = FLD_next(overflow_arg_area);
		ofstWN = WN_Add(Pointer_Mtype, WN_LdidPreg(Pointer_Mtype, 66), 
			Make_Integer_Const(MTYPE_I4, 1));
		wn = WN_Istore(TY_mtype(FLD_type(reg_save_area)), FLD_ofst(reg_save_area), Make_Pointer_Type(va_ty_index, FALSE), 
			WN_Ldid(Pointer_Mtype, ST_ofst(ap_tmp), ap_tmp, Make_Pointer_Type(va_ty_index, FALSE)), ofstWN, 4);
		WFE_Stmt_Append (wn, Get_Srcpos());
		whirl_generated = TRUE;
		wn = NULL;
		break;
#endif
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

#ifndef KEY 	// Does not exist any more
	      case BUILT_IN_VARARGS_START:
	      {
		arg1 = TREE_VALUE (arglist);
		WN *arg_wn = WFE_Expand_Expr (arg1);
		wn = WN_Lda (Pointer_Mtype, 0, WFE_Vararg_Start_ST);
		wn = WN_Stid (Pointer_Mtype, WN_offset (arg_wn),
			      WN_st (arg_wn), arg_ty_idx, wn);

		WFE_Stmt_Append (wn, Get_Srcpos());
		whirl_generated = TRUE;
		wn = NULL;
		break;
	      }
#endif // KEY

	      case BUILT_IN_VA_COPY:
	      {
		arg1 = TREE_VALUE (arglist);
		arg2 = TREE_VALUE (TREE_CHAIN (arglist));
		TY_IDX arg_ty_idx = Get_TY (TREE_TYPE (arg1));
#ifdef TARG_PPC32
		WN* addr = WFE_Expand_Expr( arg1 );
		WN* value = WFE_Expand_Expr( arg2 );
		TY_IDX va_ty_idx = Find_Vararg_TY_IDX_PPC32();
		wn = WN_Iload(MTYPE_M, 0, va_ty_idx, value);
		wn = WN_Istore(MTYPE_M, 0, Make_Pointer_Type(va_ty_idx, FALSE), addr, wn);
		WFE_Stmt_Append( wn, Get_Srcpos() );
              whirl_generated = TRUE;
		wn = NULL;
		break;
#endif
#ifdef KEY
		/* Under -m32, convert a __builtin_va_copy to an assignment if the
		   type of va_list is not array.
		   Also, the original code seems to only work for -m64, like other
		   va_XYZ code; under -m32, the source address is wrong.  (bug#2601)
		   (But even under -m64, the using of memcpy is unnecessary.)
		 */
#if defined(TARG_X8664) || defined(TARG_SL) || defined(TARG_MIPS) || defined(TARG_LOONGSON)
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
                tree last_parm = tree_last (DECL_ARGUMENTS (current_function_decl));
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

#ifdef TARG_SL
	      case BUILT_IN_BZERO:
                iopc = INTRN_BZERO;
                break;
#endif

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
	
#ifdef KEY // bug 4872
	      case BUILT_IN_STRNCPY:
	        iopc = INTRN_STRNCPY;
		break;
#endif // KEY

              case BUILT_IN_STRCMP:
		if (arglist == 0
		    /* Arg could be non-pointer if user redeclared this fcn wrong.  */
		    || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != POINTER_TYPE
		    || TREE_CHAIN (arglist) == 0
		    || TREE_CODE (TREE_TYPE (TREE_VALUE (TREE_CHAIN (arglist)))) != POINTER_TYPE)
		  break;
		else {
		  arg1 = TREE_VALUE (arglist);
		  arg2 = TREE_VALUE (TREE_CHAIN (arglist));
		  tree len1 = c_strlen_exported (arg1);
		  if (len1) {
		    tree len2 = c_strlen_exported (arg2);
#if !defined(TARG_PPC32)        // contains bug
		    if (len2) {
		      char *ptr1 = get_string_pointer (WFE_Expand_Expr (arg1));
		      char *ptr2 = get_string_pointer (WFE_Expand_Expr (arg2));
		      if (ptr1 && ptr2) { //if not, we should delete the code of arg1 and arg2
			wn = WN_Intconst (MTYPE_I4,
					  strcmp (ptr1, ptr2));
			whirl_generated = TRUE;
			break;
		      }
		    }
#endif        
		  }
		  iopc = INTRN_STRCMP;
//		  intrinsic_op = TRUE;
		}
                break;

              case BUILT_IN_STRLEN:
		if (arglist == 0
		/* Arg could be non-pointer if user redeclared this fcn wrong.  */
		   || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != POINTER_TYPE)
		  break;
		else {
		  tree src = TREE_VALUE (arglist);
		  tree len = c_strlen_exported (src);
		  if (len) {
		    wn = WFE_Expand_Expr (len);
		    whirl_generated = TRUE;
		  }
		  else {
		    iopc = INTRN_STRLEN;
//		    intrinsic_op = TRUE;
		  }
		}
                break;
#ifndef TARG_PPC32 // turn off for the timing being due to bug 
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

#ifdef TARG_NVISA
            case BUILT_IN_CEIL:
              arg_wn = WFE_Expand_Expr (TREE_VALUE (TREE_OPERAND (exp, 1)));
              wn = WN_CreateExp1 (OPR_CEIL, ret_mtype, MTYPE_F8, arg_wn);
              whirl_generated = TRUE;
              break;
            case BUILT_IN_CEILF:
              arg_wn = WFE_Expand_Expr (TREE_VALUE (TREE_OPERAND (exp, 1)));
              wn = WN_CreateExp1 (OPR_CEIL, ret_mtype, MTYPE_F4, arg_wn);
              whirl_generated = TRUE;
              break;
            case BUILT_IN_ROUND:
              arg_wn = WFE_Expand_Expr (TREE_VALUE (TREE_OPERAND (exp, 1)));
              wn = WN_CreateExp1 (OPR_RND, ret_mtype, MTYPE_F8, arg_wn);
              whirl_generated = TRUE;
              break;
            case BUILT_IN_ROUNDF:
              arg_wn = WFE_Expand_Expr (TREE_VALUE (TREE_OPERAND (exp, 1)));
              wn = WN_CreateExp1 (OPR_RND, ret_mtype, MTYPE_F4, arg_wn);
              whirl_generated = TRUE;
              break;
            case BUILT_IN_TRUNC:
              arg_wn = WFE_Expand_Expr (TREE_VALUE (TREE_OPERAND (exp, 1)));
              wn = WN_CreateExp1 (OPR_TRUNC, ret_mtype, MTYPE_F8, arg_wn);
              whirl_generated = TRUE;
              break;
            case BUILT_IN_TRUNCF:
              arg_wn = WFE_Expand_Expr (TREE_VALUE (TREE_OPERAND (exp, 1)));
              wn = WN_CreateExp1 (OPR_TRUNC, ret_mtype, MTYPE_F4, arg_wn);
              whirl_generated = TRUE;
              break;

            case BUILT_IN_MIN:
            case BUILT_IN_UMIN:
            case BUILT_IN_FMINF:
            case BUILT_IN_FMIN:
              arg1 = TREE_VALUE (arglist);
              arg2 = TREE_VALUE (TREE_CHAIN (arglist));
              wn = WN_CreateExp2 (OPR_MIN, ret_mtype, MTYPE_V,
                      WFE_Expand_Expr (arg1), WFE_Expand_Expr(arg2) );
              whirl_generated = TRUE;
              break;
            case BUILT_IN_MAX:
            case BUILT_IN_UMAX:
            case BUILT_IN_FMAXF:
            case BUILT_IN_FMAX:
              arg1 = TREE_VALUE (arglist);
              arg2 = TREE_VALUE (TREE_CHAIN (arglist));
              wn = WN_CreateExp2 (OPR_MAX, ret_mtype, MTYPE_V,
                      WFE_Expand_Expr (arg1), WFE_Expand_Expr(arg2) );
              whirl_generated = TRUE;
              break;
            case BUILT_IN_MULHI:
            case BUILT_IN_UMULHI:
            case BUILT_IN_MUL64HI:
            case BUILT_IN_UMUL64HI:
              arg1 = TREE_VALUE (arglist);
              arg2 = TREE_VALUE (TREE_CHAIN (arglist));
              wn = WN_CreateExp2 (OPR_HIGHMPY, ret_mtype, MTYPE_V,
                      WFE_Expand_Expr (arg1), WFE_Expand_Expr(arg2) );
              whirl_generated = TRUE;
              break;
            case BUILT_IN_INT2FLOAT:
            case BUILT_IN_FLOAT2INT:
            case BUILT_IN_LONGLONG2DOUBLE:
            case BUILT_IN_DOUBLE2LONGLONG:
              arg_wn = WFE_Expand_Expr (TREE_VALUE (TREE_OPERAND (exp, 1)));
              wn = WN_Tas (ret_mtype, MTYPE_To_TY(ret_mtype), arg_wn);
              whirl_generated = TRUE;
              break;
#endif
#endif // ifndef TARG_PPC32
#ifdef TARG_NVISA
              case BUILT_IN_SQRT:
              case BUILT_IN_SQRTF:      // newer gcc name
                // nvisa wants to see whirl opcode, not errno usage
#else
#ifdef KEY
	      case BUILT_IN_SQRT:
		if( flag_errno_math ){
		  break;
		}
#else
	      case BUILT_IN_FSQRT:
#endif
#endif
                arg_wn = WFE_Expand_Expr (TREE_VALUE (TREE_OPERAND (exp, 1)));
                wn = WN_CreateExp1 (OPR_SQRT, ret_mtype, MTYPE_V, arg_wn);
                whirl_generated = TRUE;
                break;

#ifdef TARG_NVISA
		/* confict with bug 3390
	      case BUILT_IN_EXPF:
		iopc = INTRN_F4EXP;
		intrinsic_op = TRUE;
		break;
		*/
              case BUILT_IN_GETSHAREDMEM:
                iopc =   INTRN_GETSHAREDMEM;
                intrinsic_op = TRUE;
		break;
              case BUILT_IN_GETBLOCKIDPTR:
                iopc =   INTRN_GETBLOCKIDPTR;
                intrinsic_op = TRUE;
		break;
              case BUILT_IN_RSQRT:
              case BUILT_IN_RSQRTF:
                arg_wn = WFE_Expand_Expr (TREE_VALUE (TREE_OPERAND (exp, 1)));
                wn = WN_CreateExp1 (OPR_RSQRT, ret_mtype, MTYPE_V, arg_wn);
                whirl_generated = TRUE;
                break;
              case BUILT_IN_SINF:
                iopc = INTRN_F4SIN;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_COSF:
                iopc = INTRN_F4COS;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_EXP2F:
                iopc = INTRN_F4EXP2;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_EXP2:
                iopc = INTRN_F8EXP2;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_LOG2F:
                iopc = INTRN_F4LOG2;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_LOG2:
                iopc = INTRN_F8LOG2;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_SATURATE:
                iopc = INTRN_F8SATURATE;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_SATURATEF:
                iopc = INTRN_F4SATURATE;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_MUL24:
                iopc = INTRN_MUL24;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_UMUL24:
                iopc = INTRN_UMUL24;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_F4ADD_ROUND:
                iopc = INTRN_F4ADD_ROUND;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_F4ADD_TRUNC:
                iopc = INTRN_F4ADD_TRUNC;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_F8ADD_ROUND:
                iopc = INTRN_F8ADD_ROUND;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_F8ADD_TRUNC:
                iopc = INTRN_F8ADD_TRUNC;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_F8ADD_FLOOR:
                iopc = INTRN_F8ADD_FLOOR;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_F8ADD_CEIL:
                iopc = INTRN_F8ADD_CEIL;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_F4MUL_ROUND:
                iopc = INTRN_F4MUL_ROUND;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_F4MUL_TRUNC:
                iopc = INTRN_F4MUL_TRUNC;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_F8MUL_ROUND:
                iopc = INTRN_F8MUL_ROUND;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_F8MUL_TRUNC:
                iopc = INTRN_F8MUL_TRUNC;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_F8MUL_FLOOR:
                iopc = INTRN_F8MUL_FLOOR;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_F8MUL_CEIL:
                iopc = INTRN_F8MUL_CEIL;
                intrinsic_op = TRUE;
                break;

              // fma could become whirl MADD,
              // but purpose of intrinsic is to guarantee it stays same,
              // not optimized with other code, so leave as intrinsic
              case BUILT_IN_FMA_ROUND:
                iopc = INTRN_F8MA_ROUND;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_FMA_TRUNC:
                iopc = INTRN_F8MA_TRUNC;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_FMA_FLOOR:
                iopc = INTRN_F8MA_FLOOR;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_FMA_CEIL:
                iopc = INTRN_F8MA_CEIL;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_FMAF:
                iopc = INTRN_F4MA;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_SAD:
                iopc = INTRN_I4SAD;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_USAD:
                iopc = INTRN_U4SAD;
                intrinsic_op = TRUE;
                break;

              case BUILT_IN_HILOINT_TO_DOUBLE:
                iopc = INTRN_F8HLI2D;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_DOUBLE_TO_LOINT:
                iopc = INTRN_I4D2LI;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_DOUBLE_TO_HIINT:
                iopc = INTRN_I4D2HI;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_DOUBLE_TO_FLOAT_ROUND:
                iopc = INTRN_F4F8CVT_ROUND;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_DOUBLE_TO_FLOAT_TRUNC:
                iopc = INTRN_F4F8CVT_TRUNC;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_DOUBLE_TO_FLOAT_CEIL:
                iopc = INTRN_F4F8CVT_CEIL;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_DOUBLE_TO_FLOAT_FLOOR:
                iopc = INTRN_F4F8CVT_FLOOR;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_DOUBLE_TO_INT_ROUND:
                iopc = INTRN_I4F8CVT_ROUND;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_DOUBLE_TO_INT_TRUNC:
                iopc = INTRN_I4F8CVT_TRUNC;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_DOUBLE_TO_INT_CEIL:
                iopc = INTRN_I4F8CVT_CEIL;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_DOUBLE_TO_INT_FLOOR:
                iopc = INTRN_I4F8CVT_FLOOR;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_DOUBLE_TO_UINT_ROUND:
                iopc = INTRN_U4F8CVT_ROUND;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_DOUBLE_TO_UINT_TRUNC:
                iopc = INTRN_U4F8CVT_TRUNC;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_DOUBLE_TO_UINT_CEIL:
                iopc = INTRN_U4F8CVT_CEIL;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_DOUBLE_TO_UINT_FLOOR:
                iopc = INTRN_U4F8CVT_FLOOR;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_INT_TO_DOUBLE_ROUND:
                iopc = INTRN_F8I4CVT_ROUND;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_INT_TO_DOUBLE_TRUNC:
                iopc = INTRN_F8I4CVT_TRUNC;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_INT_TO_DOUBLE_FLOOR:
                iopc = INTRN_F8I4CVT_FLOOR;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_INT_TO_DOUBLE_CEIL:
                iopc = INTRN_F8I4CVT_CEIL;
                intrinsic_op = TRUE;
                break;
             case BUILT_IN_UINT_TO_DOUBLE_ROUND:
               iopc = INTRN_F8U4CVT_ROUND;
               intrinsic_op = TRUE;
               break;
             case BUILT_IN_UINT_TO_DOUBLE_TRUNC:
               iopc = INTRN_F8U4CVT_TRUNC;
               intrinsic_op = TRUE;
               break;
             case BUILT_IN_UINT_TO_DOUBLE_FLOOR:
               iopc = INTRN_F8U4CVT_FLOOR;
               intrinsic_op = TRUE;
               break;
             case BUILT_IN_UINT_TO_DOUBLE_CEIL:
               iopc = INTRN_F8U4CVT_CEIL;
               intrinsic_op = TRUE;
               break;
             case BUILT_IN_INT_TO_FLOAT_ROUND:
               iopc = INTRN_F4I4CVT_ROUND;
               intrinsic_op = TRUE;
               break;
             case BUILT_IN_INT_TO_FLOAT_TRUNC:
               iopc = INTRN_F4I4CVT_TRUNC;
               intrinsic_op = TRUE;
               break;
             case BUILT_IN_INT_TO_FLOAT_FLOOR:
               iopc = INTRN_F4I4CVT_FLOOR;
               intrinsic_op = TRUE;
               break;
             case BUILT_IN_INT_TO_FLOAT_CEIL:
               iopc = INTRN_F4I4CVT_CEIL;
               intrinsic_op = TRUE;
               break;
             case BUILT_IN_UINT_TO_FLOAT_ROUND:
               iopc = INTRN_F4U4CVT_ROUND;
               intrinsic_op = TRUE;
               break;
             case BUILT_IN_UINT_TO_FLOAT_TRUNC:
               iopc = INTRN_F4U4CVT_TRUNC;
               intrinsic_op = TRUE;
               break;
             case BUILT_IN_UINT_TO_FLOAT_FLOOR:
               iopc = INTRN_F4U4CVT_FLOOR;
               intrinsic_op = TRUE;
               break;
              case BUILT_IN_UINT_TO_FLOAT_CEIL:
                iopc = INTRN_F4U4CVT_CEIL;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_FLOAT_TO_INT_ROUND:
                iopc = INTRN_I4F4CVT_ROUND;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_FLOAT_TO_INT_TRUNC:
                iopc = INTRN_I4F4CVT_TRUNC;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_FLOAT_TO_INT_FLOOR:
                iopc = INTRN_I4F4CVT_FLOOR;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_FLOAT_TO_INT_CEIL:
                iopc = INTRN_I4F4CVT_CEIL;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_FLOAT_TO_UINT_ROUND:
                iopc = INTRN_U4F4CVT_ROUND;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_FLOAT_TO_UINT_TRUNC:
                iopc = INTRN_U4F4CVT_TRUNC;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_FLOAT_TO_UINT_FLOOR:
                iopc = INTRN_U4F4CVT_FLOOR;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_FLOAT_TO_UINT_CEIL:
                iopc = INTRN_U4F4CVT_CEIL;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_FLOAT_TO_LONGLONG_ROUND:
                iopc = INTRN_I8F4CVT_ROUND;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_FLOAT_TO_LONGLONG_TRUNC:
                iopc = INTRN_I8F4CVT_TRUNC;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_FLOAT_TO_LONGLONG_FLOOR:
                iopc = INTRN_I8F4CVT_FLOOR;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_FLOAT_TO_LONGLONG_CEIL:
                iopc = INTRN_I8F4CVT_CEIL;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_FLOAT_TO_ULONGLONG_ROUND:
                iopc = INTRN_U8F4CVT_ROUND;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_FLOAT_TO_ULONGLONG_TRUNC:
                iopc = INTRN_U8F4CVT_TRUNC;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_FLOAT_TO_ULONGLONG_FLOOR:
                iopc = INTRN_U8F4CVT_FLOOR;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_FLOAT_TO_ULONGLONG_CEIL:
                iopc = INTRN_U8F4CVT_CEIL;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_DOUBLE_TO_LONGLONG_ROUND:
                iopc = INTRN_I8F8CVT_ROUND;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_DOUBLE_TO_LONGLONG_TRUNC:
                iopc = INTRN_I8F8CVT_TRUNC;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_DOUBLE_TO_LONGLONG_FLOOR:
                iopc = INTRN_I8F8CVT_FLOOR;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_DOUBLE_TO_LONGLONG_CEIL:
                iopc = INTRN_I8F8CVT_CEIL;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_DOUBLE_TO_ULONGLONG_ROUND:
                iopc = INTRN_U8F8CVT_ROUND;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_DOUBLE_TO_ULONGLONG_TRUNC:
                iopc = INTRN_U8F8CVT_TRUNC;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_DOUBLE_TO_ULONGLONG_FLOOR:
                iopc = INTRN_U8F8CVT_FLOOR;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_DOUBLE_TO_ULONGLONG_CEIL:
                iopc = INTRN_U8F8CVT_CEIL;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_LONGLONG_TO_FLOAT_ROUND:
                iopc = INTRN_F4I8CVT_ROUND;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_LONGLONG_TO_FLOAT_TRUNC:
                iopc = INTRN_F4I8CVT_TRUNC;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_LONGLONG_TO_FLOAT_FLOOR:
                iopc = INTRN_F4I8CVT_FLOOR;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_LONGLONG_TO_FLOAT_CEIL:
                iopc = INTRN_F4I8CVT_CEIL;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_ULONGLONG_TO_FLOAT_ROUND:
                iopc = INTRN_F4U8CVT_ROUND;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_ULONGLONG_TO_FLOAT_TRUNC:
                iopc = INTRN_F4U8CVT_TRUNC;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_ULONGLONG_TO_FLOAT_FLOOR:
                iopc = INTRN_F4U8CVT_FLOOR;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_ULONGLONG_TO_FLOAT_CEIL:
                iopc = INTRN_F4U8CVT_CEIL;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_LONGLONG_TO_DOUBLE_ROUND:
                iopc = INTRN_F8I8CVT_ROUND;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_LONGLONG_TO_DOUBLE_TRUNC:
                iopc = INTRN_F8I8CVT_TRUNC;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_LONGLONG_TO_DOUBLE_FLOOR:
                iopc = INTRN_F8I8CVT_FLOOR;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_LONGLONG_TO_DOUBLE_CEIL:
                iopc = INTRN_F8I8CVT_CEIL;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_ULONGLONG_TO_DOUBLE_ROUND:
                iopc = INTRN_F8U8CVT_ROUND;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_ULONGLONG_TO_DOUBLE_TRUNC:
                iopc = INTRN_F8U8CVT_TRUNC;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_ULONGLONG_TO_DOUBLE_FLOOR:
                iopc = INTRN_F8U8CVT_FLOOR;
                intrinsic_op = TRUE;
                break;
              case BUILT_IN_ULONGLONG_TO_DOUBLE_CEIL:
                iopc = INTRN_F8U8CVT_CEIL;
                intrinsic_op = TRUE;
                break;
#endif

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
		// See comments below.
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
		// See comments below.
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


#ifdef TARG_NVISA
              case BUILT_IN_SYNCHRONIZE:
                emit_builtin_synchronize (exp, num_args);
                whirl_generated = TRUE;
                break;
              case BUILT_IN_BRKPT:
                emit_builtin_brkpt ();
                whirl_generated = TRUE;
                break;
              case BUILT_IN_TRAP:
                emit_builtin_trap ();
                whirl_generated = TRUE;
                break;
              case BUILT_IN_CLOCK:
                // make it a call not an op cause not pure
                wn = emit_builtin_clock ();
                whirl_generated = TRUE;
                break;
              case BUILT_IN_IATOMICADD:
                wn = emit_builtin_atomic (exp, 2, MTYPE_I4, INTRN_I4ATOMICADD);
                whirl_generated = TRUE;
                break;
              case BUILT_IN_UATOMICADD:
                wn = emit_builtin_atomic (exp, 2, MTYPE_U4, INTRN_U4ATOMICADD);
                whirl_generated = TRUE;
                break;
              case BUILT_IN_ULLATOMICADD:
                wn = emit_builtin_atomic (exp, 2, MTYPE_U8, INTRN_U8ATOMICADD);
                whirl_generated = TRUE;
                break;
              case BUILT_IN_FATOMICADD:
                wn = emit_builtin_atomic (exp, 2, MTYPE_F4, INTRN_F4ATOMICADD);
                whirl_generated = TRUE;
                break;
              case BUILT_IN_DATOMICADD:
                wn = emit_builtin_atomic (exp, 2, MTYPE_F8, INTRN_F8ATOMICADD);
                whirl_generated = TRUE;
                break;
              case BUILT_IN_IATOMICMIN:
                wn = emit_builtin_atomic (exp, 2, MTYPE_I4, INTRN_I4ATOMICMIN);
                whirl_generated = TRUE;
                break;
              case BUILT_IN_UATOMICMIN:
                wn = emit_builtin_atomic (exp, 2, MTYPE_U4, INTRN_U4ATOMICMIN);
                whirl_generated = TRUE;
                break;
              case BUILT_IN_FATOMICMIN:
                wn = emit_builtin_atomic (exp, 2, MTYPE_F4, INTRN_F4ATOMICMIN);
                whirl_generated = TRUE;
                break;
              case BUILT_IN_IATOMICMAX:
                wn = emit_builtin_atomic (exp, 2, MTYPE_I4, INTRN_I4ATOMICMAX);
                whirl_generated = TRUE;
                break;
              case BUILT_IN_UATOMICMAX:
                wn = emit_builtin_atomic (exp, 2, MTYPE_U4, INTRN_U4ATOMICMAX);
                whirl_generated = TRUE;
                break;
              case BUILT_IN_FATOMICMAX:
                wn = emit_builtin_atomic (exp, 2, MTYPE_F4, INTRN_F4ATOMICMAX);
                whirl_generated = TRUE;
                break;
              case BUILT_IN_IATOMICEXCH:
                wn = emit_builtin_atomic (exp, 2, MTYPE_I4, INTRN_I4ATOMICEXCH);
                whirl_generated = TRUE;
                break;
              case BUILT_IN_UATOMICEXCH:
                wn = emit_builtin_atomic (exp, 2, MTYPE_U4, INTRN_U4ATOMICEXCH);
                whirl_generated = TRUE;
                break;
              case BUILT_IN_ULLATOMICEXCH:
                wn = emit_builtin_atomic (exp, 2, MTYPE_U8, INTRN_U8ATOMICEXCH);
                whirl_generated = TRUE;
                break;
              case BUILT_IN_FATOMICEXCH:
                wn = emit_builtin_atomic (exp, 2, MTYPE_F4, INTRN_F4ATOMICEXCH);
                whirl_generated = TRUE;
                break;
              case BUILT_IN_DATOMICEXCH:
                wn = emit_builtin_atomic (exp, 2, MTYPE_F8, INTRN_F8ATOMICEXCH);
                whirl_generated = TRUE;
                break;
              case BUILT_IN_IATOMICAND:
              case BUILT_IN_UATOMICAND:
                wn = emit_builtin_atomic (exp, 2, MTYPE_U4, INTRN_U4ATOMICAND);
                whirl_generated = TRUE;
                break;
              case BUILT_IN_IATOMICOR:
              case BUILT_IN_UATOMICOR:
                wn = emit_builtin_atomic (exp, 2, MTYPE_U4, INTRN_U4ATOMICOR);
                whirl_generated = TRUE;
                break;
              case BUILT_IN_IATOMICXOR:
              case BUILT_IN_UATOMICXOR:
                wn = emit_builtin_atomic (exp, 2, MTYPE_U4, INTRN_U4ATOMICXOR);
                whirl_generated = TRUE;
                break;
              case BUILT_IN_UATOMICINC:
                wn = emit_builtin_atomic (exp, 2, MTYPE_U4, INTRN_U4ATOMICINC);
                whirl_generated = TRUE;
                break;
              case BUILT_IN_UATOMICDEC:
                wn = emit_builtin_atomic (exp, 2, MTYPE_U4, INTRN_U4ATOMICDEC);
                whirl_generated = TRUE;
                break;
              case BUILT_IN_IATOMICCAS:
                wn = emit_builtin_atomic (exp, 3, MTYPE_I4, INTRN_I4ATOMICCAS);
                whirl_generated = TRUE;
                break;
              case BUILT_IN_UATOMICCAS:
                wn = emit_builtin_atomic (exp, 3, MTYPE_U4, INTRN_U4ATOMICCAS);
                whirl_generated = TRUE;
                break;
              case BUILT_IN_ULLATOMICCAS:
                wn = emit_builtin_atomic (exp, 3, MTYPE_U8, INTRN_U8ATOMICCAS);
                whirl_generated = TRUE;
                break;
              case BUILT_IN_FATOMICCAS:
                wn = emit_builtin_atomic (exp, 3, MTYPE_F4, INTRN_F4ATOMICCAS);
                whirl_generated = TRUE;
                break;
              case BUILT_IN_DATOMICCAS:
                wn = emit_builtin_atomic (exp, 3, MTYPE_F8, INTRN_F8ATOMICCAS);
                whirl_generated = TRUE;
                break;

              case BUILT_IN_ALL:
                wn = emit_builtin_vote (exp, INTRN_VOTEALL);
                whirl_generated = TRUE;
                break;
              case BUILT_IN_ANY:
                wn = emit_builtin_vote (exp, INTRN_VOTEANY);
                whirl_generated = TRUE;
                break;

#endif

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
#ifdef TARG_PPC32
                wn = WN_Lda (Pointer_Mtype, 0, tmpst);
#else
		  wn = WN_Lda (Pointer_Mtype, 0, tmpst, 
			       Make_Pointer_Type (ST_type(tmpst), FALSE));
#endif

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
#endif

	      case BUILT_IN_EXPECT:
	      {
		// __builtin_expect has two arguments
		// the first argument is the value to be returned
		// the second value is the expected value for branch prediction
/*
		WN *arg1, *arg2;
		list = TREE_OPERAND (exp, 1);
		arg1 = WFE_Expand_Expr (TREE_VALUE (list));
		list = TREE_CHAIN (list);
		arg2 = WFE_Expand_Expr (TREE_VALUE (list));
		wn   = WN_Relational (OPR_EQ, WN_rtype (arg1), arg1, arg2);
*/
#ifdef KEY
                iopc = INTRN_EXPECT;
                intrinsic_op = TRUE;
                if (ret_mtype == MTYPE_V) ret_mtype = MTYPE_I4; // bug 12344
#else
                list = TREE_OPERAND (exp, 1);
                wn   = WFE_Expand_Expr (TREE_VALUE (list));
                whirl_generated = TRUE;
#endif
		break;
	      }

	      case BUILT_IN_FFS:
		iopc = INTRN_I4FFS;
		intrinsic_op = TRUE;
                if (ret_mtype == MTYPE_V)
                  ret_mtype = MTYPE_I4;
		break;
	      case BUILT_IN_CTYPE_B_LOC: 
	        iopc = INTRN_CTYPE_B_LOC; 
		intrinsic_op = TRUE; 
		break;

	      case BUILT_IN_CTYPE_TOUPPER_LOC: 
	        iopc = INTRN_CTYPE_TOUPPER_LOC; 
		intrinsic_op = TRUE; 
		break;

	      case BUILT_IN_CTYPE_TOLOWER_LOC: 
	        iopc = INTRN_CTYPE_TOLOWER_LOC; 
		intrinsic_op = TRUE; 
		break;

#ifdef KEY
	      case BUILT_IN_EXTEND_POINTER:
	        wn = WFE_Expand_Expr (TREE_VALUE (TREE_OPERAND (exp, 1)));
		whirl_generated = TRUE;
		break;

#ifndef TARG_NVISA // handle elsewhere
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
#endif

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
#endif // KEY

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
              case BUILT_IN_TAN:
                // return type should only be F8 for tan
                if (ret_mtype == MTYPE_F8)
                {
                  iopc = INTRN_TAN;
                  intrinsic_op = TRUE;
                }
                break;
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
            case BUILT_IN_C3_INIT_ACC:
              iopc = INTRN_C3_INIT_ACC;
              break;
            case BUILT_IN_C3_SAVE_ACC:
              iopc = INTRN_C3_SAVE_ACC;
              break;
            case BUILT_IN_C3_MVFS:
              iopc = INTRN_C3_MVFS;
              intrinsic_op = TRUE;
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
#ifndef KEY
		Fatal_Error ("Encountered not-yet-supported BUILT_IN: %d at line %d\n",
			 DECL_FUNCTION_CODE (func), lineno);
#endif
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
	  } /* BUILT_IN */
#ifdef TARG_NVISA
          else if (DECL_NAME (func)) {
                // If an intrinsic returns a vector/struct,
                // that creates a problem, as intrinsic_ops by definition
                // have one return value.  So need some type of call,
                // but don't have a calling-convention for NVISA,
                // so hack around this by using asm,
                // which can have multiple results.
                // So we leave intrinsic names as regular calls to gnu,
                // then intercept calls here and treat them as intrinsics.
                char *name = IDENTIFIER_POINTER(DECL_NAME(func));
                if (strcmp(name, "__utexfetchi1D") == 0) {
                        wn = emit_builtin_texfetch(exp,
                                "tex.1d.v4.u32.s32", MTYPE_U4, MTYPE_I4);
                        asm_generated = TRUE;
                }
                else if (strcmp(name, "__itexfetchi1D") == 0) {
                        wn = emit_builtin_texfetch(exp,
                                "tex.1d.v4.s32.s32", MTYPE_I4, MTYPE_I4);
                        asm_generated = TRUE;
                }
                else if (strcmp(name, "__ftexfetchi1D") == 0) {
                        wn = emit_builtin_texfetch(exp,
                                "tex.1d.v4.f32.s32", MTYPE_F4, MTYPE_I4);
                        asm_generated = TRUE;
                }
                else if (strcmp(name, "__utexfetch1D") == 0) {
                        wn = emit_builtin_texfetch(exp,
                                "tex.1d.v4.u32.f32", MTYPE_U4, MTYPE_F4);
                        asm_generated = TRUE;
                }
                else if (strcmp(name, "__itexfetch1D") == 0) {
                        wn = emit_builtin_texfetch(exp,
                                "tex.1d.v4.s32.f32", MTYPE_I4, MTYPE_F4);
                        asm_generated = TRUE;
                }
                else if (strcmp(name, "__ftexfetch1D") == 0) {
                        wn = emit_builtin_texfetch(exp,
                                "tex.1d.v4.f32.f32", MTYPE_F4, MTYPE_F4);
                        asm_generated = TRUE;
                }
                else if (strcmp(name, "__utexfetch2D") == 0) {
                        wn = emit_builtin_texfetch(exp,
                                "tex.2d.v4.u32.f32", MTYPE_U4, MTYPE_F4);
                        asm_generated = TRUE;
                }
                else if (strcmp(name, "__itexfetch2D") == 0) {
                        wn = emit_builtin_texfetch(exp,
                                "tex.2d.v4.s32.f32", MTYPE_I4, MTYPE_F4);
                        asm_generated = TRUE;
                }
                else if (strcmp(name, "__ftexfetch2D") == 0) {
                        wn = emit_builtin_texfetch(exp,
                                "tex.2d.v4.f32.f32", MTYPE_F4, MTYPE_F4);
                        asm_generated = TRUE;
                }
                else if (strcmp(name, "__utexfetch3D") == 0) {
                        wn = emit_builtin_texfetch(exp,
                                "tex.3d.v4.u32.f32", MTYPE_U4, MTYPE_F4);
                        asm_generated = TRUE;
                }
                else if (strcmp(name, "__itexfetch3D") == 0) {
                        wn = emit_builtin_texfetch(exp,
                                "tex.3d.v4.s32.f32", MTYPE_I4, MTYPE_F4);
                        asm_generated = TRUE;
                }
                else if (strcmp(name, "__ftexfetch3D") == 0) {
                        wn = emit_builtin_texfetch(exp,
                                "tex.3d.v4.f32.f32", MTYPE_F4, MTYPE_F4);
                        asm_generated = TRUE;
                }
                else if (strcmp(name, "__double_to_int2") == 0) {
                        wn = emit_builtin_double_to_int2 (exp);
                        asm_generated = TRUE;
                }
                else if (strcmp(name, "__int2_to_double") == 0) {
                        wn = emit_builtin_int2_to_double (exp);
                        asm_generated = TRUE;
                }
                if (asm_generated) {
                        // like a call, need to generate comma stmt
                        // with asm and loading of out params
                        // (necessary when called from return stmt
                        // or when taking component of call).
                        TYPE_ID mtype;
                        PREG_NUM preg_num;
                        ST *preg_st;
                        if (!need_result) {
                                break;  // don't need out params
                        }
                        desc_ty_idx = component_ty_idx;
                        if (desc_ty_idx == 0)
                          desc_ty_idx = Get_TY (TREE_TYPE(exp));
                        ty_idx = desc_ty_idx;
                        mtype = TY_mtype(ty_idx);
                        if (TY_can_be_vector(ty_idx)) {
                          // do struct copy of asm output vector.
                          // asm_neg_preg is end of output pregs;
                          // need to add back size of vector.
                          preg_num = asm_neg_preg + TY_vector_count(ty_idx);
                          preg_st = MTYPE_To_PREG(TY_mtype(TY_vector_elem_ty(ty_idx)));
                        }
                        else {
                          // asm_neg_preg is 1 past last one, so add 1
                          preg_num = asm_neg_preg + 1;
                          preg_st = MTYPE_To_PREG(mtype);
                        }
                        wn1 = WN_CreateLdid (OPR_LDID, mtype, mtype,
                                preg_num, preg_st, ty_idx, 0);

                        wn0 = WN_CreateBlock ();
                        WN_INSERT_BlockLast (wn0, wn);

                        wn  = WN_CreateComma (OPR_COMMA, WN_rtype(wn1), MTYPE_V,
                                wn0, wn1);
                        whirl_generated = TRUE;
                }
          }
#endif

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
	      ikids [i]  = arg_wn;
	    }
#if defined(TARG_SL)
            // just mark lda as vbuf offset and no intrinsic op needed 
            // to be generatd. ikids[0] is a parameter; 
            if( iopc == INTRN_SBUF_OFFSET || iopc == INTRN_VBUF_OFFSET || 
                iopc == INTRN_VBUF_ABSOLUTE) {
              Mark_LDA_Vbuf_Offset(ikids[0], iopc);
              Adjust_Vbuf_Array_Ofst(ikids[0]);
              wn = WN_kid0(ikids[0]); 
              break; 
	    }

#if defined(TARG_SL)
            switch (ret_mtype) {
            case MTYPE_I1:
            case MTYPE_I2: ret_mtype = MTYPE_I4;   break;
            default: ;
            }
#endif
            wn = WN_Create_Intrinsic (OPR_INTRINSIC_OP, ret_mtype, MTYPE_V,
				      iopc, num_args, ikids);
	    //set deref flags for parameters if needed
	    WN_Set_Deref_If_Needed(wn);
#else 

	    wn = WN_Create_Intrinsic (OPR_INTRINSIC_OP, ret_mtype, MTYPE_V,
				      iopc, num_args, ikids);
#endif //TARG_SL
#ifdef KEY
	    if (cvt_to != MTYPE_UNKNOWN) // bug 8251
	      wn = WN_Cvt (ret_mtype, cvt_to, wn);
#endif
	    break;
	  }

          if (!need_result) { // see comment at top about ret_mtype
                ret_mtype = MTYPE_V;
          }

	  if (iopc) {
            call_wn = WN_Create (OPR_INTRINSIC_CALL, ret_mtype, MTYPE_V, num_args);
	    WN_intrinsic (call_wn) = iopc;
	  }
	  else {
            call_wn = WN_Create (OPR_CALL, ret_mtype, MTYPE_V, num_args);
            ST *st2 = DECL_ST2 (TREE_OPERAND (arg0, 0));
            if (Opt_Level > 0 && st2) {
              WN_st_idx (call_wn) = ST_st_idx (st2);
            }
            else {
              st = Get_ST (TREE_OPERAND (arg0, 0));
              WN_st_idx (call_wn) = ST_st_idx (st);
            }
	  }
        }

        else {
          if (!need_result) { // see comment at top about ret_mtype
                ret_mtype = MTYPE_V;
          }
	  num_args++;
          call_wn = WN_Create (OPR_ICALL, ret_mtype, MTYPE_V, num_args);
	  WN_kid(call_wn, num_args-1) = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
	  WN_set_ty (call_wn, TY_pointed(Get_TY(TREE_TYPE (TREE_OPERAND (exp, 0)))));
	}

	WN_Set_Linenum (call_wn, Get_Srcpos());
	WN_Set_Call_Default_Flags (call_wn);

        if (st) {
          tree func = TREE_OPERAND (arg0, 0);
          if (DECL_INLINE (func)) {
            wfe_invoke_inliner = TRUE;
          }
#ifdef TARG_IA64
          int flags = flags_from_decl_or_type (func);
          PU& pu_ent = Pu_Table[ST_pu(st)];
          if (flags & ECF_MALLOC) { 
            Set_PU_has_attr_malloc (pu_ent);
          } else if (flags & ECF_PURE) {
            Set_PU_has_attr_pure (pu_ent);
          } else if (TREE_READONLY(func)) {
            Set_PU_is_pure (pu_ent);
          }
          
          if (flags & ECF_NORETURN) {
            Set_PU_has_attr_noreturn (pu_ent);
          }
#endif
        }

        i = 0;
	for (list = TREE_OPERAND (exp, 1);
	     list;
	     list = TREE_CHAIN (list)) {
          arg_wn     = WFE_Expand_Expr (TREE_VALUE (list));
	  arg_ty_idx = Get_TY(TREE_TYPE(TREE_VALUE(list)));
	  arg_mtype  = TY_mtype(arg_ty_idx);
          if (!WFE_Keep_Zero_Length_Structs    &&
              TY_mtype (arg_ty_idx) == MTYPE_M &&
              TY_size (arg_ty_idx) == 0) {
            // zero length struct parameter
	    if (arg_wn && WN_has_side_effects (arg_wn)) {
	      arg_wn = WN_CreateEval (arg_wn);
	      WFE_Stmt_Append (arg_wn, Get_Srcpos());
            }
          }
          else {
#ifdef KEY // bug 11585
	    if (WN_operator(arg_wn) == OPR_LDA) {
	      ST *st = WN_st(arg_wn);
	      Set_ST_addr_passed(*st);
	      Set_ST_addr_saved(*st);
	    }
#endif // KEY
	    // gcc allows non-struct actual to correspond to a struct formal;
	    // fix mtype of parm node so as not to confuse back-end
	    if (arg_mtype == MTYPE_M)
	      arg_mtype = WN_rtype(arg_wn);
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
        }

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
          Mark_LDA_Vbuf_Offset(WN_kid(call_wn, nth_child), id);
        }

	//set parameter dereference for generated wn
	WN_Set_Deref_If_Needed(call_wn);

#endif // TARG_SL

        if (ret_mtype == MTYPE_V) {
	  WFE_Stmt_Append (call_wn, Get_Srcpos());
#ifdef TARG_SL
            // c3_store, c3_fftst
        if (WN_Need_Append_Intrinsic(call_wn)) {
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

            char *mrname = Label_Name_Separator "Mreturn" Label_Name_Separator;
            ST *ret_st = Gen_Temp_Symbol(ret_ty_idx,
                  st ? Index_To_Str(Save_Str2(mrname, ST_name(ST_st_idx(st))))
                     : mrname);
#ifdef KEY
	    WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, ret_st);
#endif // KEY
	    wn1 = WN_Stid (ret_mtype, 0, ret_st, ty_idx, wn1);
            WN_INSERT_BlockLast (wn0, wn1);

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
#ifndef KEY
        wn = WFE_Expand_Expr (TREE_OPERAND (exp, 0), FALSE);
#else
        wn = WFE_Expand_Expr_With_Sequence_Point(TREE_OPERAND(exp,0), MTYPE_V);
#endif
        if (wn) {
          wn = WN_CreateEval (wn);
          WFE_Stmt_Append (wn, Get_Srcpos ());
        }
#ifndef KEY
        wn = WFE_Expand_Expr (TREE_OPERAND (exp, 1));
#else
        wn = WFE_Expand_Expr (TREE_OPERAND (exp, 1), need_result);
#endif
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
#ifndef TARG_NVISA // we see this too often
	DevWarn ("Encountered SAVE_EXPR at line %d", lineno);
#endif
        wn = WFE_Save_Expr (exp);
      }
      break;

    case UNSAVE_EXPR:
      {
	DevWarn ("Encountered UNSAVE_EXPR at line %d", lineno);
        wn = WFE_Save_Expr (exp);
	WFE_Unsave_Expr (exp);
      }
      break;

    case TARGET_EXPR:
      {
	DevWarn ("Encountered TARGET_EXPR at line %d", lineno);
	wn0 = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
	wn1 = WFE_Expand_Expr (TREE_OPERAND (exp, 1));
      }
      break;

#ifdef GPLUSPLUS_FE
    case INIT_EXPR:
      {
	tree initiand    = TREE_OPERAND(exp, 0);
	tree initializer = TREE_OPERAND(exp, 1);
	tree init_expr;

	if (TREE_CODE(initiand) != VAR_DECL) {
	  DevWarn("expected VAR_DECL");
	  break;
	}

	if (TREE_CODE(initializer) != TARGET_EXPR) {
	  DevWarn("expected TARGET_EXPR");
	  break;
	}
	
	init_expr = TREE_OPERAND(initializer, 3);
	if (TREE_CODE(init_expr) != AGGR_INIT_EXPR) {
	  DevWarn("expected AGGR_INIT_EXPR");
	  break;
	}

	{
	  tree func = TREE_OPERAND(init_expr, 0);
	  tree args = TREE_OPERAND(init_expr, 1);
	  INT  num_args = 0;
	  INT  i;
	  tree list;
	  WN * call_wn;
	  ST * initiand_st = Get_ST(initiand);
	  WN * initiand_wn = WN_Lda(Pointer_Mtype,
				    ST_ofst(initiand_st), 
				    initiand_st);
	  WN * arg_wn;
	  
	  for (list = args; list; list = TREE_CHAIN(list))
	    ++num_args;
	  call_wn = WN_Create(OPR_CALL, MTYPE_V, MTYPE_V, num_args);
	  st = Get_ST(TREE_OPERAND(func, 0));
	  WN_st_idx(call_wn) = ST_st_idx(st);
	  WN_Set_Call_Default_Flags(call_wn);
	
	  
	  for (i = 0, list = TREE_OPERAND(init_expr, 1);
	       list;
	       ++i, list = TREE_CHAIN(list)) {
	    arg_wn = (i == 0) ? initiand_wn
			     : WFE_Expand_Expr(TREE_VALUE(list));
	    TY_IDX  arg_ty_idx = Get_TY(TREE_TYPE(TREE_VALUE(list)));
	    TYPE_ID arg_mtype  = TY_mtype(arg_ty_idx);
	    if (arg_mtype == MTYPE_M)
  	      arg_mtype = WN_rtype(arg_wn);
	    arg_wn = WN_CreateParm(Mtype_comparison(arg_mtype),
				   arg_wn,
				   arg_ty_idx,
				   WN_PARM_BY_VALUE);
	    WN_kid(call_wn, i) = arg_wn;
         	       WN_kid (call_wn, i) = arg_wn;
	  }

	  WFE_Stmt_Append(call_wn, Get_Srcpos());
	}       
        break;
      }

    case UNSAVE_EXPR:
      {
	wn = WFE_Expand_Expr( TREE_OPERAND (exp, 0));
	break;
      }     
    case NEW_EXPR:
      {
	wn = WFE_Expand_Expr(build_new_1(exp));
        break;
      }
    case RTL_EXPR:
      break;
#endif /* GPLUSPLUS_FE */

    case VA_ARG_EXPR:
      {
#if defined(TARG_PPC32)
      tree kid0 = TREE_OPERAND(exp, 0);
      TY_IDX va_ty_index = WFE_Load_Va_List_PPC32(kid0);

      wn = WFE_Expand_Vararg_Expr_PPC32(Get_TY (TREE_TYPE(exp)), va_ty_index);
      break;
      }     
              
#else // defined(TARG_PPC32)
#ifdef TARG_X8664
	if( TARGET_64BIT ){
	  tree kid0 = TREE_OPERAND(exp, 0);
	  WN *ap_wn;
	  ap_wn = WFE_Expand_Expr(kid0);
	  if (WN_rtype(ap_wn) == MTYPE_M) {
	    if (OPCODE_is_leaf(WN_opcode(ap_wn)))
#ifdef TARG_IA64
	      ap_wn = WN_Lda(Pointer_Mtype, 0, WN_st(ap_wn), 0);
#else
	      ap_wn = WN_Lda(Pointer_Mtype, WN_offset(ap_wn), WN_st(ap_wn), 0);
#endif
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

	  if ( mtype != MTYPE_FQ && mtype != MTYPE_M && !MTYPE_is_complex(mtype)) {
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

	    if( mtype == MTYPE_FQ ){
	      wn = WN_CreateIload(OPR_ILOAD, Widen_Mtype (mtype), mtype, 0,
				  ty_idx, Make_Pointer_Type(ty_idx), wn);
	    } else {
	      wn = WN_CreateIload(OPR_ILOAD, MTYPE_M, MTYPE_M, 0, ty_idx, 
				  Make_Pointer_Type(ty_idx), wn);
	    }
	  }

	  break;
	} // end of TARGET_64BIT
#endif
        // code swiped from builtins.c (std_expand_builtin_va_arg)
	tree type = TREE_TYPE (exp);
	TY_IDX ty_idx = Get_TY (type);
	TYPE_ID mtype = TY_mtype (ty_idx);

        /* Compute the rounded size of the type.  */
#ifdef TARG_IA64
	INT64 ty_align = TYPE_ALIGN (type) / BITSPERBYTE;
	INT64 ty_size = int_size_in_bytes (type);

	INT64 align = PARM_BOUNDARY / BITS_PER_UNIT;
	ty_size = ((ty_size + align - 1) / align) * align;
	ty_align = ((ty_align + align - 1) / align) * align;
#else
	INT64 align;
        INT64 rounded_size;
        /* Compute the rounded size of the type.  */
        align = PARM_BOUNDARY / BITS_PER_UNIT;
        rounded_size = (((int_size_in_bytes (type) + align - 1) / align) * align);				
#endif

	/* Get AP.  */
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

#ifdef TARG_IA64
	wn = WN_COPY_Tree(ap_load);

	/* Align AP for the next argument. */
	if (ty_align > align) {
		wn = WN_Binary (OPR_ADD, Pointer_Mtype, wn,
		    WN_Intconst (Pointer_Mtype, ty_align - 1));
		wn = WN_Binary (OPR_BAND, Pointer_Mtype, wn,
		    WN_Intconst (Pointer_Mtype, -ty_align));
	}

	/* Compute new value for AP.  */
	if (Target_Byte_Sex == BIG_ENDIAN) {
	  wn = WN_Binary (OPR_ADD, Pointer_Mtype, wn,
			  WN_Intconst (Pointer_Mtype, 3));
	  wn = WN_Binary (OPR_BAND, Pointer_Mtype, wn,
			  WN_Intconst (Pointer_Mtype, -8));
	}
	wn = WN_Binary (OPR_ADD, Pointer_Mtype, wn,
		WN_Intconst (Pointer_Mtype, ty_size));
 
#else
#ifndef KEY
	if (Target_Byte_Sex == BIG_ENDIAN) {
          Fail_FmtAssertion ("VA_ARG_EXPR not implemented for BIG_ENDIAN");
          INT64 adj;
	  adj = TREE_INT_CST_LOW (TYPE_SIZE (type)) / BITS_PER_UNIT;
          if (rounded_size > align)
            adj = rounded_size;
          wn = WN_Binary (OPR_ADD, Pointer_Mtype, wn,
          WN_Intconst (Pointer_Mtype, rounded_size - adj));
          }
          /* Compute new value for AP.  */
          wn = WN_Binary (OPR_ADD, Pointer_Mtype, WN_COPY_Tree (ap_load),
		            WN_Intconst (Pointer_Mtype, rounded_size));
#else
          if (Target_Byte_Sex == BIG_ENDIAN) {
            INT64 adj;
            adj = TREE_INT_CST_LOW (TYPE_SIZE (type)) / BITS_PER_UNIT;
            if (rounded_size > align)
              adj = rounded_size;
            wn = WN_Binary (OPR_ADD, Pointer_Mtype, WN_COPY_Tree (ap_load),
            WN_Intconst (Pointer_Mtype, 3));
            wn = WN_Binary (OPR_BAND, Pointer_Mtype, wn,
	                WN_Intconst (Pointer_Mtype, -8));
            wn = WN_Binary (OPR_ADD, Pointer_Mtype, wn,
            WN_Intconst (Pointer_Mtype, rounded_size));
	    } else
            /* Compute new value for AP.  */
        {
          wn = WN_Binary (OPR_ADD, Pointer_Mtype, WN_COPY_Tree (ap_load),
              WN_Intconst (Pointer_Mtype, rounded_size));
        }

#ifdef TARG_X8664 // bug 12118: pad since under -m32, vector types are 8-byte aligned
	if (MTYPE_is_vector(mtype) && ! TARGET_64BIT) {
	  wn = WN_Add(Pointer_Mtype, wn, WN_Intconst(Pointer_Mtype, 7));
	  wn = WN_Div(Pointer_Mtype, wn, WN_Intconst(Pointer_Mtype, 8));
	  wn = WN_Mpy(Pointer_Mtype, wn, WN_Intconst(Pointer_Mtype, 8));
	}
#endif // TARG_X8664
#if defined(TARG_MIPS) || defined(TARG_LOONGSON) // bug 12945: pad since long doubles are 16-byte aligned
	if (mtype == MTYPE_FQ) {
	  wn = WN_Add(Pointer_Mtype, wn, WN_Intconst(Pointer_Mtype, 15));
	  wn = WN_Div(Pointer_Mtype, wn, WN_Intconst(Pointer_Mtype, 16));
	  wn = WN_Mpy(Pointer_Mtype, wn, WN_Intconst(Pointer_Mtype, 16));
	}
#endif // TARG_MIPS
#endif // ! KEY
		    
#endif // TARG_IA64
        if (ap_st)
	  wn = WN_Stid (Pointer_Mtype, ap_offset, ap_st, ap_ty_idx, wn);
        else {
          wn = WN_CreateIstore (OPR_ISTORE, MTYPE_V, Pointer_Mtype, ap_offset,
#if defined(TARG_SL)
                                ap_addr_ty, wn, ap_addr, ap_field_id);
	  DevWarn("Forcing var-arg istore to aligned");
#else
                                ap_addr_ty, wn, ap_addr, ap_field_id);
#endif
        }
        WFE_Stmt_Append (wn, Get_Srcpos ());

#ifdef KEY
	if (Target_Byte_Sex != Host_Byte_Sex)
#ifdef TARG_IA64
          wn = WN_CreateIload (OPR_ILOAD, Widen_Mtype (mtype), mtype,
                          ((MTYPE_size_min(mtype)==32)?4:0)-ty_size,
                          ty_idx, Make_Pointer_Type (ty_idx, FALSE),
                          ap_load);
#else
          wn = WN_CreateIload (OPR_ILOAD, Widen_Mtype (mtype), mtype, 
			  ((MTYPE_size_min(mtype)==32)?4:0)-rounded_size,	  
			  ty_idx, Make_Pointer_Type (ty_idx, FALSE),
			  ap_load);
#endif
        else
#endif

#ifdef TARG_IA64
          wn = WN_CreateIload (OPR_ILOAD, Widen_Mtype (mtype), mtype, -ty_size,
                          ty_idx, Make_Pointer_Type (ty_idx, FALSE),
                          ap_load);
#else
          wn = WN_CreateIload (OPR_ILOAD, Widen_Mtype (mtype), mtype, -rounded_size,
                          ty_idx, Make_Pointer_Type (ty_idx, FALSE),
                          ap_load);
#endif

      }
      break;
#endif // defined(TARG_PPC32)

    case ERROR_MARK:

      exit (RC_USER_ERROR);
      break;

#ifdef KEY
    case COMPOUND_LITERAL_EXPR:
      {
	tree oper = TREE_OPERAND (TREE_OPERAND (exp, 0), 0);
	if (TREE_CODE (DECL_INITIAL (oper)) == CONSTRUCTOR)
	    exp = DECL_INITIAL (oper);
	else exp = oper;

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

	if (TREE_CODE (exp) == CONSTRUCTOR)
	{
	    DevWarn ("Encountered CONSTRUCTOR at line %d", lineno);
	    st = WFE_Generate_Temp_For_Initialized_Aggregate (exp, "");
	}
	else 
	{
	    st = Get_ST (exp);
            if (ST_assigned_to_dedicated_preg (st))
	    	Set_TY_is_volatile(ty_idx);
	}

	Is_True(! is_bit_field || field_id <= MAX_FIELD_ID,
		("WFE_Expand_Expr: field id for bit-field exceeds limit"));
	wn = WN_CreateLdid (OPR_LDID, rtype,
			    is_bit_field ? MTYPE_BS : desc,
			    ST_ofst(st)+component_offset+preg_num, st,
			    field_id != 0 ? hi_ty_idx : ty_idx, field_id);
	if (cvtl_size != 0)
	  wn = WN_CreateCvtl(OPR_CVTL, rtype, MTYPE_V, cvtl_size, wn);
      }
      break;

    case STMT_EXPR:
      {
	// Find the last EXPR_STMT in the COMPUND_STMT; the is the value of the 
	// expression.
	// See code in c_expand_expr in gnu/c-common.c
	// STMT_EXPR_STMT is given by first operand of exp.
	// COMPOUND_BODY is given by first operand of STMT_EXPR_STMT
	tree expr = TREE_OPERAND (TREE_OPERAND (exp, 0), 0);
	if (!expr) break;
	tree last = TREE_CHAIN (expr);
	
	while (TREE_CHAIN (last))
	  {
	    WN *dummy = WFE_Expand_Expr(expr, FALSE);
	    expr = last;
	    last = TREE_CHAIN (last);
	  }
	// EXPR_STMT_EXPR of expr is given by the first operand.
	if (TREE_OPERAND (expr, 0) && TREE_CODE (expr) == EXPR_STMT)
	  wn = WFE_Expand_Expr(TREE_OPERAND(expr, 0), need_result, nop_ty_idx, 
			       component_ty_idx, component_offset, field_id, 			       
			       is_bit_field);
	else {
	  WFE_Expand_Expr(expr, FALSE);
	  wn = WN_Intconst(MTYPE_I4, 0); // dummy, bug 10345
	}
	// FIXME: Are we missing the last expr, i.e. 'last' here?
      }
      break;
    case EXPR_WITH_FILE_LOCATION:
      {
	wn = WFE_Expand_Expr(TREE_OPERAND(exp, 0));
      }
      break;

    case LABEL_DECL:
      {
	DevWarn ("taking address of a label at line %d", lineno);
	LABEL_IDX label_idx = WFE_Get_LABEL (arg0, FALSE);
	FmtAssert (arg0->decl.symtab_idx == CURRENT_SYMTAB,
		   ("line %d: taking address of a label not defined in current function currently not implemented", lineno));
	wn = WN_LdaLabel (Pointer_Mtype, label_idx);
	Set_LABEL_addr_saved (label_idx);
	Set_PU_no_inline (Get_Current_PU ());
      }
      break;

   case SCOPE_STMT:
     {
       // If the control flows here, it can only be introduced here
       // by expansion of STMT_EXPR. Otherwise, it would have been 
       // handled in gnu/ files.
       // Ignore any new scopes introduced here - WARNING - 
       // TODO: To handle scopes here
     }
     break;

   case DECL_STMT:
     {
       // If the control flows here, it can only be introduced here
       // by expansion of STMT_EXPR. Otherwise, it would have been 
       // handled in gnu/ files.
       tree decl = TREE_OPERAND (exp, 0);
       if (TREE_CODE (decl) != FUNCTION_DECL
	   && TREE_CODE (decl) != PARM_DECL
	   && DECL_INITIAL (decl) != 0
	   && DECL_INITIAL (decl) != error_mark_node)
	 WFE_Initialize_Nested_Decl (decl);
       WFE_Decl (decl);
     }
     break;

   case EXPR_STMT:
     {
       // If the control flows here, it can only be introduced here
       // by expansion of STMT_EXPR. Otherwise, it would have been 
       // handled in gnu/ files.
       wn = WFE_Expand_Expr ( TREE_OPERAND(exp, 0), FALSE /* no result */);
       // code from WFE_One_Stmt for bug 4642
       if (wn && 
           WN_operator (wn) == OPR_COMMA && 
           WN_operator (WN_kid1 (wn)) == OPR_LDID &&
           WN_st (WN_kid1 (wn)) == Return_Val_Preg &&
           (WN_operator (WN_last (WN_kid0 (wn))) == OPR_CALL ||
            WN_operator (WN_last (WN_kid0 (wn))) == OPR_ICALL))
       {
         WN_set_rtype (WN_last (WN_kid0 (wn)), MTYPE_V);
         WFE_Stmt_Append (WN_kid0 (wn), Get_Srcpos ());
         WN_Delete (wn);
       }
       wn = NULL;
     }
     break;

   case COMPOUND_STMT:
     {
       // If the control flows here, it can only be introduced here
       // by expansion of STMT_EXPR. Otherwise, it would have been 
       // handled in gnu/ files.
       tree expr = TREE_OPERAND (exp, 0); // COMPOUND_BODY
       if (expr) // bug 3151
       {
       	 tree last = TREE_CHAIN (expr);
       
         while (TREE_CHAIN (last))
	 {
	   WFE_Expand_Expr(expr, FALSE);
	   expr = last;
	   last = TREE_CHAIN (last);
	 }
         WFE_Expand_Expr(expr, FALSE);
	 // bug 4150: don't miss the last node
         wn = WFE_Expand_Expr(last, FALSE);
       }
     }
     break;

   case IF_STMT:
     {
       // If the control flows here, it can only be introduced here
       // by expansion of STMT_EXPR. Otherwise, it would have been 
       // handled in gnu/ files.

       struct nesting * cond_nest = alloc_nesting();
       construct_nesting (cond_nest, wfe_cond_stack, wfe_nesting_stack, 0);

       wfe_nesting_stack = wfe_cond_stack = cond_nest;

       tree cond = TREE_OPERAND (exp, 0); // IF_COND
       if (cond && TREE_CODE (cond) == TREE_LIST)
	 {
	   WFE_Expand_Expr (TREE_PURPOSE (cond), FALSE);
	   cond = TREE_VALUE (cond);
	 }
       WFE_Expand_Start_Cond (cond, 0);
       if (TREE_OPERAND (exp, 1)) // THEN_CLAUSE
       {
	 WFE_Expand_Expr (TREE_OPERAND (exp, 1), FALSE); 
	 traverse_tree_chain (TREE_OPERAND (exp,1));
       }
       if (TREE_OPERAND (exp, 2)) { // ELSE_CLAUSE
	 WFE_Expand_Start_Else ();
	 WFE_Expand_Expr (TREE_OPERAND (exp, 2), FALSE); 
	 traverse_tree_chain (TREE_OPERAND (exp,2));
       }
       WFE_Expand_End_Cond ();       
       popstack (cond_nest);
     }
     break;

   case GOTO_STMT:
     {
       tree destination = TREE_OPERAND (exp, 0);

       if (TREE_CODE (destination) == LABEL_DECL)
         WFE_Expand_Goto (destination);
       else
         WFE_Expand_Computed_Goto (destination);
     }
     break;

   case LABEL_STMT:
     {
       tree label = TREE_OPERAND (exp, 0);
       LABEL_IDX label_idx = WFE_Get_LABEL (label, TRUE);
       label->decl.symtab_idx = CURRENT_SYMTAB;
       label->decl.label_defined = TRUE;
       WN *wn_tmp = WN_CreateLabel ((ST_IDX) 0, label_idx, 0, NULL);
       WFE_Stmt_Append (wn_tmp, Get_Srcpos ());
     }
     break;
     
   case FOR_STMT:
     {
       // If the control flows here, it can only be introduced here
       // by expansion of STMT_EXPR. Otherwise, it would have been 
       // handled in gnu/ files.
       //
       // 1. Expand the initialization
       //    WFE_Expand_Expr (EXPR_STMT_EXPR (FOR_INIT_STMT (exp)));
       //
       // Initialize if there is an initializer (bug 6098)
       tree init = TREE_OPERAND (TREE_OPERAND (exp, 0), 0);
       if (init)
         WFE_Expand_Expr (init, FALSE);

       struct nesting *loop_nest= alloc_nesting();
       LABEL_IDX for_exit_label_idx;

       New_LABEL (CURRENT_SYMTAB, for_exit_label_idx);
       construct_nesting (loop_nest, wfe_loop_stack, wfe_nesting_stack,
                          for_exit_label_idx);

       wfe_nesting_stack = wfe_loop_stack = loop_nest;

       WFE_Expand_Start_Loop (1, loop_nest);
       WFE_Expand_Start_Loop_Continue_Elsewhere (1, loop_nest);
       // 2. Expand the condition
       tree cond = TREE_OPERAND (exp, 1);
       FmtAssert (!cond || TREE_CODE (cond) != TREE_LIST, 
		  ("Handle this case"));
       WFE_Expand_Exit_Loop_If_False (loop_nest, cond);
       // 3. Expand the body
       //    WFE_Expand_Expr (FOR_BODY (exp));
       if (TREE_OPERAND (exp, 3))
       {
	 WFE_Expand_Expr (TREE_OPERAND (exp, 3), FALSE);
	 traverse_tree_chain (TREE_OPERAND (exp, 3));
       }
       WFE_Expand_Loop_Continue_Here ();
       // 4. Expand the post-iteration expression
       //    WFE_One_Stmt (FOR_EXPR (exp));
       if (TREE_OPERAND (exp, 2))
	 WFE_One_Stmt (TREE_OPERAND (exp, 2));
       // 5. End the loop
       WFE_Expand_End_Loop ();       
       popstack (loop_nest);
       wn = WN_CreateLabel ((ST_IDX) 0,
                            for_exit_label_idx,
                            0, NULL);
       WFE_Stmt_Append (wn, Get_Srcpos ());

       break;
     }

   case WHILE_STMT:
     {
       // If the control flows here, it can only be introduced here
       // by expansion of STMT_EXPR. Otherwise, it would have been 
       // handled in gnu/ files.
       struct nesting *loop_nest = alloc_nesting();
       LABEL_IDX while_exit_label_idx;

       New_LABEL (CURRENT_SYMTAB, while_exit_label_idx);
       construct_nesting (loop_nest, wfe_loop_stack, wfe_nesting_stack,
                          while_exit_label_idx);

       wfe_nesting_stack = wfe_loop_stack = loop_nest;

       WFE_Expand_Start_Loop (1, loop_nest);
       // 1. Expand the condition
       //   WHILE_COND (exp)
       tree cond = TREE_OPERAND (exp, 0);
       FmtAssert (TREE_CODE (cond) != TREE_LIST, 
		  ("Handle this case"));
       WFE_Expand_Exit_Loop_If_False (loop_nest, cond);
       // 2. Expand the body
       //    WFE_Expand_Expr (WHILE_BODY (exp));
       if (TREE_OPERAND (exp, 1))
       {
	 WFE_Expand_Expr (TREE_OPERAND (exp, 1), FALSE);
	 traverse_tree_chain (TREE_OPERAND (exp, 1));
       }
       // 3. End the loop
       WFE_Expand_End_Loop ();       
       popstack (loop_nest);
       wn = WN_CreateLabel ((ST_IDX) 0,
                            while_exit_label_idx,
                            0, NULL);
       WFE_Stmt_Append (wn, Get_Srcpos ());
       break;
     }

     // Fix bug 618 
   case DO_STMT:
     {
       // If the control flows here, it can only be introduced here
       // by expansion of STMT_EXPR. Otherwise, it would have been 
       // handled in gnu/ files.
       struct nesting *loop_nest = alloc_nesting();
       LABEL_IDX dostmt_exit_label_idx;

       New_LABEL (CURRENT_SYMTAB, dostmt_exit_label_idx);
       construct_nesting (loop_nest, wfe_loop_stack, wfe_nesting_stack, dostmt_exit_label_idx);

       wfe_nesting_stack = wfe_loop_stack = loop_nest;

       WFE_Expand_Start_Loop (1, loop_nest);
       WFE_Expand_Start_Loop_Continue_Elsewhere (1, loop_nest);
       // 1. Expand the body
       //    WFE_Expand_Expr (DO_BODY (exp));
       if (TREE_OPERAND (exp, 1))
       {
	 WFE_Expand_Expr (TREE_OPERAND (exp, 1), FALSE);
	 traverse_tree_chain (TREE_OPERAND (exp, 1));
       }
       // Bug 2126
       WFE_Expand_Loop_Continue_Here();
       // 2. Expand the condition
       //   WHILE_COND (exp)
       tree cond = TREE_OPERAND (exp, 0);
       FmtAssert (TREE_CODE (cond) != TREE_LIST, 
		  ("Handle this case"));
       WFE_Expand_Exit_Loop_If_False (loop_nest, cond);
       // 3. End the loop
       WFE_Expand_End_Loop ();       
       popstack (loop_nest);
       wn = WN_CreateLabel ((ST_IDX) 0,
                            dostmt_exit_label_idx,
                            0, NULL);
       WFE_Stmt_Append (wn, Get_Srcpos ());
       break;
     }

   case RETURN_STMT:
     {
       // If the control flows here, it can only be introduced here
       // by expansion of STMT_EXPR. Otherwise, it would have been 
       // handled in gnu/ files.
       if ( TREE_OPERAND (exp, 0))
	 WFE_Expand_Return (TREE_OPERAND (exp, 0));
       else
	 WFE_Null_Return ();
       break;
     }

   case SWITCH_STMT:
     {
       // If the control flows here, it can only be introduced here
       // by expansion of STMT_EXPR. Otherwise, it would have been 
       // handled in gnu/ files.
       LABEL_IDX switch_exit_label_idx;

       New_LABEL (CURRENT_SYMTAB, switch_exit_label_idx);
       struct nesting * case_nest = alloc_nesting();
       construct_nesting (case_nest, wfe_case_stack, wfe_nesting_stack, 
       			  switch_exit_label_idx);

       wfe_nesting_stack = wfe_case_stack = case_nest;

       // The condition is in SWITCH_COND (exp)
       tree cond = TREE_OPERAND (exp, 0);
       FmtAssert (TREE_CODE (cond) != TREE_LIST, 
		  ("Handle this case"));
       const char *print_name = "switch statement";
       // 1. Start case
       WFE_Expand_Start_Case (1, cond, TREE_TYPE(cond), (char *)print_name);
       // 2. Expand the body
       //    WFE_Expand_Expr (SWITCH_BODY (exp));
       if (TREE_OPERAND (exp, 1))
       {
	 WFE_Expand_Expr (TREE_OPERAND (exp, 1), FALSE);
	 traverse_tree_chain (TREE_OPERAND (exp, 1));
       }
       // 3. End case
       wn = WN_CreateLabel ((ST_IDX) 0,
			    switch_exit_label_idx,
			    0, NULL);
       WFE_Stmt_Append (wn, Get_Srcpos ());
       WFE_Expand_End_Case (cond);       
       popstack (case_nest);
       break;
     }

   case CASE_LABEL:
     {
       // If the control flows here, it can only be introduced here
       // by expansion of STMT_EXPR. Otherwise, it would have been 
       // handled in gnu/ files.
       // 1. Add a case node.       
       tree low = TREE_OPERAND (exp, 0);
       tree high = TREE_OPERAND (exp, 1);
       tree label_decl = TREE_OPERAND (exp, 2);
       if (!high)
	 high = low;
       if (!high && !low)
	 WFE_Record_Switch_Default_Label(label_decl);
       else
	 WFE_Add_Case_Node (low, high, label_decl);
       // 2. Expand label
       WFE_Expand_Label (label_decl);
       break;
     }

   case BREAK_STMT:
     {
       // If the control flows here, it can only be introduced here
       // by expansion of STMT_EXPR. Otherwise, it would have been 
       // handled in gnu/ files.
       LABEL_IDX *label_idx = (LABEL_IDX *)malloc(sizeof(LABEL_IDX));
       *label_idx = 0;

       // bug 11701
       struct nesting * n = wfe_get_matching_scope (wfe_nesting_stack);
       Is_True (n, ("WFE_Expand_Expr: break stmt without enclosing scope"));
       *label_idx = get_nesting_label (n);

       WFE_Expand_Exit_Something (n, wfe_cond_stack,
                                wfe_loop_stack, wfe_case_stack, label_idx);
       free (label_idx);

       break;
     }

     // Fix Bug 619
   case CONTINUE_STMT:
     {
       // If the control flows here, it can only be introduced here
       // by expansion of STMT_EXPR. Otherwise, it would have been 
       // handled in gnu/ files.
       WFE_Expand_Continue_Loop (NULL);
       break;
     }

   case ASM_STMT:
     {
       // If the control flows here, it can only be introduced here
       // by expansion of STMT_EXPR. Otherwise, it would have been 
       // handled in gnu/ files.
       Wfe_Expand_Asm_Operands (TREE_OPERAND (exp, 1),
       				TREE_OPERAND (exp, 2),
       				TREE_OPERAND (exp, 3),
				TREE_OPERAND (exp, 4),
				TREE_OPERAND (exp, 0) != NULL,
				NULL,
				0);
       break;
     }
   case OMP_MARKER_STMT:
     {
       // If the control flows here, it can only be introduced here
       // by expansion of STMT_EXPR. Otherwise, it would have been 
       // handled in gnu/ files.
       process_omp_stmt (exp);
       break;
     }
   case VECTOR_CST:
     {
       ST * init_st = Gen_Temp_Symbol (Get_TY(TREE_TYPE(exp)), "__vec_cst");
       Traverse_Aggregate_Vector_Const (init_st, exp, 0, 0);
       TY_IDX ty = ST_type (init_st);
       TYPE_ID mtype = TY_mtype (ty);
       wn = WN_CreateLdid (OPR_LDID, mtype, mtype, 0, init_st, ty, 0);
       break;
     }
#endif

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
               code == COMPOUND_EXPR ||
               code == INDIRECT_REF ||
               code == COMPONENT_REF ||
               code == STMT_EXPR ||
               ((code == COND_EXPR) && (TY_mtype (ty_idx) == MTYPE_V)),
	       ("WFE_Expand_Expr: NULL WHIRL tree for %s at line %d",
		Operator_From_Tree [code].name, lineno));

  return wn;
}

void WFE_One_Stmt (tree exp)
{
  WN *wn;
  wfe_save_expr_stack_last = -1; // to minimize searches
  if (get_expr_stmts_for_value ())
    wn = WFE_Expand_Expr_With_Sequence_Point (exp, TY_mtype (Get_TY (TREE_TYPE (exp))));
  else
    wn = WFE_Expand_Expr_With_Sequence_Point (exp, MTYPE_V);
  if (wn) {
    if (get_expr_stmts_for_value ()) {
      wn = WN_CreateEval (wn);
      WFE_Stmt_Append (wn, Get_Srcpos ());
      return;
    }
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
        BOOL need_eval = TRUE;
	if (WN_operator (wn) == OPR_LDID &&
	    !TY_is_volatile (ST_type (WN_st (wn))))
          need_eval = FALSE;
        else
        if (WN_operator (wn) == OPR_ILOAD &&
            WN_has_side_effects (wn) == FALSE)
          need_eval = FALSE;
        if (need_eval) {
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
  for (i = 0; i < LAST_AND_UNUSED_TREE_CODE; i++)
    FmtAssert (Operator_From_Tree [i].tree_code == i,
               ("Operator_From_Tree table incorrect at %d", i));
}

char *
WFE_Tree_Node_Name (tree op)
{
  return Operator_From_Tree [TREE_CODE (op)].name;
}
