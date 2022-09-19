/*
 * Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.
 */

////////////////////////////////////////////////////////////////////////////////
//
// Copyright 2006 PathScale, Inc. All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify it
// under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it is
// free of the rightful claim of any third person regarding infringement
// or the like.  Any license provided herein, whether implied or
// otherwise, applies only to this software file.  Patent licenses, if
// any, provided herein do not apply to combinations of this program with
// other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write the Free Software Foundation, Inc., 59
// Temple Place - Suite 330, Boston MA 02111-1307, USA.
////////////////////////////////////////////////////////////////////////////////

extern "C" {
#include "gspin-wgen-interface.h"
}
#if defined(BUILD_OS_DARWIN)
#include <limits.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <values.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include "defs.h"
#include "glob.h"
#include "config.h"
#ifdef TARG_X8664
#include "config_opt.h"
#endif
#include "wn.h"
#include "wn_util.h"
#include "targ_sim.h"
#include "const.h"
#include "intrn_info.h"
#include "c_int_model.h"

#include "ir_reader.h"
#include "wgen_misc.h"
#include "wgen_dst.h"
#include "wgen_spin_symbol.h"
#include "wgen_decl.h"
#include "wgen_expr.h"
#include "wgen_stmt.h"

#define BITS_PER_UNIT 8

extern void WGEN_add_pragma_to_enclosing_regions(WN_PRAGMA_ID, ST *);
extern void WGEN_Expand_Return(gs_t, gs_t);

LABEL_IDX loop_expr_exit_label = 0; // exit label for LOOP_EXPRs

gs_t enclosing_cleanup_point_expr = NULL;

#include <ext/hash_map>

namespace {

using __gnu_cxx::hash_map;

struct ptrhash {
  size_t operator()(void *p) const { return reinterpret_cast<size_t>(p); }
};

hash_map<gs_t, BOOL, ptrhash> referenced_by_whirl_map;
} // namespace

BOOL &REFERENCED_BY_WHIRL(gs_t t) { return referenced_by_whirl_map[t]; }

struct operator_from_tree_t {
  gs_code_t tree_code;
  OPERATOR opr;
} Operator_From_Tree[] = {
    // Generic core stuff:

    DOT,
    OPERATOR_UNKNOWN,

    // list
    EMPTY,
    OPERATOR_UNKNOWN,
    CONS,
    OPERATOR_UNKNOWN,

    // Inbuilt Constructors:

    IB_INT,
    OPERATOR_UNKNOWN,
    IB_UNSIGNED,
    OPERATOR_UNKNOWN,
    IB_LONG_LONG,
    OPERATOR_UNKNOWN,
    IB_LONG,
    OPERATOR_UNKNOWN,
    IB_LONG_DOUBLE,
    OPERATOR_UNKNOWN,
    IB_UNSIGNED_LONG,
    OPERATOR_UNKNOWN,
    IB_UNSIGNED_LONG_LONG,
    OPERATOR_UNKNOWN,
    IB_CHAR,
    OPERATOR_UNKNOWN,
    IB_STRING,
    OPERATOR_UNKNOWN,
    IB_FLOAT,
    OPERATOR_UNKNOWN,
    IB_DOUBLE,
    OPERATOR_UNKNOWN,
    IB_BOOL,
    OPERATOR_UNKNOWN,
    IB_BIT_VECTOR,
    OPERATOR_UNKNOWN,

    GS_TCC,
    OPERATOR_UNKNOWN,
    GSBI,
    OPERATOR_UNKNOWN,
    GSBI_CLASS,
    OPERATOR_UNKNOWN,

    // GCC Specific tree codes:

    GS_ABS_EXPR,
    OPR_ABS,
    GS_ADDR_EXPR,
    OPERATOR_UNKNOWN,
    GS_ALIGNOF_EXPR,
    OPERATOR_UNKNOWN,
    GS_ALIGN_INDIRECT_REF,
    OPERATOR_UNKNOWN,
    GS_ARRAY_RANGE_REF,
    OPERATOR_UNKNOWN,
    GS_ARRAY_REF,
    OPERATOR_UNKNOWN,
    GS_ARRAY_TYPE,
    OPERATOR_UNKNOWN,
    GS_ARROW_EXPR,
    OPERATOR_UNKNOWN,
    GS_ASM_EXPR,
    OPERATOR_UNKNOWN,
    GS_ASSIGN_STMT,
    OPERATOR_UNKNOWN,
    GS_BIND_EXPR,
    OPERATOR_UNKNOWN,
    GS_BIT_AND_EXPR,
    OPR_BAND,
    GS_BIT_FIELD_REF,
    OPERATOR_UNKNOWN,
    GS_BIT_IOR_EXPR,
    OPR_BIOR,
    GS_BIT_NOT_EXPR,
    OPR_BNOT,
    GS_BIT_XOR_EXPR,
    OPR_BXOR,
    GS_BLOCK,
    OPERATOR_UNKNOWN,
    GS_BOOLEAN_TYPE,
    OPERATOR_UNKNOWN,
    GS_BREAK_STMT,
    OPERATOR_UNKNOWN,
    GS_CALL_EXPR,
    OPERATOR_UNKNOWN,
    GS_CASE_LABEL_EXPR,
    OPERATOR_UNKNOWN,
    GS_CATCH_EXPR,
    OPERATOR_UNKNOWN,
    GS_CEIL_DIV_EXPR,
    OPR_DIV,
    GS_CEIL_MOD_EXPR,
    OPR_DIV,
    GS_CHAR_TYPE,
    OPERATOR_UNKNOWN,
    GS_CLEANUP,
    OPERATOR_UNKNOWN,
    GS_CLEANUP_POINT_EXPR,
    OPERATOR_UNKNOWN,
    GS_COMPLEX_CST,
    OPERATOR_UNKNOWN,
    GS_COMPLEX_EXPR,
    OPR_PAIR,
    GS_COMPLEX_TYPE,
    OPERATOR_UNKNOWN,
    GS_COMPONENT_REF,
    OPERATOR_UNKNOWN,
    GS_COMPOUND_EXPR,
    OPERATOR_UNKNOWN,
    GS_COMPOUND_LITERAL_EXPR,
    OPERATOR_UNKNOWN,
    GS_COND_EXPR,
    OPERATOR_UNKNOWN,
    GS_CONJ_EXPR,
    OPERATOR_UNKNOWN,
    GS_CONSTRUCTOR,
    OPERATOR_UNKNOWN,
    GS_CONST_DECL,
    OPERATOR_UNKNOWN,
    GS_CONTINUE_STMT,
    OPERATOR_UNKNOWN,
    GS_CONVERT_EXPR,
    OPERATOR_UNKNOWN,
    GS_DECL_EXPR,
    OPERATOR_UNKNOWN,
    GS_DO_STMT,
    OPERATOR_UNKNOWN,
    GS_EH_FILTER_EXPR,
    OPERATOR_UNKNOWN,
    GS_ENUMERAL_TYPE,
    OPERATOR_UNKNOWN,
    GS_EQ_EXPR,
    OPR_EQ,
    GS_ERROR_MARK,
    OPERATOR_UNKNOWN,
    GS_EXACT_DIV_EXPR,
    OPR_DIV,
    GS_EXC_PTR_EXPR,
    OPERATOR_UNKNOWN,
    GS_EXIT_EXPR,
    OPERATOR_UNKNOWN,
    GS_EXPR_STMT,
    OPERATOR_UNKNOWN,
    GS_FDESC_EXPR,
    OPERATOR_UNKNOWN,
    GS_FIELD_DECL,
    OPERATOR_UNKNOWN,
    GS_FILE_TYPE,
    OPERATOR_UNKNOWN,
    GS_FILTER_EXPR,
    OPERATOR_UNKNOWN,
    GS_FIX_CEIL_EXPR,
    OPERATOR_UNKNOWN,
    GS_FIX_FLOOR_EXPR,
    OPERATOR_UNKNOWN,
    GS_FIX_ROUND_EXPR,
    OPERATOR_UNKNOWN,
    GS_FIX_TRUNC_EXPR,
    OPERATOR_UNKNOWN,
    GS_FLOAT_EXPR,
    OPERATOR_UNKNOWN,
    GS_FLOOR_DIV_EXPR,
    OPERATOR_UNKNOWN,
    GS_FLOOR_MOD_EXPR,
    OPERATOR_UNKNOWN,
    GS_FOR_STMT,
    OPERATOR_UNKNOWN,
    GS_FUNCTION_DECL,
    OPERATOR_UNKNOWN,
    GS_FUNCTION_TYPE,
    OPERATOR_UNKNOWN,
    GS_GE_EXPR,
    OPR_GE,
    GS_GOTO_EXPR,
    OPERATOR_UNKNOWN,
    GS_GT_EXPR,
    OPR_GT,
    GS_IDENTIFIER_NODE,
    OPERATOR_UNKNOWN,
    GS_IMAGPART_EXPR,
    OPR_SECONDPART,
    GS_INDIRECT_REF,
    OPERATOR_UNKNOWN,
    GS_INIT_EXPR,
    OPERATOR_UNKNOWN,
    GS_INTEGER_CST,
    OPERATOR_UNKNOWN,
    GS_INTEGER_TYPE,
    OPERATOR_UNKNOWN,
    GS_LABEL_DECL,
    OPERATOR_UNKNOWN,
    GS_LABEL_EXPR,
    OPERATOR_UNKNOWN,
    GS_LANG_TYPE,
    OPERATOR_UNKNOWN,
    GS_LE_EXPR,
    OPR_LE,
    GS_LOOP_EXPR,
    OPERATOR_UNKNOWN,
    GS_LROTATE_EXPR,
    OPR_RROTATE,
    GS_LSHIFT_EXPR,
    OPR_SHL,
    GS_LTGT_EXPR,
    OPERATOR_UNKNOWN,
    GS_LT_EXPR,
    OPR_LT,
    GS_MAX_EXPR,
    OPR_MAX,
    GS_METHOD_TYPE,
    OPERATOR_UNKNOWN,
    GS_MINUS_EXPR,
    OPR_SUB,
    GS_MIN_EXPR,
    OPR_MIN,
    GS_MISALIGNED_INDIRECT_REF,
    OPERATOR_UNKNOWN,
    GS_MODIFY_EXPR,
    OPERATOR_UNKNOWN,
    GS_MULT_EXPR,
    OPR_MPY,
    GS_NAMESPACE_DECL,
    OPERATOR_UNKNOWN,
    GS_NEGATE_EXPR,
    OPR_NEG,
    GS_NE_EXPR,
    OPR_NE,
    GS_NON_LVALUE_EXPR,
    OPERATOR_UNKNOWN,
    GS_NOP_EXPR,
    OPERATOR_UNKNOWN,
    GS_OBJ_TYPE_REF,
    OPERATOR_UNKNOWN,
    GS_OFFSET_TYPE,
    OPERATOR_UNKNOWN,
    GS_ORDERED_EXPR,
    OPERATOR_UNKNOWN,
    GS_PARM_DECL,
    OPERATOR_UNKNOWN,
    GS_PHI_NODE,
    OPERATOR_UNKNOWN,
    GS_PLACEHOLDER_EXPR,
    OPERATOR_UNKNOWN,
    GS_PLUS_EXPR,
    OPR_ADD,
    GS_POINTER_TYPE,
    OPERATOR_UNKNOWN,
    GS_POLYNOMIAL_CHREC,
    OPERATOR_UNKNOWN,
    GS_POSTDECREMENT_EXPR,
    OPR_SUB,
    GS_POSTINCREMENT_EXPR,
    OPR_ADD,
    GS_PREDECREMENT_EXPR,
    OPR_SUB,
    GS_PREINCREMENT_EXPR,
    OPR_ADD,
    GS_PROGRAM,
    OPERATOR_UNKNOWN,
    GS_QUAL_UNION_TYPE,
    OPERATOR_UNKNOWN,
    GS_RANGE_EXPR,
    OPERATOR_UNKNOWN,
    GS_RDIV_EXPR,
    OPR_DIV,
    GS_REALIGN_LOAD_EXPR,
    OPERATOR_UNKNOWN,
    GS_REALPART_EXPR,
    OPR_FIRSTPART,
    GS_REAL_CST,
    OPERATOR_UNKNOWN,
    GS_REAL_TYPE,
    OPERATOR_UNKNOWN,
    GS_RECORD_TYPE,
    OPERATOR_UNKNOWN,
    GS_REFERENCE_TYPE,
    OPERATOR_UNKNOWN,
    GS_RESULT_DECL,
    OPERATOR_UNKNOWN,
    GS_RESX_EXPR,
    OPERATOR_UNKNOWN,
    GS_RETURN_EXPR,
    OPERATOR_UNKNOWN,
    GS_ROUND_DIV_EXPR,
    OPERATOR_UNKNOWN,
    GS_ROUND_MOD_EXPR,
    OPERATOR_UNKNOWN,
    GS_RROTATE_EXPR,
    OPR_RROTATE,
    GS_RSHIFT_EXPR,
    OPERATOR_UNKNOWN,
    GS_SAVE_EXPR,
    OPERATOR_UNKNOWN,
    GS_SCEV_KNOWN,
    OPERATOR_UNKNOWN,
    GS_SCEV_NOT_KNOWN,
    OPERATOR_UNKNOWN,
    GS_SCOPE,
    OPERATOR_UNKNOWN,
    GS_SCOPE_STMT,
    OPERATOR_UNKNOWN,
    GS_SIZEOF_EXPR,
    OPERATOR_UNKNOWN,
    GS_SSA_NAME,
    OPERATOR_UNKNOWN,
    GS_STATEMENT_LIST,
    OPERATOR_UNKNOWN,
    GS_STMT_EXPR,
    OPERATOR_UNKNOWN,
    GS_STRING_CST,
    OPERATOR_UNKNOWN,
    GS_SWITCH_EXPR,
    OPERATOR_UNKNOWN,
    GS_SWITCH_STMT,
    OPERATOR_UNKNOWN,
    GS_TARGET_EXPR,
    OPERATOR_UNKNOWN,
    GS_TRANSLATION_UNIT_DECL,
    OPERATOR_UNKNOWN,
    GS_TREE_BINFO,
    OPERATOR_UNKNOWN,
    GS_TREE_LIST,
    OPERATOR_UNKNOWN,
    GS_TREE_VEC,
    OPERATOR_UNKNOWN,
    GS_TRUNC_DIV_EXPR,
    OPR_DIV,
    GS_TRUNC_MOD_EXPR,
    OPR_REM,
    GS_TRUTH_ANDIF_EXPR,
    OPR_CAND,
    GS_TRUTH_AND_EXPR,
    OPR_BAND,
    GS_TRUTH_NOT_EXPR,
    OPR_LNOT,
    GS_TRUTH_ORIF_EXPR,
    OPR_CIOR,
    GS_TRUTH_OR_EXPR,
    OPR_BIOR,
    GS_TRUTH_XOR_EXPR,
    OPR_BXOR,
    GS_TRY_BLOCK,
    OPERATOR_UNKNOWN,
    GS_TRY_CATCH_EXPR,
    OPERATOR_UNKNOWN,
    GS_TRY_FINALLY_EXPR,
    OPERATOR_UNKNOWN,
    GS_TYPE_DECL,
    OPERATOR_UNKNOWN,
    GS_UNEQ_EXPR,
    OPERATOR_UNKNOWN,
    GS_UNGE_EXPR,
    OPERATOR_UNKNOWN,
    GS_UNGT_EXPR,
    OPERATOR_UNKNOWN,
    GS_UNION_TYPE,
    OPERATOR_UNKNOWN,
    GS_UNLE_EXPR,
    OPERATOR_UNKNOWN,
    GS_UNLT_EXPR,
    OPERATOR_UNKNOWN,
    GS_UNORDERED_EXPR,
    OPERATOR_UNKNOWN,
    GS_VALUE_HANDLE,
    OPERATOR_UNKNOWN,
    GS_VAR_DECL,
    OPERATOR_UNKNOWN,
    GS_VA_ARG_EXPR,
    OPERATOR_UNKNOWN,
    GS_VECTOR_CST,
    OPERATOR_UNKNOWN,
    GS_VECTOR_TYPE,
    OPERATOR_UNKNOWN,
    GS_VEC_COND_EXPR,
    OPERATOR_UNKNOWN,
    GS_VIEW_CONVERT_EXPR,
    OPERATOR_UNKNOWN,
    GS_VOID_TYPE,
    OPERATOR_UNKNOWN,
    GS_WHILE_STMT,
    OPERATOR_UNKNOWN,
    GS_WITH_CLEANUP_EXPR,
    OPERATOR_UNKNOWN,
    GS_WITH_SIZE_EXPR,
    OPERATOR_UNKNOWN,

    GS_AGGR_INIT_EXPR,
    OPERATOR_UNKNOWN,
    GS_BASELINK,
    OPERATOR_UNKNOWN,
    GS_BOUND_TEMPLATE_TEMPLATE_PARM,
    OPERATOR_UNKNOWN,
    GS_CAST_EXPR,
    OPERATOR_UNKNOWN,
    GS_CONST_CAST_EXPR,
    OPERATOR_UNKNOWN,
    GS_CLEANUP_STMT,
    OPERATOR_UNKNOWN,
    GS_CTOR_INITIALIZER,
    OPERATOR_UNKNOWN,
    GS_DELETE_EXPR,
    OPERATOR_UNKNOWN,
    GS_DEFAULT_ARG,
    OPERATOR_UNKNOWN,
    GS_DYNAMIC_CAST_EXPR,
    OPERATOR_UNKNOWN,
    GS_DOTSTAR_EXPR,
    OPERATOR_UNKNOWN,
    GS_EMPTY_CLASS_EXPR,
    OPERATOR_UNKNOWN,
    GS_EH_SPEC_BLOCK,
    OPERATOR_UNKNOWN,
    GS_HANDLER,
    OPERATOR_UNKNOWN,
    GS_IF_STMT,
    OPERATOR_UNKNOWN,
    GS_MEMBER_REF,
    OPERATOR_UNKNOWN,
    GS_MODOP_EXPR,
    OPERATOR_UNKNOWN,
    GS_MUST_NOT_THROW_EXPR,
    OPERATOR_UNKNOWN,
    GS_NEW_EXPR,
    OPERATOR_UNKNOWN,
    GS_NON_DEPENDENT_EXPR,
    OPERATOR_UNKNOWN,
    GS_OFFSET_REF,
    OPERATOR_UNKNOWN,
    GS_OFFSETOF_EXPR,
    OPERATOR_UNKNOWN,
    GS_OVERLOAD,
    OPERATOR_UNKNOWN,
    GS_PTRMEM_CST,
    OPERATOR_UNKNOWN,
    GS_PSEUDO_DTOR_EXPR,
    OPERATOR_UNKNOWN,
    GS_REINTERPRET_CAST_EXPR,
    OPERATOR_UNKNOWN,
    GS_SCOPE_REF,
    OPERATOR_UNKNOWN,
    GS_STATIC_CAST_EXPR,
    OPERATOR_UNKNOWN,
    GS_TAG_DEFN,
    OPERATOR_UNKNOWN,
    GS_TEMPLATE_DECL,
    OPERATOR_UNKNOWN,
    GS_TEMPLATE_ID_EXPR,
    OPERATOR_UNKNOWN,
    GS_TEMPLATE_PARM_INDEX,
    OPERATOR_UNKNOWN,
    GS_TEMPLATE_TYPE_PARM,
    OPERATOR_UNKNOWN,
    GS_THROW_EXPR,
    OPERATOR_UNKNOWN,
    GS_TINST_LEVEL,
    OPERATOR_UNKNOWN,
    GS_TYPEOF_TYPE,
    OPERATOR_UNKNOWN,
    GS_TYPENAME_TYPE,
    OPERATOR_UNKNOWN,
    GS_TYPE_EXPR,
    OPERATOR_UNKNOWN,
    GS_TYPEID_EXPR,
    OPERATOR_UNKNOWN,
    GS_USING_DECL,
    OPERATOR_UNKNOWN,
    GS_USING_STMT,
    OPERATOR_UNKNOWN,
    GS_UNBOUND_CLASS_TEMPLATE,
    OPERATOR_UNKNOWN,
    GS_VEC_DELETE_EXPR,
    OPERATOR_UNKNOWN,
    GS_VEC_NEW_EXPR,
    OPERATOR_UNKNOWN,
    GS_TEMPLATE_TEMPLATE_PARM,
    OPERATOR_UNKNOWN,
};

// KEY bug 11288: support for anonymous unions:
// ---------------------------------------------
// GNU3 based front-end:
// For an anonymous union, all the members must be expanded together (cf.
// expand_anon_union_decl() in GNU), the members will actually just point
// to the ST for the union itself, so that whenever we access a member,
// we access the same variable.
//
// GNU4 based front-end:
// The above does not work on g++4 based front-end. The above method actually
// cannot generate correct code for anything but trivial anon unions, one of
// the reasons being it does not have the capability to use correct type
// information.
// There is a var_decl for each of the top-level fields of an anon union.
// In g++4, such var_decl's have a decl_value_expr field, that points to the
// actual expression to use (instead of the var_decl). In WHIRL, for any
// variable having this field, we should instead use its decl_value_expr.

static bool WGEN_Call_Returns_Ptr_To_Member_Func(gs_t exp);

static WN *WGEN_Expand_Ptr_To_Member_Func_Call_Expr(gs_t exp, TY_IDX nop_ty_idx,
                                                    TYPE_ID rtype, TYPE_ID desc,
                                                    WN_OFFSET offset = 0,
                                                    UINT field_id = 0);

// The words in 'buf' are in target order. Convert them to host order
// in place. 'buf' is a two word array.
void WGEN_Convert_To_Host_Order(long *buf) {
  if (!Same_Byte_Sex) {
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
static void WGEN_add_guard_var(gs_t guard_var, WN *value_wn) {
  WN *stid, *comma;

  // Set the guard variable to 0 before the condition is evaluated.
  WN *zero_wn = WN_Intconst(MTYPE_I4, 0);
  stid = WN_Stid(MTYPE_I4, 0, Get_ST(guard_var), MTYPE_To_TY(MTYPE_I4), zero_wn,
                 0);
  WGEN_Stmt_Append(stid, Get_Srcpos());

  // Set the guard variable to 1 while evaluating the value of the conditional
  // expression.
  WN *one_wn = WN_Intconst(MTYPE_I4, 1);
  stid =
      WN_Stid(MTYPE_I4, 0, Get_ST(guard_var), MTYPE_To_TY(MTYPE_I4), one_wn, 0);
  if (WN_operator(value_wn) == OPR_COMMA) {
    comma = value_wn;
  } else if (WN_operator(WN_kid0(value_wn)) == OPR_COMMA) {
    comma = WN_kid0(value_wn);
  } else {
    // Create a comma.
    WN *wn0 = WN_CreateBlock();
    WN *wn1 = WN_kid0(value_wn);
    WN_Set_Linenum(wn0, Get_Srcpos());
    comma = WN_CreateComma(OPR_COMMA, WN_rtype(wn1), MTYPE_V, wn0, wn1);
    WN_kid0(value_wn) = comma;
  }
  WN *wn = WN_kid0(comma);
  FmtAssert(WN_operator(wn) == OPR_BLOCK,
            ("WGEN_add_guard_var: unexpected WN operator"));
  WN_INSERT_BlockFirst(wn, stid);
}

// check whether the WHIRL operator has subsumed cvtl in its semantics
// (intended only for integer operations)
bool Has_Subsumed_Cvtl(OPERATOR opr) {
  if (OPERATOR_is_load(opr) || OPERATOR_is_leaf(opr))
    return TRUE;
  if (opr == OPR_CVT || opr == OPR_CVTL || opr == OPR_TAS)
    return TRUE;
  if (opr == OPR_EQ || opr == OPR_NE || opr == OPR_GE || opr == OPR_GT ||
      opr == OPR_LE || opr == OPR_LT || opr == OPR_LNOT || opr == OPR_LAND ||
      opr == OPR_LIOR || opr == OPR_CAND || opr == OPR_CIOR)
    return TRUE;
  return FALSE;
}

// Round up an object size to the size it would require in the parameter
// area on the stack.  This is defined to be the difference between its
// start address and the lowest possible starting address of the next parameter.
inline UINT64 Parameter_Size(UINT64 sz) {
#if WORDS_BIG_ENDIAN
  return sz;
#else
  INT UNITS_PER_WORD = TARGET_64BIT ? 8 : 4;
  return (sz + UNITS_PER_WORD - 1) & ~(UNITS_PER_WORD - 1);
#endif
}

TYPE_ID
Widen_Mtype(TYPE_ID t) {
  if (MTYPE_is_m(t))
    return t;
  if (MTYPE_is_void(t) || t == MTYPE_BS) {
    Fail_FmtAssertion("Widen_Mtype: for MTYPE_V or MTYPE_BS");
    return t;
  }
  if (MTYPE_byte_size(t) >= 4)
    return t;
  return Mtype_TransferSize(MTYPE_I4, t);
}

// Traverse the tree to see if the address of a variable is being taken

void WGEN_Set_ST_Addr_Saved(WN *wn) {
  OPERATOR Operator;
  ST *st;

  Operator = WN_operator(wn);

  switch (Operator) {

  case OPR_LDA:
  case OPR_LDMA:

    st = WN_st(wn);

    if (ST_class(st) == CLASS_VAR || ST_class(st) == CLASS_FUNC)
      Set_ST_addr_saved(st);
    break;

  case OPR_ARRAY:

    WGEN_Set_ST_Addr_Saved(WN_kid0(wn));
    break;

  case OPR_LDID:

    st = WN_st(wn);
    if (ST_pt_to_unique_mem(st))
      Clear_ST_pt_to_unique_mem(st);
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

    WGEN_Set_ST_Addr_Saved(WN_kid0(wn));
    break;

  case OPR_CSELECT:

    WGEN_Set_ST_Addr_Saved(WN_kid1(wn));
    WGEN_Set_ST_Addr_Saved(WN_kid2(wn));
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

    WGEN_Set_ST_Addr_Saved(WN_kid0(wn));
    WGEN_Set_ST_Addr_Saved(WN_kid1(wn));
    break;

  case OPR_CAND:
  case OPR_CIOR:

    break;

  case OPR_COMMA:

    WGEN_Set_ST_Addr_Saved(WN_kid1(wn));
    break;

  case OPR_RCOMMA:

    WGEN_Set_ST_Addr_Saved(WN_kid0(wn));
    break;

  default:

    DevWarn("WGEN_Set_ST_Addr_Saved not implemented");
  }
} /* WGEN_Set_ST_Addr_Saved */

typedef struct wgen_save_expr_t {
  gs_t exp;
  ST *st;
#ifdef KEY
  INT32 level; // to identify which cleanup the save expr belongs to
#endif
} WGEN_SAVE_EXPR;

WGEN_SAVE_EXPR *wgen_save_expr_stack = NULL;
INT32 wgen_save_expr_stack_last = -1;
INT32 wgen_save_expr_stack_max = 0;

#ifdef KEY
INT32 wgen_save_expr_level;      // identify the current cleanup
INT32 wgen_last_save_expr_level; // the last cleanup level used
#endif

static WN *WGEN_Save_Expr(gs_t save_exp, bool need_result, TY_IDX nop_ty_idx,
                          TY_IDX component_ty_idx, INT64 component_offset,
                          UINT16 field_id) {
  INT32 i;
  gs_t exp = gs_tree_operand(save_exp, 0);
  TY_IDX ty_idx = Get_TY(gs_tree_type(exp));
  TYPE_ID mtype = TY_mtype(ty_idx);
  ST *st;
  WN *wn;
  bool found = false;

  for (i = wgen_save_expr_stack_last; i >= 0; i--) {
#ifndef KEY
    if (wgen_save_expr_stack[i].exp == exp) {
#else
    if (wgen_save_expr_stack[i].exp == save_exp &&
        wgen_save_expr_stack[i].level == wgen_save_expr_level) {
#endif
      st = wgen_save_expr_stack[i].st;
      FmtAssert(st != 0, ("WGEN_Save_Expr: st not yet assigned"));
      found = true;
      break;
    }
  }

  if (!found) { // czw 1.22
    //{
    i = ++wgen_save_expr_stack_last;
    if (i == wgen_save_expr_stack_max) {
      if (wgen_save_expr_stack == NULL) {
        wgen_save_expr_stack_max = 32;
        wgen_save_expr_stack = (WGEN_SAVE_EXPR *)malloc(
            wgen_save_expr_stack_max * sizeof(WGEN_SAVE_EXPR));
      } else {
        wgen_save_expr_stack_max =
            wgen_save_expr_stack_max + (wgen_save_expr_stack_max >> 1);
        wgen_save_expr_stack = (WGEN_SAVE_EXPR *)realloc(
            wgen_save_expr_stack,
            wgen_save_expr_stack_max * sizeof(WGEN_SAVE_EXPR));
      }
    }
#ifndef KEY
    wgen_save_expr_stack[i].exp = exp;
#else
    wgen_save_expr_stack[i].exp = save_exp;
    wgen_save_expr_stack[i].level = wgen_save_expr_level;
#endif
    wgen_save_expr_stack[i].st = 0;
#ifdef KEY
    // If exp is a CALL_EXPR that returns a ptr-to-member-function, then call
    // WGEN_Expand_Ptr_To_Member_Func_Call_Expr to expand it.  Otherwise, call
    // WGEN_Expand_Expr to do regular expansion.  Bug 3400.
    if (WGEN_Call_Returns_Ptr_To_Member_Func(exp)) {
      TYPE_ID desc = TY_mtype(Get_TY(gs_tree_type(exp)));
      wn = WGEN_Expand_Ptr_To_Member_Func_Call_Expr(exp, nop_ty_idx,
                                                    Widen_Mtype(desc), desc);
    } else
#endif
      wn = WGEN_Expand_Expr(exp);

    st = Gen_Temp_Symbol(ty_idx, "__save_expr");
#if 0 // TODO
#ifdef KEY
    WGEN_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, st);
#endif
#endif
    WGEN_Set_ST_Addr_Saved(wn);
    wn = WN_Stid(mtype, 0, st, ty_idx, wn);
    WGEN_Stmt_Append(wn, Get_Srcpos());
    wgen_save_expr_stack[i].st = st;
  }

  if (component_ty_idx == 0)
    wn = WN_Ldid(mtype, 0, st, ty_idx);
  else {
    TYPE_ID desc = TY_mtype(component_ty_idx);
    TYPE_ID rtype = Widen_Mtype(desc);
    wn = WN_CreateLdid(OPR_LDID, rtype, desc, component_offset, st,
                       field_id ? ty_idx : component_ty_idx, field_id);
  }
  return wn;
} /* WGEN_Save_Expr */

/* process the tree doing array indicing and return the WN that performs
 * the address computation; ty_idx returns the high-level array type if it
 * is a DECL node, and the element type if it is an ARRAY_REF node.
 */
static WN *WGEN_Array_Expr(gs_t exp, TY_IDX *ty_idx, TY_IDX component_ty_idx,
                           INT64 component_offset, UINT32 field_id) {
  WN *wn;
  gs_code_t code = gs_tree_code(exp);
  if (code == GS_COMPONENT_REF) {
    TY_IDX ty_idx0;
    gs_t arg0 = gs_tree_operand(exp, 0);
    gs_t arg1 = gs_tree_operand(exp, 1);
    if (component_ty_idx == 0)
      ty_idx0 = Get_TY(gs_tree_type(exp));
    else
      ty_idx0 = component_ty_idx;
#ifdef KEY // bug 10728
    if (gs_tree_this_volatile(exp))
      Set_TY_is_volatile(ty_idx0);
#endif
    Is_True(
        !gs_decl_bit_field(arg1),
        ("WGEN_Array_Expr: address arithmetic cannot use bitfield addresses"));
    INT64 ofst =
        (BITSPERBYTE * gs_get_integer_value(gs_decl_field_offset(arg1)) +
         gs_get_integer_value(gs_decl_field_bit_offset(arg1))) /
        BITSPERBYTE;
#ifdef KEY
    // Refer GCC 4.0.2: gcc.c-torture/compile/struct-non-lval-3.c
    // We only handle this case so far:
    // (p = q).x[index]
    // the lhs of modify_expr is var_decl, not an expression
    if (gs_tree_code(arg0) == GS_MODIFY_EXPR) {
      WGEN_Expand_Expr(arg0);
      gs_t lhs = gs_tree_operand(arg0, 0);
      Is_True(lhs != NULL && gs_tree_code(lhs) == GS_VAR_DECL,
              ("Unsupported lhs for `(p=q).x[n]'"));
      arg0 = lhs;
    }
#endif

#ifdef KEY // bug 9725: If the field is an array of struct, it is considered
           // a single field.
    return WGEN_Array_Expr(arg0, ty_idx, ty_idx0, ofst + component_offset,
                           DECL_FIELD_ID(arg1));
#else
    return WGEN_Array_Expr(arg0, ty_idx, ty_idx0, ofst + component_offset,
                           field_id + DECL_FIELD_ID(arg1));
#endif
  }
#ifdef KEY
  else if (code == GS_VAR_DECL && gs_decl_value_expr(exp)) {
    return WGEN_Array_Expr(gs_decl_value_expr(exp), ty_idx, component_ty_idx,
                           component_offset, field_id);
  }
#endif
  else if (code == GS_VAR_DECL || code == GS_PARM_DECL
#ifdef KEY
           || code == GS_RESULT_DECL
#endif
  ) {
    ST *st = Get_ST(exp);
    ST *base_st = ST_base(st);
    // for VLAs the instead of using the ST use its base st
    // also for the time being do not support VLAs within structs
    if (st != base_st) {
      FmtAssert(
          component_ty_idx == 0,
          ("Variable Length Arrays within struct not currently implemented"));
      wn = WN_Ldid(Pointer_Mtype, 0, base_st, ST_type(base_st));
    } else
      wn = WN_Lda(Pointer_Mtype, ST_ofst(st) + component_offset, st, field_id);
    if (component_ty_idx == 0)
      *ty_idx = ST_type(st);
    else {
      *ty_idx = component_ty_idx;
      if (TY_align(ST_type(st)) < TY_align(component_ty_idx))
        Set_TY_align(*ty_idx,
                     TY_align(ST_type(st))); // pick more stringent align
    }
    Is_True(TY_kind(*ty_idx) == KIND_ARRAY,
            ("WGEN_Array_Expr: ARRAY_REF base not of type KIND_ARRAY"));
    return wn;
  } else if (code == GS_CONSTRUCTOR) {
    ST *st = WGEN_Generate_Temp_For_Initialized_Aggregate(exp, "");
    wn = WN_Lda(Pointer_Mtype, ST_ofst(st) + component_offset, st, field_id);
    if (component_ty_idx == 0)
      *ty_idx = ST_type(st);
    else
      *ty_idx = component_ty_idx;
    Is_True(TY_kind(*ty_idx) == KIND_ARRAY,
            ("WGEN_Array_Expr: ARRAY_REF base not of type KIND_ARRAY"));
    return wn;
  } else if (code == GS_STRING_CST) {
    wn = WGEN_Expand_Expr(exp);
    *ty_idx = ST_type(TREE_STRING_ST(exp));
    return wn;
  } else if (code == GS_INDIRECT_REF) {
    wn = WGEN_Expand_Expr(gs_tree_operand(exp, 0));
    if (component_ty_idx == 0)
      *ty_idx = Get_TY(gs_tree_type(exp));
    else {
      *ty_idx = component_ty_idx;
      INT node_align = gs_type_align(gs_tree_type(exp)) / BITSPERBYTE;
      if (node_align < TY_align(component_ty_idx))
        Set_TY_align(*ty_idx, node_align); // pick more stringent align
    }
    if (component_offset != 0) { // TODO: use ILDA instead
      WN *wn0 = WN_Intconst(MTYPE_I4, component_offset);
      wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn, wn0);
    }
    return wn;
  } else if (code == GS_CALL_EXPR) {
    wn = WGEN_Expand_Expr(exp);
    FmtAssert(WN_opcode(wn) == OPC_MCOMMA,
              ("opcode other than OPC_MCOMMA for call underneath ARRAY_REF"));
    WGEN_Stmt_Append(WN_kid0(wn), Get_Srcpos());
    ST *st = WN_st(WN_kid1(wn));
    WN_Delete(WN_kid1(wn));
    WN_Delete(wn);
    wn = WN_Lda(Pointer_Mtype, ST_ofst(st) + component_offset, st, field_id);
    if (component_ty_idx == 0)
      *ty_idx = ST_type(st);
    else {
      *ty_idx = component_ty_idx;
      if (TY_align(ST_type(st)) < TY_align(component_ty_idx))
        Set_TY_align(*ty_idx,
                     TY_align(ST_type(st))); // pick more stringent align
    }
    Is_True(TY_kind(*ty_idx) == KIND_ARRAY,
            ("WGEN_Array_Expr: ARRAY_REF base not of type KIND_ARRAY"));
    return wn;
  } else if (code == GS_ARRAY_REF) { // recursive call
    WN *wn0, *wn1, *wn2;
    TY_IDX ty_idx0;
#ifdef KEY // Bug 5831.
    wn0 = WGEN_Array_Expr(gs_tree_operand(exp, 0), &ty_idx0, 0,
                          component_offset, field_id);
#else
    wn0 = WGEN_Array_Expr(gs_tree_operand(exp, 0), &ty_idx0, component_ty_idx,
                          component_offset, field_id);
#endif
    Is_True(TY_kind(ty_idx0) == KIND_ARRAY,
            ("WGEN_Array_Expr: arg 0 of ARRAY_REF not of type KIND_ARRAY"));
    ARB_HANDLE arb = TY_arb(ty_idx0);
    if (ARB_dimension(arb) == 1 && ARB_first_dimen(arb) &&
        ARB_last_dimen(arb) && ARB_const_lbnd(arb)) {
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
      wn2 = WGEN_Expand_Expr(gs_tree_operand(exp, 1));
#ifdef TARG_X8664 // bug 11705
      if (WN_operator(wn2) == OPR_SUB)
        WN_set_rtype(wn2, Mtype_TransferSign(MTYPE_I4, WN_rtype(wn2)));
#endif
#ifdef KEY
      // Expand the current dimension by growing the array just expanded.  Bug
      // 4692.
      if (gs_tree_code(gs_tree_operand(exp, 0)) == GS_ARRAY_REF) {
        Is_True(WN_operator(wn0) == OPR_ARRAY,
                ("WGEN_Array_Expr: ARRAY_REF not translated to OPR_ARRAY"));
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

      WN_element_size(wn) = TY_size(Get_TY(gs_tree_type(exp)));
    } else
      Is_True(FALSE, ("WGEN_Array_Expr: only const-bounds 1-dimension arrays "
                      "handled now"));
    if (component_ty_idx == 0) {
      *ty_idx = TY_etype(ty_idx0);
      if (TY_align(ty_idx0) < TY_align(*ty_idx))
        Set_TY_align(*ty_idx, TY_align(ty_idx0)); // pick more stringent align
#ifdef KEY                                        // bug 10728
      if (gs_tree_this_volatile(exp))
        Set_TY_is_volatile(*ty_idx);
#endif
    } else
      *ty_idx = component_ty_idx;
    return wn;
  }
#ifdef KEY
  else if (code == GS_COMPOUND_LITERAL_EXPR) {
    gs_t arg0 = gs_decl_initial(gs_tree_operand(gs_tree_operand(exp, 0), 0));
    ST *st = WGEN_Generate_Temp_For_Initialized_Aggregate(arg0, "");
    wn = WN_Lda(Pointer_Mtype, ST_ofst(st), st);
    *ty_idx = component_ty_idx == 0 ? ST_type(st) : component_ty_idx;
    return wn;
  } else if (code == GS_TARGET_EXPR) {
    wn = WGEN_Expand_Expr(exp);
    Is_True(WN_operator(wn) == OPR_LDID,
            ("WGEN_Array_Expr: OPR_LDID not found"));
    ST *st = WN_st(wn);
    wn = WN_Lda(Pointer_Mtype, ST_ofst(st) + component_offset, st, field_id);
    if (component_ty_idx == 0)
      *ty_idx = ST_type(st);
    else {
      *ty_idx = component_ty_idx;
      if (TY_align(ST_type(st)) < TY_align(component_ty_idx))
        Set_TY_align(*ty_idx,
                     TY_align(ST_type(st))); // pick more stringent align
    }
    Is_True(TY_kind(*ty_idx) == KIND_ARRAY,
            ("WGEN_Array_Expr: ARRAY_REF base not of type KIND_ARRAY"));
    return wn;
  }
#endif                                 /* KEY */
  else if (code == GS_COMPOUND_EXPR) { // wgen
    wn = WGEN_Expand_Expr(gs_tree_operand(exp, 0), FALSE);
    if (wn && WN_has_side_effects(wn)) {
      wn = WN_CreateEval(wn);
      WGEN_Stmt_Append(wn, Get_Srcpos());
    }
    return WGEN_Array_Expr(gs_tree_operand(exp, 1), ty_idx, component_ty_idx,
                           component_offset, field_id);
  } else if (code == GS_COND_EXPR) { // wgen for bug 9658
    WN *wn1, *wn2;
    wn = WGEN_Expand_Expr(gs_tree_operand(exp, 0));
    wn1 = WGEN_Array_Expr(gs_tree_operand(exp, 1), ty_idx, component_ty_idx,
                          component_offset, field_id);
    wn2 = WGEN_Array_Expr(gs_tree_operand(exp, 2), ty_idx, component_ty_idx,
                          component_offset, field_id);
    Set_PU_has_very_high_whirl(Get_Current_PU());
    return WN_CreateExp3(OPR_CSELECT, WN_rtype(wn1), MTYPE_V, wn, wn1, wn2);
  } else {
    Is_True(FALSE, ("WGEN_Array_Expr: unsupported node for base of ARRAY_REF"));
    return NULL;
  }
}

#if 0
// wgen clean-up : Code to handle lowered GNU tree should not be needed
// any more.

// EXP is a RESULT_DECL that holds the function return value.  The
// value is to be returned in the memory pointed to by the fake first
// parm which was inserted by wgen.  This function converts the RESULT_DECL
// into an INDIRECT_REF of it, and makes it write into the first fake
// parameter.
void
WGEN_fixup_result_decl (gs_t exp)
{
  Is_True (gs_tree_code(exp) == GS_RESULT_DECL,
           ("WGEN_fixup_result_decl: expected result_decl node"));
  // gs_tree_type(exp) need not be the same as
  //             gs_tree_type(gs_tree_type(Current_Function_Decl()))

  // build a new result_decl node
  gs_t ptr_var = gs_build_decl (GS_RESULT_DECL,
                                gs_build_pointer_type(gs_tree_type(exp)));
  // convert current node to indirect_ref
  _gs_code(exp, GS_INDIRECT_REF);
  gs_set_tree_operand (exp, 0, ptr_var);

  // set ST
  WN *first_formal = WN_formal(Current_Entry_WN(), 0);
  set_DECL_ST (ptr_var, WN_st(first_formal));
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
WN *WGEN_Lhs_Of_Modify_Expr(gs_code_t assign_code, gs_t lhs, WN *lhs_retval,
                            bool need_result, TY_IDX component_ty_idx,
                            INT64 component_offset, UINT32 field_id,
                            bool is_bit_field, WN *rhs_wn,
                            PREG_NUM rhs_preg_num, bool is_realpart,
                            bool is_imagpart) {
  WN *wn = NULL;
  ST *st;
  bool result_in_temp = FALSE;
  ST *result_preg_st;
  PREG_NUM result_preg;
  PREG_NUM lhs_preg_num = 0;
  gs_code_t code = gs_tree_code(lhs);
  BOOL volt = FALSE;
  if (rhs_wn != NULL) {
    WGEN_Set_ST_Addr_Saved(rhs_wn);
  }

  switch (code) {
  case GS_COMPONENT_REF: {
    INT64 ofst;
    TY_IDX ty_idx0;

    gs_t arg0 = gs_tree_operand(lhs, 0);
    gs_t arg1 = gs_tree_operand(lhs, 1);
#ifdef GPLUSPLUS_FE
    // for g++ ensure that the WHIRL type for the enclosing structure has been
    // created in order to set the field id to a non zero value
    (void)Get_TY(gs_tree_type(arg0));
#endif /* GPLUSPLUS_FE */
    if (component_ty_idx == 0)
      ty_idx0 = Get_TY(gs_tree_type(lhs));
    else
      ty_idx0 = component_ty_idx;
    if (gs_decl_bit_field(arg1))
      is_bit_field = TRUE;
    if (!is_bit_field)
      ofst = (BITSPERBYTE * gs_get_integer_value(gs_decl_field_offset(arg1)) +
              gs_get_integer_value(gs_decl_field_bit_offset(arg1))) /
             BITSPERBYTE;
    else
      ofst = 0;
#ifdef KEY // bug 10422: check if the field is volatile, arg1 is FIELD_DECL
    if (gs_tree_this_volatile(arg1)) {
      Set_TY_is_volatile(ty_idx0);
      volt = TRUE;
    }
#if 1 // wgen bug 10470
    else {
      Clear_TY_is_volatile(ty_idx0);
      volt = FALSE;
    }
#endif
#endif
#ifdef KEY
    FmtAssert(DECL_FIELD_ID(arg1) != 0,
              ("WGEN_Lhs_Of_Modify_Expr: DECL_FIELD_ID used but not set"));
#endif
    wn = WGEN_Lhs_Of_Modify_Expr(
        assign_code, arg0, NULL, need_result, ty_idx0, ofst + component_offset,
        field_id + DECL_FIELD_ID(arg1), is_bit_field, rhs_wn, rhs_preg_num,
        is_realpart, is_imagpart);
  }
    return wn;

  case GS_REALPART_EXPR: {
    gs_t arg0 = gs_tree_operand(lhs, 0);
    TY_IDX ty_idx0 = Get_TY(gs_tree_type(arg0));
    wn = WGEN_Lhs_Of_Modify_Expr(assign_code, arg0, NULL, need_result, ty_idx0,
                                 component_offset, field_id, is_bit_field,
                                 rhs_wn, rhs_preg_num, TRUE, FALSE);
  }
    return wn;

  case GS_IMAGPART_EXPR: {
    gs_t arg0 = gs_tree_operand(lhs, 0);
    TY_IDX ty_idx0 = Get_TY(gs_tree_type(arg0));
    wn = WGEN_Lhs_Of_Modify_Expr(assign_code, arg0, NULL, need_result, ty_idx0,
                                 component_offset, field_id, is_bit_field,
                                 rhs_wn, rhs_preg_num, FALSE, TRUE);
  }
    return wn;

#ifdef KEY
  case GS_TARGET_EXPR: // bug 6907
  {
    WN *wn = WGEN_Expand_Expr(lhs);
    Is_True(WN_operator(wn) == OPR_LDID,
            ("WGEN_Lhs_Of_Modify_Expr: wrong operator from TARGET_EXPR"));
    st = WN_st(wn);
  }
    // fall through
#endif

  case GS_PARM_DECL:
  case GS_VAR_DECL:
  case GS_RESULT_DECL: {
#if 0 // def KEY
      // wgen clean-up : code to handle lowered GNU tree should not be
      // needed any more.
      if (code == GS_RESULT_DECL &&
          TY_return_in_mem(Get_TY
                           (gs_tree_type
                            (gs_tree_type(Current_Function_Decl()) ) ) ) )
      {
        WGEN_fixup_result_decl (lhs);
        wn = WGEN_Lhs_Of_Modify_Expr (assign_code, lhs, NULL, need_result,
                                      component_ty_idx, component_offset,
                                      field_id, is_bit_field, rhs_wn,
                                      rhs_preg_num, is_realpart, is_imagpart);
        break;
      }
#endif
    TY_IDX hi_ty_idx =
        Get_TY(gs_tree_type(lhs)); // type associated with field_id
    if (gs_tree_this_volatile(lhs)) {
      Set_TY_is_volatile(hi_ty_idx);
      volt = TRUE;
    }
#if 1 // wgen bug 10470
    else {
      Clear_TY_is_volatile(hi_ty_idx);
      volt = FALSE;
    }
#endif
    TY_IDX desc_ty_idx = component_ty_idx;
    if (desc_ty_idx == 0)
      desc_ty_idx = hi_ty_idx;
    if (TY_is_volatile(desc_ty_idx)) {
      Clear_TY_is_volatile(desc_ty_idx);
      volt = TRUE;
    }

#ifdef KEY
    if (code != GS_TARGET_EXPR) {
      gs_t actual_decl = NULL;
      if (code == GS_VAR_DECL && (actual_decl = gs_decl_value_expr(lhs))) {

        TY_IDX ty_idx0 = Get_TY(gs_tree_type(actual_decl));
        return WGEN_Lhs_Of_Modify_Expr(assign_code, actual_decl, NULL,
                                       need_result, ty_idx0, component_offset,
                                       field_id, is_bit_field, rhs_wn,
                                       rhs_preg_num, FALSE, FALSE);
      }

      st = Get_ST(lhs);
    }
#else
    st = Get_ST(lhs);
#endif

    if (ST_assigned_to_dedicated_preg(st)) {
      Set_TY_is_volatile(hi_ty_idx);
      volt = TRUE;
    }
    Is_True(!is_bit_field || field_id <= MAX_FIELD_ID,
            ("WGEN_Lhs_Of_Modify_Expr: field id for bit-field exceeds limit"));

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
      rhs_wn = WN_CreateLdid(OPR_LDID, rtype, desc, rhs_preg_num, rhs_st,
                             desc_ty_idx, 0);
    } else {
      WN *result_wn; // the result wn to be returned
      if (assign_code == GS_MODIFY_EXPR) {
        if (is_realpart)
          rhs_wn =
              WN_Binary(OPR_COMPLEX, rtype, rhs_wn,
                        WN_Unary(OPR_IMAGPART, Mtype_complex_to_real(rtype),
                                 WN_CreateLdid(OPR_LDID, rtype, desc,
                                               ST_ofst(st) + component_offset,
                                               st, hi_ty_idx, field_id)));
        else if (is_imagpart)
          rhs_wn =
              WN_Binary(OPR_COMPLEX, rtype,
                        WN_Unary(OPR_REALPART, Mtype_complex_to_real(rtype),
                                 WN_CreateLdid(OPR_LDID, rtype, desc,
                                               ST_ofst(st) + component_offset,
                                               st, hi_ty_idx, field_id)),
                        rhs_wn);
      } else {
        if (is_realpart)
          rhs_wn = WN_Binary(OPR_COMPLEX, rtype, rhs_wn,
                             WN_Floatconst(Mtype_complex_to_real(rtype), 0.0));
        else if (is_imagpart)
          rhs_wn = WN_Binary(OPR_COMPLEX, rtype,
                             WN_Floatconst(Mtype_complex_to_real(rtype), 0.0),
                             rhs_wn);
      }

      if (assign_code == GS_PREINCREMENT_EXPR ||
          assign_code == GS_PREDECREMENT_EXPR) {
        wn =
            WN_CreateLdid(OPR_LDID, rtype, desc, ST_ofst(st) + component_offset,
                          st, hi_ty_idx, field_id);
        rhs_wn =
            WN_Binary(Operator_From_Tree[assign_code].opr, rtype, wn, rhs_wn);
        result_wn = rhs_wn;
      } else if (assign_code == GS_POSTINCREMENT_EXPR ||
                 assign_code == GS_POSTDECREMENT_EXPR) {
        result_wn =
            WN_CreateLdid(OPR_LDID, rtype, desc, ST_ofst(st) + component_offset,
                          st, hi_ty_idx, field_id);
      } else
        result_wn = rhs_wn;

      if (need_result &&
          (volt || assign_code == GS_POSTINCREMENT_EXPR ||
           assign_code == GS_POSTDECREMENT_EXPR)) { // save result in a preg
        result_in_temp = TRUE;
        result_preg_st = MTYPE_To_PREG(rtype);
        result_preg = Create_Preg(rtype, NULL);
        wn = WN_Stid(rtype, result_preg, result_preg_st, desc_ty_idx, result_wn,
                     0);
        WGEN_Stmt_Append(wn, Get_Srcpos());
        result_wn = WN_Ldid(rtype, result_preg, result_preg_st, desc_ty_idx, 0);
      }

      if (assign_code == GS_POSTINCREMENT_EXPR ||
          assign_code == GS_POSTDECREMENT_EXPR) {
        rhs_wn = WN_Binary(Operator_From_Tree[assign_code].opr, rtype,
                           result_wn, rhs_wn);
      } else
        rhs_wn = result_wn;

      // rhs_wn is now always the right-hand-side of the assignment
    }

    // the assignment
    if (!WGEN_Keep_Zero_Length_Structs && desc == MTYPE_M &&
        TY_size(hi_ty_idx) == 0) {
      // ignore zero length structs
    } else {
#ifdef KEY // bug 10422: check if the field is volatile
      if (volt)
        Set_TY_is_volatile(hi_ty_idx);
#endif
      wn = WN_Stid(desc, ST_ofst(st) + component_offset + lhs_preg_num, st,
                   hi_ty_idx, rhs_wn, field_id);
      WGEN_Stmt_Append(wn, Get_Srcpos());
    }
    if (need_result) {
      if (!result_in_temp)
        wn =
            WN_CreateLdid(OPR_LDID, rtype, desc, ST_ofst(st) + component_offset,
                          st, hi_ty_idx, field_id);
      else
        wn = WN_Ldid(rtype, result_preg, result_preg_st, desc_ty_idx, 0);
      if (is_realpart)
        wn = WN_Unary(OPR_REALPART, Mtype_complex_to_real(rtype), wn);
      else if (is_imagpart)
        wn = WN_Unary(OPR_IMAGPART, Mtype_complex_to_real(rtype), wn);
    } else
      wn = NULL;
  } break;

  case GS_INDIRECT_REF: {
    TY_IDX hi_ty_idx = Get_TY(gs_tree_type(lhs));
    if (gs_tree_this_volatile(lhs)) {
      Set_TY_is_volatile(hi_ty_idx);
      volt = TRUE;
    }
    gs_t op = gs_tree_operand(lhs, 0);

    WN *addr_wn = NULL;
    TY_IDX desc_ty_idx = component_ty_idx;
    if (desc_ty_idx == 0)
      desc_ty_idx = hi_ty_idx;
    if (TY_is_volatile(desc_ty_idx)) {
      Clear_TY_is_volatile(desc_ty_idx);
      volt = TRUE;
    }
    Is_True(!is_bit_field || field_id <= MAX_FIELD_ID,
            ("WGEN_Lhs_Of_Modify_Expr: field id for bit-field exceeds limit"));

    if (gs_tree_code(op) == GS_CALL_EXPR && lhs_retval != NULL) {
      // It's CALL_EXPR and the node has been expanded before expand the rhs.
      // The return address of the CALL_EXPR is stored in lhs_retval.
      // Refer WGEN_Expand_Expr : case GS_MODIFY_EXPR.
      // We must ensure the lhs_retval is LDID
      // Check TYPE and Operands?
      addr_wn = lhs_retval;
      FmtAssert(WN_operator(addr_wn) == OPR_LDID,
                ("Bad Operator for INDIRECT_REF-->CALL_EXPR. LDID expected."));
    } else {
      // Otherwise, we expand the lhs here.
      addr_wn = WGEN_Expand_Expr(gs_tree_operand(lhs, 0));
      if (WN_has_side_effects(addr_wn) &&
          (need_result || assign_code == GS_PREINCREMENT_EXPR ||
           assign_code == GS_PREDECREMENT_EXPR ||
           assign_code == GS_POSTINCREMENT_EXPR ||
           assign_code == GS_POSTDECREMENT_EXPR)) {
        ST *preg_st;
        PREG_NUM preg;
        TY_IDX address_ty_idx = Get_TY(gs_tree_type(gs_tree_operand(lhs, 0)));
#ifdef KEY
        // Bug 8738: PREG should NOT be VOLATILE in whirl
        if (TY_is_volatile(address_ty_idx)) {
          Clear_TY_is_volatile(address_ty_idx);
          volt = TRUE;
        }
#endif
        preg_st = MTYPE_To_PREG(Pointer_Mtype);
        preg = Create_Preg(Pointer_Mtype, NULL);
        wn = WN_Stid(Pointer_Mtype, preg, preg_st, address_ty_idx, addr_wn);
        WGEN_Set_ST_Addr_Saved(addr_wn);
#ifdef KEY
        // Handle function calls for asm input-output constraints
        // see torture test 990130-1.c
        WN *body = WGEN_Stmt_Top();
        if (body && // Do prepend only for asm's.  Bug 4732.
            WN_last(body) && WN_operator(WN_last(body)) == OPR_ASM_STMT) {
          WGEN_Stmt_Prepend_Last(wn, Get_Srcpos());
        } else
#endif /* KEY */
          WGEN_Stmt_Append(wn, Get_Srcpos());
        addr_wn = WN_Ldid(Pointer_Mtype, preg, preg_st, address_ty_idx);
      }
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
      rhs_wn = WN_CreateLdid(OPR_LDID, rtype, desc, rhs_preg_num, rhs_st,
                             desc_ty_idx, 0);
#ifdef KEY
      // Bug 8056: Need to preserve the semantics on the preg if it's size
      // is less than 4 bytes.
      if (MTYPE_byte_size(desc) < 4) {

        rhs_wn = WN_CreateCvtl(!MTYPE_signed(desc) ? OPC_U4CVTL : OPC_I4CVTL,
                               MTYPE_bit_size(desc), rhs_wn);
      }
#endif
    } else {
      WN *result_wn; // the result wn to be returned

      if (assign_code == GS_MODIFY_EXPR) {
        if (is_realpart)
          rhs_wn = WN_Binary(
              OPR_COMPLEX, rtype, rhs_wn,
              WN_Unary(OPR_IMAGPART, Mtype_complex_to_real(rtype),
                       WN_CreateIload(OPR_ILOAD, rtype, desc, component_offset,
                                      field_id != 0 ? hi_ty_idx : desc_ty_idx,
                                      Make_Pointer_Type(hi_ty_idx, FALSE),
                                      WN_COPY_Tree(addr_wn), field_id)));
        else if (is_imagpart)
          rhs_wn = WN_Binary(
              OPR_COMPLEX, rtype,
              WN_Unary(OPR_REALPART, Mtype_complex_to_real(rtype),
                       WN_CreateIload(OPR_ILOAD, rtype, desc, component_offset,
                                      field_id != 0 ? hi_ty_idx : desc_ty_idx,
                                      Make_Pointer_Type(hi_ty_idx, FALSE),
                                      WN_COPY_Tree(addr_wn), field_id)),
              rhs_wn);
      } else {
        if (is_realpart)
          rhs_wn = WN_Binary(OPR_COMPLEX, rtype, rhs_wn,
                             WN_Floatconst(Mtype_complex_to_real(rtype), 0.0));
        else if (is_imagpart)
          rhs_wn = WN_Binary(OPR_COMPLEX, rtype,
                             WN_Floatconst(Mtype_complex_to_real(rtype), 0.0),
                             rhs_wn);
      }

      if (assign_code == GS_PREINCREMENT_EXPR ||
          assign_code == GS_PREDECREMENT_EXPR) {
        wn = WN_CreateIload(OPR_ILOAD, rtype, desc, component_offset,
                            field_id != 0 ? hi_ty_idx : desc_ty_idx,
                            Make_Pointer_Type(hi_ty_idx, FALSE),
                            WN_COPY_Tree(addr_wn), field_id);
        rhs_wn =
            WN_Binary(Operator_From_Tree[assign_code].opr, rtype, wn, rhs_wn);
        result_wn = rhs_wn;
      } else if (assign_code == GS_POSTINCREMENT_EXPR ||
                 assign_code == GS_POSTDECREMENT_EXPR) {
        result_wn = WN_CreateIload(OPR_ILOAD, rtype, desc, component_offset,
                                   field_id != 0 ? hi_ty_idx : desc_ty_idx,
                                   Make_Pointer_Type(hi_ty_idx, FALSE),
                                   WN_COPY_Tree(addr_wn), field_id);
      } else
        result_wn = rhs_wn;

      if (need_result &&
          (volt || assign_code == GS_POSTINCREMENT_EXPR ||
           assign_code == GS_POSTDECREMENT_EXPR)) { // save result in a preg
        result_in_temp = TRUE;
        result_preg_st = MTYPE_To_PREG(rtype);
        result_preg = Create_Preg(rtype, NULL);
        wn = WN_Stid(rtype, result_preg, result_preg_st, desc_ty_idx, result_wn,
                     0);
        WGEN_Stmt_Append(wn, Get_Srcpos());
        ;
        result_wn = WN_Ldid(rtype, result_preg, result_preg_st, desc_ty_idx, 0);
      }

      if (assign_code == GS_POSTINCREMENT_EXPR ||
          assign_code == GS_POSTDECREMENT_EXPR) {
        rhs_wn = WN_Binary(Operator_From_Tree[assign_code].opr, rtype,
                           result_wn, rhs_wn);
      } else
        rhs_wn = result_wn;

      // rhs_wn is now always the right-hand-side of the assignment
    }

    // the assignment
    if (!WGEN_Keep_Zero_Length_Structs && desc == MTYPE_M &&
        TY_size(hi_ty_idx) == 0) {
      // ignore zero length structs
      if (WN_has_side_effects(addr_wn)) {
        wn = WN_CreateEval(addr_wn);
        WGEN_Stmt_Append(wn, Get_Srcpos());
      }
      wn = NULL;
    } else {
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
      gs_t addr = gs_tree_operand(lhs, 0);
      WN *first_formal = WN_formal(Current_Entry_WN(), 0);
      if (TY_return_in_mem(hi_ty_idx) && field_id == 0 &&
          // See if it is an indirect ref of the fake first parm.
          // bug fix for OSP_314
          //
          first_formal != NULL && (WN_operator(first_formal) != OPR_BLOCK) &&
          gs_tree_code(addr) == GS_VAR_DECL &&
          DECL_ST(addr) == WN_st(first_formal)) {
        FmtAssert(TY_mtype(hi_ty_idx) == MTYPE_M,
                  ("WGEN_Lhs_Of_Modify_Expr: return_in_mem type not MTYPE_M"));
        gs_t ptr_type = gs_tree_type(gs_tree_operand(lhs, 0));
        gs_t type = gs_tree_type(ptr_type);
        FmtAssert(gs_tree_code(ptr_type) == GS_POINTER_TYPE,
                  ("WGEN_Lhs_Of_Modify_Expr: INDIRECT_REF opnd0 is not "
                   "POINTER_TYPE"));
        FmtAssert(component_offset == 0,
                  ("WGEN_Lhs_Of_Modify_Expr: component_offset nonzero"));
        TY_IDX tidx = Get_TY(ptr_type);
        // Check object has no copy constructor.
        FmtAssert(!WGEN_has_copy_constructor(type),
                  ("WGEN_Lhs_Of_Modify_Expr: object needs copy constructor"));
      }
#endif
      wn = WN_CreateIstore(OPR_ISTORE, MTYPE_V, desc, component_offset,
                           Make_Pointer_Type(hi_ty_idx, FALSE), rhs_wn, addr_wn,
                           field_id);
      WGEN_Stmt_Append(wn, Get_Srcpos());
      if (need_result) {
        if (!result_in_temp)
          wn = WN_CreateIload(OPR_ILOAD, rtype, desc, component_offset,
                              field_id != 0 ? hi_ty_idx : desc_ty_idx,
                              Make_Pointer_Type(hi_ty_idx, FALSE),
                              WN_COPY_Tree(addr_wn), field_id);
        else
          wn = WN_Ldid(rtype, result_preg, result_preg_st, desc_ty_idx, 0);
        if (is_realpart)
          wn = WN_Unary(OPR_REALPART, Mtype_complex_to_real(rtype), wn);
        else if (is_imagpart)
          wn = WN_Unary(OPR_IMAGPART, Mtype_complex_to_real(rtype), wn);
      } else
        wn = NULL;
    }
  } break;

  case GS_ARRAY_REF: {
    TY_IDX elem_ty_idx;
    // generate the WHIRL array node
    WN *addr_wn = WGEN_Array_Expr(lhs, &elem_ty_idx, 0, 0, 0);
    if (TY_is_volatile(elem_ty_idx))
      volt = TRUE;
    TY_IDX desc_ty_idx = component_ty_idx;
    if (desc_ty_idx == 0)
      desc_ty_idx = Get_TY(gs_tree_type(lhs));
    if (TY_is_volatile(desc_ty_idx)) {
      Clear_TY_is_volatile(desc_ty_idx);
      volt = TRUE;
    }
    Is_True(!is_bit_field || field_id <= MAX_FIELD_ID,
            ("WGEN_Lhs_Of_Modify_Expr: field id for bit-field exceeds limit"));
    if (WN_has_side_effects(addr_wn) &&
        (need_result || assign_code == GS_PREINCREMENT_EXPR ||
         assign_code == GS_PREDECREMENT_EXPR ||
         assign_code == GS_POSTINCREMENT_EXPR ||
         assign_code == GS_POSTDECREMENT_EXPR)) {
      ST *preg_st;
      PREG_NUM preg;
      TY_IDX address_ty_idx = Make_Pointer_Type(elem_ty_idx, FALSE);
      preg_st = MTYPE_To_PREG(Pointer_Mtype);
      preg = Create_Preg(Pointer_Mtype, NULL);
      wn = WN_Stid(Pointer_Mtype, preg, preg_st, address_ty_idx, addr_wn);
      WGEN_Set_ST_Addr_Saved(addr_wn);
      WGEN_Stmt_Append(wn, Get_Srcpos());
      addr_wn = WN_Ldid(Pointer_Mtype, preg, preg_st, address_ty_idx);
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
      rhs_wn = WN_CreateLdid(OPR_LDID, rtype, desc, rhs_preg_num, rhs_st,
                             desc_ty_idx, 0);
    } else {
      WN *result_wn; // the result wn to be returned

      if (assign_code == GS_MODIFY_EXPR) {
        if (is_realpart)
          rhs_wn = WN_Binary(
              OPR_COMPLEX, rtype, rhs_wn,
              WN_Unary(OPR_IMAGPART, Mtype_complex_to_real(rtype),
                       WN_CreateIload(OPR_ILOAD, rtype, desc, component_offset,
                                      field_id != 0 ? elem_ty_idx : desc_ty_idx,
                                      Make_Pointer_Type(elem_ty_idx, FALSE),
                                      WN_COPY_Tree(addr_wn), field_id)));
        else if (is_imagpart)
          rhs_wn = WN_Binary(
              OPR_COMPLEX, rtype,
              WN_Unary(OPR_REALPART, Mtype_complex_to_real(rtype),
                       WN_CreateIload(OPR_ILOAD, rtype, desc, component_offset,
                                      field_id != 0 ? elem_ty_idx : desc_ty_idx,
                                      Make_Pointer_Type(elem_ty_idx, FALSE),
                                      WN_COPY_Tree(addr_wn), field_id)),
              rhs_wn);
      } else {
        if (is_realpart)
          rhs_wn = WN_Binary(OPR_COMPLEX, rtype, rhs_wn,
                             WN_Floatconst(Mtype_complex_to_real(rtype), 0.0));
        else if (is_imagpart)
          rhs_wn = WN_Binary(OPR_COMPLEX, rtype,
                             WN_Floatconst(Mtype_complex_to_real(rtype), 0.0),
                             rhs_wn);
      }

      if (assign_code == GS_PREINCREMENT_EXPR ||
          assign_code == GS_PREDECREMENT_EXPR) {
        wn = WN_CreateIload(OPR_ILOAD, rtype, desc, component_offset,
                            field_id != 0 ? elem_ty_idx : desc_ty_idx,
                            Make_Pointer_Type(elem_ty_idx, FALSE),
                            WN_COPY_Tree(addr_wn), field_id);
        rhs_wn =
            WN_Binary(Operator_From_Tree[assign_code].opr, rtype, wn, rhs_wn);
        result_wn = rhs_wn;
      } else if (assign_code == GS_POSTINCREMENT_EXPR ||
                 assign_code == GS_POSTDECREMENT_EXPR) {
        result_wn = WN_CreateIload(OPR_ILOAD, rtype, desc, component_offset,
                                   field_id != 0 ? elem_ty_idx : desc_ty_idx,
                                   Make_Pointer_Type(elem_ty_idx, FALSE),
                                   WN_COPY_Tree(addr_wn), field_id);
      } else
        result_wn = rhs_wn;

      if (need_result &&
          (volt || assign_code == GS_POSTINCREMENT_EXPR ||
           assign_code == GS_POSTDECREMENT_EXPR)) { // save result in a preg
        result_in_temp = TRUE;
        result_preg_st = MTYPE_To_PREG(rtype);
        result_preg = Create_Preg(rtype, NULL);
        wn = WN_Stid(rtype, result_preg, result_preg_st, desc_ty_idx, result_wn,
                     0);
        WGEN_Stmt_Append(wn, Get_Srcpos());
        ;
        result_wn = WN_Ldid(rtype, result_preg, result_preg_st, desc_ty_idx, 0);
      }

      if (assign_code == GS_POSTINCREMENT_EXPR ||
          assign_code == GS_POSTDECREMENT_EXPR) {
        rhs_wn = WN_Binary(Operator_From_Tree[assign_code].opr, rtype,
                           result_wn, rhs_wn);
      } else
        rhs_wn = result_wn;

      // rhs_wn is now always the right-hand-side of the assignment
    }

    // the assignment
    if (!WGEN_Keep_Zero_Length_Structs && desc == MTYPE_M &&
        TY_size(elem_ty_idx) == 0) {
      // ignore zero length structs
      if (WN_has_side_effects(addr_wn)) {
        wn = WN_CreateEval(addr_wn);
        WGEN_Stmt_Append(wn, Get_Srcpos());
      }
      wn = NULL;
    } else {
      wn = WN_CreateIstore(OPR_ISTORE, MTYPE_V, desc, component_offset,
                           Make_Pointer_Type(elem_ty_idx, FALSE), rhs_wn,
                           addr_wn, field_id);
      WGEN_Stmt_Append(wn, Get_Srcpos());
      if (need_result) {
        if (!result_in_temp)
          wn = WN_CreateIload(OPR_ILOAD, rtype, desc, component_offset,
                              field_id != 0 ? elem_ty_idx : desc_ty_idx,
                              Make_Pointer_Type(elem_ty_idx, FALSE),
                              WN_COPY_Tree(addr_wn), field_id);
        else
          wn = WN_Ldid(rtype, result_preg, result_preg_st, desc_ty_idx, 0);
        if (is_realpart)
          wn = WN_Unary(OPR_REALPART, Mtype_complex_to_real(rtype), wn);
        else if (is_imagpart)
          wn = WN_Unary(OPR_IMAGPART, Mtype_complex_to_real(rtype), wn);
      } else
        wn = NULL;
    }
  } break;
#ifdef KEY // bug 10073
  case GS_MIN_EXPR:
  case GS_MAX_EXPR: {
    gs_t arg0 = gs_tree_operand(lhs, 0);
    gs_t arg1 = gs_tree_operand(lhs, 1);

    WN *then_block = WN_CreateBlock();
    WN *else_block = WN_CreateBlock();

    WGEN_Stmt_Push(then_block, wgen_stmk_if_then, Get_Srcpos());
    WN *wn1 = WGEN_Lhs_Of_Modify_Expr(
        assign_code, arg0, NULL, TRUE, component_ty_idx, component_offset,
        field_id, is_bit_field, rhs_wn, rhs_preg_num, is_realpart, is_imagpart);
    WGEN_Stmt_Pop(wgen_stmk_if_then);

    WGEN_Stmt_Push(else_block, wgen_stmk_if_else, Get_Srcpos());
    WN *wn2 = WGEN_Lhs_Of_Modify_Expr(
        assign_code, arg1, NULL, TRUE, component_ty_idx, component_offset,
        field_id, is_bit_field, rhs_wn, rhs_preg_num, is_realpart, is_imagpart);
    WGEN_Stmt_Pop(wgen_stmk_if_else);

    Is_True(wn1 && wn2,
            ("WGEN_Lhs_Of_Modify_Expr: null operands of MIN/MAX_EXPR?"));
    WN *wn0 = WN_Relational(code == GS_MIN_EXPR ? OPR_LE : OPR_GE,
                            Widen_Mtype(TY_mtype(Get_TY(gs_tree_type(lhs)))),
                            wn1, wn2);
    WN *if_stmt = WN_CreateIf(wn0, then_block, else_block);
    WGEN_Stmt_Append(if_stmt, Get_Srcpos());
  } break;
#endif

  case GS_COMPOUND_LITERAL_EXPR: // bug 10144
  {
    gs_t var = gs_decl_initial(gs_tree_operand(gs_tree_operand(lhs, 0), 0));
    st = WGEN_Generate_Temp_For_Initialized_Aggregate(var, "");

    // bug 10281: need to make a copy so any potential store will not
    // 	overwrite the original
    ST *copy = New_ST(CURRENT_SYMTAB);
    ST_Init(copy, Save_Str(".cpfrominit"), CLASS_VAR, SCLASS_AUTO, EXPORT_LOCAL,
            ST_type(st));
    WN *init_wn = WN_CreateLdid(OPR_LDID, MTYPE_M, MTYPE_M, 0, st, ST_type(st));
    WGEN_Stmt_Append(WN_CreateStid(OPR_STID, MTYPE_V, MTYPE_M, 0, copy,
                                   ST_type(copy), init_wn),
                     Get_Srcpos());
    // the assignment
    TY_IDX desc_ty_idx = component_ty_idx;
    TYPE_ID desc = is_bit_field ? MTYPE_BS : TY_mtype(desc_ty_idx);
    if (desc == MTYPE_UNKNOWN)
      desc = WN_rtype(rhs_wn); // is a scalar
    if (desc_ty_idx == 0)
      desc_ty_idx = MTYPE_TO_TY_array[desc];

    wn = WN_Stid(desc, ST_ofst(copy) + component_offset, copy, desc_ty_idx,
                 rhs_wn, field_id);
    WGEN_Stmt_Append(wn, Get_Srcpos());
    if (need_result) // bug 10548
      wn = WN_CreateLdid(OPR_LDID, desc, desc, ST_ofst(copy) + component_offset,
                         copy, ST_type(copy));
    else
      wn = NULL;
  } break;

#ifdef KEY
  case GS_COMPOUND_EXPR: // czw
  {
    gs_t var = gs_tree_operand(lhs, 1);
    while (gs_tree_code(var) != GS_VAR_DECL)
      var = gs_tree_operand(var, 1);

    WGEN_Expand_Stmt(lhs);
    wn = WGEN_Lhs_Of_Modify_Expr(assign_code, var, lhs_retval, need_result,
                                 component_ty_idx, component_offset, field_id,
                                 is_bit_field, rhs_wn, rhs_preg_num,
                                 is_realpart, is_imagpart);
  } break;
  case GS_FILTER_EXPR:
    // TODO: Implement.
    DevWarn("NYI: FILTER_EXPR");
    wn = NULL;
    break;

  case GS_EXC_PTR_EXPR:
    // TODO: Implement.
    DevWarn("NYI: EXC_PTR_EXPR");
    wn = NULL;
    break;
#endif

  default:
    Fail_FmtAssertion(
        "WGEN_Lhs_Of_Modify_Expr: unhandled tree node in LHS of MODIFY_EXPR");
  }

  return wn;
}

/* ============================================================================
 *
 * WGEN_Expand_Expr_With_Sequence_Point
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

WN *WGEN_Expand_Expr_With_Sequence_Point(gs_t exp, TYPE_ID mtype,
                                         WN *target_wn) {
  WN *wn;

  if (mtype == MTYPE_V)
#ifdef KEY
    wn = WGEN_Expand_Expr(exp, FALSE, 0, 0, 0, 0, FALSE, FALSE, target_wn);
#else
    wn = WGEN_Expand_Expr(exp, FALSE);
#endif

  else {

    WN *comma_block = WN_CreateBlock();

    WGEN_Stmt_Push(comma_block, wgen_stmk_comma, Get_Srcpos());
#ifdef KEY
    wn = WGEN_Expand_Expr(exp, TRUE, 0, 0, 0, 0, FALSE, FALSE, target_wn);
#else
    wn = WGEN_Expand_Expr(exp);
#endif

    // normalize bool expressions
    if (gs_tree_type(exp) == gs_boolean_type_node()) {
      if (WN_operator(wn) == OPR_LDID || WN_operator(wn) == OPR_ILOAD) {
        WN *zero = WN_Intconst(WN_rtype(wn), 0);
        wn = WN_Relational(OPR_NE, MTYPE_I4, wn, zero);
      }
    }

    WGEN_Stmt_Pop(wgen_stmk_comma);
    if (WN_first(comma_block)) {
      if (wn)
        wn = WN_CreateComma(OPR_COMMA, Mtype_comparison(mtype), MTYPE_V,
                            comma_block, wn);
      else
        WGEN_Stmt_Append(comma_block, Get_Srcpos());
    } else
      WN_Delete(comma_block);
  }

  return wn;
} /* WGEN_Expand_Expr_With_Sequence_Point */

static void emit_barrier(bool type, gs_t list, INT32 k) {
  INT32 i;
  WN *wn = WN_CreateBarrier(type, k);

  for (i = 0; i < k; i++) {
    gs_t exp = gs_tree_value(list);
    ST *st = Get_ST(exp);
    WN_kid(wn, i) =
        WN_Lda(Pointer_Mtype, 0, st, Make_Pointer_Type(ST_type(st), FALSE));
    list = gs_tree_chain(list);
  }

  WGEN_Stmt_Append(wn, Get_Srcpos());
} /* emit_barrier */

static WN *emit_builtin_lock_test_and_set(gs_t exp, INT32 k) {
  WN *wn;
  WN *arg_wn;
  WN *ikids[2];
  TYPE_ID obj_mtype;
  TY_IDX arg_ty_idx;
  TYPE_ID arg_mtype;
  gs_t list = gs_tree_operand(exp, 1);
  OPCODE opc;
  INTRINSIC iopc;

  obj_mtype = TY_mtype(TY_pointed(Get_TY(gs_tree_type(gs_tree_value(list)))));
  arg_ty_idx = Get_TY(gs_tree_type(gs_tree_value(list)));
  arg_mtype = TY_mtype(arg_ty_idx);
  arg_wn = WGEN_Expand_Expr(gs_tree_value(list));
  arg_wn = WN_CreateParm(arg_mtype, arg_wn, arg_ty_idx, WN_PARM_BY_VALUE);
  ikids[0] = arg_wn;
  list = gs_tree_chain(list);
  arg_ty_idx = Get_TY(gs_tree_type(gs_tree_value(list)));
  arg_mtype = TY_mtype(arg_ty_idx);
  arg_wn = WGEN_Expand_Expr(gs_tree_value(list));
  arg_wn = WN_CreateParm(arg_mtype, arg_wn, arg_ty_idx, WN_PARM_BY_VALUE);
  ikids[1] = arg_wn;
  list = gs_tree_chain(list);

  if (obj_mtype == MTYPE_I4) {
    opc = OPC_I4INTRINSIC_CALL;
    iopc = INTRN_LOCK_TEST_AND_SET_I4;
  } else if (obj_mtype == MTYPE_U4) {
    opc = OPC_U4INTRINSIC_CALL;
    iopc = INTRN_LOCK_TEST_AND_SET_I4;
  } else if (obj_mtype == MTYPE_I8) {
    opc = OPC_I8INTRINSIC_CALL;
    iopc = INTRN_LOCK_TEST_AND_SET_I8;
  } else if (obj_mtype == MTYPE_U8) {
    opc = OPC_U8INTRINSIC_CALL;
    iopc = INTRN_LOCK_TEST_AND_SET_I8;
  } else {
    Fail_FmtAssertion("unknown object type in __builtin_lock_test_and_set");
    opc = OPCODE_UNKNOWN;
    iopc = INTRINSIC_NONE;
  }

  wn = WN_Create_Intrinsic(opc, iopc, 2, ikids);
  WGEN_Stmt_Append(wn, Get_Srcpos());

  ST *preg_st = MTYPE_To_PREG(obj_mtype);
  TY_IDX preg_ty_idx = Be_Type_Tbl(obj_mtype);
  PREG_NUM preg = Create_Preg(obj_mtype, NULL);

  wn = WN_Ldid(obj_mtype, -1, Return_Val_Preg, preg_ty_idx);
  wn = WN_Stid(obj_mtype, preg, preg_st, preg_ty_idx, wn),
  WGEN_Stmt_Append(wn, Get_Srcpos());

  emit_barrier(FALSE, list, k);

  wn = WN_Ldid(obj_mtype, preg, preg_st, preg_ty_idx);

  return wn;
} /* emit_builtin_lock_test_and_set */

static void emit_builtin_lock_release(gs_t exp, INT32 k) {
  WN *wn;
  WN *arg_wn;
  WN *ikids[1];
  TYPE_ID obj_mtype;
  TY_IDX arg_ty_idx;
  TYPE_ID arg_mtype;
  gs_t list = gs_tree_operand(exp, 1);
  OPCODE opc;
  INTRINSIC iopc;

  obj_mtype = TY_mtype(TY_pointed(Get_TY(gs_tree_type(gs_tree_value(list)))));
  arg_ty_idx = Get_TY(gs_tree_type(gs_tree_value(list)));
  arg_mtype = TY_mtype(arg_ty_idx);
  arg_wn = WGEN_Expand_Expr(gs_tree_value(list));
  arg_wn = WN_CreateParm(arg_mtype, arg_wn, arg_ty_idx, WN_PARM_BY_VALUE);
  ikids[0] = arg_wn;
  list = gs_tree_chain(list);

  emit_barrier(TRUE, list, k);

  opc = OPC_VINTRINSIC_CALL;
  if (obj_mtype == MTYPE_I4)
    iopc = INTRN_LOCK_RELEASE_I4;
  else if (obj_mtype == MTYPE_U4)
    iopc = INTRN_LOCK_RELEASE_I4;
  else if (obj_mtype == MTYPE_I8)
    iopc = INTRN_LOCK_RELEASE_I8;
  else if (obj_mtype == MTYPE_U8)
    iopc = INTRN_LOCK_RELEASE_I8;
  else {
    Fail_FmtAssertion("unknown object type in __builtin_lock_test_and_set");
    opc = OPCODE_UNKNOWN;
    iopc = INTRINSIC_NONE;
  }

  wn = WN_Create_Intrinsic(opc, iopc, 1, ikids);
  WGEN_Stmt_Append(wn, Get_Srcpos());
} /* emit_builtin_lock_release */

static WN *emit_builtin_compare_and_swap(gs_t exp, INT32 k) {
  WN *wn;
  WN *arg_wn;
  WN *ikids[3];
  TYPE_ID obj_mtype;
  TY_IDX arg_ty_idx;
  TYPE_ID arg_mtype;
  gs_t list = gs_tree_operand(exp, 1);
  OPCODE opc;
  INTRINSIC iopc;

  obj_mtype = TY_mtype(TY_pointed(Get_TY(gs_tree_type(gs_tree_value(list)))));
  arg_ty_idx = Get_TY(gs_tree_type(gs_tree_value(list)));
  arg_mtype = TY_mtype(arg_ty_idx);
  arg_wn = WGEN_Expand_Expr(gs_tree_value(list));
  arg_wn = WN_CreateParm(arg_mtype, arg_wn, arg_ty_idx, WN_PARM_BY_VALUE);
  ikids[0] = arg_wn;
  list = gs_tree_chain(list);
  arg_ty_idx = Get_TY(gs_tree_type(gs_tree_value(list)));
  arg_mtype = TY_mtype(arg_ty_idx);
  arg_wn = WGEN_Expand_Expr(gs_tree_value(list));
  arg_wn = WN_CreateParm(arg_mtype, arg_wn, arg_ty_idx, WN_PARM_BY_VALUE);
  ikids[1] = arg_wn;
  list = gs_tree_chain(list);
  arg_ty_idx = Get_TY(gs_tree_type(gs_tree_value(list)));
  arg_mtype = TY_mtype(arg_ty_idx);
  arg_wn = WGEN_Expand_Expr(gs_tree_value(list));
  arg_wn = WN_CreateParm(arg_mtype, arg_wn, arg_ty_idx, WN_PARM_BY_VALUE);
  ikids[2] = arg_wn;
  list = gs_tree_chain(list);

  emit_barrier(TRUE, list, k);

  opc = OPC_I4INTRINSIC_CALL;
  if (obj_mtype == MTYPE_I4)
    iopc = INTRN_COMPARE_AND_SWAP_I4;
  else if (obj_mtype == MTYPE_U4)
    iopc = INTRN_COMPARE_AND_SWAP_I4;
  else if (obj_mtype == MTYPE_I8)
    iopc = INTRN_COMPARE_AND_SWAP_I8;
  else if (obj_mtype == MTYPE_U8)
    iopc = INTRN_COMPARE_AND_SWAP_I8;
  else {
    Fail_FmtAssertion("unknown object type in __builtin_lock_test_and_set");
    opc = OPCODE_UNKNOWN;
    iopc = INTRINSIC_NONE;
  }

  wn = WN_Create_Intrinsic(opc, iopc, 3, ikids);
  WGEN_Stmt_Append(wn, Get_Srcpos());

  ST *preg_st = MTYPE_To_PREG(MTYPE_I4);
  TY_IDX preg_ty_idx = Be_Type_Tbl(MTYPE_I4);
  PREG_NUM preg = Create_Preg(MTYPE_I4, NULL);

  wn = WN_Ldid(MTYPE_I4, -1, Return_Val_Preg, preg_ty_idx);
  wn = WN_Stid(MTYPE_I4, preg, preg_st, preg_ty_idx, wn),
  WGEN_Stmt_Append(wn, Get_Srcpos());

  emit_barrier(FALSE, list, k);

  wn = WN_Ldid(MTYPE_I4, preg, preg_st, preg_ty_idx);

  return wn;
} /* emit_builtin_compare_and_swap */

static void emit_builtin_synchronize(gs_t exp, INT32 k) {
  WN *wn;
  gs_t list = gs_tree_operand(exp, 1);
  emit_barrier(TRUE, list, k);
  wn = WN_Create_Intrinsic(OPC_VINTRINSIC_CALL, INTRN_SYNCHRONIZE, 0, NULL);
  WGEN_Stmt_Append(wn, Get_Srcpos());
  emit_barrier(FALSE, list, k);
} /* emit_builtin_synchronize */

static char *get_string_pointer(WN *wn) {
  char *ptr = NULL;

  if (WN_operator(wn) == OPR_LDA) {
    ST *st = WN_st(wn);
    if (ST_class(st) == CLASS_CONST) {
      TCON tcon = Tcon_Table[ST_tcon(st)];
      if (TCON_ty(tcon) == MTYPE_STRING)
        ptr = ((char *)Targ_String_Address(tcon)) + WN_offset(wn);
    }
  }

  return ptr;
} /* get_string_pointer */

// Auxiliary function for WGEN_Expand_Expr, return the address of
// a tree operand.  (Used for ADDR_EXPR.)
WN *WGEN_Address_Of(gs_t arg0) {
  gs_code_t code0 = gs_tree_code(arg0);
  ST *st = 0;
  WN *wn = 0;
  WN *wn0;
  WN *wn1;
  TY_IDX ty_idx;

  switch (code0) {
#ifdef KEY
  case GS_RESULT_DECL: // bug 3878
#if 0
    // wgen clean-up: code needed to handle lowered GNU tree should not
    // be needed any more.
    if (TY_return_in_mem(Get_TY
                         (gs_tree_type
                          (gs_tree_type(Current_Function_Decl()) ) ) ) )
    {
      WGEN_fixup_result_decl (arg0);
      wn = WGEN_Address_Of (arg0);
      break;
    }
    // fall through
#endif
#endif
  case GS_VAR_DECL:
  case GS_PARM_DECL:
  case GS_FUNCTION_DECL: {
    st = Get_ST(arg0);
    ty_idx = ST_type(st);
#ifdef KEY
    // Arg0 is the virtual function table (vtable) for a class.  Initialize
    // the table.
    if (code0 == GS_VAR_DECL) {
      if (gs_decl_initial(arg0) &&
          (gs_decl_virtual_p(arg0) ||
           (/* bug 279 */ (gs_decl_tinfo_p(arg0) || lang_java) /* typeinfo ? */
            &&                                                 // czw
            /* make sure it is not an NTBS name */
            gs_tree_code(gs_decl_initial(arg0)) != GS_STRING_CST)) &&
          !gs_decl_external(arg0)) {
        gs_t init = gs_decl_initial(arg0);
        if (gs_tree_code(init) != GS_ERROR_MARK) {
          FmtAssert(gs_tree_code(init) == GS_CONSTRUCTOR,
                    ("Unexpected initializer for virtual table"));
          WGEN_Initialize_Decl(arg0);
        }
      }
    }

    if (code0 == GS_VAR_DECL && gs_decl_value_expr(arg0)) {
      wn = WGEN_Address_Of(gs_decl_value_expr(arg0));
      break;
    }
#endif
    // for VLAs, use the base_st instead of st
    if (code0 == GS_VAR_DECL && st != ST_base(st)) {
      FmtAssert(
          ST_ofst(st) == 0,
          ("Variable Length Arrays within struct not currently implemented"));
      wn = WN_Ldid(Pointer_Mtype, 0, ST_base(st), ST_type(ST_base(st)));
    } else if (!WGEN_Keep_Zero_Length_Structs && code0 == GS_PARM_DECL &&
               TY_mtype(ty_idx) == MTYPE_M && TY_size(ty_idx) == 0) {
      // taking address of zero length struct passed as parameter
      DevWarn("taking address of zero length struct %s at line %d", ST_name(st),
              lineno);
      wn = WN_Intconst(Pointer_Mtype, 0);
    } else
      wn = WN_Lda(Pointer_Mtype, ST_ofst(st), st);
  } break;

  case GS_INDIRECT_REF:
    wn = WGEN_Expand_Expr(gs_tree_operand(arg0, 0));
    break;

  case GS_STRING_CST: {
    TCON tcon;
    tcon = Host_To_Targ_String(MTYPE_STRING,
                               const_cast<char *>(gs_tree_string_pointer(arg0)),
                               gs_tree_string_length(arg0));
    ty_idx = Get_TY(gs_tree_type(arg0));
    st = New_Const_Sym(Enter_tcon(tcon), ty_idx);
    wn = WN_Lda(Pointer_Mtype, ST_ofst(st), st);
    TREE_STRING_ST(arg0) = st;
  } break;

  case GS_CONSTRUCTOR: {
    st = WGEN_Generate_Temp_For_Initialized_Aggregate(arg0, "");
    wn = WN_Lda(Pointer_Mtype, ST_ofst(st), st);
  } break;

  case GS_LABEL_DECL: {
    DevWarn("taking address of a label at line %d", lineno);
    LABEL_IDX label_idx = WGEN_Get_LABEL(arg0, FALSE);
#if 0
      FmtAssert (arg0->decl.symtab_idx == CURRENT_SYMTAB,
                 ("line %d: taking address of a label not defined in current function currently not implemented", lineno));
#endif
    wn = WN_LdaLabel(Pointer_Mtype, label_idx);
    Set_LABEL_addr_saved(label_idx);
#ifdef KEY
    // Bugs 1056 &  1227 - As a quality of implementation issue, we
    // should not prevent inlining of function explicitly marked
    // static inline just because a label therein had its address
    // taken.
    if (ST_export(Get_Current_PU_ST()) != EXPORT_LOCAL)
#endif
      Set_PU_no_inline(Get_Current_PU());
  } break;

  case GS_TARGET_EXPR: {
    WGEN_Expand_Expr(arg0);
    st = Get_ST(gs_tree_operand(arg0, 0));
    wn = WN_Lda(Pointer_Mtype, ST_ofst(st), st);
  } break;

  case GS_COMPOUND_EXPR: {
#ifdef KEY
    wn = WGEN_Expand_Expr(arg0);
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
                  ("WGEN_Address_Of: kid1 of COMMA is not LDID"));
          *st_ptr = WN_st(comma_kid1);
          break;
        default:
          FmtAssert(FALSE, ("WGEN_Address_Of: CSELECT kid NYI"));
        }
      }
      Is_True((st1 != NULL) && (st1 == st2),
              ("WGEN_Address_Of: CSELECT kids returns different STs"));
      st = st1;
    } else
      st = WN_st(wn);
#else
    st = WN_st(WGEN_Expand_Expr(arg0));
#endif
    wn = WN_Lda(Pointer_Mtype, ST_ofst(st), st);
  } break;

  case GS_NOP_EXPR: {
    wn = WGEN_Address_Of(gs_tree_operand(arg0, 0));
  } break;

  case GS_MIN_EXPR:
  case GS_MAX_EXPR: {
    // &(a <? b) or &(a >? b)
    gs_t op0 = gs_tree_operand(arg0, 0);
    gs_t op1 = gs_tree_operand(arg0, 1);
    WN *a = WGEN_Expand_Expr(op0);
    WN *b = WGEN_Expand_Expr(op1);
    FmtAssert(!WN_has_side_effects(a) && !WN_has_side_effects(b),
              ("Addr of MIN/MAX_EXPR with side effects not yet supported"));

#if 0 // GCC's same_type_p is not duplicated in wgen because it is too
      // complicated.
      FmtAssert(gs_same_type_p(gs_tree_type(op0), gs_tree_type(op1)),
                ("Types of MIN/MAX_EXPR operands differ"));
#endif
    TY_IDX ptr_ty = Make_Pointer_Type(Get_TY(gs_tree_type(op0)), FALSE);
    TYPE_ID ptr_mtype = TY_mtype(ptr_ty);
    TY_IDX arg_ty = Get_TY(gs_tree_type(gs_tree_operand(arg0, 0)));
    TYPE_ID arg_mtype = TY_mtype(arg_ty);

    WN *aptr = WGEN_Address_Of(op0);
    WN *bptr = WGEN_Address_Of(op1);
    wn = WN_Select(Widen_Mtype(ptr_mtype),
                   WN_Relational(code0 == GS_MIN_EXPR ? OPR_LT : OPR_GT,
                                 Widen_Mtype(arg_mtype), a, b),
                   aptr, bptr);
    Set_PU_has_very_high_whirl(Get_Current_PU());
  } break;

  case GS_COMPONENT_REF: {
    wn = WGEN_Expand_Expr(arg0);
    ty_idx = Get_TY(gs_tree_type(arg0));
    WN *comma = NULL;
    if (WN_operator(wn) == OPR_COMMA) { // bug 11877
      comma = wn;
      wn = WN_kid1(wn);
    }
    if (WN_operator(wn) == OPR_LDID) {
      WN_set_operator(wn, OPR_LDA);
      WN_set_rtype(wn, Pointer_Mtype);
      WN_set_desc(wn, MTYPE_V);
      WN_set_ty(wn, Make_Pointer_Type(WN_ty(wn))); // bug 10098, bug 10352
    } else if (WN_operator(wn) == OPR_ILOAD) {
      wn0 = WN_kid0(wn);
      wn1 = WN_Intconst(Pointer_Mtype, WN_offset(wn));
      wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
    } else
      Fail_FmtAssertion("WGEN_Address_Of has unhandled %s",
                        gs_code_name(code0));
    if (comma) {
      WN_set_rtype(comma, WN_rtype(wn));
      wn = comma;
    }
  } break;

#ifdef KEY // bug 3228
  case GS_ARRAY_REF:
    wn = WGEN_Expand_Expr(arg0);
    if (WN_operator(wn) == OPR_ILOAD) // bug 10105
      wn = WN_kid0(wn);
    ty_idx = Get_TY(gs_tree_type(arg0));
    break;
#endif

#ifdef KEY
  case GS_COMPOUND_LITERAL_EXPR: {
    arg0 = gs_decl_initial(gs_tree_operand(gs_tree_operand(arg0, 0), 0));
    st = WGEN_Generate_Temp_For_Initialized_Aggregate(arg0, "");

    if (CURRENT_SYMTAB == GLOBAL_SYMTAB)
      wn = WN_Lda(Pointer_Mtype, ST_ofst(st), st); // bug 10507
    else {
      // bug 10281: need to make a copy so any potential store will not
      // 	overwrite the original
      ST *copy = New_ST(CURRENT_SYMTAB);
      ST_Init(copy, Save_Str(".cpfrominit"), CLASS_VAR, SCLASS_AUTO,
              EXPORT_LOCAL, ST_type(st));
      WN *init_wn =
          WN_CreateLdid(OPR_LDID, MTYPE_M, MTYPE_M, 0, st, ST_type(st));
      WGEN_Stmt_Append(WN_CreateStid(OPR_STID, MTYPE_V, MTYPE_M, 0, copy,
                                     ST_type(copy), init_wn),
                       Get_Srcpos());

      wn = WN_Lda(Pointer_Mtype, ST_ofst(copy), copy);
    }
  } break;

  // bug 2399
  case GS_SAVE_EXPR: {
    st = WN_st(WGEN_Expand_Expr(arg0));
    wn = WN_Lda(Pointer_Mtype, ST_ofst(st), st);
  } break;

  // bug 5532, 5609
  case GS_REALPART_EXPR: {
    wn = WGEN_Expand_Expr(gs_tree_operand(arg0, 0));
    if (WN_operator(wn) == OPR_ILOAD)
      wn = WN_kid0(wn);
    else if (WN_operator(wn) == OPR_LDID)
      wn = WN_Lda(Pointer_Mtype, WN_offset(wn), WN_st(wn));
    else
      Fail_FmtAssertion("WGEN_Address_Of: NYI for REALPART_EXPR");
  } break;

  case GS_IMAGPART_EXPR: {
    wn = WGEN_Expand_Expr(gs_tree_operand(arg0, 0));
    if (WN_operator(wn) == OPR_ILOAD) {
      wn0 = WN_kid0(wn);
      TYPE_ID imag_mtype;
      switch (WN_rtype(wn)) {
      case MTYPE_C4:
        imag_mtype = MTYPE_F4;
        break;
      case MTYPE_C8:
        imag_mtype = MTYPE_F8;
        break;
#ifdef TARG_IA64
      case MTYPE_C10:
        imag_mtype = MTYPE_F10;
        break;
#else
      case MTYPE_CQ:
        imag_mtype = MTYPE_FQ;
        break;
#endif
      default:
        Fail_FmtAssertion("WGEN_Address_Of: Unexpected rtype in IMAGPART_EXPR");
      }
      INT ofst;
      if (imag_mtype == MTYPE_FQ) {
#ifdef TARG_X8664
        if (Is_Target_32bit())
          ofst = 12;
        else
#endif // TARG_X8664
          ofst = 16;
      } else
        ofst = MTYPE_byte_size(imag_mtype);

      wn1 = WN_Intconst(Pointer_Mtype, ofst);
      wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
    } else if (WN_operator(wn) == OPR_LDID)
      wn = WN_Lda(Pointer_Mtype,
                  WN_offset(wn) + MTYPE_byte_size(WN_rtype(wn)) / 2, WN_st(wn));
    else
      Fail_FmtAssertion("WGEN_Address_Of: NYI for IMAGPART_EXPR");
  } break;

  case GS_BASELINK: // bug 11167
    wn = WGEN_Address_Of(gs_baselink_functions(arg0));
    break;
#endif

  default: {
    Fail_FmtAssertion("WGEN_Address_Of: Unexpected operand %s",
                      gs_code_name(code0));
  } break;
  }

  FmtAssert(wn != 0,
            ("WGEN_Address_Of: null WHIRL tree for %s", gs_code_name(code0)));
  return wn;
}

#ifdef TARG_X8664
/* expand a VA_ARG_EXPR node for scalar type according to X86-64 ABI and
 * return the WHIRL node that represents the address to be dereferenced;
 * 'twice' is true is loading two consecutive parameters of the same type
 * because they belong to a struct; currently, twice is TRUE only if isfloat
 * is FALSE */
static WN *WGEN_x8664_va_arg(WN *ap_wn, BOOL isfloat, TY_IDX ty_idx,
                             BOOL twice) {
  /* compare gp_offset with 48 or fp_offset with 176 */
  WN *wn0 = WN_Iload(MTYPE_I4, !isfloat ? 0 : 4, MTYPE_To_TY(MTYPE_I4),
                     WN_CopyNode(ap_wn));
  WN *wn1 = WN_Intconst(MTYPE_I4, (!isfloat ? 48 : 176) - (twice ? 8 : 0));
  WN *wn = WN_Relational(OPR_GE, MTYPE_I4, wn0, wn1);
  LABEL_IDX lab1;
  New_LABEL(CURRENT_SYMTAB, lab1);
  WN *lab1_wn = WN_CreateLabel((ST_IDX)0, lab1, 0, NULL);
  wn = WN_CreateTruebr(lab1, wn);
  WGEN_Stmt_Append(wn, Get_Srcpos());

  ST *arg_temp_st = Gen_Temp_Symbol(Make_Pointer_Type(ty_idx), ".va_arg");
#if 0 // wgen TODO
  WGEN_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, arg_temp_st);
#endif
  /* compute reg_save_area+gp_offset/fp_offset and store to arg_temp_st */
  wn0 = WN_Iload(MTYPE_I4, !isfloat ? 0 : 4, MTYPE_To_TY(MTYPE_I4),
                 WN_CopyNode(ap_wn));
  wn1 = WN_Iload(Pointer_Mtype, 16, MTYPE_To_TY(Pointer_Mtype),
                 WN_CopyNode(ap_wn));
  wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
  wn = WN_Stid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx), wn);
  WGEN_Stmt_Append(wn, Get_Srcpos());

  /* increment gp_offset by 8 or fp_offset by 16 */
  wn0 = WN_Iload(MTYPE_I4, !isfloat ? 0 : 4, MTYPE_To_TY(MTYPE_I4),
                 WN_CopyNode(ap_wn));
  wn1 = WN_Intconst(MTYPE_I4, (!isfloat ? 8 : 16) * ((INT)twice + 1));
  wn = WN_Binary(OPR_ADD, MTYPE_I4, wn0, wn1);
  wn = WN_Istore(MTYPE_I4, !isfloat ? 0 : 4,
                 Make_Pointer_Type(MTYPE_To_TY(MTYPE_I4)), WN_CopyNode(ap_wn),
                 wn);
  WGEN_Stmt_Append(wn, Get_Srcpos());

  /* branch around next part */
  LABEL_IDX lab2;
  New_LABEL(CURRENT_SYMTAB, lab2);
  WN *lab2_wn = WN_CreateLabel((ST_IDX)0, lab2, 0, NULL);
  wn = WN_CreateGoto(lab2);
  WGEN_Stmt_Append(wn, Get_Srcpos());

  WGEN_Stmt_Append(lab1_wn, Get_Srcpos());

  /* store overflow_arg_area pointer to arg_temp_st */
  wn0 =
      WN_Iload(Pointer_Mtype, 8, Make_Pointer_Type(ty_idx), WN_CopyNode(ap_wn));
  wn = WN_Stid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx), wn0);
  WGEN_Stmt_Append(wn, Get_Srcpos());

  /* increment overflow_arg_area pointer by 8 */
  wn0 = WN_Iload(Pointer_Mtype, 8, MTYPE_To_TY(Pointer_Mtype),
                 WN_CopyNode(ap_wn));
  wn1 = WN_Intconst(MTYPE_U8, twice ? 16 : 8);
  wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
  wn =
      WN_Istore(Pointer_Mtype, 8, Make_Pointer_Type(MTYPE_To_TY(Pointer_Mtype)),
                WN_CopyNode(ap_wn), wn);
  WGEN_Stmt_Append(wn, Get_Srcpos());

  WGEN_Stmt_Append(lab2_wn, Get_Srcpos());

  return WN_Ldid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx));
}

/* expand a VA_ARG_EXPR node for struct type being passed in 2 different classes
 * of registers, according to X86-64 ABI and return the WHIRL node that
 * represents the address to be dereferenced; this requires allocating a
 * temporary for assembling the struct if passed in registers; isfloat0 is
 * for the first 8-byte and isfloat1 is for the second 8-byte  */
static WN *WGEN_x8664_va_arg_2_mixed(WN *ap_wn, BOOL isfloat0, BOOL isfloat1,
                                     TY_IDX ty_idx) {
  /* compare gp_offset with 48 */
  WN *wn0 = WN_Iload(MTYPE_I4, 0, MTYPE_To_TY(MTYPE_I4), WN_CopyNode(ap_wn));
  WN *wn1 = WN_Intconst(MTYPE_I4, 48);
  WN *wn = WN_Relational(OPR_GE, MTYPE_I4, wn0, wn1);
  LABEL_IDX lab1;
  New_LABEL(CURRENT_SYMTAB, lab1);
  WN *lab1_wn = WN_CreateLabel((ST_IDX)0, lab1, 0, NULL);
  wn = WN_CreateTruebr(lab1, wn);
  WGEN_Stmt_Append(wn, Get_Srcpos());
  /* compare fp_offset with 176 */
  wn0 = WN_Iload(MTYPE_I4, 4, MTYPE_To_TY(MTYPE_I4), WN_CopyNode(ap_wn));
  wn1 = WN_Intconst(MTYPE_I4, 176);
  wn = WN_Relational(OPR_GE, MTYPE_I4, wn0, wn1);
  wn = WN_CreateTruebr(lab1, wn);
  WGEN_Stmt_Append(wn, Get_Srcpos());

  /* allocate a temporary location to assemble the structure value */
  ST *struct_temp_st = Gen_Temp_Symbol(ty_idx, ".va_arg_struct");
#if 0 // wgen TODO
  WGEN_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, struct_temp_st);
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
  WGEN_Stmt_Append(wn, Get_Srcpos());
  /* compute reg_save_area+fp_offset and store dereferenced value to
   * struct_temp_st */
  wn0 = WN_Iload(MTYPE_I4, 4, MTYPE_To_TY(MTYPE_I4), WN_CopyNode(ap_wn));
  wn1 = WN_Iload(Pointer_Mtype, 16, MTYPE_To_TY(Pointer_Mtype),
                 WN_CopyNode(ap_wn));
  wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
  wn = WN_Iload(MTYPE_F8, 0, MTYPE_To_TY(MTYPE_F8), wn);
  wn = WN_Stid(MTYPE_F8, isfloat0 ? 0 : 8, struct_temp_st,
               MTYPE_To_TY(MTYPE_F8), wn);
  WGEN_Stmt_Append(wn, Get_Srcpos());

  /* increment gp_offset by 8 */
  wn0 = WN_Iload(MTYPE_I4, 0, MTYPE_To_TY(MTYPE_I4), WN_CopyNode(ap_wn));
  wn1 = WN_Intconst(MTYPE_I4, 8);
  wn = WN_Binary(OPR_ADD, MTYPE_I4, wn0, wn1);
  wn = WN_Istore(MTYPE_I4, 0, Make_Pointer_Type(MTYPE_To_TY(MTYPE_I4)),
                 WN_CopyNode(ap_wn), wn);
  WGEN_Stmt_Append(wn, Get_Srcpos());
  /* increment fp_offset by 16 */
  wn0 = WN_Iload(MTYPE_I4, 4, MTYPE_To_TY(MTYPE_I4), WN_CopyNode(ap_wn));
  wn1 = WN_Intconst(MTYPE_I4, 16);
  wn = WN_Binary(OPR_ADD, MTYPE_I4, wn0, wn1);
  wn = WN_Istore(MTYPE_I4, 4, Make_Pointer_Type(MTYPE_To_TY(MTYPE_I4)),
                 WN_CopyNode(ap_wn), wn);
  WGEN_Stmt_Append(wn, Get_Srcpos());

  /* put the address of struct_temp_st in arg_temp_st */
  ST *arg_temp_st = Gen_Temp_Symbol(Make_Pointer_Type(ty_idx), ".va_arg");
#if 0 // wgen TODO
  WGEN_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, arg_temp_st);
#endif
  wn = WN_Lda(Pointer_Mtype, 0, struct_temp_st, 0);
  Set_ST_addr_saved(struct_temp_st);
  wn = WN_Stid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx), wn);
  WGEN_Stmt_Append(wn, Get_Srcpos());

  /* branch around next part */
  LABEL_IDX lab2;
  New_LABEL(CURRENT_SYMTAB, lab2);
  WN *lab2_wn = WN_CreateLabel((ST_IDX)0, lab2, 0, NULL);
  wn = WN_CreateGoto(lab2);
  WGEN_Stmt_Append(wn, Get_Srcpos());

  WGEN_Stmt_Append(lab1_wn, Get_Srcpos());

  /* store overflow_arg_area pointer to arg_temp_st */
  wn0 =
      WN_Iload(Pointer_Mtype, 8, Make_Pointer_Type(ty_idx), WN_CopyNode(ap_wn));
  wn = WN_Stid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx), wn0);
  WGEN_Stmt_Append(wn, Get_Srcpos());

  /* increment overflow_arg_area pointer by 16 */
  wn0 = WN_Iload(Pointer_Mtype, 8, MTYPE_To_TY(Pointer_Mtype),
                 WN_CopyNode(ap_wn));
  wn1 = WN_Intconst(MTYPE_U8, 16);
  wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
  wn =
      WN_Istore(Pointer_Mtype, 8, Make_Pointer_Type(MTYPE_To_TY(Pointer_Mtype)),
                WN_CopyNode(ap_wn), wn);
  WGEN_Stmt_Append(wn, Get_Srcpos());

  WGEN_Stmt_Append(lab2_wn, Get_Srcpos());

  return WN_Ldid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx));
}

/* expand a VA_ARG_EXPR node for struct type being passed in 2 float
 * registers, according to X86-64 ABI and return the WHIRL node that
 * represents the address to be dereferenced; this requires allocating a
 * temporary for assembling the struct if passed in registers, because each
 * float register is saved into 128 bit locations */
static WN *WGEN_x8664_va_arg_2_float(WN *ap_wn, TY_IDX ty_idx) {
  LABEL_IDX lab1;
  New_LABEL(CURRENT_SYMTAB, lab1);
  WN *lab1_wn = WN_CreateLabel((ST_IDX)0, lab1, 0, NULL);
  /* compare fp_offset with 160 (176 - 16) */
  WN *wn0 = WN_Iload(MTYPE_I4, 4, MTYPE_To_TY(MTYPE_I4), WN_CopyNode(ap_wn));
  WN *wn1 = WN_Intconst(MTYPE_I4, 160);
  WN *wn = WN_Relational(OPR_GE, MTYPE_I4, wn0, wn1);
  wn = WN_CreateTruebr(lab1, wn);
  WGEN_Stmt_Append(wn, Get_Srcpos());

  /* allocate a temporary location to assemble the structure value */
  ST *struct_temp_st = Gen_Temp_Symbol(ty_idx, ".va_arg_struct");
#if 0 // wgen TODO
  WGEN_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, struct_temp_st);
#endif

  /* compute reg_save_area+fp_offset and store 1st dereferenced value to
   * struct_temp_st */
  wn0 = WN_Iload(MTYPE_I4, 4, MTYPE_To_TY(MTYPE_I4), WN_CopyNode(ap_wn));
  wn1 = WN_Iload(Pointer_Mtype, 16, MTYPE_To_TY(Pointer_Mtype),
                 WN_CopyNode(ap_wn));
  wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
  wn = WN_Iload(MTYPE_F8, 0, MTYPE_To_TY(MTYPE_F8), wn);
  wn = WN_Stid(MTYPE_F8, 0, struct_temp_st, MTYPE_To_TY(MTYPE_F8), wn);
  WGEN_Stmt_Append(wn, Get_Srcpos());
  /* compute reg_save_area+fp_offset and store 2nd dereferenced value to
   * struct_temp_st */
  wn0 = WN_Iload(MTYPE_I4, 4, MTYPE_To_TY(MTYPE_I4), WN_CopyNode(ap_wn));
  wn1 = WN_Iload(Pointer_Mtype, 16, MTYPE_To_TY(Pointer_Mtype),
                 WN_CopyNode(ap_wn));
  wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
  wn = WN_Iload(MTYPE_F8, 16, MTYPE_To_TY(MTYPE_F8), wn);
  wn = WN_Stid(MTYPE_F8, 8, struct_temp_st, MTYPE_To_TY(MTYPE_F8), wn);
  WGEN_Stmt_Append(wn, Get_Srcpos());

  /* increment fp_offset by 32 */
  wn0 = WN_Iload(MTYPE_I4, 4, MTYPE_To_TY(MTYPE_I4), WN_CopyNode(ap_wn));
  wn1 = WN_Intconst(MTYPE_I4, 32);
  wn = WN_Binary(OPR_ADD, MTYPE_I4, wn0, wn1);
  wn = WN_Istore(MTYPE_I4, 4, Make_Pointer_Type(MTYPE_To_TY(MTYPE_I4)),
                 WN_CopyNode(ap_wn), wn);
  WGEN_Stmt_Append(wn, Get_Srcpos());

  /* put the address of struct_temp_st in arg_temp_st */
  ST *arg_temp_st = Gen_Temp_Symbol(Make_Pointer_Type(ty_idx), ".va_arg");
#if 0 // wgen TODO
  WGEN_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, arg_temp_st);
#endif
  wn = WN_Lda(Pointer_Mtype, 0, struct_temp_st, 0);
  Set_ST_addr_saved(struct_temp_st);
  wn = WN_Stid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx), wn);
  WGEN_Stmt_Append(wn, Get_Srcpos());

  /* branch around next part */
  LABEL_IDX lab2;
  New_LABEL(CURRENT_SYMTAB, lab2);
  WN *lab2_wn = WN_CreateLabel((ST_IDX)0, lab2, 0, NULL);
  wn = WN_CreateGoto(lab2);
  WGEN_Stmt_Append(wn, Get_Srcpos());

  WGEN_Stmt_Append(lab1_wn, Get_Srcpos());

  /* store overflow_arg_area pointer to arg_temp_st */
  wn0 =
      WN_Iload(Pointer_Mtype, 8, Make_Pointer_Type(ty_idx), WN_CopyNode(ap_wn));
  wn = WN_Stid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx), wn0);
  WGEN_Stmt_Append(wn, Get_Srcpos());

  /* increment overflow_arg_area pointer by 16 */
  wn0 = WN_Iload(Pointer_Mtype, 8, MTYPE_To_TY(Pointer_Mtype),
                 WN_CopyNode(ap_wn));
  wn1 = WN_Intconst(MTYPE_U8, 16);
  wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
  wn =
      WN_Istore(Pointer_Mtype, 8, Make_Pointer_Type(MTYPE_To_TY(Pointer_Mtype)),
                WN_CopyNode(ap_wn), wn);
  WGEN_Stmt_Append(wn, Get_Srcpos());

  WGEN_Stmt_Append(lab2_wn, Get_Srcpos());

  return WN_Ldid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx));
}
#endif

static bool inside_eh_region = false;
// Setup an EH region, typically across a function call.
void Setup_EH_Region(bool for_unwinding) {
  WN *region_body;

  if (for_unwinding)
    region_body = WGEN_Stmt_Pop(wgen_stmk_region_body);
  else {
    region_body = WGEN_Stmt_Pop(wgen_stmk_call_region_body);
    inside_eh_region = false;
  }
  INITV_IDX iv;
  LABEL_IDX pad = 0;

  if (!for_unwinding)
    pad = lookup_cleanups(iv);
  else {
    iv = New_INITV();
    INITV_Set_ZERO(Initv_Table[iv], MTYPE_U4, 1);
  }

  INITV_IDX initv_label = New_INITV();
  if (pad)
    INITV_Init_Label(initv_label, pad, 1);
  else
    INITV_Set_ZERO(Initv_Table[initv_label], MTYPE_U4, 1);
  INITV_IDX blk = New_INITV();
  INITV_Init_Block(blk, initv_label);

  Set_INITV_next(initv_label, iv);

  TY_IDX ty = MTYPE_TO_TY_array[MTYPE_U4];
  ST *ereg =
      Gen_Temp_Named_Symbol(ty, "dummy1", CLASS_VAR, SCLASS_EH_REGION_SUPP);
  Set_ST_is_initialized(*ereg);
  Set_ST_is_not_used(*ereg);
  INITO_IDX ereg_supp = New_INITO(ST_st_idx(ereg), blk);

  WGEN_Stmt_Append(WN_CreateRegion(REGION_KIND_EH, region_body,
                                   WN_CreateBlock(), WN_CreateBlock(),
                                   New_Region_Id(), ereg_supp),
                   Get_Srcpos());
  Set_PU_has_region(Get_Current_PU());
  Set_PU_has_exc_scopes(Get_Current_PU());

#ifdef TARG_IA64
  // The following code creat a new TY for the ST that is created
  // above. Because in CG, we will get the size of the ST from its
  // TY, we should get its right size from the INITO attached with
  // it, and write it into a new TY
  TY_IDX tyi;
  TY &zty = New_TY(tyi);
  UINT inito_size = Get_INITO_Size(ereg_supp);
  TY_Init(zty, inito_size, KIND_STRUCT, MTYPE_M, ereg->u1.name_idx);
  Set_TY_align(tyi, 4);
  ST_Init(ereg, TY_name_idx(zty), CLASS_VAR, SCLASS_EH_REGION_SUPP,
          EXPORT_LOCAL, tyi);
  Set_ST_is_initialized(ereg);
#endif
}

static TY_IDX get_field_type(TY_IDX struct_type, UINT field_id) {
  Is_True(TY_kind(struct_type) == KIND_STRUCT, ("expecting KIND_STRUCT"));
  UINT cur_field_id = 0;
  FLD_HANDLE fld = FLD_get_to_field(struct_type, field_id, cur_field_id);
  Is_True(!fld.Is_Null(),
          ("Invalid field id %d for type 0x%x", field_id, struct_type));
  return FLD_type(fld);
}

#ifdef TARG_X8664
// Handle GNU x86 builtins
static WN *WGEN_target_builtins(gs_t exp, INTRINSIC *iopc, BOOL *intrinsic_op) {
  WN *wn = NULL;

  // Assumption: we would be generating intrinsics for most of the builtins
  *intrinsic_op = TRUE;

  gs_t func = gs_tree_operand(gs_tree_operand(exp, 0), 0);
  Is_True(gs_tree_code(func) == GS_FUNCTION_DECL && gs_decl_built_in(func) &&
              gs_decl_built_in_class(func) == GSBI_CLASS_BUILT_IN_MD,
          ("Invalid tree node"));

  unsigned int ins_code = gs_decl_function_code(func);
  TYPE_ID res_type = TY_mtype(Get_TY(gs_tree_type(exp)));
  gs_t t_list = gs_tree_operand(exp, 1);
  WN *arg0 = NULL, *arg1 = NULL;
  if (t_list) {
    // Assumption: every builtin has 2 kids: this will change
    arg0 = WGEN_Expand_Expr(gs_tree_value(t_list));
    if (gs_tree_chain(t_list))
      arg1 = WGEN_Expand_Expr(gs_tree_value(gs_tree_chain(t_list)));
  }

  switch (ins_code) {
  // Generate WN
  case GSBI_IX86_BUILTIN_PADDB:
  case GSBI_IX86_BUILTIN_PADDW:
  case GSBI_IX86_BUILTIN_PADDD:
  case GSBI_IX86_BUILTIN_ADDPD:
    wn = WN_Add(res_type, arg0, arg1);
    *intrinsic_op = FALSE;
    break;
  case GSBI_IX86_BUILTIN_PSUBB:
  case GSBI_IX86_BUILTIN_PSUBW:
  case GSBI_IX86_BUILTIN_PSUBD:
  case GSBI_IX86_BUILTIN_SUBPD:
    wn = WN_Sub(res_type, arg0, arg1);
    *intrinsic_op = FALSE;
    break;
  case GSBI_IX86_BUILTIN_PAND:
    if (MTYPE_is_mmx_vector(res_type))
      goto unsupported;
    wn = WN_Band(res_type, arg0, arg1);
    *intrinsic_op = FALSE;
    break;
  case GSBI_IX86_BUILTIN_PANDN:
    if (MTYPE_is_mmx_vector(res_type))
      goto unsupported;
    wn = WN_Band(res_type, WN_Bnot(res_type, arg0), arg1);
    *intrinsic_op = FALSE;
    break;
  case GSBI_IX86_BUILTIN_POR:
    if (MTYPE_is_mmx_vector(res_type))
      goto unsupported;
    wn = WN_Bior(res_type, arg0, arg1);
    *intrinsic_op = FALSE;
    break;
  case GSBI_IX86_BUILTIN_PXOR:
    if (MTYPE_is_mmx_vector(res_type))
      goto unsupported;
    wn = WN_Bxor(res_type, arg0, arg1);
    *intrinsic_op = FALSE;
    break;

  // Generate intrinsics to be expanded in CG expand
  case GSBI_IX86_BUILTIN_PADDSB:
    *iopc = INTRN_PADDSB;
    break;
  case GSBI_IX86_BUILTIN_PADDSW:
    *iopc = INTRN_PADDSW;
    break;
  case GSBI_IX86_BUILTIN_PSUBSB:
    *iopc = INTRN_PSUBSB;
    break;
  case GSBI_IX86_BUILTIN_PSUBSW:
    *iopc = INTRN_PSUBSW;
    break;
  case GSBI_IX86_BUILTIN_PADDUSB:
    *iopc = INTRN_PADDUSB;
    break;
  case GSBI_IX86_BUILTIN_PADDUSW:
    *iopc = INTRN_PADDUSW;
    break;
  case GSBI_IX86_BUILTIN_PSUBUSB:
    *iopc = INTRN_PSUBUSB;
    break;
  case GSBI_IX86_BUILTIN_PSUBUSW:
    *iopc = INTRN_PSUBUSW;
    break;
  case GSBI_IX86_BUILTIN_PMULLW:
    *iopc = INTRN_PMULLW;
    break;
  case GSBI_IX86_BUILTIN_PMULHW:
    *iopc = INTRN_PMULHW;
    break;
  case GSBI_IX86_BUILTIN_PCMPEQB:
    *iopc = INTRN_PCMPEQB;
    break;
  case GSBI_IX86_BUILTIN_PCMPEQW:
    *iopc = INTRN_PCMPEQW;
    break;
  case GSBI_IX86_BUILTIN_PCMPEQD:
    *iopc = INTRN_PCMPEQD;
    break;
  case GSBI_IX86_BUILTIN_PCMPGTB:
    *iopc = INTRN_PCMPGTB;
    break;
  case GSBI_IX86_BUILTIN_PCMPGTW:
    *iopc = INTRN_PCMPGTW;
    break;
  case GSBI_IX86_BUILTIN_PCMPGTD:
    *iopc = INTRN_PCMPGTD;
    break;
  case GSBI_IX86_BUILTIN_PUNPCKHBW:
    *iopc = INTRN_PUNPCKHBW;
    break;
  case GSBI_IX86_BUILTIN_PUNPCKHWD:
    *iopc = INTRN_PUNPCKHWD;
    break;
  case GSBI_IX86_BUILTIN_PUNPCKHDQ:
    *iopc = INTRN_PUNPCKHDQ;
    break;
  case GSBI_IX86_BUILTIN_PUNPCKLBW:
    *iopc = INTRN_PUNPCKLBW;
    break;
  case GSBI_IX86_BUILTIN_PUNPCKLWD:
    *iopc = INTRN_PUNPCKLWD;
    break;
  case GSBI_IX86_BUILTIN_PUNPCKLDQ:
    *iopc = INTRN_PUNPCKLDQ;
    break;
  case GSBI_IX86_BUILTIN_PACKSSWB:
    *iopc = INTRN_PACKSSWB;
    break;
  case GSBI_IX86_BUILTIN_PACKSSDW:
    *iopc = INTRN_PACKSSDW;
    break;
  case GSBI_IX86_BUILTIN_PACKUSWB:
    *iopc = INTRN_PACKUSWB;
    break;
  case GSBI_IX86_BUILTIN_PMULHUW:
    *iopc = INTRN_PMULHUW;
    break;
  case GSBI_IX86_BUILTIN_PAVGB:
    *iopc = INTRN_PAVGB;
    break;
  case GSBI_IX86_BUILTIN_PAVGW:
    *iopc = INTRN_PAVGW;
    break;
  case GSBI_IX86_BUILTIN_PSADBW:
    *iopc = INTRN_PSADBW;
    break;
  case GSBI_IX86_BUILTIN_PMAXUB:
    *iopc = INTRN_PMAXUB;
    break;
  case GSBI_IX86_BUILTIN_PMAXSW:
    *iopc = INTRN_PMAXSW;
    break;
  case GSBI_IX86_BUILTIN_PMINUB:
    *iopc = INTRN_PMINUB;
    break;
  case GSBI_IX86_BUILTIN_PMINSW:
    *iopc = INTRN_PMINSW;
    break;
#if 0 // wgen TODO
    case GSBI_IX86_BUILTIN_PEXTRW:
      {
        Is_True (gs_tree_code (gs_tree_value (gs_tree_chain (t_list))) == GS_INTEGER_CST,
                 ("Immediate value required by pextrw"));
        UINT val = gs_get_integer_value(gs_tree_value (gs_tree_chain (t_list)));
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
        TY_IDX arg_ty_idx = Get_TY(gs_tree_type(gs_tree_value(t_list)));
        TYPE_ID arg_mtype  = TY_mtype(arg_ty_idx);
        arg0     = WN_CreateParm (Mtype_comparison (arg_mtype), arg0,
                                  arg_ty_idx, WN_PARM_BY_VALUE);
        wn = WN_Create_Intrinsic (OPR_INTRINSIC_OP, MTYPE_U4, MTYPE_V,
                                      *iopc, 1, &arg0);
        break;
      }
    case GSBI_IX86_BUILTIN_PINSRW:
      {
        Is_True (gs_tree_code (gs_tree_value (gs_tree_chain (gs_tree_chain (t_list)))) == GS_INTEGER_CST, ("Immediate value required by pinsrw"));
        UINT val = gs_get_integer_value(gs_tree_value (gs_tree_chain (gs_tree_chain (t_list))));
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
            TY_IDX arg_ty_idx = Get_TY (gs_tree_type (gs_tree_value (t_list)));
            TYPE_ID arg_mtype = TY_mtype (arg_ty_idx);
            args[c] = WN_CreateParm (Mtype_comparison (arg_mtype), arg0,
                                        arg_ty_idx, WN_PARM_BY_VALUE);
            t_list = gs_tree_chain (t_list);
            arg0 = arg1;
        }
                                                                                
        wn = WN_Create_Intrinsic (OPR_INTRINSIC_OP, MTYPE_M8I2, MTYPE_V,
                                  *iopc, 2, args);
        break;
      }
#endif
  case GSBI_IX86_BUILTIN_PMOVMSKB:
    *iopc = INTRN_PMOVMSKB;
    break;
  case GSBI_IX86_BUILTIN_ADDPS:
    *iopc = INTRN_ADDPS;
    break;
  case GSBI_IX86_BUILTIN_SUBPS:
    *iopc = INTRN_SUBPS;
    break;
  case GSBI_IX86_BUILTIN_MULPS:
    *iopc = INTRN_MULPS;
    break;
  case GSBI_IX86_BUILTIN_DIVPS:
    *iopc = INTRN_DIVPS;
    break;
  case GSBI_IX86_BUILTIN_ADDSS:
    *iopc = INTRN_ADDSS;
    break;
  case GSBI_IX86_BUILTIN_SUBSS:
    *iopc = INTRN_SUBSS;
    break;
  case GSBI_IX86_BUILTIN_MULSS:
    *iopc = INTRN_MULSS;
    break;
  case GSBI_IX86_BUILTIN_DIVSS:
    *iopc = INTRN_DIVSS;
    break;
  case GSBI_IX86_BUILTIN_CMPEQPS:
    *iopc = INTRN_CMPEQPS;
    break;
  case GSBI_IX86_BUILTIN_CMPLTPS:
    *iopc = INTRN_CMPLTPS;
    break;
  case GSBI_IX86_BUILTIN_CMPLEPS:
    *iopc = INTRN_CMPLEPS;
    break;
  case GSBI_IX86_BUILTIN_CMPGTPS:
    *iopc = INTRN_CMPGTPS;
    break;
  case GSBI_IX86_BUILTIN_CMPGEPS:
    *iopc = INTRN_CMPGEPS;
    break;
  case GSBI_IX86_BUILTIN_CMPUNORDPS:
    *iopc = INTRN_CMPUNORDPS;
    break;
  case GSBI_IX86_BUILTIN_CMPNEQPS:
    *iopc = INTRN_CMPNEQPS;
    break;
  case GSBI_IX86_BUILTIN_CMPNLTPS:
    *iopc = INTRN_CMPNLTPS;
    break;
  case GSBI_IX86_BUILTIN_CMPNLEPS:
    *iopc = INTRN_CMPNLEPS;
    break;
  case GSBI_IX86_BUILTIN_CMPNGTPS:
    *iopc = INTRN_CMPNGTPS;
    break;
  case GSBI_IX86_BUILTIN_CMPNGEPS:
    *iopc = INTRN_CMPNGEPS;
    break;
  case GSBI_IX86_BUILTIN_CMPORDPS:
    *iopc = INTRN_CMPORDPS;
    break;
  case GSBI_IX86_BUILTIN_CMPEQSS:
    *iopc = INTRN_CMPEQSS;
    break;
  case GSBI_IX86_BUILTIN_CMPLTSS:
    *iopc = INTRN_CMPLTSS;
    break;
  case GSBI_IX86_BUILTIN_CMPLESS:
    *iopc = INTRN_CMPLESS;
    break;
  case GSBI_IX86_BUILTIN_CMPUNORDSS:
    *iopc = INTRN_CMPUNORDSS;
    break;
  case GSBI_IX86_BUILTIN_CMPNEQSS:
    *iopc = INTRN_CMPNEQSS;
    break;
  case GSBI_IX86_BUILTIN_CMPNLTSS:
    *iopc = INTRN_CMPNLTSS;
    break;
  case GSBI_IX86_BUILTIN_CMPNLESS:
    *iopc = INTRN_CMPNLESS;
    break;
  case GSBI_IX86_BUILTIN_CMPORDSS:
    *iopc = INTRN_CMPORDSS;
    break;
  case GSBI_IX86_BUILTIN_MAXPS:
    *iopc = INTRN_MAXPS;
    break;
  case GSBI_IX86_BUILTIN_MAXSS:
    *iopc = INTRN_MAXSS;
    break;
  case GSBI_IX86_BUILTIN_MINPS:
    *iopc = INTRN_MINPS;
    break;
  case GSBI_IX86_BUILTIN_MINSS:
    *iopc = INTRN_MINSS;
    break;
  case GSBI_IX86_BUILTIN_ANDPS:
    *iopc = INTRN_ANDPS;
    break;
  case GSBI_IX86_BUILTIN_ANDNPS:
    *iopc = INTRN_ANDNPS;
    break;
  case GSBI_IX86_BUILTIN_ORPS:
    *iopc = INTRN_ORPS;
    break;
  case GSBI_IX86_BUILTIN_XORPS:
    *iopc = INTRN_XORPS;
    break;
  case GSBI_IX86_BUILTIN_MOVSS:
    *iopc = INTRN_MOVSS;
    break;
  case GSBI_IX86_BUILTIN_MOVHLPS:
    *iopc = INTRN_MOVHLPS;
    break;
  case GSBI_IX86_BUILTIN_MOVLHPS:
    *iopc = INTRN_MOVLHPS;
    break;
  case GSBI_IX86_BUILTIN_UNPCKHPS:
    *iopc = INTRN_UNPCKHPS;
    break;
  case GSBI_IX86_BUILTIN_UNPCKLPS:
    *iopc = INTRN_UNPCKLPS;
    break;
  case GSBI_IX86_BUILTIN_RCPPS:
    *iopc = INTRN_RCPPS;
    break;
  case GSBI_IX86_BUILTIN_RSQRTPS:
    *iopc = INTRN_RSQRTPS;
    break;
  case GSBI_IX86_BUILTIN_SQRTPS:
    *iopc = INTRN_SQRTPS;
    break;
  case GSBI_IX86_BUILTIN_RCPSS:
    *iopc = INTRN_RCPSS;
    break;
  case GSBI_IX86_BUILTIN_RSQRTSS:
    *iopc = INTRN_RSQRTSS;
    break;
  case GSBI_IX86_BUILTIN_SQRTSS:
    *iopc = INTRN_SQRTSS;
    break;
  case GSBI_IX86_BUILTIN_SHUFPS:
    *iopc = INTRN_SHUFPS;
    break;
  case GSBI_IX86_BUILTIN_EMMS:
    *iopc = INTRN_EMMS;
    *intrinsic_op = FALSE;
    break;
#if 0 // wgen TODO
    case GSBI_IX86_BUILTIN_LOADAPS:
      *iopc = INTRN_LOADAPS;
      break;
    case GSBI_IX86_BUILTIN_STOREAPS:
      *iopc = INTRN_STOREAPS;
      *intrinsic_op = FALSE;
      break;
#endif
  case GSBI_IX86_BUILTIN_PXOR128:
    wn = WN_Bxor(res_type, arg0, arg1);
    *intrinsic_op = FALSE;
    break;
  case GSBI_IX86_BUILTIN_PSLLDQI128:
    *iopc = INTRN_PSLLDQ;
    break;
  case GSBI_IX86_BUILTIN_PSRLDQI128:
    *iopc = INTRN_PSRLDQ;
    break;
  case GSBI_IX86_BUILTIN_PSLLW128:
    *iopc = INTRN_PSLLW;
    break;
  case GSBI_IX86_BUILTIN_PSLLD128:
    *iopc = INTRN_PSLLD;
    break;
  case GSBI_IX86_BUILTIN_PSLLQ128:
    *iopc = INTRN_PSLLQ;
    break;
  case GSBI_IX86_BUILTIN_PSRLW128:
    *iopc = INTRN_PSRLW;
    break;
  case GSBI_IX86_BUILTIN_PSRLD128:
    *iopc = INTRN_PSRLD;
    break;
  case GSBI_IX86_BUILTIN_PSRLQ128:
    *iopc = INTRN_PSRLQ;
    break;
  case GSBI_IX86_BUILTIN_PSRAW128:
    *iopc = INTRN_PSRAW;
    break;
  case GSBI_IX86_BUILTIN_PSRAD128:
    *iopc = INTRN_PSRAD;
    break;
  case GSBI_IX86_BUILTIN_PSRAWI128:
    *iopc = INTRN_PSRAW;
    break;
  case GSBI_IX86_BUILTIN_PSRADI128:
    *iopc = INTRN_PSRAD;
    break;
  case GSBI_IX86_BUILTIN_PSLLWI128:
    *iopc = INTRN_PSLLW;
    break;
  case GSBI_IX86_BUILTIN_PSLLDI128:
    *iopc = INTRN_PSLLD;
    break;
  case GSBI_IX86_BUILTIN_PSLLQI128:
    *iopc = INTRN_PSLLQ;
    break;
  case GSBI_IX86_BUILTIN_PSRLWI128:
    *iopc = INTRN_PSRLW;
    break;
  case GSBI_IX86_BUILTIN_PSRLDI128:
    *iopc = INTRN_PSRLD;
    break;
  case GSBI_IX86_BUILTIN_PSRLQI128:
    *iopc = INTRN_PSRLQ;
    break;
  case GSBI_IX86_BUILTIN_MOVNTDQ:
    *iopc = INTRN_MOVNTDQ;
    *intrinsic_op = FALSE;
    break;
#if 0 // wgen TODO
    case GSBI_IX86_BUILTIN_LOADD:
      *iopc = INTRN_LOADD;
      break;
#endif
  case GSBI_IX86_BUILTIN_MOVNTPS:
    *iopc = INTRN_MOVNTPS;
    *intrinsic_op = FALSE;
    break;
#if 0 // wgen TODO
    case GSBI_IX86_BUILTIN_SSE_ZERO:
      *iopc = INTRN_SSE_ZERO;
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_CLRTI:
      *iopc = INTRN_CLRTI;
      *intrinsic_op = FALSE;
      break;
#endif
  case GSBI_IX86_BUILTIN_PSHUFD:
    *iopc = INTRN_PSHUFD;
    break;
#if 0 // wgen TODO
    case GSBI_IX86_BUILTIN_LOADSS:
      *iopc = INTRN_LOADSS;
      break;
#endif
  case GSBI_IX86_BUILTIN_DIVPD:
    wn = WN_Div(res_type, arg0, arg1);
    *intrinsic_op = FALSE;
    break;
  case GSBI_IX86_BUILTIN_MULPD:
    wn = WN_Mpy(res_type, arg0, arg1);
    *intrinsic_op = FALSE;
    break;
  case GSBI_IX86_BUILTIN_SQRTPD:
    wn = WN_Sqrt(res_type, arg0);
    *intrinsic_op = FALSE;
    break;
  case GSBI_IX86_BUILTIN_MINPD:
    wn = WN_Binary(OPR_MIN, res_type, arg0, arg1);
    *intrinsic_op = FALSE;
    break;
  case GSBI_IX86_BUILTIN_MAXPD:
    wn = WN_Binary(OPR_MAX, res_type, arg0, arg1);
    *intrinsic_op = FALSE;
    break;
  case GSBI_IX86_BUILTIN_SHUFPD:
    *iopc = INTRN_SHUFPD;
    break;
  case GSBI_IX86_BUILTIN_XORPD:
    *iopc = INTRN_XORPD;
    break;
  case GSBI_IX86_BUILTIN_ANDPD:
    *iopc = INTRN_ANDPD;
    break;
  case GSBI_IX86_BUILTIN_ORPD:
    *iopc = INTRN_ORPD;
    break;
#if 0 // wgen TODO
    case GSBI_IX86_BUILTIN_STORELPD:
      *iopc = INTRN_STORELPD;
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_STOREHPD:
      *iopc = INTRN_STOREHPD;
      *intrinsic_op = FALSE;
      break;
#endif
  case GSBI_IX86_BUILTIN_LOADLPD:
    *iopc = INTRN_LOADLPD;
    break;
  case GSBI_IX86_BUILTIN_LOADHPD:
    *iopc = INTRN_LOADHPD;
    break;
  case GSBI_IX86_BUILTIN_UNPCKLPD:
    *iopc = INTRN_UNPCKLPD;
    break;
  case GSBI_IX86_BUILTIN_UNPCKHPD:
    *iopc = INTRN_UNPCKHPD;
    break;
  case GSBI_IX86_BUILTIN_LFENCE:
    *iopc = INTRN_LFENCE;
    *intrinsic_op = FALSE;
    break;
  case GSBI_IX86_BUILTIN_MFENCE:
    *iopc = INTRN_MFENCE;
    *intrinsic_op = FALSE;
    break;
  case GSBI_IX86_BUILTIN_SFENCE:
    *iopc = INTRN_SFENCE;
    *intrinsic_op = FALSE;
    break;
  case GSBI_IX86_BUILTIN_PSHUFW:
    *iopc = INTRN_PSHUFW;
    break;
#if 0 // wgen TODO
    case GSBI_IX86_BUILTIN_LOADDQA:
      *iopc = INTRN_LOADDQA;
      break;
#endif
  case GSBI_IX86_BUILTIN_LOADDQU:
    *iopc = INTRN_LOADDQU;
    break;
#if 0 // wgen TODO
    case GSBI_IX86_BUILTIN_STOREDQA:
      *iopc = INTRN_STOREDQA;
      *intrinsic_op = FALSE;
      break;
#endif
  case GSBI_IX86_BUILTIN_STOREDQU:
    *iopc = INTRN_STOREDQU;
    *intrinsic_op = FALSE;
    break;

  default:
  unsupported:
    if (Opt_Level >
        0) { // Don't assert in front-end. If used, backend will assert.
      *iopc = INTRN_UNIMP_PURE;
      if (res_type == MTYPE_V) {
        *iopc = INTRN_UNIMP;
        *intrinsic_op = FALSE;
      }
    } else {
      *intrinsic_op = FALSE;
      // For simplicity, generate a U8 constant, and then use a cvt
      // if necessary. If void result type, generate a placeholder eval.
      wn = WN_Intconst(MTYPE_U8, 0);
      if (res_type != MTYPE_U8 && res_type != MTYPE_V)
        wn = WN_Cvt(MTYPE_U8, res_type, wn);
    }
    break;
  }

  // The following instructions expect both arguments as FP (xmm), but
  // the 2nd argument type for the corresponding builtin is INT, so we
  // need to insert a CVT here.
  switch (ins_code) {
  case GSBI_IX86_BUILTIN_PSRAWI128:
  case GSBI_IX86_BUILTIN_PSRADI128:
  case GSBI_IX86_BUILTIN_PSLLWI128:
  case GSBI_IX86_BUILTIN_PSLLDI128:
  case GSBI_IX86_BUILTIN_PSLLQI128:
  case GSBI_IX86_BUILTIN_PSRLWI128:
  case GSBI_IX86_BUILTIN_PSRLDI128:
  case GSBI_IX86_BUILTIN_PSRLQI128:
    Is_True(wn == NULL, ("WGEN_target_builtins: null WN expected"));
    WN *args[2];
    // for (int c=0; c<2; c++)
    {
      // 1st argument
      TY_IDX arg_ty_idx = Get_TY(gs_tree_type(gs_tree_value(t_list)));
      TYPE_ID arg_mtype = TY_mtype(arg_ty_idx);
      args[0] = WN_CreateParm(Mtype_comparison(arg_mtype), arg0, arg_ty_idx,
                              WN_PARM_BY_VALUE);

      // 2nd argument
      arg1 = WN_Cvt(WN_rtype(arg1), MTYPE_V16I8, arg1);
      arg_ty_idx = MTYPE_TO_TY_array[WN_rtype(arg1)];
      arg_mtype = WN_rtype(arg1);
      args[1] = WN_CreateParm(Mtype_comparison(arg_mtype), arg1, arg_ty_idx,
                              WN_PARM_BY_VALUE);
    }

    wn = WN_Create_Intrinsic(OPR_INTRINSIC_OP, res_type, MTYPE_V, *iopc, 2,
                             args);
    break;
  }

  return wn;
}
#endif // TARG_X8664

// return the very first statement under a COMPOUND_EXPR node
gs_t first_in_compound_expr(gs_t node) {
  gs_t first = gs_tree_operand(node, 0);
  while (gs_tree_code(first) == GS_COMPOUND_EXPR)
    first = gs_tree_operand(first, 0);
  return first;
}

#ifdef KEY
extern BOOL processing_function_prototype;
#endif

bool findexit(gs_t body,
              gs_t &exit_node) // for java loop_expr management		//czw
{
  static gs_t first = 0;
  if (gs_tree_code(body) == GS_COMPOUND_EXPR) {
    findexit(gs_tree_operand(body, 1), exit_node);
    return findexit(gs_tree_operand(body, 0), exit_node);
  }
  if (gs_tree_code(body) == GS_BIND_EXPR) {
    if (first == 0)
      first = body;
  }
  if (gs_tree_code(body) == GS_EXIT_EXPR) {
    if (first == 0)
      first = body;
    exit_node = body;
  }
  bool ret = first == 0 ? false : gs_tree_code(first) == GS_EXIT_EXPR;
  first = 0;
  return ret;
}
WN *WGEN_Expand_Expr(gs_t exp, bool need_result, TY_IDX nop_ty_idx,
                     TY_IDX component_ty_idx, INT64 component_offset,
                     UINT32 field_id, bool is_bit_field,
                     bool is_aggr_init_via_ctor, WN *target_wn) {
  gs_code_t code = gs_tree_code(exp);
  WN *wn0, *wn1, *wn2, *wn;
  ST *st;
  TY_IDX ty_idx;
  TY_IDX desc_ty_idx;
  gs_t arg0, arg1, arg2;
#ifdef KEY
  static BOOL must_not_throw = FALSE;
#endif
  gs_code_t tmp_code;

  wn = NULL;

  switch (code) {
  // leaves
  case GS_ADDR_EXPR:
    wn = WGEN_Address_Of(gs_tree_operand(exp, 0));
    break;

    /*FDESC_EXPR:
     *Operand0 is a function constant; result is part N of a function
     *descriptor of type ptr_mode.
     *So we should get function constant and exprand it.
     */
#ifdef TARG_IA64
  case GS_FDESC_EXPR: {
    gs_t exp_operand = gs_tree_operand(exp, 0);
    FmtAssert(gs_tree_code(exp_operand) == GS_FUNCTION_DECL,
              ("Unexpected Tree Code!!"));
    st = Get_ST(exp_operand);
    ty_idx = ST_type(st);
    wn = WN_Lda(Pointer_Mtype, ST_ofst(st), st);
  } break;
#endif

  case GS_FUNCTION_DECL: {
    st = Get_ST(exp);
    ty_idx = ST_type(st);
    wn = WN_Lda(Pointer_Mtype, ST_ofst(st), st);
  } break;

  case GS_TREE_LIST: {
    gs_t stmt;
    for (stmt = gs_tree_purpose(exp); stmt; stmt = gs_tree_chain(stmt))
      WGEN_Expand_Stmt(stmt);
    wn = WGEN_Expand_Expr(gs_tree_value(exp));
  } break;

  case GS_DECL_EXPR: {
    gs_t decl = gs_decl_expr_decl(exp);
    WGEN_Expand_Decl(decl, TRUE);
    wn = WGEN_Expand_Expr(decl);
  } break;

  case GS_BIND_EXPR: {
    gs_t body;

    Register_Cleanup(exp); // KEY bug 11188
    body = gs_bind_expr_body(exp);
    if (gs_tree_code(body) != GS_STATEMENT_LIST)
      WGEN_Expand_Stmt(body); // only 1 statement
    else {
      gs_t stmt_list = gs_statement_list_elts(body);
      gs_t list;
      for (list = stmt_list; gs_code(list) != EMPTY; list = gs_operand(list, 1))
        WGEN_Expand_Stmt(gs_operand(list, 0));
    }
    Unregister_Cleanup(); // KEY bug 11188
  } break;

  case GS_TARGET_EXPR: {
    gs_t opnd0 = gs_tree_operand(exp, 0);
    st = NULL;
    TY_IDX ty;
    TYPE_ID mtype;
#ifdef KEY
    // If we are supposed to put the result in target_wn, then give the
    // init target the same ST as target_wn.
    if (target_wn != NULL) {
      if (WN_operator(target_wn) == OPR_LDA) {
        FmtAssert(gs_tree_code(opnd0) != GS_INDIRECT_REF,
                  ("WGEN_Expand_Expr: write target mismtach"));
        set_DECL_ST(opnd0, WN_st(target_wn));
      } else if (WN_operator(target_wn) == OPR_LDID) {
        // Change the target into an INDIRECT_REF only if we have not done
        // so.
        if (gs_tree_code(opnd0) == GS_VAR_DECL) {
          gs_t ptr_var = gs_build_decl(
              GS_VAR_DECL, gs_build_pointer_type(gs_tree_type(opnd0)));
          _gs_code(opnd0, GS_INDIRECT_REF);
          gs_set_tree_operand(opnd0, 0, ptr_var);
          set_DECL_ST(ptr_var, WN_st(target_wn));
        }
      }
    }

    // We might have changed the VAR_DECL to an INDIRECT_REF or it might
    // be a reference, in which case use the referenced symbol instead
    // of creating a new one.
    if (gs_tree_code(opnd0) != GS_INDIRECT_REF &&
        gs_tree_code(opnd0) != GS_COMPONENT_REF)
#endif
    {
      st = Get_ST(gs_tree_operand(exp, 0));
      ty = ST_type(st);
      mtype = TY_mtype(ty);
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
     * is_aggr_init_via_ctor to WGEN_Expand_Expr, so it can be dealt
     * with by the AGGR_INIT_EXPR/CALL_EXPR code.
     *
     * If a target expression is initialized by a target expression,
     * it ought not to have an associated cleanup, so we clear the
     * cleanup in this case.
     */
    gs_t t = gs_tree_operand(exp, 1);
#ifdef KEY
    if (t == NULL) {
      t = gs_tree_operand(exp, 3);
      FmtAssert(t != NULL,
                ("WGEN_Expand_Expr: no initializer found for TARGET_EXPR"));
    }
#endif
    if (gs_tree_code(t) == GS_TARGET_EXPR)
      gs_set_tree_operand(t, 2, 0);
    if (gs_tree_code(t) == GS_AGGR_INIT_EXPR && gs_aggr_init_via_ctor_p(t)) {
      gs_t args = gs_tree_operand(t, 1);
      /*
       * KEY: Based on above comment, the 0th opnd of target_expr is
       * used. But see simplify_aggr_init_expr() in gnu/cp, it uses 2nd
       * opnd (the slot) of aggr_init_expr. In spite of above comment,
       * this should be used. I will leave it this way for now because
       * for the current testcase both of them are same.
       *
       * Note, while processing an init_expr/modify_expr we update the
       * 0th opnd of target_expr, and depend on this code here to update
       * the aggr_init_expr. So in such scenario, using the slot would
       * be wrong.
       */
      gs_set_tree_value(args, gs_tree_operand(exp, 0));
      WGEN_Expand_Expr(t, false, 0, 0, 0, 0, false, true);
    } else {
#ifdef KEY
      gs_t ret_type = NULL;
      if (gs_tree_code(t) ==
          GS_AGGR_INIT_EXPR) { // bug 11159: Get the return type.
        ret_type =
            gs_tree_type(gs_tree_type(gs_tree_type(gs_tree_operand(t, 0))));
      }
      // If opnd 0 of the TARGET_EXPR is an INDIRECT_REF, then tell
      // WGEN_Expand_Expr to put the result in the area addressed by the
      // INDIRECT_REF.
      if (gs_tree_code(opnd0) == GS_INDIRECT_REF) {
        ST *st = Get_ST(gs_tree_operand(opnd0, 0));
        WN *ldid_wn = WN_Ldid(Pointer_Mtype, 0, st, ST_type(st));
        WN *result_wn =
            WGEN_Expand_Expr(t, TRUE, 0, 0, 0, 0, FALSE, FALSE, ldid_wn);
        // If the result of expanding t is not an indirect reference to the
        // result area we want, then it means t has not copied the value
        // into the result area.  Do the copy.
        if (result_wn && !(WN_operator(result_wn) == OPR_ILOAD &&
                           WN_operator(WN_kid0(result_wn)) == OPR_LDID &&
                           WN_st(WN_kid0(result_wn)) == st)) {
          WGEN_Stmt_Append(WN_Istore(WN_rtype(result_wn), 0, ST_type(st),
                                     WN_CopyNode(ldid_wn), result_wn),
                           Get_Srcpos());
        }
      }
      // If the initializer returns the object in memory, then make sure
      // the type doesn't require a copy constructor, since such types
      // sometimes require one.
      else if (TY_return_in_mem(Get_TY(gs_tree_type(t))) ||
               (gs_tree_code(t) == GS_AGGR_INIT_EXPR &&
                TY_return_in_mem(Get_TY(ret_type)))) {
        gs_code_t code = gs_tree_code(t);
        if (code == GS_VAR_DECL || code == GS_PARM_DECL) {
          // The initializer is a var or parm.  We need to insert copy.
          // First make sure type has no copy constructor.
          WN *rhs_wn = WGEN_Expand_Expr(t);
          gs_t type = gs_tree_type(gs_tree_operand(exp, 0));
          Is_True(!WGEN_has_copy_constructor(type),
                  ("WGEN_Expand_Expr: type require copy constructor"));
          WGEN_Stmt_Append(WN_Stid(mtype, ST_ofst(st), st, ty, rhs_wn),
                           Get_Srcpos());
        } else {
          // The initializer is an expression.  Try to expand it directly
          // into the target.
          WN *target_wn = WN_Lda(Pointer_Mtype, 0, st, 0);
          WN *result_wn =
              WGEN_Expand_Expr(t, TRUE, 0, 0, 0, 0, FALSE, FALSE, target_wn);
          // If expanding t did not write the result into the target as a
          // side effect, then create the copy.
          if (result_wn) {
            // There may be more cases where we need to store the result.
            // Need to find a better way to catch them all.
            if (WN_operator(result_wn) == OPR_ILOAD ||
                WN_operator(result_wn) == OPR_LDID) {
              WGEN_Stmt_Append(WN_Stid(mtype, ST_ofst(st), st, ty, result_wn),
                               Get_Srcpos());
            } else if (WN_operator(result_wn) == OPR_CSELECT) {
              WN *wn = WN_CreateEval(result_wn);
              WGEN_Stmt_Append(wn, Get_Srcpos());
            }
          }
        }
      } else {
        // Bug 7862: Set addr_saved flag if the initializer is an LDA.
        WN *init = WGEN_Expand_Expr(t, TRUE);
        if (init != NULL) {
          if (WN_operator(init) == OPR_LDA)
            WGEN_Set_ST_Addr_Saved(init);
          WGEN_Stmt_Append(WN_Stid(mtype, ST_ofst(st), st, ty, init),
                           Get_Srcpos());
        }
      }
#else
      WGEN_Stmt_Append(WN_Stid(mtype, ST_ofst(st), st, ty, WGEN_Expand_Expr(t)),
                       Get_Srcpos());
#endif
    }
    if (gs_tree_operand(exp, 2)
#ifdef KEY
        // We should not be emitting all cleanups
        && gs_emit_target_expr_cleanup(exp)
#endif
    )
#ifdef KEY
      Push_Temp_Cleanup(gs_tree_operand(exp, 2), true, gs_cleanup_eh_only(exp));
#else
      Push_Temp_Cleanup(gs_tree_operand(exp, 2), true);
#endif

#ifdef KEY
    // If the target area was supplied by the caller, then return an ILOAD
    // of the target pointer.
    if (gs_tree_code(opnd0) == GS_INDIRECT_REF) {

      if (gs_tree_code(gs_tree_operand(opnd0, 0)) == GS_NOP_EXPR)
        opnd0 = gs_tree_operand(opnd0, 0);
      ST *st = Get_ST(gs_tree_operand(opnd0, 0));
      TY_IDX ty_idx = Get_TY(gs_tree_type(exp));
      WN *ldid_wn = WN_Ldid(Pointer_Mtype, 0, st, ST_type(st));
      wn = WN_Iload(TY_mtype(ty_idx), 0, ty_idx, ldid_wn);
      break;
    } else if (gs_tree_code(opnd0) == GS_COMPONENT_REF) {
      wn = WGEN_Expand_Expr(opnd0);
      break;
    }
#endif
  }

  case GS_CONSTRUCTOR:
#ifdef KEY
    // In general, if the result is not needed and EXP has no side effects,
    // then there is no need to expand EXP, regardless of what EXP is.  This
    // is what gcc's expand_expr does.  However, doing so breaks the WHIRL
    // front-end, so limit this to CONSTRUCTOR for now.
    if (!need_result && !gs_tree_side_effects(exp)) {
      return NULL;
    }
#endif
    // fall thru

  case GS_PARM_DECL: // for formal parms
  case GS_VAR_DECL:
  case GS_RESULT_DECL: {
    UINT xtra_BE_ofst = 0; // only needed for big-endian target
    PREG_NUM preg_num = 0;
    desc_ty_idx = component_ty_idx;
    TY_IDX hi_ty_idx = Get_TY(gs_tree_type(exp));
    if (desc_ty_idx == 0)
      desc_ty_idx = hi_ty_idx;

    if (!MTYPE_is_integral(TY_mtype(desc_ty_idx)))
      ty_idx = desc_ty_idx;
    else {
      ty_idx = nop_ty_idx;
      if (ty_idx == 0)
        ty_idx = desc_ty_idx;
    }

    UINT cvtl_size = 0; // if non-zero, need to generate CVTL with this size
    if (!is_bit_field) {
      if (TY_size(desc_ty_idx) > TY_size(ty_idx)) {
        if (Target_Byte_Sex == BIG_ENDIAN)
          xtra_BE_ofst = TY_size(desc_ty_idx) - TY_size(ty_idx);
        cvtl_size = TY_size(ty_idx) * 8;
        ty_idx = desc_ty_idx;
      }
    } else {
      if (TY_size(desc_ty_idx) > TY_size(ty_idx))
        ty_idx = desc_ty_idx;
    }

    TYPE_ID rtype = Widen_Mtype(TY_mtype(ty_idx));
    TYPE_ID desc = TY_mtype(desc_ty_idx);
    if (MTYPE_is_integral(desc)) {
      if (MTYPE_signed(rtype) != MTYPE_signed(desc)) {
        if (MTYPE_size_min(rtype) > MTYPE_size_min(desc) || is_bit_field)
          rtype = Mtype_TransferSign(desc, rtype);
        else
          desc = Mtype_TransferSign(rtype, desc);
      }
    }

    if (gs_tree_this_volatile(exp))
      Set_TY_is_volatile(ty_idx);
#if 1 // wgen bug 10470
    else
      Clear_TY_is_volatile(ty_idx);
#endif

#ifdef KEY
    if (code == GS_VAR_DECL && gs_decl_value_expr(exp)) {
      wn = WGEN_Expand_Expr(gs_decl_value_expr(exp), need_result, nop_ty_idx,
                            component_ty_idx, component_offset, field_id,
                            is_bit_field);
      if (cvtl_size != 0)
        wn = WN_CreateCvtl(OPR_CVTL, rtype, MTYPE_V, cvtl_size, wn);
      break;
    }
#endif

    if (code == GS_PARM_DECL || code == GS_VAR_DECL || code == GS_RESULT_DECL) {
      st = Get_ST(exp);
      if (ST_assigned_to_dedicated_preg(st))
        Set_TY_is_volatile(ty_idx);
    } else if (code == GS_CONSTRUCTOR) {
      DevWarn("Encountered CONSTRUCTOR at line %d", lineno);
      st = WGEN_Generate_Temp_For_Initialized_Aggregate(exp, "");
    }

    Is_True(!is_bit_field || field_id <= MAX_FIELD_ID,
            ("WGEN_Expand_Expr: field id for bit-field exceeds limit"));
    wn = WN_CreateLdid(OPR_LDID, rtype, is_bit_field ? MTYPE_BS : desc,
                       ST_ofst(st) + component_offset + xtra_BE_ofst + preg_num,
                       st, field_id != 0 ? hi_ty_idx : ty_idx, field_id);
    if (cvtl_size != 0)
      wn = WN_CreateCvtl(OPR_CVTL, rtype, MTYPE_V, cvtl_size, wn);
  } break;

  case GS_COMPOUND_LITERAL_EXPR: {
    gs_t oper = gs_tree_operand(gs_tree_operand(exp, 0), 0);
    if (gs_tree_code(gs_decl_initial(oper)) == GS_CONSTRUCTOR)
      exp = gs_decl_initial(oper);
    else
      exp = oper;

    desc_ty_idx = component_ty_idx;
    TY_IDX hi_ty_idx = Get_TY(gs_tree_type(exp));
    if (desc_ty_idx == 0)
      desc_ty_idx = hi_ty_idx;

    if (!MTYPE_is_integral(TY_mtype(desc_ty_idx)))
      ty_idx = desc_ty_idx;
    else {
      ty_idx = nop_ty_idx;
      if (ty_idx == 0)
        ty_idx = desc_ty_idx;
    }

    UINT cvtl_size = 0; // if non-zero, need to generate CVTL with this size
    if (!is_bit_field) {
      if (TY_size(desc_ty_idx) > TY_size(ty_idx)) {
        cvtl_size = TY_size(ty_idx) * 8;
        ty_idx = desc_ty_idx;
      }
    } else {
      if (TY_size(desc_ty_idx) > TY_size(ty_idx))
        ty_idx = desc_ty_idx;
    }

    TYPE_ID rtype = Widen_Mtype(TY_mtype(ty_idx));
    TYPE_ID desc = TY_mtype(desc_ty_idx);
    if (MTYPE_is_integral(desc)) {
      if (MTYPE_signed(rtype) != MTYPE_signed(desc)) {
        if (MTYPE_size_min(rtype) > MTYPE_size_min(desc) || is_bit_field)
          rtype = Mtype_TransferSign(desc, rtype);
        else
          desc = Mtype_TransferSign(rtype, desc);
      }
    }

    if (gs_tree_this_volatile(exp))
      Set_TY_is_volatile(ty_idx);

    if (gs_tree_code(exp) == GS_CONSTRUCTOR)
      st = WGEN_Generate_Temp_For_Initialized_Aggregate(exp, "");
    else {
      WGEN_Initialize_Decl(exp);
      st = Get_ST(exp);
      if (ST_assigned_to_dedicated_preg(st))
        Set_TY_is_volatile(ty_idx);
    }
    wn = WN_CreateLdid(OPR_LDID, rtype, is_bit_field ? MTYPE_BS : desc,
                       ST_ofst(st) + component_offset, st,
                       field_id != 0 ? hi_ty_idx : ty_idx, field_id);
    if (cvtl_size != 0)
      wn = WN_CreateCvtl(OPR_CVTL, rtype, MTYPE_V, cvtl_size, wn);
  } break;

  case GS_CONST_DECL:
    wn = WGEN_Expand_Expr(gs_decl_initial(exp), need_result);
    break;

  case GS_INTEGER_CST: {
    ty_idx = Get_TY(gs_tree_type(exp));
    TYPE_ID mtyp = TY_mtype(ty_idx);
    mtyp = (mtyp == MTYPE_V || mtyp == MTYPE_M) ? MTYPE_I4 : Widen_Mtype(mtyp);
    // if(!lang_java) //ykq for java long type is 64 bits
    wn = WN_Intconst(mtyp, gs_get_integer_value(exp));
    // else
    // wn = WN_Intconst(mtyp,
    // (gs_get_integer_highvalue(exp)<<32|gs_get_integer_value(exp)));
  } break;

  case GS_PTRMEM_CST:
    wn = WGEN_Expand_Expr(gs_expanded_ptrmem_cst(exp), need_result, nop_ty_idx,
                          component_ty_idx, component_offset, field_id);
    break;

  case GS_EMPTY_CLASS_EXPR: // bugs 10846, 11138
    ty_idx = Get_TY(gs_tree_type(exp));
    st = Gen_Temp_Symbol(ty_idx, "__empty_class_expr");
#if 0 // wgen TODO
      WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, st);
#endif
    wn = WN_Ldid(TY_mtype(ty_idx), 0, st, ty_idx);
    break;

  case GS_REAL_CST: {
    TCON tcon;
    ty_idx = Get_TY(gs_tree_type(exp));
    switch (TY_size(ty_idx)) {
    case 4:
      tcon = Host_To_Targ_Float_4(MTYPE_F4, gs_tree_real_cst_f(exp));
      break;
    case 8:
      tcon = Host_To_Targ_Float(MTYPE_F8, gs_tree_real_cst_d(exp));
      break;
#ifdef TARG_IA64
    case 12:
    case 16:
      tcon = Host_To_Targ_Float_10(MTYPE_F10, gs_tree_real_cst_ld(exp));
      break;
#else
    case 12:
    case 16:
      tcon = Host_To_Targ_Quad(gs_tree_real_cst_ld(exp));
      break;
#endif
    default:
      FmtAssert(FALSE, ("WGEN_Expand_Expr: unexpected size for real constant"));
      break;
    }
    st = New_Const_Sym(Enter_tcon(tcon), ty_idx);
    wn = WN_CreateConst(OPR_CONST, TY_mtype(ty_idx), MTYPE_V, st);
  } break;

  case GS_COMPLEX_CST: {
    TCON tcon;
    ty_idx = Get_TY(gs_tree_type(exp));
#ifdef KEY
    // Bug 949
    if (gs_tree_code(gs_tree_realpart(exp)) != GS_REAL_CST ||
        gs_tree_code(gs_tree_imagpart(exp)) != GS_REAL_CST) {
      printf("%s does not support complex integer data types "
             "(a GNU extension)\n",
             lang_cplus ? "pathCC" : "pathcc");
      exit(2);
    }
#endif
    switch (TY_size(ty_idx)) {
    case 8:
      tcon = Host_To_Targ_Complex_4(MTYPE_C4,
                                    gs_tree_real_cst_f(gs_tree_realpart(exp)),
                                    gs_tree_real_cst_f(gs_tree_imagpart(exp)));
      break;
    case 16:
      tcon = Host_To_Targ_Complex(MTYPE_C8,
                                  gs_tree_real_cst_d(gs_tree_realpart(exp)),
                                  gs_tree_real_cst_d(gs_tree_imagpart(exp)));
      break;
#ifdef TARG_IA64
    case 32:
      tcon = Host_To_Targ_Complex_10(
          MTYPE_C10, gs_tree_real_cst_ld(gs_tree_realpart(exp)),
          gs_tree_real_cst_ld(gs_tree_imagpart(exp)));
      break;
#else
    case 24:
    case 32:
      tcon =
          Host_To_Targ_Complex_Quad(gs_tree_real_cst_ld(gs_tree_realpart(exp)),
                                    gs_tree_real_cst_ld(gs_tree_imagpart(exp)));
      break;
#endif
    default:
      FmtAssert(FALSE,
                ("WGEN_Expand_Expr: unexpected size for complex constant"));
      break;
    }
    st = New_Const_Sym(Enter_tcon(tcon), ty_idx);
    wn = WN_CreateConst(OPR_CONST, TY_mtype(ty_idx), MTYPE_V, st);
  } break;

  // this should occur only if string is a statement expression
  case GS_STRING_CST: {
    TCON tcon;
    tcon = Host_To_Targ_String(MTYPE_STRING,
                               const_cast<char *>(gs_tree_string_pointer(exp)),
                               gs_tree_string_length(exp));
    ty_idx = Get_TY(gs_tree_type(exp));
    st = New_Const_Sym(Enter_tcon(tcon), ty_idx);
    wn = WN_Lda(Pointer_Mtype, ST_ofst(st), st);
    TREE_STRING_ST(exp) = st;
  } break;

  // unary ops
  case GS_BIT_NOT_EXPR:
  case GS_ABS_EXPR:
  case GS_NEGATE_EXPR:
  case GS_REALPART_EXPR:
  case GS_IMAGPART_EXPR: {
    TYPE_ID mtyp = TY_mtype(Get_TY(gs_tree_type(exp)));
#ifdef KEY
    // Bug 949, 11316
    if ((code == GS_REALPART_EXPR || code == GS_IMAGPART_EXPR) &&
        !MTYPE_float(mtyp)) {
      printf("%s does not support complex integer data types "
             "(a GNU extension)\n",
             lang_cplus ? "pathCC" : "pathcc");
      exit(2);
    }
#endif

    wn0 = WGEN_Expand_Expr(gs_tree_operand(exp, 0));
    wn = WN_Unary(Operator_From_Tree[code].opr, Widen_Mtype(mtyp), wn0);
#ifdef KEY // bug 2648
    if (mtyp != WN_rtype(wn))
      wn = WN_CreateCvtl(OPR_CVTL, WN_rtype(wn), MTYPE_V, MTYPE_size_min(mtyp),
                         wn);
#endif
  } break;

  case GS_TRUTH_NOT_EXPR:
    wn0 = WGEN_Expand_Expr(gs_tree_operand(exp, 0));
    wn1 = WN_Intconst(MTYPE_I4, 0);
    wn = WN_Relational(OPR_EQ, MTYPE_I4, wn0, wn1);
    break;

  case GS_CONJ_EXPR: {
    ty_idx = Get_TY(gs_tree_type(exp));
    TYPE_ID complex_mtype = TY_mtype(ty_idx);
    TYPE_ID float_mtype = Mtype_complex_to_real(complex_mtype);
    wn0 = WGEN_Expand_Expr(gs_tree_operand(exp, 0));
    if (WN_has_side_effects(wn0)) {
      ST *preg_st;
      PREG_NUM preg;
      preg_st = MTYPE_To_PREG(complex_mtype);
      preg = Create_Preg(complex_mtype, NULL);
      wn0 = WN_Stid(complex_mtype, preg, preg_st, ty_idx, wn0);
      WGEN_Stmt_Append(wn0, Get_Srcpos());
      wn0 = WN_Ldid(complex_mtype, preg, preg_st, ty_idx);
    }
#ifdef KEY
    // Fix bug 603
    wn = WN_Binary(OPR_COMPLEX, complex_mtype,
                   WN_Unary(OPR_REALPART, float_mtype, wn0),
                   WN_Unary(OPR_NEG, float_mtype,
                            WN_Unary(OPR_IMAGPART, float_mtype, wn0)));
#else
    wn = WN_Binary(OPR_COMPLEX, complex_mtype,
                   WN_Unary(OPR_REALPART, float_mtype, wn0),
                   WN_Unary(OPR_NEG, float_mtype,
                            WN_Unary(OPR_REALPART, float_mtype, wn0)));
#endif
  } break;

  case GS_NOP_EXPR: {
    ty_idx = Get_TY(gs_tree_type(exp));
    TYPE_ID mtyp = TY_mtype(ty_idx);
    // do not pass struct type down because will cause rtype of MTYPE_M
    wn = WGEN_Expand_Expr(gs_tree_operand(exp, 0), TRUE,
                          (mtyp == MTYPE_M || mtyp == MTYPE_V) ? 0 : ty_idx,
                          component_ty_idx, component_offset, field_id,
                          is_bit_field
#ifdef KEY
                          ,
                          FALSE, target_wn
#endif
    );
    if (mtyp == MTYPE_V)
      break;
    if (mtyp == MTYPE_M)
      break;
    if (WN_rtype(wn) == MTYPE_M)
      break;
    if (MTYPE_is_integral(mtyp) && MTYPE_is_integral(WN_rtype(wn))) {
      // For 32-bit to 64-bit conversion, make the result have the same
      // sign as the source.  Fix bug 480.
      if (MTYPE_size_min(mtyp) == 64 && MTYPE_size_min(WN_rtype(wn)) == 32 &&
          MTYPE_is_signed(mtyp) != MTYPE_is_signed(WN_rtype(wn))) {
        mtyp = MTYPE_complement(mtyp);
      }

      if (MTYPE_size_min(mtyp) < MTYPE_size_min(WN_rtype(wn))) {
        if (MTYPE_size_min(mtyp) != 32)
          wn = WN_CreateCvtl(OPR_CVTL, Widen_Mtype(mtyp), MTYPE_V,
                             MTYPE_size_min(mtyp), wn);
        else
          wn = WN_Cvt(WN_rtype(wn), mtyp, wn);
      } else {
        TY_IDX ty_idx0 = Get_TY(gs_tree_type(gs_tree_operand(exp, 0)));
        TYPE_ID mtyp0 = TY_mtype(ty_idx0);

        if (MTYPE_size_min(mtyp) > MTYPE_size_min(mtyp0) &&
            !Has_Subsumed_Cvtl(WN_operator(wn)))
          wn = WN_CreateCvtl(OPR_CVTL, Widen_Mtype(mtyp0), MTYPE_V,
                             MTYPE_size_min(mtyp0), wn);

        if (MTYPE_size_min(mtyp) > MTYPE_size_min(WN_rtype(wn)))
          wn = WN_Cvt(WN_rtype(wn), mtyp, wn);
        else { // same size
          if (mtyp != WN_rtype(wn))
            wn = WN_Cvt(WN_rtype(wn), mtyp, wn);
        }
      }
    } else {
      if (mtyp != WN_rtype(wn))
        wn = WN_Cvt(WN_rtype(wn), mtyp, wn);
    }
  } break;

  case GS_COMPONENT_REF: {
    INT64 ofst;
    arg0 = gs_tree_operand(exp, 0);
    arg1 = gs_tree_operand(exp, 1);
    // If this is an indirect of a nop_expr, we may need to fix the
    // type of the nop_expr:
    (void)Get_TY(gs_tree_type(arg0));

    if (component_ty_idx == 0)
      ty_idx = Get_TY(gs_tree_type(exp));
    else
      ty_idx = component_ty_idx;
    if (gs_decl_bit_field(arg1))
      is_bit_field = TRUE;

    if (!is_bit_field &&
        component_ty_idx == 0) { // only for top-level COMPONENT_REF
      // if size does not agree with ty_idx, fix ty_idx
      gs_t sizenode = gs_decl_size(arg1);
      if (
#ifdef KEY
          sizenode && // bug 11726, in absence of size expression
#endif
          gs_tree_code(sizenode) == GS_INTEGER_CST) {
        TYPE_ID c_mtyp = TY_mtype(ty_idx);
        INT32 bsize = gs_get_integer_value(sizenode);
        if (MTYPE_size_min(c_mtyp) > bsize) {
          FmtAssert(MTYPE_is_integral(c_mtyp),
                    ("COMPONENT_REF: integer type expected at inconsistent "
                     "field size"));
          c_mtyp = Mtype_AlignmentClass(bsize >> 3, MTYPE_type_class(c_mtyp));
          ty_idx = MTYPE_To_TY(c_mtyp);
        }
      }
    }

    if (!is_bit_field)
      ofst = (BITSPERBYTE * gs_get_integer_value(gs_decl_field_offset(arg1)) +
              gs_get_integer_value(gs_decl_field_bit_offset(arg1))) /
             BITSPERBYTE;
    else
      ofst = 0;
#ifdef KEY
    FmtAssert(DECL_FIELD_ID(arg1) != 0,
              ("WGEN_Expand_Expr: DECL_FIELD_ID used but not set"));

    // If arg0 is a CALL_EXPR that returns a ptr-to-member-function, then
    // call WGEN_Expand_Ptr_To_Member_Func_Call_Expr to expand it.
    // Otherwise, call WGEN_Expand_Expr to do regular expansion.
    // Bug 3400, 3427.
    if (WGEN_Call_Returns_Ptr_To_Member_Func(arg0)) {
      gs_t field0 = gs_type_fields(gs_tree_type(arg0));
      // Get_TY(gs_tree_type(field0)) is valid only if
      // WGEN_Call_Returns_Ptr_To_Member_Func(arg0)) is TRUE.  Bug 6022.
      TYPE_ID desc = TY_mtype(Get_TY(gs_tree_type(field0)));
      wn = WGEN_Expand_Ptr_To_Member_Func_Call_Expr(
          arg0, nop_ty_idx, Pointer_Mtype, desc, component_offset,
          field_id + DECL_FIELD_ID(arg1));
    } else
#endif
      wn = WGEN_Expand_Expr(arg0, TRUE, nop_ty_idx, ty_idx,
                            ofst + component_offset,
                            field_id + DECL_FIELD_ID(arg1), is_bit_field);

#ifdef KEY
    // For code such as (p->a = q->a).b, the gnu tree is:
    //   component_ref
    //     modify_expr
    //       indirect_ref
    //       indirect_ref
    // WGEN_Expand_Expr will call WGEN_Lhs_Of_Modify_Expr to expand the
    // modify_expr.  WGEN_Lhs_Of_Modify_Expr will return an iload
    // corresponding to p->a.  Since we want p->a.b, recreate the iload
    // here.  Bug 3122 and 3210
    if (gs_tree_code(arg0) == GS_MODIFY_EXPR) {
      TYPE_ID rtype = Widen_Mtype(TY_mtype(ty_idx));
      TYPE_ID desc = TY_mtype(ty_idx);
      if (WN_operator(wn) == OPR_ILOAD) {
        wn = WN_CreateIload(OPR_ILOAD, rtype, desc, ofst + component_offset,
                            ty_idx, Make_Pointer_Type(ty_idx, FALSE),
                            WN_kid0(wn), field_id + DECL_FIELD_ID(arg1));
      } else if (WN_operator(wn) == OPR_LDID) {
        WN_set_rtype(wn, rtype);
        WN_set_desc(wn, desc);
        WN_offset(wn) = WN_offset(wn) + ofst + component_offset;
        WN_set_field_id(wn, field_id + DECL_FIELD_ID(arg1));
      }
    }
    // bug 6122
    // Handle code like (x == 1 ? p->a : p->b).c
    else if (gs_tree_code(arg0) == GS_COND_EXPR &&
             WN_operator(wn) == OPR_CSELECT && WN_rtype(wn) == MTYPE_M) {
      // kid1 and kid2 must be type M and must be of the same struct type
      Is_True(WN_rtype(WN_kid1(wn)) == MTYPE_M, ("Unexpected type"));
      // code adapted from vho
      TY_IDX temp_ty_idx = WN_ty(WN_kid1(wn));
      // Get the struct type corresponding to the field
      if (WN_field_id(WN_kid1(wn)))
        temp_ty_idx = get_field_type(temp_ty_idx, WN_field_id(WN_kid1(wn)));
      // Store into temp symbol
      ST *temp = Gen_Temp_Symbol(temp_ty_idx, ".mcselect_store");
      wn = WN_Stid(MTYPE_M, 0, temp, temp_ty_idx, wn);
      WGEN_Stmt_Append(wn, Get_Srcpos());
      // Load correct field from temp symbol
      wn = WN_Ldid(TY_mtype(ty_idx), ofst + component_offset, temp, temp_ty_idx,
                   field_id + DECL_FIELD_ID(arg1));
    }
#endif
  }
    // czw {
    if (key_exceptions && lang_java && !(in_cleanup)) {
      if (!inside_eh_region) { // check that we are not already in a region
        WN *region_body = WN_CreateBlock();
        inside_eh_region = true;
        WGEN_Stmt_Push(region_body, wgen_stmk_call_region_body, Get_Srcpos());
      }
    } // else if (key_exceptions && inside_eh_region && opt_regions)
    //{
    // The above conditions dictate that this call MUST not be inside
    // a region. So close the region.
    // TODO: Is this only for opt_regions or in general?
    // if (Check_For_Call_Region ())
    // Did_Not_Terminate_Region = FALSE;
    //}
    // czw }
    break;

  case GS_INDIRECT_REF: {
    UINT xtra_BE_ofst = 0; // only needed for big-endian target
    wn0 = WGEN_Expand_Expr(gs_tree_operand(exp, 0));

    TY_IDX hi_ty_idx;
    if (gs_tree_code(gs_tree_type(exp)) == GS_VOID_TYPE)
      hi_ty_idx = MTYPE_To_TY(MTYPE_I4); // dummy; for bug 10176 Comment #4
    else
      hi_ty_idx = Get_TY(gs_tree_type(exp));

    desc_ty_idx = component_ty_idx;
    if (desc_ty_idx == 0)
      desc_ty_idx = hi_ty_idx;

    if (!MTYPE_is_integral(TY_mtype(desc_ty_idx)))
      ty_idx = desc_ty_idx;
    else {
      ty_idx = nop_ty_idx;
      if (ty_idx == 0)
        ty_idx = desc_ty_idx;
    }

    if (!is_bit_field) {
      if (TY_size(desc_ty_idx) > TY_size(ty_idx)) {
        if (Target_Byte_Sex == BIG_ENDIAN)
          xtra_BE_ofst = TY_size(desc_ty_idx) - TY_size(ty_idx);
        desc_ty_idx = ty_idx;
      }
    } else {
      if (TY_size(desc_ty_idx) > TY_size(ty_idx))
        ty_idx = desc_ty_idx;
    }

    TYPE_ID rtype = Widen_Mtype(TY_mtype(ty_idx));
    TYPE_ID desc = TY_mtype(desc_ty_idx);
    if (MTYPE_is_integral(desc)) {
      if (MTYPE_signed(rtype) != MTYPE_signed(desc)) {
        if (MTYPE_size_min(rtype) > MTYPE_size_min(desc) || is_bit_field)
          rtype = Mtype_TransferSign(desc, rtype);
        else
          desc = Mtype_TransferSign(rtype, desc);
      }
    }

    if (gs_tree_this_volatile(exp))
      Set_TY_is_volatile(hi_ty_idx);

    Is_True(!is_bit_field || field_id <= MAX_FIELD_ID,
            ("WGEN_Expand_Expr: field id for bit-field exceeds limit"));

    if (!WGEN_Keep_Zero_Length_Structs && rtype == MTYPE_M &&
        TY_size(hi_ty_idx) == 0) {
      if (WN_has_side_effects(wn0)) {
        wn = WN_CreateEval(wn0);
        WGEN_Stmt_Append(wn, Get_Srcpos());
      }
      wn = NULL;
    } else {
      // special case indexing into a constant string
      if (WN_operator(wn0) == OPR_LDA && ST_class(WN_st(wn0)) == CLASS_CONST &&
          is_bit_field == FALSE && field_id == 0) {
        st = WN_st(wn0);
        TCON tcon = Tcon_Table[ST_tcon(st)];
        if (TCON_ty(tcon) == MTYPE_STRING && TY_size(Be_Type_Tbl(desc)) == 1) {
          mUINT32 len = Targ_String_Length(tcon);
          mUINT64 offset = component_offset + xtra_BE_ofst + WN_offset(wn0);
          if (offset <= len && desc == MTYPE_U1 &&
              (rtype == MTYPE_U4 || rtype == MTYPE_U8)) {
            unsigned char *cp = (unsigned char *)Targ_String_Address(tcon);
            unsigned long long val = cp[offset];
            wn = WN_Intconst(rtype, val);
            break;
          } else if (offset <= len && desc == MTYPE_I1 &&
                     (rtype == MTYPE_I4 || rtype == MTYPE_I8)) {
            signed char *cp = (signed char *)Targ_String_Address(tcon);
            signed long long val = cp[offset];
            wn = WN_Intconst(rtype, val);
            break;
          }
        }
      }
      // NOTE: In GNU4, this may be a REFERENCE_REF_P.
      if (need_result)
        wn = WN_CreateIload(OPR_ILOAD, rtype, is_bit_field ? MTYPE_BS : desc,
                            component_offset + xtra_BE_ofst,
                            field_id != 0 ? hi_ty_idx : ty_idx,
                            Make_Pointer_Type(hi_ty_idx, FALSE), wn0, field_id);
      else if (WN_has_side_effects(wn0))
        wn = wn0;
    }
  }
    // czw {
    if (key_exceptions && lang_java && !(in_cleanup)) {
      if (!inside_eh_region) { // check that we are not already in a region
        WN *region_body = WN_CreateBlock();
        inside_eh_region = true;
        WGEN_Stmt_Push(region_body, wgen_stmk_call_region_body, Get_Srcpos());
      }
    } // else if (key_exceptions && inside_eh_region && opt_regions)
    //{
    // The above conditions dictate that this call MUST not be inside
    // a region. So close the region.
    // TODO: Is this only for opt_regions or in general?
    // if (Check_For_Call_Region ())
    // Did_Not_Terminate_Region = FALSE;
    //}
    // czw }
    break;

  case GS_CONVERT_EXPR:
  case GS_FLOAT_EXPR: {
#ifndef KEY // bug 10967
    wn0 = WGEN_Expand_Expr(gs_tree_operand(exp, 0));
#endif
    ty_idx = Get_TY(gs_tree_type(exp));
    TYPE_ID mtyp = TY_mtype(ty_idx);
#ifdef KEY
    // Bug 10967: Don't ask for a result if it is not required.
    // In the GCC 4 front-end, CONVERT_EXPR is often used to call
    // a function (using opnd0), and needlessly asking for the result
    // confuses the logic regarding how to close the exception
    // region. It causes the EH region to close before the call WN is
    // actually emitted.
    wn0 = WGEN_Expand_Expr(gs_tree_operand(exp, 0),
                           need_result || (mtyp != MTYPE_V));
#endif
    if (mtyp == MTYPE_V)
      wn = wn0;
    else if (MTYPE_byte_size(mtyp) < 4 && MTYPE_is_integral(WN_rtype(wn0)))
      wn = WN_CreateCvtl(OPR_CVTL, Widen_Mtype(mtyp), MTYPE_V,
                         MTYPE_size_min(mtyp), wn0);
    else {
      mtyp = Widen_Mtype(TY_mtype(ty_idx));
      if (mtyp == WN_rtype(wn0) || mtyp == MTYPE_V)
        wn = wn0;
      else {
#ifdef KEY // prevent zero extension when converting to 64-bit address type
        if (gs_tree_code(gs_tree_type(exp)) == GS_POINTER_TYPE &&
            MTYPE_byte_size(FE_Pointer_Type_To_Mtype()) == 8) {
          if (WN_operator(wn0) == OPR_CVT && WN_desc(wn0) == MTYPE_U4) {
            WN_set_desc(wn0, MTYPE_I4);
            wn = WN_Cvt(WN_rtype(wn0), mtyp, wn0);
          } else if (MTYPE_byte_size(WN_rtype(wn0) == 4))
            wn = WN_Cvt(MTYPE_I4, mtyp, wn0);
          else
            wn = WN_Cvt(WN_rtype(wn0), mtyp, wn0);
        } else
#endif
          wn = WN_Cvt(WN_rtype(wn0), mtyp, wn0);
        // The following opcodes are not valid for MIPS
        if (WN_opcode(wn) == OPC_I4U4CVT || WN_opcode(wn) == OPC_U4I4CVT ||
            WN_opcode(wn) == OPC_I8U8CVT || WN_opcode(wn) == OPC_U8I8CVT) {
          wn = WN_kid0(wn);
        }
      }
    }
  } break;

  case GS_FIX_TRUNC_EXPR: {
    wn0 = WGEN_Expand_Expr(gs_tree_operand(exp, 0));
    ty_idx = Get_TY(gs_tree_type(exp));
    TYPE_ID mtyp = Widen_Mtype(TY_mtype(ty_idx));
    wn = WN_Trunc(WN_rtype(wn0), mtyp, wn0);
  } break;

  case GS_EXPR_STMT: {
#ifdef KEY
    // bug 11169: The caller may need_result, so we should not use false.
    wn = WGEN_Expand_Expr(gs_expr_stmt_expr(exp), need_result, nop_ty_idx,
                          component_ty_idx, component_offset, field_id,
                          is_bit_field, is_aggr_init_via_ctor);
#else
    wn = WGEN_Expand_Expr(gs_expr_stmt_expr(exp), false);
#endif
  } break;

  case GS_STMT_EXPR: {
#ifdef KEY
    bool write_to_target_wn = TRUE;

    // If we need to store the result in target_wn, then give result the
    // same ST as the ST in target_wn.  To do this, first find the
    // STMT_EXPR's result, which is returned by the last EXPR_STMT in the
    // COMPOUND_STMT.  Based on code in gnu/c-common.c:c_expand_expr.
    if (target_wn != NULL &&
        gs_tree_code(gs_stmt_expr_stmt(exp)) == GS_COMPOUND_EXPR &&
        gs_tree_code(first_in_compound_expr(gs_stmt_expr_stmt(exp))) ==
            GS_SCOPE_STMT) {
      gs_t expr = gs_tree_operand(gs_stmt_expr_stmt(exp), 0);
      gs_t last = gs_tree_operand(gs_stmt_expr_stmt(exp), 1);

      while (gs_tree_code(expr) == GS_COMPOUND_EXPR) {
        expr = gs_tree_operand(expr, 1);
      }

      if (gs_tree_code(last) == GS_SCOPE_STMT &&
          gs_tree_code(expr) == GS_EXPR_STMT) {
        if (gs_tree_code(gs_expr_stmt_expr(expr)) == GS_VAR_DECL) {
          // If the last expression is a variable, then the variable is the
          // returned value.
          gs_t var_decl = gs_expr_stmt_expr(expr);
          ST *st = DECL_ST(var_decl);
          if (st == NULL) {
            // Give the returned var_decl the same ST indicated by
            // target_wn.
            if (WN_operator(target_wn) == OPR_LDA) {
              set_DECL_ST(var_decl, WN_st(target_wn));
            } else if (WN_operator(target_wn) == OPR_LDID) {
              // target_wn is an ldid of the fake first parm.  Change the
              // stmt_expr's var_decl to be an indirect_ref of the fake
              // parm.
              gs_t ptr_var = gs_build_decl(
                  GS_VAR_DECL, gs_build_pointer_type(gs_tree_type(var_decl)));
              _gs_code(var_decl, GS_INDIRECT_REF);
              gs_set_tree_operand(var_decl, 0, ptr_var);
              set_DECL_ST(ptr_var, WN_st(target_wn));
            } else {
              FmtAssert(FALSE,
                        ("WGEN_Expand_Expr: unexpected operator in target_wn"));
            }
          } else {
            // The var_decl already has a ST assigned.  This should be the
            // same ST as the target_wn.
            FmtAssert(st == WN_st(target_wn),
                      ("WGEN_Expand_Expr: STs are different"));
          }
          // Don't need target_wn anymore since the returned var_decl
          // already has the target ST.
          write_to_target_wn = FALSE;

        } else if (gs_tree_code(gs_expr_stmt_expr(expr)) == GS_INDIRECT_REF) {
          // The indirect_ref must have been a var_decl that was changed by
          // kg++fe to an indirect_ref.  This means we are already writing
          // to the target location.
          write_to_target_wn = FALSE;
        }
      }
    }
    wn = WGEN_Expand_Expr(gs_tree_operand(exp, 0), need_result, nop_ty_idx,
                          component_ty_idx, component_offset, field_id,
                          is_bit_field, is_aggr_init_via_ctor,
                          write_to_target_wn ? target_wn : NULL);
#else
    wn = WGEN_Expand_Expr(gs_tree_operand(exp, 0), need_result);
#endif
  } break;

#ifndef KEY
  case GS_SUBOBJECT:
    break;
#endif // !KEY

  case GS_CLEANUP_POINT_EXPR: {
    Push_Temp_Cleanup(exp, false);
#ifdef KEY
    // A RETURN_EXPR inside the CLEANUP_POINT_EXPR will generate a RETURN
    // WN, causing execution to bypass the cleanups that are generated by
    // the Do_Temp_Cleanups below.  As a fix, tell Expand_Return to emit
    // all the cleanups before emitting the RETURN WN.  Bug 11350.
    BOOL done_cleanups = FALSE;
    gs_t old_enclosing_cleanup_point_expr = enclosing_cleanup_point_expr;
    enclosing_cleanup_point_expr = exp;

    // bug 10850
    wn = WGEN_Expand_Expr(gs_tree_operand(exp, 0), need_result);

    // NULL means cleanups already done.
    if (enclosing_cleanup_point_expr == NULL)
      done_cleanups = TRUE;
#else
    wn = WGEN_Expand_Expr(gs_tree_operand(exp, 0), FALSE);
#endif
    WN *cleanup_block = WN_CreateBlock();
    WGEN_Stmt_Push(cleanup_block, wgen_stmk_temp_cleanup, Get_Srcpos());
#ifdef KEY
    if (!done_cleanups) // Bug 11350
#endif
      Do_Temp_Cleanups(exp);
    WGEN_Stmt_Pop(wgen_stmk_temp_cleanup);
    if (wn && WN_has_side_effects(wn) && WN_first(cleanup_block)) {
      DevWarn("CLEANUP_POINT_EXPR: expressson has side effects");
#ifdef KEY
      // We get here after fixing bug 10962. TREE_TYPE of this exp
      // may be void, so use the type from the WN created above.
      ty_idx = MTYPE_TO_TY_array[WN_rtype(wn)];
#else
      ty_idx = Get_TY(gs_tree_type(exp));
#endif
      st = Gen_Temp_Symbol(ty_idx, "__cleanup_point_expr");
#if 0 // wgen TODO
  	  WGEN_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, st);
#endif
      TYPE_ID mtype = TY_mtype(ty_idx);
      WGEN_Set_ST_Addr_Saved(wn);
      wn = WN_Stid(mtype, 0, st, ty_idx, wn);
      WGEN_Stmt_Append(wn, Get_Srcpos());
      wn = WN_Ldid(mtype, 0, st, ty_idx);
    }
    WGEN_Stmt_Append(cleanup_block, Get_Srcpos());
#ifdef KEY
    enclosing_cleanup_point_expr = old_enclosing_cleanup_point_expr;
#endif
  } break;

  case GS_THROW_EXPR:
    WGEN_One_Stmt(gs_tree_operand(exp, 0));
    break;

  case GS_TRY_CATCH_EXPR: // czw
#ifdef KEY
                          // The second operand of 'exp' should be run if the
                          // first throws an
    // exception.
    // wgen TODO: This cleanup should be treated as eh_only.
    if (lang_cplus) // czw	1.16 16:24
    {
      Register_Cleanup(exp);
      wn = WGEN_Expand_Expr(gs_tree_operand(exp, 0), need_result);
      Unregister_Cleanup();
    }
    if (lang_java) // czw	1.16 16:24
    {
      WGEN_Expand_Try_Catch(exp);
    }
#endif
    break;

  // binary
  case GS_PLUS_EXPR:
  case GS_MINUS_EXPR:
  case GS_MULT_EXPR:
  case GS_MAX_EXPR:
  case GS_MIN_EXPR:
  case GS_LSHIFT_EXPR:
  case GS_BIT_AND_EXPR:
  case GS_BIT_IOR_EXPR:
  case GS_BIT_XOR_EXPR:
  case GS_TRUNC_DIV_EXPR:
  case GS_TRUNC_MOD_EXPR:
  case GS_RDIV_EXPR:
  case GS_EXACT_DIV_EXPR:
  case GS_TRUTH_AND_EXPR:
  case GS_TRUTH_OR_EXPR:
  case GS_TRUTH_XOR_EXPR:
  case GS_COMPLEX_EXPR:
  case GS_CEIL_DIV_EXPR: {
    TYPE_ID etype = TY_mtype(Get_TY(gs_tree_type(exp)));
    wn0 = WGEN_Expand_Expr(gs_tree_operand(exp, 0));
    wn1 = WGEN_Expand_Expr(gs_tree_operand(exp, 1));
#ifdef KEY // Bug 11875
    if (code == GS_COMPLEX_EXPR && !MTYPE_float(WN_rtype(wn0))) {
      printf("%s does not support complex integer data types "
             "(a GNU extension)\n",
             lang_cplus ? "openCC" : "opencc");
      exit(2);
    }
#endif

    // generate whirl for add
    wn = WN_Binary(Operator_From_Tree[code].opr, Widen_Mtype(etype), wn0, wn1);

    // bug 2649, 5503 --- need conversion
    if ((MTYPE_is_integral(etype)) && (Widen_Mtype(etype) != etype) &&
        (TY_size(Get_TY(gs_tree_type(exp))) < 32) &&
        (code == GS_PLUS_EXPR || code == GS_MINUS_EXPR ||
         code == GS_MULT_EXPR || code == GS_LSHIFT_EXPR ||
         code == GS_BIT_XOR_EXPR || code == GS_BIT_IOR_EXPR))
      wn = WN_CreateCvtl(OPR_CVTL, Widen_Mtype(etype), MTYPE_V,
                         TY_size(Get_TY(gs_tree_type(exp))) * 8, wn);
  }
    // czw {
    if (key_exceptions && lang_java &&
        (code == GS_TRUNC_DIV_EXPR || code == GS_TRUNC_MOD_EXPR) &&
        !(in_cleanup)) {
      if (!inside_eh_region) { // check that we are not already in a region
        WN *region_body = WN_CreateBlock();
        inside_eh_region = true;
        WGEN_Stmt_Push(region_body, wgen_stmk_call_region_body, Get_Srcpos());
      }
    } // else if (key_exceptions && inside_eh_region && opt_regions)
    //{
    // The above conditions dictate that this call MUST not be inside
    // a region. So close the region.
    // TODO: Is this only for opt_regions or in general?
    // if (Check_For_Call_Region ())
    // Did_Not_Terminate_Region = FALSE;
    //}
    // czw }
    break;

  case GS_LROTATE_EXPR: {
    ty_idx = Get_TY(gs_tree_type(exp));
    TYPE_ID mtype = TY_mtype(ty_idx);
    wn0 = WGEN_Expand_Expr(gs_tree_operand(exp, 0));
    wn1 = WGEN_Expand_Expr(gs_tree_operand(exp, 1));
    wn1 = WN_Binary(OPR_SUB, Widen_Mtype(mtype),
                    WN_Intconst(Widen_Mtype(mtype), TY_size(ty_idx) * 8), wn1);
    wn = WN_Rrotate(TY_mtype(Get_TY(gs_tree_type(exp))), wn0, wn1);
  } break;

  case GS_RROTATE_EXPR: {
    wn0 = WGEN_Expand_Expr(gs_tree_operand(exp, 0));
    wn1 = WGEN_Expand_Expr(gs_tree_operand(exp, 1));
    wn = WN_Rrotate(TY_mtype(Get_TY(gs_tree_type(exp))), wn0, wn1);
  } break;

  case GS_RSHIFT_EXPR: {
    TYPE_ID mtyp = Widen_Mtype(TY_mtype(Get_TY(gs_tree_type(exp))));
    wn0 = WGEN_Expand_Expr(gs_tree_operand(exp, 0));
    wn1 = WGEN_Expand_Expr(gs_tree_operand(exp, 1));
    wn = WN_Binary(MTYPE_signed(mtyp) ? OPR_ASHR : OPR_LSHR, mtyp, wn0, wn1);
  } break;

  case GS_TRUTH_ANDIF_EXPR:
  case GS_TRUTH_ORIF_EXPR: {
    // bug 2651: evaluate the 1st operand unconditionally
    wn0 = WGEN_Expand_Expr(gs_tree_operand(exp, 0));

    // Evaluate the second condition.  Add guard variable to the cleanup if
    // there is cleanup.
    WGEN_Guard_Var_Push();
    wn1 = WGEN_Expand_Expr_With_Sequence_Point(gs_tree_operand(exp, 1),
                                               Boolean_type);
    gs_t guard_var = WGEN_Guard_Var_Pop();
    if (guard_var != NULL) {
      WGEN_add_guard_var(guard_var, wn1);
    }
    wn = WN_Binary(Operator_From_Tree[code].opr, Boolean_type, wn0, wn1);
    if (Boolean_type != MTYPE_B &&
        Widen_Mtype(TY_mtype(Get_TY(gs_tree_type(exp)))) != Boolean_type)
      wn = WN_Cvt(Boolean_type,
                  Widen_Mtype(TY_mtype(Get_TY(gs_tree_type(exp)))), wn);
  } break;

  case GS_LT_EXPR:
  case GS_LE_EXPR:
  case GS_GT_EXPR:
  case GS_GE_EXPR:
  case GS_EQ_EXPR:
  case GS_NE_EXPR: {
    wn0 = WGEN_Expand_Expr(gs_tree_operand(exp, 0));
    wn1 = WGEN_Expand_Expr(gs_tree_operand(exp, 1));
    // check if conversion is needed
    ty_idx = Get_TY(gs_tree_type(exp));
    TYPE_ID mtyp = TY_mtype(ty_idx);
    TY_IDX ty_idx0 = Get_TY(gs_tree_type(gs_tree_operand(exp, 0)));
    TYPE_ID mtyp0 = TY_mtype(ty_idx0);
    TY_IDX ty_idx1 = Get_TY(gs_tree_type(gs_tree_operand(exp, 1)));
    TYPE_ID mtyp1 = TY_mtype(ty_idx1);

    if (MTYPE_size_min(mtyp1) > MTYPE_size_min(mtyp0) &&
        !Has_Subsumed_Cvtl(WN_operator(wn0)))
      wn0 = WN_CreateCvtl(OPR_CVTL, Widen_Mtype(mtyp0), MTYPE_V,
                          MTYPE_size_min(mtyp0), wn0);
    if (MTYPE_size_min(mtyp0) > MTYPE_size_min(mtyp1) &&
        !Has_Subsumed_Cvtl(WN_operator(wn1)))
      wn1 = WN_CreateCvtl(OPR_CVTL, Widen_Mtype(mtyp1), MTYPE_V,
                          MTYPE_size_min(mtyp1), wn1);

#ifdef TARG_IA64
    wn = WN_Relational(Operator_From_Tree[code].opr, Widen_Mtype(mtyp0), wn0,
                       wn1);
#else
    wn = WN_CreateExp2(Operator_From_Tree[code].opr, Widen_Mtype(mtyp),
                       Widen_Mtype(mtyp0), wn0, wn1);
#endif
  } break;

  case GS_COND_EXPR: {
    TY_IDX ty_idx1, ty_idx2;
    if (gs_tree_operand(exp, 1) != NULL &&
        gs_tree_type(gs_tree_operand(exp, 1)) != NULL)
      ty_idx1 = Get_TY(gs_tree_type(gs_tree_operand(exp, 1)));
    else
      ty_idx1 = MTYPE_To_TY(MTYPE_V);
    if (gs_tree_operand(exp, 2) != NULL &&
        gs_tree_type(gs_tree_operand(exp, 2)) != NULL)
      ty_idx2 = Get_TY(gs_tree_type(gs_tree_operand(exp, 2)));
    else
      ty_idx2 = MTYPE_To_TY(MTYPE_V);
    if (gs_tree_type(exp) != NULL)
      ty_idx = Get_TY(gs_tree_type(exp));
    else
      ty_idx = MTYPE_To_TY(MTYPE_V);
#ifdef KEY // bug 2645
    wn0 = WGEN_Expand_Expr(gs_tree_operand(exp, 0));
#else
    wn0 = WGEN_Expand_Expr_With_Sequence_Point(gs_tree_operand(exp, 0),
                                               Boolean_type);
#endif
    if (TY_mtype(ty_idx) == MTYPE_V) {
      // If ty_idx is MTYPE_V, no return value is needed
      // We convert it into if...then...else
      WN *then_block = WN_CreateBlock();
      WN *else_block = WN_CreateBlock();
      WN *if_stmt = WN_CreateIf(wn0, then_block, else_block);
      WGEN_Stmt_Append(if_stmt, Get_Srcpos());
      WGEN_Stmt_Push(then_block, wgen_stmk_if_then, Get_Srcpos());
      wn1 = WGEN_Expand_Expr(gs_tree_operand(exp, 1), FALSE);
      if (wn1) {
        wn1 = WN_CreateEval(wn1);
        WGEN_Stmt_Append(wn1, Get_Srcpos());
      }
      WGEN_Stmt_Pop(wgen_stmk_if_then);
      if (gs_tree_operand(exp, 2) != NULL) {
        WGEN_Stmt_Push(else_block, wgen_stmk_if_else, Get_Srcpos());
        wn2 = WGEN_Expand_Expr(gs_tree_operand(exp, 2), FALSE);
        if (wn2) {
          wn2 = WN_CreateEval(wn2);
          WGEN_Stmt_Append(wn2, Get_Srcpos());
        }
        WGEN_Stmt_Pop(wgen_stmk_if_else);
      }
    } else {
#ifdef KEY

      FmtAssert((TY_mtype(ty_idx1) != MTYPE_V || TY_mtype(ty_idx2) != MTYPE_V),
                ("GS_COND_EXPR: bad MTYPE for operand 0 and 1. Can not be "
                 "MTYPE_V both."));

      // Prepare a guard variable for each part of the conditional, in case
      // the conditional has a cleanup that is executed after the whole
      // conditional expression is evaluated.  The guard variable ensures
      // that a cleanup is executed only if its part of the conditional is
      // executed.

      WGEN_Guard_Var_Push();

      if (TY_mtype(ty_idx1) == MTYPE_V) {
        // The res of operand is MTYPE_V,
        // we generate a dummy return value 0.
        // usually, this return value should not be used.
        WN *comma_block = WN_CreateBlock();
        WGEN_Stmt_Push(comma_block, wgen_stmk_comma, Get_Srcpos());
        WN *null_wn = WGEN_Expand_Expr(gs_tree_operand(exp, 1), FALSE, 0, 0, 0,
                                       0, FALSE, FALSE, target_wn);
        Is_True((null_wn == NULL),
                ("GS_COND_EXPR: Should not return a WN for MTYPE_V."));
        WGEN_Stmt_Pop(wgen_stmk_comma);
        Is_True((TY_mtype(ty_idx) != MTYPE_BS && TY_mtype(ty_idx) != MTYPE_STR),
                ("GS_COND_EXPR: bad MTYPE for ty_idx"));

        // Create a temporary st for the dummy return value.
        TYPE_ID comma_ty = Widen_Mtype(TY_mtype(ty_idx));
        ST *dummy_st = Gen_Temp_Symbol(ty_idx, "_unused");
        // Set_ST_is_not_used(dummy_st);		//ykq
        WN *dummy_wn = WN_Ldid(comma_ty, 0, ST_st_idx(dummy_st), ty_idx, 0);
        wn1 =
            WN_CreateComma(OPR_COMMA, comma_ty, MTYPE_V, comma_block, dummy_wn);
      } else {
        wn1 = WGEN_Expand_Expr_With_Sequence_Point(gs_tree_operand(exp, 1),
                                                   TY_mtype(ty_idx), target_wn);
      }
      gs_t guard_var1 = WGEN_Guard_Var_Pop();
      WGEN_Guard_Var_Push();

      if (TY_mtype(ty_idx2) == MTYPE_V) {
        WN *comma_block = WN_CreateBlock();
        WGEN_Stmt_Push(comma_block, wgen_stmk_comma, Get_Srcpos());
        WN *null_wn = WGEN_Expand_Expr(gs_tree_operand(exp, 2), FALSE, 0, 0, 0,
                                       0, FALSE, FALSE, target_wn);
        Is_True((null_wn == NULL),
                ("GS_COND_EXPR: Should not return a WN for MTYPE_V."));
        WGEN_Stmt_Pop(wgen_stmk_comma);
        Is_True((TY_mtype(ty_idx) != MTYPE_BS && TY_mtype(ty_idx) != MTYPE_STR),
                ("GS_COND_EXPR: bad MTYPE for ty_idx"));

        TYPE_ID comma_ty = Widen_Mtype(TY_mtype(ty_idx));
        ST *dummy_st = Gen_Temp_Symbol(ty_idx, "_unused");
        // Set_ST_is_not_used(dummy_st);		//ykq
        WN *dummy_wn = WN_Ldid(comma_ty, 0, ST_st_idx(dummy_st), ty_idx, 0);
        wn2 =
            WN_CreateComma(OPR_COMMA, comma_ty, MTYPE_V, comma_block, dummy_wn);
      } else {
        wn2 = WGEN_Expand_Expr_With_Sequence_Point(gs_tree_operand(exp, 2),
                                                   TY_mtype(ty_idx), target_wn);
      }
      gs_t guard_var2 = WGEN_Guard_Var_Pop();

      // Add guard variables if they are needed.
      if (guard_var1 != NULL) {
        WGEN_add_guard_var(guard_var1, wn1);
      }
      if (guard_var2 != NULL) {
        WGEN_add_guard_var(guard_var2, wn2);
      }
#else
      wn1 = WGEN_Expand_Expr_With_Sequence_Point(gs_tree_operand(exp, 1),
                                                 TY_mtype(ty_idx));
      wn2 = WGEN_Expand_Expr_With_Sequence_Point(gs_tree_operand(exp, 2),
                                                 TY_mtype(ty_idx));
#endif
      wn = WN_CreateExp3(OPR_CSELECT, Mtype_comparison(TY_mtype(ty_idx)),
                         MTYPE_V, wn0, wn1, wn2);
      Set_PU_has_very_high_whirl(Get_Current_PU());
    }
  } break;

  case GS_INIT_EXPR:
#ifdef KEY
    // Put the result in the write target if there is a write target.
    if (target_wn != NULL &&
        ((tmp_code = gs_tree_code(gs_tree_operand(exp, 0))) == GS_VAR_DECL ||
         tmp_code == GS_RESULT_DECL || tmp_code == GS_PARM_DECL)) {
      if (WN_operator(target_wn) == OPR_LDA) {
        // target_wn is a LDA of a ST.  Give the result the same ST.
        set_DECL_ST(gs_tree_operand(exp, 0), WN_st(target_wn));
      } else if (WN_operator(target_wn) == OPR_LDID) {
        // target_wn is a LDID of a ST, where ST points to the target
        // location.  This only happens if ST is the fake first parm.  To
        // have the INIT_EXPR write to the target area, change node X into an
        // indirect ref of the ST, where X is the original target of the
        // init_expr (var_decl/result_decl/parm_decl).  This will make all
        // tree nodes that point to X now point to the indirect ref.
        gs_t opnd0 = gs_tree_operand(exp, 0);
        gs_t ptr_var = gs_build_decl(
            GS_VAR_DECL, gs_build_pointer_type(gs_tree_type(opnd0)));
        _gs_code(opnd0, GS_INDIRECT_REF);
        gs_set_tree_operand(opnd0, 0, ptr_var);
        set_DECL_ST(ptr_var, WN_st(target_wn));
      }
    }
      // fall through
#endif
  case GS_MODIFY_EXPR:
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
    // Bug 11253:
    // The situation is different if the target_expr's initializer is an
    // aggr_init_expr. In this case, it is OK to replace node 2 by node 1.
    // The reason is while processing the target_expr, we in turn replace
    // the first arg of aggr_init_expr (here, x) by the first (0th)
    // operand of the target_expr.
    //
    // -----------------------------------------------------------
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

    // -----------------------------------------------------------
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
    // WGEN_Expand_Expr.  (This case was discovered after the scheme to
    // modify the init tree (described above) was implemented.  It seems the
    // correct approach is to simply set target_wn and then call
    // WGEN_Expand_Expr to expand directly into target_wn, without modifying
    // the init tree.  Change to this scheme if more problems show up.)
    //
    // As below, only check for indirect_ref if the indirect_ref is created
    // by kg++fe to access through the fake arg0, in order to avoid
    // (indirect_ref (nop (var_decl))) which is generated by g++.
    {
      gs_t init_expr_opnd0 = gs_tree_operand(exp, 0);
      if (gs_tree_code(gs_tree_operand(exp, 1)) == GS_NOP_EXPR &&
          gs_tree_code(gs_tree_operand(gs_tree_operand(exp, 1), 0)) ==
              GS_TARGET_EXPR &&
          gs_tree_code(init_expr_opnd0) == GS_INDIRECT_REF &&
          gs_tree_code(gs_tree_operand(init_expr_opnd0, 0)) == GS_VAR_DECL) {
        gs_t t = gs_tree_operand(exp, 1);
        ST *st = Get_ST(gs_tree_operand(init_expr_opnd0, 0));
        WN *target_wn = WN_Ldid(Pointer_Mtype, 0, st, ST_type(st));
        wn = WGEN_Expand_Expr(t, TRUE, 0, 0, 0, 0, FALSE, FALSE, target_wn);
        break;
      }
    }
#endif
    if (lang_cplus) {
      gs_t t = NULL;
      if (gs_tree_code(gs_tree_operand(exp, 1)) == GS_TARGET_EXPR) {
        t = gs_tree_operand(exp, 1);
        gs_set_tree_operand(t, 2, NULL);
      }
#ifdef KEY
      // Handle case where initializer is (nop (target ...)).  Bug 7792.
      else if (gs_tree_code(gs_tree_operand(exp, 1)) == GS_NOP_EXPR &&
               gs_tree_code(gs_tree_operand(gs_tree_operand(exp, 1), 0)) ==
                   GS_TARGET_EXPR) {
        t = gs_tree_operand(gs_tree_operand(exp, 1), 0);
        gs_set_tree_operand(t, 2, NULL);
      }
#endif

      if (t != NULL) {
#ifdef KEY
        // Only check for indirect_ref if the indirect_ref is created by
        // kg++fe to access through the fake arg0, in order to avoid
        // (indirect_ref (nop (var_decl))) which is generated by g++.
        gs_t init_expr_opnd0 = gs_tree_operand(exp, 0);
        if (gs_tree_code(init_expr_opnd0) == GS_INDIRECT_REF &&
            gs_tree_code(gs_tree_operand(init_expr_opnd0, 0)) ==
                GS_RESULT_DECL) {
          gs_t target_expr_opnd0 = gs_tree_operand(t, 0);
          gs_t ptr_var =
              gs_build_decl(GS_RESULT_DECL,
                            gs_tree_type(gs_tree_operand(init_expr_opnd0, 0)));
          _gs_code(target_expr_opnd0, GS_INDIRECT_REF);
          gs_set_tree_operand(target_expr_opnd0, 0, ptr_var);
          set_DECL_ST(ptr_var, DECL_ST(gs_tree_operand(init_expr_opnd0, 0)));
          wn = WGEN_Expand_Expr(t);
          break;
        }
        gs_t initializer = NULL;
#endif
        tmp_code = gs_tree_code(gs_tree_operand(exp, 0));
        if (tmp_code == GS_VAR_DECL || tmp_code == GS_RESULT_DECL ||
            tmp_code == GS_PARM_DECL) {
#ifdef KEY
          ST *st = Get_ST(gs_tree_operand(exp, 0));
          set_DECL_ST(gs_tree_operand(t, 0), st);
#else
          gs_tree_operand(t, 0) = gs_tree_operand(exp, 0);
#endif
          wn = WGEN_Expand_Expr(t);
          break;
        }
#ifdef KEY
        // Bug 11253: We could not provide the same ST to opnd0 of this
        // INIT_EXPR/MODIFY_EXPR and opnd0 of the target_expr. So try
        // replacing opnd0 of the target_expr with opnd0 of this
        // INIT_EXPR/MODIFY_EXPR.
        else if (gs_tree_code(initializer = gs_tree_operand(t, 1)) ==
                     GS_AGGR_INIT_EXPR &&
                 gs_aggr_init_via_ctor_p(initializer)) {

          gs_set_tree_operand(t, 0, gs_tree_operand(exp, 0));
          wn = WGEN_Expand_Expr(t);
          break;
        }
#endif
        DevWarn("INIT_EXPR/MODIFY_EXPR kid1 is TARGET_EXPR, kid0 is %s\n",
                gs_code_name(gs_tree_code(gs_tree_operand(exp, 0))));
      }

#if 0 // wgen clean-up: we should not need these codes any more, these
         are required to process gimplified GNU tree. Also remove
         definition of WGEN_Process_Initialization.

        gs_t lhs = gs_tree_operand(exp,0);
        gs_t rhs = gs_tree_operand(exp,1);
        // wgen TODO: revisit this fix. During gimplification, GNU4 can
        // strip off a target_expr from a modify_expr (see
        // gimplify_modify_expr_rhs).
        // So make sure the target_wn to use for returning object in memory
        // is set, if appropriate.
        if (!target_wn && gs_tree_code(exp) == GS_MODIFY_EXPR &&
            gs_tree_code(rhs) == GS_CALL_EXPR)
        {
          TY_IDX ty = Get_TY (gs_tree_type(rhs));
          if (TY_return_in_mem(ty))
          {
            if (gs_tree_code(lhs) == GS_VAR_DECL)
            {
              ST * s = Get_ST(lhs);
              target_wn = WN_Lda (Pointer_Mtype, 0, s, 0);
              wn = WGEN_Expand_Expr (rhs, TRUE, 0, 0, 0, 0, FALSE,
                                     FALSE, target_wn);
              break;
            }
          }
        }
        // DECL_EXPR nodes can be generally gimplified into either
        //   1) modify_expr, removing the decl_initial of the lhs, or
        //   2) generate no TREE code, but keeping decl_initial intact.
        // (1) is handled here, (2) is handled in Create_ST_For_Tree.
        // Bugs 11057, 11058
        else if (gs_tree_code(exp) == GS_MODIFY_EXPR &&
                 gs_tree_code(lhs) == GS_VAR_DECL &&
                 gs_tree_code(rhs) == GS_STRING_CST)
        {
          WGEN_Process_Initialization (exp);
          break;
        }
#endif
    }
    // fall through

  case GS_PREDECREMENT_EXPR:
  case GS_PREINCREMENT_EXPR:
  case GS_POSTDECREMENT_EXPR:
  case GS_POSTINCREMENT_EXPR: {
    if (gs_tree_code(gs_tree_operand(exp, 1)) == GS_ERROR_MARK)
      break;
    WN *call_return_val = NULL;

    // If gs_tree_operand(exp, 1) is a CALL_EXPR that returns a
    // ptr-to-member-function, then call
    // WGEN_Expand_Ptr_To_Member_Func_Call_Expr to expand it.  Otherwise,
    // call WGEN_Expand_Expr to do regular expansion.  Bug 4737.
    gs_t exp_opnd1 = gs_tree_operand(exp, 1);
    if (WGEN_Call_Returns_Ptr_To_Member_Func(exp_opnd1)) {
      TYPE_ID desc = TY_mtype(Get_TY(gs_tree_type(exp_opnd1)));
      wn1 = WGEN_Expand_Ptr_To_Member_Func_Call_Expr(exp_opnd1, 0,
                                                     Widen_Mtype(desc), desc);
    } else {
      gs_t lhs = gs_tree_operand(exp, 0);
      if (gs_tree_code(lhs) == GS_INDIRECT_REF &&
          gs_tree_code(gs_tree_operand(lhs, 0)) == GS_CALL_EXPR) {
        // We have a function call in lhs, we need to promote it.
        // The expr is f()=...; we must promote f() at first.
        // GCC TREE is
        // MODIFY_EXPR
        //  +-0 INDIRECT_REF
        //  |        +-0 CALL_EXPR
        //  +-1 ...
        //  Without this workaround,
        //    operand 1 will be expand first, then the CALL_EXPR in operand 0
        //    if there are several assignment, for example, f()=g()=h(), it's
        //    wrong.
        //  So we expand the CALL_EXPR at first, save its return value in
        //  call_return_val,
        //    then, pass call_return_val to WGEN_Lhs_Of_Modify_Expr
        //
        WN *call = WGEN_Expand_Expr(gs_tree_operand(lhs, 0));
        ST *preg_st;
        PREG_NUM preg;
        TY_IDX call_ty_idx = Get_TY(gs_tree_type(gs_tree_operand(lhs, 0)));
        preg_st = MTYPE_To_PREG(Pointer_Mtype);
        preg = Create_Preg(Pointer_Mtype, NULL);
        WGEN_Set_ST_Addr_Saved(call);
        WN *stid = WN_Stid(Pointer_Mtype, preg, preg_st, call_ty_idx, call);
        WGEN_Stmt_Append(stid, Get_Srcpos());
        call_return_val = WN_Ldid(Pointer_Mtype, preg, preg_st, call_ty_idx);
      }

      wn1 = WGEN_Expand_Expr(gs_tree_operand(exp, 1)); // r.h.s.
    }

#ifdef KEY // wgen bugs 10849, 10893, 10908
    if (wn1 && WN_operator(wn1) == OPR_INTCONST &&
        TY_size(Get_TY(gs_tree_type(gs_tree_operand(exp, 1)))) == 0)
      break;
#endif
    wn = WGEN_Lhs_Of_Modify_Expr(code, gs_tree_operand(exp, 0), call_return_val,
                                 need_result, 0, 0, 0, FALSE, wn1, 0, FALSE,
                                 FALSE);
  }
  /*//czw {
  if (key_exceptions  &&
      !(in_cleanup))
  {
      if (!inside_eh_region)
      { // check that we are not already in a region
          WN * region_body = WN_CreateBlock();
          inside_eh_region = true;
          WGEN_Stmt_Push (region_body, wgen_stmk_call_region_body,
  Get_Srcpos());
      }
  } //else if (key_exceptions && inside_eh_region && opt_regions)
  //{
      // The above conditions dictate that this call MUST not be inside
      // a region. So close the region.
      // TODO: Is this only for opt_regions or in general?
      //if (Check_For_Call_Region ())
          //Did_Not_Terminate_Region = FALSE;
  //}
  //czw }*/
  break;

    // ternary ops

  case GS_BIT_FIELD_REF: {
    wn = WGEN_Expand_Expr(gs_tree_operand(exp, 0), TRUE, nop_ty_idx,
                          component_ty_idx, component_offset, field_id, FALSE);
    ty_idx = Get_TY(gs_tree_type(exp));
    TYPE_ID rtype = TY_mtype(ty_idx);
    UINT siz = TY_size(ty_idx);
    TYPE_ID desc;
    if (siz <= 8) {
      if (MTYPE_signed(rtype))
        desc = Mtype_AlignmentClass(siz, MTYPE_CLASS_INTEGER);
      else
        desc = Mtype_AlignmentClass(siz, MTYPE_CLASS_UNSIGNED_INTEGER);
      rtype = Widen_Mtype(desc);
    } else
      desc = rtype;
#ifdef KEY
    // bug 3074
    while (1) {
      if ((WN_operator(wn) == OPR_CVT) &&
          (desc == rtype)) { // We do not need the CVT
        WN *del = wn;
        wn = WN_kid0(wn);
        WN_Delete(del);
      } else
        break;
    }
#endif // KEY
    WN_set_rtype(wn, rtype);
    if (WN_desc(wn) != MTYPE_V)
      WN_set_desc(wn, desc);
    INT bofst = gs_get_integer_value(gs_tree_operand(exp, 2));
    INT bsiz = gs_get_integer_value(gs_tree_operand(exp, 1));
    if ((bsiz & 7) == 0 &&                  // field size multiple of bytes
        MTYPE_size_min(desc) % bsiz == 0 && // accessed loc multiple of bsiz
        bofst % bsiz == 0) {                // bofst multiple of bsiz
      // not really a bit-field extraction!
      if (WN_desc(wn) != MTYPE_V)
        if (MTYPE_signed(rtype))
          WN_set_desc(wn, Mtype_AlignmentClass(bsiz >> 3, MTYPE_CLASS_INTEGER));
        else
          WN_set_desc(wn, Mtype_AlignmentClass(bsiz >> 3,
                                               MTYPE_CLASS_UNSIGNED_INTEGER));
      WN_load_offset(wn) = WN_load_offset(wn) + (bofst >> 3);
    } else {
#ifdef KEY
      // bofst is ofst in bits from the base of the object.
      // Convert it to ofst from the beginning of the field, and update
      // the load offset using the proper alignment
      // The change is needed when we come here with bofst > base_type_size
      mUINT16 base_type_size = MTYPE_bit_size(desc);
      WN_load_offset(wn) += (bofst / base_type_size) * MTYPE_byte_size(desc);
      bofst = bofst % base_type_size;
#endif
      if (WN_operator(wn) == OPR_LDID)
        WN_set_operator(wn, OPR_LDBITS);
      else
        WN_set_operator(wn, OPR_ILDBITS);
      WN_set_bit_offset_size(wn, bofst, bsiz);
#ifdef KEY
      WN_set_ty(wn, MTYPE_To_TY(WN_desc(wn)));
      break;
#endif
    }
    if (MTYPE_byte_size(WN_desc(wn)) != TY_size(WN_ty(wn)))
    // the container is smaller than the entire struct
#ifdef KEY
    {
      TY_IDX ty = MTYPE_To_TY(WN_desc(wn));
      if ((TY_kind(Ty_Table[WN_ty(wn)]) == KIND_STRUCT) &&
          (TY_kind(Ty_Table[ty]) != KIND_STRUCT))
        // if struct is being changed to a non-struct, the field-id
        // does not hold any more.
        WN_set_field_id(wn, 0);
      WN_set_ty(wn, ty);
    }
#else
      WN_set_ty(wn, MTYPE_To_TY(WN_desc(wn)));
#endif
  } break;

    // n-ary ops

  case GS_ARRAY_REF: {
    UINT xtra_BE_ofst = 0; // only needed for big-endian target
    TY_IDX elem_ty_idx;
    // generate the WHIRL array node
    wn0 = WGEN_Array_Expr(exp, &elem_ty_idx, 0, 0, 0);

    // generate the iload node
    TY_IDX hi_ty_idx = Get_TY(gs_tree_type(exp));
#if 1 // wgen bug 10448
    if (gs_tree_this_volatile(exp))
      Set_TY_is_volatile(hi_ty_idx);
    else
      Clear_TY_is_volatile(hi_ty_idx);
#endif
    desc_ty_idx = component_ty_idx;
    if (desc_ty_idx == 0)
      desc_ty_idx = hi_ty_idx;

    if (!MTYPE_is_integral(TY_mtype(desc_ty_idx)))
      ty_idx = desc_ty_idx;
    else {
      ty_idx = nop_ty_idx;
      if (ty_idx == 0)
        ty_idx = desc_ty_idx;
    }

    if (!is_bit_field) {
      if (TY_size(desc_ty_idx) > TY_size(ty_idx)) {
        if (Target_Byte_Sex == BIG_ENDIAN)
          xtra_BE_ofst = TY_size(desc_ty_idx) - TY_size(ty_idx);
        desc_ty_idx = ty_idx;
      }
    } else {
      if (TY_size(desc_ty_idx) > TY_size(ty_idx))
        ty_idx = desc_ty_idx;
    }

    TYPE_ID rtype = Widen_Mtype(TY_mtype(ty_idx));
    TYPE_ID desc = TY_mtype(desc_ty_idx);
    if (MTYPE_is_integral(desc)) {
      if (MTYPE_signed(rtype) != MTYPE_signed(desc)) {
        if (MTYPE_size_min(rtype) > MTYPE_size_min(desc) || is_bit_field)
          rtype = Mtype_TransferSign(desc, rtype);
        else
          desc = Mtype_TransferSign(rtype, desc);
      }
    }

    Is_True(!is_bit_field || field_id <= MAX_FIELD_ID,
            ("WGEN_Expand_Expr: field id for bit-field exceeds limit"));
    wn = WN_CreateIload(OPR_ILOAD, rtype, is_bit_field ? MTYPE_BS : desc,
                        component_offset + xtra_BE_ofst,
                        field_id != 0 ? hi_ty_idx : ty_idx,
                        Make_Pointer_Type(elem_ty_idx, FALSE), wn0, field_id);
  }
    // czw {
    if (key_exceptions && lang_java && !(in_cleanup)) {
      if (!inside_eh_region) { // check that we are not already in a region
        WN *region_body = WN_CreateBlock();
        inside_eh_region = true;
        WGEN_Stmt_Push(region_body, wgen_stmk_call_region_body, Get_Srcpos());
      }
    } // else if (key_exceptions && inside_eh_region && opt_regions)
    //{
    // The above conditions dictate that this call MUST not be inside
    // a region. So close the region.
    // TODO: Is this only for opt_regions or in general?
    // if (Check_For_Call_Region ())
    // Did_Not_Terminate_Region = FALSE;
    //}
    // czw }
    break;

  case GS_AGGR_INIT_EXPR:
  case GS_CALL_EXPR: {
    gs_t arglist = gs_tree_operand(exp, 1);
    TYPE_ID ret_mtype;
    WN *call_wn;
    WN *arg_wn;
    TY_IDX arg_ty_idx;
    TYPE_ID arg_mtype;
    INT num_args = 0;
    INT num_handlers = 0;
    INT i;
    gs_t list;

    arg0 = gs_tree_operand(exp, 0);
    gs_code_t code0 = gs_tree_code(arg0);
    // KEY:  true if type must be returned in mem
    BOOL return_in_mem = FALSE;
#ifdef KEY
    ST *ret_st = NULL; // return symbol
    if (gs_tree_code(exp) == GS_AGGR_INIT_EXPR &&
        gs_aggr_init_via_ctor_p(exp) && !is_aggr_init_via_ctor) {
      Is_True(gs_tree_operand(exp, 2),
              ("WGEN_Expand_Expr: null slot for AGGR_INIT_EXPR"));
      // bug 11188: We have not yet set up the first argument.
      gs_set_tree_value(arglist, gs_tree_operand(exp, 2));
      // make sure we take the address of this arg.
      is_aggr_init_via_ctor = true;
    }
    processing_function_prototype = TRUE; /* bug 8346 */
#endif
    for (list = gs_tree_operand(exp, 1); list; list = gs_tree_chain(list)) {
      arg_ty_idx = Get_TY(gs_tree_type(gs_tree_value(list)));
      if (!WGEN_Keep_Zero_Length_Structs && TY_mtype(arg_ty_idx) == MTYPE_M &&
          TY_size(arg_ty_idx) == 0) {
        // zero length struct parameter
      } else
        num_args++;
    }
#ifdef KEY
    processing_function_prototype = FALSE; /* bug 8346 */
    if (gs_tree_code(exp) == GS_AGGR_INIT_EXPR) {
      // bug 11159: TREE_TYPE does not contain the return type.
      ty_idx = Get_TY(gs_tree_type(gs_tree_type(gs_tree_type(arg0))));
    } else
#endif
      ty_idx = Get_TY(gs_tree_type(exp));
#if 1 // wgen bug 10448
    if (gs_tree_this_volatile(exp))
      Set_TY_is_volatile(ty_idx);
    else
      Clear_TY_is_volatile(ty_idx);
#endif
    if (need_result) {
      if (!WGEN_Keep_Zero_Length_Structs && TY_mtype(ty_idx) == MTYPE_M &&
          TY_size(ty_idx) == 0) {
        // zero length struct return
        ret_mtype = MTYPE_V;
      } else
        ret_mtype = TY_mtype(ty_idx);
#ifdef KEY
      // If the type must be returned in memory, create a symbol and pass
      // its address as the first param.
      if (TY_return_in_mem(ty_idx)) {
        ret_mtype = MTYPE_V;
        return_in_mem = TRUE;
        num_args++; // first param is address of return symbol
        if (gs_tree_code(exp) == GS_AGGR_INIT_EXPR && !target_wn) {
          // bug 11169: Use the slot to set up the target.
          gs_t slot = gs_tree_operand(exp, 2);
          FmtAssert(gs_tree_code(slot) == GS_VAR_DECL,
                    ("WGEN_Expand_Expr: Expected VAR_DECL for "
                     "AGGR_INIT_EXPR slot"));
          ST *target_st = Get_ST(slot);
          target_wn = WN_Lda(Pointer_Mtype, 0, target_st, 0);
        }
      }
#endif
    } else
      ret_mtype = MTYPE_V;
    st = NULL;
    if (code0 == GS_ADDR_EXPR && gs_tree_code(gs_tree_operand(arg0, 0))) {
      gs_t func = gs_tree_operand(arg0, 0);
      BOOL intrinsic_op = FALSE;
      BOOL whirl_generated = FALSE;
      INTRINSIC iopc = INTRINSIC_NONE;

#ifdef KEY
      // bug 8251: If we forcibly change the return type, we should
      // generate a CVT.
      TYPE_ID cvt_to = MTYPE_UNKNOWN;

      if (gs_decl_assembler_name(func))
        REFERENCED_BY_WHIRL(gs_decl_assembler_name(func)) = 1;
#endif

      if (gs_decl_built_in(func)) {
        if (gs_decl_built_in_class(func) != GSBI_CLASS_BUILT_IN_MD) {

          switch (gs_decl_function_code(func)) {

          case GSBI_END_BUILTINS:
            break;

          case GSBI_BUILT_IN_STDARG_START:
#ifdef KEY
          case GSBI_BUILT_IN_VA_START:
#endif
          {
#ifdef TARG_X8664
            if (TARGET_64BIT) {
              iopc = INTRN_VA_START;
              break;
            }
#endif
            if (arglist == NULL || gs_tree_value(arglist) == NULL ||
                gs_tree_chain(arglist) == NULL ||
                gs_tree_value(gs_tree_chain(arglist)) == NULL) {
              printf("error: too few arguments to function 'va_start'\n");
              Terminate(2);
            }
            arg1 = gs_tree_value(arglist);
            arg2 = gs_tree_value(gs_tree_chain(arglist));
            WN *arg_wn = WGEN_Expand_Expr(arg1);
            TY_IDX arg_ty_idx = Get_TY(gs_tree_type(arg1));
            while (gs_tree_code(arg2) == GS_NOP_EXPR ||
                   gs_tree_code(arg2) == GS_CONVERT_EXPR ||
                   gs_tree_code(arg2) == GS_NON_LVALUE_EXPR ||
                   gs_tree_code(arg2) == GS_INDIRECT_REF)
              arg2 = gs_tree_operand(arg2, 0);
            ST *st2 = Get_ST(arg2);
#ifdef TARG_X8664
            const int align = TARGET_64BIT ? 8 : 4;
            wn = WN_Lda(Pointer_Mtype,
                        ((TY_size(ST_type(st2)) + align - 1) & (-align)), st2);
#else
            wn = WN_Lda(Pointer_Mtype, ((TY_size(ST_type(st2)) + 7) & (-8)),
                        st2);
#endif
            if (WN_operator(arg_wn) == OPR_LDA) {
              wn = WN_Stid(Pointer_Mtype, WN_offset(arg_wn), WN_st(arg_wn),
                           arg_ty_idx, wn);
            } else {
              wn = WN_CreateIstore(OPR_ISTORE, MTYPE_V, Pointer_Mtype, 0,
                                   arg_ty_idx, wn, arg_wn, 0);
            }

            WGEN_Stmt_Append(wn, Get_Srcpos());
            whirl_generated = TRUE;
            wn = NULL;
            break;
          }

          case GSBI_BUILT_IN_VA_COPY: {
            arg1 = gs_tree_value(arglist);
            arg2 = gs_tree_value(gs_tree_chain(arglist));
            TY_IDX arg_ty_idx = Get_TY(gs_tree_type(arg1));

#ifdef TARG_X8664
            /* Under -m32, convert a __builtin_va_copy to an assignment if the
               type of va_list is not array.
               Also, the original code seems to only work for -m64, like other
               va_XYZ code; under -m32, the source address is wrong.  (bug#2601)
               (But even under -m64, the using of memcpy is unnecessary.)
             */
            if (!TARGET_64BIT) {
              FmtAssert(gs_tree_code(arglist) != GS_ARRAY_TYPE,
                        ("unexpected array type for intrinsic 'va_copy'"));
              WN *addr = WGEN_Expand_Expr(arg1);
              WN *value = WGEN_Expand_Expr(arg2);
              wn = WN_CreateIstore(OPR_ISTORE, MTYPE_V, Pointer_Mtype, 0,
                                   arg_ty_idx, value, addr, 0);

              WGEN_Stmt_Append(wn, Get_Srcpos());
              whirl_generated = TRUE;
              wn = NULL;
              break;
            }
#endif // TARG_X8664

#ifdef TARG_IA64
            // For IA64, the memcpy is not necessary
            // We use IStore directly
            FmtAssert(gs_tree_code(arglist) != GS_ARRAY_TYPE,
                      ("unexpected array type for intrinsic 'va_copy'"));
            WN *addr = WGEN_Expand_Expr(arg1);
            WN *value = WGEN_Expand_Expr(arg2);
            wn = WN_CreateIstore(OPR_ISTORE, MTYPE_V, Pointer_Mtype, 0,
                                 arg_ty_idx, value, addr, 0);

            WGEN_Stmt_Append(wn, Get_Srcpos());
            whirl_generated = TRUE;
            wn = NULL;
            break;
#endif
            // These code are for X8664, 64bit
            WN *dst = WN_CreateParm(Pointer_Mtype, WGEN_Expand_Expr(arg1),
                                    arg_ty_idx, WN_PARM_BY_VALUE);
            WN *src = WN_CreateParm(Pointer_Mtype, WGEN_Expand_Expr(arg2),
                                    arg_ty_idx, WN_PARM_BY_VALUE);
            WN *size = WN_CreateParm(
                MTYPE_I4,
                WN_Intconst(MTYPE_I4, TY_size(TY_pointed(arg_ty_idx))),
                Be_Type_Tbl(MTYPE_I4), WN_PARM_BY_VALUE);
            wn = WN_Create(OPR_INTRINSIC_CALL, ret_mtype, MTYPE_V, 3);
            WN_intrinsic(wn) = INTRN_MEMCPY;
            WN_kid0(wn) = dst;
            WN_kid1(wn) = src;
            WN_kid2(wn) = size;
            WGEN_Stmt_Append(wn, Get_Srcpos());
            whirl_generated = TRUE;
            wn = NULL;
            break;
          }

          case GSBI_BUILT_IN_VA_END: {
            arg1 = gs_tree_value(arglist);
            wn = WN_CreateEval(WGEN_Expand_Expr(arg1));
            WGEN_Stmt_Append(wn, Get_Srcpos());
            whirl_generated = TRUE;
            wn = NULL;
            break;
          }

          case GSBI_BUILT_IN_NEXT_ARG: {
            gs_t last_parm =
                gs_tree_last(gs_decl_arguments(Current_Function_Decl()));
            gs_code_t last_parm_code = gs_tree_code(last_parm);
            while (last_parm_code == GS_NOP_EXPR ||
                   last_parm_code == GS_CONVERT_EXPR ||
                   last_parm_code == GS_NON_LVALUE_EXPR ||
                   last_parm_code == GS_INDIRECT_REF)
              last_parm = gs_tree_operand(last_parm, 0);
            ST *st = Get_ST(last_parm);
            arg_wn = WN_Lda(Pointer_Mtype, ST_ofst(st), st);
            wn = WN_Binary(
                OPR_ADD, Pointer_Mtype, arg_wn,
                WN_Intconst(Pointer_Mtype, Parameter_Size(ST_size(st))));
            whirl_generated = TRUE;
            break;
          }

          case GSBI_BUILT_IN_ALLOCA:
            Set_PU_has_alloca(Get_Current_PU());
            Set_PU_has_user_alloca(Get_Current_PU());
            arg_wn = WGEN_Expand_Expr(gs_tree_value(gs_tree_operand(exp, 1)));
            wn = WN_CreateAlloca(arg_wn);
            whirl_generated = TRUE;
            break;

          case GSBI_BUILT_IN_MEMCPY:
            iopc = INTRN_MEMCPY;
            break;

          case GSBI_BUILT_IN_MEMCMP:
            iopc = INTRN_MEMCMP;
            break;

          case GSBI_BUILT_IN_MEMSET:
            iopc = INTRN_MEMSET;
            break;

          case GSBI_BUILT_IN_STRCPY:
            iopc = INTRN_STRCPY;
            break;

          case GSBI_BUILT_IN_STRCMP:
#ifdef GPLUSPLUS_FE
            iopc = INTRN_STRCMP;
#else
            if (arglist == 0
                /* Arg could be non-pointer if user redeclared this fcn wrong.
                 */
                || gs_tree_code(gs_tree_type(gs_tree_value(arglist))) !=
                       GS_POINTER_TYPE ||
                gs_tree_chain(arglist) == 0 ||
                gs_tree_code(gs_tree_type(
                    gs_tree_value(gs_tree_chain(arglist)))) != GS_POINTER_TYPE)
              break;
            else {
              arg1 = gs_tree_value(arglist);
              arg2 = gs_tree_value(gs_tree_chain(arglist));
              gs_t len1 = c_strlen(arg1);
              if (len1) {
                gs_t len2 = c_strlen(arg2);
                if (len2) {
                  char *ptr1 = get_string_pointer(WGEN_Expand_Expr(arg1));
                  char *ptr2 = get_string_pointer(WGEN_Expand_Expr(arg2));
                  if (ptr1 && ptr2) {
                    wn = WN_Intconst(MTYPE_I4, strcmp(ptr1, ptr2));
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

          case GSBI_BUILT_IN_STRLEN:
#ifdef GPLUSPLUS_FE
            iopc = INTRN_STRLEN;
#else
            if (arglist == 0
                /* Arg could be non-pointer if user redeclared this fcn wrong.
                 */
                || gs_tree_code(gs_tree_type(gs_tree_value(arglist))) !=
                       GS_POINTER_TYPE)
              break;
            else {
              gs_t src = gs_tree_value(arglist);
              gs_t len = c_strlen(src);
              if (len) {
                wn = WGEN_Expand_Expr(len);
                whirl_generated = TRUE;
              } else {
                iopc = INTRN_STRLEN;
                //		    intrinsic_op = TRUE;
              }
            }
#endif /* GPLUSPLUS_FE */
            break;

#ifdef KEY
          case GSBI_BUILT_IN_FLOOR:
            arg_wn = WGEN_Expand_Expr(gs_tree_value(gs_tree_operand(exp, 1)));
#ifdef TARG_IA64
            if (MTYPE_is_integral(ret_mtype)) {
              wn = WN_CreateExp1(OPR_FLOOR, ret_mtype, MTYPE_F8, arg_wn);
            } else {
              wn0 = WN_CreateExp1(OPR_FLOOR, MTYPE_I8, MTYPE_F8, arg_wn);
              wn = WN_Cvt(WN_rtype(wn0), ret_mtype, wn0);
            }
#else
            wn = WN_CreateExp1(OPR_FLOOR, ret_mtype, MTYPE_F8, arg_wn);
#endif
            whirl_generated = TRUE;
            break;

          case GSBI_BUILT_IN_FLOORF:
            arg_wn = WGEN_Expand_Expr(gs_tree_value(gs_tree_operand(exp, 1)));
#ifdef TARG_IA64
            if (MTYPE_is_integral(ret_mtype)) {
              wn = WN_CreateExp1(OPR_FLOOR, ret_mtype, MTYPE_F4, arg_wn);
            } else {
              wn0 = WN_CreateExp1(OPR_FLOOR, MTYPE_I8, MTYPE_F4, arg_wn);
              wn = WN_Cvt(WN_rtype(wn0), ret_mtype, wn0);
            }
#else
            wn = WN_CreateExp1(OPR_FLOOR, ret_mtype, MTYPE_F4, arg_wn);
#endif
            whirl_generated = TRUE;
            break;

          case GSBI_BUILT_IN_FLOORL:
            arg_wn = WGEN_Expand_Expr(gs_tree_value(gs_tree_operand(exp, 1)));
#ifdef TARG_IA64
            if (MTYPE_is_integral(ret_mtype)) {
              wn = WN_CreateExp1(OPR_FLOOR, ret_mtype, MTYPE_F10, arg_wn);
            } else {
              wn0 = WN_CreateExp1(OPR_FLOOR, MTYPE_I8, MTYPE_F10, arg_wn);
              wn = WN_Cvt(WN_rtype(wn0), ret_mtype, wn0);
            }
#else
            wn = WN_CreateExp1(OPR_FLOOR, ret_mtype, MTYPE_FQ, arg_wn);
#endif
            whirl_generated = TRUE;
            break;
#endif

#ifdef KEY
          case GSBI_BUILT_IN_SQRT:
            if (gs_flag_errno_math(program)) {
              break;
            }
#else
          case GSBI_BUILT_IN_FSQRT:
#endif
            arg_wn = WGEN_Expand_Expr(gs_tree_value(gs_tree_operand(exp, 1)));
            wn = WN_CreateExp1(OPR_SQRT, ret_mtype, MTYPE_V, arg_wn);
            whirl_generated = TRUE;
            break;

          case GSBI_BUILT_IN_SIN:
            intrinsic_op = TRUE;
#ifdef TARG_X8664
            if (!Force_IEEE_Comparisons) {
              iopc = INTRN_SINL;
              if (ret_mtype != MTYPE_FQ) {
                // generate a cvt to 'cvt_to'
                cvt_to = ret_mtype;
                ret_mtype = MTYPE_FQ;
              }
              break;
            }
#endif
#ifdef KEY // bug 11305
            if (ret_mtype == MTYPE_V)
              ret_mtype = MTYPE_F8;
#endif
            if (ret_mtype == MTYPE_F4)
              iopc = INTRN_F4SIN;
            else if (ret_mtype == MTYPE_F8)
              iopc = INTRN_F8SIN;
            else
              Fail_FmtAssertion("unexpected mtype for intrinsic 'sin'");
            break;

          case GSBI_BUILT_IN_COS:
            intrinsic_op = TRUE;
#ifdef TARG_X8664
            if (!Force_IEEE_Comparisons) {
              iopc = INTRN_COSL;
              if (ret_mtype != MTYPE_FQ) {
                // generate a cvt to 'cvt_to'
                cvt_to = ret_mtype;
                ret_mtype = MTYPE_FQ;
              }
              break;
            }
#endif
#ifdef KEY // bug 11305
            if (ret_mtype == MTYPE_V)
              ret_mtype = MTYPE_F8;
#endif
            if (ret_mtype == MTYPE_F4)
              iopc = INTRN_F4COS;
            else if (ret_mtype == MTYPE_F8)
              iopc = INTRN_F8COS;
            else
              Fail_FmtAssertion("unexpected mtype for intrinsic 'cos'");
            break;

#ifdef KEY
          case GSBI_BUILT_IN_EXP:
            // bug 3390
            // If return type is void, generate an intrinsic assuming
            // double (so if it is without side-effects, optimizer can
            // remove it)
            if (ret_mtype == MTYPE_V)
              ret_mtype = MTYPE_F8;

            if (ret_mtype == MTYPE_F4)
              iopc = INTRN_F4EXP;
            else if (ret_mtype == MTYPE_F8)
              iopc = INTRN_F8EXP;
            else
              Fail_FmtAssertion("unexpected mtype for intrinsic 'exp'");
            intrinsic_op = TRUE;
            break;

          case GSBI_BUILT_IN_POW:
            // Bug 8195: If for whatever reason the pow(3) call is unused,
            // need_result will be false. Then, the value that this very
            // function assigns to ret_mtype for pow(3) is MTYPE_V. So,
            // just like we handle BUILT_IN_EXP above, we need to reassign
            // ret_mtype to MTYPE_F8.

            // Note that since pow[lf](3) are not builtin's (unlike the way
            // exp[lf]?(3)'s are), we only permit ret_mtype MTYPE_F8 here.

            if (ret_mtype == MTYPE_V)
              ret_mtype = MTYPE_F8;

            FmtAssert(ret_mtype == MTYPE_F8,
                      ("unexpected mtype for intrinsic 'pow'"));
            iopc = INTRN_F8EXPEXPR;
            intrinsic_op = TRUE;
            break;

          case GSBI_BUILT_IN_POWI: // bug 10963
            if (ret_mtype == MTYPE_V)
              ret_mtype = MTYPE_F8;

            FmtAssert(ret_mtype == MTYPE_F8,
                      ("unexpected mtype for intrinsic 'powi'"));
            iopc = INTRN_F8F8I4EXPEXPR;
            intrinsic_op = TRUE;
            break;

          case GSBI_BUILT_IN_POWIF: // bug 11246
            if (ret_mtype == MTYPE_V)
              ret_mtype = MTYPE_F4;

            FmtAssert(ret_mtype == MTYPE_F4,
                      ("unexpected mtype for intrinsic 'powif'"));
            intrinsic_op = TRUE;
            iopc = INTRN_F4F4I4EXPEXPR;
            break;

          case GSBI_BUILT_IN_POWIL: // bug 11246
#ifdef TARG_IA64
                                    // on IA64, we use MTYPE_F10 for long double
            if (ret_mtype == MTYPE_V)
              ret_mtype = MTYPE_F10;
            FmtAssert(ret_mtype == MTYPE_F10,
                      ("unexpected mtype for intrinsic 'powil'"));
#else
            if (ret_mtype == MTYPE_V)
              ret_mtype = MTYPE_FQ;

            FmtAssert(ret_mtype == MTYPE_FQ,
                      ("unexpected mtype for intrinsic 'powil'"));
#endif
            iopc = INTRN_FQFQI4EXPEXPR;
            intrinsic_op = TRUE;
            break;
#endif // KEY

          case GSBI_BUILT_IN_CONSTANT_P: {
            gs_t arg = gs_tree_value(gs_tree_operand(exp, 1));
            arg = gs_strip_nops(arg);
            if (gs_really_constant_p(arg) ||
                (gs_tree_code(arg) == GS_ADDR_EXPR &&
                 gs_tree_code(gs_tree_operand(arg, 0)) == GS_STRING_CST)) {
              wn = WN_Intconst(MTYPE_I4, 1);
              whirl_generated = TRUE; // KEY
            }
#ifdef KEY_bug1058
            // If not yet compile-time constant, let the backend decide if it is
            // a constant
            else {
              iopc = INTRN_CONSTANT_P;
              intrinsic_op = TRUE;
            }
#else

            else
              wn = WN_Intconst(MTYPE_I4, 0);
            //                wn = WGEN_Expand_Expr (gs_tree_value
            //                (gs_tree_operand (exp, 1)));
            whirl_generated = TRUE;
#endif // KEY
            break;
          }
#if 0
              case BUILT_IN_LOCK_TEST_AND_SET:
                wn = emit_builtin_lock_test_and_set (exp, num_args-2);
                whirl_generated = TRUE;
                break;

              case BUILT_IN_LOCK_RELEASE:
                emit_builtin_lock_release (exp, num_args-1);
                whirl_generated = TRUE;
                break;

              case BUILT_IN_COMPARE_AND_SWAP:
                wn = emit_builtin_compare_and_swap (exp, num_args-3);
                whirl_generated = TRUE;
                break;

              case BUILT_IN_SYNCHRONIZE:
                emit_builtin_synchronize (exp, num_args);
                whirl_generated = TRUE;
                break;
#endif

          case GSBI_BUILT_IN_RETURN_ADDRESS:
            i = gs_get_integer_value(gs_tree_value(gs_tree_operand(exp, 1)));
            if (i > 0) {
              // currently don't handle levels > 0,
              // which requires iterating thru call-stack
              // and finding $ra in fixed place.
              Warning(
                  "non-zero levels not supported for builtin_return_address");
              wn = WN_Intconst(Pointer_Mtype, 0);
            } else {
              st = WGEN_Get_Return_Address_ST(i);
              wn = WN_Ldid(Pointer_Mtype, 0, st, ST_type(st));
            }
            whirl_generated = TRUE;
            break;

#ifdef KEY
          case GSBI_BUILT_IN_EXTRACT_RETURN_ADDR:
            list = gs_tree_operand(exp, 1);
            wn = WGEN_Expand_Expr(gs_tree_value(list));
            wn = WN_Binary(OPR_BAND, Pointer_Mtype, wn,
                           WN_Intconst(Pointer_Mtype, -2));
            whirl_generated = TRUE;
            break;

          case GSBI_BUILT_IN_FRAME_ADDRESS:
            Set_PU_has_alloca(Get_Current_PU());
            iopc = MTYPE_byte_size(Pointer_Mtype) == 4
                       ? INTRN_U4READFRAMEPOINTER
                       : INTRN_U4READFRAMEPOINTER;
            intrinsic_op = TRUE;
            break;
          case GSBI_BUILT_IN_APPLY_ARGS:
            Set_PU_has_alloca(Get_Current_PU());
            iopc = INTRN_APPLY_ARGS;
            break;
          case GSBI_BUILT_IN_APPLY: {
            WN *load_wn, *sp_addr;

            Set_PU_has_alloca(Get_Current_PU());

            iopc = INTRN_APPLY;
            call_wn =
                WN_Create(OPR_INTRINSIC_CALL, ret_mtype, MTYPE_V, num_args);
            WN_intrinsic(call_wn) = iopc;
            WN_Set_Linenum(call_wn, Get_Srcpos());
            WN_Set_Call_Default_Flags(call_wn);
            i = 0;
            BOOL generate_mload = FALSE;
            WN *kid1 = NULL;
            WN *kid2 = NULL;
            for (list = gs_tree_operand(exp, 1); list;
                 list = gs_tree_chain(list)) {
              arg_wn = WGEN_Expand_Expr(gs_tree_value(list));
              if (i == 1)
                kid1 = arg_wn;
              if (i == 2 && WN_operator(arg_wn) != OPR_INTCONST) {
                generate_mload = TRUE;
                kid2 = arg_wn;
              } else if (i == 2)
                kid2 = arg_wn;
              arg_ty_idx = Get_TY(gs_tree_type(gs_tree_value(list)));
              arg_mtype = TY_mtype(arg_ty_idx);
              arg_wn = WN_CreateParm(Mtype_comparison(arg_mtype), arg_wn,
                                     arg_ty_idx, WN_PARM_BY_VALUE);
              WN_kid(call_wn, i++) = arg_wn;
            }

            // Store SP & Alloca
            TY_IDX ty_idx = Make_Pointer_Type(Be_Type_Tbl(MTYPE_V), FALSE);
            ST *alloca_st_0 =
                Gen_Temp_Symbol(ty_idx, "__builtin_apply_alloca0");
#if 0 // wgen TODO
		  WGEN_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL,
						       alloca_st_0);
#endif
            WN *alloca_0 =
                WN_CreateAlloca(WN_CreateIntconst(OPC_I4INTCONST, 0));
            WN *alloca_kid0 = alloca_0;
            alloca_kid0 =
                WN_Stid(Pointer_Mtype, 0, alloca_st_0, ty_idx, alloca_kid0);
            WGEN_Stmt_Append(alloca_kid0, Get_Srcpos());
            ST *alloca_st_1 =
                Gen_Temp_Symbol(ty_idx, "__builtin_apply_alloca1");
#if 0 // wgen TODO
		  WGEN_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL,
		  				       alloca_st_1);
#endif
            WN *alloca_1 = WN_CreateAlloca(kid2);
            WN *alloca_kid1 = alloca_1;
            alloca_kid1 =
                WN_Stid(Pointer_Mtype, 0, alloca_st_1, ty_idx, alloca_kid1);
            WGEN_Stmt_Append(alloca_kid1, Get_Srcpos());

            // The src is actually in 0(kid1)
            kid1 = WN_CreateIload(
                OPR_ILOAD, MTYPE_I4, MTYPE_I4, 0, MTYPE_To_TY(MTYPE_I4),
                Make_Pointer_Type(MTYPE_To_TY(MTYPE_U8)), kid1, 0);
            load_wn = WN_CreateMload(
                0, Make_Pointer_Type(MTYPE_To_TY(MTYPE_U8)), kid1, kid2);
            sp_addr = WN_LdidPreg(MTYPE_U4, 29); // $sp
            WGEN_Stmt_Append(
                WN_CreateMstore(0, Make_Pointer_Type(MTYPE_To_TY(MTYPE_U8)),
                                load_wn, sp_addr, kid2),
                Get_Srcpos());

            WGEN_Stmt_Append(call_wn, Get_Srcpos());

            call_wn = WN_Create(OPR_ICALL, ret_mtype, MTYPE_V, 1);
            WN_kid(call_wn, 0) =
                WGEN_Expand_Expr(gs_tree_value(gs_tree_operand(exp, 1)));
            WN_set_ty(call_wn, TY_pointed(Get_TY(gs_tree_type(
                                   gs_tree_value(gs_tree_operand(exp, 1))))));
            WGEN_Stmt_Append(call_wn, Get_Srcpos());

            TY_IDX tyi;
            TY &ty = New_TY(tyi);
            TY_Init(ty, 16, KIND_STRUCT, MTYPE_M, Save_Str("__apply"));
            Set_TY_align(tyi, 8);
            ST *tmpst = New_ST(CURRENT_SYMTAB);
            ST_Init(tmpst, TY_name_idx(ty), CLASS_VAR, SCLASS_AUTO,
                    EXPORT_LOCAL, tyi);
            Set_ST_is_temp_var(tmpst);
            WN *load, *store;
            load = WN_LdidPreg(MTYPE_I8, 2); // $v0
            store =
                WN_Stid(MTYPE_I8, (WN_OFFSET)0, tmpst, Spill_Int_Type, load);
            WGEN_Stmt_Append(store, Get_Srcpos());
            load = WN_LdidPreg(MTYPE_F8, 32); //$f0
            store =
                WN_Stid(MTYPE_F8, (WN_OFFSET)8, tmpst, Spill_Int_Type, load);
            WGEN_Stmt_Append(store, Get_Srcpos());
            wn = WN_Lda(Pointer_Mtype, 0, tmpst,
                        Make_Pointer_Type(ST_type(tmpst), FALSE));

            // Dealloca/Restore SP
            WN *dealloca_wn = WN_CreateDealloca(2);
            WN_kid0(dealloca_wn) =
                WN_Ldid(Pointer_Mtype, 0, alloca_st_0, ST_type(alloca_st_0));
            WN_kid1(dealloca_wn) =
                WN_Ldid(Pointer_Mtype, 0, alloca_st_1, ST_type(alloca_st_1));
            WGEN_Stmt_Append(dealloca_wn, Get_Srcpos());

            whirl_generated = TRUE;
            break;
          }
          case GSBI_BUILT_IN_RETURN:
            Set_PU_has_alloca(Get_Current_PU());
            iopc = INTRN_RETURN;
            break;

            // Implement built-in versions of the ISO C99 floating point
            // comparison macros (that avoid raising exceptions for
            // unordered operands)
          case GSBI_BUILT_IN_ISGREATER:
            iopc = INTRN_ISGREATER;
            intrinsic_op = TRUE;
            break;
          case GSBI_BUILT_IN_ISGREATEREQUAL:
            iopc = INTRN_ISGREATEREQUAL;
            intrinsic_op = TRUE;
            break;
          case GSBI_BUILT_IN_ISLESS:
            iopc = INTRN_ISLESS;
            intrinsic_op = TRUE;
            break;
          case GSBI_BUILT_IN_ISLESSEQUAL:
            iopc = INTRN_ISLESSEQUAL;
            intrinsic_op = TRUE;
            break;
          case GSBI_BUILT_IN_ISLESSGREATER:
            iopc = INTRN_ISLESSGREATER;
            intrinsic_op = TRUE;
            break;
          case GSBI_BUILT_IN_ISUNORDERED:
            iopc = INTRN_ISUNORDERED;
            intrinsic_op = TRUE;
            break;

          case GSBI_BUILT_IN_EXPECT:
#ifdef KEY
            iopc = INTRN_EXPECT;
            intrinsic_op = TRUE;
#else
            wn = WGEN_Expand_Expr(gs_tree_value(gs_tree_operand(exp, 1)));
            whirl_generated = TRUE;
#endif
            break;

          case GSBI_BUILT_IN_FFS:
            iopc = INTRN_I4FFS;
            intrinsic_op = TRUE;
            if (ret_mtype == MTYPE_V)
              ret_mtype = MTYPE_I4;
            break;

          case GSBI_BUILT_IN_CTYPE_B_LOC:
            iopc = INTRN_CTYPE_B_LOC;
            intrinsic_op = TRUE;
            break;

          case GSBI_BUILT_IN_CTYPE_TOUPPER_LOC:
            iopc = INTRN_CTYPE_TOUPPER_LOC;
            intrinsic_op = TRUE;
            break;

          case GSBI_BUILT_IN_CTYPE_TOLOWER_LOC:
            iopc = INTRN_CTYPE_TOLOWER_LOC;
            intrinsic_op = TRUE;
            break;

          case GSBI_BUILT_IN_EXTEND_POINTER:
            wn = WGEN_Expand_Expr(gs_tree_value(gs_tree_operand(exp, 1)));
            whirl_generated = TRUE;
            break;

          case GSBI_BUILT_IN_POPCOUNT:
          case GSBI_BUILT_IN_POPCOUNTL:
          case GSBI_BUILT_IN_POPCOUNTLL:
            iopc = INTRN_POPCOUNT;
            intrinsic_op = TRUE;
            break;

          case GSBI_BUILT_IN_PARITY:
          case GSBI_BUILT_IN_PARITYL:
          case GSBI_BUILT_IN_PARITYLL:
            iopc = INTRN_PARITY;
            intrinsic_op = TRUE;
            break;

          case GSBI_BUILT_IN_CLZ:
            // INTRN_CLZ32 is inline-expanded
            iopc = INTRN_CLZ32;
            intrinsic_op = TRUE;
            break;
          case GSBI_BUILT_IN_CLZL:
            // INTRN_CLZ calls library routine regardless of ABI
            iopc = TARGET_64BIT ? INTRN_CLZ : INTRN_CLZ32;
            intrinsic_op = TRUE;
            break;
          case GSBI_BUILT_IN_CLZLL:
            iopc = INTRN_CLZ;
            intrinsic_op = TRUE;
            break;

          case GSBI_BUILT_IN_CTZ:
          case GSBI_BUILT_IN_CTZL:
            iopc = INTRN_CTZ;
            intrinsic_op = TRUE;
            break;
          case GSBI_BUILT_IN_CTZLL:
            // INTRN_CTZ64 calls library routine under -m32
            // INTRN_CTZ is single bsf instruction
            iopc = TARGET_64BIT ? INTRN_CTZ : INTRN_CTZ64;
            intrinsic_op = TRUE;
            break;

          case GSBI_BUILT_IN_TRAP:
            call_wn = WN_Create(OPR_CALL, MTYPE_V, MTYPE_V, 0);
            st = Get_ST(gs_tree_operand(arg0, 0));
            Set_ST_name_idx(st, Save_Str("abort"));
            WN_st_idx(call_wn) = ST_st_idx(st);
            WN_Set_Linenum(call_wn, Get_Srcpos());
            WN_Set_Call_Default_Flags(call_wn);
            WGEN_Stmt_Append(call_wn, Get_Srcpos());
            whirl_generated = TRUE;
            break;

          case GSBI_BUILT_IN_PREFETCH: {
            // prefetch address
            gs_t pf_arg = gs_tree_operand(exp, 1);
            WN *pf_addr = WGEN_Expand_Expr(gs_tree_value(pf_arg));
            // Note 2nd/3rd argument optional
            // read/write access
            pf_arg = gs_tree_chain(pf_arg);
            UINT32 pf_flag = 0;
            int access = 0;
            if (pf_arg && gs_tree_code(gs_tree_value(pf_arg)) == GS_INTEGER_CST)
              access = gs_get_integer_value(gs_tree_value(pf_arg));
            if (access == 0)
              PF_SET_READ(pf_flag);
            else // should be 1 (write access)
              PF_SET_WRITE(pf_flag);
            // Ignore 3rd argument which gives a measure of temporal
            // locality. LNO does analyze the temporal locality, but
            // not sure what is a good way to encode it in PREFETCH.
            PF_SET_MANUAL(pf_flag); // manual prefetch
            WGEN_Stmt_Append(WN_CreatePrefetch(0, pf_flag, pf_addr),
                             Get_Srcpos());
            whirl_generated = TRUE;
          } break;
#endif

#ifdef TARG_X8664
          case GSBI_BUILT_IN_COSF:
          case GSBI_BUILT_IN_COSL:
            if (!Force_IEEE_Comparisons) {
              iopc = INTRN_COSL;
              intrinsic_op = TRUE;
              if (ret_mtype != MTYPE_FQ) {
                // generate a cvt to 'cvt_to'
                cvt_to = ret_mtype;
                ret_mtype = MTYPE_FQ;
              }
            }
            break;

          case GSBI_BUILT_IN_SINF:
          case GSBI_BUILT_IN_SINL:
            if (!Force_IEEE_Comparisons) {
              iopc = INTRN_SINL;
              intrinsic_op = TRUE;
              if (ret_mtype != MTYPE_FQ) {
                // generate a cvt to 'cvt_to'
                cvt_to = ret_mtype;
                ret_mtype = MTYPE_FQ;
              }
            }
            break;
#endif // TARG_X8664

#ifdef KEY
          case GSBI_BUILT_IN_TAN:
            // return type should only be F8 for tan
            if (ret_mtype == MTYPE_F8) {
              iopc = INTRN_TAN;
              intrinsic_op = TRUE;
            }
            break;
#endif

          default:
            DevWarn("Encountered BUILT_IN: %d at line %d\n",
                    gs_decl_function_code(func), lineno);
            break;
          }
        } else {
#ifdef TARG_X8664
          wn = WGEN_target_builtins(exp, &iopc, &intrinsic_op);
          if (wn)
            break;
#else
          Fail_FmtAssertion("Target-specific builtins NYI");
#endif
        }
      }

      if (whirl_generated) {
        break;
      }

      if (intrinsic_op) {
        WN *ikids[16];
        for (i = 0, list = gs_tree_operand(exp, 1); list;
             i++, list = gs_tree_chain(list)) {
          arg_wn = WGEN_Expand_Expr(gs_tree_value(list));
#ifdef KEY // bug 11286
          if (i == 1 && TARGET_64BIT &&
              (gs_decl_function_code(func) == GSBI_BUILT_IN_POWI ||
               gs_decl_function_code(func) == GSBI_BUILT_IN_POWIF ||
               gs_decl_function_code(func) == GSBI_BUILT_IN_POWIL)) {
            arg_wn = WN_Int_Type_Conversion(arg_wn, MTYPE_I8);
            arg_ty_idx = MTYPE_To_TY(MTYPE_I8);
            arg_mtype = MTYPE_I8;
          } else
#endif
          {
            arg_ty_idx = Get_TY(gs_tree_type(gs_tree_value(list)));
            arg_mtype = TY_mtype(arg_ty_idx);
          }
          arg_wn = WN_CreateParm(Mtype_comparison(arg_mtype), arg_wn,
                                 arg_ty_idx, WN_PARM_BY_VALUE);
          ikids[i] = arg_wn;
        }
#if defined(KEY) && defined(TARG_X8664)
        // bug 11876: type in the tree node is wrong, so patch it
        switch (INTRN_return_kind(iopc)) {
        case IRETURN_M8I1:
          ret_mtype = MTYPE_M8I1;
          break;
        case IRETURN_M8I2:
          ret_mtype = MTYPE_M8I2;
          break;
        case IRETURN_M8I4:
          ret_mtype = MTYPE_M8I4;
          break;
        default:;
        }
#endif
        wn = WN_Create_Intrinsic(OPR_INTRINSIC_OP, ret_mtype, MTYPE_V, iopc,
                                 num_args, ikids);
#ifdef KEY
        if (cvt_to != MTYPE_UNKNOWN) // bug 8251
          wn = WN_Cvt(ret_mtype, cvt_to, wn);
#endif
        break;
      }

      if (iopc) {
        call_wn = WN_Create(OPR_INTRINSIC_CALL, ret_mtype, MTYPE_V, num_args);
        WN_intrinsic(call_wn) = iopc;
      } else {
        num_handlers = Current_Handler_Count();
        call_wn =
            WN_Create(OPR_CALL, ret_mtype, MTYPE_V, num_args + num_handlers);
        ST *st2 = DECL_ST2(gs_tree_operand(arg0, 0));

        if (Opt_Level > 0 && st2) {
          WN_st_idx(call_wn) = ST_st_idx(st2);
        } else {
          st = Get_ST(gs_tree_operand(arg0, 0));
          WN_st_idx(call_wn) = ST_st_idx(st);
        }
      }
    }

    else {
      num_args++;
      num_handlers = Current_Handler_Count();
      call_wn =
          WN_Create(OPR_ICALL, ret_mtype, MTYPE_V, num_args + num_handlers);
      WN_kid(call_wn, num_args - 1) = WGEN_Expand_Expr(gs_tree_operand(exp, 0));
      WN_set_ty(call_wn,
                TY_pointed(Get_TY(gs_tree_type(gs_tree_operand(exp, 0)))));
    }

    WN_Set_Linenum(call_wn, Get_Srcpos());
    WN_Set_Call_Default_Flags(call_wn);

    if (st) {
      gs_t func = gs_tree_operand(arg0, 0);
      if (gs_decl_inline(func)) {
        wgen_invoke_inliner = TRUE;
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
      FmtAssert(target_wn != NULL, ("WGEN_Expand_Expr: write target is NULL"));
      WN *arg_wn =
          WN_CreateParm(Pointer_Mtype, target_wn,
                        Make_Pointer_Type(ty_idx, FALSE), WN_PARM_BY_VALUE);
      WN_kid(call_wn, i++) = arg_wn;
      if (WN_operator(target_wn) == OPR_LDA) {
        ST *st = WN_st(target_wn);
        Set_ST_addr_passed(*st);
      }
    }
#endif
    for (list = gs_tree_operand(exp, 1); list; list = gs_tree_chain(list)) {
      if (i == 0 && is_aggr_init_via_ctor) {
#ifdef KEY
        // Bugs 10917, 11138: The argument may not be a _decl node,
        // but, for example, an indirect_ref of it. In that case we
        // should not generate address of a _decl node, but it should
        // be address_of(indirect_ref(_decl)), i.e. the _decl itself.
        // This can happen if _decl is a pointer.
        // See simplify_aggr_init_expr() in gnu/cp.
        arg_wn = WGEN_Address_Of(gs_tree_value(list));
#else
        ST *st = Get_ST(gs_tree_value(list));
        arg_wn = WN_Lda(Pointer_Mtype, ST_ofst(st), st);
#endif
        arg_ty_idx =
            Make_Pointer_Type(Get_TY(gs_tree_type(gs_tree_value(list))));
      } else {
#ifdef KEY
        if (gs_tree_code(gs_tree_value(list)) == GS_ADDR_EXPR &&
            gs_tree_code(gs_tree_operand(gs_tree_value(list), 0)) ==
                GS_TARGET_EXPR) {
          gs_t targ = gs_tree_operand(gs_tree_value(list), 0);
          WN *targ_wn = WGEN_Expand_Expr(targ);
          arg_wn = WN_Lda(Pointer_Mtype, 0, WN_st(targ_wn), 0);
          arg_ty_idx = Make_Pointer_Type(Get_TY(gs_tree_type(targ)));
        } else {
#endif // KEY
          arg_wn = WGEN_Expand_Expr(gs_tree_value(list));
          arg_ty_idx = Get_TY(gs_tree_type(gs_tree_value(list)));
#if 1 // wgen bug 10448
          if (gs_tree_this_volatile(gs_tree_value(list)))
            Set_TY_is_volatile(arg_ty_idx);
          else
            Clear_TY_is_volatile(arg_ty_idx);
#endif
#ifdef KEY
        }
#endif     // KEY
#ifdef KEY // bug 11585
        if (WN_operator(arg_wn) == OPR_LDA) {
          ST *st = WN_st(arg_wn);
          Set_ST_addr_passed(*st);
        }
#endif // KEY
      }

      arg_mtype = TY_mtype(arg_ty_idx);
#if 1 // wgen bug 10846
      // gcc allows non-struct actual to correspond to a struct formal;
      // fix mtype of parm node so as not to confuse back-end
      if (arg_mtype == MTYPE_M) {
        arg_mtype = WN_rtype(arg_wn);
      }
#endif
      arg_wn = WN_CreateParm(Mtype_comparison(arg_mtype), arg_wn, arg_ty_idx,
                             WN_PARM_BY_VALUE);
      WN_kid(call_wn, i++) = arg_wn;
    }

#ifdef ADD_HANDLER_INFO
    if (num_handlers)
      Add_Handler_Info(call_wn, i, num_handlers);
#endif

#ifdef KEY
    if (key_exceptions && !gs_tree_nothrow(exp) &&
        // Call terminate() "when the destruction of an object during stack
        // unwinding (except.ctor) exits using an exception" [except.terminate]
        // So we don't want to form a region in such cases.
        //
        // NOTE: It need not be a destructor call, e.g. if we inline the
        // destructor, all functions inside cannot throw. We assume that it is
        // cleanup code means it has to be a destructor, be its call or its
        // body.
        !(in_cleanup) &&
        // If this expr is wrapped in a MUST_NOT_THROW_EXPR, then we must not
        // insert this call in an eh region. Example is bug 10061. The standard
        // (Section 15.5.1) requires calling terminate()
        //   when the exception handling mechanism, after completing evaluation
        //   of the expression to be thrown but before the exception is caught
        //   (except.throw), calls a user function that exits via an uncaught
        //   exception
        // See build_throw() for GNU's handling of this situation.
        !must_not_throw) {
      if (!inside_eh_region) { // check that we are not already in a region
        WN *region_body = WN_CreateBlock();
        inside_eh_region = true;
        WGEN_Stmt_Push(region_body, wgen_stmk_call_region_body, Get_Srcpos());
      }
    } else if (key_exceptions && inside_eh_region && opt_regions) {
      // The above conditions dictate that this call MUST not be inside
      // a region. So close the region.
      // TODO: Is this only for opt_regions or in general?
      if (Check_For_Call_Region())
        Did_Not_Terminate_Region = FALSE;
    }
#endif // KEY

    if (ret_mtype == MTYPE_V
#ifdef KEY
        // If the result is already put into the preferred symbol, then emit
        // the call and we're done.
        || return_in_mem
#endif
    ) {
      WGEN_Stmt_Append(call_wn, Get_Srcpos());
    }

    else {
      wn0 = WN_CreateBlock();
      WN_INSERT_BlockLast(wn0, call_wn);

#ifdef KEY
      // Preserve type information if available, in preference to
      // (void *).
      if (nop_ty_idx && TY_kind(ty_idx) == KIND_POINTER &&
          TY_mtype(TY_pointed(ty_idx)) == MTYPE_V) /* pointer to void */
        ty_idx = nop_ty_idx;
#endif
      wn1 = WN_Ldid(ret_mtype, -1, Return_Val_Preg, ty_idx);

      if (ret_mtype == MTYPE_M) { // copy the -1 preg to a temp area

        TY_IDX ret_ty_idx = ty_idx;
#ifndef KEY
        // bug 3735: the compiler cannot arbitrarily change the alignment of
        // individual structures
        if (Aggregate_Alignment > 0 &&
            Aggregate_Alignment > TY_align(ret_ty_idx))
          Set_TY_align(ret_ty_idx, Aggregate_Alignment);
#endif // !KEY
        if (TY_align(ret_ty_idx) < MTYPE_align_best(Spill_Int_Mtype))
          Set_TY_align(ret_ty_idx, MTYPE_align_best(Spill_Int_Mtype));
        ST *ret_st = Gen_Temp_Symbol(
            ret_ty_idx,
            st ? Index_To_Str(Save_Str2(".Mreturn.", ST_name(ST_st_idx(st))))
               : ".Mreturn.");
#if 0 // wgen TODO
	    WGEN_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, ret_st);
#endif

        if (!return_in_mem) {
          wn1 = WN_Stid(ret_mtype, 0, ret_st, ty_idx, wn1);
          WN_INSERT_BlockLast(wn0, wn1);
        }

        // ritual for determining the right mtypes to be used in the LDID
        UINT xtra_BE_ofst = 0; // only needed for big-endian target
        desc_ty_idx = component_ty_idx;
        if (desc_ty_idx == 0)
          desc_ty_idx = Get_TY(gs_tree_type(exp));

        if (!MTYPE_is_integral(TY_mtype(desc_ty_idx)))
          ty_idx = desc_ty_idx;
        else {
          ty_idx = nop_ty_idx;
          if (ty_idx == 0)
            ty_idx = desc_ty_idx;
        }

        if (!is_bit_field) {
          if (TY_size(desc_ty_idx) > TY_size(ty_idx)) {
            if (Target_Byte_Sex == BIG_ENDIAN)
              xtra_BE_ofst = TY_size(desc_ty_idx) - TY_size(ty_idx);
            desc_ty_idx = ty_idx;
          }
        } else {
          if (TY_size(desc_ty_idx) > TY_size(ty_idx))
            ty_idx = desc_ty_idx;
        }

        TYPE_ID rtype = Widen_Mtype(TY_mtype(ty_idx));
        TYPE_ID desc = TY_mtype(desc_ty_idx);
        if (MTYPE_is_integral(desc)) {
          if (MTYPE_signed(rtype) != MTYPE_signed(desc)) {
            if (MTYPE_size_min(rtype) > MTYPE_size_min(desc) || is_bit_field)
              rtype = Mtype_TransferSign(desc, rtype);
            else
              desc = Mtype_TransferSign(rtype, desc);
          }
        }

        Is_True(!is_bit_field || field_id <= MAX_FIELD_ID,
                ("WGEN_Expand_Expr: field id for bit-field exceeds limit"));

        wn1 = WN_CreateLdid(
            OPR_LDID, rtype, is_bit_field ? MTYPE_BS : desc,
            ST_ofst(ret_st) + component_offset + xtra_BE_ofst, ret_st,
            (field_id != 0 && component_ty_idx != 0) ? Get_TY(gs_tree_type(exp))
                                                     : ty_idx,
            field_id);
      }

      wn = WN_CreateComma(OPR_COMMA, WN_rtype(wn1), MTYPE_V, wn0, wn1);
    }
  } break;

  case GS_UNGE_EXPR:
  case GS_UNGT_EXPR:
  case GS_UNLE_EXPR:
  case GS_UNLT_EXPR:
  case GS_LTGT_EXPR:
  case GS_ORDERED_EXPR:
  case GS_UNORDERED_EXPR:
  case GS_UNEQ_EXPR: {
    WN *ikids[2];
    WN *arg_wn;
    TY_IDX arg_ty_idx;
    TYPE_ID arg_mtype;
    INTRINSIC iopc;
    BOOL nott = FALSE;

    arg_wn = WGEN_Expand_Expr(gs_tree_operand(exp, 0));
    arg_ty_idx = Get_TY(gs_tree_type(gs_tree_operand(exp, 0)));
    arg_mtype = TY_mtype(arg_ty_idx);
    ikids[0] = WN_CreateParm(arg_mtype, arg_wn, arg_ty_idx, WN_PARM_BY_VALUE);

    arg_wn = WGEN_Expand_Expr(gs_tree_operand(exp, 1));
    arg_ty_idx = Get_TY(gs_tree_type(gs_tree_operand(exp, 1)));
    arg_mtype = TY_mtype(arg_ty_idx);
    ikids[1] = WN_CreateParm(arg_mtype, arg_wn, arg_ty_idx, WN_PARM_BY_VALUE);
    switch (code) {
    case GS_UNGE_EXPR:
      iopc = INTRN_ISGREATEREQUAL;
      break;
    case GS_UNGT_EXPR:
      iopc = INTRN_ISGREATER;
      break;
    case GS_UNLE_EXPR:
      iopc = INTRN_ISLESSEQUAL;
      break;
    case GS_UNLT_EXPR:
      iopc = INTRN_ISLESS;
      break;
    case GS_LTGT_EXPR:
      iopc = INTRN_ISLESSGREATER;
      break;
    case GS_ORDERED_EXPR:
      iopc = INTRN_ISORDERED;
      break;
    case GS_UNEQ_EXPR:
    case GS_UNORDERED_EXPR:
      iopc = INTRN_ISUNORDERED;
      break;
    }
    wn = WN_Create_Intrinsic(OPR_INTRINSIC_OP, Boolean_type, MTYPE_V, iopc, 2,
                             ikids);
    if (code == GS_UNEQ_EXPR) {
      WN *eq_wn =
          WN_Relational(OPR_EQ, arg_mtype, WN_COPY_Tree(WN_kid0(ikids[0])),
                        WN_COPY_Tree(WN_kid0(ikids[1])));
      wn = WN_Binary(OPR_CIOR, Boolean_type, wn, eq_wn);
    }
  } break;

  case GS_COMPOUND_EXPR: {
#ifdef KEY
    // If we are supposed to put the result in target_wn, then give the
    // result VAR_DECL the same ST as target_wn.
    gs_t opnd1 = gs_tree_operand(exp, 1);
    if (gs_tree_code(opnd1) == GS_VAR_DECL && target_wn != NULL) {
      ST *st = DECL_ST(opnd1);
      if (st == NULL) {
        // Don't think we would see a LDID target_wn.
        FmtAssert(WN_operator(target_wn) == OPR_LDA,
                  ("WGEN_Expand_Expr: target_wn not LDA"));
        set_DECL_ST(opnd1, WN_st(target_wn));
      } else {
        FmtAssert(
            st == WN_st(target_wn),
            ("WGEN_Expand_Expr: conflicting ST in COMPOUND_EXPR's VAR_DECL"));
      }
    }
#endif
    wn = WGEN_Expand_Expr(gs_tree_operand(exp, 0), FALSE);
    if (wn && WN_has_side_effects(wn)) {
      wn = WN_CreateEval(wn);
      WGEN_Stmt_Append(wn, Get_Srcpos());
    }
#ifdef KEY
    // bug 11238: pass on the target
    wn = WGEN_Expand_Expr(gs_tree_operand(exp, 1), need_result, 0, 0, 0, 0,
                          FALSE, FALSE, target_wn);
#else
    wn = WGEN_Expand_Expr(gs_tree_operand(exp, 1), need_result);
#endif
  } break;

  case GS_NON_LVALUE_EXPR: {
#ifdef KEY
    // Pass field_id for bug 10339.
    wn = WGEN_Expand_Expr(gs_tree_operand(exp, 0), need_result, nop_ty_idx,
                          component_ty_idx, component_offset, field_id);
#else
    wn = WGEN_Expand_Expr(gs_tree_operand(exp, 0));
#endif
  } break;

  case GS_SAVE_EXPR: {
    DevWarn("Encountered SAVE_EXPR at line %d", lineno);
    wn = WGEN_Save_Expr(exp, need_result, nop_ty_idx, component_ty_idx,
                        component_offset, field_id);
  } break;

  case GS_ERROR_MARK:
    // This is not necessarily an error:  return a constant 0.
    wn = WN_Intconst(MTYPE_I4, 0);
    break;

  case GS_LOOP_EXPR: {
    DevWarn("Encountered LOOP_EXPR at line %d\n", lineno);
    if (!lang_java) // czw
    {
      LABEL_IDX saved_loop_expr_exit_label = loop_expr_exit_label;
      loop_expr_exit_label = 0;
      gs_t body = gs_loop_expr_body(exp);
      WN *loop_test = WN_Intconst(Boolean_type, 1);
      WN *loop_body = WN_CreateBlock();
      if (body) {
        WGEN_Stmt_Push(loop_body, wgen_stmk_while_body, Get_Srcpos());
        wn = WGEN_Expand_Expr(body);
        WGEN_Stmt_Pop(wgen_stmk_while_body);
      }
      WN *loop_stmt = WN_CreateWhileDo(loop_test, loop_body);
      WGEN_Stmt_Append(loop_stmt, Get_Srcpos());
      if (loop_expr_exit_label)
        WGEN_Stmt_Append(
            WN_CreateLabel((ST_IDX)0, loop_expr_exit_label, 0, NULL),
            Get_Srcpos());
      loop_expr_exit_label = saved_loop_expr_exit_label;
    } else {
      gs_t body = gs_loop_expr_body(exp);
      gs_t loop_exit = 0;
      bool whiledo = findexit(body, loop_exit);

      WN *loop_test =
          loop_exit
              ? WN_Relational(OPR_EQ, MTYPE_I4,
                              WGEN_Expand_Expr_With_Sequence_Point(
                                  gs_tree_operand(loop_exit, 0), Boolean_type),
                              WN_Intconst(MTYPE_I4, 0))
              : WN_Intconst(MTYPE_I4, 1); // czw	NOT operation
      WN *loop_body = WN_CreateBlock();
      if (body) {
        WGEN_Stmt_Push(loop_body, wgen_stmk_while_body, Get_Srcpos());
        wn = WGEN_Expand_Expr(body);
        WGEN_Stmt_Pop(wgen_stmk_while_body);
      }
      WN *loop_stmt;
      if (whiledo)
        loop_stmt = WN_CreateWhileDo(loop_test, loop_body);
      else
        loop_stmt = WN_CreateDoWhile(loop_test, loop_body);
      WGEN_Stmt_Append(loop_stmt, Get_Srcpos());
    }
  } break;

  case GS_EXIT_EXPR: {
    DevWarn("Encountered EXIT_EXPR at line %d\n", lineno);
    if (!lang_java) // czw
    {
      WN *test = WGEN_Expand_Expr(gs_tree_operand(exp, 0));
      New_LABEL(CURRENT_SYMTAB, loop_expr_exit_label);
      WN *stmt = WN_CreateTruebr(loop_expr_exit_label, test);
      WGEN_Stmt_Append(stmt, Get_Srcpos());
    }
  } break;

  case GS_VA_ARG_EXPR: {
#ifdef TARG_X8664
    if (TARGET_64BIT) {
      gs_t kid0 = gs_tree_operand(exp, 0);
      WN *ap_wn;
      ap_wn = WGEN_Expand_Expr(kid0);
      if (WN_rtype(ap_wn) == MTYPE_M) {
        if (OPCODE_is_leaf(WN_opcode(ap_wn)))
          ap_wn = WN_Lda(Pointer_Mtype, WN_offset(ap_wn), WN_st(ap_wn), 0);
        else {
          Is_True(OPCODE_is_load(WN_opcode(ap_wn)),
                  ("WGEN_Expand_Expr: unexpected VA_ARG_EXPR argument"));
          ap_wn = WN_kid0(ap_wn);
        }
      }
      TY_IDX ty_idx = Get_TY(gs_tree_type(exp));
      TYPE_ID mtype = Fix_TY_mtype(ty_idx);

      if (mtype != MTYPE_FQ && mtype != MTYPE_M && !MTYPE_is_complex(mtype)) {
        wn = WGEN_x8664_va_arg(ap_wn, MTYPE_float(mtype), ty_idx, FALSE);
        wn = WN_CreateIload(OPR_ILOAD, Widen_Mtype(mtype), mtype, 0, ty_idx,
                            Make_Pointer_Type(ty_idx), wn);
      } else if (mtype == MTYPE_C4) {
        wn = WGEN_x8664_va_arg(ap_wn, MTYPE_float(mtype), ty_idx, FALSE);
        wn = WN_CreateIload(OPR_ILOAD, MTYPE_M, MTYPE_M, 0, ty_idx,
                            Make_Pointer_Type(ty_idx), wn);
      } else {
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
          WGEN_Stmt_Append(wn, Get_Srcpos());
          /* load pointer to overflow_arg_area */
          wn = WN_Iload(Pointer_Mtype, 8, MTYPE_To_TY(Pointer_Mtype),
                        WN_CopyNode(ap_wn));
          /* adjust with the amount just incremented */
          wn1 = WN_Intconst(MTYPE_I8, -delta);
          wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
        } else if (n == 1) {
          wn = WGEN_x8664_va_arg(ap_wn, classes[0] == X86_64_SSE_CLASS, ty_idx,
                                 FALSE);
        } else if (n > 1) { /* must be == 2 */
          if (classes[0] == classes[1]) {
            if (classes[0] == X86_64_INTEGER_CLASS)
              wn = WGEN_x8664_va_arg(ap_wn, classes[0] == X86_64_SSE_CLASS,
                                     ty_idx, TRUE /*twice*/);
            else
              wn = WGEN_x8664_va_arg_2_float(ap_wn, ty_idx);
          } else {
            wn = WGEN_x8664_va_arg_2_mixed(
                ap_wn, classes[0] == X86_64_SSE_CLASS,
                classes[1] == X86_64_SSE_CLASS, ty_idx);
          }
        }

        if (mtype == MTYPE_FQ)
          wn = WN_CreateIload(OPR_ILOAD, Widen_Mtype(mtype), mtype, 0, ty_idx,
                              Make_Pointer_Type(ty_idx), wn);
        else
          wn = WN_CreateIload(OPR_ILOAD, MTYPE_M, MTYPE_M, 0, ty_idx,
                              Make_Pointer_Type(ty_idx), wn);
      }

      break;
    } // end of TARGET_64BIT
#endif
    // code swiped from builtins.c (std_expand_builtin_va_arg)
    INT64 align;
    INT64 rounded_size;
    gs_t type = gs_tree_type(exp);
    TY_IDX ty_idx = Get_TY(type);
    TYPE_ID mtype = TY_mtype(ty_idx);

    /* Compute the rounded size of the type.  */
    align = TARGET_64BIT ? 8 : 4;
    rounded_size = ((TY_size(Get_TY(type)) + align - 1) / align) * align;

    /* Get AP.  */
    WN *ap_load = WGEN_Expand_Expr(gs_tree_operand(exp, 0));
    TY_IDX ap_ty_idx = Get_TY(gs_tree_type(gs_tree_operand(exp, 0)));
    WN *ap_addr;
    ST *ap_st;
    WN_OFFSET ap_offset;

    if (WN_operator(ap_load) == OPR_LDID) {
      ap_st = WN_st(ap_load);
      ap_offset = WN_offset(ap_load);
    } else if (WN_operator(ap_load) == OPR_ILOAD) {
      ap_st = NULL;
      ap_offset = WN_offset(ap_load);
      ap_addr = WN_COPY_Tree(WN_kid0(ap_load));
      if (WN_has_side_effects(ap_addr))
        Fail_FmtAssertion("VA_ARG_EXPR: ap address has side effects");
    } else
      Fail_FmtAssertion("VA_ARG_EXPR: unknown operator for ap");

    wn = WN_COPY_Tree(ap_load);

#ifdef TARG_IA64
    // Adjust the address on IA64 when ty_size is large than align
    INT64 ty_align = gs_type_align(type) / BITS_PER_UNIT;
    ty_align = ((ty_align + align - 1) / align) * align;

    /* Align AP for the next argument. */
    if (ty_align > align) {
      wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn,
                     WN_Intconst(Pointer_Mtype, ty_align - 1));
      wn = WN_Binary(OPR_BAND, Pointer_Mtype, wn,
                     WN_Intconst(Pointer_Mtype, -ty_align));
    }
    /* Compute new value for AP.  */
    if (Target_Byte_Sex == BIG_ENDIAN) {
      wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn, WN_Intconst(Pointer_Mtype, 3));
      wn = WN_Binary(OPR_BAND, Pointer_Mtype, wn,
                     WN_Intconst(Pointer_Mtype, -8));
    }
    wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn,
                   WN_Intconst(Pointer_Mtype, rounded_size));
#else
    if (Target_Byte_Sex == BIG_ENDIAN) {
      INT64 adj;
      adj = gs_n(gs_tree_int_cst_low(gs_type_size(type))) / BITS_PER_UNIT;
      if (rounded_size > align)
        adj = rounded_size;
      wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn, WN_Intconst(Pointer_Mtype, 3));
      wn = WN_Binary(OPR_BAND, Pointer_Mtype, wn,
                     WN_Intconst(Pointer_Mtype, -8));
      wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn,
                     WN_Intconst(Pointer_Mtype, rounded_size));
    } else

      /* Compute new value for AP.  */
      wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn,
                     WN_Intconst(Pointer_Mtype, rounded_size));
#endif

    if (ap_st)
      wn = WN_Stid(Pointer_Mtype, ap_offset, ap_st, ap_ty_idx, wn);
    else {
      wn = WN_CreateIstore(OPR_ISTORE, MTYPE_V, Pointer_Mtype, ap_offset,
                           ap_ty_idx, wn, ap_addr, 0);
    }
    WGEN_Stmt_Append(wn, Get_Srcpos());
#ifdef TARG_IA64
    wn = WN_CreateIload(OPR_ILOAD, Widen_Mtype(mtype), mtype, -rounded_size,
                        ty_idx, Make_Pointer_Type(ty_idx, FALSE), ap_load);
#else
    wn =
        WN_CreateIload(OPR_ILOAD, Widen_Mtype(mtype), mtype, -rounded_size,
                       ap_ty_idx, Make_Pointer_Type(ap_ty_idx, FALSE), ap_load);
#endif

#ifdef KEY
    if (Target_Byte_Sex != Host_Byte_Sex)
      wn = WN_CreateIload(
          OPR_ILOAD, Widen_Mtype(mtype), mtype,
          ((MTYPE_size_min(mtype) == 32) ? 4 : 0) - rounded_size, ap_ty_idx,
          Make_Pointer_Type(ap_ty_idx, FALSE), ap_load);
#endif
  } break;

  case GS_LABEL_DECL: {
    DevWarn("taking address of a label at line %d", lineno);
    LABEL_IDX label_idx = WGEN_Get_LABEL(arg0, FALSE);
#if 0
        FmtAssert (arg0->decl.symtab_idx == CURRENT_SYMTAB,
                   ("line %d: taking address of a label not defined in current function currently not implemented", lineno));
#endif
    wn = WN_LdaLabel(Pointer_Mtype, label_idx);
    Set_LABEL_addr_saved(label_idx);
    Set_PU_no_inline(Get_Current_PU());
  } break;

#ifdef KEY
  case GS_LABEL_EXPR: {
    // We assume the type of LABEL_EXPR alwayse be void and operand(0) is
    // LABEL_DECL Here will declare a new label Only apply to this case: int
    // foo(int x) { if(x) { L1; } }
    gs_t type = gs_tree_type(exp);
    Is_True(gs_code(type) == GS_VOID_TYPE,
            ("Bad expression type for LABEL_EXPR"));
    gs_t label = gs_tree_operand(exp, 0);
    FmtAssert(gs_code(label) == GS_LABEL_DECL,
              ("Bad operand 0 code for LABEL_EXPR"));

    if (!lang_java || DECL_LABEL_IDX(label) != 0) // czw
    {
      LABEL_IDX label_idx = WGEN_Get_LABEL(label, TRUE);
      WN *wn1 = WN_CreateLabel((ST_IDX)0, label_idx, 0, NULL);
      WGEN_Stmt_Append(wn1, Get_Srcpos());
      Set_LABEL_addr_saved(label_idx);
      Set_PU_no_inline(Get_Current_PU());
    }
  } break;
#endif

#ifdef KEY
  case GS_EXC_PTR_EXPR: {
    if (key_exceptions) {
      ST_IDX exc_ptr_st =
          TCON_uval(INITV_tc_val(INITO_val(Get_Current_PU().unused)));
      wn = WN_Ldid(Pointer_Mtype, 0, exc_ptr_st, Get_TY(gs_tree_type(exp)));
    } else {
      ST *preg_st = MTYPE_To_PREG(Pointer_Mtype);
      wn = WN_Ldid(Pointer_Mtype, 0, preg_st, Get_TY(gs_tree_type(exp)));
    }
  } break;

  case GS_CLEANUP_STMT:
    DevWarn("CLEANUP_STMT not implemented: at line %d\n", lineno);
    // TODO:  Return a dummy constant 0 for now.
    wn = WN_Intconst(MTYPE_I4, 0);
    break;

  case GS_MUST_NOT_THROW_EXPR:
    // Call terminate if this expr throws
    must_not_throw = TRUE;
    wn = WGEN_Expand_Expr(gs_tree_operand(exp, 0));
    must_not_throw = FALSE;
    break;

  case GS_VECTOR_CST: {
    ST *init_st = Gen_Temp_Symbol(Get_TY(gs_tree_type(exp)), "__vec_cst");
    Traverse_Aggregate_Vector_Const(init_st, exp, 0, 0);
    TY_IDX ty = ST_type(init_st);
    TYPE_ID mtype = TY_mtype(ty);
    wn = WN_CreateLdid(OPR_LDID, mtype, mtype, 0, init_st, ty, 0);
    break;
  }

  // Support this when needed.
  case GS_EH_FILTER_EXPR:
    DevWarn("EH_FILTER_EXPR not yet implemented");
    break;

  case GS_OBJ_TYPE_REF:
    // We may be able to utilize the other fields.
    wn = WGEN_Expand_Expr(gs_obj_type_ref_expr(exp), need_result);
    break;

  case GS_TRY_FINALLY_EXPR:
    // The second operand is a cleanup to be executed on any exit from
    // evaluation of first operand.
    if (lang_cplus) // czw 1.17
    {
      Register_Cleanup(exp);
      WGEN_Expand_Expr(gs_tree_operand(exp, 0), need_result);
      Unregister_Cleanup();
      WGEN_Expand_Expr(gs_tree_operand(exp, 1), need_result);
    }
    if (lang_java) // czw 1.17
    {
      WGEN_Expand_Try_Finally(exp);
    }
    break;

  case GS_FILTER_EXPR:
    // TODO: Implement.  Return dummy 0 for now.
    DevWarn("NYI: FILTER_EXPR");
    wn = WN_Intconst(MTYPE_I4, 0);
    break;

  case GS_RESX_EXPR:
    // TODO: Implement.  Return dummy 0 for now.
    DevWarn("NYI: RESX_EXPR");
    wn = WN_Intconst(MTYPE_I4, 0);
    break;
#endif /* KEY */

  case GS_ASM_EXPR:
  case GS_GOTO_EXPR:
    WGEN_Expand_Stmt(exp);
    break;

  case GS_STATEMENT_LIST: {
    gs_t stmt_list = gs_statement_list_elts(exp);
    gs_t list;
    for (list = stmt_list; gs_code(list) != EMPTY; list = gs_operand(list, 1))
      WGEN_Expand_Stmt(gs_operand(list, 0), target_wn);
  } break;

  case GS_RETURN_EXPR:
    WGEN_Expand_Stmt(exp);
    break;

  case GS_SWITCH_STMT:
    WGEN_Expand_Stmt(exp, target_wn);
    break;

  case GS_BREAK_STMT:
  case GS_CONTINUE_STMT:
  case GS_WHILE_STMT:
  case GS_DO_STMT:
  case GS_FOR_STMT:
    WGEN_Expand_Stmt(exp); // bug 10857
    break;

#ifdef KEY
  case GS_VIEW_CONVERT_EXPR: {
    wn = WGEN_Expand_Expr(gs_tree_operand(exp, 0));
    ty_idx = Get_TY(gs_tree_type(exp));
    TYPE_ID mtyp = TY_mtype(ty_idx);
    if (mtyp == WN_rtype(wn))
      break;
    // bug 11423 and bug 11752
    if (WN_operator(wn) == OPR_INTCONST && !MTYPE_is_vector(mtyp) ||
        WN_operator(wn) == OPR_CONST)
      WN_set_rtype(wn, mtyp);
    else if (OPERATOR_is_load(WN_operator(wn))) {
      WN_set_rtype(wn, mtyp);
      WN_set_desc(wn, mtyp);
    } else if (!(MTYPE_is_vector(mtyp) && MTYPE_is_vector(WN_rtype(wn)))) {
      // bug 11797: TAS between vector types not needed
      wn = WN_Tas(mtyp, MTYPE_To_TY(mtyp), wn);
    }
  } break;

  case GS_USING_DECL:
    // Nothing needs to be done.  Return a dummy constant.
    wn = WN_Intconst(MTYPE_I4, 0);
    break;
#endif

#ifdef KEY
  case GS_IF_STMT:
    WGEN_Expand_Stmt(exp, target_wn);
    break;
#endif

  default:
    FmtAssert(FALSE,
              ("WGEN_Expand_Expr: not yet implemented %s", gs_code_name(code)));
    break;
  } // end switch code

  if (need_result)
    FmtAssert(wn != 0 || code == GS_CALL_EXPR || code == GS_BIND_EXPR ||
                  code == GS_STMT_EXPR || code == GS_EXPR_STMT || // KEY
                  code == GS_COMPOUND_EXPR || code == GS_INDIRECT_REF ||
                  code == GS_COMPONENT_REF || code == GS_LOOP_EXPR ||
                  code == GS_NOP_EXPR || code == GS_THROW_EXPR ||
                  code == GS_AGGR_INIT_EXPR || code == GS_STATEMENT_LIST ||
                  code == GS_CLEANUP_POINT_EXPR ||
                  ((code == GS_COND_EXPR) && (TY_mtype(ty_idx) == MTYPE_V ||
                                              TY_mtype(ty_idx) == MTYPE_M)),
              ("WGEN_Expand_Expr: NULL WHIRL tree for %s", gs_code_name(code)));

  return wn;
}

// Like WGEN_One_Stmt but don't reuse label indexes already allocated so far.
// This is necessary because the cleanup represented by the EXP tree can be
// expanded multiple times, and each expansion needs its own set of labels.
void WGEN_One_Stmt_Cleanup(gs_t exp) {
  LABEL_IDX idx = WGEN_unusable_label_idx;
  INT32 save_expr_level = wgen_save_expr_level;

  // Don't reuse label indexes that are allocated up to this point.
  WGEN_unusable_label_idx = WGEN_last_label_idx;

  // Make the saved expr's, if any, unique to this cleanup.
  wgen_save_expr_level = ++wgen_last_save_expr_level;

  WGEN_One_Stmt(exp);
  WGEN_unusable_label_idx = idx;
  wgen_save_expr_level = save_expr_level;
}

void WGEN_One_Stmt(gs_t exp, WN *target_wn) {
  WN *wn;
  wn = WGEN_Expand_Expr_With_Sequence_Point(exp, MTYPE_V, target_wn);
  if (wn) {
    for (;;) {
      if (WN_operator(wn) == OPR_COMMA) {
        WN *crwn = wn;
        if (WN_operator(WN_kid1(wn)) == OPR_LDID &&
            WN_st(WN_kid1(wn)) == Return_Val_Preg &&
            (WN_operator(WN_last(WN_kid0(wn))) == OPR_CALL ||
             WN_operator(WN_last(WN_kid0(wn))) == OPR_ICALL)) {
          WN_set_rtype(WN_last(WN_kid0(wn)), MTYPE_V);
          WGEN_Stmt_Append(WN_kid0(wn), Get_Srcpos());
          WN_Delete(crwn);
          break;
        } else {
          WGEN_Stmt_Append(WN_kid0(wn), Get_Srcpos());
          wn = WN_kid1(wn);
          WN_Delete(crwn);
        }
      } else {
        if (WN_has_side_effects(wn)) {
          wn = WN_CreateEval(wn);
          WGEN_Stmt_Append(wn, Get_Srcpos());
        }
        break;
      }
    }
  }
}

void WGEN_Expr_Init() {}

char *WGEN_Tree_Node_Name(gs_t exp) { return gs_code_name(gs_tree_code(exp)); }

// g++ uses a record to hold a ptr-to-member-function.  Return TRUE iff EXP is
// a CALL_EXPR that returns a ptr-to-member-function and the ABI requires that
// such a record be returned in memory.
//
// Invoke WGEN_Expand_Ptr_To_Member_Func_Call_Expr to expand such calls.  The
// routine creates a temp record for the ptr-to-member-function and invokes
// WGEN_Expand_Expr to expand the return value there.
static bool WGEN_Call_Returns_Ptr_To_Member_Func(gs_t exp) {
  TY_IDX exp_ty_idx = Get_TY(gs_tree_type(exp));
  if (gs_tree_code(exp) == GS_CALL_EXPR &&
      gs_type_ptrmemfunc_p(gs_tree_type(exp)) && TY_return_in_mem(exp_ty_idx)) {
    return TRUE;
  }
  return FALSE;
}

// See comment for WGEN_Call_Returns_Ptr_To_Member_Func.
static WN *WGEN_Expand_Ptr_To_Member_Func_Call_Expr(gs_t exp, TY_IDX nop_ty_idx,
                                                    TYPE_ID rtype, TYPE_ID desc,
                                                    WN_OFFSET offset,
                                                    UINT field_id) {
  TY_IDX exp_ty_idx = Get_TY(gs_tree_type(exp));
  WN *wn;
  ST *st = New_ST(CURRENT_SYMTAB);

  ST_Init(st, Save_Str("__ptr_to_mem_func"), CLASS_VAR, SCLASS_AUTO,
          EXPORT_LOCAL, exp_ty_idx);
  WN *target_wn = WN_Lda(Pointer_Mtype, ST_ofst(st), st);
  WGEN_Expand_Expr(exp, TRUE, nop_ty_idx, exp_ty_idx, 0, 0, FALSE, FALSE,
                   target_wn);
  wn = WN_CreateLdid(OPR_LDID, rtype, desc, ST_ofst(st) + offset, st,
                     exp_ty_idx, field_id);
  return wn;
}
