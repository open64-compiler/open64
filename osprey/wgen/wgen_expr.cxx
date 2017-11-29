/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright (C) 2007, 2008 PathScale, LLC.  All Rights Reserved.
 */

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

extern "C"{
#include "gspin-wgen-interface.h"
}
#if defined(BUILD_OS_DARWIN)
#include <limits.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <values.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include "pathscale_defs.h"
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
#include "wgen_tracing.h"
#ifdef FE_GNU_4_2_0
#include "omp_types.h"
#include "wgen_omp_directives.h"
#endif
#if defined(TARG_SL)
#include "erglob.h"
#include "stdarg.h"
#endif

#define BITS_PER_UNIT 8

extern void WGEN_Expand_Return(gs_t, gs_t);
extern WN *guard_var_init_block = NULL; 

LABEL_IDX loop_expr_exit_label = 0; // exit label for LOOP_EXPRs

gs_t enclosing_cleanup_point_expr = NULL;

#include <ext/hash_map>

namespace {

  using __gnu_cxx::hash_map;

  struct ptrhash {
    size_t operator()(void* p) const { return reinterpret_cast<size_t>(p); }
  };

  hash_map<gs_t, BOOL,       ptrhash>     referenced_by_whirl_map;
}

BOOL& REFERENCED_BY_WHIRL(gs_t t)	{ return referenced_by_whirl_map[t]; }

struct operator_from_tree_t {
  gs_code_t tree_code;
  OPERATOR opr;
} Operator_From_Tree [] = {
 // Generic core stuff:

 DOT,				OPERATOR_UNKNOWN,

 // list
 EMPTY,				OPERATOR_UNKNOWN,
 CONS,				OPERATOR_UNKNOWN,

 // Inbuilt Constructors:

 IB_INT,			OPERATOR_UNKNOWN,
 IB_UNSIGNED,			OPERATOR_UNKNOWN,
 IB_LONG_LONG,			OPERATOR_UNKNOWN,
 IB_LONG,			OPERATOR_UNKNOWN,
 IB_LONG_DOUBLE,		OPERATOR_UNKNOWN,
 IB_UNSIGNED_LONG,		OPERATOR_UNKNOWN,
 IB_UNSIGNED_LONG_LONG,		OPERATOR_UNKNOWN,
 IB_CHAR,			OPERATOR_UNKNOWN,
 IB_STRING,			OPERATOR_UNKNOWN,
 IB_FLOAT,			OPERATOR_UNKNOWN,
 IB_DOUBLE,			OPERATOR_UNKNOWN,
 IB_BOOL,			OPERATOR_UNKNOWN,
 IB_BIT_VECTOR,			OPERATOR_UNKNOWN,

 GS_TCC,			OPERATOR_UNKNOWN,
 GSBI,				OPERATOR_UNKNOWN,
 GSBI_CLASS,			OPERATOR_UNKNOWN,
 
 // GCC Specific tree codes:

 GS_ABS_EXPR,			OPR_ABS,
 GS_ADDR_EXPR,			OPERATOR_UNKNOWN,
 GS_ALIGNOF_EXPR,		OPERATOR_UNKNOWN,
 GS_ALIGN_INDIRECT_REF,		OPERATOR_UNKNOWN,
 GS_ARRAY_RANGE_REF,		OPERATOR_UNKNOWN,
 GS_ARRAY_REF,			OPERATOR_UNKNOWN,
 GS_ARRAY_TYPE,			OPERATOR_UNKNOWN,
 GS_ARROW_EXPR,			OPERATOR_UNKNOWN,
 GS_ASM_EXPR,			OPERATOR_UNKNOWN,
 GS_ASSIGN_STMT,		OPERATOR_UNKNOWN,
 GS_BIND_EXPR,			OPERATOR_UNKNOWN,
 GS_BIT_AND_EXPR,		OPR_BAND,
 GS_BIT_FIELD_REF,		OPERATOR_UNKNOWN,
 GS_BIT_IOR_EXPR,		OPR_BIOR,
 GS_BIT_NOT_EXPR,		OPR_BNOT,
 GS_BIT_XOR_EXPR,		OPR_BXOR,
 GS_BLOCK,			OPERATOR_UNKNOWN,
 GS_BOOLEAN_TYPE,		OPERATOR_UNKNOWN,
 GS_BREAK_STMT,			OPERATOR_UNKNOWN,
 GS_CALL_EXPR,			OPERATOR_UNKNOWN,
 GS_CASE_LABEL_EXPR,		OPERATOR_UNKNOWN,
 GS_CATCH_EXPR,			OPERATOR_UNKNOWN,
 GS_CEIL_DIV_EXPR,		OPR_DIV,
 GS_CEIL_MOD_EXPR,		OPR_DIV,
 GS_CHAR_TYPE,			OPERATOR_UNKNOWN,
 GS_CLEANUP, 			OPERATOR_UNKNOWN,
 GS_CLEANUP_POINT_EXPR,		OPERATOR_UNKNOWN,
 GS_COMPLEX_CST,		OPERATOR_UNKNOWN,
 GS_COMPLEX_EXPR,		OPR_PAIR,
 GS_COMPLEX_TYPE,		OPERATOR_UNKNOWN,
 GS_COMPONENT_REF,		OPERATOR_UNKNOWN,
 GS_COMPOUND_EXPR,		OPERATOR_UNKNOWN,
 GS_COMPOUND_LITERAL_EXPR,	OPERATOR_UNKNOWN,
 GS_COND_EXPR,			OPERATOR_UNKNOWN,
 GS_CONJ_EXPR,			OPERATOR_UNKNOWN,
 GS_CONSTRUCTOR,		OPERATOR_UNKNOWN,
 GS_CONST_DECL,			OPERATOR_UNKNOWN,
 GS_CONTINUE_STMT,		OPERATOR_UNKNOWN,
 GS_CONVERT_EXPR,		OPERATOR_UNKNOWN,
 GS_DECL_EXPR,			OPERATOR_UNKNOWN,
 GS_DO_STMT,			OPERATOR_UNKNOWN,
 GS_EH_FILTER_EXPR,		OPERATOR_UNKNOWN,
 GS_ENUMERAL_TYPE,		OPERATOR_UNKNOWN,
 GS_EQ_EXPR,			OPR_EQ,
 GS_ERROR_MARK,			OPERATOR_UNKNOWN,
 GS_EXACT_DIV_EXPR,		OPR_DIV,
 GS_EXC_PTR_EXPR,		OPERATOR_UNKNOWN,
 GS_EXIT_EXPR,			OPERATOR_UNKNOWN,
 GS_EXPR_STMT,			OPERATOR_UNKNOWN,
 GS_FDESC_EXPR,			OPERATOR_UNKNOWN,
 GS_FIELD_DECL,			OPERATOR_UNKNOWN,
 GS_FILE_TYPE,			OPERATOR_UNKNOWN,
 GS_FILTER_EXPR,		OPERATOR_UNKNOWN,
 GS_FIX_CEIL_EXPR,		OPERATOR_UNKNOWN,
 GS_FIX_FLOOR_EXPR,		OPERATOR_UNKNOWN,
 GS_FIX_ROUND_EXPR,		OPERATOR_UNKNOWN,
 GS_FIX_TRUNC_EXPR,		OPERATOR_UNKNOWN,
 GS_FLOAT_EXPR,			OPERATOR_UNKNOWN,
 GS_FLOOR_DIV_EXPR,		OPERATOR_UNKNOWN,
 GS_FLOOR_MOD_EXPR,		OPERATOR_UNKNOWN,
 GS_FOR_STMT,			OPERATOR_UNKNOWN,
 GS_FUNCTION_DECL,		OPERATOR_UNKNOWN,
 GS_FUNCTION_TYPE,		OPERATOR_UNKNOWN,
 GS_GE_EXPR,			OPR_GE,
 GS_GOTO_EXPR,			OPERATOR_UNKNOWN,
 GS_GT_EXPR,			OPR_GT,
 GS_IDENTIFIER_NODE,		OPERATOR_UNKNOWN,
 GS_IMAGPART_EXPR,		OPR_SECONDPART,
 GS_INDIRECT_REF,		OPERATOR_UNKNOWN,
 GS_INIT_EXPR,			OPERATOR_UNKNOWN,
 GS_INTEGER_CST,		OPERATOR_UNKNOWN,
 GS_INTEGER_TYPE,		OPERATOR_UNKNOWN,
 GS_LABEL_DECL,			OPERATOR_UNKNOWN,
 GS_LABEL_EXPR,			OPERATOR_UNKNOWN,
 GS_LANG_TYPE,			OPERATOR_UNKNOWN,
 GS_LE_EXPR,			OPR_LE,
 GS_LOOP_EXPR,			OPERATOR_UNKNOWN,
 GS_LROTATE_EXPR,		OPR_RROTATE,
 GS_LSHIFT_EXPR,		OPR_SHL,
 GS_LTGT_EXPR,			OPERATOR_UNKNOWN,
 GS_LT_EXPR,			OPR_LT,
 GS_MAX_EXPR,			OPR_MAX,
 GS_METHOD_TYPE,		OPERATOR_UNKNOWN,
 GS_MINUS_EXPR,			OPR_SUB,
 GS_MIN_EXPR,			OPR_MIN,
 GS_MISALIGNED_INDIRECT_REF,	OPERATOR_UNKNOWN,
 GS_MODIFY_EXPR,		OPERATOR_UNKNOWN,
 GS_MULT_EXPR,			OPR_MPY,
 GS_NAMESPACE_DECL,		OPERATOR_UNKNOWN,
 GS_NEGATE_EXPR,		OPR_NEG,
 GS_NE_EXPR,			OPR_NE,
 GS_NON_LVALUE_EXPR,		OPERATOR_UNKNOWN,
 GS_NOP_EXPR,			OPERATOR_UNKNOWN,
 GS_OBJ_TYPE_REF,		OPERATOR_UNKNOWN,
 GS_OFFSET_TYPE,		OPERATOR_UNKNOWN,
#ifdef FE_GNU_4_2_0
 GS_OMP_ATOMIC,			OPERATOR_UNKNOWN,
 GS_OMP_CLAUSE,			OPERATOR_UNKNOWN,
 GS_OMP_CRITICAL,		OPERATOR_UNKNOWN,
 GS_OMP_FOR,			OPERATOR_UNKNOWN,
 GS_OMP_MASTER,			OPERATOR_UNKNOWN,
 GS_OMP_ORDERED,		OPERATOR_UNKNOWN,
 GS_OMP_PARALLEL,		OPERATOR_UNKNOWN,
 GS_OMP_SECTION,		OPERATOR_UNKNOWN,
 GS_OMP_SECTIONS,		OPERATOR_UNKNOWN,
 GS_OMP_SINGLE,			OPERATOR_UNKNOWN,
#endif
 GS_ORDERED_EXPR,		OPERATOR_UNKNOWN,
 GS_PARM_DECL,			OPERATOR_UNKNOWN,
 GS_PHI_NODE,			OPERATOR_UNKNOWN,
 GS_PLACEHOLDER_EXPR,		OPERATOR_UNKNOWN,
 GS_PLUS_EXPR,			OPR_ADD,
 GS_POINTER_TYPE,		OPERATOR_UNKNOWN,
 GS_POLYNOMIAL_CHREC,		OPERATOR_UNKNOWN,
 GS_POSTDECREMENT_EXPR,		OPR_SUB,
 GS_POSTINCREMENT_EXPR,		OPR_ADD,
 GS_PREDECREMENT_EXPR,		OPR_SUB,
 GS_PREINCREMENT_EXPR,		OPR_ADD,
 GS_PROGRAM, 			OPERATOR_UNKNOWN,
 GS_QUAL_UNION_TYPE,		OPERATOR_UNKNOWN,
 GS_RANGE_EXPR,			OPERATOR_UNKNOWN,
 GS_RDIV_EXPR,			OPR_DIV,
 GS_REALIGN_LOAD_EXPR,		OPERATOR_UNKNOWN,
 GS_REALPART_EXPR,		OPR_FIRSTPART,
 GS_REAL_CST,			OPERATOR_UNKNOWN,
 GS_REAL_TYPE,			OPERATOR_UNKNOWN,
 GS_RECORD_TYPE,		OPERATOR_UNKNOWN,
 GS_REFERENCE_TYPE,		OPERATOR_UNKNOWN,
 GS_RESULT_DECL,		OPERATOR_UNKNOWN,
 GS_RESX_EXPR,			OPERATOR_UNKNOWN,
 GS_RETURN_EXPR,		OPERATOR_UNKNOWN,
 GS_ROUND_DIV_EXPR,		OPERATOR_UNKNOWN,
 GS_ROUND_MOD_EXPR,		OPERATOR_UNKNOWN,
 GS_RROTATE_EXPR,		OPR_RROTATE,
 GS_RSHIFT_EXPR,		OPERATOR_UNKNOWN,
 GS_SAVE_EXPR,			OPERATOR_UNKNOWN,
 GS_SCEV_KNOWN,			OPERATOR_UNKNOWN,
 GS_SCEV_NOT_KNOWN,		OPERATOR_UNKNOWN,
 GS_SCOPE,			OPERATOR_UNKNOWN,
 GS_SCOPE_STMT,			OPERATOR_UNKNOWN,
 GS_SIZEOF_EXPR,		OPERATOR_UNKNOWN,
 GS_SSA_NAME,			OPERATOR_UNKNOWN,
 GS_STATEMENT_LIST,		OPERATOR_UNKNOWN,
 GS_STMT_EXPR,			OPERATOR_UNKNOWN,
 GS_STRING_CST,			OPERATOR_UNKNOWN,
 GS_SWITCH_EXPR,		OPERATOR_UNKNOWN,
 GS_SWITCH_STMT,		OPERATOR_UNKNOWN,
 GS_TARGET_EXPR,		OPERATOR_UNKNOWN,
 GS_TRANSLATION_UNIT_DECL,	OPERATOR_UNKNOWN,
 GS_TREE_BINFO,			OPERATOR_UNKNOWN,
 GS_TREE_LIST,			OPERATOR_UNKNOWN,
 GS_TREE_VEC,			OPERATOR_UNKNOWN,
 GS_TRUNC_DIV_EXPR,		OPR_DIV,
 GS_TRUNC_MOD_EXPR,		OPR_REM,
 GS_TRUTH_ANDIF_EXPR,		OPR_CAND,
 GS_TRUTH_AND_EXPR,		OPR_BAND,
 GS_TRUTH_NOT_EXPR,		OPR_LNOT,
 GS_TRUTH_ORIF_EXPR,		OPR_CIOR,
 GS_TRUTH_OR_EXPR,		OPR_BIOR,
 GS_TRUTH_XOR_EXPR,		OPR_BXOR,
 GS_TRY_BLOCK,			OPERATOR_UNKNOWN,
 GS_TRY_CATCH_EXPR,		OPERATOR_UNKNOWN,
 GS_TRY_FINALLY_EXPR,		OPERATOR_UNKNOWN,
 GS_TYPE_DECL,			OPERATOR_UNKNOWN,
 GS_UNEQ_EXPR,			OPERATOR_UNKNOWN,
 GS_UNGE_EXPR,			OPERATOR_UNKNOWN,
 GS_UNGT_EXPR,			OPERATOR_UNKNOWN,
 GS_UNION_TYPE,			OPERATOR_UNKNOWN,
 GS_UNLE_EXPR,			OPERATOR_UNKNOWN,
 GS_UNLT_EXPR,			OPERATOR_UNKNOWN,
 GS_UNORDERED_EXPR,		OPERATOR_UNKNOWN,
 GS_VALUE_HANDLE,		OPERATOR_UNKNOWN,
 GS_VAR_DECL,			OPERATOR_UNKNOWN,
 GS_VA_ARG_EXPR,		OPERATOR_UNKNOWN,
 GS_VECTOR_CST,			OPERATOR_UNKNOWN,
 GS_VECTOR_TYPE,		OPERATOR_UNKNOWN,
 GS_VEC_COND_EXPR,		OPERATOR_UNKNOWN,
 GS_VIEW_CONVERT_EXPR,		OPERATOR_UNKNOWN,
 GS_VOID_TYPE,			OPERATOR_UNKNOWN,
 GS_WHILE_STMT,			OPERATOR_UNKNOWN,
 GS_WITH_CLEANUP_EXPR,		OPERATOR_UNKNOWN,
 GS_WITH_SIZE_EXPR,		OPERATOR_UNKNOWN,

 GS_AGGR_INIT_EXPR,		OPERATOR_UNKNOWN,
 GS_BASELINK,			OPERATOR_UNKNOWN,
 GS_BOUND_TEMPLATE_TEMPLATE_PARM,	OPERATOR_UNKNOWN,
 GS_CAST_EXPR,			OPERATOR_UNKNOWN,
 GS_CONST_CAST_EXPR,		OPERATOR_UNKNOWN,
 GS_CLEANUP_STMT,		OPERATOR_UNKNOWN,
 GS_CTOR_INITIALIZER,		OPERATOR_UNKNOWN,
 GS_DELETE_EXPR,		OPERATOR_UNKNOWN,
 GS_DEFAULT_ARG,		OPERATOR_UNKNOWN,
 GS_DYNAMIC_CAST_EXPR,		OPERATOR_UNKNOWN,
 GS_DOTSTAR_EXPR,		OPERATOR_UNKNOWN,
 GS_EMPTY_CLASS_EXPR,		OPERATOR_UNKNOWN,
 GS_EH_SPEC_BLOCK,		OPERATOR_UNKNOWN,
 GS_HANDLER,			OPERATOR_UNKNOWN,
 GS_IF_STMT,			OPERATOR_UNKNOWN,
 GS_MEMBER_REF,			OPERATOR_UNKNOWN,
 GS_MODOP_EXPR,			OPERATOR_UNKNOWN,
 GS_MUST_NOT_THROW_EXPR,	OPERATOR_UNKNOWN,
 GS_NEW_EXPR,			OPERATOR_UNKNOWN,
 GS_NON_DEPENDENT_EXPR,		OPERATOR_UNKNOWN,
 GS_OFFSET_REF,			OPERATOR_UNKNOWN,
 GS_OFFSETOF_EXPR,		OPERATOR_UNKNOWN,
 GS_OVERLOAD,			OPERATOR_UNKNOWN,
 GS_PTRMEM_CST,			OPERATOR_UNKNOWN,
 GS_PSEUDO_DTOR_EXPR,		OPERATOR_UNKNOWN,
 GS_REINTERPRET_CAST_EXPR,	OPERATOR_UNKNOWN,
 GS_SCOPE_REF,			OPERATOR_UNKNOWN,
 GS_STATIC_CAST_EXPR,		OPERATOR_UNKNOWN,
 GS_TAG_DEFN,			OPERATOR_UNKNOWN,
 GS_TEMPLATE_DECL,		OPERATOR_UNKNOWN,
 GS_TEMPLATE_ID_EXPR,		OPERATOR_UNKNOWN,
 GS_TEMPLATE_PARM_INDEX,	OPERATOR_UNKNOWN,
 GS_TEMPLATE_TYPE_PARM,		OPERATOR_UNKNOWN,
 GS_THROW_EXPR,			OPERATOR_UNKNOWN,
 GS_TINST_LEVEL,		OPERATOR_UNKNOWN,
 GS_TYPEOF_TYPE,		OPERATOR_UNKNOWN,
 GS_TYPENAME_TYPE,		OPERATOR_UNKNOWN,
 GS_TYPE_EXPR,			OPERATOR_UNKNOWN,
 GS_TYPEID_EXPR,		OPERATOR_UNKNOWN,
 GS_USING_DECL,			OPERATOR_UNKNOWN,
 GS_USING_STMT,			OPERATOR_UNKNOWN,
 GS_UNBOUND_CLASS_TEMPLATE,	OPERATOR_UNKNOWN,
 GS_VEC_DELETE_EXPR,		OPERATOR_UNKNOWN,
 GS_VEC_NEW_EXPR,		OPERATOR_UNKNOWN,
 GS_TEMPLATE_TEMPLATE_PARM,	OPERATOR_UNKNOWN,
 GS_FREQ_HINT_STMT,		OPERATOR_UNKNOWN,
 GS_ZDL_STMT,                   OPERATOR_UNKNOWN,
};

#ifdef FE_GNU_4_2_0
extern OPERATOR
Operator_From_Tree_node (gs_code_t code) {
  return Operator_From_Tree[code].opr;
}
#endif

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

#define INTRN_EATTR_LAST 13
static INTRN_ATTR_EXTEND intrn_eattr[INTRN_EATTR_LAST] = {
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
     WGEN_Stmt_Append(stmt1, src);
   }
}
#endif

static void WN_Set_Deref_If_Needed(WN *wn) {
  INTRINSIC intrn=WN_intrinsic(wn);

#if defined(TARG_SL)
  if (intrinsic_need_deref(intrn)) {
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
#elif defined(TARG_X8664)
  if (intrn == INTRN_LOADLPD || intrn == INTRN_LOADHPD ||
      intrn == INTRN_LOADLPS || intrn == INTRN_LOADHPS ||
      intrn == INTRN_LOADUPS || intrn == INTRN_LOADUPS256 ||
      intrn == INTRN_LOADUPD || intrn == INTRN_LOADUPD256 ||
      intrn == INTRN_LOADDQU || intrn == INTRN_LOADDQU256) {
    WN_Set_Parm_Dereference(WN_kid(wn,0));
  }
#endif
}

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

static bool WGEN_Call_Returns_Ptr_To_Member_Func (gs_t exp);

static WN *WGEN_Expand_Ptr_To_Member_Func_Call_Expr (gs_t exp,
	     TY_IDX nop_ty_idx, TYPE_ID rtype, TYPE_ID desc,
	     WN* offset = NULL, UINT field_id = 0);

// The words in 'buf' are in target order. Convert them to host order
// in place. 'buf' is a two word array.
void
WGEN_Convert_To_Host_Order (long *buf)
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
//
// If NEED_COMMA is false, then only insert the conditional statements,
// without adding any comma node.
static void
WGEN_add_guard_var (gs_t guard_var, WN *value_wn, BOOL need_comma = TRUE)
{
  WN *stid, *comma;

  // Set the guard variable to 0 before the condition is evaluated.
  WN *zero_wn = WN_Intconst(MTYPE_I4, 0);
  stid = WN_Stid(MTYPE_I4, 0, Get_ST(guard_var), MTYPE_To_TY(MTYPE_I4),
		 zero_wn, 0);

  SRCPOS srcpos = Get_Srcpos(); 
  if (srcpos)
  { 
    WN_Set_Linenum ( stid, srcpos );
  }
  WN *guard_var_init_block = WGEN_Guard_Init_Block_Stack_Top(); 
  FmtAssert( (guard_var_init_block != NULL), ("Missing init block for guard variable init!"));
  WN_INSERT_BlockLast(guard_var_init_block, stid);


  // Set the guard variable to 1 while evaluating the value of the conditional
  // expression.
  WN *one_wn = WN_Intconst(MTYPE_I4, 1);
  stid = WN_Stid(MTYPE_I4, 0, Get_ST(guard_var), MTYPE_To_TY(MTYPE_I4),
		 one_wn, 0);
  if (need_comma) {
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
              ("WGEN_add_guard_var: unexpected WN operator"));
    WN_INSERT_BlockFirst(wn, stid);
  }
  else WN_INSERT_BlockFirst(value_wn, stid);
}

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
        INT UNITS_PER_WORD = TARGET_64BIT ? 8 : 4;
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
WGEN_Set_ST_Addr_Saved (WN *wn)
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

      WGEN_Set_ST_Addr_Saved (WN_kid0 (wn));
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

      WGEN_Set_ST_Addr_Saved (WN_kid0(wn));
      break;

    case OPR_CSELECT:

      WGEN_Set_ST_Addr_Saved (WN_kid1(wn));
      WGEN_Set_ST_Addr_Saved (WN_kid2(wn));
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

      WGEN_Set_ST_Addr_Saved (WN_kid0(wn));
      WGEN_Set_ST_Addr_Saved (WN_kid1(wn));
      break;

    case OPR_CAND:
    case OPR_CIOR:

      break;

    case OPR_COMMA:

      WGEN_Set_ST_Addr_Saved (WN_kid1(wn));
      break;

    case OPR_RCOMMA:

      WGEN_Set_ST_Addr_Saved (WN_kid0(wn));
      break;

    default:

      DevWarn ("WGEN_Set_ST_Addr_Saved not implemented");
  }
} /* WGEN_Set_ST_Addr_Saved */


/* to support VLA in structure in which the field offset isn't
 * constant, a WN is generated to represent the offset.
 * for normal structs, the WN is INTCONST and value if the offset.
 * for VLA in structs, the WN is an INT expression.
 */
static WN*
WGEN_Expand_Field_Offset(gs_t arg1) {
  Is_True(gs_tree_code(arg1) == GS_FIELD_DECL,
          ("WGEN_Expand_Field_Offset: arg1 is not field decl"));
  WN* ofst = WGEN_Expand_Expr(gs_decl_field_offset(arg1));
  WN* bit_ofst = WGEN_Expand_Expr(gs_decl_field_bit_offset(arg1));
  TYPE_ID rtype = WN_rtype(ofst);
  if (WN_operator(ofst) == OPR_INTCONST &&
      WN_operator(bit_ofst) == OPR_INTCONST) {
    INT64 ofst_value = (BITSPERBYTE * WN_const_val(ofst) + WN_const_val(bit_ofst))
                       / BITSPERBYTE;
    ofst = (ofst_value == 0) ? NULL : WN_Intconst(rtype, ofst_value);
  }
  else {
    ofst = WN_Div(rtype,
                  WN_Add(rtype,
                         WN_Mpy(rtype, 
                                ofst,
                                WN_Intconst(rtype, BITSPERBYTE)),
                         bit_ofst),
                  WN_Intconst(rtype, BITSPERBYTE));
  }
  return ofst;
}

/* create a new address expression for given ST and offset
 * if ofst is NULL or intconst, return LDA.
 * otherwise, generate a add expression for the address.
 */
static WN*
WGEN_Compose_Address(ST* st, WN* ofst, TY_IDX hl_ty, UINT32 field_id) {
  if (ofst == NULL || WN_operator(ofst) == OPR_INTCONST) {
    INT64 ofst_value = (ofst == NULL) ? 0 : WN_const_val(ofst);
    return WN_CreateLda(OPR_LDA, Pointer_Mtype, MTYPE_V,
                        ST_ofst(st) + ofst_value, Make_Pointer_Type(hl_ty), st, field_id);
  }
  else {
    WN* lda = WN_CreateLda(OPR_LDA, Pointer_Mtype, MTYPE_V,
                           ST_ofst(st), Make_Pointer_Type(hl_ty), st, field_id);
    return WN_Binary(OPR_ADD, Pointer_Mtype, lda, ofst);  // TODO: use ILDA instrad
  }
}

/* compose a new offset expression
 * if given ofst1 and ofst2 are constants, generate a new INTCONST WN
 * otherwise, generate a ADD expression
 */
static WN*
WGEN_Compose_Offset(WN* ofst1, WN* ofst2) {
  if (ofst1 == NULL)
    return ofst2;
  if (ofst2 == NULL)
    return ofst1;
  TYPE_ID rtype = WN_rtype(ofst1);
  if (WN_operator(ofst1) == OPR_INTCONST && WN_operator(ofst2) == OPR_INTCONST)
    return WN_Intconst(rtype, WN_const_val(ofst1) + WN_const_val(ofst2));
  else
    return WN_Add(rtype, ofst1, ofst2);
}

/* crate a Iload/Istore with constant or variable offset
 * if given offset is constant, the value is on the Iload/Istore node.
 * other, a new address is generated using OPR_ADD and used in Iload/Istore.
 */
static WN*
WGEN_CreateIload(OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
                   WN* offset, TY_IDX ty,
                   TY_IDX load_addr_ty, WN *addr, UINT field_id) {
  INT64 ofst_value = 0;
  if (offset == NULL || WN_operator(offset) == OPR_INTCONST) {
    ofst_value = (offset == NULL) ? 0 : WN_const_val(offset);
  }
  else {
    addr = WN_Binary(OPR_ADD, Pointer_Mtype, addr, offset);  // TODO: use ILDA instrad
  }
  return WN_CreateIload(opr, rtype, desc,
                        ofst_value, ty,
                        load_addr_ty, addr, field_id);
}

static WN*
WGEN_CreateIstore(OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
                    WN* offset, TY_IDX ty, WN *value, WN *addr,
                    UINT field_id) {
  INT64 ofst_value = 0;
  if (offset == NULL || WN_operator(offset) == OPR_INTCONST) {
    ofst_value = (offset == NULL) ? 0 : WN_const_val(offset);
    FmtAssert(desc != MTYPE_M || TY_size(TY_pointed(ty)) != -1,
              ("WGEN_CreateIstore: TODO: store variable length object"));
  }
  else {
    addr = WN_Binary(OPR_ADD, Pointer_Mtype, addr, offset);  // TODO: use ILDA instrad
  }
  return WN_CreateIstore(opr, rtype, desc,
                         ofst_value, ty, value, addr,
                         field_id);
}

/* create a LDID/STID with constant or variable offset
 * if given offset is constant, the LDID/STID with constant offset is generated
 * other, a new address is generated using OPR_LDA and OPR_ADD and the final returned
 * WN is Iload/Istore
 */
static WN*
WGEN_CreateLdid(OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
                  WN* offset, ST* st, TY_IDX ty, UINT field_id) {
  if (offset == NULL || WN_operator(offset) == OPR_INTCONST) {
    INT64 ofst_value = (offset == NULL) ? 0 : WN_const_val(offset);
    return WN_CreateLdid(opr, rtype, desc,
                         ST_ofst(st) + ofst_value, st, ty, field_id);
  }
  else {
    // variable offset should only appear in VLA/VLS but we never generate
    // LDID for VLA/VLS
    Is_True(FALSE, ("WGEN_CreateLdid: offset is not constant for LDID."));
    return NULL;
  }
}

static WN*
WGEN_CreateStid(OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
                  WN* offset, ST* st, TY_IDX ty, WN *value, UINT field_id) {
  if (offset == NULL || WN_operator(offset) == OPR_INTCONST) {
    INT64 ofst_value = (offset == NULL) ? 0 : WN_const_val(offset);
    return WN_CreateStid(opr, rtype, desc,
                         ST_ofst(st) + ofst_value, st, ty, value, field_id);
  }
  else {
    // variable offset should only appear in VLA/VLS but we never generate
    // STID for VLA/VLS
    Is_True(FALSE, ("WGEN_CreateStid: offset is not constant for STID."));
    return NULL;
  }
}

typedef struct wgen_save_expr_t {
  gs_t  exp;
  ST   *st;
#ifdef KEY
  INT32  level;	// to identify which cleanup the save expr belongs to
#endif
} WGEN_SAVE_EXPR;

WGEN_SAVE_EXPR *wgen_save_expr_stack      = NULL;
INT32          wgen_save_expr_stack_last = -1;
INT32          wgen_save_expr_stack_max  = 0;

#ifdef KEY
INT32 wgen_save_expr_level;	// identify the current cleanup
INT32 wgen_last_save_expr_level;	// the last cleanup level used
#endif

static WN*
WGEN_Save_Expr (gs_t save_exp,
               bool need_result,
               TY_IDX nop_ty_idx,
               TY_IDX component_ty_idx,
               WN* component_offset,
               UINT16 field_id)
{
  INT32     i;
  gs_t      exp     = gs_tree_operand (save_exp, 0);
  TY_IDX    ty_idx  = Get_TY (gs_tree_type (exp));
  TYPE_ID   mtype   = TY_mtype (ty_idx);
  ST       *st;
  WN       *wn;
  bool     found = false;  

  for (i = wgen_save_expr_stack_last; i >= 0; i--) {
#ifndef KEY
    if (wgen_save_expr_stack [i].exp == exp) {
#else
    if (wgen_save_expr_stack [i].exp == save_exp &&
    	wgen_save_expr_stack [i].level == wgen_save_expr_level) {
#endif
      st = wgen_save_expr_stack [i].st;
      FmtAssert (st != 0,
                 ("WGEN_Save_Expr: st not yet assigned"));
      found = true;
      break;
    }
  }
  
  if (!found) {
    i = ++wgen_save_expr_stack_last;
    if (i == wgen_save_expr_stack_max) {
      if (wgen_save_expr_stack == NULL) {
        wgen_save_expr_stack_max = 32;
        wgen_save_expr_stack     =
          (WGEN_SAVE_EXPR *) malloc (wgen_save_expr_stack_max *
                                    sizeof (WGEN_SAVE_EXPR));
      }
      else {
        wgen_save_expr_stack_max = wgen_save_expr_stack_max +
                                  (wgen_save_expr_stack_max >> 1);
        wgen_save_expr_stack     =
          (WGEN_SAVE_EXPR *) realloc (wgen_save_expr_stack,
                                     wgen_save_expr_stack_max *
                                     sizeof (WGEN_SAVE_EXPR));
      }
    }
#ifndef KEY
    wgen_save_expr_stack [i].exp = exp;
#else
    wgen_save_expr_stack [i].exp = save_exp;
    wgen_save_expr_stack [i].level = wgen_save_expr_level;
#endif
    wgen_save_expr_stack [i].st  = 0;
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
    wn = WGEN_Expand_Expr (exp);

    st = Gen_Temp_Symbol (ty_idx, "__save_expr");
#ifdef FE_GNU_4_2_0
    WGEN_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, st);
#endif
    WGEN_Set_ST_Addr_Saved (wn);
    wn = WN_Stid (mtype, 0, st, ty_idx, wn);
    WGEN_Stmt_Append (wn, Get_Srcpos());
    wgen_save_expr_stack [i].st = st;
  }

  if (component_ty_idx == 0)
    wn = WN_Ldid (mtype, 0, st, ty_idx);
  else {
    TYPE_ID desc  = TY_mtype(component_ty_idx);
    TYPE_ID rtype = Widen_Mtype(desc);
    wn = WGEN_CreateLdid(OPR_LDID, rtype, desc, component_offset, st,
		     field_id? ty_idx : component_ty_idx, field_id);  
  }
  return wn;
} /* WGEN_Save_Expr */

static WN * 
WGEN_Expand_Math_Errno_Sqrt(gs_t exp, TY_IDX ty_idx, TYPE_ID ret_mtype)
{
  WN *arg_wn = WGEN_Expand_Expr (gs_tree_value (gs_tree_operand (exp, 1)));
  ST *arg_st = Gen_Temp_Symbol(ty_idx, "__sqrt_arg");
#ifdef FE_GNU_4_2_0
  WGEN_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, arg_st);
#endif
  WN *stmt = WN_Stid(ret_mtype, 0, arg_st, ty_idx, arg_wn);
  WGEN_Stmt_Append(stmt, Get_Srcpos());

  arg_wn = WN_Ldid(ret_mtype, 0, arg_st, ty_idx);
  WN *wn = WN_CreateExp1 (OPR_SQRT, ret_mtype, MTYPE_V, arg_wn);

  ST *res_st = Gen_Temp_Symbol(ty_idx, "__save_sqrt");
#ifdef FE_GNU_4_2_0
  WGEN_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, res_st);
#endif
  stmt = WN_Stid(ret_mtype, 0, res_st, ty_idx, wn);
  WGEN_Stmt_Append(stmt, Get_Srcpos());

  wn = WN_Ldid(ret_mtype, 0, res_st, ty_idx);
  WN *wn0 = WN_Ldid(ret_mtype, 0, res_st, ty_idx);
  WN *then_block = WN_CreateBlock();
  WN *else_block = WN_CreateBlock();
  WN *if_stmt = WN_CreateIf( WN_Relational(OPR_NE, ret_mtype, wn, wn0),
  			     then_block, else_block);
  WGEN_Stmt_Append(if_stmt, Get_Srcpos());

  WGEN_Stmt_Push (then_block, wgen_stmk_if_then, Get_Srcpos());

  // generate the call to sqrt()
  gs_t arg0 = gs_tree_operand(exp, 0);
  WN *call_wn = WN_Create(OPR_CALL, ret_mtype, MTYPE_V, 1);
  ST *st2 = DECL_ST2(gs_tree_operand(arg0, 0));
  if (Opt_Level > 0 && st2) {
    WN_st_idx (call_wn) = ST_st_idx (st2);
  }
  else {
    ST *st = Get_ST (gs_tree_operand (arg0, 0));
    WN_st_idx (call_wn) = ST_st_idx (st);
  }
  arg_wn = WN_Ldid(ret_mtype, 0, arg_st, ty_idx);
  arg_wn = WN_CreateParm(ret_mtype, arg_wn, ty_idx, WN_PARM_BY_VALUE);
  WN_kid(call_wn, 0) = arg_wn;
  WGEN_Stmt_Append(call_wn, Get_Srcpos());

  wn = WN_Ldid (ret_mtype, -1, Return_Val_Preg, ty_idx);
  stmt = WN_Stid(ret_mtype, 0, res_st, ty_idx, wn);
  WGEN_Stmt_Append(stmt, Get_Srcpos());

  WGEN_Stmt_Pop(wgen_stmk_if_then);

  return WN_Ldid(ret_mtype, 0, res_st, ty_idx);
}

/* process the tree doing array indicing and return the WN that performs
 * the address computation; ty_idx returns the high-level array type if it
 * is a DECL node, and the element type if it is an ARRAY_REF node.
 */
static WN *
WGEN_Array_Expr(gs_t exp, 
	       TY_IDX *ty_idx, 
	       TY_IDX component_ty_idx,
	       WN* component_offset,
	       UINT32 field_id)
{
  WN *wn;
  gs_code_t code = gs_tree_code (exp);
  if (code == GS_COMPONENT_REF) {
    TY_IDX ty_idx0;
    gs_t arg0 = gs_tree_operand(exp, 0); 
    gs_t arg1 = gs_tree_operand(exp, 1); 
    if (component_ty_idx == 0)
      ty_idx0 = Get_TY(gs_tree_type(exp));
    else ty_idx0 = component_ty_idx;
#ifdef KEY // bug 10728
    if (gs_tree_this_volatile(exp))
      Set_TY_is_volatile(ty_idx0);
#endif
    Is_True(! gs_decl_bit_field(arg1),
	    ("WGEN_Array_Expr: address arithmetic cannot use bitfield addresses"));
    WN* ofst = WGEN_Expand_Field_Offset(arg1);
#ifdef KEY
    // Refer GCC 4.0.2: gcc.c-torture/compile/struct-non-lval-3.c
    // We only handle this case so far:
    // (p = q).x[index]
    // the lhs of modify_expr is var_decl, not an expression
    if ( gs_tree_code(arg0) == GS_MODIFY_EXPR ) {
      WGEN_Expand_Expr(arg0);
      gs_t lhs = gs_tree_operand(arg0, 0);
      Is_True( lhs != NULL && gs_tree_code(lhs) == GS_VAR_DECL,
		      ("Unsupported lhs for `(p=q).x[n]'"));
      arg0 = lhs;
    }
#endif

    ofst = WGEN_Compose_Offset(ofst, component_offset);    

#ifdef KEY // bug 9725: If the field is an array of struct, it is considered
           // a single field.
    return WGEN_Array_Expr(arg0, ty_idx, ty_idx0, ofst,
			  DECL_FIELD_ID(arg1));
#else
    return WGEN_Array_Expr(arg0, ty_idx, ty_idx0, ofst,
			  field_id + DECL_FIELD_ID(arg1));
#endif
  }
#ifdef KEY
  else if (code == GS_VAR_DECL && gs_decl_value_expr(exp)) {
    return WGEN_Array_Expr(gs_decl_value_expr(exp),
                           ty_idx, component_ty_idx, component_offset,
                           field_id);
  }
#endif
  else if (code == GS_VAR_DECL || code == GS_PARM_DECL
#ifdef KEY
           || code == GS_RESULT_DECL
#endif
          ) {
    ST *st = Get_ST (exp);
    ST *base_st = ST_base (st);
    if (component_ty_idx == 0)
      *ty_idx = ST_type(st);
    else {
      *ty_idx = component_ty_idx;
      if (TY_align(ST_type(st)) < TY_align(component_ty_idx))
	Set_TY_align(*ty_idx, TY_align(ST_type(st)));//pick more stringent align
    }
    Is_True(TY_kind(*ty_idx) == KIND_ARRAY,
	    ("WGEN_Array_Expr: ARRAY_REF base not of type KIND_ARRAY"));
    // for VLAs the instead of using the ST use its base st
    // also for the time being do not support VLAs within structs
    if (st != base_st) {
      DevWarn ("Encountered VLA at line %d", lineno);
      Is_True(ST_ofst(st) == 0, ("TODO: ST_ofst is not 0"));
      wn = WN_Ldid (Pointer_Mtype, 0, base_st, Make_Pointer_Type(*ty_idx));
      if (component_offset != NULL) { // TODO: use ILDA instead
        wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn, component_offset);
      }
    }
    else {
      wn = WGEN_Compose_Address(st, component_offset, ST_type(st), field_id);
    }
    return wn;
  }
  else if (code == GS_CONSTRUCTOR) {
    ST *st = WGEN_Generate_Temp_For_Initialized_Aggregate (exp, "");
    if (component_ty_idx == 0)
      *ty_idx = ST_type(st);
    else *ty_idx = component_ty_idx;
    Is_True(TY_kind(*ty_idx) == KIND_ARRAY,
	    ("WGEN_Array_Expr: ARRAY_REF base not of type KIND_ARRAY"));
    wn = WGEN_Compose_Address(st, component_offset, ST_type(st), field_id);
    return wn;
  }
  else if (code == GS_STRING_CST) {
    wn = WGEN_Expand_Expr(exp);
    *ty_idx = ST_type (TREE_STRING_ST (exp));
    return wn;
  }
  else if (code == GS_INDIRECT_REF) {
    wn = WGEN_Expand_Expr(gs_tree_operand (exp, 0));
    if (component_ty_idx == 0)
      *ty_idx = Get_TY (gs_tree_type(exp));
    else {
      *ty_idx = component_ty_idx;
      INT node_align = gs_type_align(gs_tree_type(exp)) / BITSPERBYTE;
      if (node_align < TY_align(component_ty_idx))
	Set_TY_align(*ty_idx, node_align);//pick more stringent align
    }
    if (component_offset != NULL) { // TODO: use ILDA instead
      wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn, component_offset);
    }
    return wn;
  }
  else if (code == GS_CALL_EXPR) {
    wn = WGEN_Expand_Expr(exp);
    FmtAssert (WN_opcode (wn) == OPC_MCOMMA,
               ("opcode other than OPC_MCOMMA for call underneath ARRAY_REF"));
    WGEN_Stmt_Append (WN_kid0 (wn), Get_Srcpos ());
    ST *st = WN_st (WN_kid1 (wn));
    WN_Delete (WN_kid1 (wn));
    WN_Delete (wn);
    if (component_ty_idx == 0)
      *ty_idx = ST_type(st);
    else {
      *ty_idx = component_ty_idx;
      if (TY_align(ST_type(st)) < TY_align(component_ty_idx))
	Set_TY_align(*ty_idx, TY_align(ST_type(st)));//pick more stringent align
    }
    Is_True(TY_kind(*ty_idx) == KIND_ARRAY,
	    ("WGEN_Array_Expr: ARRAY_REF base not of type KIND_ARRAY"));
    wn = WGEN_Compose_Address(st, component_offset, ST_type(st), field_id);
    return wn;
  }
  else if (code == GS_ARRAY_REF) { // recursive call
    WN *wn0, *wn1, *wn2;
    TY_IDX ty_idx0;
#ifdef KEY  // Bug 5831.
    wn0 = WGEN_Array_Expr(gs_tree_operand (exp, 0), &ty_idx0, 0,
			 component_offset, field_id);
#else
    wn0 = WGEN_Array_Expr(gs_tree_operand (exp, 0), &ty_idx0, component_ty_idx,
			 component_offset, field_id);
#endif
    Is_True(TY_kind(ty_idx0) == KIND_ARRAY,
	    ("WGEN_Array_Expr: arg 0 of ARRAY_REF not of type KIND_ARRAY"));
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
      wn2 = WGEN_Expand_Expr (gs_tree_operand (exp, 1));
#ifdef TARG_X8664 // bug 11705
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

      WN_element_size(wn) = TY_size(Get_TY (gs_tree_type(exp)));
    }
    else Is_True(FALSE,
		 ("WGEN_Array_Expr: only const-bounds 1-dimension arrays handled now"));
    if (component_ty_idx == 0) {
      *ty_idx = TY_etype(ty_idx0);
      if (TY_align(ty_idx0) < TY_align(*ty_idx))
	Set_TY_align(*ty_idx, TY_align(ty_idx0));// pick more stringent align
#ifdef KEY // bug 10728
      if (gs_tree_this_volatile(exp))
	Set_TY_is_volatile(*ty_idx);
#endif
    }
    else *ty_idx = component_ty_idx;
    return wn;
  }
#ifdef KEY
  else if (code == GS_COMPOUND_LITERAL_EXPR) {
    gs_t arg0 = gs_decl_initial (gs_tree_operand (gs_tree_operand (exp, 0), 0));
    ST *st = WGEN_Generate_Temp_For_Initialized_Aggregate (arg0, "");
    wn = WN_Lda (Pointer_Mtype, ST_ofst(st), st);
    *ty_idx = component_ty_idx == 0 ? ST_type(st) : component_ty_idx;
    return wn;
  } 
  else if (code == GS_TARGET_EXPR) {
    wn = WGEN_Expand_Expr(exp);
    Is_True(WN_operator(wn) == OPR_LDID,
	    ("WGEN_Array_Expr: OPR_LDID not found"));
    ST *st = WN_st(wn);
    if (component_ty_idx == 0)
      *ty_idx = ST_type(st);
    else {
      *ty_idx = component_ty_idx;
      if (TY_align(ST_type(st)) < TY_align(component_ty_idx))
	Set_TY_align(*ty_idx, TY_align(ST_type(st)));//pick more stringent align
    }
    Is_True(TY_kind(*ty_idx) == KIND_ARRAY,
	    ("WGEN_Array_Expr: ARRAY_REF base not of type KIND_ARRAY"));
    wn = WGEN_Compose_Address(st, component_offset, ST_type(st), field_id);
    return wn;
  }
#endif /* KEY */
  else if (code == GS_COMPOUND_EXPR) { // wgen
    wn = WGEN_Expand_Expr (gs_tree_operand (exp, 0), FALSE);
    if (wn && WN_has_side_effects(wn)) {
      wn = WN_CreateEval (wn);
      WGEN_Stmt_Append (wn, Get_Srcpos ());
    }
    return WGEN_Array_Expr(gs_tree_operand(exp, 1), ty_idx, component_ty_idx, 
    			   component_offset, field_id);
  }
  else if (code == GS_COND_EXPR) { // wgen for bug 9658
    WN *wn1, *wn2;
    wn = WGEN_Expand_Expr(gs_tree_operand(exp, 0));
    wn1 = WGEN_Array_Expr(gs_tree_operand(exp, 1), ty_idx, component_ty_idx, 
    			  component_offset, field_id);
    wn2 = WGEN_Array_Expr(gs_tree_operand(exp, 2), ty_idx, component_ty_idx, 
    			  component_offset, field_id);
    Set_PU_has_very_high_whirl(Get_Current_PU());
    return WN_CreateExp3(OPR_CSELECT, WN_rtype(wn1), MTYPE_V, wn, wn1, wn2);
  }
  else {
    Is_True(FALSE,
	    ("WGEN_Array_Expr: unsupported node for base of ARRAY_REF"));
    return NULL;
  }
}


#ifdef TARG_SL
/* For case: *p++(or --) op *p ... */
static BOOL Is_Special_Case (WN* wn)
{
  WN * body;
  WN * last;

  FmtAssert(WN_operator(wn) == OPR_ISTORE, ("WGEN_Stmt_Add: FYI"));

  body = WGEN_Stmt_Top ();

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
WGEN_Lhs_Of_Modify_Expr(gs_code_t assign_code,
		       gs_t lhs,
		       WN* lhs_retval,
		       bool need_result,
		       TY_IDX component_ty_idx, 
		       WN* component_offset,
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
  gs_code_t code = gs_tree_code (lhs);
  BOOL volt = FALSE;
#ifdef TARG_SL
  BOOL need_append = FALSE;
#endif

  if (rhs_wn != NULL) {
    WGEN_Set_ST_Addr_Saved (rhs_wn);
#ifdef TARG_SL
    need_append = WN_Need_Append_Intrinsic(rhs_wn);
#endif
  }

  TRACE_EXPAND_GS(lhs);
  switch (code) {
  case GS_COMPONENT_REF:
    {
      TY_IDX ty_idx0;

      gs_t arg0 = gs_tree_operand(lhs, 0);
      gs_t arg1 = gs_tree_operand(lhs, 1);
#ifdef GPLUSPLUS_FE
      // for g++ ensure that the WHIRL type for the enclosing structure has been
      // created in order to set the field id to a non zero value
      (void) Get_TY (gs_tree_type (arg0));
#endif /* GPLUSPLUS_FE */
      if (component_ty_idx == 0)
        ty_idx0 = Get_TY(gs_tree_type(lhs));
      else ty_idx0 = component_ty_idx;
      if (gs_decl_bit_field(arg1)) 
        is_bit_field = TRUE;
      WN* ofst;
      if (! is_bit_field) {
        ofst = WGEN_Expand_Field_Offset(arg1);
        ofst = WGEN_Compose_Offset(ofst, component_offset);
      }
      else {
        ofst = component_offset;
      }
#ifdef KEY    // bug 10422: check if the field is volatile, arg1 is FIELD_DECL
      if (gs_tree_this_volatile(arg1)) {
	Set_TY_is_volatile(ty_idx0);
	volt = TRUE;
      }
      else {
	Clear_TY_is_volatile(ty_idx0);
	volt = FALSE;
      }
#endif
#ifdef KEY
      FmtAssert (DECL_FIELD_ID(arg1) != 0,
                 ("WGEN_Lhs_Of_Modify_Expr: DECL_FIELD_ID used but not set"));
#endif
      wn = WGEN_Lhs_Of_Modify_Expr(assign_code, arg0, NULL, need_result, ty_idx0, 
				  ofst,
			          field_id + DECL_FIELD_ID(arg1), is_bit_field, 
				  rhs_wn, rhs_preg_num, is_realpart,
				  is_imagpart);
    }
    return wn;

  case GS_REALPART_EXPR:
    {
      gs_t arg0 = gs_tree_operand(lhs, 0);
      TY_IDX ty_idx0 = Get_TY(gs_tree_type(arg0));
      wn = WGEN_Lhs_Of_Modify_Expr(assign_code, arg0, NULL, need_result, ty_idx0,
				  component_offset, field_id, is_bit_field,
				  rhs_wn, rhs_preg_num, TRUE, FALSE);
    }
    return wn;

  case GS_IMAGPART_EXPR:
    {
      gs_t arg0 = gs_tree_operand(lhs, 0);
      TY_IDX ty_idx0 = Get_TY(gs_tree_type(arg0));
      wn = WGEN_Lhs_Of_Modify_Expr(assign_code, arg0, NULL, need_result, ty_idx0,
				  component_offset, field_id, is_bit_field,
				  rhs_wn, rhs_preg_num, FALSE, TRUE);
    }
    return wn;

#ifdef KEY
  case GS_TARGET_EXPR:	// bug 6907
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
  case GS_RESULT_DECL:
    {
      TY_IDX hi_ty_idx = Get_TY(gs_tree_type(lhs)); // type associated with field_id
      if (gs_tree_this_volatile(lhs)) {
        Set_TY_is_volatile(hi_ty_idx);
        volt = TRUE;
      }
      else {
	Clear_TY_is_volatile(hi_ty_idx);
	volt = FALSE;
      }
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

          TY_IDX ty_idx0 = Get_TY (gs_tree_type (actual_decl));
          return WGEN_Lhs_Of_Modify_Expr(assign_code, actual_decl, NULL, 
                                         need_result, ty_idx0,
                                         component_offset, field_id,
                                         is_bit_field, rhs_wn, rhs_preg_num,
                                         FALSE, FALSE);
        }

        st = Get_ST (lhs);
      }
#else
      st = Get_ST (lhs);
#endif

      if (ST_assigned_to_dedicated_preg (st)) {
        Set_TY_is_volatile(hi_ty_idx);
        volt = TRUE;
      }
      Is_True(! is_bit_field || field_id <= MAX_FIELD_ID,
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
        rhs_wn = WN_CreateLdid (OPR_LDID, rtype,
			        desc, rhs_preg_num, rhs_st,
			        desc_ty_idx, 0);
      }
      else {
        WN *result_wn;	// the result wn to be returned
        if (assign_code == GS_MODIFY_EXPR) {
	  if (is_realpart)
	    rhs_wn = WN_Binary(OPR_COMPLEX, rtype,
			       rhs_wn,
			       WN_Unary(OPR_IMAGPART,
				        Mtype_complex_to_real (rtype),
				        WGEN_CreateLdid(OPR_LDID, rtype, desc,
						      component_offset,
						      st, hi_ty_idx, field_id)));
	  else
	  if (is_imagpart)
	    rhs_wn = WN_Binary(OPR_COMPLEX, rtype,
			       WN_Unary(OPR_REALPART,
				        Mtype_complex_to_real (rtype),
				        WGEN_CreateLdid(OPR_LDID, rtype, desc,
						      component_offset,
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

        if (assign_code == GS_PREINCREMENT_EXPR ||
	    assign_code == GS_PREDECREMENT_EXPR) {
          wn = WGEN_CreateLdid (OPR_LDID, rtype, desc, 
			      component_offset,
			      st, hi_ty_idx, field_id);
          rhs_wn = WN_Binary(Operator_From_Tree [assign_code].opr,
		             rtype, wn, rhs_wn);
	  result_wn = rhs_wn;
        }
        else if (assign_code == GS_POSTINCREMENT_EXPR ||
	         assign_code == GS_POSTDECREMENT_EXPR) {
          result_wn = WGEN_CreateLdid (OPR_LDID, rtype, desc, 
				     component_offset,
				     st, hi_ty_idx, field_id);
        }
        else result_wn = rhs_wn;

	// OSP_382, do not store MTYPE_M into temp
        if (need_result && rtype != MTYPE_M &&
	    (volt ||
	     assign_code == GS_POSTINCREMENT_EXPR ||
	     assign_code == GS_POSTDECREMENT_EXPR)) { // save result in a preg
          result_in_temp = TRUE;
          result_preg_st = MTYPE_To_PREG(rtype);
          result_preg = Create_Preg(rtype, NULL);
          wn = WN_Stid(rtype, result_preg, result_preg_st, desc_ty_idx,
		       result_wn, 0);
          WGEN_Stmt_Append (wn, Get_Srcpos());
          result_wn = WN_Ldid(rtype, result_preg, result_preg_st, desc_ty_idx,
			      0);
        }

        if (assign_code == GS_POSTINCREMENT_EXPR ||
	    assign_code == GS_POSTDECREMENT_EXPR) {
          rhs_wn = WN_Binary(Operator_From_Tree [assign_code].opr,
		             rtype, result_wn, rhs_wn);
        }
        else rhs_wn = result_wn;

        // rhs_wn is now always the right-hand-side of the assignment
      }

      // the assignment
      if (!WGEN_Keep_Zero_Length_Structs &&
          desc == MTYPE_M               &&
          TY_size (hi_ty_idx) == 0) {
        // ignore zero length structs
      }
      else {
#ifdef KEY    // bug 10422: check if the field is volatile
	if (volt) 
	  Set_TY_is_volatile(hi_ty_idx);
#endif
        if (ST_base(st) != st) {
          ST* base_st = ST_base(st);
          Is_True(ST_ofst(st) == 0, ("TODO: ST_ofst is not 0"));
          WN* addr = WN_Ldid(Pointer_Mtype, 0, base_st, ST_type(base_st));
          wn = WGEN_CreateIstore(OPR_ISTORE, MTYPE_V, desc, component_offset,
                                   ST_type(base_st), rhs_wn, addr, field_id);
        }
        else {
          wn = WGEN_CreateStid (OPR_STID, MTYPE_V, desc, component_offset, st,
		        hi_ty_idx, rhs_wn, field_id);
        }
        WGEN_Stmt_Append(wn, Get_Srcpos());
#if defined(TARG_SL)
        if (need_append) {
          WN *ldid_wn;
          if (! result_in_temp)
            ldid_wn =  WGEN_CreateLdid(OPR_LDID, rtype, desc,
                                     component_offset, st, hi_ty_idx,
                                     field_id);
          else
            ldid_wn =  WN_Ldid(rtype, result_preg, result_preg_st, desc_ty_idx, 0);

          WFE_Stmt_Append_Extend_Intrinsic(wn, ldid_wn, Get_Srcpos());
        }
#endif
      }
      if (need_result) {
        if (! result_in_temp)
          wn = WGEN_CreateLdid(OPR_LDID, rtype, desc, 
			     component_offset, st, hi_ty_idx,
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

  case GS_INDIRECT_REF:
    {
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
      Is_True(! is_bit_field || field_id <= MAX_FIELD_ID,
              ("WGEN_Lhs_Of_Modify_Expr: field id for bit-field exceeds limit"));

      if ( gs_tree_code(op) == GS_CALL_EXPR && lhs_retval != NULL ) {
        // It's CALL_EXPR and the node has been expanded before expand the rhs.
	// The return address of the CALL_EXPR is stored in lhs_retval.
	// Refer WGEN_Expand_Expr : case GS_MODIFY_EXPR.
	// We must ensure the lhs_retval is LDID
	// Check TYPE and Operands?
	addr_wn = lhs_retval;
	FmtAssert(WN_operator(addr_wn) == OPR_LDID, 
                  ("Bad Operator for INDIRECT_REF-->CALL_EXPR. LDID expected."));
      }
      else {
        // Otherwise, we expand the lhs here.
        addr_wn = WGEN_Expand_Expr (gs_tree_operand (lhs, 0));
        if (WN_has_side_effects(addr_wn) &&
	    (need_result || 
	     assign_code == GS_PREINCREMENT_EXPR ||
	     assign_code == GS_PREDECREMENT_EXPR ||
	     assign_code == GS_POSTINCREMENT_EXPR ||
	     assign_code == GS_POSTDECREMENT_EXPR)) {
          ST       *preg_st;
          PREG_NUM  preg;
          TY_IDX    address_ty_idx = Get_TY (gs_tree_type (gs_tree_operand (lhs, 0)));
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
          WGEN_Set_ST_Addr_Saved (addr_wn);
#ifdef KEY
	  // Handle function calls for asm input-output constraints
	  // see torture test 990130-1.c
	  WN *body = WGEN_Stmt_Top();
	  if (body &&		// Do prepend only for asm's.  Bug 4732.
	      WN_last(body) &&
	      WN_operator(WN_last(body)) == OPR_ASM_STMT) {
	    WGEN_Stmt_Prepend_Last (wn, Get_Srcpos());
	  } else
#endif /* KEY */
          WGEN_Stmt_Append (wn, Get_Srcpos());
          addr_wn = WN_Ldid (Pointer_Mtype, preg, preg_st, address_ty_idx);
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

        if (assign_code == GS_MODIFY_EXPR) {
	  if (is_realpart)
	    rhs_wn = WN_Binary(OPR_COMPLEX, rtype,
			       rhs_wn,
			       WN_Unary(OPR_IMAGPART,
				        Mtype_complex_to_real (rtype),
				        WGEN_CreateIload(OPR_ILOAD, rtype, desc,
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
				        WGEN_CreateIload(OPR_ILOAD, rtype, desc,
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

        if (assign_code == GS_PREINCREMENT_EXPR ||
	    assign_code == GS_PREDECREMENT_EXPR) {
          wn = WGEN_CreateIload (OPR_ILOAD, rtype, desc, component_offset,
			       field_id != 0 ? hi_ty_idx : desc_ty_idx,
			       Make_Pointer_Type(hi_ty_idx, FALSE),
			       WN_COPY_Tree (addr_wn),
			       field_id);
          rhs_wn = WN_Binary(Operator_From_Tree [assign_code].opr,
                             rtype, wn, rhs_wn);
          result_wn = rhs_wn;
        }
        else if (assign_code == GS_POSTINCREMENT_EXPR ||
	         assign_code == GS_POSTDECREMENT_EXPR) {
	  result_wn = WGEN_CreateIload (OPR_ILOAD, rtype, desc, component_offset,
				      field_id != 0 ? hi_ty_idx : desc_ty_idx,
				      Make_Pointer_Type(hi_ty_idx, FALSE),
				      WN_COPY_Tree (addr_wn),
				      field_id);
        }
        else result_wn = rhs_wn;

	// OSP_382, do not store MTYPE_M into temp
        if (need_result && rtype != MTYPE_M &&
	    (volt ||
             assign_code == GS_POSTINCREMENT_EXPR ||
             assign_code == GS_POSTDECREMENT_EXPR)) { // save result in a preg
	  result_in_temp = TRUE;
          result_preg_st = MTYPE_To_PREG(rtype);
          result_preg = Create_Preg(rtype, NULL);
          wn = WN_Stid(rtype, result_preg, result_preg_st, desc_ty_idx,
		       result_wn, 0);
          WGEN_Stmt_Append (wn, Get_Srcpos());;
          result_wn = WN_Ldid(rtype, result_preg, result_preg_st, desc_ty_idx,
			      0);
        }

        if (assign_code == GS_POSTINCREMENT_EXPR ||
	    assign_code == GS_POSTDECREMENT_EXPR) {
          rhs_wn = WN_Binary(Operator_From_Tree [assign_code].opr,
                             rtype, result_wn, rhs_wn);
        }
        else rhs_wn = result_wn;

        // rhs_wn is now always the right-hand-side of the assignment
      }

      // the assignment
      if (!WGEN_Keep_Zero_Length_Structs &&
          desc == MTYPE_M               &&
          TY_size (hi_ty_idx) == 0) {
        // ignore zero length structs
        if (WN_has_side_effects (addr_wn)) {
	  wn = WN_CreateEval (addr_wn);
	  WGEN_Stmt_Append (wn, Get_Srcpos());
        }
        wn = NULL;
      }
      else {
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
        WN *first_formal = NULL;
        if (Current_Entry_WN() != NULL) {
            first_formal = WN_formal(Current_Entry_WN(), 0);
        }
        if (TY_return_in_mem(hi_ty_idx) &&
	    field_id == 0 &&
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
	    ("WGEN_Lhs_Of_Modify_Expr: INDIRECT_REF opnd0 is not POINTER_TYPE"));
	  FmtAssert(component_offset == NULL ||
                    (WN_operator(component_offset) == OPR_INTCONST && 
                     WN_const_val(component_offset) == 0),
		    ("WGEN_Lhs_Of_Modify_Expr: component_offset nonzero"));
	  TY_IDX tidx = Get_TY(ptr_type);
	  // Check object has no copy constructor.
	  FmtAssert(!WGEN_has_copy_constructor(type),
	      ("WGEN_Lhs_Of_Modify_Expr: object needs copy constructor"));
        }
        wn = WGEN_CreateIstore(OPR_ISTORE, MTYPE_V, desc, component_offset, 
			     Make_Pointer_Type (hi_ty_idx, FALSE),
			     rhs_wn, addr_wn, field_id);
#ifdef TARG_SL
        /* so far I only handle *p++=... cases, change this case to 
         *   *p = ... ;
         *   p++;
         * This is to make our compiler consistent with gcc. So far,
         * only POST(INC/DEC) differs from gcc.
         */
        gs_code_t post_inc_dec_code = gs_tree_code(gs_tree_operand(lhs, 0));
        if (((post_inc_dec_code == GS_POSTINCREMENT_EXPR)
          || (post_inc_dec_code == GS_POSTDECREMENT_EXPR)) && Is_Special_Case(wn))
        WGEN_Stmt_Prepend_Last(wn, Get_Srcpos());
        else 
#endif
        WGEN_Stmt_Append(wn, Get_Srcpos());
#if defined(TARG_SL)
        if (need_append) {
          WN *ldid_wn;
          if (! result_in_temp)
            ldid_wn = WGEN_CreateIload(OPR_ILOAD, rtype, desc, component_offset,
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
            wn = WGEN_CreateIload(OPR_ILOAD, rtype, desc, component_offset,
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

  case GS_ARRAY_REF:
    {
      TY_IDX elem_ty_idx;
      // generate the WHIRL array node
      WN *addr_wn = WGEN_Array_Expr(lhs, &elem_ty_idx, 0, NULL, 0);
      if (TY_is_volatile(elem_ty_idx))
        volt = TRUE;
      TY_IDX desc_ty_idx = component_ty_idx;
      if (desc_ty_idx == 0)
        desc_ty_idx = Get_TY (gs_tree_type(lhs));
      if (TY_is_volatile(desc_ty_idx)) {
        Clear_TY_is_volatile(desc_ty_idx);
        volt = TRUE;
      }
      Is_True(! is_bit_field || field_id <= MAX_FIELD_ID,
	      ("WGEN_Lhs_Of_Modify_Expr: field id for bit-field exceeds limit"));
      if (WN_has_side_effects(addr_wn) &&
          (need_result ||
           assign_code == GS_PREINCREMENT_EXPR ||
           assign_code == GS_PREDECREMENT_EXPR ||
           assign_code == GS_POSTINCREMENT_EXPR ||
	   assign_code == GS_POSTDECREMENT_EXPR)) {
        ST       *preg_st;
        PREG_NUM  preg;
        TY_IDX    address_ty_idx = Make_Pointer_Type(elem_ty_idx, FALSE);
        preg_st = MTYPE_To_PREG(Pointer_Mtype);
        preg    = Create_Preg (Pointer_Mtype, NULL);
        wn      = WN_Stid (Pointer_Mtype, preg, preg_st, address_ty_idx, addr_wn);
        WGEN_Set_ST_Addr_Saved (addr_wn);
        WGEN_Stmt_Append (wn, Get_Srcpos());
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

        if (assign_code == GS_MODIFY_EXPR) {
	  if (is_realpart)
	    rhs_wn = WN_Binary(OPR_COMPLEX, rtype,
			       rhs_wn,
			       WN_Unary(OPR_IMAGPART,
				        Mtype_complex_to_real (rtype),
				        WGEN_CreateIload(OPR_ILOAD, rtype, desc,
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
				        WGEN_CreateIload(OPR_ILOAD, rtype, desc,
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

        if (assign_code == GS_PREINCREMENT_EXPR ||
            assign_code == GS_PREDECREMENT_EXPR) {
          wn = WGEN_CreateIload (OPR_ILOAD, rtype, desc, component_offset,
                               field_id != 0 ? elem_ty_idx : desc_ty_idx,
                               Make_Pointer_Type(elem_ty_idx, FALSE),
                               WN_COPY_Tree (addr_wn),
                               field_id);
          rhs_wn = WN_Binary(Operator_From_Tree [assign_code].opr,
                             rtype, wn, rhs_wn);
	  result_wn = rhs_wn;
        }
        else if (assign_code == GS_POSTINCREMENT_EXPR ||
	         assign_code == GS_POSTDECREMENT_EXPR) {
          result_wn = WGEN_CreateIload (OPR_ILOAD, rtype, desc, component_offset,
				      field_id != 0 ? elem_ty_idx : desc_ty_idx,
				      Make_Pointer_Type(elem_ty_idx, FALSE),
				      WN_COPY_Tree (addr_wn),
				      field_id);
        }
        else result_wn = rhs_wn;

	// OSP_382, do not store MTYPE_M into temp
        if (need_result && rtype != MTYPE_M &&
	    (volt ||
             assign_code == GS_POSTINCREMENT_EXPR ||
	     assign_code == GS_POSTDECREMENT_EXPR)) { // save result in a preg
          result_in_temp = TRUE;
          result_preg_st = MTYPE_To_PREG(rtype);
          result_preg = Create_Preg(rtype, NULL);
          wn = WN_Stid(rtype, result_preg, result_preg_st, desc_ty_idx,
		       result_wn, 0);
          WGEN_Stmt_Append (wn, Get_Srcpos());;
          result_wn = WN_Ldid(rtype, result_preg, result_preg_st, desc_ty_idx,
			      0);
        }

        if (assign_code == GS_POSTINCREMENT_EXPR ||
            assign_code == GS_POSTDECREMENT_EXPR) {
          rhs_wn = WN_Binary(Operator_From_Tree [assign_code].opr,
                             rtype, result_wn, rhs_wn);
        }
        else rhs_wn = result_wn;

        // rhs_wn is now always the right-hand-side of the assignment
      }

      // the assignment
      if (!WGEN_Keep_Zero_Length_Structs &&
          desc == MTYPE_M               &&
          TY_size (elem_ty_idx) == 0) {
        // ignore zero length structs
        if (WN_has_side_effects (addr_wn)) {
          wn = WN_CreateEval (addr_wn);
          WGEN_Stmt_Append (wn, Get_Srcpos());
        }
        wn = NULL;
      }
      else {
        wn = WGEN_CreateIstore(OPR_ISTORE, MTYPE_V, desc, component_offset, 
			     Make_Pointer_Type(elem_ty_idx, FALSE), rhs_wn,
			     addr_wn, field_id);
        WGEN_Stmt_Append(wn, Get_Srcpos());
#ifdef TARG_SL
        if (need_append) {
          WN *iload;
          if (!result_in_temp)
            iload = WGEN_CreateIload (OPR_ILOAD, rtype, desc, component_offset,
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
	    wn = WGEN_CreateIload (OPR_ILOAD, rtype, desc, component_offset,
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
  case GS_MIN_EXPR:
  case GS_MAX_EXPR:
    {
      gs_t arg0 = gs_tree_operand(lhs, 0);
      gs_t arg1 = gs_tree_operand(lhs, 1);

      WN *then_block = WN_CreateBlock ();
      WN *else_block = WN_CreateBlock ();

      WGEN_Stmt_Push (then_block, wgen_stmk_if_then, Get_Srcpos());
      WN * wn1 = WGEN_Lhs_Of_Modify_Expr (assign_code, arg0, NULL, TRUE,
                                         component_ty_idx, component_offset,
                                         field_id, is_bit_field,
                                         rhs_wn, rhs_preg_num, is_realpart,
                                         is_imagpart);
      WGEN_Stmt_Pop (wgen_stmk_if_then);

      WGEN_Stmt_Push (else_block, wgen_stmk_if_else, Get_Srcpos());
      WN * wn2 = WGEN_Lhs_Of_Modify_Expr (assign_code, arg1, NULL, TRUE,
                                         component_ty_idx, component_offset,
                                         field_id, is_bit_field,
                                         rhs_wn, rhs_preg_num, is_realpart,
                                         is_imagpart);
      WGEN_Stmt_Pop (wgen_stmk_if_else);

      Is_True (wn1 && wn2,
               ("WGEN_Lhs_Of_Modify_Expr: null operands of MIN/MAX_EXPR?"));
      WN * wn0 = WN_Relational (code == GS_MIN_EXPR ? OPR_LE : OPR_GE,
                           Widen_Mtype(TY_mtype(Get_TY(gs_tree_type(lhs)))),
                           wn1, wn2);
      WN *if_stmt = WN_CreateIf (wn0, then_block, else_block);
      WGEN_Stmt_Append (if_stmt, Get_Srcpos());
    }
    break;
#endif

  case GS_COMPOUND_LITERAL_EXPR: // bug 10144
    {
#ifdef KEY // bug 14372
      TY_IDX hi_ty_idx = Get_TY(gs_tree_type(lhs)); // type associated with field id
#endif
      gs_t var = gs_decl_initial(gs_tree_operand(gs_tree_operand (lhs, 0), 0)); 
      st = WGEN_Generate_Temp_For_Initialized_Aggregate(var, "");

      // bug 10281: need to make a copy so any potential store will not 
      // 	overwrite the original
      ST *copy = New_ST(CURRENT_SYMTAB);
      ST_Init(copy, Save_Str(".cpfrominit"), CLASS_VAR, SCLASS_AUTO, 
      	      EXPORT_LOCAL, ST_type(st));
      WN *init_wn = WN_CreateLdid(OPR_LDID, MTYPE_M, MTYPE_M, 0, st, 
      				  ST_type(st));
      WGEN_Stmt_Append( WN_CreateStid(OPR_STID, MTYPE_V, MTYPE_M,
				      0, copy, ST_type(copy), init_wn),
			Get_Srcpos());
      // the assignment
      TY_IDX desc_ty_idx = component_ty_idx;
      TYPE_ID desc = is_bit_field ? MTYPE_BS : TY_mtype(desc_ty_idx);
      if (desc == MTYPE_UNKNOWN)
	desc = WN_rtype(rhs_wn); // is a scalar

      Is_True(component_offset == NULL || WN_operator(component_offset) == OPR_INTCONST,
              ("WGEN_Lhs_Of_Modify_Expr: GS_COMPOUND_LITERAL_EXPR: offset is not constant"));
      INT64 ofst_value = (component_offset == NULL) ? 0 : WN_const_val(component_offset);

#ifdef KEY // bug 14372
      wn = WN_Stid (desc, ST_ofst(copy) + ofst_value, copy,
		    hi_ty_idx, rhs_wn, field_id);
#else
      if (desc_ty_idx == 0)
        desc_ty_idx = MTYPE_TO_TY_array[desc];

      wn = WN_Stid (desc, ST_ofst(copy) + ofst_value, copy,
		    desc_ty_idx, rhs_wn, field_id);
#endif
      WGEN_Stmt_Append(wn, Get_Srcpos());
      if (need_result) // bug 10548
        wn = WN_CreateLdid(OPR_LDID, desc, desc,
                           ST_ofst(copy) + ofst_value, copy,
                           ST_type(copy));
      else
        wn = NULL;
    }
    break;

#ifdef KEY
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
    Fail_FmtAssertion ("WGEN_Lhs_Of_Modify_Expr: unhandled tree node in LHS of MODIFY_EXPR");
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

WN*
WGEN_Expand_Expr_With_Sequence_Point (gs_t exp, TYPE_ID mtype, WN* target_wn)
{
  WN *wn;

  if (mtype == MTYPE_V)
#ifdef KEY
    wn = WGEN_Expand_Expr (exp, FALSE, 0, 0, 0, 0, FALSE, FALSE, target_wn);
#else
    wn = WGEN_Expand_Expr (exp, FALSE);
#endif

  else {

    WN *comma_block      = WN_CreateBlock ();

    WGEN_Stmt_Push (comma_block, wgen_stmk_comma, Get_Srcpos ());
#ifdef KEY
    wn = WGEN_Expand_Expr (exp, TRUE, 0, 0, 0, 0, FALSE, FALSE, target_wn);
#else
    wn = WGEN_Expand_Expr (exp);
#endif

    // normalize bool expressions
    if (gs_tree_type (exp) == gs_boolean_type_node()) {
      if (WN_operator (wn) == OPR_LDID ||
          WN_operator (wn) == OPR_ILOAD) {
        WN *zero = WN_Intconst (WN_rtype (wn), 0);
        wn = WN_Relational (OPR_NE, MTYPE_I4, wn, zero);
      }
    }

    WGEN_Stmt_Pop (wgen_stmk_comma);
    if (WN_first (comma_block)) {
      if (wn)
	wn = WN_CreateComma (OPR_COMMA, Mtype_comparison (mtype), MTYPE_V,
			     comma_block, wn);
      else
	WGEN_Stmt_Append (comma_block, Get_Srcpos());
    }
    else
      WN_Delete (comma_block);
  }

  return wn;
} /* WGEN_Expand_Expr_With_Sequence_Point */

static void
emit_barrier (bool type, gs_t list, INT32 k)
{
  INT32  i;
  WN    *wn = WN_CreateBarrier (type, k);

  for (i = 0; i < k; i++) {
    gs_t exp = gs_tree_value (list);
    ST *st   = Get_ST (exp);
    WN_kid (wn, i) = WN_Lda (Pointer_Mtype, 0, st,
                             Make_Pointer_Type (ST_type (st), FALSE));
    list = gs_tree_chain (list);
  }

  WGEN_Stmt_Append (wn, Get_Srcpos());
} /* emit_barrier */

static WN *
emit_builtin_lock_test_and_set (INTRINSIC iopc, gs_t exp, INT32 k)
{
  WN        *wn;
  WN        *arg_wn;
  WN        *ikids [2];
  TYPE_ID    obj_mtype;
  TY_IDX     arg_ty_idx;
  TYPE_ID    arg_mtype;
  gs_t       list = gs_tree_operand (exp, 1);
  OPCODE     opc;

  obj_mtype  = TY_mtype (TY_pointed (Get_TY(gs_tree_type(gs_tree_value(list)))));
  arg_ty_idx = Get_TY(gs_tree_type(gs_tree_value(list)));
  arg_mtype  = TY_mtype (arg_ty_idx);
  arg_wn     = WGEN_Expand_Expr (gs_tree_value (list));
  arg_wn     = WN_CreateParm (arg_mtype, arg_wn, arg_ty_idx, WN_PARM_BY_VALUE);
  ikids [0]  = arg_wn;
  list       = gs_tree_chain (list);
  arg_ty_idx = Get_TY(gs_tree_type(gs_tree_value(list)));
  arg_mtype  = TY_mtype (arg_ty_idx);
  arg_wn     = WGEN_Expand_Expr (gs_tree_value (list));
  arg_wn     = WN_CreateParm (arg_mtype, arg_wn, arg_ty_idx, WN_PARM_BY_VALUE);
  ikids [1]  = arg_wn;
  list       = gs_tree_chain (list);

  if (obj_mtype == MTYPE_V) {
    obj_mtype = INTRN_Size_Mtype(iopc);
  }
  Is_True( (obj_mtype == MTYPE_I1 || obj_mtype == MTYPE_U1 ||
            obj_mtype == MTYPE_I2 || obj_mtype == MTYPE_U2 ||
            obj_mtype == MTYPE_I4 || obj_mtype == MTYPE_U4 ||
            obj_mtype == MTYPE_I8 || obj_mtype == MTYPE_U8 ),
           ("Unsupported object type in emit_builtin_lock_test_and_set") );

  emit_barrier (TRUE, list, k);

  opc = OPCODE_make_op(OPR_INTRINSIC_CALL, obj_mtype, MTYPE_V);
  wn = WN_Create_Intrinsic (opc, iopc, 2, ikids);
  WGEN_Stmt_Append (wn, Get_Srcpos());

  ST       *preg_st = MTYPE_To_PREG(obj_mtype);
  TY_IDX    preg_ty_idx = Be_Type_Tbl(obj_mtype);
  PREG_NUM  preg = Create_Preg (obj_mtype, NULL);

  wn = WN_Ldid (obj_mtype, -1, Return_Val_Preg, preg_ty_idx);
  wn = WN_Stid (obj_mtype, preg, preg_st, preg_ty_idx, wn),
  WGEN_Stmt_Append (wn, Get_Srcpos());

  emit_barrier (FALSE, list, k);

  wn = WN_Ldid (obj_mtype, preg, preg_st, preg_ty_idx);

  return wn;
} /* emit_builtin_lock_test_and_set */

static void
emit_builtin_lock_release (INTRINSIC iopc, gs_t exp, INT32 k)
{
  WN        *wn;
  WN        *arg_wn;
  WN        *ikids [1];
  TYPE_ID    obj_mtype;
  TY_IDX     arg_ty_idx;
  TYPE_ID    arg_mtype;
  gs_t       list = gs_tree_operand (exp, 1);
  OPCODE     opc;

  obj_mtype  = TY_mtype (TY_pointed (Get_TY(gs_tree_type(gs_tree_value(list)))));
  arg_ty_idx = Get_TY(gs_tree_type(gs_tree_value(list)));
  arg_mtype  = TY_mtype (arg_ty_idx);
  arg_wn     = WGEN_Expand_Expr (gs_tree_value (list));
  arg_wn     = WN_CreateParm (arg_mtype, arg_wn, arg_ty_idx, WN_PARM_BY_VALUE);
  ikids [0]  = arg_wn;
  list       = gs_tree_chain (list);

  if (obj_mtype == MTYPE_V) {
    // in case first arg is void*
    obj_mtype = INTRN_Size_Mtype(iopc);
  }
  Is_True( (obj_mtype == MTYPE_I1 || obj_mtype == MTYPE_U1 ||
            obj_mtype == MTYPE_I2 || obj_mtype == MTYPE_U2 ||
            obj_mtype == MTYPE_I4 || obj_mtype == MTYPE_U4 ||
            obj_mtype == MTYPE_I8 || obj_mtype == MTYPE_U8 ),
           ("Unsupported object type in emit_builtin_lock_test_and_set") );

  emit_barrier (TRUE, list, k);

  opc = OPCODE_make_op(OPR_INTRINSIC_CALL, obj_mtype, MTYPE_V);
  wn = WN_Create_Intrinsic (opc, iopc, 1, ikids);
  WGEN_Stmt_Append (wn, Get_Srcpos());
} /* emit_builtin_lock_release */

static WN *
emit_builtin_compare_and_swap (INTRINSIC  iopc, gs_t exp, INT32 k)
{
  WN        *wn;
  WN        *arg_wn;
  WN        *ikids [3];
  TYPE_ID    obj_mtype;
  TY_IDX     arg_ty_idx;
  TYPE_ID    arg_mtype;
  gs_t       list = gs_tree_operand (exp, 1);
  OPCODE     opc;

  obj_mtype  = TY_mtype (TY_pointed (Get_TY(gs_tree_type(gs_tree_value(list)))));
  arg_ty_idx = Get_TY(gs_tree_type(gs_tree_value(list)));
  arg_mtype  = TY_mtype (arg_ty_idx);
  arg_wn     = WGEN_Expand_Expr (gs_tree_value (list));
  arg_wn     = WN_CreateParm (arg_mtype, arg_wn, arg_ty_idx, WN_PARM_BY_VALUE);
  ikids [0]  = arg_wn;
  list       = gs_tree_chain (list);
  arg_ty_idx = Get_TY(gs_tree_type(gs_tree_value(list)));
  arg_mtype  = TY_mtype (arg_ty_idx);
  arg_wn     = WGEN_Expand_Expr (gs_tree_value (list));
  arg_wn     = WN_CreateParm (arg_mtype, arg_wn, arg_ty_idx, WN_PARM_BY_VALUE);
  ikids [1]  = arg_wn;
  list       = gs_tree_chain (list);
  arg_ty_idx = Get_TY(gs_tree_type(gs_tree_value(list)));
  arg_mtype  = TY_mtype (arg_ty_idx);
  arg_wn     = WGEN_Expand_Expr (gs_tree_value (list));
  arg_wn     = WN_CreateParm (arg_mtype, arg_wn, arg_ty_idx, WN_PARM_BY_VALUE);
  ikids [2]  = arg_wn;
  list       = gs_tree_chain (list);

  if (iopc == INTRN_BOOL_COMPARE_AND_SWAP_I1 ||
      iopc == INTRN_BOOL_COMPARE_AND_SWAP_I2 ||
      iopc == INTRN_BOOL_COMPARE_AND_SWAP_I4 ||
      iopc == INTRN_BOOL_COMPARE_AND_SWAP_I8) {
    obj_mtype = MTYPE_U1;
  }
  if (obj_mtype == MTYPE_V) {
    // in case first arg is void*
    obj_mtype = INTRN_Size_Mtype(iopc);
  }
  Is_True( (obj_mtype == MTYPE_I1 || obj_mtype == MTYPE_U1 ||
            obj_mtype == MTYPE_I2 || obj_mtype == MTYPE_U2 ||
            obj_mtype == MTYPE_I4 || obj_mtype == MTYPE_U4 ||
            obj_mtype == MTYPE_I8 || obj_mtype == MTYPE_U8 ),
           ("Unsupported object type in emit_builtin_lock_test_and_set") );

  emit_barrier (TRUE, list, k);

  opc = OPCODE_make_op(OPR_INTRINSIC_CALL, obj_mtype, MTYPE_V);
  wn = WN_Create_Intrinsic (opc, iopc, 3, ikids);
  WGEN_Stmt_Append (wn, Get_Srcpos());

  ST       *preg_st = MTYPE_To_PREG(obj_mtype);
  TY_IDX    preg_ty_idx = Be_Type_Tbl(obj_mtype);
  PREG_NUM  preg = Create_Preg (obj_mtype, NULL);

  wn = WN_Ldid (obj_mtype, -1, Return_Val_Preg, preg_ty_idx);
  wn = WN_Stid (obj_mtype, preg, preg_st, preg_ty_idx, wn),
  WGEN_Stmt_Append (wn, Get_Srcpos());

  emit_barrier (FALSE, list, k);

  wn = WN_Ldid (obj_mtype, preg, preg_st, preg_ty_idx);

  return wn;
} /* emit_builtin_compare_and_swap */

/*
 * emit_builtin_sync_fetch_op
 *   for FETCH_AND_OP (ADD, SUB, AND, OR, XOR, NAND)
 *   for OP_AND_FETCH (ADD, SUB, AND, OR, XOR, NAND)
 * It's necessary for GNU 4.2 FE
 */
static WN *
emit_builtin_sync_fetch_op (INTRINSIC iopc, gs_t exp, INT32 k)
{
  WN        *wn;
  WN        *arg_wn;
  WN        *ikids [2];
  TYPE_ID    obj_mtype;
  TY_IDX     arg_ty_idx;
  TYPE_ID    arg_mtype;
  gs_t       list = gs_tree_operand (exp, 1);
  OPCODE     opc;

  obj_mtype  = TY_mtype (TY_pointed (Get_TY(gs_tree_type(gs_tree_value(list)))));
  arg_ty_idx = Get_TY(gs_tree_type(gs_tree_value(list)));
  arg_mtype  = TY_mtype (arg_ty_idx);
  arg_wn     = WGEN_Expand_Expr (gs_tree_value (list));
  arg_wn     = WN_CreateParm (arg_mtype, arg_wn, arg_ty_idx, WN_PARM_BY_VALUE);
  ikids [0]  = arg_wn;
  list       = gs_tree_chain (list);
  arg_ty_idx = Get_TY(gs_tree_type(gs_tree_value(list)));
  arg_mtype  = TY_mtype (arg_ty_idx);
  arg_wn     = WGEN_Expand_Expr (gs_tree_value (list));
  arg_wn     = WN_CreateParm (arg_mtype, arg_wn, arg_ty_idx, WN_PARM_BY_VALUE);
  ikids [1]  = arg_wn;
  list       = gs_tree_chain (list);

  if ( obj_mtype == MTYPE_V ) {
    // in case first arg is void*
    obj_mtype = INTRN_Size_Mtype(iopc);
  }

  Is_True( (obj_mtype == MTYPE_I1 || obj_mtype == MTYPE_U1 ||
            obj_mtype == MTYPE_I2 || obj_mtype == MTYPE_U2 ||
            obj_mtype == MTYPE_I4 || obj_mtype == MTYPE_U4 ||
            obj_mtype == MTYPE_I8 || obj_mtype == MTYPE_U8 ),
           ("Unsupported object type in emit_builtin_sync_fetch_op") );

  emit_barrier (TRUE, list, k);
  
  opc = OPCODE_make_op(OPR_INTRINSIC_CALL, obj_mtype, MTYPE_V);
  wn = WN_Create_Intrinsic (opc, iopc, 2, ikids);
  WGEN_Stmt_Append (wn, Get_Srcpos());

  ST       *preg_st = MTYPE_To_PREG(obj_mtype);
  TY_IDX    preg_ty_idx = Be_Type_Tbl(obj_mtype);
  PREG_NUM  preg = Create_Preg (obj_mtype, NULL);

  wn = WN_Ldid (obj_mtype, -1, Return_Val_Preg, preg_ty_idx);
  wn = WN_Stid (obj_mtype, preg, preg_st, preg_ty_idx, wn),
  WGEN_Stmt_Append (wn, Get_Srcpos());

  emit_barrier (FALSE, list, k);

  wn = WN_Ldid (obj_mtype, preg, preg_st, preg_ty_idx);

  return wn;
} /* emit_builtin_sync_fetch_op */

static void
emit_builtin_synchronize (gs_t exp, INT32 k)
{
  WN *wn;
  gs_t list = gs_tree_operand (exp, 1);
  emit_barrier (TRUE,  list, k);
  wn = WN_Create_Intrinsic (OPC_VINTRINSIC_CALL, INTRN_SYNCHRONIZE, 0, NULL);
  WGEN_Stmt_Append (wn, Get_Srcpos());
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

// Auxiliary function for WGEN_Expand_Expr, return the address of
// a tree operand.  (Used for ADDR_EXPR.)
WN*
WGEN_Address_Of(gs_t arg0)
{
  gs_code_t code0 = gs_tree_code (arg0);
  ST *st = 0;
  WN* wn = 0;
  WN* wn0;
  WN* wn1;
  TY_IDX ty_idx;

  switch (code0) {
#ifdef KEY
  case GS_RESULT_DECL:	// bug 3878
#endif
  case GS_VAR_DECL:
  case GS_PARM_DECL:
  case GS_FUNCTION_DECL:
    {
      st = Get_ST (arg0);
      gs_t alias_target = gs_decl_alias_target(arg0);
      ST *real_st = (alias_target && DECL_ST(alias_target))? DECL_ST(alias_target):st;
      ty_idx = ST_type (st);
#ifdef KEY
      // Arg0 is the virtual function table (vtable) for a class.  Initialize
      // the table.
      if (code0 == GS_VAR_DECL) {
	if (gs_decl_initial(arg0) &&
	    (gs_decl_virtual_p(arg0) ||
	      (/* bug 279 */ gs_decl_tinfo_p(arg0) /* typeinfo ? */ &&
	        /* make sure it is not an NTBS name */
	        gs_tree_code(gs_decl_initial(arg0)) != GS_STRING_CST)) &&
	    !gs_decl_external(arg0)) {
	  gs_t init = gs_decl_initial(arg0);
	  if (gs_tree_code(init) != GS_ERROR_MARK) {
	    FmtAssert (gs_tree_code(init) == GS_CONSTRUCTOR,
		       ("Unexpected initializer for virtual table"));
	    WGEN_Initialize_Decl(arg0);
	  }
	}
      }

      if (code0 == GS_VAR_DECL && gs_decl_value_expr(arg0)) {
        wn = WGEN_Address_Of (gs_decl_value_expr(arg0));
        break;
      }
#endif
      // for VLAs, use the base_st instead of st
      if (code0 == GS_VAR_DECL &&
          real_st != ST_base(st)) {
        FmtAssert (ST_ofst (st) == 0,
                   ("Variable Length Arrays within struct not currently implemented"));
        wn = WN_Ldid (Pointer_Mtype, 0, ST_base(st), ST_type(ST_base(st)));
      }
      else
        if (!WGEN_Keep_Zero_Length_Structs &&
            code0 == GS_PARM_DECL            &&
            TY_mtype (ty_idx) == MTYPE_M  &&
            TY_size (ty_idx) == 0) {
          // taking address of zero length struct passed as parameter
          DevWarn ("taking address of zero length struct %s at line %d",
                   ST_name (st), lineno);
          wn = WN_Intconst (Pointer_Mtype, 0);
        }
        else
          wn = WN_Lda (Pointer_Mtype, ST_ofst(st), st);

#ifdef KEY
      if (code0 == GS_FUNCTION_DECL) {
	PU &pu = Pu_Table[ST_pu(st)];
	if (PU_is_nested_func(pu))
	  Set_PU_need_trampoline(pu);
      }
#endif
    }
    break;

  case GS_INDIRECT_REF:
    wn = WGEN_Expand_Expr (gs_tree_operand(arg0, 0));
    break;

  case GS_STRING_CST:
    {
      TCON tcon;
      tcon = Host_To_Targ_String (MTYPE_STRING,
                                  const_cast<char*>(gs_tree_string_pointer(arg0)),
                                  gs_tree_string_length(arg0));
      ty_idx = Get_TY(gs_tree_type(arg0));
      st = New_Const_Sym (Enter_tcon (tcon), ty_idx);
      wn = WN_Lda (Pointer_Mtype, ST_ofst(st), st);
      TREE_STRING_ST (arg0) = st;
    }
    break;

  case GS_CONSTRUCTOR:
    {
      st = WGEN_Generate_Temp_For_Initialized_Aggregate (arg0, "");
      wn = WN_Lda (Pointer_Mtype, ST_ofst(st), st);
    }
    break;

  case GS_LABEL_DECL:
    {
      DevWarn ("taking address of a label at line %d", lineno);
      LABEL_IDX label_idx = WGEN_Get_LABEL (arg0, FALSE);
      wn = WN_LdaLabel (Pointer_Mtype, label_idx);
      Set_LABEL_addr_saved (label_idx);
#ifdef KEY
      // Bugs 1056 &  1227 - As a quality of implementation issue, we
      // should not prevent inlining of function explicitly marked
      // static inline just because a label therein had its address
      // taken.
      if ( ST_export (Get_Current_PU_ST()) != EXPORT_LOCAL)
#endif
        Set_PU_no_inline (Get_Current_PU ());
    }
    break;

  case GS_TARGET_EXPR:
    {
      WGEN_Expand_Expr (arg0);
      st = Get_ST (gs_tree_operand(arg0, 0));
      wn = WN_Lda (Pointer_Mtype, ST_ofst(st), st);
    }
    break;

  case GS_COMPOUND_EXPR:
    {
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
      st = WN_st (WGEN_Expand_Expr (arg0));
#endif
      wn = WN_Lda (Pointer_Mtype,  ST_ofst (st), st);
    }
    break;

  case GS_NOP_EXPR:
    {
      wn = WGEN_Address_Of(gs_tree_operand(arg0, 0));
    }
    break;

  case GS_MIN_EXPR:
  case GS_MAX_EXPR:
    {
      // &(a <? b) or &(a >? b)
      gs_t op0 = gs_tree_operand(arg0, 0);
      gs_t op1 = gs_tree_operand(arg0, 1);
      WN* a = WGEN_Expand_Expr(op0);
      WN* b = WGEN_Expand_Expr(op1);
      FmtAssert(!WN_has_side_effects(a) && !WN_has_side_effects(b),
                ("Addr of MIN/MAX_EXPR with side effects not yet supported"));

      TY_IDX  ptr_ty    = Make_Pointer_Type (Get_TY(gs_tree_type(op0)), FALSE);
      TYPE_ID ptr_mtype = TY_mtype(ptr_ty);
      TY_IDX  arg_ty    = Get_TY(gs_tree_type(gs_tree_operand(arg0, 0)));
      TYPE_ID arg_mtype = TY_mtype(arg_ty);

      WN* aptr = WGEN_Address_Of(op0);
      WN* bptr = WGEN_Address_Of(op1);
      wn = WN_Select(Widen_Mtype(ptr_mtype),
                     WN_Relational(code0 == GS_MIN_EXPR ? OPR_LT : OPR_GT,
                                   Widen_Mtype(arg_mtype),
                                   a, b),
                     aptr, bptr);
      Set_PU_has_very_high_whirl (Get_Current_PU ());
    }
    break;

  case GS_COMPONENT_REF:
    {
      wn = WGEN_Expand_Expr (arg0);
      ty_idx = Get_TY(gs_tree_type(arg0));
      WN *comma = NULL;
      if (WN_operator(wn) == OPR_COMMA) { // bug 11877
	comma = wn;
	wn = WN_kid1(wn);
      }
      if (WN_operator (wn) == OPR_LDID) {
        WN_set_operator (wn, OPR_LDA);
	WN_set_rtype(wn, Pointer_Mtype);
        WN_set_desc (wn, MTYPE_V);
	WN_set_ty (wn, Make_Pointer_Type(WN_ty(wn))); // bug 10098, bug 10352
      }
      else
      if (WN_operator (wn) == OPR_ILOAD) {
        wn0 = WN_kid0 (wn);
        wn1 = WN_Intconst (Pointer_Mtype, WN_offset (wn));
        wn  = WN_Binary (OPR_ADD, Pointer_Mtype, wn0, wn1);
      }
      else
        Fail_FmtAssertion ("WGEN_Address_Of has unhandled %s",
                           gs_code_name(code0));
      if (comma) {
	WN_set_rtype(comma, WN_rtype(wn));
	wn = comma;
      }
    }
    break;

#ifdef KEY // bug 3228
  case GS_ARRAY_REF:
    wn = WGEN_Expand_Expr (arg0);
    if (WN_operator(wn) == OPR_ILOAD) // bug 10105
      wn = WN_kid0(wn);
    ty_idx = Get_TY(gs_tree_type(arg0));
    break;
#endif

#ifdef KEY
    case GS_COMPOUND_LITERAL_EXPR:
    {
      arg0 = gs_decl_initial (gs_tree_operand (gs_tree_operand (arg0, 0), 0));
      st = WGEN_Generate_Temp_For_Initialized_Aggregate (arg0, "");

      if (CURRENT_SYMTAB == GLOBAL_SYMTAB) 
	wn = WN_Lda (Pointer_Mtype, ST_ofst(st), st); // bug 10507
      else {
	// bug 10281: need to make a copy so any potential store will not 
	// 	overwrite the original
	ST *copy = New_ST(CURRENT_SYMTAB);
	ST_Init(copy, Save_Str(".cpfrominit"), CLASS_VAR, SCLASS_AUTO, 
		EXPORT_LOCAL, ST_type(st));
	WN *init_wn = WN_CreateLdid(OPR_LDID, MTYPE_M, MTYPE_M, 0, st, 
				    ST_type(st));
	WGEN_Stmt_Append( WN_CreateStid(OPR_STID, MTYPE_V, MTYPE_M,
					0, copy, ST_type(copy), init_wn),
			  Get_Srcpos());

	wn = WN_Lda (Pointer_Mtype, ST_ofst(copy), copy);
      }
    }
    break;

    // bug 2399
    case GS_SAVE_EXPR:
    {
      st = WN_st (WGEN_Expand_Expr (arg0));
      wn = WN_Lda (Pointer_Mtype,  ST_ofst (st), st);
    }
    break;

    // bug 5532, 5609
    case GS_REALPART_EXPR:
    {
      wn = WGEN_Expand_Expr (gs_tree_operand (arg0, 0));
      if (WN_operator (wn) == OPR_ILOAD)
        wn = WN_kid0 (wn);
      else if (WN_operator (wn) == OPR_LDID)
	wn = WN_Lda (Pointer_Mtype, WN_offset(wn), WN_st(wn));
      else Fail_FmtAssertion ("WGEN_Address_Of: NYI for REALPART_EXPR");
    }
    break;

    case GS_IMAGPART_EXPR:
    {
      wn = WGEN_Expand_Expr (gs_tree_operand (arg0, 0));
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
	    Fail_FmtAssertion ("WGEN_Address_Of: Unexpected rtype in IMAGPART_EXPR");
	}
	INT ofst;
	if (imag_mtype == MTYPE_F10)
	{
#ifdef TARG_X8664
	  if (Is_Target_32bit()) ofst = 12; else
#endif // TARG_X8664
	  ofst = 16;
	}
        else if (imag_mtype == MTYPE_FQ)
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
      else Fail_FmtAssertion ("WGEN_Address_Of: NYI for IMAGPART_EXPR");
    }
    break;

    case GS_BASELINK: // bug 11167
      wn = WGEN_Address_Of (gs_baselink_functions (arg0));
      break;

    case GS_CONST_DECL:  // bug 14204
      st = WGEN_Generate_Temp_For_Initialized_Aggregate
                                  (gs_decl_initial (arg0), "");
      wn = WN_Lda (Pointer_Mtype, ST_ofst(st), st);
      break;

    case GS_VA_ARG_EXPR:
      wn = WGEN_Expand_Expr (arg0);
      Is_True( WN_operator(wn) == OPR_ILOAD, 
                ("expand VA_ARG_EXPR does not return OPR_ILOAD") );
      if (WN_operator(wn) == OPR_ILOAD) {
        wn0 = WN_kid0 (wn);
        wn1 = WN_Intconst (Pointer_Mtype, WN_offset (wn));
        wn  = WN_Binary (OPR_ADD, Pointer_Mtype, wn0, wn1);
      }
      break;

#endif

  default:
    {
      Fail_FmtAssertion ("WGEN_Address_Of: Unexpected operand %s",
                         gs_code_name(code0));
    }
    break;
  }

  FmtAssert(wn != 0, ("WGEN_Address_Of: null WHIRL tree for %s",
                      gs_code_name(code0)));
  return wn;
}

#ifdef TARG_X8664
/* expand a VA_ARG_EXPR node for scalar type according to X86-64 ABI and 
 * return the WHIRL node that represents the address to be dereferenced;
 * 'twice' is true is loading two consecutive parameters of the same type
 * because they belong to a struct; currently, twice is TRUE only if isfloat
 * is FALSE */
static WN *WGEN_x8664_va_arg(WN *ap_wn, BOOL isfloat, TY_IDX ty_idx, BOOL twice)
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
  WGEN_Stmt_Append (wn, Get_Srcpos ());

  ST *arg_temp_st = Gen_Temp_Symbol(Make_Pointer_Type(ty_idx), ".va_arg");
#ifdef FE_GNU_4_2_0
  WGEN_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, arg_temp_st);
#endif
  /* compute reg_save_area+gp_offset/fp_offset and store to arg_temp_st */
  wn0 = WN_Iload(MTYPE_I4, !isfloat ? 0 : 4, MTYPE_To_TY(MTYPE_I4), 
      		 WN_CopyNode(ap_wn));
  wn1 = WN_Iload(Pointer_Mtype, 16, MTYPE_To_TY(Pointer_Mtype), 
		 WN_CopyNode(ap_wn));
  wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
  wn = WN_Stid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx), wn);
  WGEN_Stmt_Append (wn, Get_Srcpos ());

  /* increment gp_offset by 8 or fp_offset by 16 */
  wn0 = WN_Iload(MTYPE_I4, !isfloat ? 0 : 4, MTYPE_To_TY(MTYPE_I4), 
      		 WN_CopyNode(ap_wn));
  wn1 = WN_Intconst(MTYPE_I4, (!isfloat ? 8 : 16) * ((INT)twice+1));
  wn = WN_Binary(OPR_ADD, MTYPE_I4, wn0, wn1);
  wn = WN_Istore(MTYPE_I4, !isfloat ? 0 : 4, 
		 Make_Pointer_Type(MTYPE_To_TY(MTYPE_I4)), 
		 WN_CopyNode(ap_wn), wn);
  WGEN_Stmt_Append (wn, Get_Srcpos ());

  /* branch around next part */
  LABEL_IDX lab2;
  New_LABEL (CURRENT_SYMTAB, lab2);
  WN *lab2_wn = WN_CreateLabel ((ST_IDX) 0, lab2, 0, NULL);
  wn = WN_CreateGoto(lab2);
  WGEN_Stmt_Append (wn, Get_Srcpos ());

  WGEN_Stmt_Append (lab1_wn, Get_Srcpos ());

  /* store overflow_arg_area pointer to arg_temp_st */
  wn0 = WN_Iload(Pointer_Mtype, 8, Make_Pointer_Type(ty_idx), 
		 WN_CopyNode(ap_wn));
  wn = WN_Stid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx), wn0);
  WGEN_Stmt_Append (wn, Get_Srcpos ());

  /* increment overflow_arg_area pointer by 8 */
  wn0 = WN_Iload(Pointer_Mtype, 8, MTYPE_To_TY(Pointer_Mtype), 
		 WN_CopyNode(ap_wn));
  wn1 = WN_Intconst(MTYPE_U8, twice ? 16 : 8);
  wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
  wn = WN_Istore(Pointer_Mtype, 8,Make_Pointer_Type(MTYPE_To_TY(Pointer_Mtype)),
		 WN_CopyNode(ap_wn), wn);
  WGEN_Stmt_Append (wn, Get_Srcpos ());

  WGEN_Stmt_Append (lab2_wn, Get_Srcpos ());

  return WN_Ldid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx));
}

/* expand a VA_ARG_EXPR node for struct type being passed in 2 different classes
 * of registers, according to X86-64 ABI and return the WHIRL node that 
 * represents the address to be dereferenced; this requires allocating a
 * temporary for assembling the struct if passed in registers; isfloat0 is 
 * for the first 8-byte and isfloat1 is for the second 8-byte  */
static WN *WGEN_x8664_va_arg_2_mixed(WN *ap_wn, BOOL isfloat0, BOOL isfloat1, 
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
  WGEN_Stmt_Append (wn, Get_Srcpos ());
  /* compare fp_offset with 176 */
  wn0 = WN_Iload(MTYPE_I4, 4, MTYPE_To_TY(MTYPE_I4), WN_CopyNode(ap_wn));
  wn1 = WN_Intconst(MTYPE_I4, 176);
  wn = WN_Relational(OPR_GE, MTYPE_I4, wn0, wn1);
  wn = WN_CreateTruebr(lab1, wn);
  WGEN_Stmt_Append (wn, Get_Srcpos ());

  /* allocate a temporary location to assemble the structure value */
  ST *struct_temp_st = Gen_Temp_Symbol(ty_idx, ".va_arg_struct");
#ifdef FE_GNU_4_2_0
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
  WGEN_Stmt_Append (wn, Get_Srcpos ());
  /* compute reg_save_area+fp_offset and store dereferenced value to 
   * struct_temp_st */
  wn0 = WN_Iload(MTYPE_I4, 4, MTYPE_To_TY(MTYPE_I4), WN_CopyNode(ap_wn));
  wn1 = WN_Iload(Pointer_Mtype, 16, MTYPE_To_TY(Pointer_Mtype), 
		 WN_CopyNode(ap_wn));
  wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
  wn = WN_Iload(MTYPE_F8, 0, MTYPE_To_TY(MTYPE_F8), wn);
  wn = WN_Stid(MTYPE_F8, isfloat0 ? 0 : 8, struct_temp_st, 
	       MTYPE_To_TY(MTYPE_F8), wn);
  WGEN_Stmt_Append (wn, Get_Srcpos ());

  /* increment gp_offset by 8 */
  wn0 = WN_Iload(MTYPE_I4, 0, MTYPE_To_TY(MTYPE_I4), WN_CopyNode(ap_wn));
  wn1 = WN_Intconst(MTYPE_I4, 8);
  wn = WN_Binary(OPR_ADD, MTYPE_I4, wn0, wn1);
  wn = WN_Istore(MTYPE_I4, 0, Make_Pointer_Type(MTYPE_To_TY(MTYPE_I4)), 
		 WN_CopyNode(ap_wn), wn);
  WGEN_Stmt_Append (wn, Get_Srcpos ());
  /* increment fp_offset by 16 */
  wn0 = WN_Iload(MTYPE_I4, 4, MTYPE_To_TY(MTYPE_I4), WN_CopyNode(ap_wn));
  wn1 = WN_Intconst(MTYPE_I4, 16);
  wn = WN_Binary(OPR_ADD, MTYPE_I4, wn0, wn1);
  wn = WN_Istore(MTYPE_I4, 4, Make_Pointer_Type(MTYPE_To_TY(MTYPE_I4)), 
		 WN_CopyNode(ap_wn), wn);
  WGEN_Stmt_Append (wn, Get_Srcpos ());

  /* put the address of struct_temp_st in arg_temp_st */
  ST *arg_temp_st = Gen_Temp_Symbol(Make_Pointer_Type(ty_idx), ".va_arg");
#ifdef FE_GNU_4_2_0
  WGEN_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, arg_temp_st);
#endif
  wn = WN_Lda(Pointer_Mtype, 0, struct_temp_st, 0);
  Set_ST_addr_saved(struct_temp_st);
  wn = WN_Stid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx), wn);
  WGEN_Stmt_Append (wn, Get_Srcpos ());

  /* branch around next part */
  LABEL_IDX lab2;
  New_LABEL (CURRENT_SYMTAB, lab2);
  WN *lab2_wn = WN_CreateLabel ((ST_IDX) 0, lab2, 0, NULL);
  wn = WN_CreateGoto(lab2);
  WGEN_Stmt_Append (wn, Get_Srcpos ());

  WGEN_Stmt_Append (lab1_wn, Get_Srcpos ());

  /* store overflow_arg_area pointer to arg_temp_st */
  wn0 = WN_Iload(Pointer_Mtype, 8, Make_Pointer_Type(ty_idx), 
		 WN_CopyNode(ap_wn));
  wn = WN_Stid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx), wn0);
  WGEN_Stmt_Append (wn, Get_Srcpos ());

  /* increment overflow_arg_area pointer by 16 */
  wn0 = WN_Iload(Pointer_Mtype, 8, MTYPE_To_TY(Pointer_Mtype), 
		 WN_CopyNode(ap_wn));
  wn1 = WN_Intconst(MTYPE_U8, 16);
  wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
  wn = WN_Istore(Pointer_Mtype, 8,Make_Pointer_Type(MTYPE_To_TY(Pointer_Mtype)),
		 WN_CopyNode(ap_wn), wn);
  WGEN_Stmt_Append (wn, Get_Srcpos ());

  WGEN_Stmt_Append (lab2_wn, Get_Srcpos ());

  return WN_Ldid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx));
}

/* expand a VA_ARG_EXPR node for struct type being passed in 2 float
 * registers, according to X86-64 ABI and return the WHIRL node that 
 * represents the address to be dereferenced; this requires allocating a
 * temporary for assembling the struct if passed in registers, because each
 * float register is saved into 128 bit locations */
static WN *WGEN_x8664_va_arg_2_float(WN *ap_wn, TY_IDX ty_idx)
{
  LABEL_IDX lab1;
  New_LABEL (CURRENT_SYMTAB, lab1);
  WN *lab1_wn = WN_CreateLabel ((ST_IDX) 0, lab1, 0, NULL);
  /* compare fp_offset with 160 (176 - 16) */
  WN *wn0 = WN_Iload(MTYPE_I4, 4, MTYPE_To_TY(MTYPE_I4), WN_CopyNode(ap_wn));
  WN *wn1 = WN_Intconst(MTYPE_I4, 160);
  WN *wn = WN_Relational(OPR_GE, MTYPE_I4, wn0, wn1);
  wn = WN_CreateTruebr(lab1, wn);
  WGEN_Stmt_Append (wn, Get_Srcpos ());

  /* allocate a temporary location to assemble the structure value */
  ST *struct_temp_st = Gen_Temp_Symbol(ty_idx, ".va_arg_struct");
#ifdef FE_GNU_4_2_0
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
  WGEN_Stmt_Append (wn, Get_Srcpos ());
  /* compute reg_save_area+fp_offset and store 2nd dereferenced value to 
   * struct_temp_st */
  wn0 = WN_Iload(MTYPE_I4, 4, MTYPE_To_TY(MTYPE_I4), WN_CopyNode(ap_wn));
  wn1 = WN_Iload(Pointer_Mtype, 16, MTYPE_To_TY(Pointer_Mtype), 
		 WN_CopyNode(ap_wn));
  wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
  wn = WN_Iload(MTYPE_F8, 16, MTYPE_To_TY(MTYPE_F8), wn);
  wn = WN_Stid(MTYPE_F8, 8, struct_temp_st, MTYPE_To_TY(MTYPE_F8), wn);
  WGEN_Stmt_Append (wn, Get_Srcpos ());

  /* increment fp_offset by 32 */
  wn0 = WN_Iload(MTYPE_I4, 4, MTYPE_To_TY(MTYPE_I4), WN_CopyNode(ap_wn));
  wn1 = WN_Intconst(MTYPE_I4, 32);
  wn = WN_Binary(OPR_ADD, MTYPE_I4, wn0, wn1);
  wn = WN_Istore(MTYPE_I4, 4, Make_Pointer_Type(MTYPE_To_TY(MTYPE_I4)), 
		 WN_CopyNode(ap_wn), wn);
  WGEN_Stmt_Append (wn, Get_Srcpos ());

  /* put the address of struct_temp_st in arg_temp_st */
  ST *arg_temp_st = Gen_Temp_Symbol(Make_Pointer_Type(ty_idx), ".va_arg");
#ifdef FE_GNU_4_2_0
  WGEN_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, arg_temp_st);
#endif
  wn = WN_Lda(Pointer_Mtype, 0, struct_temp_st, 0);
  Set_ST_addr_saved(struct_temp_st);
  wn = WN_Stid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx), wn);
  WGEN_Stmt_Append (wn, Get_Srcpos ());

  /* branch around next part */
  LABEL_IDX lab2;
  New_LABEL (CURRENT_SYMTAB, lab2);
  WN *lab2_wn = WN_CreateLabel ((ST_IDX) 0, lab2, 0, NULL);
  wn = WN_CreateGoto(lab2);
  WGEN_Stmt_Append (wn, Get_Srcpos ());

  WGEN_Stmt_Append (lab1_wn, Get_Srcpos ());

  /* store overflow_arg_area pointer to arg_temp_st */
  wn0 = WN_Iload(Pointer_Mtype, 8, Make_Pointer_Type(ty_idx), 
		 WN_CopyNode(ap_wn));
  wn = WN_Stid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx), wn0);
  WGEN_Stmt_Append (wn, Get_Srcpos ());

  /* increment overflow_arg_area pointer by 16 */
  wn0 = WN_Iload(Pointer_Mtype, 8, MTYPE_To_TY(Pointer_Mtype), 
		 WN_CopyNode(ap_wn));
  wn1 = WN_Intconst(MTYPE_U8, 16);
  wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
  wn = WN_Istore(Pointer_Mtype, 8,Make_Pointer_Type(MTYPE_To_TY(Pointer_Mtype)),
		 WN_CopyNode(ap_wn), wn);
  WGEN_Stmt_Append (wn, Get_Srcpos ());

  WGEN_Stmt_Append (lab2_wn, Get_Srcpos ());

  return WN_Ldid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx));
}
#endif

static bool inside_eh_region = false;
// Setup an EH region, typically across a function call.
void
Setup_EH_Region (bool for_unwinding)
{
    WN * region_body;

    if (for_unwinding)
	region_body = WGEN_Stmt_Pop (wgen_stmk_region_body);
    else
    	{
	    region_body = WGEN_Stmt_Pop (wgen_stmk_call_region_body);
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

    WGEN_Stmt_Append (WN_CreateRegion (REGION_KIND_EH, region_body,
      WN_CreateBlock(), WN_CreateBlock(), New_Region_Id(), ereg_supp), Get_Srcpos());
    Set_PU_has_region (Get_Current_PU());
    Set_PU_has_exc_scopes (Get_Current_PU());

#ifdef TARG_IA64
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
#endif
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

#ifdef TARG_X8664
// Handle GNU x86 builtins
static WN *
WGEN_target_builtins (gs_t exp, INTRINSIC * iopc, BOOL * intrinsic_op)
{
  WN * wn = NULL;
                                                                                
  // Assumption: we would be generating intrinsics for most of the builtins
  *intrinsic_op = TRUE;
                                                                                
  gs_t func = gs_tree_operand (gs_tree_operand (exp, 0), 0);
  Is_True (gs_tree_code (func) == GS_FUNCTION_DECL && gs_decl_built_in (func) &&
           gs_decl_built_in_class(func) == GSBI_CLASS_BUILT_IN_MD,
	    ("Invalid tree node"));
                                                                                
  unsigned int ins_code = gs_decl_function_code (func);
  TYPE_ID res_type = TY_mtype(Get_TY(gs_tree_type(exp)));
  gs_t t_list = gs_tree_operand (exp, 1);
  WN * arg0 = NULL, * arg1 = NULL;
  if (t_list)
  {
    // Assumption: every builtin has 2 kids: this will change
    arg0 = WGEN_Expand_Expr (gs_tree_value (t_list));
    if (gs_tree_chain (t_list))
      arg1 = WGEN_Expand_Expr (gs_tree_value (gs_tree_chain (t_list)));
  }
                                                                                
  switch (ins_code)
  {
    // Generate WN
    case GSBI_IX86_BUILTIN_PADDQ:
      *iopc = INTRN_PADDQ;
      break;
    case GSBI_IX86_BUILTIN_PADDB:
    case GSBI_IX86_BUILTIN_PADDW:
    case GSBI_IX86_BUILTIN_PADDD:
    case GSBI_IX86_BUILTIN_ADDPD:
    case GSBI_IX86_BUILTIN_PADDB128:
    case GSBI_IX86_BUILTIN_PADDQ128:
      wn = WN_Add (res_type, arg0, arg1);
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_PSUBQ:
      *iopc = INTRN_PSUBQ;
      break;
    case GSBI_IX86_BUILTIN_PSUBB:
    case GSBI_IX86_BUILTIN_PSUBW:
    case GSBI_IX86_BUILTIN_PSUBD:
    case GSBI_IX86_BUILTIN_SUBPD:
    case GSBI_IX86_BUILTIN_PSUBB128:
    case GSBI_IX86_BUILTIN_PSUBW128:
    case GSBI_IX86_BUILTIN_PSUBD128:
    case GSBI_IX86_BUILTIN_PSUBQ128:
      wn = WN_Sub (res_type, arg0, arg1);
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
    case GSBI_IX86_BUILTIN_PMULUDQ:
      *iopc = INTRN_PMULUDQ;
      break;
    case GSBI_IX86_BUILTIN_PADDSB128:
      *iopc = INTRN_PADDSB128;
      break;
    case GSBI_IX86_BUILTIN_PADDSW128:
      *iopc = INTRN_PADDSW128;
      break;
    case GSBI_IX86_BUILTIN_PSUBSB128:
      *iopc = INTRN_PSUBSB128;
      break;
    case GSBI_IX86_BUILTIN_PSUBSW128:
      *iopc = INTRN_PSUBSW128;
      break;
    case GSBI_IX86_BUILTIN_PADDUSB128:
      *iopc = INTRN_PADDUSB128;
      break;
    case GSBI_IX86_BUILTIN_PADDUSW128:
      *iopc = INTRN_PADDUSW128;
      break;
    case GSBI_IX86_BUILTIN_PSUBUSB128:
      *iopc = INTRN_PSUBUSB128;
      break;
    case GSBI_IX86_BUILTIN_PSUBUSW128:
      *iopc = INTRN_PSUBUSW128;
      break;
    case GSBI_IX86_BUILTIN_PMULLW128:
      *iopc = INTRN_PMULLW128;
      break;
    case GSBI_IX86_BUILTIN_PMULHW128:
      *iopc = INTRN_PMULHW128;
      break;
    case GSBI_IX86_BUILTIN_PMULUDQ128:
      *iopc = INTRN_PMULUDQ128;
      break;
    case GSBI_IX86_BUILTIN_PADDD128:
      *iopc = INTRN_PADDD128;
      break;
    case GSBI_IX86_BUILTIN_PADDW128:
      *iopc = INTRN_PADDW128;
      break;
    case GSBI_IX86_BUILTIN_PCMPEQB:
      *iopc = INTRN_PCMPEQB;
      break;
    case GSBI_IX86_BUILTIN_PCMPEQB128:
      *iopc = INTRN_PCMPEQB128;
      break;
    case GSBI_IX86_BUILTIN_PCMPEQW:
      *iopc = INTRN_PCMPEQW;
      break;
    case GSBI_IX86_BUILTIN_PCMPEQW128:
      *iopc = INTRN_PCMPEQW128;
      break;
    case GSBI_IX86_BUILTIN_PCMPEQD:
      *iopc = INTRN_PCMPEQD;
      break;
    case GSBI_IX86_BUILTIN_PCMPEQD128:
      *iopc = INTRN_PCMPEQD128;
      break;
    case GSBI_IX86_BUILTIN_PCMPGTB:
      *iopc = INTRN_PCMPGTB;
      break;
    case GSBI_IX86_BUILTIN_PCMPGTB128:
      *iopc = INTRN_PCMPGTB128;
      break;
    case GSBI_IX86_BUILTIN_PCMPGTW:
      *iopc = INTRN_PCMPGTW;
      break;
    case GSBI_IX86_BUILTIN_PCMPGTW128:
      *iopc = INTRN_PCMPGTW128;
      break;
    case GSBI_IX86_BUILTIN_PCMPGTD:
      *iopc = INTRN_PCMPGTD;
      break;
    case GSBI_IX86_BUILTIN_PCMPGTD128:
      *iopc = INTRN_PCMPGTD128;
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
    case GSBI_IX86_BUILTIN_PUNPCKHBW128:
      *iopc = INTRN_PUNPCKHBW128;
      break;
    case GSBI_IX86_BUILTIN_PUNPCKHWD128:
      *iopc = INTRN_PUNPCKHWD128;
      break;
    case GSBI_IX86_BUILTIN_PUNPCKHDQ128:
      *iopc = INTRN_PUNPCKHDQ128;
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
    case GSBI_IX86_BUILTIN_PUNPCKLBW128:
      *iopc = INTRN_PUNPCKLBW128;
      break;
    case GSBI_IX86_BUILTIN_PUNPCKLWD128:
      *iopc = INTRN_PUNPCKLWD128;
      break;
    case GSBI_IX86_BUILTIN_PUNPCKLDQ128:
      *iopc = INTRN_PUNPCKLDQ128;
      break;
    case GSBI_IX86_BUILTIN_PUNPCKHQDQ128:
      *iopc = INTRN_PUNPCKHQDQ;
      break;
    case GSBI_IX86_BUILTIN_PUNPCKLQDQ128:
      *iopc = INTRN_PUNPCKLQDQ;
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
    case GSBI_IX86_BUILTIN_PACKSSWB128:
      *iopc = INTRN_PACKSSWB128;
      break;
    case GSBI_IX86_BUILTIN_PACKSSDW128:
      *iopc = INTRN_PACKSSDW128;
      break;
    case GSBI_IX86_BUILTIN_PACKUSWB128:
      *iopc = INTRN_PACKUSWB128;
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
    case GSBI_IX86_BUILTIN_PMULHUW128:
      *iopc = INTRN_PMULHUW128;
      break;
    case GSBI_IX86_BUILTIN_PAVGB128:
      *iopc = INTRN_PAVGB128;
      break;
    case GSBI_IX86_BUILTIN_PAVGW128:
      *iopc = INTRN_PAVGW128;
      break;
    case GSBI_IX86_BUILTIN_PSADBW128:
      *iopc = INTRN_PSADBW128;
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
    case GSBI_IX86_BUILTIN_PMAXUB128:
      *iopc = INTRN_PMAXUB128;
      break;
    case GSBI_IX86_BUILTIN_PMAXSW128:
      *iopc = INTRN_PMAXSW128;
      break;
    case GSBI_IX86_BUILTIN_PMINUB128:
      *iopc = INTRN_PMINUB128;
      break;
    case GSBI_IX86_BUILTIN_PMINSW128:
      *iopc = INTRN_PMINSW128;
      break;
    case GSBI_IX86_BUILTIN_PMOVMSKB:
      *iopc = INTRN_PMOVMSKB;
      break;
    case GSBI_IX86_BUILTIN_PMOVMSKB128:
      *iopc = INTRN_PMOVMSKB128;
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
    case GSBI_IX86_BUILTIN_ADDSD:
      *iopc = INTRN_ADDSD;
      break;
    case GSBI_IX86_BUILTIN_SUBSD:
      *iopc = INTRN_SUBSD;
      break;
    case GSBI_IX86_BUILTIN_MULSD:
      *iopc = INTRN_MULSD;
      break;
    case GSBI_IX86_BUILTIN_DIVSD:
      *iopc = INTRN_DIVSD;
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
    case GSBI_IX86_BUILTIN_CMPEQPD:
      *iopc = INTRN_CMPEQPD;
      break;
    case GSBI_IX86_BUILTIN_CMPLTPD:
      *iopc = INTRN_CMPLTPD;
      break;
    case GSBI_IX86_BUILTIN_CMPLEPD:
      *iopc = INTRN_CMPLEPD;
      break;
    case GSBI_IX86_BUILTIN_CMPGTPD:
      *iopc = INTRN_CMPGTPD;
      break;
    case GSBI_IX86_BUILTIN_CMPGEPD:
      *iopc = INTRN_CMPGEPD;
      break;
    case GSBI_IX86_BUILTIN_CMPUNORDPD:
      *iopc = INTRN_CMPUNORDPD;
      break;
    case GSBI_IX86_BUILTIN_CMPNEQPD:
      *iopc = INTRN_CMPNEQPD;
      break;
    case GSBI_IX86_BUILTIN_CMPNLTPD:
      *iopc = INTRN_CMPNLTPD;
      break;
    case GSBI_IX86_BUILTIN_CMPNLEPD:
      *iopc = INTRN_CMPNLEPD;
      break;
    case GSBI_IX86_BUILTIN_CMPNGTPD:
      *iopc = INTRN_CMPNGTPD;
      break;
    case GSBI_IX86_BUILTIN_CMPNGEPD:
      *iopc = INTRN_CMPNGEPD;
      break;
    case GSBI_IX86_BUILTIN_CMPORDPD:
      *iopc = INTRN_CMPORDPD;
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
    case GSBI_IX86_BUILTIN_CMPEQSD:
      *iopc = INTRN_CMPEQSD;
      break;
    case GSBI_IX86_BUILTIN_CMPLTSD:
      *iopc = INTRN_CMPLTSD;
      break;
    case GSBI_IX86_BUILTIN_CMPLESD:
      *iopc = INTRN_CMPLESD;
      break;
    case GSBI_IX86_BUILTIN_CMPUNORDSD:
      *iopc = INTRN_CMPUNORDSD;
      break;
    case GSBI_IX86_BUILTIN_CMPNEQSD:
      *iopc = INTRN_CMPNEQSD;
      break;
    case GSBI_IX86_BUILTIN_CMPNLTSD:
      *iopc = INTRN_CMPNLTSD;
      break;
    case GSBI_IX86_BUILTIN_CMPNLESD:
      *iopc = INTRN_CMPNLESD;
      break;
    case GSBI_IX86_BUILTIN_CMPORDSD:
      *iopc = INTRN_CMPORDSD;
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
    case GSBI_IX86_BUILTIN_CMPNGTSS:
      *iopc = INTRN_CMPNGTSS;
      break;
    case GSBI_IX86_BUILTIN_CMPNGESS:
      *iopc = INTRN_CMPNGESS;
      break;
    case GSBI_IX86_BUILTIN_CMPORDSS:
      *iopc = INTRN_CMPORDSS;
      break;
    case GSBI_IX86_BUILTIN_MAXPS:
      *iopc = INTRN_MAXPS;
      break;
    case GSBI_IX86_BUILTIN_MAXSD:
      *iopc = INTRN_MAXSD;
      break;
    case GSBI_IX86_BUILTIN_MAXSS:
      *iopc = INTRN_MAXSS;
      break;
    case GSBI_IX86_BUILTIN_MINPS:
      *iopc = INTRN_MINPS;
      break;
    case GSBI_IX86_BUILTIN_MINSD:
      *iopc = INTRN_MINSD;
      break;
    case GSBI_IX86_BUILTIN_MINSS:
      *iopc = INTRN_MINSS;
      break;
    case GSBI_IX86_BUILTIN_ANDPS:
      *iopc = INTRN_ANDPS;
      break;
    case GSBI_IX86_BUILTIN_ANDNPD:
      *iopc = INTRN_ANDNPD;
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
    case GSBI_IX86_BUILTIN_MOVSD:
      *iopc = INTRN_MOVSD;
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
    case GSBI_IX86_BUILTIN_SQRTSD:
      *iopc = INTRN_SQRTSD;
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
    case GSBI_IX86_BUILTIN_CLFLUSH:
      *iopc = INTRN_CLFLUSH;
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_PSLLDQI128:
      *iopc = INTRN_PSLLDQ;
      break;
    case GSBI_IX86_BUILTIN_PSRLDQI128:
      *iopc = INTRN_PSRLDQ;
      break;
    case GSBI_IX86_BUILTIN_PSLLW128:
      *iopc = INTRN_PSLLW128;
      break;
    case GSBI_IX86_BUILTIN_PSLLD128:
      *iopc = INTRN_PSLLD128;
      break;
    case GSBI_IX86_BUILTIN_PSLLQ128:
      *iopc = INTRN_PSLLQ128;
      break;
    case GSBI_IX86_BUILTIN_PSRLW128:
      *iopc = INTRN_PSRLW128;
      break;
    case GSBI_IX86_BUILTIN_PSRLD128:
      *iopc = INTRN_PSRLD128;
      break;
    case GSBI_IX86_BUILTIN_PSRLQ128:
      *iopc = INTRN_PSRLQ128;
      break;
    case GSBI_IX86_BUILTIN_PSRAW128:
      *iopc = INTRN_PSRAW128;
      break;
    case GSBI_IX86_BUILTIN_PSRAD128:
      *iopc = INTRN_PSRAD128;
      break;
    case GSBI_IX86_BUILTIN_PSRAWI128:
      *iopc = INTRN_PSRAWI128;
      break;
    case GSBI_IX86_BUILTIN_PSRADI128:
      *iopc = INTRN_PSRADI128;
      break;
    case GSBI_IX86_BUILTIN_PSLLWI128:
      *iopc = INTRN_PSLLWI128;
      break;
    case GSBI_IX86_BUILTIN_PSLLDI128:
      *iopc = INTRN_PSLLDI128;
      break;
    case GSBI_IX86_BUILTIN_PSLLQI128:
      *iopc = INTRN_PSLLQI128;
      break;
    case GSBI_IX86_BUILTIN_PSRLWI128:
      *iopc = INTRN_PSRLWI128;
      break;
    case GSBI_IX86_BUILTIN_PSRLDI128:
      *iopc = INTRN_PSRLDI128;
      break;
    case GSBI_IX86_BUILTIN_PSRLQI128:
      *iopc = INTRN_PSRLQI128;
      break;
    case GSBI_IX86_BUILTIN_MOVNTDQ:
      *iopc = INTRN_MOVNTDQ;
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_MOVNTPS:
      *iopc = INTRN_MOVNTPS;
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_PSHUFD:
      *iopc = INTRN_PSHUFD;
      break;
    case GSBI_IX86_BUILTIN_DIVPD:
      wn = WN_Div (res_type, arg0, arg1);
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_MULPD:
      wn = WN_Mpy (res_type, arg0, arg1);
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_SQRTPD:
      wn = WN_Sqrt (res_type, arg0);
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_MINPD:
      wn = WN_Binary (OPR_MIN, res_type, arg0, arg1);
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_MAXPD:
      wn = WN_Binary (OPR_MAX, res_type, arg0, arg1);
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
    case GSBI_IX86_BUILTIN_LOADDQU:
      *iopc = INTRN_LOADDQU;
      break;
    case GSBI_IX86_BUILTIN_STOREDQU:
      *iopc = INTRN_STOREDQU;
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_VEC_INIT_V2SI:
      *iopc = INTRN_VEC_INIT_V2SI;
      break;
    case GSBI_IX86_BUILTIN_VEC_EXT_V2SI:
      *iopc = INTRN_VEC_EXT_V2SI;
      break;
    case GSBI_IX86_BUILTIN_VEC_EXT_V4SI:
      *iopc = INTRN_VEC_EXT_V2SI;
      break;
    case GSBI_IX86_BUILTIN_VEC_EXT_V4HI:
      // add intrinsic for _mm_extract_pi16
      if (WN_operator(arg1) != OPR_INTCONST)
        Fail_FmtAssertion ("selector must be an integer constant in the range 0..3");
      switch (WN_const_val(arg1)){
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
        Fail_FmtAssertion ("selector must be an integer constant in the range 0..3" );
      }
      break;
    case GSBI_IX86_BUILTIN_VEC_EXT_V2DI:
      *iopc = INTRN_VEC_EXT_V2SI;
      break;
    case GSBI_IX86_BUILTIN_VEC_EXT_V2DF:
      wn = WN_Tas(MTYPE_F8, MTYPE_To_TY(MTYPE_V16F8), arg0);
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_VEC_EXT_V4SF:
      wn = WN_Tas(MTYPE_F4, MTYPE_To_TY(MTYPE_V16F4), arg0);
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_PMADDWD:
    case GSBI_IX86_BUILTIN_PMADDWD128:
      *iopc = INTRN_PMADDWD;
      break;
    case GSBI_IX86_BUILTIN_PSLLW:
      *iopc = INTRN_PSLLW;
      break;
    case GSBI_IX86_BUILTIN_PSLLD:
      *iopc = INTRN_PSLLD;
      break;
    case GSBI_IX86_BUILTIN_PSLLQ:
      *iopc = INTRN_PSLLQ;
      break;
    case GSBI_IX86_BUILTIN_PSRLW:
      *iopc = INTRN_PSRLW;
      break;
    case GSBI_IX86_BUILTIN_PSRLD:
      *iopc = INTRN_PSRLD;
      break;
    case GSBI_IX86_BUILTIN_PSRLQ:
      *iopc = INTRN_PSRLQ;
      break;
    case GSBI_IX86_BUILTIN_PSRAW:
      *iopc = INTRN_PSRAW;
      break;
    case GSBI_IX86_BUILTIN_PSRAD:
      *iopc = INTRN_PSRAD;
      break;
    case GSBI_IX86_BUILTIN_PSRAWI:
      *iopc = INTRN_PSRAWI;
      break;
    case GSBI_IX86_BUILTIN_PSRADI:
      *iopc = INTRN_PSRADI;
      break;
    case GSBI_IX86_BUILTIN_PSLLWI:
      *iopc = INTRN_PSLLWI;
      break;
    case GSBI_IX86_BUILTIN_PSLLDI:
      *iopc = INTRN_PSLLDI;
      break;
    case GSBI_IX86_BUILTIN_PSLLQI:
      *iopc = INTRN_PSLLQI;
      break;
    case GSBI_IX86_BUILTIN_PSRLWI:
      *iopc = INTRN_PSRLWI;
      break;
    case GSBI_IX86_BUILTIN_PSRLDI:
      *iopc = INTRN_PSRLDI;
      break;
    case GSBI_IX86_BUILTIN_PSRLQI:
      *iopc = INTRN_PSRLQI;
      break;
    case GSBI_IX86_BUILTIN_PAND:
      *iopc = INTRN_PAND_MMX;
      break;
    case GSBI_IX86_BUILTIN_PANDN:
      *iopc = INTRN_PANDN_MMX;
      break;
    case GSBI_IX86_BUILTIN_PXOR:
      *iopc = INTRN_PXOR_MMX;
      break;
    case GSBI_IX86_BUILTIN_POR:
      *iopc = INTRN_POR_MMX;
      break;
    case GSBI_IX86_BUILTIN_PAND128:
      *iopc = INTRN_PAND128;
      break;
    case GSBI_IX86_BUILTIN_PANDN128:
      *iopc = INTRN_PANDN128;
      break;
    case GSBI_IX86_BUILTIN_POR128:
      *iopc = INTRN_POR128;
      break;
    case GSBI_IX86_BUILTIN_PXOR128:
      *iopc = INTRN_PXOR128;
      break;
    case GSBI_IX86_BUILTIN_VEC_INIT_V4HI: {
      WN *arg[4];
      gs_t t_node = t_list;
      INT i;
      for (i = 0; i < 4; i++, t_node = gs_tree_chain(t_node)) {
	arg[i] = WGEN_Expand_Expr(gs_tree_value(t_node));
	arg[i] = WN_CreateCvtl(OPC_U8CVTL, 16, arg[i]);
      }
      wn = arg[3];
      for (i = 2; i >= 0; i--) {
	wn = WN_Shl(MTYPE_U8, wn, WN_Intconst(MTYPE_U8, 16));
	wn = WN_Bior(MTYPE_U8, wn, arg[i]);
      }
      *intrinsic_op = FALSE;
      break;
    }
    case GSBI_IX86_BUILTIN_VEC_INIT_V8QI: {
      WN *arg[8];
      gs_t t_node = t_list;
      INT i;
      for (i = 0; i < 8; i++, t_node = gs_tree_chain(t_node)) {
	arg[i] = WGEN_Expand_Expr(gs_tree_value(t_node));
	arg[i] = WN_CreateCvtl(OPC_U8CVTL, 8, arg[i]);
      }
      wn = arg[7];
      for (i = 6; i >= 0; i--) {
	wn = WN_Shl(MTYPE_U8, wn, WN_Intconst(MTYPE_U8, 8));
	wn = WN_Bior(MTYPE_U8, wn, arg[i]);
      }
      *intrinsic_op = FALSE;
      break;
    }
    case GSBI_IX86_BUILTIN_COMIEQSS:
    case GSBI_IX86_BUILTIN_UCOMIEQSS:
      *iopc = INTRN_COMIEQSS;
      break;
    case GSBI_IX86_BUILTIN_COMILTSS:
    case GSBI_IX86_BUILTIN_UCOMILTSS:
      *iopc = INTRN_COMILTSS;
      break;
    case GSBI_IX86_BUILTIN_COMILESS:
    case GSBI_IX86_BUILTIN_UCOMILESS:
      *iopc = INTRN_COMILESS;
      break;
    case GSBI_IX86_BUILTIN_COMIGTSS:
    case GSBI_IX86_BUILTIN_UCOMIGTSS:
      *iopc = INTRN_COMIGTSS;
      break;
    case GSBI_IX86_BUILTIN_COMIGESS:
    case GSBI_IX86_BUILTIN_UCOMIGESS:
      *iopc = INTRN_COMIGESS;
      break;
    case GSBI_IX86_BUILTIN_COMINEQSS:
    case GSBI_IX86_BUILTIN_UCOMINEQSS:
      *iopc = INTRN_COMINEQSS;
      break;
    case GSBI_IX86_BUILTIN_COMIEQSD:
    case GSBI_IX86_BUILTIN_UCOMIEQSD:
      *iopc = INTRN_COMIEQSD;
      break;
    case GSBI_IX86_BUILTIN_COMILTSD:
    case GSBI_IX86_BUILTIN_UCOMILTSD:
      *iopc = INTRN_COMILTSD;
      break;
    case GSBI_IX86_BUILTIN_COMILESD:
    case GSBI_IX86_BUILTIN_UCOMILESD:
      *iopc = INTRN_COMILESD;
      break;
    case GSBI_IX86_BUILTIN_COMIGTSD:
    case GSBI_IX86_BUILTIN_UCOMIGTSD:
      *iopc = INTRN_COMIGTSD;
      break;
    case GSBI_IX86_BUILTIN_COMIGESD:
    case GSBI_IX86_BUILTIN_UCOMIGESD:
      *iopc = INTRN_COMIGESD;
      break;
    case GSBI_IX86_BUILTIN_COMINEQSD:
    case GSBI_IX86_BUILTIN_UCOMINEQSD:
      *iopc = INTRN_COMINEQSD;
      break;
    case GSBI_IX86_BUILTIN_CVTPI2PS:
      *iopc = INTRN_CVTPI2PS;
      break;
    case GSBI_IX86_BUILTIN_CVTPS2PI:
      *iopc = INTRN_CVTPS2PI;
      break;
    case GSBI_IX86_BUILTIN_CVTSI2SS:
      *iopc = INTRN_CVTSI2SS;
      break;
    case GSBI_IX86_BUILTIN_CVTSI642SS:
      *iopc = INTRN_CVTSI642SS;
      break;
    case GSBI_IX86_BUILTIN_CVTSS2SI:
      *iopc = INTRN_CVTSS2SI;
      break;
    case GSBI_IX86_BUILTIN_CVTSS2SI64:
      *iopc = INTRN_CVTSS2SI64;
      break;
    case GSBI_IX86_BUILTIN_CVTTPS2PI:
      *iopc = INTRN_CVTTPS2PI;
      break;
    case GSBI_IX86_BUILTIN_CVTTSS2SI:
      *iopc = INTRN_CVTTSS2SI;
      break;
    case GSBI_IX86_BUILTIN_CVTTSS2SI64:
      *iopc = INTRN_CVTTSS2SI64;
      break;
    case GSBI_IX86_BUILTIN_CVTDQ2PD:
      *iopc = INTRN_CVTDQ2PD;
      break;
    case GSBI_IX86_BUILTIN_CVTDQ2PS:
      *iopc = INTRN_CVTDQ2PS;
      break;
    case GSBI_IX86_BUILTIN_CVTPD2DQ:
      *iopc = INTRN_CVTPD2DQ;
      break;
    case GSBI_IX86_BUILTIN_CVTPD2PI:
      *iopc = INTRN_CVTPD2PI;
      break;
    case GSBI_IX86_BUILTIN_CVTPD2PS:
      *iopc = INTRN_CVTPD2PS;
      break;
    case GSBI_IX86_BUILTIN_CVTTPD2DQ:
      *iopc = INTRN_CVTTPD2DQ;
      break;
    case GSBI_IX86_BUILTIN_CVTTPD2PI:
      *iopc = INTRN_CVTTPD2PI;
      break;
    case GSBI_IX86_BUILTIN_CVTPI2PD:
      *iopc = INTRN_CVTPI2PD;
      break;
    case GSBI_IX86_BUILTIN_CVTSI2SD:
      *iopc = INTRN_CVTSI2SD;
      break;
    case GSBI_IX86_BUILTIN_CVTSI642SD:
      *iopc = INTRN_CVTSI642SD;
      break;
    case GSBI_IX86_BUILTIN_CVTSD2SI:
      *iopc = INTRN_CVTSD2SI;
      break;
    case GSBI_IX86_BUILTIN_CVTSD2SI64:
      *iopc = INTRN_CVTSD2SI64;
      break;
    case GSBI_IX86_BUILTIN_CVTSD2SS:
      *iopc = INTRN_CVTSD2SS;
      break;
    case GSBI_IX86_BUILTIN_CVTSS2SD:
      *iopc = INTRN_CVTSS2SD;
      break;
    case GSBI_IX86_BUILTIN_CVTTSD2SI:
      *iopc = INTRN_CVTTSD2SI;
      break;
    case GSBI_IX86_BUILTIN_CVTTSD2SI64:
      *iopc = INTRN_CVTTSD2SI64;
      break;
    case GSBI_IX86_BUILTIN_CVTPS2DQ:
      *iopc = INTRN_CVTPS2DQ;
      break;
    case GSBI_IX86_BUILTIN_CVTPS2PD:
      *iopc = INTRN_CVTPS2PD;
      break;
    case GSBI_IX86_BUILTIN_CVTTPS2DQ:
      *iopc = INTRN_CVTTPS2DQ;
      break;
    case GSBI_IX86_BUILTIN_LOADUPS:
      *iopc = INTRN_LOADUPS;
      break;
    case GSBI_IX86_BUILTIN_STOREUPS:
      *iopc = INTRN_STOREUPS;
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_LOADUPD:
      *iopc = INTRN_LOADUPD;
      break;
    case GSBI_IX86_BUILTIN_STOREUPD:
      *iopc = INTRN_STOREUPD;
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_LOADHPS:
      *iopc = INTRN_LOADHPS;
      break;
    case GSBI_IX86_BUILTIN_STOREHPS:
      *iopc = INTRN_STOREHPS;
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_LOADLPS:
      *iopc = INTRN_LOADLPS;
      break;
    case GSBI_IX86_BUILTIN_STORELPS:
      *iopc = INTRN_STORELPS;
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_MOVMSKPS:
      *iopc = INTRN_MOVMSKPS;
      break;
    case GSBI_IX86_BUILTIN_MOVMSKPD:
      *iopc = INTRN_MOVMSKPD;
      break;
    case GSBI_IX86_BUILTIN_MASKMOVDQU:
      *iopc = INTRN_MASKMOVDQU;
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_MASKMOVQ:
      *iopc = INTRN_MASKMOVQ;
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_MOVNTPD:
      *iopc = INTRN_MOVNTPD;
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_MOVNTI:
      *iopc = INTRN_MOVNTI;
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_MOVNTQ:
      *iopc = INTRN_MOVNTQ;
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_STMXCSR:
      *iopc = INTRN_STMXCSR;
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_LDMXCSR:
      *iopc = INTRN_LDMXCSR;
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_PSHUFLW:
      *iopc = INTRN_PSHUFLW;
      break;
    case GSBI_IX86_BUILTIN_PSHUFHW:
      *iopc = INTRN_PSHUFHW;
      break;
    case GSBI_IX86_BUILTIN_MOVNTSS:
      *iopc = INTRN_MOVNTSS;
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_MOVNTSD:
      *iopc = INTRN_MOVNTSD;
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_EXTRQI:
      *iopc = INTRN_EXTRQ;
      break;
    case GSBI_IX86_BUILTIN_EXTRQ:
      *iopc = INTRN_EXTRQ;
      break;
    case GSBI_IX86_BUILTIN_INSERTQI:
      *iopc = INTRN_INSERTQI;
      break;
    case GSBI_IX86_BUILTIN_INSERTQ:
      *iopc = INTRN_INSERTQ;
      break;
#ifdef FE_GNU_4_2_0
    // SSSE3 intrinsics
    case GSBI_IX86_BUILTIN_PABSB:
    case GSBI_IX86_BUILTIN_PABSB128:
      *iopc = INTRN_PABSB;
      break;
    case GSBI_IX86_BUILTIN_PABSD:
    case GSBI_IX86_BUILTIN_PABSD128:
      *iopc = INTRN_PABSD;
      break;
    case GSBI_IX86_BUILTIN_PABSW:
    case GSBI_IX86_BUILTIN_PABSW128:
      *iopc = INTRN_PABSW;
      break;
    case GSBI_IX86_BUILTIN_PALIGNR:
    case GSBI_IX86_BUILTIN_PALIGNR128:
      *iopc = INTRN_PALIGNR;
      break;
    case GSBI_IX86_BUILTIN_PHADDD:
    case GSBI_IX86_BUILTIN_PHADDD128:
      *iopc = INTRN_PHADDD;
      break;
    case GSBI_IX86_BUILTIN_PHADDSW:
    case GSBI_IX86_BUILTIN_PHADDSW128:
      *iopc = INTRN_PHADDSW;
      break;
    case GSBI_IX86_BUILTIN_PHADDW:
    case GSBI_IX86_BUILTIN_PHADDW128:
      *iopc = INTRN_PHADDW;
      break;
    case GSBI_IX86_BUILTIN_PHSUBD:
    case GSBI_IX86_BUILTIN_PHSUBD128:
      *iopc = INTRN_PHSUBD;
      break;
    case GSBI_IX86_BUILTIN_PHSUBSW:
    case GSBI_IX86_BUILTIN_PHSUBSW128:
      *iopc = INTRN_PHSUBSW;
      break;
    case GSBI_IX86_BUILTIN_PHSUBW:
    case GSBI_IX86_BUILTIN_PHSUBW128:
      *iopc = INTRN_PHSUBW;
      break;
    case GSBI_IX86_BUILTIN_PMADDUBSW:
    case GSBI_IX86_BUILTIN_PMADDUBSW128:
      *iopc = INTRN_PMADDUBSW;
      break;
    case GSBI_IX86_BUILTIN_PMULHRSW:
    case GSBI_IX86_BUILTIN_PMULHRSW128:
      *iopc = INTRN_PMULHRSW;
      break;
    case GSBI_IX86_BUILTIN_PSHUFB:
    case GSBI_IX86_BUILTIN_PSHUFB128:
      *iopc = INTRN_PSHUFB;
      break;
    case GSBI_IX86_BUILTIN_PSIGNB:
    case GSBI_IX86_BUILTIN_PSIGNB128:
      *iopc = INTRN_PSIGNB;
      break;
    case GSBI_IX86_BUILTIN_PSIGND:
    case GSBI_IX86_BUILTIN_PSIGND128:
      *iopc = INTRN_PSIGND;
      break;
    case GSBI_IX86_BUILTIN_PSIGNW:
    case GSBI_IX86_BUILTIN_PSIGNW128:
      *iopc = INTRN_PSIGNW;
      break;
    // SSE4.1 intrinsics
    case GSBI_IX86_BUILTIN_BLENDPD:
      *iopc = INTRN_BLENDPD;
      break;
    case GSBI_IX86_BUILTIN_BLENDPS:
      *iopc = INTRN_BLENDPS;
      break;
    case GSBI_IX86_BUILTIN_BLENDVPD:
      *iopc = INTRN_BLENDVPD;
      break;
    case GSBI_IX86_BUILTIN_BLENDVPS:
      *iopc = INTRN_BLENDVPS;
      break;
    case GSBI_IX86_BUILTIN_DPPD:
      *iopc = INTRN_DPPD;
      break;
    case GSBI_IX86_BUILTIN_DPPS:
      *iopc = INTRN_DPPS;
      break;
    case GSBI_IX86_BUILTIN_INSERTPS128:
      *iopc = INTRN_INSERTPS128;
      break;
    case GSBI_IX86_BUILTIN_MOVNTDQA:
      *iopc = INTRN_MOVNTDQA;
      break;
    case GSBI_IX86_BUILTIN_MPSADBW128:
      *iopc = INTRN_MPSADBW128;
      break;
    case GSBI_IX86_BUILTIN_PACKUSDW128:
      *iopc = INTRN_PACKUSDW128;
      break;
    case GSBI_IX86_BUILTIN_PBLENDVB128:
      *iopc = INTRN_PBLENDVB128;
      break;
    case GSBI_IX86_BUILTIN_PBLENDW128:
      *iopc = INTRN_PBLENDW128;
      break;
    case GSBI_IX86_BUILTIN_PCMPEQQ:
      *iopc = INTRN_PCMPEQQ;
      break;
    case GSBI_IX86_BUILTIN_PHMINPOSUW128:
      *iopc = INTRN_PHMINPOSUW128;
      break;
    case GSBI_IX86_BUILTIN_PMAXSB128:
      *iopc = INTRN_PMAXSB128;
      break;
    case GSBI_IX86_BUILTIN_PMAXSD128:
      *iopc = INTRN_PMAXSD128;
      break;
    case GSBI_IX86_BUILTIN_PMAXUD128:
      *iopc = INTRN_PMAXUD128;
      break;
    case GSBI_IX86_BUILTIN_PMAXUW128:
      *iopc = INTRN_PMAXUW128;
      break;
    case GSBI_IX86_BUILTIN_PMINSB128:
      *iopc = INTRN_PMINSB128;
      break;
    case GSBI_IX86_BUILTIN_PMINSD128:
      *iopc = INTRN_PMINSD128;
      break;
    case GSBI_IX86_BUILTIN_PMINUD128:
      *iopc = INTRN_PMINUD128;
      break;
    case GSBI_IX86_BUILTIN_PMINUW128:
      *iopc = INTRN_PMINUW128;
      break;
    case GSBI_IX86_BUILTIN_PMOVSXBD128:
      *iopc = INTRN_PMOVSXBD128;
      break;
    case GSBI_IX86_BUILTIN_PMOVSXBQ128:
      *iopc = INTRN_PMOVSXBQ128;
      break;
    case GSBI_IX86_BUILTIN_PMOVSXBW128:
      *iopc = INTRN_PMOVSXBW128;
      break;
    case GSBI_IX86_BUILTIN_PMOVSXDQ128:
      *iopc = INTRN_PMOVSXDQ128;
      break;
    case GSBI_IX86_BUILTIN_PMOVSXWD128:
      *iopc = INTRN_PMOVSXWD128;
      break;
    case GSBI_IX86_BUILTIN_PMOVSXWQ128:
      *iopc = INTRN_PMOVSXWQ128;
      break;
    case GSBI_IX86_BUILTIN_PMOVZXBD128:
      *iopc = INTRN_PMOVZXBD128;
      break;
    case GSBI_IX86_BUILTIN_PMOVZXBQ128:
      *iopc = INTRN_PMOVZXBQ128;
      break;
    case GSBI_IX86_BUILTIN_PMOVZXBW128:
      *iopc = INTRN_PMOVZXBW128;
      break;
    case GSBI_IX86_BUILTIN_PMOVZXDQ128:
      *iopc = INTRN_PMOVZXDQ128;
      break;
    case GSBI_IX86_BUILTIN_PMOVZXWD128:
      *iopc = INTRN_PMOVZXWD128;
      break;
    case GSBI_IX86_BUILTIN_PMOVZXWQ128:
      *iopc = INTRN_PMOVZXWQ128;
      break;
    case GSBI_IX86_BUILTIN_PMULDQ128:
      *iopc = INTRN_PMULDQ128;
      break;
    case GSBI_IX86_BUILTIN_PMULLD128:
      *iopc = INTRN_PMULLD128;
      break;
    case GSBI_IX86_BUILTIN_VEC_SET_V16QI:
      *iopc = INTRN_VEC_SET_V16QI;
      break;
    case GSBI_IX86_BUILTIN_VEC_SET_V2DI:
      *iopc = INTRN_VEC_SET_V2DI;
      break;
    case GSBI_IX86_BUILTIN_VEC_SET_V4SF:
      *iopc = INTRN_VEC_SET_V4SF;
      break;
    case GSBI_IX86_BUILTIN_VEC_SET_V4SI:
      *iopc = INTRN_VEC_SET_V4SI;
      break;
    // SSE4.2 intrinsics
    case GSBI_IX86_BUILTIN_CRC32DI:
      *iopc = INTRN_CRC32DI;
      break;
    case GSBI_IX86_BUILTIN_CRC32HI:
      *iopc = INTRN_CRC32HI;
      break;
    case GSBI_IX86_BUILTIN_CRC32QI:
      *iopc = INTRN_CRC32QI;
      break;
    case GSBI_IX86_BUILTIN_CRC32SI:
      *iopc = INTRN_CRC32SI;
      break;
    case GSBI_IX86_BUILTIN_PCMPESTRI128:
      *iopc = INTRN_PCMPESTRI128;
      break;
    case GSBI_IX86_BUILTIN_PCMPESTRA128:
      *iopc = INTRN_PCMPESTRA128;
      break;
    case GSBI_IX86_BUILTIN_PCMPESTRC128:
      *iopc = INTRN_PCMPESTRC128;
      break;
    case GSBI_IX86_BUILTIN_PCMPESTRO128:
      *iopc = INTRN_PCMPESTRO128;
      break;
    case GSBI_IX86_BUILTIN_PCMPESTRS128:
      *iopc = INTRN_PCMPESTRS128;
      break;
    case GSBI_IX86_BUILTIN_PCMPESTRZ128:
      *iopc = INTRN_PCMPESTRZ128;
      break;
    case GSBI_IX86_BUILTIN_PCMPESTRM128:
      *iopc = INTRN_PCMPESTRM128;
      break;
    case GSBI_IX86_BUILTIN_PCMPGTQ:
      *iopc = INTRN_PCMPGTQ;
      break;
    case GSBI_IX86_BUILTIN_PCMPISTRI128:
      *iopc = INTRN_PCMPISTRI128;
      break;
    case GSBI_IX86_BUILTIN_PCMPISTRA128:
      *iopc = INTRN_PCMPISTRA128;
      break;
    case GSBI_IX86_BUILTIN_PCMPISTRC128:
      *iopc = INTRN_PCMPISTRC128;
      break;
    case GSBI_IX86_BUILTIN_PCMPISTRO128:
      *iopc = INTRN_PCMPISTRO128;
      break;
    case GSBI_IX86_BUILTIN_PCMPISTRS128:
      *iopc = INTRN_PCMPISTRS128;
      break;
    case GSBI_IX86_BUILTIN_PCMPISTRZ128:
      *iopc = INTRN_PCMPISTRZ128;
      break;
    case GSBI_IX86_BUILTIN_PCMPISTRM128:
      *iopc = INTRN_PCMPISTRM128;
      break;
    // AES intrinsics
    case GSBI_IX86_BUILTIN_AESDEC128:
      *iopc = INTRN_AESDEC128;
      break;
    case GSBI_IX86_BUILTIN_AESDECLAST128:
      *iopc = INTRN_AESDECLAST128;
      break;
    case GSBI_IX86_BUILTIN_AESENC128:
      *iopc = INTRN_AESENC128;
      break;
    case GSBI_IX86_BUILTIN_AESENCLAST128:
      *iopc = INTRN_AESENCLAST128;
      break;
    case GSBI_IX86_BUILTIN_AESIMC128:
      *iopc = INTRN_AESIMC128;
      break;
    case GSBI_IX86_BUILTIN_AESKEYGENASSIST128:
      *iopc = INTRN_AESKEYGENASSIST128;
      break;
    // PCLMUL intrinsics
    case GSBI_IX86_BUILTIN_PCLMULQDQ128:
      *iopc = INTRN_PCLMULQDQ128;
      break;
    // AVX intrinsics
    case GSBI_IX86_BUILTIN_ADDPD256:
    case GSBI_IX86_BUILTIN_ADDPS256:
       wn = WN_Add (res_type, arg0, arg1);
       *intrinsic_op = FALSE;
       break;
    case GSBI_IX86_BUILTIN_ADDSUBPD256:
      *iopc = INTRN_ADDSUBPD256;
      break;
    case GSBI_IX86_BUILTIN_ADDSUBPS256:
      *iopc = INTRN_ADDSUBPS256;
      break;
    case GSBI_IX86_BUILTIN_ANDNPD256:
      *iopc = INTRN_ANDNPD256;
      break;
    case GSBI_IX86_BUILTIN_ANDNPS256:
      *iopc = INTRN_ANDNPS256;
      break;
    case GSBI_IX86_BUILTIN_ANDPD256:
    case GSBI_IX86_BUILTIN_ANDPS256:
      wn = WN_Band(res_type, arg0, arg1);
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_BLENDPD256:
      *iopc = INTRN_BLENDPD256;
      break;
    case GSBI_IX86_BUILTIN_BLENDPS256:
      *iopc = INTRN_BLENDPS256;
      break;
    case GSBI_IX86_BUILTIN_BLENDVPD256:
      *iopc = INTRN_BLENDVPD256;
      break;
    case GSBI_IX86_BUILTIN_BLENDVPS256:
      *iopc = INTRN_BLENDVPS256;
      break;
    case GSBI_IX86_BUILTIN_CMPPD:
    case GSBI_IX86_BUILTIN_CMPPD256:
      *iopc = INTRN_CMPPD;
      break;
    case GSBI_IX86_BUILTIN_CMPPS:
    case GSBI_IX86_BUILTIN_CMPPS256:
      *iopc = INTRN_CMPPS;
      break;
    case GSBI_IX86_BUILTIN_CMPSD:
      *iopc = INTRN_CMPSD;
      break;
    case GSBI_IX86_BUILTIN_CMPSS:
      *iopc = INTRN_CMPSS;
      break;
    case GSBI_IX86_BUILTIN_CVTDQ2PD256:
      *iopc = INTRN_CVTDQ2PD256;
      break;
    case GSBI_IX86_BUILTIN_CVTDQ2PS256:
      *iopc = INTRN_CVTDQ2PS256;
      break;
    case GSBI_IX86_BUILTIN_CVTPD2DQ256:
      *iopc = INTRN_CVTPD2DQ256;
      break;
    case GSBI_IX86_BUILTIN_CVTPD2PS256:
      *iopc = INTRN_CVTPD2PS256;
      break;
    case GSBI_IX86_BUILTIN_CVTPS2DQ256:
      *iopc = INTRN_CVTPS2DQ256;
      break;
    case GSBI_IX86_BUILTIN_CVTPS2PD256:
      *iopc = INTRN_CVTPS2PD256;
      break;
    case GSBI_IX86_BUILTIN_CVTTPD2DQ256:
      *iopc = INTRN_CVTTPD2DQ256;
      break;
    case GSBI_IX86_BUILTIN_CVTTPS2DQ256:
      *iopc = INTRN_CVTTPS2DQ256;
      break;
    case GSBI_IX86_BUILTIN_DIVPD256:
    case GSBI_IX86_BUILTIN_DIVPS256:
      wn = WN_Div (res_type, arg0, arg1);
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_DPPS256:
      *iopc = INTRN_DPPS256;
      break;
    case GSBI_IX86_BUILTIN_HADDPD256:
      *iopc = INTRN_HADDPD256;
      break;
    case GSBI_IX86_BUILTIN_HADDPS256:
      *iopc = INTRN_HADDPS256;
      break;
    case GSBI_IX86_BUILTIN_HSUBPD256:
      *iopc = INTRN_HSUBPD256;
      break;
    case GSBI_IX86_BUILTIN_HSUBPS256:
      *iopc = INTRN_HSUBPS256;
      break;
    case GSBI_IX86_BUILTIN_LDDQU256:
      *iopc = INTRN_LDDQU256;
      break;
    case GSBI_IX86_BUILTIN_LOADDQU256:
      *iopc = INTRN_LOADDQU256;
      break;
    case GSBI_IX86_BUILTIN_LOADUPD256:
      *iopc = INTRN_LOADUPD256;
      break;
    case GSBI_IX86_BUILTIN_LOADUPS256:
      *iopc = INTRN_LOADUPS256;
      break;
    case GSBI_IX86_BUILTIN_MASKLOADPD:
      *iopc = INTRN_MASKLOADPD;
      break;
    case GSBI_IX86_BUILTIN_MASKLOADPD256:
      *iopc = INTRN_MASKLOADPD256;
      break;
    case GSBI_IX86_BUILTIN_MASKLOADPS:
      *iopc = INTRN_MASKLOADPS;
      break;
    case GSBI_IX86_BUILTIN_MASKLOADPS256:
      *iopc = INTRN_MASKLOADPS256;
      break;
    case GSBI_IX86_BUILTIN_MASKSTOREPD:
      *iopc = INTRN_MASKSTOREPD;
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_MASKSTOREPD256:
      *iopc = INTRN_MASKSTOREPD256;
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_MASKSTOREPS:
      *iopc = INTRN_MASKSTOREPS;
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_MASKSTOREPS256:
      *iopc = INTRN_MASKSTOREPS256;
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_MAXPD256:
    case GSBI_IX86_BUILTIN_MAXPS256:
      wn = WN_Binary (OPR_MAX, res_type, arg0, arg1);
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_MINPD256:
    case GSBI_IX86_BUILTIN_MINPS256:
      wn = WN_Binary (OPR_MIN, res_type, arg0, arg1);
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_MOVDDUP256:
      *iopc = INTRN_MOVDDUP256;
      break;
    case GSBI_IX86_BUILTIN_MOVMSKPD256:
      *iopc = INTRN_MOVMSKPD256;
      break;
    case GSBI_IX86_BUILTIN_MOVMSKPS256:
      *iopc = INTRN_MOVMSKPS256;
      break;
    case GSBI_IX86_BUILTIN_MOVNTDQ256:
      *iopc = INTRN_MOVNTDQ256;
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_MOVNTPD256:
      *iopc = INTRN_MOVNTPD256;
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_MOVNTPS256:
      *iopc = INTRN_MOVNTPS256;
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_MOVSHDUP256:
      *iopc = INTRN_MOVSHDUP256;
      break;
    case GSBI_IX86_BUILTIN_MOVSLDUP256:
      *iopc = INTRN_MOVSLDUP256;
      break;
    case GSBI_IX86_BUILTIN_MULPD256:
    case GSBI_IX86_BUILTIN_MULPS256:
      wn = WN_Mpy (res_type, arg0, arg1);
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_ORPD256:
      *iopc = INTRN_ORPD256;
      break;
    case GSBI_IX86_BUILTIN_ORPS256:
      *iopc = INTRN_ORPS256;
      break;
    case GSBI_IX86_BUILTIN_PD256_PD:
      *iopc = INTRN_PD256_PD;
      break;
    case GSBI_IX86_BUILTIN_PD_PD256:
      *iopc = INTRN_PD_PD256;
      break;
    case GSBI_IX86_BUILTIN_PS256_PS:
      *iopc = INTRN_PS256_PS;
      break;
    case GSBI_IX86_BUILTIN_PS_PS256:
      *iopc = INTRN_PS_PS256;
      break;
    case GSBI_IX86_BUILTIN_PTESTC256:
      *iopc = INTRN_PTESTC256;
      break;
    case GSBI_IX86_BUILTIN_PTESTNZC256:
      *iopc = INTRN_PTESTNZC256;
      break;
    case GSBI_IX86_BUILTIN_PTESTZ256:
      *iopc = INTRN_PTESTZ256;
      break;
    case GSBI_IX86_BUILTIN_RCPPS256:
      *iopc = INTRN_RCPPS256;
      break;
    case GSBI_IX86_BUILTIN_ROUNDPD256:
      *iopc = INTRN_ROUNDPD256;
      break;
    case GSBI_IX86_BUILTIN_ROUNDPS256:
      *iopc = INTRN_ROUNDPS256;
      break;
    case GSBI_IX86_BUILTIN_RSQRTPS256:
      *iopc = INTRN_RSQRTPS256;
      break;
    case GSBI_IX86_BUILTIN_RSQRTPS_NR256:
      *iopc = INTRN_RSQRTPS_NR256;
      break;
    case GSBI_IX86_BUILTIN_SHUFPD256:
      *iopc = INTRN_SHUFPD256;
      break;
    case GSBI_IX86_BUILTIN_SHUFPS256:
      *iopc = INTRN_SHUFPS256;
      break;
    case GSBI_IX86_BUILTIN_SI256_SI:
      *iopc = INTRN_SI256_SI;
      break;
    case GSBI_IX86_BUILTIN_SI_SI256:
      *iopc = INTRN_SI_SI256;
      break;
    case GSBI_IX86_BUILTIN_SQRTPD256:
      *iopc = INTRN_SQRTPD256;
      break;
    case GSBI_IX86_BUILTIN_SQRTPS256:
      *iopc = INTRN_SQRTPS256;
      break;
    case GSBI_IX86_BUILTIN_SQRTPS_NR256:
      *iopc = INTRN_SQRTPS_NR256;
      break;
    case GSBI_IX86_BUILTIN_STOREDQU256:
      *iopc = INTRN_STOREDQU256;
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_STOREUPD256:
      *iopc = INTRN_STOREUPD256;
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_STOREUPS256:
      *iopc = INTRN_STOREUPS256;
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_SUBPD256:
      *iopc = INTRN_SUBPD256;
      break;
    case GSBI_IX86_BUILTIN_SUBPS256:
      *iopc = INTRN_SUBPS256;
      break;
    case GSBI_IX86_BUILTIN_UNPCKHPD256:
      *iopc = INTRN_UNPCKHPD256;
      break;
    case GSBI_IX86_BUILTIN_UNPCKHPS256:
      *iopc = INTRN_UNPCKHPS256;
      break;
    case GSBI_IX86_BUILTIN_UNPCKLPD256:
      *iopc = INTRN_UNPCKLPD256;
      break;
    case GSBI_IX86_BUILTIN_UNPCKLPS256:
      *iopc = INTRN_UNPCKLPS256;
      break;
    case GSBI_IX86_BUILTIN_VBROADCASTPD256:
      *iopc = INTRN_VBROADCASTPD256;
      break;
    case GSBI_IX86_BUILTIN_VBROADCASTPS256:
      *iopc = INTRN_VBROADCASTPS256;
      break;
    case GSBI_IX86_BUILTIN_VBROADCASTSD256:
      *iopc = INTRN_VBROADCASTSD256;
      break;
    case GSBI_IX86_BUILTIN_VBROADCASTSS:
      *iopc = INTRN_VBROADCASTSS;
      break;
    case GSBI_IX86_BUILTIN_VBROADCASTSS256:
      *iopc = INTRN_VBROADCASTSS256;
      break;
    case GSBI_IX86_BUILTIN_EXTRACTF128PD256:
      *iopc = INTRN_EXTRACTF128PD256;
      break;
    case GSBI_IX86_BUILTIN_EXTRACTF128PS256:
      *iopc = INTRN_EXTRACTF128PS256;
      break;
    case GSBI_IX86_BUILTIN_EXTRACTF128SI256:
      *iopc = INTRN_EXTRACTF128SI256;
      break;
    case GSBI_IX86_BUILTIN_VINSERTF128PD256:
      *iopc = INTRN_VINSERTF128PD256;
      break;
    case GSBI_IX86_BUILTIN_VINSERTF128PS256:
      *iopc = INTRN_VINSERTF128PS256;
      break;
    case GSBI_IX86_BUILTIN_VINSERTF128SI256:
      *iopc = INTRN_VINSERTF128SI256;
      break;
    case GSBI_IX86_BUILTIN_VPERM2F128PD256:
      *iopc = INTRN_VPERM2F128PD256;
      break;
    case GSBI_IX86_BUILTIN_VPERM2F128PS256:
      *iopc = INTRN_VPERM2F128PS256;
      break;
    case GSBI_IX86_BUILTIN_VPERM2F128SI256:
      *iopc = INTRN_VPERM2F128SI256;
      break;
    case GSBI_IX86_BUILTIN_VPERMILPD:
      *iopc = INTRN_VPERMILPD;
      break;
    case GSBI_IX86_BUILTIN_VPERMILPD256:
      *iopc = INTRN_VPERMILPD256;
      break;
    case GSBI_IX86_BUILTIN_VPERMILPS:
      *iopc = INTRN_VPERMILPS;
      break;
    case GSBI_IX86_BUILTIN_VPERMILPS256:
      *iopc = INTRN_VPERMILPS256;
      break;
    case GSBI_IX86_BUILTIN_VPERMILVARPD:
      *iopc = INTRN_VPERMILVARPD;
      break;
    case GSBI_IX86_BUILTIN_VPERMILVARPD256:
      *iopc = INTRN_VPERMILVARPD256;
      break;
    case GSBI_IX86_BUILTIN_VPERMILVARPS:
      *iopc = INTRN_VPERMILVARPS;
      break;
    case GSBI_IX86_BUILTIN_VPERMILVARPS256:
      *iopc = INTRN_VPERMILVARPS256;
      break;
    case GSBI_IX86_BUILTIN_VTESTCPD:
      *iopc = INTRN_VTESTCPD;
      break;
    case GSBI_IX86_BUILTIN_VTESTCPD256:
      *iopc = INTRN_VTESTCPD256;
      break;
    case GSBI_IX86_BUILTIN_VTESTCPS:
      *iopc = INTRN_VTESTCPS;
      break;
    case GSBI_IX86_BUILTIN_VTESTCPS256:
      *iopc = INTRN_VTESTCPS256;
      break;
    case GSBI_IX86_BUILTIN_VTESTNZCPD:
      *iopc = INTRN_VTESTNZCPD;
      break;
    case GSBI_IX86_BUILTIN_VTESTNZCPD256:
      *iopc = INTRN_VTESTNZCPD256;
      break;
    case GSBI_IX86_BUILTIN_VTESTNZCPS:
      *iopc = INTRN_VTESTNZCPS;
      break;
    case GSBI_IX86_BUILTIN_VTESTNZCPS256:
      *iopc = INTRN_VTESTNZCPS256;
      break;
    case GSBI_IX86_BUILTIN_VTESTZPD:
      *iopc = INTRN_VTESTZPD;
      break;
    case GSBI_IX86_BUILTIN_VTESTZPD256:
      *iopc = INTRN_VTESTZPD256;
      break;
    case GSBI_IX86_BUILTIN_VTESTZPS:
      *iopc = INTRN_VTESTZPS;
      break;
    case GSBI_IX86_BUILTIN_VTESTZPS256:
      *iopc = INTRN_VTESTZPS256;
      break;
    case GSBI_IX86_BUILTIN_VZEROALL:
      *iopc = INTRN_VZEROALL;
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_VZEROUPPER:
      *iopc = INTRN_VZEROUPPER;
      *intrinsic_op = FALSE;
      break;
    case GSBI_IX86_BUILTIN_XORPD256:
      *iopc = INTRN_XORPD256;
      break;
    case GSBI_IX86_BUILTIN_XORPS256:
      *iopc = INTRN_XORPS256;
      break;

    // FMA3 intrinsics: form1
    case GSBI_IX86_BUILTIN_VFMADDPD_132:
      *iopc = INTRN_VFMADDPD_132;
      break;
    case GSBI_IX86_BUILTIN_VFMADDPD256_132:
      *iopc = INTRN_VFMADDPD256_132;
      break;
    case GSBI_IX86_BUILTIN_VFMADDPS_132:
      *iopc = INTRN_VFMADDPS_132;
      break;
    case GSBI_IX86_BUILTIN_VFMADDPS256_132:
      *iopc = INTRN_VFMADDPS256_132;
      break;
    case GSBI_IX86_BUILTIN_VFMADDSD_132:
      *iopc = INTRN_VFMADDSD_132;
      break;
    case GSBI_IX86_BUILTIN_VFMADDSS_132:
      *iopc = INTRN_VFMADDSS_132;
      break;
    case GSBI_IX86_BUILTIN_VFMADDSUBPD_132:
      *iopc = INTRN_VFMADDSUBPD_132;
      break;
    case GSBI_IX86_BUILTIN_VFMADDSUBPD256_132:
      *iopc = INTRN_VFMADDSUBPD256_132;
      break;
    case GSBI_IX86_BUILTIN_VFMADDSUBPS_132:
      *iopc = INTRN_VFMADDSUBPS_132;
      break;
    case GSBI_IX86_BUILTIN_VFMADDSUBPS256_132:
      *iopc = INTRN_VFMADDSUBPS256_132;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBADDPD_132:
      *iopc = INTRN_VFMSUBADDPD_132;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBADDPD256_132:
      *iopc = INTRN_VFMSUBADDPD256_132;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBADDPS_132:
      *iopc = INTRN_VFMSUBADDPS_132;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBADDPS256_132:
      *iopc = INTRN_VFMSUBADDPS256_132;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBPD_132:
      *iopc = INTRN_VFMSUBPD_132;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBPD256_132:
      *iopc = INTRN_VFMSUBPD256_132;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBPS_132:
      *iopc = INTRN_VFMSUBPS_132;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBPS256_132:
      *iopc = INTRN_VFMSUBPS256_132;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBSD_132:
      *iopc = INTRN_VFMSUBSD_132;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBSS_132:
      *iopc = INTRN_VFMSUBSS_132;
      break;
    case GSBI_IX86_BUILTIN_VFNMADDPD_132:
      *iopc = INTRN_VFNMADDPD_132;
      break;
    case GSBI_IX86_BUILTIN_VFNMADDPD256_132:
      *iopc = INTRN_VFNMADDPD256_132;
      break;
    case GSBI_IX86_BUILTIN_VFNMADDPS_132:
      *iopc = INTRN_VFNMADDPS_132;
      break;
    case GSBI_IX86_BUILTIN_VFNMADDPS256_132:
      *iopc = INTRN_VFNMADDPS256_132;
      break;
    case GSBI_IX86_BUILTIN_VFNMADDSD_132:
      *iopc = INTRN_VFNMADDSD_132;
      break;
    case GSBI_IX86_BUILTIN_VFNMADDSS_132:
      *iopc = INTRN_VFNMADDSS_132;
      break;
    case GSBI_IX86_BUILTIN_VFNMSUBPD_132:
      *iopc = INTRN_VFNMSUBPD_132;
      break;
    case GSBI_IX86_BUILTIN_VFNMSUBPD256_132:
      *iopc = INTRN_VFNMSUBPD256_132;
      break;
    case GSBI_IX86_BUILTIN_VFNMSUBPS_132:
      *iopc = INTRN_VFNMSUBPS_132;
      break;
    case GSBI_IX86_BUILTIN_VFNMSUBPS256_132:
      *iopc = INTRN_VFNMSUBPS256_132;
      break;
    case GSBI_IX86_BUILTIN_VFNMSUBSD_132:
      *iopc = INTRN_VFNMSUBSD_132;
      break;
    case GSBI_IX86_BUILTIN_VFNMSUBSS_132:
      *iopc = INTRN_VFNMSUBSS_132;
      break;
    // FMA3 intrinsics: form2
    case GSBI_IX86_BUILTIN_VFMADDPD_213:
      *iopc = INTRN_VFMADDPD_213;
      break;
    case GSBI_IX86_BUILTIN_VFMADDPD256_213:
      *iopc = INTRN_VFMADDPD256_213;
      break;
    case GSBI_IX86_BUILTIN_VFMADDPS_213:
      *iopc = INTRN_VFMADDPS_213;
      break;
    case GSBI_IX86_BUILTIN_VFMADDPS256_213:
      *iopc = INTRN_VFMADDPS256_213;
      break;
    case GSBI_IX86_BUILTIN_VFMADDSD_213:
      *iopc = INTRN_VFMADDSD_213;
      break;
    case GSBI_IX86_BUILTIN_VFMADDSS_213:
      *iopc = INTRN_VFMADDSS_213;
      break;
    case GSBI_IX86_BUILTIN_VFMADDSUBPD_213:
      *iopc = INTRN_VFMADDSUBPD_213;
      break;
    case GSBI_IX86_BUILTIN_VFMADDSUBPD256_213:
      *iopc = INTRN_VFMADDSUBPD256_213;
      break;
    case GSBI_IX86_BUILTIN_VFMADDSUBPS_213:
      *iopc = INTRN_VFMADDSUBPS_213;
      break;
    case GSBI_IX86_BUILTIN_VFMADDSUBPS256_213:
      *iopc = INTRN_VFMADDSUBPS256_213;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBADDPD_213:
      *iopc = INTRN_VFMSUBADDPD_213;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBADDPD256_213:
      *iopc = INTRN_VFMSUBADDPD256_213;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBADDPS_213:
      *iopc = INTRN_VFMSUBADDPS_213;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBADDPS256_213:
      *iopc = INTRN_VFMSUBADDPS256_213;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBPD_213:
      *iopc = INTRN_VFMSUBPD_213;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBPD256_213:
      *iopc = INTRN_VFMSUBPD256_213;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBPS_213:
      *iopc = INTRN_VFMSUBPS_213;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBPS256_213:
      *iopc = INTRN_VFMSUBPS256_213;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBSD_213:
      *iopc = INTRN_VFMSUBSD_213;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBSS_213:
      *iopc = INTRN_VFMSUBSS_213;
      break;
    case GSBI_IX86_BUILTIN_VFNMADDPD_213:
      *iopc = INTRN_VFNMADDPD_213;
      break;
    case GSBI_IX86_BUILTIN_VFNMADDPD256_213:
      *iopc = INTRN_VFNMADDPD256_213;
      break;
    case GSBI_IX86_BUILTIN_VFNMADDPS_213:
      *iopc = INTRN_VFNMADDPS_213;
      break;
    case GSBI_IX86_BUILTIN_VFNMADDPS256_213:
      *iopc = INTRN_VFNMADDPS256_213;
      break;
    case GSBI_IX86_BUILTIN_VFNMADDSD_213:
      *iopc = INTRN_VFNMADDSD_213;
      break;
    case GSBI_IX86_BUILTIN_VFNMADDSS_213:
      *iopc = INTRN_VFNMADDSS_213;
      break;
    case GSBI_IX86_BUILTIN_VFNMSUBPD_213:
      *iopc = INTRN_VFNMSUBPD_213;
      break;
    case GSBI_IX86_BUILTIN_VFNMSUBPD256_213:
      *iopc = INTRN_VFNMSUBPD256_213;
      break;
    case GSBI_IX86_BUILTIN_VFNMSUBPS_213:
      *iopc = INTRN_VFNMSUBPS_213;
      break;
    case GSBI_IX86_BUILTIN_VFNMSUBPS256_213:
      *iopc = INTRN_VFNMSUBPS256_213;
      break;
    case GSBI_IX86_BUILTIN_VFNMSUBSD_213:
      *iopc = INTRN_VFNMSUBSD_213;
      break;
    case GSBI_IX86_BUILTIN_VFNMSUBSS_213:
      *iopc = INTRN_VFNMSUBSS_213;
      break;
    // FMA3 intrinsics: form3
    case GSBI_IX86_BUILTIN_VFMADDPD_231:
      *iopc = INTRN_VFMADDPD_231;
      break;
    case GSBI_IX86_BUILTIN_VFMADDPD256_231:
      *iopc = INTRN_VFMADDPD256_231;
      break;
    case GSBI_IX86_BUILTIN_VFMADDPS_231:
      *iopc = INTRN_VFMADDPS_231;
      break;
    case GSBI_IX86_BUILTIN_VFMADDPS256_231:
      *iopc = INTRN_VFMADDPS256_231;
      break;
    case GSBI_IX86_BUILTIN_VFMADDSD_231:
      *iopc = INTRN_VFMADDSD_231;
      break;
    case GSBI_IX86_BUILTIN_VFMADDSS_231:
      *iopc = INTRN_VFMADDSS_231;
      break;
    case GSBI_IX86_BUILTIN_VFMADDSUBPD_231:
      *iopc = INTRN_VFMADDSUBPD_231;
      break;
    case GSBI_IX86_BUILTIN_VFMADDSUBPD256_231:
      *iopc = INTRN_VFMADDSUBPD256_231;
      break;
    case GSBI_IX86_BUILTIN_VFMADDSUBPS_231:
      *iopc = INTRN_VFMADDSUBPS_231;
      break;
    case GSBI_IX86_BUILTIN_VFMADDSUBPS256_231:
      *iopc = INTRN_VFMADDSUBPS256_231;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBADDPD_231:
      *iopc = INTRN_VFMSUBADDPD_231;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBADDPD256_231:
      *iopc = INTRN_VFMSUBADDPD256_231;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBADDPS_231:
      *iopc = INTRN_VFMSUBADDPS_231;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBADDPS256_231:
      *iopc = INTRN_VFMSUBADDPS256_231;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBPD_231:
      *iopc = INTRN_VFMSUBPD_231;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBPD256_231:
      *iopc = INTRN_VFMSUBPD256_231;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBPS_231:
      *iopc = INTRN_VFMSUBPS_231;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBPS256_231:
      *iopc = INTRN_VFMSUBPS256_231;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBSD_231:
      *iopc = INTRN_VFMSUBSD_231;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBSS_231:
      *iopc = INTRN_VFMSUBSS_231;
      break;
    case GSBI_IX86_BUILTIN_VFNMADDPD_231:
      *iopc = INTRN_VFNMADDPD_231;
      break;
    case GSBI_IX86_BUILTIN_VFNMADDPD256_231:
      *iopc = INTRN_VFNMADDPD256_231;
      break;
    case GSBI_IX86_BUILTIN_VFNMADDPS_231:
      *iopc = INTRN_VFNMADDPS_231;
      break;
    case GSBI_IX86_BUILTIN_VFNMADDPS256_231:
      *iopc = INTRN_VFNMADDPS256_231;
      break;
    case GSBI_IX86_BUILTIN_VFNMADDSD_231:
      *iopc = INTRN_VFNMADDSD_231;
      break;
    case GSBI_IX86_BUILTIN_VFNMADDSS_231:
      *iopc = INTRN_VFNMADDSS_231;
      break;
    case GSBI_IX86_BUILTIN_VFNMSUBPD_231:
      *iopc = INTRN_VFNMSUBPD_231;
      break;
    case GSBI_IX86_BUILTIN_VFNMSUBPD256_231:
      *iopc = INTRN_VFNMSUBPD256_231;
      break;
    case GSBI_IX86_BUILTIN_VFNMSUBPS_231:
      *iopc = INTRN_VFNMSUBPS_231;
      break;
    case GSBI_IX86_BUILTIN_VFNMSUBPS256_231:
      *iopc = INTRN_VFNMSUBPS256_231;
      break;
    case GSBI_IX86_BUILTIN_VFNMSUBSD_231:
      *iopc = INTRN_VFNMSUBSD_231;
      break;
    case GSBI_IX86_BUILTIN_VFNMSUBSS_231:
      *iopc = INTRN_VFNMSUBSS_231;
      break;

    // FMA4 intrinsics
    case GSBI_IX86_BUILTIN_VFMADDPD:
      *iopc = INTRN_VFMADDPD;
      break;
    case GSBI_IX86_BUILTIN_VFMADDPD256:
      *iopc = INTRN_VFMADDPD256;
      break;
    case GSBI_IX86_BUILTIN_VFMADDPS:
      *iopc = INTRN_VFMADDPS;
      break;
    case GSBI_IX86_BUILTIN_VFMADDPS256:
      *iopc = INTRN_VFMADDPS256;
      break;
    case GSBI_IX86_BUILTIN_VFMADDSD:
      *iopc = INTRN_VFMADDSD;
      break;
    case GSBI_IX86_BUILTIN_VFMADDSS:
      *iopc = INTRN_VFMADDSS;
      break;
    case GSBI_IX86_BUILTIN_VFMADDSUBPD:
      *iopc = INTRN_VFMADDSUBPD;
      break;
    case GSBI_IX86_BUILTIN_VFMADDSUBPD256:
      *iopc = INTRN_VFMADDSUBPD256;
      break;
    case GSBI_IX86_BUILTIN_VFMADDSUBPS:
      *iopc = INTRN_VFMADDSUBPS;
      break;
    case GSBI_IX86_BUILTIN_VFMADDSUBPS256:
      *iopc = INTRN_VFMADDSUBPS256;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBADDPD:
      *iopc = INTRN_VFMSUBADDPD;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBADDPD256:
      *iopc = INTRN_VFMSUBADDPD256;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBADDPS:
      *iopc = INTRN_VFMSUBADDPS;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBADDPS256:
      *iopc = INTRN_VFMSUBADDPS256;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBPD:
      *iopc = INTRN_VFMSUBPD;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBPD256:
      *iopc = INTRN_VFMSUBPD256;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBPS:
      *iopc = INTRN_VFMSUBPS;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBPS256:
      *iopc = INTRN_VFMSUBPS256;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBSD:
      *iopc = INTRN_VFMSUBSD;
      break;
    case GSBI_IX86_BUILTIN_VFMSUBSS:
      *iopc = INTRN_VFMSUBSS;
      break;
    case GSBI_IX86_BUILTIN_VFNMADDPD:
      *iopc = INTRN_VFNMADDPD;
      break;
    case GSBI_IX86_BUILTIN_VFNMADDPD256:
      *iopc = INTRN_VFNMADDPD256;
      break;
    case GSBI_IX86_BUILTIN_VFNMADDPS:
      *iopc = INTRN_VFNMADDPS;
      break;
    case GSBI_IX86_BUILTIN_VFNMADDPS256:
      *iopc = INTRN_VFNMADDPS256;
      break;
    case GSBI_IX86_BUILTIN_VFNMADDSD:
      *iopc = INTRN_VFNMADDSD;
      break;
    case GSBI_IX86_BUILTIN_VFNMADDSS:
      *iopc = INTRN_VFNMADDSS;
      break;
    case GSBI_IX86_BUILTIN_VFNMSUBPD:
      *iopc = INTRN_VFNMSUBPD;
      break;
    case GSBI_IX86_BUILTIN_VFNMSUBPD256:
      *iopc = INTRN_VFNMSUBPD256;
      break;
    case GSBI_IX86_BUILTIN_VFNMSUBPS:
      *iopc = INTRN_VFNMSUBPS;
      break;
    case GSBI_IX86_BUILTIN_VFNMSUBPS256:
      *iopc = INTRN_VFNMSUBPS256;
      break;
    case GSBI_IX86_BUILTIN_VFNMSUBSD:
      *iopc = INTRN_VFNMSUBSD;
      break;
    case GSBI_IX86_BUILTIN_VFNMSUBSS:
      *iopc = INTRN_VFNMSUBSS;
      break;

    // XOP intrinsics
    case GSBI_IX86_BUILTIN_VFRCZPD:
      *iopc = INTRN_VFRCZPD;
      break;
    case GSBI_IX86_BUILTIN_VFRCZPD256:
      *iopc = INTRN_VFRCZPD256;
      break;
    case GSBI_IX86_BUILTIN_VFRCZPS:
      *iopc = INTRN_VFRCZPS;
      break;
    case GSBI_IX86_BUILTIN_VFRCZPS256:
      *iopc = INTRN_VFRCZPS256;
      break;
    case GSBI_IX86_BUILTIN_VFRCZSD:
      *iopc = INTRN_VFRCZSD;
      break;
    case GSBI_IX86_BUILTIN_VFRCZSS:
      *iopc = INTRN_VFRCZSS;
      break;
    case GSBI_IX86_BUILTIN_VPCMOV:
      *iopc = INTRN_VPCMOV;
      break;
    case GSBI_IX86_BUILTIN_VPCMOV256:
      *iopc = INTRN_VPCMOV256;
      break;
    case GSBI_IX86_BUILTIN_VPCMOV_V16HI256:
      *iopc = INTRN_VPCMOV_V16HI256;
      break;
    case GSBI_IX86_BUILTIN_VPCMOV_V16QI:
      *iopc = INTRN_VPCMOV_V16QI;
      break;
    case GSBI_IX86_BUILTIN_VPCMOV_V2DF:
      *iopc = INTRN_VPCMOV_V2DF;
      break;
    case GSBI_IX86_BUILTIN_VPCMOV_V2DI:
      *iopc = INTRN_VPCMOV_V2DI;
      break;
    case GSBI_IX86_BUILTIN_VPCMOV_V32QI256:
      *iopc = INTRN_VPCMOV_V32QI256;
      break;
    case GSBI_IX86_BUILTIN_VPCMOV_V4DF256:
      *iopc = INTRN_VPCMOV_V4DF256;
      break;
    case GSBI_IX86_BUILTIN_VPCMOV_V4DI256:
      *iopc = INTRN_VPCMOV_V4DI256;
      break;
    case GSBI_IX86_BUILTIN_VPCMOV_V4SF:
      *iopc = INTRN_VPCMOV_V4SF;
      break;
    case GSBI_IX86_BUILTIN_VPCMOV_V4SI:
      *iopc = INTRN_VPCMOV_V4SI;
      break;
    case GSBI_IX86_BUILTIN_VPCMOV_V8HI:
      *iopc = INTRN_VPCMOV_V8HI;
      break;
    case GSBI_IX86_BUILTIN_VPCMOV_V8SF256:
      *iopc = INTRN_VPCMOV_V8SF256;
      break;
    case GSBI_IX86_BUILTIN_VPCMOV_V8SI256:
      *iopc = INTRN_VPCMOV_V8SI256;
      break;
    case GSBI_IX86_BUILTIN_VPCOMEQB:
      *iopc = INTRN_VPCOMEQB;
      break;
    case GSBI_IX86_BUILTIN_VPCOMEQD:
      *iopc = INTRN_VPCOMEQD;
      break;
    case GSBI_IX86_BUILTIN_VPCOMEQQ:
      *iopc = INTRN_VPCOMEQQ;
      break;
    case GSBI_IX86_BUILTIN_VPCOMEQUB:
      *iopc = INTRN_VPCOMEQUB;
      break;
    case GSBI_IX86_BUILTIN_VPCOMEQUD:
      *iopc = INTRN_VPCOMEQUD;
      break;
    case GSBI_IX86_BUILTIN_VPCOMEQUQ:
      *iopc = INTRN_VPCOMEQUQ;
      break;
    case GSBI_IX86_BUILTIN_VPCOMEQUW:
      *iopc = INTRN_VPCOMEQUW;
      break;
    case GSBI_IX86_BUILTIN_VPCOMEQW:
      *iopc = INTRN_VPCOMEQW;
      break;
    case GSBI_IX86_BUILTIN_VPCOMFALSEB:
      *iopc = INTRN_VPCOMFALSEB;
      break;
    case GSBI_IX86_BUILTIN_VPCOMFALSED:
      *iopc = INTRN_VPCOMFALSED;
      break;
    case GSBI_IX86_BUILTIN_VPCOMFALSEQ:
      *iopc = INTRN_VPCOMFALSEQ;
      break;
    case GSBI_IX86_BUILTIN_VPCOMFALSEUB:
      *iopc = INTRN_VPCOMFALSEUB;
      break;
    case GSBI_IX86_BUILTIN_VPCOMFALSEUD:
      *iopc = INTRN_VPCOMFALSEUD;
      break;
    case GSBI_IX86_BUILTIN_VPCOMFALSEUQ:
      *iopc = INTRN_VPCOMFALSEUQ;
      break;
    case GSBI_IX86_BUILTIN_VPCOMFALSEUW:
      *iopc = INTRN_VPCOMFALSEUW;
      break;
    case GSBI_IX86_BUILTIN_VPCOMFALSEW:
      *iopc = INTRN_VPCOMFALSEW;
      break;
    case GSBI_IX86_BUILTIN_VPCOMGEB:
      *iopc = INTRN_VPCOMGEB;
      break;
    case GSBI_IX86_BUILTIN_VPCOMGED:
      *iopc = INTRN_VPCOMGED;
      break;
    case GSBI_IX86_BUILTIN_VPCOMGEQ:
      *iopc = INTRN_VPCOMGEQ;
      break;
    case GSBI_IX86_BUILTIN_VPCOMGEUB:
      *iopc = INTRN_VPCOMGEUB;
      break;
    case GSBI_IX86_BUILTIN_VPCOMGEUD:
      *iopc = INTRN_VPCOMGEUD;
      break;
    case GSBI_IX86_BUILTIN_VPCOMGEUQ:
      *iopc = INTRN_VPCOMGEUQ;
      break;
    case GSBI_IX86_BUILTIN_VPCOMGEUW:
      *iopc = INTRN_VPCOMGEUW;
      break;
    case GSBI_IX86_BUILTIN_VPCOMGEW:
      *iopc = INTRN_VPCOMGEW;
      break;
    case GSBI_IX86_BUILTIN_VPCOMGTB:
      *iopc = INTRN_VPCOMGTB;
      break;
    case GSBI_IX86_BUILTIN_VPCOMGTD:
      *iopc = INTRN_VPCOMGTD;
      break;
    case GSBI_IX86_BUILTIN_VPCOMGTQ:
      *iopc = INTRN_VPCOMGTQ;
      break;
    case GSBI_IX86_BUILTIN_VPCOMGTUB:
      *iopc = INTRN_VPCOMGTUB;
      break;
    case GSBI_IX86_BUILTIN_VPCOMGTUD:
      *iopc = INTRN_VPCOMGTUD;
      break;
    case GSBI_IX86_BUILTIN_VPCOMGTUQ:
      *iopc = INTRN_VPCOMGTUQ;
      break;
    case GSBI_IX86_BUILTIN_VPCOMGTUW:
      *iopc = INTRN_VPCOMGTUW;
      break;
    case GSBI_IX86_BUILTIN_VPCOMGTW:
      *iopc = INTRN_VPCOMGTW;
      break;
    case GSBI_IX86_BUILTIN_VPCOMLEB:
      *iopc = INTRN_VPCOMLEB;
      break;
    case GSBI_IX86_BUILTIN_VPCOMLED:
      *iopc = INTRN_VPCOMLED;
      break;
    case GSBI_IX86_BUILTIN_VPCOMLEQ:
      *iopc = INTRN_VPCOMLEQ;
      break;
    case GSBI_IX86_BUILTIN_VPCOMLEUB:
      *iopc = INTRN_VPCOMLEUB;
      break;
    case GSBI_IX86_BUILTIN_VPCOMLEUD:
      *iopc = INTRN_VPCOMLEUD;
      break;
    case GSBI_IX86_BUILTIN_VPCOMLEUQ:
      *iopc = INTRN_VPCOMLEUQ;
      break;
    case GSBI_IX86_BUILTIN_VPCOMLEUW:
      *iopc = INTRN_VPCOMLEUW;
      break;
    case GSBI_IX86_BUILTIN_VPCOMLEW:
      *iopc = INTRN_VPCOMLEW;
      break;
    case GSBI_IX86_BUILTIN_VPCOMLTB:
      *iopc = INTRN_VPCOMLTB;
      break;
    case GSBI_IX86_BUILTIN_VPCOMLTD:
      *iopc = INTRN_VPCOMLTD;
      break;
    case GSBI_IX86_BUILTIN_VPCOMLTQ:
      *iopc = INTRN_VPCOMLTQ;
      break;
    case GSBI_IX86_BUILTIN_VPCOMLTUB:
      *iopc = INTRN_VPCOMLTUB;
      break;
    case GSBI_IX86_BUILTIN_VPCOMLTUD:
      *iopc = INTRN_VPCOMLTUD;
      break;
    case GSBI_IX86_BUILTIN_VPCOMLTUQ:
      *iopc = INTRN_VPCOMLTUQ;
      break;
    case GSBI_IX86_BUILTIN_VPCOMLTUW:
      *iopc = INTRN_VPCOMLTUW;
      break;
    case GSBI_IX86_BUILTIN_VPCOMLTW:
      *iopc = INTRN_VPCOMLTW;
      break;
    case GSBI_IX86_BUILTIN_VPCOMNEB:
      *iopc = INTRN_VPCOMNEB;
      break;
    case GSBI_IX86_BUILTIN_VPCOMNED:
      *iopc = INTRN_VPCOMNED;
      break;
    case GSBI_IX86_BUILTIN_VPCOMNEQ:
      *iopc = INTRN_VPCOMNEQ;
      break;
    case GSBI_IX86_BUILTIN_VPCOMNEUB:
      *iopc = INTRN_VPCOMNEUB;
      break;
    case GSBI_IX86_BUILTIN_VPCOMNEUD:
      *iopc = INTRN_VPCOMNEUD;
      break;
    case GSBI_IX86_BUILTIN_VPCOMNEUQ:
      *iopc = INTRN_VPCOMNEUQ;
      break;
    case GSBI_IX86_BUILTIN_VPCOMNEUW:
      *iopc = INTRN_VPCOMNEUW;
      break;
    case GSBI_IX86_BUILTIN_VPCOMNEW:
      *iopc = INTRN_VPCOMNEW;
      break;
    case GSBI_IX86_BUILTIN_VPCOMTRUEB:
      *iopc = INTRN_VPCOMTRUEB;
      break;
    case GSBI_IX86_BUILTIN_VPCOMTRUED:
      *iopc = INTRN_VPCOMTRUED;
      break;
    case GSBI_IX86_BUILTIN_VPCOMTRUEQ:
      *iopc = INTRN_VPCOMTRUEQ;
      break;
    case GSBI_IX86_BUILTIN_VPCOMTRUEUB:
      *iopc = INTRN_VPCOMTRUEUB;
      break;
    case GSBI_IX86_BUILTIN_VPCOMTRUEUD:
      *iopc = INTRN_VPCOMTRUEUD;
      break;
    case GSBI_IX86_BUILTIN_VPCOMTRUEUQ:
      *iopc = INTRN_VPCOMTRUEUQ;
      break;
    case GSBI_IX86_BUILTIN_VPCOMTRUEUW:
      *iopc = INTRN_VPCOMTRUEUW;
      break;
    case GSBI_IX86_BUILTIN_VPCOMTRUEW:
      *iopc = INTRN_VPCOMTRUEW;
      break;
    case GSBI_IX86_BUILTIN_VPHADDBD:
      *iopc = INTRN_VPHADDBD;
      break;
    case GSBI_IX86_BUILTIN_VPHADDBQ:
      *iopc = INTRN_VPHADDBQ;
      break;
    case GSBI_IX86_BUILTIN_VPHADDBW:
      *iopc = INTRN_VPHADDBW;
      break;
    case GSBI_IX86_BUILTIN_VPHADDDQ:
      *iopc = INTRN_VPHADDDQ;
      break;
    case GSBI_IX86_BUILTIN_VPHADDUBD:
      *iopc = INTRN_VPHADDUBD;
      break;
    case GSBI_IX86_BUILTIN_VPHADDUBQ:
      *iopc = INTRN_VPHADDUBQ;
      break;
    case GSBI_IX86_BUILTIN_VPHADDUBW:
      *iopc = INTRN_VPHADDUBW;
      break;
    case GSBI_IX86_BUILTIN_VPHADDUDQ:
      *iopc = INTRN_VPHADDUDQ;
      break;
    case GSBI_IX86_BUILTIN_VPHADDUWD:
      *iopc = INTRN_VPHADDUWD;
      break;
    case GSBI_IX86_BUILTIN_VPHADDUWQ:
      *iopc = INTRN_VPHADDUWQ;
      break;
    case GSBI_IX86_BUILTIN_VPHADDWD:
      *iopc = INTRN_VPHADDWD;
      break;
    case GSBI_IX86_BUILTIN_VPHADDWQ:
      *iopc = INTRN_VPHADDWQ;
      break;
    case GSBI_IX86_BUILTIN_VPHSUBBW:
      *iopc = INTRN_VPHSUBBW;
      break;
    case GSBI_IX86_BUILTIN_VPHSUBDQ:
      *iopc = INTRN_VPHSUBDQ;
      break;
    case GSBI_IX86_BUILTIN_VPHSUBWD:
      *iopc = INTRN_VPHSUBWD;
      break;
    case GSBI_IX86_BUILTIN_VPMACSDD:
      *iopc = INTRN_VPMACSDD;
      break;
    case GSBI_IX86_BUILTIN_VPMACSDQH:
      *iopc = INTRN_VPMACSDQH;
      break;
    case GSBI_IX86_BUILTIN_VPMACSDQL:
      *iopc = INTRN_VPMACSDQL;
      break;
    case GSBI_IX86_BUILTIN_VPMACSSDD:
      *iopc = INTRN_VPMACSSDD;
      break;
    case GSBI_IX86_BUILTIN_VPMACSSDQH:
      *iopc = INTRN_VPMACSSDQH;
      break;
    case GSBI_IX86_BUILTIN_VPMACSSDQL:
      *iopc = INTRN_VPMACSSDQL;
      break;
    case GSBI_IX86_BUILTIN_VPMACSSWD:
      *iopc = INTRN_VPMACSSWD;
      break;
    case GSBI_IX86_BUILTIN_VPMACSSWW:
      *iopc = INTRN_VPMACSSWW;
      break;
    case GSBI_IX86_BUILTIN_VPMACSWD:
      *iopc = INTRN_VPMACSWD;
      break;
    case GSBI_IX86_BUILTIN_VPMACSWW:
      *iopc = INTRN_VPMACSWW;
      break;
    case GSBI_IX86_BUILTIN_VPMADCSSWD:
      *iopc = INTRN_VPMADCSSWD;
      break;
    case GSBI_IX86_BUILTIN_VPMADCSWD:
      *iopc = INTRN_VPMADCSWD;
      break;
    case GSBI_IX86_BUILTIN_VPPERM:
      *iopc = INTRN_VPPERM;
      break;
    case GSBI_IX86_BUILTIN_VPROTB:
      *iopc = INTRN_VPROTB;
      break;
    case GSBI_IX86_BUILTIN_VPROTB_IMM:
      *iopc = INTRN_VPROTB_IMM;
      break;
    case GSBI_IX86_BUILTIN_VPROTD:
      *iopc = INTRN_VPROTD;
      break;
    case GSBI_IX86_BUILTIN_VPROTD_IMM:
      *iopc = INTRN_VPROTD_IMM;
      break;
    case GSBI_IX86_BUILTIN_VPROTQ:
      *iopc = INTRN_VPROTQ;
      break;
    case GSBI_IX86_BUILTIN_VPROTQ_IMM:
      *iopc = INTRN_VPROTQ_IMM;
      break;
    case GSBI_IX86_BUILTIN_VPROTW:
      *iopc = INTRN_VPROTW;
      break;
    case GSBI_IX86_BUILTIN_VPROTW_IMM:
      *iopc = INTRN_VPROTW_IMM;
      break;
    case GSBI_IX86_BUILTIN_VPSHAB:
      *iopc = INTRN_VPSHAB;
      break;
    case GSBI_IX86_BUILTIN_VPSHAD:
      *iopc = INTRN_VPSHAD;
      break;
    case GSBI_IX86_BUILTIN_VPSHAQ:
      *iopc = INTRN_VPSHAQ;
      break;
    case GSBI_IX86_BUILTIN_VPSHAW:
      *iopc = INTRN_VPSHAW;
      break;
    case GSBI_IX86_BUILTIN_VPSHLB:
      *iopc = INTRN_VPSHLB;
      break;
    case GSBI_IX86_BUILTIN_VPSHLD:
      *iopc = INTRN_VPSHLD;
      break;
    case GSBI_IX86_BUILTIN_VPSHLQ:
      *iopc = INTRN_VPSHLQ;
      break;
    case GSBI_IX86_BUILTIN_VPSHLW:
      *iopc = INTRN_VPSHLW;
      break;

#endif

    default:
unsupported:
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
  BOOL is_mmx = FALSE;
  switch (ins_code)
  {
    case GSBI_IX86_BUILTIN_PSRAW:
    case GSBI_IX86_BUILTIN_PSRAD:
    case GSBI_IX86_BUILTIN_PSLLW:
    case GSBI_IX86_BUILTIN_PSLLD:
    case GSBI_IX86_BUILTIN_PSRLW:
    case GSBI_IX86_BUILTIN_PSRLD:
      is_mmx = TRUE;
      // fall thr
      Is_True (wn == NULL, ("WGEN_target_builtins: null WN expected"));
      WN * args[2];
      // 1st argument
      TY_IDX arg_ty_idx = Get_TY (gs_tree_type (gs_tree_value (t_list)));
      TYPE_ID arg_mtype = TY_mtype (arg_ty_idx);
      args[0] = WN_CreateParm (Mtype_comparison (arg_mtype), arg0,
                               arg_ty_idx, WN_PARM_BY_VALUE);
                                                                                
      // 2nd argument
      arg1 = WN_Cvt (WN_rtype(arg1), 
                     is_mmx ? MTYPE_M8I4 : MTYPE_V16I8, arg1);
      arg_ty_idx = MTYPE_TO_TY_array[WN_rtype (arg1)];
      arg_mtype = WN_rtype (arg1);
      args[1] = WN_CreateParm (Mtype_comparison (arg_mtype), arg1,
                               arg_ty_idx, WN_PARM_BY_VALUE);
                                                                                
      wn = WN_Create_Intrinsic (OPR_INTRINSIC_OP, res_type, MTYPE_V,
                                *iopc, 2, args);
      break;
  }
  
  // The following instructions expect both arguments are MMX, but both of the 
  // argument type for the builtin is INT, so we insert a CVT here.
  switch(ins_code)
  {
    TY_IDX arg_ty_idx;
    TYPE_ID arg_mtype;
    case GSBI_IX86_BUILTIN_PADDQ:
    case GSBI_IX86_BUILTIN_PSUBQ:
    case GSBI_IX86_BUILTIN_PSLLQ:
    case GSBI_IX86_BUILTIN_PSRLQ:
      Is_True (wn == NULL, ("WGEN_target_builtins: null WN expected"));
      WN* args[2];
      // 1st argument
      arg0 = WN_Cvt (WN_rtype(arg0), MTYPE_M8I4, arg0);
      arg_ty_idx = MTYPE_TO_TY_array[WN_rtype (arg0)];
      arg_mtype = WN_rtype (arg0);
      args[0] = WN_CreateParm (Mtype_comparison (arg_mtype), arg0,
                               arg_ty_idx, WN_PARM_BY_VALUE);
      // 2nd argument
      arg1 = WN_Cvt (WN_rtype(arg1), MTYPE_M8I4, arg1);
      arg_ty_idx = MTYPE_TO_TY_array[WN_rtype (arg1)];
      arg_mtype = WN_rtype (arg1);
      args[1] = WN_CreateParm (Mtype_comparison (arg_mtype), arg1,
                               arg_ty_idx, WN_PARM_BY_VALUE);

      wn = WN_Create_Intrinsic (OPR_INTRINSIC_OP, MTYPE_M8I4, MTYPE_V,
                                *iopc, 2, args);
      break;
  }

  return wn;
}
#endif // TARG_X8664

// return the very first statement under a COMPOUND_EXPR node
gs_t first_in_compound_expr(gs_t node)
{
  gs_t first = gs_tree_operand(node, 0);
  while (gs_tree_code(first) == GS_COMPOUND_EXPR)
    first = gs_tree_operand(first, 0);
  return first;
}

#ifdef KEY
extern BOOL processing_function_prototype;

#ifdef FE_GNU_4_2_0
static inline BOOL
expr_is_final_value (gs_t expr)
{
  if (expr == NULL /* TODO: || IS_EMPTY_STMT (expr)*/)
    return FALSE;

  switch (gs_tree_code (expr))
  {
    case GS_BIND_EXPR:
    case GS_CLEANUP_POINT_EXPR:
    case GS_TRY_FINALLY_EXPR:
    case GS_TRY_CATCH_EXPR:
    case GS_STATEMENT_LIST:
    case GS_COMPOUND_EXPR:
      return FALSE;
    default:
      return TRUE;
  }
}

// Mimick voidify_wrapper_expr() to obtain any value from the statement.
static WN *
get_wrapper_value (gs_t stmt)
{
  Is_True (gs_tree_code(stmt) == GS_BIND_EXPR,
           ("get_wrapper_value() only accepts BIND_EXPR"));

  gs_t type = gs_tree_type (stmt);
  gs_t temp = NULL;
  if (type && gs_tree_code (type) != GS_VOID_TYPE)
  {
    gs_t p;

    // Traverse and go deeper looking for a value.
    for (p = stmt; p; )
    {
      switch (gs_tree_code (p))
      {
        case GS_BIND_EXPR:
          if (expr_is_final_value (gs_bind_expr_body (p)))
          {
            temp = gs_build_decl (GS_VAR_DECL, type);
            gs_t s = gs_build_2 (GS_TCC_EXPRESSION, GS_INIT_EXPR, temp,
                            gs_bind_expr_body(p));
            gs_set_operand(s, GS_TREE_TYPE, type);  
            gs_set_operand(p, GS_BIND_EXPR_BODY, s);
            goto out;
          }
          else
            p = gs_bind_expr_body (p);
          break;

        case GS_CLEANUP_POINT_EXPR:
        case GS_TRY_FINALLY_EXPR:
        case GS_TRY_CATCH_EXPR:
          if (expr_is_final_value (gs_tree_operand (p, 0)))
          {
            temp = gs_build_decl (GS_VAR_DECL, type);
            gs_t s = gs_build_2 (GS_TCC_EXPRESSION, GS_INIT_EXPR, temp,
                            gs_tree_operand(p, 0));
            gs_set_operand(s, GS_TREE_TYPE, type);  
            gs_set_tree_operand(p, 0, s);
            goto out;
          }
          else
            p = gs_tree_operand (p, 0);
          break;

        case GS_STATEMENT_LIST:
        {
	  gs_t stmt_list = gs_statement_list_elts(p);
	  for (gs_t list = stmt_list; gs_code(list) != EMPTY; 
	       list = gs_operand(list, 1))
          {
	    if (gs_code (gs_operand(list, 1)) == EMPTY /* last stmt ? */)
            {
              p = gs_operand(list, 0);
              if (expr_is_final_value (p))
              {
                temp = gs_build_decl (GS_VAR_DECL, type);
                p = gs_build_2 (GS_TCC_EXPRESSION, GS_INIT_EXPR, temp, p);
                gs_set_operand(p, GS_TREE_TYPE, type);  
                gs_set_operand(list, 0, p);
                goto out;
              }
              break;
            }
          }
        }
        break;

        case GS_COMPOUND_EXPR:
        {
          gs_t e = p;
          for (; gs_tree_code (p) == GS_COMPOUND_EXPR; p = gs_tree_operand (p, 1))
            e = p;
          if (expr_is_final_value (p))
          {
            temp = gs_build_decl (GS_VAR_DECL, type);
            p = gs_build_2 (GS_TCC_EXPRESSION, GS_INIT_EXPR, temp, p);
            gs_set_operand(p, GS_TREE_TYPE, type);  
            gs_set_tree_operand(e, 1, p);
            goto out;
          }
        }
        break;

        default:
          // expr_is_final_value() ensures we don't reach here.
          Is_True (FALSE, ("get_wrapper_value() should not reach here"));
      }
    }

    out:

      if (!temp)
        return NULL;

      ST * s = Get_ST(temp);
      TY_IDX ty_idx = ST_type(s);
      return WN_Ldid(TY_mtype(ty_idx), 0, s, ty_idx);
  }

  return NULL;
}
#endif // FE_GNU_4_2_0
#endif // KEY


// Following function is a quick kludge to bug# 586 reported in bugs.open64.net.
// The problem is that open64's backend currently does not fold 
// __builtin_object_size() into constant, causing "unresolved symbol" problem 
// at link time.
//
// This function is part of GCC extension; gcc tries to fold this function in 
// many places (see fold_builtin_object_size()@builtins.c for details), and it 
// has a pass called "objsz" dedicated to that purpose (see tree-object-size.c for 
// details).
//
// fold_builtin_object_size() is called prior to the generic -> gspin conversion;
// it is able to fold some simple cases; but in general, most cases have to handled
// by the "objsz" pass which relies on SSA. 
//
//   The value the function "__builtin_object_size(void* p, int type)" evaluated to 
// depends on the capability of compiler data flow analysis. Basically, if compiler
// reveals that pointer <p> points to object O with type T, it will return sizeof(T);
// otherwise it returns "type < 2 ? -1 : 0".
//
//  This function simply substitute the "__builtin_object_size(p, ty)" with 
//  "ty < 2 ? -1 : 0".
//  
static WN*
Fold_Object_Size (WN* intrinsic_op) {

    Is_True (WN_operator (intrinsic_op) == OPR_INTRINSIC_OP && 
             WN_intrinsic (intrinsic_op) ==  INTRN_OBJECT_SIZE,
             ("Invalid intrisic op"));
    
    // the rtype should be the type corresponing to high level type size_t
    //
    TYPE_ID rtype = WN_rtype (intrinsic_op);

    // step 1: Get the 2nd actual of __builtin_object_size()
    //
    WN* arg1 = WN_kid0(WN_kid1(intrinsic_op));
    arg1 = WN_COPY_Tree (arg1);
    WN_DELETE_Tree (intrinsic_op);

    // step 2: construct expression : "<arg1> < 2 ? -1 : 0"
    // 

    // step 2.1 create condition "<arg1> < 2".
    //
    WN* cond = WN_CreateExp2 (OPC_BI4LT,
                              WN_Type_Conversion (arg1, MTYPE_I4),
                              WN_CreateIntconst (OPC_I4INTCONST, 2));
    
    // step 2.2 create the selection exp
    WN* res = WN_CreateExp3 (OPR_SELECT, MTYPE_I4, MTYPE_V, 
                             cond,
                             WN_CreateIntconst (OPC_I4INTCONST, -1),
                             WN_CreateIntconst (OPC_I4INTCONST, 0));

    res = WN_Type_Conversion (res, rtype);
    return res;
}

WN * 
WGEN_Expand_Expr (gs_t exp,
		  bool need_result,
		  TY_IDX nop_ty_idx,
		  TY_IDX component_ty_idx,
		  WN* component_offset,
		  UINT32 field_id ,
		  bool is_bit_field,
		  bool is_aggr_init_via_ctor,
		  WN *target_wn)
{
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

  TRACE_EXPAND_GS(exp);
  switch(code)
  {
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
    case GS_FDESC_EXPR:
      {
    	 gs_t exp_operand = gs_tree_operand (exp, 0);
         FmtAssert( gs_tree_code(exp_operand) == GS_FUNCTION_DECL,("Unexpected Tree Code!!"));
         st = Get_ST (exp_operand);
         ty_idx = ST_type (st);
         wn = WN_Lda (Pointer_Mtype, ST_ofst(st), st);
      }
      break;
#endif

    case GS_FUNCTION_DECL:
      {
	 st = Get_ST (exp);
	 ty_idx = ST_type (st);
	 wn = WN_Lda (Pointer_Mtype, ST_ofst(st), st);
      }
      break;

    case GS_TREE_LIST: 
      {
	gs_t stmt;
	for (stmt = gs_tree_purpose(exp); stmt; stmt = gs_tree_chain(stmt))
	  WGEN_Expand_Stmt (stmt);
	wn = WGEN_Expand_Expr (gs_tree_value(exp));
      }
      break;

    case GS_DECL_EXPR:
      {
        gs_t decl = gs_decl_expr_decl(exp);
	WGEN_Expand_Decl (decl, TRUE);
	wn = WGEN_Expand_Expr (decl);
      }
      break;

    case GS_BIND_EXPR:
      {
	gs_t body;

	Register_Cleanup(exp); // KEY bug 11188
	body = gs_bind_expr_body(exp);
#ifdef FE_GNU_4_2_0
	// Bugs 12698, 13979: Try to get the value of the bind_expr.
	wn = get_wrapper_value(exp);
#endif
	if (gs_tree_code(body) != GS_STATEMENT_LIST)
	  WGEN_Expand_Stmt(body); // only 1 statement
	else {
	  gs_t stmt_list = gs_statement_list_elts(body);
	  gs_t list;
	  for (list = stmt_list; gs_code(list) != EMPTY; 
	       list = gs_operand(list, 1)) {
	    gs_t stmt = gs_operand(list, 0);
	    WGEN_Expand_Stmt(stmt);
	  }
	}
	Unregister_Cleanup(); // KEY bug 11188
      }
      break;

    case GS_TARGET_EXPR:
      {
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
	      gs_t ptr_var = gs_build_decl(GS_VAR_DECL,
				    gs_build_pointer_type(gs_tree_type(opnd0)));
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
	    gs_tree_code(opnd0) != GS_COMPONENT_REF &&
	    gs_tree_code(opnd0) != GS_ARRAY_REF)
#endif
	{
	st    = Get_ST (gs_tree_operand(exp, 0));
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
	  WGEN_Expand_Expr (t, false, 0, 0, 0, 0, false, true);
	}
	else 
	{
#ifdef KEY
	  gs_t ret_type = NULL;
	  if (gs_tree_code(t) == GS_AGGR_INIT_EXPR)
	  { // bug 11159: Get the return type.
	    ret_type = gs_tree_type
	                (gs_tree_type
	                 (gs_tree_type(gs_tree_operand(t, 0))));
	  }
	  // If opnd 0 of the TARGET_EXPR is an INDIRECT_REF, then tell
	  // WGEN_Expand_Expr to put the result in the area addressed by the
	  // INDIRECT_REF.
	  if (gs_tree_code(opnd0) == GS_INDIRECT_REF) {
	    gs_t var = opnd0;
	    if (gs_tree_code(gs_tree_operand(var, 0)) == GS_NON_LVALUE_EXPR ||
	        gs_tree_code(gs_tree_operand(var, 0)) == GS_NOP_EXPR)
	      var = gs_tree_operand(var, 0); // bugs 13261, 13474
	    ST *st = Get_ST(gs_tree_operand(var, 0));
	    WN *ldid_wn = WN_Ldid (Pointer_Mtype, 0, st, ST_type(st));
	    WN *result_wn = WGEN_Expand_Expr (t, TRUE, 0, 0, 0, 0, FALSE, FALSE,
					     ldid_wn);
	    // If the result of expanding t is not an indirect reference to the
	    // result area we want, then it means t has not copied the value
	    // into the result area.  Do the copy.
	    if (result_wn &&
		!(WN_operator(result_wn) == OPR_ILOAD &&
		  WN_operator(WN_kid0(result_wn)) == OPR_LDID &&
		  WN_st(WN_kid0(result_wn)) == st)) {
	      WGEN_Stmt_Append(WN_Istore(WN_rtype(result_wn), 0, ST_type(st),
					WN_CopyNode(ldid_wn), result_wn),
			      Get_Srcpos());
	    }
	  }
          else if( gs_tree_code(opnd0) == GS_COMPONENT_REF ) {
            // OSP_397, if opnd 0 of TARGET_EXPR is an COMPONENT_REF
            if (gs_tree_code(t) == GS_AGGR_INIT_EXPR ) {
              WN *target_wn = WGEN_Address_Of(opnd0);
              WN *result_wn = WGEN_Expand_Expr (t, TRUE /* for return_in_mem*/,
                                                0, 0, 0, 0, FALSE, FALSE, target_wn);
              /* We ignore the result_wn, for safety, place an assertion here */
              FmtAssert(result_wn == NULL,
                        ("result_wn should be NULL for the result is passed as param."));
            } else if (gs_tree_code(t) == GS_COMPONENT_REF &&
                    gs_tree_code(gs_tree_operand(opnd0, 0)) == GS_INDIRECT_REF) {
                /*
                 * match the following kind of tree 
                 * - TARGET_REF
                 * |-0 COMPONENT_REF
                 *   |- 0 INDIRECT_REF
                 *   |- 1 FIELD
                 * |-1 COMPONENT_REF
                 */
                // generate rhs first
  			  WN *rhs_wn = WGEN_Expand_Expr (t, TRUE /* for return_in_mem*/,
  					  0, 0, 0, 0, FALSE, FALSE, NULL);
  
                gs_t obj_instance = gs_tree_operand(opnd0, 0);
                gs_t obj_ptr = gs_tree_operand(obj_instance, 0);
  
                gs_t member = gs_tree_operand(opnd0, 1);
                WN_OFFSET ofst = 0;
                UINT xtra_BE_ofst = 0;
  
                TY_IDX obj_ty_idx = Get_TY(gs_tree_type(gs_tree_operand(obj_ptr, 0)));
  
                ofst = (BITSPERBYTE * gs_get_integer_value(gs_decl_field_offset(member)) +
                        gs_get_integer_value(gs_decl_field_bit_offset(member)))
                    / BITSPERBYTE;
  
  
                WN *obj_wn = WGEN_Expand_Expr(obj_ptr, TRUE, 0, 0, 0, 0, FALSE, FALSE, NULL);
  
                TY_IDX hi_ty_idx;
                if (gs_tree_code(gs_tree_type(opnd0)) == GS_VOID_TYPE)
                    hi_ty_idx = MTYPE_To_TY(MTYPE_I4); // dummy; for bug 10176 Comment #4
                else hi_ty_idx = Get_TY(gs_tree_type(opnd0));
  
                desc_ty_idx = hi_ty_idx;
                ty_idx = desc_ty_idx;
  
                wn = WN_CreateIstore(OPR_ISTORE, MTYPE_V, TY_mtype(hi_ty_idx),
                        xtra_BE_ofst,
                        obj_ty_idx,
                        rhs_wn, obj_wn, DECL_FIELD_ID(member));
                WGEN_Stmt_Append(wn, Get_Srcpos());
                break;
	      }
            // bug 15210
            // Replace the VAR_DECL with the REF, for this case
            //
            else if (gs_tree_code(t) == GS_COND_EXPR &&
                     gs_tree_code(gs_tree_operand(t,1)) == GS_TARGET_EXPR &&
                     gs_tree_code(gs_tree_operand(t,2)) == GS_TARGET_EXPR ) {
               gs_t t1, t2;

               t1 = gs_tree_operand(t, 1);
               t2 = gs_tree_operand(t, 2);
               gs_set_tree_operand(t1, 0, opnd0);
               gs_set_tree_operand(t2, 0, opnd0);
               wn = WGEN_Expand_Expr(t);

               break;
            } else {
              WN *target_wn = WGEN_Address_Of(opnd0);
              WN *result_wn = WGEN_Expand_Expr (t, TRUE /* for return_in_mem*/,
                                                0, 0, 0, 0, FALSE, FALSE, target_wn);
              if (result_wn != NULL) {
                wn = result_wn;
                WN_set_rtype(wn, MTYPE_V);
              }
              break;
            }
          }
	  // If the initializer returns the object in memory, then make sure
	  // the type doesn't require a copy constructor, since such types
	  // sometimes require one.
	  else if (TY_return_in_mem(Get_TY(gs_tree_type(t))) ||
	           (gs_tree_code(t) == GS_AGGR_INIT_EXPR &&
	            TY_return_in_mem(Get_TY(ret_type)))) {
	    gs_code_t code = gs_tree_code(t);
	    if (code == GS_VAR_DECL ||
		code == GS_PARM_DECL) {
	      // The initializer is a var or parm.  We need to insert copy.
	      // First make sure type has no copy constructor.
	      WN *rhs_wn = WGEN_Expand_Expr (t);
	      gs_t type = gs_tree_type(gs_tree_operand(exp, 0));
	      Is_True(!WGEN_has_copy_constructor(type),
		      ("WGEN_Expand_Expr: type require copy constructor"));
	      WGEN_Stmt_Append(WN_Stid (mtype, ST_ofst(st), st, ty, rhs_wn),
			      Get_Srcpos());
	    } else {
	      // The initializer is an expression.  Try to expand it directly
	      // into the target.
	      WN *target_wn = WN_Lda (Pointer_Mtype, 0, st, 0);
	      WN *result_wn = WGEN_Expand_Expr (t, TRUE, 0, 0, 0, 0, FALSE,
					       FALSE, target_wn);
	      // If expanding t did not write the result into the target as a
	      // side effect, then create the copy.
	      if (result_wn) {
		// There may be more cases where we need to store the result.
		// Need to find a better way to catch them all.
		if (WN_operator(result_wn) == OPR_ILOAD ||
		    WN_operator(result_wn) == OPR_LDID) {
		  WGEN_Stmt_Append(WN_Stid (mtype, ST_ofst(st), st, ty,
				  result_wn), Get_Srcpos());
		} else if (WN_operator(result_wn) == OPR_CSELECT) {
		  WN *wn = WN_CreateEval(result_wn);
		  WGEN_Stmt_Append(wn, Get_Srcpos());
		}
	      }
	    }
	  } else {
	    // Bug 7862: Set addr_saved flag if the initializer is an LDA.
	    WN * init = WGEN_Expand_Expr (t, TRUE);
	    if (init != NULL) {
	      if (WN_operator (init) == OPR_LDA)
		WGEN_Set_ST_Addr_Saved (init);
	      WGEN_Stmt_Append(WN_Stid (mtype, ST_ofst(st), st, ty, init),
			      Get_Srcpos());
	    }
	  }
#else
	  WGEN_Stmt_Append(WN_Stid (mtype, ST_ofst(st), st, ty,
		        	   WGEN_Expand_Expr (t)),
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
          Push_Temp_Cleanup(gs_tree_operand(exp, 2), true, gs_cleanup_eh_only (exp));
#else
          Push_Temp_Cleanup(gs_tree_operand(exp, 2), true);
#endif

#ifdef KEY
	// If the target area was supplied by the caller, then return an ILOAD
	// of the target pointer.
	if (gs_tree_code(opnd0) == GS_INDIRECT_REF) {
          // OSP_371, skip the NON_LVALUE_EXPR
	  // INIT_EXPR
	  //   0-> INDIRECT_REF
	  //     0-> NON_LVALUE_EXPR
	  //       0-> VAR_DECL
	  //   1-> EXPR
	  if (gs_tree_code(gs_tree_operand(opnd0, 0)) == GS_NOP_EXPR ||
              gs_tree_code(gs_tree_operand(opnd0, 0)) == GS_NON_LVALUE_EXPR)
	    opnd0 = gs_tree_operand(opnd0, 0);
	  ST *st = Get_ST(gs_tree_operand(opnd0, 0));
	  TY_IDX ty_idx = Get_TY (gs_tree_type(exp));
	  WN *ldid_wn = WN_Ldid (Pointer_Mtype, 0, st, ST_type(st));
	  wn = WN_Iload(TY_mtype(ty_idx), 0, ty_idx, ldid_wn);
	  break;
	}
        else if (gs_tree_code(opnd0) == GS_COMPONENT_REF ||
                 gs_tree_code(opnd0) == GS_ARRAY_REF) {
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
      if (!need_result &&
	  !gs_tree_side_effects(exp)) {
	return NULL;
      }
#endif
    // fall thru

    case GS_PARM_DECL: // for formal parms
    case GS_VAR_DECL:
    case GS_RESULT_DECL:
      {
	UINT xtra_BE_ofst = 0; 	// only needed for big-endian target
        PREG_NUM preg_num = 0;
	desc_ty_idx = component_ty_idx;
	TY_IDX hi_ty_idx = Get_TY (gs_tree_type(exp));
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

	if (gs_tree_this_volatile(exp))
	  Set_TY_is_volatile(ty_idx);
	else Clear_TY_is_volatile(ty_idx);

#ifdef KEY
	if (code == GS_VAR_DECL && gs_decl_value_expr(exp)) {
	  wn = WGEN_Expand_Expr (gs_decl_value_expr(exp),
	                         need_result, nop_ty_idx, component_ty_idx,
	                         component_offset, field_id, is_bit_field);
	  if (cvtl_size != 0)
	    wn = WN_CreateCvtl(OPR_CVTL, rtype, MTYPE_V, cvtl_size, wn);
	  break;
	}
#endif

	if (code == GS_PARM_DECL || 
	    code == GS_VAR_DECL || 
	    code == GS_RESULT_DECL) {
	  st = Get_ST (exp);
          if (ST_assigned_to_dedicated_preg (st))
	    Set_TY_is_volatile(ty_idx);
        }
	else
	if (code == GS_CONSTRUCTOR) {
	  DevWarn ("Encountered CONSTRUCTOR at line %d", lineno);
	  st = WGEN_Generate_Temp_For_Initialized_Aggregate (exp, "");
	}

	Is_True(! is_bit_field || field_id <= MAX_FIELD_ID,
		("WGEN_Expand_Expr: field id for bit-field exceeds limit"));
        WN* load_ofst = NULL;
        if (xtra_BE_ofst + preg_num != 0)
          load_ofst = WGEN_Compose_Offset(component_offset,
                                          WN_Intconst(Pointer_Mtype, xtra_BE_ofst));
        else
          load_ofst = component_offset;
        if (ST_base(st) != st) {
          ST* base_st = ST_base(st);
          Is_True(ST_ofst(st) == 0, ("TODO: ST_ofst is not 0"));
          WN* load_addr = WN_Ldid(Pointer_Mtype, 0, base_st, ST_type(base_st));
          TY_IDX load_ty = field_id != 0 ? hi_ty_idx : ty_idx;
          wn = WGEN_CreateIload(OPR_ILOAD, rtype,
                              is_bit_field ? MTYPE_BS : desc,
                              load_ofst,
                              load_ty,
                              ST_type(base_st),
                              load_addr,
                              field_id);
        }
        else {
	  wn = WGEN_CreateLdid (OPR_LDID, rtype,
			      is_bit_field ? MTYPE_BS : desc,
			      load_ofst, st,
			      field_id != 0 ? hi_ty_idx : ty_idx, field_id);
        }
	if (cvtl_size != 0)
	  wn = WN_CreateCvtl(OPR_CVTL, rtype, MTYPE_V, cvtl_size, wn);
      }
      break;

    case GS_COMPOUND_LITERAL_EXPR:
      {
	gs_t oper = gs_tree_operand (gs_tree_operand (exp, 0), 0);
	if (gs_tree_code (gs_decl_initial (oper)) == GS_CONSTRUCTOR)
	  exp = gs_decl_initial (oper);
	else
	  exp = oper;

	desc_ty_idx = component_ty_idx;
	TY_IDX hi_ty_idx = Get_TY (gs_tree_type(exp));
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

	if (gs_tree_this_volatile(exp))
	  Set_TY_is_volatile(ty_idx);

	if (gs_tree_code(exp) == GS_CONSTRUCTOR)
	  st = WGEN_Generate_Temp_For_Initialized_Aggregate (exp, "");
	else {
	  WGEN_Initialize_Decl (exp);
	  st = Get_ST (exp);
	  if (ST_assigned_to_dedicated_preg (st))
	    Set_TY_is_volatile (ty_idx);
	}
	wn = WGEN_CreateLdid (OPR_LDID, rtype,
			    is_bit_field ? MTYPE_BS : desc, 
			    component_offset, st,
			    field_id != 0 ? hi_ty_idx : ty_idx, field_id);
	if (cvtl_size != 0)
	  wn = WN_CreateCvtl(OPR_CVTL, rtype, MTYPE_V, cvtl_size, wn);
      }
      break;

    case GS_CONST_DECL:
        wn = WGEN_Expand_Expr(gs_decl_initial(exp), need_result);
	break;

    case GS_INTEGER_CST:
      {
	ty_idx = Get_TY (gs_tree_type(exp));
	TYPE_ID mtyp = TY_mtype(ty_idx);
	mtyp = (mtyp == MTYPE_V || mtyp == MTYPE_M) ? MTYPE_I4 : Widen_Mtype(mtyp);
	wn = WN_Intconst(mtyp, gs_get_integer_value(exp));
      }
      break;

    case GS_PTRMEM_CST:
      wn = WGEN_Expand_Expr(gs_expanded_ptrmem_cst(exp), need_result,
                            nop_ty_idx, component_ty_idx, component_offset,
                            field_id);
      break;

    case GS_EMPTY_CLASS_EXPR: // bugs 10846, 11138
      ty_idx = Get_TY (gs_tree_type(exp));
      st = Gen_Temp_Symbol (ty_idx, "__empty_class_expr");
      wn = WN_Ldid (TY_mtype (ty_idx), 0, st, ty_idx);
      break;

    case GS_REAL_CST:
      {
	TCON tcon;
	ty_idx = Get_TY (gs_tree_type(exp));
	switch (TY_size(ty_idx)) {
	case 4: 
	  tcon = Host_To_Targ_Float_4(MTYPE_F4, gs_tree_real_cst_f(exp));
	  break;
	case 8: 
	  tcon = Host_To_Targ_Float(MTYPE_F8, gs_tree_real_cst_d(exp));
	  break;
#if defined(TARG_IA64) || defined(TARG_X8664)
        case 12:
        case 16:
          // TODO handle MTYPE_F16
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
	st = New_Const_Sym (Enter_tcon (tcon), ty_idx);
	wn = WN_CreateConst (OPR_CONST, TY_mtype (ty_idx), MTYPE_V, st);
      }
      break;

    case GS_COMPLEX_CST:
      {
	TCON tcon;
	ty_idx = Get_TY (gs_tree_type(exp));
#ifdef KEY
	// Bug 949
	if (gs_tree_code(gs_tree_realpart(exp)) != GS_REAL_CST ||
	    gs_tree_code(gs_tree_imagpart(exp)) != GS_REAL_CST) {
	  printf("%s does not support complex integer data types "
		 "(a GNU extension)\n", lang_cplus ? OPEN64_NAME_PREFIX "CC" : OPEN64_NAME_PREFIX "cc");
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
	  tcon = Host_To_Targ_Complex_10(MTYPE_C10,
                                     gs_tree_real_cst_ld(gs_tree_realpart(exp)),
                                     gs_tree_real_cst_ld(gs_tree_imagpart(exp)));
          break;
#elif defined(TARG_X8664)	  
	case 24:
	case 32:
	  tcon = Host_To_Targ_Complex_10(MTYPE_C10,
				   gs_tree_real_cst_ld(gs_tree_realpart(exp)),
				   gs_tree_real_cst_ld(gs_tree_imagpart(exp)));
	  break;
#else
	case 24:
	case 32:
	  tcon = Host_To_Targ_Complex_Quad(
				   gs_tree_real_cst_ld(gs_tree_realpart(exp)),
				   gs_tree_real_cst_ld(gs_tree_imagpart(exp)));
	  break;
#endif
	default:
	  FmtAssert(FALSE, ("WGEN_Expand_Expr: unexpected size for complex constant"));
	  break;
	}
	st = New_Const_Sym (Enter_tcon (tcon), ty_idx);
	wn = WN_CreateConst (OPR_CONST, TY_mtype (ty_idx), MTYPE_V, st);
      }
      break;

    // this should occur only if string is a statement expression
    case GS_STRING_CST:
      {
	TCON tcon;
	tcon = Host_To_Targ_String (MTYPE_STRING,
				    const_cast<char*>(gs_tree_string_pointer(exp)),
				    gs_tree_string_length(exp));
	ty_idx = Get_TY(gs_tree_type(exp));
	st = New_Const_Sym (Enter_tcon (tcon), ty_idx);
	wn = WN_Lda (Pointer_Mtype, ST_ofst(st), st);
	TREE_STRING_ST (exp) = st;
      }
      break;

    // unary ops
    case GS_BIT_NOT_EXPR:
    case GS_ABS_EXPR:
    case GS_NEGATE_EXPR:
    case GS_REALPART_EXPR:
    case GS_IMAGPART_EXPR:
      {
        TYPE_ID mtyp = TY_mtype(Get_TY(gs_tree_type(exp)));
#ifdef KEY
	// Bug 949, 11316
        if ((code == GS_REALPART_EXPR ||
	     code == GS_IMAGPART_EXPR) &&
	    !MTYPE_float(mtyp)) {
	  printf("%s does not support complex integer data types "
		 "(a GNU extension)\n", lang_cplus ? OPEN64_NAME_PREFIX "CC" : OPEN64_NAME_PREFIX "cc");
	  exit(2);
	}
#endif

        wn0 = WGEN_Expand_Expr (gs_tree_operand (exp, 0));
        wn  = WN_Unary (Operator_From_Tree [code].opr, Widen_Mtype(mtyp), wn0);
#ifdef KEY // bug 2648
        if (mtyp != WN_rtype(wn))
          wn = WN_CreateCvtl (OPR_CVTL, WN_rtype(wn), MTYPE_V,
                              MTYPE_size_min(mtyp), wn);
#endif
      }
      break;

    case GS_TRUTH_NOT_EXPR:
      wn0 = WGEN_Expand_Expr (gs_tree_operand (exp, 0));
      wn1 = WN_Intconst (MTYPE_I4, 0);
      wn  = WN_Relational (OPR_EQ, MTYPE_I4, wn0, wn1);
      break;

    case GS_CONJ_EXPR:
      {
	ty_idx = Get_TY (gs_tree_type(exp));
        TYPE_ID complex_mtype = TY_mtype(ty_idx);
        TYPE_ID float_mtype   = Mtype_complex_to_real (complex_mtype);
        wn0 = WGEN_Expand_Expr (gs_tree_operand (exp, 0));
	if (WN_has_side_effects (wn0)) {
	  ST       *preg_st;
	  PREG_NUM  preg;
	  preg_st = MTYPE_To_PREG(complex_mtype);
	  preg    = Create_Preg (complex_mtype, NULL);
	  wn0     = WN_Stid (complex_mtype, preg, preg_st, ty_idx, wn0);
	  WGEN_Stmt_Append (wn0, Get_Srcpos());
	  wn0 = WN_Ldid (complex_mtype, preg, preg_st, ty_idx);
	}
#ifdef KEY
	// Fix bug 603
        wn = WN_Binary (OPR_COMPLEX, complex_mtype,
			WN_Unary (OPR_REALPART, float_mtype, wn0),
			WN_Unary (OPR_NEG, float_mtype,
				  WN_Unary (OPR_IMAGPART, float_mtype, WN_COPY_Tree(wn0))));
#else
        wn = WN_Binary (OPR_COMPLEX, complex_mtype,
			WN_Unary (OPR_REALPART, float_mtype, wn0),
			WN_Unary (OPR_NEG, float_mtype,
				  WN_Unary (OPR_REALPART, float_mtype, WN_COPY_Tree(wn0))));
#endif
      }
      break;

    case GS_NOP_EXPR:
      {
	ty_idx = Get_TY (gs_tree_type(exp));
        TYPE_ID mtyp = TY_mtype(ty_idx);
	// do not pass struct type down because will cause rtype of MTYPE_M
        wn = WGEN_Expand_Expr (gs_tree_operand (exp, 0), TRUE, 
			      (mtyp == MTYPE_M || mtyp == MTYPE_V) ? 0 : ty_idx,
			       component_ty_idx, component_offset,
			       field_id, is_bit_field
#ifdef KEY
			       , FALSE, target_wn
#endif
			       );

#ifdef KEY // bug 12548
	if (!wn)
	  break;
#endif
	if (mtyp == MTYPE_V) 
	  break;
	if (mtyp == MTYPE_M) 
	  break;
	if (WN_rtype(wn) == MTYPE_M) 
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
	    if (MTYPE_size_min(mtyp) != 32) { 
	      INT cvt_bit = gs_type_type_precision(gs_tree_type(exp));
	      if (cvt_bit == 0 || cvt_bit == 8 || cvt_bit == 16 || cvt_bit == 32) { 
	        wn = WN_CreateCvtl(OPR_CVTL, Widen_Mtype(mtyp), MTYPE_V, cvt_bit, wn);
	      } else {
	        wn = WN_CreateExp1(OPR_EXTRACT_BITS, Widen_Mtype(mtyp), MTYPE_V, wn);
	        WN_set_bit_offset_size(wn, 0, cvt_bit);
	      }     
	    }
	    else wn = WN_Cvt(WN_rtype(wn), mtyp, wn);
	  }
	  else {
	    TY_IDX ty_idx0 = Get_TY(gs_tree_type(gs_tree_operand (exp, 0)));
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
#ifdef TARG_X8664
            if (MTYPE_is_vector(mtyp) && MTYPE_is_vector(WN_rtype(wn)) &&
                MTYPE_is_mmx_vector(mtyp) == MTYPE_is_mmx_vector(WN_rtype(wn)))             {
               // CVT between vector types not needed
            }
            else
#endif
	    wn = WN_Cvt(WN_rtype(wn), mtyp, wn);
	}
      }
      break;

    case GS_COMPONENT_REF:
      {
	arg0 = gs_tree_operand (exp, 0);
	arg1 = gs_tree_operand (exp, 1);
	// If this is an indirect of a nop_expr, we may need to fix the
	// type of the nop_expr:
	(void) Get_TY(gs_tree_type(arg0));
   
	if (component_ty_idx == 0)
	  ty_idx = Get_TY (gs_tree_type(exp));
	else ty_idx = component_ty_idx;
	if (gs_decl_bit_field(arg1)) 
	  is_bit_field = TRUE;

	if (! is_bit_field && 
	    component_ty_idx == 0) {  // only for top-level COMPONENT_REF
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
	        ("COMPONENT_REF: integer type expected at inconsistent field size"));
	      c_mtyp = Mtype_AlignmentClass(bsize >> 3, MTYPE_type_class(c_mtyp));
	      ty_idx = MTYPE_To_TY(c_mtyp);
	    }
	  }
        }

        WN* ofst = NULL;
	if (! is_bit_field) {
          ofst = WGEN_Expand_Field_Offset(arg1);
          ofst = WGEN_Compose_Offset(ofst, component_offset);
        }
	else {
          ofst = component_offset;
        }
#ifdef KEY
	FmtAssert (DECL_FIELD_ID(arg1) != 0,
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
	  wn = WGEN_Expand_Ptr_To_Member_Func_Call_Expr (arg0, nop_ty_idx,
		  Pointer_Mtype, desc, ofst,
		  field_id + DECL_FIELD_ID(arg1));
	} else
#endif
        wn = WGEN_Expand_Expr (arg0, TRUE, nop_ty_idx, ty_idx, ofst,
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
            WN* iload_ofst = NULL;
            if (WN_offset(wn) != 0) {
              iload_ofst = WGEN_Compose_Offset(ofst,
                                               WN_Intconst(Pointer_Mtype, WN_offset(wn)));
            }
            else {
              iload_ofst = ofst;
            }
            wn = WGEN_CreateIload(OPR_ILOAD, rtype, desc,
			        iload_ofst, ty_idx,
			        WN_load_addr_ty(wn), WN_kid0(wn),
			        WN_field_id(wn)+field_id + DECL_FIELD_ID(arg1));
	  } 
	  else if (WN_operator(wn) == OPR_LDID) {
	    WN_set_rtype(wn, rtype);
	    WN_set_desc(wn, desc);
            Is_True(ofst == NULL || WN_operator(ofst) == OPR_INTCONST,
                    ("WGEN_Expand_Expr: ofst is not constant for LDID"));
            INT64 ofst_value = (ofst == NULL) ? 0 : WN_const_val(ofst);
	    WN_offset(wn) = WN_offset(wn) + ofst_value;
	    WN_set_field_id(wn, WN_field_id(wn)+field_id + DECL_FIELD_ID(arg1));
	  } 
	} 
	// bug 6122
	// Handle code like (x == 1 ? p->a : p->b).c
	else if (gs_tree_code(arg0) == GS_COND_EXPR &&
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
	  WGEN_Stmt_Append (wn, Get_Srcpos());
	  // Load correct field from temp symbol
          TYPE_ID rtype= Mtype_comparison(TY_mtype (ty_idx));
	  wn = WGEN_CreateLdid (OPR_LDID, rtype, TY_mtype (ty_idx), ofst,
	                temp, temp_ty_idx, field_id + DECL_FIELD_ID(arg1));
	}
#endif
      }
      break;

    case GS_INDIRECT_REF:
      {
	UINT xtra_BE_ofst = 0; 	// only needed for big-endian target
        wn0 = WGEN_Expand_Expr (gs_tree_operand (exp, 0));

	TY_IDX hi_ty_idx;
	if (gs_tree_code(gs_tree_type(exp)) == GS_VOID_TYPE)
	  hi_ty_idx = MTYPE_To_TY(MTYPE_I4); // dummy; for bug 10176 Comment #4
	else hi_ty_idx = Get_TY(gs_tree_type(exp));

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

	if (gs_tree_this_volatile(exp))
	  Set_TY_is_volatile(hi_ty_idx);

	Is_True(! is_bit_field || field_id <= MAX_FIELD_ID,
		("WGEN_Expand_Expr: field id for bit-field exceeds limit"));

#ifdef KEY // bug 12548
	if (!wn0)
	  break;
#endif

        if (!WGEN_Keep_Zero_Length_Structs &&
            rtype == MTYPE_M              &&
            TY_size (hi_ty_idx) == 0) {
	  if (WN_has_side_effects (wn0)) {
	    wn = WN_CreateEval (wn0);
	    WGEN_Stmt_Append (wn, Get_Srcpos());
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
              Is_True(component_offset == NULL || WN_operator(component_offset) == OPR_INTCONST,
                      ("WGEN_Expand_Expr: component_offset is not constant"));
              INT64 offset_value = (component_offset == NULL) ? 0 : WN_const_val(component_offset);
	      mUINT64 offset = offset_value + xtra_BE_ofst + WN_offset (wn0);
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
	  // NOTE: In GNU4, this may be a REFERENCE_REF_P.
	  if (need_result) {
            WN* iload_ofst = NULL;
            if (xtra_BE_ofst != 0)
              iload_ofst = WGEN_Compose_Offset(component_offset,
                                               WN_Intconst(Pointer_Mtype, xtra_BE_ofst));
            else
              iload_ofst = component_offset;
	    wn = WGEN_CreateIload(OPR_ILOAD, rtype,
				is_bit_field ? MTYPE_BS : desc, 
				iload_ofst,
				field_id != 0 ? hi_ty_idx : ty_idx, 
				Make_Pointer_Type (hi_ty_idx, FALSE),
				wn0, field_id);
          }
	  else
	  if (WN_has_side_effects (wn0))
	    wn = wn0;
	}
      }
      break;

    case GS_CONVERT_EXPR:
    case GS_FLOAT_EXPR:
      {
#ifndef KEY // bug 10967
        wn0 = WGEN_Expand_Expr (gs_tree_operand (exp, 0));
#endif
	ty_idx = Get_TY (gs_tree_type(exp));
	TYPE_ID mtyp = TY_mtype(ty_idx);
#ifdef KEY
	// Bug 10967: Don't ask for a result if it is not required.
	// In the GCC 4 front-end, CONVERT_EXPR is often used to call
	// a function (using opnd0), and needlessly asking for the result
	// confuses the logic regarding how to close the exception
	// region. It causes the EH region to close before the call WN is
	// actually emitted.
	wn0 = WGEN_Expand_Expr (gs_tree_operand (exp, 0),
	                        need_result || (mtyp != MTYPE_V));
#endif
#ifdef TARG_X8664
        if (mtyp != MTYPE_V && WN_operator(wn0) == OPR_COMMA &&
             gs_tree_code(gs_tree_operand (exp, 0)) == GS_CALL_EXPR ) {
          WN *ret_val = WN_kid1(wn0);
          if (WN_operator(ret_val) == OPR_LDID) { // load return value
            TYPE_ID rtype = WN_rtype(ret_val); // type in register
            TYPE_ID desc =  WN_desc(ret_val);  // type in memory
            // widen the value with proper sign/zero extension
            // such that I4I1LDID will be sign extended and
            // U4U2 will be zero extended
            // Normally the call return value is handled in GS_NOP_EXPR,
            // but for call expression which is the child of GS_CONVERT_EXPR
            // (e.g. the bool retuval is explicitly added a convert ), 
            // it should be handled here
            if (MTYPE_is_integral(rtype) && MTYPE_is_integral(desc) &&
                 MTYPE_byte_size( rtype ) > MTYPE_byte_size( desc )) {
               wn0 = WN_CreateCvtl(OPR_CVTL, Widen_Mtype(desc), MTYPE_V,
			     MTYPE_size_min(desc), wn0);
            }
          }
        }
#endif // TARG_X8664
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
#ifdef TARG_X8664
            // in 32bit mode, unsigned int to float type conversion would cause
            // precision lost if the MSB is '1', the reason for that is we use 
            // cvtsi2ss to convert the unsigned int to float, it treats the 
            // 32bit number as signed integer, so the round up is to the wrong
            // direction. To avoid the lost of precision, we should convert the 
            // unsigned int to 64bit int first
            if (MTYPE_float(mtyp) && Is_Target_32bit() && 
                  WN_rtype(wn0) == MTYPE_U4) {
	       wn0 = WN_Int_Type_Conversion(wn0, MTYPE_I8);
            }
#endif // TARG_X8664
#ifdef KEY // prevent zero extension when converting to 64-bit address type
	    if (gs_tree_code(gs_tree_type(exp)) == GS_POINTER_TYPE &&
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

    case GS_FIX_TRUNC_EXPR:
      {
        wn0 = WGEN_Expand_Expr (gs_tree_operand (exp, 0));
	ty_idx = Get_TY (gs_tree_type(exp));
	TYPE_ID mtyp = Widen_Mtype(TY_mtype(ty_idx));
	wn = WN_Trunc(WN_rtype(wn0), mtyp, wn0);
      }
      break;

    case GS_EXPR_STMT:
      {
#ifdef KEY
        // bug 11169: The caller may need_result, so we should not use false.
        wn = WGEN_Expand_Expr (gs_expr_stmt_expr(exp), need_result, nop_ty_idx,
			    component_ty_idx, component_offset, field_id,
			    is_bit_field, is_aggr_init_via_ctor);
#else
	wn = WGEN_Expand_Expr (gs_expr_stmt_expr(exp), false);
#endif
      }
      break;

    case GS_STMT_EXPR:
      {
#ifdef KEY
	bool write_to_target_wn = TRUE;

	// If we need to store the result in target_wn, then give result the
	// same ST as the ST in target_wn.  To do this, first find the
	// STMT_EXPR's result, which is returned by the last EXPR_STMT in the
	// COMPOUND_STMT.  Based on code in gnu/c-common.c:c_expand_expr.
	if (target_wn != NULL
	    && gs_tree_code (gs_stmt_expr_stmt (exp)) == GS_COMPOUND_EXPR
	    && gs_tree_code (first_in_compound_expr (gs_stmt_expr_stmt (exp))) == GS_SCOPE_STMT) {
	  gs_t expr = gs_tree_operand (gs_stmt_expr_stmt (exp), 0);
	  gs_t last = gs_tree_operand (gs_stmt_expr_stmt (exp), 1);

	  while (gs_tree_code (expr) == GS_COMPOUND_EXPR) {
	    expr = gs_tree_operand (expr, 1);
	  }

	  if (gs_tree_code (last) == GS_SCOPE_STMT
	      && gs_tree_code (expr) == GS_EXPR_STMT) {
	    if (gs_tree_code (gs_expr_stmt_expr (expr)) == GS_VAR_DECL) {
	      // If the last expression is a variable, then the variable is the
	      // returned value.
	      gs_t var_decl = gs_expr_stmt_expr (expr);
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
		  gs_t ptr_var =
		    gs_build_decl(GS_VAR_DECL,
			       gs_build_pointer_type(gs_tree_type(var_decl)));
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

	    } else if (gs_tree_code (gs_expr_stmt_expr (expr)) == GS_INDIRECT_REF) {
	      // The indirect_ref must have been a var_decl that was changed by
	      // kg++fe to an indirect_ref.  This means we are already writing
	      // to the target location.
	      write_to_target_wn = FALSE;
	    }
	  }
	}
	wn = WGEN_Expand_Expr (gs_tree_operand (exp, 0), need_result, nop_ty_idx,
			      component_ty_idx, component_offset, field_id,
			      is_bit_field, is_aggr_init_via_ctor,
			      write_to_target_wn ? target_wn : NULL);
#else
        wn = WGEN_Expand_Expr (gs_tree_operand (exp, 0), need_result);
#endif
      }
      break;

#ifndef KEY
    case GS_SUBOBJECT:
      break;
#endif // !KEY

    case GS_CLEANUP_POINT_EXPR: 
      {
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
        wn = WGEN_Expand_Expr (gs_tree_operand (exp, 0), need_result);

	// NULL means cleanups already done.
	if (enclosing_cleanup_point_expr == NULL)
	  done_cleanups = TRUE;
#else
        wn = WGEN_Expand_Expr (gs_tree_operand (exp, 0), FALSE);
#endif
        WN * cleanup_block = WN_CreateBlock ();
        WGEN_Stmt_Push (cleanup_block, wgen_stmk_temp_cleanup, Get_Srcpos ());
#ifdef KEY
	if (!done_cleanups)		// Bug 11350
#endif
          Do_Temp_Cleanups(exp);
        WGEN_Stmt_Pop (wgen_stmk_temp_cleanup);
	if (wn && WN_has_side_effects (wn) && WN_first (cleanup_block)) {
	  DevWarn("CLEANUP_POINT_EXPR: expressson has side effects");
#ifdef KEY
	  // We get here after fixing bug 10962. TREE_TYPE of this exp
	  // may be void, so use the type from the WN created above.
	  ty_idx = MTYPE_TO_TY_array[WN_rtype(wn)];
#else
	  ty_idx = Get_TY (gs_tree_type(exp));
#endif
	  st = Gen_Temp_Symbol (ty_idx, "__cleanup_point_expr");
#ifdef FE_GNU_4_2_0
  	  WGEN_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, st);
#endif
	  TYPE_ID mtype = TY_mtype (ty_idx);
	  WGEN_Set_ST_Addr_Saved (wn);
	  wn = WN_Stid (mtype, 0, st, ty_idx, wn);
	  WGEN_Stmt_Append (wn, Get_Srcpos ());
	  wn = WN_Ldid (mtype, 0, st, ty_idx);
	}
	WGEN_Stmt_Append (cleanup_block, Get_Srcpos ());
#ifdef KEY
	enclosing_cleanup_point_expr = old_enclosing_cleanup_point_expr;
#endif
      }
      break;

    case GS_THROW_EXPR:
      WGEN_One_Stmt (gs_tree_operand (exp, 0));
      break;

    case GS_TRY_CATCH_EXPR:
#ifdef KEY
      // The second operand of 'exp' should be run if the first throws an
      // exception.
      // wgen TODO: This cleanup should be treated as eh_only.
      Register_Cleanup (exp);
      wn = WGEN_Expand_Expr (gs_tree_operand (exp, 0), need_result);
      Unregister_Cleanup ();
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
    case GS_CEIL_DIV_EXPR:
       {
       TYPE_ID etype = TY_mtype(Get_TY(gs_tree_type(exp)));
       wn0 = WGEN_Expand_Expr(gs_tree_operand(exp, 0));
       wn1 = WGEN_Expand_Expr(gs_tree_operand(exp, 1));
#ifdef KEY // Bug 11875
        if (code == GS_COMPLEX_EXPR && !MTYPE_float(WN_rtype(wn0))) {
	  printf("%s does not support complex integer data types "
		 "(a GNU extension)\n", lang_cplus ? OPEN64_NAME_PREFIX "CC" : OPEN64_NAME_PREFIX "cc");
	  exit(2);
	}
#endif

       //generate whirl for add
       wn  = WN_Binary(Operator_From_Tree[code].opr, 
		       Widen_Mtype(etype), wn0, wn1);


        //bug 2649, 5503 --- need conversion
        if ((MTYPE_is_integral(etype)) &&
            (Widen_Mtype(etype) != etype) &&
	    (TY_size (Get_TY(gs_tree_type(exp))) < 32) &&
	     (code == GS_PLUS_EXPR || code == GS_MINUS_EXPR || 
	     code == GS_MULT_EXPR || code == GS_LSHIFT_EXPR || 
	     code == GS_BIT_XOR_EXPR || code == GS_BIT_IOR_EXPR))
          wn = WN_CreateCvtl(OPR_CVTL, Widen_Mtype(etype), MTYPE_V,
                             TY_size (Get_TY(gs_tree_type(exp))) * 8, wn);
       }
       break;

    case GS_LROTATE_EXPR:
      {
	ty_idx = Get_TY(gs_tree_type(exp));
	TYPE_ID mtype = TY_mtype (ty_idx);
        wn0 = WGEN_Expand_Expr (gs_tree_operand (exp, 0));
        wn1 = WGEN_Expand_Expr (gs_tree_operand (exp, 1));
	wn1 = WN_Binary (OPR_SUB, Widen_Mtype (mtype),
			 WN_Intconst (Widen_Mtype (mtype),
				      TY_size (ty_idx) * 8),
			 wn1);
	wn  = WN_Rrotate (TY_mtype(Get_TY(gs_tree_type(exp))), wn0, wn1);
      }
      break;

    case GS_RROTATE_EXPR:
      {
        wn0 = WGEN_Expand_Expr (gs_tree_operand (exp, 0));
        wn1 = WGEN_Expand_Expr (gs_tree_operand (exp, 1));
	wn  = WN_Rrotate (TY_mtype(Get_TY(gs_tree_type(exp))), wn0, wn1);
      }
      break;

    case GS_RSHIFT_EXPR:
      {
	TYPE_ID mtyp = Widen_Mtype(TY_mtype(Get_TY(gs_tree_type(exp))));
        wn0 = WGEN_Expand_Expr (gs_tree_operand (exp, 0));
        wn1 = WGEN_Expand_Expr (gs_tree_operand (exp, 1));
        wn  = WN_Binary (MTYPE_signed(mtyp) ? OPR_ASHR : OPR_LSHR,
                         mtyp, wn0, wn1);
      }
      break;

    case GS_TRUTH_ANDIF_EXPR:
    case GS_TRUTH_ORIF_EXPR:
      {
	// bug 2651: evaluate the 1st operand unconditionally
        wn0 = WGEN_Expand_Expr (gs_tree_operand (exp, 0));

	// Evaluate the second condition.  Add guard variable to the cleanup if
	// there is cleanup.
        WGEN_Guard_Var_Push();
        wn1 = WGEN_Expand_Expr_With_Sequence_Point (gs_tree_operand (exp, 1),
						   Boolean_type);
        gs_t guard_var = WGEN_Guard_Var_Pop();
	if (guard_var != NULL) {
	  WGEN_add_guard_var(guard_var, wn1);
	}
        wn  = WN_Binary (Operator_From_Tree [code].opr,
                         Boolean_type, wn0, wn1);
        if (Boolean_type != MTYPE_B &&
	    Widen_Mtype(TY_mtype(Get_TY(gs_tree_type(exp)))) != Boolean_type)
	  wn = WN_Cvt (Boolean_type, Widen_Mtype(TY_mtype(Get_TY(gs_tree_type(exp)))), wn);
      }
      break;

    case GS_LT_EXPR: 
    case GS_LE_EXPR:
    case GS_GT_EXPR:
    case GS_GE_EXPR:
    case GS_EQ_EXPR:
    case GS_NE_EXPR:
       {
       wn0 = WGEN_Expand_Expr(gs_tree_operand(exp, 0));
       wn1 = WGEN_Expand_Expr(gs_tree_operand(exp, 1));
       // check if conversion is needed
       ty_idx = Get_TY (gs_tree_type(exp));
       TYPE_ID mtyp = TY_mtype(ty_idx);
       TY_IDX ty_idx0 = Get_TY(gs_tree_type(gs_tree_operand(exp, 0)));
       TYPE_ID mtyp0 = TY_mtype(ty_idx0);
       TY_IDX ty_idx1 = Get_TY(gs_tree_type(gs_tree_operand(exp, 1)));
       TYPE_ID mtyp1 = TY_mtype(ty_idx1);

       if (MTYPE_size_min(mtyp1) > MTYPE_size_min(mtyp0) &&
	   ! Has_Subsumed_Cvtl(WN_operator(wn0)))
	 wn0 = WN_CreateCvtl(OPR_CVTL, Widen_Mtype(mtyp0), MTYPE_V,
			     MTYPE_size_min(mtyp0), wn0);
       if (MTYPE_size_min(mtyp0) > MTYPE_size_min(mtyp1) &&
	   ! Has_Subsumed_Cvtl(WN_operator(wn1)))
	 wn1 = WN_CreateCvtl(OPR_CVTL, Widen_Mtype(mtyp1), MTYPE_V,
			     MTYPE_size_min(mtyp1), wn1);

       if (MTYPE_is_integral(mtyp) && MTYPE_is_integral(mtyp0) &&
           MTYPE_size_min(mtyp) > MTYPE_size_min(mtyp0) )
         wn0 = WN_CreateCvtl(OPR_CVTL, Widen_Mtype(mtyp0), MTYPE_V,
                             MTYPE_size_min(mtyp0), wn0);

       if (MTYPE_is_integral(mtyp) && MTYPE_is_integral(mtyp1) &&
           MTYPE_size_min(mtyp) > MTYPE_size_min(mtyp1) )
         wn1 = WN_CreateCvtl(OPR_CVTL, Widen_Mtype(mtyp1), MTYPE_V,
                             MTYPE_size_min(mtyp1), wn1);

#ifdef TARG_IA64
       wn  = WN_Relational (Operator_From_Tree [code].opr,
		            Widen_Mtype(mtyp0), wn0, wn1);
#else
       wn  = WN_CreateExp2(Operator_From_Tree[code].opr, Widen_Mtype(mtyp),
			   Widen_Mtype(mtyp0), wn0, wn1);
#endif
       }
       break;

    case GS_COND_EXPR:
      {
        TY_IDX ty_idx1, ty_idx2;
        if (gs_tree_has_location(exp) == gs_true) // it would otherwise be -1
          lineno = gs_expr_lineno(exp);
	if (gs_tree_operand(exp, 1) != NULL &&
	    gs_tree_type(gs_tree_operand(exp, 1)) != NULL)
	  ty_idx1 = Get_TY (gs_tree_type(gs_tree_operand (exp, 1)));
	else ty_idx1 = MTYPE_To_TY(MTYPE_V);
	if (gs_tree_operand(exp, 2) != NULL &&
	    gs_tree_type(gs_tree_operand(exp, 2)) != NULL)
	  ty_idx2 = Get_TY (gs_tree_type(gs_tree_operand (exp, 2)));
	else ty_idx2 = MTYPE_To_TY(MTYPE_V);
	if (gs_tree_type(exp) != NULL)
	  ty_idx = Get_TY (gs_tree_type(exp));
	else ty_idx = MTYPE_To_TY(MTYPE_V);
#ifdef KEY // bug 2645
	wn0 = WGEN_Expand_Expr (gs_tree_operand (exp, 0));
#else
	wn0 = WGEN_Expand_Expr_With_Sequence_Point (gs_tree_operand (exp, 0),
						   Boolean_type);
#endif

        FmtAssert( (wn0 != NULL), ("Child 0 of IF can not be NULL!"));
	
	//15210, generate IF-struct, and discard the LDIDs
        bool typed_ite = false;
        if( gs_tree_code(gs_tree_operand(exp, 1)) == GS_TARGET_EXPR &&
            gs_tree_operand(exp, 2) != NULL       &&  // else statement can be NULL
            gs_tree_code(gs_tree_operand(exp, 2)) == GS_TARGET_EXPR ){
	 typed_ite = ( gs_tree_code(gs_tree_operand(gs_tree_operand(exp ,1), 0)) == GS_COMPONENT_REF &&
          gs_tree_code(gs_tree_operand(gs_tree_operand(exp ,2), 0)) == GS_COMPONENT_REF );
 	} 
	if (TY_mtype (ty_idx)  == MTYPE_V ||
            TY_mtype (ty_idx1) == MTYPE_V ||
            TY_mtype (ty_idx2) == MTYPE_V 
	    || typed_ite) {
	  // If ty_idx is MTYPE_V, no return value is needed
	  // We convert it into if...then...else
	  WN *then_block = WN_CreateBlock ();
	  WN *else_block = WN_CreateBlock ();
	  WN *if_stmt    = WN_CreateIf (wn0, then_block, else_block);
          WN *comma_value  = NULL;
#ifdef KEY
	  SRCPOS if_stmt_srcpos = Get_Srcpos();
         // Bug 11937: Generate guard variables where necessary. (See
         // explanation below).
         //
         // We may need to generate initializations for guard variables,
         // so write out the IF statement at the end.
         // "then" statement
          WGEN_Stmt_Push (then_block, wgen_stmk_if_then, Get_Srcpos());
          WGEN_Guard_Var_Push();
          if (!typed_ite)
	    wn1 = WGEN_Expand_Expr (gs_tree_operand (exp, 1), FALSE);
          else
   	  {
   	    wn1 = WGEN_Expand_Expr (gs_tree_operand(exp, 1), FALSE, 0, 0, 0, 0, FALSE, FALSE, target_wn);
	  }
	  gs_t guard_var1 = WGEN_Guard_Var_Pop();
          if (wn1 && !typed_ite) {
            comma_value = wn1;
            wn1 = WN_CreateEval (wn1);
            WGEN_Stmt_Append (wn1, Get_Srcpos());
          }
          WGEN_Stmt_Pop (wgen_stmk_if_then);
          // Add guard variables if they are needed.
          if (guard_var1 != NULL) {
            WGEN_add_guard_var(guard_var1, then_block, FALSE);
          }

          // "else" statement
          if (gs_tree_operand(exp, 2) != NULL) {
            WGEN_Stmt_Push (else_block, wgen_stmk_if_else, Get_Srcpos());
            WGEN_Guard_Var_Push();
            if(!typed_ite)
 	      wn2 = WGEN_Expand_Expr (gs_tree_operand (exp, 2), FALSE);
            else
   	      wn2 = WGEN_Expand_Expr (gs_tree_operand(exp, 2), FALSE, 0, 0, 0, 0, FALSE, FALSE, target_wn);
            gs_t guard_var2 = WGEN_Guard_Var_Pop();
            if (wn2 && !typed_ite) {
              comma_value = wn2;
              wn2 = WN_CreateEval (wn2);
              WGEN_Stmt_Append (wn2, Get_Srcpos());
            }
            WGEN_Stmt_Pop (wgen_stmk_if_else);
            // Add guard variables if they are needed.
            if (guard_var2 != NULL) {
              WGEN_add_guard_var(guard_var2, else_block, FALSE);
            }
          }

          if (target_wn == NULL && TY_mtype(ty_idx) != MTYPE_V &&
                  (TY_mtype(ty_idx1) == MTYPE_V || (TY_mtype(ty_idx2) == MTYPE_V))) {
              WN* block = WN_CreateBlock();
              WN_INSERT_BlockLast(block, if_stmt);
              wn  = WN_CreateComma(OPR_COMMA,
                      TY_mtype(ty_idx1) == MTYPE_V ? TY_mtype(ty_idx2) : TY_mtype(ty_idx1),
                      MTYPE_V, block, comma_value);
          } else {
              // Generate IF statement.
              WGEN_Stmt_Append (if_stmt, if_stmt_srcpos);
          }
#else
	  WGEN_Stmt_Append (if_stmt, Get_Srcpos());
	  WGEN_Stmt_Push (then_block, wgen_stmk_if_then, Get_Srcpos());
	  wn1 = WGEN_Expand_Expr (gs_tree_operand (exp, 1), FALSE);
	  if (wn1) {
	    wn1 = WN_CreateEval (wn1);
	    WGEN_Stmt_Append (wn1, Get_Srcpos());
	  }
	  WGEN_Stmt_Pop (wgen_stmk_if_then);
	  if (gs_tree_operand(exp, 2) != NULL) { 
	    WGEN_Stmt_Push (else_block, wgen_stmk_if_else, Get_Srcpos());
	    wn2 = WGEN_Expand_Expr (gs_tree_operand (exp, 2), FALSE);
	    if (wn2) {
	      wn2 = WN_CreateEval (wn2);
	      WGEN_Stmt_Append (wn2, Get_Srcpos());
	    }
	    WGEN_Stmt_Pop (wgen_stmk_if_else);
	  }
#endif
        }
	else {
#ifdef KEY
	  // Prepare a guard variable for each part of the conditional, in case
	  // the conditional has a cleanup that is executed after the whole
	  // conditional expression is evaluated.  The guard variable ensures
	  // that a cleanup is executed only if its part of the conditional is
	  // executed.
	  WGEN_Guard_Var_Push();
	  wn1 = WGEN_Expand_Expr_With_Sequence_Point (gs_tree_operand (exp, 1),
						     TY_mtype (ty_idx),
						     target_wn);
	  gs_t guard_var1 = WGEN_Guard_Var_Pop();

	  WGEN_Guard_Var_Push();
	  wn2 = WGEN_Expand_Expr_With_Sequence_Point (gs_tree_operand (exp, 2),
						     TY_mtype (ty_idx),
						     target_wn);
	  gs_t guard_var2 = WGEN_Guard_Var_Pop();

	  // Add guard variables if they are needed.
	  if (guard_var1 != NULL) {
	    WGEN_add_guard_var(guard_var1, wn1);
	  }
	  if (guard_var2 != NULL) {
	    WGEN_add_guard_var(guard_var2, wn2);
	  }
#else
	  wn1 = WGEN_Expand_Expr_With_Sequence_Point (gs_tree_operand (exp, 1),
						     TY_mtype (ty_idx));
	  wn2 = WGEN_Expand_Expr_With_Sequence_Point (gs_tree_operand (exp, 2),
						     TY_mtype (ty_idx));
#endif
	  wn  = WN_CreateExp3 (OPR_CSELECT, Mtype_comparison (TY_mtype (ty_idx)),
			   MTYPE_V, wn0, wn1, wn2);
	  Set_PU_has_very_high_whirl (Get_Current_PU ());
        }
      }
      break;

    case GS_INIT_EXPR:
#ifdef KEY
      // Put the result in the write target if there is a write target.
      if (target_wn != NULL &&
	  ((tmp_code = gs_tree_code(gs_tree_operand(exp, 0))) == GS_VAR_DECL ||
	   tmp_code == GS_RESULT_DECL ||
	   tmp_code == GS_PARM_DECL)) {
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
	  gs_t ptr_var = gs_build_decl(GS_VAR_DECL,
				    gs_build_pointer_type(gs_tree_type(opnd0)));
	  _gs_code(opnd0, GS_INDIRECT_REF);
	  gs_set_tree_operand(opnd0, 0, ptr_var);
	  set_DECL_ST(ptr_var, WN_st(target_wn));
	}
      }

#ifdef NEW_INITIALIZER
      {
        gs_t opnd0 = gs_tree_operand(exp, 0);
        gs_t opnd1 = gs_tree_operand(exp, 1);
        if( lang_cplus &&
            gs_tree_code(opnd0) == GS_INDIRECT_REF &&
            gs_tree_code(opnd1) == GS_CONSTRUCTOR ) {
          WN* target = WGEN_Address_Of(opnd0);
          ST* copy_st = WGEN_Generate_Initialized_Aggregate(target, opnd1); 
          ST* orig_st = WN_st(target);
          if ( ST_st_idx(copy_st) != ST_st_idx(orig_st) ) {
            // If the returned ST is not the original one,
            //   it means we create a new temp ST for initialization
            //   This happens on x8664, the returned struct is converted into a FORMAL.
            //   When initialize the FORMAL, we need to use a temp st.
            //   we copy the new ST into target here
            TY_IDX copy_ty = ST_type(copy_st);
            WN* ldid = WN_Ldid(TY_mtype(copy_ty), 0, copy_st, copy_ty);
            if ( WN_operator(target) == OPR_LDA ) {
              WGEN_Stmt_Append(
                         WN_Stid (TY_mtype(copy_ty), WN_lda_offset(target),
                                  orig_st, ST_type(orig_st), ldid),
                         Get_Srcpos() );
            }
            else if ( WN_operator(target) == OPR_LDID ) {
              WGEN_Stmt_Append(
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
    case GS_MODIFY_EXPR:
      if (gs_tree_has_location(exp) == gs_true) // it would otherwise be -1
        lineno = gs_expr_lineno(exp);
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
        // OSP
        gs_t init_expr_opnd0 = gs_tree_operand(exp, 0);
        if ( gs_tree_code(init_expr_opnd0) == GS_INDIRECT_REF ) {
          gs_t init_expr_opnd1 = gs_tree_operand(exp, 1);
          gs_t var_decl = gs_tree_operand(init_expr_opnd0, 0);
          if ( gs_tree_code(var_decl) == GS_NON_LVALUE_EXPR )
            var_decl = gs_tree_operand(var_decl, 0);
          if ( ( gs_tree_code(init_expr_opnd1) == GS_TARGET_EXPR ||
                 ( gs_tree_code(init_expr_opnd1) == GS_NOP_EXPR &&
                   gs_tree_code(gs_tree_operand(init_expr_opnd1, 0)) == GS_TARGET_EXPR ) ) &&
               gs_tree_code(var_decl) == GS_VAR_DECL ) {
            ST *st = Get_ST(var_decl);
            WN *target_wn = WN_Ldid(Pointer_Mtype, 0, st, ST_type(st));
            wn = WGEN_Expand_Expr(init_expr_opnd1, TRUE, 0, 0, 0, 0, FALSE, FALSE, target_wn);
            break;
          }
        }
      }
#endif
      if (lang_cplus)
      {
	gs_t t = NULL;
        if (gs_tree_code(gs_tree_operand(exp, 1)) == GS_TARGET_EXPR) {
	  t = gs_tree_operand(exp, 1);
	  gs_set_tree_operand(t, 2, NULL);
        }
#ifdef KEY
        // Handle case where initializer is (nop (target ...)).  Bug 7792.
        else if (gs_tree_code(gs_tree_operand(exp, 1)) == GS_NOP_EXPR &&
                 gs_tree_code(gs_tree_operand(gs_tree_operand(exp, 1), 0))
		   == GS_TARGET_EXPR) {
	  t = gs_tree_operand(gs_tree_operand(exp, 1), 0);
	  gs_set_tree_operand(t, 2, NULL);
        }
#endif

        if (t != NULL) {
#ifdef KEY
	  // Only check for indirect_ref if the indirect_ref is created by
	  // kg++fe to access through the fake arg0, in order to avoid
	  // (indirect_ref (nop (var_decl))) which is generated by g++.
	  //
	  // The above is no longer true in kg++fe, now INDIRECT_REFs
	  // generated by g++ are also handled. Note we may need the same
	  // support here also.
	  gs_t init_expr_opnd0 = gs_tree_operand(exp, 0);
	  if (gs_tree_code(init_expr_opnd0) == GS_INDIRECT_REF &&
	      gs_tree_code(gs_tree_operand(init_expr_opnd0, 0)) == GS_RESULT_DECL) {
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
	  if (tmp_code == GS_VAR_DECL    ||
	      tmp_code == GS_RESULT_DECL ||
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
	  else if (gs_tree_code(initializer = gs_tree_operand(t,1)) ==
	              GS_AGGR_INIT_EXPR /*&&
	           gs_aggr_init_via_ctor_p(initializer)*/) {

	    gs_set_tree_operand(t, 0, gs_tree_operand(exp,0));
	    wn = WGEN_Expand_Expr(t);
	    break;
	  }
          // OSP_397, initializing a class member having copy-constructor 
          //          by the return value of a call. 
          // Pathcc's fix is removing the upper gs_aggr_init_via_ctor_p of GS_AGGR_INIT_EXPR.
          //   else if (gs_tree_code(initializer = gs_tree_operand(t,1)) == GS_AGGR_INIT_EXPR )
          // From the testing result, we adopt the strict pattern match:
          // +INIT_EXPR
          // |-+COMPONENT_REF <this->m>
          // |-+TARGET_EXPR
          //   |-+VAR_DECL <tmp>
          //   |-+AGGR_INIT_EXPR <=f();>
          //   |-+CALL_EXPR <cleanup the tmp>
          else if (code == GS_INIT_EXPR &&
              gs_tree_code(gs_tree_operand(exp, 0)) == GS_COMPONENT_REF &&
              gs_tree_code(t) == GS_TARGET_EXPR &&
              gs_tree_code(gs_tree_operand(t, 1)) == GS_AGGR_INIT_EXPR ) {
            gs_set_tree_operand(t, 0, gs_tree_operand(exp,0));
            wn = WGEN_Expand_Expr(t);
            break;
          }

           // bug 15210
           // |-+COMPONENT_REF <this->m>
           // |-+TARGET_EXPR
           //   |-+VAR_DECL <tmp>
           //   |-+COND_EXPR
           // replace the VAR_DECL with the COMPONENT_REF, and do an EVAL to keep the code
           // 1. g++fe would explicitly call copy constructor for operand 1 and 2, if the expression 
	   //   is just a variable of the same type.
           // 2. it would generate a target expr to hold the function call, if the expression is such a call, 
	   //   which returns the same type.
           // 3. constructor or conversion operators are called, if necessary, so we do not worry
           else if (code == GS_INIT_EXPR &&
               gs_tree_code(gs_tree_operand(exp, 0)) == GS_COMPONENT_REF &&
               gs_tree_code(t) == GS_TARGET_EXPR
              && gs_tree_code(gs_tree_operand(t, 1)) == GS_COND_EXPR &&
             gs_tree_code(gs_tree_operand(gs_tree_operand(t,1), 1)) == GS_TARGET_EXPR &&
             gs_tree_code(gs_tree_operand(gs_tree_operand(t,1), 2)) == GS_TARGET_EXPR ) {
             WN *eval;
 
             gs_set_tree_operand(t, 0, gs_tree_operand(exp,0));
             WGEN_Expand_Expr(t,FALSE);
//		eval = WN_CreateEval(WGEN_Expand_Expr(t));
//             WGEN_Stmt_Append (eval, Get_Srcpos());
 
             break;
           }


#endif
	  DevWarn ("INIT_EXPR/MODIFY_EXPR kid1 is TARGET_EXPR, kid0 is %s\n",
		   gs_code_name(gs_tree_code(gs_tree_operand(exp, 0))));
        }

      }
      // fall through

    case GS_PREDECREMENT_EXPR:
    case GS_PREINCREMENT_EXPR:
    case GS_POSTDECREMENT_EXPR:
    case GS_POSTINCREMENT_EXPR:
      {
	if (gs_tree_code(gs_tree_operand(exp, 1)) == GS_ERROR_MARK)
	    break;
        WN* call_return_val = NULL;

	// If gs_tree_operand(exp, 1) is a CALL_EXPR that returns a
	// ptr-to-member-function, then call
	// WGEN_Expand_Ptr_To_Member_Func_Call_Expr to expand it.  Otherwise,
	// call WGEN_Expand_Expr to do regular expansion.  Bug 4737.
	gs_t exp_opnd1 = gs_tree_operand(exp, 1);
        // Skip the NOP_EXPRs
        while (gs_tree_code(exp_opnd1) == GS_NOP_EXPR)
          exp_opnd1 = gs_tree_operand (exp_opnd1, 0);
	if (WGEN_Call_Returns_Ptr_To_Member_Func(exp_opnd1)) {
	  TYPE_ID desc = TY_mtype(Get_TY(gs_tree_type(exp_opnd1)));
	  wn1 = WGEN_Expand_Ptr_To_Member_Func_Call_Expr(exp_opnd1, 0,
						       Widen_Mtype(desc), desc);
        } else
        {
          gs_t lhs = gs_tree_operand (exp, 0);
	  if (gs_tree_code(lhs) == GS_INDIRECT_REF &&
              gs_tree_code(gs_tree_operand (lhs, 0)) == GS_CALL_EXPR ) {
            // We have a function call in lhs, we need to promote it.
	    // The expr is f()=...; we must promote f() at first.
	    // GCC TREE is
	    // MODIFY_EXPR
	    //  +-0 INDIRECT_REF
	    //  |        +-0 CALL_EXPR
	    //  +-1 ...
	    //  Without this workaround, 
	    //    operand 1 will be expand first, then the CALL_EXPR in operand 0
	    //    if there are several assignment, for example, f()=g()=h(), it's wrong.
	    //  So we expand the CALL_EXPR at first, save its return value in call_return_val,
	    //    then, pass call_return_val to WGEN_Lhs_Of_Modify_Expr
	    //
	    WN*      call = WGEN_Expand_Expr(gs_tree_operand(lhs, 0));
	    ST*      preg_st;
	    PREG_NUM preg;
	    TY_IDX   call_ty_idx = Get_TY (gs_tree_type (gs_tree_operand (lhs, 0)));
            preg_st  = MTYPE_To_PREG(Pointer_Mtype);
	    preg     = Create_Preg (Pointer_Mtype, NULL);
	    WGEN_Set_ST_Addr_Saved (call);
	    WN* stid = WN_Stid (Pointer_Mtype, preg, preg_st, call_ty_idx, call);
	    WGEN_Stmt_Append(stid, Get_Srcpos());
            call_return_val = WN_Ldid(Pointer_Mtype, preg, preg_st, call_ty_idx);
          }
	
          wn1 = WGEN_Expand_Expr (gs_tree_operand (exp, 1)); // r.h.s.
        }

#ifdef KEY // wgen bugs 10849, 10893, 10908
	if (wn1 && WN_operator(wn1) == OPR_INTCONST && 
	    TY_size(Get_TY(gs_tree_type(gs_tree_operand(exp, 1)))) == 0)
	  break;
#endif
	wn  = WGEN_Lhs_Of_Modify_Expr(code, gs_tree_operand (exp, 0), call_return_val, need_result, 
				     0, NULL, 0, FALSE, wn1, 0, FALSE, FALSE);
      }
      break;

    // ternary ops

    case GS_BIT_FIELD_REF:
      {
        wn = WGEN_Expand_Expr (gs_tree_operand (exp, 0), TRUE, nop_ty_idx, 
			      component_ty_idx, component_offset,
			      field_id, FALSE);
	ty_idx = Get_TY (gs_tree_type(exp));
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
            && (desc == rtype))
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
	INT bofst = gs_get_integer_value(gs_tree_operand(exp, 2));
	INT bsiz =gs_get_integer_value(gs_tree_operand(exp, 1));
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
	  // bug 12394
	  if (WN_operator(wn) == OPR_ILOAD || WN_operator(wn) == OPR_ILDBITS)
	    WN_set_load_addr_ty(wn, Make_Pointer_Type(ty));
	}
#else
	  WN_set_ty (wn, MTYPE_To_TY (WN_desc(wn)));
#endif
      }
      break;

    // n-ary ops

    case GS_ARRAY_REF:
      {
	UINT xtra_BE_ofst = 0; 	// only needed for big-endian target
	TY_IDX elem_ty_idx;
	// generate the WHIRL array node
        wn0 = WGEN_Array_Expr(exp, &elem_ty_idx, 0, NULL, 0);

	// generate the iload node
	TY_IDX hi_ty_idx = Get_TY (gs_tree_type(exp));
	if (gs_tree_this_volatile(exp))
	  Set_TY_is_volatile(hi_ty_idx);
	else Clear_TY_is_volatile(hi_ty_idx);
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
		("WGEN_Expand_Expr: field id for bit-field exceeds limit"));
        WN* iload_ofst = NULL;
        if (xtra_BE_ofst != 0)
          iload_ofst = WGEN_Compose_Offset(component_offset,
                                           WN_Intconst(Pointer_Mtype, xtra_BE_ofst));
        else
          iload_ofst = component_offset;
	wn = WGEN_CreateIload(OPR_ILOAD, rtype,
			    is_bit_field ? MTYPE_BS : desc, 
			    iload_ofst,
			    field_id != 0 ? hi_ty_idx : ty_idx,
			    Make_Pointer_Type(elem_ty_idx, FALSE),
			    wn0, field_id);
      }
      break;

    case GS_AGGR_INIT_EXPR:
    case GS_CALL_EXPR:
      {
        BOOL is_gs_addr_arg = FALSE;
	gs_t arglist = gs_tree_operand (exp, 1);
        TYPE_ID ret_mtype;
        WN *call_wn;
        WN *arg_wn;
	TY_IDX  arg_ty_idx;
        TYPE_ID arg_mtype;
        INT num_args = 0;
	INT num_handlers = 0;
        INT i;
	gs_t list;
	arg0 = gs_tree_operand (exp, 0);
	gs_code_t code0 = gs_tree_code (arg0);
	// KEY:  true if type must be returned in mem
	BOOL return_in_mem = FALSE;
#ifdef KEY
	ST *ret_st = NULL;		// return symbol
	if (gs_tree_code(exp) == GS_AGGR_INIT_EXPR &&
	    gs_aggr_init_via_ctor_p(exp) &&
	    !is_aggr_init_via_ctor)
	{
	  Is_True (gs_tree_operand(exp,2),
	           ("WGEN_Expand_Expr: null slot for AGGR_INIT_EXPR"));
	  // bug 11188: We have not yet set up the first argument.
	  gs_set_tree_value (arglist, gs_tree_operand (exp, 2));
	  // make sure we take the address of this arg.
	  is_aggr_init_via_ctor = true;
	}
	processing_function_prototype = TRUE; /* bug 8346 */
#endif
	for (list = gs_tree_operand (exp, 1); list; list = gs_tree_chain (list)) {
          arg_ty_idx = Get_TY(gs_tree_type(gs_tree_value(list)));
          if (!WGEN_Keep_Zero_Length_Structs    &&
              TY_mtype (arg_ty_idx) == MTYPE_M &&
              TY_size (arg_ty_idx) == 0) {
            // zero length struct parameter
          }
          else
            num_args++;
        }
#ifdef KEY
        processing_function_prototype = FALSE; /* bug 8346 */
        if (gs_tree_code(exp) == GS_AGGR_INIT_EXPR)
        {
          // bug 11159: TREE_TYPE does not contain the return type.
          ty_idx = Get_TY(gs_tree_type(gs_tree_type(gs_tree_type(arg0))));
        }
        else
#endif
        ty_idx = Get_TY(gs_tree_type(exp));
	if (gs_tree_this_volatile(exp))
	  Set_TY_is_volatile(ty_idx);
	else Clear_TY_is_volatile(ty_idx);
        if (need_result) {
          if (!WGEN_Keep_Zero_Length_Structs  &&
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
            if (gs_tree_code(exp) == GS_AGGR_INIT_EXPR && !target_wn)
            {
              // bug 11169: Use the slot to set up the target.
              gs_t slot = gs_tree_operand(exp,2);
              FmtAssert (gs_tree_code(slot) == GS_VAR_DECL,
                         ("WGEN_Expand_Expr: Expected VAR_DECL for "
                          "AGGR_INIT_EXPR slot"));
              ST * target_st = Get_ST(slot);
              target_wn = WN_Lda (Pointer_Mtype, 0, target_st, 0);
            }
	  }
#endif
        }
        else
          ret_mtype = MTYPE_V;
        st = NULL;
        if (code0 == GS_ADDR_EXPR                  &&
            gs_tree_code (gs_tree_operand (arg0, 0))) {
	  gs_t func = gs_tree_operand (arg0, 0);
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
          
	  if (gs_decl_built_in (func)) {
	    if (gs_decl_built_in_class(func) != GSBI_CLASS_BUILT_IN_MD) {

            switch (gs_decl_function_code (func)) {

	      case GSBI_END_BUILTINS:
		break;

	      case GSBI_BUILT_IN_STDARG_START:
#ifdef KEY
	      case GSBI_BUILT_IN_VA_START:
#endif
	      {
#ifdef TARG_X8664
		if( TARGET_64BIT ){
		  iopc = INTRN_VA_START;
		  break;
		}
#endif
		arg1 = gs_tree_value (arglist);
		arg2 = gs_tree_value (gs_tree_chain (arglist));
		WN *arg_wn = WGEN_Expand_Expr (arg1);
		TY_IDX arg_ty_idx = Get_TY (gs_tree_type (arg1));
		while (gs_tree_code(arg2) == GS_NOP_EXPR
		       || gs_tree_code(arg2) == GS_CONVERT_EXPR
		       || gs_tree_code(arg2) == GS_NON_LVALUE_EXPR
		       || gs_tree_code(arg2) == GS_INDIRECT_REF)
		  arg2 = gs_tree_operand (arg2, 0);
		ST *st2 = Get_ST (arg2);
#if defined(TARG_X8664) || defined(TARG_SL)
		const int align = TARGET_64BIT ? 8 : 4;
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

		WGEN_Stmt_Append (wn, Get_Srcpos());
		whirl_generated = TRUE;
		wn = NULL;
		break;
	      }

	      case GSBI_BUILT_IN_VA_COPY:
	      {
		arg1 = gs_tree_value (arglist);
		arg2 = gs_tree_value (gs_tree_chain (arglist));
                TY_IDX arg_ty_idx = Get_TY (gs_tree_type (arg1));


#if defined(TARG_X8664) || defined(TARG_SL)
		/* Under -m32, convert a __builtin_va_copy to an assignment if the
		   type of va_list is not array.
		   Also, the original code seems to only work for -m64, like other
		   va_XYZ code; under -m32, the source address is wrong.  (bug#2601)
		   (But even under -m64, the using of memcpy is unnecessary.)
		 */
		if( !TARGET_64BIT ){
		  FmtAssert( gs_tree_code(arglist) != GS_ARRAY_TYPE,
			     ("unexpected array type for intrinsic 'va_copy'") );
		  WN* addr = WGEN_Expand_Expr( arg1 );
		  WN* value = WGEN_Expand_Expr( arg2 );
		  wn = WN_CreateIstore( OPR_ISTORE, MTYPE_V, Pointer_Mtype,
					0, arg_ty_idx, value, addr, 0 );

		  WGEN_Stmt_Append( wn, Get_Srcpos() );
		  whirl_generated = TRUE;
		  wn = NULL;
		  break;
		}
#endif // TARG_X8664

#ifdef TARG_IA64
		// For IA64, the memcpy is not necessary
		// We use IStore directly
                FmtAssert( gs_tree_code(arglist) != GS_ARRAY_TYPE,
                           ("unexpected array type for intrinsic 'va_copy'") );
                WN* addr = WGEN_Expand_Expr( arg1 );
                WN* value = WGEN_Expand_Expr( arg2 );
                wn = WN_CreateIstore( OPR_ISTORE, MTYPE_V, Pointer_Mtype,
                                      0, arg_ty_idx, value, addr, 0 );

                WGEN_Stmt_Append( wn, Get_Srcpos() );
                whirl_generated = TRUE;
                wn = NULL;
                break;
#endif
		// These code are for X8664, 64bit
		WN *dst  = WN_CreateParm (Pointer_Mtype, WGEN_Expand_Expr (arg1),
					  arg_ty_idx, WN_PARM_BY_VALUE);
		WN *src  = WN_CreateParm (Pointer_Mtype, WGEN_Expand_Expr (arg2),
					  arg_ty_idx, WN_PARM_BY_VALUE);
		WN *size = WN_CreateParm (MTYPE_I4,
					  WN_Intconst(MTYPE_I4,TY_size(TY_pointed(arg_ty_idx))),
					  Be_Type_Tbl(MTYPE_I4), WN_PARM_BY_VALUE);
		wn = WN_Create (OPR_INTRINSIC_CALL, ret_mtype, MTYPE_V, 3);
		WN_intrinsic (wn) = INTRN_MEMCPY;
		WN_kid0 (wn) = dst;
		WN_kid1 (wn) = src;
		WN_kid2 (wn) = size;
		WGEN_Stmt_Append (wn, Get_Srcpos());
		whirl_generated = TRUE;
		wn = NULL;
		break;
	      }

	      case GSBI_BUILT_IN_VA_END:
	      {
		arg1 = gs_tree_value (arglist);
		wn = WN_CreateEval ( WGEN_Expand_Expr (arg1) );
		WGEN_Stmt_Append (wn, Get_Srcpos());
		whirl_generated = TRUE;
		wn = NULL;
		break;
	      }

	      case GSBI_BUILT_IN_NEXT_ARG:
	      {
                gs_t last_parm = gs_tree_last 
				   (gs_decl_arguments(Current_Function_Decl()));
		gs_code_t last_parm_code = gs_tree_code(last_parm);
		while (last_parm_code == GS_NOP_EXPR
		       || last_parm_code == GS_CONVERT_EXPR
		       || last_parm_code == GS_NON_LVALUE_EXPR
		       || last_parm_code == GS_INDIRECT_REF)
		  last_parm = gs_tree_operand (last_parm, 0);
		ST *st = Get_ST (last_parm);
		arg_wn = WN_Lda (Pointer_Mtype, ST_ofst(st), st);
		wn = WN_Binary (OPR_ADD, Pointer_Mtype, arg_wn,
				WN_Intconst (Pointer_Mtype,
					     Parameter_Size(ST_size(st))));
                whirl_generated = TRUE;
		break;
	      }

              case GSBI_BUILT_IN_ALLOCA:
		Set_PU_has_alloca (Get_Current_PU ());
		Set_PU_has_user_alloca (Get_Current_PU ());
                arg_wn = WGEN_Expand_Expr (gs_tree_value (gs_tree_operand (exp, 1)));
	        wn = WN_CreateAlloca (arg_wn);
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
		    /* Arg could be non-pointer if user redeclared this fcn wrong.  */
		    || gs_tree_code (gs_tree_type (gs_tree_value (arglist))) != GS_POINTER_TYPE
		    || gs_tree_chain (arglist) == 0
		    || gs_tree_code (gs_tree_type (gs_tree_value (gs_tree_chain (arglist)))) != GS_POINTER_TYPE)
		  break;
		else {
		  arg1 = gs_tree_value (arglist);
		  arg2 = gs_tree_value (gs_tree_chain (arglist));
		  gs_t len1 = c_strlen (arg1);
		  if (len1) {
		    gs_t len2 = c_strlen (arg2);
		    if (len2) {
		      char *ptr1 = get_string_pointer (WGEN_Expand_Expr (arg1));
		      char *ptr2 = get_string_pointer (WGEN_Expand_Expr (arg2));
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

              case GSBI_BUILT_IN_STRLEN:
#ifdef GPLUSPLUS_FE
		iopc = INTRN_STRLEN;
#else
		if (arglist == 0
		/* Arg could be non-pointer if user redeclared this fcn wrong.  */
		   || gs_tree_code (gs_tree_type (gs_tree_value (arglist))) != GS_POINTER_TYPE)
		  break;
		else {
		  gs_t src = gs_tree_value (arglist);
		  gs_t len = c_strlen (src);
		  if (len) {
		    wn = WGEN_Expand_Expr (len);
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
	    case GSBI_BUILT_IN_FLOOR:
#ifdef FE_GNU_4_2_0
	    case GSBI_BUILT_IN_LFLOOR:
#endif
	    {
	      arg_wn = WGEN_Expand_Expr (gs_tree_value (gs_tree_operand (exp, 1)));
#if defined TARG_MIPS  // bug 12594: call library function for floor
              iopc = INTRN_FLOOR;
              intrinsic_op = TRUE;
#else	      
	      wn = WN_CreateExp1 (OPR_FLOOR, ret_mtype, MTYPE_F8, arg_wn);
              whirl_generated = TRUE;
#endif
	      break;
            }

	    case GSBI_BUILT_IN_FLOORF:
#ifdef FE_GNU_4_2_0
	    case GSBI_BUILT_IN_LFLOORF:
#endif
	    {
	      arg_wn = WGEN_Expand_Expr (gs_tree_value (gs_tree_operand (exp, 1)));
#if defined TARG_MIPS  // bug 12594: call library function for floor
              iopc = INTRN_FLOORF;
              intrinsic_op = TRUE;
#else
	      
	      wn = WN_CreateExp1 (OPR_FLOOR, ret_mtype, MTYPE_F4, arg_wn);
              whirl_generated = TRUE;
#endif
	      break;
            }

#ifndef TARG_MIPS  // Bug 9071: MIPS needs quad emulation for floor operation
            case GSBI_BUILT_IN_FLOORL:
#ifdef FE_GNU_4_2_0
            case GSBI_BUILT_IN_LFLOORL:
#endif
              arg_wn = WGEN_Expand_Expr (gs_tree_value (gs_tree_operand (exp, 1)));
#ifdef TARG_IA64
	      wn = WN_CreateExp1 (OPR_FLOOR, ret_mtype, MTYPE_F10, arg_wn);
#elif defined(TARG_X8664)
	      wn = WN_CreateExp1 (OPR_FLOOR, MTYPE_F10, MTYPE_F10, arg_wn);
	      if (ret_mtype != MTYPE_F10)
		wn = WN_Type_Conversion(wn, ret_mtype);
#else
	      wn = WN_CreateExp1 (OPR_FLOOR, ret_mtype, MTYPE_FQ, arg_wn);
#endif
	      whirl_generated = TRUE;
              break;
#endif // !TARG_MIPS
#endif

	      case GSBI_BUILT_IN_SQRT:
              case GSBI_BUILT_IN_SQRTF:
#ifndef TARG_MIPS  // MIPS needs quad emulation for sqrt operation
              case GSBI_BUILT_IN_SQRTL:
#endif
	       	if (ret_mtype == MTYPE_V) ret_mtype = MTYPE_F8;
		if (! gs_flag_errno_math(program)) {
		  arg_wn = WGEN_Expand_Expr (gs_tree_value (gs_tree_operand (exp, 1)));
		  wn = WN_CreateExp1 (OPR_SQRT, ret_mtype, MTYPE_V, arg_wn);
		}
		else wn = WGEN_Expand_Math_Errno_Sqrt(exp, ty_idx, ret_mtype);
		whirl_generated = TRUE;
		break;

#ifdef KEY
	      case GSBI_BUILT_IN_SINF:
#endif
              case GSBI_BUILT_IN_SIN:
		intrinsic_op = TRUE;
#ifdef TARG_X8664
                if (!Force_IEEE_Comparisons)
                {
                  iopc = INTRN_SINL;
		  if (ret_mtype != MTYPE_F10)
		  {
		    // generate a cvt to 'cvt_to'
		    cvt_to = ret_mtype;
		    ret_mtype = MTYPE_F10;
		  }
                  break;
                }
#endif
#ifdef KEY // bug 11305
                if (ret_mtype == MTYPE_V) ret_mtype = MTYPE_F8;
#endif
                if (ret_mtype == MTYPE_F4) iopc = INTRN_F4SIN;
                else if (ret_mtype == MTYPE_F8) iopc = INTRN_F8SIN;
                else Fail_FmtAssertion ("unexpected mtype for intrinsic 'sin'");
                break;

              case GSBI_BUILT_IN_COS:
#ifdef KEY
	      case GSBI_BUILT_IN_COSF:
#endif
		intrinsic_op = TRUE;
#ifdef TARG_X8664
                if (!Force_IEEE_Comparisons)
                {
                  iopc = INTRN_COSL;
		  if (ret_mtype != MTYPE_F10)
		  {
		    // generate a cvt to 'cvt_to'
		    cvt_to = ret_mtype;
		    ret_mtype = MTYPE_F10;
		  }
                  break;
                }
#endif
#ifdef KEY // bug 11305
                if (ret_mtype == MTYPE_V) ret_mtype = MTYPE_F8;
#endif
                if (ret_mtype == MTYPE_F4) iopc = INTRN_F4COS;
                else if (ret_mtype == MTYPE_F8) iopc = INTRN_F8COS;
                else Fail_FmtAssertion ("unexpected mtype for intrinsic 'cos'");
                break;

#ifdef KEY
              case GSBI_BUILT_IN_ACOS:
              case GSBI_BUILT_IN_ACOSF:
		if (ret_mtype == MTYPE_V) ret_mtype = MTYPE_F8;
		if (! gs_flag_errno_math(program)) {  // Bug 14262
		  if (ret_mtype == MTYPE_F4) iopc = INTRN_F4ACOS;
		  else if (ret_mtype == MTYPE_F8) iopc = INTRN_F8ACOS;
		  else Fail_FmtAssertion ("unexpected mtype for intrinsic 'acos'");
		  intrinsic_op = TRUE;
		}
                break;

              case GSBI_BUILT_IN_ASIN:
              case GSBI_BUILT_IN_ASINF:
		if (ret_mtype == MTYPE_V) ret_mtype = MTYPE_F8;
		if (! gs_flag_errno_math(program)) {  // Bug 14262
		  if (ret_mtype == MTYPE_F4) iopc = INTRN_F4ASIN;
		  else if (ret_mtype == MTYPE_F8) iopc = INTRN_F8ASIN;
		  else Fail_FmtAssertion ("unexpected mtype for intrinsic 'asin'");
		  intrinsic_op = TRUE;
		}
                break;

              case GSBI_BUILT_IN_ATAN:
              case GSBI_BUILT_IN_ATANF:
                 if (ret_mtype == MTYPE_V) ret_mtype = MTYPE_F8;
                 if (ret_mtype == MTYPE_F4) iopc = INTRN_F4ATAN;
                else if (ret_mtype == MTYPE_F8) iopc = INTRN_F8ATAN;
                else Fail_FmtAssertion ("unexpected mtype for intrinsic 'atan'");
                intrinsic_op = TRUE;
                break;

              case GSBI_BUILT_IN_ATAN2:
              case GSBI_BUILT_IN_ATAN2F:
                 if (ret_mtype == MTYPE_V) ret_mtype = MTYPE_F8;
                 if (ret_mtype == MTYPE_F4) iopc = INTRN_F4ATAN2;
                else if (ret_mtype == MTYPE_F8) iopc = INTRN_F8ATAN2;
                else Fail_FmtAssertion ("unexpected mtype for intrinsic 'atan2'");
                intrinsic_op = TRUE;
                break;

              case GSBI_BUILT_IN_SINH:
              case GSBI_BUILT_IN_SINHF:
                 if (ret_mtype == MTYPE_V) ret_mtype = MTYPE_F8;
                 if (ret_mtype == MTYPE_F4) iopc = INTRN_F4SINH;
                else if (ret_mtype == MTYPE_F8) iopc = INTRN_F8SINH;
                else Fail_FmtAssertion ("unexpected mtype for intrinsic 'sinh'");
                intrinsic_op = TRUE;
                break;

              case GSBI_BUILT_IN_COSH:
              case GSBI_BUILT_IN_COSHF:
                 if (ret_mtype == MTYPE_V) ret_mtype = MTYPE_F8;
                 if (ret_mtype == MTYPE_F4) iopc = INTRN_F4COSH;
                else if (ret_mtype == MTYPE_F8) iopc = INTRN_F8COSH;
                else Fail_FmtAssertion ("unexpected mtype for intrinsic 'cosh'");
                intrinsic_op = TRUE;
                break;

              case GSBI_BUILT_IN_TANH:
              case GSBI_BUILT_IN_TANHF:
                 if (ret_mtype == MTYPE_V) ret_mtype = MTYPE_F8;
                 if (ret_mtype == MTYPE_F4) iopc = INTRN_F4TANH;
                else if (ret_mtype == MTYPE_F8) iopc = INTRN_F8TANH;
                else Fail_FmtAssertion ("unexpected mtype for intrinsic 'tanh'");
                intrinsic_op = TRUE;
                break;

              case GSBI_BUILT_IN_LOG10:
              case GSBI_BUILT_IN_LOG10F:
		if (ret_mtype == MTYPE_V) ret_mtype = MTYPE_F8;
		if (! gs_flag_errno_math(program)) {  // Bug 14262
		  if (ret_mtype == MTYPE_F4) iopc = INTRN_F4LOG10;
		  else if (ret_mtype == MTYPE_F8) iopc = INTRN_F8LOG10;
		  else Fail_FmtAssertion ("unexpected mtype for intrinsic 'log10'");
		  intrinsic_op = TRUE;
		}
                break;

              case GSBI_BUILT_IN_LOG:
              case GSBI_BUILT_IN_LOGF:
		if (ret_mtype == MTYPE_V) ret_mtype = MTYPE_F8;

		if (! gs_flag_errno_math(program)) {  // Bug 14262
		  if (ret_mtype == MTYPE_F4) iopc = INTRN_F4LOG;
		  else if (ret_mtype == MTYPE_F8) iopc = INTRN_F8LOG;
		  else Fail_FmtAssertion ("unexpected mtype for intrinsic 'log'");
		  intrinsic_op = TRUE;
		}
                break;

              case GSBI_BUILT_IN_EXP:
              case GSBI_BUILT_IN_EXPF:
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

	      case GSBI_BUILT_IN_POW:
	      case GSBI_BUILT_IN_POWF:
                // Bug 8195: If for whatever reason the pow(3) call is unused,
                // need_result will be false. Then, the value that this very
                // function assigns to ret_mtype for pow(3) is MTYPE_V. So,
                // just like we handle BUILT_IN_EXP above, we need to reassign
                // ret_mtype to MTYPE_F8.

                // Note that since pow[lf](3) are not builtin's (unlike the way
                // exp[lf]?(3)'s are), we only permit ret_mtype MTYPE_F8 here.

                if (ret_mtype == MTYPE_V) ret_mtype = MTYPE_F8;
		if (! gs_flag_errno_math(program)) {  // Bug 14262
                  if (ret_mtype == MTYPE_F4) iopc = INTRN_F4EXPEXPR;
                  else if (ret_mtype == MTYPE_F8) iopc = INTRN_F8EXPEXPR;
		  else Fail_FmtAssertion ("unexpected mtype for intrinsic 'pow'");
		  intrinsic_op = TRUE;
		}
		break;

	      case GSBI_BUILT_IN_POWI: // bug 10963
		if (ret_mtype == MTYPE_V) ret_mtype = MTYPE_F8;
		FmtAssert(ret_mtype == MTYPE_F8,
			  ("unexpected mtype for intrinsic 'powi'"));
		iopc = INTRN_F8F8I4EXPEXPR;
		intrinsic_op = TRUE;
		break;

	      case GSBI_BUILT_IN_POWIF: // bug 11246
		if (ret_mtype == MTYPE_V) ret_mtype = MTYPE_F4;
		FmtAssert(ret_mtype == MTYPE_F4,
			  ("unexpected mtype for intrinsic 'powif'"));
		intrinsic_op = TRUE;
		iopc = INTRN_F4F4I4EXPEXPR;
		break;

	      case GSBI_BUILT_IN_POWIL: // bug 11246
#if defined(TARG_IA64) || defined(TARG_X8664)
		// on IA64 or TARG_8664, we use MTYPE_F10 for long double
		if (ret_mtype == MTYPE_V) ret_mtype = MTYPE_F10;
		FmtAssert(ret_mtype == MTYPE_F10,
                          ("unexpected mtype for intrinsic 'powil'"));
		iopc = INTRN_F10F10I4EXPEXPR;
#else		
		if (ret_mtype == MTYPE_V) ret_mtype = MTYPE_FQ;
		FmtAssert(ret_mtype == MTYPE_FQ,
			  ("unexpected mtype for intrinsic 'powil'"));
		iopc = INTRN_FQFQI4EXPEXPR;
#endif
		intrinsic_op = TRUE;
		break;
#endif // KEY

              case GSBI_BUILT_IN_CONSTANT_P:
              {
                gs_t arg = gs_tree_value (gs_tree_operand (exp, 1));
                arg = gs_strip_nops (arg);
                if (gs_really_constant_p (arg)
                    || (gs_tree_code (arg) == GS_ADDR_EXPR
                        && gs_tree_code (gs_tree_operand (arg, 0)) == GS_STRING_CST))
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
//                wn = WGEN_Expand_Expr (gs_tree_value (gs_tree_operand (exp, 1)));
                whirl_generated = TRUE;
#endif // KEY
                break;
              }
#if FE_GNU_4_2_0
              case GSBI_BUILT_IN_LOCK_TEST_AND_SET_1:
                wn = emit_builtin_lock_test_and_set (INTRN_LOCK_TEST_AND_SET_I1, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_LOCK_TEST_AND_SET_2:
                wn = emit_builtin_lock_test_and_set (INTRN_LOCK_TEST_AND_SET_I2, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_LOCK_TEST_AND_SET_4:
                wn = emit_builtin_lock_test_and_set (INTRN_LOCK_TEST_AND_SET_I4, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_LOCK_TEST_AND_SET_8:
                wn = emit_builtin_lock_test_and_set (INTRN_LOCK_TEST_AND_SET_I8, exp, num_args-2);
                whirl_generated = TRUE;
                break;

              case GSBI_BUILT_IN_LOCK_RELEASE_1:
                emit_builtin_lock_release (INTRN_LOCK_RELEASE_I1, exp, num_args-1);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_LOCK_RELEASE_2:
                emit_builtin_lock_release (INTRN_LOCK_RELEASE_I2, exp, num_args-1);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_LOCK_RELEASE_4:
                emit_builtin_lock_release (INTRN_LOCK_RELEASE_I4, exp, num_args-1);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_LOCK_RELEASE_8:
                emit_builtin_lock_release (INTRN_LOCK_RELEASE_I8, exp, num_args-1);
                whirl_generated = TRUE;
                break;

              case GSBI_BUILT_IN_VAL_COMPARE_AND_SWAP_1:
                wn = emit_builtin_compare_and_swap (INTRN_COMPARE_AND_SWAP_I1, exp, num_args-3);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_VAL_COMPARE_AND_SWAP_2:
                wn = emit_builtin_compare_and_swap (INTRN_COMPARE_AND_SWAP_I2, exp, num_args-3);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_VAL_COMPARE_AND_SWAP_4:
                wn = emit_builtin_compare_and_swap (INTRN_COMPARE_AND_SWAP_I4, exp, num_args-3);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_VAL_COMPARE_AND_SWAP_8:
                wn = emit_builtin_compare_and_swap (INTRN_COMPARE_AND_SWAP_I8, exp, num_args-3);
                whirl_generated = TRUE;
                break;

              case GSBI_BUILT_IN_BOOL_COMPARE_AND_SWAP_1:
                wn = emit_builtin_compare_and_swap (INTRN_BOOL_COMPARE_AND_SWAP_I1, exp, num_args-3);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_BOOL_COMPARE_AND_SWAP_2:
                wn = emit_builtin_compare_and_swap (INTRN_BOOL_COMPARE_AND_SWAP_I2, exp, num_args-3);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_BOOL_COMPARE_AND_SWAP_4:
                wn = emit_builtin_compare_and_swap (INTRN_BOOL_COMPARE_AND_SWAP_I4, exp, num_args-3);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_BOOL_COMPARE_AND_SWAP_8:
                wn = emit_builtin_compare_and_swap (INTRN_BOOL_COMPARE_AND_SWAP_I8, exp, num_args-3);
                whirl_generated = TRUE;
                break;
#endif

#ifdef FE_GNU_4_2_0
              case GSBI_BUILT_IN_FETCH_AND_ADD_1:
                wn = emit_builtin_sync_fetch_op (INTRN_FETCH_AND_ADD_I1, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_FETCH_AND_ADD_2:
                wn = emit_builtin_sync_fetch_op (INTRN_FETCH_AND_ADD_I2, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_FETCH_AND_ADD_4:
                wn = emit_builtin_sync_fetch_op (INTRN_FETCH_AND_ADD_I4, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_FETCH_AND_ADD_8:
                wn = emit_builtin_sync_fetch_op (INTRN_FETCH_AND_ADD_I8, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_FETCH_AND_SUB_1:
                wn = emit_builtin_sync_fetch_op (INTRN_FETCH_AND_SUB_I1, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_FETCH_AND_SUB_2:
                wn = emit_builtin_sync_fetch_op (INTRN_FETCH_AND_SUB_I2, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_FETCH_AND_SUB_4:
                wn = emit_builtin_sync_fetch_op (INTRN_FETCH_AND_SUB_I4, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_FETCH_AND_SUB_8:
                wn = emit_builtin_sync_fetch_op (INTRN_FETCH_AND_SUB_I8, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_FETCH_AND_OR_1:
                wn = emit_builtin_sync_fetch_op (INTRN_FETCH_AND_OR_I1, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_FETCH_AND_OR_2:
                wn = emit_builtin_sync_fetch_op (INTRN_FETCH_AND_OR_I2, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_FETCH_AND_OR_4:
                wn = emit_builtin_sync_fetch_op (INTRN_FETCH_AND_OR_I4, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_FETCH_AND_OR_8:
                wn = emit_builtin_sync_fetch_op (INTRN_FETCH_AND_OR_I8, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_FETCH_AND_XOR_1:
                wn = emit_builtin_sync_fetch_op (INTRN_FETCH_AND_XOR_I1, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_FETCH_AND_XOR_2:
                wn = emit_builtin_sync_fetch_op (INTRN_FETCH_AND_XOR_I2, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_FETCH_AND_XOR_4:
                wn = emit_builtin_sync_fetch_op (INTRN_FETCH_AND_XOR_I4, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_FETCH_AND_XOR_8:
                wn = emit_builtin_sync_fetch_op (INTRN_FETCH_AND_XOR_I8, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_FETCH_AND_AND_1:
                wn = emit_builtin_sync_fetch_op (INTRN_FETCH_AND_AND_I1, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_FETCH_AND_AND_2:
                wn = emit_builtin_sync_fetch_op (INTRN_FETCH_AND_AND_I2, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_FETCH_AND_AND_4:
                wn = emit_builtin_sync_fetch_op (INTRN_FETCH_AND_AND_I4, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_FETCH_AND_AND_8:
                wn = emit_builtin_sync_fetch_op (INTRN_FETCH_AND_AND_I8, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_FETCH_AND_NAND_1:
                wn = emit_builtin_sync_fetch_op (INTRN_FETCH_AND_NAND_I1, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_FETCH_AND_NAND_2:
                wn = emit_builtin_sync_fetch_op (INTRN_FETCH_AND_NAND_I2, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_FETCH_AND_NAND_4:
                wn = emit_builtin_sync_fetch_op (INTRN_FETCH_AND_NAND_I4, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_FETCH_AND_NAND_8:
                wn = emit_builtin_sync_fetch_op (INTRN_FETCH_AND_NAND_I8, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_ADD_AND_FETCH_1:
                wn = emit_builtin_sync_fetch_op (INTRN_ADD_AND_FETCH_I1, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_ADD_AND_FETCH_2:
                wn = emit_builtin_sync_fetch_op (INTRN_ADD_AND_FETCH_I2, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_ADD_AND_FETCH_4:
                wn = emit_builtin_sync_fetch_op (INTRN_ADD_AND_FETCH_I4, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_ADD_AND_FETCH_8:
                wn = emit_builtin_sync_fetch_op (INTRN_ADD_AND_FETCH_I8, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_SUB_AND_FETCH_1:
                wn = emit_builtin_sync_fetch_op (INTRN_SUB_AND_FETCH_I1, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_SUB_AND_FETCH_2:
                wn = emit_builtin_sync_fetch_op (INTRN_SUB_AND_FETCH_I2, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_SUB_AND_FETCH_4:
                wn = emit_builtin_sync_fetch_op (INTRN_SUB_AND_FETCH_I4, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_SUB_AND_FETCH_8:
                wn = emit_builtin_sync_fetch_op (INTRN_SUB_AND_FETCH_I8, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_OR_AND_FETCH_1:
                wn = emit_builtin_sync_fetch_op (INTRN_OR_AND_FETCH_I1, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_OR_AND_FETCH_2:
                wn = emit_builtin_sync_fetch_op (INTRN_OR_AND_FETCH_I2, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_OR_AND_FETCH_4:
                wn = emit_builtin_sync_fetch_op (INTRN_OR_AND_FETCH_I4, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_OR_AND_FETCH_8:
                wn = emit_builtin_sync_fetch_op (INTRN_OR_AND_FETCH_I8, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_XOR_AND_FETCH_1:
                wn = emit_builtin_sync_fetch_op (INTRN_XOR_AND_FETCH_I1, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_XOR_AND_FETCH_2:
                wn = emit_builtin_sync_fetch_op (INTRN_XOR_AND_FETCH_I2, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_XOR_AND_FETCH_4:
                wn = emit_builtin_sync_fetch_op (INTRN_XOR_AND_FETCH_I4, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_XOR_AND_FETCH_8:
                wn = emit_builtin_sync_fetch_op (INTRN_XOR_AND_FETCH_I8, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_AND_AND_FETCH_1:
                wn = emit_builtin_sync_fetch_op (INTRN_AND_AND_FETCH_I1, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_AND_AND_FETCH_2:
                wn = emit_builtin_sync_fetch_op (INTRN_AND_AND_FETCH_I2, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_AND_AND_FETCH_4:
                wn = emit_builtin_sync_fetch_op (INTRN_AND_AND_FETCH_I4, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_AND_AND_FETCH_8:
                wn = emit_builtin_sync_fetch_op (INTRN_AND_AND_FETCH_I8, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_NAND_AND_FETCH_1:
                wn = emit_builtin_sync_fetch_op (INTRN_NAND_AND_FETCH_I1, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_NAND_AND_FETCH_2:
                wn = emit_builtin_sync_fetch_op (INTRN_NAND_AND_FETCH_I2, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_NAND_AND_FETCH_4:
                wn = emit_builtin_sync_fetch_op (INTRN_NAND_AND_FETCH_I4, exp, num_args-2);
                whirl_generated = TRUE;
                break;
              case GSBI_BUILT_IN_NAND_AND_FETCH_8:
                wn = emit_builtin_sync_fetch_op (INTRN_NAND_AND_FETCH_I8, exp, num_args-2);
                whirl_generated = TRUE;
                break;
#endif

#ifdef FE_GNU_4_2_0
              case GSBI_BUILT_IN_SYNCHRONIZE:
                emit_builtin_synchronize (exp, num_args);
                whirl_generated = TRUE;
                break;

              case GSBI_BUILT_IN_GOMP_BARRIER:
                WGEN_expand_barrier ();
                whirl_generated = TRUE;
                break;
#endif

              case GSBI_BUILT_IN_RETURN_ADDRESS:
                i = gs_get_integer_value (gs_tree_value (gs_tree_operand (exp, 1)));
		if (i > 0) {
			// currently don't handle levels > 0,
			// which requires iterating thru call-stack
			// and finding $ra in fixed place.
			Warning("non-zero levels not supported for builtin_return_address");
			wn = WN_Intconst(Pointer_Mtype, 0);
		}
		else {
			st = WGEN_Get_Return_Address_ST (i);
			wn = WN_Ldid (Pointer_Mtype, 0, st, ST_type (st));
		}
                whirl_generated = TRUE;
		break;

#ifdef KEY
              case GSBI_BUILT_IN_EXTRACT_RETURN_ADDR:
		list = gs_tree_operand (exp, 1);
		wn   = WGEN_Expand_Expr (gs_tree_value (list));
                wn = WN_Binary (OPR_BAND, Pointer_Mtype, wn, 
				WN_Intconst(Pointer_Mtype, -2));
                whirl_generated = TRUE;
		break;

              case GSBI_BUILT_IN_FRAME_ADDRESS:
		Set_PU_has_alloca(Get_Current_PU());
		iopc = MTYPE_byte_size(Pointer_Mtype) == 4 ?
		   	 INTRN_U4READFRAMEPOINTER : INTRN_U8READFRAMEPOINTER;
		intrinsic_op = TRUE;
		break;
	      case GSBI_BUILT_IN_APPLY_ARGS:
#if defined(TARG_SL)
		ErrMsg(EC_Unimplemented_Feature, "__builtin_apply_args",
		  Orig_Src_File_Name?Orig_Src_File_Name:Src_File_Name, lineno);
#endif
                Set_PU_has_apply_args(Get_Current_PU());
		Set_PU_has_alloca(Get_Current_PU());
		iopc = INTRN_APPLY_ARGS;
		break;	
	      case GSBI_BUILT_IN_APPLY:
		{
#if defined(TARG_SL)
		  ErrMsg(EC_Unimplemented_Feature, "__builtin_apply",
		    Orig_Src_File_Name?Orig_Src_File_Name:Src_File_Name, lineno);
#endif
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
		  for (list = gs_tree_operand (exp, 1);
		       list;
		       list = gs_tree_chain (list)) {
		    arg_wn     = WGEN_Expand_Expr (gs_tree_value (list));
		    if (i == 1)
		      kid1 = arg_wn;
		    if (i == 2 && 
			WN_operator(arg_wn) != OPR_INTCONST) {
		      generate_mload = TRUE;
		      kid2 = arg_wn;
		    } else if (i == 2)
		      kid2 = arg_wn;
		    arg_ty_idx = Get_TY(gs_tree_type(gs_tree_value(list)));
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
#ifdef FE_GNU_4_2_0
		  WGEN_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL,
						       alloca_st_0);
#endif
		  WN *alloca_0 = 
		    WN_CreateAlloca (WN_CreateIntconst (OPC_I4INTCONST, 0));
		  WN *alloca_kid0 = alloca_0;
		  alloca_kid0 = 
		    WN_Stid (Pointer_Mtype, 
			     0, alloca_st_0, ty_idx, alloca_kid0);
		  WGEN_Stmt_Append (alloca_kid0, Get_Srcpos());
		  ST *alloca_st_1 = 
		    Gen_Temp_Symbol (ty_idx, 
				     "__builtin_apply_alloca1");
#ifdef FE_GNU_4_2_0
		  WGEN_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL,
		  				       alloca_st_1);
#endif
		  WN *alloca_1 = WN_CreateAlloca (kid2);
		  WN *alloca_kid1 = alloca_1;
		  alloca_kid1 = WN_Stid (Pointer_Mtype, 
					 0, alloca_st_1, ty_idx, alloca_kid1);
		  WGEN_Stmt_Append (alloca_kid1, Get_Srcpos());

		  // The src is actually in 0(kid1)
                  if (Is_Target_32bit())
                      kid1 = WN_CreateIload (OPR_ILOAD, MTYPE_I4, MTYPE_I4, 0,
                              MTYPE_To_TY(MTYPE_I4), 
                              Make_Pointer_Type(MTYPE_To_TY(MTYPE_U8)), 
                              kid1, 0);
                  else
                      kid1 = WN_CreateIload (OPR_ILOAD, MTYPE_I8, MTYPE_I8, 0,
                              MTYPE_To_TY(MTYPE_I8), 
                              Make_Pointer_Type(MTYPE_To_TY(MTYPE_U8)), 
                              kid1, 0);
		  load_wn = 
		    WN_CreateMload (0, 
				    Make_Pointer_Type(MTYPE_To_TY(MTYPE_U8)), 
				    kid1, kid2);
		  sp_addr = WN_LdidPreg(Is_Target_32bit() ? MTYPE_U4 : MTYPE_U8, Stack_Pointer_Preg_Offset); // $sp
		  WGEN_Stmt_Append(WN_CreateMstore (0, 
			      Make_Pointer_Type(MTYPE_To_TY(MTYPE_U8)), 
						   load_wn,
						   sp_addr,
						   kid2),
				  Get_Srcpos());

		  WGEN_Stmt_Append (call_wn, Get_Srcpos());

		  call_wn = WN_Create (OPR_ICALL, ret_mtype, MTYPE_V, 1);
		  WN_kid(call_wn, 0) = 
		    WGEN_Expand_Expr (gs_tree_value (gs_tree_operand (exp, 1)));
#if defined(TARG_X8664)
                  // We assume the function will return floating point to avoid
                  // Repair_Call_BB manually insert a "fldz" OP later.
                  WN_set_ty (call_wn, Make_Function_Type(MTYPE_To_TY(MTYPE_F10)));
#else
		  WN_set_ty (call_wn, TY_pointed(Get_TY(
			    gs_tree_type (gs_tree_value(gs_tree_operand (exp, 1))))));
#endif
		  WGEN_Stmt_Append (call_wn, Get_Srcpos());		

		  TY_IDX tyi;
		  TY& ty = New_TY(tyi);
		  TY_Init(ty, 48, KIND_STRUCT, MTYPE_M,
			  Save_Str("__apply"));
		  Set_TY_align(tyi, 8);
		  ST *tmpst = New_ST(CURRENT_SYMTAB);
		  ST_Init(tmpst, TY_name_idx(ty),
			  CLASS_VAR, SCLASS_AUTO, EXPORT_LOCAL, tyi);
		  Set_ST_is_temp_var(tmpst);

		  WN *load, *store;
                  WN_OFFSET offset = 0;

#if defined(TARG_X8664)
                  // We need to save %rax and %rdx both on x8664, however use MTYPE_M as
                  // the return type fails because of a check in lower_return_mstid function.
                  if (Is_Target_64bit()) {
                      TY_IDX int_return_tyi;
                      TY& int_return_ty = New_TY(int_return_tyi);
                      TY_Init(int_return_ty, 16, KIND_STRUCT, MTYPE_M, STR_IDX_ZERO);
                      Set_TY_align(int_return_tyi, 8);

                      load = WN_Ldid(MTYPE_M, -1, Return_Val_Preg, int_return_tyi);
                      store = WN_Stid(MTYPE_M, 
                              offset, tmpst, int_return_tyi, load);
                      offset += int_return_ty.size;
                  } else {
#endif
                  load = WN_Ldid(MTYPE_I8, -1, Return_Val_Preg, MTYPE_To_TY(MTYPE_I8));
                  store = WN_Stid(MTYPE_I8, 
                          offset, tmpst, Spill_Int_Type, load);
                  offset += 8;
#if defined(TARG_X8664)
                  }
#endif
		  WGEN_Stmt_Append (store, Get_Srcpos());		
#if !defined(TARG_SL)
		  // SL do not have float-point register
		  load = WN_Ldid(MTYPE_F10, -1, Return_Val_Preg, MTYPE_To_TY(MTYPE_F10)); 
		  store = WN_Stid(MTYPE_F10, 
				  offset, tmpst, Spill_Int_Type, load);
		  WGEN_Stmt_Append (store, Get_Srcpos());		
                  offset += 16;
#endif
#if defined(TARG_X8664)
                  // xmm0
		  load = WN_Ldid(MTYPE_V16F8, -1, Return_Val_Preg, MTYPE_To_TY(MTYPE_V16F8)); 
		  store = WN_Stid(MTYPE_V16F8, 
				  offset, tmpst, Spill_Int_Type, load);
		  WGEN_Stmt_Append (store, Get_Srcpos());		
                  offset += 16;
#endif
		  wn = WN_Lda (Pointer_Mtype, 0, tmpst);

		  // Dealloca/Restore SP
		  WN *dealloca_wn = WN_CreateDealloca (2);
		  WN_kid0 (dealloca_wn) = 
		    WN_Ldid (Pointer_Mtype, 
			     0, alloca_st_0, ST_type (alloca_st_0));
		  WN_kid1 (dealloca_wn) = 
		    WN_Ldid (Pointer_Mtype, 
			     0, alloca_st_1, ST_type (alloca_st_1));
		  WGEN_Stmt_Append (dealloca_wn, Get_Srcpos());		
		  
		  whirl_generated = TRUE;
		  break;
		}
	      case GSBI_BUILT_IN_RETURN:
#if defined(TARG_SL)
		ErrMsg(EC_Unimplemented_Feature, "__builtin_return",
		  Orig_Src_File_Name?Orig_Src_File_Name:Src_File_Name, lineno);
#endif
		Set_PU_has_alloca(Get_Current_PU());
		iopc = INTRN_RETURN;

                call_wn = WN_Create (OPR_INTRINSIC_CALL, ret_mtype, MTYPE_V, 
                        num_args);
                WN_intrinsic (call_wn) = iopc;
                WN_Set_Linenum (call_wn, Get_Srcpos());
                WN_Set_Call_Default_Flags (call_wn);
                i = 0;
                for (list = gs_tree_operand (exp, 1);
                        list;
                        list = gs_tree_chain (list)) {
                    arg_wn     = WGEN_Expand_Expr (gs_tree_value (list));
                    arg_ty_idx = Get_TY(gs_tree_type(gs_tree_value(list)));
                    arg_mtype  = TY_mtype(arg_ty_idx);
                    arg_wn = WN_CreateParm (Mtype_comparison (arg_mtype), 
                            arg_wn,
                            arg_ty_idx, WN_PARM_BY_VALUE);
                    WN_kid (call_wn, i++) = arg_wn;
                }
                WGEN_Stmt_Append (call_wn, Get_Srcpos());
                WGEN_Stmt_Append (WN_CreateReturn(), Get_Srcpos());

                whirl_generated = TRUE;
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
                if (ret_mtype == MTYPE_V) ret_mtype = MTYPE_I4; // bug 12344
#else
	        wn = WGEN_Expand_Expr (gs_tree_value (gs_tree_operand (exp, 1)));
	        whirl_generated = TRUE;
#endif
	        break;

              case GSBI_BUILT_IN_FFS:
                iopc = INTRN_I4FFS;
                intrinsic_op = TRUE;
                if (ret_mtype == MTYPE_V)
                  ret_mtype = MTYPE_I4;
                break;
#ifndef KEY
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
#endif
	      case GSBI_BUILT_IN_EXTEND_POINTER:
		wn = WGEN_Expand_Expr (gs_tree_value (gs_tree_operand (exp, 1)));
		whirl_generated = TRUE;
	        break;
	
	      case GSBI_BUILT_IN_OBJECT_SIZE:
	        iopc = INTRN_OBJECT_SIZE;
	        intrinsic_op = TRUE;
	        break;
               
#ifdef TARG_X8664
	      case GSBI_BUILT_IN_POPCOUNT:
	        iopc = INTRN_I4POPCNT;
		intrinsic_op = TRUE;
		break;
	      case GSBI_BUILT_IN_POPCOUNTL:
	        iopc = INTRN_I8POPCNT;
		intrinsic_op = TRUE;
		break;
	      case GSBI_BUILT_IN_POPCOUNTLL:
	        iopc = INTRN_POPCOUNT;
		intrinsic_op = TRUE;
		break;
#else
	      case GSBI_BUILT_IN_POPCOUNT:
	      case GSBI_BUILT_IN_POPCOUNTL:
	      case GSBI_BUILT_IN_POPCOUNTLL:
	        iopc = INTRN_POPCOUNT;
		intrinsic_op = TRUE;
		break;
#endif
	
	      case GSBI_BUILT_IN_PARITY:
	      case GSBI_BUILT_IN_PARITYL:
	      case GSBI_BUILT_IN_PARITYLL:
	        iopc = INTRN_PARITY;
		intrinsic_op = TRUE;
		break;

#ifdef TARG_MIPS
	      case GSBI_BUILT_IN_CLZ:
	      case GSBI_BUILT_IN_CLZL:
	      case GSBI_BUILT_IN_CLZLL:
		// For MIPS, Always inline-expand CLZ
		iopc = INTRN_CLZ32;
		intrinsic_op = TRUE;
		break;

	      case GSBI_BUILT_IN_CTZ:
	      case GSBI_BUILT_IN_CTZL:
	      case GSBI_BUILT_IN_CTZLL:
		// For MIPS, Always inline-expand CTZ
		iopc = INTRN_CTZ;
		intrinsic_op = TRUE;
		break;
#else	
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
#endif
	      case GSBI_BUILT_IN_TRAP:
		call_wn = WN_Create (OPR_CALL, MTYPE_V, MTYPE_V, 0);
		st = Get_ST (gs_tree_operand (arg0, 0));
		Set_ST_name_idx (st, Save_Str ("abort"));
		WN_st_idx (call_wn) = ST_st_idx (st);
		WN_Set_Linenum (call_wn, Get_Srcpos());
		WN_Set_Call_Default_Flags (call_wn);
		WGEN_Stmt_Append (call_wn, Get_Srcpos());
		whirl_generated = TRUE;
		break;

	      case GSBI_BUILT_IN_PREFETCH:
	        {
		  // prefetch address
		  gs_t pf_arg = gs_tree_operand (exp, 1);
		  WN * pf_addr = WGEN_Expand_Expr (gs_tree_value (pf_arg));
		  // Note 2nd/3rd argument optional
		  // read/write access
		  pf_arg = gs_tree_chain (pf_arg);
		  UINT32 pf_flag = 0;
		  int access = 0;
		  if (pf_arg && gs_tree_code (gs_tree_value (pf_arg)) == GS_INTEGER_CST)
		    access = gs_get_integer_value (gs_tree_value (pf_arg));
		  if (access == 0)
		    PF_SET_READ (pf_flag);
		  else // should be 1 (write access)
		    PF_SET_WRITE (pf_flag);
		  // Ignore 3rd argument which gives a measure of temporal
		  // locality. LNO does analyze the temporal locality, but
		  // not sure what is a good way to encode it in PREFETCH.
		  PF_SET_MANUAL (pf_flag); // manual prefetch
		  WGEN_Stmt_Append (WN_CreatePrefetch (0, pf_flag, pf_addr),
		                   Get_Srcpos());
		  whirl_generated = TRUE;
		}
		break;
#endif

#ifdef TARG_X8664
//            case GSBI_BUILT_IN_COSF:
              case GSBI_BUILT_IN_COSL:
                if (!Force_IEEE_Comparisons)
                {
                  iopc = INTRN_COSL;
                  intrinsic_op = TRUE;
		  if (ret_mtype != MTYPE_F10)
		  {
		    // generate a cvt to 'cvt_to'
		    cvt_to = ret_mtype;
		    ret_mtype = MTYPE_F10;
		  }
                }
                break;
                                                                                
//            case GSBI_BUILT_IN_SINF:
              case GSBI_BUILT_IN_SINL:
                if (!Force_IEEE_Comparisons)
                {
                  iopc = INTRN_SINL;
                  intrinsic_op = TRUE;
		  if (ret_mtype != MTYPE_F10)
		  {
		    // generate a cvt to 'cvt_to'
		    cvt_to = ret_mtype;
		    ret_mtype = MTYPE_F10;
		  }
                }
                break;
#endif // TARG_X8664

#ifdef KEY
              case GSBI_BUILT_IN_TAN:
              case GSBI_BUILT_IN_TANF:

                if (ret_mtype == MTYPE_V) ret_mtype = MTYPE_F8;
                if (ret_mtype == MTYPE_F4) iopc = INTRN_F4TAN;
                else if (ret_mtype == MTYPE_F8) iopc = INTRN_F8TAN;
                else Fail_FmtAssertion ("unexpected mtype for intrinsic 'tan'");
                intrinsic_op = TRUE;
                break;
/*
                // return type should only be F8 for tan
                if (ret_mtype == MTYPE_F8)
                {
                  iopc = INTRN_TAN;
                  intrinsic_op = TRUE;
                }
                break;
*/
#endif
#ifdef TARG_SL
              case GSBI_BUILT_IN_CVT64_HIGH:
                iopc = INTRN_CVT64_HIGH;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_CVT64_LOW:
                iopc = INTRN_CVT64_LOW;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_CVT32:
                iopc = INTRN_CVT32;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_LONGLONG_CVT64_HIGH:
                iopc = INTRN_LONGLONG_CVT64_HIGH;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_LONGLONG_CVT64_LOW:
                iopc = INTRN_LONGLONG_CVT64_LOW;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3AADDA:
                iopc = INTRN_C3AADDA;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3NEGA:
                iopc = INTRN_C3NEGA;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3BITR:
                iopc = INTRN_C3BITR;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3CS:
                iopc = INTRN_C3CS;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3DADD:
                iopc = INTRN_C3DADD;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3DMAC:
                iopc = INTRN_C3DMAC;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3DMACA:
                iopc = INTRN_C3DMAC_A;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3DMULA:
                iopc = INTRN_C3DMULA;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3DMULAA:
                iopc = INTRN_C3DMULA_A;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3DSHLLI:
                iopc = INTRN_C3DSHLL_I;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3FFE:
                iopc = INTRN_C3FFE;
                break;
              case GSBI_BUILT_IN_C3LD:
                iopc = INTRN_C3LD;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3ST:
                iopc = INTRN_C3ST;
                break;
              case GSBI_BUILT_IN_C3LEAD:
                iopc = INTRN_C3LEAD;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3MAC:
                iopc = INTRN_C3MAC;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3MACA:
                iopc = INTRN_C3MAC_A;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3MACAR:
                iopc = INTRN_C3MAC_AR;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3MACI:
                iopc = INTRN_C3MAC_I;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3MULA:
                iopc = INTRN_C3MULA;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3MULAA:
                iopc = INTRN_C3MULA_A;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3MULAAR:
                iopc = INTRN_C3MULA_AR;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3MULAI:
                iopc = INTRN_C3MULA_I;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3MULS:
                iopc = INTRN_C3MULS;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3MULUS:
                iopc = INTRN_C3MULUS;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3REVB:
                iopc = INTRN_C3REVB;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3ROUND:
                iopc = INTRN_C3ROUND;
                intrinsic_op = TRUE;
                break;
               case GSBI_BUILT_IN_C3SAADDA:
                iopc = INTRN_C3SAADD_A;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3SAADDHA:
                iopc = INTRN_C3SAADDH_A;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3SAADDS:
                iopc = INTRN_C3SAADDS;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3SAADDSH:
                iopc = INTRN_C3SAADDSH;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3SADDA:
                iopc = INTRN_C3SADDA;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3SADDAA:
                iopc = INTRN_C3SADDA_A;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3SAMULHA:
                iopc = INTRN_C3SAMULH_A;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3SAMULSH:
                iopc = INTRN_C3SAMULSH;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3SHAV:
                iopc = INTRN_C3SHAV;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3SHLAFAI:
                iopc = INTRN_C3SHLAFA_I;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3SHLATAI:
                iopc = INTRN_C3SHLATA_I;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3SHLAI:
                iopc = INTRN_C3SHLA_I;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3SUBC:
                iopc = INTRN_C3SUBC;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_INIT_HI:
                iopc = INTRN_INIT_HI;
                break;
              case GSBI_BUILT_IN_COPY_HI:
                iopc = INTRN_COPY_HI;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_C3_INIT_ACC:
                iopc = INTRN_C3_INIT_ACC;
                break;
              case GSBI_BUILT_IN_C3_SAVE_ACC:
                iopc = INTRN_C3_SAVE_ACC;
                break;
              case GSBI_BUILT_IN_C3_INIT_DACC:
                iopc = INTRN_C3_INIT_DACC;
                break;
              case GSBI_BUILT_IN_C3_SAVE_DACC:
                iopc = INTRN_C3_SAVE_DACC;
                break;
              case GSBI_BUILT_IN_C3_INIT_ADDR:
                iopc = INTRN_C3_INIT_ADDR;
                break;
              case GSBI_BUILT_IN_C3_SAVE_ADDR:
                iopc = INTRN_C3_SAVE_ADDR;
                break;
              case GSBI_BUILT_IN_C3_MVFS:
                iopc = INTRN_C3_MVFS;
                intrinsic_op = TRUE;
                break;
              case GSBI_BUILT_IN_SET_ADDR:
                iopc = INTRN_C3_SET_ADDR;
                break;
              case GSBI_BUILT_IN_SET_CIRCBUF:
                iopc = INTRN_C3_SET_CIRCBUF;
                break;
#endif
	      default:
		DevWarn ("Encountered BUILT_IN: %d at line %d\n",
			 gs_decl_function_code (func), lineno);
		break;
            }
	  }
	  else
	    {
#ifdef TARG_X8664
	      wn = WGEN_target_builtins (exp, &iopc, &intrinsic_op);
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
	    for (i = 0, list = gs_tree_operand (exp, 1);
		 list;
		 i++, list = gs_tree_chain (list)) {
	      // if intrinsic equals _mm_extract_pi16, 
	      // the second param is fixed to const 0,1,2,3, so 
	      // there is no need to generate the second arg node.
#ifdef TARG_X8664
              if (i ==1 && (iopc == INTRN_PEXTRW0 ||
                            iopc == INTRN_PEXTRW1 ||
                            iopc == INTRN_PEXTRW2 ||
                            iopc == INTRN_PEXTRW3)) {
                num_args = 1;
                break;
	      }
#endif
              arg_wn = WGEN_Expand_Expr (gs_tree_value (list));
	      
#ifdef KEY // bug 11286
	      if (i == 1 && TARGET_64BIT &&
		  (gs_decl_function_code(func) == GSBI_BUILT_IN_POWI ||
	           gs_decl_function_code(func) == GSBI_BUILT_IN_POWIF ||
	           gs_decl_function_code(func) == GSBI_BUILT_IN_POWIL)) {
		arg_wn = WN_Int_Type_Conversion(arg_wn, MTYPE_I8);
		arg_ty_idx = MTYPE_To_TY(MTYPE_I8);
		arg_mtype = MTYPE_I8;
	      }
	      else
#endif
	      {
		arg_ty_idx = Get_TY(gs_tree_type(gs_tree_value(list)));
		arg_mtype  = TY_mtype(arg_ty_idx);
	      }
              arg_wn     = WN_CreateParm (Mtype_comparison (arg_mtype), arg_wn,
					  arg_ty_idx, WN_PARM_BY_VALUE);
	      ikids [i]  = arg_wn;
	    }
#if defined(KEY) && defined(TARG_X8664)
	    // bug 11876: type in the tree node is wrong, so patch it
	    switch (INTRN_return_kind(iopc)) {
	    case IRETURN_M8I1: ret_mtype = MTYPE_M8I1; break;
	    case IRETURN_M8I2: ret_mtype = MTYPE_M8I2; break;
	    case IRETURN_M8I4: ret_mtype = MTYPE_M8I4; break;
            case IRETURN_I1:
            case IRETURN_I2:   ret_mtype = MTYPE_I4;   break; // promote to I4
	    default: ;
	    }
#else 
#if defined(TARG_SL)
            switch (ret_mtype) {
            case MTYPE_I1:
            case MTYPE_I2: ret_mtype = MTYPE_I4;   break;
            default: ;
            }
#endif
#endif
	    wn = WN_Create_Intrinsic (OPR_INTRINSIC_OP, ret_mtype, MTYPE_V,
				      iopc, num_args, ikids);
            WN_Set_Deref_If_Needed(wn);

        if (iopc == INTRN_OBJECT_SIZE) {
            // kludge to the undefined __builtin_object_size() problem (bug #586).  
            //
            wn = Fold_Object_Size (wn);
        }

#ifdef KEY
	    if (cvt_to != MTYPE_UNKNOWN) // bug 8251
              wn = WN_Cvt (ret_mtype, cvt_to, wn);
#endif
	    break;
	  }

	  if (iopc) {
            call_wn = WN_Create (OPR_INTRINSIC_CALL, ret_mtype, MTYPE_V, num_args);
	    WN_intrinsic (call_wn) = iopc;

#ifdef TARG_X8664
	    if (iopc == INTRN_STMXCSR) {
    	      ST *tmp_st = Gen_Temp_Symbol(MTYPE_To_TY(MTYPE_I4), "__stmxcsr");
	      arg_wn = WN_Lda(Pointer_Mtype, ST_ofst(tmp_st), tmp_st, 0);
	      arg_wn = WN_CreateParm(Pointer_Mtype, arg_wn,
				Make_Pointer_Type(MTYPE_To_TY(MTYPE_I4), FALSE),
				WN_PARM_BY_VALUE);
	      WN_kid (call_wn, 0) = arg_wn;
	      WN_set_kid_count(call_wn, 1);
	      wn0 = WN_CreateBlock ();
	      WN_INSERT_BlockLast (wn0, call_wn);
	      wn1 = WN_Ldid(MTYPE_I4, ST_ofst(tmp_st), tmp_st, MTYPE_To_TY(MTYPE_I4));
	      wn  = WN_CreateComma(OPR_COMMA, MTYPE_I4, MTYPE_V, wn0, wn1);
	      break;
	    }
#endif
	  }
	  else {
	    num_handlers = Current_Handler_Count();
            call_wn = WN_Create (OPR_CALL, ret_mtype, MTYPE_V,
                                 num_args + num_handlers);
	    ST *st2 = DECL_ST2 (gs_tree_operand (arg0, 0));

            if (Opt_Level > 0 && st2) {
              WN_st_idx (call_wn) = ST_st_idx (st2);
            }
            else {
	      st = Get_ST (gs_tree_operand (arg0, 0));
	      WN_st_idx (call_wn) = ST_st_idx (st);
            }
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
          
          wn_kid0 = WGEN_Expand_Expr (gs_tree_operand (exp, 0));
          WN_kid(call_wn, num_args-1) = wn_kid0;
          WN_set_ty (call_wn, TY_pointed(Get_TY(gs_tree_type (gs_tree_operand (exp, 0)))));

          /* 
             Check if the indirect call is a call to a virtual function 
             First, get the TY of the address and the field ID 
             1) For IA64, virtual function call is 
                     LDID object 
                   LDID object              ILOAD vptr field 
                 ILOAD vptr field   or    ADD offset 
               ICALL                    ICALL 
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
          gs_t func = gs_tree_operand (arg0, 0);
          if (gs_decl_inline (func)) {
            wgen_invoke_inliner = TRUE;
          }
          PU& pu_ent = Pu_Table[ST_pu(st)];
          // this_volatile flag in gcc means that the function will not return
          if (gs_tree_this_volatile(func)) {
            Set_PU_has_attr_noreturn (pu_ent);
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
		     ("WGEN_Expand_Expr: write target is NULL"));
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
	for (list = gs_tree_operand (exp, 1);
	     list;
	     list = gs_tree_chain (list)) {
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
	    ST * st = Get_ST(gs_tree_value(list));
	    arg_wn = WN_Lda(Pointer_Mtype, ST_ofst(st), st);
#endif
	    arg_ty_idx = Make_Pointer_Type(Get_TY(gs_tree_type(gs_tree_value(list))));
	  }
	  else {
#ifdef KEY
	    if (gs_tree_code (gs_tree_value (list)) == GS_ADDR_EXPR &&
	    	gs_tree_code (gs_tree_operand (gs_tree_value (list), 0)) == GS_TARGET_EXPR)
	    {
		gs_t targ = gs_tree_operand (gs_tree_value (list), 0);
            	WN *targ_wn = WGEN_Expand_Expr(targ);
            	arg_wn     = WN_Lda(Pointer_Mtype, 0, WN_st(targ_wn), 0);
	    	arg_ty_idx = Make_Pointer_Type(Get_TY(gs_tree_type(targ)));
	    }
	    else
	    {
#endif // KEY
            arg_wn     = WGEN_Expand_Expr (gs_tree_value (list));
	    arg_ty_idx = Get_TY(gs_tree_type(gs_tree_value(list)));
	    if (gs_tree_this_volatile(gs_tree_value(list)))
	      Set_TY_is_volatile(arg_ty_idx);
	    else Clear_TY_is_volatile(arg_ty_idx);
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
	  // gcc allows non-struct actual to correspond to a struct formal;
	  // fix mtype of parm node so as not to confuse back-end
	  if (arg_mtype == MTYPE_M) {
	    arg_mtype = WN_rtype(arg_wn);
	  }
          FmtAssert(arg_mtype != MTYPE_M || TY_size(WN_ty(arg_wn)) != -1,
                    ("WGEN_Expand_Expr: TODO: pass variable length object"));
          arg_wn = WN_CreateParm (Mtype_comparison (arg_mtype), arg_wn,
		    		  arg_ty_idx, WN_PARM_BY_VALUE);
          WN_kid (call_wn, i++) = arg_wn;
        }

#ifdef ADD_HANDLER_INFO
	if (num_handlers) 
	  Add_Handler_Info (call_wn, i, num_handlers);
#endif

#ifdef KEY
	if (emit_exceptions && !gs_tree_nothrow(exp) && 
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
            	WGEN_Stmt_Push (region_body, wgen_stmk_call_region_body, Get_Srcpos());
	    }
	} else if (emit_exceptions && inside_eh_region && opt_regions)
	{
	    // The above conditions dictate that this call MUST not be inside
	    // a region. So close the region.
	    // TODO: Is this only for opt_regions or in general?
	    if (Check_For_Call_Region ())
	    	Did_Not_Terminate_Region = FALSE;
	}
#endif // KEY

#ifdef TARG_SL
        WN_Set_Deref_If_Needed(call_wn);
#endif
        if (ret_mtype == MTYPE_V
#ifdef KEY
	   // If the result is already put into the preferred symbol, then emit
	   // the call and we're done.
	   || return_in_mem
#endif
	   ) {
	  WGEN_Stmt_Append (call_wn, Get_Srcpos());
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
#ifdef TARG_X8664
          if((ret_mtype == MTYPE_M) && (Is_Target_64bit()))
          {
            for (list = gs_tree_operand (exp, 1); list;
                 list = gs_tree_chain (list)) {
              if(gs_tree_code(gs_tree_value (list)) == GS_ADDR_EXPR)
              {
                if (gs_tree_code(
                    gs_tree_operand(gs_tree_value (list),0)) == GS_VAR_DECL )
                {
                  gs_t argument = gs_tree_operand(gs_tree_value (list),0);
                  if(gs_decl_initial(argument))
                    is_gs_addr_arg = TRUE;
                }
              }
           }
         }
#endif
	  wn1 = WN_Ldid (ret_mtype, -1, Return_Val_Preg, ty_idx);
#ifdef TARG_X8664
	  if (ret_mtype == MTYPE_M && (!is_gs_addr_arg)) { // copy the -1 preg to a temp area
#else
          if (ret_mtype == MTYPE_M){// copy the -1 preg to a temp area
#endif
	    TY_IDX ret_ty_idx = ty_idx;
            is_gs_addr_arg = FALSE;
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
#ifdef FE_GNU_4_2_0
	    WGEN_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, ret_st);
#endif

	    if (!return_in_mem) {
	      wn1 = WN_Stid (ret_mtype, 0, ret_st, ty_idx, wn1);
	      WN_INSERT_BlockLast (wn0, wn1);
	    }

	    // ritual for determining the right mtypes to be used in the LDID
            UINT xtra_BE_ofst = 0;  // only needed for big-endian target
            desc_ty_idx = component_ty_idx;
            if (desc_ty_idx == 0)
              desc_ty_idx = Get_TY (gs_tree_type(exp));
              
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
                    ("WGEN_Expand_Expr: field id for bit-field exceeds limit"));
 
            WN* ldid_ofst = NULL;
            if (xtra_BE_ofst != 0)
              ldid_ofst = WGEN_Compose_Offset(component_offset,
                                               WN_Intconst(Pointer_Mtype, xtra_BE_ofst));
            else
              ldid_ofst = component_offset;
	    wn1 = WGEN_CreateLdid(OPR_LDID, rtype,
			        is_bit_field ? MTYPE_BS : desc,
			        ldid_ofst, 
				ret_st,
				(field_id != 0 && component_ty_idx != 0) ?
				Get_TY (gs_tree_type(exp)) : ty_idx,
				field_id);
	  }

          wn  = WN_CreateComma (OPR_COMMA, WN_rtype (wn1), MTYPE_V,
                                wn0, wn1);
#ifdef TARG_X8664
          if (WN_operator(wn1) == OPR_LDID) {
            TYPE_ID rtype = WN_rtype(wn1); // type in register
            TYPE_ID desc =  WN_desc(wn1);  // type in memory
            // widen the value with proper sign/zero extension
            // such that I4I1LDID will be sign extended and
            // U4U2 will be zero extended
            if (MTYPE_is_integral(rtype) && MTYPE_is_integral(desc) &&
                // bug911 open64.net. Add Safe guard for CVTL.
                (desc != MTYPE_BS) &&
                 MTYPE_byte_size( rtype ) > MTYPE_byte_size( desc )) {
               wn = WN_CreateCvtl(OPR_CVTL, Widen_Mtype(desc), MTYPE_V,
			     MTYPE_size_min(desc), wn);
            }
          }
#endif
        }
      }
      break;

    case GS_UNGE_EXPR: 
    case GS_UNGT_EXPR: 
    case GS_UNLE_EXPR: 
    case GS_UNLT_EXPR: 
    case GS_LTGT_EXPR: 
    case GS_ORDERED_EXPR: 
    case GS_UNORDERED_EXPR: 
    case GS_UNEQ_EXPR:
      {  WN *ikids[2];
	 WN *arg_wn;
	 TY_IDX  arg_ty_idx;
         TYPE_ID arg_mtype;
	 INTRINSIC iopc;
	 BOOL nott = FALSE;

	 arg_wn     = WGEN_Expand_Expr(gs_tree_operand(exp, 0));
	 arg_ty_idx = Get_TY(gs_tree_type(gs_tree_operand(exp, 0)));
	 arg_mtype  = TY_mtype(arg_ty_idx);
	 ikids[0] = WN_CreateParm(arg_mtype, arg_wn, arg_ty_idx, 
				  WN_PARM_BY_VALUE);

	 arg_wn     = WGEN_Expand_Expr(gs_tree_operand(exp, 1));
	 arg_ty_idx = Get_TY(gs_tree_type(gs_tree_operand(exp, 1)));
	 arg_mtype  = TY_mtype(arg_ty_idx);
	 ikids[1] = WN_CreateParm(arg_mtype, arg_wn, arg_ty_idx, 
				  WN_PARM_BY_VALUE);
	 switch (code) {
	 case GS_UNGE_EXPR: iopc = INTRN_ISGREATEREQUAL; break;
	 case GS_UNGT_EXPR: iopc = INTRN_ISGREATER; break;
	 case GS_UNLE_EXPR: iopc = INTRN_ISLESSEQUAL; break;
	 case GS_UNLT_EXPR: iopc = INTRN_ISLESS; break;
	 case GS_LTGT_EXPR: iopc = INTRN_ISLESSGREATER; break;
	 case GS_ORDERED_EXPR: iopc = INTRN_ISORDERED; break;
	 case GS_UNEQ_EXPR:
	 case GS_UNORDERED_EXPR: iopc = INTRN_ISUNORDERED; break;
	 }
	 wn = WN_Create_Intrinsic(OPR_INTRINSIC_OP, Boolean_type, MTYPE_V,
				  iopc, 2, ikids);
	 if (code == GS_UNEQ_EXPR) {
	   WN *eq_wn = WN_Relational(OPR_EQ, arg_mtype,
				     WN_COPY_Tree(WN_kid0(ikids[0])),
				     WN_COPY_Tree(WN_kid0(ikids[1])));
	   wn  = WN_Binary (OPR_CIOR, Boolean_type, wn, eq_wn);
	 }
      }
      break;

    case GS_COMPOUND_EXPR:
      {
#ifdef KEY
	// If we are supposed to put the result in target_wn, then give the
	// result VAR_DECL the same ST as target_wn.
	gs_t opnd1 = gs_tree_operand(exp, 1);
	if (gs_tree_code(opnd1) == GS_VAR_DECL &&
	    target_wn != NULL) {
	  ST *st = DECL_ST(opnd1);
	  if (st == NULL) {
	    // Don't think we would see a LDID target_wn.
	    FmtAssert(WN_operator(target_wn) == OPR_LDA,
		      ("WGEN_Expand_Expr: target_wn not LDA"));
	    set_DECL_ST(opnd1, WN_st(target_wn));
	  } else {
	    FmtAssert(st == WN_st(target_wn),
	       ("WGEN_Expand_Expr: conflicting ST in COMPOUND_EXPR's VAR_DECL"));
	  }
	}
#endif
        wn = WGEN_Expand_Expr (gs_tree_operand (exp, 0), FALSE);
        if (wn && WN_has_side_effects(wn)) {
          wn = WN_CreateEval (wn);
          WGEN_Stmt_Append (wn, Get_Srcpos ());
        }
#ifdef KEY
        // bug 11238: pass on the target
        wn = WGEN_Expand_Expr (gs_tree_operand (exp, 1), need_result,
                               nop_ty_idx, component_ty_idx, component_offset,
                               field_id, is_bit_field, is_aggr_init_via_ctor, target_wn); 
#else
        wn = WGEN_Expand_Expr (gs_tree_operand (exp, 1), need_result);
#endif
      }
      break;

    case GS_NON_LVALUE_EXPR:
      {
#ifdef KEY
	// Pass field_id for bug 10339.
        wn = WGEN_Expand_Expr (gs_tree_operand (exp, 0), need_result,
			       nop_ty_idx, component_ty_idx, component_offset,
			       field_id);
#else
        wn = WGEN_Expand_Expr (gs_tree_operand (exp, 0));
#endif
      }
      break;

    case GS_SAVE_EXPR:
      {
	DevWarn ("Encountered SAVE_EXPR at line %d", lineno);
        wn = WGEN_Save_Expr (exp, need_result, nop_ty_idx,
			    component_ty_idx, component_offset, field_id);
      }
      break;

    case GS_ERROR_MARK:
      // This is not necessarily an error:  return a constant 0.
      wn = WN_Intconst(MTYPE_I4, 0);
      break;

    case GS_LOOP_EXPR:
      {
        DevWarn ("Encountered LOOP_EXPR at line %d\n", lineno);
        LABEL_IDX saved_loop_expr_exit_label = loop_expr_exit_label;
        loop_expr_exit_label = 0;
        gs_t body = gs_loop_expr_body(exp);
        WN *loop_test = WN_Intconst (Boolean_type, 1);
        WN *loop_body = WN_CreateBlock ();
        if (body) {
          WGEN_Stmt_Push (loop_body, wgen_stmk_while_body, Get_Srcpos());
          wn = WGEN_Expand_Expr (body);
          WGEN_Stmt_Pop (wgen_stmk_while_body);
        }
        WN *loop_stmt = WN_CreateWhileDo (loop_test, loop_body);
        WGEN_Stmt_Append (loop_stmt, Get_Srcpos());
        if (loop_expr_exit_label)
          WGEN_Stmt_Append (WN_CreateLabel ((ST_IDX) 0, loop_expr_exit_label, 0, NULL),
                           Get_Srcpos ());
        loop_expr_exit_label = saved_loop_expr_exit_label;
      }
      break;

    case GS_EXIT_EXPR:
      {
        DevWarn ("Encountered EXIT_EXPR at line %d\n", lineno);
	WN *test = WGEN_Expand_Expr (gs_tree_operand(exp, 0));
        New_LABEL (CURRENT_SYMTAB, loop_expr_exit_label);
        WN *stmt = WN_CreateTruebr (loop_expr_exit_label, test);
        WGEN_Stmt_Append (stmt, Get_Srcpos ());
      }
      break;

    case GS_VA_ARG_EXPR:
      {
#ifdef TARG_X8664
	if( TARGET_64BIT ){
	  gs_t kid0 = gs_tree_operand(exp, 0);
	  WN *ap_wn;
	  ap_wn = WGEN_Expand_Expr(kid0);
	  if (WN_rtype(ap_wn) == MTYPE_M) {
	    if (OPCODE_is_leaf(WN_opcode(ap_wn)))
	      ap_wn = WN_Lda(Pointer_Mtype, WN_offset(ap_wn), WN_st(ap_wn), 0);
	    else {
	      Is_True(OPCODE_is_load(WN_opcode(ap_wn)),
		      ("WGEN_Expand_Expr: unexpected VA_ARG_EXPR argument"));
	      if ( WN_offset(ap_wn) == 0 )
		ap_wn = WN_kid0(ap_wn);
	      else
		ap_wn = WN_Add(Pointer_Mtype, WN_kid0(ap_wn), WN_Intconst(Pointer_Mtype, WN_offset(ap_wn)));
	    }
	  }
	  TY_IDX ty_idx = Get_TY (gs_tree_type(exp));
	  TYPE_ID mtype = Fix_TY_mtype(ty_idx);

	  if (mtype != MTYPE_F10 && mtype != MTYPE_M && !MTYPE_is_complex(mtype)) {
	    wn = WGEN_x8664_va_arg(ap_wn, MTYPE_float(mtype), ty_idx, FALSE);
	    wn = WN_CreateIload(OPR_ILOAD, Widen_Mtype (mtype), mtype, 0,
				ty_idx, Make_Pointer_Type(ty_idx), wn);
	  }
	  else if (MTYPE_is_complex(mtype)) {
        Is_True((mtype == MTYPE_C4 || mtype == MTYPE_C8 || mtype == MTYPE_C10 ||
            mtype == MTYPE_C16 || mtype == MTYPE_CQ),
            ("WGEN_Expand_Expr: unexpected complex type"));
	    wn = WGEN_x8664_va_arg(ap_wn, MTYPE_float(mtype), ty_idx, FALSE);
	    wn = WN_CreateIload(OPR_ILOAD, mtype, mtype, 0, ty_idx,
				Make_Pointer_Type(ty_idx), wn);
	  }
	  else {
	    enum X86_64_PARM_CLASS classes[MAX_CLASSES];
	    INT n = Classify_Aggregate(ty_idx, classes);
            // handle X87 X87UP and COMPLEX_X87 cases
            if (n != 0 && (classes[0] == X86_64_X87_CLASS ||
                           classes[0] == X86_64_X87UP_CLASS ||
                           classes[0] == X86_64_COMPLEX_X87_CLASS)) {
               // x87, x87up and complex_x87 are passed in memory
               n = 0;
            }
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
	      WGEN_Stmt_Append (wn, Get_Srcpos ());
	      /* load pointer to overflow_arg_area */
	      wn = WN_Iload(Pointer_Mtype, 8, MTYPE_To_TY(Pointer_Mtype),
			    WN_CopyNode(ap_wn));
	      /* adjust with the amount just incremented */
	      wn1 = WN_Intconst(MTYPE_I8, -delta);
	      wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
	    }
	    else if (n == 1) {
	      wn = WGEN_x8664_va_arg(ap_wn, classes[0] == X86_64_SSE_CLASS,
				    ty_idx, FALSE);
	    }
	    else if (n > 1) { /* must be == 2 */
	      if (classes[0] == classes[1]) {
		if (classes[0] == X86_64_INTEGER_CLASS)
		  wn = WGEN_x8664_va_arg(ap_wn, classes[0] == X86_64_SSE_CLASS, 
					ty_idx, TRUE/*twice*/);
		else wn = WGEN_x8664_va_arg_2_float(ap_wn, ty_idx);
	      }
	      else {
		wn = WGEN_x8664_va_arg_2_mixed(ap_wn, 
					      classes[0] == X86_64_SSE_CLASS,
					      classes[1] == X86_64_SSE_CLASS, ty_idx);
	      }
	    }
	    
	    if( mtype == MTYPE_F10 )
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
	INT64 align;
	INT64 rounded_size;
	gs_t type = gs_tree_type (exp);
	TY_IDX ty_idx = Get_TY (type);
	TYPE_ID mtype = TY_mtype (ty_idx);

        /* Compute the rounded size of the type.  */
	align = TARGET_64BIT ? 8 : 4;
	rounded_size = ((TY_size(Get_TY(type)) + align - 1) / align) * align;
#if defined(TARG_MIPS) && !defined(TARG_SL) // bug 12916: further align to double-word boundary
	rounded_size = ((rounded_size + 7) / 8) * 8;
#endif

	/* Get AP.  */
	WN *ap_load = WGEN_Expand_Expr (gs_tree_operand (exp, 0));
        TY_IDX     ap_ty_idx = Get_TY (gs_tree_type (gs_tree_operand (exp, 0)));
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

	wn = WN_COPY_Tree(ap_load);
	
#ifdef TARG_IA64
	// Adjust the address on IA64 when ty_size is large than align
	INT64 ty_align =  gs_type_align(type) / BITS_PER_UNIT;
	ty_align = ((ty_align + align - 1) / align) * align;
	
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
			WN_Intconst (Pointer_Mtype, rounded_size));
#else			 
	if (Target_Byte_Sex == BIG_ENDIAN) {
	  INT64 adj;
	  adj = gs_n(gs_tree_int_cst_low(gs_type_size(type))) / BITS_PER_UNIT;
	  if (rounded_size > align)
	    adj = rounded_size;
	  wn = WN_Binary (OPR_ADD, Pointer_Mtype, WN_COPY_Tree (ap_load),
			  WN_Intconst (Pointer_Mtype, 3));
	  wn = WN_Binary (OPR_BAND, Pointer_Mtype, wn,
			  WN_Intconst (Pointer_Mtype, -8));
	  wn = WN_Binary (OPR_ADD, Pointer_Mtype, wn,
			  WN_Intconst (Pointer_Mtype, rounded_size));
	} else {
          /* Compute new value for AP.  */
          wn = WN_Binary (OPR_ADD, Pointer_Mtype, WN_COPY_Tree (ap_load),
          WN_Intconst (Pointer_Mtype, rounded_size));
        }
#ifdef TARG_X8664 // bug 12118: pad since under -m32, vector types are 8-byte aligned
	if (MTYPE_is_vector(mtype) && ! TARGET_64BIT) {
	  wn = WN_Add(Pointer_Mtype, wn, WN_Intconst(Pointer_Mtype, 7));
	  wn = WN_Div(Pointer_Mtype, wn, WN_Intconst(Pointer_Mtype, 8));
	  wn = WN_Mpy(Pointer_Mtype, wn, WN_Intconst(Pointer_Mtype, 8));
	}
#endif
#ifdef TARG_MIPS // bug 12945: pad since long doubles are 16-byte aligned
	if (mtype == MTYPE_FQ) {
	  wn = WN_Add(Pointer_Mtype, wn, WN_Intconst(Pointer_Mtype, 15));
	  wn = WN_Div(Pointer_Mtype, wn, WN_Intconst(Pointer_Mtype, 16));
	  wn = WN_Mpy(Pointer_Mtype, wn, WN_Intconst(Pointer_Mtype, 16));
	}
#endif
#endif
	if (ap_st)
	  wn = WN_Stid (Pointer_Mtype, ap_offset, ap_st, ap_ty_idx, wn);
        else {
          wn = WN_CreateIstore (OPR_ISTORE, MTYPE_V, Pointer_Mtype, ap_offset,
                                ap_addr_ty, wn, ap_addr, ap_field_id);
        }
        WGEN_Stmt_Append (wn, Get_Srcpos ());
#ifdef TARG_IA64
        wn = WN_CreateIload (OPR_ILOAD, Widen_Mtype (mtype), mtype, -rounded_size,
			     ty_idx, Make_Pointer_Type (ty_idx, FALSE),
     			     ap_load);
#else
	wn = WN_CreateIload (OPR_ILOAD, Widen_Mtype (mtype), mtype, -rounded_size,
			     ap_ty_idx, Make_Pointer_Type (ap_ty_idx, FALSE),
			     ap_load);
#endif
	
#ifdef KEY
	if (Target_Byte_Sex != Host_Byte_Sex)
          wn = WN_CreateIload (OPR_ILOAD, Widen_Mtype (mtype), mtype, 
			  ((MTYPE_size_min(mtype)==32)?4:0)-rounded_size, 
			  ty_idx, Make_Pointer_Type(ty_idx, FALSE), 
			  ap_load);
	else
#endif
        wn = WN_CreateIload (OPR_ILOAD, Widen_Mtype (mtype), mtype, -rounded_size,
			     ty_idx, Make_Pointer_Type(ty_idx, FALSE), 
			     ap_load);
      }
      break;

    case GS_LABEL_DECL:
      {
        DevWarn ("taking address of a label at line %d", lineno);
        LABEL_IDX label_idx = WGEN_Get_LABEL (arg0, FALSE);
        wn = WN_LdaLabel (Pointer_Mtype, label_idx);
        Set_LABEL_addr_saved (label_idx);
        Set_PU_no_inline (Get_Current_PU ());
      }
      break;

#ifdef KEY
    case GS_LABEL_EXPR:
      {
        // We assume the type of LABEL_EXPR alwayse be void and operand(0) is LABEL_DECL
	// Here will declare a new label
	// Only apply to this case:
	// int foo(int x) { if(x) { L1; } }
	gs_t type = gs_tree_type(exp);
	Is_True ( gs_code(type) == GS_VOID_TYPE, ("Bad expression type for LABEL_EXPR"));
	gs_t label =  gs_tree_operand(exp, 0);
	FmtAssert ( gs_code(label) == GS_LABEL_DECL, ("Bad operand 0 code for LABEL_EXPR"));

	LABEL_IDX label_idx = WGEN_Get_LABEL (label, TRUE);
	WN* wn1 = WN_CreateLabel ((ST_IDX) 0, label_idx, 0, NULL);
	WGEN_Stmt_Append (wn1, Get_Srcpos());
	Set_LABEL_addr_saved (label_idx);
	Set_PU_no_inline (Get_Current_PU ());
      }
      break;
#endif
      
#ifdef KEY
    case GS_EXC_PTR_EXPR:
    {
      if (emit_exceptions)
      {
	ST_IDX exc_ptr_st = Get_exception_pointer_symbol ();
      	wn = WN_Ldid (Pointer_Mtype, 0, exc_ptr_st, Get_TY(gs_tree_type(exp)));
      }
      else
      {
        // bug 12500: dummy exc_ptr_expr
        if (!Dummy_Exc_Ptr_Expr)
          Dummy_Exc_Ptr_Expr = Gen_Temp_Symbol (Get_TY(gs_tree_type(exp)),
                                                "__dummy_exc_ptr");
      	wn = WN_Ldid (Pointer_Mtype, 0, Dummy_Exc_Ptr_Expr, Get_TY(gs_tree_type(exp)));
      }
    }
      break;

    case GS_CLEANUP_STMT:
      DevWarn ("CLEANUP_STMT not implemented: at line %d\n", lineno);
      // TODO:  Return a dummy constant 0 for now.
      wn = WN_Intconst(MTYPE_I4, 0);
      break;

    case GS_MUST_NOT_THROW_EXPR:
      // Call terminate if this expr throws
      must_not_throw = TRUE;
      wn = WGEN_Expand_Expr (gs_tree_operand (exp,0));
      must_not_throw = FALSE;
      break;

   case GS_VECTOR_CST:
     {
       ST * init_st = Gen_Temp_Symbol (Get_TY(gs_tree_type(exp)), "__vec_cst");
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

   // Support this when needed.
   case GS_EH_FILTER_EXPR:
     DevWarn ("EH_FILTER_EXPR not yet implemented");
     break;

   case GS_OBJ_TYPE_REF:
     // We may be able to utilize the other fields.
     wn = WGEN_Expand_Expr (gs_obj_type_ref_expr (exp), need_result);
     break;

   case GS_TRY_FINALLY_EXPR:
     // The second operand is a cleanup to be executed on any exit from
     // evaluation of first operand.
     Register_Cleanup (exp);
     WGEN_Expand_Expr (gs_tree_operand (exp,0), need_result);
     Unregister_Cleanup ();
     WGEN_Expand_Expr (gs_tree_operand (exp,1), need_result);
     break;

    case GS_FILTER_EXPR:
      // TODO: Implement.  Return dummy 0 for now.
      DevWarn ("NYI: FILTER_EXPR");
      wn = WN_Intconst(MTYPE_I4, 0);
      break;

    case GS_RESX_EXPR:
      // TODO: Implement.  Return dummy 0 for now.
      DevWarn ("NYI: RESX_EXPR");
      wn = WN_Intconst(MTYPE_I4, 0);
      break;
#endif /* KEY */

    case GS_ASM_EXPR:
    case GS_GOTO_EXPR:
      WGEN_Expand_Stmt(exp);
      break;

    case GS_STATEMENT_LIST: {
	gs_t stmt_list = gs_statement_list_elts(exp);
#ifdef FE_GNU_4_2_0
        for (int i=0; i < gs_length(stmt_list); i++)
          WGEN_Expand_Stmt(gs_index(stmt_list, i), target_wn);
#else
	gs_t list;
	for (list = stmt_list; gs_code(list) != EMPTY; 
	     list = gs_operand(list, 1))
	  WGEN_Expand_Stmt(gs_operand(list, 0), target_wn);
#endif
      }
      break;

    case GS_RETURN_EXPR:
      WGEN_Expand_Stmt(exp);
      break;

    case GS_SWITCH_STMT:
      WGEN_Expand_Stmt(exp, target_wn);
      break;

#ifdef FE_GNU_4_2_0
    case GS_SWITCH_EXPR:
    case GS_CASE_LABEL_EXPR:

    // OpenMP TREE nodes
    case GS_OMP_ATOMIC:
    case GS_OMP_CRITICAL:
    case GS_OMP_FOR:
    case GS_OMP_MASTER:
    case GS_OMP_ORDERED:
    case GS_OMP_PARALLEL:
    case GS_OMP_SECTIONS:
    case GS_OMP_SINGLE:
      WGEN_Expand_Stmt(exp);
      break;
#endif

    case GS_BREAK_STMT:
    case GS_CONTINUE_STMT:
    case GS_WHILE_STMT:
    case GS_DO_STMT:
    case GS_FOR_STMT:
      WGEN_Expand_Stmt(exp); // bug 10857
      break;

#ifdef KEY
    case GS_VIEW_CONVERT_EXPR: {
	wn = WGEN_Expand_Expr (gs_tree_operand (exp, 0));
	ty_idx = Get_TY (gs_tree_type(exp));
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
	}
#ifdef TARG_X8664
	else if (MTYPE_is_vector(mtyp) && MTYPE_is_vector(WN_rtype(wn)) &&
		 MTYPE_is_mmx_vector(mtyp) == MTYPE_is_mmx_vector(WN_rtype(wn))) {
	  // bug 11797: CVT between vector types not needed
	}
#endif
	else
	  wn = WN_Cvt(WN_rtype(wn), mtyp, wn);
      }
      break;

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

    case GS_FREQ_HINT_STMT:
    case GS_ZDL_STMT:
      WGEN_Expand_Pragma(exp);
      break;
      
    default:
       FmtAssert(FALSE,
	 ("WGEN_Expand_Expr: not yet implemented %s",
	  gs_code_name(code)));
      break;
  } //end switch code

  if (need_result)
    FmtAssert (wn != 0 || code == GS_CALL_EXPR || code == GS_BIND_EXPR ||
               code == GS_STMT_EXPR     ||
               code == GS_EXPR_STMT     ||	// KEY
               code == GS_COMPOUND_EXPR ||
               code == GS_INDIRECT_REF  ||
               code == GS_COMPONENT_REF ||
               code == GS_LOOP_EXPR     ||
               code == GS_NOP_EXPR      ||
	       code == GS_THROW_EXPR    ||
	       code == GS_AGGR_INIT_EXPR ||
	       code == GS_STATEMENT_LIST ||
	       code == GS_CLEANUP_POINT_EXPR ||
               ((code == GS_COND_EXPR) && 
	        (TY_mtype(ty_idx) == MTYPE_V || TY_mtype(ty_idx) == MTYPE_M)),
	       ("WGEN_Expand_Expr: NULL WHIRL tree for %s",
		gs_code_name(code)));

  return wn;
}


// Like WGEN_One_Stmt but don't reuse label indexes already allocated so far.
// This is necessary because the cleanup represented by the EXP tree can be
// expanded multiple times, and each expansion needs its own set of labels.
void
WGEN_One_Stmt_Cleanup (gs_t exp)
{
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


void WGEN_One_Stmt (gs_t exp, WN* target_wn)
{
  WN *wn;
  wn = WGEN_Expand_Expr_With_Sequence_Point (exp, MTYPE_V, target_wn);
  if (wn) {
    for (;;) {
      if (WN_operator (wn) == OPR_COMMA) {
	WN *crwn = wn;
	if (WN_operator (WN_kid1 (wn)) == OPR_LDID                 &&
	    WN_st (WN_kid1 (wn)) == Return_Val_Preg                &&
	    (WN_operator (WN_last (WN_kid0 (wn))) == OPR_CALL   ||
	     WN_operator (WN_last (WN_kid0 (wn))) == OPR_ICALL)) {
	  WN_set_rtype (WN_last (WN_kid0 (wn)), MTYPE_V);
	  WGEN_Stmt_Append (WN_kid0 (wn), Get_Srcpos ());
	  WN_Delete (crwn);
	  break;
	}
	else {
	  WGEN_Stmt_Append (WN_kid0 (wn), Get_Srcpos ());
	  wn = WN_kid1 (wn);
	  WN_Delete (crwn);
	}
      }
      else {
	if (WN_has_side_effects (wn)) {
	  wn = WN_CreateEval (wn);
	  WGEN_Stmt_Append (wn, Get_Srcpos ());
	}
	break;
      }
    }
  }
}


void WGEN_Expr_Init()
{
}

char *
WGEN_Tree_Node_Name (gs_t exp)
{
  return gs_code_name(gs_tree_code (exp));
}

// g++ uses a record to hold a ptr-to-member-function.  Return TRUE iff EXP is
// a CALL_EXPR that returns a ptr-to-member-function and the ABI requires that
// such a record be returned in memory.
//
// Invoke WGEN_Expand_Ptr_To_Member_Func_Call_Expr to expand such calls.  The
// routine creates a temp record for the ptr-to-member-function and invokes
// WGEN_Expand_Expr to expand the return value there.
static bool
WGEN_Call_Returns_Ptr_To_Member_Func (gs_t exp)
{
  gs_t t = gs_tree_type(exp);
  FmtAssert(t != NULL,
            ("WGEN_Call_Returns_Ptr_To_Member_Func: tree type is NULL"));
  TY_IDX exp_ty_idx = Get_TY(t);
  if (gs_tree_code(exp) == GS_CALL_EXPR &&
      gs_type_ptrmemfunc_p(gs_tree_type(exp)) &&
      TY_return_in_mem(exp_ty_idx)) {
    return TRUE;
  }
  return FALSE;
}

// See comment for WGEN_Call_Returns_Ptr_To_Member_Func.
static WN*
WGEN_Expand_Ptr_To_Member_Func_Call_Expr (gs_t exp, TY_IDX nop_ty_idx,
					 TYPE_ID rtype, TYPE_ID desc,
					 WN* offset, UINT field_id)
{
  TY_IDX exp_ty_idx = Get_TY(gs_tree_type(exp));
  WN *wn;
  ST *st = New_ST (CURRENT_SYMTAB);

  ST_Init(st, Save_Str("__ptr_to_mem_func"), CLASS_VAR, SCLASS_AUTO,
	  EXPORT_LOCAL, exp_ty_idx);
  WN *target_wn = WN_Lda(Pointer_Mtype, ST_ofst(st), st);
  WGEN_Expand_Expr(exp, TRUE, nop_ty_idx, exp_ty_idx, 0, 0, FALSE, FALSE,
		  target_wn);
  wn = WGEN_CreateLdid(OPR_LDID, rtype, desc, offset,
		     st, exp_ty_idx, field_id);
  return wn;
}
