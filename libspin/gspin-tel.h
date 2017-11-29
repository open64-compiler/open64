/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
  Copyright (C) 2007. PathScale, LLC.  All rights reserved.
 */

/*
  Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.

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
 */

#ifndef __GSPIN_TEL_H__
#define __GSPIN_TEL_H__

/* This file contains our emulation of macros and functions from GCC's tree.[ch] */

extern gs_t gs_program;	/* The root program node when translating from gspin. */

extern gs_t gs_build_int_cst(gs_long_long_t n);
extern gs_t gs_build_decl(gs_code_t code, gs_t node2);
extern gs_t gs_build_if_stmt(gs_t k0, gs_t k1, gs_t k2);
extern gs_t gs_build_pointer_type(gs_t node);
extern gs_t gs_build_target_expr(gs_t k0, gs_t k1, gs_t k2, gs_t k3);
extern gs_t gs_c_common_truthvalue_conversion(gs_t node);
extern gs_t gs_strip_nops(gs_t node);
#ifdef FE_GNU_4_2_0
extern gs_t gs_build_2(gs_tree_code_class_t code_class,
                       gs_code_t code, gs_t k0, gs_t k1);
#endif


/*  Define kid positions */

/* --------------------------------------------------------------------------- */
/*   Program-wide info (gs_code GS_PROGRAM) */
/* --------------------------------------------------------------------------- */

#define GS_CC1_COMMAND_LINE_ARGS 0
#define GS_GLOBAL_TREES_LIST     1
#define GS_INTEGER_TYPES_LIST    2
#define GS_PROGRAM_DECLARATIONS  3
#define GS_GXX_EMITTED_DECLS     4
#define GS_GXX_EMITTED_ASMS      5
#define GS_PROGRAM_FLAGS	 6
/* ---- begin GS_PROGRAM_FLAGS definition ---- */
#  define GS_FLAG_ERRNO_MATH        1
#  define GS_PRAGMA_IMPLEMENTATION  2
#  define GS_PRAGMA_INTERFACE       3
/* ---- end GS_PROGRAM_FLAGS definition ---- */
#define GS_WEAK_DECLS		 7
#define GS_PROGRAM_LAST          8

/* --------------------------------------------------------------------------- */
/*   Tree nodes */
/* --------------------------------------------------------------------------- */

/* ------------------------------------- */
/*   Fields common to all tree codes. */
/* ------------------------------------- */
#define GS_TREE_CODE_CLASS		0
#define GS_TREE_TYPE			1
#define GS_TREE_CHAIN			2
#define GS_FLAGS			3	/* Assume 64-bit IB_BIT_VECTOR. */
/* ---- begin GS_FLAGS definition ---- */
/* common flags: */
#  define GS_TREE_SIDE_EFFECTS		0
#  define GS_TYPE_READONLY		1
#  define GS_TREE_READONLY		GS_TYPE_READONLY
#  define GS_TREE_CONSTANT		2
#  define GS_TYPE_SIZES_GIMPLIFIED	GS_TREE_CONSTANT
#  define GS_TREE_INVARIANT		3
#  define GS_TREE_ADDRESSABLE		4
#  define GS_TREE_THIS_VOLATILE		5
#  define GS_TREE_ASM_WRITTEN		6
#  define GS_TREE_USED			7
#  define GS_TREE_NOTHROW		8
#  define GS_TREE_PUBLIC		9
#  define GS_ASM_VOLATILE_P		GS_TREE_PUBLIC
#  define GS_TREE_PRIVATE		10
#  define GS_TREE_PROTECTED		11
#  define GS_TREE_STATIC		12
#  define GS_DWARF_ACCESS_FLAG_0	13
#  define GS_DWARF_ACCESS_FLAG_1	14
#  define GS_TREE_LANG_FLAG_0		15
#  define GS_TREE_LANG_FLAG_1		16
#  define GS_TREE_LANG_FLAG_2		17
#  define GS_TREE_LANG_FLAG_3		18
#  define GS_TREE_LANG_FLAG_4		19
#  define GS_TREE_LANG_FLAG_5		20
#  define GS_TREE_LANG_FLAG_6		21
#  define GS_TREE_NOT_EMITTED_BY_GXX	22

 
/* flags specific to GS_TCC_DECLARATION: */
#  define GS_DECL_UNSIGNED		23
#  define GS_DECL_IGNORED_P		24
#  define GS_DECL_ABSTRACT		25 
#  define GS_DECL_IN_SYSTEM_HEADER	26
#  define GS_DECL_COMMON		27
#  define GS_DECL_EXTERNAL		28
#  define GS_DECL_WEAK			29
#  define GS_DECL_REGISTER		30
#  define GS_DECL_NONLOCAL		31
#  define GS_TYPE_DECL_SUPPRESS_DEBUG	32
#  define GS_DECL_NEEDED		GS_TYPE_DECL_SUPPRESS_DEBUG
#  define GS_DECL_INLINE		33
#  define GS_DECL_DECLARED_INLINE_P	34
#  define GS_DECL_BUILT_IN		35
#  define GS_DECL_NO_STATIC_CHAIN	36
#  define GS_DECL_PACKED		37
#  define GS_DECL_REACHABLE		GS_DECL_PACKED
#  define GS_DECL_BIT_FIELD		38
#  define GS_DECL_NONADDRESSABLE_P	39
#  define GS_DECL_EMITTED_BY_GXX	40
#  define GS_DECL_IN_TEXT_SECTION	41
#  define GS_DECL_THREAD_LOCAL		42
#  define GS_DECL_TRANSPARENT_UNION	43
#  define GS_DECL_VIRTUAL_P		44
#  define GS_DECL_DEFER_OUTPUT		45
#  define GS_DECL_PRESERVE_P		46
#  define GS_DECL_LANG_FLAG_0		47
#  define GS_DECL_LANG_FLAG_1		48
#  define GS_DECL_LANG_FLAG_2		49
#  define GS_DECL_LANG_FLAG_3		50
#  define GS_DECL_LANG_FLAG_4		51
#  define GS_DECL_LANG_FLAG_5		52
#  define GS_DECL_LANG_FLAG_6		53
#  define GS_DECL_LANG_FLAG_7		54
#  define GS_DECL_USER_ALIGN		55
#  define GS_DECL_OFFSET_ALIGN		56
#  define GS_DECL_POINTER_ALIAS_SET	57
#  define GS_DECL_THUNK_P		58
#  define GS_DECL_ASSEMBLER_NAME_SET_P	59
#  define GS_DECL_ARTIFICIAL		60
#  define GS_DECL_LANG_SPECIFIC		61
#  define GS_DECL_THREADPRIVATE         62 /* RECYCLE */
#ifdef FE_GNU_4_2_0
#  define GS_C_DECL_THREADPRIVATE_P     GS_DECL_LANG_FLAG_3
#endif

/* flags specific to GS_TCC_TYPE: */
#  define GS_TYPE_UNSIGNED		23
#  define GS_TYPE_NO_FORCE_BLK		24
#  define GS_TYPE_IS_SIZETYPE			GS_TYPE_NO_FORCE_BLK
#  define GS_TYPE_RETURNS_STACK_DEPRESSED	GS_TYPE_NO_FORCE_BLK
#  define GS_TYPE_STRING_FLAG		25
#  define GS_TYPE_NEEDS_CONSTRUCTING	26
#  define GS_TYPE_TRANSPARENT_UNION	27
#  define GS_TYPE_NONALIASED_COMPONENT	GS_TYPE_TRANSPARENT_UNION
#  define GS_TYPE_PACKED		28
#  define GS_TYPE_RESTRICT		29 
#  define GS_TYPE_LANG_FLAG_0		30
#  define GS_TYPE_LANG_FLAG_1		31
#  define GS_TYPE_LANG_FLAG_2		32
#  define GS_TYPE_LANG_FLAG_3		33
#  define GS_TYPE_LANG_FLAG_4		34
#  define GS_TYPE_LANG_FLAG_5		35
#  define GS_TYPE_LANG_FLAG_6		36
#  define GS_TYPE_VOLATILE		37
#  define GS_TYPE_LANG_SPECIFIC		38
#  define GS_POINTER_TYPE_P             39
#  define GS_AGGREGATE_VALUE_P          40
#  define GS_TYPE_BIG_ENDIAN            41
#  define GS_TYPE_LITTLE_ENDIAN         42
#  define GS_TYPE_EXPLICIT_ENDIAN       43

/* flags specific to GS_TCC_EXPRESSION, GS_TCC_COMPARISON, GS_TCC_UNARY, */
/* GS_TCC_BINARY, GS_TCC_REFERENCE, GS_TCC_STATEMENT: */
#  define GS_BIT_FIELD_REF_UNSIGNED	23
#  define GS_EXPR_HAS_LOCATION		24
/*** 25 *** RECYCLE ***/
#  define GS_EMIT_TARGET_EXPR_CLEANUP	26

/* flags specific to GS_TCC_CONSTANT, GS_TCC_EXCEPTIONAL: */
#  define GS_TREE_CONSTANT_OVERFLOW	23
#  define GS_TREE_OVERFLOW		24
#  define GS_REAL_VALUE_ISINF		25
#  define GS_REAL_VALUE_ISNAN		26
#  define GS_BINFO_VIRTUAL_P		GS_TREE_CONSTANT_OVERFLOW /* TREE_BINFO */
#  define GS_TREE_SYMBOL_REFERENCED	GS_TREE_STATIC
/* ---- end GS_FLAGS definition ---- */

/* ------------------------------------- */
/*   ==== begin GS_TCC_DECLARATION fields */
/* ------------------------------------- */
#define GS_DECL_NAME			4
#define GS_DECL_MODE			5
#define GS_DECL_SOURCE_FILE		6
#define GS_DECL_SOURCE_LINE		7
#define GS_DECL_SIZE			8
#define GS_DECL_SIZE_UNIT		9
#define GS_DECL_BUILT_IN_CLASS		10
#define GS_DECL_FUNCTION_CODE		11
#define GS_DECL_FIELD_OFFSET		12
#define GS_DECL_FIELD_BIT_OFFSET	13
#define GS_DECL_CONTEXT			14
#define GS_DECL_ATTRIBUTES		15
#define GS_DECL_ABSTRACT_ORIGIN		16
#define GS_DECL_ARGUMENTS		17
#define GS_DECL_ANON_UNION_ELEMS	GS_DECL_ARGUMENTS
#define GS_DECL_INITIAL			18
#define GS_DECL_ARG_TYPE		19
#define GS_DECL_ARG_TYPE_AS_WRITTEN	20
#define GS_DECL_SAVED_TREE		21
#define GS_DECL_VALUE_EXPR		GS_DECL_SAVED_TREE
#define GS_DECL_RESULT			22
#define GS_DECL_ORIGINAL_TYPE		GS_DECL_RESULT
#define GS_LABEL_DECL_UID		23
#define GS_DECL_UID			24
#define GS_DECL_ALIGN_UNIT		25
#define GS_DECL_ASSEMBLER_NAME		26
#define GS_DECL_ALIAS_TARGET		27
#define GS_DECL_ASMREG			28
/* ---- C++ ---- */
#define GS_CP_DECL_FLAGS		29
/* ---- begin GS_CP_DECL_FLAGS definition ---- */
#  define GS_DECL_COMDAT			0
#  define GS_DECL_GLOBAL_CTOR_P			1
#  define GS_DECL_GLOBAL_DTOR_P			2
#  define GS_DECL_ONE_ONLY			3
#  define GS_DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P	4
#  define GS_DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P	5
#  define GS_DECL_FUNCTION_MEMBER_P		6
#  define GS_DECL_USES_TEMPLATE_PARMS		7
#  define GS_DECL_COPY_CONSTRUCTOR_P 		8
#  define GS_DECL_IMPLICIT_INSTANTIATION 	9
#  define GS_DECL_NAMESPACE_SCOPE_P 		10
#ifdef FE_GNU_4_2_0
#  define GS_CP_DECL_THREADPRIVATE_P		11
#else
/* RECYCLE 11 */
#endif
#  define GS_DECL_COMPLETE_CONSTRUCTOR_P        12
#  define GS_DECL_REALLY_EXTERN			13
#  define GS_DECL_USE_TEMPLATE			14
#  define GS_DECL_TEMPLATE_INSTANTIATED		15
#  define GS_DECL_TEMPLATE_SPECIALIZATION	16
#  define GS_DECL_PURE_VIRTUAL_P		17
#  define GS_DECL_THIS_THUNK_P			18
#  define GS_DECL_EXTERN_C_P			19
#ifdef FE_GNU_4_2_0
#  define GS_DECL_CONSTRUCTOR_P			GS_CP_DECL_THREADPRIVATE_P
#  define GS_DECL_COMPLETE_DESTRUCTOR_P		20
#  define GS_DECL_HAS_IN_CHARGE_PARM_P		21
#  define GS_DECL_HAS_VTT_PARM_P		22
#  define GS_DECL_ASSIGNMENT_OPERATOR_P		23
#endif
/* ---- end GS_CP_DECL_FLAGS definition ---- */
#define GS_DECL_TEMPLATE_INFO		30
#define GS_DECL_SECTION_NAME		31
#define GS_CP_NAMESPACE_DECLS		32
#define GS_CP_DECL_CONTEXT		33
#define GS_DECL_VINDEX			34
#define GS_MOST_GENERAL_TEMPLATE	35
#define GS_DECL_NAMESPACE_ALIAS	        36
#define GS_THUNK_TARGET	                37
#define GS_DECL_TI_TEMPLATE		38
#define GS_THUNK_FIXED_OFFSET		39
#define GS_THUNK_VIRTUAL_OFFSET		40
#define GS_DECL_NAMED_RETURN_OBJECT	41
#define GS_DECL_FLAG2                   42
/* ---- begin GS_DECL_FLAG2 definition ---- */
#  define GS_DECL_TLS_MODEL             0    /* 3 bits */
#  define GS_DECL_TLS_MODEL_BITS        3    
#  define GS_DECL_VISIBILITY_SPECIFIED  3    /* 1 bit */
#  define GS_DECL_VISIBILITY            4    /* 2 bits */
#  define GS_DECL_VISIBILITY_BITS       2    
#  define GS_DECL_FLAG2_LAST            6    /* next available bits, change it when */
                                             /* you add more flags */
/* ---- end GS_DECL_FLAG2 definition ---- */

/*   ==== end GS_TCC_DECLARATION fields */


/* ------------------------------------- */
/*   ==== begin GS_TCC_TYPE fields */
/* ------------------------------------- */
#define GS_TYPE_NAME			4
#define GS_TYPE_MODE			5
#define GS_TYPE_SIZE			6
#define GS_TYPE_SIZE_UNIT		7
#define GS_TYPE_USER_ALIGN		8
#define GS_TYPE_ALIGN			9
#define GS_TYPE_ALIAS_SET		10
#define GS_TYPE_ATTRIBUTES		11
#define GS_TYPE_PRECISION		12
#define GS_TYPE_VECTOR_SUBPARTS		GS_TYPE_PRECISION
#define GS_TYPE_MIN_VALUE		13
#define GS_TYPE_MAX_VALUE		14
#define GS_TYPE_VALUES			15
#define GS_TYPE_DOMAIN			GS_TYPE_VALUES
#define GS_TYPE_FIELDS			GS_TYPE_VALUES
#define GS_TYPE_DEBUG_REPRESENTATION_TYPE  GS_TYPE_VALUES
#define GS_TYPE_METHOD_BASETYPE		GS_TYPE_MAX_VALUE
#define GS_TYPE_OFFSET_BASETYPE		GS_TYPE_MAX_VALUE
#define GS_TYPE_ARG_TYPES		16
#define GS_TYPE_CONTEXT			17
#define GS_TYPE_POINTER_TO		18
#define GS_TYPE_REFERENCE_TO		19
#define GS_TYPE_NEXT_PTR_TO		GS_TYPE_MIN_VALUE
/* ---- C++ ---- */
#define GS_TYPE_BINFO			20
#define GS_TYPE_MAIN_VARIANT		21
#define GS_CP_TYPE_FLAGS		22
/* ---- begin GS_CP_TYPE_FLAGS definition ---- */
#  define GS_TYPE_PTRMEMFUNC_P		0
#  define GS_TYPE_PTRMEM_P		1
#  define GS_CLASSTYPE_INTERFACE_ONLY	2
#  define GS_IS_EMPTY_CLASS	        3
#  define GS_CLASS_TYPE_P               4
#  define GS_ANON_UNION_TYPE_P		5
#  define GS_CLASSTYPE_TEMPLATE_SPECIALIZATION	6
#  define GS_TYPE_USES_TEMPLATE_PARMS	7
#ifdef FE_GNU_4_2_0
#  define GS_CLASSTYPE_NON_POD_P	8
#  define GS_TYPE_HAS_DEFAULT_CONSTRUCTOR  9
#  define GS_TYPE_HAS_IMPLICIT_COPY_CONSTRUCTOR  10
#endif
/* ---- end GS_CP_TYPE_FLAGS definition ---- */
#define GS_TYPE_VFIELD                  GS_TYPE_MIN_VALUE
#define GS_TYPE_METHODS                 GS_TYPE_MAX_VALUE
#define GS_CLASSTYPE_AS_BASE            23
#define GS_CLASSTYPE_TYPEINFO_VAR       24
#define GS_TYPEINFO_DECL                25
#define GS_CLASSTYPE_COPY_CONSTRUCTOR	26
/*   ==== end GS_TCC_TYPE fields */


/* ------------------------------------- */
/*   ==== begin */
/*   GS_TCC_EXPRESSION */
/*   GS_TCC_COMPARISON */
/*   GS_TCC_UNARY */
/*   GS_TCC_BINARY */
/*   GS_TCC_REFERENCE */
/*   GS_TCC_STATEMENT fields */
/* ------------------------------------- */

#define GS_ARITY			4
#define GS_EXPR_FILENAME		5
#define GS_EXPR_LINENO			6
/* Slots 7-11 are reserved for operands of expression nodes. */
/* 4 seems to be the maximum arity (based on checking across all .def files). */
/* ************ FE_GNU_4_2_0 *************** */
/* For GNU 4.2 work, 5 slots 7-12 (excluding 12) are reserved for operands. */
/* See below. */
#define GS_TREE_OPERAND_ZERO		7

#define GS_ASM_STRING			7
#define GS_ASM_OUTPUTS			8
#define GS_ASM_INPUTS			9
#define GS_ASM_CLOBBERS			10 

#define GS_BIND_EXPR_VARS		7
#define GS_BIND_EXPR_BODY		8
#define GS_BIND_EXPR_BLOCK		9

#define GS_CASE_LOW			7
#define GS_CASE_HIGH			8
#define GS_CASE_LABEL			9

#ifdef FE_GNU_4_2_0
#define GS_CONSTRUCTOR_LENGTH		7
#define GS_CONSTRUCTOR_ELTS_INDEX	8
#define GS_CONSTRUCTOR_ELTS_VALUE	9
#else
#define GS_CONSTRUCTOR_ELTS		7
#endif

#define GS_DECL_EXPR_DECL		7

#define GS_DO_COND                      7
#define GS_DO_BODY                      8

#define GS_EH_SPEC_STMTS                7
#define GS_EH_SPEC_RAISES               8

#define GS_FOR_INIT_STMT		7
#define GS_FOR_COND			8
#define GS_FOR_EXPR			9
#define GS_FOR_BODY			10

#define GS_EXPR_STMT_EXPR		7

#define GS_HANDLER_PARMS                7
#define GS_HANDLER_BODY                 8

#define GS_CLEANUP_BODY                 7
#define GS_CLEANUP_EXPR                 8

#define GS_CATCH_TYPES                  7
#define GS_CATCH_BODY                   8

#define GS_IF_COND                      7
#define GS_THEN_CLAUSE                  8
#define GS_ELSE_CLAUSE                  9

#define GS_LABEL_EXPR_LABEL		7

#define GS_LOOP_EXPR_BODY		7

#define GS_TRY_STMTS                    7
#define GS_TRY_HANDLERS                 8

#define GS_STMT_EXPR_STMT		7

#define GS_WHILE_COND                   7
#define GS_WHILE_BODY                   8

#define GS_OBJ_TYPE_REF_EXPR            7
#define GS_OBJ_TYPE_REF_OBJECT          8
#define GS_OBJ_TYPE_REF_TOKEN           9

#ifdef FE_GNU_4_2_0
#define GS_OMP_PARALLEL_BODY            7
#define GS_OMP_PARALLEL_CLAUSES         8

#define GS_OMP_CRITICAL_BODY            7
#define GS_OMP_CRITICAL_NAME            8

#define GS_OMP_SECTIONS_BODY            7
#define GS_OMP_SECTIONS_CLAUSES         8

#define GS_OMP_SECTION_BODY             7

#define GS_OMP_SINGLE_BODY              7
#define GS_OMP_SINGLE_CLAUSES           8

/* GNU definition has 6 kids, but we probably won't need OMP_FOR_PRE_BODY. */
#define GS_OMP_FOR_BODY                 7
#define GS_OMP_FOR_CLAUSES              8
#define GS_OMP_FOR_INIT                 9
#define GS_OMP_FOR_COND                 10
#define GS_OMP_FOR_INCR                 11

#define GS_OMP_MASTER_BODY              7

#define GS_OMP_ORDERED_BODY             7
#endif

/* ---- C++ ---- */
#ifdef FE_GNU_4_2_0
#define GS_CP_EXPR_FLAGS                12
#else
#define GS_CP_EXPR_FLAGS		11
#endif

/* ---- begin GS_CP_EXPR_FLAGS definition ---- */
#  define GS_STMT_IS_FULL_EXPR_P	0
#  define GS_AGGR_INIT_VIA_CTOR_P	1
#  define GS_CLEANUP_EH_ONLY            2
/* ---- end GS_CP_EXPR_FLAGS definition ---- */
/*   ==== end */
/*   GS_TCC_EXPRESSION */
/*   GS_TCC_COMPARISON */
/*   GS_TCC_UNARY */
/*   GS_TCC_BINARY */
/*   GS_TCC_REFERENCE */
/*   GS_TCC_STATEMENT fields */


/* ------------------------------------- */
/*   ==== begin GS_TCC_CONSTANT fields */
/* ------------------------------------- */

/* GS_INTEGER_CST */
#define GS_TREE_INT_CST_LOW		4
#define GS_TREE_INT_CST_HIGH		5

/* GS_REAL_CST */
#define GS_TREE_REAL_CST_F		4
#define GS_TREE_REAL_CST_D		5
#define GS_TREE_REAL_CST_LD		6

/* GS_VECTOR_CST */
#define GS_TREE_VECTOR_CST_ELTS		4

/* GS_COMPLEX_CST */
#define GS_TREE_REALPART		4
#define GS_TREE_IMAGPART		5

/* GS_PTRMEM_CST */
#define GS_EXPANDED_PTRMEM_CST		4  /* result of cplus_expand_constant's */
					   /* expansion of the PTRMEM_CST */
/* GS_STRING_CST */
#define GS_TREE_STRING_POINTER		4
#define GS_TREE_STRING_LENGTH		5
/*   ==== end GS_TCC_CONSTANT fields */

/* ------------------------------------- */
/*   ==== begin GS_TCC_EXCEPTIONAL fields */
/* ------------------------------------- */

/* GS_IDENTIFIER_NODE */
#define GS_IDENTIFIER_POINTER		4

/* GS_TREE_LIST */
#define GS_TREE_PURPOSE			4
#define GS_TREE_VALUE			5

/* GS_TREE_VEC */
#define GS_TREE_VEC_LENGTH		4
#define GS_TREE_VEC_ELT			5

/* GS_STATEMENT_LIST */
#define GS_STATEMENT_LIST_ELTS		4

/* GS_TREE_BINFO */
#define GS_BINFO_TYPE			4
#define GS_BINFO_BASE_BINFOS		5	/* gspin list */
#define GS_BINFO_VPTR_FIELD		6

/* GS_BLOCK */
#define GS_BLOCK_VARS			4
#define GS_BLOCK_SUPERCONTEXT		5
#define GS_BLOCK_SUBBLOCKS		6
#define GS_BLOCK_CHAIN			7
#define GS_BLOCK_ABSTRACT_ORIGIN	8

/* GS_TEMPLATE_PARM_INDEX */
#define GS_TEMPLATE_PARM_IDX            4
#define GS_TEMPLATE_PARM_LEVEL          5
#define GS_TEMPLATE_PARM_DESCENDANTS    6
#define GS_TEMPLATE_PARM_ORIG_LEVEL     7
#define GS_TEMPLATE_PARM_DECL           8

/* GS_BASELINK */
#define GS_BASELINK_BINFO               4
#define GS_BASELINK_FUNCTIONS           5
#define GS_BASELINK_ACCESS_BINFO        6
#define GS_BASELINK_OPTYPE              7

/* GS_OVERLOAD */
#define GS_OVL_FUNCTION                 4
#define GS_OVL_CHAIN                    GS_TREE_CHAIN
#define GS_OVL_CURRENT                  5
#define GS_OVL_NEXT                     6

#ifdef FE_GNU_4_2_0
/* GS_OMP_CLAUSE */
#define GS_OMP_CLAUSE_CODE              4

#define GS_OMP_CLAUSE_DECL              5
#define GS_OMP_CLAUSE_DEFAULT_KIND      GS_OMP_CLAUSE_DECL
#define GS_OMP_CLAUSE_IF_EXPR           GS_OMP_CLAUSE_DECL
#define GS_OMP_CLAUSE_NUM_THREADS_EXPR  GS_OMP_CLAUSE_DECL
#define GS_OMP_CLAUSE_REDUCTION_CODE    6

#define GS_OMP_CLAUSE_SCHEDULE_KIND        5
#define GS_OMP_CLAUSE_SCHEDULE_CHUNK_EXPR  6
#endif
/*   ==== end GS_TCC_EXCEPTIONAL fields */


extern gs_tree_code_class_t gs_tree_code_class (gs_t t);

#define GS_LOOKUP(name, arg)       \
static inline gs_t name (gs_t t) { \
  GS_ASSERT (t != (gs_t) NULL,     \
             "Got null node");     \
  return gs_operand (t, arg);      \
}

#define GS_LOOKUP_FLAG(operand, name, flag)      \
static inline gs_bool_t name (gs_t t) {          \
  GS_ASSERT (t != (gs_t) NULL, "Got null node"); \
  return gs_bv (gs_operand (t, operand), flag);  \
}

#define GS_UPDATE_FLAG(operand, name, flag)        \
static inline void name (gs_t t, gs_bool_t val) {  \
  GS_ASSERT (t != (gs_t) NULL, "Got null node");   \
  _gs_bv (gs_operand (t, operand), flag, val);     \
}

#define GS_LOOKUP_BITS(operand, name, flag, bits)        \
static inline unsigned long name (gs_t t) {              \
  GS_ASSERT (t != (gs_t) NULL, "Got null node");         \
  return gs_bitsv (gs_operand (t, operand), flag, bits); \
}

#define GS_UPDATE_BITS(operand, name, flag, bits)        \
static inline void name (gs_t t, unsigned long val) {    \
  GS_ASSERT (t != (gs_t) NULL, "Got null node");         \
  _gs_bitsv (gs_operand (t, operand), flag, bits, val);  \
}

GS_LOOKUP (gs_cc1_command_line_args, GS_CC1_COMMAND_LINE_ARGS)
GS_LOOKUP (gs_global_trees_list, GS_GLOBAL_TREES_LIST)
GS_LOOKUP (gs_integer_types_list, GS_INTEGER_TYPES_LIST)
GS_LOOKUP (gs_program_declarations, GS_PROGRAM_DECLARATIONS)
GS_LOOKUP (gs_gxx_emitted_decls, GS_GXX_EMITTED_DECLS)
GS_LOOKUP (gs_gxx_emitted_asms, GS_GXX_EMITTED_ASMS)
GS_LOOKUP (gs_weak_decls, GS_WEAK_DECLS)

GS_LOOKUP_FLAG (GS_PROGRAM_FLAGS, gs_flag_errno_math, GS_FLAG_ERRNO_MATH)
GS_LOOKUP_FLAG (GS_PROGRAM_FLAGS, gs_pragma_implementation, GS_PRAGMA_IMPLEMENTATION)
GS_LOOKUP_FLAG (GS_PROGRAM_FLAGS, gs_pragma_interface, GS_PRAGMA_INTERFACE)

GS_LOOKUP (gs_tree_type, GS_TREE_TYPE)

/* C Flags+ { */

GS_LOOKUP_FLAG (GS_FLAGS, gs_tree_side_effects, GS_TREE_SIDE_EFFECTS)

GS_LOOKUP_FLAG (GS_FLAGS, gs_type_readonly, GS_TYPE_READONLY)
GS_LOOKUP_FLAG (GS_FLAGS, gs_tree_readonly, GS_TREE_READONLY)

GS_LOOKUP_FLAG (GS_FLAGS, gs_tree_constant, GS_TREE_CONSTANT)
GS_LOOKUP_FLAG (GS_FLAGS, gs_type_sizes_gimplified, GS_TYPE_SIZES_GIMPLIFIED)

GS_LOOKUP_FLAG (GS_FLAGS, gs_tree_invariant, GS_TREE_INVARIANT)
GS_LOOKUP_FLAG (GS_FLAGS, gs_tree_addressable, GS_TREE_ADDRESSABLE)
GS_LOOKUP_FLAG (GS_FLAGS, gs_tree_this_volatile, GS_TREE_THIS_VOLATILE)
GS_LOOKUP_FLAG (GS_FLAGS, gs_tree_asm_written, GS_TREE_ASM_WRITTEN)
GS_LOOKUP_FLAG (GS_FLAGS, gs_tree_used, GS_TREE_USED)
GS_LOOKUP_FLAG (GS_FLAGS, gs_tree_nothrow, GS_TREE_NOTHROW)
GS_LOOKUP_FLAG (GS_FLAGS, gs_tree_public, GS_TREE_PUBLIC)
GS_LOOKUP_FLAG (GS_FLAGS, gs_asm_volatile_p, GS_ASM_VOLATILE_P)
GS_LOOKUP_FLAG (GS_FLAGS, gs_tree_private, GS_TREE_PRIVATE)
#ifdef FE_GNU_4_2_0
GS_LOOKUP_FLAG (GS_FLAGS, gs_omp_parallel_combined, GS_TREE_PRIVATE)
#endif
GS_LOOKUP_FLAG (GS_FLAGS, gs_tree_protected, GS_TREE_PROTECTED)
GS_LOOKUP_FLAG (GS_FLAGS, gs_tree_static, GS_TREE_STATIC)
GS_LOOKUP_FLAG (GS_FLAGS, gs_dwarf_access_flag_0, GS_DWARF_ACCESS_FLAG_0)
GS_LOOKUP_FLAG (GS_FLAGS, gs_dwarf_access_flag_1, GS_DWARF_ACCESS_FLAG_1)
GS_LOOKUP_FLAG (GS_FLAGS, gs_tree_lang_flag_0, GS_TREE_LANG_FLAG_0)
GS_LOOKUP_FLAG (GS_FLAGS, gs_tree_lang_flag_1, GS_TREE_LANG_FLAG_1)
GS_LOOKUP_FLAG (GS_FLAGS, gs_tree_lang_flag_2, GS_TREE_LANG_FLAG_2)
GS_LOOKUP_FLAG (GS_FLAGS, gs_tree_lang_flag_3, GS_TREE_LANG_FLAG_3)
GS_LOOKUP_FLAG (GS_FLAGS, gs_tree_lang_flag_4, GS_TREE_LANG_FLAG_4)
GS_LOOKUP_FLAG (GS_FLAGS, gs_tree_lang_flag_5, GS_TREE_LANG_FLAG_5)
GS_LOOKUP_FLAG (GS_FLAGS, gs_tree_lang_flag_6, GS_TREE_LANG_FLAG_6)
GS_LOOKUP_FLAG (GS_FLAGS, gs_tree_not_emitted_by_gxx,GS_TREE_NOT_EMITTED_BY_GXX)
GS_UPDATE_FLAG (GS_FLAGS, gs_set_tree_not_emitted_by_gxx, GS_TREE_NOT_EMITTED_BY_GXX)

/* GS_TCC_DECLARATION : */
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_unsigned, GS_DECL_UNSIGNED)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_ignored_p, GS_DECL_IGNORED_P)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_abstract, GS_DECL_ABSTRACT)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_in_system_header, GS_DECL_IN_SYSTEM_HEADER)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_common, GS_DECL_COMMON)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_external, GS_DECL_EXTERNAL)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_weak, GS_DECL_WEAK)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_register, GS_DECL_REGISTER)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_nonlocal, GS_DECL_NONLOCAL)
GS_LOOKUP_FLAG (GS_FLAGS, gs_type_decl_suppress_debug, GS_TYPE_DECL_SUPPRESS_DEBUG)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_inline, GS_DECL_INLINE)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_declared_inline_p, GS_DECL_DECLARED_INLINE_P)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_built_in, GS_DECL_BUILT_IN)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_no_static_chain, GS_DECL_NO_STATIC_CHAIN)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_packed, GS_DECL_PACKED)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_bit_field, GS_DECL_BIT_FIELD)
GS_UPDATE_FLAG (GS_FLAGS, gs_set_decl_bit_field, GS_DECL_BIT_FIELD)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_nonaddressable_p, GS_DECL_NONADDRESSABLE_P)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_emitted_by_gxx, GS_DECL_EMITTED_BY_GXX)
GS_UPDATE_FLAG (GS_FLAGS, gs_set_decl_emitted_by_gxx, GS_DECL_EMITTED_BY_GXX)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_in_text_section, GS_DECL_IN_TEXT_SECTION)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_thread_local, GS_DECL_THREAD_LOCAL)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_transparent_union, GS_DECL_TRANSPARENT_UNION)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_virtual_p, GS_DECL_VIRTUAL_P)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_defer_output, GS_DECL_DEFER_OUTPUT)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_preserve_p, GS_DECL_PRESERVE_P)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_lang_flag_0, GS_DECL_LANG_FLAG_0)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_lang_flag_1, GS_DECL_LANG_FLAG_1)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_lang_flag_2, GS_DECL_LANG_FLAG_2)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_lang_flag_3, GS_DECL_LANG_FLAG_3)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_lang_flag_4, GS_DECL_LANG_FLAG_4)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_lang_flag_5, GS_DECL_LANG_FLAG_5)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_lang_flag_6, GS_DECL_LANG_FLAG_6)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_lang_flag_7, GS_DECL_LANG_FLAG_7)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_user_align, GS_DECL_USER_ALIGN)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_offset_align, GS_DECL_OFFSET_ALIGN)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_pointer_alias_set, GS_DECL_POINTER_ALIAS_SET)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_thunk_p, GS_DECL_THUNK_P)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_assembler_name_set_p, GS_DECL_ASSEMBLER_NAME_SET_P)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_artificial, GS_DECL_ARTIFICIAL)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_lang_specific, GS_DECL_LANG_SPECIFIC)
/* recycle begin */
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_threadprivate, GS_DECL_THREADPRIVATE)
GS_UPDATE_FLAG (GS_FLAGS, gs_set_decl_threadprivate, GS_DECL_THREADPRIVATE)
/* recycle end */

#ifdef FE_GNU_4_2_0
GS_LOOKUP_FLAG (GS_FLAGS, gs_c_decl_threadprivate_p, GS_C_DECL_THREADPRIVATE_P)
GS_UPDATE_FLAG (GS_FLAGS, gs_set_c_decl_threadprivate_p, GS_C_DECL_THREADPRIVATE_P)
#endif

GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_needed, GS_DECL_NEEDED)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_reachable, GS_DECL_REACHABLE)

/* GS_TCC_TYPE: */

GS_LOOKUP_FLAG (GS_FLAGS, gs_type_unsigned, GS_TYPE_UNSIGNED)
GS_UPDATE_FLAG (GS_FLAGS, gs_set_type_unsigned, GS_TYPE_UNSIGNED)

GS_LOOKUP_FLAG (GS_FLAGS, gs_type_no_force_blk, GS_TYPE_NO_FORCE_BLK)
GS_LOOKUP_FLAG (GS_FLAGS, gs_type_is_sizetype, GS_TYPE_IS_SIZETYPE)
GS_LOOKUP_FLAG (GS_FLAGS, gs_type_returns_stack_depressed, GS_TYPE_RETURNS_STACK_DEPRESSED)

GS_LOOKUP_FLAG (GS_FLAGS, gs_type_string_flag, GS_TYPE_STRING_FLAG)
GS_LOOKUP_FLAG (GS_FLAGS, gs_type_needs_constructing, GS_TYPE_NEEDS_CONSTRUCTING)

GS_LOOKUP_FLAG (GS_FLAGS, gs_type_transparent_union, GS_TYPE_TRANSPARENT_UNION)
GS_LOOKUP_FLAG (GS_FLAGS, gs_type_nonaliased_attribute, GS_TYPE_NONALIASED_COMPONENT)

GS_LOOKUP_FLAG (GS_FLAGS, gs_type_packed, GS_TYPE_PACKED)
GS_LOOKUP_FLAG (GS_FLAGS, gs_type_restrict, GS_TYPE_RESTRICT)
GS_LOOKUP_FLAG (GS_FLAGS, gs_type_lang_flag_0, GS_TYPE_LANG_FLAG_0)
GS_LOOKUP_FLAG (GS_FLAGS, gs_type_lang_flag_1, GS_TYPE_LANG_FLAG_1)
GS_LOOKUP_FLAG (GS_FLAGS, gs_type_lang_flag_2, GS_TYPE_LANG_FLAG_2)
GS_LOOKUP_FLAG (GS_FLAGS, gs_type_lang_flag_3, GS_TYPE_LANG_FLAG_3)
GS_LOOKUP_FLAG (GS_FLAGS, gs_type_lang_flag_4, GS_TYPE_LANG_FLAG_4)
GS_LOOKUP_FLAG (GS_FLAGS, gs_type_lang_flag_5, GS_TYPE_LANG_FLAG_5)
GS_LOOKUP_FLAG (GS_FLAGS, gs_type_lang_flag_6, GS_TYPE_LANG_FLAG_6)

GS_LOOKUP_FLAG (GS_FLAGS, gs_type_volatile, GS_TYPE_VOLATILE)
GS_LOOKUP_FLAG (GS_FLAGS, gs_type_lang_specific, GS_TYPE_LANG_SPECIFIC)
GS_LOOKUP_FLAG (GS_FLAGS, gs_pointer_type_p, GS_POINTER_TYPE_P)
GS_LOOKUP_FLAG (GS_FLAGS, gs_aggregate_value_p, GS_AGGREGATE_VALUE_P)
GS_LOOKUP_FLAG (GS_FLAGS, gs_type_big_endian, GS_TYPE_BIG_ENDIAN)
GS_LOOKUP_FLAG (GS_FLAGS, gs_type_little_endian, GS_TYPE_LITTLE_ENDIAN)
GS_LOOKUP_FLAG (GS_FLAGS, gs_type_explicit_endian, GS_TYPE_EXPLICIT_ENDIAN)
  /* gs_type_ref_can_alias_all reuses the GS_TREE_STATIC flag */
GS_LOOKUP_FLAG (GS_FLAGS, gs_type_ref_can_alias_all, GS_TREE_STATIC)
GS_UPDATE_FLAG (GS_FLAGS, gs_set_type_ref_can_alias_all, GS_TREE_STATIC)

/* GS_TCC_EXPRESSION: */
/* GS_TCC_COMPARISON: */
/* GS_TCC_UNARY: */
/* GS_TCC_BINARY: */
/* GS_TCC_REFERENCE: */
/* GS_TCC_STATEMENT: */

GS_LOOKUP_FLAG (GS_FLAGS, gs_bit_field_ref_unsigned, GS_BIT_FIELD_REF_UNSIGNED)
GS_LOOKUP_FLAG (GS_FLAGS, gs_expr_has_location, GS_EXPR_HAS_LOCATION)
GS_LOOKUP_FLAG (GS_FLAGS, gs_emit_target_expr_cleanup, GS_EMIT_TARGET_EXPR_CLEANUP)

/* gs_expr_has_location(t) must never be directly called when the nature of
   T is not known. gs_tree_has_location(t) should be called for any type
   of TREE node. (bug 12563) */
static inline gs_bool_t gs_tree_has_location (gs_t t) {
  if (gs_tree_code_class(t) >= GS_TCC_REFERENCE &&
      gs_tree_code_class(t) <= GS_TCC_EXPRESSION)
    return gs_expr_has_location (t);
  else
    return gs_false;
}

/* GS_TCC_CONSTANT/GS_TCC_EXCEPTIONAL: */

GS_LOOKUP_FLAG (GS_FLAGS, gs_tree_constant_overflow, GS_TREE_CONSTANT_OVERFLOW)
GS_LOOKUP_FLAG (GS_FLAGS, gs_tree_overflow, GS_TREE_OVERFLOW)

GS_LOOKUP_FLAG (GS_FLAGS, gs_real_value_isinf, GS_REAL_VALUE_ISINF)
GS_LOOKUP_FLAG (GS_FLAGS, gs_real_value_isnan, GS_REAL_VALUE_ISNAN)
  /* gs_tree_symbol_referenced reuses the GS_TREE_STATIC flag */
GS_LOOKUP_FLAG (GS_FLAGS, gs_tree_symbol_referenced, GS_TREE_SYMBOL_REFERENCED)
GS_UPDATE_FLAG (GS_FLAGS, gs_set_tree_symbol_referenced, GS_TREE_SYMBOL_REFERENCED)
/* } C Flags- */

GS_LOOKUP (gs_decl_name, GS_DECL_NAME)
static inline gs_int_t gs_label_decl_uid (gs_t t) {
  return gs_n (gs_operand (t, GS_LABEL_DECL_UID));
}
static inline gs_int_t gs_decl_uid (gs_t t) {
  return gs_n (gs_operand (t, GS_DECL_UID));
}
GS_LOOKUP (gs_decl_size, GS_DECL_SIZE)
GS_LOOKUP (gs_decl_size_unit, GS_DECL_SIZE_UNIT)
static inline unsigned int gs_decl_built_in_class (gs_t t) {
  return gs_b(gs_operand (t, GS_DECL_BUILT_IN_CLASS));
}
static inline unsigned int gs_decl_function_code (gs_t t) {
  return gs_hword(gs_operand (t, GS_DECL_FUNCTION_CODE));
}
GS_LOOKUP (gs_decl_field_offset, GS_DECL_FIELD_OFFSET)
GS_LOOKUP (gs_decl_field_bit_offset, GS_DECL_FIELD_BIT_OFFSET)
GS_LOOKUP (gs_decl_context, GS_DECL_CONTEXT)
GS_LOOKUP (gs_decl_attributes, GS_DECL_ATTRIBUTES)
GS_LOOKUP (gs_decl_abstract_origin, GS_DECL_ABSTRACT_ORIGIN)
GS_LOOKUP (gs_decl_arguments, GS_DECL_ARGUMENTS)
GS_LOOKUP (gs_decl_anon_union_elems, GS_DECL_ANON_UNION_ELEMS)
GS_LOOKUP (gs_decl_initial, GS_DECL_INITIAL)
static inline void gs_set_decl_initial(gs_t t, gs_t val) {
  gs_set_operand(t, GS_DECL_INITIAL, val);
}
GS_LOOKUP (gs_decl_saved_tree, GS_DECL_SAVED_TREE)
GS_LOOKUP (gs_decl_value_expr, GS_DECL_VALUE_EXPR)
GS_LOOKUP (gs_decl_result, GS_DECL_RESULT)
GS_LOOKUP (gs_decl_original_type, GS_DECL_ORIGINAL_TYPE)
GS_LOOKUP (gs_decl_arg_type, GS_DECL_ARG_TYPE)
GS_LOOKUP (gs_decl_arg_type_as_written, GS_DECL_ARG_TYPE_AS_WRITTEN)
GS_LOOKUP (gs_tree_chain, GS_TREE_CHAIN)
GS_LOOKUP (gs_type_name, GS_TYPE_NAME)
static inline gs_string_t gs_decl_mode (gs_t t) {
  return gs_s (gs_operand (t, GS_DECL_MODE));
}

GS_LOOKUP_BITS (GS_DECL_FLAG2, gs_decl_tls_model, GS_DECL_TLS_MODEL, GS_DECL_TLS_MODEL_BITS)
GS_UPDATE_BITS (GS_DECL_FLAG2, gs_set_decl_tls_model, GS_DECL_TLS_MODEL, GS_DECL_TLS_MODEL_BITS)
GS_LOOKUP_FLAG (GS_DECL_FLAG2, gs_decl_visibility_specified, GS_DECL_VISIBILITY_SPECIFIED)
GS_UPDATE_FLAG (GS_DECL_FLAG2, gs_set_decl_visibility_specified, GS_DECL_VISIBILITY_SPECIFIED)
GS_LOOKUP_BITS (GS_DECL_FLAG2, gs_decl_visibility, GS_DECL_VISIBILITY, GS_DECL_VISIBILITY_BITS)
GS_UPDATE_BITS (GS_DECL_FLAG2, gs_set_decl_visibility, GS_DECL_VISIBILITY, GS_DECL_VISIBILITY_BITS)

static inline gs_string_t gs_decl_source_file (gs_t t) {
  if (gs_operand (t, GS_DECL_SOURCE_FILE) != NULL)
  return gs_s (gs_operand (t, GS_DECL_SOURCE_FILE));
  else
    return NULL;
}
static inline gs_int_t gs_decl_source_line (gs_t t) {
  if (gs_operand (t, GS_DECL_SOURCE_LINE) != NULL)
  return gs_n (gs_operand (t, GS_DECL_SOURCE_LINE));
  else
    return -1;
}
GS_LOOKUP (gs_type_size, GS_TYPE_SIZE)
GS_LOOKUP (gs_type_size_unit, GS_TYPE_SIZE_UNIT)
GS_LOOKUP (gs_type_user_align, GS_TYPE_USER_ALIGN)
static inline gs_string_t gs_type_mode (gs_t t) {
  return gs_s (gs_operand (t, GS_TYPE_MODE));
}
static inline gs_int_t gs_type_align (gs_t t) {
  return gs_n (gs_operand (t, GS_TYPE_ALIGN));
}
GS_LOOKUP (gs_type_alias_set, GS_TYPE_ALIAS_SET)
GS_LOOKUP (gs_type_attributes, GS_TYPE_ATTRIBUTES)
GS_LOOKUP (gs_type_precision, GS_TYPE_PRECISION)
static inline gs_int_t gs_type_type_precision (gs_t t) {
   return gs_n (gs_operand (t, GS_TYPE_PRECISION));
}
GS_LOOKUP (gs_type_min_value, GS_TYPE_MIN_VALUE)
GS_LOOKUP (gs_type_max_value, GS_TYPE_MAX_VALUE)

GS_LOOKUP (gs_type_values, GS_TYPE_VALUES)
GS_LOOKUP (gs_type_domain, GS_TYPE_DOMAIN)
static inline gs_int_t gs_type_vector_subparts (gs_t t) {
  return gs_n (gs_operand (t, GS_TYPE_VECTOR_SUBPARTS));
}
GS_LOOKUP (gs_type_fields, GS_TYPE_FIELDS)
GS_LOOKUP (gs_type_debug_representation_type, GS_TYPE_DEBUG_REPRESENTATION_TYPE)
GS_LOOKUP (gs_type_method_basetype, GS_TYPE_METHOD_BASETYPE)
GS_LOOKUP (gs_type_arg_types, GS_TYPE_ARG_TYPES)
GS_LOOKUP (gs_type_offset_basetype, GS_TYPE_OFFSET_BASETYPE)
GS_LOOKUP (gs_type_context, GS_TYPE_CONTEXT)
GS_LOOKUP (gs_type_pointer_to, GS_TYPE_POINTER_TO)
GS_LOOKUP (gs_type_reference_to, GS_TYPE_REFERENCE_TO)
GS_LOOKUP (gs_type_next_ptr_to, GS_TYPE_NEXT_PTR_TO)
static inline void gs_set_type_next_ptr_to(gs_t t, gs_t val) {
  gs_set_operand(t, GS_TYPE_NEXT_PTR_TO, val);
}
static inline gs_int_t gs_tree_code_length (gs_t t) {
  return gs_n (gs_operand (t, GS_ARITY));
}
static inline gs_t gs_tree_operand (gs_t t, gs_count_t index) {
  return gs_operand (t, GS_TREE_OPERAND_ZERO + index);
}
GS_LOOKUP (gs_bind_expr_vars, GS_BIND_EXPR_VARS)
GS_LOOKUP (gs_bind_expr_body, GS_BIND_EXPR_BODY)
GS_LOOKUP (gs_bind_expr_block, GS_BIND_EXPR_BLOCK)

GS_LOOKUP (gs_case_low, GS_CASE_LOW)
GS_LOOKUP (gs_case_high, GS_CASE_HIGH)
GS_LOOKUP (gs_case_label, GS_CASE_LABEL)

#ifdef FE_GNU_4_2_0
static inline gs_int_t gs_constructor_length (gs_t t) {
  return gs_n (gs_operand (t, GS_CONSTRUCTOR_LENGTH));
}
static inline gs_t gs_constructor_elts_index (gs_t t, gs_count_t index) {
  return gs_index (gs_operand (t, GS_CONSTRUCTOR_ELTS_INDEX), index);
}
static inline gs_t gs_constructor_elts_value (gs_t t, gs_count_t index) {
  return gs_index (gs_operand (t, GS_CONSTRUCTOR_ELTS_VALUE), index);
}
static inline void gs_constructor_elts_set_value (gs_t t, gs_count_t index,
                                                  gs_t value) {
  gs_t list = gs_operand (t, GS_CONSTRUCTOR_ELTS_VALUE);
  gs_set_index (list, index, value);
}
#else
GS_LOOKUP (gs_constructor_elts, GS_CONSTRUCTOR_ELTS)
#endif
GS_LOOKUP (gs_decl_expr_decl, GS_DECL_EXPR_DECL)

static inline gs_string_t gs_expr_filename (gs_t t) {
  if (gs_operand (t, GS_EXPR_FILENAME) != NULL)
    return gs_s (gs_operand (t, GS_EXPR_FILENAME));
  else
    return NULL;
}

static inline gs_int_t gs_expr_lineno (gs_t t) {
  if (gs_operand (t, GS_EXPR_LINENO))
    return gs_n (gs_operand (t, GS_EXPR_LINENO));
  else return -1;
}

static inline gs_string_t gs_identifier_pointer (gs_t t) {
  return gs_s (gs_operand (t, GS_IDENTIFIER_POINTER));
}
GS_LOOKUP (gs_tree_int_cst_low, GS_TREE_INT_CST_LOW)
GS_LOOKUP (gs_tree_int_cst_high, GS_TREE_INT_CST_HIGH)
static inline gs_long_long_t gs_get_integer_value (gs_t t)
{
  GS_ASSERT (t != (gs_t) NULL, (gs_string_t) "Got null node");
  return (gs_long_long_t) gs_ull (gs_tree_int_cst_low (t));
}

static inline gs_float_t gs_tree_real_cst_f (gs_t t) {
  return gs_f (gs_operand (t, GS_TREE_REAL_CST_F));
}
static inline gs_double_t gs_tree_real_cst_d (gs_t t) {
  return gs_d (gs_operand (t, GS_TREE_REAL_CST_D));
}
static inline gs_long_double_t gs_tree_real_cst_ld (gs_t t) {
  return gs_ld (gs_operand (t, GS_TREE_REAL_CST_LD));
}
GS_LOOKUP (gs_tree_vector_cst_elts, GS_TREE_VECTOR_CST_ELTS)
GS_LOOKUP (gs_tree_realpart, GS_TREE_REALPART)
GS_LOOKUP (gs_tree_imagpart, GS_TREE_IMAGPART)
GS_LOOKUP (gs_expanded_ptrmem_cst, GS_EXPANDED_PTRMEM_CST)
static inline gs_string_t gs_tree_string_pointer (gs_t t) {
  return gs_s (gs_operand (t, GS_TREE_STRING_POINTER));
}
static inline gs_int_t gs_tree_string_length (gs_t t) {
  return gs_n (gs_operand (t, GS_TREE_STRING_LENGTH));
}
GS_LOOKUP (gs_tree_purpose, GS_TREE_PURPOSE)
GS_LOOKUP (gs_tree_value, GS_TREE_VALUE)
static inline void gs_set_tree_value(gs_t t, gs_t val) {
  gs_set_operand(t, GS_TREE_VALUE, val);
}
static inline gs_int_t gs_tree_vec_length (gs_t t) {
  return gs_n (gs_operand (t, GS_TREE_VEC_LENGTH));
}
static inline gs_t gs_tree_vec_elt (gs_t t, gs_count_t index) {
  return gs_index (gs_operand (t, GS_TREE_VEC_ELT), index);
}
GS_LOOKUP (gs_block_vars, GS_BLOCK_VARS)
GS_LOOKUP (gs_block_supercontext, GS_BLOCK_SUPERCONTEXT)
GS_LOOKUP (gs_block_subblocks, GS_BLOCK_SUBBLOCKS)
GS_LOOKUP (gs_block_chain, GS_BLOCK_CHAIN)
GS_LOOKUP (gs_block_abstract_origin, GS_BLOCK_ABSTRACT_ORIGIN)
GS_LOOKUP (gs_statement_list_elts, GS_STATEMENT_LIST_ELTS)

static inline gs_int_t gs_decl_align_unit (gs_t t) {
  return gs_n (gs_operand (t, GS_DECL_ALIGN_UNIT));
}
GS_LOOKUP (gs_decl_assembler_name, GS_DECL_ASSEMBLER_NAME)
GS_LOOKUP (gs_decl_alias_target, GS_DECL_ALIAS_TARGET)
static inline void gs_set_decl_alias_target(gs_t t, gs_t val) {
  gs_set_operand(t, GS_DECL_ALIAS_TARGET, val);
}
static inline gs_int_t gs_decl_asmreg (gs_t t) {
  gs_t asmreg = gs_operand (t, GS_DECL_ASMREG);
  return (asmreg != (gs_t) NULL) ? gs_n(asmreg) : -1;
}

extern gs_string_t gs_flag_name (gs_code_t constructor, gs_int_t attribute, gs_count_t flag, gs_tree_code_class_t tree_code_class);
extern gs_string_t gs_attribute_name (gs_int_t attribute, gs_tree_code_class_t tcc_class, gs_code_t constructor, gs_int_t * omit);

static inline gs_t gs_error_mark_node(void) {
  gs_t t = gs_index(gs_global_trees_list(gs_program), GS_TI_ERROR_MARK);
  GS_ASSERT(t != NULL, "gs_error_mark_node: got NULL node");
  return t;
}
static inline gs_t gs_void_type_node(void) {
  gs_t t = gs_index(gs_global_trees_list(gs_program), GS_TI_VOID_TYPE);
  GS_ASSERT(t != NULL, "gs_void_type_node: got NULL node");
  return t;
}
static inline gs_t gs_ptr_type_node(void) {
  gs_t t = gs_index(gs_global_trees_list(gs_program), GS_TI_PTR_TYPE);
  GS_ASSERT(t != NULL, "gs_ptr_type_node: got NULL node");
  return t;
}
static inline gs_t gs_boolean_type_node(void) {
  gs_t t = gs_index(gs_global_trees_list(gs_program), GS_TI_BOOLEAN_TYPE);
  GS_ASSERT(t != NULL, "gs_boolean_type_node: got NULL node");
  return t;
}
static inline gs_t gs_ptrdiff_type_node(void) {
  gs_t t = gs_index(gs_global_trees_list(gs_program), GS_TI_PTRDIFF_TYPE);
  GS_ASSERT(t != NULL, "gs_ptrdiff_type_node: got NULL node");
  return t;
}
#ifdef FE_GNU_4_2_0
static inline gs_t gs_void_list_node(void) {
  gs_t t = gs_index(gs_global_trees_list(gs_program), GS_TI_VOID_LIST_NODE);
  GS_ASSERT(t != NULL, "gs_void_type_node: got NULL node");
  return t;
}
#endif
static inline gs_t gs_integer_type_node(void) {
  gs_t t = gs_index (gs_integer_types_list(gs_program), GS_ITK_INT);
  GS_ASSERT(t != NULL, "gs_integer_type_node: got NULL node");
  return t;
}

/* C++ Decl Flags+ { */
GS_LOOKUP_FLAG (GS_CP_DECL_FLAGS, gs_decl_comdat, GS_DECL_COMDAT)
GS_LOOKUP_FLAG (GS_CP_DECL_FLAGS, gs_decl_global_ctor_p, GS_DECL_GLOBAL_CTOR_P)
GS_LOOKUP_FLAG (GS_CP_DECL_FLAGS, gs_decl_global_dtor_p, GS_DECL_GLOBAL_DTOR_P)
GS_LOOKUP_FLAG (GS_CP_DECL_FLAGS, gs_decl_one_only, GS_DECL_ONE_ONLY)
GS_LOOKUP_FLAG (GS_CP_DECL_FLAGS, gs_decl_maybe_in_charge_constructor_p, GS_DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P)
GS_LOOKUP_FLAG (GS_CP_DECL_FLAGS, gs_decl_maybe_in_charge_destructor_p, GS_DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P)
GS_LOOKUP_FLAG (GS_CP_DECL_FLAGS, gs_decl_function_member_p, GS_DECL_FUNCTION_MEMBER_P)

static inline gs_bool_t gs_uses_template_parms (gs_t t) {          
  GS_ASSERT (t != (gs_t) NULL, "Got null node"); 
  if (gs_tree_code (t) == GS_FUNCTION_DECL) 
    return gs_bv (gs_operand (t, GS_CP_DECL_FLAGS),
		  GS_DECL_USES_TEMPLATE_PARMS);  
  else if (gs_tree_code (t) == GS_RECORD_TYPE) 
    return gs_bv (gs_operand (t, GS_CP_TYPE_FLAGS),
		  GS_TYPE_USES_TEMPLATE_PARMS);
  GS_ASSERT(gs_false, "gs_uses_template_parms: Incorrect node");
  return gs_false;	/* unreachable */
}

GS_LOOKUP_FLAG (GS_CP_DECL_FLAGS, gs_decl_copy_constructor_p, GS_DECL_COPY_CONSTRUCTOR_P)
GS_LOOKUP_FLAG (GS_CP_DECL_FLAGS, gs_decl_implicit_instantiation, GS_DECL_IMPLICIT_INSTANTIATION)
GS_LOOKUP_FLAG (GS_CP_DECL_FLAGS, gs_decl_namespace_scope_p, GS_DECL_NAMESPACE_SCOPE_P)
#ifdef FE_GNU_4_2_0
GS_LOOKUP_FLAG (GS_CP_DECL_FLAGS, gs_cp_decl_threadprivate_p, GS_CP_DECL_THREADPRIVATE_P)
#endif
GS_LOOKUP_FLAG (GS_CP_DECL_FLAGS, gs_decl_complete_constructor_p, GS_DECL_COMPLETE_CONSTRUCTOR_P)
GS_LOOKUP_FLAG (GS_CP_DECL_FLAGS, gs_decl_really_extern, GS_DECL_REALLY_EXTERN)
GS_LOOKUP_FLAG (GS_CP_DECL_FLAGS, gs_decl_use_template, GS_DECL_USE_TEMPLATE)
GS_LOOKUP_FLAG (GS_CP_DECL_FLAGS, gs_decl_template_instantiated,
		GS_DECL_TEMPLATE_INSTANTIATED)
GS_LOOKUP_FLAG (GS_CP_DECL_FLAGS, gs_decl_template_specialization,
		GS_DECL_TEMPLATE_SPECIALIZATION)
GS_LOOKUP_FLAG (GS_CP_DECL_FLAGS, gs_decl_pure_virtual_p,
		GS_DECL_PURE_VIRTUAL_P)
GS_LOOKUP_FLAG (GS_CP_DECL_FLAGS, gs_decl_this_thunk_p,
		GS_DECL_THIS_THUNK_P)
GS_LOOKUP_FLAG (GS_CP_DECL_FLAGS, gs_decl_extern_c_p,
		GS_DECL_EXTERN_C_P)
#ifdef FE_GNU_4_2_0
GS_LOOKUP_FLAG (GS_CP_DECL_FLAGS, gs_decl_constructor_p,
		GS_DECL_CONSTRUCTOR_P)
GS_LOOKUP_FLAG (GS_CP_DECL_FLAGS, gs_decl_complete_destructor_p,
		GS_DECL_COMPLETE_DESTRUCTOR_P)
static inline gs_bool_t gs_decl_nonstatic_member_function_p (gs_t t) {
  GS_ASSERT (t != (gs_t) NULL, "Got null node");
  return (gs_tree_code (gs_tree_type (t)) == GS_METHOD_TYPE);
}
GS_LOOKUP_FLAG (GS_CP_DECL_FLAGS, gs_decl_has_in_charge_parm_p,
		GS_DECL_HAS_IN_CHARGE_PARM_P)
GS_LOOKUP_FLAG (GS_CP_DECL_FLAGS, gs_decl_has_vtt_parm_p,
		GS_DECL_HAS_VTT_PARM_P)
GS_LOOKUP_FLAG (GS_CP_DECL_FLAGS, gs_decl_assignment_operator_p,
		GS_DECL_ASSIGNMENT_OPERATOR_P)
#endif
/* C++ Decl Flags- } */

GS_LOOKUP (gs_decl_template_info, GS_DECL_TEMPLATE_INFO)
GS_LOOKUP (gs_decl_section_name, GS_DECL_SECTION_NAME)
GS_LOOKUP (gs_cp_namespace_decls, GS_CP_NAMESPACE_DECLS)
GS_LOOKUP (gs_cp_decl_context, GS_CP_DECL_CONTEXT)
GS_LOOKUP (gs_decl_vindex, GS_DECL_VINDEX)
GS_LOOKUP (gs_most_general_template, GS_MOST_GENERAL_TEMPLATE)
GS_LOOKUP (gs_decl_namespace_alias, GS_DECL_NAMESPACE_ALIAS)
GS_LOOKUP (gs_thunk_target, GS_THUNK_TARGET)
GS_LOOKUP (gs_decl_ti_template, GS_DECL_TI_TEMPLATE)
GS_LOOKUP (gs_decl_template_instantiations, GS_DECL_VINDEX)
static inline gs_long_t gs_thunk_fixed_offset (gs_t t) {
  return gs_n (gs_operand (t, GS_THUNK_FIXED_OFFSET));
}
GS_LOOKUP (gs_thunk_virtual_offset, GS_THUNK_VIRTUAL_OFFSET)
GS_LOOKUP (gs_decl_named_return_object, GS_DECL_NAMED_RETURN_OBJECT)

/* C++ Type Flags+ { */
GS_LOOKUP_FLAG (GS_CP_TYPE_FLAGS, gs_type_ptrmemfunc_p, GS_TYPE_PTRMEMFUNC_P)
GS_LOOKUP_FLAG (GS_CP_TYPE_FLAGS, gs_type_ptrmem_p, GS_TYPE_PTRMEM_P)
GS_LOOKUP_FLAG (GS_CP_TYPE_FLAGS, gs_classtype_interface_only, GS_CLASSTYPE_INTERFACE_ONLY)
GS_LOOKUP_FLAG (GS_CP_TYPE_FLAGS, gs_is_empty_class, GS_IS_EMPTY_CLASS)
GS_LOOKUP_FLAG (GS_CP_TYPE_FLAGS, gs_class_type_p, GS_CLASS_TYPE_P)
GS_LOOKUP_FLAG (GS_CP_TYPE_FLAGS, gs_anon_union_type_p, GS_ANON_UNION_TYPE_P)
GS_LOOKUP_FLAG (GS_CP_TYPE_FLAGS, gs_classtype_template_specialization,
		GS_CLASSTYPE_TEMPLATE_SPECIALIZATION)
#ifdef FE_GNU_4_2_0
GS_LOOKUP_FLAG (GS_CP_TYPE_FLAGS, gs_classtype_non_pod_p,
		GS_CLASSTYPE_NON_POD_P)
GS_LOOKUP_FLAG (GS_CP_TYPE_FLAGS, gs_type_has_default_constructor,
		GS_TYPE_HAS_DEFAULT_CONSTRUCTOR)
GS_LOOKUP_FLAG (GS_CP_TYPE_FLAGS, gs_type_has_implicit_copy_constructor,
		GS_TYPE_HAS_IMPLICIT_COPY_CONSTRUCTOR)
#endif
/* C++ Type Flags- } */

GS_LOOKUP (gs_type_binfo, GS_TYPE_BINFO)
GS_LOOKUP (gs_binfo_type, GS_BINFO_TYPE)
GS_LOOKUP (gs_binfo_base_binfos, GS_BINFO_BASE_BINFOS)
GS_LOOKUP (gs_binfo_vptr_field, GS_BINFO_VPTR_FIELD)
GS_LOOKUP_FLAG (GS_FLAGS, gs_binfo_virtual_p, GS_BINFO_VIRTUAL_P)

GS_LOOKUP (gs_decl_template_specializations, GS_DECL_SIZE)
static inline void gs_set_decl_template_specializations(gs_t t, gs_t val) {
  gs_set_operand(t, GS_DECL_SIZE, val);
}

GS_LOOKUP (gs_asm_string, GS_ASM_STRING)
GS_LOOKUP (gs_asm_outputs, GS_ASM_OUTPUTS)
GS_LOOKUP (gs_asm_inputs, GS_ASM_INPUTS)
GS_LOOKUP (gs_asm_clobbers, GS_ASM_CLOBBERS)

GS_LOOKUP (gs_do_cond, GS_DO_COND)
GS_LOOKUP (gs_do_body, GS_DO_BODY)

GS_LOOKUP (gs_expr_stmt_expr, GS_EXPR_STMT_EXPR)

GS_LOOKUP (gs_eh_spec_stmts, GS_EH_SPEC_STMTS)
GS_LOOKUP (gs_eh_spec_raises, GS_EH_SPEC_RAISES)

GS_LOOKUP (gs_for_init_stmt, GS_FOR_INIT_STMT)
GS_LOOKUP (gs_for_cond, GS_FOR_COND)
GS_LOOKUP (gs_for_expr, GS_FOR_EXPR)
GS_LOOKUP (gs_for_body, GS_FOR_BODY)

GS_LOOKUP (gs_if_cond, GS_IF_COND)                      
GS_LOOKUP (gs_then_clause, GS_THEN_CLAUSE)                 
GS_LOOKUP (gs_else_clause, GS_ELSE_CLAUSE)                 

GS_LOOKUP (gs_label_expr_label, GS_LABEL_EXPR_LABEL)            

GS_LOOKUP (gs_loop_expr_body, GS_LOOP_EXPR_BODY)              

GS_LOOKUP (gs_stmt_expr_stmt, GS_STMT_EXPR_STMT)              

GS_LOOKUP (gs_try_stmts, GS_TRY_STMTS)
GS_LOOKUP (gs_try_handlers, GS_TRY_HANDLERS)

GS_LOOKUP (gs_while_cond, GS_WHILE_COND)
GS_LOOKUP (gs_while_body, GS_WHILE_BODY)

GS_LOOKUP (gs_obj_type_ref_expr, GS_OBJ_TYPE_REF_EXPR)
GS_LOOKUP (gs_obj_type_ref_object, GS_OBJ_TYPE_REF_OBJECT)
GS_LOOKUP (gs_obj_type_ref_token, GS_OBJ_TYPE_REF_TOKEN)

#ifdef FE_GNU_4_2_0
GS_LOOKUP (gs_omp_parallel_body, GS_OMP_PARALLEL_BODY)
GS_LOOKUP (gs_omp_parallel_clauses, GS_OMP_PARALLEL_CLAUSES)
GS_LOOKUP (gs_omp_critical_body, GS_OMP_CRITICAL_BODY)
GS_LOOKUP (gs_omp_critical_name, GS_OMP_CRITICAL_NAME)
GS_LOOKUP (gs_omp_sections_body, GS_OMP_SECTIONS_BODY)
GS_LOOKUP (gs_omp_sections_clauses, GS_OMP_SECTIONS_CLAUSES)
GS_LOOKUP (gs_omp_section_body, GS_OMP_SECTION_BODY)
GS_LOOKUP (gs_omp_single_body, GS_OMP_SINGLE_BODY)
GS_LOOKUP (gs_omp_single_clauses, GS_OMP_SINGLE_CLAUSES)
GS_LOOKUP (gs_omp_for_body, GS_OMP_FOR_BODY)
GS_LOOKUP (gs_omp_for_clauses, GS_OMP_FOR_CLAUSES)
GS_LOOKUP (gs_omp_for_init, GS_OMP_FOR_INIT)
GS_LOOKUP (gs_omp_for_cond, GS_OMP_FOR_COND)
GS_LOOKUP (gs_omp_for_incr, GS_OMP_FOR_INCR)
GS_LOOKUP (gs_omp_master_body, GS_OMP_MASTER_BODY)
GS_LOOKUP (gs_omp_ordered_body, GS_OMP_ORDERED_BODY)
#endif

/* C++ Expr Flags+ { */
GS_LOOKUP_FLAG (GS_CP_EXPR_FLAGS, gs_stmt_is_full_expr_p, GS_STMT_IS_FULL_EXPR_P)
GS_LOOKUP_FLAG (GS_CP_EXPR_FLAGS, gs_aggr_init_via_ctor_p, GS_AGGR_INIT_VIA_CTOR_P)
GS_LOOKUP_FLAG (GS_CP_EXPR_FLAGS, gs_cleanup_eh_only, GS_CLEANUP_EH_ONLY)
/* C++ Expr Flags- } */

GS_LOOKUP (gs_type_main_variant, GS_TYPE_MAIN_VARIANT)
GS_LOOKUP (gs_type_vfield, GS_TYPE_VFIELD)
GS_LOOKUP (gs_type_methods, GS_TYPE_METHODS)
GS_LOOKUP (gs_classtype_as_base, GS_CLASSTYPE_AS_BASE)
GS_LOOKUP (gs_classtype_typeinfo_var, GS_CLASSTYPE_TYPEINFO_VAR)
GS_LOOKUP (gs_typeinfo_decl, GS_TYPEINFO_DECL)
GS_LOOKUP (gs_classtype_copy_constructor, GS_CLASSTYPE_COPY_CONSTRUCTOR)

static inline void gs_set_tree_operand (gs_t node, int i, gs_t opnd)
{
  gs_set_operand(node, GS_TREE_OPERAND_ZERO + i, opnd);
}

GS_LOOKUP (gs_template_parm_idx, GS_TEMPLATE_PARM_IDX)
GS_LOOKUP (gs_template_parm_level, GS_TEMPLATE_PARM_LEVEL)
GS_LOOKUP (gs_template_parm_descendants, GS_TEMPLATE_PARM_DESCENDANTS)
GS_LOOKUP (gs_template_parm_orig_level, GS_TEMPLATE_PARM_ORIG_LEVEL)
GS_LOOKUP (gs_template_parm_decl, GS_TEMPLATE_PARM_DECL)

GS_LOOKUP (gs_baselink_binfo, GS_BASELINK_BINFO)
GS_LOOKUP (gs_baselink_functions, GS_BASELINK_FUNCTIONS)
GS_LOOKUP (gs_baselink_access_binfo, GS_BASELINK_ACCESS_BINFO)
GS_LOOKUP (gs_baselink_optype, GS_BASELINK_OPTYPE)

GS_LOOKUP (gs_ovl_function, GS_OVL_FUNCTION)
GS_LOOKUP (gs_ovl_chain, GS_OVL_CHAIN)
GS_LOOKUP (gs_ovl_current, GS_OVL_CURRENT)
GS_LOOKUP (gs_ovl_next, GS_OVL_NEXT)
GS_LOOKUP_FLAG (GS_FLAGS, gs_ovl_used, GS_TREE_USED)

#ifdef FE_GNU_4_2_0
static inline gs_omp_clause_code_t gs_omp_clause_code (gs_t t)
{
  gs_t clause_code = gs_operand(t, GS_OMP_CLAUSE_CODE);
  return (clause_code != (gs_t) NULL) ?
          (gs_omp_clause_code_t) gs_n(clause_code) : GS_OMP_CLAUSE_ERROR;
}
GS_LOOKUP (gs_omp_clause_decl, GS_OMP_CLAUSE_DECL)
GS_LOOKUP (gs_omp_clause_num_threads_expr, GS_OMP_CLAUSE_NUM_THREADS_EXPR)
GS_LOOKUP (gs_omp_clause_if_expr, GS_OMP_CLAUSE_IF_EXPR)
GS_LOOKUP (gs_omp_clause_chain, GS_TREE_CHAIN) /* OMP_CLAUSE_CHAIN */

static inline gs_omp_clause_default_kind_t gs_omp_clause_default_kind (gs_t t)
{
  gs_t default_kind = gs_operand(t, GS_OMP_CLAUSE_DEFAULT_KIND);
  return (default_kind != (gs_t) NULL) ?
          (gs_omp_clause_default_kind_t) gs_n(default_kind) :
          GS_OMP_CLAUSE_DEFAULT_UNSPECIFIED;
}

static inline gs_code_t gs_omp_clause_reduction_code (gs_t t)
{
  gs_t reduction_code = gs_operand(t, GS_OMP_CLAUSE_REDUCTION_CODE);
  return (reduction_code != (gs_t) NULL) ?
          (gs_code_t) gs_n(reduction_code): GS_ERROR_MARK;
}

static inline gs_omp_clause_schedule_kind_t gs_omp_clause_schedule_kind (gs_t t)
{
  gs_t schedule_kind = gs_operand(t, GS_OMP_CLAUSE_SCHEDULE_KIND);
  return (gs_omp_clause_schedule_kind_t) gs_n(schedule_kind);
}

GS_LOOKUP (gs_omp_clause_schedule_chunk_expr, GS_OMP_CLAUSE_SCHEDULE_CHUNK_EXPR)
#endif

static inline gs_t gs_classtype_size(gs_t t)
{
  gs_t q;
  GS_ASSERT (t != (gs_t) NULL, "Got null node"); 
  q = gs_classtype_as_base (t);
  if (q)
    return gs_type_size (q);
  else
    return NULL;
}

static inline void gs_set_decl_arg_type(gs_t t, gs_t val) {
  gs_set_operand(t, GS_DECL_ARG_TYPE, val);
}

GS_LOOKUP_FLAG (GS_FLAGS, gs_cleanup_p, GS_TREE_LANG_FLAG_0)
GS_LOOKUP (gs_handler_type, GS_TREE_TYPE)
GS_LOOKUP (gs_handler_parms, GS_HANDLER_PARMS)
GS_LOOKUP (gs_handler_body, GS_HANDLER_BODY)

GS_LOOKUP (gs_cleanup_body, GS_CLEANUP_BODY)
GS_LOOKUP (gs_cleanup_expr, GS_CLEANUP_EXPR)

GS_LOOKUP (gs_catch_types, GS_CATCH_TYPES)
GS_LOOKUP (gs_catch_body, GS_CATCH_BODY)

GS_LOOKUP_FLAG (GS_FLAGS, gs_identifier_opname_p, GS_TREE_LANG_FLAG_2)
GS_LOOKUP_FLAG (GS_FLAGS, gs_identifier_typename_p, GS_TREE_LANG_FLAG_4)
GS_LOOKUP_FLAG (GS_FLAGS, gs_decl_tinfo_p, GS_TREE_LANG_FLAG_4)

static inline gs_int_t gs_list_length (gs_t t) 
{
  gs_t p;
  gs_int_t length = 0;
  GS_ASSERT (t != (gs_t) NULL, "Got null node"); 
  
  p = t;
  while (p) {
    p = gs_tree_chain (p);
    length++;
  }
  return length;
}

GS_LOOKUP_FLAG (GS_FLAGS, gs_identifier_ctor_or_dtor_p, GS_TREE_LANG_FLAG_3)

static inline int gs_decl_friend_pseudo_template_instantiation(gs_t node)
{
  return (gs_decl_template_info(node) && !gs_decl_use_template(node));
}

static inline gs_t gs_tree_last(gs_t node)
{
  if (node == NULL)
    return node;

  while (gs_tree_chain(node) != NULL)
    node = gs_tree_chain(node);
  return node;
}

static inline int gs_really_constant_p(gs_t exp)
{
  gs_code_t code = gs_tree_code(exp);

  while (code == GS_NOP_EXPR
    	 || code == GS_CONVERT_EXPR
	 || code == GS_NON_LVALUE_EXPR) {
    exp = gs_tree_operand(exp, 0);
    code = gs_tree_code(exp);
  }
  return gs_tree_constant(exp);
}

#ifdef FE_GNU_4_2_0
static inline gs_t
gs_skip_artificial_parms_for (gs_t fn, gs_t list)
{
  if (gs_decl_nonstatic_member_function_p (fn))
    list = gs_tree_chain (list);
  else
    return list;

  if (gs_decl_has_in_charge_parm_p (fn))
    list = gs_tree_chain (list);
  if (gs_decl_has_vtt_parm_p (fn))
    list = gs_tree_chain (list);
  return list;
}

#define gs_function_first_user_parmtype(fn) \
    gs_skip_artificial_parms_for (fn, gs_type_arg_types (gs_tree_type (fn)))
#endif

static inline gs_bool_t
gs_type_anonymous_p(gs_t type_tree)
{
    /* anonymous struct/union */
    if ( (((gs_tree_code(type_tree) == GS_UNION_TYPE ||
            gs_tree_code(type_tree) == GS_RECORD_TYPE) &&
            gs_type_lang_flag_5(type_tree))||
            /* anonymous enumeration */
            gs_tree_code(type_tree) == GS_ENUMERAL_TYPE) )
    {
        gs_t type_name;
        gs_t decl_name;
        if ( (type_name = gs_type_name(type_tree)) == NULL ||
                (decl_name = gs_decl_name(type_name)) == NULL )
            return gs_true;
        else {
            char *name = (char *)gs_identifier_pointer(decl_name);
            if ( name[0] == '.' && name[1] == '_')
                return gs_true;
        }
    }
    return gs_false;
}

#endif /* __GSPIN_TEL_H__ */
