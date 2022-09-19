#include "wintrinsic.h"

#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif

/* Enumeration of the return types for intrinsic functions and
 * operators.
 */
typedef enum INTR_RETURN_KIND {
  RETURN_UNKNOWN,	/* Unknown type (should never occur) */
  RETURN_V,	/* No return value (void type) */
  RETURN_I1,	/* Signed 1 byte integral */
  RETURN_I2,	/* Signed 2 byte integral */
  RETURN_I4,	/* Signed 4 byte integral */
  RETURN_I8,	/* Signed 8 byte integral */
  RETURN_U1,	/* Unsigned 1 byte integral */
  RETURN_U2,	/* Unsigned 2 byte integral */
  RETURN_U4,	/* Unsigned 4 byte integral */
  RETURN_U8,	/* Unsigned 8 byte integral */
  RETURN_F4,	/* Single precision floating point */
  RETURN_F8,	/* Double precision floating point */
  RETURN_FQ,	/* Quad precision floating point */
  RETURN_C4,	/* Single precision complex */
  RETURN_C8,	/* Double precision complex */
  RETURN_CQ,	/* Quad precision complex */
  RETURN_PV,	/* Pointer to anything (void ptr) */
  RETURN_PU1,	/* Pointer to unsigned single byte integral (char ptr) */
  RETURN_DA1,	/* Dereference type of argument 1 to get type */
  RETURN_SZT,	/* size_t */
  RETURN_PC,	/* Pointer to char */
  RETURN_F10 	/* Double extended precision floating point */
} INTR_RETURN_KIND;
typedef struct intr_map_info {
#ifdef DEBUG
 char       *wnode_name;
#endif
#ifdef CHECKING
 INTRINSIC  wnode;
#endif
 INTR_RETURN_KIND return_kind;
#ifdef BUILD_WHIRL2C
 char       *c_name;
#else
 char       *specific_name;
#endif
#ifdef MONGOOSE_BE
 char       is_actual_argument;
 char       is_cg_intrinsic;
#endif
 char       is_by_value;
 char       is_pure;
 char       has_no_side_effects;
 char       never_returns;
 char       *runtime_name;
} intr_map_info;


#ifdef DEBUG
#define INTR_wnode_name(x) intr_map[x].wnode_name
#endif
#ifdef CHECKING
#define INTR_wnode(x) intr_map[x].wnode
#endif
#define INTR_return_kind(x) intr_map[x].return_kind
#ifdef BUILD_WHIRL2C
#define INTR_c_name(x) intr_map[x].c_name
#endif
#ifdef BUILD_WHIRL2F
#define INTR_specific_name(x) intr_map[x].specific_name
#endif
#ifdef MONGOOSE_BE
#define INTR_is_actual(x) intr_map[x].is_actual_argument
#define INTR_cg_intrinsic(x) intr_map[x].is_cg_intrinsic
#endif
#define INTR_by_value(x) intr_map[x].is_by_value
#define INTR_is_pure(x) intr_map[x].is_pure
#define INTR_has_no_side_effects(x) intr_map[x].has_no_side_effects
#define INTR_never_returns(x) intr_map[x].never_returns
#define INTR_need_intrinsic_call(x) \
        ((INTR_is_pure(x) == FALSE) || \
         (INTR_has_no_side_effects(x) == FALSE) || \
         (INTR_return_kind(x) == RETURN_V))
#define INTR_rt_name(x) intr_map[x].runtime_name

EXTERN intr_map_info intr_map[INTRINSIC_LAST+1]
#if VAR_INITIALIZERS
= {
 { /* 0 */
#ifdef DEBUG
   /* wnode_name = */		"NONE",
#endif
#ifdef CHECKING
   /* wnode = */		INTRINSIC_NONE,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 1 */
#ifdef DEBUG
   /* wnode_name = */		"I4EXPEXPR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4EXPEXPR,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__powii"
 },
 { /* 2 */
#ifdef DEBUG
   /* wnode_name = */		"I8EXPEXPR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8EXPEXPR,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__powll"
 },
 { /* 3 */
#ifdef DEBUG
   /* wnode_name = */		"F4EXPEXPR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4EXPEXPR,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"powf"
 },
 { /* 4 */
#ifdef DEBUG
   /* wnode_name = */		"F8EXPEXPR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8EXPEXPR,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"pow"
 },
 { /* 5 */
#ifdef DEBUG
   /* wnode_name = */		"FQEXPEXPR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQEXPEXPR,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"qpow"
 },
 { /* 6 */
#ifdef DEBUG
   /* wnode_name = */		"C4EXPEXPR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C4EXPEXPR,
#endif
   /* return_kind = */		RETURN_C4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__powcc"
 },
 { /* 7 */
#ifdef DEBUG
   /* wnode_name = */		"C8EXPEXPR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C8EXPEXPR,
#endif
   /* return_kind = */		RETURN_C8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__powzz"
 },
 { /* 8 */
#ifdef DEBUG
   /* wnode_name = */		"CQEXPEXPR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_CQEXPEXPR,
#endif
   /* return_kind = */		RETURN_CQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__cqpow"
 },
 { /* 9 */
#ifdef DEBUG
   /* wnode_name = */		"F4I4EXPEXPR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4I4EXPEXPR,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__powri"
 },
 { /* 10 */
#ifdef DEBUG
   /* wnode_name = */		"F4I8EXPEXPR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4I8EXPEXPR,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__powrl"
 },
 { /* 11 */
#ifdef DEBUG
   /* wnode_name = */		"F8I4EXPEXPR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8I4EXPEXPR,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__powdi"
 },
 { /* 12 */
#ifdef DEBUG
   /* wnode_name = */		"F8I8EXPEXPR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8I8EXPEXPR,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__powdl"
 },
 { /* 13 */
#ifdef DEBUG
   /* wnode_name = */		"FQI4EXPEXPR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQI4EXPEXPR,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__pow_qi"
 },
 { /* 14 */
#ifdef DEBUG
   /* wnode_name = */		"FQI8EXPEXPR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQI8EXPEXPR,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__pow_ql"
 },
 { /* 15 */
#ifdef DEBUG
   /* wnode_name = */		"C4I4EXPEXPR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C4I4EXPEXPR,
#endif
   /* return_kind = */		RETURN_C4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__powci"
 },
 { /* 16 */
#ifdef DEBUG
   /* wnode_name = */		"C4I8EXPEXPR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C4I8EXPEXPR,
#endif
   /* return_kind = */		RETURN_C4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__powcl"
 },
 { /* 17 */
#ifdef DEBUG
   /* wnode_name = */		"C8I4EXPEXPR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C8I4EXPEXPR,
#endif
   /* return_kind = */		RETURN_C8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__powzi"
 },
 { /* 18 */
#ifdef DEBUG
   /* wnode_name = */		"C8I8EXPEXPR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C8I8EXPEXPR,
#endif
   /* return_kind = */		RETURN_C8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__powzl"
 },
 { /* 19 */
#ifdef DEBUG
   /* wnode_name = */		"CQI4EXPEXPR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_CQI4EXPEXPR,
#endif
   /* return_kind = */		RETURN_CQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__powcqi"
 },
 { /* 20 */
#ifdef DEBUG
   /* wnode_name = */		"CQI8EXPEXPR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_CQI8EXPEXPR,
#endif
   /* return_kind = */		RETURN_CQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__powcql"
 },
 { /* 21 */
#ifdef DEBUG
   /* wnode_name = */		"CEQEXPR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_CEQEXPR,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"s_cmp"
 },
 { /* 22 */
#ifdef DEBUG
   /* wnode_name = */		"CNEEXPR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_CNEEXPR,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"s_cmp"
 },
 { /* 23 */
#ifdef DEBUG
   /* wnode_name = */		"CGEEXPR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_CGEEXPR,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"s_cmp"
 },
 { /* 24 */
#ifdef DEBUG
   /* wnode_name = */		"CGTEXPR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_CGTEXPR,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"s_cmp"
 },
 { /* 25 */
#ifdef DEBUG
   /* wnode_name = */		"CLEEXPR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_CLEEXPR,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"s_cmp"
 },
 { /* 26 */
#ifdef DEBUG
   /* wnode_name = */		"CLTEXPR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_CLTEXPR,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"s_cmp"
 },
 { /* 27 */
#ifdef DEBUG
   /* wnode_name = */		"SUBSTRINGEXPR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_SUBSTRINGEXPR,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"s_copy"
 },
 { /* 28 */
#ifdef DEBUG
   /* wnode_name = */		"CONCATEXPR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_CONCATEXPR,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"s_cat"
 },
 { /* 29 */
#ifdef DEBUG
   /* wnode_name = */		"CASSIGNSTMT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_CASSIGNSTMT,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"s_copy"
 },
 { /* 30 */
#ifdef DEBUG
   /* wnode_name = */		"I2ABSe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2ABSe,
#endif
   /* return_kind = */		RETURN_I2,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"IIABS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"h_abs"
 },
 { /* 31 */
#ifdef DEBUG
   /* wnode_name = */		"I4ABSe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4ABSe,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"JIABS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"i_abs"
 },
 { /* 32 */
#ifdef DEBUG
   /* wnode_name = */		"I8ABSe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8ABSe,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"KIABS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"l_abs"
 },
 { /* 33 */
#ifdef DEBUG
   /* wnode_name = */		"F4ABSe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4ABSe,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ABS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"r_abs"
 },
 { /* 34 */
#ifdef DEBUG
   /* wnode_name = */		"F8ABSe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8ABSe,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DABS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"d_abs"
 },
 { /* 35 */
#ifdef DEBUG
   /* wnode_name = */		"FQABSe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQABSe,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QABS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__q_abs"
 },
 { /* 36 */
#ifdef DEBUG
   /* wnode_name = */		"F4C4ABS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4C4ABS,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"CABS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__c8abs"
 },
 { /* 37 */
#ifdef DEBUG
   /* wnode_name = */		"F4C4ABSe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4C4ABSe,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"CABS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"c_abs_"
 },
 { /* 38 */
#ifdef DEBUG
   /* wnode_name = */		"F8C8ABS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8C8ABS,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ZABS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__zabs"
 },
 { /* 39 */
#ifdef DEBUG
   /* wnode_name = */		"F8C8ABSe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8C8ABSe,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ZABS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"z_abs_"
 },
 { /* 40 */
#ifdef DEBUG
   /* wnode_name = */		"FQCQABS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQCQABS,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"CQABS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__cqabs"
 },
 { /* 41 */
#ifdef DEBUG
   /* wnode_name = */		"FQCQABSe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQCQABSe,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"CQABS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__cq_abs"
 },
 { /* 42 */
#ifdef DEBUG
   /* wnode_name = */		"I2MODe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2MODe,
#endif
   /* return_kind = */		RETURN_I2,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"IMOD",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"h_mod"
 },
 { /* 43 */
#ifdef DEBUG
   /* wnode_name = */		"I4MODe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4MODe,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"JMOD",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"i_mod"
 },
 { /* 44 */
#ifdef DEBUG
   /* wnode_name = */		"I8MODe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8MODe,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"KMOD",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"l_mod"
 },
 { /* 45 */
#ifdef DEBUG
   /* wnode_name = */		"F4MOD",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4MOD,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"AMOD",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__rmod"
 },
 { /* 46 */
#ifdef DEBUG
   /* wnode_name = */		"F4MODe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4MODe,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"AMOD",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"r_mod"
 },
 { /* 47 */
#ifdef DEBUG
   /* wnode_name = */		"F8MOD",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8MOD,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DMOD",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__dmod"
 },
 { /* 48 */
#ifdef DEBUG
   /* wnode_name = */		"F8MODe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8MODe,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DMOD",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"d_mod"
 },
 { /* 49 */
#ifdef DEBUG
   /* wnode_name = */		"FQMOD",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQMOD,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QMOD",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__qmod"
 },
 { /* 50 */
#ifdef DEBUG
   /* wnode_name = */		"FQMODe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQMODe,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QMOD",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__q_mod"
 },
 { /* 51 */
#ifdef DEBUG
   /* wnode_name = */		"F4SQRTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4SQRTe,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"SQRT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"r_sqrt"
 },
 { /* 52 */
#ifdef DEBUG
   /* wnode_name = */		"F8SQRTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8SQRTe,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DSQRT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"d_sqrt"
 },
 { /* 53 */
#ifdef DEBUG
   /* wnode_name = */		"FQSQRTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQSQRTe,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QSQRT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__q_sqrt"
 },
 { /* 54 */
#ifdef DEBUG
   /* wnode_name = */		"C4SQRTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C4SQRTe,
#endif
   /* return_kind = */		RETURN_C4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"CSQRT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"c_sqrt_"
 },
 { /* 55 */
#ifdef DEBUG
   /* wnode_name = */		"C8SQRTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C8SQRTe,
#endif
   /* return_kind = */		RETURN_C8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ZSQRT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"z_sqrt_"
 },
 { /* 56 */
#ifdef DEBUG
   /* wnode_name = */		"CQSQRTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_CQSQRTe,
#endif
   /* return_kind = */		RETURN_CQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"CQSQRT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__cq_sqrt"
 },
 { /* 57 */
#ifdef DEBUG
   /* wnode_name = */		"C4CONJG",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C4CONJG,
#endif
   /* return_kind = */		RETURN_C4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"CONJG",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__rconjg"
 },
 { /* 58 */
#ifdef DEBUG
   /* wnode_name = */		"C4CONJGe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C4CONJGe,
#endif
   /* return_kind = */		RETURN_C4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"CONJG",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"r_cnjg_"
 },
 { /* 59 */
#ifdef DEBUG
   /* wnode_name = */		"C8CONJG",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C8CONJG,
#endif
   /* return_kind = */		RETURN_C8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DCONJG",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__dconjg"
 },
 { /* 60 */
#ifdef DEBUG
   /* wnode_name = */		"C8CONJGe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C8CONJGe,
#endif
   /* return_kind = */		RETURN_C8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DCONJG",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"d_cnjg_"
 },
 { /* 61 */
#ifdef DEBUG
   /* wnode_name = */		"CQCONJG",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_CQCONJG,
#endif
   /* return_kind = */		RETURN_CQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QCONJG",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__qconjg"
 },
 { /* 62 */
#ifdef DEBUG
   /* wnode_name = */		"CQCONJGe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_CQCONJGe,
#endif
   /* return_kind = */		RETURN_CQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QCONJG",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__cq_conjg"
 },
 { /* 63 */
#ifdef DEBUG
   /* wnode_name = */		"I1DIM",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I1DIM,
#endif
   /* return_kind = */		RETURN_I1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DIM",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"b_dim"
 },
 { /* 64 */
#ifdef DEBUG
   /* wnode_name = */		"I2DIM",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2DIM,
#endif
   /* return_kind = */		RETURN_I2,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"IIDIM",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"h_dim"
 },
 { /* 65 */
#ifdef DEBUG
   /* wnode_name = */		"I2DIMe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2DIMe,
#endif
   /* return_kind = */		RETURN_I2,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"IIDIM",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"h_dim"
 },
 { /* 66 */
#ifdef DEBUG
   /* wnode_name = */		"I4DIM",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4DIM,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"JIDIM",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"i_dim"
 },
 { /* 67 */
#ifdef DEBUG
   /* wnode_name = */		"I4DIMe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4DIMe,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"JIDIM",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"i_dim"
 },
 { /* 68 */
#ifdef DEBUG
   /* wnode_name = */		"I8DIM",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8DIM,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"KIDIM",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"l_dim"
 },
 { /* 69 */
#ifdef DEBUG
   /* wnode_name = */		"I8DIMe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8DIMe,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"KIDIM",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"l_dim"
 },
 { /* 70 */
#ifdef DEBUG
   /* wnode_name = */		"F4DIM",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4DIM,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DIM",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"r_dim"
 },
 { /* 71 */
#ifdef DEBUG
   /* wnode_name = */		"F4DIMe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4DIMe,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DIM",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"r_dim"
 },
 { /* 72 */
#ifdef DEBUG
   /* wnode_name = */		"F8DIM",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8DIM,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DDIM",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"d_dim"
 },
 { /* 73 */
#ifdef DEBUG
   /* wnode_name = */		"F8DIMe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8DIMe,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DDIM",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"d_dim"
 },
 { /* 74 */
#ifdef DEBUG
   /* wnode_name = */		"FQDIM",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQDIM,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QDIM",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__qdim"
 },
 { /* 75 */
#ifdef DEBUG
   /* wnode_name = */		"FQDIMe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQDIMe,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QDIM",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__qdim"
 },
 { /* 76 */
#ifdef DEBUG
   /* wnode_name = */		"F8F4PROD",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8F4PROD,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DPROD",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"d_prod"
 },
 { /* 77 */
#ifdef DEBUG
   /* wnode_name = */		"F8F4PRODe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8F4PRODe,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DPROD",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"d_prod"
 },
 { /* 78 */
#ifdef DEBUG
   /* wnode_name = */		"FQF8PROD",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQF8PROD,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QPROD",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__qprod"
 },
 { /* 79 */
#ifdef DEBUG
   /* wnode_name = */		"FQF8PRODe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQF8PRODe,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QPROD",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__q_prod"
 },
 { /* 80 */
#ifdef DEBUG
   /* wnode_name = */		"I1SIGN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I1SIGN,
#endif
   /* return_kind = */		RETURN_I1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"SIGN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__bsign"
 },
 { /* 81 */
#ifdef DEBUG
   /* wnode_name = */		"I2SIGN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2SIGN,
#endif
   /* return_kind = */		RETURN_I2,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"IISIGN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__hsign"
 },
 { /* 82 */
#ifdef DEBUG
   /* wnode_name = */		"I2SIGNe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2SIGNe,
#endif
   /* return_kind = */		RETURN_I2,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"IISIGN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"h_sign"
 },
 { /* 83 */
#ifdef DEBUG
   /* wnode_name = */		"I4SIGN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4SIGN,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"JISIGN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__isign"
 },
 { /* 84 */
#ifdef DEBUG
   /* wnode_name = */		"I4SIGNe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4SIGNe,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"JISIGN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"i_sign"
 },
 { /* 85 */
#ifdef DEBUG
   /* wnode_name = */		"I8SIGN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8SIGN,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"KISIGN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__lsign"
 },
 { /* 86 */
#ifdef DEBUG
   /* wnode_name = */		"I8SIGNe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8SIGNe,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"KISIGN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"l_sign"
 },
 { /* 87 */
#ifdef DEBUG
   /* wnode_name = */		"F4SIGN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4SIGN,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"SIGN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__rsign"
 },
 { /* 88 */
#ifdef DEBUG
   /* wnode_name = */		"F4SIGNe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4SIGNe,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"SIGN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"r_sign"
 },
 { /* 89 */
#ifdef DEBUG
   /* wnode_name = */		"F8SIGN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8SIGN,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DSIGN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__dsign"
 },
 { /* 90 */
#ifdef DEBUG
   /* wnode_name = */		"F8SIGNe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8SIGNe,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DSIGN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"d_sign"
 },
 { /* 91 */
#ifdef DEBUG
   /* wnode_name = */		"FQSIGN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQSIGN,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QSIGN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__qsign"
 },
 { /* 92 */
#ifdef DEBUG
   /* wnode_name = */		"FQSIGNe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQSIGNe,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QSIGN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__q_sign"
 },
 { /* 93 */
#ifdef DEBUG
   /* wnode_name = */		"F4IMAGe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4IMAGe,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"IMAG",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"r_imag"
 },
 { /* 94 */
#ifdef DEBUG
   /* wnode_name = */		"F8IMAGe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8IMAGe,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DIMAG",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"d_imag"
 },
 { /* 95 */
#ifdef DEBUG
   /* wnode_name = */		"FQIMAGe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQIMAGe,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QIMAG",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__cq_imag"
 },
 { /* 96 */
#ifdef DEBUG
   /* wnode_name = */		"F4AINT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4AINT,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"AINT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"truncf"
 },
 { /* 97 */
#ifdef DEBUG
   /* wnode_name = */		"F4AINTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4AINTe,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"AINT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"r_int"
 },
 { /* 98 */
#ifdef DEBUG
   /* wnode_name = */		"F8AINT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8AINT,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DINT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"trunc"
 },
 { /* 99 */
#ifdef DEBUG
   /* wnode_name = */		"F8AINTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8AINTe,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DINT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"d_int"
 },
 { /* 100 */
#ifdef DEBUG
   /* wnode_name = */		"FQAINT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQAINT,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QINT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__qint"
 },
 { /* 101 */
#ifdef DEBUG
   /* wnode_name = */		"FQAINTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQAINTe,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QINT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__q_int"
 },
 { /* 102 */
#ifdef DEBUG
   /* wnode_name = */		"I2F4INTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2F4INTe,
#endif
   /* return_kind = */		RETURN_I2,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"IINT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__iirint"
 },
 { /* 103 */
#ifdef DEBUG
   /* wnode_name = */		"I4F4INTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4F4INTe,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"JINT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__jirint"
 },
 { /* 104 */
#ifdef DEBUG
   /* wnode_name = */		"I8F4INTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8F4INTe,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"KINT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__kirint"
 },
 { /* 105 */
#ifdef DEBUG
   /* wnode_name = */		"I2F8IDINTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2F8IDINTe,
#endif
   /* return_kind = */		RETURN_I2,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"IIDINT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__iidint"
 },
 { /* 106 */
#ifdef DEBUG
   /* wnode_name = */		"I4F8IDINTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4F8IDINTe,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"JIDINT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__jidint"
 },
 { /* 107 */
#ifdef DEBUG
   /* wnode_name = */		"I8F8IDINTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8F8IDINTe,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"KIDINT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__kidint"
 },
 { /* 108 */
#ifdef DEBUG
   /* wnode_name = */		"I2FQIQINTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2FQIQINTe,
#endif
   /* return_kind = */		RETURN_I2,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"IIQINT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__iiqint"
 },
 { /* 109 */
#ifdef DEBUG
   /* wnode_name = */		"I4FQIQINTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4FQIQINTe,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"JIQINT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__jiqint"
 },
 { /* 110 */
#ifdef DEBUG
   /* wnode_name = */		"I8FQIQINTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8FQIQINTe,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"KIQINT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__kiqint"
 },
 { /* 111 */
#ifdef DEBUG
   /* wnode_name = */		"I2F4NINT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2F4NINT,
#endif
   /* return_kind = */		RETURN_I2,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ININT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"h_nint"
 },
 { /* 112 */
#ifdef DEBUG
   /* wnode_name = */		"I2F4NINTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2F4NINTe,
#endif
   /* return_kind = */		RETURN_I2,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ININT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"h_nint"
 },
 { /* 113 */
#ifdef DEBUG
   /* wnode_name = */		"I4F4NINT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4F4NINT,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"JNINT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"i_nint"
 },
 { /* 114 */
#ifdef DEBUG
   /* wnode_name = */		"I4F4NINTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4F4NINTe,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"JNINT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"i_nint"
 },
 { /* 115 */
#ifdef DEBUG
   /* wnode_name = */		"I8F4NINT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8F4NINT,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"KNINT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"l_nint"
 },
 { /* 116 */
#ifdef DEBUG
   /* wnode_name = */		"I8F4NINTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8F4NINTe,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"KNINT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"l_nint"
 },
 { /* 117 */
#ifdef DEBUG
   /* wnode_name = */		"I2F8IDNINT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2F8IDNINT,
#endif
   /* return_kind = */		RETURN_I2,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"IIDNNT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"h_dnnt"
 },
 { /* 118 */
#ifdef DEBUG
   /* wnode_name = */		"I2F8IDNINTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2F8IDNINTe,
#endif
   /* return_kind = */		RETURN_I2,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"IIDNNT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"h_dnnt"
 },
 { /* 119 */
#ifdef DEBUG
   /* wnode_name = */		"I4F8IDNINT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4F8IDNINT,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"JIDNNT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"i_dnnt"
 },
 { /* 120 */
#ifdef DEBUG
   /* wnode_name = */		"I4F8IDNINTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4F8IDNINTe,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"JIDNNT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"i_dnnt"
 },
 { /* 121 */
#ifdef DEBUG
   /* wnode_name = */		"I8F8IDNINT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8F8IDNINT,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"KIDNNT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"l_dnnt"
 },
 { /* 122 */
#ifdef DEBUG
   /* wnode_name = */		"I8F8IDNINTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8F8IDNINTe,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"KIDNNT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"l_dnnt"
 },
 { /* 123 */
#ifdef DEBUG
   /* wnode_name = */		"I2FQIQNINT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2FQIQNINT,
#endif
   /* return_kind = */		RETURN_I2,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"IIQNNT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__iiqnnt"
 },
 { /* 124 */
#ifdef DEBUG
   /* wnode_name = */		"I2FQIQNINTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2FQIQNINTe,
#endif
   /* return_kind = */		RETURN_I2,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"IIQNNT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__ii_qnnt"
 },
 { /* 125 */
#ifdef DEBUG
   /* wnode_name = */		"I4FQIQNINT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4FQIQNINT,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"JIQNNT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__jiqnnt"
 },
 { /* 126 */
#ifdef DEBUG
   /* wnode_name = */		"I4FQIQNINTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4FQIQNINTe,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"JIQNNT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__ji_qnnt"
 },
 { /* 127 */
#ifdef DEBUG
   /* wnode_name = */		"I8FQIQNINT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8FQIQNINT,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"KIQNNT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__kiqnnt"
 },
 { /* 128 */
#ifdef DEBUG
   /* wnode_name = */		"I8FQIQNINTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8FQIQNINTe,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"KIQNNT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__ki_qnnt"
 },
 { /* 129 */
#ifdef DEBUG
   /* wnode_name = */		"F4ANINT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4ANINT,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ANINT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__rnint"
 },
 { /* 130 */
#ifdef DEBUG
   /* wnode_name = */		"F4ANINTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4ANINTe,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ANINT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"r_nint"
 },
 { /* 131 */
#ifdef DEBUG
   /* wnode_name = */		"F8ANINT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8ANINT,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DNINT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__dnint"
 },
 { /* 132 */
#ifdef DEBUG
   /* wnode_name = */		"F8ANINTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8ANINTe,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DNINT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"d_nint"
 },
 { /* 133 */
#ifdef DEBUG
   /* wnode_name = */		"FQANINT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQANINT,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QNINT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__qnint"
 },
 { /* 134 */
#ifdef DEBUG
   /* wnode_name = */		"FQANINTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQANINTe,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QNINT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__q_nint"
 },
 { /* 135 */
#ifdef DEBUG
   /* wnode_name = */		"I2BNOTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2BNOTe,
#endif
   /* return_kind = */		RETURN_I2,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"INOT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"not_h"
 },
 { /* 136 */
#ifdef DEBUG
   /* wnode_name = */		"I4BNOTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4BNOTe,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"JNOT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"not_l"
 },
 { /* 137 */
#ifdef DEBUG
   /* wnode_name = */		"I8BNOTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8BNOTe,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"KNOT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"not_ll"
 },
 { /* 138 */
#ifdef DEBUG
   /* wnode_name = */		"I2BANDe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2BANDe,
#endif
   /* return_kind = */		RETURN_I2,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"IIAND",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"and_h"
 },
 { /* 139 */
#ifdef DEBUG
   /* wnode_name = */		"I4BANDe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4BANDe,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"JIAND",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"and_l"
 },
 { /* 140 */
#ifdef DEBUG
   /* wnode_name = */		"I8BANDe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8BANDe,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"KIAND",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"and_ll"
 },
 { /* 141 */
#ifdef DEBUG
   /* wnode_name = */		"I2BIORe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2BIORe,
#endif
   /* return_kind = */		RETURN_I2,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"IIOR",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"or_h"
 },
 { /* 142 */
#ifdef DEBUG
   /* wnode_name = */		"I4BIORe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4BIORe,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"JIOR",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"or_l"
 },
 { /* 143 */
#ifdef DEBUG
   /* wnode_name = */		"I8BIORe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8BIORe,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"KIOR",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"or_ll"
 },
 { /* 144 */
#ifdef DEBUG
   /* wnode_name = */		"I2BXORe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2BXORe,
#endif
   /* return_kind = */		RETURN_I2,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"IIEOR",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"xor_h"
 },
 { /* 145 */
#ifdef DEBUG
   /* wnode_name = */		"I4BXORe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4BXORe,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"JIEOR",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"xor_l"
 },
 { /* 146 */
#ifdef DEBUG
   /* wnode_name = */		"I8BXORe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8BXORe,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"KIEOR",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"xor_ll"
 },
 { /* 147 */
#ifdef DEBUG
   /* wnode_name = */		"I1BITS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I1BITS,
#endif
   /* return_kind = */		RETURN_I1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"IBITS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"bext_b"
 },
 { /* 148 */
#ifdef DEBUG
   /* wnode_name = */		"I2BITS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2BITS,
#endif
   /* return_kind = */		RETURN_I2,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"IIBITS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"bext_h"
 },
 { /* 149 */
#ifdef DEBUG
   /* wnode_name = */		"I2BITSe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2BITSe,
#endif
   /* return_kind = */		RETURN_I2,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"IIBITS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"bext_h"
 },
 { /* 150 */
#ifdef DEBUG
   /* wnode_name = */		"I4BITS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4BITS,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"JIBITS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"bext_l"
 },
 { /* 151 */
#ifdef DEBUG
   /* wnode_name = */		"I4BITSe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4BITSe,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"JIBITS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"bext_l"
 },
 { /* 152 */
#ifdef DEBUG
   /* wnode_name = */		"I8BITS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8BITS,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"KIBITS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"bext_ll"
 },
 { /* 153 */
#ifdef DEBUG
   /* wnode_name = */		"I8BITSe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8BITSe,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"KIBITS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"bext_ll"
 },
 { /* 154 */
#ifdef DEBUG
   /* wnode_name = */		"I1BSET",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I1BSET,
#endif
   /* return_kind = */		RETURN_I1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"IBSET",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"bset_b"
 },
 { /* 155 */
#ifdef DEBUG
   /* wnode_name = */		"I2BSET",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2BSET,
#endif
   /* return_kind = */		RETURN_I2,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"IIBSET",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"bset_h"
 },
 { /* 156 */
#ifdef DEBUG
   /* wnode_name = */		"I2BSETe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2BSETe,
#endif
   /* return_kind = */		RETURN_I2,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"IIBSET",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"bset_h"
 },
 { /* 157 */
#ifdef DEBUG
   /* wnode_name = */		"I4BSET",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4BSET,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"JIBSET",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"bset_l"
 },
 { /* 158 */
#ifdef DEBUG
   /* wnode_name = */		"I4BSETe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4BSETe,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"JIBSET",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"bset_l"
 },
 { /* 159 */
#ifdef DEBUG
   /* wnode_name = */		"I8BSET",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8BSET,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"KIBSET",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"bset_ll"
 },
 { /* 160 */
#ifdef DEBUG
   /* wnode_name = */		"I8BSETe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8BSETe,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"KIBSET",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"bset_ll"
 },
 { /* 161 */
#ifdef DEBUG
   /* wnode_name = */		"I1BCLR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I1BCLR,
#endif
   /* return_kind = */		RETURN_I1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"IBCLR",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"bclr_b"
 },
 { /* 162 */
#ifdef DEBUG
   /* wnode_name = */		"I2BCLR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2BCLR,
#endif
   /* return_kind = */		RETURN_I2,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"IIBCLR",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"bclr_h"
 },
 { /* 163 */
#ifdef DEBUG
   /* wnode_name = */		"I2BCLRe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2BCLRe,
#endif
   /* return_kind = */		RETURN_I2,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"IIBCLR",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"bclr_h"
 },
 { /* 164 */
#ifdef DEBUG
   /* wnode_name = */		"I4BCLR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4BCLR,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"JIBCLR",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"bclr_l"
 },
 { /* 165 */
#ifdef DEBUG
   /* wnode_name = */		"I4BCLRe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4BCLRe,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"JIBCLR",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"bclr_l"
 },
 { /* 166 */
#ifdef DEBUG
   /* wnode_name = */		"I8BCLR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8BCLR,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"KIBCLR",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"bclr_ll"
 },
 { /* 167 */
#ifdef DEBUG
   /* wnode_name = */		"I8BCLRe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8BCLRe,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"KIBCLR",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"bclr_ll"
 },
 { /* 168 */
#ifdef DEBUG
   /* wnode_name = */		"I1BTEST",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I1BTEST,
#endif
   /* return_kind = */		RETURN_I1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"BTEST",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"btest_b"
 },
 { /* 169 */
#ifdef DEBUG
   /* wnode_name = */		"I2BTEST",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2BTEST,
#endif
   /* return_kind = */		RETURN_I2,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"BITEST",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"btest_h"
 },
 { /* 170 */
#ifdef DEBUG
   /* wnode_name = */		"I2BTESTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2BTESTe,
#endif
   /* return_kind = */		RETURN_I2,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"BITEST",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"btest_h"
 },
 { /* 171 */
#ifdef DEBUG
   /* wnode_name = */		"I4BTEST",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4BTEST,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"BJTEST",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"btest_l"
 },
 { /* 172 */
#ifdef DEBUG
   /* wnode_name = */		"I4BTESTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4BTESTe,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"BJTEST",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"btest_l"
 },
 { /* 173 */
#ifdef DEBUG
   /* wnode_name = */		"I8BTEST",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8BTEST,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"BKTEST",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"btest_ll"
 },
 { /* 174 */
#ifdef DEBUG
   /* wnode_name = */		"I8BTESTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8BTESTe,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"BKTEST",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"btest_ll"
 },
 { /* 175 */
#ifdef DEBUG
   /* wnode_name = */		"I1MVBITS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I1MVBITS,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"MVBITS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"mvbits_byte"
 },
 { /* 176 */
#ifdef DEBUG
   /* wnode_name = */		"I2MVBITS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2MVBITS,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"MVBITS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"mvbits_short"
 },
 { /* 177 */
#ifdef DEBUG
   /* wnode_name = */		"I4MVBITS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4MVBITS,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"MVBITS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"mvbits_long"
 },
 { /* 178 */
#ifdef DEBUG
   /* wnode_name = */		"I8MVBITS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8MVBITS,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"MVBITS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"mvbits_long_long"
 },
 { /* 179 */
#ifdef DEBUG
   /* wnode_name = */		"I1SHL",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I1SHL,
#endif
   /* return_kind = */		RETURN_I1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"_I4SHL",
#else
   /* specific_name = */	"LSHIFT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"shft_b"
 },
 { /* 180 */
#ifdef DEBUG
   /* wnode_name = */		"I2SHL",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2SHL,
#endif
   /* return_kind = */		RETURN_I2,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"_I4SHL",
#else
   /* specific_name = */	"LSHIFT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"shft_h"
 },
 { /* 181 */
#ifdef DEBUG
   /* wnode_name = */		"I1SHR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I1SHR,
#endif
   /* return_kind = */		RETURN_I1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"RSHIFT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"rshft_b"
 },
 { /* 182 */
#ifdef DEBUG
   /* wnode_name = */		"I2SHR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2SHR,
#endif
   /* return_kind = */		RETURN_I2,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"RSHIFT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"rshft_h"
 },
 { /* 183 */
#ifdef DEBUG
   /* wnode_name = */		"I1SHFT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I1SHFT,
#endif
   /* return_kind = */		RETURN_I1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ISHFT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"shft_b"
 },
 { /* 184 */
#ifdef DEBUG
   /* wnode_name = */		"I2SHFT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2SHFT,
#endif
   /* return_kind = */		RETURN_I2,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"IISHFT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"shft_h"
 },
 { /* 185 */
#ifdef DEBUG
   /* wnode_name = */		"I2SHFTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2SHFTe,
#endif
   /* return_kind = */		RETURN_I2,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"IISHFT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"shft_h"
 },
 { /* 186 */
#ifdef DEBUG
   /* wnode_name = */		"I4SHFT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4SHFT,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"JISHFT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"shft_l"
 },
 { /* 187 */
#ifdef DEBUG
   /* wnode_name = */		"I4SHFTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4SHFTe,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"JISHFT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"shft_l"
 },
 { /* 188 */
#ifdef DEBUG
   /* wnode_name = */		"I8SHFT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8SHFT,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"KISHFT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"shft_ll"
 },
 { /* 189 */
#ifdef DEBUG
   /* wnode_name = */		"I8SHFTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8SHFTe,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"KISHFT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"shft_ll"
 },
 { /* 190 */
#ifdef DEBUG
   /* wnode_name = */		"I1SHFTC",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I1SHFTC,
#endif
   /* return_kind = */		RETURN_I1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ISHFTC",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"shftc_b"
 },
 { /* 191 */
#ifdef DEBUG
   /* wnode_name = */		"I2SHFTC",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2SHFTC,
#endif
   /* return_kind = */		RETURN_I2,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"IISHFTC",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"shftc_h"
 },
 { /* 192 */
#ifdef DEBUG
   /* wnode_name = */		"I2SHFTCe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2SHFTCe,
#endif
   /* return_kind = */		RETURN_I2,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"IISHFTC",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"shftc_h"
 },
 { /* 193 */
#ifdef DEBUG
   /* wnode_name = */		"I4SHFTC",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4SHFTC,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"JISHFTC",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"shftc_l"
 },
 { /* 194 */
#ifdef DEBUG
   /* wnode_name = */		"I4SHFTCe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4SHFTCe,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"JISHFTC",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"shftc_l"
 },
 { /* 195 */
#ifdef DEBUG
   /* wnode_name = */		"I8SHFTC",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8SHFTC,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"KISHFTC",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"shftc_ll"
 },
 { /* 196 */
#ifdef DEBUG
   /* wnode_name = */		"I8SHFTCe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8SHFTCe,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"KISHFTC",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"shftc_ll"
 },
 { /* 197 */
#ifdef DEBUG
   /* wnode_name = */		"I4CLEN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4CLEN,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"LEN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"i_len"
 },
 { /* 198 */
#ifdef DEBUG
   /* wnode_name = */		"I4CLENe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4CLENe,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"LEN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"i_len"
 },
 { /* 199 */
#ifdef DEBUG
   /* wnode_name = */		"I4CINDEX",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4CINDEX,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"INDEX",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"i_indx"
 },
 { /* 200 */
#ifdef DEBUG
   /* wnode_name = */		"I4CINDEXe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4CINDEXe,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"INDEX",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"i_indx"
 },
 { /* 201 */
#ifdef DEBUG
   /* wnode_name = */		"CLGE",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_CLGE,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"LGE",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"l_ge"
 },
 { /* 202 */
#ifdef DEBUG
   /* wnode_name = */		"CLGEe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_CLGEe,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"LGE",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"l_ge"
 },
 { /* 203 */
#ifdef DEBUG
   /* wnode_name = */		"CLGT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_CLGT,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"LGT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"l_gt"
 },
 { /* 204 */
#ifdef DEBUG
   /* wnode_name = */		"CLGTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_CLGTe,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"LGT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"l_gt"
 },
 { /* 205 */
#ifdef DEBUG
   /* wnode_name = */		"CLLE",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_CLLE,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"LLE",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"l_le"
 },
 { /* 206 */
#ifdef DEBUG
   /* wnode_name = */		"CLLEe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_CLLEe,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"LLE",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"l_le"
 },
 { /* 207 */
#ifdef DEBUG
   /* wnode_name = */		"CLLT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_CLLT,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"LLT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"l_lt"
 },
 { /* 208 */
#ifdef DEBUG
   /* wnode_name = */		"CLLTe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_CLLTe,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"LLT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"l_lt"
 },
 { /* 209 */
#ifdef DEBUG
   /* wnode_name = */		"F4EXP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4EXP,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"EXP",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"expf"
 },
 { /* 210 */
#ifdef DEBUG
   /* wnode_name = */		"F4EXPe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4EXPe,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"EXP",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"r_exp"
 },
 { /* 211 */
#ifdef DEBUG
   /* wnode_name = */		"F8EXP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8EXP,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DEXP",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"exp"
 },
 { /* 212 */
#ifdef DEBUG
   /* wnode_name = */		"F8EXPe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8EXPe,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DEXP",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"d_exp"
 },
 { /* 213 */
#ifdef DEBUG
   /* wnode_name = */		"FQEXP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQEXP,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QEXP",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__qexp"
 },
 { /* 214 */
#ifdef DEBUG
   /* wnode_name = */		"FQEXPe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQEXPe,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QEXP",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__q_exp"
 },
 { /* 215 */
#ifdef DEBUG
   /* wnode_name = */		"C4EXP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C4EXP,
#endif
   /* return_kind = */		RETURN_C4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"CEXP",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__cexp"
 },
 { /* 216 */
#ifdef DEBUG
   /* wnode_name = */		"C4EXPe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C4EXPe,
#endif
   /* return_kind = */		RETURN_C4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"CEXP",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"c_exp_"
 },
 { /* 217 */
#ifdef DEBUG
   /* wnode_name = */		"C8EXP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C8EXP,
#endif
   /* return_kind = */		RETURN_C8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ZEXP",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__zexp"
 },
 { /* 218 */
#ifdef DEBUG
   /* wnode_name = */		"C8EXPe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C8EXPe,
#endif
   /* return_kind = */		RETURN_C8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ZEXP",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"z_exp_"
 },
 { /* 219 */
#ifdef DEBUG
   /* wnode_name = */		"CQEXP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_CQEXP,
#endif
   /* return_kind = */		RETURN_CQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"CQEXP",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__cqexp"
 },
 { /* 220 */
#ifdef DEBUG
   /* wnode_name = */		"CQEXPe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_CQEXPe,
#endif
   /* return_kind = */		RETURN_CQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"CQEXP",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__cq_exp"
 },
 { /* 221 */
#ifdef DEBUG
   /* wnode_name = */		"F4LOG",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4LOG,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ALOG",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"logf"
 },
 { /* 222 */
#ifdef DEBUG
   /* wnode_name = */		"F4LOGe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4LOGe,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ALOG",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"r_log"
 },
 { /* 223 */
#ifdef DEBUG
   /* wnode_name = */		"F8LOG",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8LOG,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DLOG",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"log"
 },
 { /* 224 */
#ifdef DEBUG
   /* wnode_name = */		"F8LOGe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8LOGe,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DLOG",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"d_log"
 },
 { /* 225 */
#ifdef DEBUG
   /* wnode_name = */		"FQLOG",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQLOG,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QLOG",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__qlog"
 },
 { /* 226 */
#ifdef DEBUG
   /* wnode_name = */		"FQLOGe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQLOGe,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QLOG",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__q_log"
 },
 { /* 227 */
#ifdef DEBUG
   /* wnode_name = */		"C4LOG",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C4LOG,
#endif
   /* return_kind = */		RETURN_C4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"CLOG",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__clog"
 },
 { /* 228 */
#ifdef DEBUG
   /* wnode_name = */		"C4LOGe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C4LOGe,
#endif
   /* return_kind = */		RETURN_C4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"CLOG",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"c_log_"
 },
 { /* 229 */
#ifdef DEBUG
   /* wnode_name = */		"C8LOG",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C8LOG,
#endif
   /* return_kind = */		RETURN_C8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ZLOG",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__zlog"
 },
 { /* 230 */
#ifdef DEBUG
   /* wnode_name = */		"C8LOGe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C8LOGe,
#endif
   /* return_kind = */		RETURN_C8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ZLOG",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"z_log_"
 },
 { /* 231 */
#ifdef DEBUG
   /* wnode_name = */		"CQLOG",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_CQLOG,
#endif
   /* return_kind = */		RETURN_CQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"CQLOG",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__cqlog"
 },
 { /* 232 */
#ifdef DEBUG
   /* wnode_name = */		"CQLOGe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_CQLOGe,
#endif
   /* return_kind = */		RETURN_CQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"CQLOG",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__cq_log"
 },
 { /* 233 */
#ifdef DEBUG
   /* wnode_name = */		"F4LOG10",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4LOG10,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ALOG10",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"log10f"
 },
 { /* 234 */
#ifdef DEBUG
   /* wnode_name = */		"F4LOG10e",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4LOG10e,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ALOG10",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"r_lg10"
 },
 { /* 235 */
#ifdef DEBUG
   /* wnode_name = */		"F8LOG10",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8LOG10,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DLOG10",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"log10"
 },
 { /* 236 */
#ifdef DEBUG
   /* wnode_name = */		"F8LOG10e",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8LOG10e,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DLOG10",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"d_lg10"
 },
 { /* 237 */
#ifdef DEBUG
   /* wnode_name = */		"FQLOG10",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQLOG10,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QLOG10",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"log10l"
 },
 { /* 238 */
#ifdef DEBUG
   /* wnode_name = */		"FQLOG10e",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQLOG10e,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QLOG10",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__q_lg10"
 },
 { /* 239 */
#ifdef DEBUG
   /* wnode_name = */		"F4COS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4COS,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"COS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"cosf"
 },
 { /* 240 */
#ifdef DEBUG
   /* wnode_name = */		"F4COSe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4COSe,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"COS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"r_cos"
 },
 { /* 241 */
#ifdef DEBUG
   /* wnode_name = */		"F8COS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8COS,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DCOS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"cos"
 },
 { /* 242 */
#ifdef DEBUG
   /* wnode_name = */		"F8COSe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8COSe,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DCOS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"d_cos"
 },
 { /* 243 */
#ifdef DEBUG
   /* wnode_name = */		"FQCOS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQCOS,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QCOS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__qcos"
 },
 { /* 244 */
#ifdef DEBUG
   /* wnode_name = */		"FQCOSe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQCOSe,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QCOS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__q_cos"
 },
 { /* 245 */
#ifdef DEBUG
   /* wnode_name = */		"C4COS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C4COS,
#endif
   /* return_kind = */		RETURN_C4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"CCOS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__ccos"
 },
 { /* 246 */
#ifdef DEBUG
   /* wnode_name = */		"C4COSe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C4COSe,
#endif
   /* return_kind = */		RETURN_C4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"CCOS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"c_cos_"
 },
 { /* 247 */
#ifdef DEBUG
   /* wnode_name = */		"C8COS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C8COS,
#endif
   /* return_kind = */		RETURN_C8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ZCOS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__zcos"
 },
 { /* 248 */
#ifdef DEBUG
   /* wnode_name = */		"C8COSe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C8COSe,
#endif
   /* return_kind = */		RETURN_C8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ZCOS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"z_cos_"
 },
 { /* 249 */
#ifdef DEBUG
   /* wnode_name = */		"CQCOS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_CQCOS,
#endif
   /* return_kind = */		RETURN_CQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"CQCOS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__cqcos"
 },
 { /* 250 */
#ifdef DEBUG
   /* wnode_name = */		"CQCOSe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_CQCOSe,
#endif
   /* return_kind = */		RETURN_CQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"CQCOS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__cq_cos"
 },
 { /* 251 */
#ifdef DEBUG
   /* wnode_name = */		"F4SIN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4SIN,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"SIN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"sinf"
 },
 { /* 252 */
#ifdef DEBUG
   /* wnode_name = */		"F4SINe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4SINe,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"SIN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"r_sin"
 },
 { /* 253 */
#ifdef DEBUG
   /* wnode_name = */		"F8SIN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8SIN,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DSIN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"sin"
 },
 { /* 254 */
#ifdef DEBUG
   /* wnode_name = */		"F8SINe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8SINe,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DSIN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"d_sin"
 },
 { /* 255 */
#ifdef DEBUG
   /* wnode_name = */		"FQSIN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQSIN,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QSIN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__qsin"
 },
 { /* 256 */
#ifdef DEBUG
   /* wnode_name = */		"FQSINe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQSINe,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QSIN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__q_sin"
 },
 { /* 257 */
#ifdef DEBUG
   /* wnode_name = */		"C4SIN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C4SIN,
#endif
   /* return_kind = */		RETURN_C4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"CSIN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__csin"
 },
 { /* 258 */
#ifdef DEBUG
   /* wnode_name = */		"C4SINe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C4SINe,
#endif
   /* return_kind = */		RETURN_C4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"CSIN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"c_sin_"
 },
 { /* 259 */
#ifdef DEBUG
   /* wnode_name = */		"C8SIN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C8SIN,
#endif
   /* return_kind = */		RETURN_C8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ZSIN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__zsin"
 },
 { /* 260 */
#ifdef DEBUG
   /* wnode_name = */		"C8SINe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C8SINe,
#endif
   /* return_kind = */		RETURN_C8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ZSIN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"z_sin_"
 },
 { /* 261 */
#ifdef DEBUG
   /* wnode_name = */		"CQSIN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_CQSIN,
#endif
   /* return_kind = */		RETURN_CQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"CQSIN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__cqsin"
 },
 { /* 262 */
#ifdef DEBUG
   /* wnode_name = */		"CQSINe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_CQSINe,
#endif
   /* return_kind = */		RETURN_CQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"CQSIN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__cq_sin"
 },
 { /* 263 */
#ifdef DEBUG
   /* wnode_name = */		"F4CIS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4CIS,
#endif
   /* return_kind = */		RETURN_C4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"CIS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__rcis"
 },
 { /* 264 */
#ifdef DEBUG
   /* wnode_name = */		"F4CISe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4CISe,
#endif
   /* return_kind = */		RETURN_C4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"CIS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"r_cis"
 },
 { /* 265 */
#ifdef DEBUG
   /* wnode_name = */		"F8CIS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8CIS,
#endif
   /* return_kind = */		RETURN_C8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DCIS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__dcis"
 },
 { /* 266 */
#ifdef DEBUG
   /* wnode_name = */		"F8CISe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8CISe,
#endif
   /* return_kind = */		RETURN_C8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DCIS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"d_cis"
 },
 { /* 267 */
#ifdef DEBUG
   /* wnode_name = */		"FQCIS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQCIS,
#endif
   /* return_kind = */		RETURN_CQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QCIS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__qcis"
 },
 { /* 268 */
#ifdef DEBUG
   /* wnode_name = */		"FQCISe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQCISe,
#endif
   /* return_kind = */		RETURN_CQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QCIS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"q_cis"
 },
 { /* 269 */
#ifdef DEBUG
   /* wnode_name = */		"F4TAN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4TAN,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"TAN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"tanf"
 },
 { /* 270 */
#ifdef DEBUG
   /* wnode_name = */		"F4TANe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4TANe,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"TAN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"r_tan"
 },
 { /* 271 */
#ifdef DEBUG
   /* wnode_name = */		"F8TAN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8TAN,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DTAN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"tan"
 },
 { /* 272 */
#ifdef DEBUG
   /* wnode_name = */		"F8TANe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8TANe,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DTAN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"d_tan"
 },
 { /* 273 */
#ifdef DEBUG
   /* wnode_name = */		"FQTAN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQTAN,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QTAN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__qtan"
 },
 { /* 274 */
#ifdef DEBUG
   /* wnode_name = */		"FQTANe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQTANe,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QTAN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__q_tan"
 },
 { /* 275 */
#ifdef DEBUG
   /* wnode_name = */		"F4COSD",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4COSD,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"COSD",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__rcosd"
 },
 { /* 276 */
#ifdef DEBUG
   /* wnode_name = */		"F4COSDe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4COSDe,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"COSD",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"r_cosd"
 },
 { /* 277 */
#ifdef DEBUG
   /* wnode_name = */		"F8COSD",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8COSD,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DCOSD",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__dcosd"
 },
 { /* 278 */
#ifdef DEBUG
   /* wnode_name = */		"F8COSDe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8COSDe,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DCOSD",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"d_cosd"
 },
 { /* 279 */
#ifdef DEBUG
   /* wnode_name = */		"FQCOSD",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQCOSD,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QCOSD",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__q_cosd"
 },
 { /* 280 */
#ifdef DEBUG
   /* wnode_name = */		"FQCOSDe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQCOSDe,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QCOSD",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__q_cosd"
 },
 { /* 281 */
#ifdef DEBUG
   /* wnode_name = */		"F4SIND",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4SIND,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"SIND",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__rsind"
 },
 { /* 282 */
#ifdef DEBUG
   /* wnode_name = */		"F4SINDe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4SINDe,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"SIND",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"r_sind"
 },
 { /* 283 */
#ifdef DEBUG
   /* wnode_name = */		"F8SIND",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8SIND,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DSIND",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__dsind"
 },
 { /* 284 */
#ifdef DEBUG
   /* wnode_name = */		"F8SINDe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8SINDe,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DSIND",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"d_sind"
 },
 { /* 285 */
#ifdef DEBUG
   /* wnode_name = */		"FQSIND",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQSIND,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QSIND",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__q_sind"
 },
 { /* 286 */
#ifdef DEBUG
   /* wnode_name = */		"FQSINDe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQSINDe,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QSIND",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__q_sind"
 },
 { /* 287 */
#ifdef DEBUG
   /* wnode_name = */		"F4TAND",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4TAND,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"TAND",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__rtand"
 },
 { /* 288 */
#ifdef DEBUG
   /* wnode_name = */		"F4TANDe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4TANDe,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"TAND",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"r_tand"
 },
 { /* 289 */
#ifdef DEBUG
   /* wnode_name = */		"F8TAND",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8TAND,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DTAND",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__dtand"
 },
 { /* 290 */
#ifdef DEBUG
   /* wnode_name = */		"F8TANDe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8TANDe,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DTAND",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"d_tand"
 },
 { /* 291 */
#ifdef DEBUG
   /* wnode_name = */		"FQTAND",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQTAND,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QTAND",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__q_tand"
 },
 { /* 292 */
#ifdef DEBUG
   /* wnode_name = */		"FQTANDe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQTANDe,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QTAND",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__q_tand"
 },
 { /* 293 */
#ifdef DEBUG
   /* wnode_name = */		"F4COSH",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4COSH,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"COSH",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"coshf"
 },
 { /* 294 */
#ifdef DEBUG
   /* wnode_name = */		"F4COSHe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4COSHe,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"COSH",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"r_cosh"
 },
 { /* 295 */
#ifdef DEBUG
   /* wnode_name = */		"F8COSH",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8COSH,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DCOSH",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"cosh"
 },
 { /* 296 */
#ifdef DEBUG
   /* wnode_name = */		"F8COSHe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8COSHe,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DCOSH",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"d_cosh"
 },
 { /* 297 */
#ifdef DEBUG
   /* wnode_name = */		"FQCOSH",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQCOSH,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QCOSH",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__qcosh"
 },
 { /* 298 */
#ifdef DEBUG
   /* wnode_name = */		"FQCOSHe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQCOSHe,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QCOSH",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__q_cosh"
 },
 { /* 299 */
#ifdef DEBUG
   /* wnode_name = */		"F4SINH",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4SINH,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"SINH",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"sinhf"
 },
 { /* 300 */
#ifdef DEBUG
   /* wnode_name = */		"F4SINHe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4SINHe,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"SINH",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"r_sinh"
 },
 { /* 301 */
#ifdef DEBUG
   /* wnode_name = */		"F8SINH",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8SINH,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DSINH",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"sinh"
 },
 { /* 302 */
#ifdef DEBUG
   /* wnode_name = */		"F8SINHe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8SINHe,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DSINH",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"d_sinh"
 },
 { /* 303 */
#ifdef DEBUG
   /* wnode_name = */		"FQSINH",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQSINH,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QSINH",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__qsinh"
 },
 { /* 304 */
#ifdef DEBUG
   /* wnode_name = */		"FQSINHe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQSINHe,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QSINH",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__q_sinh"
 },
 { /* 305 */
#ifdef DEBUG
   /* wnode_name = */		"F4TANH",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4TANH,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"TANH",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"tanhf"
 },
 { /* 306 */
#ifdef DEBUG
   /* wnode_name = */		"F4TANHe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4TANHe,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"TANH",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"r_tanh"
 },
 { /* 307 */
#ifdef DEBUG
   /* wnode_name = */		"F8TANH",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8TANH,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DTANH",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"tanh"
 },
 { /* 308 */
#ifdef DEBUG
   /* wnode_name = */		"F8TANHe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8TANHe,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DTANH",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"d_tanh"
 },
 { /* 309 */
#ifdef DEBUG
   /* wnode_name = */		"FQTANH",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQTANH,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QTANH",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__qtanh"
 },
 { /* 310 */
#ifdef DEBUG
   /* wnode_name = */		"FQTANHe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQTANHe,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QTANH",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__q_tanh"
 },
 { /* 311 */
#ifdef DEBUG
   /* wnode_name = */		"F4ACOS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4ACOS,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ACOS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"acosf"
 },
 { /* 312 */
#ifdef DEBUG
   /* wnode_name = */		"F4ACOSe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4ACOSe,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ACOS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"r_acos"
 },
 { /* 313 */
#ifdef DEBUG
   /* wnode_name = */		"F8ACOS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8ACOS,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DACOS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"acos"
 },
 { /* 314 */
#ifdef DEBUG
   /* wnode_name = */		"F8ACOSe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8ACOSe,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DACOS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"d_acos"
 },
 { /* 315 */
#ifdef DEBUG
   /* wnode_name = */		"FQACOS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQACOS,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QACOS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__qacos"
 },
 { /* 316 */
#ifdef DEBUG
   /* wnode_name = */		"FQACOSe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQACOSe,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QACOS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__q_acos"
 },
 { /* 317 */
#ifdef DEBUG
   /* wnode_name = */		"F4ASIN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4ASIN,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ASIN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"asinf"
 },
 { /* 318 */
#ifdef DEBUG
   /* wnode_name = */		"F4ASINe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4ASINe,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ASIN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"r_asin"
 },
 { /* 319 */
#ifdef DEBUG
   /* wnode_name = */		"F8ASIN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8ASIN,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DASIN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"asin"
 },
 { /* 320 */
#ifdef DEBUG
   /* wnode_name = */		"F8ASINe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8ASINe,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DASIN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"d_asin"
 },
 { /* 321 */
#ifdef DEBUG
   /* wnode_name = */		"FQASIN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQASIN,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QASIN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__qasin"
 },
 { /* 322 */
#ifdef DEBUG
   /* wnode_name = */		"FQASINe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQASINe,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QASIN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__q_asin"
 },
 { /* 323 */
#ifdef DEBUG
   /* wnode_name = */		"F4ATAN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4ATAN,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ATAN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"atanf"
 },
 { /* 324 */
#ifdef DEBUG
   /* wnode_name = */		"F4ATANe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4ATANe,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ATAN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"r_atan"
 },
 { /* 325 */
#ifdef DEBUG
   /* wnode_name = */		"F8ATAN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8ATAN,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DATAN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"atan"
 },
 { /* 326 */
#ifdef DEBUG
   /* wnode_name = */		"F8ATANe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8ATANe,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DATAN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"d_atan"
 },
 { /* 327 */
#ifdef DEBUG
   /* wnode_name = */		"FQATAN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQATAN,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QATAN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__qatan"
 },
 { /* 328 */
#ifdef DEBUG
   /* wnode_name = */		"FQATANe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQATANe,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QATAN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__q_atan"
 },
 { /* 329 */
#ifdef DEBUG
   /* wnode_name = */		"F4ACOSD",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4ACOSD,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ACOSD",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__racosd"
 },
 { /* 330 */
#ifdef DEBUG
   /* wnode_name = */		"F4ACOSDe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4ACOSDe,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ACOSD",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"r_acosd"
 },
 { /* 331 */
#ifdef DEBUG
   /* wnode_name = */		"F8ACOSD",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8ACOSD,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DACOSD",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__dacosd"
 },
 { /* 332 */
#ifdef DEBUG
   /* wnode_name = */		"F8ACOSDe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8ACOSDe,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DACOSD",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"d_acosd"
 },
 { /* 333 */
#ifdef DEBUG
   /* wnode_name = */		"FQACOSD",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQACOSD,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QACOSD",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__q_acosd"
 },
 { /* 334 */
#ifdef DEBUG
   /* wnode_name = */		"FQACOSDe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQACOSDe,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QACOSD",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__q_acosd"
 },
 { /* 335 */
#ifdef DEBUG
   /* wnode_name = */		"F4ASIND",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4ASIND,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ASIND",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__rasind"
 },
 { /* 336 */
#ifdef DEBUG
   /* wnode_name = */		"F4ASINDe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4ASINDe,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ASIND",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"r_asind"
 },
 { /* 337 */
#ifdef DEBUG
   /* wnode_name = */		"F8ASIND",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8ASIND,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DASIND",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__dasind"
 },
 { /* 338 */
#ifdef DEBUG
   /* wnode_name = */		"F8ASINDe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8ASINDe,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DASIND",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"d_asind"
 },
 { /* 339 */
#ifdef DEBUG
   /* wnode_name = */		"FQASIND",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQASIND,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QASIND",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__q_asind"
 },
 { /* 340 */
#ifdef DEBUG
   /* wnode_name = */		"FQASINDe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQASINDe,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QASIND",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__q_asind"
 },
 { /* 341 */
#ifdef DEBUG
   /* wnode_name = */		"F4ATAND",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4ATAND,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ATAND",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__ratand"
 },
 { /* 342 */
#ifdef DEBUG
   /* wnode_name = */		"F4ATANDe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4ATANDe,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ATAND",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"r_atand"
 },
 { /* 343 */
#ifdef DEBUG
   /* wnode_name = */		"F8ATAND",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8ATAND,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DATAND",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__datand"
 },
 { /* 344 */
#ifdef DEBUG
   /* wnode_name = */		"F8ATANDe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8ATANDe,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DATAND",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"d_atand"
 },
 { /* 345 */
#ifdef DEBUG
   /* wnode_name = */		"FQATAND",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQATAND,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QATAND",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__q_atand"
 },
 { /* 346 */
#ifdef DEBUG
   /* wnode_name = */		"FQATANDe",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQATANDe,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QATAND",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__q_atand"
 },
 { /* 347 */
#ifdef DEBUG
   /* wnode_name = */		"F4ATAN2",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4ATAN2,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ATAN2",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"atan2f"
 },
 { /* 348 */
#ifdef DEBUG
   /* wnode_name = */		"F4ATAN2e",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4ATAN2e,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ATAN2",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"r_atn2"
 },
 { /* 349 */
#ifdef DEBUG
   /* wnode_name = */		"F8ATAN2",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8ATAN2,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DATAN2",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"atan2"
 },
 { /* 350 */
#ifdef DEBUG
   /* wnode_name = */		"F8ATAN2e",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8ATAN2e,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DATAN2",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"d_atn2"
 },
 { /* 351 */
#ifdef DEBUG
   /* wnode_name = */		"FQATAN2",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQATAN2,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QATAN2",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__qatan2"
 },
 { /* 352 */
#ifdef DEBUG
   /* wnode_name = */		"FQATAN2e",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQATAN2e,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QATAN2",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__q_atn2"
 },
 { /* 353 */
#ifdef DEBUG
   /* wnode_name = */		"F4ATAN2D",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4ATAN2D,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ATAN2D",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__ratn2d"
 },
 { /* 354 */
#ifdef DEBUG
   /* wnode_name = */		"F4ATAN2De",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4ATAN2De,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ATAN2D",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"r_atn2d"
 },
 { /* 355 */
#ifdef DEBUG
   /* wnode_name = */		"F8ATAN2D",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8ATAN2D,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DATAN2D",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__datn2d"
 },
 { /* 356 */
#ifdef DEBUG
   /* wnode_name = */		"F8ATAN2De",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8ATAN2De,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DATAN2D",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"d_atn2d"
 },
 { /* 357 */
#ifdef DEBUG
   /* wnode_name = */		"FQATAN2D",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQATAN2D,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QATAN2D",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__qatan2d"
 },
 { /* 358 */
#ifdef DEBUG
   /* wnode_name = */		"FQATAN2De",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQATAN2De,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"QATAN2D",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	TRUE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__q_atn2d"
 },
 { /* 359 */
#ifdef DEBUG
   /* wnode_name = */		"U4I4ALLOCA",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U4I4ALLOCA,
#endif
   /* return_kind = */		RETURN_PV,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__builtin_alloca",
#else
   /* specific_name = */	"ALLOCA",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 360 */
#ifdef DEBUG
   /* wnode_name = */		"U8I8ALLOCA",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U8I8ALLOCA,
#endif
   /* return_kind = */		RETURN_PV,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__builtin_alloca",
#else
   /* specific_name = */	"ALLOCA",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 361 */
#ifdef DEBUG
   /* wnode_name = */		"U4I4MALLOC",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U4I4MALLOC,
#endif
   /* return_kind = */		RETURN_PV,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"MALLOC",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"malloc"
 },
 { /* 362 */
#ifdef DEBUG
   /* wnode_name = */		"U8I8MALLOC",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U8I8MALLOC,
#endif
   /* return_kind = */		RETURN_PV,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"MALLOC",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"malloc"
 },
 { /* 363 */
#ifdef DEBUG
   /* wnode_name = */		"U4FREE",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U4FREE,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"FREE",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"free"
 },
 { /* 364 */
#ifdef DEBUG
   /* wnode_name = */		"U8FREE",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U8FREE,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"FREE",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"free"
 },
 { /* 365 */
#ifdef DEBUG
   /* wnode_name = */		"MDATE",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_MDATE,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DATE",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"date_vms"
 },
 { /* 366 */
#ifdef DEBUG
   /* wnode_name = */		"I1DATE",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I1DATE,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"IDATE",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"idate_byte"
 },
 { /* 367 */
#ifdef DEBUG
   /* wnode_name = */		"I2DATE",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2DATE,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"IDATE",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"idate_short"
 },
 { /* 368 */
#ifdef DEBUG
   /* wnode_name = */		"I4DATE",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4DATE,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"IDATE",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"idate_long"
 },
 { /* 369 */
#ifdef DEBUG
   /* wnode_name = */		"I8DATE",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8DATE,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"IDATE",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"idate_long_long"
 },
 { /* 370 */
#ifdef DEBUG
   /* wnode_name = */		"I1ERRSNS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I1ERRSNS,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ERRSNS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"errsns_byte"
 },
 { /* 371 */
#ifdef DEBUG
   /* wnode_name = */		"I2ERRSNS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2ERRSNS,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ERRSNS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"errsns_short"
 },
 { /* 372 */
#ifdef DEBUG
   /* wnode_name = */		"I4ERRSNS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4ERRSNS,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ERRSNS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"errsns_long"
 },
 { /* 373 */
#ifdef DEBUG
   /* wnode_name = */		"I8ERRSNS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8ERRSNS,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ERRSNS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"errsns_long_long"
 },
 { /* 374 */
#ifdef DEBUG
   /* wnode_name = */		"VEXIT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_VEXIT,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"EXIT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	TRUE,
   /* runtime_name = */		"exit_noargs"
 },
 { /* 375 */
#ifdef DEBUG
   /* wnode_name = */		"I1EXIT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I1EXIT,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"EXIT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	TRUE,
   /* runtime_name = */		"exit_byte"
 },
 { /* 376 */
#ifdef DEBUG
   /* wnode_name = */		"I2EXIT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2EXIT,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"EXIT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	TRUE,
   /* runtime_name = */		"exit_short"
 },
 { /* 377 */
#ifdef DEBUG
   /* wnode_name = */		"I4EXIT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4EXIT,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"EXIT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	TRUE,
   /* runtime_name = */		"exit_long"
 },
 { /* 378 */
#ifdef DEBUG
   /* wnode_name = */		"I8EXIT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8EXIT,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"EXIT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	TRUE,
   /* runtime_name = */		"exit_long_long"
 },
 { /* 379 */
#ifdef DEBUG
   /* wnode_name = */		"TIME",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_TIME,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"TIME",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"time_vms"
 },
 { /* 380 */
#ifdef DEBUG
   /* wnode_name = */		"F4SECNDS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4SECNDS,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"SECNDS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"secnds_vms"
 },
 { /* 381 */
#ifdef DEBUG
   /* wnode_name = */		"F8SECNDS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8SECNDS,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"SECNDS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"dsecnds_vms"
 },
 { /* 382 */
#ifdef DEBUG
   /* wnode_name = */		"PAUSE",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_PAUSE,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"PAUSE",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"s_paus"
 },
 { /* 383 */
#ifdef DEBUG
   /* wnode_name = */		"STOP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_STOP,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"STOP",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	TRUE,
   /* runtime_name = */		"s_stop"
 },
 { /* 384 */
#ifdef DEBUG
   /* wnode_name = */		"F4I4RAN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4I4RAN,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"_RANF_4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_RANF_4"
 },
 { /* 385 */
#ifdef DEBUG
   /* wnode_name = */		"F4I8RAN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4I8RAN,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 386 */
#ifdef DEBUG
   /* wnode_name = */		"F8I4RAN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8I4RAN,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"_RANF_8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_RANF_8"
 },
 { /* 387 */
#ifdef DEBUG
   /* wnode_name = */		"F8I8RAN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8I8RAN,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 388 */
#ifdef DEBUG
   /* wnode_name = */		"FQI4RAN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQI4RAN,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 389 */
#ifdef DEBUG
   /* wnode_name = */		"FQI8RAN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQI8RAN,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 390 */
#ifdef DEBUG
   /* wnode_name = */		"I4DIVFLOOR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4DIVFLOOR,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"_I4DIVFLOOR",
#else
   /* specific_name = */	"INTRN_I4DIVFLOOR",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 391 */
#ifdef DEBUG
   /* wnode_name = */		"I8DIVFLOOR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8DIVFLOOR,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"_I8DIVFLOOR",
#else
   /* specific_name = */	"INTRN_I8DIVFLOOR",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 392 */
#ifdef DEBUG
   /* wnode_name = */		"U4DIVFLOOR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U4DIVFLOOR,
#endif
   /* return_kind = */		RETURN_U4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"_U4DIVFLOOR",
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 393 */
#ifdef DEBUG
   /* wnode_name = */		"U8DIVFLOOR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U8DIVFLOOR,
#endif
   /* return_kind = */		RETURN_U8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"_U8DIVFLOOR",
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 394 */
#ifdef DEBUG
   /* wnode_name = */		"I4DIVCEIL",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4DIVCEIL,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"_I4DIVCEIL",
#else
   /* specific_name = */	"INTRN_I4DIVCEIL",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 395 */
#ifdef DEBUG
   /* wnode_name = */		"I8DIVCEIL",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8DIVCEIL,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"_I8DIVCEIL",
#else
   /* specific_name = */	"INTRN_I8DIVCEIL",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 396 */
#ifdef DEBUG
   /* wnode_name = */		"U4DIVCEIL",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U4DIVCEIL,
#endif
   /* return_kind = */		RETURN_U4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"_U4DIVCEIL",
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 397 */
#ifdef DEBUG
   /* wnode_name = */		"U8DIVCEIL",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U8DIVCEIL,
#endif
   /* return_kind = */		RETURN_U8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"_U8DIVCEIL",
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 398 */
#ifdef DEBUG
   /* wnode_name = */		"I4MODFLOOR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4MODFLOOR,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"_I4MODFLOOR",
#else
   /* specific_name = */	"INTRN_I4MODFLOOR",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 399 */
#ifdef DEBUG
   /* wnode_name = */		"I8MODFLOOR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8MODFLOOR,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"_I8MODFLOOR",
#else
   /* specific_name = */	"INTRN_I8MODFLOOR",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 400 */
#ifdef DEBUG
   /* wnode_name = */		"U4MODFLOOR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U4MODFLOOR,
#endif
   /* return_kind = */		RETURN_U4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"_U4MODFLOOR",
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 401 */
#ifdef DEBUG
   /* wnode_name = */		"U8MODFLOOR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U8MODFLOOR,
#endif
   /* return_kind = */		RETURN_U8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"_U8MODFLOOR",
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 402 */
#ifdef DEBUG
   /* wnode_name = */		"I4MODCEIL",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4MODCEIL,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"_I4MODCEIL",
#else
   /* specific_name = */	"INTRN_I4MODCEIL",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 403 */
#ifdef DEBUG
   /* wnode_name = */		"I8MODCEIL",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8MODCEIL,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"_I8MODCEIL",
#else
   /* specific_name = */	"INTRN_I8MODCEIL",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 404 */
#ifdef DEBUG
   /* wnode_name = */		"U4MODCEIL",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U4MODCEIL,
#endif
   /* return_kind = */		RETURN_U4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"_U4MODCEIL",
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 405 */
#ifdef DEBUG
   /* wnode_name = */		"U8MODCEIL",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U8MODCEIL,
#endif
   /* return_kind = */		RETURN_U8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"_U8MODCEIL",
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 406 */
#ifdef DEBUG
   /* wnode_name = */		"U4I4SETSTACKPOINTER",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U4I4SETSTACKPOINTER,
#endif
   /* return_kind = */		RETURN_PV,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__builtin_setstackpointer",
#else
   /* specific_name = */	"INTRN_U4I4SETSTACKPOINTER",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 407 */
#ifdef DEBUG
   /* wnode_name = */		"U8I8SETSTACKPOINTER",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U8I8SETSTACKPOINTER,
#endif
   /* return_kind = */		RETURN_PV,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__builtin_setstackpointer",
#else
   /* specific_name = */	"INTRN_U8I8SETSTACKPOINTER",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 408 */
#ifdef DEBUG
   /* wnode_name = */		"U4READSTACKPOINTER",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U4READSTACKPOINTER,
#endif
   /* return_kind = */		RETURN_PV,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__builtin_readstackpointer",
#else
   /* specific_name = */	"INTRN_U4READSTACKPOINTER",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 409 */
#ifdef DEBUG
   /* wnode_name = */		"U8READSTACKPOINTER",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U8READSTACKPOINTER,
#endif
   /* return_kind = */		RETURN_PV,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__builtin_readstackpointer",
#else
   /* specific_name = */	"INTRN_U8READSTACKPOINTER",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 410 */
#ifdef DEBUG
   /* wnode_name = */		"ADD_AND_FETCH_I4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_ADD_AND_FETCH_I4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__add_and_fetch",
#else
   /* specific_name = */	"ADD_AND_FETCH_I4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 411 */
#ifdef DEBUG
   /* wnode_name = */		"SUB_AND_FETCH_I4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_SUB_AND_FETCH_I4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__sub_and_fetch",
#else
   /* specific_name = */	"SUB_AND_FETCH_I4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 412 */
#ifdef DEBUG
   /* wnode_name = */		"OR_AND_FETCH_I4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_OR_AND_FETCH_I4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__or_and_fetch",
#else
   /* specific_name = */	"OR_AND_FETCH_I4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 413 */
#ifdef DEBUG
   /* wnode_name = */		"XOR_AND_FETCH_I4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_XOR_AND_FETCH_I4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__xor_and_fetch",
#else
   /* specific_name = */	"XOR_AND_FETCH_I4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 414 */
#ifdef DEBUG
   /* wnode_name = */		"AND_AND_FETCH_I4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_AND_AND_FETCH_I4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__and_and_fetch",
#else
   /* specific_name = */	"AND_AND_FETCH_I4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 415 */
#ifdef DEBUG
   /* wnode_name = */		"NAND_AND_FETCH_I4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_NAND_AND_FETCH_I4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__nand_and_fetch",
#else
   /* specific_name = */	"NAND_AND_FETCH_I4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 416 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_ADD_I4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_ADD_I4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_add",
#else
   /* specific_name = */	"FETCH_AND_ADD_I4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 417 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_SUB_I4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_SUB_I4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_sub",
#else
   /* specific_name = */	"FETCH_AND_SUB_I4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 418 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_OR_I4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_OR_I4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_or",
#else
   /* specific_name = */	"FETCH_AND_OR_I4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 419 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_XOR_I4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_XOR_I4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_xor",
#else
   /* specific_name = */	"FETCH_AND_XOR_I4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 420 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_AND_I4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_AND_I4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_and",
#else
   /* specific_name = */	"FETCH_AND_AND_I4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 421 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_NAND_I4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_NAND_I4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_nand",
#else
   /* specific_name = */	"FETCH_AND_NAND_I4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 422 */
#ifdef DEBUG
   /* wnode_name = */		"ADD_AND_FETCH_I8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_ADD_AND_FETCH_I8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__add_and_fetch",
#else
   /* specific_name = */	"ADD_AND_FETCH_I8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 423 */
#ifdef DEBUG
   /* wnode_name = */		"SUB_AND_FETCH_I8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_SUB_AND_FETCH_I8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__sub_and_fetch",
#else
   /* specific_name = */	"SUB_AND_FETCH_I8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 424 */
#ifdef DEBUG
   /* wnode_name = */		"OR_AND_FETCH_I8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_OR_AND_FETCH_I8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__or_and_fetch",
#else
   /* specific_name = */	"OR_AND_FETCH_I8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 425 */
#ifdef DEBUG
   /* wnode_name = */		"XOR_AND_FETCH_I8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_XOR_AND_FETCH_I8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__xor_and_fetch",
#else
   /* specific_name = */	"XOR_AND_FETCH_I8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 426 */
#ifdef DEBUG
   /* wnode_name = */		"AND_AND_FETCH_I8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_AND_AND_FETCH_I8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__and_and_fetch",
#else
   /* specific_name = */	"AND_AND_FETCH_I8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 427 */
#ifdef DEBUG
   /* wnode_name = */		"NAND_AND_FETCH_I8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_NAND_AND_FETCH_I8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__nand_and_fetch",
#else
   /* specific_name = */	"NAND_AND_FETCH_I8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 428 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_ADD_I8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_ADD_I8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_add",
#else
   /* specific_name = */	"FETCH_AND_ADD_I8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 429 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_SUB_I8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_SUB_I8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_sub",
#else
   /* specific_name = */	"FETCH_AND_SUB_I8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 430 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_OR_I8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_OR_I8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_or",
#else
   /* specific_name = */	"FETCH_AND_OR_I8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 431 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_XOR_I8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_XOR_I8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_xor",
#else
   /* specific_name = */	"FETCH_AND_XOR_I8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 432 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_AND_I8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_AND_I8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_and",
#else
   /* specific_name = */	"FETCH_AND_AND_I8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 433 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_NAND_I8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_NAND_I8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_nand",
#else
   /* specific_name = */	"FETCH_AND_NAND_I8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 434 */
#ifdef DEBUG
   /* wnode_name = */		"LOCK_TEST_AND_SET_I4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_LOCK_TEST_AND_SET_I4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__lock_test_and_set",
#else
   /* specific_name = */	"LOCK_TEST_AND_SET_I4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 435 */
#ifdef DEBUG
   /* wnode_name = */		"LOCK_TEST_AND_SET_I8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_LOCK_TEST_AND_SET_I8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__lock_test_and_set",
#else
   /* specific_name = */	"LOCK_TEST_AND_SET_I8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 436 */
#ifdef DEBUG
   /* wnode_name = */		"LOCK_RELEASE_I4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_LOCK_RELEASE_I4,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__lock_release",
#else
   /* specific_name = */	"LOCK_RELEASE_I4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 437 */
#ifdef DEBUG
   /* wnode_name = */		"LOCK_RELEASE_I8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_LOCK_RELEASE_I8,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__lock_release",
#else
   /* specific_name = */	"LOCK_RELEASE_I8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 438 */
#ifdef DEBUG
   /* wnode_name = */		"COMPARE_AND_SWAP_I4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_COMPARE_AND_SWAP_I4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__compare_and_swap",
#else
   /* specific_name = */	"COMPARE_AND_SWAP_I4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 439 */
#ifdef DEBUG
   /* wnode_name = */		"COMPARE_AND_SWAP_I8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_COMPARE_AND_SWAP_I8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__compare_and_swap",
#else
   /* specific_name = */	"COMPARE_AND_SWAP_I8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 440 */
#ifdef DEBUG
   /* wnode_name = */		"SYNCHRONIZE",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_SYNCHRONIZE,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__synchronize",
#else
   /* specific_name = */	"SYNCHRONIZE",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 441 */
#ifdef DEBUG
   /* wnode_name = */		"RETURN_ADDRESS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_RETURN_ADDRESS,
#endif
   /* return_kind = */		RETURN_PV,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__return_address",
#else
   /* specific_name = */	"GET_RETURN_ADDRESS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 442 */
#ifdef DEBUG
   /* wnode_name = */		"U4I1ADRTMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U4I1ADRTMP,
#endif
   /* return_kind = */		RETURN_U4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 443 */
#ifdef DEBUG
   /* wnode_name = */		"U4I2ADRTMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U4I2ADRTMP,
#endif
   /* return_kind = */		RETURN_U4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 444 */
#ifdef DEBUG
   /* wnode_name = */		"U4I4ADRTMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U4I4ADRTMP,
#endif
   /* return_kind = */		RETURN_U4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 445 */
#ifdef DEBUG
   /* wnode_name = */		"U4I8ADRTMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U4I8ADRTMP,
#endif
   /* return_kind = */		RETURN_U4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 446 */
#ifdef DEBUG
   /* wnode_name = */		"U4F4ADRTMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U4F4ADRTMP,
#endif
   /* return_kind = */		RETURN_U4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 447 */
#ifdef DEBUG
   /* wnode_name = */		"U4F8ADRTMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U4F8ADRTMP,
#endif
   /* return_kind = */		RETURN_U4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 448 */
#ifdef DEBUG
   /* wnode_name = */		"U4FQADRTMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U4FQADRTMP,
#endif
   /* return_kind = */		RETURN_U4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 449 */
#ifdef DEBUG
   /* wnode_name = */		"U4C4ADRTMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U4C4ADRTMP,
#endif
   /* return_kind = */		RETURN_U4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 450 */
#ifdef DEBUG
   /* wnode_name = */		"U4C8ADRTMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U4C8ADRTMP,
#endif
   /* return_kind = */		RETURN_U4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 451 */
#ifdef DEBUG
   /* wnode_name = */		"U4CQADRTMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U4CQADRTMP,
#endif
   /* return_kind = */		RETURN_U4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 452 */
#ifdef DEBUG
   /* wnode_name = */		"U4VADRTMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U4VADRTMP,
#endif
   /* return_kind = */		RETURN_U4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 453 */
#ifdef DEBUG
   /* wnode_name = */		"U8I1ADRTMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U8I1ADRTMP,
#endif
   /* return_kind = */		RETURN_U8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 454 */
#ifdef DEBUG
   /* wnode_name = */		"U8I2ADRTMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U8I2ADRTMP,
#endif
   /* return_kind = */		RETURN_U8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 455 */
#ifdef DEBUG
   /* wnode_name = */		"U8I4ADRTMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U8I4ADRTMP,
#endif
   /* return_kind = */		RETURN_U8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 456 */
#ifdef DEBUG
   /* wnode_name = */		"U8I8ADRTMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U8I8ADRTMP,
#endif
   /* return_kind = */		RETURN_U8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 457 */
#ifdef DEBUG
   /* wnode_name = */		"U8F4ADRTMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U8F4ADRTMP,
#endif
   /* return_kind = */		RETURN_U8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 458 */
#ifdef DEBUG
   /* wnode_name = */		"U8F8ADRTMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U8F8ADRTMP,
#endif
   /* return_kind = */		RETURN_U8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 459 */
#ifdef DEBUG
   /* wnode_name = */		"U8FQADRTMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U8FQADRTMP,
#endif
   /* return_kind = */		RETURN_U8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 460 */
#ifdef DEBUG
   /* wnode_name = */		"U8C4ADRTMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U8C4ADRTMP,
#endif
   /* return_kind = */		RETURN_U8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 461 */
#ifdef DEBUG
   /* wnode_name = */		"U8C8ADRTMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U8C8ADRTMP,
#endif
   /* return_kind = */		RETURN_U8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 462 */
#ifdef DEBUG
   /* wnode_name = */		"U8CQADRTMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U8CQADRTMP,
#endif
   /* return_kind = */		RETURN_U8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 463 */
#ifdef DEBUG
   /* wnode_name = */		"U8VADRTMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U8VADRTMP,
#endif
   /* return_kind = */		RETURN_U8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 464 */
#ifdef DEBUG
   /* wnode_name = */		"I4VALTMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4VALTMP,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 465 */
#ifdef DEBUG
   /* wnode_name = */		"I8VALTMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8VALTMP,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 466 */
#ifdef DEBUG
   /* wnode_name = */		"U4VALTMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U4VALTMP,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 467 */
#ifdef DEBUG
   /* wnode_name = */		"U8VALTMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U8VALTMP,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 468 */
#ifdef DEBUG
   /* wnode_name = */		"F4VALTMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4VALTMP,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 469 */
#ifdef DEBUG
   /* wnode_name = */		"F8VALTMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8VALTMP,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 470 */
#ifdef DEBUG
   /* wnode_name = */		"FQVALTMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQVALTMP,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 471 */
#ifdef DEBUG
   /* wnode_name = */		"C4VALTMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C4VALTMP,
#endif
   /* return_kind = */		RETURN_C4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 472 */
#ifdef DEBUG
   /* wnode_name = */		"C8VALTMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C8VALTMP,
#endif
   /* return_kind = */		RETURN_C8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 473 */
#ifdef DEBUG
   /* wnode_name = */		"CQVALTMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_CQVALTMP,
#endif
   /* return_kind = */		RETURN_CQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 474 */
#ifdef DEBUG
   /* wnode_name = */		"BCOPY",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_BCOPY,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"bcopy",
#else
   /* specific_name = */	"BCOPY",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"bcopy"
 },
 { /* 475 */
#ifdef DEBUG
   /* wnode_name = */		"BCMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_BCMP,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"bcmp",
#else
   /* specific_name = */	"BCMP",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"bcmp"
 },
 { /* 476 */
#ifdef DEBUG
   /* wnode_name = */		"BZERO",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_BZERO,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"bzero",
#else
   /* specific_name = */	"BZERO",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"bzero"
 },
 { /* 477 */
#ifdef DEBUG
   /* wnode_name = */		"MEMCCPY",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_MEMCCPY,
#endif
   /* return_kind = */		RETURN_PV,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"memccpy",
#else
   /* specific_name = */	"MEMCCPY",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"memccpy"
 },
 { /* 478 */
#ifdef DEBUG
   /* wnode_name = */		"MEMCHR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_MEMCHR,
#endif
   /* return_kind = */		RETURN_PV,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"memchr",
#else
   /* specific_name = */	"MEMCHR",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"memchr"
 },
 { /* 479 */
#ifdef DEBUG
   /* wnode_name = */		"MEMCMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_MEMCMP,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"memcmp",
#else
   /* specific_name = */	"MEMCMP",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"memcmp"
 },
 { /* 480 */
#ifdef DEBUG
   /* wnode_name = */		"MEMCPY",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_MEMCPY,
#endif
   /* return_kind = */		RETURN_PV,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"memcpy",
#else
   /* specific_name = */	"MEMCPY",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"memcpy"
 },
 { /* 481 */
#ifdef DEBUG
   /* wnode_name = */		"MEMMOVE",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_MEMMOVE,
#endif
   /* return_kind = */		RETURN_PV,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"memmove",
#else
   /* specific_name = */	"MEMMOVE",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"memmove"
 },
 { /* 482 */
#ifdef DEBUG
   /* wnode_name = */		"MEMSET",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_MEMSET,
#endif
   /* return_kind = */		RETURN_PV,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"memset",
#else
   /* specific_name = */	"MEMSET",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"memset"
 },
 { /* 483 */
#ifdef DEBUG
   /* wnode_name = */		"STRCMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_STRCMP,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"strcmp",
#else
   /* specific_name = */	"STRCMP",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"strcmp"
 },
 { /* 484 */
#ifdef DEBUG
   /* wnode_name = */		"STRNCMP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_STRNCMP,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"strncmp",
#else
   /* specific_name = */	"STRNCMP",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"strncmp"
 },
 { /* 485 */
#ifdef DEBUG
   /* wnode_name = */		"STRCPY",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_STRCPY,
#endif
   /* return_kind = */		RETURN_PC,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"strcpy",
#else
   /* specific_name = */	"STRCPY",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"strcpy"
 },
 { /* 486 */
#ifdef DEBUG
   /* wnode_name = */		"STRNCPY",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_STRNCPY,
#endif
   /* return_kind = */		RETURN_PC,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"strncpy",
#else
   /* specific_name = */	"STRNCPY",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"strncpy"
 },
 { /* 487 */
#ifdef DEBUG
   /* wnode_name = */		"STRLEN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_STRLEN,
#endif
   /* return_kind = */		RETURN_SZT,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"strlen",
#else
   /* specific_name = */	"STRLEN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"strlen"
 },
 { /* 488 */
#ifdef DEBUG
   /* wnode_name = */		"PRINTF",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_PRINTF,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"printf",
#else
   /* specific_name = */	"PRINTF",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"printf"
 },
 { /* 489 */
#ifdef DEBUG
   /* wnode_name = */		"FPRINTF",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FPRINTF,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"fprintf",
#else
   /* specific_name = */	"FPRINTF",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"fprintf"
 },
 { /* 490 */
#ifdef DEBUG
   /* wnode_name = */		"SPRINTF",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_SPRINTF,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"sprintf",
#else
   /* specific_name = */	"SPRINTF",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"sprintf"
 },
 { /* 491 */
#ifdef DEBUG
   /* wnode_name = */		"PRINTW",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_PRINTW,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"printw",
#else
   /* specific_name = */	"PRINTW",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"printw"
 },
 { /* 492 */
#ifdef DEBUG
   /* wnode_name = */		"SCANF",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_SCANF,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"scanf",
#else
   /* specific_name = */	"SCANF",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"scanf"
 },
 { /* 493 */
#ifdef DEBUG
   /* wnode_name = */		"FSCANF",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FSCANF,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"fscanf",
#else
   /* specific_name = */	"FSCANF",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"fscanf"
 },
 { /* 494 */
#ifdef DEBUG
   /* wnode_name = */		"SSCANF",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_SSCANF,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"sscanf",
#else
   /* specific_name = */	"SSCANF",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"sscanf"
 },
 { /* 495 */
#ifdef DEBUG
   /* wnode_name = */		"FPUTC",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FPUTC,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"fputc",
#else
   /* specific_name = */	"FPUTC",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"fputc"
 },
 { /* 496 */
#ifdef DEBUG
   /* wnode_name = */		"FPUTS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FPUTS,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"fputs",
#else
   /* specific_name = */	"FPUTS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"fputs"
 },
 { /* 497 */
#ifdef DEBUG
   /* wnode_name = */		"FGETC",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FGETC,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"fgetc",
#else
   /* specific_name = */	"FGETC",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"fgetc"
 },
 { /* 498 */
#ifdef DEBUG
   /* wnode_name = */		"FGETS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FGETS,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"fgets",
#else
   /* specific_name = */	"FGETS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"fgets"
 },
 { /* 499 */
#ifdef DEBUG
   /* wnode_name = */		"F4VACOS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4VACOS,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"vacosf",
#else
   /* specific_name = */	"VACOSF",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"vacosf"
 },
 { /* 500 */
#ifdef DEBUG
   /* wnode_name = */		"F8VACOS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8VACOS,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"vacos",
#else
   /* specific_name = */	"VACOS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"vacos"
 },
 { /* 501 */
#ifdef DEBUG
   /* wnode_name = */		"F4VASIN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4VASIN,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"vasinf",
#else
   /* specific_name = */	"VASINF",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"vasinf"
 },
 { /* 502 */
#ifdef DEBUG
   /* wnode_name = */		"F8VASIN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8VASIN,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"vasin",
#else
   /* specific_name = */	"VASIN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"vasin"
 },
 { /* 503 */
#ifdef DEBUG
   /* wnode_name = */		"F4VATAN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4VATAN,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"vatanf",
#else
   /* specific_name = */	"VATANF",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"vatanf"
 },
 { /* 504 */
#ifdef DEBUG
   /* wnode_name = */		"F8VATAN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8VATAN,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"vatan",
#else
   /* specific_name = */	"VATAN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"vatan"
 },
 { /* 505 */
#ifdef DEBUG
   /* wnode_name = */		"F4VCOS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4VCOS,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"vcosf",
#else
   /* specific_name = */	"VCOSF",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"vcosf"
 },
 { /* 506 */
#ifdef DEBUG
   /* wnode_name = */		"F8VCOS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8VCOS,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"vcos",
#else
   /* specific_name = */	"VCOS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"vcos"
 },
 { /* 507 */
#ifdef DEBUG
   /* wnode_name = */		"F4VEXP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4VEXP,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"vexpf",
#else
   /* specific_name = */	"VEXPF",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"vexpf"
 },
 { /* 508 */
#ifdef DEBUG
   /* wnode_name = */		"F8VEXP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8VEXP,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"vexp",
#else
   /* specific_name = */	"VEXP",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"vexp"
 },
 { /* 509 */
#ifdef DEBUG
   /* wnode_name = */		"F4VLOG",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4VLOG,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"vlogf",
#else
   /* specific_name = */	"VLOGF",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"vlogf"
 },
 { /* 510 */
#ifdef DEBUG
   /* wnode_name = */		"F8VLOG",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8VLOG,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"vlog",
#else
   /* specific_name = */	"VLOG",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"vlog"
 },
 { /* 511 */
#ifdef DEBUG
   /* wnode_name = */		"F4VSIN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4VSIN,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"vsinf",
#else
   /* specific_name = */	"VSINF",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"vsinf"
 },
 { /* 512 */
#ifdef DEBUG
   /* wnode_name = */		"F8VSIN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8VSIN,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"vsin",
#else
   /* specific_name = */	"VSIN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"vsin"
 },
 { /* 513 */
#ifdef DEBUG
   /* wnode_name = */		"F4VSQRT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4VSQRT,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"vsqrtf",
#else
   /* specific_name = */	"VSQRTF",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"vsqrtf"
 },
 { /* 514 */
#ifdef DEBUG
   /* wnode_name = */		"F8VSQRT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8VSQRT,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"vsqrt",
#else
   /* specific_name = */	"VSQRT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"vsqrt"
 },
 { /* 515 */
#ifdef DEBUG
   /* wnode_name = */		"F4VTAN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4VTAN,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"vtanf",
#else
   /* specific_name = */	"VTANF",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"vtanf"
 },
 { /* 516 */
#ifdef DEBUG
   /* wnode_name = */		"F8VTAN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8VTAN,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"vtan",
#else
   /* specific_name = */	"VTAN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"vtan"
 },
 { /* 517 */
#ifdef DEBUG
   /* wnode_name = */		"NARY_ADD",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_NARY_ADD,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 518 */
#ifdef DEBUG
   /* wnode_name = */		"NARY_MPY",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_NARY_MPY,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 519 */
#ifdef DEBUG
   /* wnode_name = */		"U4I4TRAPUV_MALLOC",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U4I4TRAPUV_MALLOC,
#endif
   /* return_kind = */		RETURN_PV,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"TRAPUV_MALLOC",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__trapuv_malloc"
 },
 { /* 520 */
#ifdef DEBUG
   /* wnode_name = */		"U8I8TRAPUV_MALLOC",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_U8I8TRAPUV_MALLOC,
#endif
   /* return_kind = */		RETURN_PV,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"TRAPUV_MALLOC",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__trapuv_malloc"
 },
 { /* 521 */
#ifdef DEBUG
   /* wnode_name = */		"F77_BOUNDS_ERR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F77_BOUNDS_ERR,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	TRUE,
   /* runtime_name = */		"s_rnge"
 },
 { /* 522 */
#ifdef DEBUG
   /* wnode_name = */		"DSM_NUMTHREADS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_DSM_NUMTHREADS,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DSM_NUMTHREADS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"dsm_numthreads"
 },
 { /* 523 */
#ifdef DEBUG
   /* wnode_name = */		"DSM_CHUNKSIZE",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_DSM_CHUNKSIZE,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DSM_CHUNKSIZE",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"dsm_chunksize"
 },
 { /* 524 */
#ifdef DEBUG
   /* wnode_name = */		"DSM_THIS_CHUNKSIZE",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_DSM_THIS_CHUNKSIZE,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DSM_THIS_CHUNKSIZE",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"dsm_this_chunksize"
 },
 { /* 525 */
#ifdef DEBUG
   /* wnode_name = */		"DSM_REM_CHUNKSIZE",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_DSM_REM_CHUNKSIZE,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DSM_REM_CHUNKSIZE",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"dsm_rem_chunksize"
 },
 { /* 526 */
#ifdef DEBUG
   /* wnode_name = */		"DSM_NUMCHUNKS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_DSM_NUMCHUNKS,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DSM_NUMCHUNKS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"dsm_numchunks"
 },
 { /* 527 */
#ifdef DEBUG
   /* wnode_name = */		"DSM_THIS_THREADNUM",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_DSM_THIS_THREADNUM,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DSM_THIS_THREADNUM",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"dsm_this_threadnum"
 },
 { /* 528 */
#ifdef DEBUG
   /* wnode_name = */		"DSM_DISTRIBUTION_BLOCK",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_DSM_DISTRIBUTION_BLOCK,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DSM_DISTRIBUTION_BLOCK",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"dsm_distribution_block"
 },
 { /* 529 */
#ifdef DEBUG
   /* wnode_name = */		"DSM_DISTRIBUTION_STAR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_DSM_DISTRIBUTION_STAR,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DSM_DISTRIBUTION_STAR",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"dsm_distribution_star"
 },
 { /* 530 */
#ifdef DEBUG
   /* wnode_name = */		"DSM_ISRESHAPED",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_DSM_ISRESHAPED,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DSM_ISRESHAPED",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"dsm_isreshaped"
 },
 { /* 531 */
#ifdef DEBUG
   /* wnode_name = */		"DSM_ISDISTRIBUTED",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_DSM_ISDISTRIBUTED,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DSM_ISDISTRIBUTED",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"dsm_isdistributed"
 },
 { /* 532 */
#ifdef DEBUG
   /* wnode_name = */		"DSM_THIS_STARTINDEX",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_DSM_THIS_STARTINDEX,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DSM_THIS_STARTINGINDEX",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"dsm_this_startingindex"
 },
 { /* 533 */
#ifdef DEBUG
   /* wnode_name = */		"DSM_DISTRIBUTION_CYCLIC",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_DSM_DISTRIBUTION_CYCLIC,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DSM_DISTRIBUTION_CYCLIC",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"dsm_distribution_cyclic"
 },
 { /* 534 */
#ifdef DEBUG
   /* wnode_name = */		"MPY_AND_FETCH_I4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_MPY_AND_FETCH_I4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__mpy_and_fetch",
#else
   /* specific_name = */	"MPY_AND_FETCH_I4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 535 */
#ifdef DEBUG
   /* wnode_name = */		"MIN_AND_FETCH_I4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_MIN_AND_FETCH_I4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__min_and_fetch",
#else
   /* specific_name = */	"MIN_AND_FETCH_I4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 536 */
#ifdef DEBUG
   /* wnode_name = */		"MAX_AND_FETCH_I4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_MAX_AND_FETCH_I4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__max_and_fetch",
#else
   /* specific_name = */	"MAX_AND_FETCH_I4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 537 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_MPY_I4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_MPY_I4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_mpy",
#else
   /* specific_name = */	"FETCH_AND_MPY_I4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 538 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_MIN_I4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_MIN_I4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_min",
#else
   /* specific_name = */	"FETCH_AND_MIN_I4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 539 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_MAX_I4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_MAX_I4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_max",
#else
   /* specific_name = */	"FETCH_AND_MAX_I4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 540 */
#ifdef DEBUG
   /* wnode_name = */		"MPY_AND_FETCH_I8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_MPY_AND_FETCH_I8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__mpy_and_fetch",
#else
   /* specific_name = */	"MPY_AND_FETCH_I8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 541 */
#ifdef DEBUG
   /* wnode_name = */		"MIN_AND_FETCH_I8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_MIN_AND_FETCH_I8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__min_and_fetch",
#else
   /* specific_name = */	"MIN_AND_FETCH_I8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 542 */
#ifdef DEBUG
   /* wnode_name = */		"MAX_AND_FETCH_I8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_MAX_AND_FETCH_I8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__max_and_fetch",
#else
   /* specific_name = */	"MAX_AND_FETCH_I8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 543 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_MPY_I8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_MPY_I8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_mpy",
#else
   /* specific_name = */	"FETCH_AND_MPY_I8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 544 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_MIN_I8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_MIN_I8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_min",
#else
   /* specific_name = */	"FETCH_AND_MIN_I8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 545 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_MAX_I8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_MAX_I8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_max",
#else
   /* specific_name = */	"FETCH_AND_MAX_I8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 546 */
#ifdef DEBUG
   /* wnode_name = */		"ADD_AND_FETCH_F4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_ADD_AND_FETCH_F4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__add_and_fetch",
#else
   /* specific_name = */	"ADD_AND_FETCH_F4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 547 */
#ifdef DEBUG
   /* wnode_name = */		"SUB_AND_FETCH_F4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_SUB_AND_FETCH_F4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__sub_and_fetch",
#else
   /* specific_name = */	"SUB_AND_FETCH_F4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 548 */
#ifdef DEBUG
   /* wnode_name = */		"OR_AND_FETCH_F4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_OR_AND_FETCH_F4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__or_and_fetch",
#else
   /* specific_name = */	"OR_AND_FETCH_F4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 549 */
#ifdef DEBUG
   /* wnode_name = */		"XOR_AND_FETCH_F4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_XOR_AND_FETCH_F4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__xor_and_fetch",
#else
   /* specific_name = */	"XOR_AND_FETCH_F4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 550 */
#ifdef DEBUG
   /* wnode_name = */		"AND_AND_FETCH_F4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_AND_AND_FETCH_F4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__and_and_fetch",
#else
   /* specific_name = */	"AND_AND_FETCH_F4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 551 */
#ifdef DEBUG
   /* wnode_name = */		"NAND_AND_FETCH_F4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_NAND_AND_FETCH_F4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__nand_and_fetch",
#else
   /* specific_name = */	"NAND_AND_FETCH_F4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 552 */
#ifdef DEBUG
   /* wnode_name = */		"MPY_AND_FETCH_F4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_MPY_AND_FETCH_F4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__mpy_and_fetch",
#else
   /* specific_name = */	"MPY_AND_FETCH_F4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 553 */
#ifdef DEBUG
   /* wnode_name = */		"MIN_AND_FETCH_F4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_MIN_AND_FETCH_F4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__min_and_fetch",
#else
   /* specific_name = */	"MIN_AND_FETCH_F4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 554 */
#ifdef DEBUG
   /* wnode_name = */		"MAX_AND_FETCH_F4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_MAX_AND_FETCH_F4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__max_and_fetch",
#else
   /* specific_name = */	"MAX_AND_FETCH_F4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 555 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_ADD_F4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_ADD_F4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_add",
#else
   /* specific_name = */	"FETCH_AND_ADD_F4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 556 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_SUB_F4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_SUB_F4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_sub",
#else
   /* specific_name = */	"FETCH_AND_SUB_F4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 557 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_OR_F4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_OR_F4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_or",
#else
   /* specific_name = */	"FETCH_AND_OR_F4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 558 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_XOR_F4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_XOR_F4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_xor",
#else
   /* specific_name = */	"FETCH_AND_XOR_F4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 559 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_AND_F4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_AND_F4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_and",
#else
   /* specific_name = */	"FETCH_AND_AND_F4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 560 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_NAND_F4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_NAND_F4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_nand",
#else
   /* specific_name = */	"FETCH_AND_NAND_F4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 561 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_MPY_F4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_MPY_F4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_mpy",
#else
   /* specific_name = */	"FETCH_AND_MPY_F4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 562 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_MIN_F4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_MIN_F4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_min",
#else
   /* specific_name = */	"FETCH_AND_MIN_F4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 563 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_MAX_F4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_MAX_F4,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_max",
#else
   /* specific_name = */	"FETCH_AND_MAX_F4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 564 */
#ifdef DEBUG
   /* wnode_name = */		"ADD_AND_FETCH_F8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_ADD_AND_FETCH_F8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__add_and_fetch",
#else
   /* specific_name = */	"ADD_AND_FETCH_F8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 565 */
#ifdef DEBUG
   /* wnode_name = */		"SUB_AND_FETCH_F8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_SUB_AND_FETCH_F8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__sub_and_fetch",
#else
   /* specific_name = */	"SUB_AND_FETCH_F8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 566 */
#ifdef DEBUG
   /* wnode_name = */		"OR_AND_FETCH_F8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_OR_AND_FETCH_F8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__or_and_fetch",
#else
   /* specific_name = */	"OR_AND_FETCH_F8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 567 */
#ifdef DEBUG
   /* wnode_name = */		"XOR_AND_FETCH_F8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_XOR_AND_FETCH_F8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__xor_and_fetch",
#else
   /* specific_name = */	"XOR_AND_FETCH_F8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 568 */
#ifdef DEBUG
   /* wnode_name = */		"AND_AND_FETCH_F8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_AND_AND_FETCH_F8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__and_and_fetch",
#else
   /* specific_name = */	"AND_AND_FETCH_F8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 569 */
#ifdef DEBUG
   /* wnode_name = */		"NAND_AND_FETCH_F8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_NAND_AND_FETCH_F8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__nand_and_fetch",
#else
   /* specific_name = */	"NAND_AND_FETCH_F8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 570 */
#ifdef DEBUG
   /* wnode_name = */		"MPY_AND_FETCH_F8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_MPY_AND_FETCH_F8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__mpy_and_fetch",
#else
   /* specific_name = */	"MPY_AND_FETCH_F8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 571 */
#ifdef DEBUG
   /* wnode_name = */		"MIN_AND_FETCH_F8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_MIN_AND_FETCH_F8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__min_and_fetch",
#else
   /* specific_name = */	"MIN_AND_FETCH_F8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 572 */
#ifdef DEBUG
   /* wnode_name = */		"MAX_AND_FETCH_F8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_MAX_AND_FETCH_F8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__max_and_fetch",
#else
   /* specific_name = */	"MAX_AND_FETCH_F8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 573 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_ADD_F8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_ADD_F8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_add",
#else
   /* specific_name = */	"FETCH_AND_ADD_F8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 574 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_SUB_F8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_SUB_F8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_sub",
#else
   /* specific_name = */	"FETCH_AND_SUB_F8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 575 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_OR_F8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_OR_F8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_or",
#else
   /* specific_name = */	"FETCH_AND_OR_F8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 576 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_XOR_F8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_XOR_F8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_xor",
#else
   /* specific_name = */	"FETCH_AND_XOR_F8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 577 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_AND_F8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_AND_F8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_and",
#else
   /* specific_name = */	"FETCH_AND_AND_F8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 578 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_NAND_F8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_NAND_F8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_nand",
#else
   /* specific_name = */	"FETCH_AND_NAND_F8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 579 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_MPY_F8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_MPY_F8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_mpy",
#else
   /* specific_name = */	"FETCH_AND_MPY_F8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 580 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_MIN_F8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_MIN_F8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_min",
#else
   /* specific_name = */	"FETCH_AND_MIN_F8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 581 */
#ifdef DEBUG
   /* wnode_name = */		"FETCH_AND_MAX_F8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FETCH_AND_MAX_F8,
#endif
   /* return_kind = */		RETURN_DA1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__fetch_and_max",
#else
   /* specific_name = */	"FETCH_AND_MAX_F8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 582 */
#ifdef DEBUG
   /* wnode_name = */		"LOCK_ACQUIRE_I4",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_LOCK_ACQUIRE_I4,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__lock_acquire",
#else
   /* specific_name = */	"LOCK_ACQUIRE_I4",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 583 */
#ifdef DEBUG
   /* wnode_name = */		"LOCK_ACQUIRE_I8",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_LOCK_ACQUIRE_I8,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__lock_acquire",
#else
   /* specific_name = */	"LOCK_ACQUIRE_I8",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 584 */
#ifdef DEBUG
   /* wnode_name = */		"F90_STACKTEMPALLOC",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F90_STACKTEMPALLOC,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 585 */
#ifdef DEBUG
   /* wnode_name = */		"F90_HEAPTEMPALLOC",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F90_HEAPTEMPALLOC,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 586 */
#ifdef DEBUG
   /* wnode_name = */		"F90_STACKTEMPFREE",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F90_STACKTEMPFREE,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 587 */
#ifdef DEBUG
   /* wnode_name = */		"F90_HEAPTEMPFREE",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F90_HEAPTEMPFREE,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 588 */
#ifdef DEBUG
   /* wnode_name = */		"F4EXPONENT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4EXPONENT,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F4EXPONENT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_EXPONENT_4"
 },
 { /* 589 */
#ifdef DEBUG
   /* wnode_name = */		"F8EXPONENT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8EXPONENT,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F8EXPONENT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_EXPONENT"
 },
 { /* 590 */
#ifdef DEBUG
   /* wnode_name = */		"FQEXPONENT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQEXPONENT,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"FQEXPONENT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_EXPONENT_16"
 },
 { /* 591 */
#ifdef DEBUG
   /* wnode_name = */		"F4FRACTION",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4FRACTION,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F4FRACTION",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_FRACTION_4"
 },
 { /* 592 */
#ifdef DEBUG
   /* wnode_name = */		"F8FRACTION",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8FRACTION,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F8FRACTION",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_FRACTION"
 },
 { /* 593 */
#ifdef DEBUG
   /* wnode_name = */		"FQFRACTION",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQFRACTION,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"FQFRACTION",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_FRACTION_16"
 },
 { /* 594 */
#ifdef DEBUG
   /* wnode_name = */		"F4MODULO",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4MODULO,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F4MODULO",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_modulo4"
 },
 { /* 595 */
#ifdef DEBUG
   /* wnode_name = */		"F8MODULO",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8MODULO,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F8MODULO",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_modulo8"
 },
 { /* 596 */
#ifdef DEBUG
   /* wnode_name = */		"FQMODULO",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQMODULO,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"FQMODULO",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_moduloq"
 },
 { /* 597 */
#ifdef DEBUG
   /* wnode_name = */		"F4NEAREST",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4NEAREST,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F4NEAREST",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_NEAREST_4"
 },
 { /* 598 */
#ifdef DEBUG
   /* wnode_name = */		"F8NEAREST",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8NEAREST,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F8NEAREST",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_NEAREST"
 },
 { /* 599 */
#ifdef DEBUG
   /* wnode_name = */		"FQNEAREST",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQNEAREST,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"FQNEAREST",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_NEAREST_16"
 },
 { /* 600 */
#ifdef DEBUG
   /* wnode_name = */		"F4RRSPACING",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4RRSPACING,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F4RRSPACING",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_RRSPACING_4"
 },
 { /* 601 */
#ifdef DEBUG
   /* wnode_name = */		"F8RRSPACING",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8RRSPACING,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F8RRSPACING",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_RRSPACING"
 },
 { /* 602 */
#ifdef DEBUG
   /* wnode_name = */		"FQRRSPACING",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQRRSPACING,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"FQRRSPACING",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_RRSPACING_16"
 },
 { /* 603 */
#ifdef DEBUG
   /* wnode_name = */		"F4SCALE",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4SCALE,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F4SCALE",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_SCALE_4"
 },
 { /* 604 */
#ifdef DEBUG
   /* wnode_name = */		"F8SCALE",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8SCALE,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F8SCALE",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_SCALE"
 },
 { /* 605 */
#ifdef DEBUG
   /* wnode_name = */		"FQSCALE",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQSCALE,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"FQSCALE",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_SCALE_16"
 },
 { /* 606 */
#ifdef DEBUG
   /* wnode_name = */		"F4SET_EXPONENT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4SET_EXPONENT,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F4SET_EXPONENT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_SET_EXPONENT_4"
 },
 { /* 607 */
#ifdef DEBUG
   /* wnode_name = */		"F8SET_EXPONENT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8SET_EXPONENT,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F8SET_EXPONENT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_SET_EXPONENT"
 },
 { /* 608 */
#ifdef DEBUG
   /* wnode_name = */		"FQSET_EXPONENT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQSET_EXPONENT,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"FQSET_EXPONENT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_SET_EXPONENT_16"
 },
 { /* 609 */
#ifdef DEBUG
   /* wnode_name = */		"F4SPACING",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4SPACING,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F4SPACING",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_SPACING_4"
 },
 { /* 610 */
#ifdef DEBUG
   /* wnode_name = */		"F8SPACING",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8SPACING,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F8SPACING",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_SPACING"
 },
 { /* 611 */
#ifdef DEBUG
   /* wnode_name = */		"FQSPACING",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQSPACING,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"FQSPACING",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_SPACING_16"
 },
 { /* 612 */
#ifdef DEBUG
   /* wnode_name = */		"F4NEXTAFTER",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4NEXTAFTER,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F4NEXTAFTER",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_IEEE_NEXT_AFTER_H"
 },
 { /* 613 */
#ifdef DEBUG
   /* wnode_name = */		"F8NEXTAFTER",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8NEXTAFTER,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F8NEXTAFTER",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
#ifdef KEY /* Bug 11103 */
  /* The GNU libm -m32 version of "nextafter" fails to set exceptions. If that
   * bug is fixed, we could resume calling "nextafter" directly, and for that
   * matter, we could call "nextafterf" for F4NEXTAFTER. */
   /* runtime_name = */		"_IEEE_NEXT_AFTER"
#else /* KEY Bug 11103 */
   /* runtime_name = */		"nextafter"
#endif /* KEY Bug 11103 */
 },
 { /* 614 */
#ifdef DEBUG
   /* wnode_name = */		"FQNEXTAFTER",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQNEXTAFTER,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"FQNEXTAFTER",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"qnextafter"
 },
 { /* 615 */
#ifdef DEBUG
   /* wnode_name = */		"F4ISNAN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4ISNAN,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F4ISNAN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"isnanf"
 },
 { /* 616 */
#ifdef DEBUG
   /* wnode_name = */		"F8ISNAN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8ISNAN,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F8ISNAN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"isnan"
 },
 { /* 617 */
#ifdef DEBUG
   /* wnode_name = */		"FQISNAN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQISNAN,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"FQISNAN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"isnanq"
 },
 { /* 618 */
#ifdef DEBUG
   /* wnode_name = */		"F4SCALB",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4SCALB,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F4SCALB",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"ldexp"
 },
 { /* 619 */
#ifdef DEBUG
   /* wnode_name = */		"F8SCALB",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8SCALB,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F8SCALB",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"ldexp"
 },
 { /* 620 */
#ifdef DEBUG
   /* wnode_name = */		"FQSCALB",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQSCALB,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"FQSCALB",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"qldexp"
 },
 { /* 621 */
#ifdef DEBUG
   /* wnode_name = */		"F4IEEE_REMAINDER",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4IEEE_REMAINDER,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F4IEEE_REMAINDER",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
#ifdef KEY /* Bug 10261 */
  /* GNU libm drem and remainder have a bug: they don't ignore the rounding
   * mode as required by C99 (and F90 ieee_rem.) Using "remainder" instead of
   * "drem" causes pathcc (and pathf90) to link the version provided by
   * libmpath, which works correctly. */
   /* runtime_name = */		"remainder"
#else /* KEY Bug 10261 */
   /* runtime_name = */		"drem"
#endif /* KEY Bug 10261 */
 },
 { /* 622 */
#ifdef DEBUG
   /* wnode_name = */		"F8IEEE_REMAINDER",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8IEEE_REMAINDER,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F8IEEE_REMAINDER",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
#ifdef KEY /* Bug 10261 */
   /* See comment earlier in file */
   /* runtime_name = */		"remainder"
#else /* KEY Bug 10261 */
   /* runtime_name = */		"drem"
#endif /* KEY Bug 10261 */
 },
 { /* 623 */
#ifdef DEBUG
   /* wnode_name = */		"FQIEEE_REMAINDER",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQIEEE_REMAINDER,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"FQIEEE_REMAINDER",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_IEEE_REMAINDER_Q"
 },
 { /* 624 */
#ifdef DEBUG
   /* wnode_name = */		"F4LOGB",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4LOGB,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F4LOGB",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"logb"
 },
 { /* 625 */
#ifdef DEBUG
   /* wnode_name = */		"F8LOGB",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8LOGB,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F8LOGB",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"logb"
 },
 { /* 626 */
#ifdef DEBUG
   /* wnode_name = */		"FQLOGB",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQLOGB,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"FQLOGB",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__qlogb"
 },
 { /* 627 */
#ifdef DEBUG
   /* wnode_name = */		"F4ILOGB",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4ILOGB,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F4ILOGB",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__ilogbf"
 },
 { /* 628 */
#ifdef DEBUG
   /* wnode_name = */		"F8ILOGB",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8ILOGB,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F8ILOGB",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__ilogb"
 },
 { /* 629 */
#ifdef DEBUG
   /* wnode_name = */		"FQILOGB",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQILOGB,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"FQILOGB",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__iqlogb"
 },
 { /* 630 */
#ifdef DEBUG
   /* wnode_name = */		"F4FPCLASS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4FPCLASS,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F4FPCLASS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_fp_class_f"
 },
 { /* 631 */
#ifdef DEBUG
   /* wnode_name = */		"F8FPCLASS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8FPCLASS,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F8FPCLASS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_fp_class_d"
 },
 { /* 632 */
#ifdef DEBUG
   /* wnode_name = */		"FQFPCLASS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQFPCLASS,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"FQFPCLASS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_fp_class_q"
 },
 { /* 633 */
#ifdef DEBUG
   /* wnode_name = */		"F4FINITE",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4FINITE,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F4FINITE",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"finite"
 },
 { /* 634 */
#ifdef DEBUG
   /* wnode_name = */		"F8FINITE",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8FINITE,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F8FINITE",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"finite"
 },
 { /* 635 */
#ifdef DEBUG
   /* wnode_name = */		"FQFINITE",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQFINITE,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"FQFINITE",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__qfinite"
 },
 { /* 636 */
#ifdef DEBUG
   /* wnode_name = */		"F4UNORDERED",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4UNORDERED,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F4UNORDERED",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"unordered"
 },
 { /* 637 */
#ifdef DEBUG
   /* wnode_name = */		"F8UNORDERED",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8UNORDERED,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F8UNORDERED",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"unordered"
 },
 { /* 638 */
#ifdef DEBUG
   /* wnode_name = */		"FQUNORDERED",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQUNORDERED,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"FQUNORDERED",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"unorderedl"
 },
 { /* 639 */
#ifdef DEBUG
   /* wnode_name = */		"I1POPCNT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I1POPCNT,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"I1POPCNT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__popcnt1"
 },
 { /* 640 */
#ifdef DEBUG
   /* wnode_name = */		"I2POPCNT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2POPCNT,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"I2POPCNT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__popcnt2"
 },
 { /* 641 */
#ifdef DEBUG
   /* wnode_name = */		"I4POPCNT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4POPCNT,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"I4POPCNT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__popcnt4"
 },
 { /* 642 */
#ifdef DEBUG
   /* wnode_name = */		"I8POPCNT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8POPCNT,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"I8POPCNT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__popcnt8"
 },
 { /* 643 */
#ifdef DEBUG
   /* wnode_name = */		"I1LEADZ",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I1LEADZ,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"I1LEADZ",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__leadz1"
 },
 { /* 644 */
#ifdef DEBUG
   /* wnode_name = */		"I2LEADZ",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2LEADZ,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"I2LEADZ",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__leadz2"
 },
 { /* 645 */
#ifdef DEBUG
   /* wnode_name = */		"I4LEADZ",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4LEADZ,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"I4LEADZ",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__leadz4"
 },
 { /* 646 */
#ifdef DEBUG
   /* wnode_name = */		"I8LEADZ",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8LEADZ,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"I8LEADZ",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__leadz8"
 },
 { /* 647 */
#ifdef DEBUG
   /* wnode_name = */		"LENTRIM",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_LENTRIM,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"LENTRIM",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"i_lentrim"
 },
 { /* 648 */
#ifdef DEBUG
   /* wnode_name = */		"F90INDEX",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F90INDEX,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F90INDEX",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_index90"
 },
 { /* 649 */
#ifdef DEBUG
   /* wnode_name = */		"SCAN",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_SCAN,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"SCAN",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_scan90"
 },
 { /* 650 */
#ifdef DEBUG
   /* wnode_name = */		"VERIFY",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_VERIFY,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"VERIFY",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_verify90"
 },
 { /* 651 */
#ifdef DEBUG
   /* wnode_name = */		"ADJUSTL",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_ADJUSTL,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ADJUSTL",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_F90_ADJUSTL"
 },
 { /* 652 */
#ifdef DEBUG
   /* wnode_name = */		"ADJUSTR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_ADJUSTR,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ADJUSTR",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_F90_ADJUSTR"
 },
 { /* 653 */
#ifdef DEBUG
   /* wnode_name = */		"GET_IEEE_EXCEPTIONS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_GET_IEEE_EXCEPTIONS,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"GET_IEEE_EXCEPTIONS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__get_ieee_exceptions"
 },
 { /* 654 */
#ifdef DEBUG
   /* wnode_name = */		"GET_IEEE_INTERRUPTS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_GET_IEEE_INTERRUPTS,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"GET_IEEE_INTERRUPTS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__get_ieee_interrupts"
 },
 { /* 655 */
#ifdef DEBUG
   /* wnode_name = */		"GET_IEEE_ROUNDING_MODE",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_GET_IEEE_ROUNDING_MODE,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"GET_IEEE_ROUNDING_MODE",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"fpgetround"
 },
 { /* 656 */
#ifdef DEBUG
   /* wnode_name = */		"GET_IEEE_STATUS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_GET_IEEE_STATUS,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"GET_IEEE_STATUS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__get_ieee_status"
 },
 { /* 657 */
#ifdef DEBUG
   /* wnode_name = */		"SET_IEEE_EXCEPTIONS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_SET_IEEE_EXCEPTIONS,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"SET_IEEE_EXCEPTIONS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__set_ieee_exceptions"
 },
 { /* 658 */
#ifdef DEBUG
   /* wnode_name = */		"SET_IEEE_EXCEPTION",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_SET_IEEE_EXCEPTION,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"SET_IEEE_EXCEPTION",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__set_ieee_exception"
 },
 { /* 659 */
#ifdef DEBUG
   /* wnode_name = */		"SET_IEEE_INTERRUPTS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_SET_IEEE_INTERRUPTS,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"SET_IEEE_INTERRUPTS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__set_ieee_interrupts"
 },
 { /* 660 */
#ifdef DEBUG
   /* wnode_name = */		"SET_IEEE_ROUNDING_MODE",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_SET_IEEE_ROUNDING_MODE,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"SET_IEEE_ROUNDING_MODE",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"fpsetround"
 },
 { /* 661 */
#ifdef DEBUG
   /* wnode_name = */		"SET_IEEE_STATUS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_SET_IEEE_STATUS,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"SET_IEEE_STATUS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__set_ieee_status"
 },
 { /* 662 */
#ifdef DEBUG
   /* wnode_name = */		"ENABLE_IEEE_INTERRUPT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_ENABLE_IEEE_INTERRUPT,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"ENABLE_IEEE_INTERRUPT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__enable_ieee_interrupt"
 },
 { /* 663 */
#ifdef DEBUG
   /* wnode_name = */		"DISABLE_IEEE_INTERRUPT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_DISABLE_IEEE_INTERRUPT,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"DISABLE_IEEE_INTERRUPT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__disable_ieee_interrupt"
 },
 { /* 664 */
#ifdef DEBUG
   /* wnode_name = */		"TEST_IEEE_EXCEPTION",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_TEST_IEEE_EXCEPTION,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"TEST_IEEE_EXCEPTION",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__test_ieee_exception"
 },
 { /* 665 */
#ifdef DEBUG
   /* wnode_name = */		"TEST_IEEE_INTERRUPT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_TEST_IEEE_INTERRUPT,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"TEST_IEEE_INTERRUPT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__test_ieee_interrupt"
 },
 { /* 666 */
#ifdef DEBUG
   /* wnode_name = */		"MATMUL",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_MATMUL,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 667 */
#ifdef DEBUG
   /* wnode_name = */		"SPREAD",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_SPREAD,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 668 */
#ifdef DEBUG
   /* wnode_name = */		"RESHAPE",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_RESHAPE,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 669 */
#ifdef DEBUG
   /* wnode_name = */		"TRANSPOSE",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_TRANSPOSE,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 670 */
#ifdef DEBUG
   /* wnode_name = */		"ALL",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_ALL,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 671 */
#ifdef DEBUG
   /* wnode_name = */		"ANY",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_ANY,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 672 */
#ifdef DEBUG
   /* wnode_name = */		"COUNT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_COUNT,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 673 */
#ifdef DEBUG
   /* wnode_name = */		"PRODUCT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_PRODUCT,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 674 */
#ifdef DEBUG
   /* wnode_name = */		"SUM",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_SUM,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 675 */
#ifdef DEBUG
   /* wnode_name = */		"EOSHIFT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_EOSHIFT,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 676 */
#ifdef DEBUG
   /* wnode_name = */		"MAXVAL",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_MAXVAL,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 677 */
#ifdef DEBUG
   /* wnode_name = */		"MINVAL",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_MINVAL,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 678 */
#ifdef DEBUG
   /* wnode_name = */		"MAXLOC",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_MAXLOC,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 679 */
#ifdef DEBUG
   /* wnode_name = */		"MINLOC",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_MINLOC,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 680 */
#ifdef DEBUG
   /* wnode_name = */		"CSHIFT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_CSHIFT,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 681 */
#ifdef DEBUG
   /* wnode_name = */		"DOT_PRODUCT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_DOT_PRODUCT,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 682 */
#ifdef DEBUG
   /* wnode_name = */		"PACK",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_PACK,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 683 */
#ifdef DEBUG
   /* wnode_name = */		"UNPACK",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_UNPACK,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 684 */
#ifdef DEBUG
   /* wnode_name = */		"MERGE",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_MERGE,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 685 */
#ifdef DEBUG
   /* wnode_name = */		"CHAR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_CHAR,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 686 */
#ifdef DEBUG
   /* wnode_name = */		"MP_IN_PARALLEL_REGION",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_MP_IN_PARALLEL_REGION,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__mp_in_parallel_region",
#else
   /* specific_name = */	"__mp_in_parallel_region$",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"mp_in_parallel_region"
 },
 { /* 687 */
#ifdef DEBUG
   /* wnode_name = */		"RT_ERR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_RT_ERR,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	TRUE,
   /* runtime_name = */		"__C_runtime_error"
 },
 { /* 688 */
#ifdef DEBUG
   /* wnode_name = */		"OMP_DO_WORKSHARING",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_OMP_DO_WORKSHARING,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__omp_do_worksharing",
#else
   /* specific_name = */	"__omp_do_worksharing$",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"omp_do_worksharing"
 },
 { /* 689 */
#ifdef DEBUG
   /* wnode_name = */		"OMP_TEST_LOCK",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_OMP_TEST_LOCK,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"OMP_TEST_LOCK",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"omp_test_lock_"
 },
 { /* 690 */
#ifdef DEBUG
   /* wnode_name = */		"OMP_GET_NUM_THREADS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_OMP_GET_NUM_THREADS,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"OMP_GET_NUM_THREADS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"omp_get_num_threads_"
 },
 { /* 691 */
#ifdef DEBUG
   /* wnode_name = */		"OMP_GET_MAX_THREADS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_OMP_GET_MAX_THREADS,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"OMP_GET_MAX_THREADS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"omp_get_max_threads_"
 },
 { /* 692 */
#ifdef DEBUG
   /* wnode_name = */		"OMP_GET_THREAD_NUM",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_OMP_GET_THREAD_NUM,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"OMP_GET_THREAD_NUM",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"omp_get_thread_num_"
 },
 { /* 693 */
#ifdef DEBUG
   /* wnode_name = */		"OMP_GET_NUM_PROCS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_OMP_GET_NUM_PROCS,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"OMP_GET_NUM_PROCS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"omp_get_num_procs_"
 },
 { /* 694 */
#ifdef DEBUG
   /* wnode_name = */		"OMP_IN_PARALLEL",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_OMP_IN_PARALLEL,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"OMP_IN_PARALLEL",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"omp_in_parallel_"
 },
 { /* 695 */
#ifdef DEBUG
   /* wnode_name = */		"OMP_GET_DYNAMIC",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_OMP_GET_DYNAMIC,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"OMP_GET_DYNAMIC",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"omp_get_dynamic_"
 },
 { /* 696 */
#ifdef DEBUG
   /* wnode_name = */		"OMP_GET_NESTED",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_OMP_GET_NESTED,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"OMP_GET_NESTED",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"omp_get_nested_"
 },
 { /* 697 */
#ifdef DEBUG
   /* wnode_name = */		"I1IEEE_INT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I1IEEE_INT,
#endif
   /* return_kind = */		RETURN_I1,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"I1IEEE_INT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_IEEE_INT_D_I1"
 },
 { /* 698 */
#ifdef DEBUG
   /* wnode_name = */		"I2IEEE_INT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I2IEEE_INT,
#endif
   /* return_kind = */		RETURN_I2,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"I2IEEE_INT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_IEEE_INT_D_I2"
 },
 { /* 699 */
#ifdef DEBUG
   /* wnode_name = */		"I4IEEE_INT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4IEEE_INT,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"I4IEEE_INT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_IEEE_INT_D_I4"
 },
 { /* 700 */
#ifdef DEBUG
   /* wnode_name = */		"I8IEEE_INT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8IEEE_INT,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"I8IEEE_INT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_IEEE_INT_D_I8"
 },
 { /* 701 */
#ifdef DEBUG
   /* wnode_name = */		"F4IEEE_INT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4IEEE_INT,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F4IEEE_INT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_IEEE_INT_D_H"
 },
 { /* 702 */
#ifdef DEBUG
   /* wnode_name = */		"F8IEEE_INT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8IEEE_INT,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F8IEEE_INT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_IEEE_INT_D_R"
 },
 { /* 703 */
#ifdef DEBUG
   /* wnode_name = */		"FQIEEE_INT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FQIEEE_INT,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"FQIEEE_INT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"_IEEE_INT_D_D"
 },
 { /* 704 */
#ifdef DEBUG
   /* wnode_name = */		"F90BOUNDS_CHECK",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F90BOUNDS_CHECK,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F90BOUNDS_CHECK",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__f90_bounds_check"
 },
 { /* 705 */
#ifdef DEBUG
   /* wnode_name = */		"F90_DYNAMICTEMPALLOC",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F90_DYNAMICTEMPALLOC,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 706 */
#ifdef DEBUG
   /* wnode_name = */		"F90_DYNAMICTEMPFREE",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F90_DYNAMICTEMPFREE,
#endif
   /* return_kind = */		RETURN_UNKNOWN,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 707 */
#ifdef DEBUG
   /* wnode_name = */		"F90CONFORM_CHECK",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F90CONFORM_CHECK,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"F90CONFORM_CHECK",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__f90_conform_check"
 },
 { /* 708 */
#ifdef DEBUG
   /* wnode_name = */		"C_F4FLOOR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C_F4FLOOR,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"floorf",
#else
   /* specific_name = */	"C_F4FLOOR",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"floorf"
 },
 { /* 709 */
#ifdef DEBUG
   /* wnode_name = */		"C_F8FLOOR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C_F8FLOOR,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"floor",
#else
   /* specific_name = */	"C_F8FLOOR",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"floor"
 },
 { /* 710 */
#ifdef DEBUG
   /* wnode_name = */		"C_FQFLOOR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C_FQFLOOR,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"floorl",
#else
   /* specific_name = */	"C_FQFLOOR",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"floorl"
 },
 { /* 711 */
#ifdef DEBUG
   /* wnode_name = */		"C_F4CEIL",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C_F4CEIL,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"ceilf",
#else
   /* specific_name = */	"C_F4CEIL",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"ceilf"
 },
 { /* 712 */
#ifdef DEBUG
   /* wnode_name = */		"C_F8CEIL",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C_F8CEIL,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"ceil",
#else
   /* specific_name = */	"C_F8CEIL",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"ceil"
 },
 { /* 713 */
#ifdef DEBUG
   /* wnode_name = */		"C_FQCEIL",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C_FQCEIL,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"ceill",
#else
   /* specific_name = */	"C_FQCEIL",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"ceill"
 },
 { /* 714 */
#ifdef DEBUG
   /* wnode_name = */		"C_F4TRUNC",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C_F4TRUNC,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"truncf",
#else
   /* specific_name = */	"C_F4TRUNC",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"truncf"
 },
 { /* 715 */
#ifdef DEBUG
   /* wnode_name = */		"C_F8TRUNC",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C_F8TRUNC,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"trunc",
#else
   /* specific_name = */	"C_F8TRUNC",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"trunc"
 },
 { /* 716 */
#ifdef DEBUG
   /* wnode_name = */		"C_FQTRUNC",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_C_FQTRUNC,
#endif
   /* return_kind = */		RETURN_FQ,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"truncl",
#else
   /* specific_name = */	"C_FQTRUNC",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"truncl"
 },
 { /* 717 */
#ifdef DEBUG
   /* wnode_name = */		"I4DSHIFTL",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4DSHIFTL,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__dshiftl4",
#else
   /* specific_name = */	"I4DSHIFTL",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__dshiftl4"
 },
 { /* 718 */
#ifdef DEBUG
   /* wnode_name = */		"I8DSHIFTL",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8DSHIFTL,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__dshiftl8",
#else
   /* specific_name = */	"I8DSHIFTL",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__dshiftl8"
 },
 { /* 719 */
#ifdef DEBUG
   /* wnode_name = */		"I4DSHIFTR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4DSHIFTR,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__dshiftr4",
#else
   /* specific_name = */	"I4DSHIFTR",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__dshiftr4"
 },
 { /* 720 */
#ifdef DEBUG
   /* wnode_name = */		"I8DSHIFTR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8DSHIFTR,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__dshiftr8",
#else
   /* specific_name = */	"I8DSHIFTR",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__dshiftr8"
 },
 { /* 721 */
#ifdef DEBUG
   /* wnode_name = */		"I4GBIT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4GBIT,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__gbit4",
#else
   /* specific_name = */	"I4GBIT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__gbit4"
 },
 { /* 722 */
#ifdef DEBUG
   /* wnode_name = */		"I8GBIT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8GBIT,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__gbit8",
#else
   /* specific_name = */	"I8GBIT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__gbit8"
 },
 { /* 723 */
#ifdef DEBUG
   /* wnode_name = */		"I4GBITS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4GBITS,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__gbits4",
#else
   /* specific_name = */	"I4GBITS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__gbits4"
 },
 { /* 724 */
#ifdef DEBUG
   /* wnode_name = */		"I8GBITS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8GBITS,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__gbits8",
#else
   /* specific_name = */	"I8GBITS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__gbits8"
 },
 { /* 725 */
#ifdef DEBUG
   /* wnode_name = */		"I4MASK",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4MASK,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__mask4",
#else
   /* specific_name = */	"I4MASK",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__mask4"
 },
 { /* 726 */
#ifdef DEBUG
   /* wnode_name = */		"I8MASK",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8MASK,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__mask8",
#else
   /* specific_name = */	"I8MASK",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__mask8"
 },
 { /* 727 */
#ifdef DEBUG
   /* wnode_name = */		"I4MASKL",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4MASKL,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__maskl4",
#else
   /* specific_name = */	"I4MASKL",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__maskl4"
 },
 { /* 728 */
#ifdef DEBUG
   /* wnode_name = */		"I8MASKL",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8MASKL,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__maskl8",
#else
   /* specific_name = */	"I8MASKL",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__maskl8"
 },
 { /* 729 */
#ifdef DEBUG
   /* wnode_name = */		"I4MASKR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4MASKR,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__maskr4",
#else
   /* specific_name = */	"I4MASKR",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__maskr4"
 },
 { /* 730 */
#ifdef DEBUG
   /* wnode_name = */		"I8MASKR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8MASKR,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__maskr8",
#else
   /* specific_name = */	"I8MASKR",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__maskr8"
 },
 { /* 731 */
#ifdef DEBUG
   /* wnode_name = */		"I4PBIT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4PBIT,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__pbit4",
#else
   /* specific_name = */	"I4PBIT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__pbit4"
 },
 { /* 732 */
#ifdef DEBUG
   /* wnode_name = */		"I8PBIT",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8PBIT,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__pbit8",
#else
   /* specific_name = */	"I8PBIT",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__pbit8"
 },
 { /* 733 */
#ifdef DEBUG
   /* wnode_name = */		"I4PBITS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4PBITS,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__pbits4",
#else
   /* specific_name = */	"I4PBITS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__pbits4"
 },
 { /* 734 */
#ifdef DEBUG
   /* wnode_name = */		"I8PBITS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8PBITS,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__pbits8",
#else
   /* specific_name = */	"I8PBITS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__pbits8"
 },
 { /* 735 */
#ifdef DEBUG
   /* wnode_name = */		"I4POPPAR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4POPPAR,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__poppar4",
#else
   /* specific_name = */	"I4POPPAR",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__poppar4"
 },
 { /* 736 */
#ifdef DEBUG
   /* wnode_name = */		"I8POPPAR",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8POPPAR,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__poppar8",
#else
   /* specific_name = */	"I8POPPAR",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__poppar8"
 },
 { /* 737 */
#ifdef DEBUG
   /* wnode_name = */		"I4RTC",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4RTC,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__rtc4",
#else
   /* specific_name = */	"I4RTC",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__rtc4"
 },
 { /* 738 */
#ifdef DEBUG
   /* wnode_name = */		"I8RTC",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I8RTC,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__rtc8",
#else
   /* specific_name = */	"I8RTC",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__rtc8"
 },
 { /* 739 */
#ifdef DEBUG
   /* wnode_name = */		"GETF_EXP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_GETF_EXP,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__builtin_getf_exp",
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 740 */
#ifdef DEBUG
   /* wnode_name = */		"SETF_EXP",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_SETF_EXP,
#endif
   /* return_kind = */		RETURN_F10,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__builtin_setf_exp",
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 741 */
#ifdef DEBUG
   /* wnode_name = */		"GETF_SIG",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_GETF_SIG,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__builtin_getf_sig",
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 742 */
#ifdef DEBUG
   /* wnode_name = */		"SETF_SIG",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_SETF_SIG,
#endif
   /* return_kind = */		RETURN_F10,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__builtin_setf_sig",
#else
   /* specific_name = */	NULL,
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		TRUE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		NULL
 },
 { /* 743 */
#ifdef DEBUG
   /* wnode_name = */		"FMERGE_NS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FMERGE_NS,
#endif
   /* return_kind = */		RETURN_F10,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__builtin_fmerge_ns",
#else
   /* specific_name = */	"FMERGE_NS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__builtin_fmerge_ns"
 },
 { /* 744 */
#ifdef DEBUG
   /* wnode_name = */		"FMERGE_S",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FMERGE_S,
#endif
   /* return_kind = */		RETURN_F10,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__builtin_fmerge_s",
#else
   /* specific_name = */	"FMERGE_S",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__builtin_fmerge_s"
 },
 { /* 745 */
#ifdef DEBUG
   /* wnode_name = */		"FMERGE_SE",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_FMERGE_SE,
#endif
   /* return_kind = */		RETURN_F10,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__builtin_fmerge_se",
#else
   /* specific_name = */	"FMERGE_SE",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__builtin_fmerge_se"
 },
 { /* 746 */
#ifdef DEBUG
   /* wnode_name = */		"STOP_F90",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_STOP_F90,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		NULL,
#else
   /* specific_name = */	"_F90_STOP",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		FALSE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	TRUE,
   /* runtime_name = */		"_F90_STOP"
 },
 { /* 747 */
#ifdef DEBUG
   /* wnode_name = */		"F4VLOG10",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F4VLOG10,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"vlog10f",
#else
   /* specific_name = */	"VLOG10F",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"vlog10f"
 },
 { /* 748 */
#ifdef DEBUG
   /* wnode_name = */		"F8VLOG10",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_F8VLOG10,
#endif
   /* return_kind = */		RETURN_V,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"vlog10",
#else
   /* specific_name = */	"VLOG10",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		FALSE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	FALSE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"vlog10"
 },
 { /* 749 */
#ifdef DEBUG
   /* wnode_name = */		"MODSI3",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_MODSI3,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__modsi3",
#else
   /* specific_name = */	"__modsi3",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__modsi3"
 },
 { /* 750 */
#ifdef DEBUG
   /* wnode_name = */		"UMODSI3",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_UMODSI3,
#endif
   /* return_kind = */		RETURN_U4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__umodsi3",
#else
   /* specific_name = */	"__umodsi3",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__umodsi3"
 },
 { /* 751 */
#ifdef DEBUG
   /* wnode_name = */		"DIVSI3",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_DIVSI3,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__divsi3",
#else
   /* specific_name = */	"__divsi3",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__divsi3"
 },
 { /* 752 */
#ifdef DEBUG
   /* wnode_name = */		"UDIVSI3",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_UDIVSI3,
#endif
   /* return_kind = */		RETURN_U4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__udivsi3",
#else
   /* specific_name = */	"__udivsi3",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__udivsi3"
 },
 { /* 753 */
#ifdef DEBUG
   /* wnode_name = */		"MODDI3",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_MODDI3,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__moddi3",
#else
   /* specific_name = */	"__moddi3",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__moddi3"
 },
 { /* 754 */
#ifdef DEBUG
   /* wnode_name = */		"UMODDI3",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_UMODDI3,
#endif
   /* return_kind = */		RETURN_U8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__umoddi3",
#else
   /* specific_name = */	"__umoddi3",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__umoddi3"
 },
 { /* 755 */
#ifdef DEBUG
   /* wnode_name = */		"DIVDI3",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_DIVDI3,
#endif
   /* return_kind = */		RETURN_I8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__divdi3",
#else
   /* specific_name = */	"__divdi3",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__divdi3"
 },
 { /* 756 */
#ifdef DEBUG
   /* wnode_name = */		"UDIVDI3",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_UDIVDI3,
#endif
   /* return_kind = */		RETURN_U8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__udivdi3",
#else
   /* specific_name = */	"__udivdi3",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__udivdi3"
 },
 { /* 757 */
#ifdef DEBUG
   /* wnode_name = */		"DIVSF3",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_DIVSF3,
#endif
   /* return_kind = */		RETURN_F4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__divsf3",
#else
   /* specific_name = */	"__divsf3",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__divsf3"
 },
 { /* 758 */
#ifdef DEBUG
   /* wnode_name = */		"DIVDF3",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_DIVDF3,
#endif
   /* return_kind = */		RETURN_F8,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"__divdf3",
#else
   /* specific_name = */	"__divdf3",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"__divdf3"
 },
 { /* 759 */
#ifdef DEBUG
   /* wnode_name = */		"I4FFS",
#endif
#ifdef CHECKING
   /* wnode = */		INTRN_I4FFS,
#endif
   /* return_kind = */		RETURN_I4,
#ifdef BUILD_WHIRL2C
   /* c_name = */		"ffs",
#else
   /* specific_name = */	"I4FFS",
#endif
#ifdef MONGOOSE_BE
   /* is_actual_argument = */	FALSE,
   /* cg_intrinsic = */		FALSE,
#endif
   /* is_by_value = */		TRUE,
   /* is_pure = */		TRUE,
   /* has_no_side_effects = */	TRUE,
   /* never_returns = */	FALSE,
   /* runtime_name = */		"ffs"
 }
}
#endif /* VAR_INITIALIZERS */
;
