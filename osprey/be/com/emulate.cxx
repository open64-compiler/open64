/*
 * Copyright (C) 2008-2011 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright (C) 2007, 2008 PathScale, LLC.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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



#ifdef USE_PCH
#include "be_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#include <math.h>
#if defined(BUILD_OS_DARWIN)
#include <limits.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <values.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <alloca.h>

#include "defs.h"
#include "config.h"
#include "config_debug.h"
#include "config_opt.h"
#include "config_targ_opt.h"
#include "errors.h"
#include "erglob.h"
#include "erbe.h"
#include "tracing.h"
#include "stab.h"
#include "data_layout.h"
#include "wn.h"
#include "wn_util.h"
#include "const.h"
#include "targ_const.h"
#include "targ_sim.h"
#include "fb_whirl.h"
#include "be_symtab.h"
#include "intrn_info.h"
#include "alias_analyzer.h"


#if (__GNUC__ == 2)
//
// Provide trunc(), which doesn't exist in the GNU library. This is a
// quick and dirty hack, and should be handled elsehow.
//
static inline double trunc(double d)
{
  if (d < 0.0) {
    return 1.0 + floor(d);
  }
  else {
    return floor(d);
  }
}
#endif


/*
**	For lack of a better word, these emulations are run time
**	routines that supply functionality to whirl expression nodes
**
**	The list was ripped off from ragnarok and may be
**	incomplete/NYI
*/
typedef enum
{
  EM_TRAPUV,		/* sets fpc_csr to interrupt on NaN */
  EM_RTS_CHECKSTACK,	/* checks for stack overflow */
  
  EM_LL_MUL,		/* double-word multiply */
  EM_LL_DIV,		/* double-word divide */
  EM_ULL_DIV,		/* unsigned double-word divide */
  EM_LL_MOD,		/* double-word mod */
  EM_LL_REM,		/* double-word remainder */
  EM_ULL_REM,		/* unsigned double-word remainder */
  EM_LL_LSHIFT,		/* double-word left shift */
  EM_LL_RSHIFT,		/* double-word right shift */
  EM_ULL_RSHIFT,	/* unsigned double-word right shift */
  EM_LL_M3_DSLLV,	/* mips 3 simulation of dsllv */
  EM_LL_M3_DSRAV,	/* mips 3 simulation of dsrav */
  EM_LL_M3_DSRLV,	/* mips 3 simulation of dsrlv */
  EM_LL_TO_F,		/* cvt double-word to float */
  EM_ULL_TO_F,		/* cvt unsigned double-word to float */
  EM_LL_TO_D,		/* cvt double-word to double float */
  EM_ULL_TO_D,		/* cvt unsigned double-word to double float */
  EM_F_TO_LL,		/* cvt float to double-word */
  EM_F_TO_ULL,		/* cvt float to unsigned double-word */
  EM_F_ROUND_LL_F,	/* round float to float */
  EM_F_TRUNC_LL_F,	/* trunc float to float */
  EM_D_TO_LL,		/* cvt double float to double-word */
  EM_D_TO_ULL,		/* cvt double float to unsigned double-word */
  EM_D_ROUND_LL_D,	/* round double to double */
  EM_D_TRUNC_LL_D,	/* trunc double to double */
  EM_LL_BIT_EXTRACT ,	/* double-word bit-field extraction */
  EM_LL_BIT_INSERT ,	/* double-word bit-field insertion */
  
  EM_Q_ABS,		/* quad absolute value */
  EM_Q_SQRT,		/* quad square root */
  EM_Q_ADD,		/* quad plus */
  EM_Q_SUB,		/* quad minus */
  EM_Q_MPY,		/* quad multiply */
  EM_Q_DIV,		/* quad divide */
  EM_Q_MAX1,		/* quad max */
  EM_Q_MIN1,		/* quad min */
  EM_Q_EQ,		/* quad equal */
  EM_Q_NE,		/* quad not equal */
  EM_Q_GE,		/* quad greater equal */
  EM_Q_GT,		/* quad greater than */
  EM_Q_LE,		/* quad less equal */
  EM_Q_LT,		/* quad less than */
  EM_SNGL_Q,		/* convert quad to single */
  EM_DBLE_Q,		/* convert quad to double */
#ifdef TARG_LOONGSON
  EM_Q_NEG,             /* quad negate */
  EM_KU_QINT,		/* convert quad to unsigned 64 bits int */
  EM_JU_QINT,		/* convert quad to unsigned 32 bits int */
#endif
  EM_KI_QINT,		/* convert quad to 64 bits int */
  EM_JI_QINT,		/* convert quad to 32 bits int */
  EM_Q_EXT,		/* convert float to quad */
  EM_Q_EXTD,		/* convert double to quad */
  EM_Q_FLOTK,		/* convert to quad from 64 bits int */
  EM_Q_FLOTKU,		/* convert to quad from unsigned 64 bits int */
  EM_Q_FLOTJ,		/* convert to quad from 32 bits int */
  EM_Q_FLOTJU,		/* convert to quad from unsigned 32 bits int */
  EM_KIQNNT,		/* round quad to closest 64 bits int value */
  EM_JIQNNT,		/* round quad to closest 32 bits int value */

  EM_C4_SQRT,		/* float complex sqrt */
  EM_C8_SQRT,		/* double complex sqrt */
  EM_CQ_SQRT,		/* quad complex sqrt */
  EM_C4_RSQRT,		/* float complex recipricol sqrt */
  EM_C8_RSQRT,		/* double complex recipricol sqrt */
  EM_CQ_RSQRT,		/* quad complex recipricol sqrt */

  EM_C4_ABS,		/* float complex abs */
  EM_C8_ABS,		/* double complex abs */
  EM_CQ_ABS,		/* quad complex abs */
  EM_KI_QCEIL,		/* ceil quad to 64 bits int (f90 only) */
  EM_JI_QCEIL,		/* ceil quad to 32 bits in( f90 only)t */
  EM_KI_QFLOOR,		/* floor quad to 64 bits int (f90 only) */
  EM_JI_QFLOOR,		/* floor quad to 32 bits int (f90 only) */
  EM_LAST		/* sentinel */
} EMULATION;


/*
**	describe calling semantics for FE and runtime 
**	intrinsics and expression
*/
typedef enum
{
  COERCE_none,
  COERCE_by_reference,
  COERCE_by_value,
  COERCE_struct_by_value,
  COERCE_struct_by_reference,
  COERCE_split_complex
} COERCE, *COERCEp;

typedef struct EM_ROUTINES
{
  EMULATION	id;
  const char   *functionName;
  INT32		functionAttributes;
  COERCE	runtimeArg0coercion;
} EM_ROUTINES,  *EM_ROUTINESp;

#define	EM_id(x)		em_routines[x].id
#define	EM_rt_name(x)		em_routines[x].functionName
#define	EM_attributes(x)	em_routines[x].functionAttributes
#define	EM_coerce0(x)		em_routines[x].runtimeArg0coercion

/*
**	Keep track of intrinsic/emulation arguments
**	Problems we are trying to solve
**
**	COERCE_by_reference
**		are (unfortunately) provided by the FE to match the
**		run time routine. When we get the argument we might have
**		an address (anonymous pointer) and hence, lost the
**		type to dereference (if we are trying to inline it)
**
**	COERCE_split_complex
**		complex are split into real/imaginary pairs doubling
**		the number of argumemts
**	
**	This entire mechanism should be provided by the FE
**	as part of wtable.h
*/

#define NSE		PU_NO_SIDE_EFFECTS
#define	PURE_NSE	(PU_IS_PURE | NSE)
#define	INVALID		NULL

/*
**	The emulation table may not yet be complete (or used)
**	The fields are
**
**	EMULATION id;
**		The table must be kept in order with the enumeration
**		as it is a direct lookup
**	
**	char	*functionName;
**		The exact external name, no underbars 
**
**	INT32	functionAttributes;
**
**	COERCEp	functionArgCoercion;
**		Actual to runtime formal conversion
**		The child of an expression/intrinsic WN needs to be
**		converted to call it's runtime function.
**		ex.
**		  complex routines are now split-by_value
**
**	These routines are all by value so we already know the
**	argument type
*/

#define NONE 0
const EM_ROUTINES em_routines[]=
{
  EM_TRAPUV,        "__trapuv",      PURE_NSE,  COERCE_none,
  EM_RTS_CHECKSTACK,"_RtlCheckStack",PURE_NSE,  COERCE_none,
  EM_LL_MUL,        "__ll_mul",      PURE_NSE,  COERCE_none,
  EM_LL_DIV,        "__ll_div",      PURE_NSE,  COERCE_none,
  EM_ULL_DIV,       "__ull_div",     PURE_NSE,  COERCE_none,
  EM_LL_MOD,        "__ll_mod",      PURE_NSE,  COERCE_none,
  EM_LL_REM,        "__ll_rem",      PURE_NSE,  COERCE_none,
  EM_ULL_REM,       "__ull_rem",     PURE_NSE,  COERCE_none,
  EM_LL_LSHIFT,     "__ll_lshift",   PURE_NSE,  COERCE_none,
  EM_LL_RSHIFT,     "__ll_rshift",   PURE_NSE,  COERCE_none,
  EM_ULL_RSHIFT,    "__ull_rshift",  PURE_NSE,  COERCE_none,
  EM_LL_M3_DSLLV,   "__dsllv",       PURE_NSE,  COERCE_none,
  EM_LL_M3_DSRAV,   "__dsrav",       PURE_NSE,  COERCE_none,
  EM_LL_M3_DSRLV,   "__dsrlv",       PURE_NSE,  COERCE_none,
  EM_LL_TO_F,       "__ll_to_f",     PURE_NSE,  COERCE_none,
  EM_ULL_TO_F,      "__ull_to_f",    PURE_NSE,  COERCE_none,
  EM_LL_TO_D,       "__ll_to_d",     PURE_NSE,  COERCE_none,
  EM_ULL_TO_D,      "__ull_to_d",    PURE_NSE,  COERCE_none,
  EM_F_TO_LL,       "__f_to_ll",     PURE_NSE,  COERCE_none,
  EM_F_TO_ULL,      "__f_to_ull",    PURE_NSE,  COERCE_none,
  EM_F_ROUND_LL_F,  "__f_round_ll_f",PURE_NSE,  COERCE_none,
  EM_F_TRUNC_LL_F,  "__f_trunc_ll_f",PURE_NSE,  COERCE_none,
  EM_D_TO_LL,       "__d_to_ll",     PURE_NSE,  COERCE_none,
  EM_D_TO_ULL,      "__d_to_ull",    PURE_NSE,  COERCE_none,
  EM_D_ROUND_LL_D,  "__d_round_ll_d",PURE_NSE,  COERCE_none,
  EM_D_TRUNC_LL_D,  "__d_trunc_ll_d",PURE_NSE,  COERCE_none,
  EM_LL_BIT_EXTRACT,"__ll_bit_extract",PURE_NSE,COERCE_none,
  EM_LL_BIT_INSERT, "__ll_bit_insert",PURE_NSE, COERCE_none,
#ifdef TARG_LOONGSON
  EM_Q_ABS,         "fabsl",         PURE_NSE,  COERCE_none,
  EM_Q_SQRT,        "__qsqrt",       PURE_NSE,  COERCE_none,
  EM_Q_ADD,         "__addtf3",      PURE_NSE,  COERCE_none,
  EM_Q_SUB,         "__subtf3",      PURE_NSE,  COERCE_none,
  EM_Q_MPY,         "__multf3",      PURE_NSE,  COERCE_none,
  EM_Q_DIV,         "__divtf3",      PURE_NSE,  COERCE_none,
  EM_Q_MAX1,        "__q_max1",      PURE_NSE,  COERCE_none,
  EM_Q_MIN1,        "__q_min1",      PURE_NSE,  COERCE_none,
  EM_Q_EQ,          "__eqtf2",       PURE_NSE,  COERCE_none,
  EM_Q_NE,          "__netf2",       PURE_NSE,  COERCE_none,
  EM_Q_GE,          "__getf2",       PURE_NSE,  COERCE_none,
  EM_Q_GT,          "__gttf2",       PURE_NSE,  COERCE_none,
  EM_Q_LE,          "__letf2",       PURE_NSE,  COERCE_none,
  EM_Q_LT,          "__lttf2",       PURE_NSE,  COERCE_none,
  EM_SNGL_Q,        "__trunctfsf2",  PURE_NSE,  COERCE_none,
  EM_DBLE_Q,        "__trunctfdf2",  PURE_NSE,  COERCE_none,
  EM_Q_NEG,         "__negtf2",      PURE_NSE,  COERCE_none,
  EM_KU_QINT,       "__fixunstfdi",  PURE_NSE,  COERCE_none,
  EM_JU_QINT,       "__fixunstfsi",  PURE_NSE,  COERCE_none,
  EM_KI_QINT,       "__fixtfdi",     PURE_NSE,  COERCE_none,
  EM_JI_QINT,       "__fixtfsi",     PURE_NSE,  COERCE_none,
  EM_Q_EXT,         "__extendsftf2", PURE_NSE,  COERCE_none,
  EM_Q_EXTD,        "__extenddftf2", PURE_NSE,  COERCE_none,
  EM_Q_FLOTK,       "__floatditf",   PURE_NSE,  COERCE_none,
  EM_Q_FLOTKU,      "__floatunditf", PURE_NSE,  COERCE_none,
  EM_Q_FLOTJ,       "__floatsitf",   PURE_NSE,  COERCE_none,
  EM_Q_FLOTJU,      "__floatunsitf", PURE_NSE,  COERCE_none,
  EM_KIQNNT,        "__kiqnnt",      PURE_NSE,  COERCE_none,
  EM_JIQNNT,        "__jiqnnt",      PURE_NSE,  COERCE_none,
#else  
  EM_Q_ABS,         "__qabs",        PURE_NSE,  COERCE_none,
  EM_Q_SQRT,        "__qsqrt",       PURE_NSE,  COERCE_none,
  EM_Q_ADD,         "__q_add",       PURE_NSE,  COERCE_none,
  EM_Q_SUB,         "__q_sub",       PURE_NSE,  COERCE_none,
  EM_Q_MPY,         "__q_mul",       PURE_NSE,  COERCE_none,
  EM_Q_DIV,         "__q_div",       PURE_NSE,  COERCE_none,
  EM_Q_MAX1,        "__q_max1",      PURE_NSE,  COERCE_none,
  EM_Q_MIN1,        "__q_min1",      PURE_NSE,  COERCE_none,
  EM_Q_EQ,          "__q_eq",        PURE_NSE,  COERCE_none,
  EM_Q_NE,          "__q_ne",        PURE_NSE,  COERCE_none,
  EM_Q_GE,          "__q_ge",        PURE_NSE,  COERCE_none,
  EM_Q_GT,          "__q_gt",        PURE_NSE,  COERCE_none,
  EM_Q_LE,          "__q_le",        PURE_NSE,  COERCE_none,
  EM_Q_LT,          "__q_lt",        PURE_NSE,  COERCE_none,
  EM_SNGL_Q,        "__sngl_q",      PURE_NSE,  COERCE_none,
  EM_DBLE_Q,        "__dble_q",      PURE_NSE,  COERCE_none,
  EM_KI_QINT,       "__ki_qint",     PURE_NSE,  COERCE_none,
  EM_JI_QINT,       "__ji_qint",     PURE_NSE,  COERCE_none,
  EM_Q_EXT,         "__q_ext",       PURE_NSE,  COERCE_none,
  EM_Q_EXTD,        "__q_extd",      PURE_NSE,  COERCE_none,
  EM_Q_FLOTK,       "__q_flotk",     PURE_NSE,  COERCE_none,
  EM_Q_FLOTKU,      "__q_flotku",    PURE_NSE,  COERCE_none,
  EM_Q_FLOTJ,       "__q_flotj",     PURE_NSE,  COERCE_none,
  EM_Q_FLOTJU,      "__q_flotju",    PURE_NSE,  COERCE_none,
#endif
  EM_KIQNNT,        "__kiqnnt",      PURE_NSE,  COERCE_none,
  EM_JIQNNT,        "__jiqnnt",      PURE_NSE,  COERCE_none,
  EM_C4_SQRT,       "__csqrt",       PURE_NSE,  COERCE_split_complex,
  EM_C8_SQRT,       "__zsqrt",       PURE_NSE,  COERCE_split_complex,
  EM_CQ_SQRT,       "__cqsqrt",      PURE_NSE,  COERCE_split_complex,
  EM_C4_RSQRT,      INVALID,         NONE,      COERCE_none,
  EM_C8_RSQRT,      INVALID,         NONE,      COERCE_none,
  EM_CQ_RSQRT,      INVALID,         NONE,      COERCE_none,
  EM_C4_ABS,        INVALID,         NONE,      COERCE_none,
  EM_C8_ABS,        INVALID,         NONE,      COERCE_none,
  EM_CQ_ABS,        INVALID,         NONE,      COERCE_none,

  EM_KI_QCEIL,	    "_CEILING_16_8", PURE_NSE,  COERCE_none,
  EM_JI_QCEIL,	    "_CEILING_16_4", PURE_NSE,  COERCE_none,
  EM_KI_QFLOOR,	    "_FLOOR_16_8",   PURE_NSE,  COERCE_none,
  EM_JI_QFLOOR,	    "_FLOOR_16_4",   PURE_NSE,  COERCE_none,
};

typedef struct
{
  INTRINSIC	id;
  COERCE	runtimeArg0;
  COERCE	runtimeArg1;
} INTRINSIC_RUNTIME_FORMALS;

#define	INTR_id(x)		intrinsic_runtime_formals[(x)].id
#define	INTR_coerce0(x)		intrinsic_runtime_formals[(x)].runtimeArg0
#define	INTR_coerce1(x)		intrinsic_runtime_formals[(x)].runtimeArg1

/*
**	TODO
**	eventually the FE will supply this information
**	from the intrinsic table, when we finish the implementation
*/
INTRINSIC_RUNTIME_FORMALS intrinsic_runtime_formals[]=
{
  INTRN_C4I4EXPEXPR,	COERCE_split_complex,	COERCE_none,
  INTRN_C4I8EXPEXPR,	COERCE_split_complex,	COERCE_none,
  INTRN_C8I4EXPEXPR,	COERCE_split_complex,	COERCE_none,
  INTRN_C8I8EXPEXPR,	COERCE_split_complex,	COERCE_none,
  INTRN_CQI4EXPEXPR,	COERCE_split_complex,	COERCE_none,
  INTRN_CQI8EXPEXPR,	COERCE_split_complex,	COERCE_none,

  INTRN_C4EXPEXPR,	COERCE_split_complex,	COERCE_split_complex,
  INTRN_C8EXPEXPR,	COERCE_split_complex,	COERCE_split_complex,
  INTRN_CQEXPEXPR,	COERCE_split_complex,	COERCE_split_complex,
  INTRN_F4C4ABS,	COERCE_split_complex,	COERCE_none,
  INTRN_F8C8ABS,	COERCE_split_complex,	COERCE_none,
  INTRN_FQCQABS,	COERCE_split_complex,	COERCE_none,
  INTRN_C4EXP,		COERCE_split_complex,	COERCE_none,
  INTRN_C8EXP,		COERCE_split_complex,	COERCE_none,
  INTRN_CQEXP,		COERCE_split_complex,	COERCE_none,
  INTRN_C4LOG,		COERCE_split_complex,	COERCE_none,
  INTRN_C8LOG,		COERCE_split_complex,	COERCE_none,
  INTRN_CQLOG,		COERCE_split_complex,	COERCE_none,
  INTRN_C4COS,		COERCE_split_complex,	COERCE_none,
  INTRN_C8COS,		COERCE_split_complex,	COERCE_none,
  INTRN_CQCOS,		COERCE_split_complex,	COERCE_none,
  INTRN_C4SIN,		COERCE_split_complex,	COERCE_none,
  INTRN_C8SIN,		COERCE_split_complex,	COERCE_none,
  INTRN_CQSIN,		COERCE_split_complex,	COERCE_none
};

INT32 intrinsic_runtime_formals_size = sizeof(intrinsic_runtime_formals) /
				       sizeof( INTRINSIC_RUNTIME_FORMALS);

typedef struct
{
  INTRINSIC	id;
  TYPE_ID	parameterType0;
  TYPE_ID	parameterType1;
  TYPE_ID	parameterType2;
} INTRINSIC_PARAMETER_TYPE;

#define	INTR_parm_id(x)		intrinsic_parameter_type[(x)].id
#define	INTR_parmtype0(x)	intrinsic_parameter_type[(x)].parameterType0
#define	INTR_parmtype1(x)	intrinsic_parameter_type[(x)].parameterType1
#define	INTR_parmtype2(x)	intrinsic_parameter_type[(x)].parameterType2

INTRINSIC_PARAMETER_TYPE intrinsic_parameter_type[]=
{
  INTRN_I1DIM,		MTYPE_I1,	MTYPE_I1,	MTYPE_V,
  INTRN_I2DIM,		MTYPE_I2,	MTYPE_I2,	MTYPE_V,
  INTRN_I4DIM,		MTYPE_I4,	MTYPE_I4,	MTYPE_V,
  INTRN_I8DIM,		MTYPE_I8,	MTYPE_I8,	MTYPE_V,
  INTRN_F4DIM,		MTYPE_F4,	MTYPE_F4,	MTYPE_V,
  INTRN_F8DIM,		MTYPE_F8,	MTYPE_F8,	MTYPE_V,
  INTRN_FQDIM,		MTYPE_FQ,	MTYPE_FQ,	MTYPE_V,

  INTRN_F4MOD,		MTYPE_F4,	MTYPE_F4,	MTYPE_V,
  INTRN_F8MOD,		MTYPE_F8,	MTYPE_F8,	MTYPE_V,
  INTRN_FQMOD,		MTYPE_FQ,	MTYPE_FQ,	MTYPE_V,

  INTRN_F8F4PROD,	MTYPE_F4,	MTYPE_F4,	MTYPE_V,
  INTRN_FQF8PROD,	MTYPE_F8,	MTYPE_F8,	MTYPE_V,

  INTRN_I1SIGN,		MTYPE_I1,	MTYPE_I1,	MTYPE_V,
  INTRN_I2SIGN,		MTYPE_I2,	MTYPE_I2,	MTYPE_V,
  INTRN_I4SIGN,		MTYPE_I4,	MTYPE_I4,	MTYPE_V,
  INTRN_I8SIGN,		MTYPE_I8,	MTYPE_I8,	MTYPE_V,
  INTRN_F4SIGN,		MTYPE_F4,	MTYPE_F4,	MTYPE_V,
  INTRN_F8SIGN,		MTYPE_F8,	MTYPE_F8,	MTYPE_V,
  INTRN_FQSIGN,		MTYPE_FQ,	MTYPE_FQ,	MTYPE_V,

  INTRN_F4AINT,		MTYPE_F4,	MTYPE_V,	MTYPE_V,
  INTRN_F8AINT,		MTYPE_F8,	MTYPE_V,	MTYPE_V,
  INTRN_FQAINT,		MTYPE_FQ,	MTYPE_V,	MTYPE_V,

  INTRN_I2F4NINT,	MTYPE_F4,	MTYPE_V,	MTYPE_V,
  INTRN_I4F4NINT,	MTYPE_F4,	MTYPE_V,	MTYPE_V,
  INTRN_I8F4NINT,	MTYPE_F4,	MTYPE_V,	MTYPE_V,
  INTRN_I2F8IDNINT,	MTYPE_F8,	MTYPE_V,	MTYPE_V,
  INTRN_I4F8IDNINT,	MTYPE_F8,	MTYPE_V,	MTYPE_V,
  INTRN_I8F8IDNINT,	MTYPE_F8,	MTYPE_V,	MTYPE_V,
  INTRN_I2FQIQNINT,	MTYPE_FQ,	MTYPE_V,	MTYPE_V,
  INTRN_I4FQIQNINT,	MTYPE_FQ,	MTYPE_V,	MTYPE_V,
  INTRN_I8FQIQNINT,	MTYPE_FQ,	MTYPE_V,	MTYPE_V,

  INTRN_F4ANINT,	MTYPE_F4,	MTYPE_V,	MTYPE_V,
  INTRN_F8ANINT,	MTYPE_F8,	MTYPE_V,	MTYPE_V,
  INTRN_FQANINT,	MTYPE_FQ,	MTYPE_V,	MTYPE_V,

  INTRN_F4LOG10,	MTYPE_F4,	MTYPE_V,	MTYPE_V,
  INTRN_F8LOG10,	MTYPE_F8,	MTYPE_V,	MTYPE_V,
  INTRN_FQLOG10,	MTYPE_FQ,	MTYPE_V,	MTYPE_V,

  INTRN_I1BTEST,	MTYPE_I1,	MTYPE_I1,	MTYPE_V,
  INTRN_I2BTEST,	MTYPE_I2,	MTYPE_I2,	MTYPE_V,
  INTRN_I4BTEST,	MTYPE_I4,	MTYPE_I4,	MTYPE_V,
  INTRN_I8BTEST,	MTYPE_I8,	MTYPE_I8,	MTYPE_V,

  INTRN_I1BSET,		MTYPE_I1,	MTYPE_I1,	MTYPE_V,
  INTRN_I2BSET,		MTYPE_I2,	MTYPE_I2,	MTYPE_V,
  INTRN_I4BSET,		MTYPE_I4,	MTYPE_I4,	MTYPE_V,
  INTRN_I8BSET,		MTYPE_I8,	MTYPE_I8,	MTYPE_V,

  INTRN_I1BCLR,		MTYPE_I1,	MTYPE_I1,	MTYPE_V,
  INTRN_I2BCLR,		MTYPE_I2,	MTYPE_I2,	MTYPE_V,
  INTRN_I4BCLR,		MTYPE_I4,	MTYPE_I4,	MTYPE_V,
  INTRN_I8BCLR,		MTYPE_I8,	MTYPE_I8,	MTYPE_V,

  INTRN_I1BITS,		MTYPE_I1,	MTYPE_I1,	MTYPE_I1,
  INTRN_I2BITS,		MTYPE_I2,	MTYPE_I2,	MTYPE_I2,
  INTRN_I4BITS,		MTYPE_I4,	MTYPE_I4,	MTYPE_I4,
  INTRN_I8BITS,		MTYPE_I8,	MTYPE_I8,	MTYPE_I8,

  INTRN_I1SHL,		MTYPE_I1,	MTYPE_I1,	MTYPE_V,
  INTRN_I2SHL,		MTYPE_I2,	MTYPE_I2,	MTYPE_V,

  INTRN_I1SHR,		MTYPE_I1,	MTYPE_I1,	MTYPE_V,
  INTRN_I2SHR,		MTYPE_I2,	MTYPE_I2,	MTYPE_V,

  INTRN_I1SHFT,		MTYPE_I1,	MTYPE_I1,	MTYPE_V,
  INTRN_I2SHFT,		MTYPE_I2,	MTYPE_I2,	MTYPE_V,
  INTRN_I4SHFT,		MTYPE_I4,	MTYPE_I4,	MTYPE_V,
  INTRN_I8SHFT,		MTYPE_I8,	MTYPE_I8,	MTYPE_V,

  INTRN_I1SHFTC,	MTYPE_I1,	MTYPE_I1,	MTYPE_I1,
  INTRN_I2SHFTC,	MTYPE_I2,	MTYPE_I2,	MTYPE_I2,
  INTRN_I4SHFTC,	MTYPE_I4,	MTYPE_I4,	MTYPE_I4,
  INTRN_I8SHFTC,	MTYPE_I8,	MTYPE_I8,	MTYPE_I8,

  INTRN_I1MVBITS,	MTYPE_I1,	MTYPE_I1,	MTYPE_I1,
  INTRN_I2MVBITS,	MTYPE_I2,	MTYPE_I2,	MTYPE_I2,
  INTRN_I4MVBITS,	MTYPE_I4,	MTYPE_I4,	MTYPE_I4,
  INTRN_I8MVBITS,	MTYPE_I8,	MTYPE_I8,	MTYPE_I8,

};
INT32 intrinsic_parameter_type_size = sizeof(intrinsic_parameter_type) /
			              sizeof( INTRINSIC_PARAMETER_TYPE);


#define	WN_has_ty(x)		(OPCODE_has_1ty(WN_opcode(x)) || OPCODE_has_2ty(WN_opcode(x)))

#define	WN_is_pointer(x)	(WN_has_ty(x) && (TY_kind(WN_ty(x)) == KIND_POINTER))

#define Is_Integer_Constant(x)	(WN_operator(x) == OPR_INTCONST)

#define Is_Constant(x)		(WN_operator(x) == OPR_CONST)

#define OPCODE_is_intrinsic(op)                                         \
                ((OPCODE_operator((op)) == OPR_INTRINSIC_CALL) ||       \
                (OPCODE_operator((op)) == OPR_INTRINSIC_OP))

#define	ABS(x)		(((x)<0) ? -(x) : (x))

/* ====================================================================
*			 Exported Functions
* ====================================================================
*/
extern const char * INTR_intrinsic_name(WN *tree);

extern WN * make_pointer_to_node(WN *block, WN *tree);

/* ====================================================================
*			 Imported Functions
* ====================================================================
*/
extern PREG_NUM AssignExpr(WN *block, WN *tree, TYPE_ID type);

extern TY_IDX compute_alignment_type(WN *tree, TY_IDX, INT64 offset);

extern INT32 compute_copy_alignment(TY_IDX, TY_IDX, INT32 offset);

extern BOOL lower_is_aliased(WN *wn1, WN *wn2, INT64 size);

extern TYPE_ID compute_copy_quantum(INT32 );

extern WN *WN_I1const(TYPE_ID type, INT64 con);
extern void WN_annotate_call_flags(WN *call, ST *sym);

extern BOOL CG_bcopy_cannot_overlap;
extern BOOL CG_memcpy_cannot_overlap;
extern BOOL CG_memmove_cannot_overlap;

extern INT32 CG_memmove_inst_count;


/* ====================================================================
*			 Forward Declarations
* ====================================================================
*/
static EMULATION WN_emulation(WN *tree);

static WN *em_exp_int(WN *block, WN *x, WN *pow, TYPE_ID type);
static WN *em_exp_float(WN *block, WN *x, WN *pow, TYPE_ID type);
static WN *em_mod_float(WN *block, WN *x, WN *y);

static WN *em_complex_exp(WN *block, WN *x);
static WN *em_complex_cos(WN *block, WN *x);

static COERCE INTR_coerce_runtime(WN *tree, INT32 arg);
static TYPE_ID INTR_parameter_type(WN *tree, INT32 arg);
static TY_IDX aux_compute_alignment(WN *tree);



/* ====================================================================
 *                       private variables
 * ====================================================================
 */
static INT32 em_exp_int_max = 256;
static struct ALIAS_MANAGER *alias_manager = NULL;
#define MAX_INTRINSIC_ARGS      20





/* ====================================================================
 *
 * TYPE_ID  INTR_return_mtype(id)
 *
 *
 *
 * ==================================================================== */
TYPE_ID INTR_return_mtype(INTRINSIC id)
{
  INTRN_RETKIND rtype = INTRN_return_kind(id);

  switch(rtype)
  {
  case IRETURN_I1:	return MTYPE_I1;
  case IRETURN_I2:	return MTYPE_I2;
  case IRETURN_I4:	return MTYPE_I4;
  case IRETURN_I8:	return MTYPE_I8;
  case IRETURN_U1:	return MTYPE_U1;
  case IRETURN_U2:	return MTYPE_U2;
  case IRETURN_U4:	return MTYPE_U4;
  case IRETURN_U8:	return MTYPE_U8;
  case IRETURN_F4:	return MTYPE_F4;
  case IRETURN_F8:	return MTYPE_F8;
  case IRETURN_FQ:	return MTYPE_FQ;
  case IRETURN_C4:	return MTYPE_C4;
  case IRETURN_C8:	return MTYPE_C8;
  case IRETURN_CQ:	return MTYPE_CQ;
#if defined(TARG_X8664)
  case IRETURN_M8I1:    return MTYPE_M8I1;
  case IRETURN_M8I2:    return MTYPE_M8I2;
  case IRETURN_M8I4:    return MTYPE_M8I4;
#endif
  case IRETURN_V:	return MTYPE_V;
  case IRETURN_PV:
  case IRETURN_PU1:
  case IRETURN_DA1:
  case IRETURN_SZT:
  case IRETURN_PC     :
  case IRETURN_UNKNOWN:
    return MTYPE_UNKNOWN;
  }
  return MTYPE_UNKNOWN;
}




/* ====================================================================
 *
 * EMULATION WN_emulation(WN *tree)
 *
 * Provide the correct emulation enum for a given WN
 *
 * TODO: cache most frequently used id's
 *
 * ==================================================================== */

static EMULATION WN_emulation(WN *tree)
{
  OPCODE	op = WN_opcode(tree);
  TYPE_ID	type = OPCODE_rtype(op);

  switch (WN_operator(tree)) {
  case OPR_SQRT:
    switch(type) {
    case MTYPE_C4:	return EM_C4_SQRT;
    case MTYPE_C8:	return EM_C8_SQRT;
    case MTYPE_CQ:	return EM_CQ_SQRT;
    case MTYPE_FQ:	return EM_Q_SQRT;
    }
    break;

  case OPR_RSQRT:
    switch(type) {
    case MTYPE_C4:	return EM_C4_RSQRT;
    case MTYPE_C8:	return EM_C8_RSQRT;
    case MTYPE_CQ:	return EM_CQ_RSQRT;
    }
    break;

  case OPR_CVT:
    {
      TYPE_ID	desc = WN_desc(tree);
      if (desc == MTYPE_FQ)
      {
	switch(type) {
	case MTYPE_I4:	return EM_JI_QINT;
	case MTYPE_I8:	return EM_KI_QINT;
	case MTYPE_F4:	return EM_SNGL_Q;
	case MTYPE_F8:	return EM_DBLE_Q;
	}
	break;
      }
      else if (type == MTYPE_FQ)
      {
	switch(desc) {
	case MTYPE_U4: return EM_Q_FLOTJU;
	case MTYPE_I4: return EM_Q_FLOTJ;
	case MTYPE_U8: return EM_Q_FLOTKU;
	case MTYPE_I8: return EM_Q_FLOTK;
	case MTYPE_F8: return EM_Q_EXTD;
	case MTYPE_F4: return  EM_Q_EXT;
	}
      }
    }
    break;

  case OPR_RND:
    {
      TYPE_ID	desc = WN_desc(tree);
      if (desc == MTYPE_FQ)
      {
	switch(type)
	{
	case MTYPE_I4:	return EM_JIQNNT;
	case MTYPE_I8:	return EM_KIQNNT;
	}
	break;
      }
    }
    break;

  default:
    if (type == MTYPE_FQ)
    {
      switch(WN_operator(tree)) {
      case OPR_ISTORE:
      case OPR_ISTOREX:
      case OPR_STID:
      case OPR_ILOAD:
      case OPR_ILOADX:
      case OPR_SELECT:
      case OPR_LDID:
      case OPR_CONST:
#ifndef TARG_LOONGSON
      case OPR_NEG:
#endif    
	break;

      case OPR_ABS:	return EM_Q_ABS;
      case OPR_ADD:	return EM_Q_ADD;
      case OPR_SUB:	return EM_Q_SUB;
#ifdef TARG_LOONGSON
      case OPR_NEG:	return EM_Q_NEG;
#endif
      case OPR_MPY:	return EM_Q_MPY;
      case OPR_DIV:	return EM_Q_DIV;
      case OPR_MAX:	return EM_Q_MAX1;
      case OPR_MIN:	return EM_Q_MIN1;

      case OPR_RECIP:
      case OPR_RSQRT:
      case OPR_MADD:
      case OPR_MSUB:
      case OPR_NMADD:
      case OPR_NMSUB:
      case OPR_RND:
      case OPR_TRUNC:
      case OPR_CVT:
      case OPR_SQRT:
	Is_True(FALSE, ("WN_emulation() %s should be already processed", OPCODE_name(WN_opcode(tree))));
	break;

      case OPR_CEIL:
      case OPR_FLOOR:
      case OPR_MOD:
      case OPR_REM:
      case OPR_CVTL:
      case OPR_CALL:
      case OPR_INTRINSIC_CALL:
	Is_True(FALSE, ("WN_emulation() %s invalid context for op", OPCODE_name(WN_opcode(tree))));
      }
    }
    else if (WN_desc(tree)== MTYPE_FQ)
    {
      switch(WN_operator(tree)) {
      case OPR_EQ:	return EM_Q_EQ;
      case OPR_NE:	return EM_Q_NE;
      case OPR_GT:	return EM_Q_GT;
      case OPR_GE:	return EM_Q_GE;
      case OPR_LT:	return EM_Q_LT;
      case OPR_LE:	return EM_Q_LE;

      case OPR_TRUNC:
	switch(type)
	{
	case MTYPE_I4:	return EM_JI_QINT;
	case MTYPE_I8:	return EM_KI_QINT;
#ifdef TARG_LOONGSON
	case MTYPE_U4:	return EM_JU_QINT;
	case MTYPE_U8:	return EM_KU_QINT;
#endif
	}
	break;
      case OPR_CEIL:
	switch(type)
	{
	case MTYPE_I4:	return EM_JI_QCEIL;
	case MTYPE_I8:	return EM_KI_QCEIL;
	}
	break;
      case OPR_FLOOR:
	switch(type)
	{
	case MTYPE_I4:	return EM_JI_QFLOOR;
	case MTYPE_I8:	return EM_KI_QFLOOR;
	}
	break;
      }
    }
    break;
  }
  FmtAssert(FALSE, ("WN_emulation() %s not recognized", OPCODE_name(WN_opcode(tree))));
  return EM_LAST;
}


/* ====================================================================
 *
 *  WN *checkForZero(WN *block, TYPE_ID type, PREG_NUM xN, WN *if_else, WN *value)
 *
 *  Create test block for zero  
 *	if (x==0)
 *	{ 	ret = 0;	}
 *      else
 *	{ 	ret = value	}
 *	return ret;	
 *
 * ==================================================================== */
static WN *checkForZero(WN *block, TYPE_ID type, PREG_NUM xN, WN *if_else, WN *value)
{
  TYPE_ID	rtype = WN_rtype(value);
  WN		*if_then;
  PREG_NUM	retN;

  if_then = WN_CreateBlock();

  retN = AssignExpr(if_then, WN_Zerocon(rtype), rtype);

  {
    WN	*st;

    st = WN_StidIntoPreg(rtype, retN, MTYPE_To_PREG(rtype), value);
    WN_INSERT_BlockLast(if_else, st);
  }

  {
    WN	*cond, *IF;

    Is_True(MTYPE_is_float(type), ("unexpected type"));

#ifdef TARG_X8664
    if (type != MTYPE_V16C8)
      cond =  WN_EQ(type, 
		  WN_LdidPreg(type, xN),
		  WN_Zerocon(type));
    else
    {
      cond = WN_LAND( 
        WN_EQ(MTYPE_F8, WN_Unary(OPR_FIRSTPART, MTYPE_F8, 
          WN_LdidPreg(type, xN)), WN_Zerocon(MTYPE_F8)), 
        WN_EQ(MTYPE_F8, WN_Unary(OPR_SECONDPART, MTYPE_F8, 
          WN_LdidPreg(type, xN)), WN_Zerocon(MTYPE_F8))); 
    }
#else
    cond =  WN_EQ(type, 
 		  WN_LdidPreg(type, xN),
 		  WN_Zerocon(type));

#endif
    IF = WN_CreateIf(cond, if_then, if_else);
    WN_INSERT_BlockLast(block, IF);
  }
  return WN_LdidPreg(rtype, retN);
}



/* ====================================================================
 *
 * WN * WN_arg(WN *tree, INT32 arg)
 *
 * return Nth kid , skiping PARM
 * ==================================================================== */

static WN *WN_arg(WN *tree, INT32 arg)
{
  WN	*child= WN_kid(tree, arg);

  if (WN_operator_is(child, OPR_PARM))
  {
    return WN_kid0(child);
  }

  return child;
}




static WN *em_clen(WN *block, WN *len)
{
  return len;
}



/*
**
**  Auxillary routine to implement ( x + .5 * sign(x) )
*/
static WN *aux_nearest(TYPE_ID rtype, PREG_NUM xN)
{
  WN	*rel, *select;

  rel =   WN_GE(rtype, WN_LdidPreg(rtype, xN), WN_Zerocon(rtype));

  select = WN_Select(rtype,
		     rel,
		     WN_Floatconst(rtype, .5),
		     WN_Floatconst(rtype, -.5));

  return WN_Add(rtype, WN_LdidPreg(rtype, xN), select);
}

/*
**  Auxillary routine for Convert ( {Round,Trunc}(rtype) )
*/
static WN *aux_CvtRnd(TYPE_ID rtype, WN *x)
{
  WN		*rnd;
  TYPE_ID	intToFloat = (Slow_CVTDL) ? MTYPE_I4 : MTYPE_I8;
  
  // Needed for correctness, no matter how slow the truncate
  if (WN_rtype(x) != MTYPE_F4) {
     intToFloat = MTYPE_I8;
  }
  rnd = WN_Rnd(rtype, intToFloat, x);

  return WN_Cvt(intToFloat, rtype, rnd);
}

static WN *aux_CvtTrunc(TYPE_ID rtype, WN *x)
{
  WN		*trunc;
  TYPE_ID	intToFloat = (Slow_CVTDL) ? MTYPE_I4 : MTYPE_I8;

 /*
  *  this is em_aint()
  */
  // Needed for correctness, no matter how slow the truncate
  if (WN_rtype(x) != MTYPE_F4) {
     intToFloat = MTYPE_I8;
  }
  trunc = WN_Trunc(rtype, intToFloat, x);

  return WN_Cvt(intToFloat, rtype, trunc);
}

/*
**  Optimizer cannot deal with zero length mstore so return BLOCK
*/
static WN *aux_CreateMstore(WN_OFFSET offset, TY_IDX type, WN *value, WN *addr,
			    WN *size)
{
  if (Is_Integer_Constant(size) && WN_const_val(size) <= 0)
  {
    /* Cannot delete these nodes, since they are used later (bug 623566)
    WN_Delete(value);
    WN_Delete(addr);
    WN_Delete(size);
    */
    return WN_CreateBlock();
  }

  UINT64 ty_size = TY_size(TY_pointed(type));
  if (ty_size != 0 && WN_const_val (size) % ty_size != 0) {
      // size copied is not a multiple of the size of the type, which means 
      // that we are copying part of the type.  We then change the pointer
      // to (void*)
      static TY_IDX void_star = TY_IDX_ZERO;
      if (void_star == TY_IDX_ZERO)
	  void_star = Make_Pointer_Type (MTYPE_To_TY (MTYPE_V));
      Set_TY_IDX_index (type, TY_IDX_index (void_star));
  }
  return WN_CreateMstore(offset, type, value, addr, size);
}

/*
**
**  Notes for the following functions:
**
**  [1] Fast_trunc_Allowed	(currently when Roundoff_Level >= ROUNDOFF_SIMPLE)
**	generate trunc. This will fail when   (-2**63 <= |x| < 2**63-1)
**
**  [2]	Test x against TWO_EXP
**	Floating point value is such that (x+1 == x), ie. there is no
**	possible fractional value ie.
**		2**23 <=  |x| 		return x
**	
**  It is possible (if necessary) to special case MTYPE_F4 and generate
**  a trunc to MTYPE_I4.	
**/
#define TWO_EXP_23	8388608.0
#define TWO_EXP_52 	4503599627370496.0

/*
**
**	INTRN_I2F4NINT:
**	INTRN_I4F4NINT:
**	INTRN_I8F4NINT:
**	INTRN_I2F8IDNINT:
**	INTRN_I4F8IDNINT:
**	INTRN_I8F8IDNINT:
**	INTRN_I2FQIQNINT:
**	INTRN_I4FQIQNINT:
**	INTRN_I8FQIQNINT:
**
**	change into
**		rnd(x)				roundoff >= 3
**		trunc( x + .5 * sign(x) )
*/
static WN *em_nearest_int(WN *block, TYPE_ID rtype, WN *x)
{
  TYPE_ID	type = WN_rtype(x);

  if (Fast_NINT_Allowed)
  {
    return WN_Rnd(type, rtype, x);
  }
  else if ((type == MTYPE_F4) || (type == MTYPE_F8))
  {
    WN		*add;
    PREG_NUM	xN;

    xN = AssignExpr(block, x, type);

    add = aux_nearest(type, xN);

    if (Fast_trunc_Allowed)
    {
      return  WN_Trunc(type, rtype, add);
    }
    else
    {
      WN	*rel, *select;
      double	con= (type==MTYPE_F4) ? TWO_EXP_23 : TWO_EXP_52;

      rel = WN_GE(type,
		  WN_Abs(type, WN_LdidPreg(type, xN)),
		  WN_Floatconst(type, con));

      select = WN_Select(type, rel, WN_LdidPreg(type, xN), add);

      return WN_Trunc(type, rtype, select);
    }
  }
  else
  {
    return NULL;
  }
}

/*
**
**	INTRN_F4ANINT:
**	INTRN_F8ANINT:
**	INTRN_FQANINT:
**
**	change into
**		cvt (float, trunc( x + .5 * sign(x) ))	roundoff>= 3
*/
static WN *em_nearest_aint(WN *block, TYPE_ID rtype, WN *x)
{
  if (Fast_NINT_Allowed)
  {
    return aux_CvtRnd(rtype, x);
  }
  else if ((rtype == MTYPE_F4) || (rtype == MTYPE_F8))
  {
    PREG_NUM	xN;
    WN		*add, *cvt;

    xN = AssignExpr(block, x, rtype);

    add = aux_nearest(rtype, xN);

   /*
    *  this is em_aint()
    */
    cvt = aux_CvtTrunc(rtype, add);

    if (Fast_trunc_Allowed)
    {
      return cvt;
    }
    else
    {
      WN        *rel;
      double    con= (rtype==MTYPE_F4) ? TWO_EXP_23 : TWO_EXP_52;

      rel = WN_GE(rtype,
                  WN_Abs(rtype, WN_LdidPreg(rtype, xN)),
                  WN_Floatconst(rtype, con));

      return WN_Select(rtype, rel, WN_LdidPreg(rtype, xN), cvt);
    }
  }
  return NULL;
}

/*
**
**	INTRN_F4AINT
**	INTRN_F8AINT
**	INTRN_FQAINT
**
**	change into
**		cvt (float, trunc(x))
*/
static WN *em_aint(WN *block, TYPE_ID rtype, WN *x)
{
  if (Fast_trunc_Allowed)
  {
    return aux_CvtTrunc(rtype, x);
  }
  else if ((rtype == MTYPE_F4) || (rtype == MTYPE_F8))
  {
    PREG_NUM	xN;
    WN		*rel, *cvt;
    double	con= (rtype==MTYPE_F4) ? TWO_EXP_23 : TWO_EXP_52;

    xN = AssignExpr(block, x, rtype);

    rel = WN_GE(rtype,
		WN_Abs(rtype, WN_LdidPreg(rtype, xN)),
		WN_Floatconst(rtype, con));

    cvt = aux_CvtTrunc(rtype, WN_LdidPreg(rtype, xN));

    return WN_Select(rtype, rel, WN_LdidPreg(rtype, xN), cvt);
  }
  else
  {
    return NULL;
  }
}

/*
**
**	change into
**		| x |  if y >= 0
**	      - | x |  if y < 0
**
**	-->	absN = | x |;
**	-->	(y>=0) ? absN : -absN;		
*/
static WN *em_sign(WN *block, WN *x, WN *y)
{
  PREG_NUM	absN;
  TYPE_ID	type = WN_rtype(x);
  WN		*abs, *select;

#ifdef KEY // bug 9660
  if (MTYPE_is_integral(type) && ! MTYPE_signed(type))
    type = Mtype_TransferSign(MTYPE_I4, type);
#endif
#ifdef KEY // bug 12052
  if (MTYPE_is_integral(type) && 
      MTYPE_byte_size(type) < MTYPE_byte_size(WN_rtype(y)))
    type = Mtype_TransferSize(WN_rtype(y), type);
#endif
  abs = WN_Abs(type, x);
  absN = AssignExpr(block, abs, type);


  select = WN_Select(type,
		     WN_GE(type, y, WN_Zerocon(type)),
		     WN_LdidPreg(type, absN),
		     WN_Neg(type, WN_LdidPreg(type, absN)));
  return select;
}

/*
**
**	change into
**		cvt (x) * cvt(y)
*/
static WN *em_prod(WN *block, TYPE_ID rtype, WN *x, WN *y)
{
  TYPE_ID  type = WN_rtype(x);
  WN	   *mpy;

  mpy = WN_Mpy(rtype,
	       WN_Cvt(type, rtype, x),
	       WN_Cvt(type, rtype, y));
  return   mpy;
}

/*
**
**	change into
**		(x>y) ? (x-y) : 0
*/
static WN *em_dim(WN *block, WN *x, WN *y)
{
  PREG_NUM	xN, yN;
  TYPE_ID	type = WN_rtype(x);
  WN		*rel, *sub, *select;

  xN = AssignExpr(block, x, type);
  yN = AssignExpr(block, y, type);

  rel =   WN_GT(type, 
		WN_LdidPreg(type, xN),
		WN_LdidPreg(type, yN));

  sub =  WN_Sub(type, 
		WN_LdidPreg(type, xN),
		WN_LdidPreg(type, yN));

  select = WN_Select(type,
		     rel,
		     sub,
		     WN_Zerocon(type));
  return select;
}

/*
**
**	change into
**	    x - y * ( FLOAT ( |(x / y)| ))
*/
static WN *em_mod_float(WN *block, WN *x, WN *y)
{
  PREG_NUM	xN, yN;
  TYPE_ID	type = WN_rtype(x);
  WN		*div, *cvt, *mpy, *sub;

  if ((type == MTYPE_F4) || (type == MTYPE_F8)) {
    xN = AssignExpr(block, x, type);
    yN = AssignExpr(block, y, type);
    div =  WN_Div(type, 
		  WN_LdidPreg(type, xN),
		  WN_LdidPreg(type, yN));
    cvt = em_aint(block, type, div);
    mpy =  WN_Mpy(type, 
		  WN_LdidPreg(type, yN),
		  cvt);
    sub =  WN_Sub(type,
		  WN_LdidPreg(type, xN),
		  mpy);
    return sub;
  } else {
    return NULL;
  }
}

/*
**  WN *build_mult_tree(block, TYPE_ID type, PREG_NUM xN, int pow)
**
**	Build a multiply tree to make shipiro happy.
**	
**	Actually, create a series of temporaries to hold the powers that be.
**
**	ex.	x ** 9	(= 1001)
**		t0=	x;
**		t1=	t0*t0;		(x**2)
**		t2=	t1*t1;		(x**4)
**		t3=	t2*t2;		(x**8)
**		ans =  t3 * t0;
**
*/
#define	BIT_IS_ON(x,i)		((x) & (1<<(i)))

static WN *build_mult_tree(WN *block, TYPE_ID type, PREG_NUM xN, INT32 pow)
{
  PREG_NUM	powers[16];	/* could handle pow = 64k */
  INT32		i, n = 0;
  PREG_NUM	xNm1;
  WN		*tree = NULL;

  Is_True((pow>0), ("expected pow>0"));

  powers[n++] = xN;
  xNm1 = xN;

  for(i= 1; ((1<<i) <= pow); i++)
  {
    WN		*mpy;

    mpy = WN_Mpy(type, WN_LdidPreg(type, xNm1), WN_LdidPreg(type, xNm1));

    xNm1 = AssignExpr(block, mpy, type);

    powers[n++] = xNm1;
  }

  for(i= 0; ((1<<i) <= pow); i++)
  {
    if (BIT_IS_ON(pow, i))
    {
      PREG_NUM  powerN = powers[i];

      if (tree)
      {
	tree = WN_Mpy(type, tree, WN_LdidPreg(type, powerN));
      }
      else
      {
	tree = WN_LdidPreg(type, powerN);
      }
    }
  }
  return tree;
}

static WN *em_exp_float(WN *block, WN *x, WN *pow, TYPE_ID type)
{
  if (Is_Constant(pow))
  {
    TCON	con = Const_Val(pow);
    BOOL	sqrt, rsqrt;
#ifdef KEY
    BOOL        sqrt_25, rsqrt_25, sqrt_75, rsqrt_75;
    BOOL	cbrt_33, cbrt_66;
#endif
    WN		*tree, *x_copy;
    double	n;

   /*
    *  for complex x verify the power is a real number
    *  (TODO) general complex ** complex
    */
    if (MTYPE_is_complex(type))
    {
      TCON  Ipow;

      Ipow = Extract_Complex_Imag(con);

      if (Targ_To_Host_Float(Ipow) == 0.0)
      {
	con = Extract_Complex_Real(con);
      }
      else
      {
	return NULL;
      }
    }

    /* Workaround for bug 688.
     */
    if (MTYPE_is_vector(type))
      return NULL;

    n = Targ_To_Host_Float(con);
    sqrt = rsqrt = FALSE;
#ifdef KEY
    cbrt_33 = cbrt_66 = FALSE;
    sqrt_25 = rsqrt_25 = sqrt_75 = rsqrt_75 = FALSE;
#endif

    if (trunc(n) == n)
    {
      ;
    }
    else if ((trunc(ABS(n))+.5) == ABS(n))
    {
      /*
       *  if we need to multiply by sqrt we need a copy of x
       *  as it might get changed underneath us.
       */
      if (n<0)
	rsqrt = TRUE;
      else
	sqrt = TRUE;
      x_copy = WN_COPY_Tree(x);
    }
#ifdef KEY
    else if ((trunc(ABS(n))+.25) == ABS(n))
    {
      /*
       *  if we need to multiply by sqrt we need a copy of x
       *  as it might get changed underneath us.
       */
      if (n<0)
	rsqrt_25 = TRUE;
      else
	sqrt_25 = TRUE;
      x_copy = WN_COPY_Tree(x);
    }    
    else if ((trunc(ABS(n))+.75) == ABS(n))
    {
      /*
       *  if we need to multiply by sqrt we need a copy of x
       *  as it might get changed underneath us.
       */
      if (n<0)
	rsqrt_75 = TRUE;
      else
	sqrt_75 = TRUE;

      x_copy = WN_COPY_Tree(x);
    }    
#if !defined (TARG_MIPS) && !defined (TARG_IA64) && !defined(TARG_PPC32) && !defined(TARG_LOONGSON)
    else if (ABS((trunc(n)+1.0/3) - n) < .0000001 && 
             ! (Is_Target_64bit() && !Is_Target_Anyx86() && OPT_Fast_Math))
    { // the pow in fast_math is faster than cbrt, so no point converting
      cbrt_33 = TRUE;
      x_copy = WN_COPY_Tree(x);
    }
    else if (ABS((trunc(n)+2.0/3) - n) < .0000001 &&
             ! (Is_Target_64bit() && !Is_Target_Anyx86() && OPT_Fast_Math))
    { // the pow in fast_math is faster than cbrt, so no point converting
      cbrt_66 = TRUE;
      x_copy = WN_COPY_Tree(x);
    }
#endif
#endif
    else
    {
      return NULL;
    }

    {
      WN  *ipow = WN_Intconst(MTYPE_I4, (INT64) trunc(n));

      tree = em_exp_int(block, x, ipow, type);
    }

    if (sqrt || rsqrt)
    {
#ifdef KEY
      // bug 4824: non-constant float x could be negative
      // bug 4990: Do the check only for C/C++ and if
      // -fmath-errno (-LANG:math_errno=on)
      if (!PU_f77_lang (Get_Current_PU()) &&
          !PU_f90_lang (Get_Current_PU()) && // ! Fortran
	  LANG_Math_Errno && // -fmath-errno
          MTYPE_is_float (WN_rtype (x_copy)) &&
          (!Is_Constant (x_copy) ||
	   Targ_To_Host_Float (Const_Val (x_copy)) < 0))
        return NULL;
#endif // KEY
#ifdef TARG_X8664
      // Bug 5935 - rsqrtsd or rsqrtpd is absent.
      if (rsqrt && (type == MTYPE_F8 || type == MTYPE_V16F8))
	return NULL;
#endif 
      if (tree)
      {
	/*
	 *  x ** n+.5 	->	(x**n) * (x**.5)
	 *  where the function em_exp_int has already evaluated	
	 */
	PREG_NUM	xN, treeN;
	WN		*fractional;

	xN = AssignExpr(block, x_copy, type);
	treeN = AssignExpr(block, tree, type);

	fractional = (sqrt) ?	WN_Sqrt(type, WN_LdidPreg(type, xN)) :
				WN_Rsqrt(type, WN_LdidPreg(type, xN));

	tree =  WN_Mpy(type,
		       WN_LdidPreg(type, treeN),
		       fractional);
      }
    }
#ifdef KEY // bug 6932 
    // evaluate (x**0.25) as sqrt(sqrt(x))
    if (sqrt_25 || rsqrt_25) 
    {
      if (!PU_f77_lang (Get_Current_PU()) &&
          !PU_f90_lang (Get_Current_PU()) && // ! Fortran
	  LANG_Math_Errno && // -fmath-errno
          MTYPE_is_float (WN_rtype (x_copy)) &&
          (!Is_Constant (x_copy) ||
	   Targ_To_Host_Float (Const_Val (x_copy)) < 0))
        return NULL;
#ifdef TARG_X8664
      // rsqrtsd or rsqrtpd is absent.
      if (rsqrt_25 && (type == MTYPE_F8 || type == MTYPE_V16F8))
	return NULL;
#endif 
      if (tree)
      {
	/*
	 *  x ** n+.25 	->	(x**n) * (x**.25)
	 *  where the function em_exp_int has already evaluated	
	 */
	PREG_NUM	xN, treeN;
	WN		*fractional;

	xN = AssignExpr(block, x_copy, type);
	treeN = AssignExpr(block, tree, type);

	if (sqrt_25) 
	  fractional = WN_Sqrt(type, WN_Sqrt(type, WN_LdidPreg(type, xN)));
	else
	  fractional = WN_Sqrt(type, WN_Rsqrt(type, WN_LdidPreg(type, xN)));

	tree =  WN_Mpy(type,
		       WN_LdidPreg(type, treeN),
		       fractional);
      }      
    }
    // evaluate (x**0.75) as sqrt(x)*sqrt(sqrt(x))
    if (sqrt_75 || rsqrt_75) 
    {
      if (!PU_f77_lang (Get_Current_PU()) &&
          !PU_f90_lang (Get_Current_PU()) && // ! Fortran
	  LANG_Math_Errno && // -fmath-errno
          MTYPE_is_float (WN_rtype (x_copy)) &&
          (!Is_Constant (x_copy) ||
	   Targ_To_Host_Float (Const_Val (x_copy)) < 0))
        return NULL;
#ifdef TARG_X8664
      // rsqrtsd or rsqrtpd is absent.
      if (rsqrt_75 && (type == MTYPE_F8 || type == MTYPE_V16F8))
	return NULL;
#endif 

      if (tree)
      {
	/*
	 *  x ** n+.75 	->	(x**n) * (x**.75)
	 *  where the function em_exp_int has already evaluated	
	 */
	PREG_NUM	xN, treeN;
	WN		*fractional;

	xN = AssignExpr(block, x_copy, type);
	treeN = AssignExpr(block, tree, type);

	if (sqrt_75) 
	  fractional = WN_Mpy(type, 
			      WN_Sqrt(type, WN_LdidPreg(type, xN)), 
			      WN_Sqrt(type, 
				      WN_Sqrt(type, WN_LdidPreg(type, xN))));
	else
	  fractional = WN_Mpy(type, 
			      WN_Rsqrt(type, WN_LdidPreg(type, xN)), 
			      WN_Rsqrt(type, 
				       WN_Sqrt(type, WN_LdidPreg(type, xN))));
	
	tree =  WN_Mpy(type,
		       WN_LdidPreg(type, treeN),
		       fractional);
      }      
    }
    // evaluate (x**0.333333) by calling cbrt()/cbrtf()
#if !defined(TARG_SL)
    if (cbrt_33 || cbrt_66)
    {
      if (type != MTYPE_F4 && type != MTYPE_F8)
	return NULL;
      if (tree)
      {
	/*
	 *  x ** n+1/3 	->	(x**n) * (x**1/3)
	 *  where the function em_exp_int has already evaluated	
	 */
	 PREG_NUM xN = AssignExpr(block, x_copy, type);
	 WN *kid = WN_CreateParm(type, WN_LdidPreg(type, xN), Be_Type_Tbl(type),
	 			 WN_PARM_BY_VALUE | WN_PARM_READ_ONLY);
	 WN* fraction = WN_Create_Intrinsic(
	 			OPCODE_make_op(OPR_INTRINSIC_OP, type, MTYPE_V),
		      		type == MTYPE_F4 ? INTRN_F4CBRT : INTRN_F8CBRT,
				1, &kid);
	 if (cbrt_66) {
	   PREG_NUM x13 = AssignExpr(block, fraction, type);
	   fraction = WN_Mpy(type, WN_LdidPreg(type, x13), 
	   			   WN_LdidPreg(type, x13));
	 }
	 tree = WN_Mpy(type, tree, fraction);
      }
    }
#endif // !TARG_SL
#endif // KEY bug 6932
    return tree;
  }
 
  return NULL;
}

static WN *em_exp_int(WN *block, WN *x, WN *pow, TYPE_ID type)
{
  if (Is_Integer_Constant(pow))
  {
    INT32	n = WN_const_val(pow);
    INT32	absN = ABS(n);
    WN		*exp=  NULL;

    if (em_exp_int_max < absN || absN < 0) //in case absN == 0x80000000
      return NULL;

    switch(n) {
    case 1:
      exp = x;
      break;
    case -1:
      exp = WN_Inverse(type, x);
      break;
    case 0:
      if (MTYPE_type_class(type) & MTYPE_CLASS_INTEGER)
	exp = WN_Intconst(type, 1);
      else
	exp = WN_Floatconst(type, 1.0);
      break;
    case 2:
      {
	PREG_NUM	xN;

	xN = AssignExpr(block, x, type);
      
        exp = WN_Mpy(type,
		     WN_LdidPreg(type, xN),
		     WN_LdidPreg(type, xN));
	break;
      }
    default:
      {
	PREG_NUM	xN;

	if (Fast_Exp_Allowed)
	{
	  xN = AssignExpr(block, x, type);
    
	  exp = build_mult_tree(block, type, xN, absN);

	  WN_Delete(pow);
	  if (n < 0)
	    exp = WN_Inverse(type, exp);
	}
      }
    }
    return exp;
  }
  else if (Is_Integer_Constant(x))
  {
   /*
    *	Optimize {-2,-1,0,1,2} ** n
    */
    INT32	val = WN_const_val(x);

    switch(val)
    {
    case -2:
      {
       /*
	*  (n>=0) ? ( (n&1) ? - (1<<n) : 1<<n ) : 0
	*/
	PREG_NUM	powN, shlN;
	WN	*shl, *band, *cond, *select, *ge;

	powN = AssignExpr(block, pow, type);

	shl = WN_Shl(type,
		     WN_Intconst(type, 1),
		     WN_LdidPreg(type, powN));

	shlN = AssignExpr(block, shl, type);

	band = WN_Band(type,
		       WN_LdidPreg(type, powN),
		       WN_Intconst(type, 1));

	cond =  WN_EQ(type, band, WN_Zerocon(type));

	select = WN_Select(type,
			   cond,
			   WN_LdidPreg(type, shlN),
			   WN_Neg(type, WN_LdidPreg(type, shlN)));

	ge =   WN_GE(type,
		     WN_LdidPreg(type, powN),
		     WN_Zerocon(type));

	return  WN_Select(type,
			  ge,
			  select,
			  WN_Zerocon(type));
      }
    case -1:
      {
       /*
	*  (n&1) ? -1 : 1;
	*/
	WN	*band;

	band = WN_Band(type, pow, WN_Intconst(type, 1));

	return WN_Select(type,
			 WN_EQ(type, band, WN_Zerocon(type)),
			 WN_Intconst(type, 1),
			 WN_Intconst(type, -1));
      }
    case 0:
     /*
      *  (n==0) ? 1 : 0
      *  simpler is (n==0)
      */
      return WN_EQ(type, pow, WN_Zerocon(type));

    case 1:
     /*
      *  always and forever 1
      */
      return WN_Intconst(type, 1);

    case 2:
      {
       /*
	*  (n>=0) ? 1<<n : 0
	* simpler is (n>=0) << n
	*/
	WN	*ge;
	PREG_NUM powN;

	powN = AssignExpr(block, pow, type);

	ge =   WN_GE(type,
		     WN_LdidPreg(type, powN),
		     WN_Zerocon(type));

	return WN_Shl(type,
		      ge,
		      WN_LdidPreg(type, powN));
	
      }
    }
  }
 
  return NULL;
}

/*
**	quad negate looks like complex negate
**
**	if q = (x,y) then
**	  -q = (-x, -y)
**
**	TODO	nail down preg offset interface
** Bug 12895: MIPS quad represents ieee 128, so  -q = (-x, y)
*/
static WN *em_quad_neg(WN *block, WN *tree)
{
  TYPE_ID	newType;
  TYPE_ID	type = WN_rtype(tree);
  PREG_NUM	qN, qNlo;

  /*
   *  assign a quad preg temp as we will be referencing twice (sortof)
   */
  qN = AssignExpr(block, WN_kid0(tree), type);

  if (MTYPE_is_complex(type))
  {
    newType = MTYPE_C8;
    qNlo = qN+2;
  }
  else /* assume MTYPE_FQ */
  {
    newType = MTYPE_F8;
    qNlo = qN+1;
  }
 
  {
    WN	*wn, *st;
    ST	*npreg = MTYPE_To_PREG(newType);

#ifdef TARG_MIPS
    wn = WN_LdidPreg(newType, qN);  // Bug 12895
#else
    wn = WN_Neg(newType, WN_LdidPreg(newType, qN));
#endif
    st = WN_StidIntoPreg(newType, qN, npreg, wn);
    WN_INSERT_BlockLast(block, st);

    wn = WN_Neg(newType, WN_LdidPreg(newType, qNlo));
    st = WN_StidIntoPreg(newType, qNlo, npreg, wn);
    WN_INSERT_BlockLast(block, st);
  }
  WN_Delete(tree);

  return WN_LdidPreg(type, qN);
}


static WN *em_quad_abs(WN *block, WN *tree)
{
  TYPE_ID	newType;
  TYPE_ID	type = WN_rtype(tree);
  PREG_NUM	qN, qNlo;

  /*
   *  assign a quad preg temp as we will be referencing twice (sortof)
   */
  qN = AssignExpr(block, WN_kid0(tree), type);

  Is_True(! MTYPE_is_complex(type), ("em_quad_abs emulates FQ not CQ"));
  newType = MTYPE_F8;
  qNlo = qN+1;

  {
    WN	*wn, *st;
    ST	*npreg = MTYPE_To_PREG(newType);

#ifdef TARG_MIPS
    wn = WN_LdidPreg(newType, qN);  // Bug 12895
#else
    wn = WN_Abs(newType, WN_LdidPreg(newType, qN));
#endif
    st = WN_StidIntoPreg(newType, qN, npreg, wn);
    WN_INSERT_BlockLast(block, st);

    wn = WN_Abs(newType, WN_LdidPreg(newType, qNlo));
    st = WN_StidIntoPreg(newType, qNlo, npreg, wn);
    WN_INSERT_BlockLast(block, st);
  }
  WN_Delete(tree);

  return WN_LdidPreg(type, qN);
}


/*
**	There is no no native quad select, so we must turn the
**	expression back into an if/else block
**
**	select:	(cond) ? exp1 : exp2
**
**	-->	if (cond)	qN = exp1;
**		else		qN = exp2;
**		return qN
**		
*/
static WN *em_split_select(WN *block, WN *tree)
{
  TYPE_ID	rtype = WN_rtype(tree);
  PREG_NUM	qN;
  WN		*if_then, *if_else;


  if_then = WN_CreateBlock();
  if_else = WN_CreateBlock();
  {
    WN	*exp1 = WN_kid1(tree);

    qN = AssignExpr(if_then, exp1, rtype);
  }
  {
    WN	*wn;
    WN	*exp2 = WN_kid2(tree);
    ST	*preg = MTYPE_To_PREG(rtype);

    wn = WN_StidIntoPreg(rtype, qN, preg, exp2);
    WN_INSERT_BlockLast(if_else, wn);
  }
  {
    WN 	*IF;
    WN 	*cond = WN_kid0(tree);

    IF = WN_CreateIf(cond, if_then, if_else);
    WN_INSERT_BlockLast(block, IF);
  }
  WN_Delete(tree);

  return WN_LdidPreg(rtype, qN);
}

/*
**  Evaluate the following function
**	
**	Definition
**	x y             INTRN_DIVFLOOR          INTRN_DIVCEIL
**	---             --------------          -------------
**	+ +                 x / y                (x+y-1) / y
**	
**	- -                 x / y                (x+y+1) / y
**	
**	+ -              (x+ -1-y)/y                x / y
**	
**	- +              (x+  1-y)/y                x / y
**	
**	
**	The issue was to evaulate (divfloor) without branch code.
**	
**	Tricks
**	        f(x) =  -1      (x<0)
**	                +1      (x>=0)
**	        {
**	                t= x>>31;
**	                f= t+t+1
**	        }
**	        MASK(x,y,v)=    0       (x>=0, y>=0), (x<0, y<0)    ++, --
**	                        v       (x>=0, y<0),  (x<0, y>=0)   +-, -+
**	        {
**	                t= (x^y)>>31
**	                MASK= t & v
**	        }
**	
**	The cleverness (shapiro's) was the composition of these functions
**	to evaluate divfloor.
**	
**	        DIVFLOOR(x,y)=
**	                v =     f(y) - y;       (-1-y) [+-],   (+1-y) [-+]
**	                (x + MASK(x,y,v)) / y
**	
**	        DIVCEIL(x,y) = -DIVFLOOR(-x,y)
**
**  x,y are assumed integral or we could just do a divide/floor
**
**
*/

static WN *em_divfloor(WN *block, TYPE_ID type, WN *x, WN *y)
{
  PREG_NUM	xN, yN;
  WN		*numer, *div;

  Is_True((MTYPE_is_integral(WN_rtype(x)) &&
	   MTYPE_is_integral(WN_rtype(y))),
	  ("em_divfloor() arguments should be type integral"));

  xN = AssignExpr(block, x, type);
  yN = AssignExpr(block, y, type);
 
  {
   /*
    *	one =  1	(y >= 0)
    *	      -1	(y <  0)
    */
    TYPE_ID	ytype = WN_rtype(y);
    WN		*sra, *add, *one, *bxor, *mask, *sub, *band;
#ifdef TARG_X8664 
    // Bug 3264 - This algorithm requires that byte size be identical for 
    // ytype and type, for zero-extended 64-bit target ISA.
    if (MTYPE_is_unsigned(ytype) &&
	MTYPE_byte_size(ytype) < MTYPE_byte_size(type))
      ytype = type;
#endif

    sra = WN_Ashr(type,
		  WN_LdidPreg(type, yN),
		  WN_Intconst(type, MTYPE_size_reg(ytype)-1));

    add = WN_Add(type,
		 sra,
		 WN_COPY_Tree(sra));

    one = WN_Add(ytype,
		 add,
		 WN_Intconst(ytype, 1));
   /*
    *	mask =	 0	(x,y)= ++ --
    *	mask =	-1	(x,y)= +- +-
    */
    bxor = WN_Bxor(ytype,
		   WN_LdidPreg(type, xN),
		   WN_LdidPreg(type, yN));

    mask = WN_Ashr(type,
		   bxor,
		   WN_Intconst(type, MTYPE_size_reg(type)-1));

   /*
    *	sub =	 1 - y		(y >= 0)
    *		-1 - y		(y <  0)
    */
    sub = WN_Sub(type, one, WN_LdidPreg(type, yN));

    band = WN_Band(type, sub, mask);

    numer = WN_Add(type, band, WN_LdidPreg(type, xN));
  }

  div = WN_Div(type, numer, WN_LdidPreg(type, yN));
   
  return div;
}

/*
**  Evaluate the following function
**	
**  divceil(x,y)
**  (x)(y)	
**   +  +	(x + (y-1) / y
**   -  -	(x + (y+1) / y
**   +  -	 x         / y
**   -  +	 x         / y
**
**  x,y are assumed integral or we could just do a divide/floor
**
**  for now please note the identify
**
**  divceil(x,y)=	- divfloor(-x, y)
*/
static WN *em_divceil(WN *block, TYPE_ID type, WN *x, WN*y)
{

  WN	*divfloor;

  divfloor = em_divfloor(block, type, WN_Neg( WN_rtype(x), x), y);

  return WN_Neg(type, divfloor);
}

/* ====================================================================
 *
 *  em_alloca
 *    lower the alloca intrinsic call
 *
 *       _builtin_alloca(size)
 *
 *    to the sequence of whirl trees, if the stack direction is decrement.
 *
 *       sp = sp - soundoff(size)
 *       return  sp + arg_build_area_size
 *
 * ==================================================================== */

static WN *em_alloca(WN *block, WN *tree)
{
  WN		*size = WN_arg(tree, 0);
  TYPE_ID	stype = WN_rtype(size);
  TYPE_ID	type = WN_rtype(tree);
  BOOL		stack_decrement = (Stack_Direction() == DECREMENT);

  {
    WN		*add, *adj, *inc, *st;
    INT64	stack_alignment;
    ST		*preg = MTYPE_To_PREG(Pointer_type);

    stack_alignment = Stack_Alignment();

    add = WN_Add(stype,
		 size,
		 WN_Intconst(stype, stack_alignment-1));

    adj = WN_Band(stype,
		  add,
		  WN_Intconst(stype, -stack_alignment));

    inc = WN_Binary(stack_decrement ? OPR_SUB : OPR_ADD,
		    type,
		    WN_LdidPreg(Pointer_type, Stack_Pointer_Preg_Offset),
		    adj);

    st = WN_StidIntoPreg(Pointer_type, Stack_Pointer_Preg_Offset, preg, inc);

    WN_INSERT_BlockLast(block, st);
  }
  {
   /*
    * Add in the build area size (now that it is known)
    */
    WN	*ptr;

    Is_True(!Get_Trace(TP_DATALAYOUT, 2),("arg build area not correct"));
    ptr = WN_Binary(stack_decrement ? OPR_ADD : OPR_SUB,
		    type,
		    WN_LdidPreg(Pointer_type, Stack_Pointer_Preg_Offset),
		    WN_Intconst(type, Current_PU_Actual_Size));

    if ( DEBUG_Trap_Uv )
    {
      WN  *con, *mstore;

      con = WN_UVConst(stype);

      mstore= aux_CreateMstore(0,
			       Make_Pointer_Type(MTYPE_To_TY(stype), TRUE),
			       con,
			       WN_COPY_Tree(ptr),
			       WN_COPY_Tree(size));

      WN_INSERT_BlockLast(block, mstore);
    }

    return ptr;
  }
}

/* ====================================================================
 *
 *  WN *em_readstackpointer(WN *block, TYPE_ID rtype)
 *
 *       return  sp
 *
 * ==================================================================== */
static WN *em_readstackpointer(TYPE_ID rtype)
{
  return WN_LdidPreg(rtype, Stack_Pointer_Preg_Offset);
}

#ifdef KEY
/* ====================================================================
 *
 *  WN *em_readframepointer(WN *block, TYPE_ID rtype)
 *
 *       return  sp
 *
 * ==================================================================== */
static WN *em_readframepointer(TYPE_ID rtype)
{
  return WN_LdidPreg(rtype, Frame_Pointer_Preg_Offset);
}
#endif

/* ====================================================================
 *
 *  WN *em_setstackpointer(WN *block, TYPE_ID rtype, WN *value)
 *
 *  Set the stack pointer (sp) to value
 *
 * ==================================================================== */

static WN *em_setstackpointer(WN *block, TYPE_ID rtype, WN *value)
{
  WN	*stid;

  stid = WN_StidIntoPreg(rtype,
			 Stack_Pointer_Preg_Offset,
			 MTYPE_To_PREG(rtype),
			 value);

  WN_INSERT_BlockLast(block, stid);

  return WN_LdidPreg(rtype, Stack_Pointer_Preg_Offset);
}

static WN *createParm(WN *x, BOOL byvalue)
{
  TYPE_ID type = WN_rtype(x);
  TY_IDX ty;

  if (WN_operator_is(x, OPR_PARM))
  {
    return x;
  }

  ty = MTYPE_To_TY(type);

  return WN_CreateParm(type, x, ty,
		      (byvalue ? WN_PARM_BY_VALUE : WN_PARM_BY_REFERENCE));
}

static WN *Intrinsic(TYPE_ID type, INTRINSIC id, INT32 n, WN *x, WN *y)
{
  OPCODE	op= OPCODE_make_op (OPR_INTRINSIC_OP, type, MTYPE_V);
  WN		*wn, *kids[20];
  BOOL          byvalue = INTRN_by_value(id);

  Is_True((n<=20), ("too many arguments for Intrinsic()"));

  if (x)
    x = createParm(x,byvalue);
  if (y)
    y = createParm(y,byvalue);

  kids[0]=	x;
  kids[1]=	y;
  
  wn = WN_Create_Intrinsic(op, id, n, kids);

  return wn;
}

/* ====================================================================
 *
 *	real =  e**(rz) * cos(iz);
 *	imag =  e**(rz) * sin(iz);
 *
 * ==================================================================== */
static WN *em_complex_exp(WN *block, WN *x) 
{
  TYPE_ID	type = WN_rtype(x);
  TYPE_ID	rtype = Mtype_complex_to_real(type);
  PREG_NUM	zN, expN, iN;
  BOOL		paired_input = FALSE;
  WN		*cosine,*sine;
  WN		*exp;
  WN		*realpart, *imagpart ;
  INTRINSIC	expID;
  INTRINSIC	cosID,sinID;

  switch(rtype) {
   case MTYPE_F4: expID = INTRN_F4EXP; cosID = INTRN_F4COS; sinID = INTRN_F4SIN; break;
   case MTYPE_F8: expID = INTRN_F8EXP; cosID = INTRN_F8COS; sinID = INTRN_F8SIN; break;
   case MTYPE_FQ: expID = INTRN_FQEXP; cosID = INTRN_FQCOS; sinID = INTRN_FQSIN; break;
  }
  if (WN_operator(x) == OPR_PAIR && rtype == MTYPE_F8)
  {
    paired_input = TRUE;
    iN = AssignExpr(block, WN_COPY_Tree(WN_kid1(x)), rtype);
  } else
    zN = AssignExpr(block, x, type);
  exp= Intrinsic(rtype,
		 expID,
		 1,
		 paired_input? WN_COPY_Tree(WN_kid0(x)) :
		 WN_Realpart(rtype,WN_LdidPreg(type, zN)), NULL);
  
  expN = AssignExpr(block, exp, rtype);

  cosine = Intrinsic(rtype,
		     cosID,
		     1,
		     paired_input?WN_LdidPreg(rtype, iN):
		     WN_Imagpart(rtype,WN_LdidPreg(type, zN)), NULL);
  
  sine = Intrinsic(rtype,
		   sinID,
		   1,
		   paired_input?WN_LdidPreg(rtype, iN):
		   WN_Imagpart(rtype,WN_LdidPreg(type, zN)), NULL);
  /*
   *	cis  =  cos(iz) + i*sin(iz);
   *
   *	real =  e**(rz) * REAL(cis);
   *	imag =  e**(rz) * IMAG(cis);
   */
  /* it is a performance hack that sincos is not called directly, WOPT 
   * would combine them. The gain is that as the imagine part is used 
   * twice(fakely), so that the expression would be not evaluated 
   * between the two calls. Virtually reduce load/store, as all xmm 
   * registers are not saved by callee.  * */
  realpart = WN_Mpy(rtype,
		    WN_LdidPreg(rtype, expN),
		    cosine);
  
  imagpart = WN_Mpy(rtype,
		    WN_LdidPreg(rtype, expN),
		    sine);
  
  return WN_Complex(type, realpart, imagpart);
}

/* ====================================================================
 *
 *  WN *em_complex_cos(WN *block, WN *x) 
 *
 *	real =  cos(rz) * cosh(iz);
 *	imag = -sin(rz) * sinh(iz);
 *
 * ==================================================================== */
static WN *em_complex_cos(WN *block, WN *x) 
{
  TYPE_ID	type = WN_rtype(x);
  TYPE_ID	rtype = Mtype_complex_to_real(type);
  PREG_NUM	zN;
  WN		*realpart, *imagpart ;

  zN = AssignExpr(block, x, type);

  {
    INTRINSIC	cosID;
    INTRINSIC	coshID;
    WN		*cos, *cosh;

    switch(rtype)
    {
    case MTYPE_F4:	
      cosID = INTRN_F4COS;
      coshID = INTRN_F4COSH;
      break;
    case MTYPE_F8:
      cosID = INTRN_F8COS;
      coshID = INTRN_F8COSH;
      break;
    case MTYPE_FQ:	
      cosID = INTRN_FQCOS;
      coshID = INTRN_FQCOSH;
      break;
    }
    cos= Intrinsic(rtype,
		   cosID,
		   1,
		   WN_Realpart(rtype,WN_LdidPreg(type, zN)), NULL);
    cosh= Intrinsic(rtype,
		    coshID,
		    1,
		    WN_Imagpart(rtype,WN_LdidPreg(type, zN)), NULL);

   /*
    *	real =  cos(rz) * cosh(iz);
    */
    realpart = WN_Mpy(rtype, cos, cosh);
  }
  {
    INTRINSIC	sinID, sinhID;
    WN		*sin, *sinh;

    switch(rtype)
    {
    case MTYPE_F4:	
      sinID = INTRN_F4SIN;
      sinhID = INTRN_F4SINH;
      break;
    case MTYPE_F8:	
      sinID = INTRN_F8SIN;
      sinhID = INTRN_F8SINH;
      break;
    case MTYPE_FQ:	
      sinID = INTRN_FQSIN;
      sinhID = INTRN_FQSINH;
      break;
    }

    sin= Intrinsic(rtype,
		   sinID,
		   1,
		   WN_Realpart(rtype,WN_LdidPreg(type, zN)), NULL);
    sinh= Intrinsic(rtype,
		    sinhID,
		    1,
		    WN_Imagpart(rtype,WN_LdidPreg(type, zN)), NULL);
   /*
    *	imag = -sin(rz) * sinh(iz);
    */
    imagpart = WN_Neg(rtype, WN_Mpy(rtype, sin, sinh));
  }
  return WN_Complex(type, realpart, imagpart);
}

/* ====================================================================
 *
 *  WN *em_complex_sin(WN *block, WN *x) 
 *
 *	real = sin(rz) * cosh(iz);
 *	imag = cos(rz) * sinh(iz);
 *
 * ==================================================================== */
static WN *em_complex_sin(WN *block, WN *x) 
{
  TYPE_ID	type = WN_rtype(x);
  TYPE_ID	rtype = Mtype_complex_to_real(type);
  PREG_NUM	zN;
  WN		*realpart, *imagpart ;

  zN = AssignExpr(block, x, type);

  {
    INTRINSIC	sinID, coshID;
    WN		*sin, *cosh;

    switch(rtype)
    {
    case MTYPE_F4:	
      sinID = INTRN_F4SIN;
      coshID = INTRN_F4COSH;
      break;
    case MTYPE_F8:
      sinID = INTRN_F8SIN;
      coshID = INTRN_F8COSH;
      break;
    case MTYPE_FQ:	
      sinID = INTRN_FQSIN;
      coshID = INTRN_FQCOSH;
      break;
    }
    sin= Intrinsic(rtype,
		   sinID,
		   1,
		   WN_Realpart(rtype,WN_LdidPreg(type, zN)), NULL);

    cosh= Intrinsic(rtype,
		    coshID,
		    1,
		    WN_Imagpart(rtype,WN_LdidPreg(type, zN)), NULL);
  
   /*
    *	real = sin(rz) * cosh(iz);
    */
    realpart = WN_Mpy(rtype, sin, cosh);
  }
  {
    INTRINSIC	cosID, sinhID;
    WN		*cos, *sinh;

    switch(rtype)
    {
    case MTYPE_F4:	
      cosID = INTRN_F4COS;
      sinhID = INTRN_F4SINH;
      break;
    case MTYPE_F8:	
      cosID = INTRN_F8COS;
      sinhID = INTRN_F8SINH;
      break;
    case MTYPE_FQ:	
      cosID = INTRN_FQCOS;
      sinhID = INTRN_FQSINH;
      break;
    }

    cos= Intrinsic(rtype,
		   cosID,
		   1,
		   WN_Realpart(rtype,WN_LdidPreg(type, zN)), NULL);
    sinh= Intrinsic(rtype,
		    sinhID,
		    1,
		    WN_Imagpart(rtype,WN_LdidPreg(type, zN)), NULL);
   /*
    *	imag = cos(rz) * sinh(iz);
    */
    imagpart = WN_Mpy(rtype, cos, sinh);
  }
  return WN_Complex(type, realpart, imagpart);
}


/* ====================================================================
 *
 *  WN *em_preg_hypot(WN *block, TYPE_ID type, PREG_NUM xN, PREG_NUM yN)
 *
 *  compute sqrt ( x**2 + y**2 ) via two methods, depending on
 *  Fast_Complex_Allowed (maybe this should be (Roundoff_Level >= ROUNDOFF_ANY)??
 *
 *  WARNING!!
 *
 *  If (Fast_Complex_Allowed == FALSE) will generate more accurate code
 *  but divide by zero for xN and yN both zero !!!
 *
 * Mod: xN and yN is not PREG anymore, as some of the following code 
 * can not deal with PREG
 * ==================================================================== */
static WN *em_preg_hypot(WN *block, TYPE_ID type, WN *xT, WN *yT)
{
  if (Fast_Complex_Allowed)
  {
   /*
    *     SQRT( x**2 + y**2 )
    *
    */
    WN	*x2, *y2, *add, *hypot;
    
    x2 = WN_Mpy(type,
		WN_COPY_Tree(xT),
		WN_COPY_Tree(xT));

    y2 = WN_Mpy(type,
		WN_COPY_Tree(yT),
		WN_COPY_Tree(yT));

    add = WN_Add(type, x2, y2);
 
    hypot = WN_Sqrt(type, add);

    return hypot;
  }
  else
  {

   /*
    *    After factoring out max( |x| , |y| )
    *
    *	| x | >  | y |
    *
    *		| x | * SQRT(1 + (y/x)**2)
    *
    *	| y | >  | x |
    *
    *		| y | * SQRT(1 + (x/y)**2)
    *
    */
    PREG_NUM	axN, ayN, zN, divN;
    
    WN	*cond, *w, *z, *div, *mpy, *add, *sqrt, *hypot, *az;
    
    axN = AssignExpr(block,
		     WN_Abs(type, WN_COPY_Tree(xT)),
		     type);
    
    ayN = AssignExpr(block,
		     WN_Abs(type, WN_COPY_Tree(yT)),
		     type);
    
   /*
    *  w =  | x | >  | y |  ?  y : x
    *  z =  | x | >  | y |  ?  x : y;
    *
    *  Let div = w / z
    */
    cond = WN_GT(type,
		 WN_LdidPreg(type, axN),
		 WN_LdidPreg(type, ayN)),

    w = WN_Select(type,
		  cond,
		  WN_COPY_Tree(yT),
		  WN_COPY_Tree(xT));

    cond = WN_GT(type,
		 WN_LdidPreg(type, axN),
		 WN_LdidPreg(type, ayN));

    z = WN_Select(type,
		  cond,
		  WN_COPY_Tree(xT),
		  WN_COPY_Tree(yT));

    zN = AssignExpr(block, z, type);

    div = WN_Div(type,
		 w,
		 WN_LdidPreg(type, zN));

    divN = AssignExpr(block, div, type);

   /*
    * form zN * SQRT(1.0 + divN**2)
    */
    mpy =  WN_Mpy(type, 
		  WN_LdidPreg(type, divN),
		  WN_LdidPreg(type, divN));

    add = WN_Add(type,
		 WN_Floatconst(type, 1.0),
		 mpy);

    sqrt = WN_Sqrt(type, add);

    az = WN_Abs(type, WN_LdidPreg(type, zN));

    hypot = WN_Mpy(type, sqrt, az);

    return hypot;
  }
}

/* ====================================================================
 *
 *
 * ==================================================================== */
static WN *em_hypot(WN *block, WN *x, WN *y) 
{
  TYPE_ID	type = WN_rtype(x);

  Is_True((type == WN_rtype(y)), ("em_hypot(): type mismatch"));

  return em_preg_hypot(block, type, x, y); 
}

/* ====================================================================
 *
 *  WN *em_complex_log(WN *block, WN *x) 
 *
 *	real =	log ( sqrt(rz**2 + iz**2) )
 *	imag =	fatan2(iz, rz)
 *
 * ==================================================================== */
static WN *em_complex_log(WN *block, WN *x) 
{
  PREG_NUM	zN;
  WN		*hypot, *realpart, *imagpart ;
  TYPE_ID	type = WN_rtype(x);
  TYPE_ID	rtype = Mtype_complex_to_real(type);

  zN = AssignExpr(block, x, type);

 /*
  *  log(0) already undefined, so there is no need to test for zer
  */
  hypot = em_hypot(block,
		   WN_Realpart(rtype,WN_LdidPreg(type,zN)),
		   WN_Imagpart(rtype,WN_LdidPreg(type,zN)));
		   

  {
    INTRINSIC	logID;

    switch(rtype)
    {
    case MTYPE_F4:	logID = INTRN_F4LOG; break;
    case MTYPE_F8:	logID = INTRN_F8LOG; break;
    case MTYPE_FQ:	logID = INTRN_FQLOG; break;	
    }

    realpart= Intrinsic(rtype, logID, 1, hypot, NULL);
  }
  {
    INTRINSIC	atan2ID;

    switch(rtype)
    {
    case MTYPE_F4:	atan2ID = INTRN_F4ATAN2; break;
    case MTYPE_F8:	atan2ID = INTRN_F8ATAN2; break;
    case MTYPE_FQ:	atan2ID = INTRN_FQATAN2; break;	
    }
    imagpart= Intrinsic(rtype, atan2ID, 2,
			WN_Imagpart(rtype,WN_LdidPreg(type, zN)),
			WN_Realpart(rtype,WN_LdidPreg(type, zN)));
			
  }
  return WN_Complex(type, realpart, imagpart);
}

/* ====================================================================
 *
 *  WN *em_complex_abs(WN *block, WN *x) 
 *
 *	( sqrt(rz**2 + iz**2) )
 *
 * ==================================================================== */
static WN *em_complex_abs(WN *block, WN *z) 
{
  PREG_NUM	zN;
  WN		*hypot;
  TYPE_ID	type = WN_rtype(z);
  TYPE_ID	rtype = Mtype_complex_to_real(type);

  zN = AssignExpr(block, z, type);

  if (Fast_Complex_Allowed==FALSE)
  {
   /*
    *  It is unfortunate that we have to "know" the internals of em_preg_hypot
    *  We must check if z==0  
    */
    WN	*if_else;

    if_else = WN_CreateBlock();

    hypot = em_hypot(if_else, 
		     WN_Realpart(rtype,WN_LdidPreg(type,zN)),
		     WN_Imagpart(rtype,WN_LdidPreg(type,zN)));

    hypot = checkForZero(block, type, zN, if_else, hypot);
  }
  else
  {
    hypot = em_hypot(block, 
		     WN_Realpart(rtype,WN_LdidPreg(type,zN)),
		     WN_Imagpart(rtype,WN_LdidPreg(type,zN)));
  }

  return hypot;
}

/* ====================================================================
 *
 *  WN *em_complex_sqrt_preg(WN *block, TYPE_ID type, PREG_NUM zN) 
 *
 *  From library routine __zsqrt
 *	R(z) == I(z) == 0
 *		real = imag = 0
 *	R(z)>0
 *		real = 		sqrt ( (abs(z) + R(z)) / 2 )
 *		imag = 		I(z) / (2 * real)
 *	R(z)<=0
 *		imag = 		sqrt ( (abs(z) - R(z)) / 2 )
 *		if (I(z)<0)
 *			imag =		-imag;
 *		real = 		I(z) / (2 * imag)
 *
 *  The implementation will be
 *
 *	t1 =	sqrt ( (abs(z) + abs(R(z))) * .5 )	(NOTE R(z) always>0)
 *	t2 =	(R(z)<=0 && I(z)<0) -t1 : t1;
 *	t3 = 	(I(z) / t2) * .5
 *	realpart = (R(z)>0) ? t2 : t3
 *	imagpart = (R(z)>0) ? t3 : t2
 *
 * ==================================================================== */
static WN *em_complex_sqrt_preg(WN *block, TYPE_ID type, PREG_NUM zN_in) 
{
  PREG_NUM	ziN, zN, absN, t1N, t2N, t3N;
  TYPE_ID	rtype = Mtype_complex_to_real(type);

  zN = AssignExpr(block,WN_Realpart(rtype,WN_LdidPreg(type,zN_in)),rtype);
  ziN = AssignExpr(block,WN_Imagpart(rtype,WN_LdidPreg(type,zN_in)),rtype);

  {
   /*
    *	t1 =	sqrt ( (abs(z) + abs(R(z)) * .5 )
    */
    WN	*norm, *add, *mpy, *sqrt;

    norm = em_preg_hypot(block, rtype, WN_LdidPreg(rtype, zN), WN_LdidPreg(rtype, ziN));

    absN = AssignExpr(block, norm, rtype);

    add = WN_Add(rtype,
		 WN_LdidPreg(rtype, absN),
    		 WN_Abs(rtype, WN_LdidPreg(rtype, zN)));

    mpy = WN_Mpy(rtype, 
		 add,
		 WN_Floatconst(rtype, .5000));

    sqrt = WN_Sqrt(rtype, mpy);

    t1N = AssignExpr(block, sqrt, rtype);
  }
  {
   /*
    *	t2 =	(R(z)<=0 && I(z)<0) -t1 : t1;
    */
    WN	*le, *lt, *cond, *neg, *sel;

    le =  WN_LE(rtype,
		WN_LdidPreg(rtype, zN),
		WN_Zerocon(rtype));

    lt =  WN_LT(rtype,
		WN_LdidPreg(rtype, ziN),
		WN_Zerocon(rtype));

    cond = WN_LAND(le, lt);

    neg = WN_Neg(rtype, WN_LdidPreg(rtype, t1N));

    sel = WN_Select(rtype,
		    cond, 
		    neg,
		    WN_LdidPreg(rtype, t1N));

    t2N = AssignExpr(block, sel, rtype);
  }
  {
   /*
    *	t3 = 	(I(z) / t2) * .5
    */
    WN	*div, *mpy;

    div = WN_Div(rtype, 
		 WN_LdidPreg(rtype, ziN),
		 WN_LdidPreg(rtype, t2N));

    mpy = WN_Mpy(rtype, 
		 div,
		 WN_Floatconst(rtype, .5000));

    t3N = AssignExpr(block, mpy, rtype);
  }
  {
   /*
    *	realpart = (R(z)>0) ? t2 : t3
    *	imagpart = (R(z)>0) ? t3 : t2
    */
    WN	*gt, *realpart, *imagpart;

    gt =  WN_GT(rtype,
		WN_LdidPreg(rtype, zN),
		WN_Zerocon(rtype));

    realpart = WN_Select(rtype,
			 gt,
			 WN_LdidPreg(rtype, t2N),
			 WN_LdidPreg(rtype, t3N));

    gt =  WN_GT(rtype,
		WN_LdidPreg(rtype, zN),
		WN_Zerocon(rtype));

    imagpart = WN_Select(rtype,
			 gt,
			 WN_LdidPreg(rtype, t3N),
			 WN_LdidPreg(rtype, t2N));

    return WN_Complex(type, realpart, imagpart);
  }
}


/* ====================================================================
 *
 *  WN *em_complex_sqrt(WN *block, WN *z) 
 *
 * ==================================================================== */
static WN *em_complex_sqrt(WN *block, WN *z) 
{
  PREG_NUM	zN;
  WN		*sqrt, *if_else;
  TYPE_ID	type = WN_rtype(z);

  zN = AssignExpr(block, z, type);

  if_else = WN_CreateBlock();

  sqrt = em_complex_sqrt_preg(if_else, type, zN);

  sqrt = checkForZero(block, type, zN, if_else, sqrt);

  return sqrt;
}

/* ====================================================================
 *
 *  WN *em_conjg(WN *block, WN *x) 
 *
 *	real =	 Realpart(x)
 *	imag =	-Imagpart(x)
 *
 * ==================================================================== */
static WN *em_conjg(WN *block, WN *x) 
{
  PREG_NUM	zN;
  TYPE_ID	type = WN_rtype(x);
  TYPE_ID	rtype = Mtype_complex_to_real(type);
  WN		*realpart, *imagpart;

  zN = AssignExpr(block, x, type);

  realpart = WN_Realpart(rtype, WN_LdidPreg(type, zN));

  imagpart = WN_Neg(rtype,
		    WN_Imagpart(rtype, WN_LdidPreg(type, zN)));
  return WN_Complex(type, realpart, imagpart);
}

/* ====================================================================
 *
 *  WN *em_alog10(WN *block, WN *x) 
 *
 *	log(x) * (M_LOG10E= 0.43429448190325182765)
 *
 * ==================================================================== */
#define M_LOG10 0.4342944819032518276511289189166050822943970058036665661144537831658646492088707747292249493384317483
#define M_LOG10Q 0.434294481903251827651128918916605082294L
static WN *em_alog10(WN *block, WN *x) 
{
  TYPE_ID	type = WN_rtype(x);
  INTRINSIC	logID;
  WN		*log, *mpy;

  switch(type)
  {
  case MTYPE_F4:	logID = INTRN_F4LOG; break;
  case MTYPE_F8:	logID = INTRN_F8LOG; break;
  case MTYPE_FQ:	logID = INTRN_FQLOG; break;	
  }

  log = Intrinsic(type, logID, 1, x, NULL);

  if (type != MTYPE_FQ) {
     mpy =  WN_Mpy(type, 
		   WN_Floatconst(type, M_LOG10),
		   log);
  } else {
#ifdef TARG_LOONGSON
     TCON t_log10;
     t_log10.vals.qval.qval[0]=  0x555f5a68;
     t_log10.vals.qval.qval[1] =  0xe32a6ab7;
     t_log10.vals.qval.qval[2] =  0xb1526e50;
     t_log10.vals.qval.qval[3] =  0x3ffdbcb7;
     mpy = WN_Mpy(type, Make_Const(t_log10), log);
#else
     mpy =  WN_Mpy(type, 
		   Make_Const(Host_To_Targ_Quad(M_LOG10Q)),
		   log);
#endif
  }
  return mpy;
}

/* ====================================================================
 *
 *  WN *em_bclr(WN *block, WN *n, WN *i)
 *	clear bit i (n, i)
 *	if (0 <= i && i < NUMBERBITS)		n & ~(1<<i)
 *  	else					0
 *
 * ==================================================================== */

static WN *em_bclr(WN *block, WN *n, WN *i)
{
  TYPE_ID	type = WN_rtype(n);
  PREG_NUM	iN;
  WN		*cond, *band;

  iN = AssignExpr(block, i, type);
  {
   /*
    * form	n & ~(1<<i)
    */
    WN	*shft, *bnot;

    shft = WN_Shl(type,
	          WN_Intconst(type, 1), 
	          WN_LdidPreg(type, iN));

    bnot = WN_Bnot(type, shft);
  
    band = WN_Band(type, n, bnot);

    if (Fast_Bit_Allowed)
      return band;
  }
  {
   /*
    *  form logical condition
    *	(0 <= i && i < NUMBERBITS)
    */
    WN	*le, *lt;

    le =  WN_LE(Mtype_comparison(type),
		WN_Zerocon(type),
		WN_LdidPreg(type, iN));
    lt =  WN_LT( Mtype_comparison(type),
		WN_LdidPreg(type, iN),
		WN_Intconst(type, MTYPE_size_reg(type)));
    cond = WN_LAND(le, lt);
  }
  return WN_Select(type, cond, band, WN_Zerocon(type));
}

/* ====================================================================
 * Parity - Use xors to combine integers bits down to a single bit.
 *   t1 =  x ^ (x  >> 32);
 *   t2 = t1 ^ (t1 >> 16);
 *   t3 = t2 ^ (t2 >>  8);
 *   t4 = t3 ^ (t3 >>  4);
 *   return (0x6996 >> (t4 & 0xf)) & 1;
 * ==================================================================== */

static WN *em_parity(WN *block, WN *wn)
{
  TYPE_ID type = WN_rtype(wn);
  INT bitsize = MTYPE_size_reg(type);
  // Parity of sign/zero extension is always zero.
  if ((WN_operator(wn) == OPR_LDID || WN_operator(wn) == OPR_ILOAD)
      && bitsize > MTYPE_size_reg(WN_desc(wn))) {
    bitsize = MTYPE_size_reg(WN_desc(wn));
  }
  PREG_NUM preg = AssignExpr( block, wn, type );

  // t1 =  x ^ (x  >> 32);
  if (bitsize > 32) {
    wn = WN_Ashr( type, WN_LdidPreg(type, preg), WN_Intconst(MTYPE_I4, 32) );
    wn = WN_Bxor( type, wn, WN_LdidPreg(type, preg) );
    preg = AssignExpr( block, wn, type );
  }
  // t2 = t1 ^ (t1 >> 16);
  if (bitsize > 16) {
    wn = WN_Ashr( type, WN_LdidPreg(type, preg), WN_Intconst(MTYPE_I4, 16) );
    wn = WN_Bxor( type, wn, WN_LdidPreg(type, preg) );
    preg = AssignExpr( block, wn, type );
  }
  // t3 = t2 ^ (t1 >>  8);
  if (bitsize > 8) {
    wn = WN_Ashr( type, WN_LdidPreg(type, preg), WN_Intconst(MTYPE_I4, 8) );
    wn = WN_Bxor( type, wn, WN_LdidPreg(type, preg) );
    preg = AssignExpr( block, wn, type );
  }
  // t4 = t3 ^ (t1 >>  4);
  wn = WN_Ashr( type, WN_LdidPreg(type, preg), WN_Intconst(MTYPE_I4, 4) );
  wn = WN_Bxor( type, wn, WN_LdidPreg(type, preg) );

  // return (0x6996 >> (t4 & 0xf)) & 1;
  wn = WN_Band( type, wn, WN_Intconst(MTYPE_I4, 15) );
  wn = WN_Ashr( MTYPE_I4, WN_Intconst(MTYPE_I4, 0x6996), wn );
  wn = WN_Band( MTYPE_I4, wn, WN_Intconst(MTYPE_I4, 1) );
  return wn;
}


/* ====================================================================
 * Popcount - Count the number of "1" bits in an integer.  Here's the
 * 64-bit algorithm:
 *   t1 = x - ((x >> 1) & 0x5555555555555555);
 *   t2 = (t1 & 0x3333333333333333) + ((t1 >> 2) & 0x3333333333333333);
 *   t3 = (t2 + (t2 >> 4)) & 0x0f0f0f0f0f0f0f0f;
 *   t4 = t3 + (t3 >> 8);
 *   t5 = t4 + (t4 >> 16);
 *   t6 = t5 + (t5 >> 32);
 *   return t6 & 0x000000ff;
 * ==================================================================== */

static WN *em_popcount(WN *block, WN *wn, INT bitsize)
{
  if ( bitsize == 0 ) {
    Fail_FmtAssertion("em_popcount: expected nonzero bitsize");
  }
  TYPE_ID type = WN_rtype(wn);

  // t1 = x - ((x >> 1) & 0x5555555555555555);
  PREG_NUM preg = AssignExpr( block, wn, type );
  UINT64 mask = 0x5555555555555555ULL >> (64 - bitsize);
  WN *wn1 = WN_Intconst(type, mask);
  wn = WN_Ashr( type, WN_LdidPreg(type, preg), WN_Intconst(MTYPE_I4, 1) );
  wn = WN_Band( type, wn, wn1 );
  wn = WN_Sub( type, WN_LdidPreg(type, preg), wn );

  // t2 = (t1 & 0x3333333333333333) + ((t1 >> 2) & 0x3333333333333333);
  preg = AssignExpr( block, wn, type );
  mask = 0x3333333333333333ULL >> (64 - bitsize);
  wn1 = WN_Intconst(type, mask);
  PREG_NUM preg1 = AssignExpr( block, wn1, type );
  wn = WN_Band( type, WN_LdidPreg(type, preg), WN_LdidPreg(type, preg1) );
  wn1 = WN_Ashr( type, WN_LdidPreg(type, preg), WN_Intconst(MTYPE_I4, 2) );
  wn1 = WN_Band( type, wn1, WN_LdidPreg(type, preg1) );
  wn = WN_Add( type, wn, wn1 );

  // t3 = (t2 + (t2 >> 4)) & 0x0f0f0f0f0f0f0f0f;
  preg = AssignExpr( block, wn, type );
  mask = 0x0f0f0f0f0f0f0f0fULL >> (64 - bitsize);
  wn1 = WN_Intconst(type, mask);
  wn = WN_Ashr( type, WN_LdidPreg(type, preg), WN_Intconst(MTYPE_I4, 4) );
  wn = WN_Add( type, WN_LdidPreg(type, preg), wn );
  wn = WN_Band( type, wn, wn1 );

  if (bitsize > 8) {
    // t4 = t3 + (t3 >> 8);
    preg = AssignExpr( block, wn, type );
    wn = WN_Ashr( type, WN_LdidPreg(type, preg), WN_Intconst(MTYPE_I4, 8) );
    wn = WN_Add( type, WN_LdidPreg(type, preg), wn );
  }

  if (bitsize > 16) {
    // t5 = t4 + (t4 >> 16);
    preg = AssignExpr( block, wn, type );
    wn = WN_Ashr( type, WN_LdidPreg(type, preg), WN_Intconst(MTYPE_I4, 16) );
    wn = WN_Add( type, WN_LdidPreg(type, preg), wn );
  }

  if (bitsize > 32) {
    // t6 = t5 + (t5 >> 32);
    preg = AssignExpr( block, wn, type );
    wn = WN_Ashr( type, WN_LdidPreg(type, preg), WN_Intconst(MTYPE_I4, 32) );
    wn = WN_Add( type, WN_LdidPreg(type, preg), wn );
  }

  if (bitsize > 8) {
    // return t6 & 0x000000ff;
    // wn = WN_Band( type, wn, WN_Intconst(MTYPE_I4, 0xff) );
    wn = WN_CreateCvtl( OPC_U4CVTL, 8, wn );
  }

  return wn;
}

/* ====================================================================
 *
 *  WN *em_bset(WN *block, WN *n, WN *i)
 *
 *	set bit i (n, i)
 *	if (0 <= i && i < NUMBERBITS):		n | (1<<i)
 *	else					0
 *
 * ==================================================================== */
static WN *em_bset(WN *block, WN *n, WN *i)
{
  TYPE_ID	type = WN_rtype(n);
  PREG_NUM	iN;
  WN		*cond, *bor;

  iN = AssignExpr(block, i, type);
  {
   /*
    *  form	n | (1<<i)
    */
    WN	*shft;

    shft = WN_Shl(type,
	          WN_Intconst(type, 1), 
	          WN_LdidPreg(type, iN));

    bor = WN_Bior(type, n, shft);

    if (Fast_Bit_Allowed)
      return bor;
  }
  {
   /*
    *  form logical condition
    *	(0 <= i && i < NUMBERBITS)
    */
    WN	*le, *lt;

    le =  WN_LE(Mtype_comparison(type),
		WN_Zerocon(type),
		WN_LdidPreg(type, iN));
    lt =  WN_LT(Mtype_comparison(type),
		WN_LdidPreg(type, iN),
		WN_Intconst(type, MTYPE_size_reg(type)));
    cond = WN_LAND(le, lt);
  }
  return WN_Select(type, cond, bor, WN_Zerocon(type));
}

/* ====================================================================
 *
 *	test bit i (n, i)
 *	if (0 <= i && i < NUMBERBITS):		(n >> i) & 0x1
 *      else					0
 *
 * ==================================================================== */
static WN *em_btest(WN *block, WN *n, WN *i)
{
  TYPE_ID	type = WN_rtype(n);
  PREG_NUM	iN;
  WN		*cond, *band;

  iN = AssignExpr(block, i, type);
  {
   /*
    *  form (n >> i) & 0x1
    */
    WN	*shft;

    shft = WN_Lshr(type,
		   n,
	           WN_LdidPreg(type, iN));

    band = WN_Band(type, shft, WN_Intconst(type, 1));

    if (Fast_Bit_Allowed)
      return band;
  }
  {
   /*
    *  form logical condition
    *	(0 <= i && i < NUMBERBITS)
    */
    WN	*le, *lt;

    le =  WN_LE(Mtype_comparison(type),
		WN_Zerocon(type),
		WN_LdidPreg(type, iN));
    lt =  WN_LT(Mtype_comparison(type),
		WN_LdidPreg(type, iN),
		WN_Intconst(type, MTYPE_size_reg(type)));
    cond = WN_LAND(le, lt);
  }
  return WN_Select(type, cond, band, WN_Zerocon(type));
}


/* ====================================================================
 *
 *	auxilary routine to help develop mask
 *	does not check args, and assumes x,m are used once (dag police)
 *
 *	( (unsigned) -(m>0) >> (32-m))
 *
 *	((1<<m)-1) does not work, when m = 32
 *
 * ==================================================================== */
static WN *WN_mask(TYPE_ID type, PREG_NUM m)
{
  WN	*gt, *neg, *sub;
  gt = WN_GT(type, WN_LdidPreg(type, m), WN_Zerocon(type));
  gt = WN_Int_Type_Conversion(gt,type);
  neg = WN_Neg(type, gt);
  sub = WN_Sub(type, WN_Intconst(type, MTYPE_size_reg(type)), WN_LdidPreg(type, m));
  return WN_Lshr(type, neg, sub);
}

/* ====================================================================
 *
 *	auxilary routine to help develop mask
 *	does not check args, and assumes x,m are used once (dag police)
 *
 *	return x & ((1<<m)-1);
 *	return x & ( (unsigned) -(m>0) >> (32-m))
 *
 * ==================================================================== */
static WN *em_mask(TYPE_ID type, WN *x, PREG_NUM m)
{
  WN	*mask;

  mask = WN_mask(type, m);

  return WN_Band(type, x, mask);
}

/* ====================================================================
 *
 *	auxilary routine to help develop mask
 *	does not check args, and assumes x,m are used once (dag police)
 *
 *	return x & ~ MASK(m)
 *
 * ==================================================================== */
static WN *em_mask_complement(TYPE_ID type, WN *x, PREG_NUM m)
{
  WN	*mask, *bnot;

  mask = WN_mask(type, m);
 
  bnot =WN_Bnot(type, mask);

  return WN_Band(type, x, bnot);
}
 
/* ====================================================================
 *
 *	extract bits [i ... i+len-1] (n, i, len)
 *
 *	if (0 <= i   && i   < NUMBERBITS)	&&
 *	   (0 <= len && len <= NUMBERBITS)	&&
 *	   ((i+len) <= NUMBERBITS)		(n>>i) & (1<<len -1)
 *      else					n
 * ==================================================================== */
static WN *em_bits(WN *block, WN *n, WN *i, WN *len)
{
  TYPE_ID	type = WN_rtype(n);
  TYPE_ID	desc = WN_desc(n);
  PREG_NUM	iN, lenN, nN;
  WN		*cond, *band;

  iN = AssignExpr(block, i, desc);
  nN = AssignExpr(block, n, desc);
  lenN = AssignExpr(block, len, desc);
  {
   /*
    *
    *	 form	(n>>i) & MASK(len)
    */
    WN	*shft;

    shft = WN_Lshr(type,
		   WN_LdidPreg(type, nN),
	           WN_LdidPreg(type, iN));

    band = em_mask(type, shft, lenN);

    if (Fast_Bit_Allowed)
      return band;
  }
  {
   /*
    *  Unfortunately the region of definition is very irregular
    *  so I believe all these tests are necessary to be compatible
    *  with the library function
    *
    *	(0 <= i) && (i < NUMBERBITS)	&&
    *	(0 <= len)			&&
    *	((i+len) <= NUMBERBITS)
    */
    WN	*le, *lt, *land, *add;

    le =  WN_LE(Mtype_comparison(type),
		WN_Zerocon(type),
		WN_LdidPreg(type, iN));
    lt =  WN_LT(Mtype_comparison(type),
		WN_LdidPreg(type, iN),
		WN_Intconst(type, MTYPE_size_reg(type)));
    land = WN_LAND(le, lt);

    le =  WN_LE(Mtype_comparison(type),
		WN_Zerocon(type),
		WN_LdidPreg(type, lenN));
    land = WN_LAND(land, le);

    add = WN_Add(type,
		 WN_LdidPreg(type, iN),
		 WN_LdidPreg(type, lenN));

    lt = WN_LE(Mtype_comparison(type),
	       add,
	       WN_Intconst(type, MTYPE_size_reg(desc)));
    cond = WN_LAND(land, lt);
  }
  return WN_Select(type, cond, band, WN_LdidPreg(type, nN));
}

/* ====================================================================
 *	shift n >> i places
 *
 *	 |i| < NUMBERBITS
 *		(n<<i)
 *	 else	0
 *
 * ==================================================================== */
static WN *em_shl(WN *block, WN *n, WN *i)
{
  TYPE_ID	type = WN_rtype(n);
  TYPE_ID	desc = WN_desc(n);
  PREG_NUM	iN, nN;

  iN = AssignExpr(block, i, desc);
  nN = AssignExpr(block, n, desc);
  {
   /*
    *  form logical condition
    *	 |i| < NUMBERBITS
    *		(n<<i)
    *	 else	0
    */
    WN	*shl, *lt;

    shl = WN_Shl(type,
		 WN_LdidPreg(desc, nN),
    		 WN_LdidPreg(desc, iN));

    if (Fast_Bit_Allowed)
      return shl;

    lt =  WN_LT(Mtype_comparison(type),
		WN_LdidPreg(desc, iN),
		WN_Intconst(type, MTYPE_size_reg(desc)));

    return WN_Select(type, lt, shl, WN_Zerocon(type));
  }
}

/* ====================================================================
 *	logical shift n right i places
 *
 *	 |i| < NUMBERBITS
 *		(n>>i)
 *	 else	0
 *
 * ==================================================================== */
static WN *em_lshr(WN *block, WN *n, WN *i)
{
  TYPE_ID	type = WN_rtype(n);
  TYPE_ID	desc = WN_desc(n);
  PREG_NUM	iN, nN;

  iN = AssignExpr(block, i, desc);
  nN = AssignExpr(block, n, desc);
  {
   /*
    *  (n>>i) & ( (1<<(NUMBERBITS-i)) - 1)
    */
    WN		*val, *sub, *lt, *shr;
    PREG_NUM	subN;

    val = WN_Lshr(type, WN_LdidPreg(type, nN), WN_LdidPreg(type, iN));

    sub = WN_Sub(type,
		 WN_Intconst(type, MTYPE_size_reg(desc)),
		 WN_LdidPreg(type, iN));

    subN = AssignExpr(block, sub, type);

    shr = em_mask(type, val, subN);

    if (Fast_Bit_Allowed)
      return shr;

    lt =  WN_LT(Mtype_comparison(type),
		WN_LdidPreg(desc, iN),
		WN_Intconst(type, MTYPE_size_reg(desc)));

    return WN_Select(type, lt, shr, WN_Zerocon(type));
  }
}

/* ====================================================================
 *	shift n i places
 *
 *	 |i| < NUMBERBITS
 *		i>=0	(n<<i)
 *		i<0	(n>>(-i)) & ( (1<<(NUMBERBITS-i)) - 1)
 *	 else	0
 *
 * ==================================================================== */
static WN *em_shft(WN *block, WN *n, WN *i)
{
  TYPE_ID	type = WN_rtype(n);
  TYPE_ID	desc = WN_desc(n);
  WN		*v1, *v2;
  PREG_NUM	iN, nN;

  iN = AssignExpr(block, i, type);
  nN = AssignExpr(block, n, type);
  {
   /*
    *	form [v1]	(n<<i)
    */
    v1 = WN_Shl(type,
		WN_LdidPreg(type, nN),
	        WN_LdidPreg(type, iN));
  }
  {
   /*
    *  form [v2]	(n>>(-i)) & ( (1<<(NUMBERBITS-(-i))) - 1)
    */
    WN		*neg, *val, *add;
    PREG_NUM	addN;

    neg = WN_Neg(type, WN_LdidPreg(type, iN));

    val = WN_Lshr(type, WN_LdidPreg(type, nN), neg);

    add = WN_Add(type,
		 WN_Intconst(type, MTYPE_size_reg(desc)),
		 WN_LdidPreg(type, iN));

    addN = AssignExpr(block, add, type);

    v2 = em_mask(type, val, addN);
  }
  {
   /*
    *  form logical condition
    *	 |i| < NUMBERBITS
    *		i>=0	v1
    *		i<0	v2
    *	 else	0
    */
    WN	*abs, *lt, *ge, *select;

    ge =  WN_GE(Mtype_comparison(type),
		WN_LdidPreg(type, iN),
		WN_Zerocon(type));

    select = WN_Select(type, ge, v1, v2);

    if (Fast_Bit_Allowed)
      return select;

    abs = WN_Abs(type, WN_LdidPreg(type, iN));

    lt =  WN_LT(Mtype_comparison(type),
		abs,
		WN_Intconst(type, MTYPE_size_reg(desc)));

    return WN_Select(type, lt, select, WN_Zerocon(type));
  }
}

/* ====================================================================

 * Circular shift - The rightmost ic bits are shifted circularly k-places.
 * k > 0 => left shift.
 * k < 0 => right shift.
 * k = 0 => no shift.
 *      left shift                       right shift
 *      [  b1   |   k2   |   d   ]       [  b1   |   d   |   k2   ]
 *
 *	MASK(x) =	(1<<x)-1;
 *
 *	k2 = | k |
 *	if (0 < k2 <= NUMBITS)	&&
 *	   (    k2 <= ic     )	&&
 *	   (1 <= ic <= NUMBITS)
 *	{
 *		s1 = (k>0) ? |k|    : ic-|k|;
 *		s2 = (k>0) ? ic-|k| : |k|
 *		B1 =  m & ~MASK(ic);
 *		B2 = (m & MASK(s2)) << s1;
 *		B2 = (m & MASK(ic)) >> s2;
 *		return (B1 | B2 | B3)
 *	}
 *	else	return m
 *
 * ==================================================================== */
static WN *em_shftc(WN *block, WN *m, WN *k, WN *ic)
{
  TYPE_ID	type = WN_rtype(m);
  TYPE_ID	desc = WN_desc(m);
  WN		*shiftc;
  PREG_NUM	mN, kN, icN, kabsN, s1N, s2N;

  mN = AssignExpr(block, m, desc);
  kN = AssignExpr(block, k, desc);
  icN = AssignExpr(block, ic, desc);

  kabsN = AssignExpr(block,
		     WN_Abs(type, WN_LdidPreg(type, kN)),
	 	     type);
  {
   /*
    *		s1 = (k>0) ? |k|    : ic-|k|;
    *		s2 = (k>0) ? ic-|k| : |k|
    */
    PREG_NUM	subN;
    WN		*sub, *gt, *s1, *s2;

    sub = WN_Sub(type,
		 WN_LdidPreg(type, icN),
		 WN_LdidPreg(type, kabsN));
    subN = AssignExpr(block, sub, type);
 
    gt =  WN_GT(Mtype_comparison(type),
		WN_LdidPreg(type, kN),
		WN_Zerocon(type));

    s1 = WN_Select(type, gt,
		   WN_LdidPreg(type, kabsN),
		   WN_LdidPreg(type, subN));

    s2 = WN_Select(type, WN_COPY_Tree(gt),
		   WN_LdidPreg(type, subN),
		   WN_LdidPreg(type, kabsN));

    s1N = AssignExpr(block, s1, type);
    s2N = AssignExpr(block, s2, type);
  }
  {
   /*
    *
    *		B1 =  m & ~MASK(ic);
    *		B2 = (m & MASK(s2)) << s1;
    *		B2 = (m & MASK(ic)) >> s2;
    *		shiftc = B1 | B2 | B2;
    */
    WN	*band, *b1, *b2, *b3;

    b1 = em_mask_complement(type, WN_LdidPreg(type, mN), icN);

    band = em_mask(type, WN_LdidPreg(type, mN), s2N);

    b2 = WN_Shl(type,
		band,
		WN_LdidPreg(type, s1N));

    band = em_mask(type, WN_LdidPreg(type, mN), icN);

    b3 = WN_Lshr(type,
		 band,
		 WN_LdidPreg(type, s2N));

    shiftc = WN_Bior(type,
		     b1,
    		     WN_Bior(type, b2, b3));

    if (Fast_Bit_Allowed)
      return shiftc;
  }
  {
   /*
    *  form logical condition
    *  the above boundary if equivalent to
    *	 if  1 <= | k | <= ic	&&
    *              ic   <= NUMBITS
    *		shiftc;
    *	 else	m
    */
    WN	*le1, *le2, *land, *le, *cond;

    le1 =  WN_LE(Mtype_comparison(type),
		 WN_Intconst(type, 1),
		 WN_LdidPreg(type, kabsN));

    le2 =  WN_LE(Mtype_comparison(type),
		 WN_LdidPreg(type, kabsN),
		 WN_LdidPreg(type, icN));
    land = WN_LAND(le1, le2);

    le =  WN_LE(Mtype_comparison(type),
		WN_LdidPreg(type, icN),
		WN_Intconst(type, MTYPE_size_reg(desc)));
    cond = WN_LAND(land, le);

    return WN_Select(type, cond, shiftc, WN_LdidPreg(type, mN));
  }
}


/* ====================================================================
 *
 * BOOL	  decompose_address
 *
 * TY_IDX aux_compute_alignment
 *
 * Memory intrinsics will be changing to MSTORE/MLOAD depending on
 * size and aliasing.
 *
 * During wopt we might lose alignment information, so we go to some
 * lengths here to compute a good alignment.
 *
 * ==================================================================== */

static BOOL decompose_address(WN *addr, WN **base, INT64 *offset)
{
  switch(WN_operator(addr))
  {
  case OPR_ADD:
  case OPR_SUB:
    if (WN_is_pointer(WN_kid0(addr)) && Is_Integer_Constant(WN_kid1(addr)))
    {
      *base=	WN_kid0(addr);
      // make sure offset is positive
      *offset=	ABS(WN_const_val(WN_kid1(addr)));
      return TRUE;
    }
    break;
  default:
    if (WN_is_pointer(addr))
    {
      *offset=	0;
      *base=	addr;
      return TRUE;
    }
  }
  return FALSE;
}

static TY_IDX aux_compute_alignment(WN *wn)
{
  WN	*base;
  INT64	offset;

  if (decompose_address(wn, &base, &offset))
  {
    return compute_alignment_type(base, TY_pointed(WN_ty(base)), offset);
  }
  return compute_alignment_type(wn, MTYPE_To_TY(MTYPE_V), 0);
}


/* ====================================================================
 *
 * BOOL check_size(WN *size, WN *src, WN *dst)
 *
 * Check if the size is a constant, and below the threshold 
 * of CG_memmove_inst_count
 *
 * Count src and dst (if present)
 *
 * ==================================================================== */
 
static BOOL check_size(WN *size, WN *src, WN *dst)
{
  if (Is_Integer_Constant(size))
  {
    TY_IDX srcTY, dstTY = (TY_IDX) 0;

    INT64 n = WN_const_val(size);

    /* Here we have increased the size (CG_memmove_inst_count) allowed
     * for inline memset. NVISA expects inline, not a call.
     * Future : we need to make this loop above some threshold. 
     */
    if (n <= CG_memmove_inst_count)
      return TRUE;
#ifdef TARG_NVISA 
    else
    {
        /* NVISA expects memset to gen inline code. If too large
         * emit an error
         */
        mUINT64 srcpos = WN_Get_Linenum (src);
        if (srcpos)
            ErrMsgSrcpos (EC_Memset_Too_Large, srcpos, n);
        else
            ErrMsg (EC_Memset_Too_Large, n);
    }
#endif

    srcTY = aux_compute_alignment(src);

    if (dst)
    {
      TY_IDX dstTY = aux_compute_alignment(dst);
      n +=	n;
    }

    {
      TYPE_ID quantum;
      INT32 copy_alignment, instructions;

      copy_alignment = compute_copy_alignment(srcTY, dstTY, 0);

      quantum = compute_copy_quantum(copy_alignment);
      instructions= n / MTYPE_alignment(quantum);

      if (instructions <= CG_memmove_inst_count)
        return TRUE;
    }
  }
  else if (CG_memmove_nonconst)
    return TRUE;

  return FALSE;
}


static void aux_memory_msg(const char *msg, WN *tree, WN *mstore)
{
}

static WN *aux_memset(WN *var, WN *con, WN *size)
{
  WN	*mstore, *newcon;
  TY_IDX align;

  Is_True(Is_Integer_Constant(con), ("expected integer constant in aux_memset"));

 /*
  *  The mstore TY_align will determine the eventual alignment of the
  *  component ISTOREs, so improve the alignment if possible
  */
  align = aux_compute_alignment(var);

 /*
  *  The semantics of memset require replicating the byte constant
  */
  newcon=	WN_I1const(WN_rtype(con), WN_const_val(con));
  WN_Delete(con);

  TY_IDX ptr_ty = Make_Pointer_Type (align);
  mstore = aux_CreateMstore(0, ptr_ty, newcon, var, size);
  // Attempts to call Copy_alias_info() lead to problems when 'src'
  // has Id() == preg_id().  This seems to lead to a segfault during
  // CG emit.  However, both Copy_alias_info() and Duplicate_alias_info()
  // happily propagate the Id() to the target WN.  For now I copy
  // the AliasTag directly to make forward progress.
  AliasAnalyzer *aa = AliasAnalyzer::aliasAnalyzer();
  if (aa)
    aa->transferAliasTag(mstore,var);
  return mstore;
}

static WN *aux_memcpy(WN *src, WN *dst, WN *size)
{
  WN	*mload, *mstore;
  TY_IDX srcTY, dstTY;
  TY_IDX srcTY_ptr, dstTY_ptr;

 /*
  *  The TY_align will determine the eventual alignment of the
  *  component ILOAD/ISTOREs, so improve the alignment if possible
  */
  srcTY = aux_compute_alignment(src);

  if (TY_size(srcTY) != 0 &&
      WN_const_val(size) % TY_size(srcTY) != 0) {
    // size copied is not a multiple of the size of the type, which means
    // that we are copying part of the type.  We then change the pointer
    // to (void*)
    srcTY_ptr = Make_Pointer_Type (MTYPE_To_TY (MTYPE_V));
  }
  else srcTY_ptr = Make_Pointer_Type(srcTY);

  mload = WN_CreateMload(0, srcTY_ptr, src, size);
  // Attempts to call Copy_alias_info() lead to problems when 'src'
  // has Id() == preg_id().  This seems to lead to a segfault during
  // CG emit.  However, both Copy_alias_info() and Duplicate_alias_info()
  // happily propagate the Id() to the target WN.  For now I copy
  // the AliasTag directly to make forward progress.
  //if (alias_manager)
    //Copy_alias_info(alias_manager,src,mload);
  AliasAnalyzer *aa = AliasAnalyzer::aliasAnalyzer();
  if (aa)
    aa->transferAliasTag(mload,src);

  dstTY = aux_compute_alignment(dst);

  dstTY_ptr = Make_Pointer_Type(dstTY);
  mstore = aux_CreateMstore(0, dstTY_ptr, mload, dst, WN_COPY_Tree(size));
  if (aa)
    aa->transferAliasTag(mstore,dst);
  return mstore;
}

static WN *em_memset(WN *block, WN *tree, WN *var, WN *con, WN *size)
{
  WN	*em = NULL;

  if (check_size(size, var, NULL) && Is_Integer_Constant(con))
  {
    if (em = aux_memset(var, con, size))
    {
      aux_memory_msg("memset()", tree, em);
      WN_INSERT_BlockLast(block, em);
      return WN_COPY_Tree(var);
    }
  }
  return em;
}

static WN *em_bzero(WN *block, WN *tree, WN *var, WN *size)
{
  WN	*em = NULL;

  if (check_size(size, var, NULL))
  {
    WN *zero = WN_Zerocon(MTYPE_U8);

    if (em = aux_memset(var, zero, size))
    {
      aux_memory_msg("bzero()", tree, em);
    }
    else
    {
      WN_Delete(zero);
    }
  }
  return em;
}

static WN *em_bcopy(WN *block, WN *tree, WN *src, WN *dst, WN *size)
{
  WN	*em = NULL;

  if (check_size(size, src, dst))
  {
    if (CG_bcopy_cannot_overlap ||
	!lower_is_aliased(src, dst, WN_const_val(size)))
    {
      if (em = aux_memcpy(src, dst, size)) {
        aux_memory_msg("bcopy()", tree, em);
      }
    }
  }
  return em;
}

/*
 *  memcpy requires the src/dst to be independent.
 *  The implementation however, handles the overlap cases, so we should also,
 *  unless we can prove otherwise or the user forces us not to.
 */
// KEY: The above comment is wrong. For memcpy, src and dest may not
// overlap.
static WN *em_memcpy(WN *block, WN *tree, WN *dst, WN *src, WN *size)
{
  if (check_size(size, src, dst))
  {
#ifdef KEY
    // bugs 3510, 3924
    if ( CG_memcpy_cannot_overlap ||  // TRUE
	 ( Is_Integer_Constant(size) &&
	   ! lower_is_aliased(src, dst, WN_const_val(size)) ) )
#else
    if (CG_memcpy_cannot_overlap ||
	!lower_is_aliased(src, dst, WN_const_val(size)))
#endif
    {
      if (WN *em = aux_memcpy(src, dst, size)) {
        aux_memory_msg("memcpy()", tree, em);
        WN_INSERT_BlockLast(block, em);
        return WN_COPY_Tree(dst);
      }
    }
  }
  return NULL;
}


/*
 *  memmov does indeed handle overlaping cases.
 *  We generate mload/mstore only when we can prove there is no overlap
 *  or the user forces us to.
 */
static WN *em_memmove(WN *block, WN *tree, WN *dst, WN *src, WN *size)
{
  if (check_size(size, src, dst))
  {
    if (CG_memmove_cannot_overlap ||
	!lower_is_aliased(src, dst, WN_const_val(size)))
    {
      if (WN *em = aux_memcpy(src, dst, size)) {
        aux_memory_msg("memmove()", tree, em);
        WN_INSERT_BlockLast(block, em);
        return WN_COPY_Tree(dst);
      }
    }
  }
  return NULL;
}

#ifdef TARG_X8664
extern  /* defined in data_layout.cxx */
ST *Get_Vararg_Save_Area_Info(int &fixed_int_parms, int &fixed_float_parms,
			      ST *&upformal);

/* va_start under the X86-64 ABI */
static WN *em_x8664_va_start(WN *block, WN *ap)
{
  TY_IDX ty_idx;
  // TY_IDX va_list_struct_ty;
  INT fixed_int_parms, fixed_float_parms;
  BOOL direct;
  BOOL non_leaf = FALSE;
  if (WN_operator(ap) == OPR_LDA) {
    ty_idx = WN_ty(ap);
    Is_True(TY_kind(ty_idx) == KIND_POINTER,
	    ("em_x8664_va_start: argument not of pointer type"));
    ty_idx = TY_pointed(ty_idx);
    direct = TRUE;
    // va_list_struct_ty = TY_etype(ty_idx);
  }
  else if (WN_operator(ap) == OPR_LDID) {
    ty_idx = WN_ty(ap);
    Is_True(TY_kind(ty_idx) == KIND_POINTER,
	    ("em_x8664_va_start: argument not of pointer type"));
    ty_idx = TY_pointed(ty_idx);
    Is_True(TY_size(ty_idx) == 24,
	("em_x8664_va_start: argument pointer does not point to type va_list"));
    direct = FALSE;
    // va_list_struct_ty = ty_idx;
  }
  else { // bug 3147
    non_leaf = TRUE;
    direct = FALSE;
  }

  ST *upformal;
  ST *reg_save_area = Get_Vararg_Save_Area_Info(fixed_int_parms, fixed_float_parms, upformal);
  WN *wn;
  WN *addr;

  wn = WN_Intconst(MTYPE_I4, fixed_int_parms * 8);
  if (direct)
    wn = WN_Stid(MTYPE_I4, WN_offset(ap), WN_st(ap), MTYPE_To_TY(MTYPE_I4), wn);
  else {
    if (! non_leaf)
      addr = WN_Ldid(Pointer_Mtype, WN_offset(ap), WN_st(ap), WN_ty(ap));
    else addr = WN_COPY_Tree(ap);
    wn = WN_Istore(MTYPE_I4, 0, Make_Pointer_Type(MTYPE_To_TY(MTYPE_I4)),
    		   addr, wn);
  }
  WN_INSERT_BlockLast(block, wn);

  wn = WN_Intconst(MTYPE_I4, fixed_float_parms * 16 + 48);
  if (direct)
    wn = WN_Stid(MTYPE_I4, 4 + WN_offset(ap), WN_st(ap), MTYPE_To_TY(MTYPE_I4), wn);
  else {
    if (! non_leaf)
      addr = WN_Ldid(Pointer_Mtype, WN_offset(ap), WN_st(ap), WN_ty(ap));
    else addr = WN_COPY_Tree(ap);
    wn = WN_Istore(MTYPE_I4, 4, Make_Pointer_Type(MTYPE_To_TY(MTYPE_I4)),
    		   addr, wn);
  }
  WN_INSERT_BlockLast(block, wn);

  wn = WN_Lda(Pointer_Mtype, STB_size(upformal), upformal);
  if (direct)
    wn = WN_Stid(Pointer_Mtype, 8 + WN_offset(ap), WN_st(ap), MTYPE_To_TY(Pointer_Mtype), wn);
  else {
    if (! non_leaf)
      addr = WN_Ldid(Pointer_Mtype, WN_offset(ap), WN_st(ap), WN_ty(ap));
    else addr = WN_COPY_Tree(ap);
    wn = WN_Istore(Pointer_Mtype, 8, 
    		   Make_Pointer_Type(MTYPE_To_TY(Pointer_Mtype)), addr, wn);
  }

  if (ST_sclass(upformal) == SCLASS_UNKNOWN)
    Set_ST_sclass (upformal, SCLASS_FORMAL);

  if (reg_save_area) {
    WN_INSERT_BlockLast(block, wn);
    if (TY_size(ST_type(reg_save_area)) == 8)
      wn = WN_Lda(Pointer_Mtype, -(fixed_int_parms * 8), reg_save_area);
    else wn = WN_Lda(Pointer_Mtype, -(fixed_float_parms*16)-48, reg_save_area);
    if (direct)
      wn = WN_Stid(Pointer_Mtype, 16 + WN_offset(ap), WN_st(ap), MTYPE_To_TY(Pointer_Mtype),wn);
    else {
      if (! non_leaf)
        addr = WN_Ldid(Pointer_Mtype, WN_offset(ap), WN_st(ap), WN_ty(ap));
      else addr = WN_COPY_Tree(ap);
      wn = WN_Istore(Pointer_Mtype, 16, 
    		     Make_Pointer_Type(MTYPE_To_TY(Pointer_Mtype)), addr, wn);
    }
  }
  return wn;
}
#endif

/* ====================================================================
 *
 * COERCE INTRN_coerce_runtime(WN *tree, INT32 arg)
 *
 * Given an intrinsic or expression that will map to an emulation
 * routine, return the action required for mapping the arguments
 * to the runtime routine
 * ==================================================================== */

static COERCE INTR_coerce_runtime(WN *tree, INT32 arg)
{
  if (OPCODE_is_intrinsic(WN_opcode(tree)))
  {
    INT32	i;
    INTRINSIC	id = (INTRINSIC) WN_intrinsic(tree);

    for(i=0; i < intrinsic_runtime_formals_size; i++)
    {
      if (id == INTR_id(i))
      {
	switch(arg)
	{
	case 0:		return INTR_coerce0(i);
	case 1:		return INTR_coerce1(i);
	default:	
	  Is_True(FALSE, ("INTR_coerce_runtime, arg >1"));
	}
      }
    }
    return COERCE_none;
  }
  else
  {
   /*
    *  the coercion should be valid for all arguments
    *  otherwise the table must change
    */
    EMULATION	id = WN_emulation(tree); 

    Is_True((EM_id(id) == id), ("em_routine table in a sorry state"));
    return EM_coerce0(id);
  }
}

/* ====================================================================
 *
 * COERCE INTRN_actual(WN *tree, INT32 arg)
 *
 * Given an intrinsic or expression that will map to an emulation
 * routine, return the arguments types the FE will supply
 * to this intrinsic/expression
 * ==================================================================== */

static TYPE_ID INTR_parameter_type(WN *tree, INT32 arg)
{
  if (OPCODE_is_intrinsic(WN_opcode(tree)))
  {
    INT32	i;
    INTRINSIC	id = (INTRINSIC) WN_intrinsic(tree);
    for(i=0; i < intrinsic_parameter_type_size; i++)
    {
      if (id == INTR_parm_id(i))
      {
	switch(arg)
	{
	case 0:		return INTR_parmtype0(i);
	case 1:		return INTR_parmtype1(i);
	case 2:		return INTR_parmtype2(i);
	default:	
	  /*
	   *  take a wild guess 
	   *  if seems that the intrinsics are uniform 
	   *	(all arguments are the same)
	   */
	  return INTR_parmtype0(i);
	}
      }
    }
    Is_True(FALSE, ("INTR_parameter_type unknown for %s", INTR_intrinsic_name(tree)));
  }
  else
  {
    Is_True(FALSE, ("INTR_parameter_type not used for call by value"));
  }
  return MTYPE_V;
}




/* ====================================================================
 *
 * char *INTR_intrinsic_name(WN *tree)
 *
 * Given an intrinsic or expression that will map to an emulation
 * routine, return the action required for mapping the arguments
 * to the runtime routine
 *
 * ==================================================================== */

extern const char * INTR_intrinsic_name(WN *tree)
{
  if (OPCODE_is_intrinsic(WN_opcode(tree)))
  {
    INTRINSIC	id = (INTRINSIC) WN_intrinsic(tree);

    return INTRN_rt_name(id);
  }
  else
  {
    EMULATION	id = WN_emulation(tree); 

    Is_True((EM_id(id) == id), ("em_routine table in a sorry state"));
    return EM_rt_name(id);
  }
}

static void
Set_intrinsic_flags (ST *st, WN *tree)
{
    PU& pu = Pu_Table[ST_pu (st)];
    if (OPCODE_is_intrinsic(WN_opcode(tree))) {
	INTRINSIC	id = (INTRINSIC) WN_intrinsic(tree);

	/**************************
	  one day
	  if (INTRN_never_returns(id))
	  flags |=	???
	  **************************/
	if (INTRN_is_pure(id))
	    Set_PU_is_pure (pu);
	if (INTRN_has_no_side_effects(id))
	    Set_PU_no_side_effects (pu);

    } else {
	EMULATION	id = WN_emulation(tree); 

	if (EM_attributes(id) & PU_IS_PURE)
	    Set_PU_is_pure (pu);
	if (EM_attributes(id) & NSE)
	    Set_PU_no_side_effects (pu);
    }
}






/* ====================================================================
 *
 * WN * by_value (WN *tree, INT32 arg)
 *
 * Given an intrinsic that will map to an emulation
 * routine, return a by value argument.
 * ==================================================================== */

static WN *by_value(WN *tree, INT32 arg)
{
  TYPE_ID	type;
  INTRINSIC     id =	(INTRINSIC) WN_intrinsic(tree);
  WN		*child= WN_arg(tree, arg);

  Is_True((OPCODE_is_intrinsic(WN_opcode(tree))),
	  ("expected intrinsic call node, not %s", OPCODE_name(WN_opcode(tree))));

  if (INTRN_by_value(id)==TRUE)
  {
    return child;
  }

  type = INTR_parameter_type(tree, arg);

  Is_True((type != MTYPE_V), ("unexpected void type"));

  if (WN_operator_is(child, OPR_LDA))
  {
    return WN_Ldid(type,
		   WN_lda_offset(child),
		   WN_st(child),
		   MTYPE_To_TY(type));
  }
  return WN_Iload(type, 0, MTYPE_To_TY(type), child);
}




/* ====================================================================
 *
 * WN *return_conversion(INTRINSIC id, TYPE_ID rtype, WN *function)
 *
 * Coerce the return type to match the INTRINSIC return.
 * Necessary for correctness  
 *
 *	ex.   I1SHFT(125, 1)
 *			0d127 << 1 
 *	should return a negative value (-2)
 * ==================================================================== */

static WN *return_conversion(INTRINSIC id, TYPE_ID type, WN *function)
{
  if (MTYPE_is_integral(type))
  {
    TYPE_ID  return_type = INTR_return_mtype(id);

    if (MTYPE_is_integral(return_type))
    {
      if (return_type != type)
      {
	return WN_Type_Conversion(function, return_type);
      }
      else if (WN_rtype(function) != type)
      {
	DevWarn("Unexpected return_conversion() while processing intrinsic %s",
		INTRN_rt_name(id));
	return WN_Type_Conversion(function, return_type);
      }
    }
  }
  return function;
}



/* ====================================================================
 *
 * WN *make_pointer_to_node(WN *block, WN *tree)
 *
 * Return the address of tree.
 *	for ILOAD/ISTORE we can use the address directly
 *	for LDID/STID we can create an LDA
 *	otherwise we need to create an addressable temp, store to it
 *	and try again.
 * ==================================================================== */

extern WN *make_pointer_to_node(WN *block, WN *tree)
{
  switch (WN_operator(tree))
  {
  case OPR_ILOAD:
    return WN_kid0(tree);

  case OPR_ISTORE:
    return WN_kid1(tree);

  case OPR_LDID:
    if (WN_class(tree) == CLASS_PREG)
	break;
    return WN_Lda(Pointer_type, WN_load_offset(tree), WN_st(tree));

  case OPR_STID:
    return WN_Lda(Pointer_type, WN_store_offset(tree), WN_st(tree));

  case OPR_ARRAY:
  case OPR_LDA:
    return tree;
  }
  {
      TYPE_ID	type = WN_rtype(tree);
      ST  *st = Gen_Temp_Symbol( MTYPE_To_TY(type), "complex-temp-expr");
      WN  *stid;

      Is_True((WN_operator_is(tree, OPR_PARM)==FALSE),("bad parm"));
      /*
       *  store value to an addressible temporary, and take the address of that
       */
      stid = WN_Stid (type, 0, st, ST_type(st), tree);
      WN_INSERT_BlockLast(block, stid);

      return WN_Lda(Pointer_type, WN_store_offset(stid), st);
  }
}

/* ====================================================================
 *
 *  ST *GenLocalTable(WN *block, TYPE_ID element, INT32 n, WN **init)
 *
 *  Create a local array of TYPE_ID elements (size n),
 *  and initialize them to init.
 *
 * ==================================================================== */

static ST *GenLocalTable(WN *block, TYPE_ID element, INT32 n, WN **init)
{
  INT32	i;
  TY_IDX arrayTY;
  ST	*arrayST;

  arrayTY = Make_Array_Type (element, 1, n);
  {
    static INT32 table_cnt = 0;
    char   buffer[32];
    sprintf(buffer, "localtable.%d", ++table_cnt);
    
    arrayST = Gen_Temp_Symbol(arrayTY, buffer);
  }

  for(i=0; i<n; i++)
  {
    WN	*st;

    st = WN_Stid(element,
		 i * MTYPE_RegisterSize(element),
		 arrayST,
		 arrayTY,
		 init[i]);
    WN_INSERT_BlockLast(block, st);
  }

  return arrayST;
}

/* ====================================================================
 *
 * concatexpr (really s_cat) has to be interpreted, as the arguments
 * do not match
 *
 *	CONCATEXPR(	char	*dst,	  int dstSize,
 *			char	*src0,	  char *src1,   ... char *srcN,
 *			int	src0Size, int src1Size, ... int srcNSize)
 *	vrs.
 *	
 *	s_cat(		char	*dst,
 *			char	*src[],			vector of chars
 *			int	srcSize[],		vector of sizes
 *			int	*srcN,			number of vectors
 *			int	dstSize)
 *
 * ==================================================================== */
static WN *process_concatexpr(WN *block, WN *tree)
{
  INT16		nsrc;
  WN		**wn;
  WN		*srcAddr, *srcSize, *srcNp;

  nsrc = (WN_kid_count(tree)-2) / 2;

  wn = (WN **) alloca(nsrc * sizeof(WN));
  {
    /*
     *	create a pointer to a table of addresses (src[])
     */
    ST		*srcST;
    INT32	i;

    for(i=0; i<nsrc; i++)
      wn[i] = WN_arg(tree, i+2);

    srcST = GenLocalTable(block, Pointer_type, nsrc, wn);
    srcAddr = WN_Lda(Pointer_type, 0, srcST);
  }
  {
    /*
     *	create a pointer to a table of sizes (srcSize[])
     */
    ST		*sizeST;
    INT32	i;

    for(i=0; i<nsrc; i++)
      wn[i] = WN_arg(tree, i+2+nsrc);

    sizeST = GenLocalTable(block, Integer_type, nsrc, wn);
    srcSize = WN_Lda(Pointer_type, 0, sizeST);
  }
  {
    WN	*count;

    count = WN_Intconst(Integer_type, nsrc);
    /* pointer to srcN	*/
    srcNp = make_pointer_to_node(block, count);	
  }

  

 /*
  *  s_cat will always have 5 arguments
  */
  {
    WN		*kids[5];
    WN		*s_cat;
    INTRINSIC	id = (INTRINSIC) WN_intrinsic(tree);
    BOOL        byvalue = INTRN_by_value(id);

    kids[0] = WN_kid(tree, 0);
    kids[1] = createParm( srcAddr, byvalue );
    kids[2] = createParm( srcSize, byvalue );
    kids[3] = createParm( srcNp, byvalue );
    kids[4] = WN_kid(tree, 1);

    s_cat = WN_Create_Intrinsic(WN_opcode(tree), id, 5, kids);
    return s_cat;
  }
}

/* ====================================================================
 *
 * Annotate_Weak_Runtime
 *
 * Check the runtime routine named against a list of those which need
 * to be weak.  At this time, this list includes:
 *
 *  __C_runtime_error:  A runtime routine for reporting errors (e.g.
 *	subscript range violations, divide by zero).  This is added to
 *	libc with Irix 6.5, and is invoked only if compilations are
 *	done with -DEBUG:verbose_runtime.  If such programs are run on
 *	older Irix versions, the weak symbol will resolve to zero and
 *	the error will result in a SIGSEGV instead.
 *
 * ====================================================================
 */

static const char *Weak_Runtimes[] = {
  "__C_runtime_error",

  NULL		/* List must be null-terminated */
};

static void
Annotate_Weak_Runtime ( ST *func, const char *name )
{
  const char **weak_rt = Weak_Runtimes;

  while ( *weak_rt != NULL ) {
    if ( strcmp ( *weak_rt, name ) == 0 ) {
      Set_ST_is_weak_symbol ( func );
      return;
    }
    ++weak_rt;
  }
}

/* ====================================================================
 *
 * WN *intrinsic_runtime(WN *block, WN *tree)
 *
 * Lower tree into a call (there is no going back now)
 * The tree may be an expression or more likely an intrinsic
 *
 * ==================================================================== */

extern WN *intrinsic_runtime(WN *block, WN *tree)
{
  INT16	n;
  INT16	argC = 0;
  WN	*args[MAX_INTRINSIC_ARGS];
  const char	*function = INTR_intrinsic_name(tree);
  BOOL   byvalue = FALSE;
  BOOL   parmMod= FALSE;

  Is_True((function), ("cannot emulate (null function)"));

  if (OPCODE_is_intrinsic(WN_opcode(tree)))
  {
    switch(WN_intrinsic(tree))
    {
    case INTRN_CONCATEXPR:
      tree = process_concatexpr(block, tree);
      break;
    }

    byvalue = INTRN_by_value(WN_intrinsic(tree));
  }

  {
    /*
     *  for complex quad we must create a dummy argument
     *  that has the address of a compiler temp
     *  This is known in the FE as RSTYLE_VIA_FIRST_ARG
     *  The parameter is by reference
     */
#ifdef TARG_X8664
    if (WN_rtype(tree) == MTYPE_V16C8)
      WN_set_rtype(tree, MTYPE_C8);
#endif
    TYPE_ID	rtype = WN_rtype(tree);

    switch(rtype)
    {
#ifdef TARG_X8664
    case MTYPE_C4:
    case MTYPE_C8:
      if (Is_Target_64bit())
	break;
      // fall thru
#endif
    case MTYPE_CQ:
      {
	ST  *retST = Gen_Temp_Symbol(MTYPE_To_TY(rtype), "return_temp");

	Set_BE_ST_addr_used_locally(retST);

	args[ argC++] = createParm(WN_Lda(Pointer_type, 0, retST),  FALSE);
	parmMod=	TRUE;
      }
      break;
    default:
      break;
    }
  }

  for(n = 0; n < WN_num_actuals(tree); n++)
  {
    WN		*actual = WN_kid(tree, n);
    COERCE	coerce = INTR_coerce_runtime(tree, n);

    switch(coerce)
    {
    case COERCE_none:
      args[ argC++] = actual;
      break;

    case COERCE_struct_by_value:
      {
	TYPE_ID	type = WN_rtype(actual);
	TY_IDX complexTY;
	WN		*mload;
	INT32	size = MTYPE_size_min(type) >> 3;

       /*
	*  create a structure type that looks like complex, but has
	*  btype of M, as lower call expects this type
	*/
	TY& ty = New_TY (complexTY);
	ty = Ty_Table[MTYPE_To_TY(type)];
	Set_TY_mtype (ty, MTYPE_M);
	Set_TY_align_exp (complexTY, 3);

	TY_IDX complex_ptr = Make_Pointer_Type(complexTY);

        if (WN_operator_is(actual, OPR_PARM))
	  actual = WN_kid0(actual);

	mload = WN_CreateMload(0, complex_ptr, 
			       make_pointer_to_node(block, actual),
			       WN_Intconst(Integer_type, size));
	args[ argC++] = mload;
      }

    case COERCE_struct_by_reference:
      break;

    case COERCE_split_complex:
      {
	TYPE_ID	type = WN_rtype(actual);
	TYPE_ID	rtype = Mtype_complex_to_real(type);
	PREG_NUM	valN;

        if (WN_operator_is(actual, OPR_PARM))
	  actual = WN_kid0(actual);

	valN = AssignExpr(block, actual, type);

	args[ argC++] = WN_Realpart(rtype, WN_LdidPreg(type, valN));
	args[ argC++] = WN_Imagpart(rtype, WN_LdidPreg(type, valN));
      }
      break;

    default:
      Fail_FmtAssertion("coerce case does not exist coerce = %d", coerce);
    }
  }

  {
    TYPE_ID	rtype =  WN_rtype(tree);
    TY_IDX ty = Make_Function_Type( MTYPE_To_TY(rtype));

#ifdef TARG_X8664
    // The return type is set up correctly in Make_Function_Type (above).
    // (-m32 expects that the return type be setup correctly).
    // Currently, there is no need for setting up the parameter list 
    // for the intrinsics before setting TY_has_prototype. 
    // lower_call for x86/x8664 expects every function to have a prototype 
    // and assumes a vararg function if there is no prorotype.
    Set_TY_has_prototype(ty);

    ST* st = NULL;

    /* Do the following conversion if either of -ffast-math or -OPT:fast_math
       are specified.
       
       acos -> fastacos, acosf -> fastacosf
       asin -> fastasin, asinf -> fastasinf
       atan -> fastatan, atanf -> fastatanf
       atan2 -> fastatan2, atan2f -> fastatan2f
       cos -> fastcos, cosf -> fastcosf
       cosh -> fastcosh, coshf -> fastcoshf
       exp -> fastexp, expf -> fastexpf
       log -> fastlog, logf -> fastlogf
       pow -> fastpow, powf -> fastpowf
       sin -> fastsin, sinf -> fastsinf
       sinh -> fastsinh, sinhf -> fastsinhf
       sincos -> fastsincos, sincosf -> fastsincosf
       tan -> fasttan, tanf -> fasttanf
       tanh -> fasttanh, tanhf -> fasttanhf

    */
    if( Is_Target_64bit() &&
	!Is_Target_Anyx86() &&
	OPT_Fast_Math &&
	( WN_intrinsic(tree) == INTRN_F8ACOS ||
	  WN_intrinsic(tree) == INTRN_F4ACOS ||
	  WN_intrinsic(tree) == INTRN_F8ASIN ||
	  WN_intrinsic(tree) == INTRN_F4ASIN ||
	  WN_intrinsic(tree) == INTRN_F8ATAN ||
	  WN_intrinsic(tree) == INTRN_F4ATAN ||
	  WN_intrinsic(tree) == INTRN_F8ATAN2 ||
	  WN_intrinsic(tree) == INTRN_F4ATAN2 ||
	  WN_intrinsic(tree) == INTRN_F8COS ||
	  WN_intrinsic(tree) == INTRN_F4COS ||
	  WN_intrinsic(tree) == INTRN_F8COSH ||
	  WN_intrinsic(tree) == INTRN_F4COSH ||
	  WN_intrinsic(tree) == INTRN_F8EXP ||
	  WN_intrinsic(tree) == INTRN_F4EXP ||
	  WN_intrinsic(tree) == INTRN_F8LOG ||
	  WN_intrinsic(tree) == INTRN_F4LOG ||
	  WN_intrinsic(tree) == INTRN_F8EXPEXPR ||
	  WN_intrinsic(tree) == INTRN_F4EXPEXPR ||
	  WN_intrinsic(tree) == INTRN_F8SIN ||
	  WN_intrinsic(tree) == INTRN_F4SIN ||
	  WN_intrinsic(tree) == INTRN_F8SINH ||
	  WN_intrinsic(tree) == INTRN_F4SINH ||
	  WN_intrinsic(tree) == INTRN_SINCOS ||
	  WN_intrinsic(tree) == INTRN_SINCOSF ||
	  WN_intrinsic(tree) == INTRN_F8TAN ||
	  WN_intrinsic(tree) == INTRN_F4TAN ||
	  WN_intrinsic(tree) == INTRN_F8TANH ||
	  WN_intrinsic(tree) == INTRN_F4TANH ||
	  WN_intrinsic(tree) == INTRN_F8VSIN ||
	  WN_intrinsic(tree) == INTRN_F8VCOS ||
	  WN_intrinsic(tree) == INTRN_F8VEXP ||
	  WN_intrinsic(tree) == INTRN_F4VEXP ||
	  WN_intrinsic(tree) == INTRN_F4VLOG ||
	  WN_intrinsic(tree) == INTRN_F8VLOG ) ) {
      BOOL vector_call_check_constant_stride = FALSE;
      switch (WN_intrinsic(tree)) {
      case INTRN_F8ACOS:   st = Gen_Intrinsic_Function(ty, "fastacos"); break;
      case INTRN_F4ACOS:   st = Gen_Intrinsic_Function(ty, "fastacosf"); break;
      case INTRN_F8ASIN:   st = Gen_Intrinsic_Function(ty, "fastasin"); break;
      case INTRN_F4ASIN:   st = Gen_Intrinsic_Function(ty, "fastasinf"); break;
      case INTRN_F8ATAN:   st = Gen_Intrinsic_Function(ty, "fastatan"); break;
      case INTRN_F4ATAN:   st = Gen_Intrinsic_Function(ty, "fastatanf"); break;
      case INTRN_F8ATAN2:  st = Gen_Intrinsic_Function(ty, "fastatan2"); break;
      case INTRN_F4ATAN2:  st = Gen_Intrinsic_Function(ty, "fastatan2f"); break;
      case INTRN_F8COS:    st = Gen_Intrinsic_Function(ty, "fastcos"); break;
      case INTRN_F4COS:    st = Gen_Intrinsic_Function(ty, "fastcosf"); break;
      case INTRN_F8COSH:   st = Gen_Intrinsic_Function(ty, "fastcosh"); break;
      case INTRN_F4COSH:   st = Gen_Intrinsic_Function(ty, "fastcoshf"); break;
      case INTRN_F8EXP:    st = Gen_Intrinsic_Function(ty, "fastexp"); break;
      case INTRN_F4EXP:    st = Gen_Intrinsic_Function(ty, "fastexpf"); break;
      case INTRN_F8LOG:    st = Gen_Intrinsic_Function(ty, "fastlog"); break;
      case INTRN_F4LOG:    st = Gen_Intrinsic_Function(ty, "fastlogf"); break;
      case INTRN_F8EXPEXPR:st = Gen_Intrinsic_Function(ty, "fastpow"); break;
      case INTRN_F4EXPEXPR:st = Gen_Intrinsic_Function(ty, "fastpowf"); break;
      case INTRN_F8SIN:    st = Gen_Intrinsic_Function(ty, "fastsin"); break;
      case INTRN_F4SIN:    st = Gen_Intrinsic_Function(ty, "fastsinf"); break;
      case INTRN_F8SINH:   st = Gen_Intrinsic_Function(ty, "fastsinh"); break;
      case INTRN_F4SINH:   st = Gen_Intrinsic_Function(ty, "fastsinhf"); break;
      case INTRN_SINCOS:   st = Gen_Intrinsic_Function(ty, "fastsincos");break;
      case INTRN_SINCOSF:  st = Gen_Intrinsic_Function(ty, "fastsincosf");break;
      case INTRN_F8TAN:    st = Gen_Intrinsic_Function(ty, "fasttan"); break;
      case INTRN_F4TAN:    st = Gen_Intrinsic_Function(ty, "fasttanf"); break;
      case INTRN_F8TANH:   st = Gen_Intrinsic_Function(ty, "fasttanh"); break;
      case INTRN_F4TANH:   st = Gen_Intrinsic_Function(ty, "fasttanhf"); break;

      case INTRN_F8VSIN: 
	st = Gen_Intrinsic_Function(ty, "vrda_sin");	
	vector_call_check_constant_stride = TRUE;
	break;
      case INTRN_F8VCOS: 
	st = Gen_Intrinsic_Function(ty, "vrda_cos");
	vector_call_check_constant_stride = TRUE;
	break;
      case INTRN_F8VEXP: 
	st = Gen_Intrinsic_Function(ty, "vrda_exp");
	vector_call_check_constant_stride = TRUE;
	break;
      case INTRN_F4VEXP: 
	st = Gen_Intrinsic_Function(ty, "vrsa_expf");
	vector_call_check_constant_stride = TRUE;
	break;
      case INTRN_F8VLOG: 
	st = Gen_Intrinsic_Function(ty, "vrda_log");
	vector_call_check_constant_stride = TRUE;
	break;
      case INTRN_F4VLOG: 
	st = Gen_Intrinsic_Function(ty, "vrsa_logf");
	vector_call_check_constant_stride = TRUE;
	break;
      }

      if ( vector_call_check_constant_stride ) {
	WN* x = WN_kid(tree, 0); // opnd
	WN* y = WN_kid(tree, 1); // result
	WN* count =   WN_kid(tree, 2);
	WN* stridex = WN_kid(tree, 3);
	WN* stridey = WN_kid(tree, 4);
	if ( WN_operator(WN_kid0(stridex)) != OPR_INTCONST ||
	     WN_operator(WN_kid0(stridey)) != OPR_INTCONST ||
	     WN_const_val(WN_kid0(stridex)) != 1 ||
	     WN_const_val(WN_kid0(stridey)) != 1 )
	  st = Gen_Intrinsic_Function(ty, function);
	else {	  
	  args[ 0 ] = count;
	  args[ 1 ] = x;
	  args[ 2 ] = y;
	  argC = 3;
	}
      }
    }

#if defined(TARG_X8664)
    // Rename memcpy to the amd optimized memcpy.
    else if (WN_intrinsic(tree) == INTRN_MEMCPY &&
	       OPT_Fast_Stdlib &&
	       Is_Target_64bit()) {
      if (Is_Target_Barcelona() || Is_Target_Orochi()) {
        WN *child = WN_arg(tree, 2);
        if (Is_Integer_Constant(child)) {
          int memcpy_len = WN_const_val(child);
          if (((memcpy_len % 32) == 0) || 
              ((memcpy_len % 64) == 0)) {
            int exact = ((memcpy_len % 64) == 0);
            int strideval = memcpy_len / 64; 
            if ((memcpy_len <= 256) && (memcpy_len >= 96)) {
              WN *exact_arg = WN_CreateIntconst (OPC_I4INTCONST, exact);
              args[ argC++] = exact_arg;
              WN_const_val(child) = strideval;
	      st = Gen_Intrinsic_Function(ty, "__fastcopy_stride64_gp");
            } else if (memcpy_len > 256) {
              WN *exact_arg = WN_CreateIntconst (OPC_I4INTCONST, exact);
              args[ argC++] = exact_arg;
              WN_const_val(child) = strideval;
	      st = Gen_Intrinsic_Function(ty, "__fastcopy_stride64_gh");
            } else {
              /* if we do not match we default to the non intrinsic form */ 
              st = Gen_Intrinsic_Function(ty, function);
            }
          } else {
            /* if we do not match we default to the non intrinsic form */ 
            st = Gen_Intrinsic_Function(ty, function);
          }
        } else {
          /* if we do not match we default to the non intrinsic form */ 
          st = Gen_Intrinsic_Function(ty, function);
        }
      } else {
        st = Gen_Intrinsic_Function(ty, function);
      }
    }
#endif
#if defined(TARG_X8664) && !defined(OSP_OPT)
    // Rename memset to the PathScale optimized memset.
    else if (WN_intrinsic(tree) == INTRN_MEMSET &&
	       OPT_Fast_Stdlib &&
	       Is_Target_64bit()) {
      if (Is_Target_EM64T() || Is_Target_Core() || Is_Target_Wolfdale())
	st = Gen_Intrinsic_Function(ty, "memset.pathscale.em64t");
      else
	st = Gen_Intrinsic_Function(ty, "memset.pathscale.opteron");

    // Rename memcpy to the PathScale optimized memcpy.
    } else if (WN_intrinsic(tree) == INTRN_MEMCPY &&
	       OPT_Fast_Stdlib &&
	       Is_Target_64bit()) {
      if (Is_Target_EM64T() || Is_Target_Core() || Is_Target_Wolfdale())
	st = Gen_Intrinsic_Function(ty, "__memcpy_pathscale_em64t");
      else
	st = Gen_Intrinsic_Function(ty, "__memcpy_pathscale_opteron");
    }
#endif
    else if (WN_intrinsic(tree) == INTRN_POPCOUNT &&
    	       MTYPE_byte_size(WN_rtype(WN_kid0(tree))) <= 4 &&
               Is_Target_32bit()) {
      st = Gen_Intrinsic_Function(ty, "__popcountsi2");
#if defined(TARG_X8664) && !defined(OSP_OPT)

    } else if (WN_intrinsic(tree) == INTRN_PARITY &&
    	       MTYPE_byte_size(WN_rtype(WN_kid0(tree))) <= 4 &&
               Is_Target_32bit()) {
      st = Gen_Intrinsic_Function(ty, "__paritysi2");
#endif
    } else {
      st = Gen_Intrinsic_Function(ty, function);
    }
#elif defined(TARG_MIPS) && !defined(TARG_SL)

    if (WN_intrinsic(tree) == INTRN_POPCOUNT &&
	MTYPE_byte_size(WN_rtype(WN_kid0(tree))) <= 4) {
      // Zero extend U4 to U8
      // args[0] = WN_Cvt(MTYPE_U4, MTYPE_U8, args[0]);
      // Using __popcountsi2 fails at link-time on cross-compiler.
      WN *wn_cvt = WN_Cvt( MTYPE_U4, MTYPE_U8, WN_kid0( args[0] ) );
      args[0] = WN_CreateParm( MTYPE_U8, wn_cvt,
			       MTYPE_To_TY( MTYPE_U8 ), WN_PARM_BY_VALUE );
    }
    ST *st = Gen_Intrinsic_Function(ty, function);

#else
    ST	*st = Gen_Intrinsic_Function(ty, function);
#endif // KEY

    WN	*call;

   /*
    *  annotate st flags with NO_SIDE_EFFECTS, IS_PURE etc.
    */
    Set_intrinsic_flags (st, tree);
    Annotate_Weak_Runtime ( st, function );

#if defined(TARG_X8664)
    if (! Is_Target_64bit()) { // leave any complex type as is
      call = WN_Create(OPR_CALL, rtype, MTYPE_V, argC);
      WN_st_idx(call) = ST_st_idx(st);
    }
    else
#endif
    call = WN_Call(rtype, MTYPE_V, argC, st);
    WN_call_flag(call) = WN_call_flag(tree);

    WN_annotate_call_flags(call, st);
   /*
    *  The annotations on CQ intrinsics are not correct.
    *  They do modify their parameter
    */
    if (parmMod)
      WN_Set_Call_Parm_Mod(call);

    while (--argC >= 0)
    {
      WN_actual(call, argC) = createParm(args[ argC], byvalue);
    }

    // Update feedback
    if ( Cur_PU_Feedback )
      Cur_PU_Feedback->FB_set_in_out_same_node( call );

    return call;
  }
}

/* ====================================================================
 *
 *
 *
 * ==================================================================== */

static WN *emulate_intrinsic_op(WN *block, WN *tree)
{
  INTRINSIC     id = (INTRINSIC) WN_intrinsic(tree);
  TYPE_ID	rtype = WN_rtype(tree);
  WN		*function;

  Is_True((OPCODE_is_intrinsic(WN_opcode(tree))),
	  ("expected intrinsic call node, not %s", OPCODE_name(WN_opcode(tree))));
  Is_True((INTRN_is_actual(WN_intrinsic(tree))==FALSE),
	 ("cannot emulate INTRN_is_actual"));

  switch(id) {
  case INTRN_I4EXPEXPR:
  case INTRN_I8EXPEXPR:
   /*
    *	do these regardless of flags, as they are always safe
    */
    return em_exp_int(block, by_value(tree, 0), by_value(tree,1), rtype);

  case INTRN_F4I4EXPEXPR:
  case INTRN_F4I8EXPEXPR:
  case INTRN_F8I4EXPEXPR:
  case INTRN_F8I8EXPEXPR:
  case INTRN_FQI4EXPEXPR:
  case INTRN_FQI8EXPEXPR:
  case INTRN_C4I4EXPEXPR:
  case INTRN_C4I8EXPEXPR:
  case INTRN_C8I4EXPEXPR:
  case INTRN_C8I8EXPEXPR:
  case INTRN_CQI4EXPEXPR:
  case INTRN_CQI8EXPEXPR:
   /*
    *   The consensus is we allow constants (-1, 0, 1, 2) as
    *   always safe , regardless of the Fast_Exp_Allowed 
    */
    return em_exp_int(block, by_value(tree, 0), by_value(tree,1), rtype);

  case INTRN_F4EXPEXPR:
  case INTRN_F8EXPEXPR:
  case INTRN_FQEXPEXPR:
  case INTRN_C4EXPEXPR:
  case INTRN_C8EXPEXPR:
  case INTRN_CQEXPEXPR:
    if (Fast_Exp_Allowed)
      return em_exp_float(block, by_value(tree, 0), by_value(tree, 1), rtype);
    break;

  case INTRN_F4MOD:
  case INTRN_F8MOD:
  case INTRN_FQMOD:
    return em_mod_float(block, by_value(tree, 0), by_value(tree, 1));

  case INTRN_I1DIM:
  case INTRN_I2DIM:
    function=  em_dim(block, by_value(tree, 0), by_value(tree, 1));
    return return_conversion(id, rtype, function);

  case INTRN_I4DIM:
  case INTRN_I8DIM:
  case INTRN_F4DIM:
  case INTRN_F8DIM:
  case INTRN_FQDIM:
    return em_dim(block, by_value(tree, 0), by_value(tree, 1));

  case INTRN_F8F4PROD:
  case INTRN_FQF8PROD:
    return em_prod(block, rtype, by_value(tree, 0), by_value(tree, 1));

  case INTRN_I1SIGN:
  case INTRN_I2SIGN:
    function=  em_sign(block, by_value(tree, 0), by_value(tree, 1));
    return return_conversion(id, rtype, function);

  case INTRN_I4SIGN:
  case INTRN_I8SIGN:
  case INTRN_F4SIGN:
  case INTRN_F8SIGN:
  case INTRN_FQSIGN:
    return em_sign(block, by_value(tree, 0), by_value(tree, 1));

  case INTRN_F4AINT:
  case INTRN_F8AINT:
  case INTRN_FQAINT:
    return em_aint(block, rtype, by_value(tree, 0));

  case INTRN_I2F4NINT:
  case INTRN_I4F4NINT:
  case INTRN_I8F4NINT:
  case INTRN_I2F8IDNINT:
  case INTRN_I4F8IDNINT:
  case INTRN_I8F8IDNINT:
  case INTRN_I2FQIQNINT:
  case INTRN_I4FQIQNINT:
  case INTRN_I8FQIQNINT:
    return em_nearest_int(block, rtype, by_value(tree, 0));

  case INTRN_F4ANINT:
  case INTRN_F8ANINT:
  case INTRN_FQANINT:
    return em_nearest_aint(block, rtype, by_value(tree, 0));

  case INTRN_I4CLEN:
    return em_clen(block, by_value(tree, 1));

  case INTRN_U4I4ALLOCA:
  case INTRN_U8I8ALLOCA:
    return em_alloca(block, tree);

  case INTRN_U4READSTACKPOINTER:
  case INTRN_U8READSTACKPOINTER:
    return em_readstackpointer(Pointer_type);

#ifdef KEY
  case INTRN_U4READFRAMEPOINTER:
  case INTRN_U8READFRAMEPOINTER:
    return em_readframepointer(Pointer_type);
#endif

  case INTRN_U4I4SETSTACKPOINTER:
  case INTRN_U8I8SETSTACKPOINTER:
    return em_setstackpointer(block, Pointer_type, by_value(tree, 0));
 
  case INTRN_C4CONJG:
  case INTRN_C8CONJG:
  case INTRN_CQCONJG:
    return em_conjg(block, by_value(tree, 0));


  /*
  **	Generic problem with bit routines 
  **	They are all call by reference !!
  */
  case INTRN_I1BCLR:
  case INTRN_I2BCLR:
  case INTRN_I4BCLR:
  case INTRN_I8BCLR:
    /*
     *	clear bit i (n, i)
     *	 (0 <= i && i < NUMBERBITS):		n & ~(1<<i)
     *   else					0
     */
    return em_bclr(block, by_value(tree, 0), by_value(tree, 1));

  case INTRN_I1BSET:
  case INTRN_I2BSET:
  case INTRN_I4BSET:
  case INTRN_I8BSET:
    /*
     *	set bit i (n, i)
     *	 (0 <= i && i < NUMBERBITS):		n | (1<<i)
     *   else					0
     */
    function=	em_bset(block, by_value(tree, 0), by_value(tree, 1));
    return return_conversion(id, rtype, function);


  case INTRN_I1BTEST:
  case INTRN_I2BTEST:
  case INTRN_I4BTEST:
  case INTRN_I8BTEST:
    /*
     *	test bit i (n, i)
     *	 (0 <= i && i < NUMBERBITS):		(n >> i) & 0x1
     *   else					0
     */
    return em_btest(block, by_value(tree, 0), by_value(tree, 1));

  case INTRN_I1BITS:
  case INTRN_I2BITS:
  case INTRN_I4BITS:
  case INTRN_I8BITS:
    /*
     *	extract bits [i ... i+len-1] (n, i, len)
     *
     *	 (0 <= i   && i   <  NUMBERBITS)	&&
     *	 (0 <= len && len <= NUMBERBITS)	&&
     *	 ((i+len) <= NUMBERBITS)		(n>>i) & (1<<len -1)
     *   else					n
     */
    return em_bits(block, by_value(tree, 0), by_value(tree, 1), by_value(tree, 2));


  case INTRN_I1SHL:
  case INTRN_I2SHL:
    /*
     *	shift n << i
     *
     *	 |i| < NUMBERBITS
     *		(n<<i)
     *   else	0
     */
    function=	em_shl(block, by_value(tree, 0), by_value(tree, 1));
    return return_conversion(id, rtype, function);

  case INTRN_I1SHR:
  case INTRN_I2SHR:
    function=	em_lshr(block, by_value(tree, 0), by_value(tree, 1));
    return return_conversion(id, rtype, function);

  case INTRN_I1SHFT:
  case INTRN_I2SHFT:
  case INTRN_I4SHFT:
  case INTRN_I8SHFT:
    /*
     *	shift n i places
     *
     *	 |i| < NUMBERBITS
     *		i>=0	(n<<i)
     *		i<0	(n>>(-i)) & ( (1<<(NUMBERBITS-i)) - 1)
     *   else	0
     */
    function=	em_shft(block, by_value(tree, 0), by_value(tree, 1));
    return return_conversion(id, rtype, function);

  case INTRN_I1SHFTC:
  case INTRN_I2SHFTC:
  case INTRN_I4SHFTC:
  case INTRN_I8SHFTC:
    function=	em_shftc(block, by_value(tree, 0), by_value(tree, 1), by_value(tree, 2));
    return return_conversion(id, rtype, function);

  case INTRN_I4POPPAR:
  case INTRN_I8POPPAR:
  case INTRN_PARITY:
    function=	em_parity(block, by_value(tree, 0));
    return return_conversion(id, rtype, function);

  case INTRN_I1POPCNT:
  case INTRN_I2POPCNT:
  case INTRN_I4POPCNT:
  case INTRN_I8POPCNT:
  case INTRN_POPCOUNT:
    {
      INT bitsize = MTYPE_size_reg(WN_rtype(by_value(tree, 0)));
      switch (id) {
      case INTRN_I1POPCNT:  bitsize = 8;   break;
      case INTRN_I2POPCNT:  bitsize = 16;  break;
      case INTRN_I4POPCNT:  bitsize = 32;  break;
      case INTRN_I8POPCNT:  bitsize = 64;  break;
      }
      function = em_popcount(block, by_value(tree, 0), bitsize);
    }
    return return_conversion(id, rtype, function);

  case INTRN_CLGE:
  case INTRN_CLGT:
  case INTRN_CLLE:
  case INTRN_CLLT:
  case INTRN_CEQEXPR:
  case INTRN_CNEEXPR:
  case INTRN_CGEEXPR:
  case INTRN_CGTEXPR:
  case INTRN_CLEEXPR:
  case INTRN_CLTEXPR:
    /*
     *	interpreted bt lower_intrinsic_op()
     */
    break;

  case INTRN_SUBSTRINGEXPR:
  case INTRN_CONCATEXPR:
  case INTRN_CASSIGNSTMT:

  case INTRN_F4EXP:
  case INTRN_F8EXP:
  case INTRN_FQEXP:
    break;

  case INTRN_C4EXP:
  case INTRN_C8EXP:
  case INTRN_CQEXP:
    /*
     *	real =  e**(rz) * cos(iz);
     *	imag =  e**(rz) * sin(iz);
     */
    return em_complex_exp(block, by_value(tree, 0));

  case INTRN_F4LOG:
  case INTRN_F8LOG:
  case INTRN_FQLOG:
    break;

  case INTRN_C4LOG:
  case INTRN_C8LOG:
  case INTRN_CQLOG:
    /*
     *	real =	log ( sqrt(rz**2 + iz**2) )
     *	imag =	fatan2(iz, rz)
     */
    return em_complex_log(block, by_value(tree, 0));

  case INTRN_F4LOG10:
  case INTRN_F8LOG10:
  case INTRN_FQLOG10:
    /*
     *	log(x) * (M_LOG10E = 0.43429448190325182765)
     */
    return em_alog10(block, by_value(tree, 0));

  case INTRN_F4COS:
  case INTRN_F8COS:
  case INTRN_FQCOS:
  case INTRN_F4SIN:
  case INTRN_F8SIN:
  case INTRN_FQSIN:
  case INTRN_F4TAN:
  case INTRN_F8TAN:
  case INTRN_FQTAN:
  case INTRN_F4COSD:
  case INTRN_F8COSD:
  case INTRN_FQCOSD:
  case INTRN_F4SIND:
  case INTRN_F8SIND:
  case INTRN_FQSIND:
  case INTRN_F4TAND:
  case INTRN_F8TAND:
  case INTRN_FQTAND:
  case INTRN_F4COSH:
  case INTRN_F8COSH:
  case INTRN_FQCOSH:
  case INTRN_F4SINH:
  case INTRN_F8SINH:
  case INTRN_FQSINH:
  case INTRN_F4TANH:
  case INTRN_F8TANH:
  case INTRN_FQTANH:
  case INTRN_F4ACOS:
  case INTRN_F8ACOS:
  case INTRN_FQACOS:
  case INTRN_F4ASIN:
  case INTRN_F8ASIN:
  case INTRN_FQASIN:
  case INTRN_F4ATAN:
  case INTRN_F8ATAN:
  case INTRN_FQATAN:
  case INTRN_F4ACOSD:
  case INTRN_F8ACOSD:
  case INTRN_FQACOSD:
  case INTRN_F4ASIND:
  case INTRN_F8ASIND:
  case INTRN_FQASIND:
  case INTRN_F4ATAND:
  case INTRN_F8ATAND:
  case INTRN_FQATAND:
  case INTRN_F4ATAN2:
  case INTRN_F8ATAN2:
  case INTRN_FQATAN2:
  case INTRN_F4ATAN2D:
  case INTRN_F8ATAN2D:
  case INTRN_FQATAN2D:
    break;

  case INTRN_C4COS:
  case INTRN_C8COS:
  case INTRN_CQCOS:
    /*
     *	real =  cos(rz) * cosh(iz);
     *	imag = -sin(rz) * sinh(iz);
     */
    return em_complex_cos(block, by_value(tree, 0));

  case INTRN_F4C4ABS:
  case INTRN_F8C8ABS:
  case INTRN_FQCQABS:
    return em_complex_abs(block, by_value(tree, 0));

  case INTRN_C4SIN:
  case INTRN_C8SIN:
  case INTRN_CQSIN:
    /*
     *	real = sin(rz) * cosh(iz);
     *	imag = cos(rz) * sinh(iz);
     */
    return em_complex_sin(block, by_value(tree, 0));

  case INTRN_F4CIS:
  case INTRN_F8CIS:
  case INTRN_FQCIS:
    /*
     *	cos(x) + i*sin(x)
     *  do not expand this as the library function is much more
     *  efficient than calling cos() and sin()
     */
    break;

  case INTRN_U4I4MALLOC:
    if ( DEBUG_Trap_Uv )
      WN_intrinsic(tree) = INTRN_U4I4TRAPUV_MALLOC;
    break;

  case INTRN_U8I8MALLOC:
    if ( DEBUG_Trap_Uv )
      WN_intrinsic(tree) = INTRN_U8I8TRAPUV_MALLOC;
    break;

  case INTRN_U4FREE:
  case INTRN_U8FREE:
  case INTRN_MDATE:
  case INTRN_I4DATE:
  case INTRN_I8DATE:
  case INTRN_I4ERRSNS:
  case INTRN_I8ERRSNS:
  case INTRN_VEXIT:
  case INTRN_I4EXIT:
  case INTRN_I8EXIT:
  case INTRN_TIME:
  case INTRN_F4SECNDS:
  case INTRN_F8SECNDS:
  case INTRN_PAUSE:
  case INTRN_STOP:
  case INTRN_F77_BOUNDS_ERR:
  case INTRN_F4I4RAN:
  case INTRN_F4I8RAN:
  case INTRN_F8I4RAN:
  case INTRN_F8I8RAN:
  case INTRN_FQI4RAN:
  case INTRN_FQI8RAN:
    break;

  case INTRN_I8DIVFLOOR:
  case INTRN_I4DIVFLOOR:
  case INTRN_U4DIVFLOOR:
  case INTRN_U8DIVFLOOR:
    return em_divfloor(block, rtype, by_value(tree, 0), by_value(tree, 1));

  case INTRN_I4DIVCEIL:
  case INTRN_I8DIVCEIL:
  case INTRN_U4DIVCEIL:
  case INTRN_U8DIVCEIL:
    return em_divceil(block, rtype, by_value(tree, 0), by_value(tree, 1));

  case INTRN_I4MODFLOOR:
  case INTRN_I8MODFLOOR:
  case INTRN_U4MODFLOOR:
  case INTRN_U8MODFLOOR:
  case INTRN_I4MODCEIL:
  case INTRN_I8MODCEIL:
  case INTRN_U4MODCEIL:
  case INTRN_U8MODCEIL:
    Is_True(FALSE,
	   ("%s not yet implemented. Micheal Wolf said this would never, ever, ever. be needed. Go away", INTR_intrinsic_name(tree)));
    break;
 
  case INTRN_I1MVBITS:
  case INTRN_I2MVBITS:
  case INTRN_I4MVBITS:
  case INTRN_I8MVBITS:

  case INTRN_I4CINDEX:
    break;

  case INTRN_BZERO:
    if (CG_mem_intrinsics)
    {
      return em_bzero(block, tree, WN_arg(tree, 0), WN_arg(tree, 1));
    }
    break;

  case INTRN_MEMSET:
#if defined(KEY) && !defined(TARG_SL) && !defined(TARG_NVISA)	// Emulate memset; don't call PathScale memset.
    if (CG_mem_intrinsics && Emulate_memset)
    {
      WN * con = WN_arg(tree, 1);
      WN * size = WN_arg(tree, 2);
      if (Is_Integer_Constant(size) && Is_Integer_Constant(con)) {
	int n = (int) WN_const_val(size);
	int mod;
	if(Is_Target_64bit())
	  mod = 8;
	else
	  mod = 4;
 	if (((n % mod) == 0) && (n <= (mod * 16)))
	  return em_memset(block, tree, WN_arg(tree, 0), WN_arg(tree, 1), WN_arg(tree, 2));
      }
    }
#endif
    break;

  case INTRN_BCOPY:
    if (CG_mem_intrinsics)
    {
      return em_bcopy(block, tree, WN_arg(tree, 0), WN_arg(tree, 1), WN_arg(tree, 2));
    }
    break;

  case INTRN_MEMCPY:
    if (CG_mem_intrinsics)
    {
      return em_memcpy(block, tree, WN_arg(tree, 0), WN_arg(tree, 1), WN_arg(tree, 2));
    }
    break;

  case INTRN_MEMMOVE:
    if (CG_mem_intrinsics)
    {
      return em_memmove(block, tree, WN_arg(tree, 0), WN_arg(tree, 1), WN_arg(tree, 2));
    }
    break;

#ifdef TARG_X8664
  case INTRN_VA_START:
    if (strcmp(Get_Error_Phase(), "VHO Processing") == 0)
      break; // bug 8525: cannot lower va_start at VHO time
    return em_x8664_va_start(block, WN_arg(tree, 0));
    break;
#endif

#if defined(KEY) && !defined(TARG_SL)
  case INTRN_F4CBRT:
  case INTRN_F8CBRT:
    break;
#endif

  default:
    break;
  }

  return NULL;
}


extern WN *emulate(WN *block, WN *tree, struct ALIAS_MANAGER *alias)
{
  WN		*wn = NULL;
  alias_manager = alias;

  if (OPCODE_is_intrinsic(WN_opcode(tree)))
  {
    if (Inline_Intrinsics_Allowed)
    {
      wn = emulate_intrinsic_op(block, tree);
    }
  }
  else
  {
    switch(WN_operator(tree))
    {
#ifndef TARG_LOONGSON
    case OPR_NEG:
      if (MTYPE_is_quad(WN_rtype(tree)))
      {
	wn = em_quad_neg(block, tree);
      }
      break;
#endif

    case OPR_SELECT:
      if (MTYPE_is_quad(WN_rtype(tree)))
      {
	wn = em_split_select(block, tree);
      }
      break;

    case OPR_SQRT:
      if (MTYPE_is_complex(WN_rtype(tree)))
      {
	if (Inline_Intrinsics_Allowed)
	{
	  wn = em_complex_sqrt(block, WN_kid0(tree));
	}
      }
      break;

    case OPR_ALLOCA:
	wn = em_alloca(block,tree);
	break;

    default:
      break;
    }
  }
 
  alias_manager = NULL;
  return wn;
}

#ifdef KEY // bug 6938
extern WN *emulate_fast_exp(WN *block, WN *tree, struct ALIAS_MANAGER *alias)
{
  if (! Inline_Intrinsics_Allowed)
    return NULL;

  INTRINSIC     id = (INTRINSIC) WN_intrinsic(tree);
  TYPE_ID	rtype = WN_rtype(tree);

  switch(id) {
  case INTRN_I4EXPEXPR:
  case INTRN_I8EXPEXPR:
   /*
    *	do these regardless of flags, as they are always safe
    */
    return em_exp_int(block, by_value(tree, 0), by_value(tree,1), rtype);

  case INTRN_F4I4EXPEXPR:
  case INTRN_F4I8EXPEXPR:
  case INTRN_F8I4EXPEXPR:
  case INTRN_F8I8EXPEXPR:
  case INTRN_FQI4EXPEXPR:
  case INTRN_FQI8EXPEXPR:
  case INTRN_C4I4EXPEXPR:
  case INTRN_C4I8EXPEXPR:
  case INTRN_C8I4EXPEXPR:
  case INTRN_C8I8EXPEXPR:
  case INTRN_CQI4EXPEXPR:
  case INTRN_CQI8EXPEXPR:
   /*
    *   The consensus is we allow constants (-1, 0, 1, 2) as
    *   always safe , regardless of the Fast_Exp_Allowed 
    */
    return em_exp_int(block, by_value(tree, 0), by_value(tree,1), rtype);

  case INTRN_F4EXPEXPR:
  case INTRN_F8EXPEXPR:
  case INTRN_FQEXPEXPR:
  case INTRN_C4EXPEXPR:
  case INTRN_C8EXPEXPR:
  case INTRN_CQEXPEXPR:
    if (Fast_Exp_Allowed)
      return em_exp_float(block, by_value(tree, 0), by_value(tree, 1), rtype);
    break;
  default: ;
    break;
  }
 
  return NULL;
}
#endif
