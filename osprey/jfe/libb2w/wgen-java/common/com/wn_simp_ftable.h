/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
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


/* wn_simp_ftable.h: This file automatically generated. */

typedef simpnode (*simpfunction)(OPCODE, simpnode, simpnode, BOOL, BOOL);

static
simpfunction simplify_function_table[OPERATOR_LAST+1]={
NULL, /* index 0 is not used */
simp_abs,	 /* OPR_ABS */
simp_add_sub,	 /* OPR_ADD */
NULL, 	/* OPR_AGOTO */
NULL, 	/* OPR_ALTENTRY */
NULL, 	/* OPR_ARRAY */
NULL, 	/* OPR_ARRAYEXP */
NULL, 	/* OPR_ARRSECTION */
simp_shift,	 /* OPR_ASHR */
NULL, 	/* OPR_ASSERT */
NULL, 	/* OPR_BACKWARD_BARRIER */
simp_band,	 /* OPR_BAND */
simp_bior,	 /* OPR_BIOR */
NULL, 	/* OPR_BLOCK */
simp_bnor,	 /* OPR_BNOR */
simp_not,	 /* OPR_BNOT */
simp_bxor,	 /* OPR_BXOR */
NULL, 	/* OPR_CALL */
simp_cand, 	/* OPR_CAND */
NULL, 	/* OPR_CASEGOTO */
NULL, 	/* OPR_CEIL */
simp_cior, 	/* OPR_CIOR */
NULL, 	/* OPR_COMMA */
NULL, 	/* OPR_COMMENT */
NULL, 	/* OPR_COMPGOTO */
NULL, 	/* OPR_COMPLEX */
NULL, 	/* OPR_CONST */
NULL, 	/* OPR_CSELECT */
simp_cvt,	 /* OPR_CVT */
NULL, 	/* OPR_CVTL */
simp_div,	 /* OPR_DIV */
NULL, 	/* OPR_DIVREM */
NULL, 	/* OPR_DO_LOOP */
NULL, 	/* OPR_DO_WHILE */
simp_eq_neq,	 /* OPR_EQ */
NULL, 	/* OPR_EVAL */
NULL, 	/* OPR_EXC_SCOPE_BEGIN */
NULL, 	/* OPR_EXC_SCOPE_END */
NULL, 	/* OPR_FALSEBR */
NULL, 	/* OPR_FLOOR */
NULL, 	/* OPR_FORWARD_BARRIER */
NULL, 	/* OPR_FUNC_ENTRY */
simp_relop,	 /* OPR_GE */
NULL, 	/* OPR_GOTO */
simp_relop,	 /* OPR_GT */
NULL, 	/* OPR_HIGHMPY */
NULL, 	/* OPR_HIGHPART */
NULL, 	/* OPR_ICALL */
NULL, 	/* OPR_IDNAME */
NULL, 	/* OPR_IF */
NULL, 	/* OPR_ILDA */
NULL, 	/* OPR_ILDBITS */
NULL, 	/* OPR_ILOAD */
NULL, 	/* OPR_ILOADX */
simp_cvt,	 /* OPR_IMAGPART */
NULL, 	/* OPR_INTCONST */
NULL, 	/* OPR_INTRINSIC_CALL */
NULL, 	/* OPR_INTRINSIC_OP */
NULL, 	/* OPR_IO */
NULL, 	/* OPR_IO_ITEM */
NULL, 	/* OPR_ISTBITS */
NULL, 	/* OPR_ISTORE */
NULL, 	/* OPR_ISTOREX */
NULL, 	/* OPR_LABEL */
simp_land,	 /* OPR_LAND */
NULL, 	/* OPR_LDA */
NULL, 	/* OPR_LDBITS */
NULL, 	/* OPR_LDID */
simp_relop,	 /* OPR_LE */
simp_lior,	 /* OPR_LIOR */
simp_not,	 /* OPR_LNOT */
NULL, 	/* OPR_LOOP_INFO */
NULL, 	/* OPR_LOWPART */
simp_shift,	 /* OPR_LSHR */
simp_relop,	 /* OPR_LT */
NULL, 	/* OPR_MADD */
simp_min_max,	 /* OPR_MAX */
NULL, 	/* OPR_MAXPART */
simp_min_max,	 /* OPR_MIN */
NULL, 	/* OPR_MINMAX */
NULL, 	/* OPR_MINPART */
NULL, 	/* OPR_MLOAD */
simp_mod_rem,	 /* OPR_MOD */
simp_times,	 /* OPR_MPY */
NULL, 	/* OPR_MSTORE */
NULL, 	/* OPR_MSUB */
simp_eq_neq,	 /* OPR_NE */
simp_neg,	 /* OPR_NEG */
NULL, 	/* OPR_NMADD */
NULL, 	/* OPR_NMSUB */
NULL, 	/* OPR_OPTPARM */
NULL, 	/* OPR_OPT_CHI */
NULL, 	/* OPR_OPT_RESERVE2 */
NULL, 	/* OPR_PAREN */
NULL, 	/* OPR_PARM */
NULL, 	/* OPR_PICCALL */
NULL, 	/* OPR_PRAGMA */
NULL, 	/* OPR_PREFETCH */
NULL, 	/* OPR_PREFETCHX */
NULL, 	/* OPR_RCOMMA */
simp_cvt,	 /* OPR_REALPART */
simp_recip,	 /* OPR_RECIP */
NULL, 	/* OPR_REGION */
NULL, 	/* OPR_REGION_EXIT */
simp_mod_rem,	 /* OPR_REM */
NULL, 	/* OPR_RETURN */
NULL, 	/* OPR_RETURN_VAL */
NULL, 	/* OPR_RND */
simp_recip,	 /* OPR_RSQRT */
NULL, 	/* OPR_SELECT */
simp_shift,	 /* OPR_SHL */
simp_recip,	 /* OPR_SQRT */
NULL, 	/* OPR_STBITS */
NULL, 	/* OPR_STID */
simp_add_sub,	 /* OPR_SUB */
NULL, 	/* OPR_SWITCH */
simp_cvt,	 /* OPR_TAS */
NULL, 	/* OPR_TRAP */
NULL, 	/* OPR_TRIPLET */
NULL, 	/* OPR_TRUEBR */
simp_cvt,	 /* OPR_TRUNC */
NULL, 	/* OPR_VFCALL */
NULL, 	/* OPR_WHERE */
NULL, 	/* OPR_WHILE_DO */
NULL, 	/* OPR_XGOTO */
NULL, 	/* OPR_XMPY */
NULL, 	/* OPR_XPRAGMA */
NULL,	/* OPR_AFFIRM */
NULL,	/* OPR_ALLOCA*/
NULL,	/* OPR_DEALLOCA */
NULL,	/* OPR_LDMA */
#ifdef KEY
NULL,   /* OPR_ASM_STMT */
NULL,   /* OPR_ASM_EXPR */
NULL,   /* OPR_ASM_INPUT */
NULL,   /* OPR_RROTATE */
NULL,   /* OPR_LDA_LABEL */
NULL,   /* OPR_GOTO_OUTER_BLOCK */
NULL,   /* OPR_EXTRACT_BITS */
NULL,   /* OPR_COMPOSE_BITS */
#endif
#ifdef TARG_X8664
NULL,   /* OPR_REPLICATE */
NULL,   /* OPR_REDUCE_ADD */
NULL,   /* OPR_REDUCE_MPY */
NULL,   /* OPR_REDUCE_MAX */
NULL,   /* OPR_REDUCE_MIN */
NULL,   /* OPR_PURE_CALL_OP */
NULL,   /* OPR_SHUFFLE */
NULL,   /* OPR_ATOMIC_RSQRT */
#elif defined(TARG_MIPS)
NULL,   /* OPR_PURE_CALL_OP */
#endif
};
