/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
 */

/* Generated automatically by the program `genconditions' from the target
   machine description file.  */

#include "hconfig.h"
#include "insn-constants.h"

/* Do not allow checking to confuse the issue.  */
#undef ENABLE_CHECKING
#undef ENABLE_TREE_CHECKING
#undef ENABLE_RTL_CHECKING
#undef ENABLE_RTL_FLAG_CHECKING
#undef ENABLE_GC_CHECKING
#undef ENABLE_GC_ALWAYS_COLLECT

#include "system.h"
#include "rtl.h"
#include "tm_p.h"
#include "function.h"

/* Fake - insn-config.h doesn't exist yet.  */
#define MAX_RECOG_OPERANDS 10
#define MAX_DUP_OPERANDS 10
#define MAX_INSNS_PER_SPLIT 5

#include "regs.h"
#include "recog.h"
#include "real.h"
#include "output.h"
#include "flags.h"
#include "hard-reg-set.h"
#include "resource.h"
#include "toplev.h"
#include "reload.h"
#include "gensupport.h"

#define HAVE_eh_return 1
#include "except.h"

/* Dummy external declarations.  */
extern rtx insn;
extern rtx ins1;
extern rtx operands[];
extern int next_insn_tests_no_inequality PARAMS ((rtx));

/* If we don't have __builtin_constant_p, or it's not acceptable in
   array initializers, fall back to assuming that all conditions
   potentially vary at run time.  It works in 3.0.1 and later; 3.0
   only when not optimizing.  */
#if (GCC_VERSION >= 3001) || ((GCC_VERSION == 3000) && !__OPTIMIZE__)
# define MAYBE_EVAL(expr) (__builtin_constant_p(expr) ? (int) (expr) : -1)
#else
# define MAYBE_EVAL(expr) -1
#endif

/* This table lists each condition found in the machine description.
   Each condition is mapped to its truth value (0 or 1), or -1 if that
   cannot be calculated at compile time. */

const struct c_test insn_conditions[] = {
  { "ix86_match_ccmode (insn, CCGOCmode)\n\
   && ix86_binary_operator_ok (MINUS, QImode, operands)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCGOCmode)
   && ix86_binary_operator_ok (MINUS, QImode, operands)) },
  { "TARGET_PARTIAL_REG_STALL\n\
   && ix86_binary_operator_ok (PLUS, HImode, operands)",
    MAYBE_EVAL (TARGET_PARTIAL_REG_STALL
   && ix86_binary_operator_ok (PLUS, HImode, operands)) },
  { "! TARGET_PARTIAL_REG_STALL && reload_completed\n\
   && (GET_MODE (operands[0]) == HImode\n\
       || (GET_MODE (operands[0]) == QImode \n\
	   && (TARGET_PROMOTE_QImode || optimize_size)))",
    MAYBE_EVAL (! TARGET_PARTIAL_REG_STALL && reload_completed
   && (GET_MODE (operands[0]) == HImode
       || (GET_MODE (operands[0]) == QImode 
	   && (TARGET_PROMOTE_QImode || optimize_size)))) },
  { "!TARGET_64BIT && TARGET_CMOVE\n\
   && (GET_CODE (operands[2]) != MEM || GET_CODE (operands[3]) != MEM)",
    MAYBE_EVAL (!TARGET_64BIT && TARGET_CMOVE
   && (GET_CODE (operands[2]) != MEM || GET_CODE (operands[3]) != MEM)) },
  { "TARGET_SSE || TARGET_80387",
    MAYBE_EVAL (TARGET_SSE || TARGET_80387) },
  { "!TARGET_64BIT && TARGET_USE_LOOP\n\
   && reload_completed\n\
   && REGNO (operands[1]) != 2",
    MAYBE_EVAL (!TARGET_64BIT && TARGET_USE_LOOP
   && reload_completed
   && REGNO (operands[1]) != 2) },
  { "ix86_binary_operator_ok (ASHIFTRT, SImode, operands)",
    MAYBE_EVAL (ix86_binary_operator_ok (ASHIFTRT, SImode, operands)) },
  { "!TARGET_64BIT && TARGET_GNU_TLS",
    MAYBE_EVAL (!TARGET_64BIT && TARGET_GNU_TLS) },
  { "reload_completed\n\
    && (!REG_P (operands[0]) || ANY_QI_REG_P (operands[0]))\n\
    && ((ix86_match_ccmode (insn, CCZmode)\n\
	 && !(INTVAL (operands[1]) & ~255))\n\
	|| (ix86_match_ccmode (insn, CCNOmode)\n\
	    && !(INTVAL (operands[1]) & ~127)))\n\
    && GET_MODE (operands[0]) != QImode",
    MAYBE_EVAL (reload_completed
    && (!REG_P (operands[0]) || ANY_QI_REG_P (operands[0]))
    && ((ix86_match_ccmode (insn, CCZmode)
	 && !(INTVAL (operands[1]) & ~255))
	|| (ix86_match_ccmode (insn, CCNOmode)
	    && !(INTVAL (operands[1]) & ~127)))
    && GET_MODE (operands[0]) != QImode) },
  { "TARGET_64BIT && ix86_match_ccmode (insn, CCGOCmode)\n\
   && ix86_binary_operator_ok (LSHIFTRT, HImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_match_ccmode (insn, CCGOCmode)
   && ix86_binary_operator_ok (LSHIFTRT, HImode, operands)) },
  { "ix86_binary_operator_ok (ASHIFTRT, QImode, operands)\n\
   && (TARGET_SHIFT1 || optimize_size)",
    MAYBE_EVAL (ix86_binary_operator_ok (ASHIFTRT, QImode, operands)
   && (TARGET_SHIFT1 || optimize_size)) },
  { "ix86_binary_operator_ok (ASHIFTRT, SImode, operands)\n\
   && (TARGET_SHIFT1 || optimize_size)",
    MAYBE_EVAL (ix86_binary_operator_ok (ASHIFTRT, SImode, operands)
   && (TARGET_SHIFT1 || optimize_size)) },
  { "TARGET_SSE2 && TARGET_SSE_MATH\n\
   && GET_RTX_CLASS (GET_CODE (operands[3])) != 'c'",
    MAYBE_EVAL (TARGET_SSE2 && TARGET_SSE_MATH
   && GET_RTX_CLASS (GET_CODE (operands[3])) != 'c') },
  { "TARGET_SSE2 && TARGET_IEEE_FP && TARGET_SSE_MATH",
    MAYBE_EVAL (TARGET_SSE2 && TARGET_IEEE_FP && TARGET_SSE_MATH) },
  { "TARGET_SSE2",
    MAYBE_EVAL (TARGET_SSE2) },
  { "(GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)\n\
   && !optimize_size && TARGET_INTEGER_DFMODE_MOVES\n\
   && (reload_in_progress || reload_completed\n\
       || (ix86_cmodel == CM_MEDIUM || ix86_cmodel == CM_LARGE)\n\
       || GET_CODE (operands[1]) != CONST_DOUBLE\n\
       || memory_operand (operands[0], DFmode))",
    MAYBE_EVAL ((GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)
   && !optimize_size && TARGET_INTEGER_DFMODE_MOVES
   && (reload_in_progress || reload_completed
       || (ix86_cmodel == CM_MEDIUM || ix86_cmodel == CM_LARGE)
       || GET_CODE (operands[1]) != CONST_DOUBLE
       || memory_operand (operands[0], DFmode))) },
  { "ix86_match_ccmode (insn, CCNOmode)\n\
   && ix86_binary_operator_ok (XOR, SImode, operands)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCNOmode)
   && ix86_binary_operator_ok (XOR, SImode, operands)) },
  { "ix86_match_ccmode (insn, CCNOmode)\n\
   && ix86_unary_operator_ok (NEG, HImode, operands)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCNOmode)
   && ix86_unary_operator_ok (NEG, HImode, operands)) },
  { "ix86_unary_operator_ok (ABS, SFmode, operands)",
    MAYBE_EVAL (ix86_unary_operator_ok (ABS, SFmode, operands)) },
  { "ix86_match_ccmode (insn, CCNOmode)\n\
   && ix86_binary_operator_ok (AND, SImode, operands)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCNOmode)
   && ix86_binary_operator_ok (AND, SImode, operands)) },
  { "TARGET_64BIT && INTVAL (operands[2]) == 63 && (TARGET_USE_CLTD || optimize_size)\n\
   && ix86_binary_operator_ok (ASHIFTRT, DImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && INTVAL (operands[2]) == 63 && (TARGET_USE_CLTD || optimize_size)
   && ix86_binary_operator_ok (ASHIFTRT, DImode, operands)) },
  { "TARGET_SSE2 && TARGET_SSE_MATH && TARGET_IEEE_FP",
    MAYBE_EVAL (TARGET_SSE2 && TARGET_SSE_MATH && TARGET_IEEE_FP) },
  { "TARGET_80387\n\
   && (GET_MODE (operands[1]) == SFmode || GET_MODE (operands[1]) == DFmode)\n\
   && GET_MODE (operands[1]) == GET_MODE (operands[2])\n\
   && !ix86_use_fcomi_compare (GET_CODE (operands[0]))\n\
   && SELECT_CC_MODE (GET_CODE (operands[0]),\n\
		      operands[1], operands[2]) == CCFPmode\n\
   && ix86_fp_jump_nontrivial_p (GET_CODE (operands[0]))",
    MAYBE_EVAL (TARGET_80387
   && (GET_MODE (operands[1]) == SFmode || GET_MODE (operands[1]) == DFmode)
   && GET_MODE (operands[1]) == GET_MODE (operands[2])
   && !ix86_use_fcomi_compare (GET_CODE (operands[0]))
   && SELECT_CC_MODE (GET_CODE (operands[0]),
		      operands[1], operands[2]) == CCFPmode
   && ix86_fp_jump_nontrivial_p (GET_CODE (operands[0]))) },
  { "TARGET_64BIT\n\
   && ix86_binary_operator_ok (IOR, DImode, operands)",
    MAYBE_EVAL (TARGET_64BIT
   && ix86_binary_operator_ok (IOR, DImode, operands)) },
  { "TARGET_SSE2 && TARGET_SSE_MATH && !TARGET_IEEE_FP\n\
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)",
    MAYBE_EVAL (TARGET_SSE2 && TARGET_SSE_MATH && !TARGET_IEEE_FP
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)) },
  { "!TARGET_64BIT && reload_completed\n\
   && true_regnum (operands[0]) == true_regnum (operands[1])",
    MAYBE_EVAL (!TARGET_64BIT && reload_completed
   && true_regnum (operands[0]) == true_regnum (operands[1])) },
  { "TARGET_64BIT\n\
   &&  ix86_match_ccmode (insn, CCGCmode)",
    MAYBE_EVAL (TARGET_64BIT
   &&  ix86_match_ccmode (insn, CCGCmode)) },
  { "reload_completed\n\
   && true_regnum (operands[0]) != true_regnum (operands[1])",
    MAYBE_EVAL (reload_completed
   && true_regnum (operands[0]) != true_regnum (operands[1])) },
  { "TARGET_64BIT && ix86_match_ccmode (insn, CCGOCmode)\n\
   && ix86_binary_operator_ok (PLUS, SImode, operands)\n\
   /* Current assemblers are broken and do not allow @GOTOFF in\n\
      ought but a memory context.  */\n\
   && ! pic_symbolic_operand (operands[2], VOIDmode)",
    MAYBE_EVAL (TARGET_64BIT && ix86_match_ccmode (insn, CCGOCmode)
   && ix86_binary_operator_ok (PLUS, SImode, operands)
   /* Current assemblers are broken and do not allow @GOTOFF in
      ought but a memory context.  */
   && ! pic_symbolic_operand (operands[2], VOIDmode)) },
  { "! TARGET_NO_FANCY_MATH_387 && TARGET_80387\n\
   && (!TARGET_SSE2 || !TARGET_SSE_MATH)",
    MAYBE_EVAL (! TARGET_NO_FANCY_MATH_387 && TARGET_80387
   && (!TARGET_SSE2 || !TARGET_SSE_MATH)) },
  { "reload_completed\n\
   && ANY_QI_REG_P (operands[0])\n\
   && (ANY_QI_REG_P (operands[1]) || GET_CODE (operands[1]) == MEM)\n\
   && (TARGET_ZERO_EXTEND_WITH_AND && !optimize_size)\n\
   && !reg_overlap_mentioned_p (operands[0], operands[1])",
    MAYBE_EVAL (reload_completed
   && ANY_QI_REG_P (operands[0])
   && (ANY_QI_REG_P (operands[1]) || GET_CODE (operands[1]) == MEM)
   && (TARGET_ZERO_EXTEND_WITH_AND && !optimize_size)
   && !reg_overlap_mentioned_p (operands[0], operands[1])) },
  { "TARGET_64BIT && TARGET_80387\n\
   && ix86_unary_operator_ok (NEG, DFmode, operands)",
    MAYBE_EVAL (TARGET_64BIT && TARGET_80387
   && ix86_unary_operator_ok (NEG, DFmode, operands)) },
  { "TARGET_80387 && (!TARGET_SSE2 || !TARGET_64BIT)",
    MAYBE_EVAL (TARGET_80387 && (!TARGET_SSE2 || !TARGET_64BIT)) },
  { "TARGET_QIMODE_MATH",
    MAYBE_EVAL (TARGET_QIMODE_MATH) },
  { "TARGET_SSE\n\
   && (GET_CODE (operands[1]) == MEM || GET_CODE (operands[2]) == MEM)",
    MAYBE_EVAL (TARGET_SSE
   && (GET_CODE (operands[1]) == MEM || GET_CODE (operands[2]) == MEM)) },
  { "!TARGET_64BIT && TARGET_80387\n\
   && ix86_unary_operator_ok (ABS, XFmode, operands)",
    MAYBE_EVAL (!TARGET_64BIT && TARGET_80387
   && ix86_unary_operator_ok (ABS, XFmode, operands)) },
  { "ix86_binary_operator_ok (AND, QImode, operands)",
    MAYBE_EVAL (ix86_binary_operator_ok (AND, QImode, operands)) },
  { "TARGET_64BIT && ix86_binary_operator_ok (ROTATE, DImode, operands)\n\
   && (TARGET_SHIFT1 || optimize_size)",
    MAYBE_EVAL (TARGET_64BIT && ix86_binary_operator_ok (ROTATE, DImode, operands)
   && (TARGET_SHIFT1 || optimize_size)) },
  { "reload_completed",
    MAYBE_EVAL (reload_completed) },
  { "SSE_FLOAT_MODE_P (GET_MODE (operands[0]))\n\
   && GET_MODE (operands[0]) == GET_MODE (operands[0])",
    MAYBE_EVAL (SSE_FLOAT_MODE_P (GET_MODE (operands[0]))
   && GET_MODE (operands[0]) == GET_MODE (operands[0])) },
  { "(TARGET_64BIT && TARGET_SSE2) || TARGET_80387",
    MAYBE_EVAL ((TARGET_64BIT && TARGET_SSE2) || TARGET_80387) },
  { "! optimize_size\n\
   && get_attr_length (insn) >= ix86_cost->large_insn\n\
   && TARGET_SPLIT_LONG_MOVES",
    MAYBE_EVAL (! optimize_size
   && get_attr_length (insn) >= ix86_cost->large_insn
   && TARGET_SPLIT_LONG_MOVES) },
  { "reload_completed\n\
    && QI_REG_P (operands[0])\n\
    && ((ix86_match_ccmode (insn, CCZmode)\n\
    	 && !(INTVAL (operands[1]) & ~(255 << 8)))\n\
	|| (ix86_match_ccmode (insn, CCNOmode)\n\
	    && !(INTVAL (operands[1]) & ~(127 << 8))))\n\
    && GET_MODE (operands[0]) != QImode",
    MAYBE_EVAL (reload_completed
    && QI_REG_P (operands[0])
    && ((ix86_match_ccmode (insn, CCZmode)
    	 && !(INTVAL (operands[1]) & ~(255 << 8)))
	|| (ix86_match_ccmode (insn, CCNOmode)
	    && !(INTVAL (operands[1]) & ~(127 << 8))))
    && GET_MODE (operands[0]) != QImode) },
  { "TARGET_80387 && TARGET_SSE2",
    MAYBE_EVAL (TARGET_80387 && TARGET_SSE2) },
  { "reload_completed && SSE_REG_P (operands[0])",
    MAYBE_EVAL (reload_completed && SSE_REG_P (operands[0])) },
  { "TARGET_STACK_PROBE",
    MAYBE_EVAL (TARGET_STACK_PROBE) },
  { "TARGET_80387 || (TARGET_SSE2 && TARGET_SSE_MATH)",
    MAYBE_EVAL (TARGET_80387 || (TARGET_SSE2 && TARGET_SSE_MATH)) },
  { "TARGET_64BIT && !symbolic_operand (operands[1], DImode)\n\
   && !x86_64_immediate_operand (operands[1], DImode)",
    MAYBE_EVAL (TARGET_64BIT && !symbolic_operand (operands[1], DImode)
   && !x86_64_immediate_operand (operands[1], DImode)) },
  { "ix86_binary_operator_ok (MINUS, HImode, operands)",
    MAYBE_EVAL (ix86_binary_operator_ok (MINUS, HImode, operands)) },
  { "!optimize_size && !TARGET_USE_CLTD",
    MAYBE_EVAL (!optimize_size && !TARGET_USE_CLTD) },
  { "TARGET_SSE2 && (GET_CODE (operands[1]) == MEM || GET_CODE (operands[2]) == MEM)",
    MAYBE_EVAL (TARGET_SSE2 && (GET_CODE (operands[1]) == MEM || GET_CODE (operands[2]) == MEM)) },
  { "optimize_size || !TARGET_SUB_ESP_4",
    MAYBE_EVAL (optimize_size || !TARGET_SUB_ESP_4) },
  { "(TARGET_80387 || TARGET_SSE2)\n\
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)",
    MAYBE_EVAL ((TARGET_80387 || TARGET_SSE2)
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)) },
  { "ix86_match_ccmode (insn, CCNOmode)\n\
   && ix86_binary_operator_ok (AND, QImode, operands)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCNOmode)
   && ix86_binary_operator_ok (AND, QImode, operands)) },
  { "TARGET_64BIT && ix86_binary_operator_ok (ROTATERT, DImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_binary_operator_ok (ROTATERT, DImode, operands)) },
  { "! TARGET_PARTIAL_REG_STALL && TARGET_CMOVE\n\
   && (GET_MODE (operands[0]) == HImode\n\
       || (GET_MODE (operands[0]) == QImode \n\
	   && (TARGET_PROMOTE_QImode || optimize_size)))",
    MAYBE_EVAL (! TARGET_PARTIAL_REG_STALL && TARGET_CMOVE
   && (GET_MODE (operands[0]) == HImode
       || (GET_MODE (operands[0]) == QImode 
	   && (TARGET_PROMOTE_QImode || optimize_size)))) },
  { "TARGET_64BIT && ix86_match_ccmode (insn, CCNOmode)\n\
   && ix86_binary_operator_ok (AND, DImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_match_ccmode (insn, CCNOmode)
   && ix86_binary_operator_ok (AND, DImode, operands)) },
  { "reload_completed && !SSE_REG_P (operands[0])\n\
   && (!TARGET_64BIT || FP_REG_P (operands[0]))",
    MAYBE_EVAL (reload_completed && !SSE_REG_P (operands[0])
   && (!TARGET_64BIT || FP_REG_P (operands[0]))) },
  { "ix86_match_ccmode (insn, CCGOCmode)\n\
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)\n\
   /* Current assemblers are broken and do not allow @GOTOFF in\n\
      ought but a memory context.  */\n\
   && ! pic_symbolic_operand (operands[2], VOIDmode)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCGOCmode)
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)
   /* Current assemblers are broken and do not allow @GOTOFF in
      ought but a memory context.  */
   && ! pic_symbolic_operand (operands[2], VOIDmode)) },
  { "!TARGET_64BIT && TARGET_80387 && TARGET_USE_FIOP",
    MAYBE_EVAL (!TARGET_64BIT && TARGET_80387 && TARGET_USE_FIOP) },
  { "ix86_match_ccmode (insn, CCGOCmode)\n\
   && ix86_binary_operator_ok (ASHIFT, HImode, operands)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCGOCmode)
   && ix86_binary_operator_ok (ASHIFT, HImode, operands)) },
  { "TARGET_64BIT && ix86_match_ccmode (insn, CCGOCmode)\n\
   && ix86_binary_operator_ok (MINUS, SImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_match_ccmode (insn, CCGOCmode)
   && ix86_binary_operator_ok (MINUS, SImode, operands)) },
  { "ix86_unary_operator_ok (ABS, DFmode, operands)",
    MAYBE_EVAL (ix86_unary_operator_ok (ABS, DFmode, operands)) },
  { "ix86_match_ccmode (insn, CCNOmode) && ! optimize_size",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCNOmode) && ! optimize_size) },
  { "TARGET_64BIT && ix86_match_ccmode (insn, CCGOCmode)",
    MAYBE_EVAL (TARGET_64BIT && ix86_match_ccmode (insn, CCGOCmode)) },
  { "TARGET_PNI",
    MAYBE_EVAL (TARGET_PNI) },
  { "ix86_match_ccmode (insn, CCGOCmode)\n\
   && ix86_binary_operator_ok (PLUS, HImode, operands)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCGOCmode)
   && ix86_binary_operator_ok (PLUS, HImode, operands)) },
  { "TARGET_SSE2 && TARGET_SSE_MATH && reload_completed",
    MAYBE_EVAL (TARGET_SSE2 && TARGET_SSE_MATH && reload_completed) },
  { "TARGET_80387\n\
   && GET_RTX_CLASS (GET_CODE (operands[3])) != 'c'",
    MAYBE_EVAL (TARGET_80387
   && GET_RTX_CLASS (GET_CODE (operands[3])) != 'c') },
  { "TARGET_64BIT && ix86_binary_operator_ok (LSHIFTRT, HImode, operands)\n\
   && (TARGET_SHIFT1 || optimize_size)",
    MAYBE_EVAL (TARGET_64BIT && ix86_binary_operator_ok (LSHIFTRT, HImode, operands)
   && (TARGET_SHIFT1 || optimize_size)) },
  { "ix86_match_ccmode (insn, CCGOCmode)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCGOCmode)) },
  { "TARGET_80387 && !(TARGET_SSE2 && TARGET_SSE_MATH)",
    MAYBE_EVAL (TARGET_80387 && !(TARGET_SSE2 && TARGET_SSE_MATH)) },
  { "ix86_binary_operator_ok (ROTATE, HImode, operands)",
    MAYBE_EVAL (ix86_binary_operator_ok (ROTATE, HImode, operands)) },
  { "! TARGET_NO_FANCY_MATH_387 && TARGET_80387\n\
   && (TARGET_SSE_MATH && TARGET_MIX_SSE_I387)",
    MAYBE_EVAL (! TARGET_NO_FANCY_MATH_387 && TARGET_80387
   && (TARGET_SSE_MATH && TARGET_MIX_SSE_I387)) },
  { "TARGET_64BIT && ix86_binary_operator_ok (ROTATE, DImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_binary_operator_ok (ROTATE, DImode, operands)) },
  { "TARGET_64BIT && (TARGET_SINGLE_STRINGOP || optimize_size)",
    MAYBE_EVAL (TARGET_64BIT && (TARGET_SINGLE_STRINGOP || optimize_size)) },
  { "TARGET_80387 && reload_completed",
    MAYBE_EVAL (TARGET_80387 && reload_completed) },
  { "TARGET_64BIT && ix86_binary_operator_ok (XOR, SImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_binary_operator_ok (XOR, SImode, operands)) },
  { "ix86_unary_operator_ok (NEG, DFmode, operands)",
    MAYBE_EVAL (ix86_unary_operator_ok (NEG, DFmode, operands)) },
  { "!TARGET_80387 && TARGET_SSE2\n\
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)",
    MAYBE_EVAL (!TARGET_80387 && TARGET_SSE2
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)) },
  { "ix86_match_ccmode (insn, CCGOCmode)\n\
   && ix86_binary_operator_ok (ASHIFTRT, SImode, operands)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCGOCmode)
   && ix86_binary_operator_ok (ASHIFTRT, SImode, operands)) },
  { "ix86_match_ccmode (insn, CCGOCmode)\n\
   && ix86_binary_operator_ok (LSHIFTRT, QImode, operands)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCGOCmode)
   && ix86_binary_operator_ok (LSHIFTRT, QImode, operands)) },
  { "TARGET_64BIT\n\
   && (!TARGET_PARTIAL_REG_STALL || optimize_size)",
    MAYBE_EVAL (TARGET_64BIT
   && (!TARGET_PARTIAL_REG_STALL || optimize_size)) },
  { "!TARGET_64BIT && TARGET_CMOVE",
    MAYBE_EVAL (!TARGET_64BIT && TARGET_CMOVE) },
  { "TARGET_64BIT && ix86_match_ccmode (insn, CCNOmode)",
    MAYBE_EVAL (TARGET_64BIT && ix86_match_ccmode (insn, CCNOmode)) },
  { "TARGET_SSE && TARGET_IEEE_FP",
    MAYBE_EVAL (TARGET_SSE && TARGET_IEEE_FP) },
  { "(GET_MODE (operands[0]) == QImode || GET_MODE (operands[0]) == HImode\n\
    || (TARGET_64BIT && GET_MODE (operands[0]) == SImode))\n\
   && (!TARGET_PARTIAL_REG_STALL || optimize_size)\n\
   && GET_MODE (operands[0]) == GET_MODE (operands[1])\n\
   && (GET_MODE (operands[0]) == GET_MODE (operands[3])\n\
       || GET_MODE (operands[3]) == VOIDmode)",
    MAYBE_EVAL ((GET_MODE (operands[0]) == QImode || GET_MODE (operands[0]) == HImode
    || (TARGET_64BIT && GET_MODE (operands[0]) == SImode))
   && (!TARGET_PARTIAL_REG_STALL || optimize_size)
   && GET_MODE (operands[0]) == GET_MODE (operands[1])
   && (GET_MODE (operands[0]) == GET_MODE (operands[3])
       || GET_MODE (operands[3]) == VOIDmode)) },
  { "TARGET_80387 && GET_RTX_CLASS (GET_CODE (operands[3])) == 'c'",
    MAYBE_EVAL (TARGET_80387 && GET_RTX_CLASS (GET_CODE (operands[3])) == 'c') },
  { "TARGET_80387 && !TARGET_SSE_MATH\n\
   && GET_RTX_CLASS (GET_CODE (operands[3])) != 'c'\n\
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)",
    MAYBE_EVAL (TARGET_80387 && !TARGET_SSE_MATH
   && GET_RTX_CLASS (GET_CODE (operands[3])) != 'c'
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)) },
  { "!TARGET_64BIT && TARGET_80387\n\
   && GET_RTX_CLASS (GET_CODE (operands[3])) != 'c'",
    MAYBE_EVAL (!TARGET_64BIT && TARGET_80387
   && GET_RTX_CLASS (GET_CODE (operands[3])) != 'c') },
  { "TARGET_64BIT && TARGET_SSE2",
    MAYBE_EVAL (TARGET_64BIT && TARGET_SSE2) },
  { "TARGET_SSE_MATH && GET_RTX_CLASS (GET_CODE (operands[3])) == 'c'\n\
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)",
    MAYBE_EVAL (TARGET_SSE_MATH && GET_RTX_CLASS (GET_CODE (operands[3])) == 'c'
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)) },
  { "TARGET_64BIT && ix86_match_ccmode (insn, CCGOCmode)\n\
   && ix86_binary_operator_ok (ASHIFT, SImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_match_ccmode (insn, CCGOCmode)
   && ix86_binary_operator_ok (ASHIFT, SImode, operands)) },
  { "ix86_binary_operator_ok (ROTATERT, QImode, operands)\n\
   && (TARGET_SHIFT1 || optimize_size)",
    MAYBE_EVAL (ix86_binary_operator_ok (ROTATERT, QImode, operands)
   && (TARGET_SHIFT1 || optimize_size)) },
  { "ix86_match_ccmode (insn, CCZmode)\n\
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)\n\
   /* Current assemblers are broken and do not allow @GOTOFF in\n\
      ought but a memory context.  */\n\
   && ! pic_symbolic_operand (operands[2], VOIDmode)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCZmode)
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)
   /* Current assemblers are broken and do not allow @GOTOFF in
      ought but a memory context.  */
   && ! pic_symbolic_operand (operands[2], VOIDmode)) },
  { "TARGET_SSE2\n\
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)",
    MAYBE_EVAL (TARGET_SSE2
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)) },
  { "TARGET_64BIT && ix86_check_movabs (insn, 1)",
    MAYBE_EVAL (TARGET_64BIT && ix86_check_movabs (insn, 1)) },
  { "(! TARGET_PARTIAL_REG_STALL || optimize_size)\n\
   && ix86_match_ccmode (insn, CCNOmode)\n\
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)",
    MAYBE_EVAL ((! TARGET_PARTIAL_REG_STALL || optimize_size)
   && ix86_match_ccmode (insn, CCNOmode)
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)) },
  { "TARGET_80387 && TARGET_SSE_MATH && TARGET_MIX_SSE_I387\n\
   && GET_RTX_CLASS (GET_CODE (operands[3])) != 'c'\n\
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)",
    MAYBE_EVAL (TARGET_80387 && TARGET_SSE_MATH && TARGET_MIX_SSE_I387
   && GET_RTX_CLASS (GET_CODE (operands[3])) != 'c'
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)) },
  { "reload_completed && !SSE_REG_P (operands[0])",
    MAYBE_EVAL (reload_completed && !SSE_REG_P (operands[0])) },
  { "! TARGET_PARTIAL_REG_STALL && reload_completed\n\
   /* Ensure that the operand will remain sign-extended immediate.  */\n\
   && ix86_match_ccmode (insn, INTVAL (operands[1]) >= 0 ? CCNOmode : CCZmode)\n\
   && ! TARGET_FAST_PREFIX\n\
   && ! optimize_size",
    MAYBE_EVAL (! TARGET_PARTIAL_REG_STALL && reload_completed
   /* Ensure that the operand will remain sign-extended immediate.  */
   && ix86_match_ccmode (insn, INTVAL (operands[1]) >= 0 ? CCNOmode : CCZmode)
   && ! TARGET_FAST_PREFIX
   && ! optimize_size) },
  { "ix86_match_ccmode (insn, CCGCmode)\n\
   && find_regno_note (insn, REG_DEAD, true_regnum (operands[0]))",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCGCmode)
   && find_regno_note (insn, REG_DEAD, true_regnum (operands[0]))) },
  { "optimize_size || !TARGET_SUB_ESP_8",
    MAYBE_EVAL (optimize_size || !TARGET_SUB_ESP_8) },
  { "TARGET_SSE2 && TARGET_SSE_MATH && (!TARGET_80387 || !TARGET_MIX_SSE_I387)",
    MAYBE_EVAL (TARGET_SSE2 && TARGET_SSE_MATH && (!TARGET_80387 || !TARGET_MIX_SSE_I387)) },
  { "ix86_match_ccmode (insn, CCGOCmode)\n\
   && ix86_binary_operator_ok (PLUS, SImode, operands)\n\
   /* Current assemblers are broken and do not allow @GOTOFF in\n\
      ought but a memory context.  */\n\
   && ! pic_symbolic_operand (operands[2], VOIDmode)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCGOCmode)
   && ix86_binary_operator_ok (PLUS, SImode, operands)
   /* Current assemblers are broken and do not allow @GOTOFF in
      ought but a memory context.  */
   && ! pic_symbolic_operand (operands[2], VOIDmode)) },
  { "(GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)\n\
   && ix86_match_ccmode (insn, CCmode)",
    MAYBE_EVAL ((GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)
   && ix86_match_ccmode (insn, CCmode)) },
  { "TARGET_64BIT && TARGET_SSE2\n\
   && (reload_in_progress || reload_completed\n\
       || (register_operand (operands[0], VOIDmode)\n\
	   && register_operand (operands[1], VOIDmode)))",
    MAYBE_EVAL (TARGET_64BIT && TARGET_SSE2
   && (reload_in_progress || reload_completed
       || (register_operand (operands[0], VOIDmode)
	   && register_operand (operands[1], VOIDmode)))) },
  { "! TARGET_PARTIAL_REG_STALL || optimize_size",
    MAYBE_EVAL (! TARGET_PARTIAL_REG_STALL || optimize_size) },
  { "TARGET_64BIT && ix86_binary_operator_ok (IOR, SImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_binary_operator_ok (IOR, SImode, operands)) },
  { "TARGET_SSE && !TARGET_64BIT\n\
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)",
    MAYBE_EVAL (TARGET_SSE && !TARGET_64BIT
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)) },
  { "TARGET_64BIT && ix86_binary_operator_ok (ASHIFT, DImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_binary_operator_ok (ASHIFT, DImode, operands)) },
  { "!TARGET_PARTIAL_REG_STALL\n\
   && ix86_binary_operator_ok (ASHIFT, HImode, operands)",
    MAYBE_EVAL (!TARGET_PARTIAL_REG_STALL
   && ix86_binary_operator_ok (ASHIFT, HImode, operands)) },
  { "ix86_match_ccmode (insn, CCmode)\n\
   && ix86_binary_operator_ok (MINUS, HImode, operands)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCmode)
   && ix86_binary_operator_ok (MINUS, HImode, operands)) },
  { "ix86_binary_operator_ok (LSHIFTRT, QImode, operands)",
    MAYBE_EVAL (ix86_binary_operator_ok (LSHIFTRT, QImode, operands)) },
  { "TARGET_64BIT && TARGET_SSE",
    MAYBE_EVAL (TARGET_64BIT && TARGET_SSE) },
  { "ix86_match_ccmode (insn, CCGOCmode)\n\
   && ix86_binary_operator_ok (MINUS, SImode, operands)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCGOCmode)
   && ix86_binary_operator_ok (MINUS, SImode, operands)) },
  { "ix86_binary_operator_ok (XOR, HImode, operands)",
    MAYBE_EVAL (ix86_binary_operator_ok (XOR, HImode, operands)) },
  { "TARGET_64BIT && (!TARGET_USE_MOV0 || optimize_size)\n\
   && reload_completed",
    MAYBE_EVAL (TARGET_64BIT && (!TARGET_USE_MOV0 || optimize_size)
   && reload_completed) },
  { "! optimize_size && ! TARGET_PUSH_MEMORY",
    MAYBE_EVAL (! optimize_size && ! TARGET_PUSH_MEMORY) },
  { "ix86_match_ccmode (insn, CCGOCmode)\n\
   && ix86_binary_operator_ok (LSHIFTRT, HImode, operands)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCGOCmode)
   && ix86_binary_operator_ok (LSHIFTRT, HImode, operands)) },
  { "ix86_match_ccmode (insn, CCNOmode)\n\
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCNOmode)
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)) },
  { "TARGET_80387 && FLOAT_MODE_P (GET_MODE (operands[1]))\n\
   && !reload_completed && !reload_in_progress\n\
   && (!SSE_FLOAT_MODE_P (GET_MODE (operands[1])) || !TARGET_64BIT)",
    MAYBE_EVAL (TARGET_80387 && FLOAT_MODE_P (GET_MODE (operands[1]))
   && !reload_completed && !reload_in_progress
   && (!SSE_FLOAT_MODE_P (GET_MODE (operands[1])) || !TARGET_64BIT)) },
  { "!TARGET_64BIT\n\
   && optimize_size\n\
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)\n\
   && (reload_in_progress || reload_completed\n\
       || GET_CODE (operands[1]) != CONST_DOUBLE\n\
       || memory_operand (operands[0], XFmode))",
    MAYBE_EVAL (!TARGET_64BIT
   && optimize_size
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)
   && (reload_in_progress || reload_completed
       || GET_CODE (operands[1]) != CONST_DOUBLE
       || memory_operand (operands[0], XFmode))) },
  { "reload_completed\n\
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)\n\
   && ! (ANY_FP_REG_P (operands[0]) || \n\
	 (GET_CODE (operands[0]) == SUBREG\n\
	  && ANY_FP_REG_P (SUBREG_REG (operands[0]))))\n\
   && ! (ANY_FP_REG_P (operands[1]) || \n\
	 (GET_CODE (operands[1]) == SUBREG\n\
	  && ANY_FP_REG_P (SUBREG_REG (operands[1]))))",
    MAYBE_EVAL (reload_completed
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)
   && ! (ANY_FP_REG_P (operands[0]) || 
	 (GET_CODE (operands[0]) == SUBREG
	  && ANY_FP_REG_P (SUBREG_REG (operands[0]))))
   && ! (ANY_FP_REG_P (operands[1]) || 
	 (GET_CODE (operands[1]) == SUBREG
	  && ANY_FP_REG_P (SUBREG_REG (operands[1]))))) },
  { "TARGET_SSE && !TARGET_IEEE_FP\n\
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)",
    MAYBE_EVAL (TARGET_SSE && !TARGET_IEEE_FP
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)) },
  { "ix86_match_ccmode (insn, CCGCmode)\n\
   && (INTVAL (operands[2]) & 0xff) != 0x80",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCGCmode)
   && (INTVAL (operands[2]) & 0xff) != 0x80) },
  { "reload_completed && FLOAT_MODE_P (GET_MODE (operands[0]))",
    MAYBE_EVAL (reload_completed && FLOAT_MODE_P (GET_MODE (operands[0]))) },
  { "optimize_size || !TARGET_ADD_ESP_8",
    MAYBE_EVAL (optimize_size || !TARGET_ADD_ESP_8) },
  { "reload_completed && !SSE_REG_P (operands[0])\n\
   && !SSE_REG_P (operands[1])",
    MAYBE_EVAL (reload_completed && !SSE_REG_P (operands[0])
   && !SSE_REG_P (operands[1])) },
  { "!TARGET_64BIT && TARGET_USE_LOOP",
    MAYBE_EVAL (!TARGET_64BIT && TARGET_USE_LOOP) },
  { "ix86_match_ccmode (insn, CCNOmode)\n\
   && (GET_MODE (operands[0]) == SImode\n\
       || (TARGET_64BIT && GET_MODE (operands[0]) == DImode)\n\
       || GET_MODE (operands[0]) == HImode\n\
       || GET_MODE (operands[0]) == QImode)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCNOmode)
   && (GET_MODE (operands[0]) == SImode
       || (TARGET_64BIT && GET_MODE (operands[0]) == DImode)
       || GET_MODE (operands[0]) == HImode
       || GET_MODE (operands[0]) == QImode)) },
  { "!TARGET_64BIT && TARGET_80387\n\
   && ix86_unary_operator_ok (NEG, DFmode, operands)",
    MAYBE_EVAL (!TARGET_64BIT && TARGET_80387
   && ix86_unary_operator_ok (NEG, DFmode, operands)) },
  { "TARGET_64BIT\n\
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)",
    MAYBE_EVAL (TARGET_64BIT
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)) },
  { "optimize_size",
    MAYBE_EVAL (optimize_size) },
  { "TARGET_80387 && FLOAT_MODE_P (GET_MODE (operands[1]))\n\
   && !SSE_FLOAT_MODE_P (GET_MODE (operands[1]))",
    MAYBE_EVAL (TARGET_80387 && FLOAT_MODE_P (GET_MODE (operands[1]))
   && !SSE_FLOAT_MODE_P (GET_MODE (operands[1]))) },
  { "(!TARGET_ZERO_EXTEND_WITH_AND || optimize_size) && reload_completed",
    MAYBE_EVAL ((!TARGET_ZERO_EXTEND_WITH_AND || optimize_size) && reload_completed) },
  { "optimize_size || TARGET_USE_CLTD",
    MAYBE_EVAL (optimize_size || TARGET_USE_CLTD) },
  { "reload_completed || !TARGET_SSE",
    MAYBE_EVAL (reload_completed || !TARGET_SSE) },
  { "TARGET_80387 && ix86_unary_operator_ok (ABS, SFmode, operands) && !TARGET_SSE",
    MAYBE_EVAL (TARGET_80387 && ix86_unary_operator_ok (ABS, SFmode, operands) && !TARGET_SSE) },
  { "!TARGET_64BIT && ix86_match_ccmode (insn, CCNOmode)",
    MAYBE_EVAL (!TARGET_64BIT && ix86_match_ccmode (insn, CCNOmode)) },
  { "TARGET_SSE\n\
   && (reload_in_progress || reload_completed\n\
       || (register_operand (operands[0], VOIDmode)\n\
	   && register_operand (operands[1], VOIDmode)))",
    MAYBE_EVAL (TARGET_SSE
   && (reload_in_progress || reload_completed
       || (register_operand (operands[0], VOIDmode)
	   && register_operand (operands[1], VOIDmode)))) },
  { "TARGET_80387 || (TARGET_SSE2 && TARGET_64BIT)",
    MAYBE_EVAL (TARGET_80387 || (TARGET_SSE2 && TARGET_64BIT)) },
  { "!TARGET_64BIT && ix86_match_ccmode (insn, CCmode)",
    MAYBE_EVAL (!TARGET_64BIT && ix86_match_ccmode (insn, CCmode)) },
  { "TARGET_64BIT && ix86_match_ccmode (insn, CCNOmode)\n\
   && ix86_binary_operator_ok (IOR, SImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_match_ccmode (insn, CCNOmode)
   && ix86_binary_operator_ok (IOR, SImode, operands)) },
  { "ix86_binary_operator_ok (MINUS, SImode, operands)",
    MAYBE_EVAL (ix86_binary_operator_ok (MINUS, SImode, operands)) },
  { "TARGET_64BIT && (TARGET_USE_CLTD || optimize_size)\n\
   && INTVAL (operands[2]) == 31\n\
   && ix86_binary_operator_ok (ASHIFTRT, SImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && (TARGET_USE_CLTD || optimize_size)
   && INTVAL (operands[2]) == 31
   && ix86_binary_operator_ok (ASHIFTRT, SImode, operands)) },
  { "(GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)\n\
    && ix86_match_ccmode (insn, CCmode)",
    MAYBE_EVAL ((GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)
    && ix86_match_ccmode (insn, CCmode)) },
  { "reload_completed\n\
   && GET_CODE (operands[1]) == MEM\n\
   && (GET_MODE (operands[0]) == XFmode || GET_MODE (operands[0]) == TFmode\n\
       || GET_MODE (operands[0]) == SFmode || GET_MODE (operands[0]) == DFmode)\n\
   && GET_CODE (XEXP (operands[1], 0)) == SYMBOL_REF\n\
   && CONSTANT_POOL_ADDRESS_P (XEXP (operands[1], 0))\n\
   && (!(SSE_REG_P (operands[0]) || \n\
	 (GET_CODE (operands[0]) == SUBREG\n\
	  && SSE_REG_P (SUBREG_REG (operands[0]))))\n\
       || standard_sse_constant_p (get_pool_constant (XEXP (operands[1], 0))))\n\
   && (!(FP_REG_P (operands[0]) || \n\
	 (GET_CODE (operands[0]) == SUBREG\n\
	  && FP_REG_P (SUBREG_REG (operands[0]))))\n\
       || standard_80387_constant_p (get_pool_constant (XEXP (operands[1], 0))))",
    MAYBE_EVAL (reload_completed
   && GET_CODE (operands[1]) == MEM
   && (GET_MODE (operands[0]) == XFmode || GET_MODE (operands[0]) == TFmode
       || GET_MODE (operands[0]) == SFmode || GET_MODE (operands[0]) == DFmode)
   && GET_CODE (XEXP (operands[1], 0)) == SYMBOL_REF
   && CONSTANT_POOL_ADDRESS_P (XEXP (operands[1], 0))
   && (!(SSE_REG_P (operands[0]) || 
	 (GET_CODE (operands[0]) == SUBREG
	  && SSE_REG_P (SUBREG_REG (operands[0]))))
       || standard_sse_constant_p (get_pool_constant (XEXP (operands[1], 0))))
   && (!(FP_REG_P (operands[0]) || 
	 (GET_CODE (operands[0]) == SUBREG
	  && FP_REG_P (SUBREG_REG (operands[0]))))
       || standard_80387_constant_p (get_pool_constant (XEXP (operands[1], 0))))) },
  { "TARGET_80387 && ix86_unary_operator_ok (ABS, TFmode, operands)",
    MAYBE_EVAL (TARGET_80387 && ix86_unary_operator_ok (ABS, TFmode, operands)) },
  { "ix86_binary_operator_ok (XOR, QImode, operands)",
    MAYBE_EVAL (ix86_binary_operator_ok (XOR, QImode, operands)) },
  { "ix86_match_ccmode (insn, CCGOCmode)\n\
   && (TARGET_SHIFT1 || optimize_size)\n\
   && ix86_binary_operator_ok (LSHIFTRT, QImode, operands)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCGOCmode)
   && (TARGET_SHIFT1 || optimize_size)
   && ix86_binary_operator_ok (LSHIFTRT, QImode, operands)) },
  { "ix86_match_ccmode (insn, CCmode)\n\
   && ix86_binary_operator_ok (MINUS, SImode, operands)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCmode)
   && ix86_binary_operator_ok (MINUS, SImode, operands)) },
  { "TARGET_64BIT && ix86_match_ccmode (insn, CCZmode)\n\
   && ix86_binary_operator_ok (PLUS, SImode, operands)\n\
   /* Current assemblers are broken and do not allow @GOTOFF in\n\
      ought but a memory context.  */\n\
   && ! pic_symbolic_operand (operands[2], VOIDmode)",
    MAYBE_EVAL (TARGET_64BIT && ix86_match_ccmode (insn, CCZmode)
   && ix86_binary_operator_ok (PLUS, SImode, operands)
   /* Current assemblers are broken and do not allow @GOTOFF in
      ought but a memory context.  */
   && ! pic_symbolic_operand (operands[2], VOIDmode)) },
  { "TARGET_80387 && reload_completed\n\
   && !FP_REG_P (operands[0]) && !FP_REG_P (operands[1])",
    MAYBE_EVAL (TARGET_80387 && reload_completed
   && !FP_REG_P (operands[0]) && !FP_REG_P (operands[1])) },
  { "ix86_match_ccmode (insn, CCZmode)\n\
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCZmode)
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)) },
  { "ix86_match_ccmode (insn, CCmode)\n\
   && ix86_binary_operator_ok (MINUS, QImode, operands)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCmode)
   && ix86_binary_operator_ok (MINUS, QImode, operands)) },
  { "ix86_match_ccmode (insn, CCGOCmode)\n\
   && ix86_binary_operator_ok (ASHIFT, SImode, operands)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCGOCmode)
   && ix86_binary_operator_ok (ASHIFT, SImode, operands)) },
  { "TARGET_PREFETCH_SSE || TARGET_3DNOW",
    MAYBE_EVAL (TARGET_PREFETCH_SSE || TARGET_3DNOW) },
  { "(!TARGET_PARTIAL_REG_STALL || optimize_size)",
    MAYBE_EVAL ((!TARGET_PARTIAL_REG_STALL || optimize_size)) },
  { "!TARGET_PARTIAL_REG_STALL\n\
   && ix86_binary_operator_ok (PLUS, HImode, operands)",
    MAYBE_EVAL (!TARGET_PARTIAL_REG_STALL
   && ix86_binary_operator_ok (PLUS, HImode, operands)) },
  { "reload_completed && TARGET_ZERO_EXTEND_WITH_AND && !optimize_size",
    MAYBE_EVAL (reload_completed && TARGET_ZERO_EXTEND_WITH_AND && !optimize_size) },
  { "TARGET_80387 || TARGET_SSE2",
    MAYBE_EVAL (TARGET_80387 || TARGET_SSE2) },
  { "(GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)\n\
   && !optimize_size\n\
   && (reload_in_progress || reload_completed\n\
       || GET_CODE (operands[1]) != CONST_DOUBLE\n\
       || (ix86_cmodel == CM_MEDIUM || ix86_cmodel == CM_LARGE)\n\
       || memory_operand (operands[0], TFmode))",
    MAYBE_EVAL ((GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)
   && !optimize_size
   && (reload_in_progress || reload_completed
       || GET_CODE (operands[1]) != CONST_DOUBLE
       || (ix86_cmodel == CM_MEDIUM || ix86_cmodel == CM_LARGE)
       || memory_operand (operands[0], TFmode))) },
  { "TARGET_64BIT && ix86_binary_operator_ok (ROTATE, SImode, operands)\n\
   && (TARGET_SHIFT1 || optimize_size)",
    MAYBE_EVAL (TARGET_64BIT && ix86_binary_operator_ok (ROTATE, SImode, operands)
   && (TARGET_SHIFT1 || optimize_size)) },
  { "TARGET_80387 && TARGET_USE_FIOP",
    MAYBE_EVAL (TARGET_80387 && TARGET_USE_FIOP) },
  { "ix86_match_ccmode (insn, CCGOCmode)\n\
   && ix86_binary_operator_ok (ASHIFTRT, HImode, operands)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCGOCmode)
   && ix86_binary_operator_ok (ASHIFTRT, HImode, operands)) },
  { "TARGET_3DNOW_A",
    MAYBE_EVAL (TARGET_3DNOW_A) },
  { "TARGET_ZERO_EXTEND_WITH_AND && !optimize_size",
    MAYBE_EVAL (TARGET_ZERO_EXTEND_WITH_AND && !optimize_size) },
  { "ix86_match_ccmode (insn, CCNOmode)\n\
   && (true_regnum (operands[0]) != 0\n\
       || (GET_CODE (operands[1]) == CONST_INT\n\
	   && CONST_OK_FOR_LETTER_P (INTVAL (operands[1]), 'K')))\n\
   && find_regno_note (insn, REG_DEAD, true_regnum (operands[0]))",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCNOmode)
   && (true_regnum (operands[0]) != 0
       || (GET_CODE (operands[1]) == CONST_INT
	   && CONST_OK_FOR_LETTER_P (INTVAL (operands[1]), 'K')))
   && find_regno_note (insn, REG_DEAD, true_regnum (operands[0]))) },
  { "reload_completed\n\
    && ANY_QI_REG_P (operands[0])\n\
    && (!TARGET_PARTIAL_REG_STALL || optimize_size)\n\
    && !(~INTVAL (operands[2]) & ~255)\n\
    && !(INTVAL (operands[2]) & 128)\n\
    && GET_MODE (operands[0]) != QImode",
    MAYBE_EVAL (reload_completed
    && ANY_QI_REG_P (operands[0])
    && (!TARGET_PARTIAL_REG_STALL || optimize_size)
    && !(~INTVAL (operands[2]) & ~255)
    && !(INTVAL (operands[2]) & 128)
    && GET_MODE (operands[0]) != QImode) },
  { "!TARGET_PARTIAL_REG_STALL\n\
   && ix86_binary_operator_ok (ASHIFT, QImode, operands)",
    MAYBE_EVAL (!TARGET_PARTIAL_REG_STALL
   && ix86_binary_operator_ok (ASHIFT, QImode, operands)) },
  { "ix86_binary_operator_ok (ROTATERT, SImode, operands)\n\
   && (TARGET_SHIFT1 || optimize_size)",
    MAYBE_EVAL (ix86_binary_operator_ok (ROTATERT, SImode, operands)
   && (TARGET_SHIFT1 || optimize_size)) },
  { "TARGET_80387\n\
   && FLOAT_MODE_P (GET_MODE (operands[1]))\n\
   && GET_MODE (operands[1]) == GET_MODE (operands[2])\n\
   && ix86_fp_jump_nontrivial_p (GET_CODE (operands[0]))",
    MAYBE_EVAL (TARGET_80387
   && FLOAT_MODE_P (GET_MODE (operands[1]))
   && GET_MODE (operands[1]) == GET_MODE (operands[2])
   && ix86_fp_jump_nontrivial_p (GET_CODE (operands[0]))) },
  { "ix86_binary_operator_ok (ASHIFTRT, QImode, operands)",
    MAYBE_EVAL (ix86_binary_operator_ok (ASHIFTRT, QImode, operands)) },
  { "reload_completed \n\
   && (!TARGET_ZERO_EXTEND_WITH_AND || optimize_size)\n\
   && (!REG_P (operands[1]) || ANY_QI_REG_P (operands[1]))",
    MAYBE_EVAL (reload_completed 
   && (!TARGET_ZERO_EXTEND_WITH_AND || optimize_size)
   && (!REG_P (operands[1]) || ANY_QI_REG_P (operands[1]))) },
  { "ix86_match_ccmode (insn, CCGOCmode)\n\
   && ix86_binary_operator_ok (PLUS, QImode, operands)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCGOCmode)
   && ix86_binary_operator_ok (PLUS, QImode, operands)) },
  { "TARGET_64BIT && ix86_binary_operator_ok (LSHIFTRT, HImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_binary_operator_ok (LSHIFTRT, HImode, operands)) },
  { "TARGET_64BIT && ix86_binary_operator_ok (ASHIFTRT, DImode, operands)\n\
   && (TARGET_SHIFT1 || optimize_size)",
    MAYBE_EVAL (TARGET_64BIT && ix86_binary_operator_ok (ASHIFTRT, DImode, operands)
   && (TARGET_SHIFT1 || optimize_size)) },
  { "ix86_match_ccmode (insn, CCGOCmode)\n\
   && ix86_binary_operator_ok (ASHIFTRT, QImode, operands)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCGOCmode)
   && ix86_binary_operator_ok (ASHIFTRT, QImode, operands)) },
  { "TARGET_64BIT && ix86_binary_operator_ok (ASHIFT, SImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_binary_operator_ok (ASHIFT, SImode, operands)) },
  { "TARGET_64BIT",
    MAYBE_EVAL (TARGET_64BIT) },
  { "TARGET_64BIT && TARGET_80387 && (!TARGET_SSE || TARGET_MIX_SSE_I387)",
    MAYBE_EVAL (TARGET_64BIT && TARGET_80387 && (!TARGET_SSE || TARGET_MIX_SSE_I387)) },
  { "!TARGET_64BIT && TARGET_80387\n\
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)",
    MAYBE_EVAL (!TARGET_64BIT && TARGET_80387
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)) },
  { "!TARGET_ZERO_EXTEND_WITH_AND || optimize_size",
    MAYBE_EVAL (!TARGET_ZERO_EXTEND_WITH_AND || optimize_size) },
  { "ix86_unary_operator_ok (NOT, SImode, operands)",
    MAYBE_EVAL (ix86_unary_operator_ok (NOT, SImode, operands)) },
  { "SSE_FLOAT_MODE_P (GET_MODE (operands[0]))\n\
   && GET_MODE (operands[0]) == GET_MODE (operands[1])",
    MAYBE_EVAL (SSE_FLOAT_MODE_P (GET_MODE (operands[0]))
   && GET_MODE (operands[0]) == GET_MODE (operands[1])) },
  { "TARGET_MMX",
    MAYBE_EVAL (TARGET_MMX) },
  { "TARGET_80387 && FLOAT_MODE_P (GET_MODE (operands[1]))\n\
   && (!SSE_FLOAT_MODE_P (GET_MODE (operands[1])) || !TARGET_64BIT)",
    MAYBE_EVAL (TARGET_80387 && FLOAT_MODE_P (GET_MODE (operands[1]))
   && (!SSE_FLOAT_MODE_P (GET_MODE (operands[1])) || !TARGET_64BIT)) },
  { "TARGET_3DNOW && !TARGET_64BIT",
    MAYBE_EVAL (TARGET_3DNOW && !TARGET_64BIT) },
  { "0 && TARGET_80387 && reload_completed",
    MAYBE_EVAL (0 && TARGET_80387 && reload_completed) },
  { "!TARGET_64BIT && !optimize_size",
    MAYBE_EVAL (!TARGET_64BIT && !optimize_size) },
  { "TARGET_64BIT && ix86_unary_operator_ok (NOT, SImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_unary_operator_ok (NOT, SImode, operands)) },
  { "! TARGET_NO_FANCY_MATH_387 && TARGET_80387\n\
   && !TARGET_SSE_MATH",
    MAYBE_EVAL (! TARGET_NO_FANCY_MATH_387 && TARGET_80387
   && !TARGET_SSE_MATH) },
  { "!TARGET_64BIT && TARGET_SSE2\n\
   && (reload_in_progress || reload_completed\n\
       || (register_operand (operands[0], VOIDmode)\n\
	   && register_operand (operands[1], VOIDmode)))",
    MAYBE_EVAL (!TARGET_64BIT && TARGET_SSE2
   && (reload_in_progress || reload_completed
       || (register_operand (operands[0], VOIDmode)
	   && register_operand (operands[1], VOIDmode)))) },
  { "TARGET_64BIT && ix86_binary_operator_ok (PLUS, DImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_binary_operator_ok (PLUS, DImode, operands)) },
  { "!optimize_size\n\
   && peep2_regno_dead_p (0, FLAGS_REG)\n\
   && ((TARGET_PENTIUM \n\
        && (GET_CODE (operands[0]) != MEM\n\
            || !memory_displacement_operand (operands[0], SImode)))\n\
       || (TARGET_K6 && long_memory_operand (operands[0], SImode)))",
    MAYBE_EVAL (!optimize_size
   && peep2_regno_dead_p (0, FLAGS_REG)
   && ((TARGET_PENTIUM 
        && (GET_CODE (operands[0]) != MEM
            || !memory_displacement_operand (operands[0], SImode)))
       || (TARGET_K6 && long_memory_operand (operands[0], SImode)))) },
  { "TARGET_80387 && TARGET_SSE2\n\
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)",
    MAYBE_EVAL (TARGET_80387 && TARGET_SSE2
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)) },
  { "TARGET_80387 && (!TARGET_SSE2 || !TARGET_SSE_MATH)\n\
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)",
    MAYBE_EVAL (TARGET_80387 && (!TARGET_SSE2 || !TARGET_SSE_MATH)
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)) },
  { "TARGET_PARTIAL_REG_STALL",
    MAYBE_EVAL (TARGET_PARTIAL_REG_STALL) },
  { "!TARGET_64BIT\n\
   && !optimize_size\n\
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)\n\
   && (reload_in_progress || reload_completed\n\
       || GET_CODE (operands[1]) != CONST_DOUBLE\n\
       || memory_operand (operands[0], XFmode))",
    MAYBE_EVAL (!TARGET_64BIT
   && !optimize_size
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)
   && (reload_in_progress || reload_completed
       || GET_CODE (operands[1]) != CONST_DOUBLE
       || memory_operand (operands[0], XFmode))) },
  { "TARGET_64BIT && ix86_unary_operator_ok (NEG, SImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_unary_operator_ok (NEG, SImode, operands)) },
  { "ix86_binary_operator_ok (IOR, HImode, operands)",
    MAYBE_EVAL (ix86_binary_operator_ok (IOR, HImode, operands)) },
  { "!TARGET_64BIT && TARGET_80387\n\
   && ix86_unary_operator_ok (NEG, XFmode, operands)",
    MAYBE_EVAL (!TARGET_64BIT && TARGET_80387
   && ix86_unary_operator_ok (NEG, XFmode, operands)) },
  { "!TARGET_64BIT && TARGET_80387",
    MAYBE_EVAL (!TARGET_64BIT && TARGET_80387) },
  { "GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM",
    MAYBE_EVAL (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM) },
  { "ix86_binary_operator_ok (LSHIFTRT, HImode, operands)",
    MAYBE_EVAL (ix86_binary_operator_ok (LSHIFTRT, HImode, operands)) },
  { "ix86_match_ccmode (insn, CCNOmode)\n\
   && ix86_binary_operator_ok (XOR, QImode, operands)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCNOmode)
   && ix86_binary_operator_ok (XOR, QImode, operands)) },
  { "TARGET_80387 && (!TARGET_SSE2 || TARGET_MIX_SSE_I387)",
    MAYBE_EVAL (TARGET_80387 && (!TARGET_SSE2 || TARGET_MIX_SSE_I387)) },
  { "ix86_match_ccmode (insn, CCNOmode)\n\
   && ix86_binary_operator_ok (IOR, QImode, operands)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCNOmode)
   && ix86_binary_operator_ok (IOR, QImode, operands)) },
  { "!TARGET_64BIT && ix86_binary_operator_ok (PLUS, DImode, operands)",
    MAYBE_EVAL (!TARGET_64BIT && ix86_binary_operator_ok (PLUS, DImode, operands)) },
  { "TARGET_64BIT && !optimize_size && !TARGET_USE_CLTD",
    MAYBE_EVAL (TARGET_64BIT && !optimize_size && !TARGET_USE_CLTD) },
  { "TARGET_3DNOW",
    MAYBE_EVAL (TARGET_3DNOW) },
  { "TARGET_64BIT && ix86_match_ccmode (insn, CCmode)\n\
   && (TARGET_SHIFT1 || optimize_size)\n\
   && ix86_binary_operator_ok (ASHIFTRT, SImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_match_ccmode (insn, CCmode)
   && (TARGET_SHIFT1 || optimize_size)
   && ix86_binary_operator_ok (ASHIFTRT, SImode, operands)) },
  { "peep2_regno_dead_p (0, FLAGS_REG) && REGNO (operands[0]) == REGNO (operands[1])",
    MAYBE_EVAL (peep2_regno_dead_p (0, FLAGS_REG) && REGNO (operands[0]) == REGNO (operands[1])) },
  { "TARGET_CMOVE && TARGET_HIMODE_MATH",
    MAYBE_EVAL (TARGET_CMOVE && TARGET_HIMODE_MATH) },
  { "TARGET_80387 && ix86_unary_operator_ok (NEG, TFmode, operands)",
    MAYBE_EVAL (TARGET_80387 && ix86_unary_operator_ok (NEG, TFmode, operands)) },
  { "(GET_MODE (operands[0]) == QImode || GET_MODE (operands[0]) == HImode\n\
    || (TARGET_64BIT && GET_MODE (operands[0]) == SImode))\n\
   && (!TARGET_PARTIAL_REG_STALL || optimize_size)\n\
   && GET_MODE (operands[0]) == GET_MODE (operands[1])\n\
   && GET_MODE (operands[0]) == GET_MODE (operands[3])&& reload_completed",
    MAYBE_EVAL ((GET_MODE (operands[0]) == QImode || GET_MODE (operands[0]) == HImode
    || (TARGET_64BIT && GET_MODE (operands[0]) == SImode))
   && (!TARGET_PARTIAL_REG_STALL || optimize_size)
   && GET_MODE (operands[0]) == GET_MODE (operands[1])
   && GET_MODE (operands[0]) == GET_MODE (operands[3])&& reload_completed) },
  { "(GET_MODE (operands[0]) == QImode || GET_MODE (operands[0]) == HImode\n\
    || (TARGET_64BIT && GET_MODE (operands[0]) == SImode))\n\
   && (!TARGET_PARTIAL_REG_STALL || optimize_size)\n\
   && GET_MODE (operands[0]) == GET_MODE (operands[1])\n\
   && GET_MODE (operands[0]) == GET_MODE (operands[2])\n\
   && (GET_MODE (operands[0]) == GET_MODE (operands[3])\n\
       || GET_MODE (operands[3]) == VOIDmode)&& reload_completed",
    MAYBE_EVAL ((GET_MODE (operands[0]) == QImode || GET_MODE (operands[0]) == HImode
    || (TARGET_64BIT && GET_MODE (operands[0]) == SImode))
   && (!TARGET_PARTIAL_REG_STALL || optimize_size)
   && GET_MODE (operands[0]) == GET_MODE (operands[1])
   && GET_MODE (operands[0]) == GET_MODE (operands[2])
   && (GET_MODE (operands[0]) == GET_MODE (operands[3])
       || GET_MODE (operands[3]) == VOIDmode)&& reload_completed) },
  { "ix86_unary_operator_ok (NEG, HImode, operands)",
    MAYBE_EVAL (ix86_unary_operator_ok (NEG, HImode, operands)) },
  { "ix86_binary_operator_ok (LSHIFTRT, QImode, operands)\n\
   && (TARGET_SHIFT1 || optimize_size)",
    MAYBE_EVAL (ix86_binary_operator_ok (LSHIFTRT, QImode, operands)
   && (TARGET_SHIFT1 || optimize_size)) },
  { "TARGET_64BIT && ix86_match_ccmode (insn, CCmode)",
    MAYBE_EVAL (TARGET_64BIT && ix86_match_ccmode (insn, CCmode)) },
  { "TARGET_80387 && (!TARGET_SSE || TARGET_MIX_SSE_I387)",
    MAYBE_EVAL (TARGET_80387 && (!TARGET_SSE || TARGET_MIX_SSE_I387)) },
  { "TARGET_SSE\n\
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)",
    MAYBE_EVAL (TARGET_SSE
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)) },
  { "!TARGET_64BIT && ix86_binary_operator_ok (MINUS, DImode, operands)",
    MAYBE_EVAL (!TARGET_64BIT && ix86_binary_operator_ok (MINUS, DImode, operands)) },
  { "SSE_REG_P (operands[0]) && reload_completed",
    MAYBE_EVAL (SSE_REG_P (operands[0]) && reload_completed) },
  { "TARGET_64BIT\n\
   && ix86_binary_operator_ok (XOR, DImode, operands)",
    MAYBE_EVAL (TARGET_64BIT
   && ix86_binary_operator_ok (XOR, DImode, operands)) },
  { "TARGET_SSE && reload_completed",
    MAYBE_EVAL (TARGET_SSE && reload_completed) },
  { "TARGET_80387\n\
   && FLOAT_MODE_P (GET_MODE (operands[0]))\n\
   && GET_MODE (operands[0]) == GET_MODE (operands[1])",
    MAYBE_EVAL (TARGET_80387
   && FLOAT_MODE_P (GET_MODE (operands[0]))
   && GET_MODE (operands[0]) == GET_MODE (operands[1])) },
  { "TARGET_64BIT && ix86_match_ccmode (insn, CCNOmode)\n\
   && ix86_unary_operator_ok (NOT, SImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_match_ccmode (insn, CCNOmode)
   && ix86_unary_operator_ok (NOT, SImode, operands)) },
  { "! TARGET_PARTIAL_REG_STALL\n\
   && ix86_match_ccmode (insn, CCNOmode)\n\
   && true_regnum (operands[0]) != 0\n\
   && find_regno_note (insn, REG_DEAD, true_regnum (operands[0]))",
    MAYBE_EVAL (! TARGET_PARTIAL_REG_STALL
   && ix86_match_ccmode (insn, CCNOmode)
   && true_regnum (operands[0]) != 0
   && find_regno_note (insn, REG_DEAD, true_regnum (operands[0]))) },
  { "(GET_MODE (operands[0]) == QImode || GET_MODE (operands[0]) == HImode\n\
    || (TARGET_64BIT && GET_MODE (operands[0]) == SImode))\n\
   && (!TARGET_PARTIAL_REG_STALL || optimize_size)\n\
   && GET_MODE (operands[0]) == GET_MODE (operands[1])\n\
   && GET_MODE (operands[0]) == GET_MODE (operands[2])\n\
   && (GET_MODE (operands[0]) == GET_MODE (operands[3])\n\
       || GET_MODE (operands[3]) == VOIDmode)",
    MAYBE_EVAL ((GET_MODE (operands[0]) == QImode || GET_MODE (operands[0]) == HImode
    || (TARGET_64BIT && GET_MODE (operands[0]) == SImode))
   && (!TARGET_PARTIAL_REG_STALL || optimize_size)
   && GET_MODE (operands[0]) == GET_MODE (operands[1])
   && GET_MODE (operands[0]) == GET_MODE (operands[2])
   && (GET_MODE (operands[0]) == GET_MODE (operands[3])
       || GET_MODE (operands[3]) == VOIDmode)) },
  { "ix86_binary_operator_ok (ROTATERT, QImode, operands)",
    MAYBE_EVAL (ix86_binary_operator_ok (ROTATERT, QImode, operands)) },
  { "TARGET_64BIT && ix86_match_ccmode (insn, CCGOCmode)\n\
   && ix86_binary_operator_ok (MINUS, DImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_match_ccmode (insn, CCGOCmode)
   && ix86_binary_operator_ok (MINUS, DImode, operands)) },
  { "TARGET_80387 && FLOAT_MODE_P (GET_MODE (operands[1]))\n\
   && !reload_completed && !reload_in_progress\n\
   && (!SSE_FLOAT_MODE_P (GET_MODE (operands[1])) || !TARGET_64BIT)&& 1",
    MAYBE_EVAL (TARGET_80387 && FLOAT_MODE_P (GET_MODE (operands[1]))
   && !reload_completed && !reload_in_progress
   && (!SSE_FLOAT_MODE_P (GET_MODE (operands[1])) || !TARGET_64BIT)&& 1) },
  { "TARGET_64BIT && !symbolic_operand (operands[1], DImode)\n\
   && !x86_64_immediate_operand (operands[1], DImode) && 1",
    MAYBE_EVAL (TARGET_64BIT && !symbolic_operand (operands[1], DImode)
   && !x86_64_immediate_operand (operands[1], DImode) && 1) },
  { "TARGET_64BIT && ix86_match_ccmode (insn, CCGOCmode)\n\
   && (TARGET_SHIFT1 || optimize_size)\n\
   && ix86_binary_operator_ok (LSHIFTRT, HImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_match_ccmode (insn, CCGOCmode)
   && (TARGET_SHIFT1 || optimize_size)
   && ix86_binary_operator_ok (LSHIFTRT, HImode, operands)) },
  { "TARGET_64BIT || TARGET_INTEGER_DFMODE_MOVES",
    MAYBE_EVAL (TARGET_64BIT || TARGET_INTEGER_DFMODE_MOVES) },
  { "ix86_binary_operator_ok (ROTATE, SImode, operands)",
    MAYBE_EVAL (ix86_binary_operator_ok (ROTATE, SImode, operands)) },
  { "TARGET_64BIT && ix86_binary_operator_ok (ASHIFTRT, DImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_binary_operator_ok (ASHIFTRT, DImode, operands)) },
  { "TARGET_64BIT && ix86_check_movabs (insn, 0)",
    MAYBE_EVAL (TARGET_64BIT && ix86_check_movabs (insn, 0)) },
  { "!TARGET_64BIT && TARGET_CMOVE && reload_completed",
    MAYBE_EVAL (!TARGET_64BIT && TARGET_CMOVE && reload_completed) },
  { "TARGET_80387 && FLOAT_MODE_P (GET_MODE (operands[1]))\n\
   && !reload_completed && !reload_in_progress\n\
   && !SSE_FLOAT_MODE_P (GET_MODE (operands[1]))",
    MAYBE_EVAL (TARGET_80387 && FLOAT_MODE_P (GET_MODE (operands[1]))
   && !reload_completed && !reload_in_progress
   && !SSE_FLOAT_MODE_P (GET_MODE (operands[1]))) },
  { "TARGET_80387 && !TARGET_SSE_MATH\n\
   && GET_RTX_CLASS (GET_CODE (operands[3])) == 'c'\n\
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)",
    MAYBE_EVAL (TARGET_80387 && !TARGET_SSE_MATH
   && GET_RTX_CLASS (GET_CODE (operands[3])) == 'c'
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)) },
  { "TARGET_80387 && TARGET_SSE2 && TARGET_SSE_MATH && TARGET_MIX_SSE_I387\n\
   && GET_RTX_CLASS (GET_CODE (operands[3])) != 'c'\n\
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)",
    MAYBE_EVAL (TARGET_80387 && TARGET_SSE2 && TARGET_SSE_MATH && TARGET_MIX_SSE_I387
   && GET_RTX_CLASS (GET_CODE (operands[3])) != 'c'
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)) },
  { "! TARGET_PARTIAL_REG_STALL && reload_completed\n\
   /* Ensure that the operand will remain sign-extended immediate.  */\n\
   && ix86_match_ccmode (insn, INTVAL (operands[2]) >= 0 ? CCNOmode : CCZmode)\n\
   && ! optimize_size\n\
   && ((GET_MODE (operands[0]) == HImode && ! TARGET_FAST_PREFIX)\n\
       || (GET_MODE (operands[0]) == QImode && TARGET_PROMOTE_QImode))",
    MAYBE_EVAL (! TARGET_PARTIAL_REG_STALL && reload_completed
   /* Ensure that the operand will remain sign-extended immediate.  */
   && ix86_match_ccmode (insn, INTVAL (operands[2]) >= 0 ? CCNOmode : CCZmode)
   && ! optimize_size
   && ((GET_MODE (operands[0]) == HImode && ! TARGET_FAST_PREFIX)
       || (GET_MODE (operands[0]) == QImode && TARGET_PROMOTE_QImode))) },
  { "ix86_match_ccmode (insn, CCGCmode)\n\
   && (INTVAL (operands[2]) & 0xffffffff) != 0x80000000",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCGCmode)
   && (INTVAL (operands[2]) & 0xffffffff) != 0x80000000) },
  { "TARGET_PREFETCH_SSE && !TARGET_64BIT",
    MAYBE_EVAL (TARGET_PREFETCH_SSE && !TARGET_64BIT) },
  { "TARGET_64BIT\n\
   && INTVAL (operands[4]) + SSE_REGPARM_MAX * 16 - 16 < 128\n\
   && INTVAL (operands[4]) + INTVAL (operands[2]) * 16 >= -128",
    MAYBE_EVAL (TARGET_64BIT
   && INTVAL (operands[4]) + SSE_REGPARM_MAX * 16 - 16 < 128
   && INTVAL (operands[4]) + INTVAL (operands[2]) * 16 >= -128) },
  { "reload_completed\n\
   && ANY_QI_REG_P (operands[0])\n\
   && (TARGET_ZERO_EXTEND_WITH_AND && !optimize_size)\n\
   && !reg_overlap_mentioned_p (operands[0], operands[1])",
    MAYBE_EVAL (reload_completed
   && ANY_QI_REG_P (operands[0])
   && (TARGET_ZERO_EXTEND_WITH_AND && !optimize_size)
   && !reg_overlap_mentioned_p (operands[0], operands[1])) },
  { "reload_completed\n\
    && QI_REG_P (operands[0])\n\
    && (!TARGET_PARTIAL_REG_STALL || optimize_size)\n\
    && !(INTVAL (operands[2]) & ~(255 << 8))\n\
    && GET_MODE (operands[0]) != QImode",
    MAYBE_EVAL (reload_completed
    && QI_REG_P (operands[0])
    && (!TARGET_PARTIAL_REG_STALL || optimize_size)
    && !(INTVAL (operands[2]) & ~(255 << 8))
    && GET_MODE (operands[0]) != QImode) },
  { "GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM",
    MAYBE_EVAL (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM) },
  { "TARGET_64BIT && ix86_binary_operator_ok (AND, SImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_binary_operator_ok (AND, SImode, operands)) },
  { "ix86_unary_operator_ok (NOT, HImode, operands)",
    MAYBE_EVAL (ix86_unary_operator_ok (NOT, HImode, operands)) },
  { "! TARGET_PARTIAL_REG_STALL",
    MAYBE_EVAL (! TARGET_PARTIAL_REG_STALL) },
  { "TARGET_64BIT\n\
   && ix86_match_ccmode (insn, CCGOCmode)\n\
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)\n\
   /* Current assemblers are broken and do not allow @GOTOFF in\n\
      ought but a memory context.  */\n\
   && ! pic_symbolic_operand (operands[2], VOIDmode)",
    MAYBE_EVAL (TARGET_64BIT
   && ix86_match_ccmode (insn, CCGOCmode)
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)
   /* Current assemblers are broken and do not allow @GOTOFF in
      ought but a memory context.  */
   && ! pic_symbolic_operand (operands[2], VOIDmode)) },
  { "TARGET_80387\n\
   && SSE_FLOAT_MODE_P (GET_MODE (operands[0]))\n\
   && GET_MODE (operands[0]) == GET_MODE (operands[1])",
    MAYBE_EVAL (TARGET_80387
   && SSE_FLOAT_MODE_P (GET_MODE (operands[0]))
   && GET_MODE (operands[0]) == GET_MODE (operands[1])) },
  { "TARGET_80387 && TARGET_SSE_MATH && TARGET_SSE2 && TARGET_MIX_SSE_I387\n\
   && GET_RTX_CLASS (GET_CODE (operands[3])) == 'c'\n\
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)",
    MAYBE_EVAL (TARGET_80387 && TARGET_SSE_MATH && TARGET_SSE2 && TARGET_MIX_SSE_I387
   && GET_RTX_CLASS (GET_CODE (operands[3])) == 'c'
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)) },
  { "reload_completed\n\
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)\n\
   && (GET_MODE (operands[0]) == XFmode || GET_MODE (operands[0]) == TFmode)\n\
   && ! (ANY_FP_REG_P (operands[0]) || \n\
	 (GET_CODE (operands[0]) == SUBREG\n\
	  && ANY_FP_REG_P (SUBREG_REG (operands[0]))))\n\
   && ! (ANY_FP_REG_P (operands[1]) || \n\
	 (GET_CODE (operands[1]) == SUBREG\n\
	  && ANY_FP_REG_P (SUBREG_REG (operands[1]))))",
    MAYBE_EVAL (reload_completed
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)
   && (GET_MODE (operands[0]) == XFmode || GET_MODE (operands[0]) == TFmode)
   && ! (ANY_FP_REG_P (operands[0]) || 
	 (GET_CODE (operands[0]) == SUBREG
	  && ANY_FP_REG_P (SUBREG_REG (operands[0]))))
   && ! (ANY_FP_REG_P (operands[1]) || 
	 (GET_CODE (operands[1]) == SUBREG
	  && ANY_FP_REG_P (SUBREG_REG (operands[1]))))) },
  { "(! TARGET_PARTIAL_REG_STALL || optimize_size)\n\
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)",
    MAYBE_EVAL ((! TARGET_PARTIAL_REG_STALL || optimize_size)
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)) },
  { "TARGET_80387\n\
   && SSE_FLOAT_MODE_P (GET_MODE (operands[0]))\n\
   && GET_MODE (operands[0]) == GET_MODE (operands[0])",
    MAYBE_EVAL (TARGET_80387
   && SSE_FLOAT_MODE_P (GET_MODE (operands[0]))
   && GET_MODE (operands[0]) == GET_MODE (operands[0])) },
  { "TARGET_80387 && reload_completed && FLOAT_MODE_P (GET_MODE (operands[0]))",
    MAYBE_EVAL (TARGET_80387 && reload_completed && FLOAT_MODE_P (GET_MODE (operands[0]))) },
  { "TARGET_80387 && (!TARGET_SSE2 || !TARGET_SSE_MATH)\n\
   && GET_RTX_CLASS (GET_CODE (operands[3])) != 'c'\n\
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)",
    MAYBE_EVAL (TARGET_80387 && (!TARGET_SSE2 || !TARGET_SSE_MATH)
   && GET_RTX_CLASS (GET_CODE (operands[3])) != 'c'
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)) },
  { "ix86_match_ccmode (insn, CCNOmode)\n\
   && ix86_binary_operator_ok (AND, HImode, operands)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCNOmode)
   && ix86_binary_operator_ok (AND, HImode, operands)) },
  { "TARGET_64BIT && TARGET_80387\n\
   && ix86_unary_operator_ok (ABS, DFmode, operands)",
    MAYBE_EVAL (TARGET_64BIT && TARGET_80387
   && ix86_unary_operator_ok (ABS, DFmode, operands)) },
  { "TARGET_PREFETCH_SSE && TARGET_64BIT",
    MAYBE_EVAL (TARGET_PREFETCH_SSE && TARGET_64BIT) },
  { "TARGET_80387 || (TARGET_SSE && TARGET_64BIT)",
    MAYBE_EVAL (TARGET_80387 || (TARGET_SSE && TARGET_64BIT)) },
  { "TARGET_64BIT\n\
   && ix86_match_ccmode (insn, CCNOmode)\n\
   && ix86_binary_operator_ok (XOR, DImode, operands)",
    MAYBE_EVAL (TARGET_64BIT
   && ix86_match_ccmode (insn, CCNOmode)
   && ix86_binary_operator_ok (XOR, DImode, operands)) },
  { "(reload_completed\n\
    && dead_or_set_p (insn, operands[1])\n\
    && !reg_mentioned_p (operands[1], operands[0]))",
    MAYBE_EVAL ((reload_completed
    && dead_or_set_p (insn, operands[1])
    && !reg_mentioned_p (operands[1], operands[0]))) },
  { "! optimize_size && get_attr_length (insn) >= ix86_cost->large_insn\n\
  && TARGET_SPLIT_LONG_MOVES",
    MAYBE_EVAL (! optimize_size && get_attr_length (insn) >= ix86_cost->large_insn
  && TARGET_SPLIT_LONG_MOVES) },
  { "reload_completed && GET_CODE (operands[1]) == CONST_INT\n\
   && INTVAL (operands[1]) == -1\n\
   && (TARGET_PENTIUM || optimize_size)",
    MAYBE_EVAL (reload_completed && GET_CODE (operands[1]) == CONST_INT
   && INTVAL (operands[1]) == -1
   && (TARGET_PENTIUM || optimize_size)) },
  { "TARGET_64BIT && reload_completed && GENERAL_REG_P (operands[0])",
    MAYBE_EVAL (TARGET_64BIT && reload_completed && GENERAL_REG_P (operands[0])) },
  { "exact_log2 (INTVAL (operands[2])) >= 0\n\
   && REGNO (operands[0]) == REGNO (operands[1])\n\
   && peep2_regno_dead_p (0, FLAGS_REG)",
    MAYBE_EVAL (exact_log2 (INTVAL (operands[2])) >= 0
   && REGNO (operands[0]) == REGNO (operands[1])
   && peep2_regno_dead_p (0, FLAGS_REG)) },
  { "SSE_FLOAT_MODE_P (GET_MODE (operands[1]))\n\
   && GET_MODE (operands[1]) == GET_MODE (operands[2])\n\
   && ix86_fp_jump_nontrivial_p (GET_CODE (operands[0]))",
    MAYBE_EVAL (SSE_FLOAT_MODE_P (GET_MODE (operands[1]))
   && GET_MODE (operands[1]) == GET_MODE (operands[2])
   && ix86_fp_jump_nontrivial_p (GET_CODE (operands[0]))) },
  { "ix86_binary_operator_ok (PLUS, SImode, operands)",
    MAYBE_EVAL (ix86_binary_operator_ok (PLUS, SImode, operands)) },
  { "ix86_binary_operator_ok (ASHIFTRT, HImode, operands)\n\
   && (TARGET_SHIFT1 || optimize_size)",
    MAYBE_EVAL (ix86_binary_operator_ok (ASHIFTRT, HImode, operands)
   && (TARGET_SHIFT1 || optimize_size)) },
  { "(GET_MODE (operands[0]) == QImode || GET_MODE (operands[0]) == HImode\n\
    || (TARGET_64BIT && GET_MODE (operands[0]) == SImode))\n\
   && (!TARGET_PARTIAL_REG_STALL || optimize_size)\n\
   && GET_MODE (operands[0]) == GET_MODE (operands[1])\n\
   && (GET_MODE (operands[0]) == GET_MODE (operands[3])\n\
       || GET_MODE (operands[3]) == VOIDmode)&& reload_completed",
    MAYBE_EVAL ((GET_MODE (operands[0]) == QImode || GET_MODE (operands[0]) == HImode
    || (TARGET_64BIT && GET_MODE (operands[0]) == SImode))
   && (!TARGET_PARTIAL_REG_STALL || optimize_size)
   && GET_MODE (operands[0]) == GET_MODE (operands[1])
   && (GET_MODE (operands[0]) == GET_MODE (operands[3])
       || GET_MODE (operands[3]) == VOIDmode)&& reload_completed) },
  { "(TARGET_64BIT && TARGET_SSE) || TARGET_80387",
    MAYBE_EVAL ((TARGET_64BIT && TARGET_SSE) || TARGET_80387) },
  { "(! TARGET_NO_FANCY_MATH_387 && TARGET_80387) || TARGET_SSE_MATH",
    MAYBE_EVAL ((! TARGET_NO_FANCY_MATH_387 && TARGET_80387) || TARGET_SSE_MATH) },
  { "reload_completed\n\
   && (GET_MODE (operands[0]) == XFmode\n\
       || GET_MODE (operands[0]) == TFmode\n\
       || GET_MODE (operands[0]) == DFmode)\n\
   && !ANY_FP_REG_P (operands[1])",
    MAYBE_EVAL (reload_completed
   && (GET_MODE (operands[0]) == XFmode
       || GET_MODE (operands[0]) == TFmode
       || GET_MODE (operands[0]) == DFmode)
   && !ANY_FP_REG_P (operands[1])) },
  { "ix86_match_ccmode (insn, CCGOCmode)\n\
   && ix86_binary_operator_ok (MINUS, HImode, operands)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCGOCmode)
   && ix86_binary_operator_ok (MINUS, HImode, operands)) },
  { "ix86_binary_operator_ok (ROTATE, QImode, operands)\n\
   && (TARGET_SHIFT1 || optimize_size)",
    MAYBE_EVAL (ix86_binary_operator_ok (ROTATE, QImode, operands)
   && (TARGET_SHIFT1 || optimize_size)) },
  { "TARGET_64BIT && ix86_match_ccmode (insn, CCGOCmode)\n\
   && ix86_binary_operator_ok (ASHIFTRT, SImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_match_ccmode (insn, CCGOCmode)
   && ix86_binary_operator_ok (ASHIFTRT, SImode, operands)) },
  { "!TARGET_64BIT\n\
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)",
    MAYBE_EVAL (!TARGET_64BIT
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)) },
  { "TARGET_SSE\n\
   && (GET_CODE (operands[2]) != MEM || GET_CODE (operands[3]) != MEM)\n\
   && (!TARGET_IEEE_FP\n\
       || (GET_CODE (operands[1]) != EQ && GET_CODE (operands[1]) != NE))",
    MAYBE_EVAL (TARGET_SSE
   && (GET_CODE (operands[2]) != MEM || GET_CODE (operands[3]) != MEM)
   && (!TARGET_IEEE_FP
       || (GET_CODE (operands[1]) != EQ && GET_CODE (operands[1]) != NE))) },
  { "!TARGET_64BIT\n\
   && ix86_unary_operator_ok (NEG, DImode, operands)",
    MAYBE_EVAL (!TARGET_64BIT
   && ix86_unary_operator_ok (NEG, DImode, operands)) },
  { "!TARGET_64BIT && TARGET_80387 && !TARGET_NO_FANCY_MATH_387\n\
   && flag_unsafe_math_optimizations",
    MAYBE_EVAL (!TARGET_64BIT && TARGET_80387 && !TARGET_NO_FANCY_MATH_387
   && flag_unsafe_math_optimizations) },
  { "!TARGET_PARTIAL_REG_STALL\n\
   && ix86_binary_operator_ok (PLUS, QImode, operands)",
    MAYBE_EVAL (!TARGET_PARTIAL_REG_STALL
   && ix86_binary_operator_ok (PLUS, QImode, operands)) },
  { "ix86_match_ccmode (insn, CCNOmode)\n\
   && ix86_binary_operator_ok (IOR, HImode, operands)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCNOmode)
   && ix86_binary_operator_ok (IOR, HImode, operands)) },
  { "ix86_binary_operator_ok (AND, SImode, operands)",
    MAYBE_EVAL (ix86_binary_operator_ok (AND, SImode, operands)) },
  { "ix86_unary_operator_ok (NEG, SFmode, operands)",
    MAYBE_EVAL (ix86_unary_operator_ok (NEG, SFmode, operands)) },
  { "! TARGET_NO_FANCY_MATH_387 && TARGET_80387\n\
   && !(TARGET_SSE2 && TARGET_SSE_MATH)",
    MAYBE_EVAL (! TARGET_NO_FANCY_MATH_387 && TARGET_80387
   && !(TARGET_SSE2 && TARGET_SSE_MATH)) },
  { "ix86_binary_operator_ok (ASHIFT, SImode, operands)",
    MAYBE_EVAL (ix86_binary_operator_ok (ASHIFT, SImode, operands)) },
  { "(! TARGET_PARTIAL_REG_STALL || optimize_size)\n\
   && (TARGET_SHIFT1 || optimize_size)",
    MAYBE_EVAL ((! TARGET_PARTIAL_REG_STALL || optimize_size)
   && (TARGET_SHIFT1 || optimize_size)) },
  { "reload_completed\n\
   && ((operands_match_p (operands[1], operands[3])\n\
	&& operands_match_p (operands[2], operands[4]))\n\
       || (operands_match_p (operands[1], operands[4])\n\
	   && operands_match_p (operands[2], operands[3])))",
    MAYBE_EVAL (reload_completed
   && ((operands_match_p (operands[1], operands[3])
	&& operands_match_p (operands[2], operands[4]))
       || (operands_match_p (operands[1], operands[4])
	   && operands_match_p (operands[2], operands[3])))) },
  { "TARGET_80387\n\
   && FLOAT_MODE_P (GET_MODE (operands[1]))\n\
   && GET_MODE (operands[1]) == GET_MODE (operands[2])",
    MAYBE_EVAL (TARGET_80387
   && FLOAT_MODE_P (GET_MODE (operands[1]))
   && GET_MODE (operands[1]) == GET_MODE (operands[2])) },
  { "TARGET_SSE2\n\
   && (GET_CODE (operands[2]) != MEM || GET_CODE (operands[3]) != MEM)\n\
   && (!TARGET_IEEE_FP\n\
       || (GET_CODE (operands[1]) != EQ && GET_CODE (operands[1]) != NE))",
    MAYBE_EVAL (TARGET_SSE2
   && (GET_CODE (operands[2]) != MEM || GET_CODE (operands[3]) != MEM)
   && (!TARGET_IEEE_FP
       || (GET_CODE (operands[1]) != EQ && GET_CODE (operands[1]) != NE))) },
  { "TARGET_80387 || TARGET_SSE_MATH",
    MAYBE_EVAL (TARGET_80387 || TARGET_SSE_MATH) },
  { "TARGET_80387 && !TARGET_SSE2",
    MAYBE_EVAL (TARGET_80387 && !TARGET_SSE2) },
  { "!TARGET_64BIT && TARGET_80387 && reload_completed",
    MAYBE_EVAL (!TARGET_64BIT && TARGET_80387 && reload_completed) },
  { "! TARGET_NO_FANCY_MATH_387 && TARGET_80387 \n\
   && flag_unsafe_math_optimizations",
    MAYBE_EVAL (! TARGET_NO_FANCY_MATH_387 && TARGET_80387 
   && flag_unsafe_math_optimizations) },
  { "TARGET_64BIT && (TARGET_PENTIUM || optimize_size)\n\
   && reload_completed\n\
   && GET_CODE (operands[1]) == CONST_INT\n\
   && INTVAL (operands[1]) == -1",
    MAYBE_EVAL (TARGET_64BIT && (TARGET_PENTIUM || optimize_size)
   && reload_completed
   && GET_CODE (operands[1]) == CONST_INT
   && INTVAL (operands[1]) == -1) },
  { "TARGET_64BIT && reload_completed\n\
   && true_regnum (operands[0]) != true_regnum (operands[1])",
    MAYBE_EVAL (TARGET_64BIT && reload_completed
   && true_regnum (operands[0]) != true_regnum (operands[1])) },
  { "SSE_REG_P (operands[0]) && reload_completed\n\
   && (const0_operand (operands[2], GET_MODE (operands[0]))\n\
       || const0_operand (operands[3], GET_MODE (operands[0])))",
    MAYBE_EVAL (SSE_REG_P (operands[0]) && reload_completed
   && (const0_operand (operands[2], GET_MODE (operands[0]))
       || const0_operand (operands[3], GET_MODE (operands[0])))) },
  { "!TARGET_64BIT && TARGET_SUN_TLS",
    MAYBE_EVAL (!TARGET_64BIT && TARGET_SUN_TLS) },
  { "0",
    MAYBE_EVAL (0) },
  { "TARGET_64BIT && ix86_binary_operator_ok (PLUS, SImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_binary_operator_ok (PLUS, SImode, operands)) },
  { "optimize_size || (TARGET_FAST_PREFIX && !TARGET_PARTIAL_REG_STALL)",
    MAYBE_EVAL (optimize_size || (TARGET_FAST_PREFIX && !TARGET_PARTIAL_REG_STALL)) },
  { "TARGET_64BIT && ix86_match_ccmode (insn, CCGOCmode)\n\
   && ix86_binary_operator_ok (ASHIFT, DImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_match_ccmode (insn, CCGOCmode)
   && ix86_binary_operator_ok (ASHIFT, DImode, operands)) },
  { "TARGET_64BIT && ix86_match_ccmode (insn, CCNOmode)\n\
   && ix86_binary_operator_ok (XOR, SImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_match_ccmode (insn, CCNOmode)
   && ix86_binary_operator_ok (XOR, SImode, operands)) },
  { "TARGET_SSE2 && !TARGET_64BIT",
    MAYBE_EVAL (TARGET_SSE2 && !TARGET_64BIT) },
  { "TARGET_64BIT && TARGET_80387 && (!TARGET_SSE2 || TARGET_MIX_SSE_I387)",
    MAYBE_EVAL (TARGET_64BIT && TARGET_80387 && (!TARGET_SSE2 || TARGET_MIX_SSE_I387)) },
  { "TARGET_80387 && (!TARGET_SSE || !TARGET_64BIT || TARGET_MIX_SSE_I387)",
    MAYBE_EVAL (TARGET_80387 && (!TARGET_SSE || !TARGET_64BIT || TARGET_MIX_SSE_I387)) },
  { "TARGET_64BIT && ix86_match_ccmode (insn, CCGOCmode)\n\
   && (TARGET_SHIFT1 || optimize_size)\n\
   && ix86_binary_operator_ok (ASHIFTRT, DImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_match_ccmode (insn, CCGOCmode)
   && (TARGET_SHIFT1 || optimize_size)
   && ix86_binary_operator_ok (ASHIFTRT, DImode, operands)) },
  { "!optimize_size\n\
   && peep2_regno_dead_p (0, FLAGS_REG)\n\
   && ((TARGET_PENTIUM \n\
        && (GET_CODE (operands[0]) != MEM\n\
            || !memory_displacement_operand (operands[0], QImode)))\n\
       || (TARGET_K6 && long_memory_operand (operands[0], QImode)))",
    MAYBE_EVAL (!optimize_size
   && peep2_regno_dead_p (0, FLAGS_REG)
   && ((TARGET_PENTIUM 
        && (GET_CODE (operands[0]) != MEM
            || !memory_displacement_operand (operands[0], QImode)))
       || (TARGET_K6 && long_memory_operand (operands[0], QImode)))) },
  { "ix86_match_ccmode (insn, CCNOmode)\n\
   && ix86_unary_operator_ok (NOT, SImode, operands)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCNOmode)
   && ix86_unary_operator_ok (NOT, SImode, operands)) },
  { "!TARGET_64BIT && TARGET_STACK_PROBE",
    MAYBE_EVAL (!TARGET_64BIT && TARGET_STACK_PROBE) },
  { "TARGET_SSE && TARGET_64BIT",
    MAYBE_EVAL (TARGET_SSE && TARGET_64BIT) },
  { "TARGET_3DNOW\n\
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)",
    MAYBE_EVAL (TARGET_3DNOW
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)) },
  { "TARGET_80387\n\
   && SSE_FLOAT_MODE_P (GET_MODE (operands[1]))\n\
   && GET_MODE (operands[1]) == GET_MODE (operands[2])\n\
   && ix86_fp_jump_nontrivial_p (GET_CODE (operands[0]))",
    MAYBE_EVAL (TARGET_80387
   && SSE_FLOAT_MODE_P (GET_MODE (operands[1]))
   && GET_MODE (operands[1]) == GET_MODE (operands[2])
   && ix86_fp_jump_nontrivial_p (GET_CODE (operands[0]))) },
  { "TARGET_CMOVE\n\
   && (GET_CODE (operands[2]) != MEM || GET_CODE (operands[3]) != MEM)",
    MAYBE_EVAL (TARGET_CMOVE
   && (GET_CODE (operands[2]) != MEM || GET_CODE (operands[3]) != MEM)) },
  { "TARGET_64BIT&& reload_completed",
    MAYBE_EVAL (TARGET_64BIT&& reload_completed) },
  { "TARGET_PARTIAL_REG_STALL\n\
   && ix86_binary_operator_ok (ASHIFT, HImode, operands)",
    MAYBE_EVAL (TARGET_PARTIAL_REG_STALL
   && ix86_binary_operator_ok (ASHIFT, HImode, operands)) },
  { "TARGET_SSE2 && TARGET_SSE_MATH",
    MAYBE_EVAL (TARGET_SSE2 && TARGET_SSE_MATH) },
  { "TARGET_64BIT && ix86_binary_operator_ok (ASHIFTRT, SImode, operands)\n\
   && (TARGET_SHIFT1 || optimize_size)",
    MAYBE_EVAL (TARGET_64BIT && ix86_binary_operator_ok (ASHIFTRT, SImode, operands)
   && (TARGET_SHIFT1 || optimize_size)) },
  { "ix86_binary_operator_ok (ASHIFTRT, HImode, operands)",
    MAYBE_EVAL (ix86_binary_operator_ok (ASHIFTRT, HImode, operands)) },
  { "!TARGET_64BIT && reload_completed\n\
   && (! MMX_REG_P (operands[1]) && !SSE_REG_P (operands[1]))",
    MAYBE_EVAL (!TARGET_64BIT && reload_completed
   && (! MMX_REG_P (operands[1]) && !SSE_REG_P (operands[1]))) },
  { "!TARGET_64BIT && flag_pic",
    MAYBE_EVAL (!TARGET_64BIT && flag_pic) },
  { "TARGET_64BIT\n\
   && ix86_match_ccmode (insn, CCNOmode)\n\
   && ix86_binary_operator_ok (IOR, DImode, operands)",
    MAYBE_EVAL (TARGET_64BIT
   && ix86_match_ccmode (insn, CCNOmode)
   && ix86_binary_operator_ok (IOR, DImode, operands)) },
  { "!TARGET_64BIT && TARGET_80387\n\
   && GET_RTX_CLASS (GET_CODE (operands[3])) == 'c'",
    MAYBE_EVAL (!TARGET_64BIT && TARGET_80387
   && GET_RTX_CLASS (GET_CODE (operands[3])) == 'c') },
  { "ix86_binary_operator_ok (ROTATERT, HImode, operands)\n\
   && (TARGET_SHIFT1 || optimize_size)",
    MAYBE_EVAL (ix86_binary_operator_ok (ROTATERT, HImode, operands)
   && (TARGET_SHIFT1 || optimize_size)) },
  { "TARGET_SSE2 && TARGET_64BIT",
    MAYBE_EVAL (TARGET_SSE2 && TARGET_64BIT) },
  { "!TARGET_64BIT && TARGET_80387 && !TARGET_NO_FANCY_MATH_387 \n\
   && (TARGET_IEEE_FP || flag_unsafe_math_optimizations) ",
    MAYBE_EVAL (!TARGET_64BIT && TARGET_80387 && !TARGET_NO_FANCY_MATH_387 
   && (TARGET_IEEE_FP || flag_unsafe_math_optimizations) ) },
  { "TARGET_80387 && FLOAT_MODE_P (GET_MODE (operands[1]))\n\
   && !reload_completed && !reload_in_progress\n\
   && !SSE_FLOAT_MODE_P (GET_MODE (operands[1]))&& 1",
    MAYBE_EVAL (TARGET_80387 && FLOAT_MODE_P (GET_MODE (operands[1]))
   && !reload_completed && !reload_in_progress
   && !SSE_FLOAT_MODE_P (GET_MODE (operands[1]))&& 1) },
  { "TARGET_MMX\n\
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)",
    MAYBE_EVAL (TARGET_MMX
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)) },
  { "TARGET_64BIT\n\
   && ix86_match_ccmode (insn, CCNOmode)\n\
   /* The code below cannot deal with constants outside HOST_WIDE_INT.  */\n\
   && INTVAL (operands[1]) + INTVAL (operands[2]) < HOST_BITS_PER_WIDE_INT\n\
   /* Ensure that resulting mask is zero or sign extended operand.  */\n\
   && (INTVAL (operands[1]) + INTVAL (operands[2]) <= 32\n\
       || (INTVAL (operands[1]) + INTVAL (operands[2]) == 64\n\
	   && INTVAL (operands[1]) > 32))\n\
   && (GET_MODE (operands[0]) == SImode\n\
       || GET_MODE (operands[0]) == DImode\n\
       || GET_MODE (operands[0]) == HImode\n\
       || GET_MODE (operands[0]) == QImode)",
    MAYBE_EVAL (TARGET_64BIT
   && ix86_match_ccmode (insn, CCNOmode)
   /* The code below cannot deal with constants outside HOST_WIDE_INT.  */
   && INTVAL (operands[1]) + INTVAL (operands[2]) < HOST_BITS_PER_WIDE_INT
   /* Ensure that resulting mask is zero or sign extended operand.  */
   && (INTVAL (operands[1]) + INTVAL (operands[2]) <= 32
       || (INTVAL (operands[1]) + INTVAL (operands[2]) == 64
	   && INTVAL (operands[1]) > 32))
   && (GET_MODE (operands[0]) == SImode
       || GET_MODE (operands[0]) == DImode
       || GET_MODE (operands[0]) == HImode
       || GET_MODE (operands[0]) == QImode)) },
  { "TARGET_SSE || TARGET_3DNOW_A",
    MAYBE_EVAL (TARGET_SSE || TARGET_3DNOW_A) },
  { "(GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)\n\
   && optimize_size\n\
   && (reload_in_progress || reload_completed\n\
       || GET_CODE (operands[1]) != CONST_DOUBLE\n\
       || (ix86_cmodel == CM_MEDIUM || ix86_cmodel == CM_LARGE)\n\
       || memory_operand (operands[0], TFmode))",
    MAYBE_EVAL ((GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)
   && optimize_size
   && (reload_in_progress || reload_completed
       || GET_CODE (operands[1]) != CONST_DOUBLE
       || (ix86_cmodel == CM_MEDIUM || ix86_cmodel == CM_LARGE)
       || memory_operand (operands[0], TFmode))) },
  { "! TARGET_NO_FANCY_MATH_387 && TARGET_80387 \n\
   && (TARGET_IEEE_FP || flag_unsafe_math_optimizations) ",
    MAYBE_EVAL (! TARGET_NO_FANCY_MATH_387 && TARGET_80387 
   && (TARGET_IEEE_FP || flag_unsafe_math_optimizations) ) },
  { "exact_log2 (INTVAL (operands[1])) >= 0\n\
   && peep2_regno_dead_p (0, FLAGS_REG)",
    MAYBE_EVAL (exact_log2 (INTVAL (operands[1])) >= 0
   && peep2_regno_dead_p (0, FLAGS_REG)) },
  { "TARGET_64BIT && TARGET_STACK_PROBE",
    MAYBE_EVAL (TARGET_64BIT && TARGET_STACK_PROBE) },
  { "ix86_binary_operator_ok (PLUS, QImode, operands)",
    MAYBE_EVAL (ix86_binary_operator_ok (PLUS, QImode, operands)) },
  { "!SSE_REG_P (operands[0]) && reload_completed\n\
   && VALID_SSE_REG_MODE (GET_MODE (operands[0]))",
    MAYBE_EVAL (!SSE_REG_P (operands[0]) && reload_completed
   && VALID_SSE_REG_MODE (GET_MODE (operands[0]))) },
  { "TARGET_64BIT\n\
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)",
    MAYBE_EVAL (TARGET_64BIT
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)) },
  { "TARGET_80387 && TARGET_USE_FIOP && !TARGET_SSE_MATH",
    MAYBE_EVAL (TARGET_80387 && TARGET_USE_FIOP && !TARGET_SSE_MATH) },
  { "! TARGET_NO_FANCY_MATH_387 && TARGET_80387",
    MAYBE_EVAL (! TARGET_NO_FANCY_MATH_387 && TARGET_80387) },
  { "ix86_binary_operator_ok (ROTATE, SImode, operands)\n\
   && (TARGET_SHIFT1 || optimize_size)",
    MAYBE_EVAL (ix86_binary_operator_ok (ROTATE, SImode, operands)
   && (TARGET_SHIFT1 || optimize_size)) },
  { "!TARGET_80387 && TARGET_SSE2",
    MAYBE_EVAL (!TARGET_80387 && TARGET_SSE2) },
  { "TARGET_64BIT && ix86_unary_operator_ok (NEG, DImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_unary_operator_ok (NEG, DImode, operands)) },
  { "TARGET_64BIT && ix86_match_ccmode (insn, CCmode)\n\
   && ix86_binary_operator_ok (MINUS, SImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_match_ccmode (insn, CCmode)
   && ix86_binary_operator_ok (MINUS, SImode, operands)) },
  { "ix86_match_ccmode (insn, CCmode)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCmode)) },
  { "TARGET_64BIT && TARGET_CMOVE\n\
   && (GET_CODE (operands[2]) != MEM || GET_CODE (operands[3]) != MEM)",
    MAYBE_EVAL (TARGET_64BIT && TARGET_CMOVE
   && (GET_CODE (operands[2]) != MEM || GET_CODE (operands[3]) != MEM)) },
  { "(GET_MODE (operands[0]) == QImode\n\
    || GET_MODE (operands[0]) == HImode)\n\
   && (! TARGET_USE_MOV0 || optimize_size)\n\
   && peep2_regno_dead_p (0, FLAGS_REG)",
    MAYBE_EVAL ((GET_MODE (operands[0]) == QImode
    || GET_MODE (operands[0]) == HImode)
   && (! TARGET_USE_MOV0 || optimize_size)
   && peep2_regno_dead_p (0, FLAGS_REG)) },
  { "TARGET_80387 && reload_completed\n\
   && FLOAT_MODE_P (GET_MODE (operands[0]))",
    MAYBE_EVAL (TARGET_80387 && reload_completed
   && FLOAT_MODE_P (GET_MODE (operands[0]))) },
  { "! TARGET_NO_FANCY_MATH_387 && TARGET_80387\n\
   && (TARGET_SSE2 && TARGET_SSE_MATH && TARGET_MIX_SSE_I387)",
    MAYBE_EVAL (! TARGET_NO_FANCY_MATH_387 && TARGET_80387
   && (TARGET_SSE2 && TARGET_SSE_MATH && TARGET_MIX_SSE_I387)) },
  { "ix86_binary_operator_ok (ROTATE, HImode, operands)\n\
   && (TARGET_SHIFT1 || optimize_size)",
    MAYBE_EVAL (ix86_binary_operator_ok (ROTATE, HImode, operands)
   && (TARGET_SHIFT1 || optimize_size)) },
  { "ix86_match_ccmode (insn, CCGOCmode)\n\
   && (TARGET_SHIFT1 || optimize_size)\n\
   && ix86_binary_operator_ok (ASHIFTRT, SImode, operands)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCGOCmode)
   && (TARGET_SHIFT1 || optimize_size)
   && ix86_binary_operator_ok (ASHIFTRT, SImode, operands)) },
  { "reload_completed\n\
   && ((!TARGET_USE_MOV0 && !TARGET_PARTIAL_REG_STALL) || optimize_size)",
    MAYBE_EVAL (reload_completed
   && ((!TARGET_USE_MOV0 && !TARGET_PARTIAL_REG_STALL) || optimize_size)) },
  { "TARGET_80387 && TARGET_CMOVE\n\
   && !SSE_FLOAT_MODE_P (GET_MODE (operands[0]))\n\
   && FLOAT_MODE_P (GET_MODE (operands[0]))\n\
   && GET_MODE (operands[0]) == GET_MODE (operands[1])",
    MAYBE_EVAL (TARGET_80387 && TARGET_CMOVE
   && !SSE_FLOAT_MODE_P (GET_MODE (operands[0]))
   && FLOAT_MODE_P (GET_MODE (operands[0]))
   && GET_MODE (operands[0]) == GET_MODE (operands[1])) },
  { "(TARGET_SSE || TARGET_3DNOW_A) && TARGET_64BIT",
    MAYBE_EVAL ((TARGET_SSE || TARGET_3DNOW_A) && TARGET_64BIT) },
  { "TARGET_80387\n\
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)",
    MAYBE_EVAL (TARGET_80387
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)) },
  { "TARGET_64BIT && ix86_binary_operator_ok (ROTATE, SImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_binary_operator_ok (ROTATE, SImode, operands)) },
  { "!TARGET_64BIT && TARGET_USE_LOOP\n\
   && reload_completed\n\
   && (! REG_P (operands[2])\n\
       || ! rtx_equal_p (operands[1], operands[2]))",
    MAYBE_EVAL (!TARGET_64BIT && TARGET_USE_LOOP
   && reload_completed
   && (! REG_P (operands[2])
       || ! rtx_equal_p (operands[1], operands[2]))) },
  { "ix86_binary_operator_ok (IOR, QImode, operands)",
    MAYBE_EVAL (ix86_binary_operator_ok (IOR, QImode, operands)) },
  { "!TARGET_64BIT && TARGET_80387 && !TARGET_NO_FANCY_MATH_387",
    MAYBE_EVAL (!TARGET_64BIT && TARGET_80387 && !TARGET_NO_FANCY_MATH_387) },
  { "ix86_match_ccmode (insn, CCGOCmode)\n\
   && ix86_binary_operator_ok (ASHIFT, QImode, operands)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCGOCmode)
   && ix86_binary_operator_ok (ASHIFT, QImode, operands)) },
  { "TARGET_64BIT && reload_completed\n\
   && (SSE_REG_P (operands[1]) || MMX_REG_P (operands[1]))",
    MAYBE_EVAL (TARGET_64BIT && reload_completed
   && (SSE_REG_P (operands[1]) || MMX_REG_P (operands[1]))) },
  { "ix86_match_ccmode (insn, CCNOmode)\n\
   && ix86_unary_operator_ok (NOT, QImode, operands)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCNOmode)
   && ix86_unary_operator_ok (NOT, QImode, operands)) },
  { "TARGET_SSE",
    MAYBE_EVAL (TARGET_SSE) },
  { "(GET_MODE (operands[0]) == QImode\n\
    || GET_MODE (operands[0]) == HImode\n\
    || GET_MODE (operands[0]) == SImode\n\
    || (GET_MODE (operands[0]) == DImode && TARGET_64BIT))\n\
   && (! TARGET_USE_MOV0 || optimize_size)\n\
   && peep2_regno_dead_p (0, FLAGS_REG)",
    MAYBE_EVAL ((GET_MODE (operands[0]) == QImode
    || GET_MODE (operands[0]) == HImode
    || GET_MODE (operands[0]) == SImode
    || (GET_MODE (operands[0]) == DImode && TARGET_64BIT))
   && (! TARGET_USE_MOV0 || optimize_size)
   && peep2_regno_dead_p (0, FLAGS_REG)) },
  { "TARGET_64BIT && ix86_match_ccmode (insn, CCGOCmode)\n\
   && ix86_binary_operator_ok (PLUS, DImode, operands)\n\
   /* Current assemblers are broken and do not allow @GOTOFF in\n\
      ought but a memory context.  */\n\
   && ! pic_symbolic_operand (operands[2], VOIDmode)",
    MAYBE_EVAL (TARGET_64BIT && ix86_match_ccmode (insn, CCGOCmode)
   && ix86_binary_operator_ok (PLUS, DImode, operands)
   /* Current assemblers are broken and do not allow @GOTOFF in
      ought but a memory context.  */
   && ! pic_symbolic_operand (operands[2], VOIDmode)) },
  { "!TARGET_64BIT && reload_completed",
    MAYBE_EVAL (!TARGET_64BIT && reload_completed) },
  { "(optimize_size || !TARGET_PARTIAL_REG_STALL) && reload_completed",
    MAYBE_EVAL ((optimize_size || !TARGET_PARTIAL_REG_STALL) && reload_completed) },
  { "TARGET_HIMODE_MATH",
    MAYBE_EVAL (TARGET_HIMODE_MATH) },
  { "ix86_binary_operator_ok (ASHIFTRT, QImode, operands)\n\
   && (! TARGET_PARTIAL_REG_STALL || optimize_size)\n\
   && (TARGET_SHIFT1 || optimize_size)",
    MAYBE_EVAL (ix86_binary_operator_ok (ASHIFTRT, QImode, operands)
   && (! TARGET_PARTIAL_REG_STALL || optimize_size)
   && (TARGET_SHIFT1 || optimize_size)) },
  { "ix86_match_ccmode (insn, CCGOCmode)\n\
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCGOCmode)
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)) },
  { "TARGET_SSE\n\
   && (GET_CODE (operands[2]) != MEM || GET_CODE (operands[3]) != MEM)",
    MAYBE_EVAL (TARGET_SSE
   && (GET_CODE (operands[2]) != MEM || GET_CODE (operands[3]) != MEM)) },
  { "TARGET_PARTIAL_REG_STALL\n\
   && ix86_binary_operator_ok (PLUS, QImode, operands)",
    MAYBE_EVAL (TARGET_PARTIAL_REG_STALL
   && ix86_binary_operator_ok (PLUS, QImode, operands)) },
  { "TARGET_64BIT && reload_completed",
    MAYBE_EVAL (TARGET_64BIT && reload_completed) },
  { "!TARGET_64BIT",
    MAYBE_EVAL (!TARGET_64BIT) },
  { "TARGET_QIMODE_MATH\n\
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)",
    MAYBE_EVAL (TARGET_QIMODE_MATH
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)) },
  { "!TARGET_64BIT && optimize_size",
    MAYBE_EVAL (!TARGET_64BIT && optimize_size) },
  { "TARGET_64BIT && ix86_match_ccmode (insn, CCNOmode)\n\
   && ix86_binary_operator_ok (AND, SImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_match_ccmode (insn, CCNOmode)
   && ix86_binary_operator_ok (AND, SImode, operands)) },
  { "ix86_unary_operator_ok (NEG, SImode, operands)",
    MAYBE_EVAL (ix86_unary_operator_ok (NEG, SImode, operands)) },
  { "TARGET_SSE2 && reload_completed",
    MAYBE_EVAL (TARGET_SSE2 && reload_completed) },
  { "(! TARGET_NO_FANCY_MATH_387 && TARGET_80387)\n\
   || (TARGET_SSE2 && TARGET_SSE_MATH)",
    MAYBE_EVAL ((! TARGET_NO_FANCY_MATH_387 && TARGET_80387)
   || (TARGET_SSE2 && TARGET_SSE_MATH)) },
  { "TARGET_80387 && TARGET_SSE_MATH && TARGET_MIX_SSE_I387\n\
   && GET_RTX_CLASS (GET_CODE (operands[3])) == 'c'\n\
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)",
    MAYBE_EVAL (TARGET_80387 && TARGET_SSE_MATH && TARGET_MIX_SSE_I387
   && GET_RTX_CLASS (GET_CODE (operands[3])) == 'c'
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)) },
  { "!TARGET_64BIT && reload_completed\n\
   && (!MMX_REG_P (operands[0]) && !SSE_REG_P (operands[0]))\n\
   && (!MMX_REG_P (operands[1]) && !SSE_REG_P (operands[1]))",
    MAYBE_EVAL (!TARGET_64BIT && reload_completed
   && (!MMX_REG_P (operands[0]) && !SSE_REG_P (operands[0]))
   && (!MMX_REG_P (operands[1]) && !SSE_REG_P (operands[1]))) },
  { "ix86_match_ccmode (insn, CCNOmode)\n\
   && ix86_binary_operator_ok (XOR, HImode, operands)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCNOmode)
   && ix86_binary_operator_ok (XOR, HImode, operands)) },
  { "TARGET_64BIT && ix86_unary_operator_ok (NOT, DImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_unary_operator_ok (NOT, DImode, operands)) },
  { "TARGET_64BIT && ix86_binary_operator_ok (ROTATERT, DImode, operands)\n\
   && (TARGET_SHIFT1 || optimize_size)",
    MAYBE_EVAL (TARGET_64BIT && ix86_binary_operator_ok (ROTATERT, DImode, operands)
   && (TARGET_SHIFT1 || optimize_size)) },
  { "TARGET_80387 || TARGET_SSE",
    MAYBE_EVAL (TARGET_80387 || TARGET_SSE) },
  { "(GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)\n\
   && (optimize_size || !TARGET_INTEGER_DFMODE_MOVES)\n\
   && (reload_in_progress || reload_completed\n\
       || (ix86_cmodel == CM_MEDIUM || ix86_cmodel == CM_LARGE)\n\
       || GET_CODE (operands[1]) != CONST_DOUBLE\n\
       || memory_operand (operands[0], DFmode))",
    MAYBE_EVAL ((GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)
   && (optimize_size || !TARGET_INTEGER_DFMODE_MOVES)
   && (reload_in_progress || reload_completed
       || (ix86_cmodel == CM_MEDIUM || ix86_cmodel == CM_LARGE)
       || GET_CODE (operands[1]) != CONST_DOUBLE
       || memory_operand (operands[0], DFmode))) },
  { "reload_completed\n\
   && GET_CODE (operands[1]) == MEM\n\
   && GET_CODE (XEXP (operands[1], 0)) == SYMBOL_REF\n\
   && CONSTANT_POOL_ADDRESS_P (XEXP (operands[1], 0))",
    MAYBE_EVAL (reload_completed
   && GET_CODE (operands[1]) == MEM
   && GET_CODE (XEXP (operands[1], 0)) == SYMBOL_REF
   && CONSTANT_POOL_ADDRESS_P (XEXP (operands[1], 0))) },
  { "TARGET_CMOVE",
    MAYBE_EVAL (TARGET_CMOVE) },
  { "TARGET_3DNOW && TARGET_64BIT",
    MAYBE_EVAL (TARGET_3DNOW && TARGET_64BIT) },
  { "!TARGET_64BIT && ! TARGET_NO_FANCY_MATH_387 && TARGET_80387\n\
   && flag_unsafe_math_optimizations",
    MAYBE_EVAL (!TARGET_64BIT && ! TARGET_NO_FANCY_MATH_387 && TARGET_80387
   && flag_unsafe_math_optimizations) },
  { "TARGET_SSE2 && TARGET_SSE_MATH\n\
   && GET_RTX_CLASS (GET_CODE (operands[3])) == 'c'\n\
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)",
    MAYBE_EVAL (TARGET_SSE2 && TARGET_SSE_MATH
   && GET_RTX_CLASS (GET_CODE (operands[3])) == 'c'
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)) },
  { "TARGET_SSE\n\
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)",
    MAYBE_EVAL (TARGET_SSE
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)) },
  { "TARGET_80387 && !TARGET_SSE\n\
   && ix86_unary_operator_ok (NEG, SFmode, operands)",
    MAYBE_EVAL (TARGET_80387 && !TARGET_SSE
   && ix86_unary_operator_ok (NEG, SFmode, operands)) },
  { "!TARGET_64BIT && (TARGET_SINGLE_STRINGOP || optimize_size)",
    MAYBE_EVAL (!TARGET_64BIT && (TARGET_SINGLE_STRINGOP || optimize_size)) },
  { "reload_completed\n\
    && QI_REG_P (operands[0])\n\
    && (!TARGET_PARTIAL_REG_STALL || optimize_size)\n\
    && !(~INTVAL (operands[2]) & ~(255 << 8))\n\
    && GET_MODE (operands[0]) != QImode",
    MAYBE_EVAL (reload_completed
    && QI_REG_P (operands[0])
    && (!TARGET_PARTIAL_REG_STALL || optimize_size)
    && !(~INTVAL (operands[2]) & ~(255 << 8))
    && GET_MODE (operands[0]) != QImode) },
  { "INTVAL (operands[2]) == 31 && (TARGET_USE_CLTD || optimize_size)\n\
   && ix86_binary_operator_ok (ASHIFTRT, SImode, operands)",
    MAYBE_EVAL (INTVAL (operands[2]) == 31 && (TARGET_USE_CLTD || optimize_size)
   && ix86_binary_operator_ok (ASHIFTRT, SImode, operands)) },
  { "ix86_unary_operator_ok (NEG, QImode, operands)",
    MAYBE_EVAL (ix86_unary_operator_ok (NEG, QImode, operands)) },
  { "ix86_can_use_return_insn_p ()",
    MAYBE_EVAL (ix86_can_use_return_insn_p ()) },
  { "! TARGET_PARTIAL_REG_STALL && reload_completed\n\
   && ((GET_MODE (operands[0]) == HImode \n\
	&& ((!optimize_size && !TARGET_FAST_PREFIX)\n\
	    || GET_CODE (operands[2]) != CONST_INT\n\
	    || CONST_OK_FOR_LETTER_P (INTVAL (operands[2]), 'K')))\n\
       || (GET_MODE (operands[0]) == QImode \n\
	   && (TARGET_PROMOTE_QImode || optimize_size)))",
    MAYBE_EVAL (! TARGET_PARTIAL_REG_STALL && reload_completed
   && ((GET_MODE (operands[0]) == HImode 
	&& ((!optimize_size && !TARGET_FAST_PREFIX)
	    || GET_CODE (operands[2]) != CONST_INT
	    || CONST_OK_FOR_LETTER_P (INTVAL (operands[2]), 'K')))
       || (GET_MODE (operands[0]) == QImode 
	   && (TARGET_PROMOTE_QImode || optimize_size)))) },
  { "reload_completed && (!TARGET_USE_MOV0 || optimize_size)",
    MAYBE_EVAL (reload_completed && (!TARGET_USE_MOV0 || optimize_size)) },
  { "TARGET_64BIT && ix86_binary_operator_ok (MINUS, SImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_binary_operator_ok (MINUS, SImode, operands)) },
  { "TARGET_80387 && TARGET_CMOVE\n\
   && !SSE_FLOAT_MODE_P (GET_MODE (operands[0]))\n\
   && FLOAT_MODE_P (GET_MODE (operands[0]))\n\
   && GET_MODE (operands[0]) == GET_MODE (operands[0])",
    MAYBE_EVAL (TARGET_80387 && TARGET_CMOVE
   && !SSE_FLOAT_MODE_P (GET_MODE (operands[0]))
   && FLOAT_MODE_P (GET_MODE (operands[0]))
   && GET_MODE (operands[0]) == GET_MODE (operands[0])) },
  { "peep2_regno_dead_p (0, FLAGS_REG)",
    MAYBE_EVAL (peep2_regno_dead_p (0, FLAGS_REG)) },
  { "TARGET_64BIT && ix86_binary_operator_ok (ROTATERT, SImode, operands)\n\
   && (TARGET_SHIFT1 || optimize_size)",
    MAYBE_EVAL (TARGET_64BIT && ix86_binary_operator_ok (ROTATERT, SImode, operands)
   && (TARGET_SHIFT1 || optimize_size)) },
  { "TARGET_64BIT && ix86_binary_operator_ok (ASHIFTRT, SImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_binary_operator_ok (ASHIFTRT, SImode, operands)) },
  { "!TARGET_64BIT\n\
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)",
    MAYBE_EVAL (!TARGET_64BIT
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)) },
  { "TARGET_64BIT && ix86_match_ccmode (insn, CCGOCmode)\n\
   && ix86_binary_operator_ok (ASHIFTRT, DImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_match_ccmode (insn, CCGOCmode)
   && ix86_binary_operator_ok (ASHIFTRT, DImode, operands)) },
  { "ix86_match_ccmode (insn, CCGOCmode)\n\
   && (TARGET_SHIFT1 || optimize_size)\n\
   && ix86_binary_operator_ok (ASHIFTRT, HImode, operands)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCGOCmode)
   && (TARGET_SHIFT1 || optimize_size)
   && ix86_binary_operator_ok (ASHIFTRT, HImode, operands)) },
  { "TARGET_64BIT && ix86_binary_operator_ok (ROTATERT, SImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_binary_operator_ok (ROTATERT, SImode, operands)) },
  { "TARGET_SSE || TARGET_64BIT",
    MAYBE_EVAL (TARGET_SSE || TARGET_64BIT) },
  { "(GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)\n\
   && (reload_in_progress || reload_completed\n\
       || (ix86_cmodel == CM_MEDIUM || ix86_cmodel == CM_LARGE)\n\
       || GET_CODE (operands[1]) != CONST_DOUBLE\n\
       || memory_operand (operands[0], SFmode))",
    MAYBE_EVAL ((GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)
   && (reload_in_progress || reload_completed
       || (ix86_cmodel == CM_MEDIUM || ix86_cmodel == CM_LARGE)
       || GET_CODE (operands[1]) != CONST_DOUBLE
       || memory_operand (operands[0], SFmode))) },
  { "ix86_unary_operator_ok (NOT, QImode, operands)",
    MAYBE_EVAL (ix86_unary_operator_ok (NOT, QImode, operands)) },
  { "ix86_binary_operator_ok (MINUS, QImode, operands)",
    MAYBE_EVAL (ix86_binary_operator_ok (MINUS, QImode, operands)) },
  { "!optimize_size\n\
   && peep2_regno_dead_p (0, FLAGS_REG)\n\
   && ((TARGET_PENTIUM \n\
        && (GET_CODE (operands[0]) != MEM\n\
            || !memory_displacement_operand (operands[0], HImode)))\n\
       || (TARGET_K6 && long_memory_operand (operands[0], HImode)))",
    MAYBE_EVAL (!optimize_size
   && peep2_regno_dead_p (0, FLAGS_REG)
   && ((TARGET_PENTIUM 
        && (GET_CODE (operands[0]) != MEM
            || !memory_displacement_operand (operands[0], HImode)))
       || (TARGET_K6 && long_memory_operand (operands[0], HImode)))) },
  { "!TARGET_64BIT && TARGET_80387\n\
   && ix86_unary_operator_ok (ABS, DFmode, operands)",
    MAYBE_EVAL (!TARGET_64BIT && TARGET_80387
   && ix86_unary_operator_ok (ABS, DFmode, operands)) },
  { "!optimize_size",
    MAYBE_EVAL (!optimize_size) },
  { "ix86_binary_operator_ok (ROTATERT, SImode, operands)",
    MAYBE_EVAL (ix86_binary_operator_ok (ROTATERT, SImode, operands)) },
  { "(GET_MODE (operands[0]) == HImode\n\
    || GET_MODE (operands[0]) == SImode \n\
    || (GET_MODE (operands[0]) == DImode && TARGET_64BIT))\n\
   && (optimize_size || TARGET_PENTIUM)\n\
   && peep2_regno_dead_p (0, FLAGS_REG)",
    MAYBE_EVAL ((GET_MODE (operands[0]) == HImode
    || GET_MODE (operands[0]) == SImode 
    || (GET_MODE (operands[0]) == DImode && TARGET_64BIT))
   && (optimize_size || TARGET_PENTIUM)
   && peep2_regno_dead_p (0, FLAGS_REG)) },
  { "TARGET_80387",
    MAYBE_EVAL (TARGET_80387) },
  { "! optimize_size && ! TARGET_READ_MODIFY",
    MAYBE_EVAL (! optimize_size && ! TARGET_READ_MODIFY) },
  { "ix86_binary_operator_ok (LSHIFTRT, HImode, operands)\n\
   && (TARGET_SHIFT1 || optimize_size)",
    MAYBE_EVAL (ix86_binary_operator_ok (LSHIFTRT, HImode, operands)
   && (TARGET_SHIFT1 || optimize_size)) },
  { "(peep2_reg_dead_p (3, operands[1])\n\
    || operands_match_p (operands[1], operands[3]))\n\
   && ! reg_overlap_mentioned_p (operands[3], operands[0])",
    MAYBE_EVAL ((peep2_reg_dead_p (3, operands[1])
    || operands_match_p (operands[1], operands[3]))
   && ! reg_overlap_mentioned_p (operands[3], operands[0])) },
  { "0 && TARGET_80387 && FLOAT_MODE_P (GET_MODE (operands[0]))\n\
   && GET_MODE (XEXP (SET_SRC (PATTERN (insn)), 1)) == GET_MODE (operands[0])",
    MAYBE_EVAL (0 && TARGET_80387 && FLOAT_MODE_P (GET_MODE (operands[0]))
   && GET_MODE (XEXP (SET_SRC (PATTERN (insn)), 1)) == GET_MODE (operands[0])) },
  { "ix86_binary_operator_ok (ROTATE, QImode, operands)",
    MAYBE_EVAL (ix86_binary_operator_ok (ROTATE, QImode, operands)) },
  { "TARGET_80387 && !TARGET_SSE",
    MAYBE_EVAL (TARGET_80387 && !TARGET_SSE) },
  { "reload_completed || !TARGET_SSE2",
    MAYBE_EVAL (reload_completed || !TARGET_SSE2) },
  { "TARGET_64BIT\n\
   && ix86_match_ccmode (insn, CCZmode)\n\
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)\n\
   /* Current assemblers are broken and do not allow @GOTOFF in\n\
      ought but a memory context.  */\n\
   && ! pic_symbolic_operand (operands[2], VOIDmode)",
    MAYBE_EVAL (TARGET_64BIT
   && ix86_match_ccmode (insn, CCZmode)
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)
   /* Current assemblers are broken and do not allow @GOTOFF in
      ought but a memory context.  */
   && ! pic_symbolic_operand (operands[2], VOIDmode)) },
  { "!TARGET_64BIT\n\
   && (!TARGET_PARTIAL_REG_STALL || optimize_size)",
    MAYBE_EVAL (!TARGET_64BIT
   && (!TARGET_PARTIAL_REG_STALL || optimize_size)) },
  { "TARGET_SSE2\n\
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)",
    MAYBE_EVAL (TARGET_SSE2
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)) },
  { "reload_completed\n\
    && ANY_QI_REG_P (operands[0])\n\
    && (!TARGET_PARTIAL_REG_STALL || optimize_size)\n\
    && !(INTVAL (operands[2]) & ~255)\n\
    && (INTVAL (operands[2]) & 128)\n\
    && GET_MODE (operands[0]) != QImode",
    MAYBE_EVAL (reload_completed
    && ANY_QI_REG_P (operands[0])
    && (!TARGET_PARTIAL_REG_STALL || optimize_size)
    && !(INTVAL (operands[2]) & ~255)
    && (INTVAL (operands[2]) & 128)
    && GET_MODE (operands[0]) != QImode) },
  { "TARGET_SSE_MATH && (!TARGET_80387 || !TARGET_MIX_SSE_I387)",
    MAYBE_EVAL (TARGET_SSE_MATH && (!TARGET_80387 || !TARGET_MIX_SSE_I387)) },
  { "TARGET_64BIT && ix86_binary_operator_ok (MINUS, DImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_binary_operator_ok (MINUS, DImode, operands)) },
  { "ix86_match_ccmode (insn, CCGOCmode)\n\
   && (TARGET_SHIFT1 || optimize_size)\n\
   && ix86_binary_operator_ok (LSHIFTRT, HImode, operands)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCGOCmode)
   && (TARGET_SHIFT1 || optimize_size)
   && ix86_binary_operator_ok (LSHIFTRT, HImode, operands)) },
  { "ix86_match_ccmode (insn, CCNOmode)\n\
   && ix86_binary_operator_ok (IOR, SImode, operands)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCNOmode)
   && ix86_binary_operator_ok (IOR, SImode, operands)) },
  { "ix86_match_ccmode (insn, CCNOmode)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCNOmode)) },
  { "TARGET_64BIT && (optimize_size || TARGET_USE_CLTD)",
    MAYBE_EVAL (TARGET_64BIT && (optimize_size || TARGET_USE_CLTD)) },
  { "reload_completed\n\
   && true_regnum (operands[0]) == true_regnum (operands[1])",
    MAYBE_EVAL (reload_completed
   && true_regnum (operands[0]) == true_regnum (operands[1])) },
  { "(GET_MODE (operands[0]) == QImode || GET_MODE (operands[0]) == HImode\n\
    || (TARGET_64BIT && GET_MODE (operands[0]) == SImode))\n\
   && (!TARGET_PARTIAL_REG_STALL || optimize_size)\n\
   && GET_MODE (operands[0]) == GET_MODE (operands[1])\n\
   && GET_MODE (operands[0]) == GET_MODE (operands[3])",
    MAYBE_EVAL ((GET_MODE (operands[0]) == QImode || GET_MODE (operands[0]) == HImode
    || (TARGET_64BIT && GET_MODE (operands[0]) == SImode))
   && (!TARGET_PARTIAL_REG_STALL || optimize_size)
   && GET_MODE (operands[0]) == GET_MODE (operands[1])
   && GET_MODE (operands[0]) == GET_MODE (operands[3])) },
  { "! optimize_size\n\
   && ! TARGET_USE_MOV0\n\
   && TARGET_SPLIT_LONG_MOVES\n\
   && get_attr_length (insn) >= ix86_cost->large_insn\n\
   && peep2_regno_dead_p (0, FLAGS_REG)",
    MAYBE_EVAL (! optimize_size
   && ! TARGET_USE_MOV0
   && TARGET_SPLIT_LONG_MOVES
   && get_attr_length (insn) >= ix86_cost->large_insn
   && peep2_regno_dead_p (0, FLAGS_REG)) },
  { "TARGET_SSE_MATH\n\
   && GET_RTX_CLASS (GET_CODE (operands[3])) != 'c'",
    MAYBE_EVAL (TARGET_SSE_MATH
   && GET_RTX_CLASS (GET_CODE (operands[3])) != 'c') },
  { "TARGET_80387 && TARGET_USE_FIOP && !(TARGET_SSE2 && TARGET_SSE_MATH)",
    MAYBE_EVAL (TARGET_80387 && TARGET_USE_FIOP && !(TARGET_SSE2 && TARGET_SSE_MATH)) },
  { "SSE_REG_P (operands[0]) && reload_completed\n\
   && ((operands_match_p (operands[1], operands[3])\n\
	&& operands_match_p (operands[2], operands[4]))\n\
       || (operands_match_p (operands[1], operands[4])\n\
	   && operands_match_p (operands[2], operands[3])))",
    MAYBE_EVAL (SSE_REG_P (operands[0]) && reload_completed
   && ((operands_match_p (operands[1], operands[3])
	&& operands_match_p (operands[2], operands[4]))
       || (operands_match_p (operands[1], operands[4])
	   && operands_match_p (operands[2], operands[3])))) },
  { "ix86_binary_operator_ok (ROTATERT, HImode, operands)",
    MAYBE_EVAL (ix86_binary_operator_ok (ROTATERT, HImode, operands)) },
  { "TARGET_CMOVE && TARGET_80387\n\
   && !SSE_FLOAT_MODE_P (GET_MODE (operands[1]))\n\
   && FLOAT_MODE_P (GET_MODE (operands[1]))\n\
   && GET_MODE (operands[1]) == GET_MODE (operands[2])\n\
   && ix86_fp_jump_nontrivial_p (GET_CODE (operands[0]))",
    MAYBE_EVAL (TARGET_CMOVE && TARGET_80387
   && !SSE_FLOAT_MODE_P (GET_MODE (operands[1]))
   && FLOAT_MODE_P (GET_MODE (operands[1]))
   && GET_MODE (operands[1]) == GET_MODE (operands[2])
   && ix86_fp_jump_nontrivial_p (GET_CODE (operands[0]))) },
  { "peep2_reg_dead_p (4, operands[7]) && peep2_reg_dead_p (4, operands[8])",
    MAYBE_EVAL (peep2_reg_dead_p (4, operands[7]) && peep2_reg_dead_p (4, operands[8])) },
  { "TARGET_80387 && (!TARGET_SSE2 || !TARGET_SSE_MATH)\n\
   && GET_RTX_CLASS (GET_CODE (operands[3])) == 'c'\n\
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)",
    MAYBE_EVAL (TARGET_80387 && (!TARGET_SSE2 || !TARGET_SSE_MATH)
   && GET_RTX_CLASS (GET_CODE (operands[3])) == 'c'
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM)) },
  { "! optimize_size && ! TARGET_READ_MODIFY_WRITE",
    MAYBE_EVAL (! optimize_size && ! TARGET_READ_MODIFY_WRITE) },
  { "!TARGET_64BIT && reload_completed\n\
   && (SSE_REG_P (operands[1]) || MMX_REG_P (operands[1]))",
    MAYBE_EVAL (!TARGET_64BIT && reload_completed
   && (SSE_REG_P (operands[1]) || MMX_REG_P (operands[1]))) },
  { "TARGET_64BIT && (flow2_completed || (reload_completed && !flag_peephole2))\n\
   && !symbolic_operand (operands[1], DImode)\n\
   && !x86_64_immediate_operand (operands[1], DImode)",
    MAYBE_EVAL (TARGET_64BIT && (flow2_completed || (reload_completed && !flag_peephole2))
   && !symbolic_operand (operands[1], DImode)
   && !x86_64_immediate_operand (operands[1], DImode)) },
  { "(TARGET_SSE || TARGET_3DNOW_A) && !TARGET_64BIT",
    MAYBE_EVAL ((TARGET_SSE || TARGET_3DNOW_A) && !TARGET_64BIT) },
  { "ix86_match_ccmode (insn, CCGOCmode)\n\
   && (TARGET_SHIFT1 || optimize_size)\n\
   && ix86_binary_operator_ok (ASHIFTRT, QImode, operands)",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCGOCmode)
   && (TARGET_SHIFT1 || optimize_size)
   && ix86_binary_operator_ok (ASHIFTRT, QImode, operands)) },
  { "ix86_binary_operator_ok (XOR, SImode, operands)",
    MAYBE_EVAL (ix86_binary_operator_ok (XOR, SImode, operands)) },
  { "ix86_binary_operator_ok (AND, HImode, operands)",
    MAYBE_EVAL (ix86_binary_operator_ok (AND, HImode, operands)) },
  { "ix86_binary_operator_ok (IOR, SImode, operands)",
    MAYBE_EVAL (ix86_binary_operator_ok (IOR, SImode, operands)) },
  { "TARGET_64BIT && ix86_match_ccmode (insn, CCNOmode)\n\
   && ix86_unary_operator_ok (NOT, DImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_match_ccmode (insn, CCNOmode)
   && ix86_unary_operator_ok (NOT, DImode, operands)) },
  { "TARGET_64BIT && ix86_binary_operator_ok (AND, DImode, operands)",
    MAYBE_EVAL (TARGET_64BIT && ix86_binary_operator_ok (AND, DImode, operands)) },
  { "ix86_match_ccmode (insn, CCGCmode)\n\
   && (INTVAL (operands[2]) & 0xffff) != 0x8000",
    MAYBE_EVAL (ix86_match_ccmode (insn, CCGCmode)
   && (INTVAL (operands[2]) & 0xffff) != 0x8000) },
  { "!TARGET_64BIT && !TARGET_INTEGER_DFMODE_MOVES",
    MAYBE_EVAL (!TARGET_64BIT && !TARGET_INTEGER_DFMODE_MOVES) },
  { "TARGET_PARTIAL_REG_STALL\n\
   && ix86_binary_operator_ok (ASHIFT, QImode, operands)",
    MAYBE_EVAL (TARGET_PARTIAL_REG_STALL
   && ix86_binary_operator_ok (ASHIFT, QImode, operands)) },
  { "optimize_size || !TARGET_ADD_ESP_4",
    MAYBE_EVAL (optimize_size || !TARGET_ADD_ESP_4) },
};

const size_t n_insn_conditions = 466;
const int insn_elision_unavailable = 0;
