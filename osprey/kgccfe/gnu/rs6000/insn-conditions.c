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
  { "DEFAULT_ABI == ABI_V4 && flag_pic == 1",
    MAYBE_EVAL (DEFAULT_ABI == ABI_V4 && flag_pic == 1) },
  { "TARGET_POWER2 && TARGET_HARD_FLOAT && TARGET_FPRS",
    MAYBE_EVAL (TARGET_POWER2 && TARGET_HARD_FLOAT && TARGET_FPRS) },
  { "TARGET_UPDATE",
    MAYBE_EVAL (TARGET_UPDATE) },
  { "TARGET_ELF && DEFAULT_ABI != ABI_AIX && flag_pic == 2",
    MAYBE_EVAL (TARGET_ELF && DEFAULT_ABI != ABI_AIX && flag_pic == 2) },
  { "TARGET_PPC_GPOPT && TARGET_HARD_FLOAT && TARGET_FPRS",
    MAYBE_EVAL (TARGET_PPC_GPOPT && TARGET_HARD_FLOAT && TARGET_FPRS) },
  { "includes_rshift_p (operands[2], GEN_INT (255))",
    MAYBE_EVAL (includes_rshift_p (operands[2], GEN_INT (255))) },
  { "DEFAULT_ABI == ABI_AIX && TARGET_HARD_FLOAT && TARGET_FPRS\n\
   && TARGET_LONG_DOUBLE_128&& reload_completed",
    MAYBE_EVAL (DEFAULT_ABI == ABI_AIX && TARGET_HARD_FLOAT && TARGET_FPRS
   && TARGET_LONG_DOUBLE_128&& reload_completed) },
  { "TARGET_POWERPC64 && reload_completed\n\
   && includes_rldic_lshift_p (operands[2], operands[3])",
    MAYBE_EVAL (TARGET_POWERPC64 && reload_completed
   && includes_rldic_lshift_p (operands[2], operands[3])) },
  { "(DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_DARWIN)\n\
   && TARGET_HARD_FLOAT && TARGET_FPRS && ! TARGET_POWERPC64\n\
   && TARGET_LONG_DOUBLE_128 && reload_completed\n\
   && ((GET_CODE (operands[0]) == REG && REGNO (operands[0]) <= 31)\n\
       || (GET_CODE (operands[0]) == SUBREG\n\
	   && GET_CODE (SUBREG_REG (operands[0])) == REG\n\
	   && REGNO (SUBREG_REG (operands[0])) <= 31))",
    MAYBE_EVAL ((DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_DARWIN)
   && TARGET_HARD_FLOAT && TARGET_FPRS && ! TARGET_POWERPC64
   && TARGET_LONG_DOUBLE_128 && reload_completed
   && ((GET_CODE (operands[0]) == REG && REGNO (operands[0]) <= 31)
       || (GET_CODE (operands[0]) == SUBREG
	   && GET_CODE (SUBREG_REG (operands[0])) == REG
	   && REGNO (SUBREG_REG (operands[0])) <= 31))) },
  { "TARGET_POWERPC64 && includes_rldic_lshift_p (operands[2], operands[3])",
    MAYBE_EVAL (TARGET_POWERPC64 && includes_rldic_lshift_p (operands[2], operands[3])) },
  { "TARGET_POWERPC64 && (gpc_reg_operand (operands[0], TImode)\n\
   || gpc_reg_operand (operands[1], TImode))",
    MAYBE_EVAL (TARGET_POWERPC64 && (gpc_reg_operand (operands[0], TImode)
   || gpc_reg_operand (operands[1], TImode))) },
  { "direct_return ()",
    MAYBE_EVAL (direct_return ()) },
  { "! TARGET_POWER && ! TARGET_ISEL&& reload_completed",
    MAYBE_EVAL (! TARGET_POWER && ! TARGET_ISEL&& reload_completed) },
  { "! TARGET_POWERPC64 && reload_completed\n\
   && ((GET_CODE (operands[0]) == REG && REGNO (operands[0]) <= 31)\n\
       || (GET_CODE (operands[0]) == SUBREG\n\
	   && GET_CODE (SUBREG_REG (operands[0])) == REG\n\
	   && REGNO (SUBREG_REG (operands[0])) <= 31))",
    MAYBE_EVAL (! TARGET_POWERPC64 && reload_completed
   && ((GET_CODE (operands[0]) == REG && REGNO (operands[0]) <= 31)
       || (GET_CODE (operands[0]) == SUBREG
	   && GET_CODE (SUBREG_REG (operands[0])) == REG
	   && REGNO (SUBREG_REG (operands[0])) <= 31))) },
  { "TARGET_STRING",
    MAYBE_EVAL (TARGET_STRING) },
  { "reload_completed",
    MAYBE_EVAL (reload_completed) },
  { "TARGET_POWER2\n\
   && TARGET_HARD_FLOAT && TARGET_FPRS\n\
   && registers_ok_for_quad_peep (operands[1], operands[3])\n\
   && ! MEM_VOLATILE_P (operands[0]) && ! MEM_VOLATILE_P (operands[2])\n\
   && addrs_ok_for_quad_peep (XEXP (operands[0], 0), XEXP (operands[2], 0))",
    MAYBE_EVAL (TARGET_POWER2
   && TARGET_HARD_FLOAT && TARGET_FPRS
   && registers_ok_for_quad_peep (operands[1], operands[3])
   && ! MEM_VOLATILE_P (operands[0]) && ! MEM_VOLATILE_P (operands[2])
   && addrs_ok_for_quad_peep (XEXP (operands[0], 0), XEXP (operands[2], 0))) },
  { "TARGET_32BIT\n\
   && DEFAULT_ABI == ABI_AIX\n\
   && (INTVAL (operands[3]) & CALL_LONG) == 0",
    MAYBE_EVAL (TARGET_32BIT
   && DEFAULT_ABI == ABI_AIX
   && (INTVAL (operands[3]) & CALL_LONG) == 0) },
  { "TARGET_STRING && XVECLEN (operands[0], 0) == 5",
    MAYBE_EVAL (TARGET_STRING && XVECLEN (operands[0], 0) == 5) },
  { "TARGET_STRING && XVECLEN (operands[0], 0) == 8",
    MAYBE_EVAL (TARGET_STRING && XVECLEN (operands[0], 0) == 8) },
  { "(DEFAULT_ABI == ABI_DARWIN\n\
       || DEFAULT_ABI == ABI_V4\n\
       || DEFAULT_ABI == ABI_AIX_NODESC)\n\
   && (INTVAL (operands[3]) & CALL_LONG) == 0",
    MAYBE_EVAL ((DEFAULT_ABI == ABI_DARWIN
       || DEFAULT_ABI == ABI_V4
       || DEFAULT_ABI == ABI_AIX_NODESC)
   && (INTVAL (operands[3]) & CALL_LONG) == 0) },
  { "TARGET_STRING && TARGET_POWERPC64\n\
   && ((INTVAL (operands[2]) > 24 && INTVAL (operands[2]) < 32)\n\
       || INTVAL (operands[2]) == 0)\n\
   && (REGNO (operands[0]) < 5 || REGNO (operands[0]) > 12)\n\
   && (REGNO (operands[1]) < 5 || REGNO (operands[1]) > 12)\n\
   && REGNO (operands[4]) == 5",
    MAYBE_EVAL (TARGET_STRING && TARGET_POWERPC64
   && ((INTVAL (operands[2]) > 24 && INTVAL (operands[2]) < 32)
       || INTVAL (operands[2]) == 0)
   && (REGNO (operands[0]) < 5 || REGNO (operands[0]) > 12)
   && (REGNO (operands[1]) < 5 || REGNO (operands[1]) > 12)
   && REGNO (operands[4]) == 5) },
  { "TARGET_STRING && TARGET_POWER && ! TARGET_POWERPC64\n\
   && INTVAL (operands[2]) > 4 && INTVAL (operands[2]) <= 8",
    MAYBE_EVAL (TARGET_STRING && TARGET_POWER && ! TARGET_POWERPC64
   && INTVAL (operands[2]) > 4 && INTVAL (operands[2]) <= 8) },
  { "TARGET_POWERPC64 && (TARGET_SOFT_FLOAT || !TARGET_FPRS)\n\
   && (gpc_reg_operand (operands[0], DFmode)\n\
       || gpc_reg_operand (operands[1], DFmode))",
    MAYBE_EVAL (TARGET_POWERPC64 && (TARGET_SOFT_FLOAT || !TARGET_FPRS)
   && (gpc_reg_operand (operands[0], DFmode)
       || gpc_reg_operand (operands[1], DFmode))) },
  { "TARGET_MULTIPLE",
    MAYBE_EVAL (TARGET_MULTIPLE) },
  { "TARGET_POWERPC64 && TARGET_HARD_FLOAT && TARGET_FPRS",
    MAYBE_EVAL (TARGET_POWERPC64 && TARGET_HARD_FLOAT && TARGET_FPRS) },
  { "TARGET_32BIT",
    MAYBE_EVAL (TARGET_32BIT) },
  { "TARGET_HARD_FLOAT && !TARGET_FPRS",
    MAYBE_EVAL (TARGET_HARD_FLOAT && !TARGET_FPRS) },
  { "TARGET_MACHO && !TARGET_64BIT",
    MAYBE_EVAL (TARGET_MACHO && !TARGET_64BIT) },
  { "TARGET_POWER2\n\
   && TARGET_HARD_FLOAT && TARGET_FPRS\n\
   && registers_ok_for_quad_peep (operands[0], operands[2])\n\
   && ! MEM_VOLATILE_P (operands[1]) && ! MEM_VOLATILE_P (operands[3])\n\
   && addrs_ok_for_quad_peep (XEXP (operands[1], 0), XEXP (operands[3], 0))",
    MAYBE_EVAL (TARGET_POWER2
   && TARGET_HARD_FLOAT && TARGET_FPRS
   && registers_ok_for_quad_peep (operands[0], operands[2])
   && ! MEM_VOLATILE_P (operands[1]) && ! MEM_VOLATILE_P (operands[3])
   && addrs_ok_for_quad_peep (XEXP (operands[1], 0), XEXP (operands[3], 0))) },
  { "TARGET_32BIT && !TARGET_POWER",
    MAYBE_EVAL (TARGET_32BIT && !TARGET_POWER) },
  { "! TARGET_POWERPC64",
    MAYBE_EVAL (! TARGET_POWERPC64) },
  { "TARGET_STRING && TARGET_POWERPC64\n\
   && INTVAL (operands[2]) > 0 && INTVAL (operands[2]) <= 4",
    MAYBE_EVAL (TARGET_STRING && TARGET_POWERPC64
   && INTVAL (operands[2]) > 0 && INTVAL (operands[2]) <= 4) },
  { "reload_completed\n\
   && ((GET_CODE (operands[0]) == REG && REGNO (operands[0]) <= 31)\n\
       || (GET_CODE (operands[0]) == SUBREG\n\
	   && GET_CODE (SUBREG_REG (operands[0])) == REG\n\
	   && REGNO (SUBREG_REG (operands[0])) <= 31))",
    MAYBE_EVAL (reload_completed
   && ((GET_CODE (operands[0]) == REG && REGNO (operands[0]) <= 31)
       || (GET_CODE (operands[0]) == SUBREG
	   && GET_CODE (SUBREG_REG (operands[0])) == REG
	   && REGNO (SUBREG_REG (operands[0])) <= 31))) },
  { "INTVAL (operands[4]) >= INTVAL (operands[1])",
    MAYBE_EVAL (INTVAL (operands[4]) >= INTVAL (operands[1])) },
  { "TARGET_STRING && TARGET_POWER\n\
   && INTVAL (operands[2]) > 8 && INTVAL (operands[2]) <= 16\n\
   && (REGNO (operands[0]) < 5 || REGNO (operands[0]) > 8)\n\
   && (REGNO (operands[1]) < 5 || REGNO (operands[1]) > 8)\n\
   && REGNO (operands[4]) == 5",
    MAYBE_EVAL (TARGET_STRING && TARGET_POWER
   && INTVAL (operands[2]) > 8 && INTVAL (operands[2]) <= 16
   && (REGNO (operands[0]) < 5 || REGNO (operands[0]) > 8)
   && (REGNO (operands[1]) < 5 || REGNO (operands[1]) > 8)
   && REGNO (operands[4]) == 5) },
  { "TARGET_POWERPC64&& reload_completed",
    MAYBE_EVAL (TARGET_POWERPC64&& reload_completed) },
  { "TARGET_64BIT \n\
   && DEFAULT_ABI == ABI_AIX\n\
   && (INTVAL (operands[2]) & CALL_LONG) == 0",
    MAYBE_EVAL (TARGET_64BIT 
   && DEFAULT_ABI == ABI_AIX
   && (INTVAL (operands[2]) & CALL_LONG) == 0) },
  { "!TARGET_SPE",
    MAYBE_EVAL (!TARGET_SPE) },
  { "DEFAULT_ABI == ABI_AIX_NODESC\n\
   || DEFAULT_ABI == ABI_V4\n\
   || DEFAULT_ABI == ABI_DARWIN",
    MAYBE_EVAL (DEFAULT_ABI == ABI_AIX_NODESC
   || DEFAULT_ABI == ABI_V4
   || DEFAULT_ABI == ABI_DARWIN) },
  { "! TARGET_POWERPC64 && TARGET_HARD_FLOAT && TARGET_FPRS",
    MAYBE_EVAL (! TARGET_POWERPC64 && TARGET_HARD_FLOAT && TARGET_FPRS) },
  { "TARGET_POWER",
    MAYBE_EVAL (TARGET_POWER) },
  { "! TARGET_POWER && ! TARGET_POWERPC",
    MAYBE_EVAL (! TARGET_POWER && ! TARGET_POWERPC) },
  { "TARGET_ELF && ! TARGET_64BIT",
    MAYBE_EVAL (TARGET_ELF && ! TARGET_64BIT) },
  { "TARGET_HARD_FLOAT && !TARGET_FPRS && flag_unsafe_math_optimizations",
    MAYBE_EVAL (TARGET_HARD_FLOAT && !TARGET_FPRS && flag_unsafe_math_optimizations) },
  { "! TARGET_POWERPC64 && TARGET_HARD_FLOAT && TARGET_FPRS\n\
   && (gpc_reg_operand (operands[0], DFmode)\n\
       || gpc_reg_operand (operands[1], DFmode))",
    MAYBE_EVAL (! TARGET_POWERPC64 && TARGET_HARD_FLOAT && TARGET_FPRS
   && (gpc_reg_operand (operands[0], DFmode)
       || gpc_reg_operand (operands[1], DFmode))) },
  { "HOST_BITS_PER_WIDE_INT == 32 && ! TARGET_POWERPC64 && reload_completed",
    MAYBE_EVAL (HOST_BITS_PER_WIDE_INT == 32 && ! TARGET_POWERPC64 && reload_completed) },
  { "(DEFAULT_ABI == ABI_V4 && flag_pic == 1)\n\
   || (TARGET_TOC && TARGET_MINIMAL_TOC)\n\
   || (DEFAULT_ABI == ABI_DARWIN && flag_pic)",
    MAYBE_EVAL ((DEFAULT_ABI == ABI_V4 && flag_pic == 1)
   || (TARGET_TOC && TARGET_MINIMAL_TOC)
   || (DEFAULT_ABI == ABI_DARWIN && flag_pic)) },
  { "TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_FUSED_MADD",
    MAYBE_EVAL (TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_FUSED_MADD) },
  { "TARGET_POWERPC && TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_FUSED_MADD",
    MAYBE_EVAL (TARGET_POWERPC && TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_FUSED_MADD) },
  { "TARGET_64BIT && (INTVAL (operands[3]) & CALL_LONG) == 0",
    MAYBE_EVAL (TARGET_64BIT && (INTVAL (operands[3]) & CALL_LONG) == 0) },
  { "TARGET_PPC_GFXOPT && TARGET_HARD_FLOAT && TARGET_FPRS",
    MAYBE_EVAL (TARGET_PPC_GFXOPT && TARGET_HARD_FLOAT && TARGET_FPRS) },
  { "! TARGET_POWER && ! TARGET_POWERPC64 && !TARGET_ISEL",
    MAYBE_EVAL (! TARGET_POWER && ! TARGET_POWERPC64 && !TARGET_ISEL) },
  { "DEFAULT_ABI == ABI_V4\n\
    && flag_pic == 1\n\
    && (reload_in_progress || reload_completed)",
    MAYBE_EVAL (DEFAULT_ABI == ABI_V4
    && flag_pic == 1
    && (reload_in_progress || reload_completed)) },
  { "TARGET_POWERPC64 && reload_completed\n\
   && ((GET_CODE (operands[0]) == REG && REGNO (operands[0]) <= 31)\n\
       || (GET_CODE (operands[0]) == SUBREG\n\
	   && GET_CODE (SUBREG_REG (operands[0])) == REG\n\
	   && REGNO (SUBREG_REG (operands[0])) <= 31))",
    MAYBE_EVAL (TARGET_POWERPC64 && reload_completed
   && ((GET_CODE (operands[0]) == REG && REGNO (operands[0]) <= 31)
       || (GET_CODE (operands[0]) == SUBREG
	   && GET_CODE (SUBREG_REG (operands[0])) == REG
	   && REGNO (SUBREG_REG (operands[0])) <= 31))) },
  { "TARGET_POWER || (! TARGET_POWER && ! TARGET_POWERPC)",
    MAYBE_EVAL (TARGET_POWER || (! TARGET_POWER && ! TARGET_POWERPC)) },
  { "! gpc_reg_operand (operands[2], SImode)",
    MAYBE_EVAL (! gpc_reg_operand (operands[2], SImode)) },
  { "! TARGET_POWERPC64\n\
   && (gpc_reg_operand (operands[0], DImode)\n\
       || gpc_reg_operand (operands[1], DImode))",
    MAYBE_EVAL (! TARGET_POWERPC64
   && (gpc_reg_operand (operands[0], DImode)
       || gpc_reg_operand (operands[1], DImode))) },
  { "TARGET_HARD_FLOAT && !TARGET_FPRS && !flag_unsafe_math_optimizations",
    MAYBE_EVAL (TARGET_HARD_FLOAT && !TARGET_FPRS && !flag_unsafe_math_optimizations) },
  { "TARGET_HARD_FLOAT",
    MAYBE_EVAL (TARGET_HARD_FLOAT) },
  { "DEFAULT_ABI == ABI_AIX && TARGET_HARD_FLOAT\n\
   && TARGET_FPRS && TARGET_LONG_DOUBLE_128",
    MAYBE_EVAL (DEFAULT_ABI == ABI_AIX && TARGET_HARD_FLOAT
   && TARGET_FPRS && TARGET_LONG_DOUBLE_128) },
  { "gpc_reg_operand (operands[0], HImode)\n\
   || gpc_reg_operand (operands[1], HImode)",
    MAYBE_EVAL (gpc_reg_operand (operands[0], HImode)
   || gpc_reg_operand (operands[1], HImode)) },
  { "DEFAULT_ABI == ABI_AIX && TARGET_32BIT",
    MAYBE_EVAL (DEFAULT_ABI == ABI_AIX && TARGET_32BIT) },
  { "! TARGET_POWERPC && TARGET_HARD_FLOAT && TARGET_FPRS",
    MAYBE_EVAL (! TARGET_POWERPC && TARGET_HARD_FLOAT && TARGET_FPRS) },
  { "TARGET_POWERPC64 && num_insns_constant (operands[1], DImode) > 1",
    MAYBE_EVAL (TARGET_POWERPC64 && num_insns_constant (operands[1], DImode) > 1) },
  { "TARGET_32BIT && DEFAULT_ABI == ABI_AIX",
    MAYBE_EVAL (TARGET_32BIT && DEFAULT_ABI == ABI_AIX) },
  { "TARGET_32BIT\n\
   && DEFAULT_ABI == ABI_AIX\n\
   && (INTVAL (operands[2]) & CALL_LONG) == 0",
    MAYBE_EVAL (TARGET_32BIT
   && DEFAULT_ABI == ABI_AIX
   && (INTVAL (operands[2]) & CALL_LONG) == 0) },
  { "TARGET_STRING && TARGET_POWER\n\
   && ((INTVAL (operands[2]) > 24 && INTVAL (operands[2]) < 32)\n\
       || INTVAL (operands[2]) == 0)\n\
   && (REGNO (operands[0]) < 5 || REGNO (operands[0]) > 12)\n\
   && (REGNO (operands[1]) < 5 || REGNO (operands[1]) > 12)\n\
   && REGNO (operands[4]) == 5",
    MAYBE_EVAL (TARGET_STRING && TARGET_POWER
   && ((INTVAL (operands[2]) > 24 && INTVAL (operands[2]) < 32)
       || INTVAL (operands[2]) == 0)
   && (REGNO (operands[0]) < 5 || REGNO (operands[0]) > 12)
   && (REGNO (operands[1]) < 5 || REGNO (operands[1]) > 12)
   && REGNO (operands[4]) == 5) },
  { "TARGET_STRING && !TARGET_POWERPC64",
    MAYBE_EVAL (TARGET_STRING && !TARGET_POWERPC64) },
  { "TARGET_POWER || TARGET_ISEL",
    MAYBE_EVAL (TARGET_POWER || TARGET_ISEL) },
  { "TARGET_POWERPC64 && TARGET_UPDATE",
    MAYBE_EVAL (TARGET_POWERPC64 && TARGET_UPDATE) },
  { "gpc_reg_operand (operands[0], QImode)\n\
   || gpc_reg_operand (operands[1], QImode)",
    MAYBE_EVAL (gpc_reg_operand (operands[0], QImode)
   || gpc_reg_operand (operands[1], QImode)) },
  { "TARGET_STRING && ! TARGET_POWER",
    MAYBE_EVAL (TARGET_STRING && ! TARGET_POWER) },
  { "gpc_reg_operand (operands[0], SImode)\n\
   || gpc_reg_operand (operands[1], SImode)",
    MAYBE_EVAL (gpc_reg_operand (operands[0], SImode)
   || gpc_reg_operand (operands[1], SImode)) },
  { "DEFAULT_ABI == ABI_AIX && TARGET_HARD_FLOAT && TARGET_FPRS\n\
   && TARGET_LONG_DOUBLE_128\n\
   && (gpc_reg_operand (operands[0], TFmode)\n\
       || gpc_reg_operand (operands[1], TFmode))",
    MAYBE_EVAL (DEFAULT_ABI == ABI_AIX && TARGET_HARD_FLOAT && TARGET_FPRS
   && TARGET_LONG_DOUBLE_128
   && (gpc_reg_operand (operands[0], TFmode)
       || gpc_reg_operand (operands[1], TFmode))) },
  { "includes_lshift_p (operands[2], operands[3]) && reload_completed",
    MAYBE_EVAL (includes_lshift_p (operands[2], operands[3]) && reload_completed) },
  { "(INTVAL (operands[2]) & CALL_LONG) == 0",
    MAYBE_EVAL ((INTVAL (operands[2]) & CALL_LONG) == 0) },
  { "! TARGET_POWER&& reload_completed",
    MAYBE_EVAL (! TARGET_POWER&& reload_completed) },
  { "(DEFAULT_ABI == ABI_AIX_NODESC\n\
    || DEFAULT_ABI == ABI_V4\n\
    || DEFAULT_ABI == ABI_DARWIN)\n\
   && (INTVAL (operands[2]) & CALL_LONG) == 0",
    MAYBE_EVAL ((DEFAULT_ABI == ABI_AIX_NODESC
    || DEFAULT_ABI == ABI_V4
    || DEFAULT_ABI == ABI_DARWIN)
   && (INTVAL (operands[2]) & CALL_LONG) == 0) },
  { "TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_FUSED_MADD \n\
   && ! HONOR_SIGNED_ZEROS (DFmode)",
    MAYBE_EVAL (TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_FUSED_MADD 
   && ! HONOR_SIGNED_ZEROS (DFmode)) },
  { "TARGET_POWERPC && TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_FUSED_MADD\n\
   && ! HONOR_SIGNED_ZEROS (SFmode)",
    MAYBE_EVAL (TARGET_POWERPC && TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_FUSED_MADD
   && ! HONOR_SIGNED_ZEROS (SFmode)) },
  { "(DEFAULT_ABI == ABI_DARWIN) && flag_pic",
    MAYBE_EVAL ((DEFAULT_ABI == ABI_DARWIN) && flag_pic) },
  { "TARGET_POWERPC64 || TARGET_POWER",
    MAYBE_EVAL (TARGET_POWERPC64 || TARGET_POWER) },
  { "includes_rshift_p (operands[2], GEN_INT (65535)) && reload_completed",
    MAYBE_EVAL (includes_rshift_p (operands[2], GEN_INT (65535)) && reload_completed) },
  { "! TARGET_POWER && ! TARGET_POWERPC64 && reload_completed",
    MAYBE_EVAL (! TARGET_POWER && ! TARGET_POWERPC64 && reload_completed) },
  { "TARGET_POWERPC64",
    MAYBE_EVAL (TARGET_POWERPC64) },
  { "REGNO (operands[2]) != REGNO (operands[5])",
    MAYBE_EVAL (REGNO (operands[2]) != REGNO (operands[5])) },
  { "! TARGET_POWER && ! TARGET_ISEL",
    MAYBE_EVAL (! TARGET_POWER && ! TARGET_ISEL) },
  { "TARGET_POWERPC && ! TARGET_POWER && ! TARGET_POWERPC64",
    MAYBE_EVAL (TARGET_POWERPC && ! TARGET_POWER && ! TARGET_POWERPC64) },
  { "register_operand (operands[0], CCmode)\n\
   || register_operand (operands[1], CCmode)",
    MAYBE_EVAL (register_operand (operands[0], CCmode)
   || register_operand (operands[1], CCmode)) },
  { "TARGET_POWERPC && reload_completed",
    MAYBE_EVAL (TARGET_POWERPC && reload_completed) },
  { "TARGET_POWERPC && ! TARGET_POWERPC64",
    MAYBE_EVAL (TARGET_POWERPC && ! TARGET_POWERPC64) },
  { "includes_lshift_p (operands[2], operands[3])",
    MAYBE_EVAL (includes_lshift_p (operands[2], operands[3])) },
  { "TARGET_POWERPC64 && find_reg_note (insn, REG_NONNEG, 0)",
    MAYBE_EVAL (TARGET_POWERPC64 && find_reg_note (insn, REG_NONNEG, 0)) },
  { "TARGET_ISEL&& reload_completed",
    MAYBE_EVAL (TARGET_ISEL&& reload_completed) },
  { "TARGET_POWERPC64 && reload_completed\n\
   && includes_rldicr_lshift_p (operands[2], operands[3])",
    MAYBE_EVAL (TARGET_POWERPC64 && reload_completed
   && includes_rldicr_lshift_p (operands[2], operands[3])) },
  { "TARGET_PPC_GFXOPT",
    MAYBE_EVAL (TARGET_PPC_GFXOPT) },
  { "TARGET_64BIT && (INTVAL (operands[2]) & CALL_LONG) == 0",
    MAYBE_EVAL (TARGET_64BIT && (INTVAL (operands[2]) & CALL_LONG) == 0) },
  { "includes_rshift_p (operands[2], operands[3]) && reload_completed",
    MAYBE_EVAL (includes_rshift_p (operands[2], operands[3]) && reload_completed) },
  { "(gpc_reg_operand (operands[0], SFmode)\n\
   || gpc_reg_operand (operands[1], SFmode))\n\
   && (TARGET_HARD_FLOAT && TARGET_FPRS)",
    MAYBE_EVAL ((gpc_reg_operand (operands[0], SFmode)
   || gpc_reg_operand (operands[1], SFmode))
   && (TARGET_HARD_FLOAT && TARGET_FPRS)) },
  { "TARGET_POWERPC64\n\
   && (gpc_reg_operand (operands[0], DImode)\n\
       || gpc_reg_operand (operands[1], DImode))",
    MAYBE_EVAL (TARGET_POWERPC64
   && (gpc_reg_operand (operands[0], DImode)
       || gpc_reg_operand (operands[1], DImode))) },
  { "(32 - (INTVAL (operands[4]) & 31)) >= INTVAL (operands[1])",
    MAYBE_EVAL ((32 - (INTVAL (operands[4]) & 31)) >= INTVAL (operands[1])) },
  { "TARGET_POWERPC && TARGET_POWER",
    MAYBE_EVAL (TARGET_POWERPC && TARGET_POWER) },
  { "TARGET_MACHO && TARGET_HARD_FLOAT && TARGET_FPRS && ! TARGET_64BIT",
    MAYBE_EVAL (TARGET_MACHO && TARGET_HARD_FLOAT && TARGET_FPRS && ! TARGET_64BIT) },
  { "TARGET_STRING && ! TARGET_POWERPC64",
    MAYBE_EVAL (TARGET_STRING && ! TARGET_POWERPC64) },
  { "TARGET_POWERPC && TARGET_HARD_FLOAT && TARGET_FPRS",
    MAYBE_EVAL (TARGET_POWERPC && TARGET_HARD_FLOAT && TARGET_FPRS) },
  { "DEFAULT_ABI == ABI_AIX && TARGET_HARD_FLOAT\n\
   && TARGET_FPRS && TARGET_LONG_DOUBLE_128&& reload_completed",
    MAYBE_EVAL (DEFAULT_ABI == ABI_AIX && TARGET_HARD_FLOAT
   && TARGET_FPRS && TARGET_LONG_DOUBLE_128&& reload_completed) },
  { "GET_CODE (operands[0]) == REG \n\
   && CR_REGNO_P (REGNO (operands[0]))\n\
   && GET_CODE (operands[2]) == CONST_INT\n\
   && INTVAL (operands[2]) == 1 << (75 - REGNO (operands[0]))",
    MAYBE_EVAL (GET_CODE (operands[0]) == REG 
   && CR_REGNO_P (REGNO (operands[0]))
   && GET_CODE (operands[2]) == CONST_INT
   && INTVAL (operands[2]) == 1 << (75 - REGNO (operands[0]))) },
  { "TARGET_POWERPC64 && reload_completed\n\
   && ! gpc_reg_operand (operands[0], DImode)",
    MAYBE_EVAL (TARGET_POWERPC64 && reload_completed
   && ! gpc_reg_operand (operands[0], DImode)) },
  { "(unsigned HOST_WIDE_INT) (INTVAL (operands[1]) + 0x8000) >= 0x10000\n\
   && (INTVAL (operands[1]) & 0xffff) != 0",
    MAYBE_EVAL ((unsigned HOST_WIDE_INT) (INTVAL (operands[1]) + 0x8000) >= 0x10000
   && (INTVAL (operands[1]) & 0xffff) != 0) },
  { "(gpc_reg_operand (operands[0], SFmode)\n\
   || gpc_reg_operand (operands[1], SFmode))\n\
   && (TARGET_SOFT_FLOAT || !TARGET_FPRS)",
    MAYBE_EVAL ((gpc_reg_operand (operands[0], SFmode)
   || gpc_reg_operand (operands[1], SFmode))
   && (TARGET_SOFT_FLOAT || !TARGET_FPRS)) },
  { "DEFAULT_ABI == ABI_AIX && TARGET_POWERPC64\n\
   && TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_LONG_DOUBLE_128",
    MAYBE_EVAL (DEFAULT_ABI == ABI_AIX && TARGET_POWERPC64
   && TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_LONG_DOUBLE_128) },
  { "! TARGET_POWERPC && TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_FUSED_MADD",
    MAYBE_EVAL (! TARGET_POWERPC && TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_FUSED_MADD) },
  { "DEFAULT_ABI == ABI_DARWIN",
    MAYBE_EVAL (DEFAULT_ABI == ABI_DARWIN) },
  { "TARGET_POWERPC",
    MAYBE_EVAL (TARGET_POWERPC) },
  { "TARGET_SPE",
    MAYBE_EVAL (TARGET_SPE) },
  { "TARGET_STRING && XVECLEN (operands[0], 0) == 7",
    MAYBE_EVAL (TARGET_STRING && XVECLEN (operands[0], 0) == 7) },
  { "TARGET_MACHO && ! TARGET_64BIT",
    MAYBE_EVAL (TARGET_MACHO && ! TARGET_64BIT) },
  { "! TARGET_POWER && ! TARGET_POWERPC64",
    MAYBE_EVAL (! TARGET_POWER && ! TARGET_POWERPC64) },
  { "TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_UPDATE",
    MAYBE_EVAL (TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_UPDATE) },
  { "TARGET_POWERPC && ! TARGET_POWER",
    MAYBE_EVAL (TARGET_POWERPC && ! TARGET_POWER) },
  { "TARGET_64BIT",
    MAYBE_EVAL (TARGET_64BIT) },
  { "TARGET_STRING && XVECLEN (operands[0], 0) == 4",
    MAYBE_EVAL (TARGET_STRING && XVECLEN (operands[0], 0) == 4) },
  { "includes_rshift_p (operands[2], operands[3])",
    MAYBE_EVAL (includes_rshift_p (operands[2], operands[3])) },
  { "(DEFAULT_ABI == ABI_AIX_NODESC\n\
    || DEFAULT_ABI == ABI_V4\n\
    || DEFAULT_ABI == ABI_DARWIN)\n\
   && (INTVAL (operands[3]) & CALL_LONG) == 0",
    MAYBE_EVAL ((DEFAULT_ABI == ABI_AIX_NODESC
    || DEFAULT_ABI == ABI_V4
    || DEFAULT_ABI == ABI_DARWIN)
   && (INTVAL (operands[3]) & CALL_LONG) == 0) },
  { "TARGET_POWERPC64 && TARGET_HARD_FLOAT && TARGET_FPRS\n\
   && (gpc_reg_operand (operands[0], DFmode)\n\
       || gpc_reg_operand (operands[1], DFmode))",
    MAYBE_EVAL (TARGET_POWERPC64 && TARGET_HARD_FLOAT && TARGET_FPRS
   && (gpc_reg_operand (operands[0], DFmode)
       || gpc_reg_operand (operands[1], DFmode))) },
  { "! TARGET_POWERPC64 && (TARGET_SOFT_FLOAT || !TARGET_FPRS)\n\
   && (gpc_reg_operand (operands[0], DFmode)\n\
       || gpc_reg_operand (operands[1], DFmode))",
    MAYBE_EVAL (! TARGET_POWERPC64 && (TARGET_SOFT_FLOAT || !TARGET_FPRS)
   && (gpc_reg_operand (operands[0], DFmode)
       || gpc_reg_operand (operands[1], DFmode))) },
  { "! TARGET_POWERPC && TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_FUSED_MADD\n\
   && ! HONOR_SIGNED_ZEROS (SFmode)",
    MAYBE_EVAL (! TARGET_POWERPC && TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_FUSED_MADD
   && ! HONOR_SIGNED_ZEROS (SFmode)) },
  { "TARGET_STRING && ! TARGET_POWER\n\
   && INTVAL (operands[2]) > 8 && INTVAL (operands[2]) <= 16\n\
   && (REGNO (operands[0]) < 5 || REGNO (operands[0]) > 8)\n\
   && (REGNO (operands[1]) < 5 || REGNO (operands[1]) > 8)\n\
   && REGNO (operands[4]) == 5",
    MAYBE_EVAL (TARGET_STRING && ! TARGET_POWER
   && INTVAL (operands[2]) > 8 && INTVAL (operands[2]) <= 16
   && (REGNO (operands[0]) < 5 || REGNO (operands[0]) > 8)
   && (REGNO (operands[1]) < 5 || REGNO (operands[1]) > 8)
   && REGNO (operands[4]) == 5) },
  { "TARGET_POWERPC64 && includes_rldicr_lshift_p (operands[2], operands[3])",
    MAYBE_EVAL (TARGET_POWERPC64 && includes_rldicr_lshift_p (operands[2], operands[3])) },
  { "DEFAULT_ABI == ABI_AIX && TARGET_POWERPC64\n\
   && TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_LONG_DOUBLE_128&& reload_completed",
    MAYBE_EVAL (DEFAULT_ABI == ABI_AIX && TARGET_POWERPC64
   && TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_LONG_DOUBLE_128&& reload_completed) },
  { "TARGET_POWERPC && TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_FUSED_MADD\n\
   && HONOR_SIGNED_ZEROS (SFmode)",
    MAYBE_EVAL (TARGET_POWERPC && TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_FUSED_MADD
   && HONOR_SIGNED_ZEROS (SFmode)) },
  { "! TARGET_POWERPC64 && reload_completed\n\
   && ! gpc_reg_operand (operands[0], SImode)",
    MAYBE_EVAL (! TARGET_POWERPC64 && reload_completed
   && ! gpc_reg_operand (operands[0], SImode)) },
  { "TARGET_STRING && TARGET_POWER && ! TARGET_POWERPC64\n\
   && (gpc_reg_operand (operands[0], TImode) || gpc_reg_operand (operands[1], TImode))",
    MAYBE_EVAL (TARGET_STRING && TARGET_POWER && ! TARGET_POWERPC64
   && (gpc_reg_operand (operands[0], TImode) || gpc_reg_operand (operands[1], TImode))) },
  { "! TARGET_POWER",
    MAYBE_EVAL (! TARGET_POWER) },
  { "(TARGET_SOFT_FLOAT || !TARGET_FPRS) && TARGET_UPDATE",
    MAYBE_EVAL ((TARGET_SOFT_FLOAT || !TARGET_FPRS) && TARGET_UPDATE) },
  { "TARGET_ISEL",
    MAYBE_EVAL (TARGET_ISEL) },
  { "includes_rshift_p (operands[2], GEN_INT (65535))",
    MAYBE_EVAL (includes_rshift_p (operands[2], GEN_INT (65535))) },
  { "(TARGET_POWER2 || TARGET_POWERPC) && TARGET_HARD_FLOAT && TARGET_FPRS",
    MAYBE_EVAL ((TARGET_POWER2 || TARGET_POWERPC) && TARGET_HARD_FLOAT && TARGET_FPRS) },
  { "TARGET_POWERPC64 && reload_completed\n\
    && (fixed_regs[CR0_REGNO] || !logical_operand (operands[2], DImode))\n\
    && !mask64_operand (operands[2], DImode)",
    MAYBE_EVAL (TARGET_POWERPC64 && reload_completed
    && (fixed_regs[CR0_REGNO] || !logical_operand (operands[2], DImode))
    && !mask64_operand (operands[2], DImode)) },
  { "TARGET_HARD_FLOAT && TARGET_FPRS",
    MAYBE_EVAL (TARGET_HARD_FLOAT && TARGET_FPRS) },
  { "TARGET_STRING && TARGET_POWERPC64\n\
   && INTVAL (operands[2]) > 16 && INTVAL (operands[2]) <= 32\n\
   && (REGNO (operands[0]) < 5 || REGNO (operands[0]) > 10)\n\
   && (REGNO (operands[1]) < 5 || REGNO (operands[1]) > 10)\n\
   && REGNO (operands[4]) == 5",
    MAYBE_EVAL (TARGET_STRING && TARGET_POWERPC64
   && INTVAL (operands[2]) > 16 && INTVAL (operands[2]) <= 32
   && (REGNO (operands[0]) < 5 || REGNO (operands[0]) > 10)
   && (REGNO (operands[1]) < 5 || REGNO (operands[1]) > 10)
   && REGNO (operands[4]) == 5) },
  { "TARGET_SPE && INTVAL (operands[2]) >= 0 && INTVAL (operands[2]) <= 31",
    MAYBE_EVAL (TARGET_SPE && INTVAL (operands[2]) >= 0 && INTVAL (operands[2]) <= 31) },
  { "! TARGET_POWERPC",
    MAYBE_EVAL (! TARGET_POWERPC) },
  { "TARGET_ALTIVEC",
    MAYBE_EVAL (TARGET_ALTIVEC) },
  { "TARGET_POWERPC64 && TARGET_HARD_FLOAT && TARGET_FPRS&& reload_completed",
    MAYBE_EVAL (TARGET_POWERPC64 && TARGET_HARD_FLOAT && TARGET_FPRS&& reload_completed) },
  { "TARGET_STRING && ! TARGET_POWER\n\
   && ((INTVAL (operands[2]) > 24 && INTVAL (operands[2]) < 32)\n\
       || INTVAL (operands[2]) == 0)\n\
   && (REGNO (operands[0]) < 5 || REGNO (operands[0]) > 12)\n\
   && (REGNO (operands[1]) < 5 || REGNO (operands[1]) > 12)\n\
   && REGNO (operands[4]) == 5",
    MAYBE_EVAL (TARGET_STRING && ! TARGET_POWER
   && ((INTVAL (operands[2]) > 24 && INTVAL (operands[2]) < 32)
       || INTVAL (operands[2]) == 0)
   && (REGNO (operands[0]) < 5 || REGNO (operands[0]) > 12)
   && (REGNO (operands[1]) < 5 || REGNO (operands[1]) > 12)
   && REGNO (operands[4]) == 5) },
  { "TARGET_STRING && TARGET_POWER\n\
   && INTVAL (operands[2]) > 16 && INTVAL (operands[2]) <= 24\n\
   && (REGNO (operands[0]) < 5 || REGNO (operands[0]) > 10)\n\
   && (REGNO (operands[1]) < 5 || REGNO (operands[1]) > 10)\n\
   && REGNO (operands[4]) == 5",
    MAYBE_EVAL (TARGET_STRING && TARGET_POWER
   && INTVAL (operands[2]) > 16 && INTVAL (operands[2]) <= 24
   && (REGNO (operands[0]) < 5 || REGNO (operands[0]) > 10)
   && (REGNO (operands[1]) < 5 || REGNO (operands[1]) > 10)
   && REGNO (operands[4]) == 5) },
  { "TARGET_POWERPC64\n\
    && (fixed_regs[CR0_REGNO] || !logical_operand (operands[2], DImode))\n\
    && !mask64_operand (operands[2], DImode)",
    MAYBE_EVAL (TARGET_POWERPC64
    && (fixed_regs[CR0_REGNO] || !logical_operand (operands[2], DImode))
    && !mask64_operand (operands[2], DImode)) },
  { "find_single_use (operands[0], insn, 0)\n\
   && (GET_CODE (*find_single_use (operands[0], insn, 0)) == EQ\n\
       || GET_CODE (*find_single_use (operands[0], insn, 0)) == NE)",
    MAYBE_EVAL (find_single_use (operands[0], insn, 0)
   && (GET_CODE (*find_single_use (operands[0], insn, 0)) == EQ
       || GET_CODE (*find_single_use (operands[0], insn, 0)) == NE)) },
  { "TARGET_STRING && TARGET_POWERPC64\n\
   && INTVAL (operands[2]) > 8 && INTVAL (operands[2]) <= 16\n\
   && (REGNO (operands[0]) < 5 || REGNO (operands[0]) > 8)\n\
   && (REGNO (operands[1]) < 5 || REGNO (operands[1]) > 8)\n\
   && REGNO (operands[4]) == 5",
    MAYBE_EVAL (TARGET_STRING && TARGET_POWERPC64
   && INTVAL (operands[2]) > 8 && INTVAL (operands[2]) <= 16
   && (REGNO (operands[0]) < 5 || REGNO (operands[0]) > 8)
   && (REGNO (operands[1]) < 5 || REGNO (operands[1]) > 8)
   && REGNO (operands[4]) == 5) },
  { "TARGET_POWERPC64 && REGNO (operands[2]) != REGNO (operands[5])",
    MAYBE_EVAL (TARGET_POWERPC64 && REGNO (operands[2]) != REGNO (operands[5])) },
  { "! TARGET_POWERPC64 && reload_completed",
    MAYBE_EVAL (! TARGET_POWERPC64 && reload_completed) },
  { "DEFAULT_ABI == ABI_V4 && flag_pic == 1 && TARGET_32BIT",
    MAYBE_EVAL (DEFAULT_ABI == ABI_V4 && flag_pic == 1 && TARGET_32BIT) },
  { "TARGET_SCHED_PROLOG",
    MAYBE_EVAL (TARGET_SCHED_PROLOG) },
  { "TARGET_64BIT \n\
   && DEFAULT_ABI == ABI_AIX\n\
   && (INTVAL (operands[3]) & CALL_LONG) == 0",
    MAYBE_EVAL (TARGET_64BIT 
   && DEFAULT_ABI == ABI_AIX
   && (INTVAL (operands[3]) & CALL_LONG) == 0) },
  { "TARGET_POWERPC64 && reload_completed",
    MAYBE_EVAL (TARGET_POWERPC64 && reload_completed) },
  { "TARGET_64BIT && DEFAULT_ABI == ABI_AIX",
    MAYBE_EVAL (TARGET_64BIT && DEFAULT_ABI == ABI_AIX) },
  { "TARGET_STRING && ! TARGET_POWER\n\
   && INTVAL (operands[2]) > 16 && INTVAL (operands[2]) <= 32\n\
   && (REGNO (operands[0]) < 5 || REGNO (operands[0]) > 10)\n\
   && (REGNO (operands[1]) < 5 || REGNO (operands[1]) > 10)\n\
   && REGNO (operands[4]) == 5",
    MAYBE_EVAL (TARGET_STRING && ! TARGET_POWER
   && INTVAL (operands[2]) > 16 && INTVAL (operands[2]) <= 32
   && (REGNO (operands[0]) < 5 || REGNO (operands[0]) > 10)
   && (REGNO (operands[1]) < 5 || REGNO (operands[1]) > 10)
   && REGNO (operands[4]) == 5) },
  { "HOST_BITS_PER_WIDE_INT == 32 && TARGET_POWERPC64\n\
   && GET_CODE (operands[1]) == CONST_DOUBLE\n\
   && num_insns_constant (operands[1], DImode) == 1",
    MAYBE_EVAL (HOST_BITS_PER_WIDE_INT == 32 && TARGET_POWERPC64
   && GET_CODE (operands[1]) == CONST_DOUBLE
   && num_insns_constant (operands[1], DImode) == 1) },
  { "TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_FUSED_MADD\n\
   && ! HONOR_SIGNED_ZEROS (DFmode)",
    MAYBE_EVAL (TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_FUSED_MADD
   && ! HONOR_SIGNED_ZEROS (DFmode)) },
  { "(INTVAL (operands[3]) & CALL_LONG) == 0",
    MAYBE_EVAL ((INTVAL (operands[3]) & CALL_LONG) == 0) },
  { "(DEFAULT_ABI == ABI_DARWIN\n\
     || DEFAULT_ABI == ABI_V4\n\
     || DEFAULT_ABI == ABI_AIX_NODESC)\n\
   && (INTVAL (operands[2]) & CALL_LONG) == 0",
    MAYBE_EVAL ((DEFAULT_ABI == ABI_DARWIN
     || DEFAULT_ABI == ABI_V4
     || DEFAULT_ABI == ABI_AIX_NODESC)
   && (INTVAL (operands[2]) & CALL_LONG) == 0) },
  { "TARGET_STRING && XVECLEN (operands[0], 0) == 3",
    MAYBE_EVAL (TARGET_STRING && XVECLEN (operands[0], 0) == 3) },
  { "TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_FUSED_MADD\n\
   && HONOR_SIGNED_ZEROS (DFmode)",
    MAYBE_EVAL (TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_FUSED_MADD
   && HONOR_SIGNED_ZEROS (DFmode)) },
  { "TARGET_POWER && reload_completed",
    MAYBE_EVAL (TARGET_POWER && reload_completed) },
  { "(DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_DARWIN)\n\
   && TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_POWERPC64\n\
   && TARGET_LONG_DOUBLE_128 && reload_completed\n\
   && ((GET_CODE (operands[0]) == REG && REGNO (operands[0]) <= 31)\n\
       || (GET_CODE (operands[0]) == SUBREG\n\
	   && GET_CODE (SUBREG_REG (operands[0])) == REG\n\
	   && REGNO (SUBREG_REG (operands[0])) <= 31))",
    MAYBE_EVAL ((DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_DARWIN)
   && TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_POWERPC64
   && TARGET_LONG_DOUBLE_128 && reload_completed
   && ((GET_CODE (operands[0]) == REG && REGNO (operands[0]) <= 31)
       || (GET_CODE (operands[0]) == SUBREG
	   && GET_CODE (SUBREG_REG (operands[0])) == REG
	   && REGNO (SUBREG_REG (operands[0])) <= 31))) },
  { "TARGET_STRING && XVECLEN (operands[0], 0) == 6",
    MAYBE_EVAL (TARGET_STRING && XVECLEN (operands[0], 0) == 6) },
  { "includes_rshift_p (operands[2], GEN_INT (255)) && reload_completed",
    MAYBE_EVAL (includes_rshift_p (operands[2], GEN_INT (255)) && reload_completed) },
  { "TARGET_POWERPC || (! TARGET_POWER && ! TARGET_POWERPC)",
    MAYBE_EVAL (TARGET_POWERPC || (! TARGET_POWER && ! TARGET_POWERPC)) },
  { "TARGET_STRING && ! TARGET_POWER\n\
   && INTVAL (operands[2]) > 0 && INTVAL (operands[2]) <= 4",
    MAYBE_EVAL (TARGET_STRING && ! TARGET_POWER
   && INTVAL (operands[2]) > 0 && INTVAL (operands[2]) <= 4) },
  { "! TARGET_POWER && reload_completed",
    MAYBE_EVAL (! TARGET_POWER && reload_completed) },
  { "TARGET_POWERPC && ! TARGET_POWERPC64 && reload_completed",
    MAYBE_EVAL (TARGET_POWERPC && ! TARGET_POWERPC64 && reload_completed) },
  { "DEFAULT_ABI == ABI_AIX && TARGET_64BIT",
    MAYBE_EVAL (DEFAULT_ABI == ABI_AIX && TARGET_64BIT) },
  { "TARGET_ALTIVEC && TARGET_FUSED_MADD",
    MAYBE_EVAL (TARGET_ALTIVEC && TARGET_FUSED_MADD) },
  { "DEFAULT_ABI == ABI_AIX && TARGET_HARD_FLOAT && TARGET_FPRS\n\
   && TARGET_LONG_DOUBLE_128",
    MAYBE_EVAL (DEFAULT_ABI == ABI_AIX && TARGET_HARD_FLOAT && TARGET_FPRS
   && TARGET_LONG_DOUBLE_128) },
  { "TARGET_STRING && TARGET_POWER",
    MAYBE_EVAL (TARGET_STRING && TARGET_POWER) },
  { "TARGET_STRING && TARGET_POWER\n\
   && INTVAL (operands[2]) > 0 && INTVAL (operands[2]) <= 4",
    MAYBE_EVAL (TARGET_STRING && TARGET_POWER
   && INTVAL (operands[2]) > 0 && INTVAL (operands[2]) <= 4) },
  { "TARGET_STRING || TARGET_POWERPC64",
    MAYBE_EVAL (TARGET_STRING || TARGET_POWERPC64) },
  { "TARGET_STRING && ! TARGET_POWER && ! TARGET_POWERPC64\n\
   && (gpc_reg_operand (operands[0], TImode) || gpc_reg_operand (operands[1], TImode))",
    MAYBE_EVAL (TARGET_STRING && ! TARGET_POWER && ! TARGET_POWERPC64
   && (gpc_reg_operand (operands[0], TImode) || gpc_reg_operand (operands[1], TImode))) },
  { "TARGET_STRING && ! TARGET_POWER && ! TARGET_POWERPC64\n\
   && INTVAL (operands[2]) > 4 && INTVAL (operands[2]) <= 8",
    MAYBE_EVAL (TARGET_STRING && ! TARGET_POWER && ! TARGET_POWERPC64
   && INTVAL (operands[2]) > 4 && INTVAL (operands[2]) <= 8) },
  { "(TARGET_PPC_GPOPT || TARGET_POWER2) && TARGET_HARD_FLOAT && TARGET_FPRS",
    MAYBE_EVAL ((TARGET_PPC_GPOPT || TARGET_POWER2) && TARGET_HARD_FLOAT && TARGET_FPRS) },
  { "! TARGET_POWERPC64 && find_reg_note (insn, REG_NONNEG, 0)",
    MAYBE_EVAL (! TARGET_POWERPC64 && find_reg_note (insn, REG_NONNEG, 0)) },
};

const size_t n_insn_conditions = 183;
const int insn_elision_unavailable = 0;
