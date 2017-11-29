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
  { "INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV",
    MAYBE_EVAL (INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV) },
  { "!no_new_pseudos || reload_completed",
    MAYBE_EVAL (!no_new_pseudos || reload_completed) },
  { "reload_completed && ! TARGET_NO_PIC",
    MAYBE_EVAL (reload_completed && ! TARGET_NO_PIC) },
  { "INTEL_EXTENDED_IEEE_FORMAT && ia64_move_ok (operands[0], operands[1])",
    MAYBE_EVAL (INTEL_EXTENDED_IEEE_FORMAT && ia64_move_ok (operands[0], operands[1])) },
  { "reload_completed",
    MAYBE_EVAL (reload_completed) },
  { "INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV",
    MAYBE_EVAL (INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV) },
  { "INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_THR&& reload_completed",
    MAYBE_EVAL (INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_THR&& reload_completed) },
  { "INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_THR",
    MAYBE_EVAL (INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_THR) },
  { "addp4_optimize_ok (operands[1], operands[2])",
    MAYBE_EVAL (addp4_optimize_ok (operands[1], operands[2])) },
  { "reload_completed\n\
   && GET_CODE (operands[0]) == REG && PR_REGNO_P (REGNO (operands[0]))\n\
   && GET_CODE (operands[1]) == REG && PR_REGNO_P (REGNO (operands[1]))",
    MAYBE_EVAL (reload_completed
   && GET_CODE (operands[0]) == REG && PR_REGNO_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && PR_REGNO_P (REGNO (operands[1]))) },
  { "TARGET_TLS64",
    MAYBE_EVAL (TARGET_TLS64) },
  { "reload_completed\n\
   && GET_CODE (operands[0]) == REG && PR_REGNO_P (REGNO (operands[0]))\n\
   && rtx_equal_p (operands[0], operands[1])",
    MAYBE_EVAL (reload_completed
   && GET_CODE (operands[0]) == REG && PR_REGNO_P (REGNO (operands[0]))
   && rtx_equal_p (operands[0], operands[1])) },
  { "INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_LAT",
    MAYBE_EVAL (INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_LAT) },
  { "reload_completed && find_reg_note (insn, REG_NORETURN, NULL_RTX)",
    MAYBE_EVAL (reload_completed && find_reg_note (insn, REG_NORETURN, NULL_RTX)) },
  { "REGNO (operands[3]) == REGNO (operands[0])\n\
   && REGNO (operands[4]) == REGNO (operands[0]) + 1\n\
   && REGNO (operands[4]) == REGNO (operands[2]) + 1\n\
   && REGNO (operands[6]) == REGNO (operands[2])",
    MAYBE_EVAL (REGNO (operands[3]) == REGNO (operands[0])
   && REGNO (operands[4]) == REGNO (operands[0]) + 1
   && REGNO (operands[4]) == REGNO (operands[2]) + 1
   && REGNO (operands[6]) == REGNO (operands[2])) },
  { "(gr_register_operand (operands[3], DImode) && INTVAL (operands[1]) <= 16)\n\
   || operands[3] == const0_rtx || operands[3] == constm1_rtx",
    MAYBE_EVAL ((gr_register_operand (operands[3], DImode) && INTVAL (operands[1]) <= 16)
   || operands[3] == const0_rtx || operands[3] == constm1_rtx) },
  { "TARGET_TLS14",
    MAYBE_EVAL (TARGET_TLS14) },
  { "INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV_THR&& reload_completed",
    MAYBE_EVAL (INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV_THR&& reload_completed) },
  { "reload_in_progress",
    MAYBE_EVAL (reload_in_progress) },
  { "INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV&& reload_completed",
    MAYBE_EVAL (INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV&& reload_completed) },
  { "reload_completed\n\
   && GET_CODE (operands[0]) == REG && GR_REGNO_P (REGNO (operands[0]))\n\
   && GET_CODE (operands[1]) == REG && PR_REGNO_P (REGNO (operands[1]))",
    MAYBE_EVAL (reload_completed
   && GET_CODE (operands[0]) == REG && GR_REGNO_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && PR_REGNO_P (REGNO (operands[1]))) },
  { "reload_completed\n\
   && GET_CODE (operands[0]) == REG && PR_REGNO_P (REGNO (operands[0]))\n\
   && GET_CODE (operands[1]) == REG && PR_REGNO_P (REGNO (operands[1]))\n\
   && ! rtx_equal_p (operands[0], operands[1])",
    MAYBE_EVAL (reload_completed
   && GET_CODE (operands[0]) == REG && PR_REGNO_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && PR_REGNO_P (REGNO (operands[1]))
   && ! rtx_equal_p (operands[0], operands[1])) },
  { "reload_completed\n\
   && GET_CODE (operands[0]) == REG && PR_REGNO_P (REGNO (operands[0]))\n\
   && GET_CODE (operands[2]) == REG && PR_REGNO_P (REGNO (operands[2]))",
    MAYBE_EVAL (reload_completed
   && GET_CODE (operands[0]) == REG && PR_REGNO_P (REGNO (operands[0]))
   && GET_CODE (operands[2]) == REG && PR_REGNO_P (REGNO (operands[2]))) },
  { "TARGET_TLS22",
    MAYBE_EVAL (TARGET_TLS22) },
  { "INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV_LAT",
    MAYBE_EVAL (INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV_LAT) },
  { "CONST_OK_FOR_M (INTVAL (operands[2]))\n\
   && ia64_depz_field_mask (operands[3], operands[2]) > 0",
    MAYBE_EVAL (CONST_OK_FOR_M (INTVAL (operands[2]))
   && ia64_depz_field_mask (operands[3], operands[2]) > 0) },
  { "!TARGET_TLS64",
    MAYBE_EVAL (!TARGET_TLS64) },
  { "ia64_direct_return ()",
    MAYBE_EVAL (ia64_direct_return ()) },
  { "!INTEL_EXTENDED_IEEE_FORMAT",
    MAYBE_EVAL (!INTEL_EXTENDED_IEEE_FORMAT) },
  { "INTEL_EXTENDED_IEEE_FORMAT",
    MAYBE_EVAL (INTEL_EXTENDED_IEEE_FORMAT) },
  { "ia64_move_ok (operands[0], operands[2])\n\
   && ia64_move_ok (operands[0], operands[3])",
    MAYBE_EVAL (ia64_move_ok (operands[0], operands[2])
   && ia64_move_ok (operands[0], operands[3])) },
  { "! reload_completed",
    MAYBE_EVAL (! reload_completed) },
  { "ia64_move_ok (operands[0], operands[1])",
    MAYBE_EVAL (ia64_move_ok (operands[0], operands[1])) },
  { "INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV_LAT&& reload_completed",
    MAYBE_EVAL (INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV_LAT&& reload_completed) },
  { "reload_completed && rtx_equal_p (operands[0], operands[3])",
    MAYBE_EVAL (reload_completed && rtx_equal_p (operands[0], operands[3])) },
  { "INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV_THR",
    MAYBE_EVAL (INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV_THR) },
  { "INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_LAT&& reload_completed",
    MAYBE_EVAL (INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_LAT&& reload_completed) },
};

const size_t n_insn_conditions = 37;
const int insn_elision_unavailable = 0;
