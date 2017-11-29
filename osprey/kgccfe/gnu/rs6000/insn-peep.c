/* Generated automatically by the program `genpeep'
from the machine description file `md'.  */

#include "config.h"
#include "system.h"
#include "insn-config.h"
#include "rtl.h"
#include "tm_p.h"
#include "regs.h"
#include "output.h"
#include "real.h"
#include "recog.h"
#include "except.h"

#include "function.h"

#ifdef HAVE_peephole
extern rtx peep_operand[];

#define operands peep_operand

rtx
peephole (ins1)
     rtx ins1;
{
  rtx insn ATTRIBUTE_UNUSED, x ATTRIBUTE_UNUSED, pat ATTRIBUTE_UNUSED;

  if (NEXT_INSN (ins1)
      && GET_CODE (NEXT_INSN (ins1)) == BARRIER)
    return 0;

  insn = ins1;
  pat = PATTERN (insn);
  x = pat;
  if (GET_CODE (x) != SET) goto L1015;
  x = XEXP (pat, 0);
  operands[0] = x;
  if (! gpc_reg_operand (x, DFmode)) goto L1015;
  x = XEXP (pat, 1);
  operands[1] = x;
  if (! memory_operand (x, DFmode)) goto L1015;
  do { insn = NEXT_INSN (insn);
       if (insn == 0) goto L1015; }
  while (GET_CODE (insn) == NOTE
	 || (GET_CODE (insn) == INSN
	     && (GET_CODE (PATTERN (insn)) == USE
		 || GET_CODE (PATTERN (insn)) == CLOBBER)));
  if (GET_CODE (insn) == CODE_LABEL
      || GET_CODE (insn) == BARRIER)
    goto L1015;
  pat = PATTERN (insn);
  x = pat;
  if (GET_CODE (x) != SET) goto L1015;
  x = XEXP (pat, 0);
  operands[2] = x;
  if (! gpc_reg_operand (x, DFmode)) goto L1015;
  x = XEXP (pat, 1);
  operands[3] = x;
  if (! memory_operand (x, DFmode)) goto L1015;
  if (! (TARGET_POWER2
   && TARGET_HARD_FLOAT && TARGET_FPRS
   && registers_ok_for_quad_peep (operands[0], operands[2])
   && ! MEM_VOLATILE_P (operands[1]) && ! MEM_VOLATILE_P (operands[3])
   && addrs_ok_for_quad_peep (XEXP (operands[1], 0), XEXP (operands[3], 0)))) goto L1015;
  PATTERN (ins1) = gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (4, operands));
  INSN_CODE (ins1) = 1015;
  delete_for_peephole (NEXT_INSN (ins1), insn);
  return NEXT_INSN (insn);
 L1015:

  insn = ins1;
  pat = PATTERN (insn);
  x = pat;
  if (GET_CODE (x) != SET) goto L1016;
  x = XEXP (pat, 0);
  operands[0] = x;
  if (! memory_operand (x, DFmode)) goto L1016;
  x = XEXP (pat, 1);
  operands[1] = x;
  if (! gpc_reg_operand (x, DFmode)) goto L1016;
  do { insn = NEXT_INSN (insn);
       if (insn == 0) goto L1016; }
  while (GET_CODE (insn) == NOTE
	 || (GET_CODE (insn) == INSN
	     && (GET_CODE (PATTERN (insn)) == USE
		 || GET_CODE (PATTERN (insn)) == CLOBBER)));
  if (GET_CODE (insn) == CODE_LABEL
      || GET_CODE (insn) == BARRIER)
    goto L1016;
  pat = PATTERN (insn);
  x = pat;
  if (GET_CODE (x) != SET) goto L1016;
  x = XEXP (pat, 0);
  operands[2] = x;
  if (! memory_operand (x, DFmode)) goto L1016;
  x = XEXP (pat, 1);
  operands[3] = x;
  if (! gpc_reg_operand (x, DFmode)) goto L1016;
  if (! (TARGET_POWER2
   && TARGET_HARD_FLOAT && TARGET_FPRS
   && registers_ok_for_quad_peep (operands[1], operands[3])
   && ! MEM_VOLATILE_P (operands[0]) && ! MEM_VOLATILE_P (operands[2])
   && addrs_ok_for_quad_peep (XEXP (operands[0], 0), XEXP (operands[2], 0)))) goto L1016;
  PATTERN (ins1) = gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (4, operands));
  INSN_CODE (ins1) = 1016;
  delete_for_peephole (NEXT_INSN (ins1), insn);
  return NEXT_INSN (insn);
 L1016:

  insn = ins1;
  pat = PATTERN (insn);
  x = pat;
  if (GET_CODE (x) != SET) goto L1072;
  x = XEXP (pat, 0);
  operands[0] = x;
  if (! gpc_reg_operand (x, SImode)) goto L1072;
  x = XEXP (pat, 1);
  operands[1] = x;
  if (! scc_comparison_operator (x, SImode)) goto L1072;
  x = XEXP (XEXP (pat, 1), 0);
  operands[2] = x;
  if (! cc_reg_operand (x, VOIDmode)) goto L1072;
  x = XEXP (XEXP (pat, 1), 1);
  if (GET_CODE (x) != CONST_INT) goto L1072;
  if (XWINT (x, 0) != 0) goto L1072;
  do { insn = NEXT_INSN (insn);
       if (insn == 0) goto L1072; }
  while (GET_CODE (insn) == NOTE
	 || (GET_CODE (insn) == INSN
	     && (GET_CODE (PATTERN (insn)) == USE
		 || GET_CODE (PATTERN (insn)) == CLOBBER)));
  if (GET_CODE (insn) == CODE_LABEL
      || GET_CODE (insn) == BARRIER)
    goto L1072;
  pat = PATTERN (insn);
  x = pat;
  if (GET_CODE (x) != SET) goto L1072;
  x = XEXP (pat, 0);
  operands[3] = x;
  if (! gpc_reg_operand (x, SImode)) goto L1072;
  x = XEXP (pat, 1);
  operands[4] = x;
  if (! scc_comparison_operator (x, SImode)) goto L1072;
  x = XEXP (XEXP (pat, 1), 0);
  operands[5] = x;
  if (! cc_reg_operand (x, VOIDmode)) goto L1072;
  x = XEXP (XEXP (pat, 1), 1);
  if (GET_CODE (x) != CONST_INT) goto L1072;
  if (XWINT (x, 0) != 0) goto L1072;
  if (! (REGNO (operands[2]) != REGNO (operands[5]))) goto L1072;
  PATTERN (ins1) = gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (6, operands));
  INSN_CODE (ins1) = 1072;
  delete_for_peephole (NEXT_INSN (ins1), insn);
  return NEXT_INSN (insn);
 L1072:

  insn = ins1;
  pat = PATTERN (insn);
  x = pat;
  if (GET_CODE (x) != SET) goto L1073;
  x = XEXP (pat, 0);
  operands[0] = x;
  if (! gpc_reg_operand (x, DImode)) goto L1073;
  x = XEXP (pat, 1);
  operands[1] = x;
  if (! scc_comparison_operator (x, DImode)) goto L1073;
  x = XEXP (XEXP (pat, 1), 0);
  operands[2] = x;
  if (! cc_reg_operand (x, VOIDmode)) goto L1073;
  x = XEXP (XEXP (pat, 1), 1);
  if (GET_CODE (x) != CONST_INT) goto L1073;
  if (XWINT (x, 0) != 0) goto L1073;
  do { insn = NEXT_INSN (insn);
       if (insn == 0) goto L1073; }
  while (GET_CODE (insn) == NOTE
	 || (GET_CODE (insn) == INSN
	     && (GET_CODE (PATTERN (insn)) == USE
		 || GET_CODE (PATTERN (insn)) == CLOBBER)));
  if (GET_CODE (insn) == CODE_LABEL
      || GET_CODE (insn) == BARRIER)
    goto L1073;
  pat = PATTERN (insn);
  x = pat;
  if (GET_CODE (x) != SET) goto L1073;
  x = XEXP (pat, 0);
  operands[3] = x;
  if (! gpc_reg_operand (x, DImode)) goto L1073;
  x = XEXP (pat, 1);
  operands[4] = x;
  if (! scc_comparison_operator (x, DImode)) goto L1073;
  x = XEXP (XEXP (pat, 1), 0);
  operands[5] = x;
  if (! cc_reg_operand (x, VOIDmode)) goto L1073;
  x = XEXP (XEXP (pat, 1), 1);
  if (GET_CODE (x) != CONST_INT) goto L1073;
  if (XWINT (x, 0) != 0) goto L1073;
  if (! (TARGET_POWERPC64 && REGNO (operands[2]) != REGNO (operands[5]))) goto L1073;
  PATTERN (ins1) = gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (6, operands));
  INSN_CODE (ins1) = 1073;
  delete_for_peephole (NEXT_INSN (ins1), insn);
  return NEXT_INSN (insn);
 L1073:

  return 0;
}

rtx peep_operand[6];
#endif
