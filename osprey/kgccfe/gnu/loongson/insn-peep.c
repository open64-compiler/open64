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
  if (GET_CODE (x) != SET) goto L554;
  x = XEXP (pat, 0);
  operands[0] = x;
  if (! register_operand (x, SImode)) goto L554;
  x = XEXP (pat, 1);
  operands[1] = x;
  if (! register_operand (x, SImode)) goto L554;
  do { insn = NEXT_INSN (insn);
       if (insn == 0) goto L554; }
  while (GET_CODE (insn) == NOTE
	 || (GET_CODE (insn) == INSN
	     && (GET_CODE (PATTERN (insn)) == USE
		 || GET_CODE (PATTERN (insn)) == CLOBBER)));
  if (GET_CODE (insn) == CODE_LABEL
      || GET_CODE (insn) == BARRIER)
    goto L554;
  pat = PATTERN (insn);
  x = pat;
  if (GET_CODE (x) != SET) goto L554;
  x = XEXP (pat, 0);
  if (GET_CODE (x) != PC) goto L554;
  x = XEXP (pat, 1);
  if (GET_CODE (x) != IF_THEN_ELSE) goto L554;
  x = XEXP (XEXP (pat, 1), 0);
  operands[2] = x;
  if (! equality_op (x, SImode)) goto L554;
  x = XEXP (XEXP (XEXP (pat, 1), 0), 0);
  if (!rtx_equal_p (operands[0], x)) goto L554;
  x = XEXP (XEXP (XEXP (pat, 1), 0), 1);
  if (GET_CODE (x) != CONST_INT) goto L554;
  if (XWINT (x, 0) != 0) goto L554;
  x = XEXP (XEXP (pat, 1), 1);
  operands[3] = x;
  if (! pc_or_label_operand (x, VOIDmode)) goto L554;
  x = XEXP (XEXP (pat, 1), 2);
  operands[4] = x;
  if (! pc_or_label_operand (x, VOIDmode)) goto L554;
  if (! (TARGET_MIPS16
   && GET_CODE (operands[0]) == REG
   && REGNO (operands[0]) == 24
   && dead_or_set_p (insn, operands[0])
   && GET_CODE (operands[1]) == REG
   && M16_REG_P (REGNO (operands[1])))) goto L554;
  PATTERN (ins1) = gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (5, operands));
  INSN_CODE (ins1) = 554;
  delete_for_peephole (NEXT_INSN (ins1), insn);
  return NEXT_INSN (insn);
 L554:

  insn = ins1;
  pat = PATTERN (insn);
  x = pat;
  if (GET_CODE (x) != SET) goto L555;
  x = XEXP (pat, 0);
  operands[0] = x;
  if (! register_operand (x, DImode)) goto L555;
  x = XEXP (pat, 1);
  operands[1] = x;
  if (! register_operand (x, DImode)) goto L555;
  do { insn = NEXT_INSN (insn);
       if (insn == 0) goto L555; }
  while (GET_CODE (insn) == NOTE
	 || (GET_CODE (insn) == INSN
	     && (GET_CODE (PATTERN (insn)) == USE
		 || GET_CODE (PATTERN (insn)) == CLOBBER)));
  if (GET_CODE (insn) == CODE_LABEL
      || GET_CODE (insn) == BARRIER)
    goto L555;
  pat = PATTERN (insn);
  x = pat;
  if (GET_CODE (x) != SET) goto L555;
  x = XEXP (pat, 0);
  if (GET_CODE (x) != PC) goto L555;
  x = XEXP (pat, 1);
  if (GET_CODE (x) != IF_THEN_ELSE) goto L555;
  x = XEXP (XEXP (pat, 1), 0);
  operands[2] = x;
  if (! equality_op (x, DImode)) goto L555;
  x = XEXP (XEXP (XEXP (pat, 1), 0), 0);
  if (!rtx_equal_p (operands[0], x)) goto L555;
  x = XEXP (XEXP (XEXP (pat, 1), 0), 1);
  if (GET_CODE (x) != CONST_INT) goto L555;
  if (XWINT (x, 0) != 0) goto L555;
  x = XEXP (XEXP (pat, 1), 1);
  operands[3] = x;
  if (! pc_or_label_operand (x, VOIDmode)) goto L555;
  x = XEXP (XEXP (pat, 1), 2);
  operands[4] = x;
  if (! pc_or_label_operand (x, VOIDmode)) goto L555;
  if (! (TARGET_MIPS16 && TARGET_64BIT
   && GET_CODE (operands[0]) == REG
   && REGNO (operands[0]) == 24
   && dead_or_set_p (insn, operands[0])
   && GET_CODE (operands[1]) == REG
   && M16_REG_P (REGNO (operands[1])))) goto L555;
  PATTERN (ins1) = gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (5, operands));
  INSN_CODE (ins1) = 555;
  delete_for_peephole (NEXT_INSN (ins1), insn);
  return NEXT_INSN (insn);
 L555:

  insn = ins1;
  pat = PATTERN (insn);
  x = pat;
  if (GET_CODE (x) != SET) goto L556;
  x = XEXP (pat, 0);
  operands[0] = x;
  if (! register_operand (x, SImode)) goto L556;
  x = XEXP (pat, 1);
  operands[1] = x;
  if (! register_operand (x, SImode)) goto L556;
  do { insn = NEXT_INSN (insn);
       if (insn == 0) goto L556; }
  while (GET_CODE (insn) == NOTE
	 || (GET_CODE (insn) == INSN
	     && (GET_CODE (PATTERN (insn)) == USE
		 || GET_CODE (PATTERN (insn)) == CLOBBER)));
  if (GET_CODE (insn) == CODE_LABEL
      || GET_CODE (insn) == BARRIER)
    goto L556;
  pat = PATTERN (insn);
  x = pat;
  if (GET_CODE (x) != SET) goto L556;
  x = XEXP (pat, 0);
  if (GET_CODE (x) != PC) goto L556;
  x = XEXP (pat, 1);
  if (GET_CODE (x) != IF_THEN_ELSE) goto L556;
  x = XEXP (XEXP (pat, 1), 0);
  operands[2] = x;
  if (! equality_op (x, SImode)) goto L556;
  x = XEXP (XEXP (XEXP (pat, 1), 0), 0);
  if (!rtx_equal_p (operands[0], x)) goto L556;
  x = XEXP (XEXP (XEXP (pat, 1), 0), 1);
  if (GET_CODE (x) != CONST_INT) goto L556;
  if (XWINT (x, 0) != 0) goto L556;
  x = XEXP (XEXP (pat, 1), 1);
  operands[3] = x;
  if (! pc_or_label_operand (x, VOIDmode)) goto L556;
  x = XEXP (XEXP (pat, 1), 2);
  operands[4] = x;
  if (! pc_or_label_operand (x, VOIDmode)) goto L556;
  if (! (TARGET_MIPS16
   && GET_CODE (operands[1]) == REG
   && REGNO (operands[1]) == 24
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && dead_or_set_p (insn, operands[0]))) goto L556;
  PATTERN (ins1) = gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (5, operands));
  INSN_CODE (ins1) = 556;
  delete_for_peephole (NEXT_INSN (ins1), insn);
  return NEXT_INSN (insn);
 L556:

  insn = ins1;
  pat = PATTERN (insn);
  x = pat;
  if (GET_CODE (x) != SET) goto L557;
  x = XEXP (pat, 0);
  operands[0] = x;
  if (! register_operand (x, DImode)) goto L557;
  x = XEXP (pat, 1);
  operands[1] = x;
  if (! register_operand (x, DImode)) goto L557;
  do { insn = NEXT_INSN (insn);
       if (insn == 0) goto L557; }
  while (GET_CODE (insn) == NOTE
	 || (GET_CODE (insn) == INSN
	     && (GET_CODE (PATTERN (insn)) == USE
		 || GET_CODE (PATTERN (insn)) == CLOBBER)));
  if (GET_CODE (insn) == CODE_LABEL
      || GET_CODE (insn) == BARRIER)
    goto L557;
  pat = PATTERN (insn);
  x = pat;
  if (GET_CODE (x) != SET) goto L557;
  x = XEXP (pat, 0);
  if (GET_CODE (x) != PC) goto L557;
  x = XEXP (pat, 1);
  if (GET_CODE (x) != IF_THEN_ELSE) goto L557;
  x = XEXP (XEXP (pat, 1), 0);
  operands[2] = x;
  if (! equality_op (x, DImode)) goto L557;
  x = XEXP (XEXP (XEXP (pat, 1), 0), 0);
  if (!rtx_equal_p (operands[0], x)) goto L557;
  x = XEXP (XEXP (XEXP (pat, 1), 0), 1);
  if (GET_CODE (x) != CONST_INT) goto L557;
  if (XWINT (x, 0) != 0) goto L557;
  x = XEXP (XEXP (pat, 1), 1);
  operands[3] = x;
  if (! pc_or_label_operand (x, VOIDmode)) goto L557;
  x = XEXP (XEXP (pat, 1), 2);
  operands[4] = x;
  if (! pc_or_label_operand (x, VOIDmode)) goto L557;
  if (! (TARGET_MIPS16 && TARGET_64BIT
   && GET_CODE (operands[1]) == REG
   && REGNO (operands[1]) == 24
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && dead_or_set_p (insn, operands[0]))) goto L557;
  PATTERN (ins1) = gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (5, operands));
  INSN_CODE (ins1) = 557;
  delete_for_peephole (NEXT_INSN (ins1), insn);
  return NEXT_INSN (insn);
 L557:

  return 0;
}

rtx peep_operand[5];
#endif
