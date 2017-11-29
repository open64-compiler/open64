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
  if (GET_CODE (x) != SET) goto L529;
  x = XEXP (pat, 0);
  operands[0] = x;
  if (! register_operand (x, SImode)) goto L529;
  x = XEXP (pat, 1);
  operands[1] = x;
  if (! register_operand (x, SImode)) goto L529;
  do { insn = NEXT_INSN (insn);
       if (insn == 0) goto L529; }
  while (GET_CODE (insn) == NOTE
	 || (GET_CODE (insn) == INSN
	     && (GET_CODE (PATTERN (insn)) == USE
		 || GET_CODE (PATTERN (insn)) == CLOBBER)));
  if (GET_CODE (insn) == CODE_LABEL
      || GET_CODE (insn) == BARRIER)
    goto L529;
  pat = PATTERN (insn);
  x = pat;
  if (GET_CODE (x) != SET) goto L529;
  x = XEXP (pat, 0);
  if (GET_CODE (x) != PC) goto L529;
  x = XEXP (pat, 1);
  if (GET_CODE (x) != IF_THEN_ELSE) goto L529;
  x = XEXP (XEXP (pat, 1), 0);
  operands[2] = x;
  if (! equality_op (x, SImode)) goto L529;
  x = XEXP (XEXP (XEXP (pat, 1), 0), 0);
  if (!rtx_equal_p (operands[0], x)) goto L529;
  x = XEXP (XEXP (XEXP (pat, 1), 0), 1);
  if (GET_CODE (x) != CONST_INT) goto L529;
  if (XWINT (x, 0) != 0) goto L529;
  x = XEXP (XEXP (pat, 1), 1);
  operands[3] = x;
  if (! pc_or_label_operand (x, VOIDmode)) goto L529;
  x = XEXP (XEXP (pat, 1), 2);
  operands[4] = x;
  if (! pc_or_label_operand (x, VOIDmode)) goto L529;
  if (! (TARGET_MIPS16
   && GET_CODE (operands[0]) == REG
   && REGNO (operands[0]) == 24
   && dead_or_set_p (insn, operands[0])
   && GET_CODE (operands[1]) == REG
   && M16_REG_P (REGNO (operands[1])))) goto L529;
  PATTERN (ins1) = gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (5, operands));
  INSN_CODE (ins1) = 529;
  delete_for_peephole (NEXT_INSN (ins1), insn);
  return NEXT_INSN (insn);
 L529:

  insn = ins1;
  pat = PATTERN (insn);
  x = pat;
  if (GET_CODE (x) != SET) goto L530;
  x = XEXP (pat, 0);
  operands[0] = x;
  if (! register_operand (x, DImode)) goto L530;
  x = XEXP (pat, 1);
  operands[1] = x;
  if (! register_operand (x, DImode)) goto L530;
  do { insn = NEXT_INSN (insn);
       if (insn == 0) goto L530; }
  while (GET_CODE (insn) == NOTE
	 || (GET_CODE (insn) == INSN
	     && (GET_CODE (PATTERN (insn)) == USE
		 || GET_CODE (PATTERN (insn)) == CLOBBER)));
  if (GET_CODE (insn) == CODE_LABEL
      || GET_CODE (insn) == BARRIER)
    goto L530;
  pat = PATTERN (insn);
  x = pat;
  if (GET_CODE (x) != SET) goto L530;
  x = XEXP (pat, 0);
  if (GET_CODE (x) != PC) goto L530;
  x = XEXP (pat, 1);
  if (GET_CODE (x) != IF_THEN_ELSE) goto L530;
  x = XEXP (XEXP (pat, 1), 0);
  operands[2] = x;
  if (! equality_op (x, DImode)) goto L530;
  x = XEXP (XEXP (XEXP (pat, 1), 0), 0);
  if (!rtx_equal_p (operands[0], x)) goto L530;
  x = XEXP (XEXP (XEXP (pat, 1), 0), 1);
  if (GET_CODE (x) != CONST_INT) goto L530;
  if (XWINT (x, 0) != 0) goto L530;
  x = XEXP (XEXP (pat, 1), 1);
  operands[3] = x;
  if (! pc_or_label_operand (x, VOIDmode)) goto L530;
  x = XEXP (XEXP (pat, 1), 2);
  operands[4] = x;
  if (! pc_or_label_operand (x, VOIDmode)) goto L530;
  if (! (TARGET_MIPS16 && TARGET_64BIT
   && GET_CODE (operands[0]) == REG
   && REGNO (operands[0]) == 24
   && dead_or_set_p (insn, operands[0])
   && GET_CODE (operands[1]) == REG
   && M16_REG_P (REGNO (operands[1])))) goto L530;
  PATTERN (ins1) = gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (5, operands));
  INSN_CODE (ins1) = 530;
  delete_for_peephole (NEXT_INSN (ins1), insn);
  return NEXT_INSN (insn);
 L530:

  insn = ins1;
  pat = PATTERN (insn);
  x = pat;
  if (GET_CODE (x) != SET) goto L531;
  x = XEXP (pat, 0);
  operands[0] = x;
  if (! register_operand (x, SImode)) goto L531;
  x = XEXP (pat, 1);
  operands[1] = x;
  if (! register_operand (x, SImode)) goto L531;
  do { insn = NEXT_INSN (insn);
       if (insn == 0) goto L531; }
  while (GET_CODE (insn) == NOTE
	 || (GET_CODE (insn) == INSN
	     && (GET_CODE (PATTERN (insn)) == USE
		 || GET_CODE (PATTERN (insn)) == CLOBBER)));
  if (GET_CODE (insn) == CODE_LABEL
      || GET_CODE (insn) == BARRIER)
    goto L531;
  pat = PATTERN (insn);
  x = pat;
  if (GET_CODE (x) != SET) goto L531;
  x = XEXP (pat, 0);
  if (GET_CODE (x) != PC) goto L531;
  x = XEXP (pat, 1);
  if (GET_CODE (x) != IF_THEN_ELSE) goto L531;
  x = XEXP (XEXP (pat, 1), 0);
  operands[2] = x;
  if (! equality_op (x, SImode)) goto L531;
  x = XEXP (XEXP (XEXP (pat, 1), 0), 0);
  if (!rtx_equal_p (operands[0], x)) goto L531;
  x = XEXP (XEXP (XEXP (pat, 1), 0), 1);
  if (GET_CODE (x) != CONST_INT) goto L531;
  if (XWINT (x, 0) != 0) goto L531;
  x = XEXP (XEXP (pat, 1), 1);
  operands[3] = x;
  if (! pc_or_label_operand (x, VOIDmode)) goto L531;
  x = XEXP (XEXP (pat, 1), 2);
  operands[4] = x;
  if (! pc_or_label_operand (x, VOIDmode)) goto L531;
  if (! (TARGET_MIPS16
   && GET_CODE (operands[1]) == REG
   && REGNO (operands[1]) == 24
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && dead_or_set_p (insn, operands[0]))) goto L531;
  PATTERN (ins1) = gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (5, operands));
  INSN_CODE (ins1) = 531;
  delete_for_peephole (NEXT_INSN (ins1), insn);
  return NEXT_INSN (insn);
 L531:

  insn = ins1;
  pat = PATTERN (insn);
  x = pat;
  if (GET_CODE (x) != SET) goto L532;
  x = XEXP (pat, 0);
  operands[0] = x;
  if (! register_operand (x, DImode)) goto L532;
  x = XEXP (pat, 1);
  operands[1] = x;
  if (! register_operand (x, DImode)) goto L532;
  do { insn = NEXT_INSN (insn);
       if (insn == 0) goto L532; }
  while (GET_CODE (insn) == NOTE
	 || (GET_CODE (insn) == INSN
	     && (GET_CODE (PATTERN (insn)) == USE
		 || GET_CODE (PATTERN (insn)) == CLOBBER)));
  if (GET_CODE (insn) == CODE_LABEL
      || GET_CODE (insn) == BARRIER)
    goto L532;
  pat = PATTERN (insn);
  x = pat;
  if (GET_CODE (x) != SET) goto L532;
  x = XEXP (pat, 0);
  if (GET_CODE (x) != PC) goto L532;
  x = XEXP (pat, 1);
  if (GET_CODE (x) != IF_THEN_ELSE) goto L532;
  x = XEXP (XEXP (pat, 1), 0);
  operands[2] = x;
  if (! equality_op (x, DImode)) goto L532;
  x = XEXP (XEXP (XEXP (pat, 1), 0), 0);
  if (!rtx_equal_p (operands[0], x)) goto L532;
  x = XEXP (XEXP (XEXP (pat, 1), 0), 1);
  if (GET_CODE (x) != CONST_INT) goto L532;
  if (XWINT (x, 0) != 0) goto L532;
  x = XEXP (XEXP (pat, 1), 1);
  operands[3] = x;
  if (! pc_or_label_operand (x, VOIDmode)) goto L532;
  x = XEXP (XEXP (pat, 1), 2);
  operands[4] = x;
  if (! pc_or_label_operand (x, VOIDmode)) goto L532;
  if (! (TARGET_MIPS16 && TARGET_64BIT
   && GET_CODE (operands[1]) == REG
   && REGNO (operands[1]) == 24
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && dead_or_set_p (insn, operands[0]))) goto L532;
  PATTERN (ins1) = gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (5, operands));
  INSN_CODE (ins1) = 532;
  delete_for_peephole (NEXT_INSN (ins1), insn);
  return NEXT_INSN (insn);
 L532:

  return 0;
}

rtx peep_operand[5];
#endif
